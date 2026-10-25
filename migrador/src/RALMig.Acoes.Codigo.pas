/// Rule actions that work on Pascal code (stage M5): `identificador-renomear`
/// (tokens of .pas/.dpr/.lpr, and the class of `object X: TClass` in forms),
/// `padrao-avisar` (one manual item per occurrence, with the line and the code
/// to paste) and the code half of two actions the form unit registers:
/// RenomearPropriedadeNoCodigo (`propriedade-renomear`) and PluginEmCodigo
/// (`propriedade-para-componente`).
///
/// Nothing here searches text: every decision is made over the tokens of
/// RALMig.Tokens, so names inside comments, strings and directives are never
/// touched. A rewrite happens only when it is proved: the type of the variable
/// comes from a declaration the scanner finds in the same unit (class field,
/// local variable, global variable, parameter) and is a RAL class or a
/// descendant of one (ApiDe/ApiPara of the context). What is not proved becomes
/// a manual item with the exact code to paste.
///
/// Code written into the user's project compiles on Delphi XE and FPC 3.2
/// (Delphi and ObjFPC modes): no inline var, no generics, no multi-line strings.
unit RALMig.Acoes.Codigo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  RALMig.Texto, RALMig.Tokens, RALMig.&Uses, RALMig.Formulario, RALMig.API,
  RALMig.Regras, RALMig.Tipos;

type
  /// `identificador-renomear`: type, constant, enum value and class names in
  /// the code (identifier tokens only) and the class of `object X: TClass` in
  /// forms when the rule acts on forms.
  TAcaoIdentificadorRenomear = class(TAcaoMigracao)
  public
    function Aplicar(ARegra: TRegraMigracao; AContexto: TContextoMigracao): boolean; override;
  end;

  /// `padrao-avisar`: a manual item per occurrence of the pattern.
  TAcaoPadraoAvisar = class(TAcaoMigracao)
  public
    function Aplicar(ARegra: TRegraMigracao; AContexto: TContextoMigracao): boolean; override;
  end;

/// Code half of `propriedade-renomear` (the form unit registers the action and
/// calls this for the code files the rule acts on). Renames
/// `<expr>.<propriedade>` when the type of <expr> is proved to be the class of
/// the rule (or a descendant); when it is not found, one manual item with the
/// text of the rule. Returns how many renames and manual items were made.
function RenomearPropriedadeNoCodigo(ARegra: TRegraMigracao;
  AContexto: TContextoMigracao): integer;

/// Code half of `propriedade-para-componente`. For a server variable declared
/// in the unit and created in it (`V := TRALxServer.Create(owner)`), the
/// plugin is created at the creation point (new field/variable declared next
/// to the one of V, `Plugin := TComp.Create(owner); Plugin.Server := V;` after
/// the creation statement, unit added to the right `uses`) and every use of
/// the moved properties (`V.CompressType`, `V.CORSOptions.AllowOrigin`...) is
/// rewritten to the plugin (`Plugin.CompressType`, `Plugin.Options.AllowOrigin`).
/// The plugin is created only when the code writes one of the properties of the
/// rule (or of its `criar-quando`) and the condition holds on those values.
/// Properties that just disappear are removed (statement); what cannot be
/// proved (type not found, no creation in the unit, complex statement) becomes
/// a manual item with the code to paste. Idempotent: a plugin already linked
/// to V (`P.Server := V`) is reused. Returns how many changes and manual items.
function PluginEmCodigo(ARegra: TRegraMigracao; AContexto: TContextoMigracao): integer;

/// Adds the unit to the uses of the text once per round of edits: two callers
/// (the code half and the form half of a rule) asking for the same unit before
/// the engine consolidates produce a single entry.
procedure UsesUmaVez(ATexto: TTextoOriginal; const AUnit: string; ASecao: TSecaoUses);

implementation

resourcestring
  rsCodRenomeado = '%de% is now %para%.';
  rsCodClasseForm = 'The class of %nome% changed from %de% to %para%.';
  rsCodMovido = '%de% is now %para%.';
  rsCodRemovido = '%nome% no longer exists on the server; the line was removed.';
  rsCodFazerPlugin = 'In %arquivo%, line %linha%: %variavel% no longer has these properties; create %componente% (unit %unit%) next to where the server is created, set its Server to %variavel% and move the values to it, then delete the old lines.';
  rsCodFazerRemover = 'In %arquivo%, line %linha%: %nome% no longer exists on the server; delete this use.';
  rsCodFazerRenomear = 'In %arquivo%, line %linha%: rename %de% to %para% if %variavel% is a %classe%.';
  rsCodEscrito = 'Written right after the object is created: %trecho%';
  rsCodSemTexto = 'See the migration notes of the RAL.';
  rsCodSomenteRelatorio = 'See the report.';

const
  CTiposCodigo = [taPas, taDpr, taLpr];
  /// Member names too common to be reported when the receiver type is unknown
  CMembrosComuns = ',get,post,put,patch,delete,domain,body,content,add,free,create,open,close,' +
    'execute,execsql,applyupdates,value,text,count,items,clear,insert,remove,read,write,' +
    'send,stop,start,active,name,';
  CDiretivas = ',overload,override,virtual,abstract,reintroduce,dynamic,inline,stdcall,' +
    'cdecl,register,safecall,pascal,forward,external,deprecated,platform,experimental,' +
    'static,final,message,assembler,varargs,export,far,near,local,unsafe,delayed,';

type
  TMarcas = array of string;

  TDecl = class
  public
    Nome: string;
    Tipo: string;
    Dono: string;
    Tok: integer;
    /// Index in Rotinas, -1 for a field or a global
    Rotina: integer;
  end;

  TClasseLocal = class
  public
    Nome: string;
    Ancestral: string;
    Ini: integer;
    Fim: integer;
  end;

  TRotina = class
  public
    Ini: integer;
    Fim: integer;
    Dono: string;
    Nome: string;
    ResultTipo: string;
  end;

  TMetodoDecl = class
  public
    Classe: string;
    Nome: string;
    Tok: integer;
    Sobrescreve: boolean;
  end;

  { TAnalise }

  /// The tokens of one file plus the declarations, classes and routines the
  /// scanner found, and the questions the actions ask about them.
  TAnalise = class
  private
    FApiDe: TApi;
    FApiPara: TApi;
    FArq: TArquivoMigrado;
    FClasses: TObjectList;
    FDecls: TObjectList;
    FEhUnit: boolean;
    FImplTok: integer;
    FInit: boolean;
    FMetodos: TObjectList;
    FNomesNovos: TStringList;
    FPilha: TList;
    FPilhaClasse: TList;
    FRotinas: TObjectList;
    FRotPend: TList;
    FTexto: TTextoOriginal;
    FToks: TListaTokens;
    procedure Escanear;
    function TratarChave(i: integer): integer;
    function TratarIdent(i: integer): integer;
    function TratarClasse(i: integer): integer;
    function TratarCabecalho(i: integer): integer;
    function TratarPropriedade(i: integer): integer;
    function TentarDecl(i: integer): integer;
    function LerTipo(q: integer; out ATipo: string): integer;
    procedure Empilhar(AKind, AClasse: integer);
    procedure Desempilhar(i: integer);
    function EmCodigo: boolean;
    function DeclPermitida(i: integer): boolean;
    function DonoAtual: string;
    function RotinaAtual: integer;
    procedure AddDecl(const ANome, ATipo, ADono: string; ATok, ARotina: integer);
    procedure LerParametros(a, b, ARotina: integer);
    procedure VerDiretivas(k: integer; out ASobrescreve, ASemCorpo: boolean);
    function ChamadaClasseEhBloco(i: integer; out ANome, AAnc: string; out ASemCorpo: boolean): boolean;
    function Contem(AIni, ATok: integer): boolean;
    function GetClasseCount: integer;
    function GetMetodoCount: integer;
    function GetMetodo(AIndex: integer): TMetodoDecl;
    function GetClasse(AIndex: integer): TClasseLocal;
  public
    constructor Create(AArq: TArquivoMigrado; AApiDe, AApiPara: TApi);
    destructor Destroy; override;
    // token helpers
    function N(i: integer): string;
    function EhId(i: integer): boolean;
    function EhSimb(i: integer; const S: string): boolean;
    function EhPal(i: integer; const S: string): boolean;
    function IdIs(i: integer; const ANome: string): boolean;
    function Fecha(i: integer): integer;
    function FechaAntes(i: integer): integer;
    function ProxPV(i: integer): integer;
    function Bruto(AIni, ATam: integer): string;
    function Fatia(a, b: integer): string;
    function Linha(i: integer): integer;
    function IniciaLinha(i: integer): boolean;
    function FimDeLinhaDepois(i: integer): boolean;
    // semantic helpers
    function RotinaDe(ATok: integer): integer;
    function BuscarDecl(const ANome: string; ATok: integer): TDecl;
    function BuscarClasseLocal(const ANome: string): TClasseLocal;
    function CampoDe(const AClasse, ANome: string): TDecl;
    function AncestralDe(const ATipo: string): string;
    function ClasseConhecida(const ATipo: string): boolean;
    function TipoCasa(const ATipo: string; APadroes: TStrings): boolean;
    function MembroTipo(const ATipo, AMembro: string): string;
    function InicioDesignador(AMembro: integer): integer;
    function TipoDesignador(a, b: integer): string;
    function ValorDoTipo(const AParam, AValor: string): boolean;
    function UltimoArgumento(AAbre: integer; out AIni, AFim: integer): boolean;
    function Veredito(AMembro: integer; APadroes: TStrings): integer;
    function ChamadaViaVar(AMembro: integer; AClasses: TStrings): boolean;
    function IdentUsado(const ANome: string): boolean;
    function FimDoComando(i: integer): integer;
    function ComandoSimples(a: integer): boolean;
    property ApiDe: TApi read FApiDe;
    property ApiPara: TApi read FApiPara;
    property Arq: TArquivoMigrado read FArq;
    property Texto: TTextoOriginal read FTexto;
    property Toks: TListaTokens read FToks;
    property ImplTok: integer read FImplTok;
    property EhUnit: boolean read FEhUnit;
    property NomesNovos: TStringList read FNomesNovos;
    property ClasseCount: integer read GetClasseCount;
    property Classes[AIndex: integer]: TClasseLocal read GetClasse;
    property MetodoCount: integer read GetMetodoCount;
    property Metodos[AIndex: integer]: TMetodoDecl read GetMetodo;
  end;

const
  /// Verdicts of TAnalise.Veredito
  vSim = 1;
  vNao = 0;
  vDesconhecido = -1;

{ helpers }

{$HINTS OFF}
function IntDe(AValor: Pointer): integer;
begin
  Result := integer(PtrUInt(AValor));
end;

function PtrDe(AValor: integer): Pointer;
begin
  Result := Pointer(PtrUInt(AValor));
end;
{$HINTS ON}

function SemE(const ANome: string): string;
begin
  Result := ANome;
  if (Result <> '') and (Result[1] = '&') then
    Delete(Result, 1, 1);
end;

/// Glob with `*` only, no case
function CasaGlob(const ANome, APadrao: string): boolean;
var
  vN, vP: string;

  function Casa(ni, pi: integer): boolean;
  begin
    while pi <= Length(vP) do
    begin
      if vP[pi] = '*' then
      begin
        while (pi < Length(vP)) and (vP[pi + 1] = '*') do
          Inc(pi);
        if pi = Length(vP) then
          Exit(True);
        while ni <= Length(vN) + 1 do
        begin
          if Casa(ni, pi + 1) then
            Exit(True);
          Inc(ni);
        end;
        Exit(False);
      end;
      if (ni > Length(vN)) or (vN[ni] <> vP[pi]) then
        Exit(False);
      Inc(ni);
      Inc(pi);
    end;
    Result := ni > Length(vN);
  end;

begin
  vN := LowerCase(ANome);
  vP := LowerCase(APadrao);
  Result := Casa(1, 1);
end;

function Latin1ParaUtf8(const AText: string): string;
var
  i: integer;
  vAlto: boolean;
begin
  vAlto := False;
  for i := 1 to Length(AText) do
    if AText[i] >= #128 then
      vAlto := True;
  if not vAlto then
    Exit(AText);
  Result := '';
  for i := 1 to Length(AText) do
    if AText[i] < #128 then
      Result := Result + AText[i]
    else
      Result := Result + Chr($C0 or (Ord(AText[i]) shr 6)) + Chr($80 or (Ord(AText[i]) and $3F));
end;

procedure Dividir(const ATexto: string; ALista: TStrings);
var
  vPos: integer;
  vRest, vItem: string;
begin
  ALista.Clear;
  vRest := ATexto;
  while vRest <> '' do
  begin
    vPos := Pos(',', vRest);
    if vPos = 0 then
    begin
      vItem := vRest;
      vRest := '';
    end
    else
    begin
      vItem := Copy(vRest, 1, vPos - 1);
      vRest := Copy(vRest, vPos + 1, MaxInt);
    end;
    vItem := Trim(vItem);
    if vItem <> '' then
      ALista.Add(vItem);
  end;
end;

/// A field of the "padrao" map or a list parameter as a list the caller frees
function CampoLista(ARegra: TRegraMigracao; const AChave: string): TStringList;
begin
  Result := TStringList.Create;
  Dividir(ARegra.Mapa('padrao').Values[AChave], Result);
end;

function NaLista(const AItem: string; ALista: TStrings): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to ALista.Count - 1 do
    if SameText(ALista[i], AItem) then
      Exit(True);
end;

function ListaDeRegra(ARegra: TRegraMigracao; const ANome: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Assign(ARegra.Lista(ANome));
end;

function Marcas(const APares: array of string): TMarcas;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Length(APares));
  for i := 0 to High(APares) do
    Result[i] := APares[i];
end;

procedure AcrescentarMarca(var AMarcas: TMarcas; const ANome, AValor: string);
var
  vLen: integer;
begin
  vLen := Length(AMarcas);
  SetLength(AMarcas, vLen + 2);
  AMarcas[vLen] := ANome;
  AMarcas[vLen + 1] := AValor;
end;

function TextoOuPadrao(const ATexto, APadrao: string): string;
begin
  if ATexto = '' then
    Result := APadrao
  else
    Result := ATexto;
end;

type
  { a unit pedida a um texto que ainda nao foi consolidado: o editor de uses le'
    o original e nao a ve. O texto e' identificado pelo Id e pela geracao, nunca
    pelo endereco - um texto novo pode nascer no endereco de um liberado, e a
    unit pedida ao antigo fazia o novo parecer servido (no Windows, o .pas de um
    frame saia sem o RALCORS) }
  TRegistroUses = class
  public
    Id: QWord;
    Geracao: integer;
    Nome: string;
  end;

var
  GUsesPedidas: TObjectList = nil;

procedure UsesUmaVez(ATexto: TTextoOriginal; const AUnit: string; ASecao: TSecaoUses);
var
  vEd: TEditorUses;
  vInt: integer;
  vReg: TRegistroUses;
begin
  if GUsesPedidas = nil then
    GUsesPedidas := TObjectList.Create(True);
  { o que foi pedido a uma geracao anterior deste texto ja' foi consolidado }
  for vInt := GUsesPedidas.Count - 1 downto 0 do
    if (TRegistroUses(GUsesPedidas[vInt]).Id = ATexto.Id) and
       (TRegistroUses(GUsesPedidas[vInt]).Geracao <> ATexto.Geracao) then
      GUsesPedidas.Delete(vInt);
  for vInt := 0 to GUsesPedidas.Count - 1 do
  begin
    vReg := TRegistroUses(GUsesPedidas[vInt]);
    if (vReg.Id = ATexto.Id) and SameText(vReg.Nome, AUnit) then
      Exit;
  end;
  vEd := TEditorUses.Create(ATexto);
  try
    if not vEd.Contem(AUnit) then
    begin
      vEd.Acrescentar(AUnit, ASecao);
      vReg := TRegistroUses.Create;
      vReg.Id := ATexto.Id;
      vReg.Geracao := ATexto.Geracao;
      vReg.Nome := AUnit;
      GUsesPedidas.Add(vReg);
    end;
  finally
    vEd.Free;
  end;
end;

{ TAnalise }

constructor TAnalise.Create(AArq: TArquivoMigrado; AApiDe, AApiPara: TApi);
begin
  inherited Create;
  FArq := AArq;
  FTexto := AArq.Texto;
  FApiDe := AApiDe;
  FApiPara := AApiPara;
  FImplTok := -1;
  FClasses := TObjectList.Create(True);
  FDecls := TObjectList.Create(True);
  FMetodos := TObjectList.Create(True);
  FRotinas := TObjectList.Create(True);
  FNomesNovos := TStringList.Create;
  FNomesNovos.CaseSensitive := False;
  FPilha := TList.Create;
  FPilhaClasse := TList.Create;
  FRotPend := TList.Create;
  FToks := TokenizarPascal(FTexto.Bytes, FTexto.InicioConteudo);
  Escanear;
end;

destructor TAnalise.Destroy;
begin
  FToks.Free;
  FClasses.Free;
  FDecls.Free;
  FMetodos.Free;
  FRotinas.Free;
  FNomesNovos.Free;
  FPilha.Free;
  FPilhaClasse.Free;
  FRotPend.Free;
  inherited Destroy;
end;

function TAnalise.GetClasseCount: integer;
begin
  Result := FClasses.Count;
end;

function TAnalise.GetClasse(AIndex: integer): TClasseLocal;
begin
  Result := TClasseLocal(FClasses[AIndex]);
end;

function TAnalise.GetMetodoCount: integer;
begin
  Result := FMetodos.Count;
end;

function TAnalise.GetMetodo(AIndex: integer): TMetodoDecl;
begin
  Result := TMetodoDecl(FMetodos[AIndex]);
end;

function TAnalise.N(i: integer): string;
begin
  if (i < 0) or (i >= FToks.Count) then
    Exit('');
  Result := SemE(FToks.Texto(i));
end;

function TAnalise.EhId(i: integer): boolean;
begin
  Result := (i >= 0) and (i < FToks.Count) and (FToks[i].Tipo = ttIdentificador);
end;

function TAnalise.EhSimb(i: integer; const S: string): boolean;
begin
  Result := (i >= 0) and (i < FToks.Count) and FToks.EhSimbolo(i, S);
end;

function TAnalise.EhPal(i: integer; const S: string): boolean;
begin
  Result := (i >= 0) and (i < FToks.Count) and (FToks[i].Tipo = ttPalavraChave) and
    SameText(FToks.Texto(i), S);
end;

function TAnalise.IdIs(i: integer; const ANome: string): boolean;
begin
  Result := EhId(i) and SameText(N(i), ANome);
end;

function TAnalise.Fecha(i: integer): integer;
var
  vDepth, j: integer;
begin
  vDepth := 0;
  for j := i to FToks.Count - 1 do
  begin
    if EhSimb(j, '(') or EhSimb(j, '[') then
      Inc(vDepth)
    else if EhSimb(j, ')') or EhSimb(j, ']') then
    begin
      Dec(vDepth);
      if vDepth = 0 then
        Exit(j);
    end;
  end;
  Result := FToks.Count;
end;

function TAnalise.FechaAntes(i: integer): integer;
var
  vDepth, j: integer;
begin
  vDepth := 0;
  for j := i downto 0 do
  begin
    if EhSimb(j, ')') or EhSimb(j, ']') then
      Inc(vDepth)
    else if EhSimb(j, '(') or EhSimb(j, '[') then
    begin
      Dec(vDepth);
      if vDepth = 0 then
        Exit(j);
    end;
  end;
  Result := -1;
end;

function TAnalise.ProxPV(i: integer): integer;
var
  vDepth, j: integer;
begin
  vDepth := 0;
  j := i;
  while j < FToks.Count do
  begin
    if EhSimb(j, '(') or EhSimb(j, '[') then
      Inc(vDepth)
    else if EhSimb(j, ')') or EhSimb(j, ']') then
      Dec(vDepth)
    else if (vDepth <= 0) and EhSimb(j, ';') then
      Exit(j);
    Inc(j);
  end;
  Result := FToks.Count;
end;

function TAnalise.Bruto(AIni, ATam: integer): string;
begin
  SetString(Result, PAnsiChar(Pointer(FTexto.Bytes)) + AIni - 1, ATam);
  if FTexto.Codificacao = cdAnsi then
    Result := Latin1ParaUtf8(Result);
end;

function TAnalise.Fatia(a, b: integer): string;
begin
  if (a < 0) or (b < a) or (b >= FToks.Count) then
    Exit('');
  Result := Bruto(FToks[a].Inicio, FToks[b].Fim - FToks[a].Inicio);
end;

function TAnalise.Linha(i: integer): integer;
begin
  Result := FTexto.LinhaDoOffset(FToks[i].Inicio);
end;

function TAnalise.IniciaLinha(i: integer): boolean;
var
  vLin: integer;
  vOff: integer;
begin
  vLin := Linha(i);
  vOff := FTexto.LinhaInicio(vLin);
  Result := Trim(Bruto(vOff, FToks[i].Inicio - vOff)) = '';
end;

function TAnalise.FimDeLinhaDepois(i: integer): boolean;
var
  vLin: integer;
  vOff: integer;
begin
  vLin := FTexto.LinhaDoOffset(FToks[i].Fim - 1);
  vOff := FTexto.LinhaFim(vLin);
  Result := Trim(Bruto(FToks[i].Fim, vOff - FToks[i].Fim)) = '';
end;

{ scanner }

function TAnalise.EmCodigo: boolean;
begin
  if FPilha.Count > 0 then
    Result := IntDe(FPilha[FPilha.Count - 1]) = 1
  else
    Result := FInit;
end;

procedure TAnalise.Empilhar(AKind, AClasse: integer);
begin
  FPilha.Add(PtrDe(AKind));
  FPilhaClasse.Add(PtrDe(AClasse));
end;

procedure TAnalise.Desempilhar(i: integer);
var
  vKind, vCls: integer;
begin
  if FPilha.Count = 0 then
    Exit;
  vKind := IntDe(FPilha[FPilha.Count - 1]);
  vCls := IntDe(FPilhaClasse[FPilhaClasse.Count - 1]);
  FPilha.Delete(FPilha.Count - 1);
  FPilhaClasse.Delete(FPilhaClasse.Count - 1);
  if (vKind = 2) and (vCls >= 0) then
    TClasseLocal(FClasses[vCls]).Fim := i;
  if (vKind = 1) and (FPilha.Count = 0) and (FRotPend.Count > 0) then
  begin
    TRotina(FRotinas[IntDe(FRotPend[FRotPend.Count - 1])]).Fim := i;
    FRotPend.Delete(FRotPend.Count - 1);
  end;
end;

function TAnalise.DonoAtual: string;
var
  vCls: integer;
begin
  Result := '';
  if (FPilha.Count > 0) and (IntDe(FPilha[FPilha.Count - 1]) = 2) then
  begin
    vCls := IntDe(FPilhaClasse[FPilhaClasse.Count - 1]);
    if vCls >= 0 then
      Result := TClasseLocal(FClasses[vCls]).Nome;
  end;
end;

function TAnalise.RotinaAtual: integer;
begin
  Result := -1;
  if (FPilha.Count > 0) and (IntDe(FPilha[FPilha.Count - 1]) = 2) then
    Exit;
  if FRotPend.Count > 0 then
    Result := IntDe(FRotPend[FRotPend.Count - 1]);
end;

procedure TAnalise.AddDecl(const ANome, ATipo, ADono: string; ATok, ARotina: integer);
var
  vDecl: TDecl;
begin
  vDecl := TDecl.Create;
  vDecl.Nome := ANome;
  vDecl.Tipo := ATipo;
  vDecl.Dono := ADono;
  vDecl.Tok := ATok;
  vDecl.Rotina := ARotina;
  FDecls.Add(vDecl);
end;

procedure TAnalise.Escanear;
var
  i: integer;
begin
  i := 0;
  while i < FToks.Count do
  begin
    case FToks[i].Tipo of
      ttPalavraChave: i := TratarChave(i);
      ttIdentificador: i := TratarIdent(i);
    else
      Inc(i);
    end;
  end;
end;

function TAnalise.TratarChave(i: integer): integer;
var
  vTxt: string;
begin
  Result := i + 1;
  vTxt := LowerCase(FToks.Texto(i));
  if vTxt = 'unit' then
  begin
    if FToks.Anterior(i) < 0 then
      FEhUnit := True;
  end
  else if vTxt = 'implementation' then
  begin
    if FPilha.Count = 0 then
      FImplTok := i;
  end
  else if (vTxt = 'initialization') or (vTxt = 'finalization') then
    FInit := True
  else if (vTxt = 'begin') or (vTxt = 'try') or (vTxt = 'asm') then
    Empilhar(1, -1)
  else if vTxt = 'case' then
  begin
    if EmCodigo then
      Empilhar(1, -1);
  end
  else if vTxt = 'record' then
    Empilhar(2, -1)
  else if vTxt = 'end' then
    Desempilhar(i)
  else if vTxt = 'class' then
    Result := TratarClasse(i)
  else if (vTxt = 'object') or (vTxt = 'interface') then
  begin
    if EhSimb(FToks.Anterior(i), '=') then
      Empilhar(2, -1);
  end
  else if (vTxt = 'procedure') or (vTxt = 'function') or (vTxt = 'constructor') or
    (vTxt = 'destructor') or (vTxt = 'operator') then
    Result := TratarCabecalho(i)
  else if vTxt = 'property' then
    Result := TratarPropriedade(i);
end;

function TAnalise.TratarIdent(i: integer): integer;
begin
  Result := i + 1;
  if not DeclPermitida(i) then
    Exit;
  if EhSimb(FToks.Anterior(i), '.') then
    Exit;
  Result := TentarDecl(i);
end;

function TAnalise.DeclPermitida(i: integer): boolean;
var
  vAnt: integer;
begin
  if FPilha.Count = 0 then
    Exit(not FInit);
  if IntDe(FPilha[FPilha.Count - 1]) = 2 then
    Exit(True);
  vAnt := FToks.Anterior(i);
  Result := EhPal(vAnt, 'var') or EhPal(vAnt, 'const');
end;

function TAnalise.LerTipo(q: integer; out ATipo: string): integer;
var
  k: integer;
begin
  ATipo := '';
  if EhSimb(q, '^') then
    q := FToks.Proximo(q);
  if (q < FToks.Count) and (EhId(q) or EhPal(q, 'string')) then
  begin
    ATipo := N(q);
    k := q;
    while EhSimb(FToks.Proximo(k), '.') and EhId(FToks.Proximo(FToks.Proximo(k))) do
    begin
      k := FToks.Proximo(FToks.Proximo(k));
      ATipo := N(k);
    end;
    Result := FToks.Proximo(k);
  end
  else
    Result := q;
end;

function TAnalise.TentarDecl(i: integer): integer;
var
  vNomes: TList;
  j, p, q, vInt: integer;
  vTipo, vDono: string;
  vRot: integer;
begin
  Result := i + 1;
  vNomes := TList.Create;
  try
    vNomes.Add(PtrDe(i));
    j := i;
    while True do
    begin
      p := FToks.Proximo(j);
      if not EhSimb(p, ',') then
        Break;
      q := FToks.Proximo(p);
      if not EhId(q) then
        Exit;
      vNomes.Add(PtrDe(q));
      j := q;
    end;
    p := FToks.Proximo(j);
    if not EhSimb(p, ':') then
      Exit;
    Result := LerTipo(FToks.Proximo(p), vTipo);
    vDono := DonoAtual;
    vRot := RotinaAtual;
    for vInt := 0 to vNomes.Count - 1 do
      AddDecl(N(IntDe(vNomes[vInt])), vTipo, vDono, IntDe(vNomes[vInt]), vRot);
  finally
    vNomes.Free;
  end;
end;

function TAnalise.TratarPropriedade(i: integer): integer;
var
  j, p: integer;
  vTipo: string;
begin
  Result := i + 1;
  j := FToks.Proximo(i);
  if not EhId(j) then
    Exit;
  Result := j + 1;
  p := FToks.Proximo(j);
  if EhSimb(p, '[') then
    p := FToks.Proximo(Fecha(p));
  if EhSimb(p, ':') then
  begin
    Result := LerTipo(FToks.Proximo(p), vTipo);
    AddDecl(N(j), vTipo, DonoAtual, j, -1);
  end;
end;

function TAnalise.ChamadaClasseEhBloco(i: integer; out ANome, AAnc: string;
  out ASemCorpo: boolean): boolean;
var
  vPrev, vNxt, vFim, p, q: integer;
  vTmp: string;
begin
  Result := False;
  ANome := '';
  AAnc := '';
  ASemCorpo := False;
  vPrev := FToks.Anterior(i);
  if not EhSimb(vPrev, '=') then
    Exit;
  vNxt := FToks.Proximo(i);
  if EhPal(vNxt, 'of') then
    Exit;
  while IdIs(vNxt, 'sealed') or IdIs(vNxt, 'abstract') do
    vNxt := FToks.Proximo(vNxt);
  p := FToks.Anterior(vPrev);
  if EhSimb(p, '>') then
  begin
    while (p >= 0) and not EhSimb(p, '<') do
      p := FToks.Anterior(p);
    p := FToks.Anterior(p);
  end;
  ANome := N(p);
  if EhSimb(vNxt, '(') then
  begin
    q := FToks.Proximo(vNxt);
    LerTipo(q, vTmp);
    AAnc := vTmp;
    vFim := Fecha(vNxt);
    if EhSimb(FToks.Proximo(vFim), ';') then
      ASemCorpo := True;
  end
  else if EhSimb(vNxt, ';') then
    ASemCorpo := True;
  Result := True;
end;

function TAnalise.TratarClasse(i: integer): integer;
var
  vNome, vAnc: string;
  vSemCorpo: boolean;
  vCls: TClasseLocal;
  vIdx: integer;
begin
  Result := i + 1;
  if not ChamadaClasseEhBloco(i, vNome, vAnc, vSemCorpo) then
    Exit;
  vIdx := -1;
  if vNome <> '' then
  begin
    vCls := TClasseLocal.Create;
    vCls.Nome := vNome;
    vCls.Ancestral := vAnc;
    vCls.Ini := i;
    vCls.Fim := -1;
    if vSemCorpo then
      vCls.Fim := i;
    vIdx := FClasses.Add(vCls);
  end;
  if not vSemCorpo then
    Empilhar(2, vIdx);
end;

procedure TAnalise.VerDiretivas(k: integer; out ASobrescreve, ASemCorpo: boolean);
var
  p, q: integer;
  vNome: string;
begin
  ASobrescreve := False;
  ASemCorpo := False;
  p := ProxPV(k);
  while p < FToks.Count do
  begin
    q := FToks.Proximo(p);
    if (q >= FToks.Count) or not (FToks[q].Tipo in [ttIdentificador, ttPalavraChave]) then
      Break;
    vNome := LowerCase(N(q));
    if Pos(',' + vNome + ',', CDiretivas) = 0 then
      Break;
    if vNome = 'override' then
      ASobrescreve := True
    else if (vNome = 'forward') or (vNome = 'external') then
      ASemCorpo := True;
    p := ProxPV(q);
  end;
end;

procedure TAnalise.LerParametros(a, b, ARotina: integer);
var
  k, j, p: integer;
  vNomes: TList;
  vTipo: string;
  vInt: integer;
begin
  k := a + 1;
  vNomes := TList.Create;
  try
    while k < b do
    begin
      while EhPal(k, 'var') or EhPal(k, 'const') or EhPal(k, 'out') do
        k := FToks.Proximo(k);
      vNomes.Clear;
      j := k;
      while EhId(j) do
      begin
        vNomes.Add(PtrDe(j));
        p := FToks.Proximo(j);
        if EhSimb(p, ',') then
          j := FToks.Proximo(p)
        else
        begin
          j := p;
          Break;
        end;
      end;
      if EhSimb(j, ':') then
      begin
        LerTipo(FToks.Proximo(j), vTipo);
        for vInt := 0 to vNomes.Count - 1 do
          AddDecl(N(IntDe(vNomes[vInt])), vTipo, '', IntDe(vNomes[vInt]), ARotina);
      end;
      k := ProxPV(j);
      if k >= b then
        Break;
      k := FToks.Proximo(k);
    end;
  finally
    vNomes.Free;
  end;
end;

function TAnalise.TratarCabecalho(i: integer): integer;
var
  j, vFimNome, vParIni, vParFim, p, vCls, vRot: integer;
  vDono, vNome: string;
  vOvr, vSem: boolean;
  vMet: TMetodoDecl;
  vRotina: TRotina;
  vTop: integer;
begin
  Result := i + 1;
  j := FToks.Proximo(i);
  if not EhId(j) then
    Exit;
  vNome := N(j);
  vDono := '';
  vFimNome := j;
  while EhSimb(FToks.Proximo(vFimNome), '.') and EhId(FToks.Proximo(FToks.Proximo(vFimNome))) do
  begin
    if vDono = '' then
      vDono := vNome
    else
      vDono := vDono + '.' + vNome;
    vFimNome := FToks.Proximo(FToks.Proximo(vFimNome));
    vNome := N(vFimNome);
  end;
  Result := vFimNome + 1;
  p := FToks.Proximo(vFimNome);
  vParIni := -1;
  vParFim := -1;
  if EhSimb(p, '(') then
  begin
    vParIni := p;
    vParFim := Fecha(p);
    Result := vParFim + 1;
  end;
  if (vParFim >= 0) then
    p := vParFim
  else
    p := vFimNome;
  if FPilha.Count > 0 then
  begin
    vTop := IntDe(FPilha[FPilha.Count - 1]);
    if vTop = 2 then
    begin
      VerDiretivas(p + 1, vOvr, vSem);
      vCls := IntDe(FPilhaClasse[FPilhaClasse.Count - 1]);
      vMet := TMetodoDecl.Create;
      if vCls >= 0 then
        vMet.Classe := TClasseLocal(FClasses[vCls]).Nome;
      vMet.Nome := vNome;
      vMet.Tok := vFimNome;
      vMet.Sobrescreve := vOvr;
      FMetodos.Add(vMet);
    end;
    Exit;
  end;
  if FInit or (FEhUnit and (FImplTok < 0)) then
    Exit;
  VerDiretivas(p + 1, vOvr, vSem);
  if vSem then
    Exit;
  vRotina := TRotina.Create;
  vRotina.Ini := i;
  vRotina.Fim := -1;
  vRotina.Dono := vDono;
  vRotina.Nome := vNome;
  if EhSimb(FToks.Proximo(p), ':') then
    LerTipo(FToks.Proximo(FToks.Proximo(p)), vRotina.ResultTipo);
  vRot := FRotinas.Add(vRotina);
  FRotPend.Add(PtrDe(vRot));
  if vParIni >= 0 then
    LerParametros(vParIni, vParFim, vRot);
end;

{ lookups }

function TAnalise.Contem(AIni, ATok: integer): boolean;
var
  vRot: TRotina;
  vFim: integer;
begin
  vRot := TRotina(FRotinas[AIni]);
  vFim := vRot.Fim;
  if vFim < 0 then
    vFim := FToks.Count;
  Result := (ATok >= vRot.Ini) and (ATok <= vFim);
end;

function TAnalise.RotinaDe(ATok: integer): integer;
var
  vInt: integer;
begin
  Result := -1;
  for vInt := 0 to FRotinas.Count - 1 do
    if Contem(vInt, ATok) then
      if (Result < 0) or (TRotina(FRotinas[vInt]).Ini > TRotina(FRotinas[Result]).Ini) then
        Result := vInt;
end;

function TAnalise.BuscarClasseLocal(const ANome: string): TClasseLocal;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to FClasses.Count - 1 do
    if SameText(TClasseLocal(FClasses[vInt]).Nome, ANome) then
      Exit(TClasseLocal(FClasses[vInt]));
end;

function TAnalise.CampoDe(const AClasse, ANome: string): TDecl;
var
  vInt, vProf: integer;
  vDecl: TDecl;
  vClasse: string;
  vCls: TClasseLocal;
begin
  Result := nil;
  vClasse := AClasse;
  vProf := 0;
  while (vClasse <> '') and (vProf < 20) do
  begin
    for vInt := 0 to FDecls.Count - 1 do
    begin
      vDecl := TDecl(FDecls[vInt]);
      if (vDecl.Rotina < 0) and SameText(vDecl.Dono, vClasse) and SameText(vDecl.Nome, ANome) then
        Exit(vDecl);
    end;
    vCls := BuscarClasseLocal(vClasse);
    if vCls = nil then
      Break;
    vClasse := vCls.Ancestral;
    Inc(vProf);
  end;
end;

function TAnalise.BuscarDecl(const ANome: string; ATok: integer): TDecl;
var
  vInt, vRot, vMelhor: integer;
  vDecl: TDecl;
  vRotina: TRotina;
begin
  Result := nil;
  vMelhor := -1;
  vRot := RotinaDe(ATok);
  // locals and parameters of the routine and of the ones that contain it
  for vInt := 0 to FRotinas.Count - 1 do
    if Contem(vInt, ATok) then
      if (vMelhor < 0) or (TRotina(FRotinas[vInt]).Ini > TRotina(FRotinas[vMelhor]).Ini) then
        vMelhor := vInt;
  while vMelhor >= 0 do
  begin
    for vInt := 0 to FDecls.Count - 1 do
    begin
      vDecl := TDecl(FDecls[vInt]);
      if (vDecl.Rotina = vMelhor) and SameText(vDecl.Nome, ANome) then
        Exit(vDecl);
    end;
    // next outer routine
    vRotina := TRotina(FRotinas[vMelhor]);
    vRot := -1;
    for vInt := 0 to FRotinas.Count - 1 do
      if (vInt <> vMelhor) and Contem(vInt, ATok) and (TRotina(FRotinas[vInt]).Ini < vRotina.Ini) then
        if (vRot < 0) or (TRotina(FRotinas[vInt]).Ini > TRotina(FRotinas[vRot]).Ini) then
          vRot := vInt;
    vMelhor := vRot;
  end;
  // fields of the class of the routine
  vRot := RotinaDe(ATok);
  if (vRot >= 0) and (TRotina(FRotinas[vRot]).Dono <> '') then
  begin
    Result := CampoDe(TRotina(FRotinas[vRot]).Dono, ANome);
    if Result <> nil then
      Exit;
  end;
  for vInt := 0 to FDecls.Count - 1 do
  begin
    vDecl := TDecl(FDecls[vInt]);
    if (vDecl.Rotina < 0) and (vDecl.Dono = '') and SameText(vDecl.Nome, ANome) then
      Exit(vDecl);
  end;
end;

function TAnalise.AncestralDe(const ATipo: string): string;
var
  vCls: TClasseLocal;
  vApi: TApiClasse;
begin
  Result := '';
  vCls := BuscarClasseLocal(ATipo);
  if vCls <> nil then
    Exit(vCls.Ancestral);
  vApi := nil;
  if FApiDe <> nil then
    vApi := FApiDe.BuscarClasse(ATipo);
  if (vApi = nil) and (FApiPara <> nil) then
    vApi := FApiPara.BuscarClasse(ATipo);
  if vApi <> nil then
    Result := vApi.Ancestral;
end;

function TAnalise.ClasseConhecida(const ATipo: string): boolean;
begin
  Result := (BuscarClasseLocal(ATipo) <> nil) or
    ((FApiDe <> nil) and (FApiDe.BuscarClasse(ATipo) <> nil)) or
    ((FApiPara <> nil) and (FApiPara.BuscarClasse(ATipo) <> nil));
end;

function TAnalise.TipoCasa(const ATipo: string; APadroes: TStrings): boolean;
var
  vAtual: string;
  vProf, vInt: integer;
begin
  Result := False;
  vAtual := ATipo;
  vProf := 0;
  while (vAtual <> '') and (vProf < 25) do
  begin
    for vInt := 0 to APadroes.Count - 1 do
      if CasaGlob(vAtual, APadroes[vInt]) then
        Exit(True);
    vAtual := AncestralDe(vAtual);
    Inc(vProf);
  end;
end;

function TAnalise.MembroTipo(const ATipo, AMembro: string): string;
var
  vAtual: string;
  vProf: integer;
  vDecl: TDecl;
  vCls: TApiClasse;
  vMem: TApiMembro;
  vApi: integer;
begin
  Result := '';
  if ATipo = '' then
    Exit;
  vAtual := ATipo;
  vProf := 0;
  while (vAtual <> '') and (vProf < 25) do
  begin
    vDecl := CampoDe(vAtual, AMembro);
    if vDecl <> nil then
      Exit(vDecl.Tipo);
    for vApi := 0 to 1 do
    begin
      if vApi = 0 then
        vCls := nil
      else
        vCls := nil;
      if (vApi = 0) and (FApiDe <> nil) then
        vCls := FApiDe.BuscarClasse(vAtual)
      else if (vApi = 1) and (FApiPara <> nil) then
        vCls := FApiPara.BuscarClasse(vAtual);
      if vCls <> nil then
      begin
        vMem := vCls.Buscar(AMembro);
        if (vMem <> nil) and (vMem.Tipo <> '') then
          Exit(vMem.Tipo);
      end;
    end;
    vAtual := AncestralDe(vAtual);
    Inc(vProf);
  end;
end;

function TAnalise.InicioDesignador(AMembro: integer): integer;
var
  p, e, q, r: integer;
begin
  Result := -1;
  p := FToks.Anterior(AMembro);
  if not EhSimb(p, '.') then
    Exit;
  e := FToks.Anterior(p);
  while e >= 0 do
  begin
    if EhSimb(e, ')') or EhSimb(e, ']') then
    begin
      q := FechaAntes(e);
      if q < 0 then
        Exit;
      r := FToks.Anterior(q);
      if EhId(r) or EhSimb(r, ')') or EhSimb(r, ']') then
      begin
        e := r;
        Continue;
      end;
      Exit(q);
    end
    else if EhId(e) or EhPal(e, 'inherited') then
    begin
      r := FToks.Anterior(e);
      if EhSimb(r, '.') then
      begin
        e := FToks.Anterior(r);
        Continue;
      end;
      Exit(e);
    end
    else
      Exit;
  end;
end;

function TAnalise.TipoDesignador(a, b: integer): string;
var
  p, q, vRot, k: integer;
  vNome, vMem: string;
  vDecl: TDecl;
begin
  Result := '';
  if (a < 0) or (a >= FToks.Count) then
    Exit;
  p := a;
  if EhSimb(p, '(') then
  begin
    q := Fecha(p);
    k := p + 1;
    while k < q do
    begin
      if EhPal(k, 'as') then
      begin
        LerTipo(FToks.Proximo(k), Result);
        Break;
      end;
      Inc(k);
    end;
    p := FToks.Proximo(q);
  end
  else if EhId(p) then
  begin
    vNome := N(p);
    q := FToks.Proximo(p);
    if EhSimb(q, '(') and (q <= b) then
    begin
      if ClasseConhecida(vNome) and (BuscarDecl(vNome, p) = nil) then
        Result := vNome;
      p := FToks.Proximo(Fecha(q));
    end
    else
    begin
      if SameText(vNome, 'Self') or SameText(vNome, 'Result') then
      begin
        vRot := RotinaDe(p);
        if vRot >= 0 then
          if SameText(vNome, 'Self') then
            Result := TRotina(FRotinas[vRot]).Dono
          else
            Result := TRotina(FRotinas[vRot]).ResultTipo;
      end
      else
      begin
        vDecl := BuscarDecl(vNome, p);
        if vDecl <> nil then
          Result := vDecl.Tipo;
      end;
      p := q;
    end;
  end
  else
    Exit;
  while (p >= 0) and (p <= b) do
  begin
    if EhSimb(p, '[') then
    begin
      p := FToks.Proximo(Fecha(p));
      Result := '';
    end
    else if EhSimb(p, '.') and EhId(FToks.Proximo(p)) then
    begin
      vMem := N(FToks.Proximo(p));
      Result := MembroTipo(Result, vMem);
      p := FToks.Proximo(FToks.Proximo(p));
      if (p <= b) and EhSimb(p, '(') then
      begin
        p := FToks.Proximo(Fecha(p));
        Result := '';
      end;
    end
    else
      Break;
  end;
end;

function TAnalise.Veredito(AMembro: integer; APadroes: TStrings): integer;
var
  vIni, vFim: integer;
  vTipo: string;
begin
  vIni := InicioDesignador(AMembro);
  if vIni < 0 then
    Exit(vDesconhecido);
  vFim := FToks.Anterior(FToks.Anterior(AMembro));
  vTipo := TipoDesignador(vIni, vFim);
  if vTipo = '' then
    Exit(vDesconhecido);
  if TipoCasa(vTipo, APadroes) then
    Result := vSim
  else
    Result := vNao;
end;

/// Text of the last parameter of a normalized signature ('procedure(A; var B)' -> 'var B')
function UltimoParametro(const AAssinatura: string): string;
var
  vAbre, vFecha, vPos: integer;
begin
  vAbre := Pos('(', AAssinatura);
  vFecha := Length(AAssinatura);
  while (vFecha > 0) and (AAssinatura[vFecha] <> ')') do
    Dec(vFecha);
  if (vAbre = 0) or (vFecha <= vAbre) then
    Exit('');
  Result := Trim(Copy(AAssinatura, vAbre + 1, vFecha - vAbre - 1));
  vPos := Length(Result);
  while (vPos > 0) and (Result[vPos] <> ';') do
    Dec(vPos);
  Result := Trim(Copy(Result, vPos + 1, MaxInt));
end;

/// True when AValor is one of the values of the enum type named by AParam
function TAnalise.ValorDoTipo(const AParam, AValor: string): boolean;
var
  vTipo: TApiTipo;
begin
  Result := False;
  vTipo := nil;
  if FApiDe <> nil then
    vTipo := FApiDe.BuscarTipo(AParam);
  if (vTipo = nil) and (FApiPara <> nil) then
    vTipo := FApiPara.BuscarTipo(AParam);
  if (vTipo <> nil) and (vTipo.Forma = 'enum') then
    Result := vTipo.Valores.IndexOf(AValor) >= 0;
end;

/// Start and end tokens of the last argument of the call that opens at AAbre
function TAnalise.UltimoArgumento(AAbre: integer; out AIni, AFim: integer): boolean;
var
  j, vFecha: integer;
begin
  vFecha := Fecha(AAbre);
  AIni := FToks.Proximo(AAbre);
  AFim := FToks.Anterior(vFecha);
  Result := (vFecha < FToks.Count) and (AIni < vFecha);
  j := AIni;
  while Result and (j < vFecha) do
  begin
    if EhSimb(j, '(') or EhSimb(j, '[') then
      j := Fecha(j)
    else if EhSimb(j, ',') then
      AIni := FToks.Proximo(j);
    j := FToks.Proximo(j);
  end;
  Result := Result and (AIni <= AFim);
end;

/// True when the call already says what the rule is about: the last argument is
/// a value of the enum the signature ends with (behavior written out), or has a
/// type the scanner finds and an overload of the method in the API takes exactly that type in
/// a `var` parameter: the call picks the overload that hands the value back
/// (the synchronous form), not the one with callback and default behavior.
function TAnalise.ChamadaViaVar(AMembro: integer; AClasses: TStrings): boolean;
var
  vAbre, vIni, vFim, vIniRec, vProf, vInt, vApi: integer;
  vTipoArg, vClasse, vParam: string;
  vCls: TApiClasse;
  vMem: TApiMembro;
  vLista: TStringList;
begin
  Result := False;
  vAbre := FToks.Proximo(AMembro);
  if not (EhSimb(vAbre, '(') and UltimoArgumento(vAbre, vIni, vFim)) then
    Exit;
  vTipoArg := TipoDesignador(vIni, vFim);
  vIniRec := InicioDesignador(AMembro);
  if vIniRec < 0 then
    Exit;
  vClasse := TipoDesignador(vIniRec, FToks.Anterior(FToks.Anterior(AMembro)));
  vLista := TStringList.Create;
  try
    vProf := 0;
    while (vClasse <> '') and (vProf < 25) do
    begin
      for vApi := 0 to 1 do
      begin
        vCls := nil;
        if (vApi = 0) and (FApiDe <> nil) then
          vCls := FApiDe.BuscarClasse(vClasse)
        else if (vApi = 1) and (FApiPara <> nil) then
          vCls := FApiPara.BuscarClasse(vClasse);
        if vCls = nil then
          Continue;
        vMem := vCls.Buscar(N(AMembro));
        if vMem = nil then
          Continue;
        for vInt := 0 to vMem.Assinaturas.Count - 1 do
        begin
          vParam := UltimoParametro(vMem.Assinaturas[vInt]);
          if ValorDoTipo(vParam, N(vIni)) and (vIni = vFim) then
            Exit(True);
          if (vTipoArg <> '') and SameText(Copy(vParam, 1, 4), 'var ') then
          begin
            vLista.Clear;
            vLista.Add(Trim(Copy(vParam, 5, MaxInt)));
            if TipoCasa(vTipoArg, vLista) then
              Exit(True);
          end;
        end;
      end;
      vClasse := AncestralDe(vClasse);
      Inc(vProf);
    end;
  finally
    vLista.Free;
  end;
end;

function TAnalise.IdentUsado(const ANome: string): boolean;
var
  i: integer;
begin
  Result := FNomesNovos.IndexOf(ANome) >= 0;
  if Result then
    Exit;
  for i := 0 to FToks.Count - 1 do
    if IdIs(i, ANome) then
      Exit(True);
end;

/// Index of the last token of the statement that starts at token i (the one
/// before the `;`, or before end/else/until/except/finally at depth 0)
function TAnalise.FimDoComando(i: integer): integer;
var
  j, vDepth: integer;
begin
  vDepth := 0;
  j := i;
  while j < FToks.Count do
  begin
    if FToks.EhIgnoravel(j) then
    begin
      Inc(j);
      Continue;
    end;
    if EhSimb(j, '(') or EhSimb(j, '[') then
      Inc(vDepth)
    else if EhSimb(j, ')') or EhSimb(j, ']') then
      Dec(vDepth)
    else if vDepth <= 0 then
    begin
      if EhSimb(j, ';') or EhPal(j, 'end') or EhPal(j, 'else') or EhPal(j, 'until') or
        EhPal(j, 'except') or EhPal(j, 'finally') then
        Exit(FToks.Anterior(j));
    end;
    Inc(j);
  end;
  Result := FToks.Count - 1;
end;

/// True when the statement that starts at a follows `;`, begin, try, finally,
/// except or repeat (so a statement can be put after it or removed)
function TAnalise.ComandoSimples(a: integer): boolean;
var
  p: integer;
begin
  p := FToks.Anterior(a);
  Result := (p < 0) or EhSimb(p, ';') or EhPal(p, 'begin') or EhPal(p, 'try') or
    EhPal(p, 'finally') or EhPal(p, 'except') or EhPal(p, 'repeat');
end;

{ rule helpers }

procedure ArquivosDeCodigo(ARegra: TRegraMigracao; AContexto: TContextoMigracao; ALista: TList);
begin
  AContexto.Conjunto.Listar(TiposDaRegra(ARegra) * CTiposCodigo, ALista);
end;

function NovaAnalise(AArq: TArquivoMigrado; AContexto: TContextoMigracao): TAnalise;
begin
  Result := TAnalise.Create(AArq, AContexto.ApiDe, AContexto.ApiPara);
end;

procedure AddFeito(AContexto: TContextoMigracao; ARegra: TRegraMigracao;
  AAnalise: TAnalise; ATok: integer; const ADescricao: string);
begin
  AContexto.Resultado.AdicionarFeito(ARegra.Id, AAnalise.Arq.Relativo,
    AAnalise.Linha(ATok), ADescricao);
end;

procedure AddManual(AContexto: TContextoMigracao; ARegra: TRegraMigracao;
  AAnalise: TAnalise; ATok: integer; var AMarcas: TMarcas; const ATrechoModelo: string;
  const AFazerPadrao: string; AFazerFixo: boolean = False);
var
  vTrecho, vProblema, vFazer: string;
  vLinha, vInt: integer;
begin
  vLinha := AAnalise.Linha(ATok);
  for vInt := 0 to AContexto.Resultado.ManualCount - 1 do
    if (AContexto.Resultado.Manuais[vInt].Regra = ARegra.Id) and
      (AContexto.Resultado.Manuais[vInt].Arquivo = AAnalise.Arq.Relativo) and
      (AContexto.Resultado.Manuais[vInt].Linha = vLinha) then
      Exit;
  AcrescentarMarca(AMarcas, 'arquivo', AAnalise.Arq.Relativo);
  AcrescentarMarca(AMarcas, 'linha', IntToStr(vLinha));
  vTrecho := PreencherMarcas(ATrechoModelo, AMarcas);
  AcrescentarMarca(AMarcas, 'trecho', vTrecho);
  vProblema := TextoRegra(ARegra, 'descricao', AContexto, AMarcas);
  vFazer := TextoRegra(ARegra, 'manual', AContexto, AMarcas);
  if AFazerFixo or (vFazer = '') then
    vFazer := PreencherMarcas(AFazerPadrao, AMarcas);
  AContexto.Resultado.AdicionarManual(ARegra.Id, AAnalise.Arq.Relativo, vLinha,
    TextoOuPadrao(vProblema, rsCodSemTexto), vFazer, vTrecho, ARegra.Wiki);
end;

{ identificador-renomear }

function ParteAntesDoPonto(AAn: TAnalise; i: integer; AMapa: TStringList): boolean;
var
  p: integer;
  vNome: string;
  vInt: integer;
begin
  Result := False;
  p := AAn.Toks.Anterior(AAn.Toks.Anterior(i));
  if not AAn.EhId(p) then
    Exit;
  vNome := AAn.N(p);
  for vInt := 0 to AMapa.Count - 1 do
    if SameText(AMapa.Names[vInt], vNome) or SameText(AMapa.ValueFromIndex[vInt], vNome) then
      Exit(True);
  if (AAn.ApiDe <> nil) and (AAn.ApiDe.Unidades.IndexOf(vNome) >= 0) then
    Exit(True);
  if (AAn.ApiPara <> nil) and (AAn.ApiPara.Unidades.IndexOf(vNome) >= 0) then
    Exit(True);
end;

function NovoValidoNaApi(ARegra: TRegraMigracao; AContexto: TContextoMigracao;
  const ANovo: string): boolean;
var
  vEnum: string;
  vTipo: TApiTipo;
begin
  Result := True;
  vEnum := ARegra.Param('enumerado');
  if (vEnum = '') or (AContexto.ApiPara = nil) then
    Exit;
  vTipo := AContexto.ApiPara.BuscarTipo(vEnum);
  if (vTipo = nil) or (vTipo.Valores = nil) then
    Exit;
  Result := vTipo.Valores.IndexOf(ANovo) >= 0;
  if not Result then
    AContexto.Resultado.Erros.Add(ARegra.Id + ': ' + ANovo + ' is not a value of ' + vEnum);
end;

function RenomearEmCodigo(ARegra: TRegraMigracao; AContexto: TContextoMigracao;
  AArq: TArquivoMigrado): integer;
var
  vAn: TAnalise;
  vMapa: TStringList;
  i, vInd: integer;
  vNome, vNovo: string;
begin
  Result := 0;
  vMapa := ARegra.Mapa('mapa');
  if vMapa.Count = 0 then
    Exit;
  vAn := NovaAnalise(AArq, AContexto);
  try
    for i := 0 to vAn.Toks.Count - 1 do
    begin
      if vAn.Toks[i].Tipo <> ttIdentificador then
        Continue;
      vNome := vAn.N(i);
      vInd := vMapa.IndexOfName(vNome);
      if vInd < 0 then
        Continue;
      vNovo := vMapa.ValueFromIndex[vInd];
      if vAn.EhSimb(vAn.Toks.Anterior(i), '.') and not ParteAntesDoPonto(vAn, i, vMapa) then
        Continue;
      if not NovoValidoNaApi(ARegra, AContexto, vNovo) then
        Continue;
      if vAn.Toks.Texto(i)[1] = '&' then
        vNovo := '&' + vNovo;
      AArq.Texto.AdicionarEdicao(vAn.Toks[i].Inicio, vAn.Toks[i].Tamanho, vNovo);
      AddFeito(AContexto, ARegra, vAn, i,
        PreencherMarcas(rsCodRenomeado, ['de', vNome, 'para', vNovo]));
      Inc(Result);
    end;
  finally
    vAn.Free;
  end;
end;

function RenomearEmFormulario(ARegra: TRegraMigracao; AContexto: TContextoMigracao;
  AArq: TArquivoMigrado): integer;
var
  vForm: TFormulario;
  vMapa: TStringList;
  vLista: TList;
  vInt, vNo: integer;
  vNode: TNoFormulario;
begin
  Result := 0;
  vMapa := ARegra.Mapa('mapa');
  try
    vForm := TFormulario.Create(AArq.Texto);
  except
    on EFormularioInvalido do
      Exit;
  end;
  try
    for vInt := 0 to vMapa.Count - 1 do
    begin
      vLista := vForm.ObjetosDaClasse(vMapa.Names[vInt]);
      try
        for vNo := 0 to vLista.Count - 1 do
        begin
          vNode := TNoFormulario(vLista[vNo]);
          vForm.TrocarClasse(vNode, vMapa.ValueFromIndex[vInt]);
          AContexto.Resultado.AdicionarFeito(ARegra.Id, AArq.Relativo, vNode.LinhaInicio,
            PreencherMarcas(rsCodClasseForm, ['nome', vNode.Nome, 'de', vMapa.Names[vInt],
              'para', vMapa.ValueFromIndex[vInt]]));
          Inc(Result);
        end;
      finally
        vLista.Free;
      end;
    end;
  finally
    vForm.Free;
  end;
end;

function TAcaoIdentificadorRenomear.Aplicar(ARegra: TRegraMigracao;
  AContexto: TContextoMigracao): boolean;
var
  vArqs: TList;
  vInt: integer;
  vArq: TArquivoMigrado;
begin
  Result := True;
  vArqs := TList.Create;
  try
    AContexto.Conjunto.Listar(TiposDaRegra(ARegra), vArqs);
    for vInt := 0 to vArqs.Count - 1 do
    begin
      vArq := TArquivoMigrado(vArqs[vInt]);
      try
        if vArq.Tipo in CTiposCodigo then
          RenomearEmCodigo(ARegra, AContexto, vArq)
        else if vArq.Tipo = taForm then
          RenomearEmFormulario(ARegra, AContexto, vArq);
      except
        on E: Exception do
        begin
          AContexto.Resultado.Erros.Add(ARegra.Id + ': ' + vArq.Relativo + ': ' + E.Message);
          Result := False;
        end;
      end;
    end;
  finally
    vArqs.Free;
  end;
end;

{ padrao-avisar }

function Receptor(AAn: TAnalise; AMembro: integer): string;
var
  vIni, vFim: integer;
begin
  vIni := AAn.InicioDesignador(AMembro);
  vFim := AAn.Toks.Anterior(AAn.Toks.Anterior(AMembro));
  if vIni < 0 then
    Result := ''
  else
    Result := AAn.Fatia(vIni, vFim);
end;

function MembroComum(const ANome: string): boolean;
begin
  Result := Pos(',' + LowerCase(ANome) + ',', CMembrosComuns) > 0;
end;

/// proved (vSim) or unknown and not a common name; vNao never matches
function AceitaReceptor(AAn: TAnalise; AMembro: integer; AClasses: TStrings;
  const ANome: string): boolean;
var
  vVer: integer;
begin
  vVer := AAn.Veredito(AMembro, AClasses);
  Result := (vVer = vSim) or ((vVer = vDesconhecido) and not MembroComum(ANome));
end;

procedure PadraoIdentificador(ARegra: TRegraMigracao; AContexto: TContextoMigracao;
  AAn: TAnalise);
var
  vNomes, vClasse: TStringList;
  vInt, i, vPonto: integer;
  vCls, vMem, vRec: string;
  vMarcas: TMarcas;
begin
  vNomes := CampoLista(ARegra, 'nomes');
  vClasse := TStringList.Create;
  try
    for vInt := 0 to vNomes.Count - 1 do
    begin
      vPonto := Pos('.', vNomes[vInt]);
      if vPonto = 0 then
      begin
        for i := 0 to AAn.Toks.Count - 1 do
          if AAn.IdIs(i, vNomes[vInt]) then
          begin
            vMarcas := Marcas(['nome', AAn.N(i), 'valor', AAn.N(i)]);
            AddManual(AContexto, ARegra, AAn, i, vMarcas, ARegra.Param('trecho'), rsCodSemTexto);
          end;
        Continue;
      end;
      vCls := Copy(vNomes[vInt], 1, vPonto - 1);
      vMem := Copy(vNomes[vInt], vPonto + 1, MaxInt);
      vClasse.Clear;
      vClasse.Add(vCls);
      for i := 0 to AAn.Toks.Count - 1 do
      begin
        if not (AAn.IdIs(i, vMem) and AAn.EhSimb(AAn.Toks.Anterior(i), '.')) then
          Continue;
        if not AceitaReceptor(AAn, i, vClasse, vMem) then
          Continue;
        vRec := Receptor(AAn, i);
        vMarcas := Marcas(['nome', vMem, 'valor', vRec + '.' + vMem, 'variavel', vRec,
          'classe', vCls]);
        AddManual(AContexto, ARegra, AAn, i, vMarcas, ARegra.Param('trecho'), rsCodSemTexto);
      end;
    end;
  finally
    vNomes.Free;
    vClasse.Free;
  end;
end;

procedure PadraoChamada(ARegra: TRegraMigracao; AContexto: TContextoMigracao; AAn: TAnalise);
var
  vClasses, vMetodos: TStringList;
  i, vFim: integer;
  vRec, vAnt: string;
  vMarcas: TMarcas;
  p: integer;
begin
  vClasses := CampoLista(ARegra, 'classe');
  vMetodos := CampoLista(ARegra, 'metodos');
  try
    for i := 0 to AAn.Toks.Count - 1 do
    begin
      if not (AAn.EhId(i) and NaLista(AAn.N(i), vMetodos) and
        AAn.EhSimb(AAn.Toks.Anterior(i), '.')) then
        Continue;
      if AAn.EhSimb(AAn.Toks.Proximo(i), ':=') then
        Continue;
      p := AAn.InicioDesignador(i);
      if (p >= 0) and (AAn.EhPal(AAn.Toks.Anterior(p), 'procedure') or
        AAn.EhPal(AAn.Toks.Anterior(p), 'function') or AAn.EhPal(AAn.Toks.Anterior(p), 'constructor') or
        AAn.EhPal(AAn.Toks.Anterior(p), 'destructor')) then
        Continue;
      if not AceitaReceptor(AAn, i, vClasses, AAn.N(i)) then
        Continue;
      // the overload that hands the value back through a var parameter
      // (synchronous, no callback) is not the call the rule is about
      if AAn.ChamadaViaVar(i, vClasses) then
        Continue;
      vRec := Receptor(AAn, i);
      vFim := i;
      if AAn.EhSimb(AAn.Toks.Proximo(i), '(') then
        vFim := AAn.Fecha(AAn.Toks.Proximo(i));
      vAnt := AAn.Fatia(i, vFim);
      vMarcas := Marcas(['nome', AAn.N(i), 'variavel', vRec, 'valor', vRec + '.' + vAnt]);
      AddManual(AContexto, ARegra, AAn, i, vMarcas, ARegra.Param('trecho'), rsCodSemTexto);
    end;
  finally
    vClasses.Free;
    vMetodos.Free;
  end;
end;

procedure PadraoAtribuicao(ARegra: TRegraMigracao; AContexto: TContextoMigracao; AAn: TAnalise);
var
  vClasses, vProps: TStringList;
  i, vFim, vRhs: integer;
  vRec, vValor: string;
  vMarcas: TMarcas;
begin
  vClasses := CampoLista(ARegra, 'classe');
  vProps := CampoLista(ARegra, 'propriedade');
  try
    for i := 0 to AAn.Toks.Count - 1 do
    begin
      if not (AAn.EhId(i) and NaLista(AAn.N(i), vProps) and
        AAn.EhSimb(AAn.Toks.Anterior(i), '.')) then
        Continue;
      vRhs := AAn.Toks.Proximo(i);
      if not AAn.EhSimb(vRhs, ':=') then
        Continue;
      if not AceitaReceptor(AAn, i, vClasses, AAn.N(i)) then
        Continue;
      vFim := AAn.FimDoComando(AAn.Toks.Proximo(vRhs));
      vValor := AAn.Fatia(AAn.Toks.Proximo(vRhs), vFim);
      vRec := Receptor(AAn, i);
      vMarcas := Marcas(['nome', AAn.N(i), 'variavel', vRec, 'valor', vValor]);
      AddManual(AContexto, ARegra, AAn, i, vMarcas, ARegra.Param('trecho'), rsCodSemTexto);
    end;
  finally
    vClasses.Free;
    vProps.Free;
  end;
end;

procedure AtribuicaoEmFormulario(ARegra: TRegraMigracao; AContexto: TContextoMigracao;
  AArq: TArquivoMigrado; ANo: TNoFormulario; AClasses, AProps: TStrings; AAn: TAnalise);
var
  vInt: integer;
  vProp: TPropriedadeForm;
  vMarcas: TMarcas;
  vTrecho, vProblema, vFazer: string;
begin
  if AAn.TipoCasa(ANo.Classe, AClasses) then
    for vInt := 0 to AProps.Count - 1 do
    begin
      vProp := ANo.Propriedade(AProps[vInt]);
      if vProp = nil then
        Continue;
      vMarcas := Marcas(['nome', AProps[vInt], 'variavel', ANo.Nome, 'valor', vProp.ValorBruto,
        'arquivo', AArq.Relativo, 'linha', IntToStr(vProp.LinhaInicio)]);
      vTrecho := PreencherMarcas(ARegra.Param('trecho'), vMarcas);
      AcrescentarMarca(vMarcas, 'trecho', vTrecho);
      vProblema := TextoRegra(ARegra, 'descricao', AContexto, vMarcas);
      vFazer := TextoRegra(ARegra, 'manual', AContexto, vMarcas);
      AContexto.Resultado.AdicionarManual(ARegra.Id, AArq.Relativo, vProp.LinhaInicio,
        TextoOuPadrao(vProblema, rsCodSemTexto), TextoOuPadrao(vFazer, rsCodSemTexto),
        vTrecho, ARegra.Wiki);
    end;
  for vInt := 0 to ANo.FilhoCount - 1 do
    AtribuicaoEmFormulario(ARegra, AContexto, AArq, ANo.Filhos[vInt], AClasses, AProps, AAn);
end;

procedure PadraoAtribuicaoFormulario(ARegra: TRegraMigracao; AContexto: TContextoMigracao;
  AArq: TArquivoMigrado);
var
  vForm: TFormulario;
  vClasses, vProps: TStringList;
  vAn: TAnalise;
  vInt: integer;
begin
  try
    vForm := TFormulario.Create(AArq.Texto);
  except
    on EFormularioInvalido do
      Exit;
  end;
  vClasses := CampoLista(ARegra, 'classe');
  vProps := CampoLista(ARegra, 'propriedade');
  // only the type helpers of TAnalise are used; a form has no Pascal tokens of
  // interest, so the analysis runs on the unit-less text of the form
  vAn := TAnalise.Create(AArq, AContexto.ApiDe, AContexto.ApiPara);
  try
    for vInt := 0 to vForm.RaizCount - 1 do
      AtribuicaoEmFormulario(ARegra, AContexto, AArq, vForm.Raizes[vInt], vClasses, vProps, vAn);
  finally
    vAn.Free;
    vClasses.Free;
    vProps.Free;
    vForm.Free;
  end;
end;

procedure PadraoConversao(ARegra: TRegraMigracao; AContexto: TContextoMigracao; AAn: TAnalise);
var
  vAlvo, vMembro: string;
  i, vFim, vUlt, vIni, vPonto: integer;
  vRec: string;
  vMarcas: TMarcas;
begin
  vAlvo := ARegra.Mapa('padrao').Values['tipo-alvo'];
  vMembro := ARegra.Mapa('padrao').Values['membro'];
  for i := 0 to AAn.Toks.Count - 1 do
  begin
    if not AAn.IdIs(i, vAlvo) then
      Continue;
    vUlt := -1;
    vIni := -1;
    if AAn.EhSimb(AAn.Toks.Proximo(i), '(') then
    begin
      vFim := AAn.Fecha(AAn.Toks.Proximo(i));
      vUlt := AAn.Toks.Anterior(vFim);
      vIni := AAn.Toks.Proximo(AAn.Toks.Proximo(i));
    end
    else if AAn.EhPal(AAn.Toks.Anterior(i), 'as') then
    begin
      vUlt := AAn.Toks.Anterior(AAn.Toks.Anterior(i));
      vIni := AAn.InicioDesignador(vUlt);
    end;
    if (vUlt < 0) or not AAn.IdIs(vUlt, vMembro) or not AAn.EhSimb(AAn.Toks.Anterior(vUlt), '.') then
      Continue;
    vPonto := AAn.Toks.Anterior(vUlt);
    if vIni < 0 then
      vIni := AAn.InicioDesignador(vUlt);
    if vIni < 0 then
      vRec := ''
    else
      vRec := AAn.Fatia(vIni, AAn.Toks.Anterior(vPonto));
    if AAn.EhPal(AAn.Toks.Anterior(i), 'as') and (vIni >= 0) then
      vRec := AAn.Fatia(vIni, AAn.Toks.Anterior(vPonto));
    vMarcas := Marcas(['nome', vMembro, 'variavel', vRec, 'classe', vAlvo,
      'valor', vRec + '.' + vMembro]);
    AddManual(AContexto, ARegra, AAn, i, vMarcas, ARegra.Param('trecho'), rsCodSemTexto);
  end;
end;

function ExisteCaminho(AAn: TAnalise; const AVar: string; ACaminho: TStrings): boolean;
var
  i, p, vSeg, vIni: integer;
  vOk: boolean;
begin
  Result := False;
  for i := 0 to AAn.Toks.Count - 1 do
  begin
    if not (AAn.IdIs(i, ACaminho[0]) and AAn.EhSimb(AAn.Toks.Anterior(i), '.')) then
      Continue;
    vIni := AAn.InicioDesignador(i);
    if vIni < 0 then
      Continue;
    if not SameText(StringReplace(AAn.Fatia(vIni, AAn.Toks.Anterior(AAn.Toks.Anterior(i))), ' ', '', [rfReplaceAll]), AVar) then
      Continue;
    p := i;
    vOk := True;
    for vSeg := 1 to ACaminho.Count - 1 do
    begin
      p := AAn.Toks.Proximo(p);
      if not AAn.EhSimb(p, '.') then
      begin
        vOk := False;
        Break;
      end;
      p := AAn.Toks.Proximo(p);
      if not AAn.IdIs(p, ACaminho[vSeg]) then
      begin
        vOk := False;
        Break;
      end;
    end;
    if vOk then
      Exit(True);
  end;
end;

/// False when the API knows the class and the class has no member called
/// AMembro (e.g. an authentication class that only inherits from a name that
/// matches TRAL*Server); true when there is no API to ask
function ClasseTemCaminho(AAn: TAnalise; const AClasse, AMembro: string): boolean;
begin
  Result := ((AAn.ApiDe = nil) and (AAn.ApiPara = nil)) or
    (AAn.MembroTipo(AClasse, AMembro) <> '');
end;

/// The statement that creates the object (`V := TClass.Create(..);` alone on
/// its line) gets the rule's `trecho` right below it: the project depended on
/// what the old version did when nothing was written, so the new version has to
/// be told explicitly (D-M5). Anything less sure than that stays a manual item.
function EscreverAposCriacao(ARegra: TRegraMigracao; AContexto: TContextoMigracao;
  AAn: TAnalise; AIni, ACreate: integer; var AMarcas: TMarcas): boolean;
var
  vFim, vPv: integer;
  vTrecho: string;
begin
  Result := False;
  vTrecho := ARegra.Param('trecho');
  if (vTrecho = '') or (Pos(#10, vTrecho) > 0) then
    Exit;
  vFim := AAn.FimDoComando(ACreate);
  vPv := AAn.Toks.Proximo(vFim);
  if not (AAn.EhSimb(vPv, ';') and AAn.ComandoSimples(AIni) and AAn.IniciaLinha(AIni) and
    AAn.FimDeLinhaDepois(vPv)) then
    Exit;
  vTrecho := PreencherMarcas(vTrecho, AMarcas);
  AAn.Texto.InserirAntesDaLinha(AAn.Linha(vPv) + 1,
    AAn.Texto.Indentacao(AAn.Linha(AIni)) + vTrecho);
  AddFeito(AContexto, ARegra, AAn, AIni, PreencherMarcas(rsCodEscrito, ['trecho', vTrecho]));
  Result := True;
end;

procedure PadraoAusencia(ARegra: TRegraMigracao; AContexto: TContextoMigracao; AAn: TAnalise);
var
  vCriacao, vCaminho: TStringList;
  i, vIni, vAtrib, vCls, vNomeCls: integer;
  vVar, vClasse: string;
  vMarcas: TMarcas;
begin
  vCriacao := CampoLista(ARegra, 'criacao-de');
  vCaminho := TStringList.Create;
  try
    Dividir(StringReplace(ARegra.Mapa('padrao').Values['propriedade'], '.', ',', [rfReplaceAll]), vCaminho);
    if vCaminho.Count = 0 then
      Exit;
    for i := 0 to AAn.Toks.Count - 1 do
    begin
      if not (AAn.IdIs(i, 'Create') and AAn.EhSimb(AAn.Toks.Anterior(i), '.')) then
        Continue;
      vNomeCls := AAn.Toks.Anterior(AAn.Toks.Anterior(i));
      if not AAn.EhId(vNomeCls) then
        Continue;
      vClasse := AAn.N(vNomeCls);
      if not AAn.TipoCasa(vClasse, vCriacao) then
        Continue;
      vCls := AAn.Toks.Anterior(vNomeCls);
      if not AAn.EhSimb(vCls, ':=') then
        Continue;
      vAtrib := AAn.Toks.Anterior(vCls);
      vIni := vAtrib;
      while AAn.EhSimb(AAn.Toks.Anterior(vIni), '.') and AAn.EhId(AAn.Toks.Anterior(AAn.Toks.Anterior(vIni))) do
        vIni := AAn.Toks.Anterior(AAn.Toks.Anterior(vIni));
      vVar := StringReplace(AAn.Fatia(vIni, vAtrib), ' ', '', [rfReplaceAll]);
      if not ClasseTemCaminho(AAn, vClasse, vCaminho[0]) then
        Continue;
      if ExisteCaminho(AAn, vVar, vCaminho) then
        Continue;
      vMarcas := Marcas(['variavel', vVar, 'classe', vClasse, 'nome', vVar]);
      if EscreverAposCriacao(ARegra, AContexto, AAn, vIni, i, vMarcas) then
        Continue;
      AddManual(AContexto, ARegra, AAn, vIni, vMarcas, ARegra.Param('trecho'), rsCodSemTexto);
    end;
  finally
    vCriacao.Free;
    vCaminho.Free;
  end;
end;

procedure PadraoSobrescrita(ARegra: TRegraMigracao; AContexto: TContextoMigracao; AAn: TAnalise);
var
  vAnc, vMet: TStringList;
  vInt: integer;
  vMarcas: TMarcas;
  vCls: TClasseLocal;
begin
  vAnc := CampoLista(ARegra, 'ancestral');
  vMet := CampoLista(ARegra, 'metodo');
  try
    for vInt := 0 to AAn.MetodoCount - 1 do
    begin
      if not (AAn.Metodos[vInt].Sobrescreve and NaLista(AAn.Metodos[vInt].Nome, vMet)) then
        Continue;
      vCls := AAn.BuscarClasseLocal(AAn.Metodos[vInt].Classe);
      if (vCls = nil) or not AAn.TipoCasa(vCls.Ancestral, vAnc) then
        Continue;
      vMarcas := Marcas(['nome', AAn.Metodos[vInt].Nome, 'classe', vCls.Nome]);
      AddManual(AContexto, ARegra, AAn, AAn.Metodos[vInt].Tok, vMarcas, ARegra.Param('trecho'),
        rsCodSemTexto);
    end;
  finally
    vAnc.Free;
    vMet.Free;
  end;
end;

function TAcaoPadraoAvisar.Aplicar(ARegra: TRegraMigracao; AContexto: TContextoMigracao): boolean;
var
  vArqs: TList;
  vInt: integer;
  vTipo: string;
  vAn: TAnalise;
  vArq: TArquivoMigrado;
  vMarcas: TMarcas;
  vRotulo: string;
begin
  Result := True;
  vTipo := ARegra.PadraoTipo;
  if vTipo = 'somente-relatorio' then
  begin
    // nao procura nada no projeto: e "comportamento que mudou" (M7), nao item
    // manual, senao toda migracao terminaria com "sobrou item manual"
    vMarcas := Marcas(['arquivo', AContexto.Conjunto.Pasta, 'linha', '0']);
    vRotulo := '';
    if AContexto.Passo <> nil then
      vRotulo := AContexto.Passo.Rotulo;
    AContexto.Resultado.AdicionarComportamento(ARegra.Id, vRotulo,
      TextoOuPadrao(TextoRegra(ARegra, 'descricao', AContexto, vMarcas), rsCodSomenteRelatorio),
      TextoRegra(ARegra, 'manual', AContexto, vMarcas), ARegra.Wiki);
    Exit;
  end;
  vArqs := TList.Create;
  try
    AContexto.Conjunto.Listar(TiposDaRegra(ARegra), vArqs);
    for vInt := 0 to vArqs.Count - 1 do
    begin
      vArq := TArquivoMigrado(vArqs[vInt]);
      try
        if vArq.Tipo = taForm then
        begin
          if vTipo = 'atribuicao-propriedade' then
            PadraoAtribuicaoFormulario(ARegra, AContexto, vArq);
          Continue;
        end;
        if not (vArq.Tipo in CTiposCodigo) then
          Continue;
        vAn := NovaAnalise(vArq, AContexto);
        try
          if vTipo = 'identificador' then
            PadraoIdentificador(ARegra, AContexto, vAn)
          else if vTipo = 'chamada-metodo' then
            PadraoChamada(ARegra, AContexto, vAn)
          else if vTipo = 'atribuicao-propriedade' then
            PadraoAtribuicao(ARegra, AContexto, vAn)
          else if vTipo = 'conversao-tipo' then
            PadraoConversao(ARegra, AContexto, vAn)
          else if vTipo = 'ausencia-atribuicao' then
            PadraoAusencia(ARegra, AContexto, vAn)
          else if vTipo = 'sobrescrita-metodo' then
            PadraoSobrescrita(ARegra, AContexto, vAn);
        finally
          vAn.Free;
        end;
      except
        on E: Exception do
        begin
          AContexto.Resultado.Erros.Add(ARegra.Id + ': ' + vArq.Relativo + ': ' + E.Message);
          Result := False;
        end;
      end;
    end;
  finally
    vArqs.Free;
  end;
end;

{ propriedade-renomear (code) }

function RenomearPropriedadeNoCodigo(ARegra: TRegraMigracao;
  AContexto: TContextoMigracao): integer;
var
  vClasses: TStringList;
  vFiles: TList;
  vInt, i: integer;
  vAn: TAnalise;
  vProp, vNovo: string;
  vVer: integer;
  vMarcas: TMarcas;
begin
  Result := 0;
  vProp := ARegra.Param('propriedade');
  vNovo := ARegra.Param('para');
  vClasses := TStringList.Create;
  vFiles := TList.Create;
  try
    vClasses.Add(ARegra.Param('classe'));
    ArquivosDeCodigo(ARegra, AContexto, vFiles);
    for vInt := 0 to vFiles.Count - 1 do
    begin
      vAn := NovaAnalise(TArquivoMigrado(vFiles[vInt]), AContexto);
      try
        for i := 0 to vAn.Toks.Count - 1 do
        begin
          if not (vAn.IdIs(i, vProp) and vAn.EhSimb(vAn.Toks.Anterior(i), '.')) then
            Continue;
          vVer := vAn.Veredito(i, vClasses);
          if vVer = vSim then
          begin
            vAn.Texto.AdicionarEdicao(vAn.Toks[i].Inicio, vAn.Toks[i].Tamanho, vNovo);
            AddFeito(AContexto, ARegra, vAn, i,
              PreencherMarcas(rsCodMovido, ['de', vProp, 'para', vNovo]));
            Inc(Result);
          end
          else if vVer = vDesconhecido then
          begin
            vMarcas := Marcas(['de', vProp, 'para', vNovo, 'variavel', Receptor(vAn, i),
              'classe', ARegra.Param('classe'), 'nome', vProp]);
            AddManual(AContexto, ARegra, vAn, i, vMarcas, '', rsCodFazerRenomear);
            Inc(Result);
          end;
        end;
      finally
        vAn.Free;
      end;
    end;
  finally
    vClasses.Free;
    vFiles.Free;
  end;
end;

{ propriedade-para-componente (code) }

type
  /// One use of `<server>.<path>` in the code
  TOcorrencia = class
  public
    Membro: integer;
    Ini: integer;
    Fim: integer;
    Caminho: string;
    Listado: string;
    Atribuicao: boolean;
    FimComando: integer;
    Rhs: string;
    Servidor: string;
    Verdito: integer;
  end;

function CaminhoSemE(AAn: TAnalise; AMembro: integer; out AFim: integer): string;
var
  p: integer;
begin
  Result := AAn.N(AMembro);
  AFim := AMembro;
  p := AAn.Toks.Proximo(AMembro);
  while AAn.EhSimb(p, '.') and AAn.EhId(AAn.Toks.Proximo(p)) do
  begin
    AFim := AAn.Toks.Proximo(p);
    Result := Result + '.' + AAn.N(AFim);
    p := AAn.Toks.Proximo(AFim);
  end;
end;

function ProprioOuPrefixo(const ACaminho, AListado: string): boolean;
begin
  Result := SameText(ACaminho, AListado) or
    (Pos(LowerCase(ACaminho) + '.', LowerCase(AListado)) = 1);
end;

/// The listed path that covers the written path: the longest listed path that
/// is a prefix of it (exact or with more segments), or one that it is a prefix
/// of. AMapeado receives the destination path ('' when it has none).
function AcharListado(const ACaminho: string; ALista: TStrings; AMapa: TStringList;
  out AMapeado: string; out ACobertos: integer): string;
var
  vInt, vSegs: integer;
  vL, vSufixo: string;
begin
  Result := '';
  AMapeado := '';
  ACobertos := 0;
  for vInt := 0 to ALista.Count - 1 do
  begin
    vL := ALista[vInt];
    if (Pos(LowerCase(vL) + '.', LowerCase(ACaminho) + '.') = 1) then
    begin
      vSegs := Length(vL);
      if vSegs > Length(Result) then
        Result := vL;
    end;
  end;
  if Result <> '' then
  begin
    AMapeado := AMapa.Values[Result];
    ACobertos := Length(Result);
    Exit;
  end;
  for vInt := 0 to ALista.Count - 1 do
  begin
    vL := ALista[vInt];
    if Pos(LowerCase(ACaminho) + '.', LowerCase(vL)) = 1 then
    begin
      vSufixo := Copy(vL, Length(ACaminho) + 1, MaxInt);
      AMapeado := AMapa.Values[vL];
      if (AMapeado <> '') and (Length(AMapeado) > Length(vSufixo)) and
        SameText(Copy(AMapeado, Length(AMapeado) - Length(vSufixo) + 1, MaxInt), vSufixo) then
      begin
        Result := vL;
        AMapeado := Copy(AMapeado, 1, Length(AMapeado) - Length(vSufixo));
        ACobertos := Length(ACaminho);
        Exit;
      end;
    end;
  end;
end;

function RhsVazio(const AValor: string): boolean;
begin
  Result := (AValor = '') or (AValor = '''''') or (AValor = '[]') or (AValor = 'nil');
end;

function TemPalavra(const AValor, APalavra: string): boolean;
var
  vTok: TListaTokens;
  i: integer;
begin
  Result := False;
  vTok := TokenizarPascal(AValor);
  try
    for i := 0 to vTok.Count - 1 do
      if vTok.EhNomeIgual(i, APalavra) then
        Exit(True);
  finally
    vTok.Free;
  end;
end;

function AvaliarCond(ACond: TCondicaoRegra; AAmb: TStringList): boolean;
var
  vInt, vInd: integer;
  vRhs: string;
begin
  if ACond = nil then
    Exit(True);
  vInd := AAmb.IndexOfName(LowerCase(ACond.Propriedade));
  if vInd >= 0 then
    vRhs := Trim(AAmb.ValueFromIndex[vInd])
  else
    vRhs := '';
  case ACond.Tipo of
    tcTemPropriedade: Result := vInd >= 0;
    tcValorIgual: Result := (vInd >= 0) and SameText(vRhs, ACond.Valor);
    tcValorDiferente: Result := (vInd >= 0) and not SameText(vRhs, ACond.Valor);
    tcValorNaoVazio: Result := (vInd >= 0) and not RhsVazio(vRhs);
    tcSetContem: Result := (vInd >= 0) and TemPalavra(vRhs, ACond.Opcao);
    tcAlguma:
      begin
        Result := False;
        for vInt := 0 to ACond.FilhoCount - 1 do
          if AvaliarCond(ACond.Filhos[vInt], AAmb) then
            Result := True;
      end;
    tcTodas:
      begin
        Result := True;
        for vInt := 0 to ACond.FilhoCount - 1 do
          if not AvaliarCond(ACond.Filhos[vInt], AAmb) then
            Result := False;
      end;
  else
    Result := True;
  end;
  if ACond.Negar then
    Result := not Result;
end;

procedure PropsDaCondicao(ACond: TCondicaoRegra; ALista: TStrings);
var
  vInt: integer;
begin
  if ACond = nil then
    Exit;
  if ACond.Propriedade <> '' then
    ALista.Add(ACond.Propriedade);
  for vInt := 0 to ACond.FilhoCount - 1 do
    PropsDaCondicao(ACond.Filhos[vInt], ALista);
end;

/// True when a rule after ARegra in the step of the same action mentions the
/// property in its condition (so this one is not the last to need it)
function OutraRegraUsa(AContexto: TContextoMigracao; ARegra: TRegraMigracao;
  const APropriedade: string): boolean;
var
  vInt: integer;
  vAchou: boolean;
  vProps: TStringList;
begin
  Result := False;
  if AContexto.Passo = nil then
    Exit;
  vAchou := False;
  vProps := TStringList.Create;
  try
    for vInt := 0 to AContexto.Passo.RegraCount - 1 do
    begin
      if AContexto.Passo.Regras[vInt] = ARegra then
      begin
        vAchou := True;
        Continue;
      end;
      if vAchou and (AContexto.Passo.Regras[vInt].Acao = ARegra.Acao) then
      begin
        vProps.Clear;
        PropsDaCondicao(AContexto.Passo.Regras[vInt].CriarQuando, vProps);
        if NaLista(APropriedade, vProps) then
          Exit(True);
      end;
    end;
  finally
    vProps.Free;
  end;
end;

function NomeBaseDoPlugin(const AComponente: string): string;
begin
  Result := AComponente;
  if SameText(Copy(Result, 1, 4), 'TRAL') then
    Delete(Result, 1, 4);
  if SameText(Copy(Result, Length(Result) - 5, 6), 'Plugin') then
    Delete(Result, Length(Result) - 5, 6);
  if Result = '' then
    Result := 'Plugin';
end;

function NovoNomeDePlugin(AAn: TAnalise; const AServidor, AComponente: string): string;
var
  vPrefixo, vBase: string;
  vNum: integer;
begin
  vPrefixo := '';
  if (Length(AServidor) > 1) and (AServidor[1] in ['F', 'f']) and (AServidor[2] in ['A'..'Z']) then
    vPrefixo := 'F';
  vBase := vPrefixo + NomeBaseDoPlugin(AComponente);
  Result := vBase;
  vNum := 1;
  while AAn.IdentUsado(Result) do
  begin
    Inc(vNum);
    Result := vBase + IntToStr(vNum);
  end;
  AAn.NomesNovos.Add(Result);
end;

type
  TGrupoServidor = class
  public
    Chave: string;
    Ocs: TObjectList;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TGrupoServidor.Create;
begin
  inherited Create;
  Ocs := TObjectList.Create(True);
end;

destructor TGrupoServidor.Destroy;
begin
  Ocs.Free;
  inherited Destroy;
end;

type
  /// Everything one run of PluginEmCodigo over one file needs
  TPlugin = class
  public
    Regra: TRegraMigracao;
    Ctx: TContextoMigracao;
    An: TAnalise;
    Listadas: TStringList;
    Descartar: TStringList;
    Mapa: TStringList;
    Gatilhos: TStringList;
    Classes: TStringList;
    Componente: string;
    UnitNome: string;
    Ligacao: string;
    Total: integer;
    constructor Create(ARegra: TRegraMigracao; AContexto: TContextoMigracao; AAn: TAnalise);
    destructor Destroy; override;
    procedure Executar;
  private
    FCriacaoComplexa: boolean;
    /// pairs (first token, `;` token) of the case arms whose removal waits for
    /// the end of the run, to drop a case that ends up with nothing in it
    FBracos: TList;
    function RemoverReal(AOc: TOcorrencia): boolean;
    function EhBraco(AOc: TOcorrencia): boolean;
    function CaseDoBraco(AIni: integer): integer;
    function FimDoCase(AOf: integer): integer;
    function CaseVazio(ACase: integer; out AOf, AEnd: integer): boolean;
    function ArmaRemovida(i: integer): boolean;
    procedure ResolverBracos;
    function PularLigacao(var AUlt: integer): boolean;
    function AlvoDeCriacao(ACreate: integer): integer;
    procedure ManualUm(AOc: TOcorrencia);
    procedure ColetarOcorrencias(AGrupos: TObjectList);
    function GrupoDe(AGrupos: TObjectList; const AChave: string): TGrupoServidor;
    procedure TratarGrupo(AGrupo: TGrupoServidor);
    procedure Ambiente(AGrupo: TGrupoServidor; AAmb: TStringList);
    function PluginExistente(const AServidor: string): string;
    procedure Reescrever(AGrupo: TGrupoServidor; const APlugin: string; ACriado: boolean);
    function RemoverComando(AOc: TOcorrencia): boolean;
    procedure ManualDoGrupo(AGrupo: TGrupoServidor; const APlugin: string; ACriar: boolean);
    function TrechoDoGrupo(AGrupo: TGrupoServidor; const APlugin: string): string;
    function CriacoesDe(const AServidor: string; ALista: TList): boolean;
    function Preparar(AGrupo: TGrupoServidor; const AServidor: string; out ADecl: TDecl;
      ACriacoes: TList): boolean;
    procedure DeclararCampo(ADecl: TDecl; const APlugin: string);
    procedure InserirCriacao(ACriacao: integer; const APlugin, AServidor: string);
    function DonoSecao(ADecl: TDecl): TSecaoUses;
    procedure AddFeitoPlugin(AOc: TOcorrencia; const AMsg: string);
  end;

constructor TPlugin.Create(ARegra: TRegraMigracao; AContexto: TContextoMigracao; AAn: TAnalise);
begin
  inherited Create;
  Regra := ARegra;
  Ctx := AContexto;
  An := AAn;
  Listadas := ListaDeRegra(ARegra, 'propriedades');
  Descartar := ListaDeRegra(ARegra, 'descartar');
  Mapa := TStringList.Create;
  Mapa.Assign(ARegra.Mapa('mapeamento'));
  Gatilhos := TStringList.Create;
  PropsDaCondicao(ARegra.CriarQuando, Gatilhos);
  Classes := TStringList.Create;
  Classes.Add(ARegra.Param('classe-dona'));
  Componente := ARegra.Param('componente');
  UnitNome := ARegra.Param('unit');
  Ligacao := ARegra.Param('ligacao', 'Server');
  FBracos := TList.Create;
end;

destructor TPlugin.Destroy;
begin
  FBracos.Free;
  Listadas.Free;
  Descartar.Free;
  Mapa.Free;
  Gatilhos.Free;
  Classes.Free;
  inherited Destroy;
end;

function TPlugin.GrupoDe(AGrupos: TObjectList; const AChave: string): TGrupoServidor;
var
  vInt: integer;
begin
  for vInt := 0 to AGrupos.Count - 1 do
    if SameText(TGrupoServidor(AGrupos[vInt]).Chave, AChave) then
      Exit(TGrupoServidor(AGrupos[vInt]));
  Result := TGrupoServidor.Create;
  Result.Chave := AChave;
  AGrupos.Add(Result);
end;

procedure TPlugin.ColetarOcorrencias(AGrupos: TObjectList);
var
  i, vFim, vIni, vCob, vRhs: integer;
  vCam, vMap, vLis, vChave: string;
  vOc: TOcorrencia;
  vTodas: TStringList;
begin
  vTodas := TStringList.Create;
  try
    vTodas.AddStrings(Listadas);
    for i := 0 to Gatilhos.Count - 1 do
      if vTodas.IndexOf(Gatilhos[i]) < 0 then
        vTodas.Add(Gatilhos[i]);
    for i := 0 to An.Toks.Count - 1 do
    begin
      if not (An.EhId(i) and An.EhSimb(An.Toks.Anterior(i), '.')) then
        Continue;
      vCam := CaminhoSemE(An, i, vFim);
      vLis := AcharListado(vCam, vTodas, Mapa, vMap, vCob);
      if vLis = '' then
        Continue;
      vIni := An.InicioDesignador(i);
      if vIni < 0 then
        Continue;
      vOc := TOcorrencia.Create;
      vOc.Membro := i;
      vOc.Ini := vIni;
      vOc.Fim := vFim;
      vOc.Caminho := vCam;
      vOc.Listado := vLis;
      vOc.Servidor := StringReplace(An.Fatia(vIni, An.Toks.Anterior(An.Toks.Anterior(i))),
        ' ', '', [rfReplaceAll]);
      if (Length(vOc.Servidor) > 5) and SameText(Copy(vOc.Servidor, 1, 5), 'Self.') then
        Delete(vOc.Servidor, 1, 5);
      vOc.Verdito := An.Veredito(i, Classes);
      vRhs := An.Toks.Proximo(vFim);
      vOc.Atribuicao := An.EhSimb(vRhs, ':=');
      vOc.FimComando := vFim;
      if vOc.Atribuicao then
      begin
        vOc.FimComando := An.FimDoComando(An.Toks.Proximo(vRhs));
        vOc.Rhs := An.Fatia(An.Toks.Proximo(vRhs), vOc.FimComando);
      end;
      if vOc.Verdito = vNao then
      begin
        vOc.Free;
        Continue;
      end;
      vChave := LowerCase(vOc.Servidor);
      GrupoDe(AGrupos, vChave).Ocs.Add(vOc);
    end;
  finally
    vTodas.Free;
  end;
end;

procedure TPlugin.Ambiente(AGrupo: TGrupoServidor; AAmb: TStringList);
var
  vInt: integer;
  vOc: TOcorrencia;
begin
  for vInt := 0 to AGrupo.Ocs.Count - 1 do
  begin
    vOc := TOcorrencia(AGrupo.Ocs[vInt]);
    if vOc.Atribuicao then
      AAmb.Values[LowerCase(vOc.Caminho)] := vOc.Rhs;
  end;
end;

function TPlugin.PluginExistente(const AServidor: string): string;
var
  i: integer;
  vNome: string;
  vIni: integer;
begin
  Result := '';
  for i := 0 to An.Toks.Count - 1 do
  begin
    if not (An.IdIs(i, Ligacao) and An.EhSimb(An.Toks.Anterior(i), '.') and
      An.EhSimb(An.Toks.Proximo(i), ':=')) then
      Continue;
    vIni := An.InicioDesignador(i);
    if vIni < 0 then
      Continue;
    vNome := StringReplace(An.Fatia(vIni, An.Toks.Anterior(An.Toks.Anterior(i))), ' ', '', [rfReplaceAll]);
    if SameText(An.TipoDesignador(vIni, An.Toks.Anterior(An.Toks.Anterior(i))), Componente) and
      SameText(StringReplace(An.Fatia(An.Toks.Proximo(An.Toks.Proximo(i)),
        An.FimDoComando(An.Toks.Proximo(An.Toks.Proximo(i)))), ' ', '', [rfReplaceAll]), AServidor) then
      Exit(vNome);
  end;
end;

/// Start of the target of `<target> := TClass.Create`, ACreate being the
/// Create token; -1 when the statement is not that
function TPlugin.AlvoDeCriacao(ACreate: integer): integer;
var
  vCls, vNomeCls, vAtrib: integer;
begin
  Result := -1;
  vNomeCls := An.Toks.Anterior(An.Toks.Anterior(ACreate));
  if not An.EhId(vNomeCls) then
    Exit;
  if not An.TipoCasa(An.N(vNomeCls), Classes) then
    Exit;
  vCls := An.Toks.Anterior(vNomeCls);
  if not An.EhSimb(vCls, ':=') then
    Exit;
  vAtrib := An.Toks.Anterior(vCls);
  if not An.EhId(vAtrib) then
    Exit;
  Result := vAtrib;
  while An.EhSimb(An.Toks.Anterior(Result), '.') and
    An.EhId(An.Toks.Anterior(An.Toks.Anterior(Result))) do
    Result := An.Toks.Anterior(An.Toks.Anterior(Result));
end;

function TPlugin.CriacoesDe(const AServidor: string; ALista: TList): boolean;
var
  i, vIni, vAtrib: integer;
  vVar: string;
begin
  for i := 0 to An.Toks.Count - 1 do
  begin
    if not (An.IdIs(i, 'Create') and An.EhSimb(An.Toks.Anterior(i), '.')) then
      Continue;
    vIni := AlvoDeCriacao(i);
    if vIni < 0 then
      Continue;
    vAtrib := An.Toks.Anterior(An.Toks.Anterior(An.Toks.Anterior(An.Toks.Anterior(i))));
    vVar := StringReplace(An.Fatia(vIni, vAtrib), ' ', '', [rfReplaceAll]);
    if (Length(vVar) > 5) and SameText(Copy(vVar, 1, 5), 'Self.') then
      Delete(vVar, 1, 5);
    if not SameText(vVar, AServidor) then
      Continue;
    if not An.ComandoSimples(vIni) then
      FCriacaoComplexa := True;
    ALista.Add(PtrDe(i));
  end;
  Result := ALista.Count > 0;
end;

function TPlugin.DonoSecao(ADecl: TDecl): TSecaoUses;
begin
  if not An.EhUnit then
  begin
    if An.Arq.Tipo = taPas then
      Exit(suInterface);
    Exit(suPrograma);
  end;
  if (An.ImplTok >= 0) and (ADecl.Tok > An.ImplTok) then
    Result := suImplementation
  else
    Result := suInterface;
end;

procedure TPlugin.DeclararCampo(ADecl: TDecl; const APlugin: string);
var
  vFim, vLinha, vProx, vTipo: integer;
  vIndent, vTexto: string;
begin
  vFim := An.ProxPV(ADecl.Tok);
  if vFim >= An.Toks.Count then
    Exit;
  // after the plugin fields already declared right below the server
  while True do
  begin
    vProx := An.Toks.Proximo(vFim);
    vTipo := An.Toks.Proximo(An.Toks.Proximo(vProx));
    if not (An.EhId(vProx) and An.EhSimb(An.Toks.Proximo(vProx), ':') and An.EhId(vTipo)) then
      Break;
    if not SameText(Copy(An.N(vTipo), Length(An.N(vTipo)) - 5, 6), 'Plugin') then
      Break;
    vFim := An.ProxPV(vProx);
  end;
  vLinha := An.Linha(vFim);
  vIndent := An.Texto.Indentacao(An.Linha(ADecl.Tok));
  vTexto := vIndent + APlugin + ': ' + Componente + ';';
  if not An.IniciaLinha(ADecl.Tok) and An.EhPal(An.Toks.Anterior(ADecl.Tok), 'var') then
    vTexto := vIndent + 'var ' + APlugin + ': ' + Componente + ';';
  An.Texto.InserirAntesDaLinha(vLinha + 1, vTexto);
end;

/// When the two statements after AUlt are `P := T<x>Plugin.Create(..);` and
/// `P.<Ligacao> := ..;`, moves AUlt to the end of the second one
function TPlugin.PularLigacao(var AUlt: integer): boolean;
var
  s, c, e, t: integer;
  vCls: string;
begin
  Result := False;
  s := An.Toks.Proximo(AUlt);
  if not (An.EhId(s) and An.EhSimb(An.Toks.Proximo(s), ':=')) then
    Exit;
  c := An.Toks.Proximo(An.Toks.Proximo(s));
  if not (An.EhId(c) and An.EhSimb(An.Toks.Proximo(c), '.') and
    An.IdIs(An.Toks.Proximo(An.Toks.Proximo(c)), 'Create')) then
    Exit;
  vCls := An.N(c);
  if not SameText(Copy(vCls, Length(vCls) - 5, 6), 'Plugin') then
    Exit;
  e := An.ProxPV(c);
  t := An.Toks.Proximo(e);
  if not (An.IdIs(t, An.N(s)) and An.EhSimb(An.Toks.Proximo(t), '.') and
    An.IdIs(An.Toks.Proximo(An.Toks.Proximo(t)), Ligacao)) then
    Exit;
  AUlt := An.ProxPV(t);
  Result := True;
end;

procedure TPlugin.InserirCriacao(ACriacao: integer; const APlugin, AServidor: string);
var
  vFimCmd, vLinha, vAbre, vFecha, vIniStmt, vUlt: integer;
  vOwner, vIndent: string;
begin
  vIniStmt := AlvoDeCriacao(ACriacao);
  vFimCmd := An.FimDoComando(ACriacao);
  vUlt := vFimCmd;
  if An.EhSimb(An.Toks.Proximo(vFimCmd), ';') then
    vUlt := An.Toks.Proximo(vFimCmd);
  // after the plugin links already written below the creation
  while PularLigacao(vUlt) do
    ;
  vLinha := An.Linha(vUlt);
  vOwner := 'nil';
  vAbre := An.Toks.Proximo(ACriacao);
  if An.EhSimb(vAbre, '(') then
  begin
    vFecha := An.Fecha(vAbre);
    if An.Toks.Proximo(vAbre) = An.Toks.Anterior(vFecha) then
      vOwner := An.Fatia(An.Toks.Proximo(vAbre), An.Toks.Anterior(vFecha));
  end;
  vIndent := An.Texto.Indentacao(An.Linha(vIniStmt));
  An.Texto.InserirAntesDaLinha(vLinha + 1,
    vIndent + APlugin + ' := ' + Componente + '.Create(' + vOwner + ');' + #10 +
    vIndent + APlugin + '.' + Ligacao + ' := ' + AServidor + ';');
end;

/// True when the statement is the whole body of a numeric/string labelled arm
/// (`1: X := ..;`) or of the `else` of a case: tokens before it say so
function TPlugin.EhBraco(AOc: TOcorrencia): boolean;
var
  vPrev, vLab: integer;
begin
  vPrev := An.Toks.Anterior(AOc.Ini);
  if An.EhSimb(vPrev, ':') then
  begin
    vLab := An.Toks.Anterior(vPrev);
    Exit(An.Toks[vLab].Tipo in [ttNumero, ttString]);
  end;
  Result := An.EhPal(vPrev, 'else');
end;

/// Nearest enclosing `case` of the token (-1 when the nearest block is not one)
function TPlugin.CaseDoBraco(AIni: integer): integer;
var
  j, vProf: integer;
begin
  Result := -1;
  vProf := 0;
  j := An.Toks.Anterior(AIni);
  while j >= 0 do
  begin
    if An.EhPal(j, 'end') or An.EhPal(j, 'until') then
      Inc(vProf)
    else if An.EhPal(j, 'begin') or An.EhPal(j, 'try') or An.EhPal(j, 'record') or
      An.EhPal(j, 'repeat') or An.EhPal(j, 'case') then
    begin
      if vProf = 0 then
      begin
        if An.EhPal(j, 'case') then
          Result := j;
        Exit;
      end;
      Dec(vProf);
    end;
    j := An.Toks.Anterior(j);
  end;
end;

/// The `end` that closes the case whose `of` is AOf
function TPlugin.FimDoCase(AOf: integer): integer;
var
  j, vProf: integer;
begin
  vProf := 0;
  j := An.Toks.Proximo(AOf);
  while (j >= 0) and (j < An.Toks.Count) do
  begin
    if An.EhPal(j, 'begin') or An.EhPal(j, 'try') or An.EhPal(j, 'record') or
      An.EhPal(j, 'repeat') or An.EhPal(j, 'case') then
      Inc(vProf)
    else if An.EhPal(j, 'end') or An.EhPal(j, 'until') then
    begin
      if vProf = 0 then
        Exit(j);
      Dec(vProf);
    end;
    j := An.Toks.Proximo(j);
  end;
  Result := -1;
end;

function TPlugin.ArmaRemovida(i: integer): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := 0 to FBracos.Count div 2 - 1 do
    if (i >= IntDe(FBracos[vInt * 2])) and (i <= IntDe(FBracos[vInt * 2 + 1])) then
      Exit(True);
end;

/// A case whose selector is a plain designator and whose arms are all removed
/// ones (or bare labels, `else` and `;`) has nothing left to do
function TPlugin.CaseVazio(ACase: integer; out AOf, AEnd: integer): boolean;
var
  j: integer;
begin
  Result := False;
  AOf := An.Toks.Proximo(ACase);
  while (AOf >= 0) and (AOf < An.Toks.Count) and not An.EhPal(AOf, 'of') do
  begin
    if not (An.EhId(AOf) or An.EhSimb(AOf, '.')) then
      Exit;
    AOf := An.Toks.Proximo(AOf);
  end;
  if (AOf < 0) or (AOf >= An.Toks.Count) then
    Exit;
  AEnd := FimDoCase(AOf);
  if AEnd < 0 then
    Exit;
  j := An.Toks.Proximo(AOf);
  while j < AEnd do
  begin
    if not (ArmaRemovida(j) or An.EhSimb(j, ':') or An.EhSimb(j, ',') or An.EhSimb(j, ';') or
      An.EhPal(j, 'else') or (An.Toks[j].Tipo in [ttNumero, ttString])) then
      Exit;
    j := An.Toks.Proximo(j);
  end;
  Result := True;
end;

procedure TPlugin.ResolverBracos;
var
  vInt, vCase, vOf, vEnd, vFim, vL1, vL2, vPv: integer;
  vFeitos: TList;
  vOc: TOcorrencia;
begin
  vFeitos := TList.Create;
  try
    for vInt := 0 to FBracos.Count div 2 - 1 do
    begin
      vCase := CaseDoBraco(IntDe(FBracos[vInt * 2]));
      if (vCase >= 0) and (vFeitos.IndexOf(PtrDe(vCase)) < 0) and CaseVazio(vCase, vOf, vEnd) then
      begin
        vFeitos.Add(PtrDe(vCase));
        vFim := vEnd;
        vPv := An.Toks.Proximo(vEnd);
        if An.EhSimb(vPv, ';') then
          vFim := vPv;
        if An.ComandoSimples(vCase) and An.IniciaLinha(vCase) and An.FimDeLinhaDepois(vFim) then
        begin
          vL1 := An.Linha(vCase);
          vL2 := An.Linha(vFim);
          An.Texto.RemoverLinhas(vL1, vL2);
        end
        else
          An.Texto.AdicionarEdicao(An.Toks[vCase].Inicio,
            An.Toks[vFim].Fim - An.Toks[vCase].Inicio, '');
      end;
    end;
    // arms of the cases that keep something: removed one by one
    for vInt := 0 to FBracos.Count div 2 - 1 do
    begin
      vCase := CaseDoBraco(IntDe(FBracos[vInt * 2]));
      if (vCase >= 0) and (vFeitos.IndexOf(PtrDe(vCase)) >= 0) then
        Continue;
      vOc := TOcorrencia.Create;
      try
        vOc.Ini := IntDe(FBracos[vInt * 2]);
        vOc.FimComando := An.Toks.Anterior(IntDe(FBracos[vInt * 2 + 1]));
        vOc.Atribuicao := True;
        RemoverReal(vOc);
      finally
        vOc.Free;
      end;
    end;
  finally
    vFeitos.Free;
  end;
end;

function TPlugin.RemoverComando(AOc: TOcorrencia): boolean;
var
  vPv: integer;
begin
  vPv := An.Toks.Proximo(AOc.FimComando);
  if AOc.Atribuicao and An.EhSimb(vPv, ';') and EhBraco(AOc) then
  begin
    FBracos.Add(PtrDe(AOc.Ini));
    FBracos.Add(PtrDe(vPv));
    Exit(True);
  end;
  Result := RemoverReal(AOc);
end;

function TPlugin.RemoverReal(AOc: TOcorrencia): boolean;
var
  vPrev, vPv, vL1, vL2: integer;
begin
  Result := False;
  if not AOc.Atribuicao then
    Exit;
  vPv := An.Toks.Proximo(AOc.FimComando);
  if not An.EhSimb(vPv, ';') then
    Exit;
  if An.ComandoSimples(AOc.Ini) and An.IniciaLinha(AOc.Ini) and An.FimDeLinhaDepois(vPv) then
  begin
    vL1 := An.Linha(AOc.Ini);
    vL2 := An.Linha(vPv);
    An.Texto.RemoverLinhas(vL1, vL2);
    Exit(True);
  end;
  vPrev := An.Toks.Anterior(AOc.Ini);
  if An.EhPal(vPrev, 'then') or An.EhPal(vPrev, 'do') or An.EhPal(vPrev, 'else') or
    An.EhSimb(vPrev, ':') or An.ComandoSimples(AOc.Ini) then
  begin
    An.Texto.AdicionarEdicao(An.Toks[AOc.Ini].Inicio,
      An.Toks[AOc.FimComando].Fim - An.Toks[AOc.Ini].Inicio, '');
    Result := True;
  end;
end;

procedure TPlugin.AddFeitoPlugin(AOc: TOcorrencia; const AMsg: string);
begin
  AddFeito(Ctx, Regra, An, AOc.Membro, AMsg);
  Inc(Total);
end;

function TPlugin.TrechoDoGrupo(AGrupo: TGrupoServidor; const APlugin: string): string;
var
  vInt: integer;
  vOc: TOcorrencia;
  vMap, vDest: string;
  vCob: integer;
  vLinhas: TStringList;
begin
  vLinhas := TStringList.Create;
  try
    vLinhas.Add(APlugin + ' := ' + Componente + '.Create(nil);');
    vLinhas.Add(APlugin + '.' + Ligacao + ' := ' + TOcorrencia(AGrupo.Ocs[0]).Servidor + ';');
    for vInt := 0 to AGrupo.Ocs.Count - 1 do
    begin
      vOc := TOcorrencia(AGrupo.Ocs[vInt]);
      if not vOc.Atribuicao then
        Continue;
      if AcharListado(vOc.Caminho, Listadas, Mapa, vDest, vCob) = '' then
        Continue;
      vMap := vDest;
      if (vMap <> '') and (Pos(LowerCase(vOc.Listado), LowerCase(vOc.Caminho)) = 0) then
        vMap := vDest;
      if vMap <> '' then
        vLinhas.Add(APlugin + '.' + vMap + ' := ' + vOc.Rhs + ';');
    end;
    Result := vLinhas.Text;
    Result := Trim(StringReplace(Result, #13#10, #10, [rfReplaceAll]));
  finally
    vLinhas.Free;
  end;
end;

procedure TPlugin.ManualDoGrupo(AGrupo: TGrupoServidor; const APlugin: string; ACriar: boolean);
var
  vMarcas: TMarcas;
  vOc: TOcorrencia;
  vTrecho: string;
begin
  vOc := TOcorrencia(AGrupo.Ocs[0]);
  vTrecho := '';
  if ACriar then
    vTrecho := TrechoDoGrupo(AGrupo, APlugin);
  vMarcas := Marcas(['nome', vOc.Servidor, 'variavel', vOc.Servidor, 'valor', vOc.Rhs,
    'componente', Componente, 'unit', UnitNome]);
  if ACriar then
    AddManual(Ctx, Regra, An, vOc.Membro, vMarcas, vTrecho, rsCodFazerPlugin, True)
  else
    AddManual(Ctx, Regra, An, vOc.Membro, vMarcas, '', rsCodFazerRemover, True);
  Inc(Total);
end;

function TPlugin.Preparar(AGrupo: TGrupoServidor; const AServidor: string; out ADecl: TDecl;
  ACriacoes: TList): boolean;
var
  vOc: TOcorrencia;
begin
  Result := False;
  ADecl := nil;
  vOc := TOcorrencia(AGrupo.Ocs[0]);
  if vOc.Verdito <> vSim then
    Exit;
  if Pos('.', AServidor) > 0 then
    Exit;
  ADecl := An.BuscarDecl(AServidor, vOc.Ini);
  if (ADecl = nil) or (ADecl.Tipo = '') then
    Exit;
  FCriacaoComplexa := False;
  Result := CriacoesDe(AServidor, ACriacoes) and not FCriacaoComplexa;
end;

procedure TPlugin.Reescrever(AGrupo: TGrupoServidor; const APlugin: string; ACriado: boolean);
var
  vInt, vCob, vSegs, vFim, p, k: integer;
  vOc: TOcorrencia;
  vLis, vDest: string;
  vFeito: boolean;
begin
  for vInt := 0 to AGrupo.Ocs.Count - 1 do
  begin
    vOc := TOcorrencia(AGrupo.Ocs[vInt]);
    vLis := AcharListado(vOc.Caminho, Listadas, Mapa, vDest, vCob);
    if vLis = '' then
    begin
      // only a property of the condition (Security.Options): the last rule that
      // reads it removes the statement
      if vOc.Atribuicao and not OutraRegraUsa(Ctx, Regra, vOc.Listado) and RemoverComando(vOc) then
        AddFeitoPlugin(vOc, PreencherMarcas(rsCodRemovido, ['nome', vOc.Caminho]));
      Continue;
    end;
    if ACriado and (vDest <> '') then
    begin
      vSegs := 1;
      for k := 1 to vCob do
        if Copy(vOc.Caminho, k, 1) = '.' then
          Inc(vSegs);
      vFim := vOc.Membro;
      p := vOc.Membro;
      for k := 2 to vSegs do
      begin
        p := An.Toks.Proximo(An.Toks.Proximo(p));
        vFim := p;
      end;
      An.Texto.AdicionarEdicao(An.Toks[vOc.Ini].Inicio,
        An.Toks[vFim].Fim - An.Toks[vOc.Ini].Inicio, APlugin + '.' + vDest);
      AddFeitoPlugin(vOc, PreencherMarcas(rsCodMovido,
        ['de', vOc.Servidor + '.' + vLis, 'para', APlugin + '.' + vDest]));
    end
    else
    begin
      vFeito := RemoverComando(vOc);
      if vFeito then
        AddFeitoPlugin(vOc, PreencherMarcas(rsCodRemovido, ['nome', vOc.Servidor + '.' + vLis]))
      else
      begin
        ManualUm(vOc);
      end;
    end;
  end;
end;

procedure TPlugin.ManualUm(AOc: TOcorrencia);
var
  vMarcas: TMarcas;
begin
  vMarcas := Marcas(['nome', AOc.Servidor + '.' + AOc.Caminho, 'variavel', AOc.Servidor]);
  AddManual(Ctx, Regra, An, AOc.Membro, vMarcas, '', rsCodFazerRemover, True);
  Inc(Total);
end;

procedure TPlugin.TratarGrupo(AGrupo: TGrupoServidor);
var
  vAmb: TStringList;
  vCriacoes: TList;
  vDecl: TDecl;
  vServ, vPlugin: string;
  vInt: integer;
  vOc: TOcorrencia;
  vTemAtrib, vCriar, vOk, vDest: boolean;
  vTmp: string;
begin
  vServ := TOcorrencia(AGrupo.Ocs[0]).Servidor;
  vAmb := TStringList.Create;
  vCriacoes := TList.Create;
  try
    Ambiente(AGrupo, vAmb);
    vTemAtrib := False;
    vDest := False;
    for vInt := 0 to AGrupo.Ocs.Count - 1 do
    begin
      vOc := TOcorrencia(AGrupo.Ocs[vInt]);
      if vOc.Atribuicao then
        vTemAtrib := True;
      if NaLista(vOc.Listado, Listadas) then
        vDest := True;
    end;
    if (TOcorrencia(AGrupo.Ocs[0]).Verdito <> vSim) and not vDest then
      Exit;
    vCriar := vTemAtrib and AvaliarCond(Regra.CriarQuando, vAmb);
    vOk := Preparar(AGrupo, vServ, vDecl, vCriacoes);
    vPlugin := '';
    if vCriar and vOk then
    begin
      vPlugin := PluginExistente(vServ);
      if vPlugin = '' then
      begin
        vPlugin := NovoNomeDePlugin(An, vServ, Componente);
        DeclararCampo(vDecl, vPlugin);
        for vInt := 0 to vCriacoes.Count - 1 do
          InserirCriacao(IntDe(vCriacoes[vInt]), vPlugin, vServ);
        UsesUmaVez(An.Texto, UnitNome, DonoSecao(vDecl));
      end;
      Reescrever(AGrupo, vPlugin, True);
    end
    else if vCriar then
    begin
      vTmp := NovoNomeDePlugin(An, vServ, Componente);
      ManualDoGrupo(AGrupo, vTmp, True);
      An.NomesNovos.Delete(An.NomesNovos.IndexOf(vTmp));
    end
    else if vOk then
      Reescrever(AGrupo, '', False)
    else
      ManualDoGrupo(AGrupo, '', False);
  finally
    vAmb.Free;
    vCriacoes.Free;
  end;
end;

procedure TPlugin.Executar;
var
  vGrupos: TObjectList;
  vInt: integer;
begin
  vGrupos := TObjectList.Create(True);
  try
    ColetarOcorrencias(vGrupos);
    for vInt := 0 to vGrupos.Count - 1 do
      TratarGrupo(TGrupoServidor(vGrupos[vInt]));
    ResolverBracos;
  finally
    vGrupos.Free;
  end;
end;

function PluginEmCodigo(ARegra: TRegraMigracao; AContexto: TContextoMigracao): integer;
var
  vFiles: TList;
  vInt: integer;
  vAn: TAnalise;
  vPlug: TPlugin;
  vArq: TArquivoMigrado;
begin
  Result := 0;
  vFiles := TList.Create;
  try
    ArquivosDeCodigo(ARegra, AContexto, vFiles);
    for vInt := 0 to vFiles.Count - 1 do
    begin
      vArq := TArquivoMigrado(vFiles[vInt]);
      vAn := NovaAnalise(vArq, AContexto);
      try
        vPlug := TPlugin.Create(ARegra, AContexto, vAn);
        try
          try
            vPlug.Executar;
          except
            on E: ETextoEdicao do
              AContexto.Resultado.Erros.Add(ARegra.Id + ': ' + vArq.Relativo + ': ' + E.Message);
          end;
          Inc(Result, vPlug.Total);
        finally
          vPlug.Free;
        end;
      finally
        vAn.Free;
      end;
    end;
  finally
    vFiles.Free;
  end;
end;

initialization
  RegistrarAcao(arIdentificadorRenomear, TAcaoIdentificadorRenomear);
  RegistrarAcao(arPadraoAvisar, TAcaoPadraoAvisar);

finalization
  FreeAndNil(GUsesPedidas);

end.
