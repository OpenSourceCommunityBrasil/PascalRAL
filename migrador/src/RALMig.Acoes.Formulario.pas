/// Form rules of the RAL migrator (stage M4): rename a property, remove a
/// property (a value other than the discardable one becomes a manual item),
/// turn properties of a server into a plugin component (new object in the
/// form, field and unit in the unit next to it), write a default that changed
/// and point at objects with no equivalent. Works on .dfm/.lfm/.fmx text, in
/// memory, through RALMig.Formulario; form inheritance (inherited), frames
/// (inline) and data modules are handled. The code side of the same rules
/// (renaming in code, a server created in code) belongs to RALMig.Acoes.Codigo,
/// called from CodigoRenomearPropriedade and CodigoPluginEmCodigo.
unit RALMig.Acoes.Formulario;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALMig.Texto, RALMig.Tokens, RALMig.Regras, RALMig.API, RALMig.Tipos,
  RALMig.Formulario, RALMig.&Uses;

type
  /// The user classes declared in the .pas of the set: name -> ancestor
  TIndiceClasses = class
  private
    FAncestral: TStringList;
    FArquivo: TStringList;
    procedure LerUnidade(AArq: TArquivoMigrado);
  public
    constructor Create(AConjunto: TConjuntoArquivos);
    destructor Destroy; override;
    /// Ancestor of a class declared in the set, '' when it is not there
    function Ancestral(const AClasse: string): string;
    /// The .pas that declares the class, nil when it is not in the set
    function Arquivo(const AClasse: string): TArquivoMigrado;
  end;

  { TAcaoFormBase }

  /// What the form actions share: the cache of parsed forms, matching of
  /// classes (glob, user descendants), conditions and the report helpers
  TAcaoFormBase = class(TAcaoMigracao)
  private
    FForms: TStringList;
    FIndice: TIndiceClasses;
    FUses: TStringList;
    FUsesFeitos: TStringList;
  protected
    FCtx: TContextoMigracao;
    FRegra: TRegraMigracao;
    procedure Executar; virtual; abstract;
    function Formulario(AArq: TArquivoMigrado): TFormulario;
    procedure ListarFormularios(ALista: TList);
    procedure ColetarNos(ANo: TNoFormulario; ALista: TList);
    function Indice: TIndiceClasses;
    function ClasseConhecida(const AClasse: string): boolean;
    function ClasseCasa(const AGlob, AClasse: string): boolean;
    function Descende(const AClasse, AAlvo: string): boolean;
    function Avaliar(ACond: TCondicaoRegra; ANo: TNoFormulario;
      AArq: TArquivoMigrado): boolean;
    function EditorUses(APas: TArquivoMigrado): TEditorUses;
    procedure AcrescentarUses(APas: TArquivoMigrado; const AUnit: string);
    function NoInline(ANo: TNoFormulario): TNoFormulario;
    function ArquivoDoAncestral(AArq: TArquivoMigrado; ANo: TNoFormulario): TArquivoMigrado;
    procedure Feito(AArq: TArquivoMigrado; ALinha: integer; const ATexto: string);
    procedure Manual(AArq: TArquivoMigrado; ALinha: integer; const AProblema,
      ANome, AClasse, APropriedade, AValor, AComponente, ATrecho: string);
  public
    destructor Destroy; override;
    function Aplicar(ARegra: TRegraMigracao; AContexto: TContextoMigracao): boolean; override;
  end;

  /// propriedade-renomear
  TAcaoPropriedadeRenomear = class(TAcaoFormBase)
  protected
    procedure Executar; override;
  end;

  /// propriedade-remover
  TAcaoPropriedadeRemover = class(TAcaoFormBase)
  protected
    procedure Executar; override;
  end;

  /// valor-padrao-materializar
  TAcaoValorPadraoMaterializar = class(TAcaoFormBase)
  protected
    procedure Executar; override;
  end;

  /// objeto-sem-equivalente
  TAcaoObjetoSemEquivalente = class(TAcaoFormBase)
  protected
    procedure Executar; override;
  end;

  /// propriedade-para-componente
  TAcaoPropriedadeParaComponente = class(TAcaoFormBase)
  private
    FNovos: TStringList;
    FPosicoes: TStringList;
    function ExistentePlugin(AForm: TFormulario; AServ: TNoFormulario): TNoFormulario;
    function Presentes(ANo: TNoFormulario; ALista: TStrings): TStringList;
    procedure RemoverPropriedades(ANo: TNoFormulario; AForm: TFormulario;
      ANomes: TStrings);
    function Parentes(AA, AB: TArquivoMigrado): boolean;
    function AncestralDoArquivo(AArq: TArquivoMigrado): TArquivoMigrado;
    function NomeEmUso(AArq: TArquivoMigrado; const ANome: string): boolean;
    function NomeLivre(AArq: TArquivoMigrado): string;
    function Ocupado(APai: TNoFormulario; AX, AY: integer): boolean;
    procedure PosicaoLivre(AServ: TNoFormulario; out AX, AY: integer);
    function Ancora(AServ: TNoFormulario): TNoFormulario;
    function TextoDoObjeto(ASrv: TNoFormulario; const ANome: string;
      AX, AY: integer; out AValor: string; out AAscii: boolean;
      AAnsi: boolean): string;
    procedure ConsumirOpcoes(ASrv: TNoFormulario; AForm: TFormulario;
      ACond: TCondicaoRegra; ARemovidas: TStrings);
    procedure EditarPas(AArq: TArquivoMigrado; ASrv, AAncora: TNoFormulario;
      const ANome: string);
    procedure Criar(AArq: TArquivoMigrado; AForm: TFormulario; ASrv: TNoFormulario;
      ANomesRemover: TStringList);
    procedure TratarServidor(AArq: TArquivoMigrado; AForm: TFormulario;
      ASrv: TNoFormulario);
    procedure TratarDelegado(AArq: TArquivoMigrado; AForm: TFormulario;
      ASrv: TNoFormulario);
  protected
    procedure Executar; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  RALMig.Acoes.Codigo;

resourcestring
  SErroForm = 'Formulário %s não pôde ser lido (linha %d): %s';
  SFeitoCriado = 'Criado %s (%s) ligado a %s';
  SFeitoRemovidas = '%s: removidas %s';
  SFeitoRenomeada = '%s: propriedade %s renomeada para %s';
  SFeitoMaterializada = '%s: %s = %s escrito explicitamente';
  SFeitoCampo = 'Campo %s: %s acrescentado à classe do formulário';
  SFeitoUses = 'Unit %s acrescentada ao uses';
  SProbDestinoExiste = 'A propriedade %s já existe em %s: não foi renomeada.';
  SProbDescartavel = 'A propriedade %s de %s tem o valor %s, que não é o padrão: não foi removida.';
  SProbSemAncestral = 'O formulário ancestral de %s não está no conjunto: a propriedade não foi tocada.';
  SProbValorDiferente = 'Em %s a propriedade %s (%s) difere da do formulário ancestral e foi removida.';
  SProbPluginExiste = 'O servidor %s já tem um plugin %s e ainda tem propriedades antigas: nada foi movido.';
  SProbClasseNaoProvada = 'Não foi possível provar que a classe %s de %s descende de um servidor RAL.';
  SProbAnsi = 'O valor de %s tem caracteres fora do ASCII num arquivo ANSI: o plugin não foi criado.';
  SProbSemClasse = 'A classe %s não foi achada na unit do formulário: acrescente o campo à mão.';
  SProbSemUnit = 'O formulário %s não tem a unit par no conjunto: acrescente o campo e a unit à mão.';

{ code side of the same rules (RALMig.Acoes.Codigo) }

procedure CodigoRenomearPropriedade(ARegra: TRegraMigracao; AContexto: TContextoMigracao);
begin
  RenomearPropriedadeNoCodigo(ARegra, AContexto);
end;

procedure CodigoPluginEmCodigo(ARegra: TRegraMigracao; AContexto: TContextoMigracao);
begin
  PluginEmCodigo(ARegra, AContexto);
end;

function RotuloDoPasso(AContexto: TContextoMigracao): string;
begin
  Result := '';
  if AContexto.Passo <> nil then
    Result := AContexto.Passo.Rotulo;
end;

const
  PASSO_POSICAO = 56;
  FOLGA_POSICAO = 32;
  PALAVRAS_SECAO = ';private;protected;public;published;strict;automated;';

{ helpers }

function GlobCasa(const AGlob, ATexto: string): boolean;

  function Casa(AG, AT: integer): boolean;
  begin
    while AG <= Length(AGlob) do
    begin
      if AGlob[AG] = '*' then
      begin
        while AT <= Length(ATexto) + 1 do
        begin
          if Casa(AG + 1, AT) then
            Exit(True);
          Inc(AT);
        end;
        Exit(False);
      end;
      if (AT > Length(ATexto)) or
         ((AGlob[AG] <> '?') and (UpCase(AGlob[AG]) <> UpCase(ATexto[AT]))) then
        Exit(False);
      Inc(AG);
      Inc(AT);
    end;
    Result := AT > Length(ATexto);
  end;

begin
  Result := Casa(1, 1);
end;

function SemEspacos(const AValor: string): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 1 to Length(AValor) do
    if not (AValor[vInt] in [' ', #9, #10, #13]) then
      Result := Result + AValor[vInt];
end;

function MesmoValor(const AA, AB: string): boolean;
begin
  Result := SameText(SemEspacos(AA), SemEspacos(AB));
end;

function ValorVazio(const AValor: string): boolean;
var
  vVal: string;
begin
  vVal := SemEspacos(AValor);
  Result := (vVal = '') or (vVal = '''''') or (vVal = '[]') or (vVal = '()') or
            (vVal = '<>');
end;

/// The elements of a set value '[a, b]'
procedure ElementosDoConjunto(const AValor: string; ALista: TStrings);
var
  vTxt: string;
  vInt, vIni: integer;
begin
  vTxt := SemEspacos(AValor);
  if (vTxt <> '') and (vTxt[1] = '[') then
    Delete(vTxt, 1, 1);
  if (vTxt <> '') and (vTxt[Length(vTxt)] = ']') then
    Delete(vTxt, Length(vTxt), 1);
  vIni := 1;
  for vInt := 1 to Length(vTxt) + 1 do
    if (vInt > Length(vTxt)) or (vTxt[vInt] = ',') then
    begin
      if vInt > vIni then
        ALista.Add(Copy(vTxt, vIni, vInt - vIni));
      vIni := vInt + 1;
    end;
end;

function ConjuntoContem(const AValor, AOpcao: string): boolean;
var
  vLista: TStringList;
  vInt: integer;
begin
  Result := False;
  vLista := TStringList.Create;
  try
    ElementosDoConjunto(AValor, vLista);
    for vInt := 0 to vLista.Count - 1 do
      if SameText(vLista[vInt], AOpcao) then
        Exit(True);
  finally
    vLista.Free;
  end;
end;

function ConjuntoSem(const AValor, AOpcao: string): string;
var
  vLista: TStringList;
  vInt: integer;
begin
  vLista := TStringList.Create;
  try
    ElementosDoConjunto(AValor, vLista);
    Result := '';
    for vInt := 0 to vLista.Count - 1 do
      if not SameText(vLista[vInt], AOpcao) then
      begin
        if Result <> '' then
          Result := Result + ', ';
        Result := Result + vLista[vInt];
      end;
    Result := '[' + Result + ']';
  finally
    vLista.Free;
  end;
end;

function TemNaoAscii(const ATexto: string): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := 1 to Length(ATexto) do
    if ATexto[vInt] > #127 then
      Exit(True);
end;

function LinhasDoValor(const AValor: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Text := StringReplace(AValor, #13#10, #10, [rfReplaceAll]);
end;

{ class body in a .pas }

/// Finds `ANome = class(...) ... end;` ; AIni = token of `class`, AFim = token
/// of the closing `end`
function AcharCorpoClasse(ATok: TListaTokens; const ANome: string;
  out AIni, AFim: integer): boolean;
var
  vInt, vDepth, vParen, vAdi: integer;
begin
  Result := False;
  for vInt := 0 to ATok.Count - 1 do
  begin
    if not ATok.EhNomeIgual(vInt, ANome) then
      Continue;
    vAdi := ATok.Proximo(vInt);
    if (vAdi >= ATok.Count) or not ATok.EhSimbolo(vAdi, '=') then
      Continue;
    vAdi := ATok.Proximo(vAdi);
    if (vAdi >= ATok.Count) or not ATok.EhNomeIgual(vAdi, 'class') then
      Continue;
    AIni := vAdi;
    vAdi := ATok.Proximo(vAdi);
    if (vAdi < ATok.Count) and ATok.EhSimbolo(vAdi, '(') then
    begin
      vParen := 1;
      while (vParen > 0) and (vAdi + 1 < ATok.Count) do
      begin
        Inc(vAdi);
        if ATok.EhSimbolo(vAdi, '(') then
          Inc(vParen)
        else if ATok.EhSimbolo(vAdi, ')') then
          Dec(vParen);
      end;
      vAdi := ATok.Proximo(vAdi);
    end;
    if (vAdi < ATok.Count) and ATok.EhSimbolo(vAdi, ';') then
      Continue;
    vDepth := 0;
    vParen := AIni;
    while vParen < ATok.Count - 1 do
    begin
      Inc(vParen);
      if ATok.EhNomeIgual(vParen, 'record') then
        Inc(vDepth)
      else if ATok.EhNomeIgual(vParen, 'end') then
      begin
        if vDepth = 0 then
        begin
          AFim := vParen;
          Exit(True);
        end;
        Dec(vDepth);
      end;
    end;
    Exit(False);
  end;
end;

/// Token of the `;` that ends the field declaration, -1 when there is none.
/// With AAteSecao only the part before the first visibility word is searched.
function AcharCampo(ATok: TListaTokens; AIni, AFim: integer;
  const ANome: string; AAteSecao: boolean): integer;
var
  vInt, vParen, vProx, vFim: integer;
begin
  Result := -1;
  vParen := 0;
  for vInt := AIni + 1 to AFim - 1 do
  begin
    if ATok.EhSimbolo(vInt, '(') then
      Inc(vParen)
    else if ATok.EhSimbolo(vInt, ')') then
      Dec(vParen)
    else if (vParen = 0) and (ATok[vInt].Tipo = ttIdentificador) then
    begin
      if AAteSecao and (Pos(';' + LowerCase(ATok.Texto(vInt)) + ';', PALAVRAS_SECAO) > 0) then
        Exit;
      if ATok.EhNomeIgual(vInt, ANome) then
      begin
        vProx := ATok.Proximo(vInt);
        if (vProx < ATok.Count) and (ATok.EhSimbolo(vProx, ':') or ATok.EhSimbolo(vProx, ',')) then
        begin
          vFim := vProx;
          while (vFim < AFim) and not ATok.EhSimbolo(vFim, ';') do
            Inc(vFim);
          if vFim < AFim then
            Exit(vFim);
        end;
      end;
    end;
  end;
end;

/// Token where the first visibility section starts (AFim when none)
function AcharPrimeiraSecao(ATok: TListaTokens; AIni, AFim: integer): integer;
var
  vInt: integer;
begin
  Result := AFim;
  for vInt := AIni + 1 to AFim - 1 do
    if (ATok[vInt].Tipo = ttIdentificador) and
       (Pos(';' + LowerCase(ATok.Texto(vInt)) + ';', PALAVRAS_SECAO) > 0) then
      Exit(vInt);
end;

{ TIndiceClasses }

constructor TIndiceClasses.Create(AConjunto: TConjuntoArquivos);
var
  vLista: TList;
  vInt: integer;
begin
  inherited Create;
  FAncestral := TStringList.Create;
  FArquivo := TStringList.Create;
  FArquivo.CaseSensitive := False;
  vLista := TList.Create;
  try
    AConjunto.Listar([taPas], vLista);
    for vInt := 0 to vLista.Count - 1 do
      LerUnidade(TArquivoMigrado(vLista[vInt]));
  finally
    vLista.Free;
  end;
end;

destructor TIndiceClasses.Destroy;
begin
  FAncestral.Free;
  FArquivo.Free;
  inherited Destroy;
end;

procedure TIndiceClasses.LerUnidade(AArq: TArquivoMigrado);
var
  vTok: TListaTokens;
  vInt, vProx: integer;
  vNome, vAnc: string;
begin
  vTok := TokenizarPascal(AArq.Texto.Bytes, AArq.Texto.InicioConteudo);
  try
    for vInt := 0 to vTok.Count - 1 do
    begin
      if vTok[vInt].Tipo <> ttIdentificador then
        Continue;
      vNome := vTok.Texto(vInt);
      vProx := vTok.Proximo(vInt);
      if (vProx >= vTok.Count) or not vTok.EhSimbolo(vProx, '=') then
        Continue;
      vProx := vTok.Proximo(vProx);
      if (vProx >= vTok.Count) or not vTok.EhNomeIgual(vProx, 'class') then
        Continue;
      vProx := vTok.Proximo(vProx);
      vAnc := '';
      if (vProx < vTok.Count) and vTok.EhSimbolo(vProx, '(') then
      begin
        vProx := vTok.Proximo(vProx);
        while (vProx < vTok.Count) and vTok.EhNome(vProx) do
        begin
          vAnc := vTok.Texto(vProx);
          vProx := vTok.Proximo(vProx);
          if (vProx < vTok.Count) and vTok.EhSimbolo(vProx, '.') then
            vProx := vTok.Proximo(vProx)
          else
            Break;
        end;
      end;
      if (vAnc <> '') and (FAncestral.IndexOfName(vNome) < 0) then
      begin
        FAncestral.Add(vNome + '=' + vAnc);
        FArquivo.AddObject(vNome, AArq);
      end;
    end;
  finally
    vTok.Free;
  end;
end;

function TIndiceClasses.Ancestral(const AClasse: string): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 0 to FAncestral.Count - 1 do
    if SameText(FAncestral.Names[vInt], AClasse) then
      Exit(FAncestral.ValueFromIndex[vInt]);
end;

function TIndiceClasses.Arquivo(const AClasse: string): TArquivoMigrado;
var
  vInd: integer;
begin
  Result := nil;
  vInd := FArquivo.IndexOf(AClasse);
  if vInd >= 0 then
    Result := TArquivoMigrado(FArquivo.Objects[vInd]);
end;

{ TAcaoFormBase }

destructor TAcaoFormBase.Destroy;
begin
  FForms.Free;
  FIndice.Free;
  FUses.Free;
  FUsesFeitos.Free;
  inherited Destroy;
end;

function TAcaoFormBase.Aplicar(ARegra: TRegraMigracao; AContexto: TContextoMigracao): boolean;
begin
  FRegra := ARegra;
  FCtx := AContexto;
  FreeAndNil(FForms);
  FreeAndNil(FIndice);
  FreeAndNil(FUses);
  FreeAndNil(FUsesFeitos);
  FForms := TStringList.Create;
  FForms.OwnsObjects := True;
  FForms.CaseSensitive := False;
  FUses := TStringList.Create;
  FUses.OwnsObjects := True;
  FUsesFeitos := TStringList.Create;
  Executar;
  Result := True;
end;

function TAcaoFormBase.Formulario(AArq: TArquivoMigrado): TFormulario;
var
  vInd: integer;
begin
  vInd := FForms.IndexOf(AArq.Caminho);
  if vInd >= 0 then
    Exit(TFormulario(FForms.Objects[vInd]));
  Result := nil;
  try
    Result := TFormulario.Create(AArq.Texto);
  except
    on E: EFormularioInvalido do
      FCtx.Resultado.Erros.Add(Format(SErroForm, [AArq.Relativo, E.Linha, E.Message]));
  end;
  FForms.AddObject(AArq.Caminho, Result);
end;

procedure TAcaoFormBase.ListarFormularios(ALista: TList);
begin
  if taForm in TiposDaRegra(FRegra) then
    FCtx.Conjunto.Listar([taForm], ALista);
end;

procedure TAcaoFormBase.ColetarNos(ANo: TNoFormulario; ALista: TList);
var
  vInt: integer;
begin
  ALista.Add(ANo);
  for vInt := 0 to ANo.FilhoCount - 1 do
    ColetarNos(ANo.Filhos[vInt], ALista);
end;

function TAcaoFormBase.Indice: TIndiceClasses;
begin
  if FIndice = nil then
    FIndice := TIndiceClasses.Create(FCtx.Conjunto);
  Result := FIndice;
end;

function TAcaoFormBase.ClasseConhecida(const AClasse: string): boolean;
begin
  Result := ((FCtx.ApiDe <> nil) and (FCtx.ApiDe.BuscarClasse(AClasse) <> nil)) or
            ((FCtx.ApiPara <> nil) and (FCtx.ApiPara.BuscarClasse(AClasse) <> nil));
end;

function TAcaoFormBase.ClasseCasa(const AGlob, AClasse: string): boolean;
var
  vClasse, vAnc: string;
  vInt: integer;
begin
  Result := GlobCasa(AGlob, AClasse);
  if Result or ClasseConhecida(AClasse) then
    Exit;
  vClasse := AClasse;
  for vInt := 1 to 20 do
  begin
    vAnc := Indice.Ancestral(vClasse);
    if vAnc = '' then
      Exit(False);
    if ClasseConhecida(vAnc) then
      Exit(GlobCasa(AGlob, vAnc));
    vClasse := vAnc;
  end;
end;

function TAcaoFormBase.Descende(const AClasse, AAlvo: string): boolean;
var
  vClasse, vAnc: string;
  vInt: integer;
  vCls: TApiClasse;

  function Buscar(const ANome: string): TApiClasse;
  begin
    Result := nil;
    if FCtx.ApiPara <> nil then
      Result := FCtx.ApiPara.BuscarClasse(ANome);
    if (Result = nil) and (FCtx.ApiDe <> nil) then
      Result := FCtx.ApiDe.BuscarClasse(ANome);
  end;

begin
  vClasse := AClasse;
  for vInt := 1 to 40 do
  begin
    if SameText(vClasse, AAlvo) then
      Exit(True);
    vCls := Buscar(vClasse);
    if vCls <> nil then
      vAnc := vCls.Ancestral
    else
      vAnc := Indice.Ancestral(vClasse);
    if vAnc = '' then
      Exit(False);
    vClasse := vAnc;
  end;
  Result := False;
end;

function TAcaoFormBase.EditorUses(APas: TArquivoMigrado): TEditorUses;
var
  vInd: integer;
begin
  vInd := FUses.IndexOf(APas.Caminho);
  if vInd < 0 then
    vInd := FUses.AddObject(APas.Caminho, TEditorUses.Create(APas.Texto));
  Result := TEditorUses(FUses.Objects[vInd]);
end;

procedure TAcaoFormBase.AcrescentarUses(APas: TArquivoMigrado; const AUnit: string);
var
  vChave: string;
  vEd: TEditorUses;
begin
  vChave := LowerCase(APas.Caminho + '|' + AUnit);
  if FUsesFeitos.IndexOf(vChave) >= 0 then
    Exit;
  FUsesFeitos.Add(vChave);
  vEd := EditorUses(APas);
  if vEd.ContemEm(AUnit, suInterface) then
    Exit;
  // o lado do codigo da mesma regra pode pedir a mesma unit antes de o motor
  // consolidar: os dois passam por UsesUmaVez para sair uma entrada so
  UsesUmaVez(APas.Texto, AUnit, suInterface);
  Feito(APas, 1, Format(SFeitoUses, [AUnit]));
end;

function TAcaoFormBase.Avaliar(ACond: TCondicaoRegra; ANo: TNoFormulario;
  AArq: TArquivoMigrado): boolean;
var
  vProp: TPropriedadeForm;
  vInt: integer;
  vPas: TArquivoMigrado;
begin
  if ACond = nil then
    Exit(True);
  vProp := ANo.Propriedade(ACond.Propriedade);
  case ACond.Tipo of
    tcSempre: Result := True;
    tcTemPropriedade: Result := vProp <> nil;
    tcValorIgual: Result := (vProp <> nil) and MesmoValor(vProp.ValorBruto, ACond.Valor);
    tcValorDiferente: Result := (vProp <> nil) and not MesmoValor(vProp.ValorBruto, ACond.Valor);
    tcValorNaoVazio: Result := (vProp <> nil) and not ValorVazio(vProp.ValorBruto);
    tcSetContem: Result := (vProp <> nil) and ConjuntoContem(vProp.ValorBruto, ACond.Opcao);
    tcClasseDescendeDe: Result := Descende(ANo.Classe, ACond.Classe);
    tcUnitNoUses:
      begin
        vPas := FCtx.Conjunto.UnidadeDe(AArq);
        Result := (vPas <> nil) and EditorUses(vPas).Contem(ACond.NomeUnit);
      end;
    tcAlguma, tcTodas:
      begin
        Result := ACond.Tipo = tcTodas;
        for vInt := 0 to ACond.FilhoCount - 1 do
          if Avaliar(ACond.Filhos[vInt], ANo, AArq) = (ACond.Tipo = tcAlguma) then
          begin
            Result := ACond.Tipo = tcAlguma;
            Break;
          end;
      end;
  { alguma: true at the first true child; todas: false at the first false one }
  else
    Result := False;
  end;
  if ACond.Negar then
    Result := not Result;
end;

function TAcaoFormBase.NoInline(ANo: TNoFormulario): TNoFormulario;
begin
  Result := ANo.Pai;
  while (Result <> nil) and (Result.Tipo <> tnInline) do
    Result := Result.Pai;
end;

function TAcaoFormBase.ArquivoDoAncestral(AArq: TArquivoMigrado;
  ANo: TNoFormulario): TArquivoMigrado;
var
  vInl, vRaiz: TNoFormulario;
  vPas, vPasAnc: TArquivoMigrado;
  vAnc: string;
begin
  Result := nil;
  vInl := NoInline(ANo);
  if vInl <> nil then
  begin
    vPasAnc := Indice.Arquivo(vInl.Classe);
    if vPasAnc <> nil then
      Result := FCtx.Conjunto.FormularioDe(vPasAnc);
    Exit;
  end;
  vRaiz := ANo;
  while vRaiz.Pai <> nil do
    vRaiz := vRaiz.Pai;
  if vRaiz.Tipo <> tnInherited then
    Exit;
  vPas := FCtx.Conjunto.UnidadeDe(AArq);
  if vPas = nil then
    Exit;
  vAnc := Indice.Ancestral(vRaiz.Classe);
  vPasAnc := Indice.Arquivo(vAnc);
  if vPasAnc <> nil then
    Result := FCtx.Conjunto.FormularioDe(vPasAnc);
end;

procedure TAcaoFormBase.Feito(AArq: TArquivoMigrado; ALinha: integer; const ATexto: string);
begin
  FCtx.Resultado.AdicionarFeito(FRegra.Id, AArq.Relativo, ALinha, ATexto);
end;

procedure TAcaoFormBase.Manual(AArq: TArquivoMigrado; ALinha: integer; const AProblema,
  ANome, AClasse, APropriedade, AValor, AComponente, ATrecho: string);
var
  vProblema, vFazer: string;
begin
  vFazer := TextoRegra(FRegra, 'manual', FCtx, ['arquivo', AArq.Relativo, 'linha',
    IntToStr(ALinha), 'nome', ANome, 'classe', AClasse, 'propriedade', APropriedade,
    'valor', AValor, 'componente', AComponente, 'unit', FRegra.Param('unit'),
    'alternativa', FRegra.Param('alternativa'), 'para', FRegra.Param('para')]);
  vProblema := AProblema;
  if vProblema = '' then
    vProblema := TextoRegra(FRegra, 'descricao', FCtx, ['nome', ANome, 'classe', AClasse,
      'propriedade', APropriedade, 'valor', AValor, 'arquivo', AArq.Relativo,
      'linha', IntToStr(ALinha)]);
  FCtx.Resultado.AdicionarManual(FRegra.Id, AArq.Relativo, ALinha, vProblema, vFazer,
    ATrecho, FRegra.Wiki);
end;

{ propriedade-renomear }

procedure TAcaoPropriedadeRenomear.Executar;
var
  vForms, vNos: TList;
  vInt, vNo: integer;
  vArq: TArquivoMigrado;
  vFrm: TFormulario;
  vProp: TPropriedadeForm;
  vObj: TNoFormulario;
begin
  vForms := TList.Create;
  vNos := TList.Create;
  try
    ListarFormularios(vForms);
    for vInt := 0 to vForms.Count - 1 do
    begin
      vArq := TArquivoMigrado(vForms[vInt]);
      vFrm := Formulario(vArq);
      if vFrm = nil then
        Continue;
      vNos.Clear;
      ColetarNos(vFrm.Raiz, vNos);
      for vNo := 0 to vNos.Count - 1 do
      begin
        vObj := TNoFormulario(vNos[vNo]);
        vProp := vObj.Propriedade(FRegra.Param('propriedade'));
        if (vProp = nil) or not ClasseCasa(FRegra.Param('classe'), vObj.Classe) then
          Continue;
        if vObj.Propriedade(FRegra.Param('para')) <> nil then
        begin
          Manual(vArq, vProp.LinhaInicio, Format(SProbDestinoExiste,
            [FRegra.Param('para'), vObj.Nome]), vObj.Nome, vObj.Classe, vProp.Nome,
            vProp.ValorBruto, '', '');
          Continue;
        end;
        vFrm.RenomearPropriedade(vProp, FRegra.Param('para'));
        Feito(vArq, vProp.LinhaInicio, Format(SFeitoRenomeada,
          [vObj.Nome, vProp.Nome, FRegra.Param('para')]));
      end;
    end;
  finally
    vNos.Free;
    vForms.Free;
  end;
  CodigoRenomearPropriedade(FRegra, FCtx);
end;

{ propriedade-remover }

procedure TAcaoPropriedadeRemover.Executar;
var
  vForms, vNos: TList;
  vInt, vNo, vP: integer;
  vArq: TArquivoMigrado;
  vFrm: TFormulario;
  vObj: TNoFormulario;
  vProp: TPropriedadeForm;
  vRemovidas, vLista: TStringList;
  vMapa: TStringList;
  vIdx, vK: integer;
  vOk: boolean;
begin
  vForms := TList.Create;
  vNos := TList.Create;
  vRemovidas := TStringList.Create;
  vLista := TStringList.Create;
  try
    vMapa := FRegra.Mapa('valores-descartaveis');
    ListarFormularios(vForms);
    for vInt := 0 to vForms.Count - 1 do
    begin
      vArq := TArquivoMigrado(vForms[vInt]);
      vFrm := Formulario(vArq);
      if vFrm = nil then
        Continue;
      vNos.Clear;
      ColetarNos(vFrm.Raiz, vNos);
      for vNo := 0 to vNos.Count - 1 do
      begin
        vObj := TNoFormulario(vNos[vNo]);
        if not ClasseCasa(FRegra.Param('classe'), vObj.Classe) then
          Continue;
        vRemovidas.Clear;
        for vP := 0 to FRegra.Lista('propriedades').Count - 1 do
        begin
          vProp := vObj.Propriedade(FRegra.Lista('propriedades')[vP]);
          if vProp = nil then
            Continue;
          vIdx := vMapa.IndexOfName(vProp.Nome);
          vOk := vIdx < 0;
          if not vOk then
          begin
            vLista.CommaText := vMapa.ValueFromIndex[vIdx];
            for vK := 0 to vLista.Count - 1 do
              if MesmoValor(vLista[vK], vProp.ValorBruto) then
                vOk := True;
          end;
          if vOk then
          begin
            vFrm.TirarPropriedade(vProp);
            vRemovidas.Add(vProp.Nome);
          end
          else
            Manual(vArq, vProp.LinhaInicio, Format(SProbDescartavel,
              [vProp.Nome, vObj.Nome, vProp.ValorBruto]), vObj.Nome, vObj.Classe,
              vProp.Nome, vProp.ValorBruto, '', '');
        end;
        if vRemovidas.Count > 0 then
          Feito(vArq, vObj.LinhaInicio, Format(SFeitoRemovidas,
            [vObj.Nome, StringReplace(vRemovidas.CommaText, ',', ', ', [rfReplaceAll])]));
      end;
    end;
  finally
    vLista.Free;
    vRemovidas.Free;
    vNos.Free;
    vForms.Free;
  end;
end;

{ valor-padrao-materializar }

procedure TAcaoValorPadraoMaterializar.Executar;
var
  vForms, vNos: TList;
  vInt, vNo: integer;
  vArq: TArquivoMigrado;
  vFrm: TFormulario;
  vObj: TNoFormulario;
  vModo, vProp, vValor, vTxt: string;
begin
  vForms := TList.Create;
  vNos := TList.Create;
  try
    vModo := FRegra.Param('modo', 'escrever');
    vProp := FRegra.Param('propriedade');
    vValor := FRegra.Param('valor');
    ListarFormularios(vForms);
    for vInt := 0 to vForms.Count - 1 do
    begin
      vArq := TArquivoMigrado(vForms[vInt]);
      vFrm := Formulario(vArq);
      if vFrm = nil then
        Continue;
      vNos.Clear;
      ColetarNos(vFrm.Raiz, vNos);
      for vNo := 0 to vNos.Count - 1 do
      begin
        vObj := TNoFormulario(vNos[vNo]);
        if (vObj.Tipo = tnInherited) or (NoInline(vObj) <> nil) or
           (vObj.Propriedade(vProp) <> nil) or
           not ClasseCasa(FRegra.Param('classe'), vObj.Classe) or
           not Avaliar(FRegra.Condicao, vObj, vArq) then
          Continue;
        if vModo = 'perguntar' then
          Manual(vArq, vObj.LinhaInicio, '', vObj.Nome, vObj.Classe, vProp, vValor, '', '')
        else if vModo = 'so-relatar' then
        begin
          vTxt := TextoRegra(FRegra, 'manual', FCtx, ['arquivo', vArq.Relativo, 'nome', vObj.Nome]);
          FCtx.Resultado.AdicionarComportamento(FRegra.Id, RotuloDoPasso(FCtx),
            TextoRegra(FRegra, 'descricao', FCtx, []), vTxt, FRegra.Wiki);
        end
        else
        begin
          vFrm.InserirPropriedade(vObj, vProp + ' = ' + vValor);
          Feito(vArq, vObj.LinhaInicio, Format(SFeitoMaterializada, [vObj.Nome, vProp, vValor]));
          vTxt := TextoRegra(FRegra, 'comportamento', FCtx, ['nome', vObj.Nome]);
          if vTxt <> '' then
            FCtx.Resultado.AdicionarComportamento(FRegra.Id, RotuloDoPasso(FCtx), vTxt,
              '', FRegra.Wiki);
        end;
      end;
    end;
  finally
    vNos.Free;
    vForms.Free;
  end;
end;

{ objeto-sem-equivalente }

procedure TAcaoObjetoSemEquivalente.Executar;
var
  vForms, vNos: TList;
  vInt, vNo, vC: integer;
  vArq: TArquivoMigrado;
  vFrm: TFormulario;
  vObj: TNoFormulario;
  vTok: TListaTokens;
  vIdx: integer;
begin
  vForms := TList.Create;
  vNos := TList.Create;
  try
    ListarFormularios(vForms);
    for vInt := 0 to vForms.Count - 1 do
    begin
      vArq := TArquivoMigrado(vForms[vInt]);
      vFrm := Formulario(vArq);
      if vFrm = nil then
        Continue;
      vNos.Clear;
      ColetarNos(vFrm.Raiz, vNos);
      for vNo := 0 to vNos.Count - 1 do
      begin
        vObj := TNoFormulario(vNos[vNo]);
        for vC := 0 to FRegra.Lista('classes').Count - 1 do
          if GlobCasa(FRegra.Lista('classes')[vC], vObj.Classe) then
            Manual(vArq, vObj.LinhaInicio, '', vObj.Classe, vObj.Classe, '', '', vObj.Nome, '');
      end;
    end;
    vForms.Clear;
    if (taPas in TiposDaRegra(FRegra)) and (FRegra.Lista('identificadores').Count > 0) then
    begin
      FCtx.Conjunto.Listar([taPas], vForms);
      for vInt := 0 to vForms.Count - 1 do
      begin
        vArq := TArquivoMigrado(vForms[vInt]);
        vTok := TokenizarPascal(vArq.Texto.Bytes, vArq.Texto.InicioConteudo);
        try
          for vC := 0 to FRegra.Lista('identificadores').Count - 1 do
          begin
            vIdx := vTok.AcharIdentificador(FRegra.Lista('identificadores')[vC]);
            while vIdx >= 0 do
            begin
              Manual(vArq, vArq.Texto.LinhaDoOffset(vTok[vIdx].Inicio), '',
                FRegra.Lista('identificadores')[vC], '', '', '', '', '');
              vIdx := vTok.AcharIdentificador(FRegra.Lista('identificadores')[vC], vIdx + 1);
            end;
          end;
        finally
          vTok.Free;
        end;
      end;
    end;
  finally
    vNos.Free;
    vForms.Free;
  end;
end;

{ propriedade-para-componente }

constructor TAcaoPropriedadeParaComponente.Create;
begin
  inherited Create;
  FNovos := TStringList.Create;
  FPosicoes := TStringList.Create;
end;

destructor TAcaoPropriedadeParaComponente.Destroy;
begin
  FNovos.Free;
  FPosicoes.Free;
  inherited Destroy;
end;

function TAcaoPropriedadeParaComponente.ExistentePlugin(AForm: TFormulario;
  AServ: TNoFormulario): TNoFormulario;
var
  vNos: TList;
  vInt: integer;
  vNo: TNoFormulario;
begin
  Result := nil;
  vNos := TList.Create;
  try
    ColetarNos(AForm.Raiz, vNos);
    for vInt := 0 to vNos.Count - 1 do
    begin
      vNo := TNoFormulario(vNos[vInt]);
      if SameText(vNo.Classe, FRegra.Param('componente')) and
         SameText(Trim(vNo.Valor(FRegra.Param('ligacao', 'Server'))), AServ.Nome) then
        Exit(vNo);
    end;
  finally
    vNos.Free;
  end;
end;

/// The names of ALista that the object has, in the order of the object
function TAcaoPropriedadeParaComponente.Presentes(ANo: TNoFormulario;
  ALista: TStrings): TStringList;
var
  vInt: integer;
begin
  Result := TStringList.Create;
  for vInt := 0 to ANo.PropriedadeCount - 1 do
    if ALista.IndexOf(ANo.Propriedades[vInt].Nome) >= 0 then
      Result.Add(ANo.Propriedades[vInt].Nome);
end;

procedure TAcaoPropriedadeParaComponente.RemoverPropriedades(ANo: TNoFormulario;
  AForm: TFormulario; ANomes: TStrings);
var
  vInt: integer;
  vProp: TPropriedadeForm;
begin
  for vInt := 0 to ANomes.Count - 1 do
  begin
    vProp := ANo.Propriedade(ANomes[vInt]);
    if vProp <> nil then
      AForm.TirarPropriedade(vProp);
  end;
end;

function TAcaoPropriedadeParaComponente.AncestralDoArquivo(AArq: TArquivoMigrado): TArquivoMigrado;
var
  vFrm: TFormulario;
begin
  Result := nil;
  vFrm := Formulario(AArq);
  if (vFrm <> nil) and (vFrm.RaizCount > 0) and (vFrm.Raiz.Tipo = tnInherited) then
    Result := ArquivoDoAncestral(AArq, vFrm.Raiz);
end;

function TAcaoPropriedadeParaComponente.Parentes(AA, AB: TArquivoMigrado): boolean;
var
  vX: TArquivoMigrado;
  vInt: integer;
begin
  if AA = AB then
    Exit(True);
  vX := AA;
  for vInt := 1 to 8 do
  begin
    if vX <> nil then
      vX := AncestralDoArquivo(vX);
    if (vX <> nil) and (vX = AB) then
      Exit(True);
  end;
  vX := AB;
  for vInt := 1 to 8 do
  begin
    if vX <> nil then
      vX := AncestralDoArquivo(vX);
    if (vX <> nil) and (vX = AA) then
      Exit(True);
  end;
  Result := False;
end;

function TAcaoPropriedadeParaComponente.NomeEmUso(AArq: TArquivoMigrado;
  const ANome: string): boolean;
var
  vForms: TList;
  vInt: integer;
  vFrm: TFormulario;
begin
  Result := FNovos.IndexOf(ANome) >= 0;
  if Result then
    Exit;
  vForms := TList.Create;
  try
    FCtx.Conjunto.Listar([taForm], vForms);
    for vInt := 0 to vForms.Count - 1 do
      if Parentes(AArq, TArquivoMigrado(vForms[vInt])) then
      begin
        vFrm := Formulario(TArquivoMigrado(vForms[vInt]));
        if (vFrm <> nil) and (vFrm.AcharObjeto(ANome) <> nil) then
          Exit(True);
      end;
  finally
    vForms.Free;
  end;
end;

function TAcaoPropriedadeParaComponente.NomeLivre(AArq: TArquivoMigrado): string;
var
  vNum: integer;
begin
  vNum := 1;
  repeat
    Result := FRegra.Param('nome-base') + IntToStr(vNum);
    Inc(vNum);
  until not NomeEmUso(AArq, Result);
end;

function TAcaoPropriedadeParaComponente.Ocupado(APai: TNoFormulario; AX, AY: integer): boolean;
var
  vInt: integer;
  vFil: TNoFormulario;
begin
  Result := FPosicoes.IndexOf(IntToStr(PtrInt(APai)) + ',' + IntToStr(AX) + ',' + IntToStr(AY)) >= 0;
  if Result then
    Exit;
  for vInt := 0 to APai.FilhoCount - 1 do
  begin
    vFil := APai.Filhos[vInt];
    if (vFil.Propriedade('Width') <> nil) or (vFil.Propriedade('Height') <> nil) then
      Continue;
    if (Abs(StrToIntDef(Trim(vFil.Valor('Left')), 0) - AX) < FOLGA_POSICAO) and
       (Abs(StrToIntDef(Trim(vFil.Valor('Top')), 0) - AY) < FOLGA_POSICAO) then
      Exit(True);
  end;
end;

procedure TAcaoPropriedadeParaComponente.PosicaoLivre(AServ: TNoFormulario;
  out AX, AY: integer);
var
  vInt, vTop: integer;
begin
  AX := StrToIntDef(Trim(AServ.Valor('Left')), 0);
  vTop := StrToIntDef(Trim(AServ.Valor('Top')), 0);
  AY := vTop;
  for vInt := 1 to 500 do
  begin
    AY := vTop + vInt * PASSO_POSICAO;
    if not Ocupado(AServ.Pai, AX, AY) then
      Break;
  end;
  FPosicoes.Add(IntToStr(PtrInt(AServ.Pai)) + ',' + IntToStr(AX) + ',' + IntToStr(AY));
end;

/// The sibling after which the plugin goes: the server, or the last of the
/// plugins that follow it already linked to it
function TAcaoPropriedadeParaComponente.Ancora(AServ: TNoFormulario): TNoFormulario;
var
  vPai: TNoFormulario;
  vInt, vPos: integer;
begin
  Result := AServ;
  vPai := AServ.Pai;
  vPos := -1;
  for vInt := 0 to vPai.FilhoCount - 1 do
    if vPai.Filhos[vInt] = AServ then
      vPos := vInt;
  for vInt := vPos + 1 to vPai.FilhoCount - 1 do
    if SameText(Trim(vPai.Filhos[vInt].Valor(FRegra.Param('ligacao', 'Server'))), AServ.Nome) then
      Result := vPai.Filhos[vInt]
    else
      Break;
end;

function TAcaoPropriedadeParaComponente.TextoDoObjeto(ASrv: TNoFormulario;
  const ANome: string; AX, AY: integer; out AValor: string; out AAscii: boolean;
  AAnsi: boolean): string;
var
  vInt, vL: integer;
  vProp: TPropriedadeForm;
  vDest, vVal, vPrim: string;
  vLin: TStringList;
  vRecuo: integer;
begin
  AAscii := True;
  AValor := '';
  Result := 'object ' + ANome + ': ' + FRegra.Param('componente') + #10 + '  ' +
            FRegra.Param('ligacao', 'Server') + ' = ' + ASrv.Nome;
  for vInt := 0 to ASrv.PropriedadeCount - 1 do
  begin
    vProp := ASrv.Propriedades[vInt];
    vDest := FRegra.Mapa('mapeamento').Values[vProp.Nome];
    if (FRegra.Lista('propriedades').IndexOf(vProp.Nome) < 0) or (vDest = '') then
      Continue;
    if AValor = '' then
      AValor := vProp.ValorBruto;
    vVal := FRegra.Mapa('omitir-valores').Values[vDest];
    if (vVal <> '') and MesmoValor(vVal, vProp.ValorBruto) then
      Continue;
    if AAnsi and TemNaoAscii(vProp.ValorBruto) then
      AAscii := False;
    vLin := LinhasDoValor(vProp.ValorBruto);
    try
      vPrim := vLin[0];
      Result := Result + #10 + '  ' + vDest + ' = ' + vPrim;
      vRecuo := Length(vProp.Pai.Indentacao) + 2;
      for vL := 1 to vLin.Count - 1 do
        Result := Result + #10 + '  ' + Copy(vLin[vL], vRecuo + 1, MaxInt);
    finally
      vLin.Free;
    end;
  end;
  Result := Result + #10 + '  Left = ' + IntToStr(AX) + #10 + '  Top = ' + IntToStr(AY) +
            #10 + 'end';
end;

procedure TAcaoPropriedadeParaComponente.ConsumirOpcoes(ASrv: TNoFormulario;
  AForm: TFormulario; ACond: TCondicaoRegra; ARemovidas: TStrings);

  procedure Coletar(ACd: TCondicaoRegra; AProps, AOpcoes: TStrings);
  var
    vI: integer;
  begin
    if ACd = nil then
      Exit;
    if (ACd.Tipo = tcSetContem) and not ACd.Negar then
    begin
      AProps.Add(ACd.Propriedade);
      AOpcoes.Add(ACd.Opcao);
    end;
    for vI := 0 to ACd.FilhoCount - 1 do
      Coletar(ACd.Filhos[vI], AProps, AOpcoes);
  end;

var
  vProps, vOpcoes, vValores: TStringList;
  vInt, vIdx: integer;
  vProp: TPropriedadeForm;
  vVal: string;
begin
  vProps := TStringList.Create;
  vOpcoes := TStringList.Create;
  vValores := TStringList.Create;
  try
    Coletar(ACond, vProps, vOpcoes);
    for vInt := 0 to vProps.Count - 1 do
    begin
      vProp := ASrv.Propriedade(vProps[vInt]);
      if (vProp = nil) or (ARemovidas.IndexOf(vProp.Nome) >= 0) then
        Continue;
      vIdx := vValores.IndexOfName(vProp.Nome);
      if vIdx >= 0 then
        vVal := vValores.ValueFromIndex[vIdx]
      else
        vVal := vProp.ValorBruto;
      if ConjuntoContem(vVal, vOpcoes[vInt]) then
        vVal := ConjuntoSem(vVal, vOpcoes[vInt]);
      vValores.Values[vProp.Nome] := vVal;
    end;
    for vInt := 0 to vValores.Count - 1 do
    begin
      vProp := ASrv.Propriedade(vValores.Names[vInt]);
      if not MesmoValor(vProp.ValorBruto, vValores.ValueFromIndex[vInt]) then
        AForm.TrocarValor(vProp, vValores.ValueFromIndex[vInt]);
    end;
  finally
    vValores.Free;
    vOpcoes.Free;
    vProps.Free;
  end;
end;

procedure TAcaoPropriedadeParaComponente.EditarPas(AArq: TArquivoMigrado;
  ASrv, AAncora: TNoFormulario; const ANome: string);
var
  vPas: TArquivoMigrado;
  vTok: TListaTokens;
  vRaiz: TNoFormulario;
  vIni, vFim, vFim2, vLinha, vL, vSec, vInt: integer;
  vNomes: TStringList;
  vIndent, vCampo: string;
begin
  vPas := FCtx.Conjunto.UnidadeDe(AArq);
  vCampo := ANome + ': ' + FRegra.Param('componente') + ';';
  if vPas = nil then
  begin
    Manual(AArq, ASrv.LinhaInicio, Format(SProbSemUnit, [AArq.Relativo]), ASrv.Nome,
      FRegra.Param('componente'), '', '', ANome, vCampo);
    Exit;
  end;
  vRaiz := ASrv;
  while vRaiz.Pai <> nil do
    vRaiz := vRaiz.Pai;
  vTok := TokenizarPascal(vPas.Texto.Bytes, vPas.Texto.InicioConteudo);
  vNomes := TStringList.Create;
  try
    if not AcharCorpoClasse(vTok, vRaiz.Classe, vIni, vFim) then
    begin
      Manual(AArq, ASrv.LinhaInicio, Format(SProbSemClasse, [vRaiz.Classe]), ASrv.Nome,
        FRegra.Param('componente'), '', '', ANome, vCampo);
      Exit;
    end;
    vNomes.Add(ASrv.Nome);
    vInt := 0;
    while (vInt < ASrv.Pai.FilhoCount) and (ASrv.Pai.Filhos[vInt] <> ASrv) do
      Inc(vInt);
    for vInt := vInt + 1 to ASrv.Pai.FilhoCount - 1 do
    begin
      vNomes.Add(ASrv.Pai.Filhos[vInt].Nome);
      if ASrv.Pai.Filhos[vInt] = AAncora then
        Break;
    end;
    vLinha := -1;
    vIndent := '';
    for vInt := 0 to vNomes.Count - 1 do
    begin
      vFim2 := AcharCampo(vTok, vIni, vFim, vNomes[vInt], False);
      if vFim2 >= 0 then
      begin
        vL := vPas.Texto.LinhaDoOffset(vTok[vFim2].Inicio);
        if vL > vLinha then
          vLinha := vL;
        if vIndent = '' then
          vIndent := vPas.Texto.Indentacao(vL);
      end;
    end;
    if vLinha >= 0 then
      vLinha := vLinha + 1
    else
    begin
      vSec := AcharPrimeiraSecao(vTok, vIni, vFim);
      vLinha := vPas.Texto.LinhaDoOffset(vTok[vSec].Inicio);
      vIndent := vPas.Texto.Indentacao(vPas.Texto.LinhaDoOffset(vTok[vIni].Inicio)) + '    ';
    end;
    vPas.Texto.InserirAntesDaLinha(vLinha, vIndent + vCampo);
    Feito(vPas, vLinha, Format(SFeitoCampo, [ANome, FRegra.Param('componente')]));
  finally
    vNomes.Free;
    vTok.Free;
  end;
  AcrescentarUses(vPas, FRegra.Param('unit'));
end;

procedure TAcaoPropriedadeParaComponente.Criar(AArq: TArquivoMigrado; AForm: TFormulario;
  ASrv: TNoFormulario; ANomesRemover: TStringList);
var
  vNome, vTexto, vValor, vTxt: string;
  vX, vY: integer;
  vAscii: boolean;
  vAnc: TNoFormulario;
begin
  vNome := NomeLivre(AArq);
  PosicaoLivre(ASrv, vX, vY);
  vTexto := TextoDoObjeto(ASrv, vNome, vX, vY, vValor, vAscii,
    AArq.Texto.Codificacao = cdAnsi);
  if not vAscii then
  begin
    Manual(AArq, ASrv.LinhaInicio, Format(SProbAnsi, [ASrv.Nome]), ASrv.Nome,
      ASrv.Classe, '', vValor, vNome, '');
    Exit;
  end;
  FNovos.Add(vNome);
  vAnc := Ancora(ASrv);
  AForm.InserirObjeto(ASrv.Pai, vAnc, vTexto);
  RemoverPropriedades(ASrv, AForm, ANomesRemover);
  ConsumirOpcoes(ASrv, AForm, FRegra.CriarQuando, ANomesRemover);
  Feito(AArq, ASrv.LinhaInicio, Format(SFeitoCriado,
    [vNome, FRegra.Param('componente'), ASrv.Nome]));
  if ANomesRemover.Count > 0 then
    Feito(AArq, ASrv.LinhaInicio, Format(SFeitoRemovidas, [ASrv.Nome,
      StringReplace(ANomesRemover.CommaText, ',', ', ', [rfReplaceAll])]));
  vTxt := TextoRegra(FRegra, 'comportamento', FCtx, ['nome', ASrv.Nome, 'componente', vNome]);
  if vTxt <> '' then
    FCtx.Resultado.AdicionarComportamento(FRegra.Id, RotuloDoPasso(FCtx), vTxt, '', FRegra.Wiki);
  if taPas in TiposDaRegra(FRegra) then
    EditarPas(AArq, ASrv, vAnc, vNome);
end;

procedure TAcaoPropriedadeParaComponente.TratarServidor(AArq: TArquivoMigrado;
  AForm: TFormulario; ASrv: TNoFormulario);
var
  vPres, vDesc, vRemover: TStringList;
  vInt: integer;
begin
  vPres := Presentes(ASrv, FRegra.Lista('propriedades'));
  vDesc := Presentes(ASrv, FRegra.Lista('descartar'));
  vRemover := TStringList.Create;
  vRemover.Sorted := True;
  vRemover.Duplicates := dupIgnore;
  try
    for vInt := 0 to vDesc.Count - 1 do
      vRemover.Add(vDesc[vInt]);
    if ExistentePlugin(AForm, ASrv) <> nil then
    begin
      if vPres.Count > 0 then
        Manual(AArq, ASrv.LinhaInicio, Format(SProbPluginExiste,
          [ASrv.Nome, FRegra.Param('componente')]), ASrv.Nome, ASrv.Classe, '', '', '', '');
    end
    else if (FRegra.CriarQuando = nil) or Avaliar(FRegra.CriarQuando, ASrv, AArq) then
    begin
      for vInt := 0 to vPres.Count - 1 do
        vRemover.Add(vPres[vInt]);
      Criar(AArq, AForm, ASrv, vRemover);
      Exit;
    end;
    if vRemover.Count > 0 then
    begin
      RemoverPropriedades(ASrv, AForm, vRemover);
      Feito(AArq, ASrv.LinhaInicio, Format(SFeitoRemovidas, [ASrv.Nome,
        StringReplace(vRemover.CommaText, ',', ', ', [rfReplaceAll])]));
    end;
  finally
    vRemover.Free;
    vDesc.Free;
    vPres.Free;
  end;
end;

procedure TAcaoPropriedadeParaComponente.TratarDelegado(AArq: TArquivoMigrado;
  AForm: TFormulario; ASrv: TNoFormulario);
var
  vPres, vDesc, vTodos: TStringList;
  vAnc: TArquivoMigrado;
  vAncForm: TFormulario;
  vAncNo: TNoFormulario;
  vInt: integer;
begin
  vPres := Presentes(ASrv, FRegra.Lista('propriedades'));
  vDesc := Presentes(ASrv, FRegra.Lista('descartar'));
  vTodos := TStringList.Create;
  vTodos.Sorted := True;
  vTodos.Duplicates := dupIgnore;
  try
    vTodos.AddStrings(vPres);
    vTodos.AddStrings(vDesc);
    if vTodos.Count = 0 then
      Exit;
    vAnc := ArquivoDoAncestral(AArq, ASrv);
    if vAnc = nil then
    begin
      Manual(AArq, ASrv.LinhaInicio, Format(SProbSemAncestral, [AArq.Relativo]),
        ASrv.Nome, ASrv.Classe, '', '', '', '');
      Exit;
    end;
    vAncForm := Formulario(vAnc);
    vAncNo := nil;
    if vAncForm <> nil then
      vAncNo := vAncForm.AcharObjeto(ASrv.Nome);
    for vInt := 0 to vPres.Count - 1 do
      if (vAncNo = nil) or not MesmoValor(vAncNo.Valor(vPres[vInt]), ASrv.Valor(vPres[vInt])) then
        Manual(AArq, ASrv.LinhaInicio, Format(SProbValorDiferente,
          [ASrv.Nome, vPres[vInt], ASrv.Valor(vPres[vInt])]), ASrv.Nome, ASrv.Classe,
          vPres[vInt], ASrv.Valor(vPres[vInt]), '', '');
    RemoverPropriedades(ASrv, AForm, vTodos);
    Feito(AArq, ASrv.LinhaInicio, Format(SFeitoRemovidas, [ASrv.Nome,
      StringReplace(vTodos.CommaText, ',', ', ', [rfReplaceAll])]));
  finally
    vTodos.Free;
    vDesc.Free;
    vPres.Free;
  end;
end;

procedure TAcaoPropriedadeParaComponente.Executar;
var
  vForms, vNos: TList;
  vInt, vNo: integer;
  vArq: TArquivoMigrado;
  vFrm: TFormulario;
  vObj: TNoFormulario;
  vTemProp: boolean;
  vPres: TStringList;
begin
  FNovos.Clear;
  FPosicoes.Clear;
  vForms := TList.Create;
  vNos := TList.Create;
  try
    ListarFormularios(vForms);
    for vInt := 0 to vForms.Count - 1 do
    begin
      vArq := TArquivoMigrado(vForms[vInt]);
      vFrm := Formulario(vArq);
      if (vFrm = nil) or (vFrm.RaizCount = 0) then
        Continue;
      vNos.Clear;
      ColetarNos(vFrm.Raiz, vNos);
      for vNo := 0 to vNos.Count - 1 do
      begin
        vObj := TNoFormulario(vNos[vNo]);
        if not ClasseCasa(FRegra.Param('classe-dona'), vObj.Classe) then
        begin
          vPres := Presentes(vObj, FRegra.Lista('propriedades'));
          vTemProp := (vPres.Count > 0) and not ClasseConhecida(vObj.Classe) and
                      (Indice.Ancestral(vObj.Classe) = '');
          vPres.Free;
          if vTemProp then
            Manual(vArq, vObj.LinhaInicio, Format(SProbClasseNaoProvada,
              [vObj.Classe, vObj.Nome]), vObj.Nome, vObj.Classe, '', '', '', '');
          Continue;
        end;
        if (FRegra.Condicao <> nil) and not Avaliar(FRegra.Condicao, vObj, vArq) then
          Continue;
        if (vObj.Tipo = tnInherited) or (NoInline(vObj) <> nil) then
          TratarDelegado(vArq, vFrm, vObj)
        else
          TratarServidor(vArq, vFrm, vObj);
      end;
    end;
  finally
    vNos.Free;
    vForms.Free;
  end;
  CodigoPluginEmCodigo(FRegra, FCtx);
end;

initialization
  RegistrarAcao(arPropriedadeRenomear, TAcaoPropriedadeRenomear);
  RegistrarAcao(arPropriedadeParaComponente, TAcaoPropriedadeParaComponente);
  RegistrarAcao(arPropriedadeRemover, TAcaoPropriedadeRemover);
  RegistrarAcao(arValorPadraoMaterializar, TAcaoValorPadraoMaterializar);
  RegistrarAcao(arObjetoSemEquivalente, TAcaoObjetoSemEquivalente);

end.
