/// How each third-party dependency is installed, described as data. One JSON
/// recipe per dependency says where to download it from, what it provides (the
/// variables and packages RAL packages declare to require), how to detect it is
/// already installed, and what to do in each IDE. The action vocabulary is
/// CLOSED: variavel, libpath, dpk, lpk. There is no "run this command": the
/// recipe comes from the network, and a recipe able to run anything would make
/// the installer an attack vector. An action or field the installer does not
/// know refuses the whole recipe, by name. Who needs which recipe comes from the
/// RAL packages themselves: requires ('ZComponent', 'indylaz', 'mormot2'), the
/// search path variables ($(mormot2) in SynopseRAL.dproj) and, for what is
/// declared nowhere (uniGUI), the recipe's explicit "pacotes-ral" list. The
/// version may depend on the IDE: "fonte.versoes" is a list of rules with a
/// range (delphi-min/max, lazarus-min/max, fpc-min/max, sistemas) and "versao"
/// or "incompativel"; the first that holds for the IDE decides, and with none
/// "versao" holds. The version each RAL requires comes from the manifest
/// (RALInst.Compatibilidade), before these rules.
///
///   {
///     "nome": "mORMot2", "descricao": "...", "site": "...", "pago": false,
///     "fonte":  { "github": "synopse/mORMot2", "versao": "estavel",
///                 "versoes": [ { "fpc-min": "3.3", "versao": "master" } ] },
///     "extras": [ { "asset": "mormot2static.tgz", "destino": "static" } ],
///     "pacotes-ral": [],
///     "delphi":  { "fornece": { "variaveis": ["mormot2"], "pacotes": [] },
///                  "deteccao": [ { "variavel": "mormot2" } ],
///                  "acoes": [ { "acao": "variavel", "nome": "mormot2",
///                               "valor": "{raiz}/src" },
///                             { "acao": "libpath",
///                               "caminhos": ["$(mormot2)", "$(mormot2)/core"] } ] },
///     "lazarus": { "fornece": { "pacotes": ["mormot2"] },
///                  "deteccao": [ { "pacote": "mormot2" } ],
///                  "acoes": [ { "acao": "lpk", "modo": "link",
///                               "arquivo": "packages/lazarus/mormot2.lpk" } ] }
///   }
unit RALInst.Receitas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, fpjson,
  RALInst.Catalogo, RALInst.IDE;

type
  /// An IDE, compiler and system range, as the manifest and the recipes write
  /// it: "delphi-min": "XE8", "fpc-max": "3.2.2", "sistemas": ["windows",
  /// "linux"]. Delphi by product name (XE8, 10.1, 12); Lazarus and FPC by
  /// number.
  TCondicaoIDE = class
  public
    /// As written ('XE8')
    DelphiMax: string;
    DelphiMin: string;
    FPCMax: string;
    FPCMin: string;
    LazarusMax: string;
    LazarusMin: string;
    Sistemas: TStringList;
    constructor Create;
    destructor Destroy; override;
    /// 'Delphi XE8 or newer', 'FPC up to 3.2.2'
    function Descrever(AIDE: TIDEInstance): string;
    /// False (and the reason) when a field is not valid
    function Ler(AObj: TJSONObject; out AErro: string): boolean;
    /// The condition speaks of this kind of IDE: it has no range at all, or a
    /// range of its kind (fpc-* and lazarus-* only hold for Lazarus)
    function Relevante(AIDE: TIDEInstance): boolean;
    /// The IDE is inside the range. Unknown FPC version: ADesconhecidoAtende
    function Satisfaz(AIDE: TIDEInstance; ADesconhecidoAtende: boolean): boolean;
  end;

  /// Which version of a dependency to use in an IDE range; with Incompativel
  /// set, no version works there and the text says why.
  TRegraVersao = class
  public
    Condicao: TCondicaoIDE;
    /// Another repository for this range (owner/repo); empty = the recipe's
    Github: string;
    Incompativel: string;
    /// Where the rule came from (recipe or manifest), for the log
    Origem: string;
    Versao: string;
    constructor Create;
    destructor Destroy; override;
  end;

  /// What makes an ALREADY INSTALLED copy of the dependency incompatible with
  /// the IDE: one of its files (relative to its root) matches the expression,
  /// and the IDE is in the range. The installer does not touch the user's copy:
  /// it warns and leaves out whoever would need it.
  TVerificacao = class
  public
    Arquivo: string;
    Aviso: string;
    Condicao: TCondicaoIDE;
    Expressao: string;
    constructor Create;
    destructor Destroy; override;
  end;

  /// Kind of recipe action (the closed vocabulary).
  TTipoAcao = (taVariavel, taLibPath, taDpk, taLpk);

  /// One action of a recipe.
  TAcaoReceita = class
  public
    /// lpk: file relative to the root
    Arquivo: string;
    /// libpath: paths, with {raiz} or $(variable)
    Caminhos: TStringList;
    /// lpk: mode 'instalar' (True) or 'link' (only registers); dpk: install
    /// the design packages in the IDE
    Instalar: boolean;
    /// variavel: name
    Nome: string;
    /// dpk: folder of the .dpk files per BDS version ('23.0=packages/Delphi12')
    Pastas: TStringList;
    Tipo: TTipoAcao;
    /// variavel: value ({raiz} = the dependency folder)
    Valor: string;
    constructor Create;
    destructor Destroy; override;
  end;

  /// What a recipe says for one kind of IDE.
  TBlocoIDE = class
  public
    Acoes: TObjectList;
    /// Delphi: files RAL must find in the ALREADY INSTALLED copy (Zeos's
    /// ZComponent.inc, included by RALDBZeos): the IDE library path folders
    /// holding them enter the compilation search path
    Busca: TStringList;
    /// type=value: variavel=mormot2, unidade=mormot.core.base.pas,
    /// dcp=ZComponent, pacote=mormot2
    Deteccao: TStringList;
    Existe: boolean;
    /// Names RAL packages require that this dependency provides
    FornecePacotes: TStringList;
    ForneceVariaveis: TStringList;
    /// Delphi: .dcp files of the dependency (with wildcard: AnyDAC_Comp_D*) the
    /// RAL package compiles against (-LU); without it dcc32 embeds its units in
    /// the .bpl, and the IDE refuses the package beside its own
    PacotesLigados: TStringList;
    /// TVerificacao: installed copy that does not work in this IDE range
    Verificacoes: TObjectList;
    constructor Create;
    destructor Destroy; override;
    /// Action by index
    function Acao(AIndex: integer): TAcaoReceita;
  end;

  /// One dependency recipe.
  TReceita = class
  private
    FDelphi: TBlocoIDE;
    FExtras: TStringList;
    FLazarus: TBlocoIDE;
    FPacotesRAL: TStringList;
    FVersoes: TObjectList;
  public
    Descricao: string;
    /// GitHub repository (owner/repo)
    Github: string;
    Nome: string;
    /// Where the recipe came from (file or resource), for the log
    Origem: string;
    Pago: boolean;
    Site: string;
    /// 'estavel', a tag or a branch
    Versao: string;
    constructor Create;
    destructor Destroy; override;
    /// Does the RAL package need this dependency?
    function Atende(APacote: TPacote): boolean;
    /// The block of a kind of IDE
    function Bloco(ATipo: TTipoPacote): TBlocoIDE;
    /// Free and hosted on GitHub
    function PodeBaixar: boolean;

    property Delphi: TBlocoIDE read FDelphi;
    /// asset=destination|release (release file, extracted into <root>/<dest>)
    property Extras: TStringList read FExtras;
    property Lazarus: TBlocoIDE read FLazarus;
    /// RAL packages that use the dependency without declaring it anywhere
    property PacotesRAL: TStringList read FPacotesRAL;
    /// "fonte.versoes", TRegraVersao in order: the first that holds for the IDE
    /// decides; with none, Versao holds
    property Versoes: TObjectList read FVersoes;
  end;

  /// The loaded recipes, owned by the list.
  TReceitas = class(TObjectList)
  private
    FErros: TStringList;
    function GetItem(AIndex: integer): TReceita;
  public
    constructor Create;
    destructor Destroy; override;
    /// A recipe by name; nil if none
    function Buscar(const ANome: string): TReceita;
    /// The installer default: resources, then the 'receitas' folders beside the
    /// executable and in the data folder (which add or replace)
    procedure CarregarPadrao;
    /// Every *.json in the folder
    procedure CarregarPasta(const APasta: string);
    /// The RCDATA resources RECEITA_* of the executable
    procedure CarregarRecursos;
    /// False (and the reason in Erros) when the JSON is not a valid recipe; a
    /// recipe named as another already loaded replaces it
    function CarregarTexto(const ATexto, AOrigem: string): boolean;
    /// Recipes the packages (and what they require) need, with the names of the
    /// packages requiring each one: Objects = TReceita, text = names
    procedure Exigidas(ACatalogo: TCatalogo; ATipo: TTipoPacote; APacotes: TStrings;
      AResultado: TStrings);

    property Erros: TStringList read FErros;
    property Items[AIndex: integer]: TReceita read GetItem; default;
  end;

const
  /// Fields of a range condition
  CamposCondicao: array[0..6] of string = ('delphi-min', 'delphi-max', 'lazarus-min',
    'lazarus-max', 'fpc-min', 'fpc-max', 'sistemas');

/// True when the text matches the expression ('.' also matches line breaks)
function CasaExpressao(const ATexto, AExpressao: string): boolean;
/// {raiz} replaced by the folder; '/' replaced by the system separator
function ExpandirRaiz(const ATexto, ARaiz: string): string;
/// { "fpc-min": "3.3", "versao": "8.0-patches" } or { ..., "incompativel": ...};
/// nil (and the reason) when not valid. ACamposExtras: the other fields the
/// place accepts besides the condition ("receita" in the manifest)
function LerRegraVersao(AObj: TJSONObject; const AOrigem: string;
  const ACamposExtras: array of string; out AErro: string): TRegraVersao;
/// Splits 'owner/repo:ref' into repository and ref
procedure SepararVersao(const AVersao: string; out AGithub, ARef: string);
/// 'windows', 'linux' or 'darwin': the system the installer runs on, which is
/// where Lazarus installs
function SistemaAtual: string;
/// The version a rule asks, with the repository when it is not the recipe's:
/// 'frones/ZeosLib:master' (':' does not exist in a git ref name)
function VersaoComFonte(const AGithub, AVersao: string): string;

implementation

uses
  jsonparser, RegExpr,
  RALInst.Mensagens, RALInst.Processo;

function VersaoComFonte(const AGithub, AVersao: string): string;
begin
  if AGithub = '' then
    Result := AVersao
  else
    Result := AGithub + ':' + AVersao;
end;

procedure SepararVersao(const AVersao: string; out AGithub, ARef: string);
begin
  AGithub := '';
  ARef := AVersao;
  if Pos(':', AVersao) > 0 then
  begin
    AGithub := Copy(AVersao, 1, Pos(':', AVersao) - 1);
    ARef := Copy(AVersao, Pos(':', AVersao) + 1, MaxInt);
  end;
end;

function CasaExpressao(const ATexto, AExpressao: string): boolean;
var
  vRegex: TRegExpr;
begin
  vRegex := TRegExpr.Create(AExpressao);
  try
    vRegex.ModifierS := True;
    Result := vRegex.Exec(ATexto);
  finally
    vRegex.Free;
  end;
end;

{ TVerificacao }

constructor TVerificacao.Create;
begin
  inherited Create;
  Condicao := TCondicaoIDE.Create;
end;

destructor TVerificacao.Destroy;
begin
  Condicao.Free;
  inherited Destroy;
end;

function SistemaAtual: string;
begin
  {$IF defined(MSWINDOWS)}
  Result := 'windows';
  {$ELSEIF defined(DARWIN)}
  Result := 'darwin';
  {$ELSE}
  Result := 'linux';
  {$ENDIF}
end;

function NomeDelphiValido(const ANome: string): boolean;
begin
  Result := (ANome = '') or (ProdutoPorNome(ANome) >= 0);
end;

function VersaoNumDelphi(const ANome: string): string;
var
  vIdx: integer;
begin
  Result := '';
  vIdx := ProdutoPorNome(ANome);
  if vIdx >= 0 then
    Result := DelphiProdutos[vIdx].VersaoNum;
end;

{ TCondicaoIDE }

constructor TCondicaoIDE.Create;
begin
  inherited Create;
  Sistemas := TStringList.Create;
  Sistemas.CaseSensitive := False;
end;

destructor TCondicaoIDE.Destroy;
begin
  Sistemas.Free;
  inherited Destroy;
end;

function TCondicaoIDE.Ler(AObj: TJSONObject; out AErro: string): boolean;
var
  vLista: TJSONArray;
  vInt: integer;
begin
  AErro := '';
  DelphiMin := AObj.Get('delphi-min', '');
  DelphiMax := AObj.Get('delphi-max', '');
  LazarusMin := AObj.Get('lazarus-min', '');
  LazarusMax := AObj.Get('lazarus-max', '');
  FPCMin := AObj.Get('fpc-min', '');
  FPCMax := AObj.Get('fpc-max', '');
  if not NomeDelphiValido(DelphiMin) then
    AErro := Format(emDelphiDesconhecido, [DelphiMin])
  else if not NomeDelphiValido(DelphiMax) then
    AErro := Format(emDelphiDesconhecido, [DelphiMax]);
  vLista := AObj.Get('sistemas', TJSONArray(nil));
  if vLista <> nil then
    for vInt := 0 to Pred(vLista.Count) do
    begin
      Sistemas.Add(LowerCase(vLista.Items[vInt].AsString));
      if (Sistemas[vInt] <> 'windows') and (Sistemas[vInt] <> 'linux') and
         (Sistemas[vInt] <> 'darwin') then
        AErro := Format(emSistemaDesconhecido, [Sistemas[vInt]]);
    end;
  Result := AErro = '';
end;

function TCondicaoIDE.Relevante(AIDE: TIDEInstance): boolean;
var
  vDelphi, vLazarus: boolean;
begin
  vDelphi := (DelphiMin <> '') or (DelphiMax <> '');
  vLazarus := (LazarusMin <> '') or (LazarusMax <> '') or (FPCMin <> '') or
              (FPCMax <> '');
  if AIDE.Tipo = tiDelphi then
    Result := vDelphi or not vLazarus
  else
    Result := vLazarus or not vDelphi;
end;

function TCondicaoIDE.Satisfaz(AIDE: TIDEInstance;
  ADesconhecidoAtende: boolean): boolean;

  function Dentro(const AVersao, AMin, AMax: string): boolean;
  begin
    Result := True;
    if (AMin = '') and (AMax = '') then
      Exit;
    if AVersao = '' then
      Exit(ADesconhecidoAtende);
    if (AMin <> '') and (CompararVersoes(AVersao, AMin) < 0) then
      Exit(False);
    if (AMax <> '') and (CompararVersoes(AVersao, AMax) > 0) then
      Exit(False);
  end;

begin
  Result := False;
  if (Sistemas.Count > 0) and (Sistemas.IndexOf(SistemaAtual) < 0) then
    Exit;
  if AIDE.Tipo = tiDelphi then
    Result := Dentro(AIDE.Versao, VersaoNumDelphi(DelphiMin),
                     VersaoNumDelphi(DelphiMax))
  else
    Result := Dentro(AIDE.Versao, LazarusMin, LazarusMax) and
              Dentro(AIDE.VersaoCompilador, FPCMin, FPCMax);
end;

function TCondicaoIDE.Descrever(AIDE: TIDEInstance): string;

  procedure Juntar(const ATexto: string);
  begin
    if ATexto = '' then
      Exit;
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + ATexto;
  end;

  function Faixa(const AProduto, AMin, AMax: string): string;
  begin
    if (AMin <> '') and (AMax <> '') and SameText(AMin, AMax) then
      Result := AProduto + ' ' + AMin
    else if (AMin <> '') and (AMax <> '') then
      Result := Format(cmFaixaDeA, [AProduto, AMin, AMax])
    else if AMin <> '' then
      Result := Format(cmFaixaMin, [AProduto, AMin])
    else if AMax <> '' then
      Result := Format(cmFaixaMax, [AProduto, AMax])
    else
      Result := '';
  end;

begin
  Result := '';
  if AIDE.Tipo = tiDelphi then
    Juntar(Faixa('Delphi', DelphiMin, DelphiMax))
  else
  begin
    Juntar(Faixa('Lazarus', LazarusMin, LazarusMax));
    Juntar(Faixa('FPC', FPCMin, FPCMax));
  end;
  if Sistemas.Count > 0 then
    Juntar(Format(cmFaixaSistemas,
      [StringReplace(Sistemas.CommaText, ',', ', ', [rfReplaceAll])]));
end;

{ TRegraVersao }

constructor TRegraVersao.Create;
begin
  inherited Create;
  Condicao := TCondicaoIDE.Create;
end;

destructor TRegraVersao.Destroy;
begin
  Condicao.Free;
  inherited Destroy;
end;

function LerRegraVersao(AObj: TJSONObject; const AOrigem: string;
  const ACamposExtras: array of string; out AErro: string): TRegraVersao;
var
  vInt, vCampo: integer;
  vAchou: boolean;
  vNome: string;
begin
  Result := nil;
  AErro := '';
  for vInt := 0 to Pred(AObj.Count) do
  begin
    vNome := AObj.Names[vInt];
    vAchou := (vNome = 'versao') or (vNome = 'incompativel') or
              (vNome = 'comentario') or (vNome = 'github');
    for vCampo := Low(CamposCondicao) to High(CamposCondicao) do
      vAchou := vAchou or (vNome = CamposCondicao[vCampo]);
    for vCampo := Low(ACamposExtras) to High(ACamposExtras) do
      vAchou := vAchou or (vNome = ACamposExtras[vCampo]);
    if not vAchou then
    begin
      AErro := Format(emCampoDesconhecido, [vNome, AOrigem]);
      Exit;
    end;
  end;

  Result := TRegraVersao.Create;
  Result.Origem := AOrigem;
  Result.Versao := AObj.Get('versao', '');
  Result.Incompativel := AObj.Get('incompativel', '');
  Result.Github := AObj.Get('github', '');
  if not Result.Condicao.Ler(AObj, AErro) then
    AErro := AOrigem + ': ' + AErro
  else if (Result.Github <> '') and
          ((Pos('/', Result.Github) = 0) or (Pos(':', Result.Github) > 0)) then
    AErro := Format(emRegraGithub, [AOrigem])
  else if (Result.Versao = '') = (Result.Incompativel = '') then
    AErro := Format(emRegraVersao, [AOrigem]);
  if AErro <> '' then
    FreeAndNil(Result);
end;

function ExpandirRaiz(const ATexto, ARaiz: string): string;
begin
  Result := StringReplace(ATexto, '{raiz}', ExcludeTrailingPathDelimiter(ARaiz),
                          [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '/', PathDelim, [rfReplaceAll]);
  Result := StringReplace(Result, '\', PathDelim, [rfReplaceAll]);
end;

function NovaLista: TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
end;

{ TAcaoReceita }

constructor TAcaoReceita.Create;
begin
  inherited Create;
  Caminhos := NovaLista;
  Pastas := NovaLista;
end;

destructor TAcaoReceita.Destroy;
begin
  Pastas.Free;
  Caminhos.Free;
  inherited Destroy;
end;

{ TBlocoIDE }

constructor TBlocoIDE.Create;
begin
  inherited Create;
  FornecePacotes := NovaLista;
  ForneceVariaveis := NovaLista;
  Deteccao := NovaLista;
  Acoes := TObjectList.Create(True);
  PacotesLigados := NovaLista;
  Verificacoes := TObjectList.Create(True);
  Busca := NovaLista;
end;

destructor TBlocoIDE.Destroy;
begin
  Busca.Free;
  Verificacoes.Free;
  PacotesLigados.Free;
  Acoes.Free;
  Deteccao.Free;
  ForneceVariaveis.Free;
  FornecePacotes.Free;
  inherited Destroy;
end;

function TBlocoIDE.Acao(AIndex: integer): TAcaoReceita;
begin
  Result := TAcaoReceita(Acoes[AIndex]);
end;

{ TReceita }

constructor TReceita.Create;
begin
  inherited Create;
  FDelphi := TBlocoIDE.Create;
  FLazarus := TBlocoIDE.Create;
  FPacotesRAL := NovaLista;
  FExtras := NovaLista;
  FVersoes := TObjectList.Create(True);
end;

destructor TReceita.Destroy;
begin
  FVersoes.Free;
  FExtras.Free;
  FPacotesRAL.Free;
  FLazarus.Free;
  FDelphi.Free;
  inherited Destroy;
end;

function TReceita.Bloco(ATipo: TTipoPacote): TBlocoIDE;
begin
  if ATipo = tpDelphi then
    Result := FDelphi
  else
    Result := FLazarus;
end;

function TReceita.Atende(APacote: TPacote): boolean;
var
  vBloco: TBlocoIDE;
  vNome: string;
begin
  Result := FPacotesRAL.IndexOf(APacote.Nome) >= 0;
  if Result then
    Exit;
  vBloco := Bloco(APacote.Tipo);
  if not vBloco.Existe then
    Exit;
  for vNome in APacote.Externos do
    if vBloco.FornecePacotes.IndexOf(vNome) >= 0 then
      Exit(True);
  for vNome in APacote.Implicitos do
    if vBloco.FornecePacotes.IndexOf(vNome) >= 0 then
      Exit(True);
  for vNome in APacote.Variaveis do
    if vBloco.ForneceVariaveis.IndexOf(vNome) >= 0 then
      Exit(True);
end;

function TReceita.PodeBaixar: boolean;
begin
  Result := not Pago and (Github <> '');
end;

{ TReceitas }

constructor TReceitas.Create;
begin
  inherited Create(True);
  FErros := TStringList.Create;
end;

destructor TReceitas.Destroy;
begin
  FErros.Free;
  inherited Destroy;
end;

function TReceitas.GetItem(AIndex: integer): TReceita;
begin
  Result := TReceita(inherited Items[AIndex]);
end;

function TReceitas.Buscar(const ANome: string): TReceita;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(Count) do
    if SameText(Items[vInt].Nome, ANome) then
      Exit(Items[vInt]);
end;

function TReceitas.CarregarTexto(const ATexto, AOrigem: string): boolean;
const
  CamposRaiz: array[0..9] of string = ('nome', 'descricao', 'site', 'pago',
    'fonte', 'extras', 'pacotes-ral', 'delphi', 'lazarus', 'comentario');
var
  vJSON: TJSONData;
  vRaiz: TJSONObject;
  vReceita: TReceita;
  vErro: string;

  procedure Falhar(const AMotivo: string);
  begin
    if vErro = '' then
      vErro := AMotivo;
  end;

  procedure LerLista(ANo: TJSONData; ALista: TStrings; const ACampo: string);
  var
    vInt: integer;
  begin
    if ANo = nil then
      Exit;
    if not (ANo is TJSONArray) then
    begin
      Falhar(Format(emReceitaLista, [ACampo]));
      Exit;
    end;
    for vInt := 0 to Pred(TJSONArray(ANo).Count) do
      ALista.Add(TJSONArray(ANo).Items[vInt].AsString);
  end;

  procedure ConferirCampos(AObj: TJSONObject; const APermitidos: array of string;
    const ALugar: string);
  var
    vInt, vPerm: integer;
    vAchou: boolean;
  begin
    for vInt := 0 to Pred(AObj.Count) do
    begin
      vAchou := False;
      for vPerm := Low(APermitidos) to High(APermitidos) do
        if AObj.Names[vInt] = APermitidos[vPerm] then
          vAchou := True;
      if not vAchou then
        Falhar(Format(emCampoDesconhecido, [AObj.Names[vInt], ALugar]));
    end;
  end;

  procedure LerBloco(ANo: TJSONData; ABloco: TBlocoIDE; const AIDE: string);
  var
    vObj, vFornece, vItem: TJSONObject;
    vLista: TJSONArray;
    vInt, vCampo: integer;
    vAcao: TAcaoReceita;
    vNomeAcao: string;
    vPastas: TJSONObject;
    vVerif: TVerificacao;
    vErroCond: string;
  begin
    if ANo = nil then
      Exit;
    if not (ANo is TJSONObject) then
    begin
      Falhar(Format(emReceitaObjeto, [AIDE]));
      Exit;
    end;
    vObj := TJSONObject(ANo);
    ConferirCampos(vObj, ['fornece', 'deteccao', 'acoes', 'pacotes-ligados',
                          'verificacoes', 'busca', 'comentario'], AIDE);
    LerLista(vObj.Find('busca'), ABloco.Busca, AIDE + '.busca');
    if (ABloco.Busca.Count > 0) and (AIDE <> 'delphi') then
      Falhar(emReceitaBusca);
    ABloco.Existe := True;
    LerLista(vObj.Find('pacotes-ligados'), ABloco.PacotesLigados,
             AIDE + '.pacotes-ligados');

    // verificacoes: { "arquivo", "expressao", "aviso", faixa... }
    vLista := vObj.Get('verificacoes', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
      begin
        if not (vLista.Items[vInt] is TJSONObject) then
        begin
          Falhar(Format(emReceitaItemObjeto, [AIDE + '.verificacoes']));
          Continue;
        end;
        vItem := vLista.Objects[vInt];
        ConferirCampos(vItem, ['arquivo', 'expressao', 'aviso', 'comentario',
          'delphi-min', 'delphi-max', 'lazarus-min', 'lazarus-max', 'fpc-min',
          'fpc-max', 'sistemas'], AIDE + '.verificacoes');
        vVerif := TVerificacao.Create;
        ABloco.Verificacoes.Add(vVerif);
        vVerif.Arquivo := vItem.Get('arquivo', '');
        vVerif.Expressao := vItem.Get('expressao', '');
        vVerif.Aviso := vItem.Get('aviso', '');
        if (vVerif.Arquivo = '') or (vVerif.Expressao = '') or (vVerif.Aviso = '') then
          Falhar(emVerificacaoIncompleta)
        else if (Pos('..', vVerif.Arquivo) > 0) or (Pos(':', vVerif.Arquivo) > 0) or
                (Copy(vVerif.Arquivo, 1, 1) = '/') then
          Falhar(Format(emVerificacaoArquivo, [vVerif.Arquivo]))
        else if not vVerif.Condicao.Ler(vItem, vErroCond) then
          Falhar(AIDE + '.verificacoes: ' + vErroCond)
        else
          try
            CasaExpressao('', vVerif.Expressao);
          except
            on E: Exception do
              Falhar(Format(emVerificacaoExpressao, [E.Message]));
          end;
      end;
    if (ABloco.PacotesLigados.Count > 0) and (AIDE <> 'delphi') then
      Falhar(emReceitaPacotesLigados);

    vFornece := vObj.Get('fornece', TJSONObject(nil));
    if vFornece <> nil then
    begin
      ConferirCampos(vFornece, ['pacotes', 'variaveis'], AIDE + '.fornece');
      LerLista(vFornece.Find('pacotes'), ABloco.FornecePacotes,
               AIDE + '.fornece.pacotes');
      LerLista(vFornece.Find('variaveis'), ABloco.ForneceVariaveis,
               AIDE + '.fornece.variaveis');
    end;

    // deteccao: lista de { "variavel": "x" } / { "unidade": "x" } / ...
    vLista := vObj.Get('deteccao', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
      begin
        if not (vLista.Items[vInt] is TJSONObject) then
        begin
          Falhar(Format(emReceitaItemObjeto, [AIDE + '.deteccao']));
          Continue;
        end;
        vItem := vLista.Objects[vInt];
        ConferirCampos(vItem, ['variavel', 'unidade', 'dcp', 'pacote'],
                       AIDE + '.deteccao');
        for vCampo := 0 to Pred(vItem.Count) do
          ABloco.Deteccao.Add(vItem.Names[vCampo] + '=' + vItem.Items[vCampo].AsString);
      end;

    vLista := vObj.Get('acoes', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
      begin
        if not (vLista.Items[vInt] is TJSONObject) then
        begin
          Falhar(Format(emReceitaItemObjeto, [AIDE + '.acoes']));
          Continue;
        end;
        vItem := vLista.Objects[vInt];
        vNomeAcao := vItem.Get('acao', '');
        vAcao := TAcaoReceita.Create;
        ABloco.Acoes.Add(vAcao);
        // o vocabulario fechado: o que nao esta aqui recusa a receita
        if vNomeAcao = 'variavel' then
        begin
          vAcao.Tipo := taVariavel;
          ConferirCampos(vItem, ['acao', 'nome', 'valor', 'comentario'],
                         AIDE + '.acoes[variavel]');
          vAcao.Nome := vItem.Get('nome', '');
          vAcao.Valor := vItem.Get('valor', '');
          if (vAcao.Nome = '') or (vAcao.Valor = '') then
            Falhar(emAcaoVariavel);
        end
        else if vNomeAcao = 'libpath' then
        begin
          vAcao.Tipo := taLibPath;
          ConferirCampos(vItem, ['acao', 'caminhos', 'comentario'],
                         AIDE + '.acoes[libpath]');
          LerLista(vItem.Find('caminhos'), vAcao.Caminhos, 'libpath.caminhos');
        end
        else if (vNomeAcao = 'dpk') and (AIDE = 'delphi') then
        begin
          vAcao.Tipo := taDpk;
          ConferirCampos(vItem, ['acao', 'pastas', 'instalar', 'comentario'],
                         AIDE + '.acoes[dpk]');
          vPastas := vItem.Get('pastas', TJSONObject(nil));
          if vPastas = nil then
            Falhar(emAcaoDpk)
          else
            for vCampo := 0 to Pred(vPastas.Count) do
              vAcao.Pastas.Add(vPastas.Names[vCampo] + '=' +
                               vPastas.Items[vCampo].AsString);
          vAcao.Instalar := vItem.Get('instalar', True);
        end
        else if (vNomeAcao = 'lpk') and (AIDE = 'lazarus') then
        begin
          vAcao.Tipo := taLpk;
          ConferirCampos(vItem, ['acao', 'arquivo', 'modo', 'comentario'],
                         AIDE + '.acoes[lpk]');
          vAcao.Arquivo := vItem.Get('arquivo', '');
          vAcao.Instalar := vItem.Get('modo', 'link') = 'instalar';
          if vAcao.Arquivo = '' then
            Falhar(emAcaoLpk);
        end
        else
          Falhar(Format(emAcaoDesconhecida, [vNomeAcao, AIDE]));
      end;
  end;

var
  vFonte, vExtra: TJSONObject;
  vLista: TJSONArray;
  vInt: integer;
  vAntiga: TReceita;
  vRegra: TRegraVersao;
  vErroRegra: string;
begin
  Result := False;
  vErro := '';
  vJSON := nil;
  vReceita := TReceita.Create;
  try
    try
      vJSON := GetJSON(ATexto);
    except
      on E: Exception do
      begin
        FErros.Add(Format(emJSONInvalido, [AOrigem, E.Message]));
        Exit;
      end;
    end;
    if not (vJSON is TJSONObject) then
    begin
      FErros.Add(Format(emReceitaNaoObjeto, [AOrigem]));
      Exit;
    end;
    vRaiz := TJSONObject(vJSON);
    ConferirCampos(vRaiz, CamposRaiz, 'receita');

    vReceita.Origem := AOrigem;
    vReceita.Nome := vRaiz.Get('nome', '');
    vReceita.Descricao := vRaiz.Get('descricao', '');
    vReceita.Site := vRaiz.Get('site', '');
    vReceita.Pago := vRaiz.Get('pago', False);
    if vReceita.Nome = '' then
      Falhar(emReceitaSemNome);

    vFonte := vRaiz.Get('fonte', TJSONObject(nil));
    if vFonte <> nil then
    begin
      ConferirCampos(vFonte, ['github', 'versao', 'versoes', 'comentario'], 'fonte');
      vReceita.Github := vFonte.Get('github', '');
      vReceita.Versao := vFonte.Get('versao', 'estavel');
      if Pos('/', vReceita.Github) = 0 then
        Falhar(Format(emRegraGithub, ['fonte']));
      // F6: versao por faixa de IDE/compilador (o Zeos por FPC)
      vLista := vFonte.Get('versoes', TJSONArray(nil));
      if vLista <> nil then
        for vInt := 0 to Pred(vLista.Count) do
        begin
          if not (vLista.Items[vInt] is TJSONObject) then
          begin
            Falhar(Format(emReceitaItemObjeto, ['fonte.versoes']));
            Continue;
          end;
          vRegra := LerRegraVersao(vLista.Objects[vInt], 'fonte.versoes', [],
                                   vErroRegra);
          if vRegra = nil then
            Falhar(vErroRegra)
          else
            vReceita.Versoes.Add(vRegra);
        end;
    end;

    vLista := vRaiz.Get('extras', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
      begin
        vExtra := vLista.Objects[vInt];
        ConferirCampos(vExtra, ['asset', 'destino', 'release', 'comentario'],
                       'extras');
        if vExtra.Get('asset', '') = '' then
          Falhar(emExtraSemAsset);
        // asset=destino|release ('' = o release da propria versao; 'estavel' =
        // o release estavel mais recente, para quando a fonte e um ramo)
        vReceita.Extras.Add(vExtra.Get('asset', '') + '=' +
                            vExtra.Get('destino', '') + '|' +
                            vExtra.Get('release', ''));
      end;

    LerLista(vRaiz.Find('pacotes-ral'), vReceita.PacotesRAL, 'pacotes-ral');
    LerBloco(vRaiz.Find('delphi'), vReceita.Delphi, 'delphi');
    LerBloco(vRaiz.Find('lazarus'), vReceita.Lazarus, 'lazarus');

    if vErro <> '' then
    begin
      FErros.Add(Format(emReceitaRecusada, [AOrigem, vErro]));
      Exit;
    end;

    vAntiga := Buscar(vReceita.Nome);
    if vAntiga <> nil then
      Remove(vAntiga);
    Add(vReceita);
    vReceita := nil;
    Result := True;
  finally
    vReceita.Free;
    vJSON.Free;
  end;
end;

procedure TReceitas.CarregarPasta(const APasta: string);
var
  vBusca: TSearchRec;
  vTexto: TStringList;
  vPasta: string;
begin
  vPasta := IncludeTrailingPathDelimiter(APasta);
  if FindFirst(vPasta + '*.json', faAnyFile, vBusca) = 0 then
  try
    repeat
      vTexto := TStringList.Create;
      try
        vTexto.LoadFromFile(vPasta + vBusca.Name);
        CarregarTexto(vTexto.Text, vPasta + vBusca.Name);
      finally
        vTexto.Free;
      end;
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

const
  // RT_RCDATA: no Windows vem da unit Windows, nos outros do system
  RecursoDados = PChar(10);

function EnumerarReceita(ModuleHandle: TFPResourceHMODULE; ResourceType,
  ResourceName: PChar; lParam: PtrInt): LongBool; stdcall;
var
  vNome: string;
begin
  Result := True;
  if PtrUInt(ResourceName) shr 16 = 0 then
    Exit;
  vNome := string(ResourceName);
  if SameText(Copy(vNome, 1, 8), 'RECEITA_') then
    TStrings(Pointer(lParam)).Add(vNome);
end;

procedure TReceitas.CarregarRecursos;
var
  vNomes: TStringList;
  vNome: string;
  vStream: TResourceStream;
  vTexto: TStringList;
begin
  vNomes := TStringList.Create;
  try
    EnumResourceNames(HINSTANCE, RecursoDados, @EnumerarReceita, PtrInt(vNomes));
    for vNome in vNomes do
    begin
      vStream := TResourceStream.Create(HINSTANCE, vNome, RecursoDados);
      vTexto := TStringList.Create;
      try
        vTexto.LoadFromStream(vStream);
        CarregarTexto(vTexto.Text, Format(cmRecurso, [vNome]));
      finally
        vTexto.Free;
        vStream.Free;
      end;
    end;
  finally
    vNomes.Free;
  end;
end;

procedure TReceitas.CarregarPadrao;
begin
  CarregarRecursos;
  CarregarPasta(ExtractFilePath(ParamStr(0)) + 'receitas');
  CarregarPasta(PastaDadosInstalador + 'receitas');
end;

procedure TReceitas.Exigidas(ACatalogo: TCatalogo; ATipo: TTipoPacote;
  APacotes: TStrings; AResultado: TStrings);
var
  vLista: TList;
  vInt, vRec, vIdx: integer;
  vPacote: TPacote;
begin
  AResultado.Clear;
  vLista := TList.Create;
  try
    ACatalogo.Fechamento(ATipo, APacotes, vLista);
    for vRec := 0 to Pred(Count) do
      for vInt := 0 to Pred(vLista.Count) do
      begin
        vPacote := TPacote(vLista[vInt]);
        if not Items[vRec].Atende(vPacote) then
          Continue;
        vIdx := AResultado.IndexOfObject(Items[vRec]);
        if vIdx < 0 then
          AResultado.AddObject(vPacote.Nome, Items[vRec])
        else
          AResultado[vIdx] := AResultado[vIdx] + ', ' + vPacote.Nome;
      end;
  finally
    vLista.Free;
  end;
end;

end.
