/// Migration rules of the RAL migrator (stage M2): the model, the strict
/// reader of the rule files, the texts per language and the chain of steps
/// (1.0 -> 1.1 -> 1.2 -> 2.0). The rules are data: closed vocabulary of actions
/// and conditions, so a file with an unknown action or field is refused as a
/// whole and no rule ever runs a command. The format is described in
/// migrador/regras/LEIAME.md.
///
/// Strings are UTF-8 (the application sets DefaultSystemCodePage to CP_UTF8, as
/// every Lazarus application does); the identifiers of the rules are ASCII.
unit RALMig.Regras;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, fpjson, jsonparser;

const
  /// Origin of the rules that go from one RAL version to another (D-M9)
  OrigemRAL = 'RAL';
  IdiomaPtBR = 'pt-BR';
  IdiomaEnUS = 'en-US';
  IdiomaEsES = 'es-ES';
  /// Value of "formato" in the rule files
  FormatoRegras = 1;
  /// File of the target RAL root that adds or replaces rules (D-M1)
  ArquivoSobreposicao = 'ralmigracao.json';

type
  /// The closed vocabulary of actions.
  TAcaoRegra = (
    arPacoteRenomear, arPacoteExigirSeUnit, arCaminhoTrocar, arUnitRenomear,
    arUsesAdicionar, arIdentificadorRenomear, arPropriedadeRenomear,
    arPropriedadeParaComponente, arValorPadraoMaterializar, arPropriedadeRemover,
    arObjetoSemEquivalente, arPadraoAvisar, arPacoteDesignParaRuntime);

  /// The closed vocabulary of conditions.
  TTipoCondicao = (
    tcSempre, tcTemPropriedade, tcValorIgual, tcValorDiferente, tcValorNaoVazio,
    tcClasseDescendeDe, tcUnitNoUses, tcSetContem, tcAlguma, tcTodas);

  /// Why a chain could not be built.
  TCadeiaErro = (ceNenhum, ceVersaoDesconhecida, ceRetrocesso, ceSemPasso);

  /// A text in the supported languages, with the fallback en-US -> pt-BR.
  TTextoIdiomas = class
  private
    FIdiomas: TStringList;
    FTextos: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Definir(const AIdioma, ATexto: string);
    /// The text has that exact language (no fallback)
    function Tem(const AIdioma: string): boolean;
    /// The text of the language; when missing, en-US, then pt-BR, then any
    function Obter(const AIdioma: string): string;
    function Vazio: boolean;
    /// Has the three supported languages
    function Completo: boolean;
  end;

  /// A declared condition (no free expression); the engine evaluates it.
  TCondicaoRegra = class
  private
    FFilhos: TObjectList;
    function GetFilho(AIndex: integer): TCondicaoRegra;
    function GetFilhoCount: integer;
  public
    Tipo: TTipoCondicao;
    Propriedade: string;
    Valor: string;
    Classe: string;
    NomeUnit: string;
    Opcao: string;
    /// Inverts the result
    Negar: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure AddFilho(AFilho: TCondicaoRegra);
    /// A readable form, for tests and logs
    function Descrever: string;
    property FilhoCount: integer read GetFilhoCount;
    property Filhos[AIndex: integer]: TCondicaoRegra read GetFilho;
  end;

  /// One rule: id, action, parameters (validated per action) and texts.
  TRegraMigracao = class
  private
    FEscalares: TStringList;
    FListas: TStringList;
    FMapas: TStringList;
    FTextos: TStringList;
    FVazia: TStringList;
    function BuscarTexto(const ACampo: string): TTextoIdiomas;
  public
    Id: string;
    Acao: TAcaoRegra;
    /// File the rule came from (for messages)
    Arquivo: string;
    /// Kinds of file the rule acts on: dproj, dpk, lpi, lpk, lpr, dpr, pas, form...
    Onde: TStringList;
    /// nil = always
    Condicao: TCondicaoRegra;
    /// Only propriedade-para-componente: when the new component is created
    CriarQuando: TCondicaoRegra;
    Wiki: string;
    constructor Create;
    destructor Destroy; override;
    function NomeAcao: string;
    function AplicaEm(const AOnde: string): boolean;
    /// ACampo = 'descricao', 'manual' or 'comportamento'; '' when absent
    function Texto(const ACampo, AIdioma: string): string;
    function TemCampo(const ANome: string): boolean;
    /// A string, enum or boolean ('true'/'false') parameter
    function Param(const ANome: string; const ADefault: string = ''): string;
    function ParamBool(const ANome: string; ADefault: boolean = False): boolean;
    /// A list parameter; never nil (empty when absent), owned by the rule
    function Lista(const ANome: string): TStringList;
    /// A map parameter as Name=Value lines (a list value is joined with ',');
    /// never nil, owned by the rule
    function Mapa(const ANome: string): TStringList;
    /// The code pattern kind of padrao-avisar ('' for other actions)
    function PadraoTipo: string;
    // used by the reader
    procedure DefinirEscalar(const ANome, AValor: string);
    function NovaLista(const ANome: string): TStringList;
    function NovoMapa(const ANome: string): TStringList;
    procedure DefinirTexto(const ACampo: string; ATexto: TTextoIdiomas);
  end;

  /// An item of the "behavior that changed" block: no code pattern, only report.
  TComportamentoMigracao = class
  private
    FDescricao: TTextoIdiomas;
    FFazer: TTextoIdiomas;
  public
    Id: string;
    Wiki: string;
    Arquivo: string;
    constructor Create;
    destructor Destroy; override;
    /// ACampo = 'descricao' (what changed) or 'fazer' (what to do)
    function Texto(const ACampo, AIdioma: string): string;
    property Descricao: TTextoIdiomas read FDescricao;
    property Fazer: TTextoIdiomas read FFazer;
  end;

  /// A step of the chain: all the rules from one version to the next.
  TPassoMigracao = class
  private
    FRegras: TObjectList;
    FComportamento: TObjectList;
    function GetRegra(AIndex: integer): TRegraMigracao;
    function GetRegraCount: integer;
    function GetComp(AIndex: integer): TComportamentoMigracao;
    function GetCompCount: integer;
  public
    /// Where the steps start from; 'RAL' today (D-M9)
    Origem: string;
    /// Normalized public versions ('1.1', '2.0')
    De: string;
    Para: string;
    constructor Create;
    destructor Destroy; override;
    /// 'de>para'
    function Rotulo: string;
    function BuscarRegra(const AId: string): TRegraMigracao;
    function BuscarComportamento(const AId: string): TComportamentoMigracao;
    /// Adds the rule, or replaces the one with the same id (same position)
    procedure PutRegra(ARegra: TRegraMigracao);
    procedure PutComportamento(AComp: TComportamentoMigracao);
    /// Moves every rule and behavior item to ADestino (same id replaces); this
    /// step stays empty and can be freed
    procedure TransferirPara(ADestino: TPassoMigracao);
    property Comportamento[AIndex: integer]: TComportamentoMigracao read GetComp;
    property CompCount: integer read GetCompCount;
    property Regras[AIndex: integer]: TRegraMigracao read GetRegra; default;
    property RegraCount: integer read GetRegraCount;
  end;

  /// The steps of a chain, in order; does not own them (the set does).
  TListaPassos = class
  private
    FLista: TList;
    function GetCount: integer;
    function GetItem(AIndex: integer): TPassoMigracao;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(APasso: TPassoMigracao);
    /// Takes a step out of the chain (the step itself belongs to the rule set)
    procedure Remover(AIndex: integer);
    property Count: integer read GetCount;
    property Itens[AIndex: integer]: TPassoMigracao read GetItem; default;
  end;

  /// All the loaded rules (embedded + overlay) and the chain over them.
  TConjuntoRegras = class
  private
    FErros: TStringList;
    FPassos: TObjectList;
    function GetPasso(AIndex: integer): TPassoMigracao;
    function GetPassoCount: integer;
    function CarregarSimples(const ATexto, ANome: string; AEstrito: boolean): boolean;
    procedure Mesclar(APasso: TPassoMigracao);
  public
    constructor Create;
    destructor Destroy; override;
    /// Reads a rule file (a step, or {"passos": [...]}) and merges it by id.
    /// The whole file is refused when anything is invalid: Erros says why and
    /// nothing is merged. AEstrito demands the three languages in every text
    /// and the id prefix equal to the target version (the embedded rules).
    function CarregarTexto(const ATexto, ANome: string; AEstrito: boolean): boolean;
    function CarregarArquivo(const AArquivo: string; AEstrito: boolean): boolean;
    /// Every *.json of the folder except cobertura.json and ralmigracao.json
    function CarregarDePasta(const APasta: string; AEstrito: boolean): boolean;
    /// The three embedded files (RCDATA, see RecursoDoPasso), strict
    function CarregarEmbutidas: boolean;
    /// ralmigracao.json text: adds steps/rules, the same id replaces (not strict)
    function AplicarSobreposicaoTexto(const ATexto, ANome: string): boolean;
    function AplicarSobreposicao(const AArquivo: string): boolean;
    /// <ARaizRAL>/ralmigracao.json; true when the file does not exist
    function AplicarSobreposicaoDaRaiz(const ARaizRAL: string): boolean;
    function BuscarPasso(const AOrigem, ADe, APara: string): TPassoMigracao;
    /// First rule with that id in any step
    function BuscarRegra(const AId: string): TRegraMigracao;
    function BuscarComportamento(const AId: string): TComportamentoMigracao;
    /// The version is the start or the end of a loaded step of that origin
    function VersaoConhecida(const AOrigem, AVersao: string): boolean;
    /// The versions of the origin, oldest first
    procedure ListarVersoes(const AOrigem: string; ADest: TStringList);
    /// The steps that take AOrigem from ADe to APara, in order. Never nil; when
    /// AErro <> ceNenhum the list is empty and AMensagem says why (in the
    /// language of the program). Same version = empty list and no error.
    function Cadeia(const AOrigem, ADe, APara: string; out AErro: TCadeiaErro;
      out AMensagem: string): TListaPassos;
    property Erros: TStringList read FErros;
    property PassoCount: integer read GetPassoCount;
    property Passos[AIndex: integer]: TPassoMigracao read GetPasso;
  end;

/// '1.0', '1.1', '1.2', '2.0': v1.0 -> 1.0, dev -> 1.2, 1.3-plugin and 2.0.x
/// -> 2.0, 1.1.3 -> 1.1. '' when the text is not a version.
function NormalizarVersao(const AVersao: string): string;
/// -1, 0 or 1 comparing two versions (any form NormalizarVersao accepts)
function CompararVersoes(const AVersao1, AVersao2: string): integer;
/// 'pt-BR', 'en-US', 'es-ES' from 'pt', 'pt_br', 'EN'...; '' when unsupported
function NormalizarIdioma(const AIdioma: string): string;
function NomeDaAcao(AAcao: TAcaoRegra): string;
function TentarAcao(const ANome: string; out AAcao: TAcaoRegra): boolean;
function NomeDoTipoCondicao(ATipo: TTipoCondicao): string;
/// Name of the RCDATA resource of a step: REGRAS_<de>_<para> with the dots of
/// the versions turned into '_' and a double '_' between them: REGRAS_1_0__1_1
function RecursoDoPasso(const ADe, APara: string): string;

implementation

resourcestring
  SMsgVersaoDesconhecida = 'A versão "%s" do RAL não é conhecida pelo migrador.';
  SMsgRetrocesso = 'O migrador só migra para a frente: o destino (%s) é mais velho ' +
    'que a origem (%s). Um projeto antigo continua funcionando na versão antiga ' +
    'e não precisa de migração.';
  SMsgSemPasso = 'Não há regras de migração a partir da versão %s.';
  SMsgDestinoNoMeio = 'A versão %s não é o fim de nenhum passo de migração ' +
    '(o passo %s vai até %s).';

const
  Idiomas: array[0..2] of string = (IdiomaPtBR, IdiomaEnUS, IdiomaEsES);
  AcaoNomes: array[TAcaoRegra] of string = (
    'pacote-renomear', 'pacote-exigir-se-unit', 'caminho-trocar', 'unit-renomear',
    'uses-adicionar', 'identificador-renomear', 'propriedade-renomear',
    'propriedade-para-componente', 'valor-padrao-materializar',
    'propriedade-remover', 'objeto-sem-equivalente', 'padrao-avisar',
    'pacote-design-para-runtime');
  CondicaoNomes: array[TTipoCondicao] of string = (
    'sempre', 'tem-propriedade', 'valor-igual', 'valor-diferente',
    'valor-nao-vazio', 'classe-descende-de', 'unit-no-uses', 'set-contem',
    'alguma', 'todas');
  OndeValidos = 'dproj,dpk,lpi,lpk,lpr,dpr,pas,form,groupproj,lpg';
  PadraoTipos = 'identificador,chamada-metodo,atribuicao-propriedade,' +
    'conversao-tipo,ausencia-atribuicao,sobrescrita-metodo,somente-relatorio';
  MantemValidos = 'designonly,tem-register';

  // Field spec: name:kind[(a|b)][!]; kinds: s string, b boolean, n number,
  // l list of strings, m map, c condition, k array of conditions, t text per
  // language, a array of objects, e enum; "!" = required.
  SpecComum = 'id:s!;acao:s!;onde:l!;condicao:c;descricao:t!;manual:t!;' +
    'comportamento:t;wiki:s';
  SpecIdeTres = 'e(delphi|lazarus|ambos)';
  SpecPasso = 'formato:n;origem:s!;de:s!;para:s!;regras:a!;comportamento:a';
  SpecRaiz = 'formato:n;passos:a!';
  SpecComp = 'id:s!;descricao:t!;fazer:t!;wiki:s';

{ language and version helpers }

function NormalizarIdioma(const AIdioma: string): string;
var
  vTexto: string;
begin
  Result := '';
  vTexto := LowerCase(StringReplace(Trim(AIdioma), '_', '-', [rfReplaceAll]));
  if Copy(vTexto, 1, 2) = 'pt' then
    Result := IdiomaPtBR
  else if Copy(vTexto, 1, 2) = 'en' then
    Result := IdiomaEnUS
  else if Copy(vTexto, 1, 2) = 'es' then
    Result := IdiomaEsES;
end;

function LerNumero(const ATexto: string; var APos: integer): integer;
var
  vIni: integer;
begin
  vIni := APos;
  while (APos <= Length(ATexto)) and (ATexto[APos] in ['0'..'9']) do
    Inc(APos);
  if (APos = vIni) or (APos - vIni > 4) then
    Result := -1
  else
    Result := StrToIntDef(Copy(ATexto, vIni, APos - vIni), -1);
end;

function NormalizarVersao(const AVersao: string): string;
var
  vTexto: string;
  vPos, vMaior, vMenor: integer;
begin
  Result := '';
  vTexto := LowerCase(Trim(AVersao));
  if vTexto = 'dev' then
    Exit('1.2');
  if (vTexto <> '') and (vTexto[1] = 'v') then
    Delete(vTexto, 1, 1);
  vPos := 1;
  vMaior := LerNumero(vTexto, vPos);
  if (vMaior < 0) or (vPos > Length(vTexto)) or (vTexto[vPos] <> '.') then
    Exit;
  Inc(vPos);
  vMenor := LerNumero(vTexto, vPos);
  if vMenor < 0 then
    Exit;
  if (vPos <= Length(vTexto)) and not (vTexto[vPos] in ['.', '-', '+', '_']) then
    Exit;
  if (vMaior = 1) and (vMenor = 3) then
  begin
    // the branch 1.3-plugin became 2.0
    vMaior := 2;
    vMenor := 0;
  end;
  Result := Format('%d.%d', [vMaior, vMenor]);
end;

function CompararVersoes(const AVersao1, AVersao2: string): integer;
var
  vVer1, vVer2: string;
  vMaior1, vMenor1, vMaior2, vMenor2, vPos: integer;
begin
  vVer1 := NormalizarVersao(AVersao1);
  vVer2 := NormalizarVersao(AVersao2);
  vPos := 1;
  vMaior1 := LerNumero(vVer1, vPos);
  Inc(vPos);
  vMenor1 := LerNumero(vVer1, vPos);
  vPos := 1;
  vMaior2 := LerNumero(vVer2, vPos);
  Inc(vPos);
  vMenor2 := LerNumero(vVer2, vPos);
  if vMaior1 <> vMaior2 then
    Result := Ord(vMaior1 > vMaior2) - Ord(vMaior1 < vMaior2)
  else
    Result := Ord(vMenor1 > vMenor2) - Ord(vMenor1 < vMenor2);
end;

function NomeDaAcao(AAcao: TAcaoRegra): string;
begin
  Result := AcaoNomes[AAcao];
end;

function TentarAcao(const ANome: string; out AAcao: TAcaoRegra): boolean;
var
  vAcao: TAcaoRegra;
begin
  Result := False;
  for vAcao := Low(TAcaoRegra) to High(TAcaoRegra) do
    if AcaoNomes[vAcao] = ANome then
    begin
      AAcao := vAcao;
      Exit(True);
    end;
end;

function NomeDoTipoCondicao(ATipo: TTipoCondicao): string;
begin
  Result := CondicaoNomes[ATipo];
end;

function TentarTipoCondicao(const ANome: string; out ATipo: TTipoCondicao): boolean;
var
  vTipo: TTipoCondicao;
begin
  Result := False;
  for vTipo := Low(TTipoCondicao) to High(TTipoCondicao) do
    if CondicaoNomes[vTipo] = ANome then
    begin
      ATipo := vTipo;
      Exit(True);
    end;
end;

function RecursoDoPasso(const ADe, APara: string): string;
begin
  Result := 'REGRAS_' + StringReplace(NormalizarVersao(ADe), '.', '_', [rfReplaceAll]) +
    '__' + StringReplace(NormalizarVersao(APara), '.', '_', [rfReplaceAll]);
end;

function EmLista(const AValor, ALista: string; ASeparador: char = ','): boolean;
var
  vItens: TStringList;
begin
  vItens := TStringList.Create;
  try
    vItens.Delimiter := ASeparador;
    vItens.StrictDelimiter := True;
    vItens.DelimitedText := ALista;
    Result := vItens.IndexOf(AValor) >= 0;
  finally
    vItens.Free;
  end;
end;

{ TTextoIdiomas }

constructor TTextoIdiomas.Create;
begin
  inherited Create;
  FIdiomas := TStringList.Create;
  FTextos := TStringList.Create;
end;

destructor TTextoIdiomas.Destroy;
begin
  FIdiomas.Free;
  FTextos.Free;
  inherited Destroy;
end;

procedure TTextoIdiomas.Definir(const AIdioma, ATexto: string);
var
  vIdioma: string;
  vInd: integer;
begin
  vIdioma := NormalizarIdioma(AIdioma);
  if vIdioma = '' then
    vIdioma := AIdioma;
  vInd := FIdiomas.IndexOf(vIdioma);
  if vInd >= 0 then
    FTextos[vInd] := ATexto
  else
  begin
    FIdiomas.Add(vIdioma);
    FTextos.Add(ATexto);
  end;
end;

function TTextoIdiomas.Tem(const AIdioma: string): boolean;
begin
  Result := FIdiomas.IndexOf(NormalizarIdioma(AIdioma)) >= 0;
end;

function TTextoIdiomas.Obter(const AIdioma: string): string;
var
  vInd: integer;
begin
  vInd := FIdiomas.IndexOf(NormalizarIdioma(AIdioma));
  if vInd < 0 then
    vInd := FIdiomas.IndexOf(IdiomaEnUS);
  if vInd < 0 then
    vInd := FIdiomas.IndexOf(IdiomaPtBR);
  if (vInd < 0) and (FIdiomas.Count > 0) then
    vInd := 0;
  if vInd < 0 then
    Result := ''
  else
    Result := FTextos[vInd];
end;

function TTextoIdiomas.Vazio: boolean;
begin
  Result := FIdiomas.Count = 0;
end;

function TTextoIdiomas.Completo: boolean;
var
  vInt: integer;
begin
  Result := True;
  for vInt := Low(Idiomas) to High(Idiomas) do
    if not Tem(Idiomas[vInt]) then
      Exit(False);
end;

{ TCondicaoRegra }

constructor TCondicaoRegra.Create;
begin
  inherited Create;
  FFilhos := TObjectList.Create(True);
end;

destructor TCondicaoRegra.Destroy;
begin
  FFilhos.Free;
  inherited Destroy;
end;

function TCondicaoRegra.GetFilho(AIndex: integer): TCondicaoRegra;
begin
  Result := TCondicaoRegra(FFilhos[AIndex]);
end;

function TCondicaoRegra.GetFilhoCount: integer;
begin
  Result := FFilhos.Count;
end;

procedure TCondicaoRegra.AddFilho(AFilho: TCondicaoRegra);
begin
  FFilhos.Add(AFilho);
end;

function TCondicaoRegra.Descrever: string;
var
  vInt: integer;
begin
  Result := CondicaoNomes[Tipo];
  case Tipo of
    tcTemPropriedade, tcValorNaoVazio:
      Result := Result + '(' + Propriedade + ')';
    tcValorIgual, tcValorDiferente:
      Result := Result + '(' + Propriedade + ',' + Valor + ')';
    tcClasseDescendeDe:
      Result := Result + '(' + Classe + ')';
    tcUnitNoUses:
      Result := Result + '(' + NomeUnit + ')';
    tcSetContem:
      Result := Result + '(' + Propriedade + ',' + Opcao + ')';
    tcAlguma, tcTodas:
    begin
      Result := Result + '(';
      for vInt := 0 to Pred(FilhoCount) do
      begin
        if vInt > 0 then
          Result := Result + ';';
        Result := Result + Filhos[vInt].Descrever;
      end;
      Result := Result + ')';
    end;
  end;
  if Negar then
    Result := 'nao ' + Result;
end;

{ TRegraMigracao }

constructor TRegraMigracao.Create;
begin
  inherited Create;
  Onde := TStringList.Create;
  FEscalares := TStringList.Create;
  FEscalares.CaseSensitive := False;
  FListas := TStringList.Create;
  FListas.CaseSensitive := False;
  FListas.OwnsObjects := True;
  FMapas := TStringList.Create;
  FMapas.CaseSensitive := False;
  FMapas.OwnsObjects := True;
  FTextos := TStringList.Create;
  FTextos.CaseSensitive := False;
  FTextos.OwnsObjects := True;
  FVazia := TStringList.Create;
end;

destructor TRegraMigracao.Destroy;
begin
  Onde.Free;
  FEscalares.Free;
  FListas.Free;
  FMapas.Free;
  FTextos.Free;
  FVazia.Free;
  Condicao.Free;
  CriarQuando.Free;
  inherited Destroy;
end;

function TRegraMigracao.NomeAcao: string;
begin
  Result := AcaoNomes[Acao];
end;

function TRegraMigracao.AplicaEm(const AOnde: string): boolean;
begin
  Result := Onde.IndexOf(AOnde) >= 0;
end;

function TRegraMigracao.BuscarTexto(const ACampo: string): TTextoIdiomas;
var
  vInd: integer;
begin
  vInd := FTextos.IndexOf(ACampo);
  if vInd < 0 then
    Result := nil
  else
    Result := TTextoIdiomas(FTextos.Objects[vInd]);
end;

function TRegraMigracao.Texto(const ACampo, AIdioma: string): string;
var
  vTexto: TTextoIdiomas;
begin
  vTexto := BuscarTexto(ACampo);
  if vTexto = nil then
    Result := ''
  else
    Result := vTexto.Obter(AIdioma);
end;

function TRegraMigracao.TemCampo(const ANome: string): boolean;
begin
  Result := (FEscalares.IndexOfName(ANome) >= 0) or (FListas.IndexOf(ANome) >= 0) or
    (FMapas.IndexOf(ANome) >= 0);
end;

function TRegraMigracao.Param(const ANome: string; const ADefault: string): string;
var
  vInd: integer;
begin
  vInd := FEscalares.IndexOfName(ANome);
  if vInd < 0 then
    Result := ADefault
  else
    Result := FEscalares.ValueFromIndex[vInd];
end;

function TRegraMigracao.ParamBool(const ANome: string; ADefault: boolean): boolean;
var
  vValor: string;
begin
  vValor := Param(ANome, '');
  if vValor = '' then
    Result := ADefault
  else
    Result := SameText(vValor, 'true');
end;

function TRegraMigracao.Lista(const ANome: string): TStringList;
var
  vInd: integer;
begin
  vInd := FListas.IndexOf(ANome);
  if vInd < 0 then
    Result := FVazia
  else
    Result := TStringList(FListas.Objects[vInd]);
end;

function TRegraMigracao.Mapa(const ANome: string): TStringList;
var
  vInd: integer;
begin
  vInd := FMapas.IndexOf(ANome);
  if vInd < 0 then
    Result := FVazia
  else
    Result := TStringList(FMapas.Objects[vInd]);
end;

function TRegraMigracao.PadraoTipo: string;
begin
  Result := Mapa('padrao').Values['tipo'];
end;

procedure TRegraMigracao.DefinirEscalar(const ANome, AValor: string);
begin
  FEscalares.Add(ANome + '=' + AValor);
end;

function TRegraMigracao.NovaLista(const ANome: string): TStringList;
begin
  Result := TStringList.Create;
  FListas.AddObject(ANome, Result);
end;

function TRegraMigracao.NovoMapa(const ANome: string): TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  FMapas.AddObject(ANome, Result);
end;

procedure TRegraMigracao.DefinirTexto(const ACampo: string; ATexto: TTextoIdiomas);
begin
  FTextos.AddObject(ACampo, ATexto);
end;

{ TComportamentoMigracao }

constructor TComportamentoMigracao.Create;
begin
  inherited Create;
  FDescricao := TTextoIdiomas.Create;
  FFazer := TTextoIdiomas.Create;
end;

destructor TComportamentoMigracao.Destroy;
begin
  FDescricao.Free;
  FFazer.Free;
  inherited Destroy;
end;

function TComportamentoMigracao.Texto(const ACampo, AIdioma: string): string;
begin
  if SameText(ACampo, 'fazer') then
    Result := FFazer.Obter(AIdioma)
  else
    Result := FDescricao.Obter(AIdioma);
end;

{ TPassoMigracao }

constructor TPassoMigracao.Create;
begin
  inherited Create;
  FRegras := TObjectList.Create(True);
  FComportamento := TObjectList.Create(True);
end;

destructor TPassoMigracao.Destroy;
begin
  FRegras.Free;
  FComportamento.Free;
  inherited Destroy;
end;

function TPassoMigracao.GetRegra(AIndex: integer): TRegraMigracao;
begin
  Result := TRegraMigracao(FRegras[AIndex]);
end;

function TPassoMigracao.GetRegraCount: integer;
begin
  Result := FRegras.Count;
end;

function TPassoMigracao.GetComp(AIndex: integer): TComportamentoMigracao;
begin
  Result := TComportamentoMigracao(FComportamento[AIndex]);
end;

function TPassoMigracao.GetCompCount: integer;
begin
  Result := FComportamento.Count;
end;

function TPassoMigracao.Rotulo: string;
begin
  Result := De + '>' + Para;
end;

function TPassoMigracao.BuscarRegra(const AId: string): TRegraMigracao;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(FRegras.Count) do
    if SameText(Regras[vInt].Id, AId) then
      Exit(Regras[vInt]);
end;

function TPassoMigracao.BuscarComportamento(const AId: string): TComportamentoMigracao;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(FComportamento.Count) do
    if SameText(Comportamento[vInt].Id, AId) then
      Exit(Comportamento[vInt]);
end;

procedure TPassoMigracao.PutRegra(ARegra: TRegraMigracao);
var
  vInt: integer;
begin
  for vInt := 0 to Pred(FRegras.Count) do
    if SameText(Regras[vInt].Id, ARegra.Id) then
    begin
      FRegras[vInt] := ARegra;
      Exit;
    end;
  FRegras.Add(ARegra);
end;

procedure TPassoMigracao.PutComportamento(AComp: TComportamentoMigracao);
var
  vInt: integer;
begin
  for vInt := 0 to Pred(FComportamento.Count) do
    if SameText(Comportamento[vInt].Id, AComp.Id) then
    begin
      FComportamento[vInt] := AComp;
      Exit;
    end;
  FComportamento.Add(AComp);
end;

{ TListaPassos }

constructor TListaPassos.Create;
begin
  inherited Create;
  FLista := TList.Create;
end;

destructor TListaPassos.Destroy;
begin
  FLista.Free;
  inherited Destroy;
end;

function TListaPassos.GetCount: integer;
begin
  Result := FLista.Count;
end;

function TListaPassos.GetItem(AIndex: integer): TPassoMigracao;
begin
  Result := TPassoMigracao(FLista[AIndex]);
end;

procedure TListaPassos.Add(APasso: TPassoMigracao);
begin
  FLista.Add(APasso);
end;

procedure TListaPassos.Remover(AIndex: integer);
begin
  FLista.Delete(AIndex);
end;

{ the reader }

type
  /// Reads and validates one rule file; collects every problem.
  TLeitorRegras = class
  private
    FErros: TStringList;
    FEstrito: boolean;
    FNome: string;
    procedure Falha(const AOnde, AMsg: string);
    function CampoSpec(const ASpec, ANome: string; out ATipo: char;
      out AEnum: string; out AObrig: boolean): boolean;
    function TipoOk(AData: TJSONData; ATipo: char; const AEnum: string;
      out AMsg: string): boolean;
    procedure ValidarCampos(AObj: TJSONObject; const ASpec, AOnde: string);
    function LerCondicao(AObj: TJSONObject; const AOnde: string;
      ANivel: integer): TCondicaoRegra;
    function LerTexto(AObj: TJSONObject): TTextoIdiomas;
    procedure CopiarCampos(AObj: TJSONObject; const ASpec, AOnde: string;
      ARegra: TRegraMigracao);
    procedure ConferirId(const AId, AOnde, APara: string);
    procedure ConferirRegra(ARegra: TRegraMigracao; const AOnde, APara: string);
    procedure ConferirPadrao(ARegra: TRegraMigracao; const AOnde: string);
    procedure LerItensDoPasso(AObj: TJSONObject; const AOnde: string;
      APasso: TPassoMigracao);
    function LerRegra(AObj: TJSONObject; const AOnde, APara: string): TRegraMigracao;
    function LerComportamento(AObj: TJSONObject; const AOnde, APara: string): TComportamentoMigracao;
    function LerPasso(AObj: TJSONObject; const AOnde: string): TPassoMigracao;
  public
    constructor Create(const ANome: string; AEstrito: boolean; AErros: TStringList);
    /// Fills APassos (owned by the caller) or leaves Erros non empty
    procedure Ler(const ATexto: string; APassos: TObjectList);
  end;

/// The value of a map entry: the text, or the list joined with ','
function ValorDoMapa(AData: TJSONData): string;
var
  vInt: integer;
begin
  if AData.JSONType <> jtArray then
    Exit(AData.AsString);
  Result := '';
  for vInt := 0 to Pred(TJSONArray(AData).Count) do
  begin
    if vInt > 0 then
      Result := Result + ',';
    Result := Result + TJSONArray(AData).Strings[vInt];
  end;
end;

function SpecDaAcao(AAcao: TAcaoRegra): string;
begin
  case AAcao of
    arPacoteRenomear:
      Result := 'de:s!;para:s!;ide:' + SpecIdeTres + '!;de-lazarus:s;para-lazarus:s;' +
        'no-uses:b;instalar-na-ide:s';
    arPacoteExigirSeUnit:
      Result := 'units:l!;pacote:s!;pacote-lazarus:s;ide:' + SpecIdeTres + '!';
    arCaminhoTrocar:
      Result := 'de:s!;modo:e(trocar|acrescentar)!;para:l!;para-se-unit:m';
    arUnitRenomear:
      Result := 'mapa:m!';
    arUsesAdicionar:
      Result := 'unit:s!;quando-identificador:l!';
    arIdentificadorRenomear:
      Result := 'mapa:m!;enumerado:s';
    arPropriedadeRenomear:
      Result := 'classe:s!;propriedade:s!;para:s!';
    arPropriedadeParaComponente:
      Result := 'classe-dona:s!;propriedades:l!;mapeamento:m!;componente:s!;unit:s!;' +
        'nome-base:s!;ligacao:s!;criar-quando:c!;descartar:l;omitir-valores:m';
    arValorPadraoMaterializar:
      Result := 'classe:s!;propriedade:s!;valor:s!;quando:e(ausente);' +
        'modo:e(escrever|so-relatar|perguntar)';
    arPropriedadeRemover:
      Result := 'classe:s!;propriedades:l!;valores-descartaveis:m';
    arObjetoSemEquivalente:
      Result := 'classes:l!;alternativa:s!;unit:s;identificadores:l';
    arPadraoAvisar:
      Result := 'padrao:m!;trecho:s';
    arPacoteDesignParaRuntime:
      Result := 'de:s!;para:s!;ide:' + SpecIdeTres + '!;manter-se:l';
  else
    Result := '';
  end;
end;

function SpecDaCondicao(ATipo: TTipoCondicao): string;
begin
  Result := 'tipo:s!;negar:b';
  case ATipo of
    tcTemPropriedade, tcValorNaoVazio:
      Result := Result + ';propriedade:s!';
    tcValorIgual, tcValorDiferente:
      Result := Result + ';propriedade:s!;valor:s!';
    tcClasseDescendeDe:
      Result := Result + ';classe:s!';
    tcUnitNoUses:
      Result := Result + ';unit:s!';
    tcSetContem:
      Result := Result + ';propriedade:s!;opcao:s!';
    tcAlguma, tcTodas:
      Result := Result + ';condicoes:k!';
  end;
end;

constructor TLeitorRegras.Create(const ANome: string; AEstrito: boolean;
  AErros: TStringList);
begin
  inherited Create;
  FNome := ANome;
  FEstrito := AEstrito;
  FErros := AErros;
end;

procedure TLeitorRegras.Falha(const AOnde, AMsg: string);
begin
  FErros.Add(FNome + ': ' + AOnde + ': ' + AMsg);
end;

function TLeitorRegras.CampoSpec(const ASpec, ANome: string; out ATipo: char;
  out AEnum: string; out AObrig: boolean): boolean;
var
  vItens: TStringList;
  vInt, vPos, vFim: integer;
  vToken, vResto: string;
begin
  Result := False;
  vItens := TStringList.Create;
  try
    vItens.Delimiter := ';';
    vItens.StrictDelimiter := True;
    vItens.DelimitedText := ASpec;
    for vInt := 0 to Pred(vItens.Count) do
    begin
      vToken := vItens[vInt];
      vPos := Pos(':', vToken);
      if (vPos = 0) or (Copy(vToken, 1, vPos - 1) <> ANome) then
        Continue;
      vResto := Copy(vToken, vPos + 1, MaxInt);
      AObrig := (vResto <> '') and (vResto[Length(vResto)] = '!');
      if AObrig then
        Delete(vResto, Length(vResto), 1);
      ATipo := vResto[1];
      AEnum := '';
      vPos := Pos('(', vResto);
      vFim := Pos(')', vResto);
      if (vPos > 0) and (vFim > vPos) then
        AEnum := StringReplace(Copy(vResto, vPos + 1, vFim - vPos - 1), '|', ',',
                               [rfReplaceAll]);
      Exit(True);
    end;
  finally
    vItens.Free;
  end;
end;

function TLeitorRegras.TipoOk(AData: TJSONData; ATipo: char; const AEnum: string;
  out AMsg: string): boolean;
var
  vInt, vInd: integer;
  vObj: TJSONObject;
  vItem: TJSONData;
begin
  Result := False;
  AMsg := '';
  case ATipo of
    's': Result := AData.JSONType = jtString;
    'b': Result := AData.JSONType = jtBoolean;
    'n': Result := AData.JSONType = jtNumber;
    'a': Result := AData.JSONType = jtArray;
    'c': Result := AData.JSONType = jtObject;
    'e':
    begin
      Result := (AData.JSONType = jtString) and EmLista(AData.AsString, AEnum);
      if not Result then
        AMsg := 'valor invalido (esperado um de: ' + AEnum + ')';
    end;
    'l', 'k':
    begin
      Result := AData.JSONType = jtArray;
      if Result then
        for vInt := 0 to Pred(TJSONArray(AData).Count) do
        begin
          vItem := TJSONArray(AData).Items[vInt];
          if (ATipo = 'l') and (vItem.JSONType <> jtString) then
            Result := False;
          if (ATipo = 'k') and (vItem.JSONType <> jtObject) then
            Result := False;
        end;
    end;
    'm':
    begin
      Result := AData.JSONType = jtObject;
      if Result then
      begin
        vObj := TJSONObject(AData);
        for vInd := 0 to Pred(vObj.Count) do
        begin
          vItem := vObj.Items[vInd];
          if vItem.JSONType = jtArray then
          begin
            for vInt := 0 to Pred(TJSONArray(vItem).Count) do
              if TJSONArray(vItem).Items[vInt].JSONType <> jtString then
                Result := False;
          end
          else if vItem.JSONType <> jtString then
            Result := False;
        end;
      end;
    end;
    't':
    begin
      Result := (AData.JSONType = jtObject) and (TJSONObject(AData).Count > 0);
      if Result then
      begin
        vObj := TJSONObject(AData);
        for vInd := 0 to Pred(vObj.Count) do
        begin
          if (vObj.Items[vInd].JSONType <> jtString) or (vObj.Items[vInd].AsString = '') then
            Result := False
          else if not EmLista(vObj.Names[vInd], IdiomaPtBR + ',' + IdiomaEnUS + ',' + IdiomaEsES) then
          begin
            Result := False;
            AMsg := 'idioma desconhecido "' + vObj.Names[vInd] + '"';
          end;
        end;
        if Result and FEstrito then
          for vInt := Low(Idiomas) to High(Idiomas) do
            if vObj.Find(Idiomas[vInt]) = nil then
            begin
              Result := False;
              AMsg := 'falta o idioma ' + Idiomas[vInt];
            end;
      end;
    end;
  end;
  if (not Result) and (AMsg = '') then
    case ATipo of
      's': AMsg := 'esperado texto';
      'b': AMsg := 'esperado true ou false';
      'n': AMsg := 'esperado numero';
      'a': AMsg := 'esperada lista de objetos';
      'c': AMsg := 'esperado objeto';
      'l': AMsg := 'esperada lista de textos';
      'k': AMsg := 'esperada lista de objetos';
      'm': AMsg := 'esperado objeto com textos (ou listas de textos)';
      't': AMsg := 'esperado objeto {"pt-BR": ..., "en-US": ..., "es-ES": ...} com textos nao vazios';
    end;
end;

procedure TLeitorRegras.ValidarCampos(AObj: TJSONObject; const ASpec, AOnde: string);
var
  vInt: integer;
  vNome, vMsg, vEnum: string;
  vTipo: char;
  vObrig: boolean;
  vItens: TStringList;
  vData: TJSONData;
begin
  for vInt := 0 to Pred(AObj.Count) do
    if not CampoSpec(ASpec, AObj.Names[vInt], vTipo, vEnum, vObrig) then
      Falha(AOnde, 'campo desconhecido "' + AObj.Names[vInt] + '"')
    else if not TipoOk(AObj.Items[vInt], vTipo, vEnum, vMsg) then
      Falha(AOnde, 'campo "' + AObj.Names[vInt] + '": ' + vMsg);
  vItens := TStringList.Create;
  try
    vItens.Delimiter := ';';
    vItens.StrictDelimiter := True;
    vItens.DelimitedText := ASpec;
    for vInt := 0 to Pred(vItens.Count) do
    begin
      vNome := Copy(vItens[vInt], 1, Pos(':', vItens[vInt]) - 1);
      if CampoSpec(ASpec, vNome, vTipo, vEnum, vObrig) and vObrig then
      begin
        vData := AObj.Find(vNome);
        if vData = nil then
          Falha(AOnde, 'falta o campo "' + vNome + '"');
      end;
    end;
  finally
    vItens.Free;
  end;
end;

function TLeitorRegras.LerCondicao(AObj: TJSONObject; const AOnde: string;
  ANivel: integer): TCondicaoRegra;
var
  vTipo: TTipoCondicao;
  vNome: string;
  vArr: TJSONArray;
  vFilho: TCondicaoRegra;
  vInt, vAntes: integer;
begin
  Result := nil;
  if ANivel > 8 then
  begin
    Falha(AOnde, 'condicao aninhada demais');
    Exit;
  end;
  if (AObj.Find('tipo') = nil) or (AObj.Find('tipo').JSONType <> jtString) then
  begin
    Falha(AOnde, 'condicao sem "tipo"');
    Exit;
  end;
  vNome := AObj.Find('tipo').AsString;
  if not TentarTipoCondicao(vNome, vTipo) then
  begin
    Falha(AOnde, 'condicao desconhecida "' + vNome + '"');
    Exit;
  end;
  vAntes := FErros.Count;
  ValidarCampos(AObj, SpecDaCondicao(vTipo), AOnde + ' condicao "' + vNome + '"');
  if FErros.Count > vAntes then
    Exit;
  Result := TCondicaoRegra.Create;
  Result.Tipo := vTipo;
  Result.Propriedade := AObj.Get('propriedade', '');
  Result.Valor := AObj.Get('valor', '');
  Result.Classe := AObj.Get('classe', '');
  Result.NomeUnit := AObj.Get('unit', '');
  Result.Opcao := AObj.Get('opcao', '');
  Result.Negar := AObj.Get('negar', False);
  if vTipo in [tcAlguma, tcTodas] then
  begin
    vArr := TJSONArray(AObj.Find('condicoes'));
    for vInt := 0 to Pred(vArr.Count) do
    begin
      vFilho := LerCondicao(vArr.Objects[vInt], AOnde, ANivel + 1);
      if vFilho <> nil then
        Result.AddFilho(vFilho);
    end;
    if (vArr.Count = 0) or (Result.FilhoCount <> vArr.Count) then
    begin
      if vArr.Count = 0 then
        Falha(AOnde, 'condicao "' + vNome + '" sem condicoes');
      FreeAndNil(Result);
    end;
  end;
end;

function TLeitorRegras.LerTexto(AObj: TJSONObject): TTextoIdiomas;
var
  vInt: integer;
begin
  Result := TTextoIdiomas.Create;
  for vInt := 0 to Pred(AObj.Count) do
    Result.Definir(AObj.Names[vInt], AObj.Items[vInt].AsString);
end;

procedure TLeitorRegras.CopiarCampos(AObj: TJSONObject; const ASpec, AOnde: string;
  ARegra: TRegraMigracao);
var
  vInt, vIt: integer;
  vNome, vEnum: string;
  vTipo: char;
  vObrig: boolean;
  vData: TJSONData;
  vObj: TJSONObject;
  vLista: TStringList;
  vCond: TCondicaoRegra;
begin
  for vInt := 0 to Pred(AObj.Count) do
  begin
    vNome := AObj.Names[vInt];
    vData := AObj.Items[vInt];
    if not CampoSpec(ASpec, vNome, vTipo, vEnum, vObrig) then
      Continue;
    case vTipo of
      's', 'e': ARegra.DefinirEscalar(vNome, vData.AsString);
      'b': ARegra.DefinirEscalar(vNome, LowerCase(BoolToStr(vData.AsBoolean, True)));
      'l':
      begin
        vLista := ARegra.NovaLista(vNome);
        for vIt := 0 to Pred(TJSONArray(vData).Count) do
          vLista.Add(TJSONArray(vData).Strings[vIt]);
      end;
      'm':
      begin
        vLista := ARegra.NovoMapa(vNome);
        vObj := TJSONObject(vData);
        for vIt := 0 to Pred(vObj.Count) do
          vLista.Add(vObj.Names[vIt] + '=' + ValorDoMapa(vObj.Items[vIt]));
      end;
      't': ARegra.DefinirTexto(vNome, LerTexto(TJSONObject(vData)));
      'c':
      begin
        vCond := LerCondicao(TJSONObject(vData), AOnde + ' campo "' + vNome + '"', 0);
        if vNome = 'condicao' then
          ARegra.Condicao := vCond
        else
          ARegra.CriarQuando := vCond;
      end;
    end;
  end;
end;

procedure TLeitorRegras.ConferirPadrao(ARegra: TRegraMigracao; const AOnde: string);
var
  vMapa: TStringList;
  vTipo, vNeeds, vChave: string;
  vNecessarios: TStringList;
  vInt: integer;
begin
  vMapa := ARegra.Mapa('padrao');
  vTipo := vMapa.Values['tipo'];
  if not EmLista(vTipo, PadraoTipos) then
  begin
    Falha(AOnde, 'padrao: tipo desconhecido "' + vTipo + '"');
    Exit;
  end;
  if vTipo = 'identificador' then
    vNeeds := 'nomes'
  else if vTipo = 'chamada-metodo' then
    vNeeds := 'classe,metodos'
  else if vTipo = 'atribuicao-propriedade' then
    vNeeds := 'classe,propriedade'
  else if vTipo = 'conversao-tipo' then
    vNeeds := 'tipo-alvo,membro'
  else if vTipo = 'ausencia-atribuicao' then
    vNeeds := 'criacao-de,propriedade'
  else if vTipo = 'sobrescrita-metodo' then
    vNeeds := 'ancestral,metodo'
  else
    vNeeds := '';
  vNecessarios := TStringList.Create;
  try
    vNecessarios.CommaText := vNeeds;
    for vInt := 0 to Pred(vNecessarios.Count) do
      if vMapa.IndexOfName(vNecessarios[vInt]) < 0 then
        Falha(AOnde, 'padrao "' + vTipo + '": falta "' + vNecessarios[vInt] + '"');
    for vInt := 0 to Pred(vMapa.Count) do
    begin
      vChave := vMapa.Names[vInt];
      if (vChave <> 'tipo') and (vNecessarios.IndexOf(vChave) < 0) and
         (not EmLista(vChave, 'metodos,nomes,propriedade,classe')) then
        Falha(AOnde, 'padrao "' + vTipo + '": campo desconhecido "' + vChave + '"');
    end;
  finally
    vNecessarios.Free;
  end;
end;

procedure TLeitorRegras.ConferirId(const AId, AOnde, APara: string);
var
  vPos: integer;
begin
  vPos := Pos('/', AId);
  if (vPos < 2) or (vPos = Length(AId)) then
    Falha(AOnde, 'id deve ser <versao>/<nome>')
  else if FEstrito and (NormalizarVersao(Copy(AId, 1, vPos - 1)) <> APara) then
    Falha(AOnde, 'o prefixo do id deve ser a versao de destino do passo (' + APara + ')');
end;

procedure TLeitorRegras.ConferirRegra(ARegra: TRegraMigracao; const AOnde, APara: string);
var
  vInt: integer;
begin
  for vInt := 0 to Pred(ARegra.Onde.Count) do
    if not EmLista(ARegra.Onde[vInt], OndeValidos) then
      Falha(AOnde, 'onde: arquivo desconhecido "' + ARegra.Onde[vInt] + '"');
  if (ARegra.Onde.Count = 0) and (ARegra.Acao <> arPadraoAvisar) then
    Falha(AOnde, '"onde" nao pode ser vazio nesta acao');
  if (ARegra.Param('ide') = 'ambos') and (ARegra.Acao in [arPacoteRenomear, arPacoteExigirSeUnit]) then
  begin
    if ARegra.Acao = arPacoteRenomear then
    begin
      if (ARegra.Param('de-lazarus') = '') or (ARegra.Param('para-lazarus') = '') then
        Falha(AOnde, 'ide "ambos" pede "de-lazarus" e "para-lazarus"');
    end
    else if ARegra.Param('pacote-lazarus') = '' then
      Falha(AOnde, 'ide "ambos" pede "pacote-lazarus"');
  end;
  if ARegra.Acao = arPadraoAvisar then
    ConferirPadrao(ARegra, AOnde);
  if ARegra.Acao = arPacoteDesignParaRuntime then
    for vInt := 0 to Pred(ARegra.Lista('manter-se').Count) do
      if not EmLista(ARegra.Lista('manter-se')[vInt], MantemValidos) then
        Falha(AOnde, 'manter-se: valor desconhecido "' + ARegra.Lista('manter-se')[vInt] + '"');
  ConferirId(ARegra.Id, AOnde, APara);
end;

function TLeitorRegras.LerRegra(AObj: TJSONObject; const AOnde, APara: string): TRegraMigracao;
var
  vId, vAcaoNome, vOnde, vSpec: string;
  vAcao: TAcaoRegra;
  vAntes: integer;
begin
  Result := nil;
  vAntes := FErros.Count;
  vId := '';
  if (AObj.Find('id') <> nil) and (AObj.Find('id').JSONType = jtString) then
    vId := AObj.Find('id').AsString;
  vOnde := AOnde;
  if vId <> '' then
    vOnde := AOnde + ' regra "' + vId + '"';
  if (AObj.Find('acao') = nil) or (AObj.Find('acao').JSONType <> jtString) then
  begin
    Falha(vOnde, 'falta "acao" (texto)');
    Exit;
  end;
  vAcaoNome := AObj.Find('acao').AsString;
  if not TentarAcao(vAcaoNome, vAcao) then
  begin
    Falha(vOnde, 'acao desconhecida "' + vAcaoNome + '"');
    Exit;
  end;
  vSpec := SpecComum + ';' + SpecDaAcao(vAcao);
  ValidarCampos(AObj, vSpec, vOnde + ' (' + vAcaoNome + ')');
  if FErros.Count > vAntes then
    Exit;
  Result := TRegraMigracao.Create;
  Result.Id := vId;
  Result.Acao := vAcao;
  Result.Arquivo := FNome;
  Result.Wiki := AObj.Get('wiki', '');
  CopiarCampos(AObj, vSpec, vOnde, Result);
  Result.Onde.Assign(Result.Lista('onde'));
  ConferirRegra(Result, vOnde, APara);
  if FErros.Count > vAntes then
    FreeAndNil(Result);
end;

function TLeitorRegras.LerComportamento(AObj: TJSONObject; const AOnde,
  APara: string): TComportamentoMigracao;
var
  vAntes, vInt: integer;
  vId, vOnde: string;
  vTexto: TJSONObject;
begin
  Result := nil;
  vAntes := FErros.Count;
  vId := '';
  if (AObj.Find('id') <> nil) and (AObj.Find('id').JSONType = jtString) then
    vId := AObj.Find('id').AsString;
  vOnde := AOnde;
  if vId <> '' then
    vOnde := AOnde + ' comportamento "' + vId + '"';
  ValidarCampos(AObj, SpecComp, vOnde);
  if FErros.Count > vAntes then
    Exit;
  ConferirId(vId, vOnde, APara);
  if FErros.Count > vAntes then
    Exit;
  Result := TComportamentoMigracao.Create;
  Result.Id := vId;
  Result.Arquivo := FNome;
  Result.Wiki := AObj.Get('wiki', '');
  vTexto := TJSONObject(AObj.Find('descricao'));
  for vInt := 0 to Pred(vTexto.Count) do
    Result.Descricao.Definir(vTexto.Names[vInt], vTexto.Items[vInt].AsString);
  vTexto := TJSONObject(AObj.Find('fazer'));
  for vInt := 0 to Pred(vTexto.Count) do
    Result.Fazer.Definir(vTexto.Names[vInt], vTexto.Items[vInt].AsString);
end;

procedure TLeitorRegras.LerItensDoPasso(AObj: TJSONObject; const AOnde: string;
  APasso: TPassoMigracao);
var
  vInt: integer;
  vArr: TJSONArray;
  vRegra: TRegraMigracao;
  vComp: TComportamentoMigracao;
begin
  vArr := TJSONArray(AObj.Find('regras'));
  for vInt := 0 to Pred(vArr.Count) do
    if vArr.Items[vInt].JSONType <> jtObject then
      Falha(AOnde, Format('regras[%d]: esperado objeto', [vInt]))
    else
    begin
      vRegra := LerRegra(vArr.Objects[vInt], Format('%s regras[%d]', [AOnde, vInt]), APasso.Para);
      if vRegra = nil then
        Continue;
      if APasso.BuscarRegra(vRegra.Id) <> nil then
      begin
        Falha(AOnde, 'id de regra repetido "' + vRegra.Id + '"');
        vRegra.Free;
      end
      else
        APasso.PutRegra(vRegra);
    end;
  vArr := TJSONArray(AObj.Find('comportamento'));
  if vArr = nil then
    Exit;
  for vInt := 0 to Pred(vArr.Count) do
    if vArr.Items[vInt].JSONType <> jtObject then
      Falha(AOnde, Format('comportamento[%d]: esperado objeto', [vInt]))
    else
    begin
      vComp := LerComportamento(vArr.Objects[vInt], Format('%s comportamento[%d]', [AOnde, vInt]), APasso.Para);
      if vComp = nil then
        Continue;
      if APasso.BuscarComportamento(vComp.Id) <> nil then
      begin
        Falha(AOnde, 'id de comportamento repetido "' + vComp.Id + '"');
        vComp.Free;
      end
      else
        APasso.PutComportamento(vComp);
    end;
end;

function TLeitorRegras.LerPasso(AObj: TJSONObject; const AOnde: string): TPassoMigracao;
var
  vAntes: integer;
  vOrigem, vDe, vPara: string;
begin
  Result := nil;
  vAntes := FErros.Count;
  ValidarCampos(AObj, SpecPasso, AOnde);
  if FErros.Count > vAntes then
    Exit;
  if (AObj.Find('formato') <> nil) and (AObj.Find('formato').AsInteger <> FormatoRegras) then
  begin
    Falha(AOnde, 'formato nao suportado (esperado ' + IntToStr(FormatoRegras) + ')');
    Exit;
  end;
  vOrigem := AObj.Get('origem', '');
  vDe := NormalizarVersao(AObj.Get('de', ''));
  vPara := NormalizarVersao(AObj.Get('para', ''));
  if (vOrigem = '') or (Pos(' ', vOrigem) > 0) then
    Falha(AOnde, '"origem" invalida')
  else if (vDe = '') or (vPara = '') then
    Falha(AOnde, '"de" e "para" devem ser versoes (1.1, 2.0...)')
  else if CompararVersoes(vDe, vPara) >= 0 then
    Falha(AOnde, 'o passo tem de ir de uma versao para uma mais nova');
  if FErros.Count > vAntes then
    Exit;
  Result := TPassoMigracao.Create;
  Result.Origem := vOrigem;
  Result.De := vDe;
  Result.Para := vPara;
  LerItensDoPasso(AObj, AOnde + ' passo ' + Result.Rotulo, Result);
  if FErros.Count > vAntes then
    FreeAndNil(Result);
end;

procedure TLeitorRegras.Ler(const ATexto: string; APassos: TObjectList);
var
  vDados: TJSONData;
  vRaiz: TJSONObject;
  vArr: TJSONArray;
  vInt, vAntes: integer;
  vPasso: TPassoMigracao;
begin
  try
    vDados := GetJSON(ATexto);
  except
    on E: Exception do
    begin
      Falha('json', 'invalido: ' + E.Message);
      Exit;
    end;
  end;
  try
    if not (vDados is TJSONObject) then
    begin
      Falha('raiz', 'a raiz tem de ser um objeto');
      Exit;
    end;
    vRaiz := TJSONObject(vDados);
    if vRaiz.Find('passos') = nil then
    begin
      vPasso := LerPasso(vRaiz, 'arquivo');
      if vPasso <> nil then
        APassos.Add(vPasso);
      Exit;
    end;
    vAntes := FErros.Count;
    ValidarCampos(vRaiz, SpecRaiz, 'raiz');
    if FErros.Count > vAntes then
      Exit;
    vArr := TJSONArray(vRaiz.Find('passos'));
    for vInt := 0 to Pred(vArr.Count) do
      if vArr.Items[vInt].JSONType <> jtObject then
        Falha('raiz', Format('passos[%d]: esperado objeto', [vInt]))
      else
      begin
        vPasso := LerPasso(vArr.Objects[vInt], Format('passos[%d]', [vInt]));
        if vPasso <> nil then
          APassos.Add(vPasso);
      end;
  finally
    vDados.Free;
  end;
end;

{ file helpers }

function TextoDoArquivo(const AArquivo: string): string;
var
  vStream: TFileStream;
begin
  vStream := TFileStream.Create(AArquivo, fmOpenRead or fmShareDenyNone);
  try
    Result := '';
    SetLength(Result, vStream.Size);
    if vStream.Size > 0 then
      vStream.ReadBuffer(Result[1], vStream.Size);
  finally
    vStream.Free;
  end;
end;

function SemBOM(const ATexto: string): string;
begin
  if Copy(ATexto, 1, 3) = #$EF#$BB#$BF then
    Result := Copy(ATexto, 4, MaxInt)
  else
    Result := ATexto;
end;

{ TPassoMigracao (transfer) }

procedure TPassoMigracao.TransferirPara(ADestino: TPassoMigracao);
var
  vInt: integer;
begin
  for vInt := 0 to Pred(FRegras.Count) do
    ADestino.PutRegra(Regras[vInt]);
  for vInt := 0 to Pred(FComportamento.Count) do
    ADestino.PutComportamento(Comportamento[vInt]);
  FRegras.OwnsObjects := False;
  FComportamento.OwnsObjects := False;
end;

{ TConjuntoRegras }

constructor TConjuntoRegras.Create;
begin
  inherited Create;
  FErros := TStringList.Create;
  FPassos := TObjectList.Create(True);
end;

destructor TConjuntoRegras.Destroy;
begin
  FErros.Free;
  FPassos.Free;
  inherited Destroy;
end;

function TConjuntoRegras.GetPasso(AIndex: integer): TPassoMigracao;
begin
  Result := TPassoMigracao(FPassos[AIndex]);
end;

function TConjuntoRegras.GetPassoCount: integer;
begin
  Result := FPassos.Count;
end;

procedure TConjuntoRegras.Mesclar(APasso: TPassoMigracao);
var
  vExistente: TPassoMigracao;
begin
  vExistente := BuscarPasso(APasso.Origem, APasso.De, APasso.Para);
  if vExistente = nil then
  begin
    FPassos.Add(APasso);
    Exit;
  end;
  APasso.TransferirPara(vExistente);
  APasso.Free;
end;

function TConjuntoRegras.CarregarSimples(const ATexto, ANome: string;
  AEstrito: boolean): boolean;
var
  vLeitor: TLeitorRegras;
  vTemp: TObjectList;
  vAntes, vInt: integer;
begin
  vAntes := FErros.Count;
  vLeitor := TLeitorRegras.Create(ANome, AEstrito, FErros);
  vTemp := TObjectList.Create(True);
  try
    vLeitor.Ler(SemBOM(ATexto), vTemp);
    Result := FErros.Count = vAntes;
    if Result then
    begin
      vTemp.OwnsObjects := False;
      for vInt := 0 to Pred(vTemp.Count) do
        Mesclar(TPassoMigracao(vTemp[vInt]));
    end;
  finally
    vTemp.Free;
    vLeitor.Free;
  end;
end;

function TConjuntoRegras.CarregarTexto(const ATexto, ANome: string; AEstrito: boolean): boolean;
begin
  Result := CarregarSimples(ATexto, ANome, AEstrito);
end;

function TConjuntoRegras.CarregarArquivo(const AArquivo: string; AEstrito: boolean): boolean;
var
  vTexto: string;
begin
  if not FileExists(AArquivo) then
  begin
    FErros.Add(AArquivo + ': arquivo nao existe');
    Exit(False);
  end;
  try
    vTexto := TextoDoArquivo(AArquivo);
  except
    on E: Exception do
    begin
      FErros.Add(AArquivo + ': ' + E.Message);
      Exit(False);
    end;
  end;
  Result := CarregarSimples(vTexto, ExtractFileName(AArquivo), AEstrito);
end;

function TConjuntoRegras.CarregarDePasta(const APasta: string; AEstrito: boolean): boolean;
var
  vBusca: TSearchRec;
  vNomes: TStringList;
  vInt: integer;
  vRaiz: string;
begin
  Result := True;
  vRaiz := IncludeTrailingPathDelimiter(APasta);
  vNomes := TStringList.Create;
  try
    if FindFirst(vRaiz + '*.json', faAnyFile and not faDirectory, vBusca) = 0 then
    try
      repeat
        if not (SameText(vBusca.Name, 'cobertura.json') or
                SameText(vBusca.Name, ArquivoSobreposicao)) then
          vNomes.Add(vBusca.Name);
      until FindNext(vBusca) <> 0;
    finally
      SysUtils.FindClose(vBusca);
    end;
    vNomes.Sort;
    if vNomes.Count = 0 then
    begin
      FErros.Add(APasta + ': nenhum arquivo de regras (*.json)');
      Exit(False);
    end;
    for vInt := 0 to Pred(vNomes.Count) do
      if not CarregarArquivo(vRaiz + vNomes[vInt], AEstrito) then
        Result := False;
  finally
    vNomes.Free;
  end;
end;

const
  // RT_RCDATA: no Windows vem da unit Windows, nos outros do system
  RecursoDados = PChar(10);
  Embutidas: array[0..2, 0..1] of string = (('1.0', '1.1'), ('1.1', '1.2'), ('1.2', '2.0'));

function TConjuntoRegras.CarregarEmbutidas: boolean;
var
  vInt: integer;
  vNome, vTexto: string;
  vStream: TResourceStream;
begin
  Result := True;
  for vInt := Low(Embutidas) to High(Embutidas) do
  begin
    vNome := RecursoDoPasso(Embutidas[vInt, 0], Embutidas[vInt, 1]);
    try
      vStream := TResourceStream.Create(HINSTANCE, vNome, RecursoDados);
      try
        vTexto := '';
        SetLength(vTexto, vStream.Size);
        if vStream.Size > 0 then
          vStream.ReadBuffer(vTexto[1], vStream.Size);
      finally
        vStream.Free;
      end;
    except
      on EResNotFound do
      begin
        FErros.Add('recurso embutido ausente: ' + vNome);
        Result := False;
        Continue;
      end;
    end;
    if not CarregarSimples(vTexto, vNome, True) then
      Result := False;
  end;
end;

function TConjuntoRegras.AplicarSobreposicaoTexto(const ATexto, ANome: string): boolean;
begin
  Result := CarregarSimples(ATexto, ANome, False);
end;

function TConjuntoRegras.AplicarSobreposicao(const AArquivo: string): boolean;
begin
  Result := CarregarArquivo(AArquivo, False);
end;

function TConjuntoRegras.AplicarSobreposicaoDaRaiz(const ARaizRAL: string): boolean;
var
  vArquivo: string;
begin
  vArquivo := IncludeTrailingPathDelimiter(ARaizRAL) + ArquivoSobreposicao;
  if not FileExists(vArquivo) then
    Exit(True);
  Result := AplicarSobreposicao(vArquivo);
end;

function TConjuntoRegras.BuscarPasso(const AOrigem, ADe, APara: string): TPassoMigracao;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(FPassos.Count) do
    if SameText(Passos[vInt].Origem, AOrigem) and (Passos[vInt].De = ADe) and
       (Passos[vInt].Para = APara) then
      Exit(Passos[vInt]);
end;

function TConjuntoRegras.BuscarRegra(const AId: string): TRegraMigracao;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(FPassos.Count) do
  begin
    Result := Passos[vInt].BuscarRegra(AId);
    if Result <> nil then
      Exit;
  end;
end;

function TConjuntoRegras.BuscarComportamento(const AId: string): TComportamentoMigracao;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(FPassos.Count) do
  begin
    Result := Passos[vInt].BuscarComportamento(AId);
    if Result <> nil then
      Exit;
  end;
end;

function TConjuntoRegras.VersaoConhecida(const AOrigem, AVersao: string): boolean;
var
  vInt: integer;
  vVersao: string;
begin
  Result := False;
  vVersao := NormalizarVersao(AVersao);
  if vVersao = '' then
    Exit;
  for vInt := 0 to Pred(FPassos.Count) do
    if SameText(Passos[vInt].Origem, AOrigem) and
       ((Passos[vInt].De = vVersao) or (Passos[vInt].Para = vVersao)) then
      Exit(True);
end;

procedure TConjuntoRegras.ListarVersoes(const AOrigem: string; ADest: TStringList);
var
  vInt, vInd: integer;

  procedure Incluir(const AVersao: string);
  begin
    if ADest.IndexOf(AVersao) >= 0 then
      Exit;
    vInd := 0;
    while (vInd < ADest.Count) and (CompararVersoes(ADest[vInd], AVersao) < 0) do
      Inc(vInd);
    ADest.Insert(vInd, AVersao);
  end;

begin
  ADest.Clear;
  for vInt := 0 to Pred(FPassos.Count) do
    if SameText(Passos[vInt].Origem, AOrigem) then
    begin
      Incluir(Passos[vInt].De);
      Incluir(Passos[vInt].Para);
    end;
end;

function TConjuntoRegras.Cadeia(const AOrigem, ADe, APara: string; out AErro: TCadeiaErro;
  out AMensagem: string): TListaPassos;
var
  vDe, vPara, vAtual: string;
  vPasso, vMelhor: TPassoMigracao;
  vInt: integer;
  vAlemDoDestino: TPassoMigracao;
begin
  Result := TListaPassos.Create;
  AErro := ceNenhum;
  AMensagem := '';
  vDe := NormalizarVersao(ADe);
  vPara := NormalizarVersao(APara);
  if (vDe = '') or not VersaoConhecida(AOrigem, vDe) then
  begin
    AErro := ceVersaoDesconhecida;
    AMensagem := Format(SMsgVersaoDesconhecida, [ADe]);
    Exit;
  end;
  if (vPara = '') or not VersaoConhecida(AOrigem, vPara) then
  begin
    AErro := ceVersaoDesconhecida;
    AMensagem := Format(SMsgVersaoDesconhecida, [APara]);
    Exit;
  end;
  if CompararVersoes(vDe, vPara) > 0 then
  begin
    AErro := ceRetrocesso;
    AMensagem := Format(SMsgRetrocesso, [vPara, vDe]);
    Exit;
  end;
  vAtual := vDe;
  while CompararVersoes(vAtual, vPara) < 0 do
  begin
    // the longest step from here that does not pass the target
    vMelhor := nil;
    vAlemDoDestino := nil;
    for vInt := 0 to Pred(FPassos.Count) do
    begin
      vPasso := Passos[vInt];
      if not (SameText(vPasso.Origem, AOrigem) and (vPasso.De = vAtual)) then
        Continue;
      if CompararVersoes(vPasso.Para, vPara) > 0 then
        vAlemDoDestino := vPasso
      else if (vMelhor = nil) or (CompararVersoes(vPasso.Para, vMelhor.Para) > 0) then
        vMelhor := vPasso;
    end;
    if vMelhor = nil then
    begin
      AErro := ceSemPasso;
      if vAlemDoDestino <> nil then
        AMensagem := Format(SMsgDestinoNoMeio, [vPara, vAlemDoDestino.Rotulo, vAlemDoDestino.Para])
      else
        AMensagem := Format(SMsgSemPasso, [vAtual]);
      FreeAndNil(Result);
      Result := TListaPassos.Create;
      Exit;
    end;
    Result.Add(vMelhor);
    vAtual := vMelhor.Para;
  end;
end;

end.
