/// Types shared by the migration engine and the rule actions (M2-M5): the
/// files of the project being migrated, held in memory until the user applies,
/// the result (what was done, what needs the user, what behaves differently)
/// and the base class every action derives from. Nothing here writes to disk.
unit RALMig.Tipos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALMig.Texto, RALMig.API, RALMig.Regras;

type
  /// Kind of a project file, as the "onde" field of a rule names it
  TTipoArquivo = (taOutro, taDproj, taDpk, taLpi, taLpk, taLpr, taDpr, taPas,
    taForm, taGroupproj, taLpg);
  TTiposArquivo = set of TTipoArquivo;

  { TArquivoMigrado }

  /// One file of the project. The text is loaded on first use; edits stay in
  /// memory (TTextoOriginal) until the engine writes the result.
  TArquivoMigrado = class
  private
    FCaminho: string;
    FIgnorado: boolean;
    FMotivo: string;
    FNovo: boolean;
    FRelativo: string;
    FTexto: TTextoOriginal;
    FTipo: TTipoArquivo;
    function GetTexto: TTextoOriginal;
  public
    constructor Create(const ACaminho, ARelativo: string);
    destructor Destroy; override;
    /// True when the current text differs from the bytes on disk (or the file
    /// is new)
    function Alterado: boolean;
    /// The current bytes (original + edits already consolidated)
    function Atual: RawByteString;

    /// Absolute path
    property Caminho: string read FCaminho;
    /// Path relative to the project folder, with '/'
    property Relativo: string read FRelativo;
    property Tipo: TTipoArquivo read FTipo write FTipo;
    /// Created by the migration (it did not exist): undo deletes it
    property Novo: boolean read FNovo write FNovo;
    /// Left out (binary form the user did not let convert, form that does not
    /// parse...): no rule touches it; Motivo goes to the report
    property Ignorado: boolean read FIgnorado write FIgnorado;
    property Motivo: string read FMotivo write FMotivo;
    property Texto: TTextoOriginal read GetTexto;
  end;

  { TConjuntoArquivos }

  /// The files of one migration, owned by the set
  TConjuntoArquivos = class
  private
    FItens: TStringList;
    FPasta: string;
    function GetCount: integer;
    function GetItem(AIndex: integer): TArquivoMigrado;
  public
    constructor Create(const APasta: string);
    destructor Destroy; override;
    /// Adds a file on disk (absolute or relative to Pasta); the same file twice
    /// returns the first one
    function Adicionar(const ACaminho: string): TArquivoMigrado;
    /// A file the migration creates (AConteudo in UTF-8, line breaks of the OS)
    function CriarArquivo(const ARelativo: string; const AConteudo: RawByteString): TArquivoMigrado;
    /// By absolute or relative path, case-insensitive; nil when it is not there
    function Buscar(const ACaminho: string): TArquivoMigrado;
    /// The files of one kind (not owned by the list), ignored ones left out
    procedure Listar(ATipos: TTiposArquivo; ALista: TList);
    /// The form of a unit (.dfm/.lfm/.fmx with the same name) or nil
    function FormularioDe(AUnidade: TArquivoMigrado): TArquivoMigrado;
    /// The unit of a form (.pas with the same name) or nil
    function UnidadeDe(AFormulario: TArquivoMigrado): TArquivoMigrado;
    /// Consolidates the edits of every file, so the next rule sees them
    procedure Consolidar;
    /// The files that changed (or are new), in path order
    procedure ListarAlterados(ALista: TList);

    property Count: integer read GetCount;
    property Itens[AIndex: integer]: TArquivoMigrado read GetItem; default;
    /// Project folder, with a trailing delimiter
    property Pasta: string read FPasta;
  end;

  { TItemFeito }

  /// Something a rule changed
  TItemFeito = class
  public
    Regra: string;
    Arquivo: string;
    Linha: integer;
    Descricao: string;
  end;

  { TItemManual }

  /// Something a rule could not do safely: what, where and how to do it by hand
  TItemManual = class
  public
    Regra: string;
    Arquivo: string;
    Linha: integer;
    Problema: string;
    Fazer: string;
    /// Code ready to paste (compiles on Delphi XE and FPC 3.2), may be empty
    Trecho: string;
    Wiki: string;
  end;

  { TItemComportamento }

  /// A behaviour that changed between the versions (not an API break)
  TItemComportamento = class
  public
    Id: string;
    Passo: string;
    Descricao: string;
    Fazer: string;
    Wiki: string;
  end;

  { TResultadoMigracao }

  TResultadoMigracao = class
  private
    FComportamentos: TList;
    FDe: string;
    FErros: TStringList;
    FFeitos: TList;
    FIdioma: string;
    FManuais: TList;
    FPara: string;
    function GetComportamento(AIndex: integer): TItemComportamento;
    function GetComportamentoCount: integer;
    function GetFeito(AIndex: integer): TItemFeito;
    function GetFeitoCount: integer;
    function GetManual(AIndex: integer): TItemManual;
    function GetManualCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function AdicionarFeito(const ARegra, AArquivo: string; ALinha: integer;
      const ADescricao: string): TItemFeito;
    function AdicionarManual(const ARegra, AArquivo: string; ALinha: integer;
      const AProblema, AFazer, ATrecho, AWiki: string): TItemManual;
    /// The same id twice is kept once
    function AdicionarComportamento(const AId, APasso, ADescricao, AFazer,
      AWiki: string): TItemComportamento;

    property De: string read FDe write FDe;
    property Para: string read FPara write FPara;
    property Idioma: string read FIdioma write FIdioma;
    /// Errors that stopped part of the migration (file that could not be read...)
    property Erros: TStringList read FErros;
    property FeitoCount: integer read GetFeitoCount;
    property Feitos[AIndex: integer]: TItemFeito read GetFeito;
    property ManualCount: integer read GetManualCount;
    property Manuais[AIndex: integer]: TItemManual read GetManual;
    property ComportamentoCount: integer read GetComportamentoCount;
    property Comportamentos[AIndex: integer]: TItemComportamento read GetComportamento;
  end;

  { TContextoMigracao }

  /// What an action sees while it runs: the files, the result, the step being
  /// applied and the API of both ends of the step (nil when not available)
  TContextoMigracao = class
  public
    Conjunto: TConjuntoArquivos;
    Resultado: TResultadoMigracao;
    Passo: TPassoMigracao;
    ApiDe: TApi;
    ApiPara: TApi;
    Idioma: string;
    /// Convert binary forms to text (D-M6); False leaves them out
    ConverterBinarios: boolean;
  end;

  { TAcaoMigracao }

  /// Base of the rule actions. One instance per run; Aplicar is called once
  /// per rule of its kind, in the order of the step file. It edits the texts of
  /// the context (never the disk), reports each change with AdicionarFeito and
  /// each thing it cannot do with AdicionarManual (text of the rule, "manual",
  /// with the marks filled). Running it again on its own result changes nothing.
  /// The engine consolidates every text after each rule.
  TAcaoMigracao = class
  public
    constructor Create; virtual;
    /// False only on an internal error (the engine logs it and goes on)
    function Aplicar(ARegra: TRegraMigracao; AContexto: TContextoMigracao): boolean; virtual; abstract;
  end;
  TClasseAcaoMigracao = class of TAcaoMigracao;

/// Registers the class that runs an action (each RALMig.Acoes.* unit does it in
/// its initialization)
procedure RegistrarAcao(AAcao: TAcaoRegra; AClasse: TClasseAcaoMigracao);
/// The class of an action, nil when no unit registered one
function ClasseDaAcao(AAcao: TAcaoRegra): TClasseAcaoMigracao;

/// Kind of a file by its extension
function TipoDoArquivo(const ACaminho: string): TTipoArquivo;
/// Kind by the name used in the "onde" field ('dproj', 'form'...); taOutro if unknown
function TipoDoNome(const ANome: string): TTipoArquivo;
/// The kinds a rule acts on (from its "onde" list; empty list = every kind)
function TiposDaRegra(ARegra: TRegraMigracao): TTiposArquivo;

/// Fills the marks %nome% of a rule text: AMarcas = ['arquivo', 'x.pas', 'linha', '12']
function PreencherMarcas(const ATexto: string; const AMarcas: array of string): string;
/// Text of a rule in the language of the context with the marks filled
function TextoRegra(ARegra: TRegraMigracao; const ACampo: string;
  AContexto: TContextoMigracao; const AMarcas: array of string): string;

implementation

var
  GAcoes: array[TAcaoRegra] of TClasseAcaoMigracao;

procedure RegistrarAcao(AAcao: TAcaoRegra; AClasse: TClasseAcaoMigracao);
begin
  GAcoes[AAcao] := AClasse;
end;

function ClasseDaAcao(AAcao: TAcaoRegra): TClasseAcaoMigracao;
begin
  Result := GAcoes[AAcao];
end;

function TipoDoArquivo(const ACaminho: string): TTipoArquivo;
var
  vExt: string;
begin
  vExt := LowerCase(ExtractFileExt(ACaminho));
  if vExt = '.dproj' then
    Result := taDproj
  else if vExt = '.dpk' then
    Result := taDpk
  else if vExt = '.lpi' then
    Result := taLpi
  else if vExt = '.lpk' then
    Result := taLpk
  else if vExt = '.lpr' then
    Result := taLpr
  else if vExt = '.dpr' then
    Result := taDpr
  else if (vExt = '.pas') or (vExt = '.pp') then
    Result := taPas
  else if (vExt = '.dfm') or (vExt = '.lfm') or (vExt = '.fmx') then
    Result := taForm
  else if vExt = '.groupproj' then
    Result := taGroupproj
  else if vExt = '.lpg' then
    Result := taLpg
  else
    Result := taOutro;
end;

function TipoDoNome(const ANome: string): TTipoArquivo;
var
  vNome: string;
begin
  vNome := LowerCase(Trim(ANome));
  if vNome = 'form' then
    Result := taForm
  else if vNome = 'pas' then
    Result := taPas
  else
    Result := TipoDoArquivo('x.' + vNome);
end;

function TiposDaRegra(ARegra: TRegraMigracao): TTiposArquivo;
var
  vInt: integer;
  vTipo: TTipoArquivo;
begin
  Result := [];
  if (ARegra.Onde = nil) or (ARegra.Onde.Count = 0) then
  begin
    for vTipo := Low(TTipoArquivo) to High(TTipoArquivo) do
      Include(Result, vTipo);
    Exit;
  end;
  for vInt := 0 to ARegra.Onde.Count - 1 do
  begin
    vTipo := TipoDoNome(ARegra.Onde[vInt]);
    if vTipo <> taOutro then
      Include(Result, vTipo);
  end;
end;

function PreencherMarcas(const ATexto: string; const AMarcas: array of string): string;
var
  vInt: integer;
begin
  Result := ATexto;
  vInt := 0;
  while vInt < High(AMarcas) do
  begin
    Result := StringReplace(Result, '%' + AMarcas[vInt] + '%', AMarcas[vInt + 1],
                            [rfReplaceAll, rfIgnoreCase]);
    Inc(vInt, 2);
  end;
end;

function TextoRegra(ARegra: TRegraMigracao; const ACampo: string;
  AContexto: TContextoMigracao; const AMarcas: array of string): string;
var
  vIdioma: string;
begin
  vIdioma := '';
  if AContexto <> nil then
    vIdioma := AContexto.Idioma;
  Result := PreencherMarcas(ARegra.Texto(ACampo, vIdioma), AMarcas);
end;

function CaminhoAbsoluto(const ACaminho: string): boolean;
begin
  Result := (ExtractFileDrive(ACaminho) <> '') or
            ((ACaminho <> '') and (ACaminho[1] in ['\', '/']));
end;

function CaminhoRelativo(const APasta, ACaminho: string): string;
begin
  Result := ACaminho;
  if SameText(Copy(ACaminho, 1, Length(APasta)), APasta) then
    Result := Copy(ACaminho, Length(APasta) + 1, MaxInt);
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
end;

{ TArquivoMigrado }

constructor TArquivoMigrado.Create(const ACaminho, ARelativo: string);
begin
  inherited Create;
  FCaminho := ACaminho;
  FRelativo := ARelativo;
  FTipo := TipoDoArquivo(ACaminho);
end;

destructor TArquivoMigrado.Destroy;
begin
  FTexto.Free;
  inherited Destroy;
end;

function TArquivoMigrado.GetTexto: TTextoOriginal;
begin
  if FTexto = nil then
  begin
    FTexto := TTextoOriginal.Create;
    if FNovo or not FileExists(FCaminho) then
      FTexto.CarregarBytes('', FCaminho)
    else
      FTexto.CarregarArquivo(FCaminho);
  end;
  Result := FTexto;
end;

function TArquivoMigrado.Alterado: boolean;
begin
  if FNovo then
    Exit(True);
  if FTexto = nil then
    Exit(False);
  Result := FTexto.Alterado;
end;

function TArquivoMigrado.Atual: RawByteString;
begin
  Result := Texto.Resultado;
end;

{ TConjuntoArquivos }

constructor TConjuntoArquivos.Create(const APasta: string);
begin
  inherited Create;
  FPasta := IncludeTrailingPathDelimiter(ExpandFileName(APasta));
  FItens := TStringList.Create;
  FItens.OwnsObjects := True;
  FItens.CaseSensitive := False;
  FItens.Sorted := True;
  FItens.Duplicates := dupError;
end;

destructor TConjuntoArquivos.Destroy;
begin
  FItens.Free;
  inherited Destroy;
end;

function TConjuntoArquivos.GetCount: integer;
begin
  Result := FItens.Count;
end;

function TConjuntoArquivos.GetItem(AIndex: integer): TArquivoMigrado;
begin
  Result := TArquivoMigrado(FItens.Objects[AIndex]);
end;

function TConjuntoArquivos.Buscar(const ACaminho: string): TArquivoMigrado;
var
  vCaminho: string;
  vInd: integer;
begin
  Result := nil;
  vCaminho := ACaminho;
  if not CaminhoAbsoluto(vCaminho) then
    vCaminho := FPasta + vCaminho;
  vCaminho := ExpandFileName(vCaminho);
  vInd := FItens.IndexOf(vCaminho);
  if vInd >= 0 then
    Result := TArquivoMigrado(FItens.Objects[vInd]);
end;

function TConjuntoArquivos.Adicionar(const ACaminho: string): TArquivoMigrado;
var
  vCaminho: string;
begin
  Result := Buscar(ACaminho);
  if Result <> nil then
    Exit;
  vCaminho := ACaminho;
  if not CaminhoAbsoluto(vCaminho) then
    vCaminho := FPasta + vCaminho;
  vCaminho := ExpandFileName(vCaminho);
  Result := TArquivoMigrado.Create(vCaminho, CaminhoRelativo(FPasta, vCaminho));
  FItens.AddObject(vCaminho, Result);
end;

function TConjuntoArquivos.CriarArquivo(const ARelativo: string;
  const AConteudo: RawByteString): TArquivoMigrado;
begin
  Result := Adicionar(ARelativo);
  Result.Novo := not FileExists(Result.Caminho);
  Result.Texto.CarregarBytes('', Result.Caminho);
  Result.Texto.Codificacao := cdUtf8;
  if AConteudo <> '' then
    Result.Texto.Inserir(1, AConteudo);
end;

procedure TConjuntoArquivos.Listar(ATipos: TTiposArquivo; ALista: TList);
var
  vInt: integer;
begin
  for vInt := 0 to FItens.Count - 1 do
    if (Itens[vInt].Tipo in ATipos) and not Itens[vInt].Ignorado then
      ALista.Add(Itens[vInt]);
end;

function TConjuntoArquivos.FormularioDe(AUnidade: TArquivoMigrado): TArquivoMigrado;
var
  vBase: string;
begin
  vBase := ChangeFileExt(AUnidade.Caminho, '');
  Result := Buscar(vBase + '.dfm');
  if Result = nil then
    Result := Buscar(vBase + '.lfm');
  if Result = nil then
    Result := Buscar(vBase + '.fmx');
end;

function TConjuntoArquivos.UnidadeDe(AFormulario: TArquivoMigrado): TArquivoMigrado;
begin
  Result := Buscar(ChangeFileExt(AFormulario.Caminho, '.pas'));
  if Result = nil then
    Result := Buscar(ChangeFileExt(AFormulario.Caminho, '.pp'));
end;

procedure TConjuntoArquivos.Consolidar;
var
  vInt: integer;
  vArq: TArquivoMigrado;
begin
  for vInt := 0 to FItens.Count - 1 do
  begin
    vArq := Itens[vInt];
    if (vArq.FTexto <> nil) and (vArq.FTexto.EdicaoCount > 0) then
      vArq.FTexto.Consolidar;
  end;
end;

procedure TConjuntoArquivos.ListarAlterados(ALista: TList);
var
  vInt: integer;
begin
  for vInt := 0 to FItens.Count - 1 do
    if Itens[vInt].Alterado then
      ALista.Add(Itens[vInt]);
end;

{ TResultadoMigracao }

constructor TResultadoMigracao.Create;
begin
  inherited Create;
  FFeitos := TList.Create;
  FManuais := TList.Create;
  FComportamentos := TList.Create;
  FErros := TStringList.Create;
end;

destructor TResultadoMigracao.Destroy;
var
  vInt: integer;
begin
  for vInt := 0 to FFeitos.Count - 1 do
    TObject(FFeitos[vInt]).Free;
  for vInt := 0 to FManuais.Count - 1 do
    TObject(FManuais[vInt]).Free;
  for vInt := 0 to FComportamentos.Count - 1 do
    TObject(FComportamentos[vInt]).Free;
  FFeitos.Free;
  FManuais.Free;
  FComportamentos.Free;
  FErros.Free;
  inherited Destroy;
end;

function TResultadoMigracao.GetComportamento(AIndex: integer): TItemComportamento;
begin
  Result := TItemComportamento(FComportamentos[AIndex]);
end;

function TResultadoMigracao.GetComportamentoCount: integer;
begin
  Result := FComportamentos.Count;
end;

function TResultadoMigracao.GetFeito(AIndex: integer): TItemFeito;
begin
  Result := TItemFeito(FFeitos[AIndex]);
end;

function TResultadoMigracao.GetFeitoCount: integer;
begin
  Result := FFeitos.Count;
end;

function TResultadoMigracao.GetManual(AIndex: integer): TItemManual;
begin
  Result := TItemManual(FManuais[AIndex]);
end;

function TResultadoMigracao.GetManualCount: integer;
begin
  Result := FManuais.Count;
end;

function TResultadoMigracao.AdicionarFeito(const ARegra, AArquivo: string;
  ALinha: integer; const ADescricao: string): TItemFeito;
begin
  Result := TItemFeito.Create;
  Result.Regra := ARegra;
  Result.Arquivo := AArquivo;
  Result.Linha := ALinha;
  Result.Descricao := ADescricao;
  FFeitos.Add(Result);
end;

function TResultadoMigracao.AdicionarManual(const ARegra, AArquivo: string;
  ALinha: integer; const AProblema, AFazer, ATrecho, AWiki: string): TItemManual;
begin
  Result := TItemManual.Create;
  Result.Regra := ARegra;
  Result.Arquivo := AArquivo;
  Result.Linha := ALinha;
  Result.Problema := AProblema;
  Result.Fazer := AFazer;
  Result.Trecho := ATrecho;
  Result.Wiki := AWiki;
  FManuais.Add(Result);
end;

function TResultadoMigracao.AdicionarComportamento(const AId, APasso,
  ADescricao, AFazer, AWiki: string): TItemComportamento;
var
  vInt: integer;
begin
  for vInt := 0 to FComportamentos.Count - 1 do
    if SameText(Comportamentos[vInt].Id, AId) then
      Exit(Comportamentos[vInt]);
  Result := TItemComportamento.Create;
  Result.Id := AId;
  Result.Passo := APasso;
  Result.Descricao := ADescricao;
  Result.Fazer := AFazer;
  Result.Wiki := AWiki;
  FComportamentos.Add(Result);
end;

{ TAcaoMigracao }

constructor TAcaoMigracao.Create;
begin
  inherited Create;
end;

end.
