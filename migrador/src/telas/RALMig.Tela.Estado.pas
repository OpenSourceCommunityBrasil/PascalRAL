/// What the pages of the migrator window share (stage M8): the folder, the
/// versions, the options and the engine, plus the steps the window runs through
/// them (scan, detect, simulate with validation, apply with report, undo). No
/// LCL here, so the self-test and the pages use the same code.
unit RALMig.Tela.Estado;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALMig.API, RALMig.Tipos, RALMig.Motor, RALMig.Deteccao;

type
  /// The command line of the window.
  TArgumentosMig = record
    /// --autoteste: walks through the pages without interaction and exits
    AutoTeste: boolean;
    De: string;
    /// --gerar-po: writes the .po files and exits
    GerarPO: boolean;
    Idioma: string;
    Para: string;
    Pasta: string;
  end;

  { TEstadoMig }

  /// The migration being prepared by the window.
  TEstadoMig = class
  private
    FArquivosAchados: integer;
    FBinarios: TStringList;
    FConfianca: TConfianca;
    FConverterBinarios: boolean;
    FDe: string;
    FDeInformado: boolean;
    FDetectou: boolean;
    FEvidencias: TStringList;
    FFazerCopia: boolean;
    FForaProjetos: TStringList;
    FForaUnits: TStringList;
    FForcarDe: boolean;
    FGravado: boolean;
    FIdioma: string;
    FIgnorarErros: boolean;
    FIncluidos: TStringList;
    FMotor: TMotorMigracao;
    FPara: string;
    FParaInformado: boolean;
    FPasta: string;
    FProjetos: TStringList;
    FRelatorioHtml: string;
    FRelatorioMantido: boolean;
    FRelatorioMd: string;
    FResumo: string;
    FVersaoDetectada: string;
    procedure ConfigurarMotor;
    procedure Escanear;
    procedure GravarRelatorioDaRodada(AAlterados: integer);
    function ProjetoPrincipal(AArq: TArquivoMigrado): boolean;
    procedure Reiniciar;
    procedure SetPasta(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    /// Writes the files (with the safety copy when FazerCopia) and the report;
    /// False with the reason in AErro
    function Aplicar(out AErro: string): boolean;
    /// Changed files of the last Simular
    function ContarAlterados: integer;
    /// How many safety copies the folder has
    function ContarCopias: integer;
    /// Restores the newest safety copy; AMensagem says how it went
    function DesfazerUltima(out AMensagem: string): boolean;
    /// Detects the version of the folder (once per folder)
    procedure Detectar;
    /// The unified diff of a changed file
    function DiffDe(AArq: TArquivoMigrado): string;
    /// Marks (or unmarks) a project or folder outside the folder to come along
    procedure IncluirFora(const ACaminho: string; AIncluir: boolean);
    /// The folder the safety copies and the report live in (with a trailing
    /// delimiter)
    function PastaProjeto: string;
    /// Applies the chain in memory and validates the result against the API of
    /// the target (the items of the validation join the manual ones)
    function Simular: TResultadoMigracao;
    /// Is the target older than the source (D-M8)?
    function DestinoMaisVelho: boolean;

    /// Files of the folder (any kind) found by the last scan
    property ArquivosAchados: integer read FArquivosAchados;
    /// Binary forms found (relative paths)
    property Binarios: TStringList read FBinarios;
    property Confianca: TConfianca read FConfianca;
    property ConverterBinarios: boolean read FConverterBinarios write FConverterBinarios;
    property De: string read FDe write FDe;
    /// The source came from the command line
    property DeInformado: boolean read FDeInformado write FDeInformado;
    property Evidencias: TStringList read FEvidencias;
    /// Make the safety copy before writing (default True)
    property FazerCopia: boolean read FFazerCopia write FFazerCopia;
    /// Projects cited by a group but outside the folder (full paths)
    property ForaProjetos: TStringList read FForaProjetos;
    property ForaUnits: TStringList read FForaUnits;
    property ForcarDe: boolean read FForcarDe write FForcarDe;
    /// The files were written by Aplicar
    property Gravado: boolean read FGravado;
    property Idioma: string read FIdioma write FIdioma;
    property IgnorarErros: boolean read FIgnorarErros write FIgnorarErros;
    property Motor: TMotorMigracao read FMotor;
    property Para: string read FPara write FPara;
    property ParaInformado: boolean read FParaInformado write FParaInformado;
    /// Folder or project file being migrated; setting it starts over
    property Pasta: string read FPasta write SetPasta;
    /// Projects found (relative paths)
    property Projetos: TStringList read FProjetos;
    property RelatorioHtml: string read FRelatorioHtml;
    /// The report of an earlier run was kept (nothing changed)
    property RelatorioMantido: boolean read FRelatorioMantido;
    property RelatorioMd: string read FRelatorioMd;
    /// One line with the counts of the applied run
    property Resumo: string read FResumo;
    property VersaoDetectada: string read FVersaoDetectada;
  end;

/// Reads --de=, --para=, --idioma=, --pasta=, --gerar-po and --autoteste
function LerArgumentosMig: TArgumentosMig;
/// '1.1' of '1.1', 'v1.0', 'dev'...: the version as the combo boxes list it
function VersaoDeLista(const AVersao: string): string;

implementation

uses
  RALMig.Copia, RALMig.Formulario, RALMig.Mensagens, RALMig.Previa, RALMig.Projeto,
  RALMig.Regras, RALMig.Relatorio, RALMig.Texto, RALMig.Validacao,
  RALMig.Tela.Mensagens;

function LerArgumentosMig: TArgumentosMig;
var
  vInt: integer;
  vArg, vNome, vValor: string;
  vPos: integer;
begin
  Result.AutoTeste := False;
  Result.De := '';
  Result.GerarPO := False;
  Result.Idioma := '';
  Result.Para := '';
  Result.Pasta := '';
  for vInt := 1 to ParamCount do
  begin
    vArg := ParamStr(vInt);
    if Copy(vArg, 1, 2) <> '--' then
      Continue;
    Delete(vArg, 1, 2);
    vPos := Pos('=', vArg);
    vNome := vArg;
    vValor := '';
    if vPos > 0 then
    begin
      vNome := Copy(vArg, 1, vPos - 1);
      vValor := Copy(vArg, vPos + 1, MaxInt);
    end;
    vNome := LowerCase(vNome);
    if vNome = 'de' then
      Result.De := vValor
    else if vNome = 'para' then
      Result.Para := vValor
    else if vNome = 'idioma' then
      Result.Idioma := vValor
    else if vNome = 'pasta' then
      Result.Pasta := vValor
    else if vNome = 'gerar-po' then
      Result.GerarPO := True
    else if vNome = 'autoteste' then
      Result.AutoTeste := True;
  end;
end;

function VersaoDeLista(const AVersao: string): string;
var
  vInt: integer;
begin
  Result := '';
  if AVersao = '' then
    Exit;
  for vInt := Low(VersoesDeApi) to High(VersoesDeApi) do
    if NormalizarVersao(AVersao) = VersoesDeApi[vInt] then
      Exit(VersoesDeApi[vInt]);
end;

{ TEstadoMig }

constructor TEstadoMig.Create;
begin
  inherited Create;
  FBinarios := TStringList.Create;
  FEvidencias := TStringList.Create;
  FForaProjetos := TStringList.Create;
  FForaUnits := TStringList.Create;
  FIncluidos := TStringList.Create;
  FProjetos := TStringList.Create;
  FFazerCopia := True;
  FConverterBinarios := True;
end;

destructor TEstadoMig.Destroy;
begin
  FMotor.Free;
  FProjetos.Free;
  FIncluidos.Free;
  FForaUnits.Free;
  FForaProjetos.Free;
  FEvidencias.Free;
  FBinarios.Free;
  inherited Destroy;
end;

function TEstadoMig.Aplicar(out AErro: string): boolean;
var
  vAlterados: integer;
begin
  AErro := '';
  Result := False;
  if (FMotor = nil) or (FMotor.Conjunto = nil) then
    Exit;
  vAlterados := ContarAlterados;
  if not FMotor.Gravar(FFazerCopia) then
  begin
    AErro := FMotor.ErroGravacao;
    Exit;
  end;
  GravarRelatorioDaRodada(vAlterados);
  FGravado := True;
  Result := True;
end;

procedure TEstadoMig.ConfigurarMotor;
var
  vInt: integer;
begin
  FMotor.Pasta := FPasta;
  FMotor.De := FDe;
  FMotor.Para := FPara;
  FMotor.Idioma := FIdioma;
  FMotor.ConverterBinarios := FConverterBinarios;
  FMotor.ForcarDe := FForcarDe;
  for vInt := 0 to FIncluidos.Count - 1 do
    FMotor.Incluir(FIncluidos[vInt]);
end;

function TEstadoMig.ContarAlterados: integer;
var
  vLista: TList;
begin
  Result := 0;
  if (FMotor = nil) or (FMotor.Conjunto = nil) then
    Exit;
  vLista := TList.Create;
  try
    FMotor.Conjunto.ListarAlterados(vLista);
    Result := vLista.Count;
  finally
    vLista.Free;
  end;
end;

function TEstadoMig.ContarCopias: integer;
var
  vCopias: TStringList;
begin
  Result := 0;
  if FPasta = '' then
    Exit;
  vCopias := ListarCopias(PastaProjeto);
  try
    Result := vCopias.Count;
  finally
    vCopias.Free;
  end;
end;

function TEstadoMig.DesfazerUltima(out AMensagem: string): boolean;
var
  vCopias, vMudados: TStringList;
  vZip, vErro: string;
  vInt: integer;
begin
  Result := False;
  AMensagem := '';
  vCopias := ListarCopias(PastaProjeto);
  try
    if vCopias.Count = 0 then
    begin
      AMensagem := Format(emTelaSemCopia, [PastaProjeto]);
      Exit;
    end;
    vZip := vCopias[0];
  finally
    vCopias.Free;
  end;
  vMudados := TStringList.Create;
  try
    if not Desfazer(vZip, PastaProjeto, vMudados, vErro) then
    begin
      AMensagem := vErro;
      Exit;
    end;
    for vInt := 0 to vMudados.Count - 1 do
      AMensagem := AMensagem + Format(wmCopiaMudadoDepois, [vMudados[vInt]]) + LineEnding;
    AMensagem := AMensagem + Format(cmTelaDesfeito, [vZip]);
    FGravado := False;
    Result := True;
  finally
    vMudados.Free;
  end;
end;

function TEstadoMig.DestinoMaisVelho: boolean;
begin
  Result := (FDe <> '') and (FPara <> '') and
    (CompararVersoes(NormalizarVersao(FPara), NormalizarVersao(FDe)) < 0);
end;

procedure TEstadoMig.Detectar;
var
  vEvid: TStrings;
begin
  if FDetectou or (FMotor = nil) then
    Exit;
  FEvidencias.Clear;
  vEvid := nil;
  FVersaoDetectada := FMotor.Detectar(vEvid);
  try
    FEvidencias.Assign(vEvid);
  finally
    vEvid.Free;
  end;
  FConfianca := FMotor.Confianca;
  FDetectou := True;
end;

function TEstadoMig.DiffDe(AArq: TArquivoMigrado): string;
var
  vAntes: RawByteString;
begin
  vAntes := '';
  if FileExists(AArq.Caminho) then
    vAntes := LerBytes(AArq.Caminho);
  Result := DiffUnificado(AArq.Relativo, vAntes, AArq.Atual);
end;

procedure TEstadoMig.Escanear;
var
  vLoc: TLocalizadorArquivos;
  vLista: TList;
  vInt: integer;
  vArq: TArquivoMigrado;
begin
  FProjetos.Clear;
  FBinarios.Clear;
  FForaProjetos.Clear;
  FForaUnits.Clear;
  FArquivosAchados := 0;
  if (FPasta = '') or not (DirectoryExists(FPasta) or FileExists(FPasta)) then
    Exit;
  // sem converter: o formulario binario fica como ignorado, e e assim que se acha
  vLoc := TLocalizadorArquivos.Create(FPasta, False);
  try
    vLoc.Executar;
    FArquivosAchados := vLoc.Conjunto.Count;
    vLista := TList.Create;
    try
      vLoc.Conjunto.Listar([taDproj, taDpr, taLpi, taLpr, taDpk, taLpk, taGroupproj,
        taLpg], vLista);
      for vInt := 0 to vLista.Count - 1 do
        if ProjetoPrincipal(TArquivoMigrado(vLista[vInt])) then
          FProjetos.Add(TArquivoMigrado(vLista[vInt]).Relativo);
    finally
      vLista.Free;
    end;
    for vInt := 0 to vLoc.Conjunto.Count - 1 do
    begin
      vArq := vLoc.Conjunto[vInt];
      if vArq.Ignorado and (vArq.Tipo = taForm) and
         EhFormularioBinario(vArq.Texto.Bytes) then
        FBinarios.Add(vArq.Relativo);
    end;
    FForaProjetos.Assign(vLoc.ProjetosFora);
    FForaUnits.Assign(vLoc.UnitsFora);
  finally
    vLoc.Free;
  end;
end;

procedure TEstadoMig.GravarRelatorioDaRodada(AAlterados: integer);
var
  vDados: TDadosRelatorio;
  vErro: string;
begin
  FRelatorioMantido := False;
  vDados := DadosRelatorio(FMotor.Resultado, FMotor.Conjunto, FMotor.Conjunto.Pasta,
    FMotor.ZipCopia, True);
  FResumo := LinhaResumo(vDados);
  FRelatorioMd := FMotor.Conjunto.Pasta + NomeRelatorio(FMotor.Resultado.De,
    FMotor.Resultado.Para, 'md');
  FRelatorioHtml := FMotor.Conjunto.Pasta + NomeRelatorio(FMotor.Resultado.De,
    FMotor.Resultado.Para, 'html');
  // nada mudou: o relatorio da rodada que fez o trabalho continua valendo
  if (AAlterados = 0) and FileExists(FRelatorioMd) then
  begin
    FRelatorioMantido := True;
    Exit;
  end;
  if not GravarRelatorio(FMotor.Conjunto.Pasta, vDados, FRelatorioMd, FRelatorioHtml,
       vErro) then
  begin
    FRelatorioMd := '';
    FRelatorioHtml := '';
    FResumo := FResumo + LineEnding + Format(emTelaRelatorio, [vErro]);
  end;
end;

procedure TEstadoMig.IncluirFora(const ACaminho: string; AIncluir: boolean);
var
  vInd: integer;
begin
  vInd := FIncluidos.IndexOf(ACaminho);
  if AIncluir and (vInd < 0) then
    FIncluidos.Add(ACaminho)
  else if (not AIncluir) and (vInd >= 0) then
    FIncluidos.Delete(vInd);
end;

function TEstadoMig.PastaProjeto: string;
begin
  if DirectoryExists(FPasta) then
    Result := IncludeTrailingPathDelimiter(FPasta)
  else
    Result := ExtractFilePath(FPasta);
end;

function TEstadoMig.ProjetoPrincipal(AArq: TArquivoMigrado): boolean;
begin
  Result := True;
  // o .dpr/.lpr de um projeto que tem .dproj/.lpi e o mesmo projeto
  case AArq.Tipo of
    taDpr:
      Result := not FileExists(ChangeFileExt(AArq.Caminho, '.dproj'));
    taLpr:
      Result := not FileExists(ChangeFileExt(AArq.Caminho, '.lpi'));
  end;
end;

procedure TEstadoMig.Reiniciar;
begin
  FreeAndNil(FMotor);
  FDetectou := False;
  if not FDeInformado then
    FDe := '';
  if not FParaInformado then
    FPara := '';
  FVersaoDetectada := '';
  FConfianca := cfNenhuma;
  FEvidencias.Clear;
  FIncluidos.Clear;
  FGravado := False;
  FRelatorioHtml := '';
  FRelatorioMd := '';
  FResumo := '';
  FRelatorioMantido := False;
end;

procedure TEstadoMig.SetPasta(const AValue: string);
begin
  FPasta := AValue;
  Reiniciar;
  if FPasta = '' then
  begin
    Escanear;
    Exit;
  end;
  FMotor := TMotorMigracao.Create;
  FMotor.Pasta := FPasta;
  Escanear;
end;

function TEstadoMig.Simular: TResultadoMigracao;
var
  vApi: TApi;
begin
  ConfigurarMotor;
  Result := FMotor.Simular;
  if (FMotor.Conjunto = nil) or FMotor.Recusado then
    Exit;
  vApi := CarregarApiDaVersao(NormalizarVersao(FPara), FMotor.PastaApi);
  try
    // so o que o resultado em memoria promete: o que a IDE recusaria
    if vApi <> nil then
      ValidarResultado(FMotor.Conjunto, vApi, Result, FMotor.PastaApi);
  finally
    vApi.Free;
  end;
end;

end.
