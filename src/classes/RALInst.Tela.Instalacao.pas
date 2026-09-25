/// What the screens hand to the installation engines: the user's choice, and
/// the screen shell over each IDE instance (icon, log and the engine of its
/// kind).
unit RALInst.Tela.Instalacao;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, Forms,
  RALInst.Catalogo, RALInst.Compatibilidade, RALInst.Existente, RALInst.IDE,
  RALInst.Receitas;

type
  /// What the user chose on the features page; it holds for every checked IDE
  /// (the names are the catalog's: IndyRAL in Delphi, indyral in Lazarus).
  TEscolhaInstalacao = class
  private
    FCatalogo: TCatalogo;
    FDesinstalar: boolean;
    FManifesto: TManifesto;
    FPacotes: TStringList;
    FPastaFontes: string;
    FPastasDependencias: TStringList;
    FReceitas: TReceitas;
    FSomenteLibraryPath: boolean;
    FWin64: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    /// The chosen ones that exist in this IDE kind, with the catalog's name
    /// (IndyRAL holds for indyral in Lazarus, and what only exists on one side
    /// does not go to the other)
    procedure PacotesDoTipo(ATipo: TTipoPacote; ALista: TStrings);

    /// Owned by the features page
    property Catalogo: TCatalogo read FCatalogo write FCatalogo;
    /// No feature chosen: the run removes RAL from the IDEs that have it
    property Desinstalar: boolean read FDesinstalar write FDesinstalar;
    /// The chosen version manifest (owned by the features page)
    property Manifesto: TManifesto read FManifesto write FManifesto;
    property Pacotes: TStringList read FPacotes;
    /// Where the RAL sources are, or will be after the download
    property PastaFontes: string read FPastaFontes write FPastaFontes;
    /// Where each downloaded dependency is, or will be (name@version=folder)
    property PastasDependencias: TStringList read FPastasDependencias;
    /// The recipes (owned by the features page)
    property Receitas: TReceitas read FReceitas write FReceitas;
    /// Delphi: only the library path, compiling and installing nothing
    property SomenteLibraryPath: boolean read FSomenteLibraryPath
      write FSomenteLibraryPath;
    /// Delphi: also compiles the runtime and sets the Win64 library path
    property Win64: boolean read FWin64 write FWin64;
  end;

  /// Screen shell over an IDE instance: icon, log on the screen and the
  /// installation engine of each IDE kind. The instance belongs to the search
  /// list, not to this class.
  TIDETela = class
  private
    FExistente: TInstalacaoExistente;
    FIcon: TGraphic;
    FInstancia: TIDEInstance;
    FRecibos: integer;
    FLog: TMemo;
    FResumo: string;
    function GetBuildFile: string;
    function GetExeFile: string;
    function GetName: string;
    function GetVersion: string;
  protected
    /// Removes RAL from this IDE (receipts and what was installed by hand)
    function Desinstalar(AEscolha: TEscolhaInstalacao): boolean; virtual;
    /// Runs the installation in this IDE
    function Install(AEscolha: TEscolhaInstalacao): boolean; virtual;
    /// Destination of the core lines (TLogLinha)
    procedure LogarLinha(const ALinha: string);
    /// The line of the final report: what entered, what stayed out and why
    procedure Resumir(AOk: boolean; AAvisos: TStrings);
  public
    constructor Create(AInstancia: TIDEInstance); virtual;
    destructor Destroy; override;
    /// Where the dependency already is in this IDE ('' if it is not, or the
    /// recipe does not serve this IDE kind)
    function DependenciaInstalada(AReceita: TReceita;
      AEscolha: TEscolhaInstalacao): string; virtual;
    /// Reads what RAL the IDE already has; ACatalogo (may be nil) adds the
    /// names of the chosen version
    procedure DetectarExistente(ACatalogo: TCatalogo);
    /// Installs in this IDE, logging into ALog
    function InstallRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;
    /// Root of the installed copy of the dependency ('' if unknown)
    function PastaDependencia(AReceita: TReceita;
      AEscolha: TEscolhaInstalacao): string; virtual;
    /// What the installation will do in this IDE, without doing anything
    function Plano(AEscolha: TEscolhaInstalacao): string; virtual;
    /// One line on the RAL the IDE has ('' when it has none)
    function ResumoRAL: string;
    /// Has the IDE any RAL, by hand or by the installer?
    function TemRAL: boolean;

    property BuildFile: string read GetBuildFile;
    /// What RAL the IDE has, as of the last DetectarExistente
    property Existente: TInstalacaoExistente read FExistente;
    property ExeFile: string read GetExeFile;
    property Icon: TGraphic read FIcon;
    property Instancia: TIDEInstance read FInstancia;
    property Name: string read GetName;
    /// The summary of the last installation in this IDE (one or more lines)
    property Resumo: string read FResumo write FResumo;
    property Version: string read GetVersion;
  end;

  /// Class of an IDE shell.
  TIDETelaClass = class of TIDETela;

implementation

uses
  RALInst.Processo, RALInst.Recibos, RALInst.Tela.Mensagens, RALInst.Tela.Temas;

{ TEscolhaInstalacao }

constructor TEscolhaInstalacao.Create;
begin
  inherited Create;
  FPacotes := TStringList.Create;
  FPacotes.CaseSensitive := False;
  FPastasDependencias := TStringList.Create;
  FPastasDependencias.CaseSensitive := False;
end;

destructor TEscolhaInstalacao.Destroy;
begin
  FPastasDependencias.Free;
  FPacotes.Free;
  inherited Destroy;
end;

procedure TEscolhaInstalacao.PacotesDoTipo(ATipo: TTipoPacote; ALista: TStrings);
var
  vInt: integer;
  vPacote: TPacote;
begin
  ALista.Clear;
  for vInt := 0 to Pred(FPacotes.Count) do
  begin
    vPacote := nil;
    if FCatalogo <> nil then
      vPacote := FCatalogo.Buscar(ATipo, FPacotes[vInt]);
    if (vPacote <> nil) and (ALista.IndexOf(vPacote.Nome) < 0) then
      ALista.Add(vPacote.Nome);
  end;
end;

{ TIDETela }

constructor TIDETela.Create(AInstancia: TIDEInstance);
begin
  inherited Create;
  FInstancia := AInstancia;
  if FInstancia.ExeFile <> '' then
    FIcon := GetIconExeFile(FInstancia.ExeFile)
  else
    FIcon := GetIconExeFile(FInstancia.BuildFile);
  DetectarExistente(nil);
end;

destructor TIDETela.Destroy;
begin
  FreeAndNil(FExistente);
  FreeAndNil(FIcon);
  inherited Destroy;
end;

function TIDETela.DependenciaInstalada(AReceita: TReceita;
  AEscolha: TEscolhaInstalacao): string;
begin
  Result := '';
end;

function TIDETela.Desinstalar(AEscolha: TEscolhaInstalacao): boolean;
begin
  LogarLinha(Format(cmNaoSuportada, [Name]));
  Result := False;
end;

procedure TIDETela.DetectarExistente(ACatalogo: TCatalogo);
var
  vNomes: TStringList;
  vLista: TList;
  vRecibos: TRecibos;
  vTipo: TTipoPacote;
  vInt: integer;
begin
  FreeAndNil(FExistente);
  vNomes := TStringList.Create;
  vLista := TList.Create;
  vRecibos := TRecibos.Create(True);
  try
    vNomes.CaseSensitive := False;
    if FInstancia.Tipo = tiDelphi then
      vTipo := tpDelphi
    else
      vTipo := tpLazarus;
    if ACatalogo <> nil then
    begin
      ACatalogo.Listar(vTipo, vLista);
      for vInt := 0 to Pred(vLista.Count) do
        vNomes.Add(TPacote(vLista[vInt]).Nome);
    end;
    // o registro e a configuracao da IDE: leitura rapida, sem thread
    try
      {$IFDEF MSWINDOWS}
      if FInstancia.Tipo = tiDelphi then
        FExistente := DetectarDelphi(FInstancia, '', vNomes)
      else
      {$ENDIF}
        FExistente := DetectarLazarus(FInstancia, vNomes);
    except
      FExistente := nil;
    end;
    vRecibos.Carregar(PastaDadosInstalador + 'recibos');
    vRecibos.DaIDE(FInstancia.RootDir, vLista);
    FRecibos := vLista.Count;
  finally
    vRecibos.Free;
    vLista.Free;
    vNomes.Free;
  end;
end;
function TIDETela.GetBuildFile: string;
begin
  Result := FInstancia.BuildFile;
end;

function TIDETela.GetExeFile: string;
begin
  Result := FInstancia.ExeFile;
end;

function TIDETela.GetName: string;
begin
  Result := FInstancia.Nome;
end;

function TIDETela.GetVersion: string;
begin
  Result := FInstancia.Versao;
end;

function TIDETela.Install(AEscolha: TEscolhaInstalacao): boolean;
begin
  LogarLinha(Format(cmNaoSuportada, [Name]));
  Result := False;
end;

function TIDETela.InstallRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;
begin
  FLog := ALog;
  FResumo := '';
  try
    LogarLinha('==== ' + Name + ' (' + ExcludeTrailingPathDelimiter(FInstancia.RootDir) +
               ')');
    if AEscolha.Desinstalar then
      Result := Desinstalar(AEscolha)
    else
      Result := Install(AEscolha);
    // a pagina mostra o estado novo da IDE
    DetectarExistente(AEscolha.Catalogo);
    if FResumo = '' then
      Resumir(Result, nil);
    if Result then
      LogarLinha(Format(cmIDEConcluida, [Name]))
    else
      LogarLinha(Format(cmIDEComErro, [Name]));
    LogarLinha('');
  finally
    FLog := nil;
  end;
end;

procedure TIDETela.LogarLinha(const ALinha: string);
begin
  if FLog = nil then
    Exit;
  FLog.Lines.Add(ALinha);
  Application.ProcessMessages;
end;

function TIDETela.PastaDependencia(AReceita: TReceita;
  AEscolha: TEscolhaInstalacao): string;
begin
  Result := '';
end;

function TIDETela.Plano(AEscolha: TEscolhaInstalacao): string;
begin
  Result := Format(cmNaoSuportadaPlano, [Name]);
end;

function TIDETela.ResumoRAL: string;
begin
  Result := '';
  if (FExistente <> nil) and FExistente.Existe then
    Result := FExistente.Resumo
  else if FRecibos > 0 then
    Result := Format(cmRALPorRecibos, [FRecibos]);
end;

function TIDETela.TemRAL: boolean;
begin
  Result := ((FExistente <> nil) and FExistente.Existe) or (FRecibos > 0);
end;
procedure TIDETela.Resumir(AOk: boolean; AAvisos: TStrings);
var
  vInt: integer;
begin
  if AOk then
    FResumo := Format(cmResumoInstalado, [Name])
  else
    FResumo := Format(cmResumoComErro, [Name]);
  if AAvisos <> nil then
    for vInt := 0 to Pred(AAvisos.Count) do
      FResumo := FResumo + LineEnding + '    ' + AAvisos[vInt];
end;

end.
