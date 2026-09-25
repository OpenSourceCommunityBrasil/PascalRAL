/// Third page: the IDE installations found on the machine, to check. The
/// search runs in a background thread: the window keeps answering, and the
/// stop button cancels it.
unit RALInst.Tela.VersoesIDE;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  RALInst.IDE, RALInst.Tela.Instalacao, RALInst.Tela.ItemIDE, RALInst.Tela.Modelo,
  RALInst.Tela.Tarefa;

type
  /// How far a search goes.
  TModoBusca = (
    /// registry, known roots and the folders the user pointed before
    mbRapida,
    /// one folder the user chose (an IDE or a folder holding several)
    mbPasta,
    /// every drive (slow, on request)
    mbCompleta
  );

  /// IDE list page.
  TTelaVersoesIDE = class(TTelaModelo)
    bAddVersion: TSpeedButton;
    bAutoBusca: TSpeedButton;
    bStopBusca: TSpeedButton;
    dirSelect: TSelectDirectoryDialog;
    lbFind: TLabel;
    lbIDEListing: TLabel;
    lbSubTitle: TLabel;
    sbIDEVersions: TScrollBox;
    procedure bAddVersionClick(Sender: TObject);
    procedure bAutoBuscaClick(Sender: TObject);
    procedure bStopBuscaClick(Sender: TObject);
  private
    /// Written by the main thread, read by the search thread
    FCancelBusca: boolean;
    FContador: integer;
    /// Before an mbPasta search: the list size and whether the folder was known
    FContagemAntes: integer;
    FJaConhecida: boolean;
    /// The instances found belong to this list; the rows only show them
    FLista: TIDEList;
    FModo: TModoBusca;
    FPastaAtual: string;
    FPastaBusca: string;
    FSecao: TRTLCriticalSection;
    FTarefa: TTarefa;
    FTop: integer;
    /// Main thread: the search thread finished
    procedure BuscaTerminou(Sender: TObject);
    /// Search thread: runs the search of FModo into FLista
    procedure Buscar;
    /// Stops the running search, if any, and waits for it to end
    procedure CancelarEsperar;
    /// The searches of the chosen IDE kind: one, or both
    procedure CriarBuscas(ALista: TList);
    /// Starts a search in the background
    procedure IniciarBusca(AModo: TModoBusca; const APasta: string = '');
    /// Removes the rows
    procedure LimparVersoes;
    /// Main thread: shows the folder being searched
    procedure MostrarPasta;
    /// Rebuilds the rows from FLista
    procedure MostrarVersoes;
    /// Search thread: every folder visited
    procedure OnIDEFind(const APath: string; var ACancel: boolean);
    /// Is a search running?
    function Procurando: boolean;
  protected
    procedure SetIDE(AValue: integer); override;
    function ValidatePageNext: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AtualizarTextos; override;
    /// False when some checked IDE failed
    function InstallRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;
    /// The checked IDEs (TIDETela, owned by this page)
    procedure ListarMarcadas(ALista: TList);
    /// How many IDEs are checked
    function Marcadas: integer;
    /// The plan of every checked IDE, before running
    function Plano(AEscolha: TEscolhaInstalacao): string;
    /// The final report, one entry per checked IDE
    function Resumos: string;
  end;

implementation

{$R *.lfm}

uses
  {$IFDEF MSWINDOWS} RALInst.IDE.Delphi, RALInst.Tela.Delphi, {$ENDIF}
  RALInst.IDE.Lazarus, RALInst.Processo, RALInst.Tela.Imagens, RALInst.Tela.Lazarus,
  RALInst.Tela.Mensagens;

// as pastas que o usuario ja apontou com "adicionar pasta": entram na busca
// rapida das proximas vezes (IDE fora dos lugares de sempre, D:\IDE\lazarus)
function ArquivoPastas: string;
begin
  Result := PastaDadosInstalador + 'pastas-ide.txt';
end;

procedure LerPastas(ALista: TStrings);
begin
  ALista.Clear;
  if FileExists(ArquivoPastas) then
    try
      ALista.LoadFromFile(ArquivoPastas);
    except
      ALista.Clear;
    end;
end;

procedure LembrarPasta(const APasta: string);
var
  vLista: TStringList;
begin
  vLista := TStringList.Create;
  try
    vLista.CaseSensitive := False;
    LerPastas(vLista);
    if vLista.IndexOf(ExcludeTrailingPathDelimiter(APasta)) >= 0 then
      Exit;
    vLista.Add(ExcludeTrailingPathDelimiter(APasta));
    try
      ForceDirectories(ExtractFilePath(ArquivoPastas));
      vLista.SaveToFile(ArquivoPastas);
    except
      // lembrar e conveniencia: sem gravar, a busca so nao a acha sozinha
    end;
  finally
    vLista.Free;
  end;
end;

procedure LiberarBuscas(ALista: TList);
var
  vInt: integer;
begin
  for vInt := 0 to Pred(ALista.Count) do
    TObject(ALista[vInt]).Free;
  ALista.Clear;
end;

// '...\fim\do\caminho' quando nao cabe na linha
function CortarCaminho(const ACaminho: string; ALimite: integer): string;
var
  vPos: integer;
begin
  Result := ExcludeTrailingPathDelimiter(ACaminho);
  if Length(Result) <= ALimite then
    Exit;
  vPos := Length(Result) - ALimite + 3;
  while (vPos <= Length(Result)) and (Result[vPos] <> PathDelim) do
    Inc(vPos);
  Result := '...' + Copy(Result, vPos, MaxInt);
end;

{ TTelaVersoesIDE }

constructor TTelaVersoesIDE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitCriticalSection(FSecao);
  FLista := TIDEList.Create(True);
  lbFind.Caption := '';
end;

destructor TTelaVersoesIDE.Destroy;
begin
  CancelarEsperar;
  TThread.RemoveQueuedEvents(@MostrarPasta);
  LimparVersoes;
  FreeAndNil(FLista);
  DoneCriticalSection(FSecao);
  inherited Destroy;
end;

procedure TTelaVersoesIDE.bAddVersionClick(Sender: TObject);
begin
  if not dirSelect.Execute then
    Exit;
  // a pasta pode ser a propria IDE ou uma pasta com varias (D:\IDE\lazarus)
  FContagemAntes := FLista.Count;
  FJaConhecida := FLista.BuscarPorRaiz(dirSelect.FileName) <> nil;
  IniciarBusca(mbPasta, dirSelect.FileName);
end;

procedure TTelaVersoesIDE.bAutoBuscaClick(Sender: TObject);
begin
  // a busca rapida ja rodou ao abrir a tela; este botao varre os discos
  IniciarBusca(mbCompleta);
end;

procedure TTelaVersoesIDE.bStopBuscaClick(Sender: TObject);
begin
  FCancelBusca := True;
end;

procedure TTelaVersoesIDE.AtualizarTextos;
var
  vInt: integer;
begin
  inherited AtualizarTextos;
  if Procurando then
    lbFind.Caption := cmProcurandoIDEs;
  for vInt := 0 to Pred(sbIDEVersions.ControlCount) do
    if sbIDEVersions.Controls[vInt] is TItemIDE then
      TItemIDE(sbIDEVersions.Controls[vInt]).Atualizar;
end;

procedure TTelaVersoesIDE.BuscaTerminou(Sender: TObject);
var
  vErro: string;
begin
  vErro := TTarefa(Sender).Erro;
  Screen.Cursor := crDefault;
  bAutoBusca.Enabled := True;
  bAddVersion.Enabled := True;
  bStopBusca.Visible := False;
  lbFind.Caption := '';
  MostrarVersoes;

  if vErro <> '' then
    ShowMessage(Format(cmErroBusca, [vErro]))
  else if FModo = mbPasta then
  begin
    if (FLista.Count = FContagemAntes) and not FJaConhecida then
      ShowMessage(Format(cmNenhumaIDENaPasta, [FPastaBusca]))
    else
      LembrarPasta(FPastaBusca);
  end;
end;

procedure TTelaVersoesIDE.Buscar;
var
  vBuscas: TList;
  vPastas: TStringList;
  vInt: integer;
  vPasta: string;
begin
  vBuscas := TList.Create;
  vPastas := TStringList.Create;
  try
    CriarBuscas(vBuscas);
    for vInt := 0 to Pred(vBuscas.Count) do
    begin
      case FModo of
        mbRapida:
          begin
            // registro, raizes conhecidas e as pastas apontadas antes
            LerPastas(vPastas);
            TBuscaIDE(vBuscas[vInt]).BuscarPadrao(FLista);
            for vPasta in vPastas do
              if not FCancelBusca and DirectoryExists(vPasta) then
                TBuscaIDE(vBuscas[vInt]).BuscarEm(FLista, vPasta, 4);
          end;
        mbPasta:
          TBuscaIDE(vBuscas[vInt]).BuscarEm(FLista, FPastaBusca, 4);
        mbCompleta:
          // os discos inteiros so fazem sentido para o Lazarus: o Delphi e
          // achado pelo registro e pelas pastas ao lado das IDEs registradas
          if TObject(vBuscas[vInt]) is TBuscaLazarus then
            TBuscaLazarus(vBuscas[vInt]).BuscarCompleta(FLista);
      end;
      TBuscaIDE(vBuscas[vInt]).Finalizar(FLista);
    end;
  finally
    vPastas.Free;
    LiberarBuscas(vBuscas);
    vBuscas.Free;
  end;
end;

procedure TTelaVersoesIDE.CancelarEsperar;
begin
  if FTarefa = nil then
    Exit;
  FCancelBusca := True;
  // o WaitFor na thread principal atende o Synchronize do fim da busca
  FTarefa.WaitFor;
  FreeAndNil(FTarefa);
end;

procedure TTelaVersoesIDE.CriarBuscas(ALista: TList);
var
  vBusca: TBuscaIDE;
begin
  // 0 - Delphi, 1 - Lazarus, 2 - os dois
  ALista.Clear;
  {$IFDEF MSWINDOWS}
  if (IDE = 0) or (IDE = 2) then
  begin
    vBusca := TBuscaDelphi.Create;
    vBusca.OnBusca := @OnIDEFind;
    ALista.Add(vBusca);
  end;
  {$ENDIF}
  if (IDE = 1) or (IDE = 2) then
  begin
    vBusca := TBuscaLazarus.Create;
    vBusca.OnBusca := @OnIDEFind;
    ALista.Add(vBusca);
  end;
end;

procedure TTelaVersoesIDE.IniciarBusca(AModo: TModoBusca; const APasta: string);
begin
  CancelarEsperar;
  FModo := AModo;
  FPastaBusca := APasta;
  FCancelBusca := False;
  FContador := 0;
  bAutoBusca.Enabled := False;
  bAddVersion.Enabled := False;
  bStopBusca.Visible := True;
  lbFind.Caption := cmProcurandoIDEs;
  Screen.Cursor := crAppStart;
  FTarefa := TTarefa.Create(@Buscar, @BuscaTerminou);
end;

function TTelaVersoesIDE.InstallRAL(ALog: TMemo; AEscolha: TEscolhaInstalacao): boolean;
var
  vInt: integer;
begin
  // uma IDE que falha nao impede as outras: cada uma tem o seu relatorio
  Result := True;
  for vInt := 0 to Pred(sbIDEVersions.ControlCount) do
    if sbIDEVersions.Controls[vInt] is TItemIDE then
      if not TItemIDE(sbIDEVersions.Controls[vInt]).InstallRAL(ALog, AEscolha) then
        Result := False;
end;

procedure TTelaVersoesIDE.LimparVersoes;
var
  vInt: integer;
begin
  for vInt := Pred(sbIDEVersions.ControlCount) downto 0 do
    if sbIDEVersions.Controls[vInt] is TItemIDE then
      sbIDEVersions.Controls[vInt].Free;
  FTop := 0;
end;

procedure TTelaVersoesIDE.ListarMarcadas(ALista: TList);
var
  vInt: integer;
  vItem: TItemIDE;
begin
  ALista.Clear;
  for vInt := 0 to Pred(sbIDEVersions.ControlCount) do
    if sbIDEVersions.Controls[vInt] is TItemIDE then
    begin
      vItem := TItemIDE(sbIDEVersions.Controls[vInt]);
      if vItem.ckSelecionado.Checked then
        ALista.Add(vItem.ObjectData);
    end;
end;

function TTelaVersoesIDE.Marcadas: integer;
var
  vLista: TList;
begin
  vLista := TList.Create;
  try
    ListarMarcadas(vLista);
    Result := vLista.Count;
  finally
    vLista.Free;
  end;
end;

procedure TTelaVersoesIDE.MostrarPasta;
var
  vPasta: string;
begin
  EnterCriticalSection(FSecao);
  try
    vPasta := FPastaAtual;
  finally
    LeaveCriticalSection(FSecao);
  end;
  if Procurando then
    lbFind.Caption := CortarCaminho(vPasta, 90);
end;

procedure TTelaVersoesIDE.MostrarVersoes;
var
  vInt: integer;
  vDesmarcadas: TStringList;
  vItem: TItemIDE;
  vObj: TIDETela;
  vIDE: TIDEInstance;
begin
  // a lista inteira e redesenhada: uma IDE nova muda a ordem e os avisos
  // das outras. O que o usuario desmarcou continua desmarcado.
  vDesmarcadas := TStringList.Create;
  try
    for vInt := 0 to Pred(sbIDEVersions.ControlCount) do
      if sbIDEVersions.Controls[vInt] is TItemIDE then
      begin
        vItem := TItemIDE(sbIDEVersions.Controls[vInt]);
        if not vItem.ckSelecionado.Checked then
          vDesmarcadas.Add(vItem.ObjectData.Instancia.RootDir);
      end;

    sbIDEVersions.DisableAutoSizing;
    try
      LimparVersoes;
      for vInt := 0 to Pred(FLista.Count) do
      begin
        vIDE := FLista[vInt];
        {$IFDEF MSWINDOWS}
        if vIDE.Tipo = tiDelphi then
          vObj := TIDETelaDelphi.Create(vIDE)
        else
        {$ENDIF}
          vObj := TIDETelaLazarus.Create(vIDE);

        vItem := TItemIDE.Create(Self, vObj);
        vItem.Name := 'ItemIDE' + IntToStr(vInt);
        vItem.Parent := sbIDEVersions;
        vItem.Top := FTop;
        vItem.Align := alTop;
        if vDesmarcadas.IndexOf(vIDE.RootDir) >= 0 then
          vItem.ckSelecionado.Checked := False;
        FTop := FTop + vItem.Height;
      end;
    finally
      sbIDEVersions.EnableAutoSizing;
    end;
  finally
    vDesmarcadas.Free;
  end;
end;

procedure TTelaVersoesIDE.OnIDEFind(const APath: string; var ACancel: boolean);
begin
  ACancel := FCancelBusca;
  Inc(FContador);
  if FContador mod 200 <> 1 then
    Exit;
  EnterCriticalSection(FSecao);
  try
    FPastaAtual := APath;
  finally
    LeaveCriticalSection(FSecao);
  end;
  TThread.Queue(nil, @MostrarPasta);
end;

function TTelaVersoesIDE.Plano(AEscolha: TEscolhaInstalacao): string;
var
  vLista: TList;
  vInt: integer;
begin
  Result := '';
  vLista := TList.Create;
  try
    ListarMarcadas(vLista);
    for vInt := 0 to Pred(vLista.Count) do
      Result := Result + TIDETela(vLista[vInt]).Plano(AEscolha) + LineEnding;
  finally
    vLista.Free;
  end;
end;

function TTelaVersoesIDE.Procurando: boolean;
begin
  Result := (FTarefa <> nil) and not FTarefa.Finished;
end;

function TTelaVersoesIDE.Resumos: string;
var
  vLista: TList;
  vInt: integer;
begin
  Result := '';
  vLista := TList.Create;
  try
    ListarMarcadas(vLista);
    for vInt := 0 to Pred(vLista.Count) do
      if TIDETela(vLista[vInt]).Resumo <> '' then
        Result := Result + TIDETela(vLista[vInt]).Resumo + LineEnding;
  finally
    vLista.Free;
  end;
end;

procedure TTelaVersoesIDE.SetIDE(AValue: integer);
var
  vMudou: boolean;
begin
  // 0 - Delphi, 1 - Lazarus, 2 - os dois
  vMudou := IDE <> AValue;
  inherited SetIDE(AValue);

  bAddVersion.Visible := (AValue >= 0) and (AValue <= 2);
  bAutoBusca.Visible := (AValue = 1) or (AValue = 2);
  if not vMudou then
    Exit;

  CancelarEsperar;
  LimparVersoes;
  FLista.Clear;
  if (AValue >= 0) and (AValue <= 2) then
    IniciarBusca(mbRapida);
end;

function TTelaVersoesIDE.ValidatePageNext: boolean;
begin
  Result := False;
  if Procurando then
    ShowMessage(cmAguardeBusca)
  else if Marcadas = 0 then
    ShowMessage(cmMarqueUmaIDE)
  else
    Result := True;
end;

end.
