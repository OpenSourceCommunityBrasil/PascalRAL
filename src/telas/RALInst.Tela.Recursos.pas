/// Fourth page: RAL version, installation folder and features. The version
/// comes from GitHub (the newest stable one comes chosen) and its catalog is
/// read from the version zip, kept in the cache: the options appear with
/// nothing written into the user's folder. The real download, into
/// <folder>/PascalRAL/<version>, only happens when installing. The last option
/// of the list is a local folder with the sources, for whoever develops RAL
/// itself: nothing is downloaded. Checking a package checks what it requires;
/// what enters only as a dependency shows as "required". A package no checked
/// IDE can take is not offered at all. Listing the versions and reading a
/// version zip run in a background thread.
unit RALInst.Tela.Recursos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, ComCtrls, Buttons, LCLType,
  ExtCtrls,
  RALInst.Catalogo, RALInst.Compatibilidade, RALInst.GitHub, RALInst.IDE,
  RALInst.Processo, RALInst.Receitas, RALInst.Tela.Instalacao, RALInst.Tela.Modelo,
  RALInst.Tela.Tarefa;

type
  /// The IDE page tells which IDEs are checked (TIDETela).
  TListarIDEs = procedure(ALista: TList) of object;

  /// Kinds of package in a run: Delphi, Lazarus or both.
  TTiposPacote = set of TTipoPacote;

  /// Where a package goes in the features tree, in this order.
  TCategoriaPacote = (cpBase, cpMotor, cpDBWare, cpSwagger, cpCompressao,
    cpStorage, cpOutros);

  /// What the background thread is loading.
  TCarga = (cgNada, cgVersoes, cgCatalogo);

  /// Features page.
  TTelaRecursos = class(TTelaModelo)
    bAddVersion: TSpeedButton;
    cbVersao: TComboBox;
    ckSomentePaths: TCheckBox;
    ckWin64: TCheckBox;
    dirSelect: TSelectDirectoryDialog;
    Label2: TLabel;
    lbDestino: TLabel;
    lbedDownloadPath: TLabeledEdit;
    lbDesmarcarTodos: TLabel;
    lbInfo: TLabel;
    lbMarcarTodos: TLabel;
    lbSomentePaths: TLabel;
    lbSubTitle: TLabel;
    lbVersao: TLabel;
    lbWin64: TLabel;
    tvRecursos: TTreeView;
    procedure bAddVersionClick(Sender: TObject);
    procedure cbVersaoChange(Sender: TObject);
    procedure lbDesmarcarTodosClick(Sender: TObject);
    procedure lbedDownloadPathEditingDone(Sender: TObject);
    procedure lbMarcarTodosClick(Sender: TObject);
    procedure lbSomentePathsClick(Sender: TObject);
    procedure lbWin64Click(Sender: TObject);
    procedure tvRecursosDblClick(Sender: TObject);
    procedure tvRecursosKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    /// What the background thread is loading
    FCarga: TCarga;
    FCarregado: string;
    /// The catalog of the screens: from the version zip, or the local folder
    FCatalogo: TCatalogo;
    /// The one of the final folder, after the download: it is what installs
    FCatalogoInstalar: TCatalogo;
    /// Catalog read by the background thread, swapped in at the end
    FCatalogoLido: TCatalogo;
    FChaveCarga: string;
    FCompat: TCompatibilidade;
    FDependencias: TStringList;
    /// ide root|recipe=where it already is ('' if it is not): asking the
    /// registry at every tree drawing would be slow
    FDeteccoes: TStringList;
    FErroCarga: string;
    FErroCatalogo: string;
    FEscolha: TEscolhaInstalacao;
    /// What the user checked; the dependencies are worked out at each drawing
    FEscolhidos: TStringList;
    FIDEsVistas: string;
    /// Package=IDEs that already have it (with what it requires)
    FInstalados: TStringList;
    /// The packages the checked IDEs have installed, by name
    FInstaladosDiretos: TStringList;
    /// The chosen version manifest; reloaded with the version
    FManifesto: TManifesto;
    FModoLocal: boolean;
    FOnListarIDEs: TListarIDEs;
    FPastaBase: string;
    FPastaLocal: string;
    /// Progress text of the download, written by the thread
    FProgresso: string;
    FRamoCarga: boolean;
    FReceitas: TReceitas;
    FRefCarga: string;
    FRepo: TRepoGitHub;
    FSecao: TRTLCriticalSection;
    FTarefa: TTarefa;
    FUltimoProgresso: int64;
    FVersoes: TVersoesRAL;
    FVersoesCarregadas: boolean;
    FVersoesLidas: TVersoesRAL;
    /// Checks/unchecks a package (and tells who keeps it checked)
    procedure Alternar(APacote: TPacote; AMarcar: boolean);
    /// Identity of the checked IDEs, to notice a change
    function AssinaturaIDEs: string;
    /// The destination line under the folder
    procedure AtualizarDestino;
    /// Delphi options only for Delphi runs
    procedure AtualizarOpcoes;
    /// Waits for a running load to end
    procedure CancelarEsperar;
    /// Main thread: the background load finished
    procedure CargaTerminou(Sender: TObject);
    /// Is a background load running?
    function Carregando: boolean;
    /// Background thread: lists the versions or reads a version zip
    procedure Carregar;
    /// Loads the catalog of the chosen version (thread) or local folder
    procedure CarregarCatalogo;
    /// Lists the versions (thread)
    procedure CarregarVersoes;
    /// The compatibility check, created on first use
    function Compat: TCompatibilidade;
    /// Where the dependency already is in the IDE (cached)
    function Detectar(AIDE: TIDEInstance; AReceita: TReceita): string;
    /// Reads the RAL the checked IDEs already have, with this version's names
    procedure DetectarRAL;
    /// Checks what the IDEs already have, else RAL's core
    procedure EscolherPadrao;
    /// Is some checked IDE carrying RAL? (the names go in ANomes)
    function IDEsComRAL(ANomes: TStrings): boolean;
    /// '' when it fits every checked IDE; else 'Delphi XE2: reason' per IDE
    /// (ATodas says whether it fits none)
    function ForaDasIDEs(APacote: TPacote; out ATodas: boolean): string;
    /// The checked IDEs of one kind
    procedure IDEsDoTipo(ALista: TList; ATipo: TTipoPacote);
    /// Every checked IDE
    procedure IDEsMarcadas(ALista: TList);
    /// Why the package cannot be checked in the IDEs of its kind ('' if it can)
    function Indisponivel(APacote: TPacote): string;
    /// The reason when it cannot be checked in any kind of the run
    function IndisponivelEmTodos(APacote: TPacote): string;
    /// Builds the features tree
    procedure MontarArvore;
    /// Main thread: shows the download progress
    procedure MostrarProgresso;
    /// The chosen ones that exist in a kind, with the catalog's name
    procedure NomesDoTipo(ATipo: TTipoPacote; ALista: TStrings);
    /// The same feature in the other IDE kind; nil when there is none, or the
    /// run has a single kind
    function Par(APacote: TPacote): TPacote;
    /// Root of the installed copy of the dependency in the IDE
    function PastaInstalada(AIDE: TIDEInstance; AReceita: TReceita): string;
    /// Download progress (called by the thread)
    procedure Progresso(const ALidos, ATotal: int64);
    /// Submodules the chosen packages use, of every kind of the run
    procedure SubmodulosDaEscolha(ALista: TStrings);
    /// The kinds of IDE of the run
    function Tipos: TTiposPacote;
    /// Local folder or GitHub version: labels and catalog
    procedure TrocarModo;
    /// TrocarModo after the versions load has fully ended
    procedure TrocarModoAsync(AData: PtrInt);
    /// The chosen version; nil in local mode
    function Versao: TVersaoRAL;
  protected
    procedure SetIDE(AValue: integer); override;
    function ValidatePageNext: boolean; override;
    function ValidatePagePrior: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AoMostrar; override;
    procedure AtualizarTextos; override;
    /// Stage 1b: downloads what was planned; what fails leaves the choice, and
    /// the packages depending on it stay out at installation
    function BaixarDependencias(ALog: TLogLinha): boolean;
    /// The complete choice, for the installation engines
    function Escolha: TEscolhaInstalacao;
    /// Where the sources are, or will be after the download
    function PastaFontes: string;
    /// Decides what to download (only what some checked IDE lacks, in the
    /// version each one asks) and returns that part of the plan; the IDEs
    /// learn where each one goes
    function PlanejarDependencias: string;
    /// The part of the plan telling where RAL comes from
    function PlanoFontes: string;
    /// Stage 1 of the run: the sources in the final folder (downloads if
    /// needed); no IDE is touched when it fails
    function PrepararFontes(ALog: TLogLinha): boolean;

    property OnListarIDEs: TListarIDEs read FOnListarIDEs write FOnListarIDEs;
  published
    property Catalogo: TCatalogo read FCatalogo;
  end;

/// Where a package goes in the features tree
function CategoriaDoPacote(APacote: TPacote): TCategoriaPacote;

implementation

{$R *.lfm}

uses
  StrUtils,
  RALInst.Dependencias, RALInst.Fontes, RALInst.Mensagens, RALInst.Tela.Imagens,
  RALInst.Tela.Mensagens,
  RALInst.Zip;

const
  ImgDesmarcado = 4;
  ImgMarcado = 3;

function CategoriaDoPacote(APacote: TPacote): TCategoriaPacote;
var
  vRaiz, vNome: string;
begin
  vNome := LowerCase(APacote.Nome);
  vRaiz := LowerCase(APacote.Grupo);
  if Pos('/', vRaiz) > 0 then
    vRaiz := Copy(vRaiz, 1, Pos('/', vRaiz) - 1);
  if Pos('swagger', vNome) > 0 then
    Result := cpSwagger
  else if vRaiz = '' then
    Result := cpBase
  else if vRaiz = 'engine' then
    Result := cpMotor
  else if vRaiz = 'compression' then
    Result := cpCompressao
  else if (Pos('bson', vNome) > 0) or (Pos('storage', vNome) > 0) then
    Result := cpStorage
  else if vRaiz = 'database' then
    Result := cpDBWare
  else
    Result := cpOutros;
end;

function NomeCategoria(ACategoria: TCategoriaPacote): string;
begin
  case ACategoria of
    cpBase:       Result := cmGrupoBase;
    cpMotor:      Result := cmGrupoMotores;
    cpDBWare:     Result := cmGrupoDBWare;
    cpSwagger:    Result := cmGrupoSwagger;
    cpCompressao: Result := cmGrupoCompressao;
    cpStorage:    Result := cmGrupoStorage;
  else
    Result := cmGrupoOutros;
  end;
end;

var
  // quantos pacotes exigem cada um (nome=total), durante a ordenacao da arvore
  GDependentes: TStringList;

// base, motores, DBWare, Swagger, compressao, storage; dentro de cada grupo,
// primeiro o que os outros exigem (o RALDBPackage antes dos links), depois a
// ordem de instalacao (o PascalRAL antes do PascalRALDsgn) e o nome
function CompararNaArvore(AItem1, AItem2: Pointer): integer;
var
  vA, vB: TPacote;
begin
  vA := TPacote(AItem1);
  vB := TPacote(AItem2);
  Result := Ord(CategoriaDoPacote(vA)) - Ord(CategoriaDoPacote(vB));
  if (Result = 0) and (GDependentes <> nil) then
    Result := StrToIntDef(GDependentes.Values[vB.Nome], 0) -
              StrToIntDef(GDependentes.Values[vA.Nome], 0);
  if Result = 0 then
    Result := vA.Ordem - vB.Ordem;
  if Result = 0 then
    Result := CompareText(vA.Nome, vB.Nome);
end;

{ TTelaRecursos }

constructor TTelaRecursos.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitCriticalSection(FSecao);
  FRepo := TRepoGitHub.Create(DonoRAL, RepoRAL);
  FRepo.OnProgresso := @Progresso;
  FVersoes := TVersoesRAL.Create(True);
  FCatalogo := TCatalogo.Create;
  FEscolhidos := TStringList.Create;
  FEscolhidos.CaseSensitive := False;
  FEscolha := TEscolhaInstalacao.Create;
  FPastaBase := IncludeTrailingPathDelimiter(GetUserDir) + 'RAL';
  FDependencias := TStringList.Create;
  FDependencias.CaseSensitive := False;
  // as receitas vem embutidas no executavel; uma pasta 'receitas' ao lado dele
  // ou na pasta de dados acrescenta ou substitui, sem recompilar
  FReceitas := TReceitas.Create;
  FReceitas.CarregarPadrao;
  FManifesto := TManifesto.Create;
  FDeteccoes := TStringList.Create;
  FInstalados := TStringList.Create;
  FInstalados.CaseSensitive := False;
  FInstaladosDiretos := TStringList.Create;
  FInstaladosDiretos.CaseSensitive := False;
  lbInfo.Caption := '';
  AtualizarOpcoes;
end;

destructor TTelaRecursos.Destroy;
begin
  CancelarEsperar;
  TThread.RemoveQueuedEvents(@MostrarProgresso);
  FreeAndNil(FCompat);
  FreeAndNil(FDeteccoes);
  FreeAndNil(FInstaladosDiretos);
  FreeAndNil(FInstalados);
  FreeAndNil(FManifesto);
  FreeAndNil(FReceitas);
  FreeAndNil(FDependencias);
  FreeAndNil(FEscolha);
  FreeAndNil(FEscolhidos);
  FreeAndNil(FCatalogoInstalar);
  FreeAndNil(FCatalogoLido);
  FreeAndNil(FCatalogo);
  FreeAndNil(FVersoesLidas);
  FreeAndNil(FVersoes);
  FreeAndNil(FRepo);
  DoneCriticalSection(FSecao);
  inherited Destroy;
end;

procedure TTelaRecursos.bAddVersionClick(Sender: TObject);
begin
  if DirectoryExists(lbedDownloadPath.Text) then
    dirSelect.InitialDir := lbedDownloadPath.Text;
  if dirSelect.Execute then
  begin
    lbedDownloadPath.Text := dirSelect.FileName;
    lbedDownloadPathEditingDone(nil);
  end;
end;

procedure TTelaRecursos.cbVersaoChange(Sender: TObject);
begin
  TrocarModo;
end;

procedure TTelaRecursos.lbedDownloadPathEditingDone(Sender: TObject);
begin
  if FModoLocal then
  begin
    FPastaLocal := Trim(lbedDownloadPath.Text);
    CarregarCatalogo;
  end
  else
  begin
    FPastaBase := Trim(lbedDownloadPath.Text);
    FreeAndNil(FCatalogoInstalar);
  end;
  AtualizarDestino;
end;

procedure TTelaRecursos.lbMarcarTodosClick(Sender: TObject);
var
  vLista: TList;
  vInt: integer;
  vPacote: TPacote;
  vTipo: TTipoPacote;
begin
  if Carregando or (FCatalogo.Count = 0) then
    Exit;
  // tudo o que a arvore oferece: o que nenhuma IDE aceita nem aparece
  vLista := TList.Create;
  try
    for vTipo in Tipos do
    begin
      FCatalogo.Listar(vTipo, vLista);
      for vInt := 0 to Pred(vLista.Count) do
      begin
        vPacote := TPacote(vLista[vInt]);
        if (IndisponivelEmTodos(vPacote) = '') and
           (FEscolhidos.IndexOf(vPacote.Nome) < 0) then
          FEscolhidos.Add(vPacote.Nome);
      end;
    end;
  finally
    vLista.Free;
  end;
  MontarArvore;
end;

procedure TTelaRecursos.lbDesmarcarTodosClick(Sender: TObject);
begin
  if Carregando or (FCatalogo.Count = 0) then
    Exit;
  // nada marcado: com o RAL ja instalado, a rodada desinstala
  FEscolhidos.Clear;
  MontarArvore;
end;

procedure TTelaRecursos.lbSomentePathsClick(Sender: TObject);
begin
  ckSomentePaths.Checked := not ckSomentePaths.Checked;
end;

procedure TTelaRecursos.lbWin64Click(Sender: TObject);
begin
  ckWin64.Checked := not ckWin64.Checked;
end;

procedure TTelaRecursos.tvRecursosDblClick(Sender: TObject);
var
  vNode: TTreeNode;
begin
  vNode := tvRecursos.Selected;
  if (vNode = nil) or (vNode.Data = nil) then
    Exit;
  Alternar(TPacote(vNode.Data), vNode.ImageIndex <> ImgMarcado);
end;

procedure TTelaRecursos.tvRecursosKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  vNode: TTreeNode;
begin
  vNode := tvRecursos.Selected;
  if (vNode = nil) or (vNode.Data = nil) then
    Exit;
  case Key of
    VK_INSERT: Alternar(TPacote(vNode.Data), True);
    VK_DELETE: Alternar(TPacote(vNode.Data), False);
    VK_SPACE:  Alternar(TPacote(vNode.Data), vNode.ImageIndex <> ImgMarcado);
  else
    Exit;
  end;
  Key := 0;
end;

procedure TTelaRecursos.Alternar(APacote: TPacote; AMarcar: boolean);
var
  vFechamento: TList;
  vNomes: TStringList;
  vInt, vDep: integer;
  vQuem: string;
  vTipo: TTipoPacote;
begin
  if AMarcar then
  begin
    if FEscolhidos.IndexOf(APacote.Nome) < 0 then
      FEscolhidos.Add(APacote.Nome);
  end
  else
  begin
    vInt := FEscolhidos.IndexOf(APacote.Nome);
    if vInt >= 0 then
      FEscolhidos.Delete(vInt);

    // continua marcado se outro escolhido depende dele: dizer quem
    vFechamento := TList.Create;
    vNomes := TStringList.Create;
    try
      vQuem := '';
      for vTipo in Tipos do
      begin
        NomesDoTipo(vTipo, vNomes);
        FCatalogo.Fechamento(vTipo, vNomes, vFechamento);
        for vInt := 0 to Pred(vFechamento.Count) do
          for vDep := 0 to Pred(TPacote(vFechamento[vInt]).Internos.Count) do
            if SameText(TPacote(vFechamento[vInt]).Internos[vDep], APacote.Nome) and
               (Pos(' ' + TPacote(vFechamento[vInt]).Nome, vQuem) = 0) then
              vQuem := vQuem + ' ' + TPacote(vFechamento[vInt]).Nome;
      end;
      if vQuem <> '' then
        ShowMessage(Format(cmContinuaMarcado, [APacote.Nome, Trim(vQuem)]));
    finally
      vNomes.Free;
      vFechamento.Free;
    end;
  end;
  MontarArvore;
end;

procedure TTelaRecursos.AoMostrar;
begin
  if not FVersoesCarregadas then
    CarregarVersoes;
  // voltou da tela das IDEs com outra escolha: o que cabe em cada uma muda
  if AssinaturaIDEs <> FIDEsVistas then
  begin
    FIDEsVistas := AssinaturaIDEs;
    FreeAndNil(FCompat);
    FDeteccoes.Clear;
    DetectarRAL;
    if FCatalogo.Count > 0 then
      MontarArvore;
  end;
end;

function TTelaRecursos.AssinaturaIDEs: string;
var
  vLista: TList;
  vInt: integer;
begin
  Result := '';
  vLista := TList.Create;
  try
    IDEsMarcadas(vLista);
    for vInt := 0 to Pred(vLista.Count) do
      Result := Result + TIDETela(vLista[vInt]).Instancia.RootDir + ';';
  finally
    vLista.Free;
  end;
end;

procedure TTelaRecursos.AtualizarDestino;
begin
  if FModoLocal then
    lbDestino.Caption := cmDestinoLocal
  else if PastaFontes <> '' then
    lbDestino.Caption := Format(cmDestinoPasta,
                                [ExcludeTrailingPathDelimiter(PastaFontes)])
  else
    lbDestino.Caption := cmDestinoEscolha;
  lbDestino.Hint := lbDestino.Caption;
  lbDestino.ShowHint := True;
end;

procedure TTelaRecursos.AtualizarOpcoes;
var
  vDelphi: boolean;
begin
  // as duas opcoes so existem no Delphi
  {$IFDEF MSWINDOWS}
  vDelphi := (IDE = 0) or (IDE = 2);
  {$ELSE}
  vDelphi := False;
  {$ENDIF}
  ckSomentePaths.Visible := vDelphi;
  lbSomentePaths.Visible := vDelphi;
  ckWin64.Visible := vDelphi;
  lbWin64.Visible := vDelphi;
end;

procedure TTelaRecursos.AtualizarTextos;
var
  vInt, vIndice: integer;
begin
  inherited AtualizarTextos;
  // a lista de versoes e o que depende do modo foram escritos em codigo
  vIndice := cbVersao.ItemIndex;
  for vInt := 0 to Pred(cbVersao.Items.Count) do
    if cbVersao.Items.Objects[vInt] <> nil then
      cbVersao.Items[vInt] := TVersaoRAL(cbVersao.Items.Objects[vInt]).Descricao
    else
      cbVersao.Items[vInt] := cmPastaLocalFontes;
  cbVersao.ItemIndex := vIndice;
  if FModoLocal then
    lbedDownloadPath.EditLabel.Caption := cmPastaFontesLocal
  else
    lbedDownloadPath.EditLabel.Caption := cmPastaInstalacao;
  AtualizarDestino;
  if Carregando then
    lbInfo.Caption := cmCarregando
  else if FCatalogo.Count > 0 then
    MontarArvore
  else
    lbInfo.Caption := FErroCatalogo;
end;

function TTelaRecursos.BaixarDependencias(ALog: TLogLinha): boolean;
var
  vInt: integer;
  vReceita: TReceita;
  vBaixa: TBaixaDependencia;
  vChave: string;
begin
  Result := True;
  for vInt := Pred(FDependencias.Count) downto 0 do
  begin
    // nome@versao: a mesma dependencia pode vir em duas versoes
    vChave := FDependencias.Names[vInt];
    vReceita := FReceitas.Buscar(Copy(vChave, 1, Pos('@', vChave) - 1));
    if vReceita = nil then
      Continue;
    vBaixa := TBaixaDependencia.Create(vReceita);
    try
      vBaixa.PastaBase := FPastaBase;
      vBaixa.VersaoPedida := Copy(vChave, Pos('@', vChave) + 1, MaxInt);
      vBaixa.Log := ALog;
      if not vBaixa.Executar then
      begin
        ALog(cmPrefixoErro + vBaixa.Erro);
        FDependencias.Delete(vInt);
        Result := False;
      end;
    finally
      vBaixa.Free;
    end;
  end;
  FEscolha.PastasDependencias.Assign(FDependencias);
end;

procedure TTelaRecursos.CancelarEsperar;
begin
  if FTarefa = nil then
    Exit;
  // o WaitFor na thread principal atende o Synchronize do fim da carga
  FTarefa.WaitFor;
  FreeAndNil(FTarefa);
end;

procedure TTelaRecursos.Carregar;
var
  vZip: string;
begin
  // thread: nada de tela aqui
  FErroCarga := '';
  FUltimoProgresso := 0;
  if FCarga = cgVersoes then
  begin
    if not FRepo.ListarVersoes(FVersoesLidas) then
      FErroCarga := FRepo.Erro;
    Exit;
  end;
  // o zip da versao vai para o cache; o ramo muda, a tag nao
  if not FRepo.BaixarZip(FRefCarga, FRamoCarga, vZip) then
  begin
    FErroCarga := FRepo.Erro;
    Exit;
  end;
  try
    if not FCatalogoLido.Carregar(TOrigemZip.Create(vZip, 'PascalRAL ' + FRefCarga)) then
      FErroCarga := Format(emVersaoSemPacotes, [FRefCarga]);
  except
    on E: Exception do
      FErroCarga := Format(emLerVersao, [FRefCarga, E.Message]);
  end;
end;

function TTelaRecursos.Carregando: boolean;
begin
  Result := (FTarefa <> nil) and not FTarefa.Finished;
end;

procedure TTelaRecursos.CarregarCatalogo;
var
  vChave, vPasta: string;
  vVersao: TVersaoRAL;
begin
  vVersao := Versao;
  if FModoLocal then
    vChave := 'local:' + Trim(FPastaLocal)
  else if vVersao <> nil then
    vChave := 'github:' + vVersao.Ref
  else
    vChave := '';
  if SameText(vChave, FCarregado) or (Carregando and SameText(vChave, FChaveCarga)) then
    Exit;
  CancelarEsperar;
  FCarregado := '';
  FErroCatalogo := '';
  FreeAndNil(FCatalogoInstalar);
  FreeAndNil(FCompat);
  FManifesto.Limpar;
  FCatalogo.Limpar;
  tvRecursos.Items.Clear;

  if FModoLocal then
  begin
    // pasta local: ler o disco e rapido
    vPasta := Trim(FPastaLocal);
    FCarregado := vChave;
    if vPasta = '' then
      FErroCatalogo := cmEscolhaPastaFontes
    else if not DirectoryExists(vPasta) then
      FErroCatalogo := cmPastaNaoEncontrada
    else if not FCatalogo.Carregar(vPasta) then
      FErroCatalogo := cmPastaSemPacotes;
    if FErroCatalogo <> '' then
    begin
      FCatalogo.Limpar;
      lbInfo.Caption := FErroCatalogo;
      Exit;
    end;
    FManifesto.CarregarPadrao(FCatalogo.Origem);
    if FEscolhidos.Count = 0 then
      EscolherPadrao;
    MontarArvore;
    Exit;
  end;
  if vVersao = nil then
    Exit;

  // versao do GitHub: o zip e baixado e lido fora da thread principal
  FChaveCarga := vChave;
  FRefCarga := vVersao.Ref;
  FRamoCarga := vVersao.Tipo = tvRamo;
  FreeAndNil(FCatalogoLido);
  FCatalogoLido := TCatalogo.Create;
  FCarga := cgCatalogo;
  lbInfo.Caption := Format(cmLendoVersao, [vVersao.Ref]);
  cbVersao.Enabled := False;
  Screen.Cursor := crAppStart;
  FTarefa := TTarefa.Create(@Carregar, @CargaTerminou);
end;

procedure TTelaRecursos.CargaTerminou(Sender: TObject);
var
  vInt: integer;
  vAntes: TStringList;
begin
  Screen.Cursor := crDefault;
  cbVersao.Enabled := True;
  if FCarga = cgVersoes then
  begin
    FCarga := cgNada;
    // as versoes lidas passam a ser as da tela
    FreeAndNil(FVersoes);
    FVersoes := FVersoesLidas;
    FVersoesLidas := nil;
    cbVersao.Items.Clear;
    for vInt := 0 to Pred(FVersoes.Count) do
      cbVersao.Items.AddObject(FVersoes[vInt].Descricao, FVersoes[vInt]);
    cbVersao.Items.AddObject(cmPastaLocalFontes, nil);
    if FVersoes.Count = 0 then
    begin
      // sem rede e sem cache: sobra a pasta local
      cbVersao.ItemIndex := cbVersao.Items.Count - 1;
      lbInfo.Caption := '';
      ShowMessage(Format(cmSemVersoes, [FErroCarga]));
    end
    else
    begin
      // a primeira e a estavel mais recente
      cbVersao.ItemIndex := 0;
      if FRepo.Aviso <> '' then
        lbInfo.Caption := FRepo.Aviso;
    end;
    // a tarefa ainda esta terminando: o catalogo sai na proxima volta
    Application.QueueAsyncCall(@TrocarModoAsync, 0);
    Exit;
  end;

  FCarga := cgNada;
  if FErroCarga <> '' then
  begin
    FErroCatalogo := FErroCarga;
    FreeAndNil(FCatalogoLido);
    lbInfo.Caption := FErroCatalogo;
    Exit;
  end;
  FreeAndNil(FCatalogo);
  FCatalogo := FCatalogoLido;
  FCatalogoLido := nil;
  FCarregado := FChaveCarga;
  // o manifesto e o desta versao (ralinstaller.json nela), senao o embutido
  FManifesto.CarregarPadrao(FCatalogo.Origem);

  // o que o usuario tinha marcado sobrevive a troca de versao, no que existir
  vAntes := TStringList.Create;
  try
    vAntes.Assign(FEscolhidos);
    FEscolhidos.Clear;
    for vInt := 0 to Pred(vAntes.Count) do
      if ((tpDelphi in Tipos) and (FCatalogo.Buscar(tpDelphi, vAntes[vInt]) <> nil)) or
         ((tpLazarus in Tipos) and
          (FCatalogo.Buscar(tpLazarus, vAntes[vInt]) <> nil)) then
        FEscolhidos.Add(vAntes[vInt]);
  finally
    vAntes.Free;
  end;
  if FEscolhidos.Count = 0 then
    EscolherPadrao;
  MontarArvore;
end;

procedure TTelaRecursos.CarregarVersoes;
begin
  FVersoesCarregadas := True;
  CancelarEsperar;
  cbVersao.Items.Clear;
  FreeAndNil(FVersoesLidas);
  FVersoesLidas := TVersoesRAL.Create(True);
  FCarga := cgVersoes;
  lbInfo.Caption := cmConsultandoVersoes;
  cbVersao.Enabled := False;
  Screen.Cursor := crAppStart;
  FTarefa := TTarefa.Create(@Carregar, @CargaTerminou);
end;

function TTelaRecursos.Compat: TCompatibilidade;
begin
  if FCompat = nil then
  begin
    FCompat := TCompatibilidade.Create(FCatalogo, FManifesto, FReceitas);
    FCompat.Detectar := @Detectar;
    FCompat.PastaInstalada := @PastaInstalada;
  end;
  Result := FCompat;
end;

function TTelaRecursos.Detectar(AIDE: TIDEInstance; AReceita: TReceita): string;
var
  vLista: TList;
  vInt, vIdx: integer;
  vChave: string;
begin
  vChave := AIDE.RootDir + '|' + AReceita.Nome;
  vIdx := FDeteccoes.IndexOfName(vChave);
  if vIdx >= 0 then
    Exit(FDeteccoes.ValueFromIndex[vIdx]);
  Result := '';
  vLista := TList.Create;
  try
    IDEsMarcadas(vLista);
    for vInt := 0 to Pred(vLista.Count) do
      if TIDETela(vLista[vInt]).Instancia = AIDE then
        Result := TIDETela(vLista[vInt]).DependenciaInstalada(AReceita, Escolha);
  finally
    vLista.Free;
  end;
  FDeteccoes.Add(vChave + '=' + Result);
end;

function TTelaRecursos.Escolha: TEscolhaInstalacao;
begin
  if FCatalogoInstalar <> nil then
    FEscolha.Catalogo := FCatalogoInstalar
  else
    FEscolha.Catalogo := FCatalogo;
  FEscolha.Receitas := FReceitas;
  FEscolha.Manifesto := FManifesto;
  FEscolha.PastasDependencias.Assign(FDependencias);
  FEscolha.PastaFontes := PastaFontes;
  FEscolha.Pacotes.Assign(FEscolhidos);
  FEscolha.Desinstalar := FEscolhidos.Count = 0;
  FEscolha.SomenteLibraryPath := ckSomentePaths.Visible and ckSomentePaths.Checked;
  FEscolha.Win64 := ckWin64.Visible and ckWin64.Checked;
  Result := FEscolha;
end;

procedure TTelaRecursos.EscolherPadrao;
var
  vLista: TList;
  vInt: integer;
  vPacote: TPacote;
  vTipo: TTipoPacote;
begin
  // o que as IDEs marcadas ja tem vem marcado: instalar de novo atualiza, e
  // desmarcar tudo desinstala
  FEscolhidos.Clear;
  DetectarRAL;
  for vInt := 0 to Pred(FInstaladosDiretos.Count) do
  begin
    vPacote := TPacote(FInstaladosDiretos.Objects[vInt]);
    if (IndisponivelEmTodos(vPacote) = '') and
       (FEscolhidos.IndexOf(vPacote.Nome) < 0) then
      FEscolhidos.Add(vPacote.Nome);
  end;
  if FEscolhidos.Count > 0 then
    Exit;
  // senao o nucleo do RAL (pacotes da raiz de pkg/<IDE>), menos o assistente,
  // de cada tipo de IDE da rodada; o nome vale para os dois (sem caixa)
  vLista := TList.Create;
  try
    for vTipo in Tipos do
    begin
      FCatalogo.Listar(vTipo, vLista);
      for vInt := 0 to Pred(vLista.Count) do
      begin
        vPacote := TPacote(vLista[vInt]);
        if (vPacote.Grupo = '') and (Pos('wizard', LowerCase(vPacote.Nome)) = 0) and
           (Indisponivel(vPacote) = '') and (FEscolhidos.IndexOf(vPacote.Nome) < 0) then
          FEscolhidos.Add(vPacote.Nome);
      end;
    end;
  finally
    vLista.Free;
  end;
end;

procedure TTelaRecursos.DetectarRAL;
var
  vIDEs, vFechamento: TList;
  vNomes: TStringList;
  vInt, vPac: integer;
  vTela: TIDETela;
  vTipo: TTipoPacote;
  vPacote: TPacote;
  vNome: string;
begin
  FInstalados.Clear;
  FInstaladosDiretos.Clear;
  vIDEs := TList.Create;
  vFechamento := TList.Create;
  vNomes := TStringList.Create;
  try
    IDEsMarcadas(vIDEs);
    for vInt := 0 to Pred(vIDEs.Count) do
    begin
      vTela := TIDETela(vIDEs[vInt]);
      if vTela.Instancia.Tipo = tiDelphi then
        vTipo := tpDelphi
      else
        vTipo := tpLazarus;
      if FCatalogo.Count > 0 then
        vTela.DetectarExistente(FCatalogo);
      if (vTela.Existente = nil) or not vTela.Existente.Existe then
        Continue;
      // os nomes da IDE com a grafia do catalogo; os links do Lazarus contam
      // (o pacote de runtime so tem o link)
      vTela.Existente.ListarNomes(vNomes);
      for vPac := 0 to Pred(vTela.Existente.Links.Count) do
        vNomes.Add(vTela.Existente.Links.Names[vPac]);
      for vPac := Pred(vNomes.Count) downto 0 do
      begin
        vPacote := FCatalogo.Buscar(vTipo, vNomes[vPac]);
        if vPacote = nil then
          vNomes.Delete(vPac)
        else
        begin
          vNomes[vPac] := vPacote.Nome;
          if FInstaladosDiretos.IndexOf(vPacote.Nome) < 0 then
            FInstaladosDiretos.AddObject(vPacote.Nome, vPacote);
        end;
      end;
      // e o que eles exigem: o PascalRAL do Delphi nao fica em Known Packages
      FCatalogo.Fechamento(vTipo, vNomes, vFechamento);
      for vPac := 0 to Pred(vFechamento.Count) do
      begin
        vNome := TPacote(vFechamento[vPac]).Nome;
        if FInstalados.Values[vNome] = '' then
          FInstalados.Values[vNome] := vTela.Name
        else if Pos(vTela.Name, FInstalados.Values[vNome]) = 0 then
          FInstalados.Values[vNome] := FInstalados.Values[vNome] + ', ' + vTela.Name;
      end;
    end;
  finally
    vNomes.Free;
    vFechamento.Free;
    vIDEs.Free;
  end;
end;

function TTelaRecursos.IDEsComRAL(ANomes: TStrings): boolean;
var
  vIDEs: TList;
  vInt: integer;
begin
  Result := False;
  ANomes.Clear;
  vIDEs := TList.Create;
  try
    IDEsMarcadas(vIDEs);
    for vInt := 0 to Pred(vIDEs.Count) do
      if TIDETela(vIDEs[vInt]).TemRAL then
      begin
        ANomes.Add('  ' + TIDETela(vIDEs[vInt]).Name + ' — ' +
                   TIDETela(vIDEs[vInt]).ResumoRAL);
        Result := True;
      end;
  finally
    vIDEs.Free;
  end;
end;

function TTelaRecursos.ForaDasIDEs(APacote: TPacote; out ATodas: boolean): string;
var
  vLista: TList;
  vInt, vFora: integer;
  vMotivo: string;
  vIDE: TIDEInstance;
begin
  Result := '';
  ATodas := False;
  vFora := 0;
  vLista := TList.Create;
  try
    // as IDEs do tipo do pacote: o IndyRAL.dpk nao tem nada com o Lazarus
    IDEsDoTipo(vLista, APacote.Tipo);
    for vInt := 0 to Pred(vLista.Count) do
    begin
      vIDE := TIDETela(vLista[vInt]).Instancia;
      vMotivo := Compat.Motivo(APacote, vIDE);
      if vMotivo = '' then
        Continue;
      Inc(vFora);
      if Result <> '' then
        Result := Result + '; ';
      // uma IDE so: o motivo ja cita o nome dela quase sempre
      if vLista.Count = 1 then
        Result := vMotivo
      else
        Result := Result + vIDE.Nome + ': ' + vMotivo;
    end;
    ATodas := (vFora > 0) and (vFora = vLista.Count);
  finally
    vLista.Free;
  end;
end;

procedure TTelaRecursos.IDEsDoTipo(ALista: TList; ATipo: TTipoPacote);
var
  vInt: integer;
  vTipo: TTipoIDE;
begin
  IDEsMarcadas(ALista);
  if ATipo = tpDelphi then
    vTipo := tiDelphi
  else
    vTipo := tiLazarus;
  for vInt := Pred(ALista.Count) downto 0 do
    if TIDETela(ALista[vInt]).Instancia.Tipo <> vTipo then
      ALista.Delete(vInt);
end;

procedure TTelaRecursos.IDEsMarcadas(ALista: TList);
begin
  ALista.Clear;
  if Assigned(FOnListarIDEs) then
    FOnListarIDEs(ALista);
end;

function TTelaRecursos.Indisponivel(APacote: TPacote): string;
var
  vTodas: boolean;
  vFora: string;
  vInt: integer;
  vDep: TPacote;
begin
  Result := '';
  // do zip da versao, submodulo "ausente" e so um download a mais
  if (APacote.SubmodulosAusentes.Count > 0) and not FCatalogo.Origem.BaixaSubmodulos then
    Result := Format(cmFaltaSubmodulo, [APacote.SubmodulosAusentes.CommaText])
  else if APacote.FontesAusentes.Count > 0 then
    Result := Format(cmFaltaFonte, [APacote.FontesAusentes[0]])
  else if APacote.Ordem < 0 then
    Result := cmDependenciaCircular
  else
  begin
    // nao cabe em nenhuma das IDEs marcadas do tipo dele
    vFora := ForaDasIDEs(APacote, vTodas);
    if vTodas then
      Result := vFora;
  end;
  if Result <> '' then
    Exit;
  // o que ele exige do proprio RAL tambem tem de caber (a lista de Internos
  // segue a ordem de instalacao, entao nao ha ciclo aqui)
  for vInt := 0 to Pred(APacote.Internos.Count) do
  begin
    vDep := FCatalogo.Buscar(APacote.Tipo, APacote.Internos[vInt]);
    if (vDep <> nil) and (vDep.Ordem < APacote.Ordem) and (Indisponivel(vDep) <> '') then
      Exit(Format(cmDependeDe, [vDep.Nome]));
  end;
end;

function TTelaRecursos.IndisponivelEmTodos(APacote: TPacote): string;
var
  vPar: TPacote;
begin
  Result := Indisponivel(APacote);
  if Result = '' then
    Exit;
  vPar := Par(APacote);
  if (vPar <> nil) and (Indisponivel(vPar) = '') then
    Result := '';
end;

procedure TTelaRecursos.MontarArvore;
var
  vLista, vFechamento, vVisiveis: TList;
  vChaves, vNomes, vOcultos: TStringList;
  vInt, vMarcados, vIdx: integer;
  vPacote, vPar: TPacote;
  vGrupo, vNo: TTreeNode;
  vCategoria, vUltima: TCategoriaPacote;
  vTexto, vMotivo, vFora: string;
  vMarcado, vTodas, vDoisTipos, vTemGrupo: boolean;
  vTipo: TTipoPacote;

  function NoFechamento(APac: TPacote): boolean;
  begin
    Result := (APac <> nil) and (vFechamento.IndexOf(APac) >= 0);
  end;

begin
  tvRecursos.Items.BeginUpdate;
  vLista := TList.Create;
  vFechamento := TList.Create;
  vVisiveis := TList.Create;
  // um no por nome (IndyRAL e indyral sao o mesmo recurso); Objects = o
  // primeiro pacote achado com o nome
  vChaves := TStringList.Create;
  vChaves.CaseSensitive := False;
  vNomes := TStringList.Create;
  vOcultos := TStringList.Create;
  try
    tvRecursos.Items.Clear;
    FreeAndNil(FCatalogoInstalar);
    vDoisTipos := Tipos = [tpDelphi, tpLazarus];
    for vTipo in Tipos do
    begin
      FCatalogo.Listar(vTipo, vLista);
      for vInt := 0 to Pred(vLista.Count) do
        if vChaves.IndexOf(TPacote(vLista[vInt]).Nome) < 0 then
          vChaves.AddObject(TPacote(vLista[vInt]).Nome, TPacote(vLista[vInt]));
    end;

    // o que nenhuma IDE marcada aceita nao e oferecido: nem aparece, e sai do
    // que estava escolhido
    for vInt := 0 to Pred(vChaves.Count) do
    begin
      vPacote := TPacote(vChaves.Objects[vInt]);
      vMotivo := IndisponivelEmTodos(vPacote);
      if vMotivo = '' then
        vVisiveis.Add(vPacote)
      else
      begin
        vOcultos.Add(vPacote.Nome + ': ' + vMotivo);
        vIdx := FEscolhidos.IndexOf(vPacote.Nome);
        if vIdx >= 0 then
          FEscolhidos.Delete(vIdx);
      end;
    end;
    GDependentes := TStringList.Create;
    try
      // quem depende de cada um, direta ou indiretamente: tudo depende do
      // PascalRAL, e ele vem antes do PascalRALDsgn
      for vInt := 0 to Pred(FCatalogo.Count) do
      begin
        vNomes.Clear;
        vNomes.Add(FCatalogo[vInt].Nome);
        FCatalogo.Fechamento(FCatalogo[vInt].Tipo, vNomes, vLista);
        for vIdx := 0 to Pred(vLista.Count) do
          if TPacote(vLista[vIdx]) <> FCatalogo[vInt] then
            GDependentes.Values[TPacote(vLista[vIdx]).Nome] := IntToStr(
              StrToIntDef(GDependentes.Values[TPacote(vLista[vIdx]).Nome], 0) + 1);
      end;
      vVisiveis.Sort(@CompararNaArvore);
    finally
      FreeAndNil(GDependentes);
    end;

    // o que ja entra (escolhido ou exigido), de cada tipo
    for vTipo in Tipos do
    begin
      NomesDoTipo(vTipo, vNomes);
      FCatalogo.Fechamento(vTipo, vNomes, vLista);
      for vInt := 0 to Pred(vLista.Count) do
        vFechamento.Add(vLista[vInt]);
    end;

    vMarcados := 0;
    vGrupo := nil;
    vUltima := cpBase;
    vTemGrupo := False;
    for vInt := 0 to Pred(vVisiveis.Count) do
    begin
      vPacote := TPacote(vVisiveis[vInt]);
      vPar := Par(vPacote);
      vCategoria := CategoriaDoPacote(vPacote);
      if not vTemGrupo or (vCategoria <> vUltima) then
      begin
        vGrupo := tvRecursos.Items.Add(nil, NomeCategoria(vCategoria));
        vUltima := vCategoria;
        vTemGrupo := True;
      end;

      vTexto := vPacote.Nome;
      if vPacote.Descricao <> '' then
        vTexto := vTexto + '  —  ' + vPacote.Descricao
      else if (vPar <> nil) and (vPar.Descricao <> '') then
        vTexto := vTexto + '  —  ' + vPar.Descricao;
      // com as duas IDEs, o que so existe de um lado diz qual
      if vDoisTipos and (vPar = nil) then
        vTexto := vTexto + Format(cmSoDoTipo, [NomeTipoPacote(vPacote.Tipo)]);

      vMarcado := NoFechamento(vPacote) or NoFechamento(vPar);
      if vMarcado then
        Inc(vMarcados);
      if vMarcado and (FEscolhidos.IndexOf(vPacote.Nome) < 0) then
        vTexto := vTexto + cmNecessario;
      // ja esta na IDE (instalado pelo instalador ou a mao)
      vMotivo := FInstalados.Values[vPacote.Nome];
      if (vMotivo = '') and (vPar <> nil) then
        vMotivo := FInstalados.Values[vPar.Nome];
      if vMotivo <> '' then
        if vDoisTipos or (Pos(',', vMotivo) > 0) then
          vTexto := vTexto + Format(cmInstaladoEm, [vMotivo])
        else
          vTexto := vTexto + cmInstaladoNaIDE;
      // cabe em parte das IDEs marcadas: nas outras fica de fora
      vFora := ForaDasIDEs(vPacote, vTodas);
      if vPar <> nil then
      begin
        vMotivo := ForaDasIDEs(vPar, vTodas);
        if vMotivo <> '' then
          if vFora <> '' then
            vFora := vFora + '; ' + vMotivo
          else
            vFora := vMotivo;
      end;
      if vFora <> '' then
        vTexto := vTexto + Format(cmFicaDeForaEm, [vFora]);

      vNo := tvRecursos.Items.AddChild(vGrupo, vTexto);
      vNo.Data := vPacote;
      if vMarcado then
        vNo.ImageIndex := ImgMarcado
      else
        vNo.ImageIndex := ImgDesmarcado;
      vNo.SelectedIndex := vNo.ImageIndex;
    end;

    tvRecursos.FullExpand;
    lbInfo.Caption := Format(cmRecursosNaVersao, [vVisiveis.Count, vMarcados]);
    vTexto := '';
    if vOcultos.Count > 0 then
    begin
      lbInfo.Caption := lbInfo.Caption + Format(cmRecursosOcultos, [vOcultos.Count]);
      vTexto := cmOcultosDica + LineEnding + vOcultos.Text;
    end;
    if FCatalogo.Erros.Count > 0 then
    begin
      lbInfo.Caption := lbInfo.Caption +
                        Format(cmCatalogoAvisos, [FCatalogo.Erros.Count]);
      vTexto := vTexto + FCatalogo.Erros.Text;
    end;
    lbInfo.Hint := Trim(vTexto);
    lbInfo.ShowHint := vTexto <> '';
  finally
    vOcultos.Free;
    vNomes.Free;
    vChaves.Free;
    vVisiveis.Free;
    vFechamento.Free;
    vLista.Free;
    tvRecursos.Items.EndUpdate;
  end;
end;

procedure TTelaRecursos.MostrarProgresso;
var
  vTexto: string;
begin
  EnterCriticalSection(FSecao);
  try
    vTexto := FProgresso;
  finally
    LeaveCriticalSection(FSecao);
  end;
  if vTexto <> '' then
    lbInfo.Caption := vTexto;
end;

procedure TTelaRecursos.NomesDoTipo(ATipo: TTipoPacote; ALista: TStrings);
var
  vInt: integer;
  vPacote: TPacote;
begin
  ALista.Clear;
  for vInt := 0 to Pred(FEscolhidos.Count) do
  begin
    vPacote := FCatalogo.Buscar(ATipo, FEscolhidos[vInt]);
    if (vPacote <> nil) and (ALista.IndexOf(vPacote.Nome) < 0) then
      ALista.Add(vPacote.Nome);
  end;
end;

function TTelaRecursos.Par(APacote: TPacote): TPacote;
begin
  Result := nil;
  if Tipos <> [tpDelphi, tpLazarus] then
    Exit;
  if APacote.Tipo = tpDelphi then
    Result := FCatalogo.Buscar(tpLazarus, APacote.Nome)
  else
    Result := FCatalogo.Buscar(tpDelphi, APacote.Nome);
end;

function TTelaRecursos.PastaFontes: string;
var
  vVersao: TVersaoRAL;
begin
  vVersao := Versao;
  if FModoLocal then
    Result := FPastaLocal
  else if (vVersao <> nil) and (FPastaBase <> '') then
    Result := IncludeTrailingPathDelimiter(FPastaBase) + 'PascalRAL' + PathDelim +
              vVersao.NomePasta
  else
    Result := '';
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function TTelaRecursos.PastaInstalada(AIDE: TIDEInstance; AReceita: TReceita): string;
var
  vLista: TList;
  vInt: integer;
begin
  Result := '';
  vLista := TList.Create;
  try
    IDEsMarcadas(vLista);
    for vInt := 0 to Pred(vLista.Count) do
      if TIDETela(vLista[vInt]).Instancia = AIDE then
        Result := TIDETela(vLista[vInt]).PastaDependencia(AReceita, Escolha);
  finally
    vLista.Free;
  end;
end;

function TTelaRecursos.PlanejarDependencias: string;
var
  vIDEs, vLista: TList;
  vFora, vNomes, vExigidas, vPedidas: TStringList;
  vInt, vIDE, vPac: integer;
  vReceita: TReceita;
  vBaixa: TBaixaDependencia;
  vBloco: TBlocoIDE;
  vInstancia: TIDEInstance;
  vVersao, vMotivo, vChave: string;
  vTipo: TTipoPacote;
begin
  Result := '';
  FDependencias.Clear;
  FEscolha.PastasDependencias.Clear;
  if FCatalogo.Count = 0 then
    Exit;
  vIDEs := TList.Create;
  vLista := TList.Create;
  vFora := TStringList.Create;
  vNomes := TStringList.Create;
  vExigidas := TStringList.Create;
  // nome@versao = para quais pacotes; Objects = a receita
  vPedidas := TStringList.Create;
  Screen.Cursor := crHourGlass;
  try
    // cada IDE pede o que ela nao tem, na versao que ela aceita (o Zeos do
    // FPC 3.3 nao e o do Delphi); o que nao cabe nela nao pede nada
    IDEsMarcadas(vIDEs);
    for vIDE := 0 to Pred(vIDEs.Count) do
    begin
      vInstancia := TIDETela(vIDEs[vIDE]).Instancia;
      vLista.Clear;
      vFora.Clear;
      if vInstancia.Tipo = tiDelphi then
        vTipo := tpDelphi
      else
        vTipo := tpLazarus;
      NomesDoTipo(vTipo, vNomes);
      FCatalogo.Fechamento(vTipo, vNomes, vLista);
      Compat.Filtrar(vLista, vInstancia, vFora);
      vNomes.Clear;
      for vPac := 0 to Pred(vLista.Count) do
        vNomes.Add(TPacote(vLista[vPac]).Nome);
      FReceitas.Exigidas(FCatalogo, vTipo, vNomes, vExigidas);
      for vInt := 0 to Pred(vExigidas.Count) do
      begin
        vReceita := TReceita(vExigidas.Objects[vInt]);
        vBloco := vReceita.Bloco(vTipo);
        // comercial, ou sem como instalar neste tipo de IDE: o plano da IDE
        // diz se ela ja tem ou se falta
        if not vReceita.PodeBaixar or not vBloco.Existe or (vBloco.Acoes.Count = 0) then
          Continue;
        // a que ja esta vale
        if Detectar(vInstancia, vReceita) <> '' then
          Continue;
        vVersao := Compat.VersaoDependencia(vReceita, vInstancia, vMotivo);
        if vMotivo <> '' then
          Continue;
        vChave := ChaveDependencia(vReceita.Nome, vVersao);
        if vPedidas.IndexOfName(vChave) < 0 then
          vPedidas.AddObject(vChave + '=' + vExigidas[vInt], vReceita);
      end;
    end;

    for vInt := 0 to Pred(vPedidas.Count) do
    begin
      vReceita := TReceita(vPedidas.Objects[vInt]);
      vChave := vPedidas.Names[vInt];
      vBaixa := TBaixaDependencia.Create(vReceita);
      try
        vBaixa.PastaBase := FPastaBase;
        vBaixa.VersaoPedida := Copy(vChave, Pos('@', vChave) + 1, MaxInt);
        if vBaixa.ResolverVersao then
        begin
          FDependencias.Values[vChave] := vBaixa.PastaDestino;
          Result := Result + Format(cmPlanoBaixarDep,
            [vReceita.Nome, vBaixa.Ref,
             IfThen(SameText(vBaixa.Fonte, vReceita.Github), '',
                    Format(cmPlanoDe, [vBaixa.Fonte])),
             ExcludeTrailingPathDelimiter(vBaixa.PastaDestino),
             vPedidas.ValueFromIndex[vInt]]) + LineEnding;
        end
        else
          Result := Result + Format(cmPlanoErroDep,
                                    [vBaixa.Erro, vPedidas.ValueFromIndex[vInt]]) +
                    LineEnding;
      finally
        vBaixa.Free;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
    vPedidas.Free;
    vExigidas.Free;
    vNomes.Free;
    vFora.Free;
    vLista.Free;
    vIDEs.Free;
  end;
  FEscolha.PastasDependencias.Assign(FDependencias);
end;

function TTelaRecursos.PlanoFontes: string;
var
  vVersao: TVersaoRAL;
  vSubs: TStringList;
begin
  vVersao := Versao;
  if FModoLocal or (vVersao = nil) then
    Exit(Format(cmPlanoFontesLocal, [ExcludeTrailingPathDelimiter(PastaFontes)]) +
         LineEnding);

  vSubs := TStringList.Create;
  try
    SubmodulosDaEscolha(vSubs);
    Result := Format(cmPlanoBaixarRAL, [vVersao.Ref]);
    if (FCatalogo.Origem is TOrigemZip) and
       (TOrigemZip(FCatalogo.Origem).Commit <> '') then
      Result := Result + Format(cmPlanoCommit,
                                [Copy(TOrigemZip(FCatalogo.Origem).Commit, 1, 7)]);
    Result := Result + Format(cmPlanoPara, [ExcludeTrailingPathDelimiter(PastaFontes)]) +
              LineEnding;
    if vSubs.Count > 0 then
      Result := Result + Format(cmPlanoSubmodulos, [vSubs.CommaText]) + LineEnding;
  finally
    vSubs.Free;
  end;
end;

function TTelaRecursos.PrepararFontes(ALog: TLogLinha): boolean;
var
  vVersao: TVersaoRAL;
  vPreparo: TPreparoFontes;
begin
  Result := True;
  vVersao := Versao;
  if FModoLocal or (vVersao = nil) then
    Exit;

  FreeAndNil(FCatalogoInstalar);
  FRepo.Log := ALog;
  vPreparo := TPreparoFontes.Create(FRepo, vVersao.Ref, vVersao.Tipo = tvRamo);
  try
    vPreparo.PastaBase := FPastaBase;
    vPreparo.Log := ALog;
    SubmodulosDaEscolha(vPreparo.Submodulos);
    Result := vPreparo.Executar;
    if not Result then
    begin
      ALog(cmPrefixoErro + vPreparo.Erro);
      ALog(cmNenhumaIDEAlterada);
      Exit;
    end;

    // daqui em diante vale a pasta de verdade, com os submodulos
    FCatalogoInstalar := TCatalogo.Create;
    if not FCatalogoInstalar.Carregar(vPreparo.PastaDestino) then
    begin
      ALog(cmPrefixoErro + Format(emBaixadosSemPacotes, [vPreparo.PastaDestino]));
      FreeAndNil(FCatalogoInstalar);
      Result := False;
    end;
  finally
    FRepo.Log := nil;
    vPreparo.Free;
  end;
end;

procedure TTelaRecursos.Progresso(const ALidos, ATotal: int64);
var
  vTexto: string;
begin
  // chamado pela thread da carga (e pela da instalacao, na thread principal)
  if (ALidos - FUltimoProgresso < 256 * 1024) and (ALidos <> ATotal) then
    Exit;
  FUltimoProgresso := ALidos;
  if ATotal > 0 then
    vTexto := Format(cmBaixandoDeMB, [ALidos / 1048576, ATotal / 1048576])
  else
    vTexto := Format(cmBaixandoMB, [ALidos / 1048576]);
  EnterCriticalSection(FSecao);
  try
    FProgresso := vTexto;
  finally
    LeaveCriticalSection(FSecao);
  end;
  if GetCurrentThreadID = MainThreadID then
  begin
    MostrarProgresso;
    Application.ProcessMessages;
  end
  else
    TThread.Queue(nil, @MostrarProgresso);
end;

procedure TTelaRecursos.SetIDE(AValue: integer);
var
  vMudou: boolean;
begin
  vMudou := AValue <> IDE;
  inherited SetIDE(AValue);
  AtualizarOpcoes;
  // os nomes dos pacotes mudam de uma IDE para a outra
  if vMudou and (FCatalogo.Count > 0) and not Carregando then
  begin
    EscolherPadrao;
    MontarArvore;
  end;
end;

procedure TTelaRecursos.SubmodulosDaEscolha(ALista: TStrings);
var
  vFechamento: TList;
  vNomes: TStringList;
  vInt, vSub: integer;
  vTipo: TTipoPacote;
begin
  ALista.Clear;
  vFechamento := TList.Create;
  vNomes := TStringList.Create;
  try
    // os submodulos de todos os tipos de IDE da rodada: a pasta e uma so
    for vTipo in Tipos do
    begin
      NomesDoTipo(vTipo, vNomes);
      FCatalogo.Fechamento(vTipo, vNomes, vFechamento);
      for vInt := 0 to Pred(vFechamento.Count) do
        for vSub := 0 to Pred(TPacote(vFechamento[vInt]).Submodulos.Count) do
          if ALista.IndexOf(TPacote(vFechamento[vInt]).Submodulos[vSub]) < 0 then
            ALista.Add(TPacote(vFechamento[vInt]).Submodulos[vSub]);
    end;
  finally
    vNomes.Free;
    vFechamento.Free;
  end;
end;

function TTelaRecursos.Tipos: TTiposPacote;
begin
  // 0 - Delphi, 1 - Lazarus, 2 - os dois
  case IDE of
    0: Result := [tpDelphi];
    1: Result := [tpLazarus];
    2: Result := [tpDelphi, tpLazarus];
  else
    Result := [];
  end;
end;

procedure TTelaRecursos.TrocarModo;
begin
  FModoLocal := (cbVersao.ItemIndex >= 0) and
                (cbVersao.Items.Objects[cbVersao.ItemIndex] = nil);
  if FModoLocal then
  begin
    lbedDownloadPath.EditLabel.Caption := cmPastaFontesLocal;
    lbedDownloadPath.Text := FPastaLocal;
  end
  else
  begin
    lbedDownloadPath.EditLabel.Caption := cmPastaInstalacao;
    lbedDownloadPath.Text := FPastaBase;
  end;
  AtualizarDestino;
  CarregarCatalogo;
end;

procedure TTelaRecursos.TrocarModoAsync(AData: PtrInt);
begin
  TrocarModo;
end;

function TTelaRecursos.ValidatePageNext: boolean;
var
  vNomes: TStringList;
begin
  Result := False;
  if Carregando then
  begin
    ShowMessage(cmAguardeCarga);
    Exit;
  end;
  if not FModoLocal then
    FPastaBase := Trim(lbedDownloadPath.Text)
  else if not SameText(FPastaLocal, Trim(lbedDownloadPath.Text)) then
  begin
    FPastaLocal := Trim(lbedDownloadPath.Text);
    CarregarCatalogo;
  end;
  if FEscolhidos.Count = 0 then
  begin
    // nada marcado numa IDE que tem o RAL: a rodada desinstala, e nao precisa
    // de versao nem de pasta
    vNomes := TStringList.Create;
    try
      if not IDEsComRAL(vNomes) then
      begin
        ShowMessage(cmMarqueUmRecurso);
        Exit;
      end;
      Result := MessageDlg(cmDesinstalarTitulo,
                           Format(cmNadaMarcadoDesinstala, [vNomes.Text]),
                           mtConfirmation, [mbYes, mbNo], 0) = mrYes;
    finally
      vNomes.Free;
    end;
    Exit;
  end;
  if FCatalogo.Count = 0 then
  begin
    if FErroCatalogo <> '' then
      ShowMessage(FErroCatalogo)
    else
      ShowMessage(cmEscolhaVersao);
    Exit;
  end;
  if not FModoLocal then
  begin
    if FPastaBase = '' then
    begin
      ShowMessage(cmEscolhaPastaInstalacao);
      Exit;
    end;
    if PastaDeOutro(PastaFontes) then
    begin
      ShowMessage(Format(cmPastaDeOutro, [ExcludeTrailingPathDelimiter(PastaFontes)]));
      Exit;
    end;
  end;
  Result := True;
end;
function TTelaRecursos.ValidatePagePrior: boolean;
begin
  // voltar durante a carga e seguro: ela termina sozinha e so atualiza a tela
  Result := True;
end;

function TTelaRecursos.Versao: TVersaoRAL;
begin
  Result := nil;
  if (cbVersao.ItemIndex >= 0) and not FModoLocal then
    Result := TVersaoRAL(cbVersao.Items.Objects[cbVersao.ItemIndex]);
end;

end.
