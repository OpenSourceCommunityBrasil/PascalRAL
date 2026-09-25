/// Main window: the wizard pages (language, IDE kind, IDE list, features,
/// install) and what crosses them (language, theme, IDE kind, the plan and the
/// run).
unit RALInst.Tela.Principal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons,
  RALInst.Tela.IDE, RALInst.Tela.Idioma, RALInst.Tela.Instalar, RALInst.Tela.Modelo,
  RALInst.AutoAtualizacao, RALInst.Tela.Recursos, RALInst.Tela.Tarefa, RALInst.Tela.Temas,
  RALInst.Tela.VersoesIDE;

type
  /// The installer window.
  TTelaPrincipal = class(TForm)
    bTranslate: TButton;
    ntPages: TNotebook;
    procedure bTranslateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAtualizacao: TAtualizacao;
    FIDE: integer;
    FLanguage: TLanguages;
    FPage: integer;
    FPgIDE: TTelaIDE;
    FPgIDEVersions: TTelaVersoesIDE;
    FPgIdioma: TTelaIdioma;
    FPgInstall: TTelaInstalar;
    FPgRecursos: TTelaRecursos;
    FResultadoAtualizacao: TResultadoVerificacao;
    FTarefaAtualizacao: TTarefa;
    FTheme: TThemes;
    /// Translates every text into the language and tells the pages
    procedure AplicarLanguage(AValue: TLanguages);
    /// Main thread: the update check finished; offers the new version
    procedure AtualizacaoConsultada(Sender: TObject);
    /// Background thread: asks GitHub for a newer installer
    procedure ConsultarAtualizacao;
    /// Creates a page inside the notebook
    function CriarPagina(AClass: TTelaModeloClass): TTelaModelo;
    /// Developer tool: writes src/languages/ralinstaller.<lang>.po
    procedure GerarPO(const ALang: string);
    /// The pages, in order
    function Paginas: TList;
    procedure SetIDE(AValue: integer);
    procedure SetLanguage(AValue: TLanguages);
    procedure SetPage(AValue: integer);
    procedure SetTheme(AValue: TThemes);
    /// After the window shows, once: starts the update check
    procedure VerificarAtualizacao(AData: PtrInt);
  public
    destructor Destroy; override;
    /// Removes RAL from the checked IDEs: the installer's receipts, newest first,
    /// then what was installed by hand
    function DesinstalarRAL: boolean;
    /// The RAL the checked IDEs already have (receipts or by hand), one line per
    /// IDE; empty when none has it
    function InstalacoesExistentes: string;
    /// Downloads and installs in every checked IDE
    function InstallRAL(ALog: TMemo): boolean;
    /// No feature chosen: the run uninstalls
    function ModoDesinstalar: boolean;
    procedure NextPage;
    /// The plan of every checked IDE with the chosen features
    function PlanoInstalacao: string;
    procedure PriorPage;

  published
    /// 0 - Delphi, 1 - Lazarus, 2 - both
    property IDE: integer read FIDE write SetIDE;
    property Language: TLanguages read FLanguage write SetLanguage;
    property Page: integer read FPage write SetPage;
    property Theme: TThemes read FTheme write SetTheme;
  end;

var
  TelaPrincipal: TTelaPrincipal;

implementation

{$R *.lfm}

uses
  RALInst.Processo, RALInst.Recibos, RALInst.Traducao,
  RALInst.Versao, RALInst.Tela.GeradorPO, RALInst.Tela.Instalacao,
  RALInst.Tela.Mensagens, RALInst.Tela.Traducao;

{ TTelaPrincipal }

procedure TTelaPrincipal.bTranslateClick(Sender: TObject);
var
  vIdioma: string;
begin
  for vIdioma in IdiomasInstalador do
    GerarPO(vIdioma);
  // coletar as mensagens devolve o portugues a elas: o idioma volta
  AplicarLanguage(FLanguage);
end;

procedure TTelaPrincipal.FormCreate(Sender: TObject);
begin
  FPgIdioma := TTelaIdioma(CriarPagina(TTelaIdioma));
  FPgIDE := TTelaIDE(CriarPagina(TTelaIDE));
  FPgIDEVersions := TTelaVersoesIDE(CriarPagina(TTelaVersoesIDE));
  FPgRecursos := TTelaRecursos(CriarPagina(TTelaRecursos));
  FPgInstall := TTelaInstalar(CriarPagina(TTelaInstalar));
  // a tela de recursos confere cada pacote contra as IDEs marcadas
  FPgRecursos.OnListarIDEs := @FPgIDEVersions.ListarMarcadas;

  FTheme := tLight;
  Theme := tDark;
  FIDE := 0;
  IDE := -1;
  // o idioma do sistema ja vem escolhido; a primeira tela deixa trocar
  AplicarLanguage(IdiomaDoCodigo(IdiomaDoSistema));

  // o .old de uma atualizacao anterior sai; a consulta ao GitHub so depois
  // que a janela aparece
  LimparAtualizacaoAnterior;
  Application.QueueAsyncCall(@VerificarAtualizacao, 0);
end;

procedure TTelaPrincipal.AplicarLanguage(AValue: TLanguages);
var
  vPaginas: TList;
  vInt: integer;
begin
  FLanguage := AValue;
  AplicarIdioma(CodigoIdioma(AValue));
  Caption := 'RAL Installer ' + VersaoInstalador;
  // os textos do .lfm ja estao traduzidos; cada pagina reescreve os seus
  vPaginas := Paginas;
  try
    for vInt := 0 to Pred(vPaginas.Count) do
    begin
      TTelaModelo(vPaginas[vInt]).Language := AValue;
      TTelaModelo(vPaginas[vInt]).AtualizarTextos;
    end;
  finally
    vPaginas.Free;
  end;
end;

procedure TTelaPrincipal.AtualizacaoConsultada(Sender: TObject);
var
  vNovo: string;
begin
  case FResultadoAtualizacao of
    rvAtualizado:
      Exit;
    rvNaoVerificou:
      begin
        // sem internet ou sem cota nao e "esta atualizado": diz que nao deu
        Caption := 'RAL Installer ' + VersaoInstalador + cmNaoVerificouVersao;
        Hint := FAtualizacao.Erro;
        ShowHint := True;
        Exit;
      end;
  end;

  if MessageDlg(cmVersaoNovaTitulo,
       Format(cmVersaoNovaDisponivel, [FAtualizacao.VersaoNova, VersaoInstalador]) +
       LineEnding + LineEnding + Copy(FAtualizacao.Notas, 1, 600) + LineEnding +
       LineEnding + cmBaixarReiniciar, mtInformation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  Screen.Cursor := crHourGlass;
  try
    if not FAtualizacao.Baixar(vNovo) or not FAtualizacao.Trocar(vNovo) then
    begin
      Screen.Cursor := crDefault;
      ShowMessage(Format(cmAtualizacaoNaoFeita, [FAtualizacao.Erro, VersaoInstalador]));
      Exit;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  if FAtualizacao.Reiniciar(['--pos-atualizacao']) then
    Application.Terminate
  else
    ShowMessage(Format(cmAbraDeNovo, [FAtualizacao.VersaoNova, FAtualizacao.Erro]));
end;

procedure TTelaPrincipal.ConsultarAtualizacao;
begin
  FResultadoAtualizacao := FAtualizacao.Verificar;
end;

destructor TTelaPrincipal.Destroy;
begin
  // a consulta pode ainda estar na rede: espera por ela antes de liberar
  if FTarefaAtualizacao <> nil then
    FTarefaAtualizacao.WaitFor;
  FreeAndNil(FTarefaAtualizacao);
  FreeAndNil(FAtualizacao);
  inherited Destroy;
end;
function TTelaPrincipal.CriarPagina(AClass: TTelaModeloClass): TTelaModelo;
var
  vPage: TPage;
begin
  vPage := TPage.Create(Self);
  vPage.Parent := ntPages;

  Result := AClass.Create(Self);
  Result.Parent := vPage;
  Result.Align := alClient;
end;

function TTelaPrincipal.DesinstalarRAL: boolean;
var
  vEscolha: TEscolhaInstalacao;
  vAntes: boolean;
begin
  // a mesma rodada da instalacao, no modo desinstalar
  vEscolha := FPgRecursos.Escolha;
  vAntes := vEscolha.Desinstalar;
  vEscolha.Desinstalar := True;
  try
    Result := FPgIDEVersions.InstallRAL(FPgInstall.mLogInstall, vEscolha);
    FPgInstall.LogarLinha(cmResumoTitulo);
    FPgInstall.LogarLinha(TrimRight(FPgIDEVersions.Resumos));
  finally
    vEscolha.Desinstalar := vAntes;
  end;
end;
procedure TTelaPrincipal.GerarPO(const ALang: string);
var
  vGerador: TGeradorPO;
begin
  vGerador := TGeradorPO.Create(ALang);
  try
    vGerador.AdicionarComponentes(Self);
    vGerador.AdicionarMensagens;
    vGerador.Salvar('src' + PathDelim + 'languages' + PathDelim + 'ralinstaller.' +
                    ALang + '.po');
  finally
    vGerador.Free;
  end;
end;

function TTelaPrincipal.InstalacoesExistentes: string;
var
  vIDEs, vDaIDE: TList;
  vRecibos: TRecibos;
  vInt: integer;
  vIDE: TIDETela;
begin
  Result := '';
  vIDEs := TList.Create;
  vDaIDE := TList.Create;
  vRecibos := TRecibos.Create(True);
  try
    vRecibos.Carregar(PastaDadosInstalador + 'recibos');
    FPgIDEVersions.ListarMarcadas(vIDEs);
    for vInt := 0 to Pred(vIDEs.Count) do
    begin
      vIDE := TIDETela(vIDEs[vInt]);
      vRecibos.DaIDE(vIDE.Instancia.RootDir, vDaIDE);
      if vDaIDE.Count > 0 then
      begin
        Result := Result + '  ' + TRecibo(vDaIDE[0]).Descricao;
        if vDaIDE.Count > 1 then
          Result := Result + Format(cmMaisInstalacoes, [vDaIDE.Count - 1]);
        Result := Result + LineEnding;
      end
      // instalado a mao (ou antes do instalador): o que a IDE diz ter
      else if vIDE.TemRAL then
        Result := Result + '  ' + vIDE.Name + ' — ' + vIDE.ResumoRAL + LineEnding;
    end;
  finally
    vRecibos.Free;
    vDaIDE.Free;
    vIDEs.Free;
  end;
end;
function TTelaPrincipal.InstallRAL(ALog: TMemo): boolean;
begin
  if ModoDesinstalar then
    Exit(DesinstalarRAL);
  // etapa 1: os fontes na pasta final; se falhar, nenhuma IDE e tocada
  Result := FPgRecursos.PrepararFontes(@FPgInstall.LogarLinha);
  if not Result then
    Exit;
  // etapa 1b: as dependencias que alguma IDE nao tem; o que falhar deixa de
  // fora so os pacotes que precisavam dela
  if not FPgRecursos.BaixarDependencias(@FPgInstall.LogarLinha) then
    Result := False;
  FPgInstall.LogarLinha('');
  // etapa 2: cada IDE marcada
  if not FPgIDEVersions.InstallRAL(ALog, FPgRecursos.Escolha) then
    Result := False;
  // o relatorio final, por IDE: o que entrou, o que ficou de fora e por que
  FPgInstall.LogarLinha(cmResumoTitulo);
  FPgInstall.LogarLinha(TrimRight(FPgIDEVersions.Resumos));
end;

procedure TTelaPrincipal.NextPage;
begin
  Page := FPage + 1;
end;

function TTelaPrincipal.Paginas: TList;
begin
  Result := TList.Create;
  Result.Add(FPgIdioma);
  Result.Add(FPgIDE);
  Result.Add(FPgIDEVersions);
  Result.Add(FPgRecursos);
  Result.Add(FPgInstall);
end;

function TTelaPrincipal.ModoDesinstalar: boolean;
begin
  Result := FPgRecursos.Escolha.Desinstalar;
end;

function TTelaPrincipal.PlanoInstalacao: string;
begin
  // desinstalar nao baixa nada: so o que sai de cada IDE
  if ModoDesinstalar then
    Exit(FPgIDEVersions.Plano(FPgRecursos.Escolha));
  // as dependencias primeiro: o plano de cada IDE precisa saber para onde
  // cada uma vai ser baixada
  Result := FPgRecursos.PlanoFontes +
            FPgRecursos.PlanejarDependencias + LineEnding +
            FPgIDEVersions.Plano(FPgRecursos.Escolha);
end;

procedure TTelaPrincipal.PriorPage;
begin
  Page := FPage - 1;
end;

procedure TTelaPrincipal.SetIDE(AValue: integer);
begin
  if FIDE = AValue then
    Exit;
  FIDE := AValue;
  FPgIDE.IDE := AValue;
  FPgIDEVersions.IDE := AValue;
  FPgRecursos.IDE := AValue;
end;

procedure TTelaPrincipal.SetLanguage(AValue: TLanguages);
begin
  if FLanguage = AValue then
    Exit;
  AplicarLanguage(AValue);
end;

procedure TTelaPrincipal.SetPage(AValue: integer);
begin
  if FPage = AValue then
    Exit;
  if (AValue >= 0) and (AValue < ntPages.PageCount) then
  begin
    FPage := AValue;
    ntPages.PageIndex := AValue;
    if (ntPages.Page[AValue].ControlCount > 0) and
       (ntPages.Page[AValue].Controls[0] is TTelaModelo) then
      TTelaModelo(ntPages.Page[AValue].Controls[0]).AoMostrar;
  end;
end;

procedure TTelaPrincipal.SetTheme(AValue: TThemes);
var
  vPaginas: TList;
  vInt: integer;
begin
  if FTheme = AValue then
    Exit;
  FTheme := AValue;
  vPaginas := Paginas;
  try
    for vInt := 0 to Pred(vPaginas.Count) do
      TTelaModelo(vPaginas[vInt]).Theme := AValue;
  finally
    vPaginas.Free;
  end;
end;

procedure TTelaPrincipal.VerificarAtualizacao(AData: PtrInt);
var
  vInt: integer;
begin
  for vInt := 1 to ParamCount do
    if ParamStr(vInt) = '--pos-atualizacao' then
    begin
      ShowMessage(Format(cmAtualizadoPara, [VersaoInstalador]));
      Exit;
    end;
  // a consulta vai a rede: fora da thread principal, a janela nao para
  FAtualizacao := TAtualizacao.Create;
  FTarefaAtualizacao := TTarefa.Create(@ConsultarAtualizacao, @AtualizacaoConsultada);
end;
end.
