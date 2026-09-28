/// Last page: shows the plan (what will be done in each checked IDE) before
/// touching anything, asks for confirmation and runs. The plan compares each
/// IDE with the choice: the button says Atualizar when every IDE that changes
/// gets another version, and with nothing to do anywhere the run does not
/// start ("Reinstalar mesmo assim" does it anyway). The run log also goes to a
/// file, which is what is sent when something goes wrong.
unit RALInst.Tela.Instalar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  RALInst.Tela.Modelo;

type
  /// Plan, installation and uninstallation page.
  TTelaInstalar = class(TTelaModelo)
    lbDesinstalar: TLabel;
    lbReinstalar: TLabel;
    lbSubTitle: TLabel;
    mLogInstall: TMemo;
    procedure lbDesinstalarClick(Sender: TObject);
    procedure lbNextClick(Sender: TObject);
    procedure lbReinstalarClick(Sender: TObject);
  private
    /// No feature chosen: the page uninstalls
    FDesinstalar: boolean;
    FExistentes: string;
    FInstalling: boolean;
    /// Every checked IDE already has the chosen version and features
    FNadaAFazer: boolean;
    FPlano: string;
    /// A run happened since the plan was shown: the plan is old
    FPlanoVelho: boolean;
    /// Runs the plan (already shown and confirmed)
    procedure Executar;
    /// Computes and shows the plan; the button and the links follow it
    procedure MostrarPlano;
    /// Saves the memo into <data>\logs\<prefix>-<date>.log; '' on failure
    function SalvarLog(const APrefixo: string): string;
  protected
    function ValidatePagePrior: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    /// On entering the page: the plan, before any write
    procedure AoMostrar; override;
    /// One line in the run log (the download stage writes here too)
    procedure LogarLinha(const ALinha: string);
  end;

implementation

{$R *.lfm}

uses
  RALInst.Processo, RALInst.Situacao, RALInst.Tela.Mensagens, RALInst.Tela.Principal;

procedure ProcessarMensagens;
begin
  Application.ProcessMessages;
end;

{ TTelaInstalar }

constructor TTelaInstalar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInstalling := False;
  mLogInstall.ScrollBars := ssAutoBoth;
  mLogInstall.WordWrap := False;
  // os relatorios sao tabelas alinhadas por coluna
  {$IFDEF MSWINDOWS}
  mLogInstall.Font.Name := 'Consolas';
  {$ELSE}{$IFDEF DARWIN}
  mLogInstall.Font.Name := 'Menlo';
  {$ELSE}
  mLogInstall.Font.Name := 'Monospace';
  {$ENDIF}{$ENDIF}
end;

procedure TTelaInstalar.lbDesinstalarClick(Sender: TObject);
var
  vOk: boolean;
  vLog: string;
begin
  if FInstalling or (FExistentes = '') then
    Exit;
  if MessageDlg(cmDesinstalarTitulo, Format(cmDesinstalarPergunta, [FExistentes]),
       mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  FInstalling := True;
  AoEsperarProcesso := @ProcessarMensagens;
  Screen.Cursor := crHourGlass;
  try
    mLogInstall.Clear;
    vOk := TelaPrincipal.DesinstalarRAL;
    vLog := SalvarLog('desinstalacao');
    mLogInstall.Lines.Add('');
    if vOk then
      mLogInstall.Lines.Add(cmDesinstalacaoConcluida)
    else
      mLogInstall.Lines.Add(cmDesinstalacaoComErros);
    if vLog <> '' then
    begin
      mLogInstall.Lines.Add(Format(cmLog, [vLog]));
      mLogInstall.Lines.SaveToFile(vLog);
    end;
    FExistentes := TelaPrincipal.InstalacoesExistentes;
    lbDesinstalar.Visible := (FExistentes <> '') and not FDesinstalar;
    lbReinstalar.Visible := False;
    FPlanoVelho := True;
  finally
    Screen.Cursor := crDefault;
    AoEsperarProcesso := nil;
    FInstalling := False;
  end;
end;

procedure TTelaInstalar.lbNextClick(Sender: TObject);
var
  vTitulo: string;
begin
  if FInstalling then
    Exit;
  // depois de uma rodada, o plano mostrado ja nao vale: primeiro o novo
  if FPlanoVelho then
  begin
    MostrarPlano;
    Exit;
  end;
  if FDesinstalar then
  begin
    lbDesinstalarClick(Sender);
    Exit;
  end;
  if FNadaAFazer then
  begin
    ShowMessage(cmNadaAFazerTodas);
    Exit;
  end;
  if lbNext.Caption = cmBotaoAtualizar then
    vTitulo := cmAtualizarTitulo
  else
    vTitulo := cmInstalarTitulo;
  if MessageDlg(vTitulo, Format(cmInstalarPergunta, [TelaPrincipal.Mudancas]),
       mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  Executar;
end;

procedure TTelaInstalar.lbReinstalarClick(Sender: TObject);
begin
  if FInstalling or FDesinstalar then
    Exit;
  if MessageDlg(cmReinstalarTitulo, cmReinstalarPergunta, mtConfirmation,
       [mbYes, mbNo], 0) <> mrYes then
    Exit;
  // o plano de novo, agora pedindo tudo: e ele que vai para o log
  TelaPrincipal.Reinstalar(True);
  try
    Screen.Cursor := crHourGlass;
    try
      FPlano := TelaPrincipal.PlanoInstalacao;
    finally
      Screen.Cursor := crDefault;
    end;
    Executar;
  finally
    TelaPrincipal.Reinstalar(False);
  end;
end;

procedure TTelaInstalar.AoMostrar;
begin
  if FInstalling then
    Exit;
  MostrarPlano;
end;

procedure TTelaInstalar.Executar;
var
  vOk: boolean;
  vLog: string;
begin
  FInstalling := True;
  AoEsperarProcesso := @ProcessarMensagens;
  Screen.Cursor := crHourGlass;
  try
    mLogInstall.Clear;
    mLogInstall.Lines.Add(cmPlanoTitulo);
    mLogInstall.Lines.Add(TrimRight(FPlano));
    mLogInstall.Lines.Add('');
    Application.ProcessMessages;

    vOk := TelaPrincipal.InstallRAL(mLogInstall);

    vLog := SalvarLog('instalacao');
    mLogInstall.Lines.Add('');
    // o que acabou de ser instalado passa a poder ser desfeito daqui
    FExistentes := TelaPrincipal.InstalacoesExistentes;
    lbDesinstalar.Visible := (FExistentes <> '') and not FDesinstalar;
    lbReinstalar.Visible := False;
    FPlanoVelho := True;
    if vOk then
      mLogInstall.Lines.Add(cmInstalacaoConcluida)
    else
      mLogInstall.Lines.Add(cmInstalacaoComErros);
    if vLog <> '' then
    begin
      mLogInstall.Lines.Add(Format(cmLog, [vLog]));
      mLogInstall.Lines.SaveToFile(vLog);
    end;
  finally
    Screen.Cursor := crDefault;
    AoEsperarProcesso := nil;
    FInstalling := False;
  end;
end;

procedure TTelaInstalar.MostrarPlano;
var
  vComMudanca, vAtualizacoes: integer;
begin
  FPlanoVelho := False;
  Screen.Cursor := crHourGlass;
  try
    FDesinstalar := TelaPrincipal.ModoDesinstalar;
    FPlano := TelaPrincipal.PlanoInstalacao;
  finally
    Screen.Cursor := crDefault;
  end;
  FNadaAFazer := False;
  if FDesinstalar then
  begin
    lbNext.Caption := cmBotaoDesinstalar;
    mLogInstall.Lines.Text := cmOQueSeraFeito + LineEnding + LineEnding + FPlano +
                              LineEnding + cmCliqueDesinstalar;
  end
  else
  begin
    // o que o plano achou em cada IDE decide o botao: so atualizacoes e
    // "Atualizar"; nada em lugar nenhum nao roda
    vComMudanca := TelaPrincipal.ContarMudancas([Low(TMudancaRAL)..High(TMudancaRAL)] -
                                                [mrNada]);
    vAtualizacoes := TelaPrincipal.ContarMudancas([mrAtualizar, mrVoltar, mrTrocar,
                                                   mrRecompilar]);
    FNadaAFazer := vComMudanca = 0;
    if (vComMudanca > 0) and (vAtualizacoes = vComMudanca) then
      lbNext.Caption := cmBotaoAtualizar
    else
      lbNext.Caption := cmBotaoInstalar;
    if FNadaAFazer then
      mLogInstall.Lines.Text := cmOQueSeraFeito + LineEnding + LineEnding + FPlano +
                                LineEnding + cmNadaAFazerTodas
    else
      mLogInstall.Lines.Text := cmOQueSeraFeito + LineEnding + LineEnding + FPlano +
                                LineEnding + Format(cmCliqueInstalar, [lbNext.Caption]);
  end;
  // desinstalar: o que o instalador ja pos nestas IDEs
  FExistentes := TelaPrincipal.InstalacoesExistentes;
  // no modo desinstalar o botao principal ja faz isso
  lbDesinstalar.Visible := (FExistentes <> '') and not FDesinstalar;
  // reinstalar so muda algo onde a versao e a mesma
  lbReinstalar.Visible := not FDesinstalar and
                          (TelaPrincipal.ContarMudancas([mrNada, mrModificar]) > 0);
  mLogInstall.SelStart := 0;
end;

procedure TTelaInstalar.LogarLinha(const ALinha: string);
begin
  mLogInstall.Lines.Add(ALinha);
  Application.ProcessMessages;
end;

function TTelaInstalar.SalvarLog(const APrefixo: string): string;
var
  vPasta: string;
begin
  Result := '';
  vPasta := PastaDadosInstalador + 'logs';
  try
    ForceDirectories(vPasta);
    Result := IncludeTrailingPathDelimiter(vPasta) +
              APrefixo + '-' + FormatDateTime('yyyymmdd-hhnnss', Now) + '.log';
    mLogInstall.Lines.SaveToFile(Result);
  except
    Result := '';
  end;
end;

function TTelaInstalar.ValidatePagePrior: boolean;
begin
  Result := not FInstalling;
end;

end.
