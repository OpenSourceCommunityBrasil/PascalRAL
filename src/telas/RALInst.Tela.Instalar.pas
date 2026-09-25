/// Last page: shows the plan (what will be done in each checked IDE) before
/// touching anything, asks for confirmation and runs. The run log also goes to
/// a file, which is what is sent when something goes wrong.
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
    lbSubTitle: TLabel;
    mLogInstall: TMemo;
    procedure lbDesinstalarClick(Sender: TObject);
    procedure lbNextClick(Sender: TObject);
  private
    /// No feature chosen: the page uninstalls
    FDesinstalar: boolean;
    FExistentes: string;
    FInstalling: boolean;
    FPlano: string;
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
  RALInst.Processo, RALInst.Tela.Mensagens, RALInst.Tela.Principal;

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
  finally
    Screen.Cursor := crDefault;
    AoEsperarProcesso := nil;
    FInstalling := False;
  end;
end;

procedure TTelaInstalar.lbNextClick(Sender: TObject);
var
  vOk: boolean;
  vLog: string;
begin
  if FInstalling then
    Exit;
  if FDesinstalar then
  begin
    lbDesinstalarClick(Sender);
    Exit;
  end;
  if MessageDlg(cmInstalarTitulo, cmInstalarPergunta, mtConfirmation,
       [mbYes, mbNo], 0) <> mrYes then
    Exit;

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

procedure TTelaInstalar.AoMostrar;
begin
  if FInstalling then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    FDesinstalar := TelaPrincipal.ModoDesinstalar;
    FPlano := TelaPrincipal.PlanoInstalacao;
  finally
    Screen.Cursor := crDefault;
  end;
  if FDesinstalar then
  begin
    lbNext.Caption := cmBotaoDesinstalar;
    mLogInstall.Lines.Text := cmOQueSeraFeito + LineEnding + LineEnding + FPlano +
                              LineEnding + cmCliqueDesinstalar;
  end
  else
  begin
    lbNext.Caption := cmBotaoInstalar;
    mLogInstall.Lines.Text := cmOQueSeraFeito + LineEnding + LineEnding + FPlano +
                              LineEnding + cmCliqueInstalar;
  end;
  // o que o instalador ja pos nestas IDEs; instalar de novo passa por cima,
  // desinstalar desfaz
  FExistentes := TelaPrincipal.InstalacoesExistentes;
  // no modo desinstalar o plano ja diz o que sai de cada IDE
  if (FExistentes <> '') and not FDesinstalar then
    mLogInstall.Lines.Add(LineEnding + cmJaInstalado + LineEnding + FExistentes);
  // no modo desinstalar o botao principal ja faz isso
  lbDesinstalar.Visible := (FExistentes <> '') and not FDesinstalar;
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
