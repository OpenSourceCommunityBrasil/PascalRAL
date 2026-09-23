unit ufrm_install;

{$mode ObjFPC}{$H+}

// Ultima tela: mostra o plano (o que sera feito em cada IDE marcada) antes de
// tocar em qualquer coisa, pede confirmacao e executa. O log da rodada vai
// tambem para um arquivo, que e o que se manda quando algo da errado.

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ufrm_modelo;

type

  { Tfrm_install }

  Tfrm_install = class(Tfrm_modelo)
    lbDesinstalar: TLabel;
    lbSubTitle: TLabel;
    mLogInstall: TMemo;
    procedure lbDesinstalarClick(Sender: TObject);
    procedure lbNextClick(Sender: TObject);
  private
    FInstalling : boolean;
    FPlano: string;
    FExistentes: string;
    function SalvarLog(const APrefixo: string): string;
  protected
    function validatePagePrior : boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    // ao entrar na tela: o plano, antes de qualquer escrita
    procedure AoMostrar; override;
    // uma linha no log da rodada (a etapa de download escreve aqui)
    procedure LogarLinha(const ALinha: string);
  end;

implementation

{$R *.lfm}

uses
  umain, RALInst.Processo;

procedure ProcessarMensagens;
begin
  Application.ProcessMessages;
end;

{ Tfrm_install }

procedure Tfrm_install.LogarLinha(const ALinha: string);
begin
  mLogInstall.Lines.Add(ALinha);
  Application.ProcessMessages;
end;

procedure Tfrm_install.AoMostrar;
begin
  if FInstalling then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    FPlano := fmain.PlanoInstalacao;
  finally
    Screen.Cursor := crDefault;
  end;
  mLogInstall.Lines.Text := 'O que será feito:' + LineEnding + LineEnding + FPlano +
                            LineEnding + 'Clique em Instalar para executar.';
  // F10: o que o instalador ja pos nestas IDEs; instalar de novo passa por
  // cima, desinstalar desfaz
  FExistentes := fmain.InstalacoesExistentes;
  if FExistentes <> '' then
    mLogInstall.Lines.Add(LineEnding + 'Já instalado pelo instalador nestas IDEs ' +
      '(instalar de novo passa por cima; "Desinstalar" desfaz):' + LineEnding + FExistentes);
  lbDesinstalar.Visible := FExistentes <> '';
  mLogInstall.SelStart := 0;
end;

procedure Tfrm_install.lbDesinstalarClick(Sender: TObject);
var
  vOk: boolean;
  vLog: string;
begin
  if FInstalling or (FExistentes = '') then
    Exit;
  if MessageDlg('Desinstalar o PascalRAL',
       'Desfazer o que o instalador fez nestas IDEs:' + LineEnding + LineEnding + FExistentes +
       LineEnding + 'O registro, o library path e a configuração voltam ao que eram antes; ' +
       'os .bpl gravados são apagados e o Lazarus é reconstruído sem os pacotes. As ' +
       'dependências que já estavam instaladas não são tocadas. Feche as IDEs antes.' +
       LineEnding + LineEnding + 'Desinstalar agora?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  FInstalling := True;
  AoEsperarProcesso := @ProcessarMensagens;
  Screen.Cursor := crHourGlass;
  try
    mLogInstall.Clear;
    vOk := fmain.DesinstalarRAL;
    vLog := SalvarLog('desinstalacao');
    mLogInstall.Lines.Add('');
    if vOk then
      mLogInstall.Lines.Add('Desinstalação concluída.')
    else
      mLogInstall.Lines.Add('Desinstalação terminou com erros — veja as linhas ERRO acima.');
    if vLog <> '' then
    begin
      mLogInstall.Lines.Add('Log: ' + vLog);
      mLogInstall.Lines.SaveToFile(vLog);
    end;
    FExistentes := fmain.InstalacoesExistentes;
    lbDesinstalar.Visible := FExistentes <> '';
  finally
    Screen.Cursor := crDefault;
    AoEsperarProcesso := nil;
    FInstalling := False;
  end;
end;

function Tfrm_install.SalvarLog(const APrefixo: string): string;
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

procedure Tfrm_install.lbNextClick(Sender: TObject);
var
  vOk: boolean;
  vLog: string;
begin
  if FInstalling then
    Exit;

  if MessageDlg('Instalar o PascalRAL',
       'As IDEs marcadas terão a configuração alterada (pacotes, library path e, ' +
       'no Delphi, o registro). Feche-as antes de continuar.' + LineEnding + LineEnding +
       'Instalar agora?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  FInstalling := True;
  AoEsperarProcesso := @ProcessarMensagens;
  Screen.Cursor := crHourGlass;
  try
    mLogInstall.Clear;
    mLogInstall.Lines.Add('Plano:');
    mLogInstall.Lines.Add(TrimRight(FPlano));
    mLogInstall.Lines.Add('');
    Application.ProcessMessages;

    vOk := fmain.installRAL(mLogInstall);

    vLog := SalvarLog('instalacao');
    mLogInstall.Lines.Add('');
    // o que acabou de ser instalado passa a poder ser desfeito daqui
    FExistentes := fmain.InstalacoesExistentes;
    lbDesinstalar.Visible := FExistentes <> '';
    if vOk then
      mLogInstall.Lines.Add('Instalação concluída.')
    else
      mLogInstall.Lines.Add('Instalação terminou com erros — veja as linhas ERRO acima.');
    if vLog <> '' then
    begin
      mLogInstall.Lines.Add('Log: ' + vLog);
      mLogInstall.Lines.SaveToFile(vLog);
    end;
  finally
    Screen.Cursor := crDefault;
    AoEsperarProcesso := nil;
    FInstalling := False;
  end;
end;

function Tfrm_install.validatePagePrior: boolean;
begin
  Result := not FInstalling;
end;

constructor Tfrm_install.Create(AOwner: TComponent);
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

end.
