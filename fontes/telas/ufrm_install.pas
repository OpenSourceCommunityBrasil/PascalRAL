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
    lbSubTitle: TLabel;
    mLogInstall: TMemo;
    procedure lbNextClick(Sender: TObject);
  private
    FInstalling : boolean;
    FPlano: string;
    function SalvarLog: string;
  protected
    function validatePagePrior : boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    // chamado ao entrar na tela
    procedure MostrarPlano;
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

procedure Tfrm_install.MostrarPlano;
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
  mLogInstall.SelStart := 0;
end;

function Tfrm_install.SalvarLog: string;
var
  vPasta: string;
begin
  Result := '';
  vPasta := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'logs';
  try
    ForceDirectories(vPasta);
    Result := IncludeTrailingPathDelimiter(vPasta) +
              'instalacao-' + FormatDateTime('yyyymmdd-hhnnss', Now) + '.log';
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

    vLog := SalvarLog;
    mLogInstall.Lines.Add('');
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
