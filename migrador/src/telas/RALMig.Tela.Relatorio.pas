/// Report page: what was done, where the report files are, open the HTML, and
/// the undo.
unit RALMig.Tela.Relatorio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, Dialogs, Graphics,
  RALMig.Tela.Pagina;

type
  /// What was done, where the report is, and the undo.
  TTelaRelatorio = class(TTelaMig)
    lbAbrir: TLabel;
    lbCopia: TLabel;
    lbDesfazer: TLabel;
    lbHtml: TLabel;
    lbMd: TLabel;
    lbNota: TLabel;
    lbResumo: TLabel;
    procedure lbAbrirClick(Sender: TObject);
    procedure lbDesfazerClick(Sender: TObject);
  private
    procedure Preencher;
  public
    procedure AoMostrar; override;
    procedure AtualizarTextos; override;
    function PodeVoltar: boolean; override;
    function RotuloAvancar: string; override;
  end;

implementation

{$R *.lfm}

uses
  LCLIntf, RALMig.Tela.Mensagens;

{ TTelaRelatorio }

procedure TTelaRelatorio.AoMostrar;
begin
  Preencher;
end;

procedure TTelaRelatorio.AtualizarTextos;
begin
  inherited AtualizarTextos;
  Titular(cmRelTitulo, cmRelSub);
  lbAbrir.Caption := cmAbrirRelatorio;
  lbDesfazer.Caption := cmDesfazer;
  lbNota.Caption := cmRelNota;
  Preencher;
end;

procedure TTelaRelatorio.lbAbrirClick(Sender: TObject);
begin
  if lbAbrir.Enabled and FileExists(Estado.RelatorioHtml) then
    OpenDocument(Estado.RelatorioHtml);
end;

procedure TTelaRelatorio.lbDesfazerClick(Sender: TObject);
var
  vMensagem: string;
  vOk: boolean;
begin
  if not lbDesfazer.Enabled then
    Exit;
  if not Confirmar(cmDesfazerTitulo, cmDesfazerPergunta) then
    Exit;
  vOk := Estado.DesfazerUltima(vMensagem);
  if vOk then
    MessageDlg(cmDesfazerTitulo, vMensagem, mtInformation, [mbOK], 0)
  else
    MessageDlg(cmDesfazerTitulo, vMensagem, mtError, [mbOK], 0);
  Preencher;
end;

function TTelaRelatorio.PodeVoltar: boolean;
begin
  // voltar para aplicar de novo so confundiria: o que foi feito esta feito
  Result := False;
end;

procedure TTelaRelatorio.Preencher;
var
  vCopia: string;
begin
  if not Estado.Gravado then
  begin
    lbResumo.Caption := cmRelNaoAplicada;
    lbMd.Caption := '';
    lbHtml.Caption := '';
    lbCopia.Caption := '';
  end
  else
  begin
    lbResumo.Caption := Estado.Resumo;
    if Estado.SemPassos then
      lbMd.Caption := cmRelSemPassos
    else if Estado.RelatorioMantido then
      lbMd.Caption := Format(cmRelMantido, [Estado.RelatorioMd])
    else
      lbMd.Caption := Format(cmRelMd, [Estado.RelatorioMd]);
    lbHtml.Caption := Format(cmRelHtml, [Estado.RelatorioHtml]);
    vCopia := Estado.Motor.ZipCopia;
    if vCopia <> '' then
      lbCopia.Caption := Format(cmRelCopia, [vCopia])
    else
      lbCopia.Caption := cmRelSemCopia;
  end;
  lbAbrir.Visible := Estado.Gravado and FileExists(Estado.RelatorioHtml);
  lbDesfazer.Visible := Estado.ContarCopias > 0;
end;

function TTelaRelatorio.RotuloAvancar: string;
begin
  Result := cmConferir;
end;

end.
