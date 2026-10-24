/// Apply page (safety copy, git warning, confirmation) and report page (the
/// files written, open the HTML, undo).
unit RALMig.Tela.Aplicar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, Dialogs, Graphics,
  RALMig.Tela.Estado, RALMig.Tela.Modelo;

type
  { TPaginaAplicar }

  /// The only page that writes to the disk.
  TPaginaAplicar = class(TPaginaMig)
  private
    FAvisoGit: TLabel;
    FAvisoSemCopia: TLabel;
    FCaixaCopia: TCheckBox;
    FInfoCopia: TLabel;
    FResumo: TLabel;
    procedure CopiaClick(Sender: TObject);
    procedure MostrarAvisos;
  public
    constructor Criar(AOwner: TComponent; AEstado: TEstadoMig); override;
    procedure AoMostrar; override;
    function AoAvancar: boolean; override;
    procedure AtualizarTextos; override;
    function RotuloAvancar: string; override;
  end;

  { TPaginaRelatorio }

  /// What was done, where the report is, and the undo.
  TPaginaRelatorio = class(TPaginaMig)
  private
    FBtnAbrir: TBotaoMig;
    FBtnDesfazer: TBotaoMig;
    FCopia: TLabel;
    FHtml: TLabel;
    FMd: TLabel;
    FNota: TLabel;
    FResumo: TLabel;
    procedure AbrirClick(Sender: TObject);
    procedure DesfazerClick(Sender: TObject);
    procedure Preencher;
  public
    constructor Criar(AOwner: TComponent; AEstado: TEstadoMig); override;
    procedure AoMostrar; override;
    procedure AtualizarTextos; override;
    function PodeVoltar: boolean; override;
    function RotuloAvancar: string; override;
  end;

implementation

uses
  LCLIntf, RALMig.Copia, RALMig.Mensagens, RALMig.Tela.Mensagens;

const
  COR_AVISO = $00AAFF; // laranja, legivel nos dois temas (BGR)

{ TPaginaAplicar }

constructor TPaginaAplicar.Criar(AOwner: TComponent; AEstado: TEstadoMig);
begin
  inherited Criar(AOwner, AEstado);
  FResumo := NovoRotulo(MARGEM, 190, LARGURA_UTIL, 11, True);
  Colocar(FResumo, MARGEM, 190, LARGURA_UTIL, 48);
  FCaixaCopia := NovaCaixa(MARGEM, 248, LARGURA_UTIL);
  FCaixaCopia.Checked := True;
  FCaixaCopia.OnClick := @CopiaClick;
  FInfoCopia := NovoRotulo(MARGEM + 24, 276, LARGURA_UTIL - 24);
  Colocar(FInfoCopia, MARGEM + 24, 276, LARGURA_UTIL - 24, 44);
  FAvisoGit := NovoRotulo(MARGEM, 332, LARGURA_UTIL);
  Colocar(FAvisoGit, MARGEM, 332, LARGURA_UTIL, 64);
  FAvisoGit.Tag := -1;
  FAvisoGit.Font.Color := COR_AVISO;
  FAvisoSemCopia := NovoRotulo(MARGEM, 408, LARGURA_UTIL);
  Colocar(FAvisoSemCopia, MARGEM, 408, LARGURA_UTIL, 48);
  FAvisoSemCopia.Tag := -1;
  FAvisoSemCopia.Font.Color := COR_AVISO;
end;

procedure TPaginaAplicar.AoMostrar;
begin
  FCaixaCopia.Checked := Estado.FazerCopia;
  MostrarAvisos;
end;

function TPaginaAplicar.AoAvancar: boolean;
var
  vQuantos: integer;
  vErro, vPergunta: string;
begin
  Result := False;
  vQuantos := Estado.ContarAlterados;
  Estado.FazerCopia := FCaixaCopia.Checked;
  if Estado.FazerCopia then
    vPergunta := Format(cmConfirmarCom, [vQuantos, Estado.PastaProjeto])
  else
    vPergunta := Format(cmConfirmarSem, [vQuantos]);
  if not Confirmar(cmConfirmarTitulo, vPergunta) then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    Result := Estado.Aplicar(vErro);
  finally
    Screen.Cursor := crDefault;
  end;
  if not Result then
    MessageDlg(cmConfirmarTitulo, Format(emTelaGravar, [vErro]), mtError, [mbOK], 0);
end;

procedure TPaginaAplicar.AtualizarTextos;
begin
  Titular(cmAplicarTitulo, cmAplicarSub);
  FCaixaCopia.Caption := cmFazerCopia;
  if Estado.Pasta <> '' then
    MostrarAvisos;
end;

procedure TPaginaAplicar.CopiaClick(Sender: TObject);
begin
  Estado.FazerCopia := FCaixaCopia.Checked;
  FAvisoSemCopia.Visible := not FCaixaCopia.Checked;
end;

procedure TPaginaAplicar.MostrarAvisos;
var
  vQuantos: integer;
  vDetalhe: string;
begin
  vQuantos := Estado.ContarAlterados;
  if vQuantos = 0 then
    FResumo.Caption := cmAplicarNada
  else
    FResumo.Caption := Format(cmAplicarResumo, [vQuantos, Estado.PastaProjeto]);
  FInfoCopia.Caption := Format(cmCopiaOnde, [Estado.PastaProjeto + PASTA_COPIAS]);
  FAvisoSemCopia.Caption := cmSemCopiaAviso;
  FAvisoSemCopia.Visible := not FCaixaCopia.Checked;
  // o git so interessa quando alguma coisa vai ser gravada
  FAvisoGit.Visible := (vQuantos > 0) and GitComMudancas(Estado.PastaProjeto, vDetalhe);
  if FAvisoGit.Visible then
    FAvisoGit.Caption := Format(wmGitComMudancas, [vDetalhe]);
end;

function TPaginaAplicar.RotuloAvancar: string;
begin
  Result := cmAplicar;
end;

{ TPaginaRelatorio }

constructor TPaginaRelatorio.Criar(AOwner: TComponent; AEstado: TEstadoMig);
begin
  inherited Criar(AOwner, AEstado);
  FResumo := NovoRotulo(MARGEM, 190, LARGURA_UTIL, 11, True);
  Colocar(FResumo, MARGEM, 190, LARGURA_UTIL, 56);
  FMd := NovoRotulo(MARGEM, 258, LARGURA_UTIL);
  FHtml := NovoRotulo(MARGEM, 286, LARGURA_UTIL);
  FCopia := NovoRotulo(MARGEM, 314, LARGURA_UTIL);
  FBtnAbrir := NovoBotao(MARGEM, 372, 280, 36);
  FBtnAbrir.OnClick := @AbrirClick;
  FBtnDesfazer := NovoBotao(MARGEM + 296, 372, 280, 36);
  FBtnDesfazer.OnClick := @DesfazerClick;
  FNota := NovoRotulo(MARGEM, 432, LARGURA_UTIL);
  Colocar(FNota, MARGEM, 432, LARGURA_UTIL, 48);
end;

procedure TPaginaRelatorio.AbrirClick(Sender: TObject);
begin
  if FBtnAbrir.Enabled and FileExists(Estado.RelatorioHtml) then
    OpenDocument(Estado.RelatorioHtml);
end;

procedure TPaginaRelatorio.AoMostrar;
begin
  Preencher;
end;

procedure TPaginaRelatorio.AtualizarTextos;
begin
  Titular(cmRelTitulo, cmRelSub);
  FBtnAbrir.Texto := cmAbrirRelatorio;
  FBtnDesfazer.Texto := cmDesfazer;
  FNota.Caption := cmRelNota;
  Preencher;
end;

procedure TPaginaRelatorio.DesfazerClick(Sender: TObject);
var
  vMensagem: string;
  vOk: boolean;
begin
  if not FBtnDesfazer.Enabled then
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

function TPaginaRelatorio.PodeVoltar: boolean;
begin
  // voltar para aplicar de novo so confundiria: o que foi feito esta feito
  Result := False;
end;

procedure TPaginaRelatorio.Preencher;
var
  vCopia: string;
begin
  if not Estado.Gravado then
  begin
    FResumo.Caption := cmRelNaoAplicada;
    FMd.Caption := '';
    FHtml.Caption := '';
    FCopia.Caption := '';
  end
  else
  begin
    FResumo.Caption := Estado.Resumo;
    if Estado.RelatorioMantido then
      FMd.Caption := Format(cmRelMantido, [Estado.RelatorioMd])
    else
      FMd.Caption := Format(cmRelMd, [Estado.RelatorioMd]);
    FHtml.Caption := Format(cmRelHtml, [Estado.RelatorioHtml]);
    vCopia := Estado.Motor.ZipCopia;
    if vCopia <> '' then
      FCopia.Caption := Format(cmRelCopia, [vCopia])
    else
      FCopia.Caption := cmRelSemCopia;
  end;
  FBtnAbrir.Visible := Estado.Gravado and FileExists(Estado.RelatorioHtml);
  FBtnDesfazer.Visible := Estado.ContarCopias > 0;
end;

function TPaginaRelatorio.RotuloAvancar: string;
begin
  Result := cmConferir;
end;

end.
