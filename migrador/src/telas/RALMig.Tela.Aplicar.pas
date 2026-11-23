/// Apply page: the safety copy, the git warning and the confirmation; the only
/// page that writes to the disk.
unit RALMig.Tela.Aplicar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, Dialogs, Graphics,
  RALMig.Tela.Pagina;

type
  /// The only page that writes to the disk.
  TTelaAplicar = class(TTelaMig)
    ckCopia: TCheckBox;
    lbAvisoGit: TLabel;
    lbAvisoSemCopia: TLabel;
    lbCopia: TLabel;
    lbInfoCopia: TLabel;
    lbResumo: TLabel;
    procedure ckCopiaClick(Sender: TObject);
  private
    procedure MostrarAvisos;
  public
    procedure AoMostrar; override;
    function AoAvancar: boolean; override;
    procedure AtualizarTextos; override;
    function RotuloAvancar: string; override;
  end;

implementation

{$R *.lfm}

uses
  RALMig.Copia, RALMig.Mensagens, RALMig.Tela.Mensagens;

{ TTelaAplicar }

procedure TTelaAplicar.AoMostrar;
begin
  ckCopia.Checked := Estado.FazerCopia;
  MostrarAvisos;
end;

function TTelaAplicar.AoAvancar: boolean;
var
  vQuantos: integer;
  vErro, vPergunta: string;
begin
  Result := False;
  vQuantos := Estado.ContarAlterados;
  Estado.FazerCopia := ckCopia.Checked;
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

procedure TTelaAplicar.AtualizarTextos;
begin
  inherited AtualizarTextos;
  Titular(cmAplicarTitulo, cmAplicarSub);
  lbCopia.Caption := cmFazerCopia;
  if Estado.Pasta <> '' then
    MostrarAvisos;
end;

procedure TTelaAplicar.ckCopiaClick(Sender: TObject);
begin
  Estado.FazerCopia := ckCopia.Checked;
  lbAvisoSemCopia.Visible := not ckCopia.Checked;
end;

procedure TTelaAplicar.MostrarAvisos;
var
  vQuantos: integer;
  vDetalhe: string;
begin
  vQuantos := Estado.ContarAlterados;
  if vQuantos = 0 then
    lbResumo.Caption := cmAplicarNada
  else
    lbResumo.Caption := Format(cmAplicarResumo, [vQuantos, Estado.PastaProjeto]);
  lbInfoCopia.Caption := Format(cmCopiaOnde, [Estado.PastaProjeto + PASTA_COPIAS]);
  lbAvisoSemCopia.Caption := cmSemCopiaAviso;
  lbAvisoSemCopia.Visible := not ckCopia.Checked;
  // o git so interessa quando alguma coisa vai ser gravada
  lbAvisoGit.Visible := (vQuantos > 0) and GitComMudancas(Estado.PastaProjeto, vDetalhe);
  if lbAvisoGit.Visible then
    lbAvisoGit.Caption := Format(wmGitComMudancas, [vDetalhe]);
end;

function TTelaAplicar.RotuloAvancar: string;
begin
  Result := cmAplicar;
end;

end.
