/// Binary forms page (D-M6): convert them to text or leave them out of the
/// migration; skipped when the folder has none.
unit RALMig.Tela.Binarios;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms,
  RALMig.Tela.Pagina;

type
  /// Binary forms: convert to text or leave out.
  TTelaBinarios = class(TTelaMig)
    lbConverter: TLabel;
    lbDeixar: TLabel;
    lbExplicacao: TLabel;
    lbNota: TLabel;
    lbxBinarios: TListBox;
    rbConverter: TRadioButton;
    rbDeixar: TRadioButton;
    procedure rbConverterClick(Sender: TObject);
  public
    procedure AoMostrar; override;
    function AoAvancar: boolean; override;
    procedure AtualizarTextos; override;
    function Pular: boolean; override;
  end;

implementation

{$R *.lfm}

uses
  RALMig.Tela.Mensagens;

{ TTelaBinarios }

procedure TTelaBinarios.AoMostrar;
begin
  lbxBinarios.Items.Assign(Estado.Binarios);
  rbConverter.Checked := Estado.ConverterBinarios;
  rbDeixar.Checked := not Estado.ConverterBinarios;
end;

function TTelaBinarios.AoAvancar: boolean;
begin
  Estado.ConverterBinarios := rbConverter.Checked;
  Result := True;
end;

procedure TTelaBinarios.AtualizarTextos;
begin
  inherited AtualizarTextos;
  Titular(cmBinTitulo, cmBinSub);
  lbExplicacao.Caption := cmBinExplicacao;
  lbConverter.Caption := cmBinConverter;
  lbDeixar.Caption := cmBinDeixar;
  lbNota.Caption := cmBinNota;
end;

function TTelaBinarios.Pular: boolean;
begin
  Result := Estado.Binarios.Count = 0;
end;

procedure TTelaBinarios.rbConverterClick(Sender: TObject);
begin
  Estado.ConverterBinarios := rbConverter.Checked;
end;

end.
