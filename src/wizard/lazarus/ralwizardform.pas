unit ralwizardform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, EditBtn;

type

  { Tfralwizardform }

  Tfralwizardform = class(TForm)
    bCriarAplicacao: TSpeedButton;
    Bevel1: TBevel;
    cbTipoAplicacao: TComboBox;
    cbTipoMotor: TComboBox;
    cbAutenticacao: TComboBox;
    ckSwagger: TCheckBox;
    ckWebModule: TCheckBox;
    eDirAplicacao: TDirectoryEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    imLogo: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure bCriarAplicacaoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FResModal : TModalResult;
  public

  end;

var
  fralwizardform: Tfralwizardform;

implementation

{$R *.lfm}

{ Tfralwizardform }

procedure Tfralwizardform.bCriarAplicacaoClick(Sender: TObject);
begin
  if Trim(eDirAplicacao.Text) = '' then
  begin
    ShowMessage('Diretório deve ser preenchido');
    eDirAplicacao.SetFocus;
    Exit;
  end;

  if not DirectoryExists(eDirAplicacao.Text) then
  begin
    ShowMessage('Diretório não Existe');
    eDirAplicacao.SetFocus;
    Exit;
  end;

  FResModal := mrOK;
  Close;
end;

procedure Tfralwizardform.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  ModalResult := FResModal;
  CloseAction := caFree;
  Release;
end;

procedure Tfralwizardform.FormCreate(Sender: TObject);
begin
  FResModal := mrCancel;
end;

end.

