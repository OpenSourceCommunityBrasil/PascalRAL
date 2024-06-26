unit RALWizardForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.Buttons, ToolsAPI;

type
  TfRALWizardForm = class(TForm)
    Panel1: TPanel;
    imLogo: TImage;
    Panel2: TPanel;
    Panel3: TPanel;
    Bevel1: TBevel;
    bCriarAplicacao: TSpeedButton;
    gbOpcoes: TGroupBox;
    ckSwagger: TCheckBox;
    ckWebModule: TCheckBox;
    Label1: TLabel;
    cbTipoAplicacao: TComboBox;
    Label2: TLabel;
    cbTipoMotor: TComboBox;
    Label3: TLabel;
    cbAutenticacao: TComboBox;
    Image1: TImage;
    Label4: TLabel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    eDirAplicacao: TEdit;
    bDirectory: TSpeedButton;
    OpenDialog: TOpenDialog;
    procedure bCriarAplicacaoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure bDirectoryClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FResModal : TModalResult;
  public
    { Public declarations }
  end;

var
  fRALWizardForm: TfRALWizardForm;

implementation

{$R *.dfm}

uses
  RALWizardTools;

procedure TfRALWizardForm.bDirectoryClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    eDirAplicacao.Text := OpenDialog.FileName;
end;

procedure TfRALWizardForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  ModalResult := FResModal;
  Release;
end;

procedure TfRALWizardForm.FormCreate(Sender: TObject);
begin
  FResModal := mrCancel;
end;

procedure TfRALWizardForm.FormShow(Sender: TObject);
begin
  eDirAplicacao.Text := GetIDEProjectPath;
end;

procedure TfRALWizardForm.bCriarAplicacaoClick(Sender: TObject);
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

  if cbTipoAplicacao.ItemIndex = 1 then
  begin
    ShowMessage('Aplicação CGI - em desenvolvimento');
    cbTipoAplicacao.SetFocus;
    Exit;
  end;

  FResModal := mrOk;
  Close;
end;

end.
