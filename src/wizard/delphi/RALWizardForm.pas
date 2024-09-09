unit RALWizardForm;

{$I ..\..\src\base\PascalRAL.inc}

interface
                        
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, FileCtrl;

type
  TfRALWizardForm = class(TForm)
    Panel1: TPanel;
    imLogo: TImage;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    Label1: TLabel;
    Panel7: TPanel;
    Panel8: TPanel;
    Label2: TLabel;
    eDirAplicacao: TEdit;
    bDirectory: TSpeedButton;
    Panel9: TPanel;
    cbTipoAplicacao: TComboBox;
    Label3: TLabel;
    Panel10: TPanel;
    cbTipoMotor: TComboBox;
    Label4: TLabel;
    Panel11: TPanel;
    cbAutenticacao: TComboBox;
    Panel5: TPanel;
    Bevel1: TBevel;
    bCriarAplicacao: TSpeedButton;
    GroupBox1: TGroupBox;
    ckSwagger: TCheckBox;
    ckWebModule: TCheckBox;
    Image1: TImage;
    procedure bCriarAplicacaoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bDirectoryClick(Sender: TObject);
    procedure cbTipoAplicacaoSelect(Sender: TObject);
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

  FResModal := mrOk;
  Close;
end;

procedure TfRALWizardForm.FormShow(Sender: TObject);
begin
  eDirAplicacao.Text := GetIDEProjectPath;
end;

procedure TfRALWizardForm.FormCreate(Sender: TObject);
begin
  FResModal := mrCancel;
end;

procedure TfRALWizardForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  ModalResult := FResModal;
  Release;
end;

procedure TfRALWizardForm.bDirectoryClick(Sender: TObject);
var
  vDir : string;
begin
  {$IFDEF DELPHI10_0UP}
  if SelectDirectory('Select Directory', ExtractFileDrive(vDir), vDir,
                     [sdNewUI, sdNewFolder]) then
  {$ELSE}
  if SelectDirectory(vDir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
  {$ENDIF}
    eDirAplicacao.Text := vDir;
end;

procedure TfRALWizardForm.cbTipoAplicacaoSelect(Sender: TObject);
begin
  if cbTipoAplicacao.ItemIndex = 1 then begin
    cbTipoMotor.ItemIndex := -1;
    cbAutenticacao.ItemIndex := -1;
    ckWebModule.Checked := False;

    cbTipoMotor.Enabled := False;
    cbAutenticacao.Enabled := False;
    ckWebModule.Enabled := False;
  end
  else begin
    if cbTipoMotor.ItemIndex < 0 then
      cbTipoMotor.ItemIndex := 0;
    if cbAutenticacao.ItemIndex < 0 then
      cbAutenticacao.ItemIndex := 0;

    cbTipoMotor.Enabled := True;
    cbAutenticacao.Enabled := True;
    ckWebModule.Enabled := True;
  end
end;

end.
