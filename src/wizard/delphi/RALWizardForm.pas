/// The options form of the Delphi wizard.
unit RALWizardForm;

{$I ..\..\core\PascalRAL.inc}

interface
                        
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, FileCtrl;

type
  /// The options of the new application.
  TfRALWizardForm = class(TForm)
    /// Layout panel.
    Panel1: TPanel;
    /// The RAL logo.
    imLogo: TImage;
    /// Layout panel.
    Panel2: TPanel;
    /// Layout panel.
    Panel3: TPanel;
    /// Layout panel.
    Panel4: TPanel;
    /// Layout panel.
    Panel6: TPanel;
    /// Caption of the folder.
    Label1: TLabel;
    /// Layout panel.
    Panel7: TPanel;
    /// Layout panel.
    Panel8: TPanel;
    /// Caption of the application type.
    Label2: TLabel;
    /// Folder of the new application.
    eDirAplicacao: TEdit;
    /// Picks the folder.
    bDirectory: TSpeedButton;
    /// Layout panel.
    Panel9: TPanel;
    /// Type of the application: stand-alone, console, CGI.
    cbTipoAplicacao: TComboBox;
    /// Caption of the engine.
    Label3: TLabel;
    /// Layout panel.
    Panel10: TPanel;
    /// Engine of the server.
    cbTipoMotor: TComboBox;
    /// Caption of the authentication.
    Label4: TLabel;
    /// Layout panel.
    Panel11: TPanel;
    /// Authentication of the server.
    cbAutenticacao: TComboBox;
    /// Layout panel.
    Panel5: TPanel;
    /// Separator.
    Bevel1: TBevel;
    /// Creates the application.
    bCriarAplicacao: TSpeedButton;
    /// The optional modules.
    GroupBox1: TGroupBox;
    /// Adds the Swagger module.
    ckSwagger: TCheckBox;
    /// Adds the web module.
    ckWebModule: TCheckBox;
    /// Decorative image.
    Image1: TImage;
    /// Checks the options and closes the form with mrOk.
    procedure bCriarAplicacaoClick(Sender: TObject);
    /// Proposes the projects folder of the IDE.
    procedure FormShow(Sender: TObject);
    /// Starts with mrCancel as the result.
    procedure FormCreate(Sender: TObject);
    /// Frees the form and closes with the result set.
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    /// Picks the folder of the application.
    procedure bDirectoryClick(Sender: TObject);
    /// Enables the options of the chosen type.
    procedure cbTipoAplicacaoSelect(Sender: TObject);
  private
    /// Private declarations.
    FResModal : TModalResult;
  public
    { Public declarations }
  end;

var
  /// The form instance.
  fRALWizardForm: TfRALWizardForm;

implementation

{$R *.dfm}

uses
  RALConsts, RALWizardTools;

procedure TfRALWizardForm.bCriarAplicacaoClick(Sender: TObject);
begin
  if Trim(eDirAplicacao.Text) = '' then
  begin
    ShowMessage(emWizardDirEmpty);
    eDirAplicacao.SetFocus;
    Exit;
  end;

  if not DirectoryExists(eDirAplicacao.Text) then
  begin
    ShowMessage(emWizardDirNotFound);
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
