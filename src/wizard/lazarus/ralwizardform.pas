/// The options form of the Lazarus wizard.
unit ralwizardform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, EditBtn, RALConsts;

type

  /// The options of the new application.
  Tfralwizardform = class(TForm)
    /// Creates the application.
    bCriarAplicacao: TSpeedButton;
    /// Separator.
    Bevel1: TBevel;
    /// Type of the application: stand-alone, console, CGI.
    cbTipoAplicacao: TComboBox;
    /// Engine of the server.
    cbTipoMotor: TComboBox;
    /// Authentication of the server.
    cbAutenticacao: TComboBox;
    /// Adds the Swagger module.
    ckSwagger: TCheckBox;
    /// Adds the web module.
    ckWebModule: TCheckBox;
    /// Folder of the new application.
    eDirAplicacao: TDirectoryEdit;
    /// The optional modules.
    GroupBox1: TGroupBox;
    /// Decorative image.
    Image1: TImage;
    /// The RAL logo.
    imLogo: TImage;
    /// Caption of the folder.
    Label1: TLabel;
    /// Caption of the application type.
    Label2: TLabel;
    /// Caption of the engine.
    Label3: TLabel;
    /// Caption of the authentication.
    Label4: TLabel;
    /// Layout panel.
    Panel1: TPanel;
    /// Layout panel.
    Panel2: TPanel;
    /// Layout panel.
    Panel3: TPanel;
    /// Checks the options and closes the form with mrOk.
    procedure bCriarAplicacaoClick(Sender: TObject);
    /// Enables the options of the chosen type.
    procedure cbTipoAplicacaoSelect(Sender: TObject);
    /// Frees the form and closes with the result set.
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    /// Starts with mrCancel as the result.
    procedure FormCreate(Sender: TObject);
  private
    /// The result the form closes with.
    FResModal : TModalResult;
  public

  end;

var
  /// The form instance.
  fralwizardform: Tfralwizardform;

implementation

{$R *.lfm}

{ Tfralwizardform }

procedure Tfralwizardform.bCriarAplicacaoClick(Sender: TObject);
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

  FResModal := mrOK;
  Close;
end;

procedure Tfralwizardform.cbTipoAplicacaoSelect(Sender: TObject);
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

