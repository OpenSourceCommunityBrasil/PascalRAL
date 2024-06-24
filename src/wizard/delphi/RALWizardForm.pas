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
    procedure bCriarAplicacaoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FUnitIdent: string;
    FClassName: string;
    FFileName: string;
  public
    { Public declarations }
  end;

var
  fRALWizardForm: TfRALWizardForm;

implementation

{$R *.dfm}

uses
  RALWizardTools, RALWizardProjStandAlone, RALWizardProjConsole;

procedure TfRALWizardForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Release;
  Action := caFree;
end;

procedure TfRALWizardForm.bCriarAplicacaoClick(Sender: TObject);
var
  vModuleServices: IOTAModuleServices;
  vProjectName: string;
  vProjectDir: String;
  vOpcoes: integer;
begin
  vModuleServices := (BorlandIDEServices as IOTAModuleServices);
  vProjectName := FindNewProjectName(GetActiveProjectGroup);
  vProjectDir := IncludeTrailingPathDelimiter(GetIDEProjectPath);

  vOpcoes := 0;
  if ckSwagger.Checked then
    vOpcoes := vOpcoes + 1;
  if ckWebModule.Checked then
    vOpcoes := vOpcoes + 2;

  case cbTipoAplicacao.ItemIndex of
    0 : begin
      vModuleServices.CreateModule(TRALWizardProjStandAloneCreator.Create(vProjectName,
                                   vProjectDir, cbTipoMotor.ItemIndex,
                                   cbAutenticacao.ItemIndex, vOpcoes));
    end;
    1 : begin


    end;
    2 : begin
      vModuleServices.CreateModule(TRALWizardProjConsoleCreator.Create(vProjectName,
                                   vProjectDir, cbTipoMotor.ItemIndex,
                                   cbAutenticacao.ItemIndex, vOpcoes));
    end;
  end;

  Close;
end;

end.
