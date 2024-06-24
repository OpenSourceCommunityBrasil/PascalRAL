unit RALWizardFormCreator;

interface

uses
  Classes, SysUtils, ToolsAPI, Dialogs,
  RALWizardObjects;

type
  TRALWizardFormCreator = class(TNotifierObject, IOTACreator, IOTAModuleCreator)
  private
    FUnitIdent: string;
    FUnitIdentFrame: string;
    FClassName: string;
    FFileName: string;
    FClassNameFrame: string;
    FIsMainForm: Boolean;
    FOwner: IOTAModule;
  public
    { IOTACreator }
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    { IOTAModuleCreator }
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);

    constructor Create(AOwner: IOTAModule; AUnitIdent, AUnitIdentFrame, AClassName,
      AClassNameFrame, AFileName: string; AIsMainForm: Boolean = False);
  end;

  TRALWizardFormFile = class(TRALWizardBaseFile, IOTAFile)
  protected
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  TRALWizardFormUnitFile = class(TRALWizardBaseFile, IOTAFile)
  protected
    function GetSource: string;
    function GetAge: TDateTime;
  end;

implementation

uses
  RALWizardTools;

{ TRALWizardFormCreator }

constructor TRALWizardFormCreator.Create(AOwner: IOTAModule; AUnitIdent,
  AUnitIdentFrame, AClassName, AClassNameFrame, AFileName: string;
  AIsMainForm: Boolean);
begin
  inherited Create;
  FOwner := AOwner;
  FUnitIdent := AUnitIdent;
  FUnitIdentFrame := AUnitIdentFrame;
  FClassName := AClassName;
  FClassNameFrame := AClassNameFrame;
  FFileName := AFileName;
  FIsMainForm := AIsMainForm;
end;

procedure TRALWizardFormCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin

end;

function TRALWizardFormCreator.GetAncestorName: string;
begin
  Result := 'TForm';
end;

function TRALWizardFormCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TRALWizardFormCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TRALWizardFormCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TRALWizardFormCreator.GetFormName: string;
begin
  Result := FClassName;
end;

function TRALWizardFormCreator.GetImplFileName: string;
var
  vProjectDir: string;
Begin
  vProjectDir := IncludeTrailingPathDelimiter(GetIDEProjectPath);
  Result := MakeFileName(vProjectDir, FUnitIdent, 'pas');
end;

function TRALWizardFormCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TRALWizardFormCreator.GetMainForm: Boolean;
begin
  Result := FIsMainForm;
end;

function TRALWizardFormCreator.GetOwner: IOTAModule;
begin
  Result := FOwner;
end;

function TRALWizardFormCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TRALWizardFormCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TRALWizardFormCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TRALWizardFormCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TRALWizardFormFile.Create('', FormIdent, AncestorIdent, FClassName,
                                      FUnitIdent);
end;

function TRALWizardFormCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TRALWizardFormUnitFile.Create(ModuleIdent, FormIdent, AncestorIdent,
                                          FClassNameFrame, FUnitIdentFrame);
end;

function TRALWizardFormCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

{ TRALWizardFormFile }

function TRALWizardFormFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TRALWizardFormFile.GetSource: string;
var
  vFile : TStringList;
begin
  vFile := TStringList.Create;
  try
    vFile.Add(Format('object %s: T%s',[FormName, FormName]));
    vFile.Add('  Left = 0');
    vFile.Add('  Top = 0');
    vFile.Add('  Caption = ''RAL Application''');
    vFile.Add('  ClientHeight = 315');
    vFile.Add('  ClientWidth = 552');
    vFile.Add('  Color = clBtnFace');
    vFile.Add('  Font.Charset = DEFAULT_CHARSET');
    vFile.Add('  Font.Color = clWindowText');
    vFile.Add('  Font.Height = -11');
    vFile.Add('  Font.Name = ''Tahoma''');
    vFile.Add('  Font.Style = []');
    vFile.Add('  OldCreateOrder = False');
    vFile.Add('  OnCreate = FormCreate');
    vFile.Add('  PixelsPerInch = 96');
    vFile.Add('  TextHeight = 13');
    vFile.Add('  object server: TRALSynopseServer');
    vFile.Add('    Active = False');
    vFile.Add('    CompressType = ctNone');
    vFile.Add('    CookieLife = 30');
    vFile.Add('    CORSOptions.AllowHeaders.Strings = (');
    vFile.Add('      ''Content-Type''');
    vFile.Add('      ''Origin''');
    vFile.Add('      ''Accept''');
    vFile.Add('      ''Authorization''');
    vFile.Add('      ''Content-Encoding''');
    vFile.Add('      ''Accept-Encoding'')');
    vFile.Add('    CORSOptions.AllowOrigin = ''*''');
    vFile.Add('    CORSOptions.MaxAge = 86400');
    vFile.Add('    CriptoOptions.CriptType = crNone');
    vFile.Add('    IPConfig.IPv4Bind = ''0.0.0.0''');
    vFile.Add('    IPConfig.IPv6Bind = ''::''');
    vFile.Add('    IPConfig.IPv6Enabled = False');
    vFile.Add('    Port = 8000');
    vFile.Add('    Routes = <');
    vFile.Add('      item');
    vFile.Add('        Route = ''/ping''');
    vFile.Add('        InputParams = <>');
    vFile.Add('        AllowedMethods = [amGET]');
    vFile.Add('        AllowURIParams = False');
    vFile.Add('        Callback = False');
    vFile.Add('        Name = ''ping''');
    vFile.Add('        SkipAuthMethods = []');
    vFile.Add('        URIParams = <>');
    vFile.Add('        OnReply = serverRoutes_pingReply');
    vFile.Add('      end>');
    vFile.Add('    Security.BruteForce.ExpirationTime = 1800000');
    vFile.Add('    Security.BruteForce.MaxTry = 3');
    vFile.Add('    Security.FloodTimeInterval = 30');
    vFile.Add('    Security.Options = []');
    vFile.Add('    ShowServerStatus = True');
    vFile.Add('    PoolCount = 32');
    vFile.Add('    QueueSize = 1000');
    vFile.Add('    SSL.Enabled = False');
    vFile.Add('    Left = 208');
    vFile.Add('    Top = 112');
    vFile.Add('  end');
    vFile.Add('end');

    Result := vFile.Text;
  finally
    FreeAndNil(vFile);
  end;
end;

{ TRALWizardFormUnitFile }

function TRALWizardFormUnitFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TRALWizardFormUnitFile.GetSource: string;
var
  vFile : TStringList;
begin
  vFile := TStringList.Create;
  try
    vFile.Add(Format('unit %s;',[ModuleName]));
    vFile.Add('');
    vFile.Add('interface');
    vFile.Add('');
    vFile.Add('uses');
    vFile.Add('  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,');
    vFile.Add('  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,');
    vFile.Add('  RALCustomObjects, RALServer, RALSynopseServer, RALRequest, RALResponse,');
    vFile.Add('  RALMimeTypes;');
    vFile.Add('');
    vFile.Add('type');
    vFile.Add(Format('  T%s = class(%s)',[FormName, AncestorName]));
    vFile.Add('    server: TRALSynopseServer;');
    vFile.Add('    procedure FormCreate(Sender: TObject);');
    vFile.Add('    procedure serverRoutes_pingReply(ARequest: TRALRequest;');
    vFile.Add('                                     AResponse: TRALResponse);');
    vFile.Add('  private');
    vFile.Add('    { Private declarations }');
    vFile.Add('  public');
    vFile.Add('    { Public declarations }');
    vFile.Add('  end;');
    vFile.Add('');
    vFile.Add('var');
    vFile.Add(Format('  %s: T%s;',[FormName, FormName]));
    vFile.Add('');
    vFile.Add('implementation');
    vFile.Add('');
    vFile.Add('{$R *.dfm}');
    vFile.Add('');
    vFile.Add(Format('procedure T%s.FormCreate(Sender: TObject);',[FormName]));
    vFile.Add('begin');
    vFile.Add('  server.Active := True;');
    vFile.Add('end;');
    vFile.Add('');
    vFile.Add(Format('procedure T%s.serverRoutes_pingReply(ARequest: TRALRequest;',[FormName]));
    vFile.Add('  AResponse: TRALResponse);');
    vFile.Add('begin');
    vFile.Add('  AResponse.ContentType := rctTEXTPLAIN;');
    vFile.Add('  AResponse.ResponseText := ''pong'';');
    vFile.Add('end;');
    vFile.Add('');
    vFile.Add('end.');

    Result := vFile.Text;
  finally
    FreeAndNil(vFile);
  end;
end;

end.
