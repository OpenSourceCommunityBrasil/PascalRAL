unit RALWizardFormStandAlone;

interface

uses
  Classes, SysUtils, ToolsAPI, Dialogs,
  RALWizardObjects;

type
  { TRALWizardFormStandAloneCreator }

  TRALWizardFormStandAloneCreator = class(TNotifierObject, IOTACreator, IOTAModuleCreator)
  private
    FOwner: IOTAModule;
    FUnitName: string;
    FFormClass: string;
    FFileName: string;
    FEngine : integer;
    FAuth: integer;
    FOptions: integer;
    FIsMainForm: Boolean;
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

    constructor Create(AOwner: IOTAModule; AUnitName, AFormClass, AFileName : string;
                       AEngine, AAuth, AOptions : integer; AIsMainForm: Boolean = False);
  end;

  { TRALWizardFormStandAloneFile }

  TRALWizardFormStandAloneFile = class(TRALWizardBaseFile)
  protected
    function GetSource: string; override;
  end;

  { TRALWizardUnitStandAloneFile }

  TRALWizardUnitStandAloneFile = class(TRALWizardBaseFile)
  protected
    function GetSource: string; override;
  end;

implementation

uses
  RALWizardTools;

{ TRALWizardFormStandAloneCreator }

constructor TRALWizardFormStandAloneCreator.Create(AOwner: IOTAModule; AUnitName,
                       AFormClass, AFileName : string; AEngine, AAuth, AOptions : integer;
                       AIsMainForm: Boolean = False);
begin
  inherited Create;
  FOwner := AOwner;
  FUnitName := AUnitName;
  FFormClass := AFormClass;
  FFileName := AFileName;
  FEngine := AEngine;
  FAuth := AAuth;
  FOptions := AOptions;
  FIsMainForm := AIsMainForm;
end;

procedure TRALWizardFormStandAloneCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin

end;

function TRALWizardFormStandAloneCreator.GetAncestorName: string;
begin
  Result := 'TForm';
end;

function TRALWizardFormStandAloneCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TRALWizardFormStandAloneCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TRALWizardFormStandAloneCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TRALWizardFormStandAloneCreator.GetFormName: string;
begin
  Result := FFormClass;
end;

function TRALWizardFormStandAloneCreator.GetImplFileName: string;
var
  vProjectDir: string;
Begin
  vProjectDir := IncludeTrailingPathDelimiter(GetIDEProjectPath);
  Result := MakeFileName(vProjectDir, FUnitName, 'pas');
end;

function TRALWizardFormStandAloneCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TRALWizardFormStandAloneCreator.GetMainForm: Boolean;
begin
  Result := FIsMainForm;
end;

function TRALWizardFormStandAloneCreator.GetOwner: IOTAModule;
begin
  Result := FOwner;
end;

function TRALWizardFormStandAloneCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TRALWizardFormStandAloneCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TRALWizardFormStandAloneCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TRALWizardFormStandAloneCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TRALWizardFormStandAloneFile.Create('', FormIdent, AncestorIdent,
                                                FFormClass, FUnitName, FEngine,
                                                FAuth, FOptions);
end;

function TRALWizardFormStandAloneCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TRALWizardUnitStandAloneFile.Create(ModuleIdent, FormIdent, AncestorIdent,
                                                FFormClass, FUnitName, FEngine,
                                                FAuth, FOptions);
end;

function TRALWizardFormStandAloneCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

{ TRALWizardFormStandAloneFile }

function TRALWizardFormStandAloneFile.GetSource: string;
var
  vFile : TStringList;
  vClassServer : string;
begin
  vFile := TStringList.Create;
  try
    case Engine of
      0 : vClassServer := 'TRALIndyServer';
      1 : vClassServer := 'TRALSynopseServer';
      2 : vClassServer := 'TRALSaguiServer';
    end;

    vFile.Add(Format('object %s: T%s',[FormName, FormName]));
    vFile.Add('  Left = 0');
    vFile.Add('  Top = 0');
    vFile.Add('  Caption = ''RAL - StandAlone Application''');
    vFile.Add('  ClientHeight = 315');
    vFile.Add('  ClientWidth = 552');
    vFile.Add('  OldCreateOrder = False');
    vFile.Add('  OnCreate = FormCreate');
    vFile.Add('  PixelsPerInch = 96');
    vFile.Add('  TextHeight = 13');
    vFile.Add(Format('  object server: %s',[vClassServer]));
    if Auth = 1 then
      vFile.Add('    Authentication = authbasic')
    else if Auth = 2 then
      vFile.Add('    Authentication = authjwt');
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
    vFile.Add('    Left = 208');
    vFile.Add('    Top = 144');
    vFile.Add('  end');

    if Options and 1 > 0 then begin
      vFile.Add('  object swagger: TRALSwaggerModule');
      vFile.Add('    Server = server');
      vFile.Add('    Domain = ''/swagger''');
      vFile.Add('    PostmanTag = False');
      vFile.Add('    Left = 208');
      vFile.Add('    Top = 200');
      vFile.Add('  end');
    end;
    if Options and 2 > 0 then begin
      vFile.Add('  object webmodule: TRALWebModule');
      vFile.Add('    Server = server');
      vFile.Add('    Domain = ''/''');
      vFile.Add('    Routes = <>');
      vFile.Add('    Left = 256');
      vFile.Add('    Top = 200');
      vFile.Add('  end');
    end;

    if Auth = 1 then begin
      vFile.Add('  object authbasic: TRALServerBasicAuth');
      vFile.Add('    AuthDialog = True');
      vFile.Add('    Password = ''ralteste''');
      vFile.Add('    UserName = ''ralteste''');
      vFile.Add('    Left = 256');
      vFile.Add('    Top = 144');
      vFile.Add('  end');
    end
    else if Auth = 2 then begin
      vFile.Add('  object authjwt: TRALServerJWTAuth');
      vFile.Add('    Algorithm = tjaHSHA256');
      vFile.Add('    AuthRoute.Description.Strings = (');
      vFile.Add('      ''Get a JWT Token'')');
      vFile.Add('    AuthRoute.Route = ''/gettoken''');
      vFile.Add('    AuthRoute.InputParams = <>');
      vFile.Add('    ExpirationSecs = 1800');
      vFile.Add('    JSONKey = ''token''');
      vFile.Add('    Left = 256');
      vFile.Add('    Top = 144');
      vFile.Add('  end');
    end;
    vFile.Add('end');

    Result := vFile.Text;
  finally
    FreeAndNil(vFile);
  end;
end;

{ TRALWizardUnitStandAloneFile }

function TRALWizardUnitStandAloneFile.GetSource: string;
var
  vFile : TStringList;
  vUnitServer, vClassServer, vUnits : string;
begin
  vFile := TStringList.Create;
  try
    case Engine of
      0 : begin
        vUnitServer := 'RALIndyServer';
        vClassServer := 'TRALIndyServer';
      end;
      1 : begin
        vUnitServer := 'RALSynopseServer';
        vClassServer := 'TRALSynopseServer';
      end;
      2 : begin
        vUnitServer := 'RALSaguiServer';
        vClassServer := 'TRALSaguiServer';
      end;
    end;

    vUnits := 'RALMimeTypes';
    if Options and 1 > 0 then
      vUnits := vUnits + ', RALSwaggerModule';
    if Options and 2 > 0 then
      vUnits := vUnits + ', RALWebModule';
    if Auth > 0 then
      vUnits := vUnits + ', RALAuthentication';

    vFile.Add('// by PascalRAL - StandAlone App: '+DateTimeToStr(Now));
    vFile.Add(Format('unit %s;',[ModuleName]));
    vFile.Add('');
    vFile.Add('interface');
    vFile.Add('');
    vFile.Add('uses');
    vFile.Add('  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,');
    vFile.Add('  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,');
    vFile.Add(Format('  RALCustomObjects, RALServer, %s, RALRequest, RALResponse,',[vUnitServer]));
    vFile.Add(Format('  %s;', [vUnits]));
    vFile.Add('');
    vFile.Add('type');
    vFile.Add(Format('  T%s = class(%s)', [FormName, AncestorName]));
    vFile.Add(Format('    server: %s;', [vClassServer]));

    if Options and 1 > 0 then
      vFile.Add('    swagger: TRALSwaggerModule;');
    if Options and 2 > 0 then
      vFile.Add('    webmodule: TRALWebModule;');

    if Auth = 1 then
      vFile.Add('    authbasic: TRALServerBasicAuth;')
    else if Auth = 2 then
      vFile.Add('    authjwt: TRALServerJWTAuth;');

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
    vFile.Add('  server.Start;');
    vFile.Add('end;');
    vFile.Add('');
    vFile.Add(Format('procedure T%s.serverRoutes_pingReply(ARequest: TRALRequest;',[FormName]));
    vFile.Add('  AResponse: TRALResponse);');
    vFile.Add('begin');
    vFile.Add('  AResponse.Answer(200, ''pong'', rctTEXTPLAIN);');
    vFile.Add('end;');
    vFile.Add('');
    vFile.Add('end.');

    Result := vFile.Text;
  finally
    FreeAndNil(vFile);
  end;
end;

end.
