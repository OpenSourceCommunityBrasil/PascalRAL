unit ralwizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProjectIntf, Forms, LazIDEIntf, System.UITypes, Dialogs;

type
  { TRALWizard }

  TRALWizard = class(TProjectDescriptor)
  private
    FApplicationType : integer;
    FEngineType : integer;
    FAuthType : integer;
    FSwagger : boolean;
    FWebModule : boolean;
    FProjectDir : string;
  protected
    function CreateProjStandAlone(AProject: TLazProject) : TModalResult;
  public
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function DoInitDescriptor: TModalResult; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TRALWizardApplication }

  TRALWizardApplication = class(TRALWizard)
  protected
    function CreateFormStandAlone(AProject: TLazProject) : TModalResult;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function DoInitDescriptor: TModalResult; override;
  end;

procedure Register;

var
  RALWizardApplication : TRALWizardApplication;

implementation

uses
  ralwizardform;

procedure Register;
begin
  RALWizardApplication := TRALWizardApplication.Create;
  RegisterProjectDescriptor(RALWizardApplication);
end;

{ TRALWizard }

function TRALWizard.CreateProjStandAlone(AProject: TLazProject): TModalResult;
var
  vMainFile: TLazProjectFile;
  vFile: TStringList;
  vProjLPIName, vProjLPRName : string;
  vServerUnit : string;
begin
  Result := inherited InitProject(AProject);

  vProjLPIName := FProjectDir + 'ralproject1.lpi';
  vProjLPRName := FProjectDir + 'ralproject1.lpr';

  AProject.ProjectInfoFile := vProjLPIName;

  vMainFile := AProject.CreateProjectFile(vProjLPRName);
  vMainFile.IsPartOfProject := True;

  AProject.AddFile(vMainFile, False);
  AProject.MainFileID := 0;
  AProject.Title := 'RAL Wizard StandAlone Application';

  case FEngineType of
    0 : vServerUnit := 'indyral';
    1 : vServerUnit := 'synopseral';
    2 : vServerUnit := 'saguiral';
    3 : vServerUnit := 'fphttpral';
  end;

  vFile := TStringList.Create;
  try
    vFile.Add('// by PascalRAL - StandAlone App: '+DateTimeToStr(Now));
    vFile.Add('program ralproject1;');
    vFile.Add('');
    vFile.Add('{$mode objfpc}{$H+}');
    vFile.Add('');
    vFile.Add('uses');
    vFile.Add('  {$IFDEF UNIX}');
    vFile.Add('  cthreads,');
    vFile.Add('  {$ENDIF}');
    vFile.Add('  {$IFDEF HASAMIGA}');
    vFile.Add('  athreads,');
    vFile.Add('  {$ENDIF}');
    vFile.Add('  Interfaces, // this includes the LCL widgetset');
    vFile.Add(Format('  Forms, unit1, %s', [vServerUnit]));
    vFile.Add('  { you can add units after this };');
    vFile.Add('');
    vFile.Add('{$R *.res}');
    vFile.Add('');
    vFile.Add('begin');
    vFile.Add('  RequireDerivedFormResource:=True;');
    vFile.Add('  Application.Scaled:=True;');
    vFile.Add('  Application.Initialize;');
    vFile.Add('  Application.CreateForm(TRALForm1, RALForm1);');
    vFile.Add('  Application.Run;');
    vFile.Add('end.');

    AProject.MainFile.SetSourceText(vFile.Text, True);
  finally
    FreeAndNil(vFile);
  end;

  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements,
                                      pfMainUnitHasTitleStatement,
                                      pfLRSFilesInOutputDirectory];

  AProject.UseManifest := False;
  AProject.UseAppBundle := False;

  AProject.Modified:= True;
  Result := mrOK;
end;

function TRALWizard.GetLocalizedName: string;
begin
  Result := inherited GetLocalizedName;
end;

function TRALWizard.GetLocalizedDescription: string;
begin
  Result := inherited GetLocalizedDescription;
end;

function TRALWizard.DoInitDescriptor: TModalResult;
begin
  Result := mrCancel;

  fralwizardform := Tfralwizardform.Create(Application);
  if fralwizardform.ShowModal = mrOK then
  begin
    FApplicationType := fralwizardform.cbTipoAplicacao.ItemIndex;
    FEngineType := fralwizardform.cbTipoMotor.ItemIndex;
    FAuthType := fralwizardform.cbAutenticacao.ItemIndex;
    FSwagger := fralwizardform.ckSwagger.Checked;
    FWebModule := fralwizardform.ckWebModule.Checked;

    FProjectDir := fralwizardform.eDirAplicacao.Text;
    FProjectDir := IncludeTrailingPathDelimiter(FProjectDir);

    Result := mrOK;
  end;
end;

function TRALWizard.InitProject(AProject: TLazProject): TModalResult;
begin
  Result := inherited InitProject(AProject);

  case FApplicationType of
    0 : Result := CreateProjStandAlone(AProject);
    1 : Result := mrOK; // console
    2 : Result := mrOK; // CGI - Todo
  end;

//  LazarusIDE.DoSaveProject([]);
end;

function TRALWizard.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result := inherited CreateStartFiles(AProject);
end;

{ TRALWizardApplication }

function TRALWizardApplication.CreateFormStandAlone(AProject: TLazProject): TModalResult;
var
  vFormFile, vFormLFMFile : TLazProjectFile;
  vFile: TStringList;
  vFormUnitName, vFormLFMName: string;
  vServerUnit, vServerClass, vUnits: string;
begin
  // Define the name of the main unit and form file
  vFormUnitName := FProjectDir + 'unit1.pas';
  vFormLFMName := FProjectDir + 'unit1.lfm';

  // Create the main project file
  vFormFile := AProject.CreateProjectFile(vFormUnitName);
  vFormFile.IsPartOfProject := True;

  AProject.AddFile(vFormFile, False);

  case FEngineType of
    0 : begin
      vServerUnit := 'RALIndyServer';
      vServerClass := 'TRALIndyServer';
    end;
    1 : begin
      vServerUnit := 'RALSynopseServer';
      vServerClass := 'TRALSynopseServer';
    end;
    2 : begin
      vServerUnit := 'RALSaguiServer';
      vServerClass := 'TRALSaguiServer';
    end;
    3 : begin
      vServerUnit := 'RALfpHTTPServer';
      vServerClass := 'TRALfpHttpServer';
    end;
  end;

  vUnits := 'RALMimeTypes';
  if FSwagger then
    vUnits := vUnits + ', RALSwaggerModule';
  if FWebModule then
    vUnits := vUnits + ', RALWebModule';
  if FAuthType > 0 then
    vUnits := vUnits + ', RALAuthentication';

  // Create the source for the main unit
  vFile := TStringList.Create;
  try
    vFile.Add('// by PascalRAL - StandAlone App: '+DateTimeToStr(Now));
    vFile.Add('unit unit1;');
    vFile.Add('');
    vFile.Add('{$mode objfpc}{$H+}');
    vFile.Add('');
    vFile.Add('interface');
    vFile.Add('');
    vFile.Add('uses');
    vFile.Add(Format('  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, %s,', [vServerUnit]));
    vFile.Add('  RALRoutes, RALResponse, RALRequest,');
    vFile.Add(Format('  %s;', [vUnits]));
    vFile.Add('');
    vFile.Add('type');
    vFile.Add('');
    vFile.Add('  { TRALForm1 }');
    vFile.Add('');
    vFile.Add('  TRALForm1 = class(TForm)');
    vFile.Add(Format('    server: %s;', [vServerClass]));

    case FAuthType of
      1 : vFile.Add('    authbasic: TRALServerBasicAuth;');
      2 : vFile.Add('    authjwt: TRALServerJWTAuth;');
    end;

    if FSwagger then
      vFile.Add('    swagger: TRALSwaggerModule;');
    if FWebModule then
      vFile.Add('    webmodule: TRALWebModule;');

    vFile.Add('    procedure FormCreate(Sender: TObject);');
    vFile.Add('    procedure server_pingReply(ARequest: TRALRequest; AResponse: TRALResponse);');
    vFile.Add('  private');
    vFile.Add('');
    vFile.Add('  public');
    vFile.Add('');
    vFile.Add('  end;');
    vFile.Add('');
    vFile.Add('var');
    vFile.Add('  RALForm1: TRALForm1;');
    vFile.Add('');
    vFile.Add('implementation');
    vFile.Add('');
    vFile.Add('{$R *.lfm}');
    vFile.Add('');
    vFile.Add('{ TRALForm1 }');
    vFile.Add('');
    vFile.Add('procedure TRALForm1.FormCreate(Sender: TObject);');
    vFile.Add('begin');
    vFile.Add('  server.Start;');
    vFile.Add('end;');
    vFile.Add('');
    vFile.Add('procedure TRALForm1.server_pingReply(ARequest: TRALRequest;');
    vFile.Add('  AResponse: TRALResponse);');
    vFile.Add('begin');
    vFile.Add('  AResponse.Answer(200, ''pong'', rctTEXTPLAIN);');
    vFile.Add('end;');
    vFile.Add('');
    vFile.Add('end.');

    vFile.SaveToFile(vFormUnitName);
    vFormFile.SetSourceText(vFile.Text, True);
  finally
    vFile.Free;
  end;

  vFormLFMFile := AProject.CreateProjectFile(vFormLFMName);
  vFormLFMFile.IsPartOfProject := False;

  AProject.AddFile(vFormLFMFile, False);

  // Create the LFM file for the form
  vFile := TStringList.Create;
  try
    vFile.Add('object RALForm1: TRALForm1');
    vFile.Add('  Left = 350');
    vFile.Add('  Height = 378');
    vFile.Add('  Top = 250');
    vFile.Add('  Width = 498');
    vFile.Add('  Caption = ''RALForm1''');
    vFile.Add('  OnCreate = FormCreate');
    vFile.Add(Format('  object server: %s', [vServerClass]));
    vFile.Add('    Active = False');
    case FAuthType of
      1 : vFile.Add('    Authentication = authbasic');
      2 : vFile.Add('    Authentication = authjwt');
    end;
    vFile.Add('    CompressType = ctNone');
    vFile.Add('    CookieLife = 30');
    vFile.Add('    CORSOptions.AllowHeaders.Strings = (');
    vFile.Add('      ''Content-Type''');
    vFile.Add('      ''Origin''');
    vFile.Add('      ''Accept''');
    vFile.Add('      ''Authorization''');
    vFile.Add('      ''Content-Encoding''');
    vFile.Add('      ''Accept-Encoding''');
    vFile.Add('    )');
    vFile.Add('    CORSOptions.AllowOrigin = ''*''');
    vFile.Add('    CORSOptions.MaxAge = 86400');
    vFile.Add('    CriptoOptions.CriptType = crNone');
    vFile.Add('    IPConfig.IPv4Bind = ''0.0.0.0''');
    vFile.Add('    IPConfig.IPv6Bind = ''::''');
    vFile.Add('    IPConfig.IPv6Enabled = False');
    vFile.Add('    Port = 8000');
    vFile.Add('    Routes = <    ');
    vFile.Add('      item');
    vFile.Add('        Route = ''/ping''');
    vFile.Add('        InputParams = <>');
    vFile.Add('        AllowedMethods = [amGET]');
    vFile.Add('        AllowURIParams = False');
    vFile.Add('        Callback = False');
    vFile.Add('        Name = ''ping''');
    vFile.Add('        SkipAuthMethods = []');
    vFile.Add('        URIParams = <>');
    vFile.Add('        OnReply = server_pingReply');
    vFile.Add('      end>');
    vFile.Add('    Security.BruteForce.ExpirationTime = 1800000');
    vFile.Add('    Security.BruteForce.MaxTry = 3');
    vFile.Add('    Security.FloodTimeInterval = 30');
    vFile.Add('    Security.Options = []');
    vFile.Add('    ShowServerStatus = True');
    vFile.Add('    SSL.Enabled = False');
    vFile.Add('    SSL.SSLOptions.Mode = sslmUnassigned');
    vFile.Add('    SSL.SSLOptions.VerifyMode = []');
    vFile.Add('    SSL.SSLOptions.VerifyDepth = 0');
    vFile.Add('    Left = 236');
    vFile.Add('    Top = 172');
    vFile.Add('  end');
    if FSwagger then
    begin
      vFile.Add('  object swagger: TRALSwaggerModule');
      vFile.Add('    Server = server');
      vFile.Add('    Domain = ''/swagger''');
      vFile.Add('    PostmanTag = False');
      vFile.Add('    Left = 236');
      vFile.Add('    Top = 228');
      vFile.Add('  end');
    end;
    if FWebModule then
    begin
      vFile.Add('  object webmodule: TRALWebModule');
      vFile.Add('    Server = server');
      vFile.Add('    Domain = ''/''');
      vFile.Add('    Routes = <>');
      vFile.Add('    Left = 284');
      vFile.Add('    Top = 228');
      vFile.Add('  end');
    end;
    if FAuthType = 1 then
    begin
      vFile.Add('  object authbasic: TRALServerBasicAuth');
      vFile.Add('    AuthDialog = True');
      vFile.Add('    Password = ''ralteste''');
      vFile.Add('    UserName = ''ralteste''');
      vFile.Add('    Left = 284');
      vFile.Add('    Top = 172');
      vFile.Add('  end');
    end
    else if FAuthType = 2 then
    begin
      vFile.Add('  object authjwt: TRALServerJWTAuth');
      vFile.Add('    Algorithm = tjaHSHA256');
      vFile.Add('    AuthRoute.Description.Strings = (');
      vFile.Add('      ''Get a JWT Token''');
      vFile.Add('    )');
      vFile.Add('    AuthRoute.Route = ''/gettoken''');
      vFile.Add('    AuthRoute.InputParams = <>');
      vFile.Add('    ExpirationSecs = 1800');
      vFile.Add('    JSONKey = ''token''');
      vFile.Add('    Left = 284');
      vFile.Add('    Top = 172');
      vFile.Add('  end');
    end;
    vFile.Add('end');

    vFile.SaveToFile(vFormLFMName);
    vFormLFMFile.SetSourceText(vFile.Text, True);
  finally
    vFile.Free;
  end;

  AProject.Modified := True;

  Result := mrOk;
end;

constructor TRALWizardApplication.Create;
begin
  inherited Create;
  Name := 'Create a new Pascal RAL Server Application';
end;

function TRALWizardApplication.GetLocalizedName: string;
begin
  Result:= 'Pascal RAL Application Wizard';
end;

function TRALWizardApplication.GetLocalizedDescription: string;
begin
  Result:=  'Opens a window to choose the possible types of server applications, '+
            'as well as its data engine and other extra options';
end;

function TRALWizardApplication.InitProject(AProject: TLazProject): TModalResult;
begin
  Result := inherited InitProject(AProject);
  if Result = mrOK then
  begin
    case FApplicationType of
      0 : Result := CreateFormStandAlone(AProject);
      1 : Result := mrOK; // console
      2 : Result := mrOK; // CGI - Todo
    end;
  end;
end;

function TRALWizardApplication.DoInitDescriptor: TModalResult;
begin
  Result := inherited DoInitDescriptor;
end;

end.

