/// Module Unit for the Swagger OpenAPI implementation of the package
unit RALSwaggerModule;

interface

uses
  Classes, SysUtils,
  RALServer, RALTypes, RALRoutes, RALRequest, RALResponse, RALMIMETypes,
  RALTools;

type

  /// License of the API in the OpenAPI info.
  TRALSwaggerLicense = class(TPersistent)
  private
    FName: StringRAL;
    FURL: StringRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    constructor Create;
  published
    /// License name.
    property Name: StringRAL read FName write FName;
    /// License URL.
    property URL: StringRAL read FURL write FURL;
  end;

  { Serves Swagger UI, the OpenAPI document (JSON and YAML) and a Postman collection of
    the server. }
  TRALSwaggerModule = class(TRALModuleRoutes)
  private
    FAllowCORSVerbs : boolean;
    FEMail: StringRAL;
    FLicense: TRALSwaggerLicense;
    FPostmanFile: TFileName;
    FPostmanTag: boolean;
    FRequireAuth: boolean;
    FServersUrl: TStrings;
    FShowCustomNames: boolean;
    FSwaggerFile: TFileName;
    FSystemDescription: TStrings;
    FSystemVersion: StringRAL;
    FTermsOfService: StringRAL;
    FTitle: StringRAL;
  protected
    /// Creates the module routes again (the Postman route only with PostmanTag).
    procedure CreateRoutes;
    /// GET only, and public unless RequireAuth is on
    procedure PublishRoute(ARoute: TRALRoute);
    procedure SetDomain(const AValue: StringRAL); override;
    procedure SetLicense(const AValue: TRALSwaggerLicense);
    procedure SetPostmanFile(const AValue: TFileName);
    procedure SetPostmanTag(AValue: boolean);
    procedure SetRequireAuth(AValue: boolean);
    procedure SetServersUrl(AValue: TStrings);
    procedure SetSwaggerFile(const AValue: TFileName);
    procedure SetSystemDescription(const AValue: TStrings);
    /// Answers the page style sheet.
    procedure SwaggerCSS(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Answers the Swagger UI page.
    procedure SwaggerIndex(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Answers the script that points Swagger UI at swagger.json.
    procedure SwaggerInitializer(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Answers the OpenAPI document, SwaggerFile when set.
    procedure SwaggerJSON(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Answers the Postman collection, PostmanFile when set.
    procedure SwaggerPostman(ARequest: TRALRequest; AResponse: TRALResponse);
    /// The same document as swagger.json, in YAML.
    procedure SwaggerYAML(ARequest: TRALRequest; AResponse: TRALResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetListRoutes: TList; override;
  published
    /// Lists the OPTIONS methods too.
    property AllowCORSVerbs: boolean read FAllowCORSVerbs write FAllowCORSVerbs;
    /// Contact e-mail of the API.
    property EMail: StringRAL read FEMail write FEMail;
    /// License of the API.
    property License: TRALSwaggerLicense read FLicense write SetLicense;
    /// Postman collection served in place of the generated one; an existing file only.
    property PostmanFile: TFileName read FPostmanFile write SetPostmanFile;
    /// Serves postman.json and links it from the document.
    property PostmanTag: boolean read FPostmanTag write SetPostmanTag;
    { The Swagger routes ask the server's authentication too; False (the default)
      makes the route map public even on a server that requires a login. }
    property RequireAuth: boolean read FRequireAuth write SetRequireAuth default False;
    /// Server URLs of the document, one per line.
    property ServersUrl: TStrings read FServersUrl write SetServersUrl;
    /// Uses the route Name as the summary of each operation.
    property ShowCustomNames: boolean read FShowCustomNames write FShowCustomNames;
    /// OpenAPI document served in place of the generated one; an existing file only.
    property SwaggerFile: TFileName read FSwaggerFile write SetSwaggerFile;
    /// Description of the API.
    property SystemDescription: TStrings read FSystemDescription write SetSystemDescription;
    /// Version of the API.
    property SystemVersion: StringRAL read FSystemVersion write FSystemVersion;
    /// Terms of service URL.
    property TermsOfService: StringRAL read FTermsOfService write FTermsOfService;
    /// Title of the API.
    property Title: StringRAL read FTitle write FTitle;
  end;

implementation

uses
  RALJson, RALYAML, RALConsts, RALSwaggerExporter, RALPostmanExporter;

const
  { The page loads Swagger UI from the CDN. The version is pinned, so the
    SHA-384 of each file is fixed: with "integrity" the browser refuses a
    file that does not match, which is what protects the page (and the
    credentials typed into it) if the CDN or the path to it is ever
    tampered with. Bump the version and these three hashes TOGETHER - they
    were computed from unpkg and cross-checked against jsdelivr, e.g.
    PowerShell: [Convert]::ToBase64String([Security.Cryptography.SHA384]::
    Create().ComputeHash([IO.File]::ReadAllBytes('swagger-ui.css'))) }
  SWAGGER_UI_CDN = 'https://unpkg.com/swagger-ui-dist@5.17.9/';
  /// Subresource integrity hash of the Swagger UI style sheet.
  SWAGGER_UI_CSS_SRI = 'sha384-wxLW6kwyHktdDGr6Pv1zgm/VGJh99lfUbzSn6HNHBENZlCN7W602k9VkGdxuFvPn';
  /// Subresource integrity hash of the Swagger UI bundle.
  SWAGGER_UI_BUNDLE_SRI = 'sha384-/dEFsSqOkmDC4Li0Md5TtTcP7+H/mXJappHjKJMlSChNZC+tIWPVzRPsrAe8wHla';
  /// Subresource integrity hash of the Swagger UI preset.
  SWAGGER_UI_PRESET_SRI = 'sha384-galk03E+Pl0FjoMG8/Q/YDvlX1EBQuy2m/ZtQyd2rxoBEB5bzmAMiGC0GO11pjJz';

  { TRALSwaggerLicense }

procedure TRALSwaggerLicense.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALSwaggerLicense then
    RALAssignProperties(Self, Dest)
  else
    inherited AssignTo(Dest);
end;

constructor TRALSwaggerLicense.Create;
begin
  inherited;
  FName := '';
  FURL := '';
end;

{ TRALSwaggerModule }

constructor TRALSwaggerModule.Create(AOwner: TComponent);
begin
  inherited;
  Domain := '/swagger';

  FAllowCORSVerbs := False;
  FEMail := '';
  FLicense := TRALSwaggerLicense.Create;
  FPostmanFile := '';
  FPostmanTag := False;
  FRequireAuth := False;
  FServersUrl := TStringList.Create;
  FShowCustomNames := False;
  FSwaggerFile := '';
  FSystemVersion := '';
  FSystemDescription := TStringList.Create;
  FTermsOfService := '';
  FTitle := '';
end;

procedure TRALSwaggerModule.SetServersUrl(AValue: TStrings);
begin
  FServersUrl.Assign(AValue);
end;

procedure TRALSwaggerModule.CreateRoutes;
var
  vRoute: TRALRoute;
begin
  Routes.Clear;

  vRoute := CreateRoute('/',{$IFDEF FPC}@{$ENDIF}SwaggerIndex);
  PublishRoute(vRoute);

  vRoute := CreateRoute('/swagger.css',{$IFDEF FPC}@{$ENDIF}SwaggerCSS);
  PublishRoute(vRoute);

  vRoute := CreateRoute('/swagger-initializer.js',{$IFDEF FPC}@{$ENDIF}SwaggerInitializer);
  PublishRoute(vRoute);

  vRoute := CreateRoute('/swagger.json',{$IFDEF FPC}@{$ENDIF}SwaggerJSON);
  PublishRoute(vRoute);

  vRoute := CreateRoute('/swagger.yaml',{$IFDEF FPC}@{$ENDIF}SwaggerYAML);
  PublishRoute(vRoute);

  if FPostmanTag then
  begin
    vRoute := CreateRoute('/postman.json',{$IFDEF FPC}@{$ENDIF}SwaggerPostman);
    PublishRoute(vRoute);
  end;
end;

procedure TRALSwaggerModule.PublishRoute(ARoute: TRALRoute);
begin
  ARoute.AllowedMethods := [amGET];
  if not FRequireAuth then
    ARoute.SkipAuthMethods := [amALL];
end;

procedure TRALSwaggerModule.SetRequireAuth(AValue: boolean);
begin
  if FRequireAuth = AValue then
    Exit;

  FRequireAuth := AValue;
  CreateRoutes;
end;

destructor TRALSwaggerModule.Destroy;
begin
  FreeAndNil(FSystemDescription);
  FreeAndNil(FLicense);
  FreeAndNil(FServersUrl);
  inherited;
end;

procedure TRALSwaggerModule.SwaggerIndex(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vHTML: TStringList;
  vURL: StringRAL;
begin
  AResponse.ContentType := rctTEXTHTML;

  vURL := SWAGGER_UI_CDN;

  vHTML := TStringList.Create;
  try
    vHTML.Add('<!DOCTYPE html>');
    vHTML.Add('<html lang="en">');
    vHTML.Add('  <head>');
    vHTML.Add('    <meta charset="UTF-8">');
    vHTML.Add('    <title>RAL Swagger</title>');
    vHTML.Add('    <link rel="stylesheet" type="text/css" href="' + vURL + 'swagger-ui.css"' +
      ' integrity="' + SWAGGER_UI_CSS_SRI + '" crossorigin="anonymous" />');
    vHTML.Add('    <link rel="icon" type="image/png" href="' + vURL +
      'favicon-32x32.png" sizes="32x32" />');
    vHTML.Add('    <link rel="icon" type="image/png" href="' + vURL +
      'favicon-16x16.png" sizes="16x16" />');
    vHTML.Add('    <link rel="stylesheet" type="text/css" href=".' + Domain + '/swagger.css" />');
    vHTML.Add('  </head>');
    vHTML.Add('  <body>');
    vHTML.Add('    <div id="swagger-ui"></div>');
    vHTML.Add('    <script src="' + vURL + 'swagger-ui-bundle.js" charset="UTF-8"' +
      ' integrity="' + SWAGGER_UI_BUNDLE_SRI + '" crossorigin="anonymous"></script>');
    vHTML.Add('    <script src="' + vURL + 'swagger-ui-standalone-preset.js" charset="UTF-8"' +
      ' integrity="' + SWAGGER_UI_PRESET_SRI + '" crossorigin="anonymous"></script>');
    vHTML.Add('    <script src=".' + Domain +
      '/swagger-initializer.js" charset="UTF-8"></script>');
    vHTML.Add('  </body>');
    vHTML.Add('</html>');

    // straight into the body the response sends
    vHTML.SaveToStream(AResponse.BodyStream);
  finally
    FreeAndNil(vHTML);
  end;
end;

procedure TRALSwaggerModule.SetDomain(const AValue: StringRAL);
begin
  inherited;
  CreateRoutes;
end;

procedure TRALSwaggerModule.SetPostmanFile(const AValue: TFileName);
begin
  if (AValue <> '') and (FileExists(AValue)) then
  begin
    FPostmanFile := AValue;
    FPostmanTag := True;
  end
  else
  begin
    FPostmanFile := '';
  end;
end;

procedure TRALSwaggerModule.SetPostmanTag(AValue: boolean);
begin
  if FPostmanTag = AValue then
    Exit;

  FPostmanTag := AValue;
  CreateRoutes;
end;

procedure TRALSwaggerModule.SetSwaggerFile(const AValue: TFileName);
begin
  if (AValue <> '') and (FileExists(AValue)) then
    FSwaggerFile := AValue
  else
    FSwaggerFile := '';
end;

procedure TRALSwaggerModule.SetLicense(const AValue: TRALSwaggerLicense);
begin
  RALAssignOwned(FLicense, AValue);
end;

procedure TRALSwaggerModule.SetSystemDescription(const AValue: TStrings);
begin
  FSystemDescription.Assign(AValue);
end;

procedure TRALSwaggerModule.SwaggerCSS(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vCSS: TStringList;
begin
  AResponse.ContentType := rctTEXTCSS;

  vCSS := TStringList.Create;
  try
    vCSS.Add('html {');
    vCSS.Add('    box-sizing: border-box;');
    vCSS.Add('    overflow: -moz-scrollbars-vertical;');
    vCSS.Add('    overflow-y: scroll;');
    vCSS.Add('}');

    vCSS.Add('*,');
    vCSS.Add('*:before,');
    vCSS.Add('*:after {');
    vCSS.Add('    box-sizing: inherit;');
    vCSS.Add('}');

    vCSS.Add('body {');
    vCSS.Add('    margin: 0;');
    vCSS.Add('    background: #fafafa;');
    vCSS.Add('}');

    // straight into the body the response sends
    vCSS.SaveToStream(AResponse.BodyStream);
  finally
    FreeAndNil(vCSS);
  end;
end;

procedure TRALSwaggerModule.SwaggerInitializer(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vScript: TStringList;
begin
  AResponse.ContentType := rctTEXTJAVASCRIPT;

  vScript := TStringList.Create;
  try
    vScript.Add('window.onload = function() {');
    vScript.Add('  const definitionURL = ".' + Domain + '/swagger.json";');
    vScript.Add('');
    vScript.Add('  window.ui = SwaggerUIBundle({');
    vScript.Add('    url: definitionURL,');
    vScript.Add('    "dom_id": "#swagger-ui",');
    vScript.Add('    deepLinking: true,');
    vScript.Add('    presets: [');
    vScript.Add('      SwaggerUIBundle.presets.apis,');
    vScript.Add('      SwaggerUIStandalonePreset');
    vScript.Add('    ],');
    vScript.Add('    plugins: [');
    vScript.Add('      SwaggerUIBundle.plugins.DownloadUrl');
    vScript.Add('    ],');
    vScript.Add('    layout: "StandaloneLayout",');
    vScript.Add('    queryConfigEnabled: true,');
    { null is how Swagger UI turns the online validator off. With the URL
      there, every browser that opened this page sent the whole swagger.json
      - routes, params, table names - to validator.swagger.io }
    vScript.Add('    validatorUrl: null,');
    vScript.Add('  })');
    vScript.Add('};');

    // straight into the body the response sends
    vScript.SaveToStream(AResponse.BodyStream);
  finally
    FreeAndNil(vScript);
  end;
end;

procedure TRALSwaggerModule.SwaggerJSON(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vSwagger: TRALSwaggerExporter;
begin
  if FileExists(FSwaggerFile) then
  begin
    AResponse.Answer(FSwaggerFile);
    AResponse.ContentDispositionInline := True;
  end
  else
  begin
    vSwagger := TRALSwaggerExporter.Create;
    try
      vSwagger.SwaggerModule := Self;
      { it was ContentEncoding: "Content-Encoding: application/json" is
        not a compression a client knows, and the JSON went out untyped }
      AResponse.ContentType := rctAPPLICATIONJSON;
      { straight into the body the response sends, no copy }
      vSwagger.ExportToStream(Server, AResponse.BodyStream);
    finally
      FreeAndNil(vSwagger);
    end;
  end;
end;

procedure TRALSwaggerModule.SwaggerYAML(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vSwagger: TRALSwaggerExporter;
  vStream: TStream;
  vJSON: TRALJSONValue;
begin
  vSwagger := TRALSwaggerExporter.Create;
  vStream := nil;
  vJSON := nil;
  try
    vSwagger.SwaggerModule := Self;
    if FileExists(FSwaggerFile) then
      vStream := TFileStream.Create(FSwaggerFile, fmOpenRead or fmShareDenyWrite)
    else
      vStream := vSwagger.ExportToStream(Server);
    vStream.Position := 0;
    vJSON := TRALJSON.ParseJSON(vStream);
    AResponse.Answer(HTTP_OK, RALJSONToYAML(vJSON), 'application/yaml');
  finally
    FreeAndNil(vJSON);
    FreeAndNil(vStream);
    FreeAndNil(vSwagger);
  end;
end;

procedure TRALSwaggerModule.SwaggerPostman(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vMem: TStream;
  vPostman: TRALPostmanExporter;
begin
  if FileExists(FPostmanFile) then
  begin
    AResponse.Answer(FPostmanFile);
  end
  else
  begin
    vPostman := TRALPostmanExporter.Create;
    try
      vPostman.AllowCORSVerbs := FAllowCORSVerbs;
      vMem := vPostman.ExportToStream(Server);
      try
        AResponse.AddFile(vMem, 'postman.json');
      finally
        FreeAndNil(vMem);
      end;
    finally
      FreeAndNil(vPostman);
    end;
  end;
end;

function TRALSwaggerModule.GetListRoutes: TList;
begin
  // the routes of the Swagger module are not listed by Swagger itself
  Result := TList.Create;
end;

end.
