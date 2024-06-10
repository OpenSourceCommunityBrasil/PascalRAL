/// Module Unit for the Swagger OpenAPI implementation of the package
unit RALSwaggerModule;

interface

uses
  Classes, SysUtils,
  RALServer, RALTypes, RALRoutes, RALRequest, RALResponse, RALMIMETypes,
  RAlTools;

type

  { TRALSwaggerLicense }

  TRALSwaggerLicense = class(TPersistent)
  private
    FName: StringRAL;
    FURL: StringRAL;
  published
    constructor Create;
  published
    property Name: StringRAL read FName write FName;
    property URL: StringRAL read FURL write FURL;
  end;

  { TRALSwaggerModule }

  TRALSwaggerModule = class(TRALModuleRoutes)
  private
    FEMail: StringRAL;
    FLicense: TRALSwaggerLicense;
    FPostmanFile: TFileName;
    FPostmanTag: boolean;
    FSystemDescription: TStrings;
    FSystemVersion: StringRAL;
    FSwaggerFile: TFileName;
    FTermsOfService: StringRAL;
    FTitle: StringRAL;
  protected
    procedure CreateRoutes;
    procedure SetDomain(const AValue: StringRAL); override;
    procedure SetPostmanFile(const AValue: TFileName);
    procedure SetPostmanTag(AValue: boolean);
    procedure SetSystemDescription(const AValue: TStrings);
    procedure SetSwaggerFile(const AValue: TFileName);
    procedure SwaggerCSS(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure SwaggerIndex(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure SwaggerInitializer(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure SwaggerJSON(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure SwaggerPostman(ARequest: TRALRequest; AResponse: TRALResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetListRoutes: TList; override;
  published
    property EMail: StringRAL read FEMail write FEMail;
    property License: TRALSwaggerLicense read FLicense write FLicense;
    property PostmanFile: TFileName read FPostmanFile write SetPostmanFile;
    property PostmanTag: boolean read FPostmanTag write SetPostmanTag;
    property SystemDescription: TStrings read FSystemDescription write SetSystemDescription;
    property SystemVersion: StringRAL read FSystemVersion write FSystemVersion;
    property TermsOfService: StringRAL read FTermsOfService write FTermsOfService;
    property Title: StringRAL read FTitle write FTitle;
    property SwaggerFile: TFileName read FSwaggerFile write SetSwaggerFile;
  end;

implementation

uses
  RALSwaggerExporter, RALPostmanExporter;

  { TRALSwaggerLicense }

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
  FSystemDescription := TStringList.Create;
  FPostmanTag := False;
  FEMail := '';
  FTitle := '';
  FSystemVersion := '';
  FTermsOfService := '';
  FLicense := TRALSwaggerLicense.Create;

  Domain := '/swagger';
end;

procedure TRALSwaggerModule.CreateRoutes;
var
  vRoute: TRALRoute;
begin
  Routes.Clear;

  vRoute := CreateRoute(Domain,{$IFDEF FPC}@{$ENDIF}SwaggerIndex);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute(Domain + '/swagger.css',{$IFDEF FPC}@{$ENDIF}SwaggerCSS);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute(Domain + '/swagger-initializer.js',{$IFDEF FPC}@{$ENDIF}SwaggerInitializer);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute(Domain + '/swagger.json',{$IFDEF FPC}@{$ENDIF}SwaggerJSON);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  if FPostmanTag then
  begin
    vRoute := CreateRoute(Domain + '/postman.json',{$IFDEF FPC}@{$ENDIF}SwaggerPostman);
    vRoute.AllowedMethods := [amGET];
    vRoute.SkipAuthMethods := [amALL];
  end;
end;

destructor TRALSwaggerModule.Destroy;
begin
  FreeAndNil(FSystemDescription);
  FreeAndNil(FLicense);
  inherited;
end;

procedure TRALSwaggerModule.SwaggerIndex(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vHTML: TStringList;
  vStream: TMemoryStream;
  vURL: StringRAL;
begin
  AResponse.ContentType := rctTEXTHTML;

  vURL := 'https://unpkg.com/swagger-ui-dist@5.17.9/';

  vHTML := TStringList.Create;
  try
    vHTML.Add('<!DOCTYPE html>');
    vHTML.Add('<html lang="en">');
    vHTML.Add('  <head>');
    vHTML.Add('    <meta charset="UTF-8">');
    vHTML.Add('    <title>RAL Swagger</title>');
    vHTML.Add('    <link rel="stylesheet" type="text/css" href="' + vURL + 'swagger-ui.css" />');
    vHTML.Add('    <link rel="icon" type="image/png" href="' + vURL +
      'favicon-32x32.png" sizes="32x32" />');
    vHTML.Add('    <link rel="icon" type="image/png" href="' + vURL +
      'favicon-16x16.png" sizes="16x16" />');
    vHTML.Add('    <link rel="stylesheet" type="text/css" href=".' + Domain + '/swagger.css" />');
    vHTML.Add('  </head>');
    vHTML.Add('  <body>');
    vHTML.Add('    <div id="swagger-ui"></div>');
    vHTML.Add('    <script src="' + vURL +
      'swagger-ui-bundle.js" charset="UTF-8" crossorigin></script>');
    vHTML.Add('    <script src="' + vURL +
      'swagger-ui-standalone-preset.js" charset="UTF-8" crossorigin></script>');
    vHTML.Add('    <script src=".' + Domain +
      '/swagger-initializer.js" charset="UTF-8"></script>');
    vHTML.Add('  </body>');
    vHTML.Add('</html>');

    vStream := TMemoryStream.Create;
    try
      vHTML.SaveToStream(vStream);
      vStream.Position := 0;
      AResponse.ResponseStream := vStream;
    finally
      FreeAndNil(vStream);
    end;
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

procedure TRALSwaggerModule.SetSystemDescription(const AValue: TStrings);
begin
  FSystemDescription.Assign(AValue);
end;

procedure TRALSwaggerModule.SwaggerCSS(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vStream: TMemoryStream;
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

    vStream := TMemoryStream.Create;
    try
      vCSS.SaveToStream(vStream);
      vStream.Position := 0;
      AResponse.ResponseStream := vStream;
    finally
      FreeAndNil(vStream);
    end;
  finally
    FreeAndNil(vCSS);
  end;
end;

procedure TRALSwaggerModule.SwaggerInitializer(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vStream: TMemoryStream;
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
    vScript.Add('    validatorUrl: "https://validator.swagger.io/validator",');
    vScript.Add('  })');
    vScript.Add('};');

    vStream := TMemoryStream.Create;
    try
      vScript.SaveToStream(vStream);
      vStream.Position := 0;
      AResponse.ResponseStream := vStream;
    finally
      FreeAndNil(vStream);
    end;
  finally
    FreeAndNil(vScript);
  end;
end;

procedure TRALSwaggerModule.SwaggerJSON(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vMem: TStream;
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
      vMem := vSwagger.ExportToStream(Server);
      try
        AResponse.ResponseStream := vMem;
      finally
        FreeAndNil(vMem);
      end;
    finally
      FreeAndNil(vSwagger);
    end;
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
  // nao devolse as rotas
  Result := TList.Create;
end;

end.
