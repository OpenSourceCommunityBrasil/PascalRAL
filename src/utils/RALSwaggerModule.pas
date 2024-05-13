unit RALSwaggerModule;

interface

uses
  Classes, SysUtils,
  RALServer, RALTypes, RALRoutes, RALRequest, RALResponse, RALMIMETypes,
  RAlTools;

type
  { TRALSwaggerModule }

  TRALSwaggerModule = class(TRALModuleRoutes)
  private
    FDescription : TStrings;
    FEMail : StringRAL;
    FTitle : StringRAL;
    FVersion : StringRAL;
    FRoute : StringRAL;
  protected
    procedure SetRoute(const AValue: StringRAL);

    procedure SwaggerIndex(ARequest : TRALRequest; AResponse : TRALResponse);
    procedure SwaggerCSS(ARequest : TRALRequest; AResponse : TRALResponse);
    procedure SwaggerInitializer(ARequest : TRALRequest; AResponse : TRALResponse);
    procedure SwaggerJSON(ARequest : TRALRequest; AResponse : TRALResponse);
    procedure SwaggerPostman(ARequest : TRALRequest; AResponse : TRALResponse);

    procedure CreateRoutes;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function IsDomain : boolean; override;
    function GetListRoutes : TList; override;
  published
    property Description : TStrings read FDescription write FDescription;
    property EMail : StringRAL read FEMail write FEMail;
    property Title : StringRAL read FTitle write FTitle;
    property Version : StringRAL read FVersion write FVersion;
    property Route : StringRAL read FRoute write SetRoute;
  end;

implementation

uses
  RALSwaggerExporter, RALPostmanExporter;

{ TRALSwaggerModule }

constructor TRALSwaggerModule.Create(AOwner: TComponent);
begin
  inherited;
  FDescription := TStringList.Create;
  FRoute := '/swagger';
  CreateRoutes;
end;

procedure TRALSwaggerModule.CreateRoutes;
var
  vRoute : TRALRoute;
begin
  Routes.Clear;

  vRoute := CreateRoute(FRoute, {$IFDEF FPC}@{$ENDIF}SwaggerIndex);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute(FRoute + '/swagger.css', {$IFDEF FPC}@{$ENDIF}SwaggerCSS);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute(FRoute + '/swagger-initializer.js', {$IFDEF FPC}@{$ENDIF}SwaggerInitializer);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute(FRoute + '/swagger.json', {$IFDEF FPC}@{$ENDIF}SwaggerJSON);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute(FRoute + '/postman.json', {$IFDEF FPC}@{$ENDIF}SwaggerPostman);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];
end;

destructor TRALSwaggerModule.Destroy;
begin
  FreeAndNil(FDescription);
  inherited;
end;

procedure TRALSwaggerModule.SwaggerIndex(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vHTML : TStringList;
  vStream : TMemoryStream;
  vURL : StringRAL;
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
    vHTML.Add('    <link rel="icon" type="image/png" href="' + vURL + 'favicon-32x32.png" sizes="32x32" />');
    vHTML.Add('    <link rel="icon" type="image/png" href="' + vURL + 'favicon-16x16.png" sizes="16x16" />');
    vHTML.Add('    <link rel="stylesheet" type="text/css" href=".' + FRoute + '/swagger.css" />');
    vHTML.Add('  </head>');
    vHTML.Add('  <body>');
    vHTML.Add('    <div id="swagger-ui"></div>');
    vHTML.Add('    <script src="' + vURL + 'swagger-ui-bundle.js" charset="UTF-8" crossorigin></script>');
    vHTML.Add('    <script src="' + vURL + 'swagger-ui-standalone-preset.js" charset="UTF-8" crossorigin></script>');
    vHTML.Add('    <script src=".' + FRoute + '/swagger-initializer.js" charset="UTF-8"></script>');
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

procedure TRALSwaggerModule.SetRoute(const AValue: StringRAL);
begin
  if AValue = FRoute then
    Exit;

  FRoute := FixRoute(AValue);
  CreateRoutes;
end;

procedure TRALSwaggerModule.SwaggerCSS(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vStream : TMemoryStream;
  vCSS : TStringList;
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

procedure TRALSwaggerModule.SwaggerInitializer(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vStream : TMemoryStream;
  vScript : TStringList;
begin
  AResponse.ContentType := rctTEXTJAVASCRIPT;

  vScript := TStringList.Create;
  try
    vScript.Add('window.onload = function() {');
    vScript.Add('  const definitionURL = ".' + FRoute + '/swagger.json";');
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

procedure TRALSwaggerModule.SwaggerJSON(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vMem : TStream;
  vSwagger : TRALSwaggerExporter;
begin
  vSwagger := TRALSwaggerExporter.Create;
  try
    vSwagger.SwaggerModule := Self;
    vSwagger.Host := ARequest.Host;
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

procedure TRALSwaggerModule.SwaggerPostman(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vMem : TStream;
  vPostman : TRALPostmanExporter;
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

function TRALSwaggerModule.IsDomain: boolean;
begin
  Result := False;
end;

function TRALSwaggerModule.GetListRoutes: TList;
begin
  // nao devolse as rotas
  Result := TList.Create;
end;

end.
