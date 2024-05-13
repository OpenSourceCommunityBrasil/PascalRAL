unit RALSwaggerModule;

interface

uses
  Classes, SysUtils,
  RALServer, RALTypes, RALRoutes, RALRequest, RALResponse, RALMIMETypes;

type
  { TRALSwaggerModule }

  TRALSwaggerModule = class(TRALModuleRoutes)
  private
    FDescription : TStrings;
    FEMail : StringRAL;
    FTitle : StringRAL;
    FVersion : StringRAL;
  protected
    procedure SwaggerIndex(ARequest : TRALRequest; AResponse : TRALResponse);
    procedure SwaggerCSS(ARequest : TRALRequest; AResponse : TRALResponse);
    procedure SwaggerInitializer(ARequest : TRALRequest; AResponse : TRALResponse);
    procedure SwaggerJSON(ARequest : TRALRequest; AResponse : TRALResponse);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function CanAnswerRoute(ARequest: TRALRequest; AResponse : TRALResponse): TRALRoute; override;
    function IsDomain : boolean; override;
    function GetListRoutes : TList; override;
  published
    property Description : TStrings read FDescription write FDescription;
    property EMail : StringRAL read FEMail write FEMail;
    property Title : StringRAL read FTitle write FTitle;
    property Version : StringRAL read FVersion write FVersion;
  end;

implementation

uses
  RALSwaggerExporter;

{ TRALSwaggerModule }

constructor TRALSwaggerModule.Create(AOwner: TComponent);
var
  vRoute : TRALRoute;
begin
  inherited;
  vRoute := CreateRoute('/swagger', {$IFDEF FPC}@{$ENDIF}SwaggerIndex);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute('/swagger/swagger.css', {$IFDEF FPC}@{$ENDIF}SwaggerCSS);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute('/swagger/swagger-initializer.js', {$IFDEF FPC}@{$ENDIF}SwaggerInitializer);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute('/swagger/swagger.json', {$IFDEF FPC}@{$ENDIF}SwaggerJSON);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  FDescription := TStringList.Create;
end;

destructor TRALSwaggerModule.Destroy;
begin
  FreeAndNil(FDescription);
  inherited;
end;

function TRALSwaggerModule.CanAnswerRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
begin
  Result := Routes.CanAnswerRoute(ARequest);
end;

procedure TRALSwaggerModule.SwaggerIndex(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vHTML : TStringList;
  vStream : TMemoryStream;
  vURL : StringRAL;
begin
  AResponse.ContentType := rctTEXTHTML;

  vURL := 'https://unpkg.com/swagger-ui-dist@5.17.7/';

  vHTML := TStringList.Create;
  try
    vHTML.Add('<!DOCTYPE html>');
    vHTML.Add('<html lang="en">');
    vHTML.Add('  <head>');
    vHTML.Add('    <meta charset="UTF-8">');
    vHTML.Add('    <title>RAL Swagger</title>');
    vHTML.Add('    <link rel="stylesheet" type="text/css" href="' + vURL + 'swagger-ui.css"/>');
    vHTML.Add('    <link rel="stylesheet" type="text/css" href="swagger.css" />');
    vHTML.Add('  </head>');
    vHTML.Add('  <body>');
    vHTML.Add('    <div id="swagger-ui"></div>');
    vHTML.Add('    <script src="' + vURL + 'swagger-ui-bundle.js" charset="UTF-8" crossorigin></script>');
    vHTML.Add('    <script src="' + vURL + 'swagger-ui-standalone-preset.js" charset="UTF-8" crossorigin></script>');
    vHTML.Add('    <script src="swagger-initializer.js" charset="UTF-8"> </script>');
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
    vScript.Add('  const definitionURL = "./swagger.json";');
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
