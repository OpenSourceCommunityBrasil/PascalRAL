unit RALSwaggerModule;

interface

uses
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
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
    procedure SwaggerUI(ARequest : TRALRequest; AResponse : TRALResponse);
    procedure SwaggerUIBundle(ARequest : TRALRequest; AResponse : TRALResponse);
    procedure SwaggerUIBundleMap(ARequest : TRALRequest; AResponse : TRALResponse);
    procedure SwaggerUIStandalone(ARequest : TRALRequest; AResponse : TRALResponse);
    procedure SwaggerInitializer(ARequest : TRALRequest; AResponse : TRALResponse);
    procedure SwaggerJSON(ARequest : TRALRequest; AResponse : TRALResponse);

    function SwaggerResourceStream(AResourceName : StringRAL) : TStream;
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

{$IFNDEF FPC}
  {$R RALSwaggerModule.res}
{$ENDIF}

uses
  RALSwaggerExporter;

{ TRALSwaggerModule }

constructor TRALSwaggerModule.Create(AOwner: TComponent);
var
  vRoute : TRALRoute;
begin
  inherited;
  vRoute := CreateRoute('/swagger', @SwaggerIndex);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute('/swagger/swagger.css', @SwaggerCSS);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute('/swagger/swagger-ui.css', @SwaggerUI);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute('/swagger/swagger-ui-bundle.js', @SwaggerUIBundle);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute('/swagger/swagger-ui-bundle.js.map', @SwaggerUIBundleMap);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute('/swagger/swagger-ui-standalone-preset.js', @SwaggerUIStandalone);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute('/swagger/swagger-initializer.js', @SwaggerInitializer);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  vRoute := CreateRoute('/swagger/swagger.json', @SwaggerJSON);
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
  vResource : TStream;
begin
  AResponse.ContentType := rctTEXTHTML;
  vResource := SwaggerResourceStream('SWAGGER_INDEX');
  try
    AResponse.ResponseStream := vResource;
  finally
    FreeAndNil(vResource);
  end;
end;

procedure TRALSwaggerModule.SwaggerCSS(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vResource : TStream;
begin
  AResponse.ContentType := rctTEXTCSS;
  vResource := SwaggerResourceStream('SWAGGER_CSS');
  try
    AResponse.ResponseStream := vResource;
  finally
    FreeAndNil(vResource);
  end;
end;

procedure TRALSwaggerModule.SwaggerUI(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vResource : TStream;
begin
  AResponse.ContentType := rctTEXTCSS;
  vResource := SwaggerResourceStream('SWAGGER_UI_CSS');
  try
    AResponse.ResponseStream := vResource;
  finally
    FreeAndNil(vResource);
  end;
end;

procedure TRALSwaggerModule.SwaggerUIBundle(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vResource : TStream;
begin
  AResponse.ContentType := rctTEXTJAVASCRIPT;
  vResource := SwaggerResourceStream('SWAGGER_UI_BUNDLE_JS');
  try
    AResponse.ResponseStream := vResource;
  finally
    FreeAndNil(vResource);
  end;
end;

procedure TRALSwaggerModule.SwaggerUIBundleMap(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vResource : TStream;
begin
//  AResponse.ContentType := rctTEXTJAVASCRIPT;
  vResource := SwaggerResourceStream('SWAGGER_UI_BUNDLE_MAP');
  try
    AResponse.ResponseStream := vResource;
  finally
    FreeAndNil(vResource);
  end;
end;

procedure TRALSwaggerModule.SwaggerUIStandalone(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vResource : TStream;
begin
  AResponse.ContentType := rctTEXTJAVASCRIPT;
  vResource := SwaggerResourceStream('SWAGGER_UI_STANDALONE');
  try
    AResponse.ResponseStream := vResource;
  finally
    FreeAndNil(vResource);
  end;
end;

procedure TRALSwaggerModule.SwaggerInitializer(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vResource : TStream;
begin
  AResponse.ContentType := rctTEXTJAVASCRIPT;
  vResource := SwaggerResourceStream('SWAGGER_INITIALIZER');
  try
    AResponse.ResponseStream := vResource;
  finally
    FreeAndNil(vResource);
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
    AResponse.ResponseStream := vMem;
    vMem.Free;
  finally
    vSwagger.Free;
  end;
end;

function TRALSwaggerModule.SwaggerResourceStream(AResourceName: StringRAL): TStream;
begin
  {$IFDEF FPC}
    Result := TLazarusResourceStream.Create(AResourceName, 'RCDATA');
  {$ELSE}
    Result := TResourceStream.Create(HINSTANCE, AResourceName, 'RCDATA');
  {$ENDIF}
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

{$IFDEF FPC}
initialization
  {$I RALSwaggerModule.lrs}
{$ENDIF}

end.
