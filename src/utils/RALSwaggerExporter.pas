unit RALSwaggerExporter;

interface

uses
  Classes, SysUtils,
  RALJSON, RALServer, RALTypes, RALAuthentication, RALRoutes, RALTools,
  RALSwaggerModule;

type

  { TRALSwaggerExporter }

  TRALSwaggerExporter = class
  private
    FHost : StringRAL;
    FSwaggerModule : TRALSwaggerModule;
  protected
    function getInfo(AServer: TRALServer) : TRALJSONObject;
    function getPaths(AServer: TRALServer) : TRALJSONObject;
    function getSecurity(AAuth: TRALAuthServer) : TRALJSONObject;

    procedure RouteToJSON(AItem : TRALJSONObject; AObject : TComponent; ARoute : TRALRoute);
    procedure AuthToRoute(AItem : TRALJSONObject; AAuth : TRALAuthServer);
  public
    procedure ExportToFile(AServer : TRALServer; AFileName : TFileName);
    procedure ExportToStream(AServer : TRALServer; AStream : TStream); overload;
    function ExportToStream(AServer : TRALServer) : TStream; overload;
  published
    property Host : StringRAL read FHost write FHost;
    property SwaggerModule : TRALSwaggerModule read FSwaggerModule write FSwaggerModule;
  end;

implementation

{ TRALSwaggerExporter }

procedure TRALSwaggerExporter.AuthToRoute(AItem: TRALJSONObject; AAuth: TRALAuthServer);
var
  vRoute, vMethod : TRALJSONObject;
  vTags : TRALJSONArray;
begin
  if AAuth = nil then
    Exit;

  if AAuth is TRALServerJWTAuth then
  begin
    vRoute := TRALJSONObject.Create;
    vMethod := TRALJSONObject.Create;

    vTags := TRALJSONArray.Create;
    vTags.Add(AAuth.Name);

    vMethod.Add('tags', vTags);
    vMethod.Add('summary', 'Get a token');
    vMethod.Add('description', 'Get a token on server');
    vMethod.Add('operationId', 'getToken');

    vRoute.Add('post', vMethod);
    AItem.Add(TRALServerJWTAuth(AAuth).Route, vRoute)
  end;
end;

procedure TRALSwaggerExporter.ExportToFile(AServer: TRALServer; AFileName: TFileName);
var
  vFile : TFileStream;
begin
  vFile := TFileStream.Create(AFileName, fmCreate);
  try
    ExportToStream(AServer, vFile);
  finally
    vFile.Free;
  end;
end;

procedure TRALSwaggerExporter.ExportToStream(AServer: TRALServer; AStream: TStream);
var
  vJson : TRALJSONObject;
  vSchemas : TRALJSONArray;
  vStr : StringRAL;
begin
  vJson := TRALJSONObject.Create;
  try
    vJson.Add('swagger', '2.0');
    vJson.Add('info', getInfo(AServer));
    vJson.Add('host', FHost);
    vJson.Add('basePath', '/');

    vSchemas := TRALJSONArray.Create;
    if AServer.SSLEnabled then
      vSchemas.Add('https')
    else
      vSchemas.Add('http');

    vJson.Add('schemes', vSchemas);
    vJson.Add('paths', getPaths(AServer));

    if AServer.Authentication <> nil then
      vJson.Add('securityDefinitions', getSecurity(AServer.Authentication));

    vStr := vJson.ToJSON;
  finally
    FreeAndNil(vJson);
  end;

  AStream.Write(vStr[POSINISTR], Length(vStr));
end;

function TRALSwaggerExporter.ExportToStream(AServer: TRALServer): TStream;
begin
  Result := TMemoryStream.Create;
  ExportToStream(AServer, Result);
end;

function TRALSwaggerExporter.getInfo(AServer: TRALServer) : TRALJSONObject;
var
  vjAux1 : TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('description', FSwaggerModule.Description.Text);
  Result.Add('version', FSwaggerModule.Version);
  Result.Add('title', FSwaggerModule.Title);
  Result.Add('termsOfService', 'http://swagger.io/terms/'); // colocar

  if FSwaggerModule.EMail <> '' then
  begin
    vjAux1 := TRALJSONObject.Create;
    vjAux1.Add('email', SwaggerModule.EMail);

    Result.Add('contact', vjAux1);
  end;

  vjAux1 := TRALJSONObject.Create;
  vjAux1.Add('name', 'Apache 2.0'); // colocar
  vjAux1.Add('url', 'http://www.apache.org/licenses/LICENSE-2.0.html'); // colocar

  Result.Add('license', vjAux1);
end;

function TRALSwaggerExporter.getPaths(AServer: TRALServer): TRALJSONObject;
var
  vInt1, vInt2 : IntegerRAL;
  vRoute: TRALRoute;
  vSubRoutes: TList;
begin
  Result := TRALJSONObject.Create;

  AuthToRoute(Result, AServer.Authentication);

  // adicionando rotas do server
  for vInt1 := 0 to Pred(AServer.Routes.Count) do
  begin
    vRoute := TRALRoute(AServer.Routes.Items[vInt1]);
    RouteToJSON(Result, AServer, vRoute);
  end;

  // adicionando submodules
  for vInt1 := 0 to Pred(AServer.CountSubModules) do
  begin
    vSubRoutes := AServer.SubModule[vInt1].GetListRoutes;
    try
      for vInt2 := 0 to Pred(vSubRoutes.Count) do
      begin
        vRoute := TRALRoute(vSubRoutes.Items[vInt2]);
        RouteToJSON(Result, AServer.SubModule[vInt1], vRoute);
      end;
    finally
      FreeAndNil(vSubRoutes);
    end;
  end;
end;

function TRALSwaggerExporter.getSecurity(AAuth: TRALAuthServer): TRALJSONObject;
var
  vAuth : TRALJSONObject;
begin
  Result := TRALJSONObject.Create;

  if AAuth is TRALServerBasicAuth then
  begin
    vAuth := TRALJSONObject.Create;
    vAuth.Add('type', 'http');
    vAuth.Add('scheme', 'basic');

    Result.Add('ralAuth', vAuth);
  end
  else if AAuth is TRALServerJWTAuth then
  begin
    vAuth := TRALJSONObject.Create;
    vAuth.Add('type', 'http');
    vAuth.Add('scheme', 'bearer');
    vAuth.Add('bearerFormat', 'JWT');

    Result.Add('ralAuth', vAuth);
  end;
end;

procedure TRALSwaggerExporter.RouteToJSON(AItem: TRALJSONObject;
  AObject: TComponent; ARoute: TRALRoute);
var
  vRoute, vAuth, vjMethod : TRALJSONObject;
  vSecurity, vAuthTypes, vTags : TRALJSONArray;
  vMethod : TRALMethod;
begin
  vRoute := TRALJSONObject.Create;
  for vMethod := Low(TRALMethod) to High(TRALMethod) do
  begin
    if ARoute.IsMethodAllowed(vMethod) then begin
      vjMethod := TRALJSONObject.Create;

      if AObject.Name <> '' then
      begin
        vTags := TRALJSONArray.Create;
        vTags.Add(AObject.Name);

        vjMethod.Add('tags', vTags);
      end;
      vjMethod.Add('summary', '');
      vjMethod.Add('description', ARoute.Description.Text);
      vjMethod.Add('operationId', '');

      if not ARoute.IsMethodSkipped(vMethod) then begin
        vSecurity := TRALJSONArray.Create;

        vAuth := TRALJSONObject.Create;
        vAuthTypes := TRALJSONArray.Create;
        vAuth.Add('ralAuth', vAuthTypes);

        vSecurity.Add(vAuth);
        vjMethod.Add('security', vSecurity);
      end;

      vRoute.Add(LowerCase(RALMethodToHTTPMethod(vMethod)), vjMethod);
    end;
  end;
  AItem.Add(ARoute.Route, vRoute)
end;

end.
