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
    FSwaggerModule : TRALSwaggerModule;
  protected
    function getInfo(AServer: TRALServer) : TRALJSONObject;
    function getServers(AServer: TRALServer) : TRALJSONArray;
    function getTags(AServer: TRALServer) : TRALJSONArray;
    function getPaths(AServer: TRALServer) : TRALJSONObject;
    function getSecurity(AAuth: TRALAuthServer) : TRALJSONObject;

    procedure RouteToJSON(AItem : TRALJSONObject; AServer : TRALServer;
                          AModule : TRALModuleRoutes; ARoute : TRALRoute);
    procedure AuthToRoute(AItem : TRALJSONObject; AAuth : TRALAuthServer);
    function RouteToOperation(ARoute : StringRAL) : StringRAL;
  public
    procedure ExportToFile(AServer : TRALServer; AFileName : TFileName);
    procedure ExportToStream(AServer : TRALServer; AStream : TStream); overload;
    function ExportToStream(AServer : TRALServer) : TStream; overload;
  published
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
  vStr : StringRAL;
begin
  vJson := TRALJSONObject.Create;
  try
    vJson.Add('openapi', '3.1.0');
    vJson.Add('info', getInfo(AServer));
    vJson.Add('servers', getServers(AServer));
    if SwaggerModule.PostmanTag then
      vJson.Add('tags', getTags(AServer));
    vJson.Add('paths', getPaths(AServer));

    if AServer.Authentication <> nil then
      vJson.Add('securityDefinitions', getSecurity(AServer.Authentication));

    vStr := vJson.ToJSON;
  finally
    FreeAndNil(vJson);
  end;

  AStream.Write(vStr[POSINISTR], Length(vStr));
  AStream.Position := 0;
end;

function TRALSwaggerExporter.ExportToStream(AServer: TRALServer): TStream;
begin
  Result := TMemoryStream.Create;
  ExportToStream(AServer, Result);
end;

function TRALSwaggerExporter.getInfo(AServer: TRALServer) : TRALJSONObject;
var
  vjAux1 : TRALJSONObject;
  vAux : StringRAL;
begin
  vAux := FSwaggerModule.SystemDescription.Text;
  vAux := StringReplace(vAux, #13#10, '<br/>', [rfReplaceAll]);
  vAux := StringReplace(vAux, #13, '<br/>', [rfReplaceAll]);
  vAux := StringReplace(vAux, #10, '<br/>', [rfReplaceAll]);

  Result := TRALJSONObject.Create;
  Result.Add('title', FSwaggerModule.Title);
  Result.Add('description', vAux);
  Result.Add('version', FSwaggerModule.SystemVersion);
  if SwaggerModule.TermsOfService <> '' then
    Result.Add('termsOfService', SwaggerModule.TermsOfService);

  if FSwaggerModule.EMail <> '' then
  begin
    vjAux1 := TRALJSONObject.Create;
    vjAux1.Add('email', SwaggerModule.EMail);

    Result.Add('contact', vjAux1);
  end;

  if (SwaggerModule.License.Name <> '') or (SwaggerModule.License.URL <> '') then
  begin
    vjAux1 := TRALJSONObject.Create;
    vjAux1.Add('name', SwaggerModule.License.Name);
    vjAux1.Add('url', SwaggerModule.License.URL);

    Result.Add('license', vjAux1);
  end;
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
    RouteToJSON(Result, AServer, nil, vRoute);
  end;

  // adicionando submodules
  for vInt1 := 0 to Pred(AServer.CountSubModules) do
  begin
    vSubRoutes := AServer.SubModule[vInt1].GetListRoutes;
    try
      for vInt2 := 0 to Pred(vSubRoutes.Count) do
      begin
        vRoute := TRALRoute(vSubRoutes.Items[vInt2]);
        RouteToJSON(Result, AServer, AServer.SubModule[vInt1], vRoute);
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

function TRALSwaggerExporter.getServers(AServer: TRALServer): TRALJSONArray;
var
  vItem : TRALJSONObject;
begin
  Result := TRALJSONArray.Create;

  vItem := TRALJSONObject.Create;
  vItem.Add('url', '/');

  Result.Add(vItem);
end;

function TRALSwaggerExporter.getTags(AServer: TRALServer): TRALJSONArray;
var
  vItem1, vItem2 : TRALJSONObject;
begin
  Result := TRALJSONArray.Create;

  if SwaggerModule.PostmanTag then
  begin
    vItem2 := TRALJSONObject.Create;
    vItem2.Add('description', 'Postman Document');
    vItem2.Add('url', '.' + SwaggerModule.Domain + '/postman.json');

    vItem1 := TRALJSONObject.Create;
    vItem1.Add('name', 'Postman');
    vItem1.Add('description', 'Export API to Postman');
    vItem1.Add('externalDocs', vItem2);
  end;

  Result.Add(vItem1);
end;

procedure TRALSwaggerExporter.RouteToJSON(AItem: TRALJSONObject;
  AServer : TRALServer; AModule : TRALModuleRoutes; ARoute: TRALRoute);
var
  vRoute, vAuth, vjMethod : TRALJSONObject;
  vAux1 : TRALJSONObject;
  vSecurity, vAuthTypes, vTags : TRALJSONArray;
  vMethod : TRALMethod;
begin
  vRoute := TRALJSONObject.Create;

  for vMethod := Low(TRALMethod) to High(TRALMethod) do
  begin
    if ARoute.IsMethodAllowed(vMethod) then begin
      vjMethod := TRALJSONObject.Create;

      if (AModule <> nil) and (AModule.Name <> '') then
      begin
        vTags := TRALJSONArray.Create;
        vTags.Add(AModule.Name);

        vjMethod.Add('tags', vTags);
      end
      else if (AModule = nil) and (AServer <> nil) and (AServer.Name <> '') then
      begin
        vTags := TRALJSONArray.Create;
        vTags.Add(AServer.Name);

        vjMethod.Add('tags', vTags);
      end;
      vjMethod.Add('summary', ARoute.Name);
      vjMethod.Add('description', Trim(ARoute.Description.Text));
      vjMethod.Add('operationId', RouteToOperation(ARoute.GetFullRoute));

      if (AServer.Authentication <> nil) and
         (not ARoute.IsMethodSkipped(vMethod)) then begin
        vSecurity := TRALJSONArray.Create;

        vAuth := TRALJSONObject.Create;
        vAuthTypes := TRALJSONArray.Create;
        vAuth.Add('ralAuth', vAuthTypes);

        vSecurity.Add(vAuth);
        vjMethod.Add('security', vSecurity);
      end;

      vAux1 := TRALJSONObject.Create;;
      vjMethod.Add('responses', vAux1);

      vRoute.Add(LowerCase(RALMethodToHTTPMethod(vMethod)), vjMethod);
    end;
  end;
  AItem.Add(ARoute.GetFullRoute, vRoute)
end;

function TRALSwaggerExporter.RouteToOperation(ARoute: StringRAL): StringRAL;
begin
  if (ARoute <> '') and (ARoute[POSINISTR] = '/') then
    Delete(ARoute, 1, 1);

  Result := StringReplace(ARoute, '/', '_', [rfReplaceAll]);
end;

end.
