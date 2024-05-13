unit RALPostmanExporter;

interface

uses
  Classes, SysUtils,
  RALJSON, RALServer, RALTypes, RALAuthentication, RALRoutes, RALTools;

type
  TRALPostmanExporter = class
  protected
    function getInfo(AServer: TRALServer) : TRALJSONObject;
    function getVariables(AServer: TRALServer) : TRALJSONArray;
    function getItems(AServer: TRALServer) : TRALJSONArray;
    function getAuth(AServer: TRALServer) : TRALJSONObject;

    procedure RouteToJSON(AItem : TRALJSONArray; ARoute : TRALRoute);
    procedure AuthToRoute(AItem : TRALJSONArray; AAuth : TRALAuthServer);
  public
    procedure ExportToFile(AServer : TRALServer; AFileName : TFileName);
    procedure ExportToStream(AServer : TRALServer; AStream : TStream); overload;
    function ExportToStream(AServer : TRALServer) : TStream; overload;
  end;

implementation

{ TRALPostmanExporter }

procedure TRALPostmanExporter.AuthToRoute(AItem: TRALJSONArray; AAuth: TRALAuthServer);
var
  vRoute : TRALJSONObject;
  vAux1, vAux2 : TRALJSONObject;
  vAux3, vAux4 : TRALJSONArray;
  vFunc : TStringList;
begin
  if AAuth = nil then
    Exit;

  if AAuth is TRALServerJWTAuth then
  begin
    vRoute := TRALJSONObject.Create;

    vRoute.Add('name', 'getToken');

    // event
    vAux3 := TRALJSONArray.Create;

    vAux1 := TRALJSONObject.Create;
    vAux1.Add('listen', 'test');
    vAux1.Add('type', 'text/javascript');

    vAux4 := TRALJSONArray.Create;

    vFunc := TStringList.Create;
    vFunc.Add('pm.test("setToken", function () {');
    vFunc.Add('    var jsonData = pm.response.json();');
    vFunc.Add('    pm.collectionVariables.set("token",jsonData.token);');
    vFunc.Add('});');

    vAux4.Add(vFunc.Text);

    vAux2 := TRALJSONObject.Create;
    vAux2.Add('exec', vAux4);

    vAux1.Add('script', vAux2);

    vAux3.Add(vAux1);
    vRoute.Add('event', vAux3);

    // url - request
    vAux1 := TRALJSONObject.Create;
    vAux2 := TRALJSONObject.Create;
    vAux2.Add('type', 'noauth');

    vAux1.Add('auth', vAux2);

    vAux2 := TRALJSONObject.Create;
    vAux3 := TRALJSONArray.Create;
    vAux3.Add('{{ral_url}}');
    vAux2.Add('host', vAux3);

    vAux3 := TRALJSONArray.Create;
    vAux3.Add(TRALServerJWTAuth(AAuth).Route);
    vAux2.Add('path', vAux3);

    vAux2.Add('raw', '{{ral_url}}' + TRALServerJWTAuth(AAuth).Route);

    vAux1.Add('url', vAux2);

    vAux1.Add('method', 'POST');

    vRoute.Add('request', vAux1);

    AItem.Add(vRoute)
  end;
end;

procedure TRALPostmanExporter.ExportToFile(AServer: TRALServer; AFileName: TFileName);
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

function TRALPostmanExporter.ExportToStream(AServer: TRALServer): TStream;
begin
  Result := TMemoryStream.Create;
  ExportToStream(AServer, Result);
end;

procedure TRALPostmanExporter.ExportToStream(AServer: TRALServer; AStream: TStream);
var
  vJson, vAuth : TRALJSONObject;
  vStr : StringRAL;
begin
  vJson := TRALJSONObject.Create;
  try
    vJson.Add('info', getInfo(AServer));
    vJson.Add('variable', getVariables(AServer));
    vJson.Add('item', getItems(AServer));

    vAuth := getAuth(AServer);
    if vAuth <> nil then
      vJson.Add('auth', vAuth);

    vStr := vJson.ToJSON;
  finally
    FreeAndNil(vJson);
  end;

  AStream.Write(vStr[POSINISTR], Length(vStr));
end;

function TRALPostmanExporter.getAuth(AServer: TRALServer): TRALJSONObject;
var
  vVar : TRALJSONObject;
  vItens : TRALJSONArray;
begin
  Result := TRALJSONObject.Create;
  if AServer.Authentication is TRALServerBasicAuth then
  begin
    Result.Add('type', 'basic');
    vItens := TRALJSONArray.Create;

    vVar := TRALJSONObject.Create;
    vVar.Add('key', 'password');
    vVar.Add('type', 'string');
    vVar.Add('value', '{{pass}}');

    vItens.Add(vVar);

    vVar := TRALJSONObject.Create;
    vVar.Add('key', 'username');
    vVar.Add('type', 'string');
    vVar.Add('value', '{{user}}');

    vItens.Add(vVar);
    Result.Add('basic', vItens);
  end
  else if AServer.Authentication is TRALServerJWTAuth then
  begin
    Result.Add('type', 'bearer');

    vItens := TRALJSONArray.Create;

    vVar := TRALJSONObject.Create;
    vVar.Add('key', 'token');
    vVar.Add('type', 'string');
    vVar.Add('value', '{{token}}');

    vItens.Add(vVar);
    Result.Add('bearer', vItens);
  end;
end;

function TRALPostmanExporter.getInfo(AServer: TRALServer) : TRALJSONObject;
var
  vSchema : StringRAL;
  vpGUID : TGUID;
begin
  vSchema := 'https://schema.getpostman.com/json/collection/v2.1.0/collection.json';

  CreateGUID(vpGUID);

  Result := TRALJSONObject.Create;
  Result.Add('_exporter_id', IntToStr(Integer(Pointer(AServer))));
  Result.Add('_postman_id', GUIDToString(vpGUID));
  Result.Add('name', AServer.Name);
  Result.Add('schema', vSchema);
end;

function TRALPostmanExporter.getItems(AServer: TRALServer): TRALJSONArray;
var
  vInt1, vInt2 : IntegerRAL;
  vRoute : TRALRoute;
  vSubRoutes : TList;
begin
  Result := TRALJSONArray.Create;

  AuthToRoute(Result, AServer.Authentication);

  // adicionando rotas do server
  for vInt1 := 0 to Pred(AServer.Routes.Count) do
  begin
    vRoute := TRALRoute(AServer.Routes.Items[vInt1]);
    RouteToJSON(Result, vRoute);
  end;

  // adicionando submodules
  for vInt1 := 0 to Pred(AServer.CountSubModules) do
  begin
    vSubRoutes := AServer.SubModule[vInt1].GetListRoutes;
    try
      for vInt2 := 0 to Pred(vSubRoutes.Count) do
      begin
        vRoute := TRALRoute(vSubRoutes.Items[vInt2]);
        RouteToJSON(Result, vRoute);
      end;
    finally
      FreeAndNil(vSubRoutes);
    end;
  end;
end;

function TRALPostmanExporter.getVariables(AServer: TRALServer): TRALJSONArray;
var
  vVar : TRALJSONObject;
begin
  Result := TRALJSONArray.Create;

  vVar := TRALJSONObject.Create;
  vVar.Add('key', 'ral_url');
  vVar.Add('type', 'string');
  vVar.Add('value', 'http://localhost:' + IntToStr(AServer.Port));

  Result.Add(vVar);

  if AServer.Authentication <> nil then
  begin
    if AServer.Authentication is TRALServerBasicAuth then
    begin
      vVar := TRALJSONObject.Create;
      vVar.Add('key', 'user');
      vVar.Add('type', 'string');
      vVar.Add('value', TRALServerBasicAuth(AServer.Authentication).UserName);

      Result.Add(vVar);

      vVar := TRALJSONObject.Create;
      vVar.Add('key', 'pass');
      vVar.Add('type', 'string');
      vVar.Add('value', TRALServerBasicAuth(AServer.Authentication).Password);

      Result.Add(vVar);
    end
    else if AServer.Authentication is TRALServerJWTAuth then
    begin
      vVar := TRALJSONObject.Create;
      vVar.Add('key', 'token');
      vVar.Add('type', 'string');
      vVar.Add('value', '');

      Result.Add(vVar);
    end;
  end;
end;

procedure TRALPostmanExporter.RouteToJSON(AItem : TRALJSONArray; ARoute: TRALRoute);
var
  vRoute : TRALJSONObject;
  vAux1, vAux2 : TRALJSONObject;
  vAux3 : TRALJSONArray;
  vMethod : TRALMethod;
begin
  for vMethod := Low(TRALMethod) to High(TRALMethod) do
  begin
    if ARoute.IsMethodAllowed(vMethod) then begin
      vRoute := TRALJSONObject.Create;

      vRoute.Add('name', ARoute.Name);

      vAux1 := TRALJSONObject.Create;
      if ARoute.IsMethodSkipped(vMethod) then
      begin
        vAux2 := TRALJSONObject.Create;
        vAux2.Add('type', 'noauth');

        vAux1.Add('auth', vAux2);
      end;

      vAux2 := TRALJSONObject.Create;
      vAux3 := TRALJSONArray.Create;
      vAux3.Add('{{ral_url}}');
      vAux2.Add('host', vAux3);

      vAux3 := TRALJSONArray.Create;
      vAux3.Add(ARoute.GetFullRoute);
      vAux2.Add('path', vAux3);

      vAux2.Add('raw', '{{ral_url}}' + ARoute.GetFullRoute);

      vAux1.Add('url', vAux2);
      vAux1.Add('method', RALMethodToHTTPMethod(vMethod));

      vRoute.Add('request', vAux1);

      AItem.Add(vRoute)
    end;
  end;
end;

end.
