unit RALPostmanExporter;

interface

uses
  Classes, SysUtils,
  RALJSON, RALServer, RALTypes, RALAuthentication, RALRoutes, RALTools;

type
  TRALPostmanExporter = class
  private
    FAllowCORSVerbs: boolean;
  protected
    function getInfo(AServer: TRALServer) : TRALJSONObject;
    function getVariables(AServer: TRALServer) : TRALJSONArray;
    function getItems(AServer: TRALServer) : TRALJSONArray;
    function getAuth(AServer: TRALServer) : TRALJSONObject;

    procedure RouteToJSON(AItem: TRALJSONArray; ARoute: TRALBaseRoute; AAuth: TRALAuthServer = nil);
  public
    procedure ExportToFile(AServer : TRALServer; AFileName : TFileName);
    procedure ExportToStream(AServer : TRALServer; AStream : TStream); overload;
    function ExportToStream(AServer : TRALServer) : TStream; overload;
  published
    property AllowCORSVerbs: boolean read FAllowCORSVerbs write FAllowCORSVerbs;
  end;

implementation

{ TRALPostmanExporter }

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

  // autenticacao
  if AServer.Authentication <> nil then
    RouteToJSON(Result, AServer.Authentication.AuthRoute, AServer.Authentication);

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

procedure TRALPostmanExporter.RouteToJSON(AItem : TRALJSONArray; ARoute: TRALBaseRoute;
                                          AAuth : TRALAuthServer);
var
  vRoute : TRALJSONObject;
  vStr: StringRAL;
  vAux1, vAux2, vAux5, vAux6 : TRALJSONObject;
  vAux3, vAux4 : TRALJSONArray;
  vMethod : TRALMethod;
  vFunc, vList : TStringList;
  vInt : IntegerRAL;
  vRouteParam : TRALRouteParam;
  vStrRoute : StringRAL;
begin
  if ARoute = nil then
    Exit;

  for vMethod := Low(TRALMethod) to High(TRALMethod) do
  begin
    if (ARoute.IsMethodAllowed(vMethod)) and (vMethod <> amALL) then begin
      if (vMethod = amOPTIONS) and (not FAllowCORSVerbs) then
        Continue;

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

      vStrRoute := ARoute.GetFullRoute;

      vAux3 := TRALJSONArray.Create;

      vList := TStringList.Create;
      try
        vList.DelimitedText := '/';
        vList.Text := ARoute.GetFullRoute;

        for vInt := 0 to Pred(vList.Count) do
          vAux3.Add(vList.Strings[vInt]);
      finally
        FreeAndNil(vList);
      end;

      vAux4 := nil;
      if ARoute.URIParams.Count > 0 then
        vAux4 := TRALJSONArray.Create;

      for vInt := 0 to Pred(ARoute.URIParams.Count) do
      begin
        vRouteParam := TRALRouteParam(ARoute.URIParams.Items[vInt]);
        vAux3.Add(Format(':%s', [vRouteParam.ParamName]));
        vStrRoute := Format('%s/:%s', [vStrRoute, vRouteParam.ParamName]);

        vAux5 := TRALJSONObject.Create;
        vAux5.Add('key', vRouteParam.ParamName);
        vAux5.Add('description', vRouteParam.Description.Text);

        vAux4.Add(vAux5);
      end;

      vAux2.Add('path', vAux3);

      vAux2.Add('raw', '{{ral_url}}' + vStrRoute);
      if vAux4 <> nil then
        vAux2.Add('variable', vAux4);

      case vMethod of
        amDELETE, amPOST, amPATCH, amPUT : begin
          if ARoute.InputParams.Count > 0 then
          begin
            vAux6 := TRALJSONObject.Create;
            vAux6.Add('mode', 'formdata');
            vAux3 := TRALJSONArray.Create;
          end;

          for vInt := 0 to Pred(ARoute.InputParams.Count) do
          begin
            vRouteParam := TRALRouteParam(ARoute.InputParams.Items[vInt]);

            vAux5 := TRALJSONObject.Create;
            vAux5.Add('key', vRouteParam.ParamName);
            vAux5.Add('description', vRouteParam.Description.Text);
            vAux5.Add('type', 'text');

            vAux3.Add(vAux5);
          end;

          if ARoute.InputParams.Count > 0 then
          begin
            vAux6.Add('formdata', vAux3);
            vAux1.Add('body', vAux6);
          end;
        end;
        amGET : begin
          if ARoute.InputParams.Count > 0 then
            vAux3 := TRALJSONArray.Create;

          for vInt := 0 to Pred(ARoute.InputParams.Count) do
          begin
            vRouteParam := TRALRouteParam(ARoute.InputParams.Items[vInt]);

            vAux5 := TRALJSONObject.Create;
            vAux5.Add('key', vRouteParam.ParamName);
            vAux5.Add('description', vRouteParam.Description.Text);

            vAux3.Add(vAux5);
          end;

          if ARoute.InputParams.Count > 0 then
            vAux2.Add('query', vAux3);
        end;
      end;

      vAux1.Add('url', vAux2);
      vAux1.Add('method', RALMethodToHTTPMethod(vMethod));

      vRoute.Add('request', vAux1);

      if (AAuth <> nil) and (AAuth is TRALServerJWTAuth) and
         (vMethod <> amOPTIONS) then
      begin
        // event
        vAux3 := TRALJSONArray.Create;

        vAux1 := TRALJSONObject.Create;
        vAux1.Add('listen', 'test');
        vAux1.Add('type', 'text/javascript');

        vAux4 := TRALJSONArray.Create;

        vFunc := TStringList.Create;
        try
          vStr := TRALServerJWTAuth(AAuth).JSONKey;
          vFunc.Add('pm.test("setToken", function () {');
          vFunc.Add('    var jsonData = pm.response.json();');
          vFunc.Add('    pm.collectionVariables.set(token, jsonData.' + vStr + ');');
          vFunc.Add('});');

          vAux4.Add(vFunc.Text);
        finally
          FreeAndNil(vFunc);
        end;

        vAux2 := TRALJSONObject.Create;
        vAux2.Add('exec', vAux4);

        vAux1.Add('script', vAux2);

        vAux3.Add(vAux1);
        vRoute.Add('event', vAux3);
      end;

      AItem.Add(vRoute)
    end;
  end;
end;

end.
