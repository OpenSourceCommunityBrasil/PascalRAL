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
    FSwaggerModule: TRALSwaggerModule;
    FSchemas: TStrings;
  protected
    function getInfo(AServer: TRALServer): TRALJSONObject;
    function getServers(AServer: TRALServer): TRALJSONArray;
    function getTags(AServer: TRALServer): TRALJSONArray;
    function getPaths(AServer: TRALServer): TRALJSONObject;
    function getComponents(AServer: TRALServer): TRALJSONObject;
    function getSecurity(AAuth: TRALAuthServer): TRALJSONObject;

    procedure RouteToJSON(AItem: TRALJSONObject; AServer: TRALServer;
                          AModule: TRALModuleRoutes; ARoute: TRALBaseRoute);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ExportToFile(AServer: TRALServer; AFileName: TFileName);
    procedure ExportToStream(AServer: TRALServer; AStream: TStream); overload;
    function ExportToStream(AServer: TRALServer): TStream; overload;
  published
    property SwaggerModule: TRALSwaggerModule read FSwaggerModule write FSwaggerModule;
  end;

implementation

{ TRALSwaggerExporter }

procedure TRALSwaggerExporter.ExportToFile(AServer: TRALServer; AFileName: TFileName);
var
  vFile: TFileStream;
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
  vJson, vAux: TRALJSONObject;
  vArr: TRALJSONArray;
  vBytes: TBytes;
begin
  vJson := TRALJSONObject.Create;
  try
    vJson.Add('openapi', '3.1.0');
    vJson.Add('info', getInfo(AServer));

    vArr := getServers(AServer);
    if vArr <> nil then
      vJson.Add('servers', vArr);

    if SwaggerModule.PostmanTag then
      vJson.Add('tags', getTags(AServer));
    vJson.Add('paths', getPaths(AServer));

    vAux := getComponents(AServer);
    if vAux <> nil then
      vJson.Add('components', vAux);

    vBytes := StringToBytesUTF8(vJson.ToJSON);
    AStream.Write(vBytes[0], Length(vBytes));
    AStream.Position := 0;
  finally
    FreeAndNil(vJson);
  end;
end;

function TRALSwaggerExporter.ExportToStream(AServer: TRALServer): TStream;
begin
  Result := TMemoryStream.Create;
  ExportToStream(AServer, Result);
end;

function TRALSwaggerExporter.getComponents(AServer: TRALServer): TRALJSONObject;
var
  vSchemas: TRALJSONObject;
begin
  Result := nil;
  if (FSchemas.Count = 0) and (AServer.Authentication = nil) then
    Exit;

  Result := TRALJSONObject.Create;

  vSchemas := nil;
  if FSchemas.Count > 0 then
    vSchemas := TRALJSONObject.Create;

  while FSchemas.Count > 0 do
  begin
    vSchemas.Add(FSchemas.Strings[0], TRALJSONObject(FSchemas.Objects[0]));
    FSchemas.Delete(0);
  end;

  if vSchemas <> nil then
    Result.Add('schemas', vSchemas);

  if AServer.Authentication <> nil then
    Result.Add('securitySchemes', getSecurity(AServer.Authentication));
end;

function TRALSwaggerExporter.getInfo(AServer: TRALServer): TRALJSONObject;
var
  vjAux1: TRALJSONObject;
  vAux: StringRAL;
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
  vInt1, vInt2: IntegerRAL;
  vRoute: TRALRoute;
  vSubRoutes: TList;
begin
  Result := TRALJSONObject.Create;

  // authentication
  if AServer.Authentication <> nil then
    RouteToJSON(Result, AServer, nil, AServer.Authentication.AuthRoute);

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
  vAuth: TRALJSONObject;
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
  vItem: TRALJSONObject;
  vInt: IntegerRAL;
begin
  Result := nil;
  if FSwaggerModule.ServersUrl.Count > 0 then
  begin
    Result := TRALJSONArray.Create;

    for vInt := 0 to Pred(FSwaggerModule.ServersUrl.Count) do
    begin
      vItem := TRALJSONObject.Create;
      vItem.Add('url', FSwaggerModule.ServersUrl.Strings[vInt]);

      Result.Add(vItem);
    end;
  end;
end;

function TRALSwaggerExporter.getTags(AServer: TRALServer): TRALJSONArray;
var
  vItem1, vItem2: TRALJSONObject;
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

procedure TRALSwaggerExporter.RouteToJSON(AItem: TRALJSONObject; AServer: TRALServer;
  AModule: TRALModuleRoutes; ARoute: TRALBaseRoute);
var
  vRoute, vAuth, vjMethod: TRALJSONObject;
  vAux1, vAux2: TRALJSONObject;
  vSecurity, vAuthTypes, vTags: TRALJSONArray;
  vMethod: TRALMethod;
  vStrMethod, vStrOperation, vStrRoute, vStrAux: StringRAL;
  vInt: IntegerRAL;
  vRouteParam: TRALRouteParam;
  vURIParams: TStringList;

  function getParameterStr(AParam: StringRAL; AType: StringRAL): TRALJSONObject;
  var
    rbAux1: TRALJSONObject;
  begin
    Result := TRALJSONObject.Create;
    Result.Add('name', AParam);
    Result.Add('in', AType);
    Result.Add('required', True);

    rbAux1 := TRALJSONObject.Create;
    rbAux1.Add('type', 'string');

    Result.Add('schema', rbAux1);
  end;

  function getParameter(AParam: TRALRouteParam; AType: StringRAL): TRALJSONObject;
  var
    rbAux1: TRALJSONObject;
  begin
    Result := TRALJSONObject.Create;
    Result.Add('name', AParam.ParamName);
    Result.Add('in', AType);
    Result.Add('description', AParam.Description.Text);
    Result.Add('required', (AParam.Required) or (AType = 'path'));

    rbAux1 := TRALJSONObject.Create;
    case AParam.ParamType of
      prtBoolean:
        rbAux1.Add('type', 'boolean');
      prtInteger:
        rbAux1.Add('type', 'integer');
      prtNumber:
        rbAux1.Add('type', 'number');
      prtString:
        rbAux1.Add('type', 'string');
    end;
    // rbAux2.Add('example', '');

    Result.Add('schema', rbAux1);
  end;

  procedure getSchemaParameters;
  var
    spAux1, spAux2, spAux3: TRALJSONObject;
    spArr1: TRALJSONArray;
    spInt: IntegerRAL;
    spParam: TRALRouteParam;
  begin
    if FSchemas.IndexOf(LowerCase(ARoute.Name)) >= 0 then
      Exit;

    spAux1 := TRALJSONObject.Create;
    spAux1.Add('type', 'object');
    spAux1.Add('description', 'Schema of Route ' + ARoute.Name);

    spAux2 := TRALJSONObject.Create;
    spArr1 := nil;

    for spInt := 0 to Pred(ARoute.InputParams.Count) do
    begin
      spParam := TRALRouteParam(ARoute.InputParams.Items[spInt]);

      spAux3 := TRALJSONObject.Create;
      case spParam.ParamType of
        prtBoolean:
          spAux3.Add('type', 'boolean');
        prtInteger:
          spAux3.Add('type', 'integer');
        prtNumber:
          spAux3.Add('type', 'number');
        prtString:
          spAux3.Add('type', 'string');
      end;
      spAux3.Add('description', spParam.Description.Text);
      // spAux3.Add('example', '');

      spAux2.Add(spParam.ParamName, spAux3);

      if (spParam.Required) then
      begin
        if spArr1 = nil then
          spArr1 := TRALJSONArray.Create;

        spArr1.Add(spParam.ParamName);
      end
    end;
    spAux1.Add('properties', spAux2);

    if spArr1 <> nil then
      spAux1.Add('required', spArr1);

    FSchemas.AddObject(LowerCase(ARoute.Name), spAux1);
  end;

  procedure getRequestParameters;
  var
    rbAux1, rbAux2, rbAux3, rbAux4: TRALJSONObject;
    rbParameters: TRALJSONArray;
    rbInt, rbIdx: IntegerRAL;
    rbStr1: StringRAL;
    rbParam: TRALRouteParam;
  begin
    rbParameters := nil;

    case vMethod of
      amDELETE, amPOST, amPATCH, amPUT:
        begin
          if ARoute.InputParams.Count > 0 then
          begin
            rbAux1 := TRALJSONObject.Create;
            rbAux1.Add('$ref', '#/components/schemas/' + LowerCase(ARoute.Name));

            rbAux2 := TRALJSONObject.Create;
            rbAux2.Add('schema', rbAux1);

            rbAux3 := TRALJSONObject.Create;
            rbAux3.Add('multipart/form-data', rbAux2);

            rbAux4 := TRALJSONObject.Create;
            rbAux4.Add('content', rbAux3);

            vjMethod.Add('requestBody', rbAux4);

            getSchemaParameters;
          end;
        end;
      amGET:
        begin
          if (rbParameters = nil) and (ARoute.InputParams.Count > 0) then
            rbParameters := TRALJSONArray.Create;

          for rbInt := 0 to Pred(ARoute.InputParams.Count) do
          begin
            rbParam := TRALRouteParam(ARoute.InputParams.Items[rbInt]);

            rbAux1 := getParameter(rbParam, 'query');
            rbParameters.Add(rbAux1);
          end;
        end;
    end;

    if (rbParameters = nil) and (vURIParams.Count > 0) then
      rbParameters := TRALJSONArray.Create;

    for rbInt := 0 to Pred(vURIParams.Count) do
    begin
      rbStr1 := vURIParams.Strings[rbInt];
      rbIdx := ARoute.URIParams.IndexOf(rbStr1);
      if rbIdx >= 0 then
        rbAux1 := getParameter(TRALRouteParam(ARoute.URIParams.Items[rbIdx]), 'path')
      else
        rbAux1 := getParameterStr(vURIParams.Strings[rbInt], 'path');
      rbParameters.Add(rbAux1);
    end;

    if rbParameters <> nil then
      vjMethod.Add('parameters', rbParameters)
  end;

begin
  if ARoute = nil then
    Exit;

  vStrRoute := ARoute.GetFullRoute;
  if (vStrRoute <> '') and (vStrRoute[POSINISTR] = '/') then
    System.Delete(vStrRoute, POSINISTR, 1);

  vURIParams := TStringList.Create;
  try
    // convertendo a route em operation
    vStrOperation := vStrRoute;
    vStrOperation := StringReplace(vStrOperation, '/', '_', [rfReplaceAll]);
    vStrOperation := StringReplace(vStrOperation, ':', '', [rfReplaceAll]);

    vURIParams.LineBreak := '/';
    vURIParams.Text := vStrRoute;
    vStrRoute := '';
    vInt := 0;
    while vInt < vURIParams.Count do
    begin
      vStrAux := vURIParams.Strings[vInt];
      if (vStrAux <> '') and (vStrAux[POSINISTR] = ':') then
      begin
        System.Delete(vStrAux, POSINISTR, 1);
        vURIParams.Strings[vInt] := vStrAux;
        vStrAux := '{' + vStrAux + '}';
        vInt := vInt + 1;
      end
      else
      begin
        vURIParams.Delete(vInt);
      end;
      vStrRoute := vStrRoute + '/' + vStrAux;
    end;

    vRoute := TRALJSONObject.Create;

    for vMethod := Low(TRALMethod) to High(TRALMethod) do
    begin
      if (ARoute.IsMethodAllowed(vMethod)) and (vMethod <> amALL) then
      begin
        if (vMethod = amOPTIONS) and (not SwaggerModule.AllowCORSVerbs) then
          Continue;

        vjMethod := TRALJSONObject.Create;

        vStrMethod := LowerCase(RALMethodToHTTPMethod(vMethod));

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

        if FSwaggerModule.ShowCustomNames then
          vjMethod.Add('summary', ARoute.Name);

        vjMethod.Add('description', Trim(ARoute.Description.Text));
        vjMethod.Add('operationId', vStrOperation + '_' + vStrMethod);

        getRequestParameters;

        if (AServer.Authentication <> nil) and (not ARoute.IsMethodSkipped(vMethod)) then
        begin
          vSecurity := TRALJSONArray.Create;

          vAuth := TRALJSONObject.Create;
          vAuthTypes := TRALJSONArray.Create;
          vAuth.Add('ralAuth', vAuthTypes);

          vSecurity.Add(vAuth);
          vjMethod.Add('security', vSecurity);
        end;

        vAux1 := TRALJSONObject.Create;;
        vAux2 := TRALJSONObject.Create;;
        vAux2.Add('description', 'successful operation');

        vAux1.Add('200', vAux2);
        vjMethod.Add('responses', vAux1);

        vRoute.Add(vStrMethod, vjMethod);
      end;
    end;
    if assigned(AItem.Get(vStrRoute)) then // rota já existe, insere novo método
      TRALJSONObject(AItem.Get(vStrRoute)).Add(vStrMethod, vjMethod)
    else
      AItem.Add(vStrRoute, vRoute)
  finally
    FreeAndNil(vURIParams);
  end;
end;

constructor TRALSwaggerExporter.Create;
begin
  inherited;
  FSchemas := TStringList.Create;
end;

destructor TRALSwaggerExporter.Destroy;
begin
  FreeAndNil(FSchemas);
  inherited Destroy;
end;

end.
