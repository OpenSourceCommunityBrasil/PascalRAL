(* GraphQL over HTTP: TRALGraphQLModule serves one endpoint (Path) with the schema
  written in SDL (Schema) and the resolvers set by type and field (Resolvers),
  queries by GET and POST (application/json or application/graphql), mutations by
  POST only, the depth and cost limits, and the GraphiQL page for a browser. *)
unit RALGraphQLModule;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs, Contnrs,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALCustomObjects, RALServer, RALRoutes, RALRequest,
  RALResponse, RALParams, RALJson, RALGraphQLAST, RALGraphQLExecutor;

const
  /// TRALGraphQLModule.Path when nobody says otherwise.
  RALGraphQLDefaultPath = '/graphql';
  /// TRALGraphQLModule.MaxDepth when nobody says otherwise.
  RALGraphQLDefaultMaxDepth = 15;
  /// TRALGraphQLModule.MaxComplexity when nobody says otherwise.
  RALGraphQLDefaultMaxComplexity = 1000;
  /// The media type of a GraphQL answer (GraphQL over HTTP).
  rctGRAPHQLRESPONSE = 'application/graphql-response+json';

type
  TRALGraphQLModule = class;

  { Something that adds to a module's schema (types, fields by extend type, and
    their resolvers) every time it is built: TRALGraphQLDBBinding. It joins with
    TRALGraphQLModule.AddContributor and leaves with RemoveContributor }
  TRALGraphQLContributor = class(TRALComponent)
  public
    /// Adds SDL to ASDL (the module's own is already there).
    procedure ContributeSDL(ASDL: TStrings); virtual; abstract;
    /// Sets the resolvers of what it added.
    procedure ContributeResolvers(ASchema: TRALGraphQLSchema); virtual; abstract;
  end;

  /// The resolver of one field (or of every field of a type).
  TRALGraphQLResolver = class(TCollectionItem)
  private
    FFieldName: StringRAL;
    FOnResolve: TRALGraphQLOnResolve;
    FTypeName: StringRAL;
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(ASource: TPersistent); override;
  published
    /// The field; empty is every field of the type that has no resolver of its own.
    property FieldName: StringRAL read FFieldName write FFieldName;
    /// Resolves the field.
    property OnResolve: TRALGraphQLOnResolve read FOnResolve write FOnResolve;
    /// The type that has the field ('Query', 'Mutation', 'User').
    property TypeName: StringRAL read FTypeName write FTypeName;
  end;

  /// The resolvers of a module.
  TRALGraphQLResolvers = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALGraphQLResolver;
  public
    /// The resolvers of AOwner.
    constructor Create(AOwner: TPersistent);

    /// Adds the resolver of ATypeName.AFieldName; returns it.
    function AddResolver(const ATypeName, AFieldName: StringRAL;
      AOnResolve: TRALGraphQLOnResolve): TRALGraphQLResolver;

    /// The resolvers by index.
    property Items[AIndex: IntegerRAL]: TRALGraphQLResolver read GetItem; default;
  end;

  { A GraphQL endpoint: GET and POST at Path. The schema is the SDL of Schema,
    built (and checked) when the server starts or on the first request; the
    fields read the value their parent resolved unless Resolvers has one for
    them. A GET from a browser without a query gets GraphiQL }
  TRALGraphQLModule = class(TRALModuleRoutes)
  private
    FBuilt: TRALGraphQLSchema;
    FBuildError: StringRAL;
    FContext: TObject;
    /// TRALGraphQLContributor that add to the schema.
    FContributors: TList;
    FGraphiQL: boolean;
    /// Guards FBuilt.
    FLock: TCriticalSection;
    FMaxComplexity: IntegerRAL;
    FMaxDepth: IntegerRAL;
    FOnResolveType: TRALGraphQLOnResolveType;
    FResolvers: TRALGraphQLResolvers;
    /// Schemas replaced, kept until the module dies (a request may be using one).
    FRetired: TObjectList;
    FRoute: TRALRoute;
    FSchema: TStrings;

    function GetPath: StringRAL;
    /// Answers GET and POST.
    procedure Reply(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure SetPath(const AValue: StringRAL);
    procedure SetResolvers(const AValue: TRALGraphQLResolvers);
    procedure SetSchema(const AValue: TStrings);
    procedure SchemaChanged(Sender: TObject);
  protected
    procedure ServerActivating; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Adds AContributor to the schema (built again on the next request).
    procedure AddContributor(AContributor: TRALGraphQLContributor);
    /// Takes AContributor out.
    procedure RemoveContributor(AContributor: TRALGraphQLContributor);
    { Runs AQuery (with AVariables, a JSON object as text or empty, and
      AOperationName) and gives the answer as JSON text; ARequest goes to the
      resolvers (nil outside HTTP) }
    function Execute(const AQuery: StringRAL; const AVariables: StringRAL = '';
      const AOperationName: StringRAL = ''; ARequest: TRALRequest = nil;
      AAllowMutation: boolean = True): StringRAL;
    /// The built schema (built now if needed); raises when the SDL is wrong.
    function GetSchema: TRALGraphQLSchema;
    /// Builds the schema again from Schema and Resolvers (after changing them in code).
    procedure Reload;

    /// The application's object, given to every resolver (TRALGraphQLResolve.Context).
    property Context: TObject read FContext write FContext;
  published
    /// Serves GraphiQL to a browser's GET without a query.
    property GraphiQL: boolean read FGraphiQL write FGraphiQL default True;
    { Most fields an operation may select (introspection left out); 0 is no
      limit. Over it, the operation is refused before it runs }
    property MaxComplexity: IntegerRAL read FMaxComplexity write FMaxComplexity
      default RALGraphQLDefaultMaxComplexity;
    /// Deepest nesting of fields an operation may have; 0 is no limit.
    property MaxDepth: IntegerRAL read FMaxDepth write FMaxDepth default RALGraphQLDefaultMaxDepth;
    /// See TRALGraphQLOnResolveType.
    property OnResolveType: TRALGraphQLOnResolveType read FOnResolveType write FOnResolveType;
    /// Path of the endpoint, under Domain.
    property Path: StringRAL read GetPath write SetPath;
    /// The resolvers, by type and field.
    property Resolvers: TRALGraphQLResolvers read FResolvers write SetResolvers;
    /// The schema in SDL ('type Query { hello: String }').
    property Schema: TStrings read FSchema write SetSchema;
  end;

/// The GraphiQL page for the endpoint at AURL.
function RALGraphiQLPage(const AURL: StringRAL): StringRAL;

implementation

function RALGraphiQLPage(const AURL: StringRAL): StringRAL;
begin
  Result :=
    '<!DOCTYPE html><html lang="en"><head><meta charset="utf-8"><title>GraphiQL</title>' +
    '<link rel="stylesheet" href="https://unpkg.com/graphiql@3/graphiql.min.css"></head>' +
    '<body style="margin:0"><div id="graphiql" style="height:100vh"></div>' +
    '<script crossorigin src="https://unpkg.com/react@18/umd/react.production.min.js"></script>' +
    '<script crossorigin src="https://unpkg.com/react-dom@18/umd/react-dom.production.min.js"></script>' +
    '<script crossorigin src="https://unpkg.com/graphiql@3/graphiql.min.js"></script>' +
    '<script>const fetcher = GraphiQL.createFetcher({ url: ' + RALGraphQLQuote(AURL) + ' });' +
    'ReactDOM.createRoot(document.getElementById("graphiql")).render(' +
    'React.createElement(GraphiQL, { fetcher: fetcher }));</script></body></html>';
end;

{ TRALGraphQLResolver }

procedure TRALGraphQLResolver.Assign(ASource: TPersistent);
begin
  if ASource is TRALGraphQLResolver then
  begin
    FTypeName := TRALGraphQLResolver(ASource).FTypeName;
    FFieldName := TRALGraphQLResolver(ASource).FFieldName;
    FOnResolve := TRALGraphQLResolver(ASource).FOnResolve;
  end
  else
    inherited Assign(ASource);
end;

function TRALGraphQLResolver.GetDisplayName: string;
begin
  Result := string(FTypeName + '.' + FFieldName);
  if Result = '.' then
    Result := inherited GetDisplayName;
end;

{ TRALGraphQLResolvers }

constructor TRALGraphQLResolvers.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALGraphQLResolver);
end;

function TRALGraphQLResolvers.GetItem(AIndex: IntegerRAL): TRALGraphQLResolver;
begin
  Result := TRALGraphQLResolver(inherited Items[AIndex]);
end;

function TRALGraphQLResolvers.AddResolver(const ATypeName, AFieldName: StringRAL;
  AOnResolve: TRALGraphQLOnResolve): TRALGraphQLResolver;
begin
  Result := TRALGraphQLResolver(Add);
  Result.TypeName := ATypeName;
  Result.FieldName := AFieldName;
  Result.OnResolve := AOnResolve;
end;

{ TRALGraphQLModule }

constructor TRALGraphQLModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FRetired := TObjectList.Create(True);
  FContributors := TList.Create;
  FResolvers := TRALGraphQLResolvers.Create(Self);
  FSchema := TStringList.Create;
  TStringList(FSchema).OnChange := {$IFDEF FPC}@{$ENDIF}SchemaChanged;
  FGraphiQL := True;
  FMaxDepth := RALGraphQLDefaultMaxDepth;
  FMaxComplexity := RALGraphQLDefaultMaxComplexity;
  // the routes are the module's own: Routes is not published
  FRoute := CreateRoute(RALGraphQLDefaultPath, {$IFDEF FPC}@{$ENDIF}Reply);
  FRoute.AllowedMethods := [amGET, amPOST, amHEAD, amOPTIONS];
end;

destructor TRALGraphQLModule.Destroy;
begin
  FreeAndNil(FBuilt);
  FreeAndNil(FRetired);
  FreeAndNil(FContributors);
  FreeAndNil(FResolvers);
  FreeAndNil(FSchema);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TRALGraphQLModule.GetPath: StringRAL;
begin
  Result := FRoute.Route;
end;

procedure TRALGraphQLModule.SetPath(const AValue: StringRAL);
begin
  if AValue = '' then
    FRoute.Route := RALGraphQLDefaultPath
  else
    FRoute.Route := AValue;
end;

procedure TRALGraphQLModule.SetResolvers(const AValue: TRALGraphQLResolvers);
begin
  FResolvers.Assign(AValue);
end;

procedure TRALGraphQLModule.SetSchema(const AValue: TStrings);
begin
  FSchema.Assign(AValue);
end;

procedure TRALGraphQLModule.SchemaChanged(Sender: TObject);
begin
  // built again on the next request
  FLock.Acquire;
  try
    if FBuilt <> nil then
      FRetired.Add(FBuilt);
    FBuilt := nil;
    FBuildError := '';
  finally
    FLock.Release;
  end;
end;

procedure TRALGraphQLModule.AddContributor(AContributor: TRALGraphQLContributor);
begin
  if FContributors.IndexOf(AContributor) < 0 then
    FContributors.Add(AContributor);
  SchemaChanged(nil);
end;

procedure TRALGraphQLModule.RemoveContributor(AContributor: TRALGraphQLContributor);
begin
  if FContributors <> nil then
    FContributors.Remove(AContributor);
  if FLock <> nil then
    SchemaChanged(nil);
end;

procedure TRALGraphQLModule.Reload;
begin
  SchemaChanged(nil);
  GetSchema;
end;

procedure TRALGraphQLModule.ServerActivating;
begin
  inherited ServerActivating;
  // a wrong SDL stops the server from starting, where it is seen
  GetSchema;
end;

function TRALGraphQLModule.GetSchema: TRALGraphQLSchema;
var
  vSchema: TRALGraphQLSchema;
  vSDL: TStringList;
  vInt: IntegerRAL;
  vItem: TRALGraphQLResolver;
begin
  FLock.Acquire;
  try
    if FBuilt = nil then
    begin
      vSDL := TStringList.Create;
      try
        vSDL.Assign(FSchema);
        for vInt := 0 to FContributors.Count - 1 do
          TRALGraphQLContributor(FContributors[vInt]).ContributeSDL(vSDL);
        vSchema := TRALGraphQLSchema.Create(StringRAL(vSDL.Text));
      finally
        vSDL.Free;
      end;
      try
        for vInt := 0 to FContributors.Count - 1 do
          TRALGraphQLContributor(FContributors[vInt]).ContributeResolvers(vSchema);
        for vInt := 0 to FResolvers.Count - 1 do
        begin
          vItem := FResolvers[vInt];
          if vSchema.FindType(vItem.TypeName) = nil then
            raise Exception.Create('Resolver of an unknown type "' + string(vItem.TypeName) + '".');
          if (vItem.FieldName <> '') and (vSchema.FindField(vItem.TypeName, vItem.FieldName) = nil) then
            raise Exception.Create('Resolver of an unknown field "' + string(vItem.TypeName) + '.' +
              string(vItem.FieldName) + '".');
          vSchema.SetResolver(vItem.TypeName, vItem.FieldName, vItem.OnResolve);
        end;
      except
        vSchema.Free;
        raise;
      end;
      FBuilt := vSchema;
    end;
    Result := FBuilt;
  finally
    FLock.Release;
  end;
end;

function TRALGraphQLModule.Execute(const AQuery, AVariables, AOperationName: StringRAL;
  ARequest: TRALRequest; AAllowMutation: boolean): StringRAL;
var
  vExec: TRALGraphQLExecution;
  vVars: TRALJSON;
begin
  vVars := nil;
  vExec := TRALGraphQLExecution.Create(GetSchema, AQuery);
  try
    if RALTrim(AVariables) <> '' then
    begin
      vVars := TRALJSON.ParseJSON(AVariables);
      if vVars is TRALJSONObject then
        vExec.Variables := TRALJSONObject(vVars);
    end;
    vExec.OperationName := AOperationName;
    vExec.MaxDepth := FMaxDepth;
    vExec.MaxComplexity := FMaxComplexity;
    vExec.AllowMutation := AAllowMutation;
    vExec.Request := ARequest;
    vExec.Context := FContext;
    vExec.OnResolveType := FOnResolveType;
    Result := vExec.Run;
  finally
    vExec.Free;
    vVars.Free;
  end;
end;

/// Param AName of ARequest as text; '' when absent.
function ParamText(ARequest: TRALRequest; const AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  vParam := ARequest.ParamByName(AName);
  if vParam = nil then
    Result := ''
  else
    Result := vParam.AsString;
end;

/// Header AName of ARequest; '' when absent.
function HeaderText(ARequest: TRALRequest; const AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  vParam := ARequest.Params.GetKind[AName, rpkHEADER];
  if vParam = nil then
    Result := ''
  else
    Result := vParam.AsString;
end;

procedure TRALGraphQLModule.Reply(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vQuery, vVariables, vOperation, vAccept, vType, vAnswer, vBody: StringRAL;
  vJSON, vNode: TRALJSON;
  vGet, vRawGraphQL: boolean;
  vStatus: IntegerRAL;
begin
  vGet := ARequest.Method in [amGET, amHEAD];
  vAccept := HeaderText(ARequest, 'Accept');
  vQuery := '';
  vVariables := '';
  vOperation := '';
  if vGet then
  begin
    vQuery := ParamText(ARequest, 'query');
    vVariables := ParamText(ARequest, 'variables');
    vOperation := ParamText(ARequest, 'operationName');
    if (vQuery = '') and FGraphiQL and (RALPosText('text/html', vAccept) > 0) then
    begin
      AResponse.Answer(HTTP_OK, RALGraphiQLPage(ARequest.Query), rctTEXTHTML);
      Exit;
    end;
  end
  else
  begin
    vType := ARequest.ContentType;
    vBody := ARequest.RequestText;
    vRawGraphQL := RALPosText('application/graphql', vType) > 0;
    if vRawGraphQL and (RALPosText('application/graphql-response', vType) = 0) then
      vQuery := vBody
    else if (vType = '') or (RALPosText('json', vType) > 0) then
    begin
      vJSON := nil;
      try
        vJSON := TRALJSON.ParseJSON(vBody);
      except
        vJSON := nil;
      end;
      try
        if not (vJSON is TRALJSONObject) then
        begin
          AResponse.Answer(HTTP_BadRequest, '{"errors":[{"message":"The body must be a JSON object."}]}',
            rctAPPLICATIONJSON);
          Exit;
        end;
        vNode := TRALJSONObject(vJSON).Find('query');
        if (vNode <> nil) and (vNode.Kind = rjkString) then
          vQuery := vNode.AsString;
        vNode := TRALJSONObject(vJSON).Find('variables');
        if (vNode <> nil) and (vNode.Kind = rjkObject) then
          vVariables := vNode.ToJSON;
        vNode := TRALJSONObject(vJSON).Find('operationName');
        if (vNode <> nil) and (vNode.Kind = rjkString) then
          vOperation := vNode.AsString;
      finally
        vJSON.Free;
      end;
    end
    else
    begin
      AResponse.Answer(HTTP_UnsupportedMedia,
        '{"errors":[{"message":"Use application/json or application/graphql."}]}', rctAPPLICATIONJSON);
      Exit;
    end;
  end;
  if vQuery = '' then
  begin
    AResponse.Answer(HTTP_BadRequest, '{"errors":[{"message":"Must provide query string."}]}',
      rctAPPLICATIONJSON);
    Exit;
  end;
  try
    vAnswer := Execute(vQuery, vVariables, vOperation, ARequest, not vGet);
  except
    on e: Exception do
    begin
      // a wrong schema or variables that are not JSON
      AResponse.Answer(HTTP_InternalError, '{"errors":[{"message":' +
        RALGraphQLQuote(StringRAL(e.Message)) + '}]}', rctAPPLICATIONJSON);
      Exit;
    end;
  end;
  vStatus := HTTP_OK;
  if vGet and (Pos(StringRAL('Mutations are not allowed here'), vAnswer) > 0) and
     (Pos(StringRAL('"data"'), vAnswer) = 0) then
  begin
    // GraphQL over HTTP: a mutation by GET is 405
    AResponse.Answer(HTTP_MethodNotAllowed, vAnswer, rctAPPLICATIONJSON);
    AResponse.AddHeader('Allow', 'POST');
    Exit;
  end;
  if RALPosText(rctGRAPHQLRESPONSE, vAccept) > 0 then
  begin
    // with the new media type, a request that never ran is a 400
    if Pos(StringRAL('"data"'), vAnswer) = 0 then
      vStatus := HTTP_BadRequest;
    AResponse.Answer(vStatus, vAnswer, rctGRAPHQLRESPONSE);
  end
  else
    AResponse.Answer(vStatus, vAnswer, rctAPPLICATIONJSON);
end;

end.
