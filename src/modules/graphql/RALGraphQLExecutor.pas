(* The GraphQL executor: the schema built from its SDL text, the validation of a
  document against it (a subset of the rules of the specification: fields,
  arguments, fragments, variables, directives, leaf selections), the coercion of
  variables and arguments, the execution (fragments, @skip, @include, aliases,
  non-null propagation, errors with path and locations), the introspection
  (__schema, __type, __typename: GraphiQL works) and the depth and cost limits.
  Values are TRALJSON: a resolver answers a JSON value, and the fields under it
  are read from that value unless a resolver of their own is set. *)
unit RALGraphQLExecutor;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, Contnrs,
  RALTypes, RALTools, RALJson, RALGraphQLAST;

type
  TRALGraphQLSchema = class;
  TRALGraphQLExecution = class;

  { What a resolver gets and answers: the field, its arguments, the value of the
    parent object, the request. Answer with one of the Return methods (nothing
    is null) or Fail }
  TRALGraphQLResolve = class
  private
    FArgs: TRALJSONObject;
    FContext: TObject;
    FError: StringRAL;
    FExecution: TRALGraphQLExecution;
    FFailed: boolean;
    FFieldName: StringRAL;
    FParent: TRALJSON;
    FParentType: StringRAL;
    FRequest: TObject;
    FResult: TRALJSON;
    FSelection: TRALGQLSelection;

    procedure SetResult(AValue: TRALJSON);
  public
    destructor Destroy; override;

    /// Argument AName (borrowed), nil when absent or null.
    function Arg(const AName: StringRAL): TRALJSON;
    /// Argument AName as an integer; ADefault when absent or null.
    function ArgAsInteger(const AName: StringRAL; ADefault: Int64 = 0): Int64;
    /// Argument AName as text; '' when absent or null.
    function ArgAsString(const AName: StringRAL): StringRAL;
    /// The field fails with AMessage: null here, an error in the answer.
    procedure Fail(const AMessage: StringRAL);
    /// Answers a boolean.
    procedure ReturnBoolean(AValue: boolean);
    /// Answers a number.
    procedure ReturnFloat(AValue: Double);
    /// Answers an integer.
    procedure ReturnInteger(AValue: Int64);
    /// Answers the JSON text AJSON (an object, a list of objects...).
    procedure ReturnJSON(const AJSON: StringRAL);
    /// Answers null.
    procedure ReturnNull;
    /// Answers a text.
    procedure ReturnText(const AText: StringRAL);
    /// Answers AValue, which the executor frees.
    procedure ReturnValue(AValue: TRALJSON);

    /// The arguments, coerced to their types (defaults applied).
    property Args: TRALJSONObject read FArgs;
    /// The object given to the execution (the application's).
    property Context: TObject read FContext;
    /// The field resolved.
    property FieldName: StringRAL read FFieldName;
    /// The value of the parent object (borrowed): a resolver of a nested field reads it.
    property Parent: TRALJSON read FParent;
    /// The type that has the field.
    property ParentType: StringRAL read FParentType;
    /// The TRALRequest of the HTTP request; nil outside a module.
    property Request: TObject read FRequest;
    /// The field as the query wrote it (alias, sub-selections) for a look ahead.
    property Selection: TRALGQLSelection read FSelection;
  end;

  /// Resolves a field.
  TRALGraphQLOnResolve = procedure(AInfo: TRALGraphQLResolve) of object;
  { Says which object type an interface or union value is (ATypeName); without
    it the value's "__typename" decides }
  TRALGraphQLOnResolveType = procedure(const AAbstractType: StringRAL; AValue: TRALJSON;
    var ATypeName: StringRAL) of object;

  { A schema: its SDL parsed and checked (every type a field names exists, every
    interface is an interface...), with the built-in scalars and the
    introspection types added. Resolvers are set by "Type.field" }
  TRALGraphQLSchema = class
  private
    FDocument: TRALGQLDocument;
    FMutationType: StringRAL;
    FQueryType: StringRAL;
    /// "Type.field" -> TRALGraphQLResolverHolder.
    FResolvers: TStringList;
    FSubscriptionType: StringRAL;
    /// Name -> TRALGQLTypeDef.
    FTypes: TStringList;

    procedure Check;
  public
    { Parses and checks ASDL; raises ERALGraphQLSyntax or an exception that says
      what is wrong }
    constructor Create(const ASDL: StringRAL);
    destructor Destroy; override;

    /// The field AField of the type AType, or nil.
    function FindField(const AType, AField: StringRAL): TRALGQLFieldDef;
    /// The type AName, or nil.
    function FindType(const AName: StringRAL): TRALGQLTypeDef;
    /// Whether the object type AObject is (or implements, or is a member of) AType.
    function IsPossible(const AType, AObject: StringRAL): boolean;
    /// The resolver of AType.AField; Code nil when none.
    function Resolver(const AType, AField: StringRAL): TRALGraphQLOnResolve;
    /// Sets the resolver of AType.AField (an empty AField: every field of AType).
    procedure SetResolver(const AType, AField: StringRAL; AResolver: TRALGraphQLOnResolve);
    /// The schema as SDL (the user's types, without the built-ins).
    function ToSDL: StringRAL;

    /// The parsed SDL.
    property Document: TRALGQLDocument read FDocument;
    /// Name of the mutation root type; empty when none.
    property MutationType: StringRAL read FMutationType;
    /// Name of the query root type.
    property QueryType: StringRAL read FQueryType;
    /// Name of the subscription root type; empty when none.
    property SubscriptionType: StringRAL read FSubscriptionType;
  end;

  /// One execution of a document: build it, set the limits, call Run, read the
  /// answer (the JSON text of the data and the errors).
  TRALGraphQLExecution = class
  private
    FAllowMutation: boolean;
    FComplexity: IntegerRAL;
    FContext: TObject;
    FDepth: IntegerRAL;
    FDocument: TRALGQLDocument;
    FErrors: TRALJSONArray;
    FMaxComplexity: IntegerRAL;
    FMaxDepth: IntegerRAL;
    FOnResolveType: TRALGraphQLOnResolveType;
    FOperation: TRALGQLExecutable;
    FOperationName: StringRAL;
    FQuery: StringRAL;
    FRequest: TObject;
    FRootValue: TRALJSON;
    FSchema: TRALGraphQLSchema;
    /// The coerced variables.
    FVariables: TRALJSONObject;
    FVariablesIn: TRALJSONObject;

    procedure AddError(const AMessage: StringRAL; ANode: TRALGQLNode; APath: TStrings);
    function ApplyDirectives(ADirectives: TObjectList): boolean;
    function CoerceArgs(AField: TRALGQLFieldDef; ASelection: TRALGQLSelection;
      out AArgs: TRALJSONObject): StringRAL;
    function CoerceInput(AValue: TRALJSON; AType: TRALGQLTypeRef; out AResult: TRALJSON): StringRAL;
    function CoerceLiteral(AValue: TRALGQLValue; AType: TRALGQLTypeRef; out AResult: TRALJSON): StringRAL;
    function CoerceVariables: boolean;
    procedure CollectFields(const AType: StringRAL; ASelections: TObjectList; AGroups: TStringList;
      AVisited: TStrings);
    function CompleteValue(AType: TRALGQLTypeRef; AValue: TRALJSON; AFields: TList;
      APath: TStrings; const AField: StringRAL; ANode: TRALGQLNode; out AResult: TRALJSON): boolean;
    function DoesApply(const ACondition, AType: StringRAL): boolean;
    function ExecuteField(const AType: StringRAL; AParent: TRALJSON; AFields: TList;
      APath: TStrings; out AResult: TRALJSON): boolean;
    function ExecuteSelections(const AType: StringRAL; AValue: TRALJSON; AFields: TList;
      APath: TStrings; out AResult: TRALJSON): boolean;
    procedure Introspect(AInfo: TRALGraphQLResolve; AField: TRALGQLFieldDef);
    function Measure(const AType: StringRAL; ASelections: TObjectList; ADepth: IntegerRAL;
      AVisited: TStrings): IntegerRAL;
    function SelectOperation: boolean;
    procedure Validate;
    procedure ValidateSelections(const AType: StringRAL; ASelections: TObjectList; AVisited: TStrings;
      AUsed: TStrings);
  public
    /// An execution of AQuery against ASchema.
    constructor Create(ASchema: TRALGraphQLSchema; const AQuery: StringRAL);
    destructor Destroy; override;

    /// Parses, validates and executes; the answer as JSON text.
    function Run: StringRAL;

    /// Lets a mutation run (an HTTP GET does not).
    property AllowMutation: boolean read FAllowMutation write FAllowMutation;
    /// Cost of the operation (fields selected, introspection left out), after Run.
    property Complexity: IntegerRAL read FComplexity;
    /// The application's object, passed to the resolvers.
    property Context: TObject read FContext write FContext;
    /// Depth of the operation, after Run.
    property Depth: IntegerRAL read FDepth;
    /// Most fields selected; 0 is no limit.
    property MaxComplexity: IntegerRAL read FMaxComplexity write FMaxComplexity;
    /// Deepest nesting of fields; 0 is no limit.
    property MaxDepth: IntegerRAL read FMaxDepth write FMaxDepth;
    /// See TRALGraphQLOnResolveType.
    property OnResolveType: TRALGraphQLOnResolveType read FOnResolveType write FOnResolveType;
    /// The operation to run when the document has several.
    property OperationName: StringRAL read FOperationName write FOperationName;
    /// The TRALRequest, passed to the resolvers.
    property Request: TObject read FRequest write FRequest;
    /// The value the root fields read when they have no resolver (borrowed).
    property RootValue: TRALJSON read FRootValue write FRootValue;
    /// The variables as they came (borrowed); nil is none.
    property Variables: TRALJSONObject read FVariablesIn write FVariablesIn;
  end;

/// A GraphQL value literal as text (for defaultValue in the introspection).
function RALGraphQLPrintValue(AValue: TRALGQLValue): StringRAL;

const
  /// The introspection types of the specification, added to every schema.
  RALGraphQLIntrospectionSDL =
    'type __Schema { description: String types: [__Type!]! queryType: __Type! ' +
    'mutationType: __Type subscriptionType: __Type directives: [__Directive!]! }'#10 +
    'type __Type { kind: __TypeKind! name: String description: String specifiedByURL: String ' +
    'fields(includeDeprecated: Boolean = false): [__Field!] interfaces: [__Type!] ' +
    'possibleTypes: [__Type!] enumValues(includeDeprecated: Boolean = false): [__EnumValue!] ' +
    'inputFields(includeDeprecated: Boolean = false): [__InputValue!] ofType: __Type isOneOf: Boolean }'#10 +
    'enum __TypeKind { SCALAR OBJECT INTERFACE UNION ENUM INPUT_OBJECT LIST NON_NULL }'#10 +
    'type __Field { name: String! description: String ' +
    'args(includeDeprecated: Boolean = false): [__InputValue!]! type: __Type! ' +
    'isDeprecated: Boolean! deprecationReason: String }'#10 +
    'type __InputValue { name: String! description: String type: __Type! defaultValue: String ' +
    'isDeprecated: Boolean! deprecationReason: String }'#10 +
    'type __EnumValue { name: String! description: String isDeprecated: Boolean! deprecationReason: String }'#10 +
    'type __Directive { name: String! description: String isRepeatable: Boolean! ' +
    'locations: [__DirectiveLocation!]! args(includeDeprecated: Boolean = false): [__InputValue!]! }'#10 +
    'enum __DirectiveLocation { QUERY MUTATION SUBSCRIPTION FIELD FRAGMENT_DEFINITION ' +
    'FRAGMENT_SPREAD INLINE_FRAGMENT VARIABLE_DEFINITION SCHEMA SCALAR OBJECT FIELD_DEFINITION ' +
    'ARGUMENT_DEFINITION INTERFACE UNION ENUM ENUM_VALUE INPUT_OBJECT INPUT_FIELD_DEFINITION }'#10;

implementation

type
  /// Holds a resolver in a string list.
  TRALGraphQLResolverHolder = class
  public
    Resolver: TRALGraphQLOnResolve;
  end;

const
  /// The built-in scalars.
  cScalars: array[0..4] of StringRAL = ('Int', 'Float', 'String', 'Boolean', 'ID');
  /// Names of the type kinds in the introspection.
  cKindNames: array[TRALGQLTypeKind] of StringRAL = ('SCALAR', 'OBJECT', 'INTERFACE', 'UNION',
    'ENUM', 'INPUT_OBJECT');

function RALGraphQLPrintValue(AValue: TRALGQLValue): StringRAL;
var
  vInt: IntegerRAL;
begin
  if AValue = nil then
  begin
    Result := '';
    Exit;
  end;
  case AValue.Kind of
    gvVariable: Result := '$' + AValue.Text;
    gvString: Result := RALGraphQLQuote(AValue.Text);
    gvList:
      begin
        Result := '[';
        for vInt := 0 to AValue.Items.Count - 1 do
        begin
          if vInt > 0 then
            Result := Result + ', ';
          Result := Result + RALGraphQLPrintValue(TRALGQLValue(AValue.Items[vInt]));
        end;
        Result := Result + ']';
      end;
    gvObject:
      begin
        Result := '{';
        for vInt := 0 to AValue.Items.Count - 1 do
        begin
          if vInt > 0 then
            Result := Result + ', ';
          Result := Result + TRALGQLObjectField(AValue.Items[vInt]).Name + ': ' +
            RALGraphQLPrintValue(TRALGQLObjectField(AValue.Items[vInt]).Value);
        end;
        Result := Result + '}';
      end;
  else
    Result := AValue.Text;
  end;
end;

/// A copy of AValue (nil gives nil).
function CloneJSON(AValue: TRALJSON): TRALJSON;
var
  vInt: IntegerRAL;
  vObj: TRALJSONObject;
  vArr: TRALJSONArray;
begin
  Result := nil;
  if AValue = nil then
    Exit;
  case AValue.Kind of
    rjkObject:
      begin
        vObj := TRALJSONObject.Create;
        for vInt := 0 to TRALJSONObject(AValue).Count - 1 do
          vObj.Add(TRALJSONObject(AValue).Names[vInt], CloneJSON(TRALJSONObject(AValue).Items[vInt]));
        Result := vObj;
      end;
    rjkArray:
      begin
        vArr := TRALJSONArray.Create;
        for vInt := 0 to TRALJSONArray(AValue).Count - 1 do
          vArr.Add(CloneJSON(TRALJSONArray(AValue).Items[vInt]));
        Result := vArr;
      end;
    rjkString: Result := TRALJSONString.Create(AValue.AsString);
    rjkNumber: Result := TRALJSONNumber.Create(TRALJSONNumber(AValue).Value);
    rjkBoolean: Result := TRALJSONBoolean.Create(AValue.AsBoolean);
  else
    Result := TRALJSONNull.Create;
  end;
end;

function NewNull: TRALJSON;
begin
  Result := TRALJSONNull.Create;
end;

function IsNullValue(AValue: TRALJSON): boolean;
begin
  Result := (AValue = nil) or (AValue.Kind = rjkNull);
end;

/// Whether the text of a JSON number is an integer within 32 bits.
function IsInt32(AValue: TRALJSON; out AInt: Int64): boolean;
var
  vFloat: Double;
begin
  Result := False;
  AInt := 0;
  if (AValue = nil) or (AValue.Kind <> rjkNumber) then
    Exit;
  vFloat := AValue.AsNumber;
  if (Frac(vFloat) <> 0) or (vFloat < -2147483648.0) or (vFloat > 2147483647.0) then
    Exit;
  AInt := Round(vFloat);
  Result := True;
end;

{ TRALGraphQLResolve }

destructor TRALGraphQLResolve.Destroy;
begin
  FreeAndNil(FResult);
  FreeAndNil(FArgs);
  inherited Destroy;
end;

procedure TRALGraphQLResolve.SetResult(AValue: TRALJSON);
begin
  FreeAndNil(FResult);
  FResult := AValue;
end;

function TRALGraphQLResolve.Arg(const AName: StringRAL): TRALJSON;
begin
  Result := nil;
  if FArgs <> nil then
    Result := FArgs.Find(AName);
  if IsNullValue(Result) then
    Result := nil;
end;

function TRALGraphQLResolve.ArgAsInteger(const AName: StringRAL; ADefault: Int64): Int64;
begin
  if Arg(AName) = nil then
    Result := ADefault
  else
    Result := Arg(AName).AsInteger;
end;

function TRALGraphQLResolve.ArgAsString(const AName: StringRAL): StringRAL;
begin
  if Arg(AName) = nil then
    Result := ''
  else
    Result := Arg(AName).AsString;
end;

procedure TRALGraphQLResolve.Fail(const AMessage: StringRAL);
begin
  FFailed := True;
  FError := AMessage;
  SetResult(nil);
end;

procedure TRALGraphQLResolve.ReturnBoolean(AValue: boolean);
begin
  SetResult(TRALJSONBoolean.Create(AValue));
end;

procedure TRALGraphQLResolve.ReturnFloat(AValue: Double);
begin
  SetResult(TRALJSONNumber.Create(StringRAL(FloatToStr(AValue, RALInvariantFormat))));
end;

procedure TRALGraphQLResolve.ReturnInteger(AValue: Int64);
begin
  SetResult(TRALJSONNumber.Create(StringRAL(IntToStr(AValue))));
end;

procedure TRALGraphQLResolve.ReturnJSON(const AJSON: StringRAL);
begin
  SetResult(TRALJSON.ParseJSON(AJSON));
end;

procedure TRALGraphQLResolve.ReturnNull;
begin
  SetResult(nil);
end;

procedure TRALGraphQLResolve.ReturnText(const AText: StringRAL);
begin
  SetResult(TRALJSONString.Create(AText));
end;

procedure TRALGraphQLResolve.ReturnValue(AValue: TRALJSON);
begin
  SetResult(AValue);
end;

{ TRALGraphQLSchema }

constructor TRALGraphQLSchema.Create(const ASDL: StringRAL);
var
  vInt, vIdx: IntegerRAL;
  vDef, vBase: TRALGQLTypeDef;
  vBuiltin: TRALGQLDocument;
begin
  inherited Create;
  FTypes := TStringList.Create;
  FTypes.Sorted := True;
  FTypes.CaseSensitive := True;
  FResolvers := TStringList.Create;
  FResolvers.Sorted := True;
  FResolvers.CaseSensitive := True;
  FResolvers.OwnsObjects := True;
  FDocument := RALGraphQLParse(ASDL, False, True);
  // the built-in scalars the SDL did not declare, and the introspection types
  for vInt := 0 to High(cScalars) do
  begin
    vDef := nil;
    for vIdx := 0 to FDocument.Types.Count - 1 do
      if TRALGQLTypeDef(FDocument.Types[vIdx]).Name = cScalars[vInt] then
        vDef := TRALGQLTypeDef(FDocument.Types[vIdx]);
    if vDef = nil then
    begin
      vDef := TRALGQLTypeDef.Create(gkScalar);
      vDef.Name := cScalars[vInt];
      FDocument.Types.Add(vDef);
    end;
  end;
  // the extensions join the type of their name (or become it, when there is none)
  for vInt := FDocument.Types.Count - 1 downto 0 do
  begin
    vDef := TRALGQLTypeDef(FDocument.Types[vInt]);
    if not vDef.IsExtension then
      Continue;
    vBase := nil;
    for vIdx := 0 to FDocument.Types.Count - 1 do
      if (not TRALGQLTypeDef(FDocument.Types[vIdx]).IsExtension) and
         (TRALGQLTypeDef(FDocument.Types[vIdx]).Name = vDef.Name) then
        vBase := TRALGQLTypeDef(FDocument.Types[vIdx]);
    if vBase = nil then
    begin
      vDef.IsExtension := False;
      Continue;
    end;
    if vBase.Kind <> vDef.Kind then
      raise Exception.Create('Cannot extend "' + string(vDef.Name) + '" with another kind of type.');
    vDef.Fields.OwnsObjects := False;
    vDef.EnumValues.OwnsObjects := False;
    for vIdx := 0 to vDef.Fields.Count - 1 do
    begin
      if vBase.Field(TRALGQLFieldDef(vDef.Fields[vIdx]).Name) <> nil then
        raise Exception.Create('Field "' + string(vDef.Name) + '.' +
          string(TRALGQLFieldDef(vDef.Fields[vIdx]).Name) + '" already exists.');
      vBase.Fields.Add(vDef.Fields[vIdx]);
    end;
    for vIdx := 0 to vDef.EnumValues.Count - 1 do
      vBase.EnumValues.Add(vDef.EnumValues[vIdx]);
    vBase.Interfaces.AddStrings(vDef.Interfaces);
    vBase.PossibleTypes.AddStrings(vDef.PossibleTypes);
    FDocument.Types.Delete(vInt);
  end;
  vBuiltin := RALGraphQLParse(RALGraphQLIntrospectionSDL, False, True);
  try
    vBuiltin.Types.OwnsObjects := False;
    for vInt := 0 to vBuiltin.Types.Count - 1 do
      FDocument.Types.Add(vBuiltin.Types[vInt]);
  finally
    vBuiltin.Free;
  end;
  for vInt := 0 to FDocument.Types.Count - 1 do
  begin
    vDef := TRALGQLTypeDef(FDocument.Types[vInt]);
    if FTypes.IndexOf(string(vDef.Name)) >= 0 then
      raise Exception.Create('There can be only one type named "' + string(vDef.Name) + '".');
    FTypes.AddObject(string(vDef.Name), vDef);
  end;
  FQueryType := FDocument.QueryType;
  if FQueryType = '' then
    FQueryType := 'Query';
  FMutationType := FDocument.MutationType;
  if (FMutationType = '') and (FindType('Mutation') <> nil) then
    FMutationType := 'Mutation';
  FSubscriptionType := FDocument.SubscriptionType;
  if (FSubscriptionType = '') and (FindType('Subscription') <> nil) then
    FSubscriptionType := 'Subscription';
  Check;
end;

destructor TRALGraphQLSchema.Destroy;
begin
  FreeAndNil(FResolvers);
  FreeAndNil(FTypes);
  FreeAndNil(FDocument);
  inherited Destroy;
end;

procedure TRALGraphQLSchema.Check;
var
  vInt, vField, vArg: IntegerRAL;
  vDef, vRef: TRALGQLTypeDef;
  vFieldDef, vArgDef: TRALGQLFieldDef;

  procedure CheckRef(ARef: TRALGQLTypeRef; AInput: boolean; const AWhere: string);
  var
    vTarget: TRALGQLTypeDef;
  begin
    vTarget := FindType(ARef.NamedType);
    if vTarget = nil then
      raise Exception.Create('Unknown type "' + string(ARef.NamedType) + '" in ' + AWhere + '.');
    if AInput and not (vTarget.Kind in [gkScalar, gkEnum, gkInputObject]) then
      raise Exception.Create(AWhere + ' must be an input type, "' + string(vTarget.Name) + '" is not.');
    if (not AInput) and (vTarget.Kind = gkInputObject) then
      raise Exception.Create(AWhere + ' must be an output type, "' + string(vTarget.Name) + '" is not.');
  end;

begin
  vDef := FindType(FQueryType);
  if (vDef = nil) or (vDef.Kind <> gkObject) then
    raise Exception.Create('The query root type "' + string(FQueryType) + '" must be an object type.');
  for vInt := 0 to FTypes.Count - 1 do
  begin
    vDef := TRALGQLTypeDef(FTypes.Objects[vInt]);
    if (vDef.Kind in [gkObject, gkInterface, gkInputObject]) and (vDef.Fields.Count = 0) then
      raise Exception.Create('Type "' + string(vDef.Name) + '" must define one or more fields.');
    for vField := 0 to vDef.Fields.Count - 1 do
    begin
      vFieldDef := TRALGQLFieldDef(vDef.Fields[vField]);
      CheckRef(vFieldDef.TypeRef, vDef.Kind = gkInputObject,
        string(vDef.Name) + '.' + string(vFieldDef.Name));
      for vArg := 0 to vFieldDef.Args.Count - 1 do
      begin
        vArgDef := TRALGQLFieldDef(vFieldDef.Args[vArg]);
        CheckRef(vArgDef.TypeRef, True, string(vDef.Name) + '.' + string(vFieldDef.Name) +
          '(' + string(vArgDef.Name) + ':)');
      end;
    end;
    for vField := 0 to vDef.Interfaces.Count - 1 do
    begin
      vRef := FindType(StringRAL(vDef.Interfaces[vField]));
      if (vRef = nil) or (vRef.Kind <> gkInterface) then
        raise Exception.Create('Type "' + string(vDef.Name) + '" can only implement an interface, "' +
          vDef.Interfaces[vField] + '" is not one.');
    end;
    for vField := 0 to vDef.PossibleTypes.Count - 1 do
    begin
      vRef := FindType(StringRAL(vDef.PossibleTypes[vField]));
      if (vRef = nil) or (vRef.Kind <> gkObject) then
        raise Exception.Create('Union "' + string(vDef.Name) + '" can only have object types, "' +
          vDef.PossibleTypes[vField] + '" is not one.');
    end;
  end;
end;

function TRALGraphQLSchema.FindType(const AName: StringRAL): TRALGQLTypeDef;
var
  vIdx: Integer;
begin
  if FTypes.Find(string(AName), vIdx) then
    Result := TRALGQLTypeDef(FTypes.Objects[vIdx])
  else
    Result := nil;
end;

function TRALGraphQLSchema.FindField(const AType, AField: StringRAL): TRALGQLFieldDef;
var
  vDef: TRALGQLTypeDef;
begin
  Result := nil;
  vDef := FindType(AType);
  if (vDef <> nil) and (vDef.Kind in [gkObject, gkInterface, gkInputObject]) then
    Result := vDef.Field(AField);
end;

function TRALGraphQLSchema.IsPossible(const AType, AObject: StringRAL): boolean;
var
  vDef, vObj: TRALGQLTypeDef;
begin
  Result := AType = AObject;
  if Result then
    Exit;
  vDef := FindType(AType);
  vObj := FindType(AObject);
  if (vDef = nil) or (vObj = nil) then
    Exit;
  case vDef.Kind of
    gkUnion: Result := vDef.PossibleTypes.IndexOf(string(AObject)) >= 0;
    gkInterface: Result := vObj.Interfaces.IndexOf(string(AType)) >= 0;
  end;
end;

function TRALGraphQLSchema.Resolver(const AType, AField: StringRAL): TRALGraphQLOnResolve;
var
  vIdx: Integer;
begin
  Result := nil;
  if FResolvers.Find(string(AType + '.' + AField), vIdx) or
     FResolvers.Find(string(AType + '.'), vIdx) then
    Result := TRALGraphQLResolverHolder(FResolvers.Objects[vIdx]).Resolver;
end;

procedure TRALGraphQLSchema.SetResolver(const AType, AField: StringRAL;
  AResolver: TRALGraphQLOnResolve);
var
  vIdx: Integer;
  vHolder: TRALGraphQLResolverHolder;
begin
  if FResolvers.Find(string(AType + '.' + AField), vIdx) then
    FResolvers.Delete(vIdx);
  if not Assigned(AResolver) then
    Exit;
  vHolder := TRALGraphQLResolverHolder.Create;
  vHolder.Resolver := AResolver;
  FResolvers.AddObject(string(AType + '.' + AField), vHolder);
end;

function TRALGraphQLSchema.ToSDL: StringRAL;
var
  vInt, vField: IntegerRAL;
  vDef: TRALGQLTypeDef;
  vFieldDef: TRALGQLFieldDef;
  vKind: StringRAL;
begin
  Result := '';
  for vInt := 0 to FDocument.Types.Count - 1 do
  begin
    vDef := TRALGQLTypeDef(FDocument.Types[vInt]);
    if (Copy(vDef.Name, 1, 2) = '__') or
       ((vDef.Kind = gkScalar) and ((vDef.Name = 'Int') or (vDef.Name = 'Float') or
       (vDef.Name = 'String') or (vDef.Name = 'Boolean') or (vDef.Name = 'ID'))) then
      Continue;
    case vDef.Kind of
      gkScalar: vKind := 'scalar';
      gkObject: vKind := 'type';
      gkInterface: vKind := 'interface';
      gkUnion: vKind := 'union';
      gkEnum: vKind := 'enum';
    else
      vKind := 'input';
    end;
    Result := Result + vKind + ' ' + vDef.Name;
    if vDef.Interfaces.Count > 0 then
      Result := Result + ' implements ' + StringRAL(StringReplace(vDef.Interfaces.CommaText, ',', ' & ', [rfReplaceAll]));
    if vDef.Kind = gkUnion then
      Result := Result + ' = ' + StringRAL(StringReplace(vDef.PossibleTypes.CommaText, ',', ' | ', [rfReplaceAll]));
    if vDef.Kind = gkEnum then
    begin
      Result := Result + ' {'#10;
      for vField := 0 to vDef.EnumValues.Count - 1 do
        Result := Result + '  ' + TRALGQLFieldDef(vDef.EnumValues[vField]).Name + #10;
      Result := Result + '}';
    end
    else if vDef.Fields.Count > 0 then
    begin
      Result := Result + ' {'#10;
      for vField := 0 to vDef.Fields.Count - 1 do
      begin
        vFieldDef := TRALGQLFieldDef(vDef.Fields[vField]);
        Result := Result + '  ' + vFieldDef.Name + ': ' + vFieldDef.TypeRef.ToText + #10;
      end;
      Result := Result + '}';
    end;
    Result := Result + #10#10;
  end;
end;

{ TRALGraphQLExecution }

constructor TRALGraphQLExecution.Create(ASchema: TRALGraphQLSchema; const AQuery: StringRAL);
begin
  inherited Create;
  FSchema := ASchema;
  FQuery := AQuery;
  FErrors := TRALJSONArray.Create;
  FAllowMutation := True;
end;

destructor TRALGraphQLExecution.Destroy;
begin
  FreeAndNil(FDocument);
  FreeAndNil(FErrors);
  FreeAndNil(FVariables);
  inherited Destroy;
end;

procedure TRALGraphQLExecution.AddError(const AMessage: StringRAL; ANode: TRALGQLNode;
  APath: TStrings);
var
  vError, vLoc: TRALJSONObject;
  vLocs, vPath: TRALJSONArray;
  vInt: IntegerRAL;
  vIndex: Int64;
begin
  vError := TRALJSONObject.Create;
  vError.Add('message', AMessage);
  if (ANode <> nil) and (ANode.Line > 0) then
  begin
    vLocs := TRALJSONArray.Create;
    vLoc := TRALJSONObject.Create;
    vLoc.Add('line', ANode.Line);
    vLoc.Add('column', ANode.Column);
    vLocs.Add(vLoc);
    vError.Add('locations', vLocs);
  end;
  if APath <> nil then
  begin
    vPath := TRALJSONArray.Create;
    for vInt := 0 to APath.Count - 1 do
      // list indexes are kept as #n
      if (Copy(APath[vInt], 1, 1) = '#') and TryStrToInt64(Copy(APath[vInt], 2, MaxInt), vIndex) then
        vPath.Add(vIndex)
      else
        vPath.Add(StringRAL(APath[vInt]));
    vError.Add('path', vPath);
  end;
  FErrors.Add(vError);
end;

function TRALGraphQLExecution.SelectOperation: boolean;
var
  vInt, vCount: IntegerRAL;
  vDef: TRALGQLExecutable;
begin
  Result := False;
  FOperation := nil;
  vCount := 0;
  for vInt := 0 to FDocument.Definitions.Count - 1 do
  begin
    vDef := TRALGQLExecutable(FDocument.Definitions[vInt]);
    if vDef.IsFragment then
      Continue;
    Inc(vCount);
    if (FOperationName = '') or (vDef.Name = FOperationName) then
      if (FOperationName <> '') or (FOperation = nil) then
        FOperation := vDef;
  end;
  if vCount = 0 then
    AddError('Must provide an operation.', nil, nil)
  else if (FOperationName = '') and (vCount > 1) then
    AddError('Must provide operation name if query contains multiple operations.', nil, nil)
  else if FOperation = nil then
    AddError('Unknown operation named "' + FOperationName + '".', nil, nil)
  else
    Result := True;
end;

function TRALGraphQLExecution.DoesApply(const ACondition, AType: StringRAL): boolean;
begin
  Result := (ACondition = '') or FSchema.IsPossible(ACondition, AType);
end;

{ ---- validation ---- }

procedure TRALGraphQLExecution.ValidateSelections(const AType: StringRAL; ASelections: TObjectList;
  AVisited: TStrings; AUsed: TStrings);
var
  vInt, vArg: IntegerRAL;
  vSel: TRALGQLSelection;
  vTypeDef, vTarget: TRALGQLTypeDef;
  vField, vArgDef: TRALGQLFieldDef;
  vFragment: TRALGQLExecutable;
  vNamed: StringRAL;

  procedure CheckDirectives(ADirectives: TObjectList);
  var
    vD: IntegerRAL;
    vDir: TRALGQLDirective;
  begin
    for vD := 0 to ADirectives.Count - 1 do
    begin
      vDir := TRALGQLDirective(ADirectives[vD]);
      if (vDir.Name <> 'skip') and (vDir.Name <> 'include') then
        AddError('Unknown directive "@' + vDir.Name + '".', vDir, nil)
      else if vDir.Arg('if') = nil then
        AddError('Directive "@' + vDir.Name + '" argument "if" of type "Boolean!" is required, but it was not provided.',
          vDir, nil);
    end;
  end;

  procedure NoteVariables(AValue: TRALGQLValue);
  var
    vV: IntegerRAL;
  begin
    if AValue = nil then
      Exit;
    if AValue.Kind = gvVariable then
      AUsed.Add(string(AValue.Text))
    else if AValue.Kind = gvList then
      for vV := 0 to AValue.Items.Count - 1 do
        NoteVariables(TRALGQLValue(AValue.Items[vV]))
    else if AValue.Kind = gvObject then
      for vV := 0 to AValue.Items.Count - 1 do
        NoteVariables(TRALGQLObjectField(AValue.Items[vV]).Value);
  end;

  procedure NoteDirectiveVariables(ADirectives: TObjectList);
  var
    vD, vA: IntegerRAL;
  begin
    for vD := 0 to ADirectives.Count - 1 do
      for vA := 0 to TRALGQLDirective(ADirectives[vD]).Args.Count - 1 do
        NoteVariables(TRALGQLArgument(TRALGQLDirective(ADirectives[vD]).Args[vA]).Value);
  end;

begin
  vTypeDef := FSchema.FindType(AType);
  for vInt := 0 to ASelections.Count - 1 do
  begin
    vSel := TRALGQLSelection(ASelections[vInt]);
    CheckDirectives(vSel.Directives);
    NoteDirectiveVariables(vSel.Directives);
    case vSel.Kind of
      gsField:
        begin
          if vSel.Name = '__typename' then
          begin
            if vSel.Selections.Count > 0 then
              AddError('Field "__typename" must not have a selection since type "String!" has no subfields.',
                vSel, nil);
            Continue;
          end;
          if (AType = FSchema.QueryType) and ((vSel.Name = '__schema') or (vSel.Name = '__type')) then
          begin
            if vSel.Name = '__schema' then
              vNamed := '__Schema'
            else
            begin
              vNamed := '__Type';
              if vSel.Arg('name') = nil then
                AddError('Field "__type" argument "name" of type "String!" is required, but it was not provided.',
                  vSel, nil)
              else
                NoteVariables(vSel.Arg('name').Value);
            end;
            if vSel.Selections.Count = 0 then
              AddError('Field "' + vSel.Name + '" of type "' + vNamed + '" must have a selection of subfields.',
                vSel, nil)
            else
              ValidateSelections(vNamed, vSel.Selections, AVisited, AUsed);
            Continue;
          end;
          vField := nil;
          if (vTypeDef <> nil) and (vTypeDef.Kind in [gkObject, gkInterface]) then
            vField := vTypeDef.Field(vSel.Name);
          if vField = nil then
          begin
            AddError('Cannot query field "' + vSel.Name + '" on type "' + AType + '".', vSel, nil);
            Continue;
          end;
          for vArg := 0 to vSel.Args.Count - 1 do
          begin
            NoteVariables(TRALGQLArgument(vSel.Args[vArg]).Value);
            if vField.Arg(TRALGQLArgument(vSel.Args[vArg]).Name) = nil then
              AddError('Unknown argument "' + TRALGQLArgument(vSel.Args[vArg]).Name + '" on field "' +
                AType + '.' + vSel.Name + '".', TRALGQLArgument(vSel.Args[vArg]), nil);
          end;
          for vArg := 0 to vField.Args.Count - 1 do
          begin
            vArgDef := TRALGQLFieldDef(vField.Args[vArg]);
            if (vArgDef.TypeRef.Kind = gtNonNull) and (vArgDef.DefaultValue = nil) and
               (vSel.Arg(vArgDef.Name) = nil) then
              AddError('Field "' + vSel.Name + '" argument "' + vArgDef.Name + '" of type "' +
                vArgDef.TypeRef.ToText + '" is required, but it was not provided.', vSel, nil);
          end;
          vTarget := FSchema.FindType(vField.TypeRef.NamedType);
          if (vTarget <> nil) and (vTarget.Kind in [gkObject, gkInterface, gkUnion]) then
          begin
            if vSel.Selections.Count = 0 then
              AddError('Field "' + vSel.Name + '" of type "' + vField.TypeRef.ToText +
                '" must have a selection of subfields. Did you mean "' + vSel.Name + ' { ... }"?', vSel, nil)
            else
              ValidateSelections(vTarget.Name, vSel.Selections, AVisited, AUsed);
          end
          else if vSel.Selections.Count > 0 then
            AddError('Field "' + vSel.Name + '" must not have a selection since type "' +
              vField.TypeRef.ToText + '" has no subfields.', vSel, nil);
        end;
      gsFragmentSpread:
        begin
          vFragment := FDocument.Fragment(vSel.Name);
          if vFragment = nil then
          begin
            AddError('Unknown fragment "' + vSel.Name + '".', vSel, nil);
            Continue;
          end;
          if AVisited.IndexOf(string(vSel.Name)) >= 0 then
          begin
            AddError('Cannot spread fragment "' + vSel.Name + '" within itself.', vSel, nil);
            Continue;
          end;
          vTarget := FSchema.FindType(vFragment.TypeCondition);
          if (vTarget = nil) or not (vTarget.Kind in [gkObject, gkInterface, gkUnion]) then
          begin
            AddError('Unknown type "' + vFragment.TypeCondition + '".', vFragment, nil);
            Continue;
          end;
          AVisited.Add(string(vSel.Name));
          try
            ValidateSelections(vTarget.Name, vFragment.Selections, AVisited, AUsed);
          finally
            AVisited.Delete(AVisited.Count - 1);
          end;
        end;
      gsInlineFragment:
        begin
          vNamed := AType;
          if vSel.TypeCondition <> '' then
          begin
            vTarget := FSchema.FindType(vSel.TypeCondition);
            if (vTarget = nil) or not (vTarget.Kind in [gkObject, gkInterface, gkUnion]) then
            begin
              AddError('Unknown type "' + vSel.TypeCondition + '".', vSel, nil);
              Continue;
            end;
            vNamed := vSel.TypeCondition;
          end;
          ValidateSelections(vNamed, vSel.Selections, AVisited, AUsed);
        end;
    end;
  end;
end;

procedure TRALGraphQLExecution.Validate;
var
  vInt, vVar: IntegerRAL;
  vDef: TRALGQLExecutable;
  vRoot: StringRAL;
  vVisited, vUsed: TStringList;
  vVarDef: TRALGQLVariableDef;
  vType: TRALGQLTypeDef;
  vNames: TStringList;
begin
  vVisited := TStringList.Create;
  vUsed := TStringList.Create;
  vNames := TStringList.Create;
  try
    vNames.Sorted := True;
    vNames.Duplicates := dupError;
    for vInt := 0 to FDocument.Definitions.Count - 1 do
    begin
      vDef := TRALGQLExecutable(FDocument.Definitions[vInt]);
      if vDef.IsFragment and (vNames.IndexOf('fragment ' + string(vDef.Name)) >= 0) then
        AddError('There can be only one fragment named "' + vDef.Name + '".', vDef, nil)
      else if vDef.IsFragment then
        vNames.Add('fragment ' + string(vDef.Name))
      else if (vDef.Name <> '') and (vNames.IndexOf('op ' + string(vDef.Name)) >= 0) then
        AddError('There can be only one operation named "' + vDef.Name + '".', vDef, nil)
      else if vDef.Name <> '' then
        vNames.Add('op ' + string(vDef.Name));
    end;
    vDef := FOperation;
    case vDef.Operation of
      goMutation: vRoot := FSchema.MutationType;
      goSubscription: vRoot := FSchema.SubscriptionType;
    else
      vRoot := FSchema.QueryType;
    end;
    if vRoot = '' then
    begin
      if vDef.Operation = goMutation then
        AddError('Schema is not configured for mutations.', vDef, nil)
      else
        AddError('Schema is not configured for subscriptions.', vDef, nil);
      Exit;
    end;
    for vVar := 0 to vDef.Variables.Count - 1 do
    begin
      vVarDef := TRALGQLVariableDef(vDef.Variables[vVar]);
      vType := FSchema.FindType(vVarDef.TypeRef.NamedType);
      if (vType = nil) or not (vType.Kind in [gkScalar, gkEnum, gkInputObject]) then
        AddError('Variable "$' + vVarDef.Name + '" cannot be non-input type "' +
          vVarDef.TypeRef.ToText + '".', vVarDef, nil);
    end;
    ValidateSelections(vRoot, vDef.Selections, vVisited, vUsed);
    for vVar := 0 to vUsed.Count - 1 do
    begin
      vInt := 0;
      while (vInt < vDef.Variables.Count) and
        (TRALGQLVariableDef(vDef.Variables[vInt]).Name <> StringRAL(vUsed[vVar])) do
        Inc(vInt);
      if vInt >= vDef.Variables.Count then
        AddError('Variable "$' + StringRAL(vUsed[vVar]) + '" is not defined.', vDef, nil);
    end;
    // the limits, introspection left out
    if FErrors.Count = 0 then
    begin
      vVisited.Clear;
      FDepth := 0;
      FComplexity := Measure(vRoot, vDef.Selections, 1, vVisited);
      if (FMaxDepth > 0) and (FDepth > FMaxDepth) then
        AddError('Query is too deep: ' + StringRAL(IntToStr(FDepth)) + ' levels, the most is ' +
          StringRAL(IntToStr(FMaxDepth)) + '.', vDef, nil)
      else if (FMaxComplexity > 0) and (FComplexity > FMaxComplexity) then
        AddError('Query is too complex: ' + StringRAL(IntToStr(FComplexity)) + ' fields, the most is ' +
          StringRAL(IntToStr(FMaxComplexity)) + '.', vDef, nil);
    end;
  finally
    vNames.Free;
    vUsed.Free;
    vVisited.Free;
  end;
end;

function TRALGraphQLExecution.Measure(const AType: StringRAL; ASelections: TObjectList;
  ADepth: IntegerRAL; AVisited: TStrings): IntegerRAL;
var
  vInt: IntegerRAL;
  vSel: TRALGQLSelection;
  vField: TRALGQLFieldDef;
  vFragment: TRALGQLExecutable;
begin
  Result := 0;
  if ADepth > FDepth then
    FDepth := ADepth;
  for vInt := 0 to ASelections.Count - 1 do
  begin
    vSel := TRALGQLSelection(ASelections[vInt]);
    case vSel.Kind of
      gsField:
        begin
          if Copy(vSel.Name, 1, 2) = '__' then
            Continue; // introspection costs nothing
          Inc(Result);
          vField := FSchema.FindField(AType, vSel.Name);
          if (vField <> nil) and (vSel.Selections.Count > 0) then
            Inc(Result, Measure(vField.TypeRef.NamedType, vSel.Selections, ADepth + 1, AVisited));
        end;
      gsFragmentSpread:
        begin
          vFragment := FDocument.Fragment(vSel.Name);
          if (vFragment <> nil) and (AVisited.IndexOf(string(vSel.Name)) < 0) then
          begin
            AVisited.Add(string(vSel.Name));
            Inc(Result, Measure(vFragment.TypeCondition, vFragment.Selections, ADepth, AVisited));
            AVisited.Delete(AVisited.Count - 1);
          end;
        end;
      gsInlineFragment:
        if vSel.TypeCondition <> '' then
          Inc(Result, Measure(vSel.TypeCondition, vSel.Selections, ADepth, AVisited))
        else
          Inc(Result, Measure(AType, vSel.Selections, ADepth, AVisited));
    end;
  end;
end;

{ ---- coercion ---- }

function TRALGraphQLExecution.CoerceLiteral(AValue: TRALGQLValue; AType: TRALGQLTypeRef;
  out AResult: TRALJSON): StringRAL;
var
  vTypeDef: TRALGQLTypeDef;
  vInt, vField: IntegerRAL;
  vItem: TRALJSON;
  vArr: TRALJSONArray;
  vObj: TRALJSONObject;
  vFieldDef: TRALGQLFieldDef;
  vLiteral: TRALGQLValue;
  vVar: TRALJSON;
  vText: StringRAL;
  vInt64: Int64;
begin
  Result := '';
  AResult := nil;
  if AValue.Kind = gvVariable then
  begin
    vVar := nil;
    if FVariables <> nil then
      vVar := FVariables.Find(AValue.Text);
    if (vVar = nil) and (AType.Kind = gtNonNull) then
    begin
      Result := 'Variable "$' + AValue.Text + '" of required type "' + AType.ToText + '" was not provided.';
      Exit;
    end;
    if IsNullValue(vVar) and (AType.Kind = gtNonNull) then
    begin
      Result := 'Expected non-nullable type "' + AType.ToText + '" not to be null.';
      Exit;
    end;
    AResult := CloneJSON(vVar);
    if AResult = nil then
      AResult := NewNull;
    Exit;
  end;
  if AType.Kind = gtNonNull then
  begin
    if AValue.Kind = gvNull then
    begin
      Result := 'Expected value of type "' + AType.ToText + '", found null.';
      Exit;
    end;
    Result := CoerceLiteral(AValue, AType.OfType, AResult);
    Exit;
  end;
  if AValue.Kind = gvNull then
  begin
    AResult := NewNull;
    Exit;
  end;
  if AType.Kind = gtList then
  begin
    vArr := TRALJSONArray.Create;
    AResult := vArr;
    if AValue.Kind = gvList then
    begin
      for vInt := 0 to AValue.Items.Count - 1 do
      begin
        Result := CoerceLiteral(TRALGQLValue(AValue.Items[vInt]), AType.OfType, vItem);
        if Result <> '' then
        begin
          FreeAndNil(AResult);
          Exit;
        end;
        vArr.Add(vItem);
      end;
    end
    else
    begin
      // a single value where a list is expected is a list of one
      Result := CoerceLiteral(AValue, AType.OfType, vItem);
      if Result <> '' then
      begin
        FreeAndNil(AResult);
        Exit;
      end;
      vArr.Add(vItem);
    end;
    Exit;
  end;
  vTypeDef := FSchema.FindType(AType.Name);
  vText := RALGraphQLPrintValue(AValue);
  case vTypeDef.Kind of
    gkScalar:
      if vTypeDef.Name = 'Int' then
      begin
        if (AValue.Kind <> gvInt) or not TryStrToInt64(string(AValue.Text), vInt64) or
           (vInt64 < -2147483648) or (vInt64 > 2147483647) then
          Result := 'Int cannot represent non-integer value: ' + vText
        else
          AResult := TRALJSONNumber.Create(AValue.Text);
      end
      else if vTypeDef.Name = 'Float' then
      begin
        if not (AValue.Kind in [gvInt, gvFloat]) then
          Result := 'Float cannot represent non numeric value: ' + vText
        else
          AResult := TRALJSONNumber.Create(AValue.Text);
      end
      else if vTypeDef.Name = 'String' then
      begin
        if AValue.Kind <> gvString then
          Result := 'String cannot represent a non string value: ' + vText
        else
          AResult := TRALJSONString.Create(AValue.Text);
      end
      else if vTypeDef.Name = 'Boolean' then
      begin
        if AValue.Kind <> gvBoolean then
          Result := 'Boolean cannot represent a non boolean value: ' + vText
        else
          AResult := TRALJSONBoolean.Create(AValue.Text = 'true');
      end
      else if vTypeDef.Name = 'ID' then
      begin
        if not (AValue.Kind in [gvString, gvInt]) then
          Result := 'ID cannot represent a non-string and non-integer value: ' + vText
        else
          AResult := TRALJSONString.Create(AValue.Text);
      end
      else
      begin
        // a custom scalar: the literal as JSON
        case AValue.Kind of
          gvInt, gvFloat: AResult := TRALJSONNumber.Create(AValue.Text);
          gvBoolean: AResult := TRALJSONBoolean.Create(AValue.Text = 'true');
        else
          AResult := TRALJSONString.Create(AValue.Text);
        end;
      end;
    gkEnum:
      if (AValue.Kind <> gvEnum) or (vTypeDef.Field(AValue.Text) = nil) then
        Result := 'Value "' + vText + '" does not exist in "' + vTypeDef.Name + '" enum.'
      else
        AResult := TRALJSONString.Create(AValue.Text);
    gkInputObject:
      begin
        if AValue.Kind <> gvObject then
        begin
          Result := 'Expected value of type "' + vTypeDef.Name + '", found ' + vText + '.';
          Exit;
        end;
        vObj := TRALJSONObject.Create;
        AResult := vObj;
        for vInt := 0 to AValue.Items.Count - 1 do
          if vTypeDef.Field(TRALGQLObjectField(AValue.Items[vInt]).Name) = nil then
          begin
            Result := 'Field "' + TRALGQLObjectField(AValue.Items[vInt]).Name +
              '" is not defined by type "' + vTypeDef.Name + '".';
            FreeAndNil(AResult);
            Exit;
          end;
        for vField := 0 to vTypeDef.Fields.Count - 1 do
        begin
          vFieldDef := TRALGQLFieldDef(vTypeDef.Fields[vField]);
          vLiteral := nil;
          for vInt := 0 to AValue.Items.Count - 1 do
            if TRALGQLObjectField(AValue.Items[vInt]).Name = vFieldDef.Name then
              vLiteral := TRALGQLObjectField(AValue.Items[vInt]).Value;
          if vLiteral = nil then
            vLiteral := vFieldDef.DefaultValue;
          if vLiteral = nil then
          begin
            if vFieldDef.TypeRef.Kind = gtNonNull then
            begin
              Result := 'Field "' + vTypeDef.Name + '.' + vFieldDef.Name + '" of required type "' +
                vFieldDef.TypeRef.ToText + '" was not provided.';
              FreeAndNil(AResult);
              Exit;
            end;
            Continue;
          end;
          Result := CoerceLiteral(vLiteral, vFieldDef.TypeRef, vItem);
          if Result <> '' then
          begin
            FreeAndNil(AResult);
            Exit;
          end;
          vObj.Add(vFieldDef.Name, vItem);
        end;
      end;
  else
    Result := 'Type "' + vTypeDef.Name + '" is not an input type.';
  end;
  if (Result = '') and (AResult = nil) then
    AResult := NewNull;
end;

function TRALGraphQLExecution.CoerceInput(AValue: TRALJSON; AType: TRALGQLTypeRef;
  out AResult: TRALJSON): StringRAL;
var
  vTypeDef: TRALGQLTypeDef;
  vInt: IntegerRAL;
  vItem, vField: TRALJSON;
  vArr: TRALJSONArray;
  vObj, vIn: TRALJSONObject;
  vFieldDef: TRALGQLFieldDef;
  vInt64: Int64;
  vDefault: StringRAL;
begin
  Result := '';
  AResult := nil;
  if AType.Kind = gtNonNull then
  begin
    if IsNullValue(AValue) then
    begin
      Result := 'Expected non-nullable type "' + AType.ToText + '" not to be null.';
      Exit;
    end;
    Result := CoerceInput(AValue, AType.OfType, AResult);
    Exit;
  end;
  if IsNullValue(AValue) then
  begin
    AResult := NewNull;
    Exit;
  end;
  if AType.Kind = gtList then
  begin
    vArr := TRALJSONArray.Create;
    AResult := vArr;
    if AValue.Kind = rjkArray then
    begin
      for vInt := 0 to TRALJSONArray(AValue).Count - 1 do
      begin
        Result := CoerceInput(TRALJSONArray(AValue).Items[vInt], AType.OfType, vItem);
        if Result <> '' then
        begin
          FreeAndNil(AResult);
          Exit;
        end;
        vArr.Add(vItem);
      end;
    end
    else
    begin
      Result := CoerceInput(AValue, AType.OfType, vItem);
      if Result <> '' then
      begin
        FreeAndNil(AResult);
        Exit;
      end;
      vArr.Add(vItem);
    end;
    Exit;
  end;
  vTypeDef := FSchema.FindType(AType.Name);
  case vTypeDef.Kind of
    gkScalar:
      if vTypeDef.Name = 'Int' then
      begin
        if not IsInt32(AValue, vInt64) then
          Result := 'Int cannot represent non-integer value: ' + AValue.ToJSON
        else
          AResult := TRALJSONNumber.Create(StringRAL(IntToStr(vInt64)));
      end
      else if vTypeDef.Name = 'Float' then
      begin
        if AValue.Kind <> rjkNumber then
          Result := 'Float cannot represent non numeric value: ' + AValue.ToJSON
        else
          AResult := CloneJSON(AValue);
      end
      else if vTypeDef.Name = 'String' then
      begin
        if AValue.Kind <> rjkString then
          Result := 'String cannot represent a non string value: ' + AValue.ToJSON
        else
          AResult := CloneJSON(AValue);
      end
      else if vTypeDef.Name = 'Boolean' then
      begin
        if AValue.Kind <> rjkBoolean then
          Result := 'Boolean cannot represent a non boolean value: ' + AValue.ToJSON
        else
          AResult := CloneJSON(AValue);
      end
      else if vTypeDef.Name = 'ID' then
      begin
        if AValue.Kind = rjkString then
          AResult := CloneJSON(AValue)
        else if IsInt32(AValue, vInt64) or ((AValue.Kind = rjkNumber) and (Frac(AValue.AsNumber) = 0)) then
          AResult := TRALJSONString.Create(TRALJSONNumber(AValue).Value)
        else
          Result := 'ID cannot represent value: ' + AValue.ToJSON;
      end
      else
        AResult := CloneJSON(AValue);
    gkEnum:
      if (AValue.Kind <> rjkString) or (vTypeDef.Field(AValue.AsString) = nil) then
        Result := 'Value ' + AValue.ToJSON + ' does not exist in "' + vTypeDef.Name + '" enum.'
      else
        AResult := CloneJSON(AValue);
    gkInputObject:
      begin
        if AValue.Kind <> rjkObject then
        begin
          Result := 'Expected type "' + vTypeDef.Name + '" to be an object.';
          Exit;
        end;
        vIn := TRALJSONObject(AValue);
        for vInt := 0 to vIn.Count - 1 do
          if vTypeDef.Field(vIn.Names[vInt]) = nil then
          begin
            Result := 'Field "' + vIn.Names[vInt] + '" is not defined by type "' + vTypeDef.Name + '".';
            Exit;
          end;
        vObj := TRALJSONObject.Create;
        AResult := vObj;
        for vInt := 0 to vTypeDef.Fields.Count - 1 do
        begin
          vFieldDef := TRALGQLFieldDef(vTypeDef.Fields[vInt]);
          vField := vIn.Find(vFieldDef.Name);
          if vField = nil then
          begin
            if vFieldDef.DefaultValue <> nil then
            begin
              vDefault := CoerceLiteral(vFieldDef.DefaultValue, vFieldDef.TypeRef, vItem);
              if vDefault = '' then
                vObj.Add(vFieldDef.Name, vItem);
            end
            else if vFieldDef.TypeRef.Kind = gtNonNull then
            begin
              Result := 'Field "' + vFieldDef.Name + '" of required type "' + vFieldDef.TypeRef.ToText +
                '" was not provided.';
              FreeAndNil(AResult);
              Exit;
            end;
            Continue;
          end;
          Result := CoerceInput(vField, vFieldDef.TypeRef, vItem);
          if Result <> '' then
          begin
            Result := 'Field "' + vFieldDef.Name + '": ' + Result;
            FreeAndNil(AResult);
            Exit;
          end;
          vObj.Add(vFieldDef.Name, vItem);
        end;
      end;
  else
    Result := 'Type "' + vTypeDef.Name + '" is not an input type.';
  end;
end;

function TRALGraphQLExecution.CoerceVariables: boolean;
var
  vInt: IntegerRAL;
  vDef: TRALGQLVariableDef;
  vIn, vValue: TRALJSON;
  vError: StringRAL;
begin
  FVariables := TRALJSONObject.Create;
  for vInt := 0 to FOperation.Variables.Count - 1 do
  begin
    vDef := TRALGQLVariableDef(FOperation.Variables[vInt]);
    vIn := nil;
    if FVariablesIn <> nil then
      vIn := FVariablesIn.Find(vDef.Name);
    if vIn = nil then
    begin
      if vDef.DefaultValue <> nil then
      begin
        vError := CoerceLiteral(vDef.DefaultValue, vDef.TypeRef, vValue);
        if vError = '' then
          FVariables.Add(vDef.Name, vValue)
        else
          AddError('Variable "$' + vDef.Name + '" got invalid default value: ' + vError, vDef, nil);
      end
      else if vDef.TypeRef.Kind = gtNonNull then
        AddError('Variable "$' + vDef.Name + '" of required type "' + vDef.TypeRef.ToText +
          '" was not provided.', vDef, nil);
      Continue;
    end;
    vError := CoerceInput(vIn, vDef.TypeRef, vValue);
    if vError <> '' then
      AddError('Variable "$' + vDef.Name + '" got invalid value ' + vIn.ToJSON + '; ' + vError, vDef, nil)
    else
      FVariables.Add(vDef.Name, vValue);
  end;
  Result := FErrors.Count = 0;
end;

function TRALGraphQLExecution.CoerceArgs(AField: TRALGQLFieldDef; ASelection: TRALGQLSelection;
  out AArgs: TRALJSONObject): StringRAL;
var
  vInt: IntegerRAL;
  vDef: TRALGQLFieldDef;
  vArg: TRALGQLArgument;
  vValue: TRALJSON;
begin
  Result := '';
  AArgs := TRALJSONObject.Create;
  if AField = nil then
    Exit;
  for vInt := 0 to AField.Args.Count - 1 do
  begin
    vDef := TRALGQLFieldDef(AField.Args[vInt]);
    vArg := ASelection.Arg(vDef.Name);
    // a variable that was not given counts as not given
    if (vArg <> nil) and (vArg.Value.Kind = gvVariable) and
       ((FVariables = nil) or (FVariables.Find(vArg.Value.Text) = nil)) then
      vArg := nil;
    if vArg = nil then
    begin
      if vDef.DefaultValue <> nil then
      begin
        Result := CoerceLiteral(vDef.DefaultValue, vDef.TypeRef, vValue);
        if Result <> '' then
          Exit;
        AArgs.Add(vDef.Name, vValue);
      end
      else if vDef.TypeRef.Kind = gtNonNull then
      begin
        Result := 'Argument "' + vDef.Name + '" of required type "' + vDef.TypeRef.ToText +
          '" was not provided.';
        Exit;
      end;
      Continue;
    end;
    Result := CoerceLiteral(vArg.Value, vDef.TypeRef, vValue);
    if Result <> '' then
    begin
      Result := 'Argument "' + vDef.Name + '" has invalid value ' + RALGraphQLPrintValue(vArg.Value) +
        '. ' + Result;
      Exit;
    end;
    AArgs.Add(vDef.Name, vValue);
  end;
end;

{ ---- execution ---- }

function TRALGraphQLExecution.ApplyDirectives(ADirectives: TObjectList): boolean;
var
  vInt: IntegerRAL;
  vDir: TRALGQLDirective;
  vIf: TRALGQLValue;
  vValue: boolean;
  vVar: TRALJSON;
begin
  Result := True;
  for vInt := 0 to ADirectives.Count - 1 do
  begin
    vDir := TRALGQLDirective(ADirectives[vInt]);
    if vDir.Arg('if') = nil then
      Continue;
    vIf := vDir.Arg('if').Value;
    if vIf.Kind = gvVariable then
    begin
      vVar := nil;
      if FVariables <> nil then
        vVar := FVariables.Find(vIf.Text);
      vValue := (vVar <> nil) and (vVar.Kind = rjkBoolean) and vVar.AsBoolean;
    end
    else
      vValue := vIf.Text = 'true';
    if (vDir.Name = 'skip') and vValue then
      Result := False
    else if (vDir.Name = 'include') and not vValue then
      Result := False;
  end;
end;

procedure TRALGraphQLExecution.CollectFields(const AType: StringRAL; ASelections: TObjectList;
  AGroups: TStringList; AVisited: TStrings);
var
  vInt, vIdx: IntegerRAL;
  vSel: TRALGQLSelection;
  vFragment: TRALGQLExecutable;
begin
  for vInt := 0 to ASelections.Count - 1 do
  begin
    vSel := TRALGQLSelection(ASelections[vInt]);
    if not ApplyDirectives(vSel.Directives) then
      Continue;
    case vSel.Kind of
      gsField:
        begin
          vIdx := AGroups.IndexOf(string(vSel.ResponseKey));
          if vIdx < 0 then
            vIdx := AGroups.AddObject(string(vSel.ResponseKey), TList.Create);
          TList(AGroups.Objects[vIdx]).Add(vSel);
        end;
      gsFragmentSpread:
        begin
          if AVisited.IndexOf(string(vSel.Name)) >= 0 then
            Continue;
          vFragment := FDocument.Fragment(vSel.Name);
          if (vFragment = nil) or not DoesApply(vFragment.TypeCondition, AType) then
            Continue;
          AVisited.Add(string(vSel.Name));
          CollectFields(AType, vFragment.Selections, AGroups, AVisited);
        end;
      gsInlineFragment:
        if DoesApply(vSel.TypeCondition, AType) then
          CollectFields(AType, vSel.Selections, AGroups, AVisited);
    end;
  end;
end;

/// Frees the lists of a group list made by CollectFields.
procedure FreeGroups(AGroups: TStringList);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to AGroups.Count - 1 do
    TList(AGroups.Objects[vInt]).Free;
  AGroups.Free;
end;

function TRALGraphQLExecution.ExecuteSelections(const AType: StringRAL; AValue: TRALJSON;
  AFields: TList; APath: TStrings; out AResult: TRALJSON): boolean;
var
  vGroups: TStringList;
  vVisited: TStringList;
  vInt: IntegerRAL;
  vObj: TRALJSONObject;
  vValue: TRALJSON;
begin
  Result := True;
  vGroups := TStringList.Create;
  vVisited := TStringList.Create;
  vObj := TRALJSONObject.Create;
  AResult := vObj;
  try
    for vInt := 0 to AFields.Count - 1 do
      CollectFields(AType, TObjectList(AFields[vInt]), vGroups, vVisited);
    for vInt := 0 to vGroups.Count - 1 do
    begin
      APath.Add(vGroups[vInt]);
      try
        if not ExecuteField(AType, AValue, TList(vGroups.Objects[vInt]), APath, vValue) then
        begin
          // a non-null field went null: so does this object
          FreeAndNil(AResult);
          Result := False;
          Exit;
        end;
        vObj.Add(StringRAL(vGroups[vInt]), vValue);
      finally
        APath.Delete(APath.Count - 1);
      end;
    end;
  finally
    FreeGroups(vGroups);
    vVisited.Free;
  end;
end;

function TRALGraphQLExecution.ExecuteField(const AType: StringRAL; AParent: TRALJSON;
  AFields: TList; APath: TStrings; out AResult: TRALJSON): boolean;
var
  vSel: TRALGQLSelection;
  vField: TRALGQLFieldDef;
  vInfo: TRALGraphQLResolve;
  vResolver: TRALGraphQLOnResolve;
  vError: StringRAL;
  vSub: TList;
  vInt: IntegerRAL;
  vValue: TRALJSON;
  vTypeRef: TRALGQLTypeRef;
  vOwnedRef: TRALGQLTypeRef;
  vStringRef: TRALGQLTypeRef;
begin
  Result := True;
  AResult := nil;
  vSel := TRALGQLSelection(AFields[0]);
  if vSel.Name = '__typename' then
  begin
    AResult := TRALJSONString.Create(AType);
    Exit;
  end;
  vOwnedRef := nil;
  vField := FSchema.FindField(AType, vSel.Name);
  if (vField = nil) and (AType = FSchema.QueryType) and
     ((vSel.Name = '__schema') or (vSel.Name = '__type')) then
  begin
    vOwnedRef := TRALGQLTypeRef.Create;
    vOwnedRef.Kind := gtNamed;
    if vSel.Name = '__schema' then
    begin
      vOwnedRef.Name := '__Schema';
      vTypeRef := TRALGQLTypeRef.Create;
      vTypeRef.Kind := gtNonNull;
      vTypeRef.OfType := vOwnedRef;
      vOwnedRef := vTypeRef;
    end
    else
      vOwnedRef.Name := '__Type';
    vTypeRef := vOwnedRef;
  end
  else if vField = nil then
  begin
    AResult := NewNull;
    Exit;
  end
  else
    vTypeRef := vField.TypeRef;
  vInfo := TRALGraphQLResolve.Create;
  vSub := TList.Create;
  try
    vInfo.FExecution := Self;
    vInfo.FFieldName := vSel.Name;
    vInfo.FParentType := AType;
    vInfo.FParent := AParent;
    vInfo.FRequest := FRequest;
    vInfo.FContext := FContext;
    vInfo.FSelection := vSel;
    if vOwnedRef <> nil then
    begin
      vInfo.FArgs := TRALJSONObject.Create;
      if vSel.Arg('name') <> nil then
      begin
        // name: String!
        vStringRef := TRALGQLTypeRef.Create;
        try
          vStringRef.Kind := gtNamed;
          vStringRef.Name := 'String';
          vError := CoerceLiteral(vSel.Arg('name').Value, vStringRef, vValue);
        finally
          vStringRef.Free;
        end;
        if (vError = '') and (vValue <> nil) then
          vInfo.FArgs.Add('name', vValue)
        else
          vValue.Free;
      end;
      Introspect(vInfo, nil);
    end
    else
    begin
      vError := CoerceArgs(vField, vSel, vInfo.FArgs);
      if vError <> '' then
        vInfo.Fail(vError)
      else if Copy(AType, 1, 2) = '__' then
        Introspect(vInfo, vField)
      else
      begin
        vResolver := FSchema.Resolver(AType, vSel.Name);
        if Assigned(vResolver) then
          try
            vResolver(vInfo);
          except
            on e: Exception do
              vInfo.Fail(StringRAL(e.Message));
          end
        else if (AParent <> nil) and (AParent.Kind = rjkObject) then
          // the default resolver: the field of the parent's value
          vInfo.SetResult(CloneJSON(TRALJSONObject(AParent).Find(vSel.Name)));
      end;
    end;
    if vInfo.FFailed then
    begin
      AddError(vInfo.FError, vSel, APath);
      if vTypeRef.Kind = gtNonNull then
        Result := False
      else
        AResult := NewNull;
      Exit;
    end;
    for vInt := 0 to AFields.Count - 1 do
      vSub.Add(TRALGQLSelection(AFields[vInt]).Selections);
    if not CompleteValue(vTypeRef, vInfo.FResult, vSub, APath, AType + '.' + vSel.Name, vSel,
      AResult) then
    begin
      if vTypeRef.Kind = gtNonNull then
        Result := False
      else
        AResult := NewNull;
    end;
  finally
    vSub.Free;
    vInfo.Free;
    vOwnedRef.Free;
  end;
end;

function TRALGraphQLExecution.CompleteValue(AType: TRALGQLTypeRef; AValue: TRALJSON;
  AFields: TList; APath: TStrings; const AField: StringRAL; ANode: TRALGQLNode;
  out AResult: TRALJSON): boolean;
var
  vTypeDef: TRALGQLTypeDef;
  vArr: TRALJSONArray;
  vInt: IntegerRAL;
  vItem: TRALJSON;
  vInt64: Int64;
  vRuntime: StringRAL;
  vTypeName: TRALJSON;
  vSel: TRALGQLNode;
begin
  Result := True;
  AResult := nil;
  vSel := ANode;
  if AType.Kind = gtNonNull then
  begin
    if not CompleteValue(AType.OfType, AValue, AFields, APath, AField, ANode, AResult) then
    begin
      Result := False;
      Exit;
    end;
    if IsNullValue(AResult) then
    begin
      FreeAndNil(AResult);
      AddError('Cannot return null for non-nullable field ' + AField + '.', vSel, APath);
      Result := False;
    end;
    Exit;
  end;
  if IsNullValue(AValue) then
  begin
    AResult := NewNull;
    Exit;
  end;
  if AType.Kind = gtList then
  begin
    if AValue.Kind <> rjkArray then
    begin
      AddError('Expected Iterable, but did not find one for field ' + AField + '.', vSel, APath);
      AResult := NewNull;
      Exit;
    end;
    vArr := TRALJSONArray.Create;
    AResult := vArr;
    for vInt := 0 to TRALJSONArray(AValue).Count - 1 do
    begin
      APath.Add('#' + IntToStr(vInt));
      try
        if not CompleteValue(AType.OfType, TRALJSONArray(AValue).Items[vInt], AFields, APath, AField,
          ANode, vItem) then
        begin
          if AType.OfType.Kind = gtNonNull then
          begin
            // a non-null item went null: so does the list
            FreeAndNil(AResult);
            Result := False;
            Exit;
          end;
          vItem := NewNull;
        end;
        vArr.Add(vItem);
      finally
        APath.Delete(APath.Count - 1);
      end;
    end;
    Exit;
  end;
  vTypeDef := FSchema.FindType(AType.Name);
  case vTypeDef.Kind of
    gkScalar:
      if vTypeDef.Name = 'Int' then
      begin
        if (AValue.Kind = rjkString) and TryStrToInt64(string(AValue.AsString), vInt64) then
          AResult := TRALJSONNumber.Create(StringRAL(IntToStr(vInt64)))
        else if IsInt32(AValue, vInt64) then
          AResult := TRALJSONNumber.Create(StringRAL(IntToStr(vInt64)))
        else if AValue.Kind = rjkBoolean then
          AResult := TRALJSONNumber.Create(StringRAL(IntToStr(Ord(AValue.AsBoolean))))
        else
        begin
          AddError('Int cannot represent non-integer value: ' + AValue.ToJSON, vSel, APath);
          AResult := NewNull;
          Result := False;
        end;
      end
      else if vTypeDef.Name = 'Float' then
      begin
        if AValue.Kind = rjkNumber then
          AResult := CloneJSON(AValue)
        else if (AValue.Kind = rjkString) and (StrToFloatDef(string(AValue.AsString), -1.5e300,
          RALInvariantFormat) <> -1.5e300) then
          AResult := TRALJSONNumber.Create(AValue.AsString)
        else
        begin
          AddError('Float cannot represent non numeric value: ' + AValue.ToJSON, vSel, APath);
          Result := False;
        end;
      end
      else if (vTypeDef.Name = 'String') or (vTypeDef.Name = 'ID') then
      begin
        case AValue.Kind of
          rjkString: AResult := CloneJSON(AValue);
          rjkNumber: AResult := TRALJSONString.Create(TRALJSONNumber(AValue).Value);
          rjkBoolean:
            if AValue.AsBoolean then
              AResult := TRALJSONString.Create('true')
            else
              AResult := TRALJSONString.Create('false');
        else
          AddError(vTypeDef.Name + ' cannot represent value: ' + AValue.ToJSON, vSel, APath);
          Result := False;
        end;
      end
      else if vTypeDef.Name = 'Boolean' then
      begin
        if AValue.Kind = rjkBoolean then
          AResult := CloneJSON(AValue)
        else if AValue.Kind = rjkNumber then
          AResult := TRALJSONBoolean.Create(AValue.AsNumber <> 0)
        else
        begin
          AddError('Boolean cannot represent a non boolean value: ' + AValue.ToJSON, vSel, APath);
          Result := False;
        end;
      end
      else
        AResult := CloneJSON(AValue);
    gkEnum:
      if (AValue.Kind = rjkString) and (vTypeDef.Field(AValue.AsString) <> nil) then
        AResult := CloneJSON(AValue)
      else
      begin
        AddError('Enum "' + vTypeDef.Name + '" cannot represent value: ' + AValue.ToJSON, vSel, APath);
        Result := False;
      end;
    gkObject:
      Result := ExecuteSelections(vTypeDef.Name, AValue, AFields, APath, AResult);
    gkInterface, gkUnion:
      begin
        vRuntime := '';
        if Assigned(FOnResolveType) then
          FOnResolveType(vTypeDef.Name, AValue, vRuntime);
        if (vRuntime = '') and (AValue.Kind = rjkObject) then
        begin
          vTypeName := TRALJSONObject(AValue).Find('__typename');
          if (vTypeName <> nil) and (vTypeName.Kind = rjkString) then
            vRuntime := vTypeName.AsString;
        end;
        if (vRuntime = '') and (vTypeDef.Kind = gkUnion) and (vTypeDef.PossibleTypes.Count = 1) then
          vRuntime := StringRAL(vTypeDef.PossibleTypes[0]);
        if (vRuntime = '') or (FSchema.FindType(vRuntime) = nil) or
           (FSchema.FindType(vRuntime).Kind <> gkObject) or not FSchema.IsPossible(vTypeDef.Name, vRuntime) then
        begin
          AddError('Abstract type "' + vTypeDef.Name + '" must resolve to an Object type at runtime ' +
            '(give "__typename" in the value, or OnResolveType).', vSel, APath);
          Result := False;
          Exit;
        end;
        Result := ExecuteSelections(vRuntime, AValue, AFields, APath, AResult);
      end;
  else
    AResult := NewNull;
  end;
end;

{ ---- introspection ---- }

/// The __Type of a type reference: wrappers with ofType, a named type by its name.
function TypeRefJSON(ASchema: TRALGraphQLSchema; ARef: TRALGQLTypeRef): TRALJSON; forward;

function NamedTypeJSON(ASchema: TRALGraphQLSchema; ADef: TRALGQLTypeDef): TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('kind', cKindNames[ADef.Kind]);
  Result.Add('name', ADef.Name);
  if ADef.Description <> '' then
    Result.Add('description', ADef.Description)
  else
    Result.Add('description');
  if ADef.SpecifiedByURL <> '' then
    Result.Add('specifiedByURL', ADef.SpecifiedByURL)
  else
    Result.Add('specifiedByURL');
  Result.Add('ofType');
end;

function TypeRefJSON(ASchema: TRALGraphQLSchema; ARef: TRALGQLTypeRef): TRALJSON;
var
  vObj: TRALJSONObject;
begin
  if ARef.Kind = gtNamed then
  begin
    Result := NamedTypeJSON(ASchema, ASchema.FindType(ARef.Name));
    Exit;
  end;
  vObj := TRALJSONObject.Create;
  if ARef.Kind = gtList then
    vObj.Add('kind', 'LIST')
  else
    vObj.Add('kind', 'NON_NULL');
  vObj.Add('name');
  vObj.Add('description');
  vObj.Add('specifiedByURL');
  vObj.Add('ofType', TypeRefJSON(ASchema, ARef.OfType));
  Result := vObj;
end;

function InputValueJSON(ASchema: TRALGraphQLSchema; ADef: TRALGQLFieldDef): TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('name', ADef.Name);
  if ADef.Description <> '' then
    Result.Add('description', ADef.Description)
  else
    Result.Add('description');
  Result.Add('type', TypeRefJSON(ASchema, ADef.TypeRef));
  if ADef.DefaultValue <> nil then
    Result.Add('defaultValue', RALGraphQLPrintValue(ADef.DefaultValue))
  else
    Result.Add('defaultValue');
  Result.Add('isDeprecated', ADef.Deprecated);
  if ADef.Deprecated then
    Result.Add('deprecationReason', ADef.DeprecationReason)
  else
    Result.Add('deprecationReason');
end;

procedure TRALGraphQLExecution.Introspect(AInfo: TRALGraphQLResolve; AField: TRALGQLFieldDef);
var
  vObj: TRALJSONObject;
  vArr, vLocs: TRALJSONArray;
  vInt, vArg: IntegerRAL;
  vDef, vOther: TRALGQLTypeDef;
  vFieldDef: TRALGQLFieldDef;
  vDeprecated: boolean;
  vName: StringRAL;
  vParent: TRALJSONObject;
  vNode: TRALJSON;

  function Directive(const AName, ADescription: StringRAL; const ALocations: array of StringRAL;
    const AArgName, AArgType, AArgDefault: StringRAL): TRALJSONObject;
  var
    vL: IntegerRAL;
    vArgs: TRALJSONArray;
    vArgObj, vType, vInner: TRALJSONObject;
  begin
    Result := TRALJSONObject.Create;
    Result.Add('name', AName);
    Result.Add('description', ADescription);
    Result.Add('isRepeatable', False);
    vLocs := TRALJSONArray.Create;
    for vL := 0 to High(ALocations) do
      vLocs.Add(ALocations[vL]);
    Result.Add('locations', vLocs);
    vArgs := TRALJSONArray.Create;
    if AArgName <> '' then
    begin
      vArgObj := TRALJSONObject.Create;
      vArgObj.Add('name', AArgName);
      vArgObj.Add('description');
      vInner := NamedTypeJSON(FSchema, FSchema.FindType(Copy(AArgType, 1, Pos('!', AArgType + '!') - 1)));
      if Pos('!', AArgType) > 0 then
      begin
        vType := TRALJSONObject.Create;
        vType.Add('kind', 'NON_NULL');
        vType.Add('name');
        vType.Add('description');
        vType.Add('specifiedByURL');
        vType.Add('ofType', vInner);
        vArgObj.Add('type', vType);
      end
      else
        vArgObj.Add('type', vInner);
      if AArgDefault <> '' then
        vArgObj.Add('defaultValue', AArgDefault)
      else
        vArgObj.Add('defaultValue');
      vArgObj.Add('isDeprecated', False);
      vArgObj.Add('deprecationReason');
      vArgs.Add(vArgObj);
    end;
    Result.Add('args', vArgs);
  end;

begin
  vName := AInfo.FieldName;
  // the roots: __schema and __type(name:)
  if AField = nil then
  begin
    if vName = '__schema' then
    begin
      vObj := TRALJSONObject.Create;
      vObj.Add('description');
      vArr := TRALJSONArray.Create;
      for vInt := 0 to FSchema.FTypes.Count - 1 do
        vArr.Add(NamedTypeJSON(FSchema, TRALGQLTypeDef(FSchema.FTypes.Objects[vInt])));
      vObj.Add('types', vArr);
      vObj.Add('queryType', NamedTypeJSON(FSchema, FSchema.FindType(FSchema.QueryType)));
      if FSchema.MutationType <> '' then
        vObj.Add('mutationType', NamedTypeJSON(FSchema, FSchema.FindType(FSchema.MutationType)))
      else
        vObj.Add('mutationType');
      if FSchema.SubscriptionType <> '' then
        vObj.Add('subscriptionType', NamedTypeJSON(FSchema, FSchema.FindType(FSchema.SubscriptionType)))
      else
        vObj.Add('subscriptionType');
      vArr := TRALJSONArray.Create;
      vArr.Add(Directive('include', 'Directs the executor to include this field or fragment only when the `if` argument is true.',
        ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT'], 'if', 'Boolean!', ''));
      vArr.Add(Directive('skip', 'Directs the executor to skip this field or fragment when the `if` argument is true.',
        ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT'], 'if', 'Boolean!', ''));
      vArr.Add(Directive('deprecated', 'Marks an element of a GraphQL schema as no longer supported.',
        ['FIELD_DEFINITION', 'ARGUMENT_DEFINITION', 'INPUT_FIELD_DEFINITION', 'ENUM_VALUE'], 'reason', 'String',
        '"No longer supported"'));
      vArr.Add(Directive('specifiedBy', 'Exposes a URL that specifies the behavior of this scalar.',
        ['SCALAR'], 'url', 'String!', ''));
      vObj.Add('directives', vArr);
      AInfo.SetResult(vObj);
    end
    else
    begin
      vDef := FSchema.FindType(AInfo.ArgAsString('name'));
      if vDef <> nil then
        AInfo.SetResult(NamedTypeJSON(FSchema, vDef));
    end;
    Exit;
  end;
  // the fields of the introspection types read the parent, or the schema by its name
  if (AInfo.Parent = nil) or (AInfo.Parent.Kind <> rjkObject) then
    Exit;
  vParent := TRALJSONObject(AInfo.Parent);
  vDeprecated := (AInfo.Arg('includeDeprecated') <> nil) and AInfo.Arg('includeDeprecated').AsBoolean;
  vDef := nil;
  if (AInfo.ParentType = '__Type') and (vParent.Find('name') <> nil) and not vParent.Find('name').IsNull then
    vDef := FSchema.FindType(vParent.Find('name').AsString);
  if (AInfo.ParentType = '__Type') and (vName = 'fields') then
  begin
    if (vDef = nil) or not (vDef.Kind in [gkObject, gkInterface]) then
      Exit;
    vArr := TRALJSONArray.Create;
    for vInt := 0 to vDef.Fields.Count - 1 do
    begin
      vFieldDef := TRALGQLFieldDef(vDef.Fields[vInt]);
      if vFieldDef.Deprecated and not vDeprecated then
        Continue;
      vObj := TRALJSONObject.Create;
      vObj.Add('name', vFieldDef.Name);
      if vFieldDef.Description <> '' then
        vObj.Add('description', vFieldDef.Description)
      else
        vObj.Add('description');
      vLocs := TRALJSONArray.Create;
      for vArg := 0 to vFieldDef.Args.Count - 1 do
        vLocs.Add(InputValueJSON(FSchema, TRALGQLFieldDef(vFieldDef.Args[vArg])));
      vObj.Add('args', vLocs);
      vObj.Add('type', TypeRefJSON(FSchema, vFieldDef.TypeRef));
      vObj.Add('isDeprecated', vFieldDef.Deprecated);
      if vFieldDef.Deprecated then
        vObj.Add('deprecationReason', vFieldDef.DeprecationReason)
      else
        vObj.Add('deprecationReason');
      vArr.Add(vObj);
    end;
    AInfo.SetResult(vArr);
  end
  else if (AInfo.ParentType = '__Type') and (vName = 'interfaces') then
  begin
    if (vDef = nil) or not (vDef.Kind in [gkObject, gkInterface]) then
      Exit;
    vArr := TRALJSONArray.Create;
    for vInt := 0 to vDef.Interfaces.Count - 1 do
      vArr.Add(NamedTypeJSON(FSchema, FSchema.FindType(StringRAL(vDef.Interfaces[vInt]))));
    AInfo.SetResult(vArr);
  end
  else if (AInfo.ParentType = '__Type') and (vName = 'possibleTypes') then
  begin
    if (vDef = nil) or not (vDef.Kind in [gkInterface, gkUnion]) then
      Exit;
    vArr := TRALJSONArray.Create;
    if vDef.Kind = gkUnion then
      for vInt := 0 to vDef.PossibleTypes.Count - 1 do
        vArr.Add(NamedTypeJSON(FSchema, FSchema.FindType(StringRAL(vDef.PossibleTypes[vInt]))))
    else
      for vInt := 0 to FSchema.FTypes.Count - 1 do
      begin
        vOther := TRALGQLTypeDef(FSchema.FTypes.Objects[vInt]);
        if (vOther.Kind = gkObject) and (vOther.Interfaces.IndexOf(string(vDef.Name)) >= 0) then
          vArr.Add(NamedTypeJSON(FSchema, vOther));
      end;
    AInfo.SetResult(vArr);
  end
  else if (AInfo.ParentType = '__Type') and (vName = 'enumValues') then
  begin
    if (vDef = nil) or (vDef.Kind <> gkEnum) then
      Exit;
    vArr := TRALJSONArray.Create;
    for vInt := 0 to vDef.EnumValues.Count - 1 do
    begin
      vFieldDef := TRALGQLFieldDef(vDef.EnumValues[vInt]);
      if vFieldDef.Deprecated and not vDeprecated then
        Continue;
      vObj := TRALJSONObject.Create;
      vObj.Add('name', vFieldDef.Name);
      if vFieldDef.Description <> '' then
        vObj.Add('description', vFieldDef.Description)
      else
        vObj.Add('description');
      vObj.Add('isDeprecated', vFieldDef.Deprecated);
      if vFieldDef.Deprecated then
        vObj.Add('deprecationReason', vFieldDef.DeprecationReason)
      else
        vObj.Add('deprecationReason');
      vArr.Add(vObj);
    end;
    AInfo.SetResult(vArr);
  end
  else if (AInfo.ParentType = '__Type') and (vName = 'inputFields') then
  begin
    if (vDef = nil) or (vDef.Kind <> gkInputObject) then
      Exit;
    vArr := TRALJSONArray.Create;
    for vInt := 0 to vDef.Fields.Count - 1 do
      vArr.Add(InputValueJSON(FSchema, TRALGQLFieldDef(vDef.Fields[vInt])));
    AInfo.SetResult(vArr);
  end
  else if (AInfo.ParentType = '__Type') and (vName = 'isOneOf') then
  begin
    if (vDef <> nil) and (vDef.Kind = gkInputObject) then
      AInfo.SetResult(TRALJSONBoolean.Create(False));
  end
  else
  begin
    // the rest is in the parent's value
    vNode := vParent.Find(vName);
    AInfo.SetResult(CloneJSON(vNode));
  end;
end;

function TRALGraphQLExecution.Run: StringRAL;
var
  vOut: TRALJSONObject;
  vData: TRALJSON;
  vRoot: StringRAL;
  vFields: TList;
  vPath: TStringList;
  vRootValue: TRALJSON;
  vOwnRoot: boolean;
  vOk: boolean;
begin
  vOut := TRALJSONObject.Create;
  try
    vData := nil;
    try
      FDocument := RALGraphQLParse(FQuery, True, False);
    except
      on e: ERALGraphQLSyntax do
      begin
        FreeAndNil(FDocument);
        AddError(StringRAL(e.Message), nil, nil);
        TRALJSONObject(FErrors.Items[FErrors.Count - 1]).Add('locations',
          TRALJSON.ParseJSON(StringRAL(Format('[{"line":%d,"column":%d}]', [e.Line, e.Column]))));
      end;
    end;
    if (FDocument <> nil) and SelectOperation then
    begin
      Validate;
      if (FErrors.Count = 0) and (FOperation.Operation = goSubscription) then
        AddError('Subscriptions are not supported by this endpoint.', FOperation, nil);
      if (FErrors.Count = 0) and (FOperation.Operation = goMutation) and not FAllowMutation then
        AddError('Mutations are not allowed here (use POST).', FOperation, nil);
      if (FErrors.Count = 0) and CoerceVariables then
      begin
        case FOperation.Operation of
          goMutation: vRoot := FSchema.MutationType;
        else
          vRoot := FSchema.QueryType;
        end;
        vOwnRoot := FRootValue = nil;
        vRootValue := FRootValue;
        if vOwnRoot then
          vRootValue := TRALJSONObject.Create;
        vFields := TList.Create;
        vPath := TStringList.Create;
        try
          vFields.Add(FOperation.Selections);
          vOk := ExecuteSelections(vRoot, vRootValue, vFields, vPath, vData);
          if not vOk then
            vData := NewNull;
          vOut.Add('data', vData);
          vData := nil;
        finally
          vPath.Free;
          vFields.Free;
          if vOwnRoot then
            vRootValue.Free;
        end;
      end;
    end;
    if FErrors.Count > 0 then
    begin
      // the errors leave the execution and go in the answer
      vOut.Add('errors', FErrors);
      FErrors := TRALJSONArray.Create;
    end;
    Result := vOut.ToJSON;
  finally
    vOut.Free;
  end;
end;

end.
