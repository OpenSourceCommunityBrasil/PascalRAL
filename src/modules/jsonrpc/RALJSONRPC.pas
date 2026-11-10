/// JSON-RPC 2.0: methods, requests, results and the dispatcher that answers them.
unit RALJSONRPC;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALJson;

const
  /// The JSON-RPC version every message carries.
  cRALJSONRPCVersion = '2.0';
  /// Invalid JSON was received.
  cRALJSONRPCParseError = -32700;
  /// The JSON sent is not a valid request object.
  cRALJSONRPCInvalidRequest = -32600;
  /// The method does not exist.
  cRALJSONRPCMethodNotFound = -32601;
  /// Invalid method parameters.
  cRALJSONRPCInvalidParams = -32602;
  /// Internal JSON-RPC error: the method raised.
  cRALJSONRPCInternalError = -32603;

type
  /// The JSON kind a declared parameter accepts.
  TRALJSONRPCParamType = (jptAny, jptString, jptNumber, jptInteger, jptBoolean,
    jptObject, jptArray);

  { A JSON-RPC error, raised by a method to answer with ACode, its message and
    optional data (JSON text). Codes from -32768 to -32000 are reserved. }
  ERALJSONRPCError = class(Exception)
  private
    FCode: IntegerRAL;
    FData: StringRAL;
  public
    /// The error ACode with AMessage and the optional JSON text AData.
    constructor CreateRPC(ACode: IntegerRAL; const AMessage: string;
      const AData: StringRAL = '');

    /// The error code.
    property Code: IntegerRAL read FCode;
    /// The error data as JSON text; empty when there is none.
    property Data: StringRAL read FData;
  end;

  TRALJSONRPCMethod = class;

  { One call (or notification) being answered. Params, Id and the values read
    from them belong to the request. }
  TRALJSONRPCRequest = class
  private
    FContext: TObject;
    FId: TRALJSON;
    FMethod: StringRAL;
    FMethodItem: TRALJSONRPCMethod;
    FParams: TRALJSON;
  public
    { The call AMethod with AParams and AId (nil for a notification), which it
      does not own. }
    constructor Create(const AMethod: StringRAL; AParams, AId: TRALJSON; AContext: TObject);

    /// The parameter AName (by name, or by its declared position); nil when absent.
    function FindParam(const AName: StringRAL): TRALJSON;
    /// Whether the call is a notification, which gets no answer.
    function IsNotification: Boolean;
    { The parameter AName (by name, or by its declared position); raises an
      Invalid params error when absent. }
    function Param(const AName: StringRAL): TRALJSON;
    /// The positional parameter at AIndex (from 0); nil when absent.
    function ParamAt(AIndex: IntegerRAL): TRALJSON;
    /// How many parameters came, by name or position.
    function ParamCount: IntegerRAL;

    /// What the transport handed over: a session, a connection; nil when none.
    property Context: TObject read FContext;
    /// The id of the call; nil for a notification.
    property Id: TRALJSON read FId;
    /// The method name.
    property Method: StringRAL read FMethod;
    /// The declared method that runs the call; nil when OnUnknownMethod answers it.
    property MethodItem: TRALJSONRPCMethod read FMethodItem write FMethodItem;
    /// The params as they came (an object or an array); nil when absent.
    property Params: TRALJSON read FParams;
  end;

  /// The result a method gives; Value is owned.
  TRALJSONRPCResult = class
  private
    FValue: TRALJSON;
  public
    destructor Destroy; override;

    /// Takes the value out; the caller then owns it.
    function Extract: TRALJSON;
    /// Answers the boolean AValue.
    procedure SetBoolean(AValue: Boolean);
    /// Answers the number AValue.
    procedure SetFloat(AValue: Double);
    /// Answers the integer AValue.
    procedure SetInteger(AValue: Int64);
    /// Answers JSON parsed from the text AJSON.
    procedure SetJSON(const AJSON: StringRAL);
    /// Answers null.
    procedure SetNull;
    /// Answers the text AValue.
    procedure SetString(const AValue: StringRAL);
    /// Answers AValue, which the result then owns.
    procedure SetValue(AValue: TRALJSON);

    /// The answer; nil answers null.
    property Value: TRALJSON read FValue;
  end;

  { Runs a call; set AResult or raise ERALJSONRPCError. Runs on the thread of
    the request. }
  TRALJSONRPCExecuteEvent = procedure(Sender: TObject; ARequest: TRALJSONRPCRequest;
    AResult: TRALJSONRPCResult) of object;

  { Runs a call to a method no item declares; set AHandled when it did, or the
    caller gets Method not found. }
  TRALJSONRPCUnknownEvent = procedure(Sender: TObject; ARequest: TRALJSONRPCRequest;
    AResult: TRALJSONRPCResult; var AHandled: Boolean) of object;

  /// A declared parameter of a method.
  TRALJSONRPCParam = class(TCollectionItem)
  private
    FName: StringRAL;
    FParamType: TRALJSONRPCParamType;
    FRequired: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;

    procedure Assign(ASource: TPersistent); override;
  published
    /// The parameter name; a positional call finds the parameter by its place.
    property Name: StringRAL read FName write FName;
    /// The JSON kind it accepts; another kind is Invalid params.
    property ParamType: TRALJSONRPCParamType read FParamType write FParamType default jptAny;
    /// Whether a call without it is Invalid params.
    property Required: Boolean read FRequired write FRequired default True;
  end;

  /// The declared parameters of a method, in positional order.
  TRALJSONRPCParams = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALJSONRPCParam;
  public
    /// Adds the parameter AName of AType and returns it.
    function AddParam(const AName: StringRAL; AType: TRALJSONRPCParamType = jptAny;
      ARequired: Boolean = True): TRALJSONRPCParam;
    /// The position of the parameter AName; -1 when it is not declared.
    function IndexOfName(const AName: StringRAL): IntegerRAL;

    /// The parameters by position.
    property Items[AIndex: IntegerRAL]: TRALJSONRPCParam read GetItem; default;
  end;

  { A method the dispatcher answers. With Params declared, a call is checked
    against them before OnExecute (count, required, kinds); without, anything
    reaches OnExecute. }
  TRALJSONRPCMethod = class(TCollectionItem)
  private
    FDescription: StringRAL;
    FName: StringRAL;
    FOnExecute: TRALJSONRPCExecuteEvent;
    FParams: TRALJSONRPCParams;

    procedure SetParams(AValue: TRALJSONRPCParams);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
    { Checks the params of ARequest against Params; raises an Invalid params
      error when they do not fit. }
    procedure CheckParams(ARequest: TRALJSONRPCRequest);
  published
    /// What the method does, for documentation.
    property Description: StringRAL read FDescription write FDescription;
    /// The method name callers use; case-sensitive.
    property Name: StringRAL read FName write FName;
    /// Runs the call.
    property OnExecute: TRALJSONRPCExecuteEvent read FOnExecute write FOnExecute;
    /// The declared parameters; empty = not checked.
    property Params: TRALJSONRPCParams read FParams write SetParams;
  end;

  /// The methods of a dispatcher.
  TRALJSONRPCMethods = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALJSONRPCMethod;
  public
    /// Adds the method AName with AEvent and returns it.
    function AddMethod(const AName: StringRAL; AEvent: TRALJSONRPCExecuteEvent;
      const ADescription: StringRAL = ''): TRALJSONRPCMethod;
    /// The method AName; nil when none.
    function Find(const AName: StringRAL): TRALJSONRPCMethod;

    /// The methods by position.
    property Items[AIndex: IntegerRAL]: TRALJSONRPCMethod read GetItem; default;
  end;

  { Answers JSON-RPC 2.0 text with JSON-RPC text, whatever carries it: HTTP,
    a WebSocket, stdio. Calls of a batch run in order, on the caller's thread. }
  TRALJSONRPCDispatcher = class(TPersistent)
  private
    FErrorDetails: Boolean;
    FMaxBatch: IntegerRAL;
    FMethods: TRALJSONRPCMethods;
    FOnUnknownMethod: TRALJSONRPCUnknownEvent;
    /// The persistent that owns the dispatcher, for streaming.
    FOwner: TPersistent;

    /// The answer to the single call ACall; nil for a notification.
    function ExecuteCall(ACall: TRALJSON; AContext: TObject): TRALJSON;
    procedure SetMethods(AValue: TRALJSONRPCMethods);
  protected
    function GetOwner: TPersistent; override;
  public
    /// A dispatcher whose methods are owned (for streaming) by AOwner.
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
    { The answer to the request text ARequest: a response, an array of them, or
      '' when nothing is answered (notifications only). AContext reaches the
      methods in TRALJSONRPCRequest.Context. }
    function Execute(const ARequest: StringRAL; AContext: TObject = nil): StringRAL;
    { The answer to the parsed request ARequest (an object or a batch array); nil
      when nothing is answered. The caller frees the result, not ARequest. }
    function ExecuteJSON(ARequest: TRALJSON; AContext: TObject = nil): TRALJSON;
  published
    /// Puts the message of an exception a method raised in the error data.
    property ErrorDetails: Boolean read FErrorDetails write FErrorDetails default False;
    /// The most calls one batch may hold; more is Invalid Request. 0 = no limit.
    property MaxBatch: IntegerRAL read FMaxBatch write FMaxBatch default 0;
    /// The methods answered.
    property Methods: TRALJSONRPCMethods read FMethods write SetMethods;
    /// Answers calls to methods not in Methods.
    property OnUnknownMethod: TRALJSONRPCUnknownEvent read FOnUnknownMethod
      write FOnUnknownMethod;
  end;

{ A new error response with ACode, AMessage, the optional JSON text AData and
  a copy of AId. }
function RALJSONRPCErrorResponse(AId: TRALJSON; ACode: IntegerRAL;
  const AMessage: StringRAL; const AData: StringRAL = ''): TRALJSONObject;
/// The standard message of the reserved code ACode; empty for other codes.
function RALJSONRPCMessage(ACode: IntegerRAL): StringRAL;
/// A new JSON value equal to AValue; nil for nil.
function RALJSONClone(AValue: TRALJSON): TRALJSON;

implementation

function RALJSONClone(AValue: TRALJSON): TRALJSON;
begin
  if AValue = nil then
    Result := nil
  else
    Result := TRALJSON.ParseJSON(AValue.ToJSON);
end;

function RALJSONRPCMessage(ACode: IntegerRAL): StringRAL;
begin
  case ACode of
    cRALJSONRPCParseError: Result := 'Parse error';
    cRALJSONRPCInvalidRequest: Result := 'Invalid Request';
    cRALJSONRPCMethodNotFound: Result := 'Method not found';
    cRALJSONRPCInvalidParams: Result := 'Invalid params';
    cRALJSONRPCInternalError: Result := 'Internal error';
  else
    Result := '';
  end;
end;

function RALJSONRPCErrorResponse(AId: TRALJSON; ACode: IntegerRAL;
  const AMessage: StringRAL; const AData: StringRAL): TRALJSONObject;
var
  vError: TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('jsonrpc', cRALJSONRPCVersion);
  vError := TRALJSONObject.Create;
  vError.Add('code', Int64(ACode));
  vError.Add('message', AMessage);
  if AData <> '' then
    vError.Add('data', TRALJSON.ParseJSON(AData));
  Result.Add('error', vError);
  if AId = nil then
    Result.Add('id', TRALJSONNull.Create)
  else
    Result.Add('id', RALJSONClone(AId));
end;

/// AText as a JSON string literal.
function JSONQuote(const AText: StringRAL): StringRAL;
var
  vString: TRALJSONString;
begin
  vString := TRALJSONString.Create(AText);
  try
    Result := vString.ToJSON;
  finally
    vString.Free;
  end;
end;

{ ERALJSONRPCError }

constructor ERALJSONRPCError.CreateRPC(ACode: IntegerRAL; const AMessage: string;
  const AData: StringRAL);
begin
  inherited Create(AMessage);
  FCode := ACode;
  FData := AData;
end;

{ TRALJSONRPCRequest }

constructor TRALJSONRPCRequest.Create(const AMethod: StringRAL; AParams, AId: TRALJSON;
  AContext: TObject);
begin
  inherited Create;
  FMethod := AMethod;
  FParams := AParams;
  FId := AId;
  FContext := AContext;
end;

function TRALJSONRPCRequest.FindParam(const AName: StringRAL): TRALJSON;
var
  vIndex: IntegerRAL;
begin
  Result := nil;
  if FParams = nil then
    Exit;
  if FParams.Kind = rjkObject then
    Result := TRALJSONObject(FParams).Find(AName)
  else if (FParams.Kind = rjkArray) and (FMethodItem <> nil) then
  begin
    vIndex := FMethodItem.Params.IndexOfName(AName);
    if vIndex >= 0 then
      Result := ParamAt(vIndex);
  end;
end;

function TRALJSONRPCRequest.IsNotification: Boolean;
begin
  Result := FId = nil;
end;

function TRALJSONRPCRequest.Param(const AName: StringRAL): TRALJSON;
begin
  Result := FindParam(AName);
  if Result = nil then
    raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInvalidParams,
      string(RALJSONRPCMessage(cRALJSONRPCInvalidParams)), JSONQuote(AName));
end;

function TRALJSONRPCRequest.ParamAt(AIndex: IntegerRAL): TRALJSON;
begin
  if (FParams <> nil) and (FParams.Kind = rjkArray) and (AIndex >= 0) and
    (AIndex < TRALJSONArray(FParams).Count) then
    Result := TRALJSONArray(FParams).Items[AIndex]
  else
    Result := nil;
end;

function TRALJSONRPCRequest.ParamCount: IntegerRAL;
begin
  if FParams = nil then
    Result := 0
  else if FParams.Kind = rjkArray then
    Result := TRALJSONArray(FParams).Count
  else
    Result := TRALJSONObject(FParams).Count;
end;

{ TRALJSONRPCResult }

destructor TRALJSONRPCResult.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

function TRALJSONRPCResult.Extract: TRALJSON;
begin
  Result := FValue;
  FValue := nil;
end;

procedure TRALJSONRPCResult.SetBoolean(AValue: Boolean);
begin
  SetValue(TRALJSONBoolean.Create(AValue));
end;

procedure TRALJSONRPCResult.SetFloat(AValue: Double);
var
  vArray: TRALJSONArray;
begin
  // The array formats the number the way the JSON writer does.
  vArray := TRALJSONArray.Create;
  try
    vArray.Add(AValue);
    SetValue(TRALJSON.ParseJSON(vArray.Items[0].ToJSON));
  finally
    vArray.Free;
  end;
end;

procedure TRALJSONRPCResult.SetInteger(AValue: Int64);
begin
  SetValue(TRALJSONNumber.Create(StringRAL(IntToStr(AValue))));
end;

procedure TRALJSONRPCResult.SetJSON(const AJSON: StringRAL);
begin
  SetValue(TRALJSON.ParseJSON(AJSON));
end;

procedure TRALJSONRPCResult.SetNull;
begin
  SetValue(nil);
end;

procedure TRALJSONRPCResult.SetString(const AValue: StringRAL);
begin
  SetValue(TRALJSONString.Create(AValue));
end;

procedure TRALJSONRPCResult.SetValue(AValue: TRALJSON);
begin
  if AValue = FValue then
    Exit;
  FValue.Free;
  FValue := AValue;
end;

{ TRALJSONRPCParam }

constructor TRALJSONRPCParam.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FRequired := True;
end;

procedure TRALJSONRPCParam.Assign(ASource: TPersistent);
begin
  if ASource is TRALJSONRPCParam then
  begin
    FName := TRALJSONRPCParam(ASource).Name;
    FParamType := TRALJSONRPCParam(ASource).ParamType;
    FRequired := TRALJSONRPCParam(ASource).Required;
  end
  else
    inherited Assign(ASource);
end;

function TRALJSONRPCParam.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := string(FName);
end;

{ TRALJSONRPCParams }

function TRALJSONRPCParams.AddParam(const AName: StringRAL; AType: TRALJSONRPCParamType;
  ARequired: Boolean): TRALJSONRPCParam;
begin
  Result := TRALJSONRPCParam(Add);
  Result.Name := AName;
  Result.ParamType := AType;
  Result.Required := ARequired;
end;

function TRALJSONRPCParams.GetItem(AIndex: IntegerRAL): TRALJSONRPCParam;
begin
  Result := TRALJSONRPCParam(inherited Items[AIndex]);
end;

function TRALJSONRPCParams.IndexOfName(const AName: StringRAL): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
    if Items[vInt].Name = AName then
    begin
      Result := vInt;
      Exit;
    end;
  Result := -1;
end;

{ TRALJSONRPCMethod }

constructor TRALJSONRPCMethod.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FParams := TRALJSONRPCParams.Create(Self, TRALJSONRPCParam);
end;

destructor TRALJSONRPCMethod.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

procedure TRALJSONRPCMethod.Assign(ASource: TPersistent);
begin
  if ASource is TRALJSONRPCMethod then
  begin
    FName := TRALJSONRPCMethod(ASource).Name;
    FDescription := TRALJSONRPCMethod(ASource).Description;
    FParams.Assign(TRALJSONRPCMethod(ASource).Params);
    FOnExecute := TRALJSONRPCMethod(ASource).OnExecute;
  end
  else
    inherited Assign(ASource);
end;

procedure TRALJSONRPCMethod.CheckParams(ARequest: TRALJSONRPCRequest);
var
  vInt: IntegerRAL;
  vDecl: TRALJSONRPCParam;
  vValue: TRALJSON;
  vFits: Boolean;

  procedure Refuse(const AName: StringRAL);
  begin
    raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInvalidParams,
      string(RALJSONRPCMessage(cRALJSONRPCInvalidParams)), JSONQuote(AName));
  end;

begin
  if FParams.Count = 0 then
    Exit;
  if (ARequest.Params <> nil) and (ARequest.Params.Kind = rjkArray) and
    (TRALJSONArray(ARequest.Params).Count > FParams.Count) then
    Refuse('');
  if (ARequest.Params <> nil) and (ARequest.Params.Kind = rjkObject) then
    for vInt := 0 to TRALJSONObject(ARequest.Params).Count - 1 do
      if FParams.IndexOfName(TRALJSONObject(ARequest.Params).Names[vInt]) < 0 then
        Refuse(TRALJSONObject(ARequest.Params).Names[vInt]);
  for vInt := 0 to FParams.Count - 1 do
  begin
    vDecl := FParams[vInt];
    vValue := ARequest.FindParam(vDecl.Name);
    if vValue = nil then
    begin
      if vDecl.Required then
        Refuse(vDecl.Name);
      Continue;
    end;
    case vDecl.ParamType of
      jptString: vFits := vValue.Kind = rjkString;
      jptNumber: vFits := vValue.Kind = rjkNumber;
      jptInteger: vFits := (vValue.Kind = rjkNumber) and
        (Pos('.', string(TRALJSONNumber(vValue).Value)) = 0) and
        (Pos('e', LowerCase(string(TRALJSONNumber(vValue).Value))) = 0);
      jptBoolean: vFits := vValue.Kind = rjkBoolean;
      jptObject: vFits := vValue.Kind = rjkObject;
      jptArray: vFits := vValue.Kind = rjkArray;
    else
      vFits := True;
    end;
    if not vFits then
      Refuse(vDecl.Name);
  end;
end;

function TRALJSONRPCMethod.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := string(FName);
end;

procedure TRALJSONRPCMethod.SetParams(AValue: TRALJSONRPCParams);
begin
  FParams.Assign(AValue);
end;

{ TRALJSONRPCMethods }

function TRALJSONRPCMethods.AddMethod(const AName: StringRAL;
  AEvent: TRALJSONRPCExecuteEvent; const ADescription: StringRAL): TRALJSONRPCMethod;
begin
  Result := TRALJSONRPCMethod(Add);
  Result.Name := AName;
  Result.Description := ADescription;
  Result.OnExecute := AEvent;
end;

function TRALJSONRPCMethods.Find(const AName: StringRAL): TRALJSONRPCMethod;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
    if Items[vInt].Name = AName then
    begin
      Result := Items[vInt];
      Exit;
    end;
  Result := nil;
end;

function TRALJSONRPCMethods.GetItem(AIndex: IntegerRAL): TRALJSONRPCMethod;
begin
  Result := TRALJSONRPCMethod(inherited Items[AIndex]);
end;

{ TRALJSONRPCDispatcher }

constructor TRALJSONRPCDispatcher.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FMethods := TRALJSONRPCMethods.Create(Self, TRALJSONRPCMethod);
end;

destructor TRALJSONRPCDispatcher.Destroy;
begin
  FMethods.Free;
  inherited Destroy;
end;

procedure TRALJSONRPCDispatcher.Assign(ASource: TPersistent);
begin
  if ASource is TRALJSONRPCDispatcher then
  begin
    FErrorDetails := TRALJSONRPCDispatcher(ASource).ErrorDetails;
    FMaxBatch := TRALJSONRPCDispatcher(ASource).MaxBatch;
    FMethods.Assign(TRALJSONRPCDispatcher(ASource).Methods);
    FOnUnknownMethod := TRALJSONRPCDispatcher(ASource).OnUnknownMethod;
  end
  else
    inherited Assign(ASource);
end;

function TRALJSONRPCDispatcher.Execute(const ARequest: StringRAL; AContext: TObject): StringRAL;
var
  vRequest, vAnswer: TRALJSON;
begin
  try
    vRequest := TRALJSON.ParseJSON(ARequest);
  except
    vAnswer := RALJSONRPCErrorResponse(nil, cRALJSONRPCParseError,
      RALJSONRPCMessage(cRALJSONRPCParseError));
    try
      Result := vAnswer.ToJSON;
    finally
      vAnswer.Free;
    end;
    Exit;
  end;
  try
    vAnswer := ExecuteJSON(vRequest, AContext);
    try
      if vAnswer = nil then
        Result := ''
      else
        Result := vAnswer.ToJSON;
    finally
      vAnswer.Free;
    end;
  finally
    vRequest.Free;
  end;
end;

function TRALJSONRPCDispatcher.ExecuteCall(ACall: TRALJSON; AContext: TObject): TRALJSON;
var
  vObject: TRALJSONObject;
  vVersion, vMethod, vParams, vId: TRALJSON;
  vRequest: TRALJSONRPCRequest;
  vResult: TRALJSONRPCResult;
  vItem: TRALJSONRPCMethod;
  vHandled: Boolean;
  vAnswer: TRALJSONObject;
  vData: StringRAL;
begin
  Result := nil;
  // JSON-RPC 2.0 section 4: an object with jsonrpc "2.0", a string method,
  // structured params when present, and a string, number or null id.
  if (ACall = nil) or (ACall.Kind <> rjkObject) then
  begin
    Result := RALJSONRPCErrorResponse(nil, cRALJSONRPCInvalidRequest,
      RALJSONRPCMessage(cRALJSONRPCInvalidRequest));
    Exit;
  end;
  vObject := TRALJSONObject(ACall);
  vVersion := vObject.Find('jsonrpc');
  vMethod := vObject.Find('method');
  vParams := vObject.Find('params');
  vId := vObject.Find('id');
  if (vId <> nil) and not (vId.Kind in [rjkString, rjkNumber, rjkNull]) then
    vId := nil;
  if (vVersion = nil) or (vVersion.Kind <> rjkString) or
    (TRALJSONString(vVersion).Value <> cRALJSONRPCVersion) or (vMethod = nil) or
    (vMethod.Kind <> rjkString) or ((vParams <> nil) and
    not (vParams.Kind in [rjkObject, rjkArray])) or
    ((vObject.Find('id') <> nil) and (vId = nil)) then
  begin
    Result := RALJSONRPCErrorResponse(vId, cRALJSONRPCInvalidRequest,
      RALJSONRPCMessage(cRALJSONRPCInvalidRequest));
    Exit;
  end;

  vRequest := TRALJSONRPCRequest.Create(TRALJSONString(vMethod).Value, vParams, vId, AContext);
  vResult := TRALJSONRPCResult.Create;
  try
    try
      vItem := FMethods.Find(vRequest.Method);
      vRequest.MethodItem := vItem;
      if vItem <> nil then
      begin
        vItem.CheckParams(vRequest);
        if Assigned(vItem.OnExecute) then
          vItem.OnExecute(Self, vRequest, vResult);
      end
      else
      begin
        vHandled := False;
        if Assigned(FOnUnknownMethod) then
          FOnUnknownMethod(Self, vRequest, vResult, vHandled);
        if not vHandled then
          raise ERALJSONRPCError.CreateRPC(cRALJSONRPCMethodNotFound,
            string(RALJSONRPCMessage(cRALJSONRPCMethodNotFound)));
      end;
      if vRequest.IsNotification then
        Exit;
      vAnswer := TRALJSONObject.Create;
      vAnswer.Add('jsonrpc', cRALJSONRPCVersion);
      if vResult.Value = nil then
        vAnswer.Add('result', TRALJSONNull.Create)
      else
        vAnswer.Add('result', vResult.Extract);
      vAnswer.Add('id', RALJSONClone(vId));
      Result := vAnswer;
    except
      on E: ERALJSONRPCError do
        if not vRequest.IsNotification then
          Result := RALJSONRPCErrorResponse(vId, E.Code, StringRAL(UTF8Encode(E.Message)), E.Data);
      on E: Exception do
        if not vRequest.IsNotification then
        begin
          vData := '';
          if FErrorDetails then
            vData := JSONQuote(StringRAL(UTF8Encode(E.Message)));
          Result := RALJSONRPCErrorResponse(vId, cRALJSONRPCInternalError,
            RALJSONRPCMessage(cRALJSONRPCInternalError), vData);
        end;
    end;
  finally
    vResult.Free;
    vRequest.Free;
  end;
end;

function TRALJSONRPCDispatcher.ExecuteJSON(ARequest: TRALJSON; AContext: TObject): TRALJSON;
var
  vBatch: TRALJSONArray;
  vAnswers: TRALJSONArray;
  vAnswer: TRALJSON;
  vInt: IntegerRAL;
begin
  if (ARequest = nil) or (ARequest.Kind <> rjkArray) then
  begin
    Result := ExecuteCall(ARequest, AContext);
    Exit;
  end;
  // A batch (section 6): an empty one is one Invalid Request; the answers come
  // in an array, and a batch of notifications answers nothing.
  vBatch := TRALJSONArray(ARequest);
  if (vBatch.Count = 0) or ((FMaxBatch > 0) and (vBatch.Count > FMaxBatch)) then
  begin
    Result := RALJSONRPCErrorResponse(nil, cRALJSONRPCInvalidRequest,
      RALJSONRPCMessage(cRALJSONRPCInvalidRequest));
    Exit;
  end;
  vAnswers := TRALJSONArray.Create;
  try
    for vInt := 0 to vBatch.Count - 1 do
    begin
      vAnswer := ExecuteCall(vBatch.Items[vInt], AContext);
      if vAnswer <> nil then
        vAnswers.Add(vAnswer);
    end;
  except
    vAnswers.Free;
    raise;
  end;
  if vAnswers.Count = 0 then
  begin
    vAnswers.Free;
    Result := nil;
  end
  else
    Result := vAnswers;
end;

function TRALJSONRPCDispatcher.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TRALJSONRPCDispatcher.SetMethods(AValue: TRALJSONRPCMethods);
begin
  FMethods.Assign(AValue);
end;

end.
