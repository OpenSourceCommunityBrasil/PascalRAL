/// The JSON-RPC 2.0 client: calls, notifications and batches over a TRALClient.
unit RALJSONRPCClient;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALMIMETypes, RALTools, RALCustomObjects, RALClient, RALRequest,
  RALResponse, RALJson, RALJSONRPC;

type
  TRALJSONRPCClient = class;

  { Calls and notifications sent together in one request; the answers come by
    the position Call returned. }
  TRALJSONRPCBatch = class
  private
    /// The answers by position; nil for notifications.
    FAnswers: TList;
    /// The calls to send.
    FCalls: TRALJSONArray;
    /// The client that sends the batch.
    FClient: TRALJSONRPCClient;
    /// For each position, the id sent; -1 for a notification.
    FIds: array of IntegerRAL;

    /// Frees the answers of a previous Send.
    procedure ClearAnswers;
    /// The answer at AIndex; raises when there is none.
    function GetAnswer(AIndex: IntegerRAL): TRALJSONObject;
  public
    /// A batch sent through AClient.
    constructor Create(AClient: TRALJSONRPCClient);
    destructor Destroy; override;

    /// Adds a call of AMethod with a copy of AParams; returns its position.
    function Call(const AMethod: StringRAL; AParams: TRALJSON = nil): IntegerRAL;
    /// How many calls and notifications were added.
    function Count: IntegerRAL;
    /// The error code of the answer at AIndex; 0 when it succeeded.
    function ErrorCode(AIndex: IntegerRAL): IntegerRAL;
    /// The error message of the answer at AIndex; empty when it succeeded.
    function ErrorMessage(AIndex: IntegerRAL): StringRAL;
    /// Adds a notification of AMethod with a copy of AParams.
    procedure Notify(const AMethod: StringRAL; AParams: TRALJSON = nil);
    /// Sends the batch and keeps the answers; raises on a transport or HTTP failure.
    procedure Send;
    { The result at AIndex, owned by the batch (nil for null); raises
      ERALJSONRPCError when that call failed. }
    function Value(AIndex: IntegerRAL): TRALJSON;
  end;

  { Calls a JSON-RPC 2.0 server through Client (its engine, BaseURL,
    authentication) at Path. Safe from several threads, as TRALClient is. }
  TRALJSONRPCClient = class(TRALComponent)
  private
    FClient: TRALClient;
    /// The last id sent.
    FLastId: IntegerRAL;
    FPath: StringRAL;

    procedure SetClient(AValue: TRALClient);
  protected
    /// A new call object of AMethod with a copy of AParams and the id AId (-1: none).
    function NewCall(const AMethod: StringRAL; AParams: TRALJSON; AId: IntegerRAL): TRALJSONObject;
    /// The next id.
    function NextId: IntegerRAL;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;

    { Calls AMethod with AParams (an object or an array, not owned) and returns
      the result, which the caller frees (nil for null); raises
      ERALJSONRPCError when the server answers an error. }
    function Call(const AMethod: StringRAL; AParams: TRALJSON = nil): TRALJSON;
    /// Call with params and result as JSON text.
    function CallText(const AMethod: StringRAL; const AParams: StringRAL = ''): StringRAL;
    /// Sends ABody and returns the answer text; '' for 204. Raises on other statuses.
    function Exchange(const ABody: StringRAL): StringRAL;
    /// A new batch through this client; the caller frees it.
    function NewBatch: TRALJSONRPCBatch;
    /// Sends a notification of AMethod with AParams (not owned); nothing comes back.
    procedure Notify(const AMethod: StringRAL; AParams: TRALJSON = nil);
  published
    /// The client that carries the calls.
    property Client: TRALClient read FClient write SetClient;
    /// The route of the server's JSON-RPC module.
    property Path: StringRAL read FPath write FPath;
  end;

implementation

const
  /// TRALJSONRPCClient.Path when nobody says otherwise.
  cDefaultPath = '/rpc';

/// Raises the error object of AAnswer when it has one.
procedure RaiseIfError(AAnswer: TRALJSONObject);
var
  vError, vCode, vMessage, vData: TRALJSON;
  vDataText: StringRAL;
begin
  vError := AAnswer.Find('error');
  if (vError = nil) or (vError.Kind <> rjkObject) then
    Exit;
  vCode := TRALJSONObject(vError).Find('code');
  vMessage := TRALJSONObject(vError).Find('message');
  vData := TRALJSONObject(vError).Find('data');
  vDataText := '';
  if vData <> nil then
    vDataText := vData.ToJSON;
  if vCode = nil then
    raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInternalError, '', vDataText);
  if vMessage = nil then
    raise ERALJSONRPCError.CreateRPC(IntegerRAL(vCode.AsInt64), '', vDataText);
  raise ERALJSONRPCError.CreateRPC(IntegerRAL(vCode.AsInt64),
    {$IFDEF FPC}vMessage.AsString{$ELSE}UTF8ToString(vMessage.AsString){$ENDIF}, vDataText);
end;

{ TRALJSONRPCBatch }

constructor TRALJSONRPCBatch.Create(AClient: TRALJSONRPCClient);
begin
  inherited Create;
  FClient := AClient;
  FCalls := TRALJSONArray.Create;
  FAnswers := TList.Create;
end;

destructor TRALJSONRPCBatch.Destroy;
begin
  ClearAnswers;
  FAnswers.Free;
  FCalls.Free;
  inherited Destroy;
end;

function TRALJSONRPCBatch.Call(const AMethod: StringRAL; AParams: TRALJSON): IntegerRAL;
var
  vId: IntegerRAL;
begin
  vId := FClient.NextId;
  FCalls.Add(FClient.NewCall(AMethod, AParams, vId));
  Result := Length(FIds);
  SetLength(FIds, Result + 1);
  FIds[Result] := vId;
end;

procedure TRALJSONRPCBatch.ClearAnswers;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FAnswers.Count - 1 do
    TObject(FAnswers[vInt]).Free;
  FAnswers.Clear;
end;

function TRALJSONRPCBatch.Count: IntegerRAL;
begin
  Result := Length(FIds);
end;

function TRALJSONRPCBatch.ErrorCode(AIndex: IntegerRAL): IntegerRAL;
var
  vError: TRALJSON;
begin
  vError := GetAnswer(AIndex).Find('error');
  if (vError = nil) or (vError.Kind <> rjkObject) or
    (TRALJSONObject(vError).Find('code') = nil) then
    Result := 0
  else
    Result := IntegerRAL(TRALJSONObject(vError).Find('code').AsInt64);
end;

function TRALJSONRPCBatch.ErrorMessage(AIndex: IntegerRAL): StringRAL;
var
  vError: TRALJSON;
begin
  vError := GetAnswer(AIndex).Find('error');
  if (vError = nil) or (vError.Kind <> rjkObject) or
    (TRALJSONObject(vError).Find('message') = nil) then
    Result := ''
  else
    Result := TRALJSONObject(vError).Find('message').AsString;
end;

function TRALJSONRPCBatch.GetAnswer(AIndex: IntegerRAL): TRALJSONObject;
begin
  if (AIndex < 0) or (AIndex >= FAnswers.Count) or (FAnswers[AIndex] = nil) then
    raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInternalError, 'No answer');
  Result := TRALJSONObject(FAnswers[AIndex]);
end;

procedure TRALJSONRPCBatch.Notify(const AMethod: StringRAL; AParams: TRALJSON);
var
  vPos: IntegerRAL;
begin
  FCalls.Add(FClient.NewCall(AMethod, AParams, -1));
  vPos := Length(FIds);
  SetLength(FIds, vPos + 1);
  FIds[vPos] := -1;
end;

procedure TRALJSONRPCBatch.Send;
var
  vText: StringRAL;
  vAnswer, vItem, vId: TRALJSON;
  vInt, vPos, vCall: IntegerRAL;
begin
  ClearAnswers;
  for vInt := 0 to High(FIds) do
    FAnswers.Add(nil);
  vText := FClient.Exchange(FCalls.ToJSON);
  if vText = '' then
    Exit;
  vAnswer := TRALJSON.ParseJSON(vText);
  try
    if vAnswer.Kind = rjkObject then
      RaiseIfError(TRALJSONObject(vAnswer));
    if vAnswer.Kind <> rjkArray then
      Exit;
    // Answers may come in any order: they are matched by id.
    for vInt := 0 to TRALJSONArray(vAnswer).Count - 1 do
    begin
      vItem := TRALJSONArray(vAnswer).Items[vInt];
      if vItem.Kind <> rjkObject then
        Continue;
      vId := TRALJSONObject(vItem).Find('id');
      vPos := -1;
      if (vId <> nil) and (vId.Kind = rjkNumber) then
        for vCall := 0 to High(FIds) do
          if FIds[vCall] = vId.AsInt64 then
          begin
            vPos := vCall;
            Break;
          end;
      if (vPos >= 0) and (FAnswers[vPos] = nil) then
        FAnswers[vPos] := RALJSONClone(vItem);
    end;
  finally
    vAnswer.Free;
  end;
end;

function TRALJSONRPCBatch.Value(AIndex: IntegerRAL): TRALJSON;
var
  vAnswer: TRALJSONObject;
begin
  vAnswer := GetAnswer(AIndex);
  RaiseIfError(vAnswer);
  Result := vAnswer.Find('result');
  if (Result <> nil) and Result.IsNull then
    Result := nil;
end;

{ TRALJSONRPCClient }

constructor TRALJSONRPCClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPath := cDefaultPath;
end;

function TRALJSONRPCClient.Call(const AMethod: StringRAL; AParams: TRALJSON): TRALJSON;
var
  vCall: TRALJSONObject;
  vText: StringRAL;
  vAnswer: TRALJSON;
begin
  vCall := NewCall(AMethod, AParams, NextId);
  try
    vText := Exchange(vCall.ToJSON);
  finally
    vCall.Free;
  end;
  if vText = '' then
    raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInternalError, 'No answer');
  vAnswer := TRALJSON.ParseJSON(vText);
  try
    if vAnswer.Kind <> rjkObject then
      raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInvalidRequest,
        string(RALJSONRPCMessage(cRALJSONRPCInvalidRequest)));
    RaiseIfError(TRALJSONObject(vAnswer));
    Result := TRALJSONObject(vAnswer).Extract('result');
    if (Result <> nil) and Result.IsNull then
      FreeAndNil(Result);
  finally
    vAnswer.Free;
  end;
end;

function TRALJSONRPCClient.CallText(const AMethod, AParams: StringRAL): StringRAL;
var
  vParams, vResult: TRALJSON;
begin
  vParams := nil;
  if AParams <> '' then
    vParams := TRALJSON.ParseJSON(AParams);
  try
    vResult := Call(AMethod, vParams);
    try
      if vResult = nil then
        Result := 'null'
      else
        Result := vResult.ToJSON;
    finally
      vResult.Free;
    end;
  finally
    vParams.Free;
  end;
end;

function TRALJSONRPCClient.Exchange(const ABody: StringRAL): StringRAL;
var
  vResponse: TRALResponse;
  vStatus: IntegerRAL;
begin
  if FClient = nil then
    raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInternalError, 'TRALJSONRPCClient.Client is not set');
  FClient.Request.Clear;
  FClient.Request.AddBody(ABody, rctAPPLICATIONJSON);
  vResponse := nil;
  FClient.Post(FPath, vResponse);
  try
    vStatus := vResponse.StatusCode;
    if vStatus = HTTP_NoContent then
      Result := ''
    else if vStatus = HTTP_OK then
      Result := vResponse.ResponseText
    else
      raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInternalError,
        Format('HTTP %d', [vStatus]), StringRAL(IntToStr(vStatus)));
  finally
    vResponse.Free;
  end;
end;

function TRALJSONRPCClient.NewBatch: TRALJSONRPCBatch;
begin
  Result := TRALJSONRPCBatch.Create(Self);
end;

function TRALJSONRPCClient.NewCall(const AMethod: StringRAL; AParams: TRALJSON;
  AId: IntegerRAL): TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('jsonrpc', cRALJSONRPCVersion);
  Result.Add('method', AMethod);
  if AParams <> nil then
    Result.Add('params', RALJSONClone(AParams));
  if AId >= 0 then
    Result.Add('id', Int64(AId));
end;

function TRALJSONRPCClient.NextId: IntegerRAL;
begin
  Result := RALAtomicInc(FLastId);
end;

procedure TRALJSONRPCClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
end;

procedure TRALJSONRPCClient.Notify(const AMethod: StringRAL; AParams: TRALJSON);
var
  vCall: TRALJSONObject;
begin
  vCall := NewCall(AMethod, AParams, -1);
  try
    Exchange(vCall.ToJSON);
  finally
    vCall.Free;
  end;
end;

procedure TRALJSONRPCClient.SetClient(AValue: TRALClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

end.
