/// A module of any RAL HTTP server that reaches a TRALMQTTBroker: publish by
/// POST, read retained messages, list clients and counters. The server's
/// plugins and authentication apply to these routes as to any other, and the
/// topic goes through the broker's Permissions with the HTTP user.
unit RALMQTTRestModule;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALMIMETypes, RALServer, RALRoutes, RALRequest, RALResponse,
  RALParams, RALJson, RALMQTTTypes, RALMQTTTopic, RALMQTTBroker;

type
  { Routes, under Domain (default '/mqtt'):
    - POST publish/<topic>: the body is the content; qos (0-2) and retain
      (true/1) from the query or the headers X-MQTT-QoS and X-MQTT-Retain;
    - GET retained/<topic>: the retained message (its content type when
      the publisher gave one), 404 when there is none;
    - GET clients: the identifiers of the connected clients, as JSON;
    - GET stats: the broker's counters, as JSON.
    A topic of several levels is the rest of the path, percent-decoded }
  TRALMQTTRestModule = class(TRALModuleRoutes)
  private
    FBroker: TRALMQTTBroker;
    FClientsRoute: TRALRoute;
    FInternal: TRALRoutes;
    FPublishRoute: TRALRoute;
    FRetainedRoute: TRALRoute;
    FStatsRoute: TRALRoute;

    procedure SetBroker(AValue: TRALMQTTBroker);
    function TopicOf(ARequest: TRALRequest; const APrefix: StringRAL): StringRAL;
    function UserOf(ARequest: TRALRequest): StringRAL;
    function ParamText(ARequest: TRALRequest; const AName, AHeader: StringRAL): StringRAL;
    function Prefix(const AName: StringRAL): StringRAL;
    procedure Fail(AResponse: TRALResponse; AStatus: IntegerRAL; const AText: StringRAL);

    procedure DoPublish(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure DoRetained(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure DoClients(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure DoStats(ARequest: TRALRequest; AResponse: TRALResponse);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// The fixed routes, then publish/<topic> and retained/<topic> by prefix.
    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
      override;
    /// The module's four routes (for Swagger and Postman); the caller frees the list.
    function GetListRoutes: TList; override;
  published
    /// The broker the routes reach.
    property Broker: TRALMQTTBroker read FBroker write SetBroker;
  end;

implementation

uses
  RALTools, RALUrlCoder;

const
  cPublish = '/publish/';
  cRetained = '/retained/';

{ TRALMQTTRestModule }

constructor TRALMQTTRestModule.Create(AOwner: TComponent);

  function Add(const ARoute, AName, ADescription: StringRAL; AMethods: TRALMethods;
    AReply: TRALOnReply): TRALRoute;
  begin
    Result := TRALRoute(FInternal.Add);
    Result.Route := ARoute;
    Result.Name := AName;
    Result.Description.Text := string(ADescription);
    Result.AllowedMethods := AMethods;
    Result.OnReply := AReply;
  end;

begin
  inherited Create(AOwner);
  Domain := '/mqtt';
  // owned by the module, so their full path carries its Domain
  FInternal := TRALRoutes.Create(Self);
  FPublishRoute := Add('/publish', 'mqttpublish', 'Publishes the body to the topic after publish/',
    [amPOST], {$IFDEF FPC}@{$ENDIF}DoPublish);
  FRetainedRoute := Add('/retained', 'mqttretained', 'The retained message of the topic after retained/',
    [amGET, amHEAD], {$IFDEF FPC}@{$ENDIF}DoRetained);
  FClientsRoute := Add('/clients', 'mqttclients', 'Identifiers of the connected clients',
    [amGET, amHEAD], {$IFDEF FPC}@{$ENDIF}DoClients);
  FStatsRoute := Add('/stats', 'mqttstats', 'Counters of the broker',
    [amGET, amHEAD], {$IFDEF FPC}@{$ENDIF}DoStats);
end;

destructor TRALMQTTRestModule.Destroy;
begin
  SetBroker(nil);
  FreeAndNil(FInternal);
  inherited Destroy;
end;

procedure TRALMQTTRestModule.SetBroker(AValue: TRALMQTTBroker);
begin
  if FBroker = AValue then
    Exit;
  if FBroker <> nil then
    FBroker.RemoveFreeNotification(Self);
  FBroker := AValue;
  if FBroker <> nil then
    FBroker.FreeNotification(Self);
end;

procedure TRALMQTTRestModule.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FBroker) then
    FBroker := nil;
end;

function TRALMQTTRestModule.Prefix(const AName: StringRAL): StringRAL;
begin
  Result := Domain;
  if (Result <> '') and (Result[Length(Result)] = '/') then
    Delete(Result, Length(Result), 1);
  Result := Result + AName;
end;

function TRALMQTTRestModule.CanAnswerRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
var
  vPath: StringRAL;
begin
  Result := inherited CanAnswerRoute(ARequest, AResponse);
  if Result <> nil then
    Exit;
  vPath := ARequest.Query;
  if RALPosText(Prefix(cPublish), vPath) = 1 then
    Result := FPublishRoute
  else if RALPosText(Prefix(cRetained), vPath) = 1 then
    Result := FRetainedRoute
  else if RALSameName(vPath, Prefix('/clients')) then
    Result := FClientsRoute
  else if RALSameName(vPath, Prefix('/stats')) then
    Result := FStatsRoute;
  if (Result <> nil) and Assigned(OnBeforeAnswer) then
    OnBeforeAnswer(ARequest, AResponse);
end;

function TRALMQTTRestModule.GetListRoutes: TList;
var
  vInt: IntegerRAL;
begin
  Result := inherited GetListRoutes;
  for vInt := 0 to FInternal.Count - 1 do
    Result.Add(FInternal.Items[vInt]);
end;

function TRALMQTTRestModule.TopicOf(ARequest: TRALRequest; const APrefix: StringRAL): StringRAL;
var
  vPath: StringRAL;
begin
  vPath := ARequest.Query;
  Result := TRALHTTPCoder.DecodeURL(Copy(vPath, Length(Prefix(APrefix)) + 1, MaxInt));
end;

function TRALMQTTRestModule.UserOf(ARequest: TRALRequest): StringRAL;
begin
  // the user of the HTTP request: Basic's name or the token's subject
  Result := '';
  if ARequest.Authorization = nil then
    Exit;
  if ARequest.Authorization.AsAuthBasic <> nil then
    Result := ARequest.Authorization.AsAuthBasic.UserName
  else if ARequest.Authorization.AsAuthBearer <> nil then
    Result := ARequest.Authorization.AsAuthBearer.Payload.Subject;
end;

function TRALMQTTRestModule.ParamText(ARequest: TRALRequest;
  const AName, AHeader: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  vParam := ARequest.ParamByName(AName);
  if vParam = nil then
    vParam := ARequest.ParamByName(AHeader);
  if vParam = nil then
    Result := ''
  else
    Result := vParam.AsString;
end;

procedure TRALMQTTRestModule.Fail(AResponse: TRALResponse; AStatus: IntegerRAL;
  const AText: StringRAL);
begin
  AResponse.Answer(AStatus, AText, rctTEXTPLAIN);
end;

procedure TRALMQTTRestModule.DoPublish(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vTopic, vValue: StringRAL;
  vQoS: IntegerRAL;
  vRetain: boolean;
  vPayload: TBytes;
  vStream: TStream;
begin
  if FBroker = nil then
  begin
    Fail(AResponse, HTTP_ServiceUnavailable, StringRAL(emMQTTRestNoBroker));
    Exit;
  end;
  vTopic := TopicOf(ARequest, cPublish);
  if not RALMQTTValidTopicName(vTopic) then
  begin
    Fail(AResponse, HTTP_BadRequest, StringRAL(Format(emMQTTInvalidTopic, [string(vTopic)])));
    Exit;
  end;
  if not FBroker.CheckPermission(UserOf(ARequest), '', vTopic, maWrite) then
  begin
    Fail(AResponse, HTTP_Forbidden, StringRAL(emMQTTRestForbidden));
    Exit;
  end;
  vValue := ParamText(ARequest, 'qos', 'X-MQTT-QoS');
  vQoS := StrToIntDef(string(vValue), 0);
  if (vQoS < 0) or (vQoS > 2) then
  begin
    Fail(AResponse, HTTP_BadRequest, StringRAL(emMQTTRestQoS));
    Exit;
  end;
  vValue := ParamText(ARequest, 'retain', 'X-MQTT-Retain');
  vRetain := RALSameName(vValue, 'true') or (vValue = '1');
  vPayload := nil;
  vStream := ARequest.RequestStream;
  if (vStream <> nil) and (vStream.Size > 0) then
  begin
    SetLength(vPayload, vStream.Size);
    vStream.Position := 0;
    vStream.ReadBuffer(vPayload[0], Length(vPayload));
  end;
  FBroker.Publish(vTopic, vPayload, TRALMQTTQoS(vQoS), vRetain);
  AResponse.Answer(HTTP_NoContent);
end;

procedure TRALMQTTRestModule.DoRetained(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vTopic, vType: StringRAL;
  vMessage: TRALMQTTBrokerMessage;
  vStream: TMemoryStream;
begin
  if FBroker = nil then
  begin
    Fail(AResponse, HTTP_ServiceUnavailable, StringRAL(emMQTTRestNoBroker));
    Exit;
  end;
  vTopic := TopicOf(ARequest, cRetained);
  if not RALMQTTValidTopicName(vTopic) then
  begin
    Fail(AResponse, HTTP_BadRequest, StringRAL(Format(emMQTTInvalidTopic, [string(vTopic)])));
    Exit;
  end;
  if not FBroker.CheckPermission(UserOf(ARequest), '', vTopic, maRead) then
  begin
    Fail(AResponse, HTTP_Forbidden, StringRAL(emMQTTRestForbidden));
    Exit;
  end;
  vMessage := FBroker.RetainedMessage(vTopic);
  if vMessage = nil then
  begin
    AResponse.Answer(HTTP_NotFound);
    Exit;
  end;
  try
    vType := '';
    if vMessage.Properties <> nil then
      vType := vMessage.Properties.ContentType;
    if vType = '' then
      vType := rctAPPLICATIONOCTETSTREAM;
    vStream := TMemoryStream.Create;
    if vMessage.Payload <> nil then
      vStream.WriteBuffer(vMessage.Payload[0], Length(vMessage.Payload));
    vStream.Position := 0;
    AResponse.Answer(HTTP_OK, vStream, vType, True);
  finally
    vMessage.Release;
  end;
end;

procedure TRALMQTTRestModule.DoClients(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vList: TStringList;
  vArray: TRALJSONArray;
  vInt: IntegerRAL;
begin
  if FBroker = nil then
  begin
    Fail(AResponse, HTTP_ServiceUnavailable, StringRAL(emMQTTRestNoBroker));
    Exit;
  end;
  vList := TStringList.Create;
  vArray := TRALJSONArray.Create;
  try
    FBroker.Clients(vList);
    for vInt := 0 to vList.Count - 1 do
      vArray.Add(StringRAL(vList[vInt]));
    AResponse.Answer(HTTP_OK, vArray.ToJson, rctAPPLICATIONJSON);
  finally
    vArray.Free;
    vList.Free;
  end;
end;

procedure TRALMQTTRestModule.DoStats(ARequest: TRALRequest; AResponse: TRALResponse);
const
  cNames: array[0..6] of StringRAL = ('messagesReceived', 'messagesSent', 'bytesReceived',
    'bytesSent', 'connectionsAccepted', 'connectionsRefused', 'messagesDropped');
var
  vObject: TRALJSONObject;
  vInt: IntegerRAL;
begin
  if FBroker = nil then
  begin
    Fail(AResponse, HTTP_ServiceUnavailable, StringRAL(emMQTTRestNoBroker));
    Exit;
  end;
  vObject := TRALJSONObject.Create;
  try
    vObject.Add('connections', Int64RAL(FBroker.ConnectionCount));
    vObject.Add('sessions', Int64RAL(FBroker.SessionCount));
    for vInt := Low(cNames) to High(cNames) do
      vObject.Add(cNames[vInt], FBroker.Statistic(vInt));
    AResponse.Answer(HTTP_OK, vObject.ToJson, rctAPPLICATIONJSON);
  finally
    vObject.Free;
  end;
end;

end.
