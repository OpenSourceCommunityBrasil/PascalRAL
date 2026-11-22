{ A module of any RAL HTTP server that sends e-mail: POST <Domain>/send with a
  JSON object or a form (multipart or urlencoded) builds a TRALMailMessage and
  sends it through a TRALSMTPClient, or hands it to a TRALMailQueue. The
  server's plugins and authentication apply to the route as to any other. }
unit RALMailRestModule;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALMIMETypes, RALServer, RALRoutes, RALRequest, RALResponse,
  RALParams, RALJson, RALMailMIME, RALSMTPClient, RALMailQueue;

const
  /// Default of TRALMailRestModule.MaxRecipients.
  RALMailRestMaxRecipients = 50;

type
  { Fires before a message is sent or queued: AMessage is what the request
    built and may be changed; setting AStatus to 400 or more refuses the request
    with AError. }
  TRALMailRestSendEvent = procedure(Sender: TObject; ARequest: TRALRequest;
    AMessage: TRALMailMessage; var AStatus: IntegerRAL; var AError: StringRAL) of object;

  { Route POST <Domain>/send (Domain default '/mail') taking a JSON object or form
    fields (from, to, cc, bcc, replyTo, subject, text, html, attachments...). Answers
    200 when sent with Client, 202 with the job id when queued. }
  TRALMailRestModule = class(TRALModuleRoutes)
  private
    FAllowedFrom: TStrings;
    FClient: TRALSMTPClient;
    FDefaultFrom: StringRAL;
    /// Changes when Client does, so copies of the old settings are dropped.
    FGeneration: IntegerRAL;
    /// Copies of Client not in use, kept with their connection.
    FIdle: TList;
    /// Owner of the route.
    FInternal: TRALRoutes;
    /// Guards FIdle and FGeneration.
    FLock: TCriticalSection;
    FMaxRecipients: IntegerRAL;
    FOnBeforeSend: TRALMailRestSendEvent;
    FQueue: TRALMailQueue;
    /// The route that sends.
    FSendRoute: TRALRoute;

    /// Whether AAddress may be the author, by DefaultFrom and AllowedFrom.
    function AllowedSender(const AAddress: StringRAL): Boolean;
    /// Fills AMessage from the form fields and files of ARequest.
    function BuildFromForm(ARequest: TRALRequest; AMessage: TRALMailMessage;
      out AError: StringRAL): Boolean;
    /// Fills AMessage from a JSON object; False with AError when it is malformed.
    function BuildFromJSON(AObject: TRALJSONObject; AMessage: TRALMailMessage;
      out AError: StringRAL): Boolean;
    /// Answers the POST.
    procedure DoSend(ARequest: TRALRequest; AResponse: TRALResponse);
    /// Answers AStatus with a JSON object {"error": AText}.
    procedure Fail(AResponse: TRALResponse; AStatus: IntegerRAL; const AText: StringRAL);
    /// Frees the idle copies of Client.
    procedure FreeIdle;
    /// Gives back a copy taken by TakeClient.
    procedure GiveClient(AClient: TRALSMTPClient; AGeneration: IntegerRAL);
    /// Domain followed by AName.
    function Prefix(const AName: StringRAL): StringRAL;
    /// Sends AMessage with a copy of Client; answers the result.
    procedure SendNow(AMessage: TRALMailMessage; AResponse: TRALResponse);
    procedure SetAllowedFrom(AValue: TStrings);
    procedure SetClient(AValue: TRALSMTPClient);
    procedure SetQueue(AValue: TRALMailQueue);
    /// An idle copy of Client, or a new one; AGeneration says which settings it has.
    function TakeClient(out AGeneration: IntegerRAL): TRALSMTPClient;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// The route of the module, then <Domain>/send.
    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
      override;
    /// The send route (for Swagger and Postman); the caller frees the list.
    function GetListRoutes: TList; override;
  published
    { Authors the requests may use besides DefaultFrom: an address, @@domain
      for any address of a domain, or * for any. Empty: only DefaultFrom. }
    property AllowedFrom: TStrings read FAllowedFrom write SetAllowedFrom;
    /// The SMTP client whose settings send (a copy per concurrent request).
    property Client: TRALSMTPClient read FClient write SetClient;
    /// The author ("Name <a@@x>") when the request gives none.
    property DefaultFrom: StringRAL read FDefaultFrom write FDefaultFrom;
    /// Most recipients (To, Cc and Bcc together) of one request; 0 = no limit.
    property MaxRecipients: IntegerRAL read FMaxRecipients write FMaxRecipients
      default RALMailRestMaxRecipients;
    /// Fires before each message goes out: change it or refuse the request.
    property OnBeforeSend: TRALMailRestSendEvent read FOnBeforeSend write FOnBeforeSend;
    /// The queue the messages go to instead of being sent at once (202 and the job id).
    property Queue: TRALMailQueue read FQueue write SetQueue;
  end;

implementation

uses
  RALTools, RALBase64;

const
  /// Path of the send route, after Domain.
  cSend = '/send';
  /// Most idle copies of Client kept.
  cMaxIdle = 8;
  /// HTTP 202: the message is queued.
  cAccepted = 202;

/// Whether AName is a header field a request may add.
function AllowedHeader(const AName: StringRAL): Boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  if AName = '' then
    Exit;
  for vInt := 1 to Length(AName) do
    if not (AName[vInt] in ['A'..'Z', 'a'..'z', '0'..'9', '-']) then
      Exit;
  Result := (RALPosText('x-', AName) = 1) or (RALPosText('list-', AName) = 1);
end;

/// Whether AText holds a line break or a NUL (header injection).
function HasBreak(const AText: StringRAL): Boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 1 to Length(AText) do
    if AText[vInt] in [#0, #10, #13] then
    begin
      Result := True;
      Exit;
    end;
end;

/// Adds the mailboxes of a JSON text or array (texts or {name, address}) to AList.
function AddAddresses(AValue: TRALJSON; AList: TRALMailAddresses): Boolean;
var
  vInt: IntegerRAL;
  vItem, vNode: TRALJSON;
  vName: StringRAL;
begin
  Result := True;
  if (AValue = nil) or AValue.IsNull then
    Exit;
  case AValue.Kind of
    rjkString:
      begin
        Result := not HasBreak(AValue.AsString);
        if Result then
          AList.AddText(AValue.AsString);
      end;
    rjkObject:
      begin
        vNode := TRALJSONObject(AValue).Find('address');
        Result := (vNode <> nil) and (vNode.Kind = rjkString) and not HasBreak(vNode.AsString);
        if not Result then
          Exit;
        vName := '';
        vItem := TRALJSONObject(AValue).Find('name');
        if (vItem <> nil) and (vItem.Kind = rjkString) then
          vName := vItem.AsString;
        Result := not HasBreak(vName);
        if Result then
          AList.Add(RALTrim(vNode.AsString), vName);
      end;
    rjkArray:
      for vInt := 0 to TRALJSONArray(AValue).Count - 1 do
      begin
        vItem := TRALJSONArray(AValue).Get(vInt);
        if vItem.Kind = rjkArray then
          Result := False
        else
          Result := AddAddresses(vItem, AList);
        if not Result then
          Exit;
      end;
  else
    Result := False;
  end;
end;

/// The priority named AText; False when it is not one.
function ReadPriority(const AText: StringRAL; out APriority: TRALMailPriority): Boolean;
begin
  Result := True;
  if (AText = '') or RALSameName(AText, 'normal') then
    APriority := mpNormal
  else if RALSameName(AText, 'high') then
    APriority := mpHigh
  else if RALSameName(AText, 'low') then
    APriority := mpLow
  else
    Result := False;
end;

{ TRALMailRestModule }

constructor TRALMailRestModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Domain := '/mail';
  FAllowedFrom := TStringList.Create;
  FIdle := TList.Create;
  FLock := TCriticalSection.Create;
  FMaxRecipients := RALMailRestMaxRecipients;
  // owned by the module, so its full path carries the Domain
  FInternal := TRALRoutes.Create(Self);
  FSendRoute := TRALRoute(FInternal.Add);
  FSendRoute.Route := cSend;
  FSendRoute.Name := 'mailsend';
  FSendRoute.Description.Text := 'Sends the e-mail described by the body (JSON or form)';
  FSendRoute.AllowedMethods := [amPOST];
  FSendRoute.OnReply := DoSend;
end;

destructor TRALMailRestModule.Destroy;
begin
  SetClient(nil);
  SetQueue(nil);
  FreeIdle;
  FreeAndNil(FInternal);
  FreeAndNil(FIdle);
  FreeAndNil(FLock);
  FreeAndNil(FAllowedFrom);
  inherited Destroy;
end;

procedure TRALMailRestModule.SetAllowedFrom(AValue: TStrings);
begin
  FAllowedFrom.Assign(AValue);
end;

procedure TRALMailRestModule.SetClient(AValue: TRALSMTPClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
  FreeIdle;
end;

procedure TRALMailRestModule.SetQueue(AValue: TRALMailQueue);
begin
  if FQueue = AValue then
    Exit;
  if FQueue <> nil then
    FQueue.RemoveFreeNotification(Self);
  FQueue := AValue;
  if FQueue <> nil then
    FQueue.FreeNotification(Self);
end;

procedure TRALMailRestModule.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then
    Exit;
  if AComponent = FClient then
  begin
    FClient := nil;
    FreeIdle;
  end
  else if AComponent = FQueue then
    FQueue := nil;
end;

procedure TRALMailRestModule.FreeIdle;
var
  vInt: IntegerRAL;
begin
  if FLock = nil then
    Exit;
  FLock.Acquire;
  try
    Inc(FGeneration);
    for vInt := 0 to FIdle.Count - 1 do
      TRALSMTPClient(FIdle[vInt]).Free;
    FIdle.Clear;
  finally
    FLock.Release;
  end;
end;

function TRALMailRestModule.TakeClient(out AGeneration: IntegerRAL): TRALSMTPClient;
var
  vSource: TRALSMTPClient;
begin
  Result := nil;
  FLock.Acquire;
  try
    AGeneration := FGeneration;
    vSource := FClient;
    if FIdle.Count > 0 then
    begin
      Result := TRALSMTPClient(FIdle[FIdle.Count - 1]);
      FIdle.Delete(FIdle.Count - 1);
    end;
  finally
    FLock.Release;
  end;
  if (Result = nil) and (vSource <> nil) then
  begin
    Result := TRALSMTPClient.Create(nil);
    Result.Assign(vSource);
  end;
end;

procedure TRALMailRestModule.GiveClient(AClient: TRALSMTPClient; AGeneration: IntegerRAL);
begin
  FLock.Acquire;
  try
    if (AGeneration = FGeneration) and (FIdle.Count < cMaxIdle) then
    begin
      FIdle.Add(AClient);
      AClient := nil;
    end;
  finally
    FLock.Release;
  end;
  AClient.Free;
end;

function TRALMailRestModule.Prefix(const AName: StringRAL): StringRAL;
begin
  Result := Domain;
  if (Result <> '') and (Result[Length(Result)] = '/') then
    Delete(Result, Length(Result), 1);
  Result := Result + AName;
end;

function TRALMailRestModule.CanAnswerRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
begin
  Result := inherited CanAnswerRoute(ARequest, AResponse);
  if Result <> nil then
    Exit;
  if RALSameName(ARequest.Query, Prefix(cSend)) then
    Result := FSendRoute;
  if (Result <> nil) and Assigned(OnBeforeAnswer) then
    OnBeforeAnswer(ARequest, AResponse);
end;

function TRALMailRestModule.GetListRoutes: TList;
var
  vInt: IntegerRAL;
begin
  Result := inherited GetListRoutes;
  for vInt := 0 to FInternal.Count - 1 do
    Result.Add(FInternal.Items[vInt]);
end;

function TRALMailRestModule.AllowedSender(const AAddress: StringRAL): Boolean;
var
  vList: TRALMailAddresses;
  vInt, vAt: IntegerRAL;
  vEntry: StringRAL;
begin
  Result := False;
  if AAddress = '' then
    Exit;
  vList := TRALMailAddresses.Create(nil);
  try
    vList.AddText(FDefaultFrom);
    if (vList.Count > 0) and RALSameName(vList[0].Address, AAddress) then
    begin
      Result := True;
      Exit;
    end;
  finally
    vList.Free;
  end;
  vAt := RALMailPos('@', AAddress);
  for vInt := 0 to FAllowedFrom.Count - 1 do
  begin
    vEntry := RALTrim(StringRAL(FAllowedFrom[vInt]));
    if (vEntry = '*') or RALSameName(vEntry, AAddress) or
      ((vEntry <> '') and (vEntry[1] = '@') and (vAt > 0) and
      RALSameName(vEntry, Copy(AAddress, vAt, MaxInt))) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TRALMailRestModule.BuildFromJSON(AObject: TRALJSONObject;
  AMessage: TRALMailMessage; out AError: StringRAL): Boolean;
var
  vNode, vItem, vValue: TRALJSON;
  vInt: IntegerRAL;
  vPriority: TRALMailPriority;
  vAttachment: TRALMailAttachment;
  vBytes: TBytes;
  vStream: TMemoryStream;
  vName: StringRAL;

  function Text(const AName: StringRAL; out AValue: StringRAL): Boolean;
  begin
    AValue := '';
    vValue := AObject.Find(AName);
    Result := (vValue = nil) or vValue.IsNull or (vValue.Kind = rjkString);
    if Result and (vValue <> nil) and not vValue.IsNull then
      AValue := vValue.AsString;
    if not Result then
      AError := StringRAL(Format(emMailRestBadField, [string(AName)]));
  end;

  function Addresses(const AName: StringRAL; AList: TRALMailAddresses): Boolean;
  begin
    Result := AddAddresses(AObject.Find(AName), AList);
    if not Result then
      AError := StringRAL(Format(emMailRestBadField, [string(AName)]));
  end;

begin
  Result := False;
  AError := '';
  if not (Addresses('from', AMessage.From) and Addresses('to', AMessage.Recipients) and
    Addresses('cc', AMessage.CC) and Addresses('bcc', AMessage.BCC) and
    Addresses('replyTo', AMessage.ReplyTo)) then
    Exit;
  if not Text('subject', vName) then
    Exit;
  if HasBreak(vName) then
  begin
    AError := StringRAL(Format(emMailRestBadField, ['subject']));
    Exit;
  end;
  AMessage.Subject := vName;
  if not Text('text', vName) then
    Exit;
  AMessage.Text := vName;
  if not Text('html', vName) then
    Exit;
  AMessage.HTML := vName;
  if not Text('priority', vName) then
    Exit;
  if not ReadPriority(vName, vPriority) then
  begin
    AError := StringRAL(Format(emMailRestBadField, ['priority']));
    Exit;
  end;
  AMessage.Priority := vPriority;
  // extra header fields
  vNode := AObject.Find('headers');
  if (vNode <> nil) and not vNode.IsNull then
  begin
    if vNode.Kind <> rjkObject then
    begin
      AError := StringRAL(Format(emMailRestBadField, ['headers']));
      Exit;
    end;
    for vInt := 0 to TRALJSONObject(vNode).Count - 1 do
    begin
      vName := TRALJSONObject(vNode).GetName(vInt);
      vItem := TRALJSONObject(vNode).Get(vInt);
      if not AllowedHeader(vName) or (vItem.Kind <> rjkString) or HasBreak(vItem.AsString) then
      begin
        AError := StringRAL(Format(emMailRestBadHeader, [string(vName)]));
        Exit;
      end;
      AMessage.Headers.Add(string(vName) + '=' + string(vItem.AsString));
    end;
  end;
  // attachments, the content in base64
  vNode := AObject.Find('attachments');
  if (vNode <> nil) and not vNode.IsNull then
  begin
    if vNode.Kind <> rjkArray then
    begin
      AError := StringRAL(Format(emMailRestBadField, ['attachments']));
      Exit;
    end;
    for vInt := 0 to TRALJSONArray(vNode).Count - 1 do
    begin
      vItem := TRALJSONArray(vNode).Get(vInt);
      vValue := nil;
      if vItem.Kind = rjkObject then
        vValue := TRALJSONObject(vItem).Find('content');
      if (vValue = nil) or (vValue.Kind <> rjkString) then
      begin
        AError := StringRAL(Format(emMailRestBadField, ['attachments']));
        Exit;
      end;
      try
        vBytes := TRALBase64.DecodeAsBytes(vValue.AsString);
      except
        vBytes := nil;
      end;
      if (vBytes = nil) and (vValue.AsString <> '') then
      begin
        AError := StringRAL(Format(emMailRestBadField, ['attachments.content']));
        Exit;
      end;
      vAttachment := TRALMailAttachment(AMessage.Attachments.Add);
      vStream := TMemoryStream.Create;
      if vBytes <> nil then
        vStream.WriteBuffer(vBytes[0], Length(vBytes));
      vStream.Position := 0;
      vAttachment.Data := vStream;
      vValue := TRALJSONObject(vItem).Find('fileName');
      if (vValue <> nil) and (vValue.Kind = rjkString) and not HasBreak(vValue.AsString) then
        vAttachment.FileName := vValue.AsString;
      vValue := TRALJSONObject(vItem).Find('contentType');
      if (vValue <> nil) and (vValue.Kind = rjkString) and not HasBreak(vValue.AsString) then
        vAttachment.ContentType := vValue.AsString;
      vValue := TRALJSONObject(vItem).Find('contentId');
      if (vValue <> nil) and (vValue.Kind = rjkString) and not HasBreak(vValue.AsString) then
        vAttachment.ContentID := vValue.AsString;
      vValue := TRALJSONObject(vItem).Find('inline');
      vAttachment.IsInline := (vValue <> nil) and (vValue.Kind = rjkBoolean) and vValue.AsBoolean;
    end;
  end;
  Result := True;
end;

function TRALMailRestModule.BuildFromForm(ARequest: TRALRequest;
  AMessage: TRALMailMessage; out AError: StringRAL): Boolean;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
  vName, vValue: StringRAL;
  vPriority: TRALMailPriority;
  vAttachment: TRALMailAttachment;
  vStream: TMemoryStream;
  vContent: TStream;
begin
  Result := False;
  AError := '';
  for vInt := 0 to ARequest.Params.Count - 1 do
  begin
    vParam := ARequest.Params.Index[vInt];
    if not (vParam.Kind in [rpkFIELD, rpkBODY]) then
      Continue;
    vName := vParam.ParamName;
    if vParam.FileName <> '' then
    begin
      // a file is an attachment; cid:<id> makes an inline part
      vAttachment := TRALMailAttachment(AMessage.Attachments.Add);
      vStream := TMemoryStream.Create;
      vContent := vParam.Content;
      if vContent <> nil then
      begin
        vContent.Position := 0;
        vStream.CopyFrom(vContent, vContent.Size);
      end;
      vStream.Position := 0;
      vAttachment.Data := vStream;
      if not HasBreak(vParam.FileName) then
        vAttachment.FileName := vParam.FileName;
      if not HasBreak(vParam.ContentType) then
        vAttachment.ContentType := RALMailHeaderMain(vParam.ContentType);
      if RALPosText('cid:', vName) = 1 then
      begin
        vAttachment.IsInline := True;
        vAttachment.ContentID := Copy(vName, 5, MaxInt);
      end;
      Continue;
    end;
    vValue := vParam.AsString;
    if RALSameName(vName, 'text') then
      AMessage.Text := vValue
    else if RALSameName(vName, 'html') then
      AMessage.HTML := vValue
    else if HasBreak(vValue) then
    begin
      // every other field ends up in a header
      if RALSameName(vName, 'from') or RALSameName(vName, 'to') or RALSameName(vName, 'cc') or
        RALSameName(vName, 'bcc') or RALSameName(vName, 'replyTo') or
        RALSameName(vName, 'subject') or RALSameName(vName, 'priority') then
      begin
        AError := StringRAL(Format(emMailRestBadField, [string(vName)]));
        Exit;
      end;
    end
    else if RALSameName(vName, 'from') then
      AMessage.From.AddText(vValue)
    else if RALSameName(vName, 'to') then
      AMessage.Recipients.AddText(vValue)
    else if RALSameName(vName, 'cc') then
      AMessage.CC.AddText(vValue)
    else if RALSameName(vName, 'bcc') then
      AMessage.BCC.AddText(vValue)
    else if RALSameName(vName, 'replyTo') then
      AMessage.ReplyTo.AddText(vValue)
    else if RALSameName(vName, 'subject') then
      AMessage.Subject := vValue
    else if RALSameName(vName, 'priority') then
    begin
      if not ReadPriority(vValue, vPriority) then
      begin
        AError := StringRAL(Format(emMailRestBadField, ['priority']));
        Exit;
      end;
      AMessage.Priority := vPriority;
    end;
  end;
  Result := True;
end;

procedure TRALMailRestModule.Fail(AResponse: TRALResponse; AStatus: IntegerRAL;
  const AText: StringRAL);
var
  vObject: TRALJSONObject;
begin
  vObject := TRALJSONObject.Create;
  try
    vObject.Add('error', AText);
    AResponse.Answer(AStatus, vObject.ToJSON, rctAPPLICATIONJSON);
  finally
    vObject.Free;
  end;
end;

procedure TRALMailRestModule.SendNow(AMessage: TRALMailMessage; AResponse: TRALResponse);
var
  vClient: TRALSMTPClient;
  vGeneration, vInt: IntegerRAL;
  vObject, vItem: TRALJSONObject;
  vArray: TRALJSONArray;
  vResult: TRALSMTPRecipientResult;
begin
  vClient := TakeClient(vGeneration);
  if vClient = nil then
  begin
    Fail(AResponse, HTTP_ServiceUnavailable, StringRAL(emMailRestNoClient));
    Exit;
  end;
  vObject := TRALJSONObject.Create;
  try
    try
      vClient.Send(AMessage);
    except
      on E: ERALSMTPError do
      begin
        // the server refused: the copy stays usable unless the connection broke
        vObject.Add('error', StringRAL(E.Message));
        vObject.Add('smtpCode', Int64RAL(E.Code));
        if E.Enhanced <> '' then
          vObject.Add('enhancedCode', E.Enhanced);
        AResponse.Answer(HTTP_BadGateway, vObject.ToJSON, rctAPPLICATIONJSON);
        GiveClient(vClient, vGeneration);
        vClient := nil;
        Exit;
      end;
    end;
    vObject.Add('messageId', AMessage.MessageID);
    vArray := TRALJSONArray.Create;
    vObject.Add('recipients', vArray);
    for vInt := 0 to vClient.ResultCount - 1 do
    begin
      vResult := vClient.Results[vInt];
      vItem := TRALJSONObject.Create;
      vArray.Add(vItem);
      vItem.Add('address', vResult.Address);
      vItem.Add('accepted', vResult.Accepted);
      vItem.Add('code', Int64RAL(vResult.Code));
      vItem.Add('reply', vResult.Reply);
    end;
    AResponse.Answer(HTTP_OK, vObject.ToJSON, rctAPPLICATIONJSON);
    GiveClient(vClient, vGeneration);
    vClient := nil;
  finally
    // a transport failure drops the copy: its connection state is unknown
    vClient.Free;
    vObject.Free;
  end;
end;

procedure TRALMailRestModule.DoSend(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vMessage: TRALMailMessage;
  vJSON: TRALJSON;
  vType, vError, vBody: StringRAL;
  vRecipients: TStringList;
  vStatus: IntegerRAL;
  vOk: Boolean;
  vObject: TRALJSONObject;
begin
  if (FClient = nil) and (FQueue = nil) then
  begin
    Fail(AResponse, HTTP_ServiceUnavailable, StringRAL(emMailRestNoClient));
    Exit;
  end;
  vMessage := TRALMailMessage.Create;
  vRecipients := TStringList.Create;
  try
    vType := ARequest.ContentType;
    if (RALPosText('multipart/', vType) > 0) or
      (RALPosText('application/x-www-form-urlencoded', vType) > 0) then
      vOk := BuildFromForm(ARequest, vMessage, vError)
    else
    begin
      vBody := ARequest.RequestText;
      try
        vJSON := TRALJSON.ParseJSON(vBody);
      except
        vJSON := nil;
      end;
      try
        vOk := vJSON is TRALJSONObject;
        if vOk then
          vOk := BuildFromJSON(TRALJSONObject(vJSON), vMessage, vError)
        else
          vError := StringRAL(emMailRestBadBody);
      finally
        vJSON.Free;
      end;
    end;
    if not vOk then
    begin
      Fail(AResponse, HTTP_BadRequest, vError);
      Exit;
    end;
    // the author: the one asked for, if allowed, else DefaultFrom
    if vMessage.From.Count = 0 then
      vMessage.From.AddText(FDefaultFrom);
    if (vMessage.From.Count <> 1) or (vMessage.From[0].Address = '') then
    begin
      Fail(AResponse, HTTP_BadRequest, StringRAL(emMailRestNoFrom));
      Exit;
    end;
    if not AllowedSender(vMessage.From[0].Address) then
    begin
      Fail(AResponse, HTTP_Forbidden, StringRAL(Format(emMailRestFromRefused,
        [string(vMessage.From[0].Address)])));
      Exit;
    end;
    vMessage.AllRecipients(vRecipients);
    if vRecipients.Count = 0 then
    begin
      Fail(AResponse, HTTP_BadRequest, StringRAL(emMailNoRecipient));
      Exit;
    end;
    if (FMaxRecipients > 0) and (vRecipients.Count > FMaxRecipients) then
    begin
      Fail(AResponse, HTTP_BadRequest, StringRAL(Format(emMailRestTooManyRecipients,
        [vRecipients.Count, FMaxRecipients])));
      Exit;
    end;
    if Assigned(FOnBeforeSend) then
    begin
      vStatus := 0;
      vError := '';
      FOnBeforeSend(Self, ARequest, vMessage, vStatus, vError);
      if vStatus >= 400 then
      begin
        Fail(AResponse, vStatus, vError);
        Exit;
      end;
    end;
    if FQueue <> nil then
    begin
      vObject := TRALJSONObject.Create;
      try
        vObject.Add('jobId', FQueue.Enqueue(vMessage));
        vObject.Add('messageId', vMessage.MessageID);
        AResponse.Answer(cAccepted, vObject.ToJSON, rctAPPLICATIONJSON);
      finally
        vObject.Free;
      end;
    end
    else
      SendNow(vMessage, AResponse);
  finally
    vRecipients.Free;
    vMessage.Free;
  end;
end;

end.
