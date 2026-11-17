/// The WhatsApp channel of the chat module on Meta's official Cloud API.
unit RALChatWhatsApp;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALJson,
  RALChatMessage, RALChatChannel, RALChatHttp, RALChatGraph;

type
  /// A delivery status of a sent message, as the webhook reports it.
  TRALChatWhatsAppStatus = record
    /// The id (wamid) of the message the status is about.
    MessageId: StringRAL;
    /// The user the message went to.
    RecipientId: StringRAL;
    /// sent, delivered, read or failed.
    Status: StringRAL;
    /// When it happened, Unix seconds.
    Timestamp: Int64RAL;
    /// The price category of the message (marketing, utility, authentication, service).
    Category: StringRAL;
    /// Whether the message is charged.
    Billable: Boolean;
    /// The error code of a failed message; 0 otherwise.
    ErrorCode: Integer;
    /// What went wrong with a failed message.
    ErrorText: StringRAL;
  end;

  { Receives a delivery status; runs on the webhook's thread before the answer goes
    back to Meta, so it must be quick. }
  TRALChatWhatsAppStatusEvent = procedure(Sender: TObject;
    const AStatus: TRALChatWhatsAppStatus) of object;

  { Receives a reaction of a user to a message; an empty AEmoji removes it. Runs on the
    webhook's thread before the answer goes back to Meta, so it must be quick. }
  TRALChatWhatsAppReactionEvent = procedure(Sender: TObject;
    const AFrom, AMessageId, AEmoji: StringRAL) of object;

  { The WhatsApp channel of the Cloud API: the webhook (verify token, X-Hub-Signature-256)
    with text, media, location, contacts, button and list answers and template quick
    replies; sending text, media by link or upload, reply buttons (a list from 4 to 10
    choices), lists, location, contacts, templates and reactions (TemplateMessage,
    ReactionMessage), the typing indicator and read marks. An outgoing message whose Raw
    is a Cloud API message object (a type and its member) goes as it is. }
  TRALChatWhatsApp = class(TRALChatGraphChannel)
  private
    FBusinessAccountId: StringRAL;
    /// Name=Value of each conversation and the last message received in it.
    FLastInbound: TStringList;
    /// Guards FLastInbound.
    FLastLock: TCriticalSection;
    FOnReaction: TRALChatWhatsAppReactionEvent;
    FOnStatus: TRALChatWhatsAppStatusEvent;
    FPhoneNumberId: StringRAL;

    /// The message last received in AChat, or empty.
    function LastInbound(const AChat: StringRAL): StringRAL;
    /// Downloads a received media file: its address first, then the bytes.
    function LoadMedia(AMedia: TRALChatMedia): TStream;
    /// A message body to ATo, with the reply context when AContext is not empty.
    function NewBody(const ATo, AContext: StringRAL): TRALJSONObject;
    /// Reads one received message into AMessages.
    procedure ParseMessage(AItem: TRALJSONObject; AContacts: TRALJSONArray;
      AMessages: TRALChatMessageList);
    /// Reads one delivery status and hands it to OnStatus.
    procedure ParseStatus(AItem: TRALJSONObject);
    /// Reads the value of a change of the messages field.
    procedure ParseValue(AValue: TRALJSONObject; AMessages: TRALChatMessageList);
    /// The phone number id, or an exception when it is missing.
    function PhoneId: StringRAL;
    /// Posts a message body and fills AResult with the outcome and the new id.
    procedure Post(ABody: TRALJSONObject; const APair: StringRAL; var AResult: TRALChatSendResult);
    { The outgoing Raw of AMessage as a message object when it is one (a type and its
      member, no from); nil otherwise. The caller frees it. }
    function RawBody(AMessage: TRALChatMessage): TRALJSONObject;
    /// Remembers AId as the last message received in AChat.
    procedure Remember(const AChat, AId: StringRAL);
    /// Sends AMessage, in as many calls as its parts need.
    procedure SendMessage(AMessage: TRALChatMessage; var AResult: TRALChatSendResult);
    /// Posts a text message.
    procedure SendText(const ATo, AText, AContext, APair: StringRAL;
      var AResult: TRALChatSendResult);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId: StringRAL; out AResult: TRALChatSendResult); override;
    function Dialect: TRALChatDialect; override;
    { Marks the received message AMessageId as read (blue ticks) and, with ATyping, shows
      the typing indicator for up to 25 seconds or until the next message. Synchronous. }
    function MarkRead(const AMessageId: StringRAL; ATyping: Boolean = False): TRALChatSendResult;
    /// The pair the limiter counts: the phone number id and the user.
    function PairOf(AMessage: TRALChatMessage): StringRAL; override;
    procedure ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList); override;
    { A message that reacts to AMessageId of the conversation ATo with AEmoji (empty
      removes the reaction), to send through the bot; the caller frees it. }
    class function ReactionMessage(const ATo, AMessageId, AEmoji: StringRAL): TRALChatMessage;
    { A message with the approved template AName in ALanguage ('pt_BR', 'en_US'), its body
      parameters in AParams, to send through the bot (Send or SendWait); the only kind
      that reaches a user outside the 24 hour window. The caller frees it. }
    class function TemplateMessage(const ATo, AName, ALanguage: StringRAL;
      const AParams: array of StringRAL): TRALChatMessage;
    function TestToken(out AReport: StringRAL): Boolean; override;
    { Uploads AMedia (Data) to Meta and returns the media id (good for 30 days) in
      AMediaId; False with the reason in AResult. }
    function UploadMedia(AMedia: TRALChatMedia; out AMediaId: StringRAL;
      var AResult: TRALChatSendResult): Boolean;
  published
    /// The WhatsApp Business Account id (WABA), for the application's own calls.
    property BusinessAccountId: StringRAL read FBusinessAccountId write FBusinessAccountId;
    /// Receives the reactions of the users.
    property OnReaction: TRALChatWhatsAppReactionEvent read FOnReaction write FOnReaction;
    /// Receives the delivery statuses (sent, delivered, read, failed) and their prices.
    property OnStatus: TRALChatWhatsAppStatusEvent read FOnStatus write FOnStatus;
    { The phone number id the channel sends from (not the number itself); a webhook of
      another number of the same app is ignored. }
    property PhoneNumberId: StringRAL read FPhoneNumberId write FPhoneNumberId;
  end;

implementation

type
  /// An exception of the channel that is not worth another attempt.
  ERALChatWhatsAppFinal = class(ERALChatError);

const
  /// The longest body of an interactive message, in characters.
  cInteractiveMax = 1024;
  /// The longest caption of a media message, in characters.
  cCaptionMax = 1024;
  /// The most reply buttons a message has; more become a list.
  cReplyButtonsMax = 3;
  /// The most rows of a list.
  cListRowsMax = 10;
  /// How many conversations remember their last received message.
  cLastMax = 4096;

{ TRALChatWhatsApp }

constructor TRALChatWhatsApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastLock := TCriticalSection.Create;
  FLastInbound := TStringList.Create;
  ChannelName := 'whatsapp';
  Capabilities.Supported := [ccButtons, ccLists, ccReactions, ccTyping, ccReadReceipts,
    ccTemplates, ccMedia];
  // up to 3 are reply buttons, up to 10 a list; more become numbered text
  Capabilities.MaxButtons := cListRowsMax;
  Capabilities.MaxText := 4096;
  Capabilities.MaxMediaBytes := 104857600;
  // 80 messages a second per number; a pair (number, user) 1 every 6 s with a burst of 45
  Rates.Total := 80;
  Rates.TotalBurst := 80;
  Rates.PerPair := 1 / 6;
  Rates.PerPairBurst := 45;
end;

destructor TRALChatWhatsApp.Destroy;
begin
  FLastInbound.Free;
  FLastLock.Free;
  inherited Destroy;
end;

procedure TRALChatWhatsApp.Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId: StringRAL; out AResult: TRALChatSendResult);
var
  vId: StringRAL;
begin
  ResetResult(AResult);
  try
    case AKind of
      okTyping:
      begin
        // the indicator hangs on the last message the user sent
        vId := LastInbound(AMessage.Chat.Id);
        if vId = '' then
          AResult.Ok := True
        else
          AResult := MarkRead(vId, True);
      end;
      okEdit:
        raise ERALChatWhatsAppFinal.CreateFmt(emChatNoCapability, [string(ChannelName), 'edit']);
      okDelete:
        raise ERALChatWhatsAppFinal.CreateFmt(emChatNoCapability, [string(ChannelName), 'delete']);
    else
      SendMessage(AMessage, AResult);
    end;
  except
    on E: ERALChatError do
    begin
      AResult.Ok := False;
      AResult.Transient := False;
      AResult.ErrorText := Hide(StringRAL(E.Message));
    end;
    on E: Exception do
    begin
      AResult.Ok := False;
      AResult.Transient := True;
      AResult.ErrorText := Hide(StringRAL(E.ClassName + ': ' + E.Message));
    end;
  end;
end;

function TRALChatWhatsApp.Dialect: TRALChatDialect;
begin
  Result := cdWhatsApp;
end;

function TRALChatWhatsApp.LastInbound(const AChat: StringRAL): StringRAL;
var
  vIdx: Integer;
begin
  Result := '';
  FLastLock.Enter;
  try
    vIdx := FLastInbound.IndexOfName(string(AChat));
    if vIdx >= 0 then
      Result := StringRAL(FLastInbound.ValueFromIndex[vIdx]);
  finally
    FLastLock.Leave;
  end;
end;

function TRALChatWhatsApp.LoadMedia(AMedia: TRALChatMedia): TStream;
var
  vReply: TRALChatGraphReply;
  vUrl: StringRAL;
  vSize: Int64RAL;
begin
  if AMedia.Id = '' then
    raise ERALChatError.Create(emChatNoMedia);
  // 1. the address of the file (it expires in 5 minutes)
  Call(amGET, AMedia.Id, nil, vReply);
  try
    if not vReply.Ok then
      raise ERALChatError.CreateFmt(emChatMeApi, ['media', string(vReply.Description),
        vReply.Code, vReply.Subcode, string(vReply.TraceId)]);
    vUrl := RALChatJStr(vReply.Root, 'url');
    vSize := RALChatJInt(vReply.Root, 'file_size', -1);
  finally
    vReply.Root.Free;
  end;
  if (vSize > 0) and (MaxDownloadSize > 0) and (vSize > MaxDownloadSize) then
    raise ERALChatError.CreateFmt(emChatMeTooBig, [MaxDownloadSize]);
  // 2. the bytes, from a Meta host only, with the token
  Result := DownloadFrom(vUrl, True);
end;

function TRALChatWhatsApp.MarkRead(const AMessageId: StringRAL; ATyping: Boolean): TRALChatSendResult;
var
  vBody, vTyping: TRALJSONObject;
  vReply: TRALChatGraphReply;
begin
  ResetResult(Result);
  try
    vBody := TRALJSONObject.Create;
    try
      vBody.Add('messaging_product', 'whatsapp');
      vBody.Add('status', 'read');
      vBody.Add('message_id', AMessageId);
      if ATyping then
      begin
        vTyping := TRALJSONObject.Create;
        vBody.Add('typing_indicator', vTyping);
        vTyping.Add('type', 'text');
      end;
      Call(amPOST, PhoneId + '/messages', vBody, vReply);
    finally
      vBody.Free;
    end;
    try
      FillResult('messages', '', vReply, Result);
      if Result.Ok then
        Result.MessageId := AMessageId;
    finally
      vReply.Root.Free;
    end;
  except
    on E: Exception do
    begin
      Result.Ok := False;
      Result.Transient := not (E is ERALChatError);
      Result.ErrorText := Hide(StringRAL(E.Message));
    end;
  end;
end;

function TRALChatWhatsApp.NewBody(const ATo, AContext: StringRAL): TRALJSONObject;
var
  vContext: TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('messaging_product', 'whatsapp');
  Result.Add('recipient_type', 'individual');
  Result.Add('to', ATo);
  if AContext <> '' then
  begin
    vContext := TRALJSONObject.Create;
    Result.Add('context', vContext);
    vContext.Add('message_id', AContext);
  end;
end;

function TRALChatWhatsApp.PairOf(AMessage: TRALChatMessage): StringRAL;
begin
  Result := FPhoneNumberId + ':' + AMessage.Chat.Id;
end;

procedure TRALChatWhatsApp.ParseMessage(AItem: TRALJSONObject; AContacts: TRALJSONArray;
  AMessages: TRALChatMessageList);
var
  vType, vFrom, vId, vSub: StringRAL;
  vMessage: TRALChatMessage;
  vInfo: TRALChatInfo;
  vUser: TRALChatUser;
  vPart, vContact: TRALJSONObject;
  vList: TRALJSONArray;
  vIdx, vCode: Integer;
  vLocation: TRALChatLocation;
  vCard: TRALChatContact;
  vLoader: TRALChatMediaLoader;
begin
  vType := RALChatJStr(AItem, 'type');
  vFrom := RALChatJStr(AItem, 'from');
  vId := RALChatJStr(AItem, 'id');
  if (vFrom = '') or (vId = '') then
    Exit;
  if vType = 'reaction' then
  begin
    vPart := RALChatJObj(AItem, 'reaction');
    if Assigned(FOnReaction) and not Seen('r' + vId) then
      try
        FOnReaction(Self, vFrom, RALChatJStr(vPart, 'message_id'), RALChatJStr(vPart, 'emoji'));
      except
        // the application's handler must not refuse Meta's webhook
      end;
    Exit;
  end;
  vMessage := TRALChatMessage.Create;
  try
    vMessage.Id := vId;
    vMessage.EventId := vId;
    vMessage.Timestamp := RALChatJInt(AItem, 'timestamp', 0);
    vInfo.Id := vFrom;
    vInfo.ChatType := cctPrivate;
    vInfo.Title := '';
    vMessage.Chat := vInfo;
    vUser.Id := vFrom;
    vUser.Name := '';
    vUser.Phone := vFrom;
    if AContacts <> nil then
      for vIdx := 0 to AContacts.Count - 1 do
        if (AContacts[vIdx].Kind = rjkObject) and
           ((RALChatJStr(AContacts[vIdx], 'wa_id') = vFrom) or (AContacts.Count = 1)) then
        begin
          vUser.Name := RALChatJStr(RALChatJObj(AContacts[vIdx], 'profile'), 'name');
          Break;
        end;
    if vUser.Name = '' then
      vUser.Name := vFrom;
    vMessage.From := vUser;
    vPart := RALChatJObj(AItem, 'context');
    if (vPart <> nil) and (RALChatJStr(vPart, 'id') <> '') then
      vMessage.ReplyTo.Id := RALChatJStr(vPart, 'id');
    if vType = 'text' then
      vMessage.Text := RALChatJStr(RALChatJObj(AItem, 'text'), 'body')
    else if (vType = 'image') or (vType = 'audio') or (vType = 'video') or
      (vType = 'document') or (vType = 'sticker') then
    begin
      if vType = 'image' then
        vMessage.Kind := ckImage
      else if vType = 'audio' then
        vMessage.Kind := ckAudio
      else if vType = 'video' then
        vMessage.Kind := ckVideo
      else if vType = 'sticker' then
        vMessage.Kind := ckSticker
      else
        vMessage.Kind := ckDocument;
      vPart := RALChatJObj(AItem, vType);
      vMessage.Text := RALChatJStr(vPart, 'caption');
      vMessage.Media.Id := RALChatJStr(vPart, 'id');
      vMessage.Media.MimeType := RALChatJStr(vPart, 'mime_type');
      vMessage.Media.FileName := RALChatJStr(vPart, 'filename');
      vMessage.Media.Size := RALChatJInt(vPart, 'file_size', -1);
      vLoader := LoadMedia;
      vMessage.Media.Loader := vLoader;
    end
    else if vType = 'location' then
    begin
      vPart := RALChatJObj(AItem, 'location');
      vMessage.Kind := ckLocation;
      vLocation.Valid := True;
      Val(string(RALChatJStr(vPart, 'latitude')), vLocation.Latitude, vCode);
      if vCode <> 0 then
        vLocation.Latitude := 0;
      Val(string(RALChatJStr(vPart, 'longitude')), vLocation.Longitude, vCode);
      if vCode <> 0 then
        vLocation.Longitude := 0;
      vLocation.Title := RALChatJStr(vPart, 'name');
      if vLocation.Title = '' then
        vLocation.Title := RALChatJStr(vPart, 'address');
      vMessage.Location := vLocation;
    end
    else if vType = 'contacts' then
    begin
      vList := RALChatJArr(AItem, 'contacts');
      if (vList = nil) or (vList.Count = 0) or (vList[0].Kind <> rjkObject) then
        Exit;
      vContact := TRALJSONObject(vList[0]);
      vMessage.Kind := ckContact;
      vCard.Valid := True;
      vCard.Name := RALChatJStr(RALChatJObj(vContact, 'name'), 'formatted_name');
      vCard.Phone := '';
      vList := RALChatJArr(vContact, 'phones');
      if (vList <> nil) and (vList.Count > 0) then
      begin
        vCard.Phone := RALChatJStr(vList[0], 'phone');
        if vCard.Phone = '' then
          vCard.Phone := RALChatJStr(vList[0], 'wa_id');
      end;
      vMessage.Contact := vCard;
    end
    else if vType = 'interactive' then
    begin
      vPart := RALChatJObj(AItem, 'interactive');
      vSub := RALChatJStr(vPart, 'type');
      if (vSub <> 'button_reply') and (vSub <> 'list_reply') then
        Exit;
      vPart := RALChatJObj(vPart, vSub);
      vMessage.Kind := ckCallback;
      vMessage.CallbackData := RALChatJStr(vPart, 'id');
      vMessage.Text := RALChatJStr(vPart, 'title');
    end
    else if vType = 'button' then
    begin
      // the quick reply button of a template
      vPart := RALChatJObj(AItem, 'button');
      vMessage.Kind := ckCallback;
      vMessage.Text := RALChatJStr(vPart, 'text');
      vMessage.CallbackData := RALChatJStr(vPart, 'payload');
      if vMessage.CallbackData = '' then
        vMessage.CallbackData := vMessage.Text;
    end
    else
      Exit;
    if KeepRaw then
      vMessage.Raw := AItem.ToJSON;
    Remember(vFrom, vId);
    AMessages.AddMessage(vMessage);
    vMessage := nil;
  finally
    vMessage.Free;
  end;
end;

procedure TRALChatWhatsApp.ParseStatus(AItem: TRALJSONObject);
var
  vStatus: TRALChatWhatsAppStatus;
  vPricing, vError: TRALJSONObject;
  vErrors: TRALJSONArray;
begin
  if not Assigned(FOnStatus) then
    Exit;
  vStatus.MessageId := RALChatJStr(AItem, 'id');
  vStatus.Status := RALChatJStr(AItem, 'status');
  if Seen('s' + vStatus.MessageId + ':' + vStatus.Status) then
    Exit;
  vStatus.RecipientId := RALChatJStr(AItem, 'recipient_id');
  vStatus.Timestamp := RALChatJInt(AItem, 'timestamp', 0);
  vPricing := RALChatJObj(AItem, 'pricing');
  vStatus.Category := RALChatJStr(vPricing, 'category');
  vStatus.Billable := RALChatJStr(vPricing, 'billable') = 'true';
  vStatus.ErrorCode := 0;
  vStatus.ErrorText := '';
  vErrors := RALChatJArr(AItem, 'errors');
  if (vErrors <> nil) and (vErrors.Count > 0) and (vErrors[0].Kind = rjkObject) then
  begin
    vError := TRALJSONObject(vErrors[0]);
    vStatus.ErrorCode := RALChatJInt(vError, 'code', 0);
    vStatus.ErrorText := RALChatJStr(vError, 'message');
    if vStatus.ErrorText = '' then
      vStatus.ErrorText := RALChatJStr(vError, 'title');
    if RALChatJStr(RALChatJObj(vError, 'error_data'), 'details') <> '' then
      vStatus.ErrorText := vStatus.ErrorText + ': ' +
        RALChatJStr(RALChatJObj(vError, 'error_data'), 'details');
  end;
  try
    FOnStatus(Self, vStatus);
  except
    // the application's handler must not refuse Meta's webhook
  end;
end;

procedure TRALChatWhatsApp.ParseValue(AValue: TRALJSONObject; AMessages: TRALChatMessageList);
var
  vPhone: StringRAL;
  vList, vContacts: TRALJSONArray;
  vIdx: Integer;
begin
  vPhone := RALChatJStr(RALChatJObj(AValue, 'metadata'), 'phone_number_id');
  if (FPhoneNumberId <> '') and (vPhone <> '') and (vPhone <> FPhoneNumberId) then
    Exit;
  vContacts := RALChatJArr(AValue, 'contacts');
  vList := RALChatJArr(AValue, 'messages');
  if vList <> nil then
    for vIdx := 0 to vList.Count - 1 do
      if vList[vIdx].Kind = rjkObject then
        ParseMessage(TRALJSONObject(vList[vIdx]), vContacts, AMessages);
  vList := RALChatJArr(AValue, 'statuses');
  if vList <> nil then
    for vIdx := 0 to vList.Count - 1 do
      if vList[vIdx].Kind = rjkObject then
        ParseStatus(TRALJSONObject(vList[vIdx]));
end;

procedure TRALChatWhatsApp.ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList);
var
  vRoot, vChange: TRALJSONObject;
  vEntries, vChanges: TRALJSONArray;
  vEntry, vIdx: Integer;
begin
  vRoot := RALChatParseObject(ABody);
  if vRoot = nil then
    raise ERALChatError.CreateFmt(emChatBadWebhook, [string(ChannelName)]);
  try
    if RALChatJStr(vRoot, 'object') <> 'whatsapp_business_account' then
      Exit;
    vEntries := RALChatJArr(vRoot, 'entry');
    if vEntries = nil then
      Exit;
    for vEntry := 0 to vEntries.Count - 1 do
    begin
      vChanges := RALChatJArr(vEntries[vEntry], 'changes');
      if vChanges = nil then
        Continue;
      for vIdx := 0 to vChanges.Count - 1 do
      begin
        if vChanges[vIdx].Kind <> rjkObject then
          Continue;
        vChange := TRALJSONObject(vChanges[vIdx]);
        if RALChatJStr(vChange, 'field') <> 'messages' then
          Continue;
        if RALChatJObj(vChange, 'value') <> nil then
          ParseValue(RALChatJObj(vChange, 'value'), AMessages);
      end;
    end;
  finally
    vRoot.Free;
  end;
end;

function TRALChatWhatsApp.PhoneId: StringRAL;
begin
  Result := FPhoneNumberId;
  if Result = '' then
    raise ERALChatWhatsAppFinal.CreateFmt(emChatWaNoPhone, [string(ChannelName)]);
end;

procedure TRALChatWhatsApp.Post(ABody: TRALJSONObject; const APair: StringRAL;
  var AResult: TRALChatSendResult);
var
  vReply: TRALChatGraphReply;
  vList: TRALJSONArray;
begin
  Call(amPOST, PhoneId + '/messages', ABody, vReply);
  try
    FillResult('messages', APair, vReply, AResult);
    if AResult.Ok then
    begin
      vList := RALChatJArr(vReply.Root, 'messages');
      if (vList <> nil) and (vList.Count > 0) then
        AResult.MessageId := RALChatJStr(vList[0], 'id');
    end;
  finally
    vReply.Root.Free;
  end;
end;

class function TRALChatWhatsApp.ReactionMessage(const ATo, AMessageId,
  AEmoji: StringRAL): TRALChatMessage;
var
  vRoot, vReaction: TRALJSONObject;
begin
  Result := TRALChatMessage.Create;
  try
    Result.ChatId := ATo;
    vRoot := TRALJSONObject.Create;
    try
      vRoot.Add('type', 'reaction');
      vReaction := TRALJSONObject.Create;
      vRoot.Add('reaction', vReaction);
      vReaction.Add('message_id', AMessageId);
      vReaction.Add('emoji', AEmoji);
      Result.Raw := vRoot.ToJSON;
    finally
      vRoot.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TRALChatWhatsApp.RawBody(AMessage: TRALChatMessage): TRALJSONObject;
var
  vType: StringRAL;
begin
  Result := nil;
  if (AMessage.Raw = '') or (AMessage.Raw[1] <> '{') then
    Exit;
  Result := RALChatParseObject(AMessage.Raw);
  if Result = nil then
    Exit;
  vType := RALChatJStr(Result, 'type');
  if (vType = '') or (Result.Find(vType) = nil) or (Result.Find('from') <> nil) then
    FreeAndNil(Result);
end;

procedure TRALChatWhatsApp.Remember(const AChat, AId: StringRAL);
var
  vIdx: Integer;
begin
  FLastLock.Enter;
  try
    vIdx := FLastInbound.IndexOfName(string(AChat));
    if vIdx >= 0 then
      FLastInbound.Delete(vIdx)
    else if FLastInbound.Count >= cLastMax then
      FLastInbound.Delete(0);
    FLastInbound.Add(string(AChat) + '=' + string(AId));
  finally
    FLastLock.Leave;
  end;
end;

procedure TRALChatWhatsApp.SendMessage(AMessage: TRALChatMessage; var AResult: TRALChatSendResult);
var
  vTo, vPair, vContext, vText, vType, vMediaId, vBodyText, vLine: StringRAL;
  vBody, vPart, vAction, vRow, vSection, vReply: TRALJSONObject;
  vArray, vRows, vSections, vPhones: TRALJSONArray;
  vReplies: TRALChatButtons;
  vLinks: TRALChatButtons;
  vIdx, vRowCount: Integer;
  vPieces: TStringList;
  vCaption, vInteractive: Boolean;
  vSize, vLimit: Int64RAL;

  procedure Add(var AList: TRALChatButtons; const AButton: TRALChatButton);
  begin
    SetLength(AList, Length(AList) + 1);
    AList[High(AList)] := AButton;
  end;

  function Next: StringRAL;
  begin
    // the reply context goes on the first call only
    Result := vContext;
    vContext := '';
  end;

begin
  vTo := AMessage.Chat.Id;
  vPair := PairOf(AMessage);
  vContext := '';
  if AMessage.HasReplyTo then
    vContext := AMessage.ReplyTo.Id;
  // a message object of the application (a template, a reaction) goes as it is
  vBody := RawBody(AMessage);
  if vBody <> nil then
    try
      vPart := NewBody(vTo, Next);
      try
        while vBody.Count > 0 do
        begin
          vLine := vBody.Names[0];
          if vPart.Find(vLine) <> nil then
            vBody.Extract(vLine).Free
          else
            vPart.Add(vLine, vBody.Extract(vLine));
        end;
        Post(vPart, vPair, AResult);
      finally
        vPart.Free;
      end;
      Exit;
    finally
      vBody.Free;
    end;
  vText := AMessage.Text;
  vReplies := nil;
  vLinks := nil;
  for vIdx := 0 to High(AMessage.Buttons) do
    if AMessage.Buttons[vIdx].Url <> '' then
      Add(vLinks, AMessage.Buttons[vIdx])
    else
      Add(vReplies, AMessage.Buttons[vIdx]);
  vRowCount := Length(vReplies) + Length(AMessage.List.Items);
  vInteractive := (vRowCount > 0) or (Length(vLinks) = 1);
  if (Length(vLinks) > 1) or ((Length(vLinks) = 1) and (vRowCount > 0)) then
  begin
    // one link alone is a call-to-action button; more, or links among replies, are text
    for vIdx := 0 to High(vLinks) do
    begin
      vLine := vLinks[vIdx].Caption + ': ' + vLinks[vIdx].Url;
      if vText <> '' then
        vText := vText + #10;
      vText := vText + vLine;
    end;
    vLinks := nil;
    vInteractive := vRowCount > 0;
  end;
  if vRowCount > cListRowsMax then
    raise ERALChatWhatsAppFinal.CreateFmt(emChatMeTooManyChoices, [vRowCount, cListRowsMax]);
  // a location or a contact card is its own message, after the text
  if ((AMessage.Kind = ckLocation) and AMessage.Location.Valid) or
     ((AMessage.Kind = ckContact) and AMessage.Contact.Valid) then
  begin
    if vText <> '' then
    begin
      SendText(vTo, vText, Next, vPair, AResult);
      if not AResult.Ok then
        Exit;
    end;
    vBody := NewBody(vTo, Next);
    try
      if AMessage.Kind = ckLocation then
      begin
        vBody.Add('type', 'location');
        vPart := TRALJSONObject.Create;
        vBody.Add('location', vPart);
        vPart.Add('latitude', AMessage.Location.Latitude);
        vPart.Add('longitude', AMessage.Location.Longitude);
        if AMessage.Location.Title <> '' then
          vPart.Add('name', AMessage.Location.Title);
      end
      else
      begin
        vBody.Add('type', 'contacts');
        vArray := TRALJSONArray.Create;
        vBody.Add('contacts', vArray);
        vPart := TRALJSONObject.Create;
        vArray.Add(vPart);
        vReply := TRALJSONObject.Create;
        vPart.Add('name', vReply);
        vReply.Add('formatted_name', AMessage.Contact.Name);
        vReply.Add('first_name', AMessage.Contact.Name);
        vPhones := TRALJSONArray.Create;
        vPart.Add('phones', vPhones);
        vReply := TRALJSONObject.Create;
        vPhones.Add(vReply);
        vReply.Add('phone', AMessage.Contact.Phone);
        vReply.Add('type', 'CELL');
      end;
      Post(vBody, vPair, AResult);
    finally
      vBody.Free;
    end;
    Exit;
  end;
  if AMessage.HasMedia then
  begin
    case AMessage.Kind of
      ckImage: vType := 'image';
      ckAudio: vType := 'audio';
      ckVideo: vType := 'video';
      ckSticker: vType := 'sticker';
      ckDocument: vType := 'document';
    else
      if Copy(string(AMessage.Media.MimeType), 1, 6) = 'image/' then
        vType := 'image'
      else if Copy(string(AMessage.Media.MimeType), 1, 6) = 'audio/' then
        vType := 'audio'
      else if Copy(string(AMessage.Media.MimeType), 1, 6) = 'video/' then
        vType := 'video'
      else
        vType := 'document';
    end;
    vMediaId := '';
    if AMessage.Media.Data <> nil then
    begin
      if vType = 'image' then
        vLimit := 5242880
      else if vType = 'sticker' then
        vLimit := 512000
      else if vType = 'document' then
        vLimit := 104857600
      else
        vLimit := 16777216;
      vSize := AMessage.Media.Data.Size;
      if vSize > vLimit then
        raise ERALChatWhatsAppFinal.CreateFmt(emChatMeTooBig, [vLimit]);
      if not UploadMedia(AMessage.Media, vMediaId, AResult) then
        Exit;
    end
    else if (AMessage.Media.Url = '') and (AMessage.Media.Id = '') then
      raise ERALChatWhatsAppFinal.Create(emChatNoMedia);
    vCaption := (not vInteractive) and (vText <> '') and
      ((vType = 'image') or (vType = 'video') or (vType = 'document')) and
      (Length(UnicodeString(vText)) <= cCaptionMax);
    vBody := NewBody(vTo, Next);
    try
      vBody.Add('type', vType);
      vPart := TRALJSONObject.Create;
      vBody.Add(vType, vPart);
      if vMediaId <> '' then
        vPart.Add('id', vMediaId)
      else if AMessage.Media.Url <> '' then
        vPart.Add('link', AMessage.Media.Url)
      else
        vPart.Add('id', AMessage.Media.Id);
      if vCaption then
        vPart.Add('caption', vText);
      if (vType = 'document') and (AMessage.Media.FileName <> '') then
        vPart.Add('filename', AMessage.Media.FileName);
      Post(vBody, vPair, AResult);
    finally
      vBody.Free;
    end;
    if (not AResult.Ok) or ((vText = '') or vCaption) and not vInteractive then
      Exit;
    if vCaption then
      vText := '';
  end;
  if not vInteractive then
  begin
    SendText(vTo, vText, Next, vPair, AResult);
    Exit;
  end;
  // the body of an interactive message holds 1024 characters: the rest goes before it
  vBodyText := vText;
  vPieces := TStringList.Create;
  try
    if Length(UnicodeString(vText)) > cInteractiveMax then
    begin
      RALChatSplitText(vText, cInteractiveMax, vPieces);
      for vIdx := 0 to vPieces.Count - 2 do
      begin
        SendText(vTo, StringRAL(vPieces[vIdx]), Next, vPair, AResult);
        if not AResult.Ok then
          Exit;
      end;
      vBodyText := StringRAL(vPieces[vPieces.Count - 1]);
    end;
  finally
    vPieces.Free;
  end;
  if vBodyText = '' then
    vBodyText := AMessage.List.Title;
  if vBodyText = '' then
    vBodyText := '...';
  vBody := NewBody(vTo, Next);
  try
    vBody.Add('type', 'interactive');
    vPart := TRALJSONObject.Create;
    vBody.Add('interactive', vPart);
    vAction := TRALJSONObject.Create;
    if vRowCount = 0 then
    begin
      // one link: a call-to-action button
      vPart.Add('type', 'cta_url');
      vReply := TRALJSONObject.Create;
      vPart.Add('body', vReply);
      vReply.Add('text', vBodyText);
      vPart.Add('action', vAction);
      vAction.Add('name', 'cta_url');
      vReply := TRALJSONObject.Create;
      vAction.Add('parameters', vReply);
      vReply.Add('display_text', Shorten(vLinks[0].Caption, 20));
      vReply.Add('url', vLinks[0].Url);
    end
    else if (Length(AMessage.List.Items) = 0) and (Length(vReplies) <= cReplyButtonsMax) then
    begin
      vPart.Add('type', 'button');
      vReply := TRALJSONObject.Create;
      vPart.Add('body', vReply);
      vReply.Add('text', vBodyText);
      vPart.Add('action', vAction);
      vArray := TRALJSONArray.Create;
      vAction.Add('buttons', vArray);
      for vIdx := 0 to High(vReplies) do
      begin
        if (vReplies[vIdx].Id = '') or (Length(vReplies[vIdx].Id) > 256) then
          raise ERALChatWhatsAppFinal.Create(emChatWaButtonId);
        vRow := TRALJSONObject.Create;
        vArray.Add(vRow);
        vRow.Add('type', 'reply');
        vReply := TRALJSONObject.Create;
        vRow.Add('reply', vReply);
        vReply.Add('id', vReplies[vIdx].Id);
        vReply.Add('title', Shorten(vReplies[vIdx].Caption, 20));
      end;
    end
    else
    begin
      vPart.Add('type', 'list');
      vReply := TRALJSONObject.Create;
      vPart.Add('body', vReply);
      vReply.Add('text', vBodyText);
      vPart.Add('action', vAction);
      if AMessage.List.ButtonText <> '' then
        vAction.Add('button', Shorten(AMessage.List.ButtonText, 20))
      else
        vAction.Add('button', Shorten(StringRAL(cmChatWaListButton), 20));
      vSections := TRALJSONArray.Create;
      vAction.Add('sections', vSections);
      vSection := TRALJSONObject.Create;
      vSections.Add(vSection);
      if AMessage.List.Title <> '' then
        vSection.Add('title', Shorten(AMessage.List.Title, 24));
      vRows := TRALJSONArray.Create;
      vSection.Add('rows', vRows);
      for vIdx := 0 to High(vReplies) do
      begin
        if (vReplies[vIdx].Id = '') or (Length(vReplies[vIdx].Id) > 200) then
          raise ERALChatWhatsAppFinal.Create(emChatWaRowId);
        vRow := TRALJSONObject.Create;
        vRows.Add(vRow);
        vRow.Add('id', vReplies[vIdx].Id);
        vRow.Add('title', Shorten(vReplies[vIdx].Caption, 24));
      end;
      for vIdx := 0 to High(AMessage.List.Items) do
      begin
        if (AMessage.List.Items[vIdx].Id = '') or (Length(AMessage.List.Items[vIdx].Id) > 200) then
          raise ERALChatWhatsAppFinal.Create(emChatWaRowId);
        vRow := TRALJSONObject.Create;
        vRows.Add(vRow);
        vRow.Add('id', AMessage.List.Items[vIdx].Id);
        vRow.Add('title', Shorten(AMessage.List.Items[vIdx].Title, 24));
        if AMessage.List.Items[vIdx].Description <> '' then
          vRow.Add('description', Shorten(AMessage.List.Items[vIdx].Description, 72));
      end;
    end;
    Post(vBody, vPair, AResult);
  finally
    vBody.Free;
  end;
end;

procedure TRALChatWhatsApp.SendText(const ATo, AText, AContext, APair: StringRAL;
  var AResult: TRALChatSendResult);
var
  vBody, vText: TRALJSONObject;
begin
  vBody := NewBody(ATo, AContext);
  try
    vBody.Add('type', 'text');
    vText := TRALJSONObject.Create;
    vBody.Add('text', vText);
    vText.Add('preview_url', (Pos('https://', string(AText)) > 0) or
      (Pos('http://', string(AText)) > 0));
    vText.Add('body', AText);
    Post(vBody, APair, AResult);
  finally
    vBody.Free;
  end;
end;

class function TRALChatWhatsApp.TemplateMessage(const ATo, AName, ALanguage: StringRAL;
  const AParams: array of StringRAL): TRALChatMessage;
var
  vRoot, vTemplate, vLanguage, vComponent, vParam: TRALJSONObject;
  vComponents, vParams: TRALJSONArray;
  vIdx: Integer;
begin
  Result := TRALChatMessage.Create;
  try
    Result.ChatId := ATo;
    vRoot := TRALJSONObject.Create;
    try
      vRoot.Add('type', 'template');
      vTemplate := TRALJSONObject.Create;
      vRoot.Add('template', vTemplate);
      vTemplate.Add('name', AName);
      vLanguage := TRALJSONObject.Create;
      vTemplate.Add('language', vLanguage);
      vLanguage.Add('code', ALanguage);
      if Length(AParams) > 0 then
      begin
        vComponents := TRALJSONArray.Create;
        vTemplate.Add('components', vComponents);
        vComponent := TRALJSONObject.Create;
        vComponents.Add(vComponent);
        vComponent.Add('type', 'body');
        vParams := TRALJSONArray.Create;
        vComponent.Add('parameters', vParams);
        for vIdx := 0 to High(AParams) do
        begin
          vParam := TRALJSONObject.Create;
          vParams.Add(vParam);
          vParam.Add('type', 'text');
          vParam.Add('text', AParams[vIdx]);
        end;
      end;
      Result.Raw := vRoot.ToJSON;
    finally
      vRoot.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TRALChatWhatsApp.TestToken(out AReport: StringRAL): Boolean;
var
  vReply: TRALChatGraphReply;
begin
  AReport := '';
  try
    Call(amGET, PhoneId + '?fields=display_phone_number,verified_name', nil, vReply);
  except
    on E: Exception do
    begin
      AReport := Hide(StringRAL(E.Message));
      Result := False;
      Exit;
    end;
  end;
  try
    Result := vReply.Ok;
    if Result then
      AReport := StringRAL(Format(cmChatWaTokenOk,
        [string(RALChatJStr(vReply.Root, 'display_phone_number')),
         string(RALChatJStr(vReply.Root, 'verified_name'))]))
    else
      AReport := Hide(StringRAL(Format(emChatMeApi, [string(FPhoneNumberId),
        string(vReply.Description), vReply.Code, vReply.Subcode, string(vReply.TraceId)])));
  finally
    vReply.Root.Free;
  end;
end;

function TRALChatWhatsApp.UploadMedia(AMedia: TRALChatMedia; out AMediaId: StringRAL;
  var AResult: TRALChatSendResult): Boolean;
var
  vFields: TRALJSONObject;
  vReply: TRALChatGraphReply;
begin
  AMediaId := '';
  vFields := TRALJSONObject.Create;
  try
    vFields.Add('messaging_product', 'whatsapp');
    if AMedia.MimeType <> '' then
      vFields.Add('type', AMedia.MimeType)
    else
      vFields.Add('type', 'application/octet-stream');
    Call(amPOST, PhoneId + '/media', nil, vReply, vFields, 'file', AMedia);
  finally
    vFields.Free;
  end;
  try
    FillResult('media', '', vReply, AResult);
    if AResult.Ok then
      AMediaId := RALChatJStr(vReply.Root, 'id');
  finally
    vReply.Root.Free;
  end;
  Result := AResult.Ok and (AMediaId <> '');
end;

end.
