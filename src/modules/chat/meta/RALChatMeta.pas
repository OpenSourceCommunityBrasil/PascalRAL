/// The Messenger and Instagram channel of the chat module on Meta's Graph API.
unit RALChatMeta;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools, RALJson,
  RALChatMessage, RALChatChannel, RALChatHttp, RALChatGraph;

type
  /// Which Meta platform a TRALChatMeta talks to.
  TRALChatMetaPlatform = (
    /// Read from the webhook: object page is Messenger, instagram is Instagram.
    mpAuto,
    /// Messenger (a Facebook Page).
    mpMessenger,
    /// Instagram (a professional account linked to the app).
    mpInstagram
  );

  { The Messenger and Instagram channel: receives text, attachments, quick replies and
    postbacks; sends text, quick replies (up to 13), attachments, typing_on and
    mark_seen through the Send API of the Page. }
  TRALChatMeta = class(TRALChatGraphChannel)
  private
    FDetected: TRALChatMetaPlatform;
    FHumanAgent: Boolean;
    FPageId: StringRAL;
    FPlatform: TRALChatMetaPlatform;

    /// The attachment type of a message with media.
    function AttachmentType(AMessage: TRALChatMessage): StringRAL;
    /// Downloads a received attachment, from Meta hosts only.
    function LoadMedia(AMedia: TRALChatMedia): TStream;
    /// A body to ARecipient with the messaging type of the channel.
    function NewBody(const ARecipient: StringRAL): TRALJSONObject;
    /// Reads one messaging event into AMessages.
    procedure ParseEvent(AEvent: TRALJSONObject; AMessages: TRALChatMessageList);
    /// Posts a body (or a multipart with AMedia) and fills AResult.
    procedure Post(ABody: TRALJSONObject; AMedia: TRALChatMedia; var AResult: TRALChatSendResult);
    /// The quick replies of AMessage, or nil.
    function QuickReplies(AMessage: TRALChatMessage): TRALJSONArray;
    { The outgoing Raw of AMessage as a Send API message (text or attachment, no mid);
      nil otherwise. The caller frees it. }
    function RawMessage(AMessage: TRALChatMessage): TRALJSONObject;
    /// Posts the sender action AAction (typing_on, mark_seen) to ARecipient.
    procedure SenderAction(const ARecipient, AAction: StringRAL; var AResult: TRALChatSendResult);
    /// Sends AMessage, in as many calls as its parts need.
    procedure SendMessage(AMessage: TRALChatMessage; var AResult: TRALChatSendResult);
    /// The path of the Send API: the Page's messages, or me/messages without PageId.
    function SendPath: StringRAL;
    procedure SetPlatform(AValue: TRALChatMetaPlatform);
    /// Sets the text limit of the platform in use.
    procedure UpdateLimits;
  protected
    function SubscribeFields: StringRAL; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId: StringRAL; out AResult: TRALChatSendResult); override;
    /// Marks the conversation with ARecipient as seen (mark_seen); synchronous.
    function MarkSeen(const ARecipient: StringRAL): TRALChatSendResult;
    procedure ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList); override;
    function TestToken(out AReport: StringRAL): Boolean; override;

    /// The platform the webhook showed when Platform is mpAuto; mpAuto before any.
    property DetectedPlatform: TRALChatMetaPlatform read FDetected;
  published
    { Sends with messaging_type MESSAGE_TAG and the HUMAN_AGENT tag (a person answering,
      up to 7 days after the user wrote) instead of RESPONSE. }
    property HumanAgent: Boolean read FHumanAgent write FHumanAgent default False;
    { The id of the Page (Messenger) or of the Instagram account the channel sends
      from; empty sends to me/messages, the owner of the token. }
    property PageId: StringRAL read FPageId write FPageId;
    /// The platform; mpAuto reads it from the webhook.
    property Platform: TRALChatMetaPlatform read FPlatform write SetPlatform default mpAuto;
  end;

implementation

type
  /// An exception of the channel that is not worth another attempt.
  ERALChatMetaFinal = class(ERALChatError);

const
  /// The most quick replies of a message.
  cQuickRepliesMax = 13;

/// A number as JSON writes it, whatever the locale.
function NumberText(AValue: Double): string;
var
  vText: ShortString;
begin
  Str(AValue:0:6, vText);
  Result := string(vText);
end;

{ TRALChatMeta }

constructor TRALChatMeta.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ChannelName := 'meta';
  Capabilities.Supported := [ccButtons, ccLists, ccTyping, ccReadReceipts, ccMedia];
  Capabilities.MaxButtons := cQuickRepliesMax;
  Capabilities.MaxListItems := cQuickRepliesMax;
  Capabilities.MaxMediaBytes := 26214400;
  // the Send API of a Page takes a few hundred calls a second; a person, about one
  Rates.Total := 200;
  Rates.TotalBurst := 50;
  UpdateLimits;
end;

function TRALChatMeta.AttachmentType(AMessage: TRALChatMessage): StringRAL;
begin
  case AMessage.Kind of
    ckImage, ckSticker: Result := 'image';
    ckAudio: Result := 'audio';
    ckVideo: Result := 'video';
    ckDocument: Result := 'file';
  else
    if Copy(string(AMessage.Media.MimeType), 1, 6) = 'image/' then
      Result := 'image'
    else if Copy(string(AMessage.Media.MimeType), 1, 6) = 'audio/' then
      Result := 'audio'
    else if Copy(string(AMessage.Media.MimeType), 1, 6) = 'video/' then
      Result := 'video'
    else
      Result := 'file';
  end;
end;

function TRALChatMeta.MarkSeen(const ARecipient: StringRAL): TRALChatSendResult;
begin
  ResetResult(Result);
  try
    SenderAction(ARecipient, 'mark_seen', Result);
  except
    on E: Exception do
    begin
      Result.Ok := False;
      Result.Transient := not (E is ERALChatError);
      Result.ErrorText := Hide(StringRAL(E.Message));
    end;
  end;
end;

procedure TRALChatMeta.Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId: StringRAL; out AResult: TRALChatSendResult);
begin
  ResetResult(AResult);
  try
    case AKind of
      okTyping:
        SenderAction(AMessage.Chat.Id, 'typing_on', AResult);
      okEdit:
        raise ERALChatMetaFinal.CreateFmt(emChatNoCapability, [string(ChannelName), 'edit']);
      okDelete:
        raise ERALChatMetaFinal.CreateFmt(emChatNoCapability, [string(ChannelName), 'delete']);
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

function TRALChatMeta.LoadMedia(AMedia: TRALChatMedia): TStream;
begin
  if (AMedia.Size > 0) and (MaxDownloadSize > 0) and (AMedia.Size > MaxDownloadSize) then
    raise ERALChatError.CreateFmt(emChatMeTooBig, [MaxDownloadSize]);
  // the address is a signed CDN one: the token is not sent there
  Result := DownloadFrom(AMedia.Url, False);
end;

function TRALChatMeta.NewBody(const ARecipient: StringRAL): TRALJSONObject;
var
  vRecipient: TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  vRecipient := TRALJSONObject.Create;
  Result.Add('recipient', vRecipient);
  vRecipient.Add('id', ARecipient);
  if FHumanAgent then
  begin
    Result.Add('messaging_type', 'MESSAGE_TAG');
    Result.Add('tag', 'HUMAN_AGENT');
  end
  else
    Result.Add('messaging_type', 'RESPONSE');
end;

procedure TRALChatMeta.ParseEvent(AEvent: TRALJSONObject; AMessages: TRALChatMessageList);
var
  vSender, vType: StringRAL;
  vMsg, vPostback, vPayload, vCoordinates: TRALJSONObject;
  vAttachments: TRALJSONArray;
  vMessage: TRALChatMessage;
  vInfo: TRALChatInfo;
  vUser: TRALChatUser;
  vLocation: TRALChatLocation;
  vLoader: TRALChatMediaLoader;
  vCode: Integer;
  vKeep: Boolean;
begin
  vSender := RALChatJStr(RALChatJObj(AEvent, 'sender'), 'id');
  vMsg := RALChatJObj(AEvent, 'message');
  vPostback := RALChatJObj(AEvent, 'postback');
  if (vSender = '') or ((vMsg = nil) and (vPostback = nil)) then
    Exit;
  // the Page's own messages come back as echoes: never delivered
  if (FPageId <> '') and (vSender = FPageId) then
    Exit;
  if (vMsg <> nil) and ((RALChatJStr(vMsg, 'is_echo') = 'true') or
     (RALChatJStr(vMsg, 'is_deleted') = 'true')) then
    Exit;
  vMessage := TRALChatMessage.Create;
  try
    vMessage.Timestamp := RALChatJInt(AEvent, 'timestamp', 0) div 1000;
    vInfo.Id := vSender;
    vInfo.ChatType := cctPrivate;
    vInfo.Title := '';
    vMessage.Chat := vInfo;
    vUser.Id := vSender;
    vUser.Name := vSender;
    vUser.Phone := '';
    vMessage.From := vUser;
    if vPostback <> nil then
    begin
      vMessage.Kind := ckCallback;
      vMessage.Id := RALChatJStr(vPostback, 'mid');
      vMessage.CallbackData := RALChatJStr(vPostback, 'payload');
      vMessage.Text := RALChatJStr(vPostback, 'title');
      if vMessage.Id <> '' then
        vMessage.EventId := vMessage.Id
      else
        vMessage.EventId := 'pb' + vSender + ':' + RALChatJStr(AEvent, 'timestamp');
    end
    else
    begin
      vMessage.Id := RALChatJStr(vMsg, 'mid');
      vMessage.EventId := vMessage.Id;
      vMessage.Text := RALChatJStr(vMsg, 'text');
      if RALChatJStr(RALChatJObj(vMsg, 'reply_to'), 'mid') <> '' then
        vMessage.ReplyTo.Id := RALChatJStr(RALChatJObj(vMsg, 'reply_to'), 'mid');
      vKeep := vMessage.Text <> '';
      if RALChatJObj(vMsg, 'quick_reply') <> nil then
      begin
        vMessage.Kind := ckCallback;
        vMessage.CallbackData := RALChatJStr(RALChatJObj(vMsg, 'quick_reply'), 'payload');
        vKeep := True;
      end
      else
      begin
        vAttachments := RALChatJArr(vMsg, 'attachments');
        if (vAttachments <> nil) and (vAttachments.Count > 0) and
           (vAttachments[0].Kind = rjkObject) then
        begin
          vType := RALChatJStr(vAttachments[0], 'type');
          vPayload := RALChatJObj(vAttachments[0], 'payload');
          if vType = 'location' then
          begin
            vCoordinates := RALChatJObj(vPayload, 'coordinates');
            vMessage.Kind := ckLocation;
            vLocation.Valid := True;
            Val(string(RALChatJStr(vCoordinates, 'lat')), vLocation.Latitude, vCode);
            if vCode <> 0 then
              vLocation.Latitude := 0;
            Val(string(RALChatJStr(vCoordinates, 'long')), vLocation.Longitude, vCode);
            if vCode <> 0 then
              vLocation.Longitude := 0;
            vLocation.Title := RALChatJStr(vAttachments[0], 'title');
            vMessage.Location := vLocation;
            vKeep := True;
          end
          else if ((vType = 'image') or (vType = 'audio') or (vType = 'video') or
            (vType = 'file')) and (RALChatJStr(vPayload, 'url') <> '') then
          begin
            if RALChatJStr(vPayload, 'sticker_id') <> '' then
              vMessage.Kind := ckSticker
            else if vType = 'image' then
              vMessage.Kind := ckImage
            else if vType = 'audio' then
              vMessage.Kind := ckAudio
            else if vType = 'video' then
              vMessage.Kind := ckVideo
            else
              vMessage.Kind := ckDocument;
            vMessage.Media.Url := RALChatJStr(vPayload, 'url');
            vMessage.Media.Id := RALChatJStr(vPayload, 'attachment_id');
            vMessage.Media.Size := -1;
            vLoader := LoadMedia;
            vMessage.Media.Loader := vLoader;
            vKeep := True;
          end;
        end;
      end;
      if (not vKeep) or (vMessage.Id = '') then
        Exit;
    end;
    if KeepRaw then
      vMessage.Raw := AEvent.ToJSON;
    AMessages.AddMessage(vMessage);
    vMessage := nil;
  finally
    vMessage.Free;
  end;
end;

procedure TRALChatMeta.ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList);
var
  vRoot: TRALJSONObject;
  vObject: StringRAL;
  vEntries, vEvents: TRALJSONArray;
  vEntry, vIdx: Integer;
  vSeen: TRALChatMetaPlatform;
begin
  vRoot := RALChatParseObject(ABody);
  if vRoot = nil then
    raise ERALChatError.CreateFmt(emChatBadWebhook, [string(ChannelName)]);
  try
    vObject := RALChatJStr(vRoot, 'object');
    if vObject = 'page' then
      vSeen := mpMessenger
    else if vObject = 'instagram' then
      vSeen := mpInstagram
    else
      Exit;
    if (FPlatform = mpAuto) and (FDetected <> vSeen) then
    begin
      FDetected := vSeen;
      UpdateLimits;
    end;
    vEntries := RALChatJArr(vRoot, 'entry');
    if vEntries = nil then
      Exit;
    for vEntry := 0 to vEntries.Count - 1 do
    begin
      vEvents := RALChatJArr(vEntries[vEntry], 'messaging');
      if vEvents = nil then
        Continue;
      for vIdx := 0 to vEvents.Count - 1 do
        if vEvents[vIdx].Kind = rjkObject then
          ParseEvent(TRALJSONObject(vEvents[vIdx]), AMessages);
    end;
  finally
    vRoot.Free;
  end;
end;

procedure TRALChatMeta.Post(ABody: TRALJSONObject; AMedia: TRALChatMedia;
  var AResult: TRALChatSendResult);
var
  vReply: TRALChatGraphReply;
begin
  if AMedia <> nil then
    Call(amPOST, SendPath, nil, vReply, ABody, 'filedata', AMedia)
  else
    Call(amPOST, SendPath, ABody, vReply);
  try
    FillResult('messages', '', vReply, AResult);
    if AResult.Ok then
      AResult.MessageId := RALChatJStr(vReply.Root, 'message_id');
  finally
    vReply.Root.Free;
  end;
end;

function TRALChatMeta.QuickReplies(AMessage: TRALChatMessage): TRALJSONArray;
var
  vIdx, vCount: Integer;
  vItem: TRALJSONObject;

  procedure AddReply(const AId, ATitle: StringRAL);
  begin
    if (AId = '') or (Length(AId) > 1000) then
      raise ERALChatMetaFinal.Create(emChatMeQuickReply);
    vItem := TRALJSONObject.Create;
    Result.Add(vItem);
    vItem.Add('content_type', 'text');
    vItem.Add('title', Shorten(ATitle, 20));
    vItem.Add('payload', AId);
  end;

begin
  Result := nil;
  vCount := Length(AMessage.List.Items);
  for vIdx := 0 to High(AMessage.Buttons) do
    if AMessage.Buttons[vIdx].Url = '' then
      Inc(vCount);
  if vCount = 0 then
    Exit;
  if vCount > cQuickRepliesMax then
    raise ERALChatMetaFinal.CreateFmt(emChatMeTooManyChoices, [vCount, cQuickRepliesMax]);
  Result := TRALJSONArray.Create;
  try
    for vIdx := 0 to High(AMessage.Buttons) do
      if AMessage.Buttons[vIdx].Url = '' then
        AddReply(AMessage.Buttons[vIdx].Id, AMessage.Buttons[vIdx].Caption);
    for vIdx := 0 to High(AMessage.List.Items) do
      AddReply(AMessage.List.Items[vIdx].Id, AMessage.List.Items[vIdx].Title);
  except
    Result.Free;
    raise;
  end;
end;

function TRALChatMeta.RawMessage(AMessage: TRALChatMessage): TRALJSONObject;
begin
  Result := nil;
  if (AMessage.Raw = '') or (AMessage.Raw[1] <> '{') then
    Exit;
  Result := RALChatParseObject(AMessage.Raw);
  if Result = nil then
    Exit;
  if (Result.Find('mid') <> nil) or (Result.Find('sender') <> nil) or
     ((Result.Find('attachment') = nil) and (Result.Find('text') = nil)) then
    FreeAndNil(Result);
end;

procedure TRALChatMeta.SendMessage(AMessage: TRALChatMessage; var AResult: TRALChatSendResult);
var
  vTo, vText, vType, vLine: StringRAL;
  vBody, vMessage, vAttachment, vPayload: TRALJSONObject;
  vReplies: TRALJSONArray;
  vIdx: Integer;
  vUpload: TRALChatMedia;
begin
  vTo := AMessage.Chat.Id;
  vMessage := RawMessage(AMessage);
  if vMessage <> nil then
  begin
    vBody := NewBody(vTo);
    try
      vBody.Add('message', vMessage);
      Post(vBody, nil, AResult);
    finally
      vBody.Free;
    end;
    Exit;
  end;
  // links, a location and a contact card have no message of their own: they become text
  vText := AMessage.Text;
  for vIdx := 0 to High(AMessage.Buttons) do
    if AMessage.Buttons[vIdx].Url <> '' then
    begin
      vLine := AMessage.Buttons[vIdx].Caption + ': ' + AMessage.Buttons[vIdx].Url;
      if vText <> '' then
        vText := vText + #10;
      vText := vText + vLine;
    end;
  if (AMessage.Kind = ckLocation) and AMessage.Location.Valid then
  begin
    vLine := StringRAL(NumberText(AMessage.Location.Latitude) + ',' +
      NumberText(AMessage.Location.Longitude));
    if AMessage.Location.Title <> '' then
      vLine := AMessage.Location.Title + ' (' + vLine + ')';
    if vText <> '' then
      vText := vText + #10;
    vText := vText + vLine;
  end;
  if (AMessage.Kind = ckContact) and AMessage.Contact.Valid then
  begin
    if vText <> '' then
      vText := vText + #10;
    vText := vText + AMessage.Contact.Name + ': ' + AMessage.Contact.Phone;
  end;
  vReplies := QuickReplies(AMessage);
  try
    if AMessage.HasMedia then
    begin
      if (AMessage.Media.Data = nil) and (AMessage.Media.Url = '') and (AMessage.Media.Id = '') then
        raise ERALChatMetaFinal.Create(emChatNoMedia);
      if (AMessage.Media.Data <> nil) and (AMessage.Media.Data.Size > Capabilities.MaxMediaBytes) then
        raise ERALChatMetaFinal.CreateFmt(emChatMeTooBig, [Capabilities.MaxMediaBytes]);
      // the text goes first: an attachment carries no text
      if vText <> '' then
      begin
        vBody := NewBody(vTo);
        try
          vMessage := TRALJSONObject.Create;
          vBody.Add('message', vMessage);
          vMessage.Add('text', vText);
          Post(vBody, nil, AResult);
        finally
          vBody.Free;
        end;
        if not AResult.Ok then
          Exit;
      end;
      vType := AttachmentType(AMessage);
      vBody := NewBody(vTo);
      try
        vMessage := TRALJSONObject.Create;
        vBody.Add('message', vMessage);
        vAttachment := TRALJSONObject.Create;
        vMessage.Add('attachment', vAttachment);
        vAttachment.Add('type', vType);
        vPayload := TRALJSONObject.Create;
        vAttachment.Add('payload', vPayload);
        vUpload := nil;
        if AMessage.Media.Data <> nil then
        begin
          vPayload.Add('is_reusable', True);
          vUpload := AMessage.Media;
        end
        else if AMessage.Media.Url <> '' then
        begin
          vPayload.Add('url', AMessage.Media.Url);
          vPayload.Add('is_reusable', True);
        end
        else
          vPayload.Add('attachment_id', AMessage.Media.Id);
        if vReplies <> nil then
        begin
          vMessage.Add('quick_replies', vReplies);
          vReplies := nil;
        end;
        Post(vBody, vUpload, AResult);
      finally
        vBody.Free;
      end;
      Exit;
    end;
    vBody := NewBody(vTo);
    try
      vMessage := TRALJSONObject.Create;
      vBody.Add('message', vMessage);
      vMessage.Add('text', vText);
      if vReplies <> nil then
      begin
        vMessage.Add('quick_replies', vReplies);
        vReplies := nil;
      end;
      Post(vBody, nil, AResult);
    finally
      vBody.Free;
    end;
  finally
    vReplies.Free;
  end;
end;

procedure TRALChatMeta.SenderAction(const ARecipient, AAction: StringRAL;
  var AResult: TRALChatSendResult);
var
  vBody, vRecipient: TRALJSONObject;
begin
  vBody := TRALJSONObject.Create;
  try
    vRecipient := TRALJSONObject.Create;
    vBody.Add('recipient', vRecipient);
    vRecipient.Add('id', ARecipient);
    vBody.Add('sender_action', AAction);
    Post(vBody, nil, AResult);
  finally
    vBody.Free;
  end;
end;

function TRALChatMeta.SendPath: StringRAL;
begin
  if FPageId <> '' then
    Result := FPageId + '/messages'
  else
    Result := 'me/messages';
end;

procedure TRALChatMeta.SetPlatform(AValue: TRALChatMetaPlatform);
begin
  FPlatform := AValue;
  UpdateLimits;
end;

function TRALChatMeta.SubscribeFields: StringRAL;
begin
  Result := 'messages, messaging_postbacks';
end;

function TRALChatMeta.TestToken(out AReport: StringRAL): Boolean;
var
  vReply: TRALChatGraphReply;
begin
  AReport := '';
  try
    Call(amGET, 'me?fields=id,name', nil, vReply);
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
      AReport := StringRAL(Format(cmChatMeTokenOk, [string(RALChatJStr(vReply.Root, 'name')),
        string(RALChatJStr(vReply.Root, 'id'))]))
    else
      AReport := Hide(StringRAL(Format(emChatMeApi, ['me', string(vReply.Description),
        vReply.Code, vReply.Subcode, string(vReply.TraceId)])));
  finally
    vReply.Root.Free;
  end;
end;

procedure TRALChatMeta.UpdateLimits;
var
  vPlatform: TRALChatMetaPlatform;
begin
  vPlatform := FPlatform;
  if vPlatform = mpAuto then
    vPlatform := FDetected;
  // Messenger takes 2000 characters, Instagram 1000; unknown takes the lower
  if vPlatform = mpMessenger then
    Capabilities.MaxText := 2000
  else
    Capabilities.MaxText := 1000;
end;

end.
