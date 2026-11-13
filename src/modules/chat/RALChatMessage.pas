{ The one message model every chat channel speaks: who wrote, where, what kind,
  the text with simple formatting, media on demand, buttons and the rest, plus
  the capabilities of a channel and the fallback that turns what a channel
  cannot show into plain text. }
unit RALChatMessage;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs,
  RALTypes, RALConsts, RALExceptions;

type
  /// Raised by the chat classes.
  ERALChatError = class(ERALException);

  /// What a message carries.
  TRALChatKind = (
    /// Plain text.
    ckText,
    /// A picture.
    ckImage,
    /// A voice note or audio file.
    ckAudio,
    /// A video.
    ckVideo,
    /// Any other file.
    ckDocument,
    /// A sticker.
    ckSticker,
    /// A position on the map.
    ckLocation,
    /// A contact card.
    ckContact,
    /// A poll.
    ckPoll,
    /// A button (or list item) the user chose; the data is in CallbackData.
    ckCallback,
    /// A text that starts with a slash: Command and Arguments are filled.
    ckCommand
  );

  /// What kind of conversation a chat is.
  TRALChatType = (
    /// One user and the bot.
    cctPrivate,
    /// A group of users.
    cctGroup,
    /// A broadcast channel.
    cctChannel
  );

  /// What a channel can do besides sending text.
  TRALChatCapability = (
    /// Quick-reply buttons under a message.
    ccButtons,
    /// A list the user picks an item from.
    ccLists,
    /// A custom keyboard.
    ccKeyboard,
    /// Editing a sent message.
    ccEdit,
    /// Deleting a sent message.
    ccDelete,
    /// Reactions to a message.
    ccReactions,
    /// The "typing" indicator.
    ccTyping,
    /// Read receipts.
    ccReadReceipts,
    /// Polls.
    ccPolls,
    /// Approved message templates.
    ccTemplates,
    /// Pictures, audio, video and files.
    ccMedia
  );

  /// A set of capabilities.
  TRALChatCapabilitySet = set of TRALChatCapability;

  /// What to do with a feature the channel does not have.
  TRALChatFallback = (
    { Show it as text: buttons become a numbered list, and a numeric answer comes back
      as the button. }
    cfDegrade,
    /// Fail with ERALChatError.
    cfRaise
  );

  /// The text dialect of a channel.
  TRALChatDialect = (
    /// No formatting: the markup is dropped.
    cdPlain,
    /// Telegram HTML.
    cdTelegramHTML,
    /// Telegram MarkdownV2.
    cdTelegramMarkdownV2,
    /// WhatsApp.
    cdWhatsApp,
    /// Slack mrkdwn.
    cdSlack,
    /// Discord markdown.
    cdDiscord
  );

  /// How the text of a message is read.
  TRALChatTextFormat = (
    /// Every character is literal (a channel still escapes what its dialect needs).
    tfPlain,
    { Simple markup: *bold*, _italic_, `code`, [label](url); a backslash makes the next
      character literal. }
    tfMarkup
  );

  /// A conversation.
  TRALChatInfo = record
    /// The channel's id of the conversation.
    Id: StringRAL;
    /// Private, group or channel.
    ChatType: TRALChatType;
    /// The title of a group or channel.
    Title: StringRAL;
  end;

  /// A person.
  TRALChatUser = record
    /// The channel's id of the user.
    Id: StringRAL;
    /// The display name.
    Name: StringRAL;
    /// The phone number, when the channel tells it.
    Phone: StringRAL;
  end;

  /// A position.
  TRALChatLocation = record
    /// Whether the message has a location.
    Valid: Boolean;
    /// Degrees north.
    Latitude: Double;
    /// Degrees east.
    Longitude: Double;
    /// A name or address.
    Title: StringRAL;
  end;

  /// A contact card.
  TRALChatContact = record
    /// Whether the message has a contact.
    Valid: Boolean;
    /// The person's name.
    Name: StringRAL;
    /// The phone number.
    Phone: StringRAL;
  end;

  /// A button, a quick reply.
  TRALChatButton = record
    /// What comes back (CallbackData) when it is chosen.
    Id: StringRAL;
    /// What the user reads.
    Caption: StringRAL;
    /// When not empty, the button opens this address instead of answering.
    Url: StringRAL;
  end;

  /// Buttons in a row.
  TRALChatButtons = array of TRALChatButton;

  /// Rows of buttons: a custom keyboard.
  TRALChatKeyboard = array of TRALChatButtons;

  /// An item of a list.
  TRALChatListItem = record
    /// What comes back when it is chosen.
    Id: StringRAL;
    /// The title.
    Title: StringRAL;
    /// A second line.
    Description: StringRAL;
  end;

  /// A list the user picks from.
  TRALChatList = record
    /// The title above the list.
    Title: StringRAL;
    /// The label of the button that opens the list.
    ButtonText: StringRAL;
    /// The items.
    Items: array of TRALChatListItem;
  end;

  TRALChatMedia = class;

  /// Opens the data of received media; set by the channel that made the message.
  TRALChatMediaLoader = function(AMedia: TRALChatMedia): TStream of object;

  { A picture, audio, video or file: where it is and what it is. A received one is
    opened on demand (Open); one to send carries Data. }
  TRALChatMedia = class
  private
    FData: TStream;
    FFileName: StringRAL;
    FId: StringRAL;
    FLoader: TRALChatMediaLoader;
    FMimeType: StringRAL;
    FOwnsData: Boolean;
    FSize: Int64RAL;
    FUrl: StringRAL;

    procedure SetData(AValue: TStream);
  public
    destructor Destroy; override;

    /// Copies AMedia into this one (the data is copied into memory).
    procedure Assign(AMedia: TRALChatMedia);
    { The data as a stream the caller frees: for a received file the channel's
      download (only from the hosts of the channel), for a file to send a copy
      of Data. Raises when there is nothing to open. }
    function Open: TStream;

    /// The data of a file to send (not owned unless OwnsData).
    property Data: TStream read FData write SetData;
    /// The file name.
    property FileName: StringRAL read FFileName write FFileName;
    /// The channel's id of the file.
    property Id: StringRAL read FId write FId;
    /// What downloads a received file; set by the channel.
    property Loader: TRALChatMediaLoader read FLoader write FLoader;
    /// The media type.
    property MimeType: StringRAL read FMimeType write FMimeType;
    /// Whether the media frees Data.
    property OwnsData: Boolean read FOwnsData write FOwnsData;
    /// The size in bytes, -1 when unknown.
    property Size: Int64RAL read FSize write FSize;
    /// A public address of the file, when there is one (never a private one).
    property Url: StringRAL read FUrl write FUrl;
  end;

  /// A message, received or to send.
  TRALChatMessage = class
  private
    FButtons: TRALChatButtons;
    FCallbackData: StringRAL;
    FChannelName: StringRAL;
    FChat: TRALChatInfo;
    FCommand: StringRAL;
    FCommandArgs: StringRAL;
    FContact: TRALChatContact;
    FEventId: StringRAL;
    FFrom: TRALChatUser;
    FId: StringRAL;
    FKeyboard: TRALChatKeyboard;
    FKind: TRALChatKind;
    FList: TRALChatList;
    FLocation: TRALChatLocation;
    /// The media, created on first read.
    FMedia: TRALChatMedia;
    FRaw: StringRAL;
    /// The message answered, created on first read.
    FReplyTo: TRALChatMessage;
    FText: StringRAL;
    FTextFormat: TRALChatTextFormat;
    FTimestamp: Int64RAL;

    function GetChatId: StringRAL;
    function GetMedia: TRALChatMedia;
    function GetReplyTo: TRALChatMessage;
    procedure SetChatId(const AValue: StringRAL);
  public
    destructor Destroy; override;

    /// Adds a button under the message.
    procedure AddButton(const AId, ACaption: StringRAL);
    /// Adds a keyboard row and returns its index.
    function AddKeyboardRow(const ARow: TRALChatButtons): Integer;
    /// Adds a button that opens an address.
    procedure AddLink(const ACaption, AUrl: StringRAL);
    /// Adds an item to the list.
    procedure AddListItem(const AId, ATitle: StringRAL; const ADescription: StringRAL = '');
    /// A copy (media data included, copied into memory).
    function Clone: TRALChatMessage;
    /// Whether it has buttons, a list or a keyboard.
    function HasChoices: Boolean;
    /// Whether Media is set, without creating it.
    function HasMedia: Boolean;
    /// Whether ReplyTo is set, without creating it.
    function HasReplyTo: Boolean;
    { Turns what ACaps cannot show into text, or raises (AFallback = cfRaise). The ids
      of the buttons the numbers stand for come back in ADegraded, in order; empty when
      nothing was degraded. }
    procedure Prepare(ACaps: TRALChatCapabilitySet; AMaxButtons: Integer;
      AFallback: TRALChatFallback; const AChannel: StringRAL;
      out ADegraded: TStringList);

    /// Buttons under the message.
    property Buttons: TRALChatButtons read FButtons write FButtons;
    /// For a button answer, what the button's id was.
    property CallbackData: StringRAL read FCallbackData write FCallbackData;
    /// The name of the channel that carries the message.
    property ChannelName: StringRAL read FChannelName write FChannelName;
    /// The conversation.
    property Chat: TRALChatInfo read FChat write FChat;
    /// The id of the conversation (Chat.Id, which a property of a record cannot set).
    property ChatId: StringRAL read GetChatId write SetChatId;
    /// For a command, its name without the slash and the bot's name.
    property Command: StringRAL read FCommand write FCommand;
    /// For a command, what follows it.
    property CommandArgs: StringRAL read FCommandArgs write FCommandArgs;
    /// A contact card.
    property Contact: TRALChatContact read FContact write FContact;
    { The id the platform gave the event that brought the message (what a duplicate
      delivery repeats); Id when empty. }
    property EventId: StringRAL read FEventId write FEventId;
    /// Who wrote it.
    property From: TRALChatUser read FFrom write FFrom;
    /// The channel's id of the message.
    property Id: StringRAL read FId write FId;
    /// A custom keyboard.
    property Keyboard: TRALChatKeyboard read FKeyboard write FKeyboard;
    /// What the message is.
    property Kind: TRALChatKind read FKind write FKind;
    /// A list to pick from.
    property List: TRALChatList read FList write FList;
    /// A position.
    property Location: TRALChatLocation read FLocation write FLocation;
    /// The file, created on first read.
    property Media: TRALChatMedia read GetMedia;
    { The original JSON of the platform, for what the model does not cover; kept
      only when the channel is asked to. }
    property Raw: StringRAL read FRaw write FRaw;
    /// The message this one answers, created on first read.
    property ReplyTo: TRALChatMessage read GetReplyTo;
    /// The text.
    property Text: StringRAL read FText write FText;
    /// How Text is read.
    property TextFormat: TRALChatTextFormat read FTextFormat write FTextFormat;
    /// When it was sent, Unix seconds (0 when unknown).
    property Timestamp: Int64RAL read FTimestamp write FTimestamp;
  end;

  /// A list of messages that owns them.
  TRALChatMessageList = class(TObjectList)
  private
    function GetItem(AIndex: Integer): TRALChatMessage;
  public
    /// Creates a list that owns its messages.
    constructor Create;

    /// Adds a message, which the list now owns.
    function AddMessage(AMessage: TRALChatMessage): Integer;

    /// The message at AIndex.
    property Messages[AIndex: Integer]: TRALChatMessage read GetItem; default;
  end;

  { What a channel can do and its size limits; a channel sets it in its constructor
    to what its platform allows. }
  TRALChatCapabilities = class(TPersistent)
  private
    FMaxButtons: Integer;
    FMaxMediaBytes: Int64RAL;
    FMaxText: Integer;
    FSupported: TRALChatCapabilitySet;
  public
    constructor Create;

    /// Copies another set of capabilities.
    procedure Assign(Source: TPersistent); override;
    /// Whether ACapability is there.
    function Supports(ACapability: TRALChatCapability): Boolean;

    /// The most buttons a message may have; 0 = no limit.
    property MaxButtons: Integer read FMaxButtons write FMaxButtons;
    /// The largest file a message may carry, in bytes; 0 = unknown.
    property MaxMediaBytes: Int64RAL read FMaxMediaBytes write FMaxMediaBytes;
    /// The longest text of a message, in bytes; 0 = no limit.
    property MaxText: Integer read FMaxText write FMaxText;
    /// What the channel can do.
    property Supported: TRALChatCapabilitySet read FSupported write FSupported;
  end;

/// Converts AText to ADialect; AMarkup says whether the simple markup is read.
function RALChatConvertText(const AText: StringRAL; ADialect: TRALChatDialect;
  AMarkup: Boolean): StringRAL;
/// Cuts AText in pieces of at most AMax bytes at a line, a space or a UTF-8 boundary.
procedure RALChatSplitText(const AText: StringRAL; AMax: Integer; APieces: TStrings);
/// Whether AText is a whole number from 1 up (a degraded choice); AValue is its value.
function RALChatIsChoice(const AText: StringRAL; out AValue: Integer): Boolean;

implementation

{ TRALChatMedia }

destructor TRALChatMedia.Destroy;
begin
  SetData(nil);
  inherited Destroy;
end;

procedure TRALChatMedia.Assign(AMedia: TRALChatMedia);
var
  vCopy: TMemoryStream;
begin
  FFileName := AMedia.FFileName;
  FId := AMedia.FId;
  FLoader := AMedia.FLoader;
  FMimeType := AMedia.FMimeType;
  FSize := AMedia.FSize;
  FUrl := AMedia.FUrl;
  SetData(nil);
  if AMedia.FData <> nil then
  begin
    vCopy := TMemoryStream.Create;
    AMedia.FData.Position := 0;
    vCopy.CopyFrom(AMedia.FData, 0);
    vCopy.Position := 0;
    AMedia.FData.Position := 0;
    FData := vCopy;
    FOwnsData := True;
  end;
end;

function TRALChatMedia.Open: TStream;
begin
  Result := nil;
  if FData <> nil then
  begin
    Result := TMemoryStream.Create;
    try
      FData.Position := 0;
      Result.CopyFrom(FData, 0);
      Result.Position := 0;
      FData.Position := 0;
    except
      Result.Free;
      raise;
    end;
  end
  else if Assigned(FLoader) then
    Result := FLoader(Self);
  if Result = nil then
    raise ERALChatError.Create(emChatNoMedia);
end;

procedure TRALChatMedia.SetData(AValue: TStream);
begin
  if FData = AValue then
    Exit;
  if FOwnsData then
    FData.Free;
  FData := AValue;
  if AValue = nil then
    FOwnsData := False;
end;

{ TRALChatMessage }

destructor TRALChatMessage.Destroy;
begin
  FMedia.Free;
  FReplyTo.Free;
  inherited Destroy;
end;

procedure TRALChatMessage.AddButton(const AId, ACaption: StringRAL);
begin
  SetLength(FButtons, Length(FButtons) + 1);
  FButtons[High(FButtons)].Id := AId;
  FButtons[High(FButtons)].Caption := ACaption;
  FButtons[High(FButtons)].Url := '';
end;

procedure TRALChatMessage.AddLink(const ACaption, AUrl: StringRAL);
begin
  SetLength(FButtons, Length(FButtons) + 1);
  FButtons[High(FButtons)].Id := '';
  FButtons[High(FButtons)].Caption := ACaption;
  FButtons[High(FButtons)].Url := AUrl;
end;

procedure TRALChatMessage.AddListItem(const AId, ATitle, ADescription: StringRAL);
begin
  SetLength(FList.Items, Length(FList.Items) + 1);
  FList.Items[High(FList.Items)].Id := AId;
  FList.Items[High(FList.Items)].Title := ATitle;
  FList.Items[High(FList.Items)].Description := ADescription;
end;

function TRALChatMessage.AddKeyboardRow(const ARow: TRALChatButtons): Integer;
begin
  SetLength(FKeyboard, Length(FKeyboard) + 1);
  FKeyboard[High(FKeyboard)] := Copy(ARow, 0, Length(ARow));
  Result := High(FKeyboard);
end;

function TRALChatMessage.Clone: TRALChatMessage;
var
  vRow: Integer;
begin
  Result := TRALChatMessage.Create;
  try
    Result.FButtons := Copy(FButtons, 0, Length(FButtons));
    Result.FCallbackData := FCallbackData;
    Result.FChannelName := FChannelName;
    Result.FChat := FChat;
    Result.FCommand := FCommand;
    Result.FCommandArgs := FCommandArgs;
    Result.FContact := FContact;
    Result.FEventId := FEventId;
    Result.FFrom := FFrom;
    Result.FId := FId;
    SetLength(Result.FKeyboard, Length(FKeyboard));
    for vRow := 0 to High(FKeyboard) do
      Result.FKeyboard[vRow] := Copy(FKeyboard[vRow], 0, Length(FKeyboard[vRow]));
    Result.FKind := FKind;
    Result.FList.Title := FList.Title;
    Result.FList.ButtonText := FList.ButtonText;
    Result.FList.Items := Copy(FList.Items, 0, Length(FList.Items));
    Result.FLocation := FLocation;
    Result.FRaw := FRaw;
    Result.FText := FText;
    Result.FTextFormat := FTextFormat;
    Result.FTimestamp := FTimestamp;
    if FMedia <> nil then
      Result.Media.Assign(FMedia);
    if FReplyTo <> nil then
      Result.FReplyTo := FReplyTo.Clone;
  except
    Result.Free;
    raise;
  end;
end;

function TRALChatMessage.GetChatId: StringRAL;
begin
  Result := FChat.Id;
end;

procedure TRALChatMessage.SetChatId(const AValue: StringRAL);
begin
  FChat.Id := AValue;
end;

function TRALChatMessage.GetMedia: TRALChatMedia;
begin
  if FMedia = nil then
  begin
    FMedia := TRALChatMedia.Create;
    FMedia.FSize := -1;
  end;
  Result := FMedia;
end;

function TRALChatMessage.GetReplyTo: TRALChatMessage;
begin
  if FReplyTo = nil then
    FReplyTo := TRALChatMessage.Create;
  Result := FReplyTo;
end;

function TRALChatMessage.HasChoices: Boolean;
begin
  Result := (Length(FButtons) > 0) or (Length(FKeyboard) > 0) or (Length(FList.Items) > 0);
end;

function TRALChatMessage.HasMedia: Boolean;
begin
  Result := FMedia <> nil;
end;

function TRALChatMessage.HasReplyTo: Boolean;
begin
  Result := FReplyTo <> nil;
end;

procedure TRALChatMessage.Prepare(ACaps: TRALChatCapabilitySet; AMaxButtons: Integer;
  AFallback: TRALChatFallback; const AChannel: StringRAL; out ADegraded: TStringList);
var
  vLines: TStringList;
  vRow, vIdx: Integer;
  vText: StringRAL;

  procedure Need(ACap: TRALChatCapability; const AName: string);
  begin
    if AFallback = cfRaise then
      raise ERALChatError.CreateFmt(emChatNoCapability, [string(AChannel), AName]);
  end;

  procedure AddChoice(const AId, ACaption: StringRAL);
  begin
    ADegraded.Add(string(AId));
    vLines.Add(IntToStr(ADegraded.Count) + '. ' + string(ACaption));
  end;

begin
  ADegraded := TStringList.Create;
  vLines := TStringList.Create;
  try
    if (Length(FButtons) > 0) and (not (ccButtons in ACaps) or
       ((AMaxButtons > 0) and (Length(FButtons) > AMaxButtons))) then
    begin
      Need(ccButtons, 'buttons');
      for vIdx := 0 to High(FButtons) do
        if FButtons[vIdx].Url <> '' then
          vLines.Add(string(FButtons[vIdx].Caption + ': ' + FButtons[vIdx].Url))
        else
          AddChoice(FButtons[vIdx].Id, FButtons[vIdx].Caption);
      FButtons := nil;
    end;
    if (Length(FKeyboard) > 0) and not (ccKeyboard in ACaps) then
    begin
      Need(ccKeyboard, 'keyboard');
      for vRow := 0 to High(FKeyboard) do
        for vIdx := 0 to High(FKeyboard[vRow]) do
          if FKeyboard[vRow][vIdx].Url <> '' then
            vLines.Add(string(FKeyboard[vRow][vIdx].Caption + ': ' + FKeyboard[vRow][vIdx].Url))
          else
            AddChoice(FKeyboard[vRow][vIdx].Id, FKeyboard[vRow][vIdx].Caption);
      FKeyboard := nil;
    end;
    if (Length(FList.Items) > 0) and not (ccLists in ACaps) then
    begin
      Need(ccLists, 'lists');
      for vIdx := 0 to High(FList.Items) do
      begin
        if FList.Items[vIdx].Description <> '' then
          AddChoice(FList.Items[vIdx].Id, FList.Items[vIdx].Title + ' - ' +
            FList.Items[vIdx].Description)
        else
          AddChoice(FList.Items[vIdx].Id, FList.Items[vIdx].Title);
      end;
      FList.Items := nil;
    end;
    if FMedia <> nil then
      if not (ccMedia in ACaps) then
      begin
        Need(ccMedia, 'media');
        if FMedia.FUrl <> '' then
          vLines.Add(string(FMedia.FUrl))
        else if FMedia.FFileName <> '' then
          vLines.Add('[' + string(FMedia.FFileName) + ']');
        FreeAndNil(FMedia);
      end;
    if vLines.Count > 0 then
    begin
      vText := FText;
      if vText <> '' then
        vText := vText + #10;
      vText := vText + StringRAL(vLines.Text);
      if ADegraded.Count > 0 then
        vText := vText + #10 + StringRAL(cmChatReplyWithNumber);
      FText := vText;
    end;
  finally
    vLines.Free;
  end;
end;

{ TRALChatMessageList }

constructor TRALChatMessageList.Create;
begin
  inherited Create(True);
end;

function TRALChatMessageList.AddMessage(AMessage: TRALChatMessage): Integer;
begin
  Result := Add(AMessage);
end;

function TRALChatMessageList.GetItem(AIndex: Integer): TRALChatMessage;
begin
  Result := TRALChatMessage(inherited Items[AIndex]);
end;

{ TRALChatCapabilities }

constructor TRALChatCapabilities.Create;
begin
  inherited Create;
end;

procedure TRALChatCapabilities.Assign(Source: TPersistent);
begin
  if Source is TRALChatCapabilities then
  begin
    FMaxButtons := TRALChatCapabilities(Source).FMaxButtons;
    FMaxMediaBytes := TRALChatCapabilities(Source).FMaxMediaBytes;
    FMaxText := TRALChatCapabilities(Source).FMaxText;
    FSupported := TRALChatCapabilities(Source).FSupported;
  end
  else
    inherited Assign(Source);
end;

function TRALChatCapabilities.Supports(ACapability: TRALChatCapability): Boolean;
begin
  Result := ACapability in FSupported;
end;

{ text conversion }

type
  /// The styles the simple markup has.
  TMarkStyle = (msBold, msItalic, msCode);

const
  /// The characters MarkdownV2 wants escaped outside code.
  cSpecialMD2 = ['_', '*', '[', ']', '(', ')', '~', '`', '>', '#', '+', '-', '=', '|', '{', '}',
    '.', '!', '\'];

/// Appends one byte to AText without a string conversion.
procedure PutChar(var AText: StringRAL; AChar: AnsiChar);
var
  vLen: Integer;
begin
  vLen := Length(AText);
  SetLength(AText, vLen + 1);
  AText[vLen + 1] := AChar;
end;

/// Whether AChar is a letter or a digit.
function IsWordChar(AChar: AnsiChar): Boolean;
begin
  Result := AChar in ['a'..'z', 'A'..'Z', '0'..'9'];
end;

/// What AChar becomes in ADialect (escaped where the dialect needs it).
function EscapeFor(ADialect: TRALChatDialect; AChar: AnsiChar; AInCode: Boolean): StringRAL;
begin
  Result := StringRAL(AChar);
  case ADialect of
    cdTelegramHTML, cdSlack:
      case AChar of
        '&': Result := '&amp;';
        '<': Result := '&lt;';
        '>': Result := '&gt;';
      end;
    cdTelegramMarkdownV2:
      if AInCode then
      begin
        if (AChar = '`') or (AChar = '\') then
          Result := '\' + StringRAL(AChar);
      end
      else if AChar in cSpecialMD2 then
        Result := '\' + StringRAL(AChar);
  end;
end;

/// What opens AStyle in ADialect.
function OpenTag(ADialect: TRALChatDialect; AStyle: TMarkStyle): StringRAL;
begin
  Result := '';
  case ADialect of
    cdTelegramHTML:
      case AStyle of
        msBold: Result := '<b>';
        msItalic: Result := '<i>';
        msCode: Result := '<code>';
      end;
    cdTelegramMarkdownV2, cdWhatsApp, cdSlack:
      case AStyle of
        msBold: Result := '*';
        msItalic: Result := '_';
        msCode: Result := '`';
      end;
    cdDiscord:
      case AStyle of
        msBold: Result := '**';
        msItalic: Result := '*';
        msCode: Result := '`';
      end;
  end;
end;

/// What closes AStyle in ADialect.
function CloseTag(ADialect: TRALChatDialect; AStyle: TMarkStyle): StringRAL;
begin
  if ADialect = cdTelegramHTML then
    case AStyle of
      msBold: Result := '</b>';
      msItalic: Result := '</i>';
    else
      Result := '</code>';
    end
  else
    Result := OpenTag(ADialect, AStyle);
end;

/// A link in ADialect.
function LinkText(ADialect: TRALChatDialect; const ALabel, AUrl: StringRAL): StringRAL;
var
  vIdx: Integer;
  vUrl: StringRAL;
begin
  case ADialect of
    cdTelegramHTML:
    begin
      vUrl := '';
      for vIdx := 1 to Length(AUrl) do
        if AUrl[vIdx] = '"' then
          vUrl := vUrl + '&quot;'
        else
          PutChar(vUrl, AUrl[vIdx]);
      Result := '<a href="' + vUrl + '">' + ALabel + '</a>';
    end;
    cdTelegramMarkdownV2:
    begin
      vUrl := '';
      for vIdx := 1 to Length(AUrl) do
      begin
        if (AUrl[vIdx] = ')') or (AUrl[vIdx] = '\') then
          PutChar(vUrl, '\');
        PutChar(vUrl, AUrl[vIdx]);
      end;
      Result := '[' + ALabel + '](' + vUrl + ')';
    end;
    cdDiscord:
      Result := '[' + ALabel + '](' + AUrl + ')';
    cdSlack:
      Result := '<' + AUrl + '|' + ALabel + '>';
  else
    Result := ALabel + ' (' + AUrl + ')';
  end;
end;

function RALChatConvertText(const AText: StringRAL; ADialect: TRALChatDialect;
  AMarkup: Boolean): StringRAL;
var
  vActive: array[TMarkStyle] of Boolean;
  vIdx, vEnd, vMid: Integer;
  vC: AnsiChar;
  vStyle: TMarkStyle;
  vLabel, vUrl: StringRAL;

  function Closer(AFrom: Integer; AMark: AnsiChar): Boolean;
  var
    vJ: Integer;
  begin
    Result := False;
    vJ := AFrom;
    while vJ <= Length(AText) do
    begin
      if AText[vJ] = '\' then
        Inc(vJ)
      else if AText[vJ] = AMark then
      begin
        Result := vJ > AFrom;
        Exit;
      end;
      Inc(vJ);
    end;
  end;

  function Literal(AChar: AnsiChar): StringRAL;
  begin
    Result := EscapeFor(ADialect, AChar, vActive[msCode]);
  end;

begin
  Result := '';
  FillChar(vActive, SizeOf(vActive), 0);
  vIdx := 1;
  while vIdx <= Length(AText) do
  begin
    vC := AText[vIdx];
    if not AMarkup then
    begin
      Result := Result + Literal(vC);
      Inc(vIdx);
      Continue;
    end;
    if (vC = '\') and (vIdx < Length(AText)) and not vActive[msCode] then
    begin
      Result := Result + Literal(AText[vIdx + 1]);
      Inc(vIdx, 2);
      Continue;
    end;
    if (vC = '`') then
    begin
      if vActive[msCode] then
      begin
        Result := Result + CloseTag(ADialect, msCode);
        vActive[msCode] := False;
        Inc(vIdx);
        Continue;
      end
      else if Closer(vIdx + 1, '`') then
      begin
        Result := Result + OpenTag(ADialect, msCode);
        vActive[msCode] := True;
        Inc(vIdx);
        Continue;
      end;
    end;
    if vActive[msCode] then
    begin
      Result := Result + Literal(vC);
      Inc(vIdx);
      Continue;
    end;
    if (vC = '*') or (vC = '_') then
    begin
      if vC = '*' then
        vStyle := msBold
      else
        vStyle := msItalic;
      // an underscore inside a word (snake_case) is not an emphasis mark
      if vActive[vStyle] and ((vC <> '_') or (vIdx = Length(AText)) or
         not IsWordChar(AText[vIdx + 1])) then
      begin
        Result := Result + CloseTag(ADialect, vStyle);
        vActive[vStyle] := False;
        Inc(vIdx);
        Continue;
      end
      else if (not vActive[vStyle]) and (vIdx < Length(AText)) and
        (AText[vIdx + 1] <> ' ') and
        ((vC <> '_') or (vIdx = 1) or not IsWordChar(AText[vIdx - 1])) and
        Closer(vIdx + 1, vC) then
      begin
        Result := Result + OpenTag(ADialect, vStyle);
        vActive[vStyle] := True;
        Inc(vIdx);
        Continue;
      end;
    end;
    if vC = '[' then
    begin
      vEnd := vIdx + 1;
      while (vEnd <= Length(AText)) and (AText[vEnd] <> ']') do
        Inc(vEnd);
      if (vEnd < Length(AText)) and (AText[vEnd + 1] = '(') then
      begin
        vMid := vEnd + 2;
        while (vMid <= Length(AText)) and (AText[vMid] <> ')') do
          Inc(vMid);
        if vMid <= Length(AText) then
        begin
          vLabel := RALChatConvertText(Copy(AText, vIdx + 1, vEnd - vIdx - 1), ADialect, False);
          vUrl := Copy(AText, vEnd + 2, vMid - vEnd - 2);
          Result := Result + LinkText(ADialect, vLabel, vUrl);
          vIdx := vMid + 1;
          Continue;
        end;
      end;
    end;
    Result := Result + Literal(vC);
    Inc(vIdx);
  end;
  // a link may have eaten the closer of a style
  for vStyle := Low(TMarkStyle) to High(TMarkStyle) do
    if vActive[vStyle] then
      Result := Result + CloseTag(ADialect, vStyle);
end;

procedure RALChatSplitText(const AText: StringRAL; AMax: Integer; APieces: TStrings);
var
  vStart, vCut, vLen: Integer;
begin
  if (AMax <= 0) or (Length(AText) <= AMax) then
  begin
    APieces.Add(string(AText));
    Exit;
  end;
  vStart := 1;
  vLen := Length(AText);
  while vStart <= vLen do
  begin
    if vLen - vStart + 1 <= AMax then
    begin
      APieces.Add(string(Copy(AText, vStart, MaxInt)));
      Break;
    end;
    vCut := vStart + AMax - 1;
    // prefer the last line break, then the last space, in the second half
    while (vCut > vStart + AMax div 2) and (AText[vCut] <> #10) do
      Dec(vCut);
    if AText[vCut] <> #10 then
    begin
      vCut := vStart + AMax - 1;
      while (vCut > vStart + AMax div 2) and (AText[vCut] <> ' ') do
        Dec(vCut);
      if AText[vCut] <> ' ' then
      begin
        vCut := vStart + AMax - 1;
        // not inside a UTF-8 sequence: the next byte must not be a continuation
        while (vCut > vStart) and ((Ord(AText[vCut + 1]) and $C0) = $80) do
          Dec(vCut);
      end;
    end;
    APieces.Add(string(Copy(AText, vStart, vCut - vStart + 1)));
    vStart := vCut + 1;
  end;
end;

function RALChatIsChoice(const AText: StringRAL; out AValue: Integer): Boolean;
var
  vIdx: Integer;
  vText: StringRAL;
begin
  vText := AText;
  while (vText <> '') and (vText[1] <= ' ') do
    Delete(vText, 1, 1);
  while (vText <> '') and (vText[Length(vText)] <= ' ') do
    Delete(vText, Length(vText), 1);
  Result := (vText <> '') and (Length(vText) <= 4);
  for vIdx := 1 to Length(vText) do
    if not (vText[vIdx] in ['0'..'9']) then
      Result := False;
  AValue := 0;
  if Result then
    AValue := StrToIntDef(string(vText), 0);
  Result := Result and (AValue >= 1);
end;

end.
