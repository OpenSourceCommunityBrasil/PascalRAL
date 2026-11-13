{ The center of the chat integration: the bot knows its channels and its commands,
  receives every message the channels hand it (Receive), runs the matching command
  or OnMessage/OnCallback, and sends through each channel's outbox with the
  fallback for what a channel cannot show. It also keeps the state of each
  conversation (sessions, with expiry) in a store. }
unit RALChatBot;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALConsts, RALTools, RALCustomObjects, RALRegex, RALKeyValueStore,
  RALChatMessage, RALChatChannel, RALChatOutbox, RALChatStore;

type
  TRALChatBot = class;

  /// A message arrives.
  TRALChatMessageEvent = procedure(Sender: TObject; AChannel: TRALChatChannel;
    AMessage: TRALChatMessage) of object;

  /// Something failed while handling a message or sending one; AText carries no secret.
  TRALChatErrorEvent = procedure(Sender: TObject; AChannel: TRALChatChannel;
    const AText: string) of object;

  /// A message went out (it runs on the sender thread of the channel).
  TRALChatSentEvent = procedure(Sender: TObject; AChannel: TRALChatChannel;
    AMessage: TRALChatMessage; const AResult: TRALChatSendResult) of object;

  /// A message could not be sent after its attempts (it runs on the sender thread).
  TRALChatSendFailedEvent = procedure(Sender: TObject; AChannel: TRALChatChannel;
    AMessage: TRALChatMessage; const AError: string) of object;

  /// A line for the application's log; never carries a token or a secret.
  TRALChatLogEvent = procedure(Sender: TObject; const AText: string) of object;

  /// A command matched; set AHandled to False to let the next one (and OnMessage) see it.
  TRALChatCommandEvent = procedure(Sender: TObject; AChannel: TRALChatChannel;
    AMessage: TRALChatMessage; var AHandled: Boolean) of object;

  /// How a command recognizes a message.
  TRALChatCommandMatch = (
    /// A slash command: Pattern is its name (/start).
    cmmCommand,
    /// The whole text equals Pattern, whatever the case.
    cmmKeyword,
    /// The text starts with Pattern.
    cmmPrefix,
    /// The text contains Pattern.
    cmmContains,
    /// The text matches the regular expression Pattern.
    cmmRegex,
    /// A button answer whose id is Pattern.
    cmmCallback
  );

  /// One channel of the bot.
  TRALChatChannelItem = class(TCollectionItem)
  private
    FChannel: TRALChatChannel;

    procedure SetChannel(AValue: TRALChatChannel);
  protected
    function GetDisplayName: string; override;
  published
    /// The channel component.
    property Channel: TRALChatChannel read FChannel write SetChannel;
  end;

  /// The channels of a bot.
  TRALChatChannels = class(TOwnedCollection)
  private
    function GetItem(AIndex: Integer): TRALChatChannelItem;
  public
    /// Creates the TRALChatChannels.
    constructor Create(AOwner: TPersistent);

    /// Adds a channel to the list.
    function Add: TRALChatChannelItem;
    /// The channel named AName, nil when none.
    function Find(const AName: string): TRALChatChannel;

    /// The item at AIndex.
    property Items[AIndex: Integer]: TRALChatChannelItem read GetItem; default;
  end;

  /// A command: a way to recognize a message and the event that answers it.
  TRALChatCommand = class(TCollectionItem)
  private
    FChannels: string;
    FEnabled: Boolean;
    FMatch: TRALChatCommandMatch;
    FName: string;
    FOnExecute: TRALChatCommandEvent;
    FPattern: StringRAL;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;

    /// Whether AMessage, from the channel called AChannel, is this command's.
    function Matches(const AChannel: string; AMessage: TRALChatMessage): Boolean;
  published
    /// The channels it listens to, names separated by commas; empty listens to all.
    property Channels: string read FChannels write FChannels;
    /// Turns the command off without removing it.
    property Enabled: Boolean read FEnabled write FEnabled default True;
    /// How the message is recognized.
    property Match: TRALChatCommandMatch read FMatch write FMatch default cmmCommand;
    /// A name to tell the command in the list.
    property Name: string read FName write FName;
    /// Answers the message.
    property OnExecute: TRALChatCommandEvent read FOnExecute write FOnExecute;
    /// What the message must be: a command name, a word, a prefix or an expression.
    property Pattern: StringRAL read FPattern write FPattern;
  end;

  /// The commands of a bot.
  TRALChatCommands = class(TOwnedCollection)
  private
    function GetItem(AIndex: Integer): TRALChatCommand;
  public
    /// Creates the TRALChatCommands.
    constructor Create(AOwner: TPersistent);

    /// Adds a command.
    function Add: TRALChatCommand;

    /// The command at AIndex.
    property Items[AIndex: Integer]: TRALChatCommand read GetItem; default;
  end;

  { The state of each conversation: names and values that expire together after
    SessionTimeout seconds without a message or a change. It lives in the bot's
    store, so a persistent store keeps it through a restart. }
  TRALChatSessions = class
  private
    /// The bot that owns the sessions.
    FBot: TRALChatBot;
    /// Guards the shared state.
    FLock: TCriticalSection;

    /// The store key of a conversation's session.
    function KeyOf(const AChannel, AChat: StringRAL): StringRAL;
    /// The values of the session kept under AKey.
    function Load(const AKey: StringRAL): TStringList;
    /// Keeps AValues under AKey with the session timeout.
    procedure Save(const AKey: StringRAL; AValues: TStringList);
  public
    /// Creates the TRALChatSessions.
    constructor Create(ABot: TRALChatBot);
    destructor Destroy; override;

    /// Forgets the whole session of a conversation.
    procedure Clear(const AChannel, AChat: StringRAL);
    /// Whether the conversation has a session.
    function Exists(const AChannel, AChat: StringRAL): Boolean;
    /// The value of AName, empty when none.
    function GetValue(const AChannel, AChat, AName: StringRAL): StringRAL;
    /// Sets the value of AName (and renews the expiry).
    procedure SetValue(const AChannel, AChat, AName, AValue: StringRAL);
    /// Renews the expiry of the conversation's session, if it has one.
    procedure Touch(const AChannel, AChat: StringRAL);
  end;

  { A component that sees each message before the bot's commands and events, such
    as a conversation flow: True from Intercept keeps the message from them. }
  TRALChatInterceptor = class(TRALComponent)
  protected
    { Handles AMessage when it is its own; True when it did. Runs on the thread that
      received the message. }
    function Intercept(AChannel: TRALChatChannel; AMessage: TRALChatMessage): Boolean;
      virtual; abstract;
  end;

  { The chat bot component. Drop the channels of the platforms on the form, list
    them in Channels, write the commands in Commands (each with its own event), and
    link the bot to a TRALChatWebhookModule (or let a channel's polling call
    Receive). The events OnCallback, OnSent and OnSendFailed run on other threads
    than the main one; the handlers must be thread-safe. }
  TRALChatBot = class(TRALComponent)
  private
    FChannels: TRALChatChannels;
    FClock: TRALStoreClock;
    FCommands: TRALChatCommands;
    FFallback: TRALChatFallback;
    /// The interceptors (flows) linked to the bot, in linking order.
    FInterceptors: TList;
    /// Guards the shared state.
    FLock: TCriticalSection;
    FOnCallback: TRALChatMessageEvent;
    FOnError: TRALChatErrorEvent;
    FOnLog: TRALChatLogEvent;
    FOnMessage: TRALChatMessageEvent;
    FOnSendFailed: TRALChatSendFailedEvent;
    FOnSent: TRALChatSentEvent;
    /// The outbox of each channel, by channel name.
    FOutboxes: TStringList;
    /// The memory store used when Store is not linked.
    FOwnStore: TRALChatStore;
    FPersistFile: string;
    FSessions: TRALChatSessions;
    FSessionTimeout: Integer;
    FStore: TRALKeyValueStore;

    function GetActiveStore: TRALKeyValueStore;
    /// Tells the application how a delivery ended.
    procedure HandleDone(AChannel: TRALChatChannel; AKind: TRALChatOutKind;
      AMessage: TRALChatMessage; const AResult: TRALChatSendResult; AAttempts: Integer);
    /// The key the degraded choices of a conversation are kept under.
    function OptionsKey(const AChannel, AChat: StringRAL): StringRAL;
    /// Reads /name args into Command and CommandArgs.
    procedure ParseCommand(AMessage: TRALChatMessage);
    /// Makes ACopy ready for the channel: choices the channel lacks become text.
    procedure PrepareOut(AChannel: TRALChatChannel; ACopy: TRALChatMessage);
    /// Queues ACopy (the outbox owns it, or it is freed when this raises) in pieces.
    procedure Queue(AChannel: TRALChatChannel; ACopy: TRALChatMessage);
    /// A numeric answer to a degraded list becomes the button it stood for.
    procedure ResolveChoice(AChannel: TRALChatChannel; AMessage: TRALChatMessage);
    procedure SetChannels(AValue: TRALChatChannels);
    procedure SetCommands(AValue: TRALChatCommands);
    procedure SetPersistFile(const AValue: string);
    procedure SetStore(AValue: TRALKeyValueStore);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Adds an interceptor (a flow calls it when its Bot is set).
    procedure AddInterceptor(AInterceptor: TRALChatInterceptor);
    /// Deletes a sent message (queued).
    procedure Delete(const AChannelName, AChatId, AMessageId: StringRAL);
    /// Changes the text (and choices) of a sent message (queued).
    procedure Edit(const AChannelName, AChatId, AMessageId: StringRAL;
      AMessage: TRALChatMessage);
    /// Tells a line to OnLog.
    procedure Log(const AText: string);
    /// The outbox of a channel, created on first use.
    function OutboxOf(AChannel: TRALChatChannel): TRALChatOutbox;
    { Handles a message a channel received: its choice answer, the interceptors
      (flows), the commands, then OnCallback or OnMessage. An exception of a
      handler goes to OnError. }
    procedure Receive(AChannel: TRALChatChannel; AMessage: TRALChatMessage);
    /// Removes an interceptor.
    procedure RemoveInterceptor(AInterceptor: TRALChatInterceptor);
    { Answers AReceived in its conversation, queued; AMessage is copied, the caller
      keeps it. }
    procedure Reply(AReceived: TRALChatMessage; AMessage: TRALChatMessage); overload;
    /// Answers AReceived with text, queued.
    procedure Reply(AReceived: TRALChatMessage; const AText: StringRAL;
      AMarkup: Boolean = False); overload;
    /// Tells the application something went wrong (OnError and OnLog).
    procedure ReportError(AChannel: TRALChatChannel; const AText: string);
    { Sends AMessage through the channel AChannelName, queued: the bot copies it
      (the caller keeps it) and turns what the channel cannot show into text (or
      raises, with Fallback cfRaise). The outcome comes to OnSent or OnSendFailed. }
    procedure Send(const AChannelName: string; AMessage: TRALChatMessage);
    /// Shows "typing" in a conversation (queued).
    procedure SendTyping(const AChannelName, AChatId: StringRAL);
    { Sends and waits for the outcome: the id of the sent message; raises
      ERALChatError when it failed or ATimeoutMs passed. A message split in pieces
      returns the id of the last. }
    function SendWait(const AChannelName: string; AMessage: TRALChatMessage;
      ATimeoutMs: Integer = 30000): StringRAL;
    /// Stops every outbox (what is queued fails).
    procedure Stop;
    /// Whether the channel called AChannelName has ACapability.
    function Supports(const AChannelName: string; ACapability: TRALChatCapability): Boolean;
    /// Now, Unix seconds (Clock when set): the webhook's replay window reads it.
    function UnixNow: Int64RAL;

    { The store in use: Store when linked, else the bot's own memory store (with the
      journal of PersistFile). }
    property ActiveStore: TRALKeyValueStore read GetActiveStore;
    /// Overrides the time of day, in Unix milliseconds; for tests.
    property Clock: TRALStoreClock read FClock write FClock;
    /// The state of each conversation.
    property Sessions: TRALChatSessions read FSessions;
  published
    /// The channels the bot talks through.
    property Channels: TRALChatChannels read FChannels write SetChannels;
    /// The commands the bot answers.
    property Commands: TRALChatCommands read FCommands write SetCommands;
    /// What to do with a feature a channel lacks: degrade it to text or raise.
    property Fallback: TRALChatFallback read FFallback write FFallback default cfDegrade;
    /// A button (or list item) answer arrived.
    property OnCallback: TRALChatMessageEvent read FOnCallback write FOnCallback;
    /// Something failed.
    property OnError: TRALChatErrorEvent read FOnError write FOnError;
    /// A line to log.
    property OnLog: TRALChatLogEvent read FOnLog write FOnLog;
    /// A message that no command took.
    property OnMessage: TRALChatMessageEvent read FOnMessage write FOnMessage;
    /// A message could not be sent.
    property OnSendFailed: TRALChatSendFailedEvent read FOnSendFailed write FOnSendFailed;
    /// A message was sent.
    property OnSent: TRALChatSentEvent read FOnSent write FOnSent;
    { The journal file of the bot's own store: conversations and the ids already
      seen survive a restart. Ignored when Store is linked. }
    property PersistFile: string read FPersistFile write SetPersistFile;
    /// Seconds without a message or a change after which a session is forgotten.
    property SessionTimeout: Integer read FSessionTimeout write FSessionTimeout default 1800;
    /// A store of your own (a file or database one) instead of the bot's memory one.
    property Store: TRALKeyValueStore read FStore write SetStore;
  end;

implementation

/// Undoes Escape.
function Unescape(const AText: string): string;
var
  vIdx: Integer;
begin
  Result := '';
  vIdx := 1;
  while vIdx <= Length(AText) do
  begin
    if (AText[vIdx] = '\') and (vIdx < Length(AText)) then
    begin
      Inc(vIdx);
      case AText[vIdx] of
        't': Result := Result + #9;
        'n': Result := Result + #10;
      else
        Result := Result + AText[vIdx];
      end;
    end
    else
      Result := Result + AText[vIdx];
    Inc(vIdx);
  end;
end;

/// Writes AText on one line: backslash, tab and line feed are escaped.
function Escape(const AText: string): string;
var
  vIdx: Integer;
begin
  Result := '';
  for vIdx := 1 to Length(AText) do
    case AText[vIdx] of
      '\': Result := Result + '\\';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #13: ;
    else
      Result := Result + AText[vIdx];
    end;
end;

{ TRALChatChannelItem }

function TRALChatChannelItem.GetDisplayName: string;
begin
  if FChannel <> nil then
    Result := string(FChannel.ChannelName)
  else
    Result := inherited GetDisplayName;
end;

procedure TRALChatChannelItem.SetChannel(AValue: TRALChatChannel);
var
  vBot: TComponent;
begin
  if FChannel = AValue then
    Exit;
  FChannel := AValue;
  vBot := TComponent(TOwnedCollection(Collection).Owner);
  if (FChannel <> nil) and (vBot <> nil) then
    FChannel.FreeNotification(vBot);
end;

{ TRALChatChannels }

constructor TRALChatChannels.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALChatChannelItem);
end;

function TRALChatChannels.Add: TRALChatChannelItem;
begin
  Result := TRALChatChannelItem(inherited Add);
end;

function TRALChatChannels.Find(const AName: string): TRALChatChannel;
var
  vIdx: Integer;
begin
  Result := nil;
  for vIdx := 0 to Count - 1 do
    if (Items[vIdx].Channel <> nil) and
       SameText(string(Items[vIdx].Channel.ChannelName), AName) then
    begin
      Result := Items[vIdx].Channel;
      Exit;
    end;
end;

function TRALChatChannels.GetItem(AIndex: Integer): TRALChatChannelItem;
begin
  Result := TRALChatChannelItem(inherited Items[AIndex]);
end;

{ TRALChatCommand }

constructor TRALChatCommand.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FEnabled := True;
end;

function TRALChatCommand.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := string(FPattern);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TRALChatCommand.Matches(const AChannel: string; AMessage: TRALChatMessage): Boolean;
var
  vList: TStringList;
  vPattern, vText: string;
  vRegex: TRALRegex;
begin
  Result := False;
  if not FEnabled then
    Exit;
  if FChannels <> '' then
  begin
    vList := TStringList.Create;
    try
      vList.CommaText := StringReplace(FChannels, ' ', '', [rfReplaceAll]);
      if vList.IndexOf(AChannel) < 0 then
      begin
        Result := False;
        // names are compared without case
        Exit;
      end;
    finally
      vList.Free;
    end;
  end;
  vPattern := string(FPattern);
  vText := Trim(string(AMessage.Text));
  case FMatch of
    cmmCommand:
    begin
      if (vPattern <> '') and (vPattern[1] = '/') then
        Delete(vPattern, 1, 1);
      Result := (AMessage.Kind = ckCommand) and SameText(string(AMessage.Command), vPattern);
    end;
    cmmKeyword:
      Result := (AMessage.Kind in [ckText, ckCommand]) and SameText(vText, vPattern);
    cmmPrefix:
      Result := (AMessage.Kind in [ckText, ckCommand]) and (vPattern <> '') and
        SameText(Copy(vText, 1, Length(vPattern)), vPattern);
    cmmContains:
      Result := (AMessage.Kind in [ckText, ckCommand]) and (vPattern <> '') and
        (RALPosText(StringRAL(LowerCase(vPattern)), AMessage.Text) > 0);
    cmmRegex:
      if (AMessage.Kind in [ckText, ckCommand]) and (vPattern <> '') then
      begin
        vRegex := TRALRegex.Create(FPattern);
        try
          Result := vRegex.IsMatch(AMessage.Text);
        finally
          vRegex.Free;
        end;
      end;
    cmmCallback:
      Result := (AMessage.Kind = ckCallback) and (string(AMessage.CallbackData) = vPattern);
  end;
end;

{ TRALChatCommands }

constructor TRALChatCommands.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALChatCommand);
end;

function TRALChatCommands.Add: TRALChatCommand;
begin
  Result := TRALChatCommand(inherited Add);
end;

function TRALChatCommands.GetItem(AIndex: Integer): TRALChatCommand;
begin
  Result := TRALChatCommand(inherited Items[AIndex]);
end;

{ TRALChatSessions }

constructor TRALChatSessions.Create(ABot: TRALChatBot);
begin
  inherited Create;
  FBot := ABot;
  FLock := TCriticalSection.Create;
end;

destructor TRALChatSessions.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TRALChatSessions.Clear(const AChannel, AChat: StringRAL);
begin
  FLock.Enter;
  try
    FBot.ActiveStore.Delete(KeyOf(AChannel, AChat));
  finally
    FLock.Leave;
  end;
end;

function TRALChatSessions.Exists(const AChannel, AChat: StringRAL): Boolean;
var
  vValue: RawByteString;
begin
  Result := FBot.ActiveStore.Get(KeyOf(AChannel, AChat), vValue);
end;

function TRALChatSessions.GetValue(const AChannel, AChat, AName: StringRAL): StringRAL;
var
  vValues: TStringList;
begin
  FLock.Enter;
  try
    vValues := Load(KeyOf(AChannel, AChat));
    try
      Result := StringRAL(vValues.Values[string(AName)]);
    finally
      vValues.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALChatSessions.KeyOf(const AChannel, AChat: StringRAL): StringRAL;
begin
  Result := 'chat:s:' + AChannel + ':' + AChat;
end;

function TRALChatSessions.Load(const AKey: StringRAL): TStringList;
var
  vValue: RawByteString;
  vLines: TStringList;
  vIdx, vPos: Integer;
begin
  Result := TStringList.Create;
  if FBot.ActiveStore.Get(AKey, vValue) then
  begin
    vLines := TStringList.Create;
    try
      vLines.Text := string(UTF8String(vValue));
      for vIdx := 0 to vLines.Count - 1 do
      begin
        vPos := Pos(#9, vLines[vIdx]);
        if vPos > 1 then
          Result.Values[Unescape(Copy(vLines[vIdx], 1, vPos - 1))] :=
            Unescape(Copy(vLines[vIdx], vPos + 1, MaxInt));
      end;
    finally
      vLines.Free;
    end;
  end;
end;

procedure TRALChatSessions.Save(const AKey: StringRAL; AValues: TStringList);
var
  vText: string;
  vIdx: Integer;
begin
  vText := '';
  for vIdx := 0 to AValues.Count - 1 do
    vText := vText + Escape(AValues.Names[vIdx]) + #9 + Escape(AValues.ValueFromIndex[vIdx]) + #10;
  FBot.ActiveStore.Put(AKey, RawByteString(UTF8String(vText)),
    Int64RAL(FBot.SessionTimeout) * 1000);
end;

procedure TRALChatSessions.SetValue(const AChannel, AChat, AName, AValue: StringRAL);
var
  vValues: TStringList;
  vKey: StringRAL;
begin
  FLock.Enter;
  try
    vKey := KeyOf(AChannel, AChat);
    vValues := Load(vKey);
    try
      vValues.Values[string(AName)] := string(AValue);
      Save(vKey, vValues);
    finally
      vValues.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatSessions.Touch(const AChannel, AChat: StringRAL);
begin
  FBot.ActiveStore.Expire(KeyOf(AChannel, AChat), Int64RAL(FBot.SessionTimeout) * 1000);
end;

{ TRALChatBot }

constructor TRALChatBot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChannels := TRALChatChannels.Create(Self);
  FCommands := TRALChatCommands.Create(Self);
  FFallback := cfDegrade;
  FInterceptors := TList.Create;
  FLock := TCriticalSection.Create;
  FOutboxes := TStringList.Create;
  FOwnStore := TRALChatStore.Create(Self);
  FSessions := TRALChatSessions.Create(Self);
  FSessionTimeout := 1800;
end;

destructor TRALChatBot.Destroy;
begin
  Stop;
  FSessions.Free;
  FOutboxes.Free;
  FLock.Free;
  FInterceptors.Free;
  FCommands.Free;
  FChannels.Free;
  inherited Destroy;
end;

procedure TRALChatBot.AddInterceptor(AInterceptor: TRALChatInterceptor);
begin
  FLock.Enter;
  try
    if FInterceptors.IndexOf(AInterceptor) < 0 then
      FInterceptors.Add(AInterceptor);
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatBot.Delete(const AChannelName, AChatId, AMessageId: StringRAL);
var
  vChannel: TRALChatChannel;
  vMessage: TRALChatMessage;
begin
  vChannel := FChannels.Find(string(AChannelName));
  if vChannel = nil then
    raise ERALChatError.CreateFmt(emChatNoChannel, [string(AChannelName)]);
  vMessage := TRALChatMessage.Create;
  vMessage.ChatId := AChatId;
  OutboxOf(vChannel).Enqueue(okDelete, vMessage, AMessageId);
end;

procedure TRALChatBot.Receive(AChannel: TRALChatChannel; AMessage: TRALChatMessage);
var
  vIdx: Integer;
  vHandled: Boolean;
  vCommand: TRALChatCommand;
  vInterceptors: array of TRALChatInterceptor;
begin
  try
    if (AMessage.Chat.Id <> '') and (AChannel <> nil) then
      FSessions.Touch(AChannel.ChannelName, AMessage.Chat.Id);
    if AChannel <> nil then
      ResolveChoice(AChannel, AMessage);
    if AMessage.Kind = ckText then
      ParseCommand(AMessage);
    // a copy, so that a flow linked or freed meanwhile does not move the list
    FLock.Enter;
    try
      SetLength(vInterceptors, FInterceptors.Count);
      for vIdx := 0 to FInterceptors.Count - 1 do
        vInterceptors[vIdx] := TRALChatInterceptor(FInterceptors[vIdx]);
    finally
      FLock.Leave;
    end;
    for vIdx := 0 to Length(vInterceptors) - 1 do
      if vInterceptors[vIdx].Intercept(AChannel, AMessage) then
        Exit;
    for vIdx := 0 to FCommands.Count - 1 do
    begin
      vCommand := FCommands[vIdx];
      if (AChannel <> nil) and vCommand.Matches(string(AChannel.ChannelName), AMessage) and
         Assigned(vCommand.OnExecute) then
      begin
        vHandled := True;
        vCommand.OnExecute(vCommand, AChannel, AMessage, vHandled);
        if vHandled then
          Exit;
      end;
    end;
    if AMessage.Kind = ckCallback then
    begin
      if Assigned(FOnCallback) then
        FOnCallback(Self, AChannel, AMessage);
    end
    else if Assigned(FOnMessage) then
      FOnMessage(Self, AChannel, AMessage);
  except
    on E: Exception do
      ReportError(AChannel, E.ClassName + ': ' + E.Message);
  end;
end;

procedure TRALChatBot.Edit(const AChannelName, AChatId, AMessageId: StringRAL;
  AMessage: TRALChatMessage);
var
  vChannel: TRALChatChannel;
  vCopy: TRALChatMessage;
begin
  vChannel := FChannels.Find(string(AChannelName));
  if vChannel = nil then
    raise ERALChatError.CreateFmt(emChatNoChannel, [string(AChannelName)]);
  vCopy := AMessage.Clone;
  try
    vCopy.ChatId := AChatId;
    PrepareOut(vChannel, vCopy);
    vCopy.Text := vChannel.FormatText(vCopy.Text, vCopy.TextFormat);
    vCopy.TextFormat := tfPlain;
  except
    vCopy.Free;
    raise;
  end;
  OutboxOf(vChannel).Enqueue(okEdit, vCopy, AMessageId);
end;

function TRALChatBot.GetActiveStore: TRALKeyValueStore;
begin
  if FStore <> nil then
    Result := FStore
  else
    Result := FOwnStore;
end;

procedure TRALChatBot.HandleDone(AChannel: TRALChatChannel; AKind: TRALChatOutKind;
  AMessage: TRALChatMessage; const AResult: TRALChatSendResult; AAttempts: Integer);
begin
  if AResult.Ok then
  begin
    if (AKind = okMessage) and Assigned(FOnSent) then
      FOnSent(Self, AChannel, AMessage, AResult);
  end
  else
  begin
    Log(Format('%s: send failed after %d attempt(s): %s',
      [string(AChannel.ChannelName), AAttempts + 1, string(AResult.ErrorText)]));
    if Assigned(FOnSendFailed) then
      FOnSendFailed(Self, AChannel, AMessage, string(AResult.ErrorText));
  end;
end;

procedure TRALChatBot.Loaded;
begin
  inherited Loaded;
  if FPersistFile <> '' then
    FOwnStore.PersistFile := FPersistFile;
end;

procedure TRALChatBot.Log(const AText: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, AText);
end;

procedure TRALChatBot.Notification(AComponent: TComponent; Operation: TOperation);
var
  vIdx: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then
    Exit;
  if AComponent = FStore then
    FStore := nil;
  if AComponent is TRALChatChannel then
  begin
    // an outbox must not outlive its channel
    for vIdx := 0 to FOutboxes.Count - 1 do
      if TRALChatOutbox(FOutboxes.Objects[vIdx]).Channel = AComponent then
      begin
        TRALChatOutbox(FOutboxes.Objects[vIdx]).Free;
        FOutboxes.Delete(vIdx);
        Break;
      end;
    for vIdx := 0 to FChannels.Count - 1 do
      if FChannels[vIdx].Channel = AComponent then
        FChannels[vIdx].Channel := nil;
  end;
end;

function TRALChatBot.OptionsKey(const AChannel, AChat: StringRAL): StringRAL;
begin
  Result := 'chat:o:' + AChannel + ':' + AChat;
end;

function TRALChatBot.OutboxOf(AChannel: TRALChatChannel): TRALChatOutbox;
var
  vIdx: Integer;
begin
  FLock.Enter;
  try
    vIdx := FOutboxes.IndexOf(string(AChannel.ChannelName));
    if vIdx >= 0 then
      Result := TRALChatOutbox(FOutboxes.Objects[vIdx])
    else
    begin
      Result := TRALChatOutbox.Create(AChannel);
      Result.OnDone := HandleDone;
      FOutboxes.AddObject(string(AChannel.ChannelName), Result);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatBot.ParseCommand(AMessage: TRALChatMessage);
var
  vText, vWord: string;
  vPos: Integer;
begin
  vText := Trim(string(AMessage.Text));
  if (vText = '') or (vText[1] <> '/') then
    Exit;
  vPos := Pos(' ', vText);
  if vPos = 0 then
    vPos := Pos(#10, vText);
  if vPos = 0 then
  begin
    vWord := Copy(vText, 2, MaxInt);
    vText := '';
  end
  else
  begin
    vWord := Copy(vText, 2, vPos - 2);
    vText := Trim(Copy(vText, vPos + 1, MaxInt));
  end;
  // /start@MyBot is /start
  vPos := Pos('@', vWord);
  if vPos > 0 then
    vWord := Copy(vWord, 1, vPos - 1);
  if vWord = '' then
    Exit;
  AMessage.Kind := ckCommand;
  AMessage.Command := StringRAL(vWord);
  AMessage.CommandArgs := StringRAL(vText);
end;

procedure TRALChatBot.PrepareOut(AChannel: TRALChatChannel; ACopy: TRALChatMessage);
var
  vDegraded: TStringList;
  vIdx: Integer;
  vIds: string;
begin
  ACopy.ChannelName := AChannel.ChannelName;
  ACopy.Prepare(AChannel.Capabilities.Supported, AChannel.Capabilities.MaxButtons,
    FFallback, AChannel.ChannelName, vDegraded);
  try
    if vDegraded.Count > 0 then
    begin
      // a numeric answer in this conversation comes back as the button it stood for
      vIds := '';
      for vIdx := 0 to vDegraded.Count - 1 do
        vIds := vIds + vDegraded[vIdx] + #10;
      ActiveStore.Put(OptionsKey(AChannel.ChannelName, ACopy.Chat.Id),
        RawByteString(UTF8String(vIds)), Int64RAL(FSessionTimeout) * 1000);
    end;
  finally
    vDegraded.Free;
  end;
end;

procedure TRALChatBot.Queue(AChannel: TRALChatChannel; ACopy: TRALChatMessage);
var
  vPieces: TStringList;
  vIdx: Integer;
  vPiece: TRALChatMessage;
  vOutbox: TRALChatOutbox;
begin
  vOutbox := OutboxOf(AChannel);
  vPieces := TStringList.Create;
  try
    try
      PrepareOut(AChannel, ACopy);
      RALChatSplitText(ACopy.Text, AChannel.Capabilities.MaxText, vPieces);
    except
      ACopy.Free;
      raise;
    end;
    // the pieces before the last carry only text; the last carries everything else
    for vIdx := 0 to vPieces.Count - 2 do
    begin
      vPiece := TRALChatMessage.Create;
      vPiece.Chat := ACopy.Chat;
      vPiece.ChannelName := ACopy.ChannelName;
      vPiece.Text := AChannel.FormatText(StringRAL(vPieces[vIdx]), ACopy.TextFormat);
      vOutbox.Enqueue(okMessage, vPiece);
    end;
    ACopy.Text := AChannel.FormatText(StringRAL(vPieces[vPieces.Count - 1]), ACopy.TextFormat);
    ACopy.TextFormat := tfPlain;
  finally
    vPieces.Free;
  end;
  // the outbox owns it from here, even when it refuses it
  vOutbox.Enqueue(okMessage, ACopy);
end;

procedure TRALChatBot.RemoveInterceptor(AInterceptor: TRALChatInterceptor);
var
  vIdx: Integer;
begin
  FLock.Enter;
  try
    vIdx := FInterceptors.IndexOf(AInterceptor);
    if vIdx >= 0 then
      FInterceptors.Delete(vIdx);
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatBot.ReportError(AChannel: TRALChatChannel; const AText: string);
begin
  if AChannel <> nil then
    Log(string(AChannel.ChannelName) + ': ' + AText)
  else
    Log(AText);
  if Assigned(FOnError) then
    try
      FOnError(Self, AChannel, AText);
    except
      // an error handler that fails has nobody to tell
    end;
end;

procedure TRALChatBot.Reply(AReceived, AMessage: TRALChatMessage);
var
  vCopy: TRALChatMessage;
begin
  vCopy := AMessage.Clone;
  try
    vCopy.Chat := AReceived.Chat;
    Send(string(AReceived.ChannelName), vCopy);
  finally
    vCopy.Free;
  end;
end;

procedure TRALChatBot.Reply(AReceived: TRALChatMessage; const AText: StringRAL;
  AMarkup: Boolean);
var
  vMessage: TRALChatMessage;
begin
  vMessage := TRALChatMessage.Create;
  try
    vMessage.Text := AText;
    if AMarkup then
      vMessage.TextFormat := tfMarkup;
    Reply(AReceived, vMessage);
  finally
    vMessage.Free;
  end;
end;

procedure TRALChatBot.ResolveChoice(AChannel: TRALChatChannel; AMessage: TRALChatMessage);
var
  vChoice: Integer;
  vValue: RawByteString;
  vIds: TStringList;
  vKey: StringRAL;
begin
  if (AMessage.Kind <> ckText) or (AMessage.Chat.Id = '') then
    Exit;
  if not RALChatIsChoice(AMessage.Text, vChoice) then
    Exit;
  vKey := OptionsKey(AChannel.ChannelName, AMessage.Chat.Id);
  if not ActiveStore.Get(vKey, vValue) then
    Exit;
  vIds := TStringList.Create;
  try
    vIds.Text := string(UTF8String(vValue));
    if vChoice <= vIds.Count then
    begin
      AMessage.Kind := ckCallback;
      AMessage.CallbackData := StringRAL(vIds[vChoice - 1]);
      ActiveStore.Delete(vKey);
    end;
  finally
    vIds.Free;
  end;
end;

procedure TRALChatBot.Send(const AChannelName: string; AMessage: TRALChatMessage);
var
  vChannel: TRALChatChannel;
begin
  vChannel := FChannels.Find(AChannelName);
  if vChannel = nil then
    raise ERALChatError.CreateFmt(emChatNoChannel, [AChannelName]);
  Queue(vChannel, AMessage.Clone);
end;

procedure TRALChatBot.SendTyping(const AChannelName, AChatId: StringRAL);
var
  vChannel: TRALChatChannel;
  vMessage: TRALChatMessage;
begin
  vChannel := FChannels.Find(string(AChannelName));
  if vChannel = nil then
    raise ERALChatError.CreateFmt(emChatNoChannel, [string(AChannelName)]);
  if not vChannel.Capabilities.Supports(ccTyping) then
    Exit;
  vMessage := TRALChatMessage.Create;
  vMessage.ChatId := AChatId;
  OutboxOf(vChannel).Enqueue(okTyping, vMessage);
end;

function TRALChatBot.SendWait(const AChannelName: string; AMessage: TRALChatMessage;
  ATimeoutMs: Integer): StringRAL;
var
  vChannel: TRALChatChannel;
  vCopy: TRALChatMessage;
  vResult: TRALChatSendResult;
begin
  vChannel := FChannels.Find(AChannelName);
  if vChannel = nil then
    raise ERALChatError.CreateFmt(emChatNoChannel, [AChannelName]);
  vCopy := AMessage.Clone;
  try
    PrepareOut(vChannel, vCopy);
    vCopy.Text := vChannel.FormatText(vCopy.Text, vCopy.TextFormat);
    vCopy.TextFormat := tfPlain;
  except
    vCopy.Free;
    raise;
  end;
  if not OutboxOf(vChannel).SendWait(okMessage, vCopy, '', ATimeoutMs, vResult) then
    raise ERALChatError.CreateFmt(emChatSendTimeout, [AChannelName]);
  if not vResult.Ok then
    raise ERALChatError.CreateFmt(emChatSendFailed, [AChannelName, string(vResult.ErrorText)]);
  Result := vResult.MessageId;
end;

procedure TRALChatBot.SetChannels(AValue: TRALChatChannels);
begin
  FChannels.Assign(AValue);
end;

procedure TRALChatBot.SetCommands(AValue: TRALChatCommands);
begin
  FCommands.Assign(AValue);
end;

procedure TRALChatBot.SetPersistFile(const AValue: string);
begin
  if FPersistFile = AValue then
    Exit;
  FPersistFile := AValue;
  if not (csLoading in ComponentState) then
    FOwnStore.PersistFile := AValue;
end;

procedure TRALChatBot.SetStore(AValue: TRALKeyValueStore);
begin
  if FStore = AValue then
    Exit;
  FStore := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

procedure TRALChatBot.Stop;
var
  vIdx: Integer;
  vList: TList;
begin
  vList := TList.Create;
  try
    FLock.Enter;
    try
      for vIdx := 0 to FOutboxes.Count - 1 do
        vList.Add(FOutboxes.Objects[vIdx]);
      FOutboxes.Clear;
    finally
      FLock.Leave;
    end;
    for vIdx := 0 to vList.Count - 1 do
      TRALChatOutbox(vList[vIdx]).Free;
  finally
    vList.Free;
  end;
end;

function TRALChatBot.Supports(const AChannelName: string;
  ACapability: TRALChatCapability): Boolean;
var
  vChannel: TRALChatChannel;
begin
  vChannel := FChannels.Find(AChannelName);
  Result := (vChannel <> nil) and vChannel.Capabilities.Supports(ACapability);
end;

function TRALChatBot.UnixNow: Int64RAL;
begin
  if Assigned(FClock) then
    Result := FClock() div 1000
  else
    Result := MilliSecondsBetween(RALDateTimeToGMT(SysUtils.Now), UnixDateDelta) div 1000;
end;

end.
