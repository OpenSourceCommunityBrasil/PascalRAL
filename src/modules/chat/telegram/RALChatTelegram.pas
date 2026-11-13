/// The Telegram channel of the chat module: Bot API, webhook or getUpdates.
unit RALChatTelegram;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALJson, RALClient, RALRequest, RALResponse, RALParams,
  RALChatMessage, RALChatVerify, RALChatChannel, RALChatBot;

const
  /// Button id that makes a reply keyboard remove itself.
  cRALChatTelegramRemove = '@remove';
  /// Button id of a reply keyboard key that asks the user for the phone number.
  cRALChatTelegramRequestContact = '@contact';
  /// Button id of a reply keyboard key that asks the user for the location.
  cRALChatTelegramRequestLocation = '@location';
  /// Default address of the Bot API.
  cRALChatTelegramApi = 'https://api.telegram.org';

type
  /// The formatting dialect the channel sends.
  TRALChatTelegramTextMode = (
    /// MarkdownV2.
    ttmMarkdownV2,
    /// HTML.
    ttmHTML
  );

  /// What one call to the Bot API came to.
  TRALChatTelegramReply = record
    /// Whether Telegram accepted the call.
    Ok: Boolean;
    /// True when no answer came (connection, timeout).
    Failed: Boolean;
    /// The HTTP status; 0 when Failed.
    Status: Integer;
    /// The error_code of Telegram, or the status.
    ErrorCode: Integer;
    /// The description of the error, or the transport failure.
    Description: StringRAL;
    /// The seconds Telegram asks to wait (parameters.retry_after); 0 = none.
    RetryAfter: Integer;
    /// The whole answer; the caller frees it.
    Root: TRALJSONObject;
  end;

  TRALChatTelegram = class;

  { The thread of the long polling: it asks getUpdates, hands each update to the
    bot and confirms it with the next offset. }
  TRALChatTelegramPoller = class(TThread)
  private
    /// The client of the polling thread.
    FClient: TRALClient;
    /// Whether a request is in flight.
    FInRequest: Boolean;
    /// Guards FInRequest and the stop.
    FLock: TCriticalSection;
    /// The channel the thread works for.
    FOwner: TRALChatTelegram;
    /// Wakes the thread out of a wait.
    FWake: TEvent;

    /// Marks the start of a request; False when the thread was asked to stop.
    function BeginRequest: Boolean;
    /// Marks the end of a request; False when the thread was asked to stop meanwhile.
    function EndRequest: Boolean;
    /// Sends one call; False when the thread was asked to stop.
    function Remote(const AMethod: StringRAL; ABody: TRALJSONObject;
      out AReply: TRALChatTelegramReply): Boolean;
    /// Waits AMs milliseconds; False when the thread was asked to stop.
    function Sleep(AMs: Integer): Boolean;
  protected
    procedure Execute; override;
  public
    /// Starts the thread for AOwner.
    constructor Create(AOwner: TRALChatTelegram);
    destructor Destroy; override;

    { Asks the thread to stop. A thread in the middle of a request is left to end it
      alone (it never touches the channel again); another is waited for. }
    procedure Shutdown;
  end;

  { The thread that answers callback queries (the spinner of a pressed button)
    without holding the webhook. }
  TRALChatTelegramAnswerer = class(TThread)
  private
    /// Guards FQueue.
    FLock: TCriticalSection;
    /// The channel the thread works for.
    FOwner: TRALChatTelegram;
    /// The query ids waiting.
    FQueue: TStringList;
    /// Wakes the thread.
    FWake: TEvent;
  protected
    procedure Execute; override;
  public
    /// Starts the thread for AOwner.
    constructor Create(AOwner: TRALChatTelegram);
    destructor Destroy; override;

    /// Queues the answer of the query AId.
    procedure Add(const AId: StringRAL);
    /// Stops the thread and waits for it.
    procedure Quit;
  end;

  { The Telegram channel: webhook (verified by the secret header) or long polling,
    text in MarkdownV2 or HTML, media by upload, file_id or URL, inline and reply
    keyboards, edit, delete, typing, location, contact and polls. The token never
    reaches a log or an error text. }
  TRALChatTelegram = class(TRALChatChannel)
  private
    FActive: Boolean;
    FAllowedUpdates: StringRAL;
    /// The thread that answers callback queries.
    FAnswerer: TRALChatTelegramAnswerer;
    FApiBase: StringRAL;
    FAutoAnswerCallback: Boolean;
    FBot: TRALChatBot;
    FBotUsername: StringRAL;
    FButtonsPerRow: Integer;
    FClient: TRALClient;
    FEngineType: string;
    FLastUpdateId: Int64RAL;
    /// Guards the lazy objects.
    FLock: TCriticalSection;
    FMaxDownloadSize: Int64RAL;
    /// The channel's own client, made on first use.
    FOwnClient: TRALClient;
    /// Makes a key of its own for each private message.
    FPairCounter: IntegerRAL;
    /// The polling thread, or nil.
    FPoller: TRALChatTelegramPoller;
    FPollTimeout: Integer;
    FReceiveMode: TRALChatReceiveMode;
    FSecretToken: StringRAL;
    FTextMode: TRALChatTelegramTextMode;
    /// The verifier of the secret header (owned by the base).
    FVerifier: TRALChatSecretVerifier;

    /// Delivers a message of any kind (text, media, location, contact, poll).
    procedure DeliverMessage(AMessage: TRALChatMessage; var AResult: TRALChatSendResult);
    /// Turns a failed reply into a delivery result.
    procedure FillResult(const AMethod: StringRAL; const AReply: TRALChatTelegramReply;
      var AResult: TRALChatSendResult);
    /// Whether the text is a command for another bot (a group with several).
    function ForeignCommand(const AText: StringRAL): Boolean;
    /// The client in use: the linked one or the channel's own.
    function GetClient: TRALClient;
    /// Hides the token in AText.
    function Hide(const AText: StringRAL): StringRAL;
    /// Whether AMessage goes to a group (the 20 per minute rule).
    function IsGroupId(const AId: StringRAL): Boolean;
    /// The reply markup of AMessage, or nil.
    function MarkupOf(AMessage: TRALChatMessage; AInlineOnly: Boolean): TRALJSON;
    /// The method and field of the media kind of AMessage.
    procedure MediaMethod(AMessage: TRALChatMessage; out AMethod, AField: StringRAL);
    /// The full URL of a method of the Bot API.
    function MethodUrl(const AMethod: StringRAL): StringRAL;
    /// A new body with the chat of AMessage.
    function NewBody(AMessage: TRALChatMessage): TRALJSONObject;
    /// Makes the client of the long polling.
    function NewPollClient: TRALClient;
    /// Handles one update JSON: adds its messages to AMessages.
    procedure ParseUpdate(AUpdate: TRALJSONObject; AMessages: TRALChatMessageList);
    /// Asks Telegram to answer the callback query AId, on a thread of its own.
    procedure QueueAnswer(const AId: StringRAL);
    /// Makes the media of AMessage from the JSON of a Telegram message.
    procedure ReadMedia(AMsg: TRALJSONObject; AMessage: TRALChatMessage);
    /// Reads a Telegram message into AMessage; False when it is not for this bot.
    function ReadMessage(AMsg: TRALJSONObject; AMessage: TRALChatMessage; ADeep: Boolean): Boolean;
    /// Reads the answer of a call into AReply.
    procedure ReadReply(AStatus: Integer; const AText: StringRAL; out AReply: TRALChatTelegramReply);
    /// Reports an error to the bot, when there is one.
    procedure ReportError(const AText: string);
    /// Starts or stops the receiving.
    procedure SetActive(AValue: Boolean);
    /// Links the bot that receives the polled updates.
    procedure SetBot(AValue: TRALChatBot);
    /// Links the client.
    procedure SetClient(AValue: TRALClient);
    /// Sets the secret of the webhook verifier.
    procedure SetSecretToken(const AValue: StringRAL);
    /// Starts the polling thread.
    procedure StartPoller;
    /// Stops the polling thread.
    procedure StopPoller;
    /// The text as Telegram counts it, without the dialect's escapes.
    function Unformat(const AText: StringRAL): StringRAL;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Answers a callback query (a pressed button); AText shows as a toast or alert.
    procedure AnswerCallback(const AQueryId: StringRAL; const AText: StringRAL = '';
      AShowAlert: Boolean = False);
    { Makes one call to the Bot API (a JSON body, or multipart when AFileField names the
      field that carries AMedia) and says how it came out; never raises. }
    procedure Call(const AMethod: StringRAL; ABody: TRALJSONObject; const AFileField: StringRAL;
      AMedia: TRALChatMedia; out AReply: TRALChatTelegramReply;
      AClient: TRALClient = nil);
    procedure Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId: StringRAL; out AResult: TRALChatSendResult); override;
    function Dialect: TRALChatDialect; override;
    { Opens the file of a received media (getFile and the download from the API host);
      raises when it is above MaxDownloadSize or Telegram refuses. }
    function LoadMedia(AMedia: TRALChatMedia): TStream;
    function PairOf(AMessage: TRALChatMessage): StringRAL; override;
    procedure ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList); override;
    { Registers AUrl as the webhook of the bot, with SecretToken; False with the reason
      in AReport. }
    function RegisterWebhook(const AUrl: StringRAL; out AReport: StringRAL): Boolean;
    { Removes the webhook (needed before getUpdates works); False with the reason in
      AReport. }
    function RemoveWebhook(out AReport: StringRAL): Boolean;
    /// Starts the receiving the ReceiveMode asks for (a thread for polling).
    procedure Start;
    /// Stops the polling thread; a request in flight ends alone.
    procedure Stop;
    { Asks Telegram who the bot is (getMe): True with a text naming it, or False with the
      reason in AReport. It also fills BotUsername when empty. }
    function TestToken(out AReport: StringRAL): Boolean;

    /// The id of the last update taken (polling or webhook); the polling confirms it.
    property LastUpdateId: Int64RAL read FLastUpdateId;
  published
    { Whether the channel receives: starts the polling thread when ReceiveMode asks for
      it; applied after the form is loaded, never at design time. }
    property Active: Boolean read FActive write SetActive default False;
    { The update types asked from Telegram (allowed_updates), a JSON array text such as
      ["message","callback_query"]; empty = Telegram's default. }
    property AllowedUpdates: StringRAL read FAllowedUpdates write FAllowedUpdates;
    { The address of the Bot API, without the final slash; another one points the channel
      at a local Bot API server or a test server. }
    property ApiBase: StringRAL read FApiBase write FApiBase;
    /// Answers each pressed button at once, so that its spinner stops.
    property AutoAnswerCallback: Boolean read FAutoAnswerCallback write FAutoAnswerCallback
      default True;
    /// The bot that receives what the polling brings.
    property Bot: TRALChatBot read FBot write SetBot;
    { The username of the bot, without the at sign: a command written for another bot
      (/start@@other_bot) is not delivered. Filled by TestToken and by the polling. }
    property BotUsername: StringRAL read FBotUsername write FBotUsername;
    /// How many inline buttons go in a row.
    property ButtonsPerRow: Integer read FButtonsPerRow write FButtonsPerRow default 2;
    { The client that calls the API; leave its BaseURL empty (the channel sends whole
      URLs). Without one the channel makes its own, of EngineType. }
    property Client: TRALClient read FClient write SetClient;
    { The engine of the channel's own client (the engine unit must be in the program);
      empty = the first registered one. }
    property EngineType: string read FEngineType write FEngineType;
    /// The largest file taken from Telegram, in bytes; the Bot API allows 20 MB.
    property MaxDownloadSize: Int64RAL read FMaxDownloadSize write FMaxDownloadSize
      default 20971520;
    /// The seconds Telegram holds a getUpdates call open when there is nothing.
    property PollTimeout: Integer read FPollTimeout write FPollTimeout default 25;
    /// How updates arrive.
    property ReceiveMode: TRALChatReceiveMode read FReceiveMode write FReceiveMode
      default crmWebhook;
    { The secret_token of the webhook: Telegram sends it in a header and the module
      compares it in constant time; empty refuses every webhook. 1 to 256 letters,
      digits, underscore or hyphen. Set it from the environment in the program rather
      than in the form. }
    property SecretToken: StringRAL read FSecretToken write SetSecretToken;
    /// MarkdownV2 or HTML for the text that goes out.
    property TextMode: TRALChatTelegramTextMode read FTextMode write FTextMode
      default ttmMarkdownV2;
  end;

{ Makes AMessage remove the reply keyboard of the user (send it with some text, such as
  "ok"). }
procedure RALChatTelegramRemoveKeyboard(AMessage: TRALChatMessage);

implementation

type
  /// An exception of the channel that is not worth another attempt.
  ERALChatTelegramFinal = class(ERALChatError);

{ ---- JSON helpers ---- }

/// The member AName of AParent as an object, or nil.
function JObj(AParent: TRALJSON; const AName: StringRAL): TRALJSONObject;
var
  vItem: TRALJSON;
begin
  Result := nil;
  if (AParent <> nil) and (AParent.Kind = rjkObject) then
  begin
    vItem := TRALJSONObject(AParent).Find(AName);
    if (vItem <> nil) and (vItem.Kind = rjkObject) then
      Result := TRALJSONObject(vItem);
  end;
end;

/// The member AName of AParent as an array, or nil.
function JArr(AParent: TRALJSON; const AName: StringRAL): TRALJSONArray;
var
  vItem: TRALJSON;
begin
  Result := nil;
  if (AParent <> nil) and (AParent.Kind = rjkObject) then
  begin
    vItem := TRALJSONObject(AParent).Find(AName);
    if (vItem <> nil) and (vItem.Kind = rjkArray) then
      Result := TRALJSONArray(vItem);
  end;
end;

/// A scalar value as text.
function JText(AValue: TRALJSON): StringRAL;
begin
  Result := '';
  if AValue = nil then
    Exit;
  case AValue.Kind of
    rjkString:
      Result := AValue.AsString;
    rjkNumber:
      Result := TRALJSONNumber(AValue).Value;
    rjkBoolean:
      if AValue.AsBoolean then
        Result := 'true'
      else
        Result := 'false';
  end;
end;

/// The member AName of AParent as text; empty when missing.
function JStr(AParent: TRALJSON; const AName: StringRAL): StringRAL;
begin
  Result := '';
  if (AParent <> nil) and (AParent.Kind = rjkObject) then
    Result := JText(TRALJSONObject(AParent).Find(AName));
end;

/// The file_path and file_size of the result of a getFile call.
procedure FileInfo(ARoot: TRALJSONObject; out APath: StringRAL; out ASize: Int64);
var
  vFile: TRALJSONObject;
begin
  vFile := JObj(ARoot, 'result');
  APath := JStr(vFile, 'file_path');
  ASize := StrToInt64Def(string(JStr(vFile, 'file_size')), 0);
end;

/// Parses AText; nil when it is not JSON.
function TryParse(const AText: StringRAL): TRALJSON;
begin
  try
    Result := TRALJSON.ParseJSON(AText);
  except
    Result := nil;
  end;
end;

/// The member AName of AParent as an integer; ADefault when missing.
function JInt(AParent: TRALJSON; const AName: StringRAL; ADefault: Int64): Int64;
var
  vText: string;
begin
  vText := string(JStr(AParent, AName));
  Result := StrToInt64Def(vText, ADefault);
end;

/// A number JSON when AId is whole digits (a chat or message id), else a string.
function IdJson(const AId: StringRAL): TRALJSON;
var
  vIdx: Integer;
  vNumber: Boolean;
begin
  vNumber := AId <> '';
  for vIdx := 1 to Length(AId) do
    if not ((AId[vIdx] in ['0'..'9']) or ((vIdx = 1) and (AId[vIdx] = '-') and (Length(AId) > 1))) then
      vNumber := False;
  if vNumber then
    Result := TRALJSONNumber.Create(AId)
  else
    Result := TRALJSONString.Create(AId);
end;

/// A value as the text of a multipart field.
function FieldText(AValue: TRALJSON): StringRAL;
begin
  if AValue.Kind in [rjkString, rjkNumber, rjkBoolean] then
    Result := JText(AValue)
  else
    Result := AValue.ToJSON;
end;

/// The number of UTF-8 characters of AText.
function Utf8Length(const AText: StringRAL): Integer;
var
  vIdx: Integer;
begin
  Result := 0;
  for vIdx := 1 to Length(AText) do
    if (Ord(AText[vIdx]) and $C0) <> $80 then
      Inc(Result);
end;

/// The member AName of AParent as a float (a point, whatever the locale); 0 when missing.
function JFloat(AParent: TRALJSON; const AName: StringRAL): Double;
var
  vCode: Integer;
begin
  Val(string(JStr(AParent, AName)), Result, vCode);
  if vCode <> 0 then
    Result := 0;
end;

/// The parse_mode name of a text mode.
function ModeName(AMode: TRALChatTelegramTextMode): StringRAL;
begin
  if AMode = ttmHTML then
    Result := 'HTML'
  else
    Result := 'MarkdownV2';
end;

/// Reads the sender of a Telegram message or query into AMessage.
procedure ReadUser(AUser: TRALJSONObject; AMessage: TRALChatMessage);
var
  vUser: TRALChatUser;
begin
  vUser.Id := JStr(AUser, 'id');
  vUser.Name := StringRAL(Trim(string(JStr(AUser, 'first_name') + ' ' + JStr(AUser, 'last_name'))));
  if vUser.Name = '' then
    vUser.Name := JStr(AUser, 'username');
  vUser.Phone := '';
  AMessage.From := vUser;
end;

/// A location from a Telegram location object.
function MakeLocation(AObj: TRALJSONObject; const ATitle: StringRAL): TRALChatLocation;
begin
  Result.Valid := AObj <> nil;
  Result.Latitude := 0;
  Result.Longitude := 0;
  Result.Title := ATitle;
  if AObj <> nil then
  begin
    Result.Latitude := JFloat(AObj, 'latitude');
    Result.Longitude := JFloat(AObj, 'longitude');
  end;
end;

/// A contact from a Telegram contact object.
function MakeContact(AObj: TRALJSONObject): TRALChatContact;
begin
  Result.Valid := True;
  Result.Name := StringRAL(Trim(string(JStr(AObj, 'first_name') + ' ' + JStr(AObj, 'last_name'))));
  Result.Phone := JStr(AObj, 'phone_number');
end;

{ ---- unit routines ---- }

procedure RALChatTelegramRemoveKeyboard(AMessage: TRALChatMessage);
var
  vRow: TRALChatButtons;
begin
  vRow := nil;
  SetLength(vRow, 1);
  vRow[0].Id := cRALChatTelegramRemove;
  vRow[0].Caption := '';
  vRow[0].Url := '';
  AMessage.Keyboard := nil;
  AMessage.AddKeyboardRow(vRow);
end;

{ TRALChatTelegramPoller }

constructor TRALChatTelegramPoller.Create(AOwner: TRALChatTelegram);
begin
  FOwner := AOwner;
  FLock := TCriticalSection.Create;
  FWake := TEvent.Create(nil, False, False, '');
  FClient := AOwner.NewPollClient;
  inherited Create(False);
end;

destructor TRALChatTelegramPoller.Destroy;
begin
  FClient.Free;
  FWake.Free;
  FLock.Free;
  inherited Destroy;
end;

function TRALChatTelegramPoller.BeginRequest: Boolean;
begin
  FLock.Enter;
  try
    Result := not Terminated;
    if Result then
      FInRequest := True;
  finally
    FLock.Leave;
  end;
end;

function TRALChatTelegramPoller.EndRequest: Boolean;
begin
  FLock.Enter;
  try
    FInRequest := False;
    Result := not Terminated;
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatTelegramPoller.Execute;
var
  vSetup: Boolean;
  vDelay: Integer;
  vOffset: Int64;
  vReply: TRALChatTelegramReply;
  vBody: TRALJSONObject;
  vResult: TRALJSONArray;
  vIdx: Integer;
  vUpdate: TRALJSONObject;
  vId: Int64;
  vMessages: TRALChatMessageList;
  vJdx: Integer;
  vRoot: TRALJSONObject;

  function Fail(const AText: string): string;
  begin
    Result := AText;
  end;

begin
  vSetup := False;
  vDelay := 1000;
  while not Terminated do
  begin
    vReply.Root := nil;
    try
      try
        if not vSetup then
        begin
          vBody := TRALJSONObject.Create;
          try
            if FOwner.FReceiveMode = crmAuto then
            begin
              if not Remote('getWebhookInfo', vBody, vReply) then
                Break;
              if not vReply.Ok then
                raise ERALChatError.CreateFmt(emChatTgApi, ['getWebhookInfo', string(vReply.Description)]);
              vRoot := JObj(vReply.Root, 'result');
              if JStr(vRoot, 'url') <> '' then
                Break; // a webhook is registered: the webhook module receives
            end
            else
            begin
              if not Remote('deleteWebhook', vBody, vReply) then
                Break;
              if not vReply.Ok then
                raise ERALChatError.CreateFmt(emChatTgApi, ['deleteWebhook', string(vReply.Description)]);
            end;
          finally
            vBody.Free;
          end;
          FreeAndNil(vReply.Root);
          if FOwner.FBotUsername = '' then
          begin
            vBody := TRALJSONObject.Create;
            try
              if not Remote('getMe', vBody, vReply) then
                Break;
            finally
              vBody.Free;
            end;
            if vReply.Ok then
              FOwner.FBotUsername := JStr(JObj(vReply.Root, 'result'), 'username');
            FreeAndNil(vReply.Root);
          end;
          vSetup := True;
        end;
        vOffset := FOwner.FLastUpdateId + 1;
        vBody := TRALJSONObject.Create;
        try
          if FOwner.FLastUpdateId > 0 then
            vBody.Add('offset', vOffset);
          vBody.Add('timeout', FOwner.FPollTimeout);
          vBody.Add('limit', 100);
          if FOwner.FAllowedUpdates <> '' then
            vBody.Add('allowed_updates', TRALJSON.ParseJSON(FOwner.FAllowedUpdates));
          if not Remote('getUpdates', vBody, vReply) then
            Break;
        finally
          vBody.Free;
        end;
        if not vReply.Ok then
        begin
          if vReply.RetryAfter > 0 then
            vDelay := vReply.RetryAfter * 1000;
          raise ERALChatError.CreateFmt(emChatTgApi, ['getUpdates', string(vReply.Description)]);
        end;
        vDelay := 1000;
        vResult := JArr(vReply.Root, 'result');
        if vResult <> nil then
          for vIdx := 0 to vResult.Count - 1 do
          begin
            if Terminated then
              Break;
            if vResult[vIdx].Kind <> rjkObject then
              Continue;
            vUpdate := TRALJSONObject(vResult[vIdx]);
            vId := JInt(vUpdate, 'update_id', 0);
            if vId <= FOwner.FLastUpdateId then
              Continue; // already handled
            vMessages := TRALChatMessageList.Create;
            try
              try
                FOwner.ParseUpdate(vUpdate, vMessages);
              except
                on E: Exception do
                  FOwner.ReportError(string(FOwner.Hide(StringRAL(E.Message))));
              end;
              for vJdx := 0 to vMessages.Count - 1 do
              begin
                if (FOwner.FBot <> nil) and not Terminated then
                  FOwner.FBot.Receive(FOwner, vMessages[vJdx]);
              end;
            finally
              vMessages.Free;
            end;
            if Terminated then
              Break;
            FOwner.FLastUpdateId := vId;
          end;
      except
        on E: Exception do
        begin
          if Terminated then
            Break;
          FOwner.ReportError(string(FOwner.Hide(StringRAL(E.Message))));
          if not Sleep(vDelay) then
            Break;
          if vDelay < 30000 then
            vDelay := vDelay * 2;
        end;
      end;
    finally
      FreeAndNil(vReply.Root);
    end;
  end;
end;

function TRALChatTelegramPoller.Remote(const AMethod: StringRAL; ABody: TRALJSONObject;
  out AReply: TRALChatTelegramReply): Boolean;
var
  vUrl, vText: StringRAL;
  vResponse: TRALResponse;
  vStatus: Integer;
  vFailure: string;
begin
  AReply.Root := nil;
  vUrl := FOwner.MethodUrl(AMethod);
  vText := ABody.ToJSON;
  Result := BeginRequest;
  if not Result then
    Exit;
  vResponse := nil;
  vStatus := 0;
  vFailure := '';
  try
    try
      FClient.Request.Clear;
      FClient.Request.AddBody(vText, 'application/json');
      FClient.Post(vUrl, vResponse);
      vStatus := vResponse.StatusCode;
      vText := vResponse.ResponseText;
    except
      on E: Exception do
        vFailure := E.ClassName + ': ' + E.Message;
    end;
  finally
    vResponse.Free;
    Result := EndRequest;
  end;
  if not Result then
    Exit;
  if vFailure <> '' then
    raise ERALChatError.CreateFmt(emChatTgApi, [string(AMethod),
      string(FOwner.Hide(StringRAL(vFailure)))]);
  FOwner.ReadReply(vStatus, vText, AReply);
end;

procedure TRALChatTelegramPoller.Shutdown;
var
  vDetach: Boolean;
begin
  FLock.Enter;
  try
    Terminate;
    FWake.SetEvent;
    vDetach := FInRequest;
    if vDetach then
      FreeOnTerminate := True;
  finally
    FLock.Leave;
  end;
  if not vDetach then
  begin
    WaitFor;
    Free;
  end;
end;

function TRALChatTelegramPoller.Sleep(AMs: Integer): Boolean;
begin
  FWake.WaitFor(AMs);
  Result := not Terminated;
end;

{ TRALChatTelegramAnswerer }

constructor TRALChatTelegramAnswerer.Create(AOwner: TRALChatTelegram);
begin
  FOwner := AOwner;
  FLock := TCriticalSection.Create;
  FQueue := TStringList.Create;
  FWake := TEvent.Create(nil, False, False, '');
  inherited Create(False);
end;

destructor TRALChatTelegramAnswerer.Destroy;
begin
  FWake.Free;
  FQueue.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALChatTelegramAnswerer.Add(const AId: StringRAL);
begin
  FLock.Enter;
  try
    FQueue.Add(string(AId));
  finally
    FLock.Leave;
  end;
  FWake.SetEvent;
end;

procedure TRALChatTelegramAnswerer.Execute;
var
  vId: string;
begin
  while not Terminated do
  begin
    FWake.WaitFor(1000);
    repeat
      vId := '';
      FLock.Enter;
      try
        if FQueue.Count > 0 then
        begin
          vId := FQueue[0];
          FQueue.Delete(0);
        end;
      finally
        FLock.Leave;
      end;
      if vId <> '' then
        try
          FOwner.AnswerCallback(StringRAL(vId));
        except
          // an answer that fails only leaves the spinner of that button
        end;
    until (vId = '') or Terminated;
  end;
end;

procedure TRALChatTelegramAnswerer.Quit;
begin
  Terminate;
  FWake.SetEvent;
  WaitFor;
end;

{ TRALChatTelegram }

constructor TRALChatTelegram.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FApiBase := cRALChatTelegramApi;
  FAutoAnswerCallback := True;
  FButtonsPerRow := 2;
  FMaxDownloadSize := 20971520;
  FPollTimeout := 25;
  FTextMode := ttmMarkdownV2;
  ChannelName := 'telegram';
  Capabilities.Supported := [ccButtons, ccLists, ccKeyboard, ccEdit, ccDelete, ccTyping, ccPolls, ccMedia];
  Capabilities.MaxButtons := 100;
  Capabilities.MaxText := 4096;
  Capabilities.MaxMediaBytes := 52428800;
  // about 1 message a second to a chat, 30 a second in all, 20 a minute to a group
  Rates.PerChat := 1;
  Rates.PerChatBurst := 1;
  Rates.Total := 30;
  Rates.TotalBurst := 30;
  Rates.PerPair := 20 / 60;
  Rates.PerPairBurst := 20;
  FVerifier := TRALChatSecretVerifier.Create;
  FVerifier.HeaderName := 'X-Telegram-Bot-Api-Secret-Token';
  Verifier := FVerifier;
end;

destructor TRALChatTelegram.Destroy;
begin
  StopPoller;
  if FAnswerer <> nil then
  begin
    FAnswerer.Quit;
    FreeAndNil(FAnswerer);
  end;
  FOwnClient.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALChatTelegram.AnswerCallback(const AQueryId, AText: StringRAL; AShowAlert: Boolean);
var
  vBody: TRALJSONObject;
  vReply: TRALChatTelegramReply;
begin
  vBody := TRALJSONObject.Create;
  try
    vBody.Add('callback_query_id', AQueryId);
    if AText <> '' then
      vBody.Add('text', AText);
    if AShowAlert then
      vBody.Add('show_alert', True);
    Call('answerCallbackQuery', vBody, '', nil, vReply);
    vReply.Root.Free;
  finally
    vBody.Free;
  end;
end;

procedure TRALChatTelegram.Call(const AMethod: StringRAL; ABody: TRALJSONObject;
  const AFileField: StringRAL; AMedia: TRALChatMedia;
  out AReply: TRALChatTelegramReply; AClient: TRALClient);
var
  vClient: TRALClient;
  vResponse: TRALResponse;
  vStream: TStream;
  vParam: TRALParam;
  vIdx: Integer;
  vText: StringRAL;
  vStatus: Integer;
begin
  AReply.Ok := False;
  AReply.Failed := True;
  AReply.Status := 0;
  AReply.ErrorCode := 0;
  AReply.Description := '';
  AReply.RetryAfter := 0;
  AReply.Root := nil;
  vResponse := nil;
  vStream := nil;
  try
    try
      if AClient <> nil then
        vClient := AClient
      else
        vClient := GetClient;
      vClient.Request.Clear;
      if AFileField = '' then
        vClient.Request.AddBody(ABody.ToJSON, 'application/json')
      else
      begin
        for vIdx := 0 to ABody.Count - 1 do
          vClient.Request.AddField(ABody.Names[vIdx], FieldText(ABody.Items[vIdx]));
        vStream := AMedia.Open;
        vParam := vClient.Request.Params.AddParam(AFileField, vStream, rpkBODY);
        vParam.FileName := AMedia.FileName;
        if AMedia.MimeType <> '' then
          vParam.ContentType := AMedia.MimeType
        else
          vParam.ContentType := 'application/octet-stream';
      end;
      vClient.Post(MethodUrl(AMethod), vResponse);
      vStatus := vResponse.StatusCode;
      vText := vResponse.ResponseText;
      ReadReply(vStatus, vText, AReply);
    except
      on E: Exception do
      begin
        AReply.Failed := True;
        AReply.Ok := False;
        AReply.Description := Hide(StringRAL(E.ClassName + ': ' + E.Message));
      end;
    end;
  finally
    vStream.Free;
    vResponse.Free;
  end;
end;

procedure TRALChatTelegram.Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId: StringRAL; out AResult: TRALChatSendResult);
var
  vBody: TRALJSONObject;
  vReply: TRALChatTelegramReply;
  vMethod: StringRAL;
  vMarkup: TRALJSON;
begin
  AResult.Ok := False;
  AResult.MessageId := '';
  AResult.Transient := False;
  AResult.RetryAfterMs := 0;
  AResult.RetryScope := rsChat;
  AResult.ErrorText := '';
  AResult.Info := '';
  try
    if AKind = okMessage then
    begin
      DeliverMessage(AMessage, AResult);
      Exit;
    end;
    vBody := NewBody(AMessage);
    try
      case AKind of
        okTyping:
        begin
          vMethod := 'sendChatAction';
          vBody.Add('action', 'typing');
        end;
        okEdit:
        begin
          vMethod := 'editMessageText';
          vBody.Add('message_id', IdJson(ATargetId));
          vBody.Add('text', AMessage.Text);
          vBody.Add('parse_mode', ModeName(FTextMode));
          vMarkup := MarkupOf(AMessage, True);
          if vMarkup <> nil then
            vBody.Add('reply_markup', vMarkup);
        end;
      else
        vMethod := 'deleteMessage';
        vBody.Add('message_id', IdJson(ATargetId));
      end;
      Call(vMethod, vBody, '', nil, vReply);
      try
        FillResult(vMethod, vReply, AResult);
        // editing to the same text is not a failure
        if (not AResult.Ok) and (AKind = okEdit) and (vReply.ErrorCode = 400) and
           (Pos('not modified', string(vReply.Description)) > 0) then
        begin
          AResult.Ok := True;
          AResult.ErrorText := '';
          AResult.MessageId := ATargetId;
        end;
      finally
        vReply.Root.Free;
      end;
    finally
      vBody.Free;
    end;
  except
    on E: ERALChatTelegramFinal do
    begin
      AResult.Ok := False;
      AResult.Transient := False;
      AResult.ErrorText := Hide(StringRAL(E.Message));
    end;
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

procedure TRALChatTelegram.DeliverMessage(AMessage: TRALChatMessage;
  var AResult: TRALChatSendResult);
var
  vBody: TRALJSONObject;
  vReply: TRALChatTelegramReply;
  vMethod, vField: StringRAL;
  vMedia: TRALChatMedia;
  vMarkup, vOptions: TRALJSON;
  vReplyTo: TRALJSONObject;
  vFile: StringRAL;
  vLong: Boolean;
  vIdx: Integer;
  vOption: TRALJSONObject;
  vCount: Integer;
begin
  vMedia := nil;
  if AMessage.HasMedia then
    vMedia := AMessage.Media;
  vLong := False;
  vFile := '';
  vBody := NewBody(AMessage);
  try
    if AMessage.HasReplyTo and (AMessage.ReplyTo.Id <> '') then
    begin
      vReplyTo := TRALJSONObject.Create;
      vReplyTo.Add('message_id', IdJson(AMessage.ReplyTo.Id));
      vBody.Add('reply_parameters', vReplyTo);
    end;
    vMarkup := MarkupOf(AMessage, False);
    if vMedia <> nil then
    begin
      MediaMethod(AMessage, vMethod, vField);
      if vMedia.Data <> nil then
      begin
        if vMedia.Data.Size > Capabilities.MaxMediaBytes then
          raise ERALChatTelegramFinal.CreateFmt(emChatTgTooBig, [Capabilities.MaxMediaBytes]);
        vFile := vField;
        if vMedia.FileName = '' then
          vMedia.FileName := vField;
      end
      else if vMedia.Id <> '' then
        vBody.Add(vField, vMedia.Id)
      else if vMedia.Url <> '' then
        vBody.Add(vField, vMedia.Url)
      else
        raise ERALChatTelegramFinal.Create(emChatNoMedia);
      vLong := Utf8Length(AMessage.Text) > 1024;
      if (AMessage.Text <> '') and (not vLong) and (vMethod <> 'sendSticker') then
      begin
        vBody.Add('caption', AMessage.Text);
        vBody.Add('parse_mode', ModeName(FTextMode));
      end;
      if vLong or (vMethod = 'sendSticker') then
        FreeAndNil(vMarkup);
    end
    else
      case AMessage.Kind of
        ckLocation:
        begin
          vMethod := 'sendLocation';
          vBody.Add('latitude', AMessage.Location.Latitude);
          vBody.Add('longitude', AMessage.Location.Longitude);
        end;
        ckContact:
        begin
          vMethod := 'sendContact';
          vBody.Add('phone_number', AMessage.Contact.Phone);
          vBody.Add('first_name', AMessage.Contact.Name);
        end;
        ckPoll:
        begin
          vMethod := 'sendPoll';
          vCount := Length(AMessage.List.Items);
          if (AMessage.Text = '') or (vCount < 2) or (vCount > 10) then
            raise ERALChatTelegramFinal.Create(emChatTgPoll);
          vBody.Add('question', Unformat(AMessage.Text));
          vOptions := TRALJSONArray.Create;
          vBody.Add('options', vOptions);
          for vIdx := 0 to vCount - 1 do
          begin
            vOption := TRALJSONObject.Create;
            TRALJSONArray(vOptions).Add(vOption);
            vOption.Add('text', AMessage.List.Items[vIdx].Title);
          end;
        end;
      else
        vMethod := 'sendMessage';
        vBody.Add('text', AMessage.Text);
        vBody.Add('parse_mode', ModeName(FTextMode));
      end;
    if vMarkup <> nil then
      vBody.Add('reply_markup', vMarkup);
    Call(vMethod, vBody, vFile, vMedia, vReply);
    try
      FillResult(vMethod, vReply, AResult);
    finally
      vReply.Root.Free;
    end;
  finally
    vBody.Free;
  end;
  if AResult.Ok and vLong then
  begin
    // a caption holds 1024 characters: the text follows as a message of its own
    vBody := NewBody(AMessage);
    try
      vBody.Add('text', AMessage.Text);
      vBody.Add('parse_mode', ModeName(FTextMode));
      vMarkup := MarkupOf(AMessage, False);
      if vMarkup <> nil then
        vBody.Add('reply_markup', vMarkup);
      Call('sendMessage', vBody, '', nil, vReply);
      try
        FillResult('sendMessage', vReply, AResult);
      finally
        vReply.Root.Free;
      end;
    finally
      vBody.Free;
    end;
  end;
end;

function TRALChatTelegram.Dialect: TRALChatDialect;
begin
  if FTextMode = ttmHTML then
    Result := cdTelegramHTML
  else
    Result := cdTelegramMarkdownV2;
end;

function TRALChatTelegram.ForeignCommand(const AText: StringRAL): Boolean;
var
  vWord: string;
  vPos: Integer;
begin
  Result := False;
  if (FBotUsername = '') or (AText = '') or (AText[1] <> '/') then
    Exit;
  vWord := string(AText);
  vPos := 1;
  while (vPos <= Length(vWord)) and not CharInSet(vWord[vPos], [' ', #9, #10, #13]) do
    Inc(vPos);
  vWord := Copy(vWord, 1, vPos - 1);
  vPos := Pos('@', vWord);
  if vPos > 0 then
    Result := not SameText(Copy(vWord, vPos + 1, MaxInt), string(FBotUsername));
end;

procedure TRALChatTelegram.FillResult(const AMethod: StringRAL; const AReply: TRALChatTelegramReply;
  var AResult: TRALChatSendResult);
var
  vResult: TRALJSON;
begin
  AResult.Ok := AReply.Ok;
  if AReply.Ok then
  begin
    if AReply.Root <> nil then
    begin
      vResult := AReply.Root.Find('result');
      if (vResult <> nil) and (vResult.Kind = rjkObject) then
        AResult.MessageId := JStr(vResult, 'message_id');
    end;
    Exit;
  end;
  AResult.ErrorText := Hide(StringRAL(Format(emChatTgApi, [string(AMethod),
    string(AReply.Description)])));
  AResult.RetryScope := rsChat;
  if AReply.Failed then
    AResult.Transient := True
  else if AReply.ErrorCode = 429 then
  begin
    AResult.Transient := True;
    AResult.RetryAfterMs := Int64RAL(AReply.RetryAfter) * 1000;
  end
  else if AReply.Status >= 500 then
    AResult.Transient := True;
end;

function TRALChatTelegram.GetClient: TRALClient;
var
  vList: TStringList;
begin
  Result := FClient;
  if Result <> nil then
    Exit;
  FLock.Enter;
  try
    if FOwnClient = nil then
    begin
      FOwnClient := TRALClient.Create(nil);
      try
        if FEngineType <> '' then
          FOwnClient.EngineType := FEngineType
        else
        begin
          vList := TStringList.Create;
          try
            GetEngineList(vList);
            if vList.Count = 0 then
              raise ERALChatError.CreateFmt(emChatTgNoClient, [string(ChannelName)]);
            FOwnClient.EngineType := vList.Names[0];
            if FOwnClient.EngineType = '' then
              FOwnClient.EngineType := vList[0];
          finally
            vList.Free;
          end;
        end;
        FOwnClient.KeepCookies := False;
      except
        FreeAndNil(FOwnClient);
        raise;
      end;
    end;
    Result := FOwnClient;
  finally
    FLock.Leave;
  end;
end;

function TRALChatTelegram.Hide(const AText: StringRAL): StringRAL;
var
  vToken: StringRAL;
begin
  Result := AText;
  vToken := '';
  try
    vToken := GetToken;
  except
    // no token: nothing to hide
  end;
  if Length(vToken) >= 4 then
    Result := StringRAL(StringReplace(string(Result), string(vToken), '***', [rfReplaceAll]));
end;

function TRALChatTelegram.IsGroupId(const AId: StringRAL): Boolean;
begin
  Result := (AId <> '') and ((AId[1] = '-') or (AId[1] = '@'));
end;

function TRALChatTelegram.LoadMedia(AMedia: TRALChatMedia): TStream;
var
  vBody: TRALJSONObject;
  vReply: TRALChatTelegramReply;
  vPath, vUrl: StringRAL;
  vSize: Int64;
  vResponse: TRALResponse;
  vClient: TRALClient;
  vData: TStream;
begin
  if AMedia.Id = '' then
    raise ERALChatError.Create(emChatNoMedia);
  if (AMedia.Size > 0) and (AMedia.Size > FMaxDownloadSize) then
    raise ERALChatError.CreateFmt(emChatTgTooBig, [FMaxDownloadSize]);
  vBody := TRALJSONObject.Create;
  try
    vBody.Add('file_id', AMedia.Id);
    Call('getFile', vBody, '', nil, vReply);
  finally
    vBody.Free;
  end;
  try
    if not vReply.Ok then
      raise ERALChatError.CreateFmt(emChatTgApi, ['getFile', string(vReply.Description)]);
    FileInfo(vReply.Root, vPath, vSize);
  finally
    vReply.Root.Free;
  end;
  // the file comes only from the API host: a path cannot leave it or carry another URL
  if (vPath = '') or (vPath[1] = '/') or (Pos('..', string(vPath)) > 0) or
     (Pos('://', string(vPath)) > 0) or (Pos('\', string(vPath)) > 0) or
     (Pos('?', string(vPath)) > 0) or (Pos('#', string(vPath)) > 0) then
    raise ERALChatError.Create(emChatTgBadPath);
  if vSize > FMaxDownloadSize then
    raise ERALChatError.CreateFmt(emChatTgTooBig, [FMaxDownloadSize]);
  vUrl := FApiBase + '/file/bot' + GetToken + '/' + vPath;
  vClient := GetClient;
  vResponse := nil;
  Result := nil;
  try
    try
      vClient.Request.Clear;
      vClient.Get(vUrl, vResponse);
    except
      on E: Exception do
        raise ERALChatError.CreateFmt(emChatTgApi, ['file', string(Hide(StringRAL(E.Message)))]);
    end;
    if vResponse.StatusCode <> 200 then
      raise ERALChatError.CreateFmt(emChatTgApi, ['file', 'HTTP ' + IntToStr(vResponse.StatusCode)]);
    vData := vResponse.ResponseStream;
    if (vData <> nil) and (vData.Size > FMaxDownloadSize) then
      raise ERALChatError.CreateFmt(emChatTgTooBig, [FMaxDownloadSize]);
    Result := TMemoryStream.Create;
    try
      if vData <> nil then
      begin
        vData.Position := 0;
        Result.CopyFrom(vData, 0);
      end;
      Result.Position := 0;
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    vResponse.Free;
  end;
end;

function TRALChatTelegram.MarkupOf(AMessage: TRALChatMessage; AInlineOnly: Boolean): TRALJSON;
var
  vRoot, vObj: TRALJSONObject;
  vRows, vRow: TRALJSONArray;
  vIdx, vJdx, vPerRow: Integer;
  vButton: TRALChatButton;
  vKeys: TRALChatKeyboard;
begin
  Result := nil;
  if Length(AMessage.Buttons) > 0 then
  begin
    vPerRow := FButtonsPerRow;
    if vPerRow < 1 then
      vPerRow := 1;
    vRoot := TRALJSONObject.Create;
    vRows := TRALJSONArray.Create;
    vRoot.Add('inline_keyboard', vRows);
    Result := vRoot;
    vRow := nil;
    for vIdx := 0 to High(AMessage.Buttons) do
    begin
      vButton := AMessage.Buttons[vIdx];
      if (vButton.Url = '') and ((vButton.Id = '') or (Length(vButton.Id) > 64)) then
        raise ERALChatTelegramFinal.Create(emChatTgButtonId);
      if (vRow = nil) or (vRow.Count >= vPerRow) then
      begin
        vRow := TRALJSONArray.Create;
        vRows.Add(vRow);
      end;
      vObj := TRALJSONObject.Create;
      vRow.Add(vObj);
      vObj.Add('text', vButton.Caption);
      if vButton.Url <> '' then
        vObj.Add('url', vButton.Url)
      else
        vObj.Add('callback_data', vButton.Id);
    end;
    Exit;
  end;
  if (Length(AMessage.List.Items) > 0) and (AMessage.Kind <> ckPoll) then
  begin
    // a list is a column of callback buttons
    vRoot := TRALJSONObject.Create;
    vRows := TRALJSONArray.Create;
    vRoot.Add('inline_keyboard', vRows);
    Result := vRoot;
    for vIdx := 0 to High(AMessage.List.Items) do
    begin
      if (AMessage.List.Items[vIdx].Id = '') or (Length(AMessage.List.Items[vIdx].Id) > 64) then
        raise ERALChatTelegramFinal.Create(emChatTgButtonId);
      vRow := TRALJSONArray.Create;
      vRows.Add(vRow);
      vObj := TRALJSONObject.Create;
      vRow.Add(vObj);
      vObj.Add('text', AMessage.List.Items[vIdx].Title);
      vObj.Add('callback_data', AMessage.List.Items[vIdx].Id);
    end;
    Exit;
  end;
  vKeys := AMessage.Keyboard;
  if (Length(vKeys) = 0) or AInlineOnly then
    Exit;
  vRoot := TRALJSONObject.Create;
  Result := vRoot;
  if (Length(vKeys) = 1) and (Length(vKeys[0]) = 1) and (vKeys[0][0].Id = cRALChatTelegramRemove) then
  begin
    vRoot.Add('remove_keyboard', True);
    Exit;
  end;
  vRows := TRALJSONArray.Create;
  vRoot.Add('keyboard', vRows);
  for vIdx := 0 to High(vKeys) do
  begin
    vRow := TRALJSONArray.Create;
    vRows.Add(vRow);
    for vJdx := 0 to High(vKeys[vIdx]) do
    begin
      vObj := TRALJSONObject.Create;
      vRow.Add(vObj);
      vObj.Add('text', vKeys[vIdx][vJdx].Caption);
      if vKeys[vIdx][vJdx].Id = cRALChatTelegramRequestContact then
        vObj.Add('request_contact', True)
      else if vKeys[vIdx][vJdx].Id = cRALChatTelegramRequestLocation then
        vObj.Add('request_location', True);
    end;
  end;
  vRoot.Add('resize_keyboard', True);
end;

procedure TRALChatTelegram.MediaMethod(AMessage: TRALChatMessage; out AMethod, AField: StringRAL);
var
  vMime, vName: string;
begin
  vMime := LowerCase(string(AMessage.Media.MimeType));
  vName := LowerCase(string(AMessage.Media.FileName));
  case AMessage.Kind of
    ckImage:
    begin
      AMethod := 'sendPhoto';
      AField := 'photo';
    end;
    ckVideo:
    begin
      AMethod := 'sendVideo';
      AField := 'video';
    end;
    ckSticker:
    begin
      AMethod := 'sendSticker';
      AField := 'sticker';
    end;
    ckAudio:
      // an ogg/opus file is a voice note; anything else is music
      if (vMime = 'audio/ogg') or (vMime = 'audio/opus') or
         (ExtractFileExt(vName) = '.oga') or (ExtractFileExt(vName) = '.opus') then
      begin
        AMethod := 'sendVoice';
        AField := 'voice';
      end
      else
      begin
        AMethod := 'sendAudio';
        AField := 'audio';
      end;
  else
    AMethod := 'sendDocument';
    AField := 'document';
  end;
end;

function TRALChatTelegram.MethodUrl(const AMethod: StringRAL): StringRAL;
begin
  Result := FApiBase + '/bot' + GetToken + '/' + AMethod;
end;

function TRALChatTelegram.NewBody(AMessage: TRALChatMessage): TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('chat_id', IdJson(AMessage.Chat.Id));
end;

function TRALChatTelegram.NewPollClient: TRALClient;
var
  vList: TStringList;
begin
  Result := TRALClient.Create(nil);
  try
    if FClient <> nil then
    begin
      Result.EngineType := FClient.EngineType;
      Result.ConnectTimeout := FClient.ConnectTimeout;
      Result.SSL := FClient.SSL;
      Result.OnValidateServerCert := FClient.OnValidateServerCert;
    end
    else if FEngineType <> '' then
      Result.EngineType := FEngineType
    else
    begin
      vList := TStringList.Create;
      try
        GetEngineList(vList);
        if vList.Count = 0 then
          raise ERALChatError.CreateFmt(emChatTgNoClient, [string(ChannelName)]);
        Result.EngineType := vList.Names[0];
        if Result.EngineType = '' then
          Result.EngineType := vList[0];
      finally
        vList.Free;
      end;
    end;
    Result.KeepCookies := False;
    Result.RequestTimeout := (FPollTimeout + 15) * 1000;
  except
    Result.Free;
    raise;
  end;
end;

procedure TRALChatTelegram.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then
    Exit;
  if AComponent = FClient then
  begin
    StopPoller;
    FClient := nil;
  end;
  if AComponent = FBot then
  begin
    StopPoller;
    FBot := nil;
  end;
end;

function TRALChatTelegram.PairOf(AMessage: TRALChatMessage): StringRAL;
begin
  // the 20 per minute rule is the groups'; a private chat gets a key of its own
  if IsGroupId(AMessage.Chat.Id) then
    Result := 'g:' + AMessage.Chat.Id
  else
    Result := 'p:' + StringRAL(IntToStr(RALAtomicInc(FPairCounter)));
end;

procedure TRALChatTelegram.ParseUpdate(AUpdate: TRALJSONObject; AMessages: TRALChatMessageList);
const
  cKeys: array[0..3] of string = ('message', 'edited_message', 'channel_post', 'edited_channel_post');
var
  vIdx: Integer;
  vObj, vQuery: TRALJSONObject;
  vMessage: TRALChatMessage;
  vId: Int64;
begin
  vId := JInt(AUpdate, 'update_id', 0);
  if vId > 0 then
    FLastUpdateId := vId;
  for vIdx := 0 to High(cKeys) do
  begin
    vObj := JObj(AUpdate, StringRAL(cKeys[vIdx]));
    if vObj = nil then
      Continue;
    vMessage := TRALChatMessage.Create;
    try
      if ReadMessage(vObj, vMessage, True) then
      begin
        vMessage.EventId := StringRAL('tg' + IntToStr(vId));
        vMessage.ChannelName := ChannelName;
        if KeepRaw then
          vMessage.Raw := AUpdate.ToJSON;
        AMessages.AddMessage(vMessage);
        vMessage := nil;
      end;
    finally
      vMessage.Free;
    end;
    Exit;
  end;
  vQuery := JObj(AUpdate, 'callback_query');
  if vQuery = nil then
    Exit;
  vMessage := TRALChatMessage.Create;
  try
    vMessage.Kind := ckCallback;
    vMessage.Id := JStr(vQuery, 'id');
    vMessage.EventId := StringRAL('tg' + IntToStr(vId));
    vMessage.ChannelName := ChannelName;
    vMessage.CallbackData := JStr(vQuery, 'data');
    vObj := JObj(vQuery, 'message');
    if vObj <> nil then
    begin
      vMessage.ReplyTo.Id := JStr(vObj, 'message_id');
      ReadMessage(vObj, vMessage.ReplyTo, False);
      vMessage.Chat := vMessage.ReplyTo.Chat;
    end;
    vObj := JObj(vQuery, 'from');
    if vObj <> nil then
      ReadUser(vObj, vMessage);
    if FAutoAnswerCallback and (vMessage.Id <> '') then
      QueueAnswer(vMessage.Id);
    AMessages.AddMessage(vMessage);
    vMessage := nil;
  finally
    vMessage.Free;
  end;
end;

procedure TRALChatTelegram.ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList);
var
  vRoot: TRALJSON;
begin
  vRoot := TRALJSON.ParseJSON(ABody);
  try
    if vRoot.Kind <> rjkObject then
      raise ERALChatError.Create(emChatTgBadReply);
    ParseUpdate(TRALJSONObject(vRoot), AMessages);
  finally
    vRoot.Free;
  end;
end;

procedure TRALChatTelegram.QueueAnswer(const AId: StringRAL);
begin
  FLock.Enter;
  try
    if FAnswerer = nil then
      FAnswerer := TRALChatTelegramAnswerer.Create(Self);
  finally
    FLock.Leave;
  end;
  FAnswerer.Add(AId);
end;

procedure TRALChatTelegram.ReadMedia(AMsg: TRALJSONObject; AMessage: TRALChatMessage);
var
  vPhotos: TRALJSONArray;
  vSource: TRALJSONObject;
  vKind: TRALChatKind;
  vMime, vName: StringRAL;
  vIdx: Integer;
  vKeys: array[0..5] of string;
  vKinds: array[0..5] of TRALChatKind;
  vLoader: TRALChatMediaLoader;
begin
  vSource := nil;
  vKind := ckDocument;
  vMime := '';
  vPhotos := JArr(AMsg, 'photo');
  if (vPhotos <> nil) and (vPhotos.Count > 0) and (vPhotos[vPhotos.Count - 1].Kind = rjkObject) then
  begin
    vSource := TRALJSONObject(vPhotos[vPhotos.Count - 1]);
    vKind := ckImage;
    vMime := 'image/jpeg';
  end;
  vKeys[0] := 'document';  vKinds[0] := ckDocument;
  vKeys[1] := 'audio';     vKinds[1] := ckAudio;
  vKeys[2] := 'voice';     vKinds[2] := ckAudio;
  vKeys[3] := 'video';     vKinds[3] := ckVideo;
  vKeys[4] := 'video_note'; vKinds[4] := ckVideo;
  vKeys[5] := 'animation'; vKinds[5] := ckVideo;
  if vSource = nil then
    for vIdx := 0 to High(vKeys) do
    begin
      vSource := JObj(AMsg, StringRAL(vKeys[vIdx]));
      if vSource <> nil then
      begin
        vKind := vKinds[vIdx];
        Break;
      end;
    end;
  if vSource = nil then
  begin
    vSource := JObj(AMsg, 'sticker');
    if vSource <> nil then
      vKind := ckSticker;
  end;
  if vSource = nil then
    Exit;
  AMessage.Kind := vKind;
  if JStr(vSource, 'mime_type') <> '' then
    vMime := JStr(vSource, 'mime_type');
  vName := JStr(vSource, 'file_name');
  AMessage.Media.Id := JStr(vSource, 'file_id');
  AMessage.Media.Size := JInt(vSource, 'file_size', 0);
  AMessage.Media.MimeType := vMime;
  AMessage.Media.FileName := vName;
  vLoader := LoadMedia;
  AMessage.Media.Loader := vLoader;
end;

function TRALChatTelegram.ReadMessage(AMsg: TRALJSONObject; AMessage: TRALChatMessage;
  ADeep: Boolean): Boolean;
var
  vChat, vObj, vPoll: TRALJSONObject;
  vInfo: TRALChatInfo;
  vOptions: TRALJSONArray;
  vIdx: Integer;
  vText: StringRAL;
  vType: StringRAL;
begin
  Result := True;
  AMessage.Id := JStr(AMsg, 'message_id');
  AMessage.Timestamp := JInt(AMsg, 'date', 0);
  vChat := JObj(AMsg, 'chat');
  vInfo.Id := JStr(vChat, 'id');
  vType := JStr(vChat, 'type');
  if vType = 'private' then
    vInfo.ChatType := cctPrivate
  else if vType = 'channel' then
    vInfo.ChatType := cctChannel
  else
    vInfo.ChatType := cctGroup;
  vInfo.Title := JStr(vChat, 'title');
  if vInfo.Title = '' then
    vInfo.Title := StringRAL(Trim(string(JStr(vChat, 'first_name') + ' ' + JStr(vChat, 'last_name'))));
  if vInfo.Title = '' then
    vInfo.Title := JStr(vChat, 'username');
  AMessage.Chat := vInfo;
  vObj := JObj(AMsg, 'from');
  if vObj <> nil then
    ReadUser(vObj, AMessage);
  vText := JStr(AMsg, 'text');
  if vText = '' then
    vText := JStr(AMsg, 'caption');
  AMessage.Text := vText;
  AMessage.Kind := ckText;
  ReadMedia(AMsg, AMessage);
  vObj := JObj(AMsg, 'location');
  if (vObj <> nil) and (JObj(AMsg, 'venue') = nil) then
  begin
    AMessage.Kind := ckLocation;
    AMessage.Location := MakeLocation(vObj, '');
  end;
  vObj := JObj(AMsg, 'venue');
  if vObj <> nil then
  begin
    AMessage.Kind := ckLocation;
    AMessage.Location := MakeLocation(JObj(vObj, 'location'), JStr(vObj, 'title'));
  end;
  vObj := JObj(AMsg, 'contact');
  if vObj <> nil then
  begin
    AMessage.Kind := ckContact;
    AMessage.Contact := MakeContact(vObj);
  end;
  vPoll := JObj(AMsg, 'poll');
  if vPoll <> nil then
  begin
    AMessage.Kind := ckPoll;
    AMessage.Text := JStr(vPoll, 'question');
    vOptions := JArr(vPoll, 'options');
    if vOptions <> nil then
      for vIdx := 0 to vOptions.Count - 1 do
        AMessage.AddListItem(StringRAL(IntToStr(vIdx)), JStr(vOptions[vIdx], 'text'));
  end;
  if ADeep then
  begin
    vObj := JObj(AMsg, 'reply_to_message');
    if vObj <> nil then
      ReadMessage(vObj, AMessage.ReplyTo, False);
  end;
  if (AMessage.Kind = ckText) and ForeignCommand(AMessage.Text) then
    Result := False;
end;

procedure TRALChatTelegram.ReadReply(AStatus: Integer; const AText: StringRAL;
  out AReply: TRALChatTelegramReply);
var
  vRoot: TRALJSON;
  vParams: TRALJSONObject;
begin
  AReply.Ok := False;
  AReply.Failed := False;
  AReply.Status := AStatus;
  AReply.ErrorCode := AStatus;
  AReply.Description := '';
  AReply.RetryAfter := 0;
  AReply.Root := nil;
  vRoot := TryParse(AText);
  if (vRoot <> nil) and (vRoot.Kind = rjkObject) then
  begin
    AReply.Root := TRALJSONObject(vRoot);
    AReply.Ok := (AStatus = 200) and (JStr(vRoot, 'ok') = 'true');
    if not AReply.Ok then
    begin
      AReply.ErrorCode := Integer(JInt(vRoot, 'error_code', AStatus));
      AReply.Description := JStr(vRoot, 'description');
      vParams := JObj(vRoot, 'parameters');
      AReply.RetryAfter := Integer(JInt(vParams, 'retry_after', 0));
    end;
  end
  else
  begin
    vRoot.Free;
    AReply.Description := StringRAL('HTTP ' + IntToStr(AStatus));
  end;
  if (not AReply.Ok) and (AReply.Description = '') then
    AReply.Description := StringRAL('HTTP ' + IntToStr(AStatus));
end;

procedure TRALChatTelegram.ReportError(const AText: string);
begin
  if FBot <> nil then
    FBot.ReportError(Self, AText);
end;

function TRALChatTelegram.RegisterWebhook(const AUrl: StringRAL; out AReport: StringRAL): Boolean;
var
  vBody: TRALJSONObject;
  vReply: TRALChatTelegramReply;
  vIdx: Integer;
begin
  Result := False;
  if (FSecretToken = '') or (Length(FSecretToken) > 256) then
  begin
    AReport := StringRAL(emChatTgBadSecret);
    Exit;
  end;
  for vIdx := 1 to Length(FSecretToken) do
    if not (FSecretToken[vIdx] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-']) then
    begin
      AReport := StringRAL(emChatTgBadSecret);
      Exit;
    end;
  vBody := TRALJSONObject.Create;
  try
    vBody.Add('url', AUrl);
    vBody.Add('secret_token', FSecretToken);
    if FAllowedUpdates <> '' then
      vBody.Add('allowed_updates', TRALJSON.ParseJSON(FAllowedUpdates));
    Call('setWebhook', vBody, '', nil, vReply);
  finally
    vBody.Free;
  end;
  try
    Result := vReply.Ok;
    if Result then
      AReport := StringRAL(Format(cmChatTgWebhookSet, [string(AUrl)]))
    else
      AReport := Hide(StringRAL(Format(emChatTgApi, ['setWebhook', string(vReply.Description)])));
  finally
    vReply.Root.Free;
  end;
end;

function TRALChatTelegram.RemoveWebhook(out AReport: StringRAL): Boolean;
var
  vBody: TRALJSONObject;
  vReply: TRALChatTelegramReply;
begin
  vBody := TRALJSONObject.Create;
  try
    Call('deleteWebhook', vBody, '', nil, vReply);
  finally
    vBody.Free;
  end;
  try
    Result := vReply.Ok;
    if Result then
      AReport := StringRAL(cmChatTgWebhookRemoved)
    else
      AReport := Hide(StringRAL(Format(emChatTgApi, ['deleteWebhook', string(vReply.Description)])));
  finally
    vReply.Root.Free;
  end;
end;

procedure TRALChatTelegram.SetActive(AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  if [csDesigning, csLoading] * ComponentState <> [] then
    Exit;
  if AValue then
    Start
  else
    StopPoller;
end;

procedure TRALChatTelegram.SetBot(AValue: TRALChatBot);
begin
  if FBot = AValue then
    Exit;
  StopPoller;
  if FBot <> nil then
    FBot.RemoveFreeNotification(Self);
  FBot := AValue;
  if FBot <> nil then
    FBot.FreeNotification(Self);
end;

procedure TRALChatTelegram.SetClient(AValue: TRALClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALChatTelegram.SetSecretToken(const AValue: StringRAL);
begin
  FSecretToken := AValue;
  FVerifier.Secret := AValue;
end;

procedure TRALChatTelegram.Start;
begin
  if FReceiveMode = crmWebhook then
    Exit;
  if FBot = nil then
    raise ERALChatError.Create(emChatNoBot);
  StartPoller;
end;

procedure TRALChatTelegram.StartPoller;
begin
  FLock.Enter;
  try
    if FPoller = nil then
      FPoller := TRALChatTelegramPoller.Create(Self);
  finally
    FLock.Leave;
  end;
end;

procedure TRALChatTelegram.Stop;
begin
  StopPoller;
end;

procedure TRALChatTelegram.StopPoller;
var
  vPoller: TRALChatTelegramPoller;
begin
  FLock.Enter;
  try
    vPoller := FPoller;
    FPoller := nil;
  finally
    FLock.Leave;
  end;
  if vPoller <> nil then
    vPoller.Shutdown;
end;

function TRALChatTelegram.TestToken(out AReport: StringRAL): Boolean;
var
  vBody: TRALJSONObject;
  vReply: TRALChatTelegramReply;
begin
  vBody := TRALJSONObject.Create;
  try
    Call('getMe', vBody, '', nil, vReply);
  finally
    vBody.Free;
  end;
  try
    Result := vReply.Ok;
    if Result then
    begin
      if FBotUsername = '' then
        FBotUsername := JStr(JObj(vReply.Root, 'result'), 'username');
      AReport := StringRAL(Format(cmChatTgTokenOk, [string(JStr(JObj(vReply.Root, 'result'), 'username'))]));
    end
    else
      AReport := Hide(StringRAL(Format(emChatTgApi, ['getMe', string(vReply.Description)])));
  finally
    vReply.Root.Free;
  end;
end;

function TRALChatTelegram.Unformat(const AText: StringRAL): StringRAL;
var
  vIdx, vOut: Integer;
  vSkip: Boolean;

  procedure Put(AChar: AnsiChar);
  begin
    Inc(vOut);
    Result[vOut] := AChar;
  end;

  function Entity(const AName: StringRAL; AChar: AnsiChar): Boolean;
  begin
    Result := Copy(AText, vIdx, Length(AName)) = AName;
    if Result then
    begin
      Put(AChar);
      Inc(vIdx, Length(AName));
    end;
  end;

begin
  SetLength(Result, Length(AText));
  vOut := 0;
  vIdx := 1;
  vSkip := False;
  while vIdx <= Length(AText) do
  begin
    if FTextMode = ttmMarkdownV2 then
    begin
      if (AText[vIdx] = '\') and (vIdx < Length(AText)) then
        Inc(vIdx);
      Put(AText[vIdx]);
      Inc(vIdx);
    end
    else if AText[vIdx] = '<' then
    begin
      vSkip := True;
      Inc(vIdx);
    end
    else if AText[vIdx] = '>' then
    begin
      vSkip := False;
      Inc(vIdx);
    end
    else if vSkip then
      Inc(vIdx)
    else if not (Entity('&lt;', '<') or Entity('&gt;', '>') or Entity('&quot;', '"') or
      Entity('&amp;', '&')) then
    begin
      Put(AText[vIdx]);
      Inc(vIdx);
    end;
  end;
  SetLength(Result, vOut);
end;

procedure TRALChatTelegram.Loaded;
begin
  inherited Loaded;
  if FActive and not (csDesigning in ComponentState) then
    try
      Start;
    except
      FActive := False;
      raise;
    end;
end;

end.
