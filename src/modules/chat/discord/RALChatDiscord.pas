/// The Discord channel of the chat module: interactions, REST v10 and the gateway.
unit RALChatDiscord;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALJson, RALClient, RALWebSocket, RALWebSocketClient,
  RALEd25519, RALChatMessage, RALChatVerify, RALChatChannel, RALChatBot, RALChatHttp;

const
  /// Default address of the REST API.
  cRALChatDiscordApi = 'https://discord.com/api/v10';
  /// custom_id of the select menu that shows a list.
  cRALChatDiscordList = 'ral:list';

type
  TRALChatDiscord = class;

  /// What one call to the REST API came to.
  TRALChatDiscordReply = record
    /// Whether Discord accepted the call (a 2xx).
    Ok: Boolean;
    /// True when no answer came.
    Failed: Boolean;
    /// The HTTP status; 0 when Failed.
    Status: Integer;
    /// The code Discord gave, or 0.
    Code: Integer;
    /// The message of the error, or the transport failure.
    Description: StringRAL;
    /// Milliseconds Discord asks to wait; 0 = none.
    RetryAfterMs: Int64RAL;
    /// Whether the wait is the whole application's (a global limit).
    Global: Boolean;
    /// The answer parsed (an object), or nil; the caller frees it.
    Root: TRALJSONObject;
  end;

  /// An interaction that can still be answered with its token (valid for 15 minutes).
  TRALChatDiscordPending = record
    /// The interaction id.
    Id: StringRAL;
    /// The interaction token.
    Token: StringRAL;
    /// The channel of the interaction.
    ChatId: StringRAL;
    /// When it stops being valid, in seconds of the tick clock.
    Expires: Int64RAL;
    /// True while the deferred "thinking" message waits for its first answer.
    Deferred: Boolean;
  end;

  /// The interactions that can still be answered.
  TRALChatDiscordPendings = array of TRALChatDiscordPending;

  /// The thread that restarts the gateway connection.
  TRALChatDiscordReconnector = class(TThread)
  private
    /// The object the thread works for.
    FOwner: TRALChatDiscord;
  protected
    procedure Execute; override;
  public
    /// Starts the thread for AOwner; it frees itself.
    constructor Create(AOwner: TRALChatDiscord);
  end;

  /// The thread that sends the heartbeat of the gateway.
  TRALChatDiscordBeat = class(TThread)
  private
    /// Milliseconds between two heartbeats.
    FInterval: Integer;
    /// The object the thread works for.
    FOwner: TRALChatDiscord;
    /// Wakes the thread out of a wait.
    FWake: TEvent;
  protected
    procedure Execute; override;
  public
    /// Starts the thread for AOwner, beating every AInterval milliseconds.
    constructor Create(AOwner: TRALChatDiscord; AInterval: Integer);
    destructor Destroy; override;

    /// Stops the thread and waits for it.
    procedure Quit;
  end;

  { The Discord channel: interactions endpoint (Ed25519 verified, PONG and deferred
    answers), messages through REST with the bot token, buttons and select menus,
    embeds, attachments, edit, delete, typing and the gateway as the polling mode.
    The token never reaches a log or an error text. }
  TRALChatDiscord = class(TRALChatChannel)
  private
    FActive: Boolean;
    FApiBase: StringRAL;
    FApplicationId: StringRAL;
    /// The heartbeat thread of the gateway, or nil.
    FBeat: TRALChatDiscordBeat;
    FBot: TRALChatBot;
    /// The id of the bot itself, whose messages are not delivered.
    FBotUserId: StringRAL;
    FButtonsPerRow: Integer;
    /// The direct message channels opened, as user=channel lines.
    FDmCache: TStringList;
    FEmbedColor: Integer;
    FGatewayUrl: StringRAL;
    /// The HTTP client of the channel.
    FHttp: TRALChatHttp;
    FIntents: Integer;
    /// Guards the lazy and shared state.
    FLock: TCriticalSection;
    FMaxDownloadSize: Int64RAL;
    /// The interactions that can still be answered with their token.
    FPending: TRALChatDiscordPendings;
    FPublicKey: StringRAL;
    FReceiveMode: TRALChatReceiveMode;
    /// The sequence number of the last gateway event.
    FSeq: Int64RAL;
    /// The id of the gateway session, to resume it.
    FSessionId: StringRAL;
    /// The WebSocket connection of the receiving mode, or nil.
    FSocket: TRALWebSocketClient;
    /// Whether the connection is being closed.
    FStopping: Boolean;
    FUseEmbeds: Boolean;
    /// The verifier of the webhook (owned by the base).
    FVerifier: TRALChatEventVerifier;

    /// Remembers an interaction so that a reply can use its token.
    procedure AddPending(const AId, AToken, AChatId: StringRAL; ADeferred: Boolean);
    /// The headers with the bot token; the caller frees them.
    function AuthHeaders: TStrings;
    /// Sends one heartbeat.
    procedure Beat;
    /// The JSON body of a message or of its edit.
    function BuildBody(AMessage: TRALChatMessage; AEdit: Boolean): TRALJSONObject;
    /// Makes one call to the API and says how it came out; never raises.
    procedure Call(AMethod: TRALMethod; const APath: StringRAL; ABody: TRALJSONObject;
      AMedia: TRALChatMedia; AAuth: Boolean; out AReply: TRALChatDiscordReply);
    /// The Discord components (buttons, select menu) of AMessage, or nil.
    function ComponentsOf(AMessage: TRALChatMessage): TRALJSONArray;
    /// The channel id to write to; opens a direct message channel for "u:" ids.
    function EnsureChannel(const AChatId: StringRAL; out AChannelId: StringRAL): Boolean;
    /// Turns a reply into a delivery result.
    procedure FillResult(const AWhat: StringRAL; const AReply: TRALChatDiscordReply;
      var AResult: TRALChatSendResult);
    /// The gateway connection closed.
    procedure GatewayClose(ASender: TRALWebSocketClient; ACode: Word; const AReason: StringRAL);
    /// The gateway connection failed.
    procedure GatewayError(ASender: TRALWebSocketClient; AError: Exception);
    /// A gateway payload arrived.
    procedure GatewayMessage(ASender: TRALWebSocketClient; AMessage: TRALWebSocketMessage);
    function GetClient: TRALClient;
    function GetEngineType: string;
    /// Hides the secrets in AText.
    function Hide(const AText: StringRAL): StringRAL;
    /// Downloads a received attachment, from the platform's hosts only.
    function LoadMedia(AMedia: TRALChatMedia): TStream;
    /// Whether AUrl is a host an attachment may come from.
    function MediaHostOk(const AUrl: StringRAL): Boolean;
    /// Reads a MESSAGE_CREATE event into AMessages.
    procedure ParseGatewayMessage(AObj: TRALJSONObject; AMessages: TRALChatMessageList);
    /// Reads an interaction into AMessages and remembers its token.
    procedure ParseInteraction(AObj: TRALJSONObject; AMessages: TRALChatMessageList);
    /// Reads a Discord user into the sender record.
    function ReadUser(AUser: TRALJSONObject): TRALChatUser;
    /// Reports an error to the bot, when there is one.
    procedure ReportError(const AText: string);
    /// Sends the identify (or the resume) payload of the gateway.
    procedure SendIdentify;
    procedure SetActive(AValue: Boolean);
    procedure SetBot(AValue: TRALChatBot);
    procedure SetClient(AValue: TRALClient);
    procedure SetEngineType(const AValue: string);
    procedure SetPublicKey(const AValue: StringRAL);
    procedure SetUseEmbeds(AValue: Boolean);
    /// Finds the interaction a message answers with its token.
    function TakePending(AMessage: TRALChatMessage; out APending: TRALChatDiscordPending;
      out AFollowUp: Boolean): Boolean;
    /// Checks the Ed25519 signature of an interaction.
    procedure VerifyRequest(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
      var ATimestamp: Int64RAL; var AValid: Boolean);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Acknowledge(const ABody: StringRAL; AMessages: TRALChatMessageList;
      out AStatus: Integer; out AContentType, AText: StringRAL): Boolean; override;
    procedure Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId: StringRAL; out AResult: TRALChatSendResult); override;
    function Dialect: TRALChatDialect; override;
    { Handles one payload of the gateway (what the socket delivers): public so that a
      program can feed it, and the tests do. }
    procedure HandleGatewayPayload(const AText: StringRAL);
    procedure ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList); override;
    { Registers a slash command of the application; False with the reason in
      AReport. }
    function RegisterCommand(const AName, ADescription: StringRAL; out AReport: StringRAL): Boolean;
    /// Opens the gateway connection (ReceiveMode crmPolling or crmAuto).
    procedure Start;
    /// Closes the gateway connection.
    procedure Stop;
    { Asks Discord who the bot is: True with a text naming it, or False with the
      reason in AReport. Fills ApplicationId when empty. }
    function TestToken(out AReport: StringRAL): Boolean;
  published
    /// Whether the channel opens the gateway (never at design time).
    property Active: Boolean read FActive write SetActive default False;
    { The address of the REST API, without the final slash; another points the channel at
      a test server. }
    property ApiBase: StringRAL read FApiBase write FApiBase;
    { The id of the application: the interactions follow-ups go to it; filled by
      TestToken and by the first interaction when empty. }
    property ApplicationId: StringRAL read FApplicationId write FApplicationId;
    /// The bot that receives what the gateway brings.
    property Bot: TRALChatBot read FBot write SetBot;
    /// How many buttons go in a row (1 to 5).
    property ButtonsPerRow: Integer read FButtonsPerRow write FButtonsPerRow default 5;
    { The client that calls the API; leave its BaseURL empty (the channel sends whole
      URLs). Without one the channel makes its own, of EngineType. }
    property Client: TRALClient read GetClient write SetClient;
    /// The color of an embed, as 0xRRGGBB.
    property EmbedColor: Integer read FEmbedColor write FEmbedColor default 5793266;
    /// The engine of the channel's own client; empty = the first registered.
    property EngineType: string read GetEngineType write SetEngineType;
    { The gateway address; empty = asked of GET gateway/bot. Another points the gateway
      mode at a test server. }
    property GatewayUrl: StringRAL read FGatewayUrl write FGatewayUrl;
    { The gateway intents; the default is guild and direct messages plus the message
      content (a privileged intent to turn on in the developer portal). }
    property Intents: Integer read FIntents write FIntents default 37376;
    /// The largest attachment taken from Discord, in bytes.
    property MaxDownloadSize: Int64RAL read FMaxDownloadSize write FMaxDownloadSize
      default 26214400;
    { The public key of the application (hexadecimal, from the developer portal): the
      Ed25519 key that verifies the interactions; empty refuses every webhook. }
    property PublicKey: StringRAL read FPublicKey write SetPublicKey;
    /// How messages arrive: interactions by webhook or the gateway.
    property ReceiveMode: TRALChatReceiveMode read FReceiveMode write FReceiveMode
      default crmWebhook;
    { Sends the text as the description of an embed (4096 characters) instead of the
      content (2000). }
    property UseEmbeds: Boolean read FUseEmbeds write SetUseEmbeds default False;
  end;

implementation

type
  /// An exception of the channel that is not worth another attempt.
  ERALChatDiscordFinal = class(ERALChatError);

const
  /// The longest content of a message.
  cMaxContent = 2000;
  /// The longest description of an embed.
  cMaxEmbed = 4096;

/// The bytes of a text.
function ToBytes(const AText: StringRAL): TBytes;
begin
  SetLength(Result, Length(AText));
  if Length(AText) > 0 then
    Move(AText[1], Result[0], Length(AText));
end;

{ TRALChatDiscordReconnector }

constructor TRALChatDiscordReconnector.Create(AOwner: TRALChatDiscord);
begin
  FOwner := AOwner;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TRALChatDiscordReconnector.Execute;
begin
  try
    FOwner.Stop;
    FOwner.FStopping := False;
    if FOwner.FActive then
      FOwner.Start;
  except
    on E: Exception do
      FOwner.ReportError(string(FOwner.Hide(StringRAL(E.Message))));
  end;
end;

{ TRALChatDiscordBeat }

constructor TRALChatDiscordBeat.Create(AOwner: TRALChatDiscord; AInterval: Integer);
begin
  FOwner := AOwner;
  FInterval := AInterval;
  FWake := TEvent.Create(nil, False, False, '');
  inherited Create(False);
end;

destructor TRALChatDiscordBeat.Destroy;
begin
  FWake.Free;
  inherited Destroy;
end;

procedure TRALChatDiscordBeat.Execute;
begin
  // the first beat comes after a random part of the interval, as Discord asks
  if FWake.WaitFor(Random(FInterval) + 1) = wrSignaled then
    Exit;
  while not Terminated do
  begin
    FOwner.Beat;
    if FWake.WaitFor(FInterval) = wrSignaled then
      Break;
  end;
end;

procedure TRALChatDiscordBeat.Quit;
begin
  Terminate;
  FWake.SetEvent;
  WaitFor;
end;

{ TRALChatDiscord }

constructor TRALChatDiscord.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FDmCache := TStringList.Create;
  FHttp := TRALChatHttp.Create;
  FHttp.ChannelName := 'discord';
  FApiBase := cRALChatDiscordApi;
  FButtonsPerRow := 5;
  FEmbedColor := 5793266;
  FIntents := 37376;
  FMaxDownloadSize := 26214400;
  ChannelName := 'discord';
  ReplayWindow := 300;
  Capabilities.Supported := [ccButtons, ccLists, ccEdit, ccDelete, ccTyping, ccMedia];
  Capabilities.MaxButtons := 25;
  Capabilities.MaxText := cMaxContent;
  // the default upload limit of a server without boosts
  Capabilities.MaxMediaBytes := 10485760;
  // 5 messages per 5 seconds to a channel, 50 requests per second in all
  Rates.PerChat := 1;
  Rates.PerChatBurst := 5;
  Rates.Total := 50;
  Rates.TotalBurst := 50;
  FVerifier := TRALChatEventVerifier.Create;
  FVerifier.OnVerify := VerifyRequest;
  Verifier := FVerifier;
end;

destructor TRALChatDiscord.Destroy;
begin
  Stop;
  FHttp.Free;
  FDmCache.Free;
  FLock.Free;
  inherited Destroy;
end;

function TRALChatDiscord.Acknowledge(const ABody: StringRAL; AMessages: TRALChatMessageList;
  out AStatus: Integer; out AContentType, AText: StringRAL): Boolean;
var
  vRoot: TRALJSONObject;
  vType: Int64;
begin
  AStatus := 200;
  AContentType := 'application/json';
  vRoot := RALChatParseObject(ABody);
  try
    vType := RALChatJInt(vRoot, 'type', 0);
  finally
    vRoot.Free;
  end;
  case vType of
    1: AText := '{"type":1}';
    2: AText := '{"type":5}';
    4: AText := '{"type":8,"data":{"choices":[]}}';
  else
    AText := '{"type":6}';
  end;
  Result := True;
end;

procedure TRALChatDiscord.AddPending(const AId, AToken, AChatId: StringRAL; ADeferred: Boolean);
var
  vIdx, vNow: Integer;
  vKeep: TRALChatDiscordPendings;
begin
  if (AId = '') or (AToken = '') then
    Exit;
  FLock.Enter;
  try
    vNow := Integer(RALTickMicro div 1000000);
    // the platform may deliver an interaction twice: the first record stands
    for vIdx := 0 to High(FPending) do
      if (FPending[vIdx].Id = AId) and (FPending[vIdx].Expires > vNow) then
        Exit;
    SetLength(vKeep, 0);
    for vIdx := 0 to High(FPending) do
      if FPending[vIdx].Expires > vNow then
      begin
        SetLength(vKeep, Length(vKeep) + 1);
        vKeep[High(vKeep)] := FPending[vIdx];
      end;
    SetLength(vKeep, Length(vKeep) + 1);
    vIdx := High(vKeep);
    vKeep[vIdx].Id := AId;
    vKeep[vIdx].Token := AToken;
    vKeep[vIdx].ChatId := AChatId;
    vKeep[vIdx].Deferred := ADeferred;
    // the token lives 15 minutes; a minute is kept back
    vKeep[vIdx].Expires := vNow + 840;
    FPending := vKeep;
  finally
    FLock.Leave;
  end;
end;

function TRALChatDiscord.AuthHeaders: TStrings;
begin
  Result := TStringList.Create;
  try
    Result.Add('Authorization=Bot ' + string(GetToken));
    Result.Add('User-Agent=DiscordBot (PascalRAL, 2.0)');
  except
    Result.Free;
    raise;
  end;
end;

procedure TRALChatDiscord.Beat;
var
  vText: StringRAL;
begin
  if FSocket = nil then
    Exit;
  FLock.Enter;
  try
    if FSeq > 0 then
      vText := StringRAL('{"op":1,"d":' + IntToStr(FSeq) + '}')
    else
      vText := '{"op":1,"d":null}';
  finally
    FLock.Leave;
  end;
  FSocket.Send(vText);
end;

function TRALChatDiscord.BuildBody(AMessage: TRALChatMessage; AEdit: Boolean): TRALJSONObject;
var
  vEmbeds, vFiles: TRALJSONArray;
  vEmbed, vFile, vRef: TRALJSONObject;
  vComponents: TRALJSONArray;
  vText: StringRAL;
begin
  Result := TRALJSONObject.Create;
  try
    vText := AMessage.Text;
    if AMessage.HasMedia and (AMessage.Media.Data = nil) and (AMessage.Media.Url <> '') then
    begin
      if vText <> '' then
        vText := vText + #10;
      vText := vText + AMessage.Media.Url;
    end
    else if AMessage.HasMedia and (AMessage.Media.Data = nil) then
      raise ERALChatDiscordFinal.Create(emChatNoMedia);
    if FUseEmbeds and (vText <> '') then
    begin
      vEmbeds := TRALJSONArray.Create;
      Result.Add('embeds', vEmbeds);
      vEmbed := TRALJSONObject.Create;
      vEmbeds.Add(vEmbed);
      vEmbed.Add('description', vText);
      vEmbed.Add('color', FEmbedColor);
      if AEdit then
        Result.Add('content', '');
    end
    else if (vText <> '') or AEdit then
      Result.Add('content', vText);
    vComponents := ComponentsOf(AMessage);
    if vComponents <> nil then
      Result.Add('components', vComponents)
    else if AEdit then
      Result.Add('components', TRALJSONArray.Create);
    if (not AEdit) and AMessage.HasReplyTo and (AMessage.ReplyTo.Id <> '') and
       (AMessage.ReplyTo.Id <> AMessage.Id) and (AMessage.ReplyTo.Chat.Id = '') then
    begin
      vRef := TRALJSONObject.Create;
      vRef.Add('message_id', AMessage.ReplyTo.Id);
      Result.Add('message_reference', vRef);
    end;
    if (not AEdit) and AMessage.HasMedia and (AMessage.Media.Data <> nil) then
    begin
      if AMessage.Media.Data.Size > Capabilities.MaxMediaBytes then
        raise ERALChatDiscordFinal.CreateFmt(emChatDcTooBig, [Capabilities.MaxMediaBytes]);
      if AMessage.Media.FileName = '' then
        AMessage.Media.FileName := 'file';
      vFiles := TRALJSONArray.Create;
      Result.Add('attachments', vFiles);
      vFile := TRALJSONObject.Create;
      vFiles.Add(vFile);
      vFile.Add('id', 0);
      vFile.Add('filename', AMessage.Media.FileName);
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TRALChatDiscord.Call(AMethod: TRALMethod; const APath: StringRAL; ABody: TRALJSONObject;
  AMedia: TRALChatMedia; AAuth: Boolean; out AReply: TRALChatDiscordReply);
var
  vHeaders: TStrings;
  vHttp: TRALChatHttpReply;
  vFields: TRALJSONObject;
  vRoot: TRALJSONObject;
  vWait: Double;
  vCode: Integer;
begin
  AReply.Ok := False;
  AReply.Failed := True;
  AReply.Status := 0;
  AReply.Code := 0;
  AReply.Description := '';
  AReply.RetryAfterMs := 0;
  AReply.Global := False;
  AReply.Root := nil;
  vHeaders := nil;
  vFields := nil;
  try
    try
      if AAuth then
        vHeaders := AuthHeaders
      else
      begin
        vHeaders := TStringList.Create;
        vHeaders.Add('User-Agent=DiscordBot (PascalRAL, 2.0)');
      end;
      if AMedia <> nil then
      begin
        vFields := TRALJSONObject.Create;
        vFields.Add('payload_json', ABody.ToJSON);
        FHttp.Call(AMethod, FApiBase + APath, vHeaders, '', '', vFields, 'files[0]', AMedia, vHttp);
      end
      else if ABody <> nil then
        FHttp.Call(AMethod, FApiBase + APath, vHeaders, ABody.ToJSON, 'application/json', nil, '',
          nil, vHttp)
      else
        FHttp.Call(AMethod, FApiBase + APath, vHeaders, '', '', nil, '', nil, vHttp);
    except
      on E: Exception do
      begin
        AReply.Description := Hide(StringRAL(E.ClassName + ': ' + E.Message));
        Exit;
      end;
    end;
  finally
    vFields.Free;
    vHeaders.Free;
  end;
  AReply.Failed := vHttp.Failed;
  AReply.Status := vHttp.Status;
  if vHttp.Failed then
  begin
    AReply.Description := Hide(vHttp.Failure);
    Exit;
  end;
  AReply.Ok := (vHttp.Status >= 200) and (vHttp.Status < 300);
  AReply.RetryAfterMs := vHttp.RetryAfterMs;
  vRoot := RALChatParseObject(vHttp.Text);
  AReply.Root := vRoot;
  if not AReply.Ok then
  begin
    AReply.Description := RALChatJStr(vRoot, 'message');
    AReply.Code := Integer(RALChatJInt(vRoot, 'code', 0));
    AReply.Global := RALChatJStr(vRoot, 'global') = 'true';
    if vRoot <> nil then
    begin
      Val(string(RALChatJStr(vRoot, 'retry_after')), vWait, vCode);
      if (vCode = 0) and (vWait > 0) then
        AReply.RetryAfterMs := Round(vWait * 1000);
    end;
    if AReply.Description = '' then
      AReply.Description := StringRAL('HTTP ' + IntToStr(vHttp.Status));
  end;
end;

function TRALChatDiscord.ComponentsOf(AMessage: TRALChatMessage): TRALJSONArray;
var
  vRow, vSelect, vOptions: TRALJSONArray;
  vRowObj, vObj, vOption: TRALJSONObject;
  vIdx, vPerRow: Integer;
  vButton: TRALChatButton;
begin
  Result := nil;
  if Length(AMessage.Buttons) > 0 then
  begin
    vPerRow := FButtonsPerRow;
    if vPerRow < 1 then
      vPerRow := 1;
    if vPerRow > 5 then
      vPerRow := 5;
    Result := TRALJSONArray.Create;
    try
      vRow := nil;
      for vIdx := 0 to High(AMessage.Buttons) do
      begin
        vButton := AMessage.Buttons[vIdx];
        if (vButton.Url = '') and ((vButton.Id = '') or (Length(vButton.Id) > 100)) then
          raise ERALChatDiscordFinal.Create(emChatDcButtonId);
        if (vRow = nil) or (vRow.Count >= vPerRow) then
        begin
          vRowObj := TRALJSONObject.Create;
          Result.Add(vRowObj);
          vRowObj.Add('type', 1);
          vRow := TRALJSONArray.Create;
          vRowObj.Add('components', vRow);
        end;
        vObj := TRALJSONObject.Create;
        vRow.Add(vObj);
        vObj.Add('type', 2);
        vObj.Add('label', StringRAL(Copy(string(vButton.Caption), 1, 80)));
        if vButton.Url <> '' then
        begin
          vObj.Add('style', 5);
          vObj.Add('url', vButton.Url);
        end
        else
        begin
          vObj.Add('style', 1);
          vObj.Add('custom_id', vButton.Id);
        end;
      end;
    except
      Result.Free;
      raise;
    end;
    Exit;
  end;
  if (Length(AMessage.List.Items) > 0) and (AMessage.Kind <> ckPoll) then
  begin
    Result := TRALJSONArray.Create;
    try
      vRowObj := TRALJSONObject.Create;
      Result.Add(vRowObj);
      vRowObj.Add('type', 1);
      vRow := TRALJSONArray.Create;
      vRowObj.Add('components', vRow);
      vSelect := vRow;
      vObj := TRALJSONObject.Create;
      vSelect.Add(vObj);
      vObj.Add('type', 3);
      vObj.Add('custom_id', cRALChatDiscordList);
      if AMessage.List.Title <> '' then
        vObj.Add('placeholder', StringRAL(Copy(string(AMessage.List.Title), 1, 150)));
      vOptions := TRALJSONArray.Create;
      vObj.Add('options', vOptions);
      for vIdx := 0 to High(AMessage.List.Items) do
      begin
        if (AMessage.List.Items[vIdx].Id = '') or (Length(AMessage.List.Items[vIdx].Id) > 100) then
          raise ERALChatDiscordFinal.Create(emChatDcButtonId);
        vOption := TRALJSONObject.Create;
        vOptions.Add(vOption);
        vOption.Add('label', StringRAL(Copy(string(AMessage.List.Items[vIdx].Title), 1, 100)));
        vOption.Add('value', AMessage.List.Items[vIdx].Id);
        if AMessage.List.Items[vIdx].Description <> '' then
          vOption.Add('description', StringRAL(Copy(string(AMessage.List.Items[vIdx].Description), 1, 100)));
      end;
    except
      Result.Free;
      raise;
    end;
  end;
end;

procedure TRALChatDiscord.Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId: StringRAL; out AResult: TRALChatSendResult);
var
  vBody: TRALJSONObject;
  vReply: TRALChatDiscordReply;
  vChannel, vPath, vWhat: StringRAL;
  vPending: TRALChatDiscordPending;
  vFollow, vUsed: Boolean;
  vMedia: TRALChatMedia;
  vMethod: TRALMethod;
begin
  AResult.Ok := False;
  AResult.MessageId := '';
  AResult.Transient := False;
  AResult.RetryAfterMs := 0;
  AResult.RetryScope := rsChat;
  AResult.ErrorText := '';
  AResult.Info := '';
  try
    if not EnsureChannel(AMessage.Chat.Id, vChannel) then
    begin
      AResult.ErrorText := Hide(StringRAL(Format(emChatDcApi, ['channel', 'no DM channel'])));
      AResult.Transient := True;
      Exit;
    end;
    case AKind of
      okTyping:
      begin
        vWhat := 'typing';
        Call(amPOST, '/channels/' + vChannel + '/typing', nil, nil, True, vReply);
      end;
      okDelete:
      begin
        vWhat := 'delete';
        Call(amDELETE, '/channels/' + vChannel + '/messages/' + ATargetId, nil, nil, True, vReply);
      end;
      okEdit:
      begin
        vWhat := 'edit';
        vBody := BuildBody(AMessage, True);
        try
          Call(amPATCH, '/channels/' + vChannel + '/messages/' + ATargetId, vBody, nil, True, vReply);
        finally
          vBody.Free;
        end;
      end;
    else
      vWhat := 'message';
      vBody := BuildBody(AMessage, False);
      try
        vMedia := nil;
        if AMessage.HasMedia and (AMessage.Media.Data <> nil) then
          vMedia := AMessage.Media;
        vUsed := TakePending(AMessage, vPending, vFollow);
        if vUsed then
        begin
          if FApplicationId = '' then
            raise ERALChatDiscordFinal.CreateFmt(emChatDcNoApp, [string(ChannelName)]);
          vPath := '/webhooks/' + FApplicationId + '/' + vPending.Token;
          if vFollow then
          begin
            vMethod := amPOST;
            vPath := vPath + '?wait=true';
          end
          else
          begin
            vMethod := amPATCH;
            vPath := vPath + '/messages/@original';
          end;
          Call(vMethod, vPath, vBody, vMedia, False, vReply);
        end
        else
          Call(amPOST, '/channels/' + vChannel + '/messages', vBody, vMedia, True, vReply);
      finally
        vBody.Free;
      end;
    end;
    try
      FillResult(vWhat, vReply, AResult);
    finally
      vReply.Root.Free;
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

function TRALChatDiscord.Dialect: TRALChatDialect;
begin
  Result := cdDiscord;
end;

function TRALChatDiscord.EnsureChannel(const AChatId: StringRAL; out AChannelId: StringRAL): Boolean;
var
  vBody: TRALJSONObject;
  vReply: TRALChatDiscordReply;
  vIdx: Integer;
begin
  AChannelId := AChatId;
  Result := True;
  // "u:<user id>" is a direct message channel to a user, opened on first use
  if Copy(string(AChatId), 1, 2) <> 'u:' then
    Exit;
  FLock.Enter;
  try
    vIdx := FDmCache.IndexOfName(string(AChatId));
    if vIdx >= 0 then
    begin
      AChannelId := StringRAL(FDmCache.ValueFromIndex[vIdx]);
      Exit;
    end;
  finally
    FLock.Leave;
  end;
  vBody := TRALJSONObject.Create;
  try
    vBody.Add('recipient_id', StringRAL(Copy(string(AChatId), 3, MaxInt)));
    Call(amPOST, '/users/@me/channels', vBody, nil, True, vReply);
  finally
    vBody.Free;
  end;
  try
    AChannelId := RALChatJStr(vReply.Root, 'id');
    Result := vReply.Ok and (AChannelId <> '');
  finally
    vReply.Root.Free;
  end;
  if Result then
  begin
    FLock.Enter;
    try
      FDmCache.Add(string(AChatId) + '=' + string(AChannelId));
    finally
      FLock.Leave;
    end;
  end;
end;

procedure TRALChatDiscord.FillResult(const AWhat: StringRAL; const AReply: TRALChatDiscordReply;
  var AResult: TRALChatSendResult);
begin
  AResult.Ok := AReply.Ok;
  if AReply.Ok then
  begin
    AResult.MessageId := RALChatJStr(AReply.Root, 'id');
    Exit;
  end;
  AResult.ErrorText := Hide(StringRAL(Format(emChatDcApi, [string(AWhat), string(AReply.Description)])));
  if AReply.Failed or (AReply.Status >= 500) then
    AResult.Transient := True
  else if AReply.Status = 429 then
  begin
    AResult.Transient := True;
    AResult.RetryAfterMs := AReply.RetryAfterMs;
    if AReply.Global then
      AResult.RetryScope := rsChannel
    else
      AResult.RetryScope := rsChat;
  end;
end;

function TRALChatDiscord.GetClient: TRALClient;
begin
  Result := FHttp.Client;
end;

function TRALChatDiscord.GetEngineType: string;
begin
  Result := FHttp.EngineType;
end;

procedure TRALChatDiscord.GatewayClose(ASender: TRALWebSocketClient; ACode: Word;
  const AReason: StringRAL);
begin
  // an automatic reconnection follows; the session is resumed when Discord allows
  if ACode = 4004 then
    ReportError(string(StringRAL(Format(emChatDcGateway, ['authentication failed']))));
end;

procedure TRALChatDiscord.GatewayError(ASender: TRALWebSocketClient; AError: Exception);
begin
  ReportError(string(Hide(StringRAL(Format(emChatDcGateway, [AError.Message])))));
end;

procedure TRALChatDiscord.GatewayMessage(ASender: TRALWebSocketClient; AMessage: TRALWebSocketMessage);
begin
  HandleGatewayPayload(AMessage.Text);
end;

procedure TRALChatDiscord.HandleGatewayPayload(const AText: StringRAL);
var
  vRoot, vData: TRALJSONObject;
  vOp: Int64;
  vType: StringRAL;
  vMessages: TRALChatMessageList;
  vIdx: Integer;
  vBody: TRALJSONObject;
  vReply: TRALChatDiscordReply;
  vInter: TRALJSONObject;
  vKind: Int64;
begin
  vRoot := RALChatParseObject(AText);
  if vRoot = nil then
    Exit;
  try
    vOp := RALChatJInt(vRoot, 'op', -1);
    if RALChatJStr(vRoot, 's') <> '' then
    begin
      FLock.Enter;
      try
        FSeq := RALChatJInt(vRoot, 's', FSeq);
      finally
        FLock.Leave;
      end;
    end;
    vData := RALChatJObj(vRoot, 'd');
    case vOp of
      10:
      begin
        FreeAndNil(FBeat);
        FBeat := TRALChatDiscordBeat.Create(Self, Integer(RALChatJInt(vData, 'heartbeat_interval', 41250)));
        SendIdentify;
      end;
      1:
        Beat;
      7:
        TRALChatDiscordReconnector.Create(Self);
      9:
      begin
        if RALChatJStr(vRoot, 'd') <> 'true' then
        begin
          FSessionId := '';
          FSeq := 0;
        end;
        SendIdentify;
      end;
      0:
      begin
        vType := RALChatJStr(vRoot, 't');
        if vType = 'READY' then
        begin
          FSessionId := RALChatJStr(vData, 'session_id');
          FBotUserId := RALChatJStr(RALChatJObj(vData, 'user'), 'id');
          if FApplicationId = '' then
            FApplicationId := RALChatJStr(RALChatJObj(vData, 'application'), 'id');
        end
        else if (vType = 'MESSAGE_CREATE') or (vType = 'INTERACTION_CREATE') then
        begin
          vMessages := TRALChatMessageList.Create;
          try
            try
              if vType = 'MESSAGE_CREATE' then
                ParseGatewayMessage(vData, vMessages)
              else
              begin
                vInter := vData;
                ParseInteraction(vInter, vMessages);
                // no HTTP answer carries the acknowledgement here: it goes by REST
                vKind := RALChatJInt(vInter, 'type', 0);
                vBody := TRALJSONObject.Create;
                try
                  if vKind = 2 then
                    vBody.Add('type', 5)
                  else
                    vBody.Add('type', 6);
                  Call(amPOST, '/interactions/' + RALChatJStr(vInter, 'id') + '/' +
                    RALChatJStr(vInter, 'token') + '/callback', vBody, nil, False, vReply);
                  vReply.Root.Free;
                finally
                  vBody.Free;
                end;
              end;
            except
              on E: Exception do
                ReportError(string(Hide(StringRAL(E.Message))));
            end;
            for vIdx := 0 to vMessages.Count - 1 do
              if FBot <> nil then
                FBot.Receive(Self, vMessages[vIdx]);
          finally
            vMessages.Free;
          end;
        end;
      end;
    end;
  finally
    vRoot.Free;
  end;
end;

function TRALChatDiscord.Hide(const AText: StringRAL): StringRAL;
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

procedure TRALChatDiscord.Loaded;
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

function TRALChatDiscord.LoadMedia(AMedia: TRALChatMedia): TStream;
begin
  if AMedia.Url = '' then
    raise ERALChatError.Create(emChatNoMedia);
  if (AMedia.Size > 0) and (AMedia.Size > FMaxDownloadSize) then
    raise ERALChatError.CreateFmt(emChatDcTooBig, [FMaxDownloadSize]);
  if not MediaHostOk(AMedia.Url) then
    raise ERALChatError.Create(emChatDcBadHost);
  Result := FHttp.Download(AMedia.Url, nil, FMaxDownloadSize);
end;

function TRALChatDiscord.MediaHostOk(const AUrl: StringRAL): Boolean;
var
  vText, vHost, vApi: string;
  vPos: Integer;

  function HostOf(const AValue: string): string;
  var
    vP: Integer;
  begin
    Result := AValue;
    vP := Pos('://', Result);
    if vP > 0 then
      Delete(Result, 1, vP + 2);
    vP := Pos('/', Result);
    if vP > 0 then
      Result := Copy(Result, 1, vP - 1);
    vP := Pos(':', Result);
    if vP > 0 then
      Result := Copy(Result, 1, vP - 1);
    Result := LowerCase(Result);
  end;

begin
  vText := string(AUrl);
  vHost := HostOf(vText);
  vApi := HostOf(string(FApiBase));
  vPos := Pos('@', vHost);
  Result := (vPos = 0) and (vHost <> '') and
    ((vHost = vApi) or (vHost = 'cdn.discordapp.com') or (vHost = 'media.discordapp.net') or
     (vHost = 'discord.com'));
  if Result and (Pos('https://', LowerCase(vText)) <> 1) and (vHost <> vApi) then
    Result := False;
end;

procedure TRALChatDiscord.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then
    Exit;
  if AComponent = FHttp.Client then
    FHttp.Client := nil;
  if AComponent = FBot then
  begin
    Stop;
    FBot := nil;
  end;
end;

procedure TRALChatDiscord.ParseGatewayMessage(AObj: TRALJSONObject; AMessages: TRALChatMessageList);
var
  vAuthor: TRALJSONObject;
  vMessage: TRALChatMessage;
  vInfo: TRALChatInfo;
  vAttach: TRALJSONArray;
  vFirst: TRALJSONObject;
  vMime: StringRAL;
  vLoader: TRALChatMediaLoader;
begin
  vAuthor := RALChatJObj(AObj, 'author');
  // never answer a bot (this one included): two bots would talk forever
  if (RALChatJStr(vAuthor, 'bot') = 'true') or (RALChatJStr(vAuthor, 'id') = FBotUserId) then
    Exit;
  vMessage := TRALChatMessage.Create;
  try
    vMessage.Id := RALChatJStr(AObj, 'id');
    vMessage.EventId := 'dcm' + vMessage.Id;
    vMessage.Text := RALChatJStr(AObj, 'content');
    vMessage.From := ReadUser(vAuthor);
    vInfo.Id := RALChatJStr(AObj, 'channel_id');
    vInfo.Title := '';
    if RALChatJStr(AObj, 'guild_id') <> '' then
      vInfo.ChatType := cctGroup
    else
      vInfo.ChatType := cctPrivate;
    vMessage.Chat := vInfo;
    vAttach := RALChatJArr(AObj, 'attachments');
    if (vAttach <> nil) and (vAttach.Count > 0) and (vAttach[0].Kind = rjkObject) then
    begin
      vFirst := TRALJSONObject(vAttach[0]);
      vMime := RALChatJStr(vFirst, 'content_type');
      if Copy(string(vMime), 1, 6) = 'image/' then
        vMessage.Kind := ckImage
      else if Copy(string(vMime), 1, 6) = 'audio/' then
        vMessage.Kind := ckAudio
      else if Copy(string(vMime), 1, 6) = 'video/' then
        vMessage.Kind := ckVideo
      else
        vMessage.Kind := ckDocument;
      vMessage.Media.Id := RALChatJStr(vFirst, 'id');
      vMessage.Media.Url := RALChatJStr(vFirst, 'url');
      vMessage.Media.MimeType := vMime;
      vMessage.Media.FileName := RALChatJStr(vFirst, 'filename');
      vMessage.Media.Size := RALChatJInt(vFirst, 'size', 0);
      vLoader := LoadMedia;
      vMessage.Media.Loader := vLoader;
    end;
    if KeepRaw then
      vMessage.Raw := AObj.ToJSON;
    AMessages.AddMessage(vMessage);
    vMessage := nil;
  finally
    vMessage.Free;
  end;
end;

procedure TRALChatDiscord.ParseInteraction(AObj: TRALJSONObject; AMessages: TRALChatMessageList);
var
  vType: Int64;
  vMessage: TRALChatMessage;
  vData, vSource: TRALJSONObject;
  vInfo: TRALChatInfo;
  vText, vId, vToken: StringRAL;
  vValues: TRALJSONArray;

  procedure AddOptions(AOptions: TRALJSONArray);
  var
    vIdx: Integer;
    vOpt: TRALJSONObject;
  begin
    if AOptions = nil then
      Exit;
    for vIdx := 0 to AOptions.Count - 1 do
      if AOptions[vIdx].Kind = rjkObject then
      begin
        vOpt := TRALJSONObject(AOptions[vIdx]);
        if RALChatJArr(vOpt, 'options') <> nil then
        begin
          // a subcommand or a group: its name is part of the command line
          vText := vText + ' ' + RALChatJStr(vOpt, 'name');
          AddOptions(RALChatJArr(vOpt, 'options'));
        end
        else
          vText := vText + ' ' + RALChatJStr(vOpt, 'value');
      end;
  end;

begin
  vType := RALChatJInt(AObj, 'type', 0);
  if (vType <> 2) and (vType <> 3) then
    Exit;
  vId := RALChatJStr(AObj, 'id');
  vToken := RALChatJStr(AObj, 'token');
  if (FApplicationId = '') and (RALChatJStr(AObj, 'application_id') <> '') then
    FApplicationId := RALChatJStr(AObj, 'application_id');
  vMessage := TRALChatMessage.Create;
  try
    vMessage.Id := vId;
    vMessage.EventId := 'dci' + vId;
    vInfo.Id := RALChatJStr(AObj, 'channel_id');
    vInfo.Title := '';
    if RALChatJStr(AObj, 'guild_id') <> '' then
      vInfo.ChatType := cctGroup
    else
      vInfo.ChatType := cctPrivate;
    vMessage.Chat := vInfo;
    vSource := RALChatJObj(RALChatJObj(AObj, 'member'), 'user');
    if vSource = nil then
      vSource := RALChatJObj(AObj, 'user');
    vMessage.From := ReadUser(vSource);
    vData := RALChatJObj(AObj, 'data');
    if vType = 2 then
    begin
      vText := '/' + RALChatJStr(vData, 'name');
      AddOptions(RALChatJArr(vData, 'options'));
      vMessage.Text := vText;
      AddPending(vId, vToken, vInfo.Id, True);
    end
    else
    begin
      vMessage.Kind := ckCallback;
      vValues := RALChatJArr(vData, 'values');
      if (vValues <> nil) and (vValues.Count > 0) then
        vMessage.CallbackData := RALChatJText(vValues[0])
      else
        vMessage.CallbackData := RALChatJStr(vData, 'custom_id');
      vSource := RALChatJObj(AObj, 'message');
      if vSource <> nil then
      begin
        vMessage.ReplyTo.Id := RALChatJStr(vSource, 'id');
        vMessage.ReplyTo.Text := RALChatJStr(vSource, 'content');
        vMessage.ReplyTo.Chat := vInfo;
      end;
      AddPending(vId, vToken, vInfo.Id, False);
    end;
    if KeepRaw then
      vMessage.Raw := AObj.ToJSON;
    AMessages.AddMessage(vMessage);
    vMessage := nil;
  finally
    vMessage.Free;
  end;
end;

procedure TRALChatDiscord.ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList);
var
  vRoot: TRALJSONObject;
begin
  vRoot := RALChatParseObject(ABody);
  if vRoot = nil then
    raise ERALChatError.CreateFmt(emChatBadWebhook, [string(ChannelName)]);
  try
    ParseInteraction(vRoot, AMessages);
  finally
    vRoot.Free;
  end;
end;

function TRALChatDiscord.ReadUser(AUser: TRALJSONObject): TRALChatUser;
begin
  Result.Id := RALChatJStr(AUser, 'id');
  Result.Name := RALChatJStr(AUser, 'global_name');
  if Result.Name = '' then
    Result.Name := RALChatJStr(AUser, 'username');
  Result.Phone := '';
end;

function TRALChatDiscord.RegisterCommand(const AName, ADescription: StringRAL;
  out AReport: StringRAL): Boolean;
var
  vBody: TRALJSONObject;
  vReply: TRALChatDiscordReply;
begin
  if FApplicationId = '' then
  begin
    AReport := StringRAL(Format(emChatDcNoApp, [string(ChannelName)]));
    Result := False;
    Exit;
  end;
  vBody := TRALJSONObject.Create;
  try
    vBody.Add('name', AName);
    vBody.Add('description', ADescription);
    vBody.Add('type', 1);
    Call(amPOST, '/applications/' + FApplicationId + '/commands', vBody, nil, True, vReply);
  finally
    vBody.Free;
  end;
  try
    Result := vReply.Ok;
    if Result then
      AReport := AName
    else
      AReport := Hide(StringRAL(Format(emChatDcApi, ['commands', string(vReply.Description)])));
  finally
    vReply.Root.Free;
  end;
end;

procedure TRALChatDiscord.ReportError(const AText: string);
begin
  if FBot <> nil then
    FBot.ReportError(Self, AText);
end;

procedure TRALChatDiscord.SendIdentify;
var
  vBody, vData, vProps: TRALJSONObject;
begin
  if FSocket = nil then
    Exit;
  vBody := TRALJSONObject.Create;
  try
    vData := TRALJSONObject.Create;
    if FSessionId <> '' then
    begin
      vBody.Add('op', 6);
      vData.Add('token', GetToken);
      vData.Add('session_id', FSessionId);
      vData.Add('seq', FSeq);
    end
    else
    begin
      vBody.Add('op', 2);
      vData.Add('token', GetToken);
      vData.Add('intents', FIntents);
      vProps := TRALJSONObject.Create;
      vProps.Add('os', 'pascal');
      vProps.Add('browser', 'PascalRAL');
      vProps.Add('device', 'PascalRAL');
      vData.Add('properties', vProps);
    end;
    vBody.Add('d', vData);
    FSocket.Send(vBody.ToJSON);
  finally
    vBody.Free;
  end;
end;

procedure TRALChatDiscord.SetActive(AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  if [csDesigning, csLoading] * ComponentState <> [] then
    Exit;
  if AValue then
    Start
  else
    Stop;
end;

procedure TRALChatDiscord.SetBot(AValue: TRALChatBot);
begin
  if FBot = AValue then
    Exit;
  Stop;
  if FBot <> nil then
    FBot.RemoveFreeNotification(Self);
  FBot := AValue;
  if FBot <> nil then
    FBot.FreeNotification(Self);
end;

procedure TRALChatDiscord.SetClient(AValue: TRALClient);
begin
  if FHttp.Client = AValue then
    Exit;
  if FHttp.Client <> nil then
    FHttp.Client.RemoveFreeNotification(Self);
  FHttp.Client := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

procedure TRALChatDiscord.SetEngineType(const AValue: string);
begin
  FHttp.EngineType := AValue;
end;

procedure TRALChatDiscord.SetPublicKey(const AValue: StringRAL);
begin
  FPublicKey := AValue;
end;

procedure TRALChatDiscord.SetUseEmbeds(AValue: Boolean);
begin
  FUseEmbeds := AValue;
  if AValue then
    Capabilities.MaxText := cMaxEmbed
  else
    Capabilities.MaxText := cMaxContent;
end;

procedure TRALChatDiscord.Start;
var
  vReply: TRALChatDiscordReply;
  vUrl: StringRAL;
begin
  if FReceiveMode = crmWebhook then
    Exit;
  if FBot = nil then
    raise ERALChatError.Create(emChatNoBot);
  if FSocket <> nil then
    Exit;
  vUrl := FGatewayUrl;
  if vUrl = '' then
  begin
    Call(amGET, '/gateway/bot', nil, nil, True, vReply);
    try
      if not vReply.Ok then
        raise ERALChatError.CreateFmt(emChatDcApi, ['gateway', string(vReply.Description)]);
      vUrl := RALChatJStr(vReply.Root, 'url') + '/?v=10&encoding=json';
    finally
      vReply.Root.Free;
    end;
  end;
  FStopping := False;
  FSocket := TRALWebSocketClient.Create(nil);
  try
    FSocket.URL := vUrl;
    FSocket.AutoReconnect := True;
    FSocket.OnMessage := GatewayMessage;
    FSocket.OnClose := GatewayClose;
    FSocket.OnError := GatewayError;
    FSocket.Connect;
  except
    FreeAndNil(FSocket);
    raise;
  end;
end;

procedure TRALChatDiscord.Stop;
var
  vSocket: TRALWebSocketClient;
begin
  FStopping := True;
  vSocket := FSocket;
  FSocket := nil;
  if FBeat <> nil then
  begin
    FBeat.Quit;
    FreeAndNil(FBeat);
  end;
  if vSocket <> nil then
  begin
    try
      vSocket.Disconnect(1000, '', 2000);
    except
      // a dropped connection has nothing to close
    end;
    vSocket.Free;
  end;
end;

function TRALChatDiscord.TakePending(AMessage: TRALChatMessage; out APending: TRALChatDiscordPending;
  out AFollowUp: Boolean): Boolean;
var
  vIdx, vNow: Integer;
begin
  Result := False;
  AFollowUp := False;
  FLock.Enter;
  try
    vNow := Integer(RALTickMicro div 1000000);
    for vIdx := 0 to High(FPending) do
      if FPending[vIdx].Expires > vNow then
      begin
        if AMessage.HasReplyTo and (AMessage.ReplyTo.Id = FPending[vIdx].Id) then
        begin
          // an answer to a named interaction: the first one fills the "thinking" message
          APending := FPending[vIdx];
          AFollowUp := not FPending[vIdx].Deferred;
          FPending[vIdx].Deferred := False;
          Result := True;
          Exit;
        end;
      end;
    for vIdx := 0 to High(FPending) do
      if (FPending[vIdx].Expires > vNow) and FPending[vIdx].Deferred and
         (FPending[vIdx].ChatId = AMessage.Chat.Id) then
      begin
        APending := FPending[vIdx];
        FPending[vIdx].Deferred := False;
        Result := True;
        Exit;
      end;
  finally
    FLock.Leave;
  end;
end;

function TRALChatDiscord.TestToken(out AReport: StringRAL): Boolean;
var
  vReply: TRALChatDiscordReply;
begin
  Call(amGET, '/users/@me', nil, nil, True, vReply);
  try
    Result := vReply.Ok;
    if Result then
    begin
      FBotUserId := RALChatJStr(vReply.Root, 'id');
      if FApplicationId = '' then
        FApplicationId := FBotUserId;
      AReport := StringRAL(Format(cmChatDcTokenOk, [string(RALChatJStr(vReply.Root, 'username'))]));
    end
    else
      AReport := Hide(StringRAL(Format(emChatDcApi, ['users/@me', string(vReply.Description)])));
  finally
    vReply.Root.Free;
  end;
end;

procedure TRALChatDiscord.VerifyRequest(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
  var ATimestamp: Int64RAL; var AValid: Boolean);
var
  vSig, vKey: TBytes;
  vTs: StringRAL;
begin
  AValid := False;
  vKey := RALChatHexToBytes(FPublicKey);
  if (Length(vKey) <> 32) or not Assigned(AHeaders) then
    Exit;
  vSig := RALChatHexToBytes(AHeaders('X-Signature-Ed25519'));
  vTs := AHeaders('X-Signature-Timestamp');
  if (Length(vSig) <> 64) or (vTs = '') then
    Exit;
  ATimestamp := StrToInt64Def(string(vTs), 0);
  AValid := RALEd25519Verify(ToBytes(vTs + ABody), vSig, vKey);
end;

end.
