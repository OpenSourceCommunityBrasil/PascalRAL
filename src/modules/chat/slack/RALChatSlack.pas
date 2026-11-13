/// The Slack channel of the chat module: Events API, Web API and Socket Mode.
unit RALChatSlack;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALJson, RALClient, RALWebSocket, RALWebSocketClient,
  RALChatMessage, RALChatVerify, RALChatChannel, RALChatBot, RALChatHttp;

const
  /// Default address of the Web API.
  cRALChatSlackApi = 'https://slack.com/api';
  /// Prefix of the action id of a link button (Slack reports its click too).
  cRALChatSlackLink = 'ral_link_';

type
  TRALChatSlack = class;

  /// What one call to the Web API came to.
  TRALChatSlackReply = record
    /// Whether Slack accepted the call (ok true).
    Ok: Boolean;
    /// True when no answer came.
    Failed: Boolean;
    /// The HTTP status; 0 when Failed.
    Status: Integer;
    /// The error code of Slack, or the transport failure.
    Description: StringRAL;
    /// Milliseconds Slack asks to wait (Retry-After); 0 = none.
    RetryAfterMs: Int64RAL;
    /// The answer parsed, or nil; the caller frees it.
    Root: TRALJSONObject;
  end;

  /// The thread that opens the Socket Mode connection again.
  TRALChatSlackReconnector = class(TThread)
  private
    /// The object the thread works for.
    FOwner: TRALChatSlack;
  protected
    procedure Execute; override;
  public
    /// Starts the thread for AOwner; it frees itself.
    constructor Create(AOwner: TRALChatSlack);
  end;

  { The Slack channel: Events API and interactivity with the v0 signature (5 minute
    window), url_verification, slash commands, chat.postMessage/update/delete, Block Kit
    buttons and menus, mrkdwn, files by the upload URL flow and Socket Mode as the polling
    mode. Tokens and the signing secret never reach a log or an error text. }
  TRALChatSlack = class(TRALChatChannel)
  private
    FActive: Boolean;
    FApiBase: StringRAL;
    FAppToken: StringRAL;
    FAppTokenName: StringRAL;
    FBot: TRALChatBot;
    /// The id of the bot itself, whose messages are not delivered.
    FBotUserId: StringRAL;
    FButtonsPerRow: Integer;
    /// The HTTP client of the channel.
    FHttp: TRALChatHttp;
    FMaxDownloadSize: Int64RAL;
    FReceiveMode: TRALChatReceiveMode;
    FSigningSecret: StringRAL;
    /// The WebSocket connection of the receiving mode, or nil.
    FSocket: TRALWebSocketClient;
    FSocketUrl: StringRAL;
    /// Whether the connection is being closed.
    FStopping: Boolean;
    /// The verifier of the webhook (owned by the base).
    FVerifier: TRALChatHMACVerifier;

    /// The Block Kit blocks of AMessage, or nil when it needs none.
    function BlocksOf(AMessage: TRALChatMessage; const AText: StringRAL): TRALJSONArray;
    /// Makes one call to the API and says how it came out; never raises.
    procedure Call(const AMethod: StringRAL; ABody: TRALJSONObject; const AQuery: StringRAL;
      AAppToken: Boolean; out AReply: TRALChatSlackReply);
    /// Turns a reply into a delivery result.
    procedure FillResult(const AMethod: StringRAL; const AReply: TRALChatSlackReply;
      var AResult: TRALChatSendResult);
    /// The app-level token, from where TokenSource says.
    function GetAppToken: StringRAL;
    function GetClient: TRALClient;
    function GetEngineType: string;
    /// Hides the secrets in AText.
    function Hide(const AText: StringRAL): StringRAL;
    /// Whether AUrl is a Slack host (or the API host).
    function HostOk(const AUrl: StringRAL): Boolean;
    /// Downloads a received attachment, from the platform's hosts only.
    function LoadMedia(AMedia: TRALChatMedia): TStream;
    /// Reads a block_actions payload into AMessages.
    procedure ParseAction(APayload: TRALJSONObject; AMessages: TRALChatMessageList);
    /// Reads a slash command into AMessages.
    procedure ParseCommand(const ACommand, AText, AChannel, AUser, AName, ATrigger: StringRAL;
      AMessages: TRALChatMessageList);
    /// Reads an event_callback envelope into AMessages.
    procedure ParseEvent(AEnvelope: TRALJSONObject; AMessages: TRALChatMessageList);
    /// Reads a payload object, whatever its type.
    procedure ParseObject(ARoot: TRALJSONObject; AMessages: TRALChatMessageList);
    /// Reports an error to the bot, when there is one.
    procedure ReportError(const AText: string);
    procedure SetActive(AValue: Boolean);
    procedure SetBot(AValue: TRALChatBot);
    procedure SetClient(AValue: TRALClient);
    procedure SetEngineType(const AValue: string);
    procedure SetSigningSecret(const AValue: StringRAL);
    /// The Socket Mode connection closed.
    procedure SocketClose(ASender: TRALWebSocketClient; ACode: Word; const AReason: StringRAL);
    /// The Socket Mode connection failed.
    procedure SocketError(ASender: TRALWebSocketClient; AError: Exception);
    /// A Socket Mode envelope arrived.
    procedure SocketMessage(ASender: TRALWebSocketClient; AMessage: TRALWebSocketMessage);
    /// Sends a file by the upload URL flow.
    procedure Upload(AMessage: TRALChatMessage; var AResult: TRALChatSendResult);
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
    { Handles one message of the Socket Mode connection (what the socket delivers): the
      envelope is acknowledged and its payload handled like a webhook; public so that
      a program can feed it, and the tests do. }
    procedure HandleSocketPayload(const AText: StringRAL);
    procedure ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList); override;
    { Opens the Socket Mode connection (ReceiveMode crmPolling or crmAuto); needs the
      AppToken (xapp-...). }
    procedure Start;
    /// Closes the Socket Mode connection.
    procedure Stop;
    { Asks Slack who the token belongs to (auth.test): True with a text naming it, or
      False with the reason in AReport. }
    function TestToken(out AReport: StringRAL): Boolean;
  published
    /// Whether the channel opens Socket Mode (never at design time).
    property Active: Boolean read FActive write SetActive default False;
    { The address of the Web API, without the final slash; another points the channel at
      a test server. }
    property ApiBase: StringRAL read FApiBase write FApiBase;
    { The app-level token (xapp-...) of Socket Mode when TokenSource is tsProperty. Set it
      from the environment in the program rather than in the form. }
    property AppToken: StringRAL read FAppToken write FAppToken;
    /// The variable (tsEnvironment) or file (tsFile) that holds the app-level token.
    property AppTokenName: StringRAL read FAppTokenName write FAppTokenName;
    /// The bot that receives what Socket Mode brings.
    property Bot: TRALChatBot read FBot write SetBot;
    /// How many buttons go in a row.
    property ButtonsPerRow: Integer read FButtonsPerRow write FButtonsPerRow default 5;
    { The client that calls the API; leave its BaseURL empty (the channel sends whole
      URLs). Without one the channel makes its own, of EngineType. }
    property Client: TRALClient read GetClient write SetClient;
    /// The engine of the channel's own client; empty = the first registered.
    property EngineType: string read GetEngineType write SetEngineType;
    /// The largest file taken from Slack, in bytes.
    property MaxDownloadSize: Int64RAL read FMaxDownloadSize write FMaxDownloadSize
      default 104857600;
    /// How messages arrive: the Events API webhook or Socket Mode.
    property ReceiveMode: TRALChatReceiveMode read FReceiveMode write FReceiveMode
      default crmWebhook;
    { The signing secret of the app: the v0 HMAC key of each request; empty refuses every
      webhook. Set it from the environment in the program rather than in the form. }
    property SigningSecret: StringRAL read FSigningSecret write SetSigningSecret;
    { The Socket Mode address; empty = asked of apps.connections.open. Another points the
      mode at a test server. }
    property SocketUrl: StringRAL read FSocketUrl write FSocketUrl;
  end;

implementation

type
  /// An exception of the channel that is not worth another attempt.
  ERALChatSlackFinal = class(ERALChatError);

const
  /// The longest text of a section block.
  cSectionMax = 3000;

{ TRALChatSlackReconnector }

constructor TRALChatSlackReconnector.Create(AOwner: TRALChatSlack);
begin
  FOwner := AOwner;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TRALChatSlackReconnector.Execute;
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

{ TRALChatSlack }

constructor TRALChatSlack.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttp := TRALChatHttp.Create;
  FHttp.ChannelName := 'slack';
  FApiBase := cRALChatSlackApi;
  FButtonsPerRow := 5;
  FMaxDownloadSize := 104857600;
  ChannelName := 'slack';
  ReplayWindow := 300;
  Capabilities.Supported := [ccButtons, ccLists, ccEdit, ccDelete, ccMedia];
  Capabilities.MaxButtons := 25;
  // Slack cuts a text at 40000 characters and advises 4000
  Capabilities.MaxText := 4000;
  Capabilities.MaxMediaBytes := 1073741824;
  // chat.postMessage: about 1 message a second to a channel; a workspace tolerates bursts
  Rates.PerChat := 1;
  Rates.PerChatBurst := 1;
  Rates.Total := 20;
  Rates.TotalBurst := 20;
  FVerifier := TRALChatHMACVerifier.Create;
  FVerifier.HeaderName := 'X-Slack-Signature';
  FVerifier.Prefix := 'v0=';
  FVerifier.Template := 'v0:{ts}:{body}';
  FVerifier.TimestampHeader := 'X-Slack-Request-Timestamp';
  FVerifier.Encoding := seHex;
  Verifier := FVerifier;
end;

destructor TRALChatSlack.Destroy;
begin
  Stop;
  FHttp.Free;
  inherited Destroy;
end;

function TRALChatSlack.Acknowledge(const ABody: StringRAL; AMessages: TRALChatMessageList;
  out AStatus: Integer; out AContentType, AText: StringRAL): Boolean;
var
  vRoot: TRALJSONObject;
begin
  AStatus := 200;
  AContentType := 'text/plain';
  AText := '';
  Result := False;
  // interactivity and slash commands are forms: an empty 200 acknowledges them (a body
  // would be posted as a message by a slash command)
  if (ABody <> '') and (ABody[1] <> '{') then
  begin
    Result := True;
    Exit;
  end;
  vRoot := RALChatParseObject(ABody);
  try
    if (vRoot <> nil) and (RALChatJStr(vRoot, 'type') = 'url_verification') then
    begin
      AText := RALChatJStr(vRoot, 'challenge');
      Result := True;
    end;
  finally
    vRoot.Free;
  end;
end;

function TRALChatSlack.BlocksOf(AMessage: TRALChatMessage; const AText: StringRAL): TRALJSONArray;
var
  vPieces: TStringList;
  vIdx, vPerRow: Integer;
  vBlock, vText, vElement, vOption, vPlain: TRALJSONObject;
  vElements, vOptions: TRALJSONArray;
  vButton: TRALChatButton;
  vLink: Integer;

  procedure NewActions;
  begin
    vBlock := TRALJSONObject.Create;
    Result.Add(vBlock);
    vBlock.Add('type', 'actions');
    vElements := TRALJSONArray.Create;
    vBlock.Add('elements', vElements);
  end;

begin
  Result := nil;
  if (Length(AMessage.Buttons) = 0) and ((Length(AMessage.List.Items) = 0) or (AMessage.Kind = ckPoll)) then
    Exit;
  Result := TRALJSONArray.Create;
  try
    vPieces := TStringList.Create;
    try
      if AText <> '' then
        RALChatSplitText(AText, cSectionMax, vPieces);
      for vIdx := 0 to vPieces.Count - 1 do
      begin
        vBlock := TRALJSONObject.Create;
        Result.Add(vBlock);
        vBlock.Add('type', 'section');
        vText := TRALJSONObject.Create;
        vBlock.Add('text', vText);
        vText.Add('type', 'mrkdwn');
        vText.Add('text', StringRAL(vPieces[vIdx]));
      end;
    finally
      vPieces.Free;
    end;
    if Length(AMessage.Buttons) > 0 then
    begin
      vPerRow := FButtonsPerRow;
      if vPerRow < 1 then
        vPerRow := 1;
      if vPerRow > 5 then
        vPerRow := 5;
      vElements := nil;
      vLink := 0;
      for vIdx := 0 to High(AMessage.Buttons) do
      begin
        vButton := AMessage.Buttons[vIdx];
        if (vButton.Url = '') and ((vButton.Id = '') or (Length(vButton.Id) > 255)) then
          raise ERALChatSlackFinal.Create(emChatSlButtonId);
        if (vElements = nil) or (vElements.Count >= vPerRow) then
          NewActions;
        vElement := TRALJSONObject.Create;
        vElements.Add(vElement);
        vElement.Add('type', 'button');
        vPlain := TRALJSONObject.Create;
        vElement.Add('text', vPlain);
        vPlain.Add('type', 'plain_text');
        vPlain.Add('text', StringRAL(Copy(string(vButton.Caption), 1, 75)));
        if vButton.Url <> '' then
        begin
          Inc(vLink);
          vElement.Add('action_id', cRALChatSlackLink + StringRAL(IntToStr(vLink)));
          vElement.Add('url', vButton.Url);
        end
        else
        begin
          vElement.Add('action_id', vButton.Id);
          vElement.Add('value', vButton.Id);
        end;
      end;
    end
    else
    begin
      NewActions;
      vElement := TRALJSONObject.Create;
      vElements.Add(vElement);
      vElement.Add('type', 'static_select');
      vElement.Add('action_id', 'ral_list');
      vPlain := TRALJSONObject.Create;
      vElement.Add('placeholder', vPlain);
      vPlain.Add('type', 'plain_text');
      if AMessage.List.Title <> '' then
        vPlain.Add('text', StringRAL(Copy(string(AMessage.List.Title), 1, 150)))
      else
        vPlain.Add('text', '...');
      vOptions := TRALJSONArray.Create;
      vElement.Add('options', vOptions);
      for vIdx := 0 to High(AMessage.List.Items) do
      begin
        if (AMessage.List.Items[vIdx].Id = '') or (Length(AMessage.List.Items[vIdx].Id) > 150) then
          raise ERALChatSlackFinal.Create(emChatSlButtonId);
        vOption := TRALJSONObject.Create;
        vOptions.Add(vOption);
        vPlain := TRALJSONObject.Create;
        vOption.Add('text', vPlain);
        vPlain.Add('type', 'plain_text');
        vPlain.Add('text', StringRAL(Copy(string(AMessage.List.Items[vIdx].Title), 1, 75)));
        vOption.Add('value', AMessage.List.Items[vIdx].Id);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TRALChatSlack.Call(const AMethod: StringRAL; ABody: TRALJSONObject;
  const AQuery: StringRAL; AAppToken: Boolean; out AReply: TRALChatSlackReply);
var
  vHeaders: TStringList;
  vHttp: TRALChatHttpReply;
  vUrl: StringRAL;
begin
  AReply.Ok := False;
  AReply.Failed := True;
  AReply.Status := 0;
  AReply.Description := '';
  AReply.RetryAfterMs := 0;
  AReply.Root := nil;
  vHeaders := TStringList.Create;
  try
    try
      if AAppToken then
        vHeaders.Add('Authorization=Bearer ' + string(GetAppToken))
      else
        vHeaders.Add('Authorization=Bearer ' + string(GetToken));
      vUrl := FApiBase + '/' + AMethod;
      if AQuery <> '' then
        vUrl := vUrl + '?' + AQuery;
      if ABody <> nil then
        FHttp.Call(amPOST, vUrl, vHeaders, ABody.ToJSON, 'application/json; charset=utf-8', nil, '', nil, vHttp)
      else
        FHttp.Call(amPOST, vUrl, vHeaders, '', '', nil, '', nil, vHttp);
    except
      on E: Exception do
      begin
        AReply.Description := Hide(StringRAL(E.ClassName + ': ' + E.Message));
        Exit;
      end;
    end;
  finally
    vHeaders.Free;
  end;
  AReply.Failed := vHttp.Failed;
  AReply.Status := vHttp.Status;
  AReply.RetryAfterMs := vHttp.RetryAfterMs;
  if vHttp.Failed then
  begin
    AReply.Description := Hide(vHttp.Failure);
    Exit;
  end;
  AReply.Root := RALChatParseObject(vHttp.Text);
  AReply.Ok := (vHttp.Status = 200) and (RALChatJStr(AReply.Root, 'ok') = 'true');
  if not AReply.Ok then
  begin
    AReply.Description := RALChatJStr(AReply.Root, 'error');
    if AReply.Description = '' then
      AReply.Description := StringRAL('HTTP ' + IntToStr(vHttp.Status));
  end;
end;

procedure TRALChatSlack.Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId: StringRAL; out AResult: TRALChatSendResult);
var
  vBody: TRALJSONObject;
  vReply: TRALChatSlackReply;
  vMethod: StringRAL;
  vBlocks: TRALJSONArray;
begin
  AResult.Ok := False;
  AResult.MessageId := '';
  AResult.Transient := False;
  AResult.RetryAfterMs := 0;
  AResult.RetryScope := rsChat;
  AResult.ErrorText := '';
  AResult.Info := '';
  try
    if AKind = okTyping then
    begin
      // the Web API has no typing indicator for a bot
      AResult.Ok := True;
      Exit;
    end;
    if (AKind = okMessage) and AMessage.HasMedia and (AMessage.Media.Data <> nil) then
    begin
      Upload(AMessage, AResult);
      Exit;
    end;
    vBody := TRALJSONObject.Create;
    try
      vBody.Add('channel', AMessage.Chat.Id);
      case AKind of
        okDelete:
        begin
          vMethod := 'chat.delete';
          vBody.Add('ts', ATargetId);
        end;
      else
        if AMessage.HasMedia and (AMessage.Media.Data = nil) then
        begin
          if AMessage.Media.Url = '' then
            raise ERALChatSlackFinal.Create(emChatNoMedia);
          AMessage.Text := AMessage.Text + #10 + AMessage.Media.Url;
        end;
        vBody.Add('text', AMessage.Text);
        vBlocks := BlocksOf(AMessage, AMessage.Text);
        if vBlocks <> nil then
          vBody.Add('blocks', vBlocks)
        else if AKind = okEdit then
          vBody.Add('blocks', TRALJSONArray.Create);
        if AKind = okEdit then
        begin
          vMethod := 'chat.update';
          vBody.Add('ts', ATargetId);
        end
        else
        begin
          vMethod := 'chat.postMessage';
          if AMessage.HasReplyTo and (AMessage.ReplyTo.Id <> '') then
            vBody.Add('thread_ts', AMessage.ReplyTo.Id);
        end;
      end;
      Call(vMethod, vBody, '', False, vReply);
    finally
      vBody.Free;
    end;
    try
      FillResult(vMethod, vReply, AResult);
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

function TRALChatSlack.Dialect: TRALChatDialect;
begin
  Result := cdSlack;
end;

procedure TRALChatSlack.FillResult(const AMethod: StringRAL; const AReply: TRALChatSlackReply;
  var AResult: TRALChatSendResult);
begin
  AResult.Ok := AReply.Ok;
  if AReply.Ok then
  begin
    AResult.MessageId := RALChatJStr(AReply.Root, 'ts');
    Exit;
  end;
  AResult.ErrorText := Hide(StringRAL(Format(emChatSlApi, [string(AMethod), string(AReply.Description)])));
  if AReply.Failed or (AReply.Status >= 500) or (AReply.Status = 429) or
     (AReply.Description = 'ratelimited') or (AReply.Description = 'internal_error') or
     (AReply.Description = 'service_unavailable') or (AReply.Description = 'fatal_error') or
     (AReply.Description = 'request_timeout') then
  begin
    AResult.Transient := True;
    AResult.RetryAfterMs := AReply.RetryAfterMs;
    // Slack limits the method per workspace and channel: the channel waits
    if AReply.RetryAfterMs > 0 then
      AResult.RetryScope := rsChannel;
  end;
end;

function TRALChatSlack.GetAppToken: StringRAL;
var
  vList: TStringList;
begin
  Result := '';
  case TokenSource of
    tsProperty:
      Result := FAppToken;
    tsEnvironment:
      Result := StringRAL(GetEnvironmentVariable(string(FAppTokenName)));
    tsFile:
    begin
      vList := TStringList.Create;
      try
        try
          vList.LoadFromFile(string(FAppTokenName));
        except
          raise ERALChatError.CreateFmt(emChatTokenFile, [string(ChannelName)]);
        end;
        if vList.Count > 0 then
          Result := StringRAL(Trim(vList[0]));
      finally
        vList.Free;
      end;
    end;
  end;
  if Result = '' then
    raise ERALChatError.CreateFmt(emChatSlNoAppToken, [string(ChannelName)]);
end;

function TRALChatSlack.GetClient: TRALClient;
begin
  Result := FHttp.Client;
end;

function TRALChatSlack.GetEngineType: string;
begin
  Result := FHttp.EngineType;
end;

procedure TRALChatSlack.HandleSocketPayload(const AText: StringRAL);
var
  vRoot: TRALJSONObject;
  vType, vEnvelope: StringRAL;
  vMessages: TRALChatMessageList;
  vIdx: Integer;
  vPayload: TRALJSONObject;
begin
  vRoot := RALChatParseObject(AText);
  if vRoot = nil then
    Exit;
  try
    vType := RALChatJStr(vRoot, 'type');
    vEnvelope := RALChatJStr(vRoot, 'envelope_id');
    if vType = 'disconnect' then
    begin
      TRALChatSlackReconnector.Create(Self);
      Exit;
    end;
    if vEnvelope = '' then
      Exit;
    // acknowledged first: Slack waits 3 seconds
    if FSocket <> nil then
      FSocket.Send('{"envelope_id":"' + vEnvelope + '"}');
    vPayload := RALChatJObj(vRoot, 'payload');
    if vPayload = nil then
      Exit;
    vMessages := TRALChatMessageList.Create;
    try
      try
        if vType = 'slash_commands' then
          ParseCommand(RALChatJStr(vPayload, 'command'), RALChatJStr(vPayload, 'text'),
            RALChatJStr(vPayload, 'channel_id'), RALChatJStr(vPayload, 'user_id'),
            RALChatJStr(vPayload, 'user_name'), RALChatJStr(vPayload, 'trigger_id'), vMessages)
        else
          ParseObject(vPayload, vMessages);
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
  finally
    vRoot.Free;
  end;
end;

function TRALChatSlack.Hide(const AText: StringRAL): StringRAL;
var
  vSecret: StringRAL;
  vIdx: Integer;
begin
  Result := AText;
  for vIdx := 0 to 2 do
  begin
    vSecret := '';
    try
      case vIdx of
        0: vSecret := GetToken;
        1: vSecret := GetAppToken;
      else
        vSecret := FSigningSecret;
      end;
    except
      // not set: nothing to hide
    end;
    if Length(vSecret) >= 4 then
      Result := StringRAL(StringReplace(string(Result), string(vSecret), '***', [rfReplaceAll]));
  end;
end;

function TRALChatSlack.HostOk(const AUrl: StringRAL): Boolean;
var
  vHost, vApi: string;

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
  vHost := HostOf(string(AUrl));
  vApi := HostOf(string(FApiBase));
  Result := (Pos('@', vHost) = 0) and (vHost <> '') and
    ((vHost = vApi) or (vHost = 'slack.com') or
     ((Length(vHost) > 10) and (Copy(vHost, Length(vHost) - 9, 10) = '.slack.com')));
end;

procedure TRALChatSlack.Loaded;
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

function TRALChatSlack.LoadMedia(AMedia: TRALChatMedia): TStream;
var
  vHeaders: TStringList;
begin
  if AMedia.Url = '' then
    raise ERALChatError.Create(emChatNoMedia);
  if (AMedia.Size > 0) and (AMedia.Size > FMaxDownloadSize) then
    raise ERALChatError.CreateFmt(emChatSlTooBig, [FMaxDownloadSize]);
  if not HostOk(AMedia.Url) then
    raise ERALChatError.Create(emChatSlBadHost);
  vHeaders := TStringList.Create;
  try
    vHeaders.Add('Authorization=Bearer ' + string(GetToken));
    Result := FHttp.Download(AMedia.Url, vHeaders, FMaxDownloadSize);
  finally
    vHeaders.Free;
  end;
end;

procedure TRALChatSlack.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TRALChatSlack.ParseAction(APayload: TRALJSONObject; AMessages: TRALChatMessageList);
var
  vActions: TRALJSONArray;
  vAction, vMsg, vContainer: TRALJSONObject;
  vMessage: TRALChatMessage;
  vInfo: TRALChatInfo;
  vUser: TRALChatUser;
  vValue, vTrigger: StringRAL;
begin
  vActions := RALChatJArr(APayload, 'actions');
  if (vActions = nil) or (vActions.Count = 0) or (vActions[0].Kind <> rjkObject) then
    Exit;
  vAction := TRALJSONObject(vActions[0]);
  if Copy(string(RALChatJStr(vAction, 'action_id')), 1, Length(cRALChatSlackLink)) = cRALChatSlackLink then
    Exit;
  vValue := RALChatJStr(RALChatJObj(vAction, 'selected_option'), 'value');
  if vValue = '' then
    vValue := RALChatJStr(vAction, 'value');
  if vValue = '' then
    vValue := RALChatJStr(vAction, 'action_id');
  vTrigger := RALChatJStr(APayload, 'trigger_id');
  if vTrigger = '' then
    vTrigger := RALChatJStr(vAction, 'action_ts');
  vContainer := RALChatJObj(APayload, 'container');
  vMsg := RALChatJObj(APayload, 'message');
  vMessage := TRALChatMessage.Create;
  try
    vMessage.Kind := ckCallback;
    vMessage.Id := vTrigger;
    vMessage.EventId := 'sla' + vTrigger;
    vMessage.CallbackData := vValue;
    vInfo.Id := RALChatJStr(RALChatJObj(APayload, 'channel'), 'id');
    if vInfo.Id = '' then
      vInfo.Id := RALChatJStr(vContainer, 'channel_id');
    vInfo.Title := '';
    if (vInfo.Id <> '') and (vInfo.Id[1] = 'D') then
      vInfo.ChatType := cctPrivate
    else
      vInfo.ChatType := cctGroup;
    vMessage.Chat := vInfo;
    vUser.Id := RALChatJStr(RALChatJObj(APayload, 'user'), 'id');
    vUser.Name := RALChatJStr(RALChatJObj(APayload, 'user'), 'name');
    if vUser.Name = '' then
      vUser.Name := RALChatJStr(RALChatJObj(APayload, 'user'), 'username');
    vUser.Phone := '';
    vMessage.From := vUser;
    vMessage.ReplyTo.Id := RALChatJStr(vContainer, 'message_ts');
    if vMessage.ReplyTo.Id = '' then
      vMessage.ReplyTo.Id := RALChatJStr(vMsg, 'ts');
    vMessage.ReplyTo.Text := RALChatJStr(vMsg, 'text');
    vMessage.ReplyTo.Chat := vInfo;
    if KeepRaw then
      vMessage.Raw := APayload.ToJSON;
    AMessages.AddMessage(vMessage);
    vMessage := nil;
  finally
    vMessage.Free;
  end;
end;

procedure TRALChatSlack.ParseCommand(const ACommand, AText, AChannel, AUser, AName, ATrigger: StringRAL;
  AMessages: TRALChatMessageList);
var
  vMessage: TRALChatMessage;
  vInfo: TRALChatInfo;
  vUser: TRALChatUser;
  vLine: StringRAL;
begin
  if ACommand = '' then
    Exit;
  vLine := ACommand;
  if vLine[1] <> '/' then
    vLine := '/' + vLine;
  if AText <> '' then
    vLine := vLine + ' ' + AText;
  vMessage := TRALChatMessage.Create;
  try
    vMessage.Text := vLine;
    vMessage.Id := ATrigger;
    vMessage.EventId := 'slc' + ATrigger;
    vInfo.Id := AChannel;
    vInfo.Title := '';
    if (AChannel <> '') and (AChannel[1] = 'D') then
      vInfo.ChatType := cctPrivate
    else
      vInfo.ChatType := cctGroup;
    vMessage.Chat := vInfo;
    vUser.Id := AUser;
    vUser.Name := AName;
    vUser.Phone := '';
    vMessage.From := vUser;
    AMessages.AddMessage(vMessage);
    vMessage := nil;
  finally
    vMessage.Free;
  end;
end;

procedure TRALChatSlack.ParseEvent(AEnvelope: TRALJSONObject; AMessages: TRALChatMessageList);
var
  vEvent, vFile: TRALJSONObject;
  vType, vSub, vMime: StringRAL;
  vMessage: TRALChatMessage;
  vInfo: TRALChatInfo;
  vUser: TRALChatUser;
  vFiles: TRALJSONArray;
  vLoader: TRALChatMediaLoader;
begin
  vEvent := RALChatJObj(AEnvelope, 'event');
  vType := RALChatJStr(vEvent, 'type');
  if (vType <> 'message') and (vType <> 'app_mention') then
    Exit;
  vSub := RALChatJStr(vEvent, 'subtype');
  if (vSub <> '') and (vSub <> 'file_share') then
    Exit;
  // a bot's message (this one's included) is never delivered: two bots would talk forever
  if (RALChatJStr(vEvent, 'bot_id') <> '') or (RALChatJStr(vEvent, 'user') = FBotUserId) then
    Exit;
  vMessage := TRALChatMessage.Create;
  try
    vMessage.Id := RALChatJStr(vEvent, 'ts');
    vInfo.Id := RALChatJStr(vEvent, 'channel');
    vMessage.EventId := 'sl' + vInfo.Id + ':' + vMessage.Id;
    vMessage.Text := RALChatJStr(vEvent, 'text');
    vInfo.Title := '';
    if RALChatJStr(vEvent, 'channel_type') = 'im' then
      vInfo.ChatType := cctPrivate
    else
      vInfo.ChatType := cctGroup;
    vMessage.Chat := vInfo;
    vUser.Id := RALChatJStr(vEvent, 'user');
    vUser.Name := vUser.Id;
    vUser.Phone := '';
    vMessage.From := vUser;
    if RALChatJStr(vEvent, 'thread_ts') <> '' then
      vMessage.ReplyTo.Id := RALChatJStr(vEvent, 'thread_ts');
    vFiles := RALChatJArr(vEvent, 'files');
    if (vFiles <> nil) and (vFiles.Count > 0) and (vFiles[0].Kind = rjkObject) then
    begin
      vFile := TRALJSONObject(vFiles[0]);
      vMime := RALChatJStr(vFile, 'mimetype');
      if Copy(string(vMime), 1, 6) = 'image/' then
        vMessage.Kind := ckImage
      else if Copy(string(vMime), 1, 6) = 'audio/' then
        vMessage.Kind := ckAudio
      else if Copy(string(vMime), 1, 6) = 'video/' then
        vMessage.Kind := ckVideo
      else
        vMessage.Kind := ckDocument;
      vMessage.Media.Id := RALChatJStr(vFile, 'id');
      vMessage.Media.Url := RALChatJStr(vFile, 'url_private_download');
      vMessage.Media.MimeType := vMime;
      vMessage.Media.FileName := RALChatJStr(vFile, 'name');
      vMessage.Media.Size := RALChatJInt(vFile, 'size', 0);
      vLoader := LoadMedia;
      vMessage.Media.Loader := vLoader;
    end;
    if KeepRaw then
      vMessage.Raw := AEnvelope.ToJSON;
    AMessages.AddMessage(vMessage);
    vMessage := nil;
  finally
    vMessage.Free;
  end;
end;

procedure TRALChatSlack.ParseObject(ARoot: TRALJSONObject; AMessages: TRALChatMessageList);
var
  vType: StringRAL;
begin
  vType := RALChatJStr(ARoot, 'type');
  if vType = 'event_callback' then
    ParseEvent(ARoot, AMessages)
  else if vType = 'block_actions' then
    ParseAction(ARoot, AMessages)
  else if RALChatJStr(ARoot, 'command') <> '' then
    ParseCommand(RALChatJStr(ARoot, 'command'), RALChatJStr(ARoot, 'text'),
      RALChatJStr(ARoot, 'channel_id'), RALChatJStr(ARoot, 'user_id'),
      RALChatJStr(ARoot, 'user_name'), RALChatJStr(ARoot, 'trigger_id'), AMessages);
end;

procedure TRALChatSlack.ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList);
var
  vRoot: TRALJSONObject;
  vForm: TStringList;
  vIdx: Integer;
  vText: string;
begin
  // interactivity (payload=JSON) and slash commands come as forms, events as JSON
  if (ABody <> '') and (ABody[1] <> '{') then
  begin
    vForm := TStringList.Create;
    try
      vText := string(ABody);
      while vText <> '' do
      begin
        vIdx := Pos('&', vText);
        if vIdx = 0 then
          vIdx := Length(vText) + 1;
        vForm.Add(string(RALChatUrlDecode(StringRAL(Copy(vText, 1, vIdx - 1)))));
        Delete(vText, 1, vIdx);
      end;
      if vForm.Values['payload'] <> '' then
      begin
        vRoot := RALChatParseObject(StringRAL(vForm.Values['payload']));
        if vRoot = nil then
          raise ERALChatError.CreateFmt(emChatBadWebhook, [string(ChannelName)]);
        try
          ParseObject(vRoot, AMessages);
        finally
          vRoot.Free;
        end;
      end
      else
        ParseCommand(StringRAL(vForm.Values['command']), StringRAL(vForm.Values['text']),
          StringRAL(vForm.Values['channel_id']), StringRAL(vForm.Values['user_id']),
          StringRAL(vForm.Values['user_name']), StringRAL(vForm.Values['trigger_id']), AMessages);
    finally
      vForm.Free;
    end;
    Exit;
  end;
  vRoot := RALChatParseObject(ABody);
  if vRoot = nil then
    raise ERALChatError.CreateFmt(emChatBadWebhook, [string(ChannelName)]);
  try
    ParseObject(vRoot, AMessages);
  finally
    vRoot.Free;
  end;
end;

procedure TRALChatSlack.ReportError(const AText: string);
begin
  if FBot <> nil then
    FBot.ReportError(Self, AText);
end;

procedure TRALChatSlack.SetActive(AValue: Boolean);
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

procedure TRALChatSlack.SetBot(AValue: TRALChatBot);
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

procedure TRALChatSlack.SetClient(AValue: TRALClient);
begin
  if FHttp.Client = AValue then
    Exit;
  if FHttp.Client <> nil then
    FHttp.Client.RemoveFreeNotification(Self);
  FHttp.Client := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

procedure TRALChatSlack.SetEngineType(const AValue: string);
begin
  FHttp.EngineType := AValue;
end;

procedure TRALChatSlack.SetSigningSecret(const AValue: StringRAL);
begin
  FSigningSecret := AValue;
  FVerifier.Secret := AValue;
end;

procedure TRALChatSlack.SocketClose(ASender: TRALWebSocketClient; ACode: Word; const AReason: StringRAL);
begin
  // a Socket Mode address is good for one connection: a drop asks for a new one
  if (not FStopping) and FActive and (ASender = FSocket) then
    TRALChatSlackReconnector.Create(Self);
end;

procedure TRALChatSlack.SocketError(ASender: TRALWebSocketClient; AError: Exception);
begin
  ReportError(string(Hide(StringRAL(Format(emChatSlSocket, [AError.Message])))));
end;

procedure TRALChatSlack.SocketMessage(ASender: TRALWebSocketClient; AMessage: TRALWebSocketMessage);
begin
  HandleSocketPayload(AMessage.Text);
end;

procedure TRALChatSlack.Start;
var
  vReply: TRALChatSlackReply;
  vUrl: StringRAL;
begin
  if FReceiveMode = crmWebhook then
    Exit;
  if FBot = nil then
    raise ERALChatError.Create(emChatNoBot);
  if FSocket <> nil then
    Exit;
  vUrl := FSocketUrl;
  if vUrl = '' then
  begin
    Call('apps.connections.open', nil, '', True, vReply);
    try
      if not vReply.Ok then
        raise ERALChatError.CreateFmt(emChatSlApi, ['apps.connections.open', string(vReply.Description)]);
      vUrl := RALChatJStr(vReply.Root, 'url');
    finally
      vReply.Root.Free;
    end;
  end;
  FStopping := False;
  FSocket := TRALWebSocketClient.Create(nil);
  try
    FSocket.URL := vUrl;
    FSocket.OnMessage := SocketMessage;
    FSocket.OnClose := SocketClose;
    FSocket.OnError := SocketError;
    FSocket.Connect;
  except
    FreeAndNil(FSocket);
    raise;
  end;
end;

procedure TRALChatSlack.Stop;
var
  vSocket: TRALWebSocketClient;
begin
  FStopping := True;
  vSocket := FSocket;
  FSocket := nil;
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

function TRALChatSlack.TestToken(out AReport: StringRAL): Boolean;
var
  vReply: TRALChatSlackReply;
begin
  Call('auth.test', nil, '', False, vReply);
  try
    Result := vReply.Ok;
    if Result then
    begin
      FBotUserId := RALChatJStr(vReply.Root, 'user_id');
      AReport := StringRAL(Format(cmChatSlTokenOk, [string(RALChatJStr(vReply.Root, 'user')),
        string(RALChatJStr(vReply.Root, 'team'))]));
    end
    else
      AReport := Hide(StringRAL(Format(emChatSlApi, ['auth.test', string(vReply.Description)])));
  finally
    vReply.Root.Free;
  end;
end;

procedure TRALChatSlack.Upload(AMessage: TRALChatMessage; var AResult: TRALChatSendResult);
var
  vReply: TRALChatSlackReply;
  vUrl, vFileId, vName: StringRAL;
  vHttp: TRALChatHttpReply;
  vBody, vFile: TRALJSONObject;
  vFiles: TRALJSONArray;
begin
  if AMessage.Media.Data.Size > Capabilities.MaxMediaBytes then
    raise ERALChatSlackFinal.CreateFmt(emChatSlTooBig, [Capabilities.MaxMediaBytes]);
  vName := AMessage.Media.FileName;
  if vName = '' then
    vName := 'file';
  // 1. the address to send the bytes to
  Call('files.getUploadURLExternal', nil, 'filename=' + RALChatUrlEncode(vName) + '&length=' +
    StringRAL(IntToStr(AMessage.Media.Data.Size)), False, vReply);
  try
    FillResult('files.getUploadURLExternal', vReply, AResult);
    if not vReply.Ok then
      Exit;
    vUrl := RALChatJStr(vReply.Root, 'upload_url');
    vFileId := RALChatJStr(vReply.Root, 'file_id');
  finally
    vReply.Root.Free;
  end;
  // the address comes in an answer: it is followed only when it is a Slack host
  if not HostOk(vUrl) then
  begin
    AResult.Ok := False;
    AResult.ErrorText := StringRAL(emChatSlBadHost);
    Exit;
  end;
  // 2. the bytes
  FHttp.Call(amPOST, vUrl, nil, '', '', nil, 'file', AMessage.Media, vHttp);
  if vHttp.Failed or (vHttp.Status <> 200) then
  begin
    AResult.Ok := False;
    AResult.Transient := vHttp.Failed or (vHttp.Status >= 500);
    AResult.ErrorText := Hide(StringRAL(Format(emChatSlApi, ['upload', 'HTTP ' + IntToStr(vHttp.Status) +
      ' ' + string(vHttp.Failure)])));
    Exit;
  end;
  // 3. share it in the conversation
  vBody := TRALJSONObject.Create;
  try
    vFiles := TRALJSONArray.Create;
    vBody.Add('files', vFiles);
    vFile := TRALJSONObject.Create;
    vFiles.Add(vFile);
    vFile.Add('id', vFileId);
    vFile.Add('title', vName);
    vBody.Add('channel_id', AMessage.Chat.Id);
    if AMessage.Text <> '' then
      vBody.Add('initial_comment', AMessage.Text);
    if AMessage.HasReplyTo and (AMessage.ReplyTo.Id <> '') then
      vBody.Add('thread_ts', AMessage.ReplyTo.Id);
    Call('files.completeUploadExternal', vBody, '', False, vReply);
  finally
    vBody.Free;
  end;
  try
    FillResult('files.completeUploadExternal', vReply, AResult);
    if AResult.Ok then
      AResult.MessageId := vFileId;
  finally
    vReply.Root.Free;
  end;
end;

end.
