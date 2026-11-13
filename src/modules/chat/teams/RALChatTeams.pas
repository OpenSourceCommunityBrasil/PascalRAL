/// The Teams channel of the chat module: the Bot Framework protocol, no SDK.
unit RALChatTeams;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, Contnrs, DateUtils,
  RALTypes, RALConsts, RALTools, RALJson, RALClient, RALASN1, RALRSA, RALJWS,
  RALChatMessage, RALChatVerify, RALChatChannel, RALChatBot, RALChatHttp;

const
  /// Where the Bot Framework publishes the metadata of its signing keys.
  cRALChatTeamsOpenId = 'https://login.botframework.com/v1/.well-known/openidconfiguration';
  /// The only issuer of the tokens the Bot Framework sends, by default.
  cRALChatTeamsIssuer = 'https://api.botframework.com';
  /// The scope of the token the bot sends with.
  cRALChatTeamsScope = 'https://api.botframework.com/.default';
  /// Where the token of a tenant is asked; the tenant goes in place of the braces.
  cRALChatTeamsTokenUrl = 'https://login.microsoftonline.com/{tenant}/oauth2/v2.0/token';

type
  /// A signing key of the Bot Framework: the RSA key and the channels it vouches for.
  TRALChatTeamsKey = class
  public
    /// The channels the key is endorsed for (empty = the metadata gave none).
    Endorsements: TStringList;
    /// The public key.
    Key: TRALRSAKey;
    /// The id of the key.
    Kid: StringRAL;

    /// Creates the TRALChatTeamsKey.
    constructor Create;
    destructor Destroy; override;
  end;

  /// What one call to the connector came to.
  TRALChatTeamsReply = record
    /// Whether the connector accepted the call (a 2xx).
    Ok: Boolean;
    /// True when no answer came.
    Failed: Boolean;
    /// The HTTP status; 0 when Failed.
    Status: Integer;
    /// The error text, or the transport failure.
    Description: StringRAL;
    /// Milliseconds the connector asks to wait (Retry-After); 0 = none.
    RetryAfterMs: Int64RAL;
    /// The answer parsed, or nil; the caller frees it.
    Root: TRALJSONObject;
  end;

  { The Teams channel: activities posted by the Bot Framework, authenticated by their JWT
    (RS256 by the published keys, with issuer, audience, serviceUrl and endorsements
    checked), replies to the serviceUrl of the activity with a client-credentials token,
    Adaptive Card buttons, edit, delete and typing. The secret and the tokens never reach
    a log or an error text. The Token property holds the client secret of the app. }
  TRALChatTeams = class(TRALChatChannel)
  private
    /// When the cached access token ends, in Unix seconds.
    FAccessExpires: Int64RAL;
    /// The access token of the connector, or empty.
    FAccessToken: StringRAL;
    FAppId: StringRAL;
    FBot: TRALChatBot;
    FDefaultServiceUrl: StringRAL;
    /// The HTTP client of the channel.
    FHttp: TRALChatHttp;
    /// The signing keys of the Bot Framework.
    FKeys: TObjectList;
    /// When the keys were loaded, in Unix seconds.
    FKeysFetched: Int64RAL;
    FKeysLife: Integer;
    /// When an unknown key id last asked for the keys, in Unix seconds.
    FKeysRetry: Int64RAL;
    /// Guards the lazy and shared state.
    FLock: TCriticalSection;
    FMaxDownloadSize: Int64RAL;
    FMediaHosts: StringRAL;
    FOpenIdUrl: StringRAL;
    FRequireHttps: Boolean;
    /// Key lists replaced by a refresh, kept until the channel is freed.
    FRetired: TObjectList;
    /// The serviceUrl of each conversation, as id=url lines.
    FServiceUrls: TStringList;
    FTenantId: StringRAL;
    FTokenScope: StringRAL;
    FTokenUrl: StringRAL;
    FValidIssuers: StringRAL;
    /// The verifier of the webhook (owned by the base).
    FVerifier: TRALChatEventVerifier;

    /// The access token of the connector, asked of the tenant when none is cached.
    function AccessToken: StringRAL;
    /// The Adaptive Card of AMessage with its buttons.
    function AdaptiveCard(AMessage: TRALChatMessage): TRALJSONObject;
    /// The activity of a message, an edit or a typing.
    function BuildActivity(AMessage: TRALChatMessage; AKind: TRALChatOutKind;
      const ATargetId: StringRAL): TRALJSONObject;
    /// Makes one call to the API and says how it came out; never raises.
    procedure Call(AMethod: TRALMethod; const AUrl: StringRAL; ABody: TRALJSONObject;
      out AReply: TRALChatTeamsReply);
    /// Loads the signing keys named by the OpenID metadata; AReport says why not.
    procedure FetchKeys(out AReport: StringRAL);
    /// Turns a reply into a delivery result.
    procedure FillResult(const AWhat: StringRAL; const AReply: TRALChatTeamsReply;
      var AResult: TRALChatSendResult);
    /// The signing key of AKid, asking for the keys again when it is unknown.
    function FindKey(const AKid: StringRAL): TRALChatTeamsKey;
    function GetClient: TRALClient;
    function GetEngineType: string;
    /// Hides the secrets in AText.
    function Hide(const AText: StringRAL): StringRAL;
    /// Downloads a received attachment, from the platform's hosts only.
    function LoadMedia(AMedia: TRALChatMedia): TStream;
    /// Whether AUrl is a host an attachment may come from.
    function MediaHostOk(const AUrl: StringRAL): Boolean;
    /// The serviceUrl of a conversation.
    function ServiceUrlOf(const AConversation: StringRAL): StringRAL;
    procedure SetBot(AValue: TRALChatBot);
    procedure SetClient(AValue: TRALClient);
    procedure SetEngineType(const AValue: string);
    /// Removes the <at> mentions from the text.
    function StripMentions(const AText: StringRAL): StringRAL;
    /// The current time in Unix seconds.
    function UnixNow: Int64RAL;
    /// Checks the Ed25519 signature of an interaction.
    procedure VerifyRequest(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
      var ATimestamp: Int64RAL; var AValid: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId: StringRAL; out AResult: TRALChatSendResult); override;
    function Dialect: TRALChatDialect; override;
    procedure ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList); override;
    { Remembers the serviceUrl of a conversation, for a message to it that no activity
      of this run announced (a proactive message after a restart). }
    procedure RememberConversation(const AConversationId, AServiceUrl: StringRAL);
    { Asks for a token with the credentials and loads the signing keys: True with a text
      saying so, or False with the reason in AReport. }
    function TestCredentials(out AReport: StringRAL): Boolean;
  published
    { The Microsoft app (client) id: the audience of the incoming tokens and the client of
      the outgoing one; empty refuses every webhook. }
    property AppId: StringRAL read FAppId write FAppId;
    /// The bot, which receives the messages.
    property Bot: TRALChatBot read FBot write SetBot;
    { The client that calls the connector and the identity platform; leave its BaseURL
      empty. Without one the channel makes its own, of EngineType. }
    property Client: TRALClient read GetClient write SetClient;
    { The serviceUrl used for a conversation no activity announced; empty = none (the
      message fails). }
    property DefaultServiceUrl: StringRAL read FDefaultServiceUrl write FDefaultServiceUrl;
    /// The engine of the channel's own client; empty = the first registered.
    property EngineType: string read GetEngineType write SetEngineType;
    /// How long the signing keys are kept before being asked again, in minutes.
    property KeysLife: Integer read FKeysLife write FKeysLife default 1440;
    /// The largest attachment taken from Teams, in bytes.
    property MaxDownloadSize: Int64RAL read FMaxDownloadSize write FMaxDownloadSize
      default 26214400;
    { The host suffixes an attachment may be downloaded from, separated by commas; the
      host of the serviceUrl of the activity is always allowed. }
    property MediaHosts: StringRAL read FMediaHosts write FMediaHosts;
    { The address of the OpenID metadata of the Bot Framework (it names the key set);
      another points the channel at a test server. }
    property OpenIdUrl: StringRAL read FOpenIdUrl write FOpenIdUrl;
    { Refuses a serviceUrl, a key address or a download that is not https; turn it off
      only against a test server. }
    property RequireHttps: Boolean read FRequireHttps write FRequireHttps default True;
    { The directory (tenant) of a single-tenant app, which the token endpoint needs;
      "botframework.com" for a multi-tenant app. }
    property TenantId: StringRAL read FTenantId write FTenantId;
    /// The scope of the outgoing token.
    property TokenScope: StringRAL read FTokenScope write FTokenScope;
    /// The token endpoint; empty = the Microsoft one for TenantId.
    property TokenUrl: StringRAL read FTokenUrl write FTokenUrl;
    { The issuers accepted in a token, separated by commas (the Bot Framework's by
      default; the Emulator's and a tenant's are other ones). }
    property ValidIssuers: StringRAL read FValidIssuers write FValidIssuers;
  end;

implementation

type
  /// An exception of the channel that is not worth another attempt.
  ERALChatTeamsFinal = class(ERALChatError);

const
  /// The clock difference accepted in a token, in seconds.
  cSkew = 300;
  /// An unknown key id asks for the keys again at most this often, in seconds.
  cKeysRetry = 300;
  /// The largest inline image, in bytes.
  cInlineMax = 262144;

/// The bytes of a text.
function BytesOf(const AText: StringRAL): TBytes;
begin
  SetLength(Result, Length(AText));
  if Length(AText) > 0 then
    Move(AText[1], Result[0], Length(AText));
end;

/// A text of bytes.
function TextOf(const ABytes: TBytes): StringRAL;
begin
  SetLength(Result, Length(ABytes));
  if Length(ABytes) > 0 then
    Move(ABytes[0], Result[1], Length(ABytes));
end;

/// AText without the final slashes.
function TrimSlash(const AText: StringRAL): StringRAL;
begin
  Result := AText;
  while (Result <> '') and (Result[Length(Result)] = '/') do
    SetLength(Result, Length(Result) - 1);
end;

/// The base64 text of the whole stream.
function Base64Of(const AStream: TStream): StringRAL;
const
  cChars: array[0..63] of AnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  vData: TBytes;
  vIdx, vOut, vBlock: Integer;
begin
  SetLength(vData, AStream.Size);
  AStream.Position := 0;
  if Length(vData) > 0 then
    AStream.ReadBuffer(vData[0], Length(vData));
  SetLength(Result, ((Length(vData) + 2) div 3) * 4);
  vOut := 1;
  vIdx := 0;
  while vIdx < Length(vData) do
  begin
    vBlock := vData[vIdx] shl 16;
    if vIdx + 1 < Length(vData) then
      vBlock := vBlock or (vData[vIdx + 1] shl 8);
    if vIdx + 2 < Length(vData) then
      vBlock := vBlock or vData[vIdx + 2];
    Result[vOut] := cChars[(vBlock shr 18) and 63];
    Result[vOut + 1] := cChars[(vBlock shr 12) and 63];
    if vIdx + 1 < Length(vData) then
      Result[vOut + 2] := cChars[(vBlock shr 6) and 63]
    else
      Result[vOut + 2] := '=';
    if vIdx + 2 < Length(vData) then
      Result[vOut + 3] := cChars[vBlock and 63]
    else
      Result[vOut + 3] := '=';
    Inc(vOut, 4);
    Inc(vIdx, 3);
  end;
  AStream.Position := 0;
end;

{ TRALChatTeamsKey }

constructor TRALChatTeamsKey.Create;
begin
  inherited Create;
  Endorsements := TStringList.Create;
end;

destructor TRALChatTeamsKey.Destroy;
begin
  Key.Free;
  Endorsements.Free;
  inherited Destroy;
end;

{ TRALChatTeams }

constructor TRALChatTeams.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FKeys := TObjectList.Create(True);
  FRetired := TObjectList.Create(True);
  FServiceUrls := TStringList.Create;
  FHttp := TRALChatHttp.Create;
  FHttp.ChannelName := 'teams';
  FKeysLife := 1440;
  FMaxDownloadSize := 26214400;
  FOpenIdUrl := cRALChatTeamsOpenId;
  FRequireHttps := True;
  FTokenScope := cRALChatTeamsScope;
  FValidIssuers := cRALChatTeamsIssuer;
  FMediaHosts := '.trafficmanager.net,.microsoft.com,.sharepoint.com,.skype.com,.office.com';
  ChannelName := 'teams';
  Capabilities.Supported := [ccButtons, ccEdit, ccDelete, ccTyping, ccMedia];
  // a card holds about six actions
  Capabilities.MaxButtons := 6;
  Capabilities.MaxText := 20000;
  // an inline image travels inside the activity
  Capabilities.MaxMediaBytes := cInlineMax;
  // 1 message a second to a conversation, 50 a second for the app
  Rates.PerChat := 1;
  Rates.PerChatBurst := 1;
  Rates.Total := 50;
  Rates.TotalBurst := 50;
  FVerifier := TRALChatEventVerifier.Create;
  FVerifier.OnVerify := VerifyRequest;
  Verifier := FVerifier;
end;

destructor TRALChatTeams.Destroy;
begin
  FHttp.Free;
  FServiceUrls.Free;
  FKeys.Free;
  FRetired.Free;
  FLock.Free;
  inherited Destroy;
end;

function TRALChatTeams.AccessToken: StringRAL;
var
  vHeaders: TStringList;
  vReply: TRALChatHttpReply;
  vBody, vUrl: StringRAL;
  vRoot: TRALJSONObject;
begin
  FLock.Enter;
  try
    if (FAccessToken <> '') and (FAccessExpires > UnixNow) then
    begin
      Result := FAccessToken;
      Exit;
    end;
  finally
    FLock.Leave;
  end;
  vUrl := FTokenUrl;
  if vUrl = '' then
  begin
    if FTenantId = '' then
      raise ERALChatTeamsFinal.CreateFmt(emChatTmToken, ['TenantId']);
    vUrl := StringRAL(StringReplace(cRALChatTeamsTokenUrl, '{tenant}', string(FTenantId), []));
  end;
  vBody := 'grant_type=client_credentials&client_id=' + RALChatUrlEncode(FAppId) +
    '&client_secret=' + RALChatUrlEncode(GetToken) + '&scope=' + RALChatUrlEncode(FTokenScope);
  vHeaders := TStringList.Create;
  try
    FHttp.Call(amPOST, vUrl, vHeaders, vBody, 'application/x-www-form-urlencoded', nil, '', nil, vReply);
  finally
    vHeaders.Free;
  end;
  if vReply.Failed then
    raise ERALChatError.CreateFmt(emChatTmToken, [string(Hide(vReply.Failure))]);
  vRoot := RALChatParseObject(vReply.Text);
  try
    Result := RALChatJStr(vRoot, 'access_token');
    if (vReply.Status <> 200) or (Result = '') then
      raise ERALChatTeamsFinal.CreateFmt(emChatTmToken, [string(Hide(RALChatJStr(vRoot, 'error') + ' ' +
        RALChatJStr(vRoot, 'error_description')))]);
    FLock.Enter;
    try
      FAccessToken := Result;
      // renewed two minutes before it ends
      FAccessExpires := UnixNow + RALChatJInt(vRoot, 'expires_in', 3600) - 120;
    finally
      FLock.Leave;
    end;
  finally
    vRoot.Free;
  end;
end;

function TRALChatTeams.AdaptiveCard(AMessage: TRALChatMessage): TRALJSONObject;
var
  vBody, vActions: TRALJSONArray;
  vBlock, vAction, vData: TRALJSONObject;
  vIdx: Integer;
  vButton: TRALChatButton;
begin
  Result := TRALJSONObject.Create;
  try
    Result.Add('type', 'AdaptiveCard');
    Result.Add('$schema', 'http://adaptivecards.io/schemas/adaptive-card.json');
    Result.Add('version', '1.4');
    vBody := TRALJSONArray.Create;
    Result.Add('body', vBody);
    if AMessage.Text <> '' then
    begin
      vBlock := TRALJSONObject.Create;
      vBody.Add(vBlock);
      vBlock.Add('type', 'TextBlock');
      vBlock.Add('text', AMessage.Text);
      vBlock.Add('wrap', True);
    end;
    vActions := TRALJSONArray.Create;
    Result.Add('actions', vActions);
    for vIdx := 0 to High(AMessage.Buttons) do
    begin
      vButton := AMessage.Buttons[vIdx];
      if (vButton.Url = '') and ((vButton.Id = '') or (Length(vButton.Id) > 100)) then
        raise ERALChatTeamsFinal.Create(emChatTmButtonId);
      vAction := TRALJSONObject.Create;
      vActions.Add(vAction);
      vAction.Add('title', vButton.Caption);
      if vButton.Url <> '' then
      begin
        vAction.Add('type', 'Action.OpenUrl');
        vAction.Add('url', vButton.Url);
      end
      else
      begin
        vAction.Add('type', 'Action.Submit');
        vData := TRALJSONObject.Create;
        vAction.Add('data', vData);
        vData.Add('ralId', vButton.Id);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TRALChatTeams.BuildActivity(AMessage: TRALChatMessage; AKind: TRALChatOutKind;
  const ATargetId: StringRAL): TRALJSONObject;
var
  vAttach: TRALJSONArray;
  vItem: TRALJSONObject;
  vMime: StringRAL;
begin
  Result := TRALJSONObject.Create;
  try
    if AKind = okTyping then
    begin
      Result.Add('type', 'typing');
      Exit;
    end;
    Result.Add('type', 'message');
    if AKind = okEdit then
      Result.Add('id', ATargetId);
    if AMessage.HasMedia then
    begin
      vMime := AMessage.Media.MimeType;
      vAttach := TRALJSONArray.Create;
      Result.Add('attachments', vAttach);
      vItem := TRALJSONObject.Create;
      vAttach.Add(vItem);
      if AMessage.Media.Data <> nil then
      begin
        if (vMime = '') or (Copy(string(vMime), 1, 6) <> 'image/') or
           (AMessage.Media.Data.Size > Capabilities.MaxMediaBytes) then
          raise ERALChatTeamsFinal.Create(emChatTmMedia);
        vItem.Add('contentType', vMime);
        vItem.Add('contentUrl', 'data:' + vMime + ';base64,' + Base64Of(AMessage.Media.Data));
      end
      else if AMessage.Media.Url <> '' then
      begin
        if vMime = '' then
          vMime := 'image/png';
        vItem.Add('contentType', vMime);
        vItem.Add('contentUrl', AMessage.Media.Url);
      end
      else
        raise ERALChatTeamsFinal.Create(emChatNoMedia);
      if AMessage.Media.FileName <> '' then
        vItem.Add('name', AMessage.Media.FileName);
    end;
    if Length(AMessage.Buttons) > 0 then
    begin
      if Result.Find('attachments') = nil then
        Result.Add('attachments', TRALJSONArray.Create);
      vAttach := TRALJSONArray(Result.Find('attachments'));
      vItem := TRALJSONObject.Create;
      vAttach.Add(vItem);
      vItem.Add('contentType', 'application/vnd.microsoft.card.adaptive');
      vItem.Add('content', AdaptiveCard(AMessage));
    end
    else if AMessage.Text <> '' then
    begin
      Result.Add('text', AMessage.Text);
      Result.Add('textFormat', 'markdown');
    end;
    if (AKind = okMessage) and AMessage.HasReplyTo and (AMessage.ReplyTo.Id <> '') then
      Result.Add('replyToId', AMessage.ReplyTo.Id);
  except
    Result.Free;
    raise;
  end;
end;

procedure TRALChatTeams.Call(AMethod: TRALMethod; const AUrl: StringRAL; ABody: TRALJSONObject;
  out AReply: TRALChatTeamsReply);
var
  vHeaders: TStringList;
  vHttp: TRALChatHttpReply;
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
      vHeaders.Add('Authorization=Bearer ' + string(AccessToken));
      if ABody <> nil then
        FHttp.Call(AMethod, AUrl, vHeaders, ABody.ToJSON, 'application/json', nil, '', nil, vHttp)
      else
        FHttp.Call(AMethod, AUrl, vHeaders, '', '', nil, '', nil, vHttp);
    except
      on E: ERALChatTeamsFinal do
        raise;
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
  AReply.Ok := (vHttp.Status >= 200) and (vHttp.Status < 300);
  AReply.Root := RALChatParseObject(vHttp.Text);
  if not AReply.Ok then
  begin
    AReply.Description := RALChatJStr(RALChatJObj(AReply.Root, 'error'), 'message');
    if AReply.Description = '' then
      AReply.Description := StringRAL('HTTP ' + IntToStr(vHttp.Status));
    // a refused token is asked for again on the next attempt
    if vHttp.Status = 401 then
    begin
      FLock.Enter;
      try
        FAccessToken := '';
      finally
        FLock.Leave;
      end;
    end;
  end;
end;

procedure TRALChatTeams.Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId: StringRAL; out AResult: TRALChatSendResult);
var
  vBody: TRALJSONObject;
  vReply: TRALChatTeamsReply;
  vService, vUrl, vWhat: StringRAL;
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
    vService := ServiceUrlOf(AMessage.Chat.Id);
    if vService = '' then
      raise ERALChatTeamsFinal.CreateFmt(emChatTmNoService, [string(AMessage.Chat.Id)]);
    if FRequireHttps and (Copy(LowerCase(string(vService)), 1, 8) <> 'https://') then
      raise ERALChatTeamsFinal.Create(emChatTmBadService);
    vUrl := TrimSlash(vService) + '/v3/conversations/' + RALChatUrlEncode(AMessage.Chat.Id) + '/activities';
    vBody := nil;
    case AKind of
      okDelete:
      begin
        vWhat := 'delete';
        vMethod := amDELETE;
        vUrl := vUrl + '/' + RALChatUrlEncode(ATargetId);
      end;
      okEdit:
      begin
        vWhat := 'update';
        vMethod := amPUT;
        vUrl := vUrl + '/' + RALChatUrlEncode(ATargetId);
        vBody := BuildActivity(AMessage, AKind, ATargetId);
      end;
      okTyping:
      begin
        vWhat := 'typing';
        vMethod := amPOST;
        vBody := BuildActivity(AMessage, AKind, '');
      end;
    else
      vWhat := 'reply';
      vMethod := amPOST;
      vBody := BuildActivity(AMessage, AKind, '');
      if AMessage.HasReplyTo and (AMessage.ReplyTo.Id <> '') then
        vUrl := vUrl + '/' + RALChatUrlEncode(AMessage.ReplyTo.Id);
    end;
    try
      Call(vMethod, vUrl, vBody, vReply);
    finally
      vBody.Free;
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

function TRALChatTeams.Dialect: TRALChatDialect;
begin
  // Teams reads CommonMark: ** for bold, _ or * for italic, ` for code, [text](url)
  Result := cdDiscord;
end;

procedure TRALChatTeams.FetchKeys(out AReport: StringRAL);
var
  vHeaders: TStringList;
  vReply: TRALChatHttpReply;
  vMeta, vSet: TRALJSONObject;
  vUri: StringRAL;
  vList: TRALJSONArray;
  vIdx, vJdx: Integer;
  vEntry: TRALJSONObject;
  vKey: TRALChatTeamsKey;
  vEnd: TRALJSONArray;
  vSpki: TBytes;
  vNew: TObjectList;
begin
  AReport := '';
  vHeaders := TStringList.Create;
  vNew := TObjectList.Create(True);
  try
    if FRequireHttps and (Copy(LowerCase(string(FOpenIdUrl)), 1, 8) <> 'https://') then
    begin
      AReport := 'OpenIdUrl is not https';
      Exit;
    end;
    FHttp.Call(amGET, FOpenIdUrl, vHeaders, '', '', nil, '', nil, vReply);
    if vReply.Failed or (vReply.Status <> 200) then
    begin
      AReport := Hide(StringRAL('metadata HTTP ' + IntToStr(vReply.Status) + ' ' + string(vReply.Failure)));
      Exit;
    end;
    vMeta := RALChatParseObject(vReply.Text);
    try
      vUri := RALChatJStr(vMeta, 'jwks_uri');
    finally
      vMeta.Free;
    end;
    if (vUri = '') or (FRequireHttps and (Copy(LowerCase(string(vUri)), 1, 8) <> 'https://')) then
    begin
      AReport := 'jwks_uri is missing or not https';
      Exit;
    end;
    FHttp.Call(amGET, vUri, vHeaders, '', '', nil, '', nil, vReply);
    if vReply.Failed or (vReply.Status <> 200) then
    begin
      AReport := Hide(StringRAL('keys HTTP ' + IntToStr(vReply.Status) + ' ' + string(vReply.Failure)));
      Exit;
    end;
    vSet := RALChatParseObject(vReply.Text);
    try
      vList := RALChatJArr(vSet, 'keys');
      if vList <> nil then
        for vIdx := 0 to vList.Count - 1 do
        begin
          if vList[vIdx].Kind <> rjkObject then
            Continue;
          vEntry := TRALJSONObject(vList[vIdx]);
          if (RALChatJStr(vEntry, 'kty') <> 'RSA') or (RALChatJStr(vEntry, 'kid') = '') then
            Continue;
          vKey := TRALChatTeamsKey.Create;
          try
            vKey.Kid := RALChatJStr(vEntry, 'kid');
            vSpki := DerSequence([DerSequence([DerOID(OID_RSA_ENCRYPTION), DerNull]),
              DerBitString(DerSequence([DerIntegerBytes(RALBase64UrlDecode(RALChatJStr(vEntry, 'n'))),
                DerIntegerBytes(RALBase64UrlDecode(RALChatJStr(vEntry, 'e')))]))]);
            vKey.Key := TRALRSAKey.FromPublicKeyInfo(vSpki);
            vEnd := RALChatJArr(vEntry, 'endorsements');
            if vEnd <> nil then
              for vJdx := 0 to vEnd.Count - 1 do
                vKey.Endorsements.Add(string(RALChatJText(vEnd[vJdx])));
            vNew.Add(vKey);
            vKey := nil;
          finally
            vKey.Free;
          end;
        end;
    finally
      vSet.Free;
    end;
    if vNew.Count = 0 then
    begin
      AReport := 'no RSA key in the set';
      Exit;
    end;
    FLock.Enter;
    try
      // the old keys may be in use by a request: kept until the channel is freed
      FRetired.Add(FKeys);
      FKeys := vNew;
      vNew := nil;
      FKeysFetched := UnixNow;
    finally
      FLock.Leave;
    end;
  finally
    vNew.Free;
    vHeaders.Free;
  end;
end;

function TRALChatTeams.FindKey(const AKid: StringRAL): TRALChatTeamsKey;
var
  vReport: StringRAL;
  vNow: Int64RAL;

  function Lookup: TRALChatTeamsKey;
  var
    vJdx: Integer;
  begin
    Result := nil;
    for vJdx := 0 to FKeys.Count - 1 do
      if TRALChatTeamsKey(FKeys[vJdx]).Kid = AKid then
      begin
        Result := TRALChatTeamsKey(FKeys[vJdx]);
        Exit;
      end;
  end;

begin
  vNow := UnixNow;
  FLock.Enter;
  try
    if (FKeys.Count > 0) and (vNow - FKeysFetched < Int64RAL(FKeysLife) * 60) then
    begin
      Result := Lookup;
      // a key that is not there may be a new one: asked again, but not too often
      if (Result <> nil) or (vNow - FKeysRetry < cKeysRetry) then
        Exit;
    end;
    FKeysRetry := vNow;
  finally
    FLock.Leave;
  end;
  FetchKeys(vReport);
  FLock.Enter;
  try
    Result := Lookup;
  finally
    FLock.Leave;
  end;
  if (Result = nil) and (vReport <> '') and (FBot <> nil) then
    FBot.ReportError(Self, Format(emChatTmKeys, [string(vReport)]));
end;

procedure TRALChatTeams.FillResult(const AWhat: StringRAL; const AReply: TRALChatTeamsReply;
  var AResult: TRALChatSendResult);
begin
  AResult.Ok := AReply.Ok;
  if AReply.Ok then
  begin
    AResult.MessageId := RALChatJStr(AReply.Root, 'id');
    Exit;
  end;
  AResult.ErrorText := Hide(StringRAL(Format(emChatTmApi, [string(AWhat), string(AReply.Description)])));
  if AReply.Failed or (AReply.Status >= 500) or (AReply.Status = 429) or (AReply.Status = 401) then
  begin
    AResult.Transient := True;
    AResult.RetryAfterMs := AReply.RetryAfterMs;
  end;
end;

function TRALChatTeams.GetClient: TRALClient;
begin
  Result := FHttp.Client;
end;

function TRALChatTeams.GetEngineType: string;
begin
  Result := FHttp.EngineType;
end;

function TRALChatTeams.Hide(const AText: StringRAL): StringRAL;
var
  vSecret: StringRAL;
begin
  Result := AText;
  vSecret := '';
  try
    vSecret := GetToken;
  except
    // no secret: nothing to hide
  end;
  if Length(vSecret) >= 4 then
    Result := StringRAL(StringReplace(string(Result), string(vSecret), '***', [rfReplaceAll]));
  FLock.Enter;
  try
    vSecret := FAccessToken;
  finally
    FLock.Leave;
  end;
  if Length(vSecret) >= 8 then
    Result := StringRAL(StringReplace(string(Result), string(vSecret), '***', [rfReplaceAll]));
end;

function TRALChatTeams.LoadMedia(AMedia: TRALChatMedia): TStream;
var
  vHeaders: TStringList;
begin
  if AMedia.Url = '' then
    raise ERALChatError.Create(emChatNoMedia);
  if (AMedia.Size > 0) and (AMedia.Size > FMaxDownloadSize) then
    raise ERALChatError.CreateFmt(emChatTmTooBig, [FMaxDownloadSize]);
  if not MediaHostOk(AMedia.Url) then
    raise ERALChatError.Create(emChatTmBadHost);
  vHeaders := TStringList.Create;
  try
    // the connector's attachment store wants the bot's token; the others must not get it
    if Pos('.trafficmanager.net', LowerCase(string(AMedia.Url))) > 0 then
      vHeaders.Add('Authorization=Bearer ' + string(AccessToken));
    Result := FHttp.Download(AMedia.Url, vHeaders, FMaxDownloadSize);
  finally
    vHeaders.Free;
  end;
end;

function TRALChatTeams.MediaHostOk(const AUrl: StringRAL): Boolean;
var
  vText, vHost: string;
  vP, vIdx: Integer;
  vSuffixes: TStringList;
begin
  Result := False;
  vText := string(AUrl);
  if FRequireHttps and (Copy(LowerCase(vText), 1, 8) <> 'https://') then
    Exit;
  vHost := vText;
  vP := Pos('://', vHost);
  if vP > 0 then
    Delete(vHost, 1, vP + 2);
  vP := Pos('/', vHost);
  if vP > 0 then
    vHost := Copy(vHost, 1, vP - 1);
  vP := Pos(':', vHost);
  if vP > 0 then
    vHost := Copy(vHost, 1, vP - 1);
  vHost := LowerCase(vHost);
  if (vHost = '') or (Pos('@', vHost) > 0) then
    Exit;
  vSuffixes := TStringList.Create;
  try
    vSuffixes.CommaText := string(FMediaHosts);
    for vIdx := 0 to vSuffixes.Count - 1 do
      if (vSuffixes[vIdx] <> '') and (Length(vHost) >= Length(vSuffixes[vIdx])) and
         (Copy(vHost, Length(vHost) - Length(vSuffixes[vIdx]) + 1, MaxInt) = LowerCase(vSuffixes[vIdx])) then
      begin
        Result := True;
        Exit;
      end;
  finally
    vSuffixes.Free;
  end;
end;

procedure TRALChatTeams.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then
    Exit;
  if AComponent = FHttp.Client then
    FHttp.Client := nil;
  if AComponent = FBot then
    FBot := nil;
end;

procedure TRALChatTeams.ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList);
var
  vRoot, vConv, vFromObj, vValue, vAtt, vContent: TRALJSONObject;
  vMessage: TRALChatMessage;
  vInfo: TRALChatInfo;
  vUser: TRALChatUser;
  vAtts: TRALJSONArray;
  vIdx: Integer;
  vType, vMime: StringRAL;
  vLoader: TRALChatMediaLoader;
begin
  vRoot := RALChatParseObject(ABody);
  if vRoot = nil then
    raise ERALChatError.CreateFmt(emChatBadWebhook, [string(ChannelName)]);
  try
    if RALChatJStr(vRoot, 'type') <> 'message' then
      Exit;
    vConv := RALChatJObj(vRoot, 'conversation');
    vInfo.Id := RALChatJStr(vConv, 'id');
    if vInfo.Id = '' then
      Exit;
    RememberConversation(vInfo.Id, RALChatJStr(vRoot, 'serviceUrl'));
    vInfo.Title := RALChatJStr(vConv, 'name');
    if RALChatJStr(vConv, 'conversationType') = 'personal' then
      vInfo.ChatType := cctPrivate
    else
      vInfo.ChatType := cctGroup;
    vMessage := TRALChatMessage.Create;
    try
      vMessage.Chat := vInfo;
      vMessage.Id := RALChatJStr(vRoot, 'id');
      vMessage.EventId := 'tm' + vMessage.Id;
      vFromObj := RALChatJObj(vRoot, 'from');
      vUser.Id := RALChatJStr(vFromObj, 'id');
      vUser.Name := RALChatJStr(vFromObj, 'name');
      vUser.Phone := '';
      vMessage.From := vUser;
      vValue := RALChatJObj(vRoot, 'value');
      if (vValue <> nil) and (RALChatJStr(vValue, 'ralId') <> '') then
      begin
        vMessage.Kind := ckCallback;
        vMessage.CallbackData := RALChatJStr(vValue, 'ralId');
        vMessage.ReplyTo.Id := RALChatJStr(vRoot, 'replyToId');
        vMessage.ReplyTo.Chat := vInfo;
      end
      else
      begin
        vMessage.Text := StripMentions(RALChatJStr(vRoot, 'text'));
        if RALChatJStr(vRoot, 'replyToId') <> '' then
          vMessage.ReplyTo.Id := RALChatJStr(vRoot, 'replyToId');
        vAtts := RALChatJArr(vRoot, 'attachments');
        if vAtts <> nil then
          for vIdx := 0 to vAtts.Count - 1 do
          begin
            if vAtts[vIdx].Kind <> rjkObject then
              Continue;
            vAtt := TRALJSONObject(vAtts[vIdx]);
            vType := RALChatJStr(vAtt, 'contentType');
            vMessage.Media.FileName := RALChatJStr(vAtt, 'name');
            if vType = 'application/vnd.microsoft.teams.file.download.info' then
            begin
              vContent := RALChatJObj(vAtt, 'content');
              vMessage.Media.Url := RALChatJStr(vContent, 'downloadUrl');
              vMime := '';
              vMessage.Kind := ckDocument;
            end
            else if (RALChatJStr(vAtt, 'contentUrl') <> '') and
                    (Copy(string(vType), 1, 6) <> 'text/h') and
                    (Pos('card', string(vType)) = 0) then
            begin
              vMessage.Media.Url := RALChatJStr(vAtt, 'contentUrl');
              vMime := vType;
              if Copy(string(vType), 1, 6) = 'image/' then
                vMessage.Kind := ckImage
              else
                vMessage.Kind := ckDocument;
            end
            else
              Continue;
            vMessage.Media.MimeType := vMime;
            vLoader := LoadMedia;
            vMessage.Media.Loader := vLoader;
            Break;
          end;
      end;
      if KeepRaw then
        vMessage.Raw := vRoot.ToJSON;
      AMessages.AddMessage(vMessage);
      vMessage := nil;
    finally
      vMessage.Free;
    end;
  finally
    vRoot.Free;
  end;
end;

procedure TRALChatTeams.RememberConversation(const AConversationId, AServiceUrl: StringRAL);
var
  vIdx: Integer;
begin
  if (AConversationId = '') or (AServiceUrl = '') then
    Exit;
  FLock.Enter;
  try
    vIdx := FServiceUrls.IndexOfName(string(AConversationId));
    if vIdx >= 0 then
      FServiceUrls.ValueFromIndex[vIdx] := string(AServiceUrl)
    else
    begin
      if FServiceUrls.Count >= 10000 then
        FServiceUrls.Delete(0);
      FServiceUrls.Add(string(AConversationId) + '=' + string(AServiceUrl));
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALChatTeams.ServiceUrlOf(const AConversation: StringRAL): StringRAL;
var
  vIdx: Integer;
begin
  Result := '';
  FLock.Enter;
  try
    vIdx := FServiceUrls.IndexOfName(string(AConversation));
    if vIdx >= 0 then
      Result := StringRAL(FServiceUrls.ValueFromIndex[vIdx]);
  finally
    FLock.Leave;
  end;
  if Result = '' then
    Result := FDefaultServiceUrl;
end;

procedure TRALChatTeams.SetBot(AValue: TRALChatBot);
begin
  if FBot = AValue then
    Exit;
  if FBot <> nil then
    FBot.RemoveFreeNotification(Self);
  FBot := AValue;
  if FBot <> nil then
    FBot.FreeNotification(Self);
end;

procedure TRALChatTeams.SetClient(AValue: TRALClient);
begin
  if FHttp.Client = AValue then
    Exit;
  if FHttp.Client <> nil then
    FHttp.Client.RemoveFreeNotification(Self);
  FHttp.Client := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

procedure TRALChatTeams.SetEngineType(const AValue: string);
begin
  FHttp.EngineType := AValue;
end;

function TRALChatTeams.StripMentions(const AText: StringRAL): StringRAL;
var
  vStart, vEnd: Integer;
  vText: string;
begin
  vText := string(AText);
  repeat
    vStart := Pos('<at>', vText);
    vEnd := Pos('</at>', vText);
    if (vStart = 0) or (vEnd < vStart) then
      Break;
    Delete(vText, vStart, vEnd - vStart + 5);
  until False;
  Result := StringRAL(Trim(vText));
end;

function TRALChatTeams.TestCredentials(out AReport: StringRAL): Boolean;
var
  vReport: StringRAL;
begin
  Result := False;
  try
    AccessToken;
  except
    on E: Exception do
    begin
      AReport := Hide(StringRAL(E.Message));
      Exit;
    end;
  end;
  FetchKeys(vReport);
  if vReport <> '' then
  begin
    AReport := Hide(StringRAL(Format(emChatTmKeys, [string(vReport)])));
    Exit;
  end;
  FLock.Enter;
  try
    AReport := StringRAL(Format(cmChatTmCredentialsOk, [FKeys.Count]));
  finally
    FLock.Leave;
  end;
  Result := True;
end;

function TRALChatTeams.UnixNow: Int64RAL;
begin
  if FBot <> nil then
    Result := FBot.UnixNow
  else
    Result := MilliSecondsBetween(RALDateTimeToGMT(SysUtils.Now), UnixDateDelta) div 1000;
end;

procedure TRALChatTeams.VerifyRequest(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
  var ATimestamp: Int64RAL; var AValid: Boolean);
var
  vAuth, vHead, vClaims, vSig: StringRAL;
  vParts: TStringList;
  vHeadObj, vClaimsObj, vActivity: TRALJSONObject;
  vKey: TRALChatTeamsKey;
  vNow, vExp, vNbf: Int64RAL;
  vIssuers: TStringList;
  vAud: TRALJSON;
  vOk: Boolean;
  vIdx: Integer;
  vService, vChannel: StringRAL;
begin
  AValid := False;
  if (FAppId = '') or not Assigned(AHeaders) then
    Exit;
  vAuth := AHeaders('Authorization');
  if Copy(LowerCase(string(vAuth)), 1, 7) <> 'bearer ' then
    Exit;
  vAuth := StringRAL(Trim(Copy(string(vAuth), 8, MaxInt)));
  vParts := TStringList.Create;
  vIssuers := TStringList.Create;
  vHeadObj := nil;
  vClaimsObj := nil;
  vActivity := nil;
  try
    vParts.Delimiter := '.';
    vParts.StrictDelimiter := True;
    vParts.DelimitedText := string(vAuth);
    if vParts.Count <> 3 then
      Exit;
    vHead := StringRAL(vParts[0]);
    vClaims := StringRAL(vParts[1]);
    vSig := StringRAL(vParts[2]);
    try
      vHeadObj := RALChatParseObject(TextOf(RALBase64UrlDecode(vHead)));
      vClaimsObj := RALChatParseObject(TextOf(RALBase64UrlDecode(vClaims)));
    except
      Exit;
    end;
    if (vHeadObj = nil) or (vClaimsObj = nil) then
      Exit;
    // only RS256: "none" and a symmetric algorithm are what an attacker would offer
    if RALChatJStr(vHeadObj, 'alg') <> 'RS256' then
      Exit;
    vIssuers.CommaText := string(FValidIssuers);
    vOk := False;
    for vIdx := 0 to vIssuers.Count - 1 do
      if vIssuers[vIdx] = string(RALChatJStr(vClaimsObj, 'iss')) then
        vOk := True;
    if not vOk then
      Exit;
    vAud := vClaimsObj.Find('aud');
    vOk := False;
    if (vAud <> nil) and (vAud.Kind = rjkString) then
      vOk := vAud.AsString = FAppId
    else if (vAud <> nil) and (vAud.Kind = rjkArray) then
      for vIdx := 0 to TRALJSONArray(vAud).Count - 1 do
        if RALChatJText(TRALJSONArray(vAud)[vIdx]) = FAppId then
          vOk := True;
    if not vOk then
      Exit;
    vNow := UnixNow;
    vExp := RALChatJInt(vClaimsObj, 'exp', 0);
    vNbf := RALChatJInt(vClaimsObj, 'nbf', 0);
    if (vExp = 0) or (vNow > vExp + cSkew) or ((vNbf > 0) and (vNow < vNbf - cSkew)) then
      Exit;
    vActivity := RALChatParseObject(ABody);
    if vActivity = nil then
      Exit;
    vService := RALChatJStr(vActivity, 'serviceUrl');
    vChannel := RALChatJStr(vActivity, 'channelId');
    // the token names the service it was issued for: a reply goes only there
    if (RALChatJStr(vClaimsObj, 'serviceurl') <> '') and
       (LowerCase(string(TrimSlash(RALChatJStr(vClaimsObj, 'serviceurl')))) <>
        LowerCase(string(TrimSlash(vService)))) then
      Exit;
    vKey := FindKey(RALChatJStr(vHeadObj, 'kid'));
    if vKey = nil then
      Exit;
    if (vKey.Endorsements.Count > 0) and (vChannel <> 'emulator') and
       (vKey.Endorsements.IndexOf(string(vChannel)) < 0) then
      Exit;
    FLock.Enter;
    try
      AValid := vKey.Key.Verify(BytesOf(vHead + '.' + vClaims), RALBase64UrlDecode(vSig));
    finally
      FLock.Leave;
    end;
    if AValid then
      ATimestamp := 0;
  finally
    vActivity.Free;
    vClaimsObj.Free;
    vHeadObj.Free;
    vIssuers.Free;
    vParts.Free;
  end;
end;

end.
