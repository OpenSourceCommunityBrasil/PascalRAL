/// The Graph API base of the Meta channels (WhatsApp Cloud API, Messenger, Instagram).
unit RALChatGraph;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALJson, RALClient,
  RALChatMessage, RALChatVerify, RALChatChannel, RALChatHttp;

const
  /// Default address of the Graph API.
  cRALChatGraphApi = 'https://graph.facebook.com';
  /// Default version of the Graph API.
  cRALChatGraphVersion = 'v27.0';

type
  /// What one call to the Graph API came to.
  TRALChatGraphReply = record
    /// Whether the call worked (a 2xx answer without an error object).
    Ok: Boolean;
    /// True when no answer came.
    Failed: Boolean;
    /// The HTTP status; 0 when Failed.
    Status: Integer;
    /// The error.code of the answer; 0 when there is none.
    Code: Integer;
    /// The error.error_subcode of the answer; 0 when there is none.
    Subcode: Integer;
    /// The error.message of the answer, or the transport failure.
    Description: StringRAL;
    /// The fbtrace_id Meta gives an error, for its support.
    TraceId: StringRAL;
    /// Milliseconds the answer asks to wait (Retry-After); 0 = none.
    RetryAfterMs: Int64RAL;
    /// The answer parsed, or nil; the caller frees it.
    Root: TRALJSONObject;
  end;

  { The base of the channels on Meta's Graph API: calls with the access token, the
    X-Hub-Signature-256 check, the verify token handshake, media from Meta hosts only
    and Graph errors read into delivery results. }
  TRALChatGraphChannel = class(TRALChatChannel)
  private
    FApiBase: StringRAL;
    FApiVersion: StringRAL;
    FAppSecret: StringRAL;
    /// The HTTP client of the channel.
    FHttp: TRALChatHttp;
    /// Guards the pair failures and the seen keys.
    FLock: TCriticalSection;
    FMaxDownloadSize: Int64RAL;
    /// Name=count of the 131056 failures in a row of each pair.
    FPairFailures: TStringList;
    /// The keys of the last notifications handled, oldest first.
    FSeen: TStringList;
    /// The verifier of the webhook (owned by the base).
    FSigVerifier: TRALChatHMACVerifier;
    FVerifyToken: StringRAL;

    function GetClient: TRALClient;
    function GetEngineType: string;
    procedure SetAppSecret(const AValue: StringRAL);
    procedure SetClient(AValue: TRALClient);
    procedure SetEngineType(const AValue: string);
  protected
    { Makes one call to the API (APath after the version, e.g. 123/messages) and says how
      it came out; never raises. With AFileField the fields of AFields and the stream of
      AMedia go as a multipart, otherwise ABody as JSON (nil = no body). }
    procedure Call(AMethod: TRALMethod; const APath: StringRAL; ABody: TRALJSONObject;
      out AReply: TRALChatGraphReply; AFields: TRALJSONObject = nil;
      const AFileField: StringRAL = ''; AMedia: TRALChatMedia = nil);
    /// Downloads AUrl, from a Meta host only, with the token when AWithToken.
    function DownloadFrom(const AUrl: StringRAL; AWithToken: Boolean): TStream;
    { Turns a reply into a delivery result; APair is the key of the pair the message went
      to (the WhatsApp 131056 rule counts failures in a row per pair). }
    procedure FillResult(const AOperation, APair: StringRAL; const AReply: TRALChatGraphReply;
      var AResult: TRALChatSendResult);
    /// Hides the secrets in AText.
    function Hide(const AText: StringRAL): StringRAL;
    /// Whether AUrl is a Meta host (https) or the host of ApiBase.
    function HostOk(const AUrl: StringRAL): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// Clears AResult to a failure with no reason.
    procedure ResetResult(out AResult: TRALChatSendResult);
    { Whether AKey was already seen among the last notifications (a resent webhook);
      remembers it when not. }
    function Seen(const AKey: StringRAL): Boolean;
    /// Cuts AText at AMax characters (not bytes).
    function Shorten(const AText: StringRAL; AMax: Integer): StringRAL;
    /// The webhook fields the Meta app subscribes for this channel.
    function SubscribeFields: StringRAL; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Answers Meta's webhook check: a GET with hub.mode=subscribe and hub.verify_token
      equal to VerifyToken (compared in constant time) gets hub.challenge back; anything
      else gets 403. }
    function Handshake(AQuery: TRALChatHeaderReader; out AStatus: Integer;
      out ABody: StringRAL): Boolean; override;
    { Asks Meta whose token it is: True with a text naming the account, or False with
      the reason in AReport. }
    function TestToken(out AReport: StringRAL): Boolean; virtual; abstract;
    { The text to fill the webhook of the Meta app with: the callback address
      (AServerUrl, the public address of the webhook module, plus the channel name),
      the verify token and the fields to subscribe. }
    function WebhookInstructions(const AServerUrl: StringRAL): StringRAL; virtual;
  published
    { The address of the Graph API, without the final slash; another points the channel
      at a test server (or graph.instagram.com for Instagram Login). }
    property ApiBase: StringRAL read FApiBase write FApiBase;
    /// The version of the Graph API in the path ('v27.0'); empty = the app's default.
    property ApiVersion: StringRAL read FApiVersion write FApiVersion;
    { The app secret: the HMAC key of X-Hub-Signature-256; empty refuses every webhook.
      Set it from the environment in the program rather than in the form. }
    property AppSecret: StringRAL read FAppSecret write SetAppSecret;
    { The client that calls the API; leave its BaseURL empty (the channel sends whole
      URLs). Without one the channel makes its own, of EngineType. }
    property Client: TRALClient read GetClient write SetClient;
    /// The engine of the channel's own client; empty = the first registered.
    property EngineType: string read GetEngineType write SetEngineType;
    /// The largest file taken from Meta, in bytes.
    property MaxDownloadSize: Int64RAL read FMaxDownloadSize write FMaxDownloadSize
      default 104857600;
    { The text the Meta app sends in hub.verify_token when the webhook is registered;
      empty refuses the check. }
    property VerifyToken: StringRAL read FVerifyToken write FVerifyToken;
  end;

implementation

const
  /// How many notification keys Seen remembers.
  cSeenMax = 2048;
  /// How many pairs the 131056 rule remembers.
  cPairMax = 1024;

{ TRALChatGraphChannel }

constructor TRALChatGraphChannel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FPairFailures := TStringList.Create;
  FSeen := TStringList.Create;
  FHttp := TRALChatHttp.Create;
  FApiBase := cRALChatGraphApi;
  FApiVersion := cRALChatGraphVersion;
  FMaxDownloadSize := 104857600;
  FSigVerifier := TRALChatHMACVerifier.Create;
  FSigVerifier.HeaderName := 'X-Hub-Signature-256';
  FSigVerifier.Prefix := 'sha256=';
  FSigVerifier.Encoding := seHex;
  Verifier := FSigVerifier;
end;

destructor TRALChatGraphChannel.Destroy;
begin
  FreeAndNil(FHttp);
  FSeen.Free;
  FPairFailures.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALChatGraphChannel.Call(AMethod: TRALMethod; const APath: StringRAL;
  ABody: TRALJSONObject; out AReply: TRALChatGraphReply; AFields: TRALJSONObject;
  const AFileField: StringRAL; AMedia: TRALChatMedia);
var
  vHeaders: TStringList;
  vHttp: TRALChatHttpReply;
  vUrl: StringRAL;
  vError: TRALJSONObject;
begin
  AReply.Ok := False;
  AReply.Failed := True;
  AReply.Status := 0;
  AReply.Code := 0;
  AReply.Subcode := 0;
  AReply.Description := '';
  AReply.TraceId := '';
  AReply.RetryAfterMs := 0;
  AReply.Root := nil;
  vHeaders := TStringList.Create;
  try
    try
      vHeaders.Add('Authorization=Bearer ' + string(GetToken));
      vUrl := FApiBase + '/';
      if FApiVersion <> '' then
        vUrl := vUrl + FApiVersion + '/';
      vUrl := vUrl + APath;
      if AFileField <> '' then
        FHttp.Call(AMethod, vUrl, vHeaders, '', '', AFields, AFileField, AMedia, vHttp)
      else if ABody <> nil then
        FHttp.Call(AMethod, vUrl, vHeaders, ABody.ToJSON, 'application/json; charset=utf-8',
          nil, '', nil, vHttp)
      else
        FHttp.Call(AMethod, vUrl, vHeaders, '', '', nil, '', nil, vHttp);
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
  vError := RALChatJObj(AReply.Root, 'error');
  AReply.Ok := (vHttp.Status >= 200) and (vHttp.Status < 300) and (vError = nil);
  if AReply.Ok then
    Exit;
  AReply.Code := RALChatJInt(vError, 'code', 0);
  AReply.Subcode := RALChatJInt(vError, 'error_subcode', 0);
  AReply.Description := Hide(RALChatJStr(vError, 'message'));
  AReply.TraceId := RALChatJStr(vError, 'fbtrace_id');
  if AReply.Description = '' then
    AReply.Description := StringRAL('HTTP ' + IntToStr(vHttp.Status));
end;

function TRALChatGraphChannel.DownloadFrom(const AUrl: StringRAL; AWithToken: Boolean): TStream;
var
  vHeaders: TStringList;
begin
  if AUrl = '' then
    raise ERALChatError.Create(emChatNoMedia);
  if not HostOk(AUrl) then
    raise ERALChatError.Create(emChatMeBadHost);
  vHeaders := TStringList.Create;
  try
    if AWithToken then
      vHeaders.Add('Authorization=Bearer ' + string(GetToken));
    Result := FHttp.Download(AUrl, vHeaders, FMaxDownloadSize);
  finally
    vHeaders.Free;
  end;
end;

procedure TRALChatGraphChannel.FillResult(const AOperation, APair: StringRAL;
  const AReply: TRALChatGraphReply; var AResult: TRALChatSendResult);
var
  vIdx, vCount, vPower: Integer;
  vWait: Int64RAL;
begin
  AResult.Ok := AReply.Ok;
  AResult.Transient := False;
  AResult.RetryAfterMs := 0;
  AResult.RetryScope := rsChat;
  if AReply.Ok then
  begin
    if APair <> '' then
    begin
      FLock.Enter;
      try
        vIdx := FPairFailures.IndexOfName(string(APair));
        if vIdx >= 0 then
          FPairFailures.Delete(vIdx);
      finally
        FLock.Leave;
      end;
    end;
    Exit;
  end;
  AResult.ErrorText := Hide(StringRAL(Format(emChatMeApi, [string(AOperation),
    string(AReply.Description), AReply.Code, AReply.Subcode, string(AReply.TraceId)])));
  case AReply.Code of
    131056:
    begin
      // the pair rule: try again after 4^X seconds, X the failures in a row
      FLock.Enter;
      try
        vIdx := FPairFailures.IndexOfName(string(APair));
        if vIdx >= 0 then
        begin
          vCount := StrToIntDef(FPairFailures.ValueFromIndex[vIdx], 0);
          FPairFailures.Delete(vIdx);
        end
        else
          vCount := 0;
        if FPairFailures.Count >= cPairMax then
          FPairFailures.Delete(0);
        FPairFailures.Add(string(APair) + '=' + IntToStr(vCount + 1));
      finally
        FLock.Leave;
      end;
      vWait := 1000;
      for vPower := 1 to vCount do
        if vWait < 3600000 then
          vWait := vWait * 4;
      AResult.Transient := True;
      AResult.RetryAfterMs := vWait;
      AResult.RetryScope := rsChat;
    end;
    130429:
    begin
      AResult.Transient := True;
      AResult.RetryAfterMs := 1000;
      AResult.RetryScope := rsChannel;
    end;
    4, 17, 32, 613, 80007:
    begin
      AResult.Transient := True;
      AResult.RetryAfterMs := 60000;
      AResult.RetryScope := rsChannel;
    end;
    1, 2, 131000, 131016, 133004:
      AResult.Transient := True;
    131047:
      AResult.ErrorText := StringRAL(emChatWaWindow);
    190:
      AResult.ErrorText := StringRAL(Format(emChatMeToken, [string(ChannelName)]));
  else
    if (AReply.Code = 10) and (AReply.Subcode = 2018278) then
      AResult.ErrorText := StringRAL(emChatMeWindow)
    else if AReply.Failed or (AReply.Status >= 500) or (AReply.Status = 429) then
    begin
      AResult.Transient := True;
      AResult.RetryScope := rsChannel;
    end;
  end;
  if AResult.Transient and (AReply.RetryAfterMs > AResult.RetryAfterMs) then
    AResult.RetryAfterMs := AReply.RetryAfterMs;
end;

function TRALChatGraphChannel.GetClient: TRALClient;
begin
  Result := FHttp.Client;
end;

function TRALChatGraphChannel.GetEngineType: string;
begin
  Result := FHttp.EngineType;
end;

function TRALChatGraphChannel.Handshake(AQuery: TRALChatHeaderReader; out AStatus: Integer;
  out ABody: StringRAL): Boolean;
var
  vChallenge: StringRAL;
begin
  Result := True;
  ABody := '';
  AStatus := 403;
  if not Assigned(AQuery) or (FVerifyToken = '') then
    Exit;
  vChallenge := AQuery('hub.challenge');
  if (AQuery('hub.mode') = 'subscribe') and (vChallenge <> '') and
     RALSameSecret(AQuery('hub.verify_token'), FVerifyToken) then
  begin
    AStatus := 200;
    ABody := vChallenge;
  end;
end;

function TRALChatGraphChannel.Hide(const AText: StringRAL): StringRAL;
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
        1: vSecret := FAppSecret;
      else
        vSecret := FVerifyToken;
      end;
    except
      // not set: nothing to hide
    end;
    if Length(vSecret) >= 4 then
      Result := StringRAL(StringReplace(string(Result), string(vSecret), '***', [rfReplaceAll]));
  end;
end;

function TRALChatGraphChannel.HostOk(const AUrl: StringRAL): Boolean;
var
  vHost, vApi: string;

  function HostOf(const AValue: string): string;
  begin
    Result := RALChatUrlHost(StringRAL(AValue));
  end;

  function EndsWith(const ASuffix: string): Boolean;
  begin
    Result := (Length(vHost) > Length(ASuffix)) and
      (Copy(vHost, Length(vHost) - Length(ASuffix) + 1, Length(ASuffix)) = ASuffix);
  end;

begin
  vHost := HostOf(string(AUrl));
  vApi := HostOf(string(FApiBase));
  Result := False;
  if (vHost = '') or (Pos('@', vHost) > 0) or (Pos('\', string(AUrl)) > 0) then
    Exit;
  if vHost = vApi then
  begin
    Result := LowerCase(Copy(string(AUrl), 1, Pos('://', string(AUrl)))) =
      LowerCase(Copy(string(FApiBase), 1, Pos('://', string(FApiBase))));
    Exit;
  end;
  // a Meta host is reached over https only
  if LowerCase(Copy(string(AUrl), 1, 8)) <> 'https://' then
    Exit;
  Result := (vHost = 'graph.facebook.com') or (vHost = 'graph.instagram.com') or
    EndsWith('.fbcdn.net') or EndsWith('.fbsbx.com') or EndsWith('.whatsapp.net') or
    EndsWith('.cdninstagram.com');
end;

procedure TRALChatGraphChannel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FHttp <> nil) and (AComponent = FHttp.Client) then
    FHttp.Client := nil;
end;

procedure TRALChatGraphChannel.ResetResult(out AResult: TRALChatSendResult);
begin
  AResult.Ok := False;
  AResult.MessageId := '';
  AResult.Transient := False;
  AResult.RetryAfterMs := 0;
  AResult.RetryScope := rsChat;
  AResult.ErrorText := '';
  AResult.Info := '';
end;

function TRALChatGraphChannel.Seen(const AKey: StringRAL): Boolean;
begin
  FLock.Enter;
  try
    Result := FSeen.IndexOf(string(AKey)) >= 0;
    if Result then
      Exit;
    if FSeen.Count >= cSeenMax then
      FSeen.Delete(0);
    FSeen.Add(string(AKey));
  finally
    FLock.Leave;
  end;
end;

function TRALChatGraphChannel.SubscribeFields: StringRAL;
begin
  Result := 'messages';
end;

procedure TRALChatGraphChannel.SetAppSecret(const AValue: StringRAL);
begin
  FAppSecret := AValue;
  FSigVerifier.Secret := AValue;
end;

procedure TRALChatGraphChannel.SetClient(AValue: TRALClient);
begin
  if FHttp.Client = AValue then
    Exit;
  if FHttp.Client <> nil then
    FHttp.Client.RemoveFreeNotification(Self);
  FHttp.Client := AValue;
  if AValue <> nil then
    AValue.FreeNotification(Self);
end;

procedure TRALChatGraphChannel.SetEngineType(const AValue: string);
begin
  FHttp.EngineType := AValue;
end;

function TRALChatGraphChannel.Shorten(const AText: StringRAL; AMax: Integer): StringRAL;
var
  vWide: UnicodeString;
begin
  vWide := UnicodeString(AText);
  if Length(vWide) <= AMax then
    Result := AText
  else
    Result := StringRAL(Copy(vWide, 1, AMax));
end;

function TRALChatGraphChannel.WebhookInstructions(const AServerUrl: StringRAL): StringRAL;
var
  vUrl: string;
  vToken: string;
begin
  vUrl := string(AServerUrl);
  while (vUrl <> '') and (vUrl[Length(vUrl)] = '/') do
    Delete(vUrl, Length(vUrl), 1);
  vToken := string(FVerifyToken);
  if vToken = '' then
    vToken := cmChatMeNoVerifyToken;
  Result := StringRAL(Format(cmChatMeWebhookInfo, [vUrl + '/' + string(ChannelName), vToken,
    string(SubscribeFields)]));
end;

end.
