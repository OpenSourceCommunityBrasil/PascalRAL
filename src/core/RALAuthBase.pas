/// Base classes of every authenticator: the client contract and the server plugin.
unit RALAuthBase;

interface

uses
  Classes, SysUtils, SyncObjs,
  RALConsts, RALTypes, RALRoutes, RALTools, RALRequest, RALParams, RALResponse,
  RALCustomObjects, RALPlugin;

type
  TRALOnBeforeGetToken = procedure(ARequest: TRALRequest) of object;

  /// Base class of authenticators
  TRALAuthentication = class(TRALComponent)
  private
    FAuthType: TRALAuthTypes;
  protected
    procedure SetAuthType(AType: TRALAuthTypes);
  public
    constructor Create(AOwner: TComponent); override;
    property AuthType: TRALAuthTypes read FAuthType;
  end;

  { TRALAuthTransport }

  /// How a client authenticator reaches the network: the requests of its own
  /// (a token request) go through the same engine, BaseURL and TLS policy as
  /// the request being authenticated. TRALClient hands one to Prepare
  TRALAuthTransport = class
  public
    /// Makes the authenticated request fail with AMessage: a provider's error,
    /// a token that could not be read. The client raises it
    procedure Fail(AResponse: TRALResponse; const AMessage: StringRAL); virtual; abstract;
    /// Makes the authenticated request fail the way ASource failed - a
    /// request of the authenticator's own that never got an answer
    procedure FailWith(AResponse, ASource: TRALResponse); virtual; abstract;
    function NewRequest: TRALRequest; virtual; abstract;
    function NewResponse: TRALResponse; virtual; abstract;
    /// Sends ARequest to AURL; returns the ErrorCode, zero when an HTTP answer
    /// came (whatever its status)
    function Send(const AURL: StringRAL; ARequest: TRALRequest;
      AResponse: TRALResponse; AMethod: TRALMethod): IntegerRAL; virtual; abstract;
    /// The URL of ARoute on the server being called; an absolute URL
    /// (http:// or https://, a provider on another host) is returned as is
    function URL(const ARoute: StringRAL): StringRAL; virtual; abstract;
  end;

  { TRALAuthClient }

  /// Base class of client components' Authenticator
  /// One authenticator is meant to be SHARED by several clients - Authentication
  /// takes a FreeNotification, never ownership - so whatever an authenticator
  /// keeps between requests is touched by every thread those clients run on.
  /// Lock/Unlock is that guard, and it lives here rather than on TRALClient
  /// because a per-client lock cannot serialise what the clients share.
  TRALAuthClient = class(TRALAuthentication)
  private
    FAutoGetToken: boolean;
    FCritAuth: TCriticalSection;
    FOnBeforeGetToken: TRALOnBeforeGetToken;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// The server answered 401 (AResponse, with its WWW-Authenticate) to
    /// ARequest, as it was sent. The authenticator drops or updates what it
    /// holds and says whether the request is worth sending once more. Called
    /// under Lock, only with AutoGetToken on
    function HandleChallenge(ARequest: TRALRequest; AResponse: TRALResponse): boolean; virtual;
    /// Whether the scheme has what it needs to authenticate a request - a
    /// token, a nonce. When it has not and AutoGetToken is on, Prepare runs
    function IsAuthenticated: boolean; virtual;
    /// Obtains what the scheme needs before a request (a token), through
    /// ATransport. AVars holds the method and url of the request. Returns 0,
    /// or the ErrorCode of a failure already written to AResponse. Called
    /// under Lock
    function Prepare(ATransport: TRALAuthTransport; AVars: TStringList;
      AResponse: TRALResponse): IntegerRAL; virtual;
    /// Writes the Authorization of the request (AParams) - every request
    procedure SetAuthHeader(AVars: TStringList; AParams: TRALParams); virtual; abstract;

    /// Serialises everything that reads or writes the authenticator's own state.
    /// Reentrant, like the critical section behind it: the client holds it
    /// around the whole token fetch, and the SetToken that ends the fetch takes
    /// it again on the same thread.
    procedure Lock;
    procedure Unlock;

    property OnBeforeGetToken: TRALOnBeforeGetToken read FOnBeforeGetToken
      write FOnBeforeGetToken;
  published
    /// Sets if client will attempt to automatically ask for token from request
    property AutoGetToken: boolean read FAutoGetToken write FAutoGetToken;
  end;

  /// Base class of server components' Authenticator

  { TRALAuthServer }

  /// Authentication is a plugin of the server: in the plugin loop it answers the
  /// route of its own (the JWT token route, offered in ppResolveRoute) and, for
  /// every other route, authenticates the methods the route does not skip
  /// (SkipAuthMethods) - with no authentication plugin, nothing is skipped
  /// because nothing is asked. The verdict goes through Host.Authenticate, which
  /// tells the ppAuthResult plugins (brute force) before any module runs.
  /// TRALServer.Authentication adds it to the server's plugins, and so does its
  /// own Server property - the way to link a second one (Basic and Digest
  /// together); the scheme-specific work stays in Validate and BeforeValidate
  TRALAuthServer = class(TRALPlugin)
  private
    FAuthType: TRALAuthTypes;
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
    function GetAuthRoute: TRALBaseRoute; virtual;
    procedure SetAuthRoute(ARoute : TRALBaseRoute); virtual;
    procedure SetAuthType(AType: TRALAuthTypes);
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeforeValidate(ARequest: TRALRequest; AResponse: TRALResponse); virtual;
    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
    /// Main method of authenticator, all validations must be done here
    procedure Validate(ARequest: TRALRequest; AResponse: TRALResponse); virtual; abstract;
    /// Credentials that did not come in an Authorization header - the JWT
    /// raltoken cookie. TRALServer.DecodeAuth calls it when the header is absent
    procedure DecodeWithoutHeader(ARequest: TRALRequest); virtual;

    { the plugin side of the same work }
    /// ppAuthenticate: Validate, read as a verdict
    function Authenticate(ARequest: TRALRequest; AResponse: TRALResponse;
      ARoute: TRALRoute): TRALAuthResult; override;
    /// ppProcess: answers the plugin's own route, and 401/403 for a request
    /// that does not pass. Only the first authenticator of the server asks for
    /// the verdict: Host.Authenticate already hears all of them
    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
    /// ppResolveRoute: the plugin's own route, when it is the one requested
    function ResolveRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute; override;
    /// What a request this scheme already answered (AResult) means to the
    /// brute-force protection - see TRALServerPlugin.AttemptOf. The base rule:
    /// credentials of this scheme that were checked count, refused (401) as a
    /// guess and accepted as proof; the scheme's own routes say nothing
    function AttemptOf(ARequest: TRALRequest; AResponse: TRALResponse;
      AResult: TRALAuthResult; AOnOwnRoute: boolean): TRALAuthAttempt; override;

    property AuthRoute: TRALBaseRoute read GetAuthRoute write SetAuthRoute;
    property AuthType: TRALAuthTypes read FAuthType;
  end;

/// Adds AChallenge to the WWW-Authenticate of AResponse: several authenticators
/// refusing the same request each offer theirs (RFC 9110 11.6.1), in one field
/// - the params of a response keep one value per name
procedure RALAddChallenge(AResponse: TRALResponse; const AChallenge: StringRAL);
/// AValue as printable ASCII for a challenge's error_description: accented
/// Latin-1 letters lose the accent, anything else non-ASCII becomes '?'
function RALChallengeText(const AValue: StringRAL): StringRAL;

implementation

{ RFC 6750 only lets error_description carry printable ASCII other than '"' and
  '\', and the text comes from the language files. Works on the UTF-8 bytes, so
  it behaves the same on every compiler: an accented Latin-1 letter (lead byte
  $C3) keeps its base letter, any other non-ASCII character becomes '?' }
function RALChallengeText(const AValue: StringRAL): StringRAL;
const
  cLatin1 = 'AAAAAAACEEEEIIIIDNOOOOOxOUUUUYTsaaaaaaaceeeeiiiidnooooo/ouuuuyty';
var
  vInt, vLast, vCode: integer;
  vChar: CharRAL;
begin
  Result := '';
  vInt := POSINISTR;
  vLast := RALHighStr(AValue);
  while vInt <= vLast do
  begin
    vCode := Ord(AValue[vInt]);
    Inc(vInt);
    if vCode >= $80 then
    begin
      if (vCode = $C3) and (vInt <= vLast) and (Ord(AValue[vInt]) in [$80..$BF]) then
        vChar := CharRAL(cLatin1[POSINISTR + Ord(AValue[vInt]) - $80])
      else
        vChar := '?';
      while (vInt <= vLast) and ((Ord(AValue[vInt]) and $C0) = $80) do
        Inc(vInt);
    end
    else if vCode = Ord('"') then
      vChar := ''''
    else if vCode = Ord('\') then
      vChar := '/'
    else if (vCode < $20) or (vCode = $7F) then
      vChar := ' '
    else
      vChar := CharRAL(vCode);
    Result := Result + vChar;
  end;
end;

procedure RALAddChallenge(AResponse: TRALResponse; const AChallenge: StringRAL);
var
  vParam: TRALParam;
begin
  vParam := AResponse.Params.GetKind['WWW-Authenticate', rpkHEADER];
  if (vParam <> nil) and (vParam.AsString <> '') then
    vParam.AsString := vParam.AsString + ', ' + AChallenge
  else
    AResponse.AddHeader('WWW-Authenticate', AChallenge);
end;

{ TRALAuthClient }

constructor TRALAuthClient.Create(AOwner: TComponent);
begin
  inherited;
  FAutoGetToken := True;
  FCritAuth := TCriticalSection.Create;
end;

destructor TRALAuthClient.Destroy;
begin
  FreeAndNil(FCritAuth);
  inherited;
end;

procedure TRALAuthClient.Lock;
begin
  FCritAuth.Acquire;
end;

procedure TRALAuthClient.Unlock;
begin
  FCritAuth.Release;
end;

function TRALAuthClient.HandleChallenge(ARequest: TRALRequest;
  AResponse: TRALResponse): boolean;
begin
  { what the client always did with a 401: send once more }
  Result := True;
end;

function TRALAuthClient.IsAuthenticated: boolean;
begin
  Result := False;
end;

function TRALAuthClient.Prepare(ATransport: TRALAuthTransport; AVars: TStringList;
  AResponse: TRALResponse): IntegerRAL;
begin
  Result := 0; // nothing to obtain
end;

{ TRALAuthentication }

constructor TRALAuthentication.Create(AOwner: TComponent);
begin
  inherited;
  FAuthType := ratNone;
end;

procedure TRALAuthentication.SetAuthType(AType: TRALAuthTypes);
begin
  FAuthType := AType;
end;

{ TRALAuthServer }

constructor TRALAuthServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAuthType := ratNone;
end;

class function TRALAuthServer.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityAuthentication;
end;

function TRALAuthServer.Phases: TRALPluginPhases;
begin
  Result := [ppProcess, ppResolveRoute, ppAuthenticate];
end;

procedure TRALAuthServer.SetAuthType(AType: TRALAuthTypes);
begin
  FAuthType := AType;
end;

procedure TRALAuthServer.BeforeValidate(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(HTTP_NotFound);
end;

function TRALAuthServer.CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse)
  : TRALRoute;
begin
  { Basic has no route of its own: GetAuthRoute is nil there, and asking it for
    its full route was an access violation on every request for a route that
    does not exist - a 500 where the answer is 404 }
  Result := TRALRoute(GetAuthRoute);
  if (Result <> nil) and not RALSameName(Result.GetFullRoute, ARequest.Query) then
    Result := nil;
end;

procedure TRALAuthServer.DecodeWithoutHeader(ARequest: TRALRequest);
begin
  // only the schemes that also travel outside the header override this
end;

function TRALAuthServer.ResolveRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
begin
  Result := CanAnswerRoute(ARequest, AResponse);
end;

procedure TRALAuthServer.ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
  var AHandled: boolean);
var
  vRoute: TRALRoute;
  vResult: TRALAuthResult;
begin
  vRoute := Host.FindRoute(ARequest, AResponse);
  if vRoute = nil then
    Exit;

  { the route this plugin offered is its own to answer: no module has it }
  if ARequest.RouteOwner = Self then
  begin
    if ARequest.Method <> amOPTIONS then
    begin
      BeforeValidate(ARequest, AResponse);
      { a token route checks a secret too - the JWT login, where OnGetToken
        reads the password. It used to count for nothing, so that one could be
        tried without limit }
      if AResponse.StatusCode < HTTP_BadRequest then
        vResult := arAccepted
      else if AResponse.StatusCode = HTTP_Unauthorized then
        vResult := arUnauthorized
      else
        vResult := arForbidden;
      Host.ReportAttempt(ARequest, AResponse,
        AttemptOf(ARequest, AResponse, vResult, True));
    end;
    AHandled := True;
    Exit;
  end;

  if Host.FindPlugin(TRALAuthServer) <> Self then
    Exit;

  { what the module answers without running the route - the preflight, 405 for
    a method the route does not take - is not authenticated, and neither is a
    method the route skips }
  if (ARequest.Method = amOPTIONS) or (not vRoute.IsMethodAllowed(ARequest.Method)) or
     vRoute.IsMethodSkipped(ARequest.Method) then
    Exit;

  case Host.Authenticate(ARequest, AResponse, vRoute) of
    arAccepted:
    begin
      { one authenticator accepted: what the others that refused left - the
        401 and their challenges - is not the answer }
      if AResponse.StatusCode >= HTTP_BadRequest then
        AResponse.StatusCode := HTTP_OK;
      AResponse.Params.DelParam('WWW-Authenticate', rpkHEADER);
    end;
    arUnauthorized:
    begin
      AResponse.Answer(HTTP_Unauthorized);
      AHandled := True;
    end;
    arForbidden:
    begin
      { the authenticator's own server error - a JWT with no key says so - is
        not a refusal of the client: it goes out as it is }
      if AResponse.StatusCode < HTTP_InternalError then
        AResponse.Answer(HTTP_Forbidden);
      AHandled := True;
    end;
  end;
end;

function TRALAuthServer.Authenticate(ARequest: TRALRequest; AResponse: TRALResponse;
  ARoute: TRALRoute): TRALAuthResult;
begin
  { Validate answers by the status it leaves, which is what the server read
    before this was a plugin: below 400 lets the route run, 401 asks for
    credentials, anything else refuses them }
  Validate(ARequest, AResponse);
  if AResponse.StatusCode < HTTP_BadRequest then
    Result := arAccepted
  else if AResponse.StatusCode = HTTP_Unauthorized then
    Result := arUnauthorized
  else
    Result := arForbidden;
end;

function TRALAuthServer.AttemptOf(ARequest: TRALRequest; AResponse: TRALResponse;
  AResult: TRALAuthResult; AOnOwnRoute: boolean): TRALAuthAttempt;
begin
  { credentials of this scheme came and were checked: refused, a guess;
    accepted, proof. 401 only - a 403 is a refusal of someone already known.
    A route of the scheme's own says nothing unless the scheme says so }
  Result := raaNone;
  if AOnOwnRoute or (ARequest.Authorization.AuthType <> AuthType) then
    Exit;
  if AResult = arAccepted then
    Result := raaPassed
  else if AResult = arUnauthorized then
    Result := raaFailed;
end;

function TRALAuthServer.GetAuthRoute: TRALBaseRoute;
begin
  Result := nil;
end;

procedure TRALAuthServer.SetAuthRoute(ARoute: TRALBaseRoute);
begin
  // not implmented
end;

end.
