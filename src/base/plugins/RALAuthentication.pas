/// Base unit for all authenticators: the client contract, the server plugin,
/// and Basic and JWT. Digest is in RALDigest and OAuth2 in RALOAuth2
unit RALAuthentication;

interface

uses
  Classes, SysUtils, DateUtils, SyncObjs,
  RALToken, RALConsts, RALTypes, RALRoutes, RALBase64, RALTools, RALJson,
  RALRequest, RALParams, RALResponse, RALCustomObjects, RALMIMETypes, RALPlugin;

const
  RALTOKENName = 'raltoken';
  RALPAYLOADName = 'ral_payload';


type
  TRALOnValidate = procedure(ARequest: TRALRequest; AResponse: TRALResponse;
                             var AResult: boolean) of object;
  { of object, like every other callback declared around it. OnGetToken decides
    who gets a token and OnValidate says whether one still counts, so both need
    to reach a database - that is, the object that owns the connection. Without
    of object they can only be plain procedures, which have no Self and get at
    their state through globals.

    This was the only exception in the group: TRALOnValidate,
    TRALOnBeforeGetToken, TRALOnResolve and TRALOnGetTokenSecret are all of
    object. And TRALServerBasicAuth.OnValidate already took the of-object form
    while TRALServerJWTAuth.OnValidate - same property name, same unit - did
    not. Assigning a plain procedure to these two no longer compiles. }
  TRALOnTokenJWT = procedure(ARequest: TRALRequest; AResponse: TRALResponse;
                             AParams: TRALJWTParams; var AResult: boolean) of object;
  TRALOnTokenJWTGen = procedure(ARequest: TRALRequest; AResponse: TRALResponse;
                             AParams: TRALJWTParams; var AResult: boolean);
  TRALOnBeforeGetToken = procedure(ARequest: TRALRequest) of object;
  TRALOnResolve = procedure(AToken: StringRAL; AParams: TRALJWTParams;
                            var AResult: StringRAL) of object;

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
    /// The server answered 401 (AResponse, with its WWW-Authenticate). The
    /// authenticator drops or updates what it holds and says whether the
    /// request is worth sending once more. Called under Lock, only with
    /// AutoGetToken on
    function HandleChallenge(AResponse: TRALResponse): boolean; virtual;
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

    property AuthRoute: TRALBaseRoute read GetAuthRoute write SetAuthRoute;
    property AuthType: TRALAuthTypes read FAuthType;
  end;

  /// BasicAuth for client components
  TRALClientBasicAuth = class(TRALAuthClient)
  private
    FUserName: StringRAL;
    FPassword: StringRAL;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AUser: StringRAL;
                       const APassword: StringRAL); overload;
    function IsAuthenticated: boolean; override;
    procedure SetAuthHeader(AVars: TStringList; AParams: TRALParams); override;
  published
    property UserName: StringRAL read FUserName write FUserName;
    property Password: StringRAL read FPassword write FPassword;
  end;

  /// BasicAuth for Server components
  TRALServerBasicAuth = class(TRALAuthServer)
  private
    FAuthDialog: boolean;
    FPassword: StringRAL;
    FUserName: StringRAL;
    FOnValidate: TRALOnValidate;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AUser: StringRAL;
                       const APassword: StringRAL); overload;
    /// Validation process of the authentication is made here
    procedure Validate(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    property AuthDialog: boolean read FAuthDialog write FAuthDialog;
    property Password: StringRAL read FPassword write FPassword;
    property UserName: StringRAL read FUserName write FUserName;
    property OnValidate: TRALOnValidate read FOnValidate write FOnValidate;
  end;

  /// JWT Authenticator for Client components
  TRALClientJWTAuth = class(TRALAuthClient)
  private
    FJSONKey: StringRAL;
    FPayload: TRALJWTParams;
    FRoute: StringRAL;
    FToken: StringRAL;
  protected
    procedure SetRoute(const AValue: StringRAL);
    procedure SetToken(const AValue: StringRAL);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// A 401: the token is dropped, and the request goes again with a new one
    function HandleChallenge(AResponse: TRALResponse): boolean; override;
    function IsAuthenticated: boolean; override;
    /// Posts to Route for a token (OnBeforeGetToken, or Payload as the body)
    function Prepare(ATransport: TRALAuthTransport; AVars: TStringList;
      AResponse: TRALResponse): IntegerRAL; override;
    procedure SetAuthHeader(AVars: TStringList; AParams: TRALParams); override;

    /// Reads one claim of the token currently held, under the lock.
    /// This is the thread-safe way in: the raw Payload below is an object this
    /// class rewrites whenever a token arrives, so reading it while another
    /// thread refreshes the token walks its lists mid-swap.
    function GetClaim(const AKey: StringRAL): StringRAL;
  published
    property JSONKey: StringRAL read FJSONKey write FJSONKey;
    /// The decoded claims of the current token. Held between requests and
    /// REPLACED on every SetToken, so anything reading it from another thread
    /// has to hold Lock for as long as it uses what it read - or call GetClaim,
    /// which does that for a single claim.
    property Payload: TRALJWTParams read FPayload write FPayload;
    property Route: StringRAL read FRoute write SetRoute;
    property Token: StringRAL read FToken write SetToken;
    property OnBeforeGetToken;
  end;

  { TRALServerJWTAuth }

  /// JWT Authenticator for server components
  TRALServerJWTAuth = class(TRALAuthServer)
  private
    FAlgorithm: TRALJWTAlgorithm;
    FCollectionAuth: TOwnedCollection;
    FAuthToken: TRALBaseRoute;
    FExpSecs: IntegerRAL;
    FJSONKey: StringRAL;
    FSignSecretKey: StringRAL;
    FOnGetToken: TRALOnTokenJWT;
    FOnGetTokenGen: TRALOnTokenJWTGen;
    FOnRenewToken: TRALOnTokenJWT;
    FOnRenewTokenGen: TRALOnTokenJWTGen;
    FOnValidate: TRALOnTokenJWT;
    FOnValidateGen: TRALOnTokenJWTGen;
    FUseCookie: Boolean;
    { the work of RenewToken; with a request, OnRenewToken is consulted }
    function RenewTokenFor(ARequest: TRALRequest; AResponse: TRALResponse;
      const AToken: StringRAL; var AJSONParams: StringRAL): StringRAL;
    procedure SetUseCookie(AValue: Boolean);
    { RFC 6750 3: tells a client WHY it got the 401 - no credentials at all
      (AError empty) or a token that was refused, and what was wrong with it }
    procedure AnswerChallenge(AResponse: TRALResponse; const AError,
      ADescription: StringRAL);
  protected
    function GetAuthRoute: TRALBaseRoute; override;
    procedure SetAuthRoute(ARoute: TRALBaseRoute); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeforeValidate(ARequest: TRALRequest; AResponse: TRALResponse); override;
    /// The raltoken cookie, when no Authorization header came
    procedure DecodeWithoutHeader(ARequest: TRALRequest); override;
    function GetToken(var AJSONParams: StringRAL): StringRAL;
    /// A new token with the claims of AToken and a new expiration; '' when
    /// AToken is not valid. Called directly it does not fire OnRenewToken
    function RenewToken(const AToken: StringRAL; var AJSONParams: StringRAL): StringRAL;
    /// Validation process of the authentication is made here
    procedure Validate(ARequest: TRALRequest; AResponse: TRALResponse); override;
    property OnGetTokenGen: TRALOnTokenJWTGen read FOnGetTokenGen write FOnGetTokenGen;
    /// OnRenewToken for a plain procedure
    property OnRenewTokenGen: TRALOnTokenJWTGen read FOnRenewTokenGen
      write FOnRenewTokenGen;
    property OnValidateGen: TRALOnTokenJWTGen read FOnValidateGen write FOnValidateGen;
  published
    property Algorithm: TRALJWTAlgorithm read FAlgorithm write FAlgorithm;
    property AuthRoute;
    property ExpirationSecs: IntegerRAL read FExpSecs write FExpSecs;
    property JSONKey: StringRAL read FJSONKey write FJSONKey;
    property SignSecretKey: StringRAL read FSignSecretKey write FSignSecretKey;
    property UseCookie: Boolean read FUseCookie write SetUseCookie;

    property OnGetToken: TRALOnTokenJWT read FOnGetToken write FOnGetToken;
    /// Fired when a valid token is posted to AuthRoute to be renewed, with its
    /// claims in AParams - already checked, and with the new expiration set.
    /// Change any claim there (permissions read again from the database, say),
    /// or set AResult to False to refuse the renewal (the client gets 401 and
    /// has to ask for a first token again). Unassigned, the claims are copied
    /// as they were, which is what renewing always did
    property OnRenewToken: TRALOnTokenJWT read FOnRenewToken write FOnRenewToken;
    property OnValidate: TRALOnTokenJWT read FOnValidate write FOnValidate;
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

function TRALAuthClient.HandleChallenge(AResponse: TRALResponse): boolean;
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

{ TRALServerJWTAuth }

procedure TRALServerJWTAuth.BeforeValidate(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vToken: StringRAL;
  vStrParams: StringRAL;
  vStrResult: StringRAL;
  vResult: boolean;
  vParam: TRALParam;
  vParamJWT: TRALJWTParams;
  vCookie: TRALCookie;
begin
  if RALSameName(ARequest.Query, AuthRoute.Route) then
  begin
    vResult := False;
    vToken := '';
    vStrParams := '';
    if (ARequest.Authorization.AuthString <> '') and
      (ARequest.Authorization.AuthType = ratBearer) then
    begin
      { a valid token in hand renews itself: same claims, new expiration.
        The signature already proves who is asking, so OnGetToken is not
        consulted here - it decides who gets a FIRST token }
      vToken := RenewTokenFor(ARequest, AResponse, ARequest.Authorization.AuthString,
        vStrParams);
      vResult := vToken <> '';
    end
    else if Assigned(FOnGetToken) then
    begin
      vParamJWT := TRALJWTParams.Create;
      try
        FOnGetToken(ARequest, AResponse, vParamJWT, vResult);
        if vResult then
        begin
          vStrParams := vParamJWT.AsJSON;
          vToken := GetToken(vStrParams);
        end;
      finally
        FreeAndNil(vParamJWT);
      end;
    end
    else if Assigned(FOnGetTokenGen) then
    begin
      vParamJWT := TRALJWTParams.Create;
      try
        FOnGetTokenGen(ARequest, AResponse, vParamJWT, vResult);
        if vResult then
        begin
          vStrParams := vParamJWT.AsJSON;
          vToken := GetToken(vStrParams);
        end;
      finally
        FreeAndNil(vParamJWT);
      end;
    end
    else
    begin
      { Without OnGetToken nobody checks who is asking: any client could post
        any claims and walk away with a signed token, which made JWT the same
        as no authentication. Issuing a first token needs the event. }
      AResponse.Answer(HTTP_Unauthorized);
    end;

    if vResult then
    begin
      vStrResult := Format('{"%s":"%s"}', [FJSONKey, vToken]);
      if UseCookie then
      begin
        Finalize(vCookie); // strings inside: never FillChar over live references
        FillChar(vCookie, SizeOf(vCookie), 0);
        vCookie.Name := RALTOKENName;
        vCookie.Value := vToken;
        vCookie.Secure := true;
        vCookie.HttpOnly := true;
        vCookie.Path := '/';
        vParamJWT := TRALJWTParams.Create;
        try
          vParamJWT.AsJSON := vStrParams;
          vCookie.Expires := vParamJWT.Expiration;
        finally
          FreeAndNIl(vParamJWT);
        end;
        AResponse.AddCookie(vCookie);
      end;
      AResponse.StatusCode := HTTP_OK;
      AResponse.ContentType := rctAPPLICATIONJSON;
      AResponse.ResponseText := vStrResult;
    end
    else
    begin
      if AResponse.StatusCode < HTTP_BadRequest then
        AResponse.Answer(HTTP_Unauthorized);
    end;
  end
  else
  begin
    AResponse.Answer(HTTP_NotFound);
  end;
end;

constructor TRALServerJWTAuth.Create(AOwner: TComponent);
begin
  inherited;
  SetAuthType(ratBearer);
  UseCookie := False;

  FCollectionAuth := TOwnedCollection.Create(Self, TRALBaseRoute);

  FAuthToken := TRALBaseRoute(FCollectionAuth.Add);
  FAuthToken.Route := '/gettoken';
  FAuthToken.Name := 'gettoken';
  FAuthToken.SkipAuthMethods := [amALL];
  FAuthToken.AllowedMethods := [amPOST, amOPTIONS];
  FAuthToken.Description.Text := 'Get a JWT Token';

  FExpSecs := 1800;
  FJSONKey := 'token';
end;

destructor TRALServerJWTAuth.Destroy;
begin
  FreeAndNil(FCollectionAuth);
  inherited;
end;

function TRALServerJWTAuth.RenewToken(const AToken: StringRAL; var AJSONParams: StringRAL)
  : StringRAL;
begin
  Result := RenewTokenFor(nil, nil, AToken, AJSONParams);
end;

function TRALServerJWTAuth.RenewTokenFor(ARequest: TRALRequest;
  AResponse: TRALResponse; const AToken: StringRAL;
  var AJSONParams: StringRAL): StringRAL;
var
  vJWT: TRALJWT;
  vResult: boolean;
begin
  Result := '';
  vJWT := TRALJWT.Create;
  try
    vJWT.Header.Algorithm := FAlgorithm;
    vJWT.SignSecretKey := FSignSecretKey;
    if vJWT.isValidToken(AToken) then
    begin
      { the claims are the ones already signed in the old token, never the
        ones the client sends along: a renew must not be a rewrite }

      { guarded like GetToken does. ExpirationSecs = 0 means "the payload owns
        the expiration" - an application that sets its own exp in OnGetToken,
        because the token has to die with something else (a licence, a session,
        a shift), leaves this at zero. Unguarded, a renew wrote
        IncSecond(Now, 0) = Now and handed back a token already expired. }
      if FExpSecs > 0 then
        vJWT.Payload.Expiration := IncSecond(Now, FExpSecs);

      { renewing only ever copied the claims, so anything the application
        derives from its database at login (permissions, a store, a role)
        stayed as it was for as long as the client kept renewing }
      if ARequest <> nil then
      begin
        vResult := True;
        if Assigned(FOnRenewToken) then
          FOnRenewToken(ARequest, AResponse, vJWT.Payload, vResult)
        else if Assigned(FOnRenewTokenGen) then
          FOnRenewTokenGen(ARequest, AResponse, vJWT.Payload, vResult);
        if not vResult then
          Exit;
      end;

      AJSONParams := vJWT.Payload.AsJSON;

      Result := vJWT.Token;
    end;
  finally
    vJWT.Free;
  end;
end;

procedure TRALServerJWTAuth.AnswerChallenge(AResponse: TRALResponse;
  const AError, ADescription: StringRAL);
var
  vValue: StringRAL;
begin
  vValue := 'Bearer realm="RAL"';
  if AError <> '' then
    vValue := vValue + ', error="' + AError + '"';
  if ADescription <> '' then
    vValue := vValue + ', error_description="' + RALChallengeText(ADescription) + '"';
  RALAddChallenge(AResponse, vValue);
end;

procedure TRALServerJWTAuth.Validate(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vResult: boolean;
  vJWT: TRALJWT;
  vReason: StringRAL;
begin
  AResponse.StatusCode := HTTP_OK;
  { the same 401 page answered "no token at all", "expired" and "forged", and a
    client that lost its cookie on the way looked exactly like one holding a
    bad token. The page stays; the WWW-Authenticate header says which }
  if (ARequest.Authorization.AuthType <> ratBearer) then
  begin
    AResponse.Answer(HTTP_Unauthorized);
    AnswerChallenge(AResponse, '', '');
    Exit;
  end;

  vResult := False;
  vReason := '';

  vJWT := TRALJWT.Create;
  try
    vJWT.Header.Algorithm := FAlgorithm;
    vJWT.SignSecretKey := FSignSecretKey;
    vResult := vJWT.isValidToken(ARequest.Authorization.AuthString);
    if vResult then
    begin
      if Assigned(FOnValidate) then
        FOnValidate(ARequest, AResponse, vJWT.Payload, vResult)
      else if Assigned(FOnValidateGen) then
        FOnValidateGen(ARequest, AResponse, vJWT.Payload, vResult);
      if not vResult then
        vReason := wmJWTRefused;
    end
    { IsValidToken read the payload before deciding, so it still tells the
      two cases a client can do something about }
    else if (vJWT.Payload.Expiration > 0) and (vJWT.Payload.Expiration < Now) then
      vReason := wmJWTExpired
    else if (vJWT.Payload.NotBefore > 0) and (vJWT.Payload.NotBefore > Now) then
      vReason := wmJWTNotYetValid
    else
      vReason := wmJWTInvalid;
  finally
    FreeAndNil(vJWT);
  end;

  if (not vResult) and (AResponse.StatusCode < HTTP_BadRequest) then
  begin
    AResponse.Answer(HTTP_Unauthorized);
    AnswerChallenge(AResponse, 'invalid_token', vReason);
  end;
end;

procedure TRALServerJWTAuth.DecodeWithoutHeader(ARequest: TRALRequest);
var
  vStr, vAux, vPart: StringRAL;
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  { the Cookie header carries every cookie the browser has for the site,
    in whatever order; only the one named raltoken is the bearer. This
    used to take the first cookie, whatever its name, and any site cookie
    ahead of the token made a logged-in browser fail with 401.
    Every engine splits the cookies into rpkCOOKIE params, so the param
    named raltoken is the first place to look; the raw header is the
    fallback for an engine that kept it whole }
  vAux := '';
  vParam := ARequest.Params.GetKind[RALTOKENName, rpkCOOKIE];
  if not vParam.IsNilOrEmpty then
    vAux := Trim(vParam.AsString);
  vStr := '';
  if vAux = '' then
    vStr := ARequest.ParamByName('Cookie').AsString;
  { one "name=value" per "; " - the name has to be exactly raltoken, so a
    cookie called "xraltoken" does not match either }
  while (vStr <> '') and (vAux = '') do
  begin
    vInt := Pos(StringRAL(';'), vStr);
    if vInt > 0 then
    begin
      vPart := Trim(Copy(vStr, 1, vInt - 1));
      vStr := Copy(vStr, vInt + 1, Length(vStr));
    end
    else
    begin
      vPart := Trim(vStr);
      vStr := '';
    end;
    if Pos(StringRAL(RALTOKENName + '='), vPart) = 1 then
      vAux := Trim(Copy(vPart, Length(RALTOKENName) + 2, Length(vPart)));
  end;
  if vAux <> '' then
  begin
    ARequest.Authorization.AuthType := ratBearer;
    ARequest.Authorization.AuthString := vAux;
  end;
end;

procedure TRALServerJWTAuth.SetUseCookie(AValue: Boolean);
begin
  if FUseCookie = AValue then Exit;
  FUseCookie := AValue;
end;

function TRALServerJWTAuth.GetAuthRoute: TRALBaseRoute;
begin
  Result := FAuthToken;
end;

procedure TRALServerJWTAuth.SetAuthRoute(ARoute: TRALBaseRoute);
begin
  FAuthToken.Assign(ARoute);
end;

function TRALServerJWTAuth.GetToken(var AJSONParams: StringRAL): StringRAL;
var
  vJWT: TRALJWT;
begin
  vJWT := TRALJWT.Create;
  try
    vJWT.Header.Algorithm := FAlgorithm;
    vJWT.SignSecretKey := FSignSecretKey;

    vJWT.Payload.AsJSON := AJSONParams;
    if FExpSecs > 0 then
      vJWT.Payload.Expiration := IncSecond(Now, FExpSecs);

    AJSONParams := vJWT.Payload.AsJSON;

    Result := vJWT.Token;
  finally
    vJWT.Free;
  end;
end;

{ TRALServerBasicAuth }

constructor TRALServerBasicAuth.Create(AOwner: TComponent);
begin
  inherited;
  FAuthDialog := True;
  SetAuthType(ratBasic);
end;

constructor TRALServerBasicAuth.Create(AOwner: TComponent;
  const AUser, APassword: StringRAL);
begin
  Create(AOwner);
  UserName := AUser;
  Password := APassword;
end;

procedure TRALServerBasicAuth.Validate(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vResult: boolean;
  vAuthBasic : TRALAuthBasic;

  procedure Error401;
  begin
    if AResponse.StatusCode < HTTP_BadRequest then
      AResponse.Answer(HTTP_Unauthorized);
    if FAuthDialog then
      RALAddChallenge(AResponse, 'Basic realm="RAL Basic"');
  end;

begin
  AResponse.StatusCode := HTTP_OK;
  if (ARequest.Authorization.AuthType <> ratBasic) then
  begin
    Error401;
    Exit;
  end;

  if Assigned(FOnValidate) then
  begin
    vResult := False;
    FOnValidate(ARequest, AResponse, vResult);
    if not vResult then
      Error401;
  end
  else
  begin
    vAuthBasic := ARequest.Authorization.AsAuthBasic;
    { both compared in constant time: "<>" stops at the first wrong character,
      and the time it takes to refuse leaks how much of the secret is right }
    if (ARequest.Authorization.AuthType <> ratBasic) or (vAuthBasic = nil) or
       (Trim(FUserName) = '') or (Trim(FPassword) = '') or
       (not RALSameSecret(vAuthBasic.UserName, FUserName)) or
       (not RALSameSecret(vAuthBasic.Password, FPassword)) then
      Error401;
  end;
end;

{ TRALClientBasicAuth }

constructor TRALClientBasicAuth.Create(AOwner: TComponent);
begin
  inherited;
  SetAuthType(ratBasic);
end;

constructor TRALClientBasicAuth.Create(AOwner: TComponent;
  const AUser, APassword: StringRAL);
begin
  Create(AOwner);
  Password := APassword;
  UserName := AUser;
end;

function TRALClientBasicAuth.IsAuthenticated: boolean;
begin
  Result := True;
end;

procedure TRALClientBasicAuth.SetAuthHeader(AVars: TStringList; AParams: TRALParams);
var
  vBase64: StringRAL;
begin
  vBase64 := TRALBase64.Encode(FUserName + ':' + FPassword);
  AParams.AddParam('Authorization', 'Basic ' + vBase64, rpkHEADER);
end;

{ TRALClientJWTAuth }

constructor TRALClientJWTAuth.Create(AOwner: TComponent);
begin
  inherited;
  FJSONKey := 'token';
  FToken := '';
  FPayload := TRALJWTParams.Create;
  FRoute := '/gettoken/';
end;

destructor TRALClientJWTAuth.Destroy;
begin
  FreeAndNil(FPayload);
  inherited;
end;

function TRALClientJWTAuth.HandleChallenge(AResponse: TRALResponse): boolean;
begin
  { the token was refused or expired: a new one is fetched before the request
    goes again - what ResetToken always meant to do }
  Token := '';
  Result := True;
end;

function TRALClientJWTAuth.Prepare(ATransport: TRALAuthTransport; AVars: TStringList;
  AResponse: TRALResponse): IntegerRAL;
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vStatus, vConta: IntegerRAL;
  vJson: TRALJSONObject;
  vValue: TRALJSONValue;
  vParam: TRALParam;
begin
  Result := 0; // no http error code
  if IsAuthenticated then
    Exit;

  vConta := 0;
  vStatus := 0;
  repeat
    vResponse := ATransport.NewResponse;
    vRequest := ATransport.NewRequest;
    try
      if Assigned(OnBeforeGetToken) then
      begin
        OnBeforeGetToken(vRequest);
      end
      else
      begin
        // rpkBODY is not optional here: AddValue defaults the kind to
        // rpkNONE, and EncodeBody only ever picks rpkBODY/rpkFIELD, so the
        // payload was built and then dropped - the token request went out
        // with Content-Length 0 and the server issued a token carrying no
        // claims at all. Every other AddValue caller already says rpkBODY.
        vParam := vRequest.Params.AddValue(FPayload.AsJSON, rpkBODY);
        vParam.ContentType := rctAPPLICATIONJSON;
      end;

      Result := ATransport.Send(ATransport.URL(FRoute), vRequest, vResponse, amPOST);
      vStatus := vResponse.StatusCode;

      if (Result = 0) and (vStatus = HTTP_OK) and (not vResponse.Body.IsNilOrEmpty) then
      begin
        vJson := TRALJSONObject(TRALJSON.ParseJSON(vResponse.Body.AsString));
        try
          if vJson <> nil then
          begin
            vValue := vJson.Get(FJSONKey);
            if vValue <> nil then
              Token := vValue.AsString;
          end;
        finally
          vJson.Free;
        end;
      end;
    finally
      { the reason a token request failed goes to the response the caller
        owns: an exception with an empty message was all the application had
        to show the user }
      if Result <> 0 then
        ATransport.FailWith(AResponse, vResponse);

      FreeAndNil(vRequest);
      FreeAndNil(vResponse);
    end;
    vConta := vConta + 1;
    { Result <> 0, not Result > 0: an engine that cannot number the failure
      reports -1 (OkHttp does), and the loop then burned every attempt on a
      server already known to be unreachable }
  until ((vStatus = HTTP_Unauthorized) and (vConta > 1)) or (vStatus = HTTP_OK) or
        (vConta >= RALMAXTOKENTRIES) or (Result <> 0);
end;

function TRALClientJWTAuth.IsAuthenticated: boolean;
begin
  Lock;
  try
    Result := FToken <> '';
  finally
    Unlock;
  end;
end;

function TRALClientJWTAuth.GetClaim(const AKey: StringRAL): StringRAL;
begin
  Lock;
  try
    Result := FPayload.GetClaim(AKey);
  finally
    Unlock;
  end;
end;

procedure TRALClientJWTAuth.SetAuthHeader(AVars: TStringList; AParams: TRALParams);
var
  vToken: StringRAL;
begin
  { copied out under the lock and used from the copy: reading FToken twice - once
    to test it, once to build the header - could otherwise pick up two different
    values and send half a token }
  Lock;
  try
    vToken := FToken;
  finally
    Unlock;
  end;

  if vToken <> '' then
    AParams.AddParam('Authorization', 'Bearer ' + vToken, rpkHEADER);
end;

procedure TRALClientJWTAuth.SetRoute(const AValue: StringRAL);
begin
  FRoute := FixRoute(AValue);
  if FRoute = '/' then
    FRoute := '/gettoken/';
end;

procedure TRALClientJWTAuth.SetToken(const AValue: StringRAL);
var
  vStr: TStringList;
  vInt: IntegerRAL;
  vValue, vPayload: StringRAL;
  vOk: boolean;
begin
  { Not an assignment: this splits the token, decodes a segment and rewrites
    FPayload, which is an OBJECT. Several clients share one authenticator, so
    several threads reach here at once whenever a token expires - each 401
    resets it and asks for another. Unguarded, that is a data race on a
    refcounted string AND on FPayload's own lists.

    Two rules here, and the second is not cosmetic:
    - it all happens under Lock;
    - nothing is published until it is known good. Clearing FToken first, the
      way this used to, made every OTHER thread read IsAuthenticated = False
      during the decode and go fetch a token of its own - so a single expiry
      turned into one /gettoken per client. }
  vValue := AValue;
  vPayload := '';
  vOk := False;

  vStr := TStringList.Create;
  try
    repeat
      vInt := Pos('.', vValue);
      if (vInt = 0) and (vValue <> '') then
        vInt := Length(vValue) + 1;

      if vInt > 0 then
      begin
        vStr.Add(Copy(vValue, 1, vInt - 1));
        Delete(vValue, 1, vInt);
      end;
    until vInt = 0;

    if vStr.Count = 3 then
    begin
      { the segments are base64url (RFC 7515): "-" and "_" instead of "+"
        and "/", no padding. Decoding them as plain base64 left any claim
        whose bytes hit those two characters unreadable on the client }
      vPayload := TRALBase64.Decode(TRALBase64.FromBase64Url(vStr.Strings[1]));
      vOk := True;
    end;
  finally
    vStr.Free;
  end;

  Lock;
  try
    if vOk then
    begin
      FPayload.AsJSON := vPayload;
      FToken := AValue;
    end
    else
    begin
      { a token that does not parse leaves the client unauthenticated, exactly
        as before - and FPayload keeps whatever it had, also as before }
      FToken := '';
    end;
  finally
    Unlock;
  end;
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
begin
  vRoute := Host.FindRoute(ARequest, AResponse);
  if vRoute = nil then
    Exit;

  { the route this plugin offered is its own to answer: no module has it }
  if ARequest.RouteOwner = Self then
  begin
    if ARequest.Method <> amOPTIONS then
      BeforeValidate(ARequest, AResponse);
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

function TRALAuthServer.GetAuthRoute: TRALBaseRoute;
begin
  Result := nil;
end;

procedure TRALAuthServer.SetAuthRoute(ARoute: TRALBaseRoute);
begin
  // not implmented
end;

end.
