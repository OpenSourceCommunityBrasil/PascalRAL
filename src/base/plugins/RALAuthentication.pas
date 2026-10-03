/// Base unit for all authenticators
unit RALAuthentication;

interface

uses
  Classes, SysUtils, DateUtils, SyncObjs,
  RALToken, RALConsts, RALTypes, RALRoutes, RALBase64, RALTools, RALJson,
  RALRequest, RALParams, RALResponse, RALCustomObjects, RALUrlCoder,
  RALMIMETypes;

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
  TRALOnGetTokenSecret = procedure(ATokenAccess: StringRAL; var ATokenSecret: StringRAL)
    of object;

  /// What one request means to the brute-force protection - see
  /// TRALAuthServer.AttemptOf: a wrong secret (counted against the address),
  /// a right one (the count starts over) or neither
  TRALAuthAttempt = (raaNone, raaFailed, raaPassed);

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
    function IsAuthenticated: boolean; virtual;
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

  TRALAuthServer = class(TRALAuthentication)
  protected
    function GetAuthRoute: TRALBaseRoute; virtual;
    procedure SetAuthRoute(ARoute : TRALBaseRoute); virtual;
  public
    procedure BeforeValidate(ARequest: TRALRequest; AResponse: TRALResponse); virtual;
    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
    /// Main method of authenticator, all validations must be done here
    procedure Validate(ARequest: TRALRequest; AResponse: TRALResponse); virtual; abstract;
    /// What a request this scheme already answered means to the brute-force
    /// protection. AOnAuthRoute is True for the scheme's own routes - the JWT
    /// token route. Only a secret that was checked counts: no credentials at
    /// all is not a guess, and nobody guesses an expired token.
    function AttemptOf(ARequest: TRALRequest; AResponse: TRALResponse;
                       AOnAuthRoute: boolean): TRALAuthAttempt; virtual;

    property AuthRoute: TRALBaseRoute read GetAuthRoute write SetAuthRoute;
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
    function IsAuthenticated: boolean; override;
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
    { OnGetToken or OnGetTokenGen is assigned }
    function CanIssue: boolean;
    { the token route logs in (OnGetToken) rather than renews - see BeforeValidate }
    function LogsIn(ARequest: TRALRequest): boolean;
    { a first token from OnGetToken; '' when it refused }
    function IssueToken(ARequest: TRALRequest; AResponse: TRALResponse;
      var AJSONParams: StringRAL): StringRAL;
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
    function GetToken(var AJSONParams: StringRAL): StringRAL;
    /// A new token with the claims of AToken and a new expiration; '' when
    /// AToken is not valid. Called directly it does not fire OnRenewToken
    function RenewToken(const AToken: StringRAL; var AJSONParams: StringRAL): StringRAL;
    /// Validation process of the authentication is made here
    procedure Validate(ARequest: TRALRequest; AResponse: TRALResponse); override;
    function AttemptOf(ARequest: TRALRequest; AResponse: TRALResponse;
                       AOnAuthRoute: boolean): TRALAuthAttempt; override;
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

  { TRALClientOAuth }

  /// OAuth Authenticator for client components
  TRALClientOAuth = class(TRALAuthClient)
  private
    FAlgorithm: TRALOAuthAlgorithm;
    FCallBack: StringRAL;
    FConsumerKey: StringRAL;
    FConsumerSecret: StringRAL;
    FNonce: StringRAL;
    FRouteAuthorize: StringRAL;
    FRouteInitialize: StringRAL;
    FTokenAccess: StringRAL;
    FTokenSecret: StringRAL;
    FVerifier: StringRAL;
  public
    constructor Create(AOwner: TComponent); override;
    function IsAuthenticated: boolean; override;
    procedure SetAuthHeader(AVars: TStringList; AParams: TRALParams); override;
  published
    property Algorithm: TRALOAuthAlgorithm read FAlgorithm write FAlgorithm;
    property CallBack: StringRAL read FCallBack write FCallBack;
    property ConsumerKey: StringRAL read FConsumerKey write FConsumerKey;
    property ConsumerSecret: StringRAL read FConsumerSecret write FConsumerSecret;
    property Nonce: StringRAL read FNonce write FNonce;
    property RouteAuthorize: StringRAL read FRouteAuthorize write FRouteAuthorize;
    property RouteInitialize: StringRAL read FRouteInitialize write FRouteInitialize;
    property TokenAccess: StringRAL read FTokenAccess write FTokenAccess;
    property TokenSecret: StringRAL read FTokenSecret write FTokenSecret;
    property Verifier: StringRAL read FVerifier write FVerifier;
  end;

  { TRALServerOAuth }

  /// OAuth Authenticator for server components
  TRALServerOAuth = class(TRALAuthServer)
  private
    FAlgorithm: TRALOAuthAlgorithm;
    FConsumerKey: StringRAL;
    FConsumerSecret: StringRAL;
    FRouteAuthorize: StringRAL;
    FRouteInitialize: StringRAL;
    FOnGetTokenSecret: TRALOnGetTokenSecret;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeforeValidate(ARequest: TRALRequest; AResponse: TRALResponse); override;
    /// Validation process of the authentication is made here
    procedure Validate(ARequest: TRALRequest; AResponse: TRALResponse); override;
  end;

  /// OAuth Authenticator for client components
  TRALClientOAuth2 = class(TRALAuthClient)
  private

  public

  end;

  /// OAuth2 Authenticator for server components
  TRALServerOAuth2 = class(TRALAuthServer)
  private

  public

  end;

  { TRALClientDigest }

  /// Digest Authenticator for Client components
  TRALClientDigest = class(TRALAuthClient)
  private
    FDigestParams: TRALDigestParams;
    FPassword: StringRAL;
    FUserName: StringRAL;
  protected
    function GetEntityBody(AParams: TRALParams): StringRAL;
  public
    function IsAuthenticated: boolean; override;
    property DigestParams: TRALDigestParams read FDigestParams write FDigestParams;
    procedure SetAuthHeader(AVars: TStringList; AParams: TRALParams); override;
  published
    property Password: StringRAL read FPassword write FPassword;
    property UserName: StringRAL read FUserName write FUserName;
  end;

  /// Digest Authenticator for Server components
  TRALServerDigest = class(TRALAuthServer)
  private

  public

  end;

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

function TRALAuthClient.IsAuthenticated: boolean;
begin
  Result := False;
end;

{ TRALClientDigest }

function TRALClientDigest.GetEntityBody(AParams: TRALParams): StringRAL;
var
  vStream: TStream;
  vFreeContent: boolean;
  vContentType, vContentDisposition: StringRAL;
begin
  Result := '';
  vFreeContent := False;
  vStream := AParams.EncodeBody(vContentType, vContentDisposition);
  if vStream <> nil then
  begin
    vStream.Position := 0;
    if vStream is TStringStream then
    begin
      Result := TStringStream(vStream).DataString;
    end
    else
    begin
      SetLength(Result, vStream.Size);
      vStream.Read(Result[PosIniStr], vStream.Size);
    end;

    if vFreeContent then
      vStream.Free;
  end;
end;

function TRALClientDigest.IsAuthenticated: boolean;
begin
  Result := (FDigestParams.Nonce <> '') and (FDigestParams.Opaque <> '')
end;

procedure TRALClientDigest.SetAuthHeader(AVars: TStringList; AParams: TRALParams);
var
  vAuth: TRALDigest;
  vParams: TStringList;
  vHead: StringRAL;
  vInt: IntegerRAL;
begin
  vAuth := TRALDigest.Create;
  try
    vAuth.Params.Assign(FDigestParams);
    vAuth.UserName := FUserName;
    vAuth.Password := FPassword;
    vAuth.URL := AVars.Values['url'];
    vAuth.Method := AVars.Values['method'];
    vAuth.EntityBody := GetEntityBody(AParams);
    vAuth.Params.NC := vAuth.Params.NC + 1;

    vParams := vAuth.Header;
    try
      for vInt := 0 to Pred(vParams.Count) do
      begin
        if vInt = 0 then
          vHead := vHead + Format(' %s="%s"',
            [vParams.Names[vInt], TRALHTTPCoder.EncodeURL(vParams.ValueFromIndex[vInt])])
        else
          vHead := vHead + Format(', %s="%s"',
            [vParams.Names[vInt], TRALHTTPCoder.EncodeURL(vParams.ValueFromIndex[vInt])])
      end;

      AParams.AddParam('Authorization', 'Digest ' + vHead, rpkHEADER);
    finally
      FreeAndNil(vParams);
    end;
  finally
    FreeAndNil(vAuth);
  end;
end;

{ TRALServerOAuth }

constructor TRALServerOAuth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlgorithm := toaHSHA256;
  FRouteInitialize := '/initialize/';
  FRouteAuthorize := '/authorize/';
end;

procedure TRALServerOAuth.Validate(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vAuth: TRALOAuth;
  vResult, vGetToken: boolean;
  vTokenSecret: StringRAL;
begin
  AResponse.StatusCode := HTTP_OK;
  if (ARequest.Authorization.AuthType <> ratOAuth) then
  begin
    AResponse.Answer(HTTP_Unauthorized);
    Exit;
  end;

  vResult := False;

  vAuth := TRALOAuth.Create;
  try
    vAuth.Algorithm := FAlgorithm;
    vAuth.ConsumerKey := FConsumerKey;

    if vAuth.Load(ARequest.Authorization.AuthString) then
    begin
      if (vAuth.TokenAccess <> '') then
      begin
        vTokenSecret := '';
        if Assigned(FOnGetTokenSecret) then
          FOnGetTokenSecret(vAuth.TokenAccess, vTokenSecret);
        vAuth.TokenSecret := vTokenSecret;
        vGetToken := vTokenSecret <> '';
      end
      else
      begin
        vGetToken := True;
      end;

      if vGetToken then
      begin
        vAuth.ConsumerSecret := FConsumerSecret;
        vAuth.URL := ARequest.URL;
        vAuth.Method := RALMethodToHTTPMethod(ARequest.Method);

        vResult := vAuth.Validate;
      end;
    end;
  finally
    FreeAndNil(vAuth);
  end;

  if not vResult then
    AResponse.Answer(HTTP_Unauthorized);
end;

procedure TRALServerOAuth.BeforeValidate(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  if SameText(ARequest.Query, FRouteInitialize) then
  begin

  end
  else if SameText(ARequest.Query, FRouteAuthorize) then
  begin

  end;
end;

{ TRALClientOAuth }

constructor TRALClientOAuth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlgorithm := toaHSHA256;
  FRouteInitialize := '/initialize/';
  FRouteAuthorize := '/authorize/';
end;

function TRALClientOAuth.IsAuthenticated: boolean;
begin
  Result := (FTokenAccess <> '') and (FTokenSecret = '')
end;

procedure TRALClientOAuth.SetAuthHeader(AVars: TStringList; AParams: TRALParams);
var
  vParams: TStringList;
  vInt: IntegerRAL;
  vHead: StringRAL;
  vAuth: TRALOAuth;
begin
  vAuth := TRALOAuth.Create;
  try
    vAuth.Algorithm := FAlgorithm;
    vAuth.Nonce := FNonce;
    if FTokenAccess = '' then
      vAuth.CallBack := FCallBack;
    vAuth.ConsumerKey := FConsumerKey;
    vAuth.ConsumerSecret := FConsumerSecret;
    vAuth.TokenAccess := FTokenAccess;
    vAuth.TokenSecret := FTokenSecret;
    vAuth.Verifier := FVerifier;
    vAuth.Version := '1.0';
    vAuth.URL := AVars.Values['url'];
    vAuth.Method := AVars.Values['method'];

    vParams := vAuth.Header;
    try
      vHead := 'realm="RALOAuth"';
      for vInt := 0 to Pred(vParams.Count) do
      begin
        if vInt = 0 then
          vHead := vHead + Format(' %s="%s"',
            [vParams.Names[vInt], TRALHTTPCoder.EncodeURL(vParams.ValueFromIndex[vInt])])
        else
          vHead := vHead + Format(', %s="%s"',
            [vParams.Names[vInt], TRALHTTPCoder.EncodeURL(vParams.ValueFromIndex[vInt])])
      end;

      AParams.AddParam('Authorization', 'OAuth ' + vHead, rpkHEADER);
    finally
      FreeAndNil(vParams);
    end;
  finally
    FreeAndNil(vAuth);
  end;
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

function TRALServerJWTAuth.CanIssue: boolean;
begin
  Result := Assigned(FOnGetToken) or Assigned(FOnGetTokenGen);
end;

function TRALServerJWTAuth.LogsIn(ARequest: TRALRequest): boolean;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  { no Bearer: a login, as always. With one, a login only when a body came
    with it - a login posts its credentials, a renewal posts nothing. Not the
    query, which a renewal may carry as a cache buster, nor the headers,
    which every request carries. ContentSize is the length the client
    declared, on every engine; the kinds of the params cannot tell it: Indy
    and UniGUI hand a form's fields over as query params, and fpHTTP and CGI
    file the standard headers and the environment as fields. The body param
    covers a body sent without a length (chunked); an empty POST still
    decodes into an empty one, hence the content test }
  Result := CanIssue;
  if (not Result) or (ARequest.Authorization.AuthType <> ratBearer) or
     (ARequest.Authorization.AuthString = '') then
    Exit;

  Result := ARequest.ContentSize > 0;
  if Result then
    Exit;
  for vInt := 0 to Pred(ARequest.Params.Count) do
  begin
    vParam := ARequest.Params.Index[vInt];
    if (vParam.Kind = rpkBODY) and not vParam.IsNilOrEmpty then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TRALServerJWTAuth.IssueToken(ARequest: TRALRequest; AResponse: TRALResponse;
  var AJSONParams: StringRAL): StringRAL;
var
  vParams: TRALJWTParams;
  vResult: boolean;
begin
  Result := '';
  vResult := False;
  vParams := TRALJWTParams.Create;
  try
    if Assigned(FOnGetToken) then
      FOnGetToken(ARequest, AResponse, vParams, vResult)
    else
      FOnGetTokenGen(ARequest, AResponse, vParams, vResult);
    if vResult then
    begin
      AJSONParams := vParams.AsJSON;
      Result := GetToken(AJSONParams);
    end;
  finally
    vParams.Free;
  end;
end;

{ The token route, in this order:
  - credentials, or no token at all: OnGetToken decides (LogsIn). Credentials
    used to lose to a Bearer - the cookie, with UseCookie - so a browser
    holding a token could not log in as somebody else: the next user of a
    shared machine got the previous one's token renewed;
  - a Bearer alone renews itself, same claims and a new expiration, with
    OnRenewToken consulted. Refused - expired, forged, OnRenewToken said no -
    it is a 401 and nothing else. Handing that request to OnGetToken would
    try a password sent in a header or in the query next to a dead token,
    and such a refusal cannot be counted as a failed login: a browser whose
    token expired sends one on every renewal. With UseCookie the browser is
    told to drop the cookie, so its next login goes through;
  - with neither, 401: without OnGetToken nobody checks who is asking, and a
    client could post any claims and walk away with a signed token }
procedure TRALServerJWTAuth.BeforeValidate(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vToken: StringRAL;
  vStrParams: StringRAL;
  vParamJWT: TRALJWTParams;
  vCookie: TRALCookie;
begin
  if not RALSameName(ARequest.Query, AuthRoute.Route) then
  begin
    AResponse.Answer(HTTP_NotFound);
    Exit;
  end;

  vToken := '';
  vStrParams := '';
  if LogsIn(ARequest) then
    vToken := IssueToken(ARequest, AResponse, vStrParams)
  else if (ARequest.Authorization.AuthType = ratBearer) and
          (ARequest.Authorization.AuthString <> '') then
    vToken := RenewTokenFor(ARequest, AResponse, ARequest.Authorization.AuthString,
      vStrParams);

  if vToken <> '' then
  begin
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
    AResponse.ResponseText := Format('{"%s":"%s"}', [FJSONKey, vToken]);
  end
  else
  begin
    if AResponse.StatusCode < HTTP_BadRequest then
      AResponse.Answer(HTTP_Unauthorized);
    { the browser sends the cookie it has on every call: refused, it is told
      to drop it, or it keeps offering the same dead token }
    if UseCookie and (ARequest.Params.GetKind[RALTOKENName, rpkCOOKIE] <> nil) then
    begin
      Finalize(vCookie);
      FillChar(vCookie, SizeOf(vCookie), 0);
      vCookie.Name := RALTOKENName;
      vCookie.Secure := true;
      vCookie.HttpOnly := true;
      vCookie.Path := '/';
      vCookie.MaxAge := -1;
      AResponse.AddCookie(vCookie);
    end;
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
  AResponse.AddHeader('WWW-Authenticate', vValue);
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

function TRALServerJWTAuth.AttemptOf(ARequest: TRALRequest; AResponse: TRALResponse;
  AOnAuthRoute: boolean): TRALAuthAttempt;
begin
  Result := raaNone;
  { the login itself: OnGetToken checked whatever credentials came with it -
    a Bearer sent along does not keep a wrong password from counting. Without
    the event every first token is refused, and that is no guess }
  if AOnAuthRoute and LogsIn(ARequest) then
  begin
    if AResponse.StatusCode < HTTP_BadRequest then
      Result := raaPassed
    else if AResponse.StatusCode = HTTP_Unauthorized then
      Result := raaFailed;
  end
  { nobody guesses a token: expired, forged or refused by OnValidate it says
    nothing about a password - and counting it, eight clients behind one NAT
    whose tokens expired together locked the address out. A good one proves
    the client logged in }
  else if (ARequest.Authorization.AuthType = ratBearer) and
          (AResponse.StatusCode < HTTP_BadRequest) then
    Result := raaPassed;
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
      AResponse.AddHeader('WWW-Authenticate', 'Basic realm="RAL Basic"');
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

procedure TRALAuthServer.BeforeValidate(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(HTTP_NotFound);
end;

function TRALAuthServer.CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse)
  : TRALRoute;
begin
  Result := TRALRoute(GetAuthRoute);
  if not RALSameName(Result.GetFullRoute, ARequest.Query) then
    Result := nil;
end;

function TRALAuthServer.AttemptOf(ARequest: TRALRequest; AResponse: TRALResponse;
  AOnAuthRoute: boolean): TRALAuthAttempt;
begin
  { credentials of this scheme came and were checked: refused, a guess;
    accepted, proof. 401 only - a 403 is a refusal of someone already known.
    A route of the scheme's own says nothing unless the scheme says so: the
    OAuth ones answer without checking anything }
  Result := raaNone;
  if AOnAuthRoute or (ARequest.Authorization.AuthType <> AuthType) then
    Exit;
  if AResponse.StatusCode < HTTP_BadRequest then
    Result := raaPassed
  else if AResponse.StatusCode = HTTP_Unauthorized then
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
