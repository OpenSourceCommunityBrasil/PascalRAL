/// OAuth 2.0 (RFC 6749/6750): authorization server, resource server and
/// client, plus the loopback redirect of native apps (RFC 8252)
unit RALOAuth2;

{ TRALServerOAuth2 is two things, and either can be switched off:
  - the authorization server (Issuing): the token endpoint with
    client_credentials, refresh_token and authorization_code with PKCE S256
    (RFC 7636, always required), introspection (RFC 7662), revocation
    (RFC 7009), the JWK set and the metadata document (RFC 8414). Tokens are
    JWTs - HS* with SignSecretKey, RS*/ES* with PrivateKey through OpenSSL;
  - the resource server (Validate): it checks the bearer tokens of the routes
    it protects, its own tokens (Validation = ovLocal), another issuer's
    through its JWK set (ovJWKS - Keycloak, Entra ID), or opaque ones through an
    introspection endpoint (ovIntrospection), and the scope each route needs
    (RouteScopes).

  Clients, secrets, users and consent are the application's, through events:
  the plugin never decides who someone is. Codes, refresh tokens and revoked
  token ids live in Store, in memory by default - an application that needs them
  across restarts or servers gives it a TRALOAuth2Store of its own.

  The grant type "password" is not there on purpose (RFC 9700, OAuth 2.1 drop
  it; the JWT /gettoken already covers that case), nor are the implicit flow,
  DPoP or mTLS. }

interface

uses
  Classes, SysUtils, DateUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALBase64, RALJson, RALToken, RALJWS,
  RALHashBase, RALSHA2_32, RALMIMETypes, RALUrlCoder, RALRoutes, RALRequest,
  RALResponse, RALParams, RALCustomObjects, RALPlugin, RALAuthentication, RALServer;

type
  /// How a client proves itself to the token endpoint: HTTP Basic
  /// (client_secret_basic) or fields in the body (client_secret_post)
  TRALOAuth2ClientAuth = (ocaSecretBasic, ocaSecretPost);
  /// What a client asks for when it has no token
  TRALOAuth2GrantType = (ogtClientCredentials, ogtAuthorizationCode);
  TRALOAuth2Grant = (ogClientCredentials, ogAuthorizationCode, ogRefreshToken);
  TRALOAuth2Grants = set of TRALOAuth2Grant;
  /// Where the resource server checks a token: its own keys, the issuer's JWK
  /// set, or the issuer's introspection endpoint
  TRALOAuth2Validation = (ovLocal, ovJWKS, ovIntrospection);
  /// What the application decided in OnAuthorize: ocPending when it wrote a
  /// page (a login form) into the response and the user has not answered yet
  TRALOAuth2Consent = (ocPending, ocApproved, ocDenied);

  { TRALOAuth2Ticket }

  /// What a code or a refresh token stands for
  TRALOAuth2Ticket = class
  public
    ClientID: StringRAL;
    CodeChallenge: StringRAL;
    Expires: TDateTime;
    RedirectURI: StringRAL;
    Scope: StringRAL;
    Subject: StringRAL;
    function Clone: TRALOAuth2Ticket;
  end;

  { TRALOAuth2Store }

  /// Codes, refresh tokens and revoked access tokens, in memory and safe for
  /// several threads. Descend and override to keep them elsewhere
  TRALOAuth2Store = class
  private
    FCodes: TStringList;
    FLock: TCriticalSection;
    FRefresh: TStringList;
    FRevoked: TStringList;
    procedure Prune;
    function Take(AList: TStringList; const AKey: StringRAL): TRALOAuth2Ticket;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    /// Whether the access token with this jti was revoked
    function IsRevoked(const AJTI: StringRAL): boolean; virtual;
    /// A copy of the ticket of a refresh token still valid, or nil. The caller
    /// frees it
    function PeekRefreshToken(const AToken: StringRAL): TRALOAuth2Ticket; virtual;
    /// Revokes the access token with this jti until it expires anyway
    procedure Revoke(const AJTI: StringRAL; AExpires: TDateTime); virtual;
    /// Keeps a code; the store owns ATicket
    procedure SaveCode(const ACode: StringRAL; ATicket: TRALOAuth2Ticket); virtual;
    /// Keeps a refresh token; the store owns ATicket
    procedure SaveRefreshToken(const AToken: StringRAL; ATicket: TRALOAuth2Ticket); virtual;
    /// Takes a code out - it is single use - with its ticket, or nil when it is
    /// unknown or expired. The caller frees it
    function TakeCode(const ACode: StringRAL): TRALOAuth2Ticket; virtual;
    /// Takes a refresh token out - a refresh token is used once, and a new one
    /// comes with the new access token - or nil. The caller frees it
    function TakeRefreshToken(const AToken: StringRAL): TRALOAuth2Ticket; virtual;
  end;

  TRALOnOAuth2Authorize = procedure(ARequest: TRALRequest; AResponse: TRALResponse;
    const AClientID, AScope: StringRAL; var ASubject: StringRAL;
    var AConsent: TRALOAuth2Consent) of object;
  TRALOnOAuth2Claims = procedure(ARequest: TRALRequest; const AClientID, ASubject,
    AScope: StringRAL; AClaims: TRALJWTParams) of object;
  TRALOnOAuth2Client = procedure(ARequest: TRALRequest; const AClientID,
    AClientSecret: StringRAL; var AValid: boolean) of object;
  TRALOnOAuth2Redirect = procedure(const AClientID, ARedirectURI: StringRAL;
    var AValid: boolean) of object;
  TRALOnOAuth2Scope = procedure(ARequest: TRALRequest; const AClientID,
    ASubject: StringRAL; var AScope: StringRAL; var AValid: boolean) of object;
  TRALOnOAuth2Token = procedure(ARequest: TRALRequest; AResponse: TRALResponse;
    AClaims: TRALJWTParams; var AValid: boolean) of object;

  { TRALServerOAuth2 }

  /// OAuth2 authorization server and resource server
  TRALServerOAuth2 = class(TRALAuthServer)
  private
    FAccessTokenLifetime: IntegerRAL;
    FAlgorithm: TRALJWTAlgorithm;
    FAudience: StringRAL;
    FAuthorizeRoute: TRALRoute;
    FClientCredentialsRefresh: boolean;
    FClockSkew: IntegerRAL;
    FCodeLifetime: IntegerRAL;
    FFetchEngine: StringRAL;
    FGrants: TRALOAuth2Grants;
    FIntrospectCache: TStringList;
    FIntrospectCacheSecs: IntegerRAL;
    FIntrospectionClientID: StringRAL;
    FIntrospectionClientSecret: StringRAL;
    FIntrospectionURL: StringRAL;
    FIntrospectRoute: TRALRoute;
    FIssuer: StringRAL;
    FIssuing: boolean;
    FJWKSRoute: TRALRoute;
    FJWKSURL: StringRAL;
    FKey: TRALJWSKey;
    FKeyID: StringRAL;
    FKeyText: StringRAL;
    FLock: TCriticalSection;
    FMetadataRoute: TRALRoute;
    FPrivateKey: TStrings;
    FRedirectURIs: TStrings;
    FRefreshTokenLifetime: IntegerRAL;
    FRemoteKeys: TRALJWKSet;
    FRemoteKeysAt: TDateTime;
    FRetired: TList;
    FRevokeRoute: TRALRoute;
    FRoutes: TRALRoutes;
    FRouteScopes: TStrings;
    FSignSecretKey: StringRAL;
    FStore: TRALOAuth2Store;
    FTokenRoute: TRALRoute;
    FValidation: TRALOAuth2Validation;
    FOnAuthorize: TRALOnOAuth2Authorize;
    FOnClaims: TRALOnOAuth2Claims;
    FOnValidateClient: TRALOnOAuth2Client;
    FOnValidateRedirect: TRALOnOAuth2Redirect;
    FOnValidateScope: TRALOnOAuth2Scope;
    FOnValidateToken: TRALOnOAuth2Token;
    procedure AnswerAuthorize(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure AnswerError(AResponse: TRALResponse; AStatus: IntegerRAL;
      const AError, ADescription: StringRAL; ABasicChallenge: boolean = False);
    procedure AnswerIntrospect(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure AnswerJWKS(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure AnswerMetadata(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure AnswerRevoke(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure AnswerToken(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure AnswerTokens(ARequest: TRALRequest; AResponse: TRALResponse;
      const AClientID, ASubject, AScope: StringRAL; ARefresh: boolean);
    function BaseURL(ARequest: TRALRequest): StringRAL;
    { the client of a request to the token, introspection or revocation
      endpoint: Basic or the fields; False, with the error already answered,
      when it does not authenticate }
    function CheckClient(ARequest: TRALRequest; AResponse: TRALResponse;
      out AClientID: StringRAL): boolean;
    function CheckRedirect(const AClientID, ARedirectURI: StringRAL): boolean;
    function CheckScope(ARequest: TRALRequest; const AClientID, ASubject: StringRAL;
      var AScope: StringRAL): boolean;
    function Fetch(const AURL: StringRAL; AFields: TStrings; const AUser,
      APassword: StringRAL; out AStatus: IntegerRAL): StringRAL;
    function Introspect(const AToken: StringRAL; AClaims: TRALJWTParams;
      out AReason: StringRAL): boolean;
    function RemoteKey(const AKeyID: StringRAL): TRALJWSKey;
    function RequiredScope(ARequest: TRALRequest): StringRAL;
    procedure SetPath(AIndex: IntegerRAL; const AValue: StringRAL);
    function GetPath(AIndex: IntegerRAL): StringRAL;
    procedure SetPrivateKey(AValue: TStrings);
    procedure SetRedirectURIs(AValue: TStrings);
    procedure SetRouteScopes(AValue: TStrings);
    procedure SetStore(AValue: TRALOAuth2Store);
    function SigningKey: TRALJWSKey;
    function VerifyJWT(const AToken: StringRAL; AKey: TRALJWSKey;
      AClaims: TRALJWTParams; out AReason: StringRAL): boolean;
  protected
    function GetAuthRoute: TRALBaseRoute; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// Answers the plugin's own routes (token, authorize, introspection...)
    procedure BeforeValidate(ARequest: TRALRequest; AResponse: TRALResponse); override;
    /// Checks an access token as the resource server does - signature,
    /// dates, iss, aud, revocation - and fills AClaims; AReason says why not
    function CheckAccessToken(const AToken: StringRAL; AClaims: TRALJWTParams;
      out AReason: StringRAL): boolean;
    /// An access token as the token endpoint issues it
    function IssueAccessToken(ARequest: TRALRequest; const AClientID, ASubject,
      AScope: StringRAL): StringRAL;
    /// PKCE S256: base64url(SHA-256(AVerifier))
    class function PKCEChallenge(const AVerifier: StringRAL): StringRAL;
    function ResolveRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
      override;
    /// The resource server: the bearer token of the request, and the scope of
    /// its route
    procedure Validate(ARequest: TRALRequest; AResponse: TRALResponse); override;

    /// Where codes, refresh tokens and revocations are kept. In memory by
    /// default; an object assigned here becomes the plugin's and is freed with it
    property Store: TRALOAuth2Store read FStore write SetStore;
  published
    /// Seconds an access token is good for
    property AccessTokenLifetime: IntegerRAL read FAccessTokenLifetime
      write FAccessTokenLifetime default 3600;
    /// The alg of the tokens issued, and the only one accepted
    property Algorithm: TRALJWTAlgorithm read FAlgorithm write FAlgorithm
      default tjaHSHA256;
    /// aud of the tokens issued; when set, a token without it is refused
    property Audience: StringRAL read FAudience write FAudience;
    property AuthorizePath: StringRAL index 1 read GetPath write SetPath;
    /// Issues a refresh token with client_credentials too (RFC 6749 4.4.3 says
    /// it should not, hence off)
    property ClientCredentialsRefresh: boolean read FClientCredentialsRefresh
      write FClientCredentialsRefresh default False;
    /// Seconds of clock difference tolerated on exp and nbf
    property ClockSkew: IntegerRAL read FClockSkew write FClockSkew default 60;
    /// Seconds an authorization code is good for
    property CodeLifetime: IntegerRAL read FCodeLifetime write FCodeLifetime
      default 60;
    /// The client engine of the requests the plugin makes itself (JWK set,
    /// introspection). Empty takes the first HTTP engine linked
    property FetchEngine: StringRAL read FFetchEngine write FFetchEngine;
    property Grants: TRALOAuth2Grants read FGrants write FGrants
      default [ogClientCredentials, ogAuthorizationCode, ogRefreshToken];
    /// Seconds an introspection answer is reused
    property IntrospectionCache: IntegerRAL read FIntrospectCacheSecs
      write FIntrospectCacheSecs default 60;
    property IntrospectionClientID: StringRAL read FIntrospectionClientID
      write FIntrospectionClientID;
    property IntrospectionClientSecret: StringRAL read FIntrospectionClientSecret
      write FIntrospectionClientSecret;
    /// The introspection endpoint of the issuer, for Validation = ovIntrospection
    property IntrospectionURL: StringRAL read FIntrospectionURL write FIntrospectionURL;
    property IntrospectPath: StringRAL index 2 read GetPath write SetPath;
    /// iss of the tokens issued and required of the tokens checked, and the
    /// base of the URLs of the metadata. Usually the server's public URL
    property Issuer: StringRAL read FIssuer write FIssuer;
    /// Answers the authorization server routes. Off, the plugin only checks
    /// tokens (a resource server)
    property Issuing: boolean read FIssuing write FIssuing default True;
    property JWKSPath: StringRAL index 3 read GetPath write SetPath;
    /// The JWK set of the issuer, for Validation = ovJWKS
    property JWKSURL: StringRAL read FJWKSURL write FJWKSURL;
    /// kid of the tokens issued; empty takes the key's thumbprint (RS*/ES*)
    property KeyID: StringRAL read FKeyID write FKeyID;
    property MetadataPath: StringRAL index 4 read GetPath write SetPath;
    /// The private key in PEM, for Algorithm RS*/ES*
    property PrivateKey: TStrings read FPrivateKey write SetPrivateKey;
    /// "client_id=redirect_uri" lines allowed when OnValidateRedirect is not
    /// assigned
    property RedirectURIs: TStrings read FRedirectURIs write SetRedirectURIs;
    /// Seconds a refresh token is good for; zero issues none
    property RefreshTokenLifetime: IntegerRAL read FRefreshTokenLifetime
      write FRefreshTokenLifetime default 1209600;
    property RevokePath: StringRAL index 5 read GetPath write SetPath;
    /// "route=scope1 scope2" lines: the scopes a token needs for a route (its
    /// full route, as declared: /users/:id)
    property RouteScopes: TStrings read FRouteScopes write SetRouteScopes;
    /// The HMAC key of Algorithm HS*
    property SignSecretKey: StringRAL read FSignSecretKey write FSignSecretKey;
    property TokenPath: StringRAL index 0 read GetPath write SetPath;
    property Validation: TRALOAuth2Validation read FValidation write FValidation
      default ovLocal;

    /// Login and consent of the authorization_code flow. Unassigned, every
    /// authorization is denied
    property OnAuthorize: TRALOnOAuth2Authorize read FOnAuthorize write FOnAuthorize;
    /// Claims of your own in the access tokens issued
    property OnClaims: TRALOnOAuth2Claims read FOnClaims write FOnClaims;
    /// Whether AClientSecret is the secret of AClientID - empty for a public
    /// client in the authorization_code flow. Unassigned, no client is valid
    property OnValidateClient: TRALOnOAuth2Client read FOnValidateClient
      write FOnValidateClient;
    property OnValidateRedirect: TRALOnOAuth2Redirect read FOnValidateRedirect
      write FOnValidateRedirect;
    /// Narrow or refuse the scope asked for. Unassigned, any scope is granted
    property OnValidateScope: TRALOnOAuth2Scope read FOnValidateScope
      write FOnValidateScope;
    /// A last say over a token already checked, with its claims
    property OnValidateToken: TRALOnOAuth2Token read FOnValidateToken
      write FOnValidateToken;
  end;

  { TRALClientOAuth2 }

  /// OAuth2 client: gets, renews and sends the access token
  TRALClientOAuth2 = class(TRALAuthClient)
  private
    FAccessToken: StringRAL;
    FAudience: StringRAL;
    FClientAuth: TRALOAuth2ClientAuth;
    FClientID: StringRAL;
    FClientSecret: StringRAL;
    FExpiresAt: TDateTime;
    FGrantType: TRALOAuth2GrantType;
    FPendingCode: StringRAL;
    FPendingRedirect: StringRAL;
    FPendingState: StringRAL;
    FPendingVerifier: StringRAL;
    FRefreshMargin: IntegerRAL;
    FRefreshToken: StringRAL;
    FScope: StringRAL;
    FTokenScope: StringRAL;
    FTokenURL: StringRAL;
    function GetAccessToken: StringRAL;
    function GetRefreshToken: StringRAL;
    procedure SetRefreshToken(const AValue: StringRAL);
    { posts AFields to TokenURL; True with the tokens taken, False with the
      provider's refusal in AError (ARefused) or a transport failure (the
      result code) }
    function RequestToken(ATransport: TRALAuthTransport; AFields: TStrings;
      AResponse: TRALResponse; out AError: StringRAL): IntegerRAL;
  public
    constructor Create(AOwner: TComponent); override;
    /// The URL to send the user to (a browser), with a new state and PKCE
    /// verifier kept here. AState is what the answer must bring back
    function AuthorizationURL(const AAuthorizeURL, ARedirectURI: StringRAL;
      out AState: StringRAL): StringRAL;
    /// Forgets the tokens
    procedure ClearTokens;
    /// A 401: the access token is dropped; the refresh token, if any, renews
    /// it before the request goes again
    function HandleChallenge(AResponse: TRALResponse): boolean; override;
    /// There is an access token not about to expire
    function IsAuthenticated: boolean; override;
    /// Obtains the access token: refresh_token first, then the code of
    /// SetAuthorizationCode, or client_credentials
    function Prepare(ATransport: TRALAuthTransport; AVars: TStringList;
      AResponse: TRALResponse): IntegerRAL; override;
    /// The code the redirect brought, with its state; the next request
    /// exchanges it. Raises when the state is not the one sent
    procedure SetAuthorizationCode(const ACode, AState: StringRAL);
    procedure SetAuthHeader(AVars: TStringList; AParams: TRALParams); override;

    property AccessToken: StringRAL read GetAccessToken;
    /// When the access token expires (local time); zero when the provider
    /// did not say
    property ExpiresAt: TDateTime read FExpiresAt;
    /// Kept by the application across runs, to start from it
    property RefreshToken: StringRAL read GetRefreshToken write SetRefreshToken;
    /// The scope the provider granted
    property TokenScope: StringRAL read FTokenScope;
  published
    /// Sent as "audience" with the token request, when set
    property Audience: StringRAL read FAudience write FAudience;
    property ClientAuth: TRALOAuth2ClientAuth read FClientAuth write FClientAuth
      default ocaSecretBasic;
    property ClientID: StringRAL read FClientID write FClientID;
    property ClientSecret: StringRAL read FClientSecret write FClientSecret;
    property GrantType: TRALOAuth2GrantType read FGrantType write FGrantType
      default ogtClientCredentials;
    /// Seconds before expires_in when the token is renewed ahead of a 401
    property RefreshMargin: IntegerRAL read FRefreshMargin write FRefreshMargin
      default 30;
    property Scope: StringRAL read FScope write FScope;
    /// The token endpoint: a route of the server called, or an absolute URL
    /// (the provider is often another host)
    property TokenURL: StringRAL read FTokenURL write FTokenURL;
  end;

  { TRALOAuth2Loopback }

  /// The redirect of a desktop app (RFC 8252 7.3): a route on a TRALServer
  /// listening on 127.0.0.1 receives the code the browser brings back
  TRALOAuth2Loopback = class(TRALComponent)
  private
    FCode: StringRAL;
    FError: StringRAL;
    FEvent: TEvent;
    FPath: StringRAL;
    FRoute: TRALRoute;
    FServer: TRALServer;
    FState: StringRAL;
    procedure Reply(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure SetServer(AValue: TRALServer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// http://127.0.0.1:<Server.Port><Path>
    function RedirectURI: StringRAL;
    /// Starts Server, waits up to ATimeout ms for the browser, stops it. The
    /// error the provider sent (access_denied) comes in AError
    function WaitForCode(ATimeout: IntegerRAL; out ACode, AState,
      AError: StringRAL): boolean;
  published
    property Path: StringRAL read FPath write FPath;
    /// Any engine; its Port is the one of RedirectURI
    property Server: TRALServer read FServer write SetServer;
  end;

implementation

uses
  RALClient;

const
  cPathDefaults: array[0..5] of StringRAL = ('/oauth/token', '/oauth/authorize',
    '/oauth/introspect', '/oauth/jwks', '/.well-known/oauth-authorization-server',
    '/oauth/revoke');

function RandomText(ABytes: IntegerRAL): StringRAL;
begin
  Result := RALBase64UrlEncode(RandomBytes(ABytes));
end;

function FieldOf(ARequest: TRALRequest; const AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  vParam := ARequest.Params.GetKind[AName, rpkFIELD];
  if vParam = nil then
    vParam := ARequest.Params.GetKind[AName, rpkQUERY];
  if vParam <> nil then
    Result := vParam.AsString
  else
    Result := '';
end;

{ every scope of ANeeded is in AHave (both space-separated) }
function HasScopes(const AHave, ANeeded: StringRAL): boolean;
var
  vHave, vNeed: TStringList;
  vInt: IntegerRAL;
begin
  vHave := TStringList.Create;
  vNeed := TStringList.Create;
  try
    vHave.Delimiter := ' ';
    vHave.StrictDelimiter := True;
    vHave.DelimitedText := AHave;
    vNeed.Delimiter := ' ';
    vNeed.StrictDelimiter := True;
    vNeed.DelimitedText := ANeeded;
    Result := True;
    for vInt := 0 to Pred(vNeed.Count) do
      if (Trim(vNeed.Strings[vInt]) <> '') and (vHave.IndexOf(vNeed.Strings[vInt]) < 0) then
        Exit(False);
  finally
    vNeed.Free;
    vHave.Free;
  end;
end;

function TokenKey(const AToken: StringRAL): StringRAL;
var
  vHash: TRALSHA2_32;
begin
  vHash := TRALSHA2_32.Create;
  try
    vHash.Version := rsv256;
    vHash.OutputType := rhotBase64Url;
    Result := vHash.HashAsString(AToken);
  finally
    vHash.Free;
  end;
end;

{ TRALOAuth2Ticket }

function TRALOAuth2Ticket.Clone: TRALOAuth2Ticket;
begin
  Result := TRALOAuth2Ticket.Create;
  Result.ClientID := ClientID;
  Result.CodeChallenge := CodeChallenge;
  Result.Expires := Expires;
  Result.RedirectURI := RedirectURI;
  Result.Scope := Scope;
  Result.Subject := Subject;
end;

{ TRALOAuth2Store }

constructor TRALOAuth2Store.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FCodes := TStringList.Create;
  FCodes.Sorted := True;
  FRefresh := TStringList.Create;
  FRefresh.Sorted := True;
  FRevoked := TStringList.Create;
  FRevoked.Sorted := True;
end;

destructor TRALOAuth2Store.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Pred(FCodes.Count) do
    FCodes.Objects[vInt].Free;
  for vInt := 0 to Pred(FRefresh.Count) do
    FRefresh.Objects[vInt].Free;
  FreeAndNil(FCodes);
  FreeAndNil(FRefresh);
  FreeAndNil(FRevoked);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TRALOAuth2Store.IsRevoked(const AJTI: StringRAL): boolean;
begin
  if AJTI = '' then
    Exit(False);
  FLock.Acquire;
  try
    { by the hash: a jti is base64, and its "=" padding broke the name=value
      lines - IndexOfName never found a revoked token }
    Result := FRevoked.IndexOfName(TokenKey(AJTI)) >= 0;
  finally
    FLock.Release;
  end;
end;

function TRALOAuth2Store.PeekRefreshToken(const AToken: StringRAL): TRALOAuth2Ticket;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  FLock.Acquire;
  try
    Prune;
    vInt := FRefresh.IndexOf(TokenKey(AToken));
    if vInt >= 0 then
      Result := TRALOAuth2Ticket(FRefresh.Objects[vInt]).Clone;
  finally
    FLock.Release;
  end;
end;

procedure TRALOAuth2Store.Prune;

  procedure PruneList(AList: TStringList);
  var
    vInt: IntegerRAL;
  begin
    for vInt := Pred(AList.Count) downto 0 do
      if TRALOAuth2Ticket(AList.Objects[vInt]).Expires < Now then
      begin
        AList.Objects[vInt].Free;
        AList.Delete(vInt);
      end;
  end;

var
  vInt: IntegerRAL;
begin
  // under FLock
  PruneList(FCodes);
  PruneList(FRefresh);
  for vInt := Pred(FRevoked.Count) downto 0 do
    if StrToFloatDef(FRevoked.ValueFromIndex[vInt], 0) < Now then
      FRevoked.Delete(vInt);
end;

procedure TRALOAuth2Store.Revoke(const AJTI: StringRAL; AExpires: TDateTime);
begin
  if AJTI = '' then
    Exit;
  FLock.Acquire;
  try
    Prune;
    if FRevoked.IndexOfName(TokenKey(AJTI)) < 0 then
      FRevoked.Add(TokenKey(AJTI) + '=' + FloatToStr(AExpires));
  finally
    FLock.Release;
  end;
end;

procedure TRALOAuth2Store.SaveCode(const ACode: StringRAL; ATicket: TRALOAuth2Ticket);
begin
  FLock.Acquire;
  try
    Prune;
    FCodes.AddObject(TokenKey(ACode), ATicket);
  finally
    FLock.Release;
  end;
end;

procedure TRALOAuth2Store.SaveRefreshToken(const AToken: StringRAL;
  ATicket: TRALOAuth2Ticket);
begin
  FLock.Acquire;
  try
    Prune;
    FRefresh.AddObject(TokenKey(AToken), ATicket);
  finally
    FLock.Release;
  end;
end;

function TRALOAuth2Store.Take(AList: TStringList; const AKey: StringRAL): TRALOAuth2Ticket;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  FLock.Acquire;
  try
    Prune;
    { kept by the hash of the token, never the token itself: a dump of this
      list is no key to anything }
    vInt := AList.IndexOf(TokenKey(AKey));
    if vInt >= 0 then
    begin
      Result := TRALOAuth2Ticket(AList.Objects[vInt]);
      AList.Delete(vInt);
    end;
  finally
    FLock.Release;
  end;
end;

function TRALOAuth2Store.TakeCode(const ACode: StringRAL): TRALOAuth2Ticket;
begin
  Result := Take(FCodes, ACode);
end;

function TRALOAuth2Store.TakeRefreshToken(const AToken: StringRAL): TRALOAuth2Ticket;
begin
  Result := Take(FRefresh, AToken);
end;

{ TRALServerOAuth2 }

constructor TRALServerOAuth2.Create(AOwner: TComponent);
var
  vInt: IntegerRAL;
  vRoute: TRALRoute;
begin
  inherited Create(AOwner);
  SetAuthType(ratBearer);
  FAccessTokenLifetime := 3600;
  FAlgorithm := tjaHSHA256;
  FClientCredentialsRefresh := False;
  FClockSkew := 60;
  FCodeLifetime := 60;
  FGrants := [ogClientCredentials, ogAuthorizationCode, ogRefreshToken];
  FIntrospectCacheSecs := 60;
  FIssuing := True;
  FRefreshTokenLifetime := 1209600; // 14 days
  FValidation := ovLocal;

  FLock := TCriticalSection.Create;
  FIntrospectCache := TStringList.Create;
  FIntrospectCache.Sorted := True;
  FPrivateKey := TStringList.Create;
  FRedirectURIs := TStringList.Create;
  FRouteScopes := TStringList.Create;
  FRetired := TList.Create;
  FStore := TRALOAuth2Store.Create;
  FKey := nil;
  FRemoteKeys := nil;
  FRemoteKeysAt := 0;

  { the routes of the authorization server: nobody authenticates to reach
    them - the token endpoint authenticates the client itself }
  FRoutes := TRALRoutes.Create(Self);
  for vInt := 0 to High(cPathDefaults) do
  begin
    vRoute := TRALRoute(FRoutes.Add);
    vRoute.Route := cPathDefaults[vInt];
    vRoute.SkipAuthMethods := [amALL];
    case vInt of
      0: FTokenRoute := vRoute;
      1: FAuthorizeRoute := vRoute;
      2: FIntrospectRoute := vRoute;
      3: FJWKSRoute := vRoute;
      4: FMetadataRoute := vRoute;
      5: FRevokeRoute := vRoute;
    end;
  end;
  FTokenRoute.AllowedMethods := [amPOST, amOPTIONS];
  FIntrospectRoute.AllowedMethods := [amPOST, amOPTIONS];
  FRevokeRoute.AllowedMethods := [amPOST, amOPTIONS];
  FAuthorizeRoute.AllowedMethods := [amGET, amPOST];
  FJWKSRoute.AllowedMethods := [amGET, amOPTIONS];
  FMetadataRoute.AllowedMethods := [amGET, amOPTIONS];
end;

destructor TRALServerOAuth2.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Pred(FIntrospectCache.Count) do
    FIntrospectCache.Objects[vInt].Free;
  for vInt := 0 to Pred(FRetired.Count) do
    TObject(FRetired.Items[vInt]).Free;
  FreeAndNil(FRetired);
  FreeAndNil(FIntrospectCache);
  FreeAndNil(FRemoteKeys);
  FreeAndNil(FKey);
  FreeAndNil(FStore);
  FreeAndNil(FRoutes);
  FreeAndNil(FPrivateKey);
  FreeAndNil(FRedirectURIs);
  FreeAndNil(FRouteScopes);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALServerOAuth2.AnswerAuthorize(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vClient, vRedirect, vScope, vState, vChallenge, vSubject, vCode: StringRAL;
  vConsent: TRALOAuth2Consent;
  vTicket: TRALOAuth2Ticket;

  procedure Redirect(const AParams: StringRAL);
  var
    vSep: StringRAL;
  begin
    if Pos(StringRAL('?'), vRedirect) > 0 then
      vSep := '&'
    else
      vSep := '?';
    AResponse.Answer(HTTP_Found, '', rctTEXTPLAIN);
    AResponse.AddHeader('Location', vRedirect + vSep + AParams);
  end;

  procedure RedirectError(const AError: StringRAL);
  var
    vText: StringRAL;
  begin
    vText := 'error=' + AError;
    if vState <> '' then
      vText := vText + '&state=' + TRALHTTPCoder.EncodeURL(vState);
    Redirect(vText);
  end;

begin
  vClient := FieldOf(ARequest, 'client_id');
  vRedirect := FieldOf(ARequest, 'redirect_uri');
  vScope := FieldOf(ARequest, 'scope');
  vState := FieldOf(ARequest, 'state');
  vChallenge := FieldOf(ARequest, 'code_challenge');

  { an unknown client or redirect_uri is never redirected to: that is exactly
    how a code would be handed to someone else }
  if (vClient = '') or (vRedirect = '') or (not CheckRedirect(vClient, vRedirect)) then
  begin
    AResponse.Answer(HTTP_BadRequest, RALChallengeText(wmOAuth2RedirectURI), rctTEXTPLAIN);
    Exit;
  end;

  if not (ogAuthorizationCode in FGrants) then
  begin
    RedirectError('unsupported_response_type');
    Exit;
  end;
  if FieldOf(ARequest, 'response_type') <> 'code' then
  begin
    RedirectError('unsupported_response_type');
    Exit;
  end;
  { PKCE is not optional here (OAuth 2.1), and only S256 - plain would send
    the verifier itself through the browser }
  if (vChallenge = '') or (FieldOf(ARequest, 'code_challenge_method') <> 'S256') then
  begin
    RedirectError('invalid_request');
    Exit;
  end;

  vSubject := '';
  vConsent := ocDenied;
  if Assigned(FOnAuthorize) then
    FOnAuthorize(ARequest, AResponse, vClient, vScope, vSubject, vConsent);

  case vConsent of
    ocPending:
      Exit; // the application answered: a login page, a consent form
    ocDenied:
    begin
      RedirectError('access_denied');
      Exit;
    end;
  end;

  if not CheckScope(ARequest, vClient, vSubject, vScope) then
  begin
    RedirectError('invalid_scope');
    Exit;
  end;

  vCode := RandomText(32);
  vTicket := TRALOAuth2Ticket.Create;
  vTicket.ClientID := vClient;
  vTicket.CodeChallenge := vChallenge;
  vTicket.Expires := IncSecond(Now, FCodeLifetime);
  vTicket.RedirectURI := vRedirect;
  vTicket.Scope := vScope;
  vTicket.Subject := vSubject;
  FStore.SaveCode(vCode, vTicket);

  vCode := 'code=' + TRALHTTPCoder.EncodeURL(vCode);
  if vState <> '' then
    vCode := vCode + '&state=' + TRALHTTPCoder.EncodeURL(vState);
  Redirect(vCode);
end;

procedure TRALServerOAuth2.AnswerError(AResponse: TRALResponse; AStatus: IntegerRAL;
  const AError, ADescription: StringRAL; ABasicChallenge: boolean);
var
  vJson: TRALJSONObject;
begin
  vJson := TRALJSONObject.Create;
  try
    vJson.Add('error', AError);
    if ADescription <> '' then
      vJson.Add('error_description', RALChallengeText(ADescription));
    AResponse.Answer(AStatus, vJson.ToJSON, rctAPPLICATIONJSON);
  finally
    vJson.Free;
  end;
  AResponse.AddHeader('Cache-Control', 'no-store');
  AResponse.AddHeader('Pragma', 'no-cache');
  if ABasicChallenge then
    RALAddChallenge(AResponse, 'Basic realm="' + FIssuer + '"');
end;

procedure TRALServerOAuth2.AnswerIntrospect(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vClient, vToken, vReason: StringRAL;
  vTicket: TRALOAuth2Ticket;
  vClaims: TRALJWTParams;
  vJson: TRALJSONObject;
begin
  if not CheckClient(ARequest, AResponse, vClient) then
    Exit;
  vToken := FieldOf(ARequest, 'token');

  vJson := TRALJSONObject.Create;
  try
    vTicket := FStore.PeekRefreshToken(vToken);
    if vTicket <> nil then
    begin
      try
        vJson.Add('active', True);
        vJson.Add('token_type', 'refresh_token');
        vJson.Add('client_id', vTicket.ClientID);
        if vTicket.Subject <> '' then
          vJson.Add('sub', vTicket.Subject);
        if vTicket.Scope <> '' then
          vJson.Add('scope', vTicket.Scope);
        vJson.Add('exp', DateTimeToUnix(RALDateTimeToGMT(vTicket.Expires)));
      finally
        vTicket.Free;
      end;
    end
    else
    begin
      vClaims := TRALJWTParams.Create;
      try
        if (vToken <> '') and CheckAccessToken(vToken, vClaims, vReason) then
        begin
          vJson.Add('active', True);
          vJson.Add('token_type', 'Bearer');
          vJson.Add('client_id', vClaims.GetClaim('client_id'));
          if vClaims.Subject <> '' then
            vJson.Add('sub', vClaims.Subject);
          if vClaims.GetClaim('scope') <> '' then
            vJson.Add('scope', vClaims.GetClaim('scope'));
          if vClaims.Issuer <> '' then
            vJson.Add('iss', vClaims.Issuer);
          if vClaims.Audience <> '' then
            vJson.Add('aud', vClaims.Audience);
          if vClaims.Id <> '' then
            vJson.Add('jti', vClaims.Id);
          if vClaims.Expiration > 0 then
            vJson.Add('exp', DateTimeToUnix(RALDateTimeToGMT(vClaims.Expiration)));
          if vClaims.IssuedAt > 0 then
            vJson.Add('iat', DateTimeToUnix(RALDateTimeToGMT(vClaims.IssuedAt)));
        end
        else
          { an unknown, expired or revoked token is just inactive (RFC 7662
            2.2): the answer says nothing more to someone probing }
          vJson.Add('active', False);
      finally
        vClaims.Free;
      end;
    end;
    AResponse.Answer(HTTP_OK, vJson.ToJSON, rctAPPLICATIONJSON);
    AResponse.AddHeader('Cache-Control', 'no-store');
  finally
    vJson.Free;
  end;
end;

procedure TRALServerOAuth2.AnswerJWKS(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vKey: TRALJWSKey;
  vText: StringRAL;
begin
  vText := '{"keys":[';
  vKey := SigningKey;
  if vKey <> nil then
    vText := vText + vKey.AsJWK(TRALJWT.AlgorithmName(FAlgorithm));
  vText := vText + ']}';
  AResponse.Answer(HTTP_OK, vText, rctAPPLICATIONJSON);
end;

procedure TRALServerOAuth2.AnswerMetadata(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vJson: TRALJSONObject;
  vArray: TRALJSONArray;
  vBase: StringRAL;
begin
  vBase := BaseURL(ARequest);
  vJson := TRALJSONObject.Create;
  try
    vJson.Add('issuer', vBase);
    vJson.Add('token_endpoint', vBase + FTokenRoute.Route);
    if ogAuthorizationCode in FGrants then
      vJson.Add('authorization_endpoint', vBase + FAuthorizeRoute.Route);
    vJson.Add('introspection_endpoint', vBase + FIntrospectRoute.Route);
    vJson.Add('revocation_endpoint', vBase + FRevokeRoute.Route);
    if FAlgorithm in [tjaRS256, tjaRS384, tjaRS512, tjaES256, tjaES384] then
      vJson.Add('jwks_uri', vBase + FJWKSRoute.Route);

    vArray := TRALJSONArray.Create;
    if ogClientCredentials in FGrants then
      vArray.Add('client_credentials');
    if ogAuthorizationCode in FGrants then
      vArray.Add('authorization_code');
    if ogRefreshToken in FGrants then
      vArray.Add('refresh_token');
    vJson.Add('grant_types_supported', vArray);

    vArray := TRALJSONArray.Create;
    vArray.Add('code');
    vJson.Add('response_types_supported', vArray);

    vArray := TRALJSONArray.Create;
    vArray.Add('S256');
    vJson.Add('code_challenge_methods_supported', vArray);

    vArray := TRALJSONArray.Create;
    vArray.Add('client_secret_basic');
    vArray.Add('client_secret_post');
    vJson.Add('token_endpoint_auth_methods_supported', vArray);

    vArray := TRALJSONArray.Create;
    vArray.Add(TRALJWT.AlgorithmName(FAlgorithm));
    vJson.Add('token_endpoint_auth_signing_alg_values_supported', vArray);

    AResponse.Answer(HTTP_OK, StringReplace(vJson.ToJSON, '\/', '/', [rfReplaceAll]),
      rctAPPLICATIONJSON);
  finally
    vJson.Free;
  end;
end;

procedure TRALServerOAuth2.AnswerRevoke(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vClient, vToken, vReason: StringRAL;
  vTicket: TRALOAuth2Ticket;
  vClaims: TRALJWTParams;
begin
  if not CheckClient(ARequest, AResponse, vClient) then
    Exit;
  vToken := FieldOf(ARequest, 'token');

  vTicket := FStore.TakeRefreshToken(vToken);
  if vTicket <> nil then
  begin
    { only the client the token was issued to may revoke it; anyone else's
      attempt leaves it where it was }
    if vTicket.ClientID <> vClient then
      FStore.SaveRefreshToken(vToken, vTicket)
    else
      vTicket.Free;
  end
  else
  begin
    vClaims := TRALJWTParams.Create;
    try
      if (vToken <> '') and CheckAccessToken(vToken, vClaims, vReason) and
         (vClaims.GetClaim('client_id') = vClient) then
        FStore.Revoke(vClaims.Id, IncSecond(vClaims.Expiration, FClockSkew));
    finally
      vClaims.Free;
    end;
  end;
  { 200 whatever the token was (RFC 7009 2.2): an invalid token is already
    as revoked as it gets }
  AResponse.Answer(HTTP_OK, '', rctTEXTPLAIN);
end;

procedure TRALServerOAuth2.AnswerToken(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vGrant, vClient, vScope, vCode, vToken: StringRAL;
  vTicket: TRALOAuth2Ticket;
begin
  vGrant := FieldOf(ARequest, 'grant_type');

  if vGrant = 'client_credentials' then
  begin
    if not (ogClientCredentials in FGrants) then
    begin
      AnswerError(AResponse, HTTP_BadRequest, 'unsupported_grant_type',
        wmOAuth2UnsupportedGrant);
      Exit;
    end;
    if not CheckClient(ARequest, AResponse, vClient) then
      Exit;
    vScope := FieldOf(ARequest, 'scope');
    if not CheckScope(ARequest, vClient, vClient, vScope) then
    begin
      AnswerError(AResponse, HTTP_BadRequest, 'invalid_scope', wmOAuth2InvalidScope);
      Exit;
    end;
    AnswerTokens(ARequest, AResponse, vClient, vClient, vScope,
      FClientCredentialsRefresh);
  end
  else if vGrant = 'refresh_token' then
  begin
    if not (ogRefreshToken in FGrants) then
    begin
      AnswerError(AResponse, HTTP_BadRequest, 'unsupported_grant_type',
        wmOAuth2UnsupportedGrant);
      Exit;
    end;
    if not CheckClient(ARequest, AResponse, vClient) then
      Exit;
    vToken := FieldOf(ARequest, 'refresh_token');
    vTicket := FStore.TakeRefreshToken(vToken);
    if vTicket = nil then
    begin
      AnswerError(AResponse, HTTP_BadRequest, 'invalid_grant', wmOAuth2InvalidGrant);
      Exit;
    end;
    try
      if vTicket.ClientID <> vClient then
      begin
        AnswerError(AResponse, HTTP_BadRequest, 'invalid_grant', wmOAuth2InvalidGrant);
        Exit;
      end;
      { a refresh may narrow the scope, never widen it }
      vScope := FieldOf(ARequest, 'scope');
      if vScope = '' then
        vScope := vTicket.Scope
      else if not HasScopes(vTicket.Scope, vScope) then
      begin
        AnswerError(AResponse, HTTP_BadRequest, 'invalid_scope', wmOAuth2InvalidScope);
        Exit;
      end;
      AnswerTokens(ARequest, AResponse, vClient, vTicket.Subject, vScope, True);
    finally
      vTicket.Free;
    end;
  end
  else if vGrant = 'authorization_code' then
  begin
    if not (ogAuthorizationCode in FGrants) then
    begin
      AnswerError(AResponse, HTTP_BadRequest, 'unsupported_grant_type',
        wmOAuth2UnsupportedGrant);
      Exit;
    end;
    if not CheckClient(ARequest, AResponse, vClient) then
      Exit;
    vCode := FieldOf(ARequest, 'code');
    vTicket := nil;
    if vCode <> '' then
      vTicket := FStore.TakeCode(vCode);
    if vTicket = nil then
    begin
      AnswerError(AResponse, HTTP_BadRequest, 'invalid_grant', wmOAuth2InvalidGrant);
      Exit;
    end;
    try
      if vTicket.ClientID <> vClient then
      begin
        AnswerError(AResponse, HTTP_BadRequest, 'invalid_grant', wmOAuth2InvalidGrant);
        Exit;
      end;
      if vTicket.RedirectURI <> FieldOf(ARequest, 'redirect_uri') then
      begin
        AnswerError(AResponse, HTTP_BadRequest, 'invalid_grant', wmOAuth2RedirectURI);
        Exit;
      end;
      if not RALSameSecret(PKCEChallenge(FieldOf(ARequest, 'code_verifier')),
         vTicket.CodeChallenge) then
      begin
        AnswerError(AResponse, HTTP_BadRequest, 'invalid_grant', wmOAuth2PKCE);
        Exit;
      end;
      AnswerTokens(ARequest, AResponse, vClient, vTicket.Subject, vTicket.Scope, True);
    finally
      vTicket.Free;
    end;
  end
  else if vGrant = '' then
    AnswerError(AResponse, HTTP_BadRequest, 'invalid_request', wmOAuth2InvalidRequest)
  else
    AnswerError(AResponse, HTTP_BadRequest, 'unsupported_grant_type',
      wmOAuth2UnsupportedGrant);
end;

procedure TRALServerOAuth2.AnswerTokens(ARequest: TRALRequest; AResponse: TRALResponse;
  const AClientID, ASubject, AScope: StringRAL; ARefresh: boolean);
var
  vJson: TRALJSONObject;
  vRefresh: StringRAL;
  vTicket: TRALOAuth2Ticket;
begin
  vJson := TRALJSONObject.Create;
  try
    vJson.Add('access_token', IssueAccessToken(ARequest, AClientID, ASubject, AScope));
    vJson.Add('token_type', 'Bearer');
    vJson.Add('expires_in', FAccessTokenLifetime);
    if ARefresh and (ogRefreshToken in FGrants) and (FRefreshTokenLifetime > 0) then
    begin
      vRefresh := RandomText(32);
      vTicket := TRALOAuth2Ticket.Create;
      vTicket.ClientID := AClientID;
      vTicket.Subject := ASubject;
      vTicket.Scope := AScope;
      vTicket.Expires := IncSecond(Now, FRefreshTokenLifetime);
      FStore.SaveRefreshToken(vRefresh, vTicket);
      vJson.Add('refresh_token', vRefresh);
    end;
    if AScope <> '' then
      vJson.Add('scope', AScope);
    AResponse.Answer(HTTP_OK, vJson.ToJSON, rctAPPLICATIONJSON);
  finally
    vJson.Free;
  end;
  AResponse.AddHeader('Cache-Control', 'no-store');
  AResponse.AddHeader('Pragma', 'no-cache');
end;

function TRALServerOAuth2.BaseURL(ARequest: TRALRequest): StringRAL;
begin
  if FIssuer <> '' then
    Result := FIssuer
  else
  begin
    { HttpVersion is the scheme (the name it always had) }
    Result := LowerCase(ARequest.HttpVersion);
    if Result = '' then
      Result := 'http';
    Result := Result + '://' + ARequest.Host;
  end;
  while (Result <> '') and (Result[RALHighStr(Result)] = '/') do
    Delete(Result, Length(Result), 1);
end;

procedure TRALServerOAuth2.BeforeValidate(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vRoute: TRALRoute;
begin
  vRoute := TRALRoute(ARequest.ResolvedRoute);
  if (vRoute = nil) or (not vRoute.IsMethodAllowed(ARequest.Method)) then
  begin
    AResponse.Answer(HTTP_MethodNotAllowed);
    Exit;
  end;

  if vRoute = FTokenRoute then
    AnswerToken(ARequest, AResponse)
  else if vRoute = FAuthorizeRoute then
    AnswerAuthorize(ARequest, AResponse)
  else if vRoute = FIntrospectRoute then
    AnswerIntrospect(ARequest, AResponse)
  else if vRoute = FRevokeRoute then
    AnswerRevoke(ARequest, AResponse)
  else if vRoute = FJWKSRoute then
    AnswerJWKS(ARequest, AResponse)
  else if vRoute = FMetadataRoute then
    AnswerMetadata(ARequest, AResponse)
  else
    AResponse.Answer(HTTP_NotFound);
end;

function TRALServerOAuth2.CheckAccessToken(const AToken: StringRAL;
  AClaims: TRALJWTParams; out AReason: StringRAL): boolean;
var
  vJWT: TRALJWT;
  vKey: TRALJWSKey;
begin
  AReason := wmJWTInvalid;
  case FValidation of
    ovIntrospection:
      Result := Introspect(AToken, AClaims, AReason);
    ovJWKS:
    begin
      { the kid of the token picks the key of the issuer's set }
      vJWT := TRALJWT.Create;
      try
        vJWT.Token := AToken;
        vKey := RemoteKey(vJWT.Header.KeyID);
      finally
        vJWT.Free;
      end;
      Result := (vKey <> nil) and VerifyJWT(AToken, vKey, AClaims, AReason);
    end;
  else
    Result := VerifyJWT(AToken, SigningKey, AClaims, AReason);
  end;
end;

function TRALServerOAuth2.CheckClient(ARequest: TRALRequest; AResponse: TRALResponse;
  out AClientID: StringRAL): boolean;
var
  vSecret: StringRAL;
  vBasic: boolean;
  vAuth: TRALAuthBasic;
begin
  Result := False;
  vBasic := ARequest.Authorization.AuthType = ratBasic;
  if vBasic then
  begin
    { client_secret_basic form-encodes both before the base64 (RFC 6749
      2.3.1): a secret with ':' or '%' only survives that way }
    vAuth := ARequest.Authorization.AsAuthBasic;
    AClientID := TRALHTTPCoder.DecodeURL(vAuth.UserName);
    vSecret := TRALHTTPCoder.DecodeURL(vAuth.Password);
  end
  else
  begin
    AClientID := FieldOf(ARequest, 'client_id');
    vSecret := FieldOf(ARequest, 'client_secret');
  end;

  if (AClientID <> '') and Assigned(FOnValidateClient) then
    FOnValidateClient(ARequest, AClientID, vSecret, Result);

  if not Result then
    AnswerError(AResponse, HTTP_Unauthorized, 'invalid_client', wmOAuth2InvalidClient,
      vBasic);
end;

function TRALServerOAuth2.CheckRedirect(const AClientID, ARedirectURI: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  if Assigned(FOnValidateRedirect) then
  begin
    FOnValidateRedirect(AClientID, ARedirectURI, Result);
    Exit;
  end;
  { exact match only: a prefix match is how redirect_uri checks get bypassed }
  for vInt := 0 to Pred(FRedirectURIs.Count) do
    if (FRedirectURIs.Names[vInt] = AClientID) and
       (FRedirectURIs.ValueFromIndex[vInt] = ARedirectURI) then
      Exit(True);
end;

function TRALServerOAuth2.CheckScope(ARequest: TRALRequest; const AClientID,
  ASubject: StringRAL; var AScope: StringRAL): boolean;
begin
  Result := True;
  if Assigned(FOnValidateScope) then
    FOnValidateScope(ARequest, AClientID, ASubject, AScope, Result);
end;

function TRALServerOAuth2.Fetch(const AURL: StringRAL; AFields: TStrings;
  const AUser, APassword: StringRAL; out AStatus: IntegerRAL): StringRAL;
const
  cPreferred: array[0..3] of StringRAL = (ENGINEINDY, ENGINESYNOPSE, ENGINENETHTTP,
    ENGINEFPHTTP);
var
  vClient: TRALClient;
  vResponse: TRALResponse;
  vEngines: TStringList;
  vEngine: StringRAL;
  vInt: IntegerRAL;
begin
  Result := '';
  AStatus := 0;
  vEngine := FFetchEngine;
  if vEngine = '' then
  begin
    vEngines := TStringList.Create;
    try
      GetEngineList(vEngines);
      for vInt := 0 to High(cPreferred) do
        if vEngines.IndexOf(cPreferred[vInt]) >= 0 then
        begin
          vEngine := cPreferred[vInt];
          Break;
        end;
      if (vEngine = '') and (vEngines.Count > 0) then
        vEngine := vEngines.Strings[0];
    finally
      vEngines.Free;
    end;
  end;

  vClient := TRALClient.Create(nil);
  vResponse := nil;
  try
    vClient.EngineType := vEngine;
    if AUser <> '' then
      vClient.Request.AddHeader('Authorization', 'Basic ' + TRALBase64.Encode(
        TRALHTTPCoder.EncodeURL(AUser) + ':' + TRALHTTPCoder.EncodeURL(APassword)));
    if AFields <> nil then
    begin
      for vInt := 0 to Pred(AFields.Count) do
        vClient.Request.Params.AddParam(AFields.Names[vInt], AFields.ValueFromIndex[vInt],
          rpkFIELD);
      vClient.Post(AURL, vResponse);
    end
    else
      vClient.Get(AURL, vResponse);
    if vResponse <> nil then
    begin
      AStatus := vResponse.StatusCode;
      Result := vResponse.ResponseText;
    end;
  finally
    vResponse.Free;
    vClient.Free;
  end;
end;

function TRALServerOAuth2.GetAuthRoute: TRALBaseRoute;
begin
  { several routes, not one: ResolveRoute is overridden }
  Result := nil;
end;

function TRALServerOAuth2.GetPath(AIndex: IntegerRAL): StringRAL;
begin
  case AIndex of
    0: Result := FTokenRoute.Route;
    1: Result := FAuthorizeRoute.Route;
    2: Result := FIntrospectRoute.Route;
    3: Result := FJWKSRoute.Route;
    4: Result := FMetadataRoute.Route;
  else
    Result := FRevokeRoute.Route;
  end;
end;

function TRALServerOAuth2.Introspect(const AToken: StringRAL; AClaims: TRALJWTParams;
  out AReason: StringRAL): boolean;
var
  vKey, vText: StringRAL;
  vInt, vStatus: IntegerRAL;
  vFields: TStringList;
  vJson: TRALJSONValue;
  vValue: TRALJSONValue;
  vItem: TObject;
  vUntil: TDateTime;
begin
  Result := False;
  AReason := wmJWTInvalid;
  vKey := TokenKey(AToken);
  vText := '';

  FLock.Acquire;
  try
    vInt := FIntrospectCache.IndexOf(vKey);
    if vInt >= 0 then
    begin
      vItem := FIntrospectCache.Objects[vInt];
      if TStringList(vItem).Values['until'] <> '' then
      begin
        if StrToFloatDef(TStringList(vItem).Values['until'], 0) > Now then
          vText := TStringList(vItem).Values['json']
        else
        begin
          vItem.Free;
          FIntrospectCache.Delete(vInt);
        end;
      end;
    end;
  finally
    FLock.Release;
  end;

  if vText = '' then
  begin
    vFields := TStringList.Create;
    try
      vFields.Add('token=' + AToken);
      vFields.Add('token_type_hint=access_token');
      vText := Fetch(FIntrospectionURL, vFields, FIntrospectionClientID,
        FIntrospectionClientSecret, vStatus);
    finally
      vFields.Free;
    end;
    if vStatus <> HTTP_OK then
      Exit;
    { StringList as a record: the answer and when it stops being reused }
    vUntil := IncSecond(Now, FIntrospectCacheSecs);
    vFields := TStringList.Create;
    vFields.Values['json'] := vText;
    vFields.Values['until'] := FloatToStr(vUntil);
    FLock.Acquire;
    try
      if FIntrospectCache.Count > 1000 then
      begin
        for vInt := 0 to Pred(FIntrospectCache.Count) do
          FIntrospectCache.Objects[vInt].Free;
        FIntrospectCache.Clear;
      end;
      vInt := FIntrospectCache.IndexOf(vKey);
      if vInt >= 0 then
      begin
        FIntrospectCache.Objects[vInt].Free;
        FIntrospectCache.Delete(vInt);
      end;
      FIntrospectCache.AddObject(vKey, vFields);
    finally
      FLock.Release;
    end;
  end;

  vJson := nil;
  try
    try
      vJson := TRALJSON.ParseJSON(vText);
    except
      Exit;
    end;
    if not (vJson is TRALJSONObject) then
      Exit;
    vValue := TRALJSONObject(vJson).Get('active');
    if (vValue = nil) or (not vValue.AsBoolean) then
    begin
      AReason := wmOAuth2Revoked;
      Exit;
    end;
    AClaims.AsJSON := vText;
    if (AClaims.Expiration > 0) and (IncSecond(AClaims.Expiration, FClockSkew) < Now) then
    begin
      AReason := wmJWTExpired;
      Exit;
    end;
    if (FAudience <> '') and not AClaims.HasAudience(FAudience) then
      Exit;
    if (FIssuer <> '') and (AClaims.Issuer <> '') and (AClaims.Issuer <> FIssuer) then
      Exit;
    Result := True;
  finally
    vJson.Free;
  end;
end;

function TRALServerOAuth2.IssueAccessToken(ARequest: TRALRequest; const AClientID,
  ASubject, AScope: StringRAL): StringRAL;
var
  vJWT: TRALJWT;
  vKey: TRALJWSKey;
begin
  vJWT := TRALJWT.Create;
  try
    vJWT.Header.Algorithm := FAlgorithm;
    vJWT.SignSecretKey := FSignSecretKey;
    vKey := SigningKey;
    vJWT.SignKey := vKey;
    if FKeyID <> '' then
      vJWT.Header.KeyID := FKeyID
    else if vKey <> nil then
      vJWT.Header.KeyID := vKey.KeyID;

    if FIssuer <> '' then
      vJWT.Payload.Issuer := FIssuer;
    if FAudience <> '' then
      vJWT.Payload.Audience := FAudience;
    vJWT.Payload.Subject := ASubject;
    vJWT.Payload.IssuedAt := Now;
    vJWT.Payload.Expiration := IncSecond(Now, FAccessTokenLifetime);
    vJWT.Payload.createNewId;
    vJWT.Payload.AddClaim('client_id', AClientID);
    if AScope <> '' then
      vJWT.Payload.AddClaim('scope', AScope);
    if Assigned(FOnClaims) then
      FOnClaims(ARequest, AClientID, ASubject, AScope, vJWT.Payload);

    Result := vJWT.Token;
  finally
    vJWT.Free;
  end;
end;

class function TRALServerOAuth2.PKCEChallenge(const AVerifier: StringRAL): StringRAL;
var
  vHash: TRALSHA2_32;
begin
  vHash := TRALSHA2_32.Create;
  try
    vHash.Version := rsv256;
    vHash.OutputType := rhotBase64Url;
    Result := vHash.HashAsString(AVerifier);
  finally
    vHash.Free;
  end;
end;

function TRALServerOAuth2.RemoteKey(const AKeyID: StringRAL): TRALJWSKey;
var
  vText: StringRAL;
  vStatus: IntegerRAL;
  vNew: TRALJWKSet;
  vStale: boolean;
begin
  FLock.Acquire;
  try
    Result := nil;
    if FRemoteKeys <> nil then
      Result := FRemoteKeys.Find(AKeyID);
    { the set is read again after an hour, or sooner for a kid it does not
      have - keys rotate - but not more than once a minute: a token with an
      invented kid must not make every request fetch the set }
    vStale := (FRemoteKeys = nil) or (Now > IncHour(FRemoteKeysAt, 1)) or
      ((Result = nil) and (Now > IncSecond(FRemoteKeysAt, 60)));
    if not vStale then
      Exit;
    FRemoteKeysAt := Now;
  finally
    FLock.Release;
  end;

  vText := Fetch(FJWKSURL, nil, '', '', vStatus);
  if vStatus <> HTTP_OK then
    Exit;
  vNew := TRALJWKSet.Create;
  try
    vNew.LoadFromJSON(vText);
  except
    vNew.Free;
    Exit;
  end;

  FLock.Acquire;
  try
    { the old set may be in the hands of a request verifying right now: it is
      retired, not freed }
    if FRemoteKeys <> nil then
      FRetired.Add(FRemoteKeys);
    FRemoteKeys := vNew;
    Result := FRemoteKeys.Find(AKeyID);
  finally
    FLock.Release;
  end;
end;

function TRALServerOAuth2.RequiredScope(ARequest: TRALRequest): StringRAL;
var
  vRoute: TRALRoute;
begin
  Result := '';
  if FRouteScopes.Count = 0 then
    Exit;
  vRoute := TRALRoute(ARequest.ResolvedRoute);
  if vRoute <> nil then
    Result := Trim(FRouteScopes.Values[FixRoute(vRoute.GetFullRoute)]);
end;

function TRALServerOAuth2.ResolveRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
var
  vInt: IntegerRAL;
  vQuery: StringRAL;
begin
  Result := nil;
  if not FIssuing then
    Exit;
  vQuery := FixRoute(ARequest.Query);
  for vInt := 0 to Pred(FRoutes.Count) do
    if RALSameName(FixRoute(TRALRoute(FRoutes.Items[vInt]).Route), vQuery) then
      Exit(TRALRoute(FRoutes.Items[vInt]));
end;

procedure TRALServerOAuth2.SetPath(AIndex: IntegerRAL; const AValue: StringRAL);
begin
  case AIndex of
    0: FTokenRoute.Route := AValue;
    1: FAuthorizeRoute.Route := AValue;
    2: FIntrospectRoute.Route := AValue;
    3: FJWKSRoute.Route := AValue;
    4: FMetadataRoute.Route := AValue;
  else
    FRevokeRoute.Route := AValue;
  end;
end;

procedure TRALServerOAuth2.SetPrivateKey(AValue: TStrings);
begin
  FLock.Acquire;
  try
    FPrivateKey.Assign(AValue);
    { read again on the next use; the old key may be signing right now }
    if FKey <> nil then
      FRetired.Add(FKey);
    FKey := nil;
  finally
    FLock.Release;
  end;
end;

procedure TRALServerOAuth2.SetRedirectURIs(AValue: TStrings);
begin
  FRedirectURIs.Assign(AValue);
end;

procedure TRALServerOAuth2.SetRouteScopes(AValue: TStrings);
begin
  FRouteScopes.Assign(AValue);
end;

procedure TRALServerOAuth2.SetStore(AValue: TRALOAuth2Store);
begin
  if (AValue = nil) or (AValue = FStore) then
    Exit;
  FStore.Free;
  FStore := AValue;
end;

function TRALServerOAuth2.SigningKey: TRALJWSKey;
begin
  Result := nil;
  if not (FAlgorithm in [tjaRS256, tjaRS384, tjaRS512, tjaES256, tjaES384]) then
    Exit;
  FLock.Acquire;
  try
    { PrivateKey is a TStrings: changing its Text never goes through the
      setter, so the key is compared with the text it was read from }
    if (FKey <> nil) and (FKeyText <> StringRAL(FPrivateKey.Text)) then
    begin
      FRetired.Add(FKey);
      FKey := nil;
    end;
    if (FKey = nil) and (Trim(FPrivateKey.Text) <> '') then
    begin
      FKey := TRALJWSKey.Create;
      try
        FKey.LoadPrivateKeyPEM(StringRAL(FPrivateKey.Text));
        FKeyText := StringRAL(FPrivateKey.Text);
      except
        FreeAndNil(FKey);
        raise;
      end;
    end;
    Result := FKey;
  finally
    FLock.Release;
  end;
end;

procedure TRALServerOAuth2.Validate(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vClaims: TRALJWTParams;
  vReason, vScope, vChallenge: StringRAL;
  vValid: boolean;
begin
  AResponse.StatusCode := HTTP_OK;
  if ARequest.Authorization.AuthType <> ratBearer then
  begin
    AResponse.Answer(HTTP_Unauthorized);
    RALAddChallenge(AResponse, 'Bearer realm="RAL"');
    Exit;
  end;

  vClaims := TRALJWTParams.Create;
  try
    vValid := CheckAccessToken(ARequest.Authorization.AuthString, vClaims, vReason);
    if vValid and Assigned(FOnValidateToken) then
    begin
      FOnValidateToken(ARequest, AResponse, vClaims, vValid);
      if not vValid then
        vReason := wmJWTRefused;
    end;
    if not vValid then
    begin
      if AResponse.StatusCode < HTTP_BadRequest then
        AResponse.Answer(HTTP_Unauthorized);
      RALAddChallenge(AResponse, 'Bearer realm="RAL", error="invalid_token"' +
        ', error_description="' + RALChallengeText(vReason) + '"');
      Exit;
    end;

    { RFC 6750 3.1: a good token without the scope the route needs is 403,
      and the challenge names the scope }
    vScope := RequiredScope(ARequest);
    if (vScope <> '') and not HasScopes(vClaims.GetClaim('scope'), vScope) then
    begin
      AResponse.Answer(HTTP_Forbidden);
      vChallenge := 'Bearer realm="RAL", error="insufficient_scope", scope=' +
        RALQuoteString(vScope) + ', error_description="' +
        RALChallengeText(wmOAuth2Scope) + '"';
      RALAddChallenge(AResponse, vChallenge);
    end;
  finally
    vClaims.Free;
  end;
end;

function TRALServerOAuth2.VerifyJWT(const AToken: StringRAL; AKey: TRALJWSKey;
  AClaims: TRALJWTParams; out AReason: StringRAL): boolean;
var
  vJWT: TRALJWT;
begin
  Result := False;
  AReason := wmJWTInvalid;
  vJWT := TRALJWT.Create;
  try
    vJWT.Header.Algorithm := FAlgorithm;
    vJWT.SignSecretKey := FSignSecretKey;
    vJWT.SignKey := AKey;
    vJWT.Leeway := FClockSkew;
    if not vJWT.IsValidToken(AToken) then
    begin
      if vJWT.IsValidSignature then
      begin
        if (vJWT.Payload.Expiration > 0) and (vJWT.Payload.Expiration < Now) then
          AReason := wmJWTExpired
        else if (vJWT.Payload.NotBefore > 0) and (vJWT.Payload.NotBefore > Now) then
          AReason := wmJWTNotYetValid;
      end;
      Exit;
    end;
    if (FIssuer <> '') and (vJWT.Payload.Issuer <> FIssuer) then
      Exit;
    if (FAudience <> '') and not vJWT.Payload.HasAudience(FAudience) then
      Exit;
    if FStore.IsRevoked(vJWT.Payload.Id) then
    begin
      AReason := wmOAuth2Revoked;
      Exit;
    end;
    AClaims.AsJSON := vJWT.Payload.AsJSON;
    Result := True;
  finally
    vJWT.Free;
  end;
end;

{ TRALClientOAuth2 }

constructor TRALClientOAuth2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetAuthType(ratOAuth2);
  FClientAuth := ocaSecretBasic;
  FGrantType := ogtClientCredentials;
  FRefreshMargin := 30;
  FTokenURL := '/oauth/token';
  FExpiresAt := 0;
end;

function TRALClientOAuth2.AuthorizationURL(const AAuthorizeURL, ARedirectURI: StringRAL;
  out AState: StringRAL): StringRAL;
var
  vSep: StringRAL;
begin
  Lock;
  try
    FPendingState := RandomText(16);
    { 32 random bytes are 43 base64url characters: the least RFC 7636 4.1
      allows, and all of it entropy }
    FPendingVerifier := RandomText(32);
    FPendingRedirect := ARedirectURI;
    FPendingCode := '';
    AState := FPendingState;

    if Pos(StringRAL('?'), AAuthorizeURL) > 0 then
      vSep := '&'
    else
      vSep := '?';
    Result := AAuthorizeURL + vSep + 'response_type=code' +
      '&client_id=' + TRALHTTPCoder.EncodeURL(FClientID) +
      '&redirect_uri=' + TRALHTTPCoder.EncodeURL(ARedirectURI) +
      '&state=' + TRALHTTPCoder.EncodeURL(FPendingState) +
      '&code_challenge=' + TRALServerOAuth2.PKCEChallenge(FPendingVerifier) +
      '&code_challenge_method=S256';
    if FScope <> '' then
      Result := Result + '&scope=' + TRALHTTPCoder.EncodeURL(FScope);
  finally
    Unlock;
  end;
end;

procedure TRALClientOAuth2.ClearTokens;
begin
  Lock;
  try
    FAccessToken := '';
    FRefreshToken := '';
    FExpiresAt := 0;
    FTokenScope := '';
  finally
    Unlock;
  end;
end;

function TRALClientOAuth2.GetAccessToken: StringRAL;
begin
  Lock;
  try
    Result := FAccessToken;
  finally
    Unlock;
  end;
end;

function TRALClientOAuth2.GetRefreshToken: StringRAL;
begin
  Lock;
  try
    Result := FRefreshToken;
  finally
    Unlock;
  end;
end;

function TRALClientOAuth2.HandleChallenge(AResponse: TRALResponse): boolean;
begin
  FAccessToken := '';
  FExpiresAt := 0;
  Result := True;
end;

function TRALClientOAuth2.IsAuthenticated: boolean;
begin
  Lock;
  try
    Result := (FAccessToken <> '') and
      ((FExpiresAt = 0) or (Now < IncSecond(FExpiresAt, -FRefreshMargin)));
  finally
    Unlock;
  end;
end;

function TRALClientOAuth2.Prepare(ATransport: TRALAuthTransport; AVars: TStringList;
  AResponse: TRALResponse): IntegerRAL;
var
  vFields: TStringList;
  vError: StringRAL;
begin
  Result := 0;
  if IsAuthenticated then
    Exit;

  vFields := TStringList.Create;
  try
    { the refresh token first: it costs the provider nothing to check, and a
      user who logged in once is not sent back to the browser }
    if FRefreshToken <> '' then
    begin
      vFields.Add('grant_type=refresh_token');
      vFields.Add('refresh_token=' + FRefreshToken);
      Result := RequestToken(ATransport, vFields, AResponse, vError);
      if (Result <> 0) or (vError = '') then
        Exit; // a token, or a transport failure already written
      { refused - expired, revoked: forgotten, and the full grant follows }
      FRefreshToken := '';
      vFields.Clear;
    end;

    if FPendingCode <> '' then
    begin
      vFields.Add('grant_type=authorization_code');
      vFields.Add('code=' + FPendingCode);
      vFields.Add('redirect_uri=' + FPendingRedirect);
      vFields.Add('code_verifier=' + FPendingVerifier);
      { a code is single use: whatever the answer, it is spent }
      FPendingCode := '';
    end
    else if FGrantType = ogtClientCredentials then
    begin
      vFields.Add('grant_type=client_credentials');
      if FScope <> '' then
        vFields.Add('scope=' + FScope);
    end
    else
    begin
      ATransport.Fail(AResponse, emOAuth2NoToken);
      Result := -1;
      Exit;
    end;

    Result := RequestToken(ATransport, vFields, AResponse, vError);
    if (Result = 0) and (vError <> '') then
    begin
      ATransport.Fail(AResponse, Format(emOAuth2TokenFailed, [vError]));
      Result := -1;
    end;
  finally
    vFields.Free;
  end;
end;

function TRALClientOAuth2.RequestToken(ATransport: TRALAuthTransport; AFields: TStrings;
  AResponse: TRALResponse; out AError: StringRAL): IntegerRAL;
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vJson: TRALJSONValue;
  vValue: TRALJSONValue;
  vInt: IntegerRAL;
  vText, vAccess: StringRAL;

  function Member(const AName: StringRAL): StringRAL;
  begin
    Result := '';
    vValue := TRALJSONObject(vJson).Get(AName);
    if (vValue <> nil) and (not vValue.IsNull) then
      Result := vValue.AsString;
  end;

begin
  AError := '';
  vRequest := ATransport.NewRequest;
  vResponse := ATransport.NewResponse;
  try
    for vInt := 0 to Pred(AFields.Count) do
      vRequest.Params.AddParam(AFields.Names[vInt], AFields.ValueFromIndex[vInt], rpkFIELD);
    if FAudience <> '' then
      vRequest.Params.AddParam('audience', FAudience, rpkFIELD);

    { a public client (no secret) only names itself }
    if (FClientSecret <> '') and (FClientAuth = ocaSecretBasic) then
      vRequest.AddHeader('Authorization', 'Basic ' + TRALBase64.Encode(
        TRALHTTPCoder.EncodeURL(FClientID) + ':' + TRALHTTPCoder.EncodeURL(FClientSecret)))
    else
    begin
      vRequest.Params.AddParam('client_id', FClientID, rpkFIELD);
      if FClientSecret <> '' then
        vRequest.Params.AddParam('client_secret', FClientSecret, rpkFIELD);
    end;

    Result := ATransport.Send(ATransport.URL(FTokenURL), vRequest, vResponse, amPOST);
    if Result <> 0 then
    begin
      ATransport.FailWith(AResponse, vResponse);
      Exit;
    end;

    vText := vResponse.ResponseText;
    vJson := nil;
    try
      try
        vJson := TRALJSON.ParseJSON(vText);
      except
        vJson := nil;
      end;
      if not (vJson is TRALJSONObject) then
      begin
        AError := IntToStr(vResponse.StatusCode);
        Exit;
      end;

      vAccess := Member('access_token');
      if (vResponse.StatusCode <> HTTP_OK) or (vAccess = '') then
      begin
        { the provider's own words: error and error_description (RFC 6749
          5.2) are what the application shows }
        AError := Member('error');
        if Member('error_description') <> '' then
          AError := AError + ': ' + Member('error_description');
        if AError = '' then
          AError := emOAuth2NoToken;
        Exit;
      end;

      FAccessToken := vAccess;
      vInt := StrToIntDef(Member('expires_in'), 0);
      if vInt > 0 then
        FExpiresAt := IncSecond(Now, vInt)
      else
        FExpiresAt := 0;
      { rotation: a new refresh token replaces the old; none keeps the old }
      if Member('refresh_token') <> '' then
        FRefreshToken := Member('refresh_token');
      FTokenScope := Member('scope');
    finally
      vJson.Free;
    end;
  finally
    vRequest.Free;
    vResponse.Free;
  end;
end;

procedure TRALClientOAuth2.SetAuthHeader(AVars: TStringList; AParams: TRALParams);
var
  vToken: StringRAL;
begin
  vToken := GetAccessToken;
  if vToken <> '' then
    AParams.AddParam('Authorization', 'Bearer ' + vToken, rpkHEADER);
end;

procedure TRALClientOAuth2.SetAuthorizationCode(const ACode, AState: StringRAL);
begin
  Lock;
  try
    if (FPendingState = '') or not RALSameSecret(AState, FPendingState) then
      raise Exception.Create(emOAuth2StateMismatch);
    FPendingState := '';
    FPendingCode := ACode;
    FAccessToken := '';
    FExpiresAt := 0;
  finally
    Unlock;
  end;
end;

procedure TRALClientOAuth2.SetRefreshToken(const AValue: StringRAL);
begin
  Lock;
  try
    FRefreshToken := AValue;
  finally
    Unlock;
  end;
end;

{ TRALOAuth2Loopback }

constructor TRALOAuth2Loopback.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPath := '/callback';
  FEvent := TEvent.Create(nil, True, False, '');
  FRoute := nil;
  FServer := nil;
end;

destructor TRALOAuth2Loopback.Destroy;
begin
  SetServer(nil);
  FreeAndNil(FEvent);
  inherited Destroy;
end;

procedure TRALOAuth2Loopback.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FServer) then
  begin
    FServer := nil;
    FRoute := nil;
  end;
  inherited;
end;

function TRALOAuth2Loopback.RedirectURI: StringRAL;
var
  vPort: IntegerRAL;
begin
  vPort := DEFAULTSERVERPORT;
  if FServer <> nil then
    vPort := FServer.Port;
  Result := 'http://127.0.0.1:' + IntToStr(vPort) + FixRoute(FPath);
end;

procedure TRALOAuth2Loopback.Reply(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  FCode := FieldOf(ARequest, 'code');
  FState := FieldOf(ARequest, 'state');
  FError := FieldOf(ARequest, 'error');
  AResponse.Answer(HTTP_OK, '<!DOCTYPE html><html><body><p>' + RALPACKAGESHORT +
    '</p></body></html>', rctTEXTHTML);
  FEvent.SetEvent;
end;

procedure TRALOAuth2Loopback.SetServer(AValue: TRALServer);
begin
  if AValue = FServer then
    Exit;
  if FServer <> nil then
  begin
    if FRoute <> nil then
      FRoute.Free;
    FRoute := nil;
    FServer.RemoveFreeNotification(Self);
  end;
  FServer := AValue;
  if FServer <> nil then
    FServer.FreeNotification(Self);
end;

function TRALOAuth2Loopback.WaitForCode(ATimeout: IntegerRAL; out ACode, AState,
  AError: StringRAL): boolean;
var
  vActive: boolean;
begin
  ACode := '';
  AState := '';
  AError := '';
  Result := False;
  if FServer = nil then
    Exit;

  if FRoute = nil then
  begin
    FRoute := FServer.CreateRoute(FPath, {$IFDEF FPC}@{$ENDIF}Reply);
    FRoute.AllowedMethods := [amGET];
    FRoute.SkipAuthMethods := [amALL];
  end;

  FCode := '';
  FState := '';
  FError := '';
  FEvent.ResetEvent;
  vActive := FServer.Active;
  FServer.Active := True;
  try
    if FEvent.WaitFor(ATimeout) <> wrSignaled then
      Exit;
    ACode := FCode;
    AState := FState;
    AError := FError;
    Result := (FCode <> '') and (FError = '');
  finally
    FServer.Active := vActive;
  end;
end;

end.
