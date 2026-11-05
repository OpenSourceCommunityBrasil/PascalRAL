/// OpenID Connect: logging users in through an identity provider (Google,
/// Microsoft Entra ID, Keycloak, Auth0, Okta or any OIDC issuer), on the
/// server (TRALOIDCLogin, authorization code + PKCE) and on a desktop or
/// console client (TRALClientOIDCAuth, device flow of RFC 8628 or the
/// browser with the loopback redirect of RFC 8252).
unit RALOIDC;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, DateUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALBase64, RALJson, RALToken, RALJWS, RALUrlCoder,
  RALMIMETypes, RALRoutes, RALRequest, RALResponse, RALParams, RALCustomObjects,
  RALPlugin, RALAuthBase, RALKeyValueStore, RALAuthentication, RALOAuth2;

type
  /// Known identity providers: what changes between them is the issuer and the roles claim.
  TRALOIDCProvider = (opGeneric, opGoogle, opMicrosoft, opKeycloak, opAuth0, opOkta);

  { The endpoints of an issuer, read from its discovery document
    (/.well-known/openid-configuration), kept for CacheMinutes. }
  TRALOIDCDiscovery = class
  private
    FAuthorizationEndpoint: StringRAL;
    FDeviceEndpoint: StringRAL;
    FEndSessionEndpoint: StringRAL;
    FIssuer: StringRAL;
    FJwksURI: StringRAL;
    FKeys: TRALJWKSet;
    FKeysAt: TDateTime;
    FLoadedAt: TDateTime;
    FLock: TCriticalSection;
    FRetired: TList;
    FTokenEndpoint: StringRAL;
    FUserinfoEndpoint: StringRAL;
  public
    constructor Create;
    destructor Destroy; override;

    { Reads the discovery document of AIssuer (or AURL when given) unless it was
      read less than ACacheMinutes ago; False when it could not be read. }
    function Load(const AIssuer, AURL, AEngine: StringRAL; ACacheMinutes: IntegerRAL): boolean;
    /// Fills the endpoints from a discovery document.
    procedure LoadFromJSON(const AJSON: StringRAL);
    { The key of the issuer named AKeyID, from its JWK set (fetched again for an
      unknown kid, at most once a minute); nil when there is none. }
    function Key(const AKeyID, AEngine: StringRAL): TRALJWSKey;

    /// authorization_endpoint.
    property AuthorizationEndpoint: StringRAL read FAuthorizationEndpoint write FAuthorizationEndpoint;
    /// device_authorization_endpoint (RFC 8628).
    property DeviceEndpoint: StringRAL read FDeviceEndpoint write FDeviceEndpoint;
    /// end_session_endpoint (RP-initiated logout).
    property EndSessionEndpoint: StringRAL read FEndSessionEndpoint write FEndSessionEndpoint;
    /// issuer, as the id_token must carry it.
    property Issuer: StringRAL read FIssuer write FIssuer;
    /// jwks_uri.
    property JwksURI: StringRAL read FJwksURI write FJwksURI;
    /// token_endpoint.
    property TokenEndpoint: StringRAL read FTokenEndpoint write FTokenEndpoint;
    /// userinfo_endpoint.
    property UserinfoEndpoint: StringRAL read FUserinfoEndpoint write FUserinfoEndpoint;
  end;

  TRALOIDCLogin = class;

  { A user logged in: AClaims are the id_token's (checked), AIdentity is filled
    from them. Set AAllow to False to refuse (403), or change ARedirect. }
  TRALOnOIDCLogin = procedure(Sender: TRALOIDCLogin; ARequest: TRALRequest;
    AClaims: TRALJWTParams; AIdentity: TRALIdentity; var AAllow: boolean;
    var ARedirect: StringRAL) of object;
  /// The login failed: AError is the provider's error or RAL's reason.
  TRALOnOIDCError = procedure(Sender: TRALOIDCLogin; ARequest: TRALRequest;
    AResponse: TRALResponse; const AError: StringRAL) of object;

  { Login of a web application through an OpenID Connect provider, as a plugin
    with three routes of its own:
    - LoginPath (GET, ?return=/page): sends the browser to the provider, with
      state, nonce and PKCE S256 kept in Store for ten minutes;
    - CallbackPath: the provider's answer. The state is used once, the code is
      exchanged at the token endpoint, the id_token is checked (signature with
      the issuer's JWK set, iss, aud, azp, exp, nonce), OnLogin decides, and
      Session (a TRALServerJWTAuth with UseCookie) issues the application's
      own token in the raltoken cookie; then back to the page;
    - LogoutPath: drops the cookie and, when the provider has one, goes
      through its end_session_endpoint.
    The routes need no authentication; the rest of the server is protected by
    Session as usual. }
  TRALOIDCLogin = class(TRALPlugin)
  private
    FCacheMinutes: IntegerRAL;
    FCallbackRoute: TRALRoute;
    FClientID: StringRAL;
    FClientSecret: StringRAL;
    FDiscovery: TRALOIDCDiscovery;
    FDiscoveryURL: StringRAL;
    FFetchEngine: StringRAL;
    FIssuer: StringRAL;
    FLoginRoute: TRALRoute;
    FLogoutRoute: TRALRoute;
    FOwnStore: TRALMemoryStore;
    FPostLoginRedirect: StringRAL;
    FPostLogoutRedirect: StringRAL;
    FProvider: TRALOIDCProvider;
    FRedirectURI: StringRAL;
    FRolesClaim: StringRAL;
    FRoutes: TOwnedCollection;
    FScope: StringRAL;
    FSession: TRALServerJWTAuth;
    FStore: TRALKeyValueStore;
    FOnError: TRALOnOIDCError;
    FOnLogin: TRALOnOIDCLogin;

    procedure AnswerCallback(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure AnswerLogin(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure AnswerLogout(ARequest: TRALRequest; AResponse: TRALResponse);
    { refuses the login: OnError, or a 400/403 page with AError }
    procedure Fail(ARequest: TRALRequest; AResponse: TRALResponse; AStatus: IntegerRAL;
      const AError: StringRAL);
    function GetPath(AIndex: IntegerRAL): StringRAL;
    { the redirect_uri: RedirectURI, else the callback on the host the browser used }
    function RedirectFor(ARequest: TRALRequest): StringRAL;
    procedure SetPath(AIndex: IntegerRAL; const AValue: StringRAL);
    procedure SetProvider(AValue: TRALOIDCProvider);
    procedure SetSession(const AValue: TRALServerJWTAuth);
    procedure SetStore(const AValue: TRALKeyValueStore);
    function UsedStore: TRALKeyValueStore;
  protected
    class function DefaultPriority: IntegerRAL; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Checks AIdToken as the callback does: signature with the issuer's keys (or
      ClientSecret for HS*), iss, aud, azp, exp and, when ANonce is given, the
      nonce. Fills AClaims; AReason says why not. }
    function CheckIdToken(const AIdToken, ANonce: StringRAL; AClaims: TRALJWTParams;
      out AReason: StringRAL): boolean;
    /// Reads the discovery document now; False when the issuer did not answer.
    function Discover: boolean;
    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
    function ResolveRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute; override;

    /// The endpoints of the issuer (after Discover or the first login).
    property Discovery: TRALOIDCDiscovery read FDiscovery;
  published
    /// Minutes the discovery document is kept.
    property CacheMinutes: IntegerRAL read FCacheMinutes write FCacheMinutes default 60;
    /// Path of the provider's answer; the redirect_uri registered with it.
    property CallbackPath: StringRAL index 1 read GetPath write SetPath;
    /// Client id the provider gave the application.
    property ClientID: StringRAL read FClientID write FClientID;
    /// Client secret (confidential client); empty for a public one.
    property ClientSecret: StringRAL read FClientSecret write FClientSecret;
    /// URL of the discovery document, when it is not Issuer + /.well-known/openid-configuration.
    property DiscoveryURL: StringRAL read FDiscoveryURL write FDiscoveryURL;
    /// Client engine of the requests to the provider; empty = the first linked.
    property FetchEngine: StringRAL read FFetchEngine write FFetchEngine;
    { The issuer (https://accounts.google.com, https://login.microsoftonline.com/
      <tenant>/v2.0, https://<host>/realms/<realm>...). }
    property Issuer: StringRAL read FIssuer write FIssuer;
    /// Path that starts the login.
    property LoginPath: StringRAL index 0 read GetPath write SetPath;
    /// Path that logs out.
    property LogoutPath: StringRAL index 2 read GetPath write SetPath;
    /// Where the browser goes after the login when it brought no return.
    property PostLoginRedirect: StringRAL read FPostLoginRedirect write FPostLoginRedirect;
    /// Where the provider sends the browser after the logout (absolute URL); empty = none.
    property PostLogoutRedirect: StringRAL read FPostLogoutRedirect write FPostLogoutRedirect;
    /// Known provider: sets RolesClaim (and nothing else).
    property Provider: TRALOIDCProvider read FProvider write SetProvider default opGeneric;
    /// redirect_uri sent to the provider; empty = the callback on the host of the request.
    property RedirectURI: StringRAL read FRedirectURI write FRedirectURI;
    { Claims the roles come from (separated by commas; a dotted name walks into
      an object: realm_access.roles for Keycloak). }
    property RolesClaim: StringRAL read FRolesClaim write FRolesClaim;
    /// Scopes asked for; openid is always sent.
    property Scope: StringRAL read FScope write FScope;
    { The JWT authenticator that issues the application's token after the login
      (in its raltoken cookie: set its UseCookie). Empty = OnLogin does it. }
    property Session: TRALServerJWTAuth read FSession write SetSession;
    /// Where state, nonce and PKCE wait for the answer; empty = in memory (one server).
    property Store: TRALKeyValueStore read FStore write SetStore;
    /// The login failed.
    property OnError: TRALOnOIDCError read FOnError write FOnError;
    /// A user logged in; may refuse or change the page to go to.
    property OnLogin: TRALOnOIDCLogin read FOnLogin write FOnLogin;
  end;

  { The answer of a device authorization (RFC 8628): show UserCode and
    VerificationURI to the user, then wait. }
  TRALOIDCDeviceCode = record
    /// The code the client polls with.
    DeviceCode: StringRAL;
    /// Seconds before the codes expire.
    ExpiresIn: IntegerRAL;
    /// Seconds between two polls.
    Interval: IntegerRAL;
    /// The code the user types.
    UserCode: StringRAL;
    /// The page where the user types it.
    VerificationURI: StringRAL;
    /// The same page with the code already in it (for a QR code), when given.
    VerificationURIComplete: StringRAL;
  end;

  { An OpenID Connect client: the OAuth2 client (refresh, code with PKCE) whose
    endpoints come from the issuer's discovery document, with the id_token kept
    and checked, and the device flow for programs without a browser. }
  TRALClientOIDCAuth = class(TRALClientOAuth2)
  private
    FDiscovery: TRALOIDCDiscovery;
    FFetchEngine: StringRAL;
    FIdClaims: TRALJWTParams;
    FIdToken: StringRAL;
    FIssuer: StringRAL;
  protected
    procedure TokenReceived(AJson: TRALJSONObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Reads the discovery document and sets TokenURL; False when it could not.
    function Discover: boolean;
    { The authorization URL of the issuer for the browser flow (with state and
      PKCE kept here, as AuthorizationURL); empty when discovery failed. }
    function LoginURL(const ARedirectURI: StringRAL; out AState: StringRAL): StringRAL;
    { Starts the device flow: ADevice tells what to show the user. False, with
      AError, when the issuer has no device endpoint or refused. }
    function StartDeviceFlow(out ADevice: TRALOIDCDeviceCode; out AError: StringRAL): boolean;
    { Polls the token endpoint until the user approves, refuses or the code
      expires, or ATimeoutSec passes; True with the tokens taken. }
    function WaitDeviceFlow(const ADevice: TRALOIDCDeviceCode; ATimeoutSec: IntegerRAL;
      out AError: StringRAL): boolean;

    /// The endpoints of the issuer.
    property Discovery: TRALOIDCDiscovery read FDiscovery;
    /// The claims of IdToken, as the token said (not verified on the client).
    property IdClaims: TRALJWTParams read FIdClaims;
    /// The last id_token received.
    property IdToken: StringRAL read FIdToken;
  published
    /// Client engine of the discovery and device requests; empty = the first linked.
    property FetchEngine: StringRAL read FFetchEngine write FFetchEngine;
    /// The issuer, whose discovery document gives the endpoints.
    property Issuer: StringRAL read FIssuer write FIssuer;
  end;

/// The roles claim each provider uses.
function RALOIDCRolesClaim(AProvider: TRALOIDCProvider): StringRAL;
{ The value of AClaims under APath: a plain name, or a dotted path into an
  object ('realm_access.roles'); arrays and objects come as JSON text. }
function RALOIDCClaimPath(AClaims: TRALJWTParams; const APath: StringRAL): StringRAL;
/// Whether ARedirect is a local path (starts with one '/'): never another site.
function RALOIDCSafeRedirect(const ARedirect: StringRAL): boolean;

implementation

const
  cStatePrefix = 'oidc.state:';
  cStateTTL = 600000;

function RandomText(ABytes: IntegerRAL): StringRAL;
begin
  Result := RALBase64UrlEncode(RandomBytes(ABytes));
end;

{ AScope with openid as its first word, once }
function ScopeWithOpenID(const AScope: StringRAL): StringRAL;
var
  vList: TStringList;
  vInt: IntegerRAL;
begin
  Result := 'openid';
  vList := TStringList.Create;
  try
    vList.Text := StringReplace(string(AScope), ' ', sLineBreak, [rfReplaceAll]);
    for vInt := 0 to vList.Count - 1 do
      if (Trim(vList[vInt]) <> '') and (Trim(vList[vInt]) <> 'openid') then
        Result := Result + ' ' + StringRAL(Trim(vList[vInt]));
  finally
    vList.Free;
  end;
end;

function RALOIDCRolesClaim(AProvider: TRALOIDCProvider): StringRAL;
begin
  case AProvider of
    opMicrosoft: Result := 'roles,groups';
    opKeycloak: Result := 'realm_access.roles,groups';
    opOkta: Result := 'groups';
    opGoogle: Result := '';
  else
    Result := 'roles,groups';
  end;
end;

function RALOIDCSafeRedirect(const ARedirect: StringRAL): boolean;
begin
  Result := (Length(ARedirect) >= 1) and (ARedirect[POSINISTR] = '/') and
    ((Length(ARedirect) = 1) or not (ARedirect[POSINISTR + 1] in ['/', '\'])) and
    (Pos(StringRAL(#13), ARedirect) = 0) and (Pos(StringRAL(#10), ARedirect) = 0);
end;

function RALOIDCClaimPath(AClaims: TRALJWTParams; const APath: StringRAL): StringRAL;
var
  vDot: IntegerRAL;
  vJson, vValue: TRALJSONValue;
  vRest, vName: StringRAL;
begin
  vDot := Pos(StringRAL('.'), APath);
  if vDot = 0 then
  begin
    Result := AClaims.GetClaim(APath);
    Exit;
  end;
  Result := '';
  vJson := nil;
  try
    vJson := TRALJSON.ParseJSON(AClaims.GetClaim(Copy(APath, 1, vDot - 1)));
  except
    vJson := nil;
  end;
  try
    vValue := vJson;
    vRest := Copy(APath, vDot + 1, Length(APath));
    while (vValue <> nil) and (vRest <> '') do
    begin
      if not (vValue is TRALJSONObject) then
      begin
        vValue := nil;
        Break;
      end;
      vDot := Pos(StringRAL('.'), vRest);
      if vDot = 0 then
      begin
        vName := vRest;
        vRest := '';
      end
      else
      begin
        vName := Copy(vRest, 1, vDot - 1);
        vRest := Copy(vRest, vDot + 1, Length(vRest));
      end;
      vValue := TRALJSONObject(vValue).Get(vName);
    end;
    if vValue <> nil then
    begin
      if vValue.JsonType in [rjtObject, rjtArray] then
        Result := vValue.ToJSON
      else
        Result := vValue.AsString;
    end;
  finally
    vJson.Free;
  end;
end;

{ every role of the claims named in ARolesClaims, dotted paths included }
procedure FillRoles(AIdentity: TRALIdentity; AClaims: TRALJWTParams;
  const ARolesClaims: StringRAL);
var
  vList: TStringList;
  vInt: IntegerRAL;
begin
  vList := TStringList.Create;
  try
    vList.Text := StringReplace(string(ARolesClaims), ',', sLineBreak, [rfReplaceAll]);
    for vInt := 0 to vList.Count - 1 do
      if Trim(vList[vInt]) <> '' then
        AIdentity.AddRoles(RALRolesOfClaim(RALOIDCClaimPath(AClaims,
          StringRAL(Trim(vList[vInt])))));
  finally
    vList.Free;
  end;
end;

function JSONMember(AJson: TRALJSONValue; const AName: StringRAL): StringRAL;
var
  vValue: TRALJSONValue;
begin
  Result := '';
  if not (AJson is TRALJSONObject) then
    Exit;
  vValue := TRALJSONObject(AJson).Get(AName);
  if (vValue <> nil) and not vValue.IsNull then
    Result := vValue.AsString;
end;

function AlgorithmOf(const AName: StringRAL; out AAlg: TRALJWTAlgorithm): boolean;
var
  vAlg: TRALJWTAlgorithm;
begin
  Result := False;
  for vAlg := Low(TRALJWTAlgorithm) to High(TRALJWTAlgorithm) do
    if TRALJWT.AlgorithmName(vAlg) = AName then
    begin
      AAlg := vAlg;
      Result := True;
      Exit;
    end;
end;

{ TRALOIDCDiscovery }

constructor TRALOIDCDiscovery.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FRetired := TList.Create;
end;

destructor TRALOIDCDiscovery.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FRetired.Count - 1 do
    TObject(FRetired[vInt]).Free;
  FreeAndNil(FRetired);
  FreeAndNil(FKeys);
  FreeAndNil(FLock);
  inherited;
end;

procedure TRALOIDCDiscovery.LoadFromJSON(const AJSON: StringRAL);
var
  vJson: TRALJSONValue;
begin
  vJson := TRALJSON.ParseJSON(AJSON);
  try
    if not (vJson is TRALJSONObject) then
      raise Exception.Create(emOIDCDiscovery);
    FIssuer := JSONMember(vJson, 'issuer');
    FAuthorizationEndpoint := JSONMember(vJson, 'authorization_endpoint');
    FTokenEndpoint := JSONMember(vJson, 'token_endpoint');
    FUserinfoEndpoint := JSONMember(vJson, 'userinfo_endpoint');
    FJwksURI := JSONMember(vJson, 'jwks_uri');
    FEndSessionEndpoint := JSONMember(vJson, 'end_session_endpoint');
    FDeviceEndpoint := JSONMember(vJson, 'device_authorization_endpoint');
  finally
    vJson.Free;
  end;
end;

function TRALOIDCDiscovery.Load(const AIssuer, AURL, AEngine: StringRAL;
  ACacheMinutes: IntegerRAL): boolean;
var
  vURL, vText: StringRAL;
  vStatus: IntegerRAL;
begin
  FLock.Acquire;
  try
    Result := (FTokenEndpoint <> '') and (Now < IncMinute(FLoadedAt, ACacheMinutes));
    if Result then
      Exit;
  finally
    FLock.Release;
  end;

  vURL := AURL;
  if vURL = '' then
  begin
    vURL := AIssuer;
    while (vURL <> '') and (vURL[RALHighStr(vURL)] = '/') do
      Delete(vURL, RALHighStr(vURL), 1);
    vURL := vURL + '/.well-known/openid-configuration';
  end;
  vText := RALOAuth2Fetch(vURL, nil, '', '', AEngine, vStatus);
  Result := vStatus = HTTP_OK;
  if not Result then
    Exit;
  FLock.Acquire;
  try
    try
      LoadFromJSON(vText);
      FLoadedAt := Now;
    except
      Result := False;
    end;
  finally
    FLock.Release;
  end;
end;

function TRALOIDCDiscovery.Key(const AKeyID, AEngine: StringRAL): TRALJWSKey;
var
  vText: StringRAL;
  vStatus: IntegerRAL;
  vNew: TRALJWKSet;
  vStale: boolean;
begin
  FLock.Acquire;
  try
    Result := nil;
    if FKeys <> nil then
      Result := FKeys.Find(AKeyID);
    vStale := (FKeys = nil) or (Now > IncHour(FKeysAt, 1)) or
      ((Result = nil) and (Now > IncSecond(FKeysAt, 60)));
    if (not vStale) or (FJwksURI = '') then
      Exit;
    FKeysAt := Now;
  finally
    FLock.Release;
  end;

  vText := RALOAuth2Fetch(FJwksURI, nil, '', '', AEngine, vStatus);
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
    { a set may be in use by another request: retired, not freed }
    if FKeys <> nil then
      FRetired.Add(FKeys);
    FKeys := vNew;
    Result := FKeys.Find(AKeyID);
  finally
    FLock.Release;
  end;
end;

{ TRALOIDCLogin }

constructor TRALOIDCLogin.Create(AOwner: TComponent);

  function NewRoute(const AName, APath: StringRAL): TRALRoute;
  begin
    Result := TRALRoute(FRoutes.Add);
    Result.Name := AName;
    Result.Route := APath;
    Result.SkipAuthMethods := [amALL];
    Result.AllowedMethods := [amGET, amPOST];
  end;

begin
  inherited Create(AOwner);
  FRoutes := TOwnedCollection.Create(Self, TRALRoute);
  FLoginRoute := NewRoute('oidc_login', '/oidc/login');
  FCallbackRoute := NewRoute('oidc_callback', '/oidc/callback');
  FLogoutRoute := NewRoute('oidc_logout', '/oidc/logout');
  FDiscovery := TRALOIDCDiscovery.Create;
  FCacheMinutes := 60;
  FScope := 'openid profile email';
  FPostLoginRedirect := '/';
  FProvider := opGeneric;
  FRolesClaim := RALOIDCRolesClaim(opGeneric);
end;

destructor TRALOIDCLogin.Destroy;
begin
  FreeAndNil(FRoutes);
  FreeAndNil(FDiscovery);
  FreeAndNil(FOwnStore);
  inherited;
end;

class function TRALOIDCLogin.DefaultPriority: IntegerRAL;
begin
  { before the authenticators: its routes are its own }
  Result := RALPriorityAuthentication + 100;
end;

function TRALOIDCLogin.Phases: TRALPluginPhases;
begin
  Result := [ppProcess, ppResolveRoute];
end;

procedure TRALOIDCLogin.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FStore then
      FStore := nil
    else if AComponent = FSession then
      FSession := nil;
  end;
end;

procedure TRALOIDCLogin.SetSession(const AValue: TRALServerJWTAuth);
begin
  if FSession = AValue then
    Exit;
  if FSession <> nil then
    FSession.RemoveFreeNotification(Self);
  FSession := AValue;
  if FSession <> nil then
    FSession.FreeNotification(Self);
end;

procedure TRALOIDCLogin.SetStore(const AValue: TRALKeyValueStore);
begin
  if FStore = AValue then
    Exit;
  if FStore <> nil then
    FStore.RemoveFreeNotification(Self);
  FStore := AValue;
  if FStore <> nil then
    FStore.FreeNotification(Self);
end;

procedure TRALOIDCLogin.SetProvider(AValue: TRALOIDCProvider);
begin
  if FProvider = AValue then
    Exit;
  FProvider := AValue;
  FRolesClaim := RALOIDCRolesClaim(AValue);
  if (AValue = opGoogle) and (FIssuer = '') then
    FIssuer := 'https://accounts.google.com';
end;

function TRALOIDCLogin.UsedStore: TRALKeyValueStore;
begin
  Result := FStore;
  if Result = nil then
  begin
    if FOwnStore = nil then
      FOwnStore := TRALMemoryStore.Create(nil);
    Result := FOwnStore;
  end;
end;

function TRALOIDCLogin.GetPath(AIndex: IntegerRAL): StringRAL;
begin
  case AIndex of
    0: Result := FLoginRoute.Route;
    1: Result := FCallbackRoute.Route;
  else
    Result := FLogoutRoute.Route;
  end;
end;

procedure TRALOIDCLogin.SetPath(AIndex: IntegerRAL; const AValue: StringRAL);
begin
  case AIndex of
    0: FLoginRoute.Route := AValue;
    1: FCallbackRoute.Route := AValue;
  else
    FLogoutRoute.Route := AValue;
  end;
end;

function TRALOIDCLogin.RedirectFor(ARequest: TRALRequest): StringRAL;
var
  vScheme: StringRAL;
begin
  Result := FRedirectURI;
  if Result <> '' then
    Exit;
  vScheme := 'http';
  if RALSameName(ARequest.HttpVersion, 'HTTPS') then
    vScheme := 'https';
  Result := vScheme + '://' + ARequest.Host + FCallbackRoute.Route;
end;

function TRALOIDCLogin.Discover: boolean;
begin
  Result := FDiscovery.Load(FIssuer, FDiscoveryURL, FFetchEngine, FCacheMinutes);
end;

function TRALOIDCLogin.ResolveRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
begin
  if RALSameName(ARequest.Query, FLoginRoute.Route) then
    Result := FLoginRoute
  else if RALSameName(ARequest.Query, FCallbackRoute.Route) then
    Result := FCallbackRoute
  else if RALSameName(ARequest.Query, FLogoutRoute.Route) then
    Result := FLogoutRoute
  else
    Result := nil;
end;

procedure TRALOIDCLogin.ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
  var AHandled: boolean);
var
  vRoute: TRALRoute;
begin
  vRoute := Host.FindRoute(ARequest, AResponse);
  if (vRoute = nil) or (ARequest.RouteOwner <> Self) then
    Exit;
  AHandled := True;
  if not vRoute.IsMethodAllowed(ARequest.Method) then
  begin
    AResponse.Answer(HTTP_MethodNotAllowed);
    Exit;
  end;
  if vRoute = FLoginRoute then
    AnswerLogin(ARequest, AResponse)
  else if vRoute = FCallbackRoute then
    AnswerCallback(ARequest, AResponse)
  else
    AnswerLogout(ARequest, AResponse);
end;

procedure TRALOIDCLogin.Fail(ARequest: TRALRequest; AResponse: TRALResponse;
  AStatus: IntegerRAL; const AError: StringRAL);
begin
  AResponse.Answer(AStatus, AError, rctTEXTPLAIN);
  if Assigned(FOnError) then
    FOnError(Self, ARequest, AResponse, AError);
end;

procedure TRALOIDCLogin.AnswerLogin(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vState, vNonce, vVerifier, vReturn, vURL: StringRAL;
begin
  if not Discover then
  begin
    Fail(ARequest, AResponse, HTTP_BadGateway, emOIDCDiscovery);
    Exit;
  end;
  vReturn := ARequest.ParamByName('return').AsString;
  if not RALOIDCSafeRedirect(vReturn) then
    vReturn := FPostLoginRedirect;
  vState := RandomText(24);
  vNonce := RandomText(24);
  vVerifier := RandomText(32);
  UsedStore.Put(cStatePrefix + vState,
    RawByteString(vNonce + '|' + vVerifier + '|' + vReturn), cStateTTL);

  vURL := FDiscovery.AuthorizationEndpoint;
  if Pos(StringRAL('?'), vURL) > 0 then
    vURL := vURL + '&'
  else
    vURL := vURL + '?';
  vURL := vURL + 'response_type=code' +
    '&client_id=' + TRALHTTPCoder.EncodeURL(FClientID) +
    '&redirect_uri=' + TRALHTTPCoder.EncodeURL(RedirectFor(ARequest)) +
    '&scope=' + TRALHTTPCoder.EncodeURL(ScopeWithOpenID(FScope)) +
    '&state=' + vState +
    '&nonce=' + vNonce +
    '&code_challenge=' + TRALServerOAuth2.PKCEChallenge(vVerifier) +
    '&code_challenge_method=S256';
  AResponse.Answer(HTTP_Found, '', rctTEXTPLAIN);
  AResponse.AddHeader('Location', vURL);
  AResponse.AddHeader('Cache-Control', 'no-store');
end;

function TRALOIDCLogin.CheckIdToken(const AIdToken, ANonce: StringRAL;
  AClaims: TRALJWTParams; out AReason: StringRAL): boolean;
var
  vJWT: TRALJWT;
  vAlg: TRALJWTAlgorithm;
  vKey: TRALJWSKey;
  vHeaderAlg: StringRAL;
  vParts: IntegerRAL;
  vHeader: TRALJSONValue;
begin
  Result := False;
  AReason := wmJWTInvalid;
  { the alg the header names chooses only among what the client allows: an
    issuer key (RS*/ES*), or the client secret (HS*) when there is one }
  vParts := Pos(StringRAL('.'), AIdToken);
  if vParts < 2 then
    Exit;
  vHeader := nil;
  try
    vHeader := TRALJSON.ParseJSON(TRALBase64.Decode(
      TRALBase64.FromBase64Url(Copy(AIdToken, 1, vParts - 1))));
  except
    vHeader := nil;
  end;
  vJWT := TRALJWT.Create;
  try
    vHeaderAlg := JSONMember(vHeader, 'alg');
    if not AlgorithmOf(vHeaderAlg, vAlg) then
      Exit;
    vJWT.Header.Algorithm := vAlg;
    vJWT.Leeway := 60;
    if vAlg in [tjaHSHA256, tjaHSHA384, tjaHSHA512] then
    begin
      if FClientSecret = '' then
        Exit;
      vJWT.SignSecretKey := FClientSecret;
    end
    else
    begin
      vKey := FDiscovery.Key(JSONMember(vHeader, 'kid'), FFetchEngine);
      if vKey = nil then
        Exit;
      if ((vAlg in [tjaRS256, tjaRS384, tjaRS512]) and (vKey.KeyType <> jktRSA)) or
         ((vAlg in [tjaES256, tjaES384]) and (vKey.KeyType <> jktEC)) then
        Exit;
      vJWT.SignKey := vKey;
    end;
    if not vJWT.IsValidToken(AIdToken) then
    begin
      if (vJWT.Payload.Expiration > 0) and (vJWT.Payload.Expiration < Now) then
        AReason := wmJWTExpired;
      Exit;
    end;
    AClaims.Assign(vJWT.Payload);
  finally
    vJWT.Free;
    vHeader.Free;
  end;

  AReason := wmOIDCClaims;
  if (FDiscovery.Issuer <> '') and (AClaims.Issuer <> FDiscovery.Issuer) then
    Exit;
  if not AClaims.HasAudience(FClientID) then
    Exit;
  if (AClaims.GetClaim('azp') <> '') and (AClaims.GetClaim('azp') <> FClientID) then
    Exit;
  if AClaims.Expiration <= 0 then
    Exit;
  if (ANonce <> '') and not RALSameSecret(AClaims.GetClaim('nonce'), ANonce) then
    Exit;
  AReason := '';
  Result := True;
end;

procedure TRALOIDCLogin.AnswerCallback(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vState, vCode, vError, vReason, vNonce, vVerifier, vReturn, vText, vIdToken: StringRAL;
  vStored: RawByteString;
  vSep: IntegerRAL;
  vFields: TStringList;
  vStatus: IntegerRAL;
  vJson: TRALJSONValue;
  vClaims: TRALJWTParams;
  vAllow: boolean;
  vToken, vParams: StringRAL;
  vCookie: TRALCookie;
  vRoles: TRALJSONArray;
  vPayload: TRALJSONObject;
  vInt: IntegerRAL;
begin
  vError := ARequest.ParamByName('error').AsString;
  vState := ARequest.ParamByName('state').AsString;
  vCode := ARequest.ParamByName('code').AsString;

  { the state is used once, whatever happens next }
  vStored := '';
  if (vState = '') or not UsedStore.Get(cStatePrefix + vState, vStored) then
  begin
    Fail(ARequest, AResponse, HTTP_BadRequest, emOIDCState);
    Exit;
  end;
  UsedStore.Delete(cStatePrefix + vState);
  vSep := Pos('|', string(vStored));
  vNonce := StringRAL(Copy(vStored, 1, vSep - 1));
  vStored := Copy(vStored, vSep + 1, Length(vStored));
  vSep := Pos('|', string(vStored));
  vVerifier := StringRAL(Copy(vStored, 1, vSep - 1));
  vReturn := StringRAL(Copy(vStored, vSep + 1, Length(vStored)));

  if vError <> '' then
  begin
    Fail(ARequest, AResponse, HTTP_Forbidden, vError + ': ' +
      ARequest.ParamByName('error_description').AsString);
    Exit;
  end;
  if (vCode = '') or not Discover then
  begin
    Fail(ARequest, AResponse, HTTP_BadRequest, emOIDCDiscovery);
    Exit;
  end;

  vFields := TStringList.Create;
  vClaims := TRALJWTParams.Create;
  vJson := nil;
  try
    vFields.Add('grant_type=authorization_code');
    vFields.Add('code=' + string(vCode));
    vFields.Add('redirect_uri=' + string(RedirectFor(ARequest)));
    vFields.Add('code_verifier=' + string(vVerifier));
    vFields.Add('client_id=' + string(FClientID));
    if FClientSecret <> '' then
      vText := RALOAuth2Fetch(FDiscovery.TokenEndpoint, vFields, FClientID, FClientSecret,
        FFetchEngine, vStatus)
    else
      vText := RALOAuth2Fetch(FDiscovery.TokenEndpoint, vFields, '', '', FFetchEngine,
        vStatus);
    try
      vJson := TRALJSON.ParseJSON(vText);
    except
      vJson := nil;
    end;
    vIdToken := JSONMember(vJson, 'id_token');
    if (vStatus <> HTTP_OK) or (vIdToken = '') then
    begin
      vReason := JSONMember(vJson, 'error');
      if vReason = '' then
        vReason := emOIDCToken;
      Fail(ARequest, AResponse, HTTP_BadGateway, vReason);
      Exit;
    end;

    if not CheckIdToken(vIdToken, vNonce, vClaims, vReason) then
    begin
      Fail(ARequest, AResponse, HTTP_Forbidden, vReason);
      Exit;
    end;

    RALIdentityFromClaims(ARequest.Identity, vClaims, '', ratBearer);
    FillRoles(ARequest.Identity, vClaims, FRolesClaim);
    vAllow := True;
    if Assigned(FOnLogin) then
      FOnLogin(Self, ARequest, vClaims, ARequest.Identity, vAllow, vReturn);
    if not vAllow then
    begin
      Fail(ARequest, AResponse, HTTP_Forbidden, emOIDCRefused);
      Exit;
    end;

    if FSession <> nil then
    begin
      { the application's own token: who, and the roles, as one claim set }
      vPayload := TRALJSONObject.Create;
      try
        vPayload.Add('sub', ARequest.Identity.Name);
        vPayload.Add('iss', FDiscovery.Issuer);
        if vClaims.GetClaim('email') <> '' then
          vPayload.Add('email', vClaims.GetClaim('email'));
        if vClaims.GetClaim('name') <> '' then
          vPayload.Add('name', vClaims.GetClaim('name'));
        vRoles := TRALJSONArray.Create;
        for vInt := 0 to ARequest.Identity.Roles.Count - 1 do
          vRoles.Add(StringRAL(ARequest.Identity.Roles[vInt]));
        vPayload.Add('roles', vRoles);
        vParams := vPayload.ToJSON;
      finally
        vPayload.Free;
      end;
      vToken := FSession.GetToken(vParams);
      Finalize(vCookie);
      FillChar(vCookie, SizeOf(vCookie), 0);
      vCookie.Name := RALTOKENName;
      vCookie.Value := vToken;
      vCookie.Secure := RALSameName(ARequest.HttpVersion, 'HTTPS');
      vCookie.HttpOnly := True;
      vCookie.Path := '/';
      vCookie.SameSite := cssLax;
      AResponse.AddCookie(vCookie);
    end;

    if not RALOIDCSafeRedirect(vReturn) then
      vReturn := FPostLoginRedirect;
    AResponse.Answer(HTTP_Found, '', rctTEXTPLAIN);
    AResponse.AddHeader('Location', vReturn);
    AResponse.AddHeader('Cache-Control', 'no-store');
  finally
    vJson.Free;
    vClaims.Free;
    vFields.Free;
  end;
end;

procedure TRALOIDCLogin.AnswerLogout(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vCookie: TRALCookie;
  vURL: StringRAL;
begin
  Finalize(vCookie);
  FillChar(vCookie, SizeOf(vCookie), 0);
  vCookie.Name := RALTOKENName;
  vCookie.HttpOnly := True;
  vCookie.Secure := RALSameName(ARequest.HttpVersion, 'HTTPS');
  vCookie.Path := '/';
  vCookie.MaxAge := -1;
  AResponse.AddCookie(vCookie);

  vURL := '';
  if Discover and (FDiscovery.EndSessionEndpoint <> '') then
  begin
    vURL := FDiscovery.EndSessionEndpoint + '?client_id=' +
      TRALHTTPCoder.EncodeURL(FClientID);
    if FPostLogoutRedirect <> '' then
      vURL := vURL + '&post_logout_redirect_uri=' +
        TRALHTTPCoder.EncodeURL(FPostLogoutRedirect);
  end
  else if FPostLogoutRedirect <> '' then
    vURL := FPostLogoutRedirect
  else
    vURL := FPostLoginRedirect;
  AResponse.Answer(HTTP_Found, '', rctTEXTPLAIN);
  AResponse.AddHeader('Location', vURL);
end;

{ TRALClientOIDCAuth }

constructor TRALClientOIDCAuth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDiscovery := TRALOIDCDiscovery.Create;
  FIdClaims := TRALJWTParams.Create;
  Scope := 'openid profile email offline_access';
  GrantType := ogtAuthorizationCode;
end;

destructor TRALClientOIDCAuth.Destroy;
begin
  FreeAndNil(FDiscovery);
  FreeAndNil(FIdClaims);
  inherited;
end;

procedure TRALClientOIDCAuth.TokenReceived(AJson: TRALJSONObject);
var
  vToken: StringRAL;
  vJWT: TRALJWT;
begin
  vToken := JSONMember(AJson, 'id_token');
  if vToken = '' then
    Exit;
  FIdToken := vToken;
  vJWT := TRALJWT.Create;
  try
    vJWT.Token := vToken;
    FIdClaims.Assign(vJWT.Payload);
  finally
    vJWT.Free;
  end;
end;

function TRALClientOIDCAuth.Discover: boolean;
begin
  Result := FDiscovery.Load(FIssuer, '', FFetchEngine, 60);
  if Result and (FDiscovery.TokenEndpoint <> '') then
    TokenURL := FDiscovery.TokenEndpoint;
end;

function TRALClientOIDCAuth.LoginURL(const ARedirectURI: StringRAL;
  out AState: StringRAL): StringRAL;
begin
  Result := '';
  AState := '';
  if not Discover then
    Exit;
  Result := AuthorizationURL(FDiscovery.AuthorizationEndpoint, ARedirectURI, AState);
end;

function TRALClientOIDCAuth.StartDeviceFlow(out ADevice: TRALOIDCDeviceCode;
  out AError: StringRAL): boolean;
var
  vFields: TStringList;
  vText: StringRAL;
  vStatus: IntegerRAL;
  vJson: TRALJSONValue;
begin
  Result := False;
  AError := '';
  ADevice.DeviceCode := '';
  ADevice.UserCode := '';
  ADevice.VerificationURI := '';
  ADevice.VerificationURIComplete := '';
  ADevice.Interval := 5;
  ADevice.ExpiresIn := 0;
  if (not Discover) or (FDiscovery.DeviceEndpoint = '') then
  begin
    AError := emOIDCNoDevice;
    Exit;
  end;
  vFields := TStringList.Create;
  vJson := nil;
  try
    vFields.Add('client_id=' + string(ClientID));
    vFields.Add('scope=' + string(Scope));
    if ClientSecret <> '' then
      vText := RALOAuth2Fetch(FDiscovery.DeviceEndpoint, vFields, ClientID, ClientSecret,
        FFetchEngine, vStatus)
    else
      vText := RALOAuth2Fetch(FDiscovery.DeviceEndpoint, vFields, '', '', FFetchEngine,
        vStatus);
    try
      vJson := TRALJSON.ParseJSON(vText);
    except
      vJson := nil;
    end;
    ADevice.DeviceCode := JSONMember(vJson, 'device_code');
    if (vStatus <> HTTP_OK) or (ADevice.DeviceCode = '') then
    begin
      AError := JSONMember(vJson, 'error');
      if AError = '' then
        AError := emOIDCNoDevice;
      Exit;
    end;
    ADevice.UserCode := JSONMember(vJson, 'user_code');
    ADevice.VerificationURI := JSONMember(vJson, 'verification_uri');
    if ADevice.VerificationURI = '' then
      ADevice.VerificationURI := JSONMember(vJson, 'verification_url'); // Google
    ADevice.VerificationURIComplete := JSONMember(vJson, 'verification_uri_complete');
    ADevice.Interval := StrToIntDef(string(JSONMember(vJson, 'interval')), 5);
    ADevice.ExpiresIn := StrToIntDef(string(JSONMember(vJson, 'expires_in')), 600);
    Result := True;
  finally
    vJson.Free;
    vFields.Free;
  end;
end;

function TRALClientOIDCAuth.WaitDeviceFlow(const ADevice: TRALOIDCDeviceCode;
  ATimeoutSec: IntegerRAL; out AError: StringRAL): boolean;
var
  vFields: TStringList;
  vText, vCode: StringRAL;
  vStatus, vInterval: IntegerRAL;
  vJson: TRALJSONValue;
  vEnd: TDateTime;
begin
  Result := False;
  AError := '';
  vInterval := ADevice.Interval;
  if vInterval < 1 then
    vInterval := 5;
  if (ATimeoutSec <= 0) or (ATimeoutSec > ADevice.ExpiresIn) then
    ATimeoutSec := ADevice.ExpiresIn;
  vEnd := IncSecond(Now, ATimeoutSec);
  vFields := TStringList.Create;
  try
    vFields.Add('grant_type=urn:ietf:params:oauth:grant-type:device_code');
    vFields.Add('device_code=' + string(ADevice.DeviceCode));
    vFields.Add('client_id=' + string(ClientID));
    while Now < vEnd do
    begin
      if ClientSecret <> '' then
        vText := RALOAuth2Fetch(TokenURL, vFields, ClientID, ClientSecret, FFetchEngine,
          vStatus)
      else
        vText := RALOAuth2Fetch(TokenURL, vFields, '', '', FFetchEngine, vStatus);
      vJson := nil;
      try
        try
          vJson := TRALJSON.ParseJSON(vText);
        except
          vJson := nil;
        end;
        if vJson is TRALJSONObject then
        begin
          if TakeTokenAnswer(TRALJSONObject(vJson), vStatus, AError) then
          begin
            Result := True;
            Exit;
          end;
          vCode := JSONMember(vJson, 'error');
          if vCode = 'slow_down' then
            Inc(vInterval, 5)
          else if vCode <> 'authorization_pending' then
            Exit; // access_denied, expired_token: the end
        end
        else if vStatus = 0 then
        begin
          AError := emOIDCToken;
          Exit;
        end;
      finally
        vJson.Free;
      end;
      Sleep(vInterval * 1000);
    end;
    AError := 'expired_token';
  finally
    vFields.Free;
  end;
end;

end.
