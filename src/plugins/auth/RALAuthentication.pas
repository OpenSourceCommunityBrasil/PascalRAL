/// Basic and JWT authentication; the base classes are in RALAuthBase.
unit RALAuthentication;

interface

uses
  Classes, SysUtils, DateUtils, SyncObjs,
  RALToken, RALConsts, RALTypes, RALRoutes, RALBase64, RALTools, RALJson,
  RALRequest, RALParams, RALResponse, RALCustomObjects, RALMIMETypes, RALPlugin,
  RALAuthBase, RALKeyValueStore, RALJWS, RALPassword,
  RALStream;

const
  RALTOKENName = 'raltoken';
  RALPAYLOADName = 'ral_payload';
  /// Route of the public keys of TRALServerJWTAuth.Keys (PublishJWKS).
  RALJWKSRoute = '/.well-known/jwks.json';
  /// Claims TRALServerJWTAuth.RolesClaim reads the roles from by default.
  RALDefaultRolesClaim = 'roles,role,groups';


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
  TRALOnResolve = procedure(AToken: StringRAL; AParams: TRALJWTParams;
                            var AResult: StringRAL) of object;

  /// Same as RALAuthBase.TRALOnBeforeGetToken.
  TRALOnBeforeGetToken = RALAuthBase.TRALOnBeforeGetToken;
  /// Same as RALAuthBase.TRALAuthentication.
  TRALAuthentication = RALAuthBase.TRALAuthentication;
  /// Same as RALAuthBase.TRALAuthTransport.
  TRALAuthTransport = RALAuthBase.TRALAuthTransport;
  /// Same as RALAuthBase.TRALAuthClient.
  TRALAuthClient = RALAuthBase.TRALAuthClient;
  /// Same as RALAuthBase.TRALAuthServer.
  TRALAuthServer = RALAuthBase.TRALAuthServer;

  /// Same as RALAuthBase.TRALOnGetRoles.
  TRALOnGetRoles = RALAuthBase.TRALOnGetRoles;

  { One key of TRALServerJWTAuth.Keys: an HMAC secret (HS*) or an RSA/EC key pair
    in PEM (RS*/ES*), with the kid that names it in the token header. Tokens are
    signed with the newest active key and checked with any key not retired, so a
    key is replaced without logging anybody out: add the new one, wait for the
    tokens of the old one to expire, then set Retired. }
  TRALJWTKeyItem = class(TCollectionItem)
  private
    FAlgorithm: TRALJWTAlgorithm;
    /// The PEM keys loaded, made on first use.
    FJWSKey: TRALJWSKey;
    FKeyID: StringRAL;
    FNotBefore: TDateTime;
    FPrivateKey: TStrings;
    FPublicKey: TStrings;
    FRetired: boolean;
    FSecret: StringRAL;

    procedure KeyChanged(Sender: TObject);
    procedure SetPrivateKey(const AValue: TStrings);
    procedure SetPublicKey(const AValue: TStrings);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    /// Whether the key signs and verifies at ANow: not retired and NotBefore reached.
    function ActiveAt(ANow: TDateTime): boolean;
    /// Sets algorithm, kid and key of AJWT to this key's.
    procedure Configure(AJWT: TRALJWT);
    /// Whether the algorithm is RS* or ES* (a PEM key, not a secret).
    function IsAsymmetric: boolean;
    { The RSA/EC key of the PEM texts, loaded once (the private one when given,
      else the public); nil for an HMAC key. }
    function JWSKey: TRALJWSKey;
  published
    /// Algorithm of the key.
    property Algorithm: TRALJWTAlgorithm read FAlgorithm write FAlgorithm default tjaHSHA256;
    /// The kid of the tokens this key signs; empty = tokens without kid.
    property KeyID: StringRAL read FKeyID write FKeyID;
    { The key signs only from this time on (0 = always); verifying is not
      affected. Lets a new key be published (JWKS) before it signs. }
    property NotBefore: TDateTime read FNotBefore write FNotBefore;
    /// Private key in PEM (RS*/ES*): signs, and verifies.
    property PrivateKey: TStrings read FPrivateKey write SetPrivateKey;
    /// Public key in PEM (RS*/ES*), when the private one is not here: verifies only.
    property PublicKey: TStrings read FPublicKey write SetPublicKey;
    /// A retired key neither signs nor verifies: its tokens stop counting.
    property Retired: boolean read FRetired write FRetired default False;
    /// HMAC secret (HS*).
    property Secret: StringRAL read FSecret write FSecret;
  end;

  /// The keys of TRALServerJWTAuth.
  TRALJWTKeys = class(TOwnedCollection)
  private
    FLock: TCriticalSection;

    function GetKey(AIndex: IntegerRAL): TRALJWTKeyItem;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;

    /// Adds an RS*/ES* key from its private key in PEM.
    function AddPrivateKey(const AKeyID, APEM: StringRAL;
      AAlgorithm: TRALJWTAlgorithm = tjaRS256): TRALJWTKeyItem;
    /// Adds an HMAC key.
    function AddSecret(const AKeyID, ASecret: StringRAL;
      AAlgorithm: TRALJWTAlgorithm = tjaHSHA256): TRALJWTKeyItem;
    /// The key named AKeyID (exact), not retired; nil when there is none.
    function Find(const AKeyID: StringRAL): TRALJWTKeyItem;
    /// The public keys of the RS*/ES* keys not retired, as a JWK set (RFC 7517).
    function JWKS: StringRAL;
    /// The key that signs at ANow: the active one with the latest NotBefore (the last added on a tie).
    function Signer(ANow: TDateTime): TRALJWTKeyItem;

    /// Keys by index.
    property Keys[AIndex: IntegerRAL]: TRALJWTKeyItem read GetKey; default;
  end;

  /// BasicAuth for client components
  TRALClientBasicAuth = class(TRALAuthClient)
  private
    FPassword: StringRAL;
    FUserName: StringRAL;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AUser: StringRAL;
                       const APassword: StringRAL); overload;

    function IsAuthenticated: boolean; override;
    procedure SetAuthHeader(AVars: TStringList; AParams: TRALParams); override;
  published
    property Password: StringRAL read FPassword write FPassword;
    property UserName: StringRAL read FUserName write FUserName;
  end;

  /// BasicAuth for Server components
  TRALServerBasicAuth = class(TRALAuthServer)
  private
    FAuthDialog: boolean;
    FOnGetRoles: TRALOnGetRoles;
    FOnValidate: TRALOnValidate;
    FPassword: StringRAL;
    FPasswords: TRALPasswordList;
    FUserName: StringRAL;

    procedure SetPasswords(const AValue: TRALPasswordList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AUser: StringRAL;
                       const APassword: StringRAL); overload;

    /// Validation process of the authentication is made here
    procedure Validate(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    property AuthDialog: boolean read FAuthDialog write FAuthDialog;
    { Fired after the user was accepted, to fill the roles of ARequest.Identity
      (route Roles, policies); the name and scheme are already there. }
    property OnGetRoles: TRALOnGetRoles read FOnGetRoles write FOnGetRoles;
    property OnValidate: TRALOnValidate read FOnValidate write FOnValidate;
    property Password: StringRAL read FPassword write FPassword;
    { Users and password hashes (Argon2id, bcrypt...) checked when OnValidate is
      not assigned; UserName and Password still accept their one user. }
    property Passwords: TRALPasswordList read FPasswords write SetPasswords;
    property UserName: StringRAL read FUserName write FUserName;
  end;

  /// JWT Authenticator for Client components
  TRALClientJWTAuth = class(TRALAuthClient)
  private
    FJSONKey: StringRAL;
    FPayload: TRALJWTParams;
    FRoute: StringRAL;
    FToken: StringRAL;
  protected
    procedure SetPayload(const AValue: TRALJWTParams);
    procedure SetRoute(const AValue: StringRAL);
    procedure SetToken(const AValue: StringRAL);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Reads one claim of the token currently held, under the lock.
    /// This is the thread-safe way in: the raw Payload below is an object this
    /// class rewrites whenever a token arrives, so reading it while another
    /// thread refreshes the token walks its lists mid-swap.
    function GetClaim(const AKey: StringRAL): StringRAL;
    /// A 401: the token is dropped, and the request goes again with a new one
    function HandleChallenge(ARequest: TRALRequest; AResponse: TRALResponse): boolean; override;
    function IsAuthenticated: boolean; override;
    /// Posts to Route for a token (OnBeforeGetToken, or Payload as the body)
    function Prepare(ATransport: TRALAuthTransport; AVars: TStringList;
      AResponse: TRALResponse): IntegerRAL; override;
    procedure SetAuthHeader(AVars: TStringList; AParams: TRALParams); override;
  published
    property JSONKey: StringRAL read FJSONKey write FJSONKey;
    property OnBeforeGetToken;
    /// The decoded claims of the current token. Held between requests and
    /// REPLACED on every SetToken, so anything reading it from another thread
    /// has to hold Lock for as long as it uses what it read - or call GetClaim,
    /// which does that for a single claim.
    property Payload: TRALJWTParams read FPayload write SetPayload;
    property Route: StringRAL read FRoute write SetRoute;
    property Token: StringRAL read FToken write SetToken;
  end;

  { TRALServerJWTAuth }

  /// JWT Authenticator for server components
  TRALServerJWTAuth = class(TRALAuthServer)
  private
    FAlgorithm: TRALJWTAlgorithm;
    FAuthToken: TRALBaseRoute;
    FCollectionAuth: TOwnedCollection;
    FCookieSameSite: TRALCookieSiteScope;
    FExpSecs: IntegerRAL;
    FJSONKey: StringRAL;
    FJWKSRoute: TRALBaseRoute;
    FKeys: TRALJWTKeys;
    FOnGetToken: TRALOnTokenJWT;
    FOnGetTokenGen: TRALOnTokenJWTGen;
    FOnRenewToken: TRALOnTokenJWT;
    FOnRenewTokenGen: TRALOnTokenJWTGen;
    FOnValidate: TRALOnTokenJWT;
    FOnValidateGen: TRALOnTokenJWTGen;
    FPublishJWKS: boolean;
    FRevocationStore: TRALKeyValueStore;
    FRolesClaim: StringRAL;
    FSignSecretKey: StringRAL;
    FUseCookie: Boolean;

    { RFC 6750 3: tells a client WHY it got the 401 - no credentials at all
      (AError empty) or a token that was refused, and what was wrong with it }
    procedure AnswerChallenge(AResponse: TRALResponse; const AError,
      ADescription: StringRAL);
    { OnGetToken or OnGetTokenGen is assigned }
    function CanIssue: boolean;
    { Reads AToken into AJWT and checks it: signature (with the key its kid
      names, when there are Keys), dates and revocation. AReason says why not }
    function CheckToken(AJWT: TRALJWT; const AToken: StringRAL;
      out AReason: StringRAL): boolean;
    { False, with the 500 already answered, when SignSecretKey is empty }
    function HasSecretKey(AResponse: TRALResponse): boolean;
    { a first token from OnGetToken; '' when it refused }
    function IssueToken(ARequest: TRALRequest; AResponse: TRALResponse;
      var AJSONParams: StringRAL): StringRAL;
    { the token route logs in (OnGetToken) rather than renews - see BeforeValidate }
    function LogsIn(ARequest: TRALRequest): boolean;
    { Sets algorithm and key of AJWT for signing: the Signer of Keys, else
      Algorithm and SignSecretKey. False when Keys has no active key }
    function PrepareSigner(AJWT: TRALJWT): boolean;
    { the work of RenewToken; with a request, OnRenewToken is consulted }
    function RenewTokenFor(ARequest: TRALRequest; AResponse: TRALResponse;
      const AToken: StringRAL; var AJSONParams: StringRAL): StringRAL;
    procedure SetKeys(const AValue: TRALJWTKeys);
    procedure SetRevocationStore(const AValue: TRALKeyValueStore);
    procedure SetUseCookie(AValue: Boolean);
  protected
    function GetAuthRoute: TRALBaseRoute; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAuthRoute(ARoute: TRALBaseRoute); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AttemptOf(ARequest: TRALRequest; AResponse: TRALResponse;
      AResult: TRALAuthResult; AOnOwnRoute: boolean): TRALAuthAttempt; override;
    procedure BeforeValidate(ARequest: TRALRequest; AResponse: TRALResponse); override;
    /// The raltoken cookie, when no Authorization header came
    procedure DecodeWithoutHeader(ARequest: TRALRequest); override;
    function GetToken(var AJSONParams: StringRAL): StringRAL;
    { Whether the claims AParams (of a valid token) were revoked: their jti, or
      every token of their sub issued up to RevokeAllFor. False without a
      RevocationStore. }
    function IsRevoked(AParams: TRALJWTParams): boolean;
    /// A new token with the claims of AToken and a new expiration; '' when
    /// AToken is not valid. Called directly it does not fire OnRenewToken
    function RenewToken(const AToken: StringRAL; var AJSONParams: StringRAL): StringRAL;
    /// The token route, or the JWKS route when PublishJWKS is on.
    function ResolveRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute; override;
    { Revokes AToken (a valid token of this server, with a jti) until it would
      expire; False when it is not valid or has no jti. Needs RevocationStore. }
    function Revoke(const AToken: StringRAL): boolean;
    { Revokes every token of the user ASubject issued up to ABefore (0 = now),
      to the second: logs a user out everywhere. Needs RevocationStore. }
    procedure RevokeAllFor(const ASubject: StringRAL; ABefore: TDateTime = 0);
    /// Revokes the token whose jti is AId until AExpires (0 = forever). Needs RevocationStore.
    procedure RevokeId(const AId: StringRAL; AExpires: TDateTime = 0);
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
    /// SameSite of the raltoken cookie UseCookie sends. cssDefault (the default)
    /// writes none and leaves the browser's own rule, as it always did. cssLax
    /// or cssStrict keep another site from sending it, which is what stops a
    /// cross-site request riding on the login - and also what stops a page on
    /// another site that calls this server on purpose, with credentials
    property CookieSameSite: TRALCookieSiteScope read FCookieSameSite
      write FCookieSameSite default cssDefault;
    property ExpirationSecs: IntegerRAL read FExpSecs write FExpSecs;
    property JSONKey: StringRAL read FJSONKey write FJSONKey;
    { Signing keys with kid, for key rotation and RS*/ES*; empty = Algorithm
      and SignSecretKey, as always. }
    property Keys: TRALJWTKeys read FKeys write SetKeys;
    property OnGetToken: TRALOnTokenJWT read FOnGetToken write FOnGetToken;
    /// Fired when a valid token is posted to AuthRoute to be renewed, with its
    /// claims in AParams - already checked, and with the new expiration set.
    /// Change any claim there (permissions read again from the database, say),
    /// or set AResult to False to refuse the renewal (the client gets 401 and
    /// has to ask for a first token again). Unassigned, the claims are copied
    /// as they were, which is what renewing always did
    property OnRenewToken: TRALOnTokenJWT read FOnRenewToken write FOnRenewToken;
    property OnValidate: TRALOnTokenJWT read FOnValidate write FOnValidate;
    { Answers GET /.well-known/jwks.json with the public keys of Keys (RS*/ES*),
      for clients and other servers that verify the tokens. }
    property PublishJWKS: boolean read FPublishJWKS write FPublishJWKS default False;
    { Where revoked tokens are kept (Revoke, RevokeAllFor); share one store
      (TRALFileStore, Redis) between servers so a token revoked on one is
      refused by all. Linked, every token gets a jti and an iat. }
    property RevocationStore: TRALKeyValueStore read FRevocationStore write SetRevocationStore;
    { Claims (separated by commas) the roles of ARequest.Identity are read from:
      a JSON array, a scope (blanks) or a list (commas). }
    property RolesClaim: StringRAL read FRolesClaim write FRolesClaim;
    property SignSecretKey: StringRAL read FSignSecretKey write FSignSecretKey;
    property UseCookie: Boolean read FUseCookie write SetUseCookie;
  end;

/// Same as RALAuthBase.RALAddChallenge.
procedure RALAddChallenge(AResponse: TRALResponse; const AChallenge: StringRAL);
/// Same as RALAuthBase.RALChallengeText.
function RALChallengeText(const AValue: StringRAL): StringRAL;

implementation

procedure RALAddChallenge(AResponse: TRALResponse; const AChallenge: StringRAL);
begin
  RALAuthBase.RALAddChallenge(AResponse, AChallenge);
end;

function RALChallengeText(const AValue: StringRAL): StringRAL;
begin
  Result := RALAuthBase.RALChallengeText(AValue);
end;

{ TRALJWTKeyItem }

constructor TRALJWTKeyItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FAlgorithm := tjaHSHA256;
  FPrivateKey := TStringList.Create;
  TStringList(FPrivateKey).OnChange := {$IFDEF FPC}@{$ENDIF}KeyChanged;
  FPublicKey := TStringList.Create;
  TStringList(FPublicKey).OnChange := {$IFDEF FPC}@{$ENDIF}KeyChanged;
end;

destructor TRALJWTKeyItem.Destroy;
begin
  FreeAndNil(FJWSKey);
  FreeAndNil(FPrivateKey);
  FreeAndNil(FPublicKey);
  inherited;
end;

procedure TRALJWTKeyItem.KeyChanged(Sender: TObject);
begin
  { the loaded key no longer matches the text; made again on the next use }
  FreeAndNil(FJWSKey);
end;

procedure TRALJWTKeyItem.SetPrivateKey(const AValue: TStrings);
begin
  FPrivateKey.Assign(AValue);
end;

procedure TRALJWTKeyItem.SetPublicKey(const AValue: TStrings);
begin
  FPublicKey.Assign(AValue);
end;

procedure TRALJWTKeyItem.AssignTo(Dest: TPersistent);
var
  vDest: TRALJWTKeyItem;
begin
  if not (Dest is TRALJWTKeyItem) then
  begin
    inherited AssignTo(Dest);
    Exit;
  end;
  vDest := TRALJWTKeyItem(Dest);
  vDest.Algorithm := FAlgorithm;
  vDest.KeyID := FKeyID;
  vDest.NotBefore := FNotBefore;
  vDest.PrivateKey := FPrivateKey;
  vDest.PublicKey := FPublicKey;
  vDest.Retired := FRetired;
  vDest.Secret := FSecret;
end;

function TRALJWTKeyItem.GetDisplayName: string;
begin
  Result := string(FKeyID);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TRALJWTKeyItem.ActiveAt(ANow: TDateTime): boolean;
begin
  Result := (not FRetired) and ((FNotBefore <= 0) or (FNotBefore <= ANow));
end;

function TRALJWTKeyItem.IsAsymmetric: boolean;
begin
  Result := FAlgorithm in [tjaRS256, tjaRS384, tjaRS512, tjaES256, tjaES384];
end;

function TRALJWTKeyItem.JWSKey: TRALJWSKey;
var
  vLock: TCriticalSection;
begin
  Result := nil;
  if not IsAsymmetric then
    Exit;
  vLock := TRALJWTKeys(Collection).FLock;
  vLock.Acquire;
  try
    if FJWSKey = nil then
    begin
      FJWSKey := TRALJWSKey.Create;
      try
        if Trim(FPrivateKey.Text) <> '' then
          FJWSKey.LoadPrivateKeyPEM(StringRAL(FPrivateKey.Text))
        else
          FJWSKey.LoadPublicKeyPEM(StringRAL(FPublicKey.Text));
        FJWSKey.KeyID := FKeyID;
      except
        FreeAndNil(FJWSKey);
        raise;
      end;
    end;
    Result := FJWSKey;
  finally
    vLock.Release;
  end;
end;

procedure TRALJWTKeyItem.Configure(AJWT: TRALJWT);
begin
  AJWT.Header.Algorithm := FAlgorithm;
  AJWT.Header.KeyID := FKeyID;
  if IsAsymmetric then
    AJWT.SignKey := JWSKey
  else
    AJWT.SignSecretKey := FSecret;
end;

{ TRALJWTKeys }

constructor TRALJWTKeys.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALJWTKeyItem);
  FLock := TCriticalSection.Create;
end;

destructor TRALJWTKeys.Destroy;
begin
  inherited;
  FreeAndNil(FLock);
end;

function TRALJWTKeys.GetKey(AIndex: IntegerRAL): TRALJWTKeyItem;
begin
  Result := TRALJWTKeyItem(Items[AIndex]);
end;

function TRALJWTKeys.AddSecret(const AKeyID, ASecret: StringRAL;
  AAlgorithm: TRALJWTAlgorithm): TRALJWTKeyItem;
begin
  Result := TRALJWTKeyItem(Add);
  Result.KeyID := AKeyID;
  Result.Secret := ASecret;
  Result.Algorithm := AAlgorithm;
end;

function TRALJWTKeys.AddPrivateKey(const AKeyID, APEM: StringRAL;
  AAlgorithm: TRALJWTAlgorithm): TRALJWTKeyItem;
begin
  Result := TRALJWTKeyItem(Add);
  Result.KeyID := AKeyID;
  Result.Algorithm := AAlgorithm;
  Result.PrivateKey.Text := string(APEM);
end;

function TRALJWTKeys.Find(const AKeyID: StringRAL): TRALJWTKeyItem;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
  begin
    Result := Keys[vInt];
    if (not Result.Retired) and (Result.KeyID = AKeyID) then
      Exit;
  end;
  Result := nil;
end;

function TRALJWTKeys.Signer(ANow: TDateTime): TRALJWTKeyItem;
var
  vInt: IntegerRAL;
  vKey: TRALJWTKeyItem;
begin
  Result := nil;
  for vInt := 0 to Count - 1 do
  begin
    vKey := Keys[vInt];
    if vKey.ActiveAt(ANow) and
       ((Result = nil) or (vKey.NotBefore >= Result.NotBefore)) then
      Result := vKey;
  end;
end;

function TRALJWTKeys.JWKS: StringRAL;
var
  vInt: IntegerRAL;
  vKey: TRALJWTKeyItem;
begin
  Result := '';
  for vInt := 0 to Count - 1 do
  begin
    vKey := Keys[vInt];
    if vKey.Retired or not vKey.IsAsymmetric then
      Continue;
    if Result <> '' then
      Result := Result + ',';
    Result := Result + vKey.JWSKey.AsJWK(TRALJWT.AlgorithmName(vKey.Algorithm));
  end;
  Result := '{"keys":[' + Result + ']}';
end;

{ TRALServerJWTAuth }

{ A JWT component dropped on a form with SignSecretKey left empty cannot sign
  or check a token: the HMAC refuses an empty key, and the request died with
  "Key must be provided.", which names no setting. Without a key the server
  answers 500 and says which one is missing - a 401 would read as a wrong
  password. A key of blanks counts as none, as for TRALServerBasicAuth }
function TRALServerJWTAuth.HasSecretKey(AResponse: TRALResponse): boolean;
begin
  Result := (FKeys.Count > 0) or (RALTrim(FSignSecretKey) <> '');
  if not Result then
    AResponse.AnswerError(HTTP_InternalError, emJWTNoSecretKey);
end;

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
  if FPublishJWKS and RALSameName(ARequest.Query, RALJWKSRoute) then
  begin
    AResponse.Answer(HTTP_OK, FKeys.JWKS, rctAPPLICATIONJSON);
    AResponse.AddHeader('Cache-Control', 'max-age=300');
    Exit;
  end;

  if not RALSameName(ARequest.Query, AuthRoute.Route) then
  begin
    AResponse.Answer(HTTP_NotFound);
    Exit;
  end;

  if not HasSecretKey(AResponse) then
    Exit;

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
      vCookie.SameSite := FCookieSameSite;
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
      vCookie.SameSite := FCookieSameSite;
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

  FJWKSRoute := TRALBaseRoute(FCollectionAuth.Add);
  FJWKSRoute.Route := RALJWKSRoute;
  FJWKSRoute.Name := 'jwks';
  FJWKSRoute.SkipAuthMethods := [amALL];
  FJWKSRoute.AllowedMethods := [amGET, amOPTIONS];
  FJWKSRoute.Description.Text := 'Public keys of the JWT signatures (JWK set)';

  FKeys := TRALJWTKeys.Create(Self);
  FRolesClaim := RALDefaultRolesClaim;
  FExpSecs := 1800;
  FJSONKey := 'token';
end;

destructor TRALServerJWTAuth.Destroy;
begin
  FreeAndNil(FCollectionAuth);
  FreeAndNil(FKeys);
  inherited;
end;

procedure TRALServerJWTAuth.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FRevocationStore) then
    FRevocationStore := nil;
end;

procedure TRALServerJWTAuth.SetKeys(const AValue: TRALJWTKeys);
begin
  FKeys.Assign(AValue);
end;

procedure TRALServerJWTAuth.SetRevocationStore(const AValue: TRALKeyValueStore);
begin
  if FRevocationStore = AValue then
    Exit;
  if FRevocationStore <> nil then
    FRevocationStore.RemoveFreeNotification(Self);
  FRevocationStore := AValue;
  if FRevocationStore <> nil then
    FRevocationStore.FreeNotification(Self);
end;

function TRALServerJWTAuth.PrepareSigner(AJWT: TRALJWT): boolean;
var
  vKey: TRALJWTKeyItem;
begin
  Result := True;
  if FKeys.Count = 0 then
  begin
    AJWT.Header.Algorithm := FAlgorithm;
    AJWT.SignSecretKey := FSignSecretKey;
    Exit;
  end;
  vKey := FKeys.Signer(Now);
  Result := vKey <> nil;
  if Result then
    vKey.Configure(AJWT);
end;

function TRALServerJWTAuth.CheckToken(AJWT: TRALJWT; const AToken: StringRAL;
  out AReason: StringRAL): boolean;
var
  vKey: TRALJWTKeyItem;
begin
  AReason := '';
  if FKeys.Count = 0 then
  begin
    AJWT.Header.Algorithm := FAlgorithm;
    AJWT.SignSecretKey := FSignSecretKey;
    Result := AJWT.IsValidToken(AToken);
  end
  else
  begin
    { the kid picks the key, and the key the algorithm: the token names the
      key, never how to check it. A first read only to find the kid }
    AJWT.Token := AToken;
    vKey := FKeys.Find(AJWT.Header.KeyID);
    if vKey = nil then
    begin
      AReason := wmJWTUnknownKey;
      Result := False;
      Exit;
    end;
    vKey.Configure(AJWT);
    Result := AJWT.IsValidToken(AToken);
  end;

  if not Result then
  begin
    { IsValidToken read the payload before deciding, so it still tells the
      two cases a client can do something about }
    if (AJWT.Payload.Expiration > 0) and (AJWT.Payload.Expiration < Now) then
      AReason := wmJWTExpired
    else if (AJWT.Payload.NotBefore > 0) and (AJWT.Payload.NotBefore > Now) then
      AReason := wmJWTNotYetValid
    else
      AReason := wmJWTInvalid;
  end
  else if IsRevoked(AJWT.Payload) then
  begin
    AReason := wmJWTRevoked;
    Result := False;
  end;
end;

function TRALServerJWTAuth.IsRevoked(AParams: TRALJWTParams): boolean;
var
  vValue: RawByteString;
begin
  Result := False;
  if FRevocationStore = nil then
    Exit;
  if (AParams.Id <> '') and FRevocationStore.Get('jwt.jti:' + AParams.Id, vValue) then
  begin
    Result := True;
    Exit;
  end;
  { a user logged out everywhere: every token issued up to that second. A
    token without iat cannot prove it is newer }
  if (AParams.Subject <> '') and FRevocationStore.Get('jwt.sub:' + AParams.Subject, vValue) then
    Result := (AParams.IssuedAt <= 0) or
      (DateTimeToUnix(RALDateTimeToGMT(AParams.IssuedAt)) <= StrToInt64Def(string(vValue), 0));
end;

function TRALServerJWTAuth.Revoke(const AToken: StringRAL): boolean;
var
  vJWT: TRALJWT;
  vReason: StringRAL;
begin
  Result := False;
  if FRevocationStore = nil then
    Exit;
  vJWT := TRALJWT.Create;
  try
    if CheckToken(vJWT, AToken, vReason) and (vJWT.Payload.Id <> '') then
    begin
      RevokeId(vJWT.Payload.Id, vJWT.Payload.Expiration);
      Result := True;
    end;
  finally
    vJWT.Free;
  end;
end;

procedure TRALServerJWTAuth.RevokeAllFor(const ASubject: StringRAL; ABefore: TDateTime);
var
  vTTL: Int64RAL;
begin
  if (FRevocationStore = nil) or (ASubject = '') then
    Exit;
  if ABefore <= 0 then
    ABefore := Now;
  { kept while a token issued before it can still be alive }
  vTTL := 0;
  if FExpSecs > 0 then
    vTTL := (Int64RAL(FExpSecs) + 60) * 1000;
  FRevocationStore.Put('jwt.sub:' + ASubject,
    RawByteString(IntToStr(DateTimeToUnix(RALDateTimeToGMT(ABefore)))), vTTL);
end;

procedure TRALServerJWTAuth.RevokeId(const AId: StringRAL; AExpires: TDateTime);
var
  vTTL: Int64RAL;
begin
  if (FRevocationStore = nil) or (AId = '') then
    Exit;
  vTTL := 0;
  if AExpires > 0 then
  begin
    vTTL := MilliSecondsBetween(Now, AExpires) + 60000;
    if AExpires < Now then
      Exit; // already dead
  end;
  FRevocationStore.Put('jwt.jti:' + AId, '1', vTTL);
end;

function TRALServerJWTAuth.ResolveRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
begin
  Result := inherited ResolveRoute(ARequest, AResponse);
  if (Result = nil) and FPublishJWKS and RALSameName(ARequest.Query, RALJWKSRoute) then
    Result := TRALRoute(FJWKSRoute);
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
  vReason: StringRAL;
begin
  Result := '';
  vJWT := TRALJWT.Create;
  try
    if CheckToken(vJWT, AToken, vReason) then
    begin
      { signed again with the key that signs now, which may be newer }
      if not PrepareSigner(vJWT) then
        Exit;
      if FRevocationStore <> nil then
      begin
        vJWT.Payload.IssuedAt := Now;
        vJWT.Payload.createNewId;
      end;
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
  if not HasSecretKey(AResponse) then
    Exit;

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
    vResult := CheckToken(vJWT, ARequest.Authorization.AuthString, vReason);
    if vResult then
    begin
      { who the token says: OnValidate may read and change the identity }
      RALIdentityFromClaims(ARequest.Identity, vJWT.Payload, FRolesClaim, ratBearer);
      if Assigned(FOnValidate) then
        FOnValidate(ARequest, AResponse, vJWT.Payload, vResult)
      else if Assigned(FOnValidateGen) then
        FOnValidateGen(ARequest, AResponse, vJWT.Payload, vResult);
      if not vResult then
      begin
        vReason := wmJWTRefused;
        ARequest.Identity.Clear;
      end;
    end;
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

function TRALServerJWTAuth.AttemptOf(ARequest: TRALRequest; AResponse: TRALResponse;
  AResult: TRALAuthResult; AOnOwnRoute: boolean): TRALAuthAttempt;
begin
  Result := raaNone;
  { the login itself: OnGetToken checked whatever credentials came with it -
    a Bearer sent along does not keep a wrong password from counting. Without
    the event every first token is refused, and that is no guess }
  if AOnOwnRoute and LogsIn(ARequest) then
  begin
    if AResult = arAccepted then
      Result := raaPassed
    else if AResult = arUnauthorized then
      Result := raaFailed;
  end
  { nobody guesses a token: expired, forged or refused by OnValidate it says
    nothing about a password - and counting it, eight clients behind one NAT
    whose tokens expired together locked the address out. A good one proves
    the client logged in }
  else if (not AOnOwnRoute) and (ARequest.Authorization.AuthType = ratBearer) and
          (AResult = arAccepted) then
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
    if not PrepareSigner(vJWT) then
      raise Exception.Create(emJWTNoSigningKey);

    vJWT.Payload.AsJSON := AJSONParams;
    if FExpSecs > 0 then
      vJWT.Payload.Expiration := IncSecond(Now, FExpSecs);
    { revocation needs to name the token (jti) and to know its age (iat) }
    if FRevocationStore <> nil then
    begin
      if vJWT.Payload.Id = '' then
        vJWT.Payload.createNewId;
      if vJWT.Payload.IssuedAt <= 0 then
        vJWT.Payload.IssuedAt := Now;
    end;

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

procedure TRALServerBasicAuth.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FPasswords) then
    FPasswords := nil;
end;

procedure TRALServerBasicAuth.SetPasswords(const AValue: TRALPasswordList);
begin
  if FPasswords = AValue then
    Exit;
  if FPasswords <> nil then
    FPasswords.RemoveFreeNotification(Self);
  FPasswords := AValue;
  if FPasswords <> nil then
    FPasswords.FreeNotification(Self);
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

  vAuthBasic := ARequest.Authorization.AsAuthBasic;
  if Assigned(FOnValidate) then
  begin
    vResult := False;
    FOnValidate(ARequest, AResponse, vResult);
  end
  else
  begin
    { both compared in constant time: "<>" stops at the first wrong character,
      and the time it takes to refuse leaks how much of the secret is right }
    vResult := (vAuthBasic <> nil) and
       (Trim(FUserName) <> '') and (Trim(FPassword) <> '') and
       RALSameSecret(vAuthBasic.UserName, FUserName) and
       RALSameSecret(vAuthBasic.Password, FPassword);
    if (not vResult) and (vAuthBasic <> nil) and (FPasswords <> nil) then
      vResult := FPasswords.Verify(vAuthBasic.UserName, vAuthBasic.Password);
  end;

  if not vResult then
  begin
    Error401;
    Exit;
  end;

  { who logged in, for route Roles and policies }
  ARequest.Identity.Clear;
  ARequest.Identity.AuthScheme := ratBasic;
  if vAuthBasic <> nil then
    ARequest.Identity.Name := vAuthBasic.UserName;
  if Assigned(FOnGetRoles) then
    FOnGetRoles(ARequest, ARequest.Identity.Name, ARequest.Identity);
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

function TRALClientJWTAuth.HandleChallenge(ARequest: TRALRequest;
  AResponse: TRALResponse): boolean;
var
  vSent: TRALParam;
begin
  { the token was refused or expired: a new one is fetched before the request
    goes again - what ResetToken always meant to do. But only the token this
    request was refused with, compared under the lock (held by the caller):
    one authenticator serves many clients, and when a token expires the 401s
    of the requests already in flight arrive AFTER one of them has fetched the
    next token. Clearing unconditionally threw that fresh token away, so every
    client in turn went back to /gettoken - one after the other, since the
    fetch holds the lock. The header is what was really sent, written by
    SetAuthHeader }
  vSent := nil;
  if ARequest <> nil then
    vSent := ARequest.Params.GetKind['Authorization', rpkHEADER];
  if (vSent = nil) or (vSent.AsString = 'Bearer ' + FToken) then
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

procedure TRALClientJWTAuth.SetPayload(const AValue: TRALJWTParams);
begin
  { a copy into the object this class owns, under the lock: SetToken rewrites
    that same object on whichever thread a token arrives }
  Lock;
  try
    RALAssignOwned(FPayload, AValue);
  finally
    Unlock;
  end;
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
      try
        vPayload := TRALBase64.Decode(TRALBase64.FromBase64Url(vStr.Strings[1]));
        vOk := True;
      except
        { a payload that is not base64url, or empty, is a token that does not
          parse - see below. The decoder raises on it now, where it answered
          garbage }
        vOk := False;
      end;
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

end.
