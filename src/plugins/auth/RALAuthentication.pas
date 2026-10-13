/// Basic and JWT authentication; the base classes are in RALAuthBase.
unit RALAuthentication;

interface

uses
  Classes, SysUtils, DateUtils, SyncObjs,
  RALToken, RALConsts, RALTypes, RALRoutes, RALBase64, RALTools, RALJson,
  RALRequest, RALParams, RALResponse, RALCustomObjects, RALMIMETypes, RALPlugin,
  RALAuthBase,
  RALStream;

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
    procedure SetPayload(const AValue: TRALJWTParams);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// A 401: the token is dropped, and the request goes again with a new one
    function HandleChallenge(ARequest: TRALRequest; AResponse: TRALResponse): boolean; override;
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
    property Payload: TRALJWTParams read FPayload write SetPayload;
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
    FCookieSameSite: TRALCookieSiteScope;
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
    { False, with the 500 already answered, when SignSecretKey is empty }
    function HasSecretKey(AResponse: TRALResponse): boolean;
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
    /// The raltoken cookie, when no Authorization header came
    procedure DecodeWithoutHeader(ARequest: TRALRequest); override;
    function GetToken(var AJSONParams: StringRAL): StringRAL;
    /// A new token with the claims of AToken and a new expiration; '' when
    /// AToken is not valid. Called directly it does not fire OnRenewToken
    function RenewToken(const AToken: StringRAL; var AJSONParams: StringRAL): StringRAL;
    /// Validation process of the authentication is made here
    procedure Validate(ARequest: TRALRequest; AResponse: TRALResponse); override;
    function AttemptOf(ARequest: TRALRequest; AResponse: TRALResponse;
      AResult: TRALAuthResult; AOnOwnRoute: boolean): TRALAuthAttempt; override;
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

{ TRALServerJWTAuth }

{ A JWT component dropped on a form with SignSecretKey left empty cannot sign
  or check a token: the HMAC refuses an empty key, and the request died with
  "Key must be provided.", which names no setting. Without a key the server
  answers 500 and says which one is missing - a 401 would read as a wrong
  password. A key of blanks counts as none, as for TRALServerBasicAuth }
function TRALServerJWTAuth.HasSecretKey(AResponse: TRALResponse): boolean;
begin
  Result := RALTrim(FSignSecretKey) <> '';
  if not Result then
    AResponse.Answer(HTTP_InternalError, emJWTNoSecretKey, rctTEXTPLAIN);
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
