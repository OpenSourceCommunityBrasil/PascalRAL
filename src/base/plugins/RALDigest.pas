/// Digest authentication (RFC 7616, compatible with RFC 2617): the server
/// plugin and the client authenticator
unit RALDigest;

{ The server keeps no state per nonce: a nonce is its own timestamp plus an
  HMAC of it with a secret of the plugin, so any nonce it issued is checked by
  recomputing, and one that is too old is still recognised as the server's -
  which is what lets it answer stale=true (right credentials, old nonce) and
  the client renew without counting a failure. The only state is the optional
  replay cache: the last nc seen per nonce, pruned as the nonces expire.

  The client reads the challenge from the 401 of its own request
  (HandleChallenge) - there is no extra request just to learn the nonce - and
  counts nc under the authenticator's lock, since one authenticator serves
  several clients at once.

  qop=auth-int (a hash of the body) is not offered: the server would need the
  body as it came off the wire, before decoding, and each engine hands it over
  at a different point. }

interface

uses
  Classes, SysUtils, DateUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALToken, RALJWS, RALHashBase, RALSHA2_32,
  RALRequest, RALResponse, RALParams, RALAuthentication;

type
  /// Credentials of AUserName (as sent; the hash of it when AUserHash). Fill
  /// APassword, or AHA1 = TRALDigest.HashA1(...) to keep no password in clear
  TRALOnDigestCredential = procedure(ARequest: TRALRequest; const AUserName,
    ARealm: StringRAL; AUserHash: boolean; var APassword, AHA1: StringRAL) of object;

  { TRALServerDigest }

  /// Digest authenticator of a server: one challenge per algorithm in
  /// Algorithms, the strongest first
  TRALServerDigest = class(TRALAuthServer)
  private
    FAlgorithms: TRALDigestAlgorithms;
    FLock: TCriticalSection;
    FNonceLifetime: IntegerRAL;
    FOpaque: StringRAL;
    FPassword: StringRAL;
    FRealm: StringRAL;
    FReplayProtection: boolean;
    FSecret: StringRAL;
    FSessionAlgorithms: boolean;
    FUsed: TStringList;
    FUserHash: boolean;
    FUserName: StringRAL;
    FOnGetCredential: TRALOnDigestCredential;
    { True when the nonce is one of ours; AStale when it is, but too old }
    function CheckNonce(const ANonce: StringRAL; out AStale: boolean): boolean;
    { True when ANC is above the last one seen for ANonce; records it }
    function CheckReplay(const ANonce: StringRAL; ANC: Int64RAL): boolean;
    function NewNonce: StringRAL;
    function NonceSignature(const ATimestamp: StringRAL): StringRAL;
    function NonceTime(const ANonce: StringRAL): Int64RAL;
    procedure PruneUsed;
    procedure Refuse(AResponse: TRALResponse; AStale: boolean);
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AUser: StringRAL;
      const APassword: StringRAL); overload;
    destructor Destroy; override;
    /// Validation process of the authentication is made here
    procedure Validate(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    /// The hashes offered, one challenge each: SHA-512-256, SHA-256, MD5 in
    /// that order of preference
    property Algorithms: TRALDigestAlgorithms read FAlgorithms write FAlgorithms
      default [tdaSHA2_256, tdaMD5];
    /// Seconds a nonce is good for; after it the client is told stale=true
    property NonceLifetime: IntegerRAL read FNonceLifetime write FNonceLifetime
      default 300;
    /// Single-user shortcut, as in TRALServerBasicAuth; OnGetCredential takes
    /// precedence
    property Password: StringRAL read FPassword write FPassword;
    property Realm: StringRAL read FRealm write FRealm;
    /// Refuses a request that repeats an nc already seen with its nonce
    property ReplayProtection: boolean read FReplayProtection write FReplayProtection
      default True;
    /// Offers the -sess variants (MD5-sess, SHA-256-sess...) instead
    property SessionAlgorithms: boolean read FSessionAlgorithms
      write FSessionAlgorithms default False;
    /// Asks the client to send H(user:realm) instead of the user name
    property UserHash: boolean read FUserHash write FUserHash default False;
    property UserName: StringRAL read FUserName write FUserName;
    property OnGetCredential: TRALOnDigestCredential read FOnGetCredential
      write FOnGetCredential;
  end;

  { TRALClientDigest }

  /// Digest authenticator for client components
  TRALClientDigest = class(TRALAuthClient)
  private
    FDigest: TRALDigest;
    function GetDigestParams: TRALDigestParams;
    function GetPassword: StringRAL;
    function GetUserName: StringRAL;
    procedure SetPassword(const AValue: StringRAL);
    procedure SetUserName(const AValue: StringRAL);
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AUser: StringRAL;
      const APassword: StringRAL); overload;
    destructor Destroy; override;
    /// Takes the nonce of the 401: the strongest Digest challenge this class
    /// can answer. False when there is none, or when the same nonce came
    /// back without stale=true - the credentials are wrong, and sending them
    /// again would only count one more failure
    function HandleChallenge(AResponse: TRALResponse): boolean; override;
    /// There is a nonce to answer
    function IsAuthenticated: boolean; override;
    procedure SetAuthHeader(AVars: TStringList; AParams: TRALParams); override;

    /// The challenge being answered. The object is rewritten by every 401:
    /// hold Lock while reading it from another thread
    property DigestParams: TRALDigestParams read GetDigestParams;
  published
    property Password: StringRAL read GetPassword write SetPassword;
    property UserName: StringRAL read GetUserName write SetUserName;
  end;

implementation

{ the request-target of a full URL: path and query, as the request line has it }
function RequestTarget(const AURL: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := AURL;
  vInt := Pos(StringRAL('://'), Result);
  if vInt > 0 then
  begin
    Delete(Result, 1, vInt + 2);
    vInt := Pos(StringRAL('/'), Result);
    if vInt > 0 then
      Result := Copy(Result, vInt, Length(Result))
    else
      Result := '/';
  end;
  if Result = '' then
    Result := '/';
end;

function UnixNow: Int64RAL;
begin
  Result := DateTimeToUnix(RALDateTimeToGMT(Now));
end;

{ TRALServerDigest }

constructor TRALServerDigest.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetAuthType(ratDigest);
  FAlgorithms := [tdaSHA2_256, tdaMD5];
  FLock := TCriticalSection.Create;
  FNonceLifetime := 300;
  FRealm := 'RAL';
  FReplayProtection := True;
  FSessionAlgorithms := False;
  FUserHash := False;
  { per instance and never written anywhere: a restart invalidates the nonces
    in flight, and the clients simply get a new one with the next 401 }
  FSecret := RALBase64UrlEncode(RandomBytes(32));
  FOpaque := RALBase64UrlEncode(RandomBytes(16));
  FUsed := TStringList.Create;
  FUsed.Sorted := True;
  FUsed.Duplicates := dupIgnore;
end;

constructor TRALServerDigest.Create(AOwner: TComponent; const AUser,
  APassword: StringRAL);
begin
  Create(AOwner);
  FUserName := AUser;
  FPassword := APassword;
end;

destructor TRALServerDigest.Destroy;
begin
  FreeAndNil(FUsed);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TRALServerDigest.CheckNonce(const ANonce: StringRAL;
  out AStale: boolean): boolean;
var
  vInt: IntegerRAL;
  vTime: Int64RAL;
begin
  Result := False;
  AStale := False;
  vInt := Pos(StringRAL('.'), ANonce);
  if vInt <= 1 then
    Exit;
  if not RALSameSecret(NonceSignature(Copy(ANonce, 1, vInt - 1)),
     Copy(ANonce, vInt + 1, Length(ANonce))) then
    Exit;
  vTime := NonceTime(ANonce);
  if vTime <= 0 then
    Exit;
  Result := True;
  AStale := (UnixNow - vTime > FNonceLifetime) or (vTime - UnixNow > 60);
end;

function TRALServerDigest.CheckReplay(const ANonce: StringRAL; ANC: Int64RAL): boolean;
var
  vInt: IntegerRAL;
begin
  FLock.Acquire;
  try
    PruneUsed;
    vInt := FUsed.IndexOfName(ANonce);
    if vInt >= 0 then
    begin
      Result := ANC > StrToInt64Def(FUsed.ValueFromIndex[vInt], 0);
      if Result then
      begin
        FUsed.Delete(vInt);
        FUsed.Add(ANonce + '=' + IntToStr(ANC));
      end;
    end
    else
    begin
      Result := ANC > 0;
      if Result then
        FUsed.Add(ANonce + '=' + IntToStr(ANC));
    end;
  finally
    FLock.Release;
  end;
end;

function TRALServerDigest.NewNonce: StringRAL;
var
  vStamp: StringRAL;
begin
  vStamp := LowerCase(IntToHex(UnixNow, 12));
  Result := vStamp + '.' + NonceSignature(vStamp);
end;

function TRALServerDigest.NonceSignature(const ATimestamp: StringRAL): StringRAL;
var
  vHash: TRALSHA2_32;
begin
  vHash := TRALSHA2_32.Create;
  try
    vHash.Version := rsv256;
    vHash.OutputType := rhotBase64Url;
    Result := vHash.HMACAsString(ATimestamp + ':' + FRealm, FSecret);
  finally
    vHash.Free;
  end;
end;

function TRALServerDigest.NonceTime(const ANonce: StringRAL): Int64RAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  vInt := Pos(StringRAL('.'), ANonce);
  if vInt > 1 then
    Result := StrToInt64Def('$' + Copy(ANonce, 1, vInt - 1), 0);
end;

procedure TRALServerDigest.PruneUsed;
var
  vInt: IntegerRAL;
  vLimit: Int64RAL;
begin
  // under FLock
  if FUsed.Count = 0 then
    Exit;
  { a nonce past its lifetime is answered stale anyway, before its nc is ever
    looked at: its entry can go }
  vLimit := UnixNow - FNonceLifetime - 60;
  for vInt := Pred(FUsed.Count) downto 0 do
    if NonceTime(FUsed.Names[vInt]) < vLimit then
      FUsed.Delete(vInt);
end;

procedure TRALServerDigest.Refuse(AResponse: TRALResponse; AStale: boolean);
const
  cOrder: array[0..2] of TRALDigestAlgorithm = (tdaSHA2_512, tdaSHA2_256, tdaMD5);
var
  vInt: IntegerRAL;
  vNonce, vText: StringRAL;
begin
  if AResponse.StatusCode < HTTP_BadRequest then
    AResponse.Answer(HTTP_Unauthorized);

  { one challenge per algorithm, the strongest first, all with the same nonce:
    a client takes the first it can answer (RFC 7616 3.7) }
  vNonce := NewNonce;
  for vInt := 0 to High(cOrder) do
    if cOrder[vInt] in FAlgorithms then
    begin
      vText := 'Digest realm=' + RALQuoteString(FRealm) + ', qop="auth"' +
        ', algorithm=' + TRALDigest.AlgorithmName(cOrder[vInt], FSessionAlgorithms) +
        ', nonce=' + RALQuoteString(vNonce) + ', opaque=' + RALQuoteString(FOpaque);
      if AStale then
        vText := vText + ', stale=true';
      if FUserHash then
        vText := vText + ', userhash=true';
      vText := vText + ', charset=UTF-8';
      RALAddChallenge(AResponse, vText);
    end;
end;

procedure TRALServerDigest.Validate(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vList: TStringList;
  vUser, vRealm, vNonce, vNC, vCNonce, vQop, vURI, vPath: StringRAL;
  vPassword, vHA1, vExpected: StringRAL;
  vAlgorithm: TRALDigestAlgorithm;
  vSess, vStale, vIsHash: boolean;
  vInt: IntegerRAL;
begin
  AResponse.StatusCode := HTTP_OK;
  if ARequest.Authorization.AuthType <> ratDigest then
  begin
    Refuse(AResponse, False);
    Exit;
  end;

  vList := TStringList.Create;
  try
    RALParseAuthParams(ARequest.Authorization.AuthString, vList);

    vUser := vList.Values['username'];
    vRealm := vList.Values['realm'];
    vNonce := vList.Values['nonce'];
    vNC := LowerCase(vList.Values['nc']);
    vCNonce := vList.Values['cnonce'];
    vQop := vList.Values['qop'];
    vURI := vList.Values['uri'];
    vIsHash := SameText(vList.Values['userhash'], 'true');

    vAlgorithm := tdaMD5;
    vSess := False;
    if (vList.Values['algorithm'] <> '') and
       not TRALDigest.ParseAlgorithm(vList.Values['algorithm'], vAlgorithm, vSess) then
    begin
      Refuse(AResponse, False);
      Exit;
    end;

    { only what this server offered: its realm, its algorithms, qop=auth, and
      the request it is looking at - a response computed for another uri or
      another opaque is not an answer to this request }
    vPath := vURI;
    vInt := Pos(StringRAL('?'), vPath);
    if vInt > 0 then
      vPath := Copy(vPath, 1, vInt - 1);
    if (vUser = '') or (vRealm <> FRealm) or (not (vAlgorithm in FAlgorithms)) or
       (vSess <> FSessionAlgorithms) or (vQop <> 'auth') or (vNC = '') or
       (vCNonce = '') or (vIsHash <> FUserHash) or
       (not RALSameName(FixRoute(vPath), FixRoute(ARequest.Query))) or
       (vList.Values['opaque'] <> FOpaque) then
    begin
      Refuse(AResponse, False);
      Exit;
    end;

    if not CheckNonce(vNonce, vStale) then
    begin
      Refuse(AResponse, False);
      Exit;
    end;

    vPassword := '';
    vHA1 := '';
    if Assigned(FOnGetCredential) then
      FOnGetCredential(ARequest, vUser, vRealm, vIsHash, vPassword, vHA1)
    else if (FUserName <> '') and
      ((vIsHash and RALSameSecret(vUser, TRALDigest.UserHashOf(vAlgorithm, FUserName,
        FRealm))) or ((not vIsHash) and RALSameSecret(vUser, FUserName))) then
    begin
      vUser := FUserName;
      vPassword := FPassword;
    end;

    if vHA1 = '' then
    begin
      if vPassword = '' then
      begin
        Refuse(AResponse, False);
        Exit;
      end;
      vHA1 := TRALDigest.HashA1(vAlgorithm, vUser, FRealm, vPassword);
    end;

    vExpected := TRALDigest.ResponseFor(vAlgorithm, vSess, LowerCase(vHA1), vNonce,
      vNC, vCNonce, vQop, RALMethodToHTTPMethod(ARequest.Method), vURI);
    if not RALSameSecret(vExpected, LowerCase(vList.Values['response'])) then
    begin
      Refuse(AResponse, False);
      Exit;
    end;

    { the credentials are right: an old nonce is only stale, and the client
      renews it without counting a failure }
    if vStale then
    begin
      Refuse(AResponse, True);
      Exit;
    end;

    if FReplayProtection and
       not CheckReplay(vNonce, StrToInt64Def('$' + vNC, 0)) then
      Refuse(AResponse, False);
  finally
    vList.Free;
  end;
end;

{ TRALClientDigest }

constructor TRALClientDigest.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetAuthType(ratDigest);
  FDigest := TRALDigest.Create;
end;

constructor TRALClientDigest.Create(AOwner: TComponent; const AUser,
  APassword: StringRAL);
begin
  Create(AOwner);
  FDigest.UserName := AUser;
  FDigest.Password := APassword;
end;

destructor TRALClientDigest.Destroy;
begin
  FreeAndNil(FDigest);
  inherited Destroy;
end;

function TRALClientDigest.GetDigestParams: TRALDigestParams;
begin
  Result := FDigest.Params;
end;

function TRALClientDigest.GetPassword: StringRAL;
begin
  Result := FDigest.Password;
end;

function TRALClientDigest.GetUserName: StringRAL;
begin
  Result := FDigest.UserName;
end;

function TRALClientDigest.HandleChallenge(AResponse: TRALResponse): boolean;
const
  cOrder: array[0..2] of TRALDigestAlgorithm = (tdaSHA2_512, tdaSHA2_256, tdaMD5);
var
  vList: TStringList;
  vReader: TRALDigest;
  vBest, vRank, vOrder, vInt: IntegerRAL;
  vOldNonce: StringRAL;
  vParam: TRALParam;
begin
  Result := False;
  vParam := AResponse.Params.GetKind['WWW-Authenticate', rpkHEADER];
  if vParam = nil then
    Exit;

  vList := TStringList.Create;
  vReader := TRALDigest.Create;
  try
    RALSplitChallenges(vParam.AsString, vList);
    vBest := -1;
    vRank := MaxInt;
    for vInt := 0 to Pred(vList.Count) do
      if vReader.Load(vList.Strings[vInt]) then
        for vOrder := 0 to High(cOrder) do
          if (cOrder[vOrder] = vReader.Params.Algorithm) and (vOrder < vRank) then
          begin
            vRank := vOrder;
            vBest := vInt;
          end;
    if vBest < 0 then
      Exit;

    vReader.Load(vList.Strings[vBest]);
    vOldNonce := FDigest.Params.Nonce;
    { the nonce the credentials were just refused with, and not stale: they
      are wrong, and one more try is one more failure counted against them }
    if (vOldNonce <> '') and (vOldNonce = vReader.Params.Nonce) and
       (not vReader.Params.Stale) then
      Exit;
    FDigest.Params.Assign(vReader.Params);
    FDigest.Params.NC := 0;
    Result := True;
  finally
    vReader.Free;
    vList.Free;
  end;
end;

function TRALClientDigest.IsAuthenticated: boolean;
begin
  Lock;
  try
    Result := FDigest.Params.Nonce <> '';
  finally
    Unlock;
  end;
end;

procedure TRALClientDigest.SetAuthHeader(AVars: TStringList; AParams: TRALParams);
var
  vValue: StringRAL;
begin
  { nc counts the requests made with the nonce, across every client sharing
    this authenticator: it is taken and the header built in one go under the
    lock, or two threads send the same nc and the server refuses one as a
    replay }
  Lock;
  try
    if FDigest.Params.Nonce = '' then
      Exit;
    FDigest.Params.NC := FDigest.Params.NC + 1;
    FDigest.Method := AVars.Values['method'];
    FDigest.URL := RequestTarget(AVars.Values['url']);
    vValue := FDigest.Authorization;
  finally
    Unlock;
  end;
  AParams.AddParam('Authorization', vValue, rpkHEADER);
end;

procedure TRALClientDigest.SetPassword(const AValue: StringRAL);
begin
  FDigest.Password := AValue;
end;

procedure TRALClientDigest.SetUserName(const AValue: StringRAL);
begin
  FDigest.UserName := AValue;
end;

end.
