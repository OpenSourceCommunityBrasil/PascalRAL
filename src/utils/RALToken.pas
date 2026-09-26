/// Token structures of the authenticators: Basic credentials, JWT and Digest,
/// and the parser of the auth-params they travel in
unit RALToken;

interface

uses
  Classes, SysUtils, DateUtils,
  RALTypes, RALConsts, RALSHA2_32, RALSHA2_64, RALHashBase, RALBase64, RALMD5,
  RALJson, RALTools, RALJWS;

type
  /// Digest hashes: MD5, SHA-256 and SHA-512-256 (tdaSHA2_512, the name it
  /// always had). Each also has a -sess variant, TRALDigestParams.SessAlgorithm
  TRALDigestAlgorithm = (tdaMD5, tdaSHA2_256, tdaSHA2_512);
  TRALDigestAlgorithms = set of TRALDigestAlgorithm;
  /// HS* are HMAC with SignSecretKey; RS* (RSA) and ES* (ECDSA) sign with the
  /// private key of TRALJWT.SignKey and verify with its public key
  TRALJWTAlgorithm = (tjaHSHA256, tjaHSHA384, tjaHSHA512, tjaRS256, tjaRS384,
    tjaRS512, tjaES256, tjaES384);

  { TRALAuthBasic }

  TRALAuthBasic = class
  private
    FAuthString: StringRAL;
    FPassword: StringRAL;
    FUserName: StringRAL;
  protected
    procedure SetAuthString(const AValue: StringRAL);
  published
    property AuthString: StringRAL read FAuthString write SetAuthString;
    property Password: StringRAL read FPassword write FPassword;
    property UserName: StringRAL read FUserName write FUserName;
  end;

  { TRALJWTHeader }

  /// Class for header definitions of JWT Token
  TRALJWTHeader = class(TPersistent)
  private
    FAlgorithm: TRALJWTAlgorithm;
    FAlgorithmKnown: boolean;
    FHeaderType: StringRAL;
    FKeyID: StringRAL;
  protected
    function GetAsJSON: StringRAL;
    procedure Initialize;
    procedure SetAsJSON(const AValue: StringRAL);
  public
    constructor Create;
    procedure createKeyID;

    /// False when the alg of a token read is none this class knows: such a
    /// token is never valid
    property AlgorithmKnown: boolean read FAlgorithmKnown;
    property AsJSON: StringRAL read GetAsJSON write SetAsJSON;
  published
    property Algorithm: TRALJWTAlgorithm read FAlgorithm write FAlgorithm;
    property HeaderType: StringRAL read FHeaderType;
    property KeyID: StringRAL read FKeyID write FKeyID;
  end;

  { TRALJWTParams }

  /// Class for "body" or Param part definitions of JWT Token
  TRALJWTParams = class(TPersistent)
  private
    FAudience: StringRAL;
    FCustomClaims: TStringList;
    FExpiration: TDateTime;
    FId: StringRAL;
    FIssuedAt: TDateTime;
    FIssuer: StringRAL;
    FNotBefore: TDateTime;
    FSubject: StringRAL;
  protected
    function GetAsJSON: StringRAL;
    procedure SetAsJSON(const AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;
    /// Sets a custom claim; one already there with the same key is replaced
    procedure AddClaim(const AKey: StringRAL; const AValue: StringRAL);
    procedure Clear;
    procedure createNewId;
    procedure DelClaim(const AKey: StringRAL);
    function GetClaim(const AKey: StringRAL): StringRAL;
    /// Whether the aud claim names AAudience - aud may be one string or a
    /// JSON array of them
    function HasAudience(const AAudience: StringRAL): boolean;

    property AsJSON: StringRAL read GetAsJSON write SetAsJSON;
  published
    property Audience: StringRAL read FAudience write FAudience;
    property Expiration: TDateTime read FExpiration write FExpiration;
    property Id: StringRAL read FId write FId;
    property IssuedAt: TDateTime read FIssuedAt write FIssuedAt;
    property Issuer: StringRAL read FIssuer write FIssuer;
    property NotBefore: TDateTime read FNotBefore write FNotBefore;
    property Subject: StringRAL read FSubject write FSubject;
  end;

  { TRALJWT }

  /// Class for the JWT structure definition
  TRALJWT = class
  private
    FHeader: TRALJWTHeader;
    FLeeway: IntegerRAL;
    FPayload: TRALJWTParams;
    FSignature: StringRAL;
    FSignKey: TRALJWSKey;
    FSignSecretKey: StringRAL;
    FSigningInput: StringRAL;
    FToken: StringRAL;
  protected
    function CreateToken(AHeader, APayload: StringRAL;
      var ASignature: StringRAL): StringRAL;
    function GetToken: StringRAL;
    procedure SetToken(AValue: StringRAL);
    /// The base64url signature of AInput with the header's algorithm
    function SignInput(const AInput: StringRAL): StringRAL;
  public
    constructor Create;
    destructor Destroy; override;
    /// The alg name of AAlgorithm ('HS256', 'RS256', 'ES256'...)
    class function AlgorithmName(AAlgorithm: TRALJWTAlgorithm): StringRAL;
    /// Checks the token: the algorithm is the one this instance is set to (a
    /// token cannot choose it), the signature is over the token's own bytes,
    /// and exp/nbf hold, give or take Leeway
    function IsValidToken(const AValue: StringRAL = ''): boolean;
    /// Whether the signature of the token read is good, dates aside
    function IsValidSignature: boolean;
  published
    property Header: TRALJWTHeader read FHeader write FHeader;
    /// Seconds of clock difference tolerated on exp and nbf
    property Leeway: IntegerRAL read FLeeway write FLeeway;
    property Payload: TRALJWTParams read FPayload write FPayload;
    property Signature: StringRAL read FSignature;
    /// The key of RS*/ES* tokens: private to sign, public (or private) to
    /// verify. Not owned
    property SignKey: TRALJWSKey read FSignKey write FSignKey;
    property SignSecretKey: StringRAL read FSignSecretKey write FSignSecretKey;
    property Token: StringRAL read GetToken write SetToken;
  end;

  { TRALDigestParams }

  /// What a Digest challenge says (RFC 7616), and the client's counters
  TRALDigestParams = class(TPersistent)
  private
    FAlgorithm: TRALDigestAlgorithm;
    FCharset: StringRAL;
    FCNonce: StringRAL;
    FDomain: StringRAL;
    FNC: integer;
    FNonce: StringRAL;
    FOpaque: StringRAL;
    FQop: StringRAL;
    FRealm: StringRAL;
    FSessAlgorithm: boolean;
    FStale: boolean;
    FUserHash: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Clear;
  published
    property Algorithm: TRALDigestAlgorithm read FAlgorithm write FAlgorithm;
    property Charset: StringRAL read FCharset write FCharset;
    property CNonce: StringRAL read FCNonce write FCNonce;
    property Domain: StringRAL read FDomain write FDomain;
    /// Requests already sent with this nonce
    property NC: integer read FNC write FNC;
    property Nonce: StringRAL read FNonce write FNonce;
    property Opaque: StringRAL read FOpaque write FOpaque;
    /// The ONE qop chosen from what the challenge offered: 'auth', or '' when
    /// the server offered none (RFC 2069)
    property Qop: StringRAL read FQop write FQop;
    property Realm: StringRAL read FRealm write FRealm;
    property SessAlgorithm: boolean read FSessAlgorithm write FSessAlgorithm;
    /// The challenge said stale=true: the credentials were right, the nonce
    /// had expired
    property Stale: boolean read FStale write FStale;
    property UserHash: boolean read FUserHash write FUserHash;
  end;

  { TRALDigest }

  /// Digest computations (RFC 7616, answers RFC 2617 and 2069 servers too)
  TRALDigest = class
  private
    FEntityBody: StringRAL;
    FMethod: StringRAL;
    FParams: TRALDigestParams;
    FPassword: StringRAL;
    FURL: StringRAL;
    FUserName: StringRAL;
  public
    constructor Create;
    destructor Destroy; override;
    /// 'MD5', 'SHA-256', 'SHA-512-256', with '-sess' when ASess
    class function AlgorithmName(AAlgorithm: TRALDigestAlgorithm;
      ASess: boolean = False): StringRAL;
    /// The Authorization value ("Digest username=..., response=...") of the
    /// request in Method and URL, answering Params with Params.NC as nc and a
    /// new random cnonce
    function Authorization: StringRAL;
    /// Lowercase hex hash of AValue
    class function Hash(AAlgorithm: TRALDigestAlgorithm;
      const AValue: StringRAL): StringRAL;
    /// H(A1) = H(user:realm:password) - what a server may keep instead of the
    /// password
    class function HashA1(AAlgorithm: TRALDigestAlgorithm; const AUser, ARealm,
      APassword: StringRAL): StringRAL;
    /// Reads a Digest challenge - with or without the "Digest" in front; False
    /// when it is not a Digest challenge this class can answer
    function Load(const AChallenge: StringRAL): boolean;
    /// Reads an algorithm name; False for one not supported
    class function ParseAlgorithm(const AName: StringRAL;
      out AAlgorithm: TRALDigestAlgorithm; out ASess: boolean): boolean;
    /// The response value. AHA1 is H(A1) of the plain algorithm; the -sess
    /// variant is derived here. AQop '' is the RFC 2069 form
    class function ResponseFor(AAlgorithm: TRALDigestAlgorithm; ASess: boolean;
      const AHA1, ANonce, ANC, ACNonce, AQop, AMethod, AURI: StringRAL): StringRAL;
    /// The username a userhash challenge wants: H(user:realm)
    class function UserHashOf(AAlgorithm: TRALDigestAlgorithm;
      const AUser, ARealm: StringRAL): StringRAL;
  published
    /// The body, for qop=auth-int (not offered yet by TRALServerDigest)
    property EntityBody: StringRAL read FEntityBody write FEntityBody;
    property Method: StringRAL read FMethod write FMethod;
    property Params: TRALDigestParams read FParams write FParams;
    property Password: StringRAL read FPassword write FPassword;
    /// The request-target: path and query, as it goes in the request line
    property URL: StringRAL read FURL write FURL;
    property UserName: StringRAL read FUserName write FUserName;
  end;

/// Reads the auth-params of an Authorization or WWW-Authenticate value into
/// AList as name=value, quoted-strings unquoted and unescaped (RFC 9110 11.2).
/// Returns the scheme when the value starts with one ('Digest'), else ''
function RALParseAuthParams(const AValue: StringRAL; AList: TStrings): StringRAL;
/// A WWW-Authenticate value may carry several challenges ("Basic realm=x,
/// Digest realm=y, nonce=z"): one per line of AList, each starting with its
/// scheme
procedure RALSplitChallenges(const AValue: StringRAL; AList: TStrings);
/// AValue as a quoted-string: in quotes, with '\' before '"' and '\'
function RALQuoteString(const AValue: StringRAL): StringRAL;

implementation

{ auth-params }

function IsTokenChar(AChar: AnsiChar): boolean;
begin
  Result := not CharInSet(AChar, [#0..' ', '"', ',', '=', ';', #127]);
end;

type
  { one piece of an auth header: a bare token (a scheme, or a token68) or a
    name=value pair }
  TRALAuthPiece = record
    First: IntegerRAL;
    Name: StringRAL;
    Value: StringRAL;
    IsPair: boolean;
  end;

{ the next piece from AIndex on; False at the end }
function NextPiece(const AValue: StringRAL; var AIndex: IntegerRAL;
  out APiece: TRALAuthPiece): boolean;
var
  vLast, vStart: IntegerRAL;
  { the bytes of a UTF8String, on every compiler - CharRAL is Char (UTF-16) on
    the Delphis before UTF8Char }
  vChar: AnsiChar;
begin
  Result := False;
  vLast := RALHighStr(AValue);
  while (AIndex <= vLast) and CharInSet(AValue[AIndex], [' ', #9, ',']) do
    Inc(AIndex);
  if AIndex > vLast then
    Exit;

  APiece.First := AIndex;
  APiece.Name := '';
  APiece.Value := '';
  APiece.IsPair := False;

  vStart := AIndex;
  while (AIndex <= vLast) and IsTokenChar(AValue[AIndex]) do
    Inc(AIndex);
  APiece.Name := Copy(AValue, vStart - POSINISTR + 1, AIndex - vStart);

  { a token68 (Basic's credentials) may end in '=' padding: '=' followed by
    another '=', a ',' or the end is padding, not a pair }
  vStart := AIndex;
  while (AIndex <= vLast) and CharInSet(AValue[AIndex], [' ', #9]) do
    Inc(AIndex);
  if (AIndex <= vLast) and (AValue[AIndex] = '=') and
     not ((AIndex < vLast) and (AValue[AIndex + 1] = '=')) and
     not ((AIndex = vLast) or CharInSet(AValue[AIndex + 1], [','])) then
  begin
    APiece.IsPair := True;
    Inc(AIndex);
    while (AIndex <= vLast) and CharInSet(AValue[AIndex], [' ', #9]) do
      Inc(AIndex);
    if (AIndex <= vLast) and (AValue[AIndex] = '"') then
    begin
      Inc(AIndex);
      while (AIndex <= vLast) and (AValue[AIndex] <> '"') do
      begin
        vChar := AValue[AIndex];
        if (vChar = '\') and (AIndex < vLast) then
        begin
          Inc(AIndex);
          vChar := AValue[AIndex];
        end;
        APiece.Value := APiece.Value + vChar;
        Inc(AIndex);
      end;
      Inc(AIndex); // the closing quote
    end
    else
    begin
      vStart := AIndex;
      while (AIndex <= vLast) and not CharInSet(AValue[AIndex], [',', ' ', #9]) do
        Inc(AIndex);
      APiece.Value := Copy(AValue, vStart - POSINISTR + 1, AIndex - vStart);
    end;
  end
  else
  begin
    AIndex := vStart;
    while (AIndex <= vLast) and (AValue[AIndex] = '=') do
    begin
      APiece.Name := APiece.Name + '=';
      Inc(AIndex);
    end;
  end;
  Result := True;
end;

function RALParseAuthParams(const AValue: StringRAL; AList: TStrings): StringRAL;
var
  vIndex: IntegerRAL;
  vPiece: TRALAuthPiece;
  vFirst: boolean;
begin
  Result := '';
  AList.Clear;
  vIndex := POSINISTR;
  vFirst := True;
  while NextPiece(AValue, vIndex, vPiece) do
  begin
    if vPiece.IsPair then
      AList.Add(LowerCase(vPiece.Name) + '=' + vPiece.Value)
    else if vFirst then
      Result := vPiece.Name
    else
      Break; // the next challenge
    vFirst := False;
  end;
end;

procedure RALSplitChallenges(const AValue: StringRAL; AList: TStrings);
var
  vIndex, vStart: IntegerRAL;
  vPiece: TRALAuthPiece;

  procedure Flush(AUntil: IntegerRAL);
  var
    vText: StringRAL;
  begin
    if vStart < 0 then
      Exit;
    vText := Trim(Copy(AValue, vStart - POSINISTR + 1, AUntil - vStart));
    while (vText <> '') and (vText[RALHighStr(vText)] = ',') do
      vText := Trim(Copy(vText, 1, Length(vText) - 1));
    if vText <> '' then
      AList.Add(vText);
  end;

begin
  AList.Clear;
  vStart := -1;
  vIndex := POSINISTR;
  while NextPiece(AValue, vIndex, vPiece) do
  begin
    { a bare token right after a scheme is its token68, not a new scheme }
    if (not vPiece.IsPair) and ((vStart < 0) or (Pos('=', vPiece.Name) = 0)) then
    begin
      if (vStart >= 0) and (Pos(StringRAL(' '), Trim(Copy(AValue, vStart - POSINISTR + 1,
          vPiece.First - vStart))) = 0) then
        Continue; // "Scheme token68": the token belongs to the scheme
      Flush(vPiece.First);
      vStart := vPiece.First;
    end;
  end;
  Flush(RALHighStr(AValue) + 1);
end;

function RALQuoteString(const AValue: StringRAL): StringRAL;
begin
  Result := StringReplace(AValue, '\', '\\', [rfReplaceAll]);
  Result := '"' + StringReplace(Result, '"', '\"', [rfReplaceAll]) + '"';
end;

{ TRALDigestParams }

procedure TRALDigestParams.AssignTo(Dest: TPersistent);
var
  vDest: TRALDigestParams;
begin
  if Dest is TRALDigestParams then
  begin
    vDest := TRALDigestParams(Dest);
    vDest.Algorithm := FAlgorithm;
    vDest.Charset := FCharset;
    vDest.CNonce := FCNonce;
    vDest.Domain := FDomain;
    vDest.NC := FNC;
    vDest.Nonce := FNonce;
    vDest.Opaque := FOpaque;
    vDest.Qop := FQop;
    vDest.Realm := FRealm;
    vDest.SessAlgorithm := FSessAlgorithm;
    vDest.Stale := FStale;
    vDest.UserHash := FUserHash;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TRALDigestParams.Clear;
begin
  FAlgorithm := tdaMD5;
  FCharset := '';
  FCNonce := '';
  FDomain := '';
  FNC := 0;
  FNonce := '';
  FOpaque := '';
  FQop := '';
  FRealm := '';
  FSessAlgorithm := False;
  FStale := False;
  FUserHash := False;
end;

{ TRALDigest }

constructor TRALDigest.Create;
begin
  inherited;
  FParams := TRALDigestParams.Create;
end;

destructor TRALDigest.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

class function TRALDigest.AlgorithmName(AAlgorithm: TRALDigestAlgorithm;
  ASess: boolean): StringRAL;
begin
  case AAlgorithm of
    tdaSHA2_256: Result := 'SHA-256';
    tdaSHA2_512: Result := 'SHA-512-256';
  else
    Result := 'MD5';
  end;
  if ASess then
    Result := Result + '-sess';
end;

function TRALDigest.Authorization: StringRAL;
var
  vNC, vHA1, vUser: StringRAL;
begin
  vNC := LowerCase(IntToHex(FParams.NC, 8));
  { a cnonce the server cannot predict: it is what makes the client's half of
    the response fresh. It used to be the hash of nc - the same for everyone }
  FParams.CNonce := RALBase64UrlEncode(RandomBytes(16));

  vHA1 := HashA1(FParams.Algorithm, FUserName, FParams.Realm, FPassword);
  vUser := FUserName;
  if FParams.UserHash then
    vUser := UserHashOf(FParams.Algorithm, FUserName, FParams.Realm);

  Result := 'Digest username=' + RALQuoteString(vUser) +
    ', realm=' + RALQuoteString(FParams.Realm) +
    ', nonce=' + RALQuoteString(FParams.Nonce) +
    ', uri=' + RALQuoteString(FURL) +
    ', algorithm=' + AlgorithmName(FParams.Algorithm, FParams.SessAlgorithm) +
    ', response=' + RALQuoteString(ResponseFor(FParams.Algorithm,
      FParams.SessAlgorithm, vHA1, FParams.Nonce, vNC, FParams.CNonce, FParams.Qop,
      FMethod, FURL));
  if FParams.Qop <> '' then
    Result := Result + ', qop=' + FParams.Qop + ', nc=' + vNC +
      ', cnonce=' + RALQuoteString(FParams.CNonce);
  if FParams.Opaque <> '' then
    Result := Result + ', opaque=' + RALQuoteString(FParams.Opaque);
  if FParams.UserHash then
    Result := Result + ', userhash=true';
end;

class function TRALDigest.Hash(AAlgorithm: TRALDigestAlgorithm;
  const AValue: StringRAL): StringRAL;
var
  vHash: TRALHashBase;
begin
  case AAlgorithm of
    tdaSHA2_256:
    begin
      vHash := TRALSHA2_32.Create;
      TRALSHA2_32(vHash).Version := rsv256;
    end;
    tdaSHA2_512:
    begin
      vHash := TRALSHA2_64.Create;
      TRALSHA2_64(vHash).Version := rsv512_256;
    end;
  else
    vHash := TRALMD5.Create;
  end;
  try
    vHash.OutputType := rhotHex;
    Result := LowerCase(vHash.HashAsString(AValue));
  finally
    vHash.Free;
  end;
end;

class function TRALDigest.HashA1(AAlgorithm: TRALDigestAlgorithm; const AUser,
  ARealm, APassword: StringRAL): StringRAL;
begin
  Result := Hash(AAlgorithm, AUser + ':' + ARealm + ':' + APassword);
end;

function TRALDigest.Load(const AChallenge: StringRAL): boolean;
var
  vList: TStringList;
  vScheme, vQop, vItem: StringRAL;
  vAlgorithm: TRALDigestAlgorithm;
  vSess: boolean;
  vInt: IntegerRAL;
begin
  Result := False;
  vList := TStringList.Create;
  try
    vScheme := RALParseAuthParams(AChallenge, vList);
    if (vScheme <> '') and not SameText(vScheme, 'Digest') then
      Exit;

    vAlgorithm := tdaMD5;
    vSess := False;
    if (vList.Values['algorithm'] <> '') and
       not ParseAlgorithm(vList.Values['algorithm'], vAlgorithm, vSess) then
      Exit;
    if vList.Values['nonce'] = '' then
      Exit;

    FParams.Clear;
    FParams.Algorithm := vAlgorithm;
    FParams.SessAlgorithm := vSess;
    FParams.Realm := vList.Values['realm'];
    FParams.Nonce := vList.Values['nonce'];
    FParams.Opaque := vList.Values['opaque'];
    FParams.Domain := vList.Values['domain'];
    FParams.Charset := vList.Values['charset'];
    FParams.Stale := SameText(vList.Values['stale'], 'true');
    FParams.UserHash := SameText(vList.Values['userhash'], 'true');

    { qop is a list the server offers; the response names ONE. auth is the
      one this client does - auth-int needs the body as it goes on the wire }
    vQop := vList.Values['qop'];
    if vQop <> '' then
    begin
      vQop := StringReplace(vQop, ' ', '', [rfReplaceAll]) + ',';
      while vQop <> '' do
      begin
        vInt := Pos(StringRAL(','), vQop);
        vItem := LowerCase(Copy(vQop, 1, vInt - 1));
        Delete(vQop, 1, vInt);
        if vItem = 'auth' then
          FParams.Qop := 'auth';
      end;
      if FParams.Qop = '' then
        Exit; // auth-int only
    end;
    Result := True;
  finally
    vList.Free;
  end;
end;

class function TRALDigest.ParseAlgorithm(const AName: StringRAL;
  out AAlgorithm: TRALDigestAlgorithm; out ASess: boolean): boolean;
var
  vName: StringRAL;
begin
  vName := UpperCase(Trim(AName));
  ASess := Copy(vName, Length(vName) - 4, 5) = '-SESS';
  if ASess then
    vName := Copy(vName, 1, Length(vName) - 5);
  Result := True;
  if vName = 'MD5' then
    AAlgorithm := tdaMD5
  else if vName = 'SHA-256' then
    AAlgorithm := tdaSHA2_256
  else if vName = 'SHA-512-256' then
    AAlgorithm := tdaSHA2_512
  else
  begin
    AAlgorithm := tdaMD5;
    Result := False;
  end;
end;

class function TRALDigest.ResponseFor(AAlgorithm: TRALDigestAlgorithm;
  ASess: boolean; const AHA1, ANonce, ANC, ACNonce, AQop, AMethod,
  AURI: StringRAL): StringRAL;
var
  vHA1, vHA2: StringRAL;
begin
  vHA1 := AHA1;
  if ASess then
    vHA1 := Hash(AAlgorithm, vHA1 + ':' + ANonce + ':' + ACNonce);
  vHA2 := Hash(AAlgorithm, AMethod + ':' + AURI);
  if AQop <> '' then
    Result := Hash(AAlgorithm, vHA1 + ':' + ANonce + ':' + ANC + ':' + ACNonce + ':' +
      AQop + ':' + vHA2)
  else
    Result := Hash(AAlgorithm, vHA1 + ':' + ANonce + ':' + vHA2);
end;

class function TRALDigest.UserHashOf(AAlgorithm: TRALDigestAlgorithm;
  const AUser, ARealm: StringRAL): StringRAL;
begin
  Result := Hash(AAlgorithm, AUser + ':' + ARealm);
end;

{ TRALJWTHeader }

constructor TRALJWTHeader.Create;
begin
  Initialize;
end;

procedure TRALJWTHeader.createKeyID;
var
  vBytes: TBytes;
begin
  vBytes := randomBytes(8);
  FKeyID := TRALBase64.Encode(vBytes);
end;

function TRALJWTHeader.GetAsJSON: StringRAL;
var
  vJson: TRALJSONObject;
begin
  vJson := TRALJSONObject.Create;
  try
    vJson.Add('typ', FHeaderType);
    vJson.Add('alg', TRALJWT.AlgorithmName(FAlgorithm));

    if FKeyID <> '' then
      vJson.Add('kid', FKeyID);

    Result := vJson.ToJSON;
  finally
    FreeAndNil(vJson);
  end;
end;

procedure TRALJWTHeader.Initialize;
begin
  FHeaderType := 'JWT';
  FAlgorithm := tjaHSHA256;
  FAlgorithmKnown := True;
  FKeyID := '';
end;

procedure TRALJWTHeader.SetAsJSON(const AValue: StringRAL);
var
  vJson: TRALJSONObject;
  vInt: IntegerRAL;
  vName: StringRAL;
  vValue: TRALJSONValue;
  vAux1: StringRAL;
  vAlg: TRALJWTAlgorithm;
begin
  vJson := TRALJSONObject(TRALJSON.ParseJSON(AValue));
  try
    if vJson <> nil then
    begin
      Initialize;
      vInt := 0;
      while vInt < vJson.Count do
      begin
        vName := vJson.GetName(vInt);
        vValue := vJson.Get(vInt);
        if RALSameName(vName, 'typ') then
        begin
          FHeaderType := vValue.AsString;
        end
        else if RALSameName(vName, 'alg') then
        begin
          { an alg this class does not know ("none", say) is remembered as
            unknown, never read as HS256: IsValidToken refuses it }
          vAux1 := vValue.AsString;
          FAlgorithmKnown := False;
          for vAlg := Low(TRALJWTAlgorithm) to High(TRALJWTAlgorithm) do
            if vAux1 = TRALJWT.AlgorithmName(vAlg) then
            begin
              FAlgorithm := vAlg;
              FAlgorithmKnown := True;
            end;
        end
        else if RALSameName(vName, 'kid') then
        begin
          FKeyID := vValue.AsString;
        end;

        vInt := vInt + 1;
      end;
    end;
  finally
    FreeAndNil(vJson);
  end;
end;

{ TRALJWTParams }

procedure TRALJWTParams.AddClaim(const AKey, AValue: StringRAL);
begin
  { a claim set twice is replaced, not duplicated: the list is sorted by the
    whole "key=value" line, so a second value for the same key went in next to
    the first and the token carried the key twice - invalid JSON, read back as
    whichever came first. Renewing a token (OnRenewToken) is exactly that }
  DelClaim(AKey);
  FCustomClaims.Add(AKey + '=' + AValue);
end;

constructor TRALJWTParams.Create;
begin
  inherited Create;
  FCustomClaims := TStringList.Create;
  FCustomClaims.Sorted := True;
  Clear;
end;

procedure TRALJWTParams.createNewId;
var
  vBytes: TBytes;
begin
  vBytes := RandomBytes(10);
  FId := TRALBase64.Encode(vBytes);
end;

procedure TRALJWTParams.DelClaim(const AKey: StringRAL);
var
  vInt: IntegerRAL;
begin
  vInt := FCustomClaims.IndexOfName(AKey);
  if vInt >= 0 then
    FCustomClaims.Delete(vInt);
end;

destructor TRALJWTParams.Destroy;
begin
  FreeAndNil(FCustomClaims);
  inherited;
end;

function TRALJWTParams.GetAsJSON: StringRAL;
var
  vJson: TRALJSONObject;
  vInt: IntegerRAL;
begin
  vJson := TRALJSONObject.Create;
  try
    if FAudience <> '' then
      vJson.Add('aud', FAudience);

    { exp/iat/nbf are Unix time, which is UTC by definition; the fields hold
      local time (they come from Now). DateTimeToUnix treats its input as
      UTC, so without the conversion a token issued at UTC-3 said it would
      expire three hours earlier than meant, and one from another library
      was refused or accepted with the zone offset as the error }
    if FExpiration > 0 then
      vJson.Add('exp', DateTimeToUnix(RALDateTimeToGMT(FExpiration)));

    if FIssuedAt > 0 then
      vJson.Add('iat', DateTimeToUnix(RALDateTimeToGMT(FIssuedAt)));

    if FIssuer <> '' then
      vJson.Add('iss', FIssuer);

    if FId <> '' then
      vJson.Add('jti', FId);

    if FNotBefore > 0 then
      vJson.Add('nbf', DateTimeToUnix(RALDateTimeToGMT(FNotBefore)));

    if FSubject <> '' then
      vJson.Add('sub', FSubject);

    DelClaim('aud');
    DelClaim('exp');
    DelClaim('iat');
    DelClaim('iss');
    DelClaim('jti');
    DelClaim('nbf');
    DelClaim('sub');

    vInt := 0;
    while vInt < FCustomClaims.Count do
    begin
      vJson.Add(FCustomClaims.Names[vInt], FCustomClaims.ValueFromIndex[vInt]);
      vInt := vInt + 1;
    end;

    Result := vJson.ToJson;
  finally
    FreeAndNil(vJson);
  end;
end;

function TRALJWTParams.GetClaim(const AKey: StringRAL): StringRAL;
begin
  Result := FCustomClaims.Values[AKey];
end;

function TRALJWTParams.HasAudience(const AAudience: StringRAL): boolean;
var
  vJson: TRALJSONValue;
  vInt: IntegerRAL;
begin
  Result := FAudience = AAudience;
  if Result or (Copy(Trim(FAudience), 1, 1) <> '[') then
    Exit;
  vJson := nil;
  try
    try
      vJson := TRALJSON.ParseJSON(FAudience);
    except
      Exit;
    end;
    if vJson is TRALJSONArray then
      for vInt := 0 to Pred(TRALJSONArray(vJson).Count) do
        if TRALJSONArray(vJson).Get(vInt).AsString = AAudience then
          Exit(True);
  finally
    vJson.Free;
  end;
end;

procedure TRALJWTParams.Clear;
begin
  FAudience := '';
  FExpiration := 0;
  FIssuedAt := 0;
  FIssuer := '';
  FId := '';
  FNotBefore := 0;
  FSubject := '';
  FCustomClaims.Clear;
end;

procedure TRALJWTParams.SetAsJSON(const AValue: StringRAL);
var
  vJson: TRALJSONObject;
  vInt: IntegerRAL;
  vName: StringRAL;
  vValue: TRALJSONValue;
begin
  Clear;
  vJson := TRALJSONObject(TRALJSON.ParseJSON(AValue));
  try
    if vJson <> nil then
    begin
      vInt := 0;
      while vInt < vJson.Count do
      begin
        vName := vJson.GetName(vInt);
        vValue := vJson.Get(vInt);
        if RALSameName(vName, 'aud') then
        begin
          { aud may be an array of audiences: kept as its JSON text, which
            HasAudience reads }
          if vValue.JsonType = rjtArray then
            FAudience := vValue.ToJSON
          else
            FAudience := vValue.AsString;
        end
        else if RALSameName(vName, 'exp') then
        begin
          if vValue.JsonType = rjtNumber then
            FExpiration := RALGMTToDateTime(UnixToDateTime(vValue.AsInteger))
          else
            FExpiration := StrToDateTimeDef(vValue.AsString, 0);
        end
        else if RALSameName(vName, 'iat') then
        begin
          if vValue.JsonType = rjtNumber then
            FIssuedAt := RALGMTToDateTime(UnixToDateTime(vValue.AsInteger))
          else
            FIssuedAt := StrToDateTimeDef(vValue.AsString, 0);
        end
        else if RALSameName(vName, 'iss') then
        begin
          FIssuer := vValue.AsString;
        end
        else if RALSameName(vName, 'jti') then
        begin
          FId := vValue.AsString;
        end
        else if RALSameName(vName, 'nbf') then
        begin
          if vValue.JsonType = rjtNumber then
            FNotBefore := RALGMTToDateTime(UnixToDateTime(vValue.AsInteger))
          else
            FNotBefore := StrToDateTimeDef(vValue.AsString, 0);
        end
        else if RALSameName(vName, 'sub') then
        begin
          FSubject := vValue.AsString;
        end
        else if vValue.JsonType in [rjtObject, rjtArray] then
        begin
          AddClaim(vName, vValue.ToJSON);
        end
        else
        begin
          AddClaim(vName, vValue.AsString);
        end;

        vInt := vInt + 1;
      end;
    end;
  finally
    FreeAndNil(vJson);
  end;
end;

{ TRALJWT }

constructor TRALJWT.Create;
begin
  inherited;
  FHeader := TRALJWTHeader.Create;
  FPayload := TRALJWTParams.Create;
  FToken := '';
  FSigningInput := '';
  FSignKey := nil;
  FLeeway := 0;
end;

destructor TRALJWT.Destroy;
begin
  FreeAndNil(FHeader);
  FreeAndNil(FPayload);
  inherited;
end;

class function TRALJWT.AlgorithmName(AAlgorithm: TRALJWTAlgorithm): StringRAL;
begin
  case AAlgorithm of
    tjaHSHA384: Result := 'HS384';
    tjaHSHA512: Result := 'HS512';
    tjaRS256: Result := 'RS256';
    tjaRS384: Result := 'RS384';
    tjaRS512: Result := 'RS512';
    tjaES256: Result := 'ES256';
    tjaES384: Result := 'ES384';
  else
    Result := 'HS256';
  end;
end;

procedure TRALJWT.SetToken(AValue: StringRAL);
var
  vInt: IntegerRAL;
  vStr: TStringList;
  vWhole: StringRAL;
begin
  FToken := '';
  FSigningInput := '';
  vWhole := AValue;
  vStr := TStringList.Create;
  try
    repeat
      vInt := Pos('.', AValue);
      if (vInt = 0) and (AValue <> '') then
        vInt := Length(AValue) + 1;

      if vInt > 0 then
      begin
        vStr.Add(Copy(AValue, 1, vInt - 1));
        Delete(AValue, 1, vInt);
      end;
    until vInt = 0;

    if vStr.Count = 3 then
    begin
      { the loop above eats AValue segment by segment, and FToken used to be
        assigned AFTER it - always empty, so IsValidToken with no argument
        answered False for a token that had just been assigned }
      FToken := vWhole;
      { what was signed is these bytes as they came, not the claims parsed and
        written back: a token from another issuer orders and formats its JSON
        its own way, and re-serialising it never matched the signature }
      FSigningInput := vStr.Strings[0] + '.' + vStr.Strings[1];
      FHeader.AsJSON := TRALBase64.Decode(TRALBase64.FromBase64Url(vStr.Strings[0]));
      FPayload.AsJSON := TRALBase64.Decode(TRALBase64.FromBase64Url(vStr.Strings[1]));
      FSignature := vStr.Strings[2];
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

function TRALJWT.SignInput(const AInput: StringRAL): StringRAL;
var
  vHash: TRALHashBase;
begin
  case FHeader.Algorithm of
    tjaHSHA256, tjaHSHA384, tjaHSHA512:
    begin
      if FHeader.Algorithm = tjaHSHA256 then
      begin
        vHash := TRALSHA2_32.Create;
        TRALSHA2_32(vHash).Version := rsv256;
      end
      else
      begin
        vHash := TRALSHA2_64.Create;
        if FHeader.Algorithm = tjaHSHA384 then
          TRALSHA2_64(vHash).Version := rsv384
        else
          TRALSHA2_64(vHash).Version := rsv512;
      end;
      try
        vHash.OutputType := rhotBase64Url;
        Result := vHash.HMACAsString(AInput, FSignSecretKey);
      finally
        FreeAndNil(vHash);
      end;
    end;
    tjaRS256, tjaES256:
    begin
      if FSignKey = nil then
        raise Exception.Create(emJWSNoPrivateKey);
      Result := RALBase64UrlEncode(FSignKey.Sign(AInput, jdSHA256));
    end;
    tjaRS384, tjaES384:
    begin
      if FSignKey = nil then
        raise Exception.Create(emJWSNoPrivateKey);
      Result := RALBase64UrlEncode(FSignKey.Sign(AInput, jdSHA384));
    end;
  else
    begin
      if FSignKey = nil then
        raise Exception.Create(emJWSNoPrivateKey);
      Result := RALBase64UrlEncode(FSignKey.Sign(AInput, jdSHA512));
    end;
  end;
end;

function TRALJWT.CreateToken(AHeader, APayload: StringRAL;
  var ASignature: StringRAL): StringRAL;
var
  vStr: StringRAL;
begin
  // json codifica / para \/
  AHeader := StringReplace(AHeader, '\/', '/', [rfReplaceAll]);
  APayload := StringReplace(APayload, '\/', '/', [rfReplaceAll]);

  vStr := TRALBase64.Encode(AHeader);
  vStr := TRALBase64.ToBase64Url(vStr);

  Result := vStr + '.';

  vStr := TRALBase64.Encode(APayload);
  vStr := TRALBase64.ToBase64Url(vStr);

  Result := Result + vStr;

  ASignature := SignInput(Result);

  Result := Result + '.' + ASignature;
end;

function TRALJWT.GetToken: StringRAL;
begin
  Result := CreateToken(FHeader.AsJSON, FPayload.AsJSON, FSignature);
end;

function TRALJWT.IsValidSignature: boolean;
var
  vDigest: TRALJWSDigest;
begin
  Result := False;
  if (FSigningInput = '') or (FSignature = '') or (not FHeader.AlgorithmKnown) then
    Exit;
  case FHeader.Algorithm of
    tjaHSHA256, tjaHSHA384, tjaHSHA512:
      { constant time: the signature is the secret here }
      Result := RALSameSecret(SignInput(FSigningInput), FSignature);
  else
    begin
      if FSignKey = nil then
        Exit;
      case FHeader.Algorithm of
        tjaRS384, tjaES384: vDigest := jdSHA384;
        tjaRS512: vDigest := jdSHA512;
      else
        vDigest := jdSHA256;
      end;
      { RS* with an EC key or ES* with an RSA key is refused: a key belongs to
        one algorithm family }
      if (FHeader.Algorithm in [tjaRS256, tjaRS384, tjaRS512]) <>
         (FSignKey.KeyType = jktRSA) then
        Exit;
      Result := FSignKey.Verify(FSigningInput, RALBase64UrlDecode(FSignature), vDigest);
    end;
  end;
end;

function TRALJWT.IsValidToken(const AValue: StringRAL): boolean;
var
  vAlgorithm: TRALJWTAlgorithm;
  vNow: TDateTime;
begin
  Result := False;
  if (Trim(AValue) = '') and (Trim(FToken) = '') then
    Exit;

  { the algorithm is the one this instance was set to, never the one the token
    names: a token saying HS256 to a server expecting RS256 would otherwise be
    checked with the public key as an HMAC secret }
  vAlgorithm := FHeader.Algorithm;

  if AValue <> '' then
    Token := AValue;

  if (vAlgorithm <> FHeader.Algorithm) or (not IsValidSignature) then
    Exit;

  vNow := Now;
  if (FPayload.Expiration > 0) and
     (IncSecond(FPayload.Expiration, FLeeway) < vNow) then
    Exit;
  if (FPayload.NotBefore > 0) and (IncSecond(FPayload.NotBefore, -FLeeway) > vNow) then
    Exit;
  if (FPayload.NotBefore > 0) and (FPayload.Expiration > 0) and
     (FPayload.Expiration < FPayload.NotBefore) then
    Exit;
  Result := True;
end;

{ TRALAuthBasic }

procedure TRALAuthBasic.SetAuthString(const AValue: StringRAL);
var
  vString: StringRAL;
  vInt: IntegerRAL;
begin
  FAuthString := AValue;
  vString := TRALBase64.Decode(FAuthString);
  vInt := Pos(':', vString);
  if vInt > 0 then begin
    FUserName := Copy(vString, 1, vInt - 1);
    FPassword := Copy(vString, vInt + 1, Length(vString));
  end;
end;

end.
