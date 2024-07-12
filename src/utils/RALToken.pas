/// Class for the Token structure definition and methods, used on JWT and OAuth authentications
unit RALToken;

interface

uses
  Classes, SysUtils, DateUtils,
  RALTypes, RALSHA2_32, RALSHA2_64, RALHashes, RALBase64, RALMD5,
  RALJson, RALTools, RALUrlCoder;

type
  TRALDigestAlgorithm = (tdaMD5, tdaSHA2_256, tdaSHA2_512);
  TRALJWTAlgorithm = (tjaHSHA256, tjaHSHA384, tjaHSHA512);
  TRALOAuthAlgorithm = (toaHSHA256, toaHSHA512, toaPLAINTEXT);

  { TRALAuthBasic }

  TRALAuthBasic = class
  private
    FAuthString: StringRAL;
    FPassword : StringRAL;
    FUserName : StringRAL;
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
    FHeaderType: StringRAL;
    FKeyID: StringRAL;
  protected
    function GetAsJSON: StringRAL;
    procedure Initialize;
    procedure SetAsJSON(const AValue: StringRAL);
  public
    constructor Create;
    procedure createKeyID;

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
    procedure AddClaim(const AKey: StringRAL; const AValue: StringRAL);
    procedure Clear;
    procedure createNewId;
    procedure DelClaim(const AKey: StringRAL);
    function GetClaim(const AKey: StringRAL): StringRAL;

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

  /// Class for the JWT structure definition
  TRALJWT = class
  private
    FHeader: TRALJWTHeader;
    FPayload: TRALJWTParams;
    FSignature: StringRAL;
    FSignSecretKey: StringRAL;
    FToken : StringRAL;
  protected
    function signHS256(const ASource: StringRAL): StringRAL;
    function signHS384(const ASource: StringRAL): StringRAL;
    function signHS512(const ASource: StringRAL): StringRAL;

    function CreateToken(AHeader, APayload: StringRAL;
      var ASignature: StringRAL): StringRAL;
    function GetToken: StringRAL;
    procedure SetToken(AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;

    function IsValidToken(const AValue: StringRAL = ''): boolean;
  published
    property Header: TRALJWTHeader read FHeader write FHeader;
    property Payload: TRALJWTParams read FPayload write FPayload;
    property Signature: StringRAL read FSignature;
    property SignSecretKey: StringRAL read FSignSecretKey write FSignSecretKey;
    property Token: StringRAL read GetToken write SetToken;
  end;

  { TRALOAuth }

  /// Class for the OAuth Token definition
  TRALOAuth = class
  private
    FAlgorithm: TRALOAuthAlgorithm;
    FCallBack: StringRAL;
    FConsumerKey: StringRAL;
    FConsumerSecret: StringRAL;
    FMethod: StringRAL;
    FNonce: StringRAL;
    FSignature: StringRAL;
    FTimestamp: Int64RAL;
    FTokenAccess: StringRAL;
    FTokenSecret: StringRAL;
    FURL: StringRAL;
    FVerifier: StringRAL;
    FVersion: StringRAL;
  protected
    function AlgorithmToStr(AAlg: TRALOAuthAlgorithm): StringRAL;
    function GetHeader: TStringList;
    function GetSignature: StringRAL;
    function StrToAlgorithm(const AStr: StringRAL): TRALOAuthAlgorithm;
  public
    constructor Create;
    function Load(const AValue: StringRAL): boolean;
    function Validate: boolean;

    property Header: TStringList read GetHeader;
    property Signature: StringRAL read GetSignature;
  published
    property Algorithm: TRALOAuthAlgorithm read FAlgorithm write FAlgorithm;
    property CallBack: StringRAL read FCallBack write FCallBack;
    property ConsumerKey: StringRAL read FConsumerKey write FConsumerKey;
    property ConsumerSecret: StringRAL read FConsumerSecret write FConsumerSecret;
    property Method: StringRAL read FMethod write FMethod;
    property Nonce: StringRAL read FNonce write FNonce;
    property Timestamp: Int64RAL read FTimestamp write FTimestamp;
    property TokenAccess: StringRAL read FTokenAccess write FTokenAccess;
    property TokenSecret: StringRAL read FTokenSecret write FTokenSecret;
    property URL: StringRAL read FURL write FURL;
    property Verifier: StringRAL read FVerifier write FVerifier;
    property Version: StringRAL read FVersion write FVersion;
  end;

  /// Class for Digest auth parameters definitions
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
    FStale: StringRAL;
    FUserHash: boolean;
  published
    property Algorithm: TRALDigestAlgorithm read FAlgorithm write FAlgorithm;
    property Charset: StringRAL read FCharset write FCharset;
    property CNonce: StringRAL read FCNonce write FCNonce;
    property Domain: StringRAL read FDomain write FDomain;
    property NC: integer read FNC write FNC;
    property Nonce: StringRAL read FNonce write FNonce;
    property Opaque: StringRAL read FOpaque write FOpaque;
    property Qop: StringRAL read FQop write FQop;
    property Realm: StringRAL read FRealm write FRealm;
    property SessAlgorithm: boolean read FSessAlgorithm write FSessAlgorithm;
    property Stale: StringRAL read FStale write FStale;
    property UserHash: boolean read FUserHash write FUserHash;
  end;

  { TRALDigest }

  /// Class for Digest token definitions
  TRALDigest = class
  private
    FEntityBody: StringRAL;
    FMethod: StringRAL;
    FParams: TRALDigestParams;
    FPassword: StringRAL;
    FURL: StringRAL;
    FUserName: StringRAL;
  protected
    function GetHeader: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(const AValue: StringRAL);
    property Header: TStringList read GetHeader;
  published
    property EntityBody: StringRAL read FEntityBody write FEntityBody;
    property Method: StringRAL read FMethod write FMethod;
    property Params: TRALDigestParams read FParams write FParams;
    property Password: StringRAL read FPassword write FPassword;
    property URL: StringRAL read FURL write FURL;
    property UserName: StringRAL read FUserName write FUserName;
  end;

implementation

{ TRALDigest }

function TRALDigest.GetHeader: TStringList;
var
  vHa1, vHa2, vAux1, vNC: StringRAL;
  vHash: TRALHashes;
begin
  case FParams.Algorithm of
    tdaMD5: begin
      vHash := TRALMD5.Create;
    end;
    tdaSHA2_256: begin
      vHash := TRALSHA2_32.Create;
      TRALSHA2_32(vHash).Version := rsv256;
    end;
    tdaSHA2_512: begin
      vHash := TRALSHA2_64.Create;
      TRALSHA2_64(vHash).Version := rsv512_256;
    end;
  end;

  try
    vNC := Format('%.8d', [FParams.NC]);
    FParams.CNonce := vHash.HashAsString(vNC);

    vHa1 := Format('%s:%s:%s', [FUserName, FParams.Realm, FPassword]);
    vHa1 := vHash.HashAsString(vHa1);

    if FParams.SessAlgorithm then
      vHa1 := vHash.HashAsString(Format('%s:%s:%s',
        [vHa1, FParams.Nonce, FParams.CNonce]));

    if ((Pos('auth', LowerCase(FParams.Qop)) > 0) and
      (Pos('auth-int', LowerCase(FParams.Qop)) = 0)) or
      (Trim(FParams.Qop) = '') then
    begin
      vHa2 := Format('%s:%s', [FMethod, FURL]);
      vHa2 := vHash.HashAsString(vHa2);
    end
    else if (Pos('auth-int', LowerCase(FParams.Qop)) > 0) then
    begin
      vHa2 := vHash.HashAsString(FEntityBody);
      vHa2 := Format('%s:%s:%s', [FMethod, FURL, vHa2]);
      vHa2 := vHash.HashAsString(vHa2);
    end;

    if (Pos('auth', LowerCase(FParams.Qop)) > 0) then
      vAux1 := Format('%s:%s:%s:%s:%s:%s', [vHa1, FParams.Nonce, vNC,
        FParams.CNonce, FParams.Qop, vHa2])
    else
      vAux1 := Format('%s:%s:%s', [vHa1, FParams.Nonce, vHa2]);
    vAux1 := vHash.HashAsString(vAux1);
  finally
    FreeAndNil(vHash);
  end;

  Result := TStringList.Create;
  Result.Add('realm=' + FParams.Realm);
  Result.Add('username=' + FUserName);
  Result.Add('nonce=' + FParams.Nonce);
  Result.Add('uri=' + FURL);
  Result.Add('qop=' + FParams.Qop);
  Result.Add('nc=' + vNC);  //  nc=00000001,
  Result.Add('cnonce=' + FParams.CNonce); // cnonce="0a4f113b",
  Result.Add('response=' + vAux1);
  Result.Add('opaque=' + FParams.Opaque);
end;

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

procedure TRALDigest.Load(const AValue: StringRAL);
var
  vParams: TStringList;
  vParam, vAuth, vAux1: StringRAL;
  vIni, vLen: IntegerRAL;
  vQuoted: boolean;
begin
  vParams := TStringList.Create;
  try
    vParams.Sorted := True;
    vParam := '';
    vAuth := AValue + ',';
    vIni := POSINISTR;
    vLen := RALHighStr(vAuth);
    vQuoted := False;
    while vIni <= vLen do
    begin
      if ((vAuth[vIni] = ' ') or (vAuth[vIni] = ',')) and (not vQuoted) then
      begin
        vParam := Trim(vParam);
        if vParam <> '' then
          vParams.Add(vParam);
        vParam := '';
      end
      else if vAuth[vIni] = '"' then
      begin
        vQuoted := not vQuoted;
      end
      else
      begin
        vParam := vParam + vAuth[vIni];
      end;
      vIni := vIni + 1;
    end;

    FParams.Algorithm := tdaMD5;

    FParams.Realm := vParams.Values['realm'];
    vAux1 := LowerCase(vParams.Values['algorithm']);

    FParams.SessAlgorithm := Pos('sess', vAux1) > 0;
    if Pos('sha-256', vAux1) > 0 then
      FParams.Algorithm := tdaSHA2_256
    else if Pos('sha-512-256', vAux1) > 0 then
      FParams.Algorithm := tdaSHA2_512;

    FParams.Qop := vParams.Values['qop'];
    FParams.Nonce := vParams.Values['nonce'];
    FParams.Opaque := vParams.Values['opaque'];
    FParams.Stale := vParams.Values['stale'];
    FParams.Charset := vParams.Values['charset'];
    FParams.UserHash := SameText(vParams.Values['userhash'], 'true');
  finally
    FreeAndNil(vParams);
  end;
end;

{ TRALOAuth }

function TRALOAuth.GetSignature: StringRAL;
begin

end;

function TRALOAuth.AlgorithmToStr(AAlg: TRALOAuthAlgorithm): StringRAL;
begin
  case AAlg of
    toaHSHA256: Result := 'HMAC-SHA256';
    toaHSHA512: Result := 'HMAC-SHA512';
    toaPLAINTEXT: Result := 'PLAINTEXT';
  end;
end;

function TRALOAuth.StrToAlgorithm(const AStr: StringRAL): TRALOAuthAlgorithm;
begin
  if AStr = 'HMAC-SHA256' then
    Result := toaHSHA256
  else if AStr = 'HMAC-SHA512' then
    Result := toaHSHA512
  else
    Result := toaPLAINTEXT;
end;

function TRALOAuth.GetHeader: TStringList;
var
  vNonce, vAlgorithm: StringRAL;
  vURL, vSign, vSecret: StringRAL;
  vInt: IntegerRAL;
  vHash: TRALHashes;
begin
  inherited;
  if Trim(FNonce) = '' then
    vNonce := TRALBase64.Encode(RandomBytes(10))
  else
    vNonce := FNonce;

  FTimestamp := DateTimeToUnix(Now);

  Result := TStringList.Create;
  Result.Sorted := True;

  Result.Add('oauth_callback=' + FCallBack);
  Result.Add('oauth_consumer_key=' + FConsumerKey);
  Result.Add('oauth_nonce=' + vNonce);
  case FAlgorithm of
    toaHSHA256: vAlgorithm := 'HMAC-SHA256';
    toaHSHA512: vAlgorithm := 'HMAC-SHA512';
    toaPLAINTEXT: vAlgorithm := 'PLAINTEXT';
  end;
  Result.Add('oauth_signature_method=' + vAlgorithm);
  Result.Add('oauth_timestamp=' + IntToStr(FTimestamp));
  Result.Add('oauth_token=' + FTokenAccess);
  Result.Add('oauth_verifier=' + FVerifier);
  Result.Add('oauth_version=' + FVersion);

  vSecret := FConsumerSecret + '&' + FTokenSecret;

  if FAlgorithm <> toaPLAINTEXT then
  begin
    vSign := '';
    for vInt := 0 to Pred(Result.Count) do
    begin
      if vSign <> '' then
        vSign := vSign + '&';
      vSign := vSign + Result.Strings[vInt];
    end;
    vSign := TRALHTTPCoder.EncodeURL(vSign);
    vURL := TRALHTTPCoder.EncodeURL(FURL);

    vSign := Format('%s&%s&%s', [FMethod, vURL, vSign]);

    case FAlgorithm of
      toaHSHA256: begin
        vHash := TRALSHA2_32.Create;
        TRALSHA2_32(vHash).Version := rsv256;
      end;
      toaHSHA512: begin
        vHash := TRALSHA2_64.Create;
        TRALSHA2_64(vHash).Version := rsv512;
      end;
    end;

    try
      vHash.OutputType := rhotBase64;
      vSign := vHash.HMACAsString(vSign, vSecret);
    finally
      FreeAndNil(vHash);
    end;

    Result.Add('oauth_signature=' + vSign);
  end
  else
  begin
    Result.Add('oauth_signature=' + vSecret);
  end;
end;

constructor TRALOAuth.Create;
begin
  inherited Create;
  FAlgorithm := toaHSHA256;
end;

function TRALOAuth.Validate: boolean;
var
  vParams: TStringList;
begin
  vParams := GetHeader;
  try
    Result := vParams.Values['oauth_signature'] = FSignature;
  finally
    FreeAndNil(vParams);
  end;
end;

function TRALOAuth.Load(const AValue: StringRAL): boolean;
var
  vAuth, vParam: StringRAL;
  vIni, vLen: IntegerRAL;
  vParams: TStringList;
begin
  Result := False;

  vParams := TStringList.Create;
  try
    vParams.Sorted := True;
    vParam := '';
    vAuth := AValue + ',';
    vIni := POSINISTR;
    vLen := RALHighStr(vAuth);
    while vIni <= vLen do
    begin
      if (vAuth[vIni] = ' ') or (vAuth[vIni] = ',') then
      begin
        vParam := Trim(vParam);
        if vParam <> '' then
          vParams.Add(vParam);
        vParam := '';
      end
      else if vAuth[vIni] <> '"' then
      begin
        vParam := vParam + vAuth[vIni];
      end;
      vIni := vIni + 1;
    end;

    Result := (vParams.Values['oauth_consumer_key'] = FConsumerKey) and
      (AlgorithmToStr(FAlgorithm) = vParams.Values['oauth_signature_method']);

    if Result then
    begin
      FCallBack := TRALHTTPCoder.DecodeURL(vParams.Values['oauth_callback']);
      FNonce := TRALHTTPCoder.DecodeURL(vParams.Values['oauth_nonce']);
      FTimestamp := StrToInt64(vParams.Values['oauth_timestamp']);
      FTokenAccess := TRALHTTPCoder.DecodeURL(vParams.Values['oauth_token']);
      FVerifier := TRALHTTPCoder.DecodeURL(vParams.Values['oauth_verifier']);
      FVersion := TRALHTTPCoder.DecodeURL(vParams.Values['oauth_version']);
      FSignature := TRALHTTPCoder.DecodeURL(vParams.Values['oauth_signature']);
    end;
  finally
    vParams.Free;
  end;
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

    case FAlgorithm of
      tjaHSHA256: vJson.Add('alg', 'HS256');
      tjaHSHA512: vJson.Add('alg', 'HS512');
    end;

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
  FKeyID := '';
end;

procedure TRALJWTHeader.SetAsJSON(const AValue: StringRAL);
var
  vJson: TRALJSONObject;
  vInt: IntegerRAL;
  vName: StringRAL;
  vValue: TRALJSONValue;
  vAux1: StringRAL;
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
        if SameText(vName, 'typ') then
        begin
          FHeaderType := vValue.AsValue;
        end
        else if SameText(vName, 'alg') then
        begin
          vAux1 := vValue.AsValue;

          FAlgorithm := tjaHSHA256;
          if SameText(vAux1, 'hs256') then
            FAlgorithm := tjaHSHA256
          else if SameText(vAux1, 'hs512') then
            FAlgorithm := tjaHSHA512;
        end
        else if SameText(vName, 'kid') then
        begin
          FKeyID := vValue.AsValue;
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

    if FExpiration > 0 then
      vJson.Add('exp', DateTimeToUnix(FExpiration));

    if FIssuedAt > 0 then
      vJson.Add('iat', DateTimeToUnix(FIssuedAt));

    if FIssuer <> '' then
      vJson.Add('iss', FIssuer);

    if FId <> '' then
      vJson.Add('jti', FId);

    if FNotBefore > 0 then
      vJson.Add('nbf', DateTimeToUnix(FNotBefore));

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
        if SameText(vName, 'aud') then
        begin
          FAudience := vValue.AsValue;
        end
        else if SameText(vName, 'exp') then
        begin
          if vValue.JsonType = rjtNumber then
            FExpiration := UnixToDateTime(vValue.AsInteger)
          else
            FExpiration := StrToDateTimeDef(vValue.AsString, 0);
        end
        else if SameText(vName, 'iat') then
        begin
          if vValue.JsonType = rjtNumber then
            FIssuedAt := UnixToDateTime(vValue.AsInteger)
          else
            FIssuedAt := StrToDateTimeDef(vValue.AsString, 0);
        end
        else if SameText(vName, 'iss') then
        begin
          FIssuer := vValue.AsString;
        end
        else if SameText(vName, 'jti') then
        begin
          FId := vValue.AsString;
        end
        else if SameText(vName, 'nbf') then
        begin
          if vValue.JsonType = rjtNumber then
            FNotBefore := UnixToDateTime(vValue.AsInteger)
          else
            FNotBefore := StrToDateTimeDef(vValue.AsString, 0);
        end
        else if SameText(vName, 'sub') then
        begin
          FSubject := vValue.AsString;
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
end;

procedure TRALJWT.SetToken(AValue: StringRAL);
var
  vInt: IntegerRAL;
  vStr: TStringList;
begin
  FToken := '';
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
      FToken := AValue;
      FHeader.AsJSON := TRALBase64.Decode(vStr.Strings[0]);
      FPayload.AsJSON := TRALBase64.Decode(vStr.Strings[1]);
      FSignature := vStr.Strings[2];
    end;
  finally
    FreeAndNil(vStr);
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

  case FHeader.Algorithm of
    tjaHSHA256: ASignature := signHS256(Result);
    tjaHSHA384: ASignature := signHS384(Result);
    tjaHSHA512: ASignature := signHS512(Result);
  end;

  Result := Result + '.' + ASignature;
end;

destructor TRALJWT.Destroy;
begin
  FreeAndNil(FHeader);
  FreeAndNil(FPayload);
  inherited;
end;

function TRALJWT.GetToken: StringRAL;
begin
  Result := CreateToken(FHeader.AsJSON, FPayload.AsJSON, FSignature);
end;

function TRALJWT.signHS256(const ASource: StringRAL): StringRAL;
var
  vHash: TRALSHA2_32;
begin
  vHash := TRALSHA2_32.Create;
  try
    vHash.Version := rsv256;
    vHash.OutputType := rhotBase64Url;
    Result := vHash.HMACAsString(ASource, FSignSecretKey);
  finally
    FreeAndNil(vHash);
  end;
end;

function TRALJWT.signHS384(const ASource: StringRAL): StringRAL;
var
  vHash: TRALSHA2_64;
begin
  vHash := TRALSHA2_64.Create;
  try
    vHash.Version := rsv384;
    vHash.OutputType := rhotBase64Url;
    Result := vHash.HMACAsString(ASource, FSignSecretKey);
  finally
    FreeAndNil(vHash);
  end;
end;

function TRALJWT.signHS512(const ASource: StringRAL): StringRAL;
var
  vHash: TRALSHA2_64;
begin
  vHash := TRALSHA2_64.Create;
  try
    vHash.Version := rsv512;
    vHash.OutputType := rhotBase64Url;
    Result := vHash.HMACAsString(ASource, FSignSecretKey);
  finally
    FreeAndNil(vHash);
  end;
end;

function TRALJWT.IsValidToken(const AValue: StringRAL): boolean;
var
  vSignature: StringRAL;
  vAlgorithm: TRALJWTAlgorithm;
begin
  Result := False;
  if (Trim(AValue) = '') and (Trim(FToken) = '') then
    Exit;

  vAlgorithm := FHeader.Algorithm;

  if AValue <> '' then
    Token := AValue;

  if vAlgorithm = FHeader.Algorithm then
  begin
    vSignature := FSignature;
    GetToken;
    if vSignature = FSignature then
    begin
      Result := True;
      if (FPayload.Expiration > 0) and (FPayload.Expiration < Now) then
        Result := False
      else if (FPayload.NotBefore > 0) and (FPayload.NotBefore > Now) then
        Result := False
      else if (FPayload.NotBefore > 0) and (FPayload.Expiration > 0) and
        (FPayload.Expiration < FPayload.NotBefore) then
        Result := False;
    end;
  end;
end;

{ TRALAuthBasic }

procedure TRALAuthBasic.SetAuthString(const AValue: StringRAL);
var
  vString : StringRAL;
  vInt : IntegerRAL;
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
