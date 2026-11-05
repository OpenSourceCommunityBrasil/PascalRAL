unit LetsEncrypt;

{$IFDEF FPC}
{$mode delphi}
{$H+}
{$ENDIF}
{$IFDEF CPUX86_64}
{$DEFINE CPUX64}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  AcmeCommon,
  RALOpenSSL,
  AcmeHttp, RALTypes;

type
  TAcmeEnvironment = (aeStaging, aeProduction);
  TAcmeLogEvent = procedure(
    Sender: TObject;
    const Text: string
  ) of object;
  TLetsEncrypt = class(TComponent)
  private
    FEmail: string;
    FStoragePath: string;
    FWebRoot: string;
    FCryptoLibrary: string;
    FCurlLibrary: string;
    FCABundle: string;
    FDomains: TStringList;
    FEnvironment: TAcmeEnvironment;
    FAcceptTerms: Boolean;
    FBusy: Boolean;
    FTimeoutSeconds: Integer;
    FOnLog: TAcmeLogEvent;
    FLastOutput: string;
    FCertificateFile: string;
    FPrivateKeyFile: string;
    Crypto: TRALOpenSSL;
    Http: TAcmeHttp;
    AccountKey: RawByteString;
    AccountURL: string;
    NewNonceURL: string;
    Nonce: string;
    Started: UInt64;
    procedure SetDomains(Value: TStrings);
    function GetDomains: TStrings;
    procedure Validate;
    procedure Log(const Text: string);
    function RemainingMs: Integer;
    function SignedRequest(
      const URL: string;
      const Payload: string;
      WithJWK: Boolean = False
    ): TAcmeResponse;
    function Poll(
      const URL: string;
      const TargetStatus: string
    ): TAcmeResponse;
    procedure WaitRetry(const Value: string);
    procedure Authorize(const URL: string);
    procedure Issue(
      RenewOnly: Boolean;
      Days: Integer
    );
  protected
    { Override only for tests/custom transports; production transport verifies TLS. }
    function Request(
      const Method: string;
      const URL: string;
      const Body: string
    ): TAcmeResponse; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ObtainCertificate;
    procedure RenewCertificate(DaysBeforeExpiry: Integer = 30);
    property LastOutput: string read FLastOutput;
    property CertificateFile: string read FCertificateFile;
    property PrivateKeyFile: string read FPrivateKeyFile;
  published
    property Email: string read FEmail write FEmail;
    property Domains: TStrings read GetDomains write SetDomains;
    property StoragePath: string read FStoragePath write FStoragePath;
    property WebRoot: string read FWebRoot write FWebRoot;
    property Environment: TAcmeEnvironment read FEnvironment write FEnvironment default aeStaging;
    property AcceptTerms: Boolean read FAcceptTerms write FAcceptTerms default False;
    property TimeoutSeconds: Integer read FTimeoutSeconds write FTimeoutSeconds default 300;
    property CryptoLibrary: string read FCryptoLibrary write FCryptoLibrary;
    property CurlLibrary: string read FCurlLibrary write FCurlLibrary;
    property CABundle: string read FCABundle write FCABundle;
    property OnLog: TAcmeLogEvent read FOnLog write FOnLog;
  end;

implementation

uses
  DateUtils;

const
  StagingURL = 'https://acme-staging-v02.api.letsencrypt.org/directory';
  ProductionURL = 'https://acme-v02.api.letsencrypt.org/directory';

constructor TLetsEncrypt.Create(AOwner: TComponent);
begin
  inherited;
  FDomains := TStringList.Create;
  FEnvironment := aeStaging;
  FTimeoutSeconds := 300;
  {$IFDEF MSWINDOWS}
    FCurlLibrary := 'libcurl.dll';
  {$ELSE}
    FCurlLibrary := 'libcurl.so.4';
  {$ENDIF}
end;

destructor TLetsEncrypt.Destroy;
begin
  FDomains.Free;
  inherited;
end;

function TLetsEncrypt.GetDomains: TStrings;
begin
  Result := FDomains;
end;

procedure TLetsEncrypt.SetDomains(Value: TStrings);
begin
  FDomains.Assign(Value);
end;

procedure TLetsEncrypt.Validate;
var
  I: Integer;
begin
  if not FAcceptTerms then
    raise ERALException.Create('AcceptTerms must explicitly accept the CA terms of service.');
  if (Pos('@', FEmail) < 2) or (Trim(FEmail) <> FEmail) then
    raise ERALException.Create('Set a valid account Email.');
  if (FTimeoutSeconds < 1) or (FTimeoutSeconds > 2147483) then
    raise ERALException.Create('Invalid TimeoutSeconds.');
  if Trim(FStoragePath) = '' then
    raise ERALException.Create('Set StoragePath to a persistent private directory.');
  if FDomains.Count = 0 then
    raise ERALException.Create('Add at least one domain.');
  for I := 0 to FDomains.Count - 1 do
    ValidateDomain(FDomains[I]);
  if (FWebRoot = '') or not DirectoryExists(FWebRoot) then
    raise ERALException.Create('WebRoot must be an existing public HTTP directory.');
  if (FCABundle <> '') and not FileExists(FCABundle) then
    raise ERALException.Create('CABundle does not exist.');
end;

procedure TLetsEncrypt.Log(const Text: string);
begin
  FLastOutput := FLastOutput + Text + sLineBreak;
  if Assigned(FOnLog) then
    FOnLog(Self, Text);
end;

function TLetsEncrypt.RemainingMs: Integer;
var
  Elapsed: UInt64;
  Limit: UInt64;
begin
  Elapsed := TickMilliseconds - Started;
  Limit := UInt64(FTimeoutSeconds) * 1000;
  if Elapsed >= Limit then
    raise ERALException.Create('ACME operation timed out.');
  Result := Integer(Limit - Elapsed);
end;

function TLetsEncrypt.Request(
  const Method: string;
  const URL: string;
  const Body: string
): TAcmeResponse;
begin
  if Http = nil then
    Http := TAcmeHttp.Create(FCurlLibrary);
  Result := Http.Request(Method, URL, Body, FCABundle, RemainingMs);
end;

function TLetsEncrypt.SignedRequest(
  const URL: string;
  const Payload: string;
  WithJWK: Boolean
): TAcmeResponse;
var
  Header: string;
  EncHeader: string;
  EncPayload: string;
  JWS: string;
  Attempt: Integer;
  R: TAcmeResponse;
begin
  for Attempt := 0 to 2 do
  begin
    RemainingMs;
    if Nonce = '' then
    begin
      R := Request('HEAD', NewNonceURL, '');
      if (R.Status <> 200) and (R.Status <> 204) then
        raise ERALException.Create('Cannot obtain ACME nonce.');
      Nonce := R.Nonce;
    end;
    if Nonce = '' then
      raise ERALException.Create('Missing Replay-Nonce.');
    Header := '{"alg":"RS256","nonce":' + JsonQuote(Nonce) + ',"url":' + JsonQuote(URL);
    if WithJWK then
      Header := Header + ',"jwk":' + Crypto.JWK(AccountKey)
    else
      Header := Header + ',"kid":' + JsonQuote(AccountURL);
    Header := Header + '}';
    EncHeader := Base64Url(UTF8Encode(Header));
    EncPayload := Base64Url(UTF8Encode(Payload));
    JWS := '{"protected":' + JsonQuote(EncHeader) + ',"payload":' + JsonQuote(EncPayload) +
      ',"signature":' + JsonQuote(Base64Url(Crypto.Sign(AccountKey, UTF8Encode(EncHeader + '.' + EncPayload)))) + '}';
    Nonce := '';
    Result := Request('POST', URL, JWS);
    Nonce := Result.Nonce;
    if (Result.Status >= 200) and (Result.Status < 300) then
      Exit;
    if (Result.Status = 400) and (JsonField(Result.Body, 'type') = 'urn:ietf:params:acme:error:badNonce') and (Attempt < 2) then
      Continue;
    raise ERALException.CreateFmt('ACME HTTP %d: %s', [Result.Status, Copy(Result.Body, 1, 2048)]);
  end;
end;

procedure TLetsEncrypt.WaitRetry(const Value: string);
var
  Seconds: Int64;
  Delay: Integer;
  Remaining: Integer;
  Y: Integer;
  M: Integer;
  D: Integer;
  H: Integer;
  N: Integer;
  S: Integer;
  DatePart: TDateTime;
  TimePart: TDateTime;
  UTCNow: TDateTime;
  MonthName: string;
const
  Months = 'JanFebMarAprMayJunJulAugSepOctNovDec';
begin
  if not TryStrToInt64(Value, Seconds) then
  begin
    Seconds := 2;
    { IMF-fixdate, e.g. Wed, 21 Oct 2015 07:28:00 GMT. }
    if (Length(Value) = 29) and (Copy(Value, 26, 4) = ' GMT') then
    begin
      MonthName := Copy(Value, 9, 3);
      M := Pos(MonthName, Months);
      if (Length(MonthName) = 3) and (M > 0) and ((M - 1) mod 3 = 0) and
         TryStrToInt(Copy(Value, 6, 2), D) and TryStrToInt(Copy(Value, 13, 4), Y) and
         TryStrToInt(Copy(Value, 18, 2), H) and TryStrToInt(Copy(Value, 21, 2), N) and
         TryStrToInt(Copy(Value, 24, 2), S) then
        if TryEncodeDate(Y, (M - 1) div 3 + 1, D, DatePart) and TryEncodeTime(H, N, S, 0, TimePart) then
        begin
          {$IFDEF FPC}
          UTCNow := LocalTimeToUniversal(Now);
          {$ELSE}
          UTCNow := TTimeZone.Local.ToUniversalTime(Now);
          {$ENDIF}
          Seconds := Trunc((DatePart + TimePart - UTCNow) * 86400) + 1;
        end;
    end;
  end;
  if Seconds < 1 then
    Seconds := 1;
  Remaining := RemainingMs;
  if Seconds > Remaining div 1000 then
    raise ERALException.Create('Retry-After exceeds operation deadline.');
  Delay := Integer(Seconds * 1000);
  Sleep(Delay);
  RemainingMs;
end;

function TLetsEncrypt.Poll(
  const URL: string;
  const TargetStatus: string
): TAcmeResponse;
var
  Status: string;
begin
  repeat
    Result := SignedRequest(URL, '');
    Status := JsonField(Result.Body, 'status');
    if Status = TargetStatus then
      Exit;
    if (Status <> 'pending') and (Status <> 'processing') and (Status <> 'ready') then
      raise ERALException.Create('Unexpected ACME status: ' + Status + '. ' + Copy(Result.Body, 1, 2048));
    WaitRetry(Result.RetryAfter);
  until False;
end;

procedure TLetsEncrypt.Authorize(const URL: string);
var
  R: TAcmeResponse;
  Challenges: TStringList;
  I: Integer;
  J: Integer;
  ChallengeURL: string;
  Token: string;
  ChallengePath: string;
  Content: string;
  Domain: string;
  Found: Boolean;
begin
  R := SignedRequest(URL, '');
  Domain := JsonField(JsonField(R.Body, 'identifier'), 'value');
  Found := False;
  for I := 0 to FDomains.Count - 1 do
    if SameText(FDomains[I], Domain) then
      Found := True;
  if not Found then
    raise ERALException.Create('Unexpected authorization domain.');
  if JsonField(R.Body, 'status') = 'valid' then
    Exit;
  if JsonField(R.Body, 'status') <> 'pending' then
    raise ERALException.Create('Authorization is not pending.');
  Challenges := JsonItems(R.Body, 'challenges');
  try
    ChallengeURL := '';
    Token := '';
    for I := 0 to Challenges.Count - 1 do
      if JsonField(Challenges[I], 'type') = 'http-01' then
      begin
        ChallengeURL := JsonField(Challenges[I], 'url');
        Token := JsonField(Challenges[I], 'token');
        Break;
      end;
    if (ChallengeURL = '') or (Length(Token) < 22) or (Length(Token) > 256) then
      raise ERALException.Create('Missing/invalid HTTP-01 challenge.');
    for J := 1 to Length(Token) do
      if (Ord(Token[J]) > 127) or not CharInSet(Token[J], ['A'..'Z','a'..'z','0'..'9','-','_']) then
        raise ERALException.Create('Invalid challenge token.');
    ChallengePath := IncludeTrailingPathDelimiter(ExpandFileName(FWebRoot)) + '.well-known' + PathDelim + 'acme-challenge';
    if not ForceDirectories(ChallengePath) then
      raise ERALException.Create('Cannot create HTTP challenge directory.');
    ChallengePath := IncludeTrailingPathDelimiter(ChallengePath) + Token;
    if FileExists(ChallengePath) then
      raise ERALException.Create('Challenge file already exists; refusing to overwrite.');
    Content := Token + '.' + Base64Url(Crypto.SHA256(UTF8Encode(Crypto.JWK(AccountKey))));
    SaveBytes(ChallengePath, UTF8Encode(Content), True);
    try
      Log('Validating HTTP-01: ' + Domain);
      SignedRequest(ChallengeURL, '{}');
      Poll(URL, 'valid');
    finally
      if not SysUtils.DeleteFile(ChallengePath) then
        Log('Could not remove challenge file: ' + ChallengePath);
    end;
  finally
    Challenges.Free;
  end;
end;

procedure TLetsEncrypt.Issue(
  RenewOnly: Boolean;
  Days: Integer
);
var
  Root: string;
  CertPath: string;
  AccountPath: string;
  DirectoryURL: string;
  NewAccountURL: string;
  NewOrderURL: string;
  OrderURL: string;
  FinalizeURL: string;
  CertificateURL: string;
  Payload: string;
  Key: RawByteString;
  Chain: RawByteString;
  R: TAcmeResponse;
  Auths: TStringList;
  I: Integer;
begin
  FLastOutput := '';
  FCertificateFile := '';
  FPrivateKeyFile := '';
  Validate;
  Started := TickMilliseconds;
  Nonce := '';
  AccountURL := '';
  Crypto := TRALOpenSSL.Create;
  try
    if FCryptoLibrary <> '' then
      Crypto.LibraryPath := FCryptoLibrary;
  except
    Crypto.Free;
    raise;
  end;
  Root := IncludeTrailingPathDelimiter(ExpandFileName(FStoragePath));
  if FEnvironment = aeStaging then
  begin
    Root := Root + 'staging';
    DirectoryURL := StagingURL;
  end
  else
  begin
    Root := Root + 'production';
    DirectoryURL := ProductionURL;
  end;
  Root := IncludeTrailingPathDelimiter(Root);
  CertPath := Root + LowerCase(FDomains[0]) + PathDelim;
  if not ForceDirectories(CertPath) then
    raise ERALException.Create('Cannot create certificate storage.');
  FCertificateFile := CertPath + 'fullchain.pem';
  FPrivateKeyFile := CertPath + 'privatekey.pem';
  if FileExists(FCertificateFile) and not FileExists(FPrivateKeyFile) then
    raise ERALException.Create('Certificate exists without its private key. Restore the key before continuing.');
  if FileExists(FPrivateKeyFile) then
    Key := ReadBytes(FPrivateKeyFile)
  else
  begin
    Key := Crypto.GenerateKey;
    SaveBytes(FPrivateKeyFile, Key);
  end;
  if RenewOnly and FileExists(FCertificateFile) then
    if Crypto.CertificateDays(ReadBytes(FCertificateFile), Key, FDomains) > Days then
    begin
      Log('Certificate is outside the renewal window.');
      Exit;
    end;
  AccountPath := Root + 'account.key';
  if FileExists(AccountPath) then
    AccountKey := ReadBytes(AccountPath)
  else
  begin
    AccountKey := Crypto.GenerateKey;
    SaveBytes(AccountPath, AccountKey);
  end;
  R := Request('GET', DirectoryURL, '');
  if R.Status <> 200 then
    raise ERALException.Create('Cannot read ACME directory.');
  NewNonceURL := JsonField(R.Body, 'newNonce');
  NewAccountURL := JsonField(R.Body, 'newAccount');
  NewOrderURL := JsonField(R.Body, 'newOrder');
  if (NewNonceURL = '') or (NewAccountURL = '') or (NewOrderURL = '') then
    raise ERALException.Create('Incomplete ACME directory.');
  Log('Registering or locating ACME account.');
  R := SignedRequest(NewAccountURL, '{"termsOfServiceAgreed":true,"contact":[' + JsonQuote('mailto:' + FEmail) + ']}', True);
  AccountURL := R.Location;
  if (AccountURL = '') or (JsonField(R.Body, 'status') <> 'valid') then
    raise ERALException.Create('Invalid ACME account response.');
  Payload := '{"identifiers":[';
  for I := 0 to FDomains.Count - 1 do
  begin
    if I > 0 then
      Payload := Payload + ',';
    Payload := Payload + '{"type":"dns","value":' + JsonQuote(LowerCase(FDomains[I])) + '}';
  end;
  Log('Creating certificate order.');
  R := SignedRequest(NewOrderURL, Payload + ']}');
  OrderURL := R.Location;
  FinalizeURL := JsonField(R.Body, 'finalize');
  if (OrderURL = '') or (FinalizeURL = '') then
    raise ERALException.Create('Incomplete ACME order.');
  Auths := JsonItems(R.Body, 'authorizations');
  try
    if Auths.Count = 0 then
      raise ERALException.Create('Order contains no authorizations.');
    for I := 0 to Auths.Count - 1 do
      Authorize(Auths[I]);
  finally
    Auths.Free;
  end;
  Poll(OrderURL, 'ready');
  SignedRequest(FinalizeURL, '{"csr":' + JsonQuote(Base64Url(Crypto.CSR(Key, FDomains))) + '}');
  R := Poll(OrderURL, 'valid');
  CertificateURL := JsonField(R.Body, 'certificate');
  if CertificateURL = '' then
    raise ERALException.Create('Missing certificate URL.');
  R := SignedRequest(CertificateURL, '');
  Chain := UTF8Encode(R.Body);
  if Crypto.CertificateDays(Chain, Key, FDomains) <= 0 then
    raise ERALException.Create('CA returned an expired certificate.');
  SaveBytes(FCertificateFile, Chain);
  Log('Certificate saved: ' + FCertificateFile);
end;

procedure TLetsEncrypt.ObtainCertificate;
begin
  if FBusy then
    raise ERALException.Create('This component is already executing an operation.');
  FBusy := True;
  try
    try
      Issue(False, 0);
    except
      on E: Exception do
      begin
        FLastOutput := FLastOutput + E.Message + sLineBreak;
        raise;
      end;
    end;
  finally
    FreeAndNil(Http);
    FreeAndNil(Crypto);
    AccountKey := '';
    FBusy := False;
  end;
end;

procedure TLetsEncrypt.RenewCertificate(DaysBeforeExpiry: Integer);
begin
  if FBusy then
    raise ERALException.Create('This component is already executing an operation.');
  if DaysBeforeExpiry < 1 then
    raise ERALException.Create('DaysBeforeExpiry must be positive.');
  FBusy := True;
  try
    try
      Issue(True, DaysBeforeExpiry);
    except
      on E: Exception do
      begin
        FLastOutput := FLastOutput + E.Message + sLineBreak;
        raise;
      end;
    end;
  finally
    FreeAndNil(Http);
    FreeAndNil(Crypto);
    AccountKey := '';
    FBusy := False;
  end;
end;

end.
