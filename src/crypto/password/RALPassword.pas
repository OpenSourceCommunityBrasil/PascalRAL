{ Password hashing: Argon2id (the default), bcrypt, scrypt and PBKDF2, in the
  PHC string format ($argon2id$v=19$m=...) and bcrypt's ($2b$...). Verify
  recognises any of them, NeedsRehash says when a stored hash is weaker than
  the current settings. Pure Pascal, no library. }
unit RALPassword;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALBase64, RALCustomObjects, RALSCRAM,
  RALBlowfish, RALArgon2, RALScrypt;

type
  /// The algorithms of a password hash.
  TRALPasswordAlgorithm = (rpaArgon2id, rpaBcrypt, rpaScrypt, rpaPBKDF2SHA256,
    rpaPBKDF2SHA512);

  { How new hashes are made. The defaults are OWASP's minimums (2023):
    Argon2id m=19 MiB t=2 p=1, bcrypt cost 10, scrypt N=2^17 r=8 p=1,
    PBKDF2-SHA256 600000 and PBKDF2-SHA512 210000 iterations. }
  TRALPasswordOptions = class(TPersistent)
  private
    FAlgorithm: TRALPasswordAlgorithm;
    FArgon2Iterations: IntegerRAL;
    FArgon2MemoryKiB: IntegerRAL;
    FArgon2Parallelism: IntegerRAL;
    FBcryptCost: IntegerRAL;
    FPBKDF2SHA256Iterations: IntegerRAL;
    FPBKDF2SHA512Iterations: IntegerRAL;
    FScryptLogN: IntegerRAL;
    FScryptP: IntegerRAL;
    FScryptR: IntegerRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
  published
    /// Algorithm of new hashes.
    property Algorithm: TRALPasswordAlgorithm read FAlgorithm write FAlgorithm default rpaArgon2id;
    /// Argon2id passes (t).
    property Argon2Iterations: IntegerRAL read FArgon2Iterations write FArgon2Iterations default 2;
    /// Argon2id memory, in KiB (m).
    property Argon2MemoryKiB: IntegerRAL read FArgon2MemoryKiB write FArgon2MemoryKiB default 19456;
    /// Argon2id lanes (p).
    property Argon2Parallelism: IntegerRAL read FArgon2Parallelism write FArgon2Parallelism default 1;
    /// bcrypt cost: 2^cost rounds (4..31).
    property BcryptCost: IntegerRAL read FBcryptCost write FBcryptCost default 10;
    /// PBKDF2-HMAC-SHA256 iterations.
    property PBKDF2SHA256Iterations: IntegerRAL read FPBKDF2SHA256Iterations
      write FPBKDF2SHA256Iterations default 600000;
    /// PBKDF2-HMAC-SHA512 iterations.
    property PBKDF2SHA512Iterations: IntegerRAL read FPBKDF2SHA512Iterations
      write FPBKDF2SHA512Iterations default 210000;
    /// scrypt cost as log2(N).
    property ScryptLogN: IntegerRAL read FScryptLogN write FScryptLogN default 17;
    /// scrypt parallelism (p).
    property ScryptP: IntegerRAL read FScryptP write FScryptP default 1;
    /// scrypt block size (r).
    property ScryptR: IntegerRAL read FScryptR write FScryptR default 8;
  end;

  { A component that hashes and checks passwords with its Options: drop it on a
    form, or use the RALPassword* functions. }
  TRALPasswordHasher = class(TRALComponent)
  private
    FOptions: TRALPasswordOptions;

    procedure SetOptions(const AValue: TRALPasswordOptions);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// A new hash of APassword, with a random salt.
    function Hash(const APassword: StringRAL): StringRAL;
    /// Whether AHash is weaker than Options, or of another algorithm.
    function NeedsRehash(const AHash: StringRAL): boolean;
    /// Whether APassword matches AHash (any supported format), in constant time.
    function Verify(const APassword, AHash: StringRAL): boolean;
  published
    /// How new hashes are made.
    property Options: TRALPasswordOptions read FOptions write SetOptions;
  end;

  TRALPasswordList = class;

  /// Gives the stored hash of AUserName (AHash empty: no such user).
  TRALOnGetPasswordHash = procedure(Sender: TRALPasswordList; const AUserName: StringRAL;
    var AHash: StringRAL) of object;
  { A user logged in with a hash weaker than the settings: ANewHash is the new one to
    store. }
  TRALOnPasswordRehash = procedure(Sender: TRALPasswordList; const AUserName,
    ANewHash: StringRAL) of object;

  { Users and their password hashes, for Basic authentication or any login:
    in Users (lines 'user:hash', the htpasswd layout, or 'user=hash'), in a file
    with that layout (FileName), or given by OnGetHash. Verify takes the same
    time whether the user exists or not. }
  TRALPasswordList = class(TRALComponent)
  private
    /// Age of the file when it was last read, or -1.
    FFileAge: Integer;
    FFileName: string;
    /// Guards the list and its reload.
    FLock: TCriticalSection;
    FOnGetHash: TRALOnGetPasswordHash;
    FOnRehash: TRALOnPasswordRehash;
    FOptions: TRALPasswordOptions;
    FUsers: TStrings;

    /// Reloads FileName when it changed on disk.
    procedure CheckFile;
    procedure SetFileName(const AValue: string);
    procedure SetOptions(const AValue: TRALPasswordOptions);
    procedure SetUsers(const AValue: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// The stored hash of AUserName, '' when unknown.
    function HashOf(const AUserName: StringRAL): StringRAL;
    /// Writes Users to FileName.
    procedure Save;
    /// Stores a new hash of APassword for AUserName (in Users; Save writes the file).
    procedure SetPassword(const AUserName, APassword: StringRAL);
    { Whether APassword is the password of AUserName. An unknown user costs the
      same as a wrong password. On success with a weak hash, OnRehash gets a
      new one (and Users is updated). }
    function Verify(const AUserName, APassword: StringRAL): boolean;
  published
    { File of 'user:hash' lines (htpasswd -B writes them); read when it changes.
      A relative name is taken from the executable's folder. }
    property FileName: string read FFileName write SetFileName;
    /// Asked for the hash of a user not in Users.
    property OnGetHash: TRALOnGetPasswordHash read FOnGetHash write FOnGetHash;
    /// A user logged in with a weak hash: store ANewHash.
    property OnRehash: TRALOnPasswordRehash read FOnRehash write FOnRehash;
    /// How SetPassword and the rehash make new hashes.
    property Options: TRALPasswordOptions read FOptions write SetOptions;
    /// Users and hashes, one 'user:hash' (or 'user=hash') per line.
    property Users: TStrings read FUsers write SetUsers;
  end;

/// A new hash of APassword with the defaults (Argon2id, OWASP parameters).
function RALPasswordHash(const APassword: StringRAL): StringRAL; overload;
/// A new hash of APassword with AAlgorithm and its default cost.
function RALPasswordHash(const APassword: StringRAL;
  AAlgorithm: TRALPasswordAlgorithm): StringRAL; overload;
/// A new hash of APassword with AOptions.
function RALPasswordHash(const APassword: StringRAL;
  AOptions: TRALPasswordOptions): StringRAL; overload;
{ Whether APassword matches AHash: $argon2id$/$argon2i$/$argon2d$, $2a$/$2b$/$2y$,
  $scrypt$, $pbkdf2-sha256$/$pbkdf2-sha512$. }
function RALPasswordVerify(const APassword, AHash: StringRAL): boolean;
/// Whether AHash is of another algorithm than AOptions, or cheaper; nil = the defaults.
function RALPasswordNeedsRehash(const AHash: StringRAL;
  AOptions: TRALPasswordOptions = nil): boolean;
/// AData in base64 without padding (the PHC format's).
function RALPHCBase64(const AData: TBytes): StringRAL;
/// Decodes base64 with or without padding; False when it is not base64.
function RALPHCBase64Decode(const AText: StringRAL; out AData: TBytes): boolean;

implementation

const
  /// The Argon2 version written and the only one read (1.3).
  cArgon2DefaultVersion = $13;
  /// Bytes of the random salt of a new hash.
  cSaltLength = 16;
  /// Bytes of the derived key of a new hash.
  cHashLength = 32;

type
  /// A hash string split into its parts.
  TRALParsedHash = record
    /// The algorithm the hash names.
    Algorithm: TRALPasswordAlgorithm;
    /// The Argon2 variant (id, i, d).
    Argon: TRALArgon2Type;
    /// The Argon2 version.
    Version: IntegerRAL;
    /// The cost parameters; each algorithm reads its own.
    M, T, P, R, LogN, Iterations, Cost: Int64RAL;
    /// The salt and the derived key, decoded.
    Salt, Hash: TBytes;
    /// The bcrypt salt as written, 22 characters.
    SaltText: StringRAL;
  end;

/// The UTF-8 bytes of AText.
function BytesOf(const AText: StringRAL): TBytes;
begin
  Result := RALUTF8Bytes(AText);
end;

function RALPHCBase64(const AData: TBytes): StringRAL;
var
  vLen: IntegerRAL;
begin
  Result := TRALBase64.Encode(AData);
  vLen := Length(Result);
  while (vLen > 0) and (Result[POSINISTR + vLen - 1] = '=') do
    Dec(vLen);
  SetLength(Result, vLen);
end;

function RALPHCBase64Decode(const AText: StringRAL; out AData: TBytes): boolean;
var
  vText: StringRAL;
  vInt: IntegerRAL;
begin
  Result := False;
  AData := nil;
  for vInt := POSINISTR to RALHighStr(AText) do
    if not (AText[vInt] in ['A'..'Z', 'a'..'z', '0'..'9', '+', '/', '=']) then
      Exit;
  vText := AText;
  while (Length(vText) mod 4) <> 0 do
    vText := vText + '=';
  try
    AData := TRALBase64.DecodeAsBytes(vText);
    Result := True;
  except
    Result := False;
  end;
end;

/// 'm=19456,t=2,p=1' into the record.
procedure ParseParams(const AText: StringRAL; var AHash: TRALParsedHash);
var
  vList: TStringList;
  vInt: IntegerRAL;
  vName: string;
  vValue: Int64RAL;
begin
  vList := TStringList.Create;
  try
    vList.Text := StringReplace(string(AText), ',', sLineBreak, [rfReplaceAll]);
    for vInt := 0 to vList.Count - 1 do
    begin
      vName := LowerCase(Trim(vList.Names[vInt]));
      vValue := StrToInt64Def(Trim(vList.ValueFromIndex[vInt]), -1);
      if vName = 'm' then AHash.M := vValue
      else if vName = 't' then AHash.T := vValue
      else if vName = 'p' then AHash.P := vValue
      else if vName = 'r' then AHash.R := vValue
      else if vName = 'ln' then AHash.LogN := vValue
      else if vName = 'i' then AHash.Iterations := vValue
      else if vName = 'v' then AHash.Version := vValue;
    end;
  finally
    vList.Free;
  end;
end;

/// Splits AHash into its parts; False when it is no hash this unit knows.
function ParseHash(const AHash: StringRAL; out AParsed: TRALParsedHash): boolean;
var
  vParts: TStringList;
  vId: string;
  vIdx: IntegerRAL;
begin
  Result := False;
  AParsed.Algorithm := rpaArgon2id;
  AParsed.Argon := ra2id;
  AParsed.Salt := nil;
  AParsed.Hash := nil;
  AParsed.SaltText := '';
  AParsed.M := -1; AParsed.T := -1; AParsed.P := -1; AParsed.R := -1;
  AParsed.LogN := -1; AParsed.Iterations := -1; AParsed.Cost := -1;
  AParsed.Version := cArgon2DefaultVersion;
  if (Length(AHash) < 4) or (AHash[POSINISTR] <> '$') then
    Exit;

  vParts := TStringList.Create;
  try
    vParts.StrictDelimiter := True;
    vParts.Delimiter := '$';
    vParts.DelimitedText := string(Copy(AHash, POSINISTR + 1, Length(AHash)));
    if vParts.Count < 3 then
      Exit;
    vId := vParts[0];

    if (vId = '2a') or (vId = '2b') or (vId = '2y') then
    begin
      { $2b$12$ + 22 characters of salt + 31 of hash }
      if (vParts.Count <> 3) or (Length(vParts[2]) <> 53) then
        Exit;
      AParsed.Algorithm := rpaBcrypt;
      AParsed.Cost := StrToInt64Def(vParts[1], -1);
      AParsed.SaltText := StringRAL(Copy(vParts[2], 1, 22));
      Result := RALBcryptDecode64(AParsed.SaltText, 16, AParsed.Salt) and
        RALBcryptDecode64(StringRAL(Copy(vParts[2], 23, 31)), 23, AParsed.Hash) and
        (AParsed.Cost >= 4) and (AParsed.Cost <= 31);
      Exit;
    end;

    vIdx := 1;
    if (vId = 'argon2id') or (vId = 'argon2i') or (vId = 'argon2d') then
    begin
      if vId = 'argon2id' then AParsed.Argon := ra2id
      else if vId = 'argon2i' then AParsed.Argon := ra2i
      else AParsed.Argon := ra2d;
      AParsed.Algorithm := rpaArgon2id;
      { the version is its own field: $argon2id$v=19$m=...; old hashes lack it }
      if Copy(vParts[1], 1, 2) = 'v=' then
      begin
        ParseParams(StringRAL(vParts[1]), AParsed);
        vIdx := 2;
      end
      else
        AParsed.Version := $10;
    end
    else if vId = 'scrypt' then
      AParsed.Algorithm := rpaScrypt
    else if vId = 'pbkdf2-sha256' then
      AParsed.Algorithm := rpaPBKDF2SHA256
    else if vId = 'pbkdf2-sha512' then
      AParsed.Algorithm := rpaPBKDF2SHA512
    else
      Exit;

    if vParts.Count <> vIdx + 3 then
      Exit;
    ParseParams(StringRAL(vParts[vIdx]), AParsed);
    Result := RALPHCBase64Decode(StringRAL(vParts[vIdx + 1]), AParsed.Salt) and
      RALPHCBase64Decode(StringRAL(vParts[vIdx + 2]), AParsed.Hash) and
      (Length(AParsed.Hash) >= 4);
  finally
    vParts.Free;
  end;
end;

/// The raw hash of APassword with the parameters and salt of AParsed.
function Compute(const APassword: StringRAL; const AParsed: TRALParsedHash): TBytes;
var
  vPass: TBytes;
begin
  vPass := BytesOf(APassword);
  case AParsed.Algorithm of
    rpaArgon2id:
    begin
      if AParsed.Version <> cArgon2DefaultVersion then
        raise ERangeError.Create('Argon2: only version 19 (1.3)');
      Result := RALArgon2Hash(RALArgon2Params(AParsed.Argon, AParsed.M, AParsed.T,
        AParsed.P, Length(AParsed.Hash)), vPass, AParsed.Salt);
    end;
    rpaBcrypt:
      Result := RALBcryptRaw(APassword, AParsed.Salt, AParsed.Cost);
    rpaScrypt:
    begin
      if (AParsed.LogN < 1) or (AParsed.LogN > 30) then
        raise ERangeError.Create('scrypt: ln out of range');
      Result := RALScryptHash(vPass, AParsed.Salt, Cardinal(1) shl AParsed.LogN,
        AParsed.R, AParsed.P, Length(AParsed.Hash));
    end;
    rpaPBKDF2SHA256:
      Result := RALSCRAMPBKDF2(rsaSHA256, vPass, AParsed.Salt, AParsed.Iterations,
        Length(AParsed.Hash));
    rpaPBKDF2SHA512:
      Result := RALSCRAMPBKDF2(rsaSHA512, vPass, AParsed.Salt, AParsed.Iterations,
        Length(AParsed.Hash));
  end;
end;

function RALPasswordHash(const APassword: StringRAL; AOptions: TRALPasswordOptions): StringRAL;
var
  vSalt, vHash, vPass: TBytes;
  vOwned: boolean;
  vLogN: IntegerRAL;
begin
  vOwned := AOptions = nil;
  if vOwned then
    AOptions := TRALPasswordOptions.Create;
  try
    vSalt := RandomBytes(cSaltLength);
    vPass := BytesOf(APassword);
    case AOptions.Algorithm of
      rpaArgon2id:
      begin
        vHash := RALArgon2Hash(RALArgon2Params(ra2id, AOptions.Argon2MemoryKiB,
          AOptions.Argon2Iterations, AOptions.Argon2Parallelism, cHashLength), vPass, vSalt);
        Result := StringRAL(Format('$argon2id$v=19$m=%d,t=%d,p=%d$',
          [AOptions.Argon2MemoryKiB, AOptions.Argon2Iterations, AOptions.Argon2Parallelism])) +
          RALPHCBase64(vSalt) + '$' + RALPHCBase64(vHash);
      end;
      rpaBcrypt:
      begin
        vHash := RALBcryptRaw(APassword, vSalt, AOptions.BcryptCost);
        Result := StringRAL(Format('$2b$%.2d$', [AOptions.BcryptCost])) +
          RALBcryptEncode64(vSalt, 16) + RALBcryptEncode64(vHash, 23);
      end;
      rpaScrypt:
      begin
        vLogN := AOptions.ScryptLogN;
        vHash := RALScryptHash(vPass, vSalt, Cardinal(1) shl vLogN, AOptions.ScryptR,
          AOptions.ScryptP, cHashLength);
        Result := StringRAL(Format('$scrypt$ln=%d,r=%d,p=%d$',
          [vLogN, AOptions.ScryptR, AOptions.ScryptP])) +
          RALPHCBase64(vSalt) + '$' + RALPHCBase64(vHash);
      end;
      rpaPBKDF2SHA256:
      begin
        vHash := RALSCRAMPBKDF2(rsaSHA256, vPass, vSalt, AOptions.PBKDF2SHA256Iterations,
          cHashLength);
        Result := StringRAL(Format('$pbkdf2-sha256$i=%d,l=%d$',
          [AOptions.PBKDF2SHA256Iterations, cHashLength])) +
          RALPHCBase64(vSalt) + '$' + RALPHCBase64(vHash);
      end;
      rpaPBKDF2SHA512:
      begin
        vHash := RALSCRAMPBKDF2(rsaSHA512, vPass, vSalt, AOptions.PBKDF2SHA512Iterations, 64);
        Result := StringRAL(Format('$pbkdf2-sha512$i=%d,l=%d$',
          [AOptions.PBKDF2SHA512Iterations, 64])) +
          RALPHCBase64(vSalt) + '$' + RALPHCBase64(vHash);
      end;
    end;
  finally
    if vOwned then
      AOptions.Free;
  end;
end;

function RALPasswordHash(const APassword: StringRAL): StringRAL;
begin
  Result := RALPasswordHash(APassword, TRALPasswordOptions(nil));
end;

function RALPasswordHash(const APassword: StringRAL;
  AAlgorithm: TRALPasswordAlgorithm): StringRAL;
var
  vOptions: TRALPasswordOptions;
begin
  vOptions := TRALPasswordOptions.Create;
  try
    vOptions.Algorithm := AAlgorithm;
    Result := RALPasswordHash(APassword, vOptions);
  finally
    vOptions.Free;
  end;
end;

function RALPasswordVerify(const APassword, AHash: StringRAL): boolean;
var
  vParsed: TRALParsedHash;
begin
  Result := False;
  if not ParseHash(AHash, vParsed) then
    Exit;
  try
    Result := RALSameBytes(Compute(APassword, vParsed), vParsed.Hash);
  except
    { parameters no implementation accepts: no password matches them }
    Result := False;
  end;
end;

function RALPasswordNeedsRehash(const AHash: StringRAL; AOptions: TRALPasswordOptions): boolean;
var
  vParsed: TRALParsedHash;
  vOwned: boolean;
begin
  Result := True;
  if not ParseHash(AHash, vParsed) then
    Exit;
  vOwned := AOptions = nil;
  if vOwned then
    AOptions := TRALPasswordOptions.Create;
  try
    if vParsed.Algorithm <> AOptions.Algorithm then
      Exit;
    case vParsed.Algorithm of
      rpaArgon2id:
        Result := (vParsed.Argon <> ra2id) or (vParsed.M < AOptions.Argon2MemoryKiB) or
          (vParsed.T < AOptions.Argon2Iterations) or (vParsed.P < AOptions.Argon2Parallelism);
      rpaBcrypt:
        Result := vParsed.Cost < AOptions.BcryptCost;
      rpaScrypt:
        Result := (vParsed.LogN < AOptions.ScryptLogN) or (vParsed.R < AOptions.ScryptR) or
          (vParsed.P < AOptions.ScryptP);
      rpaPBKDF2SHA256:
        Result := vParsed.Iterations < AOptions.PBKDF2SHA256Iterations;
      rpaPBKDF2SHA512:
        Result := vParsed.Iterations < AOptions.PBKDF2SHA512Iterations;
    end;
  finally
    if vOwned then
      AOptions.Free;
  end;
end;

{ TRALPasswordOptions }

constructor TRALPasswordOptions.Create;
begin
  inherited Create;
  FAlgorithm := rpaArgon2id;
  FArgon2Iterations := 2;
  FArgon2MemoryKiB := 19456;
  FArgon2Parallelism := 1;
  FBcryptCost := 10;
  FPBKDF2SHA256Iterations := 600000;
  FPBKDF2SHA512Iterations := 210000;
  FScryptLogN := 17;
  FScryptP := 1;
  FScryptR := 8;
end;

procedure TRALPasswordOptions.AssignTo(Dest: TPersistent);
var
  vDest: TRALPasswordOptions;
begin
  if not (Dest is TRALPasswordOptions) then
  begin
    inherited AssignTo(Dest);
    Exit;
  end;
  vDest := TRALPasswordOptions(Dest);
  vDest.Algorithm := FAlgorithm;
  vDest.Argon2Iterations := FArgon2Iterations;
  vDest.Argon2MemoryKiB := FArgon2MemoryKiB;
  vDest.Argon2Parallelism := FArgon2Parallelism;
  vDest.BcryptCost := FBcryptCost;
  vDest.PBKDF2SHA256Iterations := FPBKDF2SHA256Iterations;
  vDest.PBKDF2SHA512Iterations := FPBKDF2SHA512Iterations;
  vDest.ScryptLogN := FScryptLogN;
  vDest.ScryptP := FScryptP;
  vDest.ScryptR := FScryptR;
end;

{ TRALPasswordHasher }

constructor TRALPasswordHasher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TRALPasswordOptions.Create;
end;

destructor TRALPasswordHasher.Destroy;
begin
  FreeAndNil(FOptions);
  inherited;
end;

procedure TRALPasswordHasher.SetOptions(const AValue: TRALPasswordOptions);
begin
  FOptions.Assign(AValue);
end;

function TRALPasswordHasher.Hash(const APassword: StringRAL): StringRAL;
begin
  Result := RALPasswordHash(APassword, FOptions);
end;

function TRALPasswordHasher.NeedsRehash(const AHash: StringRAL): boolean;
begin
  Result := RALPasswordNeedsRehash(AHash, FOptions);
end;

function TRALPasswordHasher.Verify(const APassword, AHash: StringRAL): boolean;
begin
  Result := RALPasswordVerify(APassword, AHash);
end;

{ TRALPasswordList }

constructor TRALPasswordList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FOptions := TRALPasswordOptions.Create;
  FUsers := TStringList.Create;
  FFileAge := -1;
end;

destructor TRALPasswordList.Destroy;
begin
  FreeAndNil(FUsers);
  FreeAndNil(FOptions);
  FreeAndNil(FLock);
  inherited;
end;

procedure TRALPasswordList.SetFileName(const AValue: string);
begin
  FFileName := AValue;
  FFileAge := -1;
end;

procedure TRALPasswordList.SetOptions(const AValue: TRALPasswordOptions);
begin
  FOptions.Assign(AValue);
end;

procedure TRALPasswordList.SetUsers(const AValue: TStrings);
begin
  FLock.Acquire;
  try
    FUsers.Assign(AValue);
  finally
    FLock.Release;
  end;
end;

procedure TRALPasswordList.CheckFile;
var
  vFile: string;
  vAge: Integer;
begin
  if FFileName = '' then
    Exit;
  vFile := FFileName;
  if ExtractFilePath(vFile) = '' then
    vFile := ExtractFilePath(ParamStr(0)) + vFile;
  vAge := FileAge(vFile);
  if (vAge = FFileAge) or (vAge = -1) then
    Exit;
  FUsers.LoadFromFile(vFile);
  FFileAge := vAge;
end;

function TRALPasswordList.HashOf(const AUserName: StringRAL): StringRAL;
var
  vInt, vSep: IntegerRAL;
  vLine: string;
begin
  Result := '';
  FLock.Acquire;
  try
    CheckFile;
    for vInt := 0 to FUsers.Count - 1 do
    begin
      vLine := FUsers[vInt];
      vSep := Pos(':', vLine);
      if (vSep = 0) or ((Pos('=', vLine) > 0) and (Pos('=', vLine) < vSep)) then
        vSep := Pos('=', vLine);
      if (vSep > 1) and (StringRAL(Copy(vLine, 1, vSep - 1)) = AUserName) then
      begin
        Result := StringRAL(Trim(Copy(vLine, vSep + 1, MaxInt)));
        Break;
      end;
    end;
  finally
    FLock.Release;
  end;
  if (Result = '') and Assigned(FOnGetHash) then
    FOnGetHash(Self, AUserName, Result);
end;

procedure TRALPasswordList.SetPassword(const AUserName, APassword: StringRAL);
var
  vHash: StringRAL;
  vInt, vSep: IntegerRAL;
  vLine: string;
begin
  vHash := RALPasswordHash(APassword, FOptions);
  FLock.Acquire;
  try
    for vInt := 0 to FUsers.Count - 1 do
    begin
      vLine := FUsers[vInt];
      vSep := Pos(':', vLine);
      if (vSep > 1) and (StringRAL(Copy(vLine, 1, vSep - 1)) = AUserName) then
      begin
        FUsers[vInt] := string(AUserName + ':' + vHash);
        Exit;
      end;
    end;
    FUsers.Add(string(AUserName + ':' + vHash));
  finally
    FLock.Release;
  end;
end;

procedure TRALPasswordList.Save;
var
  vFile: string;
begin
  if FFileName = '' then
    Exit;
  vFile := FFileName;
  if ExtractFilePath(vFile) = '' then
    vFile := ExtractFilePath(ParamStr(0)) + vFile;
  FLock.Acquire;
  try
    FUsers.SaveToFile(vFile);
    FFileAge := FileAge(vFile);
  finally
    FLock.Release;
  end;
end;

var
  { a hash nobody's password matches, checked for unknown users so that a
    missing user costs what a wrong password costs }
  gDummyHash: StringRAL;
  /// Guards the creation of gDummyHash.
  gDummyLock: TCriticalSection;

/// The hash checked for an unknown user, created once with AOptions.
function DummyHash(AOptions: TRALPasswordOptions): StringRAL;
begin
  gDummyLock.Acquire;
  try
    if gDummyHash = '' then
      gDummyHash := RALPasswordHash(RALPHCBase64(RandomBytes(16)), AOptions);
    Result := gDummyHash;
  finally
    gDummyLock.Release;
  end;
end;

function TRALPasswordList.Verify(const AUserName, APassword: StringRAL): boolean;
var
  vHash, vNew: StringRAL;
begin
  vHash := HashOf(AUserName);
  if vHash = '' then
  begin
    RALPasswordVerify(APassword, DummyHash(FOptions));
    Result := False;
    Exit;
  end;
  Result := RALPasswordVerify(APassword, vHash);
  if Result and Assigned(FOnRehash) and RALPasswordNeedsRehash(vHash, FOptions) then
  begin
    SetPassword(AUserName, APassword);
    vNew := HashOf(AUserName);
    FOnRehash(Self, AUserName, vNew);
  end;
end;

initialization
  gDummyLock := TCriticalSection.Create;

finalization
  FreeAndNil(gDummyLock);

end.
