{ DKIM (RFC 6376) signatures of outgoing mail and their verification, with
  RSA-SHA256 and Ed25519-SHA256 (RFC 8463). }
unit RALDKIM;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALBase64, RALCustomObjects, RALExceptions,
  RALHashBase, RALSHA2_32, RALRSA, RALEd25519, RALJobStore;

type
  /// The signing algorithm: a= of the signature.
  TRALDKIMAlgorithm = (
    /// rsa-sha256: understood by every verifier.
    dkaRSASHA256,
    /// ed25519-sha256 (RFC 8463): short keys and signatures; not every verifier knows it.
    dkaEd25519SHA256
  );

  /// How the headers or the body are canonicalized before hashing: c= of the signature.
  TRALDKIMCanonicalization = (
    /// relaxed: survives the whitespace changes and refolding of relays.
    dkcRelaxed,
    /// simple: the bytes as they are.
    dkcSimple
  );

  /// The verdict on one signature, in the words of Authentication-Results (RFC 8601).
  TRALDKIMVerdict = (
    /// The signature checks.
    dkvPass,
    /// The signature or the body hash does not match.
    dkvFail,
    /// The signature cannot be checked: a malformed tag, an unknown algorithm, no key.
    dkvPermError,
    /// The key could not be fetched now (a DNS failure): try again later.
    dkvTempError,
    /// The key was revoked (an empty p=).
    dkvPolicy
  );

  /// Error reading a key or signing.
  ERALDKIMError = class(ERALException);

  /// One checked signature.
  TRALDKIMResult = class
  public
    /// The signing algorithm, as written (a=).
    Algorithm: StringRAL;
    /// The signing domain (d=).
    Domain: StringRAL;
    /// The agent or user identifier (i=), when present.
    Identity: StringRAL;
    /// Why it did not pass; empty when it passed.
    Reason: StringRAL;
    /// The selector (s=).
    Selector: StringRAL;
    /// The verdict.
    Verdict: TRALDKIMVerdict;
  end;

  { Fetches the key record of ASelector._domainkey.ADomain (the TXT text, the
    strings of the record joined); ATemporary when the lookup failed and may work
    later. An empty ARecord with ATemporary False means there is no record. }
  TRALDKIMKeyLookup = procedure(Sender: TObject; const ASelector, ADomain: StringRAL;
    var ARecord: StringRAL; var ATemporary: Boolean) of object;

  { Signs messages on their way out (RFC 6376): link it to a TRALSMTPClient
    (DKIM) and every message it sends carries a DKIM-Signature of Domain with the
    key published at Selector._domainkey.Domain. Thread-safe once configured: the
    key is read once and only read afterwards. }
  TRALDKIMSigner = class(TRALComponent)
  private
    FAlgorithm: TRALDKIMAlgorithm;
    FBodyCanonicalization: TRALDKIMCanonicalization;
    FDomain: StringRAL;
    FExpireAfter: IntegerRAL;
    FHeaderCanonicalization: TRALDKIMCanonicalization;
    FIdentity: StringRAL;
    /// Guards the loaded key.
    FLock: TCriticalSection;
    FOverSign: Boolean;
    FPrivateKey: TStrings;
    FPrivateKeyFile: string;
    /// The RSA private key, when the key is RSA.
    FRSAKey: TRALRSAKey;
    /// The Ed25519 seed (32 bytes), when the key is Ed25519.
    FSeed: TBytes;
    FSelector: StringRAL;
    FSignedHeaders: TStrings;
    FSignTime: Int64;

    /// Drops the loaded key when the key text changes.
    procedure KeyChanged(Sender: TObject);
    /// Loads the key if it is not loaded yet; raises when there is none or it is invalid.
    procedure LoadKey;
    /// Parses PrivateKey or PrivateKeyFile into the RSA key or the Ed25519 seed.
    procedure ReadKey;
    procedure SetPrivateKey(AValue: TStrings);
    procedure SetPrivateKeyFile(const AValue: string);
    procedure SetSignedHeaders(AValue: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// The TXT record to publish at Selector._domainkey.Domain for the key in use.
    function KeyRecord: StringRAL;
    { The DKIM-Signature header field for the message in AData (headers, empty
      line, body), ending in CRLF, to be written before the message. AData is read
      from its position to the end and left where it was. }
    function Sign(AData: TStream): RawByteString;
    /// Writes the signature and then the message of AData to AOutput.
    procedure SignTo(AData, AOutput: TStream);

    { The signing time in Unix seconds; 0 (the default) takes the clock. Set it to
      reproduce a signature. }
    property SignTime: Int64 read FSignTime write FSignTime;
  published
    /// The signing algorithm; the key must be of that kind.
    property Algorithm: TRALDKIMAlgorithm read FAlgorithm write FAlgorithm default dkaRSASHA256;
    /// The body canonicalization.
    property BodyCanonicalization: TRALDKIMCanonicalization read FBodyCanonicalization
      write FBodyCanonicalization default dkcRelaxed;
    /// The signing domain (d=): the domain of From for DMARC to accept it.
    property Domain: StringRAL read FDomain write FDomain;
    /// Seconds the signature stays valid (x=); 0 = no expiry.
    property ExpireAfter: IntegerRAL read FExpireAfter write FExpireAfter default 0;
    /// The header canonicalization.
    property HeaderCanonicalization: TRALDKIMCanonicalization read FHeaderCanonicalization
      write FHeaderCanonicalization default dkcRelaxed;
    /// The identity (i=), an address or @@subdomain of Domain; empty leaves it out.
    property Identity: StringRAL read FIdentity write FIdentity;
    { Each signed header is listed once more than it occurs, so a relay cannot add
      a second From or Subject without breaking the signature. }
    property OverSign: Boolean read FOverSign write FOverSign default True;
    { The private key: an RSA key in PEM (PKCS#1 or PKCS#8) or, for Ed25519, the
      32-byte seed in base64 (what RFC 8463 shows) or a PEM "PRIVATE KEY". }
    property PrivateKey: TStrings read FPrivateKey write SetPrivateKey;
    /// A file with the private key, used when PrivateKey is empty.
    property PrivateKeyFile: string read FPrivateKeyFile write SetPrivateKeyFile;
    /// The selector (s=): the name the key is published under.
    property Selector: StringRAL read FSelector write FSelector;
    /// The header fields to sign, those present in the message; From is always signed.
    property SignedHeaders: TStrings read FSignedHeaders write SetSignedHeaders;
  end;

  { Checks the DKIM signatures of received messages. The key records come from
    OnKeyLookup (TRALMailVerifier links its DNS resolver there). }
  TRALDKIMVerifier = class(TRALComponent)
  private
    FMaxSignatures: IntegerRAL;
    FOnKeyLookup: TRALDKIMKeyLookup;
  public
    constructor Create(AOwner: TComponent); override;

    { Checks every DKIM-Signature of the message in AData (read from its position,
      left where it was) and adds a TRALDKIMResult per signature to AResults (the
      caller frees them). Returns whether at least one passed. }
    function Verify(AData: TStream; AResults: TList): Boolean;
  published
    /// Signatures checked per message; the rest are ignored (each costs a lookup).
    property MaxSignatures: IntegerRAL read FMaxSignatures write FMaxSignatures default 5;
    /// Fetches a key record.
    property OnKeyLookup: TRALDKIMKeyLookup read FOnKeyLookup write FOnKeyLookup;
  end;

/// The verdict as Authentication-Results names it: pass, fail, permerror...
function RALDKIMVerdictName(AVerdict: TRALDKIMVerdict): StringRAL;

{ The value of tag AName in a DKIM tag list (a signature or a key record), with
  the folding whitespace removed; empty when the tag is absent. }
function RALDKIMTag(const ATags: RawByteString; const AName: RawByteString): RawByteString;

implementation

const
  /// The header fields signed when SignedHeaders is empty, one per line.
  cDefaultSigned = 'From'#13#10'Reply-To'#13#10'Subject'#13#10'Date'#13#10'To'#13#10 +
    'Cc'#13#10'Message-ID'#13#10'In-Reply-To'#13#10'References'#13#10'MIME-Version'#13#10 +
    'Content-Type'#13#10'Content-Transfer-Encoding'#13#10'List-Unsubscribe'#13#10 +
    'List-Unsubscribe-Post';
  /// Bytes read from the message stream at a time.
  cReadBlock = 65536;

type
  { one header field of the message: the name as written and the whole field
    (name, colon, value with its folding) without the final CRLF }
  TRALDKIMHeader = record
    /// The field name as written.
    Name: RawByteString;
    /// The whole field, with its folding and without the final CRLF.
    Raw: RawByteString;
  end;
  /// The header fields of a message, in order.
  TRALDKIMHeaders = array of TRALDKIMHeader;

  { relaxed or simple body canonicalization fed in blocks, hashed as it goes
    (RFC 6376 3.4.3 and 3.4.4): empty lines are held back until a non-empty one
    follows, so the trailing ones never reach the hash }
  TRALDKIMBodyHasher = class
  private
    /// Empty lines held back until a non-empty one follows.
    FEmpty: Int64;
    /// The running SHA-256 of the canonical body.
    FHash: TRALSHA2_32;
    /// The partial line being collected.
    FLine: RawByteString;
    /// Bytes used in the partial line.
    FLineLen: IntegerRAL;
    /// True for relaxed canonicalization, False for simple.
    FRelaxed: Boolean;
    /// True once a non-empty line was seen.
    FSeenData: Boolean;

    /// Canonicalizes the collected line and hashes it, or holds it back when empty.
    procedure AddLine;
    /// Collects AText line by line.
    procedure Feed(const AText: RawByteString);
  public
    /// Creates a hasher for relaxed (True) or simple (False) canonicalization.
    constructor Create(ARelaxed: Boolean);
    destructor Destroy; override;

    /// Ends the body and returns its hash.
    function Finish: TBytes;
    /// Adds ALength bytes of body text.
    procedure Update(AData: PAnsiChar; ALength: IntegerRAL);
  end;

/// True for a space or a tab.
function IsWSP(AChar: AnsiChar): Boolean;
begin
  Result := (AChar = ' ') or (AChar = #9);
end;

/// AText with A-Z turned into a-z.
function LowerAscii(const AText: RawByteString): RawByteString;
var
  vInt: IntegerRAL;
begin
  // a new string: Delphi has no UniqueString for RawByteString
  SetLength(Result, Length(AText));
  for vInt := 1 to Length(AText) do
    if AText[vInt] in ['A'..'Z'] then
      Result[vInt] := AnsiChar(Ord(AText[vInt]) + 32)
    else
      Result[vInt] := AText[vInt];
end;

/// AText without leading and trailing spaces and tabs.
function TrimWSP(const AText: RawByteString): RawByteString;
var
  vStart, vEnd: IntegerRAL;
begin
  vStart := 1;
  vEnd := Length(AText);
  while (vStart <= vEnd) and (AText[vStart] in [' ', #9, #13, #10]) do
    Inc(vStart);
  while (vEnd >= vStart) and (AText[vEnd] in [' ', #9, #13, #10]) do
    Dec(vEnd);
  Result := Copy(AText, vStart, vEnd - vStart + 1);
end;

/// AText without any folding white space (spaces, tabs and line breaks).
function StripFWS(const AText: RawByteString): RawByteString;
var
  vInt, vOut: IntegerRAL;
begin
  SetLength(Result, Length(AText));
  vOut := 0;
  for vInt := 1 to Length(AText) do
    if not (AText[vInt] in [' ', #9, #13, #10]) then
    begin
      Inc(vOut);
      Result[vOut] := AText[vInt];
    end;
  SetLength(Result, vOut);
end;

/// The bytes of AText.
function BytesOfRaw(const AText: RawByteString): TBytes;
begin
  SetLength(Result, Length(AText));
  if Length(AText) > 0 then
    Move(AText[1], Result[0], Length(AText));
end;

/// The bytes as a string.
function RawOfBytes(const ABytes: TBytes): RawByteString;
begin
  SetLength(Result, Length(ABytes));
  if Length(ABytes) > 0 then
    Move(ABytes[0], Result[1], Length(ABytes));
end;

/// The base64 text of ABytes.
function Base64Raw(const ABytes: TBytes): RawByteString;
begin
  Result := RawByteString(TRALBase64.Encode(ABytes));
end;

/// The SHA-256 of AText.
function SHA256Raw(const AText: RawByteString): TBytes;
var
  vHash: TRALSHA2_32;
begin
  vHash := TRALSHA2_32.Create;
  try
    vHash.HashBegin;
    if AText <> '' then
      vHash.HashUpdate(@AText[1], Length(AText));
    Result := vHash.HashEnd;
  finally
    vHash.Free;
  end;
end;

{ the header block of the message: up to the empty line, read from AData;
  ABody gets what was read past it. AData is left after what was read. }
procedure ReadHeaders(AData: TStream; var AHeaders: TRALDKIMHeaders; out ABody: RawByteString);
var
  vBlock, vText: RawByteString;
  vRead, vEnd, vStart, vInt, vCount: IntegerRAL;
begin
  vText := '';
  SetLength(vBlock, cReadBlock);
  repeat
    vRead := AData.Read(vBlock[1], cReadBlock);
    if vRead > 0 then
      vText := vText + Copy(vBlock, 1, vRead);
    if Copy(vText, 1, 2) = #13#10 then
      vEnd := 1
    else
      vEnd := Pos(RawByteString(#13#10#13#10), vText);
  until (vEnd > 0) or (vRead <= 0);
  if vEnd = 0 then
  begin
    // headers only
    if Copy(vText, Length(vText) - 1, 2) <> #13#10 then
      vText := vText + #13#10;
    ABody := '';
  end
  else if vEnd = 1 then
  begin
    ABody := Copy(vText, 3, MaxInt);
    vText := '';
  end
  else
  begin
    ABody := Copy(vText, vEnd + 4, MaxInt);
    vText := Copy(vText, 1, vEnd + 1);
  end;

  vCount := 0;
  SetLength(AHeaders, 0);
  vStart := 1;
  vInt := 1;
  while vInt <= Length(vText) - 1 do
  begin
    if (vText[vInt] = #13) and (vText[vInt + 1] = #10) and
       ((vInt + 2 > Length(vText)) or not IsWSP(vText[vInt + 2])) then
    begin
      if vInt > vStart then
      begin
        SetLength(AHeaders, vCount + 1);
        AHeaders[vCount].Raw := Copy(vText, vStart, vInt - vStart);
        AHeaders[vCount].Name := TrimWSP(Copy(AHeaders[vCount].Raw, 1,
          Pos(RawByteString(':'), AHeaders[vCount].Raw) - 1));
        Inc(vCount);
      end;
      vStart := vInt + 2;
      Inc(vInt, 2);
    end
    else
      Inc(vInt);
  end;
end;

/// One header field canonicalized, with its CRLF unless ANoCRLF.
function CanonHeader(const ARaw: RawByteString; ARelaxed, ANoCRLF: Boolean): RawByteString;
var
  vColon, vInt: IntegerRAL;
  vValue: RawByteString;
  vSpace: Boolean;
begin
  if not ARelaxed then
  begin
    Result := ARaw;
    if not ANoCRLF then
      Result := Result + #13#10;
    Exit;
  end;
  vColon := Pos(RawByteString(':'), ARaw);
  Result := LowerAscii(TrimWSP(Copy(ARaw, 1, vColon - 1))) + ':';
  vValue := '';
  vSpace := False;
  for vInt := vColon + 1 to Length(ARaw) do
  begin
    if ARaw[vInt] in [#13, #10] then
      Continue;
    if IsWSP(ARaw[vInt]) then
      vSpace := True
    else
    begin
      if vSpace and (vValue <> '') then
        vValue := vValue + ' ';
      vSpace := False;
      vValue := vValue + ARaw[vInt];
    end;
  end;
  Result := Result + vValue;
  if not ANoCRLF then
    Result := Result + #13#10;
end;

{ the signed header fields named in AList (h=), each taken from the bottom up;
  a name with no occurrence left adds nothing }
function CanonSignedHeaders(const AHeaders: TRALDKIMHeaders; AList: TStrings;
  ARelaxed: Boolean): RawByteString;
var
  vUsed: array of Boolean;
  vInt, vHead: IntegerRAL;
  vName: RawByteString;
begin
  Result := '';
  SetLength(vUsed, Length(AHeaders));
  for vInt := 0 to Length(vUsed) - 1 do
    vUsed[vInt] := False;
  for vInt := 0 to AList.Count - 1 do
  begin
    vName := LowerAscii(TrimWSP(RawByteString(AList[vInt])));
    for vHead := Length(AHeaders) - 1 downto 0 do
      if not vUsed[vHead] and (LowerAscii(AHeaders[vHead].Name) = vName) then
      begin
        vUsed[vHead] := True;
        Result := Result + CanonHeader(AHeaders[vHead].Raw, ARelaxed, False);
        Break;
      end;
  end;
end;

/// The signature field with the value of b= emptied (RFC 6376 3.7), the tag kept.
function WithoutB(const ARaw: RawByteString): RawByteString;
var
  vInt, vStart, vEq, vEnd: IntegerRAL;
  vName: RawByteString;
begin
  Result := ARaw;
  vStart := Pos(RawByteString(':'), ARaw) + 1;
  while vStart <= Length(ARaw) do
  begin
    vEnd := vStart;
    while (vEnd <= Length(ARaw)) and (ARaw[vEnd] <> ';') do
      Inc(vEnd);
    vEq := 0;
    for vInt := vStart to vEnd - 1 do
      if ARaw[vInt] = '=' then
      begin
        vEq := vInt;
        Break;
      end;
    if vEq > 0 then
    begin
      vName := StripFWS(Copy(ARaw, vStart, vEq - vStart));
      if vName = 'b' then
      begin
        Result := Copy(ARaw, 1, vEq) + Copy(ARaw, vEnd, MaxInt);
        Exit;
      end;
    end;
    vStart := vEnd + 1;
  end;
end;

function RALDKIMTag(const ATags: RawByteString; const AName: RawByteString): RawByteString;
var
  vList: TStringList;
  vInt, vEq: IntegerRAL;
  vItem: RawByteString;
begin
  Result := '';
  vList := TStringList.Create;
  try
    vList.Delimiter := ';';
    vList.StrictDelimiter := True;
    vList.DelimitedText := string(ATags);
    for vInt := 0 to vList.Count - 1 do
    begin
      vItem := RawByteString(vList[vInt]);
      vEq := Pos(RawByteString('='), vItem);
      if (vEq > 0) and (StripFWS(Copy(vItem, 1, vEq - 1)) = AName) then
      begin
        Result := TrimWSP(Copy(vItem, vEq + 1, MaxInt));
        if (AName = 'b') or (AName = 'bh') or (AName = 'p') then
          Result := StripFWS(Result);
        Exit;
      end;
    end;
  finally
    vList.Free;
  end;
end;

function RALDKIMVerdictName(AVerdict: TRALDKIMVerdict): StringRAL;
begin
  case AVerdict of
    dkvPass: Result := 'pass';
    dkvFail: Result := 'fail';
    dkvTempError: Result := 'temperror';
    dkvPolicy: Result := 'policy';
  else
    Result := 'permerror';
  end;
end;

{ TRALDKIMBodyHasher }

constructor TRALDKIMBodyHasher.Create(ARelaxed: Boolean);
begin
  inherited Create;
  FRelaxed := ARelaxed;
  FHash := TRALSHA2_32.Create;
  FHash.HashBegin;
  SetLength(FLine, 1024);
  FLineLen := 0;
end;

destructor TRALDKIMBodyHasher.Destroy;
begin
  FHash.Free;
  inherited Destroy;
end;

procedure TRALDKIMBodyHasher.Feed(const AText: RawByteString);
begin
  if AText <> '' then
    FHash.HashUpdate(@AText[1], Length(AText));
end;

procedure TRALDKIMBodyHasher.AddLine;
var
  vText: RawByteString;
  vInt, vOut: IntegerRAL;
  vSpace: Boolean;
begin
  vText := Copy(FLine, 1, FLineLen);
  FLineLen := 0;
  if FRelaxed then
  begin
    // runs of WSP become one space; WSP at the end of the line goes
    vOut := 0;
    vSpace := False;
    for vInt := 1 to Length(vText) do
      if IsWSP(vText[vInt]) then
        vSpace := True
      else
      begin
        if vSpace then
        begin
          Inc(vOut);
          vText[vOut] := ' ';
        end;
        vSpace := False;
        Inc(vOut);
        vText[vOut] := vText[vInt];
      end;
    SetLength(vText, vOut);
  end;
  if vText = '' then
  begin
    Inc(FEmpty);
    Exit;
  end;
  while FEmpty > 0 do
  begin
    Feed(#13#10);
    Dec(FEmpty);
  end;
  Feed(vText + #13#10);
  FSeenData := True;
end;

procedure TRALDKIMBodyHasher.Update(AData: PAnsiChar; ALength: IntegerRAL);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to ALength - 1 do
  begin
    if AData[vInt] = #10 then
    begin
      // CRLF ends the line; a bare LF is taken as one too
      if (FLineLen > 0) and (FLine[FLineLen] = #13) then
        Dec(FLineLen);
      AddLine;
    end
    else
    begin
      if FLineLen = Length(FLine) then
        SetLength(FLine, Length(FLine) * 2);
      Inc(FLineLen);
      FLine[FLineLen] := AData[vInt];
    end;
  end;
end;

function TRALDKIMBodyHasher.Finish: TBytes;
begin
  // a last line without CRLF counts as a line
  if FLineLen > 0 then
    AddLine;
  // simple: an empty body is one CRLF; relaxed: nothing
  if not FRelaxed and not FSeenData then
    Feed(#13#10);
  Result := FHash.HashEnd;
end;

/// The body hash of what is left in AData after ABody, which was already read.
function BodyHash(const ABody: RawByteString; AData: TStream; ARelaxed: Boolean): TBytes;
var
  vHasher: TRALDKIMBodyHasher;
  vBlock: RawByteString;
  vRead: IntegerRAL;
begin
  vHasher := TRALDKIMBodyHasher.Create(ARelaxed);
  try
    if ABody <> '' then
      vHasher.Update(PAnsiChar(ABody), Length(ABody));
    SetLength(vBlock, cReadBlock);
    repeat
      vRead := AData.Read(vBlock[1], cReadBlock);
      if vRead > 0 then
        vHasher.Update(PAnsiChar(vBlock), vRead);
    until vRead <= 0;
    Result := vHasher.Finish;
  finally
    vHasher.Free;
  end;
end;

/// The canonicalization as the c= tag writes it: simple or relaxed.
function CanonName(ACanon: TRALDKIMCanonicalization): RawByteString;
begin
  if ACanon = dkcSimple then
    Result := 'simple'
  else
    Result := 'relaxed';
end;

{ TRALDKIMSigner }

constructor TRALDKIMSigner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlgorithm := dkaRSASHA256;
  FBodyCanonicalization := dkcRelaxed;
  FHeaderCanonicalization := dkcRelaxed;
  FOverSign := True;
  FLock := TCriticalSection.Create;
  FPrivateKey := TStringList.Create;
  TStringList(FPrivateKey).OnChange := KeyChanged;
  FSignedHeaders := TStringList.Create;
  FSignedHeaders.Text := cDefaultSigned;
end;

destructor TRALDKIMSigner.Destroy;
begin
  FreeAndNil(FRSAKey);
  FPrivateKey.Free;
  FSignedHeaders.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALDKIMSigner.KeyChanged(Sender: TObject);
begin
  FreeAndNil(FRSAKey);
  SetLength(FSeed, 0);
end;

procedure TRALDKIMSigner.SetPrivateKey(AValue: TStrings);
begin
  FPrivateKey.Assign(AValue);
end;

procedure TRALDKIMSigner.SetPrivateKeyFile(const AValue: string);
begin
  FPrivateKeyFile := AValue;
  KeyChanged(nil);
end;

procedure TRALDKIMSigner.SetSignedHeaders(AValue: TStrings);
begin
  FSignedHeaders.Assign(AValue);
end;

procedure TRALDKIMSigner.ReadKey;
var
  vText: StringRAL;
  vList: TStringList;
  vDER: TBytes;
  vInt: IntegerRAL;
begin
  vText := StringRAL(Trim(FPrivateKey.Text));
  if (vText = '') and (FPrivateKeyFile <> '') then
  begin
    vList := TStringList.Create;
    try
      vList.LoadFromFile(FPrivateKeyFile);
      vText := StringRAL(Trim(vList.Text));
    finally
      vList.Free;
    end;
  end;
  if vText = '' then
    raise ERALDKIMError.Create(emDKIMNoKey);
  if FAlgorithm = dkaRSASHA256 then
    FRSAKey := TRALRSAKey.FromPEM(vText)
  else if Pos('-----BEGIN', string(vText)) > 0 then
  begin
    // PKCS#8 of Ed25519 (RFC 8410): the seed is the last 32 bytes (nested OCTET STRING)
    vList := TStringList.Create;
    try
      vList.Text := string(vText);
      vText := '';
      for vInt := 0 to vList.Count - 1 do
        if Pos('-----', vList[vInt]) <> 1 then
          vText := vText + StringRAL(Trim(vList[vInt]));
    finally
      vList.Free;
    end;
    vDER := TRALBase64.DecodeAsBytes(vText);
    if (Length(vDER) <> 48) or (vDER[Length(vDER) - 34] <> $04) or (vDER[Length(vDER) - 33] <> 32) then
      raise ERALDKIMError.Create(emDKIMNotEd25519);
    FSeed := Copy(vDER, Length(vDER) - 32, 32);
  end
  else
  begin
    FSeed := TRALBase64.DecodeAsBytes(vText);
    if Length(FSeed) <> 32 then
      raise ERALDKIMError.Create(emDKIMSeedSize);
  end;
end;

procedure TRALDKIMSigner.LoadKey;
begin
  // the queue signs from several threads: the key is read once, under the lock
  FLock.Acquire;
  try
    if (FRSAKey = nil) and (Length(FSeed) <> 32) then
      ReadKey;
  finally
    FLock.Release;
  end;
end;

function TRALDKIMSigner.KeyRecord: StringRAL;
begin
  LoadKey;
  if FAlgorithm = dkaRSASHA256 then
    Result := 'v=DKIM1; k=rsa; p=' + TRALBase64.Encode(FRSAKey.PublicKeyInfo)
  else
    Result := 'v=DKIM1; k=ed25519; p=' + TRALBase64.Encode(RALEd25519PublicKey(FSeed));
end;

function TRALDKIMSigner.Sign(AData: TStream): RawByteString;
var
  vStart, vTime: Int64;
  vHeaders: TRALDKIMHeaders;
  vBody, vField, vSigned, vSig, vLine, vTag: RawByteString;
  vNames: TStringList;
  vInt, vHead, vCount: IntegerRAL;
  vBH, vSignature: TBytes;
  vRelaxed: Boolean;
  vTags: array of RawByteString;
begin
  if (FDomain = '') or (FSelector = '') then
    raise ERALDKIMError.Create(emDKIMNoDomain);
  LoadKey;
  vStart := AData.Position;
  vNames := TStringList.Create;
  try
    ReadHeaders(AData, vHeaders, vBody);
    vBH := BodyHash(vBody, AData, FBodyCanonicalization = dkcRelaxed);
    AData.Position := vStart;

    // h=: each configured name as many times as it occurs, plus one when oversigning
    for vInt := 0 to FSignedHeaders.Count - 1 do
    begin
      vField := TrimWSP(RawByteString(FSignedHeaders[vInt]));
      if vField = '' then
        Continue;
      vCount := 0;
      for vHead := 0 to Length(vHeaders) - 1 do
        if LowerAscii(vHeaders[vHead].Name) = LowerAscii(vField) then
          Inc(vCount);
      if (vCount = 0) and (LowerAscii(vField) = 'from') then
        raise ERALDKIMError.Create(emDKIMNoFrom);
      if FOverSign and (vCount > 0) then
        Inc(vCount);
      for vHead := 1 to vCount do
        vNames.Add(string(LowerAscii(vField)));
    end;

    if FSignTime > 0 then
      vTime := FSignTime
    else
      vTime := (RALJobNowMs div 1000);

    // the tags, folded at about 76 columns by WriteField below
    SetLength(vTags, 0);
    vCount := 0;
    SetLength(vTags, 12);
    vTags[vCount] := 'v=1'; Inc(vCount);
    if FAlgorithm = dkaRSASHA256 then
      vTags[vCount] := 'a=rsa-sha256'
    else
      vTags[vCount] := 'a=ed25519-sha256';
    Inc(vCount);
    vTags[vCount] := 'c=' + CanonName(FHeaderCanonicalization) + '/' +
      CanonName(FBodyCanonicalization); Inc(vCount);
    vTags[vCount] := 'd=' + RawByteString(FDomain); Inc(vCount);
    if FIdentity <> '' then
    begin
      vTags[vCount] := 'i=' + RawByteString(FIdentity); Inc(vCount);
    end;
    vTags[vCount] := 's=' + RawByteString(FSelector); Inc(vCount);
    vTags[vCount] := 't=' + RawByteString(IntToStr(vTime)); Inc(vCount);
    if FExpireAfter > 0 then
    begin
      vTags[vCount] := 'x=' + RawByteString(IntToStr(vTime + FExpireAfter)); Inc(vCount);
    end;
    vTag := 'h=';
    for vInt := 0 to vNames.Count - 1 do
    begin
      if vInt > 0 then
        vTag := vTag + ':';
      vTag := vTag + RawByteString(vNames[vInt]);
    end;
    vTags[vCount] := vTag; Inc(vCount);
    vTags[vCount] := 'bh=' + Base64Raw(vBH); Inc(vCount);

    vField := 'DKIM-Signature:';
    vLine := vField;
    for vInt := 0 to vCount - 1 do
    begin
      vTag := ' ' + vTags[vInt] + ';';
      if Length(vLine) + Length(vTag) > 76 then
      begin
        vField := vField + #13#10;
        vLine := '';
      end;
      vField := vField + vTag;
      vLine := vLine + vTag;
    end;
    vField := vField + #13#10' b=';

    vRelaxed := FHeaderCanonicalization = dkcRelaxed;
    vSigned := CanonSignedHeaders(vHeaders, vNames, vRelaxed) +
      CanonHeader(vField, vRelaxed, True);
    if FAlgorithm = dkaRSASHA256 then
      vSignature := FRSAKey.Sign(BytesOfRaw(vSigned))
    else
      vSignature := RALEd25519Sign(SHA256Raw(vSigned), FSeed);

    // b= folded in lines of 72 characters
    vSig := Base64Raw(vSignature);
    vInt := 1;
    while vInt <= Length(vSig) do
    begin
      if vInt > 1 then
        vField := vField + #13#10'  ';
      vField := vField + Copy(vSig, vInt, 72);
      Inc(vInt, 72);
    end;
    Result := vField + #13#10;
  finally
    vNames.Free;
  end;
end;

procedure TRALDKIMSigner.SignTo(AData, AOutput: TStream);
var
  vHeader: RawByteString;
begin
  vHeader := Sign(AData);
  AOutput.WriteBuffer(vHeader[1], Length(vHeader));
  AOutput.CopyFrom(AData, AData.Size - AData.Position);
end;

{ TRALDKIMVerifier }

constructor TRALDKIMVerifier.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxSignatures := 5;
end;

function TRALDKIMVerifier.Verify(AData: TStream; AResults: TList): Boolean;
var
  vStart: Int64;
  vHeaders: TRALDKIMHeaders;
  vBody, vValue, vAlg, vCanon, vRecord, vKeyType, vSigned, vH: RawByteString;
  vInt, vSlash, vCount: IntegerRAL;
  vResult: TRALDKIMResult;
  vBHRelaxed, vBHSimple, vBH, vSignature, vPublic: TBytes;
  vBodyRelaxed, vHeadRelaxed, vTemporary, vHaveRelaxed, vHaveSimple: Boolean;
  vNames: TStringList;
  vRecordText: StringRAL;
  vRSA: TRALRSAKey;
begin
  Result := False;
  vRecordText := '';
  vStart := AData.Position;
  vNames := TStringList.Create;
  try
    ReadHeaders(AData, vHeaders, vBody);
    vHaveRelaxed := False;
    vHaveSimple := False;
    vCount := 0;
    for vInt := 0 to Length(vHeaders) - 1 do
    begin
      if LowerAscii(vHeaders[vInt].Name) <> 'dkim-signature' then
        Continue;
      if vCount >= FMaxSignatures then
        Break;
      Inc(vCount);
      vResult := TRALDKIMResult.Create;
      AResults.Add(vResult);
      vResult.Verdict := dkvPermError;
      vValue := Copy(vHeaders[vInt].Raw, Pos(RawByteString(':'), vHeaders[vInt].Raw) + 1, MaxInt);
      vResult.Domain := StringRAL(RALDKIMTag(vValue, 'd'));
      vResult.Selector := StringRAL(RALDKIMTag(vValue, 's'));
      vResult.Identity := StringRAL(RALDKIMTag(vValue, 'i'));
      vAlg := LowerAscii(RALDKIMTag(vValue, 'a'));
      vResult.Algorithm := StringRAL(vAlg);
      if RALDKIMTag(vValue, 'v') <> '1' then
      begin
        vResult.Reason := 'unknown version';
        Continue;
      end;
      if (vAlg <> 'rsa-sha256') and (vAlg <> 'ed25519-sha256') then
      begin
        vResult.Reason := 'unsupported algorithm';
        Continue;
      end;
      if (vResult.Domain = '') or (vResult.Selector = '') or
         (RALDKIMTag(vValue, 'b') = '') or (RALDKIMTag(vValue, 'bh') = '') then
      begin
        vResult.Reason := 'missing tag';
        Continue;
      end;
      if RALDKIMTag(vValue, 'l') <> '' then
      begin
        // a partial body hash lets anyone append content: refused
        vResult.Reason := 'l= not accepted';
        Continue;
      end;
      vCanon := LowerAscii(RALDKIMTag(vValue, 'c'));
      if vCanon = '' then
        vCanon := 'simple/simple';
      vSlash := Pos(RawByteString('/'), vCanon);
      if vSlash = 0 then
        vCanon := vCanon + '/simple';
      vSlash := Pos(RawByteString('/'), vCanon);
      vHeadRelaxed := Copy(vCanon, 1, vSlash - 1) = 'relaxed';
      vBodyRelaxed := Copy(vCanon, vSlash + 1, MaxInt) = 'relaxed';
      if RALDKIMTag(vValue, 'x') <> '' then
        if StrToInt64Def(string(RALDKIMTag(vValue, 'x')), 0) < (RALJobNowMs div 1000) then
        begin
          vResult.Verdict := dkvFail;
          vResult.Reason := 'signature expired';
          Continue;
        end;

      // the key
      vRecordText := '';
      vTemporary := False;
      if Assigned(FOnKeyLookup) then
        FOnKeyLookup(Self, vResult.Selector, vResult.Domain, vRecordText, vTemporary);
      if vRecordText = '' then
      begin
        if vTemporary then
        begin
          vResult.Verdict := dkvTempError;
          vResult.Reason := 'key lookup failed';
        end
        else
          vResult.Reason := 'no key';
        Continue;
      end;
      vRecord := RawByteString(vRecordText);
      vKeyType := LowerAscii(RALDKIMTag(vRecord, 'k'));
      if vKeyType = '' then
        vKeyType := 'rsa';
      if RALDKIMTag(vRecord, 'p') = '' then
      begin
        vResult.Verdict := dkvPolicy;
        vResult.Reason := 'key revoked';
        Continue;
      end;
      if Copy(vAlg, 1, Length(vKeyType)) <> vKeyType then
      begin
        vResult.Reason := 'key type does not match';
        Continue;
      end;

      // the body hash, once per canonicalization
      if vBodyRelaxed and not vHaveRelaxed then
      begin
        AData.Position := vStart;
        ReadHeaders(AData, vHeaders, vBody);
        vBHRelaxed := BodyHash(vBody, AData, True);
        vHaveRelaxed := True;
      end
      else if not vBodyRelaxed and not vHaveSimple then
      begin
        AData.Position := vStart;
        ReadHeaders(AData, vHeaders, vBody);
        vBHSimple := BodyHash(vBody, AData, False);
        vHaveSimple := True;
      end;
      if vBodyRelaxed then
        vBH := vBHRelaxed
      else
        vBH := vBHSimple;
      if Base64Raw(vBH) <> RALDKIMTag(vValue, 'bh') then
      begin
        vResult.Verdict := dkvFail;
        vResult.Reason := 'body hash did not verify';
        Continue;
      end;

      vH := RALDKIMTag(vValue, 'h');
      vNames.Delimiter := ':';
      vNames.StrictDelimiter := True;
      vNames.DelimitedText := string(StripFWS(vH));
      if vNames.IndexOf('from') < 0 then
      begin
        vResult.Reason := 'From not signed';
        for vSlash := 0 to vNames.Count - 1 do
          if SameText(vNames[vSlash], 'from') then
            vResult.Reason := '';
        if vResult.Reason <> '' then
          Continue;
      end;
      vSigned := CanonSignedHeaders(vHeaders, vNames, vHeadRelaxed) +
        CanonHeader(WithoutB(vHeaders[vInt].Raw), vHeadRelaxed, True);
      vSignature := TRALBase64.DecodeAsBytes(StringRAL(RALDKIMTag(vValue, 'b')));
      vPublic := TRALBase64.DecodeAsBytes(StringRAL(RALDKIMTag(vRecord, 'p')));
      try
        if vAlg = 'rsa-sha256' then
        begin
          try
            vRSA := TRALRSAKey.FromPublicKeyInfo(vPublic);
          except
            vRSA := nil;
          end;
          if vRSA = nil then
          begin
            vResult.Reason := 'bad key';
            Continue;
          end;
          try
            if vRSA.Verify(BytesOfRaw(vSigned), vSignature) then
              vResult.Verdict := dkvPass
            else
            begin
              vResult.Verdict := dkvFail;
              vResult.Reason := 'signature did not verify';
            end;
          finally
            vRSA.Free;
          end;
        end
        else if Length(vPublic) <> 32 then
          vResult.Reason := 'bad key'
        else if RALEd25519Verify(SHA256Raw(vSigned), vSignature, vPublic) then
          vResult.Verdict := dkvPass
        else
        begin
          vResult.Verdict := dkvFail;
          vResult.Reason := 'signature did not verify';
        end;
      except
        on E: Exception do
        begin
          vResult.Verdict := dkvPermError;
          vResult.Reason := StringRAL(E.Message);
        end;
      end;
      if vResult.Verdict = dkvPass then
        Result := True;
    end;
  finally
    vNames.Free;
    AData.Position := vStart;
  end;
end;

end.
