/// A DNS stub resolver (RFC 1035) over RAL's own sockets: A, AAAA, MX, TXT and PTR.
unit RALDNS;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALCustomObjects, RALExceptions, RALSocket;

type
  /// The record types the resolver asks for.
  TRALDNSType = (
    /// An IPv4 address.
    dtA,
    /// An IPv6 address.
    dtAAAA,
    /// A mail exchanger: Preference and Data (the host name).
    dtMX,
    /// A name of a reverse lookup.
    dtPTR,
    /// Text; the strings of one record are joined.
    dtTXT
  );

  /// The response code of an answer, or why there is none.
  TRALDNSRCode = (
    /// The query worked; the answer may still be empty.
    drNoError,
    /// The query or the answer is malformed (also: an invalid name).
    drFormErr,
    /// The server failed.
    drServFail,
    /// The name does not exist.
    drNXDomain,
    /// The server does not support the query.
    drNotImp,
    /// The server refused the query.
    drRefused,
    /// Any other response code.
    drOther,
    /// No server answered in time.
    drTimeout
  );

  /// One record of an answer.
  TRALDNSRecord = record
    /// The owner name.
    Name: StringRAL;
    /// The type.
    Kind: TRALDNSType;
    /// Seconds the record may be kept.
    TTL: Cardinal;
    /// MX preference (lower is preferred); 0 for the other types.
    Preference: IntegerRAL;
    { The data: the address as text (A, AAAA), the host name (MX, PTR) or the
      text (TXT, the strings of the record joined). }
    Data: StringRAL;
  end;

  /// The records of an answer.
  TRALDNSRecords = array of TRALDNSRecord;

  { What a query found. A name that exists with no record of the type gives
    drNoError and no records; Temporary tells a failure worth retrying later
    (no answer, SERVFAIL, REFUSED...) from a definitive one. }
  TRALDNSResult = record
    /// Why there is no answer, when Temporary; empty otherwise.
    Error: string;
    /// The response code.
    RCode: TRALDNSRCode;
    /// The records of the type asked (CNAMEs are followed by the server).
    Records: TRALDNSRecords;
    /// The query failed in a way that may pass: a timeout, SERVFAIL, REFUSED.
    Temporary: Boolean;
  end;

  /// An IP address as bytes: 4 used for IPv4, 16 for IPv6.
  TRALIPBytes = array[0..15] of Byte;

  /// Raised for an invalid IP address given to the helpers of the unit.
  ERALDNSError = class(ERALException);

  { A stub resolver: asks the servers of Servers (the system's when empty) over
    UDP, repeats over TCP when the answer is truncated, and keeps the answers
    until their TTL. Thread-safe. Query is virtual: a test replaces it with a
    zone in memory and no socket. }
  TRALDNSResolver = class(TRALComponent)
  private
    /// The answers kept, by type and name.
    FCache: TStringList;
    /// Guards the cache and the system server list.
    FLock: TCriticalSection;
    FRetries: IntegerRAL;
    FServers: TStrings;
    /// The servers of the system, read once.
    FSystemServers: TStringList;
    FTimeout: IntegerRAL;
    FUseCache: Boolean;

    /// Asks one server for the records; AAnswered tells whether a valid answer came.
    function Ask(const AServer: StringRAL; const AName: StringRAL; AType: TRALDNSType;
      var AResult: TRALDNSResult; out AAnswered: Boolean): Boolean;
    /// Looks for a live answer in the cache.
    function CacheFind(const AKey: string; out AResult: TRALDNSResult): Boolean;
    /// Keeps an answer until its TTL.
    procedure CacheStore(const AKey: string; const AResult: TRALDNSResult);
    /// Appends the servers to ask: Servers, or the system ones.
    procedure LoadServers(AList: TStrings);
    procedure SetServers(AValue: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Forgets every answer kept.
    procedure ClearCache;
    { Asks for the records of AType at AName (a trailing dot is ignored). Never
      raises for a DNS failure: the result says what happened. }
    function Query(const AName: StringRAL; AType: TRALDNSType): TRALDNSResult; virtual;
    { Appends the name servers of the system to AList: /etc/resolv.conf on Unix,
      GetNetworkParams on Windows; 8.8.8.8 when the system gives none. }
    class procedure SystemServers(AList: TStrings);
  published
    { Rounds over the servers after the first without an answer; 0 = each server
      is asked once. }
    property Retries: IntegerRAL read FRetries write FRetries default 1;
    /// The name servers (IPv4 or IPv6 text), tried in order; empty = the system's.
    property Servers: TStrings read FServers write SetServers;
    /// Milliseconds to wait for one server's answer.
    property Timeout: IntegerRAL read FTimeout write FTimeout default 2000;
    /// Keeps answers until their TTL; failures are never kept.
    property UseCache: Boolean read FUseCache write FUseCache default True;
  end;

{ Reads an IPv4 (no leading zeros) or IPv6 (with ::, and a dotted IPv4 tail)
  address into AAddress: 4 or 16, the bytes used; 0 when AText is not an address. }
function RALDNSParseIP(const AText: StringRAL; out AAddress: TRALIPBytes): IntegerRAL;
{ The text of an address of ALength (4 or 16) bytes: dotted decimal, or IPv6 with
  :: for the longest run of zeros. }
function RALDNSIPText(const AAddress: TRALIPBytes; ALength: IntegerRAL): StringRAL;
{ The name of the PTR lookup of AIP: d.c.b.a.in-addr.arpa, or the nibbles under
  ip6.arpa. Raises ERALDNSError when AIP is not an address. }
function RALDNSReverseName(const AIP: StringRAL): StringRAL;

implementation

type
  /// Raised inside the unit for a malformed response.
  EDNSWire = class(Exception);

const
  /// Wire type of an A record.
  cWireA = 1;
  /// Wire type of a CNAME record.
  cWireCNAME = 5;
  /// Wire type of a PTR record.
  cWirePTR = 12;
  /// Wire type of an MX record.
  cWireMX = 15;
  /// Wire type of a TXT record.
  cWireTXT = 16;
  /// Wire type of an AAAA record.
  cWireAAAA = 28;
  /// Longest time in seconds an answer is kept.
  cMaxTTL = 3600;
  /// Seconds an empty or NXDOMAIN answer is kept.
  cNegativeTTL = 60;
  /// Answers kept before the cache is trimmed.
  cMaxCache = 2000;
  /// The server used when the system gives none.
  cFallbackServer = '8.8.8.8';

type
  /// An answer in the cache.
  TCacheEntry = class
  public
    /// Clock value after which the answer is stale.
    Expires: Int64RAL;
    /// The answer.
    Result: TRALDNSResult;
  end;

// ---- text helpers ----

/// Lowercases the ASCII letters of a name.
function LowerName(const AText: StringRAL): StringRAL;
var
  vInt: Integer;
begin
  Result := AText;
  SetLength(Result, Length(AText));
  for vInt := 1 to Length(Result) do
    if Result[vInt] in ['A'..'Z'] then
      Result[vInt] := AnsiChar(Ord(Result[vInt]) + 32);
end;

/// The position of AChar in AText from AStart; 0 when absent.
function IndexOfChar(const AText: StringRAL; AChar: AnsiChar; AStart: Integer): Integer;
var
  vInt: Integer;
begin
  for vInt := AStart to Length(AText) do
    if AText[vInt] = AChar then
    begin
      Result := vInt;
      Exit;
    end;
  Result := 0;
end;

/// The wire type number of a record type.
function WireOf(AType: TRALDNSType): Word;
begin
  case AType of
    dtA: Result := cWireA;
    dtAAAA: Result := cWireAAAA;
    dtMX: Result := cWireMX;
    dtPTR: Result := cWirePTR;
  else
    Result := cWireTXT;
  end;
end;

/// Sets AResult to an empty NOERROR answer.
procedure ClearResult(var AResult: TRALDNSResult);
begin
  AResult.Error := '';
  AResult.RCode := drNoError;
  AResult.Records := nil;
  AResult.Temporary := False;
end;

// ---- addresses ----

/// Reads a 0..255 decimal without leading zeros.
function ParseDecimal(const AText: StringRAL; out AValue: Integer): Boolean;
var
  vInt: Integer;
begin
  Result := (Length(AText) >= 1) and (Length(AText) <= 3);
  if not Result then
    Exit;
  AValue := 0;
  for vInt := 1 to Length(AText) do
  begin
    if not (AText[vInt] in ['0'..'9']) then
    begin
      Result := False;
      Exit;
    end;
    AValue := AValue * 10 + Ord(AText[vInt]) - 48;
  end;
  // no leading zero (RFC 7208 5.6)
  Result := (AValue <= 255) and ((Length(AText) = 1) or (AText[1] <> '0'));
end;

/// Reads a dotted IPv4 address into AAddress at AOffset.
function ParseIPv4(const AText: StringRAL; var AAddress: TRALIPBytes; AOffset: Integer): Boolean;
var
  vInt, vStart, vPart, vValue: Integer;
begin
  Result := False;
  vStart := 1;
  vPart := 0;
  for vInt := 1 to Length(AText) + 1 do
    if (vInt > Length(AText)) or (AText[vInt] = '.') then
    begin
      if vPart > 3 then
        Exit;
      if not ParseDecimal(Copy(AText, vStart, vInt - vStart), vValue) then
        Exit;
      AAddress[AOffset + vPart] := vValue;
      Inc(vPart);
      vStart := vInt + 1;
    end;
  Result := vPart = 4;
end;

/// Reads colon separated groups into AGroups from AIndex; the last may be a dotted IPv4.
function ParseGroups(const AText: StringRAL; var AGroups: array of Word;
  var ACount: Integer): Boolean;
var
  vInt, vStart, vValue, vLen, vDigit: Integer;
  vPiece: StringRAL;
  vIP: TRALIPBytes;
begin
  Result := False;
  ACount := 0;
  if AText = '' then
  begin
    Result := True;
    Exit;
  end;
  vStart := 1;
  for vInt := 1 to Length(AText) + 1 do
    if (vInt > Length(AText)) or (AText[vInt] = ':') then
    begin
      vPiece := Copy(AText, vStart, vInt - vStart);
      vStart := vInt + 1;
      if vPiece = '' then
        Exit;
      if IndexOfChar(vPiece, '.', 1) > 0 then
      begin
        if (vInt <= Length(AText)) or (ACount > 6) then
          Exit;
        if not ParseIPv4(vPiece, vIP, 0) then
          Exit;
        AGroups[ACount] := vIP[0] * 256 + vIP[1];
        AGroups[ACount + 1] := vIP[2] * 256 + vIP[3];
        Inc(ACount, 2);
      end
      else
      begin
        vLen := Length(vPiece);
        if (vLen > 4) or (ACount > 7) then
          Exit;
        vValue := 0;
        for vDigit := 1 to vLen do
          case vPiece[vDigit] of
            '0'..'9': vValue := vValue * 16 + Ord(vPiece[vDigit]) - 48;
            'a'..'f': vValue := vValue * 16 + Ord(vPiece[vDigit]) - 87;
            'A'..'F': vValue := vValue * 16 + Ord(vPiece[vDigit]) - 55;
          else
            Exit;
          end;
        AGroups[ACount] := vValue;
        Inc(ACount);
      end;
    end;
  Result := True;
end;

/// Reads an IPv6 address into AAddress.
function ParseIPv6(const AText: StringRAL; var AAddress: TRALIPBytes): Boolean;
var
  vLeft, vRight: array[0..7] of Word;
  vNLeft, vNRight, vPos, vInt: Integer;
  vGroups: array[0..7] of Word;
begin
  Result := False;
  vPos := 0;
  for vInt := 1 to Length(AText) - 1 do
    if (AText[vInt] = ':') and (AText[vInt + 1] = ':') then
    begin
      vPos := vInt;
      Break;
    end;
  if vPos > 0 then
  begin
    if not ParseGroups(Copy(AText, 1, vPos - 1), vLeft, vNLeft) then
      Exit;
    if not ParseGroups(Copy(AText, vPos + 2, Length(AText)), vRight, vNRight) then
      Exit;
    if vNLeft + vNRight > 7 then
      Exit;
    for vInt := 0 to 7 do
      vGroups[vInt] := 0;
    for vInt := 0 to vNLeft - 1 do
      vGroups[vInt] := vLeft[vInt];
    for vInt := 0 to vNRight - 1 do
      vGroups[8 - vNRight + vInt] := vRight[vInt];
  end
  else
  begin
    if not ParseGroups(AText, vGroups, vNLeft) then
      Exit;
    if vNLeft <> 8 then
      Exit;
  end;
  for vInt := 0 to 7 do
  begin
    AAddress[vInt * 2] := vGroups[vInt] shr 8;
    AAddress[vInt * 2 + 1] := vGroups[vInt] and $FF;
  end;
  Result := True;
end;

function RALDNSParseIP(const AText: StringRAL; out AAddress: TRALIPBytes): IntegerRAL;
var
  vInt: Integer;
begin
  for vInt := 0 to 15 do
    AAddress[vInt] := 0;
  Result := 0;
  if AText = '' then
    Exit;
  if IndexOfChar(AText, ':', 1) > 0 then
  begin
    if ParseIPv6(AText, AAddress) then
      Result := 16;
  end
  else if ParseIPv4(AText, AAddress, 0) then
    Result := 4;
end;

function RALDNSIPText(const AAddress: TRALIPBytes; ALength: IntegerRAL): StringRAL;
var
  vInt, vBest, vBestLen, vRun, vRunLen: Integer;
  vGroups: array[0..7] of Word;
begin
  Result := '';
  if ALength = 4 then
  begin
    Result := StringRAL(Format('%d.%d.%d.%d', [AAddress[0], AAddress[1], AAddress[2], AAddress[3]]));
    Exit;
  end;
  for vInt := 0 to 7 do
    vGroups[vInt] := AAddress[vInt * 2] * 256 + AAddress[vInt * 2 + 1];
  vBest := -1;
  vBestLen := 1;
  vRun := -1;
  vRunLen := 0;
  for vInt := 0 to 7 do
    if vGroups[vInt] = 0 then
    begin
      if vRun < 0 then
      begin
        vRun := vInt;
        vRunLen := 0;
      end;
      Inc(vRunLen);
      if vRunLen > vBestLen then
      begin
        vBest := vRun;
        vBestLen := vRunLen;
      end;
    end
    else
      vRun := -1;
  vInt := 0;
  while vInt < 8 do
  begin
    if vInt = vBest then
    begin
      Result := Result + '::';
      Inc(vInt, vBestLen);
      Continue;
    end;
    if (Result <> '') and (Result[Length(Result)] <> ':') then
      Result := Result + ':';
    Result := Result + StringRAL(LowerCase(IntToHex(vGroups[vInt], 1)));
    Inc(vInt);
  end;
end;

function RALDNSReverseName(const AIP: StringRAL): StringRAL;
var
  vAddr: TRALIPBytes;
  vLen, vInt: Integer;
begin
  vLen := RALDNSParseIP(AIP, vAddr);
  if vLen = 0 then
    raise ERALDNSError.CreateFmt(emDNSBadAddress, [string(AIP)]);
  Result := '';
  if vLen = 4 then
  begin
    for vInt := 3 downto 0 do
      Result := Result + StringRAL(IntToStr(vAddr[vInt])) + '.';
    Result := Result + 'in-addr.arpa';
  end
  else
  begin
    for vInt := 15 downto 0 do
      Result := Result + StringRAL(LowerCase(IntToHex(vAddr[vInt] and $0F, 1) +
        '.' + IntToHex(vAddr[vInt] shr 4, 1) + '.'));
    Result := Result + 'ip6.arpa';
  end;
end;

// ---- the wire format ----

/// Builds the query message for AName; False when the name is invalid.
function BuildQuery(AID: Word; const AName: StringRAL; AQType: Word; out AMsg: TBytes): Boolean;
var
  vPos, vStart, vLen, vInt, vCount: Integer;
begin
  Result := False;
  AMsg := nil;
  vLen := Length(AName);
  if (vLen = 0) or (vLen > 253) then
    Exit;
  SetLength(AMsg, 12 + vLen + 2 + 4);
  FillChar(AMsg[0], Length(AMsg), 0);
  AMsg[0] := AID shr 8;
  AMsg[1] := AID and $FF;
  AMsg[2] := 1; // RD
  AMsg[5] := 1; // QDCOUNT
  vPos := 12;
  vStart := 1;
  for vInt := 1 to vLen + 1 do
    if (vInt > vLen) or (AName[vInt] = '.') then
    begin
      vCount := vInt - vStart;
      if (vCount < 1) or (vCount > 63) then
      begin
        AMsg := nil;
        Exit;
      end;
      AMsg[vPos] := vCount;
      Inc(vPos);
      Move(AName[vStart], AMsg[vPos], vCount);
      Inc(vPos, vCount);
      vStart := vInt + 1;
    end;
  AMsg[vPos] := 0;
  Inc(vPos);
  AMsg[vPos] := AQType shr 8;
  AMsg[vPos + 1] := AQType and $FF;
  AMsg[vPos + 3] := 1; // class IN
  Result := True;
end;

/// Reads a big-endian 16 bit value; raises on a short message.
function ReadU16(const AMsg: TBytes; ALen, APos: Integer): Integer;
begin
  if APos + 2 > ALen then
    raise EDNSWire.Create(emDNSBadResponse);
  Result := AMsg[APos] * 256 + AMsg[APos + 1];
end;

/// Reads a name at APos (with compression pointers) and leaves APos after it.
function ReadName(const AMsg: TBytes; ALen: Integer; var APos: Integer): StringRAL;
var
  vPos, vEnd, vLabel, vJumps, vInt: Integer;
  vJumped: Boolean;
begin
  Result := '';
  vPos := APos;
  vEnd := APos;
  vJumped := False;
  vJumps := 0;
  while True do
  begin
    if vPos >= ALen then
      raise EDNSWire.Create(emDNSBadResponse);
    vLabel := AMsg[vPos];
    if vLabel = 0 then
    begin
      if not vJumped then
        vEnd := vPos + 1;
      Break;
    end;
    if (vLabel and $C0) = $C0 then
    begin
      if vPos + 1 >= ALen then
        raise EDNSWire.Create(emDNSBadResponse);
      if not vJumped then
        vEnd := vPos + 2;
      vJumped := True;
      Inc(vJumps);
      if vJumps > 32 then
        raise EDNSWire.Create(emDNSBadResponse);
      vPos := (vLabel and $3F) * 256 + AMsg[vPos + 1];
      Continue;
    end;
    if (vLabel and $C0) <> 0 then
      raise EDNSWire.Create(emDNSBadResponse);
    if (vPos + 1 + vLabel > ALen) or (Length(Result) + vLabel > 253) then
      raise EDNSWire.Create(emDNSBadResponse);
    if Result <> '' then
      Result := Result + '.';
    vInt := Length(Result);
    SetLength(Result, vInt + vLabel);
    Move(AMsg[vPos + 1], Result[vInt + 1], vLabel);
    Inc(vPos, vLabel + 1);
  end;
  APos := vEnd;
end;

/// The response code enum of a wire value.
function ResultFromRCode(ACode: Integer): TRALDNSRCode;
begin
  case ACode of
    0: Result := drNoError;
    1: Result := drFormErr;
    2: Result := drServFail;
    3: Result := drNXDomain;
    4: Result := drNotImp;
    5: Result := drRefused;
  else
    Result := drOther;
  end;
end;

{ Reads the records of the type asked from a response; False when it is not
  the answer to this query (id, question). }
function ParseResponse(const AMsg: TBytes; ALen: Integer; AID: Word;
  const AName: StringRAL; AQType: Word; var AResult: TRALDNSResult;
  out ATruncated: Boolean): Boolean;
var
  vFlags, vQD, vAN, vInt, vPos, vType, vClass, vRDLen, vRDEnd, vCount, vStr, vLen: Integer;
  vTTL: Cardinal;
  vOwner, vText: StringRAL;
  vRec: TRALDNSRecord;
  vIP: TRALIPBytes;
begin
  Result := False;
  ATruncated := False;
  try
    if ALen < 12 then
      Exit;
    if ReadU16(AMsg, ALen, 0) <> AID then
      Exit;
    vFlags := ReadU16(AMsg, ALen, 2);
    if ((vFlags and $8000) = 0) or (((vFlags shr 11) and $F) <> 0) then
      Exit;
    vQD := ReadU16(AMsg, ALen, 4);
    vAN := ReadU16(AMsg, ALen, 6);
    vPos := 12;
    if vQD <> 1 then
      Exit;
    vOwner := ReadName(AMsg, ALen, vPos);
    if (LowerName(vOwner) <> LowerName(AName)) or (ReadU16(AMsg, ALen, vPos) <> AQType) then
      Exit;
    Inc(vPos, 4);
    Result := True;
    ATruncated := (vFlags and $0200) <> 0;
    AResult.RCode := ResultFromRCode(vFlags and $F);
    if AResult.RCode <> drNoError then
      Exit;
    vCount := 0;
    SetLength(AResult.Records, vAN);
    for vInt := 1 to vAN do
    begin
      vOwner := ReadName(AMsg, ALen, vPos);
      vType := ReadU16(AMsg, ALen, vPos);
      vClass := ReadU16(AMsg, ALen, vPos + 2);
      vTTL := Cardinal(ReadU16(AMsg, ALen, vPos + 4)) shl 16 or Cardinal(ReadU16(AMsg, ALen, vPos + 6));
      vRDLen := ReadU16(AMsg, ALen, vPos + 8);
      Inc(vPos, 10);
      vRDEnd := vPos + vRDLen;
      if vRDEnd > ALen then
        raise EDNSWire.Create(emDNSBadResponse);
      if (vClass = 1) and (vType = AQType) then
      begin
        vRec.Name := vOwner;
        vRec.TTL := vTTL;
        vRec.Preference := 0;
        vRec.Data := '';
        case vType of
          cWireA:
          begin
            if vRDLen <> 4 then
              raise EDNSWire.Create(emDNSBadResponse);
            for vStr := 0 to 3 do
              vIP[vStr] := AMsg[vPos + vStr];
            vRec.Kind := dtA;
            vRec.Data := RALDNSIPText(vIP, 4);
          end;
          cWireAAAA:
          begin
            if vRDLen <> 16 then
              raise EDNSWire.Create(emDNSBadResponse);
            for vStr := 0 to 15 do
              vIP[vStr] := AMsg[vPos + vStr];
            vRec.Kind := dtAAAA;
            vRec.Data := RALDNSIPText(vIP, 16);
          end;
          cWireMX:
          begin
            vRec.Kind := dtMX;
            vRec.Preference := ReadU16(AMsg, ALen, vPos);
            vStr := vPos + 2;
            vRec.Data := ReadName(AMsg, ALen, vStr);
          end;
          cWirePTR:
          begin
            vRec.Kind := dtPTR;
            vStr := vPos;
            vRec.Data := ReadName(AMsg, ALen, vStr);
          end;
        else
          begin
            vRec.Kind := dtTXT;
            vText := '';
            vStr := vPos;
            while vStr < vRDEnd do
            begin
              vLen := AMsg[vStr];
              if vStr + 1 + vLen > vRDEnd then
                raise EDNSWire.Create(emDNSBadResponse);
              if vLen > 0 then
              begin
                SetLength(vText, Length(vText) + vLen);
                Move(AMsg[vStr + 1], vText[Length(vText) - vLen + 1], vLen);
              end;
              Inc(vStr, vLen + 1);
            end;
            vRec.Data := vText;
          end;
        end;
        AResult.Records[vCount] := vRec;
        Inc(vCount);
      end;
      vPos := vRDEnd;
    end;
    SetLength(AResult.Records, vCount);
  except
    on EDNSWire do
    begin
      AResult.Records := nil;
      Result := False;
    end;
  end;
end;

// ---- the transports ----

/// The IP family of a server address text.
function FamilyOf(const AServer: StringRAL): TRALIPFamily;
begin
  if IndexOfChar(AServer, ':', 1) > 0 then
    Result := mifIPv6
  else
    Result := mifIPv4;
end;

/// One datagram exchange: False when nothing valid came before ATimeout.
function ExchangeUDP(const AServer: StringRAL; const AQuery: TBytes; AID: Word;
  const AName: StringRAL; AQType: Word; ATimeout: Integer;
  var AResult: TRALDNSResult; out ATruncated: Boolean): Boolean;
var
  vSock: TRALSocket;
  vAddr, vFrom: TRALSocketAddress;
  vBuf: array[0..4095] of Byte;
  vMsg: TBytes;
  vDeadline: Int64RAL;
  vLeft, vGot: Integer;
begin
  Result := False;
  ATruncated := False;
  try
    if not RALResolve(AServer, 53, FamilyOf(AServer), vAddr) then
      Exit;
    vSock := TRALSocket.Create;
    try
      vSock.OpenUDP('', 0, FamilyOf(AServer));
      vSock.SendTo(AQuery[0], Length(AQuery), vAddr);
      vDeadline := RALSocketNow + ATimeout;
      while not Result do
      begin
        vLeft := Integer(vDeadline - RALSocketNow);
        if vLeft <= 0 then
          Break;
        if vSock.WaitFor(RALPollIn, vLeft) = 0 then
          Break;
        vGot := vSock.ReceiveFrom(vBuf, SizeOf(vBuf), vFrom);
        if vGot <= 0 then
          Continue;
        if not RALSameAddress(vFrom, vAddr) then
          Continue;
        SetLength(vMsg, vGot);
        Move(vBuf[0], vMsg[0], vGot);
        Result := ParseResponse(vMsg, vGot, AID, AName, AQType, AResult, ATruncated);
      end;
    finally
      vSock.Free;
    end;
  except
    on Exception do
      Result := False;
  end;
end;

/// Asks one server over TCP with the 2 byte length prefix.
function ExchangeTCP(const AServer: StringRAL; const AQuery: TBytes; AID: Word;
  const AName: StringRAL; AQType: Word; ATimeout: Integer;
  var AResult: TRALDNSResult): Boolean;
var
  vSock: TRALSocket;
  vOut, vMsg: TBytes;
  vDeadline: Int64RAL;
  vDone, vGot, vLen, vLeft: Integer;
  vTrunc: Boolean;

  function Wait(AEvent: Word): Boolean;
  begin
    vLeft := Integer(vDeadline - RALSocketNow);
    Result := (vLeft > 0) and (vSock.WaitFor(AEvent, vLeft) <> 0);
  end;

  function ReadFully(var ABuffer; ACount: Integer): Boolean;
  var
    vRead: Integer;
  begin
    vRead := 0;
    while vRead < ACount do
    begin
      if not Wait(RALPollIn) then
      begin
        Result := False;
        Exit;
      end;
      vGot := vSock.Receive(PByte(@ABuffer)[vRead], ACount - vRead);
      if vGot = 0 then
      begin
        Result := False;
        Exit;
      end;
      if vGot > 0 then
        Inc(vRead, vGot);
    end;
    Result := True;
  end;

begin
  Result := False;
  vDeadline := RALSocketNow + ATimeout;
  try
    vSock := TRALSocket.Create;
    try
      vSock.Connect(AServer, 53, ATimeout, FamilyOf(AServer));
      SetLength(vOut, Length(AQuery) + 2);
      vOut[0] := Length(AQuery) shr 8;
      vOut[1] := Length(AQuery) and $FF;
      Move(AQuery[0], vOut[2], Length(AQuery));
      vDone := 0;
      while vDone < Length(vOut) do
      begin
        if not Wait(RALPollOut) then
          Exit;
        vGot := vSock.Send(vOut[vDone], Length(vOut) - vDone);
        if vGot > 0 then
          Inc(vDone, vGot);
      end;
      SetLength(vMsg, 2);
      if not ReadFully(vMsg[0], 2) then
        Exit;
      vLen := vMsg[0] * 256 + vMsg[1];
      if vLen < 12 then
        Exit;
      SetLength(vMsg, vLen);
      if not ReadFully(vMsg[0], vLen) then
        Exit;
      Result := ParseResponse(vMsg, vLen, AID, AName, AQType, AResult, vTrunc);
    finally
      vSock.Free;
    end;
  except
    on Exception do
      Result := False;
  end;
end;

// ---- the servers of the system ----

{$IFDEF RALWindows}
type
  /// Node of the address list of GetNetworkParams.
  TIPAddrString = record
    /// The next node, or nil.
    Next: Pointer;
    /// The address as text.
    IpAddress: array[0..15] of AnsiChar;
    /// The mask as text.
    IpMask: array[0..15] of AnsiChar;
    /// Reserved by the system.
    Context: Cardinal;
  end;
  /// Pointer to a node of the address list.
  PIPAddrString = ^TIPAddrString;

  /// The network parameters of the machine (FIXED_INFO).
  TFixedInfo = record
    /// The host name.
    HostName: array[0..131] of AnsiChar;
    /// The domain name.
    DomainName: array[0..131] of AnsiChar;
    /// The server in use.
    CurrentDnsServer: Pointer;
    /// The DNS servers.
    DnsServerList: TIPAddrString;
    /// The node type.
    NodeType: Cardinal;
  end;
  /// Pointer to the network parameters.
  PFixedInfo = ^TFixedInfo;

/// Reads the network parameters of the machine.
function GetNetworkParams(AInfo: Pointer; var ASize: Cardinal): Cardinal; stdcall;
  external 'iphlpapi.dll' name 'GetNetworkParams';

/// Appends the DNS servers the operating system is configured with.
procedure LoadSystemServersOS(AList: TStrings);
var
  vSize: Cardinal;
  vBuf: Pointer;
  vItem: PIPAddrString;
begin
  vSize := 0;
  GetNetworkParams(nil, vSize);
  if vSize = 0 then
    Exit;
  GetMem(vBuf, vSize);
  try
    if GetNetworkParams(vBuf, vSize) <> 0 then
      Exit;
    vItem := @PFixedInfo(vBuf)^.DnsServerList;
    while vItem <> nil do
    begin
      if vItem^.IpAddress[0] <> #0 then
        AList.Add(string(PAnsiChar(@vItem^.IpAddress[0])));
      vItem := PIPAddrString(vItem^.Next);
    end;
  finally
    FreeMem(vBuf);
  end;
end;
{$ELSE}
/// Appends the DNS servers the operating system is configured with.
procedure LoadSystemServersOS(AList: TStrings);
var
  vFile: TStringList;
  vInt, vPos: Integer;
  vLine, vAddr: string;
begin
  if not FileExists('/etc/resolv.conf') then
    Exit;
  vFile := TStringList.Create;
  try
    try
      vFile.LoadFromFile('/etc/resolv.conf');
    except
      on Exception do
        Exit;
    end;
    for vInt := 0 to vFile.Count - 1 do
    begin
      vLine := Trim(vFile[vInt]);
      if (Copy(vLine, 1, 10) = 'nameserver') and (Length(vLine) > 10) and
         ((vLine[11] = ' ') or (vLine[11] = #9)) then
      begin
        vAddr := Trim(Copy(vLine, 11, Length(vLine)));
        vPos := Pos('%', vAddr);
        if vPos > 0 then
          vAddr := Copy(vAddr, 1, vPos - 1);
        vPos := Pos('#', vAddr);
        if vPos > 0 then
          vAddr := Trim(Copy(vAddr, 1, vPos - 1));
        if vAddr <> '' then
          AList.Add(vAddr);
      end;
    end;
  finally
    vFile.Free;
  end;
end;
{$ENDIF}

/// TRALDNSResolver
constructor TRALDNSResolver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCache := TStringList.Create;
  FCache.Sorted := True;
  FCache.CaseSensitive := True;
  FCache.OwnsObjects := True;
  FLock := TCriticalSection.Create;
  FRetries := 1;
  FServers := TStringList.Create;
  FTimeout := 2000;
  FUseCache := True;
end;

destructor TRALDNSResolver.Destroy;
begin
  FSystemServers.Free;
  FServers.Free;
  FLock.Free;
  FCache.Free;
  inherited Destroy;
end;

function TRALDNSResolver.Ask(const AServer, AName: StringRAL; AType: TRALDNSType;
  var AResult: TRALDNSResult; out AAnswered: Boolean): Boolean;
var
  vID: Word;
  vQuery: TBytes;
  vRand: TBytes;
  vTrunc: Boolean;
  vTry: TRALDNSResult;
begin
  vRand := RandomBytes(2);
  vID := vRand[0] * 256 + vRand[1];
  AAnswered := False;
  Result := False;
  if not BuildQuery(vID, AName, WireOf(AType), vQuery) then
    Exit;
  ClearResult(vTry);
  if not ExchangeUDP(AServer, vQuery, vID, AName, WireOf(AType), FTimeout, vTry, vTrunc) then
    Exit;
  if vTrunc then
  begin
    ClearResult(AResult);
    if ExchangeTCP(AServer, vQuery, vID, AName, WireOf(AType), FTimeout, AResult) then
    begin
      AAnswered := True;
      Result := True;
      Exit;
    end;
  end;
  AResult := vTry;
  AAnswered := True;
  Result := True;
end;

function TRALDNSResolver.CacheFind(const AKey: string; out AResult: TRALDNSResult): Boolean;
var
  vIdx: Integer;
begin
  ClearResult(AResult);
  Result := False;
  FLock.Enter;
  try
    vIdx := FCache.IndexOf(AKey);
    if vIdx >= 0 then
    begin
      if TCacheEntry(FCache.Objects[vIdx]).Expires > RALSocketNow then
      begin
        AResult := TCacheEntry(FCache.Objects[vIdx]).Result;
        Result := True;
      end
      else
        FCache.Delete(vIdx);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TRALDNSResolver.CacheStore(const AKey: string; const AResult: TRALDNSResult);
var
  vTTL: Cardinal;
  vInt: Integer;
  vEntry: TCacheEntry;
  vIdx: Integer;
begin
  if Length(AResult.Records) = 0 then
    vTTL := cNegativeTTL
  else
  begin
    vTTL := cMaxTTL;
    for vInt := 0 to High(AResult.Records) do
      if AResult.Records[vInt].TTL < vTTL then
        vTTL := AResult.Records[vInt].TTL;
  end;
  if vTTL = 0 then
    Exit;
  vEntry := TCacheEntry.Create;
  vEntry.Result := AResult;
  vEntry.Expires := RALSocketNow + Int64RAL(vTTL) * 1000;
  FLock.Enter;
  try
    if FCache.Count >= cMaxCache then
    begin
      for vInt := FCache.Count - 1 downto 0 do
        if TCacheEntry(FCache.Objects[vInt]).Expires <= RALSocketNow then
          FCache.Delete(vInt);
      if FCache.Count >= cMaxCache then
        FCache.Clear;
    end;
    vIdx := FCache.IndexOf(AKey);
    if vIdx >= 0 then
      FCache.Delete(vIdx);
    FCache.AddObject(AKey, vEntry);
  finally
    FLock.Leave;
  end;
end;

procedure TRALDNSResolver.ClearCache;
begin
  FLock.Enter;
  try
    FCache.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TRALDNSResolver.LoadServers(AList: TStrings);
begin
  FLock.Enter;
  try
    if FServers.Count > 0 then
      AList.AddStrings(FServers)
    else
    begin
      if FSystemServers = nil then
      begin
        FSystemServers := TStringList.Create;
        SystemServers(FSystemServers);
      end;
      AList.AddStrings(FSystemServers);
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALDNSResolver.Query(const AName: StringRAL; AType: TRALDNSType): TRALDNSResult;
var
  vName: StringRAL;
  vKey: string;
  vServers: TStringList;
  vRound, vInt: Integer;
  vTry, vBad: TRALDNSResult;
  vAnswered, vHaveBad, vDone: Boolean;
begin
  ClearResult(Result);
  vName := LowerName(AName);
  if (vName <> '') and (vName[Length(vName)] = '.') then
    SetLength(vName, Length(vName) - 1);
  vKey := Chr(Ord(AType) + 48) + string(vName);
  if FUseCache and CacheFind(vKey, Result) then
    Exit;
  if (vName = '') or (Length(vName) > 253) then
  begin
    Result.RCode := drFormErr;
    Result.Error := Format(emDNSBadName, [string(AName)]);
    Exit;
  end;
  vServers := TStringList.Create;
  try
    LoadServers(vServers);
    vDone := False;
    vHaveBad := False;
    ClearResult(vBad);
    for vRound := 0 to FRetries do
    begin
      for vInt := 0 to vServers.Count - 1 do
      begin
        ClearResult(vTry);
        if Ask(StringRAL(Trim(vServers[vInt])), vName, AType, vTry, vAnswered) and vAnswered then
        begin
          if vTry.RCode in [drNoError, drNXDomain] then
          begin
            Result := vTry;
            vDone := True;
            Break;
          end;
          if vTry.RCode = drFormErr then
          begin
            // the server cannot take this name: a retry gets the same
            Result := vTry;
            vDone := True;
            Break;
          end;
          vBad := vTry;
          vHaveBad := True;
        end;
      end;
      if vDone then
        Break;
    end;
    if not vDone then
    begin
      if vHaveBad then
        Result := vBad
      else
      begin
        Result.RCode := drTimeout;
        Result.Error := Format(emDNSNoAnswer, [Trim(vServers.CommaText), FTimeout]);
      end;
      Result.Temporary := True;
    end;
  finally
    vServers.Free;
  end;
  if (not Result.Temporary) and FUseCache then
    CacheStore(vKey, Result);
end;

procedure TRALDNSResolver.SetServers(AValue: TStrings);
begin
  FServers.Assign(AValue);
end;

class procedure TRALDNSResolver.SystemServers(AList: TStrings);
var
  vBefore: Integer;
begin
  vBefore := AList.Count;
  try
    LoadSystemServersOS(AList);
  except
    on Exception do
      ;
  end;
  if AList.Count = vBefore then
    AList.Add(cFallbackServer);
end;

end.
