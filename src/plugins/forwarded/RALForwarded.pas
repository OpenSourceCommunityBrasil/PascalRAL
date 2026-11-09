{ The client behind a proxy (RALPluginForwardedPackage): when a request comes
  from a trusted proxy, TRALForwardedPlugin puts the client's address, scheme
  and host that Forwarded (RFC 7239) or X-Forwarded-* carry in the request,
  before any plugin judges the address. IP ranges (CIDR, IPv4 and IPv6) in
  TRALIPRanges. }
unit RALForwarded;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALExceptions, RALErrorCodes, RALTypes, RALTools, RALPlugin, RALRequest, RALResponse,
  RALParams;

const
  /// Priority of the forwarded plugin: first, so every plugin sees the client.
  RALPriorityForwarded = 1000;

type
  /// An address as 16 bytes (an IPv4 one mapped to ::ffff:a.b.c.d).
  TRALIPBytes = array[0..15] of Byte;

  { A list of addresses and networks: '10.0.0.1', '10.0.0.0/8', '2001:db8::/32',
    and the words 'loopback' (127.0.0.0/8, ::1) and 'private' (10/8, 172.16/12,
    192.168/16, 169.254/16, fc00::/7, fe80::/10). Set by Text, read by Contains
    without a lock (the ranges are replaced whole) }
  TRALIPRanges = class
  private
    /// Prefix length of each range.
    FBits: array of IntegerRAL;
    /// Serialises Text; readers take no lock.
    FLock: TCriticalSection;
    /// Network address of each range.
    FRanges: array of TRALIPBytes;

    /// Adds the network ABytes with ABits of prefix.
    procedure AddRange(const ABytes: TRALIPBytes; ABits: IntegerRAL);
  public
    constructor Create;
    destructor Destroy; override;

    /// Reads one range per line of ALines; raises on a line it cannot read.
    procedure Assign(ALines: TStrings);
    /// Whether AAddress (an address, with or without brackets and port) is in a range.
    function Contains(const AAddress: StringRAL): boolean;
    /// No range at all.
    function IsEmpty: boolean;
  end;

  /// Headers TRALForwardedPlugin reads.
  TRALForwardedHeaders = (
    /// Forwarded when present, else X-Forwarded-For, -Proto and -Host.
    rfhBoth,
    /// Only Forwarded (RFC 7239).
    rfhForwarded,
    /// Only X-Forwarded-For, -Proto and -Host.
    rfhXForwarded);

  { Behind a proxy (RAL's, NGINX, a cloud balancer) every request seems to come
    from the proxy. When the peer is in TrustedProxies, this plugin takes the
    client from Forwarded or X-Forwarded-For: the chain is read from the right,
    past every trusted proxy, and the first address that is not one is the
    client (a client cannot forge the part a trusted proxy wrote). It sets
    ClientInfo.IP, the scheme (HttpVersion HTTPS or HTTP) and Host. From a
    peer that is not trusted the headers are ignored and, with
    StripUntrusted, taken out of the request. }
  TRALForwardedPlugin = class(TRALPlugin)
  private
    FClientIPHeader: StringRAL;
    FHeaders: TRALForwardedHeaders;
    /// TrustedProxies, parsed.
    FRanges: TRALIPRanges;
    FStripUntrusted: boolean;
    FTrustedProxies: TStrings;

    procedure SetTrustedProxies(const AValue: TStrings);
    /// Parses TrustedProxies again.
    procedure TrustedChanged(Sender: TObject);
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    { A header with the client's address alone, set by the proxy (X-Real-IP,
      CF-Connecting-IP); empty reads Headers. Only from a trusted peer }
    property ClientIPHeader: StringRAL read FClientIPHeader write FClientIPHeader;
    /// Headers read.
    property Headers: TRALForwardedHeaders read FHeaders write FHeaders default rfhBoth;
    /// Takes Forwarded and X-Forwarded-* out of a request whose peer is not trusted.
    property StripUntrusted: boolean read FStripUntrusted write FStripUntrusted default True;
    /// The proxies believed, one address, network or word ('private') per line.
    property TrustedProxies: TStrings read FTrustedProxies write SetTrustedProxies;
  end;

/// AAddress as 16 bytes; False when it is not an IPv4 or IPv6 address.
function RALParseIP(const AAddress: StringRAL; out ABytes: TRALIPBytes): boolean;

implementation

uses
  RALConsts;

{ The address of a Forwarded or X-Forwarded-For entry: quotes, brackets and a
  port taken off ('"[2001:db8::1]:4711"' -> '2001:db8::1', '1.2.3.4:80' -> '1.2.3.4') }
function BareAddress(const AValue: StringRAL): StringRAL;
var
  vPos, vColons, vInt: IntegerRAL;
begin
  Result := RALTrim(AValue);
  if (Length(Result) >= 2) and (Result[POSINISTR] = '"') and
     (Result[POSINISTR + Length(Result) - 1] = '"') then
    Result := Copy(Result, 2, Length(Result) - 2);
  if (Result <> '') and (Result[POSINISTR] = '[') then
  begin
    vPos := Pos(StringRAL(']'), Result);
    if vPos > 0 then
      Result := Copy(Result, 2, vPos - 2);
    Exit;
  end;
  // one colon is IPv4 with a port; more is IPv6 without brackets
  vColons := 0;
  vPos := 0;
  for vInt := 1 to Length(Result) do
    if Result[POSINISTR - 1 + vInt] = ':' then
    begin
      Inc(vColons);
      vPos := vInt;
    end;
  if vColons = 1 then
    Result := Copy(Result, 1, vPos - 1);
end;

/// Reads a dotted IPv4 address into ABytes; False when it is not one.
function ParseIPv4(const AText: StringRAL; out ABytes: array of Byte): boolean;
var
  vParts: TStringList;
  vInt, vValue: IntegerRAL;
begin
  Result := False;
  vParts := TStringList.Create;
  try
    vParts.Delimiter := '.';
    vParts.StrictDelimiter := True;
    vParts.DelimitedText := string(AText);
    if vParts.Count <> 4 then
      Exit;
    for vInt := 0 to 3 do
    begin
      if (vParts[vInt] = '') or (Length(vParts[vInt]) > 3) then
        Exit;
      vValue := StrToIntDef(vParts[vInt], -1);
      if (vValue < 0) or (vValue > 255) then
        Exit;
      ABytes[vInt] := vValue;
    end;
    Result := True;
  finally
    vParts.Free;
  end;
end;

function RALParseIP(const AAddress: StringRAL; out ABytes: TRALIPBytes): boolean;
var
  vText: StringRAL;
  vPos, vInt, vGroup, vDouble, vCount: IntegerRAL;
  vGroups: array[0..7] of Word;
  vParts: TStringList;
  vV4: array[0..3] of Byte;
  vHex: string;
begin
  Result := False;
  FillChar(ABytes, SizeOf(ABytes), 0);
  vText := BareAddress(AAddress);
  vPos := Pos(StringRAL('%'), vText); // a zone (fe80::1%eth0)
  if vPos > 0 then
    vText := Copy(vText, 1, vPos - 1);
  if vText = '' then
    Exit;
  if Pos(StringRAL(':'), vText) = 0 then
  begin
    // IPv4, mapped
    if not ParseIPv4(vText, vV4) then
      Exit;
    ABytes[10] := $FF;
    ABytes[11] := $FF;
    Move(vV4[0], ABytes[12], 4);
    Result := True;
    Exit;
  end;
  vParts := TStringList.Create;
  try
    vParts.Delimiter := ':';
    vParts.StrictDelimiter := True;
    vParts.DelimitedText := string(vText);
    // '::' at either end gives an empty part too many
    if (vParts.Count > 1) and (vParts[0] = '') and (vParts[1] = '') then
      vParts.Delete(0);
    if (vParts.Count > 1) and (vParts[vParts.Count - 1] = '') and
       (vParts[vParts.Count - 2] = '') then
      vParts.Delete(vParts.Count - 1);
    // an IPv4 tail (::ffff:1.2.3.4) is two groups
    if (vParts.Count > 0) and (Pos('.', vParts[vParts.Count - 1]) > 0) then
    begin
      if not ParseIPv4(StringRAL(vParts[vParts.Count - 1]), vV4) then
        Exit;
      vParts[vParts.Count - 1] := IntToHex(vV4[0] * 256 + vV4[1], 1);
      vParts.Add(IntToHex(vV4[2] * 256 + vV4[3], 1));
    end;
    vDouble := -1;
    vCount := 0;
    for vInt := 0 to vParts.Count - 1 do
      if vParts[vInt] = '' then
      begin
        if vDouble >= 0 then
          Exit; // two '::'
        vDouble := vInt;
      end
      else
        Inc(vCount);
    if (vCount > 8) or ((vDouble < 0) and (vCount <> 8)) then
      Exit;
    FillChar(vGroups, SizeOf(vGroups), 0);
    vGroup := 0;
    for vInt := 0 to vParts.Count - 1 do
    begin
      if vInt = vDouble then
      begin
        vGroup := vGroup + (8 - vCount);
        Continue;
      end;
      vHex := vParts[vInt];
      if (Length(vHex) > 4) or (vGroup > 7) then
        Exit;
      vGroups[vGroup] := StrToIntDef('$' + vHex, -1) and $FFFF;
      if StrToIntDef('$' + vHex, -1) < 0 then
        Exit;
      Inc(vGroup);
    end;
    for vInt := 0 to 7 do
    begin
      ABytes[vInt * 2] := vGroups[vInt] shr 8;
      ABytes[vInt * 2 + 1] := vGroups[vInt] and $FF;
    end;
    Result := True;
  finally
    vParts.Free;
  end;
end;

{ TRALIPRanges }

constructor TRALIPRanges.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TRALIPRanges.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALIPRanges.AddRange(const ABytes: TRALIPBytes; ABits: IntegerRAL);
begin
  SetLength(FRanges, Length(FRanges) + 1);
  FRanges[High(FRanges)] := ABytes;
  SetLength(FBits, Length(FBits) + 1);
  FBits[High(FBits)] := ABits;
end;

procedure TRALIPRanges.Assign(ALines: TStrings);
const
  cLoopback: array[0..1] of string = ('127.0.0.0/8', '::1/128');
  cPrivate: array[0..5] of string = ('10.0.0.0/8', '172.16.0.0/12', '192.168.0.0/16',
    '169.254.0.0/16', 'fc00::/7', 'fe80::/10');
var
  vLines: TStringList;
  vInt, vPos, vBits: IntegerRAL;
  vLine: string;
  vBytes: TRALIPBytes;
begin
  vLines := TStringList.Create;
  FLock.Acquire;
  try
    // the words become their ranges
    for vInt := 0 to ALines.Count - 1 do
    begin
      vLine := LowerCase(Trim(ALines[vInt]));
      if vLine = '' then
        Continue;
      if vLine = 'loopback' then
        for vPos := 0 to High(cLoopback) do
          vLines.Add(cLoopback[vPos])
      else if vLine = 'private' then
      begin
        for vPos := 0 to High(cLoopback) do
          vLines.Add(cLoopback[vPos]);
        for vPos := 0 to High(cPrivate) do
          vLines.Add(cPrivate[vPos]);
      end
      else
        vLines.Add(vLine);
    end;
    SetLength(FRanges, 0);
    SetLength(FBits, 0);
    for vInt := 0 to vLines.Count - 1 do
    begin
      vLine := vLines[vInt];
      vBits := -1;
      vPos := Pos('/', vLine);
      if vPos > 0 then
      begin
        vBits := StrToIntDef(Copy(vLine, vPos + 1, MaxInt), -1);
        vLine := Copy(vLine, 1, vPos - 1);
        if vBits < 0 then
          raise ERALException.CreateFmt(ecForwardedBadRange, [vLines[vInt]]);
      end;
      if not RALParseIP(StringRAL(vLine), vBytes) then
        raise ERALException.CreateFmt(ecForwardedBadRange, [vLines[vInt]]);
      // an IPv4 prefix counts from the mapped address
      if Pos(':', vLine) = 0 then
      begin
        if vBits < 0 then
          vBits := 32;
        if vBits > 32 then
          raise ERALException.CreateFmt(ecForwardedBadRange, [vLines[vInt]]);
        vBits := vBits + 96;
      end
      else if vBits < 0 then
        vBits := 128;
      if vBits > 128 then
        raise ERALException.CreateFmt(ecForwardedBadRange, [vLines[vInt]]);
      AddRange(vBytes, vBits);
    end;
  finally
    FLock.Release;
    vLines.Free;
  end;
end;

function TRALIPRanges.Contains(const AAddress: StringRAL): boolean;
var
  vBytes: TRALIPBytes;
  vInt, vByte, vBits, vMask: IntegerRAL;
  vMatch: boolean;
begin
  Result := False;
  if (Length(FRanges) = 0) or not RALParseIP(AAddress, vBytes) then
    Exit;
  FLock.Acquire;
  try
    for vInt := 0 to High(FRanges) do
    begin
      vBits := FBits[vInt];
      vMatch := True;
      vByte := 0;
      while vBits > 0 do
      begin
        if vBits >= 8 then
          vMask := $FF
        else
          vMask := ($FF shl (8 - vBits)) and $FF;
        if (vBytes[vByte] and vMask) <> (FRanges[vInt][vByte] and vMask) then
        begin
          vMatch := False;
          Break;
        end;
        Dec(vBits, 8);
        Inc(vByte);
      end;
      if vMatch then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

function TRALIPRanges.IsEmpty: boolean;
begin
  Result := Length(FRanges) = 0;
end;

{ TRALForwardedPlugin }

type
  /// One hop of the chain: the address a proxy saw, with what it saw it ask for.
  TRALForwardedHop = record
    /// Address of the hop.
    Address: StringRAL;
    /// Scheme the hop asked for.
    Proto: StringRAL;
    /// Host the hop asked for.
    Host: StringRAL;
  end;
  /// Hops of the chain, the client first.
  TRALForwardedHops = array of TRALForwardedHop;

/// Every value of the header AName, joined by commas.
function HeaderValues(ARequest: TRALRequest; const AName: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  // a repeated header is one list (RFC 9110 5.3)
  Result := '';
  for vInt := 0 to ARequest.Params.Count - 1 do
  begin
    vParam := ARequest.Params.Index[vInt];
    if (vParam.Kind = rpkHEADER) and RALSameName(vParam.ParamName, AName) then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + vParam.AsString;
    end;
  end;
end;

/// Items of a comma list, trimmed; the caller frees it.
function SplitList(const AText: StringRAL): TStringList;
begin
  Result := TStringList.Create;
  Result.Delimiter := ',';
  Result.StrictDelimiter := True;
  Result.QuoteChar := #0;
  Result.DelimitedText := string(AText);
end;

{ Forwarded: elements by commas, pairs by semicolons (quoted values may hold
  either, RFC 7239 4) }
function ParseForwarded(const AText: StringRAL): TRALForwardedHops;
var
  vInt, vStart, vCount: IntegerRAL;
  vQuoted: boolean;
  vElement: StringRAL;

  procedure AddElement(const AElement: StringRAL);
  var
    vPairs: TStringList;
    vPair, vName, vValue: string;
    vPos, vIdx: IntegerRAL;
  begin
    SetLength(Result, vCount + 1);
    Result[vCount].Address := '';
    Result[vCount].Proto := '';
    Result[vCount].Host := '';
    vPairs := TStringList.Create;
    try
      vPairs.Delimiter := ';';
      vPairs.StrictDelimiter := True;
      vPairs.QuoteChar := #0;
      vPairs.DelimitedText := string(AElement);
      for vIdx := 0 to vPairs.Count - 1 do
      begin
        vPair := Trim(vPairs[vIdx]);
        vPos := Pos('=', vPair);
        if vPos = 0 then
          Continue;
        vName := LowerCase(Trim(Copy(vPair, 1, vPos - 1)));
        vValue := Trim(Copy(vPair, vPos + 1, MaxInt));
        if (Length(vValue) >= 2) and (vValue[1] = '"') and (vValue[Length(vValue)] = '"') then
          vValue := Copy(vValue, 2, Length(vValue) - 2);
        if vName = 'for' then
          Result[vCount].Address := BareAddress(StringRAL(vValue))
        else if vName = 'proto' then
          Result[vCount].Proto := StringRAL(LowerCase(vValue))
        else if vName = 'host' then
          Result[vCount].Host := StringRAL(vValue);
      end;
    finally
      vPairs.Free;
    end;
    Inc(vCount);
  end;

begin
  SetLength(Result, 0);
  vCount := 0;
  vQuoted := False;
  vStart := 1;
  for vInt := 1 to Length(AText) do
  begin
    if AText[POSINISTR - 1 + vInt] = '"' then
      vQuoted := not vQuoted
    else if (AText[POSINISTR - 1 + vInt] = ',') and not vQuoted then
    begin
      vElement := Copy(AText, vStart, vInt - vStart);
      AddElement(vElement);
      vStart := vInt + 1;
    end;
  end;
  vElement := Copy(AText, vStart, MaxInt);
  if RALTrim(vElement) <> '' then
    AddElement(vElement);
end;

constructor TRALForwardedPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRanges := TRALIPRanges.Create;
  FTrustedProxies := TStringList.Create;
  TStringList(FTrustedProxies).OnChange := {$IFDEF FPC}@{$ENDIF}TrustedChanged;
  FHeaders := rfhBoth;
  FStripUntrusted := True;
end;

destructor TRALForwardedPlugin.Destroy;
begin
  FreeAndNil(FTrustedProxies);
  FreeAndNil(FRanges);
  inherited Destroy;
end;

class function TRALForwardedPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityForwarded;
end;

function TRALForwardedPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate];
end;

procedure TRALForwardedPlugin.SetTrustedProxies(const AValue: TStrings);
begin
  FTrustedProxies.Assign(AValue);
end;

procedure TRALForwardedPlugin.TrustedChanged(Sender: TObject);
begin
  FRanges.Assign(FTrustedProxies);
end;

procedure TRALForwardedPlugin.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vHops: TRALForwardedHops;
  vList, vProtos, vHosts: TStringList;
  vText, vClient, vProto, vHost: StringRAL;
  vInt, vIdx: IntegerRAL;
  vBytes: TRALIPBytes;
begin
  if not FRanges.Contains(ARequest.ClientInfo.IP) then
  begin
    // a forged header from the outside is never believed, nor left for others
    if FStripUntrusted then
    begin
      ARequest.Params.DelParam('Forwarded', rpkHEADER);
      ARequest.Params.DelParam('X-Forwarded-For', rpkHEADER);
      ARequest.Params.DelParam('X-Forwarded-Proto', rpkHEADER);
      ARequest.Params.DelParam('X-Forwarded-Host', rpkHEADER);
      if FClientIPHeader <> '' then
        ARequest.Params.DelParam(FClientIPHeader, rpkHEADER);
    end;
    Exit;
  end;

  vProto := '';
  vHost := '';
  vClient := '';
  if FClientIPHeader <> '' then
  begin
    vClient := BareAddress(HeaderValues(ARequest, FClientIPHeader));
    if not RALParseIP(vClient, vBytes) then
      vClient := '';
  end
  else
  begin
    SetLength(vHops, 0);
    vText := '';
    if FHeaders <> rfhXForwarded then
      vText := HeaderValues(ARequest, 'Forwarded');
    if vText <> '' then
      vHops := ParseForwarded(vText)
    else if FHeaders <> rfhForwarded then
    begin
      vList := SplitList(HeaderValues(ARequest, 'X-Forwarded-For'));
      vProtos := SplitList(HeaderValues(ARequest, 'X-Forwarded-Proto'));
      vHosts := SplitList(HeaderValues(ARequest, 'X-Forwarded-Host'));
      try
        SetLength(vHops, vList.Count);
        for vInt := 0 to vList.Count - 1 do
        begin
          vHops[vInt].Address := BareAddress(StringRAL(vList[vInt]));
          // one value for the whole chain, or one per hop
          vHops[vInt].Proto := '';
          if vProtos.Count = vList.Count then
            vHops[vInt].Proto := StringRAL(LowerCase(Trim(vProtos[vInt])))
          else if vProtos.Count > 0 then
            vHops[vInt].Proto := StringRAL(LowerCase(Trim(vProtos[0])));
          vHops[vInt].Host := '';
          if vHosts.Count = vList.Count then
            vHops[vInt].Host := StringRAL(Trim(vHosts[vInt]))
          else if vHosts.Count > 0 then
            vHops[vInt].Host := StringRAL(Trim(vHosts[0]));
        end;
      finally
        vList.Free;
        vProtos.Free;
        vHosts.Free;
      end;
    end;
    // from the right, past the trusted proxies: the first other one is the client
    vIdx := High(vHops);
    while (vIdx > 0) and FRanges.Contains(vHops[vIdx].Address) do
      Dec(vIdx);
    if vIdx >= 0 then
    begin
      // 'unknown' or '_hidden' tells nothing: the hop to its right stays
      if RALParseIP(vHops[vIdx].Address, vBytes) then
        vClient := vHops[vIdx].Address
      else if vIdx < High(vHops) then
        vClient := vHops[vIdx + 1].Address;
      vProto := vHops[vIdx].Proto;
      vHost := vHops[vIdx].Host;
    end;
  end;

  if vClient <> '' then
    ARequest.ClientInfo.IP := vClient;
  if vProto = 'https' then
    ARequest.HttpVersion := 'HTTPS'
  else if vProto = 'http' then
    ARequest.HttpVersion := 'HTTP';
  if vHost <> '' then
    ARequest.Host := vHost;
end;

end.
