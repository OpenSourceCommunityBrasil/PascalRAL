{ SPF (RFC 7208), DKIM, DMARC (RFC 7489) and the Authentication-Results header
  (RFC 8601) of a received message. }
unit RALMailVerifier;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs,
  RALTypes, RALConsts, RALTools, RALCustomObjects, RALExceptions, RALDNS,
  RALDKIM, RALSMTPServer;

type
  /// The result of an SPF check, in the words of RFC 7208 section 2.6.
  TRALSPFVerdict = (
    /// No SPF record, or no domain to check.
    spvNone,
    /// The domain does not say (?).
    spvNeutral,
    /// The client is authorized.
    spvPass,
    /// The client is not authorized (-).
    spvFail,
    /// The client is probably not authorized (~).
    spvSoftFail,
    /// A DNS failure that may pass: try again later.
    spvTempError,
    /// The record is wrong or a limit was crossed.
    spvPermError
  );

  /// The verdict of DMARC (RFC 7489).
  TRALDMARCVerdict = (
    /// The domain publishes no policy, or the message has no usable From.
    dmvNone,
    /// SPF or DKIM passed with an aligned domain.
    dmvPass,
    /// Neither SPF nor DKIM passed aligned.
    dmvFail,
    /// The policy could not be fetched now: try again later.
    dmvTempError,
    /// The policy is malformed.
    dmvPermError
  );

  /// What the domain asks to do with a message that fails DMARC (p= and sp=).
  TRALDMARCPolicy = (
    /// Nothing: only report.
    dmpNone,
    /// Treat it as suspicious (junk folder).
    dmpQuarantine,
    /// Refuse it.
    dmpReject
  );

  /// Raised for a client address that is not an IP address.
  ERALMailVerifyError = class(ERALException);

  /// What the verifier found about one message.
  TRALMailVerifyResult = class
  public
    { The Authentication-Results header field, ending with CRLF, ready to put in
      front of the message. }
    AuthenticationResults: StringRAL;
    /// The checked signatures (TRALDKIMResult), owned by the result.
    DKIM: TObjectList;
    /// DKIM-Signature of the message with an aligned d= that passed.
    DKIMAligned: Boolean;
    /// The verdict of DMARC.
    DMARC: TRALDMARCVerdict;
    /// What the verifier advises doing with the message after p=, sp= and pct=.
    DMARCDisposition: TRALDMARCPolicy;
    /// The policy that applies to the From domain (p=, or sp= for a subdomain).
    DMARCPolicy: TRALDMARCPolicy;
    /// Why DMARC has no pass or why it failed; empty on a pass.
    DMARCReason: string;
    /// The domain of the From header; empty when there is none.
    FromDomain: StringRAL;
    /// The domain DMARC took as the organizational domain of FromDomain.
    OrgDomain: StringRAL;
    /// The verdict of SPF.
    SPF: TRALSPFVerdict;
    /// SPF passed for a domain aligned with FromDomain.
    SPFAligned: Boolean;
    /// The identity SPF checked: the domain of MAIL FROM, or HELO for a null sender.
    SPFDomain: StringRAL;
    /// The explanation text of the exp= modifier when SPF failed.
    SPFExplanation: StringRAL;

    constructor Create;
    destructor Destroy; override;

    /// Empties the result for a new message.
    procedure Clear;
    /// True when DMARC failed and the applied policy is reject: refuse the message.
    function ShouldReject: Boolean;
  end;

  { Checks a client address against the SPF record of a domain (RFC 7208): every
    mechanism and modifier, macros, the limit of 10 DNS lookups and of 2 void
    lookups. One instance can run one check at a time. }
  TRALSPFChecker = class
  private
    /// The HELO name of the client.
    FHelo: StringRAL;
    FHostName: StringRAL;
    /// The client address as bytes.
    FIP: TRALIPBytes;
    /// Bytes of FIP in use: 4 or 16.
    FIPLen: Integer;
    /// The client address as text.
    FIPText: StringRAL;
    /// The local part of the sender.
    FLocalPart: StringRAL;
    FLookups: Integer;
    /// The DNS resolver.
    FResolver: TRALDNSResolver;
    /// The sender, with postmaster when it has no local part.
    FSender: StringRAL;
    /// The domain of the sender.
    FSenderDomain: StringRAL;
    /// The validated names of the client, loaded when needed.
    FValidated: TStringList;
    /// Void lookups spent.
    FVoids: Integer;

    /// Whether an A or AAAA record of ATarget holds the client address.
    function AddressMatch(const ATarget: StringRAL; ACidr4, ACidr6: Integer): Boolean;
    /// Counts one DNS lookup; ends the check past the limit.
    procedure CountLookup;
    /// Runs check_host for ADomain.
    function Evaluate(const ADomain: StringRAL; AIncluded: Boolean;
      var AExplanation: StringRAL): TRALSPFVerdict;
    /// Expands the macros of ASpec (RFC 7208 7); AExp allows c, r and t.
    function Expand(const ASpec, ADomain: StringRAL; AExp: Boolean): StringRAL;
    /// The explanation text of an exp= modifier; empty when it cannot be had.
    function Explain(const AExpSpec, ADomain: StringRAL): StringRAL;
    /// Reads the validated names of the client from its PTR record.
    procedure LoadValidated;
    /// A counted query: a temporary failure ends the check, a third void answer too.
    function Lookup(const AName: StringRAL; AType: TRALDNSType): TRALDNSResult;
    /// The value of the macro ALetter before transformers.
    function MacroValue(ALetter: AnsiChar; const ADomain: StringRAL): StringRAL;
    /// Whether the directive AKind with AArg matches the client.
    function Matches(const AKind: string; const AArg, ADomain: StringRAL): Boolean;
    /// Whether an MX host of ATarget has the client address.
    function MatchMX(const ATarget: StringRAL; ACidr4, ACidr6: Integer): Boolean;
    /// Whether the client has a validated name under ATarget.
    function MatchPTR(const ATarget: StringRAL): Boolean;
  public
    /// Creates a checker that asks AResolver.
    constructor Create(AResolver: TRALDNSResolver);
    destructor Destroy; override;

    { Runs check_host for the client AIP that said AHelo and sent from AMailFrom
      (empty for a bounce: postmaster@@AHelo is checked). AExplanation is the
      text of exp= when the result is spvFail. }
    function Check(const AIP, AHelo, AMailFrom: StringRAL;
      out AExplanation: StringRAL): TRALSPFVerdict;

    /// The name of this host that the macro r expands to in an explanation.
    property HostName: StringRAL read FHostName write FHostName;
    /// The DNS lookups the last check spent (RFC 7208 limits them to 10).
    property Lookups: Integer read FLookups;
  end;

  { Lets the application give the organizational domain of ADomain; AOrganizational
    holds the built-in answer. }
  TRALOrganizationalDomainEvent = procedure(Sender: TObject; const ADomain: StringRAL;
    var AOrganizational: StringRAL) of object;

  { Checks SPF, DKIM and DMARC of a received message and writes the
    Authentication-Results header. Link Resolver to a TRALDNSResolver, or leave
    it empty to use an internal one with the system's servers. In the OnMessage
    of a TRALSMTPServer call VerifyMessage, put AuthenticationResults in front of
    the message and refuse it when ShouldReject says so. }
  TRALMailVerifier = class(TRALComponent)
  private
    FAuthServId: StringRAL;
    FDKIM: TRALDKIMVerifier;
    FOnOrganizationalDomain: TRALOrganizationalDomainEvent;
    /// The resolver used when none is linked.
    FOwnResolver: TRALDNSResolver;
    FResolver: TRALDNSResolver;

    /// Evaluates DMARC for the From domains of the message.
    procedure CheckDMARC(AResult: TRALMailVerifyResult; const AFrom: TStrings);
    /// Gives the DKIM verifier the key record from DNS.
    procedure DoKeyLookup(Sender: TObject; const ASelector, ADomain: StringRAL;
      var ARecord: StringRAL; var ATemporary: Boolean);
    /// The linked resolver, or the internal one.
    function GetResolver: TRALDNSResolver;
    /// Builds the Authentication-Results header field.
    function HeaderText(AResult: TRALMailVerifyResult; const AHelo, AMailFrom: StringRAL): StringRAL;
    procedure SetResolver(AValue: TRALDNSResolver);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { Whether a message is subject to the policy under pct=: True with a
      probability of APct percent. }
    function Sample(APct: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { The organizational domain of ADomain: the built-in public suffix rules,
      then OnOrganizationalDomain. }
    function OrganizationalDomain(const ADomain: StringRAL): StringRAL;
    { Checks the message in AData (read from its position, left where it was)
      that the client AIP sent saying AHelo from AMailFrom (empty for a bounce),
      into AResult (which Verify empties first). Raises ERALMailVerifyError when
      AIP is not an IP address. }
    procedure Verify(const AIP, AHelo, AMailFrom: StringRAL; AData: TStream;
      AResult: TRALMailVerifyResult);
    { Verify for the message a TRALSMTPServer gives to OnMessage: the client
      address and HELO come from ASession, MAIL FROM from AEnvelope. }
    procedure VerifyMessage(ASession: TRALSMTPSession; AEnvelope: TRALSMTPEnvelope;
      AData: TStream; AResult: TRALMailVerifyResult);

    /// The DKIM checker, for its MaxSignatures.
    property DKIM: TRALDKIMVerifier read FDKIM;
  published
    /// The authserv-id of the header (RFC 8601): the name of this server.
    property AuthServId: StringRAL read FAuthServId write FAuthServId;
    /// Gives an organizational domain when the built-in rules are not enough.
    property OnOrganizationalDomain: TRALOrganizationalDomainEvent
      read FOnOrganizationalDomain write FOnOrganizationalDomain;
    /// The DNS resolver; empty = an internal one with the system's servers.
    property Resolver: TRALDNSResolver read FResolver write SetResolver;
  end;

/// The word of an SPF verdict: none, neutral, pass, fail, softfail, temperror, permerror.
function RALSPFVerdictName(AVerdict: TRALSPFVerdict): StringRAL;
/// The word of a DMARC verdict: none, pass, fail, temperror, permerror.
function RALDMARCVerdictName(AVerdict: TRALDMARCVerdict): StringRAL;
{ The organizational domain by the built-in rules: the registrable domain under
  a public suffix, for the common multi-label suffixes (com.br, co.uk, com.au...)
  and the one-label ones. }
function RALOrganizationalDomain(const ADomain: StringRAL): StringRAL;

implementation

type
  /// Ends a check with a result: raised for a permerror or temperror.
  ESPFAbort = class(Exception)
  public
    /// The result the check ends with.
    Verdict: TRALSPFVerdict;
  end;

const
  /// DNS lookups a check may spend (RFC 7208 4.6.4).
  cMaxLookups = 10;
  /// Void lookups a check tolerates.
  cMaxVoids = 2;
  /// MX records a mechanism may use.
  cMaxMX = 10;
  /// PTR names a check validates.
  cMaxPTR = 10;
  /// Bytes of the message read to find its headers.
  cMaxHeaderBytes = 262144;
  /// Public suffixes of two labels: the registrable domain under them has three.
  cSuffixes = ' '+
    'co.uk org.uk me.uk ltd.uk plc.uk ac.uk gov.uk net.uk sch.uk com.br net.br ' +
    'org.br gov.br edu.br mil.br art.br eco.br blog.br adv.br eng.br jus.br leg.br ' +
    'mp.br tv.br etc.br ind.br inf.br med.br psi.br com.au net.au org.au edu.au ' +
    'gov.au id.au asn.au co.nz org.nz net.nz govt.nz ac.nz school.nz co.jp ne.jp ' +
    'or.jp ac.jp go.jp ad.jp ed.jp gr.jp co.in net.in org.in gov.in ac.in edu.in ' +
    'firm.in gen.in ind.in com.cn net.cn org.cn gov.cn edu.cn ac.cn com.mx org.mx ' +
    'gob.mx edu.mx net.mx com.ar net.ar org.ar gob.ar com.co com.pe com.ve com.uy ' +
    'com.py com.bo com.ec co.za org.za net.za gov.za com.tr com.tw com.hk com.sg ' +
    'com.my com.ph com.vn co.id co.kr or.kr co.il com.eg com.sa com.ua com.pl ' +
    'com.pt com.es co.th in.th com.ng com.pk com.bd com.lb com.cy com.mt com.gr ' +
    'com.ru com.de com.fr co.at co.ke co.tz ';

// ---- text helpers ----

/// Appends one byte to AText without the code page conversion of string + char.
procedure AppendChar(var AText: StringRAL; AChar: AnsiChar);
begin
  SetLength(AText, Length(AText) + 1);
  AText[Length(AText)] := AChar;
end;

/// Lowercases the ASCII letters of AText.
function Low(const AText: StringRAL): StringRAL;
var
  vInt: Integer;
begin
  Result := AText;
  SetLength(Result, Length(AText));
  for vInt := 1 to Length(Result) do
    if Result[vInt] in ['A'..'Z'] then
      Result[vInt] := AnsiChar(Ord(Result[vInt]) + 32);
end;

/// The position of ASub in AText from AStart; 0 when absent.
function PosOf(const ASub, AText: StringRAL; AStart: Integer = 1): Integer;
var
  vInt, vLen: Integer;
begin
  Result := 0;
  vLen := Length(ASub);
  if vLen = 0 then
    Exit;
  for vInt := AStart to Length(AText) - vLen + 1 do
    if (AText[vInt] = ASub[1]) and (Copy(AText, vInt, vLen) = ASub) then
    begin
      Result := vInt;
      Exit;
    end;
end;

/// The position of the last AChar; 0 when absent.
function LastPos(AChar: AnsiChar; const AText: StringRAL): Integer;
var
  vInt: Integer;
begin
  for vInt := Length(AText) downto 1 do
    if AText[vInt] = AChar then
    begin
      Result := vInt;
      Exit;
    end;
  Result := 0;
end;

/// Removes the spaces, tabs and line breaks around AText.
function TrimSpaces(const AText: StringRAL): StringRAL;
var
  vStart, vEnd: Integer;
begin
  vStart := 1;
  vEnd := Length(AText);
  while (vStart <= vEnd) and (AText[vStart] in [' ', #9, #13, #10]) do
    Inc(vStart);
  while (vEnd >= vStart) and (AText[vEnd] in [' ', #9, #13, #10]) do
    Dec(vEnd);
  Result := Copy(AText, vStart, vEnd - vStart + 1);
end;

/// Whether AText starts with APrefix, ignoring case.
function StartsText(const APrefix, AText: StringRAL): Boolean;
begin
  Result := (Length(AText) >= Length(APrefix)) and
    (Low(Copy(AText, 1, Length(APrefix))) = Low(APrefix));
end;

/// Whether AText is made only of digits.
function IsDigits(const AText: StringRAL): Boolean;
var
  vInt: Integer;
begin
  Result := AText <> '';
  for vInt := 1 to Length(AText) do
    if not (AText[vInt] in ['0'..'9']) then
      Result := False;
end;

/// The value of a digit string, capped.
function IntOf(const AText: StringRAL): Integer;
var
  vInt: Integer;
begin
  Result := 0;
  for vInt := 1 to Length(AText) do
    if Result < 100000 then
      Result := Result * 10 + Ord(AText[vInt]) - 48;
end;

/// Whether AChar is a letter or a digit.
function IsAlnum(AChar: AnsiChar): Boolean;
begin
  Result := AChar in ['A'..'Z', 'a'..'z', '0'..'9'];
end;

/// Ends the check with AVerdict.
procedure SPFError(AVerdict: TRALSPFVerdict);
var
  vErr: ESPFAbort;
begin
  vErr := ESPFAbort.Create('SPF');
  vErr.Verdict := AVerdict;
  raise vErr;
end;

{ A host name usable as the domain of a check: labels of 1 to 63 letters,
  digits, - and _, at most one trailing dot. }
function ValidDomain(const ADomain: StringRAL): Boolean;
var
  vInt, vLabel: Integer;
  vText: StringRAL;
begin
  Result := False;
  vText := ADomain;
  if (vText <> '') and (vText[Length(vText)] = '.') then
    SetLength(vText, Length(vText) - 1);
  if (vText = '') or (Length(vText) > 253) then
    Exit;
  vLabel := 0;
  for vInt := 1 to Length(vText) do
    if vText[vInt] = '.' then
    begin
      if vLabel = 0 then
        Exit;
      vLabel := 0;
    end
    else
    begin
      if not (IsAlnum(vText[vInt]) or (vText[vInt] in ['-', '_'])) then
        Exit;
      Inc(vLabel);
      if vLabel > 63 then
        Exit;
    end;
  Result := vLabel > 0;
end;

/// Whether the first ABits bits of A and B are equal.
function PrefixMatch(const A, B: TRALIPBytes; ABits: Integer): Boolean;
var
  vInt, vFull: Integer;
  vMask: Byte;
begin
  Result := False;
  vFull := ABits div 8;
  for vInt := 0 to vFull - 1 do
    if A[vInt] <> B[vInt] then
      Exit;
  if ABits mod 8 <> 0 then
  begin
    vMask := Byte($FF00 shr (ABits mod 8));
    if (A[vFull] and vMask) <> (B[vFull] and vMask) then
      Exit;
  end;
  Result := True;
end;

/// Whether AName is ADomain or below it.
function IsSubdomain(const AName, ADomain: StringRAL): Boolean;
var
  vName, vDom: StringRAL;
begin
  vName := Low(AName);
  vDom := Low(ADomain);
  if (vName <> '') and (vName[Length(vName)] = '.') then
    SetLength(vName, Length(vName) - 1);
  if (vDom <> '') and (vDom[Length(vDom)] = '.') then
    SetLength(vDom, Length(vDom) - 1);
  Result := (vName = vDom) or ((Length(vName) > Length(vDom)) and
    (Copy(vName, Length(vName) - Length(vDom), Length(vDom) + 1) = '.' + vDom));
end;

// ---- organizational domain ----

function RALOrganizationalDomain(const ADomain: StringRAL): StringRAL;
var
  vText, vTail: StringRAL;
  vDots: array[0..127] of Integer;
  vCount, vInt: Integer;
  vSuffix: Boolean;
begin
  vText := Low(ADomain);
  if (vText <> '') and (vText[Length(vText)] = '.') then
    SetLength(vText, Length(vText) - 1);
  vCount := 0;
  for vInt := Length(vText) downto 1 do
    if (vText[vInt] = '.') and (vCount < 127) then
    begin
      vDots[vCount] := vInt;
      Inc(vCount);
    end;
  // vDots[0] is the last dot, vDots[1] the one before
  if vCount < 2 then
  begin
    Result := vText;
    Exit;
  end;
  vTail := Copy(vText, vDots[1] + 1, Length(vText));
  vSuffix := Pos(' ' + string(vTail) + ' ', cSuffixes) > 0;
  if vSuffix then
  begin
    if vCount >= 3 then
      Result := Copy(vText, vDots[2] + 1, Length(vText))
    else
      Result := vText;
  end
  else
    Result := vTail;
end;

function RALSPFVerdictName(AVerdict: TRALSPFVerdict): StringRAL;
begin
  case AVerdict of
    spvNone: Result := 'none';
    spvNeutral: Result := 'neutral';
    spvPass: Result := 'pass';
    spvFail: Result := 'fail';
    spvSoftFail: Result := 'softfail';
    spvTempError: Result := 'temperror';
  else
    Result := 'permerror';
  end;
end;

function RALDMARCVerdictName(AVerdict: TRALDMARCVerdict): StringRAL;
begin
  case AVerdict of
    dmvNone: Result := 'none';
    dmvPass: Result := 'pass';
    dmvFail: Result := 'fail';
    dmvTempError: Result := 'temperror';
  else
    Result := 'permerror';
  end;
end;

/// TRALMailVerifyResult
constructor TRALMailVerifyResult.Create;
begin
  inherited Create;
  DKIM := TObjectList.Create(True);
end;

destructor TRALMailVerifyResult.Destroy;
begin
  DKIM.Free;
  inherited Destroy;
end;

procedure TRALMailVerifyResult.Clear;
begin
  AuthenticationResults := '';
  DKIM.Clear;
  DKIMAligned := False;
  DMARC := dmvNone;
  DMARCDisposition := dmpNone;
  DMARCPolicy := dmpNone;
  DMARCReason := '';
  FromDomain := '';
  OrgDomain := '';
  SPF := spvNone;
  SPFAligned := False;
  SPFDomain := '';
  SPFExplanation := '';
end;

function TRALMailVerifyResult.ShouldReject: Boolean;
begin
  Result := (DMARC = dmvFail) and (DMARCDisposition = dmpReject);
end;

/// TRALSPFChecker
constructor TRALSPFChecker.Create(AResolver: TRALDNSResolver);
begin
  inherited Create;
  FResolver := AResolver;
  FHostName := 'unknown';
  FValidated := TStringList.Create;
end;

destructor TRALSPFChecker.Destroy;
begin
  FValidated.Free;
  inherited Destroy;
end;

procedure TRALSPFChecker.CountLookup;
begin
  Inc(FLookups);
  if FLookups > cMaxLookups then
    SPFError(spvPermError);
end;

{ A lookup of a mechanism: a temporary failure ends the check with temperror,
  and a third void answer with permerror. }
function TRALSPFChecker.Lookup(const AName: StringRAL; AType: TRALDNSType): TRALDNSResult;
begin
  Result := FResolver.Query(AName, AType);
  if Result.Temporary then
    SPFError(spvTempError);
  if (Result.RCode <> drNoError) or (Length(Result.Records) = 0) then
  begin
    Inc(FVoids);
    if FVoids > cMaxVoids then
      SPFError(spvPermError);
  end;
end;

function TRALSPFChecker.AddressMatch(const ATarget: StringRAL; ACidr4, ACidr6: Integer): Boolean;
var
  vRes: TRALDNSResult;
  vInt, vLen, vBits: Integer;
  vAddr: TRALIPBytes;
begin
  Result := False;
  if FIPLen = 4 then
  begin
    vRes := Lookup(ATarget, dtA);
    vBits := ACidr4;
  end
  else
  begin
    vRes := Lookup(ATarget, dtAAAA);
    vBits := ACidr6;
  end;
  for vInt := 0 to High(vRes.Records) do
  begin
    vLen := RALDNSParseIP(vRes.Records[vInt].Data, vAddr);
    if (vLen = FIPLen) and PrefixMatch(FIP, vAddr, vBits) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TRALSPFChecker.MatchMX(const ATarget: StringRAL; ACidr4, ACidr6: Integer): Boolean;
var
  vRes, vHost: TRALDNSResult;
  vInt, vLen, vBits, vIdx: Integer;
  vAddr: TRALIPBytes;
  vType: TRALDNSType;
begin
  Result := False;
  vRes := Lookup(ATarget, dtMX);
  if Length(vRes.Records) > cMaxMX then
    SPFError(spvPermError);
  if FIPLen = 4 then
  begin
    vType := dtA;
    vBits := ACidr4;
  end
  else
  begin
    vType := dtAAAA;
    vBits := ACidr6;
  end;
  for vInt := 0 to High(vRes.Records) do
  begin
    vHost := Lookup(vRes.Records[vInt].Data, vType);
    for vIdx := 0 to High(vHost.Records) do
    begin
      vLen := RALDNSParseIP(vHost.Records[vIdx].Data, vAddr);
      if (vLen = FIPLen) and PrefixMatch(FIP, vAddr, vBits) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

{ Fills FValidated with the names the PTR record of the client lists that
  resolve back to its address (at most 10 examined). }
procedure TRALSPFChecker.LoadValidated;
var
  vRes, vFwd: TRALDNSResult;
  vInt, vIdx, vLen: Integer;
  vAddr: TRALIPBytes;
  vType: TRALDNSType;
begin
  if FValidated.Count > 0 then
    Exit;
  vRes := FResolver.Query(RALDNSReverseName(FIPText), dtPTR);
  if vRes.Temporary then
    Exit;
  if FIPLen = 4 then
    vType := dtA
  else
    vType := dtAAAA;
  for vInt := 0 to High(vRes.Records) do
  begin
    if vInt >= cMaxPTR then
      Break;
    vFwd := FResolver.Query(vRes.Records[vInt].Data, vType);
    if vFwd.Temporary then
      Continue;
    for vIdx := 0 to High(vFwd.Records) do
    begin
      vLen := RALDNSParseIP(vFwd.Records[vIdx].Data, vAddr);
      if (vLen = FIPLen) and PrefixMatch(FIP, vAddr, FIPLen * 8) then
      begin
        FValidated.Add(string(Low(vRes.Records[vInt].Data)));
        Break;
      end;
    end;
  end;
  if FValidated.Count = 0 then
    FValidated.Add('');
end;

function TRALSPFChecker.MatchPTR(const ATarget: StringRAL): Boolean;
var
  vInt: Integer;
begin
  Result := False;
  LoadValidated;
  for vInt := 0 to FValidated.Count - 1 do
    if (FValidated[vInt] <> '') and IsSubdomain(StringRAL(FValidated[vInt]), ATarget) then
    begin
      Result := True;
      Exit;
    end;
end;

function TRALSPFChecker.MacroValue(ALetter: AnsiChar; const ADomain: StringRAL): StringRAL;
var
  vInt: Integer;
begin
  case ALetter of
    's': Result := FSender;
    'l': Result := FLocalPart;
    'o': Result := FSenderDomain;
    'd': Result := ADomain;
    'h': Result := FHelo;
    'c': Result := FIPText;
    'r': Result := FHostName;
    't': Result := StringRAL(IntToStr(Int64(Round((Now - EncodeDate(1970, 1, 1)) * 86400))));
    'v':
      if FIPLen = 4 then
        Result := 'in-addr'
      else
        Result := 'ip6';
    'i':
      if FIPLen = 4 then
        Result := RALDNSIPText(FIP, 4)
      else
      begin
        Result := '';
        for vInt := 0 to 15 do
        begin
          if Result <> '' then
            Result := Result + '.';
          Result := Result + StringRAL(IntToHex(FIP[vInt] shr 4, 1) + '.' +
            IntToHex(FIP[vInt] and $F, 1));
        end;
      end;
    'p':
    begin
      LoadValidated;
      Result := 'unknown';
      for vInt := 0 to FValidated.Count - 1 do
        if (FValidated[vInt] <> '') and IsSubdomain(StringRAL(FValidated[vInt]), ADomain) then
        begin
          Result := StringRAL(FValidated[vInt]);
          Exit;
        end;
      if FValidated[0] <> '' then
        Result := StringRAL(FValidated[0]);
    end;
  else
    Result := '';
  end;
end;

/// Cuts a domain longer than 253 from the left by whole labels (RFC 7208 7.3).
function CutDomain(const ADomain: StringRAL): StringRAL;
var
  vPos: Integer;
begin
  Result := ADomain;
  while Length(Result) > 253 do
  begin
    vPos := PosOf('.', Result);
    if vPos = 0 then
      Break;
    Result := Copy(Result, vPos + 1, Length(Result));
  end;
end;

/// Expands the macros of ASpec (RFC 7208 7); AExp allows c, r and t.
function TRALSPFChecker.Expand(const ASpec, ADomain: StringRAL; AExp: Boolean): StringRAL;
const
  cHex: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  vPos, vEnd, vInt, vKeep: Integer;
  vLetter: AnsiChar;
  vValue, vOut, vDelims: StringRAL;
  vParts: TStringList;
  vReverse, vUpper: Boolean;
  vDigits: StringRAL;
  vStart: Integer;
begin
  Result := '';
  vPos := 1;
  while vPos <= Length(ASpec) do
  begin
    if ASpec[vPos] <> '%' then
    begin
      AppendChar(Result, ASpec[vPos]);
      Inc(vPos);
      Continue;
    end;
    if vPos = Length(ASpec) then
      SPFError(spvPermError);
    case ASpec[vPos + 1] of
      '%': begin Result := Result + '%'; Inc(vPos, 2); end;
      '_': begin Result := Result + ' '; Inc(vPos, 2); end;
      '-': begin Result := Result + '%20'; Inc(vPos, 2); end;
      '{':
      begin
        vEnd := PosOf('}', ASpec, vPos + 2);
        if (vEnd = 0) or (vEnd < vPos + 3) then
          SPFError(spvPermError);
        vLetter := ASpec[vPos + 2];
        vUpper := vLetter in ['A'..'Z'];
        if vUpper then
          vLetter := AnsiChar(Ord(vLetter) + 32);
        if not (vLetter in ['s', 'l', 'o', 'd', 'i', 'p', 'h', 'c', 'r', 't', 'v']) then
          SPFError(spvPermError);
        if (vLetter in ['c', 'r', 't']) and not AExp then
          SPFError(spvPermError);
        vStart := vPos + 3;
        vDigits := '';
        while (vStart < vEnd) and (ASpec[vStart] in ['0'..'9']) do
        begin
          AppendChar(vDigits, ASpec[vStart]);
          Inc(vStart);
        end;
        vReverse := False;
        if (vStart < vEnd) and (ASpec[vStart] in ['r', 'R']) then
        begin
          vReverse := True;
          Inc(vStart);
        end;
        vDelims := Copy(ASpec, vStart, vEnd - vStart);
        for vInt := 1 to Length(vDelims) do
          if not (vDelims[vInt] in ['.', '-', '+', ',', '/', '_', '=']) then
            SPFError(spvPermError);
        if vDelims = '' then
          vDelims := '.';
        vKeep := 0;
        if vDigits <> '' then
        begin
          vKeep := IntOf(vDigits);
          if vKeep = 0 then
            SPFError(spvPermError);
        end;
        vValue := MacroValue(vLetter, ADomain);
        vParts := TStringList.Create;
        try
          vOut := '';
          for vInt := 1 to Length(vValue) + 1 do
            if (vInt > Length(vValue)) or (PosOf(vValue[vInt], vDelims) > 0) then
            begin
              vParts.Add(string(vOut));
              vOut := '';
            end
            else
              AppendChar(vOut, vValue[vInt]);
          if vReverse then
          begin
            vOut := '';
            for vInt := vParts.Count - 1 downto 0 do
            begin
              if vOut <> '' then
                vOut := vOut + '.';
              vOut := vOut + StringRAL(vParts[vInt]);
            end;
            vParts.Clear;
            vOut := vOut + '.';
            vStart := 1;
            for vInt := 1 to Length(vOut) do
              if vOut[vInt] = '.' then
              begin
                vParts.Add(string(Copy(vOut, vStart, vInt - vStart)));
                vStart := vInt + 1;
              end;
          end;
          vOut := '';
          for vInt := 0 to vParts.Count - 1 do
            if (vKeep = 0) or (vInt >= vParts.Count - vKeep) then
            begin
              if vOut <> '' then
                vOut := vOut + '.';
              vOut := vOut + StringRAL(vParts[vInt]);
            end;
        finally
          vParts.Free;
        end;
        if vUpper then
        begin
          vValue := '';
          for vInt := 1 to Length(vOut) do
            if IsAlnum(vOut[vInt]) or (vOut[vInt] in ['-', '.', '_', '~']) then
              AppendChar(vValue, vOut[vInt])
            else
            begin
              AppendChar(vValue, '%');
              AppendChar(vValue, cHex[Ord(vOut[vInt]) shr 4]);
              AppendChar(vValue, cHex[Ord(vOut[vInt]) and 15]);
            end;
          vOut := vValue;
        end;
        Result := Result + vOut;
        vPos := vEnd + 1;
      end;
    else
      SPFError(spvPermError);
    end;
  end;
end;

/// The explanation of an exp= modifier; empty when it cannot be had (never an error).
function TRALSPFChecker.Explain(const AExpSpec, ADomain: StringRAL): StringRAL;
var
  vTarget: StringRAL;
  vRes: TRALDNSResult;
  vInt: Integer;
begin
  Result := '';
  try
    vTarget := CutDomain(Expand(AExpSpec, ADomain, False));
    vRes := FResolver.Query(vTarget, dtTXT);
    if vRes.Temporary or (Length(vRes.Records) <> 1) then
      Exit;
    for vInt := 1 to Length(vRes.Records[0].Data) do
      if Ord(vRes.Records[0].Data[vInt]) > 127 then
        Exit;
    Result := Expand(vRes.Records[0].Data, ADomain, True);
  except
    on ESPFAbort do
      Result := '';
  end;
end;

/// Reads "/n", "//m" or "/n//m" of ATail (starting with the first slash).
procedure ParseCidr(const ATail: StringRAL; out ACidr4, ACidr6: Integer);
var
  vPos: Integer;
  vText: StringRAL;

  function One(const AText: StringRAL; AMax: Integer): Integer;
  begin
    if (not IsDigits(AText)) or ((Length(AText) > 1) and (AText[1] = '0')) or (Length(AText) > 3) then
      SPFError(spvPermError);
    Result := IntOf(AText);
    if Result > AMax then
      SPFError(spvPermError);
  end;

begin
  ACidr4 := 32;
  ACidr6 := 128;
  if ATail = '' then
    Exit;
  if Copy(ATail, 1, 2) = '//' then
    ACidr6 := One(Copy(ATail, 3, Length(ATail)), 128)
  else
  begin
    vText := Copy(ATail, 2, Length(ATail));
    vPos := PosOf('//', vText);
    if vPos > 0 then
    begin
      ACidr4 := One(Copy(vText, 1, vPos - 1), 32);
      ACidr6 := One(Copy(vText, vPos + 2, Length(vText)), 128);
    end
    else
      ACidr4 := One(vText, 32);
  end;
end;

{ A macro-string must be well formed: macros and characters (RFC 7208 7.1);
  AMacroEnd tells it ends with a macro. }
procedure CheckMacros(const ASpec: StringRAL; out AMacroEnd: Boolean);
var
  vInt, vEnd: Integer;
  vMacroEnd: Boolean;
begin
  vInt := 1;
  vMacroEnd := False;
  while vInt <= Length(ASpec) do
  begin
    if ASpec[vInt] = '%' then
    begin
      if vInt = Length(ASpec) then
        SPFError(spvPermError);
      case ASpec[vInt + 1] of
        '%', '_', '-':
        begin
          Inc(vInt, 2);
          vMacroEnd := False;
          Continue;
        end;
        '{':
        begin
          vEnd := PosOf('}', ASpec, vInt + 2);
          if (vEnd = 0) or not (ASpec[vInt + 2] in ['s', 'l', 'o', 'd', 'i', 'p', 'h', 'v',
              'S', 'L', 'O', 'D', 'I', 'P', 'H', 'V']) then
            SPFError(spvPermError);
          vMacroEnd := vEnd = Length(ASpec);
          vInt := vEnd + 1;
          Continue;
        end;
      else
        SPFError(spvPermError);
      end;
    end;
    if (Ord(ASpec[vInt]) <= 32) or (Ord(ASpec[vInt]) > 126) then
      SPFError(spvPermError);
    vMacroEnd := False;
    Inc(vInt);
  end;
  AMacroEnd := vMacroEnd;
end;

{ A domain-spec must be well formed whatever the result: macros, characters,
  and a top label (or a macro) at the end. }
procedure CheckDomainSpec(const ASpec: StringRAL);
var
  vInt, vDot: Integer;
  vTop: StringRAL;
  vHyphen, vAlpha, vMacroEnd: Boolean;
begin
  if ASpec = '' then
    SPFError(spvPermError);
  CheckMacros(ASpec, vMacroEnd);
  if vMacroEnd then
    Exit;
  vTop := ASpec;
  if vTop[Length(vTop)] = '.' then
    SetLength(vTop, Length(vTop) - 1);
  vDot := LastPos('.', vTop);
  if vDot = 0 then
    SPFError(spvPermError);
  vTop := Copy(vTop, vDot + 1, Length(vTop));
  if (vTop = '') or not IsAlnum(vTop[1]) or not IsAlnum(vTop[Length(vTop)]) then
    SPFError(spvPermError);
  vHyphen := False;
  vAlpha := False;
  for vInt := 1 to Length(vTop) do
    if vTop[vInt] = '-' then
      vHyphen := True
    else if vTop[vInt] in ['A'..'Z', 'a'..'z'] then
      vAlpha := True
    else if not (vTop[vInt] in ['0'..'9']) then
      SPFError(spvPermError);
  if (not vHyphen) and (not vAlpha) then
    SPFError(spvPermError);
end;


/// Whether ATail is "/n", "//m" or "/n//m" (digits only).
function IsCidrTail(const ATail: StringRAL): Boolean;
var
  vInt, vStart: Integer;
begin
  Result := False;
  if (ATail = '') or (ATail[1] <> '/') then
    Exit;
  vInt := 2;
  if (Length(ATail) >= 2) and (ATail[2] = '/') then
    Inc(vInt)
  else
  begin
    vStart := vInt;
    while (vInt <= Length(ATail)) and (ATail[vInt] in ['0'..'9']) do
      Inc(vInt);
    if vInt = vStart then
      Exit;
    if vInt > Length(ATail) then
    begin
      Result := True;
      Exit;
    end;
    if Copy(ATail, vInt, 2) <> '//' then
      Exit;
    Inc(vInt, 2);
  end;
  vStart := vInt;
  while (vInt <= Length(ATail)) and (ATail[vInt] in ['0'..'9']) do
    Inc(vInt);
  Result := (vInt > vStart) and (vInt > Length(ATail));
end;

{ Splits the argument of a mechanism into its domain-spec and the cidr tail:
  the tail is the part from a slash outside a macro that has the cidr form. }
procedure SplitArg(const AArg: StringRAL; out ASpec, ATail: StringRAL);
var
  vPos, vBrace: Integer;
begin
  ASpec := AArg;
  ATail := '';
  vBrace := 0;
  for vPos := 1 to Length(AArg) do
    if AArg[vPos] = '{' then
      Inc(vBrace)
    else if AArg[vPos] = '}' then
      Dec(vBrace)
    else if (AArg[vPos] = '/') and (vBrace <= 0) and IsCidrTail(Copy(AArg, vPos, Length(AArg))) then
    begin
      ASpec := Copy(AArg, 1, vPos - 1);
      ATail := Copy(AArg, vPos, Length(AArg));
      Break;
    end;
end;

/// Reads the address and prefix of an ip4 or ip6 argument.
procedure ParseIPArg(const AArg: StringRAL; AFamily: Integer; out AAddress: TRALIPBytes;
  out ABits: Integer);
var
  vPos, vC4, vC6: Integer;
  vAddr: StringRAL;
begin
  vAddr := AArg;
  vPos := PosOf('/', AArg);
  ABits := AFamily * 8;
  if vPos > 0 then
  begin
    vAddr := Copy(AArg, 1, vPos - 1);
    if AFamily = 4 then
    begin
      if PosOf('//', AArg) > 0 then
        SPFError(spvPermError);
      ParseCidr(Copy(AArg, vPos, Length(AArg)), vC4, vC6);
      ABits := vC4;
    end
    else
    begin
      ParseCidr('//' + Copy(AArg, vPos + 1, Length(AArg)), vC4, vC6);
      ABits := vC6;
    end;
  end;
  if RALDNSParseIP(vAddr, AAddress) <> AFamily then
    SPFError(spvPermError);
end;

/// Evaluates one directive against the client.
function TRALSPFChecker.Matches(const AKind: string; const AArg, ADomain: StringRAL): Boolean;
var
  vSpec, vTail, vTarget, vExpl: StringRAL;
  vCidr4, vCidr6, vBits: Integer;
  vIP: TRALIPBytes;
  vRes: TRALDNSResult;
begin
  Result := False;
  if AKind = 'all' then
  begin
    Result := True;
    Exit;
  end;
  if AKind = 'ip4' then
  begin
    ParseIPArg(AArg, 4, vIP, vBits);
    Result := (FIPLen = 4) and PrefixMatch(FIP, vIP, vBits);
    Exit;
  end;
  if AKind = 'ip6' then
  begin
    ParseIPArg(AArg, 16, vIP, vBits);
    Result := (FIPLen = 16) and PrefixMatch(FIP, vIP, vBits);
    Exit;
  end;
  SplitArg(AArg, vSpec, vTail);
  vCidr4 := 32;
  vCidr6 := 128;
  if (AKind = 'a') or (AKind = 'mx') then
    ParseCidr(vTail, vCidr4, vCidr6);
  if vSpec <> '' then
  begin
    CheckDomainSpec(vSpec);
    vTarget := CutDomain(Expand(vSpec, ADomain, False));
  end
  else
    vTarget := ADomain;
  CountLookup;
  if AKind = 'include' then
  begin
    vExpl := '';
    case Evaluate(vTarget, True, vExpl) of
      spvPass: Result := True;
      spvNone: SPFError(spvPermError);
    end;
  end
  else if AKind = 'a' then
    Result := AddressMatch(vTarget, vCidr4, vCidr6)
  else if AKind = 'mx' then
    Result := MatchMX(vTarget, vCidr4, vCidr6)
  else if AKind = 'ptr' then
    Result := MatchPTR(vTarget)
  else if AKind = 'exists' then
  begin
    vRes := Lookup(vTarget, dtA);
    Result := Length(vRes.Records) > 0;
  end;
end;

type
  /// One directive of a record.
  TSPFTerm = record
    /// The qualifier: +, -, ~ or ?.
    Qualifier: AnsiChar;
    /// The mechanism name.
    Kind: string;
    /// The text after the name.
    Arg: StringRAL;
  end;

function TRALSPFChecker.Evaluate(const ADomain: StringRAL; AIncluded: Boolean;
  var AExplanation: StringRAL): TRALSPFVerdict;
var
  vRes: TRALDNSResult;
  vRecord, vWord, vName, vRedirect, vExp, vRest, vTarget, vSpec, vTail, vSub: StringRAL;
  vInt, vCount, vPos, vStart, vNameEnd: Integer;
  vTerms: array of TSPFTerm;
  vTerm: TSPFTerm;
  vSeenRedirect, vSeenExp, vModifier, vMacro: Boolean;
  vIP: TRALIPBytes;
  vBits, vC4, vC6: Integer;

  function IsRecord(const AText: StringRAL): Boolean;
  begin
    Result := StartsText('v=spf1', AText) and ((Length(AText) = 6) or (AText[7] = ' '));
  end;

begin
  vRes := FResolver.Query(ADomain, dtTXT);
  if vRes.Temporary then
    SPFError(spvTempError);
  vCount := 0;
  vRecord := '';
  for vInt := 0 to High(vRes.Records) do
    if IsRecord(vRes.Records[vInt].Data) then
    begin
      Inc(vCount);
      vRecord := vRes.Records[vInt].Data;
    end;
  if vCount = 0 then
  begin
    Result := spvNone;
    Exit;
  end;
  if vCount > 1 then
    SPFError(spvPermError);
  for vInt := 1 to Length(vRecord) do
    if Ord(vRecord[vInt]) > 127 then
      SPFError(spvPermError);
  vRest := Copy(vRecord, 7, Length(vRecord));
  SetLength(vTerms, 0);
  vSeenRedirect := False;
  vSeenExp := False;
  vRedirect := '';
  vExp := '';
  vStart := 1;
  for vPos := 1 to Length(vRest) + 1 do
    if (vPos > Length(vRest)) or (vRest[vPos] = ' ') then
    begin
      vWord := Copy(vRest, vStart, vPos - vStart);
      vStart := vPos + 1;
      if vWord = '' then
        Continue;
      vTerm.Qualifier := '+';
      vName := vWord;
      if vName[1] in ['+', '-', '~', '?'] then
      begin
        vTerm.Qualifier := vName[1];
        vName := Copy(vName, 2, Length(vName));
      end;
      if (vName = '') or not (vName[1] in ['A'..'Z', 'a'..'z']) then
        SPFError(spvPermError);
      vNameEnd := 1;
      while (vNameEnd <= Length(vName)) and
            (IsAlnum(vName[vNameEnd]) or (vName[vNameEnd] in ['-', '_', '.'])) do
        Inc(vNameEnd);
      vTerm.Kind := string(Low(Copy(vName, 1, vNameEnd - 1)));
      vTerm.Arg := '';
      vModifier := (vNameEnd <= Length(vName)) and (vName[vNameEnd] = '=');
      if vModifier then
      begin
        if vWord[1] in ['+', '-', '~', '?'] then
          SPFError(spvPermError);
        vSub := Copy(vName, vNameEnd + 1, Length(vName));
        if vTerm.Kind = 'redirect' then
        begin
          if vSeenRedirect then
            SPFError(spvPermError);
          CheckDomainSpec(vSub);
          vSeenRedirect := True;
          vRedirect := vSub;
        end
        else if vTerm.Kind = 'exp' then
        begin
          if vSeenExp then
            SPFError(spvPermError);
          CheckDomainSpec(vSub);
          vSeenExp := True;
          vExp := vSub;
        end
        else
          CheckMacros(vSub, vMacro);
        Continue;
      end;
      // a mechanism: ':' domain-spec, or a cidr starting with '/'
      if vNameEnd <= Length(vName) then
      begin
        if vName[vNameEnd] = ':' then
          vTerm.Arg := Copy(vName, vNameEnd + 1, Length(vName))
        else if vName[vNameEnd] = '/' then
          vTerm.Arg := Copy(vName, vNameEnd, Length(vName))
        else
          SPFError(spvPermError);
      end;
      if not ((vTerm.Kind = 'all') or (vTerm.Kind = 'include') or (vTerm.Kind = 'a') or
              (vTerm.Kind = 'mx') or (vTerm.Kind = 'ptr') or (vTerm.Kind = 'ip4') or
              (vTerm.Kind = 'ip6') or (vTerm.Kind = 'exists')) then
        SPFError(spvPermError);
      if (vTerm.Kind = 'all') and (vNameEnd <= Length(vName)) then
        SPFError(spvPermError);
      if (vTerm.Kind = 'include') or (vTerm.Kind = 'exists') or (vTerm.Kind = 'ip4') or
         (vTerm.Kind = 'ip6') then
      begin
        if (vNameEnd > Length(vName)) or (vName[vNameEnd] <> ':') or (vTerm.Arg = '') then
          SPFError(spvPermError);
      end;
      if (vTerm.Kind = 'ptr') and (vNameEnd <= Length(vName)) and (vName[vNameEnd] <> ':') then
        SPFError(spvPermError);
      if (vTerm.Kind = 'a') or (vTerm.Kind = 'mx') or (vTerm.Kind = 'ptr') or
         (vTerm.Kind = 'include') or (vTerm.Kind = 'exists') then
      begin
        SplitArg(vTerm.Arg, vSpec, vTail);
        if vNameEnd <= Length(vName) then
          if (vName[vNameEnd] = ':') and (vSpec = '') then
            SPFError(spvPermError);
        if (vTerm.Kind = 'a') or (vTerm.Kind = 'mx') then
          ParseCidr(vTail, vC4, vC6)
        else if vTail <> '' then
          SPFError(spvPermError);
        if vSpec <> '' then
          CheckDomainSpec(vSpec);
      end
      else if vTerm.Kind = 'ip4' then
        ParseIPArg(vTerm.Arg, 4, vIP, vBits)
      else if vTerm.Kind = 'ip6' then
        ParseIPArg(vTerm.Arg, 16, vIP, vBits);
      SetLength(vTerms, Length(vTerms) + 1);
      vTerms[High(vTerms)] := vTerm;
    end;
  for vInt := 0 to High(vTerms) do
    if Matches(vTerms[vInt].Kind, vTerms[vInt].Arg, ADomain) then
    begin
      case vTerms[vInt].Qualifier of
        '+': Result := spvPass;
        '-': Result := spvFail;
        '~': Result := spvSoftFail;
      else
        Result := spvNeutral;
      end;
      if (Result = spvFail) and (not AIncluded) and (vExp <> '') then
        AExplanation := Explain(vExp, ADomain);
      Exit;
    end;
  if vRedirect <> '' then
  begin
    CountLookup;
    vTarget := CutDomain(Expand(vRedirect, ADomain, False));
    Result := Evaluate(vTarget, AIncluded, AExplanation);
    if Result = spvNone then
      SPFError(spvPermError);
    Exit;
  end;
  Result := spvNeutral;
end;

function TRALSPFChecker.Check(const AIP, AHelo, AMailFrom: StringRAL;
  out AExplanation: StringRAL): TRALSPFVerdict;
var
  vSender: StringRAL;
  vPos, vInt: Integer;
begin
  AExplanation := '';
  FLookups := 0;
  FVoids := 0;
  FValidated.Clear;
  FHelo := AHelo;
  FIPLen := RALDNSParseIP(AIP, FIP);
  if FIPLen = 0 then
  begin
    Result := spvNone;
    Exit;
  end;
  if FIPLen = 16 then
  begin
    vPos := 0;
    for vInt := 0 to 9 do
      if FIP[vInt] <> 0 then
        vPos := 1;
    if (vPos = 0) and (FIP[10] = $FF) and (FIP[11] = $FF) then
    begin
      for vInt := 0 to 3 do
        FIP[vInt] := FIP[12 + vInt];
      FIPLen := 4;
    end;
  end;
  FIPText := RALDNSIPText(FIP, FIPLen);
  vSender := AMailFrom;
  if vSender = '' then
    vSender := 'postmaster@' + AHelo;
  vPos := LastPos('@', vSender);
  if vPos = 0 then
  begin
    vSender := 'postmaster@' + vSender;
    vPos := 11;
  end;
  FLocalPart := Copy(vSender, 1, vPos - 1);
  FSenderDomain := Copy(vSender, vPos + 1, Length(vSender));
  if FLocalPart = '' then
    FLocalPart := 'postmaster';
  FSender := FLocalPart + '@' + FSenderDomain;
  if not ValidDomain(FSenderDomain) then
  begin
    Result := spvNone;
    Exit;
  end;
  try
    Result := Evaluate(FSenderDomain, False, AExplanation);
  except
    on E: ESPFAbort do
      Result := E.Verdict;
  end;
  if Result <> spvFail then
    AExplanation := '';
end;

// ---- the headers of a message ----

/// Reads the header block of AData from its position (position kept).
function ReadHeaderBlock(AData: TStream): StringRAL;
var
  vPos: Int64;
  vBuf: array[0..8191] of Byte;
  vGot, vOld: Integer;
begin
  Result := '';
  vPos := AData.Position;
  try
    while Length(Result) < cMaxHeaderBytes do
    begin
      vGot := AData.Read(vBuf[0], SizeOf(vBuf));
      if vGot <= 0 then
        Break;
      vOld := Length(Result);
      SetLength(Result, vOld + vGot);
      Move(vBuf[0], Result[vOld + 1], vGot);
      if (Copy(Result, 1, 2) = #13#10) or (Copy(Result, 1, 1) = #10) then
        Break;
      if (PosOf(#13#10#13#10, Result, vOld - 2) > 0) or (PosOf(#10#10, Result, vOld - 1) > 0) then
        Break;
    end;
  finally
    AData.Position := vPos;
  end;
  vOld := PosOf(#13#10#13#10, Result);
  vGot := PosOf(#10#10, Result);
  if (vOld > 0) and ((vGot = 0) or (vOld < vGot)) then
    Result := Copy(Result, 1, vOld + 1)
  else if vGot > 0 then
    Result := Copy(Result, 1, vGot);
end;

/// The domains of the addresses of a From header value, lowercase, added once.
procedure FromDomains(const AValue: StringRAL; AList: TStrings);
var
  vClean, vItem: StringRAL;
  vInt, vDepth, vStart, vOpen, vClose: Integer;
  vQuoted: Boolean;
  vTokens: TStringList;
  vDomain: StringRAL;

  procedure AddItem(const AText: StringRAL);
  var
    vAt: Integer;
  begin
    vItem := TrimSpaces(AText);
    vOpen := PosOf('<', vItem);
    if vOpen > 0 then
    begin
      vClose := PosOf('>', vItem, vOpen);
      if vClose = 0 then
        vClose := Length(vItem) + 1;
      vItem := Copy(vItem, vOpen + 1, vClose - vOpen - 1);
    end;
    vItem := TrimSpaces(vItem);
    vAt := LastPos('@', vItem);
    if vAt = 0 then
      Exit;
    vDomain := Low(TrimSpaces(Copy(vItem, vAt + 1, Length(vItem))));
    while (vDomain <> '') and (vDomain[Length(vDomain)] in ['.', '>', ';', ' ']) do
      SetLength(vDomain, Length(vDomain) - 1);
    if (vDomain <> '') and (AList.IndexOf(string(vDomain)) < 0) then
      AList.Add(string(vDomain));
  end;

begin
  // drop comments, hide the text of quoted strings
  vClean := '';
  vDepth := 0;
  vQuoted := False;
  for vInt := 1 to Length(AValue) do
  begin
    if vQuoted then
    begin
      if AValue[vInt] = '"' then
        vQuoted := False;
      Continue;
    end;
    if (AValue[vInt] = '"') and (vDepth = 0) then
    begin
      vQuoted := True;
      vClean := vClean + 'q';
    end
    else if AValue[vInt] = '(' then
      Inc(vDepth)
    else if AValue[vInt] = ')' then
    begin
      if vDepth > 0 then
        Dec(vDepth);
    end
    else if vDepth = 0 then
      AppendChar(vClean, AValue[vInt]);
  end;
  vTokens := TStringList.Create;
  try
    vDepth := 0;
    vStart := 1;
    for vInt := 1 to Length(vClean) + 1 do
      if (vInt > Length(vClean)) or ((vClean[vInt] in [',', ':', ';']) and (vDepth = 0)) then
      begin
        vTokens.Add(string(Copy(vClean, vStart, vInt - vStart)));
        vStart := vInt + 1;
      end
      else if vClean[vInt] = '<' then
        Inc(vDepth)
      else if vClean[vInt] = '>' then
        Dec(vDepth);
    for vInt := 0 to vTokens.Count - 1 do
      AddItem(StringRAL(vTokens[vInt]));
  finally
    vTokens.Free;
  end;
end;

/// The domains of every From header in the header block.
procedure CollectFrom(const AHeaders: StringRAL; AList: TStrings);
var
  vLines: TStringList;
  vInt, vStart: Integer;
  vText: StringRAL;
  vLine: StringRAL;
  vCurrent: StringRAL;
  vInFrom: Boolean;

  procedure Flush;
  begin
    if vInFrom then
      FromDomains(Copy(vCurrent, 6, Length(vCurrent)), AList);
    vInFrom := False;
    vCurrent := '';
  end;

begin
  vLines := TStringList.Create;
  try
    vText := AHeaders;
    vStart := 1;
    for vInt := 1 to Length(vText) + 1 do
      if (vInt > Length(vText)) or (vText[vInt] = #10) then
      begin
        vLine := Copy(vText, vStart, vInt - vStart);
        if (vLine <> '') and (vLine[Length(vLine)] = #13) then
          SetLength(vLine, Length(vLine) - 1);
        vLines.Add(string(vLine));
        vStart := vInt + 1;
      end;
    vInFrom := False;
    vCurrent := '';
    for vInt := 0 to vLines.Count - 1 do
    begin
      vLine := StringRAL(vLines[vInt]);
      if (vLine <> '') and (vLine[1] in [' ', #9]) then
        vCurrent := vCurrent + ' ' + vLine
      else
      begin
        Flush;
        if StartsText('from:', vLine) then
        begin
          vInFrom := True;
          vCurrent := vLine;
        end;
      end;
    end;
    Flush;
  finally
    vLines.Free;
  end;
end;

// ---- the Authentication-Results pieces ----

/// A property value of Authentication-Results: a token, or a quoted string.
function ARValue(const AText: StringRAL): StringRAL;
var
  vInt: Integer;
  vToken: Boolean;
begin
  vToken := AText <> '';
  for vInt := 1 to Length(AText) do
    if not (IsAlnum(AText[vInt]) or (AText[vInt] in ['.', '-', '_', '+', '@', '~'])) then
      vToken := False;
  if vToken then
    Result := AText
  else
  begin
    Result := '"';
    for vInt := 1 to Length(AText) do
    begin
      if AText[vInt] in ['"', '\'] then
        Result := Result + '\';
      if Ord(AText[vInt]) >= 32 then
        AppendChar(Result, AText[vInt]);
    end;
    Result := Result + '"';
  end;
end;

/// A comment of Authentication-Results from AText; empty when AText is.
function ARComment(const AText: StringRAL): StringRAL;
var
  vInt: Integer;
begin
  Result := '';
  for vInt := 1 to Length(AText) do
    if (Ord(AText[vInt]) >= 32) and not (AText[vInt] in ['(', ')', '\']) then
      AppendChar(Result, AText[vInt]);
  if Result <> '' then
    Result := ' (' + Result + ')';
end;

/// The word of a DMARC policy.
function PolicyName(APolicy: TRALDMARCPolicy): StringRAL;
begin
  case APolicy of
    dmpQuarantine: Result := 'quarantine';
    dmpReject: Result := 'reject';
  else
    Result := 'none';
  end;
end;

/// Reads a p= or sp= value; AValid False when it is not a policy.
function PolicyOf(const AText: StringRAL; out AValid: Boolean): TRALDMARCPolicy;
var
  vText: StringRAL;
begin
  vText := Low(TrimSpaces(AText));
  AValid := True;
  Result := dmpNone;
  if vText = 'none' then
    Result := dmpNone
  else if vText = 'quarantine' then
    Result := dmpQuarantine
  else if vText = 'reject' then
    Result := dmpReject
  else
    AValid := False;
end;

/// TRALMailVerifier
constructor TRALMailVerifier.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwnResolver := TRALDNSResolver.Create(Self);
  FDKIM := TRALDKIMVerifier.Create(Self);
  FDKIM.OnKeyLookup := DoKeyLookup;
end;

destructor TRALMailVerifier.Destroy;
begin
  FResolver := nil;
  inherited Destroy;
end;

function TRALMailVerifier.GetResolver: TRALDNSResolver;
begin
  if FResolver <> nil then
    Result := FResolver
  else
    Result := FOwnResolver;
end;

procedure TRALMailVerifier.SetResolver(AValue: TRALDNSResolver);
begin
  if FResolver = AValue then
    Exit;
  if FResolver <> nil then
    FResolver.RemoveFreeNotification(Self);
  FResolver := AValue;
  if FResolver <> nil then
    FResolver.FreeNotification(Self);
end;

procedure TRALMailVerifier.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FResolver) then
    FResolver := nil;
end;

function TRALMailVerifier.Sample(APct: Integer): Boolean;
begin
  if APct >= 100 then
    Result := True
  else if APct <= 0 then
    Result := False
  else
    Result := (RandomBytes(1)[0] mod 100) < APct;
end;

function TRALMailVerifier.OrganizationalDomain(const ADomain: StringRAL): StringRAL;
begin
  Result := RALOrganizationalDomain(ADomain);
  if Assigned(FOnOrganizationalDomain) then
    FOnOrganizationalDomain(Self, ADomain, Result);
end;

procedure TRALMailVerifier.DoKeyLookup(Sender: TObject; const ASelector, ADomain: StringRAL;
  var ARecord: StringRAL; var ATemporary: Boolean);
var
  vRes: TRALDNSResult;
begin
  vRes := GetResolver.Query(ASelector + '._domainkey.' + ADomain, dtTXT);
  if vRes.Temporary then
    ATemporary := True
  else if Length(vRes.Records) > 0 then
    ARecord := vRes.Records[0].Data;
end;

procedure TRALMailVerifier.CheckDMARC(AResult: TRALMailVerifyResult; const AFrom: TStrings);
var
  vFrom, vOrg, vRecord, vTag, vVal, vSP: StringRAL;
  vFound, vTemp, vAtOrg, vValid, vStrictDKIM, vStrictSPF, vHasRua, vHasSP: Boolean;
  vPolicy, vSub: TRALDMARCPolicy;
  vPct, vInt, vPos, vStart: Integer;
  vRes: TRALDNSResult;
  vText: StringRAL;

  function Find(const ADomain: StringRAL): Boolean;
  var
    vIdx, vCount: Integer;
    vData: StringRAL;
  begin
    Result := False;
    vRes := GetResolver.Query('_dmarc.' + ADomain, dtTXT);
    vTemp := vRes.Temporary;
    if vTemp then
      Exit;
    vCount := 0;
    for vIdx := 0 to High(vRes.Records) do
    begin
      vData := TrimSpaces(vRes.Records[vIdx].Data);
      if (Copy(vData, 1, 8) = 'v=DMARC1') and ((Length(vData) = 8) or (vData[9] in [';', ' ', #9])) then
      begin
        Inc(vCount);
        vRecord := vData;
      end;
    end;
    Result := vCount = 1;
  end;

  function Aligned(const A, B: StringRAL; AStrict: Boolean): Boolean;
  begin
    if AStrict then
      Result := Low(A) = Low(B)
    else
      Result := OrganizationalDomain(A) = OrganizationalDomain(B);
  end;

begin
  AResult.DMARC := dmvNone;
  if AFrom.Count = 0 then
  begin
    AResult.DMARCReason := emDMARCNoFrom;
    Exit;
  end;
  if AFrom.Count > 1 then
  begin
    AResult.DMARCReason := emDMARCManyFrom;
    Exit;
  end;
  vFrom := StringRAL(AFrom[0]);
  vOrg := OrganizationalDomain(vFrom);
  AResult.FromDomain := vFrom;
  AResult.OrgDomain := vOrg;
  vTemp := False;
  vAtOrg := False;
  vFound := Find(vFrom);
  if (not vFound) and (not vTemp) and (vOrg <> vFrom) then
  begin
    vFound := Find(vOrg);
    vAtOrg := vFound;
  end;
  if vTemp then
  begin
    AResult.DMARC := dmvTempError;
    AResult.DMARCReason := vRes.Error;
    Exit;
  end;
  if not vFound then
  begin
    AResult.DMARCReason := Format(emDMARCNoRecord, [string(vFrom)]);
    Exit;
  end;
  // the tags
  vPolicy := dmpNone;
  vSub := dmpNone;
  vHasSP := False;
  vHasRua := False;
  vPct := 100;
  vStrictDKIM := False;
  vStrictSPF := False;
  vValid := False;
  vStart := 1;
  vText := vRecord + ';';
  for vPos := 1 to Length(vText) do
    if vText[vPos] = ';' then
    begin
      vTag := TrimSpaces(Copy(vText, vStart, vPos - vStart));
      vStart := vPos + 1;
      vInt := PosOf('=', vTag);
      if vInt = 0 then
        Continue;
      vVal := TrimSpaces(Copy(vTag, vInt + 1, Length(vTag)));
      vTag := Low(TrimSpaces(Copy(vTag, 1, vInt - 1)));
      if vTag = 'p' then
        vPolicy := PolicyOf(vVal, vValid)
      else if vTag = 'sp' then
      begin
        vSub := PolicyOf(vVal, vHasSP);
      end
      else if vTag = 'pct' then
      begin
        if IsDigits(vVal) and (Length(vVal) <= 3) then
          vPct := IntOf(vVal);
        if vPct > 100 then
          vPct := 100;
      end
      else if vTag = 'adkim' then
        vStrictDKIM := Low(vVal) = 's'
      else if vTag = 'aspf' then
        vStrictSPF := Low(vVal) = 's'
      else if vTag = 'rua' then
        vHasRua := vVal <> '';
    end;
  vSP := '';
  if not vValid then
  begin
    if not vHasRua then
    begin
      AResult.DMARC := dmvPermError;
      AResult.DMARCReason := 'invalid p';
      Exit;
    end;
    vPolicy := dmpNone;
  end;
  if not vHasSP then
    vSub := vPolicy;
  // alignment
  AResult.SPFAligned := (AResult.SPF = spvPass) and (AResult.SPFDomain <> '') and
    Aligned(AResult.SPFDomain, vFrom, vStrictSPF);
  AResult.DKIMAligned := False;
  for vInt := 0 to AResult.DKIM.Count - 1 do
    if (TRALDKIMResult(AResult.DKIM[vInt]).Verdict = dkvPass) and
       Aligned(TRALDKIMResult(AResult.DKIM[vInt]).Domain, vFrom, vStrictDKIM) then
      AResult.DKIMAligned := True;
  if vAtOrg then
    AResult.DMARCPolicy := vSub
  else
    AResult.DMARCPolicy := vPolicy;
  if AResult.SPFAligned or AResult.DKIMAligned then
  begin
    AResult.DMARC := dmvPass;
    AResult.DMARCDisposition := dmpNone;
    Exit;
  end;
  AResult.DMARC := dmvFail;
  AResult.DMARCReason := 'no aligned SPF or DKIM pass';
  AResult.DMARCDisposition := AResult.DMARCPolicy;
  if (AResult.DMARCDisposition <> dmpNone) and (not Sample(vPct)) then
    case AResult.DMARCDisposition of
      dmpReject: AResult.DMARCDisposition := dmpQuarantine;
      dmpQuarantine: AResult.DMARCDisposition := dmpNone;
    end;
end;

function TRALMailVerifier.HeaderText(AResult: TRALMailVerifyResult;
  const AHelo, AMailFrom: StringRAL): StringRAL;
var
  vInt: Integer;
  vItem: TRALDKIMResult;
  vPart: StringRAL;

  procedure Add(const AText: StringRAL);
  begin
    Result := Result + ';'#13#10' ' + AText;
  end;

begin
  Result := 'Authentication-Results: ' + FAuthServId;
  if AMailFrom <> '' then
    Add('spf=' + RALSPFVerdictName(AResult.SPF) + ' smtp.mailfrom=' + ARValue(AMailFrom))
  else
    Add('spf=' + RALSPFVerdictName(AResult.SPF) + ' smtp.helo=' + ARValue(AHelo));
  if AResult.DKIM.Count = 0 then
    Add('dkim=none')
  else
    for vInt := 0 to AResult.DKIM.Count - 1 do
    begin
      vItem := TRALDKIMResult(AResult.DKIM[vInt]);
      vPart := 'dkim=' + RALDKIMVerdictName(vItem.Verdict) + ARComment(vItem.Reason) +
        ' header.d=' + ARValue(vItem.Domain) + ' header.s=' + ARValue(vItem.Selector);
      if vItem.Identity <> '' then
        vPart := vPart + ' header.i=' + ARValue(vItem.Identity);
      Add(vPart);
    end;
  vPart := 'dmarc=' + RALDMARCVerdictName(AResult.DMARC);
  if AResult.DMARC in [dmvPass, dmvFail] then
    vPart := vPart + ' (p=' + PolicyName(AResult.DMARCPolicy) + ' dis=' +
      PolicyName(AResult.DMARCDisposition) + ')'
  else
    vPart := vPart + ARComment(StringRAL(AResult.DMARCReason));
  if AResult.FromDomain <> '' then
    vPart := vPart + ' header.from=' + ARValue(AResult.FromDomain);
  Add(vPart);
  Result := Result + #13#10;
end;

procedure TRALMailVerifier.Verify(const AIP, AHelo, AMailFrom: StringRAL; AData: TStream;
  AResult: TRALMailVerifyResult);
var
  vChecker: TRALSPFChecker;
  vFrom: TStringList;
  vAddr: TRALIPBytes;
  vPos: Integer;
  vExpl: StringRAL;
begin
  AResult.Clear;
  if RALDNSParseIP(AIP, vAddr) = 0 then
    raise ERALMailVerifyError.CreateFmt(emMailVerifyBadIP, [string(AIP)]);
  vChecker := TRALSPFChecker.Create(GetResolver);
  try
    if FAuthServId <> '' then
      vChecker.HostName := FAuthServId;
    AResult.SPF := vChecker.Check(AIP, AHelo, AMailFrom, vExpl);
    AResult.SPFExplanation := vExpl;
  finally
    vChecker.Free;
  end;
  if AMailFrom <> '' then
  begin
    vPos := LastPos('@', AMailFrom);
    AResult.SPFDomain := Low(Copy(AMailFrom, vPos + 1, Length(AMailFrom)));
  end
  else
    AResult.SPFDomain := Low(AHelo);
  if AData <> nil then
  begin
    FDKIM.Verify(AData, AResult.DKIM);
    vFrom := TStringList.Create;
    try
      CollectFrom(ReadHeaderBlock(AData), vFrom);
      CheckDMARC(AResult, vFrom);
    finally
      vFrom.Free;
    end;
  end;
  AResult.AuthenticationResults := HeaderText(AResult, AHelo, AMailFrom);
end;

procedure TRALMailVerifier.VerifyMessage(ASession: TRALSMTPSession; AEnvelope: TRALSMTPEnvelope;
  AData: TStream; AResult: TRALMailVerifyResult);
begin
  Verify(ASession.RemoteIP, ASession.Helo, AEnvelope.From, AData, AResult);
end;

end.
