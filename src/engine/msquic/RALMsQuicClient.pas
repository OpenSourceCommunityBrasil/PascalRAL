/// Base unit for RALClient component using the MsQuic (QUIC) Engine
unit RALMsQuicClient;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\..\base\PascalRAL.inc}

interface

uses
  {$IFDEF RALMSQUIC_PROFILE}{$IFDEF RALWindows}Windows,{$ENDIF}{$ENDIF}
  Classes, SysUtils, SyncObjs,
  MsQuic,
  RALClient, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams, RALTools, RALCompress;

type
  { TRALMsQuicConnection }

  /// One QUIC connection, and the thing ShareConnection actually shares.
  ///
  /// It is reference counted because its sharers live on different threads and
  /// any of them may be the last to let go. Requests never queue on it: each
  /// one opens its own stream, which is the whole reason a shared QUIC
  /// connection does not serialise the way a shared HTTP/1.1 socket would.
  TRALMsQuicConnection = class
  private
    FConfiguration: HQUIC;
    FConnection: HQUIC;
    FConnectedEvent: TEvent;
    FConnectStatus: QUIC_STATUS;
    FConnected: boolean;
    FRefCount: IntegerRAL;
    FKey: StringRAL;
    FAlpn: AnsiString;
    FAlpnBuffer: QUIC_BUFFER;
    procedure Close;
  public
    constructor Create(const AKey: StringRAL; const AAlpn: AnsiString);
    destructor Destroy; override;
    /// Opens the connection and waits for the handshake. Raises on failure.
    procedure Open(const AHost: StringRAL; APort: IntegerRAL;
                   AVerifyCertificate: boolean;
                   AConnectTimeout, AKeepAlive: IntegerRAL);
    procedure AddRef;
    procedure Release;
    property Connected: boolean read FConnected;
    property Handle: HQUIC read FConnection;
    property Key: StringRAL read FKey;
  end;

  { TRALMsQuicPending }

  /// One request in flight. Released by whichever of the calling thread and the
  /// MsQuic callback finishes last, so a request that timed out cannot have its
  /// context freed while a callback still holds it.
  TRALMsQuicPending = class
  public
    Received: TMemoryStream;
    Done: TEvent;
    Stream: HQUIC;
    Failed: boolean;
    RefCount: IntegerRAL;
    constructor Create;
    destructor Destroy; override;
    procedure Release;
    /// Puts it back to the state Create leaves it in, WITHOUT giving the
    /// receive buffer back to the memory manager: only the position is
    /// rewound, so the next answer writes over the previous one and the
    /// capacity is reached once instead of once per request. Size would
    /// shrink it to zero - TMemoryStream reallocates on SetSize - which is
    /// exactly what this avoids.
    procedure Reset;
  end;

  { TRALMsQuicClientHTTP }

  /// RALClient over QUIC, talking to TRALMsQuicServer.
  ///
  /// NOT HTTP. What travels is the binary frame TRALMsQuicServer documents -
  /// method, URL, header lines, body, each length prefixed - over one QUIC
  /// stream per request. SupportsHTTP2 is False for that reason and not
  /// because something is missing: this engine is below HTTP, not beside it.
  ///
  /// WHY ShareConnection MATTERS MORE HERE THAN ANYWHERE ELSE
  ///
  /// Every other engine is one-object-one-connection, so sharing a transport
  /// between threads would make them wait for each other. QUIC streams are
  /// independent: N threads on ONE connection is not contention, it is
  /// multiplexing - one handshake, one congestion controller, and a lost
  /// packet that delays only the request it belongs to.
  ///
  /// With ShareConnection False this engine opens a connection per client
  /// instance, which is what TRALClient hands each thread - correct, and the
  /// arrangement that leaves the transport's main advantage on the table.
  TRALMsQuicClientHTTP = class(TRALClientHTTP)
  private
    FOwnConnection: TRALMsQuicConnection;
    FShared: TRALMsQuicConnection;
    { The context of the last request, kept for the next one. An engine belongs
      to one thread and runs one request at a time, so reusing it saves an
      object, a TMemoryStream and a KERNEL EVENT per request - and with twenty
      threads those allocations were contending for the memory manager, which
      is where the multiplexed arrangement was losing its transport advantage.
      A context a callback still holds (the timeout path) is never reused. }
    FSpare: TRALMsQuicPending;
    FLibPath: TFileName;
    FAlpn: AnsiString;
    function AcquirePending: TRALMsQuicPending;
    function ConnectionKey(const AHost: StringRAL; APort: IntegerRAL): StringRAL;
    function PickConnection(const AURL: StringRAL): TRALMsQuicConnection;
    procedure DropShared;
    procedure DropOwn;
    /// The headers every request carries, whatever the execution shape.
    procedure PrepareRequest(ARequest: TRALRequest);
    function BuildFrame(ARequest: TRALRequest; AMethod: TRALMethod;
                        const ARoute: StringRAL): TBytes;
    procedure ParseFrame(AFrame: PByte; ASize: IntegerRAL; AResponse: TRALResponse);
  public
    constructor Create(AOwner: TRALClient); override;
    destructor Destroy; override;
    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest;
                      AResponse: TRALResponse; AMethod: TRALMethod); override;
    class function EngineName: StringRAL; override;
    /// The version of the msquic actually loaded, asked of the library itself.
    /// Empty when it has not been loaded yet - there is nothing to report
    /// before the first request, and inventing a number would be worse.
    class function EngineVersion: StringRAL; override;
    class function PackageDependency: StringRAL; override;
    /// True, and it is the point of this engine - see the class comment.
    class function SupportsSharedConnection: boolean; override;
    class function SupportsKeepAliveInterval: boolean; override;
    /// Where to load msquic from; empty means the platform default name.
    property LibPath: TFileName read FLibPath write FLibPath;
  end;

{$IFDEF RALMSQUIC_PROFILE}
/// Where the time of a request goes, phase by phase, accumulated across every
/// thread. Only compiled with RALMSQUIC_PROFILE - the counters are lock free but
/// they are still six atomic adds per request, which is not something to carry
/// in production for a number nobody reads.
///
/// It exists because measuring from the outside could only ever rule
/// hypotheses out: the server was not CPU bound, no lock was shared by all the
/// engines, and still every transport landed on the same ceiling. The only way
/// left was to time the inside.
function RALMsQuicProfileReport: StringRAL;
procedure RALMsQuicProfileReset;
{$ENDIF}

implementation

const
  RALMSQUIC_MAX_FIELD = 64 * 1024 * 1024;

type
  PRALMsQuicSendCtx = ^TRALMsQuicSendCtx;
  TRALMsQuicSendCtx = record
    Buffer: QUIC_BUFFER;
    Data: TBytes;
  end;

var
  { Shared connections live for the process, keyed by where they go and under
    which policy - the same shape TRALnetHTTPClientHTTP uses for its pooled
    transports. }
  vPool: TStringList = nil;
  vPoolLock: TCriticalSection = nil;

  { ONE registration for the process, and this is not a micro-optimisation.
    A registration is the container MsQuic hangs its worker threads off: it
    creates a set of them, sized to the machine, and every connection under it
    shares them. Opening one PER CONNECTION - which is what this unit did at
    first - gives twenty connections twenty thread pools on a four core box,
    and the threads then spend their time changing context instead of moving
    packets. Measured: it was most of why adding threads stopped adding
    throughput.

    Configurations are cached beside it for the same reason, keyed by what
    actually distinguishes them: the ALPN and whether the certificate is
    checked. Building one costs a credential load. }
  vRegistration: HQUIC = nil;
  vConfigs: TStringList = nil;
  vGlobalLock: TCriticalSection = nil;

{ The registration and the configurations, created on first use. Callers hold
  no lock: this takes vGlobalLock itself. }
function SharedRegistration: HQUIC;
var
  vRegCfg: QUIC_REGISTRATION_CONFIG;
  vAppName: AnsiString;
  vStatus: QUIC_STATUS;
begin
  vGlobalLock.Enter;
  try
    if vRegistration = nil then
    begin
      FillChar(vRegCfg, SizeOf(vRegCfg), 0);
      vAppName := AnsiString(RALPACKAGESHORT);
      vRegCfg.AppName := PAnsiChar(vAppName);
      vRegCfg.ExecutionProfile := QUIC_EXECUTION_PROFILE_LOW_LATENCY;
      vStatus := MsQuicApi^.RegistrationOpen(@vRegCfg, vRegistration);
      if QUIC_FAILED(vStatus) then
      begin
        vRegistration := nil;
        raise Exception.CreateFmt('MsQuic RegistrationOpen: %s',
          [QuicStatusToStr(vStatus)]);
      end;
    end;
    Result := vRegistration;
  finally
    vGlobalLock.Leave;
  end;
end;

function SharedConfiguration(const AAlpn: AnsiString; AVerify: boolean;
  AIdleTimeout, AKeepAlive: IntegerRAL): HQUIC;
var
  vKey: StringRAL;
  vIdx: IntegerRAL;
  vCfg: HQUIC;
  vSettings: QUIC_SETTINGS;
  vCred: QUIC_CREDENTIAL_CONFIG;
  vAlpnBuf: QUIC_BUFFER;
  vStatus: QUIC_STATUS;
begin
  vKey := StringRAL(AAlpn) + '|' + IntToStr(Ord(AVerify)) + '|' +
          IntToStr(AIdleTimeout) + '|' + IntToStr(AKeepAlive);
  vGlobalLock.Enter;
  try
    vIdx := vConfigs.IndexOf(vKey);
    if vIdx >= 0 then
    begin
      Result := HQUIC(vConfigs.Objects[vIdx]);
      Exit;
    end;

    FillChar(vSettings, SizeOf(vSettings), 0);
    vSettings.IdleTimeoutMs := AIdleTimeout;
    { THE PROTOCOL DEFAULT, 25 ms, and not the 1 ms this used to force.

      Sitting on an acknowledgement only stalls the peer's next send when the
      answers stop coming back to back - which is what MORE THAN ONE dispatch
      thread on the server produced, and the server now has exactly one. With
      the answers serialised, the ACK rides along with the next one and the
      delay never has a chance to bite.

      Measured with 1, 5 and 25 ms against twenty connections: 240, 242 and
      251 us of server CPU per request, which is the same number three times.
      Forcing 1 ms bought nothing and cost an acknowledgement packet per
      millisecond on every idle connection - on a handset, that is battery. }
    vSettings.MaxAckDelayMs := 25;
    vSettings.IsSetFlags := QUIC_SETTING_IdleTimeoutMs or QUIC_SETTING_MaxAckDelayMs;

    { CONNECTION MIGRATION ON, stated rather than inherited. It is what lets a
      handset walk from Wi-Fi to mobile data and keep the connection: QUIC
      identifies it by a Connection ID instead of by the address pair, so the
      address changes underneath and the streams carry on. TCP cannot do it at
      all - it has to handshake again, and over TLS that is two round trips on
      the worst possible network. MsQuic defaults it on; it is written here
      because a default nobody wrote down is a default nobody can find. }
    vSettings.BitFlags := vSettings.BitFlags or QUIC_SETTINGS_BIT_MigrationEnabled;
    vSettings.IsSetFlags := vSettings.IsSetFlags or QUIC_SETTING_MigrationEnabled;

    { KEEP-ALIVE: a QUIC PING on the interval, which does two things at once.
      It stops a NAT or a firewall from forgetting the mapping of a connection
      that has gone quiet - the reason a mobile client finds its connection
      dead after a few minutes of nothing - and it makes a peer that vanished
      show up as gone within one interval instead of at the next request's
      timeout. Zero, the default, sends nothing. }
    if AKeepAlive > 0 then
    begin
      vSettings.KeepAliveIntervalMs := AKeepAlive;
      vSettings.IsSetFlags := vSettings.IsSetFlags or QUIC_SETTING_KeepAliveIntervalMs;
    end;

    QuicSetAlpn(vAlpnBuf, AAlpn);
    vStatus := MsQuicApi^.ConfigurationOpen(SharedRegistration, @vAlpnBuf, 1,
      @vSettings, SizeOf(vSettings), nil, vCfg);
    if QUIC_FAILED(vStatus) then
      raise Exception.CreateFmt('MsQuic ConfigurationOpen: %s',
        [QuicStatusToStr(vStatus)]);

    FillChar(vCred, SizeOf(vCred), 0);
    vCred.CredType := QUIC_CREDENTIAL_TYPE_NONE;
    vCred.Flags := QUIC_CREDENTIAL_FLAG_CLIENT;
    if not AVerify then
      vCred.Flags := vCred.Flags or QUIC_CREDENTIAL_FLAG_NO_CERTIFICATE_VALIDATION;
    vStatus := MsQuicApi^.ConfigurationLoadCredential(vCfg, @vCred);
    if QUIC_FAILED(vStatus) then
    begin
      MsQuicApi^.ConfigurationClose(vCfg);
      raise Exception.CreateFmt('MsQuic ConfigurationLoadCredential: %s',
        [QuicStatusToStr(vStatus)]);
    end;

    vConfigs.AddObject(vKey, TObject(vCfg));
    Result := vCfg;
  finally
    vGlobalLock.Leave;
  end;
end;

{$IFDEF RALMSQUIC_PROFILE}
type
  TRALMsQuicPhase = (qpSetup, qpConnect, qpBuild, qpAlloc, qpStart, qpWait,
                   qpParse, qpFree, qpParseHdr, qpParseProps, qpParseBody);

const
  RALMsQuicPhaseName: array[TRALMsQuicPhase] of StringRAL = (
    'preparar headers', 'pegar conexao', 'montar frame',
    'criar pendente', 'abrir+enviar stream', 'esperar resposta',
    'parsear resposta', 'liberar pendente',
    '  .. headers', '  .. propriedades', '  .. corpo');

var
  gPhase: array[TRALMsQuicPhase] of Int64;
  gPhaseReqs: Int64;
  gPhaseFreq: Int64;

function ProfTicks: Int64;
begin
  {$IFDEF RALWindows}
  QueryPerformanceCounter(Result);
  {$ELSE}
  Result := 0;
  {$ENDIF}
end;

/// Adds the elapsed time to a phase and moves the mark forward, so the caller
/// only carries one variable through SendUrl.
procedure ProfMark(APhase: TRALMsQuicPhase; var AFrom: Int64);
var
  vNow: Int64;
begin
  vNow := ProfTicks;
  RALAtomicInc(gPhase[APhase], vNow - AFrom);
  AFrom := vNow;
end;

procedure RALMsQuicProfileReset;
var
  vPhase: TRALMsQuicPhase;
begin
  for vPhase := Low(TRALMsQuicPhase) to High(TRALMsQuicPhase) do
    gPhase[vPhase] := 0;
  gPhaseReqs := 0;
end;

function RALMsQuicProfileReport: StringRAL;
var
  vPhase: TRALMsQuicPhase;
  vTotal: Int64;
begin
  if (gPhaseReqs = 0) or (gPhaseFreq = 0) then
  begin
    Result := 'sem amostras';
    Exit;
  end;
  vTotal := 0;
  for vPhase := Low(TRALMsQuicPhase) to qpFree do
    vTotal := vTotal + gPhase[vPhase];

  Result := Format('%d requisicoes, %.3f ms dentro de SendUrl por requisicao',
    [gPhaseReqs, vTotal / gPhaseFreq * 1000 / gPhaseReqs]) + HTTPLineBreak;
  for vPhase := Low(TRALMsQuicPhase) to High(TRALMsQuicPhase) do
    Result := Result + Format('  %-22s %8.3f ms  %5.1f%%',
      [RALMsQuicPhaseName[vPhase],
       gPhase[vPhase] / gPhaseFreq * 1000 / gPhaseReqs,
       gPhase[vPhase] / vTotal * 100]) + HTTPLineBreak;
end;
{$ENDIF}

{ TRALMsQuicPending }

constructor TRALMsQuicPending.Create;
begin
  inherited Create;
  Received := TMemoryStream.Create;
  Done := TEvent.Create(nil, True, False, '');
  RefCount := 2; // the calling thread and the callback chain
end;

destructor TRALMsQuicPending.Destroy;
begin
  if Stream <> nil then
    MsQuicApi^.StreamClose(Stream);
  Done.Free;
  Received.Free;
  inherited Destroy;
end;

procedure TRALMsQuicPending.Release;
begin
  if RALAtomicDec(RefCount) = 0 then
    Free;
end;

procedure TRALMsQuicPending.Reset;
begin
  if Stream <> nil then
  begin
    MsQuicApi^.StreamClose(Stream);
    Stream := nil;
  end;
  Received.Position := 0;
  Done.ResetEvent;
  Failed := False;
end;

{ callbacks }

function RALMsQuicClientStreamCallback(Stream: HQUIC; Context: Pointer;
  Event: PQUIC_STREAM_EVENT): QUIC_STATUS; cdecl;
var
  vCtx: TRALMsQuicPending;
  vRecv: PQuicReceiveData;
  vSendCtx: PRALMsQuicSendCtx;
  vIndex: Cardinal;
begin
  Result := QUIC_STATUS_SUCCESS;
  vCtx := TRALMsQuicPending(Context);
  try
    case Event^.EventType of
      QUIC_STREAM_EVENT_RECEIVE:
        begin
          vRecv := PQuicReceiveData(@Event^.Data[0]);
          if vRecv^.BufferCount > 0 then
            for vIndex := 0 to vRecv^.BufferCount - 1 do
              if vRecv^.Buffers^[vIndex].Length > 0 then
                vCtx.Received.WriteBuffer(vRecv^.Buffers^[vIndex].Buffer^,
                  vRecv^.Buffers^[vIndex].Length);
        end;

      QUIC_STREAM_EVENT_SEND_COMPLETE:
        begin
          vSendCtx := PRALMsQuicSendCtx(
            PQuicSendCompleteData(@Event^.Data[0])^.ClientContext);
          if vSendCtx <> nil then
            Dispose(vSendCtx);
        end;

      QUIC_STREAM_EVENT_PEER_SEND_ABORTED,
      QUIC_STREAM_EVENT_PEER_RECEIVE_ABORTED:
        vCtx.Failed := True;

      QUIC_STREAM_EVENT_SHUTDOWN_COMPLETE:
        begin
          vCtx.Done.SetEvent;
          vCtx.Release;
        end;
    end;
  except
    // an exception must never cross back into the C frame that called us
    Result := QUIC_STATUS_INTERNAL_ERROR;
  end;
end;

function RALMsQuicClientConnCallback(Connection: HQUIC; Context: Pointer;
  Event: PQUIC_CONNECTION_EVENT): QUIC_STATUS; cdecl;
var
  vConn: TRALMsQuicConnection;
  vTransport: PQuicShutdownByTransportData;
begin
  Result := QUIC_STATUS_SUCCESS;
  try
    vConn := TRALMsQuicConnection(Context);
    case Event^.EventType of
      QUIC_CONNECTION_EVENT_CONNECTED:
        begin
          vConn.FConnectStatus := QUIC_STATUS_SUCCESS;
          vConn.FConnected := True;
          vConn.FConnectedEvent.SetEvent;
        end;
      QUIC_CONNECTION_EVENT_SHUTDOWN_INITIATED_BY_TRANSPORT:
        begin
          { where a refused connection, a timeout and a rejected certificate
            arrive, each with a status of its own }
          vTransport := PQuicShutdownByTransportData(@Event^.Data[0]);
          if not vConn.FConnected then
            vConn.FConnectStatus := vTransport^.Status;
          vConn.FConnected := False;
        end;
      QUIC_CONNECTION_EVENT_SHUTDOWN_INITIATED_BY_PEER:
        vConn.FConnected := False;
      QUIC_CONNECTION_EVENT_SHUTDOWN_COMPLETE:
        begin
          vConn.FConnected := False;
          // releases an Open still waiting on a handshake that failed
          vConn.FConnectedEvent.SetEvent;
        end;
    end;
  except
    Result := QUIC_STATUS_INTERNAL_ERROR;
  end;
end;

{ frame helpers }

function BlockSize(ALength: IntegerRAL): IntegerRAL;
begin
  Result := 4 + ALength;
end;

function PutBlockStr(ADest: PByte; const AText: StringRAL): PByte;
var
  vLen: Cardinal;
begin
  vLen := Length(AText);
  Move(vLen, ADest^, 4);
  Inc(ADest, 4);
  if vLen > 0 then
  begin
    Move(AText[POSINISTR], ADest^, vLen);
    Inc(ADest, vLen);
  end;
  Result := ADest;
end;

/// Reads a length-prefixed block straight out of the receive buffer. A size
/// prefix read from the network is an allocation request from a stranger, so
/// it is bounded before it is believed.
function ReadBlockStr(ABuf: PByte; ASize: IntegerRAL; var APos: IntegerRAL;
  out AText: StringRAL): boolean;
var
  vLen: Cardinal;
begin
  Result := False;
  AText := '';
  if APos + 4 > ASize then
    Exit;
  Move(PByte(ABuf + APos)^, vLen, 4);
  Inc(APos, 4);
  if (vLen > RALMSQUIC_MAX_FIELD) or (APos + IntegerRAL(vLen) > ASize) then
    Exit;
  if vLen > 0 then
  begin
    SetLength(AText, vLen);
    Move(PByte(ABuf + APos)^, AText[POSINISTR], vLen);
  end;
  Inc(APos, IntegerRAL(vLen));
  Result := True;
end;

{ HEADERS TRAVEL AS PAIRS, NOT AS TEXT.

  They used to be one block built with AssignParamsListText on the sending side
  and taken apart with AppendParamsListText on the receiving one - an HTTP shape
  inside a frame that is not HTTP. That cost a string built, grown, trimmed and
  copied into the frame, then split line by line with a separator sniffer on the
  other end, twice per request. Measured on the server: 0.028 ms building them
  out of 0.19 ms for the whole request.

  The layout is a count followed by that many (name, value) length-prefixed
  blocks, the same prefix ReadBlockStr already reads. Nothing about the values
  changes, so a header carrying a colon, a line break or bytes above 127 now
  survives by construction instead of by the sniffer guessing right. }
type
  TRALMsQuicHeader = record
    Name: StringRAL;
    Value: StringRAL;
  end;
  TRALMsQuicHeaders = array of TRALMsQuicHeader;

function ReadHeaders(ABuf: PByte; ASize: IntegerRAL; var APos: IntegerRAL;
  AParams: TRALParams): Boolean;
var
  vCount, vIndex: Cardinal;
  vName, vValue: StringRAL;
begin
  Result := False;
  if APos + 4 > ASize then
    Exit;
  Move(PByte(ABuf + APos)^, vCount, 4);
  Inc(APos, 4);
  { a count read from the network is a promise, not a fact: the smallest pair
    is eight bytes, so anything above what is left in the buffer is a lie, and
    the caller answers with a transport error instead of looping on it }
  if vCount > Cardinal(ASize - APos) div 8 then
    Exit;
  for vIndex := 1 to vCount do
  begin
    if (not ReadBlockStr(ABuf, ASize, APos, vName)) or
       (not ReadBlockStr(ABuf, ASize, APos, vValue)) then
      Exit;
    AParams.AddParam(vName, vValue, rpkHEADER);
  end;
  Result := True;
end;

/// Reads every rpkHEADER param once - AsString materialises the value, so
/// asking twice would allocate twice - and answers how many bytes the block
/// will take.
function CollectHeaders(AParams: TRALParams; var AHeaders: TRALMsQuicHeaders;
  out ACount: IntegerRAL): IntegerRAL;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  ACount := 0;
  Result := 4;
  if AParams = nil then
    Exit;
  if Length(AHeaders) < AParams.Count then
    SetLength(AHeaders, AParams.Count);
  for vInt := 0 to Pred(AParams.Count) do
  begin
    vParam := AParams.Index[vInt];
    if vParam.Kind <> rpkHEADER then
      Continue;
    AHeaders[ACount].Name := vParam.ParamName;
    AHeaders[ACount].Value := vParam.AsString;
    Inc(Result, 8 + Length(AHeaders[ACount].Name) + Length(AHeaders[ACount].Value));
    Inc(ACount);
  end;
end;

function PutHeaders(ADest: PByte; const AHeaders: TRALMsQuicHeaders;
  ACount: IntegerRAL): PByte;
var
  vInt: IntegerRAL;
  vLen: Cardinal;
begin
  vLen := ACount;
  Move(vLen, ADest^, 4);
  Inc(ADest, 4);
  for vInt := 0 to ACount - 1 do
  begin
    ADest := PutBlockStr(ADest, AHeaders[vInt].Name);
    ADest := PutBlockStr(ADest, AHeaders[vInt].Value);
  end;
  Result := ADest;
end;

/// Host and port of a full URL. RALSplitHostPort is exported by RALClient but
/// takes a BARE "host:port" - handing it a whole URL makes the scheme part of
/// the host, and every connection then fails with WSAHOST_NOT_FOUND. The
/// scheme and the path are cut off here first. There is an equivalent inside
/// RALClient, but it is implementation-only.
procedure QuicHostPort(const AURL: StringRAL; out AHost: StringRAL;
  out APort: IntegerRAL);
var
  vValue: StringRAL;
  vPos: IntegerRAL;
begin
  vValue := AURL;
  vPos := Pos(StringRAL('://'), vValue);
  if vPos > 0 then
    vValue := Copy(vValue, vPos + 3, Length(vValue));

  for vPos := POSINISTR to Length(vValue) + POSINISTR - 1 do
    if vValue[vPos] = '/' then
    begin
      vValue := Copy(vValue, POSINISTR, vPos - POSINISTR);
      Break;
    end;

  RALSplitHostPort(vValue, AHost, APort);
end;

/// The path of a full URL, which is all the frame carries: the host lives in
/// the connection, not in the request. Returns '/' when the URL has no path.
function RouteFromUrl(const AURL: StringRAL): StringRAL;
var
  vPos, vLen: IntegerRAL;
begin
  Result := '/';
  vPos := Pos(StringRAL('://'), AURL);
  if vPos > 0 then
    vPos := vPos + 3
  else
    vPos := POSINISTR;

  vLen := Length(AURL) + POSINISTR;
  while (vPos < vLen) and (AURL[vPos] <> '/') do
    Inc(vPos);
  if vPos < vLen then
    Result := Copy(AURL, vPos, MaxInt);
end;

function SendAndFinish(AStream: HQUIC; const ABytes: TBytes): QUIC_STATUS;
var
  vCtx: PRALMsQuicSendCtx;
begin
  if Length(ABytes) = 0 then
  begin
    Result := MsQuicApi^.StreamShutdown(AStream,
      QUIC_STREAM_SHUTDOWN_FLAG_GRACEFUL, 0);
    Exit;
  end;
  New(vCtx);
  vCtx^.Data := ABytes;
  vCtx^.Buffer.Length := Length(vCtx^.Data);
  vCtx^.Buffer.Buffer := PByte(vCtx^.Data);
  Result := MsQuicApi^.StreamSend(AStream, @vCtx^.Buffer, 1,
    QUIC_SEND_FLAG_FIN, vCtx);
  if QUIC_FAILED(Result) then
    Dispose(vCtx);
end;

{ TRALMsQuicConnection }

constructor TRALMsQuicConnection.Create(const AKey: StringRAL;
  const AAlpn: AnsiString);
begin
  inherited Create;
  FKey := AKey;
  FAlpn := AAlpn;
  FRefCount := 1;
  FConnectedEvent := TEvent.Create(nil, True, False, '');
end;

destructor TRALMsQuicConnection.Destroy;
begin
  Close;
  { neither the configuration nor the registration are closed here: both are
    shared by the whole process and outlive any one connection }
  FConfiguration := nil;
  FConnectedEvent.Free;
  inherited Destroy;
end;

procedure TRALMsQuicConnection.AddRef;
begin
  RALAtomicInc(FRefCount);
end;

procedure TRALMsQuicConnection.Release;
begin
  if RALAtomicDec(FRefCount) = 0 then
    Free;
end;

procedure TRALMsQuicConnection.Close;
begin
  if FConnection <> nil then
  begin
    MsQuicApi^.ConnectionShutdown(FConnection,
      QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0);
    MsQuicApi^.ConnectionClose(FConnection);
    FConnection := nil;
  end;
  FConnected := False;
end;

procedure TRALMsQuicConnection.Open(const AHost: StringRAL; APort: IntegerRAL;
  AVerifyCertificate: boolean; AConnectTimeout, AKeepAlive: IntegerRAL);
var
  vHost: AnsiString;
  vStatus: QUIC_STATUS;
  vReg: HQUIC;
begin
  vReg := SharedRegistration;
  FConfiguration := SharedConfiguration(FAlpn, AVerifyCertificate,
                                        AConnectTimeout, AKeepAlive);

  FConnectStatus := QUIC_STATUS_CONNECTION_TIMEOUT;
  FConnected := False;
  FConnectedEvent.ResetEvent;

  vStatus := MsQuicApi^.ConnectionOpen(vReg, RALMsQuicClientConnCallback,
    Self, FConnection);
  if QUIC_FAILED(vStatus) then
    raise Exception.CreateFmt('MsQuic ConnectionOpen: %s',
      [QuicStatusToStr(vStatus)]);

  vHost := AnsiString(AHost);
  vStatus := MsQuicApi^.ConnectionStart(FConnection, FConfiguration,
    QUIC_ADDRESS_FAMILY_UNSPEC, PAnsiChar(vHost), Word(APort));
  if QUIC_FAILED(vStatus) then
    raise Exception.CreateFmt('MsQuic ConnectionStart: %s',
      [QuicStatusToStr(vStatus)]);

  if FConnectedEvent.WaitFor(AConnectTimeout) <> wrSignaled then
  begin
    Close;
    raise Exception.Create('QUIC handshake timed out');
  end;
  if not FConnected then
  begin
    vStatus := FConnectStatus;
    Close;
    raise Exception.CreateFmt('QUIC connect failed: %s',
      [QuicStatusToStr(vStatus)]);
  end;
end;

{ TRALMsQuicClientHTTP }

constructor TRALMsQuicClientHTTP.Create(AOwner: TRALClient);
begin
  inherited Create(AOwner);
  FAlpn := 'ralq1';
end;

destructor TRALMsQuicClientHTTP.Destroy;
begin
  if FSpare <> nil then
  begin
    FSpare.Release;
    FSpare := nil;
  end;
  DropShared;
  DropOwn;
  inherited Destroy;
end;

class function TRALMsQuicClientHTTP.EngineName: StringRAL;
begin
  Result := ENGINEMSQUIC;
end;

class function TRALMsQuicClientHTTP.EngineVersion: StringRAL;
begin
  Result := StringRAL(MsQuicVersionStr);
end;

class function TRALMsQuicClientHTTP.PackageDependency: StringRAL;
begin
  Result := 'msquic';
end;

class function TRALMsQuicClientHTTP.SupportsSharedConnection: boolean;
begin
  Result := True;
end;

{ QUIC has a keep-alive of its own - a PING frame on the interval - so this
  honours TRALClient.KeepAliveInterval the way the two HTTP/2 engines do, and
  without their condition: there is no HTTP version to be on here, because the
  connection is multiplexed and long lived from the first request. }
class function TRALMsQuicClientHTTP.SupportsKeepAliveInterval: boolean;
begin
  Result := True;
end;

function TRALMsQuicClientHTTP.AcquirePending: TRALMsQuicPending;
begin
  if (FSpare <> nil) and (FSpare.RefCount = 1) then
  begin
    { only this engine still holds it, so the callback chain is done with it }
    FSpare.Reset;
    RALAtomicInc(FSpare.RefCount);
    Result := FSpare;
    Exit;
  end;

  if FSpare <> nil then
  begin
    { a callback still holds the old one - let it go and start fresh }
    FSpare.Release;
    FSpare := nil;
  end;
  Result := TRALMsQuicPending.Create;
  FSpare := Result;
end;

function TRALMsQuicClientHTTP.ConnectionKey(const AHost: StringRAL;
  APort: IntegerRAL): StringRAL;
begin
  { Where it goes, under which ALPN, and with which certificate policy - the
    policy belongs in the key because a TLS connection carries the decision
    taken once at handshake time: two clients sharing one must judge a
    certificate the same way. }
  { the keep-alive interval is part of it: two clients that disagree on how
    often to prove the connection is alive must not end up on the same one }
  Result := Format('%s:%d|%s|%d|%s|%d', [AHost, APort, StringRAL(FAlpn),
    Ord(Parent.SSL.Verify), CertPolicyKey, Parent.KeepAliveInterval]);
end;

procedure TRALMsQuicClientHTTP.DropShared;
begin
  if FShared = nil then
    Exit;
  { THE POOL OWNS THE CONNECTION, and this method only gives back the sharer's
    reference. It used to remove the entry once the last sharer left, and that
    made ShareConnection unusable in the arrangement it exists for: with an
    engine created per request - which is what TRALClient hands every thread
    but the first - the last sharer leaves on EVERY request, so the connection
    was closed and the next request paid a full handshake, inside the pool lock,
    with every other thread queued behind it. The run did not finish. }
  FShared.Release;
  FShared := nil;
end;

procedure TRALMsQuicClientHTTP.DropOwn;
begin
  if FOwnConnection <> nil then
  begin
    FOwnConnection.Release;
    FOwnConnection := nil;
  end;
end;

function TRALMsQuicClientHTTP.PickConnection(const AURL: StringRAL): TRALMsQuicConnection;
var
  vKey: StringRAL;
  vIdx: IntegerRAL;
  vConn: TRALMsQuicConnection;
  vHost: StringRAL;
  vPort: IntegerRAL;
begin
  QuicHostPort(AURL, vHost, vPort);
  if vPort <= 0 then
    vPort := 4710;
  vKey := ConnectionKey(vHost, vPort);

  if not Parent.ShareConnection then
  begin
    { hand the borrowed one back before returning to its own: the configuration
      may have changed midway through this client's life }
    DropShared;
    if (FOwnConnection <> nil) and
       ((FOwnConnection.Key <> vKey) or (not FOwnConnection.Connected)) then
      DropOwn;
    if FOwnConnection = nil then
    begin
      FOwnConnection := TRALMsQuicConnection.Create(vKey, FAlpn);
      FOwnConnection.Open(vHost, vPort, Parent.SSL.Verify <> svNever,
        Parent.ConnectTimeout, Parent.KeepAliveInterval);
    end;
    Result := FOwnConnection;
    Exit;
  end;

  DropOwn;
  if (FShared <> nil) and (FShared.Key = vKey) and FShared.Connected then
  begin
    Result := FShared;
    Exit;
  end;
  DropShared;

  { Opened INSIDE the lock on purpose: whoever arrives second has to find a
    connection already through its handshake, not one still negotiating. }
  vPoolLock.Enter;
  try
    vIdx := vPool.IndexOf(vKey);
    if (vIdx >= 0) and TRALMsQuicConnection(vPool.Objects[vIdx]).Connected then
    begin
      vConn := TRALMsQuicConnection(vPool.Objects[vIdx]);
      vConn.AddRef;
    end
    else
    begin
      if vIdx >= 0 then
      begin
        { a dead entry still holds the pool's own reference - dropping the slot
          without giving it back leaks the connection and its handle }
        TRALMsQuicConnection(vPool.Objects[vIdx]).Release;
        vPool.Delete(vIdx);
      end;
      vConn := TRALMsQuicConnection.Create(vKey, FAlpn);
      try
        vConn.Open(vHost, vPort, Parent.SSL.Verify <> svNever,
          Parent.ConnectTimeout, Parent.KeepAliveInterval);
      except
        vConn.Free;
        raise;
      end;
      vConn.AddRef;
      vPool.AddObject(vKey, vConn);
    end;
  finally
    vPoolLock.Leave;
  end;

  FShared := vConn;
  Result := FShared;
end;


procedure TRALMsQuicClientHTTP.PrepareRequest(ARequest: TRALRequest);
begin
  ARequest.Params.AddParam('User-Agent', Parent.UserAgent, rpkHEADER);
  ARequest.ContentCompress := Parent.CompressType;
  { Accept-Encoding states what the client can READ, which does not depend on
    whether it compresses what it sends - so it is outside the CompressType
    check, the same rule every other engine follows. }
  ARequest.Params.AddParam('Accept-Encoding', GetAcceptCompress, rpkHEADER);
  ARequest.CriptoKey := Parent.CriptoOptions.Key;
  ARequest.ContentCripto := Parent.CriptoOptions.CriptType;
  if Parent.CriptoOptions.CriptType <> crNone then
  begin
    ARequest.Params.AddParam('Content-Encription', ARequest.ContentEncription, rpkHEADER);
    ARequest.Params.AddParam('Accept-Encription', SupportedEncriptKind, rpkHEADER);
  end;
end;
function TRALMsQuicClientHTTP.BuildFrame(ARequest: TRALRequest;
  AMethod: TRALMethod; const ARoute: StringRAL): TBytes;
var
  vBody: StringRAL;
  vHeaders: TRALMsQuicHeaders;
  vHdrCount, vHdrSize: IntegerRAL;
  vSource: TStream;
  vDest: PByte;
begin
  { THE BODY IS ENCODED FIRST, and the headers are built afterwards. Not a
    style choice: RequestStream runs EncodeBody, which decides between a raw
    body and multipart and WRITES BACK ContentType - with the boundary - and
    ContentEncoding, with what it actually compressed. Reading either before
    sends a multipart request with no boundary and a gzipped body with no
    Content-Encoding, which is exactly what the round-trip battery caught:
    every multipart case came back empty and every compressed case came back
    unreadable. TRALSynopseClientHTTP orders it the same way. }
  vBody := '';
  vSource := ARequest.RequestStream;
  try
    if (vSource <> nil) and (vSource.Size > 0) then
    begin
      SetLength(vBody, vSource.Size);
      vSource.Position := 0;
      vSource.ReadBuffer(vBody[POSINISTR], vSource.Size);
    end;
  finally
    FreeAndNil(vSource);
  end;

  if ARequest.ContentType <> '' then
    ARequest.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
  if ARequest.ContentDisposition <> '' then
    ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);
  if ARequest.ContentCompress <> ctNone then
    ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);

  vHdrSize := CollectHeaders(ARequest.Params, vHeaders, vHdrCount);

  { one allocation, sized up front - the TMemoryStream that used to grow here
    and then be copied out was two more allocations and a full copy per
    request }
  SetLength(Result, 1 + BlockSize(Length(ARoute)) + vHdrSize +
                    BlockSize(Length(vBody)));
  vDest := PByte(Result);
  vDest^ := Ord(AMethod);
  Inc(vDest);
  vDest := PutBlockStr(vDest, ARoute);
  vDest := PutHeaders(vDest, vHeaders, vHdrCount);
  PutBlockStr(vDest, vBody);
end;

procedure TRALMsQuicClientHTTP.ParseFrame(AFrame: PByte; ASize: IntegerRAL;
  AResponse: TRALResponse);
var
  vPos: IntegerRAL;
  vStatus: Word;
  vContentType, vBody: StringRAL;
  {$IFDEF RALMSQUIC_PROFILE}vPMark: Int64;{$ENDIF}
begin
  if ASize < 2 then
  begin
    SetTransportError(AResponse, rteOther, -1, 'truncated QUIC response frame');
    Exit;
  end;
  Move(AFrame^, vStatus, 2);
  vPos := 2;
  if (not ReadBlockStr(AFrame, ASize, vPos, vContentType)) or
     (not ReadHeaders(AFrame, ASize, vPos, AResponse.Params)) or
     (not ReadBlockStr(AFrame, ASize, vPos, vBody)) then
  begin
    SetTransportError(AResponse, rteOther, -1, 'malformed QUIC response frame');
    Exit;
  end;

  {$IFDEF RALMSQUIC_PROFILE}vPMark := ProfTicks;{$ENDIF}
  {$IFDEF RALMSQUIC_PROFILE}ProfMark(qpParseHdr, vPMark);{$ENDIF}

  AResponse.ContentEncoding := AResponse.ParamByName('Content-Encoding').AsString;
  AResponse.Params.CompressType := AResponse.ContentCompress;

  AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
  AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
  AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;

  AResponse.ContentType := vContentType;
  AResponse.ContentDisposition := AResponse.ParamByName('Content-Disposition').AsString;
  AResponse.StatusCode := vStatus;
  { there is no status line to quote: QUIC is the protocol, and it is not a
    version of HTTP }
  AResponse.Protocol := 'QUIC';
  {$IFDEF RALMSQUIC_PROFILE}ProfMark(qpParseProps, vPMark);{$ENDIF}
  AResponse.ResponseText := vBody;
  {$IFDEF RALMSQUIC_PROFILE}ProfMark(qpParseBody, vPMark);{$ENDIF}
end;

procedure TRALMsQuicClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vConn: TRALMsQuicConnection;
  vPending: TRALMsQuicPending;
  vFrame, vAnswer: TBytes;
  vStatus: QUIC_STATUS;
  vRoute: StringRAL;
  vLoadStatus: QUIC_STATUS;
  {$IFDEF RALMSQUIC_PROFILE}vMark: Int64;{$ENDIF}
begin
  {$IFDEF RALMSQUIC_PROFILE}vMark := ProfTicks;{$ENDIF}
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINEMSQUIC);

  vLoadStatus := MsQuicLoad(FLibPath);
  if QUIC_FAILED(vLoadStatus) then
  begin
    SetTransportError(AResponse, rteOther, -1, MsQuicLoadError);
    Exit;
  end;
  { MsQuic creates its worker threads inside the C library, so BeginThread
    never runs and the flag the memory manager reads to decide whether to lock
    would stay False - see TRALMsQuicServer for the full note. }
  IsMultiThread := True;

  PrepareRequest(ARequest);

  vRoute := RouteFromUrl(AURL);
  {$IFDEF RALMSQUIC_PROFILE}ProfMark(qpSetup, vMark);{$ENDIF}

  try
    vConn := PickConnection(AURL);
  except
    on e: Exception do
    begin
      { nothing was delivered, so another BaseURL may be tried with any method }
      SetTransportError(AResponse, rteConnect, 0, e.Message);
      Exit;
    end;
  end;

  {$IFDEF RALMSQUIC_PROFILE}ProfMark(qpConnect, vMark);{$ENDIF}

  vFrame := BuildFrame(ARequest, AMethod, vRoute);
  {$IFDEF RALMSQUIC_PROFILE}ProfMark(qpBuild, vMark);{$ENDIF}

  { the engine keeps its reference to the context between requests - see
    AcquirePending - so nothing is released here on the normal path }
  vPending := AcquirePending;
  try
    vStatus := MsQuicApi^.StreamOpen(vConn.Handle, QUIC_STREAM_OPEN_FLAG_NONE,
      RALMsQuicClientStreamCallback, vPending, vPending.Stream);
    if QUIC_FAILED(vStatus) then
    begin
      { no stream was opened, so no callback chain will ever give back the
        reference it was counted for: hand that one back here. The engine
        keeps its own, and the context stays reusable. }
      vPending.Release;
      SetTransportError(AResponse, rteConnect, 0, 'StreamOpen: ' +
        QuicStatusToStr(vStatus));
      Exit;
    end;

    { SHUTDOWN_ON_FAIL guarantees a SHUTDOWN_COMPLETE even when the start
      fails, and that event is the only thing that releases the callback's
      reference. }
    vStatus := MsQuicApi^.StreamStart(vPending.Stream,
      QUIC_STREAM_START_FLAG_NONE or QUIC_STREAM_START_FLAG_SHUTDOWN_ON_FAIL);
    if QUIC_FAILED(vStatus) then
    begin
      SetTransportError(AResponse, rteConnect, 0, 'StreamStart: ' +
        QuicStatusToStr(vStatus));
      Exit;
    end;

    vStatus := SendAndFinish(vPending.Stream, vFrame);
    if QUIC_FAILED(vStatus) then
    begin
      MsQuicApi^.StreamShutdown(vPending.Stream,
        QUIC_STREAM_SHUTDOWN_FLAG_ABORT or QUIC_STREAM_SHUTDOWN_FLAG_IMMEDIATE, 0);
      SetTransportError(AResponse, rteConnect, 0, 'StreamSend: ' +
        QuicStatusToStr(vStatus));
      Exit;
    end;

    {$IFDEF RALMSQUIC_PROFILE}ProfMark(qpStart, vMark);{$ENDIF}

    if vPending.Done.WaitFor(Parent.RequestTimeout) <> wrSignaled then
    begin
      MsQuicApi^.StreamShutdown(vPending.Stream,
        QUIC_STREAM_SHUTDOWN_FLAG_ABORT or QUIC_STREAM_SHUTDOWN_FLAG_IMMEDIATE, 0);
      { the request went out and the server may have run it, so only an
        idempotent method may be replayed elsewhere - which is what rteTimeout
        means to CanSwitchURL }
      SetTransportError(AResponse, rteTimeout, 0, 'timed out waiting for the answer');
      Exit;
    end;

    {$IFDEF RALMSQUIC_PROFILE}ProfMark(qpWait, vMark);{$ENDIF}

    if vPending.Failed then
    begin
      SetTransportError(AResponse, rteOther, 0, 'the peer aborted the stream');
      Exit;
    end;

    { parsed straight out of the accumulator: Position is the length written,
      and Size is only the high water mark of the buffer being reused }
    ParseFrame(PByte(vPending.Received.Memory), vPending.Received.Position,
      AResponse);
    {$IFDEF RALMSQUIC_PROFILE}
    ProfMark(qpParse, vMark);
    RALAtomicInc(gPhaseReqs, 1);
    {$ENDIF}
  finally
    { deliberately NOT released here: the engine keeps the context for the
      next request (see AcquirePending), and the callback chain gives its own
      reference back on SHUTDOWN_COMPLETE }
  end;
end;

initialization
  {$IFDEF RALMSQUIC_PROFILE}
  {$IFDEF RALWindows}QueryPerformanceFrequency(gPhaseFreq);{$ENDIF}
  {$ENDIF}
  vPool := TStringList.Create;
  vPool.Sorted := True;
  vPoolLock := TCriticalSection.Create;
  vConfigs := TStringList.Create;
  vConfigs.Sorted := True;
  vGlobalLock := TCriticalSection.Create;
  RegisterEngine(TRALMsQuicClientHTTP);

finalization
  { Order matters and it is MsQuic's, not Pascal's: connections first, then the
    configurations they were opened with, then the registration that owns the
    threads running them. Closing the registration first would be closing the
    ground under the callbacks. }
  if vPool <> nil then
  begin
    while vPool.Count > 0 do
    begin
      TRALMsQuicConnection(vPool.Objects[0]).Release;
      vPool.Delete(0);
    end;
    FreeAndNil(vPool);
  end;
  if (vConfigs <> nil) and MsQuicIsLoaded then
    while vConfigs.Count > 0 do
    begin
      MsQuicApi^.ConfigurationClose(HQUIC(vConfigs.Objects[0]));
      vConfigs.Delete(0);
    end;
  FreeAndNil(vConfigs);
  if (vRegistration <> nil) and MsQuicIsLoaded then
  begin
    MsQuicApi^.RegistrationClose(vRegistration);
    vRegistration := nil;
  end;
  FreeAndNil(vPoolLock);
  FreeAndNil(vGlobalLock);

end.
