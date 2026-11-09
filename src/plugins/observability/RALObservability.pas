/// Observability plugins: a log line per request (TRALLogPlugin), the W3C trace
/// context (TRALTracePlugin), Prometheus metrics (TRALMetricsPlugin) and health
/// routes (TRALHealthPlugin). Each one is a component linked to the server; none
/// of them changes the answer of a route.
unit RALObservability;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALPlugin, RALRequest, RALResponse,
  RALRoutes, RALJson, RALClient, RALParams;

const
  /// Priority of the log: before every other plugin, so it sees every request.
  RALPriorityLog = 990;
  /// Priority of the trace: right after the log, so the log line has the trace id.
  RALPriorityTrace = 985;

type
  /// Format of a log line.
  TRALLogFormat = (
    /// Apache common: host ident user [date] "request" status bytes
    lfCommon,
    /// Apache combined: common plus "referer" "user agent"
    lfCombined,
    /// One JSON object per line, for Loki, ELK and the like
    lfJSON);

  /// A log line is ready; ALine is the text of the format, without the line break.
  TRALOnLogLine = procedure(Sender: TObject; ARequest: TRALRequest;
    const ALine: StringRAL) of object;

  { What a log line needs from a request, taken when the request arrives: the
    server clears the request params before the answer goes out. }
  TRALLogSnapshot = class
  public
    /// The decoded request body, up to MaxBodyLog bytes (LogBodies).
    Body: StringRAL;
    /// The request headers as a JSON object, redacted (LogHeaders).
    Headers: StringRAL;
    /// The Referer header.
    Referer: StringRAL;
    /// The authenticated user, or '-'.
    User: StringRAL;
    /// The User-Agent header.
    UserAgent: StringRAL;
  end;

  { Where the lines of a TRALLogPlugin go (the OnLog event, the console, a file
    with rotation), and its settings. Counted by reference: a request finishes
    after the plugin may have gone (a form frees its components in any order),
    and the finish handler is a method of the sink, not of the plugin. }
  TRALLogSink = class
  private
    FConsole: boolean;
    FFileName: string;
    FFormat: TRALLogFormat;
    FLock: TCriticalSection;
    FLogBodies: boolean;
    FLogHeaders: boolean;
    FMaxBodyLog: IntegerRAL;
    FMaxFiles: IntegerRAL;
    FMaxSize: Int64RAL;
    FOnLog: TRALOnLogLine;
    FOwner: TObject;
    FRedact: TStringList;
    FReferences: IntegerRAL;
    FRotateDaily: boolean;
    FSnapshots: TStringList;
    FStream: TFileStream;
    FStreamDay: IntegerRAL;

    /// Opens (or rotates) the file for a line written now.
    procedure OpenFile;
    /// Moves name.log to name.1.log and so on, keeping MaxFiles.
    procedure Rotate;
    /// Takes the snapshot of ARequest out of the list, or nil.
    function TakeSnapshot(ARequest: TRALRequest): TRALLogSnapshot;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;

    /// One more user; Release drops it and the last frees the sink.
    procedure AddRef;
    /// The line of a finished request; ASnapshot is what the request carried.
    function FormatLine(ARequest: TRALRequest; ASnapshot: TRALLogSnapshot): StringRAL;
    /// Keeps what the line of ARequest needs; called when the request arrives.
    procedure Keep(ARequest: TRALRequest);
    /// Adds the decoded body to the snapshot of ARequest (LogBodies).
    procedure KeepBody(ARequest: TRALRequest);
    /// Drops a user; the last one frees the sink.
    procedure Release;
    /// The finish handler: writes the line and drops the reference of the request.
    procedure RequestDone(ARequest: TRALRequest);
    /// Writes ALine (the event, the console, the file).
    procedure Write(ARequest: TRALRequest; const ALine: StringRAL);
  end;

  { One line per request, written when the answer is ready (status, bytes and
    duration known). Headers named in RedactHeaders appear as '***'. }
  TRALLogPlugin = class(TRALPlugin)
  private
    FRedactHeaders: TStrings;
    FSink: TRALLogSink;

    function GetConsole: boolean;
    function GetFileName: string;
    function GetFormat: TRALLogFormat;
    function GetLogBodies: boolean;
    function GetLogHeaders: boolean;
    function GetMaxBodyLog: IntegerRAL;
    function GetMaxFiles: IntegerRAL;
    function GetMaxFileSize: Int64RAL;
    function GetOnLog: TRALOnLogLine;
    function GetRotateDaily: boolean;
    procedure RedactChanged(Sender: TObject);
    procedure SetConsole(const AValue: boolean);
    procedure SetFileName(const AValue: string);
    procedure SetFormat(const AValue: TRALLogFormat);
    procedure SetLogBodies(const AValue: boolean);
    procedure SetLogHeaders(const AValue: boolean);
    procedure SetMaxBodyLog(const AValue: IntegerRAL);
    procedure SetMaxFiles(const AValue: IntegerRAL);
    procedure SetMaxFileSize(const AValue: Int64RAL);
    procedure SetOnLog(const AValue: TRALOnLogLine);
    procedure SetRedactHeaders(const AValue: TStrings);
    procedure SetRotateDaily(const AValue: boolean);
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    /// Writes each line to the console (standard output) too.
    property Console: boolean read GetConsole write SetConsole default False;
    /// File the lines go to; empty = none. Rotated by MaxFileSize and RotateDaily.
    property FileName: string read GetFileName write SetFileName;
    /// Format of the lines.
    property Format: TRALLogFormat read GetFormat write SetFormat default lfCommon;
    /// JSON lines carry the request body, up to MaxBodyLog bytes.
    property LogBodies: boolean read GetLogBodies write SetLogBodies default False;
    /// JSON lines carry the request headers (RedactHeaders hidden).
    property LogHeaders: boolean read GetLogHeaders write SetLogHeaders default False;
    /// Most bytes of a body LogBodies writes.
    property MaxBodyLog: IntegerRAL read GetMaxBodyLog write SetMaxBodyLog default 1024;
    /// Rotated files kept (name.1.log ...).
    property MaxFiles: IntegerRAL read GetMaxFiles write SetMaxFiles default 7;
    /// Bytes from which the file rotates; 0 = no limit.
    property MaxFileSize: Int64RAL read GetMaxFileSize write SetMaxFileSize;
    /// Fired for each line, on the thread that served the request.
    property OnLog: TRALOnLogLine read GetOnLog write SetOnLog;
    /// Headers written as '***'.
    property RedactHeaders: TStrings read FRedactHeaders write SetRedactHeaders;
    /// Rotates the file when the day changes.
    property RotateDaily: boolean read GetRotateDaily write SetRotateDaily default False;
  end;

  /// A finished span of TRALTracePlugin.
  TRALTraceSpan = record
    /// Microseconds since the Unix epoch.
    EndUnixMicro: Int64RAL;
    Method: StringRAL;
    Name: StringRAL;
    ParentSpanId: StringRAL;
    SpanId: StringRAL;
    /// Microseconds since the Unix epoch.
    StartUnixMicro: Int64RAL;
    StatusCode: IntegerRAL;
    TraceId: StringRAL;
  end;

  /// A span of a request ended (ASpan is the server span).
  TRALOnTraceSpan = procedure(Sender: TObject; const ASpan: TRALTraceSpan) of object;

  { The spans of a TRALTracePlugin: those in flight, by request, and those
    waiting for the export. Counted by reference like TRALLogSink, and the
    finish handler is its method. }
  TRALTraceCollector = class
  private
    FExport: boolean;
    FLock: TCriticalSection;
    FOnSpan: TRALOnTraceSpan;
    FOwner: TObject;
    FPending: TList;
    FReferences: IntegerRAL;
    FStates: TStringList;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;

    /// One more user; Release drops it and the last frees the collector.
    procedure AddRef;
    /// Keeps the span AObject (a TRALTraceState) of ARequest until it finishes.
    procedure Keep(ARequest: TRALRequest; AObject: TObject);
    /// Drops a user; the last one frees the collector.
    procedure Release;
    /// The finish handler: ends the span of ARequest and drops its reference.
    procedure RequestDone(ARequest: TRALRequest);
    /// The spans waiting for the export (the caller frees the list and them).
    function TakePending: TList;
  end;

  { The W3C trace context (traceparent, tracestate): reads it from the request
    or starts a trace, puts the trace id in TRALRequest.TraceId (the log and the
    problem details carry it), answers traceresponse, and makes a TRALClient
    called from the handler carry the trace on. With ExportURL, the spans go to
    an OpenTelemetry collector (OTLP/HTTP, JSON) in batches, from a thread. }
  TRALTracePlugin = class(TRALPlugin)
  private
    FCollector: TRALTraceCollector;
    FExportEngine: StringRAL;
    FExporter: TThread;
    FExportInterval: IntegerRAL;
    FExportURL: StringRAL;
    FServiceName: StringRAL;

    function GetOnSpan: TRALOnTraceSpan;
    procedure SetExportURL(const AValue: StringRAL);
    procedure SetOnSpan(const AValue: TRALOnTraceSpan);
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Sends the pending spans to ExportURL now; the thread calls it every ExportInterval.
    procedure Flush;
    procedure ServerActivating; override;
    procedure ServerDeactivating; override;
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    /// Engine of the TRALClient that exports (empty = the first registered).
    property ExportEngine: StringRAL read FExportEngine write FExportEngine;
    /// Milliseconds between two exports.
    property ExportInterval: IntegerRAL read FExportInterval write FExportInterval default 5000;
    { OTLP/HTTP endpoint of a collector ('http://collector:4318/v1/traces');
      empty = no export (OnSpan only). }
    property ExportURL: StringRAL read FExportURL write SetExportURL;
    /// Fired for each finished server span, on the thread that served the request.
    property OnSpan: TRALOnTraceSpan read GetOnSpan write SetOnSpan;
    /// service.name of the exported spans.
    property ServiceName: StringRAL read FServiceName write FServiceName;
  end;

  { The numbers of a TRALMetricsPlugin, counted by reference like TRALLogSink:
    requests by method, route and status, the duration histogram by method and
    route, and the requests in flight. }
  TRALMetricsRegistry = class
  private
    FBuckets: array of Double;
    FCounts: TStringList;
    FDurations: TStringList;
    FInFlight: IntegerRAL;
    FLock: TCriticalSection;
    FReferences: IntegerRAL;
  public
    constructor Create;
    destructor Destroy; override;

    /// One more user; Release drops it and the last frees the registry.
    procedure AddRef;
    /// Counts a finished request of AMethod on ARoute with AStatus, in ASeconds.
    procedure Done(const AMethod, ARoute: StringRAL; AStatus: IntegerRAL; ASeconds: Double);
    /// Drops a user; the last one frees the registry.
    procedure Release;
    /// The metrics in the Prometheus text format 0.0.4.
    function Render: StringRAL;
    /// The finish handler: counts the request and drops its reference.
    procedure RequestDone(ARequest: TRALRequest);
    /// One more request in flight.
    procedure Started;
  end;

  /// Lines a TRALMetricsPlugin adds to its output (each a full sample or a # comment).
  TRALOnMetricsCollect = procedure(Sender: TObject; AOutput: TStrings) of object;

  /// Prometheus metrics at Path (GET, public): ral_http_requests_total by method,
  /// route and status, ral_http_request_duration_seconds by method and route
  /// (histogram) and ral_http_requests_in_flight. The route label is the route's
  /// path (/users/:id), never the URL, so the number of series stays small.
  TRALMetricsPlugin = class(TRALPlugin)
  private
    FOnCollect: TRALOnMetricsCollect;
    FPath: StringRAL;
    FRegistry: TRALMetricsRegistry;
    FRoutes: TRALRoutes;

    procedure SetPath(const AValue: StringRAL);
  protected
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
    /// The text /metrics answers.
    function Render: StringRAL;
    function ResolveRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute; override;
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    /// Adds lines of the application's own metrics.
    property OnCollect: TRALOnMetricsCollect read FOnCollect write FOnCollect;
    /// Path of the metrics.
    property Path: StringRAL read FPath write SetPath;
  end;

  TRALHealthCheck = class;

  /// Runs a check: set AHealthy (True by default) and an AOutput that says why.
  TRALOnHealthCheck = procedure(ACheck: TRALHealthCheck; var AHealthy: boolean;
    var AOutput: StringRAL) of object;

  /// One check of /health/ready.
  TRALHealthCheck = class(TCollectionItem)
  private
    FName: StringRAL;
    FOnCheck: TRALOnHealthCheck;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  published
    /// Name of the check in the answer ('database').
    property Name: StringRAL read FName write FName;
    /// Runs the check.
    property OnCheck: TRALOnHealthCheck read FOnCheck write FOnCheck;
  end;

  /// The checks of a TRALHealthPlugin.
  TRALHealthChecks = class(TOwnedCollection)
  public
    /// Adds a check named AName.
    function AddCheck(const AName: StringRAL; AOnCheck: TRALOnHealthCheck): TRALHealthCheck;
  end;

  { Health routes, public: LivePath answers 200 while the process answers at
    all; ReadyPath runs Checks and answers 200 when every one passes, 503
    otherwise, in the health check response format (application/health+json). }
  TRALHealthPlugin = class(TRALPlugin)
  private
    FChecks: TRALHealthChecks;
    FLivePath: StringRAL;
    FReadyPath: StringRAL;
    FRoutes: TRALRoutes;

    /// Builds the routes of LivePath and ReadyPath.
    procedure Rebuild;
    procedure SetChecks(const AValue: TRALHealthChecks);
    procedure SetLivePath(const AValue: StringRAL);
    procedure SetReadyPath(const AValue: StringRAL);
  protected
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
    /// Runs the checks: the health+json body, and whether all passed.
    function Ready(out ABody: StringRAL): boolean;
    function ResolveRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute; override;
  published
    /// The checks of ReadyPath.
    property Checks: TRALHealthChecks read FChecks write SetChecks;
    /// Path of the liveness route.
    property LivePath: StringRAL read FLivePath write SetLivePath;
    /// Path of the readiness route.
    property ReadyPath: StringRAL read FReadyPath write SetReadyPath;
  end;

/// A random lower-case hexadecimal text of ABytes bytes (trace and span ids).
function RALRandomHex(ABytes: IntegerRAL): StringRAL;

implementation

{$IFNDEF FPC}
type
  PtrUInt = NativeUInt;
{$ENDIF}

const
  cHexDigits = '0123456789abcdef';

function RALRandomHex(ABytes: IntegerRAL): StringRAL;
var
  vBytes: TBytes;
  vInt: IntegerRAL;
begin
  vBytes := RandomBytes(ABytes);
  Result := '';
  for vInt := 0 to High(vBytes) do
    Result := Result + Copy(cHexDigits, (vBytes[vInt] shr 4) + 1, 1) +
      Copy(cHexDigits, (vBytes[vInt] and 15) + 1, 1);
end;

/// Header AName of ARequest, or ''.
function HeaderOf(ARequest: TRALRequest; const AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := ARequest.Params.GetKind[AName, rpkHEADER];
  if vParam <> nil then
    Result := vParam.AsString;
end;

/// The path of the route of ARequest ('/users/:id'), or 'unmatched'.
function RouteLabel(ARequest: TRALRequest): StringRAL;
begin
  if ARequest.Route is TRALBaseRoute then
    Result := TRALBaseRoute(ARequest.Route).GetFullRoute
  else
    Result := 'unmatched';
end;

/// The user a request claims: the Basic user name, or '-'.
function UserOf(ARequest: TRALRequest): StringRAL;
begin
  Result := '-';
  if (ARequest.Authorization <> nil) and (ARequest.Authorization.AuthType = ratBasic) and
     (ARequest.Authorization.AsAuthBasic <> nil) and
     (ARequest.Authorization.AsAuthBasic.UserName <> '') then
    Result := ARequest.Authorization.AsAuthBasic.UserName;
end;

/// The HTTP version of ARequest ('1.1' when the engine did not say).
function ProtocolOf(ARequest: TRALRequest): StringRAL;
begin
  Result := ARequest.Protocol;
  if Result = '' then
    Result := '1.1';
end;

/// Microseconds since the Unix epoch, now.
function UnixMicroNow: Int64RAL;
begin
  Result := MilliSecondsBetween(RALDateTimeToGMT(Now), UnixDateDelta) * 1000;
end;

{ TRALLogSink }

constructor TRALLogSink.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  FLock := TCriticalSection.Create;
  FReferences := 1;
  FMaxFiles := 7;
  FStreamDay := -1;
  FFormat := lfCommon;
  FMaxBodyLog := 1024;
  FRedact := TStringList.Create;
  FRedact.CaseSensitive := False;
  FSnapshots := TStringList.Create;
  FSnapshots.Sorted := True;
  FSnapshots.OwnsObjects := True;
end;

destructor TRALLogSink.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FSnapshots);
  FreeAndNil(FRedact);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALLogSink.AddRef;
begin
  RALAtomicInc(FReferences);
end;

procedure TRALLogSink.Release;
begin
  if RALAtomicDec(FReferences) = 0 then
    Free;
end;

procedure TRALLogSink.Keep(ARequest: TRALRequest);
var
  vSnap: TRALLogSnapshot;
  vHeaders: TRALJSONObject;
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  vSnap := TRALLogSnapshot.Create;
  vSnap.UserAgent := HeaderOf(ARequest, 'User-Agent');
  if vSnap.UserAgent = '' then
    vSnap.UserAgent := ARequest.ClientInfo.UserAgent;
  vSnap.Referer := HeaderOf(ARequest, 'Referer');
  vSnap.User := UserOf(ARequest);
  if FLogHeaders then
  begin
    vHeaders := TRALJSONObject.Create;
    try
      for vInt := 0 to ARequest.Params.Count - 1 do
      begin
        vParam := ARequest.Params.Index[vInt];
        if vParam.Kind <> rpkHEADER then
          Continue;
        if FRedact.IndexOf(string(vParam.ParamName)) >= 0 then
          vHeaders.Add(vParam.ParamName, '***')
        else
          vHeaders.Add(vParam.ParamName, vParam.AsString);
      end;
      vSnap.Headers := vHeaders.ToJSON;
    finally
      vHeaders.Free;
    end;
  end;
  FLock.Enter;
  try
    FSnapshots.AddObject(IntToHex(PtrUInt(ARequest), 16), vSnap);
  finally
    FLock.Leave;
  end;
end;

procedure TRALLogSink.KeepBody(ARequest: TRALRequest);
var
  vIdx: Integer;
begin
  FLock.Enter;
  try
    if FSnapshots.Find(IntToHex(PtrUInt(ARequest), 16), vIdx) then
      TRALLogSnapshot(FSnapshots.Objects[vIdx]).Body := Copy(ARequest.RequestText, 1, FMaxBodyLog);
  finally
    FLock.Leave;
  end;
end;

function TRALLogSink.TakeSnapshot(ARequest: TRALRequest): TRALLogSnapshot;
var
  vIdx: Integer;
begin
  Result := nil;
  FLock.Enter;
  try
    if FSnapshots.Find(IntToHex(PtrUInt(ARequest), 16), vIdx) then
    begin
      Result := TRALLogSnapshot(FSnapshots.Objects[vIdx]);
      FSnapshots.Objects[vIdx] := nil;
      FSnapshots.Delete(vIdx);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TRALLogSink.RequestDone(ARequest: TRALRequest);
var
  vSnap: TRALLogSnapshot;
begin
  try
    vSnap := TakeSnapshot(ARequest);
    try
      if FOwner <> nil then
        Write(ARequest, FormatLine(ARequest, vSnap));
    finally
      vSnap.Free;
    end;
  finally
    Release;
  end;
end;

procedure TRALLogSink.Rotate;
var
  vInt: IntegerRAL;
  vBase, vExt: string;

  function NameOf(AIndex: IntegerRAL): string;
  begin
    if AIndex = 0 then
      Result := FFileName
    else
      Result := vBase + '.' + IntToStr(AIndex) + vExt;
  end;

begin
  FreeAndNil(FStream);
  vExt := ExtractFileExt(FFileName);
  vBase := Copy(FFileName, 1, Length(FFileName) - Length(vExt));
  if FileExists(NameOf(FMaxFiles)) then
    DeleteFile(NameOf(FMaxFiles));
  for vInt := FMaxFiles - 1 downto 0 do
    if FileExists(NameOf(vInt)) then
      RenameFile(NameOf(vInt), NameOf(vInt + 1));
end;

procedure TRALLogSink.OpenFile;
var
  vToday: IntegerRAL;
begin
  vToday := Trunc(Date);
  if (FStream <> nil) and FRotateDaily and (FStreamDay <> vToday) then
    Rotate
  else if (FStream <> nil) and (FMaxSize > 0) and (FStream.Size >= FMaxSize) then
    Rotate;
  if FStream = nil then
  begin
    if FileExists(FFileName) then
    begin
      FStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareDenyWrite);
      FStream.Seek(0, soEnd);
    end
    else
    begin
      ForceDirectories(ExtractFilePath(ExpandFileName(FFileName)));
      FStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyWrite);
    end;
    FStreamDay := vToday;
  end;
end;

procedure TRALLogSink.Write(ARequest: TRALRequest; const ALine: StringRAL);
var
  vText: StringRAL;
begin
  FLock.Enter;
  try
    if Assigned(FOnLog) then
      FOnLog(FOwner, ARequest, ALine);
    if FConsole then
      WriteLn(string(ALine));
    if FFileName <> '' then
    begin
      OpenFile;
      vText := ALine + #10;
      FStream.WriteBuffer(Pointer(vText)^, Length(vText));
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALLogSink.FormatLine(ARequest: TRALRequest; ASnapshot: TRALLogSnapshot): StringRAL;
const
  cMonths: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
    'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  vJson: TRALJSONObject;
  vNow: TDateTime;
  vBytes: StringRAL;
  vMs: Double;
  vUser, vAgent, vReferer: StringRAL;
begin
  vNow := RALDateTimeToGMT(Now);
  if ARequest.BytesSent >= 0 then
    vBytes := StringRAL(IntToStr(ARequest.BytesSent))
  else
    vBytes := '-';
  vMs := 0;
  if ARequest.StartTick > 0 then
    vMs := (RALTickMicro - ARequest.StartTick) / 1000;
  vUser := '-';
  vAgent := '';
  vReferer := '';
  if ASnapshot <> nil then
  begin
    vUser := ASnapshot.User;
    vAgent := ASnapshot.UserAgent;
    vReferer := ASnapshot.Referer;
  end;
  case FFormat of
    lfCommon, lfCombined:
      begin
        Result := ARequest.ClientInfo.IP + ' - ' + vUser + ' [' +
          StringRAL(FormatDateTime('dd', vNow) + '/' + cMonths[MonthOf(vNow)] + '/' +
          FormatDateTime('yyyy:hh:nn:ss', vNow)) + ' +0000] "' +
          RALMethodToHTTPMethod(ARequest.Method) + ' ' + ARequest.Query + ' HTTP/' +
          ProtocolOf(ARequest) + '" ' + StringRAL(IntToStr(ARequest.ResponseStatus)) + ' ' + vBytes;
        if FFormat = lfCombined then
        begin
          // an absent header is '-', as Apache writes it
          if vReferer = '' then
            vReferer := '-';
          if vAgent = '' then
            vAgent := '-';
          Result := Result + ' "' + vReferer + '" "' + vAgent + '"';
        end;
      end;
  else
    vJson := TRALJSONObject.Create;
    try
      vJson.Add('time', RALDateTimeToISO8601(vNow, True));
      vJson.Add('remote', ARequest.ClientInfo.IP);
      vJson.Add('method', RALMethodToHTTPMethod(ARequest.Method));
      vJson.Add('path', ARequest.Query);
      vJson.Add('route', RouteLabel(ARequest));
      vJson.Add('status', ARequest.ResponseStatus);
      vJson.Add('bytes', ARequest.BytesSent);
      vJson.Add('duration_ms', Round(vMs * 1000) / 1000);
      vJson.Add('user', vUser);
      vJson.Add('user_agent', vAgent);
      if ARequest.TraceId <> '' then
        vJson.Add('trace_id', ARequest.TraceId);
      Result := vJson.ToJSON;
      // the headers and the body go in as they were kept, already JSON
      if (ASnapshot <> nil) and FLogHeaders and (ASnapshot.Headers <> '') then
        Result := Copy(Result, 1, Length(Result) - 1) + ', "headers": ' + ASnapshot.Headers + '}';
    finally
      vJson.Free;
    end;
    if (ASnapshot <> nil) and FLogBodies then
    begin
      vJson := TRALJSONObject.Create;
      try
        vJson.Add('body', ASnapshot.Body);
        Result := Copy(Result, 1, Length(Result) - 1) + ', ' +
          Copy(vJson.ToJSON, 2, MaxInt);
      finally
        vJson.Free;
      end;
    end;
  end;
end;

{ TRALLogPlugin }

constructor TRALLogPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSink := TRALLogSink.Create(Self);
  FRedactHeaders := TStringList.Create;
  TStringList(FRedactHeaders).OnChange := {$IFDEF FPC}@{$ENDIF}RedactChanged;
  FRedactHeaders.Add('Authorization');
  FRedactHeaders.Add('Cookie');
  FRedactHeaders.Add('Set-Cookie');
  FRedactHeaders.Add('Proxy-Authorization');
end;

destructor TRALLogPlugin.Destroy;
begin
  // the requests still running keep the sink: it writes nothing more
  FSink.FLock.Enter;
  try
    FSink.FOnLog := nil;
    FSink.FOwner := nil;
  finally
    FSink.FLock.Leave;
  end;
  FSink.Release;
  FreeAndNil(FRedactHeaders);
  inherited Destroy;
end;

class function TRALLogPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityLog;
end;

function TRALLogPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate];
  if FSink.FLogBodies then
    Result := Result + [ppProcess];
end;

procedure TRALLogPlugin.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  FSink.AddRef;
  FSink.Keep(ARequest);
  ARequest.AddFinishHandler({$IFDEF FPC}@{$ENDIF}FSink.RequestDone);
end;

procedure TRALLogPlugin.ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
  var AHandled: boolean);
begin
  // the body is decoded by now
  FSink.KeepBody(ARequest);
end;

procedure TRALLogPlugin.RedactChanged(Sender: TObject);
begin
  FSink.FLock.Enter;
  try
    FSink.FRedact.Assign(FRedactHeaders);
  finally
    FSink.FLock.Leave;
  end;
end;

function TRALLogPlugin.GetConsole: boolean;
begin
  Result := FSink.FConsole;
end;

function TRALLogPlugin.GetFileName: string;
begin
  Result := FSink.FFileName;
end;

function TRALLogPlugin.GetFormat: TRALLogFormat;
begin
  Result := FSink.FFormat;
end;

function TRALLogPlugin.GetLogBodies: boolean;
begin
  Result := FSink.FLogBodies;
end;

function TRALLogPlugin.GetLogHeaders: boolean;
begin
  Result := FSink.FLogHeaders;
end;

function TRALLogPlugin.GetMaxBodyLog: IntegerRAL;
begin
  Result := FSink.FMaxBodyLog;
end;

function TRALLogPlugin.GetMaxFiles: IntegerRAL;
begin
  Result := FSink.FMaxFiles;
end;

function TRALLogPlugin.GetMaxFileSize: Int64RAL;
begin
  Result := FSink.FMaxSize;
end;

function TRALLogPlugin.GetOnLog: TRALOnLogLine;
begin
  Result := FSink.FOnLog;
end;

function TRALLogPlugin.GetRotateDaily: boolean;
begin
  Result := FSink.FRotateDaily;
end;

procedure TRALLogPlugin.SetConsole(const AValue: boolean);
begin
  FSink.FConsole := AValue;
end;

procedure TRALLogPlugin.SetFileName(const AValue: string);
begin
  FSink.FLock.Enter;
  try
    FreeAndNil(FSink.FStream);
    FSink.FFileName := AValue;
  finally
    FSink.FLock.Leave;
  end;
end;

procedure TRALLogPlugin.SetFormat(const AValue: TRALLogFormat);
begin
  FSink.FFormat := AValue;
end;

procedure TRALLogPlugin.SetLogBodies(const AValue: boolean);
begin
  if AValue = FSink.FLogBodies then
    Exit;
  FSink.FLogBodies := AValue;
  // the process phase is only asked for when the body is logged
  Changed;
end;

procedure TRALLogPlugin.SetLogHeaders(const AValue: boolean);
begin
  FSink.FLogHeaders := AValue;
end;

procedure TRALLogPlugin.SetMaxBodyLog(const AValue: IntegerRAL);
begin
  FSink.FMaxBodyLog := AValue;
end;

procedure TRALLogPlugin.SetMaxFiles(const AValue: IntegerRAL);
begin
  if AValue >= 1 then
    FSink.FMaxFiles := AValue;
end;

procedure TRALLogPlugin.SetMaxFileSize(const AValue: Int64RAL);
begin
  FSink.FMaxSize := AValue;
end;

procedure TRALLogPlugin.SetOnLog(const AValue: TRALOnLogLine);
begin
  FSink.FOnLog := AValue;
end;

procedure TRALLogPlugin.SetRedactHeaders(const AValue: TStrings);
begin
  FRedactHeaders.Assign(AValue);
end;

procedure TRALLogPlugin.SetRotateDaily(const AValue: boolean);
begin
  FSink.FRotateDaily := AValue;
end;

{ trace context }

threadvar
  /// The traceparent the request on this thread passes on (a new span of it).
  gTraceParent: StringRAL;

function CurrentTraceParent: StringRAL;
begin
  Result := gTraceParent;
end;

type
  /// The span a TRALTracePlugin keeps in the request until it finishes.
  TRALTraceState = class
  public
    Name: StringRAL;
    ParentSpanId: StringRAL;
    SpanId: StringRAL;
    StartUnixMicro: Int64RAL;
  end;

  /// The thread that exports the spans every ExportInterval.
  TRALTraceExporter = class(TThread)
  private
    FPlugin: TRALTracePlugin;
    FWake: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(APlugin: TRALTracePlugin);
    destructor Destroy; override;
  end;

  PRALTraceSpan = ^TRALTraceSpan;

{ Whether AValue is ALength lower-case hexadecimal digits, not all zero. }
function IsTraceHex(const AValue: StringRAL; ALength: IntegerRAL): boolean;
var
  vInt: IntegerRAL;
  vZero: boolean;
begin
  Result := Length(AValue) = ALength;
  if not Result then
    Exit;
  vZero := True;
  for vInt := 1 to ALength do
  begin
    if not (AValue[POSINISTR - 1 + vInt] in ['0'..'9', 'a'..'f']) then
    begin
      Result := False;
      Exit;
    end;
    if AValue[POSINISTR - 1 + vInt] <> '0' then
      vZero := False;
  end;
  Result := not vZero;
end;

constructor TRALTraceExporter.Create(APlugin: TRALTracePlugin);
begin
  FPlugin := APlugin;
  FWake := TEvent.Create(nil, False, False, '');
  inherited Create(False);
end;

destructor TRALTraceExporter.Destroy;
begin
  Terminate;
  FWake.SetEvent;
  inherited Destroy;
  FreeAndNil(FWake);
end;

procedure TRALTraceExporter.Execute;
begin
  while not Terminated do
  begin
    FWake.WaitFor(Cardinal(FPlugin.ExportInterval));
    try
      FPlugin.Flush;
    except
      // a collector that is down loses the batch, never the server
    end;
  end;
end;

{ TRALTraceCollector }

constructor TRALTraceCollector.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  FReferences := 1;
  FLock := TCriticalSection.Create;
  FPending := TList.Create;
  FStates := TStringList.Create;
  FStates.Sorted := True;
  FStates.OwnsObjects := True;
end;

destructor TRALTraceCollector.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FPending.Count - 1 do
    Dispose(PRALTraceSpan(FPending[vInt]));
  FreeAndNil(FPending);
  FreeAndNil(FStates);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALTraceCollector.AddRef;
begin
  RALAtomicInc(FReferences);
end;

procedure TRALTraceCollector.Release;
begin
  if RALAtomicDec(FReferences) = 0 then
    Free;
end;

procedure TRALTraceCollector.Keep(ARequest: TRALRequest; AObject: TObject);
begin
  FLock.Enter;
  try
    FStates.AddObject(IntToHex(PtrUInt(ARequest), 16), AObject);
  finally
    FLock.Leave;
  end;
end;

function TRALTraceCollector.TakePending: TList;
begin
  FLock.Enter;
  try
    Result := FPending;
    FPending := TList.Create;
  finally
    FLock.Leave;
  end;
end;

procedure TRALTraceCollector.RequestDone(ARequest: TRALRequest);
var
  vState: TRALTraceState;
  vSpan: PRALTraceSpan;
  vIdx: Integer;
begin
  gTraceParent := '';
  try
    vState := nil;
    FLock.Enter;
    try
      if FStates.Find(IntToHex(PtrUInt(ARequest), 16), vIdx) then
      begin
        vState := TRALTraceState(FStates.Objects[vIdx]);
        FStates.Objects[vIdx] := nil;
        FStates.Delete(vIdx);
      end;
    finally
      FLock.Leave;
    end;
    if vState = nil then
      Exit;
    try
      New(vSpan);
      vSpan^.TraceId := ARequest.TraceId;
      vSpan^.SpanId := vState.SpanId;
      vSpan^.ParentSpanId := vState.ParentSpanId;
      vSpan^.Name := RALMethodToHTTPMethod(ARequest.Method) + ' ' + RouteLabel(ARequest);
      vSpan^.Method := RALMethodToHTTPMethod(ARequest.Method);
      vSpan^.StartUnixMicro := vState.StartUnixMicro;
      vSpan^.EndUnixMicro := UnixMicroNow;
      vSpan^.StatusCode := ARequest.ResponseStatus;
      FLock.Enter;
      try
        if Assigned(FOnSpan) and (FOwner <> nil) then
          FOnSpan(FOwner, vSpan^);
        if FExport and (FOwner <> nil) then
        begin
          FPending.Add(vSpan);
          vSpan := nil;
        end;
      finally
        FLock.Leave;
      end;
      if vSpan <> nil then
        Dispose(vSpan);
    finally
      vState.Free;
    end;
  finally
    Release;
  end;
end;

{ TRALTracePlugin }

constructor TRALTracePlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCollector := TRALTraceCollector.Create(Self);
  FExportInterval := 5000;
  FServiceName := 'ralserver';
end;

destructor TRALTracePlugin.Destroy;
begin
  FreeAndNil(FExporter);
  // the requests still running keep the collector: it reports nothing more
  FCollector.FLock.Enter;
  try
    FCollector.FOnSpan := nil;
    FCollector.FOwner := nil;
  finally
    FCollector.FLock.Leave;
  end;
  FCollector.Release;
  inherited Destroy;
end;

class function TRALTracePlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityTrace;
end;

function TRALTracePlugin.GetOnSpan: TRALOnTraceSpan;
begin
  Result := FCollector.FOnSpan;
end;

procedure TRALTracePlugin.SetOnSpan(const AValue: TRALOnTraceSpan);
begin
  FCollector.FOnSpan := AValue;
end;

procedure TRALTracePlugin.SetExportURL(const AValue: StringRAL);
begin
  FExportURL := AValue;
  FCollector.FExport := AValue <> '';
end;

function TRALTracePlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate];
end;

procedure TRALTracePlugin.ServerActivating;
begin
  inherited;
  RALTraceParentProvider := {$IFDEF FPC}@{$ENDIF}CurrentTraceParent;
  if (FExportURL <> '') and (FExporter = nil) then
    FExporter := TRALTraceExporter.Create(Self);
end;

procedure TRALTracePlugin.ServerDeactivating;
begin
  FreeAndNil(FExporter);
  try
    if FExportURL <> '' then
      Flush;
  except
  end;
  inherited;
end;

procedure TRALTracePlugin.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vParent, vTraceId, vParentSpan, vSpan: StringRAL;
  vState: TRALTraceState;
begin
  // traceparent: version-traceid-parentid-flags (W3C Trace Context)
  vParent := RALTrim(HeaderOf(ARequest, 'traceparent'));
  vTraceId := '';
  vParentSpan := '';
  if (Length(vParent) = 55) and (vParent[POSINISTR + 2] = '-') and
     (vParent[POSINISTR + 35] = '-') and (vParent[POSINISTR + 52] = '-') and
     (Copy(vParent, 1, 2) <> 'ff') and IsTraceHex(Copy(vParent, 4, 32), 32) and
     IsTraceHex(Copy(vParent, 37, 16), 16) then
  begin
    vTraceId := Copy(vParent, 4, 32);
    vParentSpan := Copy(vParent, 37, 16);
  end;
  if vTraceId = '' then
    vTraceId := RALRandomHex(16);
  vSpan := RALRandomHex(8);
  ARequest.TraceId := vTraceId;
  AResponse.AddHeader('traceresponse', '00-' + vTraceId + '-' + vSpan + '-01');

  vState := TRALTraceState.Create;
  vState.ParentSpanId := vParentSpan;
  vState.SpanId := vSpan;
  vState.StartUnixMicro := UnixMicroNow;
  vState.Name := RALMethodToHTTPMethod(ARequest.Method) + ' ' + ARequest.Query;
  // a client called from the handler, on this thread, carries this span on
  gTraceParent := '00-' + vTraceId + '-' + vSpan + '-01';
  FCollector.AddRef;
  FCollector.Keep(ARequest, vState);
  ARequest.AddFinishHandler({$IFDEF FPC}@{$ENDIF}FCollector.RequestDone);
end;

procedure TRALTracePlugin.Flush;
var
  vList: TList;
  vInt: IntegerRAL;
  vSpan: PRALTraceSpan;
  vRoot, vResource, vAttr, vScope, vSpanJson, vStatus, vKV, vValue: TRALJSONObject;
  vResourceSpans, vAttrs, vScopeSpans, vSpans, vSpanAttrs: TRALJSONArray;
  vClient: TRALClient;
  vResp: TRALResponse;
begin
  vList := FCollector.TakePending;
  if vList.Count = 0 then
  begin
    vList.Free;
    Exit;
  end;
  vRoot := TRALJSONObject.Create;
  try
    // OTLP/HTTP JSON: resourceSpans[ { resource, scopeSpans[ { scope, spans[] } ] } ]
    vResourceSpans := TRALJSONArray.Create;
    vResource := TRALJSONObject.Create;
    vAttrs := TRALJSONArray.Create;
    vKV := TRALJSONObject.Create;
    vKV.Add('key', 'service.name');
    vValue := TRALJSONObject.Create;
    vValue.Add('stringValue', FServiceName);
    vKV.Add('value', vValue);
    vAttrs.Add(vKV);
    vAttr := TRALJSONObject.Create;
    vAttr.Add('attributes', vAttrs);
    vResource.Add('resource', vAttr);
    vScopeSpans := TRALJSONArray.Create;
    vScope := TRALJSONObject.Create;
    vValue := TRALJSONObject.Create;
    vValue.Add('name', 'PascalRAL');
    vScope.Add('scope', vValue);
    vSpans := TRALJSONArray.Create;
    for vInt := 0 to vList.Count - 1 do
    begin
      vSpan := PRALTraceSpan(vList[vInt]);
      vSpanJson := TRALJSONObject.Create;
      vSpanJson.Add('traceId', vSpan^.TraceId);
      vSpanJson.Add('spanId', vSpan^.SpanId);
      if vSpan^.ParentSpanId <> '' then
        vSpanJson.Add('parentSpanId', vSpan^.ParentSpanId);
      vSpanJson.Add('name', vSpan^.Name);
      vSpanJson.Add('kind', 2); // SPAN_KIND_SERVER
      vSpanJson.Add('startTimeUnixNano', StringRAL(IntToStr(vSpan^.StartUnixMicro) + '000'));
      vSpanJson.Add('endTimeUnixNano', StringRAL(IntToStr(vSpan^.EndUnixMicro) + '000'));
      vSpanAttrs := TRALJSONArray.Create;
      vKV := TRALJSONObject.Create;
      vKV.Add('key', 'http.response.status_code');
      vValue := TRALJSONObject.Create;
      vValue.Add('intValue', StringRAL(IntToStr(vSpan^.StatusCode)));
      vKV.Add('value', vValue);
      vSpanAttrs.Add(vKV);
      vKV := TRALJSONObject.Create;
      vKV.Add('key', 'http.request.method');
      vValue := TRALJSONObject.Create;
      vValue.Add('stringValue', vSpan^.Method);
      vKV.Add('value', vValue);
      vSpanAttrs.Add(vKV);
      vSpanJson.Add('attributes', vSpanAttrs);
      vStatus := TRALJSONObject.Create;
      if vSpan^.StatusCode >= 500 then
        vStatus.Add('code', 2)
      else
        vStatus.Add('code', 0);
      vSpanJson.Add('status', vStatus);
      vSpans.Add(vSpanJson);
      Dispose(vSpan);
    end;
    vList.Free;
    vScope.Add('spans', vSpans);
    vScopeSpans.Add(vScope);
    vResource.Add('scopeSpans', vScopeSpans);
    vResourceSpans.Add(vResource);
    vRoot.Add('resourceSpans', vResourceSpans);

    vClient := TRALClient.Create(nil);
    try
      if FExportEngine <> '' then
        vClient.EngineType := string(FExportEngine);
      vClient.BaseURL.Text := string(FExportURL);
      vClient.Request.AddBody(vRoot.ToJSON, rctAPPLICATIONJSON);
      vResp := nil;
      try
        vClient.Post('', vResp);
      finally
        vResp.Free;
      end;
    finally
      vClient.Free;
    end;
  finally
    vRoot.Free;
  end;
end;

{ TRALMetricsRegistry }

constructor TRALMetricsRegistry.Create;
const
  cBuckets: array[0..10] of Double = (0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1,
    2.5, 5, 10);
var
  vInt: IntegerRAL;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FCounts := TStringList.Create;
  FCounts.Sorted := True;
  FDurations := TStringList.Create;
  FDurations.Sorted := True;
  FDurations.OwnsObjects := False;
  FReferences := 1;
  SetLength(FBuckets, Length(cBuckets));
  for vInt := 0 to High(cBuckets) do
    FBuckets[vInt] := cBuckets[vInt];
end;

type
  /// The histogram of one method and route: counts per bucket, sum and count.
  TRALHistogram = class
  public
    Buckets: array of Int64RAL;
    Count: Int64RAL;
    Sum: Double;
  end;

destructor TRALMetricsRegistry.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FDurations.Count - 1 do
    FDurations.Objects[vInt].Free;
  FreeAndNil(FDurations);
  FreeAndNil(FCounts);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALMetricsRegistry.AddRef;
begin
  RALAtomicInc(FReferences);
end;

procedure TRALMetricsRegistry.Release;
begin
  if RALAtomicDec(FReferences) = 0 then
    Free;
end;

procedure TRALMetricsRegistry.RequestDone(ARequest: TRALRequest);
var
  vSeconds: Double;
begin
  try
    vSeconds := 0;
    if ARequest.StartTick > 0 then
      vSeconds := (RALTickMicro - ARequest.StartTick) / 1000000;
    Done(RALMethodToHTTPMethod(ARequest.Method), RouteLabel(ARequest),
      ARequest.ResponseStatus, vSeconds);
  finally
    Release;
  end;
end;

procedure TRALMetricsRegistry.Started;
begin
  RALAtomicInc(FInFlight);
end;

{ A label value as Prometheus escapes it: backslash, double quote, line feed. }
function LabelValue(const AValue: StringRAL): StringRAL;
begin
  Result := StringRAL(StringReplace(StringReplace(StringReplace(string(AValue), '\', '\\',
    [rfReplaceAll]), '"', '\"', [rfReplaceAll]), #10, '\n', [rfReplaceAll]));
end;

procedure TRALMetricsRegistry.Done(const AMethod, ARoute: StringRAL; AStatus: IntegerRAL;
  ASeconds: Double);
var
  vKey: string;
  vIdx, vInt: Integer;
  vHist: TRALHistogram;
begin
  RALAtomicDec(FInFlight);
  FLock.Enter;
  try
    vKey := 'method="' + string(LabelValue(AMethod)) + '",route="' +
      string(LabelValue(ARoute)) + '",status="' + IntToStr(AStatus) + '"';
    if FCounts.Find(vKey, vIdx) then
      FCounts.Objects[vIdx] := TObject(PtrUInt(FCounts.Objects[vIdx]) + 1)
    else
      FCounts.AddObject(vKey, TObject(PtrUInt(1)));
    vKey := 'method="' + string(LabelValue(AMethod)) + '",route="' + string(LabelValue(ARoute)) + '"';
    if FDurations.Find(vKey, vIdx) then
      vHist := TRALHistogram(FDurations.Objects[vIdx])
    else
    begin
      vHist := TRALHistogram.Create;
      SetLength(vHist.Buckets, Length(FBuckets));
      FDurations.AddObject(vKey, vHist);
    end;
    Inc(vHist.Count);
    vHist.Sum := vHist.Sum + ASeconds;
    for vInt := 0 to High(FBuckets) do
      if ASeconds <= FBuckets[vInt] then
        Inc(vHist.Buckets[vInt]);
  finally
    FLock.Leave;
  end;
end;

function TRALMetricsRegistry.Render: StringRAL;
var
  vOut: TStringList;
  vInt, vB: Integer;
  vHist: TRALHistogram;
begin
  vOut := TStringList.Create;
  try
    FLock.Enter;
    try
      vOut.Add('# HELP ral_http_requests_total Requests answered, by method, route and status.');
      vOut.Add('# TYPE ral_http_requests_total counter');
      for vInt := 0 to FCounts.Count - 1 do
        vOut.Add('ral_http_requests_total{' + FCounts[vInt] + '} ' +
          IntToStr(PtrUInt(FCounts.Objects[vInt])));
      vOut.Add('# HELP ral_http_request_duration_seconds Time to answer, by method and route.');
      vOut.Add('# TYPE ral_http_request_duration_seconds histogram');
      for vInt := 0 to FDurations.Count - 1 do
      begin
        vHist := TRALHistogram(FDurations.Objects[vInt]);
        for vB := 0 to High(FBuckets) do
          vOut.Add('ral_http_request_duration_seconds_bucket{' + FDurations[vInt] + ',le="' +
            FloatToStr(FBuckets[vB], RALInvariantFormat) + '"} ' + IntToStr(vHist.Buckets[vB]));
        vOut.Add('ral_http_request_duration_seconds_bucket{' + FDurations[vInt] + ',le="+Inf"} ' +
          IntToStr(vHist.Count));
        vOut.Add('ral_http_request_duration_seconds_sum{' + FDurations[vInt] + '} ' +
          FloatToStr(vHist.Sum, RALInvariantFormat));
        vOut.Add('ral_http_request_duration_seconds_count{' + FDurations[vInt] + '} ' +
          IntToStr(vHist.Count));
      end;
    finally
      FLock.Leave;
    end;
    vOut.Add('# HELP ral_http_requests_in_flight Requests being answered now.');
    vOut.Add('# TYPE ral_http_requests_in_flight gauge');
    vOut.Add('ral_http_requests_in_flight ' + IntToStr(FInFlight));
    vOut.LineBreak := #10;
    Result := StringRAL(vOut.Text);
  finally
    vOut.Free;
  end;
end;

{ TRALMetricsPlugin }

constructor TRALMetricsPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegistry := TRALMetricsRegistry.Create;
  FRoutes := TRALRoutes.Create(Self);
  SetPath('/metrics');
end;

destructor TRALMetricsPlugin.Destroy;
begin
  FreeAndNil(FRoutes);
  FRegistry.Release;
  inherited Destroy;
end;

function TRALMetricsPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate, ppProcess, ppResolveRoute];
end;

procedure TRALMetricsPlugin.SetPath(const AValue: StringRAL);
var
  vRoute: TRALRoute;
begin
  FPath := FixRoute(AValue);
  FRoutes.Clear;
  vRoute := TRALRoute(FRoutes.Add);
  vRoute.Route := FPath;
  vRoute.AllowedMethods := [amGET, amHEAD];
  vRoute.SkipAuthMethods := [amALL];
end;

procedure TRALMetricsPlugin.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  FRegistry.AddRef;
  FRegistry.Started;
  ARequest.AddFinishHandler({$IFDEF FPC}@{$ENDIF}FRegistry.RequestDone);
end;

function TRALMetricsPlugin.ResolveRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
begin
  Result := FRoutes.CanAnswerRoute(ARequest);
end;

function TRALMetricsPlugin.Render: StringRAL;
var
  vExtra: TStringList;
begin
  Result := FRegistry.Render;
  if Assigned(FOnCollect) then
  begin
    vExtra := TStringList.Create;
    try
      FOnCollect(Self, vExtra);
      vExtra.LineBreak := #10;
      Result := Result + StringRAL(vExtra.Text);
    finally
      vExtra.Free;
    end;
  end;
end;

procedure TRALMetricsPlugin.ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
  var AHandled: boolean);
begin
  if ARequest.RouteOwner <> Self then
    Exit;
  AHandled := True;
  if not (ARequest.Method in [amGET, amHEAD]) then
  begin
    AResponse.Answer(HTTP_MethodNotAllowed);
    AResponse.AddHeader('Allow', 'GET, HEAD');
    Exit;
  end;
  AResponse.Answer(HTTP_OK, Render, 'text/plain; version=0.0.4; charset=utf-8');
end;

{ TRALHealthCheck }

procedure TRALHealthCheck.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALHealthCheck then
  begin
    TRALHealthCheck(Dest).Name := FName;
    TRALHealthCheck(Dest).OnCheck := FOnCheck;
  end
  else
    inherited AssignTo(Dest);
end;

function TRALHealthCheck.GetDisplayName: string;
begin
  Result := string(FName);
end;

{ TRALHealthChecks }

function TRALHealthChecks.AddCheck(const AName: StringRAL;
  AOnCheck: TRALOnHealthCheck): TRALHealthCheck;
begin
  Result := TRALHealthCheck(Add);
  Result.Name := AName;
  Result.OnCheck := AOnCheck;
end;

{ TRALHealthPlugin }

constructor TRALHealthPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChecks := TRALHealthChecks.Create(Self, TRALHealthCheck);
  FRoutes := TRALRoutes.Create(Self);
  FLivePath := '/health/live';
  FReadyPath := '/health/ready';
  Rebuild;
end;

destructor TRALHealthPlugin.Destroy;
begin
  FreeAndNil(FRoutes);
  FreeAndNil(FChecks);
  inherited Destroy;
end;

function TRALHealthPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppProcess, ppResolveRoute];
end;

procedure TRALHealthPlugin.Rebuild;
var
  vRoute: TRALRoute;
begin
  FRoutes.Clear;
  vRoute := TRALRoute(FRoutes.Add);
  vRoute.Name := 'live';
  vRoute.Route := FLivePath;
  vRoute.AllowedMethods := [amGET, amHEAD];
  vRoute.SkipAuthMethods := [amALL];
  vRoute := TRALRoute(FRoutes.Add);
  vRoute.Name := 'ready';
  vRoute.Route := FReadyPath;
  vRoute.AllowedMethods := [amGET, amHEAD];
  vRoute.SkipAuthMethods := [amALL];
end;

procedure TRALHealthPlugin.SetChecks(const AValue: TRALHealthChecks);
begin
  FChecks.Assign(AValue);
end;

procedure TRALHealthPlugin.SetLivePath(const AValue: StringRAL);
begin
  FLivePath := FixRoute(AValue);
  Rebuild;
end;

procedure TRALHealthPlugin.SetReadyPath(const AValue: StringRAL);
begin
  FReadyPath := FixRoute(AValue);
  Rebuild;
end;

function TRALHealthPlugin.ResolveRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
begin
  Result := FRoutes.CanAnswerRoute(ARequest);
end;

function TRALHealthPlugin.Ready(out ABody: StringRAL): boolean;
var
  vJson, vChecks, vItem: TRALJSONObject;
  vArr: TRALJSONArray;
  vInt: IntegerRAL;
  vCheck: TRALHealthCheck;
  vHealthy: boolean;
  vOutput: StringRAL;
begin
  Result := True;
  vJson := TRALJSONObject.Create;
  try
    vChecks := TRALJSONObject.Create;
    for vInt := 0 to FChecks.Count - 1 do
    begin
      vCheck := TRALHealthCheck(FChecks.Items[vInt]);
      vHealthy := True;
      vOutput := '';
      try
        if Assigned(vCheck.OnCheck) then
          vCheck.OnCheck(vCheck, vHealthy, vOutput);
      except
        on e: Exception do
        begin
          vHealthy := False;
          vOutput := StringRAL(e.Message);
        end;
      end;
      Result := Result and vHealthy;
      vItem := TRALJSONObject.Create;
      if vHealthy then
        vItem.Add('status', 'pass')
      else
        vItem.Add('status', 'fail');
      if vOutput <> '' then
        vItem.Add('output', vOutput);
      vArr := TRALJSONArray.Create;
      vArr.Add(vItem);
      vChecks.Add(vCheck.Name, vArr);
    end;
    if Result then
      vJson.Add('status', 'pass')
    else
      vJson.Add('status', 'fail');
    vJson.Add('checks', vChecks);
    ABody := vJson.ToJSON;
  finally
    vJson.Free;
  end;
end;

procedure TRALHealthPlugin.ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
  var AHandled: boolean);
var
  vBody: StringRAL;
begin
  if ARequest.RouteOwner <> Self then
    Exit;
  AHandled := True;
  if not (ARequest.Method in [amGET, amHEAD]) then
  begin
    AResponse.Answer(HTTP_MethodNotAllowed);
    AResponse.AddHeader('Allow', 'GET, HEAD');
    Exit;
  end;
  AResponse.AddHeader('Cache-Control', 'no-store');
  if TRALRoute(ARequest.ResolvedRoute).Name = 'live' then
    AResponse.Answer(HTTP_OK, '{"status":"pass"}', 'application/health+json')
  else if Ready(vBody) then
    AResponse.Answer(HTTP_OK, vBody, 'application/health+json')
  else
    AResponse.Answer(HTTP_ServiceUnavailable, vBody, 'application/health+json');
end;

end.
