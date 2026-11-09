/// Runs a server as a Windows service (the Service Control Manager API
/// itself, no SvcMgr and no VCL) or as a Linux daemon under systemd
/// (Type=notify, SIGTERM stops gracefully, SIGHUP reloads), or in the
/// foreground of a console. The same executable installs and removes itself.
unit RALServiceHost;

{$I ..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  {$IFDEF RALWindows}
  Windows,
  {$ELSE}
  {$IFDEF FPC}
  ctypes, BaseUnix, Sockets,
  {$ELSE}
  Posix.Signal, Posix.SysSocket, Posix.SysUn, Posix.Unistd, Posix.Stdlib,
  {$ENDIF}
  {$ENDIF}
  RALTypes, RALCustomObjects, RALServer, RALExceptions, RALErrorCodes;

type
  /// What the command line asked.
  TRALServiceCommand = (scNone, scInstall, scUninstall, scStart, scStop, scConsole, scRun);

  { The host of one server: Run reads the command line (install, uninstall,
    start, stop, console) and, without one, runs as the service or daemon the
    system started. Stopping is TRALServer.Shutdown: running requests finish. }
  TRALServiceHost = class(TRALComponent)
  private
    FDescription: string;
    FDisplayName: string;
    FOnLog: TGetStrProc;
    FOnReload: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FReloadAsked: boolean;
    FServer: TRALServer;
    FServiceName: string;
    FShutdownTimeout: IntegerRAL;
    FStartArgs: string;
    FStopEvent: TEvent;
    FUnitDir: string;

    procedure SetServer(const AValue: TRALServer);
  protected
    procedure Log(const AText: string);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// Starts the server and waits until a stop is asked; True when it ran.
    function RunUntilStopped(ADaemon: boolean): boolean;
    procedure StartServer;
    procedure StopServer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// The command of the command line (first argument).
    class function CommandOf(const AArg: string): TRALServiceCommand;
    /// Installs the service (Windows) or writes the systemd unit (Linux).
    function Install: boolean;
    /// Asks the running host to stop (as the service manager or a signal does).
    procedure RequestStop;
    { Does what the command line asks and returns True; False when the program
      was not started by the service manager and got no command (a VCL or LCL
      program then opens its window). On Linux, no command is the daemon. }
    function Run: boolean;
    { Runs in the foreground until Ctrl+C (SIGINT/SIGTERM on Linux). }
    procedure RunConsole;
    /// Asks the system to start the service.
    function StartService: boolean;
    /// Asks the system to stop the service.
    function StopService: boolean;
    /// Removes the service or the unit.
    function Uninstall: boolean;
  published
    /// Text of the service in the system's list.
    property Description: string read FDescription write FDescription;
    /// Name shown by the service manager ('' = ServiceName).
    property DisplayName: string read FDisplayName write FDisplayName;
    /// Lines about what the host does (installed, started, stopping...).
    property OnLog: TGetStrProc read FOnLog write FOnLog;
    /// SIGHUP on Linux, "paramchange" on Windows: reload the configuration.
    property OnReload: TNotifyEvent read FOnReload write FOnReload;
    /// The server is up.
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    /// The server stopped.
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    /// The server the host starts and stops.
    property Server: TRALServer read FServer write SetServer;
    /// Name of the service or the systemd unit.
    property ServiceName: string read FServiceName write FServiceName;
    /// Milliseconds a stop waits for running requests.
    property ShutdownTimeout: IntegerRAL read FShutdownTimeout write FShutdownTimeout default 30000;
    /// Arguments the installed service is started with (ExecStart, binary path).
    property StartArgs: string read FStartArgs write FStartArgs;
    /// Linux: folder of the unit file; empty = /etc/systemd/system.
    property UnitDir: string read FUnitDir write FUnitDir;
  end;

/// Sends a systemd notification ('READY=1', 'STOPPING=1', 'WATCHDOG=1'); False outside systemd.
function RALSystemdNotify(const AState: string): boolean;

implementation

{$IFDEF RALWindows}
const
  cAdvapi = 'advapi32.dll';
  SC_MANAGER_ALL_ACCESS = $F003F;
  SERVICE_ALL_ACCESS_ = $F01FF;
  SERVICE_WIN32_OWN_PROCESS_ = $10;
  SERVICE_AUTO_START_ = 2;
  SERVICE_ERROR_NORMAL_ = 1;
  SERVICE_STOPPED_ = 1;
  SERVICE_START_PENDING_ = 2;
  SERVICE_STOP_PENDING_ = 3;
  SERVICE_RUNNING_ = 4;
  SERVICE_ACCEPT_STOP_ = 1;
  SERVICE_ACCEPT_SHUTDOWN_ = 4;
  SERVICE_ACCEPT_PARAMCHANGE_ = 8;
  SERVICE_CONTROL_STOP_ = 1;
  SERVICE_CONTROL_INTERROGATE_ = 4;
  SERVICE_CONTROL_SHUTDOWN_ = 5;
  SERVICE_CONTROL_PARAMCHANGE_ = 6;
  ERROR_FAILED_SERVICE_CONTROLLER_CONNECT_ = 1063;
  SERVICE_CONFIG_DESCRIPTION_ = 1;

type
  TRALServiceStatus = record
    dwServiceType: DWORD;
    dwCurrentState: DWORD;
    dwControlsAccepted: DWORD;
    dwWin32ExitCode: DWORD;
    dwServiceSpecificExitCode: DWORD;
    dwCheckPoint: DWORD;
    dwWaitHint: DWORD;
  end;
  TRALServiceTableEntry = record
    lpServiceName: PWideChar;
    lpServiceProc: Pointer;
  end;
  TRALServiceDescription = record
    lpDescription: PWideChar;
  end;

function OpenSCManagerW(AMachine, ADatabase: PWideChar; AAccess: DWORD): THandle; stdcall;
  external cAdvapi;
function CreateServiceW(AManager: THandle; AName, ADisplay: PWideChar; AAccess, AType,
  AStart, AError: DWORD; APath, AGroup: PWideChar; ATag: Pointer; ADeps, AUser,
  APassword: PWideChar): THandle; stdcall; external cAdvapi;
function OpenServiceW(AManager: THandle; AName: PWideChar; AAccess: DWORD): THandle; stdcall;
  external cAdvapi;
function DeleteService(AService: THandle): BOOL; stdcall; external cAdvapi;
function StartServiceW(AService: THandle; AArgCount: DWORD; AArgs: Pointer): BOOL; stdcall;
  external cAdvapi;
function ControlService(AService: THandle; AControl: DWORD; var AStatus: TRALServiceStatus): BOOL;
  stdcall; external cAdvapi;
function CloseServiceHandle(AHandle: THandle): BOOL; stdcall; external cAdvapi;
function StartServiceCtrlDispatcherW(ATable: Pointer): BOOL; stdcall; external cAdvapi;
function RegisterServiceCtrlHandlerExW(AName: PWideChar; AHandler, AContext: Pointer): THandle;
  stdcall; external cAdvapi;
function SetServiceStatus(AHandle: THandle; var AStatus: TRALServiceStatus): BOOL; stdcall;
  external cAdvapi;
function ChangeServiceConfig2W(AService: THandle; ALevel: DWORD; AInfo: Pointer): BOOL; stdcall;
  external cAdvapi;
{$ENDIF}

var
  { the host of this process: a service manager or a signal reaches it }
  gHost: TRALServiceHost;
  {$IFDEF RALWindows}
  gStatusHandle: THandle;
  gStatus: TRALServiceStatus;
  gName: WideString;
  {$ENDIF}

{$IFDEF RALWindows}
procedure ReportStatus(AState, AWaitHint: DWORD);
begin
  gStatus.dwServiceType := SERVICE_WIN32_OWN_PROCESS_;
  gStatus.dwCurrentState := AState;
  if AState = SERVICE_START_PENDING_ then
    gStatus.dwControlsAccepted := 0
  else
    gStatus.dwControlsAccepted := SERVICE_ACCEPT_STOP_ or SERVICE_ACCEPT_SHUTDOWN_ or
      SERVICE_ACCEPT_PARAMCHANGE_;
  gStatus.dwWin32ExitCode := 0;
  gStatus.dwWaitHint := AWaitHint;
  if (AState = SERVICE_RUNNING_) or (AState = SERVICE_STOPPED_) then
    gStatus.dwCheckPoint := 0
  else
    Inc(gStatus.dwCheckPoint);
  if gStatusHandle <> 0 then
    SetServiceStatus(gStatusHandle, gStatus);
end;

function ServiceHandler(AControl, AEventType: DWORD; AEventData, AContext: Pointer): DWORD; stdcall;
begin
  Result := 0; // NO_ERROR
  case AControl of
    SERVICE_CONTROL_STOP_, SERVICE_CONTROL_SHUTDOWN_:
    begin
      ReportStatus(SERVICE_STOP_PENDING_, gHost.ShutdownTimeout + 5000);
      gHost.RequestStop;
    end;
    SERVICE_CONTROL_PARAMCHANGE_:
    begin
      gHost.FReloadAsked := True;
      gHost.FStopEvent.SetEvent; // wakes the wait; it goes on after the reload
    end;
    SERVICE_CONTROL_INTERROGATE_:
      SetServiceStatus(gStatusHandle, gStatus);
  else
    Result := 120; // ERROR_CALL_NOT_IMPLEMENTED
  end;
end;

procedure ServiceMain(AArgCount: DWORD; AArgs: Pointer); stdcall;
begin
  gStatusHandle := RegisterServiceCtrlHandlerExW(PWideChar(gName), @ServiceHandler, nil);
  if gStatusHandle = 0 then
    Exit;
  ReportStatus(SERVICE_START_PENDING_, 30000);
  try
    gHost.RunUntilStopped(True);
  except
    on E: Exception do
      gHost.Log('service failed: ' + E.Message);
  end;
  ReportStatus(SERVICE_STOPPED_, 0);
end;

function ConsoleHandler(ACtrlType: DWORD): BOOL; stdcall;
begin
  Result := True;
  if gHost <> nil then
    gHost.RequestStop;
end;
{$ENDIF}

{$IFNDEF RALWindows}
{ the handlers only raise flags: everything else runs on the host's thread }
procedure RALSignalHandler(ASignal: {$IFDEF FPC}cint{$ELSE}Integer{$ENDIF}); cdecl;
begin
  if gHost = nil then
    Exit;
  gHost.FReloadAsked := ASignal = SIGHUP;
  gHost.FStopEvent.SetEvent;
end;
{$ENDIF}

function RALSystemdNotify(const AState: string): boolean;
{$IFNDEF RALWindows}
var
  vPath, vMsg: AnsiString;
  vSock, vLen: Integer;
  {$IFDEF FPC}
  vAddr: TUnixSockAddr;
  {$ELSE}
  vAddr: sockaddr_un;
  {$ENDIF}
begin
  Result := False;
  vPath := AnsiString(GetEnvironmentVariable('NOTIFY_SOCKET'));
  if (vPath = '') or (Length(vPath) > 107) then
    Exit;
  {$IFDEF FPC}
  vSock := fpSocket(AF_UNIX, SOCK_DGRAM, 0);
  {$ELSE}
  vSock := socket(AF_UNIX, SOCK_DGRAM, 0);
  {$ENDIF}
  if vSock < 0 then
    Exit;
  try
    FillChar(vAddr, SizeOf(vAddr), 0);
    {$IFDEF FPC}
    vAddr.family := AF_UNIX;
    Move(vPath[1], vAddr.path[0], Length(vPath));
    { '@' is the abstract namespace: a zero byte in front }
    if vPath[1] = '@' then
      vAddr.path[0] := #0;
    {$ELSE}
    vAddr.sun_family := AF_UNIX;
    Move(vPath[1], vAddr.sun_path[0], Length(vPath));
    if vPath[1] = '@' then
      vAddr.sun_path[0] := 0;
    {$ENDIF}
    vLen := 2 + Length(vPath);
    vMsg := AnsiString(AState);
    {$IFDEF FPC}
    Result := fpSendTo(vSock, PAnsiChar(vMsg), Length(vMsg), 0, PSockAddr(@vAddr), vLen) = Length(vMsg);
    {$ELSE}
    Result := sendto(vSock, PAnsiChar(vMsg)^, Length(vMsg), 0, PSockAddr(@vAddr)^, vLen) = Length(vMsg);
    {$ENDIF}
  finally
    {$IFDEF FPC}
    CloseSocket(vSock);
    {$ELSE}
    __close(vSock);
    {$ENDIF}
  end;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

{ TRALServiceHost }

constructor TRALServiceHost.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShutdownTimeout := 30000;
  FServiceName := 'RALService';
  FStopEvent := TEvent.Create(nil, False, False, '');
end;

destructor TRALServiceHost.Destroy;
begin
  if gHost = Self then
    gHost := nil;
  FreeAndNil(FStopEvent);
  inherited;
end;

procedure TRALServiceHost.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FServer) then
    FServer := nil;
end;

procedure TRALServiceHost.SetServer(const AValue: TRALServer);
begin
  if FServer = AValue then
    Exit;
  if FServer <> nil then
    FServer.RemoveFreeNotification(Self);
  FServer := AValue;
  if FServer <> nil then
    FServer.FreeNotification(Self);
end;

procedure TRALServiceHost.Log(const AText: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AText);
end;

class function TRALServiceHost.CommandOf(const AArg: string): TRALServiceCommand;
var
  vArg: string;
begin
  vArg := LowerCase(AArg);
  while (vArg <> '') and CharInSet(vArg[1], ['-', '/']) do
    Delete(vArg, 1, 1);
  if vArg = 'install' then
    Result := scInstall
  else if vArg = 'uninstall' then
    Result := scUninstall
  else if vArg = 'start' then
    Result := scStart
  else if vArg = 'stop' then
    Result := scStop
  else if vArg = 'console' then
    Result := scConsole
  else if vArg = 'run' then
    Result := scRun
  else
    Result := scNone;
end;

procedure TRALServiceHost.RequestStop;
begin
  FReloadAsked := False;
  FStopEvent.SetEvent;
end;

procedure TRALServiceHost.StartServer;
begin
  if FServer = nil then
    raise ERALException.Create(ecServiceNoServer);
  FServer.Active := True;
  Log(Format('%s listening on port %d', [FServiceName, FServer.BoundPort]));
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TRALServiceHost.StopServer;
begin
  if (FServer <> nil) and FServer.Active then
  begin
    Log(FServiceName + ' stopping');
    FServer.Shutdown(FShutdownTimeout);
  end;
  if Assigned(FOnStop) then
    FOnStop(Self);
end;

function TRALServiceHost.RunUntilStopped(ADaemon: boolean): boolean;
var
  vWatchdog: Int64;
  vWait: Cardinal;
begin
  Result := True;
  gHost := Self;
  FStopEvent.ResetEvent;
  StartServer;
  {$IFDEF RALWindows}
  if ADaemon then
    ReportStatus(SERVICE_RUNNING_, 0);
  {$ENDIF}
  RALSystemdNotify('READY=1');
  { systemd's watchdog: pinged at half the interval it asks for }
  vWatchdog := StrToInt64Def(SysUtils.GetEnvironmentVariable('WATCHDOG_USEC'), 0) div 2000;
  if vWatchdog > 0 then
    vWait := vWatchdog
  else
    vWait := 1000;
  repeat
    if FStopEvent.WaitFor(vWait) = wrSignaled then
    begin
      if not FReloadAsked then
        Break;
      FReloadAsked := False;
      RALSystemdNotify('RELOADING=1');
      Log(FServiceName + ' reloading');
      if Assigned(FOnReload) then
        FOnReload(Self);
      RALSystemdNotify('READY=1');
    end
    else if vWatchdog > 0 then
      RALSystemdNotify('WATCHDOG=1');
  until False;
  RALSystemdNotify('STOPPING=1');
  StopServer;
end;

procedure TRALServiceHost.RunConsole;
begin
  gHost := Self;
  {$IFDEF RALWindows}
  SetConsoleCtrlHandler(@ConsoleHandler, True);
  {$ELSE}
  {$IFDEF FPC}
  fpSignal(SIGTERM, @RALSignalHandler);
  fpSignal(SIGINT, @RALSignalHandler);
  fpSignal(SIGHUP, @RALSignalHandler);
  {$ELSE}
  signal(SIGTERM, RALSignalHandler);
  signal(SIGINT, RALSignalHandler);
  signal(SIGHUP, RALSignalHandler);
  {$ENDIF}
  {$ENDIF}
  RunUntilStopped(False);
end;

{$IFNDEF RALWindows}
{ runs systemctl with AArgs; True on exit code 0 }
function Systemctl(const AArgs: string): boolean;
begin
  {$IFDEF FPC}
  Result := ExecuteProcess('/bin/systemctl', AArgs) = 0;
  {$ELSE}
  Result := _system(MarshaledAString(UTF8String('/bin/systemctl ' + AArgs))) = 0;
  {$ENDIF}
end;
{$ENDIF}

function ExePath: string;
begin
  Result := ExpandFileName(ParamStr(0));
end;

function TRALServiceHost.Install: boolean;
{$IFDEF RALWindows}
var
  vManager, vService: THandle;
  vPath, vDisplay, vDesc: WideString;
  vInfo: TRALServiceDescription;
begin
  Result := False;
  vManager := OpenSCManagerW(nil, nil, SC_MANAGER_ALL_ACCESS);
  if vManager = 0 then
  begin
    Log('install: no access to the service manager (run as administrator)');
    Exit;
  end;
  try
    vPath := '"' + WideString(ExePath) + '"';
    if FStartArgs <> '' then
      vPath := vPath + ' ' + WideString(FStartArgs);
    vDisplay := WideString(FDisplayName);
    if vDisplay = '' then
      vDisplay := WideString(FServiceName);
    gName := WideString(FServiceName);
    vService := CreateServiceW(vManager, PWideChar(gName), PWideChar(vDisplay),
      SERVICE_ALL_ACCESS_, SERVICE_WIN32_OWN_PROCESS_, SERVICE_AUTO_START_,
      SERVICE_ERROR_NORMAL_, PWideChar(vPath), nil, nil, nil, nil, nil);
    Result := vService <> 0;
    if Result then
    begin
      if FDescription <> '' then
      begin
        vDesc := WideString(FDescription);
        vInfo.lpDescription := PWideChar(vDesc);
        ChangeServiceConfig2W(vService, SERVICE_CONFIG_DESCRIPTION_, @vInfo);
      end;
      CloseServiceHandle(vService);
      Log(FServiceName + ' installed');
    end
    else
      Log('install failed: ' + SysErrorMessage(GetLastError));
  finally
    CloseServiceHandle(vManager);
  end;
end;
{$ELSE}
var
  vUnit: TStringList;
  vDir: string;
begin
  vDir := FUnitDir;
  if vDir = '' then
    vDir := '/etc/systemd/system';
  vUnit := TStringList.Create;
  try
    vUnit.Add('[Unit]');
    if FDescription <> '' then
      vUnit.Add('Description=' + FDescription)
    else
      vUnit.Add('Description=' + FServiceName);
    vUnit.Add('After=network.target');
    vUnit.Add('');
    vUnit.Add('[Service]');
    vUnit.Add('Type=notify');
    vUnit.Add('ExecStart=' + Trim(ExePath + ' ' + FStartArgs));
    vUnit.Add('ExecReload=/bin/kill -HUP $MAINPID');
    vUnit.Add('Restart=on-failure');
    vUnit.Add('TimeoutStopSec=' + IntToStr(FShutdownTimeout div 1000 + 10));
    vUnit.Add('');
    vUnit.Add('[Install]');
    { a user unit (~/.config/systemd/user) has no multi-user.target }
    if Pos('/systemd/user', vDir) > 0 then
      vUnit.Add('WantedBy=default.target')
    else
      vUnit.Add('WantedBy=multi-user.target');
    try
      vUnit.SaveToFile(IncludeTrailingPathDelimiter(vDir) + FServiceName + '.service');
      Result := True;
      Log(Format('%s.service written to %s; run: systemctl daemon-reload && systemctl enable --now %s',
        [FServiceName, vDir, FServiceName]));
    except
      on E: Exception do
      begin
        Result := False;
        Log('install failed: ' + E.Message);
      end;
    end;
  finally
    vUnit.Free;
  end;
end;
{$ENDIF}

function TRALServiceHost.Uninstall: boolean;
{$IFDEF RALWindows}
var
  vManager, vService: THandle;
begin
  Result := False;
  vManager := OpenSCManagerW(nil, nil, SC_MANAGER_ALL_ACCESS);
  if vManager = 0 then
    Exit;
  try
    gName := WideString(FServiceName);
    vService := OpenServiceW(vManager, PWideChar(gName), SERVICE_ALL_ACCESS_);
    if vService = 0 then
      Exit;
    try
      Result := DeleteService(vService);
      if Result then
        Log(FServiceName + ' uninstalled');
    finally
      CloseServiceHandle(vService);
    end;
  finally
    CloseServiceHandle(vManager);
  end;
end;
{$ELSE}
var
  vDir: string;
begin
  vDir := FUnitDir;
  if vDir = '' then
    vDir := '/etc/systemd/system';
  Result := DeleteFile(IncludeTrailingPathDelimiter(vDir) + FServiceName + '.service');
  if Result then
    Log(FServiceName + '.service removed');
end;
{$ENDIF}

function TRALServiceHost.StartService: boolean;
{$IFDEF RALWindows}
var
  vManager, vService: THandle;
begin
  Result := False;
  vManager := OpenSCManagerW(nil, nil, SC_MANAGER_ALL_ACCESS);
  if vManager = 0 then
    Exit;
  try
    gName := WideString(FServiceName);
    vService := OpenServiceW(vManager, PWideChar(gName), SERVICE_ALL_ACCESS_);
    if vService = 0 then
      Exit;
    try
      Result := StartServiceW(vService, 0, nil);
    finally
      CloseServiceHandle(vService);
    end;
  finally
    CloseServiceHandle(vManager);
  end;
end;
{$ELSE}
begin
  Result := Systemctl('start ' + FServiceName);
end;
{$ENDIF}

function TRALServiceHost.StopService: boolean;
{$IFDEF RALWindows}
var
  vManager, vService: THandle;
  vStatus: TRALServiceStatus;
begin
  Result := False;
  vManager := OpenSCManagerW(nil, nil, SC_MANAGER_ALL_ACCESS);
  if vManager = 0 then
    Exit;
  try
    gName := WideString(FServiceName);
    vService := OpenServiceW(vManager, PWideChar(gName), SERVICE_ALL_ACCESS_);
    if vService = 0 then
      Exit;
    try
      Result := ControlService(vService, SERVICE_CONTROL_STOP_, vStatus);
    finally
      CloseServiceHandle(vService);
    end;
  finally
    CloseServiceHandle(vManager);
  end;
end;
{$ELSE}
begin
  Result := Systemctl('stop ' + FServiceName);
end;
{$ENDIF}

function TRALServiceHost.Run: boolean;
{$IFDEF RALWindows}
var
  vTable: array[0..1] of TRALServiceTableEntry;
{$ENDIF}
begin
  Result := True;
  case CommandOf(ParamStr(1)) of
    scInstall: Install;
    scUninstall: Uninstall;
    scStart: StartService;
    scStop: StopService;
    scConsole, scRun: RunConsole;
  else
    {$IFDEF RALWindows}
    gHost := Self;
    gName := WideString(FServiceName);
    vTable[0].lpServiceName := PWideChar(gName);
    vTable[0].lpServiceProc := @ServiceMain;
    vTable[1].lpServiceName := nil;
    vTable[1].lpServiceProc := nil;
    { returns when the service stopped; fails at once outside the SCM }
    if not StartServiceCtrlDispatcherW(@vTable) then
      Result := GetLastError <> ERROR_FAILED_SERVICE_CONTROLLER_CONNECT_;
    {$ELSE}
    { under systemd (or any supervisor) the daemon is the foreground process }
    RunConsole;
    {$ENDIF}
  end;
end;

end.
