/// Sends lines to the system's log: syslog (the journal under systemd) on
/// Linux and macOS, the Event Log on Windows. Linked to a TRALLogPlugin it
/// writes the request lines; the service host's OnLog and the application
/// use Write or WriteText directly.
unit RALSystemLog;

{$I ..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALCustomObjects, RALRequest, RALObservability;

type
  /// How serious a line is (syslog's levels; the Event Log has three).
  TRALSystemLogLevel = (sllDebug, sllInfo, sllNotice, sllWarning, sllError, sllCritical);

  { A source of the system log. Ident names the program in syslog and the
    source in the Event Log (an unregistered source still writes; the viewer
    then shows a note before the text). }
  TRALSystemLog = class(TRALComponent)
  private
    FIdent: string;
    FLevel: TRALSystemLogLevel;
    FLock: TCriticalSection;
    FLogPlugin: TRALLogPlugin;
    FOpened: boolean;
    {$IFDEF RALWindows}
    FHandle: THandle;
    {$ENDIF}
    FRequestLevel: TRALSystemLogLevel;
    procedure SetIdent(const AValue: string);
    procedure SetLogPlugin(const AValue: TRALLogPlugin);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OpenLog;
    procedure CloseLog;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Writes AText at ALevel; lines below Level are dropped.
    procedure Write(ALevel: TRALSystemLogLevel; const AText: string);
    /// Writes AText at the Level (a TGetStrProc: the service host's OnLog).
    procedure WriteText(const AText: string);
    /// The TRALOnLogLine of a log plugin: a 5xx is an error, a 4xx a warning.
    procedure WriteRequestLine(Sender: TObject; ARequest: TRALRequest;
      const ALine: StringRAL);
  published
    /// The program's name in syslog, the source in the Event Log.
    property Ident: string read FIdent write SetIdent;
    /// Lines below this level are not written.
    property Level: TRALSystemLogLevel read FLevel write FLevel default sllInfo;
    /// The plugin whose request lines go to the system log (sets its OnLog).
    property LogPlugin: TRALLogPlugin read FLogPlugin write SetLogPlugin;
    /// The level of a request line answered below 400.
    property RequestLevel: TRALSystemLogLevel read FRequestLevel write FRequestLevel default sllInfo;
  end;

implementation

{$IFDEF RALWindows}
const
  cAdvapi = 'advapi32.dll';
  EVENTLOG_ERROR_TYPE_ = 1;
  EVENTLOG_WARNING_TYPE_ = 2;
  EVENTLOG_INFORMATION_TYPE_ = 4;

function RegisterEventSourceW(AServer, ASource: PWideChar): THandle; stdcall; external cAdvapi;
function DeregisterEventSource(AHandle: THandle): LongBool; stdcall; external cAdvapi;
function ReportEventW(AHandle: THandle; AType, ACategory: Word; AEventID: Cardinal;
  AUserSid: Pointer; ANumStrings: Word; ADataSize: Cardinal; AStrings: Pointer;
  AData: Pointer): LongBool; stdcall; external cAdvapi;
{$ELSE}
const
  {$IFDEF FPC}
  cLibC = 'c';
  {$ELSE}
  cLibC = 'libc.so.6';
  {$ENDIF}
  LOG_PID_ = $01;
  LOG_DAEMON_ = 3 shl 3;
  cSyslogPriority: array[TRALSystemLogLevel] of Integer = (7, 6, 5, 4, 3, 2);

procedure libc_openlog(AIdent: PAnsiChar; AOption, AFacility: Integer); cdecl; external cLibC name 'openlog';
procedure libc_closelog; cdecl; external cLibC name 'closelog';
procedure libc_syslog(APriority: Integer; AFormat: PAnsiChar; AText: PAnsiChar); cdecl; external cLibC name 'syslog';
{$ENDIF}

{ TRALSystemLog }

constructor TRALSystemLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FIdent := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  FLevel := sllInfo;
  FRequestLevel := sllInfo;
end;

destructor TRALSystemLog.Destroy;
begin
  SetLogPlugin(nil);
  CloseLog;
  FreeAndNil(FLock);
  inherited;
end;

procedure TRALSystemLog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FLogPlugin) then
    FLogPlugin := nil;
end;

procedure TRALSystemLog.SetIdent(const AValue: string);
begin
  if AValue = FIdent then
    Exit;
  CloseLog;
  FIdent := AValue;
end;

procedure TRALSystemLog.SetLogPlugin(const AValue: TRALLogPlugin);
begin
  if FLogPlugin = AValue then
    Exit;
  if FLogPlugin <> nil then
  begin
    FLogPlugin.OnLog := nil;
    FLogPlugin.RemoveFreeNotification(Self);
  end;
  FLogPlugin := AValue;
  if FLogPlugin <> nil then
  begin
    FLogPlugin.FreeNotification(Self);
    FLogPlugin.OnLog := {$IFDEF FPC}@{$ENDIF}WriteRequestLine;
  end;
end;

{$IFNDEF RALWindows}
var
  { syslog keeps the pointer it gets: the text lives as long as the process }
  gIdent: AnsiString;
{$ENDIF}

procedure TRALSystemLog.OpenLog;
begin
  if FOpened then
    Exit;
  {$IFDEF RALWindows}
  FHandle := RegisterEventSourceW(nil, PWideChar(WideString(FIdent)));
  FOpened := FHandle <> 0;
  {$ELSE}
  gIdent := AnsiString(FIdent);
  libc_openlog(PAnsiChar(gIdent), LOG_PID_, LOG_DAEMON_);
  FOpened := True;
  {$ENDIF}
end;

procedure TRALSystemLog.CloseLog;
begin
  if not FOpened then
    Exit;
  {$IFDEF RALWindows}
  DeregisterEventSource(FHandle);
  FHandle := 0;
  {$ELSE}
  libc_closelog;
  {$ENDIF}
  FOpened := False;
end;

procedure TRALSystemLog.Write(ALevel: TRALSystemLogLevel; const AText: string);
{$IFDEF RALWindows}
var
  vText: WideString;
  vPtr: PWideChar;
  vType: Word;
{$ELSE}
var
  vText: AnsiString;
{$ENDIF}
begin
  if (ALevel < FLevel) or (AText = '') then
    Exit;
  FLock.Acquire;
  try
    OpenLog;
    if not FOpened then
      Exit;
    {$IFDEF RALWindows}
    case ALevel of
      sllWarning: vType := EVENTLOG_WARNING_TYPE_;
      sllError, sllCritical: vType := EVENTLOG_ERROR_TYPE_;
    else
      vType := EVENTLOG_INFORMATION_TYPE_;
    end;
    vText := WideString(AText);
    vPtr := PWideChar(vText);
    ReportEventW(FHandle, vType, 0, 0, nil, 1, 0, @vPtr, nil);
    {$ELSE}
    vText := {$IFDEF FPC}AnsiString(AText){$ELSE}UTF8Encode(AText){$ENDIF};
    libc_syslog(LOG_DAEMON_ or cSyslogPriority[ALevel], '%s', PAnsiChar(vText));
    {$ENDIF}
  finally
    FLock.Release;
  end;
end;

procedure TRALSystemLog.WriteText(const AText: string);
begin
  Write(FLevel, AText);
end;

procedure TRALSystemLog.WriteRequestLine(Sender: TObject; ARequest: TRALRequest;
  const ALine: StringRAL);
var
  vLevel: TRALSystemLogLevel;
begin
  vLevel := FRequestLevel;
  if ARequest <> nil then
  begin
    if ARequest.ResponseStatus >= 500 then
      vLevel := sllError
    else if ARequest.ResponseStatus >= 400 then
      vLevel := sllWarning;
  end;
  Write(vLevel, string(ALine));
end;

end.
