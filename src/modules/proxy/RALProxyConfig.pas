/// A configuration in the style of NGINX for TRALBalancer (RALModuleProxyPackage):
/// a subset of its syntax (http and stream blocks, upstream, server, listen,
/// server_name, location, proxy_pass, TLS, timeouts, include) read by a parser
/// of RAL's own, which stops at the first unknown directive and says the file,
/// line and column. TRALProxyHost runs what a configuration describes and
/// reloads another one without cutting the connections that are open.
unit RALProxyConfig;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALExceptions, RALTypes, RALConsts, RALTLS, RALProxyCore, RALBalancer;

type
  /// A configuration that cannot be used: where, and why.
  ERALProxyConfigError = class(ERALException)
  private
    FColumn: IntegerRAL;
    FFileName: string;
    FLine: IntegerRAL;
  public
    /// The error AMessage at AFileName, ALine, AColumn.
    constructor CreateAt(const AFileName: string; ALine, AColumn: IntegerRAL;
      const AMessage: string);
    /// Column (from 1) of the token at fault.
    property Column: IntegerRAL read FColumn;
    /// File the token came from ('' for text).
    property FileName: string read FFileName;
    /// Line (from 1) of the token at fault.
    property Line: IntegerRAL read FLine;
  end;

  { A configuration, read: the upstreams and the balancers it describes (one
    per listening port and mode), not started. It owns them until a
    TRALProxyHost takes the balancers }
  TRALProxyConfig = class
  private
    FAccessLog: string;
    FAccessLogFormat: string;
    FBalancers: TList;
    FErrorLog: string;
    FFileName: string;
    FPidFile: string;
    FThreadCount: IntegerRAL;
    FUpstreams: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    /// Reads AFileName (and what it includes); raises ERALProxyConfigError.
    procedure LoadFromFile(const AFileName: string);
    { Reads AText; AFileName names it in the errors and is the folder of
      relative includes }
    procedure LoadFromText(const AText: string; const AFileName: string = '');
    /// The balancer listening on APort, or nil.
    function BalancerOf(APort: IntegerRAL): TRALBalancer;
    /// The upstream named AName, or nil.
    function UpstreamOf(const AName: string): TRALUpstream;

    /// File of access_log; empty: off.
    property AccessLog: string read FAccessLog write FAccessLog;
    /// Format of access_log: 'combined' (default) or 'json'.
    property AccessLogFormat: string read FAccessLogFormat write FAccessLogFormat;
    /// The balancers (TRALBalancer), one per port.
    property Balancers: TList read FBalancers;
    /// File of error_log; empty: the standard error.
    property ErrorLog: string read FErrorLog write FErrorLog;
    /// The file read.
    property FileName: string read FFileName;
    /// File of pid.
    property PidFile: string read FPidFile write FPidFile;
    /// worker_threads (worker_processes): poll threads of each balancer; 0 is one per processor.
    property ThreadCount: IntegerRAL read FThreadCount write FThreadCount;
    /// The upstreams by name (Objects are TRALUpstream).
    property Upstreams: TStringList read FUpstreams;
  end;

  { Runs a configuration: starts its balancers, and on Reload takes another
    one in: a port that stays keeps its listener and its open connections
    (they finish on the old routes), a new port opens, a port gone closes.
    The configurations replaced are kept until Stop, as requests in flight
    still use their upstreams }
  TRALProxyHost = class
  private
    FConfig: TRALProxyConfig;
    FLock: TCriticalSection;
    FOnAccess: TRALBalancerAccessEvent;
    FRetired: TList;
    FRunning: TList;

    function Key(ABalancer: TRALBalancer): string;
    function Running(const AKey: string): TRALBalancer;
    procedure SetOnAccess(const AValue: TRALBalancerAccessEvent);
  public
    constructor Create;
    destructor Destroy; override;

    { Starts AConfig (owned from now on). Raises, with nothing left running,
      when a port does not open }
    procedure Start(AConfig: TRALProxyConfig);
    { Takes ANew (owned from now on) in place of the configuration running.
      Raises when a new port does not open; what was running keeps running }
    procedure Reload(ANew: TRALProxyConfig);
    /// Stops every balancer and frees the configurations.
    procedure Stop;
    /// The statistics of every balancer, as one JSON object by port.
    function StatsJSON: StringRAL;

    /// The configuration in force.
    property Config: TRALProxyConfig read FConfig;
    /// The balancers running (TRALBalancer).
    property Balancers: TList read FRunning;
    /// Every request or connection that ends, on a balancer's thread.
    property OnAccess: TRALBalancerAccessEvent read FOnAccess write SetOnAccess;
  end;

{ Milliseconds of an NGINX time ('30s', '500ms', '2m', '1h', '1d'; a bare
  number is seconds); -1 when it is not one }
function RALProxyParseTime(const AText: string): Int64RAL;
{ Bytes of an NGINX size ('16k', '1m', '512'); -1 when it is not one }
function RALProxyParseSize(const AText: string): Int64RAL;

implementation

type
  // one token of the text: a word, a string, a semicolon or a brace
  TRALProxyToken = class
  public
    Text: string;
    Quoted: boolean;
    FileName: string;
    Line: IntegerRAL;
    Column: IntegerRAL;
  end;

  { one directive: its name, its values, and its block when it has one }
  TRALProxyDirective = class
  public
    Name: TRALProxyToken;
    Args: TList;
    Children: TList;
    HasBlock: boolean;
    constructor Create;
    destructor Destroy; override;
    function Arg(AIndex: IntegerRAL): string;
    function ArgCount: IntegerRAL;
  end;

  { reads tokens out of the text and of what it includes, then the tree }
  TRALProxyParser = class
  private
    FTokens: TList;
    FPos: IntegerRAL;
    FDepth: IntegerRAL;

    procedure Tokenize(const AText, AFileName: string);
    procedure Include(const APattern, ABase: string; AToken: TRALProxyToken);
    function ParseBlock(AList: TList; AInBlock: boolean; AOpen: TRALProxyToken): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Parse(const AText, AFileName: string): TList;
  end;

  /// A server block of http, kept until its port's balancer is built.
  THttpServerBlock = class
  public
    Port: IntegerRAL;
    Bind: string;
    SSL: boolean;
    Names: TStringList;
    CertFile, KeyFile: string;
    Locations: TList; // TRALBalancerRoute-like data: prefix, upstream, strip
    StatsPath: string;
    PreserveHost: boolean;
    MinVersion: TRALTLSVersion;
    Where: TRALProxyDirective;
    constructor Create;
    destructor Destroy; override;
  end;

  TLocation = class
  public
    Prefix: string;
    Upstream: TRALUpstream;
    Strip: boolean;
  end;

procedure Fail(AToken: TRALProxyToken; const AMessage: string); overload;
begin
  if AToken = nil then
    raise ERALProxyConfigError.CreateAt('', 0, 0, AMessage)
  else
    raise ERALProxyConfigError.CreateAt(AToken.FileName, AToken.Line, AToken.Column, AMessage);
end;

procedure Fail(ADirective: TRALProxyDirective; const AMessage: string); overload;
begin
  Fail(ADirective.Name, AMessage);
end;

function RALProxyParseTime(const AText: string): Int64RAL;
var
  vNum: string;
  vUnit: string;
  vInt: IntegerRAL;
  vValue: Int64RAL;
begin
  Result := -1;
  vInt := 1;
  while (vInt <= Length(AText)) and CharInSet(AText[vInt], ['0'..'9']) do
    Inc(vInt);
  vNum := Copy(AText, 1, vInt - 1);
  vUnit := LowerCase(Copy(AText, vInt, MaxInt));
  if vNum = '' then
    Exit;
  vValue := StrToInt64Def(vNum, -1);
  if vValue < 0 then
    Exit;
  if (vUnit = '') or (vUnit = 's') then
    Result := vValue * 1000
  else if vUnit = 'ms' then
    Result := vValue
  else if vUnit = 'm' then
    Result := vValue * 60000
  else if vUnit = 'h' then
    Result := vValue * 3600000
  else if vUnit = 'd' then
    Result := vValue * 86400000;
end;

function RALProxyParseSize(const AText: string): Int64RAL;
var
  vNum, vUnit: string;
  vInt: IntegerRAL;
  vValue: Int64RAL;
begin
  Result := -1;
  vInt := 1;
  while (vInt <= Length(AText)) and CharInSet(AText[vInt], ['0'..'9']) do
    Inc(vInt);
  vNum := Copy(AText, 1, vInt - 1);
  vUnit := LowerCase(Copy(AText, vInt, MaxInt));
  vValue := StrToInt64Def(vNum, -1);
  if (vNum = '') or (vValue < 0) then
    Exit;
  if vUnit = '' then
    Result := vValue
  else if vUnit = 'k' then
    Result := vValue * 1024
  else if vUnit = 'm' then
    Result := vValue * 1024 * 1024;
end;

{ ERALProxyConfigError }

constructor ERALProxyConfigError.CreateAt(const AFileName: string; ALine,
  AColumn: IntegerRAL; const AMessage: string);
var
  vWhere: string;
begin
  vWhere := AFileName;
  if vWhere = '' then
    vWhere := '(text)';
  inherited Create(Format(emProxyCfgAt, [vWhere, ALine, AColumn, AMessage]));
  FFileName := AFileName;
  FLine := ALine;
  FColumn := AColumn;
end;

{ TRALProxyDirective }

constructor TRALProxyDirective.Create;
begin
  inherited Create;
  Args := TList.Create;
  Children := TList.Create;
end;

destructor TRALProxyDirective.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Children.Count - 1 do
    TObject(Children[vInt]).Free;
  Children.Free;
  Args.Free; // the tokens belong to the parser
  inherited Destroy;
end;

function TRALProxyDirective.Arg(AIndex: IntegerRAL): string;
begin
  if (AIndex >= 0) and (AIndex < Args.Count) then
    Result := TRALProxyToken(Args[AIndex]).Text
  else
    Result := '';
end;

function TRALProxyDirective.ArgCount: IntegerRAL;
begin
  Result := Args.Count;
end;

{ TRALProxyParser }

constructor TRALProxyParser.Create;
begin
  inherited Create;
  FTokens := TList.Create;
end;

destructor TRALProxyParser.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FTokens.Count - 1 do
    TObject(FTokens[vInt]).Free;
  FTokens.Free;
  inherited Destroy;
end;

procedure TRALProxyParser.Tokenize(const AText, AFileName: string);
var
  vPos, vLen, vLine, vCol, vStart, vStartCol: IntegerRAL;
  vChar, vQuote: Char;
  vToken: TRALProxyToken;
  vText: string;

  procedure Add(const S: string; AQuoted: boolean; ALine, ACol: IntegerRAL);
  begin
    vToken := TRALProxyToken.Create;
    vToken.Text := S;
    vToken.Quoted := AQuoted;
    vToken.FileName := AFileName;
    vToken.Line := ALine;
    vToken.Column := ACol;
    FTokens.Add(vToken);
  end;

begin
  vLen := Length(AText);
  vPos := 1;
  vLine := 1;
  vCol := 1;
  while vPos <= vLen do
  begin
    vChar := AText[vPos];
    if vChar = #10 then
    begin
      Inc(vLine);
      vCol := 1;
      Inc(vPos);
      Continue;
    end;
    if (vChar = ' ') or (vChar = #9) or (vChar = #13) then
    begin
      Inc(vPos);
      Inc(vCol);
      Continue;
    end;
    if vChar = '#' then
    begin
      while (vPos <= vLen) and (AText[vPos] <> #10) do
        Inc(vPos);
      Continue;
    end;
    if (vChar = ';') or (vChar = '{') or (vChar = '}') then
    begin
      Add(vChar, False, vLine, vCol);
      Inc(vPos);
      Inc(vCol);
      Continue;
    end;
    if (vChar = '"') or (vChar = '''') then
    begin
      vQuote := vChar;
      vStartCol := vCol;
      vStart := vLine;
      Inc(vPos);
      Inc(vCol);
      vText := '';
      while (vPos <= vLen) and (AText[vPos] <> vQuote) do
      begin
        if (AText[vPos] = '\') and (vPos < vLen) then
        begin
          Inc(vPos);
          Inc(vCol);
        end;
        if AText[vPos] = #10 then
        begin
          Inc(vLine);
          vCol := 0;
        end;
        vText := vText + AText[vPos];
        Inc(vPos);
        Inc(vCol);
      end;
      if vPos > vLen then
        raise ERALProxyConfigError.CreateAt(AFileName, vStart, vStartCol, emProxyCfgUnexpectedEnd);
      Inc(vPos);
      Inc(vCol);
      Add(vText, True, vStart, vStartCol);
      Continue;
    end;
    vStart := vPos;
    vStartCol := vCol;
    while (vPos <= vLen) and not CharInSet(AText[vPos], [' ', #9, #13, #10, ';', '{', '}', '#', '"', '''']) do
    begin
      Inc(vPos);
      Inc(vCol);
    end;
    Add(Copy(AText, vStart, vPos - vStart), False, vLine, vStartCol);
  end;
end;

procedure TRALProxyParser.Include(const APattern, ABase: string; AToken: TRALProxyToken);
var
  vPath, vFolder: string;
  vFiles: TStringList;
  vRec: TSearchRec;
  vInt: IntegerRAL;
  vText: TStringList;
  vTail: TList;
  vCount: IntegerRAL;
begin
  vPath := APattern;
  if (vPath <> '') and not ((vPath[1] = '/') or (vPath[1] = '\') or
     ((Length(vPath) > 1) and (vPath[2] = ':'))) then
    vPath := IncludeTrailingPathDelimiter(ExtractFilePath(ABase)) + vPath;
  vPath := StringReplace(vPath, '/', PathDelim, [rfReplaceAll]);
  vPath := StringReplace(vPath, '\', PathDelim, [rfReplaceAll]);
  vFiles := TStringList.Create;
  try
    if (Pos('*', vPath) > 0) or (Pos('?', vPath) > 0) then
    begin
      vFolder := ExtractFilePath(vPath);
      if FindFirst(vPath, faAnyFile, vRec) = 0 then
      begin
        repeat
          if (vRec.Attr and faDirectory) = 0 then
            vFiles.Add(vFolder + vRec.Name);
        until FindNext(vRec) <> 0;
        FindClose(vRec);
      end;
      vFiles.Sort; // in name order, as NGINX reads them
    end
    else
    begin
      if not FileExists(vPath) then
        Fail(AToken, Format(emProxyCfgFile, [vPath]));
      vFiles.Add(vPath);
    end;
    { the tokens of the files go where the include was: everything read after
      it is moved behind them }
    vTail := TList.Create;
    try
      vCount := FTokens.Count;
      for vInt := 0 to vFiles.Count - 1 do
      begin
        vText := TStringList.Create;
        try
          try
            vText.LoadFromFile(vFiles[vInt]);
          except
            Fail(AToken, Format(emProxyCfgFile, [vFiles[vInt]]));
          end;
          Tokenize(vText.Text, vFiles[vInt]);
        finally
          vText.Free;
        end;
      end;
      for vInt := vCount to FTokens.Count - 1 do
        vTail.Add(FTokens[vInt]);
      FTokens.Count := vCount;
      for vInt := vTail.Count - 1 downto 0 do
        FTokens.Insert(FPos, vTail[vInt]);
    finally
      vTail.Free;
    end;
  finally
    vFiles.Free;
  end;
end;

function TRALProxyParser.ParseBlock(AList: TList; AInBlock: boolean;
  AOpen: TRALProxyToken): boolean;
var
  vToken: TRALProxyToken;
  vDir: TRALProxyDirective;
begin
  Result := True;
  Inc(FDepth);
  if FDepth > 32 then
    Fail(AOpen, emProxyCfgUnexpectedEnd);
  while FPos < FTokens.Count do
  begin
    vToken := TRALProxyToken(FTokens[FPos]);
    Inc(FPos);
    if (vToken.Text = '}') and not vToken.Quoted then
    begin
      if not AInBlock then
        Fail(vToken, Format(emProxyCfgExpected, ['directive', '}']));
      Dec(FDepth);
      Exit;
    end;
    if (not vToken.Quoted) and ((vToken.Text = ';') or (vToken.Text = '{')) then
      Fail(vToken, Format(emProxyCfgExpected, ['directive', vToken.Text]));
    vDir := TRALProxyDirective.Create;
    AList.Add(vDir);
    vDir.Name := vToken;
    repeat
      if FPos >= FTokens.Count then
        Fail(vToken, emProxyCfgUnexpectedEnd);
      vToken := TRALProxyToken(FTokens[FPos]);
      Inc(FPos);
      if (not vToken.Quoted) and (vToken.Text = ';') then
        Break;
      if (not vToken.Quoted) and (vToken.Text = '{') then
      begin
        vDir.HasBlock := True;
        ParseBlock(vDir.Children, True, vToken);
        Break;
      end;
      if (not vToken.Quoted) and (vToken.Text = '}') then
        Fail(vToken, Format(emProxyCfgExpected, [';', '}']));
      vDir.Args.Add(vToken);
    until False;
    // include is resolved here: its tokens come next
    if (vDir.Name.Text = 'include') and not vDir.HasBlock then
    begin
      if vDir.ArgCount <> 1 then
        Fail(vDir, Format(emProxyCfgArgs, ['include']));
      Include(vDir.Arg(0), vDir.Name.FileName, vDir.Name);
      AList.Remove(vDir);
      vDir.Free;
    end;
  end;
  if AInBlock then
    Fail(AOpen, emProxyCfgUnexpectedEnd);
  Dec(FDepth);
end;

function TRALProxyParser.Parse(const AText, AFileName: string): TList;
begin
  Tokenize(AText, AFileName);
  FPos := 0;
  Result := TList.Create;
  try
    ParseBlock(Result, False, nil);
  except
    while Result.Count > 0 do
    begin
      TObject(Result[0]).Free;
      Result.Delete(0);
    end;
    Result.Free;
    raise;
  end;
end;

{ THttpServerBlock }

constructor THttpServerBlock.Create;
begin
  inherited Create;
  Names := TStringList.Create;
  Locations := TList.Create;
  MinVersion := mtv12;
end;

destructor THttpServerBlock.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Locations.Count - 1 do
    TObject(Locations[vInt]).Free;
  Locations.Free;
  Names.Free;
  inherited Destroy;
end;

{ named hosts before any host; within a host, the longest prefix first (the
  location NGINX picks); stable otherwise }
procedure SortRoutes(ABalancer: TRALBalancer);
var
  vI, vJ: IntegerRAL;
  vA, vB: TRALBalancerRoute;

  function Before(X, Y: TRALBalancerRoute): boolean;
  begin
    if (X.HostName <> '') <> (Y.HostName <> '') then
      Result := X.HostName <> ''
    else if X.HostName <> Y.HostName then
      Result := False
    else
      Result := Length(X.PathPrefix) > Length(Y.PathPrefix);
  end;

begin
  for vI := 1 to ABalancer.Routes.Count - 1 do
  begin
    vJ := vI;
    while vJ > 0 do
    begin
      vA := TRALBalancerRoute(ABalancer.Routes.Items[vJ]);
      vB := TRALBalancerRoute(ABalancer.Routes.Items[vJ - 1]);
      if not Before(vA, vB) then
        Break;
      vA.Index := vJ - 1;
      Dec(vJ);
    end;
  end;
end;

{ TRALProxyConfig }

constructor TRALProxyConfig.Create;
begin
  inherited Create;
  FBalancers := TList.Create;
  FUpstreams := TStringList.Create;
  FUpstreams.Sorted := True;
  FAccessLogFormat := 'combined';
end;

destructor TRALProxyConfig.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FBalancers.Count - 1 do
    TObject(FBalancers[vInt]).Free;
  FBalancers.Free;
  for vInt := 0 to FUpstreams.Count - 1 do
    FUpstreams.Objects[vInt].Free;
  FUpstreams.Free;
  inherited Destroy;
end;

function TRALProxyConfig.BalancerOf(APort: IntegerRAL): TRALBalancer;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FBalancers.Count - 1 do
  begin
    Result := TRALBalancer(FBalancers[vInt]);
    if Result.Port = APort then
      Exit;
  end;
  Result := nil;
end;

function TRALProxyConfig.UpstreamOf(const AName: string): TRALUpstream;
var
  vIndex: IntegerRAL;
begin
  vIndex := FUpstreams.IndexOf(AName);
  if vIndex >= 0 then
    Result := TRALUpstream(FUpstreams.Objects[vIndex])
  else
    Result := nil;
end;

procedure TRALProxyConfig.LoadFromFile(const AFileName: string);
var
  vText: TStringList;
begin
  vText := TStringList.Create;
  try
    try
      vText.LoadFromFile(AFileName);
    except
      raise ERALProxyConfigError.CreateAt(AFileName, 0, 0, Format(emProxyCfgFile, [AFileName]));
    end;
    LoadFromText(vText.Text, AFileName);
  finally
    vText.Free;
  end;
end;

procedure TRALProxyConfig.LoadFromText(const AText: string; const AFileName: string);
var
  vParser: TRALProxyParser;
  vRoot: TList;
  vInt, vJ: IntegerRAL;
  vDir: TRALProxyDirective;
  vHttpBlocks: TList;
  vImplicit: IntegerRAL;

  function ToInt(ADir: TRALProxyDirective; const AValue: string): IntegerRAL;
  begin
    Result := StrToIntDef(AValue, -1);
    if Result < 0 then
      Fail(ADir, Format(emProxyCfgValue, [ADir.Name.Text, AValue]));
  end;

  function ToTime(ADir: TRALProxyDirective; const AValue: string): IntegerRAL;
  var
    vMs: Int64RAL;
  begin
    vMs := RALProxyParseTime(AValue);
    if (vMs < 0) or (vMs > MaxInt) then
      Fail(ADir, Format(emProxyCfgValue, [ADir.Name.Text, AValue]));
    Result := IntegerRAL(vMs);
  end;

  procedure NeedArgs(ADir: TRALProxyDirective; AMin, AMax: IntegerRAL);
  begin
    if (ADir.ArgCount < AMin) or ((AMax >= 0) and (ADir.ArgCount > AMax)) then
      Fail(ADir, Format(emProxyCfgArgs, [ADir.Name.Text]));
    if ADir.HasBlock then
      Fail(ADir, Format(emProxyCfgExpected, [';', '{']));
  end;

  procedure NeedBlock(ADir: TRALProxyDirective);
  begin
    if not ADir.HasBlock then
      Fail(ADir, Format(emProxyCfgExpected, ['{', ';']));
  end;

  procedure Unknown(ADir: TRALProxyDirective; const AContext: string);
  begin
    Fail(ADir, Format(emProxyCfgUnknownDirective, [ADir.Name.Text, AContext]));
  end;

  { "server host:port weight=2 ..." of an upstream }
  procedure UpstreamServer(AUp: TRALUpstream; ADir: TRALProxyDirective; AStream: boolean);
  var
    vServer: TRALUpstreamServer;
    vArg, vName, vValue, vAddress: string;
    vK, vEq: IntegerRAL;
  begin
    NeedArgs(ADir, 1, -1);
    vAddress := ADir.Arg(0);
    if Pos('://', vAddress) = 0 then
    begin
      if AStream then
        vAddress := 'tcp://' + vAddress
      else
        vAddress := 'http://' + vAddress;
    end;
    vServer := AUp.Servers.AddServer(StringRAL(vAddress));
    for vK := 1 to ADir.ArgCount - 1 do
    begin
      vArg := ADir.Arg(vK);
      vEq := Pos('=', vArg);
      if vEq > 0 then
      begin
        vName := Copy(vArg, 1, vEq - 1);
        vValue := Copy(vArg, vEq + 1, MaxInt);
      end
      else
      begin
        vName := vArg;
        vValue := '';
      end;
      if vName = 'weight' then
        vServer.Weight := ToInt(ADir, vValue)
      else if vName = 'max_fails' then
        AUp.MaxFails := ToInt(ADir, vValue)
      else if vName = 'fail_timeout' then
        AUp.FailTimeout := ToTime(ADir, vValue)
      else if (vName = 'max_conns') then
        vServer.MaxConnections := ToInt(ADir, vValue)
      else if (vName = 'backup') and (vValue = '') then
        vServer.Backup := True
      else if (vName = 'down') and (vValue = '') then
        vServer.Down := True
      else
        Fail(TRALProxyToken(ADir.Args[vK]), Format(emProxyCfgValue, ['server', vArg]));
    end;
  end;

  procedure ReadUpstream(ADir: TRALProxyDirective; AStream: boolean);
  var
    vUp: TRALUpstream;
    vK, vM, vEq: IntegerRAL;
    vChild: TRALProxyDirective;
    vName, vKey, vArg: string;
  begin
    NeedBlock(ADir);
    if ADir.ArgCount <> 1 then
      Fail(ADir, Format(emProxyCfgArgs, ['upstream']));
    vName := ADir.Arg(0);
    if FUpstreams.IndexOf(vName) >= 0 then
      Fail(ADir, Format(emProxyCfgDuplicateUpstream, [vName]));
    vUp := TRALUpstream.Create(nil);
    FUpstreams.AddObject(vName, vUp);
    for vK := 1 to Length(vName) do
      if not CharInSet(vName[vK], ['a'..'z', 'A'..'Z', '0'..'9', '_']) then
      begin
        vUp := nil;
        Break;
      end;
    if vUp <> nil then
      vUp.Name := vName;
    vUp := UpstreamOf(vName);
    for vK := 0 to ADir.Children.Count - 1 do
    begin
      vChild := TRALProxyDirective(ADir.Children[vK]);
      if vChild.Name.Text = 'server' then
        UpstreamServer(vUp, vChild, AStream)
      else if vChild.Name.Text = 'least_conn' then
      begin
        NeedArgs(vChild, 0, 0);
        vUp.Policy := lpLeastConnections;
      end
      else if vChild.Name.Text = 'ip_hash' then
      begin
        NeedArgs(vChild, 0, 0);
        vUp.Policy := lpIPHash;
      end
      else if vChild.Name.Text = 'random' then
      begin
        NeedArgs(vChild, 0, 1);
        if (vChild.ArgCount = 1) and (vChild.Arg(0) <> 'two') then
          Fail(vChild, Format(emProxyCfgValue, ['random', vChild.Arg(0)]));
        vUp.Policy := lpRandomTwo;
      end
      else if vChild.Name.Text = 'hash' then
      begin
        NeedArgs(vChild, 1, 2);
        if (vChild.ArgCount = 2) and (vChild.Arg(1) <> 'consistent') then
          Fail(vChild, Format(emProxyCfgValue, ['hash', vChild.Arg(1)]));
        vKey := vChild.Arg(0);
        vUp.Policy := lpConsistentHash;
        if (vKey = '$request_uri') or (vKey = '$uri') then
          vUp.HashBy := uhPath
        else if Copy(vKey, 1, 6) = '$http_' then
        begin
          vUp.HashBy := uhHeader;
          vUp.HashName := StringRAL(StringReplace(Copy(vKey, 7, MaxInt), '_', '-', [rfReplaceAll]));
        end
        else if Copy(vKey, 1, 8) = '$cookie_' then
        begin
          vUp.HashBy := uhCookie;
          vUp.HashName := StringRAL(Copy(vKey, 9, MaxInt));
        end
        else if (vKey = '$remote_addr') or (vKey = '$binary_remote_addr') then
          vUp.Policy := lpIPHash
        else if AStream and (vKey = '$ssl_preread_server_name') then
          vUp.HashBy := uhPath // TCP mode hashes the SNI name
        else
          Fail(vChild, Format(emProxyCfgValue, ['hash', vKey]));
      end
      else if vChild.Name.Text = 'sticky' then
      begin
        NeedArgs(vChild, 2, 2);
        if vChild.Arg(0) <> 'cookie' then
          Fail(vChild, Format(emProxyCfgValue, ['sticky', vChild.Arg(0)]));
        vUp.Sticky.Enabled := True;
        vUp.Sticky.CookieName := StringRAL(vChild.Arg(1));
      end
      else if vChild.Name.Text = 'health_check' then
      begin
        NeedArgs(vChild, 0, -1);
        vUp.HealthCheck.Enabled := True;
        for vM := 0 to vChild.ArgCount - 1 do
        begin
          vArg := vChild.Arg(vM);
          vEq := Pos('=', vArg);
          vKey := Copy(vArg, 1, vEq - 1);
          vArg := Copy(vArg, vEq + 1, MaxInt);
          if vKey = 'uri' then
            vUp.HealthCheck.Path := StringRAL(vArg)
          else if vKey = 'interval' then
            vUp.HealthCheck.Interval := ToTime(vChild, vArg)
          else if vKey = 'fails' then
            vUp.HealthCheck.Falls := ToInt(vChild, vArg)
          else if vKey = 'passes' then
            vUp.HealthCheck.Rises := ToInt(vChild, vArg)
          else if vKey = 'timeout' then
            vUp.HealthCheck.Timeout := ToTime(vChild, vArg)
          else if vKey = 'status' then
            vUp.HealthCheck.ExpectedStatus := ToInt(vChild, vArg)
          else
            Fail(TRALProxyToken(vChild.Args[vM]), Format(emProxyCfgValue, ['health_check', vChild.Arg(vM)]));
        end;
      end
      else if vChild.Name.Text = 'keepalive' then
        NeedArgs(vChild, 1, 1) // kept connections are always on; the number is NGINX's
      else
        Unknown(vChild, 'upstream');
    end;
    if vUp.Servers.Count = 0 then
      Fail(ADir, Format(emProxyCfgEmptyUpstream, [vName]));
  end;

  { the balancer of a port: made once, shared by the server blocks on it }
  function BalancerFor(ADir: TRALProxyDirective; APort: IntegerRAL; const ABind: string;
    AMode: TRALBalancerMode): TRALBalancer;
  begin
    Result := BalancerOf(APort);
    if Result <> nil then
    begin
      if Result.Mode <> AMode then
        Fail(ADir, Format(emProxyCfgPortConflict, [APort]));
      Exit;
    end;
    Result := TRALBalancer.Create(nil);
    FBalancers.Add(Result);
    Result.Mode := AMode;
    Result.Port := APort;
    if ABind <> '' then
      Result.IPConfig.IPv4Bind := StringRAL(ABind);
    Result.ThreadCount := FThreadCount;
  end;

  { "listen 8080", "listen 10.0.0.1:443 ssl" }
  procedure ReadListen(ADir: TRALProxyDirective; out APort: IntegerRAL; out ABind: string;
    out ASSL: boolean);
  var
    vText: string;
    vColon, vK: IntegerRAL;
  begin
    NeedArgs(ADir, 1, -1);
    vText := ADir.Arg(0);
    ABind := '';
    vColon := LastDelimiter(':', vText);
    if vColon > 0 then
    begin
      ABind := Copy(vText, 1, vColon - 1);
      if (ABind = '*') or (ABind = '0.0.0.0') then
        ABind := '';
      vText := Copy(vText, vColon + 1, MaxInt);
    end;
    APort := StrToIntDef(vText, -1);
    if (APort <= 0) or (APort > 65535) then
      Fail(ADir, Format(emProxyCfgValue, ['listen', ADir.Arg(0)]));
    ASSL := False;
    for vK := 1 to ADir.ArgCount - 1 do
      if ADir.Arg(vK) = 'ssl' then
        ASSL := True
      else if (ADir.Arg(vK) = 'default_server') or (ADir.Arg(vK) = 'reuseport') then
        // NGINX words with nothing to do here
      else
        Fail(TRALProxyToken(ADir.Args[vK]), Format(emProxyCfgValue, ['listen', ADir.Arg(vK)]));
  end;

  { proxy_pass: an upstream by name, or an address made into an upstream of one }
  function ProxyTarget(ADir: TRALProxyDirective; AStream: boolean; out AStrip: boolean): TRALUpstream;
  var
    vText, vScheme, vHost, vUri: string;
    vPos: IntegerRAL;
  begin
    NeedArgs(ADir, 1, 1);
    vText := ADir.Arg(0);
    vScheme := '';
    AStrip := False;
    vPos := Pos('://', vText);
    if vPos > 0 then
    begin
      vScheme := LowerCase(Copy(vText, 1, vPos - 1));
      if (vScheme <> 'http') and (vScheme <> 'https') then
        Fail(ADir, Format(emProxyCfgValue, ['proxy_pass', vText]));
      vText := Copy(vText, vPos + 3, MaxInt);
    end
    else if not AStream then
      Fail(ADir, Format(emProxyCfgValue, ['proxy_pass', vText]));
    vPos := Pos('/', vText);
    vUri := '';
    if vPos > 0 then
    begin
      vUri := Copy(vText, vPos, MaxInt);
      vHost := Copy(vText, 1, vPos - 1);
    end
    else
      vHost := vText;
    // NGINX: a URI after the address takes the place of the location's prefix
    if vUri = '/' then
      AStrip := True
    else if vUri <> '' then
      Fail(ADir, Format(emProxyCfgUnsupported, ['proxy_pass', 'http://name or http://name/']));
    Result := UpstreamOf(vHost);
    if Result <> nil then
    begin
      if vScheme = 'https' then
        Fail(ADir, Format(emProxyCfgValue, ['proxy_pass', ADir.Arg(0)]));
      Exit;
    end;
    // an address: an upstream of its own, named after it
    if Pos(':', vHost) = 0 then
    begin
      if AStream then
        Fail(ADir, Format(emProxyCfgUnknownUpstream, [vHost]));
    end;
    Inc(vImplicit);
    Result := TRALUpstream.Create(nil);
    FUpstreams.AddObject('#' + IntToStr(vImplicit) + ' ' + vHost, Result);
    if vScheme = '' then
      Result.Servers.AddServer(StringRAL('tcp://' + vHost))
    else
      Result.Servers.AddServer(StringRAL(vScheme + '://' + vHost));
  end;

  procedure ReadHttpServer(ADir: TRALProxyDirective; AHttpTimeouts: TStringList);
  var
    vBlock: THttpServerBlock;
    vK, vM, vPort: IntegerRAL;
    vChild, vLoc: TRALProxyDirective;
    vBind: string;
    vSSL, vHasListen: boolean;
    vLocation: TLocation;
    vPrefix: string;
  begin
    NeedBlock(ADir);
    if ADir.ArgCount > 0 then
      Fail(ADir, Format(emProxyCfgArgs, ['server']));
    vBlock := THttpServerBlock.Create;
    vHttpBlocks.Add(vBlock);
    vBlock.Where := ADir;
    vHasListen := False;
    for vK := 0 to ADir.Children.Count - 1 do
    begin
      vChild := TRALProxyDirective(ADir.Children[vK]);
      if vChild.Name.Text = 'listen' then
      begin
        if vHasListen then
          Fail(vChild, Format(emProxyCfgArgs, ['listen']));
        ReadListen(vChild, vPort, vBind, vSSL);
        vBlock.Port := vPort;
        vBlock.Bind := vBind;
        vBlock.SSL := vSSL;
        vHasListen := True;
      end
      else if vChild.Name.Text = 'server_name' then
      begin
        NeedArgs(vChild, 1, -1);
        for vM := 0 to vChild.ArgCount - 1 do
          if (vChild.Arg(vM) <> '_') and (vChild.Arg(vM) <> '') then
            vBlock.Names.Add(LowerCase(vChild.Arg(vM)));
      end
      else if vChild.Name.Text = 'ssl_certificate' then
      begin
        NeedArgs(vChild, 1, 1);
        vBlock.CertFile := vChild.Arg(0);
      end
      else if vChild.Name.Text = 'ssl_certificate_key' then
      begin
        NeedArgs(vChild, 1, 1);
        vBlock.KeyFile := vChild.Arg(0);
      end
      else if vChild.Name.Text = 'ssl_protocols' then
      begin
        NeedArgs(vChild, 1, -1);
        vBlock.MinVersion := mtv13;
        for vM := 0 to vChild.ArgCount - 1 do
          if vChild.Arg(vM) = 'TLSv1.2' then
            vBlock.MinVersion := mtv12
          else if vChild.Arg(vM) <> 'TLSv1.3' then
            Fail(TRALProxyToken(vChild.Args[vM]), Format(emProxyCfgValue, ['ssl_protocols', vChild.Arg(vM)]));
      end
      else if (vChild.Name.Text = 'keepalive_timeout') or (vChild.Name.Text = 'client_header_timeout') or
              (vChild.Name.Text = 'proxy_read_timeout') or (vChild.Name.Text = 'proxy_buffer_size') or
              (vChild.Name.Text = 'large_client_header_buffers') then
      begin
        // the same as in http, for this server's port
        NeedArgs(vChild, 1, 2);
        AHttpTimeouts.Values[vChild.Name.Text + '@' + IntToStr(vHttpBlocks.Count - 1)] :=
          vChild.Arg(vChild.ArgCount - 1);
      end
      else if vChild.Name.Text = 'location' then
      begin
        NeedBlock(vChild);
        if vChild.ArgCount = 2 then
        begin
          if vChild.Arg(0) <> '=' then
            Fail(vChild, Format(emProxyCfgUnsupported, ['location', 'location /prefix/ or location = /path']));
          vPrefix := vChild.Arg(1);
        end
        else if vChild.ArgCount = 1 then
          vPrefix := vChild.Arg(0)
        else
          Fail(vChild, Format(emProxyCfgArgs, ['location']));
        if (vPrefix = '') or (vPrefix[1] <> '/') then
          Fail(vChild, Format(emProxyCfgValue, ['location', vPrefix]));
        vLocation := nil;
        for vM := 0 to vChild.Children.Count - 1 do
        begin
          vLoc := TRALProxyDirective(vChild.Children[vM]);
          if vLoc.Name.Text = 'proxy_pass' then
          begin
            vLocation := TLocation.Create;
            vBlock.Locations.Add(vLocation);
            vLocation.Prefix := vPrefix;
            vLocation.Upstream := ProxyTarget(vLoc, False, vLocation.Strip);
          end
          else if vLoc.Name.Text = 'stub_status' then
          begin
            NeedArgs(vLoc, 0, 0);
            vBlock.StatsPath := vPrefix;
          end
          else if vLoc.Name.Text = 'proxy_set_header' then
          begin
            NeedArgs(vLoc, 2, 2);
            if (LowerCase(vLoc.Arg(0)) = 'host') and (vLoc.Arg(1) = '$host') then
              vBlock.PreserveHost := True
            else
              Fail(vLoc, Format(emProxyCfgUnsupported, ['proxy_set_header', 'proxy_set_header Host $host']));
          end
          else if vLoc.Name.Text = 'proxy_http_version' then
            NeedArgs(vLoc, 1, 1) // always 1.1 to the servers
          else
            Unknown(vLoc, 'location');
        end;
        if (vLocation = nil) and (vBlock.StatsPath <> vPrefix) then
          Fail(vChild, Format(emProxyCfgArgs, ['location']));
      end
      else
        Unknown(vChild, 'server');
    end;
    if not vHasListen then
      Fail(ADir, emProxyCfgNoListen);
  end;

  procedure ApplyTimeout(ABal: TRALBalancer; ADir: TRALProxyDirective; const AName, AValue: string);
  var
    vSize: Int64RAL;
  begin
    if AName = 'keepalive_timeout' then
      ABal.IdleTimeout := ToTime(ADir, AValue)
    else if AName = 'client_header_timeout' then
      ABal.HeaderTimeout := ToTime(ADir, AValue)
    else if AName = 'proxy_read_timeout' then
      ABal.ServerTimeout := ToTime(ADir, AValue)
    else if AName = 'proxy_timeout' then
      ABal.TunnelTimeout := ToTime(ADir, AValue)
    else if AName = 'proxy_buffer_size' then
    begin
      vSize := RALProxyParseSize(AValue);
      if (vSize < 1024) or (vSize > 16 * 1024 * 1024) then
        Fail(ADir, Format(emProxyCfgValue, [AName, AValue]));
      ABal.BufferSize := IntegerRAL(vSize);
    end
    else if AName = 'large_client_header_buffers' then
    begin
      vSize := RALProxyParseSize(AValue);
      if (vSize < 1024) or (vSize > 1024 * 1024) then
        Fail(ADir, Format(emProxyCfgValue, [AName, AValue]));
      ABal.MaxHeaderSize := IntegerRAL(vSize);
    end;
  end;

  procedure ReadHttp(ADir: TRALProxyDirective);
  var
    vK, vM, vN: IntegerRAL;
    vChild: TRALProxyDirective;
    vBlock: THttpServerBlock;
    vBal: TRALBalancer;
    vLocation: TLocation;
    vRoute: TRALBalancerRoute;
    vTimeouts, vServerTimeouts: TStringList;
    vName, vValue: string;
    vHasDefault: boolean;
    vFirstOfPort: THttpServerBlock;
    vConnectTimeout: IntegerRAL;
    vL: IntegerRAL;
  begin
    NeedBlock(ADir);
    vTimeouts := TStringList.Create;
    vServerTimeouts := TStringList.Create;
    vConnectTimeout := -1;
    try
      // upstreams first: a server may name one declared below it
      for vK := 0 to ADir.Children.Count - 1 do
      begin
        vChild := TRALProxyDirective(ADir.Children[vK]);
        if vChild.Name.Text = 'upstream' then
          ReadUpstream(vChild, False);
      end;
      for vK := 0 to ADir.Children.Count - 1 do
      begin
        vChild := TRALProxyDirective(ADir.Children[vK]);
        if vChild.Name.Text = 'upstream' then
          Continue
        else if vChild.Name.Text = 'server' then
          ReadHttpServer(vChild, vServerTimeouts)
        else if vChild.Name.Text = 'access_log' then
        begin
          NeedArgs(vChild, 1, 2);
          if vChild.Arg(0) = 'off' then
            FAccessLog := ''
          else
            FAccessLog := vChild.Arg(0);
          if vChild.ArgCount = 2 then
          begin
            if (vChild.Arg(1) <> 'combined') and (vChild.Arg(1) <> 'json') then
              Fail(vChild, Format(emProxyCfgValue, ['access_log', vChild.Arg(1)]));
            FAccessLogFormat := vChild.Arg(1);
          end;
        end
        else if (vChild.Name.Text = 'keepalive_timeout') or (vChild.Name.Text = 'client_header_timeout') or
                (vChild.Name.Text = 'proxy_read_timeout') or (vChild.Name.Text = 'proxy_buffer_size') or
                (vChild.Name.Text = 'large_client_header_buffers') then
        begin
          NeedArgs(vChild, 1, 2);
          vTimeouts.Values[vChild.Name.Text] := vChild.Arg(vChild.ArgCount - 1);
          // checked now, where the error points at the directive
          if (vChild.Name.Text <> 'proxy_buffer_size') and (vChild.Name.Text <> 'large_client_header_buffers') then
            ToTime(vChild, vChild.Arg(vChild.ArgCount - 1))
          else if RALProxyParseSize(vChild.Arg(vChild.ArgCount - 1)) < 0 then
            Fail(vChild, Format(emProxyCfgValue, [vChild.Name.Text, vChild.Arg(vChild.ArgCount - 1)]));
        end
        else if vChild.Name.Text = 'proxy_connect_timeout' then
        begin
          NeedArgs(vChild, 1, 1);
          vConnectTimeout := ToTime(vChild, vChild.Arg(0));
        end
        else if (vChild.Name.Text = 'sendfile') or (vChild.Name.Text = 'tcp_nodelay') then
          NeedArgs(vChild, 1, 1) // NGINX switches that RAL always has on
        else
          Unknown(vChild, 'http');
      end;

      // one balancer per port; its routes are the server blocks on it
      for vK := 0 to vHttpBlocks.Count - 1 do
      begin
        vBlock := THttpServerBlock(vHttpBlocks[vK]);
        vBal := BalancerFor(vBlock.Where, vBlock.Port, vBlock.Bind, bmHTTP);
        for vM := 0 to vTimeouts.Count - 1 do
          ApplyTimeout(vBal, vBlock.Where, vTimeouts.Names[vM], vTimeouts.ValueFromIndex[vM]);
        for vM := 0 to vServerTimeouts.Count - 1 do
        begin
          vName := vServerTimeouts.Names[vM];
          vValue := vServerTimeouts.ValueFromIndex[vM];
          if Copy(vName, Pos('@', vName) + 1, MaxInt) = IntToStr(vK) then
            ApplyTimeout(vBal, vBlock.Where, Copy(vName, 1, Pos('@', vName) - 1), vValue);
        end;
        if vBlock.StatsPath <> '' then
          vBal.StatsPath := StringRAL(vBlock.StatsPath);
        if vBlock.PreserveHost then
          vBal.PreserveHost := True;
        if vBlock.SSL then
        begin
          vBal.SSL.Enabled := True;
          vBal.SSL.MinVersion := vBlock.MinVersion;
          if vBlock.CertFile = '' then
            Fail(vBlock.Where, Format(emProxyCfgNoCertificate, [vBlock.Port]));
          if vBal.SSL.CertFile = '' then
          begin
            vBal.SSL.CertFile := StringRAL(vBlock.CertFile);
            vBal.SSL.KeyFile := StringRAL(vBlock.KeyFile);
          end;
          for vM := 0 to vBlock.Names.Count - 1 do
            vBal.SSL.Certificates.AddCertificate(StringRAL(vBlock.Names[vM]),
              StringRAL(vBlock.CertFile), StringRAL(vBlock.KeyFile));
        end;
      end;
      { the routes: named hosts first, the longest prefix first within a host
        (NGINX picks the longest prefix); then the server without a name, or
        the first one of the port (NGINX's default server), for any host }
      for vN := 0 to FBalancers.Count - 1 do
      begin
        vBal := TRALBalancer(FBalancers[vN]);
        if vBal.Mode <> bmHTTP then
          Continue;
        vHasDefault := False;
        vFirstOfPort := nil;
        for vK := 0 to vHttpBlocks.Count - 1 do
        begin
          vBlock := THttpServerBlock(vHttpBlocks[vK]);
          if vBlock.Port <> vBal.Port then
            Continue;
          if vFirstOfPort = nil then
            vFirstOfPort := vBlock;
          if vBlock.Names.Count = 0 then
            vHasDefault := True;
        end;
        for vK := 0 to vHttpBlocks.Count - 1 do
        begin
          vBlock := THttpServerBlock(vHttpBlocks[vK]);
          if (vBlock.Port <> vBal.Port) or (vBlock.Names.Count = 0) then
            Continue;
          for vM := 0 to vBlock.Names.Count - 1 do
            for vL := 0 to vBlock.Locations.Count - 1 do
            begin
              vLocation := TLocation(vBlock.Locations[vL]);
              vRoute := vBal.Routes.AddRoute(StringRAL(vBlock.Names[vM]),
                StringRAL(vLocation.Prefix), vLocation.Upstream);
              vRoute.StripPrefix := vLocation.Strip;
            end;
        end;
        for vK := 0 to vHttpBlocks.Count - 1 do
        begin
          vBlock := THttpServerBlock(vHttpBlocks[vK]);
          if vBlock.Port <> vBal.Port then
            Continue;
          if (vHasDefault and (vBlock.Names.Count > 0)) or
             ((not vHasDefault) and (vBlock <> vFirstOfPort)) then
            Continue;
          for vL := 0 to vBlock.Locations.Count - 1 do
          begin
            vLocation := TLocation(vBlock.Locations[vL]);
            vRoute := vBal.Routes.AddRoute('', StringRAL(vLocation.Prefix), vLocation.Upstream);
            vRoute.StripPrefix := vLocation.Strip;
          end;
        end;
        SortRoutes(vBal);
      end;
      if vConnectTimeout >= 0 then
        for vK := 0 to FUpstreams.Count - 1 do
          TRALUpstream(FUpstreams.Objects[vK]).ConnectTimeout := vConnectTimeout;
    finally
      vTimeouts.Free;
      vServerTimeouts.Free;
    end;
  end;

  procedure ReadStream(ADir: TRALProxyDirective);
  var
    vK, vM, vPort: IntegerRAL;
    vChild, vSub: TRALProxyDirective;
    vBal: TRALBalancer;
    vBind: string;
    vSSL, vStrip: boolean;
    vNames: TStringList;
    vUp: TRALUpstream;
    vTimeouts: TStringList;
  begin
    NeedBlock(ADir);
    for vK := 0 to ADir.Children.Count - 1 do
    begin
      vChild := TRALProxyDirective(ADir.Children[vK]);
      if vChild.Name.Text = 'upstream' then
        ReadUpstream(vChild, True);
    end;
    for vK := 0 to ADir.Children.Count - 1 do
    begin
      vChild := TRALProxyDirective(ADir.Children[vK]);
      if vChild.Name.Text = 'upstream' then
        Continue;
      if vChild.Name.Text <> 'server' then
        Unknown(vChild, 'stream');
      NeedBlock(vChild);
      vBal := nil;
      vUp := nil;
      vNames := TStringList.Create;
      vTimeouts := TStringList.Create;
      try
        for vM := 0 to vChild.Children.Count - 1 do
        begin
          vSub := TRALProxyDirective(vChild.Children[vM]);
          if vSub.Name.Text = 'listen' then
          begin
            ReadListen(vSub, vPort, vBind, vSSL);
            if vSSL then
              Fail(vSub, Format(emProxyCfgUnsupported, ['listen', 'listen <port> (TLS passes through)']));
            vBal := BalancerFor(vSub, vPort, vBind, bmTCP);
          end
          else if vSub.Name.Text = 'server_name' then
          begin
            NeedArgs(vSub, 1, -1);
            for vPort := 0 to vSub.ArgCount - 1 do
              if vSub.Arg(vPort) <> '_' then
                vNames.Add(LowerCase(vSub.Arg(vPort)));
          end
          else if vSub.Name.Text = 'proxy_pass' then
            vUp := ProxyTarget(vSub, True, vStrip)
          else if (vSub.Name.Text = 'proxy_timeout') or (vSub.Name.Text = 'proxy_buffer_size') then
          begin
            NeedArgs(vSub, 1, 1);
            vTimeouts.Values[vSub.Name.Text] := vSub.Arg(0);
          end
          else if vSub.Name.Text = 'proxy_connect_timeout' then
          begin
            NeedArgs(vSub, 1, 1);
            vTimeouts.Values['#connect'] := vSub.Arg(0);
          end
          else if (vSub.Name.Text = 'ssl_preread') then
            NeedArgs(vSub, 1, 1) // RAL reads the ClientHello whenever a route has a name
          else
            Unknown(vSub, 'server');
        end;
        if vBal = nil then
          Fail(vChild, emProxyCfgNoListen);
        if vUp = nil then
          Fail(vChild, Format(emProxyCfgArgs, ['proxy_pass']));
        for vM := 0 to vTimeouts.Count - 1 do
          if vTimeouts.Names[vM] = '#connect' then
            vUp.ConnectTimeout := ToTime(vChild, vTimeouts.ValueFromIndex[vM])
          else
            ApplyTimeout(vBal, vChild, vTimeouts.Names[vM], vTimeouts.ValueFromIndex[vM]);
        if vNames.Count = 0 then
        begin
          if vBal.Upstream <> nil then
            Fail(vChild, Format(emProxyCfgPortConflict, [vBal.Port]));
          vBal.Upstream := vUp;
        end
        else
          for vM := 0 to vNames.Count - 1 do
            vBal.Routes.AddRoute(StringRAL(vNames[vM]), '', vUp);
      finally
        vNames.Free;
        vTimeouts.Free;
      end;
    end;
  end;

begin
  FFileName := AFileName;
  vImplicit := 0;
  vParser := TRALProxyParser.Create;
  vHttpBlocks := TList.Create;
  vRoot := nil;
  try
    vRoot := vParser.Parse(AText, AFileName);
    for vInt := 0 to vRoot.Count - 1 do
    begin
      vDir := TRALProxyDirective(vRoot[vInt]);
      if (vDir.Name.Text = 'worker_threads') or (vDir.Name.Text = 'worker_processes') then
      begin
        NeedArgs(vDir, 1, 1);
        if vDir.Arg(0) = 'auto' then
          FThreadCount := 0
        else
          FThreadCount := ToInt(vDir, vDir.Arg(0));
      end
      else if vDir.Name.Text = 'pid' then
      begin
        NeedArgs(vDir, 1, 1);
        FPidFile := vDir.Arg(0);
      end
      else if vDir.Name.Text = 'error_log' then
      begin
        NeedArgs(vDir, 1, 2);
        FErrorLog := vDir.Arg(0);
      end
      else if vDir.Name.Text = 'events' then
      begin
        // NGINX's events block: worker_connections is the only word known
        NeedBlock(vDir);
        for vJ := 0 to vDir.Children.Count - 1 do
          if TRALProxyDirective(vDir.Children[vJ]).Name.Text <> 'worker_connections' then
            Unknown(TRALProxyDirective(vDir.Children[vJ]), 'events');
      end
      else if (vDir.Name.Text <> 'http') and (vDir.Name.Text <> 'stream') then
        Unknown(vDir, 'main');
    end;
    for vInt := 0 to vRoot.Count - 1 do
    begin
      vDir := TRALProxyDirective(vRoot[vInt]);
      if vDir.Name.Text = 'http' then
        ReadHttp(vDir)
      else if vDir.Name.Text = 'stream' then
        ReadStream(vDir);
    end;
    for vInt := 0 to FBalancers.Count - 1 do
      TRALBalancer(FBalancers[vInt]).ThreadCount := FThreadCount;
  finally
    if vRoot <> nil then
    begin
      for vInt := 0 to vRoot.Count - 1 do
        TObject(vRoot[vInt]).Free;
      vRoot.Free;
    end;
    for vInt := 0 to vHttpBlocks.Count - 1 do
      TObject(vHttpBlocks[vInt]).Free;
    vHttpBlocks.Free;
    vParser.Free;
  end;
end;

{ TRALProxyHost }

constructor TRALProxyHost.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FRetired := TList.Create;
  FRunning := TList.Create;
end;

destructor TRALProxyHost.Destroy;
begin
  Stop;
  FreeAndNil(FRunning);
  FreeAndNil(FRetired);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TRALProxyHost.Key(ABalancer: TRALBalancer): string;
begin
  // a port that changes mode or turns TLS on or off opens again
  Result := IntToStr(Ord(ABalancer.Mode)) + ':' + BoolToStr(ABalancer.SSL.Enabled, True) + ':' +
    string(ABalancer.IPConfig.IPv4Bind) + ':' + IntToStr(ABalancer.Port);
end;

function TRALProxyHost.Running(const AKey: string): TRALBalancer;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FRunning.Count - 1 do
  begin
    Result := TRALBalancer(FRunning[vInt]);
    if Key(Result) = AKey then
      Exit;
  end;
  Result := nil;
end;

procedure TRALProxyHost.SetOnAccess(const AValue: TRALBalancerAccessEvent);
var
  vInt: IntegerRAL;
begin
  FOnAccess := AValue;
  for vInt := 0 to FRunning.Count - 1 do
    TRALBalancer(FRunning[vInt]).OnAccess := AValue;
end;

procedure TRALProxyHost.Start(AConfig: TRALProxyConfig);
var
  vInt: IntegerRAL;
  vBal: TRALBalancer;
begin
  Stop;
  FConfig := AConfig;
  try
    for vInt := 0 to AConfig.Balancers.Count - 1 do
    begin
      vBal := TRALBalancer(AConfig.Balancers[vInt]);
      vBal.OnAccess := FOnAccess;
      vBal.Active := True;
      FRunning.Add(vBal);
    end;
    // the balancers are the host's now; the configuration keeps the upstreams
    AConfig.Balancers.Clear;
  except
    for vInt := 0 to AConfig.Balancers.Count - 1 do
      if FRunning.IndexOf(AConfig.Balancers[vInt]) < 0 then
        TObject(AConfig.Balancers[vInt]).Free;
    AConfig.Balancers.Clear;
    Stop;
    raise;
  end;
end;

procedure TRALProxyHost.Reload(ANew: TRALProxyConfig);
var
  vInt, vJ: IntegerRAL;
  vNew, vOld: TRALBalancer;
  vKeys: TStringList;
  vStarted, vLater: TList;
  vBusy: boolean;
  vLaterError: string;

  procedure Open(ABalancer: TRALBalancer);
  begin
    ABalancer.OnAccess := FOnAccess;
    ABalancer.Active := True;
    vStarted.Add(ABalancer);
  end;

begin
  if FConfig = nil then
  begin
    Start(ANew);
    Exit;
  end;
  vKeys := TStringList.Create;
  vStarted := TList.Create;
  vLater := TList.Create;
  try
    for vInt := 0 to ANew.Balancers.Count - 1 do
      vKeys.Add(Key(TRALBalancer(ANew.Balancers[vInt])));
    { the new ports open first, so a port that does not open leaves what runs
      as it was; a port that changes mode or TLS must close first, and opens
      after the ports that go away }
    try
      for vInt := 0 to ANew.Balancers.Count - 1 do
      begin
        vNew := TRALBalancer(ANew.Balancers[vInt]);
        if Running(Key(vNew)) <> nil then
          Continue;
        vBusy := False;
        for vJ := 0 to FRunning.Count - 1 do
          if TRALBalancer(FRunning[vJ]).Port = vNew.Port then
            vBusy := True;
        if vBusy then
          vLater.Add(vNew)
        else
          Open(vNew);
      end;
    except
      for vInt := 0 to vStarted.Count - 1 do
        TRALBalancer(vStarted[vInt]).Active := False;
      raise;
    end;
    for vInt := FRunning.Count - 1 downto 0 do
    begin
      vOld := TRALBalancer(FRunning[vInt]);
      if vKeys.IndexOf(Key(vOld)) < 0 then
      begin
        vOld.Active := False;
        vOld.Free;
        FRunning.Delete(vInt);
      end;
    end;
    // what changed mode opens now; if it fails, the rest goes in all the same
    vLaterError := '';
    for vInt := 0 to vLater.Count - 1 do
      try
        Open(TRALBalancer(vLater[vInt]));
      except
        on E: Exception do
          if vLaterError = '' then
            vLaterError := E.Message;
      end;
    // the ports that stay take the new routes, upstreams and certificates
    for vInt := 0 to ANew.Balancers.Count - 1 do
    begin
      vNew := TRALBalancer(ANew.Balancers[vInt]);
      if (vStarted.IndexOf(vNew) >= 0) or (vLater.IndexOf(vNew) >= 0) then
        Continue;
      vOld := Running(Key(vNew));
      if vOld = nil then
        Continue;
      vOld.Upstream := vNew.Upstream;
      vOld.Routes := vNew.Routes;
      vOld.SSL := vNew.SSL;
      vOld.StatsPath := vNew.StatsPath;
      vOld.PreserveHost := vNew.PreserveHost;
      vOld.BufferSize := vNew.BufferSize;
      vOld.MaxHeaderSize := vNew.MaxHeaderSize;
      vOld.IdleTimeout := vNew.IdleTimeout;
      vOld.HeaderTimeout := vNew.HeaderTimeout;
      vOld.ServerTimeout := vNew.ServerTimeout;
      vOld.TunnelTimeout := vNew.TunnelTimeout;
      vOld.Reload;
    end;
    for vInt := 0 to vStarted.Count - 1 do
      FRunning.Add(vStarted[vInt]);
    // the balancers the host keeps leave the new configuration; the rest are freed with it
    for vInt := ANew.Balancers.Count - 1 downto 0 do
      if vStarted.IndexOf(ANew.Balancers[vInt]) >= 0 then
        ANew.Balancers.Delete(vInt);
    FLock.Acquire;
    try
      FRetired.Add(FConfig);
      FConfig := ANew;
    finally
      FLock.Release;
    end;
    // the configuration is in; a port that changed mode and did not open is told now
    if vLaterError <> '' then
      raise ERALException.Create(vLaterError);
  finally
    vLater.Free;
    vStarted.Free;
    vKeys.Free;
  end;
end;

procedure TRALProxyHost.Stop;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FRunning.Count - 1 do
    TRALBalancer(FRunning[vInt]).Active := False;
  for vInt := 0 to FRunning.Count - 1 do
    TRALBalancer(FRunning[vInt]).Free;
  FRunning.Clear;
  for vInt := 0 to FRetired.Count - 1 do
    TObject(FRetired[vInt]).Free;
  FRetired.Clear;
  FreeAndNil(FConfig);
end;

function TRALProxyHost.StatsJSON: StringRAL;
var
  vInt: IntegerRAL;
  vBal: TRALBalancer;
begin
  Result := '{';
  for vInt := 0 to FRunning.Count - 1 do
  begin
    vBal := TRALBalancer(FRunning[vInt]);
    if vInt > 0 then
      Result := Result + ',';
    Result := Result + '"' + StringRAL(IntToStr(vBal.Port)) + '":' + vBal.StatsJSON;
  end;
  Result := Result + '}';
end;

end.
