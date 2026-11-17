{ A client of Redis and Valkey on RAL's own sockets: RESP2 and RESP3, a user and
  password (ACL), the database, TLS with the certificate rule of TRALClient, a
  pool of connections any thread may share, pipelines and MULTI/EXEC, and
  helpers for the commands most used. Pub/sub has its own component
  (TRALRedisSubscriber), on a connection of its own. }
unit RALRedisClient;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs, Contnrs,
  RALTypes, RALConsts, RALExceptions, RALCustomObjects, RALClient, RALSocket, RALTLS;

const
  /// The port of Redis when nobody says otherwise.
  RALRedisDefaultPort = 6379;

type
  /// A problem talking to the server: it cannot be reached, it closed, it did not answer.
  ERALRedisError = class(ERALException);

  { The server answered a command with an error (-ERR, -WRONGTYPE, -NOAUTH...):
    the connection is still good }
  ERALRedisReplyError = class(ERALRedisError)
  private
    FServerError: StringRAL;
  public
    /// The error as the server wrote it, its prefix included.
    property ServerError: StringRAL read FServerError;
  end;

  /// The RESP version a connection speaks.
  TRALRedisProtocol = (rp2, rp3);

  { What a reply is: rrkNil (nil, RESP3 null), rrkStatus (+OK), rrkError,
    rrkInteger, rrkString (bulk and verbatim), rrkArray, rrkMap (keys and values
    alternate in Items), rrkSet, rrkDouble, rrkBoolean, rrkBigNumber, rrkPush
    (RESP3, out of band) }
  TRALRedisReplyKind = (rrkNil, rrkStatus, rrkError, rrkInteger, rrkString, rrkArray,
    rrkMap, rrkSet, rrkDouble, rrkBoolean, rrkBigNumber, rrkPush);

  { One reply of the server, with its children for an array, a map, a set or a
    push. The caller frees what a command returns }
  TRALRedisReply = class
  private
    FInteger: Int64RAL;
    /// The children (array, map, set, push); nil when there are none.
    FItems: TObjectList;
    FKind: TRALRedisReplyKind;
    FText: RawByteString;

    function GetCount: IntegerRAL;
    function GetItem(AIndex: IntegerRAL): TRALRedisReply;
  public
    /// A reply of AKind, empty.
    constructor Create(AKind: TRALRedisReplyKind);
    destructor Destroy; override;

    /// Adds AReply as the last child (owned).
    procedure Add(AReply: TRALRedisReply);
    /// The value as text (a number written out; '' for nil).
    function AsString: StringRAL;
    { The value of AKey in a map (or a RESP2 array of name, value pairs); nil
      when missing }
    function Find(const AKey: RawByteString): TRALRedisReply;
    /// The reply is nil (a missing key).
    function IsNil: boolean;

    { For rrkInteger and rrkBoolean (0/1): the number; a double cut to its
      integer part }
    property AsInteger: Int64RAL read FInteger;
    /// Number of children.
    property Count: IntegerRAL read GetCount;
    /// Child AIndex.
    property Items[AIndex: IntegerRAL]: TRALRedisReply read GetItem; default;
    /// What the reply is.
    property Kind: TRALRedisReplyKind read FKind;
    /// The bytes of a string, a status, an error, a double or a big number.
    property Text: RawByteString read FText;
  end;

  /// The TLS settings of a Redis connection.
  TRALRedisSSL = class(TRALSocketSSL);

  { The settings of a connection, copied by every connection made, so the
    component may change while connections from before still run }
  TRALRedisSettings = record
    /// Client name sent with CLIENT SETNAME (HELLO in RESP3).
    ClientName: StringRAL;
    /// Milliseconds a command may wait for its reply.
    CommandTimeout: IntegerRAL;
    /// Milliseconds a connection may take, the TLS handshake included.
    ConnectTimeout: IntegerRAL;
    /// Database selected after connecting.
    Database: IntegerRAL;
    /// Host name or address.
    Host: StringRAL;
    /// Password; empty sends no AUTH.
    Password: StringRAL;
    /// Port.
    Port: IntegerRAL;
    /// RESP version asked.
    Protocol: TRALRedisProtocol;
    /// User name of an ACL user; empty is the default user.
    UserName: StringRAL;
  end;

  { One connection to the server, used by one thread at a time: writes
    commands, reads replies. The pool of TRALRedisClient and the subscriber
    make them }
  TRALRedisConnection = class
  private
    /// Bytes received and not yet read.
    FBuffer: TBytes;
    /// The top of the channel stack (TLS over the socket, or the socket).
    FChannel: TRALChannel;
    /// The command timeout, in milliseconds.
    FCommandTimeout: IntegerRAL;
    /// Deadline of the reply being read (RALSocketNow).
    FDeadline: Int64RAL;
    /// Start of the bytes not yet read in FBuffer.
    FHead: IntegerRAL;
    /// The RESP version agreed.
    FProtocol: TRALRedisProtocol;
    /// The version of the server (HELLO in RESP3, INFO otherwise); may be empty.
    FServerVersion: StringRAL;
    /// End of the bytes received in FBuffer.
    FTail: IntegerRAL;
    /// The TLS context of a TLS connection; nil for plain.
    FTLS: TRALTLSContext;

    /// Reads more bytes into the buffer, waiting until the deadline.
    procedure Fill;
    /// Reads ACount bytes and the CRLF after them.
    function ReadBytes(ACount: IntegerRAL): RawByteString;
    /// Reads a line, without its CRLF.
    function ReadLine: RawByteString;
    /// Reads one reply (its children too), waiting until the deadline.
    function ReadValue: TRALRedisReply;
  public
    { Connects to ASettings.Host:Port (with TLS when ASSL.Enabled, the
      certificate judged by the rule of TRALClient through ASender and
      AOnCert), then says hello: HELLO 3 (falling back to RESP2 on a server that
      does not know it), AUTH, SELECT and CLIENT SETNAME as the settings ask }
    constructor Create(const ASettings: TRALRedisSettings; ASSL: TRALRedisSSL;
      ASender: TObject; AOnCert: TRALOnValidateCert);
    destructor Destroy; override;

    { Whether the connection may be used again: nothing arrived on it while it
      was idle (a close or an error would) }
    function Alive: boolean;
    /// Sends AArgs as one command and reads its reply (which may be an error reply).
    function Execute(const AArgs: array of RawByteString): TRALRedisReply;
    /// Reads the next reply, waiting up to ATimeout milliseconds; nil on timeout.
    function ReadReply(ATimeout: IntegerRAL): TRALRedisReply;
    /// Sends AArgs as one command without reading its reply.
    procedure Send(const AArgs: array of RawByteString);
    /// Sends AData as it is (commands already encoded).
    procedure SendRaw(const AData: RawByteString);

    /// The RESP version agreed with the server.
    property Protocol: TRALRedisProtocol read FProtocol;
    /// The version of the server, when it said.
    property ServerVersion: StringRAL read FServerVersion;
  end;

  TRALRedisClient = class;

  { Commands sent together and their replies read together (one round trip):
    Add the commands, then Execute. With Transaction they run inside MULTI and
    EXEC, all or none }
  TRALRedisPipeline = class
  private
    /// The client whose connection runs the commands.
    FClient: TRALRedisClient;
    FCount: IntegerRAL;
    /// The commands encoded, in order.
    FData: RawByteString;
    FTransaction: boolean;
  public
    /// A pipeline of AClient; ATransaction wraps it in MULTI/EXEC.
    constructor Create(AClient: TRALRedisClient; ATransaction: boolean = False);

    /// Adds a command.
    procedure Add(const AArgs: array of RawByteString);
    { Sends every command and returns their replies in order (owned by the
      list the caller frees); a command the server refused has an rrkError
      reply, it does not raise. In a transaction, the replies of EXEC (nil
      when a WATCH aborted it) }
    function Execute: TObjectList;

    /// Commands added.
    property Count: IntegerRAL read FCount;
    /// The commands run as one transaction (MULTI/EXEC).
    property Transaction: boolean read FTransaction;
  end;

  { A Redis or Valkey server (Host, Port) for any thread: each command takes a
    connection from the pool (PoolSize idle ones are kept) and gives it back.
    Command runs any command; the helpers cover strings, keys and expiry,
    counters, hashes, lists, sets, Lua scripts and publishing. A reply that is
    an error raises ERALRedisReplyError }
  TRALRedisClient = class(TRALComponent)
  private
    FClientName: StringRAL;
    FCommandTimeout: IntegerRAL;
    FConnectTimeout: IntegerRAL;
    FDatabase: IntegerRAL;
    FHost: StringRAL;
    /// Idle connections, the last given back first.
    FIdle: TList;
    /// Guards FIdle.
    FLock: TCriticalSection;
    FOnValidateServerCert: TRALOnValidateCert;
    FPassword: StringRAL;
    FPoolSize: IntegerRAL;
    FPort: IntegerRAL;
    FProtocol: TRALRedisProtocol;
    FSSL: TRALRedisSSL;
    FUserName: StringRAL;

    procedure SetSSL(AValue: TRALRedisSSL);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { A connection from the pool, or a new one; give it back with Release
      (or free it when it broke) }
    function Acquire: TRALRedisConnection;
    /// Closes the idle connections (the busy ones close when given back).
    procedure ClearPool;
    { Runs AArgs and returns the reply, which the caller frees; an error reply
      raises ERALRedisReplyError }
    function Command(const AArgs: array of RawByteString): TRALRedisReply;
    /// Runs AArgs and returns the integer reply.
    function CommandInt(const AArgs: array of RawByteString): Int64RAL;
    /// Runs AArgs and returns the reply as text ('' for nil).
    function CommandText(const AArgs: array of RawByteString): RawByteString;
    /// DEL: removes AKeys; how many existed.
    function Delete(const AKeys: array of RawByteString): Int64RAL;
    { EVAL: runs the Lua AScript with AKeys and AArgs; the reply, which the
      caller frees }
    function Eval(const AScript: RawByteString;
      const AKeys, AArgs: array of RawByteString): TRALRedisReply;
    /// EXISTS: whether AKey is there.
    function Exists(const AKey: RawByteString): boolean;
    /// PEXPIRE: AKey lives AMilliseconds more; False when it is missing.
    function Expire(const AKey: RawByteString; AMilliseconds: Int64RAL): boolean;
    /// GET: the value of AKey; False when it is missing.
    function GetValue(const AKey: RawByteString; out AValue: RawByteString): boolean;
    /// HGET: the field AField of the hash AKey; False when missing.
    function HashGet(const AKey, AField: RawByteString; out AValue: RawByteString): boolean;
    /// HGETALL: the fields of the hash AKey into AList as name=value lines.
    procedure HashGetAll(const AKey: RawByteString; AList: TStrings);
    /// HSET: sets the field AField of the hash AKey; True when the field is new.
    function HashSet(const AKey, AField, AValue: RawByteString): boolean;
    /// INCRBY: adds ADelta to the integer in AKey (0 when missing); the new value.
    function Increment(const AKey: RawByteString; ADelta: Int64RAL = 1): Int64RAL;
    /// LPOP (AFromLeft) or RPOP: takes one item of the list AKey; False when empty.
    function ListPop(const AKey: RawByteString; out AValue: RawByteString;
      AFromLeft: boolean = True): boolean;
    /// LPUSH (AToLeft) or RPUSH: adds AValue to the list AKey; its new length.
    function ListPush(const AKey, AValue: RawByteString; AToLeft: boolean = False): Int64RAL;
    { LRANGE: the items of the list AKey from AStart to AStop (-1 is the last)
      into AList }
    procedure ListRange(const AKey: RawByteString; AStart, AStop: Int64RAL; AList: TStrings);
    /// A connection that is not in the pool (the subscriber's); the caller frees it.
    function NewConnection: TRALRedisConnection;
    /// Starts a pipeline (ATransaction: MULTI/EXEC) the caller frees.
    function NewPipeline(ATransaction: boolean = False): TRALRedisPipeline;
    /// PING: True when the server answers.
    function Ping: boolean;
    /// PUBLISH: sends AMessage to AChannel; how many subscribers got it.
    function Publish(const AChannel, AMessage: RawByteString): Int64RAL;
    /// Gives back a connection taken with Acquire.
    procedure Release(AConnection: TRALRedisConnection);
    { SCAN: every key that matches the glob APattern into AList (several
      round trips on a big database; keys added meanwhile may be missed) }
    procedure Scan(const APattern: RawByteString; AList: TStrings);
    /// SADD: adds AMember to the set AKey; True when it is new.
    function SetAdd(const AKey, AMember: RawByteString): boolean;
    /// SMEMBERS: the members of the set AKey into AList.
    procedure SetMembers(const AKey: RawByteString; AList: TStrings);
    /// SREM: removes AMember from the set AKey; True when it was there.
    function SetRemove(const AKey, AMember: RawByteString): boolean;
    /// The settings a new connection copies.
    function Settings: TRALRedisSettings;
    /// SET: stores AValue in AKey, for AMilliseconds when above 0.
    procedure SetValue(const AKey, AValue: RawByteString; AMilliseconds: Int64RAL = 0);
    /// PTTL: milliseconds AKey still lives; -1 without expiry, -2 when missing.
    function TimeToLive(const AKey: RawByteString): Int64RAL;
  published
    /// Name the connections give themselves (CLIENT LIST shows it).
    property ClientName: StringRAL read FClientName write FClientName;
    /// Milliseconds a command may wait for its reply.
    property CommandTimeout: IntegerRAL read FCommandTimeout write FCommandTimeout default 10000;
    /// Milliseconds a connection may take, the TLS handshake included.
    property ConnectTimeout: IntegerRAL read FConnectTimeout write FConnectTimeout default 5000;
    /// Database number selected on every connection.
    property Database: IntegerRAL read FDatabase write FDatabase default 0;
    /// Host name or address of the server.
    property Host: StringRAL read FHost write FHost;
    /// Decides whether the server certificate is accepted (TLS); it overrides pins.
    property OnValidateServerCert: TRALOnValidateCert read FOnValidateServerCert
      write FOnValidateServerCert;
    /// Password (requirepass, or of UserName); empty sends no AUTH.
    property Password: StringRAL read FPassword write FPassword;
    /// Idle connections kept for the next commands.
    property PoolSize: IntegerRAL read FPoolSize write FPoolSize default 8;
    /// Port of the server.
    property Port: IntegerRAL read FPort write FPort default RALRedisDefaultPort;
    { RESP version: rp3 (the default) falls back to rp2 on a server older than
      Redis 6 }
    property Protocol: TRALRedisProtocol read FProtocol write FProtocol default rp3;
    /// TLS (rediss); off by default.
    property SSL: TRALRedisSSL read FSSL write SetSSL;
    /// ACL user; empty is the default user.
    property UserName: StringRAL read FUserName write FUserName;
  end;

/// The command AArgs in RESP (an array of bulk strings).
function RALRedisEncode(const AArgs: array of RawByteString): RawByteString;
/// Escapes the glob characters of AText (* ? [ ] \) so it matches only itself.
function RALRedisEscapeGlob(const AText: RawByteString): RawByteString;

implementation

const
  /// The end of a RESP line.
  cCRLF: RawByteString = #13#10;

function RALRedisEncode(const AArgs: array of RawByteString): RawByteString;
var
  vInt: IntegerRAL;
begin
  Result := '*' + RawByteString(IntToStr(Length(AArgs))) + cCRLF;
  for vInt := 0 to High(AArgs) do
    Result := Result + '$' + RawByteString(IntToStr(Length(AArgs[vInt]))) + cCRLF +
      AArgs[vInt] + cCRLF;
end;

function RALRedisEscapeGlob(const AText: RawByteString): RawByteString;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 1 to Length(AText) do
  begin
    if AText[vInt] in ['*', '?', '[', ']', '\'] then
      Result := Result + '\';
    Result := Result + AText[vInt];
  end;
end;

{ TRALRedisReply }

constructor TRALRedisReply.Create(AKind: TRALRedisReplyKind);
begin
  inherited Create;
  FKind := AKind;
end;

destructor TRALRedisReply.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TRALRedisReply.Add(AReply: TRALRedisReply);
begin
  if FItems = nil then
    FItems := TObjectList.Create(True);
  FItems.Add(AReply);
end;

function TRALRedisReply.AsString: StringRAL;
begin
  case FKind of
    rrkNil:
      Result := '';
    rrkInteger, rrkBoolean:
      Result := StringRAL(IntToStr(FInteger));
  else
    Result := StringRAL(FText);
  end;
end;

function TRALRedisReply.Find(const AKey: RawByteString): TRALRedisReply;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  vInt := 0;
  while vInt + 1 < Count do
  begin
    if Items[vInt].FText = AKey then
    begin
      Result := Items[vInt + 1];
      Exit;
    end;
    Inc(vInt, 2);
  end;
end;

function TRALRedisReply.GetCount: IntegerRAL;
begin
  if FItems = nil then
    Result := 0
  else
    Result := FItems.Count;
end;

function TRALRedisReply.GetItem(AIndex: IntegerRAL): TRALRedisReply;
begin
  Result := TRALRedisReply(FItems[AIndex]);
end;

function TRALRedisReply.IsNil: boolean;
begin
  Result := FKind = rrkNil;
end;

{ TRALRedisConnection }

constructor TRALRedisConnection.Create(const ASettings: TRALRedisSettings;
  ASSL: TRALRedisSSL; ASender: TObject; AOnCert: TRALOnValidateCert);
var
  vSocket: TRALSocket;
  vTLS: TRALTLSChannel;
  vCert: TRALCertInfo;
  vName: StringRAL;
  vDeadline: Int64RAL;
  vReply: TRALRedisReply;
  vVersion: TRALRedisReply;
begin
  inherited Create;
  FCommandTimeout := ASettings.CommandTimeout;
  if FCommandTimeout <= 0 then
    FCommandTimeout := 10000;
  SetLength(FBuffer, 16384);
  vDeadline := RALSocketNow + ASettings.ConnectTimeout;
  vSocket := TRALSocket.Create;
  FChannel := TRALSocketChannel.Create(vSocket);
  try
    try
      vSocket.Connect(ASettings.Host, ASettings.Port, ASettings.ConnectTimeout);
    except
      on E: Exception do
        raise ERALRedisError.CreateFmt(emRedisConnect, [string(ASettings.Host),
          ASettings.Port, E.Message]);
    end;
    if (ASSL <> nil) and ASSL.Enabled then
    begin
      FTLS := TRALTLSContext.Create(mtrClient);
      FTLS.SetMinVersion(ASSL.MinVersion);
      FTLS.ALPN := ASSL.ALPN;
      FTLS.LoadSystemCAs;
      FTLS.LoadCAFile(ASSL.CAFile);
      FTLS.UseCertificateFiles(ASSL.CertFile, ASSL.KeyFile, ASSL.KeyPassword);
      vTLS := TRALTLSChannel.Create(FChannel, FTLS);
      FChannel := vTLS;
      vName := ASSL.ServerName;
      if vName = '' then
        vName := ASettings.Host;
      if ASSL.ResumeSession then
        vTLS.SessionKey := RALTLSSessionKey(vName, ASettings.Host, ASettings.Port, ASSL.CertFile);
      vTLS.Handshake(vName, vDeadline - RALSocketNow);
      vCert := vTLS.PeerCertificate;
      if not RALTLSAcceptCertificate(ASender, ASSL, AOnCert, vCert, ASettings.Host,
           ASettings.Port) then
        raise ERALRedisError.CreateFmt(emRedisCertRefused, [string(vCert.Error)]);
    end;

    // HELLO 3 says the version, authenticates and names the connection at once
    FProtocol := rp2;
    if ASettings.Protocol = rp3 then
    begin
      if ASettings.Password <> '' then
      begin
        if ASettings.UserName <> '' then
          vName := ASettings.UserName
        else
          vName := 'default';
        if ASettings.ClientName <> '' then
          vReply := Execute(['HELLO', '3', 'AUTH', RawByteString(vName),
            RawByteString(ASettings.Password), 'SETNAME', RawByteString(ASettings.ClientName)])
        else
          vReply := Execute(['HELLO', '3', 'AUTH', RawByteString(vName),
            RawByteString(ASettings.Password)]);
      end
      else if ASettings.ClientName <> '' then
        vReply := Execute(['HELLO', '3', 'SETNAME', RawByteString(ASettings.ClientName)])
      else
        vReply := Execute(['HELLO', '3']);
      try
        if vReply.Kind <> rrkError then
        begin
          FProtocol := rp3;
          vVersion := vReply.Find('version');
          if vVersion <> nil then
            FServerVersion := vVersion.AsString;
        end
        else if Pos(RawByteString('NOAUTH'), vReply.Text) = 1 then
          raise ERALRedisError.CreateFmt(emRedisAuthFailed, [string(vReply.Text)])
        else if (Pos(RawByteString('WRONGPASS'), vReply.Text) = 1) or
          (Pos(RawByteString('NOPERM'), vReply.Text) = 1) then
          raise ERALRedisError.CreateFmt(emRedisAuthFailed, [string(vReply.Text)]);
        // any other error: a server before Redis 6, which goes on in RESP2
      finally
        vReply.Free;
      end;
    end;
    if FProtocol = rp2 then
    begin
      if ASettings.Password <> '' then
      begin
        if ASettings.UserName <> '' then
          vReply := Execute(['AUTH', RawByteString(ASettings.UserName),
            RawByteString(ASettings.Password)])
        else
          vReply := Execute(['AUTH', RawByteString(ASettings.Password)]);
        try
          if vReply.Kind = rrkError then
            raise ERALRedisError.CreateFmt(emRedisAuthFailed, [string(vReply.Text)]);
        finally
          vReply.Free;
        end;
      end;
      if ASettings.ClientName <> '' then
        Execute(['CLIENT', 'SETNAME', RawByteString(ASettings.ClientName)]).Free;
    end;
    if ASettings.Database <> 0 then
    begin
      vReply := Execute(['SELECT', RawByteString(IntToStr(ASettings.Database))]);
      try
        if vReply.Kind = rrkError then
          raise ERALRedisError.CreateFmt(emRedisSelect, [ASettings.Database, string(vReply.Text)]);
      finally
        vReply.Free;
      end;
    end;
  except
    FreeAndNil(FChannel);
    FreeAndNil(FTLS);
    raise;
  end;
end;

destructor TRALRedisConnection.Destroy;
begin
  FreeAndNil(FChannel); // the stack frees down to the socket
  FreeAndNil(FTLS);
  inherited Destroy;
end;

function TRALRedisConnection.Alive: boolean;
begin
  { an idle connection has nothing to say: any event (bytes, a close, a reset,
    which Windows reports as a hang-up) means it is not good }
  Result := (FChannel <> nil) and (FHead >= FTail) and (not FChannel.HasBuffered) and
    (FChannel.Socket.WaitFor(RALPollIn, 0) = 0);
end;

function TRALRedisConnection.Execute(const AArgs: array of RawByteString): TRALRedisReply;
begin
  Send(AArgs);
  FDeadline := RALSocketNow + FCommandTimeout;
  Result := ReadValue;
  // a RESP3 push (client tracking, a message) is not the reply of the command
  while Result.Kind = rrkPush do
  begin
    Result.Free;
    Result := ReadValue;
  end;
end;

procedure TRALRedisConnection.Fill;
var
  vRead: IntegerRAL;
  vWait: Int64RAL;
begin
  if FHead > 0 then
  begin
    if FTail > FHead then
      Move(FBuffer[FHead], FBuffer[0], FTail - FHead);
    Dec(FTail, FHead);
    FHead := 0;
  end;
  if FTail = Length(FBuffer) then
    SetLength(FBuffer, Length(FBuffer) * 2);
  vWait := FDeadline - RALSocketNow;
  if vWait < 0 then
    vWait := 0;
  try
    vRead := FChannel.ReadWait(FBuffer[FTail], Length(FBuffer) - FTail, vWait);
  except
    on E: Exception do
      raise ERALRedisError.CreateFmt(emRedisRead, [E.Message]);
  end;
  if vRead = 0 then
    raise ERALRedisError.Create(emRedisClosed);
  if vRead < 0 then
    raise ERALRedisError.CreateFmt(emRedisTimeout, [FCommandTimeout]);
  Inc(FTail, vRead);
end;

function TRALRedisConnection.ReadBytes(ACount: IntegerRAL): RawByteString;
begin
  while FTail - FHead < ACount + 2 do
    Fill;
  SetLength(Result, ACount);
  if ACount > 0 then
    Move(FBuffer[FHead], Pointer(Result)^, ACount);
  Inc(FHead, ACount + 2);
end;

function TRALRedisConnection.ReadLine: RawByteString;
var
  vInt: IntegerRAL;
begin
  vInt := FHead;
  repeat
    while vInt + 1 < FTail do
    begin
      if (FBuffer[vInt] = 13) and (FBuffer[vInt + 1] = 10) then
      begin
        SetLength(Result, vInt - FHead);
        if vInt > FHead then
          Move(FBuffer[FHead], Pointer(Result)^, vInt - FHead);
        FHead := vInt + 2;
        Exit;
      end;
      Inc(vInt);
    end;
    vInt := vInt - FHead; // Fill moves the bytes to the start
    Fill;
    vInt := FHead + vInt;
  until False;
end;

function TRALRedisConnection.ReadReply(ATimeout: IntegerRAL): TRALRedisReply;
var
  vRead: IntegerRAL;
begin
  Result := nil;
  if FHead >= FTail then
  begin
    FHead := 0;
    FTail := 0;
    try
      vRead := FChannel.ReadWait(FBuffer[0], Length(FBuffer), ATimeout);
    except
      on E: Exception do
        raise ERALRedisError.CreateFmt(emRedisRead, [E.Message]);
    end;
    if vRead = 0 then
      raise ERALRedisError.Create(emRedisClosed);
    if vRead < 0 then
      Exit;
    FTail := vRead;
  end;
  FDeadline := RALSocketNow + FCommandTimeout;
  Result := ReadValue;
end;

function TRALRedisConnection.ReadValue: TRALRedisReply;
var
  vLine: RawByteString;
  vType: AnsiChar;
  vCount, vInt: Int64RAL;
  vItems: Int64RAL;
  vFormat: TFormatSettings;
begin
  repeat
    vLine := ReadLine;
    if vLine = '' then
      raise ERALRedisError.Create(emRedisProtocol);
    vType := vLine[1];
    Delete(vLine, 1, 1);
    // an attribute (RESP3) comes before a reply: read it and let it go
    if vType = '|' then
    begin
      vCount := StrToInt64(string(vLine));
      for vInt := 1 to vCount * 2 do
        ReadValue().Free; // () : FPC reads the bare name as the result
    end;
  until vType <> '|';
  case vType of
    '+':
      begin
        Result := TRALRedisReply.Create(rrkStatus);
        Result.FText := vLine;
      end;
    '-':
      begin
        Result := TRALRedisReply.Create(rrkError);
        Result.FText := vLine;
      end;
    ':':
      begin
        Result := TRALRedisReply.Create(rrkInteger);
        Result.FInteger := StrToInt64(string(vLine));
      end;
    '_':
      Result := TRALRedisReply.Create(rrkNil);
    '#':
      begin
        Result := TRALRedisReply.Create(rrkBoolean);
        if vLine = 't' then
          Result.FInteger := 1;
        Result.FText := vLine;
      end;
    ',':
      begin
        Result := TRALRedisReply.Create(rrkDouble);
        Result.FText := vLine;
        vFormat := {$IFDEF FPC}DefaultFormatSettings{$ELSE}TFormatSettings.Create{$ENDIF};
        vFormat.DecimalSeparator := '.';
        Result.FInteger := Trunc(StrToFloatDef(string(vLine), 0, vFormat));
      end;
    '(':
      begin
        Result := TRALRedisReply.Create(rrkBigNumber);
        Result.FText := vLine;
      end;
    '$', '!', '=':
      begin
        vCount := StrToInt64(string(vLine));
        if vCount < 0 then
          Result := TRALRedisReply.Create(rrkNil)
        else
        begin
          if vType = '!' then
            Result := TRALRedisReply.Create(rrkError)
          else
            Result := TRALRedisReply.Create(rrkString);
          Result.FText := ReadBytes(vCount);
          // a verbatim string starts with its format and a colon (txt:)
          if (vType = '=') and (Length(Result.FText) >= 4) then
            Delete(Result.FText, 1, 4);
        end;
      end;
    '*', '%', '~', '>':
      begin
        vCount := StrToInt64(string(vLine));
        if vCount < 0 then
        begin
          Result := TRALRedisReply.Create(rrkNil);
          Exit;
        end;
        case vType of
          '%': Result := TRALRedisReply.Create(rrkMap);
          '~': Result := TRALRedisReply.Create(rrkSet);
          '>': Result := TRALRedisReply.Create(rrkPush);
        else
          Result := TRALRedisReply.Create(rrkArray);
        end;
        vItems := vCount;
        if vType = '%' then
          vItems := vCount * 2;
        try
          for vInt := 1 to vItems do
            Result.Add(ReadValue());
        except
          Result.Free;
          raise;
        end;
      end;
  else
    raise ERALRedisError.Create(emRedisProtocol);
  end;
end;

procedure TRALRedisConnection.Send(const AArgs: array of RawByteString);
begin
  SendRaw(RALRedisEncode(AArgs));
end;

procedure TRALRedisConnection.SendRaw(const AData: RawByteString);
begin
  try
    FChannel.WriteAll(Pointer(AData)^, Length(AData), FCommandTimeout);
    FChannel.Flush;
  except
    on E: ERALRedisError do
      raise;
    on E: Exception do
      raise ERALRedisError.CreateFmt(emRedisWrite, [E.Message]);
  end;
end;

{ TRALRedisPipeline }

constructor TRALRedisPipeline.Create(AClient: TRALRedisClient; ATransaction: boolean);
begin
  inherited Create;
  FClient := AClient;
  FTransaction := ATransaction;
end;

procedure TRALRedisPipeline.Add(const AArgs: array of RawByteString);
begin
  FData := FData + RALRedisEncode(AArgs);
  Inc(FCount);
end;

function TRALRedisPipeline.Execute: TObjectList;
var
  vConn: TRALRedisConnection;
  vInt: IntegerRAL;
  vReply, vExec: TRALRedisReply;
  vData: RawByteString;
begin
  Result := TObjectList.Create(True);
  if FCount = 0 then
    Exit;
  vData := FData;
  if FTransaction then
    vData := RALRedisEncode(['MULTI']) + vData + RALRedisEncode(['EXEC']);
  vConn := FClient.Acquire;
  try
    vConn.SendRaw(vData);
    if FTransaction then
    begin
      // +OK of MULTI, +QUEUED (or an error) per command, then the array of EXEC
      vConn.ReadReply(FClient.CommandTimeout).Free;
      for vInt := 1 to FCount do
      begin
        vReply := vConn.ReadReply(FClient.CommandTimeout);
        if vReply = nil then
          raise ERALRedisError.CreateFmt(emRedisTimeout, [FClient.CommandTimeout]);
        vReply.Free;
      end;
      vExec := vConn.ReadReply(FClient.CommandTimeout);
      if vExec = nil then
        raise ERALRedisError.CreateFmt(emRedisTimeout, [FClient.CommandTimeout]);
      try
        if vExec.Kind = rrkError then
          raise ERALRedisReplyError.CreateFmt(emRedisReply, [string(vExec.Text)]);
        if vExec.IsNil then
          Result.Add(TRALRedisReply.Create(rrkNil))
        else
          // the children move to the list the caller gets
          while vExec.Count > 0 do
            Result.Add(vExec.FItems.Extract(vExec.FItems[0]));
      finally
        vExec.Free;
      end;
    end
    else
      for vInt := 1 to FCount do
      begin
        vReply := vConn.ReadReply(FClient.CommandTimeout);
        if vReply = nil then
          raise ERALRedisError.CreateFmt(emRedisTimeout, [FClient.CommandTimeout]);
        Result.Add(vReply);
      end;
    FClient.Release(vConn);
  except
    on E: ERALRedisReplyError do
    begin
      FClient.Release(vConn);
      Result.Free;
      raise;
    end;
    on E: Exception do
    begin
      vConn.Free;
      Result.Free;
      raise;
    end;
  end;
  FData := '';
  FCount := 0;
end;

{ TRALRedisClient }

constructor TRALRedisClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIdle := TList.Create;
  FLock := TCriticalSection.Create;
  FSSL := TRALRedisSSL.Create;
  FHost := 'localhost';
  FPort := RALRedisDefaultPort;
  FProtocol := rp3;
  FPoolSize := 8;
  FConnectTimeout := 5000;
  FCommandTimeout := 10000;
end;

destructor TRALRedisClient.Destroy;
begin
  ClearPool;
  FreeAndNil(FIdle);
  FreeAndNil(FLock);
  FreeAndNil(FSSL);
  inherited Destroy;
end;

function TRALRedisClient.Acquire: TRALRedisConnection;
begin
  repeat
    Result := nil;
    FLock.Acquire;
    try
      if FIdle.Count > 0 then
      begin
        Result := TRALRedisConnection(FIdle[FIdle.Count - 1]);
        FIdle.Delete(FIdle.Count - 1);
      end;
    finally
      FLock.Release;
    end;
    if Result = nil then
    begin
      Result := NewConnection;
      Exit;
    end;
    // one the server closed while it waited is dropped, never used
    if Result.Alive then
      Exit;
    Result.Free;
  until False;
end;

procedure TRALRedisClient.ClearPool;
var
  vInt: IntegerRAL;
begin
  FLock.Acquire;
  try
    for vInt := 0 to FIdle.Count - 1 do
      TObject(FIdle[vInt]).Free;
    FIdle.Clear;
  finally
    FLock.Release;
  end;
end;

function TRALRedisClient.Command(const AArgs: array of RawByteString): TRALRedisReply;
var
  vConn: TRALRedisConnection;
  vError: StringRAL;
  vRaise: ERALRedisReplyError;
begin
  vConn := Acquire;
  try
    Result := vConn.Execute(AArgs);
  except
    vConn.Free; // a broken connection never goes back
    raise;
  end;
  Release(vConn);
  if Result.Kind = rrkError then
  begin
    vError := StringRAL(Result.Text);
    Result.Free;
    vRaise := ERALRedisReplyError.CreateFmt(emRedisReply, [string(vError)]);
    vRaise.FServerError := vError;
    raise vRaise;
  end;
end;

function TRALRedisClient.CommandInt(const AArgs: array of RawByteString): Int64RAL;
var
  vReply: TRALRedisReply;
begin
  vReply := Command(AArgs);
  try
    if vReply.Kind in [rrkInteger, rrkBoolean, rrkDouble] then
      Result := vReply.AsInteger
    else
      Result := StrToInt64Def(string(vReply.Text), 0);
  finally
    vReply.Free;
  end;
end;

function TRALRedisClient.CommandText(const AArgs: array of RawByteString): RawByteString;
var
  vReply: TRALRedisReply;
begin
  vReply := Command(AArgs);
  try
    if vReply.Kind in [rrkInteger, rrkBoolean] then
      Result := RawByteString(IntToStr(vReply.AsInteger))
    else
      Result := vReply.Text;
  finally
    vReply.Free;
  end;
end;

function TRALRedisClient.Delete(const AKeys: array of RawByteString): Int64RAL;
var
  vArgs: array of RawByteString;
  vInt: IntegerRAL;
begin
  SetLength(vArgs, Length(AKeys) + 1);
  vArgs[0] := 'DEL';
  for vInt := 0 to High(AKeys) do
    vArgs[vInt + 1] := AKeys[vInt];
  Result := CommandInt(vArgs);
end;

function TRALRedisClient.Eval(const AScript: RawByteString;
  const AKeys, AArgs: array of RawByteString): TRALRedisReply;
var
  vArgs: array of RawByteString;
  vInt, vPos: IntegerRAL;
begin
  SetLength(vArgs, 3 + Length(AKeys) + Length(AArgs));
  vArgs[0] := 'EVAL';
  vArgs[1] := AScript;
  vArgs[2] := RawByteString(IntToStr(Length(AKeys)));
  vPos := 3;
  for vInt := 0 to High(AKeys) do
  begin
    vArgs[vPos] := AKeys[vInt];
    Inc(vPos);
  end;
  for vInt := 0 to High(AArgs) do
  begin
    vArgs[vPos] := AArgs[vInt];
    Inc(vPos);
  end;
  Result := Command(vArgs);
end;

function TRALRedisClient.Exists(const AKey: RawByteString): boolean;
begin
  Result := CommandInt(['EXISTS', AKey]) > 0;
end;

function TRALRedisClient.Expire(const AKey: RawByteString; AMilliseconds: Int64RAL): boolean;
begin
  Result := CommandInt(['PEXPIRE', AKey, RawByteString(IntToStr(AMilliseconds))]) = 1;
end;

function TRALRedisClient.GetValue(const AKey: RawByteString; out AValue: RawByteString): boolean;
var
  vReply: TRALRedisReply;
begin
  vReply := Command(['GET', AKey]);
  try
    Result := not vReply.IsNil;
    AValue := vReply.Text;
  finally
    vReply.Free;
  end;
end;

function TRALRedisClient.HashGet(const AKey, AField: RawByteString;
  out AValue: RawByteString): boolean;
var
  vReply: TRALRedisReply;
begin
  vReply := Command(['HGET', AKey, AField]);
  try
    Result := not vReply.IsNil;
    AValue := vReply.Text;
  finally
    vReply.Free;
  end;
end;

procedure TRALRedisClient.HashGetAll(const AKey: RawByteString; AList: TStrings);
var
  vReply: TRALRedisReply;
  vInt: IntegerRAL;
begin
  vReply := Command(['HGETALL', AKey]);
  try
    vInt := 0;
    while vInt + 1 < vReply.Count do
    begin
      AList.Add(string(vReply[vInt].AsString) + '=' + string(vReply[vInt + 1].AsString));
      Inc(vInt, 2);
    end;
  finally
    vReply.Free;
  end;
end;

function TRALRedisClient.HashSet(const AKey, AField, AValue: RawByteString): boolean;
begin
  Result := CommandInt(['HSET', AKey, AField, AValue]) = 1;
end;

function TRALRedisClient.Increment(const AKey: RawByteString; ADelta: Int64RAL): Int64RAL;
begin
  Result := CommandInt(['INCRBY', AKey, RawByteString(IntToStr(ADelta))]);
end;

function TRALRedisClient.ListPop(const AKey: RawByteString; out AValue: RawByteString;
  AFromLeft: boolean): boolean;
var
  vReply: TRALRedisReply;
begin
  if AFromLeft then
    vReply := Command(['LPOP', AKey])
  else
    vReply := Command(['RPOP', AKey]);
  try
    Result := not vReply.IsNil;
    AValue := vReply.Text;
  finally
    vReply.Free;
  end;
end;

function TRALRedisClient.ListPush(const AKey, AValue: RawByteString; AToLeft: boolean): Int64RAL;
begin
  if AToLeft then
    Result := CommandInt(['LPUSH', AKey, AValue])
  else
    Result := CommandInt(['RPUSH', AKey, AValue]);
end;

procedure TRALRedisClient.ListRange(const AKey: RawByteString; AStart, AStop: Int64RAL;
  AList: TStrings);
var
  vReply: TRALRedisReply;
  vInt: IntegerRAL;
begin
  vReply := Command(['LRANGE', AKey, RawByteString(IntToStr(AStart)),
    RawByteString(IntToStr(AStop))]);
  try
    for vInt := 0 to vReply.Count - 1 do
      AList.Add(string(vReply[vInt].AsString));
  finally
    vReply.Free;
  end;
end;

function TRALRedisClient.NewConnection: TRALRedisConnection;
begin
  Result := TRALRedisConnection.Create(Settings, FSSL, Self, FOnValidateServerCert);
end;

function TRALRedisClient.NewPipeline(ATransaction: boolean): TRALRedisPipeline;
begin
  Result := TRALRedisPipeline.Create(Self, ATransaction);
end;

function TRALRedisClient.Ping: boolean;
begin
  Result := CommandText(['PING']) = 'PONG';
end;

function TRALRedisClient.Publish(const AChannel, AMessage: RawByteString): Int64RAL;
begin
  Result := CommandInt(['PUBLISH', AChannel, AMessage]);
end;

procedure TRALRedisClient.Release(AConnection: TRALRedisConnection);
begin
  if AConnection = nil then
    Exit;
  FLock.Acquire;
  try
    if FIdle.Count < FPoolSize then
    begin
      FIdle.Add(AConnection);
      AConnection := nil;
    end;
  finally
    FLock.Release;
  end;
  AConnection.Free;
end;

procedure TRALRedisClient.Scan(const APattern: RawByteString; AList: TStrings);
var
  vCursor: RawByteString;
  vReply: TRALRedisReply;
  vInt: IntegerRAL;
begin
  vCursor := '0';
  repeat
    vReply := Command(['SCAN', vCursor, 'MATCH', APattern, 'COUNT', '500']);
    try
      vCursor := vReply[0].Text;
      for vInt := 0 to vReply[1].Count - 1 do
        AList.Add(string(vReply[1][vInt].Text));
    finally
      vReply.Free;
    end;
  until vCursor = '0';
end;

function TRALRedisClient.SetAdd(const AKey, AMember: RawByteString): boolean;
begin
  Result := CommandInt(['SADD', AKey, AMember]) = 1;
end;

procedure TRALRedisClient.SetMembers(const AKey: RawByteString; AList: TStrings);
var
  vReply: TRALRedisReply;
  vInt: IntegerRAL;
begin
  vReply := Command(['SMEMBERS', AKey]);
  try
    for vInt := 0 to vReply.Count - 1 do
      AList.Add(string(vReply[vInt].AsString));
  finally
    vReply.Free;
  end;
end;

function TRALRedisClient.SetRemove(const AKey, AMember: RawByteString): boolean;
begin
  Result := CommandInt(['SREM', AKey, AMember]) = 1;
end;

procedure TRALRedisClient.SetSSL(AValue: TRALRedisSSL);
begin
  FSSL.Assign(AValue);
end;

procedure TRALRedisClient.SetValue(const AKey, AValue: RawByteString; AMilliseconds: Int64RAL);
begin
  if AMilliseconds > 0 then
    Command(['SET', AKey, AValue, 'PX', RawByteString(IntToStr(AMilliseconds))]).Free
  else
    Command(['SET', AKey, AValue]).Free;
end;

function TRALRedisClient.Settings: TRALRedisSettings;
begin
  Result.ClientName := FClientName;
  Result.CommandTimeout := FCommandTimeout;
  Result.ConnectTimeout := FConnectTimeout;
  Result.Database := FDatabase;
  Result.Host := FHost;
  Result.Password := FPassword;
  Result.Port := FPort;
  Result.Protocol := FProtocol;
  Result.UserName := FUserName;
end;

function TRALRedisClient.TimeToLive(const AKey: RawByteString): Int64RAL;
begin
  Result := CommandInt(['PTTL', AKey]);
end;

end.
