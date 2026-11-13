{ The SMTP server: receives mail (RFC 5321) on ports 25, 587 and 465 over RAL's
  socket event loop, with STARTTLS, AUTH through the server's plugins,
  PIPELINING and CHUNKING. }
unit RALSMTPServer;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALBase64, RALServer, RALRequest, RALResponse,
  RALRoutes, RALCustomObjects, RALStream, RALSocket, RALSocketLoop, RALTLS,
  RALSCRAM, RALMailMIME;

const
  /// The listener of Port (25): mail from other servers.
  RALSMTPListenerRelay = 0;
  /// The listener of SubmissionPort (587): mail from users, AUTH required.
  RALSMTPListenerSubmission = 1;
  /// The listener of TLSPort (465): submission over implicit TLS, AUTH required.
  RALSMTPListenerTLS = 2;

type
  TRALSMTPServer = class;
  TRALSMTPConnection = class;

  /// A SASL mechanism the server offers.
  TRALSMTPServerAuthMethod = (
    /// PLAIN (RFC 4616), only over TLS.
    ssaPlain,
    /// LOGIN, only over TLS.
    ssaLogin,
    /// SCRAM-SHA-256 (RFC 7677), with the passwords of Users or OnSCRAMCredential.
    ssaScramSHA256,
    /// XOAUTH2 (Google, Microsoft): the token goes to the authenticators as a Bearer.
    ssaXOAuth2,
    /// OAUTHBEARER (RFC 7628): the token goes to the authenticators as a Bearer.
    ssaOAuthBearer
  );
  /// The mechanisms the server offers.
  TRALSMTPServerAuthMethods = set of TRALSMTPServerAuthMethod;

  /// TLS of the TLS port and of STARTTLS.
  TRALSMTPServerSSL = class(TRALSSL)
  private
    FCertFile: StringRAL;
    FCertPEM: StringRAL;
    FKeyFile: StringRAL;
    FKeyPassword: StringRAL;
    FKeyPEM: StringRAL;
    FMinVersion: TRALTLSVersion;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;

    /// Certificate as PEM text (set by SetTLSCertificate, RALSelfSigned).
    property CertPEM: StringRAL read FCertPEM write FCertPEM;
    /// Key as PEM text.
    property KeyPEM: StringRAL read FKeyPEM write FKeyPEM;
  published
    /// Certificate file (PEM, the chain after it).
    property CertFile: StringRAL read FCertFile write FCertFile;
    /// Private key file (PEM).
    property KeyFile: StringRAL read FKeyFile write FKeyFile;
    /// Password of an encrypted key file.
    property KeyPassword: StringRAL read FKeyPassword write FKeyPassword;
    /// The oldest TLS version accepted.
    property MinVersion: TRALTLSVersion read FMinVersion write FMinVersion default mtv12;
  end;

  /// One client connection as the events see it.
  TRALSMTPSession = class
  private
    FAuthenticated: boolean;
    FHelo: StringRAL;
    FId: StringRAL;
    FListener: IntegerRAL;
    FRemoteIP: StringRAL;
    FRemotePort: IntegerRAL;
    FTag: TObject;
    FTLS: boolean;
    FUserName: StringRAL;
  public
    /// Whether AUTH succeeded.
    property Authenticated: boolean read FAuthenticated;
    /// The name the client gave in EHLO or HELO.
    property Helo: StringRAL read FHelo;
    /// A unique identifier of the connection, also in the Received header.
    property Id: StringRAL read FId;
    /// RALSMTPListenerRelay, RALSMTPListenerSubmission or RALSMTPListenerTLS.
    property Listener: IntegerRAL read FListener;
    /// The client's address.
    property RemoteIP: StringRAL read FRemoteIP;
    /// The client's port.
    property RemotePort: IntegerRAL read FRemotePort;
    /// An object of the application for this connection (not freed by the server).
    property Tag: TObject read FTag write FTag;
    /// Whether the connection is encrypted.
    property TLS: boolean read FTLS;
    /// The user AUTH accepted.
    property UserName: StringRAL read FUserName;
  end;

  /// The envelope of a received message.
  TRALSMTPEnvelope = class
  private
    FBody8Bit: boolean;
    FFrom: StringRAL;
    FRecipients: TStringList;
    FUTF8: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    /// Empties the envelope for a new transaction.
    procedure Clear;

    /// BODY=8BITMIME was declared.
    property Body8Bit: boolean read FBody8Bit;
    /// The reverse path (MAIL FROM); empty for a bounce.
    property From: StringRAL read FFrom;
    /// The accepted forward paths (RCPT TO).
    property Recipients: TStringList read FRecipients;
    /// SMTPUTF8 was declared.
    property UTF8: boolean read FUTF8;
  end;

  /// A new connection: AAccept False refuses it with AReply (a 554 by default).
  TRALSMTPConnectEvent = procedure(Sender: TObject; ASession: TRALSMTPSession;
    var AAccept: boolean; var AReply: StringRAL) of object;
  { MAIL FROM or RCPT TO: set ACode (250 by default) and AReply to refuse,
    for example 550 '5.1.1 no such user'. }
  TRALSMTPAddressEvent = procedure(Sender: TObject; ASession: TRALSMTPSession;
    const AAddress: StringRAL; var ACode: IntegerRAL; var AReply: StringRAL) of object;
  { A whole message: AData holds it (the Received header first), in memory or a
    temporary file, freed after the event: copy what must outlive it. ACode
    (250 by default) and AReply answer the client. }
  TRALSMTPMessageEvent = procedure(Sender: TObject; ASession: TRALSMTPSession;
    AEnvelope: TRALSMTPEnvelope; AData: TStream; var ACode: IntegerRAL;
    var AReply: StringRAL) of object;
  { The SCRAM credential of AUserName, for SCRAM-SHA-256 without Users: fill
    ACredential (the password, or the salted keys) and set AFound. }
  TRALSMTPSCRAMEvent = procedure(Sender: TObject; const AUserName: StringRAL;
    var ACredential: TRALSCRAMCredential; var AFound: boolean) of object;

  /// Where a connection is: TLS handshake, commands, DATA, BDAT, AUTH or closed.
  TRALSMTPPhase = (sspTLS, sspCommand, sspData, sspBdat, sspAuth, sspClosed);

  { One client connection, owned by one poll thread of the server's loop: the
    commands, the replies and the events all run on that thread. }
  TRALSMTPConnection = class(TRALSocketLoopConnection)
  private
    /// True when the next data byte starts a line.
    FAtLineStart: boolean;
    /// The mechanism of the AUTH in progress.
    FAuthMethod: TRALSMTPServerAuthMethod;
    /// The step of the AUTH in progress.
    FAuthStep: IntegerRAL;
    /// The user being authenticated.
    FAuthUser: StringRAL;
    /// Unknown or out of order commands so far.
    FBadCommands: IntegerRAL;
    /// True when the current BDAT chunk is the last.
    FBdatLast: boolean;
    /// Bytes of the current BDAT chunk still to come.
    FBdatLeft: Int64RAL;
    /// The message being received.
    FData: TRALBodyStream;
    /// Bytes of the message received so far.
    FDataSize: Int64RAL;
    /// The current transaction's envelope.
    FEnvelope: TRALSMTPEnvelope;
    /// The received bytes not yet processed.
    FIn: RawByteString;
    /// The next byte of FIn to process.
    FInPos: IntegerRAL;
    /// When the connection last moved (RALSocketNow).
    FLastActive: Int64RAL;
    /// True when the last data byte was a CR.
    FLastCR: boolean;
    /// True when MAIL FROM was given in this transaction.
    FMailGiven: boolean;
    /// The replies not yet sent.
    FOut: RawByteString;
    /// The next byte of FOut to send.
    FOutPos: IntegerRAL;
    /// Where the connection is in the dialog.
    FPhase: TRALSMTPPhase;
    /// The socket channel under TLS.
    FPlain: TRALChannel;
    /// The SCRAM exchange in progress.
    FSCRAM: TRALSCRAMServer;
    /// The server that owns the connection.
    FServer: TRALSMTPServer;
    /// What the events see of the connection.
    FSession: TRALSMTPSession;
    /// True when the TLS handshake starts once the reply is sent.
    FStartTLS: boolean;
    /// The TLS channel, once STARTTLS or implicit TLS is on.
    FTLS: TRALTLSChannel;
    /// True when the message passed MaxMessageSize; the rest is dropped.
    FTooBig: boolean;
    /// Messages received on this connection.
    FTransactions: IntegerRAL;

    /// Adds ACount bytes to the message, enforcing MaxMessageSize.
    procedure AppendData(const ABuffer; ACount: IntegerRAL);
    /// Ends AUTH: accepts AUser, refuses, or answers a temporary failure.
    procedure AuthFinish(AOk: boolean; const AUser: StringRAL; ATempFail: boolean = False);
    /// Handles a line of the AUTH exchange.
    procedure AuthLine(const ALine: RawByteString);
    /// Opens a new message body.
    procedure BeginData;
    /// Runs one command line.
    procedure Command(const ALine: RawByteString);
    /// Gives the finished message to OnMessage and replies.
    procedure Deliver;
    /// The AUTH command.
    procedure DoAuth(const AArgs: RawByteString);
    /// The EHLO and HELO commands.
    procedure DoEhlo(const AName: RawByteString; AExtended: boolean);
    /// The MAIL command.
    procedure DoMail(const AArgs: RawByteString);
    /// The RCPT command.
    procedure DoRcpt(const AArgs: RawByteString);
    /// Sends the 220 greeting.
    procedure Greet;
    /// Takes the BDAT chunk from the input; False when more bytes are needed.
    function ProcessBdat: boolean;
    /// Takes the DATA text from the input; False when more bytes are needed.
    function ProcessData: boolean;
    /// Serves every whole command and the data in the input buffer.
    procedure ProcessInput;
    /// Queues a reply line for sending.
    procedure Reply(const AText: StringRAL);
    /// Forgets the envelope and the message of the transaction.
    procedure ResetTransaction;
    /// Starts the TLS handshake on the connection.
    procedure SwitchToTLS;
  protected
    procedure Closed; override;
  public
    /// Creates the connection of ASocket, accepted on listener AListener.
    constructor Create(AServer: TRALSMTPServer; ASocket: TRALSocket; AListener: IntegerRAL);
    destructor Destroy; override;

    function FlushOutput: boolean; override;
    procedure HandleError(AError: Exception); override;
    function HasOutput: boolean; override;
    function PollEvents: Word; override;
    procedure Serve(ANow: Int64RAL); override;
    procedure Stopping; override;
  end;

  { An SMTP server (RFC 5321) over the socket event loop: Port (25) takes mail
    from other servers, SubmissionPort (587) and TLSPort (465, implicit TLS)
    take mail from users, who must authenticate there. STARTTLS (RFC 3207),
    PIPELINING (RFC 2920), CHUNKING/BDAT (RFC 3030), 8BITMIME, SMTPUTF8, SIZE
    (RFC 1870) and enhanced status codes (RFC 2034). AUTH goes through the
    server's plugins as a request to an internal route: the authenticators
    (Basic, JWT as the password with PasswordIsToken), the address lists, the
    brute-force and flood plugins apply as they do to HTTP; with no
    authenticator, Users. Each connection also passes the address lists and the
    flood plugin when it opens. The server stores nothing: OnMessage gets each
    message with its envelope. With no OnRcptTo every recipient is accepted:
    on Port, that is an open relay unless OnRcptTo checks the domains. }
  TRALSMTPServer = class(TRALServer)
  private
    FAllowPlainAuthWithoutTLS: boolean;
    FAuthMethods: TRALSMTPServerAuthMethods;
    /// The internal module that carries the AUTH routes.
    FAuthModule: TRALModuleRoutes;
    FBanner: StringRAL;
    FCommandTimeout: IntegerRAL;
    FHostName: StringRAL;
    /// The ports being listened on.
    FListeners: TList;
    /// Guards FTLSContext and the creation of connections.
    FLock: TCriticalSection;
    /// The socket event loop.
    FLoop: TRALSocketLoop;
    FMaxConnections: IntegerRAL;
    FMaxMessageSize: Int64RAL;
    FMaxRecipients: IntegerRAL;
    FMemoryLimit: Int64RAL;
    /// The counter of connection ids.
    FNextId: Int64RAL;
    FOnConnect: TRALSMTPConnectEvent;
    FOnMailFrom: TRALSMTPAddressEvent;
    FOnMessage: TRALSMTPMessageEvent;
    FOnRcptTo: TRALSMTPAddressEvent;
    FOnSCRAMCredential: TRALSMTPSCRAMEvent;
    FPasswordIsToken: boolean;
    FRequireAuth: boolean;
    FRequireTLS: boolean;
    /// Replaced TLS contexts, kept until the server stops.
    FRetiredContexts: TList;
    /// True while the server listens.
    FRunning: boolean;
    /// When the server started (RALSocketNow).
    FStarted: Int64RAL;
    FSubmissionPort: IntegerRAL;
    FThreadCount: IntegerRAL;
    /// The TLS context of the listeners and STARTTLS.
    FTLSContext: TRALTLSContext;
    FTLSPort: IntegerRAL;
    FUsers: TStrings;

    /// Takes a connection accepted on listener AListener.
    procedure Accepted(ASocket: TRALSocket; AListener: IntegerRAL);
    /// The internal route that asks the plugins about a login.
    procedure AuthRoute(ARequest: TRALRequest; AResponse: TRALResponse);
    { the plugins' verdict on a connection (AAuth empty) or on credentials:
      an HTTP status }
    function CheckAccess(ASession: TRALSMTPSession; const AAuth: StringRAL;
      AValidateOnly: boolean): IntegerRAL;
    /// Checks a user and password with the plugins, then Users.
    function CheckPassword(ASession: TRALSMTPSession; const AUser, APassword: StringRAL;
      out ATempFail: boolean): boolean;
    /// Checks a token with the plugins.
    function CheckToken(ASession: TRALSMTPSession; const AToken: StringRAL;
      out ATempFail: boolean): boolean;
    /// Reads the SCRAM credential of AUserName, from OnSCRAMCredential or Users.
    function FindSCRAMUser(const AUserName: StringRAL;
      out ACredential: TRALSCRAMCredential): boolean;
    function GetSSL: TRALSMTPServerSSL;
    /// A new session id.
    function NewId: StringRAL;
    /// A TLS context built from the SSL settings.
    function NewTLSContext: TRALTLSContext;
    procedure SetSSL(AValue: TRALSMTPServerSSL);
    procedure SetUsers(AValue: TStrings);
    /// Opens the ports.
    procedure StartListeners;
    /// Closes the ports and the connections.
    procedure StopAll;
  protected
    function CreateRALSSL: TRALSSL; override;
    function IPv6IsImplemented: boolean; override;
    procedure SetActive(const AValue: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Open connections.
    function ConnectionCount: IntegerRAL;
    function GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean; override;
    /// Hides the HTTP properties that mean nothing to an SMTP server.
    function IsPropertyRelevant(const AName: StringRAL): boolean; override;
    function SetTLSCertificate(const ACertificate: TRALTLSCertificate): TRALTLSApplyResult; override;
    { Whether the ports open and the certificate loads, without starting; AReport
      says it port by port (the verb "Test ports" of the IDE). }
    function TestPorts(out AReport: StringRAL): boolean;
    function TLSProvisioning: TRALTLSProvisioning; override;
  published
    { Offers PLAIN and LOGIN on a connection without TLS: the password then
      crosses the network in clear. Only for tests. }
    property AllowPlainAuthWithoutTLS: boolean read FAllowPlainAuthWithoutTLS
      write FAllowPlainAuthWithoutTLS default False;
    /// The SASL mechanisms offered.
    property AuthMethods: TRALSMTPServerAuthMethods read FAuthMethods write FAuthMethods
      default [ssaPlain, ssaLogin, ssaScramSHA256];
    /// Text after the host name in the greeting.
    property Banner: StringRAL read FBanner write FBanner;
    /// Milliseconds a connection may stay silent (RFC 5321 4.5.3.2: at least 5 minutes).
    property CommandTimeout: IntegerRAL read FCommandTimeout write FCommandTimeout default 300000;
    /// The name in the greeting, EHLO and Received (its DNS name); empty is 'localhost'.
    property HostName: StringRAL read FHostName write FHostName;
    /// Connections served at once; the next ones get 421.
    property MaxConnections: IntegerRAL read FMaxConnections write FMaxConnections default 1000;
    /// The largest message accepted, in bytes (SIZE); 0 = no limit.
    property MaxMessageSize: Int64RAL read FMaxMessageSize write FMaxMessageSize default 26214400;
    /// Recipients per message.
    property MaxRecipients: IntegerRAL read FMaxRecipients write FMaxRecipients default 100;
    /// A message larger than this goes to a temporary file instead of memory.
    property MemoryLimit: Int64RAL read FMemoryLimit write FMemoryLimit default 4194304;
    /// A client connected; may refuse it.
    property OnConnect: TRALSMTPConnectEvent read FOnConnect write FOnConnect;
    /// MAIL FROM was given; may refuse it.
    property OnMailFrom: TRALSMTPAddressEvent read FOnMailFrom write FOnMailFrom;
    /// A message was received.
    property OnMessage: TRALSMTPMessageEvent read FOnMessage write FOnMessage;
    /// RCPT TO was given; may refuse it.
    property OnRcptTo: TRALSMTPAddressEvent read FOnRcptTo write FOnRcptTo;
    /// Asks for the SCRAM credential of a user.
    property OnSCRAMCredential: TRALSMTPSCRAMEvent read FOnSCRAMCredential write FOnSCRAMCredential;
    /// The password of AUTH is a token (JWT): it goes to the authenticators as a Bearer.
    property PasswordIsToken: boolean read FPasswordIsToken write FPasswordIsToken default False;
    /// MAIL needs AUTH on every port (SubmissionPort and TLSPort always need it).
    property RequireAuth: boolean read FRequireAuth write FRequireAuth default False;
    /// MAIL and AUTH need TLS (STARTTLS) on every port.
    property RequireTLS: boolean read FRequireTLS write FRequireTLS default False;
    /// TLS of TLSPort and STARTTLS.
    property SSL: TRALSMTPServerSSL read GetSSL write SetSSL;
    /// The submission port (587); 0 closes it.
    property SubmissionPort: IntegerRAL read FSubmissionPort write FSubmissionPort default 587;
    /// Poll threads; 0 = one per processor.
    property ThreadCount: IntegerRAL read FThreadCount write FThreadCount default 0;
    /// The implicit TLS port (465), opened when SSL is enabled; 0 closes it.
    property TLSPort: IntegerRAL read FTLSPort write FTLSPort default 465;
    { name=password pairs AUTH checks when no authenticator plugin is linked;
      SCRAM-SHA-256 uses them too. }
    property Users: TStrings read FUsers write SetUsers;
  end;

implementation

const
  /// The internal route the authentication goes through.
  cAuthRoute = 'smtp/auth';
  /// The longest command line, in bytes.
  cMaxLine = 16384;
  /// Bad commands after which the connection is closed.
  cMaxBadCommands = 10;

type
  /// One open port.
  TRALSMTPListener = class
  public
    /// Which port it is: Port, SubmissionPort or TLSPort.
    Kind: IntegerRAL;
    /// The listening socket.
    Socket: TRALSocket;

    destructor Destroy; override;
  end;

  /// The module that carries the authentication route.
  TRALSMTPAuthModule = class(TRALModuleRoutes);

destructor TRALSMTPListener.Destroy;
begin
  FreeAndNil(Socket);
  inherited Destroy;
end;

/// AText with a-z turned into A-Z.
function UpperAscii(const AText: RawByteString): RawByteString;
var
  vInt: IntegerRAL;
begin
  SetLength(Result, Length(AText));
  for vInt := 1 to Length(AText) do
    if AText[vInt] in ['a'..'z'] then
      Result[vInt] := AnsiChar(Ord(AText[vInt]) - 32)
    else
      Result[vInt] := AText[vInt];
end;

/// Trim without a trip through string (Delphi's Trim has no RawByteString version).
function TrimRaw(const AText: RawByteString): RawByteString;
var
  vStart, vEnd: IntegerRAL;
begin
  vStart := 1;
  vEnd := Length(AText);
  while (vStart <= vEnd) and (AText[vStart] <= ' ') do
    Inc(vStart);
  while (vEnd >= vStart) and (AText[vEnd] <= ' ') do
    Dec(vEnd);
  Result := Copy(AText, vStart, vEnd - vStart + 1);
end;

{ the address between < and > of a MAIL or RCPT argument, and what follows it;
  False when there are no brackets }
function SplitPath(const AArgs: RawByteString; out APath, AParams: RawByteString): boolean;
var
  vOpen, vClose: IntegerRAL;
begin
  vOpen := Pos(RawByteString('<'), AArgs);
  vClose := Pos(RawByteString('>'), AArgs);
  Result := (vOpen > 0) and (vClose > vOpen);
  if not Result then
    Exit;
  APath := Copy(AArgs, vOpen + 1, vClose - vOpen - 1);
  // a source route (@a,@b:user@c) is ignored, as RFC 5321 4.1.1.3 allows
  if (APath <> '') and (APath[1] = '@') and (Pos(RawByteString(':'), APath) > 0) then
    APath := Copy(APath, Pos(RawByteString(':'), APath) + 1, MaxInt);
  AParams := TrimRaw(Copy(AArgs, vClose + 1, MaxInt));
end;

{ The value of AName in a MAIL or RCPT parameter list (NAME=value, or '1' for a bare
  NAME); empty when absent. }
function ParamValue(const AParams, AName: RawByteString): RawByteString;
var
  vList: TStringList;
  vInt: IntegerRAL;
begin
  Result := '';
  vList := TStringList.Create;
  try
    vList.Delimiter := ' ';
    vList.StrictDelimiter := True;
    vList.DelimitedText := string(AParams);
    for vInt := 0 to vList.Count - 1 do
      if SameText(vList.Names[vInt], string(AName)) then
        Result := RawByteString(vList.ValueFromIndex[vInt])
      else if SameText(vList[vInt], string(AName)) then
        Result := '1';
  finally
    vList.Free;
  end;
end;

{ TRALSMTPServerSSL }

constructor TRALSMTPServerSSL.Create;
begin
  inherited Create;
  FMinVersion := mtv12;
end;

procedure TRALSMTPServerSSL.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TRALSMTPServerSSL then
  begin
    TRALSMTPServerSSL(Dest).FCertFile := FCertFile;
    TRALSMTPServerSSL(Dest).FCertPEM := FCertPEM;
    TRALSMTPServerSSL(Dest).FKeyFile := FKeyFile;
    TRALSMTPServerSSL(Dest).FKeyPassword := FKeyPassword;
    TRALSMTPServerSSL(Dest).FKeyPEM := FKeyPEM;
    TRALSMTPServerSSL(Dest).FMinVersion := FMinVersion;
  end;
end;

{ TRALSMTPEnvelope }

constructor TRALSMTPEnvelope.Create;
begin
  inherited Create;
  FRecipients := TStringList.Create;
end;

destructor TRALSMTPEnvelope.Destroy;
begin
  FRecipients.Free;
  inherited Destroy;
end;

procedure TRALSMTPEnvelope.Clear;
begin
  FFrom := '';
  FRecipients.Clear;
  FBody8Bit := False;
  FUTF8 := False;
end;

{ TRALSMTPConnection }

constructor TRALSMTPConnection.Create(AServer: TRALSMTPServer; ASocket: TRALSocket;
  AListener: IntegerRAL);
begin
  inherited Create;
  FServer := AServer;
  FSocket := ASocket;
  FPlain := TRALSocketChannel.Create(ASocket);
  FChannel := FPlain;
  FEnvelope := TRALSMTPEnvelope.Create;
  FSession := TRALSMTPSession.Create;
  FSession.FId := AServer.NewId;
  FSession.FListener := AListener;
  FSession.FRemoteIP := ASocket.PeerAddress;
  FSession.FRemotePort := ASocket.PeerPort;
  FInPos := 1;
  FOutPos := 1;
  FLastActive := RALSocketNow;
  if AListener = RALSMTPListenerTLS then
  begin
    FTLS := TRALTLSChannel.Create(FChannel, AServer.FTLSContext);
    FChannel := FTLS;
    FPhase := sspTLS;
  end
  else
  begin
    FPhase := sspCommand;
    Greet;
  end;
end;

destructor TRALSMTPConnection.Destroy;
begin
  FreeAndNil(FSCRAM);
  FreeAndNil(FData);
  FreeAndNil(FEnvelope);
  FreeAndNil(FSession);
  // the stack frees down to the socket: TLS owns the plain channel, which owns the socket
  FreeAndNil(FChannel);
  FSocket := nil;
  inherited Destroy;
end;

procedure TRALSMTPConnection.Closed;
begin
  FPhase := sspClosed;
end;

procedure TRALSMTPConnection.Greet;
var
  vAccept: boolean;
  vReply: StringRAL;
  vStatus: IntegerRAL;
begin
  // the address lists and the flood plugin see the connection first
  vStatus := FServer.CheckAccess(FSession, '', True);
  if vStatus >= 400 then
  begin
    if vStatus = 429 then
      Reply('421 4.7.0 ' + FServer.FHostName + ' too many connections, try later')
    else
      Reply('554 5.7.1 ' + FServer.FHostName + ' access denied');
    CloseAfterFlush := True;
    FPhase := sspClosed;
    Exit;
  end;
  vAccept := True;
  vReply := '';
  if Assigned(FServer.FOnConnect) then
    FServer.FOnConnect(FServer, FSession, vAccept, vReply);
  if not vAccept then
  begin
    if vReply = '' then
      vReply := '554 5.7.1 ' + FServer.FHostName + ' connection refused';
    Reply(vReply);
    CloseAfterFlush := True;
    FPhase := sspClosed;
    Exit;
  end;
  vReply := '220 ' + FServer.FHostName + ' ESMTP';
  if FServer.FBanner <> '' then
    vReply := vReply + ' ' + FServer.FBanner;
  Reply(vReply);
end;

procedure TRALSMTPConnection.Reply(const AText: StringRAL);
begin
  FOut := FOut + RawByteString(AText) + #13#10;
end;

function TRALSMTPConnection.HasOutput: boolean;
begin
  Result := FOutPos <= Length(FOut);
end;

function TRALSMTPConnection.FlushOutput: boolean;
var
  vSent: IntegerRAL;
begin
  Result := True;
  try
    while FOutPos <= Length(FOut) do
    begin
      vSent := FChannel.Write(FOut[FOutPos], Length(FOut) - FOutPos + 1);
      if vSent <= 0 then
        Break;
      Inc(FOutPos, vSent);
    end;
    if FOutPos > Length(FOut) then
    begin
      FOut := '';
      FOutPos := 1;
    end;
    FChannel.Flush;
  except
    Result := False;
  end;
end;

function TRALSMTPConnection.PollEvents: Word;
begin
  Result := inherited PollEvents;
  // a STARTTLS waits for its 220 to leave in clear before reading again
  if FStartTLS then
    Result := Result and not RALPollIn;
end;

procedure TRALSMTPConnection.HandleError(AError: Exception);
begin
  if AError is ERALSocketError then
    Close
  else
  begin
    // a broken handshake or an event that raised: the client hears it, then the end
    if FPhase <> sspTLS then
      Reply('421 4.3.0 ' + FServer.FHostName + ' error: ' + StringRAL(AError.Message));
    CloseAfterFlush := True;
  end;
end;

procedure TRALSMTPConnection.Stopping;
begin
  if FPhase in [sspCommand, sspAuth] then
  begin
    Reply('421 4.3.2 ' + FServer.FHostName + ' shutting down');
    FlushOutput;
  end;
end;

procedure TRALSMTPConnection.SwitchToTLS;
begin
  // what the client sent after STARTTLS in clear is dropped (RFC 3207 5, CVE-2011-0411)
  FIn := '';
  FInPos := 1;
  FTLS := TRALTLSChannel.Create(FPlain, FServer.FTLSContext);
  FChannel := FTLS;
  FPhase := sspTLS;
  FStartTLS := False;
  // the session starts over: a new EHLO, no AUTH (RFC 3207 4.2)
  FSession.FHelo := '';
  FSession.FAuthenticated := False;
  FSession.FUserName := '';
  ResetTransaction;
end;

procedure TRALSMTPConnection.ResetTransaction;
begin
  FEnvelope.Clear;
  FMailGiven := False;
  FreeAndNil(FData);
  FDataSize := 0;
  FTooBig := False;
end;

procedure TRALSMTPConnection.Serve(ANow: Int64RAL);
var
  vBuffer: array[0..16383] of Byte;
  vRes: IntegerRAL;
  vDone: boolean;
begin
  if FPhase = sspClosed then
    Exit;
  if FStartTLS then
  begin
    // the 220 went out: the handshake starts
    if FlushOutput and not HasOutput then
      SwitchToTLS
    else
      Exit;
  end;
  if FPhase = sspTLS then
  begin
    if FTLS.HandshakeStep then
    begin
      FPhase := sspCommand;
      FSession.FTLS := True;
      FLastActive := ANow;
      // the implicit TLS port greets after the handshake
      if FSession.FListener = RALSMTPListenerTLS then
        Greet;
    end
    else
    begin
      if ANow - FLastActive > 30000 then
        Close;
      Exit;
    end;
  end;

  { a block at a time, served before the next is read: the input buffer stays
    small whatever the client sends (a 100 MB message passes through 16 KB) }
  vDone := False;
  repeat
    if FStartTLS or (FPhase = sspClosed) or CloseAfterFlush then
      Break;
    vRes := FChannel.Read(vBuffer, SizeOf(vBuffer));
    if vRes > 0 then
    begin
      SetLength(FIn, Length(FIn) + vRes);
      Move(vBuffer, FIn[Length(FIn) - vRes + 1], vRes);
      FLastActive := ANow;
      ProcessInput;
    end
    else if vRes = 0 then
      vDone := True;
  until vRes <= 0;

  if vDone then
    Close
  else if (FPhase in [sspCommand, sspAuth]) and (ANow - FLastActive > FServer.FCommandTimeout) then
  begin
    Reply('421 4.4.2 ' + FServer.FHostName + ' timeout');
    CloseAfterFlush := True;
  end
  else if (FPhase in [sspData, sspBdat]) and (ANow - FLastActive > FServer.FCommandTimeout * 2) then
    Close;
end;

procedure TRALSMTPConnection.ProcessInput;
var
  vEnd, vStart: IntegerRAL;
  vProgress: boolean;
  vLine: RawByteString;
begin
  // every whole command, and the data that follows a DATA or a BDAT
  repeat
    vProgress := False;
    if FStartTLS or (FPhase = sspClosed) or CloseAfterFlush then
      Break;
    case FPhase of
      sspData:
        vProgress := ProcessData;
      sspBdat:
        vProgress := ProcessBdat;
      sspCommand, sspAuth:
      begin
        vStart := FInPos;
        vEnd := vStart;
        while (vEnd <= Length(FIn)) and (FIn[vEnd] <> #10) do
          Inc(vEnd);
        if vEnd <= Length(FIn) then
        begin
          vLine := Copy(FIn, vStart, vEnd - vStart);
          if (vLine <> '') and (vLine[Length(vLine)] = #13) then
            SetLength(vLine, Length(vLine) - 1);
          FInPos := vEnd + 1;
          vProgress := True;
          if FPhase = sspAuth then
            AuthLine(vLine)
          else
            Command(vLine);
        end
        else if Length(FIn) - FInPos + 1 > cMaxLine then
        begin
          Reply('500 5.5.6 line too long');
          CloseAfterFlush := True;
        end;
      end;
    end;
  until not vProgress;
  // drop what was consumed
  if FInPos > 1 then
  begin
    Delete(FIn, 1, FInPos - 1);
    FInPos := 1;
  end;
end;

procedure TRALSMTPConnection.AppendData(const ABuffer; ACount: IntegerRAL);
begin
  if ACount <= 0 then
    Exit;
  Inc(FDataSize, ACount);
  if FTooBig then
    Exit;
  if (FServer.FMaxMessageSize > 0) and (FDataSize > FServer.FMaxMessageSize) then
  begin
    // read to the end and refuse: the rest is not kept
    FTooBig := True;
    Exit;
  end;
  FData.WriteBuffer(ABuffer, ACount);
end;

procedure TRALSMTPConnection.BeginData;
var
  vWith: StringRAL;
  vHeader: RawByteString;
begin
  FreeAndNil(FData);
  FData := TRALBodyStream.Create(-1, FServer.FMemoryLimit);
  FDataSize := 0;
  FTooBig := False;
  FAtLineStart := True;
  FLastCR := False;
  // the trace header (RFC 5321 4.4): ESMTP, S for TLS, A for AUTH (RFC 3848)
  vWith := 'ESMTP';
  if FSession.FTLS then
    vWith := vWith + 'S';
  if FSession.FAuthenticated then
    vWith := vWith + 'A';
  vHeader := RawByteString('Received: from ' + FSession.FHelo + ' ([' + FSession.FRemoteIP +
    '])'#13#10#9'by ' + FServer.FHostName + ' with ' + vWith + ' id ' + FSession.FId + '.' +
    StringRAL(IntToStr(FTransactions)) + ';'#13#10#9 + RALMailDate(Now) + #13#10);
  FData.WriteBuffer(vHeader[1], Length(vHeader));
end;

function TRALSMTPConnection.ProcessData: boolean;
var
  vPos, vLen, vEnd: IntegerRAL;
begin
  Result := False;
  vPos := FInPos;
  vLen := Length(FIn);
  while vPos <= vLen do
  begin
    if FAtLineStart and (FIn[vPos] = '.') then
    begin
      // the end needs the whole "." CR LF; a dot that starts a line is unstuffed
      if vPos + 2 > vLen then
        Break;
      if (FIn[vPos + 1] = #13) and (FIn[vPos + 2] = #10) then
      begin
        FInPos := vPos + 3;
        Deliver;
        Result := True;
        Exit;
      end;
      Inc(vPos);
    end;
    // up to the end of the line, or of what arrived
    vEnd := vPos;
    while (vEnd <= vLen) and (FIn[vEnd] <> #10) do
      Inc(vEnd);
    if vEnd <= vLen then
    begin
      // only CR LF starts a line: a bare LF does not end the data (SMTP smuggling)
      if vEnd > vPos then
        FAtLineStart := FIn[vEnd - 1] = #13
      else
        FAtLineStart := FLastCR;
      AppendData(FIn[vPos], vEnd - vPos + 1);
      FLastCR := False;
      vPos := vEnd + 1;
    end
    else
    begin
      AppendData(FIn[vPos], vLen - vPos + 1);
      FLastCR := FIn[vLen] = #13;
      FAtLineStart := False;
      vPos := vLen + 1;
    end;
  end;
  if vPos <> FInPos then
    Result := False;
  FInPos := vPos;
end;

function TRALSMTPConnection.ProcessBdat: boolean;
var
  vCount: Int64RAL;
begin
  Result := False;
  vCount := Length(FIn) - FInPos + 1;
  if vCount > FBdatLeft then
    vCount := FBdatLeft;
  if vCount > 0 then
  begin
    AppendData(FIn[FInPos], vCount);
    Inc(FInPos, vCount);
    Dec(FBdatLeft, vCount);
  end;
  if FBdatLeft > 0 then
    Exit;
  Result := True;
  FPhase := sspCommand;
  if FEnvelope.FRecipients.Count = 0 then
  begin
    // the chunk was read: refused without recipients (RFC 3030 3)
    Reply('554 5.5.1 no valid recipients');
    ResetTransaction;
  end
  else if FBdatLast then
    Deliver
  else if FTooBig then
  begin
    Reply('552 5.3.4 message too big');
    ResetTransaction;
  end
  else
    Reply('250 2.0.0 ' + StringRAL(IntToStr(FDataSize)) + ' octets received');
end;

procedure TRALSMTPConnection.Deliver;
var
  vCode: IntegerRAL;
  vReply: StringRAL;
begin
  FPhase := sspCommand;
  Inc(FTransactions);
  if FTooBig then
  begin
    Reply('552 5.3.4 message too big');
    ResetTransaction;
    Exit;
  end;
  vCode := 250;
  vReply := '2.0.0 ok: queued as ' + FSession.FId + '.' + StringRAL(IntToStr(FTransactions));
  if Assigned(FServer.FOnMessage) then
  begin
    FData.Position := 0;
    try
      FServer.FOnMessage(FServer, FSession, FEnvelope, FData, vCode, vReply);
    except
      on E: Exception do
      begin
        vCode := 451;
        vReply := '4.3.0 ' + StringRAL(E.Message);
      end;
    end;
  end;
  Reply(StringRAL(IntToStr(vCode)) + ' ' + vReply);
  ResetTransaction;
end;

procedure TRALSMTPConnection.Command(const ALine: RawByteString);
var
  vVerb, vArgs: RawByteString;
  vSpace: IntegerRAL;
  vParts: TStringList;
begin
  vSpace := Pos(RawByteString(' '), ALine);
  if vSpace > 0 then
  begin
    vVerb := UpperAscii(Copy(ALine, 1, vSpace - 1));
    vArgs := TrimRaw(Copy(ALine, vSpace + 1, MaxInt));
  end
  else
  begin
    vVerb := UpperAscii(TrimRaw(ALine));
    vArgs := '';
  end;

  if vVerb = 'EHLO' then
    DoEhlo(vArgs, True)
  else if vVerb = 'HELO' then
    DoEhlo(vArgs, False)
  else if vVerb = 'NOOP' then
    Reply('250 2.0.0 ok')
  else if vVerb = 'RSET' then
  begin
    ResetTransaction;
    Reply('250 2.0.0 ok');
  end
  else if vVerb = 'QUIT' then
  begin
    Reply('221 2.0.0 ' + FServer.FHostName + ' closing');
    CloseAfterFlush := True;
  end
  else if vVerb = 'STARTTLS' then
  begin
    if FSession.FTLS then
      Reply('503 5.5.1 TLS already active')
    else if FServer.FTLSContext = nil then
      Reply('454 4.7.0 TLS not available')
    else if vArgs <> '' then
      Reply('501 5.5.4 no parameters allowed')
    else
    begin
      Reply('220 2.0.0 ready to start TLS');
      FStartTLS := True;
    end;
  end
  else if vVerb = 'AUTH' then
    DoAuth(vArgs)
  else if vVerb = 'MAIL' then
    DoMail(vArgs)
  else if vVerb = 'RCPT' then
    DoRcpt(vArgs)
  else if vVerb = 'DATA' then
  begin
    if not FMailGiven then
      Reply('503 5.5.1 need MAIL before DATA')
    else if FEnvelope.FRecipients.Count = 0 then
      Reply('554 5.5.1 no valid recipients')
    else if vArgs <> '' then
      Reply('501 5.5.4 no parameters allowed')
    else
    begin
      BeginData;
      FPhase := sspData;
      Reply('354 end data with <CR><LF>.<CR><LF>');
    end;
  end
  else if vVerb = 'BDAT' then
  begin
    vParts := TStringList.Create;
    try
      vParts.Delimiter := ' ';
      vParts.StrictDelimiter := True;
      vParts.DelimitedText := string(vArgs);
      if (vParts.Count < 1) or (vParts.Count > 2) or (StrToInt64Def(vParts[0], -1) < 0) or
         ((vParts.Count = 2) and not SameText(vParts[1], 'LAST')) then
      begin
        // the size is unknown: nothing can be read in step with the client
        Reply('501 5.5.4 BDAT size [LAST]');
        CloseAfterFlush := True;
        Exit;
      end;
      // the first chunk opens the message; without recipients it is read and dropped
      if FData = nil then
      begin
        if FEnvelope.FRecipients.Count > 0 then
          BeginData
        else
          FData := TRALBodyStream.Create(-1, FServer.FMemoryLimit);
      end;
      FBdatLeft := StrToInt64(vParts[0]);
      FBdatLast := vParts.Count = 2;
      FPhase := sspBdat;
    finally
      vParts.Free;
    end;
  end
  else if vVerb = 'VRFY' then
    Reply('252 2.1.5 cannot verify, but will accept the message')
  else if vVerb = 'HELP' then
    Reply('214 2.0.0 see RFC 5321')
  else if vVerb = '' then
    Reply('500 5.5.2 empty command')
  else
  begin
    Inc(FBadCommands);
    if FBadCommands > cMaxBadCommands then
    begin
      Reply('421 4.7.0 ' + FServer.FHostName + ' too many errors');
      CloseAfterFlush := True;
    end
    else
      Reply('502 5.5.2 command not recognized');
  end;
end;

procedure TRALSMTPConnection.DoEhlo(const AName: RawByteString; AExtended: boolean);
var
  vAuth: StringRAL;
  vMethods: TRALSMTPServerAuthMethods;
begin
  if AName = '' then
  begin
    Reply('501 5.5.4 a domain or address is needed');
    Exit;
  end;
  FSession.FHelo := StringRAL(AName);
  ResetTransaction;
  if not AExtended then
  begin
    Reply('250 ' + FServer.FHostName);
    Exit;
  end;
  Reply('250-' + FServer.FHostName + ' hello ' + StringRAL(AName));
  Reply('250-PIPELINING');
  if FServer.FMaxMessageSize > 0 then
    Reply('250-SIZE ' + StringRAL(IntToStr(FServer.FMaxMessageSize)))
  else
    Reply('250-SIZE');
  Reply('250-8BITMIME');
  Reply('250-SMTPUTF8');
  Reply('250-CHUNKING');
  if (FServer.FTLSContext <> nil) and not FSession.FTLS then
    Reply('250-STARTTLS');
  vMethods := FServer.FAuthMethods;
  if not FSession.FTLS and not FServer.FAllowPlainAuthWithoutTLS then
    vMethods := vMethods - [ssaPlain, ssaLogin];
  if FServer.FRequireTLS and not FSession.FTLS then
    vMethods := [];
  if not FSession.FAuthenticated and (vMethods <> []) then
  begin
    vAuth := '';
    if ssaScramSHA256 in vMethods then
      vAuth := vAuth + ' SCRAM-SHA-256';
    if ssaPlain in vMethods then
      vAuth := vAuth + ' PLAIN';
    if ssaLogin in vMethods then
      vAuth := vAuth + ' LOGIN';
    if ssaXOAuth2 in vMethods then
      vAuth := vAuth + ' XOAUTH2';
    if ssaOAuthBearer in vMethods then
      vAuth := vAuth + ' OAUTHBEARER';
    Reply('250-AUTH' + vAuth);
  end;
  Reply('250 ENHANCEDSTATUSCODES');
end;

procedure TRALSMTPConnection.DoMail(const AArgs: RawByteString);
var
  vPath, vParams, vValue: RawByteString;
  vCode: IntegerRAL;
  vReply: StringRAL;
begin
  if FSession.FHelo = '' then
  begin
    Reply('503 5.5.1 EHLO first');
    Exit;
  end;
  if FServer.FRequireTLS and not FSession.FTLS then
  begin
    Reply('530 5.7.0 must issue a STARTTLS command first');
    Exit;
  end;
  if not FSession.FAuthenticated and (FServer.FRequireAuth or
     (FSession.FListener <> RALSMTPListenerRelay)) then
  begin
    Reply('530 5.7.0 authentication required');
    Exit;
  end;
  if FMailGiven then
  begin
    Reply('503 5.5.1 nested MAIL command');
    Exit;
  end;
  if (UpperAscii(Copy(AArgs, 1, 5)) <> 'FROM:') or
     not SplitPath(Copy(AArgs, 6, MaxInt), vPath, vParams) then
  begin
    Reply('501 5.5.4 syntax: MAIL FROM:<address>');
    Exit;
  end;
  vValue := ParamValue(vParams, 'SIZE');
  if (vValue <> '') and (FServer.FMaxMessageSize > 0) and
     (StrToInt64Def(string(vValue), 0) > FServer.FMaxMessageSize) then
  begin
    Reply('552 5.3.4 message size exceeds fixed maximum message size');
    Exit;
  end;
  vCode := 250;
  vReply := '2.1.0 ok';
  if Assigned(FServer.FOnMailFrom) then
    FServer.FOnMailFrom(FServer, FSession, UTF8ToString(vPath), vCode, vReply);
  if vCode >= 400 then
  begin
    Reply(StringRAL(IntToStr(vCode)) + ' ' + vReply);
    Exit;
  end;
  ResetTransaction;
  FEnvelope.FFrom := UTF8ToString(vPath);
  FEnvelope.FBody8Bit := SameText(string(ParamValue(vParams, 'BODY')), '8BITMIME');
  FEnvelope.FUTF8 := ParamValue(vParams, 'SMTPUTF8') <> '';
  // MAIL was given, even for the null reverse path
  FMailGiven := True;
  Reply(StringRAL(IntToStr(vCode)) + ' ' + vReply);
end;

procedure TRALSMTPConnection.DoRcpt(const AArgs: RawByteString);
var
  vPath, vParams: RawByteString;
  vCode: IntegerRAL;
  vReply: StringRAL;
begin
  if not FMailGiven then
  begin
    Reply('503 5.5.1 need MAIL before RCPT');
    Exit;
  end;
  if (UpperAscii(Copy(AArgs, 1, 3)) <> 'TO:') or
     not SplitPath(Copy(AArgs, 4, MaxInt), vPath, vParams) or (vPath = '') then
  begin
    Reply('501 5.5.4 syntax: RCPT TO:<address>');
    Exit;
  end;
  if FEnvelope.FRecipients.Count >= FServer.FMaxRecipients then
  begin
    Reply('452 4.5.3 too many recipients');
    Exit;
  end;
  vCode := 250;
  vReply := '2.1.5 ok';
  if Assigned(FServer.FOnRcptTo) then
    FServer.FOnRcptTo(FServer, FSession, UTF8ToString(vPath), vCode, vReply);
  if vCode < 400 then
    FEnvelope.FRecipients.Add(string(UTF8ToString(vPath)));
  Reply(StringRAL(IntToStr(vCode)) + ' ' + vReply);
end;

procedure TRALSMTPConnection.DoAuth(const AArgs: RawByteString);
var
  vSpace: IntegerRAL;
  vMech, vInitial: RawByteString;
  vMethods: TRALSMTPServerAuthMethods;
begin
  if FSession.FHelo = '' then
  begin
    Reply('503 5.5.1 EHLO first');
    Exit;
  end;
  if FSession.FAuthenticated then
  begin
    Reply('503 5.5.1 already authenticated');
    Exit;
  end;
  if FMailGiven then
  begin
    Reply('503 5.5.1 AUTH not allowed during a mail transaction');
    Exit;
  end;
  vSpace := Pos(RawByteString(' '), AArgs);
  if vSpace > 0 then
  begin
    vMech := UpperAscii(Copy(AArgs, 1, vSpace - 1));
    vInitial := TrimRaw(Copy(AArgs, vSpace + 1, MaxInt));
  end
  else
  begin
    vMech := UpperAscii(AArgs);
    vInitial := '';
  end;
  vMethods := FServer.FAuthMethods;
  if vMech = 'PLAIN' then
    FAuthMethod := ssaPlain
  else if vMech = 'LOGIN' then
    FAuthMethod := ssaLogin
  else if vMech = 'SCRAM-SHA-256' then
    FAuthMethod := ssaScramSHA256
  else if vMech = 'XOAUTH2' then
    FAuthMethod := ssaXOAuth2
  else if vMech = 'OAUTHBEARER' then
    FAuthMethod := ssaOAuthBearer
  else
  begin
    Reply('504 5.5.4 unrecognized authentication type');
    Exit;
  end;
  if not (FAuthMethod in vMethods) then
  begin
    Reply('504 5.5.4 unrecognized authentication type');
    Exit;
  end;
  if (FServer.FRequireTLS or ((FAuthMethod in [ssaPlain, ssaLogin]) and
     not FServer.FAllowPlainAuthWithoutTLS)) and not FSession.FTLS then
  begin
    Reply('538 5.7.11 encryption required for requested authentication mechanism');
    Exit;
  end;
  FPhase := sspAuth;
  FAuthStep := 0;
  FreeAndNil(FSCRAM);
  if FAuthMethod = ssaScramSHA256 then
    FSCRAM := TRALSCRAMServer.Create(rsaSHA256, FServer.FindSCRAMUser);
  if vInitial = '=' then
    AuthLine('')
  else if vInitial <> '' then
    AuthLine(vInitial)
  else if FAuthMethod = ssaLogin then
  begin
    FAuthStep := 1;
    Reply('334 VXNlcm5hbWU6');
  end
  else
    Reply('334 ');
end;

procedure TRALSMTPConnection.AuthFinish(AOk: boolean; const AUser: StringRAL;
  ATempFail: boolean);
begin
  FPhase := sspCommand;
  FreeAndNil(FSCRAM);
  FAuthStep := 0;
  if AOk then
  begin
    FSession.FAuthenticated := True;
    FSession.FUserName := AUser;
    Reply('235 2.7.0 authentication successful');
  end
  else if ATempFail then
    Reply('454 4.7.0 temporary authentication failure')
  else
    Reply('535 5.7.8 authentication credentials invalid');
end;

procedure TRALSMTPConnection.AuthLine(const ALine: RawByteString);
var
  vText, vUser, vPass, vToken, vPart: StringRAL;
  vParts: TStringList;
  vInt: IntegerRAL;
  vTemp: boolean;
begin
  if ALine = '*' then
  begin
    // the client gave up (RFC 4954 4)
    FPhase := sspCommand;
    FreeAndNil(FSCRAM);
    Reply('501 5.7.0 authentication canceled');
    Exit;
  end;
  vText := '';
  if ALine <> '' then
    try
      vText := TRALBase64.Decode(StringRAL(ALine));
    except
      FPhase := sspCommand;
      Reply('501 5.5.2 cannot decode response');
      Exit;
    end;
  vTemp := False;
  case FAuthMethod of
    ssaPlain:
    begin
      // authzid NUL authcid NUL password
      vInt := RALMailPos(#0, vText);
      vText := Copy(vText, vInt + 1, MaxInt);
      vInt := RALMailPos(#0, vText);
      if vInt = 0 then
      begin
        AuthFinish(False, '');
        Exit;
      end;
      vUser := Copy(vText, 1, vInt - 1);
      vPass := Copy(vText, vInt + 1, MaxInt);
      AuthFinish(FServer.CheckPassword(FSession, vUser, vPass, vTemp), vUser, vTemp);
    end;
    ssaLogin:
    begin
      if FAuthStep = 0 then
      begin
        // AUTH LOGIN with the user name already given
        FAuthStep := 1;
      end;
      if FAuthStep = 1 then
      begin
        FAuthUser := vText;
        FAuthStep := 2;
        Reply('334 UGFzc3dvcmQ6');
      end
      else
      begin
        vUser := FAuthUser;
        FAuthUser := '';
        AuthFinish(FServer.CheckPassword(FSession, vUser, vText, vTemp), vUser, vTemp);
      end;
    end;
    ssaScramSHA256:
    begin
      if FSCRAM.Done then
      begin
        // the empty line after the server-final message
        vUser := FSCRAM.UserName;
        if FSCRAM.Accepted and (FServer.CheckAccess(FSession, '', True) < 400) then
          AuthFinish(True, vUser)
        else
          AuthFinish(False, vUser);
        Exit;
      end;
      try
        vToken := FSCRAM.Step(vText);
      except
        AuthFinish(False, '');
        Exit;
      end;
      if FSCRAM.Done and not FSCRAM.Accepted then
        AuthFinish(False, FSCRAM.UserName)
      else
        Reply('334 ' + TRALBase64.Encode(vToken));
    end;
    ssaXOAuth2, ssaOAuthBearer:
    begin
      if FAuthStep = 9 then
      begin
        // the client's empty answer to the error challenge
        AuthFinish(False, '');
        Exit;
      end;
      vUser := '';
      vToken := '';
      vParts := TStringList.Create;
      try
        vParts.Text := StringReplace(string(vText), #1, #13#10, [rfReplaceAll]);
        for vInt := 0 to vParts.Count - 1 do
        begin
          vPart := StringRAL(vParts[vInt]);
          if Copy(vPart, 1, 5) = 'user=' then
            vUser := Copy(vPart, 6, MaxInt)
          else if (Copy(vPart, 1, 2) = 'n,') and (RALMailPos('a=', vPart) > 0) then
            vUser := Copy(vPart, RALMailPos('a=', vPart) + 2, MaxInt)
          else if SameText(Copy(vPart, 1, 12), 'auth=Bearer ') then
            vToken := Copy(vPart, 13, MaxInt);
        end;
      finally
        vParts.Free;
      end;
      if (vUser <> '') and (vUser[Length(vUser)] = ',') then
        SetLength(vUser, Length(vUser) - 1);
      if (vToken <> '') and FServer.CheckToken(FSession, vToken, vTemp) then
        AuthFinish(True, vUser)
      else if vTemp then
        AuthFinish(False, vUser, True)
      else
      begin
        // the error goes in a challenge; the client answers empty, then 535
        FAuthStep := 9;
        Reply('334 ' + TRALBase64.Encode('{"status":"invalid_token","schemes":"bearer"}'));
      end;
    end;
  end;
end;

{ TRALSMTPServer }

constructor TRALSMTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Port := 25;
  SetEngine('SMTP ' + StringRAL(RALVERSION));
  FAuthMethods := [ssaPlain, ssaLogin, ssaScramSHA256];
  FCommandTimeout := 300000;
  FMaxConnections := 1000;
  FMaxMessageSize := 26214400;
  FMaxRecipients := 100;
  FMemoryLimit := 4194304;
  FSubmissionPort := 587;
  FTLSPort := 465;
  FUsers := TStringList.Create;
  FLock := TCriticalSection.Create;
  FListeners := TList.Create;
  FRetiredContexts := TList.Create;
  { the internal route every AUTH goes through: the plugins and the
    authenticators of the server see it as a request }
  FAuthModule := TRALSMTPAuthModule.Create(nil);
  FAuthModule.CreateRoute(cAuthRoute, AuthRoute);
  FAuthModule.Server := Self;
end;

destructor TRALSMTPServer.Destroy;
var
  vInt: IntegerRAL;
begin
  if FRunning then
    StopAll;
  FAuthModule.Server := nil;
  FreeAndNil(FAuthModule);
  for vInt := 0 to FRetiredContexts.Count - 1 do
    TObject(FRetiredContexts[vInt]).Free;
  FreeAndNil(FRetiredContexts);
  FreeAndNil(FTLSContext);
  FreeAndNil(FListeners);
  FreeAndNil(FLock);
  FreeAndNil(FUsers);
  inherited Destroy;
end;

function TRALSMTPServer.CreateRALSSL: TRALSSL;
begin
  Result := TRALSMTPServerSSL.Create;
end;

function TRALSMTPServer.GetSSL: TRALSMTPServerSSL;
begin
  Result := TRALSMTPServerSSL(GetDefaultSSL);
end;

procedure TRALSMTPServer.SetSSL(AValue: TRALSMTPServerSSL);
begin
  GetDefaultSSL.Assign(AValue);
end;

procedure TRALSMTPServer.SetUsers(AValue: TStrings);
begin
  FUsers.Assign(AValue);
end;

function TRALSMTPServer.IPv6IsImplemented: boolean;
begin
  Result := True;
end;

function TRALSMTPServer.IsPropertyRelevant(const AName: StringRAL): boolean;
begin
  Result := not (RALSameName(AName, 'Routes') or RALSameName(AName, 'ResponsePages') or
    RALSameName(AName, 'CookieLife') or RALSameName(AName, 'ServerStatus') or
    RALSameName(AName, 'ShowServerStatus') or RALSameName(AName, 'SessionTimeout') or
    RALSameName(AName, 'OnRequest') or RALSameName(AName, 'OnResponse') or
    RALSameName(AName, 'StreamKeepAlive') or RALSameName(AName, 'StreamMaxPending') or
    RALSameName(AName, 'StreamSendTimeout') or RALSameName(AName, 'OnStreamClosed'));
  if Result then
    Result := inherited IsPropertyRelevant(AName);
end;

function TRALSMTPServer.TLSProvisioning: TRALTLSProvisioning;
begin
  Result := tpPEMFiles;
end;

function TRALSMTPServer.GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean;
begin
  RALClearTLSCertificate(ACertificate);
  ACertificate.CertificateFile := SSL.CertFile;
  ACertificate.PrivateKeyFile := SSL.KeyFile;
  ACertificate.PrivateKeyPassword := SSL.KeyPassword;
  ACertificate.CertificatePEM := SSL.CertPEM;
  ACertificate.PrivateKeyPEM := SSL.KeyPEM;
  Result := (SSL.CertFile <> '') or (SSL.CertPEM <> '');
end;

function TRALSMTPServer.SetTLSCertificate(
  const ACertificate: TRALTLSCertificate): TRALTLSApplyResult;
var
  vNew: TRALTLSContext;
begin
  SSL.CertFile := ACertificate.CertificateFile;
  SSL.KeyFile := ACertificate.PrivateKeyFile;
  SSL.KeyPassword := ACertificate.PrivateKeyPassword;
  SSL.CertPEM := ACertificate.CertificatePEM;
  SSL.KeyPEM := ACertificate.PrivateKeyPEM;
  SSL.Enabled := True;
  Result := tarApplied;
  if not FRunning then
    Exit;
  if FTLSContext = nil then
  begin
    // the listeners opened without TLS
    Result := tarRestartNeeded;
    Exit;
  end;
  { a context for the connections to come; the replaced one serves the open ones
    until the server stops }
  vNew := NewTLSContext;
  FLock.Acquire;
  try
    FRetiredContexts.Add(FTLSContext);
    FTLSContext := vNew;
  finally
    FLock.Release;
  end;
end;

function TRALSMTPServer.NewTLSContext: TRALTLSContext;
begin
  Result := TRALTLSContext.Create(mtrServer);
  try
    Result.SetMinVersion(SSL.MinVersion);
    if SSL.CertPEM <> '' then
      Result.UseCertificatePEM(SSL.CertPEM, SSL.KeyPEM)
    else
      Result.UseCertificateFiles(SSL.CertFile, SSL.KeyFile, SSL.KeyPassword);
  except
    Result.Free;
    raise;
  end;
end;

function TRALSMTPServer.NewId: StringRAL;
begin
  Result := StringRAL(IntToHex(FStarted and $FFFFFFFF, 8) + IntToHex(RALAtomicInc(FNextId, 1), 6));
end;

function TRALSMTPServer.ConnectionCount: IntegerRAL;
begin
  Result := 0;
  if FLoop <> nil then
    Result := FLoop.ConnectionCount;
end;

function TRALSMTPServer.TestPorts(out AReport: StringRAL): boolean;
var
  vLines: TStringList;
  vContext: TRALTLSContext;

  procedure Try_(APort: IntegerRAL; const AKind: string);
  var
    vSocket: TRALSocket;
    vFamily: TRALIPFamily;
    vAddress: StringRAL;
  begin
    if APort <= 0 then
      Exit;
    if IPConfig.IPv6Enabled then
    begin
      vFamily := mifAny;
      vAddress := IPConfig.IPv6Bind;
      if RALSameName(vAddress, '::') then
        vAddress := '';
    end
    else
    begin
      vFamily := mifIPv4;
      vAddress := IPConfig.IPv4Bind;
      if vAddress = '0.0.0.0' then
        vAddress := '';
    end;
    vSocket := TRALSocket.Create;
    try
      try
        vSocket.Listen(vAddress, APort, vFamily, 1);
        vLines.Add(Format(cmMailPortFree, [APort, AKind]));
      except
        on E: Exception do
        begin
          vLines.Add(Format(cmMailPortBusy, [APort, AKind, E.Message]));
          Result := False;
        end;
      end;
    finally
      vSocket.Free;
    end;
  end;

begin
  Result := True;
  vLines := TStringList.Create;
  try
    if Active then
      vLines.Add(cmMailServerRunning)
    else
    begin
      Try_(Port, 'SMTP');
      Try_(FSubmissionPort, 'submission');
      if SSL.Enabled then
      begin
        Try_(FTLSPort, 'SMTPS');
        try
          vContext := NewTLSContext;
          vContext.Free;
          vLines.Add(cmMailCertificateOk);
        except
          on E: Exception do
          begin
            vLines.Add(Format(cmMailCertificateFailed, [E.Message]));
            Result := False;
          end;
        end;
      end;
    end;
    AReport := StringRAL(Trim(vLines.Text));
  finally
    vLines.Free;
  end;
end;

procedure TRALSMTPServer.SetActive(const AValue: boolean);
begin
  if AValue = Active then
    Exit;
  if csDesigning in ComponentState then
  begin
    inherited SetActive(AValue);
    Exit;
  end;
  if AValue then
  begin
    // plugins and modules first: RALSelfSigned sets the certificate here
    inherited SetActive(True);
    try
      StartListeners;
    except
      StopAll;
      inherited SetActive(False);
      raise;
    end;
  end
  else
  begin
    inherited SetActive(False);
    StopAll;
  end;
end;

procedure TRALSMTPServer.StartListeners;
var
  vThreads, vInt: IntegerRAL;

  procedure Open(APort, AKind: IntegerRAL);
  var
    vListener: TRALSMTPListener;
    vFamily: TRALIPFamily;
    vAddress: StringRAL;
  begin
    if APort <= 0 then
      Exit;
    vListener := TRALSMTPListener.Create;
    try
      vListener.Kind := AKind;
      vListener.Socket := TRALSocket.Create;
      if IPConfig.IPv6Enabled then
      begin
        vFamily := mifAny;
        vAddress := IPConfig.IPv6Bind;
        if RALSameName(vAddress, '::') then
          vAddress := '';
      end
      else
      begin
        vFamily := mifIPv4;
        vAddress := IPConfig.IPv4Bind;
        if vAddress = '0.0.0.0' then
          vAddress := '';
      end;
      vListener.Socket.Listen(vAddress, APort, vFamily);
      FListeners.Add(vListener);
    except
      vListener.Free;
      raise;
    end;
  end;

begin
  FStarted := RALSocketNow;
  if FHostName = '' then
    FHostName := 'localhost';
  if SSL.Enabled then
    FTLSContext := NewTLSContext;
  Open(Port, RALSMTPListenerRelay);
  Open(FSubmissionPort, RALSMTPListenerSubmission);
  if FTLSContext <> nil then
    Open(FTLSPort, RALSMTPListenerTLS);
  vThreads := FThreadCount;
  if vThreads <= 0 then
    vThreads := RALCPUCount;
  FLoop := TRALSocketLoop.Create;
  FLoop.OnAccept := Accepted;
  for vInt := 0 to FListeners.Count - 1 do
    FLoop.AddListener(TRALSMTPListener(FListeners[vInt]).Socket,
      TRALSMTPListener(FListeners[vInt]).Kind);
  FLoop.StartThreads(vThreads);
  FRunning := True;
  FLoop.StartAccepting;
end;

procedure TRALSMTPServer.StopAll;
var
  vInt: IntegerRAL;
begin
  if FLoop <> nil then
  begin
    FLoop.StopAccepting;
    FLoop.StopThreads;
    FreeAndNil(FLoop);
  end;
  for vInt := 0 to FListeners.Count - 1 do
    TObject(FListeners[vInt]).Free;
  FListeners.Clear;
  FRunning := False;
  for vInt := 0 to FRetiredContexts.Count - 1 do
    TObject(FRetiredContexts[vInt]).Free;
  FRetiredContexts.Clear;
  FreeAndNil(FTLSContext);
end;

procedure TRALSMTPServer.Accepted(ASocket: TRALSocket; AListener: IntegerRAL);
var
  vConn: TRALSMTPConnection;
  vBusy: RawByteString;
begin
  if ConnectionCount >= FMaxConnections then
  begin
    // over the limit: told so, on the accepting thread
    try
      if AListener <> RALSMTPListenerTLS then
      begin
        vBusy := RawByteString('421 4.3.2 ' + FHostName + ' too many connections, try later'#13#10);
        ASocket.Send(vBusy[1], Length(vBusy));
      end;
    except
      // the client is gone already
    end;
    ASocket.Free;
    Exit;
  end;
  try
    FLock.Acquire;
    try
      vConn := TRALSMTPConnection.Create(Self, ASocket, AListener);
    finally
      FLock.Release;
    end;
  except
    ASocket.Free;
    Exit;
  end;
  FLoop.Adopt(vConn);
end;

procedure TRALSMTPServer.AuthRoute(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  // reached only when every plugin and authenticator let the AUTH through
  AResponse.Answer(HTTP_OK, 'ok', 'text/plain');
end;

function TRALSMTPServer.CheckAccess(ASession: TRALSMTPSession; const AAuth: StringRAL;
  AValidateOnly: boolean): IntegerRAL;
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
begin
  vRequest := CreateRequest;
  vResponse := CreateResponse;
  try
    vRequest.Method := amPOST;
    vRequest.Query := '/' + cAuthRoute;
    vRequest.ClientInfo.IP := ASession.FRemoteIP;
    vRequest.ClientInfo.Port := ASession.FRemotePort;
    vRequest.ClientInfo.UserAgent := 'SMTP';
    if AAuth <> '' then
    begin
      vRequest.Params.AddParam('Authorization', AAuth, rpkHEADER);
      DecodeAuthValue(vRequest, AAuth);
    end;
    ValidateRequest(vRequest, vResponse);
    if (vResponse.StatusCode < HTTP_BadRequest) and not AValidateOnly then
      ProcessCommands(vRequest, vResponse);
    Result := vResponse.StatusCode;
  finally
    vRequest.Free;
    vResponse.Free;
  end;
end;

function TRALSMTPServer.CheckPassword(ASession: TRALSMTPSession; const AUser,
  APassword: StringRAL; out ATempFail: boolean): boolean;
var
  vStatus, vIndex: IntegerRAL;
  vAuth: StringRAL;
begin
  ATempFail := False;
  if FPasswordIsToken then
    vAuth := 'Bearer ' + APassword
  else
    vAuth := 'Basic ' + TRALBase64.Encode(AUser + ':' + APassword);
  vStatus := CheckAccess(ASession, vAuth, False);
  if vStatus >= 400 then
  begin
    ATempFail := (vStatus = 429) or (vStatus = HTTP_ServiceUnavailable);
    Result := False;
    Exit;
  end;
  if HasAuthentication then
  begin
    Result := True;
    Exit;
  end;
  // with no authenticator: Users
  vIndex := FUsers.IndexOfName(string(AUser));
  Result := (vIndex >= 0) and RALSameSecret(StringRAL(FUsers.ValueFromIndex[vIndex]), APassword);
end;

function TRALSMTPServer.CheckToken(ASession: TRALSMTPSession; const AToken: StringRAL;
  out ATempFail: boolean): boolean;
var
  vStatus: IntegerRAL;
begin
  ATempFail := False;
  // a token needs an authenticator that understands it
  if not HasAuthentication then
  begin
    Result := False;
    Exit;
  end;
  vStatus := CheckAccess(ASession, 'Bearer ' + AToken, False);
  ATempFail := (vStatus = 429) or (vStatus = HTTP_ServiceUnavailable);
  Result := vStatus < 400;
end;

function TRALSMTPServer.FindSCRAMUser(const AUserName: StringRAL;
  out ACredential: TRALSCRAMCredential): boolean;
var
  vIndex: IntegerRAL;
begin
  ACredential.Password := '';
  ACredential.Iterations := 0;
  SetLength(ACredential.Salt, 0);
  SetLength(ACredential.StoredKey, 0);
  SetLength(ACredential.ServerKey, 0);
  Result := False;
  if Assigned(FOnSCRAMCredential) then
    FOnSCRAMCredential(Self, AUserName, ACredential, Result);
  if Result then
    Exit;
  vIndex := FUsers.IndexOfName(string(AUserName));
  if vIndex >= 0 then
  begin
    ACredential.Password := StringRAL(FUsers.ValueFromIndex[vIndex]);
    Result := True;
  end;
end;

end.
