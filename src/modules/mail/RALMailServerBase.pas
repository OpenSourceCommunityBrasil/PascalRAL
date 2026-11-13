/// What the POP3 and IMAP servers share: ports, TLS, login and the connection loop.
unit RALMailServerBase;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALBase64, RALExceptions, RALServer, RALRequest, RALResponse,
  RALRoutes, RALCustomObjects, RALSocket, RALSocketLoop, RALTLS, RALSCRAM,
  RALMailStore, RALMailMIME;

const
  /// The listener of the plain port (with STARTTLS or STLS).
  RALMailListenerPlain = 0;
  /// The listener of the implicit TLS port.
  RALMailListenerTLS = 1;

type
  TRALMailServer = class;

  /// A SASL mechanism the server offers.
  TRALMailServerAuthMethod = (
    /// PLAIN (RFC 4616), only over TLS.
    msaPlain,
    /// SCRAM-SHA-256 (RFC 7677), with the passwords of Users or OnSCRAMCredential.
    msaScramSHA256
  );
  /// The mechanisms the server offers.
  TRALMailServerAuthMethods = set of TRALMailServerAuthMethod;

  /// The result of one step of a SASL exchange.
  TRALMailSASLResult = (
    /// The mechanism wants another response.
    msrContinue,
    /// The user is authenticated.
    msrOk,
    /// The credentials are wrong.
    msrFailed,
    /// The login could not be judged now (flood limit, plugin busy).
    msrTemporary
  );

  { The SCRAM credential of AUserName, for SCRAM-SHA-256 without Users: fill
    ACredential (the password, or the salted keys) and set AFound. }
  TRALMailSCRAMEvent = procedure(Sender: TObject; const AUserName: StringRAL;
    var ACredential: TRALSCRAMCredential; var AFound: boolean) of object;

  /// TLS of the TLS port and of STARTTLS or STLS.
  TRALMailServerSSL = class(TRALSSL)
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
  TRALMailSession = class
  private
    FAuthenticated: boolean;
    FId: StringRAL;
    FListener: IntegerRAL;
    FRemoteIP: StringRAL;
    FRemotePort: IntegerRAL;
    FTag: TObject;
    FTLS: boolean;
    FUserName: StringRAL;
  public
    /// Whether the login succeeded.
    property Authenticated: boolean read FAuthenticated;
    /// A unique identifier of the connection.
    property Id: StringRAL read FId;
    /// RALMailListenerPlain or RALMailListenerTLS.
    property Listener: IntegerRAL read FListener;
    /// The client's address.
    property RemoteIP: StringRAL read FRemoteIP;
    /// The client's port.
    property RemotePort: IntegerRAL read FRemotePort;
    /// An object of the application for this connection (not freed by the server).
    property Tag: TObject read FTag write FTag;
    /// Whether the connection is encrypted.
    property TLS: boolean read FTLS;
    /// The user the login accepted.
    property UserName: StringRAL read FUserName;
  end;

  { The SASL exchange of one login: PLAIN or SCRAM-SHA-256. The wire layer
    decodes the base64 of each response and encodes the challenge Step
    answers. }
  TRALMailSASL = class
  private
    /// The mechanism.
    FMethod: TRALMailServerAuthMethod;
    /// The SCRAM exchange.
    FSCRAM: TRALSCRAMServer;
    /// The server that owns the connection.
    FServer: TRALMailServer;
    /// The session the exchange belongs to.
    FSession: TRALMailSession;
    /// The steps taken.
    FStage: IntegerRAL;
    FUserName: StringRAL;
  public
    /// An exchange of AMethod for ASession.
    constructor Create(AServer: TRALMailServer; ASession: TRALMailSession;
      AMethod: TRALMailServerAuthMethod);
    destructor Destroy; override;

    { Takes the client's response (AHasInput False for the first step without an
      initial response) and gives the challenge to send in AOutput. }
    function Step(const AInput: StringRAL; AHasInput: boolean;
      out AOutput: StringRAL): TRALMailSASLResult;

    /// The user the exchange accepted.
    property UserName: StringRAL read FUserName;
  end;

  { A connection of a mail server on the socket loop: reads 16 KB blocks and hands
    them to ProcessInput, one block at a time; queues the replies; switches to TLS
    after STARTTLS or STLS. One poll thread owns it, and every method below runs on
    that thread. }
  TRALMailConnection = class(TRALSocketLoopConnection)
  private
    /// The greeting was sent.
    FGreeted: boolean;
    /// The TLS handshake is not over.
    FHandshaking: boolean;
    /// When the connection last moved (RALSocketNow).
    FLastActive: Int64RAL;
    /// The replies not yet sent.
    FOut: RawByteString;
    /// The next byte of FOut to send.
    FOutPos: IntegerRAL;
    /// The socket channel under TLS.
    FPlain: TRALChannel;
    FServer: TRALMailServer;
    FSession: TRALMailSession;
    /// True when the TLS handshake starts once the reply is sent.
    FStartTLS: boolean;
    /// The TLS channel, once STARTTLS or implicit TLS is on.
    FTLS: TRALTLSChannel;

    /// Starts the TLS handshake on the connection.
    procedure SwitchToTLS;
  protected
    /// The received bytes not yet processed.
    FIn: RawByteString;
    /// The next byte of FIn to process.
    FInPos: IntegerRAL;

    /// Starts the TLS handshake once what was queued went out.
    procedure BeginTLS;
    procedure Closed; override;
    { The text sent when the connection stops working (an exception in the
      protocol code); empty sends nothing. }
    function FaultReply(const AMessage: StringRAL): RawByteString; virtual;
    /// The text sent first (after the handshake on the TLS port); empty sends nothing.
    function Greeting: RawByteString; virtual;
    /// Bytes queued and not yet sent.
    function PendingOutput: IntegerRAL;
    { Serves every whole command in FIn from FInPos on, taking what it uses
      (compact FIn, FInPos = 1); it may leave bytes when it waits for something
      else. }
    procedure ProcessInput; virtual; abstract;
    { The text of a connection the access rules refused; the connection closes
      after it. }
    function RefusedReply(ATooMany: boolean): RawByteString; virtual;
    /// Whether the protocol waits for the client, so silence is no timeout (IMAP IDLE).
    function Resting: boolean; virtual;
    /// Queues bytes for sending.
    procedure Send(const AText: RawByteString);
    /// Marks the session as logged in as AUser.
    procedure SetLoggedIn(const AUser: StringRAL);
    /// The text sent when the server stops; empty sends nothing.
    function StopReply: RawByteString; virtual;
    { Called on every pass of the poll thread, after the input: the protocol's
      own deadlines and notifications. }
    procedure Tick(ANow: Int64RAL); virtual;
    /// The connection is encrypted now: the protocol starts over.
    procedure TLSStarted; virtual;
  public
    /// Creates the connection of ASocket, accepted on listener AListener.
    constructor Create(AServer: TRALMailServer; ASocket: TRALSocket; AListener: IntegerRAL);
    destructor Destroy; override;

    function FlushOutput: boolean; override;
    procedure HandleError(AError: Exception); override;
    function HasOutput: boolean; override;
    function PollEvents: Word; override;
    procedure Serve(ANow: Int64RAL); override;
    procedure Stopping; override;

    /// The server that owns the connection.
    property Server: TRALMailServer read FServer;
    /// What the events see of the connection.
    property Session: TRALMailSession read FSession;
    /// True from the STARTTLS or STLS reply until the handshake starts.
    property TLSPending: boolean read FStartTLS;
  end;

  { The base of the servers that serve the mailboxes of a TRALMailStore (POP3,
    IMAP) on RAL's socket event loop. The login goes through the server's
    plugins as a request to an internal route (authenticators, address lists,
    brute-force and flood plugins apply); with no authenticator Users holds the
    passwords. }
  TRALMailServer = class(TRALServer)
  private
    FAllowPlainAuthWithoutTLS: boolean;
    FAuthMethods: TRALMailServerAuthMethods;
    /// The internal module that carries the login route.
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
    /// The counter of connection ids.
    FNextId: Int64RAL;
    FOnSCRAMCredential: TRALMailSCRAMEvent;
    FPasswordIsToken: boolean;
    FRequireTLS: boolean;
    /// Replaced TLS contexts, kept until the server stops.
    FRetiredContexts: TList;
    /// True while the server listens.
    FRunning: boolean;
    /// When the server started (RALSocketNow).
    FStarted: Int64RAL;
    FStore: TRALMailStore;
    FThreadCount: IntegerRAL;
    /// The TLS context of the listeners and STARTTLS.
    FTLSContext: TRALTLSContext;
    FTLSPort: IntegerRAL;
    FUsers: TStrings;

    /// Takes a connection accepted on listener AListener.
    procedure Accepted(ASocket: TRALSocket; AListener: IntegerRAL);
    /// The internal route that asks the plugins about a login.
    procedure AuthRoute(ARequest: TRALRequest; AResponse: TRALResponse);
    function GetSSL: TRALMailServerSSL;
    /// A new session id.
    function NewId: StringRAL;
    /// A TLS context built from the SSL settings.
    function NewTLSContext: TRALTLSContext;
    procedure SetSSL(AValue: TRALMailServerSSL);
    procedure SetStore(AValue: TRALMailStore);
    procedure SetUsers(AValue: TStrings);
    /// Opens the ports.
    procedure StartListeners;
    /// Closes the ports and the connections.
    procedure StopAll;
  protected
    /// The text sent to a client over MaxConnections; empty sends nothing.
    function BusyReply: RawByteString; virtual;
    { The connection of ASocket accepted on AListener (RALMailListenerPlain or
      RALMailListenerTLS). }
    function CreateConnection(ASocket: TRALSocket; AListener: IntegerRAL): TRALMailConnection; virtual; abstract;
    function CreateRALSSL: TRALSSL; override;
    function IPv6IsImplemented: boolean; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    /// The name of the protocol in the port report ('POP3').
    function ProtocolName: string; virtual; abstract;
    /// The name of the secure port in the port report ('POP3S').
    function SecureProtocolName: string; virtual; abstract;
    procedure SetActive(const AValue: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { The plugins' verdict on a connection (AAuth empty) or on credentials: an
      HTTP status. }
    function CheckAccess(ASession: TRALMailSession; const AAuth: StringRAL;
      AValidateOnly: boolean): IntegerRAL;
    { Checks a user and password with the plugins, then Users; ATempFail is set
      when the login could not be judged now. }
    function CheckPassword(ASession: TRALMailSession; const AUser, APassword: StringRAL;
      out ATempFail: boolean): boolean;
    /// Open connections.
    function ConnectionCount: IntegerRAL;
    /// Reads the SCRAM credential of AUserName, from OnSCRAMCredential or Users.
    function FindSCRAMUser(const AUserName: StringRAL;
      out ACredential: TRALSCRAMCredential): boolean;
    function GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean; override;
    /// Hides the HTTP properties that mean nothing to a mail server.
    function IsPropertyRelevant(const AName: StringRAL): boolean; override;
    function SetTLSCertificate(const ACertificate: TRALTLSCertificate): TRALTLSApplyResult; override;
    { Whether the ports open and the certificate loads, without starting; AReport
      says it port by port (the verb "Test ports" of the IDE). }
    function TestPorts(out AReport: StringRAL): boolean;
    function TLSProvisioning: TRALTLSProvisioning; override;

    /// The implicit TLS port, opened when SSL is enabled; 0 closes it.
    property TLSPort: IntegerRAL read FTLSPort write FTLSPort;
  published
    { Offers PLAIN, USER/PASS and LOGIN on a connection without TLS: the password
      then crosses the network in clear. Only for tests. }
    property AllowPlainAuthWithoutTLS: boolean read FAllowPlainAuthWithoutTLS
      write FAllowPlainAuthWithoutTLS default False;
    /// The SASL mechanisms offered.
    property AuthMethods: TRALMailServerAuthMethods read FAuthMethods write FAuthMethods
      default [msaPlain, msaScramSHA256];
    /// Text after the protocol name in the greeting.
    property Banner: StringRAL read FBanner write FBanner;
    { Milliseconds a connection may stay silent (IMAP IDLE excepted); the RFCs ask
      for at least 10 minutes (POP3) and 30 minutes (IMAP). }
    property CommandTimeout: IntegerRAL read FCommandTimeout write FCommandTimeout default 1800000;
    /// The name in the greeting (its DNS name); empty is 'localhost'.
    property HostName: StringRAL read FHostName write FHostName;
    /// Connections served at once; the next ones are told so and closed.
    property MaxConnections: IntegerRAL read FMaxConnections write FMaxConnections default 1000;
    /// Asks for the SCRAM credential of a user.
    property OnSCRAMCredential: TRALMailSCRAMEvent read FOnSCRAMCredential write FOnSCRAMCredential;
    /// The password is a token (JWT): it goes to the authenticators as a Bearer.
    property PasswordIsToken: boolean read FPasswordIsToken write FPasswordIsToken default False;
    /// A login needs TLS (STARTTLS or STLS) on every port.
    property RequireTLS: boolean read FRequireTLS write FRequireTLS default False;
    /// TLS of the TLS port and STARTTLS or STLS.
    property SSL: TRALMailServerSSL read GetSSL write SetSSL;
    /// The mailboxes the server serves.
    property Store: TRALMailStore read FStore write SetStore;
    /// Poll threads; 0 = one per processor.
    property ThreadCount: IntegerRAL read FThreadCount write FThreadCount default 0;
    { name=password pairs the login checks when no authenticator plugin is linked;
      SCRAM-SHA-256 uses them too. }
    property Users: TStrings read FUsers write SetUsers;
  end;

/// AText with a-z turned into A-Z.
function RALMailUpper(const AText: RawByteString): RawByteString;
/// AText with A-Z turned into a-z.
function RALMailLower(const AText: RawByteString): RawByteString;
/// AText without the blanks and control characters at both ends.
function RALMailTrim(const AText: RawByteString): RawByteString;

implementation

const
  /// The internal route the login goes through.
  cAuthRoute = 'mail/auth';
  /// The unprocessed input after which the connection stops reading, in bytes.
  cMaxInput = 262144;
  /// The queued output after which the connection stops reading, in bytes.
  cMaxOutput = 1048576;

type
  /// One open port.
  TRALMailListener = class
  public
    /// Which port it is: RALMailListenerPlain or RALMailListenerTLS.
    Kind: IntegerRAL;
    /// The listening socket.
    Socket: TRALSocket;

    destructor Destroy; override;
  end;

  /// The module that carries the login route.
  TRALMailAuthModule = class(TRALModuleRoutes);

function RALMailUpper(const AText: RawByteString): RawByteString;
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

function RALMailLower(const AText: RawByteString): RawByteString;
var
  vInt: IntegerRAL;
begin
  SetLength(Result, Length(AText));
  for vInt := 1 to Length(AText) do
    if AText[vInt] in ['A'..'Z'] then
      Result[vInt] := AnsiChar(Ord(AText[vInt]) + 32)
    else
      Result[vInt] := AText[vInt];
end;

function RALMailTrim(const AText: RawByteString): RawByteString;
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

destructor TRALMailListener.Destroy;
begin
  FreeAndNil(Socket);
  inherited Destroy;
end;

{ TRALMailServerSSL }

constructor TRALMailServerSSL.Create;
begin
  inherited Create;
  FMinVersion := mtv12;
end;

procedure TRALMailServerSSL.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TRALMailServerSSL then
  begin
    TRALMailServerSSL(Dest).FCertFile := FCertFile;
    TRALMailServerSSL(Dest).FCertPEM := FCertPEM;
    TRALMailServerSSL(Dest).FKeyFile := FKeyFile;
    TRALMailServerSSL(Dest).FKeyPassword := FKeyPassword;
    TRALMailServerSSL(Dest).FKeyPEM := FKeyPEM;
    TRALMailServerSSL(Dest).FMinVersion := FMinVersion;
  end;
end;

{ TRALMailSASL }

constructor TRALMailSASL.Create(AServer: TRALMailServer; ASession: TRALMailSession;
  AMethod: TRALMailServerAuthMethod);
begin
  inherited Create;
  FServer := AServer;
  FSession := ASession;
  FMethod := AMethod;
  if AMethod = msaScramSHA256 then
    FSCRAM := TRALSCRAMServer.Create(rsaSHA256, AServer.FindSCRAMUser);
end;

destructor TRALMailSASL.Destroy;
begin
  FreeAndNil(FSCRAM);
  inherited Destroy;
end;

function TRALMailSASL.Step(const AInput: StringRAL; AHasInput: boolean;
  out AOutput: StringRAL): TRALMailSASLResult;
var
  vText, vUser, vPass: StringRAL;
  vInt: IntegerRAL;
  vTemp: boolean;
begin
  AOutput := '';
  Result := msrContinue;
  if not AHasInput then
    // no initial response: an empty challenge asks for it
    Exit;
  vText := AInput;
  Inc(FStage);
  case FMethod of
    msaPlain:
    begin
      // authzid NUL authcid NUL password
      vInt := RALMailPos(#0, vText);
      vText := Copy(vText, vInt + 1, MaxInt);
      vInt := RALMailPos(#0, vText);
      if vInt = 0 then
      begin
        Result := msrFailed;
        Exit;
      end;
      vUser := Copy(vText, 1, vInt - 1);
      vPass := Copy(vText, vInt + 1, MaxInt);
      vTemp := False;
      if FServer.CheckPassword(FSession, vUser, vPass, vTemp) then
      begin
        FUserName := vUser;
        Result := msrOk;
      end
      else if vTemp then
        Result := msrTemporary
      else
        Result := msrFailed;
    end;
    msaScramSHA256:
    begin
      if FSCRAM.Done then
      begin
        // the empty response after the server-final message
        if FSCRAM.Accepted and (FServer.CheckAccess(FSession, '', True) < 400) then
        begin
          FUserName := FSCRAM.UserName;
          Result := msrOk;
        end
        else
          Result := msrFailed;
        Exit;
      end;
      try
        AOutput := FSCRAM.Step(vText);
      except
        Result := msrFailed;
        Exit;
      end;
      if FSCRAM.Done and not FSCRAM.Accepted then
        Result := msrFailed;
    end;
  end;
end;

{ TRALMailConnection }

constructor TRALMailConnection.Create(AServer: TRALMailServer; ASocket: TRALSocket;
  AListener: IntegerRAL);
begin
  inherited Create;
  FServer := AServer;
  FSocket := ASocket;
  FPlain := TRALSocketChannel.Create(ASocket);
  FChannel := FPlain;
  FSession := TRALMailSession.Create;
  FSession.FId := AServer.NewId;
  FSession.FListener := AListener;
  FSession.FRemoteIP := ASocket.PeerAddress;
  FSession.FRemotePort := ASocket.PeerPort;
  FInPos := 1;
  FOutPos := 1;
  FLastActive := RALSocketNow;
  if AListener = RALMailListenerTLS then
  begin
    FTLS := TRALTLSChannel.Create(FChannel, AServer.FTLSContext);
    FChannel := FTLS;
    FHandshaking := True;
  end;
end;

destructor TRALMailConnection.Destroy;
begin
  FreeAndNil(FSession);
  // the stack frees down to the socket: TLS owns the plain channel, which owns the socket
  FreeAndNil(FChannel);
  FSocket := nil;
  inherited Destroy;
end;

procedure TRALMailConnection.Closed;
begin
  // nothing to release here: the destructor frees the channel stack
end;

function TRALMailConnection.FaultReply(const AMessage: StringRAL): RawByteString;
begin
  Result := '';
end;

function TRALMailConnection.Greeting: RawByteString;
begin
  Result := '';
end;

function TRALMailConnection.RefusedReply(ATooMany: boolean): RawByteString;
begin
  Result := '';
end;

function TRALMailConnection.Resting: boolean;
begin
  Result := False;
end;

function TRALMailConnection.StopReply: RawByteString;
begin
  Result := '';
end;

procedure TRALMailConnection.TLSStarted;
begin
  // nothing by default
end;

procedure TRALMailConnection.Tick(ANow: Int64RAL);
begin
  // nothing by default
end;

procedure TRALMailConnection.SetLoggedIn(const AUser: StringRAL);
begin
  FSession.FAuthenticated := True;
  FSession.FUserName := AUser;
end;

procedure TRALMailConnection.Send(const AText: RawByteString);
begin
  FOut := FOut + AText;
end;

function TRALMailConnection.PendingOutput: IntegerRAL;
begin
  Result := Length(FOut) - FOutPos + 1;
end;

procedure TRALMailConnection.BeginTLS;
begin
  FStartTLS := True;
end;

function TRALMailConnection.HasOutput: boolean;
begin
  Result := FOutPos <= Length(FOut);
end;

function TRALMailConnection.FlushOutput: boolean;
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

function TRALMailConnection.PollEvents: Word;
begin
  Result := inherited PollEvents;
  // a STARTTLS waits for its reply to leave in clear before reading again; a
  // connection with too much unprocessed input or unsent output stops reading
  if FStartTLS or (Length(FIn) - FInPos + 1 > cMaxInput) or (PendingOutput > cMaxOutput) then
    Result := Result and not RALPollIn;
end;

procedure TRALMailConnection.HandleError(AError: Exception);
var
  vText: RawByteString;
begin
  if AError is ERALSocketError then
    Close
  else
  begin
    // a broken handshake or code that raised: the client hears it, then the end
    if not FHandshaking then
    begin
      vText := FaultReply(StringRAL(AError.Message));
      if vText <> '' then
        Send(vText);
    end;
    CloseAfterFlush := True;
  end;
end;

procedure TRALMailConnection.Stopping;
var
  vText: RawByteString;
begin
  vText := StopReply;
  if (vText <> '') and not FHandshaking then
  begin
    Send(vText);
    FlushOutput;
  end;
end;

procedure TRALMailConnection.SwitchToTLS;
begin
  // what the client sent after the command in clear is dropped (CVE-2011-0411)
  FIn := '';
  FInPos := 1;
  FTLS := TRALTLSChannel.Create(FPlain, FServer.FTLSContext);
  FChannel := FTLS;
  FHandshaking := True;
  FStartTLS := False;
end;

procedure TRALMailConnection.Serve(ANow: Int64RAL);
var
  vBuffer: array[0..16383] of Byte;
  vRes: IntegerRAL;
  vDone: boolean;
  vStatus: IntegerRAL;
  vText: RawByteString;
begin
  if IsClosed or CloseAfterFlush then
    Exit;
  if FStartTLS then
  begin
    // the reply went out: the handshake starts
    if FlushOutput and not HasOutput then
      SwitchToTLS
    else
      Exit;
  end;
  if FHandshaking then
  begin
    if FTLS.HandshakeStep then
    begin
      FHandshaking := False;
      FSession.FTLS := True;
      FLastActive := ANow;
      if FSession.FListener <> RALMailListenerTLS then
        TLSStarted;
    end
    else
    begin
      if ANow - FLastActive > 30000 then
        Close;
      Exit;
    end;
  end;
  if not FGreeted then
  begin
    FGreeted := True;
    // the address lists and the flood plugin see the connection first
    vStatus := FServer.CheckAccess(FSession, '', True);
    if vStatus >= 400 then
    begin
      vText := RefusedReply(vStatus = 429);
      if vText <> '' then
        Send(vText);
      CloseAfterFlush := True;
      Exit;
    end;
    vText := Greeting;
    if vText <> '' then
      Send(vText);
  end;

  { a block at a time, served before the next is read: the input buffer stays
    small whatever the client sends }
  vDone := False;
  if FInPos <= Length(FIn) then
    ProcessInput;
  while not (FStartTLS or IsClosed or CloseAfterFlush or
    (Length(FIn) - FInPos + 1 > cMaxInput) or (PendingOutput > cMaxOutput)) do
  begin
    vRes := FChannel.Read(vBuffer, SizeOf(vBuffer));
    if vRes <= 0 then
    begin
      vDone := vRes = 0;
      Break;
    end;
    SetLength(FIn, Length(FIn) + vRes);
    Move(vBuffer, FIn[Length(FIn) - vRes + 1], vRes);
    FLastActive := ANow;
    ProcessInput;
  end;

  if vDone then
    Close
  else
  begin
    Tick(ANow);
    if (not Resting) and (ANow - FLastActive > FServer.FCommandTimeout) then
      Close;
  end;
end;

{ TRALMailServer }

constructor TRALMailServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAuthMethods := [msaPlain, msaScramSHA256];
  FCommandTimeout := 1800000;
  FMaxConnections := 1000;
  FUsers := TStringList.Create;
  FLock := TCriticalSection.Create;
  FListeners := TList.Create;
  FRetiredContexts := TList.Create;
  { the internal route every login goes through: the plugins and the
    authenticators of the server see it as a request }
  FAuthModule := TRALMailAuthModule.Create(nil);
  FAuthModule.CreateRoute(cAuthRoute, AuthRoute);
  FAuthModule.Server := Self;
end;

destructor TRALMailServer.Destroy;
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

function TRALMailServer.BusyReply: RawByteString;
begin
  Result := '';
end;

function TRALMailServer.CreateRALSSL: TRALSSL;
begin
  Result := TRALMailServerSSL.Create;
end;

function TRALMailServer.GetSSL: TRALMailServerSSL;
begin
  Result := TRALMailServerSSL(GetDefaultSSL);
end;

procedure TRALMailServer.SetSSL(AValue: TRALMailServerSSL);
begin
  GetDefaultSSL.Assign(AValue);
end;

procedure TRALMailServer.SetStore(AValue: TRALMailStore);
begin
  if AValue = FStore then
    Exit;
  if FStore <> nil then
    FStore.RemoveFreeNotification(Self);
  FStore := AValue;
  if FStore <> nil then
    FStore.FreeNotification(Self);
end;

procedure TRALMailServer.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = FStore) then
  begin
    // connections use the store: the server stops before it goes
    if FRunning then
      SetActive(False);
    FStore := nil;
  end;
end;

procedure TRALMailServer.SetUsers(AValue: TStrings);
begin
  FUsers.Assign(AValue);
end;

function TRALMailServer.IPv6IsImplemented: boolean;
begin
  Result := True;
end;

function TRALMailServer.IsPropertyRelevant(const AName: StringRAL): boolean;
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

function TRALMailServer.TLSProvisioning: TRALTLSProvisioning;
begin
  Result := tpPEMFiles;
end;

function TRALMailServer.GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean;
begin
  RALClearTLSCertificate(ACertificate);
  ACertificate.CertificateFile := SSL.CertFile;
  ACertificate.PrivateKeyFile := SSL.KeyFile;
  ACertificate.PrivateKeyPassword := SSL.KeyPassword;
  ACertificate.CertificatePEM := SSL.CertPEM;
  ACertificate.PrivateKeyPEM := SSL.KeyPEM;
  Result := (SSL.CertFile <> '') or (SSL.CertPEM <> '');
end;

function TRALMailServer.SetTLSCertificate(
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

function TRALMailServer.NewTLSContext: TRALTLSContext;
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

function TRALMailServer.NewId: StringRAL;
begin
  Result := StringRAL(IntToHex(FStarted and $FFFFFFFF, 8) + IntToHex(RALAtomicInc(FNextId, 1), 6));
end;

function TRALMailServer.ConnectionCount: IntegerRAL;
begin
  Result := 0;
  if FLoop <> nil then
    Result := FLoop.ConnectionCount;
end;

function TRALMailServer.TestPorts(out AReport: StringRAL): boolean;
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
      Try_(Port, ProtocolName);
      if SSL.Enabled then
      begin
        Try_(FTLSPort, SecureProtocolName);
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

procedure TRALMailServer.SetActive(const AValue: boolean);
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
    if FStore = nil then
      raise ERALException.Create(emMailServerNoStore);
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

procedure TRALMailServer.StartListeners;
var
  vThreads, vInt: IntegerRAL;

  procedure Open(APort, AKind: IntegerRAL);
  var
    vListener: TRALMailListener;
    vFamily: TRALIPFamily;
    vAddress: StringRAL;
  begin
    if APort <= 0 then
      Exit;
    vListener := TRALMailListener.Create;
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
  Open(Port, RALMailListenerPlain);
  if FTLSContext <> nil then
    Open(FTLSPort, RALMailListenerTLS);
  vThreads := FThreadCount;
  if vThreads <= 0 then
    vThreads := RALCPUCount;
  FLoop := TRALSocketLoop.Create;
  FLoop.OnAccept := Accepted;
  for vInt := 0 to FListeners.Count - 1 do
    FLoop.AddListener(TRALMailListener(FListeners[vInt]).Socket,
      TRALMailListener(FListeners[vInt]).Kind);
  FLoop.StartThreads(vThreads);
  FRunning := True;
  FLoop.StartAccepting;
end;

procedure TRALMailServer.StopAll;
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

procedure TRALMailServer.Accepted(ASocket: TRALSocket; AListener: IntegerRAL);
var
  vConn: TRALMailConnection;
  vBusy: RawByteString;
begin
  if ConnectionCount >= FMaxConnections then
  begin
    // over the limit: told so, on the accepting thread
    try
      vBusy := BusyReply;
      if (AListener <> RALMailListenerTLS) and (vBusy <> '') then
        ASocket.Send(vBusy[1], Length(vBusy));
    except
      // the client is gone already
    end;
    ASocket.Free;
    Exit;
  end;
  try
    FLock.Acquire;
    try
      vConn := CreateConnection(ASocket, AListener);
    finally
      FLock.Release;
    end;
  except
    ASocket.Free;
    Exit;
  end;
  FLoop.Adopt(vConn);
end;

procedure TRALMailServer.AuthRoute(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  // reached only when every plugin and authenticator let the login through
  AResponse.Answer(HTTP_OK, 'ok', 'text/plain');
end;

function TRALMailServer.CheckAccess(ASession: TRALMailSession; const AAuth: StringRAL;
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
    vRequest.ClientInfo.UserAgent := StringRAL(ProtocolName);
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

function TRALMailServer.CheckPassword(ASession: TRALMailSession; const AUser,
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

function TRALMailServer.FindSCRAMUser(const AUserName: StringRAL;
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
