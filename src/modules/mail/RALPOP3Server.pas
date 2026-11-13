/// The POP3 server: serves the INBOX of a TRALMailStore (RFC 1939, 2449, 5034).
unit RALPOP3Server;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools, RALBase64, RALStream, RALSocket, RALSocketLoop,
  RALMailStore, RALMailServerBase;

type
  TRALPOP3Server = class;

  /// Where a POP3 session is: login, a SASL exchange or the maildrop.
  TRALPOP3Phase = (p3Authorization, p3SASL, p3Transaction);

  { One POP3 client connection: the commands, the replies and the events all run
    on one poll thread. The maildrop is the INBOX of the user, locked for the
    session; the list of messages is taken at login and deletions are applied at
    QUIT. }
  TRALPOP3Connection = class(TRALMailConnection)
  private
    /// Failed logins and unknown commands so far.
    FBadLogins: IntegerRAL;
    /// The messages marked with DELE.
    FDeleted: array of boolean;
    /// True while the header of the message being sent is not over.
    FInHeader: boolean;
    /// True while ProcessInput runs.
    FInProcess: boolean;
    /// True when the last byte sent of the message was a LF.
    FLastLF: boolean;
    /// Characters of the current line of a message being sent, CR not counted.
    FLineLength: IntegerRAL;
    /// Lines of the body still to send (TOP); -1 sends all.
    FLinesLeft: IntegerRAL;
    /// Bytes of the current message sent so far have a line start.
    FLineStart: boolean;
    /// Whether the maildrop of the user is held.
    FLocked: boolean;
    /// The messages at login.
    FMessages: TRALMailMessageInfos;
    /// Where the session is.
    FPhase: TRALPOP3Phase;
    /// The SASL exchange in progress.
    FSASL: TRALMailSASL;
    /// The server that owns the connection.
    FServer: TRALPOP3Server;
    /// The message being sent (RETR, TOP); nil when none.
    FStream: TStream;
    /// The user given with USER.
    FUser: StringRAL;
    /// The UIDVALIDITY of the maildrop.
    FValidity: Cardinal;

    /// Runs a command line.
    procedure Command(const ALine: RawByteString);
    /// The AUTH command.
    procedure DoAuth(const AArgs: RawByteString);
    /// The LIST and UIDL commands.
    procedure DoList(const AArg: RawByteString; AUnique: boolean);
    /// The PASS command.
    procedure DoPass(const AArg: RawByteString);
    /// The QUIT command: applies the deletions.
    procedure DoQuit;
    /// The RETR and TOP commands: starts sending the message.
    procedure DoRetr(const AArg, ALines: RawByteString; AWhole: boolean);
    /// The STAT command.
    procedure DoStat;
    /// Ends the message being sent: the end line and the next command.
    procedure EndStream;
    /// A login that succeeded: takes the maildrop; false when it cannot be taken.
    function Enter(const AUser: StringRAL): boolean;
    /// The result of a login attempt.
    procedure LoginResult(AResult: TRALMailSASLResult; const AUser: StringRAL);
    /// The message of the number in AArg, or -1 after the reply that tells why not.
    function MessageIndex(const AArg: RawByteString): IntegerRAL;
    /// Whether PLAIN and USER/PASS may be used now.
    function PlainAllowed: boolean;
    /// Sends the next piece of the message being sent.
    procedure Pump;
    /// A step of a SASL exchange with the client's response line.
    procedure SASLLine(const ALine: RawByteString);
    /// Whether SCRAM-SHA-256 may be used now.
    function ScramAllowed: boolean;
    /// Gives the maildrop back.
    procedure Unlock;
  protected
    procedure Closed; override;
    function FaultReply(const AMessage: StringRAL): RawByteString; override;
    function Greeting: RawByteString; override;
    procedure ProcessInput; override;
    function RefusedReply(ATooMany: boolean): RawByteString; override;
    function StopReply: RawByteString; override;
    procedure Tick(ANow: Int64RAL); override;
    procedure TLSStarted; override;
  public
    /// Creates the connection of ASocket, accepted on listener AListener.
    constructor Create(AServer: TRALPOP3Server; ASocket: TRALSocket; AListener: IntegerRAL);
    destructor Destroy; override;

    function HasOutput: boolean; override;
  end;

  { A POP3 server (RFC 1939) over the socket event loop: Port (110) with STLS
    (RFC 2595), TLSPort (995) with implicit TLS, CAPA, UIDL, TOP and PIPELINING
    (RFC 2449), SASL PLAIN and SCRAM-SHA-256 (RFC 5034) and USER/PASS. The
    mailbox of a user is the INBOX of Store. The login goes through the
    server's plugins as the other mail servers' does. }
  TRALPOP3Server = class(TRALMailServer)
  protected
    function BusyReply: RawByteString; override;
    function CreateConnection(ASocket: TRALSocket; AListener: IntegerRAL): TRALMailConnection; override;
    function ProtocolName: string; override;
    function SecureProtocolName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// The implicit TLS port (995), opened when SSL is enabled; 0 closes it.
    property TLSPort default 995;
  end;

implementation

const
  /// The longest command line, in bytes.
  cMaxLine = 2048;
  /// Failed logins after which the connection is closed.
  cMaxBadLogins = 10;
  /// A message is sent while less than this is queued, in bytes.
  cPumpLimit = 131072;

{ TRALPOP3Connection }

constructor TRALPOP3Connection.Create(AServer: TRALPOP3Server; ASocket: TRALSocket;
  AListener: IntegerRAL);
begin
  inherited Create(AServer, ASocket, AListener);
  FServer := AServer;
  FLinesLeft := -1;
end;

destructor TRALPOP3Connection.Destroy;
begin
  Unlock;
  FreeAndNil(FStream);
  FreeAndNil(FSASL);
  inherited Destroy;
end;

procedure TRALPOP3Connection.Closed;
begin
  Unlock;
end;

procedure TRALPOP3Connection.Unlock;
begin
  if FLocked and (FServer.Store <> nil) then
    FServer.Store.UnlockMaildrop(Session.UserName);
  FLocked := False;
end;

function TRALPOP3Connection.HasOutput: boolean;
begin
  // a message being sent keeps the poll awake for the next piece
  Result := inherited HasOutput or (FStream <> nil);
end;

function TRALPOP3Connection.Greeting: RawByteString;
begin
  Result := '+OK ' + RawByteString(FServer.HostName) + ' POP3 server ready';
  if FServer.Banner <> '' then
    Result := Result + ' ' + RawByteString(FServer.Banner);
  Result := Result + #13#10;
end;

function TRALPOP3Connection.RefusedReply(ATooMany: boolean): RawByteString;
begin
  if ATooMany then
    Result := '-ERR [SYS/TEMP] too many connections, try later'#13#10
  else
    Result := '-ERR access denied'#13#10;
end;

function TRALPOP3Connection.FaultReply(const AMessage: StringRAL): RawByteString;
begin
  Result := '-ERR [SYS/TEMP] internal error'#13#10;
end;

function TRALPOP3Connection.StopReply: RawByteString;
begin
  Result := '-ERR [SYS/TEMP] server shutting down'#13#10;
end;

procedure TRALPOP3Connection.TLSStarted;
begin
  // the session starts over (RFC 2595 4)
  FPhase := p3Authorization;
  FUser := '';
  FreeAndNil(FSASL);
end;

procedure TRALPOP3Connection.Tick(ANow: Int64RAL);
begin
  if FStream <> nil then
    Pump;
end;

procedure TRALPOP3Connection.ProcessInput;
var
  vEnd: IntegerRAL;
  vLine: RawByteString;
begin
  FInProcess := True;
  try
    while (not IsClosed) and (not CloseAfterFlush) and (FStream = nil) and not TLSPending do
    begin
      vEnd := FInPos;
      while (vEnd <= Length(FIn)) and (FIn[vEnd] <> #10) do
        Inc(vEnd);
      if vEnd > Length(FIn) then
      begin
        if Length(FIn) - FInPos + 1 > cMaxLine then
        begin
          Send('-ERR line too long'#13#10);
          CloseAfterFlush := True;
        end;
        Break;
      end;
      vLine := Copy(FIn, FInPos, vEnd - FInPos);
      if (vLine <> '') and (vLine[Length(vLine)] = #13) then
        SetLength(vLine, Length(vLine) - 1);
      FInPos := vEnd + 1;
      if FPhase = p3SASL then
        SASLLine(vLine)
      else
        Command(vLine);
    end;
    if FInPos > 1 then
    begin
      Delete(FIn, 1, FInPos - 1);
      FInPos := 1;
    end;
  finally
    FInProcess := False;
  end;
end;

procedure TRALPOP3Connection.Command(const ALine: RawByteString);
var
  vSpace: IntegerRAL;
  vCmd, vArgs, vArg2: RawByteString;
begin
  vSpace := Pos(RawByteString(' '), ALine);
  if vSpace > 0 then
  begin
    vCmd := RALMailUpper(Copy(ALine, 1, vSpace - 1));
    vArgs := RALMailTrim(Copy(ALine, vSpace + 1, MaxInt));
  end
  else
  begin
    vCmd := RALMailUpper(ALine);
    vArgs := '';
  end;
  if vCmd = 'QUIT' then
  begin
    DoQuit;
    Exit;
  end;
  if vCmd = 'CAPA' then
  begin
    Send('+OK Capability list follows'#13#10 + 'TOP'#13#10 + 'UIDL'#13#10 + 'PIPELINING'#13#10 +
      'RESP-CODES'#13#10 + 'AUTH-RESP-CODE'#13#10 + 'EXPIRE NEVER'#13#10);
    if FPhase = p3Authorization then
    begin
      if PlainAllowed then
        Send('USER'#13#10);
      if not Session.TLS and (FServer.SSL.Enabled) then
        Send('STLS'#13#10);
      Send('SASL');
      if PlainAllowed then
        Send(' PLAIN');
      if ScramAllowed then
        Send(' SCRAM-SHA-256');
      Send(#13#10);
    end;
    Send('IMPLEMENTATION RAL'#13#10'.'#13#10);
    Exit;
  end;
  if FPhase = p3Authorization then
  begin
    if vCmd = 'USER' then
    begin
      if vArgs = '' then
        Send('-ERR missing user name'#13#10)
      else if not PlainAllowed then
        Send('-ERR [AUTH] plaintext authentication is allowed only over TLS'#13#10)
      else
      begin
        FUser := StringRAL(vArgs);
        Send('+OK'#13#10);
      end;
    end
    else if vCmd = 'PASS' then
    begin
      // the password is the rest of the line, blanks included
      vSpace := Pos(RawByteString(' '), ALine);
      DoPass(Copy(ALine, vSpace + 1, MaxInt));
    end
    else if vCmd = 'AUTH' then
      DoAuth(vArgs)
    else if vCmd = 'STLS' then
    begin
      if Session.TLS then
        Send('-ERR already using TLS'#13#10)
      else if not FServer.SSL.Enabled then
        Send('-ERR STLS not available'#13#10)
      else
      begin
        Send('+OK Begin TLS negotiation'#13#10);
        BeginTLS;
      end;
    end
    else if vCmd = 'APOP' then
      Send('-ERR APOP not supported'#13#10)
    else
      Send('-ERR command not valid in the authorization state'#13#10);
    Exit;
  end;
  // TRANSACTION
  if vCmd = 'STAT' then
    DoStat
  else if vCmd = 'LIST' then
    DoList(vArgs, False)
  else if vCmd = 'UIDL' then
    DoList(vArgs, True)
  else if vCmd = 'RETR' then
    DoRetr(vArgs, '', True)
  else if vCmd = 'TOP' then
  begin
    vSpace := Pos(RawByteString(' '), vArgs);
    if vSpace = 0 then
      Send('-ERR syntax: TOP msg lines'#13#10)
    else
    begin
      vArg2 := RALMailTrim(Copy(vArgs, vSpace + 1, MaxInt));
      DoRetr(Copy(vArgs, 1, vSpace - 1), vArg2, False);
    end;
  end
  else if vCmd = 'DELE' then
  begin
    vSpace := MessageIndex(vArgs);
    if vSpace >= 0 then
    begin
      FDeleted[vSpace] := True;
      Send('+OK message ' + RawByteString(IntToStr(vSpace + 1)) + ' deleted'#13#10);
    end;
  end
  else if vCmd = 'NOOP' then
    Send('+OK'#13#10)
  else if vCmd = 'RSET' then
  begin
    for vSpace := 0 to High(FDeleted) do
      FDeleted[vSpace] := False;
    Send('+OK'#13#10);
  end
  else
  begin
    Inc(FBadLogins);
    Send('-ERR unknown command'#13#10);
  end;
end;

function TRALPOP3Connection.PlainAllowed: boolean;
begin
  Result := (msaPlain in FServer.AuthMethods) and
    (Session.TLS or (FServer.AllowPlainAuthWithoutTLS and not FServer.RequireTLS));
end;

function TRALPOP3Connection.ScramAllowed: boolean;
begin
  Result := (msaScramSHA256 in FServer.AuthMethods) and (Session.TLS or not FServer.RequireTLS);
end;

function TRALPOP3Connection.MessageIndex(const AArg: RawByteString): IntegerRAL;
var
  vNumber: Int64;
begin
  Result := -1;
  vNumber := StrToInt64Def(string(AArg), 0);
  if (vNumber < 1) or (vNumber > Length(FMessages)) then
    Send('-ERR no such message'#13#10)
  else if FDeleted[vNumber - 1] then
    Send('-ERR message is deleted'#13#10)
  else
    Result := vNumber - 1;
end;

procedure TRALPOP3Connection.DoStat;
var
  vInt, vCount: IntegerRAL;
  vSize: Int64;
begin
  vCount := 0;
  vSize := 0;
  for vInt := 0 to High(FMessages) do
    if not FDeleted[vInt] then
    begin
      Inc(vCount);
      Inc(vSize, FMessages[vInt].Size);
    end;
  Send('+OK ' + RawByteString(IntToStr(vCount) + ' ' + IntToStr(vSize)) + #13#10);
end;

procedure TRALPOP3Connection.DoList(const AArg: RawByteString; AUnique: boolean);

  function Item(AIndex: IntegerRAL): RawByteString;
  begin
    if AUnique then
      Result := RawByteString(IntToStr(AIndex + 1) + ' ' + IntToStr(FValidity) + '.' +
        IntToStr(FMessages[AIndex].UID))
    else
      Result := RawByteString(IntToStr(AIndex + 1) + ' ' + IntToStr(FMessages[AIndex].Size));
  end;

var
  vInt, vCount: IntegerRAL;
  vSize: Int64;
begin
  if AArg <> '' then
  begin
    vInt := MessageIndex(AArg);
    if vInt >= 0 then
      Send('+OK ' + Item(vInt) + #13#10);
    Exit;
  end;
  vCount := 0;
  vSize := 0;
  for vInt := 0 to High(FMessages) do
    if not FDeleted[vInt] then
    begin
      Inc(vCount);
      Inc(vSize, FMessages[vInt].Size);
    end;
  if AUnique then
    Send('+OK'#13#10)
  else
    Send('+OK ' + RawByteString(IntToStr(vCount) + ' messages (' + IntToStr(vSize) + ' octets)') + #13#10);
  for vInt := 0 to High(FMessages) do
    if not FDeleted[vInt] then
      Send(Item(vInt) + #13#10);
  Send('.'#13#10);
end;

procedure TRALPOP3Connection.DoRetr(const AArg, ALines: RawByteString; AWhole: boolean);
var
  vIndex: IntegerRAL;
  vLines: Int64;
begin
  vIndex := MessageIndex(AArg);
  if vIndex < 0 then
    Exit;
  vLines := -1;
  if not AWhole then
  begin
    vLines := StrToInt64Def(string(ALines), -1);
    if vLines < 0 then
    begin
      Send('-ERR invalid number of lines'#13#10);
      Exit;
    end;
  end;
  FStream := FServer.Store.OpenMessage(Session.UserName, 'INBOX', FMessages[vIndex].UID);
  if FStream = nil then
  begin
    Send('-ERR message no longer available'#13#10);
    Exit;
  end;
  if AWhole then
    Send('+OK ' + RawByteString(IntToStr(FMessages[vIndex].Size)) + ' octets'#13#10)
  else
    Send('+OK'#13#10);
  FLineStart := True;
  FLineLength := 0;
  FInHeader := not AWhole;
  FLinesLeft := vLines;
  FLastLF := True;
  Pump;
end;

procedure TRALPOP3Connection.Pump;
var
  vBuffer: array[0..16383] of Byte;
  vOut: RawByteString;
  vRead, vInt, vPos: IntegerRAL;
  vChar: AnsiChar;
  vDone: boolean;
begin
  vDone := False;
  while (FStream <> nil) and (PendingOutput < cPumpLimit) and not vDone do
  begin
    vRead := FStream.Read(vBuffer, SizeOf(vBuffer));
    if vRead <= 0 then
    begin
      vDone := True;
      Break;
    end;
    SetLength(vOut, vRead * 2);
    vPos := 0;
    for vInt := 0 to vRead - 1 do
    begin
      vChar := AnsiChar(vBuffer[vInt]);
      // a line that starts with a dot gets another (RFC 1939 3)
      if FLineStart and (vChar = '.') then
      begin
        Inc(vPos);
        vOut[vPos] := '.';
      end;
      Inc(vPos);
      vOut[vPos] := vChar;
      FLineStart := vChar = #10;
      FLastLF := FLineStart;
      if vChar = #10 then
      begin
        if FInHeader then
        begin
          if FLineLength = 0 then
          begin
            FInHeader := False;
            if FLinesLeft = 0 then
              vDone := True;
          end;
        end
        else if FLinesLeft > 0 then
        begin
          Dec(FLinesLeft);
          if FLinesLeft = 0 then
            vDone := True;
        end;
        FLineLength := 0;
      end
      else if vChar <> #13 then
        Inc(FLineLength);
      if vDone then
        Break;
    end;
    SetLength(vOut, vPos);
    Send(vOut);
  end;
  if vDone then
    EndStream;
end;

procedure TRALPOP3Connection.EndStream;
begin
  FreeAndNil(FStream);
  if not FLastLF then
    Send(#13#10);
  Send('.'#13#10);
  // the commands that came while the message went out
  if not FInProcess then
    ProcessInput;
end;

procedure TRALPOP3Connection.DoQuit;
var
  vInt, vCount: IntegerRAL;
  vUIDs: TRALMailUIDs;
  vFailed: boolean;
begin
  if FPhase <> p3Transaction then
  begin
    Send('+OK bye'#13#10);
    CloseAfterFlush := True;
    Exit;
  end;
  // the update state: the marked messages go
  vFailed := False;
  SetLength(vUIDs, Length(FMessages));
  vCount := 0;
  for vInt := 0 to High(FMessages) do
    if FDeleted[vInt] then
    begin
      vUIDs[vCount] := FMessages[vInt].UID;
      Inc(vCount);
    end;
  SetLength(vUIDs, vCount);
  if vCount > 0 then
    try
      FServer.Store.DeleteMessages(Session.UserName, 'INBOX', vUIDs);
    except
      vFailed := True;
    end;
  Unlock;
  if vFailed then
    Send('-ERR [SYS/PERM] some deleted messages were not removed'#13#10)
  else
    Send('+OK bye'#13#10);
  CloseAfterFlush := True;
end;

function TRALPOP3Connection.Enter(const AUser: StringRAL): boolean;
var
  vNext: Cardinal;
  vAll: TRALMailMessageInfos;
  vInt, vCount: IntegerRAL;
  vSize: Int64;
begin
  Result := False;
  if not FServer.Store.TryLockMaildrop(AUser) then
  begin
    Send('-ERR [IN-USE] the maildrop is locked by another session'#13#10);
    Exit;
  end;
  try
    vAll := FServer.Store.ListMessages(AUser, 'INBOX', FValidity, vNext);
  except
    FServer.Store.UnlockMaildrop(AUser);
    Send('-ERR [SYS/PERM] the maildrop cannot be opened'#13#10);
    Exit;
  end;
  // a message an IMAP client marked \Deleted is already gone for POP3
  SetLength(FMessages, Length(vAll));
  vCount := 0;
  vSize := 0;
  for vInt := 0 to High(vAll) do
    if not RALMailFlagHas(vAll[vInt].Flags, '\Deleted') then
    begin
      FMessages[vCount] := vAll[vInt];
      Inc(vSize, vAll[vInt].Size);
      Inc(vCount);
    end;
  SetLength(FMessages, vCount);
  SetLength(FDeleted, vCount);
  for vInt := 0 to vCount - 1 do
    FDeleted[vInt] := False;
  SetLoggedIn(AUser);
  FLocked := True;
  FPhase := p3Transaction;
  Send('+OK maildrop has ' + RawByteString(IntToStr(vCount) + ' messages (' + IntToStr(vSize) +
    ' octets)') + #13#10);
  Result := True;
end;

procedure TRALPOP3Connection.LoginResult(AResult: TRALMailSASLResult; const AUser: StringRAL);
begin
  case AResult of
    msrOk:
      Enter(AUser);
    msrTemporary:
      Send('-ERR [SYS/TEMP] authentication is not possible now, try later'#13#10);
  else
    begin
      Inc(FBadLogins);
      Send('-ERR [AUTH] authentication failed'#13#10);
    end;
  end;
  if FBadLogins >= cMaxBadLogins then
    CloseAfterFlush := True;
end;

procedure TRALPOP3Connection.DoPass(const AArg: RawByteString);
var
  vTemp: boolean;
begin
  if FUser = '' then
  begin
    Send('-ERR USER first'#13#10);
    Exit;
  end;
  vTemp := False;
  if FServer.CheckPassword(Session, FUser, StringRAL(AArg), vTemp) then
    LoginResult(msrOk, FUser)
  else if vTemp then
    LoginResult(msrTemporary, '')
  else
    LoginResult(msrFailed, '');
  FUser := '';
end;

procedure TRALPOP3Connection.DoAuth(const AArgs: RawByteString);
var
  vSpace: IntegerRAL;
  vMech, vInitial: RawByteString;
  vMethod: TRALMailServerAuthMethod;
begin
  if AArgs = '' then
  begin
    // the list of mechanisms (RFC 5034 4)
    Send('+OK'#13#10);
    if PlainAllowed then
      Send('PLAIN'#13#10);
    if ScramAllowed then
      Send('SCRAM-SHA-256'#13#10);
    Send('.'#13#10);
    Exit;
  end;
  vSpace := Pos(RawByteString(' '), AArgs);
  if vSpace > 0 then
  begin
    vMech := RALMailUpper(Copy(AArgs, 1, vSpace - 1));
    vInitial := RALMailTrim(Copy(AArgs, vSpace + 1, MaxInt));
  end
  else
  begin
    vMech := RALMailUpper(AArgs);
    vInitial := '';
  end;
  if vMech = 'PLAIN' then
    vMethod := msaPlain
  else if vMech = 'SCRAM-SHA-256' then
    vMethod := msaScramSHA256
  else
  begin
    Send('-ERR [AUTH] unsupported authentication mechanism'#13#10);
    Exit;
  end;
  if not (vMethod in FServer.AuthMethods) then
  begin
    Send('-ERR [AUTH] unsupported authentication mechanism'#13#10);
    Exit;
  end;
  if ((vMethod = msaPlain) and not PlainAllowed) or ((vMethod = msaScramSHA256) and not ScramAllowed) then
  begin
    Send('-ERR [AUTH] encryption required for this authentication mechanism'#13#10);
    Exit;
  end;
  FreeAndNil(FSASL);
  FSASL := TRALMailSASL.Create(FServer, Session, vMethod);
  FPhase := p3SASL;
  if vInitial = '' then
    // no initial response: an empty challenge asks for it
    Send('+ '#13#10)
  else
    SASLLine(vInitial);
end;

procedure TRALPOP3Connection.SASLLine(const ALine: RawByteString);
var
  vText, vOutput: StringRAL;
  vResult: TRALMailSASLResult;
begin
  if ALine = '*' then
  begin
    // the client gave up (RFC 5034 4)
    FPhase := p3Authorization;
    FreeAndNil(FSASL);
    Send('-ERR [AUTH] authentication canceled'#13#10);
    Exit;
  end;
  vText := '';
  if (ALine <> '') and (ALine <> '=') then
    try
      vText := TRALBase64.Decode(StringRAL(ALine));
    except
      FPhase := p3Authorization;
      FreeAndNil(FSASL);
      Send('-ERR [AUTH] cannot decode the response'#13#10);
      Exit;
    end;
  vResult := FSASL.Step(vText, True, vOutput);
  case vResult of
    msrContinue:
      Send('+ ' + RawByteString(TRALBase64.Encode(vOutput)) + #13#10);
  else
    begin
      vText := FSASL.UserName;
      FPhase := p3Authorization;
      FreeAndNil(FSASL);
      LoginResult(vResult, vText);
    end;
  end;
end;

{ TRALPOP3Server }

constructor TRALPOP3Server.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Port := 110;
  TLSPort := 995;
  SetEngine('POP3 ' + StringRAL(RALVERSION));
end;

function TRALPOP3Server.BusyReply: RawByteString;
begin
  Result := '-ERR [SYS/TEMP] too many connections, try later'#13#10;
end;

function TRALPOP3Server.CreateConnection(ASocket: TRALSocket;
  AListener: IntegerRAL): TRALMailConnection;
begin
  Result := TRALPOP3Connection.Create(Self, ASocket, AListener);
end;

function TRALPOP3Server.ProtocolName: string;
begin
  Result := 'POP3';
end;

function TRALPOP3Server.SecureProtocolName: string;
begin
  Result := 'POP3S';
end;

end.
