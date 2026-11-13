{ The POP3 client: downloads and deletes the messages of a mailbox (RFC 1939), with
  STLS (RFC 2595), SASL (RFC 5034), CAPA and UIDL (RFC 2449). }
unit RALPOP3Client;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools, RALBase64, RALCustomObjects, RALExceptions, RALStream,
  RALClient, RALTLS, RALSCRAM, RALMailMIME, RALMailTransport, RALSMTPClient;

type
  /// A POP3 failure: an -ERR answer, or the connection.
  ERALPOP3Error = class(ERALException);

  /// A line of the conversation: AOutgoing for what the client sent (secrets masked).
  TRALPOP3LogEvent = procedure(Sender: TObject; const ALine: StringRAL; AOutgoing: Boolean) of object;

  { Reads a mailbox over POP3 on the calling thread. Messages are numbered from 1
    within a session; UIDL gives identifiers that last between sessions. Deletions
    take effect when the session ends with Disconnect (QUIT). One client per thread. }
  TRALPOP3Client = class(TRALComponent)
  private
    FAccessToken: StringRAL;
    FAuthMethod: TRALMailAuthMethod;
    FCapabilities: TStringList;
    FConnectTimeout: IntegerRAL;
    FHost: StringRAL;
    /// The connection with its line reader.
    FLink: TRALMailLink;
    FOnGetAccessToken: TRALSMTPTokenEvent;
    FOnLog: TRALPOP3LogEvent;
    FPassword: StringRAL;
    FPort: IntegerRAL;
    FReadTimeout: IntegerRAL;
    FSecurity: TRALMailSecurity;
    FSSL: TRALSocketSSL;
    FUserName: StringRAL;

    /// Logs in with the configured method.
    procedure Authenticate;
    /// Sends ALine and returns the status line.
    function Command(const ALine: RawByteString; ASecret: Boolean = False): RawByteString;
    /// FPort, or the default of the security mode when 0.
    function EffectivePort: IntegerRAL;
    /// Whether the server offers AMechanism.
    function HasSASL(const AMechanism: StringRAL): Boolean;
    /// Passes ALine to OnLog.
    procedure Log(const ALine: RawByteString; AOutgoing: Boolean);
    /// Reads the CAPA list.
    procedure ReadCapabilities;
    /// Reads a multi-line answer into AStream.
    procedure ReadMultiLine(AStream: TStream);
    /// Reads the status line of an answer.
    function ReadStatus: RawByteString;
    procedure SetSSL(AValue: TRALSocketSSL);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Connects, protects and logs in.
    procedure Connect;
    /// Whether a connection is open.
    function Connected: Boolean;
    /// Marks message ANumber for deletion at the end of the session.
    procedure Delete(ANumber: IntegerRAL);
    { Ends the session (QUIT): the marked messages are deleted, then the connection
      closes. }
    procedure Disconnect;
    /// The size of each message: AList gets "number=size" pairs.
    procedure List(AList: TStrings);
    /// Keeps the connection alive.
    procedure Noop;
    /// Unmarks every message marked for deletion.
    procedure Reset;
    /// Reads message ANumber into AMessage.
    procedure Retrieve(ANumber: IntegerRAL; AMessage: TRALMailMessage);
    /// Writes message ANumber, as the server keeps it, to AStream.
    procedure RetrieveRaw(ANumber: IntegerRAL; AStream: TStream);
    /// The number of messages and their total size.
    procedure Stat(out ACount: IntegerRAL; out ASize: Int64);
    /// Writes the header and the first ALines lines of the body of ANumber to AStream.
    procedure Top(ANumber, ALines: IntegerRAL; AStream: TStream);
    /// The lasting identifier of each message: AList gets "number=uid" pairs.
    procedure UIDL(AList: TStrings);

    /// What CAPA announced (upper case), empty when the server has no CAPA.
    property Capabilities: TStringList read FCapabilities;
  published
    /// The OAuth2 access token of XOAUTH2 and OAUTHBEARER (or OnGetAccessToken).
    property AccessToken: StringRAL read FAccessToken write FAccessToken;
    /// The login; maAuto picks a SASL mechanism the server offers, or USER/PASS.
    property AuthMethod: TRALMailAuthMethod read FAuthMethod write FAuthMethod default maAuto;
    /// Milliseconds to connect (and finish the TLS handshake).
    property ConnectTimeout: IntegerRAL read FConnectTimeout write FConnectTimeout default 30000;
    /// The server.
    property Host: StringRAL read FHost write FHost;
    /// Called before a token login, to give a fresh token.
    property OnGetAccessToken: TRALSMTPTokenEvent read FOnGetAccessToken write FOnGetAccessToken;
    /// The conversation, with secrets masked.
    property OnLog: TRALPOP3LogEvent read FOnLog write FOnLog;
    /// The password.
    property Password: StringRAL read FPassword write FPassword;
    /// The port; 0 takes 995 for msImplicitTLS and 110 otherwise.
    property Port: IntegerRAL read FPort write FPort default 0;
    /// Milliseconds to wait for an answer.
    property ReadTimeout: IntegerRAL read FReadTimeout write FReadTimeout default 60000;
    /// How the connection is protected.
    property Security: TRALMailSecurity read FSecurity write FSecurity default msStartTLSIfOffered;
    /// The certificate rules (those of TRALClient).
    property SSL: TRALSocketSSL read FSSL write SetSSL;
    /// The user.
    property UserName: StringRAL read FUserName write FUserName;
  end;

implementation

{ TRALPOP3Client }

constructor TRALPOP3Client.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSSL := TRALSocketSSL.Create;
  FCapabilities := TStringList.Create;
  FConnectTimeout := 30000;
  FReadTimeout := 60000;
  FSecurity := msStartTLSIfOffered;
  FAuthMethod := maAuto;
end;

destructor TRALPOP3Client.Destroy;
begin
  if Connected then
    try
      // the destructor does not delete what was marked: only Disconnect commits
      FLink.Close;
    except
      // nothing to do
    end;
  FreeAndNil(FLink);
  FCapabilities.Free;
  FSSL.Free;
  inherited Destroy;
end;

procedure TRALPOP3Client.SetSSL(AValue: TRALSocketSSL);
begin
  FSSL.Assign(AValue);
end;

function TRALPOP3Client.EffectivePort: IntegerRAL;
begin
  Result := FPort;
  if Result = 0 then
  begin
    if FSecurity = msImplicitTLS then
      Result := 995
    else
      Result := 110;
  end;
end;

procedure TRALPOP3Client.Log(const ALine: RawByteString; AOutgoing: Boolean);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, StringRAL(ALine), AOutgoing);
end;

function TRALPOP3Client.ReadStatus: RawByteString;
begin
  Result := FLink.ReadLine;
  Log(Result, False);
  if Copy(Result, 1, 3) <> '+OK' then
  begin
    if Copy(Result, 1, 1) = '+' then
      Exit; // a SASL continuation
    raise ERALPOP3Error.Create(Format(emPOP3Answer, [string(Result)]));
  end;
end;

function TRALPOP3Client.Command(const ALine: RawByteString; ASecret: Boolean): RawByteString;
begin
  if not Connected then
    Connect;
  if ASecret then
    Log(Copy(ALine, 1, RALMailPos(' ', ALine + ' ')) + '****', True)
  else
    Log(ALine, True);
  FLink.Write(ALine + #13#10);
  Result := ReadStatus;
end;

procedure TRALPOP3Client.ReadMultiLine(AStream: TStream);
var
  vLine: RawByteString;
const
  cCRLF: RawByteString = #13#10;
begin
  // lines until "." alone; a leading dot is doubled on the wire
  repeat
    vLine := FLink.ReadLine;
    if vLine = '.' then
      Break;
    if (vLine <> '') and (vLine[1] = '.') then
      System.Delete(vLine, 1, 1);
    if AStream <> nil then
    begin
      if vLine <> '' then
        AStream.WriteBuffer(vLine[1], Length(vLine));
      AStream.WriteBuffer(cCRLF[1], 2);
    end;
  until False;
end;

procedure TRALPOP3Client.ReadCapabilities;
var
  vData: TStringStream;
  vInt: IntegerRAL;
begin
  FCapabilities.Clear;
  FLink.Write('CAPA'#13#10);
  Log('CAPA', True);
  try
    ReadStatus;
  except
    // a server without CAPA (RFC 1939 only)
    Exit;
  end;
  vData := TStringStream.Create('');
  try
    ReadMultiLine(vData);
    FCapabilities.Text := UpperCase(vData.DataString);
    for vInt := FCapabilities.Count - 1 downto 0 do
      if Trim(FCapabilities[vInt]) = '' then
        FCapabilities.Delete(vInt);
  finally
    vData.Free;
  end;
end;

function TRALPOP3Client.HasSASL(const AMechanism: StringRAL): Boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 0 to FCapabilities.Count - 1 do
    if (Copy(FCapabilities[vInt], 1, 5) = 'SASL ') and
       (Pos(' ' + UpperCase(string(AMechanism)) + ' ', FCapabilities[vInt] + ' ') > 0) then
      Result := True;
end;

procedure TRALPOP3Client.Connect;
begin
  if Connected then
    Exit;
  if FLink = nil then
    FLink := TRALMailLink.Create(Self, FSSL, ERALPOP3Error);
  FLink.ReadTimeout := FReadTimeout;
  try
    FLink.Connect(FHost, EffectivePort, FConnectTimeout);
    if FSecurity = msImplicitTLS then
      FLink.StartTLS(FConnectTimeout);
    ReadStatus;
    ReadCapabilities;
    if FSecurity in [msStartTLS, msStartTLSIfOffered] then
    begin
      if FCapabilities.IndexOf('STLS') >= 0 then
      begin
        Command('STLS');
        FLink.StartTLS(FConnectTimeout);
        // what the server said before TLS does not count (RFC 2595 4)
        ReadCapabilities;
      end
      else if FSecurity = msStartTLS then
        raise ERALPOP3Error.Create(string(emMailNoStartTLS));
    end;
    Authenticate;
  except
    FLink.Close;
    raise;
  end;
end;

procedure TRALPOP3Client.Authenticate;
var
  vMethod: TRALMailAuthMethod;
  vToken: StringRAL;
  vLine: RawByteString;
  vScram: TRALSCRAMClient;
begin
  vMethod := FAuthMethod;
  if vMethod = maNone then
    Exit;
  vToken := FAccessToken;
  if Assigned(FOnGetAccessToken) and (vMethod in [maAuto, maXOAuth2, maOAuthBearer]) then
    FOnGetAccessToken(Self, vToken);
  if vMethod = maAuto then
  begin
    if (vToken <> '') and HasSASL('XOAUTH2') then
      vMethod := maXOAuth2
    else if (vToken <> '') and HasSASL('OAUTHBEARER') then
      vMethod := maOAuthBearer
    else if HasSASL('SCRAM-SHA-256') then
      vMethod := maScramSHA256
    else
      vMethod := maLogin; // USER/PASS
  end;
  case vMethod of
    maPlain:
      Command('AUTH PLAIN ' + RawByteString(RALSASLPlain(FUserName, FPassword)), True);
    maXOAuth2:
    begin
      vLine := Command('AUTH XOAUTH2 ' + RawByteString(RALSASLXOAuth2(FUserName, vToken)), True);
      if Copy(vLine, 1, 3) <> '+OK' then
      begin
        // the error comes in a challenge: an empty answer, then the -ERR
        FLink.Write(#13#10);
        ReadStatus;
      end;
    end;
    maOAuthBearer:
    begin
      vLine := Command('AUTH OAUTHBEARER ' + RawByteString(RALSASLOAuthBearer(FUserName, vToken,
        FHost, EffectivePort)), True);
      if Copy(vLine, 1, 3) <> '+OK' then
      begin
        FLink.Write('AQ==' + #13#10);
        ReadStatus;
      end;
    end;
    maScramSHA256, maScramSHA1:
    begin
      if vMethod = maScramSHA256 then
        vScram := TRALSCRAMClient.Create(rsaSHA256, FUserName, FPassword)
      else
        vScram := TRALSCRAMClient.Create(rsaSHA1, FUserName, FPassword);
      try
        if vMethod = maScramSHA256 then
          vLine := Command('AUTH SCRAM-SHA-256 ' + RawByteString(TRALBase64.Encode(vScram.ClientFirst)), True)
        else
          vLine := Command('AUTH SCRAM-SHA-1 ' + RawByteString(TRALBase64.Encode(vScram.ClientFirst)), True);
        FLink.Write(RawByteString(TRALBase64.Encode(vScram.ClientFinal(
          TRALBase64.Decode(StringRAL(Trim(string(Copy(vLine, 2, MaxInt)))))))) + #13#10);
        vLine := ReadStatus;
        if Copy(vLine, 1, 3) <> '+OK' then
        begin
          if not vScram.CheckServerFinal(TRALBase64.Decode(StringRAL(Trim(string(Copy(vLine, 2, MaxInt)))))) then
            raise ERALPOP3Error.Create(string(emMailServerProof));
          FLink.Write(#13#10);
          ReadStatus;
        end;
      finally
        vScram.Free;
      end;
    end;
  else
    Command('USER ' + RawByteString(FUserName));
    Command('PASS ' + RawByteString(FPassword), True);
  end;
end;

function TRALPOP3Client.Connected: Boolean;
begin
  Result := (FLink <> nil) and FLink.Connected;
end;

procedure TRALPOP3Client.Disconnect;
begin
  if not Connected then
    Exit;
  try
    Command('QUIT');
  finally
    FLink.Close;
  end;
end;

procedure TRALPOP3Client.Stat(out ACount: IntegerRAL; out ASize: Int64);
var
  vParts: TStringList;
begin
  vParts := TStringList.Create;
  try
    vParts.Delimiter := ' ';
    vParts.DelimitedText := string(Command('STAT'));
    ACount := 0;
    ASize := 0;
    if vParts.Count >= 3 then
    begin
      ACount := StrToIntDef(vParts[1], 0);
      ASize := StrToInt64Def(vParts[2], 0);
    end;
  finally
    vParts.Free;
  end;
end;

procedure TRALPOP3Client.List(AList: TStrings);
var
  vData: TStringStream;
begin
  Command('LIST');
  vData := TStringStream.Create('');
  try
    ReadMultiLine(vData);
    AList.Text := StringReplace(Trim(vData.DataString), ' ', '=', [rfReplaceAll]);
  finally
    vData.Free;
  end;
end;

procedure TRALPOP3Client.UIDL(AList: TStrings);
var
  vData: TStringStream;
  vInt, vSpace: IntegerRAL;
  vLines: TStringList;
begin
  Command('UIDL');
  vData := TStringStream.Create('');
  vLines := TStringList.Create;
  try
    ReadMultiLine(vData);
    vLines.Text := vData.DataString;
    AList.Clear;
    for vInt := 0 to vLines.Count - 1 do
    begin
      vSpace := Pos(' ', vLines[vInt]);
      if vSpace > 0 then
        AList.Add(Copy(vLines[vInt], 1, vSpace - 1) + '=' + Copy(vLines[vInt], vSpace + 1, MaxInt));
    end;
  finally
    vLines.Free;
    vData.Free;
  end;
end;

procedure TRALPOP3Client.RetrieveRaw(ANumber: IntegerRAL; AStream: TStream);
begin
  Command('RETR ' + RawByteString(IntToStr(ANumber)));
  ReadMultiLine(AStream);
end;

procedure TRALPOP3Client.Retrieve(ANumber: IntegerRAL; AMessage: TRALMailMessage);
var
  vData: TRALBodyStream;
begin
  vData := TRALBodyStream.Create(-1, AMessage.SpoolAbove);
  try
    RetrieveRaw(ANumber, vData);
    vData.Position := 0;
    AMessage.LoadFromStream(vData);
  finally
    vData.Free;
  end;
end;

procedure TRALPOP3Client.Top(ANumber, ALines: IntegerRAL; AStream: TStream);
begin
  Command('TOP ' + RawByteString(IntToStr(ANumber)) + ' ' + RawByteString(IntToStr(ALines)));
  ReadMultiLine(AStream);
end;

procedure TRALPOP3Client.Delete(ANumber: IntegerRAL);
begin
  Command('DELE ' + RawByteString(IntToStr(ANumber)));
end;

procedure TRALPOP3Client.Reset;
begin
  Command('RSET');
end;

procedure TRALPOP3Client.Noop;
begin
  Command('NOOP');
end;

end.
