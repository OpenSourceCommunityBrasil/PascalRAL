{ The SMTP client: sends RALMailMIME messages over RAL's sockets with the
  extensions that save round trips. }
unit RALSMTPClient;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALBase64, RALCustomObjects, RALExceptions, RALStream,
  RALClient, RALSocket, RALTLS, RALSCRAM, RALMailMIME, RALDKIM;

type
  /// How the connection is protected.
  TRALMailSecurity = (
    /// Plain text (port 25 between servers).
    msNone,
    /// STARTTLS, required: the send fails when the server does not offer it (port 587).
    msStartTLS,
    /// STARTTLS when the server offers it, plain otherwise.
    msStartTLSIfOffered,
    /// TLS from the first byte (port 465, "SMTPS").
    msImplicitTLS
  );

  /// The SASL mechanism of AUTH.
  TRALMailAuthMethod = (
    /// The strongest the server offers that the credentials allow.
    maAuto,
    /// No authentication.
    maNone,
    /// PLAIN (RFC 4616).
    maPlain,
    /// LOGIN (the old two-step one).
    maLogin,
    /// SCRAM-SHA-256 (RFC 7677).
    maScramSHA256,
    /// SCRAM-SHA-1 (RFC 5802).
    maScramSHA1,
    /// XOAUTH2 (Gmail, Microsoft 365) with AccessToken.
    maXOAuth2,
    /// OAUTHBEARER (RFC 7628) with AccessToken.
    maOAuthBearer
  );

  /// A refusal of the server: the reply code, the enhanced code (RFC 3463) and the text.
  ERALSMTPError = class(ERALException)
  private
    FCode: IntegerRAL;
    FEnhanced: StringRAL;
    FReply: StringRAL;
  public
    /// The error of a reply.
    constructor CreateReply(ACode: IntegerRAL; const AReply: StringRAL);

    /// The reply code (0 for a failure that is not a reply).
    property Code: IntegerRAL read FCode;
    /// The enhanced status code (5.1.1...), empty when the server sent none.
    property Enhanced: StringRAL read FEnhanced;
    /// The reply text.
    property Reply: StringRAL read FReply;
  end;

  /// What happened to one recipient of a send.
  TRALSMTPRecipientResult = class
  public
    /// The address.
    Address: StringRAL;
    /// The reply code to RCPT TO (250, 251 accepted).
    Code: IntegerRAL;
    /// The reply text.
    Reply: StringRAL;

    /// Whether the server took the recipient.
    function Accepted: Boolean;
  end;

  /// A line of the conversation: AOutgoing for what the client sent (secrets masked).
  TRALSMTPLogEvent = procedure(Sender: TObject; const ALine: StringRAL; AOutgoing: Boolean) of object;
  /// Asks for a fresh OAuth2 access token (XOAUTH2, OAUTHBEARER).
  TRALSMTPTokenEvent = procedure(Sender: TObject; var AToken: StringRAL) of object;

  { Sends messages over SMTP (RFC 5321) on the calling thread. The connection is
    kept between messages (KeepConnection) and reopened when the server closed
    it; MAIL, every RCPT and DATA go in one write when the server offers
    PIPELINING (RFC 2920); BDAT replaces DATA when it offers CHUNKING (RFC
    3030); 8BITMIME and SMTPUTF8 avoid encoding text; SIZE is declared. One
    client per thread: create one per sending thread. }
  TRALSMTPClient = class(TRALComponent)
  private
    FAccessToken: StringRAL;
    FAllow8Bit: Boolean;
    /// True once AUTH succeeded on the open connection.
    FAuthenticated: Boolean;
    FAuthMethod: TRALMailAuthMethod;
    /// The channel in use: the plain socket or the TLS one.
    FChannel: TRALChannel;
    FChunking: Boolean;
    FConnectTimeout: IntegerRAL;
    FDKIM: TRALDKIMSigner;
    FExtensions: TStringList;
    FHeloName: StringRAL;
    FHost: StringRAL;
    /// Bytes read and not yet used.
    FInBuffer: TBytes;
    /// Bytes in FInBuffer.
    FInCount: IntegerRAL;
    FKeepConnection: Boolean;
    /// The recipient results of the last send.
    FLastResults: TList;
    FMaxSize: Int64;
    FOnGetAccessToken: TRALSMTPTokenEvent;
    FOnLog: TRALSMTPLogEvent;
    FPassword: StringRAL;
    FPipelining: Boolean;
    FPort: IntegerRAL;
    FReadTimeout: IntegerRAL;
    FSecurity: TRALMailSecurity;
    /// The connection.
    FSocket: TRALSocket;
    FSSL: TRALSocketSSL;
    /// The TLS context of the connection.
    FTLSContext: TRALTLSContext;
    FUserName: StringRAL;

    /// Authenticates with the method AuthMethod picks.
    procedure Authenticate;
    /// Frees the results of the last send.
    procedure ClearResults;
    /// Port, or the one Security implies when it is 0.
    function EffectivePort: IntegerRAL;
    /// Reads a reply and raises unless its code is AExpected (its hundred when < 10).
    function Expect(AExpected: IntegerRAL; out AText: StringRAL): IntegerRAL;
    function GetResult(AIndex: IntegerRAL): TRALSMTPRecipientResult;
    function GetResultCount: IntegerRAL;
    /// Whether the server announced AName (EHLO keyword), and its parameters.
    function HasExtension(const AName: StringRAL; out AParams: StringRAL): Boolean; overload;
    /// Whether the server announced AName.
    function HasExtension(const AName: StringRAL): Boolean; overload;
    /// Sends EHLO (HELO when refused) and reads the extensions.
    procedure Hello;
    /// Reads one reply (all its lines); AText gets the text of every line.
    function ReadReply(out AText: StringRAL): IntegerRAL;
    /// Sends a command line (logged, masked when ASecret).
    procedure SendLine(const ALine: StringRAL; ASecret: Boolean = False);
    procedure SetDKIM(AValue: TRALDKIMSigner);
    procedure SetSSL(AValue: TRALSocketSSL);
    /// Wraps the channel in TLS and runs the handshake with Host.
    procedure StartTLS;
    /// Writes the message from AStream, dot-stuffed, ended by CRLF.CRLF (DATA).
    procedure WriteDotStuffed(AStream: TStream);
    /// Writes ASize bytes to the channel.
    procedure WriteRaw(const AData; ASize: IntegerRAL);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Copies the settings (not the connection) of another TRALSMTPClient.
    procedure Assign(ASource: TPersistent); override;
    /// Connects, protects and authenticates (Send does it when needed).
    procedure Connect;
    /// Whether a connection is open.
    function Connected: Boolean;
    /// Sends QUIT and closes the connection.
    procedure Disconnect;
    { Sends AMessage to its To, Cc and Bcc (the envelope sender is its Sender or
      first From). Raises ERALSMTPError when no recipient is accepted or the
      server refuses the message; the result of each recipient is in Results. }
    procedure Send(AMessage: TRALMailMessage);
    { Sends the bytes of AData (a whole message, CRLF lines) from AFrom to
      ARecipients: what a queue or a relay sends. }
    procedure SendRaw(const AFrom: StringRAL; ARecipients: TStrings; AData: TStream;
      AUTF8: Boolean = False);
    { Connects a copy of the settings (security and AUTH included), then
      disconnects; AReport says what the server offers or why it failed (the
      verb "Test connection" of the IDE). }
    function TestConnection(out AReport: StringRAL): Boolean;

    /// The keywords of the server's EHLO (upper case keyword, then its parameters).
    property Extensions: TStringList read FExtensions;
    /// The SIZE the server announced (0 when none).
    property MaxSize: Int64 read FMaxSize;
    /// The number of Results.
    property ResultCount: IntegerRAL read GetResultCount;
    /// The recipients of the last send, with what the server said.
    property Results[AIndex: IntegerRAL]: TRALSMTPRecipientResult read GetResult;
  published
    /// The OAuth2 access token of XOAUTH2 and OAUTHBEARER (or OnGetAccessToken).
    property AccessToken: StringRAL read FAccessToken write FAccessToken;
    /// Sends 8-bit text and UTF-8 headers if the server takes them (8BITMIME, SMTPUTF8).
    property Allow8Bit: Boolean read FAllow8Bit write FAllow8Bit default True;
    /// The SASL mechanism; maAuto picks the strongest offered.
    property AuthMethod: TRALMailAuthMethod read FAuthMethod write FAuthMethod default maAuto;
    /// Uses BDAT when the server offers CHUNKING.
    property Chunking: Boolean read FChunking write FChunking default True;
    /// Milliseconds to connect (and finish the TLS handshake).
    property ConnectTimeout: IntegerRAL read FConnectTimeout write FConnectTimeout default 30000;
    /// Signs every message sent (Send and SendRaw) with DKIM; nil sends them as they are.
    property DKIM: TRALDKIMSigner read FDKIM write SetDKIM;
    /// The name given in EHLO; empty takes the machine's.
    property HeloName: StringRAL read FHeloName write FHeloName;
    /// The server.
    property Host: StringRAL read FHost write FHost;
    /// Keeps the connection between messages (closed by Disconnect or when freed).
    property KeepConnection: Boolean read FKeepConnection write FKeepConnection default True;
    /// Asks for a fresh access token before XOAUTH2/OAUTHBEARER.
    property OnGetAccessToken: TRALSMTPTokenEvent read FOnGetAccessToken write FOnGetAccessToken;
    /// Each line sent and received (secrets masked).
    property OnLog: TRALSMTPLogEvent read FOnLog write FOnLog;
    /// The password (or app password) of AUTH.
    property Password: StringRAL read FPassword write FPassword;
    /// Sends MAIL, RCPT and DATA together when the server offers PIPELINING.
    property Pipelining: Boolean read FPipelining write FPipelining default True;
    /// The port; 0 takes 465 for msImplicitTLS, 587 for STARTTLS and 25 otherwise.
    property Port: IntegerRAL read FPort write FPort default 0;
    /// Milliseconds to wait for each reply.
    property ReadTimeout: IntegerRAL read FReadTimeout write FReadTimeout default 60000;
    /// How the connection is protected.
    property Security: TRALMailSecurity read FSecurity write FSecurity default msStartTLSIfOffered;
    /// TLS: certificate rule (Verify, Pins), client certificate, minimum version.
    property SSL: TRALSocketSSL read FSSL write SetSSL;
    /// The user of AUTH; empty sends no AUTH (unless an access token is given).
    property UserName: StringRAL read FUserName write FUserName;
  end;

implementation

/// Whether AText is 7-bit.
function IsASCII(const AText: StringRAL): Boolean;
var
  vInt: IntegerRAL;
begin
  for vInt := 1 to Length(AText) do
    if Ord(AText[vInt]) > 127 then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

{ ERALSMTPError }

constructor ERALSMTPError.CreateReply(ACode: IntegerRAL; const AReply: StringRAL);
var
  vText: StringRAL;
begin
  FCode := ACode;
  FReply := AReply;
  // "5.1.1 text": an enhanced code first (RFC 2034)
  vText := RALTrim(AReply);
  if (Length(vText) >= 5) and (vText[1] in ['2', '4', '5']) and (vText[2] = '.') then
    FEnhanced := Copy(vText, 1, Pos(' ', string(vText + ' ')) - 1);
  inherited Create(Format('SMTP %d %s', [ACode, string(AReply)]));
end;

{ TRALSMTPRecipientResult }

function TRALSMTPRecipientResult.Accepted: Boolean;
begin
  Result := (Code = 250) or (Code = 251);
end;

{ TRALSMTPClient }

constructor TRALSMTPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSSL := TRALSocketSSL.Create;
  FExtensions := TStringList.Create;
  FLastResults := TList.Create;
  FAllow8Bit := True;
  FChunking := True;
  FPipelining := True;
  FKeepConnection := True;
  FConnectTimeout := 30000;
  FReadTimeout := 60000;
  FSecurity := msStartTLSIfOffered;
  SetLength(FInBuffer, 16384);
end;

destructor TRALSMTPClient.Destroy;
begin
  if Connected then
    try
      Disconnect;
    except
      // a dead connection must not stop the destructor
    end;
  FreeAndNil(FChannel);
  FreeAndNil(FSocket);
  FreeAndNil(FTLSContext);
  ClearResults;
  FLastResults.Free;
  FExtensions.Free;
  FSSL.Free;
  inherited Destroy;
end;

procedure TRALSMTPClient.Authenticate;
var
  vMethod: TRALMailAuthMethod;
  vText, vToken, vMechs: StringRAL;
  vScram: TRALSCRAMClient;
  vAlg: TRALSCRAMAlg;
  vCode: IntegerRAL;
begin
  FAuthenticated := False;
  vMethod := FAuthMethod;
  if vMethod = maNone then
    Exit;
  vToken := FAccessToken;
  if Assigned(FOnGetAccessToken) and (vMethod in [maAuto, maXOAuth2, maOAuthBearer]) then
    FOnGetAccessToken(Self, vToken);
  if (FUserName = '') and (vToken = '') then
    Exit;
  if not HasExtension('AUTH', vMechs) then
    raise ERALSMTPError.CreateReply(0, emMailNoAuth);
  vMechs := ' ' + StringRAL(UpperCase(string(vMechs))) + ' ';
  if vMethod = maAuto then
  begin
    if (vToken <> '') and (Pos(' XOAUTH2 ', string(vMechs)) > 0) then
      vMethod := maXOAuth2
    else if (vToken <> '') and (Pos(' OAUTHBEARER ', string(vMechs)) > 0) then
      vMethod := maOAuthBearer
    else if Pos(' SCRAM-SHA-256 ', string(vMechs)) > 0 then
      vMethod := maScramSHA256
    else if Pos(' SCRAM-SHA-1 ', string(vMechs)) > 0 then
      vMethod := maScramSHA1
    else if Pos(' PLAIN ', string(vMechs)) > 0 then
      vMethod := maPlain
    else if Pos(' LOGIN ', string(vMechs)) > 0 then
      vMethod := maLogin
    else
      raise ERALSMTPError.CreateReply(0, StringRAL(Format(emMailNoCommonAuth, [string(RALTrim(vMechs))])));
  end;
  case vMethod of
    maPlain:
      begin
        SendLine('AUTH PLAIN ' + TRALBase64.Encode(#0 + FUserName + #0 + FPassword), True);
        Expect(235, vText);
      end;
    maLogin:
      begin
        SendLine('AUTH LOGIN');
        Expect(334, vText);
        SendLine(TRALBase64.Encode(FUserName), True);
        Expect(334, vText);
        SendLine(TRALBase64.Encode(FPassword), True);
        Expect(235, vText);
      end;
    maXOAuth2:
      begin
        SendLine('AUTH XOAUTH2 ' + TRALBase64.Encode('user=' + FUserName + #1'auth=Bearer ' +
          vToken + #1#1), True);
        vCode := ReadReply(vText);
        if vCode = 334 then
        begin
          // the server sent a JSON error: an empty line ends the exchange
          SendLine('');
          vCode := ReadReply(vText);
        end;
        if vCode <> 235 then
          raise ERALSMTPError.CreateReply(vCode, vText);
      end;
    maOAuthBearer:
      begin
        SendLine('AUTH OAUTHBEARER ' + TRALBase64.Encode('n,a=' + FUserName + ','#1'host=' +
          FHost + #1'auth=Bearer ' + vToken + #1#1), True);
        vCode := ReadReply(vText);
        if vCode = 334 then
        begin
          SendLine(TRALBase64.Encode(#1));
          vCode := ReadReply(vText);
        end;
        if vCode <> 235 then
          raise ERALSMTPError.CreateReply(vCode, vText);
      end;
    maScramSHA256, maScramSHA1:
      begin
        if vMethod = maScramSHA256 then
        begin
          vAlg := rsaSHA256;
          SendLine('AUTH SCRAM-SHA-256');
        end
        else
        begin
          vAlg := rsaSHA1;
          SendLine('AUTH SCRAM-SHA-1');
        end;
        Expect(334, vText);
        vScram := TRALSCRAMClient.Create(vAlg, FUserName, FPassword);
        try
          SendLine(TRALBase64.Encode(vScram.ClientFirst), True);
          Expect(334, vText);
          SendLine(TRALBase64.Encode(vScram.ClientFinal(TRALBase64.Decode(RALTrim(vText)))), True);
          vCode := ReadReply(vText);
          if vCode = 334 then
          begin
            if not vScram.CheckServerFinal(TRALBase64.Decode(RALTrim(vText))) then
              raise ERALSMTPError.CreateReply(0, emMailServerProof);
            SendLine('');
            vCode := ReadReply(vText);
          end;
          if vCode <> 235 then
            raise ERALSMTPError.CreateReply(vCode, vText);
        finally
          vScram.Free;
        end;
      end;
  end;
  FAuthenticated := True;
end;

procedure TRALSMTPClient.ClearResults;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FLastResults.Count - 1 do
    TObject(FLastResults[vInt]).Free;
  FLastResults.Clear;
end;

function TRALSMTPClient.EffectivePort: IntegerRAL;
begin
  Result := FPort;
  if Result = 0 then
    case FSecurity of
      msImplicitTLS: Result := 465;
      msStartTLS: Result := 587;
    else
      Result := 25;
    end;
end;

function TRALSMTPClient.TestConnection(out AReport: StringRAL): Boolean;
var
  vCopy: TRALSMTPClient;
  vTLS: StringRAL;
  vExt: string;
  vInt: IntegerRAL;
begin
  vCopy := TRALSMTPClient.Create(nil);
  try
    vCopy.Assign(Self);
    vCopy.OnLog := nil;
    try
      vCopy.Connect;
      if vCopy.FChannel is TRALTLSChannel then
        vTLS := TRALTLSChannel(vCopy.FChannel).VersionName
      else
        vTLS := 'no TLS';
      if vCopy.FAuthenticated then
        vTLS := vTLS + ', AUTH ok';
      vExt := '';
      for vInt := 0 to vCopy.Extensions.Count - 1 do
      begin
        if vExt <> '' then
          vExt := vExt + ', ';
        vExt := vExt + vCopy.Extensions[vInt];
      end;
      AReport := StringRAL(Format(cmMailTestConnected, [string(FHost), vCopy.EffectivePort,
        string(vTLS), vExt]));
      vCopy.Disconnect;
      Result := True;
    except
      on E: Exception do
      begin
        AReport := StringRAL(Format(cmMailTestFailed, [E.Message]));
        Result := False;
      end;
    end;
  finally
    vCopy.Free;
  end;
end;

procedure TRALSMTPClient.Connect;
var
  vPort: IntegerRAL;
  vText: StringRAL;
begin
  if Connected then
    Exit;
  vPort := EffectivePort;
  FInCount := 0;
  FSocket := TRALSocket.Create;
  try
    FSocket.Connect(FHost, vPort, FConnectTimeout);
    FChannel := TRALSocketChannel.Create(FSocket, False);
    if FSecurity = msImplicitTLS then
      StartTLS;
    Expect(220, vText);
    Hello;
    if FSecurity in [msStartTLS, msStartTLSIfOffered] then
    begin
      if HasExtension('STARTTLS') then
      begin
        SendLine('STARTTLS');
        Expect(220, vText);
        StartTLS;
        // what the server said before TLS does not count (RFC 3207 4.2)
        Hello;
      end
      else if FSecurity = msStartTLS then
        raise ERALSMTPError.CreateReply(0, emMailNoStartTLS);
    end;
    Authenticate;
  except
    FreeAndNil(FChannel);
    FreeAndNil(FSocket);
    FreeAndNil(FTLSContext);
    raise;
  end;
end;

function TRALSMTPClient.Connected: Boolean;
begin
  Result := FChannel <> nil;
end;

procedure TRALSMTPClient.Disconnect;
var
  vText: StringRAL;
begin
  if not Connected then
    Exit;
  try
    SendLine('QUIT');
    ReadReply(vText);
  finally
    FreeAndNil(FChannel);
    FreeAndNil(FSocket);
    FreeAndNil(FTLSContext);
  end;
end;

function TRALSMTPClient.Expect(AExpected: IntegerRAL; out AText: StringRAL): IntegerRAL;
begin
  Result := ReadReply(AText);
  if Result <> AExpected then
    raise ERALSMTPError.CreateReply(Result, AText);
end;

function TRALSMTPClient.GetResult(AIndex: IntegerRAL): TRALSMTPRecipientResult;
begin
  Result := TRALSMTPRecipientResult(FLastResults[AIndex]);
end;

function TRALSMTPClient.GetResultCount: IntegerRAL;
begin
  Result := FLastResults.Count;
end;

function TRALSMTPClient.HasExtension(const AName: StringRAL; out AParams: StringRAL): Boolean;
var
  vInt: IntegerRAL;
  vLine: StringRAL;
begin
  for vInt := 0 to FExtensions.Count - 1 do
  begin
    vLine := StringRAL(FExtensions[vInt]);
    if (vLine = AName) or (Copy(vLine, 1, Length(AName) + 1) = AName + ' ') or
       (Copy(vLine, 1, Length(AName) + 1) = AName + '=') then
    begin
      AParams := RALTrim(Copy(vLine, Length(AName) + 2, MaxInt));
      Result := True;
      Exit;
    end;
  end;
  AParams := '';
  Result := False;
end;

function TRALSMTPClient.HasExtension(const AName: StringRAL): Boolean;
var
  vParams: StringRAL;
begin
  Result := HasExtension(AName, vParams);
end;

procedure TRALSMTPClient.Hello;
var
  vName, vText, vLine: StringRAL;
  vCode: IntegerRAL;
  vLines: TStringList;
  vInt: IntegerRAL;
  vParams: StringRAL;
begin
  vName := FHeloName;
  if vName = '' then
    vName := 'localhost';
  FExtensions.Clear;
  FMaxSize := 0;
  SendLine('EHLO ' + vName);
  vCode := ReadReply(vText);
  if vCode <> 250 then
  begin
    SendLine('HELO ' + vName);
    Expect(250, vText);
    Exit;
  end;
  // the lines after the first are the extensions
  vLines := TStringList.Create;
  try
    vLines.Text := string(vText);
    for vInt := 1 to vLines.Count - 1 do
    begin
      vLine := StringRAL(UpperCase(Copy(vLines[vInt], 1, Pos(' ', vLines[vInt] + ' ') - 1))) +
        StringRAL(Copy(vLines[vInt], Pos(' ', vLines[vInt] + ' '), MaxInt));
      FExtensions.Add(string(RALTrim(vLine)));
    end;
  finally
    vLines.Free;
  end;
  if HasExtension('SIZE', vParams) then
    FMaxSize := StrToInt64Def(string(vParams), 0);
end;

function TRALSMTPClient.ReadReply(out AText: StringRAL): IntegerRAL;
var
  vLine: StringRAL;
  vInt, vRead: IntegerRAL;
  vMore: Boolean;
  vFound: Boolean;
begin
  AText := '';
  repeat
    // one line, from what is buffered or what arrives
    vFound := False;
    vLine := '';
    while not vFound do
    begin
      for vInt := 0 to FInCount - 1 do
        if FInBuffer[vInt] = 10 then
        begin
          SetLength(vLine, vInt);
          if vInt > 0 then
            Move(FInBuffer[0], Pointer(vLine)^, vInt);
          if (vLine <> '') and (vLine[Length(vLine)] = #13) then
            SetLength(vLine, Length(vLine) - 1);
          Move(FInBuffer[vInt + 1], FInBuffer[0], FInCount - vInt - 1);
          Dec(FInCount, vInt + 1);
          vFound := True;
          Break;
        end;
      if vFound then
        Break;
      if FInCount = Length(FInBuffer) then
        SetLength(FInBuffer, Length(FInBuffer) * 2);
      vRead := FChannel.ReadWait(FInBuffer[FInCount], Length(FInBuffer) - FInCount, FReadTimeout);
      if vRead <= 0 then
      begin
        FreeAndNil(FChannel);
        FreeAndNil(FSocket);
        FreeAndNil(FTLSContext);
        if vRead = 0 then
          raise ERALSMTPError.CreateReply(0, emMailClosed)
        else
          raise ERALSMTPError.CreateReply(0, emMailTimeout);
      end;
      Inc(FInCount, vRead);
    end;
    if Assigned(FOnLog) then
      FOnLog(Self, vLine, False);
    if Length(vLine) < 3 then
      raise ERALSMTPError.CreateReply(0, StringRAL(Format(emMailMalformedReply, [string(vLine)])));
    Result := StrToIntDef(string(Copy(vLine, 1, 3)), 0);
    vMore := (Length(vLine) > 3) and (vLine[4] = '-');
    if AText <> '' then
      AText := AText + #13#10;
    AText := AText + Copy(vLine, 5, MaxInt);
  until not vMore;
end;

procedure TRALSMTPClient.Send(AMessage: TRALMailMessage);
var
  vData: TRALBodyStream;
  vRecipients: TStringList;
  vFrom: StringRAL;
  vOptions: TRALMailWriteOptions;
  vUTF8: Boolean;
  vInt: IntegerRAL;
begin
  vRecipients := TStringList.Create;
  vData := TRALBodyStream.Create(-1, 4 * 1024 * 1024);
  try
    AMessage.AllRecipients(vRecipients);
    if AMessage.Sender <> '' then
      vFrom := AMessage.Sender
    else if AMessage.From.Count > 0 then
      vFrom := AMessage.From[0].Address
    else
      vFrom := '';
    // the extensions decide the encoding: connect first
    Connect;
    vOptions := [];
    if FAllow8Bit and HasExtension('8BITMIME') then
      Include(vOptions, mwo8Bit);
    vUTF8 := False;
    if FAllow8Bit and HasExtension('SMTPUTF8') then
    begin
      Include(vOptions, mwoUTF8Headers);
      // the envelope needs SMTPUTF8 only for non-ASCII addresses or headers
      for vInt := 0 to vRecipients.Count - 1 do
        if not IsASCII(StringRAL(vRecipients[vInt])) then
          vUTF8 := True;
      if not IsASCII(vFrom) or not IsASCII(AMessage.Subject) then
        vUTF8 := True;
      if not vUTF8 then
        Exclude(vOptions, mwoUTF8Headers);
    end;
    AMessage.SaveToStream(vData, vOptions);
    vData.Position := 0;
    SendRaw(vFrom, vRecipients, vData, vUTF8);
  finally
    vData.Free;
    vRecipients.Free;
  end;
end;

procedure TRALSMTPClient.SendLine(const ALine: StringRAL; ASecret: Boolean);
var
  vLine: StringRAL;
begin
  if Assigned(FOnLog) then
  begin
    if ASecret then
      FOnLog(Self, Copy(ALine, 1, Pos(' ', string(ALine + ' '))) + '****', True)
    else
      FOnLog(Self, ALine, True);
  end;
  vLine := ALine + #13#10;
  WriteRaw(Pointer(vLine)^, Length(vLine));
end;

procedure TRALSMTPClient.SendRaw(const AFrom: StringRAL; ARecipients: TStrings;
  AData: TStream; AUTF8: Boolean);
var
  vMail, vText, vBatch: StringRAL;
  vInt, vCode, vAccepted: IntegerRAL;
  vResult: TRALSMTPRecipientResult;
  vPipe, vBDat, v8Bit: Boolean;
  vSize: Int64;
  vBuffer: TBytes;
  vRead: IntegerRAL;
  vSigned: TRALBodyStream;
  vSigner: TRALDKIMSigner;
begin
  ClearResults;
  if ARecipients.Count = 0 then
    raise ERALSMTPError.CreateReply(0, emMailNoRecipient);
  if FDKIM <> nil then
  begin
    // the signature goes before the message: a signed copy is what is sent
    vSigner := FDKIM;
    vSigned := TRALBodyStream.Create(-1, 4 * 1024 * 1024);
    try
      vSigner.SignTo(AData, vSigned);
      vSigned.Position := 0;
      FDKIM := nil;
      try
        SendRaw(AFrom, ARecipients, vSigned, AUTF8);
      finally
        FDKIM := vSigner;
      end;
    finally
      vSigned.Free;
    end;
    Exit;
  end;
  { a kept connection with something to read before any command was closed by
    the server (a 421 on idle, or the end of the stream): open a new one }
  if Connected and (FInCount = 0) and (FSocket <> nil) and
     ((FSocket.WaitFor(RALPollIn, 0) and RALPollIn) <> 0) and not FChannel.HasBuffered then
  begin
    FreeAndNil(FChannel);
    FreeAndNil(FSocket);
    FreeAndNil(FTLSContext);
  end;
  Connect;
  vSize := AData.Size - AData.Position;
  if (FMaxSize > 0) and (vSize > FMaxSize) then
    raise ERALSMTPError.CreateReply(552, '5.3.4 ' + StringRAL(Format(emMailTooBig,
      [vSize, FMaxSize])));
  vPipe := FPipelining and HasExtension('PIPELINING');
  vBDat := FChunking and HasExtension('CHUNKING');
  v8Bit := HasExtension('8BITMIME');
  vMail := 'MAIL FROM:<' + AFrom + '>';
  if HasExtension('SIZE') then
    vMail := vMail + StringRAL(' SIZE=' + IntToStr(vSize));
  if v8Bit and FAllow8Bit then
    vMail := vMail + ' BODY=8BITMIME';
  if AUTF8 then
    vMail := vMail + ' SMTPUTF8';
  try
    if vPipe then
    begin
      // MAIL, every RCPT and DATA (not BDAT: the data follows the answers) in one write
      vBatch := vMail + #13#10;
      for vInt := 0 to ARecipients.Count - 1 do
        vBatch := vBatch + 'RCPT TO:<' + StringRAL(ARecipients[vInt]) + '>'#13#10;
      if not vBDat then
        vBatch := vBatch + 'DATA'#13#10;
      if Assigned(FOnLog) then
        FOnLog(Self, RALTrimRight(vBatch), True);
      WriteRaw(Pointer(vBatch)^, Length(vBatch));
      vCode := ReadReply(vText);
      if vCode <> 250 then
      begin
        // the RCPTs and DATA answers still come: read them before failing
        for vInt := 0 to ARecipients.Count - 1 do
          ReadReply(vText);
        if not vBDat then
          ReadReply(vText);
        raise ERALSMTPError.CreateReply(vCode, vText);
      end;
    end
    else
    begin
      SendLine(vMail);
      Expect(250, vText);
    end;
    vAccepted := 0;
    for vInt := 0 to ARecipients.Count - 1 do
    begin
      if not vPipe then
        SendLine('RCPT TO:<' + StringRAL(ARecipients[vInt]) + '>');
      vResult := TRALSMTPRecipientResult.Create;
      vResult.Address := StringRAL(ARecipients[vInt]);
      vResult.Code := ReadReply(vResult.Reply);
      FLastResults.Add(vResult);
      if vResult.Accepted then
        Inc(vAccepted);
    end;
    if vAccepted = 0 then
    begin
      if vPipe and not vBDat then
      begin
        // DATA was sent: its 503/554 comes, and a 354 must be closed with an empty body
        vCode := ReadReply(vText);
        if vCode = 354 then
        begin
          SendLine('.');
          ReadReply(vText);
        end;
      end;
      // the handler below sends the RSET
      raise ERALSMTPError.CreateReply(TRALSMTPRecipientResult(FLastResults[0]).Code,
        StringRAL(Format(emMailNoneAccepted, [string(TRALSMTPRecipientResult(FLastResults[0]).Reply)])));
    end;
    if vBDat then
    begin
      // one final BDAT chunk with the whole message: the server needs no end marker
      vMail := StringRAL('BDAT ' + IntToStr(vSize) + ' LAST');
      SendLine(vMail);
      SetLength(vBuffer, 65536);
      repeat
        vRead := AData.Read(vBuffer[0], Length(vBuffer));
        if vRead > 0 then
          WriteRaw(vBuffer[0], vRead);
      until vRead <= 0;
      Expect(250, vText);
    end
    else
    begin
      if not vPipe then
        SendLine('DATA');
      Expect(354, vText);
      WriteDotStuffed(AData);
      Expect(250, vText);
    end;
  except
    on E: ERALSMTPError do
    begin
      // a refusal leaves the connection usable for the next message
      if Connected and (E.Code >= 400) then
      begin
        try
          SendLine('RSET');
          ReadReply(vText);
        except
          FreeAndNil(FChannel);
          FreeAndNil(FSocket);
          FreeAndNil(FTLSContext);
        end;
      end;
      raise;
    end;
  end;
  if not FKeepConnection then
    Disconnect;
end;

procedure TRALSMTPClient.SetSSL(AValue: TRALSocketSSL);
begin
  FSSL.Assign(AValue);
end;

procedure TRALSMTPClient.Assign(ASource: TPersistent);
var
  vSource: TRALSMTPClient;
begin
  if not (ASource is TRALSMTPClient) then
  begin
    inherited Assign(ASource);
    Exit;
  end;
  vSource := TRALSMTPClient(ASource);
  FAccessToken := vSource.AccessToken;
  FAllow8Bit := vSource.Allow8Bit;
  FAuthMethod := vSource.AuthMethod;
  FChunking := vSource.Chunking;
  FConnectTimeout := vSource.ConnectTimeout;
  SetDKIM(vSource.DKIM);
  FHeloName := vSource.HeloName;
  FHost := vSource.Host;
  FKeepConnection := vSource.KeepConnection;
  FPassword := vSource.Password;
  FPipelining := vSource.Pipelining;
  FPort := vSource.Port;
  FReadTimeout := vSource.ReadTimeout;
  FSecurity := vSource.Security;
  FSSL.Assign(vSource.SSL);
  FUserName := vSource.UserName;
  FOnGetAccessToken := vSource.OnGetAccessToken;
  FOnLog := vSource.OnLog;
end;

procedure TRALSMTPClient.SetDKIM(AValue: TRALDKIMSigner);
begin
  if FDKIM = AValue then
    Exit;
  if FDKIM <> nil then
    FDKIM.RemoveFreeNotification(Self);
  FDKIM := AValue;
  if FDKIM <> nil then
    FDKIM.FreeNotification(Self);
end;

procedure TRALSMTPClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDKIM) then
    FDKIM := nil;
end;

procedure TRALSMTPClient.StartTLS;
var
  vTLS: TRALTLSChannel;
  vCert: TRALCertInfo;
  vName: StringRAL;
begin
  FreeAndNil(FTLSContext);
  FTLSContext := TRALTLSContext.Create(mtrClient);
  FTLSContext.SetMinVersion(FSSL.MinVersion);
  FTLSContext.LoadSystemCAs;
  if FSSL.CAFile <> '' then
    FTLSContext.LoadCAFile(FSSL.CAFile);
  if FSSL.CertFile <> '' then
    FTLSContext.UseCertificateFiles(FSSL.CertFile, FSSL.KeyFile, FSSL.KeyPassword);
  vTLS := TRALTLSChannel.Create(FChannel, FTLSContext);
  FChannel := vTLS;
  vName := FSSL.ServerName;
  if vName = '' then
    vName := FHost;
  vTLS.Handshake(vName, FConnectTimeout);
  vCert := vTLS.PeerCertificate;
  if not RALTLSAcceptCertificate(Self, FSSL, nil, vCert, FHost, FSocket.PeerPort) then
    raise ERALSMTPError.CreateReply(0, StringRAL(Format(emMailCertRefused, [string(vCert.Error)])));
end;

procedure TRALSMTPClient.WriteDotStuffed(AStream: TStream);
var
  vIn, vOut: TBytes;
  vRead, vInt, vCount: IntegerRAL;
  vLineStart: Boolean;
  vLast: Byte;
begin
  SetLength(vIn, 65536);
  SetLength(vOut, 65536 * 2 + 8);
  vLineStart := True;
  vLast := 10;
  repeat
    vRead := AStream.Read(vIn[0], Length(vIn));
    vCount := 0;
    for vInt := 0 to vRead - 1 do
    begin
      // a dot at the start of a line is doubled (RFC 5321 4.5.2)
      if vLineStart and (vIn[vInt] = Ord('.')) then
      begin
        vOut[vCount] := Ord('.');
        Inc(vCount);
      end;
      vOut[vCount] := vIn[vInt];
      Inc(vCount);
      vLineStart := vIn[vInt] = 10;
      vLast := vIn[vInt];
    end;
    if vCount > 0 then
      WriteRaw(vOut[0], vCount);
  until vRead <= 0;
  if vLast <> 10 then
    WriteRaw(PAnsiChar(#13#10)^, 2);
  WriteRaw(PAnsiChar('.'#13#10)^, 3);
  if Assigned(FOnLog) then
    FOnLog(Self, '.', True);
end;

procedure TRALSMTPClient.WriteRaw(const AData; ASize: IntegerRAL);
begin
  if FChannel = nil then
    raise ERALSMTPError.CreateReply(0, emMailNotConnected);
  FChannel.WriteAll(AData, ASize, FReadTimeout);
end;

end.
