{ The connection the mailbox clients (IMAP, POP3) share: a socket, TLS from the start
  or on request with the client's certificate rule, buffered reading of lines and
  counted bytes, and the SASL initial responses. }
unit RALMailTransport;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALBase64, RALExceptions, RALClient, RALSocket, RALTLS;

type
  /// The exception class a link raises, so each protocol keeps its own.
  ERALMailErrorClass = class of ERALException;

  { One connection of a mail client: written and read on the caller's thread.
    Lines end in CRLF (a bare LF is accepted); what is read past a line stays
    buffered for the next read. }
  TRALMailLink = class
  private
    /// Bytes received and not yet consumed.
    FBuffer: TBytes;
    /// The channel on top: the socket's, or TLS over it.
    FChannel: TRALChannel;
    /// How many bytes of FBuffer are filled.
    FCount: IntegerRAL;
    /// What a failure raises.
    FErrorClass: ERALMailErrorClass;
    /// The host connected to (for the certificate check).
    FHost: StringRAL;
    /// The component that owns the link (sender of the certificate check).
    FOwner: TObject;
    /// Milliseconds a read may wait.
    FReadTimeout: IntegerRAL;
    /// The socket under the channel.
    FSocket: TRALSocket;
    /// The client's TLS settings.
    FSSL: TRALSocketSSL;
    /// The TLS context of the connection, when TLS is on.
    FTLSContext: TRALTLSContext;

    /// Raises AMessage as the protocol's error.
    procedure Fail(const AMessage: StringRAL);
    /// Reads more bytes into the buffer; raises on a close or a timeout.
    procedure Fill;
  public
    { A link raising AErrorClass, checking certificates by ASSL's rule with
      AOwner as the sender. }
    constructor Create(AOwner: TObject; ASSL: TRALSocketSSL; AErrorClass: ERALMailErrorClass);
    destructor Destroy; override;

    /// Closes the connection, without a word to the server.
    procedure Close;
    { Whether the server already closed an idle connection (something to read
      before any command was sent): a kept connection is then reopened. }
    function ClosedByServer: Boolean;
    /// Connects to AHost:APort within ATimeout milliseconds.
    procedure Connect(const AHost: StringRAL; APort, ATimeout: IntegerRAL);
    /// Whether a connection is open.
    function Connected: Boolean;
    { Whether something can be read within ATimeout milliseconds, without
      closing the connection when nothing comes (IMAP IDLE waits with it). }
    function Readable(ATimeout: IntegerRAL): Boolean;
    /// Reads ACount bytes into AStream (a literal of IMAP, for example).
    procedure ReadBytes(AStream: TStream; ACount: Int64);
    /// Reads one line, without its CRLF.
    function ReadLine: RawByteString;
    /// Starts TLS over the connection and checks the server's certificate.
    procedure StartTLS(ATimeout: IntegerRAL);
    /// The TLS version in use ('TLSv1.3'), or empty without TLS.
    function TLSVersion: StringRAL;
    /// Writes AText as it is.
    procedure Write(const AText: RawByteString); overload;
    /// Writes the rest of AStream.
    procedure Write(AStream: TStream); overload;

    /// Milliseconds a read waits before it fails.
    property ReadTimeout: IntegerRAL read FReadTimeout write FReadTimeout;
  end;

/// The SASL PLAIN initial response of AUser and APassword, in base64 (RFC 4616).
function RALSASLPlain(const AUser, APassword: StringRAL): StringRAL;
/// The XOAUTH2 initial response of AUser and the access token AToken, in base64.
function RALSASLXOAuth2(const AUser, AToken: StringRAL): StringRAL;
{ The OAUTHBEARER initial response (RFC 7628) of AUser and AToken for AHost:APort,
  in base64. }
function RALSASLOAuthBearer(const AUser, AToken, AHost: StringRAL; APort: IntegerRAL): StringRAL;

implementation

function RALSASLPlain(const AUser, APassword: StringRAL): StringRAL;
begin
  Result := TRALBase64.Encode(#0 + AUser + #0 + APassword);
end;

function RALSASLXOAuth2(const AUser, AToken: StringRAL): StringRAL;
begin
  Result := TRALBase64.Encode('user=' + AUser + #1'auth=Bearer ' + AToken + #1#1);
end;

function RALSASLOAuthBearer(const AUser, AToken, AHost: StringRAL; APort: IntegerRAL): StringRAL;
begin
  Result := TRALBase64.Encode('n,a=' + AUser + ','#1'host=' + AHost + #1'port=' +
    StringRAL(IntToStr(APort)) + #1'auth=Bearer ' + AToken + #1#1);
end;

{ TRALMailLink }

constructor TRALMailLink.Create(AOwner: TObject; ASSL: TRALSocketSSL;
  AErrorClass: ERALMailErrorClass);
begin
  inherited Create;
  FOwner := AOwner;
  FSSL := ASSL;
  FErrorClass := AErrorClass;
  FReadTimeout := 60000;
  SetLength(FBuffer, 16384);
end;

destructor TRALMailLink.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TRALMailLink.Fail(const AMessage: StringRAL);
begin
  raise FErrorClass.Create(string(AMessage));
end;

procedure TRALMailLink.Close;
begin
  // the TLS channel frees the plain one under it; the socket is the link's
  FreeAndNil(FChannel);
  FreeAndNil(FSocket);
  FreeAndNil(FTLSContext);
  FCount := 0;
end;

procedure TRALMailLink.Connect(const AHost: StringRAL; APort, ATimeout: IntegerRAL);
begin
  Close;
  FHost := AHost;
  FSocket := TRALSocket.Create;
  try
    FSocket.Connect(AHost, APort, ATimeout);
    FChannel := TRALSocketChannel.Create(FSocket, False);
  except
    Close;
    raise;
  end;
end;

function TRALMailLink.Connected: Boolean;
begin
  Result := FChannel <> nil;
end;

function TRALMailLink.ClosedByServer: Boolean;
begin
  Result := Connected and (FCount = 0) and not FChannel.HasBuffered and
    ((FSocket.WaitFor(RALPollIn, 0) and RALPollIn) <> 0);
end;

procedure TRALMailLink.StartTLS(ATimeout: IntegerRAL);
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
  // what was buffered in clear does not count after the handshake (RFC 3207 4.2, 2595 4)
  FCount := 0;
  vTLS := TRALTLSChannel.Create(FChannel, FTLSContext);
  FChannel := vTLS;
  vName := FSSL.ServerName;
  if vName = '' then
    vName := FHost;
  vTLS.Handshake(vName, ATimeout);
  vCert := vTLS.PeerCertificate;
  if not RALTLSAcceptCertificate(FOwner, FSSL, nil, vCert, FHost, FSocket.PeerPort) then
    Fail(StringRAL(Format(emMailCertRefused, [string(vCert.Error)])));
end;

function TRALMailLink.TLSVersion: StringRAL;
begin
  if FChannel is TRALTLSChannel then
    Result := TRALTLSChannel(FChannel).VersionName
  else
    Result := '';
end;

procedure TRALMailLink.Fill;
var
  vRead: IntegerRAL;
begin
  if FChannel = nil then
    Fail(emMailNotConnected);
  if FCount = Length(FBuffer) then
    SetLength(FBuffer, Length(FBuffer) * 2);
  vRead := FChannel.ReadWait(FBuffer[FCount], Length(FBuffer) - FCount, FReadTimeout);
  if vRead <= 0 then
  begin
    Close;
    if vRead = 0 then
      Fail(emMailClosed)
    else
      Fail(emMailTimeout);
  end;
  Inc(FCount, vRead);
end;

function TRALMailLink.ReadLine: RawByteString;
var
  vInt, vStart: IntegerRAL;
begin
  vStart := 0;
  repeat
    for vInt := vStart to FCount - 1 do
      if FBuffer[vInt] = 10 then
      begin
        SetLength(Result, vInt);
        if vInt > 0 then
          Move(FBuffer[0], Result[1], vInt);
        if (Result <> '') and (Result[Length(Result)] = #13) then
          SetLength(Result, Length(Result) - 1);
        Move(FBuffer[vInt + 1], FBuffer[0], FCount - vInt - 1);
        Dec(FCount, vInt + 1);
        Exit;
      end;
    vStart := FCount;
    Fill;
  until False;
end;

function TRALMailLink.Readable(ATimeout: IntegerRAL): Boolean;
begin
  Result := Connected and ((FCount > 0) or FChannel.HasBuffered or
    ((FSocket.WaitFor(RALPollIn, ATimeout) and RALPollIn) <> 0));
end;

procedure TRALMailLink.ReadBytes(AStream: TStream; ACount: Int64);
var
  vTake: IntegerRAL;
begin
  while ACount > 0 do
  begin
    if FCount = 0 then
      Fill;
    vTake := FCount;
    if vTake > ACount then
      vTake := IntegerRAL(ACount);
    if AStream <> nil then
      AStream.WriteBuffer(FBuffer[0], vTake);
    Move(FBuffer[vTake], FBuffer[0], FCount - vTake);
    Dec(FCount, vTake);
    Dec(ACount, vTake);
  end;
end;

procedure TRALMailLink.Write(const AText: RawByteString);
begin
  if FChannel = nil then
    Fail(emMailNotConnected);
  if AText <> '' then
    FChannel.WriteAll(AText[1], Length(AText), FReadTimeout);
end;

procedure TRALMailLink.Write(AStream: TStream);
var
  vBlock: TBytes;
  vRead: IntegerRAL;
begin
  if FChannel = nil then
    Fail(emMailNotConnected);
  SetLength(vBlock, 65536);
  repeat
    vRead := AStream.Read(vBlock[0], Length(vBlock));
    if vRead > 0 then
      FChannel.WriteAll(vBlock[0], vRead, FReadTimeout);
  until vRead <= 0;
end;

end.
