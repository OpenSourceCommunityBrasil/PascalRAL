{ The IMAP client: reads and manages a mailbox over IMAP4rev2 (RFC 9051), accepting
  IMAP4rev1 servers (RFC 3501), with UIDPLUS, MOVE, IDLE, LITERAL+ and SASL-IR. }
unit RALIMAPClient;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools, RALBase64, RALCustomObjects, RALExceptions, RALStream,
  RALClient, RALSocket, RALTLS, RALSCRAM, RALMailMIME, RALMailTransport, RALSMTPClient;

type
  /// An IMAP failure: a negative or bad-command answer, or the connection.
  ERALIMAPError = class(ERALException)
  private
    FStatus: StringRAL;
  public
    /// The error of a tagged answer with status AStatus and text AText.
    constructor CreateStatus(const AStatus, AText: StringRAL);

    /// The answer status, or empty for a connection failure.
    property Status: StringRAL read FStatus;
  end;

  /// A message as a listing shows it: what FetchSummaries reads.
  TRALIMAPSummary = class
  public
    /// The Date header.
    Date: TDateTime;
    /// The flags (\Seen, \Flagged, keywords...), space separated.
    Flags: StringRAL;
    /// The From header, decoded.
    From: StringRAL;
    /// The size in bytes.
    Size: Int64;
    /// The Subject header, decoded.
    Subject: StringRAL;
    /// The unique identifier in the mailbox.
    UID: Cardinal;
  end;

  /// A line of the conversation: AOutgoing for what the client sent (secrets masked).
  TRALIMAPLogEvent = procedure(Sender: TObject; const ALine: StringRAL; AOutgoing: Boolean) of object;

  { Reads a mailbox over IMAP on the calling thread. Every message is addressed by
    its UID in the selected mailbox (stable across sessions while UIDValidity holds).
    One client per thread. }
  TRALIMAPClient = class(TRALComponent)
  private
    FAccessToken: StringRAL;
    FAuthMethod: TRALMailAuthMethod;
    FCapabilities: TStringList;
    FConnectTimeout: IntegerRAL;
    /// Cached mailbox hierarchy delimiter.
    FDelimiter: StringRAL;
    FExists: IntegerRAL;
    FHost: StringRAL;
    /// The connection with its line reader.
    FLink: TRALMailLink;
    /// The literals of the answer being read.
    FLiterals: TStringList;
    /// Where the next literal is written instead of FLiterals, or nil.
    FLiteralTarget: TStream;
    FOnGetAccessToken: TRALSMTPTokenEvent;
    FOnLog: TRALIMAPLogEvent;
    FPassword: StringRAL;
    FPort: IntegerRAL;
    FReadTimeout: IntegerRAL;
    FRecent: IntegerRAL;
    /// The untagged lines received since the last tagged answer.
    FReplies: TStringList;
    FSecurity: TRALMailSecurity;
    FSelected: StringRAL;
    FSSL: TRALSocketSSL;
    /// Counter behind the command tags.
    FTag: IntegerRAL;
    FUIDNext: Cardinal;
    FUIDValidity: Cardinal;
    FUserName: StringRAL;
    /// Whether UTF8=ACCEPT is enabled.
    FUTF8: Boolean;

    /// Logs in with the configured method.
    procedure Authenticate;
    /// Sends ACommand and returns the tagged answer.
    function Command(const ACommand: RawByteString; ASecret: Boolean = False): RawByteString;
    /// FPort, or the default of the security mode when 0.
    function EffectivePort: IntegerRAL;
    /// Passes ALine to OnLog.
    procedure Log(const ALine: RawByteString; AOutgoing: Boolean);
    /// AName as a command argument, quoted or encoded.
    function MailboxArg(const AName: StringRAL): RawByteString;
    /// A new unique command tag.
    function NextTag: RawByteString;
    /// Records the state an untagged line announces.
    procedure ParseUntagged(const ALine: RawByteString);
    /// Reads one answer line, collecting its literals.
    function ReadResponse: RawByteString;
    /// Asks for the server capabilities.
    procedure RequestCapabilities;
    /// Runs the SASL exchange of AMechanism, starting with AInitial.
    procedure SaslExchange(const AMechanism, AInitial: RawByteString);
    procedure SetSSL(AValue: TRALSocketSSL);
    /// The UID set argument for AUIDs.
    function UIDList(const AUIDs: array of Cardinal): RawByteString;
    /// Reads answers until the one tagged ATag; raises on failure when ARaise.
    function WaitTagged(const ATag: RawByteString; ARaise: Boolean = True): RawByteString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Appends AMessage to AMailbox with AFlags ('\Seen', for example); the new UID, or 0
      when the server does not say. }
    function Append(const AMailbox: StringRAL; AMessage: TRALMailMessage;
      const AFlags: StringRAL = ''): Cardinal;
    /// Appends the bytes of AData (a whole message) to AMailbox.
    function AppendRaw(const AMailbox: StringRAL; AData: TStream; const AFlags: StringRAL = ''): Cardinal;
    /// Connects, protects and logs in.
    procedure Connect;
    /// Whether a connection is open.
    function Connected: Boolean;
    /// Copies the messages to ADestination.
    procedure Copy(const AUIDs: array of Cardinal; const ADestination: StringRAL);
    /// Creates a mailbox.
    procedure CreateMailbox(const AName: StringRAL);
    /// Marks the messages \Deleted and removes them now (only them, with UIDPLUS).
    procedure Delete(const AUIDs: array of Cardinal);
    /// Deletes a mailbox.
    procedure DeleteMailbox(const AName: StringRAL);
    { The character that separates the levels of mailbox names on this server
      ('/' on Gmail, '.' on a default Dovecot); empty when the server has no levels. }
    function Delimiter: StringRAL;
    /// Logs out and closes the connection.
    procedure Disconnect;
    /// Removes every message marked \Deleted in the selected mailbox.
    procedure Expunge;
    /// Reads message AUID whole into AMessage; APeek leaves \Seen alone.
    procedure FetchMessage(AUID: Cardinal; AMessage: TRALMailMessage; APeek: Boolean = True);
    /// Writes message AUID, as the server keeps it, to AStream.
    procedure FetchRaw(AUID: Cardinal; AStream: TStream; APeek: Boolean = True);
    { Adds a TRALIMAPSummary (the caller frees it) to AList for each of AUIDs, or for
      every message when AUIDs is empty. }
    procedure FetchSummaries(const AUIDs: array of Cardinal; AList: TList);
    /// Whether the server announced ACapability (IDLE, MOVE, UIDPLUS...).
    function HasCapability(const ACapability: StringRAL): Boolean;
    /// The mailboxes whose names match APattern ('*' every one, '%' one level).
    procedure ListMailboxes(AList: TStrings; const APattern: StringRAL = '*');
    /// Moves the messages to ADestination (MOVE, or copy and delete without it).
    procedure Move(const AUIDs: array of Cardinal; const ADestination: StringRAL);
    /// Keeps the connection alive and reads what changed.
    procedure Noop;
    /// Renames a mailbox.
    procedure RenameMailbox(const AName, ANewName: StringRAL);
    { The UIDs of the messages matching ACriteria, an IMAP search key (everything,
      unseen, 'FROM "ana"'...), in AList as numbers. }
    procedure Search(const ACriteria: StringRAL; AList: TStrings);
    /// Opens AMailbox; AReadOnly opens it with EXAMINE.
    procedure Select(const AMailbox: StringRAL; AReadOnly: Boolean = False);
    /// Adds (AAdd) or removes AFlags ('\Seen \Flagged') on the messages.
    procedure SetFlags(const AUIDs: array of Cardinal; const AFlags: StringRAL; AAdd: Boolean = True);
    /// The number of messages and of unseen ones in AMailbox, without selecting it.
    procedure Status(const AMailbox: StringRAL; out AMessages, AUnseen: IntegerRAL);
    { Waits up to ATimeout milliseconds for the selected mailbox to change (IDLE):
      True when a message arrived, left or changed flags; Exists has the new count. }
    function WaitForChanges(ATimeout: IntegerRAL): Boolean;

    /// What the server announced (upper case).
    property Capabilities: TStringList read FCapabilities;
    /// Messages in the selected mailbox.
    property Exists: IntegerRAL read FExists;
    /// Messages new to this session in the selected mailbox.
    property Recent: IntegerRAL read FRecent;
    /// The selected mailbox.
    property Selected: StringRAL read FSelected;
    /// The UID the next message of the selected mailbox will get.
    property UIDNext: Cardinal read FUIDNext;
    /// Changes when the UIDs of the selected mailbox stop naming the same messages.
    property UIDValidity: Cardinal read FUIDValidity;
  published
    /// The OAuth2 access token of XOAUTH2 and OAUTHBEARER (or OnGetAccessToken).
    property AccessToken: StringRAL read FAccessToken write FAccessToken;
    /// The login; maAuto picks the strongest the server offers.
    property AuthMethod: TRALMailAuthMethod read FAuthMethod write FAuthMethod default maAuto;
    /// Milliseconds to connect (and finish the TLS handshake).
    property ConnectTimeout: IntegerRAL read FConnectTimeout write FConnectTimeout default 30000;
    /// The server.
    property Host: StringRAL read FHost write FHost;
    /// Called before a token login, to give a fresh token.
    property OnGetAccessToken: TRALSMTPTokenEvent read FOnGetAccessToken write FOnGetAccessToken;
    /// The conversation, with secrets masked.
    property OnLog: TRALIMAPLogEvent read FOnLog write FOnLog;
    /// The password.
    property Password: StringRAL read FPassword write FPassword;
    /// The port; 0 takes 993 for msImplicitTLS and 143 otherwise.
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

/// AName in the modified UTF-7 of IMAP mailbox names (RFC 3501 5.1.3).
function RALIMAPEncodeUTF7(const AName: StringRAL): RawByteString;
/// A mailbox name in modified UTF-7 back to text.
function RALIMAPDecodeUTF7(const AName: RawByteString): StringRAL;

implementation

const
  /// Surrounds the index of a literal in an answer line.
  cLiteralMark = #0;

type
  /// The kinds of token of an IMAP answer.
  TRALIMAPToken = (itEnd, itAtom, itString, itNil, itOpen, itClose);

  { Reads the tokens of one answer line; literals were replaced by a mark and an
    index into the client's list. }
  TRALIMAPScanner = class
  private
    /// The line being scanned.
    FLine: RawByteString;
    /// The literals the line refers to.
    FLiterals: TStringList;
    /// The next character to read.
    FPos: IntegerRAL;
  public
    /// Starts scanning ALine; ALiterals holds its literals.
    constructor Create(const ALine: RawByteString; ALiterals: TStringList);

    /// The next token, its text in AText.
    function Next(out AText: RawByteString): TRALIMAPToken;
    /// Skips a parenthesized list whose "(" was just read.
    procedure SkipList;
  end;

constructor TRALIMAPScanner.Create(const ALine: RawByteString; ALiterals: TStringList);
begin
  inherited Create;
  FLine := ALine;
  FLiterals := ALiterals;
  FPos := 1;
end;

function TRALIMAPScanner.Next(out AText: RawByteString): TRALIMAPToken;
var
  vStart, vDepth: IntegerRAL;
begin
  AText := '';
  while (FPos <= Length(FLine)) and (FLine[FPos] = ' ') do
    Inc(FPos);
  if FPos > Length(FLine) then
  begin
    Result := itEnd;
    Exit;
  end;
  case FLine[FPos] of
    '(':
    begin
      Inc(FPos);
      Result := itOpen;
    end;
    ')':
    begin
      Inc(FPos);
      Result := itClose;
    end;
    '"':
    begin
      Inc(FPos);
      while (FPos <= Length(FLine)) and (FLine[FPos] <> '"') do
      begin
        if (FLine[FPos] = '\') and (FPos < Length(FLine)) then
          Inc(FPos);
        AText := AText + FLine[FPos];
        Inc(FPos);
      end;
      Inc(FPos);
      Result := itString;
    end;
    cLiteralMark:
    begin
      vStart := FPos + 1;
      FPos := vStart;
      while (FPos <= Length(FLine)) and (FLine[FPos] <> cLiteralMark) do
        Inc(FPos);
      AText := RawByteString(FLiterals[StrToIntDef(string(Copy(FLine, vStart, FPos - vStart)), 0)]);
      Inc(FPos);
      Result := itString;
    end;
  else
    // an atom; a section in brackets (BODY[HEADER.FIELDS (FROM)]) belongs to it
    vStart := FPos;
    vDepth := 0;
    while FPos <= Length(FLine) do
    begin
      if FLine[FPos] = '[' then
        Inc(vDepth)
      else if FLine[FPos] = ']' then
        Dec(vDepth)
      else if (vDepth = 0) and (FLine[FPos] in [' ', '(', ')']) then
        Break;
      Inc(FPos);
    end;
    AText := Copy(FLine, vStart, FPos - vStart);
    if UpperCase(string(AText)) = 'NIL' then
      Result := itNil
    else
      Result := itAtom;
  end;
end;

procedure TRALIMAPScanner.SkipList;
var
  vDepth: IntegerRAL;
  vText: RawByteString;
  vToken: TRALIMAPToken;
begin
  vDepth := 1;
  repeat
    vToken := Next(vText);
    if vToken = itOpen then
      Inc(vDepth)
    else if vToken = itClose then
      Dec(vDepth);
  until (vDepth = 0) or (vToken = itEnd);
end;

{ modified UTF-7 }

function RALIMAPEncodeUTF7(const AName: StringRAL): RawByteString;
var
  vWide: UnicodeString;
  vInt, vBits, vValue: IntegerRAL;
  vInShift: Boolean;
  vCode: Word;
const
  cB64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,';

  procedure Flush;
  begin
    if vBits > 0 then
      Result := Result + AnsiChar(cB64[((vValue shl (6 - vBits)) and $3F) + 1]);
    Result := Result + '-';
    vBits := 0;
    vValue := 0;
    vInShift := False;
  end;

begin
  vWide := UnicodeString(AName);
  Result := '';
  vInShift := False;
  vBits := 0;
  vValue := 0;
  for vInt := 1 to Length(vWide) do
  begin
    vCode := Ord(vWide[vInt]);
    if (vCode >= $20) and (vCode <= $7E) then
    begin
      if vInShift then
        Flush;
      if vCode = Ord('&') then
        Result := Result + '&-'
      else
        Result := Result + AnsiChar(vCode);
    end
    else
    begin
      if not vInShift then
      begin
        Result := Result + '&';
        vInShift := True;
      end;
      vValue := (vValue shl 16) or vCode;
      Inc(vBits, 16);
      while vBits >= 6 do
      begin
        Dec(vBits, 6);
        Result := Result + AnsiChar(cB64[((vValue shr vBits) and $3F) + 1]);
      end;
      vValue := vValue and ((1 shl vBits) - 1);
    end;
  end;
  if vInShift then
    Flush;
end;

function RALIMAPDecodeUTF7(const AName: RawByteString): StringRAL;
var
  vWide: UnicodeString;
  vInt, vBits, vValue, vDigit: IntegerRAL;
begin
  vWide := '';
  vInt := 1;
  while vInt <= Length(AName) do
  begin
    if AName[vInt] <> '&' then
    begin
      vWide := vWide + WideChar(Ord(AName[vInt]));
      Inc(vInt);
      Continue;
    end;
    Inc(vInt);
    if (vInt <= Length(AName)) and (AName[vInt] = '-') then
    begin
      vWide := vWide + '&';
      Inc(vInt);
      Continue;
    end;
    vBits := 0;
    vValue := 0;
    while (vInt <= Length(AName)) and (AName[vInt] <> '-') do
    begin
      case AName[vInt] of
        'A'..'Z': vDigit := Ord(AName[vInt]) - Ord('A');
        'a'..'z': vDigit := Ord(AName[vInt]) - Ord('a') + 26;
        '0'..'9': vDigit := Ord(AName[vInt]) - Ord('0') + 52;
        '+': vDigit := 62;
        ',': vDigit := 63;
      else
        vDigit := 0;
      end;
      vValue := (vValue shl 6) or vDigit;
      Inc(vBits, 6);
      if vBits >= 16 then
      begin
        Dec(vBits, 16);
        vWide := vWide + WideChar((vValue shr vBits) and $FFFF);
        vValue := vValue and ((1 shl vBits) - 1);
      end;
      Inc(vInt);
    end;
    Inc(vInt);
  end;
  Result := StringRAL(vWide);
end;

/// AText as an IMAP quoted string.
function Quote(const AText: RawByteString): RawByteString;
var
  vInt: IntegerRAL;
begin
  Result := '"';
  for vInt := 1 to Length(AText) do
  begin
    if AText[vInt] in ['"', '\'] then
      Result := Result + '\';
    Result := Result + AText[vInt];
  end;
  Result := Result + '"';
end;

/// Whether AText cannot go as a quoted string (CR, LF, NUL or 8-bit bytes).
function NeedsLiteral(const AText: RawByteString): Boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 1 to Length(AText) do
    if (AText[vInt] in [#0, #10, #13]) or (Ord(AText[vInt]) > 127) then
    begin
      Result := True;
      Exit;
    end;
end;

{ ERALIMAPError }

constructor ERALIMAPError.CreateStatus(const AStatus, AText: StringRAL);
begin
  inherited Create(Format(emIMAPAnswer, [string(AStatus), string(AText)]));
  FStatus := AStatus;
end;

{ TRALIMAPClient }

constructor TRALIMAPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSSL := TRALSocketSSL.Create;
  FCapabilities := TStringList.Create;
  FReplies := TStringList.Create;
  FLiterals := TStringList.Create;
  FConnectTimeout := 30000;
  FReadTimeout := 60000;
  FSecurity := msStartTLSIfOffered;
  FAuthMethod := maAuto;
end;

destructor TRALIMAPClient.Destroy;
begin
  if Connected then
    try
      Disconnect;
    except
      // a dead connection must not stop the destructor
    end;
  FreeAndNil(FLink);
  FLiterals.Free;
  FReplies.Free;
  FCapabilities.Free;
  FSSL.Free;
  inherited Destroy;
end;

procedure TRALIMAPClient.SetSSL(AValue: TRALSocketSSL);
begin
  FSSL.Assign(AValue);
end;

function TRALIMAPClient.EffectivePort: IntegerRAL;
begin
  Result := FPort;
  if Result = 0 then
  begin
    if FSecurity = msImplicitTLS then
      Result := 993
    else
      Result := 143;
  end;
end;

procedure TRALIMAPClient.Log(const ALine: RawByteString; AOutgoing: Boolean);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, StringRAL(ALine), AOutgoing);
end;

function TRALIMAPClient.NextTag: RawByteString;
begin
  Inc(FTag);
  Result := RawByteString('A' + Format('%.4d', [FTag]));
end;

function TRALIMAPClient.MailboxArg(const AName: StringRAL): RawByteString;
begin
  // UTF8=ACCEPT lets names go as UTF-8; otherwise modified UTF-7
  if FUTF8 then
    Result := Quote(RawByteString(AName))
  else
    Result := Quote(RALIMAPEncodeUTF7(AName));
end;

function TRALIMAPClient.ReadResponse: RawByteString;
var
  vLine: RawByteString;
  vOpen, vSize: IntegerRAL;
  vSizeText: string;
  vData: TStringStream;
  vToStream: Boolean;
begin
  Result := '';
  repeat
    vLine := FLink.ReadLine;
    Log(vLine, False);
    // a literal: {n} (or {n+}) at the end of the line, then n bytes, then the rest
    if (vLine <> '') and (vLine[Length(vLine)] = '}') then
    begin
      vOpen := LastDelimiter('{', string(vLine));
      vSizeText := StringReplace(string(System.Copy(vLine, vOpen + 1, Length(vLine) - vOpen - 1)), '+', '', []);
      vSize := StrToIntDef(vSizeText, -1);
      if (vOpen > 0) and (vSize >= 0) then
      begin
        Result := Result + System.Copy(vLine, 1, vOpen - 1);
        // the body of a FETCH goes to the caller's stream when there is one
        vToStream := (FLiteralTarget <> nil) and
          (Pos('BODY[', UpperCase(string(System.Copy(vLine, 1, vOpen - 1)))) > 0);
        if vToStream then
        begin
          FLink.ReadBytes(FLiteralTarget, vSize);
          FLiterals.Add('');
        end
        else
        begin
          vData := TStringStream.Create('');
          try
            FLink.ReadBytes(vData, vSize);
            FLiterals.Add(vData.DataString);
          finally
            vData.Free;
          end;
        end;
        Result := Result + cLiteralMark + RawByteString(IntToStr(FLiterals.Count - 1)) + cLiteralMark;
        Continue;
      end;
    end;
    Result := Result + vLine;
    Break;
  until False;
end;

procedure TRALIMAPClient.ParseUntagged(const ALine: RawByteString);
var
  vScan: TRALIMAPScanner;
  vFirst, vSecond, vText: RawByteString;
  vCode: string;
  vPos: IntegerRAL;
begin
  vScan := TRALIMAPScanner.Create(ALine, FLiterals);
  try
    vScan.Next(vText); // the *
    vScan.Next(vFirst);
    vScan.Next(vSecond);
    if UpperCase(string(vFirst)) = 'CAPABILITY' then
    begin
      FCapabilities.Clear;
      FCapabilities.Add(UpperCase(string(vSecond)));
      while vScan.Next(vText) = itAtom do
        FCapabilities.Add(UpperCase(string(vText)));
    end
    else if UpperCase(string(vSecond)) = 'EXISTS' then
      FExists := StrToIntDef(string(vFirst), FExists)
    else if UpperCase(string(vSecond)) = 'RECENT' then
      FRecent := StrToIntDef(string(vFirst), FRecent)
    else if UpperCase(string(vSecond)) = 'EXPUNGE' then
      Dec(FExists);
  finally
    vScan.Free;
  end;
  // response codes: [UIDVALIDITY n], [UIDNEXT n], [CAPABILITY ...]
  vPos := RALMailPos('[', ALine);
  if vPos > 0 then
  begin
    vCode := UpperCase(string(System.Copy(ALine, vPos + 1, MaxInt)));
    if System.Copy(vCode, 1, 12) = 'UIDVALIDITY ' then
      FUIDValidity := StrToInt64Def(System.Copy(vCode, 13, Pos(']', vCode) - 13), 0)
    else if System.Copy(vCode, 1, 8) = 'UIDNEXT ' then
      FUIDNext := StrToInt64Def(System.Copy(vCode, 9, Pos(']', vCode) - 9), 0)
    else if System.Copy(vCode, 1, 11) = 'CAPABILITY ' then
    begin
      FCapabilities.Delimiter := ' ';
      FCapabilities.DelimitedText := System.Copy(vCode, 12, Pos(']', vCode) - 12);
    end;
  end;
end;

function TRALIMAPClient.WaitTagged(const ATag: RawByteString; ARaise: Boolean): RawByteString;
var
  vLine: RawByteString;
  vStatus: string;
  vRest: RawByteString;
begin
  repeat
    vLine := ReadResponse;
    if System.Copy(vLine, 1, 2) = '* ' then
    begin
      FReplies.Add(string(vLine));
      ParseUntagged(vLine);
      Continue;
    end;
    if System.Copy(vLine, 1, Length(ATag) + 1) = ATag + ' ' then
    begin
      vRest := System.Copy(vLine, Length(ATag) + 2, MaxInt);
      vStatus := UpperCase(string(System.Copy(vRest, 1, RALMailPos(' ', vRest + ' ') - 1)));
      Result := vRest;
      if vStatus <> 'OK' then
      begin
        // a tagged answer may still carry a response code ([CAPABILITY ...])
        ParseUntagged('* ' + vRest);
        if ARaise then
          raise ERALIMAPError.CreateStatus(StringRAL(vStatus),
            StringRAL(System.Copy(vRest, Length(vStatus) + 2, MaxInt)));
      end
      else
        ParseUntagged('* ' + vRest);
      Exit;
    end;
    // a continuation (+) out of place, or another tag: kept for the caller
    FReplies.Add(string(vLine));
  until False;
end;

function TRALIMAPClient.Command(const ACommand: RawByteString; ASecret: Boolean): RawByteString;
var
  vTag: RawByteString;
begin
  if not Connected then
    Connect;
  FReplies.Clear;
  FLiterals.Clear;
  vTag := NextTag;
  if ASecret then
    Log(vTag + ' ' + System.Copy(ACommand, 1, RALMailPos(' ', ACommand + ' ')) + '****', True)
  else
    Log(vTag + ' ' + ACommand, True);
  FLink.Write(vTag + ' ' + ACommand + #13#10);
  Result := WaitTagged(vTag);
end;

procedure TRALIMAPClient.RequestCapabilities;
begin
  Command('CAPABILITY');
end;

function TRALIMAPClient.HasCapability(const ACapability: StringRAL): Boolean;
begin
  Result := FCapabilities.IndexOf(UpperCase(string(ACapability))) >= 0;
end;

procedure TRALIMAPClient.Connect;
var
  vGreeting: RawByteString;
begin
  if Connected then
    Exit;
  if FLink = nil then
    FLink := TRALMailLink.Create(Self, FSSL, ERALIMAPError);
  FLink.ReadTimeout := FReadTimeout;
  FCapabilities.Clear;
  FUTF8 := False;
  FSelected := '';
  FDelimiter := #0;
  try
    FLink.Connect(FHost, EffectivePort, FConnectTimeout);
    if FSecurity = msImplicitTLS then
      FLink.StartTLS(FConnectTimeout);
    FLiterals.Clear;
    vGreeting := ReadResponse;
    if System.Copy(vGreeting, 1, 4) = '* BY' then
      raise ERALIMAPError.CreateStatus('BYE', StringRAL(System.Copy(vGreeting, 7, MaxInt)));
    ParseUntagged(vGreeting);
    if FCapabilities.Count = 0 then
      RequestCapabilities;
    if (FSecurity in [msStartTLS, msStartTLSIfOffered]) then
    begin
      if HasCapability('STARTTLS') then
      begin
        Command('STARTTLS');
        FLink.StartTLS(FConnectTimeout);
        // what the server said before TLS does not count (RFC 9051 6.2.1)
        RequestCapabilities;
      end
      else if FSecurity = msStartTLS then
        raise ERALIMAPError.CreateStatus('', emMailNoStartTLS);
    end;
    // a PREAUTH greeting is already logged in
    if System.Copy(vGreeting, 1, 9) <> '* PREAUTH' then
      Authenticate;
    if HasCapability('IMAP4REV2') or HasCapability('UTF8=ACCEPT') then
    begin
      if HasCapability('UTF8=ACCEPT') then
        Command('ENABLE UTF8=ACCEPT')
      else
        Command('ENABLE IMAP4rev2');
      FUTF8 := HasCapability('UTF8=ACCEPT') or HasCapability('IMAP4REV2');
    end;
  except
    FLink.Close;
    raise;
  end;
end;

procedure TRALIMAPClient.SaslExchange(const AMechanism, AInitial: RawByteString);
var
  vTag, vLine: RawByteString;
begin
  FReplies.Clear;
  FLiterals.Clear;
  vTag := NextTag;
  if (AInitial <> '') and HasCapability('SASL-IR') then
  begin
    Log(vTag + ' AUTHENTICATE ' + AMechanism + ' ****', True);
    FLink.Write(vTag + ' AUTHENTICATE ' + AMechanism + ' ' + AInitial + #13#10);
  end
  else
  begin
    Log(vTag + ' AUTHENTICATE ' + AMechanism, True);
    FLink.Write(vTag + ' AUTHENTICATE ' + AMechanism + #13#10);
    vLine := ReadResponse;
    if System.Copy(vLine, 1, 1) <> '+' then
      raise ERALIMAPError.CreateStatus('NO', StringRAL(vLine));
    Log('****', True);
    if AInitial = '' then
      FLink.Write('=' + #13#10)
    else
      FLink.Write(AInitial + #13#10);
  end;
  // a challenge after the response is an error report (XOAUTH2): answer empty,
  // then the failure
  vLine := ReadResponse;
  if System.Copy(vLine, 1, 1) = '+' then
  begin
    FLink.Write(#13#10);
    vLine := ReadResponse;
  end;
  if System.Copy(vLine, 1, Length(vTag) + 4) <> vTag + ' OK ' then
  begin
    if System.Copy(vLine, 1, Length(vTag) + 1) = vTag + ' ' then
      raise ERALIMAPError.CreateStatus('NO', StringRAL(System.Copy(vLine, Length(vTag) + 2, MaxInt)));
    raise ERALIMAPError.CreateStatus('NO', StringRAL(vLine));
  end;
  ParseUntagged('* ' + System.Copy(vLine, Length(vTag) + 2, MaxInt));
end;

procedure TRALIMAPClient.Authenticate;
var
  vMethod: TRALMailAuthMethod;
  vToken: StringRAL;
  vScram: TRALSCRAMClient;
  vTag, vLine: RawByteString;
begin
  vMethod := FAuthMethod;
  if vMethod = maNone then
    Exit;
  vToken := FAccessToken;
  if Assigned(FOnGetAccessToken) and (vMethod in [maAuto, maXOAuth2, maOAuthBearer]) then
    FOnGetAccessToken(Self, vToken);
  if (FUserName = '') and (vToken = '') then
    Exit;
  if vMethod = maAuto then
  begin
    if (vToken <> '') and HasCapability('AUTH=XOAUTH2') then
      vMethod := maXOAuth2
    else if (vToken <> '') and HasCapability('AUTH=OAUTHBEARER') then
      vMethod := maOAuthBearer
    else if HasCapability('AUTH=SCRAM-SHA-256') then
      vMethod := maScramSHA256
    else if HasCapability('AUTH=PLAIN') then
      vMethod := maPlain
    else
      vMethod := maLogin;
  end;
  // LOGIN takes only quoted ASCII: other text goes by PLAIN when the server has it
  if (vMethod = maLogin) and (NeedsLiteral(RawByteString(FUserName)) or
     NeedsLiteral(RawByteString(FPassword))) and HasCapability('AUTH=PLAIN') then
    vMethod := maPlain;
  case vMethod of
    maPlain:
      SaslExchange('PLAIN', RawByteString(RALSASLPlain(FUserName, FPassword)));
    maXOAuth2:
      SaslExchange('XOAUTH2', RawByteString(RALSASLXOAuth2(FUserName, vToken)));
    maOAuthBearer:
      SaslExchange('OAUTHBEARER', RawByteString(RALSASLOAuthBearer(FUserName, vToken, FHost,
        EffectivePort)));
    maScramSHA256, maScramSHA1:
    begin
      if vMethod = maScramSHA256 then
        vScram := TRALSCRAMClient.Create(rsaSHA256, FUserName, FPassword)
      else
        vScram := TRALSCRAMClient.Create(rsaSHA1, FUserName, FPassword);
      try
        vTag := NextTag;
        if vMethod = maScramSHA256 then
          vLine := 'SCRAM-SHA-256'
        else
          vLine := 'SCRAM-SHA-1';
        Log(vTag + ' AUTHENTICATE ' + vLine, True);
        FLink.Write(vTag + ' AUTHENTICATE ' + vLine + #13#10);
        vLine := ReadResponse;
        if System.Copy(vLine, 1, 1) <> '+' then
          raise ERALIMAPError.CreateStatus('NO', StringRAL(vLine));
        FLink.Write(RawByteString(TRALBase64.Encode(vScram.ClientFirst)) + #13#10);
        vLine := ReadResponse;
        if System.Copy(vLine, 1, 1) <> '+' then
          raise ERALIMAPError.CreateStatus('NO', StringRAL(vLine));
        FLink.Write(RawByteString(TRALBase64.Encode(vScram.ClientFinal(
          TRALBase64.Decode(StringRAL(Trim(string(System.Copy(vLine, 2, MaxInt)))))))) + #13#10);
        vLine := ReadResponse;
        if System.Copy(vLine, 1, 1) = '+' then
        begin
          if not vScram.CheckServerFinal(TRALBase64.Decode(StringRAL(Trim(string(System.Copy(vLine, 2, MaxInt)))))) then
            raise ERALIMAPError.CreateStatus('NO', emMailServerProof);
          FLink.Write(#13#10);
          vLine := ReadResponse;
        end;
        if System.Copy(vLine, 1, Length(vTag) + 4) <> vTag + ' OK ' then
          raise ERALIMAPError.CreateStatus('NO', StringRAL(vLine));
        ParseUntagged('* ' + System.Copy(vLine, Length(vTag) + 2, MaxInt));
      finally
        vScram.Free;
      end;
    end;
  else
    if HasCapability('LOGINDISABLED') then
      raise ERALIMAPError.CreateStatus('NO', emMailNoAuth);
    Command('LOGIN ' + Quote(RawByteString(FUserName)) + ' ' + Quote(RawByteString(FPassword)), True);
  end;
  // the capabilities may change after login
  RequestCapabilities;
end;

function TRALIMAPClient.Connected: Boolean;
begin
  Result := (FLink <> nil) and FLink.Connected;
end;

procedure TRALIMAPClient.Disconnect;
begin
  if not Connected then
    Exit;
  try
    Command('LOGOUT');
  finally
    FLink.Close;
    FSelected := '';
  end;
end;

function TRALIMAPClient.Delimiter: StringRAL;
var
  vInt: IntegerRAL;
  vScan: TRALIMAPScanner;
  vText: RawByteString;
begin
  if not Connected then
    Connect;
  if FDelimiter = #0 then
  begin
    // LIST "" "" answers the root and the delimiter (RFC 9051 6.3.9)
    FDelimiter := '';
    Command('LIST "" ""');
    for vInt := 0 to FReplies.Count - 1 do
      if UpperCase(System.Copy(FReplies[vInt], 1, 7)) = '* LIST ' then
      begin
        vScan := TRALIMAPScanner.Create(RawByteString(FReplies[vInt]), FLiterals);
        try
          vScan.Next(vText);
          vScan.Next(vText);
          if vScan.Next(vText) = itOpen then
            vScan.SkipList;
          if vScan.Next(vText) = itString then
            FDelimiter := StringRAL(vText);
        finally
          vScan.Free;
        end;
      end;
  end;
  Result := FDelimiter;
end;

procedure TRALIMAPClient.Noop;
begin
  Command('NOOP');
end;

procedure TRALIMAPClient.ListMailboxes(AList: TStrings; const APattern: StringRAL);
var
  vInt: IntegerRAL;
  vScan: TRALIMAPScanner;
  vText, vName: RawByteString;
  vToken: TRALIMAPToken;
begin
  AList.Clear;
  Command('LIST "" ' + Quote(RawByteString(APattern)));
  for vInt := 0 to FReplies.Count - 1 do
  begin
    if UpperCase(System.Copy(FReplies[vInt], 1, 7)) <> '* LIST ' then
      Continue;
    vScan := TRALIMAPScanner.Create(RawByteString(FReplies[vInt]), FLiterals);
    try
      vScan.Next(vText);
      vScan.Next(vText);
      // (flags) "delimiter" name
      if vScan.Next(vText) = itOpen then
        vScan.SkipList;
      vScan.Next(vText);
      vToken := vScan.Next(vName);
      if vToken in [itAtom, itString] then
      begin
        if FUTF8 then
          AList.Add(string(StringRAL(vName)))
        else
          AList.Add(string(RALIMAPDecodeUTF7(vName)));
      end;
    finally
      vScan.Free;
    end;
  end;
end;

procedure TRALIMAPClient.Select(const AMailbox: StringRAL; AReadOnly: Boolean);
begin
  FExists := 0;
  FRecent := 0;
  FUIDValidity := 0;
  FUIDNext := 0;
  if AReadOnly then
    Command('EXAMINE ' + MailboxArg(AMailbox))
  else
    Command('SELECT ' + MailboxArg(AMailbox));
  FSelected := AMailbox;
end;

procedure TRALIMAPClient.Status(const AMailbox: StringRAL; out AMessages, AUnseen: IntegerRAL);
var
  vInt: IntegerRAL;
  vScan: TRALIMAPScanner;
  vText, vName: RawByteString;
  vToken: TRALIMAPToken;
begin
  AMessages := 0;
  AUnseen := 0;
  Command('STATUS ' + MailboxArg(AMailbox) + ' (MESSAGES UNSEEN)');
  for vInt := 0 to FReplies.Count - 1 do
  begin
    if UpperCase(System.Copy(FReplies[vInt], 1, 9)) <> '* STATUS ' then
      Continue;
    vScan := TRALIMAPScanner.Create(RawByteString(FReplies[vInt]), FLiterals);
    try
      vScan.Next(vText);
      vScan.Next(vText);
      vScan.Next(vText); // the name
      if vScan.Next(vText) <> itOpen then
        Continue;
      repeat
        vToken := vScan.Next(vName);
        if vToken <> itAtom then
          Break;
        vScan.Next(vText);
        if UpperCase(string(vName)) = 'MESSAGES' then
          AMessages := StrToIntDef(string(vText), 0)
        else if UpperCase(string(vName)) = 'UNSEEN' then
          AUnseen := StrToIntDef(string(vText), 0);
      until False;
    finally
      vScan.Free;
    end;
  end;
end;

procedure TRALIMAPClient.Search(const ACriteria: StringRAL; AList: TStrings);
var
  vInt, vNum: IntegerRAL;
  vParts: TStringList;
  vPart: IntegerRAL;
  vLine: string;
begin
  AList.Clear;
  Command('UID SEARCH ' + RawByteString(ACriteria));
  vParts := TStringList.Create;
  try
    for vInt := 0 to FReplies.Count - 1 do
    begin
      vLine := FReplies[vInt];
      // * SEARCH 1 2 3 (rev1), or * ESEARCH (TAG "A1") UID with a set 1:3,5 (rev2)
      if UpperCase(System.Copy(vLine, 1, 9)) = '* SEARCH ' then
      begin
        vParts.Delimiter := ' ';
        vParts.DelimitedText := Trim(System.Copy(vLine, 10, MaxInt));
        for vPart := 0 to vParts.Count - 1 do
          if StrToInt64Def(vParts[vPart], -1) > 0 then
            AList.Add(vParts[vPart]);
      end
      else if UpperCase(System.Copy(vLine, 1, 10)) = '* ESEARCH ' then
      begin
        vPart := Pos(' ALL ', UpperCase(vLine));
        if vPart > 0 then
        begin
          vParts.Delimiter := ',';
          vParts.DelimitedText := Trim(System.Copy(vLine, vPart + 5, MaxInt));
          for vPart := 0 to vParts.Count - 1 do
            if Pos(':', vParts[vPart]) > 0 then
            begin
              // a range a:b
              for vNum := StrToInt(System.Copy(vParts[vPart], 1, Pos(':', vParts[vPart]) - 1)) to
                StrToInt(System.Copy(vParts[vPart], Pos(':', vParts[vPart]) + 1, MaxInt)) do
                AList.Add(IntToStr(vNum));
            end
            else
              AList.Add(vParts[vPart]);
        end;
      end;
    end;
  finally
    vParts.Free;
  end;
end;

function TRALIMAPClient.UIDList(const AUIDs: array of Cardinal): RawByteString;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 0 to High(AUIDs) do
  begin
    if vInt > 0 then
      Result := Result + ',';
    Result := Result + RawByteString(IntToStr(AUIDs[vInt]));
  end;
  if Result = '' then
    Result := '1:*';
end;

procedure TRALIMAPClient.FetchRaw(AUID: Cardinal; AStream: TStream; APeek: Boolean);
begin
  FLiteralTarget := AStream;
  try
    if APeek then
      Command('UID FETCH ' + RawByteString(IntToStr(AUID)) + ' (BODY.PEEK[])')
    else
      Command('UID FETCH ' + RawByteString(IntToStr(AUID)) + ' (BODY[])');
  finally
    FLiteralTarget := nil;
  end;
end;

procedure TRALIMAPClient.FetchMessage(AUID: Cardinal; AMessage: TRALMailMessage; APeek: Boolean);
var
  vData: TRALBodyStream;
begin
  vData := TRALBodyStream.Create(-1, AMessage.SpoolAbove);
  try
    FetchRaw(AUID, vData, APeek);
    vData.Position := 0;
    AMessage.LoadFromStream(vData);
  finally
    vData.Free;
  end;
end;

procedure TRALIMAPClient.FetchSummaries(const AUIDs: array of Cardinal; AList: TList);
var
  vInt: IntegerRAL;
  vScan: TRALIMAPScanner;
  vText, vName: RawByteString;
  vToken: TRALIMAPToken;
  vSummary: TRALIMAPSummary;
  vHeader: TRALMailMessage;
  vStream: TStringStream;
begin
  Command('UID FETCH ' + UIDList(AUIDs) +
    ' (UID FLAGS RFC822.SIZE BODY.PEEK[HEADER.FIELDS (FROM SUBJECT DATE)])');
  for vInt := 0 to FReplies.Count - 1 do
  begin
    if Pos(' FETCH (', UpperCase(FReplies[vInt])) = 0 then
      Continue;
    vSummary := TRALIMAPSummary.Create;
    vScan := TRALIMAPScanner.Create(RawByteString(FReplies[vInt]), FLiterals);
    try
      vScan.Next(vText);
      vScan.Next(vText);
      vScan.Next(vText);
      vScan.Next(vText); // (
      repeat
        vToken := vScan.Next(vName);
        if vToken <> itAtom then
          Break;
        vName := RawByteString(UpperCase(string(vName)));
        if vName = 'UID' then
        begin
          vScan.Next(vText);
          vSummary.UID := StrToInt64Def(string(vText), 0);
        end
        else if vName = 'RFC822.SIZE' then
        begin
          vScan.Next(vText);
          vSummary.Size := StrToInt64Def(string(vText), 0);
        end
        else if vName = 'FLAGS' then
        begin
          vSummary.Flags := '';
          if vScan.Next(vText) = itOpen then
            while vScan.Next(vText) = itAtom do
            begin
              if vSummary.Flags <> '' then
                vSummary.Flags := vSummary.Flags + ' ';
              vSummary.Flags := vSummary.Flags + StringRAL(vText);
            end;
        end
        else if System.Copy(vName, 1, 5) = 'BODY[' then
        begin
          vScan.Next(vText);
          // the header fields, read as a message without body
          vHeader := TRALMailMessage.Create;
          vStream := TStringStream.Create(string(vText) + #13#10);
          try
            vHeader.LoadFromStream(vStream);
            vSummary.Subject := vHeader.Subject;
            if vHeader.From.Count > 0 then
            begin
              vSummary.From := vHeader.From[0].Address;
              if vHeader.From[0].Name <> '' then
                vSummary.From := vHeader.From[0].Name + ' <' + vSummary.From + '>';
            end;
            vSummary.Date := vHeader.Date;
          finally
            vStream.Free;
            vHeader.Free;
          end;
        end
        else if vScan.Next(vText) = itOpen then
          vScan.SkipList;
      until False;
    finally
      vScan.Free;
    end;
    AList.Add(vSummary);
  end;
end;

procedure TRALIMAPClient.SetFlags(const AUIDs: array of Cardinal; const AFlags: StringRAL;
  AAdd: Boolean);
begin
  if AAdd then
    Command('UID STORE ' + UIDList(AUIDs) + ' +FLAGS.SILENT (' + RawByteString(AFlags) + ')')
  else
    Command('UID STORE ' + UIDList(AUIDs) + ' -FLAGS.SILENT (' + RawByteString(AFlags) + ')');
end;

procedure TRALIMAPClient.Copy(const AUIDs: array of Cardinal; const ADestination: StringRAL);
begin
  Command('UID COPY ' + UIDList(AUIDs) + ' ' + MailboxArg(ADestination));
end;

procedure TRALIMAPClient.Delete(const AUIDs: array of Cardinal);
begin
  SetFlags(AUIDs, '\Deleted', True);
  // UIDPLUS removes only these; without it EXPUNGE takes every \Deleted message
  if HasCapability('UIDPLUS') or HasCapability('IMAP4REV2') then
    Command('UID EXPUNGE ' + UIDList(AUIDs))
  else
    Command('EXPUNGE');
end;

procedure TRALIMAPClient.Move(const AUIDs: array of Cardinal; const ADestination: StringRAL);
begin
  if HasCapability('MOVE') or HasCapability('IMAP4REV2') then
    Command('UID MOVE ' + UIDList(AUIDs) + ' ' + MailboxArg(ADestination))
  else
  begin
    Copy(AUIDs, ADestination);
    Delete(AUIDs);
  end;
end;

procedure TRALIMAPClient.Expunge;
begin
  Command('EXPUNGE');
end;

procedure TRALIMAPClient.CreateMailbox(const AName: StringRAL);
begin
  Command('CREATE ' + MailboxArg(AName));
end;

procedure TRALIMAPClient.DeleteMailbox(const AName: StringRAL);
begin
  Command('DELETE ' + MailboxArg(AName));
end;

procedure TRALIMAPClient.RenameMailbox(const AName, ANewName: StringRAL);
begin
  Command('RENAME ' + MailboxArg(AName) + ' ' + MailboxArg(ANewName));
end;

function TRALIMAPClient.AppendRaw(const AMailbox: StringRAL; AData: TStream;
  const AFlags: StringRAL): Cardinal;
var
  vTag, vHead, vLine, vCode: RawByteString;
  vSize: Int64;
  vPlus: Boolean;
begin
  Result := 0;
  if not Connected then
    Connect;
  FReplies.Clear;
  FLiterals.Clear;
  vSize := AData.Size - AData.Position;
  vPlus := HasCapability('LITERAL+') or HasCapability('LITERAL-') and (vSize <= 4096);
  vTag := NextTag;
  vHead := vTag + ' APPEND ' + MailboxArg(AMailbox);
  if AFlags <> '' then
    vHead := vHead + ' (' + RawByteString(AFlags) + ')';
  if vPlus then
    vHead := vHead + ' {' + RawByteString(IntToStr(vSize)) + '+}'
  else
    vHead := vHead + ' {' + RawByteString(IntToStr(vSize)) + '}';
  Log(vHead, True);
  FLink.Write(vHead + #13#10);
  if not vPlus then
  begin
    // the server says when the literal may come
    vLine := ReadResponse;
    if System.Copy(vLine, 1, 1) <> '+' then
      raise ERALIMAPError.CreateStatus('NO', StringRAL(vLine));
  end;
  FLink.Write(AData);
  FLink.Write(#13#10);
  vLine := WaitTagged(vTag);
  // [APPENDUID validity uid] (UIDPLUS)
  vCode := RawByteString(UpperCase(string(vLine)));
  if RALMailPos('[APPENDUID ', vCode) > 0 then
  begin
    vCode := System.Copy(vCode, RALMailPos('[APPENDUID ', vCode) + 11, MaxInt);
    vCode := System.Copy(vCode, RALMailPos(' ', vCode) + 1, MaxInt);
    Result := StrToInt64Def(string(System.Copy(vCode, 1, RALMailPos(']', vCode) - 1)), 0);
  end;
end;

function TRALIMAPClient.Append(const AMailbox: StringRAL; AMessage: TRALMailMessage;
  const AFlags: StringRAL): Cardinal;
var
  vData: TRALBodyStream;
begin
  vData := TRALBodyStream.Create(-1, 4 * 1024 * 1024);
  try
    // UTF-8 headers only where the server takes them
    if FUTF8 then
      AMessage.SaveToStream(vData, [mwo8Bit, mwoUTF8Headers])
    else
      AMessage.SaveToStream(vData, [mwo8Bit]);
    vData.Position := 0;
    Result := AppendRaw(AMailbox, vData, AFlags);
  finally
    vData.Free;
  end;
end;

function TRALIMAPClient.WaitForChanges(ATimeout: IntegerRAL): Boolean;
var
  vTag, vLine: RawByteString;
  vEnd, vLeft: Int64;
  vWord: string;
begin
  Result := False;
  if not Connected then
    Connect;
  if not HasCapability('IDLE') then
  begin
    // without IDLE: a NOOP after the wait
    Sleep(ATimeout);
    vLine := RawByteString(IntToStr(FExists));
    Noop;
    Result := (RawByteString(IntToStr(FExists)) <> vLine) or (FReplies.Count > 0);
    Exit;
  end;
  FReplies.Clear;
  FLiterals.Clear;
  vTag := NextTag;
  Log(vTag + ' IDLE', True);
  FLink.Write(vTag + ' IDLE' + #13#10);
  vLine := ReadResponse;
  if System.Copy(vLine, 1, 1) <> '+' then
    raise ERALIMAPError.CreateStatus('NO', StringRAL(vLine));
  vEnd := RALSocketNow + ATimeout;
  repeat
    vLeft := vEnd - RALSocketNow;
    if (vLeft <= 0) or not FLink.Readable(IntegerRAL(vLeft)) then
      Break;
    vLine := ReadResponse;
    if System.Copy(vLine, 1, 2) = '* ' then
    begin
      ParseUntagged(vLine);
      FReplies.Add(string(vLine));
      vWord := UpperCase(string(vLine));
      // a keep-alive "* OK Still here" changes nothing
      if (Pos(' EXISTS', vWord) > 0) or (Pos(' EXPUNGE', vWord) > 0) or
         (Pos(' FETCH ', vWord) > 0) then
        Result := True;
    end;
  until Result;
  Log('DONE', True);
  FLink.Write('DONE' + #13#10);
  WaitTagged(vTag);
end;

end.
