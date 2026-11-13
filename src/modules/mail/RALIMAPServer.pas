/// The IMAP server: serves the mailboxes of a TRALMailStore (RFC 3501, 9051).
unit RALIMAPServer;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools, RALBase64, RALSocket, RALSocketLoop,
  RALMailMIME, RALIMAPClient, RALMailStore, RALMailServerBase;

type
  TRALIMAPServer = class;

  /// What the connection is reading: a command, a literal, an IDLE or a SASL response.
  TRALIMAPInput = (
    /// A command line.
    iiCommand,
    /// A literal inside a command, kept in the command text.
    iiLiteral,
    /// The message of an APPEND.
    iiAppend,
    /// A literal that is thrown away (the command was refused).
    iiSkip,
    /// IDLE: waiting for DONE.
    iiIdle,
    /// A response of AUTHENTICATE.
    iiSASL
  );

  { One IMAP client connection: the commands, the replies and the events run on
    one poll thread. The connection keeps its own view of the selected mailbox
    (the messages and flags the client was told about) and compares it with the
    store after each command and, while IDLE, on every pass of the thread. }
  TRALIMAPConnection = class(TRALMailConnection)
  private
    /// The internal date given to the APPEND in progress.
    FAppendDate: TDateTime;
    /// The flags given to the APPEND in progress.
    FAppendFlags: StringRAL;
    /// The mailbox of the APPEND in progress.
    FAppendMailbox: StringRAL;
    /// The message of the APPEND in progress.
    FAppendStream: TMemoryStream;
    /// The tag of the APPEND in progress.
    FAppendTag: RawByteString;
    /// Failed logins so far.
    FBadLogins: IntegerRAL;
    /// The command being read, literals included as quoted strings.
    FCmd: RawByteString;
    /// ENABLE IMAP4rev2 was given.
    FEnabledRev2: boolean;
    /// ENABLE UTF8=ACCEPT was given.
    FEnabledUTF8: boolean;
    /// The items of the FETCH in progress (a TObject of this unit).
    FFetch: TObject;
    /// The tag of the IDLE in progress.
    FIdleTag: RawByteString;
    /// True while ProcessInput runs.
    FInProcess: boolean;
    /// What the connection is reading.
    FInput: TRALIMAPInput;
    /// Done is telling the client what changed.
    FInSync: boolean;
    /// The FETCH is running: the messages (indexes of the view) still to send.
    FJobActive: boolean;
    /// The messages (indexes of the view) the FETCH in progress sends.
    FJobIndex: TRALMailUIDs;
    /// The command name the FETCH in progress completes with.
    FJobName: RawByteString;
    /// The next message of FJobIndex to send.
    FJobPos: IntegerRAL;
    /// The tag of the FETCH in progress.
    FJobTag: RawByteString;
    /// The FETCH in progress is a UID FETCH.
    FJobUID: boolean;
    /// Bytes of the current literal still to come.
    FLitLeft: Int64;
    /// The literal waits for a continuation request (it is not LITERAL+).
    FLitSync: boolean;
    /// The bytes of the literal being read.
    FLitText: RawByteString;
    /// The command in progress may not send EXPUNGE responses.
    FNoExpunge: boolean;
    /// The mailbox is selected with EXAMINE.
    FReadOnly: boolean;
    /// Messages with a UID from this one on are recent in this session.
    FRecentFrom: Cardinal;
    /// The SASL exchange in progress.
    FSASL: TRALMailSASL;
    /// The tag of the AUTHENTICATE in progress.
    FSASLTag: RawByteString;
    /// The selected mailbox; empty when none.
    FSelected: StringRAL;
    /// The server that owns the connection.
    FServer: TRALIMAPServer;
    /// The reply sent when the literal being skipped ends.
    FSkipReply: RawByteString;
    /// The store's version of the selected mailbox when the view was last compared.
    FSyncVersion: Int64;
    /// The UIDVALIDITY of the selected mailbox.
    FValidity: Cardinal;
    /// The messages of the selected mailbox as the client knows them.
    FView: TRALMailMessageInfos;
    /// Messages in the view.
    FViewCount: IntegerRAL;

    /// Starts an APPEND whose literal was announced; False when it was refused.
    function BeginAppend: boolean;
    /// Starts a literal inside a command.
    procedure BeginLiteral;
    /// The capabilities as the CAPABILITY response lists them.
    function CapabilityText: RawByteString;
    /// Stores the message of the APPEND once its literal was read.
    procedure CmdAppendDone;
    /// The AUTHENTICATE command.
    procedure CmdAuthenticate(const ATag, AArgs: RawByteString);
    /// The COPY and MOVE commands.
    procedure CmdCopy(const ATag, AArgs: RawByteString; AUID, AMove: boolean);
    /// The CREATE and DELETE commands.
    procedure CmdCreateDelete(const ATag, ACommand, AArgs: RawByteString);
    /// The ENABLE command.
    procedure CmdEnable(const ATag, AArgs: RawByteString);
    /// The EXPUNGE and UID EXPUNGE commands.
    procedure CmdExpunge(const ATag, AArgs: RawByteString; AUID: boolean);
    /// The FETCH command: starts the work of Pump.
    procedure CmdFetch(const ATag, AArgs: RawByteString; AUID: boolean);
    /// The LIST and LSUB commands.
    procedure CmdList(const ATag, ACommand, AArgs: RawByteString);
    /// The LOGIN command.
    procedure CmdLogin(const ATag, AArgs: RawByteString);
    /// The RENAME command.
    procedure CmdRename(const ATag, AArgs: RawByteString);
    /// The SEARCH command.
    procedure CmdSearch(const ATag, AArgs: RawByteString; AUID: boolean);
    /// The SELECT and EXAMINE commands.
    procedure CmdSelect(const ATag, AArgs: RawByteString; AReadOnly: boolean);
    /// The STATUS command.
    procedure CmdStatus(const ATag, AArgs: RawByteString);
    /// The STORE command.
    procedure CmdStore(const ATag, AArgs: RawByteString; AUID: boolean);
    /// The SUBSCRIBE and UNSUBSCRIBE commands.
    procedure CmdSubscribe(const ATag, ACommand, AArgs: RawByteString);
    /// The mailbox name of the client as text of the store.
    function DecodeName(const AName: RawByteString): StringRAL;
    /// Sends the end of a tagged command: tag, status and text.
    procedure Done(const ATag, AStatus, AText: RawByteString);
    /// The name of the store as the client reads it.
    function EncodeName(const AName: StringRAL): RawByteString;
    /// Runs a whole command.
    procedure Execute(const ALine: RawByteString);
    /// Sends one message of the FETCH in progress.
    procedure FetchOne(AIndex: IntegerRAL);
    /// Starts the work of the FETCH in progress or ends it.
    procedure FinishJob;
    /// Handles the line of an IDLE.
    procedure IdleLine(const ALine: RawByteString);
    /// Takes a command line.
    procedure InputLine(const ALine: RawByteString);
    /// Takes the literal marker off the end of ALine; True when there was one.
    function LiteralMarker(var ALine: RawByteString): boolean;
    /// The text of a message, for a search.
    function LoadText(AUID: Cardinal): RawByteString;
    /// Loads the view of the selected mailbox from the store.
    procedure LoadView;
    /// Whether PLAIN and LOGIN may be used now.
    function PlainAllowed: boolean;
    /// Sends the next messages of the FETCH in progress.
    procedure Pump;
    /// The number of messages of the view that are recent in this session.
    function RecentCount: IntegerRAL;
    /// Removes the messages of AUIDs from the view, telling the client unless ASilent.
    procedure RemoveFromView(const AUIDs: TRALMailUIDs; ASilent: boolean);
    /// The indexes of the view that a sequence set (or UID set) selects.
    function ResolveSet(const ASet: RawByteString; AUID: boolean): TRALMailUIDs;
    /// A response of an AUTHENTICATE.
    procedure SASLLine(const ALine: RawByteString);
    /// Ends the AUTHENTICATE with the result of the exchange.
    procedure SASLResult(AResult: TRALMailSASLResult);
    /// Whether SCRAM-SHA-256 may be used now.
    function ScramAllowed: boolean;
    /// Sends the flags of a message as a FETCH response.
    procedure SendFlags(AIndex: IntegerRAL; AUID: boolean);
    /// Sends the store error AError as the negative reply of ATag.
    procedure StoreFailure(const ATag: RawByteString; AError: ERALMailStoreError);
    /// Tells the client what changed in the selected mailbox since it last knew.
    procedure SyncView(AAllowExpunge: boolean);
    /// Forgets the selected mailbox.
    procedure Unselect;
    /// The index of AUID in the view, or -1.
    function ViewFind(AUID: Cardinal): IntegerRAL;
  protected
    procedure Closed; override;
    function FaultReply(const AMessage: StringRAL): RawByteString; override;
    function Greeting: RawByteString; override;
    procedure ProcessInput; override;
    function RefusedReply(ATooMany: boolean): RawByteString; override;
    function Resting: boolean; override;
    function StopReply: RawByteString; override;
    procedure Tick(ANow: Int64RAL); override;
    procedure TLSStarted; override;
  public
    /// Creates the connection of ASocket, accepted on listener AListener.
    constructor Create(AServer: TRALIMAPServer; ASocket: TRALSocket; AListener: IntegerRAL);
    destructor Destroy; override;

    function HasOutput: boolean; override;
  end;

  { An IMAP server over the socket event loop: Port (143) with STARTTLS and
    TLSPort (993) with implicit TLS. It answers IMAP4rev1 (RFC 3501) clients
    and IMAP4rev2 (RFC 9051) ones, with SASL-IR, LITERAL+, UIDPLUS, MOVE, IDLE,
    UNSELECT, ENABLE, ID, NAMESPACE, CHILDREN and UTF8=ACCEPT. The mailboxes are
    those of Store; a message a TRALSMTPServer delivers there (Store.Deliver)
    reaches the clients of that mailbox as an EXISTS response, at once when they
    are in IDLE. The login goes through the server's plugins as the other mail
    servers' does. }
  TRALIMAPServer = class(TRALMailServer)
  private
    FMaxMessageSize: Int64;
  protected
    function BusyReply: RawByteString; override;
    function CreateConnection(ASocket: TRALSocket; AListener: IntegerRAL): TRALMailConnection; override;
    function ProtocolName: string; override;
    function SecureProtocolName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    /// The largest message APPEND accepts, in bytes; 0 = no limit.
    property MaxMessageSize: Int64 read FMaxMessageSize write FMaxMessageSize default 26214400;
    /// The implicit TLS port (993), opened when SSL is enabled; 0 closes it.
    property TLSPort default 993;
  end;

implementation

const
  /// The longest command, literals included, in bytes.
  cMaxCommand = 1048576;
  /// The longest literal inside a command other than APPEND, in bytes.
  cMaxLiteral = 65536;
  /// A FETCH sends messages while less than this is queued, in bytes.
  cPumpLimit = 262144;
  /// Line break of the wire.
  cCRLF: RawByteString = #13#10;
  /// Month names of IMAP dates.
  cMonths: array[1..12] of RawByteString = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
    'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  /// The system flags the mailboxes accept.
  cSystemFlagList = '\Answered \Flagged \Deleted \Seen \Draft';

type
  /// A range of a sequence set; Hi = -1 is '*'.
  TSetRange = record
    /// The first number of the range.
    Lo, Hi: Int64;
  end;
  /// The ranges of a sequence set.
  TSetRanges = array of TSetRange;

  /// What a FETCH item asks.
  TFetchKind = (fkFlags, fkUID, fkInternalDate, fkSize, fkEnvelope, fkStructure, fkBodyBasic,
    fkSection, fkRFC822, fkRFC822Header, fkRFC822Text);

  /// One item of a FETCH.
  TFetchItem = record
    /// What the item or node is.
    Kind: TFetchKind;
    /// The text between the brackets of BODY[...].
    Section: RawByteString;
    /// The item does not set \Seen.
    Peek: boolean;
    /// The first byte of the partial range.
    PartStart: Int64;
    /// -1 when the item has no partial range.
    PartLength: Int64;
    /// The item has a partial range.
    HasPartial: boolean;
  end;

  /// The items of a FETCH.
  TFetchSpec = class
  public
    /// The items in order.
    Items: array of TFetchItem;
    /// An item needs the text of the message.
    NeedMessage: boolean;
    /// An item sets \Seen.
    SetsSeen: boolean;
  end;

  /// A part of a MIME message: offsets are into the message text.
  TMimePart = class
  public
    /// Bytes of the body.
    BodyLen: IntegerRAL;
    /// Offset of the body.
    BodyStart: IntegerRAL;
    /// The parts of a multipart.
    Children: TList;
    /// Content-Description.
    Description: RawByteString;
    /// The disposition, upper case.
    Disposition: RawByteString;
    /// The disposition parameters as an IMAP list.
    DispositionParams: RawByteString;
    /// The message of a message/rfc822 part.
    Embedded: TMimePart;
    /// Content-Transfer-Encoding, upper case.
    Encoding: RawByteString;
    /// Bytes of the header, with the empty line.
    HeaderLen: IntegerRAL;
    /// Offset of the header.
    HeaderStart: IntegerRAL;
    /// Content-ID.
    Id: RawByteString;
    /// Content-Language.
    Language: RawByteString;
    /// Lines of the body.
    Lines: IntegerRAL;
    /// Content-Location.
    Location: RawByteString;
    /// Content-MD5.
    MD5: RawByteString;
    /// The media type, lower case.
    MediaType: RawByteString;
    /// The Content-Type parameters as an IMAP list.
    Params: RawByteString;
    /// The media subtype, lower case.
    SubType: RawByteString;

    constructor Create;
    destructor Destroy; override;
  end;

constructor TMimePart.Create;
begin
  inherited Create;
  Children := TList.Create;
end;

destructor TMimePart.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Children.Count - 1 do
    TObject(Children[vInt]).Free;
  Children.Free;
  Embedded.Free;
  inherited Destroy;
end;

/// AValue as text.
function IStr(AValue: Int64): RawByteString;
begin
  Result := RawByteString(IntToStr(AValue));
end;

/// AText as a quoted string.
function Quoted(const AText: RawByteString): RawByteString;
var
  vInt: IntegerRAL;
begin
  Result := '"';
  for vInt := 1 to Length(AText) do
  begin
    if (AText[vInt] = '"') or (AText[vInt] = '\') then
      Result := Result + '\';
    Result := Result + AText[vInt];
  end;
  Result := Result + '"';
end;

/// AText as a quoted string, or as a literal when it holds line breaks or NUL.
function AString(const AText: RawByteString): RawByteString;
var
  vInt: IntegerRAL;
begin
  for vInt := 1 to Length(AText) do
    if AText[vInt] in [#0, #10, #13] then
    begin
      Result := '{' + IStr(Length(AText)) + '}' + cCRLF + AText;
      Exit;
    end;
  Result := Quoted(AText);
end;

/// AText as a string, or NIL when it is empty.
function NString(const AText: RawByteString): RawByteString;
begin
  if AText = '' then
    Result := 'NIL'
  else
    Result := AString(AText);
end;

/// Moves APos past the blanks.
procedure SkipBlanks(const AText: RawByteString; var APos: IntegerRAL);
begin
  while (APos <= Length(AText)) and (AText[APos] = ' ') do
    Inc(APos);
end;

/// The next atom (up to a blank or a parenthesis, brackets kept whole).
function ReadAtom(const AText: RawByteString; var APos: IntegerRAL): RawByteString;
var
  vStart, vDepth: IntegerRAL;
begin
  SkipBlanks(AText, APos);
  vStart := APos;
  vDepth := 0;
  while APos <= Length(AText) do
  begin
    if AText[APos] = '[' then
      Inc(vDepth)
    else if AText[APos] = ']' then
      Dec(vDepth)
    else if (vDepth <= 0) and (AText[APos] in [' ', '(', ')']) then
      Break
    else if (vDepth > 0) and (AText[APos] = '"') then
    begin
      Inc(APos);
      while (APos <= Length(AText)) and (AText[APos] <> '"') do
        Inc(APos);
    end;
    Inc(APos);
  end;
  Result := Copy(AText, vStart, APos - vStart);
end;

/// The next astring: quoted (unescaped) or an atom.
function ReadString(const AText: RawByteString; var APos: IntegerRAL): RawByteString;
begin
  SkipBlanks(AText, APos);
  Result := '';
  if (APos <= Length(AText)) and (AText[APos] = '"') then
  begin
    Inc(APos);
    while (APos <= Length(AText)) and (AText[APos] <> '"') do
    begin
      if (AText[APos] = '\') and (APos < Length(AText)) then
        Inc(APos);
      Result := Result + AText[APos];
      Inc(APos);
    end;
    Inc(APos);
  end
  else
    Result := ReadAtom(AText, APos);
end;

/// The inside of the next parenthesized list; False when the next token is not a list.
function ReadList(const AText: RawByteString; var APos: IntegerRAL; out AInner: RawByteString): boolean;
var
  vStart, vDepth: IntegerRAL;
begin
  SkipBlanks(AText, APos);
  Result := (APos <= Length(AText)) and (AText[APos] = '(');
  if not Result then
    Exit;
  Inc(APos);
  vStart := APos;
  vDepth := 1;
  while APos <= Length(AText) do
  begin
    if AText[APos] = '"' then
    begin
      Inc(APos);
      while (APos <= Length(AText)) and (AText[APos] <> '"') do
      begin
        if AText[APos] = '\' then
          Inc(APos);
        Inc(APos);
      end;
    end
    else if AText[APos] = '(' then
      Inc(vDepth)
    else if AText[APos] = ')' then
    begin
      Dec(vDepth);
      if vDepth = 0 then
        Break;
    end;
    Inc(APos);
  end;
  AInner := Copy(AText, vStart, APos - vStart);
  Inc(APos);
end;

/// A sequence set as ranges; False when it is not one.
function ParseSet(const AText: RawByteString; out ARanges: TSetRanges): boolean;
var
  vInt, vStart, vColon: IntegerRAL;
  vPart: RawByteString;

  function Num(const AValue: RawByteString; out ANumber: Int64): boolean;
  begin
    if AValue = '*' then
    begin
      ANumber := -1;
      Result := True;
    end
    else
    begin
      ANumber := StrToInt64Def(string(AValue), -1);
      Result := (ANumber > 0) and (AValue <> '');
    end;
  end;

  function Take(const APiece: RawByteString): boolean;
  var
    vLo, vHi: Int64;
  begin
    vColon := Pos(RawByteString(':'), APiece);
    if vColon > 0 then
      Result := Num(Copy(APiece, 1, vColon - 1), vLo) and Num(Copy(APiece, vColon + 1, MaxInt), vHi)
    else
    begin
      Result := Num(APiece, vLo);
      vHi := vLo;
    end;
    if not Result then
      Exit;
    SetLength(ARanges, Length(ARanges) + 1);
    ARanges[High(ARanges)].Lo := vLo;
    ARanges[High(ARanges)].Hi := vHi;
  end;

begin
  SetLength(ARanges, 0);
  Result := AText <> '';
  vStart := 1;
  for vInt := 1 to Length(AText) + 1 do
    if (vInt > Length(AText)) or (AText[vInt] = ',') then
    begin
      vPart := Copy(AText, vStart, vInt - vStart);
      if not Take(vPart) then
      begin
        Result := False;
        Exit;
      end;
      vStart := vInt + 1;
    end;
end;

/// Whether AValue is in ARanges ('*' is AMax).
function InSet(const ARanges: TSetRanges; AValue, AMax: Int64): boolean;
var
  vInt: IntegerRAL;
  vLo, vHi, vColon: Int64;
begin
  Result := False;
  for vInt := 0 to High(ARanges) do
  begin
    vLo := ARanges[vInt].Lo;
    vHi := ARanges[vInt].Hi;
    if vLo = -1 then
      vLo := AMax;
    if vHi = -1 then
      vHi := AMax;
    if vLo > vHi then
    begin
      // a:b with a > b is b:a
      vColon := vLo;
      vLo := vHi;
      vHi := vColon;
    end;
    if (AValue >= vLo) and (AValue <= vHi) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

/// The UIDs as a set text ('1:3,5').
function UIDSetText(const AUIDs: TRALMailUIDs): RawByteString;
var
  vInt, vStart: IntegerRAL;
begin
  Result := '';
  vInt := 0;
  while vInt <= High(AUIDs) do
  begin
    vStart := vInt;
    while (vInt < High(AUIDs)) and (AUIDs[vInt + 1] = AUIDs[vInt] + 1) do
      Inc(vInt);
    if Result <> '' then
      Result := Result + ',';
    if vInt > vStart then
      Result := Result + IStr(AUIDs[vStart]) + ':' + IStr(AUIDs[vInt])
    else
      Result := Result + IStr(AUIDs[vStart]);
    Inc(vInt);
  end;
end;

/// The flags of a message as an IMAP list.
function FlagList(const AFlags: StringRAL): RawByteString;
begin
  Result := '(' + RawByteString(AFlags) + ')';
end;

/// '1-Jan-2024' or '01-Jan-2024' as a date; 0 when it is not one.
function ParseSearchDate(const AText: RawByteString): TDateTime;
var
  vFirst, vSecond, vMonth, vInt: IntegerRAL;
begin
  Result := 0;
  vFirst := Pos(RawByteString('-'), AText);
  if vFirst = 0 then
    Exit;
  vSecond := vFirst + Pos(RawByteString('-'), Copy(AText, vFirst + 1, MaxInt));
  if vSecond = vFirst then
    Exit;
  vMonth := 0;
  for vInt := 1 to 12 do
    if RALMailLower(Copy(AText, vFirst + 1, vSecond - vFirst - 1)) = RALMailLower(cMonths[vInt]) then
      vMonth := vInt;
  if vMonth = 0 then
    Exit;
  try
    Result := EncodeDate(StrToIntDef(string(Copy(AText, vSecond + 1, 4)), 0), vMonth,
      StrToIntDef(string(Copy(AText, 1, vFirst - 1)), 0));
  except
    Result := 0;
  end;
end;

/// '"17-Jul-1996 02:44:25 -0700"' as UTC; 0 when it is not a date-time.
function ParseInternalDate(const AText: RawByteString): TDateTime;
var
  vDate: TDateTime;
  vTime, vZone: RawByteString;
  vSpace: IntegerRAL;
  vOffset: IntegerRAL;
begin
  Result := 0;
  vSpace := Pos(RawByteString(' '), AText);
  if vSpace = 0 then
    Exit;
  vDate := ParseSearchDate(Copy(AText, 1, vSpace - 1));
  if vDate = 0 then
    Exit;
  vTime := Copy(AText, vSpace + 1, 8);
  vZone := RALMailTrim(Copy(AText, vSpace + 10, MaxInt));
  try
    Result := vDate + EncodeTime(StrToIntDef(string(Copy(vTime, 1, 2)), 0),
      StrToIntDef(string(Copy(vTime, 4, 2)), 0), StrToIntDef(string(Copy(vTime, 7, 2)), 0), 0);
  except
    Result := 0;
    Exit;
  end;
  if Length(vZone) >= 5 then
  begin
    vOffset := StrToIntDef(string(Copy(vZone, 2, 2)), 0) * 60 + StrToIntDef(string(Copy(vZone, 4, 2)), 0);
    if vZone[1] = '-' then
      vOffset := -vOffset;
    Result := Result - vOffset / 1440;
  end;
end;

/// A UTC date-time as '17-Jul-1996 02:44:25 +0000'.
function InternalDateText(ADate: TDateTime): RawByteString;
var
  vYear, vMonth, vDay, vH, vN, vS, vMs: Word;
begin
  DecodeDate(ADate, vYear, vMonth, vDay);
  DecodeTime(ADate, vH, vN, vS, vMs);
  Result := RawByteString(Format('%.2d-%s-%.4d %.2d:%.2d:%.2d +0000', [vDay, string(cMonths[vMonth]),
    vYear, vH, vN, vS]));
end;

{ the message text }

/// The offset after the empty line that ends the header; the end + 1 when there is none.
function HeaderEnd(const AMessage: RawByteString; AStart, AStop: IntegerRAL): IntegerRAL;
var
  vPos, vLineStart: IntegerRAL;
begin
  vPos := AStart;
  vLineStart := AStart;
  Result := AStop + 1;
  while vPos <= AStop do
  begin
    if AMessage[vPos] = #10 then
    begin
      // an empty line: the line start is the line end (or a lone CR)
      if (vPos = vLineStart) or ((vPos = vLineStart + 1) and (AMessage[vLineStart] = #13)) then
      begin
        Result := vPos + 1;
        Exit;
      end;
      vLineStart := vPos + 1;
    end;
    Inc(vPos);
  end;
end;

/// The value of the first header AName in AHeader, unfolded and trimmed.
function HeaderValue(const AHeader: RawByteString; const AName: RawByteString): RawByteString;
var
  vPos, vEnd, vInt: IntegerRAL;
  vLower, vKey: RawByteString;
begin
  Result := '';
  vLower := RALMailLower(AHeader);
  vKey := RALMailLower(AName) + ':';
  vPos := 1;
  while vPos <= Length(AHeader) do
  begin
    if Copy(vLower, vPos, Length(vKey)) = vKey then
    begin
      vEnd := vPos;
      repeat
        while (vEnd <= Length(AHeader)) and (AHeader[vEnd] <> #10) do
          Inc(vEnd);
        Inc(vEnd);
      until (vEnd > Length(AHeader)) or not (AHeader[vEnd] in [' ', #9]);
      Result := Copy(AHeader, vPos + Length(vKey), vEnd - vPos - Length(vKey));
      // unfold: line breaks go, the blanks around them stay as one
      vInt := 1;
      while vInt <= Length(Result) do
        if Result[vInt] in [#13, #10] then
          Delete(Result, vInt, 1)
        else
          Inc(vInt);
      Result := RALMailTrim(Result);
      Exit;
    end;
    while (vPos <= Length(AHeader)) and (AHeader[vPos] <> #10) do
      Inc(vPos);
    Inc(vPos);
  end;
end;

/// The header lines named in AFields (or all but them when ANot), then an empty line.
function FilterHeader(const AHeader: RawByteString; const AFields: TStringList;
  ANot: boolean): RawByteString;
var
  vPos, vEnd, vColon: IntegerRAL;
  vName: RawByteString;
  vMatch: boolean;
begin
  Result := '';
  vPos := 1;
  while vPos <= Length(AHeader) do
  begin
    vEnd := vPos;
    repeat
      while (vEnd <= Length(AHeader)) and (AHeader[vEnd] <> #10) do
        Inc(vEnd);
      Inc(vEnd);
    until (vEnd > Length(AHeader)) or not (AHeader[vEnd] in [' ', #9]);
    vColon := Pos(RawByteString(':'), Copy(AHeader, vPos, vEnd - vPos));
    if vColon > 0 then
    begin
      vName := RALMailLower(Copy(AHeader, vPos, vColon - 1));
      vMatch := AFields.IndexOf(string(vName)) >= 0;
      if vMatch <> ANot then
        Result := Result + Copy(AHeader, vPos, vEnd - vPos);
    end;
    vPos := vEnd;
  end;
  Result := Result + cCRLF;
end;

/// The value of the parameter AName in the parameter text AParams ('a="b"; c=d').
function ParamOf(const AParams, AName: RawByteString): RawByteString;
begin
  Result := RawByteString(RALMailHeaderParam(StringRAL('x; ' + string(AParams)), StringRAL(AName)));
end;

/// Parses the part of AMessage between AStart and AStop (offsets, inclusive).
function ParsePart(const AMessage: RawByteString; AStart, AStop: IntegerRAL;
  const ADefaultType: RawByteString; ADepth: IntegerRAL): TMimePart;
var
  vHeader, vType, vMain, vDisp, vBoundary: RawByteString;
  vSlash, vPos, vLineStart, vPartStart, vLineEnd, vInt: IntegerRAL;
  vChild: TMimePart;
  vSemi: IntegerRAL;
  vBodyEnd: IntegerRAL;

  function Between(const AValue: RawByteString): RawByteString;
  var
    vSemicolon: IntegerRAL;
  begin
    vSemicolon := Pos(RawByteString(';'), AValue);
    if vSemicolon = 0 then
      Result := ''
    else
      Result := Copy(AValue, vSemicolon + 1, MaxInt);
  end;

  /// '("A" "b" "C" "d")' of the parameters of a header value, or NIL.
  function ParamList(const AValue: RawByteString): RawByteString;
  var
    vText, vName, vValue: RawByteString;
    vEq, vSep, vPosition: IntegerRAL;
    vQuote: boolean;
  begin
    Result := '';
    vText := Between(AValue);
    vPosition := 1;
    while vPosition <= Length(vText) do
    begin
      vSep := vPosition;
      vQuote := False;
      while (vSep <= Length(vText)) and (vQuote or (vText[vSep] <> ';')) do
      begin
        if vText[vSep] = '"' then
          vQuote := not vQuote;
        Inc(vSep);
      end;
      vName := RALMailTrim(Copy(vText, vPosition, vSep - vPosition));
      vEq := Pos(RawByteString('='), vName);
      if vEq > 0 then
      begin
        vValue := RALMailTrim(Copy(vName, vEq + 1, MaxInt));
        vName := RALMailTrim(Copy(vName, 1, vEq - 1));
        if (Length(vValue) >= 2) and (vValue[1] = '"') then
          vValue := Copy(vValue, 2, Length(vValue) - 2);
        if Result <> '' then
          Result := Result + ' ';
        Result := Result + Quoted(RALMailUpper(vName)) + ' ' + Quoted(vValue);
      end;
      vPosition := vSep + 1;
    end;
    if Result = '' then
      Result := 'NIL'
    else
      Result := '(' + Result + ')';
  end;

begin
  Result := TMimePart.Create;
  try
    Result.HeaderStart := AStart;
    Result.BodyStart := HeaderEnd(AMessage, AStart, AStop);
    Result.HeaderLen := Result.BodyStart - AStart;
    Result.BodyLen := AStop - Result.BodyStart + 1;
    if Result.BodyLen < 0 then
      Result.BodyLen := 0;
    vHeader := Copy(AMessage, AStart, Result.HeaderLen);
    vType := HeaderValue(vHeader, 'Content-Type');
    if vType = '' then
      vType := ADefaultType;
    vMain := RALMailLower(RALMailTrim(Copy(vType, 1, Pos(RawByteString(';'), vType + ';') - 1)));
    vSlash := Pos(RawByteString('/'), vMain);
    if vSlash > 0 then
    begin
      Result.MediaType := Copy(vMain, 1, vSlash - 1);
      Result.SubType := Copy(vMain, vSlash + 1, MaxInt);
    end
    else
    begin
      Result.MediaType := 'text';
      Result.SubType := 'plain';
    end;
    Result.Params := ParamList(vType);
    Result.Id := HeaderValue(vHeader, 'Content-ID');
    Result.Description := HeaderValue(vHeader, 'Content-Description');
    Result.Encoding := RALMailUpper(HeaderValue(vHeader, 'Content-Transfer-Encoding'));
    if Result.Encoding = '' then
      Result.Encoding := '7BIT';
    Result.MD5 := HeaderValue(vHeader, 'Content-MD5');
    Result.Language := HeaderValue(vHeader, 'Content-Language');
    Result.Location := HeaderValue(vHeader, 'Content-Location');
    vDisp := HeaderValue(vHeader, 'Content-Disposition');
    if vDisp <> '' then
    begin
      vSemi := Pos(RawByteString(';'), vDisp + ';');
      Result.Disposition := RALMailUpper(RALMailTrim(Copy(vDisp, 1, vSemi - 1)));
      Result.DispositionParams := ParamList(vDisp);
    end;
    vBodyEnd := Result.BodyStart + Result.BodyLen - 1;
    for vInt := Result.BodyStart to vBodyEnd do
      if AMessage[vInt] = #10 then
        Inc(Result.Lines);
    if ADepth > 12 then
      Exit;
    if (Result.MediaType = 'multipart') then
    begin
      vBoundary := ParamOf(vType, 'boundary');
      if vBoundary = '' then
        Exit;
      vBoundary := '--' + vBoundary;
      vPos := Result.BodyStart;
      vLineStart := vPos;
      vPartStart := 0;
      while vPos <= vBodyEnd + 1 do
      begin
        // a line at vLineStart; its end is the next LF
        vLineEnd := vLineStart;
        while (vLineEnd <= vBodyEnd) and (AMessage[vLineEnd] <> #10) do
          Inc(vLineEnd);
        if Copy(AMessage, vLineStart, Length(vBoundary)) = vBoundary then
        begin
          if vPartStart > 0 then
          begin
            // the part ends before the line break that precedes the boundary
            vInt := vLineStart - 1;
            if (vInt >= vPartStart) and (AMessage[vInt] = #10) then
              Dec(vInt);
            if (vInt >= vPartStart) and (AMessage[vInt] = #13) then
              Dec(vInt);
            if Result.SubType = 'digest' then
              vChild := ParsePart(AMessage, vPartStart, vInt, 'message/rfc822', ADepth + 1)
            else
              vChild := ParsePart(AMessage, vPartStart, vInt, 'text/plain', ADepth + 1);
            Result.Children.Add(vChild);
          end;
          if Copy(AMessage, vLineStart + Length(vBoundary), 2) = '--' then
            Break;
          vPartStart := vLineEnd + 1;
        end;
        vLineStart := vLineEnd + 1;
        vPos := vLineStart;
        if vLineStart > vBodyEnd then
          Break;
      end;
    end
    else if (Result.MediaType = 'message') and (Result.SubType = 'rfc822') and (Result.BodyLen > 0) then
      Result.Embedded := ParsePart(AMessage, Result.BodyStart, vBodyEnd, 'text/plain', ADepth + 1);
  except
    Result.Free;
    raise;
  end;
end;

/// The address list of a header as IMAP addresses, or NIL.
function AddressList(const AValue: RawByteString): RawByteString;
var
  vPos, vStart, vDepth: IntegerRAL;
  vQuote: boolean;
  vItem: RawByteString;

  procedure Take(const AItem: RawByteString);
  var
    vName, vAddr, vLocal, vHost: RawByteString;
    vOpen, vClose, vAt: IntegerRAL;
  begin
    vName := '';
    vAddr := RALMailTrim(AItem);
    if vAddr = '' then
      Exit;
    vOpen := Pos(RawByteString('<'), vAddr);
    vClose := Pos(RawByteString('>'), vAddr);
    if (vOpen > 0) and (vClose > vOpen) then
    begin
      vName := RALMailTrim(Copy(vAddr, 1, vOpen - 1));
      if (Length(vName) >= 2) and (vName[1] = '"') and (vName[Length(vName)] = '"') then
        vName := Copy(vName, 2, Length(vName) - 2);
      vAddr := Copy(vAddr, vOpen + 1, vClose - vOpen - 1);
    end;
    vAt := 0;
    for vOpen := 1 to Length(vAddr) do
      if vAddr[vOpen] = '@' then
        vAt := vOpen;
    if vAt > 0 then
    begin
      vLocal := Copy(vAddr, 1, vAt - 1);
      vHost := Copy(vAddr, vAt + 1, MaxInt);
    end
    else
    begin
      vLocal := vAddr;
      vHost := '';
    end;
    Result := Result + '(' + NString(vName) + ' NIL ' + NString(vLocal) + ' ' + NString(vHost) + ')';
  end;

begin
  Result := '';
  vStart := 1;
  vDepth := 0;
  vQuote := False;
  for vPos := 1 to Length(AValue) + 1 do
  begin
    if vPos > Length(AValue) then
      Take(Copy(AValue, vStart, vPos - vStart))
    else if AValue[vPos] = '"' then
      vQuote := not vQuote
    else if not vQuote then
    begin
      if AValue[vPos] = '<' then
        Inc(vDepth)
      else if AValue[vPos] = '>' then
        Dec(vDepth)
      else if (AValue[vPos] = ',') and (vDepth <= 0) then
      begin
        vItem := Copy(AValue, vStart, vPos - vStart);
        Take(vItem);
        vStart := vPos + 1;
      end;
    end;
  end;
  if Result = '' then
    Result := 'NIL'
  else
    Result := '(' + Result + ')';
end;

/// The ENVELOPE of a message with header AHeader.
function EnvelopeText(const AHeader: RawByteString): RawByteString;
var
  vFrom, vSender, vReply: RawByteString;
begin
  vFrom := AddressList(HeaderValue(AHeader, 'From'));
  vSender := AddressList(HeaderValue(AHeader, 'Sender'));
  vReply := AddressList(HeaderValue(AHeader, 'Reply-To'));
  if vSender = 'NIL' then
    vSender := vFrom;
  if vReply = 'NIL' then
    vReply := vFrom;
  Result := '(' + NString(HeaderValue(AHeader, 'Date')) + ' ' + NString(HeaderValue(AHeader, 'Subject')) +
    ' ' + vFrom + ' ' + vSender + ' ' + vReply + ' ' + AddressList(HeaderValue(AHeader, 'To')) + ' ' +
    AddressList(HeaderValue(AHeader, 'Cc')) + ' ' + AddressList(HeaderValue(AHeader, 'Bcc')) + ' ' +
    NString(HeaderValue(AHeader, 'In-Reply-To')) + ' ' + NString(HeaderValue(AHeader, 'Message-ID')) + ')';
end;

/// The BODYSTRUCTURE (or BODY when not AExtended) of APart.
function StructureText(const AMessage: RawByteString; APart: TMimePart; AExtended: boolean): RawByteString;
var
  vInt: IntegerRAL;
  vExt, vDisp: RawByteString;
begin
  if APart.Disposition <> '' then
    vDisp := '(' + Quoted(APart.Disposition) + ' ' + APart.DispositionParams + ')'
  else
    vDisp := 'NIL';
  if APart.MediaType = 'multipart' then
  begin
    Result := '(';
    for vInt := 0 to APart.Children.Count - 1 do
      Result := Result + StructureText(AMessage, TMimePart(APart.Children[vInt]), AExtended);
    if APart.Children.Count = 0 then
      Result := Result + '("TEXT" "PLAIN" NIL NIL NIL "7BIT" 0 0)';
    Result := Result + ' ' + Quoted(RALMailUpper(APart.SubType));
    if AExtended then
    begin
      if APart.Params = '' then
        Result := Result + ' NIL'
      else
        Result := Result + ' ' + APart.Params;
      Result := Result + ' ' + vDisp + ' ' + NString(APart.Language) + ' ' + NString(APart.Location);
    end;
    Result := Result + ')';
    Exit;
  end;
  Result := '(' + Quoted(RALMailUpper(APart.MediaType)) + ' ' + Quoted(RALMailUpper(APart.SubType)) + ' ';
  if APart.Params = '' then
    Result := Result + 'NIL'
  else
    Result := Result + APart.Params;
  Result := Result + ' ' + NString(APart.Id) + ' ' + NString(APart.Description) + ' ' +
    Quoted(APart.Encoding) + ' ' + IStr(APart.BodyLen);
  if (APart.MediaType = 'message') and (APart.SubType = 'rfc822') and (APart.Embedded <> nil) then
    Result := Result + ' ' + EnvelopeText(Copy(AMessage, APart.Embedded.HeaderStart, APart.Embedded.HeaderLen)) +
      ' ' + StructureText(AMessage, APart.Embedded, AExtended) + ' ' + IStr(APart.Lines)
  else if APart.MediaType = 'text' then
    Result := Result + ' ' + IStr(APart.Lines);
  if AExtended then
  begin
    vExt := NString(APart.MD5);
    Result := Result + ' ' + vExt + ' ' + vDisp + ' ' + NString(APart.Language) + ' ' + NString(APart.Location);
  end;
  Result := Result + ')';
end;

/// The part a numeric path ('2.1') names, or nil.
function FindPart(ARoot: TMimePart; const APath: RawByteString; out AAtEnd: boolean): TMimePart;
var
  vNode: TMimePart;
  vNumbers: TStringList;
  vInt, vK: IntegerRAL;
  vText: string;
begin
  AAtEnd := True;
  vNode := ARoot;
  Result := ARoot;
  if APath = '' then
    Exit;
  vNumbers := TStringList.Create;
  try
    vNumbers.Delimiter := '.';
    vNumbers.StrictDelimiter := True;
    vText := string(APath);
    vNumbers.DelimitedText := vText;
    for vInt := 0 to vNumbers.Count - 1 do
    begin
      vK := StrToIntDef(vNumbers[vInt], 0);
      if vK < 1 then
      begin
        Result := nil;
        Exit;
      end;
      // the numbers of a message part continue inside the embedded message
      if (vNode.Embedded <> nil) and (vNode.MediaType = 'message') then
        vNode := vNode.Embedded;
      if vNode.MediaType = 'multipart' then
      begin
        if vK > vNode.Children.Count then
        begin
          Result := nil;
          Exit;
        end;
        vNode := TMimePart(vNode.Children[vK - 1]);
      end
      else if vK <> 1 then
      begin
        Result := nil;
        Exit;
      end;
    end;
    Result := vNode;
  finally
    vNumbers.Free;
  end;
end;

/// The text of a BODY[ASection] of AMessage; False when the section does not exist.
function SectionText(const AMessage: RawByteString; ARoot: TMimePart; const ASection: RawByteString;
  out AText: RawByteString): boolean;
var
  vSection, vPath, vSpec, vFields: RawByteString;
  vInt, vParen: IntegerRAL;
  vPart: TMimePart;
  vAtEnd: boolean;
  vList: TStringList;
  vPieces: TStringList;
begin
  Result := False;
  AText := '';
  vSection := RALMailTrim(ASection);
  // the numeric path, then the specifier
  vInt := 1;
  while (vInt <= Length(vSection)) and (vSection[vInt] in ['0'..'9', '.']) do
    Inc(vInt);
  vPath := Copy(vSection, 1, vInt - 1);
  vSpec := Copy(vSection, vInt, MaxInt);
  if (vPath <> '') and (vPath[Length(vPath)] = '.') then
    SetLength(vPath, Length(vPath) - 1)
  else if (vPath <> '') and (vSpec <> '') then
    Exit;
  vPart := FindPart(ARoot, vPath, vAtEnd);
  if vPart = nil then
    Exit;
  vFields := '';
  vParen := Pos(RawByteString('('), vSpec);
  if vParen > 0 then
  begin
    vFields := Copy(vSpec, vParen + 1, MaxInt);
    vFields := Copy(vFields, 1, Pos(RawByteString(')'), vFields) - 1);
    vSpec := RALMailUpper(RALMailTrim(Copy(vSpec, 1, vParen - 1)));
  end
  else
    vSpec := RALMailUpper(vSpec);
  if vSpec = '' then
  begin
    if vPath = '' then
      AText := AMessage
    else
      AText := Copy(AMessage, vPart.BodyStart, vPart.BodyLen);
    Result := True;
  end
  else if vSpec = 'TEXT' then
  begin
    // of the message, or of an embedded message
    if (vPath <> '') and (vPart.Embedded <> nil) then
      vPart := vPart.Embedded;
    AText := Copy(AMessage, vPart.BodyStart, vPart.BodyLen);
    Result := True;
  end
  else if (vSpec = 'HEADER') or (vSpec = 'HEADER.FIELDS') or (vSpec = 'HEADER.FIELDS.NOT') then
  begin
    if (vPath <> '') and (vPart.Embedded <> nil) then
      vPart := vPart.Embedded
    else if (vPath <> '') and not ((vPart.MediaType = 'message') and (vPart.SubType = 'rfc822')) then
      Exit;
    AText := Copy(AMessage, vPart.HeaderStart, vPart.HeaderLen);
    if vSpec <> 'HEADER' then
    begin
      vList := TStringList.Create;
      vPieces := TStringList.Create;
      try
        vPieces.Delimiter := ' ';
        vPieces.StrictDelimiter := True;
        vPieces.DelimitedText := string(RALMailLower(RALMailTrim(vFields)));
        for vInt := 0 to vPieces.Count - 1 do
          if vPieces[vInt] <> '' then
            vList.Add(StringReplace(vPieces[vInt], '"', '', [rfReplaceAll]));
        AText := FilterHeader(AText, vList, vSpec = 'HEADER.FIELDS.NOT');
      finally
        vPieces.Free;
        vList.Free;
      end;
    end;
    Result := True;
  end
  else if vSpec = 'MIME' then
  begin
    if vPath = '' then
      Exit;
    AText := Copy(AMessage, vPart.HeaderStart, vPart.HeaderLen);
    Result := True;
  end;
end;

{ FETCH items }

/// Reads the items of a FETCH (a list or a macro) into ASpec; False when invalid.
function ParseFetch(const AText: RawByteString; ASpec: TFetchSpec): boolean;
var
  vText, vName, vInner, vPart: RawByteString;
  vPos, vClose, vDot: IntegerRAL;
  vItem: TFetchItem;

  procedure Add(AKind: TFetchKind);
  begin
    vItem := Default(TFetchItem);
    vItem.Kind := AKind;
    vItem.PartLength := -1;
    SetLength(ASpec.Items, Length(ASpec.Items) + 1);
    ASpec.Items[High(ASpec.Items)] := vItem;
  end;

begin
  vText := RALMailTrim(AText);
  if (vText <> '') and (vText[1] = '(') then
  begin
    vPos := 1;
    if ReadList(vText, vPos, vInner) then
      vText := vInner;
  end;
  vText := RALMailTrim(vText);
  if RALMailUpper(vText) = 'ALL' then
    vText := 'FLAGS INTERNALDATE RFC822.SIZE ENVELOPE'
  else if RALMailUpper(vText) = 'FAST' then
    vText := 'FLAGS INTERNALDATE RFC822.SIZE'
  else if RALMailUpper(vText) = 'FULL' then
    vText := 'FLAGS INTERNALDATE RFC822.SIZE ENVELOPE BODY';
  vPos := 1;
  while True do
  begin
    vName := ReadAtom(vText, vPos);
    if vName = '' then
      Break;
    vPart := RALMailUpper(vName);
    if vPart = 'FLAGS' then
      Add(fkFlags)
    else if vPart = 'UID' then
      Add(fkUID)
    else if vPart = 'INTERNALDATE' then
      Add(fkInternalDate)
    else if vPart = 'RFC822.SIZE' then
      Add(fkSize)
    else if vPart = 'ENVELOPE' then
      Add(fkEnvelope)
    else if vPart = 'BODYSTRUCTURE' then
      Add(fkStructure)
    else if vPart = 'BODY' then
      Add(fkBodyBasic)
    else if vPart = 'RFC822' then
      Add(fkRFC822)
    else if vPart = 'RFC822.HEADER' then
      Add(fkRFC822Header)
    else if vPart = 'RFC822.TEXT' then
      Add(fkRFC822Text)
    else if (Copy(vPart, 1, 5) = 'BODY[') or (Copy(vPart, 1, 10) = 'BODY.PEEK[') then
    begin
      Add(fkSection);
      vItem := ASpec.Items[High(ASpec.Items)];
      vItem.Peek := Copy(vPart, 1, 10) = 'BODY.PEEK[';
      vDot := Pos(RawByteString('['), vName);
      vClose := Length(vName);
      while (vClose > vDot) and (vName[vClose] <> ']') do
        Dec(vClose);
      if vClose <= vDot then
      begin
        Result := False;
        Exit;
      end;
      vItem.Section := Copy(vName, vDot + 1, vClose - vDot - 1);
      if (vClose < Length(vName)) and (vName[vClose + 1] = '<') then
      begin
        vInner := Copy(vName, vClose + 2, Length(vName) - vClose - 2);
        vDot := Pos(RawByteString('.'), vInner);
        vItem.HasPartial := True;
        if vDot > 0 then
        begin
          vItem.PartStart := StrToInt64Def(string(Copy(vInner, 1, vDot - 1)), 0);
          vItem.PartLength := StrToInt64Def(string(Copy(vInner, vDot + 1, MaxInt)), 0);
        end
        else
          vItem.PartStart := StrToInt64Def(string(vInner), 0);
      end;
      ASpec.Items[High(ASpec.Items)] := vItem;
    end
    else
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := Length(ASpec.Items) > 0;
  for vPos := 0 to High(ASpec.Items) do
  begin
    if ASpec.Items[vPos].Kind in [fkEnvelope, fkStructure, fkBodyBasic, fkSection, fkRFC822,
       fkRFC822Header, fkRFC822Text] then
      ASpec.NeedMessage := True;
    if ((ASpec.Items[vPos].Kind = fkSection) and not ASpec.Items[vPos].Peek) or
       (ASpec.Items[vPos].Kind in [fkRFC822, fkRFC822Text]) then
      ASpec.SetsSeen := True;
  end;
end;

{ SEARCH }

type
  /// The kinds of search key.
  TSearchKind = (skAll, skFlag, skHeaderText, skBody, skText, skDate, skSize, skUID, skSeq, skNot,
    skOr, skAnd, skRecent);

  /// A node of a search key tree.
  TSearchNode = class
  public
    /// The date of the date keys.
    Date: TDateTime;
    /// For skDate: 0 since, 1 before, 2 on; 3 is added to use the Date header.
    DateOp: IntegerRAL;
    /// The header for skHeaderText.
    Field: RawByteString;
    /// The flag of skFlag (canonical).
    Flag: StringRAL;
    /// What the item or node is.
    Kind: TSearchKind;
    /// skSize: True for LARGER.
    Larger: boolean;
    /// skFlag: the message must not have it.
    Negate: boolean;
    /// The number of LARGER and SMALLER, or a flag of RECENT, NEW and OLD.
    Num: Int64;
    /// The set of UID and sequence keys.
    Ranges: TSetRanges;
    /// The keys inside a negation, an alternative or a list.
    Subs: TList;
    /// The text searched, lower case.
    Text: RawByteString;

    /// The create.
    constructor Create(AKind: TSearchKind);
    destructor Destroy; override;
  end;

  /// Loads message texts for a search, keeping the last.
  TSearchContext = class
  public
    /// The text of the last message loaded.
    CachedText: RawByteString;
    /// The UID of the last message loaded.
    CachedUID: Cardinal;
    /// Bytes of the header, with the empty line.
    HeaderLen: IntegerRAL;
    /// Reads the text of a message.
    Loader: function(AUID: Cardinal): RawByteString of object;
    /// The largest sequence number.
    MaxSeq: Int64;
    /// The largest UID.
    MaxUID: Int64;
    /// The first UID that is recent.
    RecentFrom: Cardinal;

    /// The text of the message AUID.
    function MessageText(AUID: Cardinal): RawByteString;
  end;

constructor TSearchNode.Create(AKind: TSearchKind);
begin
  inherited Create;
  Kind := AKind;
  Subs := TList.Create;
end;

destructor TSearchNode.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Subs.Count - 1 do
    TObject(Subs[vInt]).Free;
  Subs.Free;
  inherited Destroy;
end;

function TSearchContext.MessageText(AUID: Cardinal): RawByteString;
begin
  if (CachedUID <> AUID) or (CachedUID = 0) then
  begin
    CachedText := Loader(AUID);
    CachedUID := AUID;
    HeaderLen := HeaderEnd(CachedText, 1, Length(CachedText)) - 1;
  end;
  Result := CachedText;
end;

/// A search key from AText at APos; raises Exception when it is not valid.
function ParseSearchKey(const AText: RawByteString; var APos: IntegerRAL): TSearchNode;
var
  vWord, vUpper, vArg, vInner: RawByteString;
  vInt: IntegerRAL;
  vRanges: TSetRanges;

  procedure Bad;
  begin
    raise Exception.Create('bad search key');
  end;

  function Str: RawByteString;
  begin
    SkipBlanks(AText, APos);
    if APos > Length(AText) then
      Bad;
    Result := ReadString(AText, APos);
  end;

  function Flag(const AName: string; ANegate: boolean): TSearchNode;
  begin
    Result := TSearchNode.Create(skFlag);
    Result.Flag := StringRAL(AName);
    Result.Negate := ANegate;
  end;

  function HeaderKey(const AField: RawByteString): TSearchNode;
  begin
    Result := TSearchNode.Create(skHeaderText);
    Result.Field := AField;
    Result.Text := RALMailLower(Str);
  end;

  function DateKey(AOp: IntegerRAL): TSearchNode;
  begin
    Result := TSearchNode.Create(skDate);
    Result.DateOp := AOp;
    Result.Date := ParseSearchDate(Str);
    if Result.Date = 0 then
    begin
      Result.Free;
      Bad;
    end;
  end;

begin
  Result := nil;
  SkipBlanks(AText, APos);
  if (APos <= Length(AText)) and (AText[APos] = '(') then
  begin
    if not ReadList(AText, APos, vInner) then
      Bad;
    Result := TSearchNode.Create(skAnd);
    try
      vInt := 1;
      while True do
      begin
        SkipBlanks(vInner, vInt);
        if vInt > Length(vInner) then
          Break;
        Result.Subs.Add(ParseSearchKey(vInner, vInt));
      end;
    except
      Result.Free;
      raise;
    end;
    Exit;
  end;
  vWord := ReadAtom(AText, APos);
  vUpper := RALMailUpper(vWord);
  if vUpper = '' then
    Bad;
  if vUpper = 'ALL' then
    Result := TSearchNode.Create(skAll)
  else if vUpper = 'ANSWERED' then
    Result := Flag('\Answered', False)
  else if vUpper = 'UNANSWERED' then
    Result := Flag('\Answered', True)
  else if vUpper = 'DELETED' then
    Result := Flag('\Deleted', False)
  else if vUpper = 'UNDELETED' then
    Result := Flag('\Deleted', True)
  else if vUpper = 'DRAFT' then
    Result := Flag('\Draft', False)
  else if vUpper = 'UNDRAFT' then
    Result := Flag('\Draft', True)
  else if vUpper = 'FLAGGED' then
    Result := Flag('\Flagged', False)
  else if vUpper = 'UNFLAGGED' then
    Result := Flag('\Flagged', True)
  else if vUpper = 'SEEN' then
    Result := Flag('\Seen', False)
  else if vUpper = 'UNSEEN' then
    Result := Flag('\Seen', True)
  else if vUpper = 'KEYWORD' then
    Result := Flag(string(Str), False)
  else if vUpper = 'UNKEYWORD' then
    Result := Flag(string(Str), True)
  else if (vUpper = 'RECENT') or (vUpper = 'NEW') or (vUpper = 'OLD') then
  begin
    Result := TSearchNode.Create(skRecent);
    Result.Num := Ord(vUpper = 'OLD');
    Result.Negate := vUpper = 'NEW';
  end
  else if vUpper = 'FROM' then
    Result := HeaderKey('From')
  else if vUpper = 'TO' then
    Result := HeaderKey('To')
  else if vUpper = 'CC' then
    Result := HeaderKey('Cc')
  else if vUpper = 'BCC' then
    Result := HeaderKey('Bcc')
  else if vUpper = 'SUBJECT' then
    Result := HeaderKey('Subject')
  else if vUpper = 'HEADER' then
  begin
    vArg := Str;
    Result := HeaderKey(vArg);
  end
  else if vUpper = 'BODY' then
  begin
    Result := TSearchNode.Create(skBody);
    Result.Text := RALMailLower(Str);
  end
  else if vUpper = 'TEXT' then
  begin
    Result := TSearchNode.Create(skText);
    Result.Text := RALMailLower(Str);
  end
  else if vUpper = 'SINCE' then
    Result := DateKey(0)
  else if vUpper = 'BEFORE' then
    Result := DateKey(1)
  else if vUpper = 'ON' then
    Result := DateKey(2)
  else if vUpper = 'SENTSINCE' then
    Result := DateKey(3)
  else if vUpper = 'SENTBEFORE' then
    Result := DateKey(4)
  else if vUpper = 'SENTON' then
    Result := DateKey(5)
  else if (vUpper = 'LARGER') or (vUpper = 'SMALLER') then
  begin
    Result := TSearchNode.Create(skSize);
    Result.Larger := vUpper = 'LARGER';
    Result.Num := StrToInt64Def(string(Str), -1);
    if Result.Num < 0 then
    begin
      Result.Free;
      Bad;
    end;
  end
  else if vUpper = 'UID' then
  begin
    if not ParseSet(Str, vRanges) then
      Bad;
    Result := TSearchNode.Create(skUID);
    Result.Ranges := vRanges;
  end
  else if vUpper = 'NOT' then
  begin
    Result := TSearchNode.Create(skNot);
    try
      Result.Subs.Add(ParseSearchKey(AText, APos));
    except
      Result.Free;
      raise;
    end;
  end
  else if vUpper = 'OR' then
  begin
    Result := TSearchNode.Create(skOr);
    try
      Result.Subs.Add(ParseSearchKey(AText, APos));
      Result.Subs.Add(ParseSearchKey(AText, APos));
    except
      Result.Free;
      raise;
    end;
  end
  else if ParseSet(vWord, vRanges) then
  begin
    Result := TSearchNode.Create(skSeq);
    Result.Ranges := vRanges;
  end
  else
    Bad;
end;

/// Whether the header AField of the message holds AText (lower case), words decoded.
function HeaderHas(const AMessage: RawByteString; AHeaderLen: IntegerRAL; const AField, AText: RawByteString): boolean;
var
  vValue: RawByteString;
begin
  vValue := HeaderValue(Copy(AMessage, 1, AHeaderLen), AField);
  vValue := RALMailLower(RawByteString(RALMailDecodeWords(StringRAL(vValue))));
  Result := Pos(AText, vValue) > 0;
end;

/// Whether the message of AInfo matches ANode.
function Matches(ANode: TSearchNode; const AInfo: TRALMailMessageInfo; ASeq: Int64;
  ACtx: TSearchContext): boolean;
var
  vInt: IntegerRAL;
  vText: RawByteString;
  vDate: TDateTime;
begin
  Result := False;
  case ANode.Kind of
    skAll:
      Result := True;
    skFlag:
      Result := RALMailFlagHas(AInfo.Flags, ANode.Flag) <> ANode.Negate;
    skRecent:
    begin
      Result := AInfo.UID >= ACtx.RecentFrom;
      if ANode.Num = 1 then
        Result := not Result;
      if ANode.Negate then
        Result := Result and not RALMailFlagHas(AInfo.Flags, '\Seen');
    end;
    skHeaderText:
    begin
      vText := ACtx.MessageText(AInfo.UID);
      Result := HeaderHas(vText, ACtx.HeaderLen, ANode.Field, ANode.Text);
    end;
    skBody:
    begin
      vText := ACtx.MessageText(AInfo.UID);
      Result := Pos(ANode.Text, RALMailLower(Copy(vText, ACtx.HeaderLen + 1, MaxInt))) > 0;
    end;
    skText:
    begin
      vText := ACtx.MessageText(AInfo.UID);
      Result := Pos(ANode.Text, RALMailLower(vText)) > 0;
    end;
    skDate:
    begin
      if ANode.DateOp >= 3 then
      begin
        vText := ACtx.MessageText(AInfo.UID);
        vDate := RALMailParseDate(StringRAL(HeaderValue(Copy(vText, 1, ACtx.HeaderLen), 'Date')));
        if vDate = 0 then
          vDate := AInfo.Date;
      end
      else
        vDate := AInfo.Date;
      vDate := Trunc(vDate);
      case ANode.DateOp mod 3 of
        0: Result := vDate >= ANode.Date;
        1: Result := vDate < ANode.Date;
      else
        Result := vDate = ANode.Date;
      end;
    end;
    skSize:
      if ANode.Larger then
        Result := AInfo.Size > ANode.Num
      else
        Result := AInfo.Size < ANode.Num;
    skUID:
      Result := InSet(ANode.Ranges, AInfo.UID, ACtx.MaxUID);
    skSeq:
      Result := InSet(ANode.Ranges, ASeq, ACtx.MaxSeq);
    skNot:
      Result := not Matches(TSearchNode(ANode.Subs[0]), AInfo, ASeq, ACtx);
    skOr:
      Result := Matches(TSearchNode(ANode.Subs[0]), AInfo, ASeq, ACtx) or
        Matches(TSearchNode(ANode.Subs[1]), AInfo, ASeq, ACtx);
    skAnd:
    begin
      Result := True;
      for vInt := 0 to ANode.Subs.Count - 1 do
        if not Matches(TSearchNode(ANode.Subs[vInt]), AInfo, ASeq, ACtx) then
        begin
          Result := False;
          Break;
        end;
    end;
  end;
end;

/// Whether AName matches the LIST pattern APattern ('*' any, '%' any but the delimiter).
function GlobMatch(const APattern, AName: RawByteString; APat, AIdx: IntegerRAL): boolean;
begin
  while APat <= Length(APattern) do
  begin
    case APattern[APat] of
      '*', '%':
      begin
        // try every length of the rest
        while True do
        begin
          if GlobMatch(APattern, AName, APat + 1, AIdx) then
          begin
            Result := True;
            Exit;
          end;
          if (AIdx > Length(AName)) or ((APattern[APat] = '%') and (AName[AIdx] = '/')) then
          begin
            Result := False;
            Exit;
          end;
          Inc(AIdx);
        end;
      end;
    else
      if (AIdx > Length(AName)) or (APattern[APat] <> AName[AIdx]) then
      begin
        Result := False;
        Exit;
      end;
      Inc(AIdx);
    end;
    Inc(APat);
  end;
  Result := AIdx > Length(AName);
end;

{ TRALIMAPConnection }

constructor TRALIMAPConnection.Create(AServer: TRALIMAPServer; ASocket: TRALSocket;
  AListener: IntegerRAL);
begin
  inherited Create(AServer, ASocket, AListener);
  FServer := AServer;
end;

destructor TRALIMAPConnection.Destroy;
begin
  FreeAndNil(FAppendStream);
  FreeAndNil(FSASL);
  FreeAndNil(FFetch);
  inherited Destroy;
end;

procedure TRALIMAPConnection.Closed;
begin
  FreeAndNil(FFetch);
  FJobActive := False;
end;

function TRALIMAPConnection.HasOutput: boolean;
begin
  // a FETCH in progress keeps the poll awake for the next messages
  Result := inherited HasOutput or FJobActive;
end;

function TRALIMAPConnection.Resting: boolean;
begin
  Result := (FInput = iiIdle) or FJobActive or (PendingOutput > 0);
end;

function TRALIMAPConnection.CapabilityText: RawByteString;
begin
  Result := 'IMAP4rev2 IMAP4rev1';
  if not Session.TLS and FServer.SSL.Enabled then
    Result := Result + ' STARTTLS';
  if not PlainAllowed then
    Result := Result + ' LOGINDISABLED';
  if PlainAllowed then
    Result := Result + ' AUTH=PLAIN';
  if ScramAllowed then
    Result := Result + ' AUTH=SCRAM-SHA-256';
  Result := Result + ' SASL-IR LITERAL+ UIDPLUS MOVE IDLE UNSELECT ENABLE ID NAMESPACE CHILDREN UTF8=ACCEPT';
end;

function TRALIMAPConnection.Greeting: RawByteString;
begin
  Result := '* OK [CAPABILITY ' + CapabilityText + '] ' + RawByteString(FServer.HostName) + ' IMAP server ready';
  if FServer.Banner <> '' then
    Result := Result + ' ' + RawByteString(FServer.Banner);
  Result := Result + cCRLF;
end;

function TRALIMAPConnection.RefusedReply(ATooMany: boolean): RawByteString;
begin
  if ATooMany then
    Result := '* BYE [UNAVAILABLE] too many connections, try later'#13#10
  else
    Result := '* BYE access denied'#13#10;
end;

function TRALIMAPConnection.FaultReply(const AMessage: StringRAL): RawByteString;
begin
  Result := '* BYE [SERVERBUG] internal error'#13#10;
end;

function TRALIMAPConnection.StopReply: RawByteString;
begin
  Result := '* BYE server shutting down'#13#10;
end;

procedure TRALIMAPConnection.TLSStarted;
begin
  FCmd := '';
  FInput := iiCommand;
end;

function TRALIMAPConnection.PlainAllowed: boolean;
begin
  Result := (msaPlain in FServer.AuthMethods) and
    (Session.TLS or (FServer.AllowPlainAuthWithoutTLS and not FServer.RequireTLS));
end;

function TRALIMAPConnection.ScramAllowed: boolean;
begin
  Result := (msaScramSHA256 in FServer.AuthMethods) and (Session.TLS or not FServer.RequireTLS);
end;

function TRALIMAPConnection.DecodeName(const AName: RawByteString): StringRAL;
begin
  if FEnabledUTF8 then
    Result := StringRAL(AName)
  else
    Result := RALIMAPDecodeUTF7(AName);
end;

function TRALIMAPConnection.EncodeName(const AName: StringRAL): RawByteString;
begin
  if FEnabledUTF8 then
    Result := RawByteString(AName)
  else
    Result := RALIMAPEncodeUTF7(AName);
end;

procedure TRALIMAPConnection.Done(const ATag, AStatus, AText: RawByteString);
begin
  // what changed in the mailbox is told before the command completes
  if (FSelected <> '') and not FInSync and not CloseAfterFlush and not FJobActive then
  begin
    FInSync := True;
    try
      SyncView(not FNoExpunge);
    finally
      FInSync := False;
    end;
  end;
  Send(ATag + ' ' + AStatus + ' ' + AText + cCRLF);
end;

procedure TRALIMAPConnection.StoreFailure(const ATag: RawByteString; AError: ERALMailStoreError);
begin
  case AError.Kind of
    mekNoMailbox:
      Done(ATag, 'NO', '[NONEXISTENT] ' + RawByteString(AError.Message));
    mekExists:
      Done(ATag, 'NO', '[ALREADYEXISTS] ' + RawByteString(AError.Message));
    mekBadName, mekProtected:
      Done(ATag, 'NO', RawByteString(AError.Message));
  else
    Done(ATag, 'NO', '[SERVERBUG] ' + RawByteString(AError.Message));
  end;
end;

function TRALIMAPConnection.LoadText(AUID: Cardinal): RawByteString;
var
  vStream: TStream;
begin
  Result := '';
  vStream := FServer.Store.OpenMessage(Session.UserName, FSelected, AUID);
  if vStream = nil then
    Exit;
  try
    SetLength(Result, vStream.Size);
    if Length(Result) > 0 then
      vStream.ReadBuffer(Pointer(Result)^, Length(Result));
  finally
    vStream.Free;
  end;
end;

function TRALIMAPConnection.ViewFind(AUID: Cardinal): IntegerRAL;
var
  vLow, vHigh, vMid: IntegerRAL;
begin
  Result := -1;
  vLow := 0;
  vHigh := FViewCount - 1;
  while vLow <= vHigh do
  begin
    vMid := (vLow + vHigh) div 2;
    if FView[vMid].UID = AUID then
    begin
      Result := vMid;
      Exit;
    end
    else if FView[vMid].UID < AUID then
      vLow := vMid + 1
    else
      vHigh := vMid - 1;
  end;
end;

function TRALIMAPConnection.RecentCount: IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  for vInt := 0 to FViewCount - 1 do
    if FView[vInt].UID >= FRecentFrom then
      Inc(Result);
end;

function TRALIMAPConnection.ResolveSet(const ASet: RawByteString; AUID: boolean): TRALMailUIDs;
var
  vRanges: TSetRanges;
  vInt, vCount: IntegerRAL;
  vMax, vValue: Int64;
begin
  SetLength(Result, 0);
  if not ParseSet(ASet, vRanges) then
    raise Exception.Create('bad set');
  SetLength(Result, FViewCount);
  vCount := 0;
  if AUID then
  begin
    vMax := 0;
    if FViewCount > 0 then
      vMax := FView[FViewCount - 1].UID;
  end
  else
    vMax := FViewCount;
  for vInt := 0 to FViewCount - 1 do
  begin
    if AUID then
      vValue := FView[vInt].UID
    else
      vValue := vInt + 1;
    if InSet(vRanges, vValue, vMax) then
    begin
      Result[vCount] := vInt;
      Inc(vCount);
    end;
  end;
  SetLength(Result, vCount);
end;

procedure TRALIMAPConnection.Unselect;
begin
  FSelected := '';
  FViewCount := 0;
  SetLength(FView, 0);
  FReadOnly := False;
end;

procedure TRALIMAPConnection.LoadView;
var
  vNext: Cardinal;
begin
  FSyncVersion := FServer.Store.MailboxVersion(Session.UserName, FSelected);
  FView := FServer.Store.ListMessages(Session.UserName, FSelected, FValidity, vNext);
  FViewCount := Length(FView);
  FRecentFrom := vNext;
end;

procedure TRALIMAPConnection.SendFlags(AIndex: IntegerRAL; AUID: boolean);
begin
  Send('* ' + IStr(AIndex + 1) + ' FETCH (FLAGS ' + FlagList(FView[AIndex].Flags));
  if AUID then
    Send(' UID ' + IStr(FView[AIndex].UID));
  Send(')'#13#10);
end;

procedure TRALIMAPConnection.RemoveFromView(const AUIDs: TRALMailUIDs; ASilent: boolean);
var
  vInt, vKept, vRemoved: IntegerRAL;
  vWanted: TStringList;
begin
  vWanted := TStringList.Create;
  try
    vWanted.Sorted := True;
    for vInt := 0 to High(AUIDs) do
      vWanted.Add(Format('%.12d', [Int64(AUIDs[vInt])]));
    vKept := 0;
    vRemoved := 0;
    for vInt := 0 to FViewCount - 1 do
      if vWanted.IndexOf(Format('%.12d', [Int64(FView[vInt].UID)])) >= 0 then
      begin
        if not ASilent then
          Send('* ' + IStr(vInt + 1 - vRemoved) + ' EXPUNGE'#13#10);
        Inc(vRemoved);
      end
      else
      begin
        FView[vKept] := FView[vInt];
        Inc(vKept);
      end;
    FViewCount := vKept;
  finally
    vWanted.Free;
  end;
end;

procedure TRALIMAPConnection.SyncView(AAllowExpunge: boolean);
var
  vVersion: Int64;
  vInfos, vNew: TRALMailMessageInfos;
  vNext, vValidity: Cardinal;
  vOld, vJ, vCount, vInt, vAdded: IntegerRAL;
  vRemovedSeq, vUpdated: array of IntegerRAL;
  vSkipped: boolean;
begin
  if (FSelected = '') or FJobActive then
    Exit;
  vVersion := FServer.Store.MailboxVersion(Session.UserName, FSelected);
  if vVersion = FSyncVersion then
    Exit;
  if vVersion < 0 then
  begin
    Send('* BYE the selected mailbox was deleted'#13#10);
    CloseAfterFlush := True;
    Exit;
  end;
  vInfos := FServer.Store.ListMessages(Session.UserName, FSelected, vValidity, vNext);
  vOld := FViewCount;
  SetLength(vNew, vOld + Length(vInfos));
  SetLength(vRemovedSeq, 0);
  SetLength(vUpdated, 0);
  vCount := 0;
  vJ := 0;
  vSkipped := False;
  vInt := 0;
  while vInt < vOld do
  begin
    if (vJ < Length(vInfos)) and (vInfos[vJ].UID = FView[vInt].UID) then
    begin
      if vInfos[vJ].Flags <> FView[vInt].Flags then
      begin
        SetLength(vUpdated, Length(vUpdated) + 1);
        vUpdated[High(vUpdated)] := vCount;
      end;
      vNew[vCount] := vInfos[vJ];
      Inc(vCount);
      Inc(vInt);
      Inc(vJ);
    end
    else if (vJ >= Length(vInfos)) or (vInfos[vJ].UID > FView[vInt].UID) then
    begin
      if AAllowExpunge then
      begin
        SetLength(vRemovedSeq, Length(vRemovedSeq) + 1);
        vRemovedSeq[High(vRemovedSeq)] := vInt + 1;
      end
      else
      begin
        // the client still has it: it goes at the next opportunity
        vNew[vCount] := FView[vInt];
        Inc(vCount);
        vSkipped := True;
      end;
      Inc(vInt);
    end
    else
      Inc(vJ);
  end;
  vAdded := 0;
  while vJ < Length(vInfos) do
  begin
    vNew[vCount] := vInfos[vJ];
    Inc(vCount);
    Inc(vAdded);
    Inc(vJ);
  end;
  for vInt := 0 to High(vRemovedSeq) do
    Send('* ' + IStr(vRemovedSeq[vInt] - vInt) + ' EXPUNGE'#13#10);
  FView := vNew;
  FViewCount := vCount;
  if vAdded > 0 then
  begin
    Send('* ' + IStr(vCount) + ' EXISTS'#13#10);
    if not FEnabledRev2 then
      Send('* ' + IStr(RecentCount) + ' RECENT'#13#10);
  end;
  for vInt := 0 to High(vUpdated) do
    SendFlags(vUpdated[vInt], False);
  if vSkipped then
    FSyncVersion := -2
  else
    FSyncVersion := vVersion;
end;

procedure TRALIMAPConnection.Tick(ANow: Int64RAL);
begin
  if FJobActive then
    Pump
  else if (FInput = iiIdle) and (FSelected <> '') then
    SyncView(True);
end;

procedure TRALIMAPConnection.ProcessInput;
var
  vEnd, vAvail, vTake: IntegerRAL;
  vLine: RawByteString;
begin
  FInProcess := True;
  try
    while (not IsClosed) and (not CloseAfterFlush) and (not TLSPending) and (not FJobActive) do
    begin
      if FInput in [iiLiteral, iiAppend, iiSkip] then
      begin
        vAvail := Length(FIn) - FInPos + 1;
        vTake := vAvail;
        if vTake > FLitLeft then
          vTake := FLitLeft;
        if vTake > 0 then
        begin
          case FInput of
            iiLiteral:
              FLitText := FLitText + Copy(FIn, FInPos, vTake);
            iiAppend:
              FAppendStream.WriteBuffer(FIn[FInPos], vTake);
          end;
          Inc(FInPos, vTake);
          Dec(FLitLeft, vTake);
        end;
        if FLitLeft > 0 then
          Break;
        case FInput of
          iiLiteral:
          begin
            FCmd := FCmd + Quoted(FLitText);
            FLitText := '';
          end;
          iiSkip:
          begin
            Send(FSkipReply);
            FCmd := '';
          end;
        end;
        FInput := iiCommand;
        Continue;
      end;
      vEnd := FInPos;
      while (vEnd <= Length(FIn)) and (FIn[vEnd] <> #10) do
        Inc(vEnd);
      if vEnd > Length(FIn) then
      begin
        if Length(FIn) - FInPos + 1 > cMaxCommand then
        begin
          Send('* BYE command too long'#13#10);
          CloseAfterFlush := True;
        end;
        Break;
      end;
      vLine := Copy(FIn, FInPos, vEnd - FInPos);
      if (vLine <> '') and (vLine[Length(vLine)] = #13) then
        SetLength(vLine, Length(vLine) - 1);
      FInPos := vEnd + 1;
      case FInput of
        iiIdle:
          IdleLine(vLine);
        iiSASL:
          SASLLine(vLine);
      else
        InputLine(vLine);
      end;
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

function TRALIMAPConnection.LiteralMarker(var ALine: RawByteString): boolean;
var
  vOpen, vInt: IntegerRAL;
  vDigits: RawByteString;
begin
  Result := False;
  if (ALine = '') or (ALine[Length(ALine)] <> '}') then
    Exit;
  vOpen := Length(ALine);
  while (vOpen > 0) and (ALine[vOpen] <> '{') do
    Dec(vOpen);
  if vOpen = 0 then
    Exit;
  vDigits := Copy(ALine, vOpen + 1, Length(ALine) - vOpen - 1);
  FLitSync := True;
  if (vDigits <> '') and (vDigits[Length(vDigits)] = '+') then
  begin
    FLitSync := False;
    SetLength(vDigits, Length(vDigits) - 1);
  end;
  if vDigits = '' then
    Exit;
  for vInt := 1 to Length(vDigits) do
    if not (vDigits[vInt] in ['0'..'9']) then
      Exit;
  FLitLeft := StrToInt64Def(string(vDigits), -1);
  if FLitLeft < 0 then
    Exit;
  ALine := Copy(ALine, 1, vOpen - 1);
  Result := True;
end;

procedure TRALIMAPConnection.BeginLiteral;
var
  vTag: RawByteString;
  vPos: IntegerRAL;
begin
  vPos := 1;
  vTag := ReadAtom(FCmd, vPos);
  if FLitLeft > cMaxLiteral then
  begin
    if FLitSync then
    begin
      Done(vTag, 'BAD', '[TOOBIG] literal too large');
      FCmd := '';
    end
    else
    begin
      FSkipReply := vTag + ' BAD [TOOBIG] literal too large' + cCRLF;
      FInput := iiSkip;
    end;
    Exit;
  end;
  if FLitSync then
    Send('+ Ready for literal data'#13#10);
  FLitText := '';
  FInput := iiLiteral;
end;

function TRALIMAPConnection.BeginAppend: boolean;
var
  vPos: IntegerRAL;
  vInner, vDate: RawByteString;
  vRefusal: RawByteString;
  vName: StringRAL;
begin
  Result := False;
  vPos := 1;
  FAppendTag := ReadAtom(FCmd, vPos);
  ReadAtom(FCmd, vPos);
  vRefusal := '';
  if not Session.Authenticated then
    vRefusal := FAppendTag + ' BAD command not valid in this state'
  else
  begin
    vName := DecodeName(ReadString(FCmd, vPos));
    FAppendFlags := '';
    FAppendDate := 0;
    if ReadList(FCmd, vPos, vInner) then
      FAppendFlags := StringRAL(vInner);
    SkipBlanks(FCmd, vPos);
    if (vPos <= Length(FCmd)) and (FCmd[vPos] = '"') then
    begin
      vDate := ReadString(FCmd, vPos);
      FAppendDate := ParseInternalDate(vDate);
    end;
    FAppendMailbox := vName;
    if not FServer.Store.MailboxExists(Session.UserName, vName) then
      vRefusal := FAppendTag + ' NO [TRYCREATE] the mailbox does not exist'
    else if (FServer.MaxMessageSize > 0) and (FLitLeft > FServer.MaxMessageSize) then
      vRefusal := FAppendTag + ' NO [TOOBIG] the message is too large';
  end;
  if vRefusal <> '' then
  begin
    if FLitSync then
    begin
      Send(vRefusal + cCRLF);
      FCmd := '';
    end
    else
    begin
      FSkipReply := vRefusal + cCRLF;
      FInput := iiSkip;
    end;
    Exit;
  end;
  FAppendStream := TMemoryStream.Create;
  if FLitSync then
    Send('+ Ready for literal data'#13#10);
  FInput := iiAppend;
  Result := True;
end;

procedure TRALIMAPConnection.CmdAppendDone;
var
  vUID: Cardinal;
  vStatus: TRALMailboxStatus;
begin
  FAppendStream.Position := 0;
  try
    try
      vUID := FServer.Store.Append(Session.UserName, FAppendMailbox, FAppendStream, FAppendFlags, FAppendDate);
      vStatus := FServer.Store.Status(Session.UserName, FAppendMailbox);
      Done(FAppendTag, 'OK', '[APPENDUID ' + IStr(vStatus.UIDValidity) + ' ' + IStr(vUID) + '] APPEND completed');
    except
      on E: ERALMailStoreError do
        if E.Kind = mekNoMailbox then
          Done(FAppendTag, 'NO', '[TRYCREATE] the mailbox does not exist')
        else
          StoreFailure(FAppendTag, E);
    end;
  finally
    FreeAndNil(FAppendStream);
    FCmd := '';
  end;
end;

procedure TRALIMAPConnection.InputLine(const ALine: RawByteString);
var
  vCmd: RawByteString;
  vPos: IntegerRAL;
begin
  if FAppendStream <> nil then
  begin
    // the rest of the APPEND line after the message
    if RALMailTrim(ALine) <> '' then
    begin
      Done(FAppendTag, 'BAD', 'unexpected data after the message');
      FreeAndNil(FAppendStream);
      FCmd := '';
    end
    else
      CmdAppendDone;
    Exit;
  end;
  FCmd := FCmd + ALine;
  if Length(FCmd) > cMaxCommand then
  begin
    Send('* BYE command too long'#13#10);
    CloseAfterFlush := True;
    Exit;
  end;
  if LiteralMarker(FCmd) then
  begin
    vPos := 1;
    ReadAtom(FCmd, vPos);
    if RALMailUpper(ReadAtom(FCmd, vPos)) = 'APPEND' then
      BeginAppend
    else
      BeginLiteral;
    Exit;
  end;
  vCmd := FCmd;
  FCmd := '';
  Execute(vCmd);
end;

procedure TRALIMAPConnection.IdleLine(const ALine: RawByteString);
begin
  if RALMailUpper(RALMailTrim(ALine)) = 'DONE' then
  begin
    FInput := iiCommand;
    FNoExpunge := False;
    Done(FIdleTag, 'OK', 'IDLE terminated');
  end
  else
  begin
    FInput := iiCommand;
    Done(FIdleTag, 'BAD', 'IDLE expects DONE');
  end;
end;

procedure TRALIMAPConnection.Execute(const ALine: RawByteString);
var
  vPos: IntegerRAL;
  vTag, vCmd, vRest, vSub: RawByteString;
  vUID, vSelected: boolean;
begin
  vPos := 1;
  vTag := ReadAtom(ALine, vPos);
  vCmd := RALMailUpper(ReadAtom(ALine, vPos));
  vRest := RALMailTrim(Copy(ALine, vPos, MaxInt));
  if vTag = '' then
  begin
    Send('* BAD missing tag'#13#10);
    Exit;
  end;
  if vCmd = '' then
  begin
    Done(vTag, 'BAD', 'missing command');
    Exit;
  end;
  vUID := False;
  if vCmd = 'UID' then
  begin
    vPos := 1;
    vSub := RALMailUpper(ReadAtom(vRest, vPos));
    vRest := RALMailTrim(Copy(vRest, vPos, MaxInt));
    vUID := True;
    vCmd := vSub;
    if (vCmd <> 'FETCH') and (vCmd <> 'STORE') and (vCmd <> 'SEARCH') and (vCmd <> 'COPY') and
       (vCmd <> 'MOVE') and (vCmd <> 'EXPUNGE') then
    begin
      Done(vTag, 'BAD', 'unknown UID command');
      Exit;
    end;
  end;
  vSelected := FSelected <> '';
  // EXPUNGE responses may not come during FETCH, STORE and SEARCH (RFC 3501 7.4.1)
  FNoExpunge := (vCmd = 'FETCH') or (vCmd = 'STORE') or (vCmd = 'SEARCH');
  try
    if vCmd = 'CAPABILITY' then
    begin
      Send('* CAPABILITY ' + CapabilityText + cCRLF);
      Done(vTag, 'OK', 'CAPABILITY completed');
    end
    else if vCmd = 'NOOP' then
      Done(vTag, 'OK', 'NOOP completed')
    else if vCmd = 'LOGOUT' then
    begin
      Send('* BYE logging out'#13#10);
      Done(vTag, 'OK', 'LOGOUT completed');
      CloseAfterFlush := True;
      Exit;
    end
    else if vCmd = 'ID' then
    begin
      Send('* ID ("name" "PascalRAL" "version" "' + RawByteString(RALVERSION) + '")'#13#10);
      Done(vTag, 'OK', 'ID completed');
    end
    else if vCmd = 'STARTTLS' then
    begin
      if Session.TLS then
        Done(vTag, 'BAD', 'TLS is already active')
      else if Session.Authenticated then
        Done(vTag, 'BAD', 'STARTTLS is not valid after login')
      else if not FServer.SSL.Enabled then
        Done(vTag, 'NO', 'TLS is not available')
      else
      begin
        Done(vTag, 'OK', 'Begin TLS negotiation now');
        BeginTLS;
      end;
      Exit;
    end
    else if vCmd = 'LOGIN' then
      CmdLogin(vTag, vRest)
    else if vCmd = 'AUTHENTICATE' then
      CmdAuthenticate(vTag, vRest)
    else if not Session.Authenticated then
      Done(vTag, 'BAD', 'command not valid in this state, log in first')
    else if vCmd = 'ENABLE' then
      CmdEnable(vTag, vRest)
    else if vCmd = 'NAMESPACE' then
    begin
      Send('* NAMESPACE (("" "/")) NIL NIL'#13#10);
      Done(vTag, 'OK', 'NAMESPACE completed');
    end
    else if vCmd = 'SELECT' then
      CmdSelect(vTag, vRest, False)
    else if vCmd = 'EXAMINE' then
      CmdSelect(vTag, vRest, True)
    else if (vCmd = 'CREATE') or (vCmd = 'DELETE') then
      CmdCreateDelete(vTag, vCmd, vRest)
    else if vCmd = 'RENAME' then
      CmdRename(vTag, vRest)
    else if (vCmd = 'SUBSCRIBE') or (vCmd = 'UNSUBSCRIBE') then
      CmdSubscribe(vTag, vCmd, vRest)
    else if (vCmd = 'LIST') or (vCmd = 'LSUB') then
      CmdList(vTag, vCmd, vRest)
    else if vCmd = 'STATUS' then
      CmdStatus(vTag, vRest)
    else if vCmd = 'APPEND' then
      Done(vTag, 'BAD', 'APPEND needs a literal')
    else if vCmd = 'IDLE' then
    begin
      Send('+ idling'#13#10);
      FIdleTag := vTag;
      FInput := iiIdle;
      Exit;
    end
    else if not vSelected then
      Done(vTag, 'BAD', 'command not valid in this state, select a mailbox first')
    else if vCmd = 'CHECK' then
      Done(vTag, 'OK', 'CHECK completed')
    else if vCmd = 'CLOSE' then
    begin
      if not FReadOnly then
        FServer.Store.Expunge(Session.UserName, FSelected);
      Unselect;
      Done(vTag, 'OK', 'CLOSE completed');
      Exit;
    end
    else if vCmd = 'UNSELECT' then
    begin
      Unselect;
      Done(vTag, 'OK', 'UNSELECT completed');
      Exit;
    end
    else if vCmd = 'EXPUNGE' then
      CmdExpunge(vTag, vRest, vUID)
    else if vCmd = 'SEARCH' then
      CmdSearch(vTag, vRest, vUID)
    else if vCmd = 'FETCH' then
    begin
      CmdFetch(vTag, vRest, vUID);
      Exit;
    end
    else if vCmd = 'STORE' then
      CmdStore(vTag, vRest, vUID)
    else if vCmd = 'COPY' then
      CmdCopy(vTag, vRest, vUID, False)
    else if vCmd = 'MOVE' then
      CmdCopy(vTag, vRest, vUID, True)
    else
      Done(vTag, 'BAD', 'unknown command');
  except
    on E: ERALMailStoreError do
      StoreFailure(vTag, E);
    on E: ERALSocketError do
      raise;
    on E: Exception do
      Done(vTag, 'BAD', 'invalid arguments');
  end;
end;

procedure TRALIMAPConnection.CmdLogin(const ATag, AArgs: RawByteString);
var
  vPos: IntegerRAL;
  vUser, vPass: RawByteString;
  vTemp: boolean;
begin
  if Session.Authenticated then
  begin
    Done(ATag, 'BAD', 'already logged in');
    Exit;
  end;
  if not PlainAllowed then
  begin
    Done(ATag, 'NO', '[PRIVACYREQUIRED] LOGIN is allowed only over TLS');
    Exit;
  end;
  vPos := 1;
  vUser := ReadString(AArgs, vPos);
  vPass := ReadString(AArgs, vPos);
  vTemp := False;
  if FServer.CheckPassword(Session, StringRAL(vUser), StringRAL(vPass), vTemp) then
  begin
    SetLoggedIn(StringRAL(vUser));
    Done(ATag, 'OK', '[CAPABILITY ' + CapabilityText + '] LOGIN completed');
  end
  else if vTemp then
    Done(ATag, 'NO', '[UNAVAILABLE] try later')
  else
  begin
    Inc(FBadLogins);
    Done(ATag, 'NO', '[AUTHENTICATIONFAILED] authentication failed');
    if FBadLogins >= 10 then
      CloseAfterFlush := True;
  end;
end;

procedure TRALIMAPConnection.CmdAuthenticate(const ATag, AArgs: RawByteString);
var
  vPos: IntegerRAL;
  vMech, vInitial: RawByteString;
  vMethod: TRALMailServerAuthMethod;
begin
  if Session.Authenticated then
  begin
    Done(ATag, 'BAD', 'already logged in');
    Exit;
  end;
  vPos := 1;
  vMech := RALMailUpper(ReadAtom(AArgs, vPos));
  vInitial := ReadAtom(AArgs, vPos);
  if vMech = 'PLAIN' then
    vMethod := msaPlain
  else if vMech = 'SCRAM-SHA-256' then
    vMethod := msaScramSHA256
  else
  begin
    Done(ATag, 'NO', '[CANNOT] unsupported authentication mechanism');
    Exit;
  end;
  if ((vMethod = msaPlain) and not PlainAllowed) or ((vMethod = msaScramSHA256) and not ScramAllowed) then
  begin
    Done(ATag, 'NO', '[PRIVACYREQUIRED] this mechanism is allowed only over TLS');
    Exit;
  end;
  FreeAndNil(FSASL);
  FSASL := TRALMailSASL.Create(FServer, Session, vMethod);
  FSASLTag := ATag;
  FInput := iiSASL;
  if vInitial = '' then
    Send('+ '#13#10)
  else
    SASLLine(vInitial);
end;

procedure TRALIMAPConnection.SASLLine(const ALine: RawByteString);
var
  vText, vOutput: StringRAL;
  vResult: TRALMailSASLResult;
begin
  if ALine = '*' then
  begin
    FInput := iiCommand;
    FreeAndNil(FSASL);
    Done(FSASLTag, 'BAD', 'authentication canceled');
    Exit;
  end;
  vText := '';
  if (ALine <> '') and (ALine <> '=') then
    try
      vText := TRALBase64.Decode(StringRAL(ALine));
    except
      FInput := iiCommand;
      FreeAndNil(FSASL);
      Done(FSASLTag, 'BAD', 'cannot decode the response');
      Exit;
    end;
  vResult := FSASL.Step(vText, True, vOutput);
  if vResult = msrContinue then
    Send('+ ' + RawByteString(TRALBase64.Encode(vOutput)) + cCRLF)
  else
    SASLResult(vResult);
end;

procedure TRALIMAPConnection.SASLResult(AResult: TRALMailSASLResult);
begin
  FInput := iiCommand;
  case AResult of
    msrOk:
    begin
      SetLoggedIn(FSASL.UserName);
      Done(FSASLTag, 'OK', '[CAPABILITY ' + CapabilityText + '] authentication successful');
    end;
    msrTemporary:
      Done(FSASLTag, 'NO', '[UNAVAILABLE] try later');
  else
    begin
      Inc(FBadLogins);
      Done(FSASLTag, 'NO', '[AUTHENTICATIONFAILED] authentication failed');
      if FBadLogins >= 10 then
        CloseAfterFlush := True;
    end;
  end;
  FreeAndNil(FSASL);
end;

procedure TRALIMAPConnection.CmdEnable(const ATag, AArgs: RawByteString);
var
  vPos: IntegerRAL;
  vWord, vDone: RawByteString;
begin
  vPos := 1;
  vDone := '';
  while True do
  begin
    vWord := RALMailUpper(ReadAtom(AArgs, vPos));
    if vWord = '' then
      Break;
    if vWord = 'UTF8=ACCEPT' then
    begin
      FEnabledUTF8 := True;
      vDone := vDone + ' UTF8=ACCEPT';
    end
    else if vWord = 'IMAP4REV2' then
    begin
      FEnabledRev2 := True;
      vDone := vDone + ' IMAP4rev2';
    end;
  end;
  Send('* ENABLED' + vDone + cCRLF);
  Done(ATag, 'OK', 'ENABLE completed');
end;

procedure TRALIMAPConnection.CmdSelect(const ATag, AArgs: RawByteString; AReadOnly: boolean);
var
  vPos, vInt, vUnseen: IntegerRAL;
  vName: StringRAL;
  vCanonical: StringRAL;
begin
  vPos := 1;
  vName := DecodeName(ReadString(AArgs, vPos));
  Unselect;
  vCanonical := TRALMailStore.NormalizeMailbox(vName);
  if vCanonical = '' then
  begin
    Done(ATag, 'NO', 'invalid mailbox name');
    Exit;
  end;
  FSelected := vCanonical;
  try
    LoadView;
  except
    Unselect;
    raise;
  end;
  FReadOnly := AReadOnly;
  Send('* FLAGS (' + cSystemFlagList + ')'#13#10);
  Send('* ' + IStr(FViewCount) + ' EXISTS'#13#10);
  if not FEnabledRev2 then
    Send('* 0 RECENT'#13#10);
  vUnseen := 0;
  for vInt := 0 to FViewCount - 1 do
    if not RALMailFlagHas(FView[vInt].Flags, '\Seen') then
    begin
      vUnseen := vInt + 1;
      Break;
    end;
  if (vUnseen > 0) and not FEnabledRev2 then
    Send('* OK [UNSEEN ' + IStr(vUnseen) + '] first unseen message'#13#10);
  Send('* OK [UIDVALIDITY ' + IStr(FValidity) + '] UIDs valid'#13#10);
  Send('* OK [UIDNEXT ' + IStr(FRecentFrom) + '] predicted next UID'#13#10);
  if AReadOnly then
    Send('* OK [PERMANENTFLAGS ()] no permanent flags permitted'#13#10)
  else
    Send('* OK [PERMANENTFLAGS (' + cSystemFlagList + ' \*)] flags permitted'#13#10);
  if AReadOnly then
    Done(ATag, 'OK', '[READ-ONLY] EXAMINE completed')
  else
    Done(ATag, 'OK', '[READ-WRITE] SELECT completed');
end;

procedure TRALIMAPConnection.CmdCreateDelete(const ATag, ACommand, AArgs: RawByteString);
var
  vPos: IntegerRAL;
  vName: StringRAL;
begin
  vPos := 1;
  vName := DecodeName(ReadString(AArgs, vPos));
  if (vName <> '') and (vName[Length(vName)] = '/') then
    vName := Copy(vName, 1, Length(vName) - 1);
  if ACommand = 'CREATE' then
    FServer.Store.CreateMailbox(Session.UserName, vName)
  else
    FServer.Store.DeleteMailbox(Session.UserName, vName);
  Done(ATag, 'OK', ACommand + ' completed');
end;

procedure TRALIMAPConnection.CmdRename(const ATag, AArgs: RawByteString);
var
  vPos: IntegerRAL;
  vOld, vNew: StringRAL;
begin
  vPos := 1;
  vOld := DecodeName(ReadString(AArgs, vPos));
  vNew := DecodeName(ReadString(AArgs, vPos));
  FServer.Store.RenameMailbox(Session.UserName, vOld, vNew);
  Done(ATag, 'OK', 'RENAME completed');
end;

procedure TRALIMAPConnection.CmdSubscribe(const ATag, ACommand, AArgs: RawByteString);
var
  vPos: IntegerRAL;
  vName: StringRAL;
begin
  vPos := 1;
  vName := DecodeName(ReadString(AArgs, vPos));
  if ACommand = 'SUBSCRIBE' then
  begin
    if not FServer.Store.MailboxExists(Session.UserName, vName) then
      raise ERALMailStoreError.CreateKind(mekNoMailbox, vName);
    FServer.Store.Subscribe(Session.UserName, TRALMailStore.NormalizeMailbox(vName));
  end
  else
    FServer.Store.Unsubscribe(Session.UserName, TRALMailStore.NormalizeMailbox(vName));
  Done(ATag, 'OK', ACommand + ' completed');
end;

procedure TRALIMAPConnection.CmdList(const ATag, ACommand, AArgs: RawByteString);
var
  vPos, vInt, vSlash: IntegerRAL;
  vRef, vPattern, vFull, vAttr, vInner: RawByteString;
  vNames, vAll: TStringList;
  vName: RawByteString;
  vIsSub: boolean;
begin
  vPos := 1;
  // the extended forms: selection options before, RETURN after (ignored)
  SkipBlanks(AArgs, vPos);
  if (vPos <= Length(AArgs)) and (AArgs[vPos] = '(') then
    ReadList(AArgs, vPos, vInner);
  vRef := RawByteString(DecodeName(ReadString(AArgs, vPos)));
  vPattern := RawByteString(DecodeName(ReadString(AArgs, vPos)));
  vIsSub := ACommand = 'LSUB';
  if (vPattern = '') and not vIsSub then
  begin
    Send('* LIST (\Noselect) "/" ""'#13#10);
    Done(ATag, 'OK', 'LIST completed');
    Exit;
  end;
  vFull := vRef + vPattern;
  vNames := TStringList.Create;
  vAll := TStringList.Create;
  try
    FServer.Store.ListMailboxes(Session.UserName, vAll);
    if vIsSub then
    begin
      FServer.Store.ListSubscriptions(Session.UserName, vNames);
      if vNames.IndexOf('INBOX') < 0 then
        vNames.Add('INBOX');
    end
    else
    begin
      vNames.Assign(vAll);
      // a parent that has no mailbox of its own is listed as not selectable
      for vInt := 0 to vAll.Count - 1 do
      begin
        vName := RawByteString(vAll[vInt]);
        for vSlash := 1 to Length(vName) do
          if (vName[vSlash] = '/') and (vNames.IndexOf(string(Copy(vName, 1, vSlash - 1))) < 0) then
            vNames.AddObject(string(Copy(vName, 1, vSlash - 1)), TObject(Pointer(1)));
      end;
    end;
    vNames.Sort;
    for vInt := 0 to vNames.Count - 1 do
    begin
      vName := RawByteString(vNames[vInt]);
      if not GlobMatch(vFull, vName, 1, 1) then
        Continue;
      vAttr := '';
      for vSlash := 0 to vAll.Count - 1 do
        if Copy(RawByteString(vAll[vSlash]), 1, Length(vName) + 1) = vName + '/' then
        begin
          vAttr := '\HasChildren';
          Break;
        end;
      if vAttr = '' then
        vAttr := '\HasNoChildren';
      if (vNames.Objects[vInt] <> nil) then
        vAttr := '\Noselect \HasChildren';
      Send('* ' + ACommand + ' (' + vAttr + ') "/" ' + AString(EncodeName(StringRAL(vName))) + cCRLF);
    end;
  finally
    vAll.Free;
    vNames.Free;
  end;
  Done(ATag, 'OK', ACommand + ' completed');
end;

procedure TRALIMAPConnection.CmdStatus(const ATag, AArgs: RawByteString);
var
  vPos: IntegerRAL;
  vName: StringRAL;
  vItems, vWord, vText: RawByteString;
  vStatus: TRALMailboxStatus;
  vItemPos: IntegerRAL;
begin
  vPos := 1;
  vName := DecodeName(ReadString(AArgs, vPos));
  if not ReadList(AArgs, vPos, vItems) then
    raise Exception.Create('bad status');
  vStatus := FServer.Store.Status(Session.UserName, vName);
  vText := '';
  vItemPos := 1;
  while True do
  begin
    vWord := RALMailUpper(ReadAtom(vItems, vItemPos));
    if vWord = '' then
      Break;
    if vText <> '' then
      vText := vText + ' ';
    if vWord = 'MESSAGES' then
      vText := vText + 'MESSAGES ' + IStr(vStatus.Messages)
    else if vWord = 'RECENT' then
      vText := vText + 'RECENT 0'
    else if vWord = 'UIDNEXT' then
      vText := vText + 'UIDNEXT ' + IStr(vStatus.UIDNext)
    else if vWord = 'UIDVALIDITY' then
      vText := vText + 'UIDVALIDITY ' + IStr(vStatus.UIDValidity)
    else if vWord = 'UNSEEN' then
      vText := vText + 'UNSEEN ' + IStr(vStatus.Unseen)
    else if vWord = 'SIZE' then
      vText := vText + 'SIZE ' + IStr(vStatus.Size)
    else if vWord = 'DELETED' then
      vText := vText + 'DELETED ' + IStr(vStatus.Deleted)
    else
      raise Exception.Create('bad status item');
  end;
  Send('* STATUS ' + AString(EncodeName(vName)) + ' (' + vText + ')'#13#10);
  Done(ATag, 'OK', 'STATUS completed');
end;

procedure TRALIMAPConnection.CmdFetch(const ATag, AArgs: RawByteString; AUID: boolean);
var
  vPos: IntegerRAL;
  vSet, vRest: RawByteString;
  vSpec: TFetchSpec;
begin
  vPos := 1;
  vSet := ReadAtom(AArgs, vPos);
  vRest := Copy(AArgs, vPos, MaxInt);
  vSpec := TFetchSpec.Create;
  if not ParseFetch(vRest, vSpec) then
  begin
    vSpec.Free;
    Done(ATag, 'BAD', 'invalid FETCH items');
    Exit;
  end;
  try
    FJobIndex := ResolveSet(vSet, AUID);
  except
    vSpec.Free;
    raise;
  end;
  FreeAndNil(FFetch);
  FFetch := vSpec;
  FJobTag := ATag;
  FJobUID := AUID;
  FJobPos := 0;
  if AUID then
    FJobName := 'UID FETCH'
  else
    FJobName := 'FETCH';
  FJobActive := True;
  Pump;
end;

procedure TRALIMAPConnection.Pump;
begin
  while FJobActive and (PendingOutput < cPumpLimit) and (FJobPos <= High(FJobIndex)) and
        not IsClosed do
  begin
    FetchOne(FJobIndex[FJobPos]);
    Inc(FJobPos);
  end;
  if FJobActive and (FJobPos > High(FJobIndex)) then
    FinishJob;
end;

procedure TRALIMAPConnection.FinishJob;
begin
  FJobActive := False;
  FreeAndNil(FFetch);
  FNoExpunge := True;
  Done(FJobTag, 'OK', FJobName + ' completed');
  // the commands that came while the messages went out
  if not FInProcess then
    ProcessInput;
end;

procedure TRALIMAPConnection.FetchOne(AIndex: IntegerRAL);
var
  vSpec: TFetchSpec;
  vInfo: TRALMailMessageInfo;
  vMessage, vText, vName, vLine: RawByteString;
  vRoot: TMimePart;
  vStream: TStream;
  vInt: IntegerRAL;
  vItem: TFetchItem;
  vHasFlags, vHasUID, vFlagsChanged: boolean;
  vHeaderLen: IntegerRAL;
  vInfos: TRALMailMessageInfos;
  vUIDs: TRALMailUIDs;
begin
  vSpec := TFetchSpec(FFetch);
  vInfo := FView[AIndex];
  vMessage := '';
  vRoot := nil;
  vFlagsChanged := False;
  try
    if vSpec.NeedMessage then
    begin
      vStream := FServer.Store.OpenMessage(Session.UserName, FSelected, vInfo.UID);
      // a message another session expunged cannot be fetched
      if vStream = nil then
        Exit;
      try
        SetLength(vMessage, vStream.Size);
        if Length(vMessage) > 0 then
          vStream.ReadBuffer(Pointer(vMessage)^, Length(vMessage));
      finally
        vStream.Free;
      end;
    end;
    if vSpec.SetsSeen and not FReadOnly and not RALMailFlagHas(vInfo.Flags, '\Seen') then
    begin
      SetLength(vUIDs, 1);
      vUIDs[0] := vInfo.UID;
      vInfos := FServer.Store.SetFlags(Session.UserName, FSelected, vUIDs, '\Seen', mfmAdd);
      if Length(vInfos) > 0 then
      begin
        FView[AIndex].Flags := vInfos[0].Flags;
        vInfo := FView[AIndex];
        vFlagsChanged := True;
      end;
    end;
    vHeaderLen := 0;
    if vSpec.NeedMessage then
      vHeaderLen := HeaderEnd(vMessage, 1, Length(vMessage)) - 1;
    vHasFlags := False;
    vHasUID := False;
    vLine := '';
    for vInt := 0 to High(vSpec.Items) do
    begin
      vItem := vSpec.Items[vInt];
      if vLine <> '' then
        vLine := vLine + ' ';
      case vItem.Kind of
        fkFlags:
        begin
          vHasFlags := True;
          vLine := vLine + 'FLAGS ' + FlagList(vInfo.Flags);
        end;
        fkUID:
        begin
          vHasUID := True;
          vLine := vLine + 'UID ' + IStr(vInfo.UID);
        end;
        fkInternalDate:
          vLine := vLine + 'INTERNALDATE "' + InternalDateText(vInfo.Date) + '"';
        fkSize:
          vLine := vLine + 'RFC822.SIZE ' + IStr(vInfo.Size);
        fkEnvelope:
          vLine := vLine + 'ENVELOPE ' + EnvelopeText(Copy(vMessage, 1, vHeaderLen));
        fkStructure, fkBodyBasic:
        begin
          if vRoot = nil then
            vRoot := ParsePart(vMessage, 1, Length(vMessage), 'text/plain', 0);
          if vItem.Kind = fkStructure then
            vLine := vLine + 'BODYSTRUCTURE ' + StructureText(vMessage, vRoot, True)
          else
            vLine := vLine + 'BODY ' + StructureText(vMessage, vRoot, False);
        end;
        fkRFC822, fkRFC822Header, fkRFC822Text:
        begin
          case vItem.Kind of
            fkRFC822:
            begin
              vName := 'RFC822';
              vText := vMessage;
            end;
            fkRFC822Header:
            begin
              vName := 'RFC822.HEADER';
              vText := Copy(vMessage, 1, vHeaderLen);
            end;
          else
            vName := 'RFC822.TEXT';
            vText := Copy(vMessage, vHeaderLen + 1, MaxInt);
          end;
          vLine := vLine + vName + ' {' + IStr(Length(vText)) + '}' + cCRLF + vText;
        end;
        fkSection:
        begin
          if vRoot = nil then
            vRoot := ParsePart(vMessage, 1, Length(vMessage), 'text/plain', 0);
          vName := 'BODY[' + vItem.Section + ']';
          if not SectionText(vMessage, vRoot, vItem.Section, vText) then
            vLine := vLine + vName + ' NIL'
          else
          begin
            if vItem.HasPartial then
            begin
              vText := Copy(vText, vItem.PartStart + 1, MaxInt);
              if (vItem.PartLength >= 0) and (Length(vText) > vItem.PartLength) then
                SetLength(vText, vItem.PartLength);
              vName := vName + '<' + IStr(vItem.PartStart) + '>';
            end;
            vLine := vLine + vName + ' {' + IStr(Length(vText)) + '}' + cCRLF + vText;
          end;
        end;
      end;
    end;
    if FJobUID and not vHasUID then
      vLine := vLine + ' UID ' + IStr(vInfo.UID);
    if vFlagsChanged and not vHasFlags then
      vLine := vLine + ' FLAGS ' + FlagList(vInfo.Flags);
    Send('* ' + IStr(AIndex + 1) + ' FETCH (' + vLine + ')'#13#10);
  finally
    vRoot.Free;
  end;
end;

procedure TRALIMAPConnection.CmdStore(const ATag, AArgs: RawByteString; AUID: boolean);
var
  vPos, vInt: IntegerRAL;
  vSet, vItem, vFlags, vInner: RawByteString;
  vMode: TRALMailFlagMode;
  vSilent: boolean;
  vIdx, vUIDs: TRALMailUIDs;
  vInfos: TRALMailMessageInfos;
  vIndex: IntegerRAL;
begin
  if FReadOnly then
  begin
    Done(ATag, 'NO', '[READ-ONLY] the mailbox is read-only');
    Exit;
  end;
  vPos := 1;
  vSet := ReadAtom(AArgs, vPos);
  SkipBlanks(AArgs, vPos);
  // a modifier list such as (UNCHANGEDSINCE n) is not supported: ignored
  if (vPos <= Length(AArgs)) and (AArgs[vPos] = '(') then
    ReadList(AArgs, vPos, vInner);
  vItem := RALMailUpper(ReadAtom(AArgs, vPos));
  vFlags := RALMailTrim(Copy(AArgs, vPos, MaxInt));
  if (vFlags <> '') and (vFlags[1] = '(') then
    vFlags := Copy(vFlags, 2, Length(vFlags) - 2);
  vMode := mfmReplace;
  if (vItem <> '') and (vItem[1] = '+') then
  begin
    vMode := mfmAdd;
    Delete(vItem, 1, 1);
  end
  else if (vItem <> '') and (vItem[1] = '-') then
  begin
    vMode := mfmRemove;
    Delete(vItem, 1, 1);
  end;
  vSilent := Copy(vItem, Length(vItem) - 6, 7) = '.SILENT';
  if vSilent then
    SetLength(vItem, Length(vItem) - 7);
  if vItem <> 'FLAGS' then
  begin
    Done(ATag, 'BAD', 'invalid STORE item');
    Exit;
  end;
  vIdx := ResolveSet(vSet, AUID);
  SetLength(vUIDs, Length(vIdx));
  for vInt := 0 to High(vIdx) do
    vUIDs[vInt] := FView[vIdx[vInt]].UID;
  vInfos := FServer.Store.SetFlags(Session.UserName, FSelected, vUIDs, StringRAL(vFlags), vMode);
  for vInt := 0 to High(vInfos) do
  begin
    vIndex := ViewFind(vInfos[vInt].UID);
    if vIndex < 0 then
      Continue;
    FView[vIndex].Flags := vInfos[vInt].Flags;
    if not vSilent then
      SendFlags(vIndex, AUID);
  end;
  Done(ATag, 'OK', 'STORE completed');
end;

procedure TRALIMAPConnection.CmdExpunge(const ATag, AArgs: RawByteString; AUID: boolean);
var
  vRemoved, vWanted, vIdx: TRALMailUIDs;
  vInt: IntegerRAL;
begin
  if FReadOnly then
  begin
    Done(ATag, 'NO', '[READ-ONLY] the mailbox is read-only');
    Exit;
  end;
  if AUID and (AArgs <> '') then
  begin
    vIdx := ResolveSet(AArgs, True);
    SetLength(vWanted, Length(vIdx));
    for vInt := 0 to High(vIdx) do
      vWanted[vInt] := FView[vIdx[vInt]].UID;
    vRemoved := FServer.Store.Expunge(Session.UserName, FSelected, vWanted);
  end
  else
    vRemoved := FServer.Store.Expunge(Session.UserName, FSelected);
  RemoveFromView(vRemoved, False);
  Done(ATag, 'OK', 'EXPUNGE completed');
end;

procedure TRALIMAPConnection.CmdCopy(const ATag, AArgs: RawByteString; AUID, AMove: boolean);
var
  vPos, vInt: IntegerRAL;
  vSet: RawByteString;
  vDest: StringRAL;
  vIdx, vUIDs, vSource, vTarget: TRALMailUIDs;
  vStatus: TRALMailboxStatus;
  vCode: RawByteString;
begin
  if AMove and FReadOnly then
  begin
    Done(ATag, 'NO', '[READ-ONLY] the mailbox is read-only');
    Exit;
  end;
  vPos := 1;
  vSet := ReadAtom(AArgs, vPos);
  vDest := DecodeName(ReadString(AArgs, vPos));
  if not FServer.Store.MailboxExists(Session.UserName, vDest) then
  begin
    Done(ATag, 'NO', '[TRYCREATE] the destination mailbox does not exist');
    Exit;
  end;
  vIdx := ResolveSet(vSet, AUID);
  SetLength(vUIDs, Length(vIdx));
  for vInt := 0 to High(vIdx) do
    vUIDs[vInt] := FView[vIdx[vInt]].UID;
  if AMove then
    FServer.Store.MoveMessages(Session.UserName, FSelected, vDest, vUIDs, vSource, vTarget)
  else
    FServer.Store.CopyMessages(Session.UserName, FSelected, vDest, vUIDs, vSource, vTarget);
  vStatus := FServer.Store.Status(Session.UserName, vDest);
  if Length(vSource) > 0 then
    vCode := '[COPYUID ' + IStr(vStatus.UIDValidity) + ' ' + UIDSetText(vSource) + ' ' +
      UIDSetText(vTarget) + '] '
  else
    vCode := '';
  if AMove then
  begin
    if vCode <> '' then
      Send('* OK ' + vCode + 'moved'#13#10);
    RemoveFromView(vSource, False);
    Done(ATag, 'OK', 'MOVE completed');
  end
  else
    Done(ATag, 'OK', vCode + 'COPY completed');
end;

procedure TRALIMAPConnection.CmdSearch(const ATag, AArgs: RawByteString; AUID: boolean);
var
  vPos, vInt: IntegerRAL;
  vText, vWord, vReturn, vOut, vCharset: RawByteString;
  vNode: TSearchNode;
  vAnd: TSearchNode;
  vCtx: TSearchContext;
  vHits: TRALMailUIDs;
  vCount: IntegerRAL;
  vEsearch: boolean;
  vInner: RawByteString;
  vItemPos: IntegerRAL;
  vValue: Cardinal;
begin
  vText := AArgs;
  vPos := 1;
  vEsearch := FEnabledRev2;
  vReturn := '';
  SkipBlanks(vText, vPos);
  vWord := RALMailUpper(Copy(vText, vPos, 6));
  if vWord = 'RETURN' then
  begin
    Inc(vPos, 6);
    ReadList(vText, vPos, vInner);
    vReturn := RALMailUpper(vInner);
    vEsearch := True;
  end;
  vItemPos := vPos;
  vWord := RALMailUpper(ReadAtom(vText, vItemPos));
  if vWord = 'CHARSET' then
  begin
    vCharset := RALMailUpper(ReadString(vText, vItemPos));
    if (vCharset <> 'UTF-8') and (vCharset <> 'US-ASCII') then
    begin
      Done(ATag, 'NO', '[BADCHARSET (UTF-8 US-ASCII)] charset not supported');
      Exit;
    end;
    vPos := vItemPos;
  end;
  vAnd := TSearchNode.Create(skAnd);
  vCtx := TSearchContext.Create;
  try
    try
      while True do
      begin
        SkipBlanks(vText, vPos);
        if vPos > Length(vText) then
          Break;
        vAnd.Subs.Add(ParseSearchKey(vText, vPos));
      end;
    except
      Done(ATag, 'BAD', 'invalid search criteria');
      Exit;
    end;
    if vAnd.Subs.Count = 0 then
    begin
      Done(ATag, 'BAD', 'missing search criteria');
      Exit;
    end;
    vNode := vAnd;
    vCtx.Loader := LoadText;
    vCtx.MaxSeq := FViewCount;
    vCtx.MaxUID := 0;
    if FViewCount > 0 then
      vCtx.MaxUID := FView[FViewCount - 1].UID;
    vCtx.RecentFrom := FRecentFrom;
    SetLength(vHits, FViewCount);
    vCount := 0;
    for vInt := 0 to FViewCount - 1 do
      if Matches(vNode, FView[vInt], vInt + 1, vCtx) then
      begin
        if AUID then
          vHits[vCount] := FView[vInt].UID
        else
          vHits[vCount] := vInt + 1;
        Inc(vCount);
      end;
    SetLength(vHits, vCount);
  finally
    vCtx.Free;
    vAnd.Free;
  end;
  if vEsearch then
  begin
    vOut := '* ESEARCH (TAG ' + Quoted(ATag) + ')';
    if AUID then
      vOut := vOut + ' UID';
    if (vReturn = '') or (Pos(RawByteString('ALL'), vReturn) > 0) then
      if vCount > 0 then
        vOut := vOut + ' ALL ' + UIDSetText(vHits);
    if (vReturn <> '') and (vCount > 0) and (Pos(RawByteString('MIN'), vReturn) > 0) then
      vOut := vOut + ' MIN ' + IStr(vHits[0]);
    if (vReturn <> '') and (vCount > 0) and (Pos(RawByteString('MAX'), vReturn) > 0) then
    begin
      vValue := vHits[vCount - 1];
      vOut := vOut + ' MAX ' + IStr(vValue);
    end;
    if (vReturn <> '') and (Pos(RawByteString('COUNT'), vReturn) > 0) then
      vOut := vOut + ' COUNT ' + IStr(vCount);
    Send(vOut + cCRLF);
  end
  else
  begin
    vOut := '* SEARCH';
    for vInt := 0 to vCount - 1 do
      vOut := vOut + ' ' + IStr(vHits[vInt]);
    Send(vOut + cCRLF);
  end;
  Done(ATag, 'OK', 'SEARCH completed');
end;

{ TRALIMAPServer }

constructor TRALIMAPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Port := 143;
  TLSPort := 993;
  FMaxMessageSize := 26214400;
  SetEngine('IMAP ' + StringRAL(RALVERSION));
end;

function TRALIMAPServer.BusyReply: RawByteString;
begin
  Result := '* BYE [UNAVAILABLE] too many connections, try later'#13#10;
end;

function TRALIMAPServer.CreateConnection(ASocket: TRALSocket;
  AListener: IntegerRAL): TRALMailConnection;
begin
  Result := TRALIMAPConnection.Create(Self, ASocket, AListener);
end;

function TRALIMAPServer.ProtocolName: string;
begin
  Result := 'IMAP';
end;

function TRALIMAPServer.SecureProtocolName: string;
begin
  Result := 'IMAPS';
end;

end.
