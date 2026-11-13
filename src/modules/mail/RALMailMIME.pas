{ E-mail messages (RFC 5322 and MIME): written to a stream and read from one in
  pieces, without holding a large part in memory. }
unit RALMailMIME;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, DateUtils,
  RALTypes, RALTools, RALBase64, RALStream, RALExceptions;

type
  /// The priority a message asks for (X-Priority and Importance).
  TRALMailPriority = (mpNormal, mpHigh, mpLow);

  /// What the writer may put on the wire without encoding.
  TRALMailWriteOption = (
    /// 8-bit text as it is (8BITMIME), lines up to 998 bytes; otherwise quoted-printable.
    mwo8Bit,
    /// UTF-8 in the headers as it is (SMTPUTF8, RFC 6532); otherwise RFC 2047 words.
    mwoUTF8Headers
  );
  /// A set of write options.
  TRALMailWriteOptions = set of TRALMailWriteOption;

  /// A mailbox: a display name and an address.
  TRALMailAddress = class(TCollectionItem)
  private
    FAddress: StringRAL;
    FName: StringRAL;
  public
    procedure Assign(ASource: TPersistent); override;
    /// The mailbox as a header shows it: "Name" <address>, name encoded if needed.
    function HeaderText(AUTF8: Boolean = False): StringRAL;
  published
    /// The address (local@@domain).
    property Address: StringRAL read FAddress write FAddress;
    /// The display name, UTF-8.
    property Name: StringRAL read FName write FName;
  end;

  /// A list of mailboxes (From, To, Cc...).
  TRALMailAddresses = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALMailAddress;
  public
    /// An empty list owned by AOwner.
    constructor Create(AOwner: TPersistent);

    /// Adds a mailbox; returns it.
    function Add(const AAddress: StringRAL; const AName: StringRAL = ''): TRALMailAddress;
    /// Adds the mailboxes of a header value ("A <a@@x>, b@@y", encoded words decoded).
    procedure AddText(const AText: StringRAL);
    /// The list as a header value (folded by the writer).
    function HeaderText(AUTF8: Boolean = False): StringRAL;

    /// The mailboxes.
    property Items[AIndex: IntegerRAL]: TRALMailAddress read GetItem; default;
  end;

  { A file the message carries: an attachment, or an inline part (an image the
    HTML shows by cid:ContentID). The content is a file on disk (Path) or a
    stream the attachment owns (Data); a received one is in Data, spooled to a
    temporary file when it is large. }
  TRALMailAttachment = class(TCollectionItem)
  private
    FContentID: StringRAL;
    FContentType: StringRAL;
    FData: TStream;
    FFileName: StringRAL;
    FInline: Boolean;
    FPath: string;

    procedure SetData(AValue: TStream);
  public
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
    /// Points the attachment at a file on disk (read when the message is written).
    procedure LoadFromFile(const APath: string);
    /// Copies AStream (from its position) into the attachment.
    procedure LoadFromStream(AStream: TStream);
    /// Takes AText as the content.
    procedure LoadFromText(const AText: StringRAL);
    /// A stream over the content (a new one for Path); the caller frees it.
    function OpenContent: TStream;
    /// Writes the content to APath.
    procedure SaveToFile(const APath: string);
    /// The content size in bytes (-1 when unknown).
    function Size: Int64;

    /// The content, owned by the attachment; nil when it comes from Path.
    property Data: TStream read FData write SetData;
  published
    /// The Content-ID an inline part is shown by (without the angle brackets).
    property ContentID: StringRAL read FContentID write FContentID;
    /// The media type; empty takes it from the file name.
    property ContentType: StringRAL read FContentType write FContentType;
    /// The file name shown (UTF-8; RFC 2231 encoded when it needs it).
    property FileName: StringRAL read FFileName write FFileName;
    /// An inline part (Content-Disposition: inline) instead of an attachment.
    property IsInline: Boolean read FInline write FInline default False;
    /// A file on disk to send (empty when the content is in Data).
    property Path: string read FPath write FPath;
  end;

  /// The attachments of a message.
  TRALMailAttachments = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALMailAttachment;
  public
    /// An empty list owned by AOwner.
    constructor Create(AOwner: TPersistent);

    /// Adds an attachment of the file APath; returns it.
    function AddFile(const APath: string; const AFileName: StringRAL = ''): TRALMailAttachment;
    /// Adds an inline part of the file APath shown by cid:AContentID; returns it.
    function AddInline(const APath: string; const AContentID: StringRAL): TRALMailAttachment;

    /// The attachments.
    property Items[AIndex: IntegerRAL]: TRALMailAttachment read GetItem; default;
  end;

  { A message: the envelope-free part SMTP carries (RFC 5322 headers and a
    MIME body). Written in one pass: text alone is text/plain, text and HTML a
    multipart/alternative, inline parts a multipart/related around the HTML,
    attachments a multipart/mixed around all of it. Read in one pass too: the
    first text/plain and text/html go to Text and HTML, the other parts to
    Attachments (large ones in temporary files). }
  TRALMailMessage = class(TPersistent)
  private
    FAttachments: TRALMailAttachments;
    FBCC: TRALMailAddresses;
    FCC: TRALMailAddresses;
    FDate: TDateTime;
    FFrom: TRALMailAddresses;
    FHeaders: TStringList;
    FHTML: StringRAL;
    FInReplyTo: StringRAL;
    FMessageID: StringRAL;
    FPriority: TRALMailPriority;
    FReadReceiptTo: StringRAL;
    FRecipients: TRALMailAddresses;
    FReferences: StringRAL;
    FReplyTo: TRALMailAddresses;
    FSender: StringRAL;
    FSpoolAbove: Int64;
    FSubject: StringRAL;
    FText: StringRAL;

    procedure SetAttachments(AValue: TRALMailAttachments);
    procedure SetBCC(AValue: TRALMailAddresses);
    procedure SetCC(AValue: TRALMailAddresses);
    procedure SetFrom(AValue: TRALMailAddresses);
    procedure SetHeaders(AValue: TStringList);
    procedure SetRecipients(AValue: TRALMailAddresses);
    procedure SetReplyTo(AValue: TRALMailAddresses);
  public
    constructor Create;
    destructor Destroy; override;

    /// Every address of To, Cc and Bcc (the SMTP envelope recipients).
    procedure AllRecipients(AList: TStrings);
    procedure Assign(ASource: TPersistent); override;
    /// The message as text (for small messages and tests).
    function AsText(AOptions: TRALMailWriteOptions = []): StringRAL;
    /// Empties the message.
    procedure Clear;
    /// Reads a .eml file.
    procedure LoadFromFile(const APath: string);
    /// Reads a message from AStream (from its position to the end).
    procedure LoadFromStream(AStream: TStream);
    /// Writes a .eml file.
    procedure SaveToFile(const APath: string; AOptions: TRALMailWriteOptions = []);
    /// Writes the message to AStream (CRLF lines); fills Date and MessageID when empty.
    procedure SaveToStream(AStream: TStream; AOptions: TRALMailWriteOptions = []);

    /// Parts larger than this (bytes) go to a temporary file; 0 keeps all in memory.
    property SpoolAbove: Int64 read FSpoolAbove write FSpoolAbove;
  published
    /// Files the message carries.
    property Attachments: TRALMailAttachments read FAttachments write SetAttachments;
    /// Blind copies: envelope only, never written in the headers.
    property BCC: TRALMailAddresses read FBCC write SetBCC;
    /// Copies.
    property CC: TRALMailAddresses read FCC write SetCC;
    /// The date; 0 takes the moment it is written.
    property Date: TDateTime read FDate write FDate;
    /// The authors (usually one).
    property From: TRALMailAddresses read FFrom write SetFrom;
    /// Other header fields (Name=Value), written as they are and filled when reading.
    property Headers: TStringList read FHeaders write SetHeaders;
    /// The HTML body, UTF-8.
    property HTML: StringRAL read FHTML write FHTML;
    /// The Message-ID this one answers.
    property InReplyTo: StringRAL read FInReplyTo write FInReplyTo;
    /// The Message-ID (without angle brackets); empty makes one when written.
    property MessageID: StringRAL read FMessageID write FMessageID;
    /// The priority.
    property Priority: TRALMailPriority read FPriority write FPriority default mpNormal;
    /// Where a read receipt goes (Disposition-Notification-To).
    property ReadReceiptTo: StringRAL read FReadReceiptTo write FReadReceiptTo;
    /// The recipients (To).
    property Recipients: TRALMailAddresses read FRecipients write SetRecipients;
    /// Message-IDs of the thread.
    property References: StringRAL read FReferences write FReferences;
    /// Where answers go.
    property ReplyTo: TRALMailAddresses read FReplyTo write SetReplyTo;
    /// The Sender, when it is not the author.
    property Sender: StringRAL read FSender write FSender;
    /// The subject, UTF-8.
    property Subject: StringRAL read FSubject write FSubject;
    /// The plain text body, UTF-8.
    property Text: StringRAL read FText write FText;
  end;

{ The byte position of ASub in AText, from 1 (0 when absent): Pos on a UTF8String
  converted to string counts UTF-16 characters on Delphi, which does not match the
  byte positions Copy uses on the UTF8String. }
function RALMailPos(const ASub, AText: RawByteString): IntegerRAL;

/// Text as an RFC 2047 encoded word when it is not plain printable ASCII.
function RALMailEncodeWord(const AText: StringRAL): StringRAL;
/// Decodes the RFC 2047 encoded words of a header value into UTF-8.
function RALMailDecodeWords(const AText: StringRAL): StringRAL;
/// Text in quoted-printable (RFC 2045), CRLF line breaks, lines of at most 76.
function RALMailEncodeQP(const AText: StringRAL): StringRAL;
/// Quoted-printable decoded (soft line breaks removed).
function RALMailDecodeQP(const AText: StringRAL): StringRAL;
/// A date in the RFC 5322 form, local time with its offset.
function RALMailDate(ADate: TDateTime): StringRAL;
/// A date of an RFC 5322 header, in local time; 0 when it cannot be read.
function RALMailParseDate(const AText: StringRAL): TDateTime;
/// Bytes of ACharset as UTF-8 (UTF-8, ASCII, ISO-8859-1/-15, Windows-1252; others kept).
function RALMailCharsetToUTF8(const AText: StringRAL; const ACharset: StringRAL): StringRAL;
/// The value of the parameter AName of a header value (RFC 2045 and the RFC 2231 forms).
function RALMailHeaderParam(const AValue, AName: StringRAL): StringRAL;
/// The header value without its parameters, lower case (the media type, the disposition).
function RALMailHeaderMain(const AValue: StringRAL): StringRAL;

implementation

const
  /// Header names the message has properties for (not kept in Headers).
  cKnown: array[0..17] of StringRAL = ('from', 'to', 'cc', 'bcc', 'reply-to',
    'subject', 'date', 'message-id', 'in-reply-to', 'references', 'mime-version',
    'content-type', 'content-transfer-encoding', 'x-priority', 'importance',
    'disposition-notification-to', 'sender', 'content-disposition');
  /// Windows-1252 0x80..0x9F as Unicode (0 = unassigned, kept as Latin-1).
  cCP1252: array[$80..$9F] of Word = ($20AC, 0, $201A, $0192, $201E, $2026, $2020,
    $2021, $02C6, $2030, $0160, $2039, $0152, 0, $017D, 0, 0, $2018, $2019, $201C,
    $201D, $2022, $2013, $2014, $02DC, $2122, $0161, $203A, $0153, 0, $017E, $0178);
  /// Media types by extension, for attachments without one.
  cTypes: array[0..15, 0..1] of string = (('.txt', 'text/plain'), ('.htm', 'text/html'),
    ('.html', 'text/html'), ('.pdf', 'application/pdf'), ('.png', 'image/png'),
    ('.jpg', 'image/jpeg'), ('.jpeg', 'image/jpeg'), ('.gif', 'image/gif'),
    ('.svg', 'image/svg+xml'), ('.zip', 'application/zip'), ('.xml', 'application/xml'),
    ('.json', 'application/json'), ('.csv', 'text/csv'), ('.eml', 'message/rfc822'),
    ('.ics', 'text/calendar'), ('.xlsx', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'));
  /// Days and months of RFC 5322.
  cDays: array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  /// Months of RFC 5322.
  cMonths: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
    'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

type
  /// Buffers the writer's output (64 KB) before it reaches the stream.
  TRALMailOut = class
  private
    /// The pending output bytes.
    FBuffer: TBytes;
    /// Bytes used in the buffer.
    FCount: IntegerRAL;
    /// The stream written to.
    FStream: TStream;
  public
    /// Creates a writer over AStream.
    constructor Create(AStream: TStream);
    destructor Destroy; override;

    /// Writes what is buffered.
    procedure Flush;
    /// Writes ASize bytes.
    procedure WriteBuffer(const AData; ASize: IntegerRAL);
    /// Writes text and CRLF.
    procedure WriteLine(const AText: StringRAL);
    /// Writes text.
    procedure WriteText(const AText: StringRAL);
  end;

  /// Reads a stream line by line (CRLF or LF), with the line breaks it had.
  TRALMailIn = class
  private
    /// The bytes read ahead.
    FBuffer: TBytes;
    /// Bytes in the buffer.
    FCount: IntegerRAL;
    /// The next byte to give.
    FPos: IntegerRAL;
    /// The stream read from.
    FStream: TStream;

    /// Reads more of the stream; False at its end.
    function Fill: Boolean;
  public
    /// Creates a reader over AStream.
    constructor Create(AStream: TStream);

    /// The next line without its break; False at the end. ABreak says whether it had one.
    function ReadLine(out ALine: StringRAL; out ABreak: Boolean): Boolean;
  end;

/// Appends one byte to AText as it is (no code page conversion on Delphi).
procedure AppendByte(var AText: StringRAL; AValue: Byte);
var
  vLength: IntegerRAL;
begin
  vLength := Length(AText);
  SetLength(AText, vLength + 1);
  PByte(NativeUInt(Pointer(AText)) + NativeUInt(vLength))^ := AValue;
end;

/// Appends a Unicode code point as UTF-8.
procedure AppendUTF8(var AText: StringRAL; ACode: Cardinal);
begin
  if ACode < $80 then
    AppendByte(AText, ACode)
  else if ACode < $800 then
  begin
    AppendByte(AText, $C0 or (ACode shr 6));
    AppendByte(AText, $80 or (ACode and $3F));
  end
  else
  begin
    AppendByte(AText, $E0 or (ACode shr 12));
    AppendByte(AText, $80 or ((ACode shr 6) and $3F));
    AppendByte(AText, $80 or (ACode and $3F));
  end;
end;

/// Whether AText is printable ASCII (space included).
function IsPlainASCII(const AText: StringRAL): Boolean;
var
  vInt: IntegerRAL;
begin
  for vInt := 1 to Length(AText) do
    if (Ord(AText[vInt]) < 32) or (Ord(AText[vInt]) > 126) then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

/// Lower case of ASCII letters only.
function LowerASCII(const AText: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := AText;
  for vInt := 1 to Length(Result) do
    if Result[vInt] in ['A'..'Z'] then
      Result[vInt] := CharRAL(Ord(Result[vInt]) + 32);
end;

/// A random hex text of ALength*2 characters (boundaries, Message-IDs).
function RandomHex(ALength: IntegerRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 1 to ALength do
    Result := Result + StringRAL(LowerCase(IntToHex(Random(256), 2)));
end;

/// The media type of a file name by its extension.
function TypeOfFile(const AName: string): StringRAL;
var
  vExt: string;
  vInt: IntegerRAL;
begin
  vExt := LowerCase(ExtractFileExt(AName));
  for vInt := Low(cTypes) to High(cTypes) do
    if cTypes[vInt, 0] = vExt then
    begin
      Result := StringRAL(cTypes[vInt, 1]);
      Exit;
    end;
  Result := 'application/octet-stream';
end;

{ A value quoted when it has characters a token (AWords False: a MIME
  parameter) or a phrase of words (AWords True: a display name) cannot hold. }
function QuoteIfNeeded(const AValue: StringRAL; AWords: Boolean = False): StringRAL;
var
  vInt: IntegerRAL;
  vNeed: Boolean;
begin
  vNeed := AValue = '';
  for vInt := 1 to Length(AValue) do
    if (AValue[vInt] in ['(', ')', '<', '>', '@', ',', ';', ':', '\', '"', '[', ']', '.']) or
       (not AWords and (AValue[vInt] in ['/', '?', '=', ' ', #9])) then
      vNeed := True;
  if not vNeed then
  begin
    Result := AValue;
    Exit;
  end;
  Result := '"';
  for vInt := 1 to Length(AValue) do
  begin
    if AValue[vInt] in ['"', '\'] then
      Result := Result + '\';
    AppendByte(Result, Ord(AValue[vInt]));
  end;
  Result := Result + '"';
end;

function RALMailPos(const ASub, AText: RawByteString): IntegerRAL;
var
  vInt, vLen: IntegerRAL;
begin
  vLen := Length(ASub);
  if vLen > 0 then
    for vInt := 1 to Length(AText) - vLen + 1 do
      if (AText[vInt] = ASub[1]) and ((vLen = 1) or CompareMem(@AText[vInt], @ASub[1], vLen)) then
      begin
        Result := vInt;
        Exit;
      end;
  Result := 0;
end;

function RALMailEncodeWord(const AText: StringRAL): StringRAL;
var
  vStart, vLen: IntegerRAL;
  vChunk: StringRAL;
begin
  if IsPlainASCII(AText) then
  begin
    Result := AText;
    Exit;
  end;
  // base64 words of at most 45 bytes (60 encoded), never cutting a UTF-8 sequence
  Result := '';
  vStart := 1;
  while vStart <= Length(AText) do
  begin
    vLen := 45;
    if vStart + vLen - 1 > Length(AText) then
      vLen := Length(AText) - vStart + 1
    else
      while (vLen > 1) and ((Ord(AText[vStart + vLen]) and $C0) = $80) do
        Dec(vLen);
    vChunk := Copy(AText, vStart, vLen);
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + '=?UTF-8?B?' + TRALBase64.Encode(vChunk) + '?=';
    Inc(vStart, vLen);
  end;
end;

/// Decodes one encoded word's text; AOk False when it is not one.
function DecodeWord(const AWord: StringRAL; out AOk: Boolean): StringRAL;
var
  vCharset, vEnc, vText: StringRAL;
  vP1, vP2, vInt: IntegerRAL;
begin
  Result := '';
  AOk := False;
  // =?charset?X?text?=
  if (Length(AWord) < 8) or (Copy(AWord, 1, 2) <> '=?') or
     (Copy(AWord, Length(AWord) - 1, 2) <> '?=') then
    Exit;
  vP1 := RALMailPos('?', Copy(AWord, 3, MaxInt)) + 2;
  if vP1 <= 2 then
    Exit;
  vCharset := Copy(AWord, 3, vP1 - 3);
  // a language after * (RFC 2231) is dropped
  if RALMailPos('*', vCharset) > 0 then
    vCharset := Copy(vCharset, 1, RALMailPos('*', vCharset) - 1);
  vP2 := RALMailPos('?', Copy(AWord, vP1 + 1, MaxInt)) + vP1;
  if (vP2 <= vP1) or (vP2 <> vP1 + 2) then
    Exit;
  vEnc := LowerASCII(Copy(AWord, vP1 + 1, 1));
  vText := Copy(AWord, vP2 + 1, Length(AWord) - vP2 - 2);
  if vEnc = 'b' then
  begin
    while Length(vText) mod 4 <> 0 do
      vText := vText + '=';
    Result := TRALBase64.Decode(vText);
  end
  else if vEnc = 'q' then
  begin
    for vInt := 1 to Length(vText) do
      if vText[vInt] = '_' then
        vText[vInt] := ' ';
    Result := RALMailDecodeQP(vText);
  end
  else
    Exit;
  Result := RALMailCharsetToUTF8(Result, vCharset);
  AOk := True;
end;

function RALMailDecodeWords(const AText: StringRAL): StringRAL;
var
  vPos, vEnd, vScan: IntegerRAL;
  vWord, vDecoded, vGap: StringRAL;
  vOk, vLastWasWord: Boolean;
begin
  Result := '';
  vPos := 1;
  vLastWasWord := False;
  vGap := '';
  while vPos <= Length(AText) do
  begin
    if (AText[vPos] = '=') and (vPos < Length(AText)) and (AText[vPos + 1] = '?') then
    begin
      // find the end ?= after charset?enc?text
      vScan := vPos + 2;
      vEnd := 0;
      // skip charset and encoding (two '?')
      while (vScan <= Length(AText)) and (AText[vScan] <> '?') do
        Inc(vScan);
      Inc(vScan, 3);
      while vScan < Length(AText) do
      begin
        if (AText[vScan] = '?') and (AText[vScan + 1] = '=') then
        begin
          vEnd := vScan + 1;
          Break;
        end;
        Inc(vScan);
      end;
      if vEnd > 0 then
      begin
        vWord := Copy(AText, vPos, vEnd - vPos + 1);
        vDecoded := DecodeWord(vWord, vOk);
        if vOk then
        begin
          // white space between two encoded words is dropped (RFC 2047 6.2)
          if not vLastWasWord then
            Result := Result + vGap;
          vGap := '';
          Result := Result + vDecoded;
          vLastWasWord := True;
          vPos := vEnd + 1;
          Continue;
        end;
      end;
    end;
    if AText[vPos] in [' ', #9] then
    begin
      AppendByte(vGap, Ord(AText[vPos]));
      Inc(vPos);
      Continue;
    end;
    Result := Result + vGap;
    vGap := '';
    vLastWasWord := False;
    AppendByte(Result, Ord(AText[vPos]));
    Inc(vPos);
  end;
  Result := Result + vGap;
end;

function RALMailEncodeQP(const AText: StringRAL): StringRAL;
var
  vInt, vCol: IntegerRAL;
  vChar: Byte;
  vPiece: StringRAL;

  procedure Put(const APiece: StringRAL);
  begin
    // a soft break keeps lines at 76 with the '='
    if vCol + Length(APiece) > 75 then
    begin
      Result := Result + '='#13#10;
      vCol := 0;
    end;
    Result := Result + APiece;
    Inc(vCol, Length(APiece));
  end;

begin
  Result := '';
  vCol := 0;
  vInt := 1;
  while vInt <= Length(AText) do
  begin
    vChar := Ord(AText[vInt]);
    if (vChar = 13) or (vChar = 10) then
    begin
      // a hard break, CRLF whatever came
      Result := Result + #13#10;
      vCol := 0;
      if (vChar = 13) and (vInt < Length(AText)) and (AText[vInt + 1] = #10) then
        Inc(vInt);
      Inc(vInt);
      Continue;
    end;
    if (vChar in [32, 9]) then
    begin
      // white space before a break or at the end is encoded
      if (vInt = Length(AText)) or (AText[vInt + 1] in [#13, #10]) then
        vPiece := StringRAL('=' + IntToHex(vChar, 2))
      else
      begin
        vPiece := '';
        AppendByte(vPiece, vChar);
      end;
    end
    else if (vChar < 33) or (vChar > 126) or (vChar = Ord('=')) or
            ((vChar = Ord('.')) and (vCol = 0)) then
      vPiece := StringRAL('=' + IntToHex(vChar, 2))
    else
    begin
      vPiece := '';
      AppendByte(vPiece, vChar);
    end;
    Put(vPiece);
    Inc(vInt);
  end;
end;

function RALMailDecodeQP(const AText: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
  vHex: string;
begin
  Result := '';
  vInt := 1;
  while vInt <= Length(AText) do
  begin
    if AText[vInt] = '=' then
    begin
      if (vInt + 1 <= Length(AText)) and (AText[vInt + 1] in [#13, #10]) then
      begin
        // a soft break
        Inc(vInt, 2);
        if (vInt <= Length(AText)) and (AText[vInt - 1] = #13) and (AText[vInt] = #10) then
          Inc(vInt);
        Continue;
      end;
      vHex := string(Copy(AText, vInt + 1, 2));
      if (Length(vHex) = 2) and CharInSet(vHex[1], ['0'..'9', 'A'..'F', 'a'..'f']) and
         CharInSet(vHex[2], ['0'..'9', 'A'..'F', 'a'..'f']) then
      begin
        AppendByte(Result, StrToInt('$' + vHex));
        Inc(vInt, 3);
        Continue;
      end;
    end;
    AppendByte(Result, Ord(AText[vInt]));
    Inc(vInt);
  end;
end;

function RALMailCharsetToUTF8(const AText: StringRAL; const ACharset: StringRAL): StringRAL;
var
  vCharset: StringRAL;
  vInt: IntegerRAL;
  vByte: Byte;
begin
  vCharset := LowerASCII(RALTrim(ACharset));
  if (vCharset = '') or (vCharset = 'utf-8') or (vCharset = 'utf8') or
     (vCharset = 'us-ascii') or (vCharset = 'ascii') then
  begin
    Result := AText;
    Exit;
  end;
  if (vCharset <> 'iso-8859-1') and (vCharset <> 'latin1') and (vCharset <> 'iso-8859-15') and
     (vCharset <> 'windows-1252') and (vCharset <> 'cp1252') then
  begin
    Result := AText;
    Exit;
  end;
  Result := '';
  for vInt := 1 to Length(AText) do
  begin
    vByte := Ord(AText[vInt]);
    if (vByte >= $80) and (vByte <= $9F) and ((vCharset = 'windows-1252') or (vCharset = 'cp1252')) and
       (cCP1252[vByte] <> 0) then
      AppendUTF8(Result, cCP1252[vByte])
    else if (vCharset = 'iso-8859-15') and (vByte = $A4) then
      AppendUTF8(Result, $20AC)
    else
      AppendUTF8(Result, vByte);
  end;
end;

function RALMailDate(ADate: TDateTime): StringRAL;
var
  vBias: Int64;
  vSign: Char;
begin
  vBias := Round((ADate - RALDateTimeToGMT(ADate)) * 1440);
  if vBias < 0 then
    vSign := '-'
  else
    vSign := '+';
  Result := StringRAL(Format('%s, %d %s %.4d %s %s%.2d%.2d', [cDays[DayOfWeek(ADate)],
    DayOf(ADate), cMonths[MonthOf(ADate)], YearOf(ADate),
    FormatDateTime('hh":"nn":"ss', ADate), vSign, Abs(vBias) div 60, Abs(vBias) mod 60]));
end;

function RALMailParseDate(const AText: StringRAL): TDateTime;
var
  vParts: TStringList;
  vText, vZone: string;
  vDay, vMonth, vYear, vInt, vH, vN, vS, vOffset: IntegerRAL;
  vTime: TStringList;
begin
  Result := 0;
  vText := Trim(string(AText));
  // a comment (UT) or the weekday are dropped
  if Pos('(', vText) > 0 then
    vText := Trim(Copy(vText, 1, Pos('(', vText) - 1));
  if Pos(',', vText) > 0 then
    vText := Trim(Copy(vText, Pos(',', vText) + 1, MaxInt));
  vParts := TStringList.Create;
  vTime := TStringList.Create;
  try
    vParts.Delimiter := ' ';
    vParts.StrictDelimiter := False;
    vParts.DelimitedText := vText;
    if vParts.Count < 4 then
      Exit;
    vDay := StrToIntDef(vParts[0], 0);
    vMonth := 0;
    for vInt := 1 to 12 do
      if SameText(cMonths[vInt], vParts[1]) then
        vMonth := vInt;
    vYear := StrToIntDef(vParts[2], 0);
    if vYear < 50 then
      Inc(vYear, 2000)
    else if vYear < 100 then
      Inc(vYear, 1900);
    vTime.Delimiter := ':';
    vTime.StrictDelimiter := True;
    vTime.DelimitedText := vParts[3];
    vH := StrToIntDef(vTime[0], 0);
    if vTime.Count > 1 then
      vN := StrToIntDef(vTime[1], 0)
    else
      vN := 0;
    if vTime.Count > 2 then
      vS := StrToIntDef(vTime[2], 0)
    else
      vS := 0;
    vOffset := 0;
    if vParts.Count > 4 then
    begin
      vZone := vParts[4];
      if (Length(vZone) = 5) and CharInSet(vZone[1], ['+', '-']) then
      begin
        vOffset := StrToIntDef(Copy(vZone, 2, 2), 0) * 60 + StrToIntDef(Copy(vZone, 4, 2), 0);
        if vZone[1] = '-' then
          vOffset := -vOffset;
      end;
    end;
    if (vMonth = 0) or (vDay = 0) or not TryEncodeDateTime(vYear, vMonth, vDay, vH, vN, vS, 0, Result) then
    begin
      Result := 0;
      Exit;
    end;
    // to UTC, then to local
    Result := RALGMTToDateTime(IncMinute(Result, -vOffset));
  finally
    vParts.Free;
    vTime.Free;
  end;
end;

function RALMailHeaderMain(const AValue: StringRAL): StringRAL;
var
  vPos: IntegerRAL;
begin
  vPos := RALMailPos(';', AValue);
  if vPos > 0 then
    Result := LowerASCII(RALTrim(Copy(AValue, 1, vPos - 1)))
  else
    Result := LowerASCII(RALTrim(AValue));
end;

function RALMailHeaderParam(const AValue, AName: StringRAL): StringRAL;
var
  vRest, vPart, vKey, vVal: StringRAL;
  vPos, vInt: IntegerRAL;
  vQuoted: Boolean;
  vPieces: TStringList;
  vExtended: Boolean;
  vCharset: StringRAL;
begin
  Result := '';
  vPieces := TStringList.Create;
  try
    vPieces.Sorted := False;
    vExtended := False;
    vCharset := '';
    // split on ';' outside quotes
    vRest := AValue;
    vPos := RALMailPos(';', vRest);
    if vPos = 0 then
      Exit;
    vRest := Copy(vRest, vPos + 1, MaxInt);
    while vRest <> '' do
    begin
      vPart := '';
      vQuoted := False;
      vInt := 1;
      while vInt <= Length(vRest) do
      begin
        if vRest[vInt] = '"' then
          vQuoted := not vQuoted
        else if (vRest[vInt] = '\') and vQuoted and (vInt < Length(vRest)) then
        begin
          AppendByte(vPart, Ord(vRest[vInt]));
          Inc(vInt);
        end
        else if (vRest[vInt] = ';') and not vQuoted then
          Break;
        AppendByte(vPart, Ord(vRest[vInt]));
        Inc(vInt);
      end;
      vRest := Copy(vRest, vInt + 1, MaxInt);
      vPos := RALMailPos('=', vPart);
      if vPos = 0 then
        Continue;
      vKey := LowerASCII(RALTrim(Copy(vPart, 1, vPos - 1)));
      vVal := RALTrim(Copy(vPart, vPos + 1, MaxInt));
      if (Length(vVal) >= 2) and (vVal[1] = '"') and (vVal[Length(vVal)] = '"') then
      begin
        vVal := Copy(vVal, 2, Length(vVal) - 2);
        vVal := StringRAL(StringReplace(StringReplace(string(vVal), '\"', '"', [rfReplaceAll]),
          '\\', '\', [rfReplaceAll]));
      end;
      // name, name*, name*0, name*0*, name*1...
      if vKey = LowerASCII(AName) then
      begin
        Result := vVal;
        Exit;
      end;
      if Copy(vKey, 1, Length(AName) + 1) = LowerASCII(AName) + '*' then
      begin
        if vKey[Length(vKey)] = '*' then
        begin
          vExtended := True;
          // the first piece carries charset'language'
          if (vCharset = '') and (RALMailPos('''', vVal) > 0) then
          begin
            vCharset := Copy(vVal, 1, RALMailPos('''', vVal) - 1);
            vVal := Copy(vVal, RALMailPos('''', vVal) + 1, MaxInt);
            vVal := Copy(vVal, RALMailPos('''', vVal) + 1, MaxInt);
          end;
          vVal := StringRAL(StringReplace(string(vVal), '%', '=', [rfReplaceAll]));
          vVal := RALMailDecodeQP(vVal);
        end;
        vPieces.Add(string(vVal));
      end;
    end;
    if vPieces.Count > 0 then
    begin
      Result := '';
      for vInt := 0 to vPieces.Count - 1 do
        Result := Result + StringRAL(vPieces[vInt]);
      if vExtended then
        Result := RALMailCharsetToUTF8(Result, vCharset);
    end;
  finally
    vPieces.Free;
  end;
end;

/// A parameter in the RFC 2231 form when it is not plain ASCII.
function ParamText(const AName, AValue: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  if IsPlainASCII(AValue) then
  begin
    Result := AName + '=' + QuoteIfNeeded(AValue);
    Exit;
  end;
  Result := AName + '*=UTF-8''''';
  for vInt := 1 to Length(AValue) do
    if AValue[vInt] in ['A'..'Z', 'a'..'z', '0'..'9', '.', '-', '_'] then
      AppendByte(Result, Ord(AValue[vInt]))
    else
      Result := Result + StringRAL('%' + IntToHex(Ord(AValue[vInt]), 2));
end;

{ TRALMailOut }

constructor TRALMailOut.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  SetLength(FBuffer, 65536);
end;

destructor TRALMailOut.Destroy;
begin
  Flush;
  inherited Destroy;
end;

procedure TRALMailOut.Flush;
begin
  if FCount > 0 then
    FStream.WriteBuffer(FBuffer[0], FCount);
  FCount := 0;
end;

procedure TRALMailOut.WriteBuffer(const AData; ASize: IntegerRAL);
begin
  if ASize <= 0 then
    Exit;
  if FCount + ASize > Length(FBuffer) then
    Flush;
  if ASize > Length(FBuffer) then
    FStream.WriteBuffer(AData, ASize)
  else
  begin
    Move(AData, FBuffer[FCount], ASize);
    Inc(FCount, ASize);
  end;
end;

procedure TRALMailOut.WriteLine(const AText: StringRAL);
begin
  WriteText(AText);
  WriteText(#13#10);
end;

procedure TRALMailOut.WriteText(const AText: StringRAL);
begin
  if AText <> '' then
    WriteBuffer(Pointer(AText)^, Length(AText));
end;

{ TRALMailIn }

constructor TRALMailIn.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  SetLength(FBuffer, 65536);
end;

function TRALMailIn.Fill: Boolean;
var
  vRead: IntegerRAL;
begin
  if FPos < FCount then
  begin
    Move(FBuffer[FPos], FBuffer[0], FCount - FPos);
    Dec(FCount, FPos);
  end
  else
    FCount := 0;
  FPos := 0;
  if FCount = Length(FBuffer) then
    SetLength(FBuffer, Length(FBuffer) * 2);
  Result := False;
  if FStream = nil then
    Exit;
  vRead := FStream.Read(FBuffer[FCount], Length(FBuffer) - FCount);
  if vRead > 0 then
  begin
    Inc(FCount, vRead);
    Result := True;
  end;
end;

function TRALMailIn.ReadLine(out ALine: StringRAL; out ABreak: Boolean): Boolean;
var
  vInt, vEnd: IntegerRAL;
begin
  ALine := '';
  ABreak := False;
  while True do
  begin
    for vInt := FPos to FCount - 1 do
      if FBuffer[vInt] = 10 then
      begin
        vEnd := vInt;
        if (vEnd > FPos) and (FBuffer[vEnd - 1] = 13) then
          Dec(vEnd);
        SetLength(ALine, vEnd - FPos);
        if vEnd > FPos then
          Move(FBuffer[FPos], Pointer(ALine)^, vEnd - FPos);
        FPos := vInt + 1;
        ABreak := True;
        Result := True;
        Exit;
      end;
    if not Fill then
    begin
      // the last line, without a break
      Result := FCount > FPos;
      if Result then
      begin
        SetLength(ALine, FCount - FPos);
        Move(FBuffer[FPos], Pointer(ALine)^, FCount - FPos);
        FPos := FCount;
      end;
      Exit;
    end;
  end;
end;

{ TRALMailAddress }

procedure TRALMailAddress.Assign(ASource: TPersistent);
begin
  if ASource is TRALMailAddress then
  begin
    FAddress := TRALMailAddress(ASource).Address;
    FName := TRALMailAddress(ASource).Name;
  end
  else
    inherited Assign(ASource);
end;

function TRALMailAddress.HeaderText(AUTF8: Boolean): StringRAL;
begin
  if FName = '' then
    Result := FAddress
  else if AUTF8 or IsPlainASCII(FName) then
    Result := QuoteIfNeeded(FName, True) + ' <' + FAddress + '>'
  else
    Result := RALMailEncodeWord(FName) + ' <' + FAddress + '>';
end;

{ TRALMailAddresses }

constructor TRALMailAddresses.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALMailAddress);
end;

function TRALMailAddresses.Add(const AAddress, AName: StringRAL): TRALMailAddress;
begin
  Result := TRALMailAddress(inherited Add);
  Result.Address := AAddress;
  Result.Name := AName;
end;

procedure TRALMailAddresses.AddText(const AText: StringRAL);
var
  vInt, vDepth: IntegerRAL;
  vQuoted: Boolean;
  vPart: StringRAL;

  procedure Take(const AItem: StringRAL);
  var
    vItem, vName, vAddr: StringRAL;
    vLt, vGt: IntegerRAL;
  begin
    vItem := RALTrim(AItem);
    if vItem = '' then
      Exit;
    vLt := RALMailPos('<', vItem);
    vGt := RALMailPos('>', vItem);
    if (vLt > 0) and (vGt > vLt) then
    begin
      vAddr := RALTrim(Copy(vItem, vLt + 1, vGt - vLt - 1));
      vName := RALTrim(Copy(vItem, 1, vLt - 1));
      if (Length(vName) >= 2) and (vName[1] = '"') and (vName[Length(vName)] = '"') then
        vName := StringRAL(StringReplace(string(Copy(vName, 2, Length(vName) - 2)), '\"', '"',
          [rfReplaceAll]));
      vName := RALMailDecodeWords(vName);
    end
    else
    begin
      vAddr := vItem;
      vName := '';
    end;
    Add(vAddr, vName);
  end;

begin
  // split on commas outside quotes and angle brackets
  vPart := '';
  vQuoted := False;
  vDepth := 0;
  for vInt := 1 to Length(AText) do
  begin
    if AText[vInt] = '"' then
      vQuoted := not vQuoted
    else if not vQuoted and (AText[vInt] = '<') then
      Inc(vDepth)
    else if not vQuoted and (AText[vInt] = '>') then
      Dec(vDepth)
    else if not vQuoted and (vDepth = 0) and (AText[vInt] = ',') then
    begin
      Take(vPart);
      vPart := '';
      Continue;
    end;
    AppendByte(vPart, Ord(AText[vInt]));
  end;
  Take(vPart);
end;

function TRALMailAddresses.GetItem(AIndex: IntegerRAL): TRALMailAddress;
begin
  Result := TRALMailAddress(inherited Items[AIndex]);
end;

function TRALMailAddresses.HeaderText(AUTF8: Boolean): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 0 to Count - 1 do
  begin
    if vInt > 0 then
      Result := Result + ', ';
    Result := Result + Items[vInt].HeaderText(AUTF8);
  end;
end;

{ TRALMailAttachment }

destructor TRALMailAttachment.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TRALMailAttachment.Assign(ASource: TPersistent);
var
  vSource: TStream;
begin
  if ASource is TRALMailAttachment then
  begin
    FContentID := TRALMailAttachment(ASource).ContentID;
    FContentType := TRALMailAttachment(ASource).ContentType;
    FFileName := TRALMailAttachment(ASource).FileName;
    FInline := TRALMailAttachment(ASource).IsInline;
    FPath := TRALMailAttachment(ASource).Path;
    FreeAndNil(FData);
    if TRALMailAttachment(ASource).Data <> nil then
    begin
      vSource := TRALMailAttachment(ASource).Data;
      vSource.Position := 0;
      LoadFromStream(vSource);
    end;
  end
  else
    inherited Assign(ASource);
end;

procedure TRALMailAttachment.LoadFromFile(const APath: string);
begin
  FPath := APath;
  FreeAndNil(FData);
  if FFileName = '' then
    FFileName := StringRAL(ExtractFileName(APath));
end;

procedure TRALMailAttachment.LoadFromStream(AStream: TStream);
begin
  FreeAndNil(FData);
  FPath := '';
  FData := TMemoryStream.Create;
  FData.CopyFrom(AStream, AStream.Size - AStream.Position);
  FData.Position := 0;
end;

procedure TRALMailAttachment.LoadFromText(const AText: StringRAL);
begin
  FreeAndNil(FData);
  FPath := '';
  FData := TMemoryStream.Create;
  if AText <> '' then
    FData.WriteBuffer(Pointer(AText)^, Length(AText));
  FData.Position := 0;
end;

function TRALMailAttachment.OpenContent: TStream;
begin
  if FData <> nil then
  begin
    // a view the caller frees, over the attachment's own data
    Result := TMemoryStream.Create;
    FData.Position := 0;
    Result.CopyFrom(FData, FData.Size);
    Result.Position := 0;
  end
  else if FPath <> '' then
    Result := TFileStream.Create(FPath, fmOpenRead or fmShareDenyWrite)
  else
    Result := TMemoryStream.Create;
end;

procedure TRALMailAttachment.SaveToFile(const APath: string);
var
  vOut: TFileStream;
  vIn: TStream;
begin
  vIn := OpenContent;
  try
    vOut := TFileStream.Create(APath, fmCreate);
    try
      vOut.CopyFrom(vIn, 0);
    finally
      vOut.Free;
    end;
  finally
    vIn.Free;
  end;
end;

procedure TRALMailAttachment.SetData(AValue: TStream);
begin
  if AValue = FData then
    Exit;
  FData.Free;
  FData := AValue;
  if FData <> nil then
    FPath := '';
end;

function TRALMailAttachment.Size: Int64;
var
  vInfo: TSearchRec;
begin
  if FData <> nil then
    Result := FData.Size
  else if (FPath <> '') and (FindFirst(FPath, faAnyFile, vInfo) = 0) then
  begin
    Result := vInfo.Size;
    FindClose(vInfo);
  end
  else
    Result := -1;
end;

{ TRALMailAttachments }

constructor TRALMailAttachments.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALMailAttachment);
end;

function TRALMailAttachments.AddFile(const APath: string;
  const AFileName: StringRAL): TRALMailAttachment;
begin
  Result := TRALMailAttachment(inherited Add);
  Result.LoadFromFile(APath);
  if AFileName <> '' then
    Result.FileName := AFileName;
end;

function TRALMailAttachments.AddInline(const APath: string;
  const AContentID: StringRAL): TRALMailAttachment;
begin
  Result := AddFile(APath);
  Result.ContentID := AContentID;
  Result.IsInline := True;
end;

function TRALMailAttachments.GetItem(AIndex: IntegerRAL): TRALMailAttachment;
begin
  Result := TRALMailAttachment(inherited Items[AIndex]);
end;

{ TRALMailMessage }

constructor TRALMailMessage.Create;
begin
  inherited Create;
  FFrom := TRALMailAddresses.Create(Self);
  FReplyTo := TRALMailAddresses.Create(Self);
  FRecipients := TRALMailAddresses.Create(Self);
  FCC := TRALMailAddresses.Create(Self);
  FBCC := TRALMailAddresses.Create(Self);
  FAttachments := TRALMailAttachments.Create(Self);
  FHeaders := TStringList.Create;
  FSpoolAbove := 4 * 1024 * 1024;
end;

destructor TRALMailMessage.Destroy;
begin
  FFrom.Free;
  FReplyTo.Free;
  FRecipients.Free;
  FCC.Free;
  FBCC.Free;
  FAttachments.Free;
  FHeaders.Free;
  inherited Destroy;
end;

procedure TRALMailMessage.AllRecipients(AList: TStrings);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FRecipients.Count - 1 do
    AList.Add(string(FRecipients[vInt].Address));
  for vInt := 0 to FCC.Count - 1 do
    AList.Add(string(FCC[vInt].Address));
  for vInt := 0 to FBCC.Count - 1 do
    AList.Add(string(FBCC[vInt].Address));
end;

function TRALMailMessage.AsText(AOptions: TRALMailWriteOptions): StringRAL;
var
  vStream: TMemoryStream;
begin
  vStream := TMemoryStream.Create;
  try
    SaveToStream(vStream, AOptions);
    SetLength(Result, vStream.Size);
    if vStream.Size > 0 then
      Move(vStream.Memory^, Pointer(Result)^, vStream.Size);
  finally
    vStream.Free;
  end;
end;

procedure TRALMailMessage.Assign(ASource: TPersistent);
var
  vSource: TRALMailMessage;
begin
  if ASource is TRALMailMessage then
  begin
    vSource := TRALMailMessage(ASource);
    FFrom.Assign(vSource.From);
    FReplyTo.Assign(vSource.ReplyTo);
    FRecipients.Assign(vSource.Recipients);
    FCC.Assign(vSource.CC);
    FBCC.Assign(vSource.BCC);
    FAttachments.Assign(vSource.Attachments);
    FHeaders.Assign(vSource.Headers);
    FSubject := vSource.Subject;
    FText := vSource.Text;
    FHTML := vSource.HTML;
    FDate := vSource.Date;
    FMessageID := vSource.MessageID;
    FInReplyTo := vSource.InReplyTo;
    FReferences := vSource.References;
    FReadReceiptTo := vSource.ReadReceiptTo;
    FPriority := vSource.Priority;
    FSender := vSource.Sender;
    FSpoolAbove := vSource.SpoolAbove;
  end
  else
    inherited Assign(ASource);
end;

procedure TRALMailMessage.Clear;
begin
  FFrom.Clear;
  FReplyTo.Clear;
  FRecipients.Clear;
  FCC.Clear;
  FBCC.Clear;
  FAttachments.Clear;
  FHeaders.Clear;
  FSubject := '';
  FText := '';
  FHTML := '';
  FDate := 0;
  FMessageID := '';
  FInReplyTo := '';
  FReferences := '';
  FReadReceiptTo := '';
  FPriority := mpNormal;
  FSender := '';
end;

procedure TRALMailMessage.LoadFromFile(const APath: string);
var
  vStream: TFileStream;
begin
  vStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(vStream);
  finally
    vStream.Free;
  end;
end;

type
  /// The headers of one part, unfolded.
  TRALMailPartHead = record
    /// Content-Type.
    ContentType: StringRAL;
    /// Content-Transfer-Encoding, lower case.
    Encoding: StringRAL;
    /// Content-Disposition.
    Disposition: StringRAL;
    /// Content-ID, without angle brackets.
    ContentID: StringRAL;
  end;

  /// The reader of a message: parts by boundaries, decoded into their targets.
  TRALMailReader = class
  private
    /// True when FPending holds a line to give back.
    FHasPending: Boolean;
    /// The buffered input.
    FIn: TRALMailIn;
    /// The message being filled.
    FMessage: TRALMailMessage;
    /// The line given back by a part reader, to read again.
    FPending: StringRAL;
    /// Whether FPending had a line break.
    FPendingBreak: Boolean;

    /// The next line, or the one pushed back.
    function NextLine(out ALine: StringRAL; out ABreak: Boolean): Boolean;
    /// Reads a part body up to one of ABoundaries; returns the boundary line met (or '').
    function ReadBody(ATarget: TStream; const AEncoding: StringRAL;
      const ABoundaries: TStrings): StringRAL;
    /// Reads a block of header lines (unfolded) up to the empty line.
    procedure ReadHeaders(AList: TStrings);
    /// Reads a part whose headers are in AHeaders; returns the boundary line that ends.
    function ReadPart(AHeaders: TStrings; const ABoundaries: TStrings): StringRAL;
  public
    /// Creates a reader that fills AMessage from AStream.
    constructor Create(AMessage: TRALMailMessage; AStream: TStream);
    destructor Destroy; override;

    /// Reads the whole message.
    procedure Run;
  end;

/// The value of the header AName in AList (Name: value lines), '' when absent.
function HeaderOf(AList: TStrings; const AName: StringRAL): StringRAL;
var
  vInt, vPos: IntegerRAL;
  vLine: StringRAL;
begin
  for vInt := 0 to AList.Count - 1 do
  begin
    vLine := StringRAL(AList[vInt]);
    vPos := RALMailPos(':', vLine);
    if (vPos > 0) and (LowerASCII(RALTrim(Copy(vLine, 1, vPos - 1))) = AName) then
    begin
      Result := RALTrim(Copy(vLine, vPos + 1, MaxInt));
      Exit;
    end;
  end;
  Result := '';
end;

constructor TRALMailReader.Create(AMessage: TRALMailMessage; AStream: TStream);
begin
  inherited Create;
  FMessage := AMessage;
  FIn := TRALMailIn.Create(AStream);
end;

destructor TRALMailReader.Destroy;
begin
  FIn.Free;
  inherited Destroy;
end;

function TRALMailReader.NextLine(out ALine: StringRAL; out ABreak: Boolean): Boolean;
begin
  if FHasPending then
  begin
    ALine := FPending;
    ABreak := FPendingBreak;
    FHasPending := False;
    Result := True;
  end
  else
    Result := FIn.ReadLine(ALine, ABreak);
end;

function TRALMailReader.ReadBody(ATarget: TStream; const AEncoding: StringRAL;
  const ABoundaries: TStrings): StringRAL;
var
  vLine, vOut, vCarry, vB64: StringRAL;
  vBreak, vFirst, vPrevBreak: Boolean;
  vInt: IntegerRAL;
  vBytes: TBytes;
begin
  Result := '';
  vFirst := True;
  vPrevBreak := False;
  vCarry := '';
  while NextLine(vLine, vBreak) do
  begin
    // a boundary line ends the part
    if (ABoundaries <> nil) and (Copy(vLine, 1, 2) = '--') then
      for vInt := 0 to ABoundaries.Count - 1 do
        if (RALTrimRight(vLine) = '--' + StringRAL(ABoundaries[vInt])) or
           (RALTrimRight(vLine) = '--' + StringRAL(ABoundaries[vInt]) + '--') then
        begin
          Result := RALTrimRight(vLine);
          Exit;
        end;
    if AEncoding = 'base64' then
    begin
      // whole groups of 4 are decoded now, the rest waits for the next line
      vB64 := vCarry;
      for vInt := 1 to Length(vLine) do
        if vLine[vInt] in ['A'..'Z', 'a'..'z', '0'..'9', '+', '/', '='] then
          AppendByte(vB64, Ord(vLine[vInt]));
      vInt := (Length(vB64) div 4) * 4;
      vCarry := Copy(vB64, vInt + 1, MaxInt);
      if vInt > 0 then
      begin
        vBytes := TRALBase64.DecodeAsBytes(Copy(vB64, 1, vInt));
        if (Length(vBytes) > 0) and (ATarget <> nil) then
          ATarget.WriteBuffer(vBytes[0], Length(vBytes));
      end;
    end
    else if AEncoding = 'quoted-printable' then
    begin
      // the break before this line is text (the last one belongs to the boundary)
      if not vFirst and vPrevBreak and (ATarget <> nil) then
        ATarget.WriteBuffer(PAnsiChar(#13#10)^, 2);
      if (vLine <> '') and (vLine[Length(vLine)] = '=') then
      begin
        vOut := RALMailDecodeQP(Copy(vLine, 1, Length(vLine) - 1));
        vPrevBreak := False;
      end
      else
      begin
        vOut := RALMailDecodeQP(vLine);
        vPrevBreak := vBreak;
      end;
      if (vOut <> '') and (ATarget <> nil) then
        ATarget.WriteBuffer(Pointer(vOut)^, Length(vOut));
      vFirst := False;
      Continue;
    end
    else if ATarget <> nil then
    begin
      if not vFirst and vPrevBreak then
        ATarget.WriteBuffer(PAnsiChar(#13#10)^, 2);
      if vLine <> '' then
        ATarget.WriteBuffer(Pointer(vLine)^, Length(vLine));
      vPrevBreak := vBreak;
    end;
    vFirst := False;
  end;
end;

procedure TRALMailReader.ReadHeaders(AList: TStrings);
var
  vLine: StringRAL;
  vBreak: Boolean;
begin
  while NextLine(vLine, vBreak) do
  begin
    if vLine = '' then
      Exit;
    if (vLine[1] in [' ', #9]) and (AList.Count > 0) then
      // a folded line continues the previous one
      AList[AList.Count - 1] := AList[AList.Count - 1] + string(vLine)
    else
      AList.Add(string(vLine));
  end;
end;

function TRALMailReader.ReadPart(AHeaders: TStrings; const ABoundaries: TStrings): StringRAL;
var
  vType, vMain, vBoundary, vLine, vEnd, vName, vCharset, vText: StringRAL;
  vEncoding, vDisposition: StringRAL;
  vTarget: TStream;
  vSub: TStringList;
  vAttach: TRALMailAttachment;
  vIsAttachment: Boolean;
begin
  vType := HeaderOf(AHeaders, 'content-type');
  if vType = '' then
    vType := 'text/plain; charset=us-ascii';
  vMain := RALMailHeaderMain(vType);
  vEncoding := LowerASCII(HeaderOf(AHeaders, 'content-transfer-encoding'));
  vDisposition := HeaderOf(AHeaders, 'content-disposition');
  if Copy(vMain, 1, 10) = 'multipart/' then
  begin
    vBoundary := RALMailHeaderParam(vType, 'boundary');
    ABoundaries.Insert(0, string(vBoundary));
    try
      // the preamble, up to the first boundary
      vEnd := ReadBody(nil, '7bit-skip', ABoundaries);
      while (vEnd <> '') and (vEnd = '--' + vBoundary) do
      begin
        vSub := TStringList.Create;
        try
          ReadHeaders(vSub);
          vEnd := ReadPart(vSub, ABoundaries);
        finally
          vSub.Free;
        end;
      end;
      // the epilogue after this multipart's closing boundary
      if vEnd = '--' + vBoundary + '--' then
        vEnd := ReadBody(nil, '7bit-skip', ABoundaries);
    finally
      ABoundaries.Delete(ABoundaries.IndexOf(string(vBoundary)));
    end;
    Result := vEnd;
    Exit;
  end;
  vName := RALMailHeaderParam(vDisposition, 'filename');
  if vName = '' then
    vName := RALMailHeaderParam(vType, 'name');
  vName := RALMailDecodeWords(vName);
  vIsAttachment := (RALMailHeaderMain(vDisposition) = 'attachment') or (vName <> '') or
    ((vMain <> 'text/plain') and (vMain <> 'text/html'));
  if not vIsAttachment and (((vMain = 'text/plain') and (FMessage.Text = '')) or
     ((vMain = 'text/html') and (FMessage.HTML = ''))) then
  begin
    vTarget := TMemoryStream.Create;
    try
      Result := ReadBody(vTarget, vEncoding, ABoundaries);
      SetLength(vText, vTarget.Size);
      if vTarget.Size > 0 then
        Move(TMemoryStream(vTarget).Memory^, Pointer(vText)^, vTarget.Size);
      vCharset := RALMailHeaderParam(vType, 'charset');
      vText := RALMailCharsetToUTF8(vText, vCharset);
      if vMain = 'text/plain' then
        FMessage.Text := vText
      else
        FMessage.HTML := vText;
    finally
      vTarget.Free;
    end;
    Exit;
  end;
  vAttach := TRALMailAttachment(FMessage.Attachments.Add);
  vAttach.FileName := vName;
  vAttach.ContentType := vMain;
  vAttach.IsInline := RALMailHeaderMain(vDisposition) = 'inline';
  vLine := HeaderOf(AHeaders, 'content-id');
  if (Length(vLine) >= 2) and (vLine[1] = '<') then
    vLine := Copy(vLine, 2, Length(vLine) - 2);
  vAttach.ContentID := vLine;
  vTarget := TRALBodyStream.Create(-1, FMessage.SpoolAbove);
  try
    Result := ReadBody(vTarget, vEncoding, ABoundaries);
  except
    vTarget.Free;
    raise;
  end;
  vTarget.Position := 0;
  vAttach.Data := vTarget;
end;

procedure TRALMailReader.Run;
var
  vHeaders, vBoundaries: TStringList;
  vInt, vPos: IntegerRAL;
  vName, vValue, vLower: StringRAL;
  vKnown: Boolean;
  vKey: IntegerRAL;
begin
  vHeaders := TStringList.Create;
  vBoundaries := TStringList.Create;
  try
    ReadHeaders(vHeaders);
    for vInt := 0 to vHeaders.Count - 1 do
    begin
      vPos := Pos(':', vHeaders[vInt]);
      if vPos = 0 then
        Continue;
      vName := RALTrim(StringRAL(Copy(vHeaders[vInt], 1, vPos - 1)));
      vValue := RALTrim(StringRAL(Copy(vHeaders[vInt], vPos + 1, MaxInt)));
      vLower := LowerASCII(vName);
      vKnown := False;
      for vKey := Low(cKnown) to High(cKnown) do
        if cKnown[vKey] = vLower then
          vKnown := True;
      if vLower = 'from' then
        FMessage.From.AddText(vValue)
      else if vLower = 'to' then
        FMessage.Recipients.AddText(vValue)
      else if vLower = 'cc' then
        FMessage.CC.AddText(vValue)
      else if vLower = 'bcc' then
        FMessage.BCC.AddText(vValue)
      else if vLower = 'reply-to' then
        FMessage.ReplyTo.AddText(vValue)
      else if vLower = 'subject' then
        FMessage.Subject := RALMailDecodeWords(vValue)
      else if vLower = 'date' then
        FMessage.Date := RALMailParseDate(vValue)
      else if vLower = 'message-id' then
        FMessage.MessageID := StringRAL(StringReplace(StringReplace(string(vValue), '<', '', []), '>', '', []))
      else if vLower = 'in-reply-to' then
        FMessage.InReplyTo := StringRAL(StringReplace(StringReplace(string(vValue), '<', '', []), '>', '', []))
      else if vLower = 'references' then
        FMessage.References := vValue
      else if vLower = 'sender' then
        FMessage.Sender := vValue
      else if vLower = 'disposition-notification-to' then
        FMessage.ReadReceiptTo := vValue
      else if vLower = 'x-priority' then
      begin
        if Copy(vValue, 1, 1) = '1' then
          FMessage.Priority := mpHigh
        else if Copy(vValue, 1, 1) = '5' then
          FMessage.Priority := mpLow;
      end
      else if not vKnown then
        FMessage.Headers.Add(string(vName) + '=' + string(RALMailDecodeWords(vValue)));
    end;
    ReadPart(vHeaders, vBoundaries);
  finally
    vHeaders.Free;
    vBoundaries.Free;
  end;
end;

procedure TRALMailMessage.LoadFromStream(AStream: TStream);
var
  vReader: TRALMailReader;
begin
  Clear;
  vReader := TRALMailReader.Create(Self, AStream);
  try
    vReader.Run;
  finally
    vReader.Free;
  end;
end;

procedure TRALMailMessage.SaveToFile(const APath: string; AOptions: TRALMailWriteOptions);
var
  vStream: TFileStream;
begin
  vStream := TFileStream.Create(APath, fmCreate);
  try
    SaveToStream(vStream, AOptions);
  finally
    vStream.Free;
  end;
end;

/// Writes a header field folded at commas and spaces near 78 columns.
procedure WriteHeader(AOut: TRALMailOut; const AName, AValue: StringRAL);
var
  vLine, vRest: StringRAL;
  vCut, vInt: IntegerRAL;
begin
  vLine := AName + ': ';
  vRest := AValue;
  while Length(vLine) + Length(vRest) > 78 do
  begin
    // the last space before column 78 (and after the name) folds the line
    vCut := 0;
    for vInt := 78 - Length(vLine) downto 1 do
      if (vInt <= Length(vRest)) and (vRest[vInt] = ' ') then
      begin
        vCut := vInt;
        Break;
      end;
    if vCut <= 1 then
    begin
      // no space early enough: the first one after (or none: the line stays long)
      vCut := RALMailPos(' ', Copy(vRest, 2, MaxInt));
      if vCut = 0 then
        Break;
      Inc(vCut);
    end;
    AOut.WriteLine(vLine + Copy(vRest, 1, vCut - 1));
    vLine := ' ';
    vRest := Copy(vRest, vCut + 1, MaxInt);
  end;
  AOut.WriteLine(vLine + vRest);
end;

/// Writes a text body: 8bit when allowed and the lines are short, else quoted-printable.
procedure WriteTextPart(AOut: TRALMailOut; const AType, AText: StringRAL;
  AOptions: TRALMailWriteOptions);
var
  vInt, vLine, vOut: IntegerRAL;
  vPlain, vLong, vCRLF, v8Bit: Boolean;
  vText: RawByteString;
  vChar: AnsiChar;
begin
  { one pass over the bytes: printable ASCII or not, a line over 998 bytes, line
    ends already CRLF everywhere }
  vPlain := True;
  vLong := False;
  vCRLF := True;
  vLine := 0;
  for vInt := 1 to Length(AText) do
  begin
    vChar := AText[vInt];
    if (vChar = #13) or (vChar = #10) then
    begin
      if (vChar = #13) and ((vInt = Length(AText)) or (AText[vInt + 1] <> #10)) then
        vCRLF := False
      else if (vChar = #10) and ((vInt = 1) or (AText[vInt - 1] <> #13)) then
        vCRLF := False;
      vLine := 0;
    end
    else
    begin
      Inc(vLine);
      if vLine > 998 then
        vLong := True;
      if (Ord(vChar) < 32) or (Ord(vChar) > 126) then
        vPlain := False;
    end;
  end;
  v8Bit := (mwo8Bit in AOptions) and not vLong;
  AOut.WriteLine('Content-Type: ' + AType + '; charset=UTF-8');
  if vPlain and not vLong then
  begin
    AOut.WriteLine('Content-Transfer-Encoding: 7bit');
    v8Bit := True;
  end
  else if v8Bit then
    AOut.WriteLine('Content-Transfer-Encoding: 8bit')
  else
    AOut.WriteLine('Content-Transfer-Encoding: quoted-printable');
  AOut.WriteLine('');
  if not v8Bit then
    AOut.WriteText(RALMailEncodeQP(AText))
  else if vCRLF then
    AOut.WriteText(AText)
  else
  begin
    // CRLF everywhere: a bare CR or LF becomes CRLF
    SetLength(vText, Length(AText) * 2);
    vOut := 0;
    vInt := 1;
    while vInt <= Length(AText) do
    begin
      vChar := AText[vInt];
      if (vChar = #13) or (vChar = #10) then
      begin
        if (vChar = #13) and (vInt < Length(AText)) and (AText[vInt + 1] = #10) then
          Inc(vInt);
        vText[vOut + 1] := #13;
        vText[vOut + 2] := #10;
        Inc(vOut, 2);
      end
      else
      begin
        Inc(vOut);
        vText[vOut] := vChar;
      end;
      Inc(vInt);
    end;
    SetLength(vText, vOut);
    AOut.WriteText(StringRAL(vText));
  end;
  AOut.WriteLine('');
end;

/// Writes a file part in base64, read from its content in blocks.
procedure WriteFilePart(AOut: TRALMailOut; AAttach: TRALMailAttachment);
var
  vType, vName: StringRAL;
  vIn: TStream;
  vBuffer: TBytes;
  vRead, vPos: IntegerRAL;
  vEncoded: StringRAL;
  vChunk: TBytes;
begin
  vName := AAttach.FileName;
  if (vName = '') and (AAttach.Path <> '') then
    vName := StringRAL(ExtractFileName(AAttach.Path));
  vType := AAttach.ContentType;
  if vType = '' then
    vType := TypeOfFile(string(vName));
  if vName <> '' then
    AOut.WriteLine('Content-Type: ' + vType + '; ' + ParamText('name', vName))
  else
    AOut.WriteLine('Content-Type: ' + vType);
  AOut.WriteLine('Content-Transfer-Encoding: base64');
  if AAttach.IsInline then
  begin
    if vName <> '' then
      AOut.WriteLine('Content-Disposition: inline; ' + ParamText('filename', vName))
    else
      AOut.WriteLine('Content-Disposition: inline');
  end
  else
    AOut.WriteLine('Content-Disposition: attachment; ' + ParamText('filename', vName));
  if AAttach.ContentID <> '' then
    AOut.WriteLine('Content-ID: <' + AAttach.ContentID + '>');
  AOut.WriteLine('');
  if AAttach.Data <> nil then
  begin
    vIn := AAttach.Data;
    vIn.Position := 0;
  end
  else
    vIn := AAttach.OpenContent;
  try
    // 57 bytes are one line of 76 base64 characters
    SetLength(vBuffer, 57 * 1024);
    repeat
      vRead := vIn.Read(vBuffer[0], Length(vBuffer));
      vPos := 0;
      while vPos < vRead do
      begin
        if vRead - vPos >= 57 then
          SetLength(vChunk, 57)
        else
          SetLength(vChunk, vRead - vPos);
        Move(vBuffer[vPos], vChunk[0], Length(vChunk));
        vEncoded := TRALBase64.Encode(vChunk);
        AOut.WriteLine(vEncoded);
        Inc(vPos, Length(vChunk));
      end;
    until vRead < Length(vBuffer);
  finally
    if AAttach.Data = nil then
      vIn.Free;
  end;
end;

procedure TRALMailMessage.SaveToStream(AStream: TStream; AOptions: TRALMailWriteOptions);
var
  vOut: TRALMailOut;
  vUTF8: Boolean;
  vMixed, vRelated, vAlt: StringRAL;
  vInt, vAttachments, vInlines: IntegerRAL;
  vDomain: StringRAL;

  procedure WriteBody;
  var
    vI: IntegerRAL;
  begin
    if FHTML = '' then
    begin
      WriteTextPart(vOut, 'text/plain', FText, AOptions);
      Exit;
    end;
    if vInlines > 0 then
    begin
      vOut.WriteLine('Content-Type: multipart/related; boundary="' + vRelated + '"');
      vOut.WriteLine('');
      vOut.WriteLine('--' + vRelated);
    end;
    if FText <> '' then
    begin
      vOut.WriteLine('Content-Type: multipart/alternative; boundary="' + vAlt + '"');
      vOut.WriteLine('');
      vOut.WriteLine('--' + vAlt);
      WriteTextPart(vOut, 'text/plain', FText, AOptions);
      vOut.WriteLine('--' + vAlt);
      WriteTextPart(vOut, 'text/html', FHTML, AOptions);
      vOut.WriteLine('--' + vAlt + '--');
    end
    else
      WriteTextPart(vOut, 'text/html', FHTML, AOptions);
    if vInlines > 0 then
    begin
      for vI := 0 to FAttachments.Count - 1 do
        if FAttachments[vI].IsInline then
        begin
          vOut.WriteLine('--' + vRelated);
          WriteFilePart(vOut, FAttachments[vI]);
        end;
      vOut.WriteLine('--' + vRelated + '--');
    end;
  end;

begin
  vUTF8 := mwoUTF8Headers in AOptions;
  if FDate = 0 then
    FDate := Now;
  if FMessageID = '' then
  begin
    vDomain := 'localhost';
    if (FFrom.Count > 0) and (RALMailPos('@', FFrom[0].Address) > 0) then
      vDomain := Copy(FFrom[0].Address, RALMailPos('@', FFrom[0].Address) + 1, MaxInt);
    FMessageID := RandomHex(16) + '@' + vDomain;
  end;
  vAttachments := 0;
  vInlines := 0;
  for vInt := 0 to FAttachments.Count - 1 do
    if FAttachments[vInt].IsInline and (FHTML <> '') then
      Inc(vInlines)
    else
      Inc(vAttachments);
  vMixed := 'ralmix_' + RandomHex(12);
  vRelated := 'ralrel_' + RandomHex(12);
  vAlt := 'ralalt_' + RandomHex(12);
  vOut := TRALMailOut.Create(AStream);
  try
    if FFrom.Count > 0 then
      WriteHeader(vOut, 'From', FFrom.HeaderText(vUTF8));
    if FSender <> '' then
      WriteHeader(vOut, 'Sender', FSender);
    if FReplyTo.Count > 0 then
      WriteHeader(vOut, 'Reply-To', FReplyTo.HeaderText(vUTF8));
    if FRecipients.Count > 0 then
      WriteHeader(vOut, 'To', FRecipients.HeaderText(vUTF8));
    if FCC.Count > 0 then
      WriteHeader(vOut, 'Cc', FCC.HeaderText(vUTF8));
    if vUTF8 then
      WriteHeader(vOut, 'Subject', FSubject)
    else
      WriteHeader(vOut, 'Subject', RALMailEncodeWord(FSubject));
    WriteHeader(vOut, 'Date', RALMailDate(FDate));
    WriteHeader(vOut, 'Message-ID', '<' + FMessageID + '>');
    if FInReplyTo <> '' then
      WriteHeader(vOut, 'In-Reply-To', '<' + FInReplyTo + '>');
    if FReferences <> '' then
      WriteHeader(vOut, 'References', FReferences);
    if FReadReceiptTo <> '' then
      WriteHeader(vOut, 'Disposition-Notification-To', FReadReceiptTo);
    case FPriority of
      mpHigh:
        begin
          WriteHeader(vOut, 'X-Priority', '1 (Highest)');
          WriteHeader(vOut, 'Importance', 'High');
        end;
      mpLow:
        begin
          WriteHeader(vOut, 'X-Priority', '5 (Lowest)');
          WriteHeader(vOut, 'Importance', 'Low');
        end;
    end;
    for vInt := 0 to FHeaders.Count - 1 do
      if vUTF8 then
        WriteHeader(vOut, StringRAL(FHeaders.Names[vInt]), StringRAL(FHeaders.ValueFromIndex[vInt]))
      else
        WriteHeader(vOut, StringRAL(FHeaders.Names[vInt]),
          RALMailEncodeWord(StringRAL(FHeaders.ValueFromIndex[vInt])));
    vOut.WriteLine('MIME-Version: 1.0');
    if vAttachments > 0 then
    begin
      vOut.WriteLine('Content-Type: multipart/mixed; boundary="' + vMixed + '"');
      vOut.WriteLine('');
      vOut.WriteLine('This is a multi-part message in MIME format.');
      vOut.WriteLine('--' + vMixed);
      WriteBody;
      for vInt := 0 to FAttachments.Count - 1 do
        if not (FAttachments[vInt].IsInline and (FHTML <> '')) then
        begin
          vOut.WriteLine('--' + vMixed);
          WriteFilePart(vOut, FAttachments[vInt]);
        end;
      vOut.WriteLine('--' + vMixed + '--');
    end
    else
      WriteBody;
  finally
    vOut.Free;
  end;
end;

procedure TRALMailMessage.SetAttachments(AValue: TRALMailAttachments);
begin
  FAttachments.Assign(AValue);
end;

procedure TRALMailMessage.SetBCC(AValue: TRALMailAddresses);
begin
  FBCC.Assign(AValue);
end;

procedure TRALMailMessage.SetCC(AValue: TRALMailAddresses);
begin
  FCC.Assign(AValue);
end;

procedure TRALMailMessage.SetFrom(AValue: TRALMailAddresses);
begin
  FFrom.Assign(AValue);
end;

procedure TRALMailMessage.SetHeaders(AValue: TStringList);
begin
  FHeaders.Assign(AValue);
end;

procedure TRALMailMessage.SetRecipients(AValue: TRALMailAddresses);
begin
  FRecipients.Assign(AValue);
end;

procedure TRALMailMessage.SetReplyTo(AValue: TRALMailAddresses);
begin
  FReplyTo.Assign(AValue);
end;

initialization
  Randomize;

end.
