/// XML pull parser of RAL: tokens over UTF-8, encodings, entities, namespaces, limits.
unit RALXMLReader;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts;

const
  /// Namespace the prefix xml is bound to.
  RALXMLNamespaceXML = 'http://www.w3.org/XML/1998/namespace';
  /// Namespace of the xmlns attributes.
  RALXMLNamespaceXMLNS = 'http://www.w3.org/2000/xmlns/';

type
  /// Error of a document not well-formed, or of a limit; Line and Column are 1-based.
  ERALXMLError = class(Exception)
  private
    FColumn: IntegerRAL;
    FLine: IntegerRAL;
  public
    /// Raises AMessage at ALine, AColumn; 0 for both when the place is unknown.
    constructor CreateAt(const AMessage: string; ALine, AColumn: IntegerRAL);

    /// Column of the error, counted in characters.
    property Column: IntegerRAL read FColumn;
    /// Line of the error.
    property Line: IntegerRAL read FLine;
  end;

  /// Kind of the token the reader stands on.
  TRALXMLToken = (xtkNone, xtkStartElement, xtkEndElement, xtkText, xtkCData, xtkComment,
    xtkProcessingInstruction, xtkDocType, xtkEndOfDocument);

  { Event that gives the content of an external entity or DTD; the library never
    opens a file or a URL itself. AHandled False refuses the entity. }
  TRALXMLResolveEntityEvent = procedure(ASender: TObject; const APublicId,
    ASystemId: StringRAL; var AContent: StringRAL; var AHandled: boolean) of object;

  /// Limits and choices of the parser; the defaults are the safe ones.
  TRALXMLReaderOptions = class(TPersistent)
  private
    FMaxAttributes: IntegerRAL;
    FMaxDepth: IntegerRAL;
    FMaxEntityDepth: IntegerRAL;
    FMaxEntityExpansion: IntegerRAL;
    FMaxNameLength: IntegerRAL;
    FNamespaces: boolean;
    FNormalizeAttributeBlanks: boolean;
    FOnResolveEntity: TRALXMLResolveEntityEvent;
    FProcessInternalEntities: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
  published
    /// Most attributes one element may carry.
    property MaxAttributes: IntegerRAL read FMaxAttributes write FMaxAttributes default 256;
    /// Most elements open at once.
    property MaxDepth: IntegerRAL read FMaxDepth write FMaxDepth default 256;
    /// Most entities expanded inside one another.
    property MaxEntityDepth: IntegerRAL read FMaxEntityDepth write FMaxEntityDepth default 8;
    /// Most bytes all entity expansions of a document may produce.
    property MaxEntityExpansion: IntegerRAL read FMaxEntityExpansion
      write FMaxEntityExpansion default 65536;
    /// Longest name, in bytes.
    property MaxNameLength: IntegerRAL read FMaxNameLength write FMaxNameLength default 1024;
    /// Applies Namespaces in XML: prefixes are resolved and checked.
    property Namespaces: boolean read FNamespaces write FNamespaces default True;
    { Turns line breaks and tabs written inside an attribute value into spaces, as
      XML asks; False keeps them, as ADO expects of the files it writes. }
    property NormalizeAttributeBlanks: boolean read FNormalizeAttributeBlanks
      write FNormalizeAttributeBlanks default True;
    /// Gives the content of external entities and DTDs; unassigned, none is read.
    property OnResolveEntity: TRALXMLResolveEntityEvent read FOnResolveEntity
      write FOnResolveEntity;
    { Expands the entities the DOCTYPE declares, as XML 1.0 asks of a parser;
      False makes a reference to one an error. }
    property ProcessInternalEntities: boolean read FProcessInternalEntities
      write FProcessInternalEntities default True;
  end;

  /// List of strings.
  TRALXMLStringArray = array of StringRAL;

  { Interned names: the same name always comes back as the same string, so a
    repeated element name costs no allocation and compares by pointer. }
  TRALXMLNameTable = class
  private
    FCount: IntegerRAL;
    /// Hash of each slot, 0 when the slot is free.
    FHashes: array of Cardinal;
    /// Names, one per slot.
    FItems: TRALXMLStringArray;

    /// Doubles the table.
    procedure Grow;
    /// Hash of the ALen bytes at AText.
    function HashOf(AText: PByte; ALen: IntegerRAL): Cardinal;
  public
    constructor Create;

    /// Returns the interned copy of the ALen bytes at AText.
    function Add(AText: PByte; ALen: IntegerRAL): StringRAL; overload;
    /// Returns the interned copy of AName.
    function Add(const AName: StringRAL): StringRAL; overload;
    /// Returns the interned copy of AName, or '' when it was never added.
    function Find(const AName: StringRAL): StringRAL;

    /// Names in the table.
    property Count: IntegerRAL read FCount;
  end;

  /// An input being read: the document, or the replacement text of an entity.
  TRALXMLFrame = record
    /// Element depth when the input started; an entity must close what it opens.
    Depth: IntegerRAL;
    /// Name of the entity, empty for the document.
    Entity: StringRAL;
    /// The input is an external entity.
    IsExternal: boolean;
    /// Next byte to read.
    Position: PByte;
    /// First byte of the input.
    Start: PByte;
    /// One past the last byte.
    Stop: PByte;
    /// Keeps the bytes of the input alive.
    Text: StringRAL;
  end;

  /// An attribute of the element the reader stands on.
  TRALXMLAttribute = record
    /// Part after the prefix.
    LocalName: StringRAL;
    /// Qualified name, as written.
    Name: StringRAL;
    /// Namespace of the prefix; empty for an attribute with no prefix.
    NamespaceURI: StringRAL;
    /// Prefix, or empty.
    Prefix: StringRAL;
    /// Written in the document; False for a default from the DTD.
    Specified: boolean;
    /// Normalized value, references expanded.
    Value: StringRAL;
  end;

  /// An element still open, and how many namespace bindings it made.
  TRALXMLOpenElement = record
    /// Qualified name, to match the end tag.
    Name: StringRAL;
    /// Namespace bindings it pushed.
    NsCount: IntegerRAL;
  end;

  /// A namespace prefix in scope and its URI.
  TRALXMLNsBinding = record
    /// Prefix; empty for the default namespace.
    Prefix: StringRAL;
    /// URI; empty undeclares the prefix.
    URI: StringRAL;
  end;

  /// A general or parameter entity declared in the DTD.
  TRALXMLEntity = record
    /// Declared in the external subset or in an external parameter entity.
    DeclaredExternally: boolean;
    /// The entity is external (SYSTEM or PUBLIC).
    IsExternal: boolean;
    /// Name of the entity.
    Name: StringRAL;
    /// Notation of an unparsed entity, or empty.
    Notation: StringRAL;
    /// Public identifier of an external entity.
    PublicId: StringRAL;
    /// System identifier of an external entity.
    SystemId: StringRAL;
    /// Replacement text of an internal entity.
    Value: StringRAL;
  end;

  /// An attribute default the DTD declares.
  TRALXMLAttrDecl = record
    /// Attribute name.
    Attribute: StringRAL;
    /// Element name.
    Element: StringRAL;
    /// There is a default value.
    HasDefault: boolean;
    /// Of a type other than CDATA: blanks are collapsed.
    Tokenized: boolean;
    /// Default value, normalized.
    Value: StringRAL;
  end;

  /// Pull parser: Read moves to the next token, and the properties describe it.
  TRALXMLReader = class
  private
    FAttrCount: IntegerRAL;
    /// Attribute defaults declared in the DTD, in use.
    FAttrDeclCount: IntegerRAL;
    /// Attribute defaults declared in the DTD.
    FAttrDecls: array of TRALXMLAttrDecl;
    /// Attributes of the start tag read.
    FAttributes: array of TRALXMLAttribute;
    /// Value being built.
    FBuffer: TBytes;
    /// Bytes in use in FBuffer.
    FBufLen: IntegerRAL;
    FDocTypeName: StringRAL;
    FDocTypePublicId: StringRAL;
    FDocTypeSystemId: StringRAL;
    FElementCount: IntegerRAL;
    /// Elements open, the innermost last.
    FElements: array of TRALXMLOpenElement;
    /// An empty element tag still owes its end token.
    FEmptyPending: boolean;
    /// Entities declared in the DTD.
    FEntities: array of TRALXMLEntity;
    /// Entities in use in FEntities.
    FEntityCount: IntegerRAL;
    /// Bytes produced by entity expansion, against MaxEntityExpansion.
    FExpanded: Int64RAL;
    /// Frames in use in FFrames.
    FFrameCount: IntegerRAL;
    /// Input and entity texts being read, the innermost last.
    FFrames: array of TRALXMLFrame;
    /// The DTD has external parts not read, so undeclared entities are no error.
    FHasExternalParts: boolean;
    /// Document being read, as UTF-8.
    FInput: StringRAL;
    FIsEmptyElement: boolean;
    FIsWhitespace: boolean;
    FLocalName: StringRAL;
    FName: StringRAL;
    FNames: TRALXMLNameTable;
    FNamespaceURI: StringRAL;
    /// Namespace declarations in scope.
    FNsBindings: array of TRALXMLNsBinding;
    /// Declarations in use in FNsBindings.
    FNsCount: IntegerRAL;
    FOptions: TRALXMLReaderOptions;
    /// The reader frees the name table.
    FOwnsNames: boolean;
    /// The reader frees the options.
    FOwnsOptions: boolean;
    FPrefix: StringRAL;
    /// The root element was closed.
    FRootClosed: boolean;
    /// A DOCTYPE was read.
    FSeenDocType: boolean;
    /// The root element was opened.
    FSeenRoot: boolean;
    /// Declarations were skipped after an external parameter entity.
    FSkippedDecls: boolean;
    FStandalone: StringRAL;
    /// Reading began.
    FStarted: boolean;
    FToken: TRALXMLToken;
    FValue: StringRAL;
    /// The document declares XML 1.1.
    FVersion11: boolean;
    FXMLEncoding: StringRAL;
    /// Interned namespace of the xml prefix.
    FXmlNsURI: StringRAL;
    /// Interned namespace of the xmlns prefix.
    FXmlNsxmlnsURI: StringRAL;
    FXMLVersion: StringRAL;

    /// Declares the default AValue of AAttribute in AElement.
    procedure AddAttrDecl(const AElement, AAttribute, AValue: StringRAL;
      AHasDefault, ATokenized: boolean);
    /// Declares AEntity; the first declaration of a name wins.
    procedure AddEntity(const AEntity: TRALXMLEntity; AParameter: boolean);
    /// Adds the declared defaults the start tag did not write.
    procedure ApplyAttrDefaults;
    /// Appends AByte to the value being built.
    procedure BufAdd(AByte: Byte);
    /// Appends ALen bytes at AText to the value being built.
    procedure BufAddBytes(AText: PByte; ALen: IntegerRAL);
    /// Appends the code point ACode, in UTF-8, to the value being built.
    procedure BufAddCodePoint(ACode: Cardinal);
    /// Raises when two attributes of the tag have the same name.
    procedure CheckAttrUnique;
    /// Raises when ACode is not an XML character.
    procedure CheckChar(ACode: Cardinal);
    /// Raises past MaxDepth.
    procedure CheckLimitDepth;
    /// The innermost frame was read to its end.
    function CurrentFrameEnded: boolean;
    /// Decodes the UTF-8 character at P and moves past it.
    function DecodeChar(var P: PByte; AStop: PByte): Cardinal;
    /// Index of the entity AName, general or parameter, or -1.
    function EntityIndex(const AName: StringRAL; AParameter: boolean): IntegerRAL;
    /// Raises ERALXMLError with AMessage at the current position.
    procedure Error(const AMessage: string);
    /// Appends the expansion of the entity AName to an attribute value.
    procedure ExpandAttrEntity(const AName: StringRAL; ADepth: IntegerRAL);
    /// Appends AText to an attribute value, expanding its references.
    procedure ExpandAttrText(const AText: StringRAL; ADepth: IntegerRAL);
    /// Index of the default of AAttribute in AElement, or -1.
    function FindAttrDecl(const AElement, AAttribute: StringRAL): IntegerRAL;
    /// Current position in the innermost frame.
    function Frame: PByte;
    /// Namespace bound to APrefix, '' for an unbound default.
    function LookupNamespace(const APrefix: StringRAL): StringRAL;
    /// Starts reading AText, the expansion of AEntity.
    procedure NewFrame(const AText, AEntity: StringRAL; AExternal: boolean);
    /// AValue with blanks collapsed and trimmed (non-CDATA attributes).
    function NormalizeTokens(const AValue: StringRAL): StringRAL;
    /// Parses an ATTLIST declaration.
    procedure ParseAttListDecl(var P: PByte; AStop: PByte);
    /// Parses the attributes of a start tag.
    procedure ParseAttributes;
    /// Parses a CDATA section.
    procedure ParseCData;
    /// Parses a character reference and appends its character.
    procedure ParseCharRef(var P: PByte; AStop: PByte);
    /// Parses a comment.
    procedure ParseComment;
    /// Parses the declarations of the internal subset, or of an external one.
    procedure ParseDeclarations(AExternal: boolean);
    /// Parses the DOCTYPE and its internal subset.
    procedure ParseDocType;
    /// Parses an end tag.
    procedure ParseEndTag;
    /// Parses an ENTITY declaration.
    procedure ParseEntityDecl(var P: PByte; AStop: PByte; AExternal: boolean);
    /// Parses a processing instruction.
    procedure ParsePI;
    /// Parses a start tag.
    procedure ParseStartTag;
    /// Parses character data; False when there was none.
    function ParseText: boolean;
    /// Parses the XML declaration, or a text declaration when AText.
    procedure ParseXMLDecl(AText: boolean);
    /// Leaves the innermost frame.
    procedure PopFrame;
    /// Reads a quoted attribute value, normalized and expanded.
    procedure ReadAttrValue(var P: PByte; AStop: PByte; ADepth: IntegerRAL);
    /// Reads a quoted entity value, with its parameter references expanded.
    function ReadEntityValue(var P: PByte; AStop: PByte; AExternal: boolean): StringRAL;
    /// Reads a name, or an Nmtoken when AToken.
    function ReadName(var P: PByte; AStop: PByte; AToken: boolean = False): StringRAL;
    /// Reads a quoted literal.
    function ReadQuoted(var P: PByte; AStop: PByte): StringRAL;
    /// Resolves the namespaces of the element and of its attributes.
    procedure ResolveElementNamespaces;
    { The text of an external entity from OnResolveEntity, or '' when it is not
      resolved. }
    function ResolveExternal(const APublicId, ASystemId: StringRAL;
      out AContent: StringRAL): boolean;
    /// Moves the position of the innermost frame to P.
    procedure SetFramePos(P: PByte);
    /// Skips blanks; True when there was any.
    function SkipSpaces(var P: PByte; AStop: PByte): boolean;
    /// Splits AName at its colon into APrefix and ALocal.
    procedure SplitName(const AName: StringRAL; out APrefix, ALocal: StringRAL);
    /// The bytes at P start with AText.
    function StartsWith(P, AStop: PByte; const AText: StringRAL): boolean;
    /// Raises when AName is not a valid qualified name.
    procedure ValidateQName(const AName: StringRAL);
  public
    { Reader over ANames and AOptions when given; nil for both creates its own. Neither
      is freed by the reader when given. }
    constructor Create(ANames: TRALXMLNameTable = nil; AOptions: TRALXMLReaderOptions = nil);
    destructor Destroy; override;

    /// Returns the value of the attribute named AName, or '' when absent.
    function AttributeValue(const AName: StringRAL): StringRAL;
    /// Column of the current position in the document.
    function CurrentColumn: IntegerRAL;
    /// Line of the current position in the document.
    function CurrentLine: IntegerRAL;
    /// Returns True when the element has the attribute AName.
    function HasAttribute(const AName: StringRAL): boolean;
    /// Moves to the next token; False at the end of the document.
    function Read: boolean;
    { Reads the text of the element just started, through its end tag, and
      returns it; child elements are an error. }
    function ReadElementText: StringRAL;
    { Uses the document AData, of ASize bytes; ACharset is the charset parameter of
      the media type, if any. The bytes must outlive the reader. }
    procedure SetInput(AData: Pointer; ASize: IntegerRAL; const ACharset: StringRAL = '');
    /// Uses the bytes of AStream from its start; ACharset as in SetInput.
    procedure SetInputStream(AStream: TStream; const ACharset: StringRAL = '');
    /// Uses AXML; the reader keeps a reference to it.
    procedure SetInputString(const AXML: StringRAL; const ACharset: StringRAL = '');
    /// Skips the element just started, through its end tag.
    procedure Skip;

    /// Number of attributes of the current element.
    property AttributeCount: IntegerRAL read FAttrCount;
    /// Elements open, the current one included.
    property Depth: IntegerRAL read FElementCount;
    /// Name of the DOCTYPE, empty without one.
    property DocTypeName: StringRAL read FDocTypeName;
    /// Public identifier of the DOCTYPE.
    property DocTypePublicId: StringRAL read FDocTypePublicId;
    /// System identifier of the DOCTYPE.
    property DocTypeSystemId: StringRAL read FDocTypeSystemId;
    /// The element was written as an empty tag; its end comes as the next token.
    property IsEmptyElement: boolean read FIsEmptyElement;
    /// The text holds only blanks.
    property IsWhitespace: boolean read FIsWhitespace;
    /// Part of the name after the prefix.
    property LocalName: StringRAL read FLocalName;
    /// Qualified name of an element, target of a processing instruction.
    property Name: StringRAL read FName;
    /// Namespace of the current element.
    property NamespaceURI: StringRAL read FNamespaceURI;
    /// Name table the reader interns into.
    property NameTable: TRALXMLNameTable read FNames;
    /// Options in use.
    property Options: TRALXMLReaderOptions read FOptions;
    /// Prefix of the current element.
    property Prefix: StringRAL read FPrefix;
    /// Standalone of the XML declaration: yes, no or empty.
    property Standalone: StringRAL read FStandalone;
    /// Kind of the current token.
    property Token: TRALXMLToken read FToken;
    /// Text, CDATA, comment, data of a processing instruction, or internal subset.
    property Value: StringRAL read FValue;
    /// Encoding of the XML declaration, as written.
    property XMLEncoding: StringRAL read FXMLEncoding;
    /// Version of the XML declaration; '1.0' without one.
    property XMLVersion: StringRAL read FXMLVersion;
  end;

/// Returns the attribute AIndex of AReader.
function RALXMLReaderAttribute(AReader: TRALXMLReader; AIndex: IntegerRAL): TRALXMLAttribute;
/// Converts AData, of ASize bytes in AEncoding, to UTF-8; raises for an unknown encoding.
function RALXMLToUTF8(AData: PByte; ASize: IntegerRAL; const AEncoding: StringRAL): StringRAL;
{ Returns the encoding AData declares or starts with (BOM); ACharset wins over the
  declaration. }
function RALXMLDetectEncoding(AData: PByte; ASize: IntegerRAL; const ACharset: StringRAL;
  out ABOMSize: IntegerRAL): StringRAL;
/// True when ACode is a character XML 1.0 allows in a document.
function RALXMLIsChar(ACode: Cardinal): boolean;
/// True when ACode may start an XML name.
function RALXMLIsNameStartChar(ACode: Cardinal): boolean;
/// True when ACode may follow the first character of an XML name.
function RALXMLIsNameChar(ACode: Cardinal): boolean;
/// True when AName is an XML name; ANCName also refuses a colon.
function RALXMLIsName(const AName: StringRAL; ANCName: boolean = False): boolean;
/// Returns the ALen bytes at AText as a string.
function RALXMLMakeString(AText: PByte; ALen: IntegerRAL): StringRAL;
{ True when AType is an XML media type: application/xml, text/xml or any +xml
  (RFC 7303, 6839). }
function RALIsXMLMediaType(const AType: StringRAL): boolean;
/// Returns the charset parameter of the media type AType, or ''.
function RALMediaTypeCharset(const AType: StringRAL): StringRAL;

implementation

const
  /// Byte of '<'.
  cLT = Ord('<');
  /// Byte of '>'.
  cGT = Ord('>');
  /// Byte of '&'.
  cAmp = Ord('&');
  /// Byte of the double quote.
  cQuot = Ord('"');
  /// Byte of the single quote.
  cApos = Ord('''');
  /// Byte of '/'.
  cSlash = Ord('/');
  /// Byte of '?'.
  cQuest = Ord('?');
  /// Byte of '!'.
  cExcl = Ord('!');
  /// Byte of '='.
  cEq = Ord('=');
  /// Byte of ';'.
  cSemi = Ord(';');
  /// Byte of '#'.
  cHash = Ord('#');
  /// Byte of '%'.
  cPct = Ord('%');
  /// Byte of '['.
  cLBrack = Ord('[');
  /// Byte of ']'.
  cRBrack = Ord(']');
  /// Byte of '-'.
  cDash = Ord('-');
  /// Byte of ':'.
  cColon = Ord(':');
  /// Byte of the blank.
  cSpace = 32;
  /// Byte of the tab.
  cTab = 9;
  /// Byte of the line feed.
  cLF = 10;
  /// Byte of the carriage return.
  cCR = 13;

  /// Windows-1252 bytes 128..159; the rest maps to the same code point
  cWin1252: array[128..159] of Word = ($20AC, $FFFD, $201A, $0192, $201E, $2026,
    $2020, $2021, $02C6, $2030, $0160, $2039, $0152, $FFFD, $017D, $FFFD, $FFFD,
    $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $0161, $203A,
    $0153, $FFFD, $017E, $0178);

function RALXMLMakeString(AText: PByte; ALen: IntegerRAL): StringRAL;
begin
  Result := '';
  if ALen <= 0 then
    Exit;
  SetLength(Result, ALen);
  Move(AText^, Pointer(Result)^, ALen);
end;

/// AText is not empty and holds only digits.
function AllDigits(const AText: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  Result := AText <> '';
  for vInt := 1 to Length(AText) do
    if (AText[vInt] < '0') or (AText[vInt] > '9') then
    begin
      Result := False;
      Exit;
    end;
end;

/// AByte is an XML blank: space, tab, CR or LF.
function IsBlank(AByte: Byte): boolean; inline;
begin
  Result := (AByte = cSpace) or (AByte = cTab) or (AByte = cLF) or (AByte = cCR);
end;

/// AText with ASCII letters in lower case.
function LowerAscii(const AText: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
  vPtr: PByte;
begin
  Result := RALXMLMakeString(PByte(Pointer(AText)), Length(AText));
  if Result = '' then
    Exit;
  vPtr := PByte(Pointer(Result));
  for vInt := 0 to Length(Result) - 1 do
  begin
    if (vPtr^ >= Ord('A')) and (vPtr^ <= Ord('Z')) then
      vPtr^ := vPtr^ + 32;
    Inc(vPtr);
  end;
end;

/// AText without the XML blanks around it.
function TrimAscii(const AText: StringRAL): StringRAL;
var
  vStart, vStop: IntegerRAL;
begin
  vStart := 1;
  vStop := Length(AText);
  while (vStart <= vStop) and IsBlank(Byte(AText[vStart])) do
    Inc(vStart);
  while (vStop >= vStart) and IsBlank(Byte(AText[vStop])) do
    Dec(vStop);
  Result := Copy(AText, vStart, vStop - vStart + 1);
end;

function RALXMLIsChar(ACode: Cardinal): boolean;
begin
  case ACode of
    $9, $A, $D, $20..$D7FF, $E000..$FFFD, $10000..$10FFFF:
      Result := True;
  else
    Result := False;
  end;
end;

function RALXMLIsNameStartChar(ACode: Cardinal): boolean;
begin
  case ACode of
    Ord(':'), Ord('A')..Ord('Z'), Ord('_'), Ord('a')..Ord('z'), $C0..$D6, $D8..$F6,
    $F8..$2FF, $370..$37D, $37F..$1FFF, $200C..$200D, $2070..$218F, $2C00..$2FEF,
    $3001..$D7FF, $F900..$FDCF, $FDF0..$FFFD, $10000..$EFFFF:
      Result := True;
  else
    Result := False;
  end;
end;

function RALXMLIsNameChar(ACode: Cardinal): boolean;
begin
  case ACode of
    Ord('-'), Ord('.'), Ord('0')..Ord('9'), $B7, $300..$36F, $203F..$2040:
      Result := True;
  else
    Result := RALXMLIsNameStartChar(ACode);
  end;
end;

/// Decodes the UTF-8 sequence at P (moved past it); $FFFFFFFF when malformed
function Utf8Next(var P: PByte; AStop: PByte): Cardinal;
var
  vByte: Byte;
  vNeed, vInt: IntegerRAL;
  vMin: Cardinal;
begin
  vByte := P^;
  Inc(P);
  if vByte < $80 then
  begin
    Result := vByte;
    Exit;
  end;

  if (vByte and $E0) = $C0 then
  begin
    Result := vByte and $1F;
    vNeed := 1;
    vMin := $80;
  end
  else if (vByte and $F0) = $E0 then
  begin
    Result := vByte and $0F;
    vNeed := 2;
    vMin := $800;
  end
  else if (vByte and $F8) = $F0 then
  begin
    Result := vByte and $07;
    vNeed := 3;
    vMin := $10000;
  end
  else
  begin
    Result := $FFFFFFFF;
    Exit;
  end;

  for vInt := 1 to vNeed do
  begin
    if (NativeUInt(P) >= NativeUInt(AStop)) or ((P^ and $C0) <> $80) then
    begin
      Result := $FFFFFFFF;
      Exit;
    end;
    Result := (Result shl 6) or (P^ and $3F);
    Inc(P);
  end;

  // overlong forms, surrogates and code points past the last plane are not UTF-8
  if (Result < vMin) or ((Result >= $D800) and (Result <= $DFFF)) or (Result > $10FFFF) then
    Result := $FFFFFFFF;
end;

function RALXMLIsName(const AName: StringRAL; ANCName: boolean): boolean;
var
  vPtr, vStop: PByte;
  vCode: Cardinal;
  vFirst: boolean;
begin
  Result := False;
  if AName = '' then
    Exit;

  vPtr := PByte(Pointer(AName));
  vStop := vPtr + Length(AName);
  vFirst := True;
  while NativeUInt(vPtr) < NativeUInt(vStop) do
  begin
    vCode := Utf8Next(vPtr, vStop);
    if vCode = $FFFFFFFF then
      Exit;
    if ANCName and (vCode = Ord(':')) then
      Exit;
    if vFirst then
    begin
      if not RALXMLIsNameStartChar(vCode) then
        Exit;
      vFirst := False;
    end
    else if not RALXMLIsNameChar(vCode) then
      Exit;
  end;
  Result := True;
end;

function RALMediaTypeCharset(const AType: StringRAL): StringRAL;
var
  vLower: StringRAL;
  vPos, vEnd: IntegerRAL;
begin
  Result := '';
  vLower := LowerAscii(AType);
  vPos := Pos(StringRAL('charset='), vLower);
  if vPos = 0 then
    Exit;

  vPos := vPos + 8;
  vEnd := vPos;
  while (vEnd <= Length(AType)) and (AType[vEnd] <> ';') do
    Inc(vEnd);
  Result := TrimAscii(Copy(AType, vPos, vEnd - vPos));
  if (Length(Result) >= 2) and (Result[1] = '"') and (Result[Length(Result)] = '"') then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

function RALIsXMLMediaType(const AType: StringRAL): boolean;
var
  vLower: StringRAL;
  vPos: IntegerRAL;
begin
  vLower := LowerAscii(AType);
  vPos := Pos(StringRAL(';'), vLower);
  if vPos > 0 then
    vLower := Copy(vLower, 1, vPos - 1);
  vLower := TrimAscii(vLower);

  Result := (vLower = 'application/xml') or (vLower = 'text/xml') or
    (vLower = 'application/xml-external-parsed-entity') or
    (vLower = 'text/xml-external-parsed-entity') or (vLower = 'application/xml-dtd') or
    ((Length(vLower) > 4) and (Copy(vLower, Length(vLower) - 3, 4) = '+xml'));
end;

/// Reads the encoding pseudo-attribute of an XML or text declaration in ASCII bytes
function DeclaredEncoding(AData: PByte; ASize: IntegerRAL): StringRAL;
var
  vText, vLower: StringRAL;
  vPos, vEnd, vLen: IntegerRAL;
  vQuote: AnsiChar;
begin
  Result := '';
  vLen := ASize;
  if vLen > 200 then
    vLen := 200;
  vText := RALXMLMakeString(AData, vLen);
  if Copy(vText, 1, 5) <> '<?xml' then
    Exit;

  vEnd := Pos(StringRAL('?>'), vText);
  if vEnd = 0 then
    Exit;

  vText := Copy(vText, 1, vEnd);
  vLower := LowerAscii(vText);
  vPos := Pos(StringRAL('encoding'), vLower);
  if vPos = 0 then
    Exit;

  vPos := vPos + 8;
  while (vPos <= Length(vText)) and (IsBlank(Byte(vText[vPos])) or (vText[vPos] = '=')) do
    Inc(vPos);
  if vPos > Length(vText) then
    Exit;

  vQuote := AnsiChar(vText[vPos]);
  if (vQuote <> '"') and (vQuote <> '''') then
    Exit;
  Inc(vPos);
  vEnd := vPos;
  while (vEnd <= Length(vText)) and (AnsiChar(vText[vEnd]) <> vQuote) do
    Inc(vEnd);
  Result := Copy(vText, vPos, vEnd - vPos);
end;

function RALXMLDetectEncoding(AData: PByte; ASize: IntegerRAL; const ACharset: StringRAL;
  out ABOMSize: IntegerRAL): StringRAL;
begin
  ABOMSize := 0;
  // RFC 7303 section 8: the BOM, then the charset parameter, then the declaration
  if (ASize >= 3) and (AData^ = $EF) and ((AData + 1)^ = $BB) and ((AData + 2)^ = $BF) then
  begin
    ABOMSize := 3;
    Result := 'utf-8';
    Exit;
  end;

  if (ASize >= 2) and (AData^ = $FE) and ((AData + 1)^ = $FF) then
  begin
    ABOMSize := 2;
    Result := 'utf-16be';
    Exit;
  end;

  if (ASize >= 2) and (AData^ = $FF) and ((AData + 1)^ = $FE) then
  begin
    ABOMSize := 2;
    Result := 'utf-16le';
    Exit;
  end;

  if ACharset <> '' then
  begin
    Result := LowerAscii(ACharset);
    Exit;
  end;

  // UTF-16 without a BOM, recognized by the first characters of "<?xml"
  if (ASize >= 4) and (AData^ = 0) and ((AData + 1)^ = $3C) and ((AData + 2)^ = 0) and
     ((AData + 3)^ = $3F) then
  begin
    Result := 'utf-16be';
    Exit;
  end;

  if (ASize >= 4) and (AData^ = $3C) and ((AData + 1)^ = 0) and ((AData + 2)^ = $3F) and
     ((AData + 3)^ = 0) then
  begin
    Result := 'utf-16le';
    Exit;
  end;

  Result := LowerAscii(DeclaredEncoding(AData, ASize));
  if Result = '' then
    Result := 'utf-8';
end;

/// Appends ACode in UTF-8 to AResult, whose length in use is ALen.
procedure AppendUtf8(var AResult: StringRAL; var ALen: IntegerRAL; ACode: Cardinal);
begin
  if ALen + 4 > Length(AResult) then
    SetLength(AResult, Length(AResult) * 2 + 16);

  if ACode < $80 then
  begin
    PByte(Pointer(AResult))[ALen] := ACode;
    Inc(ALen);
  end
  else if ACode < $800 then
  begin
    PByte(Pointer(AResult))[ALen] := $C0 or (ACode shr 6);
    PByte(Pointer(AResult))[ALen + 1] := $80 or (ACode and $3F);
    Inc(ALen, 2);
  end
  else if ACode < $10000 then
  begin
    PByte(Pointer(AResult))[ALen] := $E0 or (ACode shr 12);
    PByte(Pointer(AResult))[ALen + 1] := $80 or ((ACode shr 6) and $3F);
    PByte(Pointer(AResult))[ALen + 2] := $80 or (ACode and $3F);
    Inc(ALen, 3);
  end
  else
  begin
    PByte(Pointer(AResult))[ALen] := $F0 or (ACode shr 18);
    PByte(Pointer(AResult))[ALen + 1] := $80 or ((ACode shr 12) and $3F);
    PByte(Pointer(AResult))[ALen + 2] := $80 or ((ACode shr 6) and $3F);
    PByte(Pointer(AResult))[ALen + 3] := $80 or (ACode and $3F);
    Inc(ALen, 4);
  end;
end;

function RALXMLToUTF8(AData: PByte; ASize: IntegerRAL; const AEncoding: StringRAL): StringRAL;
var
  vEnc: StringRAL;
  vLen, vInt: IntegerRAL;
  vUnit, vLow: Cardinal;
  vBig: boolean;
begin
  Result := '';
  vEnc := LowerAscii(AEncoding);
  if (vEnc = 'utf-8') or (vEnc = 'utf8') then
  begin
    Result := RALXMLMakeString(AData, ASize);
    Exit;
  end;

  SetLength(Result, ASize + 16);
  vLen := 0;

  if (vEnc = 'utf-16') or (vEnc = 'utf-16le') or (vEnc = 'utf-16be') or
     (vEnc = 'ucs-2') or (vEnc = 'iso-10646-ucs-2') then
  begin
    vBig := vEnc = 'utf-16be';
    if Odd(ASize) then
      raise ERALXMLError.CreateAt(emXMLEncodingData, 0, 0);
    vInt := 0;
    while vInt < ASize do
    begin
      if vBig then
        vUnit := (Cardinal(AData[vInt]) shl 8) or AData[vInt + 1]
      else
        vUnit := (Cardinal(AData[vInt + 1]) shl 8) or AData[vInt];
      Inc(vInt, 2);

      if (vUnit >= $D800) and (vUnit <= $DBFF) then
      begin
        if vInt + 1 >= ASize then
          raise ERALXMLError.CreateAt(emXMLEncodingData, 0, 0);
        if vBig then
          vLow := (Cardinal(AData[vInt]) shl 8) or AData[vInt + 1]
        else
          vLow := (Cardinal(AData[vInt + 1]) shl 8) or AData[vInt];
        if (vLow < $DC00) or (vLow > $DFFF) then
          raise ERALXMLError.CreateAt(emXMLEncodingData, 0, 0);
        Inc(vInt, 2);
        vUnit := $10000 + ((vUnit - $D800) shl 10) + (vLow - $DC00);
      end
      else if (vUnit >= $DC00) and (vUnit <= $DFFF) then
        raise ERALXMLError.CreateAt(emXMLEncodingData, 0, 0);

      AppendUtf8(Result, vLen, vUnit);
    end;
  end
  else if (vEnc = 'iso-8859-1') or (vEnc = 'latin1') or (vEnc = 'iso_8859-1') or
          (vEnc = 'l1') then
  begin
    for vInt := 0 to ASize - 1 do
      AppendUtf8(Result, vLen, AData[vInt]);
  end
  else if (vEnc = 'us-ascii') or (vEnc = 'ascii') then
  begin
    for vInt := 0 to ASize - 1 do
    begin
      if AData[vInt] > 127 then
        raise ERALXMLError.CreateAt(emXMLEncodingData, 0, 0);
      AppendUtf8(Result, vLen, AData[vInt]);
    end;
  end
  else if (vEnc = 'windows-1252') or (vEnc = 'cp1252') then
  begin
    for vInt := 0 to ASize - 1 do
    begin
      if (AData[vInt] >= 128) and (AData[vInt] <= 159) then
        AppendUtf8(Result, vLen, cWin1252[AData[vInt]])
      else
        AppendUtf8(Result, vLen, AData[vInt]);
    end;
  end
  else
    raise ERALXMLError.CreateAt(Format(emXMLEncodingUnknown, [AEncoding]), 0, 0);

  SetLength(Result, vLen);
end;

function RALXMLReaderAttribute(AReader: TRALXMLReader; AIndex: IntegerRAL): TRALXMLAttribute;
begin
  Result := AReader.FAttributes[AIndex];
end;

{ ERALXMLError }

constructor ERALXMLError.CreateAt(const AMessage: string; ALine, AColumn: IntegerRAL);
begin
  if ALine > 0 then
    inherited Create(Format(emXMLAtPosition, [AMessage, ALine, AColumn]))
  else
    inherited Create(AMessage);
  FLine := ALine;
  FColumn := AColumn;
end;

{ TRALXMLReaderOptions }

procedure TRALXMLReaderOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALXMLReaderOptions then
  begin
    TRALXMLReaderOptions(Dest).FMaxAttributes := FMaxAttributes;
    TRALXMLReaderOptions(Dest).FMaxDepth := FMaxDepth;
    TRALXMLReaderOptions(Dest).FMaxEntityDepth := FMaxEntityDepth;
    TRALXMLReaderOptions(Dest).FMaxEntityExpansion := FMaxEntityExpansion;
    TRALXMLReaderOptions(Dest).FMaxNameLength := FMaxNameLength;
    TRALXMLReaderOptions(Dest).FNamespaces := FNamespaces;
    TRALXMLReaderOptions(Dest).FNormalizeAttributeBlanks := FNormalizeAttributeBlanks;
    TRALXMLReaderOptions(Dest).FProcessInternalEntities := FProcessInternalEntities;
    TRALXMLReaderOptions(Dest).FOnResolveEntity := FOnResolveEntity;
  end
  else
    inherited AssignTo(Dest);
end;

constructor TRALXMLReaderOptions.Create;
begin
  inherited Create;
  FMaxAttributes := 256;
  FMaxDepth := 256;
  FMaxEntityDepth := 8;
  FMaxEntityExpansion := 65536;
  FMaxNameLength := 1024;
  FNamespaces := True;
  FNormalizeAttributeBlanks := True;
  FProcessInternalEntities := True;
end;

{ TRALXMLNameTable }

constructor TRALXMLNameTable.Create;
begin
  inherited Create;
  SetLength(FItems, 64);
  SetLength(FHashes, 64);
end;

function TRALXMLNameTable.HashOf(AText: PByte; ALen: IntegerRAL): Cardinal;
var
  vInt: IntegerRAL;
begin
  // FNV-1a; {$Q-} keeps the multiplication modular under overflow checks
  {$IFOPT Q+}{$DEFINE RALXML_Q}{$Q-}{$ENDIF}
  {$IFOPT R+}{$DEFINE RALXML_R}{$R-}{$ENDIF}
  Result := 2166136261;
  for vInt := 0 to ALen - 1 do
    Result := (Result xor AText[vInt]) * 16777619;
  if Result = 0 then
    Result := 1;
  {$IFDEF RALXML_Q}{$Q+}{$UNDEF RALXML_Q}{$ENDIF}
  {$IFDEF RALXML_R}{$R+}{$UNDEF RALXML_R}{$ENDIF}
end;

procedure TRALXMLNameTable.Grow;
var
  vOldItems: TRALXMLStringArray;
  vOldHashes: array of Cardinal;
  vInt, vSlot, vMask: IntegerRAL;
begin
  vOldItems := FItems;
  vOldHashes := nil;
  SetLength(vOldHashes, Length(FHashes));
  if Length(FHashes) > 0 then
    Move(FHashes[0], vOldHashes[0], Length(FHashes) * SizeOf(Cardinal));
  FItems := nil;
  FHashes := nil;
  SetLength(FItems, Length(vOldItems) * 2);
  SetLength(FHashes, Length(vOldItems) * 2);
  vMask := Length(FItems) - 1;
  for vInt := 0 to High(vOldItems) do
  begin
    if vOldHashes[vInt] = 0 then
      Continue;
    vSlot := vOldHashes[vInt] and Cardinal(vMask);
    while FHashes[vSlot] <> 0 do
      vSlot := (vSlot + 1) and vMask;
    FHashes[vSlot] := vOldHashes[vInt];
    FItems[vSlot] := vOldItems[vInt];
  end;
end;

function TRALXMLNameTable.Add(AText: PByte; ALen: IntegerRAL): StringRAL;
var
  vHash: Cardinal;
  vSlot, vMask: IntegerRAL;
begin
  vHash := HashOf(AText, ALen);
  vMask := Length(FItems) - 1;
  vSlot := vHash and Cardinal(vMask);
  while FHashes[vSlot] <> 0 do
  begin
    if (FHashes[vSlot] = vHash) and (Length(FItems[vSlot]) = ALen) and
       ((ALen = 0) or CompareMem(Pointer(FItems[vSlot]), AText, ALen)) then
    begin
      Result := FItems[vSlot];
      Exit;
    end;
    vSlot := (vSlot + 1) and vMask;
  end;

  Result := RALXMLMakeString(AText, ALen);
  FHashes[vSlot] := vHash;
  FItems[vSlot] := Result;
  Inc(FCount);
  if FCount * 2 > Length(FItems) then
    Grow;
end;

function TRALXMLNameTable.Add(const AName: StringRAL): StringRAL;
begin
  Result := Add(PByte(Pointer(AName)), Length(AName));
end;

function TRALXMLNameTable.Find(const AName: StringRAL): StringRAL;
var
  vHash: Cardinal;
  vSlot, vMask: IntegerRAL;
begin
  Result := '';
  vHash := HashOf(PByte(Pointer(AName)), Length(AName));
  vMask := Length(FItems) - 1;
  vSlot := vHash and Cardinal(vMask);
  while FHashes[vSlot] <> 0 do
  begin
    if (FHashes[vSlot] = vHash) and (FItems[vSlot] = AName) then
    begin
      Result := FItems[vSlot];
      Exit;
    end;
    vSlot := (vSlot + 1) and vMask;
  end;
end;

{ TRALXMLReader }

constructor TRALXMLReader.Create(ANames: TRALXMLNameTable; AOptions: TRALXMLReaderOptions);
begin
  inherited Create;
  FNames := ANames;
  FOwnsNames := FNames = nil;
  if FOwnsNames then
    FNames := TRALXMLNameTable.Create;

  FOptions := AOptions;
  FOwnsOptions := FOptions = nil;
  if FOwnsOptions then
    FOptions := TRALXMLReaderOptions.Create;

  FXmlNsURI := FNames.Add(RALXMLNamespaceXML);
  FXmlNsxmlnsURI := FNames.Add(RALXMLNamespaceXMLNS);
  SetLength(FBuffer, 256);
  SetLength(FAttributes, 8);
  SetLength(FElements, 16);
  SetLength(FNsBindings, 8);
  SetLength(FFrames, 4);
  FXMLVersion := '1.0';
end;

destructor TRALXMLReader.Destroy;
begin
  if FOwnsNames then
    FreeAndNil(FNames);
  if FOwnsOptions then
    FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TRALXMLReader.SetInputString(const AXML: StringRAL; const ACharset: StringRAL);
begin
  // the reader keeps a reference, so the bytes live as long as it needs them
  FInput := AXML;
  SetInput(Pointer(FInput), Length(FInput), ACharset);
end;

procedure TRALXMLReader.SetInputStream(AStream: TStream; const ACharset: StringRAL);
var
  vSize: Int64RAL;
begin
  vSize := AStream.Size;
  if (AStream is TCustomMemoryStream) and (vSize < MaxInt) then
  begin
    SetInput(TCustomMemoryStream(AStream).Memory, vSize, ACharset);
    Exit;
  end;

  FInput := '';
  SetLength(FInput, vSize);
  AStream.Position := 0;
  if vSize > 0 then
    AStream.ReadBuffer(Pointer(FInput)^, vSize);
  SetInput(Pointer(FInput), Length(FInput), ACharset);
end;

procedure TRALXMLReader.SetInput(AData: Pointer; ASize: IntegerRAL; const ACharset: StringRAL);
var
  vEnc: StringRAL;
  vBOM: IntegerRAL;
  vStart: PByte;
begin
  FFrameCount := 0;
  FElementCount := 0;
  FNsCount := 0;
  FAttrCount := 0;
  FEntityCount := 0;
  FAttrDeclCount := 0;
  FExpanded := 0;
  FSeenRoot := False;
  FRootClosed := False;
  FSeenDocType := False;
  FStarted := False;
  FEmptyPending := False;
  FHasExternalParts := False;
  FSkippedDecls := False;
  FToken := xtkNone;
  FXMLVersion := '1.0';
  FXMLEncoding := '';
  FStandalone := '';
  FVersion11 := False;
  FDocTypeName := '';
  FDocTypePublicId := '';
  FDocTypeSystemId := '';

  vStart := AData;
  if ASize < 0 then
    ASize := 0;

  vEnc := RALXMLDetectEncoding(vStart, ASize, ACharset, vBOM);
  if (vEnc <> 'utf-8') and (vEnc <> 'utf8') then
  begin
    FInput := RALXMLToUTF8(vStart + vBOM, ASize - vBOM, vEnc);
    vStart := PByte(Pointer(FInput));
    ASize := Length(FInput);
    vBOM := 0;
  end;

  NewFrame('', '', False);
  FFrames[0].Start := vStart + vBOM;
  FFrames[0].Position := vStart + vBOM;
  FFrames[0].Stop := vStart + ASize;
end;

procedure TRALXMLReader.NewFrame(const AText, AEntity: StringRAL; AExternal: boolean);
begin
  if FFrameCount >= Length(FFrames) then
    SetLength(FFrames, FFrameCount * 2);

  FFrames[FFrameCount].Text := AText;
  FFrames[FFrameCount].Entity := AEntity;
  FFrames[FFrameCount].IsExternal := AExternal;
  FFrames[FFrameCount].Depth := FElementCount;
  FFrames[FFrameCount].Start := PByte(Pointer(FFrames[FFrameCount].Text));
  FFrames[FFrameCount].Position := FFrames[FFrameCount].Start;
  FFrames[FFrameCount].Stop := FFrames[FFrameCount].Start + Length(AText);
  Inc(FFrameCount);
end;

procedure TRALXMLReader.PopFrame;
begin
  if FFrames[FFrameCount - 1].Depth <> FElementCount then
    Error(Format(emXMLEntityElements, [FFrames[FFrameCount - 1].Entity]));
  FFrames[FFrameCount - 1].Text := '';
  Dec(FFrameCount);
end;

function TRALXMLReader.Frame: PByte;
begin
  Result := FFrames[FFrameCount - 1].Position;
end;

procedure TRALXMLReader.SetFramePos(P: PByte);
begin
  FFrames[FFrameCount - 1].Position := P;
end;

function TRALXMLReader.CurrentFrameEnded: boolean;
begin
  Result := NativeUInt(FFrames[FFrameCount - 1].Position) >=
    NativeUInt(FFrames[FFrameCount - 1].Stop);
end;

function TRALXMLReader.CurrentLine: IntegerRAL;
var
  vPtr, vStop: PByte;
begin
  Result := 1;
  if FFrameCount = 0 then
    Exit;
  vPtr := FFrames[0].Start;
  vStop := FFrames[0].Position;
  while NativeUInt(vPtr) < NativeUInt(vStop) do
  begin
    if vPtr^ = cLF then
      Inc(Result);
    Inc(vPtr);
  end;
end;

function TRALXMLReader.CurrentColumn: IntegerRAL;
var
  vPtr, vStop: PByte;
begin
  Result := 1;
  if FFrameCount = 0 then
    Exit;
  vPtr := FFrames[0].Start;
  vStop := FFrames[0].Position;
  while NativeUInt(vPtr) < NativeUInt(vStop) do
  begin
    if vPtr^ = cLF then
      Result := 1
    else if (vPtr^ and $C0) <> $80 then
      Inc(Result);
    Inc(vPtr);
  end;
end;

procedure TRALXMLReader.Error(const AMessage: string);
var
  vMsg: string;
begin
  vMsg := AMessage;
  if (FFrameCount > 1) and (FFrames[FFrameCount - 1].Entity <> '') then
    vMsg := Format(emXMLInEntity, [vMsg, string(FFrames[FFrameCount - 1].Entity)]);
  raise ERALXMLError.CreateAt(vMsg, CurrentLine, CurrentColumn);
end;

procedure TRALXMLReader.BufAdd(AByte: Byte);
begin
  if FBufLen >= Length(FBuffer) then
    SetLength(FBuffer, Length(FBuffer) * 2);
  FBuffer[FBufLen] := AByte;
  Inc(FBufLen);
end;

procedure TRALXMLReader.BufAddBytes(AText: PByte; ALen: IntegerRAL);
begin
  if ALen <= 0 then
    Exit;
  while FBufLen + ALen > Length(FBuffer) do
    SetLength(FBuffer, Length(FBuffer) * 2);
  Move(AText^, FBuffer[FBufLen], ALen);
  Inc(FBufLen, ALen);
end;

procedure TRALXMLReader.BufAddCodePoint(ACode: Cardinal);
begin
  if ACode < $80 then
    BufAdd(ACode)
  else if ACode < $800 then
  begin
    BufAdd($C0 or (ACode shr 6));
    BufAdd($80 or (ACode and $3F));
  end
  else if ACode < $10000 then
  begin
    BufAdd($E0 or (ACode shr 12));
    BufAdd($80 or ((ACode shr 6) and $3F));
    BufAdd($80 or (ACode and $3F));
  end
  else
  begin
    BufAdd($F0 or (ACode shr 18));
    BufAdd($80 or ((ACode shr 12) and $3F));
    BufAdd($80 or ((ACode shr 6) and $3F));
    BufAdd($80 or (ACode and $3F));
  end;
end;

procedure TRALXMLReader.CheckChar(ACode: Cardinal);
begin
  if not RALXMLIsChar(ACode) then
    Error(Format(emXMLInvalidChar, [IntToHex(ACode, 4)]));
end;

function TRALXMLReader.DecodeChar(var P: PByte; AStop: PByte): Cardinal;
begin
  Result := Utf8Next(P, AStop);
  if Result = $FFFFFFFF then
    Error(emXMLInvalidUTF8);
end;

function TRALXMLReader.SkipSpaces(var P: PByte; AStop: PByte): boolean;
begin
  Result := False;
  while (NativeUInt(P) < NativeUInt(AStop)) and IsBlank(P^) do
  begin
    Inc(P);
    Result := True;
  end;
end;

function TRALXMLReader.StartsWith(P, AStop: PByte; const AText: StringRAL): boolean;
begin
  Result := (NativeUInt(AStop) - NativeUInt(P) >= NativeUInt(Length(AText))) and
    CompareMem(P, Pointer(AText), Length(AText));
end;

function TRALXMLReader.ReadName(var P: PByte; AStop: PByte; AToken: boolean): StringRAL;
var
  vStart, vLast: PByte;
  vCode: Cardinal;
  vFirst: boolean;
begin
  vStart := P;
  vFirst := not AToken;
  while NativeUInt(P) < NativeUInt(AStop) do
  begin
    vLast := P;
    if P^ < $80 then
    begin
      vCode := P^;
      Inc(P);
    end
    else
      vCode := DecodeChar(P, AStop);

    if vFirst then
    begin
      if not RALXMLIsNameStartChar(vCode) then
      begin
        P := vLast;
        Break;
      end;
      vFirst := False;
    end
    else if not RALXMLIsNameChar(vCode) then
    begin
      P := vLast;
      Break;
    end;
  end;

  if P = vStart then
    Error(emXMLNameExpected);
  if NativeUInt(P) - NativeUInt(vStart) > NativeUInt(FOptions.MaxNameLength) then
    Error(Format(emXMLNameLimit, [FOptions.MaxNameLength]));
  Result := FNames.Add(vStart, P - vStart);
end;

procedure TRALXMLReader.ValidateQName(const AName: StringRAL);
var
  vPos: IntegerRAL;
begin
  if not FOptions.Namespaces then
    Exit;
  vPos := Pos(StringRAL(':'), AName);
  if vPos = 0 then
    Exit;
  // a QName has one colon, with a name on each side
  if (vPos = 1) or (vPos = Length(AName)) or
     (Pos(StringRAL(':'), Copy(AName, vPos + 1, MaxInt)) > 0) or
     not RALXMLIsName(Copy(AName, vPos + 1, MaxInt), True) then
    Error(Format(emXMLInvalidQName, [AName]));
end;

procedure TRALXMLReader.SplitName(const AName: StringRAL; out APrefix, ALocal: StringRAL);
var
  vPos: IntegerRAL;
begin
  vPos := 0;
  if FOptions.Namespaces then
    vPos := Pos(StringRAL(':'), AName);
  if vPos = 0 then
  begin
    APrefix := '';
    ALocal := AName;
  end
  else
  begin
    APrefix := FNames.Add(PByte(Pointer(AName)), vPos - 1);
    ALocal := FNames.Add(PByte(Pointer(AName)) + vPos, Length(AName) - vPos);
  end;
end;

function TRALXMLReader.ReadQuoted(var P: PByte; AStop: PByte): StringRAL;
var
  vQuote: Byte;
  vStart: PByte;
begin
  if (NativeUInt(P) >= NativeUInt(AStop)) or ((P^ <> cQuot) and (P^ <> cApos)) then
    Error(emXMLQuoteExpected);
  vQuote := P^;
  Inc(P);
  vStart := P;
  while (NativeUInt(P) < NativeUInt(AStop)) and (P^ <> vQuote) do
    Inc(P);
  if NativeUInt(P) >= NativeUInt(AStop) then
    Error(emXMLUnexpectedEnd);
  Result := RALXMLMakeString(vStart, P - vStart);
  Inc(P);
end;

function TRALXMLReader.EntityIndex(const AName: StringRAL; AParameter: boolean): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := -1;
  for vInt := 0 to FEntityCount - 1 do
    if (FEntities[vInt].Name = AName) and
       ((FEntities[vInt].Notation = '%') = AParameter) then
    begin
      Result := vInt;
      Exit;
    end;
end;

procedure TRALXMLReader.AddEntity(const AEntity: TRALXMLEntity; AParameter: boolean);
begin
  // the first declaration of a name binds it; later ones are ignored (XML 1.0 4.2)
  if EntityIndex(AEntity.Name, AParameter) >= 0 then
    Exit;
  if FEntityCount >= Length(FEntities) then
    SetLength(FEntities, FEntityCount * 2 + 8);
  FEntities[FEntityCount] := AEntity;
  // a parameter entity is told apart by '%' in Notation, which it never has otherwise
  if AParameter then
    FEntities[FEntityCount].Notation := '%';
  Inc(FEntityCount);
end;

procedure TRALXMLReader.AddAttrDecl(const AElement, AAttribute, AValue: StringRAL;
  AHasDefault, ATokenized: boolean);
begin
  if FindAttrDecl(AElement, AAttribute) >= 0 then
    Exit;
  if FAttrDeclCount >= Length(FAttrDecls) then
    SetLength(FAttrDecls, FAttrDeclCount * 2 + 8);
  FAttrDecls[FAttrDeclCount].Element := AElement;
  FAttrDecls[FAttrDeclCount].Attribute := AAttribute;
  FAttrDecls[FAttrDeclCount].Value := AValue;
  FAttrDecls[FAttrDeclCount].HasDefault := AHasDefault;
  FAttrDecls[FAttrDeclCount].Tokenized := ATokenized;
  Inc(FAttrDeclCount);
end;

function TRALXMLReader.FindAttrDecl(const AElement, AAttribute: StringRAL): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := -1;
  for vInt := 0 to FAttrDeclCount - 1 do
    if (FAttrDecls[vInt].Element = AElement) and (FAttrDecls[vInt].Attribute = AAttribute) then
    begin
      Result := vInt;
      Exit;
    end;
end;

function TRALXMLReader.ResolveExternal(const APublicId, ASystemId: StringRAL;
  out AContent: StringRAL): boolean;
var
  vBOM: IntegerRAL;
  vEnc: StringRAL;
begin
  Result := False;
  AContent := '';
  if not Assigned(FOptions.OnResolveEntity) then
    Exit;
  FOptions.OnResolveEntity(Self, APublicId, ASystemId, AContent, Result);
  if not Result then
    Exit;

  // an external entity has its own encoding and may start with a text declaration
  vEnc := RALXMLDetectEncoding(PByte(Pointer(AContent)), Length(AContent), '', vBOM);
  if (vEnc <> 'utf-8') and (vEnc <> 'utf8') then
    AContent := RALXMLToUTF8(PByte(Pointer(AContent)) + vBOM, Length(AContent) - vBOM, vEnc)
  else if vBOM > 0 then
    AContent := Copy(AContent, vBOM + 1, MaxInt);
end;

procedure TRALXMLReader.CheckLimitDepth;
begin
  if FFrameCount - 1 > FOptions.MaxEntityDepth then
    Error(Format(emXMLEntityLimit, [FOptions.MaxEntityExpansion]));
end;

procedure TRALXMLReader.ParseCharRef(var P: PByte; AStop: PByte);
var
  vCode: Cardinal;
  vHex: boolean;
  vDigit: IntegerRAL;
  vAny: boolean;
begin
  // P stands after "&#"
  vHex := (NativeUInt(P) < NativeUInt(AStop)) and (P^ = Ord('x'));
  if vHex then
    Inc(P);
  vCode := 0;
  vAny := False;
  while (NativeUInt(P) < NativeUInt(AStop)) and (P^ <> cSemi) do
  begin
    case P^ of
      Ord('0')..Ord('9'):
        vDigit := P^ - Ord('0');
      Ord('a')..Ord('f'):
        if vHex then vDigit := P^ - Ord('a') + 10 else vDigit := -1;
      Ord('A')..Ord('F'):
        if vHex then vDigit := P^ - Ord('A') + 10 else vDigit := -1;
    else
      vDigit := -1;
    end;
    if vDigit < 0 then
      Error(emXMLInvalidCharRef);
    if vHex then
      vCode := vCode * 16 + Cardinal(vDigit)
    else
      vCode := vCode * 10 + Cardinal(vDigit);
    if vCode > $10FFFF then
      Error(emXMLInvalidCharRef);
    vAny := True;
    Inc(P);
  end;
  if (not vAny) or (NativeUInt(P) >= NativeUInt(AStop)) then
    Error(emXMLInvalidCharRef);
  Inc(P);

  // XML 1.1 allows the C0 and C1 controls through a reference, never #0
  if FVersion11 and (vCode >= 1) and (vCode <= $1F) then
    BufAddCodePoint(vCode)
  else
  begin
    if not RALXMLIsChar(vCode) then
      Error(Format(emXMLInvalidChar, [IntToHex(vCode, 4)]));
    BufAddCodePoint(vCode);
  end;
end;

function TRALXMLReader.NormalizeTokens(const AValue: StringRAL): StringRAL;
var
  vInt, vLen: IntegerRAL;
  vSpace: boolean;
begin
  // a tokenized attribute type collapses its blanks (XML 1.0 3.3.3)
  SetLength(Result, Length(AValue));
  vLen := 0;
  vSpace := True;
  for vInt := 1 to Length(AValue) do
  begin
    if AValue[vInt] = ' ' then
    begin
      if not vSpace then
      begin
        Inc(vLen);
        Result[vLen] := ' ';
        vSpace := True;
      end;
    end
    else
    begin
      Inc(vLen);
      Result[vLen] := AValue[vInt];
      vSpace := False;
    end;
  end;
  if (vLen > 0) and (Result[vLen] = ' ') then
    Dec(vLen);
  SetLength(Result, vLen);
end;

procedure TRALXMLReader.ExpandAttrText(const AText: StringRAL; ADepth: IntegerRAL);
var
  vPtr, vStop: PByte;
  vName: StringRAL;
  vCode: Cardinal;
begin
  vPtr := PByte(Pointer(AText));
  vStop := vPtr + Length(AText);
  while NativeUInt(vPtr) < NativeUInt(vStop) do
  begin
    case vPtr^ of
      cLT:
        Error(emXMLLtInAttribute);
      cAmp:
        begin
          Inc(vPtr);
          if (NativeUInt(vPtr) < NativeUInt(vStop)) and (vPtr^ = cHash) then
          begin
            Inc(vPtr);
            ParseCharRef(vPtr, vStop);
          end
          else
          begin
            vName := ReadName(vPtr, vStop);
            if (NativeUInt(vPtr) >= NativeUInt(vStop)) or (vPtr^ <> cSemi) then
              Error(emXMLSemicolonExpected);
            Inc(vPtr);
            ExpandAttrEntity(vName, ADepth + 1);
          end;
        end;
      cTab, cLF, cCR:
        begin
          // replacement text was normalized already; its blanks become spaces
          BufAdd(cSpace);
          Inc(vPtr);
        end;
    else
      if vPtr^ < $80 then
      begin
        BufAdd(vPtr^);
        Inc(vPtr);
      end
      else
      begin
        vCode := DecodeChar(vPtr, vStop);
        CheckChar(vCode);
        BufAddCodePoint(vCode);
      end;
    end;
  end;
end;

procedure TRALXMLReader.ExpandAttrEntity(const AName: StringRAL; ADepth: IntegerRAL);
var
  vIdx: IntegerRAL;
begin
  if AName = 'lt' then
    BufAdd(cLT)
  else if AName = 'gt' then
    BufAdd(cGT)
  else if AName = 'amp' then
    BufAdd(cAmp)
  else if AName = 'apos' then
    BufAdd(cApos)
  else if AName = 'quot' then
    BufAdd(cQuot)
  else
  begin
    vIdx := EntityIndex(AName, False);
    if vIdx < 0 then
    begin
      if FHasExternalParts and (FStandalone <> 'yes') then
        Exit; // declared, maybe, where the parser does not read: skipped
      Error(Format(emXMLUndefinedEntity, [AName]));
    end;
    if not FOptions.ProcessInternalEntities then
      Error(Format(emXMLEntityDisabled, [AName]));
    if FEntities[vIdx].IsExternal then
      Error(Format(emXMLExternalInAttribute, [AName]));
    if (FStandalone = 'yes') and FEntities[vIdx].DeclaredExternally then
      Error(Format(emXMLUndefinedEntity, [AName]));
    if ADepth > FOptions.MaxEntityDepth then
      Error(Format(emXMLEntityLimit, [FOptions.MaxEntityExpansion]));

    FExpanded := FExpanded + Length(FEntities[vIdx].Value);
    if FExpanded > FOptions.MaxEntityExpansion then
      Error(Format(emXMLEntityLimit, [FOptions.MaxEntityExpansion]));
    ExpandAttrText(FEntities[vIdx].Value, ADepth);
  end;
end;

procedure TRALXMLReader.ReadAttrValue(var P: PByte; AStop: PByte; ADepth: IntegerRAL);
var
  vQuote: Byte;
  vName: StringRAL;
  vCode: Cardinal;
begin
  // AttValue normalization (XML 1.0 3.3.3): blanks become spaces, references expand
  if (NativeUInt(P) >= NativeUInt(AStop)) or ((P^ <> cQuot) and (P^ <> cApos)) then
    Error(emXMLQuoteExpected);
  vQuote := P^;
  Inc(P);
  while True do
  begin
    if NativeUInt(P) >= NativeUInt(AStop) then
      Error(emXMLUnexpectedEnd);
    if P^ = vQuote then
    begin
      Inc(P);
      Break;
    end;

    case P^ of
      cLT:
        begin
          SetFramePos(P);
          Error(emXMLLtInAttribute);
        end;
      cAmp:
        begin
          Inc(P);
          if (NativeUInt(P) < NativeUInt(AStop)) and (P^ = cHash) then
          begin
            Inc(P);
            ParseCharRef(P, AStop);
          end
          else
          begin
            SetFramePos(P);
            vName := ReadName(P, AStop);
            if (NativeUInt(P) >= NativeUInt(AStop)) or (P^ <> cSemi) then
              Error(emXMLSemicolonExpected);
            Inc(P);
            ExpandAttrEntity(vName, ADepth + 1);
          end;
        end;
      cCR:
        begin
          if FOptions.NormalizeAttributeBlanks then
            BufAdd(cSpace)
          else
            BufAdd(cCR);
          Inc(P);
          if (NativeUInt(P) < NativeUInt(AStop)) and (P^ = cLF) then
          begin
            if not FOptions.NormalizeAttributeBlanks then
              BufAdd(cLF);
            Inc(P);
          end;
        end;
      cTab, cLF:
        begin
          if FOptions.NormalizeAttributeBlanks then
            BufAdd(cSpace)
          else
            BufAdd(P^);
          Inc(P);
        end;
    else
      if P^ < $20 then
      begin
        SetFramePos(P);
        Error(Format(emXMLInvalidChar, [IntToHex(P^, 4)]));
      end
      else if P^ < $80 then
      begin
        BufAdd(P^);
        Inc(P);
      end
      else
      begin
        vCode := DecodeChar(P, AStop);
        CheckChar(vCode);
        BufAddCodePoint(vCode);
      end;
    end;
  end;
end;

function TRALXMLReader.LookupNamespace(const APrefix: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  if APrefix = 'xml' then
  begin
    Result := FXmlNsURI;
    Exit;
  end;
  for vInt := FNsCount - 1 downto 0 do
    if FNsBindings[vInt].Prefix = APrefix then
    begin
      Result := FNsBindings[vInt].URI;
      Exit;
    end;
  Result := '';
end;

procedure TRALXMLReader.ParseAttributes;
var
  vPtr, vStop: PByte;
  vName: StringRAL;
  vSpace: boolean;
begin
  vPtr := Frame;
  vStop := FFrames[FFrameCount - 1].Stop;
  FAttrCount := 0;
  while True do
  begin
    vSpace := SkipSpaces(vPtr, vStop);
    if NativeUInt(vPtr) >= NativeUInt(vStop) then
    begin
      SetFramePos(vPtr);
      Error(emXMLUnexpectedEnd);
    end;
    if (vPtr^ = cGT) or (vPtr^ = cSlash) then
      Break;
    if not vSpace then
    begin
      SetFramePos(vPtr);
      Error(emXMLSpaceExpected);
    end;

    SetFramePos(vPtr);
    vName := ReadName(vPtr, vStop);
    SkipSpaces(vPtr, vStop);
    if (NativeUInt(vPtr) >= NativeUInt(vStop)) or (vPtr^ <> cEq) then
    begin
      SetFramePos(vPtr);
      Error(emXMLEqualsExpected);
    end;
    Inc(vPtr);
    SkipSpaces(vPtr, vStop);

    if FAttrCount >= FOptions.MaxAttributes then
    begin
      SetFramePos(vPtr);
      Error(Format(emXMLAttributeLimit, [FOptions.MaxAttributes]));
    end;
    if FAttrCount >= Length(FAttributes) then
      SetLength(FAttributes, FAttrCount * 2);

    FBufLen := 0;
    SetFramePos(vPtr);
    ReadAttrValue(vPtr, vStop, 0);
    FAttributes[FAttrCount].Name := vName;
    FAttributes[FAttrCount].Value := RALXMLMakeString(@FBuffer[0], FBufLen);
    FAttributes[FAttrCount].Specified := True;
    FAttributes[FAttrCount].NamespaceURI := '';
    Inc(FAttrCount);
  end;
  SetFramePos(vPtr);
end;

procedure TRALXMLReader.ApplyAttrDefaults;
var
  vInt, vAttr: IntegerRAL;
  vFound: boolean;
begin
  for vInt := 0 to FAttrDeclCount - 1 do
  begin
    if FAttrDecls[vInt].Element <> FName then
      Continue;

    vFound := False;
    for vAttr := 0 to FAttrCount - 1 do
      if FAttributes[vAttr].Name = FAttrDecls[vInt].Attribute then
      begin
        if FAttrDecls[vInt].Tokenized then
          FAttributes[vAttr].Value := NormalizeTokens(FAttributes[vAttr].Value);
        vFound := True;
        Break;
      end;

    if (not vFound) and FAttrDecls[vInt].HasDefault then
    begin
      if FAttrCount >= Length(FAttributes) then
        SetLength(FAttributes, FAttrCount * 2);
      FAttributes[FAttrCount].Name := FAttrDecls[vInt].Attribute;
      FAttributes[FAttrCount].Value := FAttrDecls[vInt].Value;
      FAttributes[FAttrCount].Specified := False;
      FAttributes[FAttrCount].NamespaceURI := '';
      Inc(FAttrCount);
    end;
  end;
end;

procedure TRALXMLReader.CheckAttrUnique;
var
  vInt, vOther: IntegerRAL;
begin
  for vInt := 1 to FAttrCount - 1 do
    for vOther := 0 to vInt - 1 do
    begin
      if FAttributes[vInt].Name = FAttributes[vOther].Name then
        Error(Format(emXMLDuplicateAttribute, [FAttributes[vInt].Name]));
      if FOptions.Namespaces and (FAttributes[vInt].Prefix <> '') and
         (FAttributes[vOther].Prefix <> '') and
         (FAttributes[vInt].LocalName = FAttributes[vOther].LocalName) and
         (FAttributes[vInt].NamespaceURI = FAttributes[vOther].NamespaceURI) then
        Error(Format(emXMLDuplicateAttribute, [FAttributes[vInt].Name]));
    end;
end;

procedure TRALXMLReader.ResolveElementNamespaces;
var
  vInt, vPushed: IntegerRAL;
  vPrefix, vLocal, vURI: StringRAL;
begin
  vPushed := 0;
  for vInt := 0 to FAttrCount - 1 do
  begin
    SplitName(FAttributes[vInt].Name, vPrefix, vLocal);
    FAttributes[vInt].Prefix := vPrefix;
    FAttributes[vInt].LocalName := vLocal;
    if not FOptions.Namespaces then
      Continue;

    ValidateQName(FAttributes[vInt].Name);
    if (vPrefix = 'xmlns') or ((vPrefix = '') and (vLocal = 'xmlns')) then
    begin
      vURI := FNames.Add(FAttributes[vInt].Value);
      if vPrefix = '' then
        vPrefix := ''
      else
        vPrefix := vLocal;

      // the reserved prefixes and namespaces (Namespaces in XML 1.0, 3)
      if vPrefix = 'xmlns' then
        Error(emXMLReservedPrefix);
      if (vPrefix = 'xml') and (vURI <> FXmlNsURI) then
        Error(emXMLReservedPrefix);
      if (vPrefix <> 'xml') and ((vURI = FXmlNsURI) or (vURI = FXmlNsxmlnsURI)) then
        Error(emXMLReservedPrefix);
      if (vPrefix <> '') and (vURI = '') and not FVersion11 then
        Error(Format(emXMLUndeclaredPrefix, [vPrefix]));

      if FNsCount >= Length(FNsBindings) then
        SetLength(FNsBindings, FNsCount * 2);
      FNsBindings[FNsCount].Prefix := vPrefix;
      FNsBindings[FNsCount].URI := vURI;
      Inc(FNsCount);
      Inc(vPushed);
      FAttributes[vInt].NamespaceURI := FXmlNsxmlnsURI;
    end;
  end;
  FElements[FElementCount - 1].NsCount := vPushed;

  if not FOptions.Namespaces then
  begin
    FPrefix := '';
    FLocalName := FName;
    FNamespaceURI := '';
    Exit;
  end;

  ValidateQName(FName);
  SplitName(FName, FPrefix, FLocalName);
  if (FPrefix = 'xmlns') then
    Error(emXMLReservedPrefix);
  FNamespaceURI := LookupNamespace(FPrefix);
  if (FPrefix <> '') and (FNamespaceURI = '') then
    Error(Format(emXMLUndeclaredPrefix, [FPrefix]));

  for vInt := 0 to FAttrCount - 1 do
  begin
    if FAttributes[vInt].NamespaceURI = FXmlNsxmlnsURI then
      Continue;
    vPrefix := FAttributes[vInt].Prefix;
    if vPrefix = '' then
      Continue;
    vURI := LookupNamespace(vPrefix);
    if vURI = '' then
      Error(Format(emXMLUndeclaredPrefix, [vPrefix]));
    FAttributes[vInt].NamespaceURI := vURI;
  end;
end;

procedure TRALXMLReader.ParseStartTag;
var
  vPtr, vStop: PByte;
begin
  vPtr := Frame;
  vStop := FFrames[FFrameCount - 1].Stop;
  // P stands after '<'
  FName := ReadName(vPtr, vStop);
  SetFramePos(vPtr);

  if FRootClosed then
    Error(emXMLContentAfterRoot);
  if (FElementCount = 0) and FSeenRoot then
    Error(emXMLContentAfterRoot);
  FSeenRoot := True;

  ParseAttributes;
  vPtr := Frame;
  FIsEmptyElement := vPtr^ = cSlash;
  if FIsEmptyElement then
  begin
    Inc(vPtr);
    if (NativeUInt(vPtr) >= NativeUInt(vStop)) or (vPtr^ <> cGT) then
    begin
      SetFramePos(vPtr);
      Error(emXMLGreaterExpected);
    end;
  end;
  Inc(vPtr);
  SetFramePos(vPtr);

  if FElementCount >= FOptions.MaxDepth then
    Error(Format(emXMLDepthLimit, [FOptions.MaxDepth]));
  if FElementCount >= Length(FElements) then
    SetLength(FElements, FElementCount * 2);
  FElements[FElementCount].Name := FName;
  FElements[FElementCount].NsCount := 0;
  Inc(FElementCount);

  if FAttrDeclCount > 0 then
    ApplyAttrDefaults;
  ResolveElementNamespaces;
  if FAttrCount > 1 then
    CheckAttrUnique;

  FToken := xtkStartElement;
  FValue := '';
  FEmptyPending := FIsEmptyElement;
end;

procedure TRALXMLReader.ParseEndTag;
var
  vPtr, vStop: PByte;
  vName: StringRAL;
begin
  vPtr := Frame;
  vStop := FFrames[FFrameCount - 1].Stop;
  // P stands after '</'
  vName := ReadName(vPtr, vStop);
  SkipSpaces(vPtr, vStop);
  if (NativeUInt(vPtr) >= NativeUInt(vStop)) or (vPtr^ <> cGT) then
  begin
    SetFramePos(vPtr);
    Error(emXMLGreaterExpected);
  end;
  Inc(vPtr);

  if FElementCount <= FFrames[FFrameCount - 1].Depth then
    Error(Format(emXMLUnexpectedEndTag, [vName]));
  if FElements[FElementCount - 1].Name <> vName then
    Error(Format(emXMLTagMismatch, [vName, FElements[FElementCount - 1].Name]));
  SetFramePos(vPtr);

  FName := vName;
  if FOptions.Namespaces then
  begin
    SplitName(FName, FPrefix, FLocalName);
    FNamespaceURI := LookupNamespace(FPrefix);
  end
  else
  begin
    FPrefix := '';
    FLocalName := FName;
    FNamespaceURI := '';
  end;

  Dec(FNsCount, FElements[FElementCount - 1].NsCount);
  Dec(FElementCount);
  if FElementCount = 0 then
    FRootClosed := True;
  FAttrCount := 0;
  FIsEmptyElement := False;
  FToken := xtkEndElement;
  FValue := '';
end;

procedure TRALXMLReader.ParseComment;
var
  vPtr, vStop, vStart: PByte;
  vCode: Cardinal;
begin
  vPtr := Frame;
  vStop := FFrames[FFrameCount - 1].Stop;
  // P stands after '<!--'
  vStart := vPtr;
  while True do
  begin
    if NativeUInt(vPtr) >= NativeUInt(vStop) then
    begin
      SetFramePos(vPtr);
      Error(emXMLUnexpectedEnd);
    end;
    if vPtr^ = cDash then
    begin
      if (NativeUInt(vPtr) + 1 < NativeUInt(vStop)) and ((vPtr + 1)^ = cDash) then
      begin
        if (NativeUInt(vPtr) + 2 < NativeUInt(vStop)) and ((vPtr + 2)^ = cGT) then
          Break;
        SetFramePos(vPtr);
        Error(emXMLDoubleDash);
      end;
      Inc(vPtr);
    end
    else if vPtr^ < $80 then
    begin
      if (vPtr^ < $20) and not IsBlank(vPtr^) then
      begin
        SetFramePos(vPtr);
        Error(Format(emXMLInvalidChar, [IntToHex(vPtr^, 4)]));
      end;
      Inc(vPtr);
    end
    else
    begin
      vCode := DecodeChar(vPtr, vStop);
      CheckChar(vCode);
    end;
  end;

  FValue := RALXMLMakeString(vStart, vPtr - vStart);
  SetFramePos(vPtr + 3);
  FToken := xtkComment;
  FName := '';
end;

procedure TRALXMLReader.ParsePI;
var
  vPtr, vStop, vStart: PByte;
  vCode: Cardinal;
begin
  vPtr := Frame;
  vStop := FFrames[FFrameCount - 1].Stop;
  // P stands after '<?'
  FName := ReadName(vPtr, vStop);
  if LowerAscii(FName) = 'xml' then
  begin
    SetFramePos(vPtr);
    Error(emXMLDeclarationPlace);
  end;
  if FOptions.Namespaces and (Pos(StringRAL(':'), FName) > 0) then
  begin
    SetFramePos(vPtr);
    Error(Format(emXMLInvalidName, [FName]));
  end;

  if (NativeUInt(vPtr) < NativeUInt(vStop)) and not IsBlank(vPtr^) and
     not StartsWith(vPtr, vStop, '?>') then
  begin
    SetFramePos(vPtr);
    Error(emXMLSpaceExpected);
  end;
  SkipSpaces(vPtr, vStop);

  vStart := vPtr;
  while True do
  begin
    if NativeUInt(vPtr) >= NativeUInt(vStop) then
    begin
      SetFramePos(vPtr);
      Error(emXMLUnexpectedEnd);
    end;
    if (vPtr^ = cQuest) and (NativeUInt(vPtr) + 1 < NativeUInt(vStop)) and
       ((vPtr + 1)^ = cGT) then
      Break;
    if vPtr^ < $80 then
    begin
      if (vPtr^ < $20) and not IsBlank(vPtr^) then
      begin
        SetFramePos(vPtr);
        Error(Format(emXMLInvalidChar, [IntToHex(vPtr^, 4)]));
      end;
      Inc(vPtr);
    end
    else
    begin
      vCode := DecodeChar(vPtr, vStop);
      CheckChar(vCode);
    end;
  end;

  FValue := RALXMLMakeString(vStart, vPtr - vStart);
  SetFramePos(vPtr + 2);
  FToken := xtkProcessingInstruction;
end;

procedure TRALXMLReader.ParseCData;
var
  vPtr, vStop, vStart: PByte;
  vCode: Cardinal;
begin
  vPtr := Frame;
  vStop := FFrames[FFrameCount - 1].Stop;
  if FElementCount = 0 then
  begin
    SetFramePos(vPtr);
    Error(emXMLTextOutsideRoot);
  end;
  // P stands after '<![CDATA['
  vStart := vPtr;
  FBufLen := 0;
  while True do
  begin
    if NativeUInt(vPtr) >= NativeUInt(vStop) then
    begin
      SetFramePos(vPtr);
      Error(emXMLUnexpectedEnd);
    end;
    if (vPtr^ = cRBrack) and StartsWith(vPtr, vStop, ']]>') then
      Break;
    if vPtr^ = cCR then
    begin
      BufAddBytes(vStart, vPtr - vStart);
      BufAdd(cLF);
      Inc(vPtr);
      if (NativeUInt(vPtr) < NativeUInt(vStop)) and (vPtr^ = cLF) then
        Inc(vPtr);
      vStart := vPtr;
    end
    else if vPtr^ < $80 then
    begin
      if (vPtr^ < $20) and not IsBlank(vPtr^) then
      begin
        SetFramePos(vPtr);
        Error(Format(emXMLInvalidChar, [IntToHex(vPtr^, 4)]));
      end;
      Inc(vPtr);
    end
    else
    begin
      vCode := DecodeChar(vPtr, vStop);
      CheckChar(vCode);
    end;
  end;
  BufAddBytes(vStart, vPtr - vStart);

  FValue := RALXMLMakeString(@FBuffer[0], FBufLen);
  SetFramePos(vPtr + 3);
  FToken := xtkCData;
  FName := '';
end;

function TRALXMLReader.ParseText: boolean;
var
  vPtr, vStop, vStart, vSave: PByte;
  vCode: Cardinal;
  vName: StringRAL;
  vIdx, vLevel: IntegerRAL;
  vContent: StringRAL;
begin
  // True when a text token was produced; False when an entity frame was pushed
  Result := False;
  vPtr := Frame;
  vStop := FFrames[FFrameCount - 1].Stop;
  vStart := vPtr;
  FBufLen := 0;
  FIsWhitespace := True;

  while (NativeUInt(vPtr) < NativeUInt(vStop)) and (vPtr^ <> cLT) do
  begin
    case vPtr^ of
      cAmp:
        begin
          BufAddBytes(vStart, vPtr - vStart);
          vSave := vPtr;
          if FElementCount = 0 then
          begin
            SetFramePos(vPtr);
            Error(emXMLTextOutsideRoot);
          end;
          Inc(vPtr);
          if (NativeUInt(vPtr) < NativeUInt(vStop)) and (vPtr^ = cHash) then
          begin
            Inc(vPtr);
            SetFramePos(vSave);
            ParseCharRef(vPtr, vStop);
            FIsWhitespace := False;
          end
          else
          begin
            SetFramePos(vPtr);
            vName := ReadName(vPtr, vStop);
            if (NativeUInt(vPtr) >= NativeUInt(vStop)) or (vPtr^ <> cSemi) then
            begin
              SetFramePos(vPtr);
              Error(emXMLSemicolonExpected);
            end;
            Inc(vPtr);

            if vName = 'lt' then
              BufAdd(cLT)
            else if vName = 'gt' then
              BufAdd(cGT)
            else if vName = 'amp' then
              BufAdd(cAmp)
            else if vName = 'apos' then
              BufAdd(cApos)
            else if vName = 'quot' then
              BufAdd(cQuot)
            else
            begin
              SetFramePos(vSave);
              vIdx := EntityIndex(vName, False);
              if vIdx < 0 then
              begin
                if not (FHasExternalParts and (FStandalone <> 'yes')) then
                  Error(Format(emXMLUndefinedEntity, [vName]));
                // declared, maybe, where the parser does not read: skipped
                vStart := vPtr;
                Continue;
              end;
              if not FOptions.ProcessInternalEntities then
                Error(Format(emXMLEntityDisabled, [vName]));
              if FEntities[vIdx].Notation <> '' then
                Error(Format(emXMLUnparsedEntity, [vName]));
              if (FStandalone = 'yes') and FEntities[vIdx].DeclaredExternally then
                Error(Format(emXMLUndefinedEntity, [vName]));

              // recursion: the entity is already being read further down
              for vLevel := 1 to FFrameCount - 1 do
                if FFrames[vLevel].Entity = vName then
                  Error(Format(emXMLEntityRecursion, [vName]));

              if FEntities[vIdx].IsExternal then
              begin
                if not ResolveExternal(FEntities[vIdx].PublicId, FEntities[vIdx].SystemId,
                  vContent) then
                  Error(Format(emXMLExternalEntity, [vName]));
                // a text declaration may start an external parsed entity
                if Copy(vContent, 1, 5) = '<?xml' then
                begin
                  vIdx := Pos(StringRAL('?>'), vContent);
                  if vIdx > 0 then
                    vContent := Copy(vContent, vIdx + 2, MaxInt);
                end;
              end
              else
                vContent := FEntities[vIdx].Value;

              FExpanded := FExpanded + Length(vContent);
              if FExpanded > FOptions.MaxEntityExpansion then
                Error(Format(emXMLEntityLimit, [FOptions.MaxEntityExpansion]));

              SetFramePos(vPtr);
              NewFrame(vContent, vName, FEntities[vIdx].IsExternal);
              CheckLimitDepth;

              if FBufLen > 0 then
              begin
                FValue := RALXMLMakeString(@FBuffer[0], FBufLen);
                FToken := xtkText;
                FName := '';
                if (FElementCount = 0) and not FIsWhitespace then
                  Error(emXMLTextOutsideRoot);
                Result := True;
              end;
              Exit;
            end;
            FIsWhitespace := False;
          end;
          vStart := vPtr;
        end;
      cCR:
        begin
          BufAddBytes(vStart, vPtr - vStart);
          BufAdd(cLF);
          Inc(vPtr);
          if (NativeUInt(vPtr) < NativeUInt(vStop)) and (vPtr^ = cLF) then
            Inc(vPtr)
          else if FVersion11 and StartsWith(vPtr, vStop, #$C2#$85) then
            Inc(vPtr, 2);
          vStart := vPtr;
        end;
      cSpace, cTab, cLF:
        Inc(vPtr);
      cRBrack:
        begin
          if StartsWith(vPtr, vStop, ']]>') then
          begin
            SetFramePos(vPtr);
            Error(emXMLCDataEnd);
          end;
          FIsWhitespace := False;
          Inc(vPtr);
        end;
    else
      FIsWhitespace := False;
      if vPtr^ < $80 then
      begin
        if vPtr^ < $20 then
        begin
          SetFramePos(vPtr);
          Error(Format(emXMLInvalidChar, [IntToHex(vPtr^, 4)]));
        end;
        Inc(vPtr);
      end
      else
      begin
        vSave := vPtr;
        vCode := DecodeChar(vPtr, vStop);
        if FVersion11 and ((vCode = $85) or (vCode = $2028)) then
        begin
          // XML 1.1 line ends: NEL and LINE SEPARATOR become a line feed
          BufAddBytes(vStart, vSave - vStart);
          BufAdd(cLF);
          vStart := vPtr;
        end
        else
        begin
          if not RALXMLIsChar(vCode) then
          begin
            SetFramePos(vSave);
            Error(Format(emXMLInvalidChar, [IntToHex(vCode, 4)]));
          end;
        end;
      end;
    end;
  end;

  SetFramePos(vPtr);
  if (FBufLen = 0) then
    FValue := RALXMLMakeString(vStart, vPtr - vStart)
  else
  begin
    BufAddBytes(vStart, vPtr - vStart);
    FValue := RALXMLMakeString(@FBuffer[0], FBufLen);
  end;

  if FValue = '' then
    Exit;

  if FElementCount = 0 then
  begin
    if not FIsWhitespace then
      Error(emXMLTextOutsideRoot);
  end;

  FToken := xtkText;
  FName := '';
  Result := True;
end;

procedure TRALXMLReader.ParseXMLDecl(AText: boolean);
var
  vPtr, vStop: PByte;
  vName, vValue: StringRAL;
  vSeen: IntegerRAL;
begin
  vPtr := Frame;
  vStop := FFrames[FFrameCount - 1].Stop;
  // P stands after '<?xml'; the version comes first, then encoding, then standalone
  vSeen := 0;
  while True do
  begin
    if not SkipSpaces(vPtr, vStop) then
    begin
      if StartsWith(vPtr, vStop, '?>') then
        Break;
      SetFramePos(vPtr);
      Error(emXMLSpaceExpected);
    end;
    if StartsWith(vPtr, vStop, '?>') then
      Break;

    SetFramePos(vPtr);
    vName := ReadName(vPtr, vStop);
    SkipSpaces(vPtr, vStop);
    if (NativeUInt(vPtr) >= NativeUInt(vStop)) or (vPtr^ <> cEq) then
    begin
      SetFramePos(vPtr);
      Error(emXMLEqualsExpected);
    end;
    Inc(vPtr);
    SkipSpaces(vPtr, vStop);
    SetFramePos(vPtr);
    vValue := ReadQuoted(vPtr, vStop);

    if (vName = 'version') and (vSeen = 0) then
    begin
      if (Length(vValue) < 3) or (Copy(vValue, 1, 2) <> '1.') or
         not AllDigits(Copy(vValue, 3, MaxInt)) then
        Error(Format(emXMLBadDeclaration, [vName]));
      if not AText then
      begin
        FXMLVersion := vValue;
        FVersion11 := vValue = '1.1';
      end;
      vSeen := 1;
    end
    else if (vName = 'encoding') and (vSeen <= 1) and (AText or (vSeen = 1)) then
    begin
      if (vValue = '') or not (AnsiChar(vValue[1]) in ['A'..'Z', 'a'..'z']) then
        Error(Format(emXMLBadDeclaration, [vName]));
      if not AText then
        FXMLEncoding := vValue;
      vSeen := 2;
    end
    else if (vName = 'standalone') and (vSeen >= 1) and (vSeen <= 2) and not AText then
    begin
      if (vValue <> 'yes') and (vValue <> 'no') then
        Error(Format(emXMLBadDeclaration, [vName]));
      FStandalone := vValue;
      vSeen := 3;
    end
    else
      Error(Format(emXMLBadDeclaration, [vName]));
  end;

  if (vSeen = 0) and not AText then
    Error(Format(emXMLBadDeclaration, ['version']));
  if AText and (vSeen < 2) then
    Error(Format(emXMLBadDeclaration, ['encoding']));
  SetFramePos(vPtr + 2);
end;

function TRALXMLReader.ReadEntityValue(var P: PByte; AStop: PByte;
  AExternal: boolean): StringRAL;
var
  vQuote: Byte;
  vName: StringRAL;
  vIdx: IntegerRAL;
  vCode: Cardinal;
begin
  // EntityValue: character references expand now, general references stay as written
  if (NativeUInt(P) >= NativeUInt(AStop)) or ((P^ <> cQuot) and (P^ <> cApos)) then
    Error(emXMLQuoteExpected);
  vQuote := P^;
  Inc(P);
  FBufLen := 0;
  while True do
  begin
    if NativeUInt(P) >= NativeUInt(AStop) then
      Error(emXMLUnexpectedEnd);
    if P^ = vQuote then
    begin
      Inc(P);
      Break;
    end;

    case P^ of
      cAmp:
        begin
          Inc(P);
          if (NativeUInt(P) < NativeUInt(AStop)) and (P^ = cHash) then
          begin
            Inc(P);
            ParseCharRef(P, AStop);
          end
          else
          begin
            vName := ReadName(P, AStop);
            if (NativeUInt(P) >= NativeUInt(AStop)) or (P^ <> cSemi) then
              Error(emXMLSemicolonExpected);
            Inc(P);
            BufAdd(cAmp);
            BufAddBytes(PByte(Pointer(vName)), Length(vName));
            BufAdd(cSemi);
          end;
        end;
      cPct:
        begin
          // a parameter reference inside a declaration: only outside the internal subset
          if not AExternal then
            Error(emXMLPEInInternalSubset);
          Inc(P);
          vName := ReadName(P, AStop);
          if (NativeUInt(P) >= NativeUInt(AStop)) or (P^ <> cSemi) then
            Error(emXMLSemicolonExpected);
          Inc(P);
          vIdx := EntityIndex(vName, True);
          if vIdx < 0 then
            Error(Format(emXMLUndefinedEntity, [vName]));
          BufAddBytes(PByte(Pointer(FEntities[vIdx].Value)), Length(FEntities[vIdx].Value));
        end;
      cCR:
        begin
          BufAdd(cLF);
          Inc(P);
          if (NativeUInt(P) < NativeUInt(AStop)) and (P^ = cLF) then
            Inc(P);
        end;
    else
      if P^ < $80 then
      begin
        if (P^ < $20) and not IsBlank(P^) then
          Error(Format(emXMLInvalidChar, [IntToHex(P^, 4)]));
        BufAdd(P^);
        Inc(P);
      end
      else
      begin
        vCode := DecodeChar(P, AStop);
        CheckChar(vCode);
        BufAddCodePoint(vCode);
      end;
    end;
  end;
  Result := RALXMLMakeString(@FBuffer[0], FBufLen);
end;

procedure TRALXMLReader.ParseEntityDecl(var P: PByte; AStop: PByte; AExternal: boolean);
var
  vEntity: TRALXMLEntity;
  vParameter: boolean;
begin
  // P stands after '<!ENTITY'
  if not SkipSpaces(P, AStop) then
    Error(emXMLSpaceExpected);

  vParameter := (NativeUInt(P) < NativeUInt(AStop)) and (P^ = cPct);
  if vParameter then
  begin
    Inc(P);
    if not SkipSpaces(P, AStop) then
      Error(emXMLSpaceExpected);
  end;

  vEntity.Name := ReadName(P, AStop);
  if FOptions.Namespaces and (Pos(StringRAL(':'), vEntity.Name) > 0) then
    Error(Format(emXMLInvalidName, [vEntity.Name]));
  if not SkipSpaces(P, AStop) then
    Error(emXMLSpaceExpected);

  vEntity.Value := '';
  vEntity.PublicId := '';
  vEntity.SystemId := '';
  vEntity.Notation := '';
  vEntity.IsExternal := False;
  vEntity.DeclaredExternally := AExternal;

  if (NativeUInt(P) < NativeUInt(AStop)) and ((P^ = cQuot) or (P^ = cApos)) then
    vEntity.Value := ReadEntityValue(P, AStop, AExternal)
  else if StartsWith(P, AStop, 'SYSTEM') then
  begin
    Inc(P, 6);
    if not SkipSpaces(P, AStop) then
      Error(emXMLSpaceExpected);
    vEntity.SystemId := ReadQuoted(P, AStop);
    vEntity.IsExternal := True;
  end
  else if StartsWith(P, AStop, 'PUBLIC') then
  begin
    Inc(P, 6);
    if not SkipSpaces(P, AStop) then
      Error(emXMLSpaceExpected);
    vEntity.PublicId := ReadQuoted(P, AStop);
    if not SkipSpaces(P, AStop) then
      Error(emXMLSpaceExpected);
    vEntity.SystemId := ReadQuoted(P, AStop);
    vEntity.IsExternal := True;
  end
  else
    Error(emXMLBadDTD);

  if vEntity.IsExternal and not vParameter then
  begin
    if SkipSpaces(P, AStop) and StartsWith(P, AStop, 'NDATA') then
    begin
      Inc(P, 5);
      if not SkipSpaces(P, AStop) then
        Error(emXMLSpaceExpected);
      vEntity.Notation := ReadName(P, AStop);
    end;
  end;

  SkipSpaces(P, AStop);
  if (NativeUInt(P) >= NativeUInt(AStop)) or (P^ <> cGT) then
    Error(emXMLGreaterExpected);
  Inc(P);

  // after a parameter reference the parser could not read, declarations are not processed
  if not FSkippedDecls then
    AddEntity(vEntity, vParameter);
end;

procedure TRALXMLReader.ParseAttListDecl(var P: PByte; AStop: PByte);
var
  vElement, vAttr, vType, vDefault: StringRAL;
  vHasDefault, vTokenized: boolean;
  vStart: PByte;
  vDepth: IntegerRAL;
begin
  // P stands after '<!ATTLIST'
  if not SkipSpaces(P, AStop) then
    Error(emXMLSpaceExpected);
  vElement := ReadName(P, AStop);

  while True do
  begin
    SkipSpaces(P, AStop);
    if NativeUInt(P) >= NativeUInt(AStop) then
      Error(emXMLUnexpectedEnd);
    if P^ = cGT then
    begin
      Inc(P);
      Break;
    end;

    vAttr := ReadName(P, AStop);
    if not SkipSpaces(P, AStop) then
      Error(emXMLSpaceExpected);

    // the type: a keyword, NOTATION (...), or an enumeration (...)
    if (NativeUInt(P) < NativeUInt(AStop)) and (P^ = Ord('(')) then
    begin
      vType := 'ENUM';
      vDepth := 0;
      while (NativeUInt(P) < NativeUInt(AStop)) and (P^ <> Ord(')')) do
      begin
        Inc(P);
        Inc(vDepth);
      end;
      if NativeUInt(P) >= NativeUInt(AStop) then
        Error(emXMLUnexpectedEnd);
      if vDepth <= 1 then
        Error(emXMLBadDTD);
      Inc(P);
    end
    else
    begin
      vType := ReadName(P, AStop);
      if (vType <> 'CDATA') and (vType <> 'ID') and (vType <> 'IDREF') and
         (vType <> 'IDREFS') and (vType <> 'ENTITY') and (vType <> 'ENTITIES') and
         (vType <> 'NMTOKEN') and (vType <> 'NMTOKENS') and (vType <> 'NOTATION') then
        Error(emXMLBadDTD);
      if vType = 'NOTATION' then
      begin
        if not SkipSpaces(P, AStop) then
          Error(emXMLSpaceExpected);
        if (NativeUInt(P) >= NativeUInt(AStop)) or (P^ <> Ord('(')) then
          Error(emXMLBadDTD);
        while (NativeUInt(P) < NativeUInt(AStop)) and (P^ <> Ord(')')) do
          Inc(P);
        if NativeUInt(P) >= NativeUInt(AStop) then
          Error(emXMLUnexpectedEnd);
        Inc(P);
      end;
    end;
    vTokenized := vType <> 'CDATA';

    if not SkipSpaces(P, AStop) then
      Error(emXMLSpaceExpected);

    vHasDefault := False;
    vDefault := '';
    if StartsWith(P, AStop, '#REQUIRED') then
      Inc(P, 9)
    else if StartsWith(P, AStop, '#IMPLIED') then
      Inc(P, 8)
    else
    begin
      if StartsWith(P, AStop, '#FIXED') then
      begin
        Inc(P, 6);
        if not SkipSpaces(P, AStop) then
          Error(emXMLSpaceExpected);
      end;
      vStart := P;
      FBufLen := 0;
      ReadAttrValue(P, AStop, 0);
      vDefault := RALXMLMakeString(@FBuffer[0], FBufLen);
      if vTokenized then
        vDefault := NormalizeTokens(vDefault);
      vHasDefault := vStart <> P;
    end;

    if not FSkippedDecls then
      AddAttrDecl(vElement, vAttr, vDefault, vHasDefault, vTokenized);
  end;
end;

procedure TRALXMLReader.ParseDeclarations(AExternal: boolean);
var
  vPtr, vStop: PByte;
  vName, vContent: StringRAL;
  vIdx, vLevel: IntegerRAL;
  vCode: Cardinal;
  vQuote: Byte;
begin
  // the markup declarations of a subset, until ']' (internal) or the end of the frame
  while True do
  begin
    vPtr := Frame;
    vStop := FFrames[FFrameCount - 1].Stop;
    SkipSpaces(vPtr, vStop);
    SetFramePos(vPtr);

    if NativeUInt(vPtr) >= NativeUInt(vStop) then
    begin
      if FFrameCount > 1 then
      begin
        // the replacement of a parameter entity ended: back to the one that referenced it
        PopFrame;
        Continue;
      end;
      if AExternal then
        Exit;
      Error(emXMLUnexpectedEnd);
    end;

    if vPtr^ = cRBrack then
    begin
      if AExternal or (FFrameCount > 1) then
        Error(emXMLBadDTD);
      Inc(vPtr);
      SetFramePos(vPtr);
      Exit;
    end;

    if vPtr^ = cPct then
    begin
      Inc(vPtr);
      vName := ReadName(vPtr, vStop);
      if (NativeUInt(vPtr) >= NativeUInt(vStop)) or (vPtr^ <> cSemi) then
        Error(emXMLSemicolonExpected);
      Inc(vPtr);
      SetFramePos(vPtr);
      vIdx := EntityIndex(vName, True);
      if vIdx < 0 then
      begin
        if FStandalone = 'yes' then
          Error(Format(emXMLUndefinedEntity, [vName]));
        FHasExternalParts := True;
        FSkippedDecls := FStandalone <> 'yes';
        Continue;
      end;

      for vLevel := 1 to FFrameCount - 1 do
        if FFrames[vLevel].Entity = '%' + vName then
          Error(Format(emXMLEntityRecursion, [vName]));

      if FEntities[vIdx].IsExternal then
      begin
        FHasExternalParts := True;
        if not ResolveExternal(FEntities[vIdx].PublicId, FEntities[vIdx].SystemId,
          vContent) then
        begin
          // XML 1.0 5.1: declarations after an unread parameter entity are not processed
          FSkippedDecls := FStandalone <> 'yes';
          Continue;
        end;
        if Copy(vContent, 1, 5) = '<?xml' then
        begin
          vLevel := Pos(StringRAL('?>'), vContent);
          if vLevel > 0 then
            vContent := Copy(vContent, vLevel + 2, MaxInt);
        end;
        NewFrame(vContent, '%' + vName, True);
      end
      else
        NewFrame(FEntities[vIdx].Value, '%' + vName, AExternal);
      CheckLimitDepth;
      Continue;
    end;

    if vPtr^ <> cLT then
      Error(emXMLBadDTD);

    if StartsWith(vPtr, vStop, '<!--') then
    begin
      SetFramePos(vPtr + 4);
      ParseComment;
    end
    else if StartsWith(vPtr, vStop, '<?') then
    begin
      SetFramePos(vPtr + 2);
      if StartsWith(vPtr + 2, vStop, 'xml') and
         ((NativeUInt(vPtr) + 5 >= NativeUInt(vStop)) or IsBlank((vPtr + 5)^)) then
      begin
        // a text declaration may open an external subset
        if not AExternal then
          Error(emXMLDeclarationPlace);
        SetFramePos(vPtr + 5);
        ParseXMLDecl(True);
      end
      else
        ParsePI;
    end
    else if StartsWith(vPtr, vStop, '<!ENTITY') then
    begin
      Inc(vPtr, 8);
      ParseEntityDecl(vPtr, vStop, AExternal or (FFrameCount > 1) and
        FFrames[FFrameCount - 1].IsExternal);
      SetFramePos(vPtr);
    end
    else if StartsWith(vPtr, vStop, '<!ATTLIST') then
    begin
      Inc(vPtr, 9);
      ParseAttListDecl(vPtr, vStop);
      SetFramePos(vPtr);
    end
    else if StartsWith(vPtr, vStop, '<!ELEMENT') or StartsWith(vPtr, vStop, '<!NOTATION') then
    begin
      // element and notation declarations only matter to validation: syntax is checked
      if StartsWith(vPtr, vStop, '<!ELEMENT') then
        Inc(vPtr, 9)
      else
        Inc(vPtr, 10);
      if not SkipSpaces(vPtr, vStop) then
        Error(emXMLSpaceExpected);
      ReadName(vPtr, vStop);
      if not SkipSpaces(vPtr, vStop) then
        Error(emXMLSpaceExpected);
      vQuote := 0;
      while True do
      begin
        if NativeUInt(vPtr) >= NativeUInt(vStop) then
          Error(emXMLUnexpectedEnd);
        if vQuote <> 0 then
        begin
          if vPtr^ = vQuote then
            vQuote := 0;
        end
        else if (vPtr^ = cQuot) or (vPtr^ = cApos) then
          vQuote := vPtr^
        else if vPtr^ = cGT then
          Break
        else if vPtr^ = cLT then
          Error(emXMLBadDTD);
        if vPtr^ < $80 then
          Inc(vPtr)
        else
        begin
          vCode := DecodeChar(vPtr, vStop);
          CheckChar(vCode);
        end;
      end;
      SetFramePos(vPtr + 1);
    end
    else if StartsWith(vPtr, vStop, '<![') then
    begin
      // conditional sections exist only in the external subset
      if not AExternal then
        Error(emXMLBadDTD);
      Inc(vPtr, 3);
      SkipSpaces(vPtr, vStop);
      if StartsWith(vPtr, vStop, 'INCLUDE') then
      begin
        Inc(vPtr, 7);
        SkipSpaces(vPtr, vStop);
        if (NativeUInt(vPtr) >= NativeUInt(vStop)) or (vPtr^ <> cLBrack) then
          Error(emXMLBadDTD);
        Inc(vPtr);
        // the included text runs until its "]]>", read as more declarations
        vLevel := 1;
        vContent := '';
        vIdx := 0;
        while vLevel > 0 do
        begin
          if NativeUInt(vPtr) >= NativeUInt(vStop) then
            Error(emXMLUnexpectedEnd);
          if StartsWith(vPtr, vStop, '<![') then
            Inc(vLevel)
          else if StartsWith(vPtr, vStop, ']]>') then
          begin
            Dec(vLevel);
            if vLevel = 0 then
              Break;
          end;
          Inc(vPtr);
          Inc(vIdx);
        end;
        vContent := RALXMLMakeString(vPtr - vIdx, vIdx);
        SetFramePos(vPtr + 3);
        NewFrame(vContent, '', True);
        CheckLimitDepth;
      end
      else if StartsWith(vPtr, vStop, 'IGNORE') then
      begin
        Inc(vPtr, 6);
        SkipSpaces(vPtr, vStop);
        if (NativeUInt(vPtr) >= NativeUInt(vStop)) or (vPtr^ <> cLBrack) then
          Error(emXMLBadDTD);
        vLevel := 1;
        Inc(vPtr);
        while vLevel > 0 do
        begin
          if NativeUInt(vPtr) >= NativeUInt(vStop) then
            Error(emXMLUnexpectedEnd);
          if StartsWith(vPtr, vStop, '<![') then
          begin
            Inc(vLevel);
            Inc(vPtr, 3);
          end
          else if StartsWith(vPtr, vStop, ']]>') then
          begin
            Dec(vLevel);
            Inc(vPtr, 3);
          end
          else
            Inc(vPtr);
        end;
        SetFramePos(vPtr);
      end
      else
        Error(emXMLBadDTD);
    end
    else
      Error(emXMLBadDTD);
  end;
end;

procedure TRALXMLReader.ParseDocType;
var
  vPtr, vStop, vStart: PByte;
  vContent: StringRAL;
  vFrames: IntegerRAL;
begin
  vPtr := Frame;
  vStop := FFrames[FFrameCount - 1].Stop;
  // P stands after '<!DOCTYPE'
  if FSeenDocType or FSeenRoot or (FFrameCount > 1) then
    Error(emXMLDocTypePlace);
  FSeenDocType := True;

  if not SkipSpaces(vPtr, vStop) then
    Error(emXMLSpaceExpected);
  SetFramePos(vPtr);
  FDocTypeName := ReadName(vPtr, vStop);
  SkipSpaces(vPtr, vStop);

  if StartsWith(vPtr, vStop, 'SYSTEM') then
  begin
    Inc(vPtr, 6);
    if not SkipSpaces(vPtr, vStop) then
      Error(emXMLSpaceExpected);
    FDocTypeSystemId := ReadQuoted(vPtr, vStop);
    FHasExternalParts := True;
  end
  else if StartsWith(vPtr, vStop, 'PUBLIC') then
  begin
    Inc(vPtr, 6);
    if not SkipSpaces(vPtr, vStop) then
      Error(emXMLSpaceExpected);
    FDocTypePublicId := ReadQuoted(vPtr, vStop);
    if not SkipSpaces(vPtr, vStop) then
      Error(emXMLSpaceExpected);
    FDocTypeSystemId := ReadQuoted(vPtr, vStop);
    FHasExternalParts := True;
  end;
  SkipSpaces(vPtr, vStop);

  FValue := '';
  if (NativeUInt(vPtr) < NativeUInt(vStop)) and (vPtr^ = cLBrack) then
  begin
    Inc(vPtr);
    vStart := vPtr;
    SetFramePos(vPtr);
    vFrames := FFrameCount;
    ParseDeclarations(False);
    while FFrameCount > vFrames do
      PopFrame;
    vPtr := Frame;
    FValue := RALXMLMakeString(vStart, vPtr - vStart - 1);
    SkipSpaces(vPtr, vStop);
  end;

  if (NativeUInt(vPtr) >= NativeUInt(vStop)) or (vPtr^ <> cGT) then
  begin
    SetFramePos(vPtr);
    Error(emXMLGreaterExpected);
  end;
  SetFramePos(vPtr + 1);

  // the external subset is read only when the application provides it
  if FDocTypeSystemId <> '' then
  begin
    if ResolveExternal(FDocTypePublicId, FDocTypeSystemId, vContent) then
    begin
      if Copy(vContent, 1, 5) = '<?xml' then
      begin
        vFrames := Pos(StringRAL('?>'), vContent);
        if vFrames > 0 then
          vContent := Copy(vContent, vFrames + 2, MaxInt);
      end;
      vFrames := FFrameCount;
      NewFrame(vContent, '[dtd]', True);
      ParseDeclarations(True);
      while FFrameCount > vFrames do
        PopFrame;
    end;
  end;

  FToken := xtkDocType;
  FName := FDocTypeName;
end;

function TRALXMLReader.Read: boolean;
var
  vPtr, vStop: PByte;
begin
  Result := False;
  if FFrameCount = 0 then
    Exit;
  if FToken = xtkEndOfDocument then
    Exit;

  // the end of an element written as <a/>
  if FEmptyPending then
  begin
    FEmptyPending := False;
    Dec(FNsCount, FElements[FElementCount - 1].NsCount);
    Dec(FElementCount);
    if FElementCount = 0 then
      FRootClosed := True;
    FAttrCount := 0;
    FToken := xtkEndElement;
    FValue := '';
    Result := True;
    Exit;
  end;

  if not FStarted then
  begin
    FStarted := True;
    vPtr := Frame;
    vStop := FFrames[0].Stop;
    if StartsWith(vPtr, vStop, '<?xml') and
       ((NativeUInt(vPtr) + 5 < NativeUInt(vStop)) and IsBlank((vPtr + 5)^)) then
    begin
      SetFramePos(vPtr + 5);
      ParseXMLDecl(False);
    end;
  end;

  while True do
  begin
    if CurrentFrameEnded then
    begin
      if FFrameCount > 1 then
      begin
        PopFrame;
        Continue;
      end;
      if not FSeenRoot then
        Error(emXMLNoRoot);
      if FElementCount > 0 then
        Error(Format(emXMLUnclosed, [FElements[FElementCount - 1].Name]));
      FToken := xtkEndOfDocument;
      FName := '';
      FValue := '';
      Exit;
    end;

    vPtr := Frame;
    vStop := FFrames[FFrameCount - 1].Stop;
    if vPtr^ <> cLT then
    begin
      if ParseText then
      begin
        // blanks between markup outside the root are not reported
        if (FElementCount = 0) and FIsWhitespace then
          Continue;
        Result := True;
        Exit;
      end;
      Continue;
    end;

    Inc(vPtr);
    if NativeUInt(vPtr) >= NativeUInt(vStop) then
    begin
      SetFramePos(vPtr);
      Error(emXMLUnexpectedEnd);
    end;

    case vPtr^ of
      cSlash:
        begin
          SetFramePos(vPtr + 1);
          ParseEndTag;
        end;
      cQuest:
        begin
          SetFramePos(vPtr + 1);
          if StartsWith(vPtr + 1, vStop, 'xml') and
             ((NativeUInt(vPtr) + 4 < NativeUInt(vStop)) and IsBlank((vPtr + 4)^)) then
            Error(emXMLDeclarationPlace);
          ParsePI;
        end;
      cExcl:
        begin
          if StartsWith(vPtr, vStop, '!--') then
          begin
            SetFramePos(vPtr + 3);
            ParseComment;
          end
          else if StartsWith(vPtr, vStop, '![CDATA[') then
          begin
            SetFramePos(vPtr + 8);
            ParseCData;
          end
          else if StartsWith(vPtr, vStop, '!DOCTYPE') then
          begin
            SetFramePos(vPtr + 8);
            ParseDocType;
          end
          else
          begin
            SetFramePos(vPtr);
            Error(emXMLSyntax);
          end;
        end;
    else
      SetFramePos(vPtr);
      ParseStartTag;
    end;
    Result := True;
    Exit;
  end;
end;

function TRALXMLReader.ReadElementText: StringRAL;
var
  vDepth: IntegerRAL;
begin
  Result := '';
  if FToken <> xtkStartElement then
    Exit;
  vDepth := FElementCount;
  while Read do
  begin
    case FToken of
      xtkText, xtkCData:
        Result := Result + FValue;
      xtkEndElement:
        if FElementCount < vDepth then
          Exit;
      xtkStartElement:
        Error(Format(emXMLUnexpectedElement, [FName]));
    end;
  end;
end;

procedure TRALXMLReader.Skip;
var
  vDepth: IntegerRAL;
begin
  if FToken <> xtkStartElement then
    Exit;
  vDepth := FElementCount;
  while Read do
    if (FToken = xtkEndElement) and (FElementCount < vDepth) then
      Exit;
end;

function TRALXMLReader.AttributeValue(const AName: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 0 to FAttrCount - 1 do
    if FAttributes[vInt].Name = AName then
    begin
      Result := FAttributes[vInt].Value;
      Exit;
    end;
end;

function TRALXMLReader.HasAttribute(const AName: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 0 to FAttrCount - 1 do
    if FAttributes[vInt].Name = AName then
    begin
      Result := True;
      Exit;
    end;
end;

end.
