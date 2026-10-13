/// XML writer of RAL: writes UTF-8 straight into a stream, escaping only where needed.
unit RALXMLWriter;

{$I ../../base/PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALXMLReader;

type
  { Writes a document element by element into a stream, in UTF-8. Start tags stay
    open until something follows, so an element with no content is written <a/>. }
  TRALXMLWriter = class
  private
    /// Bytes not yet written to the stream.
    FBuffer: TBytes;
    /// Bytes in use in FBuffer.
    FBufLen: IntegerRAL;
    FDepth: IntegerRAL;
    /// Each open element has children.
    FHasChildren: array of boolean;
    FIndent: boolean;
    FIndentText: StringRAL;
    /// Each open element has text, so no indentation goes inside it.
    FMixed: array of boolean;
    /// Names of the open elements.
    FNames: TRALXMLStringArray;
    /// A start tag waits for its '>'.
    FOpenTag: boolean;
    /// Stream written to.
    FStream: TStream;
    /// Something was written, so the next line breaks.
    FWroteSomething: boolean;

    /// Writes the '>' of a start tag still open.
    procedure CloseStartTag;
    /// Breaks the line and indents to ADepth, when Indent.
    procedure NewLine(ADepth: IntegerRAL);
    /// Writes AByte.
    procedure Put(AByte: Byte);
    /// Writes ALen bytes at AText.
    procedure PutBytes(AText: PByte; ALen: IntegerRAL);
    /// Writes AValue escaped as text, or as an attribute value when AAttribute.
    procedure PutEscaped(const AValue: StringRAL; AAttribute: boolean);
    /// Writes AText as it is.
    procedure PutString(const AText: StringRAL);
  public
    /// Writer into AStream; AIndent puts each element on a line of its own.
    constructor Create(AStream: TStream; AIndent: boolean = False);
    destructor Destroy; override;

    /// Adds an attribute to the start tag just written.
    procedure Attribute(const AName, AValue: StringRAL);
    /// Writes a CDATA section; a "]]>" inside is split across two sections.
    procedure CData(const AValue: StringRAL);
    /// Writes a comment; raises when AValue holds "--" or ends with "-".
    procedure Comment(const AValue: StringRAL);
    /// Writes a DOCTYPE; AInternalSubset goes between brackets as given.
    procedure DocType(const AName, APublicId, ASystemId, AInternalSubset: StringRAL);
    /// Writes the start tag, the escaped text and the end tag of an element.
    procedure ElementText(const AName, AValue: StringRAL);
    /// Writes the end tag of the element open last, or "/>" when it has no content.
    procedure EndElement;
    /// Writes every byte buffered so far into the stream.
    procedure Flush;
    /// Writes a processing instruction.
    procedure ProcessingInstruction(const ATarget, AData: StringRAL);
    /// Writes AXML as it is, with no escaping: the caller answers for it.
    procedure Raw(const AXML: StringRAL);
    /// Opens an element; attributes may follow until anything else is written.
    procedure StartElement(const AName: StringRAL);
    /// Writes escaped text.
    procedure Text(const AValue: StringRAL);
    /// Writes the XML declaration, 1.0 and UTF-8; AStandalone is yes, no or ''.
    procedure WriteDeclaration(const AStandalone: StringRAL = '');

    /// Elements open.
    property Depth: IntegerRAL read FDepth;
    /// Puts each element on a line of its own, indented by IndentText.
    property Indent: boolean read FIndent write FIndent;
    /// Text of one level of indentation; two spaces by default.
    property IndentText: StringRAL read FIndentText write FIndentText;
  end;

/// Returns AValue escaped for the content of an element.
function RALXMLEscapeText(const AValue: StringRAL): StringRAL;
/// Returns AValue escaped for an attribute value between double quotes.
function RALXMLEscapeAttribute(const AValue: StringRAL): StringRAL;
/// True when every character of AValue may be written in XML 1.0 text.
function RALXMLIsValidText(const AValue: StringRAL): boolean;
{ Returns AName encoded as an XML name: each character a name may not hold becomes
  _xHHHH_, as .NET's XmlConvert.EncodeName writes it. }
function RALXMLEncodeName(const AName: StringRAL): StringRAL;
/// Returns the name RALXMLEncodeName encoded, decoded.
function RALXMLDecodeName(const AName: StringRAL): StringRAL;

implementation

const
  /// Size of the buffer flushed to the stream.
  cBufferSize = 16384;
  /// Upper case hexadecimal digits.
  cHex: array[0..15] of AnsiChar = '0123456789ABCDEF';

/// Decodes the UTF-8 sequence at P; $FFFFFFFF when malformed. P moves past it.
function NextCode(var P: PByte; AStop: PByte): Cardinal;
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
  if (Result < vMin) or ((Result >= $D800) and (Result <= $DFFF)) or (Result > $10FFFF) then
    Result := $FFFFFFFF;
end;

function RALXMLIsValidText(const AValue: StringRAL): boolean;
var
  vPtr, vStop: PByte;
  vCode: Cardinal;
begin
  Result := False;
  vPtr := PByte(Pointer(AValue));
  vStop := vPtr + Length(AValue);
  while NativeUInt(vPtr) < NativeUInt(vStop) do
  begin
    if vPtr^ < $80 then
    begin
      if (vPtr^ < $20) and (vPtr^ <> 9) and (vPtr^ <> 10) and (vPtr^ <> 13) then
        Exit;
      Inc(vPtr);
    end
    else
    begin
      vCode := NextCode(vPtr, vStop);
      if not RALXMLIsChar(vCode) then
        Exit;
    end;
  end;
  Result := True;
end;

/// Text a stream writer needs, one per escaped character
function EscapeOf(AByte: Byte; AAttribute: boolean): StringRAL;
begin
  case AByte of
    Ord('&'): Result := '&amp;';
    Ord('<'): Result := '&lt;';
    Ord('>'): Result := '&gt;';
    Ord('"'): if AAttribute then Result := '&quot;' else Result := '"';
    9: if AAttribute then Result := '&#9;' else Result := #9;
    10: if AAttribute then Result := '&#10;' else Result := #10;
    13: Result := '&#13;';
  else
    Result := '';
  end;
end;

/// AByte has to be escaped in text, or in an attribute value when AAttribute.
function NeedsEscape(AByte: Byte; AAttribute: boolean): boolean; inline;
begin
  case AByte of
    Ord('&'), Ord('<'), Ord('>'), 13:
      Result := True;
    Ord('"'), 9, 10:
      Result := AAttribute;
  else
    Result := AByte < $20;
  end;
end;

/// AValue escaped as text, or as an attribute value when AAttribute.
function EscapeString(const AValue: StringRAL; AAttribute: boolean): StringRAL;
var
  vStream: TBytesStream;
  vWriter: TRALXMLWriter;
begin
  vStream := TBytesStream.Create;
  try
    vWriter := TRALXMLWriter.Create(vStream);
    try
      vWriter.PutEscaped(AValue, AAttribute);
      vWriter.Flush;
    finally
      vWriter.Free;
    end;
    Result := RALXMLMakeString(PByte(vStream.Bytes), vStream.Size);
  finally
    vStream.Free;
  end;
end;

function RALXMLEscapeText(const AValue: StringRAL): StringRAL;
begin
  Result := EscapeString(AValue, False);
end;

function RALXMLEscapeAttribute(const AValue: StringRAL): StringRAL;
begin
  Result := EscapeString(AValue, True);
end;

function RALXMLEncodeName(const AName: StringRAL): StringRAL;
var
  vPtr, vStop, vStart: PByte;
  vCode: Cardinal;
  vFirst, vValid: boolean;
  vLen: IntegerRAL;

  procedure PutHex(ACode: Cardinal; ADigits: IntegerRAL);
  var
    vInt: IntegerRAL;
  begin
    Result := Result + '_x';
    for vInt := ADigits - 1 downto 0 do
      Result := Result + StringRAL(cHex[(ACode shr (vInt * 4)) and 15]);
    Result := Result + '_';
  end;

begin
  Result := '';
  if AName = '' then
    Exit;
  vPtr := PByte(Pointer(AName));
  vStop := vPtr + Length(AName);
  vFirst := True;
  while NativeUInt(vPtr) < NativeUInt(vStop) do
  begin
    vStart := vPtr;
    vCode := NextCode(vPtr, vStop);
    if vCode = $FFFFFFFF then
    begin
      PutHex(vStart^, 4);
      vPtr := vStart + 1;
      vFirst := False;
      Continue;
    end;

    if vFirst then
      vValid := RALXMLIsNameStartChar(vCode) and (vCode <> Ord(':'))
    else
      vValid := RALXMLIsNameChar(vCode) and (vCode <> Ord(':'));

    // an "_x" that would read as an escape is itself escaped
    if (vCode = Ord('_')) and (NativeUInt(vPtr) < NativeUInt(vStop)) and (vPtr^ = Ord('x')) then
      vValid := False;

    if vValid then
    begin
      vLen := vPtr - vStart;
      Result := Result + RALXMLMakeString(vStart, vLen);
    end
    else if vCode > $FFFF then
      PutHex(vCode, 8)
    else
      PutHex(vCode, 4);
    vFirst := False;
  end;
end;

/// Value of the ACount hexadecimal digits of AName from AStart; False for a non-digit
function HexValue(const AName: StringRAL; AStart, ACount: IntegerRAL; out ACode: Cardinal): boolean;
var
  vInt: IntegerRAL;
  vChr: AnsiChar;
begin
  ACode := 0;
  Result := False;
  for vInt := AStart to AStart + ACount - 1 do
  begin
    vChr := AnsiChar(AName[vInt]);
    case vChr of
      '0'..'9': ACode := ACode * 16 + Cardinal(Ord(vChr) - Ord('0'));
      'a'..'f': ACode := ACode * 16 + Cardinal(Ord(vChr) - Ord('a') + 10);
      'A'..'F': ACode := ACode * 16 + Cardinal(Ord(vChr) - Ord('A') + 10);
    else
      Exit;
    end;
  end;
  Result := ACode <= $10FFFF;
end;

/// The UTF-8 of the code point ACode.
function Utf8Of(ACode: Cardinal): StringRAL;
begin
  if ACode < $80 then
    Result := StringRAL(AnsiChar(ACode))
  else if ACode < $800 then
    Result := StringRAL(AnsiChar($C0 or (ACode shr 6))) + StringRAL(AnsiChar($80 or (ACode and $3F)))
  else if ACode < $10000 then
    Result := StringRAL(AnsiChar($E0 or (ACode shr 12))) +
      StringRAL(AnsiChar($80 or ((ACode shr 6) and $3F))) +
      StringRAL(AnsiChar($80 or (ACode and $3F)))
  else
    Result := StringRAL(AnsiChar($F0 or (ACode shr 18))) +
      StringRAL(AnsiChar($80 or ((ACode shr 12) and $3F))) +
      StringRAL(AnsiChar($80 or ((ACode shr 6) and $3F))) +
      StringRAL(AnsiChar($80 or (ACode and $3F)));
end;

function RALXMLDecodeName(const AName: StringRAL): StringRAL;
var
  vInt, vStart: IntegerRAL;
  vCode: Cardinal;
begin
  if Pos(StringRAL('_x'), AName) = 0 then
  begin
    Result := AName;
    Exit;
  end;

  // _xHHHH_ or _xHHHHHHHH_ becomes the character; anything else stays as written
  Result := '';
  vStart := 1;
  vInt := 1;
  while vInt <= Length(AName) do
  begin
    if (AName[vInt] = '_') and (vInt + 6 <= Length(AName)) and (AName[vInt + 1] = 'x') then
    begin
      if (AName[vInt + 6] = '_') and HexValue(AName, vInt + 2, 4, vCode) then
      begin
        Result := Result + Copy(AName, vStart, vInt - vStart) + Utf8Of(vCode);
        vInt := vInt + 7;
        vStart := vInt;
        Continue;
      end;
      if (vInt + 10 <= Length(AName)) and (AName[vInt + 10] = '_') and
         HexValue(AName, vInt + 2, 8, vCode) then
      begin
        Result := Result + Copy(AName, vStart, vInt - vStart) + Utf8Of(vCode);
        vInt := vInt + 11;
        vStart := vInt;
        Continue;
      end;
    end;
    Inc(vInt);
  end;
  Result := Result + Copy(AName, vStart, MaxInt);
end;

{ TRALXMLWriter }

constructor TRALXMLWriter.Create(AStream: TStream; AIndent: boolean);
begin
  inherited Create;
  FStream := AStream;
  FIndent := AIndent;
  FIndentText := '  ';
  SetLength(FBuffer, cBufferSize);
  SetLength(FNames, 16);
  SetLength(FHasChildren, 16);
  SetLength(FMixed, 16);
end;

destructor TRALXMLWriter.Destroy;
begin
  Flush;
  inherited Destroy;
end;

procedure TRALXMLWriter.Flush;
begin
  if FBufLen > 0 then
  begin
    FStream.WriteBuffer(FBuffer[0], FBufLen);
    FBufLen := 0;
  end;
end;

procedure TRALXMLWriter.Put(AByte: Byte);
begin
  if FBufLen >= cBufferSize then
    Flush;
  FBuffer[FBufLen] := AByte;
  Inc(FBufLen);
end;

procedure TRALXMLWriter.PutBytes(AText: PByte; ALen: IntegerRAL);
begin
  if ALen <= 0 then
    Exit;
  if FBufLen + ALen > cBufferSize then
  begin
    Flush;
    // a long run goes straight to the stream
    if ALen >= cBufferSize then
    begin
      FStream.WriteBuffer(AText^, ALen);
      Exit;
    end;
  end;
  Move(AText^, FBuffer[FBufLen], ALen);
  Inc(FBufLen, ALen);
end;

procedure TRALXMLWriter.PutString(const AText: StringRAL);
begin
  PutBytes(PByte(Pointer(AText)), Length(AText));
end;

procedure TRALXMLWriter.PutEscaped(const AValue: StringRAL; AAttribute: boolean);
var
  vPtr, vStop, vRun: PByte;
  vCode: Cardinal;
  vEscape: StringRAL;
begin
  vPtr := PByte(Pointer(AValue));
  vStop := vPtr + Length(AValue);
  vRun := vPtr;
  while NativeUInt(vPtr) < NativeUInt(vStop) do
  begin
    if vPtr^ < $80 then
    begin
      if NeedsEscape(vPtr^, AAttribute) then
      begin
        PutBytes(vRun, vPtr - vRun);
        vEscape := EscapeOf(vPtr^, AAttribute);
        if vEscape = '' then
          raise ERALXMLError.CreateAt(Format(emXMLInvalidChar, [IntToHex(vPtr^, 4)]), 0, 0);
        PutString(vEscape);
        Inc(vPtr);
        vRun := vPtr;
      end
      else
        Inc(vPtr);
    end
    else
    begin
      vCode := NextCode(vPtr, vStop);
      if not RALXMLIsChar(vCode) then
      begin
        if vCode = $FFFFFFFF then
          raise ERALXMLError.CreateAt(emXMLInvalidUTF8, 0, 0);
        raise ERALXMLError.CreateAt(Format(emXMLInvalidChar, [IntToHex(vCode, 4)]), 0, 0);
      end;
    end;
  end;
  PutBytes(vRun, vPtr - vRun);
end;

procedure TRALXMLWriter.NewLine(ADepth: IntegerRAL);
var
  vInt: IntegerRAL;
begin
  if not FWroteSomething then
    Exit;
  Put(10);
  for vInt := 1 to ADepth do
    PutString(FIndentText);
end;

procedure TRALXMLWriter.CloseStartTag;
begin
  if FOpenTag then
  begin
    Put(Ord('>'));
    FOpenTag := False;
  end;
end;

procedure TRALXMLWriter.WriteDeclaration(const AStandalone: StringRAL);
begin
  PutString('<?xml version="1.0" encoding="UTF-8"');
  if AStandalone <> '' then
  begin
    PutString(' standalone="');
    PutString(AStandalone);
    Put(Ord('"'));
  end;
  PutString('?>');
  FWroteSomething := True;
end;

procedure TRALXMLWriter.DocType(const AName, APublicId, ASystemId,
  AInternalSubset: StringRAL);
begin
  CloseStartTag;
  if FIndent then
    NewLine(0);
  PutString('<!DOCTYPE ');
  PutString(AName);
  if APublicId <> '' then
  begin
    PutString(' PUBLIC "');
    PutString(APublicId);
    PutString('" "');
    PutString(ASystemId);
    Put(Ord('"'));
  end
  else if ASystemId <> '' then
  begin
    PutString(' SYSTEM "');
    PutString(ASystemId);
    Put(Ord('"'));
  end;
  if AInternalSubset <> '' then
  begin
    PutString(' [');
    PutString(AInternalSubset);
    Put(Ord(']'));
  end;
  Put(Ord('>'));
  FWroteSomething := True;
end;

procedure TRALXMLWriter.StartElement(const AName: StringRAL);
begin
  CloseStartTag;
  if FDepth > 0 then
    FHasChildren[FDepth - 1] := True;
  if FIndent and ((FDepth = 0) or not FMixed[FDepth - 1]) then
    NewLine(FDepth);

  Put(Ord('<'));
  PutString(AName);
  FOpenTag := True;
  FWroteSomething := True;

  if FDepth >= Length(FNames) then
  begin
    SetLength(FNames, FDepth * 2);
    SetLength(FHasChildren, FDepth * 2);
    SetLength(FMixed, FDepth * 2);
  end;
  FNames[FDepth] := AName;
  FHasChildren[FDepth] := False;
  FMixed[FDepth] := False;
  Inc(FDepth);
end;

procedure TRALXMLWriter.Attribute(const AName, AValue: StringRAL);
begin
  if not FOpenTag then
    raise ERALXMLError.CreateAt(emXMLAttributeOutsideTag, 0, 0);
  Put(Ord(' '));
  PutString(AName);
  PutString('="');
  PutEscaped(AValue, True);
  Put(Ord('"'));
end;

procedure TRALXMLWriter.Text(const AValue: StringRAL);
begin
  if AValue = '' then
    Exit;
  CloseStartTag;
  if FDepth > 0 then
    FMixed[FDepth - 1] := True;
  PutEscaped(AValue, False);
end;

procedure TRALXMLWriter.CData(const AValue: StringRAL);
var
  vPos: IntegerRAL;
  vRest: StringRAL;
begin
  CloseStartTag;
  if FDepth > 0 then
    FMixed[FDepth - 1] := True;
  if not RALXMLIsValidText(AValue) then
    raise ERALXMLError.CreateAt(emXMLInvalidTextWrite, 0, 0);

  vRest := AValue;
  vPos := Pos(StringRAL(']]>'), vRest);
  while vPos > 0 do
  begin
    // "]]>" ends a section: the "]]" goes in one, the ">" in the next
    PutString('<![CDATA[');
    PutString(Copy(vRest, 1, vPos + 1));
    PutString(']]>');
    vRest := Copy(vRest, vPos + 2, MaxInt);
    vPos := Pos(StringRAL(']]>'), vRest);
  end;
  PutString('<![CDATA[');
  PutString(vRest);
  PutString(']]>');
end;

procedure TRALXMLWriter.Comment(const AValue: StringRAL);
begin
  if (Pos(StringRAL('--'), AValue) > 0) or
     ((AValue <> '') and (AValue[Length(AValue)] = '-')) or not RALXMLIsValidText(AValue) then
    raise ERALXMLError.CreateAt(emXMLInvalidComment, 0, 0);
  CloseStartTag;
  if FIndent and ((FDepth = 0) or not FMixed[FDepth - 1]) then
    NewLine(FDepth);
  if FDepth > 0 then
    FHasChildren[FDepth - 1] := True;
  PutString('<!--');
  PutString(AValue);
  PutString('-->');
  FWroteSomething := True;
end;

procedure TRALXMLWriter.ProcessingInstruction(const ATarget, AData: StringRAL);
begin
  if (Pos(StringRAL('?>'), AData) > 0) or not RALXMLIsValidText(AData) then
    raise ERALXMLError.CreateAt(emXMLInvalidPI, 0, 0);
  CloseStartTag;
  if FIndent and ((FDepth = 0) or not FMixed[FDepth - 1]) then
    NewLine(FDepth);
  if FDepth > 0 then
    FHasChildren[FDepth - 1] := True;
  PutString('<?');
  PutString(ATarget);
  if AData <> '' then
  begin
    Put(Ord(' '));
    PutString(AData);
  end;
  PutString('?>');
  FWroteSomething := True;
end;

procedure TRALXMLWriter.Raw(const AXML: StringRAL);
begin
  CloseStartTag;
  if FDepth > 0 then
    FMixed[FDepth - 1] := True;
  PutString(AXML);
  FWroteSomething := True;
end;

procedure TRALXMLWriter.EndElement;
begin
  if FDepth = 0 then
    raise ERALXMLError.CreateAt(emXMLNoOpenElement, 0, 0);
  Dec(FDepth);
  if FOpenTag then
  begin
    PutString('/>');
    FOpenTag := False;
  end
  else
  begin
    if FIndent and FHasChildren[FDepth] and not FMixed[FDepth] then
      NewLine(FDepth);
    PutString('</');
    PutString(FNames[FDepth]);
    Put(Ord('>'));
  end;
  FNames[FDepth] := '';
end;

procedure TRALXMLWriter.ElementText(const AName, AValue: StringRAL);
begin
  StartElement(AName);
  Text(AValue);
  EndElement;
end;

end.
