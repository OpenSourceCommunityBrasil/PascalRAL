/// ASN.1 in DER: the writer that builds keys and certificates, and a reader that
/// walks them
unit RALASN1;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\base\PascalRAL.inc}

{ Only what X.509, PKCS#1, PKCS#8 and PKCS#12 need: tags of one byte (number
  up to 30), definite lengths, and the universal types those formats use. The
  writer returns each element whole, so a structure is built from the inside
  out: DerSequence([DerInteger(...), DerOID(...)]) }

interface

uses
  SysUtils,
  RALTypes, RALConsts, RALBase64, RALBigInt;

const
  ASN1_BOOLEAN = $01;
  ASN1_INTEGER = $02;
  ASN1_BIT_STRING = $03;
  ASN1_OCTET_STRING = $04;
  ASN1_NULL = $05;
  ASN1_OID = $06;
  ASN1_UTF8_STRING = $0C;
  ASN1_PRINTABLE_STRING = $13;
  ASN1_IA5_STRING = $16;
  ASN1_UTC_TIME = $17;
  ASN1_GENERALIZED_TIME = $18;
  ASN1_BMP_STRING = $1E;
  ASN1_SEQUENCE = $30;
  ASN1_SET = $31;
  /// context-specific class, primitive; add the tag number
  ASN1_CONTEXT = $80;
  /// context-specific class, constructed; add the tag number
  ASN1_CONTEXT_CONSTRUCTED = $A0;

type
  ERALASN1 = class(Exception);

  TRALBytesArray = array of TBytes;

  /// one element read: where it is in the buffer and what it holds
  TRALASN1Element = record
    Tag: Byte;
    /// offset of the tag byte
    Start: IntegerRAL;
    /// offset of the first byte of the contents
    ContentStart: IntegerRAL;
    ContentLength: IntegerRAL;
    /// offset right after the element
    Next: IntegerRAL;
  end;

{ writer }

function DerElement(ATag: Byte; const AContent: TBytes): TBytes;
function DerConcat(const AParts: array of TBytes): TBytes;
function DerSequence(const AParts: array of TBytes): TBytes;
/// a SET OF; DER wants the elements sorted by their encoding, and they are
function DerSet(const AParts: array of TBytes): TBytes;
function DerInteger(const AValue: TRALBigNum): TBytes;
function DerIntegerValue(AValue: Int64RAL): TBytes;
/// an INTEGER from big-endian unsigned bytes (a serial number)
function DerIntegerBytes(const AValue: TBytes): TBytes;
function DerBoolean(AValue: boolean): TBytes;
function DerNull: TBytes;
/// an OBJECT IDENTIFIER from its dotted text: '1.2.840.113549.1.1.11'
function DerOID(const ADotted: StringRAL): TBytes;
function DerOctetString(const AValue: TBytes): TBytes;
/// a BIT STRING of whole bytes (no unused bits)
function DerBitString(const AValue: TBytes): TBytes;
/// a BIT STRING of named bits (KeyUsage): bit 0 is the most significant bit of
/// the first byte, and the trailing zero bits are dropped as DER requires
function DerNamedBits(ABits: Cardinal): TBytes;
function DerUTF8String(const AValue: StringRAL): TBytes;
function DerPrintableString(const AValue: StringRAL): TBytes;
function DerIA5String(const AValue: StringRAL): TBytes;
/// a BMPString (UTF-16 big-endian)
function DerBMPString(const AValue: StringRAL): TBytes;
/// UTCTime until 2049, GeneralizedTime from 2050 on (RFC 5280 4.1.2.5); the
/// time is taken as UTC
function DerTime(AValueUTC: TDateTime): TBytes;
/// [ATagNo] EXPLICIT: the element wrapped in a constructed context tag
function DerExplicit(ATagNo: Byte; const AContent: TBytes): TBytes;
/// [ATagNo] IMPLICIT over primitive contents
function DerImplicit(ATagNo: Byte; const AContent: TBytes): TBytes;
/// AlgorithmIdentifier; AParams empty means none, use DerNull where the
/// algorithm wants NULL
function DerAlgorithm(const AOID: StringRAL; const AParams: TBytes): TBytes;

{ reader }

/// the element that starts at APos; raises when it does not fit in ALimit
function DerRead(const AData: TBytes; APos: IntegerRAL; ALimit: IntegerRAL = -1): TRALASN1Element;
/// the element at APos, which must carry ATag
function DerExpect(const AData: TBytes; APos: IntegerRAL; ATag: Byte;
  ALimit: IntegerRAL = -1): TRALASN1Element;
/// the children of a constructed element
function DerChildren(const AData: TBytes; const AParent: TRALASN1Element): TRALASN1Element;
/// the elements inside AParent, in order
function DerItems(const AData: TBytes; const AParent: TRALASN1Element): TRALBytesArray;
/// the whole encoding of the element (tag, length and contents)
function DerBytes(const AData: TBytes; const AElement: TRALASN1Element): TBytes;
/// the contents only
function DerContent(const AData: TBytes; const AElement: TRALASN1Element): TBytes;
/// the element's contents read as an INTEGER (must be non-negative)
function DerAsBigNum(const AData: TBytes; const AElement: TRALASN1Element): TRALBigNum;
function DerAsInt64(const AData: TBytes; const AElement: TRALASN1Element): Int64RAL;
function DerAsOID(const AData: TBytes; const AElement: TRALASN1Element): StringRAL;
/// UTF8String, PrintableString, IA5String, BMPString and the like, as UTF-8
function DerAsString(const AData: TBytes; const AElement: TRALASN1Element): StringRAL;
/// UTCTime or GeneralizedTime, in UTC
function DerAsTime(const AData: TBytes; const AElement: TRALASN1Element): TDateTime;
/// the bytes of a BIT STRING without the unused-bits byte
function DerAsBitString(const AData: TBytes; const AElement: TRALASN1Element): TBytes;

/// parses a whole buffer that must be exactly one element; returns it
function DerRoot(const AData: TBytes): TRALASN1Element;

{ PEM (RFC 7468) }

/// '-----BEGIN <ALabel>-----', the base64 in lines of 64, '-----END ...', each
/// line ended by LF
function PemEncode(const ALabel: StringRAL; const ADER: TBytes): StringRAL;
/// the DER of the first block labelled ALabel ('' = the first block of any
/// label); ALabelFound receives the label. Empty when there is none
function PemDecode(const APEM: StringRAL; const ALabel: StringRAL;
  out ALabelFound: StringRAL): TBytes; overload;
function PemDecode(const APEM: StringRAL; const ALabel: StringRAL = ''): TBytes; overload;
/// whether the text holds at least one PEM block
function IsPem(const AText: StringRAL): boolean;

implementation

function Utf8Bytes(const AValue: StringRAL): TBytes;
begin
  SetLength(Result, Length(AValue));
  if Length(AValue) > 0 then
    Move(AValue[POSINISTR], Result[0], Length(AValue));
end;

function BytesUtf8(const AValue: TBytes): StringRAL;
begin
  SetLength(Result, Length(AValue));
  if Length(AValue) > 0 then
    Move(AValue[0], Result[POSINISTR], Length(AValue));
end;

function DerLength(ALength: IntegerRAL): TBytes;
var
  vCount, vInt: IntegerRAL;
  vValue: Cardinal;
begin
  if ALength < 128 then
  begin
    SetLength(Result, 1);
    Result[0] := ALength;
    Exit;
  end;

  vCount := 0;
  vValue := ALength;
  while vValue > 0 do
  begin
    Inc(vCount);
    vValue := vValue shr 8;
  end;

  SetLength(Result, vCount + 1);
  Result[0] := $80 or vCount;
  for vInt := 1 to vCount do
    Result[vInt] := Byte(Cardinal(ALength) shr ((vCount - vInt) * 8));
end;

function DerElement(ATag: Byte; const AContent: TBytes): TBytes;
var
  vLen: TBytes;
begin
  vLen := DerLength(Length(AContent));
  SetLength(Result, 1 + Length(vLen) + Length(AContent));
  Result[0] := ATag;
  Move(vLen[0], Result[1], Length(vLen));
  if Length(AContent) > 0 then
    Move(AContent[0], Result[1 + Length(vLen)], Length(AContent));
end;

function DerConcat(const AParts: array of TBytes): TBytes;
var
  vInt, vSize, vPos: IntegerRAL;
begin
  vSize := 0;
  for vInt := Low(AParts) to High(AParts) do
    Inc(vSize, Length(AParts[vInt]));

  SetLength(Result, vSize);
  vPos := 0;
  for vInt := Low(AParts) to High(AParts) do
  begin
    if Length(AParts[vInt]) > 0 then
      Move(AParts[vInt][0], Result[vPos], Length(AParts[vInt]));
    Inc(vPos, Length(AParts[vInt]));
  end;
end;

function DerSequence(const AParts: array of TBytes): TBytes;
begin
  Result := DerElement(ASN1_SEQUENCE, DerConcat(AParts));
end;

function CompareBytes(const A, B: TBytes): IntegerRAL;
var
  vInt, vLen: IntegerRAL;
begin
  vLen := Length(A);
  if Length(B) < vLen then
    vLen := Length(B);
  for vInt := 0 to vLen - 1 do
    if A[vInt] <> B[vInt] then
    begin
      Result := IntegerRAL(A[vInt]) - IntegerRAL(B[vInt]);
      Exit;
    end;
  Result := Length(A) - Length(B);
end;

function DerSet(const AParts: array of TBytes): TBytes;
var
  vSorted: TRALBytesArray;
  vI, vJ: IntegerRAL;
  vTemp: TBytes;
begin
  SetLength(vSorted, Length(AParts));
  for vI := 0 to High(vSorted) do
    vSorted[vI] := AParts[Low(AParts) + vI];

  for vI := 1 to High(vSorted) do
  begin
    vTemp := vSorted[vI];
    vJ := vI - 1;
    while (vJ >= 0) and (CompareBytes(vSorted[vJ], vTemp) > 0) do
    begin
      vSorted[vJ + 1] := vSorted[vJ];
      Dec(vJ);
    end;
    vSorted[vJ + 1] := vTemp;
  end;

  Result := DerElement(ASN1_SET, DerConcat(vSorted));
end;

function DerIntegerBytes(const AValue: TBytes): TBytes;
var
  vStart: IntegerRAL;
  vContent: TBytes;
begin
  { minimal: no leading zero bytes, except one when the top bit is set (the
    value is unsigned, and DER integers are two's complement) }
  vStart := 0;
  while (vStart < Length(AValue) - 1) and (AValue[vStart] = 0) do
    Inc(vStart);

  if Length(AValue) = 0 then
  begin
    SetLength(vContent, 1);
    vContent[0] := 0;
  end
  else if AValue[vStart] and $80 <> 0 then
  begin
    SetLength(vContent, Length(AValue) - vStart + 1);
    vContent[0] := 0;
    Move(AValue[vStart], vContent[1], Length(AValue) - vStart);
  end
  else
    vContent := Copy(AValue, vStart, Length(AValue) - vStart);

  Result := DerElement(ASN1_INTEGER, vContent);
end;

function DerInteger(const AValue: TRALBigNum): TBytes;
begin
  Result := DerIntegerBytes(BigToBytes(AValue, 1));
end;

function DerIntegerValue(AValue: Int64RAL): TBytes;
var
  vBytes: TBytes;
  vInt: IntegerRAL;
begin
  if AValue < 0 then
    raise ERALASN1.Create(emASN1Negative);

  SetLength(vBytes, 8);
  for vInt := 0 to 7 do
    vBytes[vInt] := Byte(AValue shr ((7 - vInt) * 8));
  Result := DerIntegerBytes(vBytes);
end;

function DerBoolean(AValue: boolean): TBytes;
var
  vContent: TBytes;
begin
  SetLength(vContent, 1);
  if AValue then
    vContent[0] := $FF
  else
    vContent[0] := 0;
  Result := DerElement(ASN1_BOOLEAN, vContent);
end;

function DerNull: TBytes;
begin
  Result := DerElement(ASN1_NULL, nil);
end;

function DerOID(const ADotted: StringRAL): TBytes;
var
  vArcs: array of Int64RAL;
  vInt, vPos, vCount: IntegerRAL;
  vContent: TBytes;
  vValue: Int64RAL;
  vChar: AnsiChar;

  procedure AddArc(AValue: Int64RAL);
  var
    vTemp: TBytes;
    vLen, vIdx: IntegerRAL;
  begin
    { base 128, most significant group first, bit 8 set on all but the last }
    SetLength(vTemp, 10);
    vLen := 0;
    repeat
      vTemp[vLen] := AValue and $7F;
      AValue := AValue shr 7;
      Inc(vLen);
    until AValue = 0;

    vPos := Length(vContent);
    SetLength(vContent, vPos + vLen);
    for vIdx := 0 to vLen - 1 do
    begin
      vContent[vPos + vIdx] := vTemp[vLen - 1 - vIdx];
      if vIdx < vLen - 1 then
        vContent[vPos + vIdx] := vContent[vPos + vIdx] or $80;
    end;
  end;

begin
  SetLength(vArcs, 0);
  vCount := 0;
  vValue := 0;
  for vInt := POSINISTR to Length(ADotted) - 1 + POSINISTR do
  begin
    vChar := ADotted[vInt];
    if vChar = '.' then
    begin
      SetLength(vArcs, vCount + 1);
      vArcs[vCount] := vValue;
      Inc(vCount);
      vValue := 0;
    end
    else if (vChar >= '0') and (vChar <= '9') then
      vValue := vValue * 10 + (Ord(vChar) - Ord('0'))
    else
      raise ERALASN1.CreateFmt(emASN1InvalidOID, [string(ADotted)]);
  end;
  SetLength(vArcs, vCount + 1);
  vArcs[vCount] := vValue;
  Inc(vCount);

  if (vCount < 2) or (vArcs[0] > 2) then
    raise ERALASN1.CreateFmt(emASN1InvalidOID, [string(ADotted)]);

  vContent := nil;
  AddArc(vArcs[0] * 40 + vArcs[1]);
  for vInt := 2 to vCount - 1 do
    AddArc(vArcs[vInt]);

  Result := DerElement(ASN1_OID, vContent);
end;

function DerOctetString(const AValue: TBytes): TBytes;
begin
  Result := DerElement(ASN1_OCTET_STRING, AValue);
end;

function DerBitString(const AValue: TBytes): TBytes;
var
  vContent: TBytes;
begin
  SetLength(vContent, Length(AValue) + 1);
  vContent[0] := 0;
  if Length(AValue) > 0 then
    Move(AValue[0], vContent[1], Length(AValue));
  Result := DerElement(ASN1_BIT_STRING, vContent);
end;

function DerNamedBits(ABits: Cardinal): TBytes;
var
  vContent: TBytes;
  vHigh, vBytes, vInt, vUnused: IntegerRAL;
begin
  { the highest bit number in use }
  vHigh := -1;
  for vInt := 0 to 31 do
    if (ABits shr vInt) and 1 = 1 then
      vHigh := vInt;

  if vHigh < 0 then
  begin
    SetLength(vContent, 1);
    vContent[0] := 0;
    Result := DerElement(ASN1_BIT_STRING, vContent);
    Exit;
  end;

  vBytes := vHigh div 8 + 1;
  vUnused := vBytes * 8 - (vHigh + 1);
  SetLength(vContent, vBytes + 1);
  for vInt := 0 to vBytes do
    vContent[vInt] := 0;
  vContent[0] := vUnused;
  for vInt := 0 to vHigh do
    if (ABits shr vInt) and 1 = 1 then
      vContent[1 + vInt div 8] := vContent[1 + vInt div 8] or ($80 shr (vInt mod 8));
  Result := DerElement(ASN1_BIT_STRING, vContent);
end;

function DerUTF8String(const AValue: StringRAL): TBytes;
begin
  Result := DerElement(ASN1_UTF8_STRING, Utf8Bytes(AValue));
end;

function DerPrintableString(const AValue: StringRAL): TBytes;
begin
  Result := DerElement(ASN1_PRINTABLE_STRING, Utf8Bytes(AValue));
end;

function DerIA5String(const AValue: StringRAL): TBytes;
begin
  Result := DerElement(ASN1_IA5_STRING, Utf8Bytes(AValue));
end;

function DerBMPString(const AValue: StringRAL): TBytes;
var
  vWide: UnicodeString;
  vContent: TBytes;
  vInt: IntegerRAL;
begin
  vWide := UnicodeString(AValue);
  SetLength(vContent, Length(vWide) * 2);
  for vInt := 1 to Length(vWide) do
  begin
    vContent[(vInt - 1) * 2] := Byte(Ord(vWide[vInt]) shr 8);
    vContent[(vInt - 1) * 2 + 1] := Byte(Ord(vWide[vInt]));
  end;
  Result := DerElement(ASN1_BMP_STRING, vContent);
end;

function DerTime(AValueUTC: TDateTime): TBytes;
var
  vText: string;
  vYear, vMonth, vDay, vHour, vMin, vSec, vMSec: Word;
begin
  DecodeDate(AValueUTC, vYear, vMonth, vDay);
  DecodeTime(AValueUTC, vHour, vMin, vSec, vMSec);
  if (vYear >= 1950) and (vYear < 2050) then
  begin
    vText := Format('%.2d%.2d%.2d%.2d%.2d%.2dZ', [vYear mod 100, vMonth, vDay,
      vHour, vMin, vSec]);
    Result := DerElement(ASN1_UTC_TIME, Utf8Bytes(StringRAL(vText)));
  end
  else
  begin
    vText := Format('%.4d%.2d%.2d%.2d%.2d%.2dZ', [vYear, vMonth, vDay, vHour,
      vMin, vSec]);
    Result := DerElement(ASN1_GENERALIZED_TIME, Utf8Bytes(StringRAL(vText)));
  end;
end;

function DerExplicit(ATagNo: Byte; const AContent: TBytes): TBytes;
begin
  Result := DerElement(ASN1_CONTEXT_CONSTRUCTED or ATagNo, AContent);
end;

function DerImplicit(ATagNo: Byte; const AContent: TBytes): TBytes;
begin
  Result := DerElement(ASN1_CONTEXT or ATagNo, AContent);
end;

function DerAlgorithm(const AOID: StringRAL; const AParams: TBytes): TBytes;
begin
  Result := DerSequence([DerOID(AOID), AParams]);
end;

{ reader }

function DerRead(const AData: TBytes; APos: IntegerRAL; ALimit: IntegerRAL): TRALASN1Element;
var
  vLen, vCount, vInt: IntegerRAL;
begin
  if (ALimit < 0) or (ALimit > Length(AData)) then
    ALimit := Length(AData);

  if APos + 2 > ALimit then
    raise ERALASN1.Create(emASN1Truncated);

  Result.Start := APos;
  Result.Tag := AData[APos];
  if Result.Tag and $1F = $1F then
    raise ERALASN1.Create(emASN1Unsupported);

  Inc(APos);
  vLen := AData[APos];
  Inc(APos);
  if vLen and $80 <> 0 then
  begin
    vCount := vLen and $7F;
    { no indefinite length (0) in DER, and nothing here is 2 GB }
    if (vCount = 0) or (vCount > 4) then
      raise ERALASN1.Create(emASN1Unsupported);
    if APos + vCount > ALimit then
      raise ERALASN1.Create(emASN1Truncated);
    vLen := 0;
    for vInt := 1 to vCount do
    begin
      if vLen > $7FFFFF then
        raise ERALASN1.Create(emASN1Truncated);
      vLen := (vLen shl 8) or AData[APos];
      Inc(APos);
    end;
  end;

  if (vLen < 0) or (APos + vLen > ALimit) then
    raise ERALASN1.Create(emASN1Truncated);

  Result.ContentStart := APos;
  Result.ContentLength := vLen;
  Result.Next := APos + vLen;
end;

function DerExpect(const AData: TBytes; APos: IntegerRAL; ATag: Byte;
  ALimit: IntegerRAL): TRALASN1Element;
begin
  Result := DerRead(AData, APos, ALimit);
  if Result.Tag <> ATag then
    raise ERALASN1.CreateFmt(emASN1UnexpectedTag, [ATag, Result.Tag, APos]);
end;

function DerChildren(const AData: TBytes; const AParent: TRALASN1Element): TRALASN1Element;
begin
  Result := DerRead(AData, AParent.ContentStart,
    AParent.ContentStart + AParent.ContentLength);
end;

function DerItems(const AData: TBytes; const AParent: TRALASN1Element): TRALBytesArray;
var
  vPos, vEnd, vCount: IntegerRAL;
  vElem: TRALASN1Element;
begin
  SetLength(Result, 0);
  vCount := 0;
  vPos := AParent.ContentStart;
  vEnd := AParent.ContentStart + AParent.ContentLength;
  while vPos < vEnd do
  begin
    vElem := DerRead(AData, vPos, vEnd);
    SetLength(Result, vCount + 1);
    Result[vCount] := DerBytes(AData, vElem);
    Inc(vCount);
    vPos := vElem.Next;
  end;
end;

function DerBytes(const AData: TBytes; const AElement: TRALASN1Element): TBytes;
begin
  Result := Copy(AData, AElement.Start, AElement.Next - AElement.Start);
end;

function DerContent(const AData: TBytes; const AElement: TRALASN1Element): TBytes;
begin
  Result := Copy(AData, AElement.ContentStart, AElement.ContentLength);
end;

function DerAsBigNum(const AData: TBytes; const AElement: TRALASN1Element): TRALBigNum;
begin
  if AElement.Tag <> ASN1_INTEGER then
    raise ERALASN1.CreateFmt(emASN1UnexpectedTag, [ASN1_INTEGER, AElement.Tag,
      AElement.Start]);
  if (AElement.ContentLength > 0) and (AData[AElement.ContentStart] and $80 <> 0) then
    raise ERALASN1.Create(emASN1Negative);
  Result := BigFromBytes(DerContent(AData, AElement));
end;

function DerAsInt64(const AData: TBytes; const AElement: TRALASN1Element): Int64RAL;
var
  vInt: IntegerRAL;
begin
  if AElement.Tag <> ASN1_INTEGER then
    raise ERALASN1.CreateFmt(emASN1UnexpectedTag, [ASN1_INTEGER, AElement.Tag,
      AElement.Start]);
  if AElement.ContentLength > 8 then
    raise ERALASN1.Create(emASN1Unsupported);
  if (AElement.ContentLength > 0) and (AData[AElement.ContentStart] and $80 <> 0) then
    raise ERALASN1.Create(emASN1Negative);

  Result := 0;
  for vInt := 0 to AElement.ContentLength - 1 do
    Result := (Result shl 8) or AData[AElement.ContentStart + vInt];
end;

function DerAsOID(const AData: TBytes; const AElement: TRALASN1Element): StringRAL;
var
  vInt: IntegerRAL;
  vValue: Int64RAL;
  vFirst: boolean;
  vByte: Byte;
begin
  if AElement.Tag <> ASN1_OID then
    raise ERALASN1.CreateFmt(emASN1UnexpectedTag, [ASN1_OID, AElement.Tag,
      AElement.Start]);

  Result := '';
  vValue := 0;
  vFirst := True;
  for vInt := 0 to AElement.ContentLength - 1 do
  begin
    vByte := AData[AElement.ContentStart + vInt];
    vValue := (vValue shl 7) or (vByte and $7F);
    if vByte and $80 = 0 then
    begin
      if vFirst then
      begin
        if vValue < 80 then
          Result := StringRAL(IntToStr(vValue div 40) + '.' + IntToStr(vValue mod 40))
        else
          Result := StringRAL('2.' + IntToStr(vValue - 80));
        vFirst := False;
      end
      else
        Result := Result + '.' + StringRAL(IntToStr(vValue));
      vValue := 0;
    end;
  end;
end;

function DerAsString(const AData: TBytes; const AElement: TRALASN1Element): StringRAL;
var
  vWide: UnicodeString;
  vInt: IntegerRAL;
begin
  if AElement.Tag = ASN1_BMP_STRING then
  begin
    SetLength(vWide, AElement.ContentLength div 2);
    for vInt := 1 to Length(vWide) do
      vWide[vInt] := WideChar((Word(AData[AElement.ContentStart + (vInt - 1) * 2]) shl 8)
        or AData[AElement.ContentStart + (vInt - 1) * 2 + 1]);
    Result := StringRAL(vWide);
  end
  else
    Result := BytesUtf8(DerContent(AData, AElement));
end;

function DerAsTime(const AData: TBytes; const AElement: TRALASN1Element): TDateTime;
var
  vText: string;
  vYear, vPos: IntegerRAL;

  function Num(AStart, ALen: IntegerRAL): IntegerRAL;
  begin
    Result := StrToIntDef(Copy(vText, AStart, ALen), -1);
    if Result < 0 then
      raise ERALASN1.CreateFmt(emASN1InvalidTime, [vText]);
  end;

begin
  vText := string(BytesUtf8(DerContent(AData, AElement)));
  if AElement.Tag = ASN1_UTC_TIME then
  begin
    vYear := Num(1, 2);
    if vYear >= 50 then
      vYear := 1900 + vYear
    else
      vYear := 2000 + vYear;
    vPos := 3;
  end
  else if AElement.Tag = ASN1_GENERALIZED_TIME then
  begin
    vYear := Num(1, 4);
    vPos := 5;
  end
  else
    raise ERALASN1.CreateFmt(emASN1UnexpectedTag, [ASN1_UTC_TIME, AElement.Tag,
      AElement.Start]);

  { DER times are always YY[YY]MMDDHHMMSSZ }
  if (Length(vText) < vPos + 10) or (vText[Length(vText)] <> 'Z') then
    raise ERALASN1.CreateFmt(emASN1InvalidTime, [vText]);

  Result := EncodeDate(vYear, Num(vPos, 2), Num(vPos + 2, 2)) +
    EncodeTime(Num(vPos + 4, 2), Num(vPos + 6, 2), Num(vPos + 8, 2), 0);
end;

function DerAsBitString(const AData: TBytes; const AElement: TRALASN1Element): TBytes;
begin
  if AElement.Tag <> ASN1_BIT_STRING then
    raise ERALASN1.CreateFmt(emASN1UnexpectedTag, [ASN1_BIT_STRING, AElement.Tag,
      AElement.Start]);
  if AElement.ContentLength < 1 then
    raise ERALASN1.Create(emASN1Truncated);
  Result := Copy(AData, AElement.ContentStart + 1, AElement.ContentLength - 1);
end;

function DerRoot(const AData: TBytes): TRALASN1Element;
begin
  Result := DerRead(AData, 0);
  if Result.Next <> Length(AData) then
    raise ERALASN1.Create(emASN1Trailing);
end;

function PemEncode(const ALabel: StringRAL; const ADER: TBytes): StringRAL;
var
  vB64: StringRAL;
  vPos: IntegerRAL;
begin
  vB64 := TRALBase64.Encode(ADER);
  Result := '-----BEGIN ' + ALabel + '-----'#10;
  vPos := 0;
  while vPos < Length(vB64) do
  begin
    Result := Result + Copy(vB64, vPos + 1, 64) + #10;
    Inc(vPos, 64);
  end;
  Result := Result + '-----END ' + ALabel + '-----'#10;
end;

{ 1-based position of ASub in AText from AFrom on, 0 when absent; bytes, not
  characters - Pos over string() would count UTF-16 units on Delphi }
function FindFrom(const ASub, AText: StringRAL; AFrom: IntegerRAL): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  if AFrom < 1 then
    AFrom := 1;
  for vInt := AFrom to Length(AText) - Length(ASub) + 1 do
    if CompareMem(@AText[vInt - 1 + POSINISTR], @ASub[POSINISTR], Length(ASub)) then
    begin
      Result := vInt;
      Exit;
    end;
end;

function PemDecode(const APEM: StringRAL; const ALabel: StringRAL;
  out ALabelFound: StringRAL): TBytes;
const
  cBegin: StringRAL = '-----BEGIN ';
  cDashes: StringRAL = '-----';
var
  vPos, vLabelEnd, vEnd, vInt, vLen: IntegerRAL;
  vLabel, vBody, vClean: StringRAL;
  vChar: AnsiChar;
begin
  Result := nil;
  ALabelFound := '';
  vPos := 1;
  while True do
  begin
    vPos := FindFrom(cBegin, APEM, vPos);
    if vPos = 0 then
      Exit;

    vLabelEnd := FindFrom(cDashes, APEM, vPos + Length(cBegin));
    if vLabelEnd = 0 then
      Exit;
    vLabel := Copy(APEM, vPos + Length(cBegin), vLabelEnd - vPos - Length(cBegin));
    vEnd := FindFrom('-----END ' + vLabel + cDashes, APEM, vLabelEnd);
    if vEnd = 0 then
      Exit;

    if (ALabel = '') or (vLabel = ALabel) then
    begin
      vBody := Copy(APEM, vLabelEnd + Length(cDashes), vEnd - vLabelEnd - Length(cDashes));
      { headers (Proc-Type, DEK-Info) mean an encrypted legacy key: not read }
      if FindFrom(':', vBody, 1) > 0 then
        Exit;
      SetLength(vClean, Length(vBody));
      vLen := 0;
      for vInt := POSINISTR to Length(vBody) - 1 + POSINISTR do
      begin
        vChar := vBody[vInt];
        if not ((vChar = ' ') or (vChar = #9) or (vChar = #10) or (vChar = #13)) then
        begin
          vClean[vLen + POSINISTR] := vChar;
          Inc(vLen);
        end;
      end;
      SetLength(vClean, vLen);
      Result := TRALBase64.DecodeAsBytes(vClean);
      ALabelFound := vLabel;
      Exit;
    end;
    vPos := vEnd + 1;
  end;
end;

function PemDecode(const APEM: StringRAL; const ALabel: StringRAL): TBytes;
var
  vFound: StringRAL;
begin
  Result := PemDecode(APEM, ALabel, vFound);
end;

function IsPem(const AText: StringRAL): boolean;
begin
  Result := FindFrom('-----BEGIN ', AText, 1) > 0;
end;

end.
