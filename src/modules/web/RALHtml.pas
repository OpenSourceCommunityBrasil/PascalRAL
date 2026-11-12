/// Escaping of text for HTML, attributes, JavaScript and URLs, and an HTML sanitizer.
unit RALHtml;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes;

{ Escapes the five HTML special characters (& < > " ') as entities, for text
  placed between tags or inside a quoted attribute. }
function RALHtmlEscape(const AText: StringRAL): StringRAL;
{ Escapes every ASCII character but letters and digits as &#xHH;, for a value
  placed in an attribute, quoted or not. Bytes of UTF-8 sequences are kept. }
function RALHtmlAttrEscape(const AText: StringRAL): StringRAL;
{ Escapes every ASCII character but letters and digits as \xHH, and U+2028 and
  U+2029 as \uHHHH, for a value placed inside a quoted JavaScript string. }
function RALJsEscape(const AText: StringRAL): StringRAL;
{ Percent-encodes every byte but letters, digits and - _ . ~ (RFC 3986
  unreserved), for a value placed in a URL path segment or query. }
function RALUrlEscape(const AText: StringRAL): StringRAL;

type
  { Cleans HTML that came from users with an allow-list: only the listed tags and
    attributes survive, URLs keep only the listed schemes, and the output is
    well-formed with every text and value escaped. Reading is thread-safe as long
    as nobody changes the lists meanwhile. }
  TRALHtmlSanitizer = class(TPersistent)
  private
    FAllowedAttributes: TStrings;
    FAllowedSchemes: TStrings;
    FAllowedTags: TStrings;
    FDropContentTags: TStrings;

    procedure SetAllowedAttributes(const AValue: TStrings);
    procedure SetAllowedSchemes(const AValue: TStrings);
    procedure SetAllowedTags(const AValue: TStrings);
    procedure SetDropContentTags(const AValue: TStrings);
  public
    /// Creates the sanitizer with a safe default policy (no style, no class).
    constructor Create;
    destructor Destroy; override;

    /// Copies the policy of another sanitizer.
    procedure Assign(ASource: TPersistent); override;
    /// Returns AHtml reduced to what the policy allows.
    function Sanitize(const AHtml: StringRAL): StringRAL;

    { Attributes kept, as "attr" for any allowed tag or "tag.attr" for one tag.
      An on* attribute is never kept, whatever is listed. }
    property AllowedAttributes: TStrings read FAllowedAttributes write SetAllowedAttributes;
    /// URL schemes a link may use (without the colon); relative URLs are always kept.
    property AllowedSchemes: TStrings read FAllowedSchemes write SetAllowedSchemes;
    /// Names of the tags that are kept.
    property AllowedTags: TStrings read FAllowedTags write SetAllowedTags;
    /// Names of the tags dropped together with everything inside them.
    property DropContentTags: TStrings read FDropContentTags write SetDropContentTags;
  end;

/// Sanitizes AHtml with the default policy of TRALHtmlSanitizer.
function RALSanitizeHtml(const AHtml: StringRAL): StringRAL;

implementation

const
  /// Hexadecimal digits, upper case.
  cHexDigits: array[0..15] of AnsiChar = '0123456789ABCDEF';

/// Whether AChar is an ASCII letter or digit.
function IsAlphaNum(AChar: AnsiChar): Boolean;
begin
  Result := ((AChar >= 'a') and (AChar <= 'z')) or ((AChar >= 'A') and (AChar <= 'Z')) or
    ((AChar >= '0') and (AChar <= '9'));
end;

/// Appends AValue to ABuffer at AIndex, growing ABuffer when needed.
procedure Put(var ABuffer: StringRAL; var AIndex: IntegerRAL; const AValue: StringRAL);
var
  vLen: IntegerRAL;
begin
  vLen := Length(AValue);
  if vLen = 0 then
    Exit;
  if AIndex + vLen - 1 > Length(ABuffer) then
    SetLength(ABuffer, (Length(ABuffer) + vLen) * 2);
  Move(AValue[1], ABuffer[AIndex], vLen);
  Inc(AIndex, vLen);
end;

/// Appends one byte to ABuffer at AIndex, growing ABuffer when needed.
procedure PutChar(var ABuffer: StringRAL; var AIndex: IntegerRAL; AChar: AnsiChar);
begin
  if AIndex > Length(ABuffer) then
    SetLength(ABuffer, (Length(ABuffer) + 1) * 2);
  ABuffer[AIndex] := AChar;
  Inc(AIndex);
end;

/// The two hexadecimal digits of AChar.
function HexOf(AChar: AnsiChar): StringRAL;
begin
  Result := '';
  SetLength(Result, 2);
  Result[1] := cHexDigits[Ord(AChar) shr 4];
  Result[2] := cHexDigits[Ord(AChar) and 15];
end;

function RALHtmlEscape(const AText: StringRAL): StringRAL;
var
  vInt, vOut: IntegerRAL;
begin
  Result := '';
  vOut := 1;
  SetLength(Result, Length(AText) + 16);
  for vInt := 1 to Length(AText) do
    case AText[vInt] of
      '&': Put(Result, vOut, '&amp;');
      '<': Put(Result, vOut, '&lt;');
      '>': Put(Result, vOut, '&gt;');
      '"': Put(Result, vOut, '&quot;');
      '''': Put(Result, vOut, '&#39;');
    else
      PutChar(Result, vOut, AText[vInt]);
    end;
  SetLength(Result, vOut - 1);
end;

function RALHtmlAttrEscape(const AText: StringRAL): StringRAL;
var
  vInt, vOut: IntegerRAL;
  vChar: AnsiChar;
begin
  Result := '';
  vOut := 1;
  SetLength(Result, Length(AText) + 16);
  for vInt := 1 to Length(AText) do
  begin
    vChar := AText[vInt];
    if IsAlphaNum(vChar) or (Ord(vChar) >= $80) then
      PutChar(Result, vOut, vChar)
    else
      Put(Result, vOut, '&#x' + HexOf(vChar) + ';');
  end;
  SetLength(Result, vOut - 1);
end;

function RALJsEscape(const AText: StringRAL): StringRAL;
var
  vInt, vOut, vLen: IntegerRAL;
  vChar: AnsiChar;
begin
  Result := '';
  vOut := 1;
  vLen := Length(AText);
  SetLength(Result, vLen + 16);
  vInt := 1;
  while vInt <= vLen do
  begin
    vChar := AText[vInt];
    // U+2028 and U+2029 end a line inside a script: E2 80 A8 / E2 80 A9.
    if (Ord(vChar) = $E2) and (vInt + 2 <= vLen) and (Ord(AText[vInt + 1]) = $80) and
      ((Ord(AText[vInt + 2]) = $A8) or (Ord(AText[vInt + 2]) = $A9)) then
    begin
      if Ord(AText[vInt + 2]) = $A8 then
        Put(Result, vOut, '\u2028')
      else
        Put(Result, vOut, '\u2029');
      Inc(vInt, 3);
      Continue;
    end;
    if IsAlphaNum(vChar) or (Ord(vChar) >= $80) then
      PutChar(Result, vOut, vChar)
    else
      Put(Result, vOut, '\x' + HexOf(vChar));
    Inc(vInt);
  end;
  SetLength(Result, vOut - 1);
end;

function RALUrlEscape(const AText: StringRAL): StringRAL;
var
  vInt, vOut: IntegerRAL;
  vChar: AnsiChar;
begin
  Result := '';
  vOut := 1;
  SetLength(Result, Length(AText) + 16);
  for vInt := 1 to Length(AText) do
  begin
    vChar := AText[vInt];
    if IsAlphaNum(vChar) or (vChar = '-') or (vChar = '_') or (vChar = '.') or
      (vChar = '~') then
      PutChar(Result, vOut, vChar)
    else
      Put(Result, vOut, '%' + HexOf(vChar));
  end;
  SetLength(Result, vOut - 1);
end;

const
  /// Tags kept by the default policy.
  cDefaultTags = 'a,b,i,em,strong,u,s,p,br,ul,ol,li,blockquote,code,pre,h1,h2,h3,h4,h5,h6,' +
    'span,div,img,table,thead,tbody,tfoot,tr,th,td,hr,sub,sup,caption';
  /// Attributes kept by the default policy.
  cDefaultAttributes = 'title,a.href,img.src,img.alt,img.width,img.height,td.colspan,' +
    'td.rowspan,th.colspan,th.rowspan';
  /// Tags dropped with their content by the default policy.
  cDefaultDrop = 'script,style,iframe,object,embed,template,noscript,svg,math,textarea,' +
    'select,title,head';
  /// Elements that have no closing tag.
  cVoidTags = ',area,base,br,col,embed,hr,img,input,link,meta,source,track,wbr,';
  /// Attributes that hold a URL.
  cUrlAttrs = ',href,src,action,formaction,xlink:href,background,cite,poster,';

/// Whether AChar separates tokens inside a tag.
function IsBlank(AChar: AnsiChar): Boolean;
begin
  Result := Ord(AChar) in [9, 10, 12, 13, 32];
end;

/// Whether AChar is an ASCII letter.
function IsLetter(AChar: AnsiChar): Boolean;
begin
  Result := ((AChar >= 'a') and (AChar <= 'z')) or ((AChar >= 'A') and (AChar <= 'Z'));
end;

/// Whether AChar may be part of a tag name.
function IsNameChar(AChar: AnsiChar): Boolean;
begin
  Result := IsAlphaNum(AChar) or (AChar = '-') or (AChar = ':') or (AChar = '_');
end;

/// Lower case of an ASCII letter, other bytes unchanged.
function LowerChar(AChar: AnsiChar): AnsiChar;
begin
  if (AChar >= 'A') and (AChar <= 'Z') then
    Result := AnsiChar(Ord(AChar) + 32)
  else
    Result := AChar;
end;

/// AText with ASCII letters in lower case.
function AsciiLower(const AText: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := AText;
  for vInt := 1 to Length(Result) do
    Result[vInt] := LowerChar(Result[vInt]);
end;

/// Where ALower (lower case) starts in AText from AFrom, ASCII case ignored, or 0.
function FindText(const ALower, AText: StringRAL; AFrom: IntegerRAL): IntegerRAL;
var
  vInt, vSub, vLen: IntegerRAL;
begin
  Result := 0;
  vLen := Length(ALower);
  vInt := AFrom;
  while vInt + vLen - 1 <= Length(AText) do
  begin
    vSub := 0;
    while (vSub < vLen) and (LowerChar(AText[vInt + vSub]) = ALower[vSub + 1]) do
      Inc(vSub);
    if vSub = vLen then
    begin
      Result := vInt;
      Exit;
    end;
    Inc(vInt);
  end;
end;

/// Whether AList holds ALower (list entries compared in lower case).
function ListHas(AList: TStrings; const ALower: StringRAL): Boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 0 to AList.Count - 1 do
    if AsciiLower(StringRAL(AList[vInt])) = ALower then
    begin
      Result := True;
      Exit;
    end;
end;

/// Appends the UTF-8 form of the code point ACode to ABuffer.
procedure PutUtf8(var ABuffer: StringRAL; var AIndex: IntegerRAL; ACode: Cardinal);
begin
  if ACode < $80 then
    PutChar(ABuffer, AIndex, AnsiChar(ACode))
  else if ACode < $800 then
  begin
    PutChar(ABuffer, AIndex, AnsiChar($C0 or (ACode shr 6)));
    PutChar(ABuffer, AIndex, AnsiChar($80 or (ACode and $3F)));
  end
  else if ACode < $10000 then
  begin
    PutChar(ABuffer, AIndex, AnsiChar($E0 or (ACode shr 12)));
    PutChar(ABuffer, AIndex, AnsiChar($80 or ((ACode shr 6) and $3F)));
    PutChar(ABuffer, AIndex, AnsiChar($80 or (ACode and $3F)));
  end
  else
  begin
    PutChar(ABuffer, AIndex, AnsiChar($F0 or (ACode shr 18)));
    PutChar(ABuffer, AIndex, AnsiChar($80 or ((ACode shr 12) and $3F)));
    PutChar(ABuffer, AIndex, AnsiChar($80 or ((ACode shr 6) and $3F)));
    PutChar(ABuffer, AIndex, AnsiChar($80 or (ACode and $3F)));
  end;
end;

/// Value of a hexadecimal or decimal digit, or -1.
function DigitOf(AChar: AnsiChar; AHex: Boolean): Integer;
begin
  Result := -1;
  if (AChar >= '0') and (AChar <= '9') then
    Result := Ord(AChar) - Ord('0')
  else if AHex and (AChar >= 'a') and (AChar <= 'f') then
    Result := Ord(AChar) - Ord('a') + 10
  else if AHex and (AChar >= 'A') and (AChar <= 'F') then
    Result := Ord(AChar) - Ord('A') + 10;
end;

{ Reads the character reference that starts at S[AIndex] (an ampersand). Returns 1
  with ACode and ALen when it is a known reference (numeric, with or without the
  semicolon, or one of a few named ones), 2 with ALen for a well-formed unknown
  named one, and 0 when it is not a reference. }
function ReadEntity(const S: StringRAL; AIndex: IntegerRAL; out ACode: Cardinal;
  out ALen: IntegerRAL): Integer;
var
  vPos, vStart, vDigit: IntegerRAL;
  vHex, vSemi: Boolean;
  vName: StringRAL;
begin
  Result := 0;
  ACode := 0;
  ALen := 1;
  vPos := AIndex + 1;
  if vPos > Length(S) then
    Exit;
  if S[vPos] = '#' then
  begin
    Inc(vPos);
    vHex := (vPos <= Length(S)) and ((S[vPos] = 'x') or (S[vPos] = 'X'));
    if vHex then
      Inc(vPos);
    vStart := vPos;
    while (vPos <= Length(S)) and (DigitOf(S[vPos], vHex) >= 0) do
    begin
      vDigit := DigitOf(S[vPos], vHex);
      if ACode <= $10FFFF then
      begin
        if vHex then
          ACode := ACode * 16 + Cardinal(vDigit)
        else
          ACode := ACode * 10 + Cardinal(vDigit);
      end;
      Inc(vPos);
    end;
    if vPos = vStart then
      Exit;
    if (vPos <= Length(S)) and (S[vPos] = ';') then
      Inc(vPos);
    if (ACode = 0) or (ACode > $10FFFF) or ((ACode >= $D800) and (ACode <= $DFFF)) then
      ACode := $FFFD;
    ALen := vPos - AIndex;
    Result := 1;
    Exit;
  end;
  vStart := vPos;
  while (vPos <= Length(S)) and (vPos - vStart < 32) and IsAlphaNum(S[vPos]) do
    Inc(vPos);
  if (vPos = vStart) or not IsLetter(S[vStart]) then
    Exit;
  vName := Copy(S, vStart, vPos - vStart);
  vSemi := (vPos <= Length(S)) and (S[vPos] = ';');
  ALen := vPos - AIndex;
  if vSemi then
    Inc(ALen);
  Result := 1;
  if (vName = 'amp') or (vName = 'AMP') then
    ACode := Ord('&')
  else if (vName = 'lt') or (vName = 'LT') then
    ACode := Ord('<')
  else if (vName = 'gt') or (vName = 'GT') then
    ACode := Ord('>')
  else if (vName = 'quot') or (vName = 'QUOT') then
    ACode := Ord('"')
  else if not vSemi then
    Result := 0
  else if vName = 'apos' then
    ACode := 39
  else if vName = 'nbsp' then
    ACode := $A0
  else if vName = 'Tab' then
    ACode := 9
  else if vName = 'NewLine' then
    ACode := 10
  else if vName = 'colon' then
    ACode := Ord(':')
  else if vName = 'sol' then
    ACode := Ord('/')
  else if vName = 'lpar' then
    ACode := Ord('(')
  else if vName = 'rpar' then
    ACode := Ord(')')
  else
    Result := 2;
end;

{ Decodes the known character references of AText and escapes the result for text or
  a double-quoted value. An unknown named reference stays as written. With
  ADropControl the bytes below 32 are removed. }
function EscapeDecoded(const AText: StringRAL; ADropControl: Boolean): StringRAL;
var
  vInt, vOut, vPend, vLen: IntegerRAL;
  vPlain: StringRAL;
  vKind: Integer;
  vCode: Cardinal;

  procedure Flush;
  begin
    if vPend > 1 then
      Put(Result, vOut, RALHtmlEscape(Copy(vPlain, 1, vPend - 1)));
    vPend := 1;
  end;

begin
  Result := '';
  vPlain := '';
  vOut := 1;
  vPend := 1;
  SetLength(Result, Length(AText) + 16);
  SetLength(vPlain, Length(AText) + 16);
  vInt := 1;
  while vInt <= Length(AText) do
  begin
    if AText[vInt] = '&' then
    begin
      vKind := ReadEntity(AText, vInt, vCode, vLen);
      if vKind = 1 then
      begin
        if not (ADropControl and (vCode < 32)) then
          PutUtf8(vPlain, vPend, vCode);
        Inc(vInt, vLen);
        Continue;
      end;
      if vKind = 2 then
      begin
        Flush;
        Put(Result, vOut, Copy(AText, vInt, vLen));
        Inc(vInt, vLen);
        Continue;
      end;
    end;
    if not (ADropControl and (Ord(AText[vInt]) < 32)) then
      PutChar(vPlain, vPend, AText[vInt]);
    Inc(vInt);
  end;
  Flush;
  SetLength(Result, vOut - 1);
end;

/// AValue as a browser reads a scheme: references decoded, blanks and controls removed.
function DecodedForCheck(const AValue: StringRAL): StringRAL;
var
  vInt, vOut, vLen: IntegerRAL;
  vCode: Cardinal;
begin
  Result := '';
  vOut := 1;
  SetLength(Result, Length(AValue) + 16);
  vInt := 1;
  while vInt <= Length(AValue) do
  begin
    if (AValue[vInt] = '&') and (ReadEntity(AValue, vInt, vCode, vLen) = 1) then
    begin
      if vCode > 32 then
        PutUtf8(Result, vOut, vCode);
      Inc(vInt, vLen);
      Continue;
    end;
    if Ord(AValue[vInt]) > 32 then
      PutChar(Result, vOut, AValue[vInt]);
    Inc(vInt);
  end;
  SetLength(Result, vOut - 1);
end;

type
  /// One run of the sanitizer over a text.
  TSanRun = class
  private
    /// Output buffer.
    FBuf: StringRAL;
    /// Depth of the open tag stack.
    FDepth: IntegerRAL;
    /// Length of the source text.
    FLen: IntegerRAL;
    /// Characters written to the buffer.
    FOut: IntegerRAL;
    /// Sanitizer whose rules apply.
    FOwner: TRALHtmlSanitizer;
    /// Read position in the source.
    FPos: IntegerRAL;
    /// Source text.
    FSrc: StringRAL;
    /// Open tags.
    FStack: array of StringRAL;

    /// True when the tag may carry the attribute.
    function AttributeAllowed(const ATag, AName: StringRAL): Boolean;
    /// Handles a closing tag.
    procedure CloseTag;
    /// Appends text to the output.
    procedure Emit(const AValue: StringRAL);
    /// Handles an opening tag.
    procedure OpenTag;
    /// True when the URL scheme is allowed.
    function SafeUrl(const AValue: StringRAL): Boolean;
    /// Skips the source up to AEnd.
    procedure SkipTo(const AEnd: StringRAL);
    /// Writes an allowed tag with its attributes.
    procedure WriteTag(const ATag: StringRAL; const ANames, AValues: array of StringRAL;
      ACount: IntegerRAL);
  public
    /// Sanitizes AHtml for AOwner and returns the result.
    function Run(AOwner: TRALHtmlSanitizer; const AHtml: StringRAL): StringRAL;
  end;

procedure TSanRun.Emit(const AValue: StringRAL);
begin
  Put(FBuf, FOut, AValue);
end;

function TSanRun.AttributeAllowed(const ATag, AName: StringRAL): Boolean;
begin
  Result := (Copy(AName, 1, 2) <> 'on') and (AName <> 'srcset') and
    (ListHas(FOwner.AllowedAttributes, AName) or
    ListHas(FOwner.AllowedAttributes, ATag + '.' + AName));
end;

function TSanRun.SafeUrl(const AValue: StringRAL): Boolean;
var
  vText, vScheme: StringRAL;
  vInt: IntegerRAL;
begin
  Result := True;
  vText := DecodedForCheck(AValue);
  for vInt := 1 to Length(vText) do
    case vText[vInt] of
      '/', '?', '#': Exit;
      ':':
        begin
          vScheme := AsciiLower(Copy(vText, 1, vInt - 1));
          Result := ListHas(FOwner.AllowedSchemes, vScheme);
          Exit;
        end;
    end;
end;

procedure TSanRun.WriteTag(const ATag: StringRAL; const ANames, AValues: array of StringRAL;
  ACount: IntegerRAL);
var
  vInt, vOther: IntegerRAL;
  vDup: Boolean;
  vValue: StringRAL;
begin
  Emit('<' + ATag);
  for vInt := 0 to ACount - 1 do
  begin
    if not AttributeAllowed(ATag, ANames[vInt]) then
      Continue;
    vDup := False;
    for vOther := 0 to vInt - 1 do
      if ANames[vOther] = ANames[vInt] then
        vDup := True;
    if vDup then
      Continue;
    if FindText(',' + ANames[vInt] + ',', cUrlAttrs, 1) > 0 then
    begin
      if not SafeUrl(AValues[vInt]) then
        Continue;
      vValue := EscapeDecoded(AValues[vInt], True);
    end
    else
      vValue := EscapeDecoded(AValues[vInt], False);
    Emit(' ' + ANames[vInt] + '="' + vValue + '"');
  end;
  Emit('>');
end;

procedure TSanRun.SkipTo(const AEnd: StringRAL);
var
  vPos: IntegerRAL;
begin
  vPos := FindText(AEnd, FSrc, FPos);
  if vPos = 0 then
    FPos := FLen + 1
  else
    FPos := vPos + Length(AEnd);
end;

procedure TSanRun.OpenTag;
var
  vPos, vStart, vCount: IntegerRAL;
  vTag, vName, vValue: StringRAL;
  vNames, vValues: array of StringRAL;
  vQuote: AnsiChar;
  vClosed: Boolean;
begin
  vNames := nil;
  vValues := nil;
  vPos := FPos + 1;
  vStart := vPos;
  while (vPos <= FLen) and IsNameChar(FSrc[vPos]) do
    Inc(vPos);
  vTag := AsciiLower(Copy(FSrc, vStart, vPos - vStart));
  vCount := 0;
  vClosed := False;
  while vPos <= FLen do
  begin
    while (vPos <= FLen) and (IsBlank(FSrc[vPos]) or (FSrc[vPos] = '/')) do
      Inc(vPos);
    if vPos > FLen then
      Break;
    if FSrc[vPos] = '>' then
    begin
      Inc(vPos);
      vClosed := True;
      Break;
    end;
    vStart := vPos;
    Inc(vPos);
    while (vPos <= FLen) and not (IsBlank(FSrc[vPos]) or (FSrc[vPos] = '=') or
      (FSrc[vPos] = '>') or (FSrc[vPos] = '/')) do
      Inc(vPos);
    vName := AsciiLower(Copy(FSrc, vStart, vPos - vStart));
    vValue := '';
    while (vPos <= FLen) and IsBlank(FSrc[vPos]) do
      Inc(vPos);
    if (vPos <= FLen) and (FSrc[vPos] = '=') then
    begin
      Inc(vPos);
      while (vPos <= FLen) and IsBlank(FSrc[vPos]) do
        Inc(vPos);
      if vPos > FLen then
        Break;
      if (FSrc[vPos] = '"') or (FSrc[vPos] = #39) then
      begin
        vQuote := FSrc[vPos];
        Inc(vPos);
        vStart := vPos;
        while (vPos <= FLen) and (FSrc[vPos] <> vQuote) do
          Inc(vPos);
        if vPos > FLen then
          Break;
        vValue := Copy(FSrc, vStart, vPos - vStart);
        Inc(vPos);
      end
      else
      begin
        vStart := vPos;
        while (vPos <= FLen) and not (IsBlank(FSrc[vPos]) or (FSrc[vPos] = '>')) do
          Inc(vPos);
        vValue := Copy(FSrc, vStart, vPos - vStart);
      end;
    end;
    if vCount >= Length(vNames) then
    begin
      SetLength(vNames, vCount * 2 + 4);
      SetLength(vValues, vCount * 2 + 4);
    end;
    vNames[vCount] := vName;
    vValues[vCount] := vValue;
    Inc(vCount);
  end;
  if not vClosed then
  begin
    // a tag cut by the end of the text is dropped
    FPos := FLen + 1;
    Exit;
  end;
  FPos := vPos;
  if ListHas(FOwner.DropContentTags, vTag) then
  begin
    SkipTo('</' + vTag);
    if FPos <= FLen then
      SkipTo('>');
    Exit;
  end;
  if not ListHas(FOwner.AllowedTags, vTag) then
    Exit;
  WriteTag(vTag, vNames, vValues, vCount);
  if FindText(',' + vTag + ',', cVoidTags, 1) = 0 then
  begin
    if FDepth >= Length(FStack) then
      SetLength(FStack, FDepth * 2 + 8);
    FStack[FDepth] := vTag;
    Inc(FDepth);
  end;
end;

procedure TSanRun.CloseTag;
var
  vPos, vStart, vInt, vFound: IntegerRAL;
  vTag: StringRAL;
begin
  vPos := FPos + 2;
  vStart := vPos;
  while (vPos <= FLen) and IsNameChar(FSrc[vPos]) do
    Inc(vPos);
  vTag := AsciiLower(Copy(FSrc, vStart, vPos - vStart));
  FPos := vPos;
  SkipTo('>');
  vFound := -1;
  for vInt := FDepth - 1 downto 0 do
    if FStack[vInt] = vTag then
    begin
      vFound := vInt;
      Break;
    end;
  if vFound < 0 then
    Exit;
  while FDepth > vFound do
  begin
    Dec(FDepth);
    Emit('</' + FStack[FDepth] + '>');
  end;
end;

function TSanRun.Run(AOwner: TRALHtmlSanitizer; const AHtml: StringRAL): StringRAL;
var
  vStart, vNext: IntegerRAL;
begin
  FOwner := AOwner;
  FSrc := AHtml;
  FLen := Length(AHtml);
  FPos := 1;
  FOut := 1;
  FDepth := 0;
  FBuf := '';
  SetLength(FBuf, FLen + 64);
  while FPos <= FLen do
  begin
    if FSrc[FPos] <> '<' then
    begin
      vStart := FPos;
      vNext := vStart;
      while (vNext <= FLen) and (FSrc[vNext] <> '<') do
        Inc(vNext);
      Emit(EscapeDecoded(Copy(FSrc, vStart, vNext - vStart), False));
      FPos := vNext;
      Continue;
    end;
    if FPos = FLen then
    begin
      Emit('&lt;');
      Inc(FPos);
    end
    else if IsLetter(FSrc[FPos + 1]) then
      OpenTag
    else if FSrc[FPos + 1] = '/' then
    begin
      if (FPos + 2 <= FLen) and IsLetter(FSrc[FPos + 2]) then
        CloseTag
      else
      begin
        Inc(FPos, 2);
        SkipTo('>');
      end;
    end
    else if (FSrc[FPos + 1] = '!') or (FSrc[FPos + 1] = '?') then
    begin
      if Copy(FSrc, FPos, 4) = '<!--' then
      begin
        Inc(FPos, 4);
        SkipTo('-->');
      end
      else
      begin
        Inc(FPos, 2);
        SkipTo('>');
      end;
    end
    else
    begin
      Emit('&lt;');
      Inc(FPos);
    end;
  end;
  while FDepth > 0 do
  begin
    Dec(FDepth);
    Emit('</' + FStack[FDepth] + '>');
  end;
  SetLength(FBuf, FOut - 1);
  Result := FBuf;
end;

{ TRALHtmlSanitizer }

constructor TRALHtmlSanitizer.Create;
begin
  inherited Create;
  FAllowedTags := TStringList.Create;
  FAllowedAttributes := TStringList.Create;
  FAllowedSchemes := TStringList.Create;
  FDropContentTags := TStringList.Create;
  FAllowedTags.CommaText := cDefaultTags;
  FAllowedAttributes.CommaText := cDefaultAttributes;
  FAllowedSchemes.CommaText := 'http,https,mailto';
  FDropContentTags.CommaText := cDefaultDrop;
end;

destructor TRALHtmlSanitizer.Destroy;
begin
  FAllowedTags.Free;
  FAllowedAttributes.Free;
  FAllowedSchemes.Free;
  FDropContentTags.Free;
  inherited Destroy;
end;

procedure TRALHtmlSanitizer.Assign(ASource: TPersistent);
begin
  if ASource is TRALHtmlSanitizer then
  begin
    FAllowedTags.Assign(TRALHtmlSanitizer(ASource).AllowedTags);
    FAllowedAttributes.Assign(TRALHtmlSanitizer(ASource).AllowedAttributes);
    FAllowedSchemes.Assign(TRALHtmlSanitizer(ASource).AllowedSchemes);
    FDropContentTags.Assign(TRALHtmlSanitizer(ASource).DropContentTags);
  end
  else
    inherited Assign(ASource);
end;

procedure TRALHtmlSanitizer.SetAllowedAttributes(const AValue: TStrings);
begin
  FAllowedAttributes.Assign(AValue);
end;

procedure TRALHtmlSanitizer.SetAllowedSchemes(const AValue: TStrings);
begin
  FAllowedSchemes.Assign(AValue);
end;

procedure TRALHtmlSanitizer.SetAllowedTags(const AValue: TStrings);
begin
  FAllowedTags.Assign(AValue);
end;

procedure TRALHtmlSanitizer.SetDropContentTags(const AValue: TStrings);
begin
  FDropContentTags.Assign(AValue);
end;

function TRALHtmlSanitizer.Sanitize(const AHtml: StringRAL): StringRAL;
var
  vRun: TSanRun;
begin
  vRun := TSanRun.Create;
  try
    Result := vRun.Run(Self, AHtml);
  finally
    vRun.Free;
  end;
end;

function RALSanitizeHtml(const AHtml: StringRAL): StringRAL;
var
  vSan: TRALHtmlSanitizer;
begin
  vSan := TRALHtmlSanitizer.Create;
  try
    Result := vSan.Sanitize(AHtml);
  finally
    vSan.Free;
  end;
end;

end.
