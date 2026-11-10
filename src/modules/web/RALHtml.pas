/// Escaping of text for HTML, HTML attributes, JavaScript and URLs.
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

end.
