/// Class for the Encoder/Decoder mechanism for HTTP URLs
unit RALUrlCoder;

interface

uses
  Classes, SysUtils,
  RALTypes, RALTools;

type

  { TRALHTTPCoder }

  TRALHTTPCoder = class
    class function DecodeURL(const AUrl: StringRAL): StringRAL;
    class function EncodeURL(const AUrl: StringRAL): StringRAL;
    class function DecodeHTML(const AHtml: StringRAL): StringRAL;
    class function EncodeHTML(const AHtml: StringRAL): StringRAL;
  end;

implementation

const
  URLStrTable: array[0..255] of StringRAL = (
    '%00', '%01', '%02', '%03', '%04', '%05', '%06', '%07', '%08',
    '%09', '%0A', '%0B', '%0C', '%0D', '%0E', '%0F', '%10', '%11',
    '%12', '%13', '%14', '%15', '%16', '%17', '%18', '%19', '%1A',
    '%1B', '%1C', '%1D', '%1E', '%1F', '+', '!', '%22', '%23',
    '$', '%25', '%26', '''', '(', ')', '*', '%2B', '%2C', '-', '.',
    '%2F', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '%3A',
    '%3B', '%3C', '%3D', '%3E', '%3F', '@', 'A', 'B', 'C', 'D', 'E',
    'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
    'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '%5B', '%5C', '%5D',
    '%5E', '_', '%60', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
    'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '%7B', '%7C', '%7D', '%7E', '%7F', '%80',
    '%81', '%82', '%83', '%84', '%85', '%86', '%87', '%88', '%89',
    '%8A', '%8B', '%8C', '%8D', '%8E', '%8F', '%90', '%91', '%92',
    '%93', '%94', '%95', '%96', '%97', '%98', '%99', '%9A', '%9B',
    '%9C', '%9D', '%9E', '%9F', '%A0', '%A1', '%A2', '%A3', '%A4',
    '%A5', '%A6', '%A7', '%A8', '%A9', '%AA', '%AB', '%AC', '%AD',
    '%AE', '%AF', '%B0', '%B1', '%B2', '%B3', '%B4', '%B5', '%B6',
    '%B7', '%B8', '%B9', '%BA', '%BB', '%BC', '%BD', '%BE', '%BF',
    '%C0', '%C1', '%C2', '%C3', '%C4', '%C5', '%C6', '%C7', '%C8',
    '%C9', '%CA', '%CB', '%CC', '%CD', '%CE', '%CF', '%D0', '%D1',
    '%D2', '%D3', '%D4', '%D5', '%D6', '%D7', '%D8', '%D9', '%DA',
    '%DB', '%DC', '%DD', '%DE', '%DF', '%E0', '%E1', '%E2', '%E3',
    '%E4', '%E5', '%E6', '%E7', '%E8', '%E9', '%EA', '%EB', '%EC',
    '%ED', '%EE', '%EF', '%F0', '%F1', '%F2', '%F3', '%F4', '%F5',
    '%F6', '%F7', '%F8', '%F9', '%FA', '%FB', '%FC', '%FD', '%FE',
    '%FF');

  HTMLStrTable: array[0..255] of StringRAL = (
    '&#00;', '&#01;', '&#02;', '&#03;', '&#04;', '&#05;',
    '&#06;', '&#07;', '&#08;', '&#09;', '&#10;', '&#11;',
    '&#12;', '&#13;', '&#14;', '&#15;', '&#16;', '&#17;',
    '&#18;', '&#19;', '&#20;', '&#21;', '&#22;', '&#23;',
    '&#24;', '&#25;', '&#26;', '&#27;', '&#28;', '&#29;',
    '&#30;', '&#31;', '&nbsp;', '!', '&quot;', '#', '$', '%',
    '&amp;', '&apos;', '(', ')', '*', '+', ',', '-', '.', '/', '0',
    '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '&lt;',
    '=', '&gt;', '?', '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U',
    'V', 'W', 'X', 'Y', 'Z', '[', '\', ']', '^', '_', '`', 'a', 'b',
    'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|',
    '}', '&tilde;', '&#127;', '&euro;', '&#129;', '&sbquo;',
    '&fnof;', '&bdquo;', '&hellip;', '&dagger;', '&Dagger;',
    '&circ;', '&permil;', '&Scaron;', '&lsaquo;', '&Oelig;',
    '&#141;', '&Zcaron;', '&#143;', '&#144;', '&lsquo;',
    '&rsquo;', '&ldquo;', '&rdquo;', '&bull;', '&ndash;',
    '&mdash;', '&#152;', '&trade;', '&scaron;', '&rsaquo;',
    '&oelig;', '&#157;', '&zcaron;', '&Yuml;', '&nbsp;',
    '&iexcl;', '&cent;', '&pound;', '&curren;', '&yen;',
    '&brvbar;', '&sect;', '&uml;', '&copy;', '&ordf;',
    '&laquo;', '&not;', '&shy;', '&reg;', '&macr;', '&deg;',
    '&plusmn;', '&sup2;', '&sup3;', '&cute;', '&micro;',
    '&para;', '&middot;', '&cedil;', '&sup1;', '&ordm;',
    '&raquo;', '&frac14;', '&frac12;', '&frac34;', '&iquest;',
    '&Agrave;', '&Aacute;', '&Acirc;', '&Atilde;', '&Auml;',
    '&Aring;', '&AElig;', '&Ccedil;', '&Egrave;', '&Eacute;',
    '&Ecirc;', '&Euml;', '&Igrave;', '&Iacute;', '&Icirc;',
    '&Iuml;', '&ETH;', '&Ntilde;', '&Ograve;', '&Oacute;',
    '&Ocirc;', '&Otilde;', '&Ouml;', '&times;', '&Oslash;',
    '&Ugrave;', '&Uacute;', '&Ucirc;', '&Uuml;', '&Yacute;',
    '&THORN;', '&szlig;', '&agrave;', '&aacute;', '&acirc;',
    '&atilde;', '&auml;', '&aring;', '&aelig;', '&ccedil;',
    '&egrave;', '&eacute;', '&ecirc;', '&euml;', '&igrave;',
    '&iacute;', '&icirc;', '&iuml;', '&eth;', '&ntilde;',
    '&ograve;', '&oacute;', '&ocirc;', '&otilde;', '&ouml;',
    '&divide;', '&oslash;', '&ugrave;', '&uacute;', '&ucirc;',
    '&uuml;', '&yacute;', '&thorn;', '&yuml;');

  { TRALHTTPCoder }

{ the value of a hexadecimal digit, 255 for any other byte }
function HexDigit(AByte: Byte): Byte;
begin
  case AByte of
    Ord('0')..Ord('9'):
      Result := AByte - Ord('0');
    Ord('A')..Ord('F'):
      Result := AByte - Ord('A') + 10;
    Ord('a')..Ord('f'):
      Result := AByte - Ord('a') + 10;
  else
    Result := 255;
  end;
end;

class function TRALHTTPCoder.DecodeURL(const AUrl: StringRAL): StringRAL;
var
  vSrc, vEnd, vDest: PByte;
  vHigh, vLow: Byte;
  vDecoded: StringRAL;
begin
  { nothing to decode is the common case - a name, a number, a plain word -
    and then the answer is the string itself, not a copy: it used to cost a
    byte buffer and a new string every time }
  vSrc := PByte(Pointer(AUrl));
  vEnd := vSrc + Length(AUrl);
  while (vSrc < vEnd) and (vSrc^ <> Ord('%')) and (vSrc^ <> Ord('+')) do
    Inc(vSrc);
  if vSrc = vEnd then
  begin
    Result := AUrl;
    Exit;
  end;

  { decoded byte by byte straight into a string that can only get shorter:
    "%C3%A7" is the UTF-8 of "ç", and appending CharRAL(#$C3) to a UTF8String
    on Delphi converted that byte from the ANSI codepage first - every
    non-ASCII character came out doubly encoded ("Ã§"). A '%' without two
    hexadecimal digits after it stays as it is. Result is written last, after
    AUrl was read whole, because a caller writing "S := DecodeURL(S)" may
    hand both the same variable }
  SetLength(vDecoded, Length(AUrl));
  vDest := PByte(Pointer(vDecoded));
  Move(PByte(Pointer(AUrl))^, vDest^, vSrc - PByte(Pointer(AUrl)));
  Inc(vDest, vSrc - PByte(Pointer(AUrl)));
  while vSrc < vEnd do
  begin
    vDest^ := vSrc^;
    if vSrc^ = Ord('+') then
      vDest^ := Ord(' ')
    else if (vSrc^ = Ord('%')) and (vEnd - vSrc > 2) then
    begin
      vHigh := HexDigit(vSrc[1]);
      vLow := HexDigit(vSrc[2]);
      if (vHigh < 16) and (vLow < 16) then
      begin
        vDest^ := (vHigh shl 4) or vLow;
        Inc(vSrc, 2);
      end;
    end;
    Inc(vDest);
    Inc(vSrc);
  end;
  SetLength(vDecoded, vDest - PByte(Pointer(vDecoded)));
  Result := vDecoded;
end;

{ Every byte of AText replaced by its entry of ATable, in one allocation. The
  result used to grow by concatenation - a reallocation and a copy of all of it
  per input byte, quadratic - and EncodeURL runs over every form field sent }
function EncodeByTable(const AText: StringRAL; const ATable: array of StringRAL): StringRAL;
var
  vInt, vLen: IntegerRAL;
  vDest: PByte;
begin
  vLen := 0;
  for vInt := POSINISTR to RALHighStr(AText) do
    Inc(vLen, Length(ATable[Ord(AText[vInt])]));
  SetLength(Result, vLen);
  if vLen = 0 then
    Exit;
  vDest := PByte(Pointer(Result));
  for vInt := POSINISTR to RALHighStr(AText) do
  begin
    vLen := Length(ATable[Ord(AText[vInt])]);
    if vLen > 0 then
    begin
      Move(Pointer(ATable[Ord(AText[vInt])])^, vDest^, vLen);
      Inc(vDest, vLen);
    end;
  end;
end;

class function TRALHTTPCoder.EncodeURL(const AUrl: StringRAL): StringRAL;
begin
  Result := EncodeByTable(AUrl, URLStrTable);
end;

class function TRALHTTPCoder.DecodeHTML(const AHtml: StringRAL): StringRAL;
var
  vInt, vHigh, vOut, vEsc, vChr: IntegerRAL;

  { the byte whose entity is AHtml[AFrom..AFrom + ALen - 1], or -1 }
  function EntityAt(AFrom, ALen: IntegerRAL): IntegerRAL;
  var
    vIdx: IntegerRAL;
  begin
    for vIdx := 0 to 255 do
      if (Length(HTMLStrTable[vIdx]) = ALen) and
         CompareMem(@AHtml[AFrom], Pointer(HTMLStrTable[vIdx]), ALen) then
      begin
        Result := vIdx;
        Exit;
      end;
    Result := -1;
  end;

  procedure Keep(AFrom, ATo: IntegerRAL);
  begin
    if ATo >= AFrom then
    begin
      Move(AHtml[AFrom], Result[vOut], ATo - AFrom + 1);
      Inc(vOut, ATo - AFrom + 1);
    end;
  end;

begin
  { One pass into a result that can only shrink - an entity becomes one byte -
    instead of concatenating a character at a time, which was quadratic, and
    in an unterminated '&...' as well. An '&' that opens no entity is kept as
    it is: the text from it to the next '&' used to be dropped }
  SetLength(Result, Length(AHtml));
  vOut := POSINISTR;
  vEsc := -1;
  vHigh := RALHighStr(AHtml);
  for vInt := POSINISTR to vHigh do
  begin
    if AHtml[vInt] = '&' then
    begin
      if vEsc >= 0 then
        Keep(vEsc, vInt - 1);
      vEsc := vInt;
    end
    else if (AHtml[vInt] = ';') and (vEsc >= 0) then
    begin
      vChr := EntityAt(vEsc, vInt - vEsc + 1);
      if vChr >= 0 then
      begin
        // a StringRAL element is AnsiChar everywhere; CharRAL is WideChar
        // before Delphi 10.1
        Result[vOut] := AnsiChar(vChr);
        Inc(vOut);
      end
      else
        Keep(vEsc, vInt);
      vEsc := -1;
    end
    else if vEsc < 0 then
    begin
      Result[vOut] := AHtml[vInt];
      Inc(vOut);
    end;
  end;
  if vEsc >= 0 then
    Keep(vEsc, vHigh);
  SetLength(Result, vOut - POSINISTR);
end;

class function TRALHTTPCoder.EncodeHTML(const AHtml: StringRAL): StringRAL;
begin
  Result := EncodeByTable(AHtml, HTMLStrTable);
end;

end.
