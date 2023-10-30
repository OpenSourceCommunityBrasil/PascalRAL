unit RALUrlCoder;

interface

uses
  Classes, SysUtils,
  RALTypes, RALTools;

type

  { TRALHTTPCoder }

  TRALHTTPCoder = class
    class function DecodeURL(AUrl : StringRAL) : StringRAL;
    class function EncodeURL(AUrl : StringRAL) : StringRAL;

    class function DecodeHTML(AHtml : StringRAL) : StringRAL;
    class function EncodeHTML(AHtml : StringRAL) : StringRAL;
  end;

implementation

const
  URLStrTable : array[0..255] of StringRAL = (
                         '%00','%01','%02','%03','%04','%05','%06','%07','%08',
                         '%09','%0A','%0B','%0C','%0D','%0E','%0F','%10','%11',
                         '%12','%13','%14','%15','%16','%17','%18','%19','%1A',
                         '%1B','%1C','%1D','%1E','%1F','+','!','%22','%23',
                         '$','%25','%26','''','(',')','*','%2B','%2C','-','.',
                         '%2F','0','1','2','3','4','5','6','7','8','9','%3A',
                         '%3B','%3C','%3D','%3E','%3F','@','A','B','C','D','E',
                         'F','G','H','I','J','K','L','M','N','O','P','Q','R',
                         'S','T','U','V','W','X','Y','Z','%5B','%5C','%5D',
                         '%5E','_','%60','a','b','c','d','e','f','g','h','i',
                         'j','k','l','m','n','o','p','q','r','s','t','u','v',
                         'w','x','y','z','%7B','%7C','%7D','%7E','%7F','%80',
                         '%81','%82','%83','%84','%85','%86','%87','%88','%89',
                         '%8A','%8B','%8C','%8D','%8E','%8F','%90','%91','%92',
                         '%93','%94','%95','%96','%97','%98','%99','%9A','%9B',
                         '%9C','%9D','%9E','%9F','%A0','%A1','%A2','%A3','%A4',
                         '%A5','%A6','%A7','%A8','%A9','%AA','%AB','%AC','%AD',
                         '%AE','%AF','%B0','%B1','%B2','%B3','%B4','%B5','%B6',
                         '%B7','%B8','%B9','%BA','%BB','%BC','%BD','%BE','%BF',
                         '%C0','%C1','%C2','%C3','%C4','%C5','%C6','%C7','%C8',
                         '%C9','%CA','%CB','%CC','%CD','%CE','%CF','%D0','%D1',
                         '%D2','%D3','%D4','%D5','%D6','%D7','%D8','%D9','%DA',
                         '%DB','%DC','%DD','%DE','%DF','%E0','%E1','%E2','%E3',
                         '%E4','%E5','%E6','%E7','%E8','%E9','%EA','%EB','%EC',
                         '%ED','%EE','%EF','%F0','%F1','%F2','%F3','%F4','%F5',
                         '%F6','%F7','%F8','%F9','%FA','%FB','%FC','%FD','%FE',
                         '%FF');

  HTMLStrTable : array[0..255] of StringRAL = (
                         '&#00;','&#01;','&#02;','&#03;','&#04;','&#05;',
                         '&#06;','&#07;','&#08;','&#09;','&#10;','&#11;',
                         '&#12;','&#13;','&#14;','&#15;','&#16;','&#17;',
                         '&#18;','&#19;','&#20;','&#21;','&#22;','&#23;',
                         '&#24;','&#25;','&#26;','&#27;','&#28;','&#29;',
                         '&#30;','&#31;','&nbsp;','!','&quot;','#','$','%',
                         '&amp;','&apos;','(',')','*','+',',','-','.','/','0',
                         '1','2','3','4','5','6','7','8','9',':',';','&lt;',
                         '=','&gt;','?','@','A','B','C','D','E','F','G','H',
                         'I','J','K','L','M','N','O','P','Q','R','S','T','U',
                         'V','W','X','Y','Z','[','\',']','^','_','`','a','b',
                         'c','d','e','f','g','h','i','j','k','l','m','n','o',
                         'p','q','r','s','t','u','v','w','x','y','z','{','|',
                         '}','&tilde;','&#127;','&euro;','&#129;','&sbquo;',
                         '&fnof;','&bdquo;','&hellip;','&dagger;','&Dagger;',
                         '&circ;','&permil;','&Scaron;','&lsaquo;','&Oelig;',
                         '&#141;','&Zcaron;','&#143;','&#144;','&lsquo;',
                         '&rsquo;','&ldquo;','&rdquo;','&bull;','&ndash;',
                         '&mdash;','&#152;','&trade;','&scaron;','&rsaquo;',
                         '&oelig;','&#157;','&zcaron;','&Yuml;','&nbsp;',
                         '&iexcl;','&cent;','&pound;','&curren;','&yen;',
                         '&brvbar;','&sect;','&uml;','&copy;','&ordf;',
                         '&laquo;','&not;','&shy;','&reg;','&macr;','&deg;',
                         '&plusmn;','&sup2;','&sup3;','&cute;','&micro;',
                         '&para;','&middot;','&cedil;','&sup1;','&ordm;',
                         '&raquo;','&frac14;','&frac12;','&frac34;','&iquest;',
                         '&Agrave;','&Aacute;','&Acirc;','&Atilde;','&Auml;',
                         '&Aring;','&AElig;','&Ccedil;','&Egrave;','&Eacute;',
                         '&Ecirc;','&Euml;','&Igrave;','&Iacute;','&Icirc;',
                         '&Iuml;','&ETH;','&Ntilde;','&Ograve;','&Oacute;',
                         '&Ocirc;','&Otilde;','&Ouml;','&times;','&Oslash;',
                         '&Ugrave;','&Uacute;','&Ucirc;','&Uuml;','&Yacute;',
                         '&THORN;','&szlig;','&agrave;','&aacute;','&acirc;',
                         '&atilde;','&auml;','&aring;','&aelig;','&ccedil;',
                         '&egrave;','&eacute;','&ecirc;','&euml;','&igrave;',
                         '&iacute;','&icirc;','&iuml;','&eth;','&ntilde;',
                         '&ograve;','&oacute;','&ocirc;','&otilde;','&ouml;',
                         '&divide;','&oslash;','&ugrave;','&uacute;','&ucirc;',
                         '&uuml;','&yacute;','&thorn;','&yuml;');

{ TRALHTTPCoder }

class function TRALHTTPCoder.DecodeURL(AUrl : StringRAL) : StringRAL;
var
  vInt, vChr, vLen : IntegerRAL;
  vStr : StringRAL;
begin
  Result := '';
  vInt := RALLowStr(AUrl);
  vLen := RALHighStr(AUrl);
  while vInt <= vLen do
  begin
    if AUrl[vInt] = '+' then
    begin
      Result := Result + ' ';
    end
    else if AUrl[vInt] = '%' then
    begin
      if vInt + 2 <= vLen then
      begin
        vStr := '$'+Copy(AUrl,vInt+1,2);
        if TryStrToInt(vStr,vChr) then
        begin
          Result := Result + CharRAL(vChr);
          vInt := vInt + 2;
        end
        else
        begin
          Result := Result + '%';
        end;
      end
      else begin
        Result := Result + '%';
      end;
    end
    else
    begin
      Result := Result + AUrl[vInt];
    end;
    vInt := vInt + 1;
  end;
end;

class function TRALHTTPCoder.EncodeURL(AUrl : StringRAL) : StringRAL;
var
  vInt, vChr : IntegerRAL;
begin
  Result := '';
  vInt := RALLowStr(AUrl);
  while vInt <= RALHighStr(AUrl) do
  begin
    vChr := Ord(AUrl[vInt]);
    Result := Result + URLStrTable[vChr];
    vInt := vInt + 1;
  end;
end;

class function TRALHTTPCoder.DecodeHTML(AHtml : StringRAL) : StringRAL;
var
  vEsc : boolean;
  vCode : StringRAL;
  vInt, vChr : Integer;
begin
  Result := '';
  vCode := '';
  vEsc := False;

  vInt := RALLowStr(AHtml);
  while vInt <= RALHighStr(AHtml) do begin
    if AHtml[vInt] = '&' then begin
      vEsc := True;
      vCode := AHtml[vInt];
    end
    else if (AHtml[vInt] = ';') and (vEsc) then begin
      vCode := vCode + AHtml[vInt];
      vChr := 0;
      while vChr <= 255 do begin
        if HTMLStrTable[vChr] = vCode then begin
          Result := Result + CharRAL(vChr);
          Break;
        end;
        vChr := vChr + 1;
      end;

      if vChr > 255 then
        Result := Result + vCode;

      vCode := '';
      vEsc := False;
    end
    else begin
      if vEsc then
        vCode := vCode + AHtml[vInt]
      else
        Result := Result + AHtml[vInt];
    end;
    vInt := vInt + 1;
  end;

  if vCode <> '' then
    Result := Result + vCode;
end;

class function TRALHTTPCoder.EncodeHTML(AHtml : StringRAL) : StringRAL;
var
  vInt, vChr : IntegerRAL;
begin
  Result := '';
  vInt := RALLowStr(AHtml);
  while vInt <= RALHighStr(AHtml) do
  begin
    vChr := Ord(AHtml[vInt]);
    Result := Result + HTMLStrTable[vChr];
    vInt := vInt + 1;
  end;
end;

end.

