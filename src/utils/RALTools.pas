unit RALTools;

interface

uses
  Classes, SysUtils, Variants, StrUtils, TypInfo,
  RALTypes, RALConsts;

function VarToBytes(v: variant): TBytes;
function BytesToString(b: TBytes): StringRAL;
function FixRoute(ARoute: StringRAL): StringRAL;
function RandomBytes(numOfBytes: IntegerRAL): TBytes;
function HTTPMethodToRALMethod(AMethod: StringRAL): TRALMethod;
function RALMethodToHTTPMethod(AMethod: TRALMethod): StringRAL;
function RALLowStr(AStr : StringRAL) : IntegerRAL;
function RALHighStr(AStr : StringRAL) : IntegerRAL;
function StrIsUTF8(AStr : StringRAL) : boolean;
function StrCompressToCompress(AStr : StringRAL) : TRALCompressType;
function CompressToStrCompress(ACompress : TRALCompressType) : StringRAL;
function StrCriptoToCripto(AStr : StringRAL) : TRALCriptoType;
function CriptoToStrCripto(ACripto : TRALCriptoType) : StringRAL;

implementation

function VarToBytes(v: variant): TBytes;
var
  vTyp: TVarType;
  vStream: TStringStream;
begin
  vTyp := VarType(v);

  // 258 - varUString - string
  // 256 - varString  - ansistring
  // 008 - varOleStr  - widestring

  case vTyp of
    varUString, varString, varOleStr:
    begin
      SetLength(Result, 0);
      if v <> '' then
      begin
        vStream := TStringStream.Create(StringRAL(v));
        try
          Result := vStream.Bytes;
        finally
          FreeAndNil(vStream);
        end;
      end;
    end;
  end;
end;

function BytesToString(b: TBytes): StringRAL;
begin
  Result := '';
  if Length(b) > 0 then
  begin
    SetLength(Result, Length(b));
    Move(b[0], Result[PosIniStr], Length(b));
  end;
end;

function FixRoute(ARoute: StringRAL): StringRAL;
begin
  Result := '/' + ARoute + '/';

  // path transversal fix
  ARoute := StringReplace(ARoute, '../', '', [rfReplaceAll]);

  while Pos(StringRAL('//'), Result) > 0 do
    Result := StringReplace(Result, '//', '/', [rfReplaceAll]);
end;

function RandomBytes(numOfBytes: IntegerRAL): TBytes;
var
  vInt: IntegerRAL;
begin
  SetLength(Result, numOfBytes);
  Randomize;
  for vInt := 1 to numOfBytes do
    Result[vInt - 1] := Random(256);
end;

function HTTPMethodToRALMethod(AMethod: StringRAL): TRALMethod;
var
  vInt: IntegerRAL;
begin
  AMethod := 'am' + UpperCase(AMethod);
  vInt := GetEnumValue(TypeInfo(TRALMethod), AMethod);
  if vInt <> -1 then
    Result := TRALMethod(vInt)
  else
    Result := amGET;
end;

function RALMethodToHTTPMethod(AMethod: TRALMethod): StringRAL;
begin
  Result := GetEnumName(TypeInfo(TRALMethod), ord(AMethod));
  Delete(Result, 1, 2); // delete 'am'
end;

function RALLowStr(AStr : StringRAL) : integer;
begin
  {$IFNDEF FPC}
    {$IFNDEF DELPHIXE2}
      Result := 1;
    {$ELSE}
      Result := Low(AStr);
    {$ENDIF}
  {$ELSE}
    Result := Low(AStr);
  {$ENDIF}
end;

function RALHighStr(AStr : StringRAL) : integer;
begin
  {$IFNDEF FPC}
    {$IFNDEF DELPHIXE2}
      Result := Length(AStr);
    {$ELSE}
      Result := High(AStr);
    {$ENDIF}
  {$ELSE}
    Result := High(AStr);
  {$ENDIF}
end;

function StrIsUTF8(AStr : StringRAL) : boolean;
var
  vStr : TStringStream;
  ySeq, nSeq, i : integer;
  pvByte, nwByte : byte;
begin
  vStr := TStringStream.Create(AStr);
  try
    vStr.Position := 0;

    ySeq := 0;
    nseq := 0;
    if vStr.Size > 1 then begin
      vStr.Read(pvByte,1);
      for i := 1 to vStr.Size do begin
        vStr.Read(nwByte,1);
        if ((nwByte and $c0) = $80) then begin
          if ((pvByte and $c0) = $c0) then begin
            Inc(ySeq)
          end
          else  begin
            if ((pvByte and $80) = $0) then
              Inc(nSeq);
          end;
        end;
        pvByte := nwByte;
      end;
    end;
    Result := ySeq > nSeq;
  finally
    FreeAndNil(vStr);
  end;
end;

function StrCompressToCompress(AStr : StringRAL) : TRALCompressType;
begin
  if SameText(AStr,'gzip') then
    Result := ctGZip
  else if SameText(AStr,'zlib') then
    Result := ctZLib
  else if SameText(AStr,'deflate') then
    Result := ctDeflate
  else
    Result := ctNone;
end;

function CompressToStrCompress(ACompress : TRALCompressType) : StringRAL;
begin
  case ACompress of
    ctNone    : Result := '';
    ctGZip    : Result := 'gzip';
    ctDeflate : Result := 'deflate';
    ctZLib    : Result := 'zlib';
  end;
end;

function StrCriptoToCripto(AStr : StringRAL) : TRALCriptoType;
begin
  if SameText(AStr,'aes128cbc_pkcs7') then
    Result := crAES128
  else if SameText(AStr,'aes192cbc_pkcs7') then
    Result := crAES192
  else if SameText(AStr,'aes256cbc_pkcs7') then
    Result := crAES256
  else
    Result := crNone;
end;

function CriptoToStrCripto(ACripto : TRALCriptoType) : StringRAL;
begin
  case ACripto of
    crNone   : Result := '';
    crAES128 : Result := 'aes128cbc_pkcs7';
    crAES192 : Result := 'aes192cbc_pkcs7';
    crAES256 : Result := 'aes256cbc_pkcs7';
  end;
end;

end.
