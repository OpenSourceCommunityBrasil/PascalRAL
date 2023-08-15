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

implementation

function VarToBytes(v: variant): TBytes;
var
  vTyp: TVarType;
begin
  vTyp := VarType(v);

  // 258 - varUString - string
  // 256 - varString  - ansistring
  // 008 - varOleStr  - widestring

  { TODO -o Fernando -c Implementacao :
    Analisar compatibilidade com indy e synopse abaixo.
    Indy usa ANSI pra tudo, Synopse usa UTF8 }
  case vTyp of
    varUString, varString, varOleStr:
    begin
      SetLength(Result, 0);
      if v <> '' then
      begin
        {$IFNDEF FPC}
        Result := TEncoding.UTF8.GetBytes(StringRAL(v));
        {$ELSE}
        SetLength(Result, Length(v));
        // Move(Pointer(@StringRAL(v)[PosIniStr])^, Pointer(Result)^, Length(v));
        {$ENDIF}
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
  while Pos(StringRAL('.'), Result) > 0 do
    Result := ReplaceStr(Result, '.', '');

  while Pos(StringRAL('//'), Result) > 0 do
    Result := ReplaceStr(Result, '//', '/');
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

end.
