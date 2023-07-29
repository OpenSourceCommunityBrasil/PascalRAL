unit RALTools;

interface

uses
  {$IFNDEF FPC}
  JSON,
  {$ENDIF}
  Classes, SysUtils, Variants, StrUtils, TypInfo,
  RALTypes, RALConsts;

function VarToBytes(v: variant): TBytes;
function BytesToString(b: TBytes): StringRAL;
function FixRoute(ARoute : StringRAL) : StringRAL;
function RandomBytes(numOfBytes : IntegerRAL) : TBytes;

implementation

function VarToBytes(v: variant): TBytes;
var
  vTyp: TVarType;
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
        {$IFNDEF FPC}
        Result := TEncoding.ANSI.GetBytes(StringRAL(v));
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

function FixRoute(ARoute : StringRAL) : StringRAL;
begin
  Result := '/'+ARoute+'/';
  while Pos('//', Result) > 0 do
    Result := ReplaceStr(Result, '//', '/');
end;

function RandomBytes(numOfBytes : IntegerRAL) : TBytes;
var
  vInt : IntegerRAL;
begin
  SetLength(Result,numOfBytes);
  Randomize;
  for vInt := 1 to numOfBytes do
    Result[vInt-1] := Random(256);
end;

end.
