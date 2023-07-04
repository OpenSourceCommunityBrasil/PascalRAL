unit RALTools;

interface

uses
  Classes, SysUtils, Variants,
  RALTypes;

function VarToBytes(v: variant): TBytes;
function BytesToString(b: TBytes): StringRAL;

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
            Move(Pointer(@StringRAL(v)[PosIniStr])^, Pointer(Result)^, Length(v));
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

end.
