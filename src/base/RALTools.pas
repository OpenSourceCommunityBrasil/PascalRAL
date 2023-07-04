unit RALTools;

interface

uses
  Classes, SysUtils, Variants,
  RALTypes, RALConsts;

function VarToBytes(v : variant) : TBytes;
function BytesToString(b : TBytes) : StringRAL;

implementation

function VarToBytes(v : variant) : TBytes;
var
  vTyp : TVarType;
  vInt : IntegerRAL;
begin
  vTyp := VarType(v);

  // 258 - varUString - string
  // 256 - varString  - ansistring
  // 008 - varOleStr  - widestring

  case vTyp of
    varUString,
    varString,
    varOleStr : begin
      SetLength(Result, 0);
      if v <> '' then begin
        vInt := (Length(v)+1) * SizeOf(CharRAL);
        SetLength(Result, vInt);
        Move(Pointer(@StringRAL(v)[PosIniStr])^, Pointer(Result)^, vInt);
      end;
    end;
  end;
end;

function BytesToString(b : TBytes) : StringRAL;
var
  vInt : IntegerRAL;
begin
  Result := '';
  if Length(b) > 0 then begin
    vInt := (Length(b) div SizeOf(CharRAL)) - 1;
    SetLength(Result,vInt);
    Move(b[0],Result[PosIniStr],Length(b));
  end;
end;


end.
