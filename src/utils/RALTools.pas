unit RALTools;

interface

uses
  {$IFNDEF FPC}
  JSON,
  {$ENDIF}
  Classes, SysUtils, Variants,
  RALTypes;

{$IFNDEF FPC}
type
  TJSONHelper = class helper for TJSONObject
  public
    function Add(const Pair: TJSONPair): TJSONObject; overload;
    function Add(const Str: TJSONString; const Val: TJSONValue): TJSONObject; overload;
    function Add(const Str: string; const Val: TJSONValue): TJSONObject; overload;
    function Add(const Str: string; const Val: string): TJSONObject; overload;
    function Add(const Str: string; const Val: int64): TJSONObject; overload;
    function Add(const Str: string; const Val: integer): TJSONObject; overload;
    function Add(const Str: string; const Val: double): TJSONObject; overload;
    function Add(const Str: string; const Val: boolean): TJSONObject; overload;
  end;

  TJSONAncestorHelper = class helper for TJSONAncestor
  public
    function AsJSON: string; inline;
  end;
{$ENDIF}

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

{$IFNDEF FPC}
{ TJSONHelper }

function TJSONHelper.Add(const Str, Val: string): TJSONObject;
begin
  Result := AddPair(str, val);
end;

function TJSONHelper.Add(const Str: string; const Val: TJSONValue): TJSONObject;
begin
  Result := AddPair(str, val);
end;

function TJSONHelper.Add(const Str: TJSONString; const Val: TJSONValue): TJSONObject;
begin
  Result := AddPair(str, val);
end;

function TJSONHelper.Add(const Pair: TJSONPair): TJSONObject;
begin
  Result := AddPair(Pair);
end;


function TJSONHelper.Add(const Str: string; const Val: boolean): TJSONObject;
begin
  Result := AddPair(str, val);
end;


function TJSONHelper.Add(const Str: string; const Val: double): TJSONObject;
begin
  Result := AddPair(str, val);
end;


function TJSONHelper.Add(const Str: string; const Val: integer): TJSONObject;
begin
  Result := AddPair(str, val);
end;


function TJSONHelper.Add(const Str: string; const Val: int64): TJSONObject;
begin
  Result := AddPair(str, val);
end;

{ TJSONAncestorHelper }

function TJSONAncestorHelper.AsJSON: string;
begin
  Result := ToJSON;
end;
{$ENDIF}

end.
