unit RALTypes;

interface

uses
  Classes,
  RALConsts;

// compatibility types
{
  These definitions are meant to keep same code across all versions of the IDE
  or IDEs that might differ on the charset code or basic type length.
  Expect heavy usage of IFDEFs at this point
}
type
  // numeric types
  IntegerRAL = integer;
  Int64RAL = int64;
  DoubleRAL = double;

  // text types
  {$IFDEF FPC}
  StringRAL = string;
  CharRAL = Char;
  {$ELSE}
  StringRAL = UTF8String;
  CharRAL = WideChar;
  {$ENDIF}
  PCharRAL = ^CharRAL;

  TRALComponent = class(TComponent)
  private
    function getVersion: string;
  published
    property Version: string read getVersion;
  end;

const
  PosIniStr = 1;

implementation

{ TRALComponent }

function TRALComponent.getVersion: string;
begin
  Result := RALVERSION;
end;

end.
