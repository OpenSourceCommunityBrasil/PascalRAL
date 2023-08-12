unit RALJSON;

interface

{$I ../base/PascalRAL.inc}

uses
  {$IFNDEF FPC}
    {$IFDEF DELPHI2010UP}
       RALJSON.Delphi,
    {$ELSE}
       RALJSON.lkJSON,
    {$ENDIF}
  {$ELSE}
    RALJSON.FPC,
  {$ENDIF}
  Classes, SysUtils;

type
  TRALJSONValue  = TRALJSONValueBase;
  TRALJSONObject = TRALJSONObjectBase;
  TRALJSON       = TRALJSONBase;
  TRALJSONArray  = TRALJSONArrayBase;

implementation

end.
