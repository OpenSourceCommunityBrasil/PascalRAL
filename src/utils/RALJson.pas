unit RALJSON;

interface

{$I ../base/PascalRAL.inc}

{$DEFINE RALlkJSON}
{.$DEFINE RALuJSON}

uses
  {$IFNDEF FPC}
    {$IFDEF DELPHI2010UP}
       RALJSON_Delphi,
    {$ELSE}
       {$IFDEF RALlkJSON}
         RALJSON_lkJSON,
       {$ENDIF}
       {$IFDEF RALlkJSON}
         RALJSON_uJSON,
       {$ENDIF}
    {$ENDIF}
  {$ELSE}
    RALJSON_FPC,
  {$ENDIF}
  Classes, SysUtils;

type
  TRALJSONValue  = TRALJSONValueBase;
  TRALJSONObject = TRALJSONObjectBase;
  TRALJSON       = TRALJSONBase;
  TRALJSONArray  = TRALJSONArrayBase;

implementation

end.
