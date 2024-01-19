{
  @abstract Base unit for JSON Mapping
  Native JSON Libraries are depending on IDE version, older Delphi Versions don't have
  a native JSON library, for those older versions there are 2 libraries that are
  supported by PascalRAL, which you enable/disable within PascalRAL.inc
}
unit RALJSON;

interface

{$I ../base/PascalRAL.inc}

{$IFDEF FPC}
  {$I RALJSON_FPC.inc}
{$ELSE}
  {$IFDEF DELPHI2010UP}
    {$I RALJSON_Delphi.inc}
  {$ELSE}
    {$IFDEF RALlkJSON}
      {$I RALJSON_lkJSON.inc}
    {$ELSE}
      {$IFDEF RALuJSON}
        {$I RALJSON_uJSON.inc}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

end.
