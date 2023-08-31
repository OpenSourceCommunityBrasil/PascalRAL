unit RALJSON;

interface

{$I ../base/PascalRAL.inc}

{$DEFINE RALlkJSON}
{.$DEFINE RALuJSON}

// DBXJSON
// 2010  - CompVersion 21 - Tested OK
// XE    - CompVersion 22 - Tested OK
// XE2   - CompVersion 23 - Tested OK
// XE3   - CompVersion 24 - NOT Tested
// XE4   - CompVersion 25 - Tested OK
// XE5   - CompVersion 26 - Tested OK

// System.JSON
// XE6   - CompVersion 27 - Tested OK
// XE7   - CompVersion 28 - Tested OK
// XE8   - CompVersion 29 - Tested OK
// XE10  - 29 > - Em Testes

uses
  {$IFNDEF FPC}
    {$IFDEF DELPHI2010UP}
      {$IFDEF DELPHIXE6UP}
        System.JSON,
      {$ELSE}
        DBXJSON,
      {$ENDIF}
    {$ELSE}
       {$IFDEF RALlkJSON}
         ulkJSON,
       {$ENDIF}
       {$IFDEF RALlkJSON}
         uJSON,
       {$ENDIF}
    {$ENDIF}
  {$ELSE}
    fpjson,
  {$ENDIF}
  Variants, RALTypes, Classes, SysUtils;

{$IFNDEF FPC}
  {$IFDEF DELPHI2010UP}
    {$I RALJSON_Delphi.inc}
  {$ELSE}
    {$IFDEF RALlkJSON}
      {$I RALJSON_lkJSON.inc}
    {$ENDIF}
    {$IFDEF RALlkJSON}
      {$I RALJSON_uJSON.inc}
    {$ENDIF}
  {$ENDIF}
{$ELSE}
  {$I RALJSON_FPC.inc}
{$ENDIF}

end.
