{
@abstract Unit for all type definitions used within PascalRAL
  These definitions are meant to keep same code across all versions of the IDE
  or IDEs that might differ on the charset code or basic type length.
  Expect heavy usage of IFDEFs at this unit
}
unit RALTypes;

interface

{$I ..\base\PascalRAL.inc}

uses
  Classes, SysUtils,
  RALConsts;

type
  IntegerRAL = integer;
  Int64RAL = int64;
  DoubleRAL = double;

  {$IFDEF FPC}
  StringRAL = string;
  CharRAL = Char;
  {$ELSE}
  StringRAL = utf8string;
  CharRAL = widechar;
  {$ENDIF}
  PCharRAL = ^CharRAL;

  {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
  TBytes = array of byte;
  {$IFEND}

  TRALCompressType = (ctNone, ctDeflate, ctZLib, ctGZip, ctZStd);
  TRALCriptoType = (crNone, crAES128, crAES192, crAES256);
  TRALJSONType = (rjtString, rjtNumber, rjtBoolean, rjtObject, rjtArray);
  TRALMethod = (amALL, amGET, amPOST, amPUT, amPATCH, amDELETE, amOPTIONS, amHEAD, amTRACE);
  TRALMethods = set of TRALMethod;
  TRALParamKind = (rpkNONE, rpkBODY, rpkFIELD, rpkHEADER, rpkQUERY, rpkCOOKIE);
  TRALParamKinds = set of TRALParamKind;
  TRALServerOption = (rsoBruteForceProtection, rsoDDoSProtection, rsoEnableBlackList,
    rsoEnableBlockList, rsoEnableWhiteList, rsoIPBroadcastProtection,
    rsoPathTransvBlackList);
  TRALServerOptions = set of TRALServerOption;

const
  {$IFDEF HAS_FMX}
    {$IFNDEF DELPHI12UP}
      PosIniStr = 0;
    {$ELSE}
      PosIniStr = 1;
    {$ENDIF}
  {$ELSE}
    PosIniStr = 1;
  {$ENDIF}

function RALLowStr(AStr: StringRAL): IntegerRAL;
function RALHighStr(const AStr: StringRAL): IntegerRAL;

implementation

function RALLowStr(AStr: StringRAL): integer;
begin
  {$IFNDEF FPC}
    {$IFNDEF DELPHIXE2UP}
      Result := 1;
    {$ELSE}
      Result := Low(AStr);
    {$ENDIF}
  {$ELSE}
    Result := Low(AStr);
  {$ENDIF}
end;

function RALHighStr(const AStr: StringRAL): integer;
begin
  {$IFNDEF FPC}
    {$IFNDEF DELPHIXE2UP}
      Result := Length(AStr);
    {$ELSE}
      Result := High(AStr);
    {$ENDIF}
  {$ELSE}
    Result := High(AStr);
  {$ENDIF}
end;

end.
