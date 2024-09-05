{ @abstract Unit for all type definitions used within PascalRAL
  These definitions are meant to keep same code across all versions of the IDE
  or IDEs that might differ on the charset code or basic type length.
  Expect heavy usage of IFDEFs in this unit
}
unit RALTypes;

interface

{$I ..\base\PascalRAL.inc}

uses
  {$IFDEF FPC}
  bufstream,
  {$ENDIF}
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

  {$IF DEFINED(DELPHI10_1UP) OR DEFINED(FPC)}
  TRALBufFileStream = TBufferedFileStream;
  {$ELSE}
  TRALBufFileStream = TFileStream;
  {$IFEND}

  TRALCriptoType = (crNone, crAES128, crAES192, crAES256);
  TRALJSONType = (rjtString, rjtNumber, rjtBoolean, rjtObject, rjtArray);
  TRALMethod = (amALL, amGET, amPOST, amPUT, amPATCH, amDELETE, amOPTIONS, amHEAD, amTRACE);
  TRALMethods = set of TRALMethod;
  TRALParamKind = (rpkNONE, rpkBODY, rpkFIELD, rpkHEADER, rpkQUERY, rpkCOOKIE);
  TRALParamKinds = set of TRALParamKind;
  TRALSecurityOption = (rsoBruteForceProtection, rsoFloodProtection, rsoPathTransvBlackList);
  TRALSecurityOptions = set of TRALSecurityOption;
  TRALExecBehavior = (ebSingleThread, ebMultiThread, ebDefault);
  TRALDateTimeFormat = (dtfUnix, dtfISO8601, dtfCustom);

const
  {$IF Defined(FPC) or Defined(DELPHIXE3UP)}
  POSINISTR = Low(String);
  {$ELSE}
  POSINISTR = 1;
  {$IFEND}

  // old versions of Delphi that don't have sLineBreak
  {$IF not Defined(FPC) AND not Defined(DELPHI7UP)}
  sLineBreak = #13#10;
  {$IFEND}
function RALHighStr(const AStr: StringRAL): integer;

implementation

function RALHighStr(const AStr: StringRAL): integer;
begin
  {$IF not Defined(FPC) and not Defined(DELPHIXE2UP)}
  Result := Length(AStr);
  {$ELSE}
  Result := High(AStr);
  {$IFEND}
end;

end.
