unit RALTypes;

interface

{$I ..\base\PascalRAL.inc}

// compatibility types
{
  These definitions are meant to keep same code across all versions of the IDE
  or IDEs that might differ on the charset code or basic type length.
  Expect heavy usage of IFDEFs at this unit
}

uses
  Classes, SysUtils,
  RALConsts;

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

  // nao foi encontrado TBytes no Delphi7
  // encontrado no Delphi 2010
  {$IF (NOT DEFINED(DELPHI2010UP)) AND (NOT DEFINED(FPC))}
  TBytes = array of byte;
  {$ENDIF}

  TRALJSONType = (rjtString, rjtNumber, rjtBoolean, rjtObject, rjtArray);
  TRALMethod = (amALL, amGET, amPOST, amPUT, amPATCH, amDELETE, amOPTIONS, amHEAD, amTRACE);
  TRALMethods = set of TRALMethod;
  TRALParamKind = (rpkNONE, rpkBODY, rpkFIELD, rpkHEADER, rpkQUERY, rpkCOOKIE);
  TRALParamKinds = set of TRALParamKind;
  TRALServerOption = (rsoBruteForceProtection, rsoDDoSProtection, rsoEnableBlackList, rsoEnableBlockList,
    rsoEnableWhiteList, rsoIPBroadcastProtection, rsoPathTransvBlackList);
  TRALServerOptions = set of TRALServerOption;
  TRALCompressType = (ctNone, ctDeflate, ctZLib, ctGZip);
  TRALCriptoType = (crNone, crAES);

const
  {$IFDEF HAS_FMX}
    PosIniStr = 0;
  {$ELSE}
    PosIniStr = 1;
  {$ENDIF}


implementation

end.
