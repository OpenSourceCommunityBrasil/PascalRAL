unit RALTypes;

interface

{$I ..\base\PascalRAL.inc}

// compatibility types
{
  These definitions are meant to keep same code across all versions of the IDE
  or IDEs that might differ on the charset code or basic type length.
  Expect heavy usage of IFDEFs at this point
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

  TRALMethod = (amALL, amGET, amPOST, amPUT, amPATCH, amDELETE, amOPTION, amHEAD, amTRACE);
  TRALMethods = set of TRALMethod;
  TRALParamKind = (rpkNONE, rpkBODY, rpkFIELD, rpkHEADER, rpkQUERY, rpkCOOKIE);
  TRALParamKinds = set of TRALParamKind;
  TRALJSONType = (rjtString, rjtNumber, rjtBoolean, rjtObject, rjtArray);

const
  PosIniStr = 1;

implementation

end.
