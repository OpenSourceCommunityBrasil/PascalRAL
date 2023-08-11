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
  {$IF DEFINED(FPC)}
   httpprotocol,
  {$ELSEIF Defined(DELPHIXE7UP)}
  NetEncoding,
  {$ELSE}
  HttpApp,
  {$IFEND}
  Classes,
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
  TRALParamKind = (rpkNONE, rpkBODY, rpkFIELD, rpkHEADER, rpkQUERY);
  TRALParamKinds = set of TRALParamKind;

  TRALComponent = class(TComponent)
  private
    function getVersion: string;
  published
    property Version: string read getVersion;
  end;

function RALHTTPDecode(const AStr: string): string;

const
  PosIniStr = 1;

implementation

function RALHTTPDecode(const AStr: string): string;
begin
  {$IFDEF DELPHIXE7UP}
  Result := TNetEncoding.URL.Decode(AStr);
  {$ELSE}
  Result := HTTPDecode(AStr);
  {$ENDIF}
end;

{ TRALComponent }

function TRALComponent.getVersion: string;
begin
  Result := RALVERSION;
end;

end.
