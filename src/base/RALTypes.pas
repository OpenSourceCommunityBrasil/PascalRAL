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

  {$IF Defined(FPC) OR NOT Defined(DELPHIXE3UP)}
    StringRAL = string;
    CharRAL = Char;
  {$ELSE}
    StringRAL = WideString;
    CharRAL = WideChar;
  {$IFEND}
  PCharRAL = ^CharRAL;

  {$IF NOT DEFINED(FPC) AND NOT DEFINED(DELPHI2010UP)}
  TBytes = array of byte;
  {$IFEND}

  {$IF DEFINED(DELPHI10_1UP) OR DEFINED(FPC)}
  TRALBufFileStream = TBufferedFileStream;
  {$ELSE}
  TRALBufFileStream = TFileStream;
  {$IFEND}

  TRALCriptoType = (crNone, crAES128, crAES192, crAES256);
  TRALJSONType = (rjtString, rjtNumber, rjtBoolean, rjtObject, rjtArray);
  TRALMethod = (amALL, amGET, amPOST, amPUT, amPATCH, amDELETE, amOPTIONS,
    amHEAD, amTRACE);
  TRALMethods = set of TRALMethod;
  TRALParamKind = (rpkNONE, rpkBODY, rpkFIELD, rpkHEADER, rpkQUERY, rpkCOOKIE);
  TRALParamKinds = set of TRALParamKind;
  TRALSecurityOption = (rsoBruteForceProtection, rsoFloodProtection,
    rsoPathTransvBlackList);
  TRALSecurityOptions = set of TRALSecurityOption;
  TRALExecBehavior = (ebSingleThread, ebMultiThread, ebDefault);
  TRALDateTimeFormat = (dtfUnix, dtfISO8601, dtfCustom);

  // TRALStringStream = TStringStream;
  TRALStringStream = class(TStringStream)
  public
    constructor Create(AString: StringRAL); overload;
    constructor Create(ABytes: TBytes); overload;
  end;

const
  {$IF Defined(FPC) OR Defined(DELPHIXE3UP)}
  POSINISTR = Low(String);
  {$ELSE}
  POSINISTR = 1;
  {$IFEND}

  // old versions of Delphi that don't have sLineBreak
  {$IF NOT Defined(FPC) AND NOT Defined(DELPHI7UP)}
  sLineBreak = #13#10;
  {$IFEND}
  // Returns the last position of a string
function RALHighStr(const AStr: StringRAL): integer;
// Converts a given AStream to an UTF8String
function StreamToString(AStream: TStream): StringRAL;
// Creates a TStream and writes the given AStr into it
function StringToStream(const AStr: StringRAL): TStream;
{$IFNDEF DELPHIXE6UP}
function DateToISO8601(const AValue: TDateTime): StringRAL;
function ISO8601ToDate(const AValue: StringRAL): TDateTime;
{$ENDIF}

implementation

function RALHighStr(const AStr: StringRAL): integer;
begin
  {$IF NOT Defined(FPC) AND NOT Defined(DELPHIXE3UP)}
  Result := Length(AStr);
  {$ELSE}
  Result := High(AStr);
  {$IFEND}
end;

function StringToStream(const AStr: StringRAL): TStream;
begin
  Result := TRALStringStream.Create(AStr);
  Result.Position := 0;
end;

function StreamToString(AStream: TStream): StringRAL;
var
  vStream: TStringStream;
begin
  Result := '';
  if (AStream = nil) or (AStream.Size = 0) then
    Exit;

  AStream.Position := 0;

  if AStream.InheritsFrom(TStringStream) then
  begin
    Result := TStringStream(AStream).DataString;
  end
  else
  begin
    vStream := TStringStream.Create;
    try
      vStream.CopyFrom(AStream, AStream.Size);
      Result := vStream.DataString;
    finally
      FreeAndNil(vStream);
    end;
  end

  // fonte antigo, guardando pra limpar depois
  {
    else if AStream.InheritsFrom(TMemoryStream) then
    begin
    SetLength(Result, AStream.Size);
    Move(TMemoryStream(AStream).Memory^, Result[PosIniStr], AStream.Size);
    end
    else
    begin
    SetLength(Result, AStream.Size);
    AStream.Read(Result[PosIniStr], AStream.Size);
    end;
  }
end;

{$IF NOT Defined(FPC) AND NOT Defined(DELPHIXE6UP)}
function DateToISO8601(const AValue: TDateTime): StringRAL;
var
  vFmt: TFormatSettings;
begin
  vFmt.DateSeparator := '-';
  vFmt.ShortDateFormat := 'yyyy-mm-dd';
  vFmt.TimeSeparator := ':';
  vFmt.ShortTimeFormat := 'hh:nn:ss';
  vFmt.LongTimeFormat := 'hh:nn:ss.zzz';

  Result := StringReplace(DateToStr(AValue, vFmt), ' ', 'T', []);
end;

function ISO8601ToDate(const AValue: StringRAL): TDateTime;
var
  vFmt: TFormatSettings;
begin
  vFmt.DateSeparator := '-';
  vFmt.ShortDateFormat := 'yyyy-mm-dd';
  vFmt.TimeSeparator := ':';
  vFmt.ShortTimeFormat := 'hh:nn:ss';
  vFmt.LongTimeFormat := 'hh:nn:ss.zzz';

  Result := StrToDate(StringReplace(AValue, 'T', ' ', []), vFmt);
end;
{$IFEND}
{ TRALStringStream }

constructor TRALStringStream.Create(AString: StringRAL);
begin
  {$IF NOT Defined(FPC) AND NOT Defined(DELPHIXE5UP)}
  inherited Create(AString);
  {$ELSE}
  inherited Create(AString, TEncoding.UTF8);
  {$IFEND}
end;

constructor TRALStringStream.Create(ABytes: TBytes);
begin
  inherited Create(ABytes);
end;

end.
