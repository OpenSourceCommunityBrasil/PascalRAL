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
  UInt64RAL = UInt64;

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
  TRALStringStream = class(TMemoryStream)
  public
    constructor Create(AString: StringRAL); overload;
    constructor Create(AStream: TStream); overload;
    constructor Create(ABytes: TBytes); overload;

    function DataString: StringRAL;

    procedure WriteBytes(ABytes: TBytes);
    procedure WriteString(AString : StringRAL);
    procedure WriteStream(AStream: TStream);
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

function StringToBytes(const AString: StringRAL): TBytes;
function BytesToString(const ABytes: TBytes): StringRAL;

{$IF NOT Defined(FPC) AND NOT Defined(DELPHIXE6UP)}
function DateToISO8601(const AValue: TDateTime): StringRAL;
function ISO8601ToDate(const AValue: StringRAL): TDateTime;
{$IFEND}

implementation

function RALHighStr(const AStr: StringRAL): integer;
begin
  {$IF NOT Defined(FPC) AND NOT Defined(DELPHIXE3UP)}
  Result := Length(AStr);
  {$ELSE}
  Result := High(AStr);
  {$IFEND}
end;

function StringToBytes(const AString: StringRAL): TBytes;
{$IFNDEF HAS_Encoding}
  var
    vStr : ansistring;
{$ENDIF}
begin
  {$IFDEF HAS_Encoding}
    Result := TEncoding.UTF8.GetBytes(AString);
  {$ELSE}
    vStr := Uft8Encode(AString);
    SetLength(Result, Length(vStr));
    Move(vStr[POSINISTR], Result[0], Length(vStr));
  {$ENDIF}
end;

function BytesToString(const ABytes: TBytes): StringRAL;
{$IFNDEF HAS_Encoding}
  var
    vStr : ansistring;
{$ENDIF}
begin
  {$IFDEF HAS_Encoding}
    Result := TEncoding.UTF8.GetString(ABytes);
  {$ELSE}
    SetLength(vStr, Length(ABytes));
    Move(ABytes[0], vStr[POSINISTR], Length(ABytes));
    Result := Utf8Decode(vStr);
  {$ENDIF}
end;

function StringToStream(const AStr: StringRAL): TStream;
begin
  Result := TRALStringStream.Create(AStr);
  Result.Position := 0;
end;

function StreamToString(AStream: TStream): StringRAL;
var
  vStream: TStream;
begin
  Result := '';
  if (AStream = nil) or (AStream.Size = 0) then
    Exit;

  AStream.Position := 0;

  if AStream.InheritsFrom(TStringStream) then
  begin
    Result := TStringStream(AStream).DataString;
  end
  else if AStream.InheritsFrom(TRALStringStream) then
  begin
    Result := TRALStringStream(AStream).DataString;
  end
  else
  begin
    vStream := TStringStream.Create;
    try
      vStream.CopyFrom(AStream, AStream.Size);
      Result := TStringStream(vStream).DataString;
    finally
      FreeAndNil(vStream);
    end;
  end;
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

constructor TRALStringStream.Create(ABytes: TBytes);
begin
  inherited Create;
  WriteBytes(ABytes);
end;

constructor TRALStringStream.Create(AString: StringRAL);
var
  vBytes: TBytes;
begin
  inherited Create;
  WriteString(AString);
end;

constructor TRALStringStream.Create(AStream: TStream);
var
  vStream: TStringStream;
  vBytes: TBytes;
begin
  inherited Create;
  WriteStream(AStream);
end;

function TRALStringStream.DataString: StringRAL;
var
  vBytes: TBytes;
begin
  Self.Position := 0;

  SetLength(vBytes, Self.Size);
  Read(vBytes[0], Self.Size);
  Result := BytesToString(vBytes);
end;

procedure TRALStringStream.WriteBytes(ABytes: TBytes);
begin
  Write(ABytes[0], Length(ABytes));
end;

procedure TRALStringStream.WriteString(AString: StringRAL);
var
  vBytes : TBytes;
begin
  vBytes := StringToBytes(AString);
  WriteBytes(vBytes);
end;

procedure TRALStringStream.WriteStream(AStream: TStream);
var
  vBytes : TBytes;
begin
  AStream.Position := 0;
  SetLength(vBytes, AStream.Size);
  AStream.Read(vBytes[0], AStream.Size);
  WriteBytes(vBytes);
end;

end.
