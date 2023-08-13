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
  TRALParamKind = (rpkNONE, rpkBODY, rpkFIELD, rpkHEADER, rpkQUERY);
  TRALParamKinds = set of TRALParamKind;
  TRALJSONType = (rjtString, rjtNumber, rjtBoolean, rjtObject, rjtArray);

  TRALComponent = class(TComponent)
  private
    function getVersion: string;
  published
    property Version: string read getVersion;
  end;

  { TRALStringList }

  TRALStringList = class(TStringList)
  private
    function FindNameSeparator(ASource : StringRAL) : StringRAL;
    procedure AppendLine(ALine, ANameSeparator : StringRAL);
  public
    procedure AppendList(ASource: TStringList; ANameSeparator : StringRAL = ''); overload;
    procedure AppendList(ASource: TStrings; ANameSeparator : StringRAL = ''); overload;
    procedure AppendListText(ASource: StringRAL; ANameSeparator : StringRAL = '');
    procedure AppendQuery(ASource: StringRAL; ANameSeparator : StringRAL = '='; ALineSeparator : StringRAL = '&');
  end;

const
  PosIniStr = 1;

implementation

uses
  RALTools, RALUrlCoder;

{ TRALStringList }

function TRALStringList.FindNameSeparator(ASource : StringRAL) : StringRAL;
var
  vPos, vMin : IntegerRAL;
begin
  vMin := Length(ASource);
  vPos := Pos('=', Result);
  if (vPos > 0) and (vPos < vMin) then
    Result := '=';

  vPos := Pos(': ', Result);
  if (vPos > 0) and (vPos < vMin) then
    Result := ': ';
end;

procedure TRALStringList.AppendLine(ALine, ANameSeparator : StringRAL);
var
  vPos: SizeInt;
  vName, vValue: StringRAL;
begin
  if ALine = '' then
    Exit;

  vPos := Pos(ANameSeparator, ALine);

  vName := Copy(ALine, RALLowStr(ALine), vPos-1);
  vName := TRALHTTPCoder.DecodeURL(vName);

  vValue := Copy(ALine, vPos+Length(ANameSeparator), Length(ALine));
  vValue := TRALHTTPCoder.DecodeURL(vValue);

  Add(vName + '=' + vValue);
end;

procedure TRALStringList.AppendList(ASource : TStringList; ANameSeparator : StringRAL);
begin
  AppendList(TStrings(ASource), ANameSeparator);
end;

procedure TRALStringList.AppendList(ASource : TStrings; ANameSeparator : StringRAL);
var
  vInt: IntegerRAL;
  vStr: StringRAL;
begin
  if (ANameSeparator = '') and (ASource.Count > 0) then
    ANameSeparator := FindNameSeparator(ASource.Strings[0]);

  for vInt := 0 to Pred(ASource.Count) do
  begin
    vStr := ASource.Strings[vInt];
    AppendLine(vStr, ANameSeparator);
  end;
end;

procedure TRALStringList.AppendListText(ASource : StringRAL; ANameSeparator : StringRAL);
var
  vInt, vPos : IntegerRAL;
  vLine : StringRAL;
  vIs13 : boolean;
begin
  if (ANameSeparator = '') and (ASource <> '') then
    ANameSeparator := FindNameSeparator(ASource);

  vLine := '';
  for vInt := RALLowStr(ASource) to RALHighStr(ASource) do
  begin
    if ASource[vInt] = #13 then
    begin
      AppendLine(vLine, ANameSeparator);
      vIs13 := True;
      vLine := '';
    end
    else if ASource[vInt] = #10 then
    begin
      if not vIs13 then
        AppendLine(vLine, ANameSeparator);
      vIs13 := False;
      vLine := '';
    end
    else
    begin
      vLine := vLine + ASource[vInt];
      vIs13 := False;
    end;
  end;
end;

procedure TRALStringList.AppendQuery(ASource : StringRAL; ANameSeparator : StringRAL; ALineSeparator : StringRAL);
var
  vLine : StringRAL;
  vInt : IntegerRAL;
begin
  vLine := '';
  vInt := RALLowStr(ASource);
  while vInt <= RALHighStr(ASource) do
  begin
    if Copy(ASource, vInt, Length(ALineSeparator)) = ALineSeparator then
    begin
      AppendLine(vLine, ANameSeparator);
      vLine := '';
      vInt := vInt + Length(ALineSeparator) - 1;
    end
    else
    begin
      vLine := vLine + ASource[vInt];
    end;
    vInt := vInt + 1;
  end;
end;

{ TRALComponent }

function TRALComponent.getVersion: string;
begin
  Result := RALVERSION;
end;

end.
