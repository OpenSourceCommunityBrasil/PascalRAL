{ YAML 1.2 written from a parsed JSON value (block style), for the API
  documentation (swagger.yaml) and the YAML formatter of objects. Writing
  only. }
unit RALYAML;

{$I ..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils,
  RALTypes, RALJson;

/// A parsed JSON value as YAML 1.2 text (block style, LF line ends).
function RALJSONToYAML(AValue: TRALJSONValue): StringRAL;
{ A JSON number as short text, the same on every JSON backend: an integer
  without decimals, else the shortest exact form with '.' (12.5, not 1.25E+001) }
function RALJSONNumberText(AValue: TRALJSONValue): StringRAL;

implementation

function RALJSONNumberText(AValue: TRALJSONValue): StringRAL;
var
  vValue: Double;
  vFmt: TFormatSettings;
begin
  vValue := AValue.AsFloat;
  if (Frac(vValue) = 0) and (Abs(vValue) < 1E15) then
    Result := StringRAL(IntToStr(Trunc(vValue)))
  else
  begin
    vFmt := {$IFDEF FPC}DefaultFormatSettings{$ELSE}FormatSettings{$ENDIF};
    vFmt.DecimalSeparator := '.';
    Result := StringRAL(FloatToStrF(vValue, ffGeneral, 15, 0, vFmt));
  end;
end;

/// AText as a JSON string literal, which is also a YAML double-quoted scalar.
function YAMLQuote(const AText: StringRAL): StringRAL;
const
  cHex: StringRAL = '0123456789abcdef';
var
  vInt: IntegerRAL;
  vCode: Byte;
begin
  Result := '"';
  for vInt := POSINISTR to RALHighStr(AText) do
  begin
    vCode := Ord(AText[vInt]);
    case vCode of
      Ord('"'): Result := Result + '\"';
      Ord('\'): Result := Result + '\\';
      9: Result := Result + '\t';
      10: Result := Result + '\n';
      13: Result := Result + '\r';
      0..8, 11, 12, 14..31:
        Result := Result + '\u00' + cHex[POSINISTR + (vCode shr 4)] + cHex[POSINISTR + (vCode and 15)];
    else
      Result := Result + AText[vInt];
    end;
  end;
  Result := Result + '"';
end;


/// A scalar as YAML text, quoted when plain text would read as something else.
function YAMLScalar(AValue: TRALJSONValue): StringRAL;
var
  vText: StringRAL;
  vInt: IntegerRAL;
  vPlain: boolean;
begin
  if AValue.IsNull then
  begin
    Result := 'null';
    Exit;
  end;
  case AValue.JsonType of
    rjtNumber:
      Result := RALJSONNumberText(AValue);
    rjtBoolean:
      Result := StringRAL(LowerCase(string(AValue.ToJson)));
  else
    vText := AValue.AsString;
    { plain when nothing in it could be read as another type or as syntax }
    vPlain := (vText <> '') and not (vText[POSINISTR] in ['-', '?', ':', ',', '[', ']', '{',
      '}', '#', '&', '*', '!', '|', '>', '''', '"', '%', '@', '`', ' ', '0'..'9', '.', '+']);
    if vPlain then
      for vInt := POSINISTR to RALHighStr(vText) do
        if (vText[vInt] < ' ') or (vText[vInt] = '#') or ((vText[vInt] = ':') and
           ((vInt = RALHighStr(vText)) or (vText[vInt + 1] = ' '))) then
        begin
          vPlain := False;
          Break;
        end;
    if vPlain and (vText[RALHighStr(vText)] = ' ') then
      vPlain := False;
    if vPlain then
    begin
      vText := StringRAL(LowerCase(string(vText)));
      vPlain := (vText <> 'true') and (vText <> 'false') and (vText <> 'null') and
        (vText <> 'yes') and (vText <> 'no') and (vText <> '~');
    end;
    if vPlain then
      Result := AValue.AsString
    else
      Result := YAMLQuote(AValue.AsString);
  end;
end;

/// Appends AValue to AOut as YAML, nested at AIndent.
procedure YAMLWrite(AValue: TRALJSONValue; const AIndent: StringRAL; var AOut: StringRAL;
  AInlineFirst: boolean);
var
  vInt: IntegerRAL;
  vObj: TRALJSONObject;
  vArr: TRALJSONArray;
  vChild: TRALJSONValue;
  vPrefix: StringRAL;
begin
  if (AValue is TRALJSONObject) and (TRALJSONObject(AValue).Count > 0) then
  begin
    vObj := TRALJSONObject(AValue);
    for vInt := 0 to vObj.Count - 1 do
    begin
      if (vInt > 0) or not AInlineFirst then
        vPrefix := AIndent
      else
        vPrefix := '';
      vChild := vObj.Get(vInt);
      AOut := AOut + vPrefix;
      if (Pos(':', string(vObj.GetName(vInt))) > 0) or (Pos(' ', string(vObj.GetName(vInt))) > 0) or
         (vObj.GetName(vInt) = '') or (vObj.GetName(vInt)[POSINISTR] in ['/', '-', '#', '0'..'9']) then
        AOut := AOut + YAMLQuote(vObj.GetName(vInt)) + ':'
      else
        AOut := AOut + vObj.GetName(vInt) + ':';
      if ((vChild is TRALJSONObject) and (TRALJSONObject(vChild).Count > 0)) then
      begin
        AOut := AOut + #10;
        YAMLWrite(vChild, AIndent + '  ', AOut, False);
      end
      else if (vChild is TRALJSONArray) and (TRALJSONArray(vChild).Count > 0) then
      begin
        AOut := AOut + #10;
        YAMLWrite(vChild, AIndent, AOut, False);
      end
      else if vChild is TRALJSONObject then
        AOut := AOut + ' {}'#10
      else if vChild is TRALJSONArray then
        AOut := AOut + ' []'#10
      else
        AOut := AOut + ' ' + YAMLScalar(vChild) + #10;
    end;
  end
  else if (AValue is TRALJSONArray) and (TRALJSONArray(AValue).Count > 0) then
  begin
    vArr := TRALJSONArray(AValue);
    for vInt := 0 to vArr.Count - 1 do
    begin
      if (vInt > 0) or not AInlineFirst then
        AOut := AOut + AIndent;
      AOut := AOut + '- ';
      vChild := vArr.Get(vInt);
      if (vChild is TRALJSONObject) and (TRALJSONObject(vChild).Count > 0) then
        YAMLWrite(vChild, AIndent + '  ', AOut, True)
      else if (vChild is TRALJSONArray) and (TRALJSONArray(vChild).Count > 0) then
        YAMLWrite(vChild, AIndent + '  ', AOut, True)
      else if vChild is TRALJSONObject then
        AOut := AOut + '{}'#10
      else if vChild is TRALJSONArray then
        AOut := AOut + '[]'#10
      else
        AOut := AOut + YAMLScalar(vChild) + #10;
    end;
  end
  else if AValue is TRALJSONObject then
    AOut := AOut + AIndent + '{}'#10
  else if AValue is TRALJSONArray then
    AOut := AOut + AIndent + '[]'#10
  else
    AOut := AOut + AIndent + YAMLScalar(AValue) + #10;
end;

function RALJSONToYAML(AValue: TRALJSONValue): StringRAL;
begin
  Result := '';
  if AValue <> nil then
    YAMLWrite(AValue, '', Result, False);
end;

end.
