/// Formatters: how an object is read from a request body and written into a
/// response, chosen by Content-Type (reading) and Accept with its q values
/// (writing). JSON, HTML forms, CSV and YAML come registered; a formatter of
/// your own registers like the storages do.
unit RALObjectFormat;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, TypInfo, DB,
  RALTypes, RALConsts, RALTools, RALJson, RALYAML, RALParams, RALRequest, RALObjectJSON;

type
  { Reads and writes objects in one media type. A class, registered with
    RALRegisterFormatter; the first registered (JSON) answers when Accept is
    empty or '*/*'. }
  TRALObjectFormatter = class
  public
    /// Whether it reads objects of AClass.
    class function CanRead(AClass: TClass): boolean; virtual;
    /// Whether it writes objects of AClass.
    class function CanWrite(AClass: TClass): boolean; virtual;
    /// Whether AMediaType (no parameters, lowercase) is one it handles.
    class function Handles(const AMediaType: StringRAL): boolean; virtual;
    /// The media type it writes ('application/json').
    class function MediaType: StringRAL; virtual; abstract;
    /// Fills AObj from the body (or the fields) of ARequest.
    class procedure Read(ARequest: TRALRequest; AObj: TObject;
      AOptions: TRALObjectJSONOptions); virtual;
    /// AObj as text in the media type.
    class function Write(AObj: TObject; AOptions: TRALObjectJSONOptions): StringRAL; virtual; abstract;
  end;

  /// A formatter class.
  TRALObjectFormatterClass = class of TRALObjectFormatter;

  /// JSON (application/json and every '+json' type).
  TRALJSONFormatter = class(TRALObjectFormatter)
  public
    class function Handles(const AMediaType: StringRAL): boolean; override;
    class function MediaType: StringRAL; override;
    class procedure Read(ARequest: TRALRequest; AObj: TObject;
      AOptions: TRALObjectJSONOptions); override;
    class function Write(AObj: TObject; AOptions: TRALObjectJSONOptions): StringRAL; override;
  end;

  { HTML forms: application/x-www-form-urlencoded and multipart/form-data read
    into the published properties by name; written urlencoded (simple
    properties only). }
  TRALFormFormatter = class(TRALObjectFormatter)
  public
    class function Handles(const AMediaType: StringRAL): boolean; override;
    class function MediaType: StringRAL; override;
    class procedure Read(ARequest: TRALRequest; AObj: TObject;
      AOptions: TRALObjectJSONOptions); override;
    class function Write(AObj: TObject; AOptions: TRALObjectJSONOptions): StringRAL; override;
  end;

  { CSV (RFC 4180, comma, header line): a collection, a registered list or a
    dataset as rows; an object alone as one row. Reads into a collection. }
  TRALCSVFormatter = class(TRALObjectFormatter)
  public
    class function CanRead(AClass: TClass): boolean; override;
    class function MediaType: StringRAL; override;
    class procedure Read(ARequest: TRALRequest; AObj: TObject;
      AOptions: TRALObjectJSONOptions); override;
    class function Write(AObj: TObject; AOptions: TRALObjectJSONOptions): StringRAL; override;
  end;

  /// YAML 1.2 (application/yaml), written only.
  TRALYAMLFormatter = class(TRALObjectFormatter)
  public
    class function CanRead(AClass: TClass): boolean; override;
    class function Handles(const AMediaType: StringRAL): boolean; override;
    class function MediaType: StringRAL; override;
    class function Write(AObj: TObject; AOptions: TRALObjectJSONOptions): StringRAL; override;
  end;

/// Registers AFormatter (after the ones already there).
procedure RALRegisterFormatter(AFormatter: TRALObjectFormatterClass);
/// Removes AFormatter.
procedure RALUnregisterFormatter(AFormatter: TRALObjectFormatterClass);
/// The formatter that reads AMediaType into AClass, or nil.
function RALFormatterForRead(const AMediaType: StringRAL; AClass: TClass): TRALObjectFormatterClass;
{ The formatter that writes AClass for the Accept header AAccept (q values
  respected, q=0 refuses), and the media type chosen; nil when none. }
function RALFormatterForWrite(const AAccept: StringRAL; AClass: TClass;
  out AMediaType: StringRAL): TRALObjectFormatterClass;
/// The media type of a Content-Type, lowercase and without parameters.
function RALBareMediaType(const AContentType: StringRAL): StringRAL;
{ The language of ASupported that the Accept-Language AHeader prefers (q values
  and prefixes: 'pt' matches 'pt-BR'); the first of ASupported when none. }
function RALPreferredLanguage(const AHeader: StringRAL; const ASupported: array of StringRAL): StringRAL;

implementation

var
  gFormatters: TList;
  { q values are written with a dot, whatever the machine's locale }
  gDot: TFormatSettings;

procedure RALRegisterFormatter(AFormatter: TRALObjectFormatterClass);
begin
  if gFormatters.IndexOf(Pointer(AFormatter)) < 0 then
    gFormatters.Add(Pointer(AFormatter));
end;

procedure RALUnregisterFormatter(AFormatter: TRALObjectFormatterClass);
begin
  gFormatters.Remove(Pointer(AFormatter));
end;

function RALBareMediaType(const AContentType: StringRAL): StringRAL;
var
  vPos: IntegerRAL;
begin
  Result := AContentType;
  vPos := Pos(';', string(Result));
  if vPos > 0 then
    Result := Copy(Result, 1, vPos - 1);
  Result := StringRAL(LowerCase(Trim(string(Result))));
end;

function RALFormatterForRead(const AMediaType: StringRAL; AClass: TClass): TRALObjectFormatterClass;
var
  vInt: IntegerRAL;
  vType: StringRAL;
begin
  vType := RALBareMediaType(AMediaType);
  if vType = '' then
    vType := 'application/json';
  for vInt := 0 to gFormatters.Count - 1 do
  begin
    Result := TRALObjectFormatterClass(gFormatters[vInt]);
    if Result.Handles(vType) and Result.CanRead(AClass) then
      Exit;
  end;
  Result := nil;
end;

type
  TAcceptRange = record
    MediaType: StringRAL;
    Quality: Double;
    Order: IntegerRAL;
  end;

function RALFormatterForWrite(const AAccept: StringRAL; AClass: TClass;
  out AMediaType: StringRAL): TRALObjectFormatterClass;
var
  vList: TStringList;
  vRanges: array of TAcceptRange;
  vInt, vJ, vPos: IntegerRAL;
  vPart, vParams: string;
  vTmp: TAcceptRange;
  vFmt: TRALObjectFormatterClass;
  vRange: StringRAL;

  function Fits(AFmt: TRALObjectFormatterClass; const ARange: StringRAL): boolean;
  var
    vMain: StringRAL;
  begin
    if (ARange = '*/*') or (ARange = '') then
      Result := True
    else if (Length(ARange) > 2) and (Copy(ARange, Length(ARange) - 1, 2) = '/*') then
    begin
      vMain := Copy(ARange, 1, Length(ARange) - 1);
      Result := Copy(AFmt.MediaType, 1, Length(vMain)) = vMain;
    end
    else
      Result := AFmt.Handles(ARange);
  end;

  function Refused(AFmt: TRALObjectFormatterClass): boolean;
  var
    vIdx: IntegerRAL;
  begin
    Result := False;
    for vIdx := 0 to High(vRanges) do
      if (vRanges[vIdx].Quality <= 0) and AFmt.Handles(vRanges[vIdx].MediaType) then
      begin
        Result := True;
        Exit;
      end;
  end;

begin
  Result := nil;
  AMediaType := '';
  vList := TStringList.Create;
  try
    vList.StrictDelimiter := True;
    vList.Delimiter := ',';
    vList.DelimitedText := string(AAccept);
    SetLength(vRanges, 0);
    for vInt := 0 to vList.Count - 1 do
    begin
      vPart := Trim(vList[vInt]);
      if vPart = '' then
        Continue;
      SetLength(vRanges, Length(vRanges) + 1);
      vRanges[High(vRanges)].Order := vInt;
      vRanges[High(vRanges)].Quality := 1;
      vPos := Pos(';', vPart);
      if vPos > 0 then
      begin
        vParams := LowerCase(Copy(vPart, vPos + 1, MaxInt));
        vPart := Copy(vPart, 1, vPos - 1);
        vPos := Pos('q=', vParams);
        if vPos > 0 then
          vRanges[High(vRanges)].Quality := StrToFloatDef(Trim(Copy(vParams, vPos + 2, 5)), 1, gDot);
      end;
      vRanges[High(vRanges)].MediaType := StringRAL(LowerCase(Trim(vPart)));
    end;
  finally
    vList.Free;
  end;
  if Length(vRanges) = 0 then
  begin
    SetLength(vRanges, 1);
    vRanges[0].MediaType := '*/*';
    vRanges[0].Quality := 1;
  end;
  { stable sort by quality, highest first }
  for vInt := 1 to High(vRanges) do
  begin
    vTmp := vRanges[vInt];
    vJ := vInt - 1;
    while (vJ >= 0) and (vRanges[vJ].Quality < vTmp.Quality) do
    begin
      vRanges[vJ + 1] := vRanges[vJ];
      Dec(vJ);
    end;
    vRanges[vJ + 1] := vTmp;
  end;
  for vInt := 0 to High(vRanges) do
  begin
    if vRanges[vInt].Quality <= 0 then
      Continue;
    vRange := vRanges[vInt].MediaType;
    for vJ := 0 to gFormatters.Count - 1 do
    begin
      vFmt := TRALObjectFormatterClass(gFormatters[vJ]);
      if Fits(vFmt, vRange) and vFmt.CanWrite(AClass) then
      begin
        { a media type refused with q=0 elsewhere in the list stays refused }
        if Refused(vFmt) then
          Continue;
        Result := vFmt;
        if (Pos('*', string(vRange)) > 0) then
          AMediaType := vFmt.MediaType
        else
          AMediaType := vRange;
        Exit;
      end;
    end;
  end;
end;

function RALPreferredLanguage(const AHeader: StringRAL; const ASupported: array of StringRAL): StringRAL;
var
  vList: TStringList;
  vInt, vJ, vPos: IntegerRAL;
  vBest: Double;
  vPart, vTag: string;
  vQ: Double;
begin
  Result := '';
  if Length(ASupported) > 0 then
    Result := ASupported[0];
  vBest := 0;
  vList := TStringList.Create;
  try
    vList.StrictDelimiter := True;
    vList.Delimiter := ',';
    vList.DelimitedText := string(AHeader);
    for vInt := 0 to vList.Count - 1 do
    begin
      vPart := Trim(vList[vInt]);
      vQ := 1;
      vPos := Pos(';', vPart);
      if vPos > 0 then
      begin
        vTag := LowerCase(Copy(vPart, vPos + 1, MaxInt));
        vPart := Copy(vPart, 1, vPos - 1);
        vPos := Pos('q=', vTag);
        if vPos > 0 then
          vQ := StrToFloatDef(Trim(Copy(vTag, vPos + 2, 5)), 1, gDot);
      end;
      vTag := LowerCase(Trim(vPart));
      if (vQ <= vBest) or (vTag = '') then
        Continue;
      for vJ := 0 to High(ASupported) do
        if (vTag = '*') or (LowerCase(string(ASupported[vJ])) = vTag) or
           (Pos(vTag + '-', LowerCase(string(ASupported[vJ]))) = 1) or
           (Pos(LowerCase(string(ASupported[vJ])) + '-', vTag) = 1) then
        begin
          Result := ASupported[vJ];
          vBest := vQ;
          Break;
        end;
    end;
  finally
    vList.Free;
  end;
end;

{ TRALObjectFormatter }

class function TRALObjectFormatter.CanRead(AClass: TClass): boolean;
begin
  Result := True;
end;

class function TRALObjectFormatter.CanWrite(AClass: TClass): boolean;
begin
  Result := True;
end;

class function TRALObjectFormatter.Handles(const AMediaType: StringRAL): boolean;
begin
  Result := AMediaType = MediaType;
end;

class procedure TRALObjectFormatter.Read(ARequest: TRALRequest; AObj: TObject;
  AOptions: TRALObjectJSONOptions);
begin
  raise ERALObjectJSON.CreateFmt('%s does not read', [ClassName]);
end;

{ TRALJSONFormatter }

class function TRALJSONFormatter.Handles(const AMediaType: StringRAL): boolean;
begin
  Result := (AMediaType = 'application/json') or (AMediaType = 'text/json') or
    ((Length(AMediaType) > 5) and (Copy(AMediaType, Length(AMediaType) - 4, 5) = '+json'));
end;

class function TRALJSONFormatter.MediaType: StringRAL;
begin
  Result := 'application/json';
end;

class procedure TRALJSONFormatter.Read(ARequest: TRALRequest; AObj: TObject;
  AOptions: TRALObjectJSONOptions);
begin
  TRALObjectJSON.FromText(ARequest.RequestText, AObj, AOptions);
end;

class function TRALJSONFormatter.Write(AObj: TObject; AOptions: TRALObjectJSONOptions): StringRAL;
begin
  Result := TRALObjectJSON.ToText(AObj, AOptions);
end;

{ TRALFormFormatter }

class function TRALFormFormatter.Handles(const AMediaType: StringRAL): boolean;
begin
  Result := (AMediaType = 'application/x-www-form-urlencoded') or
    (AMediaType = 'multipart/form-data');
end;

class function TRALFormFormatter.MediaType: StringRAL;
begin
  Result := 'application/x-www-form-urlencoded';
end;

class procedure TRALFormFormatter.Read(ARequest: TRALRequest; AObj: TObject;
  AOptions: TRALObjectJSONOptions);
var
  vJSON: TRALJSONObject;
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  { the fields as a JSON object of strings: the serializer converts the types }
  vJSON := TRALJSONObject.Create;
  try
    for vInt := 0 to ARequest.Params.Count - 1 do
    begin
      vParam := ARequest.Params.Index[vInt];
      if vParam.Kind in [rpkFIELD, rpkBODY] then
        vJSON.Add(vParam.ParamName, vParam.AsString);
    end;
    TRALObjectJSON.FromJSON(vJSON, AObj, AOptions);
  finally
    vJSON.Free;
  end;
end;

function URLEncode(const AText: StringRAL): StringRAL;
const
  cHex: StringRAL = '0123456789ABCDEF';
var
  vInt: IntegerRAL;
  vChar: AnsiChar;
begin
  Result := '';
  for vInt := POSINISTR to RALHighStr(AText) do
  begin
    vChar := AText[vInt];
    if vChar in ['A'..'Z', 'a'..'z', '0'..'9', '-', '.', '_', '~'] then
      Result := Result + vChar
    else if vChar = ' ' then
      Result := Result + '+'
    else
      Result := Result + '%' + cHex[POSINISTR + (Ord(vChar) shr 4)] +
        cHex[POSINISTR + (Ord(vChar) and 15)];
  end;
end;

class function TRALFormFormatter.Write(AObj: TObject; AOptions: TRALObjectJSONOptions): StringRAL;
var
  vJSON: TRALJSONValue;
  vObj: TRALJSONObject;
  vInt: IntegerRAL;
  vValue: TRALJSONValue;
begin
  Result := '';
  vJSON := TRALObjectJSON.ToJSON(AObj, AOptions);
  try
    if not (vJSON is TRALJSONObject) then
      Exit;
    vObj := TRALJSONObject(vJSON);
    for vInt := 0 to vObj.Count - 1 do
    begin
      vValue := vObj.Get(vInt);
      if vValue.IsNull or (vValue.JsonType in [rjtObject, rjtArray]) then
        Continue;
      if Result <> '' then
        Result := Result + '&';
      Result := Result + URLEncode(vObj.GetName(vInt)) + '=' + URLEncode(vValue.AsString);
    end;
  finally
    vJSON.Free;
  end;
end;

{ TRALCSVFormatter }

function CSVField(const AText: StringRAL): StringRAL;
begin
  if (Pos(',', string(AText)) > 0) or (Pos('"', string(AText)) > 0) or
     (Pos(#10, string(AText)) > 0) or (Pos(#13, string(AText)) > 0) then
    Result := '"' + StringRAL(StringReplace(string(AText), '"', '""', [rfReplaceAll])) + '"'
  else
    Result := AText;
end;

class function TRALCSVFormatter.CanRead(AClass: TClass): boolean;
begin
  Result := AClass.InheritsFrom(TCollection);
end;

class function TRALCSVFormatter.MediaType: StringRAL;
begin
  Result := 'text/csv';
end;

class function TRALCSVFormatter.Write(AObj: TObject; AOptions: TRALObjectJSONOptions): StringRAL;
var
  vJSON: TRALJSONValue;
  vRows: TRALJSONArray;
  vOwnRows: boolean;
  vHeader: TStringList;
  vInt, vCol: IntegerRAL;
  vRow: TRALJSONObject;
  vValue: TRALJSONValue;
  vLine: StringRAL;
begin
  Result := '';
  vJSON := TRALObjectJSON.ToJSON(AObj, AOptions);
  vOwnRows := False;
  vRows := nil;
  vHeader := TStringList.Create;
  try
    if vJSON is TRALJSONArray then
      vRows := TRALJSONArray(vJSON)
    else
    begin
      vRows := TRALJSONArray.Create;
      vOwnRows := True;
      vRows.Add(TRALJSON.ParseJSON(vJSON.ToJson));
    end;
    { the header: every member name, in the order first seen }
    for vInt := 0 to vRows.Count - 1 do
      if vRows.Get(vInt) is TRALJSONObject then
      begin
        vRow := TRALJSONObject(vRows.Get(vInt));
        for vCol := 0 to vRow.Count - 1 do
          if vHeader.IndexOf(string(vRow.GetName(vCol))) < 0 then
            vHeader.Add(string(vRow.GetName(vCol)));
      end;
    vLine := '';
    for vCol := 0 to vHeader.Count - 1 do
    begin
      if vCol > 0 then
        vLine := vLine + ',';
      vLine := vLine + CSVField(StringRAL(vHeader[vCol]));
    end;
    Result := vLine + #13#10;
    for vInt := 0 to vRows.Count - 1 do
    begin
      if not (vRows.Get(vInt) is TRALJSONObject) then
        Continue;
      vRow := TRALJSONObject(vRows.Get(vInt));
      vLine := '';
      for vCol := 0 to vHeader.Count - 1 do
      begin
        if vCol > 0 then
          vLine := vLine + ',';
        vValue := vRow.Get(StringRAL(vHeader[vCol]));
        if (vValue = nil) or vValue.IsNull then
          Continue;
        if vValue.JsonType in [rjtObject, rjtArray] then
          vLine := vLine + CSVField(vValue.ToJson)
        else if vValue.JsonType = rjtNumber then
          vLine := vLine + RALJSONNumberText(vValue)
        else
          vLine := vLine + CSVField(vValue.AsString);
      end;
      Result := Result + vLine + #13#10;
    end;
  finally
    vHeader.Free;
    if vOwnRows then
      vRows.Free;
    vJSON.Free;
  end;
end;

{ splits CSV text into rows of fields (RFC 4180) }
procedure ParseCSV(const AText: StringRAL; ARows: TList);
var
  vInt, vLen: IntegerRAL;
  vField: StringRAL;
  vRow: TStringList;
  vQuoted: boolean;
  vChar: AnsiChar;
begin
  vLen := Length(AText);
  vInt := POSINISTR;
  vRow := TStringList.Create;
  vField := '';
  vQuoted := False;
  while vInt <= vLen + POSINISTR - 1 do
  begin
    vChar := AText[vInt];
    if vQuoted then
    begin
      if vChar = '"' then
      begin
        if (vInt < vLen + POSINISTR - 1) and (AText[vInt + 1] = '"') then
        begin
          vField := vField + '"';
          Inc(vInt);
        end
        else
          vQuoted := False;
      end
      else
        vField := vField + vChar;
    end
    else if vChar = '"' then
      vQuoted := True
    else if vChar = ',' then
    begin
      vRow.Add(string(vField));
      vField := '';
    end
    else if (vChar = #13) or (vChar = #10) then
    begin
      if (vChar = #13) and (vInt < vLen + POSINISTR - 1) and (AText[vInt + 1] = #10) then
        Inc(vInt);
      vRow.Add(string(vField));
      vField := '';
      ARows.Add(vRow);
      vRow := TStringList.Create;
    end
    else
      vField := vField + vChar;
    Inc(vInt);
  end;
  if (vField <> '') or (vRow.Count > 0) then
  begin
    vRow.Add(string(vField));
    ARows.Add(vRow);
  end
  else
    vRow.Free;
end;

class procedure TRALCSVFormatter.Read(ARequest: TRALRequest; AObj: TObject;
  AOptions: TRALObjectJSONOptions);
var
  vRows: TList;
  vArr: TRALJSONArray;
  vObj: TRALJSONObject;
  vHeader, vRow: TStringList;
  vInt, vCol: IntegerRAL;
begin
  vRows := TList.Create;
  vArr := TRALJSONArray.Create;
  try
    ParseCSV(ARequest.RequestText, vRows);
    if vRows.Count = 0 then
      Exit;
    vHeader := TStringList(vRows[0]);
    for vInt := 1 to vRows.Count - 1 do
    begin
      vRow := TStringList(vRows[vInt]);
      vObj := TRALJSONObject.Create;
      for vCol := 0 to vHeader.Count - 1 do
        if vCol < vRow.Count then
          vObj.Add(StringRAL(vHeader[vCol]), StringRAL(vRow[vCol]));
      vArr.Add(vObj);
    end;
    TRALObjectJSON.FromJSON(vArr, AObj, AOptions);
  finally
    for vInt := 0 to vRows.Count - 1 do
      TObject(vRows[vInt]).Free;
    vRows.Free;
    vArr.Free;
  end;
end;


class function TRALYAMLFormatter.CanRead(AClass: TClass): boolean;
begin
  Result := False;
end;

class function TRALYAMLFormatter.Handles(const AMediaType: StringRAL): boolean;
begin
  Result := (AMediaType = 'application/yaml') or (AMediaType = 'text/yaml') or
    (AMediaType = 'application/x-yaml');
end;

class function TRALYAMLFormatter.MediaType: StringRAL;
begin
  Result := 'application/yaml';
end;

class function TRALYAMLFormatter.Write(AObj: TObject; AOptions: TRALObjectJSONOptions): StringRAL;
var
  vJSON: TRALJSONValue;
begin
  vJSON := TRALObjectJSON.ToJSON(AObj, AOptions);
  try
    Result := RALJSONToYAML(vJSON);
  finally
    vJSON.Free;
  end;
end;

initialization
  gDot := {$IFDEF FPC}DefaultFormatSettings{$ELSE}FormatSettings{$ENDIF};
  gDot.DecimalSeparator := '.';
  gFormatters := TList.Create;
  RALRegisterFormatter(TRALJSONFormatter);
  RALRegisterFormatter(TRALFormFormatter);
  RALRegisterFormatter(TRALCSVFormatter);
  RALRegisterFormatter(TRALYAMLFormatter);

finalization
  FreeAndNil(gFormatters);

end.
