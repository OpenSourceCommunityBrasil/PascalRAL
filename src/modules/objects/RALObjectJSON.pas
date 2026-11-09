/// Objects to JSON and back through their published properties (TypInfo), the
/// same on every Delphi from XE and on FPC 3.2: simple types, enumerations,
/// sets, TDateTime, nested TPersistent, TCollection, TStrings, TStream (base64),
/// TDataSet (array of records), lists registered with RegisterListClass and any
/// class with a registered converter. Extended RTTI (fields, records, generic
/// lists, attributes) is sugar in RALObjectRTTI.
unit RALObjectJSON;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, TypInfo, Variants, DB,
  RALTypes, RALTools, RALBase64, RALJson;

type
  /// How property names become JSON names.
  TRALObjectNaming = (
    /// As declared (FirstName).
    onAsIs,
    /// firstName.
    onCamel,
    /// first_name.
    onSnake,
    /// first-name.
    onKebab,
    /// firstname.
    onLower);

  /// How enumerations are written.
  TRALObjectEnumAs = (eaName, eaNumber);

  /// How TDateTime is written: ISO 8601 with the local offset, ISO 8601 in UTC
  /// ('Z'), or Unix seconds.
  TRALObjectDateFormat = (dfISO8601, dfISO8601UTC, dfUnix);

  /// What reading a JSON null into a property does.
  TRALObjectNullAs = (naSkip, naClear);

  /// Settings of the conversion.
  TRALObjectJSONOptions = class(TPersistent)
  private
    FDateFormat: TRALObjectDateFormat;
    FEnumAs: TRALObjectEnumAs;
    FIgnoreUnknown: boolean;
    FMaxDepth: IntegerRAL;
    FNaming: TRALObjectNaming;
    FNullAs: TRALObjectNullAs;
    FSkipDefault: boolean;
    FSkipNil: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
  published
    /// How TDateTime is written (any ISO 8601 or a number is read).
    property DateFormat: TRALObjectDateFormat read FDateFormat write FDateFormat default dfISO8601;
    /// Enumerations by name or by ordinal (both are read).
    property EnumAs: TRALObjectEnumAs read FEnumAs write FEnumAs default eaName;
    /// A JSON name no property has is ignored; False raises ERALObjectJSON.
    property IgnoreUnknown: boolean read FIgnoreUnknown write FIgnoreUnknown default True;
    /// Nesting beyond this writes null: a cycle cannot loop forever.
    property MaxDepth: IntegerRAL read FMaxDepth write FMaxDepth default 32;
    /// How property names become JSON names (reading accepts the declared name too).
    property Naming: TRALObjectNaming read FNaming write FNaming default onAsIs;
    /// What a JSON null does to a property.
    property NullAs: TRALObjectNullAs read FNullAs write FNullAs default naSkip;
    /// Ordinal properties equal to their declared default are not written.
    property SkipDefault: boolean read FSkipDefault write FSkipDefault default False;
    /// Object properties that are nil are not written (else null).
    property SkipNil: boolean read FSkipNil write FSkipNil default False;
  end;

  /// The JSON could not be read into the object.
  ERALObjectJSON = class(Exception);

  { Base of the classes the binding creates by name (BodyClass, lists): its
    constructor is virtual, so the class's own Create runs and creates what it
    owns. TPersistent.Create is not virtual; a TPersistent that is not a
    TRALObject (or a TComponent) is created without its constructor. }
  TRALObject = class(TPersistent)
  public
    constructor Create; virtual;
  end;

  /// A TRALObject class.
  TRALObjectClass = class of TRALObject;

  { Values that may be absent, for records and the extended RTTI of
    RALObjectRTTI: HasValue False is written as null, and null is read as
    HasValue False. Plain records, the same on every compiler. }
  TRALNullableInteger = record
    HasValue: boolean;
    Value: Integer;
  end;
  TRALNullableInt64 = record
    HasValue: boolean;
    Value: Int64;
  end;
  TRALNullableDouble = record
    HasValue: boolean;
    Value: Double;
  end;
  TRALNullableCurrency = record
    HasValue: boolean;
    Value: Currency;
  end;
  TRALNullableBoolean = record
    HasValue: boolean;
    Value: boolean;
  end;
  TRALNullableDateTime = record
    HasValue: boolean;
    Value: TDateTime;
  end;
  TRALNullableString = record
    HasValue: boolean;
    Value: StringRAL;
  end;

  TRALJSONTextWriter = class;

  { Converts one class to JSON and back; register it with RegisterConverter.
    Write emits one JSON value; Read takes the parsed value. }
  TRALObjectConverter = class
  public
    /// Fills AObj from AValue.
    class procedure Read(AValue: TRALJSONValue; AObj: TObject;
      AOptions: TRALObjectJSONOptions); virtual; abstract;
    /// The JSON schema of the class, for the API documentation ('' = an object).
    class function Schema: StringRAL; virtual;
    /// Writes AObj as one JSON value.
    class procedure Write(AObj: TObject; AWriter: TRALJSONTextWriter;
      AOptions: TRALObjectJSONOptions); virtual; abstract;
  end;

  /// A converter class.
  TRALObjectConverterClass = class of TRALObjectConverter;

  { A JSON writer into a string: values, objects and arrays, with the commas
    and the escapes. }
  TRALJSONTextWriter = class
  private
    FBuffer: StringRAL;
    FFirst: array of boolean;
    FLen: IntegerRAL;
    FPendingName: boolean;

    procedure Append(const AText: StringRAL);
    procedure Separator;
  public
    constructor Create;

    /// Starts an array.
    procedure BeginArray;
    /// Starts an object.
    procedure BeginObject;
    /// Ends the current array.
    procedure EndArray;
    /// Ends the current object.
    procedure EndObject;
    /// The name of the next member.
    procedure Name(const AName: StringRAL);
    /// The text written so far.
    function Text: StringRAL;
    /// A boolean.
    procedure ValueBoolean(AValue: boolean);
    /// A number, with '.' as decimal separator.
    procedure ValueFloat(AValue: Extended);
    /// An integer.
    procedure ValueInteger(AValue: Int64RAL);
    /// null.
    procedure ValueNull;
    /// JSON text already formed, as it is.
    procedure ValueRaw(const AJSON: StringRAL);
    /// A string, escaped.
    procedure ValueString(const AValue: StringRAL);
  end;

  { Objects to JSON and back. AOptions nil = the defaults. }
  TRALObjectJSON = class
  public
    /// Fills AObj from a parsed JSON value.
    class procedure FromJSON(AJSON: TRALJSONValue; AObj: TObject;
      AOptions: TRALObjectJSONOptions = nil);
    /// Fills AObj from JSON text; raises ERALObjectJSON on text that is not JSON.
    class procedure FromText(const AText: StringRAL; AObj: TObject;
      AOptions: TRALObjectJSONOptions = nil);
    /// APropName as AOptions name it in JSON.
    class function JSONName(const APropName: StringRAL; ANaming: TRALObjectNaming): StringRAL;
    /// AObj as a parsed JSON object (the caller frees it).
    class function ToJSON(AObj: TObject; AOptions: TRALObjectJSONOptions = nil): TRALJSONValue;
    /// AObj as JSON text.
    class function ToText(AObj: TObject; AOptions: TRALObjectJSONOptions = nil): StringRAL;
    /// Writes AObj into AWriter (one value).
    class procedure WriteObject(AObj: TObject; AWriter: TRALJSONTextWriter;
      AOptions: TRALObjectJSONOptions; ADepth: IntegerRAL = 0);
  end;

{ A new object of AClass through the right constructor: TRALObject and
  TComponent (owner nil) virtually, TCollectionItem refused (it needs its
  collection), any other class with TObject.Create. }
function RALCreateObject(AClass: TClass): TObject;
type
  { What the attributes of a property say (RALObjectRTTI sets it where the
    compiler has attributes): its JSON name, or that it is left out. False when
    nothing is said about it. }
  TRALPropertyNameHook = function(AClass: TClass; const APropName: StringRAL;
    out AJSONName: StringRAL; out AIgnore: boolean): boolean;

  { Creates an object of a class that is neither a TRALObject nor a TComponent
    through its own parameterless constructor (RALObjectRTTI sets it where the
    compiler has extended RTTI); nil = it cannot. }
  TRALObjectFactoryHook = function(AClass: TClass): TObject;

  { Writes AObj when its class is one the portable serializer does not know
    (the generic lists of RALObjectRTTI); False = not handled. }
  TRALObjectWriteHook = function(AObj: TObject; AWriter: TRALJSONTextWriter;
    AOptions: TRALObjectJSONOptions; ADepth: IntegerRAL): boolean;
  { Reads AValue into AObj when its class is one the portable serializer does
    not know; False = not handled. }
  TRALObjectReadHook = function(AValue: TRALJSONValue; AObj: TObject;
    AOptions: TRALObjectJSONOptions; ADepth: IntegerRAL): boolean;

var
  /// Consulted for every property written or read; nil without RALObjectRTTI.
  RALPropertyNameHook: TRALPropertyNameHook = nil;
  /// Asked before writing an object of an unknown kind; nil without RALObjectRTTI.
  RALObjectWriteHook: TRALObjectWriteHook = nil;
  /// Asked before reading into an object of an unknown kind; nil without RALObjectRTTI.
  RALObjectReadHook: TRALObjectReadHook = nil;
  /// The constructor finder of RALCreateObject; nil without RALObjectRTTI.
  RALObjectFactoryHook: TRALObjectFactoryHook = nil;

{ The JSON name of the property APropName of AClass (and its descendants),
  whatever the naming: what the importer of OpenAPI writes for names Pascal
  cannot spell, and the portable twin of [JSONName]. }
procedure RALRegisterPropertyName(AClass: TClass; const APropName, AJSONName: StringRAL);
/// Registers AConverter for AClass and its descendants (the closest class wins).
procedure RALRegisterObjectConverter(AClass: TClass; AConverter: TRALObjectConverterClass);
/// Removes the converter of AClass.
procedure RALUnregisterObjectConverter(AClass: TClass);
/// The converter for AClass (or its closest ancestor), or nil.
function RALObjectConverterOf(AClass: TClass): TRALObjectConverterClass;
{ Declares that AListClass (a TObjectList or TList descendant) holds AItemClass
  objects, so it is written as an array and read by creating AItemClass. }
procedure RALRegisterListClass(AListClass, AItemClass: TClass);
/// The item class of a registered list class (or of its closest ancestor), or nil.
function RALListItemClass(AListClass: TClass): TClass;
{ Registers AClass by name for the binding (TRALRoute.BodyClass) and the
  documentation; TPersistent classes are found by GetClass too. }
procedure RALRegisterObjectClass(AClass: TClass);
/// The class registered (or by GetClass) under AName, or nil.
function RALFindObjectClass(const AName: StringRAL): TClass;
/// AText as a JSON string literal, quotes included.
function RALJSONQuote(const AText: StringRAL): StringRAL;
/// The type info of a published property (the same on Delphi and FPC).
function RALPropType(AProp: PPropInfo): PTypeInfo;

implementation

var
  gNames: TStringList;
  gConverters: TStringList;
  gLists: TStringList;
  gClasses: TStringList;
  gFormat: TFormatSettings;

{ registries keep the class pointer in Objects; the name is only for sorting }

procedure RALRegisterPropertyName(AClass: TClass; const APropName, AJSONName: StringRAL);
begin
  gNames.Values[AClass.ClassName + '.' + string(APropName)] := string(AJSONName);
end;

{ the registered JSON name of APropName in AClass or an ancestor, or '' }
function RegisteredName(AClass: TClass; const APropName: StringRAL): StringRAL;
begin
  Result := '';
  if gNames.Count = 0 then
    Exit;
  while AClass <> nil do
  begin
    Result := StringRAL(gNames.Values[AClass.ClassName + '.' + string(APropName)]);
    if Result <> '' then
      Exit;
    AClass := AClass.ClassParent;
  end;
end;

procedure RALRegisterObjectConverter(AClass: TClass; AConverter: TRALObjectConverterClass);
var
  vIdx: IntegerRAL;
begin
  { the class in Objects, the converter's address in the text }
  vIdx := gConverters.IndexOfObject(TObject(AClass));
  if vIdx >= 0 then
    gConverters.Delete(vIdx);
  gConverters.AddObject(IntToStr(NativeInt(AConverter)), TObject(AClass));
end;

procedure RALUnregisterObjectConverter(AClass: TClass);
var
  vIdx: IntegerRAL;
begin
  vIdx := gConverters.IndexOfObject(TObject(AClass));
  if vIdx >= 0 then
    gConverters.Delete(vIdx);
end;

function RALObjectConverterOf(AClass: TClass): TRALObjectConverterClass;
var
  vIdx: IntegerRAL;
  vBest: TClass;
  vCls: TClass;
begin
  Result := nil;
  vBest := nil;
  for vIdx := 0 to gConverters.Count - 1 do
  begin
    vCls := TClass(gConverters.Objects[vIdx]);
    if AClass.InheritsFrom(vCls) and ((vBest = nil) or vCls.InheritsFrom(vBest)) then
    begin
      vBest := vCls;
      Result := TRALObjectConverterClass(NativeInt(StrToInt64(gConverters.Strings[vIdx])));
    end;
  end;
end;

procedure RALRegisterListClass(AListClass, AItemClass: TClass);
var
  vIdx: IntegerRAL;
begin
  vIdx := gLists.IndexOfObject(TObject(AListClass));
  if vIdx >= 0 then
    gLists.Delete(vIdx);
  gLists.AddObject(IntToStr(NativeInt(AItemClass)), TObject(AListClass));
end;

function RALListItemClass(AListClass: TClass): TClass;
var
  vIdx: IntegerRAL;
  vBest, vCls: TClass;
begin
  Result := nil;
  vBest := nil;
  for vIdx := 0 to gLists.Count - 1 do
  begin
    vCls := TClass(gLists.Objects[vIdx]);
    if AListClass.InheritsFrom(vCls) and ((vBest = nil) or vCls.InheritsFrom(vBest)) then
    begin
      vBest := vCls;
      Result := TClass(NativeInt(StrToInt64(gLists.Strings[vIdx])));
    end;
  end;
end;

procedure RALRegisterObjectClass(AClass: TClass);
begin
  if gClasses.IndexOf(AClass.ClassName) < 0 then
    gClasses.AddObject(AClass.ClassName, TObject(AClass));
end;

function RALFindObjectClass(const AName: StringRAL): TClass;
var
  vIdx: IntegerRAL;
begin
  vIdx := gClasses.IndexOf(string(AName));
  if vIdx >= 0 then
    Result := TClass(gClasses.Objects[vIdx])
  else
    Result := GetClass(string(AName));
end;

function RALPropType(AProp: PPropInfo): PTypeInfo;
begin
  {$IFDEF FPC}
  Result := AProp^.PropType;
  {$ELSE}
  Result := AProp^.PropType^;
  {$ENDIF}
end;

function RALJSONQuote(const AText: StringRAL): StringRAL;
const
  cHex: StringRAL = '0123456789abcdef';
var
  vInt, vLen, vOut: IntegerRAL;
  vCode: Byte;
begin
  vLen := Length(AText);
  SetLength(Result, vLen * 6 + 2);
  vOut := POSINISTR;
  Result[vOut] := '"';
  Inc(vOut);
  for vInt := POSINISTR to POSINISTR + vLen - 1 do
  begin
    vCode := Ord(AText[vInt]);
    case vCode of
      Ord('"'), Ord('\'):
      begin
        Result[vOut] := '\';
        Result[vOut + 1] := AText[vInt];
        Inc(vOut, 2);
      end;
      8, 9, 10, 12, 13:
      begin
        Result[vOut] := '\';
        case vCode of
          8: Result[vOut + 1] := 'b';
          9: Result[vOut + 1] := 't';
          10: Result[vOut + 1] := 'n';
          12: Result[vOut + 1] := 'f';
        else
          Result[vOut + 1] := 'r';
        end;
        Inc(vOut, 2);
      end;
      0..7, 11, 14..31:
      begin
        Result[vOut] := '\';
        Result[vOut + 1] := 'u';
        Result[vOut + 2] := '0';
        Result[vOut + 3] := '0';
        Result[vOut + 4] := cHex[POSINISTR + (vCode shr 4)];
        Result[vOut + 5] := cHex[POSINISTR + (vCode and 15)];
        Inc(vOut, 6);
      end;
    else
      Result[vOut] := AText[vInt];
      Inc(vOut);
    end;
  end;
  Result[vOut] := '"';
  SetLength(Result, vOut - POSINISTR + 1);
end;

{ TRALObject }

constructor TRALObject.Create;
begin
  inherited Create;
end;

function RALCreateObject(AClass: TClass): TObject;
begin
  if AClass.InheritsFrom(TRALObject) then
    Result := TRALObjectClass(AClass).Create
  else if AClass.InheritsFrom(TComponent) then
    Result := TComponentClass(AClass).Create(nil)
  else if AClass.InheritsFrom(TCollectionItem) then
    raise ERALObjectJSON.CreateFmt('%s needs its collection', [AClass.ClassName])
  else
  begin
    Result := nil;
    if Assigned(RALObjectFactoryHook) then
      Result := RALObjectFactoryHook(AClass);
    if Result = nil then
      Result := AClass.Create;
  end;
end;

{ TRALObjectConverter }

class function TRALObjectConverter.Schema: StringRAL;
begin
  Result := '';
end;

{ TRALObjectJSONOptions }

constructor TRALObjectJSONOptions.Create;
begin
  inherited Create;
  FDateFormat := dfISO8601;
  FEnumAs := eaName;
  FIgnoreUnknown := True;
  FMaxDepth := 32;
  FNaming := onAsIs;
  FNullAs := naSkip;
end;

procedure TRALObjectJSONOptions.AssignTo(Dest: TPersistent);
var
  vDest: TRALObjectJSONOptions;
begin
  if not (Dest is TRALObjectJSONOptions) then
  begin
    inherited AssignTo(Dest);
    Exit;
  end;
  vDest := TRALObjectJSONOptions(Dest);
  vDest.DateFormat := FDateFormat;
  vDest.EnumAs := FEnumAs;
  vDest.IgnoreUnknown := FIgnoreUnknown;
  vDest.MaxDepth := FMaxDepth;
  vDest.Naming := FNaming;
  vDest.NullAs := FNullAs;
  vDest.SkipDefault := FSkipDefault;
  vDest.SkipNil := FSkipNil;
end;

{ TRALJSONTextWriter }

constructor TRALJSONTextWriter.Create;
begin
  inherited Create;
  SetLength(FBuffer, 256);
  FLen := 0;
  SetLength(FFirst, 1);
  FFirst[0] := True;
end;

procedure TRALJSONTextWriter.Append(const AText: StringRAL);
var
  vLen: IntegerRAL;
begin
  vLen := Length(AText);
  if vLen = 0 then
    Exit;
  if FLen + vLen > Length(FBuffer) then
  begin
    if Length(FBuffer) * 2 > FLen + vLen then
      SetLength(FBuffer, Length(FBuffer) * 2)
    else
      SetLength(FBuffer, FLen + vLen + 256);
  end;
  Move(Pointer(AText)^, FBuffer[POSINISTR + FLen], vLen);
  Inc(FLen, vLen);
end;

procedure TRALJSONTextWriter.Separator;
begin
  if FPendingName then
  begin
    FPendingName := False;
    Exit;
  end;
  if not FFirst[High(FFirst)] then
    Append(',');
  FFirst[High(FFirst)] := False;
end;

procedure TRALJSONTextWriter.BeginArray;
begin
  Separator;
  Append('[');
  SetLength(FFirst, Length(FFirst) + 1);
  FFirst[High(FFirst)] := True;
end;

procedure TRALJSONTextWriter.BeginObject;
begin
  Separator;
  Append('{');
  SetLength(FFirst, Length(FFirst) + 1);
  FFirst[High(FFirst)] := True;
end;

procedure TRALJSONTextWriter.EndArray;
begin
  SetLength(FFirst, Length(FFirst) - 1);
  Append(']');
end;

procedure TRALJSONTextWriter.EndObject;
begin
  SetLength(FFirst, Length(FFirst) - 1);
  Append('}');
end;

procedure TRALJSONTextWriter.Name(const AName: StringRAL);
begin
  Separator;
  Append(RALJSONQuote(AName));
  Append(':');
  FPendingName := True;
end;

function TRALJSONTextWriter.Text: StringRAL;
begin
  Result := Copy(FBuffer, POSINISTR, FLen);
end;

procedure TRALJSONTextWriter.ValueBoolean(AValue: boolean);
begin
  Separator;
  if AValue then
    Append('true')
  else
    Append('false');
end;

procedure TRALJSONTextWriter.ValueFloat(AValue: Extended);
var
  vText: string;
  vDouble, vBack: Double;
begin
  Separator;
  if (Frac(AValue) = 0) and (Abs(AValue) < 1E15) then
    Append(StringRAL(IntToStr(Trunc(AValue))))
  else
  begin
    { the shortest of 15 digits when it reads back the same, else all 17 }
    vDouble := AValue;
    vText := FloatToStrF(vDouble, ffGeneral, 15, 0, gFormat);
    vBack := StrToFloat(vText, gFormat);
    if vBack <> vDouble then
      vText := FloatToStrF(vDouble, ffGeneral, 17, 0, gFormat);
    Append(StringRAL(vText));
  end;
end;

procedure TRALJSONTextWriter.ValueInteger(AValue: Int64RAL);
begin
  Separator;
  Append(StringRAL(IntToStr(AValue)));
end;

procedure TRALJSONTextWriter.ValueNull;
begin
  Separator;
  Append('null');
end;

procedure TRALJSONTextWriter.ValueRaw(const AJSON: StringRAL);
begin
  Separator;
  Append(AJSON);
end;

procedure TRALJSONTextWriter.ValueString(const AValue: StringRAL);
begin
  Separator;
  Append(RALJSONQuote(AValue));
end;

{ helpers }

function IsDateType(AType: PTypeInfo): boolean;
begin
  Result := (AType = TypeInfo(TDateTime)) or (AType = TypeInfo(TDate)) or
    (AType = TypeInfo(TTime));
end;

function IsBoolType(AType: PTypeInfo): boolean;
begin
  {$IFDEF FPC}
  Result := AType^.Kind = tkBool;
  {$ELSE}
  Result := (AType^.Kind = tkEnumeration) and (GetTypeData(AType)^.BaseType^ = TypeInfo(Boolean));
  {$ENDIF}
end;

function GetText(AObj: TObject; AProp: PPropInfo): StringRAL;
begin
  {$IFDEF FPC}
  Result := UTF8Encode(GetUnicodeStrProp(AObj, AProp));
  {$ELSE}
  Result := UTF8Encode(GetStrProp(AObj, AProp));
  {$ENDIF}
end;

procedure SetText(AObj: TObject; AProp: PPropInfo; const AValue: StringRAL);
begin
  {$IFDEF FPC}
  SetUnicodeStrProp(AObj, AProp, UTF8Decode(AValue));
  {$ELSE}
  SetStrProp(AObj, AProp, UTF8ToString(AValue));
  {$ENDIF}
end;

{ UTF-8 to the RTL's string }
function FromUTF8(const AValue: StringRAL): string;
begin
  {$IFDEF FPC}
  Result := string(UTF8Decode(AValue));
  {$ELSE}
  Result := UTF8ToString(AValue);
  {$ENDIF}
end;

function PropName(AProp: PPropInfo): StringRAL;
begin
  Result := StringRAL(AProp^.Name);
end;

{ the properties of AObj in declaration order }
function PropsOf(AObj: TObject; out AList: PPropList): IntegerRAL;
begin
  AList := nil;
  Result := GetPropList(PTypeInfo(AObj.ClassInfo), tkProperties, nil, False);
  if Result <= 0 then
    Exit;
  GetMem(AList, Result * SizeOf(PPropInfo));
  Result := GetPropList(PTypeInfo(AObj.ClassInfo), tkProperties, AList, False);
end;

function DateText(AValue: TDateTime; AOptions: TRALObjectJSONOptions): StringRAL;
begin
  case AOptions.DateFormat of
    dfISO8601UTC: Result := RALDateTimeToISO8601(RALDateTimeToGMT(AValue), True);
  else
    Result := RALDateTimeToISO8601(AValue, False);
  end;
end;

function JSONDate(AValue: TRALJSONValue; AOptions: TRALObjectJSONOptions): TDateTime;
var
  vText: StringRAL;
begin
  if AValue.JsonType = rjtNumber then
  begin
    Result := RALGMTToDateTime(UnixDateDelta + AValue.AsFloat / SecsPerDay);
    Exit;
  end;
  vText := AValue.AsString;
  if vText = '' then
  begin
    Result := 0;
    Exit;
  end;
  if not RALTryISO8601ToDateTime(vText, Result) then
    raise ERALObjectJSON.CreateFmt('Not a date: %s', [string(vText)]);
  { RAL's ISO reader keeps a 'Z' as written: a UTC value goes back to local }
  if vText[RALHighStr(vText)] in ['Z', 'z'] then
    Result := RALGMTToDateTime(Result);
end;

{ TRALObjectJSON }

class function TRALObjectJSON.JSONName(const APropName: StringRAL;
  ANaming: TRALObjectNaming): StringRAL;
var
  vInt: IntegerRAL;
  vChar: AnsiChar;
  vPrevLower, vSep: boolean;
  vSepChar: AnsiChar;
begin
  case ANaming of
    onAsIs: Result := APropName;
    onLower: Result := StringRAL(LowerCase(string(APropName)));
    onCamel:
    begin
      Result := APropName;
      { the leading capitals lowered: ID -> id, URLPath -> urlPath }
      vInt := POSINISTR;
      while (vInt <= RALHighStr(Result)) and (Result[vInt] in ['A'..'Z']) do
      begin
        if (vInt > POSINISTR) and (vInt < RALHighStr(Result)) and
           (Result[vInt + 1] in ['a'..'z']) then
          Break;
        Result[vInt] := AnsiChar(Ord(Result[vInt]) + 32);
        Inc(vInt);
      end;
    end;
  else
    begin
      if ANaming = onSnake then
        vSepChar := '_'
      else
        vSepChar := '-';
      Result := '';
      vPrevLower := False;
      for vInt := POSINISTR to RALHighStr(APropName) do
      begin
        vChar := APropName[vInt];
        vSep := (vChar in ['A'..'Z']) and (vPrevLower or ((vInt < RALHighStr(APropName)) and
          (vInt > POSINISTR) and (APropName[vInt + 1] in ['a'..'z'])));
        if vSep and (Result <> '') and (Result[RALHighStr(Result)] <> vSepChar) then
          Result := Result + vSepChar;
        if vChar in ['A'..'Z'] then
          vChar := AnsiChar(Ord(vChar) + 32);
        Result := Result + vChar;
        vPrevLower := (APropName[vInt] in ['a'..'z', '0'..'9']);
      end;
    end;
  end;
end;

procedure WriteValueOf(AObj: TObject; AWriter: TRALJSONTextWriter;
  AOptions: TRALObjectJSONOptions; ADepth: IntegerRAL); forward;

{ the JSON name of AProp of AClass: the attribute's, else by the naming; False
  when the property is left out }
function NameOf(AClass: TClass; AProp: PPropInfo; AOptions: TRALObjectJSONOptions;
  out AName: StringRAL): boolean;
var
  vIgnore: boolean;
begin
  Result := True;
  AName := RegisteredName(AClass, PropName(AProp));
  if AName <> '' then
    Exit;
  if Assigned(RALPropertyNameHook) and
     RALPropertyNameHook(AClass, PropName(AProp), AName, vIgnore) then
  begin
    Result := not vIgnore;
    if AName <> '' then
      Exit;
  end;
  AName := TRALObjectJSON.JSONName(PropName(AProp), AOptions.Naming);
end;

procedure WriteDataSet(ADataSet: TDataSet; AWriter: TRALJSONTextWriter;
  AOptions: TRALObjectJSONOptions);
var
  vBook: TBookmark;
  vInt: IntegerRAL;
  vField: TField;
begin
  AWriter.BeginArray;
  if ADataSet.Active then
  begin
    ADataSet.DisableControls;
    vBook := ADataSet.GetBookmark;
    try
      ADataSet.First;
      while not ADataSet.Eof do
      begin
        AWriter.BeginObject;
        for vInt := 0 to ADataSet.FieldCount - 1 do
        begin
          vField := ADataSet.Fields[vInt];
          AWriter.Name(TRALObjectJSON.JSONName(StringRAL(vField.FieldName), AOptions.Naming));
          if vField.IsNull then
            AWriter.ValueNull
          else
            case vField.DataType of
              ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc:
                AWriter.ValueInteger(vField.AsLargeInt);
              ftFloat, ftCurrency, ftBCD, ftFMTBcd:
                AWriter.ValueFloat(vField.AsFloat);
              ftBoolean:
                AWriter.ValueBoolean(vField.AsBoolean);
              ftDate, ftTime, ftDateTime, ftTimeStamp:
                AWriter.ValueString(DateText(vField.AsDateTime, AOptions));
              ftBlob, ftGraphic, ftVarBytes, ftBytes:
                AWriter.ValueString(TRALBase64.Encode(vField.AsBytes));
            else
              AWriter.ValueString(UTF8Encode(vField.AsString));
            end;
        end;
        AWriter.EndObject;
        ADataSet.Next;
      end;
      ADataSet.GotoBookmark(vBook);
    finally
      ADataSet.FreeBookmark(vBook);
      ADataSet.EnableControls;
    end;
  end;
  AWriter.EndArray;
end;

procedure WriteProps(AObj: TObject; AWriter: TRALJSONTextWriter;
  AOptions: TRALObjectJSONOptions; ADepth: IntegerRAL);
var
  vList: PPropList;
  vCount, vInt: IntegerRAL;
  vProp: PPropInfo;
  vType: PTypeInfo;
  vName: StringRAL;
  vOrd: Int64RAL;
  vChild: TObject;
  vSet: string;
  vSetList: TStringList;
  vIdx: IntegerRAL;
begin
  AWriter.BeginObject;
  vCount := PropsOf(AObj, vList);
  try
    for vInt := 0 to vCount - 1 do
    begin
      vProp := vList^[vInt];
      if vProp^.GetProc = nil then
        Continue;
      vType := RALPropType(vProp);
      if vType^.Kind in [tkMethod, tkInterface, tkArray, tkRecord, tkDynArray] then
        Continue;
      if not NameOf(AObj.ClassType, vProp, AOptions, vName) then
        Continue;
      case vType^.Kind of
        tkInteger, tkChar, tkWChar, tkEnumeration {$IFDEF FPC}, tkBool{$ENDIF}:
        begin
          vOrd := GetOrdProp(AObj, vProp);
          if AOptions.SkipDefault and (vProp^.Default <> Integer($80000000)) and
             (vOrd = vProp^.Default) then
            Continue;
          AWriter.Name(vName);
          if IsBoolType(vType) then
            AWriter.ValueBoolean(vOrd <> 0)
          else if vType^.Kind = tkEnumeration then
          begin
            if AOptions.EnumAs = eaNumber then
              AWriter.ValueInteger(vOrd)
            else
              AWriter.ValueString(StringRAL(GetEnumName(vType, vOrd)));
          end
          else if vType^.Kind in [tkChar, tkWChar] then
            AWriter.ValueString(UTF8Encode(WideString(WideChar(vOrd))))
          else
            AWriter.ValueInteger(vOrd);
        end;
        tkInt64 {$IFDEF FPC}, tkQWord{$ENDIF}:
        begin
          AWriter.Name(vName);
          AWriter.ValueInteger(GetInt64Prop(AObj, vProp));
        end;
        tkFloat:
        begin
          AWriter.Name(vName);
          if IsDateType(vType) then
          begin
            if GetFloatProp(AObj, vProp) = 0 then
              AWriter.ValueNull
            else if AOptions.DateFormat = dfUnix then
              AWriter.ValueInteger(Round((RALDateTimeToGMT(GetFloatProp(AObj, vProp)) -
                UnixDateDelta) * SecsPerDay))
            else
              AWriter.ValueString(DateText(GetFloatProp(AObj, vProp), AOptions));
          end
          else
            AWriter.ValueFloat(GetFloatProp(AObj, vProp));
        end;
        tkSet:
        begin
          AWriter.Name(vName);
          AWriter.BeginArray;
          vSet := GetSetProp(AObj, vProp, False);
          vSetList := TStringList.Create;
          try
            vSetList.CommaText := vSet;
            for vIdx := 0 to vSetList.Count - 1 do
              AWriter.ValueString(StringRAL(Trim(vSetList[vIdx])));
          finally
            vSetList.Free;
          end;
          AWriter.EndArray;
        end;
        tkClass:
        begin
          vChild := GetObjectProp(AObj, vProp);
          if (vChild = nil) and AOptions.SkipNil then
            Continue;
          AWriter.Name(vName);
          if vChild = nil then
            AWriter.ValueNull
          else
            WriteValueOf(vChild, AWriter, AOptions, ADepth + 1);
        end;
        tkVariant:
        begin
          AWriter.Name(vName);
          AWriter.ValueString(StringRAL(VarToStr(GetVariantProp(AObj, vProp))));
        end;
      else
        { the string kinds }
        AWriter.Name(vName);
        AWriter.ValueString(GetText(AObj, vProp));
      end;
    end;
  finally
    if vList <> nil then
      FreeMem(vList);
  end;
  AWriter.EndObject;
end;

procedure WriteValueOf(AObj: TObject; AWriter: TRALJSONTextWriter;
  AOptions: TRALObjectJSONOptions; ADepth: IntegerRAL);
var
  vConv: TRALObjectConverterClass;
  vInt: IntegerRAL;
  vStream: TStream;
  vBytes: TBytes;
  vPos: Int64RAL;
begin
  if ADepth > AOptions.MaxDepth then
  begin
    AWriter.ValueNull;
    Exit;
  end;
  vConv := RALObjectConverterOf(AObj.ClassType);
  if vConv <> nil then
    vConv.Write(AObj, AWriter, AOptions)
  else if Assigned(RALObjectWriteHook) and RALObjectWriteHook(AObj, AWriter, AOptions, ADepth) then
    Exit
  else if AObj is TCollection then
  begin
    AWriter.BeginArray;
    for vInt := 0 to TCollection(AObj).Count - 1 do
      WriteValueOf(TCollection(AObj).Items[vInt], AWriter, AOptions, ADepth + 1);
    AWriter.EndArray;
  end
  else if AObj is TStrings then
  begin
    AWriter.BeginArray;
    for vInt := 0 to TStrings(AObj).Count - 1 do
      AWriter.ValueString(UTF8Encode(TStrings(AObj)[vInt]));
    AWriter.EndArray;
  end
  else if AObj is TStream then
  begin
    vStream := TStream(AObj);
    vPos := vStream.Position;
    SetLength(vBytes, vStream.Size);
    vStream.Position := 0;
    if Length(vBytes) > 0 then
      vStream.ReadBuffer(vBytes[0], Length(vBytes));
    vStream.Position := vPos;
    AWriter.ValueString(TRALBase64.Encode(vBytes));
  end
  else if AObj is TDataSet then
    WriteDataSet(TDataSet(AObj), AWriter, AOptions)
  else if (AObj is TList) and (RALListItemClass(AObj.ClassType) <> nil) then
  begin
    AWriter.BeginArray;
    for vInt := 0 to TList(AObj).Count - 1 do
      if TList(AObj).Items[vInt] = nil then
        AWriter.ValueNull
      else
        WriteValueOf(TObject(TList(AObj).Items[vInt]), AWriter, AOptions, ADepth + 1);
    AWriter.EndArray;
  end
  else
    WriteProps(AObj, AWriter, AOptions, ADepth);
end;

class procedure TRALObjectJSON.WriteObject(AObj: TObject; AWriter: TRALJSONTextWriter;
  AOptions: TRALObjectJSONOptions; ADepth: IntegerRAL);
var
  vOwned: boolean;
begin
  vOwned := AOptions = nil;
  if vOwned then
    AOptions := TRALObjectJSONOptions.Create;
  try
    if AObj = nil then
      AWriter.ValueNull
    else
      WriteValueOf(AObj, AWriter, AOptions, ADepth);
  finally
    if vOwned then
      AOptions.Free;
  end;
end;

class function TRALObjectJSON.ToText(AObj: TObject; AOptions: TRALObjectJSONOptions): StringRAL;
var
  vWriter: TRALJSONTextWriter;
begin
  vWriter := TRALJSONTextWriter.Create;
  try
    WriteObject(AObj, vWriter, AOptions);
    Result := vWriter.Text;
  finally
    vWriter.Free;
  end;
end;

class function TRALObjectJSON.ToJSON(AObj: TObject; AOptions: TRALObjectJSONOptions): TRALJSONValue;
begin
  Result := TRALJSON.ParseJSON(ToText(AObj, AOptions));
end;

{ reading }

procedure ReadInto(AValue: TRALJSONValue; AObj: TObject; AOptions: TRALObjectJSONOptions;
  ADepth: IntegerRAL); forward;

function NewItem(AClass: TClass): TObject;
begin
  Result := RALCreateObject(AClass);
end;

procedure ReadDataSet(AValue: TRALJSONValue; ADataSet: TDataSet;
  AOptions: TRALObjectJSONOptions);
var
  vInt, vMember, vIdx: IntegerRAL;
  vRow: TRALJSONObject;
  vField: TField;
  vItem: TRALJSONValue;
begin
  if not (AValue is TRALJSONArray) then
    raise ERALObjectJSON.Create('A dataset is read from an array');
  if not ADataSet.Active then
    ADataSet.Open;
  for vInt := 0 to TRALJSONArray(AValue).Count - 1 do
  begin
    if not (TRALJSONArray(AValue).Get(vInt) is TRALJSONObject) then
      Continue;
    vRow := TRALJSONObject(TRALJSONArray(AValue).Get(vInt));
    ADataSet.Append;
    for vMember := 0 to vRow.Count - 1 do
    begin
      vField := nil;
      for vIdx := 0 to ADataSet.FieldCount - 1 do
        if (TRALObjectJSON.JSONName(StringRAL(ADataSet.Fields[vIdx].FieldName),
            AOptions.Naming) = vRow.GetName(vMember)) or
           SameText(ADataSet.Fields[vIdx].FieldName, string(vRow.GetName(vMember))) then
        begin
          vField := ADataSet.Fields[vIdx];
          Break;
        end;
      if vField = nil then
        Continue;
      vItem := vRow.Get(vMember);
      if vItem.IsNull then
        vField.Clear
      else
        case vField.DataType of
          ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc:
            vField.AsLargeInt := vItem.AsInteger;
          ftFloat, ftCurrency, ftBCD, ftFMTBcd:
            vField.AsFloat := vItem.AsFloat;
          ftBoolean:
            vField.AsBoolean := vItem.AsBoolean;
          ftDate, ftTime, ftDateTime, ftTimeStamp:
            vField.AsDateTime := JSONDate(vItem, AOptions);
          ftBlob, ftGraphic, ftVarBytes, ftBytes:
            vField.AsBytes := TRALBase64.DecodeAsBytes(vItem.AsString);
        else
          vField.AsString := FromUTF8(vItem.AsString);
        end;
    end;
    ADataSet.Post;
  end;
end;

procedure ReadList(AValue: TRALJSONValue; AObj: TObject; AOptions: TRALObjectJSONOptions;
  ADepth: IntegerRAL);
var
  vInt: IntegerRAL;
  vArr: TRALJSONArray;
  vItem: TObject;
  vClass: TClass;
  vBytes: TBytes;
begin
  if AObj is TStream then
  begin
    vBytes := TRALBase64.DecodeAsBytes(AValue.AsString);
    TStream(AObj).Size := 0;
    if Length(vBytes) > 0 then
      TStream(AObj).WriteBuffer(vBytes[0], Length(vBytes));
    TStream(AObj).Position := 0;
    Exit;
  end;
  if AObj is TDataSet then
  begin
    ReadDataSet(AValue, TDataSet(AObj), AOptions);
    Exit;
  end;
  if not (AValue is TRALJSONArray) then
    raise ERALObjectJSON.CreateFmt('%s is read from an array', [AObj.ClassName]);
  vArr := TRALJSONArray(AValue);
  if AObj is TCollection then
  begin
    TCollection(AObj).BeginUpdate;
    try
      TCollection(AObj).Clear;
      for vInt := 0 to vArr.Count - 1 do
        ReadInto(vArr.Get(vInt), TCollection(AObj).Add, AOptions, ADepth + 1);
    finally
      TCollection(AObj).EndUpdate;
    end;
  end
  else if AObj is TStrings then
  begin
    TStrings(AObj).BeginUpdate;
    try
      TStrings(AObj).Clear;
      for vInt := 0 to vArr.Count - 1 do
        TStrings(AObj).Add(FromUTF8(vArr.Get(vInt).AsString));
    finally
      TStrings(AObj).EndUpdate;
    end;
  end
  else if AObj is TList then
  begin
    vClass := RALListItemClass(AObj.ClassType);
    if vClass = nil then
      raise ERALObjectJSON.CreateFmt('%s: list class not registered', [AObj.ClassName]);
    TList(AObj).Clear;
    for vInt := 0 to vArr.Count - 1 do
    begin
      if vArr.Get(vInt).IsNull then
      begin
        TList(AObj).Add(nil);
        Continue;
      end;
      vItem := NewItem(vClass);
      try
        ReadInto(vArr.Get(vInt), vItem, AOptions, ADepth + 1);
      except
        vItem.Free;
        raise;
      end;
      TList(AObj).Add(vItem);
    end;
  end;
end;

{ the property of AObj that AName names, by the naming or as declared }
function FindProp(AObj: TObject; AList: PPropList; ACount: IntegerRAL;
  const AName: StringRAL; AOptions: TRALObjectJSONOptions): PPropInfo;
var
  vInt: IntegerRAL;
  vName: StringRAL;
begin
  for vInt := 0 to ACount - 1 do
  begin
    Result := AList^[vInt];
    if NameOf(AObj.ClassType, Result, AOptions, vName) and ((vName = AName) or
       SameText(string(PropName(Result)), string(AName))) then
      Exit;
  end;
  Result := nil;
end;

procedure ReadProps(AValue: TRALJSONObject; AObj: TObject; AOptions: TRALObjectJSONOptions;
  ADepth: IntegerRAL);
var
  vList: PPropList;
  vCount, vMember, vIdx: IntegerRAL;
  vProp: PPropInfo;
  vType: PTypeInfo;
  vItem: TRALJSONValue;
  vChild: TObject;
  vSet: string;
  vOrd: IntegerRAL;
  vText: StringRAL;
  vClass: TClass;
begin
  vCount := PropsOf(AObj, vList);
  try
    for vMember := 0 to AValue.Count - 1 do
    begin
      vProp := FindProp(AObj, vList, vCount, AValue.GetName(vMember), AOptions);
      if vProp = nil then
      begin
        if not AOptions.IgnoreUnknown then
          raise ERALObjectJSON.CreateFmt('%s has no property for "%s"',
            [AObj.ClassName, string(AValue.GetName(vMember))]);
        Continue;
      end;
      vItem := AValue.Get(vMember);
      vType := RALPropType(vProp);

      { an object property is filled in place, writable or not }
      if vType^.Kind = tkClass then
      begin
        vChild := GetObjectProp(AObj, vProp);
        if vItem.IsNull then
        begin
          if (AOptions.NullAs = naClear) and (vProp^.SetProc <> nil) and (vChild <> nil) then
          begin
            SetObjectProp(AObj, vProp, nil);
            vChild.Free;
          end;
          Continue;
        end;
        if vChild = nil then
        begin
          if vProp^.SetProc = nil then
            Continue;
          vClass := GetTypeData(vType)^.ClassType;
          vChild := NewItem(vClass);
          SetObjectProp(AObj, vProp, vChild);
        end;
        ReadInto(vItem, vChild, AOptions, ADepth + 1);
        Continue;
      end;

      if vProp^.SetProc = nil then
        Continue;
      if vItem.IsNull then
      begin
        if AOptions.NullAs = naClear then
          case vType^.Kind of
            tkInteger, tkChar, tkWChar, tkEnumeration, tkSet {$IFDEF FPC}, tkBool{$ENDIF}:
              SetOrdProp(AObj, vProp, 0);
            tkInt64 {$IFDEF FPC}, tkQWord{$ENDIF}: SetInt64Prop(AObj, vProp, 0);
            tkFloat: SetFloatProp(AObj, vProp, 0);
            tkVariant: SetVariantProp(AObj, vProp, Null);
          else
            SetText(AObj, vProp, '');
          end;
        Continue;
      end;

      try
        case vType^.Kind of
          tkInteger:
            if vItem.JsonType = rjtString then
              SetOrdProp(AObj, vProp, StrToInt(string(vItem.AsString)))
            else
              SetOrdProp(AObj, vProp, vItem.AsInteger);
          tkChar, tkWChar:
          begin
            vText := vItem.AsString;
            if vText = '' then
              SetOrdProp(AObj, vProp, 0)
            else
              SetOrdProp(AObj, vProp, Ord(UTF8Decode(vText)[1]));
          end;
          tkEnumeration {$IFDEF FPC}, tkBool{$ENDIF}:
          begin
            if IsBoolType(vType) then
            begin
              if vItem.JsonType = rjtBoolean then
                SetOrdProp(AObj, vProp, Ord(vItem.AsBoolean))
              else
                SetOrdProp(AObj, vProp, Ord(SameText(string(vItem.AsString), 'true') or
                  (vItem.AsString = '1')));
            end
            else if vItem.JsonType = rjtNumber then
              SetOrdProp(AObj, vProp, vItem.AsInteger)
            else
            begin
              vOrd := GetEnumValue(vType, string(vItem.AsString));
              if vOrd < 0 then
                raise ERALObjectJSON.CreateFmt('"%s" is not a value of %s',
                  [string(vItem.AsString), string(vType^.Name)]);
              SetOrdProp(AObj, vProp, vOrd);
            end;
          end;
          tkInt64 {$IFDEF FPC}, tkQWord{$ENDIF}:
            if vItem.JsonType = rjtString then
              SetInt64Prop(AObj, vProp, StrToInt64(string(vItem.AsString)))
            else
              SetInt64Prop(AObj, vProp, vItem.AsInteger);
          tkFloat:
          begin
            if IsDateType(vType) then
              SetFloatProp(AObj, vProp, JSONDate(vItem, AOptions))
            else if vItem.JsonType = rjtString then
              SetFloatProp(AObj, vProp, StrToFloat(string(vItem.AsString), gFormat))
            else
              SetFloatProp(AObj, vProp, vItem.AsFloat);
          end;
          tkSet:
          begin
            vSet := '';
            if vItem is TRALJSONArray then
            begin
              for vIdx := 0 to TRALJSONArray(vItem).Count - 1 do
              begin
                if vSet <> '' then
                  vSet := vSet + ',';
                vSet := vSet + string(TRALJSONArray(vItem).Get(vIdx).AsString);
              end;
            end
            else
              vSet := string(vItem.AsString);
            SetSetProp(AObj, vProp, vSet);
          end;
          tkVariant:
            SetVariantProp(AObj, vProp, vItem.AsValue);
        else
          if vItem.JsonType in [rjtObject, rjtArray] then
            SetText(AObj, vProp, vItem.ToJson)
          else
            SetText(AObj, vProp, vItem.AsString);
        end;
      except
        on E: ERALObjectJSON do
          raise;
        on E: Exception do
          raise ERALObjectJSON.CreateFmt('%s.%s: %s', [AObj.ClassName,
            string(PropName(vProp)), E.Message]);
      end;
    end;
  finally
    if vList <> nil then
      FreeMem(vList);
  end;
end;

procedure ReadInto(AValue: TRALJSONValue; AObj: TObject; AOptions: TRALObjectJSONOptions;
  ADepth: IntegerRAL);
var
  vConv: TRALObjectConverterClass;
begin
  if ADepth > AOptions.MaxDepth then
    raise ERALObjectJSON.Create('JSON nested deeper than MaxDepth');
  vConv := RALObjectConverterOf(AObj.ClassType);
  if vConv <> nil then
    vConv.Read(AValue, AObj, AOptions)
  else if Assigned(RALObjectReadHook) and RALObjectReadHook(AValue, AObj, AOptions, ADepth) then
    Exit
  else if (AObj is TCollection) or (AObj is TStrings) or (AObj is TStream) or
          (AObj is TDataSet) or ((AObj is TList) and (RALListItemClass(AObj.ClassType) <> nil)) then
    ReadList(AValue, AObj, AOptions, ADepth)
  else if AValue is TRALJSONObject then
    ReadProps(TRALJSONObject(AValue), AObj, AOptions, ADepth)
  else
    raise ERALObjectJSON.CreateFmt('%s is read from an object', [AObj.ClassName]);
end;

class procedure TRALObjectJSON.FromJSON(AJSON: TRALJSONValue; AObj: TObject;
  AOptions: TRALObjectJSONOptions);
var
  vOwned: boolean;
begin
  vOwned := AOptions = nil;
  if vOwned then
    AOptions := TRALObjectJSONOptions.Create;
  try
    ReadInto(AJSON, AObj, AOptions, 0);
  finally
    if vOwned then
      AOptions.Free;
  end;
end;

class procedure TRALObjectJSON.FromText(const AText: StringRAL; AObj: TObject;
  AOptions: TRALObjectJSONOptions);
var
  vJSON: TRALJSONValue;
begin
  try
    vJSON := TRALJSON.ParseJSON(AText);
  except
    on E: Exception do
      raise ERALObjectJSON.Create('Not JSON: ' + E.Message);
  end;
  if vJSON = nil then
    raise ERALObjectJSON.Create('Not JSON');
  try
    FromJSON(vJSON, AObj, AOptions);
  finally
    vJSON.Free;
  end;
end;

initialization
  gNames := TStringList.Create;
  gConverters := TStringList.Create;
  gLists := TStringList.Create;
  gClasses := TStringList.Create;
  gClasses.Sorted := True;
  gFormat := {$IFDEF FPC}DefaultFormatSettings{$ELSE}FormatSettings{$ENDIF};
  gFormat.DecimalSeparator := '.';
  gFormat.ThousandSeparator := #0;

finalization
  FreeAndNil(gNames);
  FreeAndNil(gConverters);
  FreeAndNil(gLists);
  FreeAndNil(gClasses);

end.
