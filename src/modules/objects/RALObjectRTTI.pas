/// Sugar of the object serializer, where the compiler has extended RTTI and
/// attributes (Delphi 2010 and later): [JSONName('x')] and [JSONIgnore] on
/// properties, validation attributes ([Required], [MinLength], [MaxLength],
/// [Range], [Pattern], [Email]), and records, dynamic arrays and the generic
/// lists of Generics.Collections through TRALObjectRTTI. Compiles empty
/// elsewhere.
unit RALObjectRTTI;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{$IFDEF RALHasExtRTTI}
uses
  Classes, SysUtils, TypInfo, Rtti, SyncObjs,
  RALTypes, RALConsts, RALTools, RALJson, RALRegex, RALObjectJSON, RALObjectBinding;

type
  /// The JSON name of a property.
  JSONNameAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    /// The name.
    property Name: string read FName;
  end;

  /// The property is neither written nor read.
  JSONIgnoreAttribute = class(TCustomAttribute);

  /// The property must have a value (not empty, not nil).
  RequiredAttribute = class(TCustomAttribute);

  /// A text of at least Value characters.
  MinLengthAttribute = class(TCustomAttribute)
  private
    FValue: Integer;
  public
    constructor Create(AValue: Integer);
    /// The minimum.
    property Value: Integer read FValue;
  end;

  /// A text of at most Value characters.
  MaxLengthAttribute = class(TCustomAttribute)
  private
    FValue: Integer;
  public
    constructor Create(AValue: Integer);
    /// The maximum.
    property Value: Integer read FValue;
  end;

  /// A number from Min to Max.
  RangeAttribute = class(TCustomAttribute)
  private
    FMax: Double;
    FMin: Double;
  public
    constructor Create(AMin, AMax: Double);
    /// The maximum.
    property Max: Double read FMax;
    /// The minimum.
    property Min: Double read FMin;
  end;

  /// A text the regular expression (RALRegex, whole text) matches.
  PatternAttribute = class(TCustomAttribute)
  private
    FPattern: string;
  public
    constructor Create(const APattern: string);
    /// The pattern.
    property Pattern: string read FPattern;
  end;

  /// An e-mail address.
  EmailAttribute = class(TCustomAttribute);

  { Records, dynamic arrays, generic lists and objects to JSON and back through
    the extended RTTI. Objects inside go through TRALObjectJSON (published
    properties); a record's public fields are its members. }
  TRALObjectRTTI = class
  public
    /// AValue as JSON text.
    class function ToText<T>(const AValue: T; AOptions: TRALObjectJSONOptions = nil): StringRAL;
    /// A T read from AText (a class is created; the caller frees it).
    class function FromText<T>(const AText: StringRAL; AOptions: TRALObjectJSONOptions = nil): T;
    /// AValue as JSON text.
    class function ValueToText(const AValue: TValue; AOptions: TRALObjectJSONOptions = nil): StringRAL;
    /// AJSON read into a value of the type ATypeInfo.
    class function ValueFromJSON(AJSON: TRALJSONValue; ATypeInfo: PTypeInfo;
      AOptions: TRALObjectJSONOptions = nil): TValue;
  end;
{$ENDIF}

implementation

{$IFDEF RALHasExtRTTI}
var
  gContext: TRttiContext;
  gCache: TStringList;
  gLock: TCriticalSection;

{ attributes }

constructor JSONNameAttribute.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

constructor MinLengthAttribute.Create(AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

constructor MaxLengthAttribute.Create(AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

constructor RangeAttribute.Create(AMin, AMax: Double);
begin
  inherited Create;
  FMin := AMin;
  FMax := AMax;
end;

constructor PatternAttribute.Create(const APattern: string);
begin
  inherited Create;
  FPattern := APattern;
end;

{ the name hook: what [JSONName] and [JSONIgnore] say, cached per class and
  property ('' nothing, '-' ignored, else the name) }
function NameHook(AClass: TClass; const APropName: StringRAL; out AJSONName: StringRAL;
  out AIgnore: boolean): boolean;
var
  vKey, vValue: string;
  vIdx: Integer;
  vType: TRttiType;
  vProp: TRttiProperty;
  vAttr: TCustomAttribute;
begin
  AJSONName := '';
  AIgnore := False;
  vKey := AClass.ClassName + '.' + string(APropName);
  gLock.Acquire;
  try
    vIdx := gCache.IndexOfName(vKey);
    if vIdx >= 0 then
      vValue := gCache.ValueFromIndex[vIdx]
    else
    begin
      vValue := '';
      vType := gContext.GetType(AClass);
      vProp := nil;
      if vType <> nil then
        vProp := vType.GetProperty(string(APropName));
      if vProp <> nil then
        for vAttr in vProp.GetAttributes do
          if vAttr is JSONIgnoreAttribute then
            vValue := '-'
          else if (vAttr is JSONNameAttribute) and (vValue <> '-') then
            vValue := '=' + JSONNameAttribute(vAttr).Name;
      gCache.Add(vKey + '=' + vValue);
    end;
  finally
    gLock.Release;
  end;
  Result := vValue <> '';
  if vValue = '-' then
    AIgnore := True
  else if Result then
    AJSONName := StringRAL(Copy(vValue, 2, MaxInt));
end;

{ the validation hook: the attributes of every property of AObj }
procedure AttributeValidation(AObj: TObject; AErrors: TStrings);
var
  vType: TRttiType;
  vProp: TRttiProperty;
  vAttr: TCustomAttribute;
  vValue: TValue;
  vText, vName: string;
  vJSONName: StringRAL;
  vIgnore, vEmpty: boolean;
  vNumber: Double;

  procedure Fail(const AMessage: string);
  begin
    AErrors.Add(vName + '=' + AMessage);
  end;

begin
  vType := gContext.GetType(AObj.ClassType);
  if vType = nil then
    Exit;
  for vProp in vType.GetProperties do
  begin
    if not vProp.IsReadable then
      Continue;
    vName := vProp.Name;
    if NameHook(AObj.ClassType, StringRAL(vProp.Name), vJSONName, vIgnore) and (vJSONName <> '') then
      vName := string(vJSONName)
    else
      vName := string(TRALObjectJSON.JSONName(StringRAL(vProp.Name), RALObjectSettings.JSON.Naming));
    vValue := TValue.Empty;
    for vAttr in vProp.GetAttributes do
    begin
      if vValue.IsEmpty then
        vValue := vProp.GetValue(AObj);
      vEmpty := vValue.IsEmpty or (vValue.IsObject and (vValue.AsObject = nil));
      vText := '';
      vNumber := 0;
      if not vEmpty then
        case vValue.Kind of
          tkInteger, tkInt64, tkFloat:
            vNumber := vValue.AsExtended;
          tkString, tkLString, tkWString, tkUString, tkChar, tkWChar:
            vText := vValue.ToString;
        end;
      if vValue.Kind in [tkString, tkLString, tkWString, tkUString] then
        vEmpty := Trim(vText) = '';
      if vAttr is RequiredAttribute then
      begin
        if vEmpty then
          Fail(string(wmValRequired));
      end
      else if vEmpty then
        Continue
      else if vAttr is MinLengthAttribute then
      begin
        if Length(vText) < MinLengthAttribute(vAttr).Value then
          Fail(Format(string(wmValMinLength), [IntToStr(MinLengthAttribute(vAttr).Value)]));
      end
      else if vAttr is MaxLengthAttribute then
      begin
        if Length(vText) > MaxLengthAttribute(vAttr).Value then
          Fail(Format(string(wmValMaxLength), [IntToStr(MaxLengthAttribute(vAttr).Value)]));
      end
      else if vAttr is RangeAttribute then
      begin
        if vNumber < RangeAttribute(vAttr).Min then
          Fail(Format(string(wmValMin), [FloatToStr(RangeAttribute(vAttr).Min)]))
        else if vNumber > RangeAttribute(vAttr).Max then
          Fail(Format(string(wmValMax), [FloatToStr(RangeAttribute(vAttr).Max)]));
      end
      else if vAttr is PatternAttribute then
      begin
        if not RALRegexIsMatch(UTF8Encode(vText), StringRAL('^(?:' +
           PatternAttribute(vAttr).Pattern + ')$')) then
          Fail(string(wmValPattern));
      end
      else if vAttr is EmailAttribute then
      begin
        if not RALRegexIsMatch(UTF8Encode(vText),
           '^[A-Za-z0-9._%+-]+@[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?(?:\.[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?)+$') then
          Fail(string(wmValEmail));
      end;
    end;
  end;
end;

{ the factory hook: the class's own Create without parameters }
function RTTIFactory(AClass: TClass): TObject;
var
  vType: TRttiType;
  vMethod: TRttiMethod;
begin
  Result := nil;
  vType := gContext.GetType(AClass);
  if vType = nil then
    Exit;
  for vMethod in vType.GetMethods('Create') do
    if vMethod.IsConstructor and (Length(vMethod.GetParameters) = 0) then
    begin
      Result := vMethod.Invoke(AClass, []).AsObject;
      Exit;
    end;
end;

{ TRALObjectRTTI }

function IsNullable(AType: TRttiType; out AHas, AValue: TRttiField): boolean;
begin
  AHas := nil;
  AValue := nil;
  Result := False;
  if (AType = nil) or (AType.TypeKind <> tkRecord) then
    Exit;
  AHas := AType.GetField('HasValue');
  AValue := AType.GetField('Value');
  Result := (AHas <> nil) and (AValue <> nil) and (Length(AType.GetFields) = 2);
end;

function IsGenericList(AObj: TObject): boolean;
var
  vName: string;
begin
  vName := AObj.ClassName;
  Result := (Pos('TList<', vName) = 1) or (Pos('TObjectList<', vName) = 1);
end;

procedure WriteValue(const AValue: TValue; AWriter: TRALJSONTextWriter;
  AOptions: TRALObjectJSONOptions; ADepth: Integer);
var
  vType: TRttiType;
  vField, vHas, vVal: TRttiField;
  vInt: Integer;
  vArr: TValue;
  vMethod: TRttiMethod;
  vInfo: PTypeInfo;
begin
  if ADepth > AOptions.MaxDepth then
  begin
    AWriter.ValueNull;
    Exit;
  end;
  vInfo := AValue.TypeInfo;
  case AValue.Kind of
    tkUnknown:
      AWriter.ValueNull;
    tkInteger, tkInt64:
      AWriter.ValueInteger(AValue.AsInt64);
    tkFloat:
      if (vInfo = TypeInfo(TDateTime)) or (vInfo = TypeInfo(TDate)) or (vInfo = TypeInfo(TTime)) then
      begin
        if AValue.AsExtended = 0 then
          AWriter.ValueNull
        else if AOptions.DateFormat = dfISO8601UTC then
          AWriter.ValueString(RALDateTimeToISO8601(RALDateTimeToGMT(AValue.AsExtended), True))
        else
          AWriter.ValueString(RALDateTimeToISO8601(AValue.AsExtended, False));
      end
      else
        AWriter.ValueFloat(AValue.AsExtended);
    tkEnumeration:
      if vInfo = TypeInfo(Boolean) then
        AWriter.ValueBoolean(AValue.AsBoolean)
      else if AOptions.EnumAs = eaNumber then
        AWriter.ValueInteger(AValue.AsOrdinal)
      else
        AWriter.ValueString(StringRAL(GetEnumName(vInfo, AValue.AsOrdinal)));
    tkString, tkLString, tkWString, tkUString, tkChar, tkWChar:
      AWriter.ValueString(UTF8Encode(AValue.ToString));
    tkClass:
      if AValue.AsObject = nil then
        AWriter.ValueNull
      else if IsGenericList(AValue.AsObject) then
      begin
        vMethod := gContext.GetType(AValue.AsObject.ClassType).GetMethod('ToArray');
        vArr := vMethod.Invoke(AValue.AsObject, []);
        WriteValue(vArr, AWriter, AOptions, ADepth + 1);
      end
      else
        TRALObjectJSON.WriteObject(AValue.AsObject, AWriter, AOptions, ADepth);
    tkDynArray, tkArray:
    begin
      AWriter.BeginArray;
      for vInt := 0 to AValue.GetArrayLength - 1 do
        WriteValue(AValue.GetArrayElement(vInt), AWriter, AOptions, ADepth + 1);
      AWriter.EndArray;
    end;
    tkRecord:
    begin
      vType := gContext.GetType(vInfo);
      if IsNullable(vType, vHas, vVal) then
      begin
        if vHas.GetValue(AValue.GetReferenceToRawData).AsBoolean then
          WriteValue(vVal.GetValue(AValue.GetReferenceToRawData), AWriter, AOptions, ADepth + 1)
        else
          AWriter.ValueNull;
        Exit;
      end;
      AWriter.BeginObject;
      for vField in vType.GetFields do
      begin
        if vField.FieldType = nil then
          Continue;
        AWriter.Name(TRALObjectJSON.JSONName(StringRAL(vField.Name), AOptions.Naming));
        WriteValue(vField.GetValue(AValue.GetReferenceToRawData), AWriter, AOptions, ADepth + 1);
      end;
      AWriter.EndObject;
    end;
  else
    AWriter.ValueString(UTF8Encode(AValue.ToString));
  end;
end;

function WriteHook(AObj: TObject; AWriter: TRALJSONTextWriter;
  AOptions: TRALObjectJSONOptions; ADepth: IntegerRAL): boolean;
var
  vArr: TValue;
begin
  Result := IsGenericList(AObj);
  if not Result then
    Exit;
  vArr := gContext.GetType(AObj.ClassType).GetMethod('ToArray').Invoke(AObj, []);
  WriteValue(vArr, AWriter, AOptions, ADepth + 1);
end;

class function TRALObjectRTTI.ValueToText(const AValue: TValue;
  AOptions: TRALObjectJSONOptions): StringRAL;
var
  vWriter: TRALJSONTextWriter;
  vOwned: boolean;
begin
  vOwned := AOptions = nil;
  if vOwned then
    AOptions := TRALObjectJSONOptions.Create;
  vWriter := TRALJSONTextWriter.Create;
  try
    WriteValue(AValue, vWriter, AOptions, 0);
    Result := vWriter.Text;
  finally
    vWriter.Free;
    if vOwned then
      AOptions.Free;
  end;
end;

class function TRALObjectRTTI.ToText<T>(const AValue: T; AOptions: TRALObjectJSONOptions): StringRAL;
begin
  Result := ValueToText(TValue.From<T>(AValue), AOptions);
end;

function ReadValue(AJSON: TRALJSONValue; ATypeInfo: PTypeInfo;
  AOptions: TRALObjectJSONOptions; ADepth: Integer): TValue;
var
  vType, vElem: TRttiType;
  vField, vHas, vVal: TRttiField;
  vInt: Integer;
  vItems: array of TValue;
  vObj: TObject;
  vAdd: TRttiMethod;
  vMember: TRALJSONValue;
  vDate: TDateTime;
  vClass: TClass;
  vText: StringRAL;
begin
  if ADepth > AOptions.MaxDepth then
    raise ERALObjectJSON.Create('JSON nested deeper than MaxDepth');
  vType := gContext.GetType(ATypeInfo);
  case ATypeInfo^.Kind of
    tkInteger:
      if AJSON.JsonType = rjtString then
        Result := TValue.FromOrdinal(ATypeInfo, StrToInt(string(AJSON.AsString)))
      else
        Result := TValue.FromOrdinal(ATypeInfo, AJSON.AsInteger);
    tkInt64:
    begin
      if AJSON.JsonType = rjtString then
        Result := StrToInt64(string(AJSON.AsString))
      else
        Result := AJSON.AsInteger;
      Result := Result.Cast(ATypeInfo);
    end;
    tkFloat:
      if (ATypeInfo = TypeInfo(TDateTime)) or (ATypeInfo = TypeInfo(TDate)) or
         (ATypeInfo = TypeInfo(TTime)) then
      begin
        vDate := 0;
        if AJSON.JsonType = rjtNumber then
          vDate := RALGMTToDateTime(UnixDateDelta + AJSON.AsFloat / SecsPerDay)
        else
        begin
          vText := AJSON.AsString;
          if (vText <> '') and not RALTryISO8601ToDateTime(vText, vDate) then
            raise ERALObjectJSON.CreateFmt('Not a date: %s', [string(vText)]);
          if (vText <> '') and (vText[RALHighStr(vText)] in ['Z', 'z']) then
            vDate := RALGMTToDateTime(vDate);
        end;
        TValue.Make(@vDate, ATypeInfo, Result);
      end
      else
      begin
        Result := TValue.From<Extended>(AJSON.AsFloat);
        Result := Result.Cast(ATypeInfo);
      end;
    tkEnumeration:
      if ATypeInfo = TypeInfo(Boolean) then
        Result := AJSON.AsBoolean
      else if AJSON.JsonType = rjtNumber then
        Result := TValue.FromOrdinal(ATypeInfo, AJSON.AsInteger)
      else
      begin
        vInt := GetEnumValue(ATypeInfo, string(AJSON.AsString));
        if vInt < 0 then
          raise ERALObjectJSON.CreateFmt('"%s" is not a value of %s',
            [string(AJSON.AsString), string(ATypeInfo^.Name)]);
        Result := TValue.FromOrdinal(ATypeInfo, vInt);
      end;
    tkString, tkLString, tkWString, tkUString:
    begin
      Result := TValue.From<string>(UTF8ToString(AJSON.AsString));
      Result := Result.Cast(ATypeInfo);
    end;
    tkDynArray:
    begin
      if not (AJSON is TRALJSONArray) then
        raise ERALObjectJSON.Create('An array is read from a JSON array');
      vElem := TRttiDynamicArrayType(vType).ElementType;
      SetLength(vItems, TRALJSONArray(AJSON).Count);
      for vInt := 0 to High(vItems) do
        vItems[vInt] := ReadValue(TRALJSONArray(AJSON).Get(vInt), vElem.Handle, AOptions, ADepth + 1);
      Result := TValue.FromArray(ATypeInfo, vItems);
    end;
    tkClass:
    begin
      if AJSON.IsNull then
      begin
        Result := TValue.From<TObject>(nil);
        Exit;
      end;
      vClass := GetTypeData(ATypeInfo)^.ClassType;
      vObj := RALCreateObject(vClass);
      try
        if (Pos('TList<', vClass.ClassName) = 1) or (Pos('TObjectList<', vClass.ClassName) = 1) then
        begin
          vAdd := nil;
          for vAdd in vType.GetMethods('Add') do
            if Length(vAdd.GetParameters) = 1 then
              Break;
          if (vAdd = nil) or not (AJSON is TRALJSONArray) then
            raise ERALObjectJSON.CreateFmt('%s is read from an array', [vClass.ClassName]);
          vElem := vAdd.GetParameters[0].ParamType;
          for vInt := 0 to TRALJSONArray(AJSON).Count - 1 do
            vAdd.Invoke(vObj, [ReadValue(TRALJSONArray(AJSON).Get(vInt), vElem.Handle,
              AOptions, ADepth + 1)]);
        end
        else
          TRALObjectJSON.FromJSON(AJSON, vObj, AOptions);
      except
        vObj.Free;
        raise;
      end;
      TValue.Make(@vObj, ATypeInfo, Result);
    end;
    tkRecord:
    begin
      TValue.Make(nil, ATypeInfo, Result);
      if IsNullable(vType, vHas, vVal) then
      begin
        if not AJSON.IsNull then
        begin
          vVal.SetValue(Result.GetReferenceToRawData,
            ReadValue(AJSON, vVal.FieldType.Handle, AOptions, ADepth + 1));
          vHas.SetValue(Result.GetReferenceToRawData, True);
        end;
        Exit;
      end;
      if not (AJSON is TRALJSONObject) then
        raise ERALObjectJSON.Create('A record is read from a JSON object');
      for vField in vType.GetFields do
      begin
        if vField.FieldType = nil then
          Continue;
        vMember := TRALJSONObject(AJSON).Get(TRALObjectJSON.JSONName(StringRAL(vField.Name),
          AOptions.Naming));
        if vMember = nil then
          vMember := TRALJSONObject(AJSON).Get(StringRAL(vField.Name));
        if (vMember = nil) or (vMember.IsNull and (vField.FieldType.TypeKind <> tkRecord)) then
          Continue;
        vField.SetValue(Result.GetReferenceToRawData,
          ReadValue(vMember, vField.FieldType.Handle, AOptions, ADepth + 1));
      end;
    end;
  else
    raise ERALObjectJSON.CreateFmt('%s cannot be read', [string(ATypeInfo^.Name)]);
  end;
end;

function ReadHook(AValue: TRALJSONValue; AObj: TObject;
  AOptions: TRALObjectJSONOptions; ADepth: IntegerRAL): boolean;
var
  vType, vElem: TRttiType;
  vAdd, vClear: TRttiMethod;
  vInt: Integer;
begin
  Result := IsGenericList(AObj);
  if not Result then
    Exit;
  if not (AValue is TRALJSONArray) then
    raise ERALObjectJSON.CreateFmt('%s is read from an array', [AObj.ClassName]);
  vType := gContext.GetType(AObj.ClassType);
  vClear := vType.GetMethod('Clear');
  if vClear <> nil then
    vClear.Invoke(AObj, []);
  vAdd := nil;
  for vAdd in vType.GetMethods('Add') do
    if Length(vAdd.GetParameters) = 1 then
      Break;
  vElem := vAdd.GetParameters[0].ParamType;
  for vInt := 0 to TRALJSONArray(AValue).Count - 1 do
    vAdd.Invoke(AObj, [ReadValue(TRALJSONArray(AValue).Get(vInt), vElem.Handle, AOptions,
      ADepth + 1)]);
end;

class function TRALObjectRTTI.ValueFromJSON(AJSON: TRALJSONValue; ATypeInfo: PTypeInfo;
  AOptions: TRALObjectJSONOptions): TValue;
var
  vOwned: boolean;
begin
  vOwned := AOptions = nil;
  if vOwned then
    AOptions := TRALObjectJSONOptions.Create;
  try
    Result := ReadValue(AJSON, ATypeInfo, AOptions, 0);
  finally
    if vOwned then
      AOptions.Free;
  end;
end;

class function TRALObjectRTTI.FromText<T>(const AText: StringRAL;
  AOptions: TRALObjectJSONOptions): T;
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
    Result := ValueFromJSON(vJSON, TypeInfo(T), AOptions).AsType<T>;
  finally
    vJSON.Free;
  end;
end;

initialization
  gContext := TRttiContext.Create;
  gCache := TStringList.Create;
  gLock := TCriticalSection.Create;
  RALPropertyNameHook := NameHook;
  RALObjectFactoryHook := RTTIFactory;
  RALObjectWriteHook := WriteHook;
  RALObjectReadHook := ReadHook;
  RALRegisterValidationHook(AttributeValidation);

finalization
  RALPropertyNameHook := nil;
  RALObjectFactoryHook := nil;
  RALObjectWriteHook := nil;
  RALObjectReadHook := nil;
  RALUnregisterValidationHook(AttributeValidation);
  FreeAndNil(gLock);
  FreeAndNil(gCache);
  gContext.Free;
{$ENDIF}

end.
