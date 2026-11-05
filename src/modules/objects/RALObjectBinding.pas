/// Binding: objects read from the request (body by Content-Type, params by
/// name), validated by rules, and written into the response by Accept; the
/// runner of routes with OnReplyObject and the JSON schema of a class for the
/// API documentation.
unit RALObjectBinding;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, TypInfo,
  RALTypes, RALConsts, RALTools, RALJson, RALParams, RALRequest, RALResponse,
  RALRoutes, RALRegex, RALCustomObjects, RALObjectJSON, RALObjectFormat;

type
  { A vrCustom rule (ARule.Value names it) or any check of your own: add an
    error to AErrors (Field=message) to refuse. }
  TRALCustomValidation = procedure(AObj: TObject; ARule: TRALValidationRule;
    AErrors: TStrings) of object;
  { Extra validation of AObj (the attributes of RALObjectRTTI register one). }
  TRALObjectValidationHook = procedure(AObj: TObject; AErrors: TStrings);

  { How the binding behaves; one global instance (RALObjectSettings), which the
    TRALObjectSettings component edits in the Object Inspector. }
  TRALObjectBindingSettings = class(TPersistent)
  private
    FJSON: TRALObjectJSONOptions;
    FUnreadableStatus: IntegerRAL;
    FValidationStatus: IntegerRAL;
    FOnCustomValidation: TRALCustomValidation;
    procedure SetJSON(const AValue: TRALObjectJSONOptions);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;

    /// Called for vrCustom rules.
    property OnCustomValidation: TRALCustomValidation read FOnCustomValidation
      write FOnCustomValidation;
  published
    /// Names, dates, enumerations and the rest of the conversion.
    property JSON: TRALObjectJSONOptions read FJSON write SetJSON;
    /// Status of a body that cannot be read (broken JSON).
    property UnreadableStatus: IntegerRAL read FUnreadableStatus write FUnreadableStatus default 400;
    /// Status of a body that breaks a rule.
    property ValidationStatus: IntegerRAL read FValidationStatus write FValidationStatus default 422;
  end;

  { The binding settings in the Object Inspector: drop one on a form; it edits
    the global settings (RALObjectSettings), so one per application. }
  TRALObjectSettings = class(TRALComponent)
  private
    function GetSettings: TRALObjectBindingSettings;
    function GetOnCustomValidation: TRALCustomValidation;
    procedure SetSettings(const AValue: TRALObjectBindingSettings);
    procedure SetOnCustomValidation(const AValue: TRALCustomValidation);
  published
    /// The global settings of the binding.
    property Settings: TRALObjectBindingSettings read GetSettings write SetSettings;
    /// Called for the vrCustom rules.
    property OnCustomValidation: TRALCustomValidation read GetOnCustomValidation
      write SetOnCustomValidation;
  end;

/// The global settings of the binding.
function RALObjectSettings: TRALObjectBindingSettings;
{ Reads the body of ARequest into AObj with the formatter of its Content-Type.
  False with AError (and AStatus 415 or 400) when it cannot. }
function RALReadBody(ARequest: TRALRequest; AObj: TObject; out AError: StringRAL;
  out AStatus: IntegerRAL): boolean; overload;
/// Reads the body into AObj; raises ERALObjectJSON when it cannot.
procedure RALReadBody(ARequest: TRALRequest; AObj: TObject); overload;
{ Fills the published properties of AObj from the params of ARequest (path,
  query, header, cookie, field) of the same name (or JSON name). }
procedure RALReadParams(ARequest: TRALRequest; AObj: TObject);
{ Answers AObj with AStatus in the format Accept asks (406 when none); frees AObj
  with AOwns. }
procedure RALAnswerObject(ARequest: TRALRequest; AResponse: TRALResponse;
  AStatus: IntegerRAL; AObj: TObject; AOwns: boolean = False);
{ Checks AObj against ARules (and the registered hooks); each failure is a line
  'field=message' in AErrors. True when none. }
function RALValidateObject(AObj: TObject; ARules: TRALValidationRules; AErrors: TStrings): boolean;
{ Answers the problem+json of a validation failure: title, status and 'errors'
  with the messages of each field. }
procedure RALAnswerValidation(ARequest: TRALRequest; AResponse: TRALResponse;
  AStatus: IntegerRAL; AErrors: TStrings);
/// The JSON schema (draft 2020-12, as OpenAPI 3.1 uses) of AClass, with ARules.
function RALObjectSchema(AClass: TClass; ARules: TRALValidationRules = nil): StringRAL;
/// Registers AHook to run in every validation (the attributes' rules).
procedure RALRegisterValidationHook(AHook: TRALObjectValidationHook);
/// Removes AHook.
procedure RALUnregisterValidationHook(AHook: TRALObjectValidationHook);

implementation

var
  gSettings: TRALObjectBindingSettings;
  gHooks: array of TRALObjectValidationHook;

function RALObjectSettings: TRALObjectBindingSettings;
begin
  Result := gSettings;
end;

{ TRALObjectSettings }

function TRALObjectSettings.GetSettings: TRALObjectBindingSettings;
begin
  Result := gSettings;
end;

procedure TRALObjectSettings.SetSettings(const AValue: TRALObjectBindingSettings);
begin
  gSettings.Assign(AValue);
end;

function TRALObjectSettings.GetOnCustomValidation: TRALCustomValidation;
begin
  Result := gSettings.OnCustomValidation;
end;

procedure TRALObjectSettings.SetOnCustomValidation(const AValue: TRALCustomValidation);
begin
  gSettings.OnCustomValidation := AValue;
end;

procedure RALRegisterValidationHook(AHook: TRALObjectValidationHook);
begin
  SetLength(gHooks, Length(gHooks) + 1);
  gHooks[High(gHooks)] := AHook;
end;

procedure RALUnregisterValidationHook(AHook: TRALObjectValidationHook);
var
  vInt, vJ: IntegerRAL;
begin
  for vInt := High(gHooks) downto 0 do
    if @gHooks[vInt] = @AHook then
    begin
      for vJ := vInt to High(gHooks) - 1 do
        gHooks[vJ] := gHooks[vJ + 1];
      SetLength(gHooks, Length(gHooks) - 1);
    end;
end;

{ TRALObjectBindingSettings }

constructor TRALObjectBindingSettings.Create;
begin
  inherited Create;
  FJSON := TRALObjectJSONOptions.Create;
  FUnreadableStatus := HTTP_BadRequest;
  FValidationStatus := 422;
end;

destructor TRALObjectBindingSettings.Destroy;
begin
  FreeAndNil(FJSON);
  inherited;
end;

procedure TRALObjectBindingSettings.SetJSON(const AValue: TRALObjectJSONOptions);
begin
  FJSON.Assign(AValue);
end;

procedure TRALObjectBindingSettings.AssignTo(Dest: TPersistent);
var
  vDest: TRALObjectBindingSettings;
begin
  if not (Dest is TRALObjectBindingSettings) then
  begin
    inherited AssignTo(Dest);
    Exit;
  end;
  vDest := TRALObjectBindingSettings(Dest);
  vDest.JSON := FJSON;
  vDest.UnreadableStatus := FUnreadableStatus;
  vDest.ValidationStatus := FValidationStatus;
  vDest.OnCustomValidation := FOnCustomValidation;
end;

{ binding }

function RALReadBody(ARequest: TRALRequest; AObj: TObject; out AError: StringRAL;
  out AStatus: IntegerRAL): boolean;
var
  vFmt: TRALObjectFormatterClass;
  vType: StringRAL;
begin
  Result := False;
  AError := '';
  AStatus := 0;
  vType := RALBareMediaType(ARequest.ContentType);
  vFmt := RALFormatterForRead(vType, AObj.ClassType);
  if vFmt = nil then
  begin
    AStatus := HTTP_UnsupportedMedia;
    AError := StringRAL(Format(emObjectNoReader, [string(vType)]));
    Exit;
  end;
  try
    vFmt.Read(ARequest, AObj, gSettings.JSON);
    Result := True;
  except
    on E: Exception do
    begin
      AStatus := gSettings.UnreadableStatus;
      AError := StringRAL(Format(emObjectUnreadable, [E.Message]));
    end;
  end;
end;

procedure RALReadBody(ARequest: TRALRequest; AObj: TObject);
var
  vError: StringRAL;
  vStatus: IntegerRAL;
begin
  if not RALReadBody(ARequest, AObj, vError, vStatus) then
    raise ERALObjectJSON.Create(string(vError));
end;

procedure RALReadParams(ARequest: TRALRequest; AObj: TObject);
var
  vJSON: TRALJSONObject;
  vInt: IntegerRAL;
  vParam: TRALParam;
  vSeen: TStringList;
begin
  vJSON := TRALJSONObject.Create;
  vSeen := TStringList.Create;
  try
    vSeen.Sorted := True;
    for vInt := 0 to ARequest.Params.Count - 1 do
    begin
      vParam := ARequest.Params.Index[vInt];
      if (vParam.ParamName = '') or (vSeen.IndexOf(string(vParam.ParamName)) >= 0) then
        Continue;
      vSeen.Add(string(vParam.ParamName));
      vJSON.Add(vParam.ParamName, vParam.AsString);
    end;
    TRALObjectJSON.FromJSON(vJSON, AObj, gSettings.JSON);
  finally
    vSeen.Free;
    vJSON.Free;
  end;
end;

procedure RALAnswerObject(ARequest: TRALRequest; AResponse: TRALResponse;
  AStatus: IntegerRAL; AObj: TObject; AOwns: boolean);
var
  vFmt: TRALObjectFormatterClass;
  vType: StringRAL;
  vClass: TClass;
begin
  try
    if AObj = nil then
      vClass := TObject
    else
      vClass := AObj.ClassType;
    vFmt := RALFormatterForWrite(ARequest.GetHeader('Accept'), vClass, vType);
    if vFmt = nil then
    begin
      AResponse.AnswerError(HTTP_NotAcceptable, emObjectNoWriter);
      Exit;
    end;
    if AObj = nil then
      AResponse.Answer(AStatus, 'null', vType)
    else
      AResponse.Answer(AStatus, vFmt.Write(AObj, gSettings.JSON), vType);
  finally
    if AOwns then
      AObj.Free;
  end;
end;

{ validation }

function FindProp(AObj: TObject; const AField: StringRAL): PPropInfo;
var
  vList: PPropList;
  vCount, vInt: IntegerRAL;
begin
  Result := nil;
  vCount := GetPropList(PTypeInfo(AObj.ClassInfo), tkProperties, nil, False);
  if vCount <= 0 then
    Exit;
  GetMem(vList, vCount * SizeOf(PPropInfo));
  try
    GetPropList(PTypeInfo(AObj.ClassInfo), tkProperties, vList, False);
    for vInt := 0 to vCount - 1 do
      if SameText(string(vList^[vInt]^.Name), string(AField)) or
         (TRALObjectJSON.JSONName(StringRAL(vList^[vInt]^.Name), gSettings.JSON.Naming) = AField) then
      begin
        Result := vList^[vInt];
        Exit;
      end;
  finally
    FreeMem(vList);
  end;
end;

{ the value of the property as text, and whether it is empty }
function ValueOf(AObj: TObject; AProp: PPropInfo; out AEmpty: boolean;
  out AIsNumber: boolean; out ANumber: Double): StringRAL;
var
  vType: PTypeInfo;
  vChild: TObject;
begin
  vType := RALPropType(AProp);
  AIsNumber := False;
  ANumber := 0;
  AEmpty := False;
  case vType^.Kind of
    tkInteger, tkInt64 {$IFDEF FPC}, tkQWord{$ENDIF}:
    begin
      AIsNumber := True;
      if vType^.Kind = tkInteger then
        ANumber := GetOrdProp(AObj, AProp)
      else
        ANumber := GetInt64Prop(AObj, AProp);
      Result := StringRAL(FloatToStr(ANumber));
    end;
    tkFloat:
    begin
      AIsNumber := True;
      ANumber := GetFloatProp(AObj, AProp);
      Result := StringRAL(FloatToStr(ANumber));
      AEmpty := (ANumber = 0) and ((vType = TypeInfo(TDateTime)) or (vType = TypeInfo(TDate)));
    end;
    tkEnumeration {$IFDEF FPC}, tkBool{$ENDIF}:
      Result := StringRAL(GetEnumName(vType, GetOrdProp(AObj, AProp)));
    tkClass:
    begin
      vChild := GetObjectProp(AObj, AProp);
      AEmpty := (vChild = nil) or ((vChild is TCollection) and (TCollection(vChild).Count = 0)) or
        ((vChild is TStrings) and (TStrings(vChild).Count = 0));
      Result := '';
    end;
    tkSet:
    begin
      Result := StringRAL(GetSetProp(AObj, AProp, False));
      AEmpty := Result = '';
    end;
  else
    {$IFDEF FPC}
    Result := UTF8Encode(GetUnicodeStrProp(AObj, AProp));
    {$ELSE}
    Result := UTF8Encode(GetStrProp(AObj, AProp));
    {$ENDIF}
    AEmpty := Trim(string(Result)) = '';
  end;
end;

function CharCount(const AText: StringRAL): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  { UTF-8 characters: every byte that does not continue one }
  Result := 0;
  for vInt := POSINISTR to RALHighStr(AText) do
    if (Ord(AText[vInt]) and $C0) <> $80 then
      Inc(Result);
end;

function DefaultMessage(ARule: TRALValidationRule): StringRAL;
begin
  case ARule.Rule of
    vrRequired: Result := wmValRequired;
    vrMinLength: Result := StringRAL(Format(wmValMinLength, [string(ARule.Value)]));
    vrMaxLength: Result := StringRAL(Format(wmValMaxLength, [string(ARule.Value)]));
    vrMin: Result := StringRAL(Format(wmValMin, [string(ARule.Value)]));
    vrMax: Result := StringRAL(Format(wmValMax, [string(ARule.Value)]));
    vrPattern: Result := wmValPattern;
    vrEmail: Result := wmValEmail;
    vrUrl: Result := wmValUrl;
    vrUUID: Result := wmValUUID;
    vrIn: Result := StringRAL(Format(wmValIn, [string(ARule.Value)]));
    vrDate: Result := wmValDate;
  else
    Result := wmValCustom;
  end;
end;

function InvFloat(const AText: StringRAL): Double;
var
  vFmt: TFormatSettings;
begin
  vFmt := {$IFDEF FPC}DefaultFormatSettings{$ELSE}FormatSettings{$ENDIF};
  vFmt.DecimalSeparator := '.';
  Result := StrToFloatDef(string(AText), 0, vFmt);
end;

function RALValidateObject(AObj: TObject; ARules: TRALValidationRules; AErrors: TStrings): boolean;
var
  vInt, vHook: IntegerRAL;
  vRule: TRALValidationRule;
  vProp: PPropInfo;
  vText, vMsg: StringRAL;
  vEmpty, vIsNumber, vOk: boolean;
  vNumber: Double;
  vDate: TDateTime;
  vList: TStringList;
  vCount: IntegerRAL;
begin
  vCount := AErrors.Count;
  if (AObj <> nil) and (ARules <> nil) then
    for vInt := 0 to ARules.Count - 1 do
    begin
      vRule := ARules[vInt];
      if vRule.Rule = vrCustom then
      begin
        if Assigned(gSettings.OnCustomValidation) then
          gSettings.OnCustomValidation(AObj, vRule, AErrors);
        Continue;
      end;
      vProp := FindProp(AObj, vRule.Field);
      if vProp = nil then
        Continue;
      vText := ValueOf(AObj, vProp, vEmpty, vIsNumber, vNumber);
      vOk := True;
      if vRule.Rule = vrRequired then
        vOk := not vEmpty
      else if vEmpty and not vIsNumber then
        vOk := True // an empty optional value is not judged; vrRequired says if it must exist
      else
        case vRule.Rule of
          vrMinLength: vOk := CharCount(vText) >= StrToIntDef(string(vRule.Value), 0);
          vrMaxLength: vOk := CharCount(vText) <= StrToIntDef(string(vRule.Value), MaxInt);
          vrMin: vOk := vIsNumber and (vNumber >= InvFloat(vRule.Value));
          vrMax: vOk := vIsNumber and (vNumber <= InvFloat(vRule.Value));
          vrPattern: vOk := RALRegexIsMatch(vText, '^(?:' + vRule.Value + ')$', []);
          vrEmail: vOk := RALRegexIsMatch(vText,
            '^[A-Za-z0-9.!#$%&''*+/=?^_`{|}~-]+@[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?(?:\.[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?)+$', []);
          vrUrl: vOk := RALRegexIsMatch(vText, '^[Hh][Tt][Tt][Pp][Ss]?://[^\s/?#]+[^\s]*$', []);
          vrUUID: vOk := RALRegexIsMatch(vText,
            '^[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}$', []);
          vrIn:
          begin
            vList := TStringList.Create;
            try
              vList.CommaText := string(vRule.Value);
              vOk := vList.IndexOf(string(vText)) >= 0;
            finally
              vList.Free;
            end;
          end;
          vrDate:
            vOk := vIsNumber or RALTryISO8601ToDateTime(vText, vDate);
        end;
      if not vOk then
      begin
        vMsg := vRule.Message;
        if vMsg = '' then
          vMsg := DefaultMessage(vRule);
        AErrors.Add(string(TRALObjectJSON.JSONName(StringRAL(vProp^.Name), gSettings.JSON.Naming)) +
          '=' + string(vMsg));
      end;
    end;
  if AObj <> nil then
    for vHook := 0 to High(gHooks) do
      gHooks[vHook](AObj, AErrors);
  Result := AErrors.Count = vCount;
end;

procedure RALAnswerValidation(ARequest: TRALRequest; AResponse: TRALResponse;
  AStatus: IntegerRAL; AErrors: TStrings);
var
  vWriter: TRALJSONTextWriter;
  vFields: TStringList;
  vInt, vJ: IntegerRAL;
  vName: string;
begin
  vWriter := TRALJSONTextWriter.Create;
  vFields := TStringList.Create;
  try
    for vInt := 0 to AErrors.Count - 1 do
      if vFields.IndexOf(AErrors.Names[vInt]) < 0 then
        vFields.Add(AErrors.Names[vInt]);
    vWriter.BeginObject;
    vWriter.Name('type');
    vWriter.ValueString('about:blank');
    vWriter.Name('title');
    vWriter.ValueString(wmValTitle);
    vWriter.Name('status');
    vWriter.ValueInteger(AStatus);
    if ARequest.TraceId <> '' then
    begin
      vWriter.Name('traceId');
      vWriter.ValueString(ARequest.TraceId);
    end;
    vWriter.Name('errors');
    vWriter.BeginObject;
    for vInt := 0 to vFields.Count - 1 do
    begin
      vName := vFields[vInt];
      vWriter.Name(StringRAL(vName));
      vWriter.BeginArray;
      for vJ := 0 to AErrors.Count - 1 do
        if AErrors.Names[vJ] = vName then
          vWriter.ValueString(StringRAL(AErrors.ValueFromIndex[vJ]));
      vWriter.EndArray;
    end;
    vWriter.EndObject;
    vWriter.EndObject;
    AResponse.Answer(AStatus, vWriter.Text, 'application/problem+json');
  finally
    vFields.Free;
    vWriter.Free;
  end;
end;

procedure RunObjectRoute(ARoute: TRALBaseRoute; ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vClass: TClass;
  vObj: TObject;
  vError: StringRAL;
  vStatus: IntegerRAL;
  vErrors: TStringList;
begin
  vObj := nil;
  if ARoute.BodyClass <> '' then
  begin
    vClass := RALFindObjectClass(ARoute.BodyClass);
    if vClass = nil then
    begin
      AResponse.AnswerError(HTTP_InternalError,
        StringRAL(Format(emObjectNoBodyClass, [string(ARoute.BodyClass)])));
      Exit;
    end;
    vObj := RALCreateObject(vClass);
  end;
  vErrors := TStringList.Create;
  try
    if vObj <> nil then
    begin
      if not RALReadBody(ARequest, vObj, vError, vStatus) then
      begin
        AResponse.AnswerProblem(vStatus, vError);
        Exit;
      end;
      if not RALValidateObject(vObj, ARoute.ValidationRules, vErrors) then
      begin
        RALAnswerValidation(ARequest, AResponse, gSettings.ValidationStatus, vErrors);
        Exit;
      end;
    end;
    ARoute.OnReplyObject(ARequest, AResponse, vObj);
  finally
    vErrors.Free;
    vObj.Free;
  end;
end;

{ schema }

procedure SchemaOf(AClass: TClass; ARules: TRALValidationRules; AWriter: TRALJSONTextWriter;
  ADepth: IntegerRAL);
var
  vList: PPropList;
  vCount, vInt, vRule: IntegerRAL;
  vProp: PPropInfo;
  vType: PTypeInfo;
  vData: PTypeData;
  vRequired: TStringList;
  vName: StringRAL;
  vConv: TRALObjectConverterClass;
  vChild: TClass;
  vEnum: IntegerRAL;
  vR: TRALValidationRule;
begin
  vConv := RALObjectConverterOf(AClass);
  if (vConv <> nil) and (vConv.Schema <> '') then
  begin
    AWriter.ValueRaw(vConv.Schema);
    Exit;
  end;
  if AClass.InheritsFrom(TStrings) then
  begin
    AWriter.ValueRaw('{"type":"array","items":{"type":"string"}}');
    Exit;
  end;
  if AClass.InheritsFrom(TStream) then
  begin
    AWriter.ValueRaw('{"type":"string","contentEncoding":"base64"}');
    Exit;
  end;
  if (ADepth > 8) or (AClass.ClassInfo = nil) then
  begin
    AWriter.ValueRaw('{"type":"object"}');
    Exit;
  end;
  if RALListItemClass(AClass) <> nil then
  begin
    AWriter.BeginObject;
    AWriter.Name('type');
    AWriter.ValueString('array');
    AWriter.Name('items');
    SchemaOf(RALListItemClass(AClass), nil, AWriter, ADepth + 1);
    AWriter.EndObject;
    Exit;
  end;
  vRequired := TStringList.Create;
  AWriter.BeginObject;
  AWriter.Name('type');
  AWriter.ValueString('object');
  AWriter.Name('properties');
  AWriter.BeginObject;
  vCount := GetPropList(PTypeInfo(AClass.ClassInfo), tkProperties, nil, False);
  vList := nil;
  try
    if vCount > 0 then
    begin
      GetMem(vList, vCount * SizeOf(PPropInfo));
      GetPropList(PTypeInfo(AClass.ClassInfo), tkProperties, vList, False);
    end;
    for vInt := 0 to vCount - 1 do
    begin
      vProp := vList^[vInt];
      vType := RALPropType(vProp);
      if vType^.Kind in [tkMethod, tkInterface, tkArray, tkRecord, tkDynArray] then
        Continue;
      vName := TRALObjectJSON.JSONName(StringRAL(vProp^.Name), gSettings.JSON.Naming);
      AWriter.Name(vName);
      vData := GetTypeData(vType);
      case vType^.Kind of
        tkInteger, tkInt64 {$IFDEF FPC}, tkQWord{$ENDIF}:
        begin
          AWriter.BeginObject;
          AWriter.Name('type');
          AWriter.ValueString('integer');
          if vType^.Kind = tkInt64 then
          begin
            AWriter.Name('format');
            AWriter.ValueString('int64');
          end;
        end;
        tkFloat:
        begin
          AWriter.BeginObject;
          AWriter.Name('type');
          if (vType = TypeInfo(TDateTime)) or (vType = TypeInfo(TDate)) or (vType = TypeInfo(TTime)) then
          begin
            AWriter.ValueString('string');
            AWriter.Name('format');
            AWriter.ValueString('date-time');
          end
          else
            AWriter.ValueString('number');
        end;
        tkEnumeration {$IFDEF FPC}, tkBool{$ENDIF}:
        begin
          AWriter.BeginObject;
          AWriter.Name('type');
          {$IFDEF FPC}
          if vType^.Kind = tkBool then
          {$ELSE}
          if vData^.BaseType^ = TypeInfo(Boolean) then
          {$ENDIF}
            AWriter.ValueString('boolean')
          else
          begin
            AWriter.ValueString('string');
            AWriter.Name('enum');
            AWriter.BeginArray;
            for vEnum := vData^.MinValue to vData^.MaxValue do
              AWriter.ValueString(StringRAL(GetEnumName(vType, vEnum)));
            AWriter.EndArray;
          end;
        end;
        tkSet:
        begin
          AWriter.BeginObject;
          AWriter.Name('type');
          AWriter.ValueString('array');
          AWriter.Name('items');
          AWriter.ValueRaw('{"type":"string"}');
          AWriter.Name('uniqueItems');
          AWriter.ValueBoolean(True);
        end;
        tkClass:
        begin
          vChild := vData^.ClassType;
          if vChild.InheritsFrom(TCollection) then
          begin
            AWriter.BeginObject;
            AWriter.Name('type');
            AWriter.ValueString('array');
          end
          else
          begin
            SchemaOf(vChild, nil, AWriter, ADepth + 1);
            Continue;
          end;
        end;
      else
        AWriter.BeginObject;
        AWriter.Name('type');
        AWriter.ValueString('string');
      end;
      { the rules of this property as JSON schema keywords }
      if ARules <> nil then
        for vRule := 0 to ARules.Count - 1 do
        begin
          vR := ARules[vRule];
          if not (SameText(string(vR.Field), string(vProp^.Name)) or (vR.Field = vName)) then
            Continue;
          case vR.Rule of
            vrRequired: if vRequired.IndexOf(string(vName)) < 0 then vRequired.Add(string(vName));
            vrMinLength: begin AWriter.Name('minLength'); AWriter.ValueRaw(vR.Value); end;
            vrMaxLength: begin AWriter.Name('maxLength'); AWriter.ValueRaw(vR.Value); end;
            vrMin: begin AWriter.Name('minimum'); AWriter.ValueRaw(vR.Value); end;
            vrMax: begin AWriter.Name('maximum'); AWriter.ValueRaw(vR.Value); end;
            vrPattern: begin AWriter.Name('pattern'); AWriter.ValueString(vR.Value); end;
            vrEmail: begin AWriter.Name('format'); AWriter.ValueString('email'); end;
            vrUrl: begin AWriter.Name('format'); AWriter.ValueString('uri'); end;
            vrUUID: begin AWriter.Name('format'); AWriter.ValueString('uuid'); end;
            vrDate: begin AWriter.Name('format'); AWriter.ValueString('date-time'); end;
          end;
        end;
      AWriter.EndObject;
    end;
  finally
    if vList <> nil then
      FreeMem(vList);
  end;
  AWriter.EndObject;
  if vRequired.Count > 0 then
  begin
    AWriter.Name('required');
    AWriter.BeginArray;
    for vInt := 0 to vRequired.Count - 1 do
      AWriter.ValueString(StringRAL(vRequired[vInt]));
    AWriter.EndArray;
  end;
  AWriter.EndObject;
  vRequired.Free;
end;

function RALObjectSchema(AClass: TClass; ARules: TRALValidationRules): StringRAL;
var
  vWriter: TRALJSONTextWriter;
begin
  vWriter := TRALJSONTextWriter.Create;
  try
    SchemaOf(AClass, ARules, vWriter, 0);
    Result := vWriter.Text;
  finally
    vWriter.Free;
  end;
end;

function SchemaProvider(const AClassName: StringRAL; ARules: TRALValidationRules): StringRAL;
var
  vClass: TClass;
begin
  Result := '';
  vClass := RALFindObjectClass(AClassName);
  if vClass <> nil then
    Result := RALObjectSchema(vClass, ARules);
end;

initialization
  gSettings := TRALObjectBindingSettings.Create;
  RALObjectRouteRunner := RunObjectRoute;
  RALObjectSchemaProvider := SchemaProvider;

finalization
  RALObjectRouteRunner := nil;
  RALObjectSchemaProvider := nil;
  FreeAndNil(gSettings);

end.
