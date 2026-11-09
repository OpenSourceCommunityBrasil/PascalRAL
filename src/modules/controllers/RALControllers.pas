/// Controllers by attributes: a class whose methods carry [GET], [POST]...
/// becomes routes of a TRALControllerModule, its parameters filled from the
/// path, the query, the headers and the body, its result written in the format
/// Accept asks. Only where the compiler has attributes and extended RTTI
/// (Delphi 2010 and later); the unit compiles empty elsewhere.
unit RALControllers;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALServer, RALRoutes
  {$IF Defined(RALHasAttributes) and Defined(RALHasExtRTTI)}
  , TypInfo, Rtti, RALConsts, RALTools, RALJson, RALRequest, RALResponse,
  RALObjectJSON, RALObjectBinding, RALObjectRTTI
  {$IFEND};

{$IF Defined(RALHasAttributes) and Defined(RALHasExtRTTI)}
type
  /// The path of a controller (on the class) or of an action (on a method).
  PathAttribute = class(TCustomAttribute)
  private
    FPath: string;
  public
    constructor Create(const APath: string);

    /// The path; '{id}' and ':id' are both params.
    property Path: string read FPath;
  end;

  /// Base of the method attributes; APath is added to the controller's path.
  TRALHTTPMethodAttribute = class(TCustomAttribute)
  private
    FPath: string;
  public
    constructor Create(const APath: string = '');

    /// The method the attribute stands for.
    function Method: TRALMethod; virtual; abstract;

    /// The path of the action, after the controller's.
    property Path: string read FPath;
  end;

  /// GET.
  GETAttribute = class(TRALHTTPMethodAttribute)
  public
    function Method: TRALMethod; override;
  end;

  /// POST.
  POSTAttribute = class(TRALHTTPMethodAttribute)
  public
    function Method: TRALMethod; override;
  end;

  /// PUT.
  PUTAttribute = class(TRALHTTPMethodAttribute)
  public
    function Method: TRALMethod; override;
  end;

  /// DELETE.
  DELETEAttribute = class(TRALHTTPMethodAttribute)
  public
    function Method: TRALMethod; override;
  end;

  /// PATCH.
  PATCHAttribute = class(TRALHTTPMethodAttribute)
  public
    function Method: TRALMethod; override;
  end;

  /// The media type the action answers in (else the one Accept asks).
  ProducesAttribute = class(TCustomAttribute)
  private
    FMediaType: string;
  public
    constructor Create(const AMediaType: string);

    /// The media type.
    property MediaType: string read FMediaType;
  end;

  /// Base of the parameter source attributes; AName empty = the parameter's name.
  TRALParamSourceAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string = '');

    /// The name of the value in its source.
    property Name: string read FName;
  end;

  /// A path param ('{id}').
  FromPathAttribute = class(TRALParamSourceAttribute);
  /// A query param.
  FromQueryAttribute = class(TRALParamSourceAttribute);
  /// A header.
  FromHeaderAttribute = class(TRALParamSourceAttribute);
  /// The body (the default for a class, record or array parameter).
  FromBodyAttribute = class(TCustomAttribute);

  /// Roles the action (or every action of the class) asks; see TRALRoute.Roles.
  RolesAttribute = class(TCustomAttribute)
  private
    FRoles: string;
  public
    constructor Create(const ARoles: string);

    /// Roles separated by commas.
    property Roles: string read FRoles;
  end;

  /// The action takes requests without authentication.
  AllowAnonymousAttribute = class(TCustomAttribute);

  { A route that runs one method of a controller: creates the controller,
    fills the parameters, invokes, answers with the result. }
  TRALControllerRoute = class(TRALRoute)
  private
    FController: TClass;
    FMethodName: string;
    FProduces: string;
  public
    /// Runs the action.
    procedure Execute(ARequest: TRALRequest; AResponse: TRALResponse); override;

    /// The method of the action.
    property ActionName: string read FMethodName;
    /// The controller class.
    property Controller: TClass read FController;
  end;

  { A module whose routes come from controller classes: RegisterController
    reads the attributes of a class and creates one route per action. A
    controller is created per request (TRALObject, TComponent or any class
    with a parameterless constructor) and freed after it. }
  TRALControllerModule = class(TRALModuleRoutes)
  private
    FControllers: TStrings;

    procedure SetControllers(const AValue: TStrings);
  protected
    procedure Loaded; override;
    class function RouteClass: TRALRouteClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Creates the routes of the actions of AClass; returns how many.
    function RegisterController(AClass: TClass): IntegerRAL;
  published
    { Names of controller classes (registered with RALRegisterController) whose
      routes are created when the module loads. }
    property Controllers: TStrings read FControllers write SetControllers;
    property Domain;
    property Server;
  end;

/// Makes AClass known by name to TRALControllerModule.Controllers.
procedure RALRegisterController(AClass: TClass);
/// The '{id}' params of APath written as ':id'.
function RALControllerPath(const APath: string): string;
{$IFEND}

implementation

{$IF Defined(RALHasAttributes) and Defined(RALHasExtRTTI)}
var
  gContext: TRttiContext;
  gControllers: TStringList;

procedure RALRegisterController(AClass: TClass);
begin
  if gControllers.IndexOf(AClass.ClassName) < 0 then
    gControllers.AddObject(AClass.ClassName, TObject(AClass));
end;

function RALControllerPath(const APath: string): string;
var
  vInt: Integer;
begin
  Result := '';
  vInt := 1;
  while vInt <= Length(APath) do
  begin
    if APath[vInt] = '{' then
    begin
      Result := Result + ':';
      Inc(vInt);
      while (vInt <= Length(APath)) and (APath[vInt] <> '}') do
      begin
        Result := Result + APath[vInt];
        Inc(vInt);
      end;
    end
    else
      Result := Result + APath[vInt];
    Inc(vInt);
  end;
end;

function JoinPath(const A, B: string): string;
begin
  if A = '' then
    Result := B
  else if B = '' then
    Result := A
  else if (A[Length(A)] = '/') and (B[1] = '/') then
    Result := A + Copy(B, 2, MaxInt)
  else if (A[Length(A)] <> '/') and (B[1] <> '/') then
    Result := A + '/' + B
  else
    Result := A + B;
  if (Result = '') or (Result[1] <> '/') then
    Result := '/' + Result;
end;

{ attributes }

constructor PathAttribute.Create(const APath: string);
begin
  inherited Create;
  FPath := APath;
end;

constructor TRALHTTPMethodAttribute.Create(const APath: string);
begin
  inherited Create;
  FPath := APath;
end;

function GETAttribute.Method: TRALMethod;
begin
  Result := amGET;
end;

function POSTAttribute.Method: TRALMethod;
begin
  Result := amPOST;
end;

function PUTAttribute.Method: TRALMethod;
begin
  Result := amPUT;
end;

function DELETEAttribute.Method: TRALMethod;
begin
  Result := amDELETE;
end;

function PATCHAttribute.Method: TRALMethod;
begin
  Result := amPATCH;
end;

constructor ProducesAttribute.Create(const AMediaType: string);
begin
  inherited Create;
  FMediaType := AMediaType;
end;

constructor TRALParamSourceAttribute.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

constructor RolesAttribute.Create(const ARoles: string);
begin
  inherited Create;
  FRoles := ARoles;
end;

{ conversion of text to a parameter's value }

function TextToValue(const AText: string; AType: TRttiType): TValue;
var
  vInfo: PTypeInfo;
  vDate: TDateTime;
  vFmt: TFormatSettings;
begin
  vInfo := AType.Handle;
  case AType.TypeKind of
    tkInteger:
      Result := TValue.FromOrdinal(vInfo, StrToInt(AText));
    tkInt64:
      Result := TValue.From<Int64>(StrToInt64(AText)).Cast(vInfo);
    tkFloat:
      if (vInfo = TypeInfo(TDateTime)) or (vInfo = TypeInfo(TDate)) then
      begin
        if not RALTryISO8601ToDateTime(UTF8Encode(AText), vDate) then
          raise EConvertError.CreateFmt('"%s" is not a date', [AText]);
        TValue.Make(@vDate, vInfo, Result);
      end
      else
      begin
        vFmt := FormatSettings;
        vFmt.DecimalSeparator := '.';
        Result := TValue.From<Extended>(StrToFloat(AText, vFmt)).Cast(vInfo);
      end;
    tkEnumeration:
      if vInfo = TypeInfo(Boolean) then
        Result := SameText(AText, 'true') or (AText = '1')
      else
        Result := TValue.FromOrdinal(vInfo, GetEnumValue(vInfo, AText));
  else
    Result := TValue.From<string>(AText).Cast(vInfo);
  end;
end;

{ TRALControllerRoute }

procedure TRALControllerRoute.Execute(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vType: TRttiType;
  vMethod, vM: TRttiMethod;
  vParams: TArray<TRttiParameter>;
  vArgs: array of TValue;
  vInt: Integer;
  vAttr: TCustomAttribute;
  vSource: TCustomAttribute;
  vName, vText: string;
  vInstance, vBody: TObject;
  vOwned: TList;
  vResult: TValue;
  vErrors: TStringList;
  vError: StringRAL;
  vStatus: IntegerRAL;
  vJSON: TRALJSONValue;
begin
  vType := gContext.GetType(FController);
  vMethod := nil;
  for vM in vType.GetMethods(FMethodName) do
  begin
    vMethod := vM;
    Break;
  end;
  if vMethod = nil then
  begin
    AResponse.Answer(HTTP_NotFound);
    Exit;
  end;
  vOwned := TList.Create;
  vErrors := TStringList.Create;
  vInstance := RALCreateObject(FController);
  try
    vParams := vMethod.GetParameters;
    SetLength(vArgs, Length(vParams));
    for vInt := 0 to High(vParams) do
    begin
      vSource := nil;
      for vAttr in vParams[vInt].GetAttributes do
        if (vAttr is TRALParamSourceAttribute) or (vAttr is FromBodyAttribute) then
          vSource := vAttr;
      vName := vParams[vInt].Name;
      if (vSource is TRALParamSourceAttribute) and (TRALParamSourceAttribute(vSource).Name <> '') then
        vName := TRALParamSourceAttribute(vSource).Name;

      { the request and the response themselves }
      if (vParams[vInt].ParamType.TypeKind = tkClass) and
         (TRttiInstanceType(vParams[vInt].ParamType).MetaclassType.InheritsFrom(TRALRequest)) then
        vArgs[vInt] := ARequest
      else if (vParams[vInt].ParamType.TypeKind = tkClass) and
         (TRttiInstanceType(vParams[vInt].ParamType).MetaclassType.InheritsFrom(TRALResponse)) then
        vArgs[vInt] := AResponse
      { the body: asked for, or a class, record or array without a source }
      else if (vSource is FromBodyAttribute) or ((vSource = nil) and
         (vParams[vInt].ParamType.TypeKind in [tkClass, tkRecord, tkDynArray])) then
      begin
        if vParams[vInt].ParamType.TypeKind = tkClass then
        begin
          vBody := RALCreateObject(TRttiInstanceType(vParams[vInt].ParamType).MetaclassType);
          vOwned.Add(vBody);
          if not RALReadBody(ARequest, vBody, vError, vStatus) then
          begin
            AResponse.AnswerProblem(vStatus, vError);
            Exit;
          end;
          if not RALValidateObject(vBody, ValidationRules, vErrors) then
          begin
            RALAnswerValidation(ARequest, AResponse, RALObjectSettings.ValidationStatus, vErrors);
            Exit;
          end;
          vArgs[vInt] := vBody;
        end
        else
        begin
          try
            vJSON := TRALJSON.ParseJSON(ARequest.RequestText);
          except
            vJSON := nil;
          end;
          if vJSON = nil then
          begin
            AResponse.AnswerProblem(RALObjectSettings.UnreadableStatus,
              StringRAL(Format(emObjectUnreadable, ['not JSON'])));
            Exit;
          end;
          try
            vArgs[vInt] := TRALObjectRTTI.ValueFromJSON(vJSON, vParams[vInt].ParamType.Handle,
              RALObjectSettings.JSON);
          finally
            vJSON.Free;
          end;
        end;
      end
      else
      begin
        if vSource is FromHeaderAttribute then
          vText := string(ARequest.GetHeader(StringRAL(vName)))
        else
          vText := UTF8ToString(ARequest.ParamByName(StringRAL(vName)).AsString);
        try
          if vText = '' then
            TValue.Make(nil, vParams[vInt].ParamType.Handle, vArgs[vInt])
          else
            vArgs[vInt] := TextToValue(vText, vParams[vInt].ParamType);
        except
          on E: Exception do
          begin
            AResponse.AnswerProblem(HTTP_BadRequest, StringRAL(vName + ': ' + E.Message));
            Exit;
          end;
        end;
      end;
    end;

    vResult := vMethod.Invoke(vInstance, vArgs);

    if vMethod.ReturnType = nil then
    begin
      if AResponse.StatusCode = HTTP_OK then
        AResponse.Answer(HTTP_NoContent, '', '');
    end
    else if vResult.IsObject then
    begin
      if FProduces <> '' then
      begin
        ARequest.Params.DelParam('Accept', rpkHEADER);
        ARequest.Params.AddParam('Accept', StringRAL(FProduces), rpkHEADER);
      end;
      RALAnswerObject(ARequest, AResponse, AResponse.StatusCode, vResult.AsObject, True);
    end
    else if (vResult.Kind in [tkString, tkLString, tkWString, tkUString]) and
            (FProduces <> '') and (Pos('json', FProduces) = 0) then
      AResponse.Answer(AResponse.StatusCode, UTF8Encode(vResult.AsString), StringRAL(FProduces))
    else
      AResponse.Answer(AResponse.StatusCode, TRALObjectRTTI.ValueToText(vResult,
        RALObjectSettings.JSON), 'application/json');
  finally
    for vInt := 0 to vOwned.Count - 1 do
      TObject(vOwned[vInt]).Free;
    vOwned.Free;
    vErrors.Free;
    vInstance.Free;
  end;
end;

{ TRALControllerModule }

constructor TRALControllerModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControllers := TStringList.Create;
end;

destructor TRALControllerModule.Destroy;
begin
  FreeAndNil(FControllers);
  inherited;
end;

class function TRALControllerModule.RouteClass: TRALRouteClass;
begin
  Result := TRALControllerRoute;
end;

procedure TRALControllerModule.SetControllers(const AValue: TStrings);
begin
  FControllers.Assign(AValue);
end;

procedure TRALControllerModule.Loaded;
var
  vInt, vIdx: Integer;
begin
  inherited;
  for vInt := 0 to FControllers.Count - 1 do
  begin
    vIdx := gControllers.IndexOf(Trim(FControllers[vInt]));
    if vIdx >= 0 then
      RegisterController(TClass(gControllers.Objects[vIdx]));
  end;
end;

function TRALControllerModule.RegisterController(AClass: TClass): IntegerRAL;
var
  vType: TRttiType;
  vMethod: TRttiMethod;
  vAttr: TCustomAttribute;
  vBase, vClassRoles, vRoles, vProduces: string;
  vAnonymous, vClassAnonymous: boolean;
  vVerbs: TRALMethods;
  vSub: string;
  vRoute: TRALControllerRoute;
  vPass: Integer;
  vHasParam: boolean;
begin
  Result := 0;
  vType := gContext.GetType(AClass);
  vBase := '';
  vClassRoles := '';
  vClassAnonymous := False;
  for vAttr in vType.GetAttributes do
    if vAttr is PathAttribute then
      vBase := PathAttribute(vAttr).Path
    else if vAttr is RolesAttribute then
      vClassRoles := RolesAttribute(vAttr).Roles
    else if vAttr is AllowAnonymousAttribute then
      vClassAnonymous := True;
  // two passes, literal paths first: among routes of the same weight the first
  // wins, so /x/soma must come before /x/:id
  for vPass := 0 to 1 do
  for vMethod in vType.GetMethods do
  begin
    if vMethod.Parent <> vType then
      Continue;
    vVerbs := [];
    vSub := '';
    vRoles := vClassRoles;
    vProduces := '';
    vAnonymous := vClassAnonymous;
    for vAttr in vMethod.GetAttributes do
      if vAttr is TRALHTTPMethodAttribute then
      begin
        Include(vVerbs, TRALHTTPMethodAttribute(vAttr).Method);
        if TRALHTTPMethodAttribute(vAttr).Path <> '' then
          vSub := TRALHTTPMethodAttribute(vAttr).Path;
      end
      else if vAttr is PathAttribute then
        vSub := PathAttribute(vAttr).Path
      else if vAttr is RolesAttribute then
        vRoles := RolesAttribute(vAttr).Roles
      else if vAttr is ProducesAttribute then
        vProduces := ProducesAttribute(vAttr).MediaType
      else if vAttr is AllowAnonymousAttribute then
        vAnonymous := True;
    if vVerbs = [] then
      Continue;
    vHasParam := Pos('{', vBase + vSub) + Pos(':', vBase + vSub) > 0;
    if vHasParam <> (vPass = 1) then
      Continue;
    vRoute := TRALControllerRoute(NewRoute(StringRAL(RALControllerPath(JoinPath(vBase, vSub)))));
    vRoute.FController := AClass;
    vRoute.FMethodName := vMethod.Name;
    vRoute.FProduces := vProduces;
    vRoute.Name := StringRAL(AClass.ClassName + '.' + vMethod.Name);
    vRoute.AllowedMethods := vVerbs;
    vRoute.Roles := StringRAL(vRoles);
    if vAnonymous then
      vRoute.SkipAuthMethods := [amALL];
    Inc(Result);
  end;
end;

initialization
  gContext := TRttiContext.Create;
  gControllers := TStringList.Create;

finalization
  FreeAndNil(gControllers);
  gContext.Free;
{$IFEND}

end.
