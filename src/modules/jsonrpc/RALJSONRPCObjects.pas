{ Sugar of the JSON-RPC dispatcher: the public methods of an object become
  JSON-RPC methods through extended RTTI (Delphi 2010 and later; empty elsewhere). }
unit RALJSONRPCObjects;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{$IFDEF RALHasExtRTTI}
uses
  Classes, SysUtils, TypInfo, Rtti, SyncObjs,
  RALTypes, RALJson, RALJSONRPC;

{ Declares in ADispatcher one method per public or published method of AObject
  (APrefix + '.' + name, or the name alone when APrefix is empty). }
procedure RALJSONRPCRegisterObject(ADispatcher: TRALJSONRPCDispatcher;
  AObject: TObject; const APrefix: StringRAL = '');
/// Removes from ADispatcher the methods RALJSONRPCRegisterObject declared for AObject.
procedure RALJSONRPCUnregisterObject(ADispatcher: TRALJSONRPCDispatcher;
  AObject: TObject);
{$ENDIF}

implementation

{$IFDEF RALHasExtRTTI}

type
  /// Runs one RTTI method of an object as a JSON-RPC method.
  TRALJSONRPCObjectBinding = class
  private
    /// Keeps the RTTI of FMethod alive.
    FContext: TRttiContext;
    FDispatcher: TRALJSONRPCDispatcher;
    /// The method run.
    FMethod: TRttiMethod;
    FName: StringRAL;
    FObject: TObject;
    /// The parameters of FMethod, in order.
    FParams: TArray<TRttiParameter>;

    /// The argument AIndex of the call ARequest, converted to its Pascal type.
    function ArgumentOf(ARequest: TRALJSONRPCRequest; AIndex: Integer): TValue;
    /// Puts the returned AValue in AResult as JSON.
    procedure ResultOf(const AValue: TValue; AResult: TRALJSONRPCResult);
  public
    /// A binding that calls AMethod of AObject, declared as AName in ADispatcher.
    constructor Create(ADispatcher: TRALJSONRPCDispatcher; AObject: TObject;
      AMethod: TRttiMethod; const AName: StringRAL);
    destructor Destroy; override;

    /// Reads the arguments, calls the method and answers its result.
    procedure Execute(Sender: TObject; ARequest: TRALJSONRPCRequest;
      AResult: TRALJSONRPCResult);

    /// The dispatcher the method was declared in.
    property Dispatcher: TRALJSONRPCDispatcher read FDispatcher;
    /// The method name in the dispatcher.
    property Name: StringRAL read FName;
    /// The object the method runs on.
    property Target: TObject read FObject;
  end;

var
  /// Every binding created, owned until unregistered or the unit ends.
  gBindings: TList = nil;
  /// Guards gBindings.
  gLock: TCriticalSection = nil;

/// Raises the JSON-RPC Invalid params error.
procedure InvalidParams;
begin
  raise ERALJSONRPCError.CreateRPC(cRALJSONRPCInvalidParams,
    string(RALJSONRPCMessage(cRALJSONRPCInvalidParams)));
end;

/// Whether the stop class AClass ends the walk up the ancestors.
function IsStopClass(AClass: TClass): Boolean;
begin
  Result := (AClass = TObject) or (AClass = TPersistent) or
    (AClass = TComponent) or (AClass = TInterfacedObject);
end;

/// The JSON kind a parameter or result of AType maps to; False when unsupported.
function TypeSupported(AType: TRttiType; out AParamType: TRALJSONRPCParamType): Boolean;
begin
  Result := True;
  if AType = nil then
  begin
    Result := False;
    Exit;
  end;
  case AType.TypeKind of
    tkInteger, tkInt64:
      AParamType := jptInteger;
    tkFloat:
      AParamType := jptNumber;
    tkUString, tkLString, tkWString, tkString:
      AParamType := jptString;
    tkEnumeration:
      begin
        AParamType := jptString;
        if AType.Handle = TypeInfo(Boolean) then
          AParamType := jptBoolean;
      end;
    tkClass:
      begin
        AParamType := jptAny;
        Result := TRttiInstanceType(AType).MetaclassType.InheritsFrom(TRALJSON);
      end;
  else
    Result := False;
  end;
end;

{ Whether the method can be exposed: a plain function or procedure whose
  parameters and result are supported. }
function MethodSupported(AMethod: TRttiMethod): Boolean;
var
  vParam: TRttiParameter;
  vType: TRALJSONRPCParamType;
begin
  Result := (AMethod.MethodKind in [mkProcedure, mkFunction]) and
    (AMethod.Visibility in [mvPublic, mvPublished]);
  if not Result then
    Exit;
  for vParam in AMethod.GetParameters do
    if (vParam.Flags * [pfVar, pfOut] <> []) or (vParam.ParamType = nil) or
       not TypeSupported(vParam.ParamType, vType) then
    begin
      Result := False;
      Exit;
    end;
  if AMethod.ReturnType <> nil then
  begin
    Result := TypeSupported(AMethod.ReturnType, vType);
  end;
end;

{ TRALJSONRPCObjectBinding }

constructor TRALJSONRPCObjectBinding.Create(ADispatcher: TRALJSONRPCDispatcher;
  AObject: TObject; AMethod: TRttiMethod; const AName: StringRAL);
begin
  inherited Create;
  FContext := TRttiContext.Create;
  FDispatcher := ADispatcher;
  FObject := AObject;
  FMethod := AMethod;
  FName := AName;
  FParams := AMethod.GetParameters;
end;

destructor TRALJSONRPCObjectBinding.Destroy;
begin
  FContext.Free;
  inherited;
end;

function TRALJSONRPCObjectBinding.ArgumentOf(ARequest: TRALJSONRPCRequest;
  AIndex: Integer): TValue;
var
  vParam: TRttiParameter;
  vJSON: TRALJSON;
  vInt: Int64;
  vData: PTypeData;
  vSingle: Single;
  vDouble: Double;
  vExt: Extended;
  vCurr: Currency;
begin
  vParam := FParams[AIndex];
  vJSON := ARequest.FindParam(StringRAL(vParam.Name));
  if vJSON = nil then
    InvalidParams;
  case vParam.ParamType.TypeKind of
    tkInteger, tkInt64:
      begin
        if vJSON.Kind <> rjkNumber then
          InvalidParams;
        vInt := vJSON.AsInt64;
        if vParam.ParamType.TypeKind = tkInt64 then
          Result := TValue.From<Int64>(vInt)
        else
        begin
          vData := GetTypeData(vParam.ParamType.Handle);
          if vData^.OrdType = otULong then
          begin
            if (vInt < 0) or (vInt > High(Cardinal)) then
              InvalidParams;
          end
          else if (vInt < vData^.MinValue) or (vInt > vData^.MaxValue) then
            InvalidParams;
          TValue.Make(NativeInt(vInt), vParam.ParamType.Handle, Result);
        end;
      end;
    tkFloat:
      begin
        if vJSON.Kind <> rjkNumber then
          InvalidParams;
        case GetTypeData(vParam.ParamType.Handle)^.FloatType of
          ftSingle:
            begin
              vSingle := vJSON.AsNumber;
              TValue.Make(@vSingle, vParam.ParamType.Handle, Result);
            end;
          ftDouble:
            begin
              vDouble := vJSON.AsNumber;
              TValue.Make(@vDouble, vParam.ParamType.Handle, Result);
            end;
          ftExtended:
            begin
              vExt := vJSON.AsNumber;
              TValue.Make(@vExt, vParam.ParamType.Handle, Result);
            end;
          ftCurr:
            begin
              vCurr := vJSON.AsNumber;
              TValue.Make(@vCurr, vParam.ParamType.Handle, Result);
            end;
        else
          InvalidParams;
        end;
      end;
    tkUString, tkLString, tkWString, tkString:
      begin
        if vJSON.Kind <> rjkString then
          InvalidParams;
        Result := TValue.From<string>(UTF8ToString(vJSON.AsString));
      end;
    tkEnumeration:
      begin
        if vJSON.Kind <> rjkBoolean then
          InvalidParams;
        Result := TValue.From<Boolean>(vJSON.AsBoolean);
      end;
    tkClass:
      Result := TValue.From<TRALJSON>(vJSON);
  else
    InvalidParams;
  end;
end;

procedure TRALJSONRPCObjectBinding.Execute(Sender: TObject;
  ARequest: TRALJSONRPCRequest; AResult: TRALJSONRPCResult);
var
  vArgs: array of TValue;
  vInt: Integer;
  vResult: TValue;
begin
  SetLength(vArgs, Length(FParams));
  for vInt := 0 to High(FParams) do
    vArgs[vInt] := ArgumentOf(ARequest, vInt);
  vResult := FMethod.Invoke(FObject, vArgs);
  ResultOf(vResult, AResult);
end;

procedure TRALJSONRPCObjectBinding.ResultOf(const AValue: TValue;
  AResult: TRALJSONRPCResult);
var
  vObj: TObject;
begin
  if FMethod.ReturnType = nil then
  begin
    AResult.SetNull;
    Exit;
  end;
  case FMethod.ReturnType.TypeKind of
    tkInteger, tkInt64:
      AResult.SetInteger(AValue.AsInt64);
    tkFloat:
      AResult.SetFloat(AValue.AsExtended);
    tkUString, tkLString, tkWString, tkString:
      AResult.SetString(StringRAL(AValue.AsString));
    tkEnumeration:
      if FMethod.ReturnType.Handle = TypeInfo(Boolean) then
        AResult.SetBoolean(AValue.AsBoolean)
      else
        AResult.SetString(StringRAL(GetEnumName(FMethod.ReturnType.Handle,
          AValue.AsOrdinal)));
    tkClass:
      begin
        vObj := AValue.AsObject;
        if vObj = nil then
          AResult.SetNull
        else
          AResult.SetValue(vObj as TRALJSON);
      end;
  else
    AResult.SetNull;
  end;
end;

procedure RALJSONRPCRegisterObject(ADispatcher: TRALJSONRPCDispatcher;
  AObject: TObject; const APrefix: StringRAL);
var
  vContext: TRttiContext;
  vType: TRttiType;
  vMethod: TRttiMethod;
  vParam: TRttiParameter;
  vParamType: TRALJSONRPCParamType;
  vName: StringRAL;
  vItem: TRALJSONRPCMethod;
  vBinding: TRALJSONRPCObjectBinding;
begin
  vContext := TRttiContext.Create;
  try
    vType := vContext.GetType(AObject.ClassType);
    while (vType is TRttiInstanceType) and
          not IsStopClass(TRttiInstanceType(vType).MetaclassType) do
    begin
      for vMethod in vType.GetDeclaredMethods do
      begin
        if not MethodSupported(vMethod) then
          Continue;
        if APrefix = '' then
          vName := StringRAL(vMethod.Name)
        else
          vName := APrefix + '.' + StringRAL(vMethod.Name);
        if ADispatcher.Methods.Find(vName) <> nil then
          Continue;
        vBinding := TRALJSONRPCObjectBinding.Create(ADispatcher, AObject, vMethod, vName);
        vItem := ADispatcher.Methods.AddMethod(vName, vBinding.Execute);
        for vParam in vMethod.GetParameters do
        begin
          TypeSupported(vParam.ParamType, vParamType);
          vItem.Params.AddParam(StringRAL(vParam.Name), vParamType);
        end;
        gLock.Enter;
        try
          gBindings.Add(vBinding);
        finally
          gLock.Leave;
        end;
      end;
      vType := TRttiInstanceType(vType).BaseType;
    end;
  finally
    vContext.Free;
  end;
end;

procedure RALJSONRPCUnregisterObject(ADispatcher: TRALJSONRPCDispatcher;
  AObject: TObject);
var
  vInt: Integer;
  vBinding: TRALJSONRPCObjectBinding;
  vItem: TRALJSONRPCMethod;
begin
  gLock.Enter;
  try
    for vInt := gBindings.Count - 1 downto 0 do
    begin
      vBinding := TRALJSONRPCObjectBinding(gBindings[vInt]);
      if (vBinding.Dispatcher <> ADispatcher) or (vBinding.Target <> AObject) then
        Continue;
      vItem := ADispatcher.Methods.Find(vBinding.Name);
      if (vItem <> nil) and (TMethod(vItem.OnExecute).Data = vBinding) then
        vItem.Free;
      gBindings.Delete(vInt);
      vBinding.Free;
    end;
  finally
    gLock.Leave;
  end;
end;

/// Frees every binding left.
procedure FreeBindings;
var
  vInt: Integer;
begin
  for vInt := 0 to gBindings.Count - 1 do
    TObject(gBindings[vInt]).Free;
  gBindings.Free;
  gBindings := nil;
end;

initialization
  gLock := TCriticalSection.Create;
  gBindings := TList.Create;

finalization
  FreeBindings;
  gLock.Free;
  gLock := nil;
{$ENDIF}

end.
