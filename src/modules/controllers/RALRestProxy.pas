/// The typed client by interface: an interface (descending from IInvokable)
/// whose methods carry the same attributes as a controller ([GET('/x/{id}')],
/// [FromQuery], [FromHeader], [FromBody]) becomes a client of the API, built at
/// run time with TVirtualInterface. Only where the compiler has attributes and
/// TVirtualInterface (Delphi XE2 and later); compiles empty elsewhere.
unit RALRestProxy;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

{$IF Defined(RALHasAttributes) and Defined(RALHasVirtualInterface) and Defined(RALHasExtRTTI)}
uses
  Classes, SysUtils, TypInfo, Rtti,
  RALTypes, RALConsts, RALClient, RALResponse, RALJson, RALObjectJSON, RALObjectRTTI,
  RALControllers;

type
  /// An answer of 400 or more, or no answer at all, to a proxy call.
  ERALRestProxy = class(Exception)
  private
    FBody: string;
    FStatus: Integer;
  public
    /// An error for the HTTP AStatus answered with ABody.
    constructor CreateStatus(AStatus: Integer; const ABody: string);

    /// The body of the answer (the problem+json of an error).
    property Body: string read FBody;
    /// The status; 0 when no answer came.
    property Status: Integer read FStatus;
  end;

  /// The object behind a proxy: every call of the interface becomes a request.
  TRALProxyInvoker = class(TVirtualInterface)
  private
    /// Client that sends the calls.
    FClient: TRALClient;

    /// Turns a call of the interface into an HTTP request and its answer into Result.
    procedure DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
  public
    /// An invoker of the interface PIID over AClient.
    constructor Create(PIID: PTypeInfo; AClient: TRALClient);
  end;

  { Builds clients of an API described by an interface: TRALRestProxy.Make<I>
    (Client) returns an I whose every call is an HTTP request through Client
    (not owned; it must outlive the proxy). }
  TRALRestProxy = class
  public
    /// A client of the interface I.
    class function Make<I: IInvokable>(AClient: TRALClient): I;
  end;
{$IFEND}

implementation

{$IF Defined(RALHasAttributes) and Defined(RALHasVirtualInterface) and Defined(RALHasExtRTTI)}
constructor ERALRestProxy.CreateStatus(AStatus: Integer; const ABody: string);
begin
  if AStatus = 0 then
    inherited Create('No answer')
  else
    inherited CreateFmt('HTTP %d: %s', [AStatus, Copy(ABody, 1, 300)]);
  FStatus := AStatus;
  FBody := ABody;
end;

/// AText encoded for a URL path segment or query value.
function URLEncode(const AText: string): string;
const
  cHex = '0123456789ABCDEF';
var
  vUTF8: UTF8String;
  vInt: Integer;
  vChar: AnsiChar;
begin
  vUTF8 := UTF8Encode(AText);
  Result := '';
  for vInt := 1 to Length(vUTF8) do
  begin
    vChar := vUTF8[vInt];
    if vChar in ['A'..'Z', 'a'..'z', '0'..'9', '-', '.', '_', '~'] then
      Result := Result + string(vChar)
    else
      Result := Result + '%' + cHex[(Ord(vChar) shr 4) + 1] + cHex[(Ord(vChar) and 15) + 1];
  end;
end;

/// AValue as the text sent in a URL.
function ValueText(const AValue: TValue): string;
var
  vFmt: TFormatSettings;
begin
  case AValue.Kind of
    tkFloat:
      if (AValue.TypeInfo = TypeInfo(TDateTime)) or (AValue.TypeInfo = TypeInfo(TDate)) then
      begin
        Result := string(TRALObjectRTTI.ValueToText(AValue));
        Result := StringReplace(Result, '"', '', [rfReplaceAll]);
      end
      else
      begin
        vFmt := FormatSettings;
        vFmt.DecimalSeparator := '.';
        Result := FloatToStr(AValue.AsExtended, vFmt);
      end;
    tkEnumeration:
      if AValue.TypeInfo = TypeInfo(Boolean) then
        Result := LowerCase(BoolToStr(AValue.AsBoolean, True))
      else
        Result := GetEnumName(AValue.TypeInfo, AValue.AsOrdinal);
  else
    Result := AValue.ToString;
  end;
end;

constructor TRALProxyInvoker.Create(PIID: PTypeInfo; AClient: TRALClient);
begin
  inherited Create(PIID, DoInvoke);
  FClient := AClient;
end;

procedure TRALProxyInvoker.DoInvoke(Method: TRttiMethod; const Args: TArray<TValue>;
  out Result: TValue);
var
  vAttr, vSource: TCustomAttribute;
  vVerb: TRALMethod;
  vPath, vQuery, vName, vBody, vText: string;
  vParams: TArray<TRttiParameter>;
  vInt: Integer;
  vResponse: TRALResponse;
  vStatus: Integer;
  vJSON: TRALJSONValue;
begin
  vVerb := amGET;
  vPath := '';
  for vAttr in Method.GetAttributes do
    if vAttr is TRALHTTPMethodAttribute then
    begin
      vVerb := TRALHTTPMethodAttribute(vAttr).Method;
      vPath := TRALHTTPMethodAttribute(vAttr).Path;
    end
    else if vAttr is PathAttribute then
      vPath := PathAttribute(vAttr).Path;
  vQuery := '';
  vBody := '';
  FClient.Request.Clear;
  FClient.Request.AddHeader('Accept', 'application/json');
  vParams := Method.GetParameters;
  for vInt := 0 to High(vParams) do
  begin
    vSource := nil;
    for vAttr in vParams[vInt].GetAttributes do
      if (vAttr is TRALParamSourceAttribute) or (vAttr is FromBodyAttribute) then
        vSource := vAttr;
    vName := vParams[vInt].Name;
    if (vSource is TRALParamSourceAttribute) and (TRALParamSourceAttribute(vSource).Name <> '') then
      vName := TRALParamSourceAttribute(vSource).Name;
    { Args[0] is the interface itself }
    if (vSource is FromBodyAttribute) or ((vSource = nil) and
       (vParams[vInt].ParamType.TypeKind in [tkClass, tkRecord, tkDynArray])) then
    begin
      if Args[vInt + 1].IsObject then
        vBody := string(TRALObjectJSON.ToText(Args[vInt + 1].AsObject))
      else
        vBody := string(TRALObjectRTTI.ValueToText(Args[vInt + 1]));
    end
    else if vSource is FromHeaderAttribute then
      FClient.Request.AddHeader(StringRAL(vName), StringRAL(ValueText(Args[vInt + 1])))
    else if (vSource is FromPathAttribute) or (Pos('{' + vName + '}', vPath) > 0) then
      vPath := StringReplace(vPath, '{' + vName + '}', URLEncode(ValueText(Args[vInt + 1])),
        [rfReplaceAll, rfIgnoreCase])
    else
    begin
      if vQuery <> '' then
        vQuery := vQuery + '&';
      vQuery := vQuery + URLEncode(vName) + '=' + URLEncode(ValueText(Args[vInt + 1]));
    end;
  end;
  if vQuery <> '' then
    vPath := vPath + '?' + vQuery;
  if vBody <> '' then
    FClient.Request.AddBody(UTF8Encode(vBody), 'application/json');

  vResponse := nil;
  try
    FClient.Execute(StringRAL(vPath), vVerb, vResponse);
    if vResponse = nil then
      raise ERALRestProxy.CreateStatus(0, '');
    vStatus := vResponse.StatusCode;
    vText := UTF8ToString(vResponse.ResponseText);
    if (vStatus = 0) or (vStatus >= 400) then
      raise ERALRestProxy.CreateStatus(vStatus, vText);
    if Method.ReturnType = nil then
      Exit;
    if (Method.ReturnType.TypeKind in [tkString, tkLString, tkWString, tkUString]) and
       ((vText = '') or (vText[1] <> '"')) then
    begin
      Result := TValue.From<string>(vText).Cast(Method.ReturnType.Handle);
      Exit;
    end;
    vJSON := TRALJSON.ParseJSON(vResponse.ResponseText);
    try
      Result := TRALObjectRTTI.ValueFromJSON(vJSON, Method.ReturnType.Handle);
    finally
      vJSON.Free;
    end;
  finally
    vResponse.Free;
  end;
end;

class function TRALRestProxy.Make<I>(AClient: TRALClient): I;
var
  vInvoker: TRALProxyInvoker;
begin
  vInvoker := TRALProxyInvoker.Create(TypeInfo(I), AClient);
  if not Supports(vInvoker, GetTypeData(TypeInfo(I))^.Guid, Result) then
  begin
    vInvoker.Free;
    raise Exception.CreateFmt('%s has no GUID', [string(PTypeInfo(TypeInfo(I))^.Name)]);
  end;
end;
{$IFEND}

end.
