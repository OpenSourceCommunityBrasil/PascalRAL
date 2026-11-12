/// The gRPC server: a module that answers gRPC, gRPC-Web and gRPC-Web text on any engine.
unit RALGrpcModule;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALMIMETypes, RALTools, RALServer, RALRoutes, RALRequest, RALResponse,
  RALProtobuf, RALGrpc;

type
  TRALGrpcModule = class;

  { One call on the server: the request messages, the replies, the status and
    the metadata. A unary method reads ReadRequest and answers SendReply; a
    streaming one reads Receive until nil and answers Send per message. }
  TRALGrpcServerCall = class
  private
    /// True when the request is gzip compressed.
    FCompressed: Boolean;
    FDeadline: Int64;
    /// Index of the next request frame.
    FFrameIndex: IntegerRAL;
    /// The request frames.
    FFrames: TList;
    FMethod: StringRAL;
    FModule: TRALGrpcModule;
    FProtocol: TRALGrpcProtocol;
    /// The reply frames written so far.
    FReplies: TMemoryStream;
    /// Number of replies written.
    FReplyCount: IntegerRAL;
    FRequest: TRALRequest;
    FRequestClass: TRALProtoMessageClass;
    FResponse: TRALResponse;
    FServiceName: StringRAL;
    FStatus: IntegerRAL;
    FStatusMessage: StringRAL;
    /// True for a streaming call.
    FStreaming: Boolean;
    /// The trailers of the call.
    FTrailers: TStringList;

    /// Frees the request frames.
    procedure ClearFrames;
    /// Writes the end of the call: the trailers, in the body or as HTTP trailers.
    procedure Finish;
    /// Reads every request frame of the body.
    procedure ReadFrames;
    /// Starts the live answer (status and headers out) for Send.
    procedure StartStream;
    /// The response body form of AFrame (base64 for gRPC-Web text).
    function Wire(const AFrame: TBytes): StringRAL;
  public
    /// A call of AModule for ARequest, answered in AResponse.
    constructor Create(AModule: TRALGrpcModule; ARequest: TRALRequest;
      AResponse: TRALResponse; AProtocol: TRALGrpcProtocol);
    destructor Destroy; override;

    /// Adds a response header (metadata); before the first Send.
    procedure AddHeader(const AName, AValue: StringRAL);
    /// Adds a trailer (metadata sent with the status).
    procedure AddTrailer(const AName, AValue: StringRAL);
    /// Whether the deadline the client set (grpc-timeout) passed.
    function Expired: Boolean;
    /// Ends the call with AStatus and AMessage (no error is raised).
    procedure Fail(AStatus: IntegerRAL; const AMessage: StringRAL);
    /// A request header (metadata) by name; empty when absent.
    function Header(const AName: StringRAL): StringRAL;
    { The first request message as AClass (RequestClass when nil); the caller
      frees it. INVALID_ARGUMENT when there is none. }
    function ReadRequest(AClass: TRALProtoMessageClass = nil): TRALProtoMessage;
    { The next request message as AClass (RequestClass when nil), nil after the
      last; the caller frees it. }
    function Receive(AClass: TRALProtoMessageClass = nil): TRALProtoMessage;
    { Sends one reply message now (server streaming); False when the client
      left or the deadline passed (the call then ends with that status). }
    function Send(AMessage: TRALProtoMessage): Boolean;
    /// Sets the reply of a unary call (kept until the call ends); nil sends none.
    procedure SendReply(AMessage: TRALProtoMessage);
    /// Milliseconds until the deadline; -1 when the client set none.
    function TimeLeft: Int64;

    /// Tick (milliseconds) when the call must end; 0 when the client set no grpc-timeout.
    property Deadline: Int64 read FDeadline;
    /// The method name (the last part of the path).
    property Method: StringRAL read FMethod;
    /// The module answering the call.
    property Module: TRALGrpcModule read FModule;
    /// The wire form of the call.
    property Protocol: TRALGrpcProtocol read FProtocol;
    /// The core request (headers, client address, identity).
    property Request: TRALRequest read FRequest;
    /// The class Receive and ReadRequest create when given none.
    property RequestClass: TRALProtoMessageClass read FRequestClass write FRequestClass;
    /// The core response.
    property Response: TRALResponse read FResponse;
    /// The full service name (package.Service).
    property ServiceName: StringRAL read FServiceName;
    /// The status the call will end with.
    property Status: IntegerRAL read FStatus write FStatus;
    /// The status message.
    property StatusMessage: StringRAL read FStatusMessage write FStatusMessage;
  end;

  { A service written in code: ralprotoc makes a base class per .proto service
    whose Execute calls the methods you override. Link it with
    TRALGrpcModule.AddService. }
  TRALGrpcService = class
  public
    /// Runs the method of ACall.
    procedure Execute(ACall: TRALGrpcServerCall); virtual; abstract;
    /// The shape of AMethod; gmkUnknown when the service has no such method.
    function MethodKind(const AMethod: StringRAL): TRALGrpcMethodKind; virtual; abstract;
    /// The full name of the service (package.Service).
    function ServiceName: StringRAL; virtual; abstract;
  end;

  /// Answers one call of a method configured in the Object Inspector.
  TRALGrpcCallEvent = procedure(Sender: TObject; ACall: TRALGrpcServerCall) of object;

  /// A method answered by an event, for services without generated code.
  TRALGrpcMethod = class(TCollectionItem)
  private
    FKind: TRALGrpcMethodKind;
    FMethod: StringRAL;
    FOnCall: TRALGrpcCallEvent;
    FServiceName: StringRAL;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;

    procedure Assign(ASource: TPersistent); override;
  published
    /// The shape of the method.
    property Kind: TRALGrpcMethodKind read FKind write FKind default gmkUnary;
    /// The method name.
    property Method: StringRAL read FMethod write FMethod;
    /// Answers each call.
    property OnCall: TRALGrpcCallEvent read FOnCall write FOnCall;
    /// The full service name (package.Service).
    property ServiceName: StringRAL read FServiceName write FServiceName;
  end;

  /// The methods of a TRALGrpcModule answered by events.
  TRALGrpcMethods = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALGrpcMethod;
  public
    /// An empty list owned by AOwner.
    constructor Create(AOwner: TPersistent);

    /// Adds a method.
    function Add: TRALGrpcMethod;
    /// The method of AService and AMethod, nil when none.
    function Find(const AService, AMethod: StringRAL): TRALGrpcMethod;

    /// The methods.
    property Items[AIndex: IntegerRAL]: TRALGrpcMethod read GetItem; default;
  end;

  { Answers gRPC calls (POST /package.Service/Method under Domain) through the
    services linked with AddService and the Methods answered by events.
    gRPC-Web (binary and text) runs on every engine, over HTTP/1.1 too; native
    gRPC needs an HTTP/2 engine that sends trailers. A browser client also
    needs TRALCORSPlugin to let its origin in; the module exposes grpc-status
    and grpc-message itself. }
  TRALGrpcModule = class(TRALModuleRoutes)
  private
    FAcceptProtocols: TRALGrpcProtocols;
    /// The route that receives every call.
    FCallRoute: TRALRoute;
    /// The routes owned by the module.
    FInternal: TRALRoutes;
    FMaxMessageSize: IntegerRAL;
    FMethods: TRALGrpcMethods;
    FOnUnknownMethod: TRALGrpcCallEvent;
    /// The services the module frees.
    FOwned: TList;
    /// The services registered.
    FServices: TList;

    /// Answers a call through the internal route.
    procedure CallRequest(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure SetMethods(AValue: TRALGrpcMethods);
    /// Splits the path under Domain into service and method; False when it is not one.
    function SplitPath(const APath: StringRAL; out AService, AMethod: StringRAL): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Links AService; the module frees it when AOwns. A service of the same
      name already linked is replaced. }
    procedure AddService(AService: TRALGrpcService; AOwns: Boolean = True);
    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute; override;
    function DefaultContentType: StringRAL; override;
    /// The service linked under AName, nil when none.
    function FindService(const AName: StringRAL): TRALGrpcService;
    /// Unlinks AService (frees it if the module owned it).
    procedure RemoveService(AService: TRALGrpcService);
  published
    /// The wire forms answered; another gRPC content type is 415.
    property AcceptProtocols: TRALGrpcProtocols read FAcceptProtocols write FAcceptProtocols
      default [gpNative, gpWeb, gpWebText];
    /// The largest request message accepted (RESOURCE_EXHAUSTED above); 0 = no limit.
    property MaxMessageSize: IntegerRAL read FMaxMessageSize write FMaxMessageSize
      default cRALGrpcMaxMessage;
    /// Methods answered by events.
    property Methods: TRALGrpcMethods read FMethods write SetMethods;
    /// Answers an unknown method; UNIMPLEMENTED when unassigned.
    property OnUnknownMethod: TRALGrpcCallEvent read FOnUnknownMethod write FOnUnknownMethod;
  end;

implementation

type
  /// One request frame kept between ReadFrames and Receive.
  TRALGrpcFrame = class
  public
    /// The message bytes (decompressed).
    Data: TBytes;
  end;

{ TRALGrpcServerCall }

constructor TRALGrpcServerCall.Create(AModule: TRALGrpcModule; ARequest: TRALRequest;
  AResponse: TRALResponse; AProtocol: TRALGrpcProtocol);
var
  vTimeout: Int64;
begin
  inherited Create;
  FModule := AModule;
  FRequest := ARequest;
  FResponse := AResponse;
  FProtocol := AProtocol;
  FFrames := TList.Create;
  FReplies := TMemoryStream.Create;
  FTrailers := TStringList.Create;
  FStatus := RAL_GRPC_OK;
  FCompressed := RALSameName(ARequest.GetHeader('grpc-encoding'), 'gzip');
  vTimeout := RALGrpcTimeoutMS(ARequest.GetHeader('grpc-timeout'));
  if vTimeout >= 0 then
    FDeadline := (RALTickMicro div 1000) + vTimeout;
end;

destructor TRALGrpcServerCall.Destroy;
begin
  ClearFrames;
  FFrames.Free;
  FReplies.Free;
  FTrailers.Free;
  inherited Destroy;
end;

procedure TRALGrpcServerCall.AddHeader(const AName, AValue: StringRAL);
begin
  FResponse.AddHeader(AName, AValue);
end;

procedure TRALGrpcServerCall.AddTrailer(const AName, AValue: StringRAL);
begin
  FTrailers.Add(string(AName) + '=' + string(AValue));
end;

procedure TRALGrpcServerCall.ClearFrames;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FFrames.Count - 1 do
    TObject(FFrames[vInt]).Free;
  FFrames.Clear;
end;

function TRALGrpcServerCall.Expired: Boolean;
begin
  Result := (FDeadline > 0) and ((RALTickMicro div 1000) >= FDeadline);
end;

procedure TRALGrpcServerCall.Fail(AStatus: IntegerRAL; const AMessage: StringRAL);
begin
  FStatus := AStatus;
  FStatusMessage := AMessage;
end;

procedure TRALGrpcServerCall.Finish;
var
  vText: StringRAL;
  vInt: IntegerRAL;
begin
  if FProtocol = gpNative then
  begin
    // HTTP/2: the status goes as trailers; with no message, in the headers
    // (the "trailers-only" answer every client reads, whatever the engine)
    if FReplyCount = 0 then
    begin
      FResponse.AddHeader('grpc-status', StringRAL(IntToStr(FStatus)));
      if FStatusMessage <> '' then
        FResponse.AddHeader('grpc-message', RALGrpcEncodeMessage(FStatusMessage));
    end
    else
    begin
      FResponse.AddTrailer('grpc-status', StringRAL(IntToStr(FStatus)));
      if FStatusMessage <> '' then
        FResponse.AddTrailer('grpc-message', RALGrpcEncodeMessage(FStatusMessage));
    end;
    for vInt := 0 to FTrailers.Count - 1 do
      if FReplyCount = 0 then
        FResponse.AddHeader(StringRAL(FTrailers.Names[vInt]), StringRAL(FTrailers.ValueFromIndex[vInt]))
      else
        FResponse.AddTrailer(StringRAL(FTrailers.Names[vInt]), StringRAL(FTrailers.ValueFromIndex[vInt]));
    if FStreaming then
      FResponse.EndStream
    else
    begin
      FReplies.Position := 0;
      FResponse.Answer(HTTP_OK, FReplies, RALGrpcContentType(FProtocol));
      // an empty body loses its type, which a trailers-only answer must keep
      if FResponse.ContentType = '' then
        FResponse.AddHeader('content-type', RALGrpcContentType(FProtocol));
    end;
    Exit;
  end;
  // gRPC-Web: the trailers are the last frame of the body
  vText := Wire(RALGrpcTrailerFrame(FStatus, FStatusMessage, FTrailers));
  if FStreaming then
  begin
    FResponse.Send(vText);
    FResponse.EndStream;
    Exit;
  end;
  if FReplyCount = 0 then
  begin
    // trailers-only: also in the headers, for clients that look there first
    FResponse.AddHeader('grpc-status', StringRAL(IntToStr(FStatus)));
    if FStatusMessage <> '' then
      FResponse.AddHeader('grpc-message', RALGrpcEncodeMessage(FStatusMessage));
  end;
  if Length(vText) > 0 then
    FReplies.WriteBuffer(vText[1], Length(vText));
  FReplies.Position := 0;
  FResponse.Answer(HTTP_OK, FReplies, RALGrpcContentType(FProtocol));
end;

function TRALGrpcServerCall.Header(const AName: StringRAL): StringRAL;
begin
  Result := FRequest.GetHeader(AName);
end;

procedure TRALGrpcServerCall.ReadFrames;
var
  vReader: TRALGrpcFrameReader;
  vStream: TStream;
  vBuffer: array[0..16383] of Byte;
  vRead: IntegerRAL;
  vFlags: Byte;
  vData: TBytes;
  vFrame: TRALGrpcFrame;
begin
  vReader := TRALGrpcFrameReader.Create(FProtocol = gpWebText, FModule.MaxMessageSize);
  try
    vStream := FRequest.RequestStream;
    if vStream <> nil then
    begin
      vStream.Position := 0;
      repeat
        vRead := vStream.Read(vBuffer, SizeOf(vBuffer));
        if vRead > 0 then
          vReader.Feed(vBuffer, vRead);
      until vRead <= 0;
    end;
    while vReader.Next(vFlags, vData) do
    begin
      if (vFlags and cRALGrpcFlagTrailers) <> 0 then
        Continue;
      if (vFlags and cRALGrpcFlagCompressed) <> 0 then
      begin
        if not FCompressed then
          raise ERALGrpcError.CreateStatus(RAL_GRPC_INTERNAL,
            'compressed message without grpc-encoding');
        vData := RALGrpcGunzip(vData);
      end;
      vFrame := TRALGrpcFrame.Create;
      vFrame.Data := vData;
      FFrames.Add(vFrame);
    end;
    if vReader.Pending then
      raise ERALGrpcError.CreateStatus(RAL_GRPC_INTERNAL, 'request body ends inside a frame');
  finally
    vReader.Free;
  end;
end;

function TRALGrpcServerCall.ReadRequest(AClass: TRALProtoMessageClass): TRALProtoMessage;
begin
  FFrameIndex := 0;
  Result := Receive(AClass);
  if Result = nil then
    raise ERALGrpcError.CreateStatus(RAL_GRPC_INVALID_ARGUMENT, 'the call has no request message');
end;

function TRALGrpcServerCall.Receive(AClass: TRALProtoMessageClass): TRALProtoMessage;
begin
  Result := nil;
  if FFrameIndex >= FFrames.Count then
    Exit;
  if AClass = nil then
    AClass := FRequestClass;
  if AClass = nil then
    raise ERALGrpcError.CreateStatus(RAL_GRPC_INTERNAL, 'no request class');
  Result := AClass.Create;
  try
    Result.Decode(TRALGrpcFrame(FFrames[FFrameIndex]).Data);
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      raise ERALGrpcError.CreateStatus(RAL_GRPC_INTERNAL, StringRAL(E.Message));
    end;
  end;
  Inc(FFrameIndex);
end;

function TRALGrpcServerCall.Send(AMessage: TRALProtoMessage): Boolean;
var
  vText: StringRAL;
begin
  Result := False;
  if FStatus <> RAL_GRPC_OK then
    Exit;
  if Expired then
  begin
    Fail(RAL_GRPC_DEADLINE_EXCEEDED, 'deadline exceeded');
    Exit;
  end;
  if not FStreaming then
    StartStream;
  vText := Wire(RALGrpcMessageFrame(AMessage));
  Inc(FReplyCount);
  Result := FResponse.Send(vText);
  if not Result then
    Fail(RAL_GRPC_CANCELLED, 'the client left');
end;

procedure TRALGrpcServerCall.SendReply(AMessage: TRALProtoMessage);
var
  vText: StringRAL;
begin
  if AMessage = nil then
    Exit;
  if FStreaming then
  begin
    Send(AMessage);
    Exit;
  end;
  vText := Wire(RALGrpcMessageFrame(AMessage));
  if Length(vText) > 0 then
    FReplies.WriteBuffer(vText[1], Length(vText));
  Inc(FReplyCount);
end;

procedure TRALGrpcServerCall.StartStream;
begin
  FStreaming := True;
  FResponse.BeginStream(HTTP_OK, RALGrpcContentType(FProtocol));
  // what SendReply kept goes first
  if FReplies.Size > 0 then
    FResponse.Send(FReplies.Memory^, FReplies.Size);
  FReplies.Clear;
end;

function TRALGrpcServerCall.TimeLeft: Int64;
begin
  if FDeadline = 0 then
    Result := -1
  else
  begin
    Result := FDeadline - (RALTickMicro div 1000);
    if Result < 0 then
      Result := 0;
  end;
end;

function TRALGrpcServerCall.Wire(const AFrame: TBytes): StringRAL;
begin
  if FProtocol = gpWebText then
    Result := RALGrpcBase64(AFrame)
  else
  begin
    SetLength(Result, Length(AFrame));
    if Length(AFrame) > 0 then
      Move(AFrame[0], Result[1], Length(AFrame));
  end;
end;

{ TRALGrpcMethod }

constructor TRALGrpcMethod.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FKind := gmkUnary;
end;

procedure TRALGrpcMethod.Assign(ASource: TPersistent);
begin
  if ASource is TRALGrpcMethod then
  begin
    FKind := TRALGrpcMethod(ASource).Kind;
    FMethod := TRALGrpcMethod(ASource).Method;
    FServiceName := TRALGrpcMethod(ASource).ServiceName;
    FOnCall := TRALGrpcMethod(ASource).OnCall;
  end
  else
    inherited Assign(ASource);
end;

function TRALGrpcMethod.GetDisplayName: string;
begin
  Result := string(FServiceName) + '/' + string(FMethod);
  if Result = '/' then
    Result := inherited GetDisplayName;
end;

{ TRALGrpcMethods }

constructor TRALGrpcMethods.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALGrpcMethod);
end;

function TRALGrpcMethods.Add: TRALGrpcMethod;
begin
  Result := TRALGrpcMethod(inherited Add);
end;

function TRALGrpcMethods.Find(const AService, AMethod: StringRAL): TRALGrpcMethod;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
  begin
    Result := Items[vInt];
    if (Result.ServiceName = AService) and (Result.Method = AMethod) then
      Exit;
  end;
  Result := nil;
end;

function TRALGrpcMethods.GetItem(AIndex: IntegerRAL): TRALGrpcMethod;
begin
  Result := TRALGrpcMethod(inherited Items[AIndex]);
end;

{ TRALGrpcModule }

constructor TRALGrpcModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptProtocols := [gpNative, gpWeb, gpWebText];
  FMaxMessageSize := cRALGrpcMaxMessage;
  FMethods := TRALGrpcMethods.Create(Self);
  FServices := TList.Create;
  FOwned := TList.Create;
  // the route every call answers through, out of Routes
  FInternal := TRALRoutes.Create(Self, TRALRoute);
  FCallRoute := TRALRoute(FInternal.Add);
  FCallRoute.Route := '';
  FCallRoute.AllowedMethods := [amPOST];
  FCallRoute.OnReply := CallRequest;
end;

destructor TRALGrpcModule.Destroy;
begin
  while FServices.Count > 0 do
    RemoveService(TRALGrpcService(FServices[0]));
  FServices.Free;
  FOwned.Free;
  FInternal.Free;
  FMethods.Free;
  inherited Destroy;
end;

procedure TRALGrpcModule.AddService(AService: TRALGrpcService; AOwns: Boolean);
var
  vOld: TRALGrpcService;
begin
  vOld := FindService(AService.ServiceName);
  if (vOld <> nil) and (vOld <> AService) then
    RemoveService(vOld);
  if FServices.IndexOf(AService) < 0 then
    FServices.Add(AService);
  if AOwns and (FOwned.IndexOf(AService) < 0) then
    FOwned.Add(AService);
end;

procedure TRALGrpcModule.CallRequest(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vProtocol: TRALGrpcProtocol;
  vService, vMethodName: StringRAL;
  vCall: TRALGrpcServerCall;
  vImpl: TRALGrpcService;
  vMethod: TRALGrpcMethod;
begin
  if not RALGrpcProtocolOf(ARequest.ContentType, vProtocol) or
     not (vProtocol in FAcceptProtocols) then
  begin
    AResponse.Answer(HTTP_UnsupportedMedia, '', rctTEXTPLAIN);
    Exit;
  end;
  SplitPath(ARequest.Query, vService, vMethodName);
  { a browser reads the status headers of a trailers-only gRPC-Web answer only
    when CORS exposes them; set before the handler can commit the answer }
  if vProtocol <> gpNative then
    AResponse.AddHeader('Access-Control-Expose-Headers', 'grpc-status, grpc-message');
  vCall := TRALGrpcServerCall.Create(Self, ARequest, AResponse, vProtocol);
  try
    vCall.FServiceName := vService;
    vCall.FMethod := vMethodName;
    try
      vCall.ReadFrames;
      vImpl := FindService(vService);
      vMethod := nil;
      if (vImpl <> nil) and (vImpl.MethodKind(vMethodName) = gmkUnknown) then
        vImpl := nil;
      if vImpl = nil then
        vMethod := FMethods.Find(vService, vMethodName);
      if vImpl <> nil then
        vImpl.Execute(vCall)
      else if (vMethod <> nil) and Assigned(vMethod.OnCall) then
        vMethod.OnCall(Self, vCall)
      else if Assigned(FOnUnknownMethod) then
        FOnUnknownMethod(Self, vCall)
      else
        vCall.Fail(RAL_GRPC_UNIMPLEMENTED, 'unknown method ' + vService + '/' + vMethodName);
    except
      on E: ERALGrpcError do
        vCall.Fail(E.Status, E.StatusMessage);
      on E: Exception do
        vCall.Fail(RAL_GRPC_UNKNOWN, ErrorText(E));
    end;
    if (vCall.Status = RAL_GRPC_OK) and vCall.Expired then
      vCall.Fail(RAL_GRPC_DEADLINE_EXCEEDED, 'deadline exceeded');
    vCall.Finish;
  finally
    vCall.Free;
  end;
end;

function TRALGrpcModule.CanAnswerRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
var
  vProtocol: TRALGrpcProtocol;
  vService, vMethod: StringRAL;
begin
  // the module's own routes first; then any POST of a gRPC content type
  Result := inherited CanAnswerRoute(ARequest, AResponse);
  if (Result = nil) and (ARequest.Method in [amPOST, amOPTIONS]) and
     SplitPath(ARequest.Query, vService, vMethod) and
     ((ARequest.Method = amOPTIONS) or RALGrpcProtocolOf(ARequest.ContentType, vProtocol)) then
    Result := FCallRoute;
end;

function TRALGrpcModule.DefaultContentType: StringRAL;
begin
  Result := RALGrpcContentType(gpWeb);
end;

function TRALGrpcModule.FindService(const AName: StringRAL): TRALGrpcService;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FServices.Count - 1 do
  begin
    Result := TRALGrpcService(FServices[vInt]);
    if Result.ServiceName = AName then
      Exit;
  end;
  Result := nil;
end;

procedure TRALGrpcModule.RemoveService(AService: TRALGrpcService);
var
  vOwned: Boolean;
begin
  if FServices.IndexOf(AService) < 0 then
    Exit;
  FServices.Remove(AService);
  vOwned := FOwned.IndexOf(AService) >= 0;
  FOwned.Remove(AService);
  if vOwned then
    AService.Free;
end;

procedure TRALGrpcModule.SetMethods(AValue: TRALGrpcMethods);
begin
  FMethods.Assign(AValue);
end;

function TRALGrpcModule.SplitPath(const APath: StringRAL; out AService,
  AMethod: StringRAL): Boolean;
var
  vPath: StringRAL;
  vPos: IntegerRAL;
begin
  AService := '';
  AMethod := '';
  vPath := APath;
  if (Domain <> '') and (Domain <> '/') then
  begin
    if Pos(string(Domain), string(vPath)) <> 1 then
    begin
      Result := False;
      Exit;
    end;
    vPath := Copy(vPath, Length(Domain) + 1, MaxInt);
  end;
  if (vPath <> '') and (vPath[1] = '/') then
    Delete(vPath, 1, 1);
  vPos := Pos('/', string(vPath));
  Result := (vPos > 1) and (vPos < Length(vPath)) and
    (Pos('/', string(Copy(vPath, vPos + 1, MaxInt))) = 0);
  if Result then
  begin
    AService := Copy(vPath, 1, vPos - 1);
    AMethod := Copy(vPath, vPos + 1, MaxInt);
  end;
end;

end.
