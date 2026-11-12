/// The gRPC client: calls through a TRALClient, as gRPC-Web on any engine or native
/// gRPC over HTTP/2.
unit RALGrpcClient;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools, RALCustomObjects, RALClient, RALResponse,
  RALProtobuf, RALGrpc;

type
  { Receives each reply of a streaming call as it arrives, on the calling
    thread; the message is freed after the event. ACancel ends the call
    (CANCELLED). }
  TRALGrpcMessageEvent = procedure(ASender: TObject; AMessage: TRALProtoMessage;
    var ACancel: Boolean) of object;

  { Calls gRPC methods through Client (its engine, BaseURL, TLS and
    authentication). ralprotoc writes a descendant per .proto service with a
    typed method per rpc. gRPC-Web (the default) works over any engine and
    HTTP/1.1; gpNative needs an engine that reads HTTP/2 trailers. Client and
    server streams are sent whole and read as they come (half duplex). }
  TRALGrpcClient = class(TRALComponent)
  private
    FClient: TRALClient;
    FCompress: Boolean;
    FLastMetadata: TStringList;
    FLastStatus: IntegerRAL;
    FLastStatusMessage: StringRAL;
    FMaxMessageSize: IntegerRAL;
    FMetadata: TStrings;
    FPrefix: StringRAL;
    FProtocol: TRALGrpcProtocol;
    FTimeout: IntegerRAL;

    procedure SetClient(AValue: TRALClient);
    procedure SetMetadata(AValue: TStrings);
  protected
    { Sends the frames of ARequests to APath and reads the replies as
      AReplyClass: each to AOnMessage when assigned, else the first kept in
      AReply. Raises ERALGrpcError for a status other than OK. }
    procedure Invoke(const APath: StringRAL; const ARequests: array of TRALProtoMessage;
      AReplyClass: TRALProtoMessageClass; AOnMessage: TRALGrpcMessageEvent;
      out AReply: TRALProtoMessage);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// A half duplex bidirectional stream: sends ARequests, replies go to AOnMessage.
    procedure CallBidiStream(const APath: StringRAL; ARequests: TRALProtoMessageList;
      AReplyClass: TRALProtoMessageClass; AOnMessage: TRALGrpcMessageEvent);
    /// A client stream: sends ARequests and returns the reply, which the caller frees.
    function CallClientStream(const APath: StringRAL; ARequests: TRALProtoMessageList;
      AReplyClass: TRALProtoMessageClass): TRALProtoMessage;
    /// A server stream: sends ARequest, each reply goes to AOnMessage as it arrives.
    procedure CallServerStream(const APath: StringRAL; ARequest: TRALProtoMessage;
      AReplyClass: TRALProtoMessageClass; AOnMessage: TRALGrpcMessageEvent);
    /// A unary call to APath (/package.Service/Method); the caller frees the reply.
    function CallUnary(const APath: StringRAL; ARequest: TRALProtoMessage;
      AReplyClass: TRALProtoMessageClass): TRALProtoMessage;

    { The response headers and trailers of the last call on this object
      (name=value); not for several threads at once. }
    property LastMetadata: TStringList read FLastMetadata;
    /// The status of the last call on this object.
    property LastStatus: IntegerRAL read FLastStatus;
    /// The status message of the last call on this object.
    property LastStatusMessage: StringRAL read FLastStatusMessage;
  published
    /// The TRALClient that carries the calls.
    property Client: TRALClient read FClient write SetClient;
    /// Compresses requests with gzip (grpc-encoding); needs RALCompressZLib linked.
    property Compress: Boolean read FCompress write FCompress default False;
    /// The largest reply message accepted; 0 = no limit.
    property MaxMessageSize: IntegerRAL read FMaxMessageSize write FMaxMessageSize
      default cRALGrpcMaxMessage;
    /// Headers sent with every call (name=value).
    property Metadata: TStrings read FMetadata write SetMetadata;
    /// A path put before /package.Service/Method (the module's Domain).
    property Prefix: StringRAL read FPrefix write FPrefix;
    /// The wire form of the calls.
    property Protocol: TRALGrpcProtocol read FProtocol write FProtocol default gpWeb;
    /// The deadline of each call in milliseconds (grpc-timeout); 0 = none.
    property Timeout: IntegerRAL read FTimeout write FTimeout default 0;
  end;

implementation

type
  { The state of one call while its answer is read: the frame reader and
    what to do with each message. }
  TRALGrpcCallReader = class
  private
    /// True after the caller cancelled the call.
    FCancelled: Boolean;
    /// Whether OnData received any piece.
    FFed: Boolean;
    /// Called for each reply message.
    FOnMessage: TRALGrpcMessageEvent;
    /// The client that made the call.
    FOwner: TRALGrpcClient;
    /// Splits the response into frames.
    FReader: TRALGrpcFrameReader;
    /// The reply being filled.
    FReply: TRALProtoMessage;
    /// Class of the reply messages.
    FReplyClass: TRALProtoMessageClass;
    /// The trailers of the call.
    FTrailers: TStringList;
    /// True once the trailers were read.
    FTrailersRead: Boolean;
  public
    /// Creates the writer for a file of a schema.
    constructor Create(AOwner: TRALGrpcClient; AText: Boolean;
      AReplyClass: TRALProtoMessageClass; AOnMessage: TRALGrpcMessageEvent);
    destructor Destroy; override;

    /// Takes the frames fed so far.
    procedure Drain(var ACancel: Boolean);
    /// The OnReceiveData of the call.
    procedure OnData(ASender: TObject; AResponse: TRALResponse; const ABuffer;
      ASize: IntegerRAL; var ACancel: Boolean);
  end;

{ TRALGrpcCallReader }

constructor TRALGrpcCallReader.Create(AOwner: TRALGrpcClient; AText: Boolean;
  AReplyClass: TRALProtoMessageClass; AOnMessage: TRALGrpcMessageEvent);
begin
  inherited Create;
  FOwner := AOwner;
  FReader := TRALGrpcFrameReader.Create(AText, AOwner.MaxMessageSize);
  FReplyClass := AReplyClass;
  FOnMessage := AOnMessage;
  FTrailers := TStringList.Create;
end;

destructor TRALGrpcCallReader.Destroy;
begin
  FReader.Free;
  FReply.Free;
  FTrailers.Free;
  inherited Destroy;
end;

procedure TRALGrpcCallReader.Drain(var ACancel: Boolean);
var
  vFlags: Byte;
  vData: TBytes;
  vMessage: TRALProtoMessage;
begin
  while not ACancel and FReader.Next(vFlags, vData) do
  begin
    if (vFlags and cRALGrpcFlagTrailers) <> 0 then
    begin
      RALGrpcParseTrailers(vData, FTrailers);
      FTrailersRead := True;
      Continue;
    end;
    if (vFlags and cRALGrpcFlagCompressed) <> 0 then
      vData := RALGrpcGunzip(vData);
    vMessage := FReplyClass.Create;
    try
      vMessage.Decode(vData);
    except
      vMessage.Free;
      raise;
    end;
    if Assigned(FOnMessage) then
    begin
      try
        FOnMessage(FOwner, vMessage, ACancel);
      finally
        vMessage.Free;
      end;
      if ACancel then
        FCancelled := True;
    end
    else if FReply = nil then
      FReply := vMessage
    else
      vMessage.Free;
  end;
end;

procedure TRALGrpcCallReader.OnData(ASender: TObject; AResponse: TRALResponse;
  const ABuffer; ASize: IntegerRAL; var ACancel: Boolean);
begin
  FFed := True;
  FReader.Feed(ABuffer, ASize);
  Drain(ACancel);
end;

{ TRALGrpcClient }

constructor TRALGrpcClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProtocol := gpWeb;
  FMaxMessageSize := cRALGrpcMaxMessage;
  FMetadata := TStringList.Create;
  FLastMetadata := TStringList.Create;
end;

destructor TRALGrpcClient.Destroy;
begin
  FMetadata.Free;
  FLastMetadata.Free;
  inherited Destroy;
end;

procedure TRALGrpcClient.CallBidiStream(const APath: StringRAL;
  ARequests: TRALProtoMessageList; AReplyClass: TRALProtoMessageClass;
  AOnMessage: TRALGrpcMessageEvent);
var
  vRequests: array of TRALProtoMessage;
  vInt: IntegerRAL;
  vReply: TRALProtoMessage;
begin
  SetLength(vRequests, ARequests.Count);
  for vInt := 0 to ARequests.Count - 1 do
    vRequests[vInt] := ARequests.Messages[vInt];
  Invoke(APath, vRequests, AReplyClass, AOnMessage, vReply);
  vReply.Free;
end;

function TRALGrpcClient.CallClientStream(const APath: StringRAL;
  ARequests: TRALProtoMessageList; AReplyClass: TRALProtoMessageClass): TRALProtoMessage;
var
  vRequests: array of TRALProtoMessage;
  vInt: IntegerRAL;
begin
  SetLength(vRequests, ARequests.Count);
  for vInt := 0 to ARequests.Count - 1 do
    vRequests[vInt] := ARequests.Messages[vInt];
  Invoke(APath, vRequests, AReplyClass, nil, Result);
end;

procedure TRALGrpcClient.CallServerStream(const APath: StringRAL;
  ARequest: TRALProtoMessage; AReplyClass: TRALProtoMessageClass;
  AOnMessage: TRALGrpcMessageEvent);
var
  vReply: TRALProtoMessage;
begin
  Invoke(APath, [ARequest], AReplyClass, AOnMessage, vReply);
  vReply.Free;
end;

function TRALGrpcClient.CallUnary(const APath: StringRAL; ARequest: TRALProtoMessage;
  AReplyClass: TRALProtoMessageClass): TRALProtoMessage;
begin
  Invoke(APath, [ARequest], AReplyClass, nil, Result);
end;

procedure TRALGrpcClient.Invoke(const APath: StringRAL;
  const ARequests: array of TRALProtoMessage; AReplyClass: TRALProtoMessageClass;
  AOnMessage: TRALGrpcMessageEvent; out AReply: TRALProtoMessage);
var
  vBody, vFrame: TBytes;
  vText: StringRAL;
  vInt, vStatus: IntegerRAL;
  vCall: TRALGrpcCallReader;
  vResponse: TRALResponse;
  vProtocol: TRALGrpcProtocol;
  vCancel: Boolean;
  vStatusText: StringRAL;
begin
  AReply := nil;
  if FClient = nil then
    raise ERALGrpcError.CreateStatus(RAL_GRPC_INTERNAL, 'TRALGrpcClient.Client is not set');
  FLastMetadata.Clear;
  FLastStatus := RAL_GRPC_UNKNOWN;
  FLastStatusMessage := '';
  vBody := nil;
  for vInt := 0 to High(ARequests) do
  begin
    vFrame := RALGrpcMessageFrame(ARequests[vInt], FCompress);
    SetLength(vBody, Length(vBody) + Length(vFrame));
    Move(vFrame[0], vBody[Length(vBody) - Length(vFrame)], Length(vFrame));
  end;
  if FProtocol = gpWebText then
    vText := RALGrpcBase64(vBody)
  else
  begin
    SetLength(vText, Length(vBody));
    if Length(vBody) > 0 then
      Move(vBody[0], vText[1], Length(vBody));
  end;
  FClient.Request.Clear;
  FClient.Request.AddBody(vText, RALGrpcContentType(FProtocol));
  if FProtocol = gpNative then
    FClient.Request.AddHeader('te', 'trailers')
  else
  begin
    FClient.Request.AddHeader('x-grpc-web', '1');
    FClient.Request.AddHeader('accept', RALGrpcContentType(FProtocol));
  end;
  if FCompress then
    FClient.Request.AddHeader('grpc-encoding', 'gzip');
  FClient.Request.AddHeader('grpc-accept-encoding', 'gzip');
  if FTimeout > 0 then
    FClient.Request.AddHeader('grpc-timeout', RALGrpcTimeoutText(FTimeout));
  for vInt := 0 to FMetadata.Count - 1 do
    FClient.Request.AddHeader(StringRAL(FMetadata.Names[vInt]),
      StringRAL(FMetadata.ValueFromIndex[vInt]));
  vCall := TRALGrpcCallReader.Create(Self, FProtocol = gpWebText, AReplyClass, AOnMessage);
  try
    vResponse := nil;
    try
      try
        FClient.Post(FPrefix + APath, vCall.OnData, vResponse);
      except
        on E: ERALGrpcError do
          raise;
        on E: Exception do
        begin
          // the call raises for a transport failure or a cancel
          if vCall.FCancelled then
            FLastStatus := RAL_GRPC_CANCELLED
          else if (FTimeout > 0) and (Pos('imeout', E.Message) > 0) then
            FLastStatus := RAL_GRPC_DEADLINE_EXCEEDED
          else
            FLastStatus := RAL_GRPC_UNAVAILABLE;
          FLastStatusMessage := StringRAL(E.Message);
          raise ERALGrpcError.CreateStatus(FLastStatus, FLastStatusMessage);
        end;
      end;
      // an answer the engine did not hand over in pieces (not 2xx, or buffered)
      if not vCall.FFed and (vResponse.StatusCode = HTTP_OK) and (vResponse.ResponseStream <> nil) and
         (vResponse.ResponseStream.Size > 0) then
      begin
        vText := vResponse.ResponseText;
        if Length(vText) > 0 then
          vCall.FReader.Feed(vText[1], Length(vText));
      end;
      vCancel := vCall.FCancelled;
      vCall.Drain(vCancel);
      if vResponse.ErrorCode <> 0 then
      begin
        if vCall.FCancelled then
          vStatus := RAL_GRPC_CANCELLED
        else if vResponse.TransportError = rteTimeout then
          vStatus := RAL_GRPC_DEADLINE_EXCEEDED
        else
          vStatus := RAL_GRPC_UNAVAILABLE;
        FLastStatus := vStatus;
        FLastStatusMessage := StringRAL('transport error ' + IntToStr(vResponse.ErrorCode));
        raise ERALGrpcError.CreateStatus(FLastStatus, FLastStatusMessage);
      end;
      if vResponse.StatusCode <> HTTP_OK then
      begin
        case vResponse.StatusCode of
          HTTP_BadRequest: vStatus := RAL_GRPC_INTERNAL;
          HTTP_Unauthorized: vStatus := RAL_GRPC_UNAUTHENTICATED;
          HTTP_Forbidden: vStatus := RAL_GRPC_PERMISSION_DENIED;
          HTTP_NotFound: vStatus := RAL_GRPC_UNIMPLEMENTED;
          429, 502, 503, 504: vStatus := RAL_GRPC_UNAVAILABLE;
        else
          vStatus := RAL_GRPC_UNKNOWN;
        end;
        FLastStatus := vStatus;
        FLastStatusMessage := StringRAL('HTTP ' + IntToStr(vResponse.StatusCode));
        raise ERALGrpcError.CreateStatus(FLastStatus, FLastStatusMessage);
      end;
      if not RALGrpcProtocolOf(vResponse.ContentType, vProtocol) then
      begin
        FLastStatus := RAL_GRPC_UNKNOWN;
        FLastStatusMessage := 'not a gRPC answer: ' + vResponse.ContentType;
        raise ERALGrpcError.CreateStatus(FLastStatus, FLastStatusMessage);
      end;
      // the status: trailer frame (gRPC-Web), HTTP trailers (native), or the headers
      // (trailers-only)
      vStatusText := '';
      if vCall.FTrailersRead then
      begin
        FLastMetadata.AddStrings(vCall.FTrailers);
        vStatusText := StringRAL(vCall.FTrailers.Values['grpc-status']);
        FLastStatusMessage := RALGrpcDecodeMessage(StringRAL(vCall.FTrailers.Values['grpc-message']));
      end
      else if (vResponse.Trailers <> nil) and (vResponse.Trailers.Values['grpc-status'] <> '') then
      begin
        FLastMetadata.AddStrings(vResponse.Trailers);
        vStatusText := StringRAL(vResponse.Trailers.Values['grpc-status']);
        FLastStatusMessage := RALGrpcDecodeMessage(StringRAL(vResponse.Trailers.Values['grpc-message']));
      end
      else
      begin
        vStatusText := vResponse.GetHeader('grpc-status');
        FLastStatusMessage := RALGrpcDecodeMessage(vResponse.GetHeader('grpc-message'));
      end;
      if vStatusText = '' then
      begin
        FLastStatus := RAL_GRPC_INTERNAL;
        if vCall.FCancelled then
          FLastStatus := RAL_GRPC_CANCELLED;
        FLastStatusMessage := 'the answer has no grpc-status';
      end
      else
        FLastStatus := StrToIntDef(string(vStatusText), RAL_GRPC_UNKNOWN);
      if vCall.FCancelled and (FLastStatus = RAL_GRPC_OK) then
        FLastStatus := RAL_GRPC_CANCELLED;
      if FLastStatus <> RAL_GRPC_OK then
        raise ERALGrpcError.CreateStatus(FLastStatus, FLastStatusMessage);
      if not Assigned(AOnMessage) then
      begin
        if vCall.FReply = nil then
          raise ERALGrpcError.CreateStatus(RAL_GRPC_INTERNAL, 'the answer has no message');
        AReply := vCall.FReply;
        vCall.FReply := nil;
      end;
    finally
      vResponse.Free;
    end;
  finally
    vCall.Free;
  end;
end;

procedure TRALGrpcClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
end;

procedure TRALGrpcClient.SetClient(AValue: TRALClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALGrpcClient.SetMetadata(AValue: TStrings);
begin
  FMetadata.Assign(AValue);
end;

end.
