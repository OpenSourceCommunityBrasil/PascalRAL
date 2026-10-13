/// SOAP server module of RAL: operations as routes, SOAP 1.1 and 1.2, faults and WSDL.
unit RALSOAPModule;

{$I ../../base/PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALConsts, RALServer, RALRoutes, RALRequest, RALResponse, RALParams,
  RALMIMETypes, RALTools, RALXMLReader, RALXML, RALStorageXML, RALSOAPTypes;

type
  TRALSOAPModule = class;
  TRALSOAPOperation = class;
  TRALSOAPRequest = class;
  TRALSOAPResponse = class;

  /// Handler of an operation.
  TRALSOAPOnExecute = procedure(ARequest: TRALSOAPRequest; AResponse: TRALSOAPResponse) of object;

  { What the module learned from the head of a request: version, action, the first
    element of the Body, an error; and the whole envelope once a handler asks. }
  TRALSOAPEnvelopeInfo = class
  private
    FAction: StringRAL;
    FDocument: TRALXMLDocument;
    FError: StringRAL;
    FErrorCode: TRALSOAPFaultCode;
    FPayloadName: StringRAL;
    FPayloadNamespace: StringRAL;
    FVersion: TRALSOAPVersion;
  public
    destructor Destroy; override;

    /// SOAPAction (1.1) or the action of the content type (1.2), unquoted.
    property Action: StringRAL read FAction;
    /// The parsed envelope; nil until a handler asks for it.
    property Document: TRALXMLDocument read FDocument;
    /// Why the request is refused, or ''.
    property Error: StringRAL read FError;
    /// Fault code of Error.
    property ErrorCode: TRALSOAPFaultCode read FErrorCode;
    /// Local name of the first element of the Body.
    property PayloadName: StringRAL read FPayloadName;
    /// Namespace of the first element of the Body.
    property PayloadNamespace: StringRAL read FPayloadNamespace;
    /// Version of the envelope.
    property Version: TRALSOAPVersion read FVersion;
  end;

  /// An operation of the module: a route with SOAP settings and a typed handler.
  TRALSOAPOperation = class(TRALRoute)
  private
    FHeaders: TStrings;
    FNamespace: StringRAL;
    FOnExecute: TRALSOAPOnExecute;
    FResultDataSet: TDataSet;
    FResultXMLType: TRALXMLType;
    FSOAPAction: StringRAL;
    FStyle: TRALSOAPStyle;

    function GetModule: TRALSOAPModule;
    function GetOperationName: StringRAL;
    procedure SetHeaders(const AValue: TStrings);
    procedure SetOperationName(const AValue: StringRAL);
    procedure SetResultDataSet(const AValue: TDataSet);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    /// SOAPAction of the operation: its own, or the namespace followed by the name.
    function EffectiveAction: StringRAL;
    /// Namespace of the operation: its own, or the module's.
    function EffectiveNamespace: StringRAL;

    /// Module of the operation, or nil.
    property Module: TRALSOAPModule read GetModule;
  published
    { Local names of the header blocks the operation processes; another block marked
      mustUnderstand fails. }
    property Headers: TStrings read FHeaders write SetHeaders;
    /// Name of the operation; also its path below the module's Domain.
    property Name: StringRAL read GetOperationName write SetOperationName;
    /// Namespace of the operation's elements; empty takes the module's.
    property Namespace: StringRAL read FNamespace write FNamespace;
    /// Handler of the operation; with none, ResultDataSet answers.
    property OnExecute: TRALSOAPOnExecute read FOnExecute write FOnExecute;
    /// Dataset opened and answered inside the Body when the handler wrote nothing.
    property ResultDataSet: TDataSet read FResultDataSet write SetResultDataSet;
    /// Format of the dataset of ResultDataSet.
    property ResultXMLType: TRALXMLType read FResultXMLType write FResultXMLType
      default xtDBWare;
    /// SOAPAction clients send; empty takes the namespace followed by the name.
    property SOAPAction: StringRAL read FSOAPAction write FSOAPAction;
    /// document (literal wrapped, the default) or rpc (literal).
    property Style: TRALSOAPStyle read FStyle write FStyle default ssDocument;
  end;

  /// The request as an operation sees it: the envelope, the payload and its values.
  TRALSOAPRequest = class(TRALModuleRequest)
  private
    /// What the module read from the head of the envelope.
    FInfo: TRALSOAPEnvelopeInfo;

    function GetOperation: TRALSOAPOperation;
    function GetVersion: TRALSOAPVersion;
  public
    constructor Create(AModule: TRALModuleRoutes; ARoute: TRALRoute;
      ACore: TRALRequest); override;

    /// SOAPAction or action the client sent.
    function Action: StringRAL;
    /// The parsed envelope; raises ERALSOAPFault (Sender) when it does not parse.
    function Document: TRALXMLDocument;
    /// The Envelope element.
    function Envelope: TRALXMLNode;
    /// Child of the payload named AName (local name), or nil.
    function Param(const AName: StringRAL): TRALXMLNode;
    /// Text of the child AName of the payload, else of the param AName, else ADefault.
    function ParamValue(const AName: StringRAL; const ADefault: StringRAL = ''): StringRAL;
    /// First element of the Body: the operation's wrapper.
    function Payload: TRALXMLNode;
    /// Body element.
    function SOAPBody: TRALXMLNode;
    /// Header element, or nil.
    function SOAPHeader: TRALXMLNode;

    /// Operation being answered.
    property Operation: TRALSOAPOperation read GetOperation;
    /// Version of the envelope; the answer goes in the same one.
    property Version: TRALSOAPVersion read GetVersion;
  end;

  /// The response as an operation writes it: an envelope built with the fluent API.
  TRALSOAPResponse = class(TRALModuleResponse)
  private
    /// Body element of the answer.
    FBody: TRALXMLNode;
    FDocument: TRALXMLDocument;
    FFaulted: boolean;
    /// Header element of the answer, or nil.
    FHeader: TRALXMLNode;
    /// Response wrapper of the operation, or nil.
    FResult: TRALXMLNode;
    /// True once the envelope went into the core response.
    FSent: boolean;
    /// HTTP status of the answer: 200, or the one of the fault.
    FStatus: IntegerRAL;
    FVersion: TRALSOAPVersion;

    /// Creates the Envelope and its Body when they do not exist yet.
    procedure BuildEnvelope;
    function GetSOAPBody: TRALXMLNode;
    function GetSOAPHeader: TRALXMLNode;
    procedure SetVersion(const AValue: TRALSOAPVersion);
  public
    constructor Create(AModule: TRALModuleRoutes; ARoute: TRALRoute;
      ACore: TRALResponse); override;
    destructor Destroy; override;

    /// Adds a header block named AName in ANamespace and returns it.
    function AddHeaderBlock(const AName, ANamespace: StringRAL): TRALXMLNode;
    /// Adds a child named AName to the result wrapper and returns it.
    function AddResult(const AName: StringRAL): TRALXMLNode;
    /// Answers with a copy of ANode as the content of the Body.
    procedure Answer(ANode: TRALXMLNode); overload;
    /// Answers with ADataSet, in AXMLType, inside the result wrapper.
    procedure Answer(ADataSet: TDataSet; AXMLType: TRALXMLType = xtDBWare); overload;
    { Answers a fault of ACode with AReason and the XML ADetail; the HTTP status
      follows the version. }
    procedure AnswerFault(ACode: TRALSOAPFaultCode; const AReason: StringRAL;
      const ADetail: StringRAL = '');
    /// Answers with the XML fragment AXML as the content of the Body.
    procedure AnswerXML(const AXML: StringRAL);
    /// True when something was written into the Body.
    function HasContent: boolean;
    /// The operation's response wrapper (OperationResponse), created on first use.
    function Payload: TRALXMLNode;
    /// Writes the envelope into the core response; the module calls it after the handler.
    procedure Send;

    /// The response envelope document.
    property Document: TRALXMLDocument read FDocument;
    /// True once AnswerFault was called.
    property Faulted: boolean read FFaulted;
    /// Body element.
    property SOAPBody: TRALXMLNode read GetSOAPBody;
    /// Header element, created on first use.
    property SOAPHeader: TRALXMLNode read GetSOAPHeader;
    /// Version of the envelope; it follows the request's.
    property Version: TRALSOAPVersion read FVersion write SetVersion;
  end;

  { SOAP module: each operation is a route, answered at the module's Domain (by
    SOAPAction, action or the first element of the Body) or at Domain/Name; a GET
    of the Domain answers the WSDL. }
  TRALSOAPModule = class(TRALModuleRoutes)
  private
    FContentType: StringRAL;
    /// Route of a POST to the endpoint whose operation is not found.
    FDispatchRoute: TRALSOAPOperation;
    FEnvelopeContentType: StringRAL;
    /// Routes of the endpoint itself, kept out of Operations.
    FInternal: TRALRoutes;
    FNamespace: StringRAL;
    FReaderOptions: TRALXMLReaderOptions;
    FServiceName: StringRAL;
    FServiceURL: StringRAL;
    FVersions: TRALSOAPVersions;
    FWSDLContentType: StringRAL;
    FWSDLFile: string;
    FWSDLRequireAuth: boolean;
    /// Route of a GET of the endpoint: the WSDL.
    FWSDLRoute: TRALSOAPOperation;

    /// Answers the WSDL: WSDLFile, or the one generated for the address asked.
    procedure AnswerWSDL(ARequest: TRALRequest; AResponse: TRALResponse);
    /// The operation of the action, else of the first element of the Body; nil if none.
    function FindOperation(AInfo: TRALSOAPEnvelopeInfo): TRALSOAPOperation;
    function GetOperations: TRALRoutes;
    /// Reads the version and the first element of the Body into AInfo, before any auth.
    procedure ReadHead(ARequest: TRALRequest; AInfo: TRALSOAPEnvelopeInfo);
    procedure SetOperations(const AValue: TRALRoutes);
    procedure SetReaderOptions(const AValue: TRALXMLReaderOptions);
    procedure SetWSDLRequireAuth(const AValue: boolean);
  protected
    procedure ExecuteContext(ARoute: TRALRoute; ARequest: TRALModuleRequest;
      AResponse: TRALModuleResponse); override;
    function HandleException(ARequest: TRALRequest; AResponse: TRALResponse;
      AException: Exception): boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    class function RequestClass: TRALModuleRequestClass; override;
    class function ResponseClass: TRALModuleResponseClass; override;
    class function RouteClass: TRALRouteClass; override;
    procedure SetDomain(const AValue: StringRAL); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Adds an operation named AName with the handler AHandler and returns it.
    function AddOperation(const AName: StringRAL; AHandler: TRALSOAPOnExecute;
      const ADescription: StringRAL = ''): TRALSOAPOperation;
    /// Operation of the request, read from the head of the envelope; the WSDL for a GET.
    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute; override;
    /// Content type of the answers that are not envelopes (application/xml by default).
    function DefaultContentType: StringRAL; override;
    /// The operation named AName, or nil.
    function FindOperationByName(const AName: StringRAL): TRALSOAPOperation;
    /// The WSDL of the module for a service at AURL.
    function GenerateWSDL(const AURL: StringRAL): StringRAL;
  published
    { Content type of the answers of the module that are not envelopes; empty is
      application/xml. }
    property ContentType: StringRAL read FContentType write FContentType;
    { Content type of the envelopes, faults included; empty follows the version
      (text/xml for 1.1, application/soap+xml for 1.2), as the standards ask. }
    property EnvelopeContentType: StringRAL read FEnvelopeContentType write FEnvelopeContentType;
    /// Target namespace of the service and of the operations that have none.
    property Namespace: StringRAL read FNamespace write FNamespace;
    /// Operations of the service.
    property Operations: TRALRoutes read GetOperations write SetOperations;
    /// Limits of the parser for the envelopes that arrive.
    property ReaderOptions: TRALXMLReaderOptions read FReaderOptions write SetReaderOptions;
    /// Name of the service in the WSDL.
    property ServiceName: StringRAL read FServiceName write FServiceName;
    /// Address of the service in the WSDL; empty takes the address of the request.
    property ServiceURL: StringRAL read FServiceURL write FServiceURL;
    /// Versions of SOAP accepted; another one is answered VersionMismatch.
    property Versions: TRALSOAPVersions read FVersions write FVersions default [sv11, sv12];
    /// Content type of the WSDL; empty is text/xml.
    property WSDLContentType: StringRAL read FWSDLContentType write FWSDLContentType;
    /// File served as the WSDL in place of the one generated.
    property WSDLFile: string read FWSDLFile write FWSDLFile;
    /// The WSDL asks the server's authentication too; False serves it to anyone.
    property WSDLRequireAuth: boolean read FWSDLRequireAuth write SetWSDLRequireAuth
      default False;
  end;

implementation

uses
  RALWSDLExporter;

{ The charset the client declared, from the header as it came: ContentType gains
  '; charset=utf-8' when it has none, and that would override the XML declaration
  (RFC 7303). '' lets the BOM or the declaration decide. }
function RequestCharset(ARequest: TRALRequest): StringRAL;
begin
  Result := RALMediaTypeCharset(ARequest.GetHeader('Content-Type'));
end;

/// ' (SOAPAction "x")' for an action, '' without one.
function ActionSuffix(const AAction: StringRAL): string;
begin
  if AAction <> '' then
    Result := ' (SOAPAction "' + string(AAction) + '")'
  else
    Result := '';
end;

{ TRALSOAPEnvelopeInfo }

destructor TRALSOAPEnvelopeInfo.Destroy;
begin
  FreeAndNil(FDocument);
  inherited Destroy;
end;

{ TRALSOAPOperation }

constructor TRALSOAPOperation.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FHeaders := TStringList.Create;
  FResultXMLType := xtDBWare;
  FStyle := ssDocument;
  AllowedMethods := [amPOST, amOPTIONS];
end;

destructor TRALSOAPOperation.Destroy;
begin
  FreeAndNil(FHeaders);
  inherited Destroy;
end;

procedure TRALSOAPOperation.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TRALSOAPOperation then
  begin
    TRALSOAPOperation(Dest).FHeaders.Assign(FHeaders);
    TRALSOAPOperation(Dest).FNamespace := FNamespace;
    TRALSOAPOperation(Dest).FOnExecute := FOnExecute;
    TRALSOAPOperation(Dest).FResultXMLType := FResultXMLType;
    TRALSOAPOperation(Dest).FSOAPAction := FSOAPAction;
    TRALSOAPOperation(Dest).FStyle := FStyle;
    TRALSOAPOperation(Dest).ResultDataSet := FResultDataSet;
  end;
end;

function TRALSOAPOperation.GetModule: TRALSOAPModule;
begin
  Result := nil;
  if (Collection is TRALRoutes) and (TRALRoutes(Collection).Owner is TRALSOAPModule) then
    Result := TRALSOAPModule(TRALRoutes(Collection).Owner);
end;

function TRALSOAPOperation.GetOperationName: StringRAL;
begin
  Result := inherited Name;
end;

procedure TRALSOAPOperation.SetOperationName(const AValue: StringRAL);
begin
  // the path follows the name while the path was never set apart from it
  if (Route = '') or (Route = inherited Name) or (Route = '/' + inherited Name) then
    Route := AValue;
  inherited Name := AValue;
end;

procedure TRALSOAPOperation.SetHeaders(const AValue: TStrings);
begin
  FHeaders.Assign(AValue);
end;

procedure TRALSOAPOperation.SetResultDataSet(const AValue: TDataSet);
begin
  FResultDataSet := AValue;
  if (AValue <> nil) and (Module <> nil) then
    AValue.FreeNotification(Module);
end;

function TRALSOAPOperation.EffectiveNamespace: StringRAL;
begin
  Result := FNamespace;
  if (Result = '') and (Module <> nil) then
    Result := Module.Namespace;
end;

function TRALSOAPOperation.EffectiveAction: StringRAL;
var
  vNamespace: StringRAL;
begin
  Result := FSOAPAction;
  if Result <> '' then
    Exit;
  vNamespace := EffectiveNamespace;
  if (vNamespace <> '') and (vNamespace[Length(vNamespace)] in ['/', '#', ':']) then
    Result := vNamespace + Name
  else if vNamespace <> '' then
    Result := vNamespace + '/' + Name
  else
    Result := Name;
end;

{ TRALSOAPRequest }

constructor TRALSOAPRequest.Create(AModule: TRALModuleRoutes; ARoute: TRALRoute;
  ACore: TRALRequest);
begin
  inherited Create(AModule, ARoute, ACore);
  if ACore.RouteData is TRALSOAPEnvelopeInfo then
    FInfo := TRALSOAPEnvelopeInfo(ACore.RouteData);
end;

function TRALSOAPRequest.GetVersion: TRALSOAPVersion;
begin
  if FInfo <> nil then
    Result := FInfo.Version
  else
    Result := sv11;
end;

function TRALSOAPRequest.GetOperation: TRALSOAPOperation;
begin
  if Route is TRALSOAPOperation then
    Result := TRALSOAPOperation(Route)
  else
    Result := nil;
end;

function TRALSOAPRequest.Action: StringRAL;
begin
  if FInfo <> nil then
    Result := FInfo.Action
  else
    Result := '';
end;

function TRALSOAPRequest.Document: TRALXMLDocument;
var
  vStream: TStream;
begin
  if FInfo = nil then
  begin
    FInfo := TRALSOAPEnvelopeInfo.Create;
    Core.RouteData := FInfo;
  end;
  if FInfo.FDocument = nil then
  begin
    FInfo.FDocument := TRALXMLDocument.Create;
    if Module is TRALSOAPModule then
      FInfo.FDocument.Options.Assign(TRALSOAPModule(Module).ReaderOptions);
    try
      vStream := Core.RequestStream;
      if vStream = nil then
        raise ERALXMLError.CreateAt(emSOAPNoEnvelope, 0, 0);
      FInfo.FDocument.LoadFromStream(vStream, RequestCharset(Core));
    except
      on E: Exception do
      begin
        FreeAndNil(FInfo.FDocument);
        raise ERALSOAPFault.CreateFault(sfcSender, StringRAL(E.Message));
      end;
    end;
  end;
  Result := FInfo.FDocument;
end;

function TRALSOAPRequest.Envelope: TRALXMLNode;
begin
  Result := Document.Root;
end;

function TRALSOAPRequest.SOAPHeader: TRALXMLNode;
begin
  Result := Envelope.ChildNS(RALSOAPNamespace(Version), 'Header');
end;

function TRALSOAPRequest.SOAPBody: TRALXMLNode;
begin
  Result := Envelope.ChildNS(RALSOAPNamespace(Version), 'Body');
  if Result = nil then
    raise ERALSOAPFault.CreateFault(sfcSender, StringRAL(emSOAPNoBody));
end;

function TRALSOAPRequest.Payload: TRALXMLNode;
begin
  Result := SOAPBody.FirstElement;
end;

function TRALSOAPRequest.Param(const AName: StringRAL): TRALXMLNode;
var
  vPayload: TRALXMLNode;
begin
  Result := nil;
  vPayload := Payload;
  if vPayload <> nil then
    Result := vPayload.Child(AName);
end;

function TRALSOAPRequest.ParamValue(const AName: StringRAL;
  const ADefault: StringRAL): StringRAL;
var
  vNode: TRALXMLNode;
  vParam: TRALParam;
begin
  vNode := Param(AName);
  if vNode <> nil then
  begin
    Result := vNode.Text;
    Exit;
  end;
  vParam := Core.ParamByName(AName);
  if vParam <> nil then
    Result := vParam.AsString
  else
    Result := ADefault;
end;

{ TRALSOAPResponse }

constructor TRALSOAPResponse.Create(AModule: TRALModuleRoutes; ARoute: TRALRoute;
  ACore: TRALResponse);
begin
  inherited Create(AModule, ARoute, ACore);
  FDocument := TRALXMLDocument.Create;
  FVersion := sv11;
  FStatus := HTTP_OK;
end;

destructor TRALSOAPResponse.Destroy;
begin
  FreeAndNil(FDocument);
  inherited Destroy;
end;

procedure TRALSOAPResponse.SetVersion(const AValue: TRALSOAPVersion);
begin
  if AValue = FVersion then
    Exit;
  FVersion := AValue;
  // an envelope begun in the other version starts again
  FDocument.Clear;
  FBody := nil;
  FHeader := nil;
  FResult := nil;
end;

procedure TRALSOAPResponse.BuildEnvelope;
begin
  if FBody <> nil then
    Exit;
  FBody := FDocument.CreateRoot('soap:Envelope')
    .AddNamespace('soap', RALSOAPNamespace(FVersion))
    .AddChild('soap:Body');
end;

function TRALSOAPResponse.GetSOAPBody: TRALXMLNode;
begin
  BuildEnvelope;
  Result := FBody;
end;

function TRALSOAPResponse.GetSOAPHeader: TRALXMLNode;
begin
  BuildEnvelope;
  if FHeader = nil then
  begin
    FHeader := FDocument.CreateElement('soap:Header');
    FDocument.Root.InsertBefore(FHeader, FBody);
  end;
  Result := FHeader;
end;

function TRALSOAPResponse.AddHeaderBlock(const AName, ANamespace: StringRAL): TRALXMLNode;
begin
  Result := GetSOAPHeader.AddChild(AName);
  if ANamespace <> '' then
  begin
    if Pos(StringRAL(':'), AName) > 0 then
      Result.AddNamespace(Copy(AName, 1, Pos(StringRAL(':'), AName) - 1), ANamespace)
    else
      Result.AddNamespace('', ANamespace);
  end;
end;

function TRALSOAPResponse.Payload: TRALXMLNode;
var
  vOperation: TRALSOAPOperation;
  vNamespace: StringRAL;
begin
  if FResult = nil then
  begin
    vOperation := nil;
    if Route is TRALSOAPOperation then
      vOperation := TRALSOAPOperation(Route);
    if vOperation = nil then
    begin
      FResult := GetSOAPBody;
      Exit(FResult);
    end;

    vNamespace := vOperation.EffectiveNamespace;
    if vOperation.Style = ssRPC then
    begin
      // rpc/literal: the wrapper is qualified, the parts are not
      FResult := GetSOAPBody.AddChild('tns:' + vOperation.Name + 'Response');
      FResult.AddNamespace('tns', vNamespace);
    end
    else
    begin
      // document/literal wrapped, qualified elements: the default namespace covers all
      FResult := GetSOAPBody.AddChild(vOperation.Name + 'Response');
      if vNamespace <> '' then
        FResult.AddNamespace('', vNamespace);
    end;
  end;
  Result := FResult;
end;

function TRALSOAPResponse.AddResult(const AName: StringRAL): TRALXMLNode;
begin
  Result := Payload.AddChild(AName);
end;

procedure TRALSOAPResponse.Answer(ANode: TRALXMLNode);
begin
  GetSOAPBody.Clear;
  FResult := nil;
  if ANode <> nil then
    FBody.AddXML(ANode.ToXML(False));
end;

procedure TRALSOAPResponse.Answer(ADataSet: TDataSet; AXMLType: TRALXMLType);
begin
  Payload.AppendDataSet(ADataSet, AXMLType);
end;

procedure TRALSOAPResponse.AnswerXML(const AXML: StringRAL);
begin
  GetSOAPBody.Clear;
  FResult := nil;
  FBody.AddXML(AXML);
end;

procedure TRALSOAPResponse.AnswerFault(ACode: TRALSOAPFaultCode; const AReason: StringRAL;
  const ADetail: StringRAL);
begin
  RALSOAPBuildFault(FDocument, FVersion, ACode, AReason, ADetail);
  FBody := FDocument.Root.Child('Body');
  FHeader := nil;
  FResult := nil;
  FFaulted := True;
  FStatus := RALSOAPFaultStatus(FVersion, ACode);
end;

function TRALSOAPResponse.HasContent: boolean;
begin
  Result := (FBody <> nil) and (FBody.FirstChild <> nil);
end;

procedure TRALSOAPResponse.Send;
var
  vStream: TMemoryStream;
  vType: StringRAL;
begin
  if FSent then
    Exit;
  FSent := True;
  BuildEnvelope;

  vType := '';
  if Module is TRALSOAPModule then
    vType := TRALSOAPModule(Module).EnvelopeContentType;
  if vType = '' then
    vType := RALSOAPContentType(FVersion);

  vStream := TMemoryStream.Create;
  try
    FDocument.SaveToStream(vStream, True);
    vStream.Position := 0;
    Core.Answer(FStatus, vStream, vType, True);
    vStream := nil;
  finally
    vStream.Free;
  end;
end;

{ TRALSOAPModule }

constructor TRALSOAPModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVersions := [sv11, sv12];
  FReaderOptions := TRALXMLReaderOptions.Create;
  FServiceName := 'RALService';
  FNamespace := 'urn:ral-service';

  // the endpoint itself: a GET answers the WSDL, a POST the operation it names
  FInternal := TRALRoutes.Create(Self, TRALSOAPOperation);
  FWSDLRoute := TRALSOAPOperation(FInternal.Add);
  FWSDLRoute.Route := '';
  FWSDLRoute.AllowedMethods := [amGET, amHEAD];
  FWSDLRoute.SkipAuthMethods := [amALL];
  FDispatchRoute := TRALSOAPOperation(FInternal.Add);
  FDispatchRoute.Route := '';
  FDispatchRoute.AllowedMethods := [amPOST, amOPTIONS];
end;

destructor TRALSOAPModule.Destroy;
begin
  FreeAndNil(FInternal);
  FreeAndNil(FReaderOptions);
  inherited Destroy;
end;

class function TRALSOAPModule.RouteClass: TRALRouteClass;
begin
  Result := TRALSOAPOperation;
end;

class function TRALSOAPModule.RequestClass: TRALModuleRequestClass;
begin
  Result := TRALSOAPRequest;
end;

class function TRALSOAPModule.ResponseClass: TRALModuleResponseClass;
begin
  Result := TRALSOAPResponse;
end;

function TRALSOAPModule.DefaultContentType: StringRAL;
begin
  Result := FContentType;
  if Result = '' then
    Result := rctAPPLICATIONXML;
end;

procedure TRALSOAPModule.SetDomain(const AValue: StringRAL);
var
  vInt: IntegerRAL;
begin
  inherited SetDomain(AValue);
  if FInternal <> nil then
    for vInt := 0 to FInternal.Count - 1 do
      TRALBaseRoute(FInternal.Items[vInt]).UpdateSegments;
end;

procedure TRALSOAPModule.SetReaderOptions(const AValue: TRALXMLReaderOptions);
begin
  FReaderOptions.Assign(AValue);
end;

procedure TRALSOAPModule.SetWSDLRequireAuth(const AValue: boolean);
begin
  FWSDLRequireAuth := AValue;
  if AValue then
    FWSDLRoute.SkipAuthMethods := []
  else
    FWSDLRoute.SkipAuthMethods := [amALL];
end;

function TRALSOAPModule.GetOperations: TRALRoutes;
begin
  Result := Routes;
end;

procedure TRALSOAPModule.SetOperations(const AValue: TRALRoutes);
begin
  Routes := AValue;
end;

procedure TRALSOAPModule.Notification(AComponent: TComponent; Operation: TOperation);
var
  vInt: IntegerRAL;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TDataSet) and (Routes <> nil) then
    for vInt := 0 to Routes.Count - 1 do
      if TRALSOAPOperation(Routes.Items[vInt]).ResultDataSet = AComponent then
        TRALSOAPOperation(Routes.Items[vInt]).ResultDataSet := nil;
end;

function TRALSOAPModule.AddOperation(const AName: StringRAL; AHandler: TRALSOAPOnExecute;
  const ADescription: StringRAL): TRALSOAPOperation;
begin
  Result := TRALSOAPOperation(NewRoute(AName, ADescription));
  Result.Name := AName;
  Result.OnExecute := AHandler;
end;

function TRALSOAPModule.FindOperationByName(const AName: StringRAL): TRALSOAPOperation;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to Routes.Count - 1 do
    if TRALSOAPOperation(Routes.Items[vInt]).Name = AName then
    begin
      Result := TRALSOAPOperation(Routes.Items[vInt]);
      Exit;
    end;
end;

procedure TRALSOAPModule.ReadHead(ARequest: TRALRequest; AInfo: TRALSOAPEnvelopeInfo);
var
  vReader: TRALXMLReader;
  vStream: TStream;
  vNamespace: StringRAL;
begin
  // only the head, before any authentication: up to the first element of the Body
  vStream := ARequest.RequestStream;
  if (vStream = nil) or (vStream.Size = 0) then
  begin
    AInfo.FError := StringRAL(emSOAPNoEnvelope);
    AInfo.FErrorCode := sfcSender;
    Exit;
  end;

  vReader := TRALXMLReader.Create(nil, FReaderOptions);
  try
    try
      vReader.SetInputStream(vStream, RequestCharset(ARequest));
      while vReader.Read do
      begin
        case vReader.Token of
          xtkDocType, xtkProcessingInstruction:
            begin
              // SOAP 1.2 part 1, 5: no DOCTYPE and no processing instruction
              AInfo.FError := StringRAL(emSOAPDocType);
              AInfo.FErrorCode := sfcSender;
              Exit;
            end;
          xtkStartElement:
            begin
              case vReader.Depth of
                1:
                  begin
                    vNamespace := vReader.NamespaceURI;
                    if vReader.LocalName <> 'Envelope' then
                    begin
                      AInfo.FError := StringRAL(emSOAPNoEnvelope);
                      AInfo.FErrorCode := sfcSender;
                      Exit;
                    end;
                    if vNamespace = RALSOAP11Namespace then
                      AInfo.FVersion := sv11
                    else if vNamespace = RALSOAP12Namespace then
                      AInfo.FVersion := sv12
                    else
                    begin
                      AInfo.FError := StringRAL(emSOAPVersionMismatch);
                      AInfo.FErrorCode := sfcVersionMismatch;
                      Exit;
                    end;
                    if not (AInfo.FVersion in FVersions) then
                    begin
                      AInfo.FError := StringRAL(emSOAPVersionMismatch);
                      AInfo.FErrorCode := sfcVersionMismatch;
                      Exit;
                    end;
                  end;
                2:
                  if vReader.LocalName <> 'Body' then
                    vReader.Skip;
                3:
                  begin
                    AInfo.FPayloadName := vReader.LocalName;
                    AInfo.FPayloadNamespace := vReader.NamespaceURI;
                    Exit;
                  end;
              end;
            end;
        end;
      end;
    except
      on E: Exception do
      begin
        AInfo.FError := StringRAL(E.Message);
        AInfo.FErrorCode := sfcSender;
      end;
    end;
  finally
    vReader.Free;
  end;
end;

function TRALSOAPModule.FindOperation(AInfo: TRALSOAPEnvelopeInfo): TRALSOAPOperation;
var
  vInt: IntegerRAL;
  vOperation: TRALSOAPOperation;
begin
  Result := nil;
  // the action names the operation; the element of the Body does when there is none
  if AInfo.Action <> '' then
    for vInt := 0 to Routes.Count - 1 do
    begin
      vOperation := TRALSOAPOperation(Routes.Items[vInt]);
      if vOperation.EffectiveAction = AInfo.Action then
      begin
        Result := vOperation;
        Exit;
      end;
    end;

  if AInfo.PayloadName <> '' then
    for vInt := 0 to Routes.Count - 1 do
    begin
      vOperation := TRALSOAPOperation(Routes.Items[vInt]);
      if (vOperation.Name = AInfo.PayloadName) and
         ((AInfo.PayloadNamespace = '') or (vOperation.EffectiveNamespace = '') or
          (vOperation.EffectiveNamespace = AInfo.PayloadNamespace)) then
      begin
        Result := vOperation;
        Exit;
      end;
    end;
end;

function TRALSOAPModule.CanAnswerRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
var
  vInfo: TRALSOAPEnvelopeInfo;
  vEndpoint: boolean;
begin
  Result := nil;
  vEndpoint := RALSameName(ARequest.Query, FixRoute(Domain));
  if not vEndpoint then
  begin
    // Domain/Name: the operation by its path
    Result := inherited CanAnswerRoute(ARequest, AResponse);
    if (Result = nil) or (ARequest.Method in [amGET, amHEAD, amOPTIONS]) then
      Exit;
  end
  else if ARequest.Method in [amGET, amHEAD] then
  begin
    Result := FWSDLRoute;
    if Assigned(OnBeforeAnswer) then
      OnBeforeAnswer(ARequest, AResponse);
    Exit;
  end;

  if ARequest.Method <> amPOST then
  begin
    if Result = nil then
      Result := FDispatchRoute;
    Exit;
  end;

  vInfo := TRALSOAPEnvelopeInfo.Create;
  ARequest.RouteData := vInfo;
  ReadHead(ARequest, vInfo);
  if vInfo.FVersion = sv12 then
    vInfo.FAction := RALMediaTypeParam(ARequest.ContentType, 'action');
  if vInfo.FAction = '' then
    vInfo.FAction := RALSOAPUnquote(ARequest.GetHeader('SOAPAction'));

  if Result = nil then
  begin
    if vInfo.Error = '' then
      Result := FindOperation(vInfo);
    if Result = nil then
      Result := FDispatchRoute;
    if Assigned(OnBeforeAnswer) then
      OnBeforeAnswer(ARequest, AResponse);
  end;
end;

procedure TRALSOAPModule.AnswerWSDL(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vText, vURL, vType: StringRAL;
  vStream: TMemoryStream;
begin
  vType := FWSDLContentType;
  if vType = '' then
    vType := rctTEXTXML;

  if FWSDLFile <> '' then
  begin
    vStream := TMemoryStream.Create;
    try
      vStream.LoadFromFile(FWSDLFile);
      vStream.Position := 0;
      AResponse.Answer(HTTP_OK, vStream, vType, True);
      vStream := nil;
    finally
      vStream.Free;
    end;
    Exit;
  end;

  vURL := FServiceURL;
  if vURL = '' then
  begin
    vURL := ARequest.URL;
    if Pos(StringRAL('?'), vURL) > 0 then
      vURL := Copy(vURL, 1, Pos(StringRAL('?'), vURL) - 1);
  end;
  vText := GenerateWSDL(vURL);
  AResponse.Answer(HTTP_OK, vText, vType);
end;

function TRALSOAPModule.GenerateWSDL(const AURL: StringRAL): StringRAL;
var
  vExporter: TRALWSDLExporter;
begin
  vExporter := TRALWSDLExporter.Create;
  try
    Result := vExporter.Generate(Self, AURL);
  finally
    vExporter.Free;
  end;
end;

procedure TRALSOAPModule.ExecuteContext(ARoute: TRALRoute; ARequest: TRALModuleRequest;
  AResponse: TRALModuleResponse);
var
  vRequest: TRALSOAPRequest;
  vResponse: TRALSOAPResponse;
  vOperation: TRALSOAPOperation;
  vInfo: TRALSOAPEnvelopeInfo;
  vHeader, vBlock: TRALXMLNode;
  vMust, vRole: StringRAL;
  vNamespace: StringRAL;
begin
  if ARoute = FWSDLRoute then
  begin
    AnswerWSDL(ARequest.Core, AResponse.Core);
    Exit;
  end;

  vRequest := TRALSOAPRequest(ARequest);
  vResponse := TRALSOAPResponse(AResponse);
  vInfo := nil;
  if ARequest.Core.RouteData is TRALSOAPEnvelopeInfo then
    vInfo := TRALSOAPEnvelopeInfo(ARequest.Core.RouteData);
  if vInfo <> nil then
    vResponse.Version := vInfo.Version;

  try
    if (vInfo = nil) then
      raise ERALSOAPFault.CreateFault(sfcSender, StringRAL(emSOAPNoEnvelope));
    if vInfo.Error <> '' then
      raise ERALSOAPFault.CreateFault(vInfo.ErrorCode, vInfo.Error);
    if ARoute = FDispatchRoute then
      raise ERALSOAPFault.CreateFault(sfcSender,
        StringRAL(Format(emSOAPOperationNotFound, [string(vInfo.PayloadName),
        ActionSuffix(vInfo.Action)])));

    vOperation := TRALSOAPOperation(ARoute);

    // a header block for this node that the operation does not process
    vHeader := vRequest.SOAPHeader;
    if vHeader <> nil then
    begin
      vNamespace := RALSOAPNamespace(vRequest.Version);
      vBlock := vHeader.FirstElement;
      while vBlock <> nil do
      begin
        vMust := vBlock.AttributeNS(vNamespace, 'mustUnderstand');
        if vRequest.Version = sv12 then
          vRole := vBlock.AttributeNS(vNamespace, 'role')
        else
          vRole := vBlock.AttributeNS(vNamespace, 'actor');
        if ((vMust = '1') or (vMust = 'true')) and
           ((vRole = '') or (vRole = 'http://www.w3.org/2003/05/soap-envelope/role/next') or
            (vRole = 'http://www.w3.org/2003/05/soap-envelope/role/ultimateReceiver') or
            (vRole = 'http://schemas.xmlsoap.org/soap/actor/next')) and
           (vOperation.Headers.IndexOf(string(vBlock.LocalName)) < 0) then
          raise ERALSOAPFault.CreateFault(sfcMustUnderstand,
            StringRAL(Format(emSOAPMustUnderstand, [string(vBlock.NodeName)])));
        vBlock := vBlock.NextElement;
      end;
    end;

    if Assigned(vOperation.OnExecute) then
      vOperation.OnExecute(vRequest, vResponse);

    if (not vResponse.Faulted) and (not vResponse.HasContent) and
       (vOperation.ResultDataSet <> nil) then
    begin
      if not vOperation.ResultDataSet.Active then
        vOperation.ResultDataSet.Open;
      vResponse.Answer(vOperation.ResultDataSet, vOperation.ResultXMLType);
    end;

    // an operation that answers nothing answers its empty wrapper
    if (not vResponse.Faulted) and (not vResponse.HasContent) then
      vResponse.Payload;
  except
    on E: ERALSOAPFault do
      vResponse.AnswerFault(E.Code, E.Reason, E.Detail);
  end;
  vResponse.Send;
end;

function TRALSOAPModule.HandleException(ARequest: TRALRequest; AResponse: TRALResponse;
  AException: Exception): boolean;
var
  vDocument: TRALXMLDocument;
  vVersion: TRALSOAPVersion;
  vStream: TMemoryStream;
  vCode: TRALSOAPFaultCode;
  vReason, vDetail, vType: StringRAL;
begin
  // every exception of an operation is answered as a fault, never as a page of the server
  vVersion := sv11;
  if ARequest.RouteData is TRALSOAPEnvelopeInfo then
    vVersion := TRALSOAPEnvelopeInfo(ARequest.RouteData).Version;

  vDetail := '';
  if AException is ERALSOAPFault then
  begin
    vCode := ERALSOAPFault(AException).Code;
    vReason := ERALSOAPFault(AException).Reason;
    vDetail := ERALSOAPFault(AException).Detail;
  end
  else
  begin
    vCode := sfcReceiver;
    vReason := ErrorText(AException);
  end;

  vDocument := TRALXMLDocument.Create;
  vStream := TMemoryStream.Create;
  try
    RALSOAPBuildFault(vDocument, vVersion, vCode, vReason, vDetail);
    vDocument.SaveToStream(vStream, True);
    vStream.Position := 0;
    vType := FEnvelopeContentType;
    if vType = '' then
      vType := RALSOAPContentType(vVersion);
    AResponse.Params.ClearParams(rpkBODY);
    AResponse.Answer(RALSOAPFaultStatus(vVersion, vCode), vStream, vType, True);
    vStream := nil;
  finally
    vStream.Free;
    vDocument.Free;
  end;
  Result := True;
end;

end.
