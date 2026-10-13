/// SOAP client of RAL: a TRALClient calling operations set up in the IDE or from a WSDL.
unit RALSOAPClient;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB,
  RALTypes, RALConsts, RALClient, RALRequest, RALResponse, RALRoutes, RALParams,
  RALMIMETypes, RALTools, RALCompress, RALXMLReader, RALXML, RALStorageXML, RALSOAPTypes;

type
  TRALSOAPClient = class;
  TRALSOAPClientOperation = class;

  /// A value sent or received by an operation.
  TRALSOAPClientParam = class(TCollectionItem)
  private
    FName: StringRAL;
    FParamType: TRALRouteParamType;
    FRequired: boolean;
    FValue: StringRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  published
    /// Name of the element.
    property Name: StringRAL read FName write FName;
    /// Type, as the WSDL declares it.
    property ParamType: TRALRouteParamType read FParamType write FParamType default prtString;
    /// The WSDL marks it required.
    property Required: boolean read FRequired write FRequired default False;
    /// Value as text; for prtXML, an XML fragment.
    property Value: StringRAL read FValue write FValue;
  end;

  /// Values of an operation.
  TRALSOAPClientParams = class(TOwnedCollection)
  private
    function GetParam(const AName: StringRAL): TRALSOAPClientParam;
  public
    /// Collection owned by AOwner.
    constructor Create(AOwner: TPersistent);

    /// Adds a value named AName with AValue and returns it.
    function AddParam(const AName: StringRAL; const AValue: StringRAL = '';
      AType: TRALRouteParamType = prtString): TRALSOAPClientParam;
    /// The value named AName, or nil.
    function Find(const AName: StringRAL): TRALSOAPClientParam;
    /// Text of the value named AName, or ''.
    function ValueOf(const AName: StringRAL): StringRAL;

    /// The value named AName; created when absent.
    property Param[const AName: StringRAL]: TRALSOAPClientParam read GetParam; default;
  end;

  /// Event fired with a fault an operation received.
  TRALSOAPOnFault = procedure(Sender: TObject; AOperation: TRALSOAPClientOperation;
    AFault: ERALSOAPFault) of object;
  /// Event fired with the envelope an operation received.
  TRALSOAPOnResponse = procedure(Sender: TObject; AOperation: TRALSOAPClientOperation;
    AResponse: TRALXMLDocument) of object;

  /// An operation of the service: its values, its answer and its events.
  TRALSOAPClientOperation = class(TCollectionItem)
  private
    FLastError: StringRAL;
    FName: StringRAL;
    FNamespace: StringRAL;
    FOnFault: TRALSOAPOnFault;
    FOnResponse: TRALSOAPOnResponse;
    FParams: TRALSOAPClientParams;
    FResponse: TRALXMLDocument;
    FResultDataSet: TDataSet;
    FResults: TRALSOAPClientParams;
    FSOAPAction: StringRAL;
    FStyle: TRALSOAPStyle;

    function GetClient: TRALSOAPClient;
    procedure SetParams(const AValue: TRALSOAPClientParams);
    procedure SetResultDataSet(const AValue: TDataSet);
    procedure SetResults(const AValue: TRALSOAPClientParams);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    /// SOAPAction sent: its own, or the namespace followed by the name.
    function EffectiveAction: StringRAL;
    /// Namespace of the operation: its own, or the client's.
    function EffectiveNamespace: StringRAL;
    { Calls the operation with Params; fills Results and ResultDataSet. True on
      success; a fault goes to OnFault, or raises when no event takes it. With
      ebMultiThread it returns at once and the answer arrives in the main thread. }
    function Execute(AExecBehavior: TRALExecBehavior = ebSingleThread): boolean;

    /// Client of the operation.
    property Client: TRALSOAPClient read GetClient;
    /// Reason of the last fault, '' after a call that succeeded.
    property LastError: StringRAL read FLastError;
    /// Envelope of the last answer; nil before the first call.
    property Response: TRALXMLDocument read FResponse;
  published
    /// Name of the operation.
    property Name: StringRAL read FName write FName;
    /// Namespace of the operation; empty takes the client's.
    property Namespace: StringRAL read FNamespace write FNamespace;
    /// Fired with a fault; with no handler here or on the client, Execute raises it.
    property OnFault: TRALSOAPOnFault read FOnFault write FOnFault;
    /// Fired with each answer that is not a fault.
    property OnResponse: TRALSOAPOnResponse read FOnResponse write FOnResponse;
    /// Values sent.
    property Params: TRALSOAPClientParams read FParams write SetParams;
    /// Dataset that receives a dataset the answer carries.
    property ResultDataSet: TDataSet read FResultDataSet write SetResultDataSet;
    /// Values of the last answer.
    property Results: TRALSOAPClientParams read FResults write SetResults;
    /// SOAPAction sent; empty takes the namespace followed by the name.
    property SOAPAction: StringRAL read FSOAPAction write FSOAPAction;
    /// document (literal wrapped) or rpc (literal).
    property Style: TRALSOAPStyle read FStyle write FStyle default ssDocument;
  end;

  /// Operations of a client.
  TRALSOAPClientOperations = class(TOwnedCollection)
  private
    function GetOperation(const AName: StringRAL): TRALSOAPClientOperation;
  public
    /// Collection owned by AOwner.
    constructor Create(AOwner: TPersistent);

    /// The operation named AName, or nil.
    function Find(const AName: StringRAL): TRALSOAPClientOperation;

    /// The operation named AName; raises when there is none.
    property Operation[const AName: StringRAL]: TRALSOAPClientOperation read GetOperation; default;
  end;

  { A TRALClient that speaks SOAP: engines, failover, TLS, pins, pool and
    authentication are the client's own. Operations come from the IDE or ImportWSDL;
    requests go uncompressed unless CompressType says otherwise. }
  TRALSOAPClient = class(TRALClient)
  private
    FEnvelopeContentType: StringRAL;
    FNamespace: StringRAL;
    FOnFault: TRALSOAPOnFault;
    FOnSOAPResponse: TRALSOAPOnResponse;
    FOperations: TRALSOAPClientOperations;
    FServicePath: StringRAL;
    /// Calls running in threads of their own, waited for when the client is freed.
    FThreads: TThreadList;
    FVersion: TRALSOAPVersion;
    FWSDL: StringRAL;

    /// Gives AFault to the events of AOperation; False when no event takes it.
    function ApplyFault(AOperation: TRALSOAPClientOperation; AFault: ERALSOAPFault): boolean;
    /// Fills Response, Results and ResultDataSet of AOperation and fires its events.
    procedure ApplyResponse(AOperation: TRALSOAPClientOperation; AResponse: TRALXMLDocument);
    /// The wrapper element of AOperation built from its Params; the caller frees it.
    function BuildPayload(AOperation: TRALSOAPClientOperation): TRALXMLDocument;
    { The SOAPAction of AOperation; builds APayload when nil, returning in ABuilt
      the document the caller frees. }
    function PrepareCall(const AOperation: StringRAL; var APayload: TRALXMLNode;
      out ABuilt: TRALXMLDocument): StringRAL;
    { Posts the envelope of APayload with AAction and returns the answer, which the
      caller frees; a fault raises ERALSOAPFault. }
    function Send(const AAction: StringRAL; APayload: TRALXMLNode;
      out AStatus: IntegerRAL): TRALXMLDocument;
    procedure SetOperations(const AValue: TRALSOAPClientOperations);
    /// Hands the answer of a finished call thread to its operation, in the main thread.
    procedure ThreadDone(Sender: TObject);
    /// Waits for the call threads still running; their answers are dropped.
    procedure WaitCalls;
  protected
    procedure CopyProperties(ADest: TRALClient); override;
    /// Text of ASource, a URL fetched with this client's settings or a file.
    function FetchText(const ASource: StringRAL): StringRAL;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Calls AOperation with APayload (the operation's wrapper element; nil builds it
      from the operation's Params) and returns the answer envelope, which the caller
      frees; a fault raises ERALSOAPFault. }
    function Call(const AOperation: StringRAL; APayload: TRALXMLNode = nil): TRALXMLDocument;
    /// Calls AOperation with the XML APayloadXML as the content of the Body; as Call.
    function CallXML(const AOperation: StringRAL; const APayloadXML: StringRAL): TRALXMLDocument;
    /// Calls the operation AOperation as TRALSOAPClientOperation.Execute does.
    function Execute(const AOperation: StringRAL;
      AExecBehavior: TRALExecBehavior = ebSingleThread): boolean;
    { Reads the WSDL at ASource (a URL or a file; empty takes WSDL) and fills
      Operations, Namespace, BaseURL and ServicePath. }
    procedure ImportWSDL(const ASource: StringRAL = '');
    { Fills Operations, Namespace, BaseURL and ServicePath from the WSDL text AWSDL;
      its imports are fetched relative to ALocation, where it was read. }
    procedure LoadWSDL(const AWSDL: StringRAL; const ALocation: StringRAL = '');
  published
    { Content type of the envelopes sent; empty follows the version (text/xml for
      1.1, application/soap+xml for 1.2). }
    property EnvelopeContentType: StringRAL read FEnvelopeContentType write FEnvelopeContentType;
    /// Target namespace of the service.
    property Namespace: StringRAL read FNamespace write FNamespace;
    /// Fired with a fault an operation received, when the operation has no OnFault.
    property OnFault: TRALSOAPOnFault read FOnFault write FOnFault;
    /// Fired with each answer, when the operation has no OnResponse.
    property OnSOAPResponse: TRALSOAPOnResponse read FOnSOAPResponse write FOnSOAPResponse;
    /// Operations of the service.
    property Operations: TRALSOAPClientOperations read FOperations write SetOperations;
    /// Path of the service below BaseURL.
    property ServicePath: StringRAL read FServicePath write FServicePath;
    /// Version of SOAP the client speaks.
    property Version: TRALSOAPVersion read FVersion write FVersion default sv11;
    /// Address or file of the WSDL that ImportWSDL reads.
    property WSDL: StringRAL read FWSDL write FWSDL;
  end;

/// Route param type of the XSD type AType (prefix ignored).
function RALSOAPParamTypeOf(const AType: StringRAL): TRALRouteParamType;

implementation

/// AName without its namespace prefix.
function LocalOf(const AName: StringRAL): StringRAL;
var
  vPos: IntegerRAL;
begin
  vPos := Pos(StringRAL(':'), AName);
  if vPos > 0 then
    Result := Copy(AName, vPos + 1, MaxInt)
  else
    Result := AName;
end;

function RALSOAPParamTypeOf(const AType: StringRAL): TRALRouteParamType;
var
  vType: StringRAL;
begin
  vType := LocalOf(AType);
  if vType = 'boolean' then
    Result := prtBoolean
  else if (vType = 'int') or (vType = 'long') or (vType = 'short') or (vType = 'byte') or
          (vType = 'integer') or (vType = 'unsignedInt') or (vType = 'unsignedLong') or
          (vType = 'unsignedShort') or (vType = 'unsignedByte') or
          (vType = 'nonNegativeInteger') or (vType = 'positiveInteger') then
    Result := prtInteger
  else if (vType = 'double') or (vType = 'float') then
    Result := prtNumber
  else if vType = 'decimal' then
    Result := prtDecimal
  else if vType = 'date' then
    Result := prtDate
  else if vType = 'time' then
    Result := prtTime
  else if vType = 'dateTime' then
    Result := prtDateTime
  else if vType = 'base64Binary' then
    Result := prtBase64
  else if vType = 'anyType' then
    Result := prtXML
  else
    Result := prtString;
end;

{ TRALSOAPClientParam }

procedure TRALSOAPClientParam.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALSOAPClientParam then
  begin
    TRALSOAPClientParam(Dest).FName := FName;
    TRALSOAPClientParam(Dest).FParamType := FParamType;
    TRALSOAPClientParam(Dest).FRequired := FRequired;
    TRALSOAPClientParam(Dest).FValue := FValue;
  end
  else
    inherited AssignTo(Dest);
end;

function TRALSOAPClientParam.GetDisplayName: string;
begin
  Result := string(FName);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

{ TRALSOAPClientParams }

constructor TRALSOAPClientParams.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALSOAPClientParam);
end;

function TRALSOAPClientParams.Find(const AName: StringRAL): TRALSOAPClientParam;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to Count - 1 do
    if TRALSOAPClientParam(Items[vInt]).Name = AName then
    begin
      Result := TRALSOAPClientParam(Items[vInt]);
      Exit;
    end;
end;

function TRALSOAPClientParams.AddParam(const AName: StringRAL; const AValue: StringRAL;
  AType: TRALRouteParamType): TRALSOAPClientParam;
begin
  Result := Find(AName);
  if Result = nil then
  begin
    Result := TRALSOAPClientParam(Add);
    Result.Name := AName;
    Result.ParamType := AType;
  end;
  Result.Value := AValue;
end;

function TRALSOAPClientParams.GetParam(const AName: StringRAL): TRALSOAPClientParam;
begin
  Result := Find(AName);
  if Result = nil then
  begin
    Result := TRALSOAPClientParam(Add);
    Result.Name := AName;
  end;
end;

function TRALSOAPClientParams.ValueOf(const AName: StringRAL): StringRAL;
var
  vParam: TRALSOAPClientParam;
begin
  vParam := Find(AName);
  if vParam <> nil then
    Result := vParam.Value
  else
    Result := '';
end;

{ TRALSOAPClientOperation }

constructor TRALSOAPClientOperation.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FParams := TRALSOAPClientParams.Create(Self);
  FResults := TRALSOAPClientParams.Create(Self);
  FStyle := ssDocument;
end;

destructor TRALSOAPClientOperation.Destroy;
begin
  FreeAndNil(FResponse);
  FreeAndNil(FParams);
  FreeAndNil(FResults);
  inherited Destroy;
end;

procedure TRALSOAPClientOperation.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALSOAPClientOperation then
  begin
    TRALSOAPClientOperation(Dest).FName := FName;
    TRALSOAPClientOperation(Dest).FNamespace := FNamespace;
    TRALSOAPClientOperation(Dest).FSOAPAction := FSOAPAction;
    TRALSOAPClientOperation(Dest).FStyle := FStyle;
    TRALSOAPClientOperation(Dest).FOnFault := FOnFault;
    TRALSOAPClientOperation(Dest).FOnResponse := FOnResponse;
    TRALSOAPClientOperation(Dest).FParams.Assign(FParams);
    TRALSOAPClientOperation(Dest).FResults.Assign(FResults);
    TRALSOAPClientOperation(Dest).ResultDataSet := FResultDataSet;
  end
  else
    inherited AssignTo(Dest);
end;

function TRALSOAPClientOperation.GetDisplayName: string;
begin
  Result := string(FName);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TRALSOAPClientOperation.GetClient: TRALSOAPClient;
begin
  Result := nil;
  if (Collection is TRALSOAPClientOperations) and
     (TRALSOAPClientOperations(Collection).Owner is TRALSOAPClient) then
    Result := TRALSOAPClient(TRALSOAPClientOperations(Collection).Owner);
end;

procedure TRALSOAPClientOperation.SetParams(const AValue: TRALSOAPClientParams);
begin
  FParams.Assign(AValue);
end;

procedure TRALSOAPClientOperation.SetResults(const AValue: TRALSOAPClientParams);
begin
  FResults.Assign(AValue);
end;

procedure TRALSOAPClientOperation.SetResultDataSet(const AValue: TDataSet);
begin
  FResultDataSet := AValue;
  if (AValue <> nil) and (Client <> nil) then
    AValue.FreeNotification(Client);
end;

function TRALSOAPClientOperation.EffectiveNamespace: StringRAL;
begin
  Result := FNamespace;
  if (Result = '') and (Client <> nil) then
    Result := Client.Namespace;
end;

function TRALSOAPClientOperation.EffectiveAction: StringRAL;
var
  vNamespace: StringRAL;
begin
  Result := FSOAPAction;
  if Result <> '' then
    Exit;
  vNamespace := EffectiveNamespace;
  if (vNamespace <> '') and (vNamespace[Length(vNamespace)] in ['/', '#', ':']) then
    Result := vNamespace + FName
  else if vNamespace <> '' then
    Result := vNamespace + '/' + FName
  else
    Result := FName;
end;

function TRALSOAPClientOperation.Execute(AExecBehavior: TRALExecBehavior): boolean;
begin
  if Client = nil then
    raise ERALSOAPFault.CreateFault(sfcSender, StringRAL(emSOAPNoClient));
  Result := Client.Execute(FName, AExecBehavior);
end;

{ TRALSOAPClientOperations }

constructor TRALSOAPClientOperations.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALSOAPClientOperation);
end;

function TRALSOAPClientOperations.Find(const AName: StringRAL): TRALSOAPClientOperation;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to Count - 1 do
    if TRALSOAPClientOperation(Items[vInt]).Name = AName then
    begin
      Result := TRALSOAPClientOperation(Items[vInt]);
      Exit;
    end;
end;

function TRALSOAPClientOperations.GetOperation(const AName: StringRAL): TRALSOAPClientOperation;
begin
  Result := Find(AName);
  if Result = nil then
    raise ERALSOAPFault.CreateFault(sfcSender,
      StringRAL(Format(emSOAPOperationNotFound, [string(AName), ''])));
end;

type
  { The documents of one WSDL: the definitions it imports and the schemas its types
    import or include, each fetched once. }
  TRALWSDLContext = class
  private
    /// Client whose settings fetch the imported documents.
    FClient: TRALSOAPClient;
    FDefinitions: TList;
    /// Documents loaded, freed with the context.
    FDocuments: TList;
    /// Schema elements of every document, the inline ones first.
    FSchemas: TList;
    /// Locations already fetched, so an import cycle ends.
    FSeen: TStringList;

    /// Parses AText into a document the context keeps.
    function Load(const AText: StringRAL): TRALXMLDocument;
    /// ALocation made absolute against ABase, a URL or a file.
    function Resolve(const ALocation, ABase: StringRAL): StringRAL;
  public
    /// Context that fetches through AClient.
    constructor Create(AClient: TRALSOAPClient);
    destructor Destroy; override;

    /// Adds the definitions of ADoc, read from ALocation, and what they import.
    procedure AddDefinitions(ADefs: TRALXMLNode; const ALocation: StringRAL);
    /// Adds the schema ASchema, read from ALocation, and what it imports or includes.
    procedure AddSchema(ASchema: TRALXMLNode; const ALocation: StringRAL);
    /// The child ALocal named AName (prefix ignored) of any of the definitions.
    function FindDefinition(const ALocal, AName: StringRAL): TRALXMLNode;
    /// The top-level ALocal named AName (prefix ignored) of any of the schemas.
    function FindSchemaItem(const ALocal, AName: StringRAL): TRALXMLNode;

    /// Definitions elements, the main one first.
    property Definitions: TList read FDefinitions;
  end;

const
  /// Most documents one WSDL may bring in through its imports.
  cWSDLMaxDocuments = 64;

/// The child ALocal of AParent whose name attribute is AName, prefix ignored.
function ChildByName(AParent: TRALXMLNode; const ALocal, AName: StringRAL): TRALXMLNode;
begin
  Result := AParent.FirstElement;
  while Result <> nil do
  begin
    if (Result.LocalName = ALocal) and (Result.Attribute('name') = LocalOf(AName)) then
      Exit;
    Result := Result.NextElement;
  end;
end;

{ TRALWSDLContext }

constructor TRALWSDLContext.Create(AClient: TRALSOAPClient);
begin
  inherited Create;
  FClient := AClient;
  FDefinitions := TList.Create;
  FDocuments := TList.Create;
  FSchemas := TList.Create;
  FSeen := TStringList.Create;
end;

destructor TRALWSDLContext.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FDocuments.Count - 1 do
    TObject(FDocuments[vInt]).Free;
  FreeAndNil(FDocuments);
  FreeAndNil(FDefinitions);
  FreeAndNil(FSchemas);
  FreeAndNil(FSeen);
  inherited Destroy;
end;

function TRALWSDLContext.Load(const AText: StringRAL): TRALXMLDocument;
begin
  Result := TRALXMLDocument.Create;
  FDocuments.Add(Result);
  Result.LoadFromString(AText);
end;

function TRALWSDLContext.Resolve(const ALocation, ABase: StringRAL): StringRAL;
var
  vPos, vSlash: IntegerRAL;
  vBase: StringRAL;
begin
  if (ALocation = '') or (Pos(StringRAL('://'), ALocation) > 0) or (ABase = '') then
  begin
    Result := ALocation;
    Exit;
  end;

  vBase := ABase;
  vPos := Pos(StringRAL('?'), vBase);
  if vPos > 0 then
    vBase := Copy(vBase, 1, vPos - 1);

  if Pos(StringRAL('://'), vBase) = 0 then
  begin
    // a file: relative to its folder
    Result := StringRAL(ExtractFilePath(string(vBase))) + ALocation;
    Exit;
  end;

  if ALocation[1] = '?' then
    Result := vBase + ALocation
  else if ALocation[1] = '/' then
  begin
    vPos := Pos(StringRAL('://'), vBase) + 3;
    while (vPos <= Length(vBase)) and (vBase[vPos] <> '/') do
      Inc(vPos);
    Result := Copy(vBase, 1, vPos - 1) + ALocation;
  end
  else
  begin
    vSlash := 0;
    for vPos := Length(vBase) downto 1 do
      if vBase[vPos] = '/' then
      begin
        vSlash := vPos;
        Break;
      end;
    Result := Copy(vBase, 1, vSlash) + ALocation;
  end;
end;

procedure TRALWSDLContext.AddDefinitions(ADefs: TRALXMLNode; const ALocation: StringRAL);
var
  vChild, vSchema: TRALXMLNode;
  vLocation: StringRAL;
  vDoc: TRALXMLDocument;
begin
  FDefinitions.Add(ADefs);
  vChild := ADefs.FirstElement;
  while vChild <> nil do
  begin
    if vChild.LocalName = 'types' then
    begin
      vSchema := vChild.FirstElement;
      while vSchema <> nil do
      begin
        if vSchema.LocalName = 'schema' then
          AddSchema(vSchema, ALocation);
        vSchema := vSchema.NextElement;
      end;
    end
    else if (vChild.LocalName = 'import') and (vChild.Attribute('location') <> '') then
    begin
      vLocation := Resolve(vChild.Attribute('location'), ALocation);
      if (vLocation <> '') and (FSeen.IndexOf(string(vLocation)) < 0) and
         (FDocuments.Count < cWSDLMaxDocuments) then
      begin
        FSeen.Add(string(vLocation));
        vDoc := Load(FClient.FetchText(vLocation));
        if (vDoc.Root <> nil) and (vDoc.Root.LocalName = 'definitions') then
          AddDefinitions(vDoc.Root, vLocation)
        else if (vDoc.Root <> nil) and (vDoc.Root.LocalName = 'schema') then
          AddSchema(vDoc.Root, vLocation);
      end;
    end;
    vChild := vChild.NextElement;
  end;
end;

procedure TRALWSDLContext.AddSchema(ASchema: TRALXMLNode; const ALocation: StringRAL);
var
  vChild: TRALXMLNode;
  vLocation: StringRAL;
  vDoc: TRALXMLDocument;
begin
  FSchemas.Add(ASchema);
  vChild := ASchema.FirstElement;
  while vChild <> nil do
  begin
    if ((vChild.LocalName = 'import') or (vChild.LocalName = 'include')) and
       (vChild.Attribute('schemaLocation') <> '') then
    begin
      vLocation := Resolve(vChild.Attribute('schemaLocation'), ALocation);
      if (vLocation <> '') and (FSeen.IndexOf(string(vLocation)) < 0) and
         (FDocuments.Count < cWSDLMaxDocuments) then
      begin
        FSeen.Add(string(vLocation));
        vDoc := Load(FClient.FetchText(vLocation));
        if (vDoc.Root <> nil) and (vDoc.Root.LocalName = 'schema') then
          AddSchema(vDoc.Root, vLocation);
      end;
    end;
    vChild := vChild.NextElement;
  end;
end;

function TRALWSDLContext.FindDefinition(const ALocal, AName: StringRAL): TRALXMLNode;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to FDefinitions.Count - 1 do
  begin
    Result := ChildByName(TRALXMLNode(FDefinitions[vInt]), ALocal, AName);
    if Result <> nil then
      Exit;
  end;
end;

function TRALWSDLContext.FindSchemaItem(const ALocal, AName: StringRAL): TRALXMLNode;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to FSchemas.Count - 1 do
  begin
    Result := ChildByName(TRALXMLNode(FSchemas[vInt]), ALocal, AName);
    if Result <> nil then
      Exit;
  end;
end;

type
  { Sends one call of a client off the calling thread; OnTerminate, in the main
    thread, hands the answer or the fault back to the client. }
  TRALSOAPCallThread = class(TThread)
  private
    /// SOAPAction sent.
    FAction: StringRAL;
    /// Client that sends the call.
    FClient: TRALSOAPClient;
    /// Fault received, or the transport failure as a Receiver fault.
    FFault: ERALSOAPFault;
    /// Name of the operation answered.
    FOperation: StringRAL;
    /// Wrapper element sent, built in the calling thread.
    FPayload: TRALXMLDocument;
    /// False once the client stops waiting for the answer.
    FReport: boolean;
    /// Answer envelope, handed to the operation.
    FResponse: TRALXMLDocument;
  protected
    procedure Execute; override;
  public
    /// Suspended thread that sends APayload, which it then owns, for AOperation.
    constructor Create(AClient: TRALSOAPClient; const AOperation, AAction: StringRAL;
      APayload: TRALXMLDocument);
    destructor Destroy; override;
  end;

{ TRALSOAPCallThread }

constructor TRALSOAPCallThread.Create(AClient: TRALSOAPClient; const AOperation,
  AAction: StringRAL; APayload: TRALXMLDocument);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FClient := AClient;
  FReport := True;
  FOperation := AOperation;
  FAction := AAction;
  FPayload := APayload;
  OnTerminate := {$IFDEF FPC}@{$ENDIF}AClient.ThreadDone;
end;

destructor TRALSOAPCallThread.Destroy;
begin
  FreeAndNil(FPayload);
  FreeAndNil(FResponse);
  FreeAndNil(FFault);
  inherited Destroy;
end;

procedure TRALSOAPCallThread.Execute;
var
  vStatus: IntegerRAL;
begin
  try
    FResponse := FClient.Send(FAction, FPayload.Root, vStatus);
  except
    on E: ERALSOAPFault do
      FFault := ERALSOAPFault(AcquireExceptionObject);
    // a transport failure has no envelope: it arrives as a Receiver fault
    on E: Exception do
      FFault := ERALSOAPFault.CreateFault(sfcReceiver, StringRAL(E.Message));
  end;
end;

{ TRALSOAPClient }

constructor TRALSOAPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreads := TThreadList.Create;
  FOperations := TRALSOAPClientOperations.Create(Self);
  FVersion := sv11;
  // SOAP servers rarely read a compressed request; answers may still come compressed
  CompressType := ctNone;
end;

destructor TRALSOAPClient.Destroy;
begin
  WaitCalls;
  FreeAndNil(FThreads);
  FreeAndNil(FOperations);
  inherited Destroy;
end;

procedure TRALSOAPClient.SetOperations(const AValue: TRALSOAPClientOperations);
begin
  FOperations.Assign(AValue);
end;

procedure TRALSOAPClient.CopyProperties(ADest: TRALClient);
begin
  inherited CopyProperties(ADest);
  if ADest is TRALSOAPClient then
  begin
    TRALSOAPClient(ADest).EnvelopeContentType := FEnvelopeContentType;
    TRALSOAPClient(ADest).Namespace := FNamespace;
    TRALSOAPClient(ADest).ServicePath := FServicePath;
    TRALSOAPClient(ADest).Version := FVersion;
    TRALSOAPClient(ADest).WSDL := FWSDL;
    TRALSOAPClient(ADest).OnFault := FOnFault;
    TRALSOAPClient(ADest).OnSOAPResponse := FOnSOAPResponse;
    TRALSOAPClient(ADest).Operations.Assign(FOperations);
  end;
end;

procedure TRALSOAPClient.Notification(AComponent: TComponent; Operation: TOperation);
var
  vInt: IntegerRAL;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TDataSet) and (FOperations <> nil) then
    for vInt := 0 to FOperations.Count - 1 do
      if TRALSOAPClientOperation(FOperations.Items[vInt]).ResultDataSet = AComponent then
        TRALSOAPClientOperation(FOperations.Items[vInt]).ResultDataSet := nil;
end;

function TRALSOAPClient.BuildPayload(AOperation: TRALSOAPClientOperation): TRALXMLDocument;
var
  vNode, vChild: TRALXMLNode;
  vInt: IntegerRAL;
  vParam: TRALSOAPClientParam;
  vNamespace: StringRAL;
begin
  Result := TRALXMLDocument.Create;
  try
    vNamespace := AOperation.EffectiveNamespace;
    if AOperation.Style = ssRPC then
    begin
      // rpc/literal: the wrapper is qualified, the parts are not
      vNode := Result.CreateRoot('tns:' + AOperation.Name);
      vNode.AddNamespace('tns', vNamespace);
    end
    else
    begin
      vNode := Result.CreateRoot(AOperation.Name);
      if vNamespace <> '' then
        vNode.AddNamespace('', vNamespace);
    end;

    for vInt := 0 to AOperation.Params.Count - 1 do
    begin
      vParam := TRALSOAPClientParam(AOperation.Params.Items[vInt]);
      // an optional value left empty is not sent
      if (vParam.Value = '') and not vParam.Required then
        Continue;
      vChild := vNode.AddChild(vParam.Name);
      if vParam.ParamType = prtXML then
        vChild.AddXML(vParam.Value)
      else
        vChild.WithText(vParam.Value);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TRALSOAPClient.Send(const AAction: StringRAL; APayload: TRALXMLNode;
  out AStatus: IntegerRAL): TRALXMLDocument;
var
  vEnvelope: TRALXMLDocument;
  vResponse: TRALResponse;
  vType, vText: StringRAL;
  vFault: ERALSOAPFault;
begin
  vEnvelope := TRALXMLDocument.Create;
  try
    vEnvelope.CreateRoot('soap:Envelope').AddNamespace('soap', RALSOAPNamespace(FVersion))
      .AddChild('soap:Body').AddXML(APayload.ToXML(False));
    vText := vEnvelope.SaveToString(True);
  finally
    vEnvelope.Free;
  end;

  vType := FEnvelopeContentType;
  if vType = '' then
  begin
    vType := RALSOAPContentType(FVersion);
    // SOAP 1.2 carries the action in the content type (RFC 3902)
    if (FVersion = sv12) and (AAction <> '') then
      vType := vType + '; action="' + AAction + '"';
  end;

  Request.Params.ClearParams(rpkBODY);
  Request.ContentType := vType;
  if FVersion = sv11 then
    Request.AddHeader('SOAPAction', '"' + AAction + '"')
  else
    Request.Params.DelParam('SOAPAction', rpkHEADER);
  Request.AddBody(vText, vType);

  vResponse := nil;
  try
    Post(FServicePath, vResponse);
    AStatus := vResponse.StatusCode;
    vText := vResponse.ResponseText;
  finally
    vResponse.Free;
  end;

  Result := TRALXMLDocument.Create;
  try
    if vText = '' then
      raise ERALSOAPFault.CreateFault(sfcReceiver,
        StringRAL(Format(emSOAPEmptyAnswer, [AStatus])));
    try
      Result.LoadFromString(vText);
    except
      on E: ERALXMLError do
        raise ERALSOAPFault.CreateFault(sfcReceiver, StringRAL(Format(emSOAPInvalidAnswer,
          [AStatus, E.Message])));
    end;
    vFault := RALSOAPReadFault(Result);
    if vFault <> nil then
    begin
      vFault.HTTPStatus := AStatus;
      raise vFault;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TRALSOAPClient.PrepareCall(const AOperation: StringRAL; var APayload: TRALXMLNode;
  out ABuilt: TRALXMLDocument): StringRAL;
var
  vOperation: TRALSOAPClientOperation;
  vNamespace: StringRAL;
begin
  ABuilt := nil;
  vOperation := FOperations.Find(AOperation);
  if vOperation <> nil then
  begin
    Result := vOperation.EffectiveAction;
    if APayload = nil then
    begin
      ABuilt := BuildPayload(vOperation);
      APayload := ABuilt.Root;
    end;
    Exit;
  end;

  // an operation not in the list: the namespace and the name make its action
  vNamespace := FNamespace;
  if (vNamespace <> '') and (vNamespace[Length(vNamespace)] in ['/', '#', ':']) then
    Result := vNamespace + AOperation
  else if vNamespace <> '' then
    Result := vNamespace + '/' + AOperation
  else
    Result := AOperation;
  if APayload = nil then
  begin
    ABuilt := TRALXMLDocument.Create;
    APayload := ABuilt.CreateRoot(AOperation);
    if vNamespace <> '' then
      APayload.AddNamespace('', vNamespace);
  end;
end;

function TRALSOAPClient.Call(const AOperation: StringRAL; APayload: TRALXMLNode): TRALXMLDocument;
var
  vBuilt: TRALXMLDocument;
  vAction: StringRAL;
  vStatus: IntegerRAL;
begin
  vAction := PrepareCall(AOperation, APayload, vBuilt);
  try
    Result := Send(vAction, APayload, vStatus);
  finally
    vBuilt.Free;
  end;
end;

function TRALSOAPClient.CallXML(const AOperation: StringRAL;
  const APayloadXML: StringRAL): TRALXMLDocument;
var
  vDoc: TRALXMLDocument;
begin
  vDoc := TRALXMLDocument.Create;
  try
    vDoc.LoadFromString(APayloadXML);
    Result := Call(AOperation, vDoc.Root);
  finally
    vDoc.Free;
  end;
end;

function TRALSOAPClient.ApplyFault(AOperation: TRALSOAPClientOperation;
  AFault: ERALSOAPFault): boolean;
begin
  AOperation.FLastError := AFault.Reason;
  Result := True;
  if Assigned(AOperation.OnFault) then
    AOperation.OnFault(Self, AOperation, AFault)
  else if Assigned(FOnFault) then
    FOnFault(Self, AOperation, AFault)
  else
    Result := False;
end;

procedure TRALSOAPClient.ApplyResponse(AOperation: TRALSOAPClientOperation;
  AResponse: TRALXMLDocument);
var
  vPayload, vChild: TRALXMLNode;
  vParam: TRALSOAPClientParam;
begin
  FreeAndNil(AOperation.FResponse);
  AOperation.FResponse := AResponse;
  AOperation.FLastError := '';

  // the answer's values, by the local names of the wrapper's children
  AOperation.Results.Clear;
  vPayload := nil;
  if (AResponse.Root <> nil) and (AResponse.Root.Child('Body') <> nil) then
    vPayload := AResponse.Root.Child('Body').FirstElement;
  if vPayload <> nil then
  begin
    vChild := vPayload.FirstElement;
    while vChild <> nil do
    begin
      if not RALStorageXMLIsRoot(vChild.LocalName) then
      begin
        vParam := AOperation.Results.AddParam(vChild.LocalName);
        if vChild.FirstElement <> nil then
        begin
          vParam.ParamType := prtXML;
          vParam.Value := vChild.InnerText;
        end
        else
          vParam.Value := vChild.Text;
      end;
      vChild := vChild.NextElement;
    end;

    if (AOperation.ResultDataSet <> nil) and (vPayload.FirstElement <> nil) then
      vPayload.ReadDataSet(AOperation.ResultDataSet);
  end;

  if Assigned(AOperation.OnResponse) then
    AOperation.OnResponse(Self, AOperation, AResponse)
  else if Assigned(FOnSOAPResponse) then
    FOnSOAPResponse(Self, AOperation, AResponse);
end;

function TRALSOAPClient.Execute(const AOperation: StringRAL;
  AExecBehavior: TRALExecBehavior): boolean;
var
  vOperation: TRALSOAPClientOperation;
  vResponse: TRALXMLDocument;
  vBuilt: TRALXMLDocument;
  vPayload: TRALXMLNode;
  vAction: StringRAL;
  vThread: TRALSOAPCallThread;
begin
  vOperation := FOperations.Operation[AOperation];

  if AExecBehavior = ebMultiThread then
  begin
    // the payload is read from the operation here, never by the other thread
    vPayload := nil;
    vAction := PrepareCall(AOperation, vPayload, vBuilt);
    vThread := TRALSOAPCallThread.Create(Self, AOperation, vAction, vBuilt);
    FThreads.Add(vThread);
    vThread.Start;
    Result := True;
    Exit;
  end;

  Result := False;
  try
    vResponse := Call(AOperation);
  except
    on E: ERALSOAPFault do
    begin
      if not ApplyFault(vOperation, E) then
        raise;
      Exit;
    end;
  end;
  ApplyResponse(vOperation, vResponse);
  Result := True;
end;

procedure TRALSOAPClient.ThreadDone(Sender: TObject);
var
  vThread: TRALSOAPCallThread;
  vOperation: TRALSOAPClientOperation;
  vList: TList;
  vInt: IntegerRAL;
begin
  vThread := TRALSOAPCallThread(Sender);
  vList := FThreads.LockList;
  try
    vInt := vList.IndexOf(vThread);
    if vInt >= 0 then
      vList.Delete(vInt);
  finally
    FThreads.UnlockList;
  end;
  if not vThread.FReport then
    Exit;
  vOperation := FOperations.Find(vThread.FOperation);
  if vOperation = nil then
    Exit;
  if vThread.FFault <> nil then
    // with no handler there is nobody to raise it to: LastError keeps the reason
    ApplyFault(vOperation, vThread.FFault)
  else if vThread.FResponse <> nil then
  begin
    ApplyResponse(vOperation, vThread.FResponse);
    vThread.FResponse := nil;
  end;
end;

procedure TRALSOAPClient.WaitCalls;
var
  vList: TList;
  vInt, vPending, vRemaining: IntegerRAL;
  vMainThread: boolean;
begin
  if FThreads = nil then
    Exit;
  vList := FThreads.LockList;
  try
    // a call that ends while the client is freed reports to nobody
    for vInt := 0 to vList.Count - 1 do
      TRALSOAPCallThread(vList[vInt]).FReport := False;
    vPending := vList.Count;
  finally
    FThreads.UnlockList;
  end;

  // OnTerminate is delivered through Synchronize: the main thread pumps it here
  vMainThread := {$IF (DEFINED(FPC)) OR (NOT DEFINED(DELPHIXE3UP))}TThread.CurrentThread.ThreadID{$ELSE}TThread.Current.ThreadID{$IFEND} = MainThreadID;
  vRemaining := ConnectTimeout + RequestTimeout + 1000;
  while (vPending > 0) and (vRemaining > 0) do
  begin
    if vMainThread then
      CheckSynchronize(10)
    else
      Sleep(10);
    Dec(vRemaining, 10);
    vList := FThreads.LockList;
    try
      vPending := vList.Count;
    finally
      FThreads.UnlockList;
    end;
  end;
end;

function TRALSOAPClient.FetchText(const ASource: StringRAL): StringRAL;
var
  vClient: TRALClient;
  vResponse: TRALResponse;
  vStream: TMemoryStream;
  vBase, vPath: StringRAL;
  vPos: IntegerRAL;
begin
  if (Copy(LowerCase(string(ASource)), 1, 7) = 'http://') or
     (Copy(LowerCase(string(ASource)), 1, 8) = 'https://') then
  begin
    // a URL: fetched with this client's settings, the engine included
    vPos := Pos(StringRAL('://'), ASource) + 3;
    while (vPos <= Length(ASource)) and (ASource[vPos] <> '/') do
      Inc(vPos);
    vBase := Copy(ASource, 1, vPos - 1);
    vPath := Copy(ASource, vPos, MaxInt);
    if vPath = '' then
      vPath := '/';

    vClient := Clone(nil);
    try
      vClient.BaseURL.Text := string(vBase);
      vResponse := nil;
      try
        vClient.Get(vPath, vResponse);
        if vResponse.StatusCode <> HTTP_OK then
          raise ERALSOAPFault.CreateFault(sfcReceiver,
            StringRAL(Format(emSOAPEmptyAnswer, [vResponse.StatusCode])));
        Result := vResponse.ResponseText;
      finally
        vResponse.Free;
      end;
    finally
      vClient.Free;
    end;
  end
  else
  begin
    vStream := TMemoryStream.Create;
    try
      vStream.LoadFromFile(string(ASource));
      Result := RALXMLMakeString(vStream.Memory, vStream.Size);
    finally
      vStream.Free;
    end;
  end;
end;

procedure TRALSOAPClient.ImportWSDL(const ASource: StringRAL);
var
  vSource: StringRAL;
begin
  vSource := ASource;
  if vSource = '' then
    vSource := FWSDL;
  if vSource = '' then
    raise ERALSOAPFault.CreateFault(sfcSender, StringRAL(emSOAPNoWSDL));
  LoadWSDL(FetchText(vSource), vSource);
  if FWSDL = '' then
    FWSDL := vSource;
end;

procedure TRALSOAPClient.LoadWSDL(const AWSDL: StringRAL; const ALocation: StringRAL);
var
  vContext: TRALWSDLContext;
  vDoc: TRALXMLDocument;
  vDefs, vPortType, vNode, vPort, vAddress, vChild: TRALXMLNode;
  vBindingName, vSoapNs, vLocation: StringRAL;
  vBindings: TList;
  vInt, vPos: IntegerRAL;

  // Adds to AParams the elements of the sequence of the complex type AType
  procedure ReadComplexType(AType: TRALXMLNode; AParams: TRALSOAPClientParams;
    ADepth: IntegerRAL);
  var
    vSeq, vItem, vRef: TRALXMLNode;
    vParam: TRALSOAPClientParam;
    vName, vType: StringRAL;
  begin
    if (AType = nil) or (ADepth > 8) then
      Exit;
    // an extension adds its own elements after those of its base
    vSeq := AType.GetNode('complexContent/extension');
    if vSeq <> nil then
    begin
      ReadComplexType(vContext.FindSchemaItem('complexType', vSeq.Attribute('base')),
        AParams, ADepth + 1);
      AType := vSeq;
    end;
    vSeq := AType.Child('sequence');
    if vSeq = nil then
      vSeq := AType.Child('all');
    if vSeq = nil then
      Exit;
    vItem := vSeq.FirstElement;
    while vItem <> nil do
    begin
      if vItem.LocalName = 'element' then
      begin
        vName := vItem.Attribute('name');
        vType := vItem.Attribute('type');
        if (vName = '') and (vItem.Attribute('ref') <> '') then
        begin
          vRef := vContext.FindSchemaItem('element', vItem.Attribute('ref'));
          vName := LocalOf(vItem.Attribute('ref'));
          if vRef <> nil then
            vType := vRef.Attribute('type');
        end;
        if vName <> '' then
        begin
          // a complex child travels as XML
          if (vType = '') and (vItem.Child('complexType') <> nil) then
            vType := 'anyType'
          else if (vType <> '') and (vContext.FindSchemaItem('complexType', vType) <> nil) then
            vType := 'anyType';
          vParam := AParams.AddParam(vName, '', RALSOAPParamTypeOf(vType));
          vParam.Required := (vItem.Attribute('minOccurs') <> '0') and
            (vItem.Attribute('nillable') <> 'true');
        end;
      end;
      vItem := vItem.NextElement;
    end;
  end;

  // Adds to AParams the content of the wrapper element named AName
  procedure ReadElement(const AName: StringRAL; AParams: TRALSOAPClientParams);
  var
    vItem: TRALXMLNode;
  begin
    vItem := vContext.FindSchemaItem('element', AName);
    if vItem = nil then
      Exit;
    if vItem.Child('complexType') <> nil then
      ReadComplexType(vItem.Child('complexType'), AParams, 0)
    else if vItem.Attribute('type') <> '' then
      ReadComplexType(vContext.FindSchemaItem('complexType', vItem.Attribute('type')),
        AParams, 0);
  end;

  // Adds to AParams the parts of the message AMessage (rpc), or its wrapper's content
  procedure ReadMessage(const AMessage: StringRAL; AParams: TRALSOAPClientParams);
  var
    vM, vP: TRALXMLNode;
    vType: StringRAL;
  begin
    vM := vContext.FindDefinition('message', AMessage);
    if vM = nil then
      Exit;
    vP := vM.FirstElement;
    while vP <> nil do
    begin
      if vP.LocalName = 'part' then
      begin
        if vP.Attribute('element') <> '' then
          ReadElement(vP.Attribute('element'), AParams)
        else
        begin
          vType := vP.Attribute('type');
          if vContext.FindSchemaItem('complexType', vType) <> nil then
            vType := 'anyType';
          // an rpc part is always sent
          AParams.AddParam(vP.Attribute('name'), '', RALSOAPParamTypeOf(vType)).Required := True;
        end;
      end;
      vP := vP.NextElement;
    end;
  end;

  { The SOAP bindings of the version the client speaks, else every SOAP binding: a
    service may split its operations among bindings (one per style) }
  procedure FindBindings(AList: TList);
  var
    vI, vPass: IntegerRAL;
    vC: TRALXMLNode;
  begin
    for vPass := 0 to 1 do
    begin
      for vI := 0 to vContext.Definitions.Count - 1 do
      begin
        vC := TRALXMLNode(vContext.Definitions[vI]).FirstElement;
        while vC <> nil do
        begin
          if (vC.LocalName = 'binding') and
             (((vPass = 0) and (vC.ChildNS(vSoapNs, 'binding') <> nil)) or
              ((vPass = 1) and (vC.Child('binding') <> nil))) then
            AList.Add(vC);
          vC := vC.NextElement;
        end;
      end;
      if AList.Count > 0 then
        Exit;
    end;
  end;

  // Adds the operations of APortType, with what ABinding (or nil) says of each
  procedure ReadPortType(APortType, ABinding: TRALXMLNode);
  var
    vStyle: StringRAL;
    vNode, vMsg, vBindOp, vSoapOp, vElement: TRALXMLNode;
    vOperation: TRALSOAPClientOperation;
  begin
    vStyle := '';
    if (ABinding <> nil) and (ABinding.Child('binding') <> nil) then
      vStyle := ABinding.Child('binding').Attribute('style');
    vNode := APortType.FirstElement;
    while vNode <> nil do
    begin
      if (vNode.LocalName = 'operation') and
         (FOperations.Find(vNode.Attribute('name')) = nil) then
      begin
        vOperation := TRALSOAPClientOperation(FOperations.Add);
        vOperation.Name := vNode.Attribute('name');
        vMsg := vNode.Child('input');
        if vMsg <> nil then
          ReadMessage(vMsg.Attribute('message'), vOperation.Params);
        vMsg := vNode.Child('output');
        if vMsg <> nil then
          ReadMessage(vMsg.Attribute('message'), vOperation.Results);

        if vStyle = 'rpc' then
          vOperation.Style := ssRPC;
        if ABinding <> nil then
        begin
          vBindOp := ChildByName(ABinding, 'operation', vOperation.Name);
          if vBindOp <> nil then
          begin
            vSoapOp := vBindOp.Child('operation');
            if vSoapOp <> nil then
            begin
              vOperation.SOAPAction := vSoapOp.Attribute('soapAction');
              if vSoapOp.Attribute('style') = 'rpc' then
                vOperation.Style := ssRPC
              else if vSoapOp.Attribute('style') = 'document' then
                vOperation.Style := ssDocument;
            end;
            // rpc/literal: the wrapper's namespace is the one of soap:body
            vElement := vBindOp.GetNode('input/body');
            if (vElement <> nil) and (vElement.Attribute('namespace') <> '') and
               (vElement.Attribute('namespace') <> FNamespace) then
              vOperation.Namespace := vElement.Attribute('namespace');
          end;
        end;
      end;
      vNode := vNode.NextElement;
    end;
  end;

begin
  vContext := TRALWSDLContext.Create(Self);
  vBindings := TList.Create;
  try
    vDoc := vContext.Load(AWSDL);
    vDefs := vDoc.Root;
    if (vDefs = nil) or (vDefs.LocalName <> 'definitions') then
      raise ERALSOAPFault.CreateFault(sfcSender, StringRAL(emSOAPInvalidWSDL));
    vContext.AddDefinitions(vDefs, ALocation);
    FNamespace := vDefs.Attribute('targetNamespace');

    if FVersion = sv12 then
      vSoapNs := RALWSDLSOAP12Namespace
    else
      vSoapNs := RALWSDLSOAP11Namespace;
    FindBindings(vBindings);

    vPortType := nil;
    if vBindings.Count > 0 then
      vPortType := vContext.FindDefinition('portType',
        TRALXMLNode(vBindings[0]).Attribute('type'));
    if vPortType = nil then
      for vInt := 0 to vContext.Definitions.Count - 1 do
      begin
        vPortType := TRALXMLNode(vContext.Definitions[vInt]).Child('portType');
        if vPortType <> nil then
          Break;
      end;
    if vPortType = nil then
      raise ERALSOAPFault.CreateFault(sfcSender, StringRAL(emSOAPInvalidWSDL));
    // the operations belong to the namespace of the port type's definitions
    if (vPortType.Parent <> nil) and (vPortType.Parent.Attribute('targetNamespace') <> '') then
      FNamespace := vPortType.Parent.Attribute('targetNamespace');

    FOperations.Clear;
    if vBindings.Count = 0 then
      ReadPortType(vPortType, nil)
    else
      for vInt := 0 to vBindings.Count - 1 do
      begin
        vNode := vContext.FindDefinition('portType',
          TRALXMLNode(vBindings[vInt]).Attribute('type'));
        if vNode <> nil then
          ReadPortType(vNode, TRALXMLNode(vBindings[vInt]));
      end;
    vBindingName := '';
    if vBindings.Count > 0 then
      vBindingName := TRALXMLNode(vBindings[0]).Attribute('name');

    // the address of the port that uses the chosen binding
    vLocation := '';
    for vInt := 0 to vContext.Definitions.Count - 1 do
    begin
      vChild := TRALXMLNode(vContext.Definitions[vInt]).FirstElement;
      while (vChild <> nil) and (vLocation = '') do
      begin
        if vChild.LocalName = 'service' then
        begin
          vPort := vChild.FirstElement;
          while (vPort <> nil) and (vLocation = '') do
          begin
            if (vPort.LocalName = 'port') and ((vBindingName = '') or
               (LocalOf(vPort.Attribute('binding')) = vBindingName)) then
            begin
              vAddress := vPort.Child('address');
              if vAddress <> nil then
                vLocation := vAddress.Attribute('location');
            end;
            vPort := vPort.NextElement;
          end;
        end;
        vChild := vChild.NextElement;
      end;
      if vLocation <> '' then
        Break;
    end;

    if vLocation <> '' then
    begin
      vPos := Pos(StringRAL('://'), vLocation);
      if vPos > 0 then
      begin
        vPos := vPos + 3;
        while (vPos <= Length(vLocation)) and (vLocation[vPos] <> '/') do
          Inc(vPos);
        BaseURL.Text := string(Copy(vLocation, 1, vPos - 1));
        FServicePath := Copy(vLocation, vPos, MaxInt);
        if FServicePath = '' then
          FServicePath := '/';
      end;
    end;
  finally
    vBindings.Free;
    vContext.Free;
  end;
end;

end.
