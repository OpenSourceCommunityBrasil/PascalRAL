unit RALfpHTTPServer;

interface

uses
  Classes, SysUtils, syncobjs,
  fphttpserver, sslbase, fpHTTP, fphttpapp, httpprotocol,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams;

type

  { TRALfpHTTPCertData }

  TRALfpHTTPCertData = class(TCertificateData)
  private
    function GetFileName(AIndex: Integer) : string;
    procedure SetFileName(AIndex: Integer; AValue : string);
  published
    property KeyPassword;
    property CipherList;
    Property HostName;
    property CertificateFile: string Index 0 read GetFileName write SetFileName;
    property TrustCertificateFile: string Index 1 read GetFileName write SetFileName;
    property PrivateKeyFile: string Index 2 read GetFileName write SetFileName;
    property PFXFile: string Index 3 read GetFileName write SetFileName;
    property CertCAFile: string Index 4 read GetFileName write SetFileName;
  end;

  TRALfpHTTPSSL = class(TRALSSL)
  private
    FSSLOptions: TRALfpHTTPCertData;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property SSLOptions: TRALfpHTTPCertData read FSSLOptions write FSSLOptions;
  end;

  TRALfpHttpServer = class;

  { TRALfpHttpServerThread }

  TRALfpHttpServerThread = class(TThread)
  private
    FParent: TRALfpHttpServer;
    FHttp: TFPHttpServer;
    FEvent: TSimpleEvent;
  protected
    function GetActive: boolean;
    procedure SetActive(AValue: boolean);

    function GetPort: IntegerRAL;
    procedure SetPort(AValue: IntegerRAL);

    function GetSessionTimeout : IntegerRAL;
    procedure SetSessionTimeout(const AValue: IntegerRAL);

    procedure DecodeAuth(ARequest: TFPHTTPConnectionRequest; AResult: TRALRequest);
    procedure EncodeBody(AResponse: TRALResponse; AResponseInfo: TFPHTTPConnectionResponse);

    procedure OnCommandProcess(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
                               var AResponse: TFPHTTPConnectionResponse);

    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create(AOwner: TRALfpHttpServer);
    destructor Destroy; override;
  published
    property Active: boolean read GetActive write SetActive;
    property Port: IntegerRAL read GetPort write SetPort;
    property SessionTimeout: IntegerRAL read GetSessionTimeout write SetSessionTimeout;
  end;

  TRALfpHttpServer = class(TRALServer)
  private
    FHttpThread : TRALfpHttpServerThread;
  protected
    procedure SetActive(const AValue: boolean); override;
    procedure SetPort(const AValue: IntegerRAL); override;
    procedure SetSessionTimeout(const AValue: IntegerRAL); override;
    function CreateRALSSL: TRALSSL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALfpHTTPCertData }

function TRALfpHTTPCertData.GetFileName(AIndex : Integer) : string;
begin
  case AIndex of
    0 : Result := Certificate.FileName;
    1 : Result := TrustedCertificate.FileName;
    2 : Result := PrivateKey.FileName;
    3 : Result := PFX.FileName;
    4 : Result := CertCA.FileName;
  end;
end;

procedure TRALfpHTTPCertData.SetFileName(AIndex : Integer; AValue : string);
begin
  case AIndex of
    0 : Certificate.FileName := AValue;
    1 : TrustedCertificate.FileName := AValue;
    2 : PrivateKey.FileName := AValue;
    3 : PFX.FileName := AValue;
    4 : CertCA.FileName := AValue;
  end;
end;

{ TRALfpHttpServerThread }

function TRALfpHttpServerThread.GetPort : IntegerRAL;
begin
  Result := FParent.Port;
end;

procedure TRALfpHttpServerThread.SetPort(AValue : IntegerRAL);
var
  vActive: boolean;
begin
  inherited;
  vActive := Self.Active;
  Active := False;

  FHttp.Port := AValue;

  Active := vActive;
end;

procedure TRALfpHttpServerThread.SetSessionTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FHttp.AcceptIdleTimeout := AValue;
end;

procedure TRALfpHttpServerThread.DecodeAuth(ARequest: TFPHTTPConnectionRequest; AResult : TRALRequest);
var
  vStr, vAux: StringRAL;
  vInt: IntegerRAL;
begin
  if FParent.Authentication = nil then
    Exit;

  AResult.Authorization.AuthType := ratNone;
  AResult.Authorization.AuthString := '';

  vStr := ARequest.GetHeader(hhAuthorization);
  if vStr <> '' then begin
    vInt := Pos(' ', vStr);
    vAux := Trim(Copy(vStr, 1, vInt - 1));
    if SameText(vAux, 'Basic') then
      AResult.Authorization.AuthType := ratBasic
    else if SameText(vAux, 'Bearer') then
      AResult.Authorization.AuthType := ratBearer;
    AResult.Authorization.AuthString := Copy(vStr, vInt + 1, Length(vStr));
  end;
end;

procedure TRALfpHttpServerThread.EncodeBody(AResponse : TRALResponse; AResponseInfo : TFPHTTPConnectionResponse);
var
//  vMultPart: TIdMultiPartFormDataStream;
  vInt: integer;
begin
  if AResponse.Params.Count(rpkBODY) = 1 then begin
    AResponseInfo.ContentStream := AResponse.Params.Param[0].AsStream;
    AResponseInfo.FreeContentStream := False;
  end
  else begin
{
    vMultPart := TIdMultiPartFormDataStream.Create;
    vInt := 0;
    while vInt < AResponse.Params.Count do
    begin
      vMultPart.AddFormField(AResponse.Params.Param[vInt].ParamName,
                             AResponse.Params.Param[vInt].ContentType,
                             '', // charset
                             AResponse.Params.Param[vInt].AsStream);
      vInt := vInt + 1;
    end;
    vMultPart.Position := 0;
    AResponseInfo.ContentStream := vMultPart;
    AResponseInfo.FreeContentStream := True;
}
  end;
end;

procedure TRALfpHttpServerThread.OnCommandProcess(Sender: TObject;
                                 var ARequest: TFPHTTPConnectionRequest;
                                 var AResponse: TFPHTTPConnectionResponse);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vParam : TRALParam;
  vInt: integer;
  vStr1, vStr2: StringRAL;
  vParamQuery : StringRAL;
begin
  vRequest := TRALRequest.Create;
  try
    with vRequest do
    begin
      ClientInfo.IP := ARequest.RemoteAddress;
      if ClientInfo.IP = '' then
        ClientInfo.IP := ARequest.RemoteHost;

      ClientInfo.MACAddress := '';
      ClientInfo.UserAgent := ARequest.UserAgent;

      Query := ARequest.URI;
      vInt := Pos('?', Query);
      if vInt > 0 then
      begin
        vParamQuery := Copy(Query, vInt + 1, Length(Query));
        Query := Copy(Query, 1, vInt - 1);

        Params.DecodeQuery(vParamQuery);
      end;

      Method := amGET;
      if SameText(ARequest.Method, 'POST') then
        Method := amPOST
      else if SameText(ARequest.Method, 'DELETE') then
        Method := amDELETE
      else if SameText(ARequest.Method, 'PUT') then
        Method := amPUT
      else if SameText(ARequest.Method, 'OPTION') then
        Method := amPUT;

      ContentType := ARequest.ContentType;
      ContentSize := ARequest.ContentLength;

      DecodeAuth(ARequest, vRequest);
      Params.AppendParams(ARequest.CustomHeaders, rpkHEADER);

      // headers tambem
      vInt := 0;
      while vInt < ARequest.FieldCount do
      begin
        vStr1 := ARequest.FieldNames[vInt];
        vStr2 := ARequest.FieldValues[vInt];

        vParam := Params.AddParam(vStr1, vStr2, rpkHEADER);

        vInt := vInt + 1;
      end;

      Params.AppendParams(ARequest.QueryFields, rpkQUERY);
      Params.DecodeBody(ARequest.Content, ARequest.ContentType);

      ARequest.Content := '';
      ARequest.QueryFields.Clear;
      ARequest.CustomHeaders.Clear;
      ARequest.Files.Clear;
    end;

    vResponse := FParent.ProcessCommands(vRequest);

    try
      with AResponse do
      begin
        Code := vResponse.RespCode;
        ContentType := vResponse.ContentType;

        vResponse.Params.AssignParams(CustomHeaders, rpkHEADER);
        EncodeBody(vResponse, AResponse);

        CustomHeaders.Add('Connection=close');
        SendContent;
      end;
    finally
      FreeAndNil(vResponse);
    end;
  finally
    FreeAndNil(vRequest);
  end;
end;

function TRALfpHttpServerThread.GetSessionTimeout : IntegerRAL;
begin
  Result := FHttp.AcceptIdleTimeout;
end;

function TRALfpHttpServerThread.GetActive : boolean;
begin
  Result := FParent.Active;
end;

procedure TRALfpHttpServerThread.SetActive(AValue : boolean);
begin
  if AValue then begin
    FHttp.UseSSL := False;
    if FParent.SSL.Enabled then begin
      FHttp.UseSSL := True;
      FHttp.CertificateData.Assign(TRALfpHTTPSSL(FParent.SSL).SSLOptions);
    end;
  end;
  if (not AValue) and (Active) then
    FHttp.Active := False;

  if not Terminated then
    FEvent.SetEvent;
end;

procedure TRALfpHttpServerThread.Execute;
begin
  while not Terminated do begin
    if (Terminated) or (FEvent = nil) then
      Break;

    FEvent.WaitFor(INFINITE);

    if (Terminated) or (FEvent = nil) then
      Break;

    FEvent.ResetEvent;

    if (Terminated) or (FEvent = nil) then
      Break;

    if (FParent.Active) then
      FHttp.Active := FParent.Active;
  end;
end;

procedure TRALfpHttpServerThread.TerminatedSet;
begin
  if Active then
    Active := False;
  inherited TerminatedSet;
end;

constructor TRALfpHttpServerThread.Create(AOwner: TRALfpHttpServer);
begin
  FParent := AOwner;

  FreeOnTerminate := False;

  FHttp := TFPHttpServer.Create(AOwner);
  FHttp.QueueSize := 15;
  FHttp.Threaded := True;
  FHttp.OnRequest := @OnCommandProcess;

  FEvent := TSimpleEvent.Create;
  FEvent.ResetEvent;

  inherited Create(False);
end;

destructor TRALfpHttpServerThread.Destroy;
begin
  FHttp.Active := False;
  FEvent.SetEvent;
  FParent := nil;
  FreeAndNil(FEvent);
  FreeAndNil(FHttp);
end;

{ TRALfpHTTPSSL }

constructor TRALfpHTTPSSL.Create;
begin
  inherited;
  FSSLOptions := TRALfpHTTPCertData.Create;
end;

destructor TRALfpHTTPSSL.Destroy;
begin
  FSSLOptions.Free;
  inherited;
end;

{ TRALfpHttpServer }

constructor TRALfpHttpServer.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('fpHTTP');
  FHttpThread := TRALfpHttpServerThread.Create(Self);
  FHttpThread.Port := Port;
end;

function TRALfpHttpServer.CreateRALSSL: TRALSSL;
begin
  Result := TRALfpHTTPSSL.Create;
end;

destructor TRALfpHttpServer.Destroy;
begin
  FHttpThread.Terminate;
  FHttpThread.Free;
  inherited;
end;

procedure TRALfpHttpServer.SetActive(const AValue: boolean);
begin
  inherited;
  FHttpThread.Active := AValue;
end;

procedure TRALfpHttpServer.SetPort(const AValue: IntegerRAL);
begin
  inherited;
  FHttpThread.Port := AValue;
end;

procedure TRALfpHttpServer.SetSessionTimeout(const AValue : IntegerRAL);
begin
  inherited;
  FHttpThread.SessionTimeout := AValue;
end;

end.
