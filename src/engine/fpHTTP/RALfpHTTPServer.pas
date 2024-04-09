unit RALfpHTTPServer;

interface

uses
  Classes, SysUtils,
  fphttpserver, sslbase, fpHTTP, httpprotocol, fphttpclient, opensslsockets,
  RALServer, RALTypes, RALConsts, RALRequest, RALResponse,
  RALParams, RALMultipartCoder, RALTools;

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
  protected
    function GetActive: boolean;
    procedure SetActive(AValue: boolean);

    function GetQueueSize: IntegerRAL;
    procedure SetQueueSize(const AValue: IntegerRAL);

    function GetURLServer: StringRAL;

    function GetPort: IntegerRAL;
    procedure SetPort(AValue: IntegerRAL);

    function GetSessionTimeout: IntegerRAL;
    procedure SetSessionTimeout(const AValue: IntegerRAL);

    procedure DecodeAuth(ARequest: TFPHTTPConnectionRequest; AResult: TRALRequest);

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
    property QueueSize : IntegerRAL read GetQueueSize write SetQueueSize;
  end;

  TRALfpHttpServer = class(TRALServer)
  private
    FHttpThread: TRALfpHttpServerThread;
  protected
    procedure SetActive(const AValue: boolean); override;
    procedure SetPort(const AValue: IntegerRAL); override;

    function GetSSL: TRALfpHTTPSSL;
    procedure SetSSL(const AValue: TRALfpHTTPSSL);

    function GetQueueSize: IntegerRAL;
    procedure SetQueueSize(const AValue: IntegerRAL);

    procedure SetSessionTimeout(const AValue: IntegerRAL); override;
    function CreateRALSSL: TRALSSL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property QueueSize : IntegerRAL read GetQueueSize write SetQueueSize;
    property SSL: TRALfpHTTPSSL read GetSSL write SetSSL;
  end;

implementation

{ TRALfpHTTPCertData }

function TRALfpHTTPCertData.GetFileName(AIndex: Integer): string;
begin
  case AIndex of
    0 : Result := Certificate.FileName;
    1 : Result := TrustedCertificate.FileName;
    2 : Result := PrivateKey.FileName;
    3 : Result := PFX.FileName;
    4 : Result := CertCA.FileName;
  end;
end;

procedure TRALfpHTTPCertData.SetFileName(AIndex: Integer; AValue: string);
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

function TRALfpHttpServerThread.GetPort: IntegerRAL;
begin
  Result := FParent.Port;
end;

function TRALfpHttpServerThread.GetQueueSize: IntegerRAL;
begin
  Result := FHttp.QueueSize;
end;

procedure TRALfpHttpServerThread.SetPort(AValue: IntegerRAL);
var
  vActive: boolean;
begin
  vActive := Self.Active;
  Active := False;

  FHttp.Port := AValue;

  Active := vActive;
end;

procedure TRALfpHttpServerThread.SetQueueSize(const AValue: IntegerRAL);
begin
  FHttp.QueueSize := AValue;
end;

procedure TRALfpHttpServerThread.SetSessionTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FHttp.AcceptIdleTimeout := AValue;
end;

procedure TRALfpHttpServerThread.DecodeAuth(ARequest: TFPHTTPConnectionRequest; AResult: TRALRequest);
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

procedure TRALfpHttpServerThread.OnCommandProcess(Sender: TObject;
                                 var ARequest: TFPHTTPConnectionRequest;
                                 var AResponse: TFPHTTPConnectionResponse);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vInt: integer;
  vStr1, vStr2: StringRAL;
  vConnClose : boolean;
begin
  vRequest := FParent.CreateRequest;
  vResponse := FParent.CreateResponse;
  try
    with vRequest do
    begin
      ClientInfo.IP := ARequest.RemoteAddress;
      if ClientInfo.IP = '' then
        ClientInfo.IP := ARequest.RemoteHost;

      ClientInfo.MACAddress := '';
      ClientInfo.UserAgent := ARequest.UserAgent;

      Query := ARequest.URI;
      Params.AppendParamsUrl(ARequest.URI, rpkQUERY);

      Method := HTTPMethodToRALMethod(ARequest.Method);

      ContentType := ARequest.ContentType;
      ContentSize := ARequest.ContentLength;
      ContentEncoding := ARequest.ContentEncoding;
      AcceptEncoding := ARequest.AcceptEncoding;

      DecodeAuth(ARequest, vRequest);
      Params.AppendParams(ARequest.CustomHeaders, rpkHEADER);

      ContentEncription := ParamByName('Content-Encription').AsString;
      AcceptEncription := ParamByName('Accept-Encription').AsString;

      FParent.ValidateRequest(vRequest, vResponse);
      if vResponse.StatusCode < 400 then
      begin
        // fields tambem
        vInt := 0;
        while vInt < ARequest.FieldCount do
        begin
          vStr1 := ARequest.FieldNames[vInt];
          vStr2 := ARequest.FieldValues[vInt];

          Params.AddParam(vStr1, vStr2, rpkFIELD);

          vInt := vInt + 1;
        end;

        Params.AppendParams(ARequest.QueryFields,rpkQUERY);
        Params.AppendParams(ARequest.CookieFields,rpkCOOKIE);

        Params.CompressType := ContentCompress;
        Params.CriptoOptions.CriptType := ContentCripto;
        Params.CriptoOptions.Key := FParent.CriptoOptions.Key;
        RequestText := ARequest.Content;

        Host := ARequest.Host;
        vInt := Pos('/', ARequest.ProtocolVersion);
        if vInt > 0 then
        begin
          HttpVersion := Copy(ARequest.ProtocolVersion, 1, vInt-1);
          Protocol := Copy(ARequest.ProtocolVersion, vInt+1, 3);
        end
        else begin
          HttpVersion := 'HTTP';
          Protocol := '1.0';
        end;

        vConnClose := False;
        if Protocol = '1.0' then
          vConnClose := True;
        if SameText(ARequest.GetHeader(hhConnection), 'close') then
          vConnClose := True;

        ARequest.Content := '';
        ARequest.QueryFields.Clear;
        ARequest.CustomHeaders.Clear;
        ARequest.CookieFields.Clear;
        ARequest.Files.Clear;
      end;
    end;

    FParent.ProcessCommands(vRequest, vResponse);
    with vResponse do
    begin
      AResponse.Code := StatusCode;

      if ContentEncoding <> '' then
        AResponse.ContentEncoding := ContentEncoding;

      if AcceptEncoding <> '' then
        Params.AddParam('Accept-Encoding', AcceptEncoding, rpkHEADER);

      if ContentEncription <> '' then
        Params.AddParam('Content-Encription', ContentEncription, rpkHEADER);

      AResponse.Server := 'RAL_fpHTTP';
      if vConnClose then
        AResponse.Connection := 'close';

      Params.AssignParams(AResponse.CookieFields, rpkCOOKIE);

      AResponse.ContentStream := ResponseStream;

      AResponse.FreeContentStream := True;
      AResponse.ContentType := ContentType;

      if ContentDisposition <> '' then
        Params.AddParam('Content-Disposition', ContentDisposition, rpkHEADER);

      Params.AssignParams(AResponse.CustomHeaders, rpkHEADER, ': ');

      AResponse.SendContent;
    end;
  finally
    FreeAndNil(vResponse);
    FreeAndNil(vRequest);
  end;
end;

function TRALfpHttpServerThread.GetSessionTimeout: IntegerRAL;
begin
  Result := FHttp.AcceptIdleTimeout;
end;

function TRALfpHttpServerThread.GetActive: boolean;
begin
  Result := FParent.Active;
end;

procedure TRALfpHttpServerThread.SetActive(AValue: boolean);
begin
  if AValue then begin
    FHttp.UseSSL := False;
    if FParent.SSL.Enabled then begin
      FHttp.UseSSL := True;
      FHttp.CertificateData.Assign(FParent.SSL.SSLOptions);
    end;
  end
  else if (not AValue) and (FHttp.Active) then begin
    FHttp.Active := False;
  end;
end;

function TRALfpHttpServerThread.GetURLServer: StringRAL;
begin
  Result := 'http';
  if FParent.SSL.Enabled then
    Result := Result + 's';
  Result := Result + '://127.0.0.1:' + IntToStr(Port);
end;

procedure TRALfpHttpServerThread.Execute;
begin
  while not Terminated do
  begin
    if (FParent.Active) then
      FHttp.Active := FParent.Active;
  end;
end;

procedure TRALfpHttpServerThread.TerminatedSet;
var
  vFP : TFPHTTPClient;
begin
  if FHttp.Active then begin
    Active := False;
    // fernando - 30/07/2023
    // POG para fechar o socket assim q ele for desativado
    // ao ativar o Server ele congela a thread e ao desativar ele mantem ela
    // congelada ate que uma conexao client tente conectar, permitindo assim
    // destruir a thread.
    vFP := TFPHTTPClient.Create(nil);
    try
      try
        {$warnings off}
        vFP.Get(GetURLServer);
        {$warnings on}
      except

      end;
    finally
      FreeAndNil(vFP);
    end;
  end;
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

  inherited Create(True);
end;

destructor TRALfpHttpServerThread.Destroy;
begin
  if FHttp.Active then
    FHttp.Active := False;
  FParent := nil;
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
  if Active then
  begin
    FHttpThread.Terminate;
    FHttpThread.WaitFor;
  end;
  FreeAndNil(FHttpThread);
  inherited;
end;

function TRALfpHttpServer.GetQueueSize: IntegerRAL;
begin
  Result := FHttpThread.QueueSize;
end;

function TRALfpHttpServer.GetSSL: TRALfpHTTPSSL;
begin
  Result := TRALfpHTTPSSL(GetDefaultSSL);
end;

procedure TRALfpHttpServer.SetActive(const AValue: boolean);
var
  vActive: boolean;
begin
  vActive := Active;

  inherited;

  if AValue = vActive then
    Exit;

  FHttpThread.Active := AValue;

  if AValue then
    FHttpThread.Start;
end;

procedure TRALfpHttpServer.SetPort(const AValue: IntegerRAL);
begin
  if AValue = Port then
    Exit;

  FHttpThread.Port := AValue;
  inherited;
end;

procedure TRALfpHttpServer.SetQueueSize(const AValue: IntegerRAL);
begin
  FHttpThread.QueueSize := AValue;
end;

procedure TRALfpHttpServer.SetSessionTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FHttpThread.SessionTimeout := AValue;
end;

procedure TRALfpHttpServer.SetSSL(const AValue: TRALfpHTTPSSL);
begin
  TRALfpHTTPSSL(GetDefaultSSL).Assign(AValue);
end;

end.
