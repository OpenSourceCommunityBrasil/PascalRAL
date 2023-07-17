unit RALfpHTTPServer;

interface

uses
  Classes, SysUtils, syncobjs,
  fphttpserver, sslbase, fpHTTP, fphttpapp,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRoutes;

type
  TRALfpHTTPSSL = class(TRALSSL)
  private
    FSSLOptions : TCertificateData;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property SSLOptions: TCertificateData read FSSLOptions write FSSLOptions;
  end;

  TRALfpHTTPServer = class;

  { TRALfpHTTPServerThread }

  TRALfpHTTPServerThread = class(TThread)
  private
    FParent : TRALfpHTTPServer;
    FHttp : TFPHttpServer;
    FEvent : TSimpleEvent;
  protected
    function GetAtive : boolean;
    procedure SetAtive(AValue : boolean);

    function GetPort : IntegerRAL;
    procedure SetPort(AValue : IntegerRAL);

    procedure DecodeAuth(ARequest : TFPHTTPConnectionRequest; AResult : TRALRequest);
    procedure EncodeParams(AResponse: TRALResponse; AResponseInfo: TFPHTTPConnectionResponse);

    procedure OnCommandProcess(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
                               var AResponse : TFPHTTPConnectionResponse);

    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create(AOwner: TRALfpHTTPServer);
    destructor Destroy; override;
  published
    property Active : boolean read GetAtive write SetAtive;
    property Port : IntegerRAL read GetPort write SetPort;
  end;

  TRALfpHTTPServer = class(TRALServer)
  private
    FHttpThread : TRALfpHTTPServerThread;
  protected
    procedure SetActive(const AValue: boolean); override;
    procedure SetPort(const AValue: IntegerRAL); override;
    function CreateRALSSL: TRALSSL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALfpHTTPServerThread }

function TRALfpHTTPServerThread.GetPort : IntegerRAL;
begin
  Result := FParent.Port;
end;

procedure TRALfpHTTPServerThread.SetPort(AValue : IntegerRAL);
var
  vActive: boolean;
begin
  inherited;
  vActive := Self.Active;
  Active := False;

  FHttp.Port := AValue;

  Active := vActive;
end;

procedure TRALfpHTTPServerThread.DecodeAuth(ARequest : TFPHTTPConnectionRequest; AResult : TRALRequest);
var
  vStr, vAux : StringRAL;
  vInt : IntegerRAL;
begin
  AResult.Authorization.AuthType := ratNone;
  AResult.Authorization.AuthString := '';

  vStr := ARequest.GetCustomHeader('Authorization');
  if vStr <> '' then begin
    vInt := Pos(' ',vStr);
    vAux := Trim(Copy(vStr, 1, vInt - 1));
    if SameText(vAux,'Basic') then
      AResult.Authorization.AuthType := ratBasic
    else if SameText(vAux,'Bearer') then
      AResult.Authorization.AuthType := ratBearer;
    AResult.Authorization.AuthString := Copy(vStr, vInt + 1, Length(vStr));
  end;
end;

procedure TRALfpHTTPServerThread.EncodeParams(AResponse : TRALResponse; AResponseInfo : TFPHTTPConnectionResponse);
begin

end;

procedure TRALfpHTTPServerThread.OnCommandProcess(Sender : TObject;
                                 var ARequest : TFPHTTPConnectionRequest;
                                 var AResponse : TFPHTTPConnectionResponse);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vInt: integer;
  vStr1, vStr2: StringRAL;
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
      vInt := Pos('?',Query);
      if vInt > 0 then
        Query := Copy(Query, 1, vInt - 1);

      Method := amGET;
      if SameText(ARequest.Method,'POST') then
        Method := amPOST
      else if SameText(ARequest.Method,'DELETE') then
          Method := amDELETE
      else if SameText(ARequest.Method,'PUT') then
          Method := amPUT
      else if SameText(ARequest.Method,'OPTION') then
          Method := amPUT;

      ContentType := ARequest.ContentType;
      ContentSize := ARequest.ContentLength;

      DecodeAuth(ARequest,vRequest);

      vInt := 0;
      while vInt < ARequest.CustomHeaders.Count do
      begin
        vStr1 := ARequest.CustomHeaders.Names[vInt];
        vStr2 := ARequest.CustomHeaders.ValueFromIndex[vInt];

        Params.AddParam(vStr1, vStr2);
        Headers.Add(vStr1 + '=' + vStr2);

        vInt := vInt + 1;
      end;

      // headers tambem
      vInt := 0;
      while vInt < ARequest.FieldCount do
      begin
        vStr1 := ARequest.FieldNames[vInt];
        vStr2 := ARequest.FieldValues[vInt];

        Params.AddParam(vStr1, vStr2);
        Headers.Add(vStr1 + '=' + vStr2);

        vInt := vInt + 1;
      end;

      vInt := 0;
      while vInt < ARequest.QueryFields.Count do
      begin
        vStr1 := ARequest.QueryFields.Names[vInt];
        vStr2 := ARequest.QueryFields.ValueFromIndex[vInt];

        Params.AddParam(vStr1, vStr2);

        vInt := vInt + 1;
      end;

      // aki vem os forms data, inclusive os files (com o value = filename)
      vStr2 := ARequest.ContentFields.Text;
      vInt := 0;
      while vInt < ARequest.ContentFields.Count do
      begin
        vStr1 := ARequest.ContentFields.Names[vInt];
        vStr2 := ARequest.ContentFields.ValueFromIndex[vInt];

        Params.AddParam(vStr1, vStr2);

        vInt := vInt + 1;
      end;

      if ARequest.Files.Count > 0 then
      begin
        vInt := 0;
        while vInt < ARequest.Files.Count do
        begin
          vStr1 := ARequest.Files.Files[vInt].FieldName;
          vRequest.Params.AddParam(vStr1, ARequest.Files.Files[vInt].Stream);
          vInt := vInt + 1;
        end;
      end
      else if (ARequest.ContentLength > 0) then
      begin
        vRequest.Params.AddParam('ral_body', ARequest.Content);
      end;
    end;

    vResponse := FParent.ProcessCommands(vRequest);

    if (vResponse.Body.Count > 1) then
      vResponse.ContentType := TRALContentType.ctMULTIPARTFORMDATA;
    try
      with AResponse do
      begin
        Code := vResponse.RespCode;
        ContentType := vResponse.ContentType;

        vInt := 0;
        while vInt < vResponse.Headers.Count do
        begin
          CustomHeaders.Add(vResponse.Headers.Strings[vInt]);
          vInt := vInt + 1;
        end;

        if (vResponse.Body.Count > 0) then begin
          if SameText(ContentType, TRALContentType.ctMULTIPARTFORMDATA) then
          begin
            EncodeParams(vResponse, AResponse);
          end
          else
          begin
            ContentStream := vResponse.Body.Param[0].AsStream;
          end;
        end;
        FreeContentStream := True;
        SendContent;
      end;
    finally
      FreeAndNil(vResponse);
    end;
  finally
    FreeAndNil(vRequest);
  end;
end;

function TRALfpHTTPServerThread.GetAtive : boolean;
begin
  Result := FParent.Active;
end;

procedure TRALfpHTTPServerThread.SetAtive(AValue : boolean);
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
  FEvent.SetEvent;
end;

procedure TRALfpHTTPServerThread.Execute;
begin
  while not Terminated do begin
    FEvent.WaitFor(INFINITE);
    FEvent.ResetEvent;

    if Terminated then
      Break;

    if (FParent.Active) then
      FHttp.Active := FParent.Active;
  end;
end;

procedure TRALfpHTTPServerThread.TerminatedSet;
begin
  Active := False;
  inherited TerminatedSet;
end;

constructor TRALfpHTTPServerThread.Create(AOwner : TRALfpHTTPServer);
begin
  FParent := AOwner;

  FHttp := TFPHttpServer.Create(AOwner);
  FHttp.QueueSize := 15;
  FHttp.Threaded := True;
  FHttp.OnRequest := @OnCommandProcess;

  FEvent := TSimpleEvent.Create;
  FEvent.ResetEvent;

  inherited Create(False);
end;

destructor TRALfpHTTPServerThread.Destroy;
begin
  FHttp.Active := False;
  FEvent.SetEvent;
  FEvent.Free;
  FHttp.Free;
end;

{ TRALfpHTTPSSL }

constructor TRALfpHTTPSSL.Create;
begin
  inherited;
  FSSLOptions := TCertificateData.Create;
end;

destructor TRALfpHTTPSSL.Destroy;
begin
  FSSLOptions.Free;
  inherited;
end;

{ TRALfpHTTPServer }

constructor TRALfpHTTPServer.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('fpHTTP');
  FHttpThread := TRALfpHTTPServerThread.Create(Self);
  FHttpThread.Port := Port;
end;

function TRALfpHTTPServer.CreateRALSSL: TRALSSL;
begin
  Result := TRALfpHTTPSSL.Create;
end;

destructor TRALfpHTTPServer.Destroy;
begin
  FHttpThread.Terminate;
  FHttpThread.Free;
  inherited;
end;

procedure TRALfpHTTPServer.SetActive(const AValue: boolean);
begin
  inherited;
  FHttpThread.Active := AValue;
end;

procedure TRALfpHTTPServer.SetPort(const AValue: IntegerRAL);
begin
  inherited;
  FHttpThread.Port := AValue;
end;

end.
