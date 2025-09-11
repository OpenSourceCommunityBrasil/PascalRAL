/// Base unit for RALServer component using mORMot2 Engine
unit RALSynopseServer;

interface

uses
  Classes, SysUtils, syncobjs, StrUtils, DateUtils,
  mormot.net.server, mormot.net.http, mormot.net.async, mormot.core.os,
  mormot.core.base, mormot.rest.http.server, mormot.rest.server, mormot.net.sock,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams, RALTools, RALBase64;

type

  { TRALSynopseSSL }

  TRALSynopseSSL = class(TRALSSL)
  private
    FCACertificatesFile: TFileName;
    FCertificateFile: TFileName;
    FPrivateKeyFile: TFileName;
    FPrivateKeyPassword: StringRAL;
  published
    property CACertificatesFile: TFileName read FCACertificatesFile write FCACertificatesFile;
    property CertificateFile: TFileName read FCertificateFile write FCertificateFile;
    property PrivateKeyFile: TFileName read FPrivateKeyFile write FPrivateKeyFile;
    property PrivateKeyPassword: StringRAL read FPrivateKeyPassword write FPrivateKeyPassword;
  end;

  { TRALSynopseServer }

  TRALSynopseServer = class(TRALServer)
  private
    FHttp: THttpServerSocketGeneric;
    FPoolCount: IntegerRAL;
    FQueueSize: IntegerRAL;
  protected
    function CreateRALSSL: TRALSSL; override;
    procedure DecodeAuth(AResult: TRALRequest);
    function GetSSL: TRALSynopseSSL;
    function IPv6IsImplemented: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    procedure SetPort(const AValue: IntegerRAL); override;
    procedure SetPoolCount(const AValue: IntegerRAL);
    procedure SetQueueSize(const AValue: IntegerRAL);
    procedure SetSSL(const AValue: TRALSynopseSSL);
    function OnCommandProcess(AContext: THttpServerRequestAbstract): Cardinal;
    function OnSendFile(AContext: THttpServerRequestAbstract; const LocalFileName: TFileName): boolean;
    procedure OnHttpTerminate(ASender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PoolCount: IntegerRAL read FPoolCount write SetPoolCount;
    property QueueSize: IntegerRAL read FQueueSize write SetQueueSize;
    property SSL: TRALSynopseSSL read GetSSL write SetSSL;
  end;

implementation

{ TRALSynopseServer }

procedure TRALSynopseServer.SetActive(const AValue: boolean);
var
  vAddr: StringRAL;
  vOptions: THttpServerOptions;
  vActive: boolean;
begin
  vActive := Active;

  inherited;

  if AValue = vActive then
    Exit;

  if AValue then
  begin
    if IPConfig.IPv6Enabled then
      vAddr := Format('[%s]:%d', [IPConfig.IPv6Bind, Self.Port])
    else
      vAddr := IntToStr(Self.Port);

    // THttpAsyncServer - AB funciona com a opcao -v
    // THttpServer - AB funciona sem opcao -v

    vOptions := [hsoNoXPoweredHeader, hsoNoStats, hsoHeadersInterning,
                 hsoThreadSmooting, hsoHeadersUnfiltered];
    //                 hsoThreadCpuAffinity, hsoThreadSocketAffinity];

    // variavel definida mormot.net.sock
    RemoteIPLocalHostAsVoidInServers := False;

    if SSL.Enabled then
      vOptions := vOptions + [hsoEnableTls];

    FHttp := THttpServer.Create(vAddr, nil, nil, '', FPoolCount, SessionTimeout, vOptions);
    FHttp.HttpQueueLength := FQueueSize;
    FHttp.OnSendFile := {$IFDEF FPC}@{$ENDIF}OnSendFile;
    FHttp.ServerName := 'RAL_Mormot2';
    FHttp.OnTerminate := {$IFDEF FPC}@{$ENDIF}OnHttpTerminate;
    //    FHttp.RegisterCompressGzStatic := True;
    FHttp.OnRequest := {$IFDEF FPC}@{$ENDIF}OnCommandProcess;
    if SSL.Enabled then
    begin
      with SSL as TRALSynopseSSL do
      begin
        FHttp.WaitStarted(30, CertificateFile, PrivateKeyFile,
          PrivateKeyPassword, CACertificatesFile);
        FHttp.InitializeTlsAfterBind;
      end;
    end
    else
    begin
      FHttp.WaitStarted;
    end;
  end
  else
  begin
    if FHttp <> nil then begin
      FHttp.Shutdown;
      FHttp.Sock.Close;
      FHttp.Terminate;
      FHttp.WaitFor;
      FreeAndNil(FHttp);
    end;
  end;
end;

procedure TRALSynopseServer.SetPoolCount(const AValue: IntegerRAL);
var
  vActive: boolean;
begin
  if AValue = Port then
    Exit;

  if AValue > 256 then
    FPoolCount := 256
  else
    FPoolCount := AValue;

  vActive := Active;
  Active := False;
  Active := vActive;
end;

procedure TRALSynopseServer.SetPort(const AValue: IntegerRAL);
var
  vActive: boolean;
begin
  if AValue = Port then
    Exit;

  inherited;

  vActive := Active;
  Active := False;
  Active := vActive;
end;

procedure TRALSynopseServer.SetQueueSize(const AValue: IntegerRAL);
begin
  if AValue = FQueueSize then
    Exit;

  FQueueSize := AValue;
  if FHttp <> nil then
    FHttp.HttpQueueLength := FQueueSize;
end;

procedure TRALSynopseServer.SetSSL(const AValue: TRALSynopseSSL);
begin
  TRALSynopseSSL(GetDefaultSSL).Assign(AValue);
end;

function TRALSynopseServer.IPv6IsImplemented: boolean;
begin
  Result := True;
end;

procedure TRALSynopseServer.DecodeAuth(AResult: TRALRequest);
var
  vStr, vAux: StringRAL;
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  if Authentication = nil then
    Exit;

  AResult.Authorization.AuthType := ratNone;
  AResult.Authorization.AuthString := '';

  vParam := AResult.Params.GetKind['Authorization', rpkHEADER];
  if not vParam.IsNilOrEmpty then
  begin
    vStr := vParam.AsString;
    if vStr <> EmptyStr then
    begin
      vInt := Pos(' ', vStr);
      vAux := Trim(Copy(vStr, 1, vInt - 1));
      if SameText(vAux, 'Basic') then
        AResult.Authorization.AuthType := ratBasic
      else if SameText(vAux, 'Bearer') then
        AResult.Authorization.AuthType := ratBearer;
      AResult.Authorization.AuthString := Copy(vStr, vInt + 1, Length(vStr));
    end;
  end;
end;

function TRALSynopseServer.CreateRALSSL: TRALSSL;
begin
  inherited;
  Result := TRALSynopseSSL.Create;
end;

function TRALSynopseServer.OnCommandProcess(AContext: THttpServerRequestAbstract): Cardinal;
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vHeaders: StringRAL;
  vInt: IntegerRAL;
begin
  vRequest := CreateRequest;
  vResponse := CreateResponse;

  try
    try
      vRequest.AddHeader('RALEngine', ENGINESYNOPSE);
      vRequest.ClientInfo.IP := RawUtf8(AContext.RemoteIP);
      if vRequest.ClientInfo.IP = EmptyStr then
        vRequest.ClientInfo.IP := '127.0.0.1';
      //ClientInfo.Porta := StrToInt(AContext.RemotePort);
      vRequest.ClientInfo.Port := 0;

      vRequest.ClientInfo.MACAddress := EmptyStr;
      vRequest.ClientInfo.UserAgent := RawUtf8(AContext.UserAgent);

      vRequest.ContentType := RawUtf8(AContext.InContentType);
      vRequest.ContentSize := Length(AContext.InContent);

      vRequest.Query := RawUtf8(AContext.Url);
      vRequest.Params.AppendParamsUrl(vRequest.Query, rpkQUERY);

      vRequest.Method := HTTPMethodToRALMethod(RawUtf8(AContext.Method));

      vRequest.Params.AppendParamsListText(RawUtf8(AContext.InHeaders), rpkHEADER);
      DecodeAuth(vRequest);

      vRequest.ContentDisposition := vRequest.Params.Get['Content-Disposition'].AsString;
      vRequest.ContentEncoding := vRequest.Params.Get['Content-Encoding'].AsString;
      vRequest.AcceptEncoding := vRequest.Params.Get['Accept-Encoding'].AsString;

      vRequest.ContentEncription := vRequest.ParamByName('Content-Encription').AsString;
      vRequest.AcceptEncription := vRequest.ParamByName('Accept-Encription').AsString;

      vRequest.AddCookies(vRequest.ParamByName('Cookie').AsString);

      ValidateRequest(vRequest, vResponse);
      if vResponse.StatusCode < HTTP_BadRequest then
      begin
        vRequest.Params.CompressType := vRequest.ContentCompress;
        vRequest.Params.CriptoOptions.CriptType := vRequest.ContentCripto;
        vRequest.Params.CriptoOptions.Key := CriptoOptions.Key;
        vRequest.RequestText := RawUtf8(AContext.InContent);

        vRequest.Host := AContext.Host;
        vRequest.Protocol := '1.1';
        if SSL.Enabled then
          vRequest.HttpVersion := 'HTTPS'
        else
          vRequest.HttpVersion := 'HTTP';

        AContext.InContent := EmptyStr;
        AContext.InHeaders := EmptyStr;
      end;

      ProcessCommands(vRequest, vResponse);
      with vResponse do
      begin
        AContext.OutContent := ResponseText;
        AContext.OutContentType := ContentType;

        if (vResponse.ContentDisposition <> EmptyStr) then
          Params.AddParam('Content-Disposition', ContentDisposition, rpkHEADER);

        if vResponse.ContentEncoding <> EmptyStr then
          Params.AddParam('Content-Encoding', ContentEncoding, rpkHEADER);

        if vResponse.AcceptEncoding <> EmptyStr then
          Params.AddParam('Accept-Encoding', AcceptEncoding, rpkHEADER);

        if vResponse.ContentEncription <> EmptyStr then
          Params.AddParam('Content-Encription', ContentEncription, rpkHEADER);

        vHeaders := Params.AssignParamsListText(rpkHEADER, ': ');
        vHeaders := vHeaders + HTTPLineBreak;
        vHeaders := vHeaders + GetParamsCookiesText(IncMinute(Now, CookieLife));

        AContext.OutCustomHeaders := Trim(vHeaders);

        Result := StatusCode;
      end;
    except
      on e: exception do
        if Assigned(OnServerError) then
          OnServerError(e)
        else if RaiseError then
          raise;
    end;
  finally
    FreeAndNil(vResponse);
    FreeAndNil(vRequest);
  end;
end;

function TRALSynopseServer.OnSendFile(AContext: THttpServerRequestAbstract;
  const LocalFileName: TFileName): boolean;
begin
  // para OutContentType = STATICFILE_CONTENT_TYPE
  {$IFNDEF FPC}
    AContext.OutContent := UTF8Decode(AContext.OutContent);
  {$ENDIF}
  Result := True;
end;

procedure TRALSynopseServer.OnHttpTerminate(ASender: TObject);
begin
  Active := False;
end;

constructor TRALSynopseServer.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := nil;
  FPoolCount := 32; // ou SystemInfo.dwNumberOfProcessors + 1
  FQueueSize := 1000; // padrao do synopse
  SetEngine('mORMot2 ' + SYNOPSE_FRAMEWORK_FULLVERSION);
end;

destructor TRALSynopseServer.Destroy;
begin
  Active := False;
  inherited;
end;

function TRALSynopseServer.GetSSL: TRALSynopseSSL;
begin
  Result := TRALSynopseSSL(GetDefaultSSL);
end;

end.
