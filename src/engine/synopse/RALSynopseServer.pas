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
    function GetSSL: TRALSynopseSSL;
    function IPv6IsImplemented: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    procedure SetPort(const AValue: IntegerRAL); override;
    procedure SetPoolCount(const AValue: IntegerRAL);
    procedure SetQueueSize(const AValue: IntegerRAL);
    procedure SetSSL(const AValue: TRALSynopseSSL);
    function OnCommandProcess(AContext: THttpServerRequestAbstract): Cardinal;
    function OnSendFile(AContext: THttpServerRequestAbstract; const LocalFileName: TFileName): boolean;
    procedure OnHttpTerminate(ASender: TObject);
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

      // Parse cookie na entrada
      vRequest.AddCookies(vRequest.ParamByName('Cookie').AsString);

      DecodeAuth(vRequest);

      vRequest.ContentDisposition := vRequest.Params.Get['Content-Disposition'].AsString;
      vRequest.ContentEncoding := vRequest.Params.Get['Content-Encoding'].AsString;
      vRequest.AcceptEncoding := vRequest.Params.Get['Accept-Encoding'].AsString;

      vRequest.ContentEncription := vRequest.ParamByName('Content-Encription').AsString;
      vRequest.AcceptEncription := vRequest.ParamByName('Accept-Encription').AsString;

      ValidateRequest(vRequest, vResponse);
      if vResponse.StatusCode < HTTP_BadRequest then
      begin
        vRequest.Params.CompressType := vRequest.ContentCompress;
        vRequest.Params.CriptoOptions.CriptType := vRequest.ContentCripto;
        vRequest.Params.CriptoOptions.Key := CriptoOptions.Key;

        vRequest.RequestText := RawUtf8(AContext.InContent);
        vRequest.Host := AContext.Host;
        vRequest.Protocol := '1.1';
        vRequest.HttpVersion := IfThen(SSL.Enabled, 'HTTPS', 'HTTP');

        //if SSL.Enabled then
        //  vRequest.HttpVersion := 'HTTPS'
        //else
        //  vRequest.HttpVersion := 'HTTP';

        AContext.InContent := EmptyStr;
        AContext.InHeaders := EmptyStr;
      end;

      ProcessCommands(vRequest, vResponse);

      //with vResponse do
      begin
        AContext.OutContent := vResponse.ResponseText;
        AContext.OutContentType := vResponse.ContentType;

        //if (vResponse.ContentDisposition <> EmptyStr) then
          vResponse.Params.AddParam('Content-Disposition', vResponse.ContentDisposition, rpkHEADER);

        //if vResponse.ContentEncoding <> EmptyStr then
          vResponse.Params.AddParam('Content-Encoding', vResponse.ContentEncoding, rpkHEADER);

        //if vResponse.AcceptEncoding <> EmptyStr then
          vResponse.Params.AddParam('Accept-Encoding', vResponse.AcceptEncoding, rpkHEADER);

        //if vResponse.ContentEncription <> EmptyStr then
          vResponse.Params.AddParam('Content-Encription', vResponse.ContentEncription, rpkHEADER);

        // parse cookie na saída
        vHeaders := vResponse.Params.AssignParamsListText(rpkHEADER, ': ');
        if vResponse.Params.Count(rpkCOOKIE) > 0 then
          vHeaders := vHeaders + HTTPLineBreak + vResponse.Params.AssignParamsListText(rpkCOOKIE, ': ');

        //vHeaders := vHeaders + GetParamsCookiesText(IncMinute(Now, CookieLife));

        AContext.OutCustomHeaders := Trim(vHeaders);

        Result := vResponse.StatusCode;
      end;
    except
      on e: exception do
        if Assigned(OnServerError) then
          OnServerError(e)
        else if RaiseError then
          raise
        else
          vResponse.Answer(HTTP_InternalError, e.Message, rctTEXTPLAIN);
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
  FQueueSize := 1000; // Tamanho da fila de threads. Padrao do synopse: 1000
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
