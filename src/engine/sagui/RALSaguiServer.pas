unit RALSaguiServer;

interface

uses
  Classes, SysUtils,
  libsagui,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams, RALTools;

type
  TRALSaguiSSL = class(TRALSSL)
  private
    FPrivateKey: string;
    FPrivatePassword: string;
    FCertificate: string;
    FTrust: string;
    FDHParams: string;
    FPriorities: string;
  public
    constructor Create;
    destructor Destroy; override;
  published
    /// Content of the private key (key.pem) to be used by the HTTPS server.
    property PrivateKey: string read FPrivateKey write FPrivateKey;
    /// { Password of the private key.
    property PrivatePassword: string read FPrivatePassword write FPrivatePassword;
    /// Content of the certificate (cert.pem) to be used by the HTTPS server.
    property Certificate: string read FCertificate write FCertificate;
    /// Content of the certificate (ca.pem) to be used by the HTTPS server for
    ///  client authentication.
    property Trust: string read FTrust write FTrust;
    /// Content of the Diffie-Hellman parameters (dh.pem) to be used by the HTTPS
    ///  server for key exchange.
    property DHParams: string read FDHParams write FDHParams;
    /// Content of the cipher algorithm. Default: @code(NORMAL).
    property Priorities: string read FPriorities write FPriorities;
  end;

  { TRALSaguiServer }

  TRALSaguiServer = class(TRALServer)
  private
    FHandle: Psg_httpsrv;

    class function DoAuthenticationCallback(Acls: Pcvoid; Aauth: Psg_httpauth;
      Areq: Psg_httpreq; Ares: Psg_httpres): cbool; cdecl; static;
    class procedure DoRequestCallback(Acls: Pcvoid; Areq: Psg_httpreq;
      Ares: Psg_httpres); cdecl; static;
    class procedure DoErrorCallback(Acls: Pcvoid;
      const Aerr: Pcchar); cdecl; static;

    class function DoStreamRead(Acls: Pcvoid; Aoffset: cuint64_t; Abuf: Pcchar;
      Asize: csize_t): cssize_t; cdecl; static;
    class procedure DoStreamFree(Acls: Pcvoid); cdecl; static;

    class procedure DoClientConnectionCallback(Acls: Pcvoid;
      const Aclient: Pcvoid; Aclosed: Pcbool); cdecl; static;
  protected
    function CreateRALSSL: TRALSSL; override;
    procedure SetActive(const AValue: boolean); override;
    procedure SetSessionTimeout(const AValue: IntegerRAL); override;
    procedure SetPort(const AValue: IntegerRAL); override;
    function IPv6IsImplemented : boolean; override;

    function GetSSL: TRALSaguiSSL;
    procedure SetSSL(const AValue: TRALSaguiSSL);

    procedure CreateServerHandle;
    procedure ShutdownServerHandle;
    procedure FreeServerHandle;

    function InitilizeServer : boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SSL: TRALSaguiSSL read GetSSL write SetSSL;
  end;

implementation

{ TRALSaguiServer }

constructor TRALSaguiServer.Create(AOwner: TComponent);
begin
  inherited;
  SgLib.Check;
  SetEngine('Sagui ' + sg_version_str);
  FHandle := nil;
end;

function TRALSaguiServer.CreateRALSSL: TRALSSL;
begin
  Result := TRALSaguiSSL.Create;
end;

procedure TRALSaguiServer.CreateServerHandle;
var
  vAuth : sg_httpauth_cb;
begin
  if Authentication <> nil then
    vAuth := DoAuthenticationCallback
  else
    vAuth := nil;
  FHandle := sg_httpsrv_new2(vAuth, DoRequestCallback, DoErrorCallback, Self);
//  if not Assigned(FHandle) then
//    raise EInvalidPointer.Create(SBrookCannotCreateServerHandle);
end;

destructor TRALSaguiServer.Destroy;
begin
  inherited;
end;

class function TRALSaguiServer.DoAuthenticationCallback(Acls: Pcvoid;
  Aauth: Psg_httpauth; Areq: Psg_httpreq; Ares: Psg_httpres): cbool;
begin

end;

class procedure TRALSaguiServer.DoClientConnectionCallback(Acls: Pcvoid;
  const Aclient: Pcvoid; Aclosed: Pcbool);
begin

end;

class procedure TRALSaguiServer.DoErrorCallback(Acls: Pcvoid;
  const Aerr: Pcchar);
begin

end;

class procedure TRALSaguiServer.DoRequestCallback(Acls: Pcvoid;
  Areq: Psg_httpreq; Ares: Psg_httpres);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vServer : TRALSaguiServer;
begin
  vServer := Acls;
  vRequest := TRALRequest.Create;
  try
    with vRequest do
    begin
      ClientInfo.IP := 'localhost';
      ClientInfo.MACAddress := '';
      ClientInfo.UserAgent := '';

      ContentType := '';
      ContentEncoding := '';
      AcceptEncoding := '';
      ContentSize := 0;

      Query := '/ping';

      Method := amGET;

{
      Params.AppendParams(ARequestInfo.RawHeaders, rpkHEADER);
      Params.AppendParams(ARequestInfo.CustomHeaders, rpkHEADER);
      Params.AppendParams(ARequestInfo.Params, rpkQUERY);

      if ARequestInfo.Params.Count = 0 then begin
        Params.AppendParamsUrl(ARequestInfo.QueryParams, rpkQUERY);
        Params.AppendParamsUrl(ARequestInfo.UnparsedParams, rpkQUERY);
      end;

      for vInt := 0 to Pred(AResponseInfo.Cookies.Count) do begin
        vIdCookie := AResponseInfo.Cookies.Cookies[vInt];
        Params.AddParam(vIdCookie.CookieName, vIdCookie.Value, rpkCOOKIE);
      end;

      ContentEncription := ParamByName('Content-Encription').AsString;
      AcceptEncription := ParamByName('Accept-Encription').AsString;;

      Params.CompressType := ContentCompress;
      Params.CriptoOptions.CriptType := ContentCripto;
      Params.CriptoOptions.Key := CriptoOptions.Key;
      Stream := Params.DecodeBody(ARequestInfo.PostStream, ARequestInfo.ContentType);

      Host := ARequestInfo.Host;
      vInt := Pos('/', ARequestInfo.Version);
      if vInt > 0 then
      begin
        HttpVersion := Copy(ARequestInfo.Version, 1, vInt-1);
        Protocol := Copy(ARequestInfo.Version, vInt+1, 3);
      end
      else begin
        HttpVersion := 'HTTP';
        Protocol := '1.0';
      end;
}
    end;

    vResponse := vServer.ProcessCommands(vRequest);

    try
      with vResponse do
      begin
        sg_httpres_sendstream(Ares, 0, DoStreamRead, ResponseStream, DoStreamFree, StatusCode);
      end;
    finally
      FreeAndNil(vResponse);
    end;
  finally
    FreeAndNil(vRequest);
  end;
end;

class procedure TRALSaguiServer.DoStreamFree(Acls: Pcvoid);
begin
  TStream(Acls).Free;
end;

class function TRALSaguiServer.DoStreamRead(Acls: Pcvoid; Aoffset: cuint64_t;
  Abuf: Pcchar; Asize: csize_t): cssize_t;
begin
  Result := TStream(Acls).Read(Abuf^, Asize);
  if Result = 0 then
    Exit(sg_eor(False));
  if Result < 0 then
    Result := sg_eor(True);
end;

procedure TRALSaguiServer.FreeServerHandle;
begin
  if FHandle <> nil then
    sg_httpsrv_free(FHandle);
  FHandle := nil;
end;

function TRALSaguiServer.GetSSL: TRALSaguiSSL;
begin
  Result := TRALSaguiSSL(GetDefaultSSL);
end;

procedure TRALSaguiServer.SetActive(const AValue: boolean);
begin
  if AValue = Active then
    Exit;

  SgLib.Check;

  if AValue then begin
    CreateServerHandle;
    if not InitilizeServer then
      FreeServerHandle;
  end
  else begin
    ShutdownServerHandle;
    FreeServerHandle;
  end;

  inherited;
end;

procedure TRALSaguiServer.SetSessionTimeout(const AValue: IntegerRAL);
begin
  inherited;
end;

procedure TRALSaguiServer.SetSSL(const AValue: TRALSaguiSSL);
begin
  TRALSaguiSSL(GetDefaultSSL).Assign(AValue);
end;

procedure TRALSaguiServer.ShutdownServerHandle;
begin
  if FHandle <> nil then
    sg_httpsrv_shutdown(FHandle);
end;

procedure TRALSaguiServer.SetPort(const AValue: IntegerRAL);
var
  vActive: boolean;
begin
  if AValue = Port then
    Exit;

  vActive := Self.Active;
  Active := False;

  Active := vActive;
  inherited;
end;

function TRALSaguiServer.InitilizeServer : boolean;
begin
  if SSL.Enabled then
  begin
//    if not Assigned(sg_httpsrv_tls_listen3) then
//      raise ENotSupportedException.Create(SBrookTLSNotAvailable);

    Result := sg_httpsrv_tls_listen3(FHandle,
      PAnsiChar(SSL.PrivateKey),
      PAnsiChar(SSL.PrivatePassword),
      PAnsiChar(SSL.Certificate),
      PAnsiChar(SSL.Trust),
      PAnsiChar(SSL.DHParams),
      PAnsiChar(SSL.Priorities), Port, False);
  end
  else
  begin
    Result := sg_httpsrv_listen(FHandle, Port, False);
  end;
end;

function TRALSaguiServer.IPv6IsImplemented : boolean;
begin
  Result := True;
end;

{ TRALSaguiSSL }

constructor TRALSaguiSSL.Create;
begin
  inherited;
end;

destructor TRALSaguiSSL.Destroy;
begin
  inherited;
end;

end.
