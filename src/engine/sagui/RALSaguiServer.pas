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

    class function GetSaguiIP(AReq : Psg_httpreq) : StringRAL;
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

  TRALSaguiStringMap = class(TPersistent)
  private
    FHandle: PPsg_strmap;
    FCurrent: PPsg_strmap;
  protected
    function GetCount: Integer;
  public
    constructor Create(AHandle: Pointer);
    destructor Destroy; override;

    function First : boolean;
    function Next : boolean;
    procedure GetPair(var AName, AValue : StringRAL);
    function MapList : TStringList;
    procedure AppendToParams(AParams : TRALParams; AKind : TRALParamKind);

    // Counts the total pairs present in the map.
    property Count: Integer read GetCount;
  end;

  TRALSaguiUploadFile = class(TPersistent)
  private
    FHandle: Psg_httpupld;
    FStreamHandle: Pointer;
    FDirectory: string;
    FField: string;
    FName: string;
    FMime: string;
    FEncoding: string;
    FSize: UInt64;
  protected
    function GetHandle: Pointer;
  public
    constructor Create(AHandle : Pointer);
    /// Handle of an upload.
    property Handle: Pointer read GetHandle;
    /// Stream handle of the upload. }
    property StreamHandle: Pointer read FStreamHandle;
  published
    /// Directory of the uploaded file.
    property Directory: string read FDirectory;
    /// Field name of the upload.
    property Field: string read FField;
    /// Name of the uploaded file.
    property Name: string read FName;
    /// MIME (content-type) of the upload.
    property Mime: string read FMime;
    /// Encoding (transfer-encoding) of the upload.
    property Encoding: string read FEncoding;
    /// Size of the upload.
    property Size: UInt64 read FSize;
  end;

  TRALSaguiUploadMap = class(TPersistent)
  private
    FHandle: Psg_httpupld;
    FCurrent: Psg_httpupld;
  protected
    function GetCount: Integer;
  public
    constructor Create(AHandle: Pointer);
    destructor Destroy; override;

    function First : boolean;
    function Next : boolean;
    function GetFile : TRALSaguiUploadFile;
    procedure AppendToParams(AParams : TRALParams);

    // Counts the total pairs present in the map.
    property Count: Integer read GetCount;
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
  Active := False;
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
  vStrMap : TRALSaguiStringMap;
  vFileMap : TRALSaguiUploadMap;
  vStr : StringRAL;
  vInt : IntegerRAL;
begin
  vServer := Acls;
  vRequest := TRALServerRequest.Create;
  try
    with vRequest do
    begin
      // headers
      vStrMap := TRALSaguiStringMap.Create(sg_httpreq_headers(Areq));
      try
        vStrMap.AppendToParams(Params, rpkHEADER);
      finally
        vStrMap.Free;
      end;

      // fields
      vStrMap := TRALSaguiStringMap.Create(sg_httpreq_fields(Areq));
      try
        vStrMap.AppendToParams(Params, rpkFIELD);
      finally
        vStrMap.Free;
      end;

      // cookies
      vStrMap := TRALSaguiStringMap.Create(sg_httpreq_cookies(Areq));
      try
        vStrMap.AppendToParams(Params, rpkCOOKIE);
      finally
        vStrMap.Free;
      end;

      // query
      vStrMap := TRALSaguiStringMap.Create(sg_httpreq_params(Areq));
      try
        vStrMap.AppendToParams(Params, rpkQUERY);
      finally
        vStrMap.Free;
      end;

      // body
      vFileMap := TRALSaguiUploadMap.Create(sg_httpreq_uploads(Areq));
      try
        vFileMap.AppendToParams(Params);
      finally
        vStrMap.Free;
      end;

{
  FPayload := CreatePayload(sg_httpreq_payload(FHandle));
  FServerHandle := sg_httpreq_srv(FHandle);
  FIsUploading := sg_httpreq_is_uploading(FHandle);
  if Assigned(sg_httpreq_tls_session) then
    FTLSSession := sg_httpreq_tls_session(FHandle);
}


      ClientInfo.IP := GetSaguiIP(Areq);
      ClientInfo.MACAddress := '';
      ClientInfo.UserAgent := '';

      ContentSize := 0;

      Query := sg_httpreq_path(Areq);
      Method := HTTPMethodToRALMethod(sg_httpreq_method(Areq));

      ContentType := ParamByName('Content-Type').AsString;
      ContentEncoding := ParamByName('Content-Encoding').AsString;
//      AcceptEncoding := ParamByName('Accept-Encoding').AsString;
      ContentEncription := ParamByName('Content-Encription').AsString;
      AcceptEncription := ParamByName('Accept-Encription').AsString;
      Host := ParamByName('Host').AsString;

      Params.CompressType := ContentCompress;
      Params.CriptoOptions.CriptType := ContentCripto;
      Params.CriptoOptions.Key := vServer.CriptoOptions.Key;

      vStr := sg_httpreq_version(Areq);
      vInt := Pos('/', vStr);
      if vInt > 0 then
      begin
        HttpVersion := Copy(vStr, 1, vInt-1);
        Protocol := Copy(vStr, vInt+1, 3);
      end
      else begin
        HttpVersion := 'HTTP';
        Protocol := '1.0';
      end;
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
  if Acls = nil then
    Exit;

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

class function TRALSaguiServer.GetSaguiIP(AReq: Psg_httpreq): StringRAL;
var
  vIP: array[0..45] of cchar;
  vClient: Pcvoid;
begin
  Result := '';

  SgLib.Check;
  vClient := sg_httpreq_client(AReq);
  if not Assigned(vClient) then
    Exit;

  FillChar(vIP, Length(vIP), 0);
  SgLib.CheckLastError(sg_ip(vClient, @vIP[0], Length(vIP)));
  SetLength(Result, Length(vIP));
  Move(vIP[0], Result[PosIniStr], Length(vIP));
  Result := Trim(Result);
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
    if not Assigned(sg_httpsrv_tls_listen3) then
      raise ENotSupportedException.Create('DLL não possui TLS implementado');

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

{ TRALSaguiStringMap }

procedure TRALSaguiStringMap.AppendToParams(AParams: TRALParams;
  AKind: TRALParamKind);
var
  vName, vValue : StringRAL;
begin
  if not Assigned(FHandle^) then
    Exit;

  First;
  repeat
    GetPair(vName, vValue);
    AParams.AddParam(vName, vValue, AKind);
  until not Next;
end;

constructor TRALSaguiStringMap.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FCurrent := AHandle;
end;

destructor TRALSaguiStringMap.Destroy;
begin
  FCurrent := nil;

  SgLib.Check;
  sg_strmap_cleanup(FHandle);
  inherited;
end;

function TRALSaguiStringMap.First: boolean;
begin
  FCurrent := FHandle;
  Result := True;
end;

function TRALSaguiStringMap.GetCount: Integer;
begin
  SgLib.Check;
  Result := sg_strmap_count(FHandle^);
end;

procedure TRALSaguiStringMap.GetPair(var AName, AValue: StringRAL);
begin
  SgLib.Check;
  AName := sg_strmap_name(FCurrent^);
  AValue := sg_strmap_val(FCurrent^);
end;

function TRALSaguiStringMap.MapList: TStringList;
var
  vName, vValue : StringRAL;
begin
  Result := TStringList.Create;

  First;
  repeat
    GetPair(vName, vValue);
    Result.Add(vName + '=' + vValue);
  until not Next;
end;

function TRALSaguiStringMap.Next: boolean;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_strmap_next(FCurrent));
  Result := Assigned(FCurrent^);
end;

{ TRALSaguiUploadMap }

procedure TRALSaguiUploadMap.AppendToParams(AParams: TRALParams);
var
  vFile : TRALSaguiUploadFile;
  vParam : TRALParam;
begin
  First;
  repeat
    vFile := GetFile;
    try
      if Assigned(vFile.StreamHandle) then
      begin
        vParam := AParams.NewParam;
        vParam.ParamName := vFile.Field;
        vParam.FileName := vFile.Name;
        vParam.AsStream := TStream(vFile.StreamHandle);
        vParam.ContentType := vFile.Mime;
        vParam.Kind := rpkBODY;
      end;
    finally
      vFile.Free;
    end;
  until not Next;
end;

constructor TRALSaguiUploadMap.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FCurrent := AHandle;
end;

destructor TRALSaguiUploadMap.Destroy;
begin
  FCurrent := nil;
  inherited;
end;

function TRALSaguiUploadMap.First: boolean;
begin
  FCurrent := FHandle;
  Result := True;
end;

function TRALSaguiUploadMap.GetCount: Integer;
begin
  SgLib.Check;
  Result := sg_httpuplds_count(FHandle);
end;

function TRALSaguiUploadMap.GetFile : TRALSaguiUploadFile;
begin
  Result := TRALSaguiUploadFile.Create(FCurrent);
end;

function TRALSaguiUploadMap.Next: boolean;
begin
  SgLib.Check;
  SgLib.CheckLastError(sg_httpuplds_next(@FCurrent));
  Result := Assigned(FCurrent);
end;

{ TRALSaguiUploadFile }

constructor TRALSaguiUploadFile.Create(AHandle: Pointer);
begin
  FHandle := AHandle;

  SgLib.Check;
  FStreamHandle := sg_httpupld_handle(FHandle);
  FDirectory := sg_httpupld_dir(FHandle);
  FField := sg_httpupld_field(FHandle);
  FName := sg_httpupld_name(FHandle);
  FMime := sg_httpupld_mime(FHandle);
  FEncoding := sg_httpupld_encoding(FHandle);
  FSize := sg_httpupld_size(FHandle);
end;

function TRALSaguiUploadFile.GetHandle: Pointer;
begin
  Result := FHandle;
end;

end.
