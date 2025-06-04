/// Base unit for RALServer component using Indy Engine
unit RALSaguiServer;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils, DateUtils,
  libsagui,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams, RALTools, RALCompress;

type
  { TRALSaguiSSL }

  TRALSaguiSSL = class(TRALSSL)
  private
    FCertificate: StringRAL;
    FDHParams: StringRAL;
    FPriorities: StringRAL;
    FPrivateKey: StringRAL;
    FPrivatePassword: StringRAL;
    FTrust: StringRAL;
  published
    /// Content of the certificate (cert.pem) to be used by the HTTPS server.
    property Certificate: StringRAL read FCertificate write FCertificate;
    /// Content of the Diffie-Hellman parameters (dh.pem) to be used by the HTTPS
    ///  server for key exchange.
    property DHParams: StringRAL read FDHParams write FDHParams;
    /// Content of the cipher algorithm. Default: @code(NORMAL).
    property Priorities: StringRAL read FPriorities write FPriorities;
    /// Content of the private key (key.pem) to be used by the HTTPS server.
    property PrivateKey: StringRAL read FPrivateKey write FPrivateKey;
    /// { Password of the private key.
    property PrivatePassword: StringRAL read FPrivatePassword write FPrivatePassword;
    /// Content of the certificate (ca.pem) to be used by the HTTPS server for
    ///  client authentication.
    property Trust: StringRAL read FTrust write FTrust;
  end;

  { TRALSaguiServer }

  TRALSaguiServer = class(TRALServer)
  private
    FHandle: Psg_httpsrv;
    FLibPath: TFileName;
    FPoolCount: IntegerRAL;
    FConnectionLimit: Int64RAL;
    class procedure DecodeAuth(AAuthorization: StringRAL; AResult: TRALRequest);
    class procedure DoClientConnectionCallback(Acls: Pcvoid; const Aclient: Pcvoid;
      Aclosed: Pcbool); cdecl; static;
    class procedure DoErrorCallback(Acls: Pcvoid; const Aerr: Pcchar); cdecl; static;
    class procedure DoRequestCallback(Acls: Pcvoid; Areq: Psg_httpreq; Ares: Psg_httpres);
      cdecl; static;
    class function DoStreamRead(Acls: Pcvoid; Aoffset: cuint64_t; Abuf: Pcchar;
      Asize: csize_t): cssize_t; cdecl; static;
    class procedure DoStreamFree(Acls: Pcvoid); cdecl; static;
    class function GetSaguiIP(AReq: Psg_httpreq): StringRAL;
    procedure SetConnectionLimit(const Value: Int64RAL);
    procedure SetPoolCount(const Value: IntegerRAL);
    function GetPoolCount: IntegerRAL;
    function GetConnectionLimit: Int64RAL;
  protected
    function CreateRALSSL: TRALSSL; override;
    procedure CreateServerHandle;
    procedure FreeServerHandle;
    function InitializeServer: boolean;
    function IPv6IsImplemented: boolean; override;

    function GetSSL: TRALSaguiSSL;
    procedure SetActive(const AValue: boolean); override;
    procedure SetLibPath(const AValue: TFileName);
    procedure SetPort(const AValue: IntegerRAL); override;
    procedure SetSessionTimeout(const AValue: IntegerRAL); override;
    procedure SetSSL(const AValue: TRALSaguiSSL);
    procedure ShutdownServerHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ConnectionLimit: Int64RAL read GetConnectionLimit write SetConnectionLimit default 1000;
    property LibPath: TFileName read FLibPath write SetLibPath;
    property PoolCount: IntegerRAL read GetPoolCount write SetPoolCount default 32;
    property SSL: TRALSaguiSSL read GetSSL write SetSSL;
  end;

implementation

type
  { TRALSaguiStringMap }

  TRALSaguiStringMap = class(TPersistent)
  private
    FCurrent: PPsg_strmap;
    FFreeOnDestroy: boolean;
    FHandle: PPsg_strmap;
  protected
    function GetCount: integer;
  public
    constructor Create(AHandle: Pointer);
    destructor Destroy; override;

    procedure Add(const AName, AValue: StringRAL);
    procedure AppendToParams(AParams: TRALParams; AKind: TRALParamKind);
    procedure AssignFromParams(AParams: TRALParams; AKind: TRALParamKind);
    /// Counts the total pairs present in the map.
    property Count: integer read GetCount;
    function First: boolean;
    procedure GetPair(var AName, AValue: StringRAL);
    function MapList: TStringList;
    function Next: boolean;

    property FreeOnDestroy: boolean read FFreeOnDestroy write FFreeOnDestroy;
  end;

  { TRALSaguiUploadFile }

  TRALSaguiUploadFile = class(TPersistent)
  private
    FDirectory: StringRAL;
    FEncoding: StringRAL;
    FField: StringRAL;
    FHandle: Psg_httpupld;
    FMime: StringRAL;
    FName: StringRAL;
    FSize: uint64;
    FStreamHandle: Pointer;
  protected
    function GetHandle: Pointer;
  public
    constructor Create(AHandle: Pointer);
    /// Handle of an upload.
    property Handle: Pointer read GetHandle;
    /// Stream handle of the upload. }
    property StreamHandle: Pointer read FStreamHandle;
  published
    /// Directory of the uploaded file.
    property Directory: StringRAL read FDirectory;
    /// Encoding (transfer-encoding) of the upload.
    property Encoding: StringRAL read FEncoding;
    /// Field name of the upload.
    property Field: StringRAL read FField;
    /// Name of the uploaded file.
    property Name: StringRAL read FName;
    /// MIME (content-type) of the upload.
    property Mime: StringRAL read FMime;
    /// Size of the upload.
    property Size: uint64 read FSize;
  end;

  { TRALSaguiUploadMap }

  TRALSaguiUploadMap = class(TPersistent)
  private
    FHandle: Psg_httpupld;
    FCurrent: Psg_httpupld;
  protected
    function GetCount: integer;
  public
    constructor Create(AHandle: Pointer);
    destructor Destroy; override;

    function First: boolean;
    function Next: boolean;
    function GetFile: TRALSaguiUploadFile;
    procedure AppendToParams(AParams: TRALParams);

    // Counts the total pairs present in the map.
    property Count: integer read GetCount;
  end;

  { TRALSaguiServer }

constructor TRALSaguiServer.Create(AOwner: TComponent);
begin
  inherited;
  FHandle := nil;
  FLibPath := '';

  if SgLib.Handle <> 0 then
    SetEngine('Sagui ' + sg_version_str)
  else
    SetEngine('Sagui');

  {$IFDEF RALWINDOWS}
    if SgLib.Handle <> 0 then
      FLibPath := GetModuleName(SgLib.Handle);
  {$ELSE}
    FLibPath := SgLib.GetLastName;
  {$ENDIF}

  ConnectionLimit := -4;
  PoolCount := -1;
end;

function TRALSaguiServer.CreateRALSSL: TRALSSL;
begin
  Result := TRALSaguiSSL.Create;
end;

procedure TRALSaguiServer.CreateServerHandle;
begin
  FHandle := sg_httpsrv_new2(nil, DoRequestCallback, DoErrorCallback, Self);
  if not Assigned(FHandle) then
    raise Exception.Create(emSaguiServerCreateError);
end;

destructor TRALSaguiServer.Destroy;
begin
  Active := False;
  inherited;
end;

class procedure TRALSaguiServer.DoClientConnectionCallback(Acls: Pcvoid;
  const Aclient: Pcvoid; Aclosed: Pcbool);
begin

end;

class procedure TRALSaguiServer.DoErrorCallback(Acls: Pcvoid; const Aerr: Pcchar);
begin
  // procedure obrigatoria para instanciar o server (CreateServerHandle)
end;

class procedure TRALSaguiServer.DoRequestCallback(Acls: Pcvoid; Areq: Psg_httpreq;
  Ares: Psg_httpres);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vServer: TRALSaguiServer;
  vStrMap: TRALSaguiStringMap;
  vFileMap: TRALSaguiUploadMap;
  vPayLoad: Psg_str;
  vStr: StringRAL;
  vInt: IntegerRAL;
  vParam: TRALParam;
  vRespStream: TStream;
  vCookies: TStringList;
  vPayloadLen : integer;
  vPPayload : Pcchar;
  vPayloadStream : TMemoryStream;
  vPay : ansistring;
begin
  vServer := TRALSaguiServer(Acls);
  vRequest := vServer.CreateRequest;
  vResponse := vServer.CreateResponse;
  try
    with vRequest do
    begin
      // headers
      vStrMap := TRALSaguiStringMap.Create(sg_httpreq_headers(Areq));
      try
        vStrMap.AppendToParams(Params, rpkHEADER);
      finally
        FreeAndNil(vStrMap);
      end;

      ContentType := ParamByName('Content-Type').AsString;
      ContentEncoding := ParamByName('Content-Encoding').AsString;
      AcceptEncoding := ParamByName('Accept-Encoding').AsString;
      ContentEncription := ParamByName('Content-Encription').AsString;
      AcceptEncription := ParamByName('Accept-Encription').AsString;
      Host := ParamByName('Host').AsString;

      Params.CompressType := ContentCompress;
      Params.CriptoOptions.CriptType := ContentCripto;
      Params.CriptoOptions.Key := CriptoKey;

      if vServer.Authentication <> nil then
        DecodeAuth(ParamByName('Authorization').AsString, vRequest);

      ClientInfo.IP := GetSaguiIP(Areq);
      ClientInfo.MACAddress := '';
      ClientInfo.UserAgent := ParamByName('User-Agent').AsString;

      Query := sg_httpreq_path(Areq);
      Method := HTTPMethodToRALMethod(sg_httpreq_method(Areq));

      vServer.ValidateRequest(vRequest, vResponse);
      if vResponse.StatusCode < HTTP_BadRequest then
      begin
        // fields
        vStrMap := TRALSaguiStringMap.Create(sg_httpreq_fields(Areq));
        try
          vStrMap.AppendToParams(Params, rpkFIELD);
        finally
          FreeAndNil(vStrMap);
        end;

        // cookies
        vStrMap := TRALSaguiStringMap.Create(sg_httpreq_cookies(Areq));
        try
          vStrMap.AppendToParams(Params, rpkCOOKIE);
        finally
          FreeAndNil(vStrMap);
        end;

        // query
        vStrMap := TRALSaguiStringMap.Create(sg_httpreq_params(Areq));
        try
          vStrMap.AppendToParams(Params, rpkQUERY);
        finally
          FreeAndNil(vStrMap);
        end;

        // body - stream (files)
        vFileMap := TRALSaguiUploadMap.Create(sg_httpreq_uploads(Areq));
        try
          vFileMap.AppendToParams(Params);
        finally
          FreeAndNil(vFileMap);
        end;

        vPayLoad := sg_httpreq_payload(Areq);
        if Assigned(vPayLoad) then
        begin
          vPayloadLen := sg_str_length(vPayLoad);
          if vPayloadLen > 0 then
          begin
            vPPayload := sg_str_content(vPayLoad);
            SetLength(vPay, vPayloadLen);
            Move(vPPayload^, vPay[1], vPayloadLen);
            vPayloadStream := TMemoryStream.Create;
            try
              vPayloadStream.Write(vPay[1], Length(vPay));
              vPayloadStream.Position := 0;

              RequestStream := vPayloadStream;
            finally
               FreeAndNil(vPayloadStream);
            end;
          end;
        end;

        ContentSize := 0;

        vStr := sg_httpreq_version(Areq);
        vInt := Pos('/', vStr);
        if vInt > 0 then
        begin
          HttpVersion := Copy(vStr, 1, vInt - 1);
          Protocol := Copy(vStr, vInt + 1, 3);
        end
        else
        begin
          HttpVersion := 'HTTP';
          Protocol := '1.0';
        end;
      end;
    end;

    vServer.ProcessCommands(vRequest, vResponse);

    vStrMap := TRALSaguiStringMap.Create(sg_httpres_headers(Ares));
    vStrMap.FreeOnDestroy := False;

    vRespStream := vResponse.ResponseStream;

    vStrMap.AssignFromParams(vResponse.Params, rpkHEADER);

    vStrMap.Add('Server', 'RAL_Sagui');
    vStrMap.Add('Content-Type', vResponse.ContentType);
    vStrMap.Add('Content-Encoding', vResponse.ContentEncoding);
    vStrMap.Add('Content-Disposition', vResponse.ContentDisposition);
    vStrMap.Add('Accept-Encoding', vResponse.AcceptEncoding);
    vStrMap.Add('Content-Encription', vResponse.ContentEncription);

    vCookies := TStringList.Create;
    try
      vResponse.GetParamsCookies(vCookies, IncMinute(Now, vServer.CookieLife));
      for vInt := 0 to Pred(vCookies.Count) do
        vStrMap.Add('Set-Cookie', vCookies.Strings[vInt]);
    finally
      FreeAndNil(vCookies);
    end;

    if vRespStream <> nil then
      sg_httpres_sendstream(Ares, vRespStream.Size, DoStreamRead, vRespStream,
                            DoStreamFree, vResponse.StatusCode)
    else
      sg_httpres_sendstream(Ares, 0, DoStreamRead, nil, DoStreamFree,
                            vResponse.StatusCode)
  finally
    FreeAndNil(vStrMap);
    FreeAndNil(vResponse);
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
  begin
    sg_eor(True);
    Exit;
  end;

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

function TRALSaguiServer.GetConnectionLimit: Int64RAL;
begin
  Result := FConnectionLimit;
end;

function TRALSaguiServer.GetPoolCount: IntegerRAL;
begin
  Result := FPoolCount;
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

class procedure TRALSaguiServer.DecodeAuth(AAuthorization: StringRAL; AResult: TRALRequest);
var
  vInt: IntegerRAL;
  vAux: StringRAL;
begin
  AResult.Authorization.AuthType := ratNone;
  AResult.Authorization.AuthString := '';

  if AAuthorization <> '' then
  begin
    vInt := Pos(' ', AAuthorization);
    vAux := Trim(Copy(AAuthorization, 1, vInt - 1));
    if SameText(vAux, 'Basic') then
      AResult.Authorization.AuthType := ratBasic
    else if SameText(vAux, 'Bearer') then
      AResult.Authorization.AuthType := ratBearer;
    AResult.Authorization.AuthString := Copy(AAuthorization, vInt + 1, Length(AAuthorization));
  end;
end;

function TRALSaguiServer.GetSSL: TRALSaguiSSL;
begin
  Result := TRALSaguiSSL(GetDefaultSSL);
end;

procedure TRALSaguiServer.SetActive(const AValue: boolean);
var
  vActive: boolean;
begin
  vActive := Active;

  inherited;

  if AValue = vActive then
    Exit;

  if (AValue) and (FLibPath <> '') then
  begin
    try
      SgLib.Load(FLibPath);
    except
      raise Exception.Create(emSaguiLibraryLoadError);
    end;
  end;

  SgLib.Check;
  if AValue then
  begin
    SetEngine('Sagui ' + sg_version_str);
    CreateServerHandle;
    if not InitializeServer then
      FreeServerHandle;
    SetConnectionLimit(ConnectionLimit);
    SetPoolCount(PoolCount);
  end
  else
  begin
    ShutdownServerHandle;
    FreeServerHandle;
  end;
end;

procedure TRALSaguiServer.SetConnectionLimit(const Value: Int64RAL);
begin
  FConnectionLimit := Value;
  if FHandle <> nil then
    sg_httpsrv_set_con_limit(FHandle, Value);
end;

procedure TRALSaguiServer.SetLibPath(const AValue: TFileName);
begin
  if AValue = FLibPath then
    Exit;

  if FileExists(AValue) then
    FLibPath := AValue;
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

procedure TRALSaguiServer.SetPoolCount(const Value: IntegerRAL);
begin
  FPoolCount := Value;
  if FHandle <> nil then
    sg_httpsrv_set_thr_pool_size(FHandle, Value);
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

function TRALSaguiServer.InitializeServer: boolean;
begin
  if SSL.Enabled then
  begin
    if not Assigned(sg_httpsrv_tls_listen3) then
      raise Exception.Create(emSaguiServerUnsupportedTLS);

    Result := sg_httpsrv_tls_listen3(FHandle, pansichar(SSL.PrivateKey),
      pansichar(SSL.PrivatePassword), pansichar(SSL.Certificate),
      pansichar(SSL.Trust), pansichar(SSL.DHParams), pansichar(SSL.Priorities), Port, False);
  end
  else
  begin
    Result := sg_httpsrv_listen(FHandle, Port, False);
  end;
end;

function TRALSaguiServer.IPv6IsImplemented: boolean;
begin
  Result := True;
end;

{ TRALSaguiStringMap }

procedure TRALSaguiStringMap.Add(const AName, AValue: StringRAL);
begin
  if AValue = '' then
    Exit;

  SgLib.Check;
  SgLib.CheckLastError(sg_strmap_add(FHandle, PAnsiChar(AnsiString(AName)),
                       PAnsiChar(AnsiString(AValue))));
end;

procedure TRALSaguiStringMap.AppendToParams(AParams: TRALParams; AKind: TRALParamKind);
var
  vName, vValue: StringRAL;
begin
  if not Assigned(FHandle^) then
    Exit;

  First;
  repeat
    GetPair(vName, vValue);
    AParams.AddParam(vName, vValue, AKind);
  until not Next;
end;

procedure TRALSaguiStringMap.AssignFromParams(AParams: TRALParams; AKind: TRALParamKind);
var
  vInt: integer;
begin
  for vInt := 0 to Pred(AParams.Count) do
  begin
    if AParams.Index[vInt].Kind = AKind then
      Add(AParams.Index[vInt].ParamName, AParams.Index[vInt].AsString);
  end;
end;

constructor TRALSaguiStringMap.Create(AHandle: Pointer);
begin
  inherited Create;
  FHandle := AHandle;
  FCurrent := AHandle;
  FFreeOnDestroy := True;
end;

destructor TRALSaguiStringMap.Destroy;
begin
  FCurrent := nil;
  if FFreeOnDestroy then
  begin
    SgLib.Check;
    sg_strmap_cleanup(FHandle);
  end;
  FHandle := nil;
  inherited;
end;

function TRALSaguiStringMap.First: boolean;
begin
  FCurrent := FHandle;
  Result := True;
end;

function TRALSaguiStringMap.GetCount: integer;
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
  vName, vValue: StringRAL;
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
  vFile: TRALSaguiUploadFile;
  vParam: TRALParam;
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
  FHandle := nil;
  inherited;
end;

function TRALSaguiUploadMap.First: boolean;
begin
  FCurrent := FHandle;
  Result := True;
end;

function TRALSaguiUploadMap.GetCount: integer;
begin
  SgLib.Check;
  Result := sg_httpuplds_count(FHandle);
end;

function TRALSaguiUploadMap.GetFile: TRALSaguiUploadFile;
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
