/// The in-memory engine, for integration tests without a port: a
/// TRALInProcServer listens on no socket, and a TRALClient with EngineType
/// 'InProc' and BaseURL 'inproc://<ServerName>' hands its requests straight to
/// it. Every plugin, module, encoder and the whole request pipeline run as
/// over the network; only the bytes never leave the process.
unit RALInProc;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALConsts, RALTools, RALServer, RALClient, RALRequest, RALResponse,
  RALParams, RALCustomObjects, RALStream, RALCompress, RALExceptions, RALErrorCodes;

const
  /// EngineType of the in-memory client.
  ENGINEINPROC = 'InProc';

type
  { A server without a socket: while Active, a TRALClient whose BaseURL is
    inproc://ServerName reaches it. }
  TRALInProcServer = class(TRALServer)
  private
    FServerName: StringRAL;

    procedure SetServerName(const AValue: StringRAL);
  protected
    procedure SetActive(const AValue: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    /// The host of inproc:// URLs that reach this server.
    property ServerName: StringRAL read FServerName write SetServerName;
  end;

  /// The client engine of inproc:// URLs.
  TRALInProcClientHTTP = class(TRALClientHTTP)
  public
    class function EngineName: StringRAL; override;
    class function EngineVersion: StringRAL; override;
    class function PackageDependency: StringRAL; override;
    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
      AMethod: TRALMethod); override;
    class function SupportsMethod(AMethod: TRALMethod): boolean; override;
  end;

/// The active in-memory server named AName, or nil.
function RALFindInProcServer(const AName: StringRAL): TRALInProcServer;

implementation

var
  gServers: TStringList;
  gLock: TCriticalSection;

function RALFindInProcServer(const AName: StringRAL): TRALInProcServer;
var
  vIdx: IntegerRAL;
begin
  Result := nil;
  gLock.Acquire;
  try
    vIdx := gServers.IndexOf(LowerCase(string(AName)));
    if vIdx >= 0 then
      Result := TRALInProcServer(gServers.Objects[vIdx]);
  finally
    gLock.Release;
  end;
end;

{ TRALInProcServer }

constructor TRALInProcServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServerName := 'default';
  { no socket: a port is never asked of the system }
  Port := 80;
  SetEngine(ENGINEINPROC);
end;

destructor TRALInProcServer.Destroy;
begin
  Active := False;
  inherited;
end;

procedure TRALInProcServer.SetServerName(const AValue: StringRAL);
var
  vActive: boolean;
begin
  if AValue = FServerName then
    Exit;
  vActive := Active;
  if vActive then
    Active := False;
  FServerName := AValue;
  if vActive then
    Active := True;
end;

procedure TRALInProcServer.SetActive(const AValue: boolean);
var
  vIdx: IntegerRAL;
begin
  if AValue = Active then
    Exit;
  { the name is checked before the server starts: a taken name leaves it stopped }
  if AValue and (RALFindInProcServer(FServerName) <> nil) then
    raise ERALException.CreateFmt(ecInProcNameTaken, [string(FServerName)]);
  inherited;
  gLock.Acquire;
  try
    vIdx := gServers.IndexOf(LowerCase(string(FServerName)));
    if Active then
    begin
      if vIdx < 0 then
        gServers.AddObject(LowerCase(string(FServerName)), Self);
    end
    else if (vIdx >= 0) and (gServers.Objects[vIdx] = Self) then
      gServers.Delete(vIdx);
  finally
    gLock.Release;
  end;
end;

{ TRALInProcClientHTTP }

class function TRALInProcClientHTTP.EngineName: StringRAL;
begin
  Result := ENGINEINPROC;
end;

class function TRALInProcClientHTTP.EngineVersion: StringRAL;
begin
  Result := '1.0';
end;

class function TRALInProcClientHTTP.PackageDependency: StringRAL;
begin
  Result := 'InProcRAL';
end;

class function TRALInProcClientHTTP.SupportsMethod(AMethod: TRALMethod): boolean;
begin
  Result := (AMethod > amALL) and (AMethod < amUNKNOWN);
end;

procedure TRALInProcClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vServer: TRALInProcServer;
  vHost, vPath: StringRAL;
  vPos, vInt: IntegerRAL;
  vSource, vWire: TStream;
  vSrvReq: TRALRequest;
  vSrvResp: TRALResponse;
  vParam: TRALParam;
  vHeaders, vCookies: TStringList;
  vResult: TRALBodyStream;
  vSink: TRALClientStreamSink;
  vDest: TStream;
begin
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', ENGINEINPROC);

  { inproc://name/path?query }
  vHost := AURL;
  vPos := Pos(StringRAL('://'), vHost);
  if vPos > 0 then
    Delete(vHost, 1, vPos + 2);
  vPos := Pos(StringRAL('/'), vHost);
  if vPos > 0 then
  begin
    vPath := Copy(vHost, vPos, Length(vHost));
    vHost := Copy(vHost, 1, vPos - 1);
  end
  else
    vPath := '/';
  vPos := Pos(StringRAL(':'), vHost);
  if vPos > 0 then
    vHost := Copy(vHost, 1, vPos - 1);
  vServer := RALFindInProcServer(vHost);
  if vServer = nil then
  begin
    SetTransportError(AResponse, rteConnect, -1,
      StringRAL(Format(emInProcNotFound, [string(vHost)])));
    Exit;
  end;

  { the request as an engine puts it on the wire }
  ARequest.ContentCompress := Parent.CompressType;
  ARequest.Params.AddParam('Accept-Encoding', AcceptEncodingFor(ARequest), rpkHEADER);
  ARequest.CriptoKey := Parent.CriptoOptions.Key;
  ARequest.ContentCripto := Parent.CriptoOptions.CriptType;
  if Parent.CriptoOptions.CriptType <> crNone then
  begin
    ARequest.Params.AddParam('Content-Encription', ARequest.ContentEncription, rpkHEADER);
    ARequest.Params.AddParam('Accept-Encription', SupportedEncriptKind, rpkHEADER);
  end;
  ARequest.Params.AddParam('User-Agent', Parent.UserAgent, rpkHEADER);
  vSource := ARequest.TakeWireStream;

  vSrvReq := vServer.CreateRequest;
  vSrvResp := vServer.CreateResponse;
  vResult := TRALBodyStream.Create(-1, Parent.SpoolAbove);
  vSink := NewResponseSink(AResponse, vResult);
  vHeaders := TStringList.Create;
  vCookies := TStringList.Create;
  try
    vSrvReq.Method := AMethod;
    vSrvReq.Query := vPath;
    vSrvReq.Host := vHost;
    vSrvReq.HttpVersion := 'HTTP';
    vSrvReq.Protocol := 'HTTP/1.1';
    vSrvReq.ClientInfo.IP := '127.0.0.1';
    vSrvReq.ClientInfo.UserAgent := Parent.UserAgent;
    vSrvReq.ContentType := ARequest.ContentType;
    vSrvReq.ContentDisposition := ARequest.ContentDisposition;
    if ARequest.ContentCompress <> ctNone then
      vSrvReq.ContentEncoding := ARequest.ContentEncoding;
    vSrvReq.ContentEncription := ARequest.ContentEncription;
    vSrvReq.AcceptEncoding := AcceptEncodingFor(ARequest);
    if vSource <> nil then
      vSrvReq.ContentSize := vSource.Size;
    for vInt := 0 to ARequest.Params.Count - 1 do
    begin
      vParam := ARequest.Params.Index[vInt];
      if vParam.Kind = rpkHEADER then
        vSrvReq.Params.AddParam(vParam.ParamName, vParam.AsString, rpkHEADER);
    end;
    if ARequest.ContentType <> '' then
      vSrvReq.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
    vSrvReq.AddCookies(ARequest.Params.CookieHeaderText);
    vServer.DecodeAuth(vSrvReq);

    vServer.ValidateRequest(vSrvReq, vSrvResp);
    if (vSrvResp.StatusCode < HTTP_BadRequest) and (vSource <> nil) then
    begin
      vSource.Position := 0;
      vSrvReq.SetWireBody(vSource, boBorrowed);
    end;
    vServer.ProcessCommands(vSrvReq, vSrvResp);
    vServer.FinishStream(vSrvReq, vSrvResp);
    vWire := vSrvResp.TakeWireStream;
    try
      vSrvReq.Finish;
      { the head, as a network client reads it }
      for vInt := 0 to vSrvResp.Params.Count - 1 do
      begin
        vParam := vSrvResp.Params.Index[vInt];
        if (vParam.Kind = rpkHEADER) and (vParam.ParamName <> '') then
          vHeaders.Add(string(vParam.ParamName) + ': ' + string(vParam.AsString));
      end;
      if vSrvResp.ContentType <> '' then
        vHeaders.Add('Content-Type: ' + string(vSrvResp.ContentType));
      if vSrvResp.ContentEncoding <> '' then
        vHeaders.Add('Content-Encoding: ' + string(vSrvResp.ContentEncoding));
      if vSrvResp.ContentDisposition <> '' then
        vHeaders.Add('Content-Disposition: ' + string(vSrvResp.ContentDisposition));
      if vSrvResp.ContentEncription <> '' then
        vHeaders.Add('Content-Encription: ' + string(vSrvResp.ContentEncription));
      vSrvResp.GetParamsCookies(vCookies, IncMinute(Now, vServer.CookieLife));
      for vInt := 0 to vCookies.Count - 1 do
        vHeaders.Add('Set-Cookie: ' + vCookies[vInt]);
      AResponse.StatusCode := vSrvResp.StatusCode;
      AResponse.Protocol := 'HTTP/1.1';
      AResponse.ContentType := vSrvResp.ContentType;
      AResponse.ContentDisposition := vSrvResp.ContentDisposition;
      AResponse.ContentEncoding := vSrvResp.ContentEncoding;
      AResponse.Params.CompressType := AResponse.ContentCompress;
      AResponse.ContentEncription := vSrvResp.ContentEncription;
      AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
      AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;
      if not HeadRead then
        AResponse.Params.AppendParams(vHeaders, rpkHEADER);

      vDest := vResult;
      if vSink <> nil then
        vDest := vSink;
      if (vWire <> nil) and (AMethod <> amHEAD) then
      begin
        vWire.Position := 0;
        vDest.CopyFrom(vWire, vWire.Size);
      end;
      if vSink <> nil then
        vSink.Finish;
      AResponse.SetWireBody(vResult.Detach, boOwned);
    finally
      vWire.Free;
    end;
  finally
    vCookies.Free;
    vHeaders.Free;
    vSink.Free;
    vResult.Free;
    vSrvReq.Free;
    vSrvResp.Free;
    vSource.Free;
  end;
end;

initialization
  gLock := TCriticalSection.Create;
  gServers := TStringList.Create;
  RegisterEngine(TRALInProcClientHTTP);

finalization
  UnregisterEngine(TRALInProcClientHTTP);
  FreeAndNil(gServers);
  FreeAndNil(gLock);

end.
