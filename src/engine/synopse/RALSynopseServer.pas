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
    FCertificateFile: TFileName;
    FPrivateKeyFile: TFileName;
    FPrivateKeyPassword: StringRAL;
    FCACertificatesFile: TFileName;
  published
    property CertificateFile: TFileName read FCertificateFile write FCertificateFile;
    property PrivateKeyFile: TFileName read FPrivateKeyFile write FPrivateKeyFile;
    property PrivateKeyPassword: StringRAL read FPrivateKeyPassword write FPrivateKeyPassword;
    property CACertificatesFile: TFileName read FCACertificatesFile write FCACertificatesFile;
  end;

  { TRALSynopseServer }

  TRALSynopseServer = class(TRALServer)
  private
    FHttp: THttpServerSocketGeneric;
    FPoolCount: IntegerRAL;
    FQueueSize: IntegerRAL;
  protected
    function CreateRALSSL: TRALSSL; override;
    procedure SetActive(const AValue: boolean); override;
    procedure SetPort(const AValue: IntegerRAL); override;

    function GetSSL: TRALSynopseSSL;
    procedure SetSSL(const AValue: TRALSynopseSSL);

    procedure SetPoolCount(const AValue: IntegerRAL);
    procedure SetQueueSize(const AValue: IntegerRAL);
    function GetCookieExpires(ADateTime : TDateTime) : StringRAL;

    function IPv6IsImplemented: boolean; override;

    procedure DecodeAuth(AResult: TRALRequest);
    function OnCommandProcess(AContext: THttpServerRequestAbstract): Cardinal;
    function OnSendFile(AContext: THttpServerRequestAbstract; const LocalFileName: TFileName): boolean;
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
    if FHttp <> nil then
      FreeAndNil(FHttp);
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
    if vStr <> '' then
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
  vCookies: TStringList;
  vInt: IntegerRAL;
begin
  vRequest := CreateRequest;
  vResponse := CreateResponse;

  try
    with vRequest do
    begin
      ClientInfo.IP := AContext.RemoteIP;
      if ClientInfo.IP = '' then
        ClientInfo.IP := '127.0.0.1';
      //ClientInfo.Porta := StrToInt(AContext.RemotePort);
      ClientInfo.Porta := 0;

      ClientInfo.MACAddress := '';
      ClientInfo.UserAgent := AContext.UserAgent;

      ContentType := AContext.InContentType;
      ContentSize := Length(AContext.InContent);

      Query := AContext.Url;
      Params.AppendParamsUrl(AContext.Url, rpkQUERY);

      Method := HTTPMethodToRALMethod(AContext.Method);

      Params.AppendParamsListText(AContext.InHeaders, rpkHEADER);
      DecodeAuth(vRequest);

      ContentDisposition := Params.Get['Content-Disposition'].AsString;
      ContentEncoding := Params.Get['Content-Encoding'].AsString;
      AcceptEncoding := Params.Get['Accept-Encoding'].AsString;

      ContentEncription := ParamByName('Content-Encription').AsString;
      AcceptEncription := ParamByName('Accept-Encription').AsString;

      AddCookies(ParamByName('Cookie').AsString);

      ValidateRequest(vRequest, vResponse);
      if vResponse.StatusCode < 400 then
      begin
        Params.CompressType := ContentCompress;
        Params.CriptoOptions.CriptType := ContentCripto;
        Params.CriptoOptions.Key := CriptoOptions.Key;
        RequestText := AContext.InContent;

        Host := AContext.Host;
        Protocol := '1.1';
        if SSL.Enabled then
          HttpVersion := 'HTTPS'
        else
          HttpVersion := 'HTTP';

        AContext.InContent := '';
        AContext.InHeaders := '';
      end;
    end;

    ProcessCommands(vRequest, vResponse);
    with vResponse do
    begin
      AContext.OutContent := ResponseText;
      AContext.OutContentType := ContentType;

      if vResponse.ContentDisposition <> '' then
        Params.AddParam('Content-Disposition', ContentDisposition, rpkHEADER);

      if vResponse.ContentEncoding <> '' then
        Params.AddParam('Content-Encoding', ContentEncoding, rpkHEADER);

      if vResponse.AcceptEncoding <> '' then
        Params.AddParam('Accept-Encoding', AcceptEncoding, rpkHEADER);

      if vResponse.ContentEncription <> '' then
        Params.AddParam('Content-Encription', ContentEncription, rpkHEADER);

      vCookies := TStringList.Create;
      try
        Params.AssignParams(vCookies, rpkCOOKIE);
        for vInt := 0 to Pred(vCookies.Count) do
          Params.AddParam('Set-Cookie', vCookies.Strings[vInt] + GetCookieExpires(Now), rpkHEADER);
      finally
        FreeAndNil(vCookies);
      end;

      AContext.OutCustomHeaders := Params.AssignParamsListText(rpkHEADER, ': ');

      Result := StatusCode;
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

constructor TRALSynopseServer.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := nil;
  FPoolCount := 32; // ou SystemInfo.dwNumberOfProcessors + 1
  FQueueSize := 1000; // padrao do synopse
  SetEngine('Synopse ' + SYNOPSE_FRAMEWORK_FULLVERSION);
end;

destructor TRALSynopseServer.Destroy;
begin
  if Assigned(FHttp) then
    FreeAndNil(FHttp);
  inherited;
end;

function TRALSynopseServer.GetCookieExpires(ADateTime: TDateTime): StringRAL;
const
  HTTPMonths : array[1..12] of string[3] = (
    'Jan', 'Feb', 'Mar', 'Apr',
    'May', 'Jun', 'Jul', 'Aug',
    'Sep', 'Oct', 'Nov', 'Dec');
  HTTPDays: array[1..7] of string[3] = (
    'Sun', 'Mon', 'Tue', 'Wed',
    'Thu', 'Fri', 'Sat');

  DateFormat = '"%s", dd "%s" yyyy hh:mm:ss';
  Expire     ='; Expires=%s GMT';
var
  vYear, vMonth, vDay: Word;
begin
  ADateTime := RALDateTimeToGMT(IncMinute(ADateTime, 30));
  DecodeDate(ADateTime, vYear, vMonth, vDay);

  Result := FormatDateTime(DateFormat, ADateTime);
  Result := Format(Result, [HTTPDays[DayOfWeek(ADateTime)], HTTPMonths[vMonth]]);
  Result := Format(Expire, [Result]);
end;

function TRALSynopseServer.GetSSL: TRALSynopseSSL;
begin
  Result := TRALSynopseSSL(GetDefaultSSL);
end;

end.
