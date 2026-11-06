uses
  Classes, SysUtils, DateUtils,
  {$IFDEF DELPHIXE2UP}
  Web.HTTPApp,
  {$ELSE}
  HTTPApp,
  {$ENDIF}
  RALServer, RALRequest, RALResponse, RALTools, RALTypes, RALParams, RALConsts,
  RALMIMETypes, RALStream;

type
  { The server of the library. Active registers it as the one the requests
    reach (one per library); Active := False at unload stops its plugins. }
  TRALWebBrokerServer = class(TRALServer)
  private
    FExtraHeaders: TStrings;
    procedure SetExtraHeaders(const AValue: TStrings);
  protected
    procedure SetActive(const AValue: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Answers a WebBroker request; the web module calls it for every request
      (and so can an application's own TWebModule, in an action) }
    procedure HandleWebRequest(ARequest: TWebRequest; AResponse: TWebResponse);
  published
    { Header names read one by one where the web server gives no list of
      them (Apache before Delphi 13): the usual ones are always read }
    property ExtraHeaders: TStrings read FExtraHeaders write SetExtraHeaders;
  end;

  { The web module of the library: Application.WebModuleClass :=
    TRALWebBrokerModule. Every request goes to the active TRALWebBrokerServer. }
  TRALWebBrokerModule = class(TCustomWebDispatcher)
  private
    procedure DefaultAction(Sender: TObject; ARequest: TWebRequest;
      AResponse: TWebResponse; var AHandled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

/// The active server of this library, or nil.
function RALWebBrokerActiveServer: TRALWebBrokerServer;

implementation

const
  { what is read when the web server lists no headers }
  cKnownHeaders: array[0..27] of string = ('Accept', 'Accept-Charset',
    'Accept-Encoding', 'Accept-Language', 'Accept-Encription', 'Authorization',
    'Cache-Control', 'Content-Disposition', 'Content-Encoding',
    'Content-Encription', 'Content-Type', 'Cookie', 'Host', 'If-Match',
    'If-Modified-Since', 'If-None-Match', 'Origin', 'Range', 'Referer',
    'User-Agent', 'X-Forwarded-For', 'X-Forwarded-Host', 'X-Forwarded-Proto',
    'X-Real-IP', 'X-Requested-With', 'Access-Control-Request-Method',
    'Access-Control-Request-Headers', 'traceparent');

var
  gServer: TRALWebBrokerServer;

function RALWebBrokerActiveServer: TRALWebBrokerServer;
begin
  Result := gServer;
end;

{ RawContent is AnsiString up to XE3 or so and TBytes later: the overload
  the compiler picks writes it into the stream }
procedure WriteRaw(AStream: TStream; const ARaw: TBytes); overload;
begin
  if Length(ARaw) > 0 then
    AStream.WriteBuffer(ARaw[0], Length(ARaw));
end;

procedure WriteRaw(AStream: TStream; const ARaw: AnsiString); overload;
begin
  if Length(ARaw) > 0 then
    AStream.WriteBuffer(Pointer(ARaw)^, Length(ARaw));
end;

{ the whole body: IIS hands the first piece over (RawContent) and the rest is
  read from the client }
procedure SetRawBody(ARequest: TRALRequest; AWebRequest: TWebRequest);
var
  vStream: TMemoryStream;
  vBuffer: array[0..65535] of Byte;
  vRead: Integer;
begin
  {$IFDEF DELPHI10_0UP}
  if AWebRequest.ContentLength > Length(AWebRequest.RawContent) then
    AWebRequest.ReadTotalContent;
  {$ENDIF}
  vStream := TMemoryStream.Create;
  try
    WriteRaw(vStream, AWebRequest.RawContent);
    while vStream.Size < AWebRequest.ContentLength do
    begin
      vRead := AWebRequest.ReadClient(vBuffer, SizeOf(vBuffer));
      if vRead <= 0 then
        Break;
      vStream.WriteBuffer(vBuffer, vRead);
    end;
  except
    vStream.Free;
    raise;
  end;
  if vStream.Size = 0 then
    vStream.Free
  else
  begin
    vStream.Position := 0;
    ARequest.SetWireBody(vStream, boOwned);
  end;
end;

{ one cookie line as a WebBroker cookie (as the CGI engine does it) }
procedure AddWebCookie(AResponse: TWebResponse; const ACookie: TRALCookie);
var
  vCookie: TCookie;
begin
  if ACookie.Name = '' then
    Exit;
  vCookie := AResponse.Cookies.Add;
  vCookie.Name := string(ACookie.Name);
  vCookie.Value := string(ACookie.Value);
  vCookie.Domain := string(ACookie.Domain);
  vCookie.Path := string(ACookie.Path);
  if ACookie.MaxAge < 0 then
    vCookie.Expires := EncodeDate(1970, 1, 1)
  else if ACookie.MaxAge > 0 then
    vCookie.Expires := IncSecond(RALDateTimeToGMT(Now), ACookie.MaxAge)
  else if ACookie.Expires <> 0 then
    vCookie.Expires := RALDateTimeToGMT(ACookie.Expires);
  vCookie.Secure := ACookie.Secure;
  {$IFDEF DELPHI10_4UP}
  vCookie.HttpOnly := ACookie.HttpOnly;
  case ACookie.SameSite of
    cssLax: vCookie.SameSite := 'Lax';
    cssNone: vCookie.SameSite := 'None';
    cssStrict: vCookie.SameSite := 'Strict';
  end;
  {$ENDIF}
end;

{ the request headers: the list the web server gives (Delphi 13's
  ExtractAllHeaders, IIS's ALL_RAW), else the known names one by one }
procedure ReadHeaders(ARequest: TWebRequest; ARALRequest: TRALRequest; AExtra: TStrings);
var
  vList: TStringList;
  vInt, vPos: IntegerRAL;
  vLine, vValue: string;

  procedure AddByName(const AName: string);
  begin
    vValue := ARequest.GetFieldByName(AName);
    if vValue <> '' then
      ARALRequest.Params.AddParam(StringRAL(AName), StringRAL(vValue), rpkHEADER);
  end;

begin
  vList := TStringList.Create;
  try
    {$IFDEF DELPHI13UP}
    try
      ARequest.ExtractAllHeaders(vList);
    except
      vList.Clear;
    end;
    {$ENDIF}
    if vList.Count = 0 then
      vList.Text := ARequest.GetFieldByName('ALL_RAW');
    if vList.Count > 0 then
    begin
      for vInt := 0 to vList.Count - 1 do
      begin
        vLine := vList[vInt];
        vPos := Pos(':', vLine);
        if vPos > 1 then
          ARALRequest.Params.AddParam(StringRAL(Trim(Copy(vLine, 1, vPos - 1))),
            StringRAL(Trim(Copy(vLine, vPos + 1, Length(vLine)))), rpkHEADER);
      end;
    end
    else
    begin
      for vInt := Low(cKnownHeaders) to High(cKnownHeaders) do
        AddByName(cKnownHeaders[vInt]);
      if AExtra <> nil then
        for vInt := 0 to AExtra.Count - 1 do
          if Trim(AExtra[vInt]) <> '' then
            AddByName(Trim(AExtra[vInt]));
    end;
  finally
    vList.Free;
  end;
end;

{ TRALWebBrokerServer }

constructor TRALWebBrokerServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExtraHeaders := TStringList.Create;
  SetEngine('WebBroker - Delphi');
end;

destructor TRALWebBrokerServer.Destroy;
begin
  Active := False;
  FreeAndNil(FExtraHeaders);
  inherited;
end;

procedure TRALWebBrokerServer.SetExtraHeaders(const AValue: TStrings);
begin
  FExtraHeaders.Assign(AValue);
end;

procedure TRALWebBrokerServer.SetActive(const AValue: boolean);
begin
  if AValue = Active then
    Exit;
  inherited;
  if Active then
    gServer := Self
  else if gServer = Self then
    gServer := nil;
end;

procedure TRALWebBrokerServer.HandleWebRequest(ARequest: TWebRequest; AResponse: TWebResponse);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vCookies: TStringList;
  vInt: IntegerRAL;
  vQuery: string;
begin
  vRequest := CreateRequest;
  vResponse := CreateResponse;
  try
    try
      vRequest.ClientInfo.IP := StringRAL(ARequest.RemoteIP);
      if vRequest.ClientInfo.IP = '' then
        vRequest.ClientInfo.IP := StringRAL(ARequest.RemoteAddr);
      vRequest.ClientInfo.UserAgent := StringRAL(ARequest.UserAgent);
      vRequest.ContentType := StringRAL(ARequest.ContentType);
      vRequest.ContentSize := ARequest.ContentLength;

      vQuery := ARequest.PathInfo;
      if vQuery = '' then
        vQuery := '/';
      if ARequest.Query <> '' then
        vQuery := vQuery + '?' + ARequest.Query;
      vRequest.Query := StringRAL(vQuery);
      vRequest.Method := HTTPMethodToRALMethod(StringRAL(ARequest.Method));
      vRequest.Protocol := StringRAL(ARequest.ProtocolVersion);
      vRequest.Host := StringRAL(ARequest.Host);
      if SameText(ARequest.GetFieldByName('HTTPS'), 'on') then
        vRequest.HttpVersion := 'HTTPS'
      else
        vRequest.HttpVersion := 'HTTP';

      ReadHeaders(ARequest, vRequest, FExtraHeaders);
      vRequest.ContentEncoding := vRequest.GetHeader('Content-Encoding');
      vRequest.AcceptEncoding := vRequest.GetHeader('Accept-Encoding');
      vRequest.ContentEncription := vRequest.GetHeader('Content-Encription');
      vRequest.AcceptEncription := vRequest.GetHeader('Accept-Encription');
      vRequest.AddCookies(vRequest.Params.GetKind['Cookie', rpkHEADER].AsString);
      DecodeAuth(vRequest);

      ValidateRequest(vRequest, vResponse);
      if vResponse.StatusCode < HTTP_BadRequest then
        SetRawBody(vRequest, ARequest);
      ProcessCommands(vRequest, vResponse);

      AResponse.ContentStream := vResponse.TakeWireStream;
      vRequest.Finish;

      if vResponse.ContentEncription <> '' then
        vResponse.Params.AddParam('Content-Encription', vResponse.ContentEncription, rpkHEADER);
      if vResponse.ContentDisposition <> '' then
        vResponse.Params.AddParam('Content-Disposition', vResponse.ContentDisposition, rpkHEADER);
      vResponse.Params.AssignParams(AResponse.CustomHeaders, rpkHEADER, '=');
      vCookies := TStringList.Create;
      try
        vResponse.GetParamsCookies(vCookies, IncMinute(Now, CookieLife));
        for vInt := 0 to vCookies.Count - 1 do
          AddWebCookie(AResponse, GetRALCookieFromText(StringRAL(vCookies[vInt])));
      finally
        vCookies.Free;
      end;
      AResponse.ContentType := string(vResponse.ContentType);
      AResponse.ContentEncoding := string(vResponse.ContentEncoding);
      AResponse.StatusCode := vResponse.StatusCode;
      AResponse.FreeContentStream := True;
    except
      on E: Exception do
      begin
        AResponse.StatusCode := HTTP_InternalError;
        AResponse.ContentType := rctTEXTPLAIN;
        AResponse.Content := string(ErrorText(E));
        if Assigned(OnServerError) then
          OnServerError(E);
      end;
    end;
  finally
    vRequest.Free;
    vResponse.Free;
  end;
end;

{ TRALWebBrokerModule }

constructor TRALWebBrokerModule.Create(AOwner: TComponent);
var
  vItem: TWebActionItem;
begin
  inherited Create(AOwner);
  vItem := Actions.Add;
  vItem.Default := True;
  vItem.PathInfo := '/';
  vItem.OnAction := DefaultAction;
end;

procedure TRALWebBrokerModule.DefaultAction(Sender: TObject; ARequest: TWebRequest;
  AResponse: TWebResponse; var AHandled: Boolean);
begin
  AHandled := True;
  if gServer = nil then
  begin
    AResponse.StatusCode := HTTP_ServiceUnavailable;
    AResponse.ContentType := rctTEXTPLAIN;
    AResponse.Content := 'no active TRALWebBrokerServer';
    Exit;
  end;
  gServer.HandleWebRequest(ARequest, AResponse);
end;
