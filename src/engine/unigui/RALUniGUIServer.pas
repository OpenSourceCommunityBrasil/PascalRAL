/// Base unit for RALServer component using UniGUI Engine
unit RALUniGUIServer;

interface

uses
  Classes, SysUtils, DateUtils,
  UniGUIServer, uIdCustomHTTPServer, uIdContext, uIdCookie,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams, RALTools, RALStream;

type
  { TRALUniGUIServer }

  TRALUniGUIServer = class(TRALServer)
  private
    FOnHTTPCommand: TUniHTTPCommandEvent;
    FOnParseAuthentication: TIdHTTPParseAuthenticationEvent;
  protected
    procedure DecodeAuth(ARequest: TIdHTTPRequestInfo; AResult: TRALRequest);
    procedure SetActive(const AValue: boolean); override;

    procedure OnRALHTTPCommand(ARequestInfo: TIdHTTPRequestInfo;
                               AResponseInfo: TIdHTTPResponseInfo;
                               var Handled: Boolean);

    procedure OnRALParseAuthentication(AContext: TIdContext; const AAuthType,
                                       AAuthData: string; var VUsername,
                                       VPassword: string; var VHandled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALUniGUIServer }

constructor TRALUniGUIServer.Create(AOwner: TComponent);
begin
  inherited;
  SetEngine('UniGUI '+UniServerInstance.UniGUIVersion);
end;

procedure TRALUniGUIServer.DecodeAuth(ARequest: TIdHTTPRequestInfo;
  AResult: TRALRequest);
begin
  if not HasAuthentication then
    Exit;
  DecodeAuthValue(AResult, ARequest.RawHeaders.Values['Authorization']);
end;

destructor TRALUniGUIServer.Destroy;
begin
  Active := False;
  inherited;
end;

procedure TRALUniGUIServer.OnRALHTTPCommand(ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo; var Handled: Boolean);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vInt: IntegerRAL;
  vCookies: TStringList;
  vParam: TRALParam;
begin
  if Assigned(FOnHTTPCommand) then
    FOnHTTPCommand(ARequestInfo, AResponseInfo, Handled);

  if not SameText(Copy(ARequestInfo.Document,1,4),'/rug') then
  begin
    Handled := False;
    Exit;
  end;

  Handled := True;

  vRequest := CreateRequest;
  vResponse := CreateResponse;

  try
    with vRequest do
    begin
      ClientInfo.IP := ARequestInfo.RemoteIP;
      { ClientInfo.ConnectionID stays 0 on purpose: the server here is UniGUI's,
        RAL only hooks its events, and the hook hands over the request info
        without the context that owns the socket. Zero is the documented
        answer for "the engine cannot tell" - see TRALClientInfo.ConnectionID }
      ClientInfo.MACAddress := '';
      ClientInfo.UserAgent := ARequestInfo.UserAgent;

      ContentType := ARequestInfo.ContentType;
      ContentDisposition := ARequestInfo.ContentDisposition;
      ContentEncoding := ARequestInfo.ContentEncoding;
      AcceptEncoding := ARequestInfo.AcceptEncoding;
      ContentSize := ARequestInfo.ContentLength;

      Query := Copy(ARequestInfo.Document, 5, Length(ARequestInfo.Document));
      Query := FixRoute(Query);

      Method := HTTPMethodToRALMethod(ARequestInfo.Command);

      DecodeAuth(ARequestInfo, vRequest);

      Params.AppendParams(ARequestInfo.RawHeaders, rpkHEADER);
      Params.AppendParams(ARequestInfo.CustomHeaders, rpkHEADER);

      ContentEncription := ParamByName('Content-Encription').AsString;
      AcceptEncription := ParamByName('Accept-Encription').AsString;;

      ValidateRequest(vRequest, vResponse);
      if vResponse.StatusCode < HTTP_BadRequest then
      begin
        { each param with the kind of where it came from, decoded once.
          Indy's Params mixes the query string with a urlencoded form, both already
          decoded - and AppendParamLine decoded them again, so a '%2B' turned into a
          space. The raw texts go through the one parser; the cookies come from
          their header, as on every engine }
        Params.AppendParamsText(ARequestInfo.QueryParams, rpkQUERY);
        Params.AppendParamsText(ARequestInfo.FormParams, rpkFIELD);
        AddCookies(Params.GetKind['Cookie', rpkHEADER].AsString);

        { lent, not copied: the Indy under UniGUI frees PostStream after this
          callback returns, and nobody reads it again - so the cipher may work
          on it in place }
        SetWireBody(ARequestInfo.PostStream, boBorrowedWritable);

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

        { PostStream is NOT emptied here any more: it is the body now, and
          the params read from it until the request is freed }
        ARequestInfo.RawHeaders.Clear;
        ARequestInfo.CustomHeaders.Clear;
        ARequestInfo.Cookies.Clear;
        ARequestInfo.Params.Clear;
      end;
    end;

    ProcessCommands(vRequest, vResponse);

    with vResponse do
    begin
      { the body first, handed over at once so nothing can leak it: only
        after TakeWireStream do ContentEncoding and ContentType say what was
        really done }
      AResponseInfo.ContentText := '';
      AResponseInfo.ContentStream := TakeWireStream;
      AResponseInfo.FreeContentStream := AResponseInfo.ContentStream <> nil;

      AResponseInfo.ResponseNo := StatusCode;

      AResponseInfo.Server := 'RAL_UniGUI';
      AResponseInfo.ContentEncoding := ContentEncoding;
      AResponseInfo.ContentDisposition := ContentDisposition;

      vParam := Params.GetKind['WWW-Authenticate', rpkHEADER];
      if vParam <> nil then
      begin
        AResponseInfo.WWWAuthenticate.Add(RALSafeHeaderText(vParam.AsString));
        vResponse.Params.DelParam('WWW-Authenticate');
      end;

      if vResponse.AcceptEncoding <> '' then
        Params.AddParam('Accept-Encoding', vResponse.AcceptEncoding, rpkHEADER);

      if vResponse.ContentEncription <> '' then
        Params.AddParam('Content-Encription', vResponse.ContentEncription, rpkHEADER);

      Params.AssignParams(AResponseInfo.CustomHeaders, rpkHEADER, ': ');

      { every cookie whole, on its own Set-Cookie line, from the builder all
        engines share. A TIdCookie made a cookie CALLED Set-Cookie out of an
        AddCookie(TRALCookie) one, and lasted 30 minutes whatever CookieLife
        said }
      vCookies := TStringList.Create;
      try
        GetParamsCookies(vCookies, IncMinute(Now, CookieLife));
        for vInt := 0 to Pred(vCookies.Count) do
          AResponseInfo.CustomHeaders.AddValue('Set-Cookie', vCookies[vInt]);
      finally
        FreeAndNil(vCookies);
      end;

      AResponseInfo.ContentType := ContentType;
      AResponseInfo.ContentDisposition := ContentDisposition;

      AResponseInfo.ContentLength := 0;
      AResponseInfo.FreeContentStream := False;

      if AResponseInfo.ContentStream <> nil then begin
        AResponseInfo.ContentLength := AResponseInfo.ContentStream.Size;
        AResponseInfo.FreeContentStream := True;
      end;

      AResponseInfo.WriteContent;
    end;
  finally
    FreeAndNil(vResponse);
    FreeAndNil(vRequest);
  end;
end;

procedure TRALUniGUIServer.OnRALParseAuthentication(AContext: TIdContext;
  const AAuthType, AAuthData: string; var VUsername, VPassword: string;
  var VHandled: Boolean);
begin
  if Assigned(FOnParseAuthentication) then
    FOnParseAuthentication(AContext, AAuthType, AAuthData, VUsername, VPassword, VHandled);

  VHandled := True;
end;

procedure TRALUniGUIServer.SetActive(const AValue: boolean);
var
  vActive: boolean;
begin
  vActive := Active;

  inherited;

  if AValue = vActive then
    Exit;

  if AValue then
  begin
    FOnHTTPCommand := UniServerInstance.OnHTTPCommand;
    FOnParseAuthentication := UniServerInstance.OnParseAuthentication;

    UniServerInstance.OnHTTPCommand := OnRALHTTPCommand;
    UniServerInstance.OnParseAuthentication := OnRALParseAuthentication;
  end
  else begin
    UniServerInstance.OnHTTPCommand := FOnHTTPCommand;
    UniServerInstance.OnParseAuthentication := FOnParseAuthentication;
  end;
end;

end.
