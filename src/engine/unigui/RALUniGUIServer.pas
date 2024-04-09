unit RALUniGUIServer;

interface

uses
  Classes, SysUtils,
  UniGUIServer, uIdCustomHTTPServer, uIdContext,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRequest, RALResponse,
  RALParams, RALTools;

type
  { TRALUniGUIServer }

  TRALUniGUIServer = class(TRALServer)
  private
    FOnHTTPCommand : TUniHTTPCommandEvent;
  protected
    procedure SetActive(const AValue: boolean); override;
    procedure DecodeAuth(ARequest: TIdHTTPRequestInfo; AResult: TRALRequest);

    procedure OnRALHTTPCommand(ARequestInfo: TIdHTTPRequestInfo;
                               AResponseInfo: TIdHTTPResponseInfo;
                               var Handled: Boolean);
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
var
  vStr, vAux: StringRAL;
  vInt: IntegerRAL;
begin
  if Authentication = nil then
    Exit;

  AResult.Authorization.AuthType := ratNone;
  AResult.Authorization.AuthString := '';

  vStr := ARequest.RawHeaders.Values['Authorization'];
  if vStr <> '' then begin
    vInt := Pos(' ', vStr);
    vAux := Trim(Copy(vStr, 1, vInt - 1));
    if SameText(vAux, 'Basic') then
      AResult.Authorization.AuthType := ratBasic
    else if SameText(vAux, 'Bearer') then
      AResult.Authorization.AuthType := ratBearer;
    AResult.Authorization.AuthString := Copy(vStr, vInt + 1, Length(vStr));
  end;
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
  vParam : TRALParam;
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
      ClientInfo.MACAddress := '';
      ClientInfo.UserAgent := ARequestInfo.UserAgent;

      ContentType := ARequestInfo.ContentType;
      ContentDisposition := ARequestInfo.ContentDisposition;
      ContentEncoding := ARequestInfo.ContentEncoding;
      AcceptEncoding := ARequestInfo.AcceptEncoding;
      ContentSize := ARequestInfo.ContentLength;

      Query := Copy(ARequestInfo.Document,5,Length(ARequestInfo.Document));
      Query := FixRoute(Query);

      Method := HTTPMethodToRALMethod(ARequestInfo.Command);

      DecodeAuth(ARequestInfo, vRequest);

      Params.AppendParams(ARequestInfo.RawHeaders, rpkHEADER);
      Params.AppendParams(ARequestInfo.CustomHeaders, rpkHEADER);

      ContentEncription := ParamByName('Content-Encription').AsString;
      AcceptEncription := ParamByName('Accept-Encription').AsString;;

      ValidateRequest(vRequest, vResponse);
      if vResponse.StatusCode < 400 then
      begin
        Params.AppendParams(ARequestInfo.Params, rpkQUERY);

        if ARequestInfo.Params.Count = 0 then begin
          Params.AppendParamsUrl(ARequestInfo.QueryParams, rpkQUERY);
          Params.AppendParamsUrl(ARequestInfo.UnparsedParams, rpkQUERY);
        end;

        Params.CompressType := ContentCompress;
        Params.CriptoOptions.CriptType := ContentCripto;
        Params.CriptoOptions.Key := CriptoOptions.Key;
        RequestStream := ARequestInfo.PostStream;

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

        // limpando para economia de memoria
        if (ARequestInfo.PostStream <> nil) then
          ARequestInfo.PostStream.Size := 0;

        ARequestInfo.RawHeaders.Clear;
        ARequestInfo.CustomHeaders.Clear;
        ARequestInfo.Cookies.Clear;
        ARequestInfo.Params.Clear;
      end;
    end;

    ProcessCommands(vRequest, vResponse);

    with vResponse do
    begin
      AResponseInfo.ResponseNo := StatusCode;

      AResponseInfo.Server := 'RAL_UniGUI';
      AResponseInfo.ContentEncoding := ContentEncoding;
      AResponseInfo.ContentDisposition := ContentDisposition;

      vParam := Params.GetKind['WWW-Authenticate', rpkHEADER];
      if vParam <> nil then
      begin
        AResponseInfo.WWWAuthenticate.Add(vParam.AsString);
        vResponse.Params.DelParam('WWW-Authenticate');
      end;

      if vResponse.AcceptEncoding <> '' then
        Params.AddParam('Accept-Encoding', vResponse.AcceptEncoding, rpkHEADER);

      if vResponse.ContentEncription <> '' then
        Params.AddParam('Content-Encription', vResponse.ContentEncription, rpkHEADER);

      Params.AssignParams(AResponseInfo.CustomHeaders, rpkHEADER, ': ');

      AResponseInfo.ContentText := '';
      AResponseInfo.ContentStream := ResponseStream;
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
    UniServerInstance.OnHTTPCommand := OnRALHTTPCommand;
  end
  else begin
    UniServerInstance.OnHTTPCommand := FOnHTTPCommand;
  end;
end;

end.
