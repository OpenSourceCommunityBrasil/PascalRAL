unit RALIndyServer;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, IdHTTPServer, IdCustomHTTPServer, IdContext, IdMessageCoder,
  IdGlobalProtocols, IdMessageCoderMIME, IdGlobal, IdMultipartFormData,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRoutes;

type
  TRALIndySSL = class(TRALSSL)
  private
    FSSLOptions: TIdSSLOptions;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property SSLOptions: TIdSSLOptions read FSSLOptions write FSSLOptions;
  end;

  TRALIndyServer = class(TRALServer)
  private
    FHttp: TIdHTTPServer;
    FHandlerSSL: TIdServerIOHandlerSSLOpenSSL;
  protected
    procedure SetActive(const Value: boolean); override;
    procedure SetPort(const Value: IntegerRAL); override;
    function CreateRALSSL: TRALSSL; override;
    procedure DecodeParams(var ARequest: TRALRequest; ARequestInfo: TIdHTTPRequestInfo);
    procedure EncodeParams(AResponse: TRALResponse; AResponseInfo: TIdHTTPResponseInfo);
    procedure DecodeAuth(var ARequest: TRALRequest; AParam, AValue : StringRAL);
    procedure OnCommandProcess(AContext: TIdContext;
                               ARequestInfo: TIdHTTPRequestInfo;
                               AResponseInfo: TIdHTTPResponseInfo);
    procedure OnParseAuthentication(AContext: TIdContext;
                                    const AAuthType, AAuthData: String;
                                    var VUsername, VPassword: String;
                                    var VHandled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALIndyServer }

constructor TRALIndyServer.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := TIdHTTPServer.Create(nil);
  FHttp.OnCommandGet := OnCommandProcess;
  FHttp.OnCommandOther := OnCommandProcess;
  FHttp.OnParseAuthentication := OnParseAuthentication;
  FHandlerSSL := TIdServerIOHandlerSSLOpenSSL.Create(nil);
end;

function TRALIndyServer.CreateRALSSL: TRALSSL;
begin
  Result := TRALIndySSL.Create;
end;

procedure TRALIndyServer.DecodeAuth(var ARequest: TRALRequest; AParam,
  AValue: StringRAL);
var
  vInt : IntegerRAL;
  vAuthType : StringRAL;
begin
  if not SameText(AParam,'Authorization') then
    Exit;

  vAuthType := '';

  vInt := Pos(' ',AValue);
  if vInt > 0 then begin
    vAuthType := Copy(AValue,1,vInt-1);
    Delete(AValue,1,vInt);
  end;

  if vAuthType <> '' then begin
    ARequest.Authorization.AuthString := AValue;
    if SameText(vAuthType,'basic') then
      ARequest.Authorization.AuthType := ratBasic
    else if SameText(vAuthType,'bearer') then
      ARequest.Authorization.AuthType := ratBearer
  end;
end;

procedure TRALIndyServer.DecodeParams(var ARequest: TRALRequest;
  ARequestInfo: TIdHTTPRequestInfo);
var
  vBoundary, vBoundaryStart, vBoundaryEnd, vLine, vName: StringRAL;
  vIdxName: IntegerRAL;
  vDecoder, vReader: TIdMessageDecoder;
  vBoundaryFound, vIsStartBoundary, vMsgEnd: boolean;
  vStream: TMemoryStream;
begin
  vBoundary := ExtractHeaderSubItem(ARequestInfo.ContentType, 'boundary', QuoteHTTP);
  if vBoundary = '' then
    Exit;
  vBoundaryStart := '--' + vBoundary;
  vBoundaryEnd := vBoundaryStart + '--';
  vDecoder := TIdMessageDecoderMIME.Create(nil);
  try
    TIdMessageDecoderMIME(vDecoder).MIMEBoundary := vBoundary;
    vDecoder.SourceStream := ARequestInfo.PostStream;
    vDecoder.FreeSourceStream := False;
    vBoundaryFound := False;
    vIsStartBoundary := False;
    repeat
      vLine := ReadLnFromStream(ARequestInfo.PostStream, -1, True);
      if vLine = vBoundaryStart then
      begin
        vBoundaryFound := True;
        vIsStartBoundary := True;
      end
      else if vLine = vBoundaryEnd then
      begin
        vBoundaryFound := True;
      end;
    until vBoundaryFound;
    if (not vBoundaryFound) or (not vIsStartBoundary) then
      Exit;
    vIdxName := 1;
    vMsgEnd := False;
    repeat
      TIdMessageDecoderMIME(vDecoder).MIMEBoundary := vBoundary;
      vDecoder.SourceStream := ARequestInfo.PostStream;
      vDecoder.FreeSourceStream := False;
      vDecoder.ReadHeader;
      case vDecoder.PartType of
        mcptText, mcptAttachment:
        begin
            vStream := TMemoryStream.Create;
            try
              vReader := vDecoder.ReadBody(vStream, vMsgEnd);
              vStream.Position := 0;
              vName := vDecoder.Headers.Values['name'];
              if vName = '' then
              begin
                vName := 'ral_multpart' + IntToStr(vIdxName);
                vIdxName := vIdxName + 1;
              end;
              ARequest.Params.AddParam(vName, vStream);
            finally
              FreeAndNil(vStream);
            end;
            FreeAndNil(vDecoder);
            vDecoder := vReader;
        end;
        mcptIgnore:
        begin
            FreeAndNil(vDecoder);
            vDecoder := TIdMessageDecoderMIME.Create(nil);
        end;
        mcptEOF:
        begin
            FreeAndNil(vDecoder);
            vMsgEnd := True;
        end;
      end;
    until (vDecoder = nil) or vMsgEnd;
  finally
    FreeAndNil(vDecoder);
  end;
end;

destructor TRALIndyServer.Destroy;
begin
  if FHttp.Active then
    FHttp.Active := False;
  FreeAndNil(FHttp);
  FreeAndNil(FHandlerSSL);
  inherited;
end;

procedure TRALIndyServer.EncodeParams(AResponse: TRALResponse;
  AResponseInfo: TIdHTTPResponseInfo);
var
  vMultPart: TIdMultiPartFormDataStream;
  vInt: integer;
  vStream: TStream;
begin
  vMultPart := TIdMultiPartFormDataStream.Create;
  try
    vInt := 0;
    while vInt < AResponse.Body.Count do
    begin
      vStream := AResponse.Body.Param[vInt].AsStream;
      vMultPart.AddFormField(AResponse.Body.Param[vInt].ParamName,
                             AResponse.Body.Param[vInt].ContentType,
                             '', // charset
                             vStream);
      vInt := vInt + 1;
    end;
    vMultPart.Position := 0;
    AResponseInfo.ContentStream.Size := 0;
    AResponseInfo.ContentStream.CopyFrom(vMultPart, vMultPart.Size);
  finally
    FreeAndNil(vMultPart);
  end;
end;

procedure TRALIndyServer.OnCommandProcess(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vInt: integer;
  vStr1, vStr2: StringRAL;
  vMultPart: TIdMultiPartFormDataStream;
begin
  vRequest := TRALRequest.Create;
  try
    with vRequest do
    begin
      ClientInfo.IP := ARequestInfo.RemoteIP;
      ClientInfo.MACAddress := '';
      ClientInfo.UserAgent := ARequestInfo.UserAgent;
      Query := ARequestInfo.Document;
      case ARequestInfo.CommandType of
        hcUnknown, hcHEAD, hcGET, hcTRACE:
          Method := amGET;
        hcPOST:
          Method := amPOST;
        hcDELETE:
          Method := amDELETE;
        hcPUT:
          Method := amPUT;
        hcOPTION:
          Method := amOPTION;
      end;
      ContentType := ExtractHeaderItem(ARequestInfo.ContentType);
      ContentSize := ARequestInfo.ContentLength;
      if AContext.Data is TRALAuthClient then begin
        Authorization.AuthType := TRALAuthClient(AContext.Data).AuthType;
        Authorization.AuthString := TRALAuthClient(AContext.Data).AuthString;
        AContext.Data.Free;
        AContext.Data := nil;
      end;
      vInt := 0;
      while vInt < ARequestInfo.RawHeaders.Count do
      begin
        vStr1 := ARequestInfo.RawHeaders.Names[vInt];
        vStr2 := ARequestInfo.RawHeaders.Values[vStr1];
//        DecodeAuth(vRequest,vStr1,vStr2);
        Params.AddParam(vStr1, vStr2);
        Headers.Add(vStr1 + '=' + vStr2);
        vInt := vInt + 1;
      end;
      vInt := 0;
      while vInt < ARequestInfo.CustomHeaders.Count do
      begin
        vStr1 := ARequestInfo.CustomHeaders.Names[vInt];
        vStr2 := ARequestInfo.CustomHeaders.Values[vStr1];
//        DecodeAuth(vRequest,vStr1,vStr2);
        Params.AddParam(vStr1, vStr2);
        Headers.Add(vStr1 + '=' + vStr2);
        vInt := vInt + 1;
      end;
      if ARequestInfo.Params.Count > 0 then
      begin
        vInt := 0;
        while vInt < ARequestInfo.Params.Count do
        begin
          vStr1 := ARequestInfo.Params.Names[vInt];
          vStr2 := ARequestInfo.Params.ValueFromIndex[vInt];
          Params.AddParam(vStr1, vStr2);
          vInt := vInt + 1;
        end;
      end
      else
      begin
        vStr1 := ARequestInfo.QueryParams;
        if vStr1 = '' then
          vStr1 := ARequestInfo.UnparsedParams;
        //TODO
      end;
      if (ARequestInfo.PostStream <> nil) and (ARequestInfo.PostStream.Size > 0) then
      begin
        if SameText(ContentType, TRALContentType.ctMULTIPARTFORMDATA) then
        begin
          DecodeParams(vRequest, ARequestInfo);
        end
        else
        begin
          Params.AddParam('ral_body', ARequestInfo.PostStream, ContentType);
        end;
        // limpando para economia de memoria
        ARequestInfo.PostStream.Size := 0;
      end;
    end;
    vResponse := ProcessCommands(vRequest);
    if (vResponse.Body.Count > 1) then
      vResponse.ContentType := TRALContentType.ctMULTIPARTFORMDATA;
    try
      with AResponseInfo do
      begin
        ResponseNo := vResponse.RespCode;
        ContentType := vResponse.ContentType;
        vInt := 0;
        while vInt < vResponse.Headers.Count do
        begin
          CustomHeaders.Add(vResponse.Headers.Strings[vInt]);
          vInt := vInt + 1;
        end;
        if (vResponse.Body.Count > 0) then begin
          if SameText(ContentType, TRALContentType.ctMULTIPARTFORMDATA) then
          begin
            EncodeParams(vResponse, AResponseInfo);
          end
          else
          begin
            ContentStream := vResponse.Body.Param[0].AsStream;
          end;
        end;
        WriteContent;
      end;
    finally
      FreeAndNil(vResponse);
    end;
  finally
    FreeAndNil(vRequest);
  end;
end;

procedure TRALIndyServer.OnParseAuthentication(AContext: TIdContext;
  const AAuthType, AAuthData: String; var VUsername, VPassword: String;
  var VHandled: Boolean);
var
  vAuth : TRALAuthClient;
begin
  VHandled := False;
  if Authentication <> nil then begin
    case Authentication.AuthType of
      ratBasic  : VHandled := SameText(AAuthType,'basic');
      ratBearer : VHandled := SameText(AAuthType,'bearer');
    end;

    if VHandled then begin
      vAuth := TRALAuthClient.Create;
      vAuth.AuthType := Authentication.AuthType;
      vAuth.AuthString := AAuthData;

      AContext.Data := vAuth;
    end;
  end;
end;

procedure TRALIndyServer.SetActive(const Value: boolean);
begin
  if Assigned(SSL) then
    FHandlerSSL.SSLOptions.Assign(TRALIndySSL(SSL).SSLOptions);
  FHttp.IOHandler := nil;
  if (Assigned(SSL)) and (SSL.Enabled) then
    FHttp.IOHandler := FHandlerSSL;
  FHttp.Active := Value;
  inherited;
end;

procedure TRALIndyServer.SetPort(const Value: IntegerRAL);
var
  vActive: boolean;
begin
  inherited;
  vActive := Self.Active;
  Active := False;
  FHttp.DefaultPort := Value;
  FHttp.Bindings.Clear;
  with FHttp.Bindings.Add do begin
    IP := '0.0.0.0';
    Port := Value;
    IPVersion := Id_IPv4;
  end;
  with FHttp.Bindings.Add do begin
    IP := '::';
    Port := Value;
    IPVersion := Id_IPv6;
  end;
  Active := vActive;
end;

{ TRALIndySSL }

constructor TRALIndySSL.Create;
begin
  inherited;
  FSSLOptions := TIdSSLOptions.Create;
end;

destructor TRALIndySSL.Destroy;
begin
  FreeAndNil(FSSLOptions);
  inherited;
end;

end.
