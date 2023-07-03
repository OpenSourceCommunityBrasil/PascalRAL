unit IndyRAL;

interface

uses
  Classes, SysUtils,
  RALServer, RALClient,
  IdHTTPServer, IdHTTP, IdContext, IdCustomHTTPServer, IdMultipartFormData,
  IdMessageCoder, IdGlobalProtocols, IdMessageCoderMIME, IdGlobal;

// IdHTTPServer, IdCustomHTTPServer, IdContext, IdSocketHandle, IdGlobal,

type
  TRALIndyServer = class(TRALServer)
  private
    FHttp : TIdHTTPServer;
  protected
    procedure SetActive(const Value: boolean); override;
    procedure DecodeParams(var ARequest : TRALRequest; ARequestInfo : TIdHTTPRequestInfo);
    procedure EncodeParams(AResponse : TRALResponse; AResponseInfo : TIdHTTPResponseInfo);


    procedure OnCommandProcess(AContext: TIdContext;
                               ARequestInfo: TIdHTTPRequestInfo;
                               AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TRALIndyClient = class(TRALClient)
  private

  public

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALIndyServer]);
end;

{ TRALIndyServer }

constructor TRALIndyServer.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := TIdHTTPServer.Create(nil);
  FHttp.OnCommandGet := OnCommandProcess;
  FHttp.OnCommandOther := OnCommandProcess;
end;

procedure TRALIndyServer.DecodeParams(var ARequest: TRALRequest; ARequestInfo: TIdHTTPRequestInfo);
var
  vBoundary, vBoundaryStart, vBoundaryEnd,
  vLine, vName : string;

  vIdxName : integer;

  vDecoder, vReader : TIdMessageDecoder;
  vBoundaryFound, vIsStartBoundary, vMsgEnd : boolean;
  vStream : TMemoryStream;
begin
  vBoundary := ExtractHeaderSubItem(ARequest.ContentType, 'boundary', QuoteHTTP);
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
      if vLine = vBoundaryStart then begin
        vBoundaryFound := True;
        vIsStartBoundary := True;
      end
      else if vLine = vBoundaryEnd then begin
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
              vReader := vDecoder.ReadBody(vStream,vMsgEnd);
              vStream.Position := 0;

              vName := vDecoder.Headers.Values['name'];
              if vName <> '' then begin
                vName := 'ral_multpart'+IntToStr(vIdxName);
                vIdxName := vIdxName + 1;
              end;

              ARequest.Params.AddParam(vName,vStream);
            finally
              vStream.Free;
            end;

            vDecoder.Free;
            vDecoder := vReader;
          end;
        mcptIgnore:
          begin
            vDecoder.Free;
            vDecoder := TIdMessageDecoderMIME.Create(nil);
          end;
        mcptEOF:
          begin
            vDecoder.Free;
            vMsgEnd := True;
          end;
      end;
    until (vDecoder = nil) or vMsgEnd;
  finally
    vDecoder.Free;
  end;
end;

destructor TRALIndyServer.Destroy;
begin
  FHttp.Active := False;
  FHttp.Free;
  inherited;
end;

procedure TRALIndyServer.EncodeParams(AResponse: TRALResponse;
  AResponseInfo: TIdHTTPResponseInfo);
var
  vMultPart : TIdMultiPartFormDataStream;
  vInt : integer;
  vStream : TStream;
begin
  vMultPart := TIdMultiPartFormDataStream.Create;
  try
    vInt := 0;
    while vInt < AResponse.Body.Count do begin
      vStream := AResponse.Body.Param[vInt].AsStream;
      vMultPart.AddFormField(AResponse.Body.Param[vInt].ParamName,
                             AResponse.Body.Param[vInt].ContentType,
                             '', // charset
                             vStream);
      FreeAndNil(vStream);
      vInt := vInt + 1;
    end;

    vMultPart.Position := 0;
    AResponseInfo.ContentStream.Size := 0;
    AResponseInfo.ContentStream.CopyFrom(vMultPart,vMultPart.Size);
  finally
    vMultPart.Free;
  end;
end;

procedure TRALIndyServer.OnCommandProcess(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  vRequest : TRALRequest;
  vResponse : TRALResponse;

  vInt : integer;
  vStr1, vStr2 : string;
  vMultPart : TIdMultiPartFormDataStream;
begin
  vRequest := TRALRequest.Create;
  try
    with vRequest do begin
      ClientInfo.IP := ARequestInfo.RemoteIP;
      ClientInfo.MACAddress := '';
      ClientInfo.UserAgent := ARequestInfo.UserAgent;

      Query := ARequestInfo.Document;

      ContentType := ARequestInfo.ContentType;
      ContentSize := ARequestInfo.ContentLength;

      vInt := 0;
      while vInt < ARequestInfo.RawHeaders.Count do begin
        vStr1 := ARequestInfo.RawHeaders.Names[vInt];
        vStr2 := ARequestInfo.RawHeaders.ValueFromIndex[vInt];

        Params.AddParam(vStr1,vStr2);

        Headers.Add(vStr1+'='+vStr2);
        vInt := vInt + 1;
      end;

      vInt := 0;
      while vInt < ARequestInfo.CustomHeaders.Count do begin
        vStr1 := ARequestInfo.CustomHeaders.Names[vInt];
        vStr2 := ARequestInfo.CustomHeaders.ValueFromIndex[vInt];

        Params.AddParam(vStr1,vStr2);

        Headers.Add(vStr1+'='+vStr2);
        vInt := vInt + 1;
      end;

      if ARequestInfo.Params.Count > 0 then begin
        vInt := 0;
        while vInt < ARequestInfo.Params.Count do begin
          vStr1 := ARequestInfo.Params.Names[vInt];
          vStr2 := ARequestInfo.Params.ValueFromIndex[vInt];

          Params.AddParam(vStr1,vStr2);

          vInt := vInt + 1;
        end;
      end
      else begin
        vStr1 := ARequestInfo.QueryParams;
        if vStr1 = '' then
         vStr1 := ARequestInfo.UnparsedParams;
      end;

      if (ARequestInfo.PostStream <> nil) and (ARequestInfo.PostStream.Size > 0) then begin
        if SameText(ContentType,'text/plain') then begin
          Params.AddParam('ral_body',ARequestInfo.PostStream,'text/plain');
        end
        else if SameText(ContentType,'multipart/form-data') then begin
          DecodeParams(vRequest,ARequestInfo);
        end
        else begin
          Params.AddParam('ral_body',ARequestInfo.PostStream,ContentType);
        end;

        //limpando para economia de memoria
        ARequestInfo.PostStream.Size := 0;
      end;
    end;

    vResponse := ProcessCommands(vRequest);

    if (vResponse.Body.Count > 1) then
      vResponse.ContentType := 'multipart/form-data';

    try
      with AResponseInfo do begin
        ResponseNo := vResponse.RespCode;
        ContentType := vResponse.ContentType;

        vInt := 0;
        while vInt < vResponse.Headers.Count do begin
          CustomHeaders.Add(vResponse.Headers.Strings[vInt]);
          vInt := vInt + 1;
        end;

        if (vResponse.Body.Count > 0) then begin
          if SameText(ContentType,'text/plain') then begin
            ContentText := vResponse.Body.Param[0].AsString;
          end
          else if SameText(ContentType,'multipart/form-data') then begin
            EncodeParams(vResponse,AResponseInfo);
          end
          else begin
            if vResponse.Body.Param[0].IsString then
              ContentText := vResponse.Body.Param[0].AsString
            else
              ContentStream := vResponse.Body.Param[0].AsStream;
          end;
        end;

        WriteContent;
      end;
    finally
      vResponse.Free
    end;
  finally
    vRequest.Free;
  end;
end;

procedure TRALIndyServer.SetActive(const Value: boolean);
begin
  FHttp.DefaultPort := Port;
  FHttp.Active := Value;
  inherited;
end;

end.
