unit IndyRAL;

interface

uses
  Classes, SysUtils,
  RALServer, RALClient,
  IdHTTPServer, IdHTTP, IdContext, IdCustomHTTPServer, IdMultipartFormData,
  IdMessageCoder;

type
  TRALIndyServer = class(TRALServer)
  private
    FHttp : TIdHTTPServer;
  protected
    procedure SetActive(const Value: boolean); override;
    procedure DecodeParams(var ARequest : TRALRequest; AStream : TStream);


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
  RegisterComponents('RAL - Sockets', [TRALIndyServer]);
end;

{ TRALIndyServer }

constructor TRALIndyServer.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := TIdHTTPServer.Create(nil);
  FHttp.OnCommandGet := OnCommandProcess;
  FHttp.OnCommandOther := OnCommandProcess;
end;

procedure TRALIndyServer.DecodeParams(var ARequest: TRALRequest;
  AStream: TStream);
var
  vDecodeMsg : TIdMessageDecoder;
begin

end;

destructor TRALIndyServer.Destroy;
begin
  FHttp.Active := False;
  FHttp.Free;
  inherited;
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

      if SameText(ContentType,'text/plain') then begin
        Params.AddParam('ral_body',ARequestInfo.PostStream,'text/plain');
      end
      else if SameText(ContentType,'multipart/form-data') then begin
        DecodeParams(vRequest,ARequestInfo.PostStream);
      end
      else begin
        Params.AddParam('ral_body',ARequestInfo.PostStream,ContentType);
      end;
    end;

    vResponse := ProcessCommands(vRequest);

    try
      with AResponseInfo do begin
        ResponseNo := 200;
        ContentText := vResponse.Content;
        ContentType := vResponse.ContentType;
        ContentLength := vResponse.ContentSize;

        vInt := 0;
        while vInt < vResponse.Headers.Count do begin
          CustomHeaders.Add(vResponse.Headers.Strings[vInt]);
          vInt := vInt + 1;
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
