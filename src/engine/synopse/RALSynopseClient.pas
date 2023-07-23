unit RALSynopseClient;

interface

uses
  Classes, SysUtils,
  mormot.net.client, mormot.core.base,
  RALClient, RALParams, RALTypes, RALConsts, RALAuthentication;

type

  { TRALSynopseClient }

  TRALSynopseClient = class(TRALClient)
  private
    FHttp : THttpClientSocket;
  protected
    function EncodeParams(AParams: TRALParams; var AFreeAfter : boolean): TStream;

    procedure SetConnectTimeout(const AValue: IntegerRAL); override;
    procedure SetRequestTimeout(const AValue: IntegerRAL); override;
    procedure SetUserAgent(const AValue : StringRAL); override;

    procedure SetUseSSL(const AValue: boolean); override;
    function SendUrl(AURL: StringRAL; AMethod: TRALMethod;
                     AHeaders: TStringList = nil;
                     ABody: TRALParams = nil): IntegerRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALSynopseClient }

constructor TRALSynopseClient.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := THttpClientSocket.Create;
  SetEngine('Synopse ' + SYNOPSE_FRAMEWORK_FULLVERSION);
end;

destructor TRALSynopseClient.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

function TRALSynopseClient.EncodeParams(AParams : TRALParams; var AFreeAfter : boolean) : TStream;
begin
  Result := nil;
  if AParams = nil then
    Exit;

  AFreeAfter := False;

  if AParams.Count = 1 then
  begin
    Result := AParams.Param[0].AsStream;
  end
  else if AParams.Count > 1 then
  begin
    // multipart todo
  end;
  Result.Position := 0;
end;

procedure TRALSynopseClient.SetConnectTimeout(const AValue : IntegerRAL);
begin
  inherited;
  FHttp.SendTimeout := AValue;
end;

procedure TRALSynopseClient.SetRequestTimeout(const AValue : IntegerRAL);
begin
  inherited;
  FHttp.ReceiveTimeout := AValue;
end;

procedure TRALSynopseClient.SetUserAgent(const AValue : StringRAL);
begin
  inherited;
  FHttp.UserAgent := AValue;
end;

function TRALSynopseClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod;
  AHeaders: TStringList; ABody: TRALParams): IntegerRAL;
var
  vInt: IntegerRAL;
  vSource, vResult : TStream;
  vStr1, vStr2: StringRAL;
  vFree : boolean;
  vHeader : StringRAL;
begin
  inherited;
  vHeader := '';
  if AHeaders <> nil then begin
    for vInt := 0 to AHeaders.Count - 1 do
      vHeader := vHeader + AHeaders.Names[vInt]+': '+AHeaders.ValueFromIndex[vInt] + #13#10;
  end;

  vHeader := vHeader + 'User-Agent: '+UserAgent;

  vFree := False;
  vSource := EncodeParams(ABody,vFree);
  try
    vResult := TStringStream.Create;
    try
      case AMethod of
        amGET:
          Result := FHttp.Request(AURL, 'Get', 0, vHeader, '', '', False, vSource, vResult);

        amPOST:
          Result := FHttp.Request(AURL, 'Post', 0, vHeader, '', '', False, vSource, vResult);

        amPUT:
          Result := FHttp.Request(AURL, 'Put', 0, vHeader, '', '', False, vSource, vResult);

        amPATCH:
          Result := FHttp.Request(AURL, 'Put', 0, vHeader, '', '', False, vSource, vResult);

        amDELETE:
          Result := FHttp.Request(AURL, 'Delete', 0, vHeader, '', '', False, vSource, vResult);
      end;
    except
      on e : Exception do begin
        vResult.Size := 0;
        TStringStream(vResult).WriteString(e.Message);
      end;
    end;
    vResult.Position := 0;

    ResponseCode := Result;
    SetResponse(vResult);
  finally
    if vFree then
      FreeAndNil(vSource);
  end;
end;

procedure TRALSynopseClient.SetUseSSL(const AValue: boolean);
begin
  inherited;
  FHttp.TLS.Enabled := AValue;
end;

end.
