unit RALnetHTTPClient;

interface

uses
  Classes, SysUtils,
  System.Net.HttpClient, System.Net.HttpClientComponent, System.Net.UrlClient,
  RALClient, RALParams, RALTypes, RALConsts, RALAuthentication;

type

  { TRALnetHTTPClient }

  TRALnetHTTPClient = class(TRALClient)
  private
    FHttp: TNetHTTPClient;
  protected
    function EncodeParams(AParams: TRALParams; var AFreeAfter : boolean): TStream;
    procedure SetUserAgent(const AValue : StringRAL); override;
    procedure SetConnectTimeout(const Value: IntegerRAL); override;
    procedure SetRequestTimeout(const Value: IntegerRAL); override;
    procedure SetUseSSL(const Value: boolean); override;

    function SendUrl(AURL: StringRAL; AMethod: TRALMethod;
                     AHeaders: TStringList = nil;
                     ABody: TRALParams = nil): IntegerRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALnetHTTPClient }

constructor TRALnetHTTPClient.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := TNetHTTPClient.Create(nil);
  SetEngine('netHTTP');
end;

destructor TRALnetHTTPClient.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

function TRALnetHTTPClient.EncodeParams(AParams : TRALParams; var AFreeAfter : boolean) : TStream;
var
  vInt: IntegerRAL;
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
    // todo
  end;
  Result.Position := 0;
end;

function TRALnetHTTPClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod;
  AHeaders: TStringList; ABody: TRALParams): IntegerRAL;
var
  vInt: IntegerRAL;
  vSource, vResult : TStream;
  vStr1, vStr2: StringRAL;
  vFree : boolean;
  vHeaders : TNetHeaders;
  vReponse : IHTTPResponse;
begin
  inherited;

  if AHeaders <> nil then
  begin
    SetLength(vHeaders,AHeaders.Count);
    for vInt := 0 to AHeaders.Count - 1 do
    begin
      vStr1 := AHeaders.Names[vInt];
      vStr2 := AHeaders.ValueFromIndex[vInt];

      vHeaders[vInt] := TNameValuePair.Create(vStr1, vStr2);
    end;
  end;

  vFree := False;
  vSource := EncodeParams(ABody,vFree);
  try
    vResult := TStringStream.Create;
    try
      case AMethod of
        amGET    : vReponse := FHttp.Get(AURL, vResult, vHeaders);
        amPOST   : vReponse := FHttp.Post(AURL, vSource, vResult, vHeaders);
        amPUT    : vReponse := FHttp.Put(AURL, vSource, vResult, vHeaders);
        amPATCH  : vReponse := FHttp.Patch(AURL, vSource, vResult, vHeaders);
        amDELETE : vReponse := FHttp.Delete(AURL, vResult, vHeaders);
        amTRACE  : vReponse := FHttp.Trace(AURL, vResult, vHeaders);
        amHEAD   : vReponse := FHttp.Head(AURL, vHeaders);
        amOPTION : vReponse := FHttp.Options(AURL, vResult, vHeaders);
      end;
    except
      vResult.Size := 0;
      TStringStream(vResult).WriteString(vReponse.ContentAsString);
    end;
    vResult.Position := 0;

    ResponseCode := vReponse.GetStatusCode;
    SetResponse(vResult);
    Result := vReponse.GetStatusCode;
  finally
    if vFree then
      FreeAndNil(vSource);
  end;
end;

procedure TRALnetHTTPClient.SetConnectTimeout(const Value: IntegerRAL);
begin
  inherited;
  FHttp.ConnectionTimeout := Value;
end;

procedure TRALnetHTTPClient.SetRequestTimeout(const Value: IntegerRAL);
begin
  inherited;
  FHttp.ResponseTimeout := Value;
end;

procedure TRALnetHTTPClient.SetUserAgent(const AValue: StringRAL);
begin
  inherited;
  FHttp.UserAgent := AValue
end;

procedure TRALnetHTTPClient.SetUseSSL(const Value: boolean);
begin
  inherited;
  FHttp.SecureProtocols := [];
  if Value then
    FHttp.SecureProtocols := [THTTPSecureProtocol.SSL2,
                              THTTPSecureProtocol.SSL3,
                              THTTPSecureProtocol.TLS1,
                              THTTPSecureProtocol.TLS11,
                              THTTPSecureProtocol.TLS12]
end;

end.
