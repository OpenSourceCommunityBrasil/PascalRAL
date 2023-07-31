unit RALfpHTTPClient;

interface

uses
  Classes, SysUtils,
  fphttpclient, fphttp,
  RALClient, RALRoutes, RALTypes, RALConsts, RALAuthentication, RALParams,
  RALRequest;

type

  { TRALfpHttpClient }

  TRALfpHttpClient = class(TRALClient)
  private
    FHttp : TFPHTTPClient;
  protected
    function EncodeParams(AParams: TRALParams; var AFreeAfter : boolean): TStream;
    procedure SetConnectTimeout(const Value: IntegerRAL); override;
    procedure SetRequestTimeout(const Value: IntegerRAL); override;

    function SendUrl(AURL: StringRAL; AMethod: TRALMethod;
                     AHeaders: TStringList = nil;
                     ABody: TRALParams = nil): IntegerRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALfpHttpClient }

constructor TRALfpHttpClient.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := TFPHTTPClient.Create(nil);
  SetEngine('fpHTTP');
end;

destructor TRALfpHttpClient.Destroy;
begin
  FreeAndNil(FHttp);
  inherited;
end;

function TRALfpHttpClient.EncodeParams(AParams : TRALParams; var AFreeAfter : boolean) : TStream;
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

procedure TRALfpHttpClient.SetConnectTimeout(const Value: IntegerRAL);
begin
  inherited;
  FHttp.ConnectTimeout := Value;
end;

procedure TRALfpHttpClient.SetRequestTimeout(const Value: IntegerRAL);
begin
  inherited;
  FHttp.IOTimeout := Value;
end;

function TRALfpHttpClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod;
  AHeaders: TStringList; ABody: TRALParams): IntegerRAL;
var
  vInt: IntegerRAL;
  vSource, vResult : TStream;
  vStr1, vStr2: StringRAL;
  vFree : boolean;
begin
  inherited;

  if AHeaders <> nil then
  begin
    for vInt := 0 to AHeaders.Count - 1 do
    begin
      vStr1 := AHeaders.Names[vInt];
      vStr2 := AHeaders.ValueFromIndex[vInt];

      FHttp.RequestHeaders.AddPair(vStr1, vStr2);
    end;
  end;

  FHttp.RequestHeaders.AddPair('User-Agent',UserAgent);

  vFree := False;
  vSource := EncodeParams(ABody,vFree);
  try
    FHttp.RequestBody := vSource;
    vResult := TStringStream.Create;
    try
      case AMethod of
        amGET    : FHttp.Get(AURL, vResult);
        amPOST   : FHttp.Post(AURL, vResult);
        amPUT    : FHttp.Put(AURL, vResult);
        amPATCH  : FHttp.HTTPMethod('PATCH',AURL,vResult,[]); // sem funcao
        amDELETE : FHttp.Delete(AURL, vResult);
        amTRACE  : FHttp.HTTPMethod('TRACE',AURL,vResult,[]); // sem funcao
        amHEAD   : FHttp.HTTPMethod('HEAD',AURL,vResult,[]); // trata diferente
        amOPTION : FHttp.Options(AURL, vResult);
      end;
    except
      vResult.Size := 0;
      TStringStream(vResult).WriteString(FHttp.ResponseStatusText);
    end;
    vResult.Position := 0;

    ResponseCode := FHttp.ResponseStatusCode;
    SetResponse(vResult);
    Result := FHttp.ResponseStatusCode;
  finally
    if vFree then
      FreeAndNil(vSource);
  end;
end;

end.
