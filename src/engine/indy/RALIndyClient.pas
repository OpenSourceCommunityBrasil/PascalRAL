unit RALIndyClient;

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, IdHTTP, IdMultipartFormData, IdAuthentication, IdGlobal,
  RALClient, RALParams, RALTypes, RALConsts, RALAuthentication;

type

  { TRALIndyClient }

  TRALIndyClient = class(TRALClient)
  private
    FHttp: TIdHTTP;
    FHandlerSSL: TIdSSLIOHandlerSocketOpenSSL;
  protected
    function EncodeParams(AParams: TRALParams; var AFreeAfter : boolean): TStream;

    procedure SetUseSSL(const Value: boolean); override;
    procedure SetConnectTimeout(const AValue: IntegerRAL); override;
    procedure SetRequestTimeout(const AValue: IntegerRAL); override;
    procedure SetUserAgent(const AValue : StringRAL); override;

    function SendUrl(AURL: StringRAL; AMethod: TRALMethod;
                     AHeaders: TStringList = nil;
                     ABody: TRALParams = nil): IntegerRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALIndyClient }

constructor TRALIndyClient.Create(AOwner: TComponent);
begin
  inherited;
  FHttp := TIdHTTP.Create(nil);
  FHandlerSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  SetEngine('Indy ' + gsIdVersion);
end;

destructor TRALIndyClient.Destroy;
begin
  FreeAndNil(FHttp);
  FreeAndNil(FHandlerSSL);
  inherited;
end;

function TRALIndyClient.EncodeParams(AParams : TRALParams; var AFreeAfter : boolean) : TStream;
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
    Result := TIdMultiPartFormDataStream.Create;
    for vInt := 0 to AParams.Count - 1 do
    begin
      with Result as TIdMultiPartFormDataStream do
      begin
        AddFormField(AParams.Param[vInt].ParamName,
                     AParams.Param[vInt].ContentType,
                     '', // charset
                     AParams.Param[vInt].AsStream);
      end;
    end;
    AFreeAfter := True;
  end;
  Result.Position := 0;
end;

function TRALIndyClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod;
  AHeaders: TStringList; ABody: TRALParams): IntegerRAL;
var
  vInt: IntegerRAL;
  vSource, vResult : TStream;
  vStr1, vStr2: StringRAL;
  vFree : boolean;
begin
  inherited;
  FHttp.Request.Clear;
  FHttp.Request.CustomHeaders.Clear;
  FHttp.Request.CustomHeaders.FoldLines := False;
  FHttp.Request.UserAgent := UserAgent;

  FHttp.Response.Clear;

  if AHeaders <> nil then
  begin
    for vInt := 0 to AHeaders.Count - 1 do
    begin
      vStr1 := AHeaders.Names[vInt];
      vStr2 := AHeaders.ValueFromIndex[vInt];

      FHttp.Request.CustomHeaders.AddValue(vStr1, vStr2);
    end;
  end;

  vFree := False;
  vSource := EncodeParams(ABody, vFree);
  try
    vResult := TStringStream.Create;
    try
      case AMethod of
        amGET:
          FHttp.Get(AURL, vResult);

        amPOST:
          FHttp.Post(AURL, vSource, vResult);

        amPUT:
          FHttp.Put(AURL, vSource, vResult);

        amPATCH:
          FHttp.Patch(AURL, vSource, vResult);

        amDELETE:
          FHttp.Delete(AURL, vResult);
      end;
    except
      vResult.Size := 0;
      TStringStream(vResult).WriteString(FHttp.ResponseText);
    end;
    vResult.Position := 0;

    ResponseCode := FHttp.ResponseCode;
    SetResponse(vResult);
    Result := FHttp.ResponseCode;
  finally
    if vFree then
      FreeAndNil(vSource);
  end;
end;

procedure TRALIndyClient.SetConnectTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FHttp.ConnectTimeout := AValue;
end;

procedure TRALIndyClient.SetRequestTimeout(const AValue: IntegerRAL);
begin
  inherited;
  FHttp.ReadTimeout := AValue;
end;

procedure TRALIndyClient.SetUserAgent(const AValue : StringRAL);
begin
  inherited;
  FHttp.Request.UserAgent := AValue;
end;

procedure TRALIndyClient.SetUseSSL(const Value: boolean);
begin
  inherited;
  FHttp.IOHandler := nil;
  if Value then
    FHttp.IOHandler := FHandlerSSL;
end;

end.
