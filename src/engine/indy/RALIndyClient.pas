unit RALIndyClient;

interface

uses
  Classes, SysUtils,
  IdSSLOpenSSL, IdHTTP, IdMultipartFormData, IdAuthentication,
  RALClient, RALParams, RALTypes, RALConsts, RALAuthentication;

type
  TRALIndyClient = class(TRALClient)
  private
    FHttp: TIdHTTP;
    FHandlerSSL: TIdSSLIOHandlerSocketOpenSSL;
  protected
    function EncodeParams(AParams: TRALParams): TStream;

    procedure SetUseSSL(const Value: boolean); override;
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
end;

destructor TRALIndyClient.Destroy;
begin
  FreeAndNil(FHttp);
  FreeAndNil(FHandlerSSL);
  inherited;
end;

function TRALIndyClient.EncodeParams(AParams: TRALParams): TStream;
var
  vInt : IntegerRAL;
  vStream : TStream;
  vField : TIdFormDataField;
begin
  Result := nil;
  if AParams = nil then
    Exit;

  if AParams.Count = 0 then
    Exit;

  if AParams.Count = 1 then
  begin
    Result := AParams.Param[0].AsStream;
  end
  else
  begin
    Result := TIdMultiPartFormDataStream.Create;
    vInt := 0;
    while vInt < AParams.Count do
    begin
      vStream := AParams.Param[vInt].AsStream;
      with Result as TIdMultiPartFormDataStream do
      begin
        vField := AddFormField(AParams.Param[vInt].ParamName,
                               AParams.Param[vInt].ContentType,
                               '', // charset
                               vStream);
      end;
      vInt := vInt + 1;
    end;
  end;
  Result.Position := 0;
end;

function TRALIndyClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod;
  AHeaders: TStringList; ABody: TRALParams): IntegerRAL;
var
  vInt : IntegerRAL;
  vSource, vResult, vContent : TStream;
  vStr1, vStr2 : StringRAL;
begin
  inherited;
  FHttp.Request.Clear;
  FHttp.Request.CustomHeaders.FoldLines := False;
//  FHttp.Request.ContentType := '';

  if AHeaders <> nil then
  begin
    vInt := 0;
    while vInt < AHeaders.Count do
    begin
      vStr1 := AHeaders.Names[vInt];
      vStr2 := AHeaders.ValueFromIndex[vInt];
      FHttp.Request.CustomHeaders.AddValue(vStr1,vStr2);
      vInt := vInt + 1;
    end;
  end;

  vSource := EncodeParams(ABody);
  try
    vContent := TMemoryStream.Create;
    try
      try
        case AMethod of
          amGET : begin
            FHttp.Get(AURL,vContent);
          end;
          amPOST : begin
            FHttp.Post(AURL,vSource,vContent);
          end;
          amPUT : begin
            FHttp.Put(AURL,vSource,vContent);
          end;
          amPATCH : begin
            FHttp.Patch(AURL,vSource,vContent);
          end;
          amDELETE : begin
            FHttp.Delete(AURL,vContent);
          end;
        end;
        vContent.Position := 0;

        if Pos('text/',LowerCase(FHttp.Response.ContentType)) = 1 then
        begin
          vResult := TStringStream.Create;
          vResult.CopyFrom(vContent, vContent.Size);
        end
        else
        begin
          vResult := TMemoryStream.Create;
          vResult.CopyFrom(vContent, vContent.Size);
        end;
      except
        vResult := TStringStream.Create(FHttp.ResponseText);
      end;
      vResult.Position := 0;

      ResponseCode := FHttp.ResponseCode;
      SetResponse(vResult);
      Result := FHttp.ResponseCode;
    finally
      FreeAndNil(vContent);
    end;
  finally
    FreeAndNil(vSource);
  end;
end;

procedure TRALIndyClient.SetUseSSL(const Value: boolean);
begin
  inherited;
  FHttp.IOHandler := nil;
  if Value then
    FHttp.IOHandler := FHandlerSSL;
end;

end.
