unit RALResponse;

interface

uses
  Classes, SysUtils,
  RALTypes, RALParams, RALMIMETypes;

type

  { TRALResponse }

  TRALResponse = class
  private
    FHeaders: TStringList;
    FBody: TRALParams;
    FContentType: StringRAL;
    FRespCode: IntegerRAL;
    FResponse : TRALParam;
  protected
    procedure SetContentType(const AValue: StringRAL);
    function GetResponseStream: TStream;
    function GetResponseText: StringRAL;
    procedure SetResponseStream(const AValue: TStream);
    procedure SetResponseText(const AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Answer(AStatusCode: IntegerRAL; AMessage: StringRAL;
                     AContentType: TRALContentType = TRALContentType.ctTEXTHTML);
    property Body: TRALParams read FBody;
    property ResponseText : StringRAL read GetResponseText write SetResponseText;
    property ResponseStream : TStream read GetResponseStream write SetResponseStream;
    property ContentType: StringRAL read FContentType write SetContentType;
    property Headers: TStringList read FHeaders write FHeaders;
    property RespCode: IntegerRAL read FRespCode write FRespCode;
  end;

implementation

{ TRALResponse }

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; AMessage: StringRAL;
  AContentType: TRALContentType);
begin
  RespCode := AStatusCode;
  ResponseText := AMessage;
end;

constructor TRALResponse.Create;
begin
  inherited;
  FHeaders := TStringList.Create;
  FContentType := TRALContentType.ctTEXTHTML;
  FBody := TRALParams.Create;
  FResponse := nil; // pertence ao body;
end;

destructor TRALResponse.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FBody);
  FResponse := nil; // pertence ao body;
  inherited;
end;

function TRALResponse.GetResponseStream: TStream;
begin
  Result := nil;
  if FResponse <> nil then
    Result := FResponse.AsStream;
end;

function TRALResponse.GetResponseText: StringRAL;
begin
  Result := '';
  if FResponse <> nil then
    Result := FResponse.AsString;
end;

procedure TRALResponse.SetContentType(const AValue: StringRAL);
begin
  FContentType := AValue;
  if (FResponse <> nil) and (FBody.Count = 1) then
    FResponse.ContentType := AValue;
end;

procedure TRALResponse.SetResponseStream(const AValue: TStream);
begin
  Body.ClearParams;
  if AValue.Size > 0 then begin
    FResponse := Body.AddValue(AValue);
    FResponse.ContentType := FContentType;
  end;
end;

procedure TRALResponse.SetResponseText(const AValue: StringRAL);
begin
  Body.ClearParams;
  if AValue <> '' then begin
    FResponse := Body.AddValue(AValue);
    FResponse.ContentType := FContentType;
  end;
end;

end.

