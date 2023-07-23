unit RALResponse;

interface

uses
  Classes, SysUtils,
  RALTypes, RALParams, RALMIMETypes;

type

  { TRALResponse }

  TRALResponse = class
  private
    FParams : TRALParams;
    FContentType: StringRAL;
    FRespCode: IntegerRAL;
  protected
    function GetResponseStream: TStream;
    function GetResponseText: StringRAL;
    procedure SetResponseStream(const AValue: TStream);
    procedure SetResponseText(const AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Answer(AStatusCode: IntegerRAL; AMessage: StringRAL;
                     AContentType: TRALContentType = TRALContentType.ctTEXTHTML);
    property Params : TRALParams read FParams;
    property ResponseText : StringRAL read GetResponseText write SetResponseText;
    property ResponseStream : TStream read GetResponseStream write SetResponseStream;
    property ContentType: StringRAL read FContentType write FContentType;
    property RespCode: IntegerRAL read FRespCode write FRespCode;
  end;

implementation

{ TRALResponse }

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; AMessage: StringRAL;
  AContentType: TRALContentType);
begin
  RespCode := AStatusCode;
  ResponseText := AMessage;
  FContentType := AContentType;
end;

constructor TRALResponse.Create;
begin
  inherited;
  FContentType := TRALContentType.ctTEXTHTML;
  FParams := TRALParams.Create;
end;

destructor TRALResponse.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

function TRALResponse.GetResponseStream: TStream;
begin
  Result := nil;
end;

function TRALResponse.GetResponseText: StringRAL;
begin
  Result := Params.Param[0].AsString;
end;

procedure TRALResponse.SetResponseStream(const AValue: TStream);
var
  vParam : TRALParam;
begin
  FParams.ClearParams(rpkBODY);
  if AValue.Size > 0 then begin
    vParam := FParams.AddValue(AValue);
    vParam.ContentType := FContentType;
  end;
end;

procedure TRALResponse.SetResponseText(const AValue: StringRAL);
var
  vParam : TRALParam;
begin
  FParams.ClearParams(rpkBODY);
  if AValue <> '' then begin
    vParam := FParams.AddValue(AValue);
    vParam.ContentType := FContentType;
  end;
end;

end.

