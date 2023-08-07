unit RALResponse;

interface

uses
  Classes, SysUtils,
  RALTypes, RALParams, RALMIMETypes, RALConsts;

type

  { TRALResponse }

  TRALResponse = class
  private
    FParams: TRALParams;
    FContentType: StringRAL;
    FRespCode: IntegerRAL;
    FFreeContent: boolean;
  protected
    function GetResponseStream: TStream;
    function GetResponseText: StringRAL;
    procedure SetResponseStream(const AValue: TStream);
    procedure SetResponseText(const AValue: StringRAL);
    procedure SetContentType(const AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Answer(AStatusCode: IntegerRAL; AMessage: StringRAL;
                     AContentType: StringRAL = rctTEXTHTML);
    property Params: TRALParams read FParams;
    property ResponseText: StringRAL read GetResponseText write SetResponseText;
    property ResponseStream: TStream read GetResponseStream write SetResponseStream;
    property ContentType: StringRAL read FContentType write SetContentType;
    property StatusCode: IntegerRAL read FRespCode write FRespCode;
    property FreeContent: boolean read FFreeContent;
  end;

implementation

{ TRALResponse }

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; AMessage: StringRAL;
  AContentType: StringRAL);
begin
  StatusCode := AStatusCode;
  ContentType := AContentType;
  ResponseText := AMessage;
end;

constructor TRALResponse.Create;
begin
  inherited;
  ContentType := rctTEXTHTML;
  FParams := TRALParams.Create;
  FFreeContent := False;
end;

destructor TRALResponse.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

function TRALResponse.GetResponseStream: TStream;
begin
  Result := Params.EncodeBody(FContentType, FFreeContent);
end;

function TRALResponse.GetResponseText: StringRAL;
var
  vStream: TStream;
begin
  Result := '';
  vStream := Params.EncodeBody(FContentType, FFreeContent);
  if vStream <> nil then
  begin
    vStream.Position := 0;
    if vStream is TStringStream then begin
      Result := TStringStream(vStream).DataString;
    end
    else begin
      SetLength(Result, vStream.Size);
      vStream.Read(Result[PosIniStr], vStream.Size);
    end;

    if FFreeContent then
      vStream.Free;

    FFreeContent := False;
  end;
end;

procedure TRALResponse.SetContentType(const AValue: StringRAL);
begin
  FContentType := AValue;
  if Pos('charset=', FContentType) = 0 then
    FContentType := FContentType + '; charset=utf-8';
end;

procedure TRALResponse.SetResponseStream(const AValue: TStream);
var
  vParam: TRALParam;
begin
  FParams.ClearParams(rpkBODY);
  if AValue.Size > 0 then begin
    vParam := FParams.AddValue(AValue);
    vParam.ContentType := ContentType;
    vParam.Kind := rpkBODY;
  end;
end;

procedure TRALResponse.SetResponseText(const AValue: StringRAL);
var
  vParam: TRALParam;
begin
  FParams.ClearParams(rpkBODY);
  if AValue <> '' then begin
    vParam := FParams.AddValue(AValue);
    vParam.ContentType := ContentType;
    vParam.Kind := rpkBODY;
  end;
end;

end.

