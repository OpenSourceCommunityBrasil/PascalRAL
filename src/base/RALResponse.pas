unit RALResponse;

interface

uses
  Classes, SysUtils,
  RALTypes, RALParams, RALMIMETypes, RALCustomObjects, RALStream;

type

  { TRALResponse }

  TRALResponse = class(TRALHTTPHeaderInfo)
  private
    FContentType: StringRAL;
    FStatusCode: IntegerRAL;
    FFreeContent: boolean;
    FCriptoKey: StringRAL;
  protected
    function GetResponseStream: TStream;
    function GetResponseText: StringRAL;
    procedure SetResponseStream(const AValue: TStream);
    procedure SetResponseText(const AValue: StringRAL);
    procedure SetContentType(const AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Answer(AStatusCode: IntegerRAL; const AMessage: StringRAL;
                     const AContentType: StringRAL = rctAPPLICATIONJSON); overload;
    procedure Answer(const AFileName: StringRAL); overload;
    procedure Answer(AStatusCode: IntegerRAL; AFile: TStream; const AFileName: StringRAL); overload;

    function AddHeader(const AName: StringRAL; const AValue : StringRAL) : TRALResponse; reintroduce;
    function AddField(const AName: StringRAL; const AValue : StringRAL) : TRALResponse; reintroduce;
    function AddCookie(const AName: StringRAL; const AValue : StringRAL) : TRALResponse; reintroduce;
    function AddFile(const AFileName : StringRAL) : TRALResponse; reintroduce; overload;
    function AddFile(AStream : TStream; const AFileName : StringRAL = '') : TRALResponse; reintroduce; overload;
    function AddBody(const AText: StringRAL; const AContextType : StringRAL = rctTEXTPLAIN) : TRALResponse; reintroduce;
  published
    property ResponseText: StringRAL read GetResponseText write SetResponseText;
    property ResponseStream: TStream read GetResponseStream write SetResponseStream;
    property ContentType: StringRAL read FContentType write SetContentType;
    property StatusCode: IntegerRAL read FStatusCode write FStatusCode;
    property CriptoKey: StringRAL read FCriptoKey write FCriptoKey;
    property FreeContent: boolean read FFreeContent;
  end;

implementation

{ TRALResponse }

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; const AMessage: StringRAL;
  const AContentType: StringRAL);
begin
  StatusCode := AStatusCode;
  ContentType := AContentType;
  ResponseText := AMessage;
end;

procedure TRALResponse.Answer(const AFileName: StringRAL);
begin
  AddFile(AFileName);
end;

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; AFile: TStream;
  const AFileName: StringRAL);
begin
  StatusCode := AStatusCode;
  AddFile(AFile, AFileName);
end;

function TRALResponse.AddHeader(const AName, AValue : StringRAL) : TRALResponse;
begin
  inherited AddHeader(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddField(const AName, AValue : StringRAL) : TRALResponse;
begin
  inherited AddField(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddBody(const AText, AContextType: StringRAL): TRALResponse;
begin
  inherited AddBody(AText, AContextType);
  Result := Self;
end;

function TRALResponse.AddCookie(const AName, AValue : StringRAL) : TRALResponse;
begin
  inherited AddCookie(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddFile(const AFileName : StringRAL) : TRALResponse;
begin
  inherited AddFile(AFileName);
  Result := Self;
end;

function TRALResponse.AddFile(AStream : TStream; const AFileName : StringRAL) : TRALResponse;
begin
  inherited AddFile(AStream, AFileName);
  Result := Self;
end;

constructor TRALResponse.Create;
begin
  inherited;
  ContentType := rctAPPLICATIONJSON;
  FFreeContent := False;
end;

destructor TRALResponse.Destroy;
begin
  inherited;
end;

function TRALResponse.GetResponseStream: TStream;
begin
  Params.CriptoOptions.CriptType := ContentCripto;
  Params.CriptoOptions.Key := CriptoKey;
  Params.CompressType := ContentCompress;
  Result := Params.EncodeBody(FContentType, FFreeContent);
end;

function TRALResponse.GetResponseText: StringRAL;
var
  vStream: TStream;
begin
  vStream := GetResponseStream;
  try
    Result := StreamToString(vStream);
  finally
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
  Params.ClearParams(rpkBODY);
  if AValue.Size > 0 then begin
    vParam := Params.AddValue(AValue, rpkBODY);
    vParam.ContentType := ContentType;
  end;
end;

procedure TRALResponse.SetResponseText(const AValue: StringRAL);
var
  vParam: TRALParam;
begin
  Params.ClearParams(rpkBODY);
  if AValue <> '' then begin
    vParam := Params.AddValue(AValue);
    vParam.ContentType := ContentType;
    vParam.Kind := rpkBODY;
  end;
end;

end.

