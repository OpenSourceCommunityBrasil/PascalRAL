unit RALResponse;

interface

uses
  Classes, SysUtils,
  RALTypes, RALParams, RALMIMETypes, RALCustomObjects;

type

  { TRALResponse }

  TRALResponse = class(TRALHTTPHeaderInfo)
  private
    FContentType: StringRAL;
    FRespCode: IntegerRAL;
    FFreeContent: boolean;
    FCompress : boolean;
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
                     AContentType: StringRAL = rctAPPLICATIONJSON); overload;
    procedure Answer(AFileName: StringRAL); overload;
    procedure Answer(AStatusCode: IntegerRAL; AFile: TStream; AFileName: StringRAL); overload;

    function AddHeader(AName, AValue : StringRAL) : TRALResponse; reintroduce;
    function AddField(AName, AValue : StringRAL) : TRALResponse; reintroduce;
    function AddCookie(AName, AValue : StringRAL) : TRALResponse; reintroduce;
    function AddFile(AFileName : StringRAL) : TRALResponse; reintroduce; overload;
    function AddFile(AStream : TStream; AFileName : StringRAL = '') : TRALResponse; reintroduce; overload;
    function AddBody(AText: StringRAL; AContextType : StringRAL = rctTEXTPLAIN) : TRALResponse; reintroduce;
  published
    property ResponseText: StringRAL read GetResponseText write SetResponseText;
    property ResponseStream: TStream read GetResponseStream write SetResponseStream;
    property ContentType: StringRAL read FContentType write SetContentType;
    property Compress: boolean read FCompress write FCompress;
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

procedure TRALResponse.Answer(AFileName: StringRAL);
begin
  AddFile(AFileName);
end;

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; AFile: TStream;
  AFileName: StringRAL);
begin
  StatusCode := AStatusCode;
  AddFile(AFile, AFileName);
end;

function TRALResponse.AddHeader(AName, AValue : StringRAL) : TRALResponse;
begin
  inherited AddHeader(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddField(AName, AValue : StringRAL) : TRALResponse;
begin
  inherited AddField(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddBody(AText, AContextType: StringRAL): TRALResponse;
begin
  inherited AddBody(AText, AContextType);
  Result := Self;
end;

function TRALResponse.AddCookie(AName, AValue : StringRAL) : TRALResponse;
begin
  inherited AddCookie(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddFile(AFileName : StringRAL) : TRALResponse;
begin
  inherited AddFile(AFileName);
  Result := Self;
end;

function TRALResponse.AddFile(AStream : TStream; AFileName : StringRAL) : TRALResponse;
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
  Result := Params.EncodeBody(FContentType, FFreeContent, FCompress);
end;

function TRALResponse.GetResponseText: StringRAL;
var
  vStream: TStream;
begin
  Result := '';
  vStream := Params.EncodeBody(FContentType, FFreeContent, FCompress);
  if vStream <> nil then
  begin
    vStream.Position := 0;
    if vStream is TStringStream then begin
      Result := TStringStream(vStream).DataString;
    end
    else if vStream.InheritsFrom(TMemoryStream) then begin
      SetLength(Result, vStream.Size);
      Move(TMemoryStream(vStream).Memory, Result[PosIniStr], vStream.Size);
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

