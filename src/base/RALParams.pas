unit RALParams;

interface

uses
  Classes, SysUtils,
  RALTypes, RALMIMETypes, RALMimeCoder;

type
  TRALParamKind = (rpkNONE, rpkBODY, rpkFIELD, rpkHEADER, rpkQUERY);

  { TRALParam }

  TRALParam = class
  private
    FParamName: StringRAL;
    FContentType: StringRAL;
    FContent: TStringStream;
    FKind : TRALParamKind;
  protected
    function GetAsString: StringRAL;
    procedure SetAsString(const AValue : StringRAL);

    function GetAsStream : TStream;
    procedure SetAsStream(const AValue: TStream);

    function GetContentSize: Int64RAL;
  public
    constructor Create;
    destructor Destroy; override;

    property AsStream: TStream read GetAsStream write SetAsStream;
    property AsString: StringRAL read GetAsString write SetAsString;
  public
    property ParamName: StringRAL read FParamName write FParamName;
    property ContentType: StringRAL read FContentType write FContentType;
    property ContentSize: Int64RAL read GetContentSize;
    property Kind : TRALParamKind read FKind write FKind;
  end;

  { TRALParams }

  TRALParams = class
  private
    FParams: TList;
    FNextParam: IntegerRAL;
  protected
    function GetParam(idx: IntegerRAL): TRALParam;
    function GetParamNameKind(name : StringRAL; kind : TRALParamKind) : TRALParam;
    function GetParamName(name: StringRAL): TRALParam;
    function NextParamStr : StringRAL;
    function NextParamInt : IntegerRAL;

    procedure OnFormBodyData(Sender : TObject; AFormData : TRALMIMEFormData; var AFreeData : boolean);
  public
    constructor Create;
    destructor Destroy; override;

    function Count: IntegerRAL; overload;
    function Count(AKind : TRALParamKind) : IntegerRAL; overload;

    function AddParam(AName, AContent: StringRAL; AKind : TRALParamKind = rpkNONE): TRALParam; overload;
    function AddParam(AName: StringRAL; AContent: TStream; AKind : TRALParamKind = rpkNONE) : TRALParam; overload;
    function AddValue(AContent: StringRAL): TRALParam; overload;
    function AddValue(AContent: TStream): TRALParam; overload;
    function NewParam: TRALParam;

    procedure ClearParams; overload;
    procedure ClearParams(AKind : TRALParamKind); overload;

    procedure AppendParams(ASource: TStringList; AKind : TRALParamKind); overload;
    procedure AppendParams(ASource: TStrings;  AKind : TRALParamKind); overload;

    procedure AcquireParams(ASource: TStringList; AKind : TRALParamKind; ASeparator : StringRAL = '=');

    procedure DecodeBody(ASource : TStream; AContentType : StringRAL); overload;
    procedure DecodeBody(ASource : StringRAL; AContentType : StringRAL); overload;

    procedure DecodeQuery(ASource : StringRAL);

    property Param[idx: IntegerRAL]: TRALParam read GetParam;
    property ParamName[name: StringRAL]: TRALParam read GetParamName;
    property ParamNameKind[name : StringRAL; kind : TRALParamKind] : TRALParam read GetParamNameKind;
  end;

implementation

{ TRALParam }

constructor TRALParam.Create;
begin
  inherited;
  FContent := TStringStream.Create;
  FContentType := TRALContentType.ctTEXTPLAIN;
  FKind := rpkNONE;
end;

destructor TRALParam.Destroy;
begin
  FreeAndNil(FContent);
  inherited;
end;

function TRALParam.GetAsStream: TStream;
begin
  Result := FContent;
  Result.Position := 0;
end;

function TRALParam.GetAsString: StringRAL;
begin
  Result := '';
  if FContent.Size > 0 then
  begin
    Result := FContent.DataString;
    FContent.Position := 0;
  end;
end;

function TRALParam.GetContentSize: Int64RAL;
begin
  Result := FContent.Size;
end;

procedure TRALParam.SetAsStream(const AValue: TStream);
begin
  FContent.Size := 0;
  FContent.CopyFrom(AValue, AValue.Size);
  FContent.Position := 0;
end;

procedure TRALParam.SetAsString(const AValue: StringRAL);
begin
  FContent.Size := 0;
  FContent.WriteString(AValue);
  FContent.Position := 0;
end;

{ TRALParams }

function TRALParams.AddParam(AName, AContent : StringRAL; AKind : TRALParamKind) : TRALParam;
begin
  Result := ParamNameKind[AName,AKind];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AName;
  Result.AsString := AContent;
  Result.ContentType := TRALContentTypeHelper.ctTEXTPLAIN;
end;

function TRALParams.AddParam(AName : StringRAL; AContent : TStream; AKind : TRALParamKind) : TRALParam;
begin
  Result := ParamNameKind[AName,AKind];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AName;
  Result.AsStream := AContent;
  Result.ContentType := TRALContentTypeHelper.ctAPPLICATIONOCTETSTREAM;
end;

function TRALParams.AddValue(AContent : StringRAL) : TRALParam;
begin
  Result := NewParam;
  Result.ParamName := NextParamStr;
  Result.AsString := AContent;
  Result.ContentType := TRALContentTypeHelper.ctTEXTPLAIN;
end;

function TRALParams.AddValue(AContent : TStream) : TRALParam;
begin
  Result := NewParam;
  Result.ParamName := NextParamStr;
  Result.AsStream := AContent;
  Result.ContentType := TRALContentTypeHelper.ctAPPLICATIONOCTETSTREAM;
end;

procedure TRALParams.ClearParams;
begin
  while FParams.Count > 0 do
  begin
    TObject(FParams.Items[FParams.Count - 1]).Free;
    FParams.Delete(FParams.Count - 1);
  end;
end;

procedure TRALParams.ClearParams(AKind : TRALParamKind);
var
  vInt : IntegerRAL;
  vParam : TRALParam;
begin
  vInt := FParams.Count - 1;
  while vInt >= 0 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind = AKind then
    begin
      vParam.Free;
      FParams.Delete(vInt);
    end;
    vInt := vInt - 1;
  end;
end;

procedure TRALParams.AppendParams(ASource : TStringList; AKind : TRALParamKind);
var
  vInt : integer;
  vParam : TRALParam;
  vName : StringRAL;
begin
  for vInt := 0 to ASource.Count - 1 do
  begin
    vName := ASource.Names[vInt];
    vParam := ParamNameKind[vName,AKind];
    if vParam = nil then
      vParam := NewParam;
    vParam.ParamName := vName;
    vParam.AsString := ASource.Values[ASource.Names[vInt]];
    vParam.ContentType := TRALContentTypeHelper.ctTEXTPLAIN;
    vParam.Kind := AKind;
  end;
end;

procedure TRALParams.AppendParams(ASource : TStrings; AKind : TRALParamKind);
begin
  AppendParams(TStringList(ASource),AKind);
end;

procedure TRALParams.AcquireParams(ASource : TStringList; AKind : TRALParamKind; ASeparator : StringRAL);
var
  vInt : IntegerRAL;
  vParam : TRALParam;
begin
  for vInt := 0 to FParams.Count - 1 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind = AKind then
      ASource.Add(vParam.ParamName+ASeparator+vParam.AsString);
  end;
end;

procedure TRALParams.DecodeBody(ASource : TStream; AContentType : StringRAL);
var
  vParam : TRALParam;
  vDecoder : TRALMIMEDecoder;
begin
  if Pos(TRALContentType.ctMULTIPARTFORMDATA, LowerCase(AContentType)) > 0 then
  begin
    vDecoder := TRALMIMEDecoder.Create;
    vDecoder.ContentType := AContentType;
    vDecoder.OnFormDataComplete := {$IFDEF FPC}@{$ENDIF}OnFormBodyData;
    vDecoder.ProcessMultiPart(ASource);
    vDecoder.Free;
  end
  else begin
    vParam := NewParam;
    vParam.ParamName := 'ral_body';
    vParam.AsStream := ASource;
    vParam.ContentType := AContentType;
    vParam.Kind := rpkBODY;
  end;
end;

procedure TRALParams.DecodeBody(ASource : StringRAL; AContentType : StringRAL);
var
  vParam : TRALParam;
  vDecoder : TRALMIMEDecoder;
begin
  if Pos(TRALContentType.ctMULTIPARTFORMDATA, LowerCase(AContentType)) > 0 then
  begin
    vDecoder := TRALMIMEDecoder.Create;
    vDecoder.ContentType := AContentType;
    vDecoder.OnFormDataComplete := {$IFDEF FPC}@{$ENDIF}OnFormBodyData;
    vDecoder.ProcessMultiPart(ASource);
    vDecoder.Free;
  end
  else begin
    vParam := NewParam;
    vParam.ParamName := 'ral_body';
    vParam.AsString := ASource;
    vParam.ContentType := AContentType;
    vParam.Kind := rpkBODY;
  end;
end;

procedure TRALParams.DecodeQuery(ASource : StringRAL);
var
  vStringList : TStringList;
begin
  if Trim(ASource) = '' then
    Exit;

  ASource := StringReplace(ASource,'&amp;',#38,[rfReplaceAll]);
  ASource := StringReplace(ASource,#13#10,'|',[rfReplaceAll]);
  ASource := StringReplace(ASource,'&',#13#10,[rfReplaceAll]);

  vStringList := TStringList.Create;
  try
    vStringList.Text := ASource;
    AppendParams(vStringList,rpkQUERY);
  finally
    FreeAndNil(vStringList);
  end;
end;

function TRALParams.Count: IntegerRAL;
begin
  Result := FParams.Count;
end;

function TRALParams.Count(AKind : TRALParamKind) : IntegerRAL;
var
  vInt : IntegerRAL;
  vParam : TRALParam;
begin
  Result := 0;
  for vInt := 0 to FParams.Count - 1 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind = AKind then
      Result := Result + 1;
  end;
end;

constructor TRALParams.Create;
begin
  inherited;
  FParams := TList.Create;
  FNextParam := 0;
end;

destructor TRALParams.Destroy;
begin
  ClearParams;
  FreeAndNil(FParams);
  inherited;
end;

function TRALParams.GetParamNameKind(name : StringRAL; kind : TRALParamKind) : TRALParam;
var
  vInt : IntegerRAL;
  vParam: TRALParam;
begin
  Result := nil;

  for vInt := 0 to FParams.Count - 1 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if (SameText(vParam.ParamName, name)) and (vParam.Kind = kind) then
    begin
      Result := vParam;
      Break;
    end;
  end;
end;

function TRALParams.GetParam(idx: IntegerRAL): TRALParam;
begin
  Result := nil;
  if (idx >= 0) and (idx < FParams.Count) then
    Result := TRALParam(FParams.Items[idx]);
end;

function TRALParams.GetParamName(name: StringRAL): TRALParam;
var
  vInt : IntegerRAL;
  vParam : TRALParam;
begin
  Result := nil;

  for vInt := 0 to FParams.Count - 1 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if SameText(vParam.ParamName, name) then
    begin
      Result := vParam;
      Break;
    end;
  end;
end;

function TRALParams.NewParam: TRALParam;
begin
  Result := TRALParam.Create;
  Result.Kind := rpkNONE;
  FParams.Add(Result)
end;

function TRALParams.NextParamStr : StringRAL;
begin
  FNextParam := FNextParam + 1;
  Result := 'ral_param'+IntToStr(FNextParam);
end;

function TRALParams.NextParamInt : IntegerRAL;
begin
  FNextParam := FNextParam + 1;
  Result := FNextParam;
end;

procedure TRALParams.OnFormBodyData(Sender : TObject; AFormData : TRALMIMEFormData; var AFreeData : boolean);
var
  vParam : TRALParam;
begin
  vParam := NewParam;
  vParam.ParamName := AFormData.Name;
  if vParam.ParamName = '' then
    vParam.ParamName := 'ral_body'+IntToStr(NextParamInt);

  vParam.AsStream := AFormData.AsStream;

  vParam.ContentType := TRALContentType.ctTEXTPLAIN;
  if AFormData.ContentType <> '' then
    vParam.ContentType := AFormData.ContentType;

  vParam.Kind := rpkBODY;

  AFreeData := True;
end;

end.

