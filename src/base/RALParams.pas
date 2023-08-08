unit RALParams;

interface

uses
  Classes, SysUtils,
  RALTypes, RALMIMETypes, RALMultipartCoder;

type
  TRALParamKind = (rpkNONE, rpkBODY, rpkFIELD, rpkHEADER, rpkQUERY);
  TRALParamKinds = set of TRALParamKind;

  { TRALParam }

  TRALParam = class
  private
    FParamName: StringRAL;
    FContentType: StringRAL;
    FContent: TStream;
    FFileName: StringRAL;
    FKind: TRALParamKind;
  protected
    function GetAsString: StringRAL;
    procedure SetAsString(const AValue : StringRAL);

    function GetAsStream : TStream;
    procedure SetAsStream(const AValue: TStream);

    function GetContentSize: Int64RAL;
  public
    constructor Create;
    destructor Destroy; override;

    procedure OpenFile(AFileName : StringRAL);

    property AsStream: TStream read GetAsStream write SetAsStream;
    property AsString: StringRAL read GetAsString write SetAsString;
  public
    property ParamName: StringRAL read FParamName write FParamName;
    property ContentType: StringRAL read FContentType write FContentType;
    property ContentSize: Int64RAL read GetContentSize;
    property FileName: StringRAL read FFileName write FFileName;
    property Kind : TRALParamKind read FKind write FKind;
  end;

  { TRALParams }

  TRALParams = class
  private
    FNextParam: IntegerRAL;
    FParams: TList;
  protected
    function GetParam(idx: IntegerRAL): TRALParam;
    function GetParamName(name: StringRAL): TRALParam;
    function GetParamNameKind(name: StringRAL; kind: TRALParamKind): TRALParam;
    function NextParamInt: IntegerRAL;
    function NextParamStr: StringRAL;

    procedure OnFormBodyData(Sender: TObject; AFormData: TRALMultipartFormData; var AFreeData : boolean);
  public
    constructor Create;
    destructor Destroy; override;

    function AddParam(AName, AContent: StringRAL; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    function AddParam(AName: StringRAL; AContent: TStream; AKind: TRALParamKind = rpkNONE) : TRALParam; overload;
    function AddFile(AParamName, AFileName: StringRAL): TRALParam;
    function AddValue(AContent: StringRAL; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    function AddValue(AContent: TStream; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    procedure AppendParams(ASource: TStringList; AKind: TRALParamKind); overload;
    procedure AppendParams(ASource: TStrings;  AKind: TRALParamKind); overload;

    procedure AssignParams(ADest: TStringList; AKind: TRALParamKind;
                           ASeparator: StringRAL = '=');
    function AsString: StringRAL;
    procedure ClearParams; overload;
    procedure ClearParams(AKind: TRALParamKind); overload;
    function Count: IntegerRAL; overload;
    function Count(AKind: TRALParamKind): IntegerRAL; overload;
    function Count(AKinds: TRALParamKinds): IntegerRAL; overload;
    procedure DecodeBody(ASource: TStream; AContentType: StringRAL); overload;
    procedure DecodeBody(ASource: StringRAL; AContentType: StringRAL); overload;
    procedure DecodeFields(ASource: StringRAL);
    procedure DecodeQuery(ASource: StringRAL);
    function EncodeBody(var AContentType: StringRAL; var AFreeContent: Boolean): TStream;
    function NewParam: TRALParam;
    property Param[idx: IntegerRAL]: TRALParam read GetParam;
    property ParamName[name: StringRAL]: TRALParam read GetParamName;
    property ParamNameKind[name: StringRAL; kind: TRALParamKind]: TRALParam read GetParamNameKind;
    function URLEncodedToList(ASource: StringRAL): TStringList;
  end;

implementation

{ TRALParam }

constructor TRALParam.Create;
begin
  inherited;
  FContent := nil;
  FContentType := rctTEXTPLAIN;
  FKind := rpkNONE;
end;

destructor TRALParam.Destroy;
begin
  FreeAndNil(FContent);
  inherited;
end;

procedure TRALParam.OpenFile(AFileName : StringRAL);
begin
  if FContent <> nil then
    FreeAndNil(FContent);

  if FileExists(AFileName) then
    FContent := TFileStream.Create(AFileName,fmOpenRead)
  else
    FContent := TMemoryStream.Create;
  FContent.Position := 0;
end;

function TRALParam.GetAsStream: TStream;
begin
  Result := FContent;
  if Result <> nil then
    Result.Position := 0;
end;

function TRALParam.GetAsString: StringRAL;
begin
  Result := '';
  if (FContent <> nil) and (FContent.Size > 0) then
  begin
    if FContent.InheritsFrom(TStringStream) then
    begin
      Result := TStringStream(FContent).DataString;
    end
    else begin
      SetLength(Result, FContent.Size);
      FContent.Read(Result[PosIniStr], FContent.Size);
    end;
    FContent.Position := 0;
  end;
end;

function TRALParam.GetContentSize: Int64RAL;
begin
  Result := FContent.Size;
end;

procedure TRALParam.SetAsStream(const AValue: TStream);
begin
  if FContent <> nil then
    FreeAndNil(FContent);

  FContent := TStringStream.Create;
  FContent.CopyFrom(AValue, AValue.Size);
  FContent.Position := 0;
end;

procedure TRALParam.SetAsString(const AValue: StringRAL);
begin
  if FContent <> nil then
    FreeAndNil(FContent);

  {$IFNDEF FPC}
    FContent := TStringStream.Create(AValue,TEncoding.UTF8);
  {$ELSE}
    FContent := TStringStream.Create(AValue);
  {$ENDIF}
  FContent.Position := 0;
end;

{ TRALParams }

function TRALParams.AddParam(AName, AContent: StringRAL; AKind: TRALParamKind): TRALParam;
begin
  Result := ParamNameKind[AName,AKind];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AName;
  Result.AsString := AContent;
  Result.ContentType := rctTEXTPLAIN;
  Result.Kind := AKind;
end;

function TRALParams.AddParam(AName: StringRAL; AContent: TStream; AKind: TRALParamKind) : TRALParam;
begin
  Result := ParamNameKind[AName,AKind];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AName;
  Result.AsStream := AContent;
  Result.ContentType := rctAPPLICATIONOCTETSTREAM;
  Result.Kind := AKind;
end;

function TRALParams.AddFile(AParamName, AFileName: StringRAL): TRALParam;
var
  vMime: TRALMIMEType;
begin
  Result := ParamNameKind[AParamName, rpkBODY];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AParamName;
  Result.FileName := ExtractFileName(AFileName);
  Result.OpenFile(AFileName);
  Result.Kind := rpkBODY;

  vMime := TRALMIMEType.Create;
  try
    Result.ContentType := vMime.GetMIMEType(AFileName);
    if Result.ContentType = '' then
      Result.ContentType := rctAPPLICATIONOCTETSTREAM;
  finally
    FreeAndNil(vMime);
  end;
end;

function TRALParams.AddValue(AContent: StringRAL; AKind: TRALParamKind = rpkNONE): TRALParam;
begin
  Result := NewParam;
  Result.ParamName := NextParamStr;
  Result.AsString := AContent;
  Result.ContentType := rctTEXTPLAIN;
  Result.Kind := AKind;
end;

function TRALParams.AddValue(AContent: TStream; AKind: TRALParamKind = rpkNONE): TRALParam;
begin
  Result := NewParam;
  Result.ParamName := NextParamStr;
  Result.AsStream := AContent;
  Result.ContentType := rctAPPLICATIONOCTETSTREAM;
  Result.Kind := AKind;
end;

procedure TRALParams.ClearParams;
begin
  while FParams.Count > 0 do
  begin
    TObject(FParams.Items[FParams.Count - 1]).Free;
    FParams.Delete(FParams.Count - 1);
  end;
end;

procedure TRALParams.ClearParams(AKind: TRALParamKind);
var
  vInt: IntegerRAL;
  vParam: TRALParam;
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

procedure TRALParams.AppendParams(ASource: TStringList; AKind: TRALParamKind);
var
  vInt: integer;
  vParam: TRALParam;
  vName: StringRAL;
begin
  for vInt := 0 to ASource.Count - 1 do
  begin
    vName := ASource.Names[vInt];
    vParam := ParamNameKind[vName, AKind];
    if vParam = nil then
      vParam := NewParam;
    vParam.ParamName := vName;
    vParam.AsString := ASource.Values[ASource.Names[vInt]];
    vParam.ContentType := rctTEXTPLAIN;
    vParam.Kind := AKind;
  end;
end;

procedure TRALParams.AppendParams(ASource: TStrings; AKind: TRALParamKind);
begin
  AppendParams(TStringList(ASource), AKind);
end;

procedure TRALParams.AssignParams(ADest: TStringList; AKind: TRALParamKind; ASeparator: StringRAL);
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  for vInt := 0 to FParams.Count - 1 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind = AKind then
      ADest.Add(vParam.ParamName + ASeparator + vParam.AsString);
  end;
end;

function TRALParams.AsString: StringRAL;
var
  I: IntegerRAL;
begin
  Result := '';
  for I := 0 to pred(FParams.Count) do
  begin
    Result := Result + TRALParam(FParams.Items[I]).AsString;
    if FParams.Count > 0 then
    Result := Result + ', ';
  end;
end;

procedure TRALParams.DecodeBody(ASource: TStream; AContentType: StringRAL);
var
  vParam: TRALParam;
  vDecoder: TRALMultipartDecoder;
  vStream: TStringStream;
begin
  if ASource = nil then
    Exit;

  if Pos(rctMULTIPARTFORMDATA, LowerCase(AContentType)) > 0 then
  begin
    vDecoder := TRALMultipartDecoder.Create;
    vDecoder.ContentType := AContentType;
    vDecoder.OnFormDataComplete := {$IFDEF FPC}@{$ENDIF}OnFormBodyData;
    vDecoder.ProcessMultiPart(ASource);
    vDecoder.Free;
  end
  else if Pos(rctAPPLICATIONXWWWFORMURLENCODED, LowerCase(AContentType)) > 0 then
  begin
    vStream := TStringStream.Create;
    try
      vStream.CopyFrom(ASource, ASource.Size);
      DecodeFields(vStream.DataString);
    finally
      FreeAndNil(vStream);
    end;
  end
  else begin
    vParam := NewParam;
    vParam.ParamName := 'ral_body';
    vParam.AsStream := ASource;
    vParam.ContentType := AContentType;
    vParam.Kind := rpkBODY;
  end;
end;

procedure TRALParams.DecodeBody(ASource: StringRAL; AContentType: StringRAL);
var
  vParam: TRALParam;
  vDecoder: TRALMultipartDecoder;
begin
  if ASource = '' then
    Exit;

  if Pos(rctMULTIPARTFORMDATA, LowerCase(AContentType)) > 0 then
  begin
    vDecoder := TRALMultipartDecoder.Create;
    vDecoder.ContentType := AContentType;
    vDecoder.OnFormDataComplete := {$IFDEF FPC}@{$ENDIF}OnFormBodyData;
    vDecoder.ProcessMultiPart(ASource);
    vDecoder.Free;
  end
  else if Pos(rctAPPLICATIONXWWWFORMURLENCODED, LowerCase(AContentType)) > 0 then
  begin
    DecodeFields(ASource);
  end
  else begin
    vParam := NewParam;
    vParam.ParamName := 'ral_body';
    vParam.AsString := ASource;
    vParam.ContentType := AContentType;
    vParam.Kind := rpkBODY;
  end;
end;

function TRALParams.EncodeBody(var AContentType: StringRAL; var AFreeContent: Boolean): TStream;
var
  vMultPart: TRALMultipartEncoder;
  vInt1, vInt2: integer;
  vItem: TRALParam;
  vString, vValor: StringRAL;
begin
  AFreeContent := False;
  Result := nil;

  vInt1 := Count(rpkBODY);
  vInt2 := Count(rpkFIELD);
  if vInt1+vInt2 = 1 then
  begin
    Result := Param[0].AsStream;
    AContentType := Param[0].ContentType;
  end
  else if (vInt2 > 0) and (vInt1 = 0) then
  begin
    vString := '';
    for vInt1 := 0 to Pred(Count) do
    begin
      vItem := Param[vInt1];
      if vItem.Kind in [rpkFIELD] then
      begin
        if vString <> '' then
          vString := vString + '&';

        vValor := vItem.ParamName + '=' + vItem.AsString;
        vValor := StringReplace(vValor, '&', '%26', [rfReplaceAll]);
        vValor := StringReplace(vValor, '&amp;', '%26', [rfReplaceAll]);

        vString := vString + vValor;
      end;
    end;
    Result := TStringStream.Create(vString);
    Result.Position := 0;

    AFreeContent := True;
    AContentType := rctAPPLICATIONXWWWFORMURLENCODED;
  end
  else if vInt1+vInt2 > 1 then
  begin
    vMultPart := TRALMultipartEncoder.Create;
    try
      for vInt1 := 0 to Pred(Count) do
      begin
        vItem := Param[vInt1];
        if vItem.Kind in [rpkBODY, rpkFIELD] then
        begin
          vMultPart.AddStream(Param[vInt1].ParamName,
                              Param[vInt1].AsStream,
                              Param[vInt1].FileName,
                              Param[vInt1].ContentType);
        end;
      end;
      Result := vMultPart.AsStream;
      AContentType := vMultPart.ContentType;
      AFreeContent := True;
    finally
      FreeAndNil(vMultPart);
    end;
  end;
end;

function TRALParams.URLEncodedToList(ASource: StringRAL): TStringList;
begin
  Result := TStringList.Create;
  if Trim(ASource) = '' then
    Exit;

  ASource := StringReplace(ASource, '&amp;', '%26', [rfReplaceAll]);
  ASource := StringReplace(ASource, '&', #13#10, [rfReplaceAll]);
  Result.Text := ASource;
end;

procedure TRALParams.DecodeQuery(ASource: StringRAL);
var
  vStringList: TStringList;
begin
  vStringList := URLEncodedToList(ASource);
  try
    AppendParams(vStringList, rpkQUERY);
  finally
    FreeAndNil(vStringList);
  end;
end;

procedure TRALParams.DecodeFields(ASource: StringRAL);
var
  vStringList: TStringList;
begin
  vStringList := URLEncodedToList(ASource);
  try
    AppendParams(vStringList, rpkFIELD);
  finally
    FreeAndNil(vStringList);
  end;
end;

function TRALParams.Count: IntegerRAL;
begin
  Result := FParams.Count;
end;

function TRALParams.Count(AKind: TRALParamKind): IntegerRAL;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  Result := 0;
  for vInt := 0 to Pred(FParams.Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind = AKind then
      Result := Result + 1;
  end;
end;

function TRALParams.Count(AKinds: TRALParamKinds): IntegerRAL;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  Result := 0;
  for vInt := 0 to Pred(FParams.Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind in AKinds then
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

function TRALParams.GetParamNameKind(name: StringRAL; kind: TRALParamKind): TRALParam;
var
  vInt: IntegerRAL;
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
  vInt: IntegerRAL;
  vParam: TRALParam;
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

function TRALParams.NextParamStr: StringRAL;
begin
  FNextParam := FNextParam + 1;
  Result := 'ral_param' + IntToStr(FNextParam);
end;

function TRALParams.NextParamInt: IntegerRAL;
begin
  FNextParam := FNextParam + 1;
  Result := FNextParam;
end;

procedure TRALParams.OnFormBodyData(Sender: TObject; AFormData: TRALMultipartFormData; var AFreeData : boolean);
var
  vParam: TRALParam;
begin
  vParam := NewParam;
  if AFormData.Name = '' then
    vParam.ParamName := 'ral_body' + IntToStr(NextParamInt)
  else
    vParam.ParamName := AFormData.Name;

  vParam.AsStream := AFormData.AsStream;
  vParam.FileName := AFormData.Filename;

  if AFormData.ContentType = '' then
    vParam.ContentType := AFormData.ContentType
  else
    vParam.ContentType := rctTEXTPLAIN;

  if vParam.FileName <> '' then
    vParam.Kind := rpkBODY
  else
    vParam.Kind := rpkFIELD;

  AFreeData := True;
end;

end.

