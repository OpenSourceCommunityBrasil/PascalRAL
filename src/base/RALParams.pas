unit RALParams;

interface

uses
  Classes, SysUtils,
  RALTypes, RALMIMETypes, RALMultipartCoder;

type

  { TRALParam }

  TRALParam = class
  private
    FParamName: StringRAL;
    FContentType: StringRAL;
    FContent: TStream;
    FFileName: StringRAL;
    FKind: TRALParamKind;
  protected
    function GetAsFile: TFileStream;
    function GetAsStream: TStream;
    function GetAsString: StringRAL;
    procedure SetAsString(const AValue: StringRAL);
    procedure SetAsStream(const AValue: TStream);
    function GetContentSize: Int64RAL;
  public
    constructor Create;
    destructor Destroy; override;

    property AsStream: TStream read GetAsStream write SetAsStream;
    property AsString: StringRAL read GetAsString write SetAsString;
    property AsFile: TFileStream read GetAsFile;
    procedure OpenFile(AFileName: StringRAL);
    procedure SaveToFile(AFileName: StringRAL); overload;
    procedure SaveToFile; overload;
  public
    property ContentType: StringRAL read FContentType write FContentType;
    property ContentSize: Int64RAL read GetContentSize;
    property FileName: StringRAL read FFileName write FFileName;
    property Kind: TRALParamKind read FKind write FKind;
    property ParamName: StringRAL read FParamName write FParamName;
  end;

  { TRALParams }

  TRALParams = class
  private
    FNextParam: IntegerRAL;
    FParams: TList;
  protected
    function GetBody: TList;
    function GetParam(idx: IntegerRAL): TRALParam; overload;
    function GetParam(name: StringRAL): TRALParam; overload;
    function GetParam(name: StringRAL; Kind: TRALParamKind): TRALParam; overload;
    function NextParamInt: IntegerRAL;
    function NextParamStr: StringRAL;

    procedure OnFormBodyData(Sender: TObject; AFormData: TRALMultipartFormData;
      var AFreeData: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    function AddParam(AName, AContent: StringRAL; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    function AddParam(AName: StringRAL; AContent: TStream; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    function AddFile(AParamName, AFileName: StringRAL): TRALParam;
    function AddValue(AContent: StringRAL; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    function AddValue(AContent: TStream; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    procedure AppendParams(ASource: TStringList; AKind: TRALParamKind); overload;
    procedure AppendParams(ASource: TStrings; AKind: TRALParamKind); overload;
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
    function EncodeBody(var AContentType: StringRAL; var AFreeContent: boolean): TStream;
    function NewParam: TRALParam;
    function URLEncodedToList(ASource: StringRAL): TStringList;

    property Body: TList read GetBody;
    property Param[idx: IntegerRAL]: TRALParam read GetParam;
    property ParamByName[name: StringRAL]: TRALParam read GetParam;
    property ParamByNameAndKind[name: StringRAL; Kind: TRALParamKind]: TRALParam
      read GetParam;
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

procedure TRALParam.OpenFile(AFileName: StringRAL);
begin
  if FContent <> nil then
    FreeAndNil(FContent);

  if FileExists(AFileName) then
    FContent := TFileStream.Create(AFileName, fmOpenRead)
  else
    FContent := TMemoryStream.Create;
  FContent.Position := 0;
end;

function TRALParam.GetAsFile: TFileStream;
begin
  if FKind = rpkBODY then
  try
    Result := TFileStream(FContent);
  except
    Result := nil;
  end;
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
    else
    begin
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

procedure TRALParam.SaveToFile(AFileName: StringRAL);
var
  vFile: TFileStream;
begin
  vFile := TFileStream.Create(AFileName, fmCreate);
  if FContent <> nil then begin
    FContent.Position := 0;
    vFile.CopyFrom(FContent, FContent.Size);
  end;
  vFile.Free;
end;

procedure TRALParam.SaveToFile;
var
  vFile, vExt: StringRAL;
  vMime: TRALMIMEType;
begin
  vMime := TRALMIMEType.Create;
  try
    vExt := vMime.GetMIMEContentExt(FContentType);
  finally
    FreeAndNil(vMime);
  end;

  vFile := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
  vFile := vFile + FParamName + vExt;
  SaveToFile(vFile);
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

  FContent := TStringStream.Create(AValue);
  FContent.Position := 0;
end;

{ TRALParams }

function TRALParams.AddParam(AName, AContent: StringRAL; AKind: TRALParamKind): TRALParam;
begin
  Result := ParamByNameAndKind[AName, AKind];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AName;
  Result.AsString := AContent;
  Result.ContentType := rctTEXTPLAIN;
  Result.Kind := AKind;
end;

function TRALParams.AddParam(AName: StringRAL; AContent: TStream; AKind: TRALParamKind): TRALParam;
begin
  Result := ParamByNameAndKind[AName, AKind];
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
  Result := ParamByNameAndKind[AParamName, rpkBODY];
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
    vParam := ParamByNameAndKind[vName, AKind];
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
  else
  begin
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
  else
  begin
    vParam := NewParam;
    vParam.ParamName := 'ral_body';
    vParam.AsString := ASource;
    vParam.ContentType := AContentType;
    vParam.Kind := rpkBODY;
  end;
end;

function TRALParams.EncodeBody(var AContentType: StringRAL; var AFreeContent: boolean): TStream;
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
  if vInt1 + vInt2 = 1 then
  begin
    Result := Param[0].AsStream;
    AContentType := Param[0].ContentType;
  end
  else if (vInt2 > 0) and (vInt1 = 0) then
  begin
    vString := '';
    for vInt1 := 0 to pred(Count) do
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
  else if vInt1 + vInt2 > 1 then
  begin
    vMultPart := TRALMultipartEncoder.Create;
    try
      for vInt1 := 0 to pred(Count) do
      begin
        vItem := Param[vInt1];
        if vItem.Kind in [rpkBODY, rpkFIELD] then
        begin
          vMultPart.AddStream(Param[vInt1].ParamName, Param[vInt1].AsStream,
                              Param[vInt1].FileName, Param[vInt1].ContentType);
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
  for vInt := 0 to pred(FParams.Count) do
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
  for vInt := 0 to pred(FParams.Count) do
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

function TRALParams.GetParam(name: StringRAL; Kind: TRALParamKind): TRALParam;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  Result := nil;

  for vInt := 0 to FParams.Count - 1 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if (SameText(vParam.ParamName, name)) and (vParam.Kind = Kind) then
    begin
      Result := vParam;
      Break;
    end;
  end;
end;

function TRALParams.GetBody: TList;
var
  I: IntegerRAL;
begin
  Result := TList.Create;
  for I := 0 to Pred(FParams.Count) do
    if TRALParam(FParams.Items[I]).Kind = rpkBODY then
      Result.Add(TRALParam(FParams.Items[I]));
end;

function TRALParams.GetParam(idx: IntegerRAL): TRALParam;
begin
  Result := nil;
  if (idx >= 0) and (idx < FParams.Count) then
    Result := TRALParam(FParams.Items[idx]);
end;

function TRALParams.GetParam(name: StringRAL): TRALParam;
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

procedure TRALParams.OnFormBodyData(Sender: TObject; AFormData: TRALMultipartFormData; var AFreeData: boolean);
var
  vParam: TRALParam;
begin
  vParam := NewParam;
  if AFormData.name = '' then
    vParam.ParamName := 'ral_body' + IntToStr(NextParamInt)
  else
    vParam.ParamName := AFormData.name;

  vParam.AsStream := AFormData.AsStream;
  vParam.FileName := AFormData.FileName;

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
