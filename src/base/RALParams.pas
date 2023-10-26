unit RALParams;

interface

uses
  Classes, SysUtils,
  RALTypes, RALMIMETypes, RALMultipartCoder, RALTools, RALUrlCoder,
  RALCompressZLib, RALCripto, RALCriptoAES, RALStream;

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
    function IsNilOrEmpty: boolean;
    function Size: Int64;
    procedure OpenFile(AFileName: StringRAL);
    procedure SaveToFile(AFileName: StringRAL); overload;
    procedure SaveToFile(AFolderName,AFileName: StringRAL); overload;
    procedure SaveToFile; overload;
    function SaveToStream : TStream; overload;
    procedure SaveToStream(var AStream : TStream); overload;
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

    FCompressType: TRALCompressType;
    FCriptoOptions: TRALCriptoOptions;
  protected
    function GetBody: TList;
    function GetParam(idx: IntegerRAL): TRALParam; overload;
    function GetParam(name: StringRAL): TRALParam; overload;
    function GetParam(name: StringRAL; Kind: TRALParamKind): TRALParam; overload;
    function NextParamInt: IntegerRAL;
    function NextParamStr: StringRAL;
    function FindNameSeparator(ASource: StringRAL): StringRAL;
    procedure AppendParamLine(ALine, ANameSeparator: StringRAL; AKind: TRALParamKind);

    procedure OnFormBodyData(Sender: TObject; AFormData: TRALMultipartFormData; var AFreeData: boolean);

    function Compress(AStream : TStream) : TStream;
    function Encrypt(AStream : TStream) : TStream;

    function Decompress(AStream : TStream) : TStream; overload;
    function Decompress(ASource : StringRAL) : StringRAL; overload;

    function Decrypt(AStream : TStream) : TStream; overload;
    function Decrypt(ASource : StringRAL) : StringRAL; overload;
  public
    constructor Create;
    destructor Destroy; override;

    function AddParam(AName, AValue: StringRAL; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    function AddParam(AName: StringRAL; AContent: TStream; AKind: TRALParamKind = rpkNONE): TRALParam; overload;

    function AddValue(AContent: StringRAL; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    function AddValue(AContent: TStream; AKind: TRALParamKind = rpkNONE): TRALParam; overload;

    function AddFile(AParamName, AFileName: StringRAL): TRALParam; overload;
    function AddFile(AFileName: StringRAL): TRALParam; overload;

    procedure AppendParams(ASource: TStringList; AKind: TRALParamKind); overload;
    procedure AppendParams(ASource: TStrings; AKind: TRALParamKind); overload;
    procedure AppendParamsListText(ASource: StringRAL; AKind: TRALParamKind;
                                   ANameSeparator: StringRAL = '');
    procedure AppendParamsText(AText: StringRAL; AKind: TRALParamKind;
                               ANameSeparator: StringRAL = '='; ALineSeparator: StringRAL = '&');
    procedure AppendParamsUrl(AUrlQuery: StringRAL; AKind: TRALParamKind);
    procedure AppendParamsUri(AFullURI, APartialURI: StringRAL; AKind: TRALParamKind);

    procedure AssignParams(ADest: TStringList; AKind: TRALParamKind; ASeparator: StringRAL = '='); overload;
    procedure AssignParams(ADest: TStrings; AKind: TRALParamKind; ASeparator: StringRAL = '='); overload;
    function AssignParamsListText(AKind: TRALParamKind; ANameSeparator: StringRAL = '='): StringRAL;
    function AssignParamsText(AKind: TRALParamKind; ANameSeparator: StringRAL = '=';
                              ALineSeparator: StringRAL = '&'): StringRAL;
    function AssignParamsUrl(AKind: TRALParamKind): StringRAL;

    procedure ClearParams; overload;
    procedure ClearParams(AKind: TRALParamKind); overload;

    function Count: IntegerRAL; overload;
    function Count(AKind: TRALParamKind): IntegerRAL; overload;
    function Count(AKinds: TRALParamKinds): IntegerRAL; overload;

    procedure DecodeFields(ASource: StringRAL; AKind: TRALParamKind = rpkFIELD);

    function DecodeBody(ASource: TStream; AContentType: StringRAL): TStream; overload;
    function DecodeBody(ASource: StringRAL; AContentType: StringRAL): TStream; overload;
    function EncodeBody(var AContentType: StringRAL; var AFreeContent: boolean): TStream;

    function NewParam: TRALParam;
    function URLEncodedToList(ASource: StringRAL): TStringList;

    function AsString: StringRAL;

    property Body: TList read GetBody;
    property Param[idx: IntegerRAL]: TRALParam read GetParam;
    property Get[name: StringRAL]: TRALParam read GetParam;
    property GetKind[name: StringRAL; Kind: TRALParamKind]: TRALParam read GetParam;

    property CompressType : TRALCompressType read FCompressType write FCompressType;
    property CriptoOptions : TRALCriptoOptions read FCriptoOptions write FCriptoOptions;
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

function TRALParam.IsNilOrEmpty: boolean;
begin
  Result := (Self = nil) or ((Self <> nil) and (Self.Size = 0));
end;

function TRALParam.Size : Int64;
begin
  if FContent <> nil then
    Result := FContent.Size
  else
    Result := 0;
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
  Result := nil;

  if Self <> nil then
  begin
    Result := FContent;
    if Result <> nil then
      Result.Position := 0;
  end;
end;

function TRALParam.GetAsString: StringRAL;
begin
  Result := '';
  if (Self <> nil) and (FContent <> nil) and (FContent.Size > 0) then
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
  if FContent <> nil then
  begin
    FContent.Position := 0;
    vFile.CopyFrom(FContent, FContent.Size);
  end;
  vFile.Free;
end;

procedure TRALParam.SaveToFile;
begin
  SaveToFile('','');
end;

procedure TRALParam.SaveToStream(var AStream: TStream);
begin
  if (FContent = nil) and (FContent.Size = 0) then
    Exit;

  AStream.Size := 0;
  AStream.CopyFrom(FContent, FContent.Size);
  AStream.Position := 0;
end;

function TRALParam.SaveToStream: TStream;
begin
  Result := TMemoryStream.Create;
  SaveToStream(Result);
end;

procedure TRALParam.SaveToFile(AFolderName, AFileName: StringRAL);
var
  vMime: TRALMIMEType;
  vExt : StringRAL;
begin
  if AFolderName = '' then
    AFolderName := ExtractFileDir(ParamStr(0));

  AFolderName := IncludeTrailingPathDelimiter(AFolderName);

  if AFileName = '' then
  begin
    if FileName = '' then
    begin
      vMime := TRALMIMEType.Create;
      try
        vExt := vMime.GetMIMEContentExt(FContentType);
      finally
        FreeAndNil(vMime);
      end;

      AFileName := FParamName + vExt;
    end
    else
    begin
      AFileName := FileName;
    end;
  end;

  SaveToFile(AFolderName+AFileName);
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

function TRALParams.AddParam(AName, AValue: StringRAL; AKind: TRALParamKind): TRALParam;
begin
  Result := GetKind[AName, AKind];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AName;
  Result.AsString := AValue;
  Result.ContentType := rctTEXTPLAIN;
  Result.Kind := AKind;
end;

function TRALParams.AddParam(AName: StringRAL; AContent: TStream; AKind: TRALParamKind): TRALParam;
begin
  Result := GetKind[AName, AKind];
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
  Result := GetKind[AParamName, rpkBODY];
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

function TRALParams.AddFile(AFileName: StringRAL): TRALParam;
var
  vMime: TRALMIMEType;
begin
  Result := NewParam;
  Result.ParamName := NextParamStr;
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
begin
  AppendParams(TStrings(ASource), AKind);
end;

procedure TRALParams.AppendParams(ASource: TStrings; AKind: TRALParamKind);
var
  vInt: integer;
  vSeparator: StringRAL;
begin
  if ASource.Count > 0 then
    vSeparator := FindNameSeparator(ASource.Strings[0]);

  for vInt := 0 to Pred(ASource.Count) do
    AppendParamLine(ASource.Strings[vInt], vSeparator, AKind);
end;

procedure TRALParams.AppendParamsListText(ASource: StringRAL; AKind: TRALParamKind; ANameSeparator: StringRAL);
var
  vInt: IntegerRAL;
  vLine: StringRAL;
  vIs13: boolean;
begin
  {$IFNDEF FPC}
    ASource := UTF8ToString(ASource);
  {$ENDIF}

  if (ASource <> '') and (ANameSeparator = '') then
    ANameSeparator := FindNameSeparator(ASource);

  vLine := '';
  for vInt := RALLowStr(ASource) to RALHighStr(ASource) do
  begin
    if ASource[vInt] = #13 then
    begin
      AppendParamLine(vLine, ANameSeparator, AKind);
      vIs13 := True;
      vLine := '';
    end
    else if ASource[vInt] = #10 then
    begin
      if not vIs13 then
        AppendParamLine(vLine, ANameSeparator, AKind);
      vIs13 := False;
      vLine := '';
    end
    else
    begin
      vLine := vLine + ASource[vInt];
      vIs13 := False;
    end;
  end;

  if vLine <> '' then
    AppendParamLine(vLine, ANameSeparator, AKind);
end;

procedure TRALParams.AppendParamsText(AText: StringRAL; AKind: TRALParamKind;
            ANameSeparator: StringRAL; ALineSeparator: StringRAL);
var
  vLine: StringRAL;
  vInt: IntegerRAL;
begin
  vLine := '';
  vInt := RALLowStr(AText);
  while vInt <= RALHighStr(AText) do
  begin
    if Copy(AText, vInt, Length(ALineSeparator)) = ALineSeparator then
    begin
      AppendParamLine(vLine, ANameSeparator, AKind);
      vLine := '';
      vInt := vInt + Length(ALineSeparator) - 1;
    end
    else
    begin
      vLine := vLine + AText[vInt];
    end;
    vInt := vInt + 1;
  end;

  if vLine <> '' then
    AppendParamLine(vLine, ANameSeparator, AKind);
end;

procedure TRALParams.AppendParamsUri(AFullURI, APartialURI: StringRAL; AKind: TRALParamKind);
var
  vInt, vIdx: IntegerRAL;
begin
  if SameText(AFullURI, APartialURI) then
    Exit;

  AFullURI := FixRoute(AFullURI);
  APartialURI := FixRoute(APartialURI);

  if Pos(LowerCase(APartialURI), LowerCase(AFullURI)) > 0 then
  begin
    Delete(AFullURI, 1, Length(APartialURI)); // removendo partialuri
    vIdx := 1;
    repeat
      vInt := Pos('/', AFullURI);
      if vInt > 0 then
      begin
        AddParam('uri' + IntToStr(vIdx), Copy(AFullURI, 1, vInt - 1), AKind);
        Delete(AFullURI, 1, vInt);
        vIdx := vIdx + 1;
      end;
    until vInt = 0;
  end;
end;

procedure TRALParams.AppendParamsUrl(AUrlQuery: StringRAL; AKind: TRALParamKind);
var
  vInt: IntegerRAL;
begin
  vInt := Pos('?', AUrlQuery);
  if vInt > 0 then
    System.Delete(AUrlQuery, 1, vInt);

  AppendParamsText(AUrlQuery, AKind);
end;

procedure TRALParams.AssignParams(ADest: TStringList; AKind: TRALParamKind; ASeparator: StringRAL);
begin
  AssignParams(TStrings(ADest), AKind, ASeparator);
end;

procedure TRALParams.AssignParams(ADest: TStrings; AKind: TRALParamKind; ASeparator: StringRAL);
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  for vInt := 0 to Pred(FParams.Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind = AKind then
      ADest.Add(vParam.ParamName + ASeparator + vParam.AsString);
  end;
end;

function TRALParams.AssignParamsListText(AKind: TRALParamKind; ANameSeparator: StringRAL): StringRAL;
begin
  Result := AssignParamsText(AKind, ANameSeparator, #13#10);
end;

function TRALParams.AssignParamsText(AKind: TRALParamKind; ANameSeparator: StringRAL;
           ALineSeparator: StringRAL): StringRAL;
var
  vInt: integer;
  vParam: TRALParam;
begin
  Result := '';
  for vInt := 0 to Pred(Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind = AKind then
    begin
      if Result <> '' then
        Result := Result + ALineSeparator;
      Result := Result + vParam.ParamName + ANameSeparator + vParam.AsString;
    end;
  end;

  Result := TrimRight(Result);
end;

function TRALParams.AssignParamsUrl(AKind: TRALParamKind): StringRAL;
begin
  Result := AssignParamsText(AKind);
end;

function TRALParams.AsString: StringRAL;
var
  I: IntegerRAL;
begin
  Result := '';
  for I := 0 to Pred(FParams.Count) do
  begin
    Result := Result + TRALParam(FParams.Items[I]).AsString;
    if FParams.Count > 0 then
      Result := Result + ', ';
  end;
end;

function TRALParams.DecodeBody(ASource : TStream; AContentType : StringRAL) : TStream;
var
  vParam: TRALParam;
  vDecoder: TRALMultipartDecoder;
  vTemp : TStream;
begin
  Result := nil;
  if ASource = nil then
    Exit;

  ASource.Position := 0;

  Result := TMemoryStream.Create;
  Result.CopyFrom(ASource, ASource.Size);

  if (FCriptoOptions.CriptType <> crNone) and (FCriptoOptions.Key <> '') then
  begin
    vTemp := Decrypt(Result);
    FreeAndNil(Result);
    Result := vTemp;
  end;

  if FCompressType <> ctNone then
  begin
    vTemp := Decompress(Result);
    FreeAndNil(Result);
    Result := vTemp;
  end;

  if Pos(rctMULTIPARTFORMDATA, LowerCase(AContentType)) > 0 then
  begin
    vDecoder := TRALMultipartDecoder.Create;
    vDecoder.ContentType := AContentType;
    vDecoder.OnFormDataComplete := {$IFDEF FPC}@{$ENDIF}OnFormBodyData;
    vDecoder.ProcessMultiPart(Result);
    vDecoder.Free;
  end
  else if Pos(rctAPPLICATIONXWWWFORMURLENCODED, LowerCase(AContentType)) > 0 then
  begin
    DecodeFields(StreamToString(Result));
  end
  else
  begin
    vParam := NewParam;
    vParam.ParamName := 'ral_body';
    vParam.AsStream := Result;
    vParam.ContentType := AContentType;
    vParam.Kind := rpkBODY;
  end;
end;

function TRALParams.DecodeBody(ASource : StringRAL; AContentType : StringRAL) : TStream;
var
  vStream: TStream;
begin
  Result := nil;
  if ASource = '' then
    Exit;

  vStream := StringToStream(ASource);
  try
    Result := DecodeBody(vStream, AContentType);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALParams.EncodeBody(var AContentType: StringRAL; var AFreeContent: boolean): TStream;
var
  vMultPart: TRALMultipartEncoder;
  vInt1, vInt2: integer;
  vItem: TRALParam;
  vString, vValor: StringRAL;
  vResult, vTemp : TStream;
begin
  AFreeContent := False;
  Result := nil;
  vResult := nil;

  vInt1 := Count(rpkBODY);
  vInt2 := Count(rpkFIELD);
  if vInt1 + vInt2 = 1 then
  begin
    vResult := Param[0].SaveToStream;
    AContentType := Param[0].ContentType;
    AFreeContent := True;
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
    vResult := TStringStream.Create(vString);
    vResult.Position := 0;

    AFreeContent := True;
    AContentType := rctAPPLICATIONXWWWFORMURLENCODED;
  end
  else if vInt1 + vInt2 > 1 then
  begin
    vMultPart := TRALMultipartEncoder.Create;
    try
      for vInt1 := 0 to Pred(Count) do
      begin
        vItem := Param[vInt1];
        if vItem.Kind in [rpkBODY, rpkFIELD] then
        begin
          vMultPart.AddStream(Param[vInt1].ParamName, Param[vInt1].AsStream,
                              Param[vInt1].FileName, Param[vInt1].ContentType);
        end;
      end;
      vResult := vMultPart.AsStream;
      AContentType := vMultPart.ContentType;
      AFreeContent := True;
    finally
      FreeAndNil(vMultPart);
    end;
  end;

  if (FCompressType <> ctNone) and (vResult <> nil) then
  begin
    vTemp := Compress(vResult);
    if AFreeContent then
      FreeAndNil(vResult);
    AFreeContent := True;
    vResult := vTemp;
  end;

  if (FCriptoOptions.CriptType <> crNone) and (Trim(FCriptoOptions.Key) <> '') and
     (vResult <> nil) then
  begin
    vTemp := Encrypt(vResult);
    if AFreeContent then
      FreeAndNil(vResult);
    AFreeContent := True;
    vResult := vTemp;
  end;

  Result := vResult;
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

procedure TRALParams.DecodeFields(ASource: StringRAL; AKind: TRALParamKind = rpkFIELD);
var
  vStringList: TStringList;
begin
  vStringList := URLEncodedToList(ASource);
  try
    AppendParams(vStringList, AKind);
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
  FCriptoOptions := TRALCriptoOptions.Create;

  FCompressType := ctGZip;
  FNextParam := 0;
end;

destructor TRALParams.Destroy;
begin
  ClearParams;
  FreeAndNil(FParams);
  FreeAndNil(FCriptoOptions);
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

function TRALParams.FindNameSeparator(ASource: StringRAL): StringRAL;
var
  vPos, vMin: IntegerRAL;
begin
  vMin := Length(ASource);
  vPos := Pos('=', ASource);
  if (vPos > 0) and (vPos < vMin) then
    Result := '=';

  vPos := Pos(': ', ASource);
  if (vPos > 0) and (vPos < vMin) then
    Result := ': ';
end;

procedure TRALParams.AppendParamLine(ALine, ANameSeparator: StringRAL; AKind: TRALParamKind);
var
  vPos: IntegerRAL;
  vName, vValue: StringRAL;
  vParam: TRALParam;
begin
  if ALine = '' then
    Exit;

  vPos := Pos(ANameSeparator, ALine);
  if vPos > 0 then
  begin
    vName := Copy(ALine, RALLowStr(ALine), vPos - 1);
    vName := TRALHTTPCoder.DecodeURL(vName);

    vValue := Copy(ALine, vPos + Length(ANameSeparator), Length(ALine));
    vValue := TRALHTTPCoder.DecodeURL(vValue);

    vParam := GetKind[vName, AKind];
    if vParam = nil then
      vParam := NewParam;
    vParam.ParamName := vName;
    vParam.AsString := vValue;
    vParam.ContentType := rctTEXTPLAIN;
    vParam.Kind := AKind;
  end;
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

  vParam.Kind := rpkBODY;

  AFreeData := True;
end;

function TRALParams.Compress(AStream : TStream) : TStream;
begin
  Result := nil;
  case FCompressType of
    ctDeflate,
    ctGZip,
    ctZLib    : Result := TRALCompressZLib.Compress(AStream, FCompressType);
  end;
end;

function TRALParams.Encrypt(AStream : TStream) : TStream;
var
  vCript : TRALCripto;
begin
  Result := nil;
  case FCriptoOptions.CriptType of
    crAES128 : begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES128;
    end;
    crAES192 : begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES192;
    end;
    crAES256 : begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES256;
    end;
  end;

  try
    vCript.Key := FCriptoOptions.Key;
    Result := vCript.EncodeAsStream(AStream);
  finally
    FreeAndNil(vCript);
  end;
end;

function TRALParams.Decompress(AStream : TStream) : TStream;
begin
  Result := nil;
  case FCompressType of
    ctDeflate,
    ctGZip,
    ctZLib    : Result := TRALCompressZLib.Decompress(AStream, FCompressType);
  end;
end;

function TRALParams.Decompress(ASource : StringRAL) : StringRAL;
begin
  Result := '';
  case FCompressType of
    ctDeflate,
    ctGZip,
    ctZLib    : Result := TRALCompressZLib.Decompress(ASource, FCompressType);
  end;
end;

function TRALParams.Decrypt(AStream : TStream) : TStream;
var
  vCript : TRALCripto;
begin
  Result := nil;
  case FCriptoOptions.CriptType of
    crAES128 : begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES128;
    end;
    crAES192 : begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES192;
    end;
    crAES256 : begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES256;
    end;
  end;

  try
    vCript.Key := FCriptoOptions.Key;
    Result := vCript.DecodeAsStream(AStream);
  finally
    FreeAndNil(vCript);
  end;
end;

function TRALParams.Decrypt(ASource : StringRAL) : StringRAL;
var
  vCript : TRALCripto;
begin
  Result := '';
  case FCriptoOptions.CriptType of
    crAES128 : begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES128;
    end;
    crAES192 : begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES192;
    end;
    crAES256 : begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES256;
    end;
  end;

  try
    vCript.Key := FCriptoOptions.Key;
    Result := vCript.Decode(ASource);
  finally
    FreeAndNil(vCript);
  end;
end;

end.
