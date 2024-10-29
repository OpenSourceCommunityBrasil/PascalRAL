/// Unit that contains everything related to Params from either the query request
/// or response.
unit RALParams;

interface

uses
  Classes, SysUtils, TypInfo,
  RALTypes, RALMIMETypes, RALMultipartCoder, RALTools, RALUrlCoder,
  RALCripto, RALCriptoAES, RALStream, RALCompress, RALConsts;

type

  { TRALParam }

  /// This is the object of all the data that is traded between request and response.
  /// each RALParam has a name, a kind and a content that can either be a text
  /// (String) or a bytearray (Stream)
  TRALParam = class
  private
    FContent: TStream;
    FContentType: StringRAL;
    FContentDispositionInline: Boolean;
    FFileName: StringRAL;
    FKind: TRALParamKind;
    FParamName: StringRAL;
  protected
    function GetAsBoolean: Boolean;
    function GetAsDouble: DoubleRAL;
    function GetAsInteger: IntegerRAL;
    function GetAsStream: TStream;
    function GetAsString: StringRAL;
    function GetContentDisposition: StringRAL;
    function GetContentSize: Int64RAL;
    procedure SetAsBoolean(const AValue: Boolean);
    procedure SetAsDouble(const AValue: DoubleRAL);
    procedure SetAsInteger(const AValue: IntegerRAL);
    procedure SetAsString(const AValue: StringRAL);
    procedure SetAsStream(const AValue: TStream);
    procedure SetContentDisposition(AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clone(ASource: TRALParam);
    function IsNilOrEmpty: Boolean;
    /// Clears and assign a file to the FContent.
    procedure OpenFile(const AFileName: StringRAL);
    /// Saves FContent to the default executable location.
    procedure SaveToFile; overload;
    /// Save FContent with the given Filename.
    procedure SaveToFile(const AFileName: StringRAL); overload;
    /// Save FContent with the given Filename and the foldername.
    procedure SaveToFile(AFolderName, AFileName: StringRAL); overload;
    function SaveToStream: TStream; overload;
    procedure SaveToStream(AStream: TStream); overload;
    function Size: Int64;

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDouble: DoubleRAL read GetAsDouble write SetAsDouble;
    property AsInteger: IntegerRAL read GetAsInteger write SetAsInteger;
    property AsStream: TStream read GetAsStream write SetAsStream;
    property AsString: StringRAL read GetAsString write SetAsString;
    property Content: TStream read FContent;
    property ContentDisposition: StringRAL read GetContentDisposition write SetContentDisposition;
    property ContentDispositionInline: Boolean read FContentDispositionInline write FContentDispositionInline;
    property ContentSize: Int64RAL read GetContentSize;
    property ContentType: StringRAL read FContentType write FContentType;
    property FileName: StringRAL read FFileName write FFileName;
    property Kind: TRALParamKind read FKind write FKind;
    property ParamName: StringRAL read FParamName write FParamName;
  end;

  { TRALParams }

  /// Collection of TRALParam objects
  TRALParams = class
  private
    FCompressType: TRALCompressType;
    FContentDispositionInline: Boolean;
    FCriptoOptions: TRALCriptoOptions;
    FNextParam: IntegerRAL;
    FParams: TList;
  protected
    /// Decodes the ALine URL and adds it to the param list.
    procedure AppendParamLine(const ALine: StringRAL; const ANameSeparator: StringRAL;
      AKind: TRALParamKind);
    /// Compresses the input stream into a TStream.
    function Compress(AStream: TStream): TStream;
    /// Decompresses the input string into an UTF8 String.
    function Decompress(const ASource: StringRAL): StringRAL; overload;
    /// Decompresses the input stream into a TStream.
    function Decompress(AStream: TStream): TStream; overload;
    /// Decrypts the input stream into a TStream.
    function Decrypt(AStream: TStream): TStream; overload;
    /// Decrypts the input string into an UTF8 String.
    function Decrypt(const ASource: StringRAL): StringRAL; overload;
    /// Encrypts the whole class instead of each individual object.
    function Encrypt(AStream: TStream): TStream;

    /// Results either = or : if found on the input text.
    function FindNameSeparator(const ASource: StringRAL): StringRAL;
    function GetBody: TList;
    function GetParam(AIndex: IntegerRAL; AKind: TRALParamKind): TRALParam; overload;
    function GetParam(AIndex: IntegerRAL): TRALParam; overload;
    function GetParam(AName: StringRAL): TRALParam; overload;
    function GetParam(AName: StringRAL; AKind: TRALParamKind): TRALParam; overload;
    /// Moves to the next param and returns its index.
    function NextParamInt: IntegerRAL;
    /// Moves to the next param and returns its internal name.
    function NextParamStr: StringRAL;
    /// Event to be called during the processing of FormData.
    procedure OnFormBodyData(Sender: TObject; AFormData: TRALMultipartFormData;
      var AFreeData: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    /// Locate the RALParam with the given AParamName and fills it with a file from the AFileName.
    function AddFile(const AParamName: StringRAL; const AFileName: StringRAL): TRALParam; overload;
    /// Creates a new RALParam in the internal list and fills it with a file from the AFileName.
    function AddFile(const AFileName: StringRAL): TRALParam; overload;
    /// AddParam is used to include a TRALParam Object into the internal list.
    function AddParam(const AName: StringRAL; const AValue: StringRAL;
                      AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    /// AddParam is used to include a TRALParam Object into the internal list.
    function AddParam(const AName: StringRAL; AContent: TStream;
                      AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    /// AddValue creates a new RALParam in the internal list and fills it with the given parameters.
    function AddValue(const AContent: StringRAL; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    /// AddValue creates a new RALParam in the internal list and fills it with the given parameters.
    function AddValue(AContent: TStream; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    /// Used to append a list of params (ASource) to the current params list.
    procedure AppendParams(ASource: TStringList; AKind: TRALParamKind); overload;
    /// Used to append a list of params (ASource) to the current params list.
    procedure AppendParams(ASource: TStrings; AKind: TRALParamKind); overload;
    /// Used to append a list of params in a string to the current params list.
    procedure AppendParamsListText(ASource: StringRAL; AKind: TRALParamKind;
                                   ANameSeparator: StringRAL = '');
    /// Appends params based on a string 'AText'.
    procedure AppendParamsText(AText: StringRAL; AKind: TRALParamKind;
                               const ANameSeparator: StringRAL = '=';
                               const ALineSeparator: StringRAL = '&');
    /// Appends params based on the full URL given.
    procedure AppendParamsUrl(AUrlQuery: StringRAL; AKind: TRALParamKind);
    /// Appends params based on the full URL given separated by '/'.
    procedure AppendParamsUri(AFullURI, APartialURI: StringRAL; AKind: TRALParamKind);
    /// Fills the 'ADest' StringList with RALParams matching 'AKind'.
    procedure AssignParams(ADest: TStringList; AKind: TRALParamKind;
                           ASeparator: StringRAL = '='); overload;
    /// Fills the 'ADest' Strings with RALParams matching 'AKind'.
    procedure AssignParams(ADest: TStrings; AKind: TRALParamKind;
                           ASeparator: StringRAL = '='); overload;
    /// Returns an UTF8 String with RALParams matching 'AKind'.
    function AssignParamsListText(AKind: TRALParamKind;
                                  const ANameSeparator: StringRAL = '='): StringRAL;
    /// Returns an UTF8 String with RALParams matching 'AKind'. Can accept a different Line Separator than CRLF.
    function AssignParamsText(AKind: TRALParamKind; AUrlEncoded: boolean = False;
                              const ANameSeparator: StringRAL = '=';
                              const ALineSeparator: StringRAL = '&'): StringRAL;
    /// Returns an UTF8 String with RALParams matching 'AKind' using default URL separators.
    function AssignParamsUrl(AKind: TRALParamKind): StringRAL;
    /// Clears all params.
    procedure ClearParams; overload;
    /// Clears all params matching AKind.
    procedure ClearParams(AKind: TRALParamKind); overload;
    /// Returns total ammount of RALParams.
    function Count: IntegerRAL; overload;
    /// Returns total ammount of RALParams matching AKind.
    function Count(AKind: TRALParamKind): IntegerRAL; overload;
    /// Returns total ammount of RALParams matching multiple kinds.
    function Count(AKinds: TRALParamKinds): IntegerRAL; overload;
    /// Returns a TStream with the filtered Stream body contents.
    function DecodeBody(ASource: TStream; const AContentType: StringRAL;
                        const AContentDisposition: StringRAL = ''): TStream; overload;
    /// Returns a TStream with the filtered String body contents.
    function DecodeBody(const ASource, AContentType: StringRAL;
                        const AContentDisposition: StringRAL = ''): TStream; overload;
    /// Decode and append RALParams based on the ASource input.
    procedure DecodeFields(const ASource: StringRAL; AKind: TRALParamKind = rpkFIELD);
    /// Removes a RALParam matching the given AName.
    procedure DelParam(const AName: StringRAL); overload;
    /// Removes a RALParam matching the given AName and AKind.
    procedure DelParam(const AName: StringRAL; AKind: TRALParamKind); overload;
    /// Returns a TStream with all RALParams that matches 'Body' Kind.
    function EncodeBody(var AContentType, AContentDisposition: StringRAL): TStream;
    /// creates and returns an empty param for a more flexible way of coding.
    function NewParam: TRALParam;
    /// converts a HTML encoded URL into a TStringList.
    function URLEncodedToList(ASource: StringRAL): TStringList;
    /// returns all the params in a comma separated UTF8string.
    function AsString: StringRAL;

    /// Grabs only the body kind of params, excluding headers and cookies.
    property Body: TList read GetBody;
    /// Grabs a param by its index on the TRALParams list.
    property Index[AIndex: IntegerRAL]: TRALParam read GetParam;
    /// Grabs a param by its index on the TRALParams list.
    property IndexKind[AIndex: IntegerRAL; AKind: TRALParamKind]: TRALParam read GetParam;
    /// Grabs a param by its name.
    property Get[AName: StringRAL]: TRALParam read GetParam;
    /// Grabs a param by its name and kind since you can have multiple kinds with same name.
    property GetKind[AName: StringRAL; AKind: TRALParamKind]: TRALParam read GetParam;
  published
    /// Which algorithm to compress the content of params.
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    /// Configuration of the cryptography used on params for a secure P2P traffic.
    property CriptoOptions: TRALCriptoOptions read FCriptoOptions write FCriptoOptions;
    property ContentDispositionInline: Boolean read FContentDispositionInline
      write FContentDispositionInline;
  end;

implementation

{ TRALParam }

procedure TRALParam.Clone(ASource: TRALParam);
begin
  ASource.ContentDisposition := Self.ContentDisposition;
  ASource.ContentType := Self.ContentType;
  ASource.FileName := Self.FileName;
  ASource.Kind := Self.Kind;
  ASource.ParamName := Self.ParamName;
  ASource.AsStream := Self.Content;
end;

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

function TRALParam.IsNilOrEmpty: Boolean;
begin
  Result := (Self = nil) or ((Self <> nil) and (Self.Size = 0));
end;

function TRALParam.Size: Int64;
begin
  if FContent <> nil then
    Result := FContent.Size
  else
    Result := 0;
end;

procedure TRALParam.OpenFile(const AFileName: StringRAL);
begin
  if FContent <> nil then
    FreeAndNil(FContent);

  if FileExists(AFileName) then
  begin
    FContent := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    FContent.Position := 0;
  end
  else
  begin
    FContent := TMemoryStream.Create;
  end;
end;

function TRALParam.GetAsBoolean: Boolean;
var
  vStr: StringRAL;
begin
  Result := False;
  if (Self <> nil) then
  begin
    vStr := StreamToString(FContent);
    Result := (vStr = '1') or (SameText(vStr, 'true'));
  end;
end;

function TRALParam.GetAsDouble: DoubleRAL;
begin
  Result := 0;
  if (Self <> nil) then
    Result := StrToFloatDef(StreamToString(FContent), 0);
end;

function TRALParam.GetAsInteger: IntegerRAL;
begin
  Result := 0;
  if (Self <> nil) then
    Result := StrToIntDef(StreamToString(FContent), 0);
end;

function TRALParam.GetAsStream: TStream;
begin
  Result := nil;

  if Self <> nil then
    Result := SaveToStream;
end;

function TRALParam.GetAsString: StringRAL;
begin
  Result := '';
  if (Self <> nil) then
    Result := StreamToString(FContent);
end;

function TRALParam.GetContentDisposition: StringRAL;
begin
  if (FFileName <> '') and (not FContentDispositionInline) then
    Result := Format('attachment; name="%s"; filename="%s"', [FParamName, FFileName])
  else
    Result := Format('inline; name="%s"', [FParamName]);
end;

function TRALParam.GetContentSize: Int64RAL;
begin
  Result := FContent.Size;
end;

procedure TRALParam.SaveToFile(const AFileName: StringRAL);
begin
  SaveStream(FContent, AFileName);
end;

procedure TRALParam.SaveToFile;
begin
  SaveToFile('', '');
end;

procedure TRALParam.SaveToStream(AStream: TStream);
begin
  if (FContent = nil) or (FContent.Size = 0) then
    Exit;

  FContent.Position := 0;
  AStream.CopyFrom(FContent, FContent.Size);
end;

function TRALParam.SaveToStream: TStream;
begin
  Result := TRALStringStream.Create;
  SaveToStream(Result);

  Result.Position := 0;
end;

procedure TRALParam.SaveToFile(AFolderName, AFileName: StringRAL);
var
  vMime: TRALMIMEType;
  vExt: StringRAL;
begin
  if AFolderName = '' then
    AFolderName := ExtractFileDir(ParamStr(0));

  AFolderName := IncludeTrailingPathDelimiter(AFolderName);

  if AFileName = '' then
  begin
    if FFileName = '' then
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
      AFileName := FFileName;
    end;
  end;

  SaveToFile(AFolderName + AFileName);
end;

procedure TRALParam.SetAsBoolean(const AValue: Boolean);
var
  vStr: StringRAL;
begin
  vStr := IntToStr(Integer(AValue));
  SetAsString(vStr);
end;

procedure TRALParam.SetAsDouble(const AValue: DoubleRAL);
var
  vStr: StringRAL;
begin
  vStr := FloatToStr(AValue);
  SetAsString(vStr);
end;

procedure TRALParam.SetAsInteger(const AValue: IntegerRAL);
var
  vStr: StringRAL;
begin
  vStr := IntToStr(AValue);
  SetAsString(vStr);
end;

procedure TRALParam.SetAsStream(const AValue: TStream);
begin
  if FContent <> nil then
    FreeAndNil(FContent);

  AValue.Position := 0;

  FContent := TRALStringStream.Create(AValue);
  FContent.Position := 0;
end;

procedure TRALParam.SetAsString(const AValue: StringRAL);
begin
  if FContent <> nil then
    FreeAndNil(FContent);

  FContent := StringToStreamUTF8(AValue);
end;

procedure TRALParam.SetContentDisposition(AValue: StringRAL);
var
  vStr: StringRAL;

  function GetWord(var AStr: StringRAL): StringRAL;
  var
    vInt, vLen: Integer;
    vQuoted: Boolean;
    vChr: CharRAL;
  begin
    Result := '';
    vLen := Length(AStr);
    vQuoted := False;
    for vInt := 1 to vLen do
    begin
      vChr := Char(AStr[vInt]);
      if (vChr = '"') then
      begin
        vQuoted := not vQuoted;
      end
      else if not (CharInSet(vChr, [' ', '=', ';', ':'])) or vQuoted then
      begin
        Result := Result + vChr;
      end
      else if (CharInSet(vChr, [';', ':', '='])) and (not vQuoted) then
      begin
        Delete(AStr, 1, vInt);
        Exit;
      end;
    end;
    AStr := '';
  end;

  function ProcessVar(const AHeader, AValue: StringRAL): Boolean;
  begin
    Result := True;
    if SameText(AHeader, 'name') then
      FParamName := AValue
    else if SameText(AHeader, 'filename') then
      FFileName := AValue
    else
      Result := False;
  end;

begin
  AValue := Trim(AValue);
  // captura o tipo de content-disposition (inline, attachment, form-data)
  vStr := GetWord(AValue);
  // captura o primeiro param
  vStr := GetWord(AValue);
  while (vStr <> '') do
  begin
    ProcessVar(vStr, GetWord(AValue));
    vStr := GetWord(AValue);
  end;
end;

{ TRALParams }

function TRALParams.AddParam(const AName, AValue: StringRAL; AKind: TRALParamKind): TRALParam;
begin
  if (AName <> '') and (AValue <> '') then
  begin
    Result := GetKind[AName, AKind];
    if Result = nil then
      Result := NewParam;

    Result.ParamName := AName;
    Result.AsString := AValue;
    Result.ContentType := rctTEXTPLAIN;
    Result.Kind := AKind;
  end;
end;

function TRALParams.AddParam(const AName: StringRAL; AContent: TStream;
  AKind: TRALParamKind): TRALParam;
begin
  Result := GetKind[AName, AKind];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AName;
  Result.AsStream := AContent;
  Result.ContentType := rctAPPLICATIONOCTETSTREAM;
  Result.Kind := AKind;
end;

function TRALParams.AddFile(const AParamName, AFileName: StringRAL): TRALParam;
var
  vMime: TRALMIMEType;
begin
  if (AParamName <> '') and (AFileName <> '') then
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
end;

function TRALParams.AddFile(const AFileName: StringRAL): TRALParam;
var
  vMime: TRALMIMEType;
begin
  if AFileName <> '' then
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
end;

function TRALParams.AddValue(const AContent: StringRAL; AKind: TRALParamKind = rpkNONE)
  : TRALParam;
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
  vInt: Integer;
  vSeparator: StringRAL;
begin
  if ASource.Count > 0 then
    vSeparator := FindNameSeparator(ASource.Strings[0]);

  for vInt := 0 to Pred(ASource.Count) do
    AppendParamLine(ASource.Strings[vInt], vSeparator, AKind);
end;

procedure TRALParams.AppendParamsListText(ASource: StringRAL; AKind: TRALParamKind;
  ANameSeparator: StringRAL);
var
  vInt: IntegerRAL;
  vLine: StringRAL;
  vIs13: Boolean;
begin
  {$IFNDEF FPC}
  ASource := UTF8ToString(ASource);
  {$ENDIF}
  if (ASource <> '') and (ANameSeparator = '') then
    ANameSeparator := FindNameSeparator(ASource);

  vLine := '';
  for vInt := POSINISTR to RALHighStr(ASource) do
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
  const ANameSeparator: StringRAL; const ALineSeparator: StringRAL);
var
  vLine: StringRAL;
  vIndex: IntegerRAL;
begin
  repeat
    vIndex := Pos(ALineSeparator, AText);
    if vIndex > 0 then
      vLine := Copy(AText, POSINISTR, vIndex - 1)
    else
      vLine := AText;
    if vLine <> '' then
    begin
      AppendParamLine(vLine, ANameSeparator, AKind);
      Delete(AText, POSINISTR, vIndex);
    end
  until vIndex = 0;
end;

procedure TRALParams.AppendParamsUri(AFullURI, APartialURI: StringRAL; AKind: TRALParamKind);
var
  vInt, vIdx: IntegerRAL;
  vParam: TRALParam;
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
        vParam := GetKind['ral_uriparam' + IntToStr(vIdx), AKind];
        if vParam = nil then
        begin
          vParam := NewParam;
          vParam.ParamName := 'ral_uriparam' + IntToStr(vIdx);
        end;
        vParam.AsString := Copy(AFullURI, 1, vInt - 1);
        vParam.Kind := AKind;

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

procedure TRALParams.AssignParams(ADest: TStringList; AKind: TRALParamKind;
  ASeparator: StringRAL);
begin
  AssignParams(TStrings(ADest), AKind, ASeparator);
end;

procedure TRALParams.AssignParams(ADest: TStrings; AKind: TRALParamKind;
  ASeparator: StringRAL);
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

function TRALParams.AssignParamsListText(AKind: TRALParamKind;
  const ANameSeparator: StringRAL): StringRAL;
begin
  Result := AssignParamsText(AKind, False, ANameSeparator, HTTPLineBreak);
end;

function TRALParams.AssignParamsText(AKind: TRALParamKind; AUrlEncoded: boolean;
  const ANameSeparator: StringRAL; const ALineSeparator: StringRAL): StringRAL;
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
      Result := Result + vParam.ParamName + ANameSeparator;
      if AUrlEncoded then
        Result := Result + TRALHTTPCoder.EncodeURL(vParam.AsString)
      else
        Result := Result + vParam.AsString;
    end;
  end;

  Result := TrimRight(Result);
end;

function TRALParams.AssignParamsUrl(AKind: TRALParamKind): StringRAL;
begin
  Result := AssignParamsText(AKind, True);
end;

function TRALParams.AsString: StringRAL;
var
  I: IntegerRAL;
begin
  Result := '';
  if (FParams <> nil) and (FParams.Count > 0) then
    for I := 0 to Pred(FParams.Count) do
    begin
      Result := Result + TRALParam(FParams.Items[I]).AsString;
      if FParams.Count > 0 then
        Result := Result + ', ';
    end;
end;

function TRALParams.DecodeBody(ASource: TStream;
  const AContentType, AContentDisposition: StringRAL): TStream;
var
  vParam: TRALParam;
  vDecoder: TRALMultipartDecoder;
  vTemp: TStream;
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
    try
      vDecoder.ContentType := AContentType;
      vDecoder.OnFormDataComplete := {$IFDEF FPC}@{$ENDIF}OnFormBodyData;
      vDecoder.ProcessMultiPart(Result);
    finally
      FreeAndNil(vDecoder);
    end;
  end
  else if Pos(rctAPPLICATIONXWWWFORMURLENCODED, LowerCase(AContentType)) > 0 then
  begin
    DecodeFields(StreamToString(Result));
  end
  else
  begin
    vParam := NewParam;
    vParam.ParamName := 'ral_body';
    vParam.FileName := '';
    vParam.ContentDisposition := AContentDisposition;
    vParam.AsStream := Result;
    vParam.ContentType := AContentType;
    vParam.Kind := rpkBODY;
  end;
end;

function TRALParams.DecodeBody(
  const ASource, AContentType, AContentDisposition: StringRAL): TStream;
var
  vStream: TStream;
begin
  Result := nil;
  if ASource = '' then
    Exit;

  // deve manter TStringStream pois nesse ponto o ASource ainda pode estar
  // compress e criptografado
  vStream := TStringStream.Create(ASource);
  try
    Result := DecodeBody(vStream, AContentType, AContentDisposition);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALParams.EncodeBody(var AContentType, AContentDisposition: StringRAL): TStream;
var
  vMultPart: TRALMultipartEncoder;
  vInt1, vInt2: integer;
  vItem: TRALParam;
  vString, vValor: StringRAL;
  vTemp: TStream;
begin
  Result := nil;

  vInt1 := Count(rpkBODY);
  vInt2 := Count(rpkFIELD);

  AContentDisposition := '';

  if vInt1 + vInt2 = 1 then
  begin
    if vInt1 > 0 then
      vItem := IndexKind[0, rpkBODY]
    else
      vItem := IndexKind[0, rpkFIELD];

    vItem.ContentDispositionInline := FContentDispositionInline;

    if Pos(StringRAL('ral_param'), vItem.ParamName) > 0 then
      vItem.ParamName := 'ral_body';

    Result := vItem.SaveToStream;

    AContentType := vItem.ContentType;
    AContentDisposition := vItem.ContentDisposition;
  end
  else if (vInt2 > 0) and (vInt1 = 0) then
  begin
    vString := '';
    for vInt1 := 0 to Pred(Count) do
    begin
      vItem := Index[vInt1];
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

    AContentType := rctAPPLICATIONXWWWFORMURLENCODED;
  end
  else if vInt1 + vInt2 > 1 then
  begin
    vMultPart := TRALMultipartEncoder.Create;
    try
      for vInt1 := 0 to Pred(Count) do
      begin
        vItem := Index[vInt1];
        if vItem.Kind in [rpkBODY, rpkFIELD] then
        begin
          vMultPart.AddStream(Index[vInt1].ParamName, Index[vInt1].Content,
            Index[vInt1].FileName, Index[vInt1].ContentType);
        end;
      end;
      Result := vMultPart.AsStream;
      AContentType := vMultPart.ContentType;
    finally
      FreeAndNil(vMultPart);
    end;
  end;

  if (FCompressType <> ctNone) and (Result <> nil) then
  begin
    vTemp := Compress(Result);
    FreeAndNil(Result);
    Result := vTemp;
  end;

  if (FCriptoOptions.CriptType <> crNone) and (Trim(FCriptoOptions.Key) <> '') and
    (Result <> nil) then
  begin
    vTemp := Encrypt(Result);
    FreeAndNil(Result);
    Result := vTemp;
  end;
end;

function TRALParams.URLEncodedToList(ASource: StringRAL): TStringList;
begin
  Result := TStringList.Create;
  if Trim(ASource) = '' then
    Exit;

  ASource := StringReplace(ASource, '&amp;', '%26', [rfReplaceAll]);
  ASource := StringReplace(ASource, '&', HTTPLineBreak, [rfReplaceAll]);
  Result.Text := ASource;
end;

procedure TRALParams.DecodeFields(const ASource: StringRAL; AKind: TRALParamKind = rpkFIELD);
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

function TRALParams.GetParam(AName: StringRAL; AKind: TRALParamKind): TRALParam;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  Result := nil;

  for vInt := 0 to FParams.Count - 1 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if (SameText(vParam.ParamName, AName)) and (vParam.Kind = AKind) then
    begin
      Result := vParam;
      Break;
    end;
  end;
end;

function TRALParams.GetParam(AIndex: IntegerRAL; AKind: TRALParamKind): TRALParam;
var
  vInt, vIdxParam: IntegerRAL;
  vParam: TRALParam;
begin
  Result := nil;
  vIdxParam := 0;

  for vInt := 0 to FParams.Count - 1 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if (vParam.Kind = AKind) and (vIdxParam = AIndex) then
    begin
      Result := vParam;
      Break;
    end
    else if (vParam.Kind = AKind) then
    begin
      vIdxParam := vIdxParam + 1;
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

function TRALParams.GetParam(AIndex: IntegerRAL): TRALParam;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FParams.Count) then
    Result := TRALParam(FParams.Items[AIndex]);
end;

function TRALParams.GetParam(AName: StringRAL): TRALParam;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  Result := nil;

  for vInt := 0 to FParams.Count - 1 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if SameText(vParam.ParamName, AName) then
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
  FParams.Add(Result);
end;

function TRALParams.NextParamStr: StringRAL;
begin
  FNextParam := FNextParam + 1;
  Result := 'ral_param' + IntToStr(FNextParam);
end;

function TRALParams.FindNameSeparator(const ASource: StringRAL): StringRAL;
var
  vPos, vMin: IntegerRAL;
begin
  vMin := Length(ASource);
  vPos := Pos('=', ASource);
  if (vPos > 0) and (vPos < vMin) then
    Result := '=';

  vPos := Pos(StringRAL(': '), ASource);
  if (vPos > 0) and (vPos < vMin) then
    Result := ': ';
end;

procedure TRALParams.AppendParamLine(const ALine, ANameSeparator: StringRAL;
  AKind: TRALParamKind);
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
    vName := Copy(ALine, POSINISTR, vPos - 1);
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

procedure TRALParams.OnFormBodyData(Sender: TObject; AFormData: TRALMultipartFormData;
  var AFreeData: boolean);
var
  vParam: TRALParam;
begin
  vParam := NewParam;
  if AFormData.Name = '' then
    vParam.ParamName := 'ral_body' + IntToStr(NextParamInt)
  else
    vParam.ParamName := AFormData.Name;

  vParam.AsStream := AFormData.AsStream;
  vParam.FileName := AFormData.FileName;

  if AFormData.ContentType <> '' then
    vParam.ContentType := AFormData.ContentType
  else
    vParam.ContentType := rctTEXTPLAIN;

  if AFormData.Disposition <> '' then
    vParam.ContentDisposition := AFormData.Disposition;

  vParam.Kind := rpkBODY;

  AFreeData := True;
end;

function TRALParams.Compress(AStream: TStream): TStream;
var
  vCompress: TRALCompress;
  vClass: TRALCompressClass;
begin
  Result := nil;

  vClass := TRALCompress.GetCompressClass(FCompressType);
  if vClass <> nil then
  begin
    vCompress := vClass.Create;
    try
      vCompress.Format := FCompressType;
      Result := vCompress.Compress(AStream);
    finally
      vCompress.Free;
    end;
  end;
end;

function TRALParams.Encrypt(AStream: TStream): TStream;
var
  vCript: TRALCripto;
begin
  Result := nil;
  case FCriptoOptions.CriptType of
    crAES128:
    begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES128;
    end;
    crAES192:
    begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES192;
    end;
    crAES256:
    begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES256;
    end;
  end;

  try
    vCript.Key := FCriptoOptions.Key;
    Result := vCript.EncryptAsStream(AStream);
  finally
    FreeAndNil(vCript);
  end;
end;

function TRALParams.Decompress(AStream: TStream): TStream;
var
  vCompress: TRALCompress;
  vClass: TRALCompressClass;
begin
  Result := nil;

  vClass := TRALCompress.GetCompressClass(FCompressType);
  if vClass <> nil then
  begin
    vCompress := vClass.Create;
    try
      vCompress.Format := FCompressType;
      Result := vCompress.Decompress(AStream);
    finally
      vCompress.Free;
    end;
  end;
end;

function TRALParams.Decompress(const ASource: StringRAL): StringRAL;
var
  vStream, vResult: TStream;
begin
  Result := '';
  if Result <> '' then
  begin
    vStream := StringToStream(ASource);
    try
      vStream.Position := 0;
      vResult := Decompress(vStream);
      try
        Result := StreamToString(vResult);
      finally
        vResult.Free;
      end;
    finally
      vStream.Free;
    end;
  end;
end;

function TRALParams.Decrypt(AStream: TStream): TStream;
var
  vCript: TRALCripto;
begin
  Result := nil;
  case FCriptoOptions.CriptType of
    crAES128:
    begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES128;
    end;
    crAES192:
    begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES192;
    end;
    crAES256:
    begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES256;
    end;
  end;

  try
    vCript.Key := FCriptoOptions.Key;
    Result := vCript.DecryptAsStream(AStream);
  finally
    FreeAndNil(vCript);
  end;
end;

function TRALParams.Decrypt(const ASource: StringRAL): StringRAL;
var
  vCript: TRALCripto;
begin
  Result := '';
  case FCriptoOptions.CriptType of
    crAES128:
    begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES128;
    end;
    crAES192:
    begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES192;
    end;
    crAES256:
    begin
      vCript := TRALCriptoAES.Create;
      TRALCriptoAES(vCript).AESType := tAES256;
    end;
  end;

  try
    vCript.Key := FCriptoOptions.Key;
    Result := vCript.Decrypt(ASource);
  finally
    FreeAndNil(vCript);
  end;
end;

procedure TRALParams.DelParam(const AName: StringRAL; AKind: TRALParamKind);
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  for vInt := Pred(FParams.Count) downto 0 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if SameText(vParam.ParamName, AName) and (vParam.Kind = AKind) then
    begin
      vParam.Free;
      FParams.Delete(vInt);
    end;
  end;
end;

procedure TRALParams.DelParam(const AName: StringRAL);
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  for vInt := Pred(FParams.Count) downto 0 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if SameText(vParam.ParamName, AName) then
    begin
      vParam.Free;
      FParams.Delete(vInt);
    end;
  end;
end;

end.
