unit RALMultipartCoder;

interface

uses
  Classes, SysUtils,
  RALTypes, RALMIMETypes;

type
  { TRALMultipartFormData }

  TRALMultipartFormData = class
  private
    FFreeBuffer: boolean;
    FDisposition: StringRAL;
    FName: StringRAL;
    FFilename: StringRAL;
    FContentType: StringRAL;
    FDescription: StringRAL;
    FBufferStream: TStream;
  protected
    procedure SetBufferStream(AValue: TStream);
    function GetBufferStream: TStream;
    function GetBufferString: StringRAL;
    procedure SetBufferString(const AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ProcessHeader(AHeader: StringRAL);

    procedure SaveToFile(const AFileName: StringRAL);
    procedure SaveToStream(var AStream: TStream);
    procedure OpenFile(const AFileName: StringRAL);

    property AsStream: TStream read GetBufferStream write SetBufferStream;
    property AsString: StringRAL read GetBufferString write SetBufferString;
  published
    property Disposition: StringRAL read FDisposition write FDisposition;
    property Description: StringRAL read FDescription write FDescription;
    property Name: StringRAL read FName write FName;
    property Filename: StringRAL read FFilename write FFilename;
    property ContentType: StringRAL read FContentType write FContentType;
  end;

  TRALMultipartFormDataComplete = procedure(Sender: TObject; AFormData: TRALMultipartFormData; var AFreeData: boolean) of object;

  { TRALMultipartDecoder }

  TRALMultipartDecoder = class
  private
    FBoundary: StringRAL;
    FBuffer: array [0 .. 4095] of Byte;
    FIndex: IntegerRAL;
    FItemForm: TRALMultipartFormData;
    FWaitSepEnd: boolean;
    FIs13: boolean;
    FFormData: TList;
    FOnFormDataComplete: TRALMultipartFormDataComplete;
  protected
    procedure SetContentType(AValue: StringRAL);

    procedure ProcessBuffer(AInput: PByte; AInputLen: IntegerRAL);
    function ProcessLine : PByte;
    procedure ClearItems;

    function BurnBuffer: PByte;
    function ResetBuffer: PByte;
    function GetFormData(idx: Integer): TRALMultipartFormData;

    procedure FinalizeItem;
  public
    constructor Create;
    destructor Destroy; override;

    function FormDataCount: IntegerRAL;

    procedure ProcessMultiPart(AStream: TStream); overload;
    procedure ProcessMultiPart(const AString: StringRAL); overload;

    property FormData[idx: Integer]: TRALMultipartFormData read GetFormData;
  published
    property Boundary: StringRAL read FBoundary write FBoundary;
    property ContentType: StringRAL write SetContentType;
    property OnFormDataComplete: TRALMultipartFormDataComplete read FOnFormDataComplete write FOnFormDataComplete;
  end;

  { TRALMultipartEncoder }

  TRALMultipartEncoder = class
  private
    FBoundary: StringRAL;
    FFormData: TList;
  protected
    function GetBoundary: StringRAL;
    function GetContentType: StringRAL;

    procedure ClearItems;
  public
    constructor Create;
    destructor Destroy; override;

    function FormDataCount: IntegerRAL;

    procedure AddField(const AName: StringRAL; const AValue: StringRAL);
    procedure AddStream(const AName: StringRAL; const AFileStream: TStream;
                        const AFileName: StringRAL = ''; const AContentType: StringRAL = '');
    procedure AddFile(const AName: StringRAL; const AFileName: StringRAL; const AContentType: StringRAL = '');

    procedure SaveToFile(const AFileName: StringRAL);
    function AsStream: TStringStream;
  published
    property Boundary: StringRAL read GetBoundary write FBoundary;
    property ContentType: StringRAL read GetContentType;
  end;

implementation

{ TRALMultipartEncoder }

function TRALMultipartEncoder.GetContentType: StringRAL;
begin
  Result := rctMULTIPARTFORMDATA + '; boundary=' + Boundary;
end;

procedure TRALMultipartEncoder.ClearItems;
begin
  while FFormData.Count > 0 do
  begin
    TObject(FFormData.Items[FFormData.Count - 1]).Free;
    FFormData.Delete(FFormData.Count - 1);
  end;
end;

constructor TRALMultipartEncoder.Create;
begin
  inherited;
  FFormData := TList.Create;
end;

destructor TRALMultipartEncoder.Destroy;
begin
  ClearItems;
  FreeAndNil(FFormData);
  inherited Destroy;
end;

function TRALMultipartEncoder.FormDataCount: IntegerRAL;
begin
  Result := FFormData.Count;
end;

procedure TRALMultipartEncoder.AddField(const AName, AValue: StringRAL);
var
  vField: TRALMultipartFormData;
begin
  vField := TRALMultipartFormData.Create;
  vField.Name := AName;
  vField.AsString := AValue;
  vField.ContentType := rctTEXTPLAIN;

  FFormData.Add(vField);
end;

procedure TRALMultipartEncoder.AddStream(const AName: StringRAL; const AFileStream: TStream;
                                         const AFileName: StringRAL; const AContentType: StringRAL);
var
  vField: TRALMultipartFormData;
begin
  vField := TRALMultipartFormData.Create;
  vField.Name := AName;
  if AFileName <> '' then
    vField.Filename := ExtractFileName(AFileName);
  vField.AsStream := AFileStream;

  if AContentType <> '' then
    vField.ContentType := AContentType
  else
    vField.ContentType := rctAPPLICATIONOCTETSTREAM;

  FFormData.Add(vField);
end;

procedure TRALMultipartEncoder.AddFile(const AName, AFileName: StringRAL; const AContentType: StringRAL);
var
  vField: TRALMultipartFormData;
begin
  vField := TRALMultipartFormData.Create;
  vField.Name := AName;
  vField.Filename := ExtractFileName(AFileName);
  vField.OpenFile(AFileName);

  if AContentType <> '' then
    vField.ContentType := AContentType
  else
    vField.ContentType := rctAPPLICATIONOCTETSTREAM;

  FFormData.Add(vField);
end;

procedure TRALMultipartEncoder.SaveToFile(const AFileName: StringRAL);
var
  vFile: TStringStream;
begin
  vFile := AsStream;
  try
    vFile.SaveToFile(AFileName);
  finally
    vFile.Free;
  end;
end;

function TRALMultipartEncoder.AsStream: TStringStream;
var
  vInt: IntegerRAL;
  vHeaderFile, vHeaderField, vHeaderEnd: StringRAL;
  vItem: TRALMultipartFormData;
begin
  vHeaderFile := '----------------------------%s' + #13#10
               + 'Content-Disposition: %s; name="%s"; filename="%s"' + #13#10
               + 'Content-Type: %s' + #13#10#13#10;

  vHeaderField := '----------------------------%s' + #13#10
                + 'Content-Disposition: %s; name="%s"' + #13#10
                + 'Content-Type: %s' + #13#10#13#10;

  vHeaderEnd := '----------------------------%s--';

  Result := TStringStream.Create;
  for vInt := 0 to Pred(FFormData.Count) do
  begin
    vItem := TRALMultipartFormData(FFormData.Items[vInt]);
    if vItem.Filename <> '' then
    begin
      Result.WriteString(Format(vHeaderFile, [Boundary, vItem.Disposition, vItem.Name,
                         vItem.Filename, vItem.ContentType]));
    end
    else
    begin
      Result.WriteString(Format(vHeaderField, [Boundary, vItem.Disposition, vItem.Name,
                         vItem.ContentType]));
    end;
    vItem.AsStream.Position := 0;
    Result.CopyFrom(vItem.AsStream, vItem.AsStream.Size);
    Result.WriteString(#13#10);
  end;
  Result.WriteString(Format(vHeaderEnd, [Boundary]));
  Result.Position := 0;
end;

function TRALMultipartEncoder.GetBoundary: StringRAL;
begin
  if FBoundary = '' then
    FBoundary := 'ral' + FormatDateTime('ddmmyyyyhhnnsszzz', Now);
  Result := FBoundary;
end;

{ TRALMultipartFormData }

function TRALMultipartFormData.GetBufferStream: TStream;
begin
  Result := FBufferStream;
end;

function TRALMultipartFormData.GetBufferString: StringRAL;
begin
  if FBufferStream.InheritsFrom(TStringStream) then
  begin
    Result := TStringStream(FBufferStream).DataString;
  end
  else
  begin
    FBufferStream.Position := 0;
    SetLength(Result, FBufferStream.Size);
    FBufferStream.Write(Result[PosIniStr], FBufferStream.Size);
  end;
end;

procedure TRALMultipartFormData.SetBufferString(const AValue: StringRAL);
begin
  if (FBufferStream <> nil) and (FFreeBuffer) then
    FBufferStream.Free;

  FBufferStream := TStringStream.Create(AValue);
  FFreeBuffer := True;
end;

procedure TRALMultipartFormData.SetBufferStream(AValue: TStream);
begin
  if FBufferStream = AValue then
    Exit;

  if (FBufferStream <> nil) and (FFreeBuffer) then
    FBufferStream.Free;

  FBufferStream := AValue;
  FBufferStream.Position := 0;
  FFreeBuffer := False;
end;

constructor TRALMultipartFormData.Create;
begin
  inherited;
  FBufferStream := TStringStream.Create;
  FDisposition := 'form-data';
  FDescription := '';
  FName := '';
  FFilename := '';
  FContentType := '';
  FFreeBuffer := True;
end;

destructor TRALMultipartFormData.Destroy;
begin
  if FFreeBuffer then
    FBufferStream.Free;
  inherited Destroy;
end;

procedure TRALMultipartFormData.ProcessHeader(AHeader: StringRAL);
var
  vStr: StringRAL;

  function GetWord(var AStr: StringRAL): StringRAL;
  var
    vInt, vLen: Integer;
    vQuoted: boolean;
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
      else if not(CharInSet(vChr, [' ', '=', ';', ':'])) or vQuoted then
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

  function ProcessVar(const AHeader, AValue: StringRAL): boolean;
  begin
    Result := True;
    if SameText(AHeader, 'content-disposition') then
      FDisposition := AValue
    else if SameText(AHeader, 'name') then
      FName := AValue
    else if SameText(AHeader, 'filename') then
      FFilename := AValue
    else if SameText(AHeader, 'content-description') then
      FDescription := AValue
    else if SameText(AHeader, 'content-type') then
      FContentType := AValue
    else
      Result := False;
  end;

begin
  AHeader := Trim(AHeader);
  vStr := GetWord(AHeader);
  while (vStr <> '') do
  begin
    ProcessVar(vStr, GetWord(AHeader));
    vStr := GetWord(AHeader);
  end;
end;

procedure TRALMultipartFormData.SaveToFile(const AFileName: StringRAL);
begin
  if FBufferStream.InheritsFrom(TCustomMemoryStream) then
    TCustomMemoryStream(FBufferStream).SaveToFile(AFileName)
end;

procedure TRALMultipartFormData.SaveToStream(var AStream: TStream);
begin
  FBufferStream.Position := 0;
  AStream.Size := 0;

  AStream.CopyFrom(FBufferStream, FBufferStream.Size);

  AStream.Position := 0;
  FBufferStream.Position := 0;
end;

procedure TRALMultipartFormData.OpenFile(const AFileName: StringRAL);
begin
  if (FBufferStream <> nil) and (FFreeBuffer) then
    FBufferStream.Free;

  if FileExists(AFileName) then
    FBufferStream := TFileStream.Create(AFileName, fmOpenRead)
  else
    FBufferStream := TStringStream.Create;
  FFreeBuffer := True;
end;

{ TRALMultipartDecoder }

function TRALMultipartDecoder.GetFormData(idx: Integer): TRALMultipartFormData;
begin
  Result := nil;
  if (idx >= 0) and (idx < FFormData.Count) then
    Result := TRALMultipartFormData(FFormData.Items[idx]);
end;

procedure TRALMultipartDecoder.FinalizeItem;
var
  vFreeItem: boolean;
begin
  if FItemForm <> nil then
  begin
    // tirando CRLF do fim do arquivo
    FItemForm.AsStream.Size := FItemForm.AsStream.Size - 2;
    FItemForm.AsStream.Position := 0;

    vFreeItem := False;
    if Assigned(FOnFormDataComplete) then
      FOnFormDataComplete(Self, FItemForm, vFreeItem);

    if not vFreeItem then
      FFormData.Add(FItemForm)
    else
      FreeAndNil(FItemForm);
  end;
  FItemForm := nil;
end;

procedure TRALMultipartDecoder.SetContentType(AValue: StringRAL);
var
  vInt: IntegerRAL;
begin
  vInt := Pos('boundary', LowerCase(AValue));
  if vInt > 0 then
  begin
    Delete(AValue, 1, vInt - 1);
    vInt := Pos('=', AValue);
    Delete(AValue, 1, vInt);
    vInt := Pos(';', AValue);
    if vInt > 0 then
      Delete(AValue, vInt, Length(AValue));
    FBoundary := AValue;
  end;
end;

procedure TRALMultipartDecoder.ProcessBuffer(AInput: PByte; AInputLen: IntegerRAL);
var
  vBuffer: PByte;
begin
  vBuffer := @FBuffer[FIndex];
  while AInputLen > 0 do
  begin
    vBuffer^ := AInput^;
    Inc(FIndex);
    Inc(vBuffer);

    if AInput^ = 13 then
    begin
      FIs13 := True;
    end
    else if (AInput^ = 10) and (FIs13) then
    begin
      vBuffer := ProcessLine;
      FIs13 := False;
    end
    else
    begin
      FIs13 := False;
    end;

    if FIndex = 4096 then
      vBuffer := BurnBuffer;
    Dec(AInputLen);
    Inc(AInput);
  end;
end;

function TRALMultipartDecoder.ProcessLine : PByte;
var
  vLine: StringRAL;
begin
  if FIndex < 500 then
  begin
    SetLength(vLine, FIndex);
    Move(FBuffer[0], vLine[PosIniStr], FIndex);
    // boundary end of file
    if Pos('--' + FBoundary + '--', vLine) > 0 then
    begin
      FinalizeItem;
      Result := ResetBuffer;
    end
    // boundary begin of file
    else if Pos('--' + FBoundary + #13#10, vLine) > 0 then
    begin
      FinalizeItem;
      FItemForm := TRALMultipartFormData.Create;
      FWaitSepEnd := True;
      Result := ResetBuffer;
    end
    // line separator header e file
    else if (vLine = #13#10) and (FWaitSepEnd) then
    begin
      FWaitSepEnd := False;
      Result := ResetBuffer;
    end
    // line de headers
    else if FWaitSepEnd then
    begin
      FItemForm.ProcessHeader(vLine);
      Result := ResetBuffer;
    end
    // end of stream
    else
    begin
      Result := BurnBuffer;
    end
  end
  else
  // se o buffer tiver mais q 500 chars significa q o conteudo do
  // buffer eh o parte do arquivo e nao um novo boundary
  begin
    SetLength(vLine, FIndex);
    Move(FBuffer[0], vLine[PosIniStr], FIndex);
    Result := BurnBuffer;
  end;
end;

function TRALMultipartDecoder.BurnBuffer: PByte;
begin
  if FIndex > 0 then
    FItemForm.AsStream.Write(FBuffer[0], FIndex);
  Result := ResetBuffer;
end;

function TRALMultipartDecoder.ResetBuffer: PByte;
begin
  FIndex := 0;
  Result := @FBuffer[FIndex];
end;

procedure TRALMultipartDecoder.ClearItems;
begin
  while FFormData.Count > 0 do
  begin
    TObject(FFormData.Items[FFormData.Count - 1]).Free;
    FFormData.Delete(FFormData.Count - 1);
  end;
end;

constructor TRALMultipartDecoder.Create;
begin
  inherited;
  FFormData := TList.Create;
end;

destructor TRALMultipartDecoder.Destroy;
begin
  FItemForm := nil;
  ClearItems;
  FFormData.Free;
  inherited Destroy;
end;

procedure TRALMultipartDecoder.ProcessMultiPart(AStream: TStream);
var
  vInBuf: array [0 .. 4095] of Byte;
  vBytesRead: IntegerRAL;
  vPosition, vSize: Int64RAL;
begin
  AStream.Position := 0;
  vPosition := 0;
  vSize := AStream.Size;

  FIndex := 0;
  FWaitSepEnd := False;
  FIs13 := False;
  FItemForm := nil;

  while vPosition < vSize do
  begin
    vBytesRead := AStream.Read(vInBuf[0], Length(vInBuf));
    ProcessBuffer(@vInBuf[0], vBytesRead);
    vPosition := vPosition + vBytesRead;
  end;

  if FIndex > 0 then
    ProcessLine;
end;

procedure TRALMultipartDecoder.ProcessMultiPart(const AString: StringRAL);
var
  vInBuf: array [0 .. 4095] of Byte;
  vBytesRead: IntegerRAL;
  vPosition, vSize: Int64RAL;
begin
  vPosition := 0;
  vSize := Length(AString);

  FIndex := 0;
  FWaitSepEnd := False;
  FIs13 := False;
  FItemForm := nil;

  while vPosition < vSize do
  begin
    vBytesRead := 4096;
    if vSize - vPosition < 4096 then
      vBytesRead := vSize - vPosition;

    Move(AString[vPosition + PosIniStr], vInBuf[0], vBytesRead);
    ProcessBuffer(@vInBuf[0], vBytesRead);

    vPosition := vPosition + vBytesRead;
  end;

  if FIndex > 0 then
    ProcessLine;
end;

function TRALMultipartDecoder.FormDataCount: IntegerRAL;
begin
  Result := FFormData.Count;
end;

end.
