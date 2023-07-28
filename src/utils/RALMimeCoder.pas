unit RALMimeCoder;

interface

uses
  Classes, SysUtils,
  RALTypes;

type
  { TRALMIMEFormData }

  TRALMIMEFormData = class
  private
    FDisposition : StringRAL;
    FName : StringRAL;
    FFilename : StringRAL;
    FContentType: StringRAL;
    FDescription : StringRAL;
    FBufferStream : TMemoryStream;
  protected
    property BufferStream : TMemoryStream read FBufferStream write FBufferStream;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ProcessHeader(AHeader : StringRAL);

    procedure SaveToFile(AFileName : StringRAL); overload;
    procedure SaveToFile(var AStream : TStream); overload;

    property AsStream : TMemoryStream read FBufferStream;
  published
    property Disposition : StringRAL read FDisposition write FDisposition;
    property Description : StringRAL read FDescription write FDescription;
    property Name : StringRAL read FName write FName;
    property Filename : StringRAL read FFilename write FFilename;
    property ContentType: StringRAL read FContentType write FContentType;
  end;

  TRALMIMEFormDataComplete = procedure(Sender : TObject; AFormData : TRALMIMEFormData; var AFreeData : boolean) of object;

  { TRALMIMEDecoder }

  TRALMIMEDecoder = class
  private
    FBoundary : StringRAL;
    FBuffer : array[0..4095] of Byte;
    FIndex : IntegerRAL;
    FItemForm : TRALMimeFormData;
    FWaitSepEnd : boolean;
    FIs13 : boolean;
    FFormData : TList;
    FOnFormDataComplete : TRALMIMEFormDataComplete;
  protected
    procedure SetContentType(AValue : StringRAL);

    procedure ProcessBuffer(AInput : PByte; AInputLen: IntegerRAL);
    procedure ClearItems;

    function BurnBuffer : PByte;
    function ResetBuffer : PByte;
    function GetFormData(idx : Integer) : TRALMIMEFormData;

    procedure FinalizeItem;
  public
    constructor Create;
    destructor Destroy; override;

    function FormDataCount : IntegerRAL;

    procedure ProcessMultiPart(AStream : TStream); overload;
    procedure ProcessMultiPart(AString : StringRAL); overload;

    property FormData[Idx : Integer] : TRALMIMEFormData read GetFormData;
  published
    property Boundary : StringRAL read FBoundary write FBoundary;
    property ContentType : StringRAL write SetContentType;
    property OnFormDataComplete : TRALMIMEFormDataComplete read FOnFormDataComplete write FOnFormDataComplete;
  end;

implementation

{ TRALMIMEFormData }

constructor TRALMIMEFormData.Create;
begin
  inherited;
  FBufferStream := TMemoryStream.Create;
  FDisposition := '';
  FDescription := '';
  FName := '';
  FFilename := '';
  FContentType := '';
end;

destructor TRALMIMEFormData.Destroy;
begin
  FBufferStream.Free;
  inherited Destroy;
end;

procedure TRALMIMEFormData.ProcessHeader(AHeader : StringRAL);
var
  vStr : StringRAL;

  function GetWord(var AStr : StringRAL) : StringRAL;
  var
    vInt,vLen : Integer;
    vQuoted : Boolean;
    vChr : Char;
  begin
    Result := '';
    vLen := Length(AStr);
    vQuoted := False;
    for vInt := 1 to vLen do
    begin
      vChr := AStr[vInt];
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
    Result:=True;
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
    ProcessVar(vStr,GetWord(AHeader));
    vStr := GetWord(AHeader);
  end;
end;

procedure TRALMIMEFormData.SaveToFile(AFileName : StringRAL);
begin
  FBufferStream.SaveToFile(AFileName);
end;

procedure TRALMIMEFormData.SaveToFile(var AStream : TStream);
begin
  FBufferStream.Position := 0;
  AStream.Size := 0;

  AStream.CopyFrom(FBufferStream, FBufferStream.Size);

  AStream.Position := 0;
  FBufferStream.Position := 0;
end;

{ TRALMIMEDecoder }

function TRALMIMEDecoder.GetFormData(idx : Integer) : TRALMIMEFormData;
begin
  Result := nil;
  if (idx >= 0) and (idx < FFormData.Count) then
    Result := TRALMimeFormData(FFormData.Items[idx]);
end;

procedure TRALMIMEDecoder.FinalizeItem;
var
  vFreeItem : boolean;
begin
  if FItemForm <> nil then
  begin
    // tirando CRLF do fim do arquivo
    FItemForm.BufferStream.Size := FItemForm.BufferStream.Size - 2;

    vFreeItem := False;
    if Assigned(FOnFormDataComplete) then
      FOnFormDataComplete(Self, FItemForm, vFreeItem);

    if not vFreeItem then
      FFormData.Add(FItemForm)
    else
      FreeAndNil(FItemForm);
  end;
end;

procedure TRALMIMEDecoder.SetContentType(AValue : StringRAL);
var
  vInt : IntegerRAL;
begin
  vInt := Pos('boundary', LowerCase(AValue));
  if vInt > 0 then
  begin
    Delete(AValue, 1, vInt - 1);
    vInt := Pos('=', AValue);
    Delete(AValue, 1, vInt);
    FBoundary := AValue;
  end;
end;

procedure TRALMIMEDecoder.ProcessBuffer(AInput : PByte; AInputLen : IntegerRAL);
var
  vBuffer : PByte;
  vLine : StringRAL;
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
      if FIndex < 500 then
      begin
        SetLength(vLine, FIndex);
        Move(FBuffer[0], vLine[1], FIndex);
        // boundary end of file
        if Pos('--'+FBoundary+'--', vLine) > 0 then
        begin
          FinalizeItem;
          vBuffer := ResetBuffer;
        end
        // boundary begin of file
        else if Pos('--'+FBoundary+#13#10, vLine) > 0 then
        begin
          FinalizeItem;
          FItemForm := TRALMIMEFormData.Create;
          FWaitSepEnd := True;
          vBuffer := ResetBuffer;
        end
        // line separator header e file
        else if (vLine = #13#10) and (FWaitSepEnd) then
        begin
          FWaitSepEnd := False;
          vBuffer := ResetBuffer;
        end
        // line de headers
        else if FWaitSepEnd then
        begin
          FItemForm.ProcessHeader(vLine);
          vBuffer := ResetBuffer;
        end
        // end of stream
        else
        begin
          vBuffer := BurnBuffer;
        end;
      end
      // se o buffer tiver mais q 500 chars significa q o conteudo do
      // buffer eh o parte do arquivo e nao um novo boundary
      else
      begin
        vBuffer := BurnBuffer;
      end;
      FIs13 := False;
    end;

    if FIndex = 4096 then
      vBuffer := BurnBuffer;
    Dec(AInputLen);
    Inc(AInput);
  end;
end;

function TRALMIMEDecoder.BurnBuffer : PByte;
begin
  FItemForm.BufferStream.Write(FBuffer[0],FIndex);
  Result := ResetBuffer;
end;

function TRALMIMEDecoder.ResetBuffer : PByte;
begin
  FIndex := 0;
  Result := @FBuffer[FIndex];
end;

procedure TRALMIMEDecoder.ClearItems;
begin
  while FFormData.Count > 0 do
  begin
    TObject(FFormData.Items[FFormData.Count - 1]).Free;
    FFormData.Delete(FFormData.Count - 1);
  end;
end;

constructor TRALMIMEDecoder.Create;
begin
  inherited;
  FFormData := TList.Create;
end;

destructor TRALMIMEDecoder.Destroy;
begin
  FItemForm := nil;
  ClearItems;
  FFormData.Free;
  inherited Destroy;
end;

procedure TRALMIMEDecoder.ProcessMultiPart(AStream : TStream);
var
  vInBuf: array[0..4095] of Byte;
  vBytesRead : IntegerRAL;
  vPosition, vSize : Int64RAL;
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
end;

procedure TRALMIMEDecoder.ProcessMultiPart(AString : StringRAL);
var
  vInBuf: array[0..4095] of Byte;
  vBytesRead : IntegerRAL;
  vPosition, vSize : Int64RAL;
begin
  vPosition := 1;
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

    Move(AString[vPosition], vInBuf[0], vBytesRead);
    ProcessBuffer(@vInBuf[0], vBytesRead);

    vPosition := vPosition + vBytesRead;
  end;
end;

function TRALMIMEDecoder.FormDataCount : IntegerRAL;
begin
  Result := FFormData.Count;
end;

end.

