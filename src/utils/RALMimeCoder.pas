unit RALMimeCoder;

interface

uses
  Classes, SysUtils,
  RALTypes;

type

  { TRALMimeFormData }

  TRALMimeFormData = class
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
  published
    property Disposition : StringRAL read FDisposition write FDisposition;
    property Description : StringRAL read FDescription write FDescription;
    property Name : StringRAL read FName write FName;
    property Filename : StringRAL read FFilename write FFilename;
    property ContentType: StringRAL read FContentType write FContentType;
  end;

  { TRALMimeDecoder }

  TRALMimeDecoder = class
  private
    FBoundary : StringRAL;
    FBuffer : array[0..4095] of Byte;
    FIndex : IntegerRAL;
    FItemForm : TRALMimeFormData;
    FWaitSepEnd : boolean;
    FIs13 : boolean;
    FFormData : TList;
  protected
    procedure ProcessBuffer(AInput : PByte; AInputLen: IntegerRAL);
    procedure ClearItems;

    function BurnBuffer : PByte;
    function ResetBuffer : PByte;
    function NewItem : TRALMimeFormData;
    function GetFormData(idx : Integer) : TRALMimeFormData;
  public
    constructor Create;
    destructor Destroy; override;

    function FormDataCount : IntegerRAL;

    procedure ProcessMultiPart(AStream : TStream); overload;
    procedure ProcessMultiPart(AString : StringRAL); overload;

    property FormData[Idx : Integer] : TRALMimeFormData read GetFormData;
  published
    property Boundary : StringRAL read FBoundary write FBoundary;
  end;

implementation

{ TRALMimeFormData }

constructor TRALMimeFormData.Create;
begin
  inherited;
  FBufferStream := TMemoryStream.Create;
  FDisposition := '';
  FDescription := '';
  FName := '';
  FFilename := '';
  FContentType := '';
end;

destructor TRALMimeFormData.Destroy;
begin
  FBufferStream.Free;
  inherited Destroy;
end;

procedure TRALMimeFormData.ProcessHeader(AHeader : StringRAL);
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
      else if not (vChr in [' ','=',';',':']) or vQuoted then
      begin
          Result := Result + vChr;
      end
      else if (vChr in [';',':','=']) and (not vQuoted) then
      begin
        Delete(AStr,1,vInt);
        Exit;
      end;
    end;
    AStr := '';
  end;

  function ProcessVar(const AHeader, AValue: StringRAL): Boolean;
  begin
    Result:=True;
    if SameText(AHeader,'content-disposition') then
      FDisposition := AValue
    else if SameText(AHeader,'name') then
      FName := AValue
    else if SameText(AHeader,'filename') then
      FFilename := AValue
    else if SameText(AHeader,'content-description') then
      FDescription := AValue
    else if SameText(AHeader,'content-type') then
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

procedure TRALMimeFormData.SaveToFile(AFileName : StringRAL);
begin
  FBufferStream.SaveToFile(AFileName);
end;

procedure TRALMimeFormData.SaveToFile(var AStream : TStream);
begin
  FBufferStream.Position := 0;
  AStream.Size := 0;

  AStream.CopyFrom(FBufferStream, FBufferStream.Size);

  AStream.Position := 0;
  FBufferStream.Position := 0;
end;

{ TRALMimeDecoder }

function TRALMimeDecoder.GetFormData(idx : Integer) : TRALMimeFormData;
begin
  Result := nil;
  if (idx >= 0) and (idx < FFormData.Count) then
    Result := TRALMimeFormData(FFormData.Items[idx]);
end;

procedure TRALMimeDecoder.ProcessBuffer(AInput : PByte; AInputLen : IntegerRAL);
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

    if AInput^ = 13 then begin
      FIs13 := True;
    end
    else if (AInput^ = 10) and (FIs13) then begin
      if FIndex < 500 then begin
        SetLength(vLine,FIndex);
        Move(FBuffer[0],vLine[1],FIndex);
        // boundary end of file
        if Pos('--'+FBoundary+'--',vLine) > 0 then begin
          vBuffer := ResetBuffer;
        end
        // boundary begin of file
        else if Pos('--'+FBoundary+#13#10,vLine) > 0 then begin
          FItemForm := NewItem;
          FWaitSepEnd := True;
          vBuffer := ResetBuffer;
        end
        // line separator header e file
        else if (vLine = #13#10) and (FWaitSepEnd) then begin
          FWaitSepEnd := False;
          vBuffer := ResetBuffer;
        end
        // line de headers
        else if FWaitSepEnd then begin
          FItemForm.ProcessHeader(vLine);
          vBuffer := ResetBuffer;
        end
        // end of stream
        else begin
          vBuffer := BurnBuffer;
        end;
      end
      // se o buffer tiver mais q 500 chars significa q o conteudo do
      // buffer eh o parte do arquivo e nao um novo boundary
      else begin
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

function TRALMimeDecoder.BurnBuffer : PByte;
begin
  FItemForm.BufferStream.Write(FBuffer[0],FIndex);
  Result := ResetBuffer;
end;

function TRALMimeDecoder.ResetBuffer : PByte;
begin
  FIndex := 0;
  Result := @FBuffer[FIndex];
end;

function TRALMimeDecoder.NewItem : TRALMimeFormData;
begin
  Result := TRALMimeFormData.Create;
  FFormData.Add(Result);
end;

procedure TRALMimeDecoder.ClearItems;
begin
  while FFormData.Count > 0 do begin
    TObject(FFormData.Items[FFormData.Count-1]).Free;
    FFormData.Delete(FFormData.Count-1);
  end;
end;

constructor TRALMimeDecoder.Create;
begin
  inherited;
  FFormData := TList.Create;
end;

destructor TRALMimeDecoder.Destroy;
begin
  FItemForm := nil;
  ClearItems;
  FFormData.Free;
  inherited Destroy;
end;

procedure TRALMimeDecoder.ProcessMultiPart(AStream : TStream);
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

procedure TRALMimeDecoder.ProcessMultiPart(AString : StringRAL);
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

    Move(AString[vPosition],vInBuf[0],vBytesRead);
    ProcessBuffer(@vInBuf[0], vBytesRead);

    vPosition := vPosition + vBytesRead;
  end;
end;

function TRALMimeDecoder.FormDataCount : IntegerRAL;
begin
  Result := FFormData.Count;
end;

end.

