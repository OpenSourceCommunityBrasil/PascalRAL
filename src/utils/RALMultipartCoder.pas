/// Base Class for PascalRAL's Multipart processor
unit RALMultipartCoder;

{$I ..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALMIMETypes, RALStream, RALConsts, RALTools;

type
  { TRALMultipartFormData }

  /// Base class for the Multipart object of the request
  TRALMultipartFormData = class
  private
    FBufferStream: TStream;
    FContentType: StringRAL;
    FDescription: StringRAL;
    FDisposition: StringRAL;
    FFilename: StringRAL;
    FFreeBuffer: boolean;
    FName: StringRAL;
  protected
    function GetBufferStream: TStream;
    function GetBufferString: StringRAL;
    procedure SetBufferStream(AValue: TStream);
    procedure SetBufferString(const AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;
    /// Initializes a File by its AFileName
    procedure OpenFile(const AFileName: StringRAL);
    /// Parse the Header of the Multipart Form
    procedure ProcessHeader(AHeader: StringRAL);
    /// Saves the Multipart content to a AFileName in the disk
    procedure SaveToFile(const AFileName: StringRAL);
    /// Saves the Multipart content into an AStream
    procedure SaveToStream(var AStream: TStream);
    /// Takes AStream as the content and owns it from now on
    procedure AdoptBuffer(AStream: TStream);
    { Hands the content over: the caller owns it from now on and this part is
      left empty. A copy when this part did not own it - a stream lent by the
      caller of TRALMultipartEncoder.AddStream }
    function ReleaseBuffer: TStream;
    /// Whether the content is this part's own, freed with it
    property OwnsBuffer: boolean read FFreeBuffer;

    property AsStream: TStream read GetBufferStream write SetBufferStream;
    property AsString: StringRAL read GetBufferString write SetBufferString;
  published
    property ContentType: StringRAL read FContentType write FContentType;
    property Description: StringRAL read FDescription write FDescription;
    property Disposition: StringRAL read FDisposition write FDisposition;
    property Filename: StringRAL read FFilename write FFilename;
    property Name: StringRAL read FName write FName;
  end;

  /// Event fired at the end of the processing of MultipartForm
  TRALMultipartFormDataComplete = procedure(Sender: TObject;
    AFormData: TRALMultipartFormData; var AFreeData: boolean) of object;

  { TRALMultipartDecoder }

  /// Base class for the object that will parse Multipart from the HTTP Request
  TRALMultipartDecoder = class
  private
    FBoundary: StringRAL;
    FBuffer: TBytes;
    FFormData: TList;
    FIndex: IntegerRAL;
    FIs13: boolean;
    FItemForm: TRALMultipartFormData;
    FWaitSepEnd: boolean;
    FOnFormDataComplete: TRALMultipartFormDataComplete;
    { parts as windows over the body instead of copies: the absolute position
      of the byte being read, where the content of the current part started,
      and the stream they are windows of }
    FSliceParts: boolean;
    FSliceSource: TStream;
    FAbs: Int64RAL;
    FContentStart: Int64RAL;
  protected
    /// used to write the info of the Multipart into the stream buffer
    function BurnBuffer: PByte;
    /// destroys the content of the Multipart
    procedure ClearItems;
    { Method called at the end of each part. ALineStart is where the delimiter
      line that ends it starts: the content ends two bytes before (its CRLF) }
    procedure FinalizeItem(ALineStart: Int64RAL);
    /// Gets an item from the FormData based on the index provided
    function GetFormData(idx: Integer): TRALMultipartFormData;
    /// Main method that reads the Multipart
    procedure ProcessBuffer(AInput: PByte; AInputLen: IntegerRAL);
    /// Function that separates Multipart by its lines
    function ProcessLine: PByte;
    /// Function that initializes the Multipart buffer
    function ResetBuffer: PByte;
    /// Setter function for Content-type
    procedure SetContentType(AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;

    /// Returns the ammount of items in the Multipart
    function FormDataCount: IntegerRAL;
    /// Processes the Multipart from a Stream input
    procedure ProcessMultiPart(AStream: TStream); overload;
    /// Processes the Multipart from a String input
    procedure ProcessMultiPart(const AString: StringRAL); overload;
    /// Gets an item from the FormData based on the index provided
    property FormData[idx: Integer]: TRALMultipartFormData read GetFormData;
  published
    property Boundary: StringRAL read FBoundary write FBoundary;
    property ContentType: StringRAL write SetContentType;
    { True: each part's content is a window (RALStreamSlice) over the stream
      given to ProcessMultiPart, not a copy - the caller keeps that stream
      alive for as long as the parts are read. The string overload always
      copies }
    property SliceParts: boolean read FSliceParts write FSliceParts;
    property OnFormDataComplete: TRALMultipartFormDataComplete read FOnFormDataComplete
      write FOnFormDataComplete;
  end;

  { TRALMultipartEncoder }

  /// Base class for the object that will create a Multipart on HTTP response
  TRALMultipartEncoder = class
  private
    FBoundary: StringRAL;
    FFormData: TList;
  protected
    procedure ClearItems;
    function GetBoundary: StringRAL;
    function GetContentType: StringRAL;
    /// The delimiter and the headers of one part, up to the blank line
    function PartHeader(AItem: TRALMultipartFormData): StringRAL;
    /// The closing delimiter
    function EndDelimiter: StringRAL;
  public
    constructor Create;
    destructor Destroy; override;
    /// Adds a pair of UTF8String to the Multipart
    procedure AddField(const AName: StringRAL; const AValue: StringRAL);
    /// Adds a Stream into the multipart with the ContentType informed
    procedure AddStream(const AName: StringRAL; const AFileStream: TStream;
      const AFileName: StringRAL = ''; const AContentType: StringRAL = ''); overload;
    /// The same, taking AFileStream over when AOwned: it is freed with the
    /// encoder, or moves into the stream AsConcatStream answers
    procedure AddStream(const AName: StringRAL; const AFileStream: TStream;
      const AFileName, AContentType: StringRAL; AOwned: boolean); overload;
    /// Adds a file into the multipart based on the AFileName
    procedure AddFile(const AName: StringRAL; const AFileName: StringRAL;
      const AContentType: StringRAL = '');
    /// Returns a stream with the content of the Multipart
    function AsStream: TStream;
    { The multipart as a TRALConcatStream: the headers and each part's stream,
      read in turn, nothing joined. The parts this encoder owns move into it;
      the ones it was lent stay referenced, and must outlive it }
    function AsConcatStream: TStream;
    /// Gets the ammount of items in the Multipart
    function FormDataCount: IntegerRAL;
    /// Saves the content of the Multipart to an AFileName file
    procedure SaveToFile(const AFileName: StringRAL);
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

procedure TRALMultipartEncoder.AddStream(const AName: StringRAL;
  const AFileStream: TStream; const AFileName: StringRAL; const AContentType: StringRAL);
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

procedure TRALMultipartEncoder.AddStream(const AName: StringRAL;
  const AFileStream: TStream; const AFileName, AContentType: StringRAL; AOwned: boolean);
begin
  AddStream(AName, AFileStream, AFileName, AContentType);
  if AOwned then
    TRALMultipartFormData(FFormData.Items[FFormData.Count - 1]).AdoptBuffer(AFileStream);
end;

procedure TRALMultipartEncoder.AddFile(const AName, AFileName: StringRAL;
  const AContentType: StringRAL);
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
  vFile: TStream;
begin
  vFile := AsStream;
  try
    SaveStream(vFile, AFileName);
  finally
    vFile.Free;
  end;
end;

function TRALMultipartEncoder.PartHeader(AItem: TRALMultipartFormData): StringRAL;
begin
  { The delimiter is "--" plus the boundary, and nothing else. RFC 2046 defines
    it that way, and the Content-Type we send declares the boundary alone
    ("boundary=ralNNN"), so the twenty-eight dashes written here did not match
    what we announced.

    It went unnoticed because every parser this had met is lenient: RAL's own
    decoder below looks for the delimiter with Pos(), a substring search that
    finds "--ralNNN" inside "----------------------------ralNNN", and Indy,
    mORMot2 and fpHTTP all hand the raw body to that same decoder. The
    libmicrohttpd parser under the Sagui engine anchors the delimiter at the
    start of the line, finds no match, and drops the whole body without an
    error - requests reached the handler with no params, no body, no cookies.

    A part names a file only when the caller gave it a filename. The encoder
    does not invent one: whether a part should look like a file on the wire
    is a decision about what the part IS, and only the caller knows that -
    see TRALParams.EncodeBody, which names its own envelope parts and leaves
    plain form fields alone. }
  if AItem.Filename <> '' then
    Result := Format('--%s' + HTTPLineBreak +
      'Content-Disposition: %s; name="%s"; filename="%s"' + HTTPLineBreak +
      'Content-Type: %s' + HTTPLineBreak + HTTPLineBreak,
      [Boundary, AItem.Disposition, AItem.Name, AItem.Filename, AItem.ContentType])
  else
    Result := Format('--%s' + HTTPLineBreak +
      'Content-Disposition: %s; name="%s"' + HTTPLineBreak +
      'Content-Type: %s' + HTTPLineBreak + HTTPLineBreak,
      [Boundary, AItem.Disposition, AItem.Name, AItem.ContentType]);
end;

function TRALMultipartEncoder.EndDelimiter: StringRAL;
begin
  Result := Format('--%s--', [Boundary]);
end;

function TRALMultipartEncoder.AsStream: TStream;
var
  vInt: IntegerRAL;
  vItem: TRALMultipartFormData;
begin
  Result := TRALStringStream.Create;
  for vInt := 0 to Pred(FFormData.Count) do
  begin
    vItem := TRALMultipartFormData(FFormData.Items[vInt]);
    TRALStringStream(Result).WriteString(PartHeader(vItem));
    if vItem.AsStream <> nil then
    begin
      vItem.AsStream.Position := 0;
      RALCopyStream(vItem.AsStream, Result, vItem.AsStream.Size);
    end;
    TRALStringStream(Result).WriteString(HTTPLineBreak);
  end;
  TRALStringStream(Result).WriteString(EndDelimiter);
  Result.Position := 0;
end;

function TRALMultipartEncoder.AsConcatStream: TStream;
var
  vInt: IntegerRAL;
  vItem: TRALMultipartFormData;
  vConcat: TRALConcatStream;
  vOwned: boolean;
begin
  vConcat := TRALConcatStream.Create;
  try
    for vInt := 0 to Pred(FFormData.Count) do
    begin
      vItem := TRALMultipartFormData(FFormData.Items[vInt]);
      vConcat.Add(RawByteString(PartHeader(vItem)));
      if vItem.AsStream <> nil then
      begin
        vOwned := vItem.OwnsBuffer;
        if vOwned then
          vConcat.Add(vItem.ReleaseBuffer, True)
        else
          vConcat.Add(vItem.AsStream, False);
      end;
      vConcat.Add(RawByteString(HTTPLineBreak));
    end;
    vConcat.Add(RawByteString(EndDelimiter));
  except
    vConcat.Free;
    raise;
  end;
  Result := vConcat;
end;

function TRALMultipartEncoder.GetBoundary: StringRAL;
var
  vBytes: TBytes;
  vInt: IntegerRAL;
begin
  { random, not the clock: a boundary an attacker can predict lets a crafted
    part value end the multipart early }
  if FBoundary = '' then
  begin
    vBytes := RandomBytes(12);
    FBoundary := 'ral';
    for vInt := 0 to High(vBytes) do
      FBoundary := FBoundary + StringRAL(IntToHex(vBytes[vInt], 2));
  end;
  Result := FBoundary;
end;

{ TRALMultipartFormData }

function TRALMultipartFormData.GetBufferStream: TStream;
begin
  Result := FBufferStream;
end;

function TRALMultipartFormData.GetBufferString: StringRAL;
begin
  Result := StreamToString(FBufferStream);
end;

procedure TRALMultipartFormData.SetBufferString(const AValue: StringRAL);
begin
  if (FBufferStream <> nil) and (FFreeBuffer) then
    FBufferStream.Free;

  FBufferStream := StringToStream(AValue);
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
  FBufferStream := TMemoryStream.Create;
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
      vChr := CharRAL(AStr[vInt]);
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
    if RALSameName(AHeader, 'content-disposition') then
      FDisposition := AValue
    else if RALSameName(AHeader, 'name') then
      FName := AValue
    else if RALSameName(AHeader, 'filename') then
      FFilename := AValue
    else if RALSameName(AHeader, 'content-description') then
      FDescription := AValue
    else if RALSameName(AHeader, 'content-type') then
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

procedure TRALMultipartFormData.AdoptBuffer(AStream: TStream);
begin
  if (FBufferStream <> nil) and FFreeBuffer and (FBufferStream <> AStream) then
    FBufferStream.Free;
  FBufferStream := AStream;
  if FBufferStream <> nil then
    FBufferStream.Position := 0;
  FFreeBuffer := True;
end;

function TRALMultipartFormData.ReleaseBuffer: TStream;
begin
  if FFreeBuffer then
  begin
    Result := FBufferStream;
    FBufferStream := nil;
    FFreeBuffer := False;
  end
  else
  begin
    Result := TMemoryStream.Create;
    if FBufferStream <> nil then
    begin
      FBufferStream.Position := 0;
      RALCopyStream(FBufferStream, Result, FBufferStream.Size);
    end;
  end;
  if Result <> nil then
    Result.Position := 0;
end;

procedure TRALMultipartFormData.SaveToFile(const AFileName: StringRAL);
begin
  SaveStream(FBufferStream, AFileName);
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
    FBufferStream := TMemoryStream.Create;
  FFreeBuffer := True;
end;

{ TRALMultipartDecoder }

function TRALMultipartDecoder.GetFormData(idx: Integer): TRALMultipartFormData;
begin
  Result := nil;
  if (idx >= 0) and (idx < FFormData.Count) then
    Result := TRALMultipartFormData(FFormData.Items[idx]);
end;

procedure TRALMultipartDecoder.FinalizeItem(ALineStart: Int64RAL);
var
  vFreeItem: boolean;
  vEnd: Int64RAL;
begin
  if FItemForm <> nil then
  begin
    if FSliceSource <> nil then
    begin
      { the content is what lies between the blank line after the headers and
        the CRLF before this delimiter }
      if FContentStart < 0 then
        FContentStart := ALineStart;
      vEnd := ALineStart - 2;
      if vEnd < FContentStart then
        vEnd := FContentStart;
      FItemForm.AdoptBuffer(RALStreamSlice(FSliceSource, FContentStart,
        vEnd - FContentStart));
    end
    else
    begin
      // drop the HTTPLineBreak that closes the part; an empty part has none
      if FItemForm.AsStream.Size >= 2 then
        FItemForm.AsStream.Size := FItemForm.AsStream.Size - 2;
    end;
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
    Delete(AValue, 1, vInt);
    Delete(AValue, 1, Pos('=', AValue));
    vInt := Pos(';', AValue);
    if vInt > 0 then
      Delete(AValue, vInt, Length(AValue));
    FBoundary := AValue;
  end;
end;

procedure TRALMultipartDecoder.ProcessBuffer(AInput: PByte; AInputLen: IntegerRAL);
var
  vBuffer: PByte;
  {$IFDEF RAL_DEBUG}
  vLine: StringRAL;
  {$ENDIF}
begin
  vBuffer := @FBuffer[FIndex];
  while AInputLen > 0 do
  begin
    { counts the byte being read: from here on FAbs - FIndex is where the line
      in the buffer starts, and FAbs is where the next byte is }
    Inc(FAbs);
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

    if FIndex = Length(FBuffer) then
    begin
      {$IFDEF RAL_DEBUG}
      SetLength(vLine, FIndex);
      Move(FBuffer[0], vLine[PosIniStr], FIndex);
      {$ENDIF}
      vBuffer := BurnBuffer;
    end;
    Dec(AInputLen);
    Inc(AInput);
  end;
end;

function TRALMultipartDecoder.ProcessLine: PByte;
var
  vLine: StringRAL;
  vLineStart: Int64RAL;
begin
  vLineStart := FAbs - FIndex;
  if FIndex < MultipartLineLength then
  begin
    vLine := BytesToStringUTF8(FBuffer);
    SetLength(vLine, FIndex);

    // boundary end of file
    if Pos('--' + FBoundary + '--', vLine) > 0 then
    begin
      FinalizeItem(vLineStart);
      Result := ResetBuffer;
    end
    // boundary begin of file
    else if Pos('--' + FBoundary + HTTPLineBreak, vLine) > 0 then
    begin
      FinalizeItem(vLineStart);
      FItemForm := TRALMultipartFormData.Create;
      FContentStart := -1;
      FWaitSepEnd := True;
      Result := ResetBuffer;
    end
    // line separator header e file
    else if (vLine = HTTPLineBreak) and (FWaitSepEnd) then
    begin
      FWaitSepEnd := False;
      FContentStart := FAbs;
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
    {$IFDEF RAL_DEBUG}
    vLine := BytesToStringUTF8(FBuffer);
    SetLength(vLine, FIndex);
    {$ENDIF}
    Result := BurnBuffer;
  end;
end;

function TRALMultipartDecoder.BurnBuffer: PByte;
begin
  { windows over the body need nothing written: the content stays where it is }
  if (FIndex > 0) and (FSliceSource = nil) and (FItemForm <> nil) then
    FItemForm.AsStream.Write(FBuffer[0], FIndex);
  Result := ResetBuffer;
end;

function TRALMultipartDecoder.ResetBuffer: PByte;
begin
  FIndex := 0;
  FillChar(FBuffer[0], Length(FBuffer), 0);
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
  SetLength(FBuffer, DEFAULTDECODERBUFFERSIZE); //65536
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
  vInBuf: array of Byte;
  vBytesRead: IntegerRAL;
  vPosition, vSize: Int64RAL;
begin
  AStream.Position := 0;
  vPosition := 0;
  vSize := AStream.Size;

  if vSize > DEFAULTBUFFERSTREAMSIZE then
    SetLength(vInBuf, DEFAULTBUFFERSTREAMSIZE)
  else
    SetLength(vInBuf, vSize);

  FIndex := 0;
  FAbs := 0;
  FContentStart := -1;
  FWaitSepEnd := False;
  FIs13 := False;
  FItemForm := nil;
  if FSliceParts then
    FSliceSource := AStream
  else
    FSliceSource := nil;
  try
    while vPosition < vSize do
    begin
      { a window reads the parent itself: position it where this piece is }
      AStream.Position := vPosition;
      vBytesRead := AStream.Read(vInBuf[0], Length(vInBuf));
      if vBytesRead <= 0 then
        Break;
      ProcessBuffer(@vInBuf[0], vBytesRead);
      vPosition := vPosition + vBytesRead;
    end;

    if FIndex > 0 then
      ProcessLine;
  finally
    FSliceSource := nil;
  end;
end;

procedure TRALMultipartDecoder.ProcessMultiPart(const AString: StringRAL);
var
  vInBuf: array of Byte;
  vBytesRead: IntegerRAL;
  vPosition, vSize: Int64RAL;
begin
  vPosition := 0;
  vSize := Length(AString);

  if vSize > DEFAULTBUFFERSTREAMSIZE then
    SetLength(vInBuf, DEFAULTBUFFERSTREAMSIZE)
  else
    SetLength(vInBuf, vSize);

  FIndex := 0;
  FAbs := 0;
  FContentStart := -1;
  FWaitSepEnd := False;
  FIs13 := False;
  FItemForm := nil;
  FSliceSource := nil;

  while vPosition < vSize do
  begin
    vBytesRead := Length(vInBuf);
    if vSize - vPosition < Length(vInBuf) then
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
