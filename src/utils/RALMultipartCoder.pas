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
    FClosed: boolean;
    FFormData: TList;
    FIndex: IntegerRAL;
    FIs13: boolean;
    FItemForm: TRALMultipartFormData;
    FPartCount: IntegerRAL;
    FWaitSepEnd: boolean;
    FOnFormDataComplete: TRALMultipartFormDataComplete;
  protected
    /// Resets what one body processed leaves behind, before the next one
    procedure BeginBody;
    /// used to write the info of the Multipart into the stream buffer
    function BurnBuffer: PByte;
    /// destroys the content of the Multipart
    procedure ClearItems;
    /// Ends a body: its last line, and the part it never closed
    procedure EndBody;
    /// Method called at the end of the Multipart processing to remove linebreaks
    procedure FinalizeItem;
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
    /// Whether the last body processed ended with its close delimiter - what
    /// an empty form, with no part at all, still carries
    property Closed: boolean read FClosed;
    /// The parts the last body processed was cut into, including those
    /// OnFormDataComplete took over. A part the body never closed is not one
    property PartCount: IntegerRAL read FPartCount;
  published
    property Boundary: StringRAL read FBoundary write FBoundary;
    property ContentType: StringRAL write SetContentType;
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
  public
    constructor Create;
    destructor Destroy; override;
    /// Adds a pair of UTF8String to the Multipart
    procedure AddField(const AName: StringRAL; const AValue: StringRAL);
    /// Adds a Stream into the multipart with the ContentType informed
    procedure AddStream(const AName: StringRAL; const AFileStream: TStream;
      const AFileName: StringRAL = ''; const AContentType: StringRAL = '');
    /// Adds a file into the multipart based on the AFileName
    procedure AddFile(const AName: StringRAL; const AFileName: StringRAL;
      const AContentType: StringRAL = '');
    /// Returns a stream with the content of the Multipart
    function AsStream: TStream;
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

function TRALMultipartEncoder.AsStream: TStream;
var
  vInt: IntegerRAL;
  vHeaderFile, vHeaderField, vHeaderEnd: StringRAL;
  vItem: TRALMultipartFormData;
  vString, vFile: StringRAL;
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
    error - requests reached the handler with no params, no body, no cookies. }
  vHeaderFile := '--%s' + HTTPLineBreak +
    'Content-Disposition: %s; name="%s"; filename="%s"' + HTTPLineBreak + 'Content-Type: %s' +
    HTTPLineBreak+HTTPLineBreak;

  vHeaderField := '--%s' + HTTPLineBreak +
    'Content-Disposition: %s; name="%s"' + HTTPLineBreak + 'Content-Type: %s' + HTTPLineBreak+HTTPLineBreak;

  vHeaderEnd := '--%s--';

  Result := TRALStringStream.Create;
  for vInt := 0 to Pred(FFormData.Count) do
  begin
    vItem := TRALMultipartFormData(FFormData.Items[vInt]);

    { A part names a file only when the caller gave it a filename. The encoder
      does not invent one: whether a part should look like a file on the wire
      is a decision about what the part IS, and only the caller knows that -
      see TRALParams.EncodeBody, which names its own envelope parts and leaves
      plain form fields alone. }
    vFile := vItem.Filename;
    if vFile <> '' then
      vString := Format(vHeaderFile, [Boundary, vItem.Disposition, vItem.Name,
        vFile, vItem.ContentType])
    else
      vString := Format(vHeaderField, [Boundary, vItem.Disposition, vItem.Name,
        vItem.ContentType]);
    TRALStringStream(Result).WriteString(vString);
    vItem.AsStream.Position := 0;
    Result.CopyFrom(vItem.AsStream, vItem.AsStream.Size);

    vString := HTTPLineBreak;
    TRALStringStream(Result).WriteString(vString);
  end;
  vString := Format(vHeaderEnd, [Boundary]);
  TRALStringStream(Result).WriteString(vString);
  Result.Position := 0;
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

procedure TRALMultipartDecoder.FinalizeItem;
var
  vFreeItem: boolean;
begin
  if FItemForm <> nil then
  begin
    // drop the HTTPLineBreak that closes the part; an empty part has none
    if FItemForm.AsStream.Size >= 2 then
      FItemForm.AsStream.Size := FItemForm.AsStream.Size - 2;
    FItemForm.AsStream.Position := 0;
    Inc(FPartCount);

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
    { RFC 2046 lets the boundary travel quoted - boundary="..." is what .NET's
      HttpClient sends - and the quotes are not part of it: kept, they made a
      delimiter that never matched, and the whole body was dropped under a 200.
      A boundary has no ';' of its own, so cutting there was already safe }
    AValue := RALTrim(AValue);
    if (Length(AValue) >= 2) and (AValue[POSINISTR] = '"') and
       (AValue[RALHighStr(AValue)] = '"') then
      AValue := Copy(AValue, POSINISTR + 1, Length(AValue) - 2);
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
begin
  if FIndex < MultipartLineLength then
  begin
    { the line's own bytes, no more: it used to convert the whole 64 KB buffer
      and cut the result down, on every line of the body - and ResetBuffer
      zeroed those 64 KB after each one so the conversion would stop there }
    SetLength(vLine, FIndex);
    if FIndex > 0 then
      Move(FBuffer[0], vLine[POSINISTR], FIndex);

    // boundary end of file
    if Pos('--' + FBoundary + '--', vLine) > 0 then
    begin
      FinalizeItem;
      FClosed := True;
      Result := ResetBuffer;
    end
    // boundary begin of file
    else if Pos('--' + FBoundary + HTTPLineBreak, vLine) > 0 then
    begin
      FinalizeItem;
      FItemForm := TRALMultipartFormData.Create;
      FWaitSepEnd := True;
      Result := ResetBuffer;
    end
    // line separator header e file
    else if (vLine = HTTPLineBreak) and (FWaitSepEnd) then
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
    {$IFDEF RAL_DEBUG}
    vLine := BytesToStringUTF8(FBuffer);
    SetLength(vLine, FIndex);
    {$ENDIF}
    Result := BurnBuffer;
  end;
end;

function TRALMultipartDecoder.BurnBuffer: PByte;
begin
  { no part open: the preamble before the first delimiter or the epilogue
    after the last, which RFC 2046 says to ignore. Writing them went through
    a nil FItemForm - an access violation from any body that had one }
  if (FIndex > 0) and (FItemForm <> nil) then
    FItemForm.AsStream.Write(FBuffer[0], FIndex);
  Result := ResetBuffer;
end;

function TRALMultipartDecoder.ResetBuffer: PByte;
begin
  FIndex := 0; // FIndex is the end of the data: what lies past it is never read
  Result := @FBuffer[FIndex];
end;

procedure TRALMultipartDecoder.BeginBody;
begin
  FIndex := 0;
  FWaitSepEnd := False;
  FIs13 := False;
  FreeAndNil(FItemForm);
  FPartCount := 0;
  FClosed := False;
end;

procedure TRALMultipartDecoder.EndBody;
begin
  if FIndex > 0 then
    ProcessLine;
  { a part the body opened and never closed is not delivered. It never was -
    but nothing freed it either: the destructor only let go of the pointer,
    and a truncated upload leaked its part, whatever it held }
  FreeAndNil(FItemForm);
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
  { an open part is never in FFormData: FinalizeItem hands it over or frees
    it, and lets go of it either way }
  FreeAndNil(FItemForm);
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
  BeginBody;
  { with no boundary nothing can be delimited: an empty one made any line
    holding '--' a delimiter }
  if FBoundary = '' then
    Exit;

  AStream.Position := 0;
  vPosition := 0;
  vSize := AStream.Size;

  if vSize > DEFAULTBUFFERSTREAMSIZE then
    SetLength(vInBuf, DEFAULTBUFFERSTREAMSIZE)
  else
    SetLength(vInBuf, vSize);

  while vPosition < vSize do
  begin
    vBytesRead := AStream.Read(vInBuf[0], Length(vInBuf));
    // a stream that holds less than its Size said: the loop never ended
    if vBytesRead <= 0 then
      Break;
    ProcessBuffer(@vInBuf[0], vBytesRead);
    vPosition := vPosition + vBytesRead;
  end;

  EndBody;
end;

procedure TRALMultipartDecoder.ProcessMultiPart(const AString: StringRAL);
var
  vInBuf: array of Byte;
  vBytesRead: IntegerRAL;
  vPosition, vSize: Int64RAL;
begin
  BeginBody;
  if FBoundary = '' then
    Exit;

  vPosition := 0;
  vSize := Length(AString);

  if vSize > DEFAULTBUFFERSTREAMSIZE then
    SetLength(vInBuf, DEFAULTBUFFERSTREAMSIZE)
  else
    SetLength(vInBuf, vSize);

  while vPosition < vSize do
  begin
    vBytesRead := Length(vInBuf);
    if vSize - vPosition < Length(vInBuf) then
      vBytesRead := vSize - vPosition;

    Move(AString[vPosition + PosIniStr], vInBuf[0], vBytesRead);
    ProcessBuffer(@vInBuf[0], vBytesRead);

    vPosition := vPosition + vBytesRead;
  end;

  EndBody;
end;

function TRALMultipartDecoder.FormDataCount: IntegerRAL;
begin
  Result := FFormData.Count;
end;

end.
