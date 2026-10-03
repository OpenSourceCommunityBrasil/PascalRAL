/// Class with all the functions related to Stream on RAL
unit RALStream;

interface

{$I ..\base\PascalRAL.inc}

uses
  Classes, SysUtils,
  RALTypes, RALConsts;

type

  { TRALBinaryWriter }

  TRALBinaryWriter = class
  private
    FStream: TStream;
    /// Refuses a size prefix that announces more than the stream still holds
    procedure CheckSize(ASize: UInt64RAL);
    /// Reads exactly ACount bytes or raises - see there
    procedure ReadExact(var ABuffer; ACount: IntegerRAL);
  protected
    // read and write UTF-7
    function ReadSize: UInt64RAL;
    procedure WriteSize(ASize: UInt64RAL);
  public
    constructor Create(const AStream: TStream);

    procedure WriteShortint(AValue: Shortint);
    procedure WriteByte(AValue: Byte);
    procedure WriteSmallint(AValue: Smallint);
    procedure WriteWord(AValue: Word);
    procedure WriteInteger(AValue: IntegerRAL);
    procedure WriteLongWord(AValue: LongWord);
    procedure WriteInt64(AValue: Int64RAL);
    procedure WriteQWord(AValue: UInt64);
    procedure WriteBoolean(AValue: Boolean);
    procedure WriteFloat(AValue: Double);
    procedure WriteDateTime(AValue: TDateTime);
    procedure WriteStream(AValue: TStream);
    procedure WriteBytes(AValue: TBytes);
    procedure WriteString(AValue: StringRAL);
    procedure WriteChar(AValue: CharRAL);
    procedure WriteBytesDirect(AValue: TBytes);

    function ReadShortint: Shortint;
    function ReadByte: Byte;
    function ReadSmallint: Smallint;
    function ReadWord: Word;
    function ReadInteger: IntegerRAL;
    function ReadLongWord: LongWord;
    function ReadInt64: Int64RAL;
    function ReadQWord: UInt64;
    function ReadBoolean: Boolean;
    function ReadFloat: Double;
    function ReadDateTime: TDateTime;
    procedure ReadStream(AStream: TStream);
    function ReadBytes: TBytes;
    function ReadString: StringRAL;
    function ReadChar: CharRAL;
    function ReadBytesDirect(ALength: integer): TBytes;
  end;

  TRALStringStream = class(TMemoryStream)
  public
    constructor Create(AString: StringRAL); overload;
    constructor Create(AStream: TStream); overload;
    constructor Create(ABytes: TBytes); overload;

    function DataString: StringRAL;

    procedure WriteBytes(ABytes: TBytes);
    procedure WriteString(AString: StringRAL);
    procedure WriteStream(AStream: TStream);
  end;

/// Saves the stream into a file given the AFileName
procedure SaveStream(AStream: TStream; const AFileName: StringRAL);
/// Creates a TStream and write ABytes on it
function BytesToStream(ABytes: TBytes): TStream;
/// Converts a given AStream to TBytes
function StreamToBytes(AStream: TStream): TBytes;
// Converts a given AStream to an UTF8String
function StreamToString(AStream: TStream): StringRAL;
// Converts a given AStream to a byte string
function StreamToByteString(AStream: TStream): StringRAL;
// Creates a TStream and writes the given AStr into it
function StringToStreamUTF8(const AStr: StringRAL): TStream;
function StringToStream(const AStr: StringRAL): TStream;

implementation

{ Every [0] below is guarded by a length: on an empty array it is past the end,
  and $R+ refuses it even for a count of 0. Built that way, an empty POST - which
  Indy hands over as an empty stream - failed in here, and Indy answered its own
  "200 OK" page to every one of them }

function BytesToStream(ABytes: TBytes): TStream;
begin
  Result := TMemoryStream.Create;
  if Length(ABytes) > 0 then
    Result.Write(ABytes[0], Length(ABytes));
  Result.Position := 0;
end;

function StreamToBytes(AStream: TStream): TBytes;
begin
  AStream.Position := 0;
  SetLength(Result, AStream.Size);
  if Length(Result) = 0 then
    Exit;

  if AStream.InheritsFrom(TMemoryStream) then
    Move(TMemoryStream(AStream).Memory^, Result[0], Length(Result))
  else
    AStream.Read(Result[0], Length(Result));
end;

procedure SaveStream(AStream: TStream; const AFileName: StringRAL);
var
  vFile: TFileStream;
begin
  AStream.Position := 0;

  vFile := TFileStream.Create(AFileName, fmCreate);
  try
    vFile.Size := AStream.Size;
    vFile.Position := 0;
    vFile.CopyFrom(AStream, AStream.Size);
  finally
    vFile.Free;
  end;
end;

function StringToStream(const AStr: StringRAL): TStream;
var
  vBytes : TBytes;
begin
  vBytes := StringToBytes(AStr);
  Result := BytesToStream(vBytes)
end;

function StringToStreamUTF8(const AStr: StringRAL): TStream;
begin
  Result := TRALStringStream.Create(AStr);
  Result.Position := 0;
end;

function StreamToByteString(AStream: TStream): StringRAL;
var
  vBytes: TBytes;
begin
  SetLength(vBytes, AStream.Size);
  if Length(vBytes) > 0 then
    AStream.Read(vBytes[0], Length(vBytes));
  Result := BytesToString(vBytes)
end;

function StreamToString(AStream: TStream): StringRAL;
var
  vBytes: TBytes;
begin
  Result := '';
  if (AStream = nil) or (AStream.Size = 0) then
    Exit;

  AStream.Position := 0;

  if AStream.InheritsFrom(TStringStream) then
  begin
    Result := TStringStream(AStream).DataString;
  end
  else if AStream.InheritsFrom(TRALStringStream) then
  begin
    Result := TRALStringStream(AStream).DataString;
  end
  else
  begin
    SetLength(vBytes, AStream.Size);
    AStream.Read(vBytes[0], AStream.Size);
    Result := BytesToStringUTF8(vBytes)
  end;
end;

{ TRALBinaryWriter }

function TRALBinaryWriter.ReadSize: UInt64RAL;
var
  vMult: IntegerRAL;
  vByte: Byte;
begin
  Result := 0;
  vMult := 0;

  { a stream that ends before the size does is malformed, and ReadByte now
    says so: this used to answer whatever it had read so far - 0 at the very
    end - so a count read off the network could drive a caller's loop through
    billions of empty strings past the end of a twenty-byte body }
  repeat
    vByte := ReadByte;
    { widened before the shift: "(vByte and 127) shl 28" is 32-bit arithmetic
      on both compilers, so every size from 2 GB up wrapped to garbage }
    Result := Result + (UInt64RAL(vByte and 127) shl vMult);
    vMult := vMult + 7;
  until (vByte and 128) = 0;
end;

procedure TRALBinaryWriter.WriteSize(ASize: UInt64RAL);
var
  vByte: Byte;
begin
  while ASize >= 0 do begin
    vByte := ASize and 127;
    ASize := ASize shr 7;
    if ASize > 0 then
      vByte := vByte or 128;

    WriteByte(vByte);

    if ASize = 0 then
      Break;
  end;
end;

constructor TRALBinaryWriter.Create(const AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

procedure TRALBinaryWriter.WriteShortint(AValue: Shortint);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteByte(AValue: Byte);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteSmallint(AValue: Smallint);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteWord(AValue: Word);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteInteger(AValue: IntegerRAL);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteLongWord(AValue: LongWord);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteInt64(AValue: Int64RAL);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteQWord(AValue: UInt64);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteBoolean(AValue: Boolean);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteFloat(AValue: Double);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteDateTime(AValue: TDateTime);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

procedure TRALBinaryWriter.WriteStream(AValue: TStream);
begin
  AValue.Position := 0;

  WriteSize(AValue.Size);
  FStream.CopyFrom(AValue, AValue.Size);
end;

procedure TRALBinaryWriter.WriteBytes(AValue: TBytes);
begin
  WriteSize(Length(AValue));
  if Length(AValue) > 0 then
    FStream.Write(AValue[0], Length(AValue));
end;

procedure TRALBinaryWriter.WriteString(AValue: StringRAL);
var
  vBytes : TBytes;
begin
  vBytes := StringToBytesUTF8(AValue);
  WriteSize(Length(vBytes));
  if Length(vBytes) > 0 then
    FStream.Write(vBytes[0], Length(vBytes));
end;

procedure TRALBinaryWriter.WriteBytesDirect(AValue: TBytes);
begin
  if Length(AValue) > 0 then
    FStream.Write(AValue[0], Length(AValue));
end;

procedure TRALBinaryWriter.WriteChar(AValue: CharRAL);
begin
  FStream.Write(AValue, SizeOf(AValue));
end;

function TRALBinaryWriter.ReadShortint: Shortint;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadByte: Byte;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadSmallint: Smallint;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadWord: Word;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadInteger: IntegerRAL;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadLongWord: LongWord;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadInt64: Int64RAL;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadQWord: UInt64;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadBoolean: Boolean;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadFloat: Double;
begin
  ReadExact(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadDateTime: TDateTime;
begin
  ReadExact(Result, SizeOf(Result));
end;

{ Every fixed-size read goes through here. Stream.Read returns how much it
  actually read, and nothing looked: at the end of the stream the value came
  back as whatever the stack held - and these values come off the network,
  where one of them was cast straight to an enum that indexes a table. }
procedure TRALBinaryWriter.ReadExact(var ABuffer; ACount: IntegerRAL);
begin
  if FStream.Read(ABuffer, ACount) <> ACount then
    raise Exception.Create(emStreamSizeBeyondEnd);
end;

{ the size prefix is a varint: nine bytes can announce 2^63 bytes. Allocating
  or copying what it says before checking what the stream still holds let a
  twenty-byte packet ask for gigabytes - out of memory on the server }
procedure TRALBinaryWriter.CheckSize(ASize: UInt64RAL);
begin
  if ASize > UInt64RAL(FStream.Size - FStream.Position) then
    raise Exception.Create(emStreamSizeBeyondEnd);
end;

procedure TRALBinaryWriter.ReadStream(AStream: TStream);
var
  vQWord : UInt64RAL;
begin
  vQWord := ReadSize;
  CheckSize(vQWord);
  AStream.CopyFrom(FStream, vQWord);
  AStream.Position := 0;
end;

function TRALBinaryWriter.ReadBytes: TBytes;
var
  vQWord: UInt64RAL;
begin
  vQWord := ReadSize;
  CheckSize(vQWord);
  SetLength(Result, vQWord);
  if vQWord > 0 then
    FStream.Read(Result[0], vQWord);
end;

function TRALBinaryWriter.ReadString: StringRAL;
var
  vQWord : UInt64RAL;
  vBytes : TBytes;
begin
  vQWord := ReadSize;
  CheckSize(vQWord);
  Result := '';
  if vQWord > 0 then
  begin
    SetLength(vBytes, vQWord);
    FStream.Read(vBytes[0], vQWord);
    Result := BytesToStringUTF8(vBytes);
  end;
end;

function TRALBinaryWriter.ReadBytesDirect(ALength: integer): TBytes;
begin
  SetLength(Result, ALength);
  if ALength > 0 then
    ReadExact(Result[0], ALength);
end;

function TRALBinaryWriter.ReadChar: CharRAL;
begin
  ReadExact(Result, SizeOf(Result));
end;

{ TRALStringStream }

constructor TRALStringStream.Create(ABytes: TBytes);
begin
  inherited Create;
  WriteBytes(ABytes);
end;

constructor TRALStringStream.Create(AString: StringRAL);
var
  vBytes: TBytes;
begin
  inherited Create;
  WriteString(AString);
end;

constructor TRALStringStream.Create(AStream: TStream);
var
  vStream: TStringStream;
  vBytes: TBytes;
begin
  inherited Create;
  WriteStream(AStream);
end;

function TRALStringStream.DataString: StringRAL;
var
  vBytes: TBytes;
begin
  Result := '';
  Self.Position := 0;
  if Self.Size = 0 then
    Exit;

  SetLength(vBytes, Self.Size);
  Read(vBytes[0], Self.Size);
  Result := BytesToStringUTF8(vBytes);
end;

procedure TRALStringStream.WriteBytes(ABytes: TBytes);
begin
  if Length(ABytes) > 0 then
    Write(ABytes[0], Length(ABytes));
end;

procedure TRALStringStream.WriteString(AString: StringRAL);
var
  vBytes : TBytes;
begin
  vBytes := StringToBytesUTF8(AString);
  WriteBytes(vBytes);
end;

{ a count of 0 copies the whole of AStream from its start, on both compilers -
  straight across, where a copy into an array first doubled the body in memory }
procedure TRALStringStream.WriteStream(AStream: TStream);
begin
  CopyFrom(AStream, 0);
end;

end.
