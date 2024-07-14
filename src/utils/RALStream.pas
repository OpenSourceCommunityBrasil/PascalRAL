/// Class with all the functions related to Stream on RAL
unit RALStream;

interface

uses
  Classes, SysUtils,
  RALTypes;

type

  { TRALBinaryWriter }

  TRALBinaryWriter = class
  private
    FStream : TStream;
  protected
    // read and write UTF-7
    function ReadSize: QWord;
    procedure WriteSize(ASize: QWord);
  public
    constructor Create(const AStream : TStream);

    procedure WriteShortint(AValue: Shortint);
    procedure WriteByte(AValue: Byte);
    procedure WriteSmallint(AValue: Smallint);
    procedure WriteWord(AValue: Word);
    procedure WriteInteger(AValue : IntegerRAL);
    procedure WriteLongWord(AValue: LongWord);
    procedure WriteInt64(AValue: Int64RAL);
    procedure WriteQWord(AValue: QWord);
    procedure WriteBoolean(AValue: Boolean);
    procedure WriteFloat(AValue: Double);
    procedure WriteDateTime(AValue: TDateTime);
    procedure WriteStream(AValue: TStream);
    procedure WriteBytes(AValue: TBytes);
    procedure WriteString(AValue: StringRAL);

    function ReadShortint: Shortint;
    function ReadByte: Byte;
    function ReadSmallint: Smallint;
    function ReadWord: Word;
    function ReadInteger: IntegerRAL;
    function ReadLongWord: LongWord;
    function ReadInt64: Int64RAL;
    function ReadQWord: QWord;
    function ReadBoolean: Boolean;
    function ReadFloat: Double;
    function ReadDateTime: TDateTime;
    procedure ReadStream(AStream: TStream);
    function ReadBytes: TBytes;
    function ReadString: StringRAL;
  end;

/// Creates a TStream and write ABytes on it
function BytesToStream(ABytes: TBytes): TStream;
/// Saves the stream into a file given the AFileName
procedure SaveStream(AStream: TStream; const AFileName: StringRAL);
/// Converts a given AStream to TBytes
function StreamToBytes(AStream: TStream): TBytes;
/// Converts a given AStream to an UTF8String
function StreamToString(AStream: TStream): StringRAL;
/// Creates a TStream and writes the given AStr into it
function StringToStream(const AStr: StringRAL): TStream;

implementation

function StringToStream(const AStr: StringRAL): TStream;
begin
  Result := TStringStream.Create(AStr);
  Result.Position := 0;
end;

function StreamToString(AStream: TStream): StringRAL;
begin
  Result := '';
  if (AStream = nil) or (AStream.Size = 0) then
    Exit;

  AStream.Position := 0;

  if AStream is TStringStream then
  begin
    Result := TStringStream(AStream).DataString;
  end
  else if AStream.InheritsFrom(TMemoryStream) then
  begin
    SetLength(Result, AStream.Size);
    Move(TMemoryStream(AStream).Memory^, Result[PosIniStr], AStream.Size);
  end
  else
  begin
    SetLength(Result, AStream.Size);
    AStream.Read(Result[PosIniStr], AStream.Size);
  end;
end;

function BytesToStream(ABytes: TBytes): TStream;
begin
  Result := TMemoryStream.Create;
  Result.Write(ABytes[0], Length(ABytes));
  Result.Position := 0;
end;

function StreamToBytes(AStream: TStream): TBytes;
begin
  AStream.Position := 0;

  if AStream.InheritsFrom(TMemoryStream) then
  begin
    SetLength(Result, AStream.Size);
    Move(TMemoryStream(AStream).Memory^, Result[0], AStream.Size);
  end
  else
  begin
    SetLength(Result, AStream.Size);
    AStream.Read(Result[0], AStream.Size);
  end;
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

{ TRALBinaryWriter }

function TRALBinaryWriter.ReadSize: QWord;
var
  vMult: integer;
  vByte: Byte;
begin
  Result := 0;
  vMult := 0;
  repeat
    vByte := ReadByte;
    Result := Result + ((vByte and 127) shl vMult);
    vMult := vMult + 7;
  until (vByte and 128) = 0;
end;

procedure TRALBinaryWriter.WriteSize(ASize: QWord);
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

procedure TRALBinaryWriter.WriteQWord(AValue: QWord);
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
var
  vQWord : QWord;
begin
  AValue.Position := 0;

  WriteSize(AValue.Size);
  FStream.CopyFrom(AValue, AValue.Size);
end;

procedure TRALBinaryWriter.WriteBytes(AValue: TBytes);
var
  vQWord : QWord;
begin
  WriteSize(Length(AValue));
  if Length(AValue) > 0 then
    FStream.Write(AValue[0], Length(AValue));
end;

procedure TRALBinaryWriter.WriteString(AValue: StringRAL);
begin
  WriteSize(Length(AValue));
  if AValue <> '' then
    FStream.Write(AValue[POSINISTR], Length(AValue));
end;

function TRALBinaryWriter.ReadShortint: Shortint;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadByte: Byte;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadSmallint: Smallint;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadWord: Word;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadInteger: IntegerRAL;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadLongWord: LongWord;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadInt64: Int64RAL;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadQWord: QWord;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadBoolean: Boolean;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadFloat: Double;
begin
  FStream.Read(Result, SizeOf(Result));
end;

function TRALBinaryWriter.ReadDateTime: TDateTime;
begin
  FStream.Read(Result, SizeOf(Result));
end;

procedure TRALBinaryWriter.ReadStream(AStream: TStream);
var
  vQWord : QWord;
begin
  vQWord := ReadSize;
  AStream.CopyFrom(FStream, vQWord);
end;

function TRALBinaryWriter.ReadBytes: TBytes;
var
  vQWord: QWord;
begin
  vQWord := ReadSize;
  SetLength(Result, vQWord);
  if vQWord > 0 then
    FStream.Read(Result[0], vQWord);
end;

function TRALBinaryWriter.ReadString: StringRAL;
var
  vQWord : QWord;
begin
  vQWord := ReadSize;
  Result := '';
  if vQWord > 0 then
  begin
    SetLength(Result, vQWord);
    FStream.Read(Result[POSINISTR], vQWord);
  end;
end;

end.
