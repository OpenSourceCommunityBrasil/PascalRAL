/// Class with all the functions related to Stream on RAL
unit RALStream;

interface

uses
  Classes, SysUtils,
  RALTypes;

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

end.
