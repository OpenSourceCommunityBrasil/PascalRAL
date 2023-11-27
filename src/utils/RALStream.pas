unit RALStream;

interface

uses
  Classes, SysUtils,
  RALTypes;

function StringToStream(AStr : StringRAL) : TStream;
function StreamToString(AStream : TStream) : StringRAL;

function BytesToStream(ABytes : TBytes) : TStream;
function StreamToBytes(AStream : TStream) : TBytes;

procedure SaveStream(AStream : TStream; AFileName : StringRAL);

implementation

function StringToStream(AStr : StringRAL) : TStream;
begin
  Result := TStringStream.Create(AStr);
  Result.Position := 0;
end;

function StreamToString(AStream : TStream) : StringRAL;
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

function BytesToStream(ABytes : TBytes) : TStream;
begin
  Result := TMemoryStream.Create;
  Result.Write(ABytes[0], Length(ABytes));
  Result.Position := 0;
end;

function StreamToBytes(AStream : TStream) : TBytes;
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

procedure SaveStream(AStream : TStream; AFileName : StringRAL);
var
  vFile : TFileStream;
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
