unit RALCompress;

interface

uses
  {$IFDEF FPC}
    bufstream,
  {$ENDIF}
  Classes, SysUtils,
  RALTypes, RALStream;

type
  { TRALCompress }

  /// Compression class for PascalRAL
  TRALCompress = class(TPersistent)
  protected
    procedure InitCompress(AInStream, AOutStream: TStream); virtual; abstract;
    procedure InitDeCompress(AInStream, AOutStream: TStream); virtual; abstract;
  public
    function Compress(AStream: TStream): TStream; overload;
    function Compress(const AString: StringRAL): StringRAL; overload;
    procedure CompressFile(AInFile, AOutFile: StringRAL);
    function Decompress(AStream: TStream): TStream; overload;
    function Decompress(const AString: StringRAL): StringRAL; overload;
    procedure DecompressFile(AInFile, AOutFile: StringRAL);

    class function GetSuportedCompress : StringRAL;
  end;

  TRALCompressClass = class of TRALCompress;

implementation

{ TRALCompress }

function TRALCompress.Compress(AStream: TStream): TStream;
begin
  Result := TMemoryStream.Create;
  InitCompress(AStream, Result);
end;

function TRALCompress.Compress(const AString: StringRAL): StringRAL;
var
  vStr, vRes: TStream;
begin
  vStr := StringToStream(AString);
  try
    vRes := Compress(vStr);
    try
      Result := StreamToString(vRes);
    finally
      FreeAndNil(vRes);
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

procedure TRALCompress.CompressFile(AInFile, AOutFile: StringRAL);
var
  vInStream: TFileStream;
  vOutStream: TBufferedFileStream;
begin
  vInStream := TFileStream.Create(AInFile, fmOpenRead or fmShareDenyWrite);
  vOutStream := TBufferedFileStream.Create(AOutFile, fmCreate);
  try
    vInStream.Position := 0;
    InitCompress(vInStream, vOutStream);
  finally
    FreeAndNil(vInStream);
    FreeAndNil(vOutStream);
  end;
end;

function TRALCompress.Decompress(const AString: StringRAL): StringRAL;
var
  vStr, vRes: TStream;
begin
  vStr := StringToStream(AString);
  try
    vRes := Decompress(vStr);
    try
      Result := StreamToString(vRes);
    finally
      FreeAndNil(vRes);
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

function TRALCompress.Decompress(AStream: TStream): TStream;
begin
  Result := TMemoryStream.Create;
  InitDeCompress(AStream, Result);
end;

procedure TRALCompress.DecompressFile(AInFile, AOutFile: StringRAL);
var
  vInStream: TFileStream;
  vOutStream: TBufferedFileStream;
begin
  vInStream := TFileStream.Create(AInFile, fmOpenReadWrite);
  vOutStream := TBufferedFileStream.Create(AOutFile, fmCreate);
  try
    vInStream.Position := 0;
    InitDeCompress(vInStream, vOutStream);
  finally
    FreeAndNil(vInStream);
    FreeAndNil(vOutStream);
  end;
end;

class function TRALCompress.GetSuportedCompress: StringRAL;
begin
  Result := '';
  if GetClass('TRALCompressZLib') <> nil then
    Result := 'gzip, zlib, deflate';

  if GetClass('TRALCompressZStd') <> nil then
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + 'zstd';
  end;
end;

end.
