unit RALCompress;

interface

uses
  {$IFDEF FPC}
    bufstream,
  {$ENDIF}
  Classes, SysUtils,
  RALTypes, RALStream;

type
  TRALCompressType = (ctNone, ctDeflate, ctZLib, ctGZip, ctZStd);
  TRALCompressLibs = (clZLib, clZStd);

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
    class procedure UpdateDeclaredClasses;
    class function StringToCompress(const AStr: StringRAL): TRALCompressType;
    class function CompressToString(ACompress: TRALCompressType): StringRAL;
    class function GetBestCompress(const AEncoding : StringRAL) : TRALCompressType;
  end;

  TRALCompressClass = class of TRALCompress;

implementation

const
  cCompressTypeStr : array[TRALCompressType] of StringRAL = (
                          '','deflate','zlib','gzip','zstd');
  cCompressLibsClass : array[TRALCompressLibs] of StringRAL = (
                          'TRALCompressZLib','TRALCompressZStd');

var
  vDeclaredCompressLibs : array[TRALCompressLibs] of boolean;

procedure LoadDeclaredCompressLibs;
var
  vLib : TRALCompressLibs;
begin
  for vLib := Low(TRALCompressLibs) to High(TRALCompressLibs) do
    vDeclaredCompressLibs[vLib] := GetClass(cCompressLibsClass[vLib]) <> nil;
end;

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
  if vDeclaredCompressLibs[clZLib] then
    Result := 'gzip, zlib, deflate';

  if vDeclaredCompressLibs[clZStd] then
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + 'zstd';
  end;
end;

class procedure TRALCompress.UpdateDeclaredClasses;
begin
  LoadDeclaredCompressLibs;
end;

class function TRALCompress.StringToCompress(const AStr: StringRAL): TRALCompressType;
begin
  if (vDeclaredCompressLibs[clZLib]) and SameText(AStr, 'gzip') then
    Result := ctGZip
  else if (vDeclaredCompressLibs[clZLib]) and SameText(AStr, 'zlib') then
    Result := ctZLib
  else if (vDeclaredCompressLibs[clZLib]) and SameText(AStr, 'deflate') then
    Result := ctDeflate
  else if (vDeclaredCompressLibs[clZStd]) and SameText(AStr, 'zstd') then
    Result := ctZStd
  else
    Result := ctNone;
end;

class function TRALCompress.CompressToString(ACompress: TRALCompressType): StringRAL;
begin
  case ACompress of
    ctNone: Result := '';
    ctGZip: Result := 'gzip';
    ctDeflate: Result := 'deflate';
    ctZLib: Result := 'zlib';
    ctZStd: Result := 'zstd';
  end;
end;

class function TRALCompress.GetBestCompress(const AEncoding: StringRAL): TRALCompressType;
var
  vStr: StringRAL;
begin
  vStr := LowerCase(AEncoding);
  if (vDeclaredCompressLibs[clZStd]) and (Pos('zstd', vStr) > 0) then
    Result := ctZStd
  else if (vDeclaredCompressLibs[clZLib]) and (Pos('gzip', vStr) > 0) then
    Result := ctGZip
  else if (vDeclaredCompressLibs[clZLib]) and (Pos('deflate', vStr) > 0) then
    Result := ctDeflate
  else if (vDeclaredCompressLibs[clZLib]) and (Pos('zlib', vStr) > 0) then
    Result := ctZLib
  else
    Result := ctNone;
end;

initialization
  LoadDeclaredCompressLibs;

end.
