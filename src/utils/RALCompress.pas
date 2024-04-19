unit RALCompress;

interface

uses
  {$IFDEF FPC}
    bufstream,
  {$ENDIF}
  Classes, SysUtils, TypInfo,
  RALTypes, RALStream;

type
  TRALCompressClass = class of TRALCompress;

  TRALCompressType = (ctNone, ctDeflate, ctZLib, ctGZip, ctZStd, ctBrotli);
  TRALCompressTypes = set of TRALCompressType;
  TRALCompressLibs = (clZLib, clZStd, clBrotli);

  { TRALCompress }

  /// Compression class for PascalRAL
  TRALCompress = class(TPersistent)
  private
    FFormat: TRALCompressType;
  protected
    procedure InitCompress(AInStream, AOutStream: TStream); virtual; abstract;
    procedure InitDeCompress(AInStream, AOutStream: TStream); virtual; abstract;
    procedure SetFormat(AValue: TRALCompressType); virtual;

    class function CheckDependence : boolean; virtual; abstract;
  public
    function Compress(AStream: TStream): TStream; overload;
    function Compress(const AString: StringRAL): StringRAL; overload;
    procedure CompressFile(AInFile, AOutFile: StringRAL);
    function Decompress(AStream: TStream): TStream; overload;
    function Decompress(const AString: StringRAL): StringRAL; overload;
    procedure DecompressFile(AInFile, AOutFile: StringRAL);

    class function GetSuportedCompress : StringRAL;
    class function GetInstaledList : TStringList;
    class procedure UpdateDeclaredClasses;
    class function StringToCompress(const AStr: StringRAL): TRALCompressType;
    class function CompressToString(ACompress: TRALCompressType): StringRAL;
    class function GetBestCompress(const AEncoding : StringRAL): TRALCompressType;
    class function GetCompressClass(ACompress: TRALCompressType): TRALCompressClass;
    class procedure CheckDependencies;
  published
    property Format: TRALCompressType read FFormat write SetFormat;
  end;

implementation

const
  cCompressTypeStr : array[TRALCompressType] of StringRAL = (
                          '', 'deflate', 'zlib', 'gzip', 'zstd', 'br');
  cCompressLibsClass : array[TRALCompressLibs] of StringRAL = (
                          'TRALCompressZLib', 'TRALCompressZStd', 'TRALCompressBrotli');
  cCompressLibsTypes : array[TRALCompressLibs] of TRALCompressTypes = (
                          [ctDeflate, ctZLib, ctGZip], [ctZStd], [ctBrotli]);

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

procedure TRALCompress.SetFormat(AValue: TRALCompressType);
begin
  FFormat := AValue;
end;

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

  if vDeclaredCompressLibs[clBrotli] then
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + 'br';
  end;
end;

class function TRALCompress.GetInstaledList: TStringList;
var
  vLib: TRALCompressLibs;
  vCompress: TRALCompressClass;
  vTypes: TRALCompressTypes;
  vType: TRALCompressType;
  vStr: StringRAL;
begin
  Result := TStringList.Create;
  Result.Add(GetEnumName(TypeInfo(TRALCompressType), 0));  // clNone

  for vLib := Low(TRALCompressLibs) to High(TRALCompressLibs) do
  begin
    vCompress := TRALCompressClass(GetClass(cCompressLibsClass[vLib]));
    if (vCompress <> nil) then
    begin
      vTypes := cCompressLibsTypes[vLib];
      for vType := Low(TRALCompressType) to High(TRALCompressType) do
      begin
        if vType in vTypes then
        begin
          vStr := GetEnumName(TypeInfo(TRALCompressType), Ord(vType));
          if Result.IndexOf(vStr) < 0 then
            Result.Add(vStr);
        end;
      end;
    end;
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
  else if (vDeclaredCompressLibs[clBrotli]) and SameText(AStr, 'br') then
    Result := ctBrotli
  else
    Result := ctNone;
end;

class function TRALCompress.CompressToString(ACompress: TRALCompressType): StringRAL;
begin
  case ACompress of
    ctNone    : Result := '';
    ctGZip    : Result := 'gzip';
    ctDeflate : Result := 'deflate';
    ctZLib    : Result := 'zlib';
    ctZStd    : Result := 'zstd';
    ctBrotli  : Result := 'br';
  end;
end;

class function TRALCompress.GetBestCompress(const AEncoding: StringRAL): TRALCompressType;
var
  vStr: StringRAL;
begin
  vStr := LowerCase(AEncoding);
  if (vDeclaredCompressLibs[clZStd]) and (Pos('zstd', vStr) > 0) then
    Result := ctZStd
  else if (vDeclaredCompressLibs[clBrotli]) and (Pos('br', vStr) > 0) then
    Result := ctBrotli
  else if (vDeclaredCompressLibs[clZLib]) and (Pos('gzip', vStr) > 0) then
    Result := ctGZip
  else if (vDeclaredCompressLibs[clZLib]) and (Pos('deflate', vStr) > 0) then
    Result := ctDeflate
  else if (vDeclaredCompressLibs[clZLib]) and (Pos('zlib', vStr) > 0) then
    Result := ctZLib
  else
    Result := ctNone;
end;

class function TRALCompress.GetCompressClass(ACompress: TRALCompressType): TRALCompressClass;
begin
  Result := nil;

  case ACompress of
    ctNone    : Result := nil;
    ctGZip,
    ctDeflate,
    ctZLib    : Result := TRALCompressClass(GetClass(cCompressLibsClass[clZLib]));
    ctZStd    : Result := TRALCompressClass(GetClass(cCompressLibsClass[clZStd]));
    ctBrotli  : Result := TRALCompressClass(GetClass(cCompressLibsClass[clBrotli]));
  end;
end;

class procedure TRALCompress.CheckDependencies;
var
  vLib : TRALCompressLibs;
  vCompress : TRALCompressClass;
  vLibs : StringRAL;
begin
  vLibs := '';
  for vLib := Low(TRALCompressLibs) to High(TRALCompressLibs) do
  begin
    vCompress := TRALCompressClass(GetClass(cCompressLibsClass[vLib]));
    if (vCompress <> nil) and (not vCompress.CheckDependence) then
    begin
      if vLibs <> '' then
        vLibs := vLibs + ', ';
      vLibs := vLibs + cCompressLibsClass[vLib];
    end;
  end;

  if vLibs <> '' then
    raise Exception.CreateFmt('The classes %s - libraries files not found!', [vLibs]);
end;

initialization
  LoadDeclaredCompressLibs;

end.
