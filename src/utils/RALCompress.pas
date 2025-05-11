/// Unit that handles base compression definitions used on the package
unit RALCompress;

interface

uses
  {$IFDEF FPC}
    bufstream,
  {$ENDIF}
  Classes, SysUtils, TypInfo,
  RALTypes, RALStream, RALConsts;

type
  TRALCompressClass = class of TRALCompress;

  TRALCompressType = (ctNone, ctDeflate, ctZLib, ctGZip, ctZStd, ctBrotli);
  TRALCompressTypes = set of TRALCompressType;

  { TRALCompress }

  /// Compression class for PascalRAL
  TRALCompress = class(TPersistent)
  private
    FFormat: TRALCompressType;
  protected
    procedure InitCompress(AInStream, AOutStream: TStream); virtual; abstract;
    procedure InitDeCompress(AInStream, AOutStream: TStream); virtual; abstract;
    procedure SetFormat(AValue: TRALCompressType); virtual;
  public
    function Compress(AStream: TStream): TStream; overload;
    function Compress(const AString: StringRAL): StringRAL; overload;
    procedure CompressFile(AInFile, AOutFile: StringRAL);
    function Decompress(AStream: TStream): TStream; overload;
    function Decompress(const AString: StringRAL): StringRAL; overload;
    procedure DecompressFile(AInFile, AOutFile: StringRAL);

    class function CompressToString(ACompress: TRALCompressType): StringRAL;
    class function StringToCompress(const AStr: StringRAL): TRALCompressType;
    class function GetBestCompress(const AEncoding: StringRAL): TRALCompressType;
    class function CompressTypes : TRALCompressTypes; virtual; abstract;
    class function BestCompressFromClass(ATypes : TRALCompressTypes) : TRALCompressType; virtual;
  published
    property Format: TRALCompressType read FFormat write SetFormat;
  end;

  procedure RegisterCompress(ACompress : TRALCompressClass);
  procedure UnregisterCompress(ACompress : TRALCompressClass);
  function GetCompressClass(ACompressType : TRALCompressType) : TRALCompressClass;
  procedure GetCompressList(AList : TStrings);
  function GetSuportedCompress : TRALCompressTypes;
  function GetAcceptCompress : StringRAL;

implementation

const
  CompressWeight : array[TRALCompressType] of integer = (0, 1, 2, 3, 5, 4);
  CompressNames : array[TRALCompressType] of StringRAL = ('', 'deflate', 'zlib', 'gzip', 'zstd', 'br');

var
  CompressDefs : TStringList;

procedure CheckCompressDefs;
begin
  if CompressDefs = nil then
  begin
    CompressDefs := TStringList.Create;
    CompressDefs.Sorted := True;
  end;
end;

procedure DoneCompressDefs;
begin
  FreeAndNil(CompressDefs);
end;

procedure RegisterCompress(ACompress: TRALCompressClass);
var
  vTypes: TRALCompressTypes;
  vType: TRALCompressType;
  vStrType: StringRAL;
begin
  CheckCompressDefs;
  vTypes := ACompress.CompressTypes;
  for vType := Low(TRALCompressType) to High(TRALCompressType) do begin
    vStrType := GetEnumName(TypeInfo(TRALCompressType), Ord(vType));
    if (vType in vTypes) and (CompressDefs.IndexOfName(vStrType) < 0) then
      CompressDefs.Add(vStrType + '=' + ACompress.ClassName);
  end;
end;

procedure UnregisterCompress(ACompress: TRALCompressClass);
var
  vTypes: TRALCompressTypes;
  vType: TRALCompressType;
  vStrType: StringRAL;
  vPos : IntegerRAL;
begin
  CheckCompressDefs;
  vTypes := ACompress.CompressTypes;
  for vType := Low(TRALCompressType) to High(TRALCompressType) do begin
    if vType in vTypes then
    begin
      vStrType := GetEnumName(TypeInfo(TRALCompressType), Ord(vType));
      vPos := CompressDefs.IndexOfName(vStrType);
      if vPos >= 0 then
        CompressDefs.Delete(vPos);
    end;
  end;
end;

function GetCompressClass(ACompressType: TRALCompressType): TRALCompressClass;
var
  vPos : IntegerRAL;
  vStrType : StringRAL;
begin
  Result := nil;
  CheckCompressDefs;
  vStrType := GetEnumName(TypeInfo(TRALCompressType), Ord(ACompressType));
  vPos := CompressDefs.IndexOfName(vStrType);
  if vPos >= 0 then
    Result := TRALCompressClass(GetClass(CompressDefs.ValueFromIndex[vPos]));
end;

procedure GetCompressList(AList: TStrings);
var
  vInt : IntegerRAL;
  vStrType : StringRAL;
  vList : TStringList;
begin
  CheckCompressDefs;

  vList := TStringList.Create;
  try
    for vInt := 0 to Pred(CompressDefs.Count) do
      vList.Add(CompressDefs.Names[vInt]);

    vList.Sort;
    AList.Assign(vList);
  finally
    FreeAndNil(vList);
  end;

  vStrType := GetEnumName(TypeInfo(TRALCompressType), Ord(ctNone));
  AList.Insert(0, vStrType);
end;

function GetSuportedCompress: TRALCompressTypes;
var
  vInt: IntegerRAL;
  vClass: TRALCompressClass;
begin
  Result := [];
  for vInt := 0 to Pred(CompressDefs.Count) do
  begin
    vClass := TRALCompressClass(GetClass(CompressDefs.ValueFromIndex[vInt]));
    Result := Result + vClass.CompressTypes;
  end;
end;

function GetAcceptCompress: StringRAL;
var
  vTypes : TRALCompressTypes;
  vType: TRALCompressType;
  vStrTypes: StringRAL;
begin
  vTypes := GetSuportedCompress;

  vStrTypes := '';
  for vType := Low(TRALCompressType) to High(TRALCompressType) do
  begin
    if (vType in vTypes) and (CompressNames[vType] <> '') then
    begin
      if vStrTypes <> '' then
        vStrTypes := vStrTypes + ', ';
      vStrTypes := vStrTypes + CompressNames[vType];
    end;
  end;
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
  vOutStream: TRALBufFileStream;
begin
  vInStream := TFileStream.Create(AInFile, fmOpenRead or fmShareDenyWrite);
  vOutStream := TRALBufFileStream.Create(AOutFile, fmCreate);
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
  if AStream.Size > 0 then
    InitDeCompress(AStream, Result);
end;

procedure TRALCompress.DecompressFile(AInFile, AOutFile: StringRAL);
var
  vInStream: TFileStream;
  vOutStream: TRALBufFileStream;
begin
  vInStream := TFileStream.Create(AInFile, fmOpenReadWrite);
  vOutStream := TRALBufFileStream.Create(AOutFile, fmCreate);
  try
    vInStream.Position := 0;
    InitDeCompress(vInStream, vOutStream);
  finally
    FreeAndNil(vInStream);
    FreeAndNil(vOutStream);
  end;
end;

class function TRALCompress.CompressToString(ACompress: TRALCompressType): StringRAL;
begin
  Result := CompressNames[ACompress];
end;

class function TRALCompress.StringToCompress(const AStr: StringRAL): TRALCompressType;
begin
  if SameText(AStr, 'gzip') then
    Result := ctGZip
  else if SameText(AStr, 'zlib') then
    Result := ctZLib
  else if SameText(AStr, 'deflate') then
    Result := ctDeflate
  else if SameText(AStr, 'zstd') then
    Result := ctZStd
  else if SameText(AStr, 'br') then
    Result := ctBrotli
  else
    Result := ctNone;
end;

class function TRALCompress.GetBestCompress(const AEncoding: StringRAL): TRALCompressType;
var
  vInt: IntegerRAL;
  vClass: TRALCompressClass;
  vList: TStringList;
  vTypes: TRALCompressTypes;
  vType: TRALCompressType;
  vMax: integer;
begin
  Result := ctNone;

  vTypes := [];
  vList := TStringList.Create;
  try
    vList.LineBreak := ',';
    vList.Text := AEncoding;
    for vInt := 0 to Pred(vList.Count) do
      vTypes := vTypes + [StringToCompress(Trim(vList.Strings[vInt]))];
  finally
    FreeAndNil(vList);
  end;

  vMax := -1;
  for vInt := 0 to Pred(CompressDefs.Count) do
  begin
    vClass := TRALCompressClass(GetClass(CompressDefs.ValueFromIndex[vInt]));
    vType := vClass.BestCompressFromClass(vTypes);
    if CompressWeight[vType] > vMax then
    begin
      vMax := CompressWeight[vType];
      Result := vType;
    end;
  end;
end;

class function TRALCompress.BestCompressFromClass(ATypes: TRALCompressTypes): TRALCompressType;
begin
  Result := ctNone;
end;

initialization

finalization
  DoneCompressDefs;

end.
