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
  /// One entry of an Accept-Encoding / Content-Encoding list: 'gzip;q=0.5'
  /// gives 'gzip' and 0.5. The name comes lowercase; with no q, the quality is 1
  procedure RALSplitCoding(const AToken: StringRAL; out AName: StringRAL;
    out AQuality: Double);
  /// Raises emDecompressLimit when ACurrent passes RALMaxDecompressedSize;
  /// every compressor calls it from its decompression loop
  procedure RALCheckDecompressedSize(ACurrent: Int64);

var
  { Ceiling, in bytes, for what one Decompress may produce. A 1 MB gzip of
    zeros inflates to 1 GB, and without a ceiling that is one request worth
    of memory per attacker. Zero (the default) keeps the old behaviour: no
    limit at all. 512 MB is a sane value for a server }
  RALMaxDecompressedSize: Int64 = 0;

implementation

procedure RALCheckDecompressedSize(ACurrent: Int64);
begin
  if (RALMaxDecompressedSize > 0) and (ACurrent > RALMaxDecompressedSize) then
    raise Exception.Create(emDecompressLimit);
end;

procedure RALSplitCoding(const AToken: StringRAL; out AName: StringRAL;
  out AQuality: Double);
var
  vPos, vCode: integer;
  vParam: StringRAL;
  vValue: Double;
begin
  AQuality := 1;
  vPos := Pos(StringRAL(';'), AToken);
  if vPos = 0 then
  begin
    AName := LowerCase(Trim(AToken));
    Exit;
  end;
  AName := LowerCase(Trim(Copy(AToken, 1, vPos - 1)));
  vParam := LowerCase(StringReplace(Copy(AToken, vPos + 1, MaxInt), ' ', '',
    [rfReplaceAll]));
  // q always uses the dot (RFC 9110 12.4.2), so Val and not the locale
  if Copy(vParam, 1, 2) = 'q=' then
  begin
    Val(string(Copy(vParam, 3, MaxInt)), vValue, vCode);
    if vCode = 0 then
      AQuality := vValue;
  end;
end;

const
  CompressWeight : array[TRALCompressType] of integer = (0, 1, 2, 3, 5, 4);
  CompressNames : array[TRALCompressType] of StringRAL = ('', 'deflate', 'zlib', 'gzip', 'zstd', 'br');

var
  CompressDefs : TStringList;
  { The class of each type, kept at registration time. The lookup used to go
    by NAME and end in GetClass, which takes MonitorEnter on the RTL's global
    class registry - a process-wide lock, taken several times per request
    (GetBestCompress alone takes one per registered compressor).
    RegisterCompress already receives the class; keeping the pointer removes the
    lock, the GetEnumName and the list search from the hot path in one go.
    CompressDefs stays for the by-name listing at design time. }
  CompressClasses : array[TRALCompressType] of TRALCompressClass;

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
    begin
      CompressDefs.Add(vStrType + '=' + ACompress.ClassName);
      CompressClasses[vType] := ACompress;
    end;
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
      if CompressClasses[vType] = ACompress then
        CompressClasses[vType] := nil;
    end;
  end;
end;

function GetCompressClass(ACompressType: TRALCompressType): TRALCompressClass;
begin
  { was GetEnumName + IndexOfName + GetClass, with the global lock; now it is
    an array index }
  Result := CompressClasses[ACompressType];
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
  vType: TRALCompressType;
begin
  { through the class array: the by-name version called GetClass - and the
    RTL's global lock - once per registered entry. And GetClass could answer nil
    when a class was listed without RegisterClass, which made the vClass
    .CompressTypes right after it an access violation }
  Result := [];
  for vType := Low(TRALCompressType) to High(TRALCompressType) do
    if CompressClasses[vType] <> nil then
      Result := Result + [vType];
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

  Result := vStrTypes;
end;

{ TRALCompress }

procedure TRALCompress.SetFormat(AValue: TRALCompressType);
begin
  FFormat := AValue;
end;

function TRALCompress.Compress(AStream: TStream): TStream;
begin
  Result := TMemoryStream.Create;
  if AStream = nil then
    Exit;

  // start from the beginning whatever the caller left behind. DecodeBody
  // and EncodeBody hand over a stream still positioned at the end of a
  // CopyFrom, and only the FPC gzip branch happened to reposition it while
  // reading its own header and trailer - deflate and zlib read nothing and
  // raised "buffer error".
  AStream.Position := 0;
  // a compressor that raises (zstd/brotli without their DLL, for one) must
  // not leave the output stream behind with the exception
  try
    InitCompress(AStream, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
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
  if (AStream = nil) or (AStream.Size = 0) then
    Exit;

  // same reason as Compress: normalise the position instead of trusting it
  AStream.Position := 0;
  try
    InitDeCompress(AStream, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
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
var
  vName: StringRAL;
  vQuality: Double;
begin
  { a header entry may carry parameters - 'gzip;q=1.0' is what many clients
    send - and comparing the whole entry recognised none of them }
  RALSplitCoding(AStr, vName, vQuality);
  if (vName = 'gzip') or (vName = 'x-gzip') then
    Result := ctGZip
  else if vName = 'zlib' then
    Result := ctZLib
  else if vName = 'deflate' then
    Result := ctDeflate
  else if vName = 'zstd' then
    Result := ctZStd
  else if vName = 'br' then
    Result := ctBrotli
  else
    Result := ctNone;
end;

class function TRALCompress.GetBestCompress(const AEncoding: StringRAL): TRALCompressType;
var
  vInt, vIni, vHigh: IntegerRAL;
  vName: StringRAL;
  vQuality: Double;
  vClass: TRALCompressClass;
  vTypes: TRALCompressTypes;
  vType, vRegType: TRALCompressType;
  vMax: integer;
begin
  Result := ctNone;

  { the per-call TStringList is gone: besides the allocation, "Text :=
    AEncoding" and reading it back through Strings[] converted the header from
    UTF-8 to UTF-16 and back on Delphi. This loop splits on the comma straight
    over the StringRAL. An empty run is skipped, which comes to the same thing:
    it stood for ctNone, and ctNone is the neutral element in
    BestCompressFromClass }
  vTypes := [];
  vHigh := RALHighStr(AEncoding);
  vIni := POSINISTR;
  vInt := POSINISTR;
  while vInt <= vHigh + 1 do
  begin
    if (vInt > vHigh) or (AEncoding[vInt] = ',') then
    begin
      if vInt > vIni then
      begin
        // q=0 means "not this one" (RFC 9110 12.5.3)
        RALSplitCoding(Copy(AEncoding, vIni, vInt - vIni), vName, vQuality);
        if vQuality > 0 then
          vTypes := vTypes + [StringToCompress(vName)];
      end;
      vIni := vInt + 1;
    end;
    Inc(vInt);
  end;

  { through the class array, no GetClass: this loop ran on every read of
    AcceptCompress or ContentCompress - several times per request - and each
    turn took the global lock of the RTL's class registry }
  vMax := -1;
  for vRegType := Low(TRALCompressType) to High(TRALCompressType) do
  begin
    vClass := CompressClasses[vRegType];
    if vClass = nil then
      Continue;
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
