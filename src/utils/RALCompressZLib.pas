unit RALCompressZLib;

interface

uses
  {$IFDEF FPC}
    ZLib, ZStream,
  {$ELSE}
    ZLib,
  {$ENDIF}
  Classes, SysUtils,
  RALTypes, RALConsts, RALStream;

type
  { TRALCompressZLib }

  TRALCompressZLib = class
  protected
    class procedure UpdateCRC32(var ACRC32 : LongWord; AOctet: Byte);
  public
    class function Compress(AStream: TStream; AFormat: TRALCompressType = ctDeflate): TStream; overload;
    class function Compress(const AString: StringRAL; AFormat: TRALCompressType = ctDeflate): StringRAL; overload;
    class function Decompress(AStream: TStream; AFormat: TRALCompressType = ctDeflate): TStream; overload;
    class function Decompress(const AString: StringRAL; AFormat: TRALCompressType = ctDeflate): StringRAL; overload;
  end;

implementation

const
  GZipHeader : array[0..9] of byte = ($1F,$8B,$08,$00,$00,$00,$00,$00,$00,$00);

  Crc_32_Tab : Array[0..255] of LongWord = (
    $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535,
    $9e6495a3, $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd,
    $e7b82d07, $90bf1d91, $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d,
    $6ddde4eb, $f4d4b551, $83d385c7, $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec,
    $14015c4f, $63066cd9, $fa0f3d63, $8d080df5, $3b6e20c8, $4c69105e, $d56041e4,
    $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b, $35b5a8fa, $42b2986c,
    $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59, $26d930ac,
    $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
    $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab,
    $b6662d3d, $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f,
    $9fbfe4a5, $e8b8d433, $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb,
    $086d3d2d, $91646c97, $e6635c01, $6b6b51f4, $1c6c6162, $856530d8, $f262004e,
    $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457, $65b0d9c6, $12b7e950, $8bbeb8ea,
    $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65, $4db26158, $3ab551ce,
    $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb, $4369e96a,
    $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
    $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409,
    $ce61e49f, $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81,
    $b7bd5c3b, $c0ba6cad, $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739,
    $9dd277af, $04db2615, $73dc1683, $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8,
    $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1, $f00f9344, $8708a3d2, $1e01f268,
    $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7, $fed41b76, $89d32be0,
    $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5, $d6d6a3e8,
    $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
    $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef,
    $4669be79, $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703,
    $220216b9, $5505262f, $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7,
    $b5d0cf31, $2cd99e8b, $5bdeae1d, $9b64c2b0, $ec63f226, $756aa39c, $026d930a,
    $9c0906a9, $eb0e363f, $72076785, $05005713, $95bf4a82, $e2b87a14, $7bb12bae,
    $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21, $86d3d2d4, $f1d4e242,
    $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777, $88085ae6,
    $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
    $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d,
    $3e6e77db, $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5,
    $47b2cf7f, $30b5ffe9, $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605,
    $cdd70693, $54de5729, $23d967bf, $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94,
    $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d);

{ TRALCompressZLib }

class function TRALCompressZLib.Compress(AStream: TStream; AFormat: TRALCompressType): TStream;
var
  vBuf: array[0..4095] of Byte;
  vZip: TCompressionStream;
  vCount, vInt: Integer;
  vCRC32, vSize : LongWord;
begin
  vCRC32 := $FFFFFFFF;

  vSize := AStream.Size;

  Result := TMemoryStream.Create;
  if AFormat = ctGZip then
    Result.Write(GZipHeader[0], Length(GZipHeader));

  {$IFDEF FPC}
    if AFormat = ctZLib then
      vZip := TCompressionStream.Create(clmax, Result)
    else
      vZip := TCompressionStream.Create(clmax, Result, True);
  {$ELSE}
    if AFormat = ctZLib then
      vZip := TCompressionStream.Create(Result, zcMax, 15)
    else
      vZip := TCompressionStream.Create(Result, zcMax, -15);
  {$ENDIF}
  try
    repeat
      vCount := AStream.Read(vBuf[0], Length(vBuf));

      for vInt := 0 to vCount-1 do
        UpdateCRC32(vCRC32, vBuf[vInt]);

      vZip.Write(vBuf[0], vCount);
    until (vCount = 0);
  finally
    FreeAndNil(vZip);
  end;

  vCRC32 := not vCRC32;
  if AFormat = ctGZip then
  begin
    Result.Write(vCRC32, SizeOf(vCRC32));
    Result.Write(vSize, SizeOf(vSize));
  end;

  Result.Position := 0;
end;

class function TRALCompressZLib.Decompress(AStream: TStream; AFormat: TRALCompressType): TStream;
var
  vBuf: array[0..4095] of Byte;
  vZip: TDeCompressionStream;
  vCount, vInt: integer;
  vCRC32, vCRCFile, vFileSize: LongWord;
begin
  vCRC32 := $FFFFFFFF;
  vCRCFile := $FFFFFFFF;

  // conferir o header
  if AFormat = ctGZip then
  begin
    AStream.Position := AStream.Size - (2 * SizeOf(LongWord));
    AStream.Read(vCRCFile, SizeOf(vCRCFile));
    AStream.Read(vFileSize, SizeOf(vFileSize));

    AStream.Size := AStream.Size - (2 * SizeOf(LongWord));
    AStream.Position := 10;
  end;

  Result := TMemoryStream.Create;
  {$IFDEF FPC}
    if AFormat = ctZLib then
      vZip := TDeCompressionStream.Create(AStream)
    else
      vZip := TDeCompressionStream.Create(AStream, True);
  {$ELSE}
    if AFormat = ctZLib then
      vZip := TDeCompressionStream.Create(AStream, 15)
    else
      vZip := TDeCompressionStream.Create(AStream, -15);
  {$ENDIF}
  try
    repeat
      vCount := vZip.Read(vBuf[0], Length(vBuf));

      for vInt := 0 to vCount-1 do
        UpdateCRC32(vCRC32, vBuf[vInt]);

      Result.Write(vBuf[0], vCount);
    until (vCount = 0);
  finally
    FreeAndNil(vZip);
  end;

  Result.Position := 0;

  vCRC32 := not vCRC32;
  if (AFormat = ctGZip) and ((vCRC32 <> vCRCFile) or (vFileSize <> Result.Size)) then
  begin
    Result.Size := 0;
    raise Exception.Create(emContentCheckError);
  end;
end;

class function TRALCompressZLib.Compress(const AString: StringRAL; AFormat: TRALCompressType): StringRAL;
var
  vStr, vRes: TStream;
begin
  vStr := StringToStream(AString);
  try
    vRes := Compress(vStr, AFormat);
    try
      Result := StreamToString(vRes);
    finally
      FreeAndNil(vRes);
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

class function TRALCompressZLib.Decompress(const AString: StringRAL; AFormat: TRALCompressType): StringRAL;
var
  vStr, vRes: TStream;
begin
  vStr := StringToStream(AString);
  try
    vRes := Decompress(vStr, AFormat);
    try
      Result := StreamToString(vRes);
    finally
      FreeAndNil(vRes);
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

class procedure TRALCompressZLib.UpdateCRC32(var ACRC32: LongWord; AOctet: Byte);
begin
  ACRC32 := Crc_32_Tab[Byte(ACRC32 xor LongInt(AOctet))] xor
            ((ACRC32 shr 8) and $00FFFFFF);
end;

end.

