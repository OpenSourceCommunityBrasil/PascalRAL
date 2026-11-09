/// Brotli compression (RFC 7932) over the pascal_brotli submodule.
unit RALCompressBrotli;

interface

uses
  Classes, SysUtils, brotlistream, brotlilib,
  RALExceptions, RALErrorCodes, RALCompress, RALTypes, RALConsts;

type

  /// Compression class Brotli for PascalRAL
  TRALCompressBrotli = class(TRALCompress)
  protected
    procedure InitCompress(AInStream, AOutStream: TStream); override;
    procedure InitDeCompress(AInStream, AOutStream: TStream); override;
    procedure SetFormat(AValue: TRALCompressType); override;
  public
    class function BestCompressFromClass(ATypes : TRALCompressTypes) : TRALCompressType; override;
    class function CompressTypes : TRALCompressTypes; override;
  end;

implementation

{ TRALCompressBrotli }

{ The encoder is driven here, one instance for the whole body, and not through
  pascal_brotli's TBrotliCompressionStream, which leaks 1 KB per stream in its
  Flush and builds an encoder per Write. The output is one standard brotli
  stream. }
procedure TRALCompressBrotli.InitCompress(AInStream, AOutStream: TStream);
var
  vIn, vOut: TBytes;
  vState, vNextIn, vNextOut: Pointer;
  vCount, vOperation: Integer;
  vAvailIn, vAvailOut: NativeUInt;
  vFinish: boolean;
begin
  TBrotli.Check;
  // work buffers of a fixed size, as TRALCompressZLib.InitCompress explains
  SetLength(vIn, DEFAULTCOMPRESSBUFFERSIZE);
  SetLength(vOut, DEFAULTCOMPRESSBUFFERSIZE);

  vState := BrotliEncoderCreateInstance(nil, nil, nil);
  if vState = nil then
    raise ERALException.CreateFmt(ecCompressFailed, ['brotli']);
  try
    BrotliEncoderSetParameter(vState, Ord(BROTLI_PARAM_QUALITY), 5);
    BrotliEncoderSetParameter(vState, Ord(BROTLI_PARAM_LGWIN), 22);
    repeat
      vCount := AInStream.Read(vIn[0], Length(vIn));
      vFinish := vCount <= 0;
      if vFinish then
      begin
        vCount := 0;
        vOperation := Ord(BROTLI_OPERATION_FINISH);
      end
      else
        vOperation := Ord(BROTLI_OPERATION_PROCESS);
      vAvailIn := vCount;
      vNextIn := @vIn[0];
      { a piece is done when the encoder took all of it, the end when the
        encoder says it finished; either way only once it holds no output }
      repeat
        vAvailOut := Length(vOut);
        vNextOut := @vOut[0];
        if BrotliEncoderCompressStream(vState, vOperation, vAvailIn, @vNextIn,
             vAvailOut, @vNextOut, nil) <> BROTLI_TRUE then
          raise ERALException.CreateFmt(ecCompressFailed, ['brotli']);
        if vAvailOut < NativeUInt(Length(vOut)) then
          AOutStream.WriteBuffer(vOut[0], Length(vOut) - Integer(vAvailOut));
      until (BrotliEncoderHasMoreOutput(vState) <> BROTLI_TRUE) and
            (((not vFinish) and (vAvailIn = 0)) or
             (vFinish and (BrotliEncoderIsFinished(vState) = BROTLI_TRUE)));
    until vFinish;
  finally
    BrotliEncoderDestroyInstance(vState);
    AOutStream.Position := 0;
  end;
end;

procedure TRALCompressBrotli.InitDeCompress(AInStream, AOutStream: TStream);
var
  vBuf: TBytes;
  vZip : TBrotliDecompressionStream;
  vCount: Integer;
begin
  SetLength(vBuf, DEFAULTCOMPRESSBUFFERSIZE); // see InitCompress

  vZip := TBrotliDecompressionStream.Create(AInStream);
  try
    repeat
      vCount := vZip.Read(vBuf[0], Length(vBuf));
      AOutStream.Write(vBuf[0], vCount);
      RALCheckDecompressedSize(AOutStream.Size);
    until (vCount = 0);
  finally
    FreeAndNil(vZip);
    AOutStream.Position := 0;
  end;
end;

procedure TRALCompressBrotli.SetFormat(AValue: TRALCompressType);
begin
  if AValue = Format then
    Exit;

  if AValue <> ctBrotli then
  begin
    raise ERALException.Create(ecInvalidFormat);
    Exit;
  end;

  inherited;
end;

class function TRALCompressBrotli.CompressTypes: TRALCompressTypes;
begin
  Result := [ctBrotli];
end;

class function TRALCompressBrotli.BestCompressFromClass(ATypes: TRALCompressTypes): TRALCompressType;
begin
  Result:= inherited BestCompressFromClass(ATypes);
  if ctBrotli in ATypes then
    Result := ctBrotli;
end;

initialization
  RegisterClass(TRALCompressBrotli);
  RegisterCompress(TRALCompressBrotli);

finalization
  // see RALCompressZStd
  UnregisterCompress(TRALCompressBrotli);

end.
