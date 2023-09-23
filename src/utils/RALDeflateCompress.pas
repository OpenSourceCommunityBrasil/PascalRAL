unit RALDeflateCompress;

interface

uses
  {$IFDEF FPC}
    ZLib, ZStream,
  {$ELSE}
    ZLib,
  {$ENDIF}
  Classes, SysUtils,
  RALTypes;

type

  { TRALDeflateCompress }

  TRALDeflateCompress = class
  public
    class function Compress(AStream : TStream) : TStream; overload;
    class function Compress(AString : StringRAL) : StringRAL; overload;
    class function Decompress(AStream : TStream) : TStream; overload;
    class function Decompress(AString : StringRAL) : StringRAL; overload;
  end;

implementation

{ TRALDeflateCompress }

class function TRALDeflateCompress.Compress(AStream: TStream): TStream;
var
  vBuf: PByte;
  vZip: TCompressionStream;
  vCount, vNewCount: Integer;
begin
  {$IFDEF FPC}
    vBuf := GetMem(4096);
  {$ELSE}
    GetMem(vBuf, 4096);
  {$ENDIF}

  Result := TStringStream.Create;
  try
    {$IFDEF FPC}
      vZip := TCompressionStream.Create(clmax, Result, True);
    {$ELSE}
      vZip := TCompressionStream.Create(Result, zcMax, -15);
    {$ENDIF}
    try
      repeat
        vCount := AStream.Read(vBuf^, 4096);
        vNewCount := vCount;
        while (vNewCount > 0) do
          vNewCount := vNewCount - vZip.Write(vBuf^, vNewCount);
      until (vCount = 0);
    finally
      FreeAndNil(vZip);
    end;
  finally
    FreeMem(vBuf);
  end;
  Result.Position := 0;
end;

class function TRALDeflateCompress.Decompress(AStream: TStream): TStream;
var
  vBuf: PByte;
  vZip: TDeCompressionStream;
  vCount: integer;
begin
  {$IFDEF FPC}
    vBuf := GetMem(4096);
  {$ELSE}
    GetMem(vBuf, 4096);
  {$ENDIF}

  Result := TStringStream.Create;
  try
    {$IFDEF FPC}
      vZip := TDeCompressionStream.Create(AStream, True);
    {$ELSE}
      vZip := TDeCompressionStream.Create(AStream, -15);
    {$ENDIF}
    try
      repeat
        vCount := vZip.Read(vBuf^, 4096);
        Result.Write(vBuf^, vCount);
      until (vCount = 0);
    finally
      FreeAndNil(vZip);
    end;
  finally
    FreeMem(vBuf);
  end;
  Result.Position := 0;
end;

class function TRALDeflateCompress.Compress(AString: StringRAL): StringRAL;
var
  vStr, vRes : TStringStream;
begin
  vStr := TStringStream.Create(AString);
  try
    vRes := TStringStream(Compress(vStr));
    try
      Result := vRes.DataString;
    finally
      FreeAndNil(vRes);
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

class function TRALDeflateCompress.Decompress(AString: StringRAL): StringRAL;
var
  vStr, vRes : TStringStream;
begin
  vStr := TStringStream.Create(AString);
  try
    vRes := TStringStream(Decompress(vStr));
    try
      Result := vRes.DataString;
    finally
      FreeAndNil(vRes);
    end;
  finally
    FreeAndNil(vStr);
  end;
end;

end.

