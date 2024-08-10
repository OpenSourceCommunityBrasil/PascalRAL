unit tar_gzip;

interface

uses
  Classes, SysUtils, gzio;

type
  TTGZHeader = record
    name: array [0..99] of AnsiChar;
    mode: array[0..7] of AnsiChar;
    uid: array[0..7] of AnsiChar;
    gid: array[0..7] of AnsiChar;
    size: array[0..11] of AnsiChar;
    mtime: array[0..11] of AnsiChar;
    chksum: array[0..7] of AnsiChar;
    typeflag: AnsiChar;
    linkname: array[0..99] of AnsiChar;
    magic: array[0..5] of AnsiChar;
    version: array[0..1] of AnsiChar;
    uname: array[0..31] of AnsiChar;
    gname: array[0..31] of AnsiChar;
    devmajor: array[0..7] of AnsiChar;
    devminor: array[0..7] of AnsiChar;
    prefix: array[0..154] of AnsiChar;
    padding: array[0..11] of AnsiChar;
  end;

  { TTGZDecompress }

  TTGZDecompress = class
  private
    FOutputFolder : string;
    FFileName : string;
  protected
    function OctToInt(AValue: string): LongInt;
  public
    constructor Create(AFileName : TFileName);
    procedure ExtractAll;
  published
    property OutputFolder : string read FOutputFolder write FOutputFolder;
    property FileName : string read FFileName write FFileName;
  end;

implementation

{ TTGZDecompress }

function TTGZDecompress.OctToInt(AValue: string): LongInt;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(AValue) do
    Result := Result * 8 + StrToInt(System.Copy(AValue, i, 1));
end;

constructor TTGZDecompress.Create(AFileName: TFileName);
begin
  inherited Create;
  FFileName := AFileName;
end;

procedure TTGZDecompress.ExtractAll;
var
  vGZip: gzFile;
  vLastRead: Integer;
  vHeader : TTGZHeader;
  vUseLongName: Boolean;
  vOutFile: TFileStream;
  vLongName : string;
  vFileSize : Integer;
  vBuf: array [0..511] of Byte;
  vLongNamePos : PChar;
begin
  vGZip := gzopen(FFileName, 'rb');

  vUseLongName := False;
  vLongName := '';

  FOutputFolder := IncludeTrailingPathDelimiter(FOutputFolder);
  ForceDirectories(FOutputFolder);

  try
    repeat
      vLastRead := gzread(vGZip, @vHeader, SizeOf(TTGZHeader));
      if vLastRead > 0 then
      begin
        if vHeader.typeflag = '0' then
        begin
          if vUseLongName then
            vOutFile := TFileStream.Create(FOutputFolder + vLongName, fmCreate)
          else
            vOutFile := TFileStream.Create(FOutputFolder + vHeader.name, fmCreate);
          try
            vFileSize := OctToInt(vHeader.size);

            while vFileSize > 0 do
            begin
              vLastRead := gzread(vGZip, @vBuf, 512);
              if vFileSize > vLastRead then
                vOutFile.Write(vBuf, vLastRead)
              else
                vOutFile.Write(vBuf, vFileSize);
              Dec(vFileSize, vLastRead);
            end;
          finally
            vOutFile.Free;
          end;
          {$IFDEF UNIX}
            if vUseLongName then
              FpChmod(FOutputFolder + vLongName, OctToInt(vHeader.mode))
            else
              FpChmod(FOutputFolder + Headder.name, OctToInt(vHeader.mode));
          {$ENDIF}
          vUseLongName := False;
        end
        else if vHeader.typeflag = '5' then
        begin
          if vUseLongName then
            ForceDirectories(FOutputFolder + vLongName)
          else
            ForceDirectories(FOutputFolder + vHeader.name);
          vUseLongName := False;
        end
        else if vHeader.typeflag = '2' then
        begin
          {$IFDEF UNIX}
            FpLink(vHeader.linkname, FOutputFolder + vHeader.name)
          {$ENDIF}
        end
        else if vHeader.typeflag = 'L' then
        begin
          vFileSize := OctToInt(vHeader.size);
          SetLength(vLongName, vFileSize);
          vLongNamePos := PChar(@vLongName[1]);

          while vFileSize > 0 do
          begin
            vLastRead := gzread(vGZip, @vBuf, 512);
            if vFileSize > vLastRead then
            begin
              Move(vBuf, vLongNamePos^, vLastRead);
              Inc(vLongNamePos, vLastRead);
            end
            else
            begin
              Move(vBuf, vLongNamePos^, vFileSize);
              Inc(vLongNamePos, vFileSize);
            end;

            Dec(vFileSize, vLastRead);
          end;
          vUseLongName := True;
        end
        else if vHeader.typeflag <> #0 then
          raise Exception.Create(vHeader.name + ': ' + vHeader.typeflag);
      end;
    until vLastRead < SizeOf(TTGZHeader);
  finally
    gzclose(vGZip);
  end;
end;

end.

