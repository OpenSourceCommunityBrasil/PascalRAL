/// ClamAV scanner for uploads: talks to clamd over TCP with RAL's own sockets.
unit RALClamAV;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALErrorCodes, RALSocket, RALUploadScan;

type
  { Scans streams with a clamd daemon (the INSTREAM command). One connection per
    call, as clamd closes it after answering, so it is thread-safe. }
  TRALClamAVScanner = class(TRALUploadScanner)
  private
    FChunkSize: IntegerRAL;
    FHost: StringRAL;
    FPort: IntegerRAL;
    FTimeout: IntegerRAL;

    /// Sends ACommand and returns the answer.
    function Command(const ACommand: RawByteString): RawByteString;
    /// Opens a connection to clamd.
    function Open: TRALSocketChannel;
    /// Reads the NUL terminated answer; empty when clamd closed without one.
    function ReadAnswer(AChannel: TRALSocketChannel): RawByteString;
  public
    constructor Create(AOwner: TComponent); override;

    /// True when clamd answers PONG.
    function Ping: boolean;
    function Scan(AStream: TStream; const AFileName: StringRAL;
      out AThreat: StringRAL): TRALUploadVerdict; override;
    /// Version text of the daemon; raises when clamd cannot be reached.
    function Version: StringRAL;
  published
    /// Bytes sent per INSTREAM chunk.
    property ChunkSize: IntegerRAL read FChunkSize write FChunkSize default 65536;
    /// Address of the clamd daemon.
    property Host: StringRAL read FHost write FHost;
    /// TCP port of clamd.
    property Port: IntegerRAL read FPort write FPort default 3310;
    /// Milliseconds to connect, and to wait for each step of the exchange.
    property Timeout: IntegerRAL read FTimeout write FTimeout default 30000;
  end;

implementation

const
  /// The INSTREAM command, NUL terminated.
  cInStream: RawByteString = 'zINSTREAM'#0;
  /// End of a clamd line.
  cNul: RawByteString = #0;
  /// Separates the stream name from the verdict.
  cSep: RawByteString = ': ';
  /// Largest answer line read.
  cMaxAnswer = 4096;

{ TRALClamAVScanner }

constructor TRALClamAVScanner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChunkSize := 65536;
  FHost := '127.0.0.1';
  FPort := 3310;
  FTimeout := 30000;
end;

function TRALClamAVScanner.Open: TRALSocketChannel;
var
  vSocket: TRALSocket;
begin
  vSocket := TRALSocket.Create;
  try
    vSocket.Connect(FHost, FPort, FTimeout);
  except
    vSocket.Free;
    raise;
  end;
  Result := TRALSocketChannel.Create(vSocket, True);
end;

function TRALClamAVScanner.ReadAnswer(AChannel: TRALSocketChannel): RawByteString;
var
  vBuf: array[0..511] of AnsiChar;
  vRes, vPos: IntegerRAL;
  vEnd: Int64RAL;
begin
  Result := '';
  vEnd := RALSocketNow + FTimeout;
  while Length(Result) < cMaxAnswer do
  begin
    vRes := AChannel.ReadWait(vBuf, SizeOf(vBuf), 100);
    if vRes = RALSocketWouldBlock then
    begin
      if RALSocketNow >= vEnd then
        raise ERALSocketError.CreateCode(0, 'clamd did not answer in time');
      Continue;
    end;
    if vRes <= 0 then
      Break;
    SetLength(Result, Length(Result) + vRes);
    Move(vBuf, Result[Length(Result) - vRes + 1], vRes);
    vPos := Pos(cNul, Result);
    if vPos > 0 then
    begin
      SetLength(Result, vPos - 1);
      Break;
    end;
  end;
  while (Result <> '') and (Result[Length(Result)] in [#10, #13, #0]) do
    SetLength(Result, Length(Result) - 1);
end;

function TRALClamAVScanner.Command(const ACommand: RawByteString): RawByteString;
var
  vChannel: TRALSocketChannel;
begin
  vChannel := Open;
  try
    vChannel.WriteAll(ACommand[1], Length(ACommand), FTimeout);
    Result := ReadAnswer(vChannel);
  finally
    vChannel.Free;
  end;
end;

function TRALClamAVScanner.Ping: boolean;
begin
  try
    Result := Command('zPING'#0) = 'PONG';
  except
    Result := False;
  end;
end;

function TRALClamAVScanner.Version: StringRAL;
begin
  Result := StringRAL(Command('zVERSION'#0));
end;

function TRALClamAVScanner.Scan(AStream: TStream; const AFileName: StringRAL;
  out AThreat: StringRAL): TRALUploadVerdict;
var
  vChannel: TRALSocketChannel;
  vBuf: RawByteString;
  vLen: array[0..3] of Byte;
  vChunk, vRead: IntegerRAL;
  vAnswer: RawByteString;
  vSendError: string;
  vPos: IntegerRAL;
begin
  Result := uvError;
  AThreat := '';
  vSendError := '';
  vChunk := FChunkSize;
  if vChunk < 1 then
    vChunk := 65536;
  try
    vChannel := Open;
    try
      try
        vChannel.WriteAll(cInStream[1], Length(cInStream), FTimeout);
        SetLength(vBuf, vChunk);
        repeat
          vRead := AStream.Read(vBuf[1], vChunk);
          if vRead > 0 then
          begin
            vLen[0] := Byte(vRead shr 24);
            vLen[1] := Byte(vRead shr 16);
            vLen[2] := Byte(vRead shr 8);
            vLen[3] := Byte(vRead);
            vChannel.WriteAll(vLen, 4, FTimeout);
            vChannel.WriteAll(vBuf[1], vRead, FTimeout);
          end;
        until vRead < vChunk;
        FillChar(vLen, SizeOf(vLen), 0);
        vChannel.WriteAll(vLen, 4, FTimeout);
      except
        on E: Exception do
          // clamd may answer (and close) before the end, as past StreamMaxLength
          vSendError := E.Message;
      end;
      vAnswer := '';
      try
        vAnswer := ReadAnswer(vChannel);
      except
        on E: Exception do
          if vSendError = '' then
            raise;
      end;
      if (vAnswer = '') and (vSendError <> '') then
      begin
        AThreat := StringRAL(vSendError);
        Exit;
      end;
    finally
      vChannel.Free;
    end;
  except
    on E: Exception do
    begin
      AThreat := StringRAL(E.Message);
      Result := uvError;
      Exit;
    end;
  end;

  if vAnswer = 'stream: OK' then
    Result := uvClean
  else if (Length(vAnswer) > 6) and (Copy(vAnswer, Length(vAnswer) - 5, 6) = ' FOUND') then
  begin
    vAnswer := Copy(vAnswer, 1, Length(vAnswer) - 6);
    vPos := Pos(cSep, vAnswer);
    if vPos > 0 then
      vAnswer := Copy(vAnswer, vPos + 2, MaxInt);
    AThreat := StringRAL(vAnswer);
    Result := uvInfected;
  end
  else if (Length(vAnswer) >= 5) and (Copy(vAnswer, Length(vAnswer) - 4, 5) = 'ERROR') then
    AThreat := StringRAL(vAnswer)
  else
    AThreat := StringRAL(Format(RALGetErrorMessage(ecClamAVAnswer), [string(vAnswer)]));
end;

end.
