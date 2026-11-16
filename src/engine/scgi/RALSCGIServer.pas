{ SCGI server engine (the protocol of python.ca/scgi, written here, the same
  on Delphi and FPC): nginx (scgi_pass) or Apache (mod_proxy_scgi) hand each
  request to it over TCP, on a connection of its own. }
unit RALSCGIServer;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALGatewayServer;

type
  { The SCGI engine: the CGI variables arrive as a netstring of NUL-separated
    names and values ("70:CONTENT_LENGTH\0...\0,"), the body after it, and the
    answer is a CGI answer ("Status: 200 OK") until the connection closes.
    Port is the TCP port nginx's scgi_pass points at }
  TRALSCGIServer = class(TRALGatewayServer)
  protected
    function ReadEnvironment(AConnection: TRALGatewayConnection; AEnv: TStrings): boolean; override;
    function StatusLine(AStatusCode: IntegerRAL): RawByteString; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TRALSCGIServer }

constructor TRALSCGIServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Port := 4000;
  SetEngine('SCGI');
end;

function TRALSCGIServer.ReadEnvironment(AConnection: TRALGatewayConnection;
  AEnv: TStrings): boolean;
var
  vChar: AnsiChar;
  vDigits, vLen, vPos, vZero, vValueEnd: IntegerRAL;
  vBlock: RawByteString;
begin
  Result := False;
  // the length of the netstring: decimal digits up to ':'
  vLen := 0;
  vDigits := 0;
  repeat
    if not AConnection.ReadExact(vChar, 1) then
      Exit;
    if vChar = ':' then
      Break;
    if not (vChar in ['0'..'9']) then
      Exit;
    vLen := vLen * 10 + Ord(vChar) - Ord('0');
    Inc(vDigits);
    if (vDigits > 9) or (vLen > MaxHeaderSize) then
      Exit;
  until False;
  if (vDigits = 0) or (vLen = 0) then
    Exit;
  SetLength(vBlock, vLen);
  if not AConnection.ReadExact(vBlock[1], vLen) then
    Exit;
  if not AConnection.ReadExact(vChar, 1) or (vChar <> ',') then
    Exit;
  // name NUL value NUL, repeated
  vPos := 1;
  while vPos <= vLen do
  begin
    vZero := vPos;
    while (vZero <= vLen) and (vBlock[vZero] <> #0) do
      Inc(vZero);
    vValueEnd := vZero + 1;
    while (vValueEnd <= vLen) and (vBlock[vValueEnd] <> #0) do
      Inc(vValueEnd);
    if vValueEnd > vLen then
      Exit;
    AEnv.Add(string(Copy(vBlock, vPos, vZero - vPos)) + '=' +
      string(Copy(vBlock, vZero + 1, vValueEnd - vZero - 1)));
    vPos := vValueEnd + 1;
  end;
  // the specification: CONTENT_LENGTH comes first and SCGI=1 is there
  Result := (AEnv.Count > 0) and (AEnv.Names[0] = 'CONTENT_LENGTH') and
    (AEnv.Values['SCGI'] = '1');
end;

function TRALSCGIServer.StatusLine(AStatusCode: IntegerRAL): RawByteString;
begin
  Result := RawByteString('Status: ' + IntToStr(AStatusCode) + ' ' +
    ReasonPhrase(AStatusCode));
end;

end.
