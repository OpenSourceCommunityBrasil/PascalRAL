{ uwsgi server engine: nginx (uwsgi_pass) hands each request to it over TCP, on a
  connection of its own. }
unit RALUwsgiServer;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALGatewayServer;

type
  { The uwsgi engine: nginx (uwsgi_pass) sends each request on its own connection and
    Port is the TCP port it points at. }
  TRALUwsgiServer = class(TRALGatewayServer)
  protected
    function ReadEnvironment(AConnection: TRALGatewayConnection; AEnv: TStrings): boolean; override;
    function StatusLine(AStatusCode: IntegerRAL): RawByteString; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TRALUwsgiServer }

constructor TRALUwsgiServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Port := 3031;
  SetEngine('uwsgi');
end;

function TRALUwsgiServer.ReadEnvironment(AConnection: TRALGatewayConnection;
  AEnv: TStrings): boolean;
var
  vHead: array[0..3] of Byte;
  vSize, vPos, vNameLen, vValueLen: IntegerRAL;
  vBlock: RawByteString;
begin
  Result := False;
  if not AConnection.ReadExact(vHead, 4) then
    Exit;
  // modifier 0: a request with its CGI variables (the only kind served)
  if vHead[0] <> 0 then
    Exit;
  vSize := vHead[1] or (vHead[2] shl 8);
  if (vSize = 0) or (vSize > MaxHeaderSize) then
    Exit;
  SetLength(vBlock, vSize);
  if not AConnection.ReadExact(vBlock[1], vSize) then
    Exit;
  vPos := 1;
  while vPos <= vSize do
  begin
    if vPos + 1 > vSize then
      Exit;
    vNameLen := Ord(vBlock[vPos]) or (Ord(vBlock[vPos + 1]) shl 8);
    Inc(vPos, 2);
    if vPos + vNameLen + 1 > vSize then
      Exit;
    vValueLen := Ord(vBlock[vPos + vNameLen]) or (Ord(vBlock[vPos + vNameLen + 1]) shl 8);
    if vPos + vNameLen + 2 + vValueLen - 1 > vSize then
      Exit;
    AEnv.Add(string(Copy(vBlock, vPos, vNameLen)) + '=' +
      string(Copy(vBlock, vPos + vNameLen + 2, vValueLen)));
    Inc(vPos, vNameLen + 2 + vValueLen);
  end;
  Result := AEnv.Count > 0;
end;

function TRALUwsgiServer.StatusLine(AStatusCode: IntegerRAL): RawByteString;
begin
  Result := RawByteString('HTTP/1.1 ' + IntToStr(AStatusCode) + ' ' +
    ReasonPhrase(AStatusCode));
end;

end.
