/// The tunnels an MQTT client can go through to reach its broker: an HTTP
/// proxy (CONNECT, RFC 9110 9.3.6) and SOCKS5 (RFC 1928, with user name and
/// password by RFC 1929). The tunnel is opened on the plain socket, before TLS
/// or WebSocket.
unit RALMQTTProxy;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALMQTTTypes, RALMQTTSocket;

type
  /// Kind of proxy.
  TRALMQTTProxyKind = (
    /// No proxy: the socket goes straight to the broker.
    mpkNoProxy,
    /// An HTTP proxy, by CONNECT.
    mpkHTTP,
    /// A SOCKS5 proxy.
    mpkSOCKS5);

  /// Proxy settings of an MQTT client.
  TRALMQTTProxy = class(TPersistent)
  private
    FHost: StringRAL;
    FKind: TRALMQTTProxyKind;
    FPassword: StringRAL;
    FPort: IntegerRAL;
    FUserName: StringRAL;
  public
    procedure Assign(Source: TPersistent); override;
    { Asks the proxy, over AChannel (a socket already connected to it), for a
      tunnel to AHost:APort, within ATimeout ms; raises when it refuses }
    procedure OpenTunnel(AChannel: TRALMQTTChannel; const AHost: StringRAL;
      APort, ATimeout: IntegerRAL);
  published
    /// Address of the proxy.
    property Host: StringRAL read FHost write FHost;
    /// Kind of proxy; mpkNoProxy turns it off.
    property Kind: TRALMQTTProxyKind read FKind write FKind default mpkNoProxy;
    /// Password, when the proxy asks for one.
    property Password: StringRAL read FPassword write FPassword;
    /// Port of the proxy.
    property Port: IntegerRAL read FPort write FPort default 0;
    /// User name, when the proxy asks for one.
    property UserName: StringRAL read FUserName write FUserName;
  end;

implementation

uses
  RALBase64;

procedure ProxyError(const AReason: string);
begin
  raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError,
    Format(emMQTTProxyRefused, [AReason]));
end;

{ reads exactly ACount bytes before the deadline }
procedure ReadExact(AChannel: TRALMQTTChannel; var ABuffer; ACount: IntegerRAL;
  ADeadline: Int64RAL);
var
  vData: PByte;
  vDone, vRes, vWait: IntegerRAL;
begin
  vData := @ABuffer;
  vDone := 0;
  while vDone < ACount do
  begin
    vWait := ADeadline - RALMQTTNow;
    if vWait <= 0 then
      ProxyError(emMQTTProxyTimeout);
    vRes := AChannel.ReadWait(vData[vDone], ACount - vDone, vWait);
    if vRes = 0 then
      ProxyError(emMQTTConnectionClosed);
    if vRes > 0 then
      Inc(vDone, vRes);
  end;
end;

{ TRALMQTTProxy }

procedure TRALMQTTProxy.Assign(Source: TPersistent);
begin
  if Source is TRALMQTTProxy then
  begin
    FHost := TRALMQTTProxy(Source).Host;
    FKind := TRALMQTTProxy(Source).Kind;
    FPassword := TRALMQTTProxy(Source).Password;
    FPort := TRALMQTTProxy(Source).Port;
    FUserName := TRALMQTTProxy(Source).UserName;
  end
  else
    inherited Assign(Source);
end;

procedure TRALMQTTProxy.OpenTunnel(AChannel: TRALMQTTChannel; const AHost: StringRAL;
  APort, ATimeout: IntegerRAL);
var
  vDeadline: Int64RAL;
  vRequest, vTarget: StringRAL;
  vLine: AnsiString;
  vByte: Byte;
  vHead: array[0..3] of Byte;
  vBuf: TBytes;
  vLen, vPos: IntegerRAL;
  vHost: AnsiString;
  vUser, vPass: AnsiString;
begin
  vDeadline := RALMQTTNow + ATimeout;
  case FKind of
    mpkHTTP:
    begin
      vTarget := AHost;
      if Pos(StringRAL(':'), vTarget) > 0 then
        vTarget := '[' + vTarget + ']';
      vTarget := vTarget + ':' + StringRAL(IntToStr(APort));
      vRequest := 'CONNECT ' + vTarget + ' HTTP/1.1'#13#10 + 'Host: ' + vTarget + #13#10;
      if FUserName <> '' then
        vRequest := vRequest + 'Proxy-Authorization: Basic ' +
          TRALBase64.Encode(FUserName + ':' + FPassword) + #13#10;
      vRequest := vRequest + #13#10;
      AChannel.WriteAll(Pointer(vRequest)^, Length(vRequest), ATimeout);
      { the answer is read one byte at a time up to the empty line: nothing of
        what the broker sends after it may be swallowed here }
      vLine := '';
      repeat
        ReadExact(AChannel, vByte, 1, vDeadline);
        vLine := vLine + AnsiChar(vByte);
        if Length(vLine) > 16384 then
          ProxyError('HTTP');
      until (Length(vLine) >= 4) and (Copy(vLine, Length(vLine) - 3, 4) = #13#10#13#10);
      vPos := Pos(AnsiString(#13#10), vLine);
      vLine := Copy(vLine, 1, vPos - 1);
      // HTTP/1.x 2xx
      if (Copy(vLine, 1, 5) <> 'HTTP/') or (Pos(' 2', string(vLine)) = 0) then
        ProxyError(string(vLine));
    end;
    mpkSOCKS5:
    begin
      // greeting: no authentication, or user name and password
      SetLength(vBuf, 4);
      vBuf[0] := 5;
      if FUserName <> '' then
      begin
        vBuf[1] := 2;
        vBuf[2] := 0;
        vBuf[3] := 2;
        vLen := 4;
      end
      else
      begin
        vBuf[1] := 1;
        vBuf[2] := 0;
        vLen := 3;
      end;
      AChannel.WriteAll(vBuf[0], vLen, ATimeout);
      ReadExact(AChannel, vHead, 2, vDeadline);
      if vHead[0] <> 5 then
        ProxyError('SOCKS5');
      if vHead[1] = 2 then
      begin
        vUser := AnsiString(FUserName);
        vPass := AnsiString(FPassword);
        SetLength(vBuf, 3 + Length(vUser) + Length(vPass));
        vBuf[0] := 1;
        vBuf[1] := Length(vUser);
        if vUser <> '' then
          Move(vUser[1], vBuf[2], Length(vUser));
        vBuf[2 + Length(vUser)] := Length(vPass);
        if vPass <> '' then
          Move(vPass[1], vBuf[3 + Length(vUser)], Length(vPass));
        AChannel.WriteAll(vBuf[0], Length(vBuf), ATimeout);
        ReadExact(AChannel, vHead, 2, vDeadline);
        if vHead[1] <> 0 then
          ProxyError(emMQTTProxyAuth);
      end
      else if vHead[1] <> 0 then
        ProxyError(emMQTTProxyAuth);

      // CONNECT by name: the proxy resolves it
      vHost := AnsiString(AHost);
      if (Length(vHost) > 1) and (vHost[1] = '[') then
        vHost := Copy(vHost, 2, Length(vHost) - 2);
      SetLength(vBuf, 7 + Length(vHost));
      vBuf[0] := 5;
      vBuf[1] := 1;
      vBuf[2] := 0;
      vBuf[3] := 3;
      vBuf[4] := Length(vHost);
      Move(vHost[1], vBuf[5], Length(vHost));
      vBuf[5 + Length(vHost)] := Byte(APort shr 8);
      vBuf[6 + Length(vHost)] := Byte(APort);
      AChannel.WriteAll(vBuf[0], Length(vBuf), ATimeout);
      ReadExact(AChannel, vHead, 4, vDeadline);
      if vHead[1] <> 0 then
        ProxyError(Format(emMQTTProxySocksReply, [vHead[1]]));
      // the bound address that follows, whatever its type
      case vHead[3] of
        1: vLen := 4;
        4: vLen := 16;
        3:
        begin
          ReadExact(AChannel, vByte, 1, vDeadline);
          vLen := vByte;
        end;
      else
        ProxyError('SOCKS5');
        vLen := 0;
      end;
      SetLength(vBuf, vLen + 2);
      ReadExact(AChannel, vBuf[0], vLen + 2, vDeadline);
    end;
  end;
end;

end.
