/// Lightweight User-Agent reader: browser, operating system and device kind.
unit RALUserAgent;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALTools, RALRequest, RALParams;

type
  /// Kind of device behind a User-Agent.
  TRALDeviceKind = (rdkUnknown, rdkDesktop, rdkMobile, rdkTablet, rdkBot);

  /// What a User-Agent string says about the client.
  TRALUserAgentInfo = record
    /// Browser or tool name, empty when unknown.
    Browser: StringRAL;
    /// Version of the browser as written in the User-Agent.
    BrowserVersion: StringRAL;
    /// Operating system name, empty when unknown.
    OS: StringRAL;
    /// Version of the operating system (Windows NT numbers are translated).
    OSVersion: StringRAL;
    /// Kind of device.
    Device: TRALDeviceKind;
    /// True for crawlers and other robots.
    IsBot: Boolean;
    /// True for phones and tablets.
    IsMobile: Boolean;
  end;

/// Reads AUserAgent; fields it does not tell stay empty.
function RALParseUserAgent(const AUserAgent: StringRAL): TRALUserAgentInfo;
/// Reads the User-Agent header of ARequest (nil gives an empty result).
function RALUserAgentOf(ARequest: TRALRequest): TRALUserAgentInfo;

implementation

type
  /// Name of a robot and the token that identifies it (lower case).
  TBotName = record
    /// Lower case text found in the User-Agent.
    Token: StringRAL;
    /// Display name of the robot.
    Name: StringRAL;
  end;

const
  /// Robots recognized by name.
  cBots: array[0..7] of TBotName = (
    (Token: 'googlebot'; Name: 'Googlebot'),
    (Token: 'bingbot'; Name: 'Bingbot'),
    (Token: 'slurp'; Name: 'Slurp'),
    (Token: 'duckduckbot'; Name: 'DuckDuckBot'),
    (Token: 'baiduspider'; Name: 'Baiduspider'),
    (Token: 'yandexbot'; Name: 'YandexBot'),
    (Token: 'facebookexternalhit'; Name: 'facebookexternalhit'),
    (Token: 'twitterbot'; Name: 'Twitterbot'));

/// Whether ASub (lower case) appears in AText, ASCII case ignored.
function Has(const ASub, AText: StringRAL): Boolean;
begin
  Result := RALPosText(ASub, AText) > 0;
end;

{ The run of digits, dots and underscores right after ATokenLower in AText, with
  the underscores written as dots. Empty when the token is absent. }
function VersionAfter(const ATokenLower, AText: StringRAL): StringRAL;
var
  vPos, vEnd: IntegerRAL;
begin
  Result := '';
  vPos := RALPosText(ATokenLower, AText);
  if vPos = 0 then
    Exit;
  vPos := vPos + Length(ATokenLower);
  vEnd := vPos;
  while (vEnd <= Length(AText)) and
    ((AText[vEnd] in ['0'..'9']) or (AText[vEnd] = '.') or (AText[vEnd] = '_')) do
    Inc(vEnd);
  Result := Copy(AText, vPos, vEnd - vPos);
  for vPos := 1 to Length(Result) do
    if Result[vPos] = '_' then
      Result[vPos] := '.';
end;

/// Fills the browser fields when ATokenLower is in AUA.
function TryBrowser(var AInfo: TRALUserAgentInfo; const AUA, AToken, AName: StringRAL;
  const AVersionToken: StringRAL): Boolean;
begin
  Result := Has(AToken, AUA);
  if Result then
  begin
    AInfo.Browser := AName;
    AInfo.BrowserVersion := VersionAfter(AVersionToken, AUA);
  end;
end;

/// Maps the Windows NT number to the marketing version.
function WindowsVersion(const ANT: StringRAL): StringRAL;
begin
  if ANT = '10.0' then
    Result := '10'
  else if ANT = '6.3' then
    Result := '8.1'
  else if ANT = '6.2' then
    Result := '8'
  else if ANT = '6.1' then
    Result := '7'
  else if ANT = '6.0' then
    Result := 'Vista'
  else if (ANT = '5.1') or (ANT = '5.2') then
    Result := 'XP'
  else
    Result := ANT;
end;

/// Finds the browser, tool or robot.
procedure ReadBrowser(var AInfo: TRALUserAgentInfo; const AUA: StringRAL);
var
  vInt: Integer;
begin
  for vInt := Low(cBots) to High(cBots) do
    if TryBrowser(AInfo, AUA, cBots[vInt].Token, cBots[vInt].Name,
      cBots[vInt].Token + '/') then
    begin
      AInfo.IsBot := True;
      Exit;
    end;
  if Has('crawler', AUA) or Has('spider', AUA) or Has('bot', AUA) then
  begin
    AInfo.Browser := 'Bot';
    AInfo.IsBot := True;
    Exit;
  end;
  if TryBrowser(AInfo, AUA, 'curl/', 'curl', 'curl/') or
    TryBrowser(AInfo, AUA, 'wget/', 'Wget', 'wget/') or
    TryBrowser(AInfo, AUA, 'postmanruntime/', 'PostmanRuntime', 'postmanruntime/') or
    TryBrowser(AInfo, AUA, 'okhttp/', 'okhttp', 'okhttp/') or
    TryBrowser(AInfo, AUA, 'edg/', 'Edge', 'edg/') or
    TryBrowser(AInfo, AUA, 'edga/', 'Edge', 'edga/') or
    TryBrowser(AInfo, AUA, 'edgios/', 'Edge', 'edgios/') or
    TryBrowser(AInfo, AUA, 'edge/', 'Edge', 'edge/') or
    TryBrowser(AInfo, AUA, 'opr/', 'Opera', 'opr/') or
    TryBrowser(AInfo, AUA, 'samsungbrowser/', 'Samsung Internet', 'samsungbrowser/') or
    TryBrowser(AInfo, AUA, 'firefox/', 'Firefox', 'firefox/') or
    TryBrowser(AInfo, AUA, 'fxios/', 'Firefox', 'fxios/') or
    TryBrowser(AInfo, AUA, 'chrome/', 'Chrome', 'chrome/') or
    TryBrowser(AInfo, AUA, 'crios/', 'Chrome', 'crios/') or
    TryBrowser(AInfo, AUA, 'opera', 'Opera', 'version/') or
    TryBrowser(AInfo, AUA, 'msie ', 'Internet Explorer', 'msie ') or
    TryBrowser(AInfo, AUA, 'trident/', 'Internet Explorer', 'rv:') then
    Exit;
  if Has('safari/', AUA) and Has('version/', AUA) then
    TryBrowser(AInfo, AUA, 'safari/', 'Safari', 'version/');
end;

/// Finds the operating system.
procedure ReadOS(var AInfo: TRALUserAgentInfo; const AUA: StringRAL);
begin
  if Has('windows phone', AUA) then
  begin
    AInfo.OS := 'Windows Phone';
    AInfo.OSVersion := VersionAfter('windows phone ', AUA);
  end
  else if Has('windows nt ', AUA) then
  begin
    AInfo.OS := 'Windows';
    AInfo.OSVersion := WindowsVersion(VersionAfter('windows nt ', AUA));
  end
  else if Has('iphone os ', AUA) or Has('cpu os ', AUA) or Has('ipad', AUA) or
    Has('iphone', AUA) or Has('ipod', AUA) then
  begin
    AInfo.OS := 'iOS';
    AInfo.OSVersion := VersionAfter('iphone os ', AUA);
    if AInfo.OSVersion = '' then
      AInfo.OSVersion := VersionAfter('cpu os ', AUA);
  end
  else if Has('android', AUA) then
  begin
    AInfo.OS := 'Android';
    AInfo.OSVersion := VersionAfter('android ', AUA);
  end
  else if Has('cros ', AUA) then
    AInfo.OS := 'Chrome OS'
  else if Has('mac os x', AUA) then
  begin
    AInfo.OS := 'macOS';
    AInfo.OSVersion := VersionAfter('mac os x ', AUA);
  end
  else if Has('linux', AUA) or Has('x11', AUA) then
    AInfo.OS := 'Linux';
end;

function RALParseUserAgent(const AUserAgent: StringRAL): TRALUserAgentInfo;
var
  vAndroid, vTablet, vMobile: Boolean;
begin
  Result.Browser := '';
  Result.BrowserVersion := '';
  Result.OS := '';
  Result.OSVersion := '';
  Result.Device := rdkUnknown;
  Result.IsBot := False;
  Result.IsMobile := False;
  if AUserAgent = '' then
    Exit;
  ReadBrowser(Result, AUserAgent);
  ReadOS(Result, AUserAgent);
  if Result.IsBot then
  begin
    Result.Device := rdkBot;
    Exit;
  end;
  vAndroid := Result.OS = 'Android';
  vTablet := Has('ipad', AUserAgent) or (vAndroid and not Has('mobile', AUserAgent)) or
    (Has('tablet', AUserAgent) and (Result.OS <> 'Windows'));
  vMobile := Has('mobi', AUserAgent) or Has('iphone', AUserAgent) or
    Has('ipod', AUserAgent) or (Result.OS = 'Windows Phone');
  if vTablet then
    Result.Device := rdkTablet
  else if vMobile then
    Result.Device := rdkMobile
  else if Result.OS <> '' then
    Result.Device := rdkDesktop;
  Result.IsMobile := Result.Device in [rdkMobile, rdkTablet];
end;

function RALUserAgentOf(ARequest: TRALRequest): TRALUserAgentInfo;
var
  vParam: TRALParam;
begin
  vParam := nil;
  if ARequest <> nil then
    vParam := ARequest.Params.GetKind['User-Agent', rpkHEADER];
  if vParam = nil then
    Result := RALParseUserAgent('')
  else
    Result := RALParseUserAgent(vParam.AsString);
end;

end.
