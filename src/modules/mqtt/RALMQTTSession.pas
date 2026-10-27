/// Pieces of an MQTT session shared by the client and the broker: the Packet
/// Identifiers in use, the Topic Alias maps of both directions, and the broker
/// address in URI form.
unit RALMQTTSession;

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
  RALTypes, RALConsts, RALMQTTTypes;

type
  { The Packet Identifiers in use on one side of a session. Next hands out the
    next one not in use, never 0 [MQTT-2.2.1-3]. Not thread-safe }
  TRALMQTTPacketIds = class
  private
    FCount: IntegerRAL;
    FLast: Word;
    FUsed: array[0..8191] of Byte;
  public
    /// Forgets every identifier.
    procedure Clear;
    /// Frees AId.
    procedure Release(AId: Word);
    /// Marks AId as in use (one read back from storage).
    procedure Reserve(AId: Word);
    /// Whether AId is in use.
    function InUse(AId: Word): boolean;
    /// The next identifier not in use; 0 when all 65 535 are.
    function Next: Word;
    /// Identifiers in use.
    property Count: IntegerRAL read FCount;
  end;

  { Topic Aliases a sender uses: up to Maximum topics get a number, the least
    recently used giving its number up when a new topic needs one }
  TRALMQTTAliasOut = class
  private
    FMaximum: IntegerRAL;
    FTopics: array of StringRAL;
    FUsed: array of Int64RAL;
    FTick: Int64RAL;
  public
    /// Forgets every alias and sets the maximum the receiver accepts.
    procedure Reset(AMaximum: IntegerRAL);
    { The alias for ATopic: ANew True when the topic must go with it (first
      use of the number); 0 when aliases are off }
    function AliasFor(const ATopic: StringRAL; out ANew: boolean): Word;
    /// The maximum in force.
    property Maximum: IntegerRAL read FMaximum;
  end;

  /// Topic Aliases a receiver keeps, alias to topic, for one connection [MQTT-3.3.2-7].
  TRALMQTTAliasIn = class
  private
    FMaximum: IntegerRAL;
    FTopics: array of StringRAL;
  public
    /// Forgets every alias; AMaximum is what this side announced.
    procedure Reset(AMaximum: IntegerRAL);
    { Resolves the topic of a PUBLISH: with a name, the alias (if any) is set
      to it; without one, the alias must be known. Raises Topic Alias invalid
      (0x94) or Protocol Error otherwise }
    function Resolve(const ATopic: StringRAL; AAlias: Cardinal; AHasAlias: boolean): StringRAL;
  end;

  /// Transport of a broker URI.
  TRALMQTTURIScheme = (musMQTT, musMQTTS, musWS, musWSS, musMQTTSN);

  /// A broker address: mqtt://, mqtts://, ws://, wss://, mqtt-sn://.
  TRALMQTTURI = record
    /// Scheme.
    Scheme: TRALMQTTURIScheme;
    /// Host, without brackets.
    Host: StringRAL;
    /// Port; the scheme's default when the URI has none.
    Port: IntegerRAL;
    /// Path (WebSocket); '/mqtt' when none.
    Path: StringRAL;
    /// User name in the URI, if any.
    UserName: StringRAL;
    /// Password in the URI, if any.
    Password: StringRAL;
  end;

{ Reads AText as a broker URI. A text without scheme is 'host[:port]' over TCP.
  False when it cannot be read }
function RALMQTTParseURI(const AText: StringRAL; out AURI: TRALMQTTURI): boolean;
/// AURI as text.
function RALMQTTURIText(const AURI: TRALMQTTURI): StringRAL;

implementation

uses
  RALTools, RALUrlCoder;

{ TRALMQTTPacketIds }

procedure TRALMQTTPacketIds.Clear;
begin
  FillChar(FUsed, SizeOf(FUsed), 0);
  FCount := 0;
end;

function TRALMQTTPacketIds.InUse(AId: Word): boolean;
begin
  Result := (FUsed[AId shr 3] and (1 shl (AId and 7))) <> 0;
end;

function TRALMQTTPacketIds.Next: Word;
var
  vTry: IntegerRAL;
  vId: Word;
begin
  Result := 0;
  if FCount >= 65535 then
    Exit;
  vId := FLast;
  for vTry := 1 to 65535 do
  begin
    if vId = 65535 then
      vId := 1
    else
      Inc(vId);
    if not InUse(vId) then
    begin
      Reserve(vId);
      FLast := vId;
      Result := vId;
      Exit;
    end;
  end;
end;

procedure TRALMQTTPacketIds.Release(AId: Word);
begin
  if (AId <> 0) and InUse(AId) then
  begin
    FUsed[AId shr 3] := FUsed[AId shr 3] and not Byte(1 shl (AId and 7));
    Dec(FCount);
  end;
end;

procedure TRALMQTTPacketIds.Reserve(AId: Word);
begin
  if (AId <> 0) and not InUse(AId) then
  begin
    FUsed[AId shr 3] := FUsed[AId shr 3] or Byte(1 shl (AId and 7));
    Inc(FCount);
  end;
end;

{ TRALMQTTAliasOut }

function TRALMQTTAliasOut.AliasFor(const ATopic: StringRAL; out ANew: boolean): Word;
var
  vInt, vFree, vOldest: IntegerRAL;
begin
  Result := 0;
  ANew := False;
  if FMaximum <= 0 then
    Exit;
  Inc(FTick);
  vFree := -1;
  vOldest := 0;
  for vInt := 0 to FMaximum - 1 do
  begin
    if FTopics[vInt] = ATopic then
    begin
      FUsed[vInt] := FTick;
      Result := vInt + 1;
      Exit;
    end;
    if (FTopics[vInt] = '') and (vFree < 0) then
      vFree := vInt;
    if FUsed[vInt] < FUsed[vOldest] then
      vOldest := vInt;
  end;
  if vFree < 0 then
    vFree := vOldest;
  FTopics[vFree] := ATopic;
  FUsed[vFree] := FTick;
  ANew := True;
  Result := vFree + 1;
end;

procedure TRALMQTTAliasOut.Reset(AMaximum: IntegerRAL);
begin
  if AMaximum < 0 then
    AMaximum := 0;
  FMaximum := AMaximum;
  FTopics := nil;
  FUsed := nil;
  SetLength(FTopics, AMaximum);
  SetLength(FUsed, AMaximum);
  FTick := 0;
end;

{ TRALMQTTAliasIn }

procedure TRALMQTTAliasIn.Reset(AMaximum: IntegerRAL);
begin
  if AMaximum < 0 then
    AMaximum := 0;
  FMaximum := AMaximum;
  FTopics := nil;
  SetLength(FTopics, AMaximum + 1);
end;

function TRALMQTTAliasIn.Resolve(const ATopic: StringRAL; AAlias: Cardinal;
  AHasAlias: boolean): StringRAL;
begin
  if not AHasAlias then
  begin
    if ATopic = '' then
      RALMQTTMalformed(emMQTTTopicNameEmpty, MQTTRC_ProtocolError);
    Result := ATopic;
    Exit;
  end;
  // [MQTT-3.3.2-10], [MQTT-3.3.2-12]: 1 up to what this side announced
  if (AAlias = 0) or (AAlias > Cardinal(FMaximum)) then
    RALMQTTMalformed(Format(emMQTTAliasInvalid, [AAlias]), MQTTRC_TopicAliasInvalid);
  if ATopic <> '' then
  begin
    FTopics[AAlias] := ATopic;
    Result := ATopic;
  end
  else
  begin
    Result := FTopics[AAlias];
    if Result = '' then
      RALMQTTMalformed(Format(emMQTTAliasInvalid, [AAlias]), MQTTRC_ProtocolError);
  end;
end;

{ URI }

function RALMQTTParseURI(const AText: StringRAL; out AURI: TRALMQTTURI): boolean;
var
  vText, vScheme, vAuth, vHostPort: StringRAL;
  vPos, vSlash: IntegerRAL;
begin
  Result := False;
  AURI.Scheme := musMQTT;
  AURI.Host := '';
  AURI.Port := 0;
  AURI.Path := RALMQTTWSPath;
  AURI.UserName := '';
  AURI.Password := '';
  vText := RALTrim(AText);
  if vText = '' then
    Exit;
  vPos := Pos(StringRAL('://'), vText);
  if vPos > 0 then
  begin
    vScheme := StringRAL(LowerCase(string(Copy(vText, 1, vPos - 1))));
    vText := Copy(vText, vPos + 3, MaxInt);
    if (vScheme = 'mqtt') or (vScheme = 'tcp') then
      AURI.Scheme := musMQTT
    else if (vScheme = 'mqtts') or (vScheme = 'ssl') or (vScheme = 'tls') then
      AURI.Scheme := musMQTTS
    else if vScheme = 'ws' then
      AURI.Scheme := musWS
    else if vScheme = 'wss' then
      AURI.Scheme := musWSS
    else if (vScheme = 'mqtt-sn') or (vScheme = 'mqttsn') or (vScheme = 'udp') then
      AURI.Scheme := musMQTTSN
    else
      Exit;
  end;
  vSlash := Pos(StringRAL('/'), vText);
  if vSlash > 0 then
  begin
    if AURI.Scheme in [musWS, musWSS] then
      AURI.Path := Copy(vText, vSlash, MaxInt);
    vText := Copy(vText, 1, vSlash - 1);
  end;
  vPos := Pos(StringRAL('@'), vText);
  if vPos > 0 then
  begin
    vAuth := Copy(vText, 1, vPos - 1);
    vText := Copy(vText, vPos + 1, MaxInt);
    vPos := Pos(StringRAL(':'), vAuth);
    if vPos > 0 then
    begin
      AURI.UserName := TRALHTTPCoder.DecodeURL(Copy(vAuth, 1, vPos - 1));
      AURI.Password := TRALHTTPCoder.DecodeURL(Copy(vAuth, vPos + 1, MaxInt));
    end
    else
      AURI.UserName := TRALHTTPCoder.DecodeURL(vAuth);
  end;
  vHostPort := vText;
  if (vHostPort <> '') and (vHostPort[1] = '[') then
  begin
    vPos := Pos(StringRAL(']'), vHostPort);
    if vPos <= 0 then
      Exit;
    AURI.Host := Copy(vHostPort, 2, vPos - 2);
    vHostPort := Copy(vHostPort, vPos + 1, MaxInt);
    if (vHostPort <> '') and (vHostPort[1] = ':') then
      AURI.Port := StrToIntDef(string(Copy(vHostPort, 2, MaxInt)), -1);
  end
  else
  begin
    vPos := Pos(StringRAL(':'), vHostPort);
    if vPos > 0 then
    begin
      AURI.Host := Copy(vHostPort, 1, vPos - 1);
      AURI.Port := StrToIntDef(string(Copy(vHostPort, vPos + 1, MaxInt)), -1);
    end
    else
      AURI.Host := vHostPort;
  end;
  if (AURI.Host = '') or (AURI.Port < 0) or (AURI.Port > 65535) then
    Exit;
  if AURI.Port = 0 then
    case AURI.Scheme of
      musMQTT: AURI.Port := RALMQTTPort;
      musMQTTS: AURI.Port := RALMQTTTLSPort;
      musWS: AURI.Port := RALMQTTWSPort;
      musWSS: AURI.Port := RALMQTTWSSPort;
      musMQTTSN: AURI.Port := RALMQTTSNPort;
    end;
  Result := True;
end;

function RALMQTTURIText(const AURI: TRALMQTTURI): StringRAL;
const
  cSchemes: array[TRALMQTTURIScheme] of string = ('mqtt', 'mqtts', 'ws', 'wss', 'mqtt-sn');
var
  vHost: StringRAL;
begin
  vHost := AURI.Host;
  if Pos(StringRAL(':'), vHost) > 0 then
    vHost := '[' + vHost + ']';
  Result := StringRAL(cSchemes[AURI.Scheme]) + '://' + vHost + ':' +
    StringRAL(IntToStr(AURI.Port));
  if AURI.Scheme in [musWS, musWSS] then
    Result := Result + AURI.Path;
end;

end.
