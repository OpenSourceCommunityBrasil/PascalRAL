/// The bytes of MQTT-SN 2.0, an OASIS draft (the working text of the MQTT-SN
/// subcommittee as read on 26/10/2026). It may change until it is published;
/// a change touches this unit only. The PROTECTION envelope (0xFF), still
/// unfinished in the draft itself, is not implemented.
unit RALMQTTSNPacket20;

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
  RALTypes, RALConsts, RALMQTTTypes, RALMQTTSNTypes;

type
  /// MQTT-SN 2.0 (draft).
  TRALMQTTSNCodec20 = class(TRALMQTTSNCodec)
  public
    function Version: TRALMQTTSNVersion; override;
    function Encode(APacket: TRALMQTTSNPacket): TBytes; override;
    function Decode(const AData: TBytes): TRALMQTTSNPacket; override;
  end;

implementation

const
  { -1: not in 2.0 (the old will packets, 0x06-0x09 and 0x1A-0x1D, are forbidden) }
  cTypes: array[TRALMQTTSNKind] of Integer = ($00, $01, $02, $03, $04, $05, -1, -1, -1,
    -1, $0A, $0B, $0C, $0D, $0E, $0F, $10, $11, $12, $13, $14, $15, $16, $17, $18, -1,
    -1, -1, -1, $FE);

function QoSBits(AQoS: ShortInt): Byte;
begin
  if AQoS < 0 then
    Result := $60
  else
    Result := (AQoS and 3) shl 5;
end;

function QoSOf(AFlags: Byte): ShortInt;
begin
  Result := (AFlags shr 5) and 3;
  if Result = 3 then
    Result := -1;
end;

{ TRALMQTTSNCodec20 }

function TRALMQTTSNCodec20.Version: TRALMQTTSNVersion;
begin
  Result := sv20;
end;

function TRALMQTTSNCodec20.Encode(APacket: TRALMQTTSNPacket): TBytes;
var
  vW: TRALMQTTWriter;
  vFlags: Byte;
  vHead: TBytes;
  vType: Integer;

  procedure TopicAndData(AType: TRALMQTTSNTopicType; AId: Word; const AName: StringRAL;
    const AData: TBytes);
  begin
    // a long name: its length in the topic field, the name before the data
    if AType = sttLong then
    begin
      vW.WriteWord(Word(Length(AName)));
      if AName <> '' then
        vW.WriteRaw(Pointer(AName)^, Length(AName));
    end
    else
      vW.WriteWord(AId);
    vW.WriteRawBytes(AData);
  end;

  procedure Auth;
  begin
    vW.WriteByte(Byte(Length(APacket.AuthMethod)));
    if APacket.AuthMethod <> '' then
      vW.WriteRaw(Pointer(APacket.AuthMethod)^, Length(APacket.AuthMethod));
    vW.WriteWord(Word(Length(APacket.AuthData)));
    vW.WriteRawBytes(APacket.AuthData);
  end;

begin
  vType := cTypes[APacket.Kind];
  if vType < 0 then
    raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError,
      Format(emMQTTSNNotInVersion, [string(RALMQTTSNKindName(APacket.Kind)), '2.0']));
  vW := TRALMQTTWriter.Create;
  try
    case APacket.Kind of
      snkADVERTISE:
      begin
        vW.WriteByte(APacket.GwId);
        vW.WriteWord(APacket.Duration);
      end;
      snkSEARCHGW:
        vW.WriteByte(APacket.Radius);
      snkGWINFO:
      begin
        vW.WriteByte(APacket.GwId);
        vW.WriteRawBytes(APacket.GwAddress);
      end;
      snkAUTH:
      begin
        vW.WriteByte(APacket.ReturnCode);
        vW.WriteByte(Byte(Length(APacket.AuthMethod)));
        if APacket.AuthMethod <> '' then
          vW.WriteRaw(Pointer(APacket.AuthMethod)^, Length(APacket.AuthMethod));
        vW.WriteRawBytes(APacket.AuthData);
      end;
      snkCONNECT:
      begin
        vFlags := (APacket.DefaultAwakeMessages and $0F) shl 3;
        if APacket.CleanStart then
          vFlags := vFlags or $01;
        if APacket.Will then
          vFlags := vFlags or $02;
        if APacket.HasAuth then
          vFlags := vFlags or $04;
        vW.WriteByte(vFlags);
        if APacket.Will then
        begin
          vFlags := Ord(APacket.WillTopicType) or ((APacket.WillQoS and 3) shl 2);
          if APacket.WillRetain then
            vFlags := vFlags or $10;
          vW.WriteByte(vFlags);
        end;
        vW.WriteByte(APacket.CorrelationId);
        vW.WriteByte($02); // Protocol Version of 2.0
        vW.WriteWord(APacket.KeepAlive);
        vW.WriteCardinal(APacket.SessionExpiry);
        vW.WriteWord(APacket.MaxPacketSize);
        if APacket.Will then
        begin
          if APacket.WillTopicType = sttLong then
            vW.WriteWord(Word(Length(APacket.WillTopic)))
          else
            vW.WriteWord(APacket.WillTopicId);
          vW.WriteWord(Word(Length(APacket.WillData)));
          if (APacket.WillTopicType = sttLong) and (APacket.WillTopic <> '') then
            vW.WriteRaw(Pointer(APacket.WillTopic)^, Length(APacket.WillTopic));
          vW.WriteRawBytes(APacket.WillData);
        end;
        if APacket.HasAuth then
          Auth;
        if APacket.ClientID <> '' then
          vW.WriteRaw(Pointer(APacket.ClientID)^, Length(APacket.ClientID));
      end;
      snkCONNACK:
      begin
        vFlags := 0;
        if APacket.SessionPresent and (APacket.ReturnCode = 0) then
          vFlags := vFlags or $01;
        if APacket.HasAuth then
          vFlags := vFlags or $02;
        vW.WriteByte(vFlags);
        vW.WriteByte(APacket.CorrelationId);
        vW.WriteByte(APacket.ReturnCode);
        vW.WriteCardinal(APacket.SessionExpiry);
        if APacket.HasAuth then
          Auth;
        if APacket.AssignedClientID <> '' then
          vW.WriteRaw(Pointer(APacket.AssignedClientID)^, Length(APacket.AssignedClientID));
      end;
      snkREGISTER:
      begin
        vW.WriteWord(APacket.TopicId);
        vW.WriteByte(APacket.CorrelationId);
        if APacket.TopicName <> '' then
          vW.WriteRaw(Pointer(APacket.TopicName)^, Length(APacket.TopicName));
      end;
      snkREGACK:
      begin
        vW.WriteByte(Ord(APacket.TopicType));
        vW.WriteWord(APacket.TopicId);
        vW.WriteByte(APacket.CorrelationId);
        vW.WriteByte(APacket.ReturnCode);
      end;
      snkPUBLISH:
      begin
        vFlags := QoSBits(APacket.QoS) or Byte(Ord(APacket.TopicType));
        if APacket.Dup then
          vFlags := vFlags or $80;
        if APacket.Retain then
          vFlags := vFlags or $10;
        vW.WriteByte(vFlags);
        if APacket.QoS > 0 then // no identifier for QoS 0 and -1
          vW.WriteWord(APacket.MsgId);
        TopicAndData(APacket.TopicType, APacket.TopicId, APacket.TopicName, APacket.Data);
      end;
      snkPUBLISHWOS:
      begin
        vFlags := Ord(APacket.TopicType);
        if APacket.Retain then
          vFlags := vFlags or $10;
        vW.WriteByte(vFlags);
        TopicAndData(APacket.TopicType, APacket.TopicId, APacket.TopicName, APacket.Data);
      end;
      snkPUBACK, snkPUBCOMP, snkPUBREC, snkPUBREL, snkUNSUBACK:
      begin
        vW.WriteWord(APacket.MsgId);
        vW.WriteByte(APacket.ReturnCode);
      end;
      snkSUBSCRIBE, snkUNSUBSCRIBE:
      begin
        vFlags := Ord(APacket.TopicType);
        if APacket.Kind = snkSUBSCRIBE then
        begin
          vFlags := vFlags or QoSBits(APacket.QoS) or ((APacket.RetainHandling and 3) shl 2);
          if APacket.NoLocal then
            vFlags := vFlags or $80;
          if APacket.RetainAsPublished then
            vFlags := vFlags or $10;
        end;
        vW.WriteByte(vFlags);
        vW.WriteWord(APacket.MsgId);
        if APacket.TopicType = sttLong then
          vW.WriteRaw(Pointer(APacket.TopicName)^, Length(APacket.TopicName))
        else
          vW.WriteWord(APacket.TopicId);
      end;
      snkSUBACK:
      begin
        vW.WriteByte(Ord(APacket.TopicType));
        vW.WriteWord(APacket.TopicId);
        vW.WriteWord(APacket.MsgId);
        vW.WriteByte(APacket.ReturnCode);
      end;
      snkPINGREQ:
      begin
        vW.WriteByte(APacket.CorrelationId);
        if APacket.ClientID <> '' then
          vW.WriteRaw(Pointer(APacket.ClientID)^, Length(APacket.ClientID));
      end;
      snkPINGRESP:
      begin
        vW.WriteByte(APacket.CorrelationId);
        if APacket.MessagesRemaining >= 0 then
          vW.WriteByte(Byte(APacket.MessagesRemaining));
      end;
      snkDISCONNECT:
      begin
        vFlags := 0;
        if APacket.HasReason then
          vFlags := vFlags or $08;
        if APacket.HasDuration then
          vFlags := vFlags or $04;
        if APacket.ReasonString <> '' then
          vFlags := vFlags or $02;
        if APacket.RetainRegistrations then
          vFlags := vFlags or $01;
        vW.WriteByte(vFlags);
        if APacket.HasReason then
          vW.WriteByte(APacket.ReturnCode);
        if APacket.HasDuration then
          vW.WriteCardinal(APacket.SessionExpiry);
        if APacket.ReasonString <> '' then
          vW.WriteRaw(Pointer(APacket.ReasonString)^, Length(APacket.ReasonString));
      end;
      snkENCAPSULATED:
      begin
        SetLength(vHead, 3 + Length(APacket.NodeId));
        vHead[0] := Length(vHead);
        vHead[1] := $FE;
        vHead[2] := APacket.Ctrl;
        if APacket.NodeId <> nil then
          Move(APacket.NodeId[0], vHead[3], Length(APacket.NodeId));
        vW.WriteRawBytes(vHead);
        vW.WriteRawBytes(APacket.Inner);
        Result := vW.ToBytes;
        Exit;
      end;
    end;
    Result := RALMQTTSNFrame(Byte(vType), vW.ToBytes);
  finally
    vW.Free;
  end;
end;

function TRALMQTTSNCodec20.Decode(const AData: TBytes): TRALMQTTSNPacket;
var
  vType, vFlags: Byte;
  vPos, vLen, vTopicLen, vWillLen, vAuthLen: IntegerRAL;
  vKind, vScan: TRALMQTTSNKind;
  vFound: boolean;

  procedure Need(ACount: IntegerRAL);
  begin
    if (ACount < 0) or (vPos + ACount > vLen) then
      RALMQTTMalformed(emMQTTTruncated);
  end;

  function GetByte: Byte;
  begin
    Need(1);
    Result := AData[vPos];
    Inc(vPos);
  end;

  function GetWord: Word;
  begin
    Need(2);
    Result := (Word(AData[vPos]) shl 8) or AData[vPos + 1];
    Inc(vPos, 2);
  end;

  function GetCardinal: Cardinal;
  begin
    Result := Cardinal(GetWord) shl 16;
    Result := Result or GetWord;
  end;

  function GetBytes(ACount: IntegerRAL): TBytes;
  begin
    Need(ACount);
    Result := Copy(AData, vPos, ACount);
    Inc(vPos, ACount);
  end;

  function GetText(ACount: IntegerRAL): StringRAL;
  begin
    Result := RALMQTTText(GetBytes(ACount));
    if not RALMQTTValidString(Result) then
      RALMQTTMalformed(emMQTTInvalidUTF8);
  end;

  { the topic field and what follows: data, or a long name and then data }
  procedure TopicAndData(APacket: TRALMQTTSNPacket);
  begin
    if APacket.TopicType = sttLong then
    begin
      vTopicLen := GetWord;
      APacket.TopicName := GetText(vTopicLen);
    end
    else
      APacket.TopicId := GetWord;
    APacket.Data := GetBytes(vLen - vPos);
  end;

  procedure Auth(APacket: TRALMQTTSNPacket);
  begin
    vAuthLen := GetByte;
    APacket.AuthMethod := GetText(vAuthLen);
    vAuthLen := GetWord;
    APacket.AuthData := GetBytes(vAuthLen);
  end;

begin
  vLen := Length(AData);
  if (vLen >= 2) and (AData[1] = $FE) then
  begin
    if (AData[0] < 3) or (AData[0] > vLen) then
      RALMQTTMalformed(emMQTTTruncated);
    Result := TRALMQTTSNPacket.Create(snkENCAPSULATED);
    Result.Ctrl := AData[2];
    Result.NodeId := Copy(AData, 3, AData[0] - 3);
    Result.Inner := Copy(AData, AData[0], vLen - AData[0]);
    Exit;
  end;
  RALMQTTSNHeader(AData, vType, vPos);
  vFound := False;
  vKind := snkADVERTISE;
  for vScan := Low(TRALMQTTSNKind) to High(TRALMQTTSNKind) do
    if cTypes[vScan] = vType then
    begin
      vKind := vScan;
      vFound := True;
      Break;
    end;
  if not vFound then
    RALMQTTMalformed(Format(emMQTTSNUnknownType, [vType]));
  Result := TRALMQTTSNPacket.Create(vKind);
  try
    case vKind of
      snkADVERTISE:
      begin
        Result.GwId := GetByte;
        Result.Duration := GetWord;
      end;
      snkSEARCHGW:
        Result.Radius := GetByte;
      snkGWINFO:
      begin
        Result.GwId := GetByte;
        Result.GwAddress := GetBytes(vLen - vPos);
      end;
      snkAUTH:
      begin
        Result.ReturnCode := GetByte;
        vAuthLen := GetByte;
        Result.AuthMethod := GetText(vAuthLen);
        Result.AuthData := GetBytes(vLen - vPos);
      end;
      snkCONNECT:
      begin
        vFlags := GetByte;
        if (vFlags and $80) <> 0 then
          RALMQTTMalformed(emMQTTConnectReserved);
        Result.CleanStart := (vFlags and $01) <> 0;
        Result.Will := (vFlags and $02) <> 0;
        Result.HasAuth := (vFlags and $04) <> 0;
        Result.DefaultAwakeMessages := (vFlags shr 3) and $0F;
        if Result.Will then
        begin
          vFlags := GetByte;
          if (vFlags and $E0) <> 0 then
            RALMQTTMalformed(emMQTTWillFlags);
          Result.WillTopicType := TRALMQTTSNTopicType(vFlags and 3);
          Result.WillQoS := (vFlags shr 2) and 3;
          if Result.WillQoS = 3 then
            RALMQTTMalformed(emMQTTWillFlags);
          Result.WillRetain := (vFlags and $10) <> 0;
        end;
        Result.CorrelationId := GetByte;
        if GetByte <> $02 then
          RALMQTTMalformed(emMQTTSNProtocolId, MQTTRC_UnsupportedVersion);
        Result.KeepAlive := GetWord;
        if Result.KeepAlive = 0 then // a protocol error in 2.0 (3.1.4.5)
          RALMQTTMalformed(emMQTTSNKeepAliveZero, MQTTRC_ProtocolError);
        Result.SessionExpiry := GetCardinal;
        Result.MaxPacketSize := GetWord;
        if Result.Will then
        begin
          vTopicLen := GetWord;
          vWillLen := GetWord;
          if Result.WillTopicType = sttLong then
            Result.WillTopic := GetText(vTopicLen)
          else
            Result.WillTopicId := vTopicLen;
          Result.WillData := GetBytes(vWillLen);
        end;
        if Result.HasAuth then
          Auth(Result);
        Result.ClientID := GetText(vLen - vPos);
      end;
      snkCONNACK:
      begin
        vFlags := GetByte;
        if (vFlags and $FC) <> 0 then
          RALMQTTMalformed(emMQTTConnackReserved);
        Result.SessionPresent := (vFlags and $01) <> 0;
        Result.HasAuth := (vFlags and $02) <> 0;
        Result.CorrelationId := GetByte;
        Result.ReturnCode := GetByte;
        Result.SessionExpiry := GetCardinal;
        if Result.HasAuth then
          Auth(Result);
        Result.AssignedClientID := GetText(vLen - vPos);
      end;
      snkREGISTER:
      begin
        Result.TopicId := GetWord;
        Result.CorrelationId := GetByte;
        Result.TopicName := GetText(vLen - vPos);
      end;
      snkREGACK:
      begin
        Result.TopicType := TRALMQTTSNTopicType(GetByte and 3);
        Result.TopicId := GetWord;
        Result.CorrelationId := GetByte;
        Result.ReturnCode := GetByte;
      end;
      snkPUBLISH:
      begin
        vFlags := GetByte;
        if (vFlags and $0C) <> 0 then
          RALMQTTMalformed(emMQTTSNReservedFlags);
        Result.Dup := (vFlags and $80) <> 0;
        Result.QoS := QoSOf(vFlags);
        Result.Retain := (vFlags and $10) <> 0;
        Result.TopicType := TRALMQTTSNTopicType(vFlags and 3);
        if Result.QoS > 0 then
          Result.MsgId := GetWord;
        TopicAndData(Result);
      end;
      snkPUBLISHWOS:
      begin
        vFlags := GetByte;
        if (vFlags and $EC) <> 0 then
          RALMQTTMalformed(emMQTTSNReservedFlags);
        Result.QoS := -1;
        Result.Retain := (vFlags and $10) <> 0;
        Result.TopicType := TRALMQTTSNTopicType(vFlags and 3);
        if Result.TopicType = sttNormal then // a session alias needs a session
          RALMQTTMalformed(emMQTTSNTopicType);
        TopicAndData(Result);
      end;
      snkPUBACK, snkPUBCOMP, snkPUBREC, snkPUBREL, snkUNSUBACK:
      begin
        Result.MsgId := GetWord;
        Result.ReturnCode := GetByte;
      end;
      snkSUBSCRIBE, snkUNSUBSCRIBE:
      begin
        vFlags := GetByte;
        Result.TopicType := TRALMQTTSNTopicType(vFlags and 3);
        if vKind = snkSUBSCRIBE then
        begin
          Result.NoLocal := (vFlags and $80) <> 0;
          Result.QoS := (vFlags shr 5) and 3;
          if Result.QoS = 3 then
            RALMQTTMalformed(emMQTTSubscribeOptions, MQTTRC_ProtocolError);
          Result.RetainAsPublished := (vFlags and $10) <> 0;
          Result.RetainHandling := (vFlags shr 2) and 3;
          if Result.RetainHandling = 3 then
            RALMQTTMalformed(emMQTTSubscribeOptions, MQTTRC_ProtocolError);
        end
        else if (vFlags and $FC) <> 0 then
          RALMQTTMalformed(emMQTTSNReservedFlags);
        Result.MsgId := GetWord;
        if Result.TopicType = sttLong then
          Result.TopicName := GetText(vLen - vPos)
        else
          Result.TopicId := GetWord;
      end;
      snkSUBACK:
      begin
        vFlags := GetByte;
        if (vFlags and $FC) <> 0 then
          RALMQTTMalformed(emMQTTSNReservedFlags);
        Result.TopicType := TRALMQTTSNTopicType(vFlags and 3);
        Result.TopicId := GetWord;
        Result.MsgId := GetWord;
        Result.ReturnCode := GetByte;
      end;
      snkPINGREQ:
      begin
        Result.CorrelationId := GetByte;
        Result.ClientID := GetText(vLen - vPos);
      end;
      snkPINGRESP:
      begin
        Result.CorrelationId := GetByte;
        if vPos < vLen then
          Result.MessagesRemaining := GetByte;
      end;
      snkDISCONNECT:
      begin
        vFlags := GetByte;
        if (vFlags and $F0) <> 0 then
          RALMQTTMalformed(emMQTTSNReservedFlags);
        Result.RetainRegistrations := (vFlags and $01) <> 0;
        Result.HasReason := (vFlags and $08) <> 0;
        Result.HasDuration := (vFlags and $04) <> 0;
        if Result.HasReason then
          Result.ReturnCode := GetByte;
        if Result.HasDuration then
          Result.SessionExpiry := GetCardinal;
        if (vFlags and $02) <> 0 then
          Result.ReasonString := GetText(vLen - vPos);
      end;
    end;
    if vPos <> vLen then
      RALMQTTMalformed(Format(emMQTTTrailingBytes, [string(RALMQTTSNKindName(vKind))]));
  except
    Result.Free;
    raise;
  end;
end;

initialization
  RALMQTTSNRegisterCodec(sv20, TRALMQTTSNCodec20);

end.
