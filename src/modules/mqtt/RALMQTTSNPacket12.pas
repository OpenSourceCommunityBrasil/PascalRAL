/// The bytes of MQTT-SN 1.2 (IBM, November 2013): every packet of the
/// specification, both directions, and the forwarder encapsulation.
unit RALMQTTSNPacket12;

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
  /// MQTT-SN 1.2.
  TRALMQTTSNCodec12 = class(TRALMQTTSNCodec)
  public
    function Version: TRALMQTTSNVersion; override;
    function Encode(APacket: TRALMQTTSNPacket): TBytes; override;
    function Decode(const AData: TBytes): TRALMQTTSNPacket; override;
  end;

implementation

const
  cTypes: array[TRALMQTTSNKind] of Integer = ($00, $01, $02, -1, $04, $05, $06, $07, $08,
    $09, $0A, $0B, $0C, $0D, $0E, $0F, $10, -1, $12, $13, $14, $15, $16, $17, $18, $1A,
    $1B, $1C, $1D, $FE);

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

{ TRALMQTTSNCodec12 }

function TRALMQTTSNCodec12.Version: TRALMQTTSNVersion;
begin
  Result := sv12;
end;

function TRALMQTTSNCodec12.Encode(APacket: TRALMQTTSNPacket): TBytes;
var
  vW: TRALMQTTWriter;
  vFlags: Byte;
  vHead: TBytes;
  vType: Integer;
begin
  vType := cTypes[APacket.Kind];
  if vType < 0 then
    raise ERALMQTTError.CreateReason(MQTTRC_ProtocolError,
      Format(emMQTTSNNotInVersion, [string(RALMQTTSNKindName(APacket.Kind)), '1.2']));
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
      snkCONNECT:
      begin
        vFlags := 0;
        if APacket.Will then
          vFlags := vFlags or $08;
        if APacket.CleanStart then
          vFlags := vFlags or $04;
        vW.WriteByte(vFlags);
        vW.WriteByte($01); // ProtocolId of 1.2
        vW.WriteWord(APacket.Duration);
        if APacket.ClientID <> '' then
          vW.WriteRaw(Pointer(APacket.ClientID)^, Length(APacket.ClientID));
      end;
      snkCONNACK, snkWILLTOPICRESP, snkWILLMSGRESP:
        vW.WriteByte(APacket.ReturnCode);
      snkWILLTOPICREQ, snkWILLMSGREQ, snkPINGRESP:
        ;
      snkWILLTOPIC, snkWILLTOPICUPD:
        // an empty one (no flags, no topic) removes the will
        if APacket.WillTopic <> '' then
        begin
          vFlags := QoSBits(APacket.WillQoS);
          if APacket.WillRetain then
            vFlags := vFlags or $10;
          vW.WriteByte(vFlags);
          vW.WriteRaw(Pointer(APacket.WillTopic)^, Length(APacket.WillTopic));
        end;
      snkWILLMSG, snkWILLMSGUPD:
        vW.WriteRawBytes(APacket.WillData);
      snkREGISTER:
      begin
        vW.WriteWord(APacket.TopicId);
        vW.WriteWord(APacket.MsgId);
        if APacket.TopicName <> '' then
          vW.WriteRaw(Pointer(APacket.TopicName)^, Length(APacket.TopicName));
      end;
      snkREGACK, snkPUBACK:
      begin
        vW.WriteWord(APacket.TopicId);
        vW.WriteWord(APacket.MsgId);
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
        vW.WriteWord(APacket.TopicId);
        vW.WriteWord(APacket.MsgId);
        vW.WriteRawBytes(APacket.Data);
      end;
      snkPUBCOMP, snkPUBREC, snkPUBREL, snkUNSUBACK:
        vW.WriteWord(APacket.MsgId);
      snkSUBSCRIBE, snkUNSUBSCRIBE:
      begin
        vFlags := Ord(APacket.TopicType);
        if APacket.Kind = snkSUBSCRIBE then
          vFlags := vFlags or QoSBits(APacket.QoS);
        if APacket.Dup then
          vFlags := vFlags or $80;
        vW.WriteByte(vFlags);
        vW.WriteWord(APacket.MsgId);
        if APacket.TopicType = sttNormal then
          vW.WriteRaw(Pointer(APacket.TopicName)^, Length(APacket.TopicName))
        else
          vW.WriteWord(APacket.TopicId);
      end;
      snkSUBACK:
      begin
        vW.WriteByte(QoSBits(APacket.QoS));
        vW.WriteWord(APacket.TopicId);
        vW.WriteWord(APacket.MsgId);
        vW.WriteByte(APacket.ReturnCode);
      end;
      snkPINGREQ:
        if APacket.ClientID <> '' then
          vW.WriteRaw(Pointer(APacket.ClientID)^, Length(APacket.ClientID));
      snkDISCONNECT:
        if APacket.HasDuration then
          vW.WriteWord(APacket.Duration);
      snkENCAPSULATED:
      begin
        // the length covers only the encapsulation header (5.5 of 1.2)
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

function TRALMQTTSNCodec12.Decode(const AData: TBytes): TRALMQTTSNPacket;
var
  vType, vFlags: Byte;
  vPos, vLen: IntegerRAL;
  vKind, vScan: TRALMQTTSNKind;
  vFound: boolean;

  procedure Need(ACount: IntegerRAL);
  begin
    if vPos + ACount > vLen then
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

  function GetRest: TBytes;
  begin
    Result := Copy(AData, vPos, vLen - vPos);
    vPos := vLen;
  end;

  function GetText: StringRAL;
  begin
    Result := RALMQTTText(GetRest);
    if not RALMQTTValidString(Result) then
      RALMQTTMalformed(emMQTTInvalidUTF8);
  end;

  procedure CheckTopicType(AFlags: Byte);
  begin
    if (AFlags and 3) = 3 then
      RALMQTTMalformed(emMQTTSNTopicType);
  end;

begin
  vLen := Length(AData);
  if (vLen >= 2) and (AData[1] = $FE) then
  begin
    // encapsulated: its own length, then a whole packet
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
        Result.GwAddress := GetRest;
      end;
      snkCONNECT:
      begin
        vFlags := GetByte;
        Result.Will := (vFlags and $08) <> 0;
        Result.CleanStart := (vFlags and $04) <> 0;
        if GetByte <> $01 then
          RALMQTTMalformed(emMQTTSNProtocolId, MQTTRC_UnsupportedVersion);
        Result.Duration := GetWord;
        Result.ClientID := GetText;
      end;
      snkCONNACK, snkWILLTOPICRESP, snkWILLMSGRESP:
        Result.ReturnCode := GetByte;
      snkWILLTOPICREQ, snkWILLMSGREQ, snkPINGRESP:
        ;
      snkWILLTOPIC, snkWILLTOPICUPD:
        if vPos < vLen then
        begin
          vFlags := GetByte;
          Result.WillQoS := (vFlags shr 5) and 3;
          if Result.WillQoS = 3 then
            RALMQTTMalformed(emMQTTWillFlags);
          Result.WillRetain := (vFlags and $10) <> 0;
          Result.WillTopic := GetText;
        end;
      snkWILLMSG, snkWILLMSGUPD:
        Result.WillData := GetRest;
      snkREGISTER:
      begin
        Result.TopicId := GetWord;
        Result.MsgId := GetWord;
        Result.TopicName := GetText;
      end;
      snkREGACK, snkPUBACK:
      begin
        Result.TopicId := GetWord;
        Result.MsgId := GetWord;
        Result.ReturnCode := GetByte;
      end;
      snkPUBLISH:
      begin
        vFlags := GetByte;
        CheckTopicType(vFlags);
        Result.Dup := (vFlags and $80) <> 0;
        Result.QoS := QoSOf(vFlags);
        Result.Retain := (vFlags and $10) <> 0;
        Result.TopicType := TRALMQTTSNTopicType(vFlags and 3);
        Result.TopicId := GetWord;
        Result.MsgId := GetWord;
        Result.Data := GetRest;
      end;
      snkPUBCOMP, snkPUBREC, snkPUBREL, snkUNSUBACK:
        Result.MsgId := GetWord;
      snkSUBSCRIBE, snkUNSUBSCRIBE:
      begin
        vFlags := GetByte;
        CheckTopicType(vFlags);
        Result.Dup := (vFlags and $80) <> 0;
        Result.QoS := QoSOf(vFlags);
        Result.TopicType := TRALMQTTSNTopicType(vFlags and 3);
        Result.MsgId := GetWord;
        if Result.TopicType = sttNormal then
          Result.TopicName := GetText
        else
          Result.TopicId := GetWord;
      end;
      snkSUBACK:
      begin
        Result.QoS := QoSOf(GetByte);
        Result.TopicId := GetWord;
        Result.MsgId := GetWord;
        Result.ReturnCode := GetByte;
      end;
      snkPINGREQ:
        Result.ClientID := GetText;
      snkDISCONNECT:
        if vPos < vLen then
        begin
          Result.HasDuration := True;
          Result.Duration := GetWord;
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
  RALMQTTSNRegisterCodec(sv12, TRALMQTTSNCodec12);

end.
