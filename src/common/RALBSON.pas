unit RALBSON;

{$IFDEF FPC}
{$mode delphi}
{$H+}
{$ENDIF}

{$I ..\core\PascalRAL.inc}

interface

uses
  Classes,
  SysUtils,
  RALJson, RALTypes;

const
  RALBSONRootElementKey = '$RALElement';

type
  { BSON roots are documents. Canonical mode preserves BSON numeric types. }
  TRALBSON = class
  public
    class function Parse(
      const Bytes: TBytes;
      Canonical: Boolean = True;
      MaxDocumentSize: Integer = 0;
      MaxDepth: Integer = 0;
      UnwrapRootElement: Boolean = True
    ): TRALJSON; overload; static;
    class function Parse(
      Stream: TStream;
      Canonical: Boolean = True;
      MaxDocumentSize: Integer = 0;
      MaxDepth: Integer = 0;
      UnwrapRootElement: Boolean = True
    ): TRALJSON; overload; static;
    class function Encode(
      Document: TRALJSON;
      InterpretExtendedJSON: Boolean = True;
      MaxDocumentSize: Integer = 0;
      MaxDepth: Integer = 0
    ): TBytes; static;
    class procedure SaveToStream(
      Document: TRALJSON;
      Stream: TStream;
      InterpretExtendedJSON: Boolean = True;
      MaxDocumentSize: Integer = 0;
      MaxDepth: Integer = 0
    ); static;
  end;

implementation

uses
  RALBase64, RALExceptions, RALErrorCodes;

type
  TRALBSONReader = class
  private
    FStream: TStream;
    FPosition: Int64;
    FCanonical: Boolean;
    FMaxDocumentSize: Integer;
    FMaxDepth: Integer;
    procedure ReadRaw(
      var Buffer;
      Count: Integer;
      Limit: Int64
    );
    function ReadByte(Limit: Int64): Byte;
    function Read32(Limit: Int64): Cardinal;
    function Read64(Limit: Int64): UInt64;
    function ReadSigned32(Limit: Int64): Integer;
    function ReadSigned64(Limit: Int64): Int64;
    function ReadText(
      Limit: Int64;
      CString: Boolean
    ): StringRAL;
    function ReadValue(
      Kind: Byte;
      Limit: Int64;
      Depth: Integer
    ): TRALJSON;
  public
    constructor Create(
      Stream: TStream;
      Canonical: Boolean;
      MaxSize: Integer;
      MaxDepth: Integer
    );
    function ReadDocument(
      IsArray: Boolean;
      Limit: Int64;
      Depth: Integer
    ): TRALJSON;
  end;

  TRALBSONWriter = class
  private
    FStream: TMemoryStream;
    FExtended: Boolean;
    FMaxDocumentSize: Integer;
    FMaxDepth: Integer;
    procedure WriteRaw(
      const Buffer;
      Count: Integer
    );
    procedure WriteByte(Value: Byte);
    procedure Write32(Value: Cardinal);
    procedure Write64(Value: UInt64);
    procedure WriteSigned32(Value: Integer);
    procedure WriteSigned64(Value: Int64);
    procedure WriteText(
      const Value: StringRAL;
      CString: Boolean
    );
    procedure WriteElement(
      const Name: StringRAL;
      Value: TRALJSON;
      Depth: Integer
    );
  public
    constructor Create(
      Extended: Boolean;
      MaxSize: Integer;
      MaxDepth: Integer
    );
    destructor Destroy; override;
    procedure WriteDocument(
      Value: TRALJSON;
      Depth: Integer
    );
    procedure WriteRoot(Document: TRALJSON);
    property Stream: TMemoryStream read FStream;
  end;

procedure CheckLimits(
  MaxSize: Integer;
  MaxDepth: Integer
);
begin
  if (MaxSize < 0) or (MaxDepth < 0) then
    raise ERALException.Create(ecBSONNegativeLimits);
end;

procedure AddOwned(
  Obj: TRALJSONObject;
  const Name: StringRAL;
  Value: TRALJSON
);
begin
  try
    Obj.Add(Name, Value);
  except
    Value.Free;
    raise;
  end;
end;

function Wrap(
  const Name: StringRAL;
  Value: TRALJSON
): TRALJSONObject;
begin
  Result := nil;
  try
    Result := TRALJSONObject.Create;
  except
    Value.Free;
    raise;
  end;
  try
    AddOwned(Result, Name, Value);
  except
    Result.Free;
    raise;
  end;
end;

function WrapString(
  const Name: StringRAL;
  const Value: StringRAL
): TRALJSONObject;
begin
  Result := Wrap(Name, TRALJSONString.Create(Value));
end;

function RequireString(Value: TRALJSON): StringRAL;
begin
  if (Value = nil) or (Value.Kind <> rjkString) then
    raise ERALException.Create(ecExtendedJSONStringRequired);
  Result := Value.AsString;
end;

function RequireObject(
  Value: TRALJSON;
  Count: Integer
): TRALJSONObject;
begin
  if (Value = nil) or (Value.Kind <> rjkObject) then
    raise ERALException.Create(ecExtendedJSONObjectRequired);
  Result := Value.AsObject;
  if Result.Count <> Count then
    raise ERALException.Create(ecExtendedJSONInvalidMemberCount);
end;

function IntegerText(const Text: StringRAL): Int64;
var
  Number: TRALJSONNumber;
begin
  Number := TRALJSONNumber.Create(Text);
  try
    Result := Number.AsInt64;
  finally
    Number.Free;
  end;
end;

function HexValue(C: AnsiChar): Integer;
begin
  case C of
    '0'..'9':
      Result := Ord(C) - Ord('0');
    'a'..'f':
      Result := Ord(C) - Ord('a') + 10;
    'A'..'F':
      Result := Ord(C) - Ord('A') + 10;
  else
    raise ERALException.Create(ecHexInvalidText);
  end;
end;

function ToHex(const Bytes: TBytes): StringRAL;
const
  Digits: StringRAL = '0123456789abcdef';
var
  I: Integer;
begin
  if Length(Bytes) > MaxInt div 2 then
    raise EIntOverflow.Create('Hexadecimal output is too large.');
  SetLength(Result, Length(Bytes) * 2);
  for I := 0 to Length(Bytes) - 1 do
  begin
    Result[I * 2 + 1] := Digits[(Bytes[I] shr 4) + 1];
    Result[I * 2 + 2] := Digits[(Bytes[I] and 15) + 1];
  end;
end;

function FromHex(
  const Text: StringRAL;
  Size: Integer
): TBytes;
var
  I: Integer;
begin
  Result := nil;
  if Length(Text) <> Size * 2 then
    raise ERALException.Create(ecHexInvalidLength);
  SetLength(Result, Size);
  for I := 0 to Size - 1 do
    Result[I] := HexValue(Text[I * 2 + 1]) * 16 + HexValue(Text[I * 2 + 2]);
end;

function FloatText(Bits: UInt64): StringRAL;
var
  Value: Double;
  Settings: TFormatSettings;
begin
  if (Bits and UInt64($7FF0000000000000)) = UInt64($7FF0000000000000) then
  begin
    if (Bits and UInt64($000FFFFFFFFFFFFF)) <> 0 then
      Exit('NaN');
    if (Bits shr 63) <> 0 then
      Exit('-Infinity');
    Exit('Infinity');
  end;
  if Bits = UInt64($8000000000000000) then
    Exit('-0.0');
  Move(Bits, Value, 8);
  {$IFDEF FPC}
  Settings := DefaultFormatSettings;
  {$ELSE}
  Settings := TFormatSettings.Create;
  {$ENDIF}
  Settings.DecimalSeparator := '.';
  Result := FloatToStrF(Value, ffGeneral, 17, 0, Settings);
  if (Pos('.', Result) = 0) and (Pos('E', Result) = 0) and (Pos('e', Result) = 0) then
    Result := Result + '.0';
end;

function FloatBits(const Text: StringRAL): UInt64;
var
  Value: Double;
  Number: TRALJSONNumber;
begin
  if Text = 'NaN' then
    Exit(UInt64($7FF8000000000000));
  if Text = 'Infinity' then
    Exit(UInt64($7FF0000000000000));
  if Text = '-Infinity' then
    Exit(UInt64($FFF0000000000000));
  Number := TRALJSONNumber.Create(Text);
  try
    Value := Number.AsNumber;
    Move(Value, Result, 8);
    if (Value = 0) and (Length(Text) > 0) and (Text[1] = '-') then
      Result := UInt64($8000000000000000);
  finally
    Number.Free;
  end;
end;

procedure ValidateRegexOptions(const Text: StringRAL);
var
  I: Integer;
begin
  for I := 1 to Length(Text) do
  begin
    if Pos(Text[I], 'ilmsux') = 0 then
      raise ERALException.Create(ecBSONInvalidRegexOption);
    if (I > 1) and (Text[I] <= Text[I - 1]) then
      raise ERALException.Create(ecBSONRegexOptionsOrder);
  end;
end;

constructor TRALBSONReader.Create(
  Stream: TStream;
  Canonical: Boolean;
  MaxSize: Integer;
  MaxDepth: Integer
);
begin
  inherited Create;
  if Stream = nil then
    raise ERALException.Create(ecBSONSourceRequired);
  CheckLimits(MaxSize, MaxDepth);
  FStream := Stream;
  FCanonical := Canonical;
  FMaxDocumentSize := MaxSize;
  FMaxDepth := MaxDepth;
end;

procedure TRALBSONReader.ReadRaw(
  var Buffer;
  Count: Integer;
  Limit: Int64
);
var
  Cursor: PByte;
  Remaining: Integer;
  ReadCount: Integer;
begin
  if (Count < 0) or (Count > Limit - FPosition) then
    raise ERALException.Create(ecBSONValueBeyondDocument);
  Cursor := @Buffer;
  Remaining := Count;
  while Remaining > 0 do
  begin
    ReadCount := FStream.Read(Cursor^, Remaining);
    if (ReadCount <= 0) or (ReadCount > Remaining) then
      raise ERALException.Create(ecBSONInvalidRead);
    Inc(Cursor, ReadCount);
    Inc(FPosition, ReadCount);
    Dec(Remaining, ReadCount);
  end;
end;

function TRALBSONReader.ReadByte(Limit: Int64): Byte;
begin
  ReadRaw(Result, 1, Limit);
end;

function TRALBSONReader.Read32(Limit: Int64): Cardinal;
var
  Bytes: array[0..3] of Byte;
begin
  ReadRaw(Bytes, 4, Limit);
  Result := Cardinal(Bytes[0]) or (Cardinal(Bytes[1]) shl 8) or
    (Cardinal(Bytes[2]) shl 16) or (Cardinal(Bytes[3]) shl 24);
end;

function TRALBSONReader.Read64(Limit: Int64): UInt64;
var
  LowPart: Cardinal;
  HighPart: Cardinal;
begin
  LowPart := Read32(Limit);
  HighPart := Read32(Limit);
  Result := UInt64(LowPart) or (UInt64(HighPart) shl 32);
end;

function TRALBSONReader.ReadSigned32(Limit: Int64): Integer;
var
  Bits: Cardinal;
begin
  Bits := Read32(Limit);
  Move(Bits, Result, 4);
end;

function TRALBSONReader.ReadSigned64(Limit: Int64): Int64;
var
  Bits: UInt64;
begin
  Bits := Read64(Limit);
  Move(Bits, Result, 8);
end;

function TRALBSONReader.ReadText(
  Limit: Int64;
  CString: Boolean
): StringRAL;
var
  Bytes: RawByteString;
  Count: Integer;
  Capacity: Integer;
  Value: Byte;
begin
  if not CString then
  begin
    Count := ReadSigned32(Limit);
    if (Count < 1) or (Count > Limit - FPosition) then
      raise ERALException.Create(ecBSONInvalidStringLength);
    SetLength(Bytes, Count - 1);
    if Count > 1 then
      ReadRaw(Bytes[1], Count - 1, Limit);
    if ReadByte(Limit) <> 0 then
      raise ERALException.Create(ecBSONMissingStringTerminator);
  end
  else
  begin
    Count := 0;
    Capacity := 0;
    repeat
      Value := ReadByte(Limit);
      if Value = 0 then
        Break;
      if Count = Capacity then
      begin
        if Capacity > MaxInt div 2 then
          raise EIntOverflow.Create('BSON key is too large.');
        Capacity := Capacity * 2;
        if Capacity < 64 then
          Capacity := 64;
        SetLength(Bytes, Capacity);
      end;
      Inc(Count);
      Bytes[Count] := AnsiChar(Value);
    until False;
    SetLength(Bytes, Count);
  end;
  Result := TRALJSON.DecodeUTF8(Bytes);
end;

function TRALBSONReader.ReadDocument(
  IsArray: Boolean;
  Limit: Int64;
  Depth: Integer
): TRALJSON;
var
  Start: Int64;
  Finish: Int64;
  Size: Integer;
  Kind: Byte;
  Name: StringRAL;
  Value: TRALJSON;
  Index: Integer;
begin
  if (FMaxDepth > 0) and (Depth > FMaxDepth) then
    raise ERALException.Create(ecBSONDepthExceeded);
  Start := FPosition;
  Size := ReadSigned32(Limit);
  if (Size < 5) or (Size - 4 > Limit - FPosition) then
    raise ERALException.Create(ecBSONInvalidDocumentLength);
  if (FMaxDocumentSize > 0) and (Size > FMaxDocumentSize) then
    raise ERALException.Create(ecBSONDocumentTooLarge);
  Finish := Start + Size;
  if IsArray then
    Result := TRALJSONArray.Create
  else
    Result := TRALJSONObject.Create;
  try
    Index := 0;
    while FPosition < Finish - 1 do
    begin
      Kind := ReadByte(Finish - 1);
      if Kind = 0 then
        raise ERALException.Create(ecBSONPrematureDocumentTerminator);
      Name := ReadText(Finish - 1, True);
      if IsArray then
      begin
        if Name <> IntToStr(Index) then
          raise ERALException.Create(ecBSONNonSequentialArrayIndexes);
      end
      else if Result.AsObject.Find(Name) <> nil then
        raise ERALException.CreateFmt(ecBSONDuplicateMember, [Name]);
      Value := ReadValue(Kind, Finish - 1, Depth);
      try
        if IsArray then
          Result.AsArray.Add(Value)
        else
          Result.AsObject.Add(Name, Value);
      except
        Value.Free;
        raise;
      end;
      Inc(Index);
    end;
    if ReadByte(Finish) <> 0 then
      raise ERALException.Create(ecBSONMissingDocumentTerminator);
  except
    Result.Free;
    raise;
  end;
end;

function TRALBSONReader.ReadValue(
  Kind: Byte;
  Limit: Int64;
  Depth: Integer
): TRALJSON;
var
  Text: StringRAL;
  Name: StringRAL;
  Number: Int64;
  Bits: UInt64;
  Size: Integer;
  SubType: Byte;
  Bytes: TBytes;
  Obj: TRALJSONObject;
  Scope: TRALJSON;
  Finish: Int64;
  Stamp: Cardinal;
  Increment: Cardinal;
begin
  case Kind of
    $01:
      begin
        Bits := Read64(Limit);
        Text := FloatText(Bits);
        if FCanonical or ((Bits and UInt64($7FF0000000000000)) = UInt64($7FF0000000000000)) then
          Result := WrapString('$numberDouble', Text)
        else
          Result := TRALJSONNumber.Create(Text);
      end;
    $02:
      Result := TRALJSONString.Create(ReadText(Limit, False));
    $03:
      Result := ReadDocument(False, Limit, Depth + 1);
    $04:
      Result := ReadDocument(True, Limit, Depth + 1);
    $05:
      begin
        Size := ReadSigned32(Limit);
        SubType := ReadByte(Limit);
        if (Size < 0) or (Size > Limit - FPosition) then
          raise ERALException.Create(ecBSONInvalidBinaryLength);
        if SubType = 2 then
        begin
          if Size < 4 then
            raise ERALException.Create(ecBSONInvalidOldBinaryLength);
          if ReadSigned32(Limit) <> Size - 4 then
            raise ERALException.Create(ecBSONOldBinaryLengthMismatch);
          Dec(Size, 4);
        end;
        SetLength(Bytes, Size);
        if Size > 0 then
          ReadRaw(Bytes[0], Size, Limit);
        Obj := TRALJSONObject.Create;
        try
          AddOwned(Obj, 'base64', TRALJSONString.Create(TRALBase64.Encode(Bytes)));
          AddOwned(Obj, 'subType', TRALJSONString.Create(LowerCase(IntToHex(SubType, 2))));
        except
          Obj.Free;
          raise;
        end;
        Result := Wrap('$binary', Obj);
      end;
    $06:
      Result := Wrap('$undefined', TRALJSONBoolean.Create(True));
    $07:
      begin
        SetLength(Bytes, 12);
        ReadRaw(Bytes[0], 12, Limit);
        Result := WrapString('$oid', ToHex(Bytes));
      end;
    $08:
      begin
        SubType := ReadByte(Limit);
        if SubType > 1 then
          raise ERALException.Create(ecBSONInvalidBoolean);
        Result := TRALJSONBoolean.Create(SubType = 1);
      end;
    $09:
      Result := Wrap('$date', WrapString('$numberLong', IntToStr(ReadSigned64(Limit))));
    $0A:
      Result := TRALJSONNull.Create;
    $0B:
      begin
        Text := ReadText(Limit, True);
        Name := ReadText(Limit, True);
        ValidateRegexOptions(Name);
        Obj := TRALJSONObject.Create;
        try
          AddOwned(Obj, 'pattern', TRALJSONString.Create(Text));
          AddOwned(Obj, 'options', TRALJSONString.Create(Name));
        except
          Obj.Free;
          raise;
        end;
        Result := Wrap('$regularExpression', Obj);
      end;
    $0C:
      begin
        Text := ReadText(Limit, False);
        SetLength(Bytes, 12);
        ReadRaw(Bytes[0], 12, Limit);
        Obj := TRALJSONObject.Create;
        try
          AddOwned(Obj, '$ref', TRALJSONString.Create(Text));
          AddOwned(Obj, '$id', WrapString('$oid', ToHex(Bytes)));
        except
          Obj.Free;
          raise;
        end;
        Result := Wrap('$dbPointer', Obj);
      end;
    $0D:
      Result := WrapString('$code', ReadText(Limit, False));
    $0E:
      Result := WrapString('$symbol', ReadText(Limit, False));
    $0F:
      begin
        Finish := FPosition;
        Size := ReadSigned32(Limit);
        if (Size < 14) or (Size - 4 > Limit - FPosition) then
          raise ERALException.Create(ecBSONInvalidCodeWithScopeLength);
        Inc(Finish, Size);
        Text := ReadText(Finish, False);
        Scope := ReadDocument(False, Finish, Depth + 1);
        try
          if FPosition <> Finish then
            raise ERALException.Create(ecBSONCodeWithScopeLengthMismatch);
          Obj := WrapString('$code', Text);
        except
          Scope.Free;
          raise;
        end;
        try
          AddOwned(Obj, '$scope', Scope);
        except
          Obj.Free;
          raise;
        end;
        Result := Obj;
      end;
    $10, $12:
      begin
        if Kind = $10 then
          Number := ReadSigned32(Limit)
        else
          Number := ReadSigned64(Limit);
        Text := IntToStr(Number);
        if FCanonical then
        begin
          if Kind = $10 then
            Name := '$numberInt'
          else
            Name := '$numberLong';
          Result := WrapString(Name, Text);
        end
        else
          Result := TRALJSONNumber.Create(Text);
      end;
    $11:
      begin
        Increment := Read32(Limit);
        Stamp := Read32(Limit);
        Obj := TRALJSONObject.Create;
        try
          AddOwned(Obj, 't', TRALJSONNumber.Create(IntToStr(Int64(Stamp))));
          AddOwned(Obj, 'i', TRALJSONNumber.Create(IntToStr(Int64(Increment))));
        except
          Obj.Free;
          raise;
        end;
        Result := Wrap('$timestamp', Obj);
      end;
    $13:
      raise ERALException.Create(ecBSONDecimal128Unsupported);
    $7F:
      Result := Wrap('$maxKey', TRALJSONNumber.Create('1'));
    $FF:
      Result := Wrap('$minKey', TRALJSONNumber.Create('1'));
  else
    raise ERALException.CreateFmt(ecBSONUnknownType, [Kind]);
  end;
end;

constructor TRALBSONWriter.Create(
  Extended: Boolean;
  MaxSize: Integer;
  MaxDepth: Integer
);
begin
  inherited Create;
  CheckLimits(MaxSize, MaxDepth);
  FExtended := Extended;
  FMaxDocumentSize := MaxSize;
  FMaxDepth := MaxDepth;
  FStream := TMemoryStream.Create;
end;

destructor TRALBSONWriter.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TRALBSONWriter.WriteRaw(
  const Buffer;
  Count: Integer
);
begin
  if (Count < 0) or (Count > MaxInt - FStream.Position) then
    raise EIntOverflow.Create('BSON output exceeds Int32 capacity.');
  if (FMaxDocumentSize > 0) and (Count > FMaxDocumentSize - FStream.Position) then
    raise ERALException.Create(ecBSONOutputTooLarge);
  if Count > 0 then
    FStream.WriteBuffer(Buffer, Count);
end;

procedure TRALBSONWriter.WriteByte(Value: Byte);
begin
  WriteRaw(Value, 1);
end;

procedure TRALBSONWriter.Write32(Value: Cardinal);
var
  Bytes: array[0..3] of Byte;
  I: Integer;
begin
  for I := 0 to 3 do
    Bytes[I] := Byte((Value shr (I * 8)) and 255);
  WriteRaw(Bytes, 4);
end;

procedure TRALBSONWriter.Write64(Value: UInt64);
begin
  Write32(Cardinal(Value and UInt64($FFFFFFFF)));
  Write32(Cardinal(Value shr 32));
end;

procedure TRALBSONWriter.WriteSigned32(Value: Integer);
var
  Bits: Cardinal;
begin
  Move(Value, Bits, 4);
  Write32(Bits);
end;

procedure TRALBSONWriter.WriteSigned64(Value: Int64);
var
  Bits: UInt64;
begin
  Move(Value, Bits, 8);
  Write64(Bits);
end;

procedure TRALBSONWriter.WriteText(
  const Value: StringRAL;
  CString: Boolean
);
var
  Bytes: RawByteString;
begin
  Bytes := TRALJSON.EncodeUTF8(Value);
  if CString then
  begin
    if Pos(AnsiString(#0), Bytes) <> 0 then
      raise ERALException.Create(ecBSONCStringContainsNUL);
  end
  else
  begin
    if Length(Bytes) = MaxInt then
      raise EIntOverflow.Create('BSON string is too large.');
    WriteSigned32(Length(Bytes) + 1);
  end;
  if Length(Bytes) > 0 then
    WriteRaw(Bytes[1], Length(Bytes));
  WriteByte(0);
end;

procedure TRALBSONWriter.WriteDocument(
  Value: TRALJSON;
  Depth: Integer
);
var
  Start: Int64;
  Finish: Int64;
  I: Integer;
begin
  if (FMaxDepth > 0) and (Depth > FMaxDepth) then
    raise ERALException.Create(ecBSONDepthExceeded);
  if (Value = nil) or not (Value.Kind in [rjkObject, rjkArray]) then
    raise ERALException.Create(ecBSONDocumentRequired);
  Start := FStream.Position;
  Write32(0);
  if Value.Kind = rjkObject then
    for I := 0 to Value.AsObject.Count - 1 do
      WriteElement(Value.AsObject.Names[I], Value.AsObject.Items[I], Depth)
  else
    for I := 0 to Value.AsArray.Count - 1 do
      WriteElement(IntToStr(I), Value.AsArray[I], Depth);
  WriteByte(0);
  Finish := FStream.Position;
  if Finish - Start > MaxInt then
    raise EIntOverflow.Create('BSON document exceeds Int32 capacity.');
  FStream.Position := Start;
  WriteSigned32(Finish - Start);
  FStream.Position := Finish;
end;

function ExtendedType(Obj: TRALJSONObject): Integer;
var
  Name: StringRAL;
begin
  Result := -1;
  if (Obj.Count = 2) and (Obj.Find('$code') <> nil) and (Obj.Find('$scope') <> nil) then
    Exit($0F);
  if Obj.Count <> 1 then
    Exit;
  Name := Obj.Names[0];
  if Name = '$numberDouble' then
    Result := $01
  else if Name = '$binary' then
    Result := $05
  else if Name = '$undefined' then
    Result := $06
  else if Name = '$oid' then
    Result := $07
  else if Name = '$date' then
    Result := $09
  else if Name = '$regularExpression' then
    Result := $0B
  else if Name = '$dbPointer' then
    Result := $0C
  else if Name = '$code' then
    Result := $0D
  else if Name = '$symbol' then
    Result := $0E
  else if Name = '$numberInt' then
    Result := $10
  else if Name = '$timestamp' then
    Result := $11
  else if Name = '$numberLong' then
    Result := $12
  else if Name = '$numberDecimal' then
    Result := $13
  else if Name = '$maxKey' then
    Result := $7F
  else if Name = '$minKey' then
    Result := $FF;
end;

function RequireUInt32(Value: TRALJSON): Cardinal;
var
  Number: Int64;
begin
  if (Value = nil) or (Value.Kind <> rjkNumber) then
    raise ERALException.Create(ecBSONTimestampUnsignedRequired);
  Number := Value.AsInt64;
  if (Number < 0) or (Number > Int64(High(Cardinal))) then
    raise ERALException.Create(ecBSONTimestampUInt32Range);
  Result := Number;
end;

procedure TRALBSONWriter.WriteElement(
  const Name: StringRAL;
  Value: TRALJSON;
  Depth: Integer
);
var
  Kind: Integer;
  Obj: TRALJSONObject;
  Payload: TRALJSONObject;
  Text: StringRAL;
  Number: Int64;
  Bytes: TBytes;
  Start: Int64;
  Finish: Int64;
  SubType: Byte;
begin
  if Value = nil then
    raise ERALException.Create(ecBSONValueRequired);
  Obj := nil;
  Kind := -1;
  if FExtended and (Value.Kind = rjkObject) then
  begin
    Obj := Value.AsObject;
    Kind := ExtendedType(Obj);
  end;
  if Kind < 0 then
    case Value.Kind of
      rjkObject:
        Kind := $03;
      rjkArray:
        Kind := $04;
      rjkString:
        Kind := $02;
      rjkBoolean:
        Kind := $08;
      rjkNull:
        Kind := $0A;
      rjkNumber:
        begin
          Text := Value.ToJSON;
          if (Pos('.', Text) > 0) or (Pos('e', Text) > 0) or (Pos('E', Text) > 0) then
            Kind := $01
          else
          begin
            Number := Value.AsInt64;
            if (Number >= Low(Integer)) and (Number <= High(Integer)) then
              Kind := $10
            else
              Kind := $12;
          end;
        end;
    end;
  if Kind = $13 then
    raise ERALException.Create(ecBSONDecimal128Unsupported);
  WriteByte(Kind);
  WriteText(Name, True);
  case Kind of
    $01:
      begin
        if Obj <> nil then
          Text := RequireString(Obj['$numberDouble'])
        else
          Text := Value.ToJSON;
        Write64(FloatBits(Text));
      end;
    $02: WriteText(Value.AsString, False);
    $03, $04: WriteDocument(Value, Depth + 1);
    $05:
      begin
        Payload := RequireObject(Obj['$binary'], 2);
        Text := RequireString(Payload['subType']);
        if (Length(Text) < 1) or (Length(Text) > 2) then
          raise ERALException.Create(ecBSONInvalidBinarySubtype);
        SubType := HexValue(Text[1]);
        if Length(Text) = 2 then
          SubType := SubType * 16 + HexValue(Text[2]);
        Bytes := TRALBase64.DecodeAsBytes(RequireString(Payload['base64']));
        if SubType = 2 then
        begin
          if Length(Bytes) > MaxInt - 4 then
            raise EIntOverflow.Create('BSON binary is too large.');
          WriteSigned32(Length(Bytes) + 4);
        end
        else
          WriteSigned32(Length(Bytes));
        WriteByte(SubType);
        if SubType = 2 then
          WriteSigned32(Length(Bytes));
        if Length(Bytes) > 0 then
          WriteRaw(Bytes[0], Length(Bytes));
      end;
    $06:
      if (Obj['$undefined'].Kind <> rjkBoolean) or not Obj['$undefined'].AsBoolean then
        raise ERALException.Create(ecBSONInvalidUndefined);
    $07:
      begin
        Bytes := FromHex(RequireString(Obj['$oid']), 12);
        WriteRaw(Bytes[0], 12);
      end;
    $08: WriteByte(Ord(Value.AsBoolean));
    $09:
      begin
        Payload := RequireObject(Obj['$date'], 1);
        WriteSigned64(IntegerText(RequireString(Payload['$numberLong'])));
      end;
    $0A: ;
    $0B:
      begin
        Payload := RequireObject(Obj['$regularExpression'], 2);
        Text := RequireString(Payload['options']);
        ValidateRegexOptions(Text);
        WriteText(RequireString(Payload['pattern']), True);
        WriteText(Text, True);
      end;
    $0C:
      begin
        Payload := RequireObject(Obj['$dbPointer'], 2);
        WriteText(RequireString(Payload['$ref']), False);
        Payload := RequireObject(Payload['$id'], 1);
        Bytes := FromHex(RequireString(Payload['$oid']), 12);
        WriteRaw(Bytes[0], 12);
      end;
    $0D: WriteText(RequireString(Obj['$code']), False);
    $0E: WriteText(RequireString(Obj['$symbol']), False);
    $0F:
      begin
        Payload := RequireObject(Obj['$scope'], Obj['$scope'].AsObject.Count);
        Start := FStream.Position;
        Write32(0);
        WriteText(RequireString(Obj['$code']), False);
        WriteDocument(Payload, Depth + 1);
        Finish := FStream.Position;
        FStream.Position := Start;
        WriteSigned32(Finish - Start);
        FStream.Position := Finish;
      end;
    $10, $12:
      begin
        if Obj <> nil then
          Number := IntegerText(RequireString(Obj.Items[0]))
        else
          Number := Value.AsInt64;
        if Kind = $10 then
        begin
          if (Number < Low(Integer)) or (Number > High(Integer)) then
            raise ERALException.Create(ecBSONNumberIntRange);
          WriteSigned32(Number);
        end
        else
          WriteSigned64(Number);
      end;
    $11:
      begin
        Payload := RequireObject(Obj['$timestamp'], 2);
        Write32(RequireUInt32(Payload['i']));
        Write32(RequireUInt32(Payload['t']));
      end;
    $7F, $FF:
      begin
        if (Obj.Items[0].Kind <> rjkNumber) or (Obj.Items[0].AsInt64 <> 1) then
          raise ERALException.Create(ecBSONInvalidMinMaxKey);
      end;
  end;
end;

procedure TRALBSONWriter.WriteRoot(Document: TRALJSON);
var
  Finish: Int64;
begin
  if Document = nil then
    raise ERALException.Create(ecBSONRootRequired);
  if Document.Kind = rjkObject then
  begin
    WriteDocument(Document, 1);
    Exit;
  end;
  Write32(0);
  WriteElement(RALBSONRootElementKey, Document, 1);
  WriteByte(0);
  Finish := FStream.Position;
  FStream.Position := 0;
  WriteSigned32(Finish);
  FStream.Position := Finish;
end;

class function TRALBSON.Parse(
  const Bytes: TBytes;
  Canonical: Boolean;
  MaxDocumentSize: Integer;
  MaxDepth: Integer;
  UnwrapRootElement: Boolean
): TRALJSON;
var
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create(Bytes);
  try
    Result := Parse(Stream, Canonical, MaxDocumentSize, MaxDepth, UnwrapRootElement);
    if Stream.Position <> Stream.Size then
    begin
      Result.Free;
      raise ERALException.Create(ecBSONUnexpectedTrailingBytes);
    end;
  finally
    Stream.Free;
  end;
end;

class function TRALBSON.Parse(
  Stream: TStream;
  Canonical: Boolean;
  MaxDocumentSize: Integer;
  MaxDepth: Integer;
  UnwrapRootElement: Boolean
): TRALJSON;
var
  Reader: TRALBSONReader;
  Wrapper: TRALJSONObject;
begin
  Reader := TRALBSONReader.Create(Stream, Canonical, MaxDocumentSize, MaxDepth);
  try
    Result := Reader.ReadDocument(False, High(Int64), 1);
    if UnwrapRootElement then
    begin
      Wrapper := Result.AsObject;
      if (Wrapper.Count = 1) and (Wrapper.Find(RALBSONRootElementKey) <> nil) then
      begin
        Result := Wrapper.Extract(RALBSONRootElementKey);
        Wrapper.Free;
      end;
    end;
  finally
    Reader.Free;
  end;
end;

class function TRALBSON.Encode(
  Document: TRALJSON;
  InterpretExtendedJSON: Boolean;
  MaxDocumentSize: Integer;
  MaxDepth: Integer
): TBytes;
var
  Writer: TRALBSONWriter;
begin
  Result := nil;
  Writer := TRALBSONWriter.Create(InterpretExtendedJSON, MaxDocumentSize, MaxDepth);
  try
    Writer.WriteRoot(Document);
    SetLength(Result, Writer.Stream.Size);
    if Length(Result) > 0 then
      Move(Writer.Stream.Memory^, Result[0], Length(Result));
  finally
    Writer.Free;
  end;
end;

class procedure TRALBSON.SaveToStream(
  Document: TRALJSON;
  Stream: TStream;
  InterpretExtendedJSON: Boolean;
  MaxDocumentSize: Integer;
  MaxDepth: Integer
);
var
  Writer: TRALBSONWriter;
begin
  if Stream = nil then
    raise ERALException.Create(ecBSONDestinationRequired);
  Writer := TRALBSONWriter.Create(InterpretExtendedJSON, MaxDocumentSize, MaxDepth);
  try
    Writer.WriteRoot(Document);
    Writer.Stream.Position := 0;
    Stream.CopyFrom(Writer.Stream, Writer.Stream.Size);
  finally
    Writer.Free;
  end;
end;

end.
