unit RALJSON;

{$IFDEF FPC}
{$mode delphi}
{$H+}
{$ENDIF}

{$I ..\base\PascalRAL.inc}

interface

uses
  Classes,
  SysUtils,
  Contnrs,
  Variants,
  RALTypes;

type

  TRALJSON = class;
  TRALJSONObject = class;
  TRALJSONArray = class;
  TRALJSONString = class;
  TRALJSONNumber = class;
  TRALJSONBoolean = class;

  TRALJSONKind = (rjkObject, rjkArray, rjkString, rjkNumber, rjkBoolean, rjkNull);

  { Containers own their children. Free only the root returned by ParseJSON.
    FPC strings are UTF-8; Delphi strings are UTF-16. }
  TRALJSON = class abstract
  protected
    function GetKind: TRALJSONKind; virtual; abstract;
    function GetAsString: string; virtual;
    function GetAsNumber: Double; virtual;
    function GetAsInt64: Int64; virtual;
    function GetAsBoolean: Boolean; virtual;
    function GetAsObject: TRALJSONObject;
    function GetAsArray: TRALJSONArray;
    function GetIsNull: Boolean;
  public
    class function ParseJSON(const Text: string;
      MaxDocumentSize: Int64 = 0;
      MaxTokenSize: Integer = 0;
      MaxDepth: Integer = 0
    ): TRALJSON; overload; static;
    class function ParseJSON(const Bytes: TBytes;
      MaxDocumentSize: Int64 = 0;
      MaxTokenSize: Integer = 0;
      MaxDepth: Integer = 0
    ): TRALJSON; overload; static;
    class function ParseJSON(Stream: TStream;
      MaxDocumentSize: Int64 = 0;
      MaxTokenSize: Integer = 0;
      MaxDepth: Integer = 0
    ): TRALJSON; overload; static;
    class function LoadFromStream(Stream: TStream;
      MaxDocumentSize: Int64 = 0;
      MaxTokenSize: Integer = 0;
      MaxDepth: Integer = 0
    ): TRALJSON; static;
    class function DecodeUTF8(const Bytes: RawByteString): string; static;
    class function EncodeUTF8(const Text: string): RawByteString; static;
    function ToJSON: string; virtual; abstract;
    function AsInteger: Int64;
    function AsFloat: Double;
    function AsValue: Variant;
    function JsonType: TRALJSONType;
    property Kind: TRALJSONKind read GetKind;
    property AsObject: TRALJSONObject read GetAsObject;
    property AsArray: TRALJSONArray read GetAsArray;
    property AsString: string read GetAsString;
    property AsNumber: Double read GetAsNumber;
    property AsInt64: Int64 read GetAsInt64;
    property AsBoolean: Boolean read GetAsBoolean;
    property IsNull: Boolean read GetIsNull;
  end;

  { Compatibility name for values returned by ParseJSON and Get. }
  TRALJSONValue = TRALJSON;

  TRALJSONObject = class(TRALJSON)
  private
    FNames: TStringList;
    FValues: TObjectList;
    procedure AddParsed(
      const Name: string;
      Value: TRALJSON
    );
    function GetCount: Integer;
    function GetItem(Index: Integer): TRALJSON;
    function GetValue(const Name: string): TRALJSON;
  protected
    function GetKind: TRALJSONKind; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const Name: string; Value: TRALJSON): TRALJSONObject; overload;
    function Add(const Name, Value: string): TRALJSONObject; overload;
    function Add(const Name: string; Value: Int64): TRALJSONObject; overload;
    function Add(const Name: string; Value: Integer): TRALJSONObject; overload;
    function Add(const Name: string; Value: Double): TRALJSONObject; overload;
    function Add(const Name: string; Value: Boolean): TRALJSONObject; overload;
    function Add(const Name: string): TRALJSONObject; overload;
    { Returned values remain owned by this container. }
    function Get(const Name: string): TRALJSON; overload;
    function Get(Index: Integer): TRALJSON; overload;
    function GetName(Index: Integer): string;
    { Transfers ownership to the caller; returns nil when absent. }
    function Extract(const Name: string): TRALJSON;
    function Find(const Name: string): TRALJSON;
    function ToJSON: string; override;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName;
    property Items[Index: Integer]: TRALJSON read GetItem;
    property Values[const Name: string]: TRALJSON read GetValue; default;
  end;

  TRALJSONArray = class(TRALJSON)
  private
    FValues: TObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TRALJSON;
  protected
    function GetKind: TRALJSONKind; override;
  public
    constructor Create;
    function Add(Value: TRALJSON): TRALJSONArray; overload;
    function Add(const Value: string): TRALJSONArray; overload;
    function Add(Value: Int64): TRALJSONArray; overload;
    function Add(Value: Integer): TRALJSONArray; overload;
    function Add(Value: Double): TRALJSONArray; overload;
    function Add(Value: Boolean): TRALJSONArray; overload;
    function Add: TRALJSONArray; overload;
    { Returned values remain owned by this container. }
    function Get(Index: Integer): TRALJSON;
    destructor Destroy; override;
    function ToJSON: string; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TRALJSON read GetItem; default;
  end;

  TRALJSONString = class(TRALJSON)
  private
    FValue: string;
  protected
    function GetKind: TRALJSONKind; override;
    function GetAsString: string; override;
  public
    constructor Create(const Value: string);
    function ToJSON: string; override;
    property Value: string read FValue;
  end;

  TRALJSONNumber = class(TRALJSON)
  private
    FValue: string;
  protected
    function GetKind: TRALJSONKind; override;
    function GetAsNumber: Double; override;
    function GetAsInt64: Int64; override;
  public
    { Value retains the exact JSON number, without rounding large integers. }
    constructor Create(const Value: string);
    function ToJSON: string; override;
    property Value: string read FValue;
  end;

  TRALJSONBoolean = class(TRALJSON)
  private
    FValue: Boolean;
  protected
    function GetKind: TRALJSONKind; override;
    function GetAsBoolean: Boolean; override;
  public
    constructor Create(Value: Boolean);
    function ToJSON: string; override;
    property Value: Boolean read FValue;
  end;

  TRALJSONNull = class(TRALJSON)
  protected
    function GetKind: TRALJSONKind; override;
  public
    function ToJSON: string; override;
  end;

  TRALJSONTokenKind = (rjtkNone, rjtkString, rjtkNumber, rjtkLiteral);
  TRALJSONExpectation = (
    rjeObjectKeyOrEnd,
    rjeObjectKey,
    rjeObjectColon,
    rjeObjectValue,
    rjeObjectCommaOrEnd,
    rjeArrayValueOrEnd,
    rjeArrayValue,
    rjeArrayCommaOrEnd
  );

  TRALJSONParserFrame = record
    Container: TRALJSON;
    Expectation: TRALJSONExpectation;
    MemberName: string;
  end;

  { Feed UTF-8 bytes through Update; Finish transfers the root to the caller.
    Reset is required after Finish or a parsing error. No input block is retained. }
  TRALJSONParser = class
  private
    FRoot: TRALJSON;
    FFrames: array of TRALJSONParserFrame;
    FDepth: Integer;
    FMaxDocumentSize: Int64;
    FMaxTokenSize: Integer;
    FMaxDepth: Integer;
    FTokenKind: TRALJSONTokenKind;
    FToken: RawByteString;
    FTokenLength: Integer;
    FLiteral: RawByteString;
    FEscaped: Boolean;
    FTokenHasEscape: Boolean;
    FTokenIsASCII: Boolean;
    FBOMLength: Integer;
    FTotalBytes: Int64;
    FFinished: Boolean;
    FFailed: Boolean;
    procedure SetMaxDocumentSize(Value: Int64);
    procedure SetMaxTokenSize(Value: Integer);
    procedure SetMaxDepth(Value: Integer);
    procedure CheckLimitChange;
    procedure EnsureActive;
    procedure Fail(const MessageText: string);
    procedure AppendToken(Value: Byte);
    function TokenText(
      Start: Integer;
      Count: Integer
    ): string;
    procedure CompleteToken;
    procedure AddValue(Value: TRALJSON);
    procedure StartContainer(IsObject: Boolean);
    procedure CloseContainer(IsObject: Boolean);
    procedure Consume(Value: Byte);
  public
    constructor Create(
      MaxDocumentSize: Int64 = 0;
      MaxTokenSize: Integer = 0;
      MaxDepth: Integer = 0
    );
    destructor Destroy; override;
    procedure Reset;
    procedure Update(
      const Buffer;
      Count: NativeInt
    ); overload;
    procedure Update(const Bytes: RawByteString); overload;
    function Finish: TRALJSON;
    { Zero disables a limit. Configure before Update or after Reset. }
    property MaxDocumentSize: Int64 read FMaxDocumentSize write SetMaxDocumentSize;
    property MaxTokenSize: Integer read FMaxTokenSize write SetMaxTokenSize;
    property MaxDepth: Integer read FMaxDepth write SetMaxDepth;
    property TotalBytes: Int64 read FTotalBytes;
  end;

implementation

uses
  Math;

type
  { Grow geometrically rather than copying an entire container for each item. }
  TRALJSONTextBuffer = record
    Text: string;
    Count: Integer;
    procedure Append(const Value: string);
    function Finish: string;
  end;

  TRALJSONTokenReader = class
  private
    FText: UnicodeString;
    FPosition: Integer;
    function Peek: WideChar;
    procedure Fail(const MessageText: string);
    procedure Expect(Value: WideChar);
    function ReadString: string;
    function ReadHex: Word;
  public
    constructor Create(const Text: UnicodeString);
  end;

procedure ValidateJSONNumber(const Value: string);
var
  Position: Integer;

  function Peek: Char;
  begin
    if Position > Length(Value) then
      Result := #0
    else
      Result := Value[Position];
  end;

  procedure ReadDigits;
  begin
    if (Peek < '0') or (Peek > '9') then
      raise ERALException.Create('Missing JSON number digits.');
    repeat
      Inc(Position);
    until (Peek < '0') or (Peek > '9');
  end;

begin
  Position := 1;
  if Peek = '-' then
    Inc(Position);
  if Peek = '0' then
    Inc(Position)
  else
  begin
    if (Peek < '1') or (Peek > '9') then
      raise ERALException.Create('Invalid JSON number.');
    ReadDigits;
  end;
  if Peek = '.' then
  begin
    Inc(Position);
    ReadDigits;
  end;
  if (Peek = 'e') or (Peek = 'E') then
  begin
    Inc(Position);
    if (Peek = '+') or (Peek = '-') then
      Inc(Position);
    ReadDigits;
  end;
  if Position <= Length(Value) then
    raise ERALException.Create('Invalid JSON number.');
end;

function NativeText(const Text: UnicodeString): string;
begin
  {$IFDEF FPC}
  Result := UTF8Encode(Text);
  {$ELSE}
  Result := Text;
  {$ENDIF}
end;

function DecodeUTF8Range(
  const Bytes: RawByteString;
  Start: Integer;
  CountBytes: Integer
): UnicodeString;
var
  I: Integer;
  Last: Integer;
  J: Integer;
  Count: Integer;
  Written: Integer;
  First: Cardinal;
  CodePoint: Cardinal;
  Minimum: Cardinal;
  NextByte: Cardinal;
begin
  SetLength(Result, CountBytes);
  Last := Start + CountBytes - 1;
  I := Start;
  Written := 0;
  while I <= Last do
  begin
    First := Ord(Bytes[I]);
    Inc(I);
    Count := 0;
    Minimum := 0;
    if First < $80 then
      CodePoint := First
    else if (First >= $C2) and (First <= $DF) then
    begin
      Count := 1;
      CodePoint := First and $1F;
      Minimum := $80;
    end
    else if (First >= $E0) and (First <= $EF) then
    begin
      Count := 2;
      CodePoint := First and $0F;
      Minimum := $800;
    end
    else if (First >= $F0) and (First <= $F4) then
    begin
      Count := 3;
      CodePoint := First and $07;
      Minimum := $10000;
    end
    else
      raise ERALException.Create('Invalid UTF-8 leading byte.');
    for J := 1 to Count do
    begin
      if I > Last then
        raise ERALException.Create('Incomplete UTF-8 sequence.');
      NextByte := Ord(Bytes[I]);
      Inc(I);
      if (NextByte < $80) or (NextByte > $BF) then
        raise ERALException.Create('Invalid UTF-8 continuation byte.');
      CodePoint := (CodePoint shl 6) or (NextByte and $3F);
    end;
    if (CodePoint < Minimum) or (CodePoint > $10FFFF) or
       ((CodePoint >= $D800) and (CodePoint <= $DFFF)) then
      raise ERALException.Create('Invalid UTF-8 code point.');
    Inc(Written);
    if CodePoint < $10000 then
      Result[Written] := WideChar(CodePoint)
    else
    begin
      CodePoint := CodePoint - $10000;
      Result[Written] := WideChar($D800 or (CodePoint shr 10));
      Inc(Written);
      Result[Written] := WideChar($DC00 or (CodePoint and $3FF));
    end;
  end;
  SetLength(Result, Written);
end;

function DecodeUTF8(const Bytes: RawByteString): UnicodeString;
begin
  Result := DecodeUTF8Range(Bytes, 1, Length(Bytes));
end;

function UnicodeText(const Text: string): UnicodeString;
begin
  {$IFDEF FPC}
  Result := DecodeUTF8(RawByteString(Text));
  {$ELSE}
  Result := Text;
  {$ENDIF}
end;

procedure TRALJSONTextBuffer.Append(const Value: string);
var
  Needed: Integer;
  Capacity: Integer;
begin
  if Length(Value) > MaxInt - Count then
    raise EIntOverflow.Create('JSON output is too large.');
  Needed := Count + Length(Value);
  if NativeInt(Needed) > High(NativeInt) div SizeOf(Char) then
    raise EIntOverflow.Create('JSON output exceeds the native buffer capacity.');
  if Needed > Length(Text) then
  begin
    Capacity := Length(Text);
    if Capacity < 256 then
      Capacity := 256;
    while Capacity < Needed do
      if Capacity > MaxInt div 2 then
        Capacity := Needed
      else
        Capacity := Capacity * 2;
    if NativeInt(Capacity) > High(NativeInt) div SizeOf(Char) then
      Capacity := Needed;
    SetLength(Text, Capacity);
  end;
  if Length(Value) > 0 then
    Move(Value[1], Text[Count + 1], NativeInt(Length(Value)) * SizeOf(Char));
  Count := Needed;
end;

function TRALJSONTextBuffer.Finish: string;
begin
  SetLength(Text, Count);
  Result := Text;
end;

function QuoteText(const Text: string): string;
const
  HexDigits: string = '0123456789ABCDEF';
var
  Source: UnicodeString;
  Encoded: UnicodeString;
  I: Integer;
  Written: Integer;
  Required: Integer;
  Code: Word;
  PlainASCII: Boolean;
begin
  if Length(Text) > MaxInt - 2 then
    raise EIntOverflow.Create('JSON string is too large.');
  PlainASCII := True;
  for I := 1 to Length(Text) do
    if (Ord(Text[I]) < 32) or (Ord(Text[I]) >= 128) or
       (Text[I] = '"') or (Text[I] = '\') then
    begin
      PlainASCII := False;
      Break;
    end;
  if PlainASCII then
  begin
    Result := '"' + Text + '"';
    Exit;
  end;
  Source := UnicodeText(Text);
  Required := 2;
  I := 1;
  while I <= Length(Source) do
  begin
    Code := Ord(Source[I]);
    if Required > MaxInt - 6 then
      raise EIntOverflow.Create('JSON string is too large.');
    case Code of
      0..31: Inc(Required, 6);
      34, 92: Inc(Required, 2);
      $D800..$DBFF:
        begin
          if I = Length(Source) then
            raise ERALException.Create('Unpaired UTF-16 surrogate.');
          if (Ord(Source[I + 1]) < $DC00) or (Ord(Source[I + 1]) > $DFFF) then
            raise ERALException.Create('Unpaired UTF-16 surrogate.');
          Inc(I);
          Inc(Required, 2);
        end;
      $DC00..$DFFF:
        raise ERALException.Create('Unpaired UTF-16 surrogate.');
    else
      Inc(Required);
    end;
    Inc(I);
  end;
  if NativeInt(Required) > High(NativeInt) div SizeOf(WideChar) then
    raise EIntOverflow.Create('JSON string exceeds the native buffer capacity.');
  SetLength(Encoded, Required);
  Encoded[1] := '"';
  Written := 1;
  for I := 1 to Length(Source) do
  begin
    Code := Ord(Source[I]);
    if Code < 32 then
    begin
      Encoded[Written + 1] := '\';
      Encoded[Written + 2] := 'u';
      Encoded[Written + 3] := '0';
      Encoded[Written + 4] := '0';
      Encoded[Written + 5] := WideChar(HexDigits[(Code shr 4) + 1]);
      Encoded[Written + 6] := WideChar(HexDigits[(Code and 15) + 1]);
      Inc(Written, 6);
    end
    else
    begin
      if (Code = 34) or (Code = 92) then
      begin
        Inc(Written);
        Encoded[Written] := '\';
      end;
      Inc(Written);
      Encoded[Written] := Source[I];
    end;
  end;
  Encoded[Written + 1] := '"';
  Result := NativeText(Encoded);
end;

class function TRALJSON.DecodeUTF8(const Bytes: RawByteString): string;
begin
  Result := NativeText(DecodeUTF8Range(Bytes, 1, Length(Bytes)));
end;

class function TRALJSON.EncodeUTF8(const Text: string): RawByteString;
begin
  Result := UTF8Encode(UnicodeText(Text));
end;

class function TRALJSON.ParseJSON(const Text: string;
  MaxDocumentSize: Int64;
  MaxTokenSize: Integer;
  MaxDepth: Integer
): TRALJSON;
var
  Parser: TRALJSONParser;
  Bytes: RawByteString;
  {$IFNDEF FPC}
  I: Integer;
  {$ENDIF}
begin
  {$IFDEF FPC}
  Bytes := RawByteString(Text);
  {$ELSE}
  I := 1;
  while I <= Length(Text) do
  begin
    if (Ord(Text[I]) >= $D800) and (Ord(Text[I]) <= $DBFF) then
    begin
      Inc(I);
      if I > Length(Text) then
        raise ERALException.Create('Unpaired UTF-16 surrogate.');
      if (Ord(Text[I]) < $DC00) or (Ord(Text[I]) > $DFFF) then
        raise ERALException.Create('Unpaired UTF-16 surrogate.');
    end
    else if (Ord(Text[I]) >= $DC00) and (Ord(Text[I]) <= $DFFF) then
      raise ERALException.Create('Unpaired UTF-16 surrogate.');
    Inc(I);
  end;
  Bytes := UTF8Encode(Text);
  {$ENDIF}
  Parser := TRALJSONParser.Create(MaxDocumentSize, MaxTokenSize, MaxDepth);
  try
    Parser.Update(Bytes);
    Result := Parser.Finish;
  finally
    Parser.Free;
  end;
end;

class function TRALJSON.ParseJSON(const Bytes: TBytes;
  MaxDocumentSize: Int64;
  MaxTokenSize: Integer;
  MaxDepth: Integer
): TRALJSON;
var
  Parser: TRALJSONParser;
begin
  Parser := TRALJSONParser.Create(MaxDocumentSize, MaxTokenSize, MaxDepth);
  try
    if Length(Bytes) > 0 then
      Parser.Update(Bytes[0], Length(Bytes));
    Result := Parser.Finish;
  finally
    Parser.Free;
  end;
end;

class function TRALJSON.ParseJSON(Stream: TStream;
  MaxDocumentSize: Int64;
  MaxTokenSize: Integer;
  MaxDepth: Integer
): TRALJSON;
begin
  Result := LoadFromStream(Stream, MaxDocumentSize, MaxTokenSize, MaxDepth);
end;

class function TRALJSON.LoadFromStream(Stream: TStream;
  MaxDocumentSize: Int64;
  MaxTokenSize: Integer;
  MaxDepth: Integer
): TRALJSON;
var
  Buffer: array[0..8191] of Byte;
  Count: Integer;
  Parser: TRALJSONParser;
begin
  if Stream = nil then
    raise ERALException.Create('JSON stream cannot be nil.');
  Parser := TRALJSONParser.Create(MaxDocumentSize, MaxTokenSize, MaxDepth);
  try
    repeat
      Count := Stream.Read(Buffer, SizeOf(Buffer));
      if Count < 0 then
        raise ERALException.Create('Failed to read JSON stream.');
      if Count = 0 then
        Break;
      Parser.Update(Buffer, Count);
    until False;
    Result := Parser.Finish;
  finally
    Parser.Free;
  end;
end;

function ConversionNumberText(const Value: string): string;
begin
  Result := Trim(Value);
  try
    ValidateJSONNumber(Result);
  except
    on E: ERALException do
      raise ERALException.Create('JSON string cannot be converted to a number.');
  end;
end;

function ConversionDouble(const Value: string): Double;
var
  Text: string;
  Settings: TFormatSettings;
begin
  Text := ConversionNumberText(Value);
  {$IFDEF FPC}
  Settings := DefaultFormatSettings;
  {$ELSE}
  Settings := TFormatSettings.Create;
  {$ENDIF}
  Settings.DecimalSeparator := '.';
  Settings.ThousandSeparator := #0;
  if not TryStrToFloat(Text, Result, Settings) then
    raise ERALException.Create('JSON value is outside the Double range.');
  if IsNan(Result) or IsInfinite(Result) then
    raise ERALException.Create('JSON value is outside the Double range.');
end;

function ConversionBoolean(const Value: string): Boolean;
var
  Text: string;
  I: Integer;
begin
  Text := Trim(Value);
  if SameText(Text, 'true') then
    Exit(True);
  if SameText(Text, 'false') then
    Exit(False);
  Text := ConversionNumberText(Text);
  { Inspect the mantissa, without losing precision or overflowing Double. }
  Result := False;
  for I := 1 to Length(Text) do
  begin
    if (Text[I] = 'e') or (Text[I] = 'E') then
      Break;
    if (Text[I] >= '1') and (Text[I] <= '9') then
      Exit(True);
  end;
end;

function TRALJSON.GetAsString: string;
begin
  case Kind of
    rjkObject, rjkArray, rjkNumber, rjkBoolean:
      Result := ToJSON;
    rjkNull:
      Result := '';
  else
    raise ERALException.Create('JSON value cannot be converted to a string.');
  end;
end;

function TRALJSON.GetAsNumber: Double;
begin
  case Kind of
    rjkString:
      Result := ConversionDouble(AsString);
    rjkBoolean:
      Result := Ord(AsBoolean);
  else
    raise ERALException.Create('JSON value cannot be converted to a number.');
  end;
end;

function TRALJSON.GetAsInt64: Int64;
var
  Text: string;
begin
  case Kind of
    rjkString:
      begin
        Text := ConversionNumberText(AsString);
        if not TryStrToInt64(Text, Result) then
          raise ERALException.Create('JSON string is not an Int64 integer.');
      end;
    rjkBoolean:
      Result := Ord(AsBoolean);
  else
    raise ERALException.Create('JSON value cannot be converted to Int64.');
  end;
end;

function TRALJSON.GetAsBoolean: Boolean;
begin
  case Kind of
    rjkString, rjkNumber:
      Result := ConversionBoolean(AsString);
  else
    raise ERALException.Create('JSON value cannot be converted to a boolean.');
  end;
end;

function TRALJSON.GetAsObject: TRALJSONObject;
begin
  if not (Self is TRALJSONObject) then
    raise ERALException.Create('JSON value is not an object.');
  Result := TRALJSONObject(Self);
end;

function TRALJSON.GetAsArray: TRALJSONArray;
begin
  if not (Self is TRALJSONArray) then
    raise ERALException.Create('JSON value is not an array.');
  Result := TRALJSONArray(Self);
end;

function TRALJSON.GetIsNull: Boolean;
begin
  Result := (Self = nil) or (Kind = rjkNull);
end;

function TRALJSON.AsInteger: Int64;
begin
  if (Self = nil) or IsNull then
    Result := -1
  else
    try
      Result := AsInt64;
    except
      on E: ERALException do Result := -1;
    end;
end;

function TRALJSON.AsFloat: Double;
begin
  if (Self = nil) or IsNull then
    Result := -1
  else
    try
      Result := AsNumber;
    except
      on E: ERALException do Result := -1;
    end;
end;

function TRALJSON.AsValue: Variant;
begin
  if (Self = nil) or IsNull then
    Result := Variants.Null
  else
    Result := AsString;
end;

function TRALJSON.JsonType: TRALJSONType;
begin
  Result := rjtString;
  if Self = nil then
    Exit;
  case Kind of
    rjkObject: Result := rjtObject;
    rjkArray: Result := rjtArray;
    rjkNumber: Result := rjtNumber;
    rjkBoolean: Result := rjtBoolean;
  end;
end;

function JSONFloatText(Value: Double): string;
var
  Settings: TFormatSettings;
begin
  {$IFDEF FPC}
  Settings := DefaultFormatSettings;
  {$ELSE}
  Settings := TFormatSettings.Create;
  {$ENDIF}
  Settings.DecimalSeparator := '.';
  Settings.ThousandSeparator := #0;
  Result := FloatToStr(Value, Settings);
end;

function TRALJSONObject.Add(const Name: string; const Value: string): TRALJSONObject;
var
  Child: TRALJSON;
begin
  Child := TRALJSONString.Create(Value);
  try
    Result := Add(Name, Child);
  except
    Child.Free;
    raise;
  end;
end;

function TRALJSONObject.Add(const Name: string; Value: Int64): TRALJSONObject;
var
  Child: TRALJSON;
begin
  Child := TRALJSONNumber.Create(IntToStr(Value));
  try
    Result := Add(Name, Child);
  except
    Child.Free;
    raise;
  end;
end;

function TRALJSONObject.Add(const Name: string; Value: Integer): TRALJSONObject;
var
  Child: TRALJSON;
begin
  Child := TRALJSONNumber.Create(IntToStr(Value));
  try
    Result := Add(Name, Child);
  except
    Child.Free;
    raise;
  end;
end;

function TRALJSONObject.Add(const Name: string; Value: Double): TRALJSONObject;
var
  Child: TRALJSON;
begin
  Child := TRALJSONNumber.Create(JSONFloatText(Value));
  try
    Result := Add(Name, Child);
  except
    Child.Free;
    raise;
  end;
end;

function TRALJSONObject.Add(const Name: string; Value: Boolean): TRALJSONObject;
var
  Child: TRALJSON;
begin
  Child := TRALJSONBoolean.Create(Value);
  try
    Result := Add(Name, Child);
  except
    Child.Free;
    raise;
  end;
end;

function TRALJSONObject.Add(const Name: string): TRALJSONObject;
var
  Child: TRALJSON;
begin
  Child := TRALJSONNull.Create;
  try
    Result := Add(Name, Child);
  except
    Child.Free;
    raise;
  end;
end;

function TRALJSONArray.Add(const Value: string): TRALJSONArray;
var
  Child: TRALJSON;
begin
  Child := TRALJSONString.Create(Value);
  try
    Result := Add(Child);
  except
    Child.Free;
    raise;
  end;
end;

function TRALJSONArray.Add(Value: Int64): TRALJSONArray;
var
  Child: TRALJSON;
begin
  Child := TRALJSONNumber.Create(IntToStr(Value));
  try
    Result := Add(Child);
  except
    Child.Free;
    raise;
  end;
end;

function TRALJSONArray.Add(Value: Integer): TRALJSONArray;
var
  Child: TRALJSON;
begin
  Child := TRALJSONNumber.Create(IntToStr(Value));
  try
    Result := Add(Child);
  except
    Child.Free;
    raise;
  end;
end;

function TRALJSONArray.Add(Value: Double): TRALJSONArray;
var
  Child: TRALJSON;
begin
  Child := TRALJSONNumber.Create(JSONFloatText(Value));
  try
    Result := Add(Child);
  except
    Child.Free;
    raise;
  end;
end;

function TRALJSONArray.Add(Value: Boolean): TRALJSONArray;
var
  Child: TRALJSON;
begin
  Child := TRALJSONBoolean.Create(Value);
  try
    Result := Add(Child);
  except
    Child.Free;
    raise;
  end;
end;

function TRALJSONArray.Add: TRALJSONArray;
var
  Child: TRALJSON;
begin
  Child := TRALJSONNull.Create;
  try
    Result := Add(Child);
  except
    Child.Free;
    raise;
  end;
end;

function TRALJSONObject.Get(const Name: string): TRALJSON;
begin
  if Self = nil then
    Result := nil
  else
    Result := Find(Name);
end;

function TRALJSONObject.Get(Index: Integer): TRALJSON;
begin
  if Self = nil then
    Result := nil
  else
    Result := Items[Index];
end;

function TRALJSONArray.Get(Index: Integer): TRALJSON;
begin
  if Self = nil then
    Result := nil
  else
    Result := Items[Index];
end;

constructor TRALJSONObject.Create;
begin
  inherited Create;
  FNames := TStringList.Create;
  FValues := TObjectList.Create(True);
end;

destructor TRALJSONObject.Destroy;
begin
  FValues.Free;
  FNames.Free;
  inherited;
end;

function TRALJSONObject.Add(const Name: string; Value: TRALJSON): TRALJSONObject;
begin
  if Value = nil then
  begin
    Result := Add(Name);
    Exit;
  end;
  if Value = Self then
    raise ERALException.Create('Invalid JSON child.');
  if Find(Name) <> nil then
    raise ERALException.Create('Duplicate JSON member: ' + Name);
  AddParsed(Name, Value);
  Result := Self;
end;

procedure TRALJSONObject.AddParsed(
  const Name: string;
  Value: TRALJSON
);
begin
  { Private parser insertion: CompleteToken already checked this key. }
  FNames.Add(Name);
  try
    FValues.Add(Value);
  except
    FNames.Delete(FNames.Count - 1);
    raise;
  end;
end;

function TRALJSONObject.GetKind: TRALJSONKind;
begin
  Result := rjkObject;
end;

function TRALJSONObject.GetCount: Integer;
begin
  if Self = nil then
    Result := 0
  else
    Result := FValues.Count;
end;

function TRALJSONObject.GetName(Index: Integer): string;
begin
  if Self = nil then
    Result := ''
  else
    Result := FNames[Index];
end;

function TRALJSONObject.GetItem(Index: Integer): TRALJSON;
begin
  Result := TRALJSON(FValues[Index]);
end;

function TRALJSONObject.Extract(const Name: string): TRALJSON;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if FNames[I] = Name then
    begin
      Result := TRALJSON(FValues.Extract(FValues[I]));
      FNames.Delete(I);
      Exit;
    end;
end;

function TRALJSONObject.Find(const Name: string): TRALJSON;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if FNames[I] = Name then
    begin
      Result := Items[I];
      Exit;
    end;
end;

function TRALJSONObject.GetValue(const Name: string): TRALJSON;
begin
  Result := Find(Name);
  if Result = nil then
    raise ERALException.Create('JSON member not found: ' + Name);
end;

function TRALJSONObject.ToJSON: string;
var
  I: Integer;
  Buffer: TRALJSONTextBuffer;
begin
  Buffer.Count := 0;
  Buffer.Append('{');
  for I := 0 to Count - 1 do
  begin
    if I > 0 then
      Buffer.Append(',');
    Buffer.Append(QuoteText(Names[I]));
    Buffer.Append(':');
    Buffer.Append(Items[I].ToJSON);
  end;
  Buffer.Append('}');
  Result := Buffer.Finish;
end;

constructor TRALJSONArray.Create;
begin
  inherited Create;
  FValues := TObjectList.Create(True);
end;

destructor TRALJSONArray.Destroy;
begin
  FValues.Free;
  inherited;
end;

function TRALJSONArray.Add(Value: TRALJSON): TRALJSONArray;
begin
  if Value = nil then
  begin
    Result := Add;
    Exit;
  end;
  if Value = Self then
    raise ERALException.Create('Invalid JSON child.');
  FValues.Add(Value);
  Result := Self;
end;

function TRALJSONArray.GetKind: TRALJSONKind;
begin
  Result := rjkArray;
end;

function TRALJSONArray.GetCount: Integer;
begin
  if Self = nil then
    Result := 0
  else
    Result := FValues.Count;
end;

function TRALJSONArray.GetItem(Index: Integer): TRALJSON;
begin
  Result := TRALJSON(FValues[Index]);
end;

function TRALJSONArray.ToJSON: string;
var
  I: Integer;
  Buffer: TRALJSONTextBuffer;
begin
  Buffer.Count := 0;
  Buffer.Append('[');
  for I := 0 to Count - 1 do
  begin
    if I > 0 then
      Buffer.Append(',');
    Buffer.Append(Items[I].ToJSON);
  end;
  Buffer.Append(']');
  Result := Buffer.Finish;
end;

constructor TRALJSONString.Create(const Value: string);
begin
  inherited Create;
  FValue := Value;
end;

function TRALJSONString.GetKind: TRALJSONKind;
begin
  Result := rjkString;
end;

function TRALJSONString.GetAsString: string;
begin
  Result := FValue;
end;

function TRALJSONString.ToJSON: string;
begin
  Result := QuoteText(FValue);
end;

constructor TRALJSONNumber.Create(const Value: string);
begin
  inherited Create;
  ValidateJSONNumber(Value);
  FValue := Value;
end;

function TRALJSONNumber.GetKind: TRALJSONKind;
begin
  Result := rjkNumber;
end;

function TRALJSONNumber.GetAsNumber: Double;
var
  Settings: TFormatSettings;
begin
  {$IFDEF FPC}
  Settings := DefaultFormatSettings;
  {$ELSE}
  Settings := TFormatSettings.Create;
  {$ENDIF}
  Settings.DecimalSeparator := '.';
  Settings.ThousandSeparator := #0;
  if not TryStrToFloat(FValue, Result, Settings) then
    raise ERALException.Create('JSON number is outside the Double range.');
  if IsNan(Result) or IsInfinite(Result) then
    raise ERALException.Create('JSON number is outside the Double range.');
end;

function TRALJSONNumber.GetAsInt64: Int64;
begin
  if not TryStrToInt64(FValue, Result) then
    raise ERALException.Create('JSON number is not an Int64 integer.');
end;

function TRALJSONNumber.ToJSON: string;
begin
  Result := FValue;
end;

constructor TRALJSONBoolean.Create(Value: Boolean);
begin
  inherited Create;
  FValue := Value;
end;

function TRALJSONBoolean.GetKind: TRALJSONKind;
begin
  Result := rjkBoolean;
end;

function TRALJSONBoolean.GetAsBoolean: Boolean;
begin
  Result := FValue;
end;

function TRALJSONBoolean.ToJSON: string;
begin
  if FValue then
    Result := 'true'
  else
    Result := 'false';
end;

function TRALJSONNull.GetKind: TRALJSONKind;
begin
  Result := rjkNull;
end;

function TRALJSONNull.ToJSON: string;
begin
  Result := 'null';
end;

constructor TRALJSONTokenReader.Create(const Text: UnicodeString);
begin
  inherited Create;
  FText := Text;
  FPosition := 1;
end;

function TRALJSONTokenReader.Peek: WideChar;
begin
  if FPosition > Length(FText) then
    Result := #0
  else
    Result := FText[FPosition];
end;

procedure TRALJSONTokenReader.Fail(const MessageText: string);
begin
  raise ERALException.CreateFmt('%s Position: %d.', [MessageText, FPosition]);
end;

procedure TRALJSONTokenReader.Expect(Value: WideChar);
begin
  if Peek <> Value then
    Fail('Unexpected JSON character');
  Inc(FPosition);
end;

function TRALJSONTokenReader.ReadHex: Word;
var
  I: Integer;
  Digit: Integer;
  C: WideChar;
begin
  Result := 0;
  for I := 1 to 4 do
  begin
    C := Peek;
    if (C >= '0') and (C <= '9') then
      Digit := Ord(C) - Ord('0')
    else if (C >= 'A') and (C <= 'F') then
      Digit := Ord(C) - Ord('A') + 10
    else if (C >= 'a') and (C <= 'f') then
      Digit := Ord(C) - Ord('a') + 10
    else
    begin
      Fail('Invalid Unicode escape');
      Digit := 0;
    end;
    Result := (Result shl 4) or Digit;
    Inc(FPosition);
  end;
end;

function TRALJSONTokenReader.ReadString: string;
var
  Text: UnicodeString;
  C: WideChar;
  HighSurrogate: Word;
  LowSurrogate: Word;
  Written: Integer;

  procedure AppendChar(Value: WideChar);
  begin
    Inc(Written);
    Text[Written] := Value;
  end;

begin
  Expect('"');
  SetLength(Text, Length(FText));
  Written := 0;
  while Peek <> '"' do
  begin
    if (FPosition > Length(FText)) or (Ord(Peek) < 32) then
      Fail('Unterminated string or unescaped control character');
    C := Peek;
    Inc(FPosition);
    if C = '\' then
    begin
      C := Peek;
      Inc(FPosition);
      case C of
        '"', '\', '/':
          AppendChar(C);
        'b':
          AppendChar(#8);
        'f':
          AppendChar(#12);
        'n':
          AppendChar(#10);
        'r':
          AppendChar(#13);
        't':
          AppendChar(#9);
        'u':
          begin
            HighSurrogate := ReadHex;
            if (HighSurrogate >= $D800) and (HighSurrogate <= $DBFF) then
            begin
              Expect('\');
              Expect('u');
              LowSurrogate := ReadHex;
              if (LowSurrogate < $DC00) or (LowSurrogate > $DFFF) then
                Fail('Unpaired Unicode surrogate');
              AppendChar(WideChar(HighSurrogate));
              AppendChar(WideChar(LowSurrogate));
            end
            else if (HighSurrogate >= $DC00) and (HighSurrogate <= $DFFF) then
              Fail('Unpaired Unicode surrogate')
            else
              AppendChar(WideChar(HighSurrogate));
          end;
      else
        Fail('Invalid JSON escape');
      end;
    end
    else if (Ord(C) >= $D800) and (Ord(C) <= $DBFF) then
    begin
      if (Ord(Peek) < $DC00) or (Ord(Peek) > $DFFF) then
        Fail('Unpaired Unicode surrogate');
      AppendChar(C);
      AppendChar(Peek);
      Inc(FPosition);
    end
    else if (Ord(C) >= $DC00) and (Ord(C) <= $DFFF) then
      Fail('Unpaired Unicode surrogate')
    else
      AppendChar(C);
  end;
  Inc(FPosition);
  SetLength(Text, Written);
  Result := NativeText(Text);
end;

constructor TRALJSONParser.Create(
  MaxDocumentSize: Int64;
  MaxTokenSize: Integer;
  MaxDepth: Integer
);
begin
  inherited Create;
  SetMaxDocumentSize(MaxDocumentSize);
  SetMaxTokenSize(MaxTokenSize);
  SetMaxDepth(MaxDepth);
  Reset;
end;

procedure TRALJSONParser.CheckLimitChange;
begin
  if (FTotalBytes <> 0) or FFinished or FFailed then
    raise ERALException.Create('Call Reset before changing JSON limits.');
end;

procedure TRALJSONParser.SetMaxDocumentSize(Value: Int64);
begin
  CheckLimitChange;
  if Value < 0 then
    raise ERALException.Create('MaxDocumentSize cannot be negative.');
  FMaxDocumentSize := Value;
end;

procedure TRALJSONParser.SetMaxTokenSize(Value: Integer);
begin
  CheckLimitChange;
  if Value < 0 then
    raise ERALException.Create('MaxTokenSize cannot be negative.');
  FMaxTokenSize := Value;
end;

procedure TRALJSONParser.SetMaxDepth(Value: Integer);
begin
  CheckLimitChange;
  if Value < 0 then
    raise ERALException.Create('MaxDepth cannot be negative.');
  FMaxDepth := Value;
end;

destructor TRALJSONParser.Destroy;
begin
  FRoot.Free;
  inherited;
end;

procedure TRALJSONParser.Reset;
var
  I: Integer;
begin
  FreeAndNil(FRoot);
  for I := 0 to FDepth - 1 do
  begin
    FFrames[I].Container := nil;
    FFrames[I].MemberName := '';
  end;
  FDepth := 0;
  FTokenKind := rjtkNone;
  FToken := '';
  FTokenLength := 0;
  FLiteral := '';
  FEscaped := False;
  FTokenHasEscape := False;
  FTokenIsASCII := True;
  FBOMLength := 0;
  FTotalBytes := 0;
  FFinished := False;
  FFailed := False;
end;

procedure TRALJSONParser.EnsureActive;
begin
  if FFinished then
    raise ERALException.Create('Parser already finished. Call Reset before reuse.');
  if FFailed then
    raise ERALException.Create('Parser failed. Call Reset before reuse.');
end;

procedure TRALJSONParser.Fail(const MessageText: string);
begin
  raise ERALException.CreateFmt('%s Byte position: %d.', [MessageText, FTotalBytes]);
end;

procedure TRALJSONParser.AppendToken(Value: Byte);
var
  Capacity: Integer;
begin
  if (FMaxTokenSize > 0) and (FTokenLength >= FMaxTokenSize) then
    Fail('JSON token exceeds MaxTokenSize');
  if FTokenLength = MaxInt then
    raise EIntOverflow.Create('JSON token exceeds the string index capacity.');
  if FTokenLength = Length(FToken) then
  begin
    if Length(FToken) > MaxInt div 2 then
      Capacity := MaxInt
    else
      Capacity := Length(FToken) * 2;
    if Capacity < 256 then
      Capacity := 256;
    if (FMaxTokenSize > 0) and (Capacity > FMaxTokenSize) then
      Capacity := FMaxTokenSize;
    SetLength(FToken, Capacity);
  end;
  if Value >= 128 then
    FTokenIsASCII := False;
  if Value = Ord('\') then
    FTokenHasEscape := True;
  Inc(FTokenLength);
  FToken[FTokenLength] := AnsiChar(Value);
end;

procedure TRALJSONParser.AddValue(Value: TRALJSON);
var
  Index: Integer;
begin
  try
    Index := FDepth - 1;
    if Index < 0 then
    begin
      if FRoot <> nil then
        Fail('Unexpected content after JSON value');
      FRoot := Value;
    end
    else
      case FFrames[Index].Expectation of
        rjeObjectValue:
          begin
            TRALJSONObject(FFrames[Index].Container).AddParsed(FFrames[Index].MemberName, Value);
            FFrames[Index].MemberName := '';
            FFrames[Index].Expectation := rjeObjectCommaOrEnd;
          end;
        rjeArrayValueOrEnd, rjeArrayValue:
          begin
            TRALJSONArray(FFrames[Index].Container).FValues.Add(Value);
            FFrames[Index].Expectation := rjeArrayCommaOrEnd;
          end;
      else
        Fail('Unexpected JSON value');
      end;
  except
    Value.Free;
    raise;
  end;
end;

function TRALJSONParser.TokenText(
  Start: Integer;
  Count: Integer
): string;
{$IFNDEF FPC}
var
  I: Integer;
{$ENDIF}
begin
  SetLength(Result, Count);
  {$IFDEF FPC}
  if Count > 0 then
    Move(FToken[Start], Result[1], Count);
  {$ELSE}
  for I := 1 to Count do
    Result[I] := Char(Ord(FToken[Start + I - 1]));
  {$ENDIF}
end;

procedure TRALJSONParser.CompleteToken;
var
  Reader: TRALJSONTokenReader;
  Text: string;
  Index: Integer;
begin
  case FTokenKind of
    rjtkString:
      begin
        if not FTokenHasEscape then
        begin
          if FTokenIsASCII then
            Text := TokenText(2, FTokenLength - 2)
          else
            Text := NativeText(DecodeUTF8Range(FToken, 2, FTokenLength - 2));
        end
        else
        begin
          Reader := TRALJSONTokenReader.Create(DecodeUTF8Range(FToken, 1, FTokenLength));
          try
            Text := Reader.ReadString;
          finally
            Reader.Free;
          end;
        end;
        Index := FDepth - 1;
        if (Index >= 0) and
           (FFrames[Index].Expectation in [rjeObjectKeyOrEnd, rjeObjectKey]) then
        begin
          if TRALJSONObject(FFrames[Index].Container).Find(Text) <> nil then
            Fail('Duplicate JSON member: ' + Text);
          FFrames[Index].MemberName := Text;
          FFrames[Index].Expectation := rjeObjectColon;
        end
        else
          AddValue(TRALJSONString.Create(Text));
      end;
    rjtkNumber:
      AddValue(TRALJSONNumber.Create(TokenText(1, FTokenLength)));
    rjtkLiteral:
      begin
        if FTokenLength <> Length(FLiteral) then
          Fail('Incomplete JSON literal');
        if FLiteral = 'null' then
          AddValue(TRALJSONNull.Create)
        else
          AddValue(TRALJSONBoolean.Create(FLiteral = 'true'));
      end;
  else
    Fail('No JSON token to complete');
  end;
  FTokenKind := rjtkNone;
  FTokenHasEscape := False;
  FTokenIsASCII := True;
  FTokenLength := 0;
  FLiteral := '';
  if Length(FToken) > 8192 then
    SetLength(FToken, 8192);
end;

procedure TRALJSONParser.StartContainer(IsObject: Boolean);
var
  Container: TRALJSON;
  Capacity: Integer;
begin
  if (FMaxDepth > 0) and (FDepth >= FMaxDepth) then
    Fail('JSON nesting exceeds MaxDepth');
  if FDepth >= MaxInt div SizeOf(TRALJSONParserFrame) then
    raise EIntOverflow.Create('JSON nesting exceeds the frame index capacity.');
  if FDepth = Length(FFrames) then
  begin
    if Length(FFrames) > (MaxInt div SizeOf(TRALJSONParserFrame)) div 2 then
      Capacity := MaxInt div SizeOf(TRALJSONParserFrame)
    else
      Capacity := Length(FFrames) * 2;
    if Capacity < 16 then
      Capacity := 16;
    if (FMaxDepth > 0) and (Capacity > FMaxDepth) then
      Capacity := FMaxDepth;
    SetLength(FFrames, Capacity);
  end;
  if IsObject then
    Container := TRALJSONObject.Create
  else
    Container := TRALJSONArray.Create;
  AddValue(Container);
  FFrames[FDepth].Container := Container;
  if IsObject then
    FFrames[FDepth].Expectation := rjeObjectKeyOrEnd
  else
    FFrames[FDepth].Expectation := rjeArrayValueOrEnd;
  Inc(FDepth);
end;

procedure TRALJSONParser.CloseContainer(IsObject: Boolean);
var
  Index: Integer;
begin
  Index := FDepth - 1;
  if Index < 0 then
    Fail('Unexpected JSON closing delimiter');
  if IsObject then
  begin
    if not (FFrames[Index].Container is TRALJSONObject) or
       not (FFrames[Index].Expectation in [rjeObjectKeyOrEnd, rjeObjectCommaOrEnd]) then
      Fail('Object is incomplete or has an unexpected closing delimiter');
  end
  else if not (FFrames[Index].Container is TRALJSONArray) or
          not (FFrames[Index].Expectation in [rjeArrayValueOrEnd, rjeArrayCommaOrEnd]) then
    Fail('Array is incomplete or has an unexpected closing delimiter');
  FFrames[Index].Container := nil;
  FFrames[Index].MemberName := '';
  Dec(FDepth);
end;

procedure TRALJSONParser.Consume(Value: Byte);
var
  Index: Integer;
begin
  if FBOMLength <> 0 then
  begin
    if ((FBOMLength = 1) and (Value <> $BB)) or
       ((FBOMLength = 2) and (Value <> $BF)) then
      Fail('Invalid UTF-8 BOM');
    Inc(FBOMLength);
    if FBOMLength = 3 then
      FBOMLength := 0;
    Exit;
  end;
  if (FTotalBytes = 1) and (Value = $EF) then
  begin
    FBOMLength := 1;
    Exit;
  end;
  case FTokenKind of
    rjtkString:
      begin
        if Value < 32 then
          Fail('Unescaped control character in JSON string');
        AppendToken(Value);
        if FEscaped then
          FEscaped := False
        else if Value = Ord('\') then
          FEscaped := True
        else if Value = Ord('"') then
          CompleteToken;
        Exit;
      end;
    rjtkNumber:
      begin
        if ((Value >= Ord('0')) and (Value <= Ord('9'))) or
           (Value = Ord('-')) or (Value = Ord('+')) or (Value = Ord('.')) or
           (Value = Ord('e')) or (Value = Ord('E')) then
        begin
          AppendToken(Value);
          Exit;
        end;
        CompleteToken;
        { The delimiter still belongs to the surrounding object/array. }
      end;
    rjtkLiteral:
      begin
        if Value <> Ord(FLiteral[FTokenLength + 1]) then
          Fail('Invalid JSON literal');
        AppendToken(Value);
        if FTokenLength = Length(FLiteral) then
          CompleteToken;
        Exit;
      end;
  end;
  Index := FDepth - 1;
  case Value of
    9, 10, 13, 32:
      Exit;
    Ord('{'):
      StartContainer(True);
    Ord('['):
      StartContainer(False);
    Ord('}'):
      CloseContainer(True);
    Ord(']'):
      CloseContainer(False);
    Ord(':'):
      begin
        if Index < 0 then
          Fail('Unexpected JSON colon');
        if FFrames[Index].Expectation <> rjeObjectColon then
          Fail('Unexpected JSON colon');
        FFrames[Index].Expectation := rjeObjectValue;
      end;
    Ord(','):
      begin
        if Index < 0 then
          Fail('Unexpected JSON comma');
        case FFrames[Index].Expectation of
          rjeObjectCommaOrEnd:
            FFrames[Index].Expectation := rjeObjectKey;
          rjeArrayCommaOrEnd:
            FFrames[Index].Expectation := rjeArrayValue;
        else
          Fail('Unexpected JSON comma');
        end;
      end;
    Ord('"'):
      begin
        FTokenKind := rjtkString;
        FEscaped := False;
        AppendToken(Value);
      end;
    Ord('-'), Ord('0')..Ord('9'):
      begin
        FTokenKind := rjtkNumber;
        AppendToken(Value);
      end;
    Ord('t'), Ord('f'), Ord('n'):
      begin
        FTokenKind := rjtkLiteral;
        if Value = Ord('t') then
          FLiteral := 'true'
        else if Value = Ord('f') then
          FLiteral := 'false'
        else
          FLiteral := 'null';
        AppendToken(Value);
      end;
  else
    Fail('Unexpected JSON character');
  end;
end;

procedure TRALJSONParser.Update(
  const Buffer;
  Count: NativeInt
);
var
  Cursor: PByte;
  I: NativeInt;
begin
  EnsureActive;
  try
    if Count < 0 then
      Fail('JSON block size cannot be negative');
    if Count > High(Int64) - FTotalBytes then
      raise EIntOverflow.Create('JSON byte count exceeds Int64 capacity.');
    if (FMaxDocumentSize > 0) and (Count > FMaxDocumentSize - FTotalBytes) then
      Fail('JSON input exceeds MaxDocumentSize');
    Cursor := @Buffer;
    for I := 0 to Count - 1 do
    begin
      Inc(FTotalBytes);
      Consume(Cursor^);
      Inc(Cursor);
    end;
  except
    FFailed := True;
    raise;
  end;
end;

procedure TRALJSONParser.Update(const Bytes: RawByteString);
begin
  if Length(Bytes) = 0 then
    Update(Bytes, 0)
  else
    Update(Bytes[1], Length(Bytes));
end;

function TRALJSONParser.Finish: TRALJSON;
begin
  EnsureActive;
  try
    if FBOMLength <> 0 then
      Fail('Incomplete UTF-8 BOM');
    if FTokenKind = rjtkNumber then
      CompleteToken
    else if FTokenKind <> rjtkNone then
      Fail('Incomplete JSON token');
    if FDepth <> 0 then
      Fail('Incomplete JSON container');
    if FRoot = nil then
      Fail('Expected a JSON value');
    Result := FRoot;
    FRoot := nil;
    FFinished := True;
    FToken := '';
  except
    FFailed := True;
    raise;
  end;
end;

end.
