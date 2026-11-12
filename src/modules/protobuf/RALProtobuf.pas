/// Protocol Buffers runtime: the wire format, the base message and its JSON mapping.
unit RALProtobuf;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses
  Classes, SysUtils, Contnrs, Math,
  RALTypes, RALExceptions, RALJson, RALBase64, RALTools;

type
  TRALProtoMessage = class;

  /// A class of message, for lists and nested messages created by type.
  TRALProtoMessageClass = class of TRALProtoMessage;

  /// The wire type of a field: the low three bits of its tag.
  TRALProtoWireType = (rpwVarint, rpwFixed64, rpwLengthDelimited, rpwStartGroup,
    rpwEndGroup, rpwFixed32);

  { How a scalar is written: the .proto type of a number, which decides its
    wire type and encoding. }
  TRALProtoScalar = (rpsInt32, rpsInt64, rpsUInt32, rpsUInt64, rpsSInt32,
    rpsSInt64, rpsBool, rpsEnum, rpsFixed32, rpsFixed64, rpsSFixed32, rpsSFixed64,
    rpsFloat, rpsDouble);

  /// Options of the JSON mapping.
  TRALProtoJSONOption = (
    /// Writes fields that hold their default value.
    rpjEmitDefaults,
    /// Writes enums as numbers instead of names.
    rpjEnumsAsNumbers,
    /// Writes the .proto field names instead of the lowerCamelCase JSON names.
    rpjOriginalNames,
    /// Skips unknown fields and enum names when reading instead of raising.
    rpjIgnoreUnknown
  );
  /// A set of JSON mapping options.
  TRALProtoJSONOptions = set of TRALProtoJSONOption;

  /// Repeated int32, sint32, sfixed32 and enum values.
  TRALProtoInt32Array = array of Integer;
  /// Repeated int64, sint64 and sfixed64 values.
  TRALProtoInt64Array = array of Int64;
  /// Repeated uint32 and fixed32 values.
  TRALProtoUInt32Array = array of Cardinal;
  /// Repeated uint64 and fixed64 values.
  TRALProtoUInt64Array = array of UInt64RAL;
  /// Repeated bool values.
  TRALProtoBoolArray = array of Boolean;
  /// Repeated float values.
  TRALProtoSingleArray = array of Single;
  /// Repeated double values.
  TRALProtoDoubleArray = array of Double;
  /// Repeated string values, UTF-8.
  TRALProtoStringArray = array of StringRAL;
  /// Repeated bytes values.
  TRALProtoBytesArray = array of TBytes;

  { Writes the wire format into one growing buffer. A nested message is written
    in place: BeginLength marks where it starts and EndLength inserts its size
    before it, so no temporary buffer is made per message. }
  TRALProtoWriter = class
  private
    /// The bytes written so far.
    FBuffer: TBytes;
    FPosition: IntegerRAL;

    /// Makes room for ACount more bytes.
    procedure Grow(ACount: IntegerRAL);
  public
    /// Starts a length-delimited value; pass the result to EndLength.
    function BeginLength: IntegerRAL;
    /// Empties the buffer and keeps its memory.
    procedure Clear;
    /// Ends the value BeginLength started, writing its size in front of it.
    procedure EndLength(AMark: IntegerRAL);
    /// Writes the buffer to AStream.
    procedure SaveToStream(AStream: TStream);
    /// The bytes written so far.
    function ToBytes: TBytes;
    /// Writes a bool as a varint.
    procedure WriteBool(AValue: Boolean);
    /// Writes a length and the bytes.
    procedure WriteBytes(const AValue: TBytes);
    /// Writes a double as 8 little-endian bytes.
    procedure WriteDouble(AValue: Double);
    /// Writes an enum value as an int32.
    procedure WriteEnum(AValue: Integer);
    /// Writes 4 little-endian bytes.
    procedure WriteFixed32(AValue: Cardinal);
    /// Writes 8 little-endian bytes.
    procedure WriteFixed64(AValue: UInt64RAL);
    /// Writes a float as 4 little-endian bytes.
    procedure WriteFloat(AValue: Single);
    /// Writes an int32; a negative value takes 10 bytes, as the format says.
    procedure WriteInt32(AValue: Integer);
    /// Writes an int64 as a varint.
    procedure WriteInt64(AValue: Int64);
    /// Writes a length, then the message.
    procedure WriteMessage(AMessage: TRALProtoMessage);
    /// Writes ACount bytes as they are.
    procedure WriteRaw(const AData; ACount: IntegerRAL);
    /// Writes bytes as they are.
    procedure WriteRawBytes(const AValue: TBytes);
    /// Writes a repeated bool field, packed or one tag per value.
    procedure WriteRepeatedBool(AField: IntegerRAL; const AValues: TRALProtoBoolArray;
      APacked: Boolean);
    /// Writes a repeated bytes field.
    procedure WriteRepeatedBytes(AField: IntegerRAL; const AValues: TRALProtoBytesArray);
    /// Writes a repeated double field, packed or one tag per value.
    procedure WriteRepeatedDouble(AField: IntegerRAL; const AValues: TRALProtoDoubleArray;
      APacked: Boolean);
    /// Writes a repeated int32, sint32, sfixed32 or enum field.
    procedure WriteRepeatedInt32(AField: IntegerRAL; AKind: TRALProtoScalar;
      const AValues: TRALProtoInt32Array; APacked: Boolean);
    /// Writes a repeated int64, sint64 or sfixed64 field.
    procedure WriteRepeatedInt64(AField: IntegerRAL; AKind: TRALProtoScalar;
      const AValues: TRALProtoInt64Array; APacked: Boolean);
    /// Writes a repeated float field, packed or one tag per value.
    procedure WriteRepeatedSingle(AField: IntegerRAL; const AValues: TRALProtoSingleArray;
      APacked: Boolean);
    /// Writes a repeated string field.
    procedure WriteRepeatedString(AField: IntegerRAL; const AValues: TRALProtoStringArray);
    /// Writes a repeated uint32 or fixed32 field.
    procedure WriteRepeatedUInt32(AField: IntegerRAL; AKind: TRALProtoScalar;
      const AValues: TRALProtoUInt32Array; APacked: Boolean);
    /// Writes a repeated uint64 or fixed64 field.
    procedure WriteRepeatedUInt64(AField: IntegerRAL; AKind: TRALProtoScalar;
      const AValues: TRALProtoUInt64Array; APacked: Boolean);
    /// Writes an sfixed32: 4 little-endian bytes.
    procedure WriteSFixed32(AValue: Integer);
    /// Writes an sfixed64: 8 little-endian bytes.
    procedure WriteSFixed64(AValue: Int64);
    /// Writes an sint32 (zigzag varint).
    procedure WriteSInt32(AValue: Integer);
    /// Writes an sint64 (zigzag varint).
    procedure WriteSInt64(AValue: Int64);
    /// Writes a length and the UTF-8 text.
    procedure WriteString(const AValue: StringRAL);
    /// Writes a field tag.
    procedure WriteTag(AField: IntegerRAL; AWire: TRALProtoWireType);
    /// Writes a uint32 as a varint.
    procedure WriteUInt32(AValue: Cardinal);
    /// Writes a uint64 as a varint.
    procedure WriteUInt64(AValue: UInt64RAL);
    /// Writes a varint.
    procedure WriteVarint(AValue: UInt64RAL);

    /// The number of bytes written.
    property Position: IntegerRAL read FPosition;
  end;

  { Reads the wire format straight from memory: a nested message narrows the
    limit instead of being copied. Created on TBytes it keeps a reference to
    them; created on a pointer the memory must outlive the reader. }
  TRALProtoReader = class
  private
    /// The message bytes when the reader owns them.
    FBytes: TBytes;
    /// Start of the message bytes being read.
    FData: PByte;
    /// Current nesting depth of sub-messages.
    FDepth: IntegerRAL;
    /// End position of the current message or sub-message.
    FLimit: IntegerRAL;
    FMaxDepth: IntegerRAL;
    FPosition: IntegerRAL;
    /// Total size of the data.
    FSize: IntegerRAL;
    FTagPosition: IntegerRAL;

    /// Raises when fewer than ACount bytes are left before the limit.
    procedure Need(ACount: IntegerRAL);
    /// Skips a group up to the end tag of AField.
    procedure SkipGroup(AField: IntegerRAL);
  public
    /// Reads ASize bytes at AData; the memory must outlive the reader.
    constructor Create(AData: Pointer; ASize: IntegerRAL); overload;
    /// Reads ABytes; the reader keeps a reference to them.
    constructor Create(const ABytes: TBytes); overload;

    /// Whether the position reached the current limit.
    function EOF: Boolean;
    /// Ends a nested value: restores the limit PushLimit returned.
    procedure PopLimit(AOldLimit: IntegerRAL);
    /// Narrows the readable range to the next ALength bytes; returns the old limit.
    function PushLimit(ALength: IntegerRAL): IntegerRAL;
    /// Reads a bool.
    function ReadBool: Boolean;
    /// Reads a length and that many bytes.
    function ReadBytes: TBytes;
    /// Reads a double.
    function ReadDouble: Double;
    /// Reads an enum value (an int32).
    function ReadEnum: Integer;
    /// Reads 4 little-endian bytes.
    function ReadFixed32: Cardinal;
    /// Reads 8 little-endian bytes.
    function ReadFixed64: UInt64RAL;
    /// Reads a float.
    function ReadFloat: Single;
    /// Reads an int32 (the low 32 bits of the varint).
    function ReadInt32: Integer;
    /// Reads an int64.
    function ReadInt64: Int64;
    /// Reads a length prefix and checks it fits before the limit.
    function ReadLength: IntegerRAL;
    /// Reads a length, then merges the message into AMessage.
    procedure ReadMessage(AMessage: TRALProtoMessage);
    /// Reads one bool or a packed run of them into AValues.
    procedure ReadRepeatedBool(AWire: TRALProtoWireType; var AValues: TRALProtoBoolArray);
    /// Reads one double or a packed run of them into AValues.
    procedure ReadRepeatedDouble(AWire: TRALProtoWireType; var AValues: TRALProtoDoubleArray);
    /// Reads one int32-sized value or a packed run of them into AValues.
    procedure ReadRepeatedInt32(AWire: TRALProtoWireType; AKind: TRALProtoScalar;
      var AValues: TRALProtoInt32Array);
    /// Reads one int64-sized value or a packed run of them into AValues.
    procedure ReadRepeatedInt64(AWire: TRALProtoWireType; AKind: TRALProtoScalar;
      var AValues: TRALProtoInt64Array);
    /// Reads one float or a packed run of them into AValues.
    procedure ReadRepeatedSingle(AWire: TRALProtoWireType; var AValues: TRALProtoSingleArray);
    /// Reads one uint32-sized value or a packed run of them into AValues.
    procedure ReadRepeatedUInt32(AWire: TRALProtoWireType; AKind: TRALProtoScalar;
      var AValues: TRALProtoUInt32Array);
    /// Reads one uint64-sized value or a packed run of them into AValues.
    procedure ReadRepeatedUInt64(AWire: TRALProtoWireType; AKind: TRALProtoScalar;
      var AValues: TRALProtoUInt64Array);
    /// Reads an sfixed32.
    function ReadSFixed32: Integer;
    /// Reads an sfixed64.
    function ReadSFixed64: Int64;
    /// Reads an sint32 (zigzag).
    function ReadSInt32: Integer;
    /// Reads an sint64 (zigzag).
    function ReadSInt64: Int64;
    /// Reads a length and the UTF-8 text.
    function ReadString: StringRAL;
    /// Reads the next tag; False at the limit.
    function ReadTag(out AField: IntegerRAL; out AWire: TRALProtoWireType): Boolean;
    /// Reads a uint32 (the low 32 bits of the varint).
    function ReadUInt32: Cardinal;
    /// Reads a uint64.
    function ReadUInt64: UInt64RAL;
    /// Reads a varint of up to 10 bytes.
    function ReadVarint: UInt64RAL;
    /// Skips the value of a field whose tag was just read.
    procedure SkipField(AField: IntegerRAL; AWire: TRALProtoWireType);

    /// The deepest nesting of messages and groups accepted (100 by default).
    property MaxDepth: IntegerRAL read FMaxDepth write FMaxDepth;
    /// The offset of the next byte.
    property Position: IntegerRAL read FPosition;
    /// The offset of the last tag read, for keeping an unknown field whole.
    property TagPosition: IntegerRAL read FTagPosition;
  end;

  { The base of every generated message. Fields the class does not know are kept
    as they came and written back after the known ones, so a message passes
    through a program built on an older .proto unchanged. }
  TRALProtoMessage = class(TPersistent)
  private
    FUnknownFields: TBytes;
  protected
    /// Keeps a varint field the class read but refuses (an unknown closed-enum number).
    procedure AddUnknownVarint(AField: IntegerRAL; AValue: UInt64RAL);
    /// Reads one field; False when the field or its wire type is not known.
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; virtual;
    /// Writes the known fields, in field number order.
    procedure EncodeFields(AWriter: TRALProtoWriter); virtual;
    /// Reads one JSON member; False when the name is not a field.
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; virtual;
    /// Clears the fields of the class (the unknown fields are cleared by Clear).
    procedure ResetFields; virtual;
    /// Writes the fields as JSON members.
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); virtual;
  public
    /// Creates an empty message.
    constructor Create; virtual;

    /// Copies another message of the same class (through the wire format).
    procedure Assign(ASource: TPersistent); override;
    /// Resets every field to its default and drops the unknown fields.
    procedure Clear;
    /// A new message of the same class with the same content.
    function Clone: TRALProtoMessage;
    /// Clears the message and reads ABytes.
    procedure Decode(const ABytes: TBytes); overload;
    /// Clears the message and reads ASize bytes at AData.
    procedure Decode(AData: Pointer; ASize: IntegerRAL); overload;
    /// Clears the message and reads the rest of AStream.
    procedure DecodeFromStream(AStream: TStream);
    /// The message in the wire format.
    function Encode: TBytes;
    /// Writes the message (without a length) to AWriter.
    procedure EncodeTo(AWriter: TRALProtoWriter);
    /// Writes the message in the wire format to AStream.
    procedure EncodeToStream(AStream: TStream);
    /// Clears the message and reads it from JSON text (the form ReadJSONValue takes).
    procedure FromJSON(const AText: StringRAL;
      AOptions: TRALProtoJSONOptions = []);
    /// Merges the fields read from AReader up to its limit.
    procedure MergeFrom(AReader: TRALProtoReader);
    /// The full .proto name (package.Name); empty for a hand-written class.
    class function ProtoName: StringRAL; virtual;
    /// Merges the members of a JSON object.
    procedure ReadJSON(AObject: TRALJSONObject; AOptions: TRALProtoJSONOptions = []); virtual;
    { Merges a JSON value: an object for a plain message; the well-known types
      take their own forms (a string for a Timestamp, any value for a Value). }
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); virtual;
    /// The message as JSON text (the proto3 canonical mapping).
    function ToJSON(AOptions: TRALProtoJSONOptions = []): StringRAL;
    /// The message as a JSON value; the caller frees it.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; virtual;

    /// The fields the class did not know, as they came on the wire.
    property UnknownFields: TBytes read FUnknownFields write FUnknownFields;
  end;

  /// A repeated message field: owns its items.
  TRALProtoMessageList = class
  private
    FItemClass: TRALProtoMessageClass;
    /// The messages held by the list.
    FItems: TObjectList;

    function GetCount: IntegerRAL;
    function GetMessage(AIndex: IntegerRAL): TRALProtoMessage;
  public
    /// Creates an empty list of AItemClass messages.
    constructor Create(AItemClass: TRALProtoMessageClass);
    destructor Destroy; override;

    /// Adds a new empty item and returns it.
    function AddNew: TRALProtoMessage;
    /// Adds AItem; the list owns it.
    procedure Append(AItem: TRALProtoMessage);
    /// Copies the items of ASource.
    procedure Assign(ASource: TRALProtoMessageList);
    /// Frees every item.
    procedure Clear;
    /// Frees the item at AIndex.
    procedure Delete(AIndex: IntegerRAL);
    /// Writes every item as the field AField.
    procedure EncodeTo(AWriter: TRALProtoWriter; AField: IntegerRAL);
    /// Removes the item at AIndex from the list without freeing it.
    function Extract(AIndex: IntegerRAL): TRALProtoMessage;
    /// Reads one item (a length-delimited value) and adds it.
    procedure ReadItem(AReader: TRALProtoReader);
    /// Reads a JSON array of objects; null leaves the list empty.
    procedure ReadJSON(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
    /// The items as a JSON array; the caller frees it.
    function ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSONArray;

    /// The number of items.
    property Count: IntegerRAL read GetCount;
    /// The class of the items.
    property ItemClass: TRALProtoMessageClass read FItemClass;
    /// The items.
    property Messages[AIndex: IntegerRAL]: TRALProtoMessage read GetMessage;
  end;

{ Appends one byte to AText as it is. On Delphi, Text + AnsiChar goes through
  the ANSI code page and turns a byte above 127 into another. }
procedure RALProtoAppendByte(var AText: StringRAL; AValue: Byte);
/// The message class registered under a full .proto name, nil when none.
function RALProtoFindMessage(const AFullName: StringRAL): TRALProtoMessageClass;
{ Registers a generated class under its full .proto name (generated units do it
  in their initialization), so Any and gRPC reflection find it. }
procedure RALProtoRegisterMessage(AClass: TRALProtoMessageClass);
/// Removes a class RALProtoRegisterMessage added.
procedure RALProtoUnregisterMessage(AClass: TRALProtoMessageClass);
/// Appends a value to a repeated bool field.
procedure RALProtoAddBool(var AValues: TRALProtoBoolArray; AValue: Boolean);
/// Appends a value to a repeated bytes field.
procedure RALProtoAddBytes(var AValues: TRALProtoBytesArray; const AValue: TBytes);
/// Appends a value to a repeated double field.
procedure RALProtoAddDouble(var AValues: TRALProtoDoubleArray; AValue: Double);
/// Appends a value to a repeated int32, sint32, sfixed32 or enum field.
procedure RALProtoAddInt32(var AValues: TRALProtoInt32Array; AValue: Integer);
/// Appends a value to a repeated int64, sint64 or sfixed64 field.
procedure RALProtoAddInt64(var AValues: TRALProtoInt64Array; AValue: Int64);
/// Appends a value to a repeated float field.
procedure RALProtoAddSingle(var AValues: TRALProtoSingleArray; AValue: Single);
/// Appends a value to a repeated string field.
procedure RALProtoAddString(var AValues: TRALProtoStringArray; const AValue: StringRAL);
/// Appends a value to a repeated uint32 or fixed32 field.
procedure RALProtoAddUInt32(var AValues: TRALProtoUInt32Array; AValue: Cardinal);
/// Appends a value to a repeated uint64 or fixed64 field.
procedure RALProtoAddUInt64(var AValues: TRALProtoUInt64Array; AValue: UInt64RAL);
/// A JSON value as an array; raises for any other kind.
function RALProtoArrayOf(AValue: TRALJSON): TRALJSONArray;
/// A JSON value as an object; raises for any other kind.
function RALProtoObjectOf(AValue: TRALJSON): TRALJSONObject;
/// The bits of a double, to tell 0.0 from -0.0.
function RALProtoDoubleBits(AValue: Double): UInt64RAL;
/// Bool as JSON.
function RALProtoJSONBool(AValue: Boolean): TRALJSON;
/// Bytes as JSON: a base64 string.
function RALProtoJSONBytes(const AValue: TBytes): TRALJSON;
/// Double as JSON: a number, or "NaN", "Infinity", "-Infinity".
function RALProtoJSONDouble(AValue: Double): TRALJSON;
/// An enum as JSON: its name when it has one and the options allow, else its number.
function RALProtoJSONEnum(AValue: Integer; const AName: StringRAL;
  AOptions: TRALProtoJSONOptions): TRALJSON;
/// Int32 as JSON: a number.
function RALProtoJSONInt32(AValue: Integer): TRALJSON;
/// Int64 as JSON: a decimal string, as the mapping says.
function RALProtoJSONInt64(AValue: Int64): TRALJSON;
/// The lowerCamelCase JSON name of a .proto field name.
function RALProtoJSONName(const AName: StringRAL): StringRAL;
/// Float as JSON: the shortest number that reads back the same float.
function RALProtoJSONSingle(AValue: Single): TRALJSON;
/// String as JSON.
function RALProtoJSONString(const AValue: StringRAL): TRALJSON;
/// The JSON member name of a field under AOptions.
function RALProtoKey(AOptions: TRALProtoJSONOptions;
  const AJSONName, AOriginalName: StringRAL): StringRAL;
/// A map key read from its JSON member name.
function RALProtoKeyToBool(const AKey: StringRAL): Boolean;
/// A map key read from its JSON member name.
function RALProtoKeyToInt32(const AKey: StringRAL): Integer;
/// A map key read from its JSON member name.
function RALProtoKeyToInt64(const AKey: StringRAL): Int64;
/// A map key read from its JSON member name.
function RALProtoKeyToUInt32(const AKey: StringRAL): Cardinal;
/// A map key read from its JSON member name.
function RALProtoKeyToUInt64(const AKey: StringRAL): UInt64RAL;
/// Uint32 as JSON: a number.
function RALProtoJSONUInt32(AValue: Cardinal): TRALJSON;
/// Uint64 as JSON: a decimal string.
function RALProtoJSONUInt64(AValue: UInt64RAL): TRALJSON;
/// A JSON bool (or "true"/"false") as a bool.
function RALProtoReadBool(AValue: TRALJSON): Boolean;
/// A JSON base64 string (standard or URL alphabet, padded or not) as bytes.
function RALProtoReadBytes(AValue: TRALJSON): TBytes;
/// A JSON number or numeric string as a double.
function RALProtoReadDouble(AValue: TRALJSON): Double;
/// A JSON number or numeric string as an int32; raises when out of range.
function RALProtoReadInt32(AValue: TRALJSON): Integer;
/// A JSON number or numeric string as an int64.
function RALProtoReadInt64(AValue: TRALJSON): Int64;
/// A JSON number or numeric string as a float.
function RALProtoReadSingle(AValue: TRALJSON): Single;
/// A JSON string as text.
function RALProtoReadString(AValue: TRALJSON): StringRAL;
/// A JSON number or numeric string as a uint32.
function RALProtoReadUInt32(AValue: TRALJSON): Cardinal;
/// A JSON number or numeric string as a uint64.
function RALProtoReadUInt64(AValue: TRALJSON): UInt64RAL;
/// Text from the hex of its UTF-8 bytes (non-ASCII defaults of generated code).
function RALProtoHexText(const AHex: StringRAL): StringRAL;
/// The decimal text of a uint64.
function RALProtoUInt64Text(AValue: UInt64RAL): StringRAL;
/// The bits of a float, to tell 0.0 from -0.0.
function RALProtoSingleBits(AValue: Single): Cardinal;

implementation

const
  /// The default nesting limit of a reader.
  cDefaultMaxDepth = 100;

var
  /// Registered classes by full name; filled by initialization sections, read after.
  gMessages: TStringList;

/// Raises the error every malformed message ends in.
procedure ProtoError(const AText: string);
begin
  raise ERALException.Create('Protobuf: ' + AText);
end;

/// The zigzag form of an sint32.
function ZigZag32(AValue: Integer): Cardinal;
begin
  if AValue < 0 then
    Result := (Cardinal(not AValue) shl 1) or 1
  else
    Result := Cardinal(AValue) shl 1;
end;

/// The zigzag form of an sint64.
function ZigZag64(AValue: Int64): UInt64RAL;
begin
  if AValue < 0 then
    Result := (UInt64RAL(not AValue) shl 1) or 1
  else
    Result := UInt64RAL(AValue) shl 1;
end;

/// An sint32 from its zigzag form.
function UnZigZag32(AValue: Cardinal): Integer;
begin
  if (AValue and 1) <> 0 then
    Result := not Integer(AValue shr 1)
  else
    Result := Integer(AValue shr 1);
end;

/// An sint64 from its zigzag form.
function UnZigZag64(AValue: UInt64RAL): Int64;
begin
  if (AValue and 1) <> 0 then
    Result := not Int64(AValue shr 1)
  else
    Result := Int64(AValue shr 1);
end;

/// The number of bytes of a varint.
function VarintSize(AValue: UInt64RAL): IntegerRAL;
begin
  Result := 1;
  while (AValue and not UInt64RAL($7F)) <> 0 do
  begin
    AValue := AValue shr 7;
    Inc(Result);
  end;
end;

/// Whether a scalar kind goes on the wire as a varint.
function IsVarintKind(AKind: TRALProtoScalar): Boolean;
begin
  Result := AKind in [rpsInt32, rpsInt64, rpsUInt32, rpsUInt64, rpsSInt32,
    rpsSInt64, rpsBool, rpsEnum];
end;

procedure RALProtoAppendByte(var AText: StringRAL; AValue: Byte);
var
  vLength: IntegerRAL;
begin
  vLength := Length(AText);
  SetLength(AText, vLength + 1);
  PByte(NativeUInt(Pointer(AText)) + NativeUInt(vLength))^ := AValue;
end;

{ Registry }

function RALProtoFindMessage(const AFullName: StringRAL): TRALProtoMessageClass;
var
  vIndex: Integer;
begin
  Result := nil;
  if (gMessages <> nil) and gMessages.Find(string(AFullName), vIndex) then
    Result := TRALProtoMessageClass(gMessages.Objects[vIndex]);
end;

procedure RALProtoRegisterMessage(AClass: TRALProtoMessageClass);
var
  vIndex: Integer;
begin
  if gMessages = nil then
  begin
    gMessages := TStringList.Create;
    gMessages.Sorted := True;
    gMessages.CaseSensitive := True;
  end;
  if gMessages.Find(string(AClass.ProtoName), vIndex) then
    gMessages.Objects[vIndex] := TObject(AClass)
  else
    gMessages.AddObject(string(AClass.ProtoName), TObject(AClass));
end;

procedure RALProtoUnregisterMessage(AClass: TRALProtoMessageClass);
var
  vIndex: Integer;
begin
  if (gMessages <> nil) and gMessages.Find(string(AClass.ProtoName), vIndex) and
     (gMessages.Objects[vIndex] = TObject(AClass)) then
    gMessages.Delete(vIndex);
end;

{ Repeated field helpers }

procedure RALProtoAddBool(var AValues: TRALProtoBoolArray; AValue: Boolean);
begin
  SetLength(AValues, Length(AValues) + 1);
  AValues[High(AValues)] := AValue;
end;

procedure RALProtoAddBytes(var AValues: TRALProtoBytesArray; const AValue: TBytes);
begin
  SetLength(AValues, Length(AValues) + 1);
  AValues[High(AValues)] := AValue;
end;

procedure RALProtoAddDouble(var AValues: TRALProtoDoubleArray; AValue: Double);
begin
  SetLength(AValues, Length(AValues) + 1);
  AValues[High(AValues)] := AValue;
end;

procedure RALProtoAddInt32(var AValues: TRALProtoInt32Array; AValue: Integer);
begin
  SetLength(AValues, Length(AValues) + 1);
  AValues[High(AValues)] := AValue;
end;

procedure RALProtoAddInt64(var AValues: TRALProtoInt64Array; AValue: Int64);
begin
  SetLength(AValues, Length(AValues) + 1);
  AValues[High(AValues)] := AValue;
end;

procedure RALProtoAddSingle(var AValues: TRALProtoSingleArray; AValue: Single);
begin
  SetLength(AValues, Length(AValues) + 1);
  AValues[High(AValues)] := AValue;
end;

procedure RALProtoAddString(var AValues: TRALProtoStringArray; const AValue: StringRAL);
begin
  SetLength(AValues, Length(AValues) + 1);
  AValues[High(AValues)] := AValue;
end;

procedure RALProtoAddUInt32(var AValues: TRALProtoUInt32Array; AValue: Cardinal);
begin
  SetLength(AValues, Length(AValues) + 1);
  AValues[High(AValues)] := AValue;
end;

procedure RALProtoAddUInt64(var AValues: TRALProtoUInt64Array; AValue: UInt64RAL);
begin
  SetLength(AValues, Length(AValues) + 1);
  AValues[High(AValues)] := AValue;
end;

function RALProtoDoubleBits(AValue: Double): UInt64RAL;
begin
  Move(AValue, Result, SizeOf(Result));
end;

function RALProtoSingleBits(AValue: Single): Cardinal;
begin
  Move(AValue, Result, SizeOf(Result));
end;

{ JSON helpers }

/// The decimal text of a uint64 (an Int64 on compilers without UInt64).
function UInt64Text(AValue: UInt64RAL): StringRAL;
var
  vDigit: IntegerRAL;
begin
  if AValue = 0 then
  begin
    Result := '0';
    Exit;
  end;
  Result := '';
  while AValue <> 0 do
  begin
    vDigit := IntegerRAL(AValue mod 10);
    {$IF NOT (DEFINED(FPC) OR DEFINED(DELPHIXE2UP))}
    // Int64 stands for uint64 here: mod and div of a negative are signed
    if vDigit < 0 then
      vDigit := vDigit + 10;
    {$IFEND}
    Result := StringRAL(Char(Ord('0') + vDigit)) + Result;
    AValue := AValue div 10;
  end;
end;

/// The digits of a JSON integer value: a number or a numeric string.
function IntegerText(AValue: TRALJSON): StringRAL;
var
  vDouble: Double;
  vPos: IntegerRAL;
begin
  if AValue = nil then
    ProtoError('missing integer');
  if AValue.Kind = rjkString then
    Result := RALTrim(TRALJSONString(AValue).Value)
  else if AValue.Kind = rjkNumber then
    Result := TRALJSONNumber(AValue).Value
  else
  begin
    ProtoError('integer expected');
    Result := '';
  end;
  // 1e3 or 5.0 are integers too, if the value has no fraction
  vPos := Pos('.', string(Result)) + Pos('e', LowerCase(string(Result)));
  if vPos > 0 then
  begin
    if not RALTryStrToFloat(Result, vDouble) or (Frac(vDouble) <> 0) or
       (Abs(vDouble) > 1.8e19) then
      ProtoError('not an integer: ' + string(Result));
    Result := StringRAL(FloatToStrF(vDouble, ffFixed, 20, 0, RALInvariantFormat));
  end;
end;

/// Parses decimal digits into a uint64, with an optional sign flag.
function ParseDecimal(const AText: StringRAL; out ANegative: Boolean): UInt64RAL;
var
  vInt, vStart: IntegerRAL;
  vDigit: UInt64RAL;
begin
  Result := 0;
  ANegative := False;
  vStart := 1;
  if (Length(AText) > 0) and (AText[1] = '-') then
  begin
    ANegative := True;
    vStart := 2;
  end;
  if vStart > Length(AText) then
    ProtoError('empty number');
  for vInt := vStart to Length(AText) do
  begin
    if not (AText[vInt] in ['0'..'9']) then
      ProtoError('not an integer: ' + string(AText));
    vDigit := Ord(AText[vInt]) - Ord('0');
    {$IF DEFINED(FPC) OR DEFINED(DELPHIXE2UP)}
    if Result > (High(UInt64RAL) - vDigit) div 10 then
      ProtoError('integer out of range: ' + string(AText));
    {$IFEND}
    Result := Result * 10 + vDigit;
  end;
end;

function RALProtoArrayOf(AValue: TRALJSON): TRALJSONArray;
begin
  if (AValue = nil) or (AValue.Kind <> rjkArray) then
    ProtoError('JSON array expected');
  Result := TRALJSONArray(AValue);
end;

function RALProtoObjectOf(AValue: TRALJSON): TRALJSONObject;
begin
  if (AValue = nil) or (AValue.Kind <> rjkObject) then
    ProtoError('JSON object expected');
  Result := TRALJSONObject(AValue);
end;

function RALProtoJSONEnum(AValue: Integer; const AName: StringRAL;
  AOptions: TRALProtoJSONOptions): TRALJSON;
begin
  if (AName <> '') and not (rpjEnumsAsNumbers in AOptions) then
    Result := TRALJSONString.Create(AName)
  else
    Result := RALProtoJSONInt32(AValue);
end;

function RALProtoKey(AOptions: TRALProtoJSONOptions;
  const AJSONName, AOriginalName: StringRAL): StringRAL;
begin
  if rpjOriginalNames in AOptions then
    Result := AOriginalName
  else
    Result := AJSONName;
end;

function RALProtoKeyToBool(const AKey: StringRAL): Boolean;
begin
  if AKey = 'true' then
    Result := True
  else if AKey = 'false' then
    Result := False
  else
  begin
    ProtoError('bool map key expected');
    Result := False;
  end;
end;

function RALProtoKeyToInt32(const AKey: StringRAL): Integer;
var
  vKey: TRALJSON;
begin
  vKey := TRALJSONString.Create(AKey);
  try
    Result := RALProtoReadInt32(vKey);
  finally
    vKey.Free;
  end;
end;

function RALProtoKeyToInt64(const AKey: StringRAL): Int64;
var
  vKey: TRALJSON;
begin
  vKey := TRALJSONString.Create(AKey);
  try
    Result := RALProtoReadInt64(vKey);
  finally
    vKey.Free;
  end;
end;

function RALProtoKeyToUInt32(const AKey: StringRAL): Cardinal;
var
  vKey: TRALJSON;
begin
  vKey := TRALJSONString.Create(AKey);
  try
    Result := RALProtoReadUInt32(vKey);
  finally
    vKey.Free;
  end;
end;

function RALProtoKeyToUInt64(const AKey: StringRAL): UInt64RAL;
var
  vKey: TRALJSON;
begin
  vKey := TRALJSONString.Create(AKey);
  try
    Result := RALProtoReadUInt64(vKey);
  finally
    vKey.Free;
  end;
end;

function RALProtoHexText(const AHex: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  SetLength(Result, Length(AHex) div 2);
  for vInt := 1 to Length(Result) do
    Result[vInt] := CharRAL(StrToInt('$' + string(Copy(AHex, vInt * 2 - 1, 2))));
end;

function RALProtoUInt64Text(AValue: UInt64RAL): StringRAL;
begin
  Result := UInt64Text(AValue);
end;

function RALProtoJSONBool(AValue: Boolean): TRALJSON;
begin
  Result := TRALJSONBoolean.Create(AValue);
end;

function RALProtoJSONBytes(const AValue: TBytes): TRALJSON;
begin
  Result := TRALJSONString.Create(TRALBase64.Encode(AValue));
end;

function RALProtoJSONDouble(AValue: Double): TRALJSON;
begin
  if IsNan(AValue) then
    Result := TRALJSONString.Create('NaN')
  else if IsInfinite(AValue) and (AValue > 0) then
    Result := TRALJSONString.Create('Infinity')
  else if IsInfinite(AValue) then
    Result := TRALJSONString.Create('-Infinity')
  else if RALProtoDoubleBits(AValue) = UInt64RAL($8000000000000000) then
    Result := TRALJSONNumber.Create('-0')
  else
    Result := TRALJSONNumber.Create(StringRAL(FloatToStr(AValue, RALInvariantFormat)));
end;

function RALProtoJSONInt32(AValue: Integer): TRALJSON;
begin
  Result := TRALJSONNumber.Create(StringRAL(IntToStr(AValue)));
end;

function RALProtoJSONInt64(AValue: Int64): TRALJSON;
begin
  Result := TRALJSONString.Create(StringRAL(IntToStr(AValue)));
end;

function RALProtoJSONName(const AName: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
  vUpper: Boolean;
begin
  Result := '';
  vUpper := False;
  for vInt := 1 to Length(AName) do
  begin
    if AName[vInt] = '_' then
      vUpper := True
    else if vUpper then
    begin
      if AName[vInt] in ['a'..'z'] then
        RALProtoAppendByte(Result, Ord(AName[vInt]) - 32)
      else
        RALProtoAppendByte(Result, Ord(AName[vInt]));
      vUpper := False;
    end
    else
      RALProtoAppendByte(Result, Ord(AName[vInt]));
  end;
end;

function RALProtoJSONSingle(AValue: Single): TRALJSON;
var
  vDigits: IntegerRAL;
  vText: string;
  vBack: Double;
  vSingle: Single;
begin
  if IsNan(AValue) then
    Result := TRALJSONString.Create('NaN')
  else if IsInfinite(AValue) and (AValue > 0) then
    Result := TRALJSONString.Create('Infinity')
  else if IsInfinite(AValue) then
    Result := TRALJSONString.Create('-Infinity')
  else if RALProtoSingleBits(AValue) = $80000000 then
    Result := TRALJSONNumber.Create('-0')
  else
  begin
    // the shortest text that reads back the same float, not the double's digits
    vText := '';
    for vDigits := 6 to 9 do
    begin
      vText := FloatToStrF(AValue, ffGeneral, vDigits, 0, RALInvariantFormat);
      if RALTryStrToFloat(StringRAL(vText), vBack) then
      begin
        vSingle := vBack;
        if vSingle = AValue then
          Break;
      end;
    end;
    Result := TRALJSONNumber.Create(StringRAL(vText));
  end;
end;

function RALProtoJSONString(const AValue: StringRAL): TRALJSON;
begin
  Result := TRALJSONString.Create(AValue);
end;

function RALProtoJSONUInt32(AValue: Cardinal): TRALJSON;
begin
  Result := TRALJSONNumber.Create(StringRAL(IntToStr(Int64(AValue))));
end;

function RALProtoJSONUInt64(AValue: UInt64RAL): TRALJSON;
begin
  Result := TRALJSONString.Create(UInt64Text(AValue));
end;

function RALProtoReadBool(AValue: TRALJSON): Boolean;
begin
  if (AValue <> nil) and (AValue.Kind = rjkBoolean) then
    Result := TRALJSONBoolean(AValue).Value
  else if (AValue <> nil) and (AValue.Kind = rjkString) and
     ((TRALJSONString(AValue).Value = 'true') or (TRALJSONString(AValue).Value = 'false')) then
    Result := TRALJSONString(AValue).Value = 'true'
  else
  begin
    ProtoError('bool expected');
    Result := False;
  end;
end;

function RALProtoReadBytes(AValue: TRALJSON): TBytes;
var
  vText: StringRAL;
  vInt: IntegerRAL;
begin
  vText := RALProtoReadString(AValue);
  // the URL alphabet is accepted too
  for vInt := 1 to Length(vText) do
    if vText[vInt] = '-' then
      vText[vInt] := '+'
    else if vText[vInt] = '_' then
      vText[vInt] := '/';
  while Length(vText) mod 4 <> 0 do
    vText := vText + '=';
  Result := TRALBase64.DecodeAsBytes(vText);
end;

function RALProtoReadDouble(AValue: TRALJSON): Double;
var
  vText: StringRAL;
begin
  if AValue = nil then
    ProtoError('number expected');
  if AValue.Kind = rjkNumber then
    vText := TRALJSONNumber(AValue).Value
  else if AValue.Kind = rjkString then
  begin
    vText := TRALJSONString(AValue).Value;
    if vText = 'NaN' then
    begin
      Result := NaN;
      Exit;
    end
    else if vText = 'Infinity' then
    begin
      Result := Infinity;
      Exit;
    end
    else if vText = '-Infinity' then
    begin
      Result := NegInfinity;
      Exit;
    end;
  end
  else
    vText := '';
  if not RALTryStrToFloat(vText, Result) then
    ProtoError('number expected');
end;

function RALProtoReadInt32(AValue: TRALJSON): Integer;
var
  vValue: Int64;
begin
  vValue := RALProtoReadInt64(AValue);
  if (vValue < Low(Integer)) or (vValue > High(Integer)) then
    ProtoError('int32 out of range');
  Result := Integer(vValue);
end;

function RALProtoReadInt64(AValue: TRALJSON): Int64;
var
  vValue: UInt64RAL;
  vNegative: Boolean;
begin
  vValue := ParseDecimal(IntegerText(AValue), vNegative);
  {$IF DEFINED(FPC) OR DEFINED(DELPHIXE2UP)}
  if (vNegative and (vValue > UInt64RAL(High(Int64)) + 1)) or
     ((not vNegative) and (vValue > UInt64RAL(High(Int64)))) then
    ProtoError('int64 out of range');
  {$IFEND}
  if vNegative then
    Result := -Int64(vValue)
  else
    Result := Int64(vValue);
end;

function RALProtoReadSingle(AValue: TRALJSON): Single;
var
  vValue: Double;
begin
  vValue := RALProtoReadDouble(AValue);
  if not IsNan(vValue) and not IsInfinite(vValue) and
     (Abs(vValue) > 3.4028234663852886e38) then
    ProtoError('float out of range');
  Result := vValue;
end;

function RALProtoReadString(AValue: TRALJSON): StringRAL;
begin
  if (AValue = nil) or (AValue.Kind <> rjkString) then
    ProtoError('string expected');
  Result := TRALJSONString(AValue).Value;
end;

function RALProtoReadUInt32(AValue: TRALJSON): Cardinal;
var
  vValue: UInt64RAL;
  vNegative: Boolean;
begin
  vValue := ParseDecimal(IntegerText(AValue), vNegative);
  if (vNegative and (vValue <> 0)) or (vValue > High(Cardinal)) then
    ProtoError('uint32 out of range');
  Result := Cardinal(vValue);
end;

function RALProtoReadUInt64(AValue: TRALJSON): UInt64RAL;
var
  vNegative: Boolean;
begin
  Result := ParseDecimal(IntegerText(AValue), vNegative);
  if vNegative and (Result <> 0) then
    ProtoError('uint64 out of range');
end;

{ TRALProtoWriter }

function TRALProtoWriter.BeginLength: IntegerRAL;
begin
  Result := FPosition;
end;

procedure TRALProtoWriter.Clear;
begin
  FPosition := 0;
end;

procedure TRALProtoWriter.EndLength(AMark: IntegerRAL);
var
  vLength, vSize, vInt: IntegerRAL;
  vValue: Cardinal;
begin
  vLength := FPosition - AMark;
  vSize := VarintSize(vLength);
  Grow(vSize);
  if vLength > 0 then
    Move(FBuffer[AMark], FBuffer[AMark + vSize], vLength);
  vValue := vLength;
  for vInt := 0 to vSize - 1 do
  begin
    if vInt < vSize - 1 then
      FBuffer[AMark + vInt] := Byte(vValue and $7F) or $80
    else
      FBuffer[AMark + vInt] := Byte(vValue and $7F);
    vValue := vValue shr 7;
  end;
  Inc(FPosition, vSize);
end;

procedure TRALProtoWriter.Grow(ACount: IntegerRAL);
var
  vNew: IntegerRAL;
begin
  if FPosition + ACount <= Length(FBuffer) then
    Exit;
  vNew := Length(FBuffer) * 2;
  if vNew < 256 then
    vNew := 256;
  while vNew < FPosition + ACount do
    vNew := vNew * 2;
  SetLength(FBuffer, vNew);
end;

procedure TRALProtoWriter.SaveToStream(AStream: TStream);
begin
  if FPosition > 0 then
    AStream.WriteBuffer(FBuffer[0], FPosition);
end;

function TRALProtoWriter.ToBytes: TBytes;
begin
  SetLength(Result, FPosition);
  if FPosition > 0 then
    Move(FBuffer[0], Result[0], FPosition);
end;

procedure TRALProtoWriter.WriteBool(AValue: Boolean);
begin
  Grow(1);
  FBuffer[FPosition] := Ord(AValue);
  Inc(FPosition);
end;

procedure TRALProtoWriter.WriteBytes(const AValue: TBytes);
begin
  WriteVarint(Length(AValue));
  WriteRawBytes(AValue);
end;

procedure TRALProtoWriter.WriteDouble(AValue: Double);
begin
  WriteFixed64(RALProtoDoubleBits(AValue));
end;

procedure TRALProtoWriter.WriteEnum(AValue: Integer);
begin
  WriteInt32(AValue);
end;

procedure TRALProtoWriter.WriteFixed32(AValue: Cardinal);
begin
  Grow(4);
  FBuffer[FPosition] := Byte(AValue);
  FBuffer[FPosition + 1] := Byte(AValue shr 8);
  FBuffer[FPosition + 2] := Byte(AValue shr 16);
  FBuffer[FPosition + 3] := Byte(AValue shr 24);
  Inc(FPosition, 4);
end;

procedure TRALProtoWriter.WriteFixed64(AValue: UInt64RAL);
var
  vInt: IntegerRAL;
begin
  Grow(8);
  for vInt := 0 to 7 do
  begin
    FBuffer[FPosition + vInt] := Byte(AValue);
    AValue := AValue shr 8;
  end;
  Inc(FPosition, 8);
end;

procedure TRALProtoWriter.WriteFloat(AValue: Single);
begin
  WriteFixed32(RALProtoSingleBits(AValue));
end;

procedure TRALProtoWriter.WriteInt32(AValue: Integer);
begin
  // a negative int32 is sign-extended to 64 bits
  WriteVarint(UInt64RAL(Int64(AValue)));
end;

procedure TRALProtoWriter.WriteInt64(AValue: Int64);
begin
  WriteVarint(UInt64RAL(AValue));
end;

procedure TRALProtoWriter.WriteMessage(AMessage: TRALProtoMessage);
var
  vMark: IntegerRAL;
begin
  vMark := BeginLength;
  if AMessage <> nil then
    AMessage.EncodeTo(Self);
  EndLength(vMark);
end;

procedure TRALProtoWriter.WriteRaw(const AData; ACount: IntegerRAL);
begin
  if ACount <= 0 then
    Exit;
  Grow(ACount);
  Move(AData, FBuffer[FPosition], ACount);
  Inc(FPosition, ACount);
end;

procedure TRALProtoWriter.WriteRawBytes(const AValue: TBytes);
begin
  if Length(AValue) > 0 then
    WriteRaw(AValue[0], Length(AValue));
end;

procedure TRALProtoWriter.WriteRepeatedBool(AField: IntegerRAL;
  const AValues: TRALProtoBoolArray; APacked: Boolean);
var
  vInt, vMark: IntegerRAL;
begin
  if Length(AValues) = 0 then
    Exit;
  if APacked then
  begin
    WriteTag(AField, rpwLengthDelimited);
    vMark := BeginLength;
    for vInt := 0 to High(AValues) do
      WriteBool(AValues[vInt]);
    EndLength(vMark);
  end
  else
    for vInt := 0 to High(AValues) do
    begin
      WriteTag(AField, rpwVarint);
      WriteBool(AValues[vInt]);
    end;
end;

procedure TRALProtoWriter.WriteRepeatedBytes(AField: IntegerRAL;
  const AValues: TRALProtoBytesArray);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to High(AValues) do
  begin
    WriteTag(AField, rpwLengthDelimited);
    WriteBytes(AValues[vInt]);
  end;
end;

procedure TRALProtoWriter.WriteRepeatedDouble(AField: IntegerRAL;
  const AValues: TRALProtoDoubleArray; APacked: Boolean);
var
  vInt: IntegerRAL;
begin
  if Length(AValues) = 0 then
    Exit;
  if APacked then
  begin
    WriteTag(AField, rpwLengthDelimited);
    WriteVarint(Length(AValues) * 8);
    for vInt := 0 to High(AValues) do
      WriteDouble(AValues[vInt]);
  end
  else
    for vInt := 0 to High(AValues) do
    begin
      WriteTag(AField, rpwFixed64);
      WriteDouble(AValues[vInt]);
    end;
end;

procedure TRALProtoWriter.WriteRepeatedInt32(AField: IntegerRAL;
  AKind: TRALProtoScalar; const AValues: TRALProtoInt32Array; APacked: Boolean);
var
  vInt, vMark: IntegerRAL;

  procedure WriteOne(AValue: Integer);
  begin
    case AKind of
      rpsSInt32: WriteSInt32(AValue);
      rpsSFixed32: WriteSFixed32(AValue);
    else
      WriteInt32(AValue);
    end;
  end;

begin
  if Length(AValues) = 0 then
    Exit;
  if APacked then
  begin
    WriteTag(AField, rpwLengthDelimited);
    vMark := BeginLength;
    for vInt := 0 to High(AValues) do
      WriteOne(AValues[vInt]);
    EndLength(vMark);
  end
  else
    for vInt := 0 to High(AValues) do
    begin
      if AKind = rpsSFixed32 then
        WriteTag(AField, rpwFixed32)
      else
        WriteTag(AField, rpwVarint);
      WriteOne(AValues[vInt]);
    end;
end;

procedure TRALProtoWriter.WriteRepeatedInt64(AField: IntegerRAL;
  AKind: TRALProtoScalar; const AValues: TRALProtoInt64Array; APacked: Boolean);
var
  vInt, vMark: IntegerRAL;

  procedure WriteOne(AValue: Int64);
  begin
    case AKind of
      rpsSInt64: WriteSInt64(AValue);
      rpsSFixed64: WriteSFixed64(AValue);
    else
      WriteInt64(AValue);
    end;
  end;

begin
  if Length(AValues) = 0 then
    Exit;
  if APacked then
  begin
    WriteTag(AField, rpwLengthDelimited);
    vMark := BeginLength;
    for vInt := 0 to High(AValues) do
      WriteOne(AValues[vInt]);
    EndLength(vMark);
  end
  else
    for vInt := 0 to High(AValues) do
    begin
      if AKind = rpsSFixed64 then
        WriteTag(AField, rpwFixed64)
      else
        WriteTag(AField, rpwVarint);
      WriteOne(AValues[vInt]);
    end;
end;

procedure TRALProtoWriter.WriteRepeatedSingle(AField: IntegerRAL;
  const AValues: TRALProtoSingleArray; APacked: Boolean);
var
  vInt: IntegerRAL;
begin
  if Length(AValues) = 0 then
    Exit;
  if APacked then
  begin
    WriteTag(AField, rpwLengthDelimited);
    WriteVarint(Length(AValues) * 4);
    for vInt := 0 to High(AValues) do
      WriteFloat(AValues[vInt]);
  end
  else
    for vInt := 0 to High(AValues) do
    begin
      WriteTag(AField, rpwFixed32);
      WriteFloat(AValues[vInt]);
    end;
end;

procedure TRALProtoWriter.WriteRepeatedString(AField: IntegerRAL;
  const AValues: TRALProtoStringArray);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to High(AValues) do
  begin
    WriteTag(AField, rpwLengthDelimited);
    WriteString(AValues[vInt]);
  end;
end;

procedure TRALProtoWriter.WriteRepeatedUInt32(AField: IntegerRAL;
  AKind: TRALProtoScalar; const AValues: TRALProtoUInt32Array; APacked: Boolean);
var
  vInt, vMark: IntegerRAL;
begin
  if Length(AValues) = 0 then
    Exit;
  if APacked then
  begin
    WriteTag(AField, rpwLengthDelimited);
    vMark := BeginLength;
    for vInt := 0 to High(AValues) do
      if AKind = rpsFixed32 then
        WriteFixed32(AValues[vInt])
      else
        WriteUInt32(AValues[vInt]);
    EndLength(vMark);
  end
  else
    for vInt := 0 to High(AValues) do
      if AKind = rpsFixed32 then
      begin
        WriteTag(AField, rpwFixed32);
        WriteFixed32(AValues[vInt]);
      end
      else
      begin
        WriteTag(AField, rpwVarint);
        WriteUInt32(AValues[vInt]);
      end;
end;

procedure TRALProtoWriter.WriteRepeatedUInt64(AField: IntegerRAL;
  AKind: TRALProtoScalar; const AValues: TRALProtoUInt64Array; APacked: Boolean);
var
  vInt, vMark: IntegerRAL;
begin
  if Length(AValues) = 0 then
    Exit;
  if APacked then
  begin
    WriteTag(AField, rpwLengthDelimited);
    vMark := BeginLength;
    for vInt := 0 to High(AValues) do
      if AKind = rpsFixed64 then
        WriteFixed64(AValues[vInt])
      else
        WriteUInt64(AValues[vInt]);
    EndLength(vMark);
  end
  else
    for vInt := 0 to High(AValues) do
      if AKind = rpsFixed64 then
      begin
        WriteTag(AField, rpwFixed64);
        WriteFixed64(AValues[vInt]);
      end
      else
      begin
        WriteTag(AField, rpwVarint);
        WriteUInt64(AValues[vInt]);
      end;
end;

procedure TRALProtoWriter.WriteSFixed32(AValue: Integer);
begin
  WriteFixed32(Cardinal(AValue));
end;

procedure TRALProtoWriter.WriteSFixed64(AValue: Int64);
begin
  WriteFixed64(UInt64RAL(AValue));
end;

procedure TRALProtoWriter.WriteSInt32(AValue: Integer);
begin
  WriteVarint(ZigZag32(AValue));
end;

procedure TRALProtoWriter.WriteSInt64(AValue: Int64);
begin
  WriteVarint(ZigZag64(AValue));
end;

procedure TRALProtoWriter.WriteString(const AValue: StringRAL);
begin
  WriteVarint(Length(AValue));
  if Length(AValue) > 0 then
    WriteRaw(AValue[1], Length(AValue));
end;

procedure TRALProtoWriter.WriteTag(AField: IntegerRAL; AWire: TRALProtoWireType);
begin
  WriteVarint((Cardinal(AField) shl 3) or Cardinal(Ord(AWire)));
end;

procedure TRALProtoWriter.WriteUInt32(AValue: Cardinal);
begin
  WriteVarint(AValue);
end;

procedure TRALProtoWriter.WriteUInt64(AValue: UInt64RAL);
begin
  WriteVarint(AValue);
end;

procedure TRALProtoWriter.WriteVarint(AValue: UInt64RAL);
begin
  Grow(10);
  while (AValue and not UInt64RAL($7F)) <> 0 do
  begin
    FBuffer[FPosition] := Byte(AValue and $7F) or $80;
    Inc(FPosition);
    AValue := AValue shr 7;
  end;
  FBuffer[FPosition] := Byte(AValue);
  Inc(FPosition);
end;

{ TRALProtoReader }

constructor TRALProtoReader.Create(AData: Pointer; ASize: IntegerRAL);
begin
  inherited Create;
  FData := AData;
  FSize := ASize;
  FLimit := ASize;
  FMaxDepth := cDefaultMaxDepth;
end;

constructor TRALProtoReader.Create(const ABytes: TBytes);
begin
  FBytes := ABytes;
  if Length(FBytes) > 0 then
    Create(@FBytes[0], Length(FBytes))
  else
    Create(nil, 0);
end;

function TRALProtoReader.EOF: Boolean;
begin
  Result := FPosition >= FLimit;
end;

procedure TRALProtoReader.Need(ACount: IntegerRAL);
begin
  if (ACount < 0) or (FPosition + ACount > FLimit) then
    ProtoError('truncated message');
end;

procedure TRALProtoReader.PopLimit(AOldLimit: IntegerRAL);
begin
  FLimit := AOldLimit;
end;

function TRALProtoReader.PushLimit(ALength: IntegerRAL): IntegerRAL;
begin
  Need(ALength);
  Result := FLimit;
  FLimit := FPosition + ALength;
end;

function TRALProtoReader.ReadBool: Boolean;
begin
  Result := ReadVarint <> 0;
end;

function TRALProtoReader.ReadBytes: TBytes;
var
  vLength: IntegerRAL;
begin
  vLength := ReadLength;
  SetLength(Result, vLength);
  if vLength > 0 then
    Move(PByte(NativeUInt(FData) + NativeUInt(FPosition))^, Result[0], vLength);
  Inc(FPosition, vLength);
end;

function TRALProtoReader.ReadDouble: Double;
var
  vBits: UInt64RAL;
begin
  vBits := ReadFixed64;
  Move(vBits, Result, SizeOf(Result));
end;

function TRALProtoReader.ReadEnum: Integer;
begin
  Result := ReadInt32;
end;

function TRALProtoReader.ReadFixed32: Cardinal;
var
  vData: PByte;
begin
  Need(4);
  vData := PByte(NativeUInt(FData) + NativeUInt(FPosition));
  Result := Cardinal(vData^) or (Cardinal(PByte(NativeUInt(vData) + 1)^) shl 8) or
    (Cardinal(PByte(NativeUInt(vData) + 2)^) shl 16) or
    (Cardinal(PByte(NativeUInt(vData) + 3)^) shl 24);
  Inc(FPosition, 4);
end;

function TRALProtoReader.ReadFixed64: UInt64RAL;
var
  vInt: IntegerRAL;
  vData: PByte;
begin
  Need(8);
  vData := PByte(NativeUInt(FData) + NativeUInt(FPosition));
  Result := 0;
  for vInt := 7 downto 0 do
    Result := (Result shl 8) or PByte(NativeUInt(vData) + NativeUInt(vInt))^;
  Inc(FPosition, 8);
end;

function TRALProtoReader.ReadFloat: Single;
var
  vBits: Cardinal;
begin
  vBits := ReadFixed32;
  Move(vBits, Result, SizeOf(Result));
end;

function TRALProtoReader.ReadInt32: Integer;
begin
  Result := Integer(Cardinal(ReadVarint and $FFFFFFFF));
end;

function TRALProtoReader.ReadInt64: Int64;
begin
  Result := Int64(ReadVarint);
end;

function TRALProtoReader.ReadLength: IntegerRAL;
var
  vValue: UInt64RAL;
begin
  vValue := ReadVarint;
  {$IF DEFINED(FPC) OR DEFINED(DELPHIXE2UP)}
  if vValue > UInt64RAL(FLimit - FPosition) then
  {$ELSE}
  if (vValue < 0) or (vValue > FLimit - FPosition) then
  {$IFEND}
    ProtoError('length past the end of the message');
  Result := IntegerRAL(vValue);
end;

procedure TRALProtoReader.ReadMessage(AMessage: TRALProtoMessage);
var
  vOld: IntegerRAL;
begin
  vOld := PushLimit(ReadLength);
  Inc(FDepth);
  try
    if FDepth > FMaxDepth then
      ProtoError('messages nested too deep');
    AMessage.MergeFrom(Self);
  finally
    Dec(FDepth);
  end;
  PopLimit(vOld);
end;

procedure TRALProtoReader.ReadRepeatedBool(AWire: TRALProtoWireType;
  var AValues: TRALProtoBoolArray);
var
  vOld: IntegerRAL;
begin
  if AWire = rpwLengthDelimited then
  begin
    vOld := PushLimit(ReadLength);
    while not EOF do
      RALProtoAddBool(AValues, ReadBool);
    PopLimit(vOld);
  end
  else
    RALProtoAddBool(AValues, ReadBool);
end;

procedure TRALProtoReader.ReadRepeatedDouble(AWire: TRALProtoWireType;
  var AValues: TRALProtoDoubleArray);
var
  vOld, vCount, vBase, vInt: IntegerRAL;
begin
  if AWire = rpwLengthDelimited then
  begin
    vOld := PushLimit(ReadLength);
    vCount := (FLimit - FPosition) div 8;
    vBase := Length(AValues);
    SetLength(AValues, vBase + vCount);
    for vInt := 0 to vCount - 1 do
      AValues[vBase + vInt] := ReadDouble;
    if not EOF then
      ProtoError('packed double run with a partial value');
    PopLimit(vOld);
  end
  else
    RALProtoAddDouble(AValues, ReadDouble);
end;

procedure TRALProtoReader.ReadRepeatedInt32(AWire: TRALProtoWireType;
  AKind: TRALProtoScalar; var AValues: TRALProtoInt32Array);
var
  vOld: IntegerRAL;

  function ReadOne: Integer;
  begin
    case AKind of
      rpsSInt32: Result := ReadSInt32;
      rpsSFixed32: Result := ReadSFixed32;
    else
      Result := ReadInt32;
    end;
  end;

begin
  if AWire = rpwLengthDelimited then
  begin
    vOld := PushLimit(ReadLength);
    while not EOF do
      RALProtoAddInt32(AValues, ReadOne);
    PopLimit(vOld);
  end
  else
    RALProtoAddInt32(AValues, ReadOne);
end;

procedure TRALProtoReader.ReadRepeatedInt64(AWire: TRALProtoWireType;
  AKind: TRALProtoScalar; var AValues: TRALProtoInt64Array);
var
  vOld: IntegerRAL;

  function ReadOne: Int64;
  begin
    case AKind of
      rpsSInt64: Result := ReadSInt64;
      rpsSFixed64: Result := ReadSFixed64;
    else
      Result := ReadInt64;
    end;
  end;

begin
  if AWire = rpwLengthDelimited then
  begin
    vOld := PushLimit(ReadLength);
    while not EOF do
      RALProtoAddInt64(AValues, ReadOne);
    PopLimit(vOld);
  end
  else
    RALProtoAddInt64(AValues, ReadOne);
end;

procedure TRALProtoReader.ReadRepeatedSingle(AWire: TRALProtoWireType;
  var AValues: TRALProtoSingleArray);
var
  vOld, vCount, vBase, vInt: IntegerRAL;
begin
  if AWire = rpwLengthDelimited then
  begin
    vOld := PushLimit(ReadLength);
    vCount := (FLimit - FPosition) div 4;
    vBase := Length(AValues);
    SetLength(AValues, vBase + vCount);
    for vInt := 0 to vCount - 1 do
      AValues[vBase + vInt] := ReadFloat;
    if not EOF then
      ProtoError('packed float run with a partial value');
    PopLimit(vOld);
  end
  else
    RALProtoAddSingle(AValues, ReadFloat);
end;

procedure TRALProtoReader.ReadRepeatedUInt32(AWire: TRALProtoWireType;
  AKind: TRALProtoScalar; var AValues: TRALProtoUInt32Array);
var
  vOld: IntegerRAL;
begin
  if AWire = rpwLengthDelimited then
  begin
    vOld := PushLimit(ReadLength);
    while not EOF do
      if AKind = rpsFixed32 then
        RALProtoAddUInt32(AValues, ReadFixed32)
      else
        RALProtoAddUInt32(AValues, ReadUInt32);
    PopLimit(vOld);
  end
  else if AKind = rpsFixed32 then
    RALProtoAddUInt32(AValues, ReadFixed32)
  else
    RALProtoAddUInt32(AValues, ReadUInt32);
end;

procedure TRALProtoReader.ReadRepeatedUInt64(AWire: TRALProtoWireType;
  AKind: TRALProtoScalar; var AValues: TRALProtoUInt64Array);
var
  vOld: IntegerRAL;
begin
  if AWire = rpwLengthDelimited then
  begin
    vOld := PushLimit(ReadLength);
    while not EOF do
      if AKind = rpsFixed64 then
        RALProtoAddUInt64(AValues, ReadFixed64)
      else
        RALProtoAddUInt64(AValues, ReadUInt64);
    PopLimit(vOld);
  end
  else if AKind = rpsFixed64 then
    RALProtoAddUInt64(AValues, ReadFixed64)
  else
    RALProtoAddUInt64(AValues, ReadUInt64);
end;

function TRALProtoReader.ReadSFixed32: Integer;
begin
  Result := Integer(ReadFixed32);
end;

function TRALProtoReader.ReadSFixed64: Int64;
begin
  Result := Int64(ReadFixed64);
end;

function TRALProtoReader.ReadSInt32: Integer;
begin
  Result := UnZigZag32(Cardinal(ReadVarint and $FFFFFFFF));
end;

function TRALProtoReader.ReadSInt64: Int64;
begin
  Result := UnZigZag64(ReadVarint);
end;

function TRALProtoReader.ReadString: StringRAL;
var
  vLength: IntegerRAL;
begin
  vLength := ReadLength;
  SetLength(Result, vLength);
  if vLength > 0 then
    Move(PByte(NativeUInt(FData) + NativeUInt(FPosition))^, Result[1], vLength);
  Inc(FPosition, vLength);
end;

function TRALProtoReader.ReadTag(out AField: IntegerRAL;
  out AWire: TRALProtoWireType): Boolean;
var
  vTag: UInt64RAL;
  vWire: IntegerRAL;
begin
  Result := not EOF;
  AField := 0;
  AWire := rpwVarint;
  if not Result then
    Exit;
  FTagPosition := FPosition;
  vTag := ReadVarint;
  {$IF DEFINED(FPC) OR DEFINED(DELPHIXE2UP)}
  if vTag > $FFFFFFFF then
  {$ELSE}
  if (vTag < 0) or (vTag > $FFFFFFFF) then
  {$IFEND}
    ProtoError('tag out of range');
  vWire := IntegerRAL(vTag and 7);
  AField := IntegerRAL(vTag shr 3);
  if (AField = 0) or (vWire > Ord(High(TRALProtoWireType))) then
    ProtoError('invalid tag');
  AWire := TRALProtoWireType(vWire);
end;

function TRALProtoReader.ReadUInt32: Cardinal;
begin
  Result := Cardinal(ReadVarint and $FFFFFFFF);
end;

function TRALProtoReader.ReadUInt64: UInt64RAL;
begin
  Result := ReadVarint;
end;

function TRALProtoReader.ReadVarint: UInt64RAL;
var
  vShift: IntegerRAL;
  vByte: Byte;
begin
  Result := 0;
  vShift := 0;
  repeat
    if FPosition >= FLimit then
      ProtoError('truncated varint');
    vByte := PByte(NativeUInt(FData) + NativeUInt(FPosition))^;
    Inc(FPosition);
    if vShift < 64 then
      Result := Result or (UInt64RAL(vByte and $7F) shl vShift);
    Inc(vShift, 7);
    if (vShift >= 70) and ((vByte and $80) <> 0) then
      ProtoError('varint longer than 10 bytes');
  until (vByte and $80) = 0;
end;

procedure TRALProtoReader.SkipField(AField: IntegerRAL; AWire: TRALProtoWireType);
begin
  case AWire of
    rpwVarint: ReadVarint;
    rpwFixed64:
      begin
        Need(8);
        Inc(FPosition, 8);
      end;
    rpwLengthDelimited: Inc(FPosition, ReadLength);
    rpwStartGroup: SkipGroup(AField);
    rpwFixed32:
      begin
        Need(4);
        Inc(FPosition, 4);
      end;
  else
    ProtoError('unexpected end-group tag');
  end;
end;

procedure TRALProtoReader.SkipGroup(AField: IntegerRAL);
var
  vField: IntegerRAL;
  vWire: TRALProtoWireType;
begin
  Inc(FDepth);
  try
    if FDepth > FMaxDepth then
      ProtoError('groups nested too deep');
    while True do
    begin
      if not ReadTag(vField, vWire) then
        ProtoError('group without its end tag');
      if vWire = rpwEndGroup then
      begin
        if vField <> AField then
          ProtoError('end-group tag of another group');
        Break;
      end;
      SkipField(vField, vWire);
    end;
  finally
    Dec(FDepth);
  end;
end;

{ TRALProtoMessage }

constructor TRALProtoMessage.Create;
begin
  inherited Create;
end;

procedure TRALProtoMessage.AddUnknownVarint(AField: IntegerRAL; AValue: UInt64RAL);
var
  vWriter: TRALProtoWriter;
begin
  vWriter := TRALProtoWriter.Create;
  try
    vWriter.WriteRawBytes(FUnknownFields);
    vWriter.WriteTag(AField, rpwVarint);
    vWriter.WriteVarint(AValue);
    FUnknownFields := vWriter.ToBytes;
  finally
    vWriter.Free;
  end;
end;

procedure TRALProtoMessage.Assign(ASource: TPersistent);
begin
  if (ASource <> nil) and (ASource.ClassType = ClassType) then
    Decode(TRALProtoMessage(ASource).Encode)
  else
    inherited Assign(ASource);
end;

class function TRALProtoMessage.ProtoName: StringRAL;
begin
  Result := '';
end;

procedure TRALProtoMessage.Clear;
begin
  ResetFields;
  FUnknownFields := nil;
end;

function TRALProtoMessage.Clone: TRALProtoMessage;
begin
  Result := TRALProtoMessageClass(ClassType).Create;
  try
    Result.Assign(Self);
  except
    Result.Free;
    raise;
  end;
end;

procedure TRALProtoMessage.Decode(const ABytes: TBytes);
var
  vReader: TRALProtoReader;
begin
  Clear;
  vReader := TRALProtoReader.Create(ABytes);
  try
    MergeFrom(vReader);
  finally
    vReader.Free;
  end;
end;

procedure TRALProtoMessage.Decode(AData: Pointer; ASize: IntegerRAL);
var
  vReader: TRALProtoReader;
begin
  Clear;
  vReader := TRALProtoReader.Create(AData, ASize);
  try
    MergeFrom(vReader);
  finally
    vReader.Free;
  end;
end;

function TRALProtoMessage.DecodeField(AReader: TRALProtoReader;
  AField: IntegerRAL; AWire: TRALProtoWireType): Boolean;
begin
  Result := False;
end;

procedure TRALProtoMessage.DecodeFromStream(AStream: TStream);
var
  vBytes: TBytes;
  vSize: Int64;
begin
  // a memory stream is read where it is, any other is read once into memory
  if AStream is TCustomMemoryStream then
  begin
    vSize := AStream.Size - AStream.Position;
    Decode(Pointer(NativeUInt(TCustomMemoryStream(AStream).Memory) +
      NativeUInt(AStream.Position)), IntegerRAL(vSize));
    AStream.Position := AStream.Size;
  end
  else
  begin
    vSize := AStream.Size - AStream.Position;
    SetLength(vBytes, vSize);
    if vSize > 0 then
      AStream.ReadBuffer(vBytes[0], vSize);
    Decode(vBytes);
  end;
end;

function TRALProtoMessage.Encode: TBytes;
var
  vWriter: TRALProtoWriter;
begin
  vWriter := TRALProtoWriter.Create;
  try
    EncodeTo(vWriter);
    Result := vWriter.ToBytes;
  finally
    vWriter.Free;
  end;
end;

procedure TRALProtoMessage.EncodeFields(AWriter: TRALProtoWriter);
begin
  // a message without fields writes nothing
end;

procedure TRALProtoMessage.EncodeTo(AWriter: TRALProtoWriter);
begin
  EncodeFields(AWriter);
  AWriter.WriteRawBytes(FUnknownFields);
end;

procedure TRALProtoMessage.EncodeToStream(AStream: TStream);
var
  vWriter: TRALProtoWriter;
begin
  vWriter := TRALProtoWriter.Create;
  try
    EncodeTo(vWriter);
    vWriter.SaveToStream(AStream);
  finally
    vWriter.Free;
  end;
end;

procedure TRALProtoMessage.FromJSON(const AText: StringRAL;
  AOptions: TRALProtoJSONOptions);
var
  vJSON: TRALJSON;
begin
  Clear;
  vJSON := TRALJSON.ParseJSON(AText);
  try
    ReadJSONValue(vJSON, AOptions);
  finally
    vJSON.Free;
  end;
end;

procedure TRALProtoMessage.MergeFrom(AReader: TRALProtoReader);
var
  vField, vStart, vSize, vOld: IntegerRAL;
  vWire: TRALProtoWireType;
begin
  while AReader.ReadTag(vField, vWire) do
  begin
    vStart := AReader.TagPosition;
    if vWire = rpwEndGroup then
      ProtoError('unexpected end-group tag');
    if not DecodeField(AReader, vField, vWire) then
    begin
      AReader.SkipField(vField, vWire);
      vSize := AReader.Position - vStart;
      vOld := Length(FUnknownFields);
      SetLength(FUnknownFields, vOld + vSize);
      Move(PByte(NativeUInt(AReader.FData) + NativeUInt(vStart))^, FUnknownFields[vOld], vSize);
    end;
  end;
end;

procedure TRALProtoMessage.ReadJSON(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
var
  vInt: IntegerRAL;
  vValue: TRALJSON;
begin
  for vInt := 0 to AObject.Count - 1 do
  begin
    vValue := AObject.Items[vInt];
    // null is the same as absent for every field but google.protobuf.Value:
    // the generated ReadJSONField decides
    if not ReadJSONField(AObject.Names[vInt], vValue, AOptions) and
       not (rpjIgnoreUnknown in AOptions) then
      ProtoError('unknown JSON field ' + string(AObject.Names[vInt]));
  end;
end;

procedure TRALProtoMessage.ReadJSONValue(AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions);
begin
  if (AValue = nil) or (AValue.Kind <> rjkObject) then
    ProtoError('JSON object expected for ' + ClassName);
  ReadJSON(TRALJSONObject(AValue), AOptions);
end;

function TRALProtoMessage.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := False;
end;

procedure TRALProtoMessage.ResetFields;
begin
  // a message without fields has nothing to reset
end;

function TRALProtoMessage.ToJSON(AOptions: TRALProtoJSONOptions): StringRAL;
var
  vJSON: TRALJSON;
begin
  vJSON := ToJSONValue(AOptions);
  try
    Result := vJSON.ToJSON;
  finally
    vJSON.Free;
  end;
end;

function TRALProtoMessage.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
begin
  Result := TRALJSONObject.Create;
  try
    WriteJSONFields(TRALJSONObject(Result), AOptions);
  except
    Result.Free;
    raise;
  end;
end;

procedure TRALProtoMessage.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  // a message without fields is {}
end;

{ TRALProtoMessageList }

constructor TRALProtoMessageList.Create(AItemClass: TRALProtoMessageClass);
begin
  inherited Create;
  FItemClass := AItemClass;
  FItems := TObjectList.Create(True);
end;

destructor TRALProtoMessageList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TRALProtoMessageList.AddNew: TRALProtoMessage;
begin
  Result := FItemClass.Create;
  FItems.Add(Result);
end;

procedure TRALProtoMessageList.Append(AItem: TRALProtoMessage);
begin
  FItems.Add(AItem);
end;

procedure TRALProtoMessageList.Assign(ASource: TRALProtoMessageList);
var
  vInt: IntegerRAL;
begin
  Clear;
  if ASource = nil then
    Exit;
  for vInt := 0 to ASource.Count - 1 do
    FItems.Add(ASource.Messages[vInt].Clone);
end;

procedure TRALProtoMessageList.Clear;
begin
  FItems.Clear;
end;

procedure TRALProtoMessageList.Delete(AIndex: IntegerRAL);
begin
  FItems.Delete(AIndex);
end;

procedure TRALProtoMessageList.EncodeTo(AWriter: TRALProtoWriter; AField: IntegerRAL);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FItems.Count - 1 do
  begin
    AWriter.WriteTag(AField, rpwLengthDelimited);
    AWriter.WriteMessage(TRALProtoMessage(FItems[vInt]));
  end;
end;

function TRALProtoMessageList.Extract(AIndex: IntegerRAL): TRALProtoMessage;
begin
  Result := TRALProtoMessage(FItems[AIndex]);
  FItems.OwnsObjects := False;
  try
    FItems.Delete(AIndex);
  finally
    FItems.OwnsObjects := True;
  end;
end;

function TRALProtoMessageList.GetCount: IntegerRAL;
begin
  Result := FItems.Count;
end;

function TRALProtoMessageList.GetMessage(AIndex: IntegerRAL): TRALProtoMessage;
begin
  Result := TRALProtoMessage(FItems[AIndex]);
end;

procedure TRALProtoMessageList.ReadItem(AReader: TRALProtoReader);
var
  vItem: TRALProtoMessage;
begin
  vItem := FItemClass.Create;
  try
    AReader.ReadMessage(vItem);
  except
    vItem.Free;
    raise;
  end;
  FItems.Add(vItem);
end;

procedure TRALProtoMessageList.ReadJSON(AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions);
var
  vInt: IntegerRAL;
  vItem: TRALJSON;
begin
  if (AValue = nil) or (AValue.Kind = rjkNull) then
    Exit;
  if AValue.Kind <> rjkArray then
    ProtoError('JSON array expected');
  for vInt := 0 to TRALJSONArray(AValue).Count - 1 do
  begin
    vItem := TRALJSONArray(AValue).Items[vInt];
    if vItem = nil then
      ProtoError('JSON value expected in the array');
    AddNew.ReadJSONValue(vItem, AOptions);
  end;
end;

function TRALProtoMessageList.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSONArray;
var
  vInt: IntegerRAL;
begin
  Result := TRALJSONArray.Create;
  try
    for vInt := 0 to FItems.Count - 1 do
      Result.Add(TRALProtoMessage(FItems[vInt]).ToJSONValue(AOptions));
  except
    Result.Free;
    raise;
  end;
end;

initialization

finalization
  FreeAndNil(gMessages);

end.
