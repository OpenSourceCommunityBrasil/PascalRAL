{ The well-known types of google/protobuf (empty, timestamp, duration,
  field_mask, wrappers, struct, any). The classes were written by ralprotoc from
  those .proto files; the JSON forms the proto3 mapping gives them (a Timestamp
  is an RFC 3339 string, a wrapper its value, a Struct any object...) and the
  helpers below each class are written by hand. }
unit RALProtobufWKT;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  RALTypes,
  RALJson,
  RALExceptions,
  RALProtobuf;

type
  /// The JSON null.
  TProtoNullValue = type Integer;
  /// Which field of the oneof kind is set.
  TProtoValue_KindCase = (ProtoValue_Kind_NotSet, ProtoValue_Kind_NullValue, ProtoValue_Kind_NumberValue, ProtoValue_Kind_StringValue, ProtoValue_Kind_BoolValue, ProtoValue_Kind_StructValue, ProtoValue_Kind_ListValue);

const
  /// google.protobuf.NullValue.NULL_VALUE.
  Proto_NULL_VALUE = TProtoNullValue(0);

type
  TProtoEmpty = class;
  TProtoTimestamp = class;
  TProtoDuration = class;
  TProtoFieldMask = class;
  TProtoDoubleValue = class;
  TProtoFloatValue = class;
  TProtoInt64Value = class;
  TProtoUInt64Value = class;
  TProtoInt32Value = class;
  TProtoUInt32Value = class;
  TProtoBoolValue = class;
  TProtoStringValue = class;
  TProtoBytesValue = class;
  TProtoStruct = class;
  TProtoStruct_FieldsEntry = class;
  TProtoValue = class;
  TProtoListValue = class;
  TProtoAny = class;

  /// A repeated Empty field.
  TProtoEmptyList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoEmpty;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoEmpty;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoEmpty read GetItem; default;
  end;

  /// A repeated Timestamp field.
  TProtoTimestampList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoTimestamp;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoTimestamp;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoTimestamp read GetItem; default;
  end;

  /// A repeated Duration field.
  TProtoDurationList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoDuration;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoDuration;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoDuration read GetItem; default;
  end;

  /// A repeated FieldMask field.
  TProtoFieldMaskList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoFieldMask;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoFieldMask;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoFieldMask read GetItem; default;
  end;

  /// A repeated DoubleValue field.
  TProtoDoubleValueList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoDoubleValue;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoDoubleValue;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoDoubleValue read GetItem; default;
  end;

  /// A repeated FloatValue field.
  TProtoFloatValueList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoFloatValue;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoFloatValue;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoFloatValue read GetItem; default;
  end;

  /// A repeated Int64Value field.
  TProtoInt64ValueList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoInt64Value;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoInt64Value;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoInt64Value read GetItem; default;
  end;

  /// A repeated UInt64Value field.
  TProtoUInt64ValueList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoUInt64Value;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoUInt64Value;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoUInt64Value read GetItem; default;
  end;

  /// A repeated Int32Value field.
  TProtoInt32ValueList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoInt32Value;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoInt32Value;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoInt32Value read GetItem; default;
  end;

  /// A repeated UInt32Value field.
  TProtoUInt32ValueList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoUInt32Value;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoUInt32Value;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoUInt32Value read GetItem; default;
  end;

  /// A repeated BoolValue field.
  TProtoBoolValueList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoBoolValue;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoBoolValue;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoBoolValue read GetItem; default;
  end;

  /// A repeated StringValue field.
  TProtoStringValueList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoStringValue;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoStringValue;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoStringValue read GetItem; default;
  end;

  /// A repeated BytesValue field.
  TProtoBytesValueList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoBytesValue;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoBytesValue;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoBytesValue read GetItem; default;
  end;

  /// A repeated Struct field.
  TProtoStructList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoStruct;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoStruct;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoStruct read GetItem; default;
  end;

  /// A repeated FieldsEntry field.
  TProtoStruct_FieldsEntryList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoStruct_FieldsEntry;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoStruct_FieldsEntry;
    /// Removes an earlier entry with the key of the last one (the last wins).
    procedure DropDuplicateKey;
    /// The entry of AKey, nil when there is none.
    function Find(const AKey: StringRAL): TProtoStruct_FieldsEntry;
    /// The entry of AKey, added when there is none.
    function Put(const AKey: StringRAL): TProtoStruct_FieldsEntry;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoStruct_FieldsEntry read GetItem; default;
  end;

  /// A repeated Value field.
  TProtoValueList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoValue;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoValue;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoValue read GetItem; default;
  end;

  /// A repeated ListValue field.
  TProtoListValueList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoListValue;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoListValue;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoListValue read GetItem; default;
  end;

  /// A repeated Any field.
  TProtoAnyList = class(TRALProtoMessageList)
  private
    function GetItem(AIndex: IntegerRAL): TProtoAny;
  public
    /// Creates an empty list.
    constructor Create;

    /// Adds a new empty item and returns it.
    function Add: TProtoAny;

    /// The items.
    property Items[AIndex: IntegerRAL]: TProtoAny read GetItem; default;
  end;

  /// A generic empty message, the request or reply of a method that needs none.
  TProtoEmpty = class(TRALProtoMessage)
  private
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.Empty'.
    class function ProtoName: StringRAL; override;
  end;

  /// A point in time: seconds and nanoseconds from the Unix epoch, no time zone.
  TProtoTimestamp = class(TRALProtoMessage)
  private
    FNanos: Integer;
    FSeconds: Int64;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// The time as a UTC TDateTime (to the millisecond).
    function AsDateTime: TDateTime;
    /// Sets the time from a UTC TDateTime.
    procedure FromDateTime(AValue: TDateTime);
    /// 'google.protobuf.Timestamp'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 2, nanos.
    property Nanos: Integer read FNanos write FNanos;
    /// Field 1, seconds.
    property Seconds: Int64 read FSeconds write FSeconds;
  end;

  /// A signed span of time: seconds and nanoseconds of the same sign.
  TProtoDuration = class(TRALProtoMessage)
  private
    FNanos: Integer;
    FSeconds: Int64;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// The span in milliseconds (nanoseconds below a millisecond dropped).
    function AsMilliseconds: Int64;
    /// Sets the span from milliseconds.
    procedure FromMilliseconds(AValue: Int64);
    /// 'google.protobuf.Duration'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 2, nanos.
    property Nanos: Integer read FNanos write FNanos;
    /// Field 1, seconds.
    property Seconds: Int64 read FSeconds write FSeconds;
  end;

  /// A set of field paths, written in JSON as one comma-separated string.
  TProtoFieldMask = class(TRALProtoMessage)
  private
    FPaths: TRALProtoStringArray;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.FieldMask'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 1, paths.
    property Paths: TRALProtoStringArray read FPaths write FPaths;
  end;

  /// A double that may be absent; in JSON, the number itself.
  TProtoDoubleValue = class(TRALProtoMessage)
  private
    FValue: Double;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.DoubleValue'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 1, value.
    property Value: Double read FValue write FValue;
  end;

  /// A float that may be absent; in JSON, the number itself.
  TProtoFloatValue = class(TRALProtoMessage)
  private
    FValue: Single;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.FloatValue'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 1, value.
    property Value: Single read FValue write FValue;
  end;

  /// An int64 that may be absent; in JSON, the number itself (a string).
  TProtoInt64Value = class(TRALProtoMessage)
  private
    FValue: Int64;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.Int64Value'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 1, value.
    property Value: Int64 read FValue write FValue;
  end;

  /// A uint64 that may be absent; in JSON, the number itself (a string).
  TProtoUInt64Value = class(TRALProtoMessage)
  private
    FValue: UInt64RAL;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.UInt64Value'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 1, value.
    property Value: UInt64RAL read FValue write FValue;
  end;

  /// An int32 that may be absent; in JSON, the number itself.
  TProtoInt32Value = class(TRALProtoMessage)
  private
    FValue: Integer;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.Int32Value'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 1, value.
    property Value: Integer read FValue write FValue;
  end;

  /// A uint32 that may be absent; in JSON, the number itself.
  TProtoUInt32Value = class(TRALProtoMessage)
  private
    FValue: Cardinal;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.UInt32Value'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 1, value.
    property Value: Cardinal read FValue write FValue;
  end;

  /// A bool that may be absent; in JSON, the bool itself.
  TProtoBoolValue = class(TRALProtoMessage)
  private
    FValue: Boolean;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.BoolValue'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 1, value.
    property Value: Boolean read FValue write FValue;
  end;

  /// A string that may be absent; in JSON, the string itself.
  TProtoStringValue = class(TRALProtoMessage)
  private
    FValue: StringRAL;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.StringValue'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 1, value.
    property Value: StringRAL read FValue write FValue;
  end;

  /// Bytes that may be absent; in JSON, the base64 string itself.
  TProtoBytesValue = class(TRALProtoMessage)
  private
    FValue: TBytes;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.BytesValue'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 1, value.
    property Value: TBytes read FValue write FValue;
  end;

  /// A JSON object: names and values.
  TProtoStruct = class(TRALProtoMessage)
  private
    FFields: TProtoStruct_FieldsEntryList;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.Struct'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 1, fields.
    property Fields: TProtoStruct_FieldsEntryList read FFields;
  end;

  /// The .proto message google.protobuf.Struct.FieldsEntry.
  TProtoStruct_FieldsEntry = class(TRALProtoMessage)
  private
    FKey: StringRAL;
    FValue: TProtoValue;

    procedure SetValue(AValue: TProtoValue);
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.Struct.FieldsEntry'.
    class function ProtoName: StringRAL; override;

    /// Field 1, key.
    property Key: StringRAL read FKey write FKey;
    /// Field 2, value.
    property Value: TProtoValue read FValue write SetValue;
  end;

  /// Any JSON value.
  TProtoValue = class(TRALProtoMessage)
  private
    FBoolValue: Boolean;
    FKindCase: TProtoValue_KindCase;
    FListValue: TProtoListValue;
    FNullValue: TProtoNullValue;
    FNumberValue: Double;
    FStringValue: StringRAL;
    FStructValue: TProtoStruct;

    procedure SetBoolValue(AValue: Boolean);
    procedure SetListValue(AValue: TProtoListValue);
    procedure SetNullValue(AValue: TProtoNullValue);
    procedure SetNumberValue(AValue: Double);
    procedure SetStringValue(const AValue: StringRAL);
    procedure SetStructValue(AValue: TProtoStruct);
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// Clears whichever field of the oneof kind is set.
    procedure ClearKind;
    /// 'google.protobuf.Value'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 4, bool_value.
    property BoolValue: Boolean read FBoolValue write SetBoolValue;
    /// Which field of the oneof kind is set.
    property KindCase: TProtoValue_KindCase read FKindCase;
    /// Field 6, list_value.
    property ListValue: TProtoListValue read FListValue write SetListValue;
    /// Field 1, null_value.
    property NullValue: TProtoNullValue read FNullValue write SetNullValue;
    /// Field 2, number_value.
    property NumberValue: Double read FNumberValue write SetNumberValue;
    /// Field 3, string_value.
    property StringValue: StringRAL read FStringValue write SetStringValue;
    /// Field 5, struct_value.
    property StructValue: TProtoStruct read FStructValue write SetStructValue;
  end;

  /// A JSON array.
  TProtoListValue = class(TRALProtoMessage)
  private
    FValues: TProtoValueList;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// 'google.protobuf.ListValue'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;

    /// Field 1, values.
    property Values: TProtoValueList read FValues;
  end;

  /// A message of any type, with the URL of its type.
  TProtoAny = class(TRALProtoMessage)
  private
    FTypeUrl: StringRAL;
    FValue: TBytes;
  protected
    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
      AWire: TRALProtoWireType): Boolean; override;
    procedure EncodeFields(AWriter: TRALProtoWriter); override;
    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
      AOptions: TRALProtoJSONOptions): Boolean; override;
    procedure ResetFields; override;
    procedure WriteJSONFields(AObject: TRALJSONObject;
      AOptions: TRALProtoJSONOptions); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// Packs AMessage: its type URL and its bytes.
    procedure Pack(AMessage: TRALProtoMessage);
    /// 'google.protobuf.Any'.
    class function ProtoName: StringRAL; override;
    /// Reads the JSON form of the type.
    procedure ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions = []); override;
    /// The JSON form of the type.
    function ToJSONValue(AOptions: TRALProtoJSONOptions = []): TRALJSON; override;
    /// A new message of the packed type (registered by its unit); the caller frees it.
    function Unpack: TRALProtoMessage;
    /// Reads the packed message into AMessage; False when the type is another.
    function UnpackTo(AMessage: TRALProtoMessage): Boolean;

    /// Field 1, type_url.
    property TypeUrl: StringRAL read FTypeUrl write FTypeUrl;
    /// Field 2, value.
    property Value: TBytes read FValue write FValue;
  end;

/// The name of a NullValue value, empty for a number without one.
function ProtoNullValueToName(AValue: Integer): StringRAL;
/// The value of a NullValue name; False when there is none.
function ProtoNullValueFromName(const AName: StringRAL; out AValue: Integer): Boolean;
/// A NullValue from JSON (name or number); False for a name the options let pass.
function ProtoNullValueFromJSON(AValue: TRALJSON; AOptions: TRALProtoJSONOptions;
  out AResult: Integer): Boolean;

implementation

uses
  Math;

type
  /// Reaches the protected members of any message (ReadJSONField for Any).
  TRALProtoMessageAccess = class(TRALProtoMessage);

const
  /// The type URL prefix of Any.
  cTypeUrlPrefix = 'type.googleapis.com/';
  /// The most seconds a Timestamp or a Duration may hold (year 9999 / 10000 years).
  cMaxSeconds = Int64(253402300799);
  /// The fewest seconds a Timestamp may hold (year 1).
  cMinSeconds = Int64(-62135596800);
  /// The bound of a Duration in seconds.
  cMaxDuration = Int64(315576000000);
  /// Seconds between 0001-01-01 and the Unix epoch.
  cUnixEpochDays = 25569;

/// Raises a JSON form error of a well-known type.
procedure WKTError(const AText: string);
begin
  raise ERALException.Create('Protobuf: ' + AText);
end;

/// Day number of a civil date, 0 at the Unix epoch (proleptic Gregorian).
function DaysFromCivil(AYear, AMonth, ADay: Int64): Int64;
var
  vEra, vYoe, vDoy, vDoe: Int64;
begin
  if AMonth <= 2 then
    Dec(AYear);
  if AYear >= 0 then
    vEra := AYear div 400
  else
    vEra := (AYear - 399) div 400;
  vYoe := AYear - vEra * 400;
  if AMonth > 2 then
    vDoy := (153 * (AMonth - 3) + 2) div 5 + ADay - 1
  else
    vDoy := (153 * (AMonth + 9) + 2) div 5 + ADay - 1;
  vDoe := vYoe * 365 + vYoe div 4 - vYoe div 100 + vDoy;
  Result := vEra * 146097 + vDoe - 719468;
end;

/// The civil date of a day number, 0 at the Unix epoch.
procedure CivilFromDays(ADays: Int64; out AYear, AMonth, ADay: Int64);
var
  vEra, vDoe, vYoe, vDoy, vMp: Int64;
begin
  ADays := ADays + 719468;
  if ADays >= 0 then
    vEra := ADays div 146097
  else
    vEra := (ADays - 146096) div 146097;
  vDoe := ADays - vEra * 146097;
  vYoe := (vDoe - vDoe div 1460 + vDoe div 36524 - vDoe div 146096) div 365;
  AYear := vYoe + vEra * 400;
  vDoy := vDoe - (365 * vYoe + vYoe div 4 - vYoe div 100);
  vMp := (5 * vDoy + 2) div 153;
  ADay := vDoy - (153 * vMp + 2) div 5 + 1;
  if vMp < 10 then
    AMonth := vMp + 3
  else
    AMonth := vMp - 9;
  if AMonth <= 2 then
    Inc(AYear);
end;

/// The fraction of nanoseconds in 0, 3, 6 or 9 digits, with its dot.
function NanosText(ANanos: Integer): string;
begin
  if ANanos = 0 then
    Result := ''
  else if ANanos mod 1000000 = 0 then
    Result := '.' + Format('%.3d', [ANanos div 1000000])
  else if ANanos mod 1000 = 0 then
    Result := '.' + Format('%.6d', [ANanos div 1000])
  else
    Result := '.' + Format('%.9d', [ANanos]);
end;

/// Reads ACount digits at APos of AText.
function Digits(const AText: string; var APos: IntegerRAL; ACount: IntegerRAL): Int64;
var
  vInt: IntegerRAL;
begin
  Result := 0;
  for vInt := 1 to ACount do
  begin
    if (APos > Length(AText)) or not CharInSet(AText[APos], ['0'..'9']) then
      WKTError('malformed time: ' + AText);
    Result := Result * 10 + Ord(AText[APos]) - Ord('0');
    Inc(APos);
  end;
end;

/// Reads an optional fraction at APos (".123456789") as nanoseconds.
function Fraction(const AText: string; var APos: IntegerRAL): Integer;
var
  vDigits: IntegerRAL;
begin
  Result := 0;
  if (APos > Length(AText)) or (AText[APos] <> '.') then
    Exit;
  Inc(APos);
  vDigits := 0;
  while (APos <= Length(AText)) and CharInSet(AText[APos], ['0'..'9']) do
  begin
    if vDigits = 9 then
      WKTError('more than 9 fractional digits: ' + AText);
    Result := Result * 10 + Ord(AText[APos]) - Ord('0');
    Inc(APos);
    Inc(vDigits);
  end;
  if vDigits = 0 then
    WKTError('empty fraction: ' + AText);
  while vDigits < 9 do
  begin
    Result := Result * 10;
    Inc(vDigits);
  end;
end;

/// A JSON field path (lowerCamelCase) as a .proto path (snake_case).
function SnakePath(const APath: string): string;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 1 to Length(APath) do
  begin
    if APath[vInt] = '_' then
      WKTError('field mask path with "_" in JSON: ' + APath);
    if CharInSet(APath[vInt], ['A'..'Z']) then
      Result := Result + '_' + LowerCase(APath[vInt])
    else
      Result := Result + APath[vInt];
  end;
end;

/// A .proto path (snake_case) as a JSON field path (lowerCamelCase).
function CamelPath(const APath: string): string;
var
  vInt: IntegerRAL;
  vUpper: Boolean;
begin
  Result := '';
  vUpper := False;
  for vInt := 1 to Length(APath) do
    if APath[vInt] = '_' then
    begin
      if (vInt = Length(APath)) or not CharInSet(APath[vInt + 1], ['a'..'z']) then
        WKTError('field mask path that has no JSON form: ' + APath);
      vUpper := True;
    end
    else if vUpper then
    begin
      Result := Result + UpCase(APath[vInt]);
      vUpper := False;
    end
    else
    begin
      if CharInSet(APath[vInt], ['A'..'Z']) then
        WKTError('field mask path that has no JSON form: ' + APath);
      Result := Result + APath[vInt];
    end;
end;

/// Whether a message class has a JSON form of its own (Any then wraps it in "value").
function HasOwnJSON(AClass: TRALProtoMessageClass): Boolean;
begin
  Result := (Copy(AClass.ProtoName, 1, 16) = 'google.protobuf.') and
    (AClass.ProtoName <> 'google.protobuf.Empty');
end;

function ProtoNullValueToName(AValue: Integer): StringRAL;
begin
  case AValue of
    0: Result := 'NULL_VALUE';
  else
    Result := '';
  end;
end;

function ProtoNullValueFromName(const AName: StringRAL; out AValue: Integer): Boolean;
begin
  Result := True;
  if AName = 'NULL_VALUE' then
    AValue := 0
  else
  begin
    AValue := 0;
    Result := False;
  end;
end;

function ProtoNullValueFromJSON(AValue: TRALJSON; AOptions: TRALProtoJSONOptions;
  out AResult: Integer): Boolean;
begin
  Result := True;
  if (AValue <> nil) and (AValue.Kind = rjkString) then
  begin
    Result := ProtoNullValueFromName(TRALJSONString(AValue).Value, AResult);
    if not Result and not (rpjIgnoreUnknown in AOptions) then
      raise ERALException.Create('unknown NullValue name ' + string(TRALJSONString(AValue).Value));
  end
  else if (AValue <> nil) and (AValue.Kind = rjkNull) then
    AResult := 0
  else
    AResult := RALProtoReadInt32(AValue);
end;

{ TProtoEmptyList }

constructor TProtoEmptyList.Create;
begin
  inherited Create(TProtoEmpty);
end;

function TProtoEmptyList.Add: TProtoEmpty;
begin
  Result := TProtoEmpty(AddNew);
end;

function TProtoEmptyList.GetItem(AIndex: IntegerRAL): TProtoEmpty;
begin
  Result := TProtoEmpty(Messages[AIndex]);
end;

{ TProtoEmpty }

constructor TProtoEmpty.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoEmpty.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.Empty';
end;

destructor TProtoEmpty.Destroy;
begin
  inherited Destroy;
end;

function TProtoEmpty.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := False;
end;

procedure TProtoEmpty.EncodeFields(AWriter: TRALProtoWriter);
begin
end;

function TProtoEmpty.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := False;
end;

procedure TProtoEmpty.ResetFields;
begin
end;

procedure TProtoEmpty.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
end;

{ TProtoTimestampList }

constructor TProtoTimestampList.Create;
begin
  inherited Create(TProtoTimestamp);
end;

function TProtoTimestampList.Add: TProtoTimestamp;
begin
  Result := TProtoTimestamp(AddNew);
end;

function TProtoTimestampList.GetItem(AIndex: IntegerRAL): TProtoTimestamp;
begin
  Result := TProtoTimestamp(Messages[AIndex]);
end;

{ TProtoTimestamp }

constructor TProtoTimestamp.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoTimestamp.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.Timestamp';
end;

destructor TProtoTimestamp.Destroy;
begin
  inherited Destroy;
end;

function TProtoTimestamp.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwVarint then
        FSeconds := AReader.ReadInt64
      else
        Result := False;
    2:
      if AWire = rpwVarint then
        FNanos := AReader.ReadInt32
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoTimestamp.EncodeFields(AWriter: TRALProtoWriter);
begin
  if FSeconds <> 0 then
  begin
    AWriter.WriteTag(1, rpwVarint);
    AWriter.WriteInt64(FSeconds);
  end;
  if FNanos <> 0 then
  begin
    AWriter.WriteTag(2, rpwVarint);
    AWriter.WriteInt32(FNanos);
  end;
end;

function TProtoTimestamp.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if AName = 'seconds' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FSeconds := RALProtoReadInt64(AValue);
  end
  else if AName = 'nanos' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FNanos := RALProtoReadInt32(AValue);
  end
  else
    Result := False;
end;

procedure TProtoTimestamp.ResetFields;
begin
  FSeconds := 0;
  FNanos := 0;
end;

procedure TProtoTimestamp.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (FSeconds <> 0) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'seconds', 'seconds'), RALProtoJSONInt64(FSeconds));
  if (FNanos <> 0) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'nanos', 'nanos'), RALProtoJSONInt32(FNanos));
end;

{ TProtoDurationList }

constructor TProtoDurationList.Create;
begin
  inherited Create(TProtoDuration);
end;

function TProtoDurationList.Add: TProtoDuration;
begin
  Result := TProtoDuration(AddNew);
end;

function TProtoDurationList.GetItem(AIndex: IntegerRAL): TProtoDuration;
begin
  Result := TProtoDuration(Messages[AIndex]);
end;

{ TProtoDuration }

constructor TProtoDuration.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoDuration.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.Duration';
end;

destructor TProtoDuration.Destroy;
begin
  inherited Destroy;
end;

function TProtoDuration.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwVarint then
        FSeconds := AReader.ReadInt64
      else
        Result := False;
    2:
      if AWire = rpwVarint then
        FNanos := AReader.ReadInt32
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoDuration.EncodeFields(AWriter: TRALProtoWriter);
begin
  if FSeconds <> 0 then
  begin
    AWriter.WriteTag(1, rpwVarint);
    AWriter.WriteInt64(FSeconds);
  end;
  if FNanos <> 0 then
  begin
    AWriter.WriteTag(2, rpwVarint);
    AWriter.WriteInt32(FNanos);
  end;
end;

function TProtoDuration.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if AName = 'seconds' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FSeconds := RALProtoReadInt64(AValue);
  end
  else if AName = 'nanos' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FNanos := RALProtoReadInt32(AValue);
  end
  else
    Result := False;
end;

procedure TProtoDuration.ResetFields;
begin
  FSeconds := 0;
  FNanos := 0;
end;

procedure TProtoDuration.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (FSeconds <> 0) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'seconds', 'seconds'), RALProtoJSONInt64(FSeconds));
  if (FNanos <> 0) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'nanos', 'nanos'), RALProtoJSONInt32(FNanos));
end;

{ TProtoFieldMaskList }

constructor TProtoFieldMaskList.Create;
begin
  inherited Create(TProtoFieldMask);
end;

function TProtoFieldMaskList.Add: TProtoFieldMask;
begin
  Result := TProtoFieldMask(AddNew);
end;

function TProtoFieldMaskList.GetItem(AIndex: IntegerRAL): TProtoFieldMask;
begin
  Result := TProtoFieldMask(Messages[AIndex]);
end;

{ TProtoFieldMask }

constructor TProtoFieldMask.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoFieldMask.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.FieldMask';
end;

destructor TProtoFieldMask.Destroy;
begin
  inherited Destroy;
end;

function TProtoFieldMask.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwLengthDelimited then
        RALProtoAddString(FPaths, AReader.ReadString)
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoFieldMask.EncodeFields(AWriter: TRALProtoWriter);
begin
  AWriter.WriteRepeatedString(1, FPaths);
end;

function TProtoFieldMask.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
var
  vInt: IntegerRAL;
  vArray: TRALJSONArray;
begin
  Result := True;
  if AName = 'paths' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    vArray := RALProtoArrayOf(AValue);
    for vInt := 0 to vArray.Count - 1 do
      RALProtoAddString(FPaths, RALProtoReadString(vArray.Items[vInt]));
  end
  else
    Result := False;
end;

procedure TProtoFieldMask.ResetFields;
begin
  FPaths := nil;
end;

procedure TProtoFieldMask.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
var
  vInt: IntegerRAL;
  vArray: TRALJSONArray;
begin
  if (Length(FPaths) > 0) or (rpjEmitDefaults in AOptions) then
  begin
    vArray := TRALJSONArray.Create;
    AObject.Add(RALProtoKey(AOptions, 'paths', 'paths'), vArray);
    for vInt := 0 to High(FPaths) do
      vArray.Add(RALProtoJSONString(FPaths[vInt]));
  end;
end;

{ TProtoDoubleValueList }

constructor TProtoDoubleValueList.Create;
begin
  inherited Create(TProtoDoubleValue);
end;

function TProtoDoubleValueList.Add: TProtoDoubleValue;
begin
  Result := TProtoDoubleValue(AddNew);
end;

function TProtoDoubleValueList.GetItem(AIndex: IntegerRAL): TProtoDoubleValue;
begin
  Result := TProtoDoubleValue(Messages[AIndex]);
end;

{ TProtoDoubleValue }

constructor TProtoDoubleValue.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoDoubleValue.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.DoubleValue';
end;

destructor TProtoDoubleValue.Destroy;
begin
  inherited Destroy;
end;

function TProtoDoubleValue.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwFixed64 then
        FValue := AReader.ReadDouble
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoDoubleValue.EncodeFields(AWriter: TRALProtoWriter);
begin
  if RALProtoDoubleBits(FValue) <> 0 then
  begin
    AWriter.WriteTag(1, rpwFixed64);
    AWriter.WriteDouble(FValue);
  end;
end;

function TProtoDoubleValue.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if AName = 'value' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FValue := RALProtoReadDouble(AValue);
  end
  else
    Result := False;
end;

procedure TProtoDoubleValue.ResetFields;
begin
  FValue := 0;
end;

procedure TProtoDoubleValue.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (RALProtoDoubleBits(FValue) <> 0) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'value', 'value'), RALProtoJSONDouble(FValue));
end;

{ TProtoFloatValueList }

constructor TProtoFloatValueList.Create;
begin
  inherited Create(TProtoFloatValue);
end;

function TProtoFloatValueList.Add: TProtoFloatValue;
begin
  Result := TProtoFloatValue(AddNew);
end;

function TProtoFloatValueList.GetItem(AIndex: IntegerRAL): TProtoFloatValue;
begin
  Result := TProtoFloatValue(Messages[AIndex]);
end;

{ TProtoFloatValue }

constructor TProtoFloatValue.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoFloatValue.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.FloatValue';
end;

destructor TProtoFloatValue.Destroy;
begin
  inherited Destroy;
end;

function TProtoFloatValue.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwFixed32 then
        FValue := AReader.ReadFloat
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoFloatValue.EncodeFields(AWriter: TRALProtoWriter);
begin
  if RALProtoSingleBits(FValue) <> 0 then
  begin
    AWriter.WriteTag(1, rpwFixed32);
    AWriter.WriteFloat(FValue);
  end;
end;

function TProtoFloatValue.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if AName = 'value' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FValue := RALProtoReadSingle(AValue);
  end
  else
    Result := False;
end;

procedure TProtoFloatValue.ResetFields;
begin
  FValue := 0;
end;

procedure TProtoFloatValue.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (RALProtoSingleBits(FValue) <> 0) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'value', 'value'), RALProtoJSONSingle(FValue));
end;

{ TProtoInt64ValueList }

constructor TProtoInt64ValueList.Create;
begin
  inherited Create(TProtoInt64Value);
end;

function TProtoInt64ValueList.Add: TProtoInt64Value;
begin
  Result := TProtoInt64Value(AddNew);
end;

function TProtoInt64ValueList.GetItem(AIndex: IntegerRAL): TProtoInt64Value;
begin
  Result := TProtoInt64Value(Messages[AIndex]);
end;

{ TProtoInt64Value }

constructor TProtoInt64Value.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoInt64Value.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.Int64Value';
end;

destructor TProtoInt64Value.Destroy;
begin
  inherited Destroy;
end;

function TProtoInt64Value.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwVarint then
        FValue := AReader.ReadInt64
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoInt64Value.EncodeFields(AWriter: TRALProtoWriter);
begin
  if FValue <> 0 then
  begin
    AWriter.WriteTag(1, rpwVarint);
    AWriter.WriteInt64(FValue);
  end;
end;

function TProtoInt64Value.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if AName = 'value' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FValue := RALProtoReadInt64(AValue);
  end
  else
    Result := False;
end;

procedure TProtoInt64Value.ResetFields;
begin
  FValue := 0;
end;

procedure TProtoInt64Value.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (FValue <> 0) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'value', 'value'), RALProtoJSONInt64(FValue));
end;

{ TProtoUInt64ValueList }

constructor TProtoUInt64ValueList.Create;
begin
  inherited Create(TProtoUInt64Value);
end;

function TProtoUInt64ValueList.Add: TProtoUInt64Value;
begin
  Result := TProtoUInt64Value(AddNew);
end;

function TProtoUInt64ValueList.GetItem(AIndex: IntegerRAL): TProtoUInt64Value;
begin
  Result := TProtoUInt64Value(Messages[AIndex]);
end;

{ TProtoUInt64Value }

constructor TProtoUInt64Value.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoUInt64Value.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.UInt64Value';
end;

destructor TProtoUInt64Value.Destroy;
begin
  inherited Destroy;
end;

function TProtoUInt64Value.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwVarint then
        FValue := AReader.ReadUInt64
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoUInt64Value.EncodeFields(AWriter: TRALProtoWriter);
begin
  if FValue <> 0 then
  begin
    AWriter.WriteTag(1, rpwVarint);
    AWriter.WriteUInt64(FValue);
  end;
end;

function TProtoUInt64Value.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if AName = 'value' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FValue := RALProtoReadUInt64(AValue);
  end
  else
    Result := False;
end;

procedure TProtoUInt64Value.ResetFields;
begin
  FValue := 0;
end;

procedure TProtoUInt64Value.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (FValue <> 0) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'value', 'value'), RALProtoJSONUInt64(FValue));
end;

{ TProtoInt32ValueList }

constructor TProtoInt32ValueList.Create;
begin
  inherited Create(TProtoInt32Value);
end;

function TProtoInt32ValueList.Add: TProtoInt32Value;
begin
  Result := TProtoInt32Value(AddNew);
end;

function TProtoInt32ValueList.GetItem(AIndex: IntegerRAL): TProtoInt32Value;
begin
  Result := TProtoInt32Value(Messages[AIndex]);
end;

{ TProtoInt32Value }

constructor TProtoInt32Value.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoInt32Value.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.Int32Value';
end;

destructor TProtoInt32Value.Destroy;
begin
  inherited Destroy;
end;

function TProtoInt32Value.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwVarint then
        FValue := AReader.ReadInt32
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoInt32Value.EncodeFields(AWriter: TRALProtoWriter);
begin
  if FValue <> 0 then
  begin
    AWriter.WriteTag(1, rpwVarint);
    AWriter.WriteInt32(FValue);
  end;
end;

function TProtoInt32Value.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if AName = 'value' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FValue := RALProtoReadInt32(AValue);
  end
  else
    Result := False;
end;

procedure TProtoInt32Value.ResetFields;
begin
  FValue := 0;
end;

procedure TProtoInt32Value.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (FValue <> 0) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'value', 'value'), RALProtoJSONInt32(FValue));
end;

{ TProtoUInt32ValueList }

constructor TProtoUInt32ValueList.Create;
begin
  inherited Create(TProtoUInt32Value);
end;

function TProtoUInt32ValueList.Add: TProtoUInt32Value;
begin
  Result := TProtoUInt32Value(AddNew);
end;

function TProtoUInt32ValueList.GetItem(AIndex: IntegerRAL): TProtoUInt32Value;
begin
  Result := TProtoUInt32Value(Messages[AIndex]);
end;

{ TProtoUInt32Value }

constructor TProtoUInt32Value.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoUInt32Value.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.UInt32Value';
end;

destructor TProtoUInt32Value.Destroy;
begin
  inherited Destroy;
end;

function TProtoUInt32Value.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwVarint then
        FValue := AReader.ReadUInt32
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoUInt32Value.EncodeFields(AWriter: TRALProtoWriter);
begin
  if FValue <> 0 then
  begin
    AWriter.WriteTag(1, rpwVarint);
    AWriter.WriteUInt32(FValue);
  end;
end;

function TProtoUInt32Value.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if AName = 'value' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FValue := RALProtoReadUInt32(AValue);
  end
  else
    Result := False;
end;

procedure TProtoUInt32Value.ResetFields;
begin
  FValue := 0;
end;

procedure TProtoUInt32Value.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (FValue <> 0) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'value', 'value'), RALProtoJSONUInt32(FValue));
end;

{ TProtoBoolValueList }

constructor TProtoBoolValueList.Create;
begin
  inherited Create(TProtoBoolValue);
end;

function TProtoBoolValueList.Add: TProtoBoolValue;
begin
  Result := TProtoBoolValue(AddNew);
end;

function TProtoBoolValueList.GetItem(AIndex: IntegerRAL): TProtoBoolValue;
begin
  Result := TProtoBoolValue(Messages[AIndex]);
end;

{ TProtoBoolValue }

constructor TProtoBoolValue.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoBoolValue.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.BoolValue';
end;

destructor TProtoBoolValue.Destroy;
begin
  inherited Destroy;
end;

function TProtoBoolValue.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwVarint then
        FValue := AReader.ReadBool
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoBoolValue.EncodeFields(AWriter: TRALProtoWriter);
begin
  if FValue then
  begin
    AWriter.WriteTag(1, rpwVarint);
    AWriter.WriteBool(FValue);
  end;
end;

function TProtoBoolValue.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if AName = 'value' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FValue := RALProtoReadBool(AValue);
  end
  else
    Result := False;
end;

procedure TProtoBoolValue.ResetFields;
begin
  FValue := False;
end;

procedure TProtoBoolValue.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (FValue) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'value', 'value'), RALProtoJSONBool(FValue));
end;

{ TProtoStringValueList }

constructor TProtoStringValueList.Create;
begin
  inherited Create(TProtoStringValue);
end;

function TProtoStringValueList.Add: TProtoStringValue;
begin
  Result := TProtoStringValue(AddNew);
end;

function TProtoStringValueList.GetItem(AIndex: IntegerRAL): TProtoStringValue;
begin
  Result := TProtoStringValue(Messages[AIndex]);
end;

{ TProtoStringValue }

constructor TProtoStringValue.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoStringValue.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.StringValue';
end;

destructor TProtoStringValue.Destroy;
begin
  inherited Destroy;
end;

function TProtoStringValue.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwLengthDelimited then
        FValue := AReader.ReadString
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoStringValue.EncodeFields(AWriter: TRALProtoWriter);
begin
  if FValue <> '' then
  begin
    AWriter.WriteTag(1, rpwLengthDelimited);
    AWriter.WriteString(FValue);
  end;
end;

function TProtoStringValue.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if AName = 'value' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FValue := RALProtoReadString(AValue);
  end
  else
    Result := False;
end;

procedure TProtoStringValue.ResetFields;
begin
  FValue := '';
end;

procedure TProtoStringValue.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (FValue <> '') or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'value', 'value'), RALProtoJSONString(FValue));
end;

{ TProtoBytesValueList }

constructor TProtoBytesValueList.Create;
begin
  inherited Create(TProtoBytesValue);
end;

function TProtoBytesValueList.Add: TProtoBytesValue;
begin
  Result := TProtoBytesValue(AddNew);
end;

function TProtoBytesValueList.GetItem(AIndex: IntegerRAL): TProtoBytesValue;
begin
  Result := TProtoBytesValue(Messages[AIndex]);
end;

{ TProtoBytesValue }

constructor TProtoBytesValue.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoBytesValue.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.BytesValue';
end;

destructor TProtoBytesValue.Destroy;
begin
  inherited Destroy;
end;

function TProtoBytesValue.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwLengthDelimited then
        FValue := AReader.ReadBytes
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoBytesValue.EncodeFields(AWriter: TRALProtoWriter);
begin
  if Length(FValue) > 0 then
  begin
    AWriter.WriteTag(1, rpwLengthDelimited);
    AWriter.WriteBytes(FValue);
  end;
end;

function TProtoBytesValue.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if AName = 'value' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FValue := RALProtoReadBytes(AValue);
  end
  else
    Result := False;
end;

procedure TProtoBytesValue.ResetFields;
begin
  FValue := nil;
end;

procedure TProtoBytesValue.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (Length(FValue) > 0) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'value', 'value'), RALProtoJSONBytes(FValue));
end;

{ TProtoStructList }

constructor TProtoStructList.Create;
begin
  inherited Create(TProtoStruct);
end;

function TProtoStructList.Add: TProtoStruct;
begin
  Result := TProtoStruct(AddNew);
end;

function TProtoStructList.GetItem(AIndex: IntegerRAL): TProtoStruct;
begin
  Result := TProtoStruct(Messages[AIndex]);
end;

{ TProtoStruct }

constructor TProtoStruct.Create;
begin
  inherited Create;
  FFields := TProtoStruct_FieldsEntryList.Create;
  ResetFields;
end;

class function TProtoStruct.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.Struct';
end;

destructor TProtoStruct.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

function TProtoStruct.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwLengthDelimited then
      begin
        FFields.ReadItem(AReader);
        FFields.DropDuplicateKey;
      end
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoStruct.EncodeFields(AWriter: TRALProtoWriter);
begin
  FFields.EncodeTo(AWriter, 1);
end;

function TProtoStruct.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
var
  vInt: IntegerRAL;
  vObject: TRALJSONObject;
begin
  Result := True;
  if AName = 'fields' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    vObject := RALProtoObjectOf(AValue);
    for vInt := 0 to vObject.Count - 1 do
    begin
      with FFields.Put(vObject.Names[vInt]) do
      begin
        Value := TProtoValue.Create;
        Value.ReadJSONValue(vObject.Items[vInt], AOptions);
      end;
    end;
  end
  else
    Result := False;
end;

procedure TProtoStruct.ResetFields;
begin
  FFields.Clear;
end;

procedure TProtoStruct.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
var
  vInt: IntegerRAL;
  vMap: TRALJSONObject;
begin
  if (FFields.Count > 0) or (rpjEmitDefaults in AOptions) then
  begin
    vMap := TRALJSONObject.Create;
    AObject.Add(RALProtoKey(AOptions, 'fields', 'fields'), vMap);
    for vInt := 0 to FFields.Count - 1 do
      if FFields[vInt].Value <> nil then
        vMap.Add(FFields[vInt].Key, FFields[vInt].Value.ToJSONValue(AOptions))
      else
        vMap.Add(FFields[vInt].Key, TRALJSONObject.Create);
  end;
end;

{ TProtoStruct_FieldsEntryList }

constructor TProtoStruct_FieldsEntryList.Create;
begin
  inherited Create(TProtoStruct_FieldsEntry);
end;

function TProtoStruct_FieldsEntryList.Add: TProtoStruct_FieldsEntry;
begin
  Result := TProtoStruct_FieldsEntry(AddNew);
end;

function TProtoStruct_FieldsEntryList.GetItem(AIndex: IntegerRAL): TProtoStruct_FieldsEntry;
begin
  Result := TProtoStruct_FieldsEntry(Messages[AIndex]);
end;

procedure TProtoStruct_FieldsEntryList.DropDuplicateKey;
var
  vInt: IntegerRAL;
begin
  for vInt := Count - 2 downto 0 do
    if Items[vInt].Key = Items[Count - 1].Key then
      Delete(vInt);
end;

function TProtoStruct_FieldsEntryList.Find(const AKey: StringRAL): TProtoStruct_FieldsEntry;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
    if Items[vInt].Key = AKey then
    begin
      Result := Items[vInt];
      Exit;
    end;
  Result := nil;
end;

function TProtoStruct_FieldsEntryList.Put(const AKey: StringRAL): TProtoStruct_FieldsEntry;
begin
  Result := Find(AKey);
  if Result = nil then
  begin
    Result := Add;
    Result.Key := AKey;
  end;
end;

{ TProtoStruct_FieldsEntry }

constructor TProtoStruct_FieldsEntry.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoStruct_FieldsEntry.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.Struct.FieldsEntry';
end;

destructor TProtoStruct_FieldsEntry.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

function TProtoStruct_FieldsEntry.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwLengthDelimited then
        FKey := AReader.ReadString
      else
        Result := False;
    2:
      if AWire = rpwLengthDelimited then
      begin
        if FValue = nil then
          FValue := TProtoValue.Create;
        AReader.ReadMessage(FValue);
      end
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoStruct_FieldsEntry.EncodeFields(AWriter: TRALProtoWriter);
begin
  begin
    AWriter.WriteTag(1, rpwLengthDelimited);
    AWriter.WriteString(FKey);
  end;
  if FValue <> nil then
  begin
    AWriter.WriteTag(2, rpwLengthDelimited);
    AWriter.WriteMessage(FValue);
  end;
end;

function TProtoStruct_FieldsEntry.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if AName = 'key' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FKey := RALProtoReadString(AValue);
  end
  else if AName = 'value' then
  begin
    if FValue = nil then
      SetValue(TProtoValue.Create);
    FValue.ReadJSONValue(AValue, AOptions);
  end
  else
    Result := False;
end;

procedure TProtoStruct_FieldsEntry.ResetFields;
begin
  FKey := '';
  FreeAndNil(FValue);
end;

procedure TProtoStruct_FieldsEntry.SetValue(AValue: TProtoValue);
begin
  if AValue = FValue then
    Exit;
  FValue.Free;
  FValue := AValue;
end;

procedure TProtoStruct_FieldsEntry.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (FKey <> '') or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'key', 'key'), RALProtoJSONString(FKey));
  if FValue <> nil then
    AObject.Add(RALProtoKey(AOptions, 'value', 'value'), FValue.ToJSONValue(AOptions));
end;

{ TProtoValueList }

constructor TProtoValueList.Create;
begin
  inherited Create(TProtoValue);
end;

function TProtoValueList.Add: TProtoValue;
begin
  Result := TProtoValue(AddNew);
end;

function TProtoValueList.GetItem(AIndex: IntegerRAL): TProtoValue;
begin
  Result := TProtoValue(Messages[AIndex]);
end;

{ TProtoValue }

constructor TProtoValue.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoValue.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.Value';
end;

destructor TProtoValue.Destroy;
begin
  FStructValue.Free;
  FListValue.Free;
  inherited Destroy;
end;

procedure TProtoValue.ClearKind;
begin
  FNullValue := 0;
  FNumberValue := 0;
  FStringValue := '';
  FBoolValue := False;
  FreeAndNil(FStructValue);
  FreeAndNil(FListValue);
  FKindCase := ProtoValue_Kind_NotSet;
end;

function TProtoValue.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwVarint then
        SetNullValue(AReader.ReadEnum)
      else
        Result := False;
    2:
      if AWire = rpwFixed64 then
        SetNumberValue(AReader.ReadDouble)
      else
        Result := False;
    3:
      if AWire = rpwLengthDelimited then
        SetStringValue(AReader.ReadString)
      else
        Result := False;
    4:
      if AWire = rpwVarint then
        SetBoolValue(AReader.ReadBool)
      else
        Result := False;
    5:
      if AWire = rpwLengthDelimited then
      begin
        if FStructValue = nil then
          SetStructValue(TProtoStruct.Create);
        AReader.ReadMessage(FStructValue);
      end
      else
        Result := False;
    6:
      if AWire = rpwLengthDelimited then
      begin
        if FListValue = nil then
          SetListValue(TProtoListValue.Create);
        AReader.ReadMessage(FListValue);
      end
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoValue.EncodeFields(AWriter: TRALProtoWriter);
begin
  if FKindCase = ProtoValue_Kind_NullValue then
  begin
    AWriter.WriteTag(1, rpwVarint);
    AWriter.WriteEnum(FNullValue);
  end;
  if FKindCase = ProtoValue_Kind_NumberValue then
  begin
    AWriter.WriteTag(2, rpwFixed64);
    AWriter.WriteDouble(FNumberValue);
  end;
  if FKindCase = ProtoValue_Kind_StringValue then
  begin
    AWriter.WriteTag(3, rpwLengthDelimited);
    AWriter.WriteString(FStringValue);
  end;
  if FKindCase = ProtoValue_Kind_BoolValue then
  begin
    AWriter.WriteTag(4, rpwVarint);
    AWriter.WriteBool(FBoolValue);
  end;
  if FKindCase = ProtoValue_Kind_StructValue then
  begin
    AWriter.WriteTag(5, rpwLengthDelimited);
    AWriter.WriteMessage(FStructValue);
  end;
  if FKindCase = ProtoValue_Kind_ListValue then
  begin
    AWriter.WriteTag(6, rpwLengthDelimited);
    AWriter.WriteMessage(FListValue);
  end;
end;

function TProtoValue.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
var
  vEnum: Integer;
begin
  Result := True;
  if (AName = 'nullValue') or (AName = 'null_value') then
  begin
    if ProtoNullValueFromJSON(AValue, AOptions, vEnum) then
      SetNullValue(vEnum);
  end
  else if (AName = 'numberValue') or (AName = 'number_value') then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    SetNumberValue(RALProtoReadDouble(AValue));
  end
  else if (AName = 'stringValue') or (AName = 'string_value') then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    SetStringValue(RALProtoReadString(AValue));
  end
  else if (AName = 'boolValue') or (AName = 'bool_value') then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    SetBoolValue(RALProtoReadBool(AValue));
  end
  else if (AName = 'structValue') or (AName = 'struct_value') then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    if FStructValue = nil then
      SetStructValue(TProtoStruct.Create);
    FStructValue.ReadJSONValue(AValue, AOptions);
  end
  else if (AName = 'listValue') or (AName = 'list_value') then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    if FListValue = nil then
      SetListValue(TProtoListValue.Create);
    FListValue.ReadJSONValue(AValue, AOptions);
  end
  else
    Result := False;
end;

procedure TProtoValue.ResetFields;
begin
  FNullValue := 0;
  FNumberValue := 0;
  FStringValue := '';
  FBoolValue := False;
  FreeAndNil(FStructValue);
  FreeAndNil(FListValue);
  FKindCase := ProtoValue_Kind_NotSet;
end;

procedure TProtoValue.SetNullValue(AValue: TProtoNullValue);
begin
  ClearKind;
  FNullValue := AValue;
  FKindCase := ProtoValue_Kind_NullValue;
end;

procedure TProtoValue.SetNumberValue(AValue: Double);
begin
  ClearKind;
  FNumberValue := AValue;
  FKindCase := ProtoValue_Kind_NumberValue;
end;

procedure TProtoValue.SetStringValue(const AValue: StringRAL);
begin
  ClearKind;
  FStringValue := AValue;
  FKindCase := ProtoValue_Kind_StringValue;
end;

procedure TProtoValue.SetBoolValue(AValue: Boolean);
begin
  ClearKind;
  FBoolValue := AValue;
  FKindCase := ProtoValue_Kind_BoolValue;
end;

procedure TProtoValue.SetStructValue(AValue: TProtoStruct);
begin
  if AValue = FStructValue then
    Exit;
  ClearKind;
  FStructValue := AValue;
  if AValue <> nil then
    FKindCase := ProtoValue_Kind_StructValue;
end;

procedure TProtoValue.SetListValue(AValue: TProtoListValue);
begin
  if AValue = FListValue then
    Exit;
  ClearKind;
  FListValue := AValue;
  if AValue <> nil then
    FKindCase := ProtoValue_Kind_ListValue;
end;

procedure TProtoValue.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if FKindCase = ProtoValue_Kind_NullValue then
    AObject.Add(RALProtoKey(AOptions, 'nullValue', 'null_value'), TRALJSONNull.Create);
  if FKindCase = ProtoValue_Kind_NumberValue then
    AObject.Add(RALProtoKey(AOptions, 'numberValue', 'number_value'), RALProtoJSONDouble(FNumberValue));
  if FKindCase = ProtoValue_Kind_StringValue then
    AObject.Add(RALProtoKey(AOptions, 'stringValue', 'string_value'), RALProtoJSONString(FStringValue));
  if FKindCase = ProtoValue_Kind_BoolValue then
    AObject.Add(RALProtoKey(AOptions, 'boolValue', 'bool_value'), RALProtoJSONBool(FBoolValue));
  if FKindCase = ProtoValue_Kind_StructValue then
    AObject.Add(RALProtoKey(AOptions, 'structValue', 'struct_value'), FStructValue.ToJSONValue(AOptions));
  if FKindCase = ProtoValue_Kind_ListValue then
    AObject.Add(RALProtoKey(AOptions, 'listValue', 'list_value'), FListValue.ToJSONValue(AOptions));
end;

{ TProtoListValueList }

constructor TProtoListValueList.Create;
begin
  inherited Create(TProtoListValue);
end;

function TProtoListValueList.Add: TProtoListValue;
begin
  Result := TProtoListValue(AddNew);
end;

function TProtoListValueList.GetItem(AIndex: IntegerRAL): TProtoListValue;
begin
  Result := TProtoListValue(Messages[AIndex]);
end;

{ TProtoListValue }

constructor TProtoListValue.Create;
begin
  inherited Create;
  FValues := TProtoValueList.Create;
  ResetFields;
end;

class function TProtoListValue.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.ListValue';
end;

destructor TProtoListValue.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

function TProtoListValue.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwLengthDelimited then
        FValues.ReadItem(AReader)
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoListValue.EncodeFields(AWriter: TRALProtoWriter);
begin
  FValues.EncodeTo(AWriter, 1);
end;

function TProtoListValue.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if AName = 'values' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FValues.ReadJSON(AValue, AOptions);
  end
  else
    Result := False;
end;

procedure TProtoListValue.ResetFields;
begin
  FValues.Clear;
end;

procedure TProtoListValue.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (FValues.Count > 0) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'values', 'values'), FValues.ToJSONValue(AOptions));
end;

{ TProtoAnyList }

constructor TProtoAnyList.Create;
begin
  inherited Create(TProtoAny);
end;

function TProtoAnyList.Add: TProtoAny;
begin
  Result := TProtoAny(AddNew);
end;

function TProtoAnyList.GetItem(AIndex: IntegerRAL): TProtoAny;
begin
  Result := TProtoAny(Messages[AIndex]);
end;

{ TProtoAny }

constructor TProtoAny.Create;
begin
  inherited Create;
  ResetFields;
end;

class function TProtoAny.ProtoName: StringRAL;
begin
  Result := 'google.protobuf.Any';
end;

destructor TProtoAny.Destroy;
begin
  inherited Destroy;
end;

function TProtoAny.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;
  AWire: TRALProtoWireType): Boolean;
begin
  Result := True;
  case AField of
    1:
      if AWire = rpwLengthDelimited then
        FTypeUrl := AReader.ReadString
      else
        Result := False;
    2:
      if AWire = rpwLengthDelimited then
        FValue := AReader.ReadBytes
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TProtoAny.EncodeFields(AWriter: TRALProtoWriter);
begin
  if FTypeUrl <> '' then
  begin
    AWriter.WriteTag(1, rpwLengthDelimited);
    AWriter.WriteString(FTypeUrl);
  end;
  if Length(FValue) > 0 then
  begin
    AWriter.WriteTag(2, rpwLengthDelimited);
    AWriter.WriteBytes(FValue);
  end;
end;

function TProtoAny.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;
  AOptions: TRALProtoJSONOptions): Boolean;
begin
  Result := True;
  if (AName = 'typeUrl') or (AName = 'type_url') then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FTypeUrl := RALProtoReadString(AValue);
  end
  else if AName = 'value' then
  begin
    if AValue.Kind = rjkNull then
      Exit;
    FValue := RALProtoReadBytes(AValue);
  end
  else
    Result := False;
end;

procedure TProtoAny.ResetFields;
begin
  FTypeUrl := '';
  FValue := nil;
end;

procedure TProtoAny.WriteJSONFields(AObject: TRALJSONObject;
  AOptions: TRALProtoJSONOptions);
begin
  if (FTypeUrl <> '') or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'typeUrl', 'type_url'), RALProtoJSONString(FTypeUrl));
  if (Length(FValue) > 0) or (rpjEmitDefaults in AOptions) then
    AObject.Add(RALProtoKey(AOptions, 'value', 'value'), RALProtoJSONBytes(FValue));
end;

{ JSON forms of the well-known types }

function TProtoTimestamp.AsDateTime: TDateTime;
begin
  Result := cUnixEpochDays + FSeconds / 86400 + (FNanos div 1000000) / 86400000;
end;

procedure TProtoTimestamp.FromDateTime(AValue: TDateTime);
var
  vMillis: Int64;
begin
  vMillis := Round((AValue - cUnixEpochDays) * 86400000);
  if vMillis >= 0 then
    FSeconds := vMillis div 1000
  else
    FSeconds := -((-vMillis + 999) div 1000);
  FNanos := Integer(vMillis - FSeconds * 1000) * 1000000;
end;

procedure TProtoTimestamp.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
var
  vText: string;
  vPos: IntegerRAL;
  vYear, vMonth, vDay, vHour, vMinute, vSecond, vOffset: Int64;
  vNanos: Integer;
begin
  vText := string(RALProtoReadString(AValue));
  vPos := 1;
  vYear := Digits(vText, vPos, 4);
  if Copy(vText, vPos, 1) <> '-' then WKTError('malformed time: ' + vText);
  Inc(vPos);
  vMonth := Digits(vText, vPos, 2);
  if Copy(vText, vPos, 1) <> '-' then WKTError('malformed time: ' + vText);
  Inc(vPos);
  vDay := Digits(vText, vPos, 2);
  if Copy(vText, vPos, 1) <> 'T' then WKTError('malformed time: ' + vText);
  Inc(vPos);
  vHour := Digits(vText, vPos, 2);
  if Copy(vText, vPos, 1) <> ':' then WKTError('malformed time: ' + vText);
  Inc(vPos);
  vMinute := Digits(vText, vPos, 2);
  if Copy(vText, vPos, 1) <> ':' then WKTError('malformed time: ' + vText);
  Inc(vPos);
  vSecond := Digits(vText, vPos, 2);
  vNanos := Fraction(vText, vPos);
  vOffset := 0;
  if Copy(vText, vPos, 1) = 'Z' then
    Inc(vPos)
  else if (Copy(vText, vPos, 1) = '+') or (Copy(vText, vPos, 1) = '-') then
  begin
    Inc(vPos);
    vOffset := Digits(vText, vPos, 2) * 3600;
    if Copy(vText, vPos, 1) <> ':' then WKTError('malformed time: ' + vText);
    Inc(vPos);
    vOffset := vOffset + Digits(vText, vPos, 2) * 60;
    if vText[vPos - 6] = '+' then
      vOffset := -vOffset;
  end
  else
    WKTError('time without its zone: ' + vText);
  if vPos <= Length(vText) then
    WKTError('malformed time: ' + vText);
  if (vMonth < 1) or (vMonth > 12) or (vDay < 1) or (vDay > 31) or (vHour > 23) or
     (vMinute > 59) or (vSecond > 59) then
    WKTError('time out of range: ' + vText);
  FSeconds := DaysFromCivil(vYear, vMonth, vDay) * 86400 + vHour * 3600 + vMinute * 60 +
    vSecond + vOffset;
  FNanos := vNanos;
  if (FSeconds < cMinSeconds) or (FSeconds > cMaxSeconds) then
    WKTError('time out of range: ' + vText);
end;

function TProtoTimestamp.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
var
  vDays, vRest, vYear, vMonth, vDay: Int64;
begin
  if (FSeconds < cMinSeconds) or (FSeconds > cMaxSeconds) or (FNanos < 0) or
     (FNanos > 999999999) then
    WKTError('Timestamp out of range');
  if FSeconds >= 0 then
    vDays := FSeconds div 86400
  else
    vDays := -((-FSeconds + 86399) div 86400);
  vRest := FSeconds - vDays * 86400;
  CivilFromDays(vDays, vYear, vMonth, vDay);
  Result := TRALJSONString.Create(StringRAL(Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d%sZ',
    [vYear, vMonth, vDay, vRest div 3600, (vRest mod 3600) div 60, vRest mod 60,
     NanosText(FNanos)])));
end;

function TProtoDuration.AsMilliseconds: Int64;
begin
  Result := FSeconds * 1000 + FNanos div 1000000;
end;

procedure TProtoDuration.FromMilliseconds(AValue: Int64);
begin
  FSeconds := AValue div 1000;
  FNanos := Integer(AValue mod 1000) * 1000000;
end;

procedure TProtoDuration.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
var
  vText: string;
  vPos: IntegerRAL;
  vNegative: Boolean;
  vDigits: IntegerRAL;
begin
  vText := string(RALProtoReadString(AValue));
  if (Length(vText) < 2) or (vText[Length(vText)] <> 's') then
    WKTError('malformed duration: ' + vText);
  vPos := 1;
  vNegative := vText[1] = '-';
  if vNegative then
    Inc(vPos);
  FSeconds := 0;
  vDigits := 0;
  while (vPos <= Length(vText)) and CharInSet(vText[vPos], ['0'..'9']) do
  begin
    if vDigits > 12 then
      WKTError('duration out of range: ' + vText);
    FSeconds := FSeconds * 10 + Ord(vText[vPos]) - Ord('0');
    Inc(vPos);
    Inc(vDigits);
  end;
  if vDigits = 0 then
    WKTError('malformed duration: ' + vText);
  FNanos := Fraction(vText, vPos);
  if vPos <> Length(vText) then
    WKTError('malformed duration: ' + vText);
  if FSeconds > cMaxDuration then
    WKTError('duration out of range: ' + vText);
  if vNegative then
  begin
    FSeconds := -FSeconds;
    FNanos := -FNanos;
  end;
end;

function TProtoDuration.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
var
  vText: string;
begin
  if (FSeconds > cMaxDuration) or (FSeconds < -cMaxDuration) or (Abs(FNanos) > 999999999) or
     ((FSeconds > 0) and (FNanos < 0)) or ((FSeconds < 0) and (FNanos > 0)) then
    WKTError('Duration out of range');
  vText := IntToStr(Abs(FSeconds)) + NanosText(Abs(FNanos)) + 's';
  if (FSeconds < 0) or (FNanos < 0) then
    vText := '-' + vText;
  Result := TRALJSONString.Create(StringRAL(vText));
end;

procedure TProtoFieldMask.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
var
  vList: TStringList;
  vInt: IntegerRAL;
begin
  FPaths := nil;
  vList := TStringList.Create;
  try
    vList.StrictDelimiter := True;
    vList.Delimiter := ',';
    vList.DelimitedText := string(RALProtoReadString(AValue));
    for vInt := 0 to vList.Count - 1 do
      if vList[vInt] <> '' then
        RALProtoAddString(FPaths, StringRAL(SnakePath(vList[vInt])));
  finally
    vList.Free;
  end;
end;

function TProtoFieldMask.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
var
  vText: string;
  vInt: IntegerRAL;
begin
  vText := '';
  for vInt := 0 to High(FPaths) do
  begin
    if vInt > 0 then
      vText := vText + ',';
    vText := vText + CamelPath(string(FPaths[vInt]));
  end;
  Result := TRALJSONString.Create(StringRAL(vText));
end;

procedure TProtoDoubleValue.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
begin
  FValue := RALProtoReadDouble(AValue);
end;

function TProtoDoubleValue.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
begin
  Result := RALProtoJSONDouble(FValue);
end;

procedure TProtoFloatValue.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
begin
  FValue := RALProtoReadSingle(AValue);
end;

function TProtoFloatValue.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
begin
  Result := RALProtoJSONSingle(FValue);
end;

procedure TProtoInt64Value.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
begin
  FValue := RALProtoReadInt64(AValue);
end;

function TProtoInt64Value.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
begin
  Result := RALProtoJSONInt64(FValue);
end;

procedure TProtoUInt64Value.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
begin
  FValue := RALProtoReadUInt64(AValue);
end;

function TProtoUInt64Value.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
begin
  Result := RALProtoJSONUInt64(FValue);
end;

procedure TProtoInt32Value.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
begin
  FValue := RALProtoReadInt32(AValue);
end;

function TProtoInt32Value.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
begin
  Result := RALProtoJSONInt32(FValue);
end;

procedure TProtoUInt32Value.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
begin
  FValue := RALProtoReadUInt32(AValue);
end;

function TProtoUInt32Value.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
begin
  Result := RALProtoJSONUInt32(FValue);
end;

procedure TProtoBoolValue.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
begin
  FValue := RALProtoReadBool(AValue);
end;

function TProtoBoolValue.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
begin
  Result := RALProtoJSONBool(FValue);
end;

procedure TProtoStringValue.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
begin
  FValue := RALProtoReadString(AValue);
end;

function TProtoStringValue.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
begin
  Result := RALProtoJSONString(FValue);
end;

procedure TProtoBytesValue.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
begin
  FValue := RALProtoReadBytes(AValue);
end;

function TProtoBytesValue.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
begin
  Result := RALProtoJSONBytes(FValue);
end;

procedure TProtoStruct.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
var
  vObject: TRALJSONObject;
  vInt: IntegerRAL;
  vEntry: TProtoStruct_FieldsEntry;
begin
  vObject := RALProtoObjectOf(AValue);
  for vInt := 0 to vObject.Count - 1 do
  begin
    vEntry := FFields.Put(vObject.Names[vInt]);
    vEntry.Value := TProtoValue.Create;
    vEntry.Value.ReadJSONValue(vObject.Items[vInt], AOptions);
  end;
end;

function TProtoStruct.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
var
  vInt: IntegerRAL;
begin
  Result := TRALJSONObject.Create;
  try
    for vInt := 0 to FFields.Count - 1 do
      if FFields[vInt].Value <> nil then
        TRALJSONObject(Result).Add(FFields[vInt].Key, FFields[vInt].Value.ToJSONValue(AOptions))
      else
        TRALJSONObject(Result).Add(FFields[vInt].Key);
  except
    Result.Free;
    raise;
  end;
end;

procedure TProtoValue.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
var
  vStruct: TProtoStruct;
  vList: TProtoListValue;
begin
  if (AValue = nil) or (AValue.Kind = rjkNull) then
    NullValue := Proto_NULL_VALUE
  else
    case AValue.Kind of
      rjkNumber: NumberValue := RALProtoReadDouble(AValue);
      rjkString: StringValue := TRALJSONString(AValue).Value;
      rjkBoolean: BoolValue := TRALJSONBoolean(AValue).Value;
      rjkObject:
        begin
          vStruct := TProtoStruct.Create;
          StructValue := vStruct;
          vStruct.ReadJSONValue(AValue, AOptions);
        end;
      rjkArray:
        begin
          vList := TProtoListValue.Create;
          ListValue := vList;
          vList.ReadJSONValue(AValue, AOptions);
        end;
    end;
end;

function TProtoValue.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
begin
  case FKindCase of
    ProtoValue_Kind_NumberValue:
      begin
        if IsNan(FNumberValue) or IsInfinite(FNumberValue) then
          WKTError('a Value cannot hold NaN or Infinity in JSON');
        Result := RALProtoJSONDouble(FNumberValue);
      end;
    ProtoValue_Kind_StringValue: Result := TRALJSONString.Create(FStringValue);
    ProtoValue_Kind_BoolValue: Result := TRALJSONBoolean.Create(FBoolValue);
    ProtoValue_Kind_StructValue: Result := FStructValue.ToJSONValue(AOptions);
    ProtoValue_Kind_ListValue: Result := FListValue.ToJSONValue(AOptions);
    ProtoValue_Kind_NullValue: Result := TRALJSONNull.Create;
  else
    begin
      WKTError('a Value with no kind set has no JSON form');
      Result := nil;
    end;
  end;
end;

procedure TProtoListValue.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
begin
  FValues.ReadJSON(RALProtoArrayOf(AValue), AOptions);
end;

function TProtoListValue.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
begin
  Result := FValues.ToJSONValue(AOptions);
end;

procedure TProtoAny.Pack(AMessage: TRALProtoMessage);
begin
  FTypeUrl := cTypeUrlPrefix + AMessage.ProtoName;
  FValue := AMessage.Encode;
end;

/// The full name of the type of a type URL (what follows the last slash).
function TypeNameOf(const AUrl: StringRAL): StringRAL;
var
  vPos: IntegerRAL;
begin
  vPos := Length(AUrl);
  while (vPos > 0) and (AUrl[vPos] <> '/') do
    Dec(vPos);
  Result := Copy(AUrl, vPos + 1, MaxInt);
end;

procedure TProtoAny.ReadJSONValue(AValue: TRALJSON; AOptions: TRALProtoJSONOptions);
var
  vObject: TRALJSONObject;
  vType: TRALJSON;
  vClass: TRALProtoMessageClass;
  vMessage: TRALProtoMessage;
  vInt: IntegerRAL;
begin
  vObject := RALProtoObjectOf(AValue);
  vType := vObject.Find('@type');
  if vType = nil then
  begin
    if vObject.Count = 0 then
      Exit;
    WKTError('Any without "@type"');
  end;
  FTypeUrl := RALProtoReadString(vType);
  vClass := RALProtoFindMessage(TypeNameOf(FTypeUrl));
  if vClass = nil then
    WKTError('Any of an unregistered type: ' + string(FTypeUrl));
  vMessage := vClass.Create;
  try
    if HasOwnJSON(vClass) then
      vMessage.ReadJSONValue(vObject.Find('value'), AOptions)
    else
      for vInt := 0 to vObject.Count - 1 do
        if (vObject.Names[vInt] <> '@type') and
           not TRALProtoMessageAccess(vMessage).ReadJSONField(vObject.Names[vInt],
             vObject.Items[vInt], AOptions) and not (rpjIgnoreUnknown in AOptions) then
          WKTError('unknown JSON field ' + string(vObject.Names[vInt]));
    FValue := vMessage.Encode;
  finally
    vMessage.Free;
  end;
end;

function TProtoAny.ToJSONValue(AOptions: TRALProtoJSONOptions): TRALJSON;
var
  vMessage: TRALProtoMessage;
  vInner: TRALJSON;
  vObject: TRALJSONObject;
  vName: StringRAL;
begin
  if FTypeUrl = '' then
  begin
    Result := TRALJSONObject.Create;
    Exit;
  end;
  vMessage := Unpack;
  if vMessage = nil then
    WKTError('Any of an unregistered type: ' + string(FTypeUrl));
  try
    vObject := TRALJSONObject.Create;
    try
      vObject.Add('@type', FTypeUrl);
      vInner := vMessage.ToJSONValue(AOptions);
      if HasOwnJSON(TRALProtoMessageClass(vMessage.ClassType)) then
        vObject.Add('value', vInner)
      else
        try
          // one statement per step: the order arguments are evaluated in is the
          // compiler's
          while TRALJSONObject(vInner).Count > 0 do
          begin
            vName := TRALJSONObject(vInner).Names[0];
            vObject.Add(vName, TRALJSONObject(vInner).Extract(vName));
          end;
        finally
          vInner.Free;
        end;
    except
      vObject.Free;
      raise;
    end;
  finally
    vMessage.Free;
  end;
  Result := vObject;
end;

function TProtoAny.Unpack: TRALProtoMessage;
var
  vClass: TRALProtoMessageClass;
begin
  Result := nil;
  vClass := RALProtoFindMessage(TypeNameOf(FTypeUrl));
  if vClass = nil then
    Exit;
  Result := vClass.Create;
  try
    Result.Decode(FValue);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TProtoAny.UnpackTo(AMessage: TRALProtoMessage): Boolean;
begin
  Result := TypeNameOf(FTypeUrl) = AMessage.ProtoName;
  if Result then
    AMessage.Decode(FValue);
end;

initialization
  RALProtoRegisterMessage(TProtoEmpty);
  RALProtoRegisterMessage(TProtoTimestamp);
  RALProtoRegisterMessage(TProtoDuration);
  RALProtoRegisterMessage(TProtoFieldMask);
  RALProtoRegisterMessage(TProtoDoubleValue);
  RALProtoRegisterMessage(TProtoFloatValue);
  RALProtoRegisterMessage(TProtoInt64Value);
  RALProtoRegisterMessage(TProtoUInt64Value);
  RALProtoRegisterMessage(TProtoInt32Value);
  RALProtoRegisterMessage(TProtoUInt32Value);
  RALProtoRegisterMessage(TProtoBoolValue);
  RALProtoRegisterMessage(TProtoStringValue);
  RALProtoRegisterMessage(TProtoBytesValue);
  RALProtoRegisterMessage(TProtoStruct);
  RALProtoRegisterMessage(TProtoStruct_FieldsEntry);
  RALProtoRegisterMessage(TProtoValue);
  RALProtoRegisterMessage(TProtoListValue);
  RALProtoRegisterMessage(TProtoAny);

finalization
  RALProtoUnregisterMessage(TProtoEmpty);
  RALProtoUnregisterMessage(TProtoTimestamp);
  RALProtoUnregisterMessage(TProtoDuration);
  RALProtoUnregisterMessage(TProtoFieldMask);
  RALProtoUnregisterMessage(TProtoDoubleValue);
  RALProtoUnregisterMessage(TProtoFloatValue);
  RALProtoUnregisterMessage(TProtoInt64Value);
  RALProtoUnregisterMessage(TProtoUInt64Value);
  RALProtoUnregisterMessage(TProtoInt32Value);
  RALProtoUnregisterMessage(TProtoUInt32Value);
  RALProtoUnregisterMessage(TProtoBoolValue);
  RALProtoUnregisterMessage(TProtoStringValue);
  RALProtoUnregisterMessage(TProtoBytesValue);
  RALProtoUnregisterMessage(TProtoStruct);
  RALProtoUnregisterMessage(TProtoStruct_FieldsEntry);
  RALProtoUnregisterMessage(TProtoValue);
  RALProtoUnregisterMessage(TProtoListValue);
  RALProtoUnregisterMessage(TProtoAny);

end.
