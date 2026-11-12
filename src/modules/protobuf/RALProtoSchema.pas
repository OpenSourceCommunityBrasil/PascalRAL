/// The .proto language: a parser for proto2, proto3 and edition 2023 files into a model
/// with every type resolved.
unit RALProtoSchema;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs,
  RALTypes, RALExceptions, RALProtobuf;

type
  TRALProtoEnumDef = class;
  TRALProtoFileDef = class;
  TRALProtoMessageDef = class;
  TRALProtoSchema = class;

  /// The label of a field as the model sees it.
  TRALProtoFieldLabel = (
    /// A singular field without presence: its default value is not written.
    rflImplicit,
    /// A singular field with presence (proto2 optional, proto3 optional, editions).
    rflOptional,
    /// A proto2 required field (read and written as optional).
    rflRequired,
    /// A repeated field.
    rflRepeated
  );

  /// The type of a field.
  TRALProtoFieldType = (rftDouble, rftFloat, rftInt64, rftUInt64, rftInt32,
    rftFixed64, rftFixed32, rftBool, rftString, rftBytes, rftUInt32, rftSFixed32,
    rftSFixed64, rftSInt32, rftSInt64, rftEnum, rftMessage);

  /// A field of a message.
  TRALProtoFieldDef = class
  private
    FComment: StringRAL;
    FDefaultValue: StringRAL;
    FEnumType: TRALProtoEnumDef;
    FFieldType: TRALProtoFieldType;
    FHasDefault: Boolean;
    FJSONName: StringRAL;
    FLabel: TRALProtoFieldLabel;
    FMapEntry: TRALProtoMessageDef;
    FMessageType: TRALProtoMessageDef;
    FName: StringRAL;
    FNumber: IntegerRAL;
    FOneofIndex: IntegerRAL;
    FPacked: Boolean;
    FTypeName: StringRAL;
  public
    /// Whether the field is a map (a repeated entry message with key and value).
    function IsMap: Boolean;

    /// The leading comment of the field in the .proto, without the slashes.
    property Comment: StringRAL read FComment write FComment;
    /// The [default = ...] text of a proto2 field, as written.
    property DefaultValue: StringRAL read FDefaultValue write FDefaultValue;
    /// The enum of an enum field.
    property EnumType: TRALProtoEnumDef read FEnumType write FEnumType;
    /// The label of the field.
    property FieldLabel: TRALProtoFieldLabel read FLabel write FLabel;
    /// The type of the field.
    property FieldType: TRALProtoFieldType read FFieldType write FFieldType;
    /// Whether [default = ...] was given.
    property HasDefault: Boolean read FHasDefault write FHasDefault;
    /// The JSON name: json_name, or the lowerCamelCase of the name.
    property JSONName: StringRAL read FJSONName write FJSONName;
    /// The entry message of a map field, nil otherwise.
    property MapEntry: TRALProtoMessageDef read FMapEntry write FMapEntry;
    /// The message of a message field.
    property MessageType: TRALProtoMessageDef read FMessageType write FMessageType;
    /// The name of the field in the .proto.
    property Name: StringRAL read FName write FName;
    /// The field number.
    property Number: IntegerRAL read FNumber write FNumber;
    /// The index of the oneof the field belongs to, -1 for none.
    property OneofIndex: IntegerRAL read FOneofIndex write FOneofIndex;
    /// Whether a repeated scalar is written packed.
    property PackedEncoding: Boolean read FPacked write FPacked;
    /// The type as written in the .proto (a scalar name or a message/enum name).
    property TypeName: StringRAL read FTypeName write FTypeName;
  end;

  /// A value of an enum.
  TRALProtoEnumValueDef = class
  private
    FComment: StringRAL;
    FName: StringRAL;
    FNumber: IntegerRAL;
  public
    /// The leading comment of the value.
    property Comment: StringRAL read FComment write FComment;
    /// The name of the value.
    property Name: StringRAL read FName write FName;
    /// The number of the value.
    property Number: IntegerRAL read FNumber write FNumber;
  end;

  /// An enum.
  TRALProtoEnumDef = class
  private
    FClosed: Boolean;
    FComment: StringRAL;
    FFile: TRALProtoFileDef;
    FFullName: StringRAL;
    FName: StringRAL;
    FParent: TRALProtoMessageDef;
    /// The values of the enum.
    FValues: TObjectList;

    function GetCount: IntegerRAL;
    function GetValue(AIndex: IntegerRAL): TRALProtoEnumValueDef;
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds a value.
    function AddValue(const AName: StringRAL; ANumber: IntegerRAL): TRALProtoEnumValueDef;
    /// The value named AName, nil when there is none.
    function FindValue(const AName: StringRAL): TRALProtoEnumValueDef;

    /// Whether unknown numbers are kept out of the field (proto2 enums).
    property Closed: Boolean read FClosed write FClosed;
    /// The leading comment of the enum.
    property Comment: StringRAL read FComment write FComment;
    /// The number of values.
    property Count: IntegerRAL read GetCount;
    /// The file that declares the enum.
    property FileDef: TRALProtoFileDef read FFile write FFile;
    /// The name with the package and the enclosing messages.
    property FullName: StringRAL read FFullName write FFullName;
    /// The name of the enum.
    property Name: StringRAL read FName write FName;
    /// The message the enum is nested in, nil at the top.
    property Parent: TRALProtoMessageDef read FParent write FParent;
    /// The values, in declaration order.
    property Values[AIndex: IntegerRAL]: TRALProtoEnumValueDef read GetValue;
  end;

  /// A message.
  TRALProtoMessageDef = class
  private
    FComment: StringRAL;
    /// The nested enums.
    FEnums: TObjectList;
    /// The fields of the message.
    FFields: TObjectList;
    FFile: TRALProtoFileDef;
    FFullName: StringRAL;
    FIsMapEntry: Boolean;
    /// The nested messages.
    FMessages: TObjectList;
    FName: StringRAL;
    FOneofs: TStringList;
    FParent: TRALProtoMessageDef;

    function GetEnum(AIndex: IntegerRAL): TRALProtoEnumDef;
    function GetEnumCount: IntegerRAL;
    function GetField(AIndex: IntegerRAL): TRALProtoFieldDef;
    function GetFieldCount: IntegerRAL;
    function GetMessage(AIndex: IntegerRAL): TRALProtoMessageDef;
    function GetMessageCount: IntegerRAL;
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds a field.
    function AddField: TRALProtoFieldDef;
    /// The field numbered ANumber, nil when there is none.
    function FindField(ANumber: IntegerRAL): TRALProtoFieldDef;

    /// The leading comment of the message.
    property Comment: StringRAL read FComment write FComment;
    /// The number of nested enums.
    property EnumCount: IntegerRAL read GetEnumCount;
    /// The nested enums.
    property Enums[AIndex: IntegerRAL]: TRALProtoEnumDef read GetEnum;
    /// The number of fields.
    property FieldCount: IntegerRAL read GetFieldCount;
    /// The fields, in declaration order.
    property Fields[AIndex: IntegerRAL]: TRALProtoFieldDef read GetField;
    /// The file that declares the message.
    property FileDef: TRALProtoFileDef read FFile write FFile;
    /// The name with the package and the enclosing messages.
    property FullName: StringRAL read FFullName write FFullName;
    /// Whether the message is the entry of a map field.
    property IsMapEntry: Boolean read FIsMapEntry write FIsMapEntry;
    /// The number of nested messages.
    property MessageCount: IntegerRAL read GetMessageCount;
    /// The nested messages.
    property Messages[AIndex: IntegerRAL]: TRALProtoMessageDef read GetMessage;
    /// The name of the message.
    property Name: StringRAL read FName write FName;
    /// The names of the oneofs (a proto3 optional field has none).
    property Oneofs: TStringList read FOneofs;
    /// The message this one is nested in, nil at the top.
    property Parent: TRALProtoMessageDef read FParent write FParent;
  end;

  /// A method of a service.
  TRALProtoMethodDef = class
  private
    FClientStreaming: Boolean;
    FComment: StringRAL;
    FInputName: StringRAL;
    FInputType: TRALProtoMessageDef;
    FName: StringRAL;
    FOutputName: StringRAL;
    FOutputType: TRALProtoMessageDef;
    FServerStreaming: Boolean;
  public
    /// Whether the client sends a stream of messages.
    property ClientStreaming: Boolean read FClientStreaming write FClientStreaming;
    /// The leading comment of the method.
    property Comment: StringRAL read FComment write FComment;
    /// The input type as written.
    property InputName: StringRAL read FInputName write FInputName;
    /// The input message.
    property InputType: TRALProtoMessageDef read FInputType write FInputType;
    /// The name of the method.
    property Name: StringRAL read FName write FName;
    /// The output type as written.
    property OutputName: StringRAL read FOutputName write FOutputName;
    /// The output message.
    property OutputType: TRALProtoMessageDef read FOutputType write FOutputType;
    /// Whether the server answers with a stream of messages.
    property ServerStreaming: Boolean read FServerStreaming write FServerStreaming;
  end;

  /// A service.
  TRALProtoServiceDef = class
  private
    FComment: StringRAL;
    FFile: TRALProtoFileDef;
    FFullName: StringRAL;
    /// The methods of the service.
    FMethods: TObjectList;
    FName: StringRAL;

    function GetCount: IntegerRAL;
    function GetMethod(AIndex: IntegerRAL): TRALProtoMethodDef;
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds a method.
    function AddMethod: TRALProtoMethodDef;

    /// The leading comment of the service.
    property Comment: StringRAL read FComment write FComment;
    /// The number of methods.
    property Count: IntegerRAL read GetCount;
    /// The file that declares the service.
    property FileDef: TRALProtoFileDef read FFile write FFile;
    /// The name with the package, as gRPC paths use it.
    property FullName: StringRAL read FFullName write FFullName;
    /// The methods.
    property Methods[AIndex: IntegerRAL]: TRALProtoMethodDef read GetMethod;
    /// The name of the service.
    property Name: StringRAL read FName write FName;
  end;

  /// The syntax a file declares.
  TRALProtoSyntax = (rsxProto2, rsxProto3, rsxEditions);

  /// A parsed .proto file.
  TRALProtoFileDef = class
  private
    /// The enums of the file.
    FEnums: TObjectList;
    FFileName: StringRAL;
    FImports: TStringList;
    /// The messages of the file.
    FMessages: TObjectList;
    FOptions: TStringList;
    FPackage: StringRAL;
    FPascalUnit: StringRAL;
    /// The services of the file.
    FServices: TObjectList;
    FSyntax: TRALProtoSyntax;

    function GetEnum(AIndex: IntegerRAL): TRALProtoEnumDef;
    function GetEnumCount: IntegerRAL;
    function GetMessage(AIndex: IntegerRAL): TRALProtoMessageDef;
    function GetMessageCount: IntegerRAL;
    function GetService(AIndex: IntegerRAL): TRALProtoServiceDef;
    function GetServiceCount: IntegerRAL;
  public
    constructor Create;
    destructor Destroy; override;

    /// The number of top-level enums.
    property EnumCount: IntegerRAL read GetEnumCount;
    /// The top-level enums.
    property Enums[AIndex: IntegerRAL]: TRALProtoEnumDef read GetEnum;
    /// The name the file was loaded under (as an import names it).
    property FileName: StringRAL read FFileName write FFileName;
    /// The files it imports, as written.
    property Imports: TStringList read FImports;
    /// The number of top-level messages.
    property MessageCount: IntegerRAL read GetMessageCount;
    /// The top-level messages.
    property Messages[AIndex: IntegerRAL]: TRALProtoMessageDef read GetMessage;
    /// The file options (name=value; strings without quotes).
    property Options: TStringList read FOptions;
    /// The package, empty when none.
    property Package: StringRAL read FPackage write FPackage;
    /// The Pascal unit generated from the file.
    property PascalUnit: StringRAL read FPascalUnit write FPascalUnit;
    /// The number of services.
    property ServiceCount: IntegerRAL read GetServiceCount;
    /// The services.
    property Services[AIndex: IntegerRAL]: TRALProtoServiceDef read GetService;
    /// The syntax.
    property Syntax: TRALProtoSyntax read FSyntax write FSyntax;
  end;

  /// Loads a file not found on the search paths; return '' when there is none.
  TRALProtoLoadEvent = procedure(const AFileName: StringRAL; var ASource: StringRAL;
    var AFound: Boolean) of object;

  { Every .proto file loaded, the files they import, and every type by its full
    name. Load parses a file and its imports; types are resolved when all are in. }
  TRALProtoSchema = class
  private
    /// The files loaded.
    FFiles: TObjectList;
    FOnLoad: TRALProtoLoadEvent;
    FSearchPaths: TStringList;
    /// Every type by full name.
    FTypes: TStringList;

    function GetFile(AIndex: IntegerRAL): TRALProtoFileDef;
    function GetFileCount: IntegerRAL;
    /// Adds the built-in google/protobuf files the generator knows.
    function LoadBuiltIn(const AFileName: StringRAL; out ASource: StringRAL): Boolean;
    /// Indexes the types of a file and of its messages.
    procedure RegisterTypes(AFile: TRALProtoFileDef);
    /// Resolves the type names of every field and method of a file.
    procedure ResolveFile(AFile: TRALProtoFileDef);
    /// Resolves ATypeName as seen from AScope (a full name).
    function ResolveType(const AScope, ATypeName: StringRAL): TObject;
  public
    constructor Create;
    destructor Destroy; override;

    /// The file loaded under AFileName, nil when none.
    function FindFile(const AFileName: StringRAL): TRALProtoFileDef;
    /// The message or enum of a full name (without the leading dot), nil when none.
    function FindType(const AFullName: StringRAL): TObject;
    /// Loads a file by name (searched in SearchPaths) with its imports.
    function Load(const AFileName: StringRAL): TRALProtoFileDef;
    /// Parses ASource as the file AFileName, then loads its imports.
    function LoadFromText(const AFileName, ASource: StringRAL): TRALProtoFileDef;

    /// The number of files.
    property FileCount: IntegerRAL read GetFileCount;
    /// The files, imports first.
    property Files[AIndex: IntegerRAL]: TRALProtoFileDef read GetFile;
    /// Asked for a file found neither on the paths nor built in.
    property OnLoad: TRALProtoLoadEvent read FOnLoad write FOnLoad;
    /// Folders where imports are looked for, after the importing file's own.
    property SearchPaths: TStringList read FSearchPaths;
  end;

/// The Pascal unit name for a .proto file name: its base name, letters and digits only.
function RALProtoUnitNameOf(const AFileName: StringRAL): StringRAL;

implementation

const
  /// Names of the scalar types, in TRALProtoFieldType order up to rftSInt64.
  cScalarNames: array[rftDouble..rftSInt64] of StringRAL = ('double', 'float',
    'int64', 'uint64', 'int32', 'fixed64', 'fixed32', 'bool', 'string', 'bytes',
    'uint32', 'sfixed32', 'sfixed64', 'sint32', 'sint64');

  /// google/protobuf/empty.proto.
  cEmptyProto = 'syntax = "proto3"; package google.protobuf; message Empty {}';
  /// google/protobuf/timestamp.proto.
  cTimestampProto = 'syntax = "proto3"; package google.protobuf; ' +
    'message Timestamp { int64 seconds = 1; int32 nanos = 2; }';
  /// google/protobuf/duration.proto.
  cDurationProto = 'syntax = "proto3"; package google.protobuf; ' +
    'message Duration { int64 seconds = 1; int32 nanos = 2; }';
  /// google/protobuf/field_mask.proto.
  cFieldMaskProto = 'syntax = "proto3"; package google.protobuf; ' +
    'message FieldMask { repeated string paths = 1; }';
  /// google/protobuf/wrappers.proto.
  cWrappersProto = 'syntax = "proto3"; package google.protobuf; ' +
    'message DoubleValue { double value = 1; } message FloatValue { float value = 1; } ' +
    'message Int64Value { int64 value = 1; } message UInt64Value { uint64 value = 1; } ' +
    'message Int32Value { int32 value = 1; } message UInt32Value { uint32 value = 1; } ' +
    'message BoolValue { bool value = 1; } message StringValue { string value = 1; } ' +
    'message BytesValue { bytes value = 1; }';
  /// google/protobuf/struct.proto.
  cStructProto = 'syntax = "proto3"; package google.protobuf; ' +
    'message Struct { map<string, Value> fields = 1; } ' +
    'message Value { oneof kind { NullValue null_value = 1; double number_value = 2; ' +
    'string string_value = 3; bool bool_value = 4; Struct struct_value = 5; ' +
    'ListValue list_value = 6; } } ' +
    'enum NullValue { NULL_VALUE = 0; } ' +
    'message ListValue { repeated Value values = 1; }';
  /// google/protobuf/any.proto.
  cAnyProto = 'syntax = "proto3"; package google.protobuf; ' +
    'message Any { string type_url = 1; bytes value = 2; }';

type
  /// The kind of a token.
  TRALProtoTokenKind = (rtkEOF, rtkIdent, rtkInt, rtkFloat, rtkString, rtkSymbol);

  /// Reads the tokens of one .proto file and builds its model.
  TRALProtoParser = class
  private
    /// Column of the current character.
    FColumn: IntegerRAL;
    /// The comment collected before the next token.
    FComment: StringRAL;
    /// The file definition being built.
    FFile: TRALProtoFileDef;
    /// Name of the file being parsed.
    FFileName: StringRAL;
    /// Kind of the current token.
    FKind: TRALProtoTokenKind;
    /// Line of the current character.
    FLine: IntegerRAL;
    /// Position of the next character in the source.
    FPosition: IntegerRAL;
    /// The source text.
    FSource: StringRAL;
    /// Text of the current token.
    FText: StringRAL;
    /// Column where the current token starts.
    FTokenColumn: IntegerRAL;
    /// Comment attached to the current token.
    FTokenComment: StringRAL;
    /// Line where the current token starts.
    FTokenLine: IntegerRAL;

    /// Raises a parse error at the current token.
    procedure Error(const AText: string);
    /// Requires the given symbol and moves on.
    procedure Expect(const ASymbol: StringRAL);
    /// Requires an identifier and returns it.
    function ExpectIdent: StringRAL;
    /// Requires an integer and returns it.
    function ExpectInt: Int64;
    /// Tells whether the current token is the given symbol.
    function IsSymbol(const ASymbol: StringRAL): Boolean;
    /// Reads the next token.
    procedure Next;
    /// Parses a constant value and returns its text.
    function ParseConstant: StringRAL;
    /// Parses an enum declaration.
    procedure ParseEnum(AParent: TRALProtoMessageDef);
    /// Parses a field declaration.
    procedure ParseField(AMessage: TRALProtoMessageDef; AOneof: IntegerRAL;
      const ALabel: StringRAL);
    /// Parses the bracketed options of a field.
    procedure ParseFieldOptions(AField: TRALProtoFieldDef);
    /// Parses a message declaration.
    procedure ParseMessage(AParent: TRALProtoMessageDef);
    /// Parses the members of a message.
    procedure ParseMessageBody(AMessage: TRALProtoMessageDef);
    /// Parses an option statement.
    procedure ParseOption(AOptions: TStringList);
    /// Parses a reserved statement.
    procedure ParseReserved;
    /// Parses a service declaration.
    procedure ParseService;
    /// Skips a braced block.
    procedure SkipBlock;
    /// Skips blanks and comments, collecting the comment text.
    procedure SkipSpaces;
    /// Returns the collected comment and clears it.
    function TakeComment: StringRAL;
  public
    /// Parses ASource into AFile.
    procedure Parse(AFile: TRALProtoFileDef; const ASource: StringRAL);
  end;

/// Raises a schema error.
procedure SchemaError(const AText: string);
begin
  raise ERALException.Create('ralprotoc: ' + AText);
end;

function RALProtoUnitNameOf(const AFileName: StringRAL): StringRAL;
var
  vName: string;
  vInt: IntegerRAL;
  vUpper: Boolean;
begin
  vName := ChangeFileExt(ExtractFileName(StringReplace(string(AFileName), '/',
    PathDelim, [rfReplaceAll])), '');
  Result := '';
  vUpper := True;
  for vInt := 1 to Length(vName) do
    if CharInSet(vName[vInt], ['A'..'Z', 'a'..'z', '0'..'9']) then
    begin
      if vUpper then
        Result := Result + StringRAL(UpCase(vName[vInt]))
      else
        Result := Result + StringRAL(vName[vInt]);
      vUpper := False;
    end
    else
      vUpper := True;
  if (Result = '') or CharInSet(Char(Result[1]), ['0'..'9']) then
    Result := 'Proto' + Result;
end;

{ TRALProtoFieldDef }

function TRALProtoFieldDef.IsMap: Boolean;
begin
  Result := FMapEntry <> nil;
end;

{ TRALProtoEnumDef }

constructor TRALProtoEnumDef.Create;
begin
  inherited Create;
  FValues := TObjectList.Create(True);
end;

destructor TRALProtoEnumDef.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

function TRALProtoEnumDef.AddValue(const AName: StringRAL;
  ANumber: IntegerRAL): TRALProtoEnumValueDef;
begin
  Result := TRALProtoEnumValueDef.Create;
  Result.Name := AName;
  Result.Number := ANumber;
  FValues.Add(Result);
end;

function TRALProtoEnumDef.FindValue(const AName: StringRAL): TRALProtoEnumValueDef;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FValues.Count - 1 do
  begin
    Result := TRALProtoEnumValueDef(FValues[vInt]);
    if Result.Name = AName then
      Exit;
  end;
  Result := nil;
end;

function TRALProtoEnumDef.GetCount: IntegerRAL;
begin
  Result := FValues.Count;
end;

function TRALProtoEnumDef.GetValue(AIndex: IntegerRAL): TRALProtoEnumValueDef;
begin
  Result := TRALProtoEnumValueDef(FValues[AIndex]);
end;

{ TRALProtoMessageDef }

constructor TRALProtoMessageDef.Create;
begin
  inherited Create;
  FFields := TObjectList.Create(True);
  FMessages := TObjectList.Create(True);
  FEnums := TObjectList.Create(True);
  FOneofs := TStringList.Create;
end;

destructor TRALProtoMessageDef.Destroy;
begin
  FFields.Free;
  FMessages.Free;
  FEnums.Free;
  FOneofs.Free;
  inherited Destroy;
end;

function TRALProtoMessageDef.AddField: TRALProtoFieldDef;
begin
  Result := TRALProtoFieldDef.Create;
  Result.OneofIndex := -1;
  FFields.Add(Result);
end;

function TRALProtoMessageDef.FindField(ANumber: IntegerRAL): TRALProtoFieldDef;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FFields.Count - 1 do
  begin
    Result := TRALProtoFieldDef(FFields[vInt]);
    if Result.Number = ANumber then
      Exit;
  end;
  Result := nil;
end;

function TRALProtoMessageDef.GetEnum(AIndex: IntegerRAL): TRALProtoEnumDef;
begin
  Result := TRALProtoEnumDef(FEnums[AIndex]);
end;

function TRALProtoMessageDef.GetEnumCount: IntegerRAL;
begin
  Result := FEnums.Count;
end;

function TRALProtoMessageDef.GetField(AIndex: IntegerRAL): TRALProtoFieldDef;
begin
  Result := TRALProtoFieldDef(FFields[AIndex]);
end;

function TRALProtoMessageDef.GetFieldCount: IntegerRAL;
begin
  Result := FFields.Count;
end;

function TRALProtoMessageDef.GetMessage(AIndex: IntegerRAL): TRALProtoMessageDef;
begin
  Result := TRALProtoMessageDef(FMessages[AIndex]);
end;

function TRALProtoMessageDef.GetMessageCount: IntegerRAL;
begin
  Result := FMessages.Count;
end;

{ TRALProtoServiceDef }

constructor TRALProtoServiceDef.Create;
begin
  inherited Create;
  FMethods := TObjectList.Create(True);
end;

destructor TRALProtoServiceDef.Destroy;
begin
  FMethods.Free;
  inherited Destroy;
end;

function TRALProtoServiceDef.AddMethod: TRALProtoMethodDef;
begin
  Result := TRALProtoMethodDef.Create;
  FMethods.Add(Result);
end;

function TRALProtoServiceDef.GetCount: IntegerRAL;
begin
  Result := FMethods.Count;
end;

function TRALProtoServiceDef.GetMethod(AIndex: IntegerRAL): TRALProtoMethodDef;
begin
  Result := TRALProtoMethodDef(FMethods[AIndex]);
end;

{ TRALProtoFileDef }

constructor TRALProtoFileDef.Create;
begin
  inherited Create;
  FMessages := TObjectList.Create(True);
  FEnums := TObjectList.Create(True);
  FServices := TObjectList.Create(True);
  FImports := TStringList.Create;
  FOptions := TStringList.Create;
end;

destructor TRALProtoFileDef.Destroy;
begin
  FMessages.Free;
  FEnums.Free;
  FServices.Free;
  FImports.Free;
  FOptions.Free;
  inherited Destroy;
end;

function TRALProtoFileDef.GetEnum(AIndex: IntegerRAL): TRALProtoEnumDef;
begin
  Result := TRALProtoEnumDef(FEnums[AIndex]);
end;

function TRALProtoFileDef.GetEnumCount: IntegerRAL;
begin
  Result := FEnums.Count;
end;

function TRALProtoFileDef.GetMessage(AIndex: IntegerRAL): TRALProtoMessageDef;
begin
  Result := TRALProtoMessageDef(FMessages[AIndex]);
end;

function TRALProtoFileDef.GetMessageCount: IntegerRAL;
begin
  Result := FMessages.Count;
end;

function TRALProtoFileDef.GetService(AIndex: IntegerRAL): TRALProtoServiceDef;
begin
  Result := TRALProtoServiceDef(FServices[AIndex]);
end;

function TRALProtoFileDef.GetServiceCount: IntegerRAL;
begin
  Result := FServices.Count;
end;

{ TRALProtoParser }

procedure TRALProtoParser.Error(const AText: string);
begin
  SchemaError(Format('%s:%d:%d: %s', [string(FFileName), FTokenLine, FTokenColumn, AText]));
end;

procedure TRALProtoParser.Expect(const ASymbol: StringRAL);
begin
  if not IsSymbol(ASymbol) then
    Error(Format('"%s" expected, found "%s"', [string(ASymbol), string(FText)]));
  Next;
end;

function TRALProtoParser.ExpectIdent: StringRAL;
begin
  if FKind <> rtkIdent then
    Error(Format('identifier expected, found "%s"', [string(FText)]));
  Result := FText;
  Next;
end;

function TRALProtoParser.ExpectInt: Int64;
var
  vNegative: Boolean;
begin
  vNegative := IsSymbol('-');
  if vNegative then
    Next;
  if FKind <> rtkInt then
    Error(Format('integer expected, found "%s"', [string(FText)]));
  if (Length(FText) > 1) and (FText[1] = '0') and CharInSet(Char(FText[2]), ['x', 'X']) then
    Result := StrToInt64('$' + string(Copy(FText, 3, MaxInt)))
  else if (Length(FText) > 1) and (FText[1] = '0') then
  begin
    // octal
    Result := 0;
    while Length(FText) > 1 do
    begin
      Result := Result * 8 + Ord(FText[2]) - Ord('0');
      Delete(FText, 2, 1);
    end;
  end
  else
    Result := StrToInt64(string(FText));
  if vNegative then
    Result := -Result;
  Next;
end;

function TRALProtoParser.IsSymbol(const ASymbol: StringRAL): Boolean;
begin
  Result := (FKind in [rtkSymbol, rtkIdent]) and (FText = ASymbol);
end;

procedure TRALProtoParser.Next;
var
  vChar: CharRAL;
  vQuote: CharRAL;
  vValue: IntegerRAL;
  vDigits: IntegerRAL;
begin
  SkipSpaces;
  FTokenLine := FLine;
  FTokenColumn := FColumn;
  FTokenComment := TakeComment;
  FText := '';
  if FPosition > Length(FSource) then
  begin
    FKind := rtkEOF;
    Exit;
  end;
  vChar := FSource[FPosition];
  if CharInSet(Char(vChar), ['A'..'Z', 'a'..'z', '_']) or
     ((vChar = '.') and (FPosition < Length(FSource)) and
      CharInSet(Char(FSource[FPosition + 1]), ['A'..'Z', 'a'..'z', '_'])) then
  begin
    // identifiers keep their dots: foo.bar.Baz, .pkg.Type
    FKind := rtkIdent;
    while (FPosition <= Length(FSource)) and
      CharInSet(Char(FSource[FPosition]), ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) do
    begin
      RALProtoAppendByte(FText, Ord(FSource[FPosition]));
      Inc(FPosition);
      Inc(FColumn);
    end;
  end
  else if CharInSet(Char(vChar), ['0'..'9']) or
     ((vChar = '.') and (FPosition < Length(FSource)) and
      CharInSet(Char(FSource[FPosition + 1]), ['0'..'9'])) then
  begin
    FKind := rtkInt;
    while (FPosition <= Length(FSource)) and
      (CharInSet(Char(FSource[FPosition]), ['0'..'9', 'a'..'z', 'A'..'Z', '.']) or
       (CharInSet(Char(FSource[FPosition]), ['+', '-']) and
        CharInSet(Char(FSource[FPosition - 1]), ['e', 'E']) and (FKind = rtkFloat))) do
    begin
      if CharInSet(Char(FSource[FPosition]), ['.', 'e', 'E']) and
         not ((Length(FText) > 1) and CharInSet(Char(FText[2]), ['x', 'X'])) then
        FKind := rtkFloat;
      RALProtoAppendByte(FText, Ord(FSource[FPosition]));
      Inc(FPosition);
      Inc(FColumn);
    end;
  end
  else if (vChar = '"') or (vChar = '''') then
  begin
    FKind := rtkString;
    vQuote := vChar;
    Inc(FPosition);
    Inc(FColumn);
    while True do
    begin
      if FPosition > Length(FSource) then
        Error('string without its closing quote');
      vChar := FSource[FPosition];
      Inc(FPosition);
      Inc(FColumn);
      if vChar = vQuote then
        Break;
      if (vChar = #10) then
        Error('line break inside a string');
      if vChar = '\' then
      begin
        vChar := FSource[FPosition];
        Inc(FPosition);
        Inc(FColumn);
        case vChar of
          'n': FText := FText + #10;
          'r': FText := FText + #13;
          't': FText := FText + #9;
          'a': FText := FText + #7;
          'b': FText := FText + #8;
          'f': FText := FText + #12;
          'v': FText := FText + #11;
          'x', 'X':
            begin
              vValue := 0;
              vDigits := 0;
              while (vDigits < 2) and (FPosition <= Length(FSource)) and
                CharInSet(Char(FSource[FPosition]), ['0'..'9', 'a'..'f', 'A'..'F']) do
              begin
                vValue := vValue * 16 + StrToInt('$' + string(FSource[FPosition]));
                Inc(FPosition);
                Inc(FColumn);
                Inc(vDigits);
              end;
              RALProtoAppendByte(FText, vValue);
            end;
          '0'..'7':
            begin
              vValue := Ord(vChar) - Ord('0');
              vDigits := 1;
              while (vDigits < 3) and (FPosition <= Length(FSource)) and
                CharInSet(Char(FSource[FPosition]), ['0'..'7']) do
              begin
                vValue := vValue * 8 + Ord(FSource[FPosition]) - Ord('0');
                Inc(FPosition);
                Inc(FColumn);
                Inc(vDigits);
              end;
              RALProtoAppendByte(FText, vValue);
            end;
        else
          RALProtoAppendByte(FText, Ord(vChar));
        end;
      end
      else
        RALProtoAppendByte(FText, Ord(vChar));
    end;
  end
  else
  begin
    FKind := rtkSymbol;
    FText := vChar;
    Inc(FPosition);
    Inc(FColumn);
  end;
end;

procedure TRALProtoParser.Parse(AFile: TRALProtoFileDef; const ASource: StringRAL);
var
  vSyntax, vImport: StringRAL;
begin
  FFile := AFile;
  FFileName := AFile.FileName;
  FSource := ASource;
  FPosition := 1;
  FLine := 1;
  FColumn := 1;
  // a BOM is not .proto
  if Copy(FSource, 1, 3) = #$EF#$BB#$BF then
    FPosition := 4;
  AFile.Syntax := rsxProto2;
  Next;
  if IsSymbol('syntax') or IsSymbol('edition') then
  begin
    if IsSymbol('edition') then
      AFile.Syntax := rsxEditions;
    Next;
    Expect('=');
    if FKind <> rtkString then
      Error('syntax string expected');
    vSyntax := FText;
    Next;
    Expect(';');
    if AFile.Syntax <> rsxEditions then
    begin
      if vSyntax = 'proto3' then
        AFile.Syntax := rsxProto3
      else if vSyntax <> 'proto2' then
        Error('unknown syntax ' + string(vSyntax));
    end
    else if vSyntax <> '2023' then
      Error('only edition 2023 is read');
  end;
  while FKind <> rtkEOF do
  begin
    if IsSymbol(';') then
      Next
    else if IsSymbol('package') then
    begin
      Next;
      AFile.Package := ExpectIdent;
      Expect(';');
    end
    else if IsSymbol('import') then
    begin
      Next;
      if IsSymbol('public') or IsSymbol('weak') then
        Next;
      if FKind <> rtkString then
        Error('import file name expected');
      vImport := FText;
      Next;
      Expect(';');
      AFile.Imports.Add(string(vImport));
    end
    else if IsSymbol('option') then
      ParseOption(AFile.Options)
    else if IsSymbol('message') then
      ParseMessage(nil)
    else if IsSymbol('enum') then
      ParseEnum(nil)
    else if IsSymbol('service') then
      ParseService
    else if IsSymbol('extend') then
    begin
      Next;
      ExpectIdent;
      SkipBlock;
    end
    else
      Error(Format('unexpected "%s"', [string(FText)]));
  end;
end;

function TRALProtoParser.ParseConstant: StringRAL;
begin
  Result := '';
  if IsSymbol('-') or IsSymbol('+') then
  begin
    if IsSymbol('-') then
      Result := '-';
    Next;
  end;
  if IsSymbol('{') then
  begin
    // an aggregate option (text format): not used by the generator
    SkipBlock;
    Exit;
  end;
  if not (FKind in [rtkIdent, rtkInt, rtkFloat, rtkString]) then
    Error('constant expected');
  Result := Result + FText;
  Next;
  // adjacent strings concatenate
  while FKind = rtkString do
  begin
    Result := Result + FText;
    Next;
  end;
end;

procedure TRALProtoParser.ParseEnum(AParent: TRALProtoMessageDef);
var
  vEnum: TRALProtoEnumDef;
  vValue: TRALProtoEnumValueDef;
  vOptions: TStringList;
  vComment: StringRAL;
begin
  vEnum := TRALProtoEnumDef.Create;
  vEnum.Comment := FTokenComment;
  if AParent = nil then
    FFile.FEnums.Add(vEnum)
  else
    AParent.FEnums.Add(vEnum);
  vEnum.FileDef := FFile;
  vEnum.Parent := AParent;
  vEnum.Closed := FFile.Syntax = rsxProto2;
  Next;
  vEnum.Name := ExpectIdent;
  Expect('{');
  vOptions := TStringList.Create;
  try
    while not IsSymbol('}') do
    begin
      if FKind = rtkEOF then
        Error('enum without its closing brace');
      if IsSymbol(';') then
        Next
      else if IsSymbol('option') then
        ParseOption(vOptions)
      else if IsSymbol('reserved') then
        ParseReserved
      else
      begin
        vComment := FTokenComment;
        vValue := vEnum.AddValue(ExpectIdent, 0);
        vValue.Comment := vComment;
        Expect('=');
        vValue.Number := IntegerRAL(ExpectInt);
        if IsSymbol('[') then
        begin
          Next;
          while not IsSymbol(']') do
          begin
            if FKind = rtkEOF then
              Error('options without their closing bracket');
            Next;
          end;
          Next;
        end;
        Expect(';');
      end;
    end;
    if vOptions.Values['features.enum_type'] = 'CLOSED' then
      vEnum.Closed := True;
  finally
    vOptions.Free;
  end;
  Next;
  if vEnum.Count = 0 then
    Error('enum ' + string(vEnum.Name) + ' has no values');
end;

procedure TRALProtoParser.ParseField(AMessage: TRALProtoMessageDef;
  AOneof: IntegerRAL; const ALabel: StringRAL);
var
  vField, vKey, vValue: TRALProtoFieldDef;
  vEntry: TRALProtoMessageDef;
  vType, vValueType, vName: StringRAL;
  vComment: StringRAL;
  vFieldType: TRALProtoFieldType;
begin
  vComment := FTokenComment;
  if IsSymbol('map') then
  begin
    Next;
    Expect('<');
    vType := ExpectIdent;
    Expect(',');
    vValueType := ExpectIdent;
    Expect('>');
    vField := AMessage.AddField;
    vField.Comment := vComment;
    vField.Name := ExpectIdent;
    Expect('=');
    vField.Number := IntegerRAL(ExpectInt);
    vField.FieldLabel := rflRepeated;
    vField.FieldType := rftMessage;
    // the entry message protoc makes: <Name>Entry { key = 1; value = 2; }
    vEntry := TRALProtoMessageDef.Create;
    AMessage.FMessages.Add(vEntry);
    vEntry.FileDef := FFile;
    vEntry.Parent := AMessage;
    vEntry.IsMapEntry := True;
    vName := StringRAL(UpCase(Char(vField.Name[1]))) + Copy(vField.Name, 2, MaxInt);
    while Pos('_', string(vName)) > 0 do
      vName := Copy(vName, 1, Pos('_', string(vName)) - 1) +
        StringRAL(UpCase(Char((vName + ' ')[Pos('_', string(vName)) + 1]))) +
        Copy(vName, Pos('_', string(vName)) + 2, MaxInt);
    vEntry.Name := StringRAL(TrimRight(string(vName))) + 'Entry';
    vField.TypeName := vEntry.Name;
    vField.MapEntry := vEntry;
    vKey := vEntry.AddField;
    vKey.Name := 'key';
    vKey.Number := 1;
    vKey.TypeName := vType;
    vKey.FieldType := rftMessage;
    vKey.FieldLabel := rflImplicit;
    vValue := vEntry.AddField;
    vValue.Name := 'value';
    vValue.Number := 2;
    vValue.TypeName := vValueType;
    vValue.FieldType := rftMessage;
    vValue.FieldLabel := rflImplicit;
    ParseFieldOptions(vField);
    Expect(';');
    Exit;
  end;
  vField := AMessage.AddField;
  vField.Comment := vComment;
  vField.OneofIndex := AOneof;
  if ALabel = 'repeated' then
    vField.FieldLabel := rflRepeated
  else if ALabel = 'required' then
    vField.FieldLabel := rflRequired
  else if (ALabel = 'optional') or (FFile.Syntax <> rsxProto3) or (AOneof >= 0) then
    vField.FieldLabel := rflOptional
  else
    vField.FieldLabel := rflImplicit;
  if (ALabel = 'optional') and (FFile.Syntax = rsxEditions) then
    Error('editions have no "optional" label');
  vField.TypeName := ExpectIdent;
  if vField.TypeName = 'group' then
    Error('groups are not supported');
  vField.Name := ExpectIdent;
  Expect('=');
  vField.Number := IntegerRAL(ExpectInt);
  if (vField.Number < 1) or (vField.Number > 536870911) or
     ((vField.Number >= 19000) and (vField.Number <= 19999)) then
    Error('field number out of range');
  vField.FieldType := rftMessage;
  for vFieldType := Low(cScalarNames) to High(cScalarNames) do
    if cScalarNames[vFieldType] = vField.TypeName then
      vField.FieldType := vFieldType;
  // repeated scalars are packed in proto3 and editions unless told otherwise
  vField.PackedEncoding := (vField.FieldLabel = rflRepeated) and
    (FFile.Syntax <> rsxProto2) and not (vField.FieldType in [rftString, rftBytes]);
  ParseFieldOptions(vField);
  Expect(';');
end;

procedure TRALProtoParser.ParseFieldOptions(AField: TRALProtoFieldDef);
var
  vName, vValue: StringRAL;
begin
  if not IsSymbol('[') then
    Exit;
  Next;
  while True do
  begin
    vName := '';
    if IsSymbol('(') then
    begin
      Next;
      vName := '(' + ExpectIdent + ')';
      Expect(')');
      if FKind = rtkIdent then
        vName := vName + ExpectIdent;
    end
    else
      vName := ExpectIdent;
    Expect('=');
    vValue := ParseConstant;
    if vName = 'packed' then
      AField.PackedEncoding := vValue = 'true'
    else if vName = 'default' then
    begin
      AField.DefaultValue := vValue;
      AField.HasDefault := True;
    end
    else if vName = 'json_name' then
      AField.JSONName := vValue
    else if vName = 'features.field_presence' then
    begin
      if (vValue = 'IMPLICIT') and (AField.FieldLabel = rflOptional) and
         (AField.OneofIndex < 0) then
        AField.FieldLabel := rflImplicit
      else if vValue = 'LEGACY_REQUIRED' then
        AField.FieldLabel := rflRequired;
    end
    else if vName = 'features.repeated_field_encoding' then
      AField.PackedEncoding := (vValue = 'PACKED') and (AField.FieldLabel = rflRepeated);
    if IsSymbol(',') then
      Next
    else
      Break;
  end;
  Expect(']');
end;

procedure TRALProtoParser.ParseMessage(AParent: TRALProtoMessageDef);
var
  vMessage: TRALProtoMessageDef;
begin
  vMessage := TRALProtoMessageDef.Create;
  vMessage.Comment := FTokenComment;
  if AParent = nil then
    FFile.FMessages.Add(vMessage)
  else
    AParent.FMessages.Add(vMessage);
  vMessage.FileDef := FFile;
  vMessage.Parent := AParent;
  Next;
  vMessage.Name := ExpectIdent;
  Expect('{');
  ParseMessageBody(vMessage);
end;

procedure TRALProtoParser.ParseMessageBody(AMessage: TRALProtoMessageDef);
var
  vLabel: StringRAL;
  vOneof: IntegerRAL;
  vOptions: TStringList;
begin
  vOptions := TStringList.Create;
  try
    while not IsSymbol('}') do
    begin
      if FKind = rtkEOF then
        Error('message without its closing brace');
      if IsSymbol(';') then
        Next
      else if IsSymbol('message') then
        ParseMessage(AMessage)
      else if IsSymbol('enum') then
        ParseEnum(AMessage)
      else if IsSymbol('option') then
        ParseOption(vOptions)
      else if IsSymbol('reserved') or IsSymbol('extensions') then
        ParseReserved
      else if IsSymbol('extend') then
      begin
        Next;
        ExpectIdent;
        SkipBlock;
      end
      else if IsSymbol('oneof') then
      begin
        Next;
        vOneof := AMessage.Oneofs.Add(string(ExpectIdent));
        Expect('{');
        while not IsSymbol('}') do
        begin
          if FKind = rtkEOF then
            Error('oneof without its closing brace');
          if IsSymbol(';') then
            Next
          else if IsSymbol('option') then
            ParseOption(vOptions)
          else
            ParseField(AMessage, vOneof, '');
        end;
        Next;
      end
      else
      begin
        vLabel := '';
        if IsSymbol('repeated') or IsSymbol('optional') or IsSymbol('required') then
        begin
          vLabel := FText;
          Next;
        end;
        ParseField(AMessage, -1, vLabel);
      end;
    end;
  finally
    vOptions.Free;
  end;
  Next;
end;

procedure TRALProtoParser.ParseOption(AOptions: TStringList);
var
  vName: StringRAL;
begin
  Next;
  vName := '';
  while not IsSymbol('=') do
  begin
    if FKind in [rtkEOF] then
      Error('option without "="');
    vName := vName + FText;
    Next;
  end;
  Next;
  AOptions.Values[string(vName)] := string(ParseConstant);
  Expect(';');
end;

procedure TRALProtoParser.ParseReserved;
begin
  Next;
  while not IsSymbol(';') do
  begin
    if FKind = rtkEOF then
      Error('reserved without ";"');
    if IsSymbol('[') then
    begin
      // extensions 1 to max [declaration = ...]
      while not IsSymbol(']') do
      begin
        if FKind = rtkEOF then
          Error('options without their closing bracket');
        Next;
      end;
    end;
    Next;
  end;
  Next;
end;

procedure TRALProtoParser.ParseService;
var
  vService: TRALProtoServiceDef;
  vMethod: TRALProtoMethodDef;
  vOptions: TStringList;
begin
  vService := TRALProtoServiceDef.Create;
  vService.Comment := FTokenComment;
  FFile.FServices.Add(vService);
  vService.FileDef := FFile;
  Next;
  vService.Name := ExpectIdent;
  if FFile.Package <> '' then
    vService.FullName := FFile.Package + '.' + vService.Name
  else
    vService.FullName := vService.Name;
  Expect('{');
  vOptions := TStringList.Create;
  try
    while not IsSymbol('}') do
    begin
      if FKind = rtkEOF then
        Error('service without its closing brace');
      if IsSymbol(';') then
        Next
      else if IsSymbol('option') then
        ParseOption(vOptions)
      else if IsSymbol('rpc') then
      begin
        vMethod := vService.AddMethod;
        vMethod.Comment := FTokenComment;
        Next;
        vMethod.Name := ExpectIdent;
        Expect('(');
        if IsSymbol('stream') then
        begin
          vMethod.ClientStreaming := True;
          Next;
        end;
        vMethod.InputName := ExpectIdent;
        Expect(')');
        Expect('returns');
        Expect('(');
        if IsSymbol('stream') then
        begin
          vMethod.ServerStreaming := True;
          Next;
        end;
        vMethod.OutputName := ExpectIdent;
        Expect(')');
        if IsSymbol('{') then
          SkipBlock
        else
          Expect(';');
      end
      else
        Error(Format('unexpected "%s" in a service', [string(FText)]));
    end;
  finally
    vOptions.Free;
  end;
  Next;
end;

procedure TRALProtoParser.SkipBlock;
var
  vDepth: IntegerRAL;
begin
  Expect('{');
  vDepth := 1;
  while vDepth > 0 do
  begin
    if FKind = rtkEOF then
      Error('block without its closing brace');
    if IsSymbol('{') then
      Inc(vDepth)
    else if IsSymbol('}') then
      Dec(vDepth);
    Next;
  end;
end;

procedure TRALProtoParser.SkipSpaces;
var
  vLine: StringRAL;
  vLastLine: IntegerRAL;
begin
  vLastLine := -10;
  while FPosition <= Length(FSource) do
  begin
    if FSource[FPosition] = #10 then
    begin
      Inc(FLine);
      FColumn := 1;
      Inc(FPosition);
    end
    else if CharInSet(Char(FSource[FPosition]), [#9, #11, #12, #13, ' ']) then
    begin
      Inc(FPosition);
      Inc(FColumn);
    end
    else if Copy(FSource, FPosition, 2) = '//' then
    begin
      Inc(FPosition, 2);
      vLine := '';
      while (FPosition <= Length(FSource)) and (FSource[FPosition] <> #10) do
      begin
        RALProtoAppendByte(vLine, Ord(FSource[FPosition]));
        Inc(FPosition);
      end;
      // a comment block is the lines right above the token; a blank line starts a new one
      if FLine <> vLastLine + 1 then
        FComment := '';
      vLastLine := FLine;
      vLine := StringRAL(Trim(string(vLine)));
      if FComment <> '' then
        FComment := FComment + ' ';
      FComment := FComment + vLine;
    end
    else if Copy(FSource, FPosition, 2) = '/*' then
    begin
      Inc(FPosition, 2);
      vLine := '';
      while (FPosition <= Length(FSource)) and (Copy(FSource, FPosition, 2) <> '*/') do
      begin
        if FSource[FPosition] = #10 then
        begin
          Inc(FLine);
          vLine := vLine + ' ';
        end
        else if FSource[FPosition] <> #13 then
          RALProtoAppendByte(vLine, Ord(FSource[FPosition]));
        Inc(FPosition);
      end;
      if FPosition > Length(FSource) then
        Error('comment without its end');
      Inc(FPosition, 2);
      FComment := StringRAL(Trim(string(vLine)));
      vLastLine := FLine;
    end
    else
      Break;
  end;
end;

function TRALProtoParser.TakeComment: StringRAL;
begin
  Result := FComment;
  FComment := '';
end;

{ TRALProtoSchema }

constructor TRALProtoSchema.Create;
begin
  inherited Create;
  FFiles := TObjectList.Create(True);
  FSearchPaths := TStringList.Create;
  FTypes := TStringList.Create;
  FTypes.Sorted := True;
  FTypes.Duplicates := dupError;
  FTypes.CaseSensitive := True;
end;

destructor TRALProtoSchema.Destroy;
begin
  FTypes.Free;
  FSearchPaths.Free;
  FFiles.Free;
  inherited Destroy;
end;

function TRALProtoSchema.FindFile(const AFileName: StringRAL): TRALProtoFileDef;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FFiles.Count - 1 do
  begin
    Result := TRALProtoFileDef(FFiles[vInt]);
    if Result.FileName = AFileName then
      Exit;
  end;
  Result := nil;
end;

function TRALProtoSchema.FindType(const AFullName: StringRAL): TObject;
var
  vIndex: Integer;
begin
  if FTypes.Find(string(AFullName), vIndex) then
    Result := FTypes.Objects[vIndex]
  else
    Result := nil;
end;

function TRALProtoSchema.GetFile(AIndex: IntegerRAL): TRALProtoFileDef;
begin
  Result := TRALProtoFileDef(FFiles[AIndex]);
end;

function TRALProtoSchema.GetFileCount: IntegerRAL;
begin
  Result := FFiles.Count;
end;

function TRALProtoSchema.Load(const AFileName: StringRAL): TRALProtoFileDef;
var
  vSource: StringRAL;
  vFound: Boolean;
  vInt: IntegerRAL;
  vPath: string;
  vStream: TFileStream;
begin
  Result := FindFile(AFileName);
  if Result <> nil then
    Exit;
  vFound := False;
  vSource := '';
  vPath := StringReplace(string(AFileName), '/', PathDelim, [rfReplaceAll]);
  if FileExists(vPath) then
    vFound := True
  else
    for vInt := 0 to FSearchPaths.Count - 1 do
      if FileExists(IncludeTrailingPathDelimiter(FSearchPaths[vInt]) + vPath) then
      begin
        vPath := IncludeTrailingPathDelimiter(FSearchPaths[vInt]) + vPath;
        vFound := True;
        Break;
      end;
  if vFound then
  begin
    vStream := TFileStream.Create(vPath, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(vSource, vStream.Size);
      if vStream.Size > 0 then
        vStream.ReadBuffer(vSource[1], vStream.Size);
    finally
      vStream.Free;
    end;
  end
  else
    vFound := LoadBuiltIn(AFileName, vSource);
  if not vFound and Assigned(FOnLoad) then
    FOnLoad(AFileName, vSource, vFound);
  if not vFound then
    SchemaError('file not found: ' + string(AFileName));
  Result := LoadFromText(AFileName, vSource);
end;

function TRALProtoSchema.LoadBuiltIn(const AFileName: StringRAL;
  out ASource: StringRAL): Boolean;
begin
  Result := True;
  if AFileName = 'google/protobuf/empty.proto' then
    ASource := cEmptyProto
  else if AFileName = 'google/protobuf/timestamp.proto' then
    ASource := cTimestampProto
  else if AFileName = 'google/protobuf/duration.proto' then
    ASource := cDurationProto
  else if AFileName = 'google/protobuf/wrappers.proto' then
    ASource := cWrappersProto
  else if AFileName = 'google/protobuf/struct.proto' then
    ASource := cStructProto
  else if AFileName = 'google/protobuf/field_mask.proto' then
    ASource := cFieldMaskProto
  else if AFileName = 'google/protobuf/any.proto' then
    ASource := cAnyProto
  else
    Result := False;
end;

function TRALProtoSchema.LoadFromText(const AFileName,
  ASource: StringRAL): TRALProtoFileDef;
var
  vParser: TRALProtoParser;
  vInt: IntegerRAL;
  vFile: TRALProtoFileDef;
begin
  Result := TRALProtoFileDef.Create;
  try
    Result.FileName := AFileName;
    Result.PascalUnit := RALProtoUnitNameOf(AFileName);
    if Copy(AFileName, 1, 16) = 'google/protobuf/' then
      Result.PascalUnit := 'RALProtobufWKT';
    vParser := TRALProtoParser.Create;
    try
      vParser.Parse(Result, ASource);
    finally
      vParser.Free;
    end;
  except
    Result.Free;
    raise;
  end;
  // imports first, so the files come out in dependency order
  for vInt := 0 to Result.Imports.Count - 1 do
    Load(StringRAL(Result.Imports[vInt]));
  FFiles.Add(Result);
  RegisterTypes(Result);
  ResolveFile(Result);
  for vInt := 0 to FFiles.Count - 1 do
  begin
    vFile := TRALProtoFileDef(FFiles[vInt]);
    if (vFile <> Result) and (vFile.PascalUnit = Result.PascalUnit) and
       (vFile.PascalUnit <> 'RALProtobufWKT') then
      SchemaError(Format('%s and %s give the same unit name %s',
        [string(vFile.FileName), string(AFileName), string(Result.PascalUnit)]));
  end;
end;

procedure TRALProtoSchema.RegisterTypes(AFile: TRALProtoFileDef);

  procedure Add(const AName: StringRAL; AType: TObject);
  begin
    if FindType(AName) <> nil then
      SchemaError(Format('%s: %s is declared twice', [string(AFile.FileName), string(AName)]));
    FTypes.AddObject(string(AName), AType);
  end;

  procedure AddMessage(AMessage: TRALProtoMessageDef; const APrefix: StringRAL);
  var
    vInt: IntegerRAL;
  begin
    if APrefix <> '' then
      AMessage.FullName := APrefix + '.' + AMessage.Name
    else
      AMessage.FullName := AMessage.Name;
    Add(AMessage.FullName, AMessage);
    for vInt := 0 to AMessage.EnumCount - 1 do
    begin
      AMessage.Enums[vInt].FullName := AMessage.FullName + '.' + AMessage.Enums[vInt].Name;
      Add(AMessage.Enums[vInt].FullName, AMessage.Enums[vInt]);
    end;
    for vInt := 0 to AMessage.MessageCount - 1 do
      AddMessage(AMessage.Messages[vInt], AMessage.FullName);
  end;

var
  vInt: IntegerRAL;
begin
  for vInt := 0 to AFile.EnumCount - 1 do
  begin
    if AFile.Package <> '' then
      AFile.Enums[vInt].FullName := AFile.Package + '.' + AFile.Enums[vInt].Name
    else
      AFile.Enums[vInt].FullName := AFile.Enums[vInt].Name;
    Add(AFile.Enums[vInt].FullName, AFile.Enums[vInt]);
  end;
  for vInt := 0 to AFile.MessageCount - 1 do
    AddMessage(AFile.Messages[vInt], AFile.Package);
end;

procedure TRALProtoSchema.ResolveFile(AFile: TRALProtoFileDef);

  procedure ResolveField(AMessage: TRALProtoMessageDef; AField: TRALProtoFieldDef);
  var
    vType: TObject;
    vFieldType: TRALProtoFieldType;
  begin
    if AField.JSONName = '' then
    begin
      AField.JSONName := AField.Name;
      while Pos('_', string(AField.JSONName)) > 0 do
        AField.JSONName := Copy(AField.JSONName, 1, Pos('_', string(AField.JSONName)) - 1) +
          StringRAL(UpCase(Char((AField.JSONName + ' ')[Pos('_', string(AField.JSONName)) + 1]))) +
          Copy(AField.JSONName, Pos('_', string(AField.JSONName)) + 2, MaxInt);
      AField.JSONName := StringRAL(TrimRight(string(AField.JSONName)));
    end;
    if AField.FieldType <> rftMessage then
      Exit;
    if AField.MapEntry <> nil then
    begin
      AField.MessageType := AField.MapEntry;
      Exit;
    end;
    for vFieldType := Low(cScalarNames) to High(cScalarNames) do
      if cScalarNames[vFieldType] = AField.TypeName then
      begin
        AField.FieldType := vFieldType;
        Exit;
      end;
    vType := ResolveType(AMessage.FullName, AField.TypeName);
    if vType is TRALProtoMessageDef then
      AField.MessageType := TRALProtoMessageDef(vType)
    else if vType is TRALProtoEnumDef then
    begin
      AField.FieldType := rftEnum;
      AField.EnumType := TRALProtoEnumDef(vType);
    end
    else
      SchemaError(Format('%s: unknown type %s in %s', [string(AFile.FileName),
        string(AField.TypeName), string(AMessage.FullName)]));
    if (AField.FieldType = rftMessage) and (AField.FieldLabel = rflImplicit) then
      AField.FieldLabel := rflOptional;
  end;

  procedure ResolveMessage(AMessage: TRALProtoMessageDef);
  var
    vInt: IntegerRAL;
  begin
    for vInt := 0 to AMessage.FieldCount - 1 do
      ResolveField(AMessage, AMessage.Fields[vInt]);
    for vInt := 0 to AMessage.MessageCount - 1 do
      ResolveMessage(AMessage.Messages[vInt]);
  end;

  function ResolveMethodType(const AName: StringRAL): TRALProtoMessageDef;
  var
    vType: TObject;
  begin
    vType := ResolveType(AFile.Package, AName);
    if not (vType is TRALProtoMessageDef) then
      SchemaError(Format('%s: unknown message %s', [string(AFile.FileName), string(AName)]));
    Result := TRALProtoMessageDef(vType);
  end;

var
  vInt, vMethod: IntegerRAL;
  vService: TRALProtoServiceDef;
begin
  for vInt := 0 to AFile.MessageCount - 1 do
    ResolveMessage(AFile.Messages[vInt]);
  for vInt := 0 to AFile.ServiceCount - 1 do
  begin
    vService := AFile.Services[vInt];
    for vMethod := 0 to vService.Count - 1 do
    begin
      vService.Methods[vMethod].InputType :=
        ResolveMethodType(vService.Methods[vMethod].InputName);
      vService.Methods[vMethod].OutputType :=
        ResolveMethodType(vService.Methods[vMethod].OutputName);
    end;
  end;
end;

function TRALProtoSchema.ResolveType(const AScope, ATypeName: StringRAL): TObject;
var
  vScope, vFirst, vRest: StringRAL;
  vPos: IntegerRAL;
  vFound: TObject;
begin
  if (ATypeName <> '') and (ATypeName[1] = '.') then
  begin
    Result := FindType(Copy(ATypeName, 2, MaxInt));
    Exit;
  end;
  // the first part is looked for from the innermost scope outward, the rest inside it
  vPos := Pos('.', string(ATypeName));
  if vPos > 0 then
  begin
    vFirst := Copy(ATypeName, 1, vPos - 1);
    vRest := Copy(ATypeName, vPos, MaxInt);
  end
  else
  begin
    vFirst := ATypeName;
    vRest := '';
  end;
  vScope := AScope;
  while True do
  begin
    if vScope <> '' then
      vFound := FindType(vScope + '.' + vFirst)
    else
      vFound := FindType(vFirst);
    if vFound <> nil then
    begin
      if vRest = '' then
        Result := vFound
      else if vScope <> '' then
        Result := FindType(vScope + '.' + vFirst + vRest)
      else
        Result := FindType(vFirst + vRest);
      Exit;
    end;
    if vScope = '' then
      Break;
    vPos := Length(vScope);
    while (vPos > 0) and (vScope[vPos] <> '.') do
      Dec(vPos);
    if vPos > 0 then
      vScope := Copy(vScope, 1, vPos - 1)
    else
      vScope := '';
  end;
  // a package prefix that is not a type: a.b.Type seen from anywhere
  Result := FindType(ATypeName);
end;

end.
