/// Writes the Pascal unit of a .proto file: a class per message on RALProtobuf, with no
/// RTTI at run time.
unit RALProtoCodeGen;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALExceptions, RALProtoSchema;

type
  /// What the generator writes besides the messages.
  TRALProtoCodeGenOptions = record
    /// The unit name; empty takes the file's (RALProtoUnitNameOf).
    PascalUnit: StringRAL;
    /// Writes a gRPC client and service base class per service (RALModuleGrpcPackage).
    Grpc: Boolean;
  end;

/// Default options: the file's unit name; gRPC classes when the file has services.
function RALProtoCodeGenOptions: TRALProtoCodeGenOptions;
/// The Pascal class name of a message.
function RALProtoClassName(AMessage: TRALProtoMessageDef): StringRAL;
/// The Pascal unit of AFile (loaded in ASchema with its imports).
function RALProtoToPascal(ASchema: TRALProtoSchema; AFile: TRALProtoFileDef;
  const AOptions: TRALProtoCodeGenOptions): StringRAL;

implementation

const
  /// Words a Pascal identifier may not be, and names the base class or the RTL use.
  cReserved: array[0..98] of string = ('and', 'array', 'as', 'asm', 'begin', 'case',
    'class', 'const', 'constructor', 'destructor', 'dispinterface', 'div', 'do',
    'downto', 'else', 'end', 'except', 'exports', 'file', 'finalization', 'finally',
    'for', 'function', 'goto', 'if', 'implementation', 'in', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library', 'mod', 'nil',
    'not', 'object', 'of', 'or', 'out', 'packed', 'procedure', 'program', 'property',
    'raise', 'record', 'repeat', 'resourcestring', 'set', 'shl', 'shr', 'string',
    'then', 'threadvar', 'to', 'try', 'type', 'unit', 'until', 'uses', 'var',
    'while', 'with', 'xor', 'operator', 'self', 'result', 'absolute', 'abstract',
    'assign', 'clear', 'clone', 'create', 'decode', 'destroy', 'encode', 'free',
    'classname', 'classtype', 'unknownfields', 'tojson', 'fromjson', 'readjson',
    'mergefrom', 'encodeto', 'decodefromstream', 'encodetostream', 'tojsonvalue',
    'readjsonvalue', 'specialize', 'generic', 'on', 'strict', 'private', 'public',
    'protected', 'published', 'default');
  /// RTL identifiers an enum constant would hide (High, Length...); they get a prefix.
  cBuiltIns: array[0..93] of string = ('high', 'low', 'length', 'setlength', 'ord', 'chr', 'succ', 'pred', 'inc',
    'dec', 'sizeof', 'assigned', 'exit', 'break', 'continue', 'true', 'false',
    'abs', 'round', 'trunc', 'copy', 'pos', 'delete', 'insert', 'format',
    'integer', 'boolean', 'byte', 'word', 'cardinal', 'double', 'single', 'int64',
    'uint64', 'char', 'maxint', 'frac', 'int', 'sqr', 'sqrt', 'odd', 'new',
    'dispose', 'halt', 'write', 'writeln', 'read', 'readln', 'include', 'exclude',
    'move', 'fillchar', 'now', 'date', 'time', 'result', 'self', 'freeandnil',
    'inttostr', 'strtoint', 'trim', 'uppercase', 'lowercase', 'stringral',
    'integerral', 'uint64ral', 'pchar', 'pointer', 'variant', 'currency',
    'extended', 'real', 'text', 'shortint', 'smallint', 'longint', 'longword',
    'nativeint', 'nativeuint', 'qword', 'ansistring', 'widestring',
    'unicodestring', 'rawbytestring', 'utf8string', 'ptruint', 'ptrint',
    'sysutils', 'classes', 'system', 'raltypes', 'ralprotobuf', 'raljson',
    'ralexceptions');
  /// Type names the generated code or the RTL already use.
  cReservedTypes: array[0..23] of string = ('tbytes', 'tobject', 'tpersistent',
    'tclass', 'tstream', 'tlist', 'tstrings', 'tstringlist', 'tcomponent',
    'tcollection', 'tdatetime', 'ttimestamp', 'tguid', 'tarray', 'tvalue',
    'tencoding', 'tformatsettings', 'tthread', 'tinterfacedobject', 'tmemorystream',
    'tfilestream', 'tobjectlist', 'texception', 'tmethod');

type
  /// The writer: a buffer of lines and the schema being written.
  TRALProtoWriterGen = class
  private
    /// The enums found.
    FEnums: TList;
    /// The file being written.
    FFile: TRALProtoFileDef;
    /// The output lines.
    FLines: TStringList;
    /// The messages found.
    FMessages: TList;
    /// The options of the run.
    FOptions: TRALProtoCodeGenOptions;
    /// The schema the file belongs to.
    FSchema: TRALProtoSchema;

    /// Appends a line to the output.
    procedure Add(const ALine: string); overload;
    /// Appends a line to the output.
    procedure Add(const AFormat: string; const AArgs: array of const); overload;
    /// Gathers the messages and enums of the file, nested ones included.
    procedure Collect;
    /// The Pascal name of a field.
    function FieldName(AMessage: TRALProtoMessageDef; AField: TRALProtoFieldDef): string;
    /// Writes the class declaration of a message.
    procedure WriteClass(AMessage: TRALProtoMessageDef);
    /// Writes the method bodies of a message class.
    procedure WriteClassBody(AMessage: TRALProtoMessageDef);
    /// Writes a .proto comment as a doc comment.
    procedure WriteComment(const AIndent, AComment: string);
    /// Writes the binary decoding of a message.
    procedure WriteDecode(AMessage: TRALProtoMessageDef);
    /// Writes the binary encoding of a message.
    procedure WriteEncode(AMessage: TRALProtoMessageDef);
    /// Writes the name and value conversion functions of an enum.
    procedure WriteEnumFunctions(AEnum: TRALProtoEnumDef; AInterface: Boolean);
    /// Writes the implementation of the gRPC classes.
    procedure WriteGrpcImplementation;
    /// Writes the declarations of the gRPC classes.
    procedure WriteGrpcInterface;
    /// Writes the JSON reading of a message.
    procedure WriteJSONRead(AMessage: TRALProtoMessageDef);
    /// Writes the JSON writing of a message.
    procedure WriteJSONWrite(AMessage: TRALProtoMessageDef);
    /// Writes the list class of a message type.
    procedure WriteListClass(AMessage: TRALProtoMessageDef; AInterface: Boolean);
  public
    /// Creates the writer for a file of a schema.
    constructor Create(ASchema: TRALProtoSchema; AFile: TRALProtoFileDef;
      const AOptions: TRALProtoCodeGenOptions);
    destructor Destroy; override;

    /// Writes the whole unit.
    function Generate: StringRAL;
  end;

/// Whether AName is a Pascal reserved word or a member of the base class.
function IsReserved(const AName: string): Boolean;
var
  vInt: IntegerRAL;
begin
  for vInt := Low(cReserved) to High(cReserved) do
    if SameText(cReserved[vInt], AName) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

/// snake_case to PascalCase.
function PascalCase(const AName: StringRAL): string;
var
  vInt: IntegerRAL;
  vUpper: Boolean;
begin
  Result := '';
  vUpper := True;
  for vInt := 1 to Length(AName) do
    if AName[vInt] = '_' then
      vUpper := True
    else if vUpper then
    begin
      Result := Result + UpCase(Char(AName[vInt]));
      vUpper := False;
    end
    else
      Result := Result + Char(AName[vInt]);
  if Result = '' then
    Result := 'Field';
end;

/// The name chain of a message or enum below the package: Outer_Inner.
function ScopedName(AParent: TRALProtoMessageDef; const AName: StringRAL): string;
begin
  Result := string(AName);
  while AParent <> nil do
  begin
    Result := string(AParent.Name) + '_' + Result;
    AParent := AParent.Parent;
  end;
end;

/// The type prefix of a file: TProto for the google.protobuf types, T otherwise.
function TypePrefix(AFile: TRALProtoFileDef): string;
begin
  if AFile.Package = 'google.protobuf' then
    Result := 'TProto'
  else
    Result := 'T';
end;

/// A type name kept off the RTL names generated code uses.
function SafeTypeName(const AName: string): string;
var
  vInt: IntegerRAL;
begin
  Result := AName;
  for vInt := Low(cReservedTypes) to High(cReservedTypes) do
    if SameText(cReservedTypes[vInt], AName) then
    begin
      Result := AName + '_';
      Exit;
    end;
end;

function RALProtoClassName(AMessage: TRALProtoMessageDef): StringRAL;
begin
  Result := StringRAL(SafeTypeName(TypePrefix(AMessage.FileDef) +
    ScopedName(AMessage.Parent, AMessage.Name)));
end;

/// The Pascal type of an enum.
function EnumTypeName(AEnum: TRALProtoEnumDef): string;
begin
  Result := SafeTypeName(TypePrefix(AEnum.FileDef) + ScopedName(AEnum.Parent, AEnum.Name));
end;

/// The prefix of an enum's functions: the type name without its T.
function EnumFuncName(AEnum: TRALProtoEnumDef): string;
begin
  Result := Copy(EnumTypeName(AEnum), 2, MaxInt);
end;

/// The constant of an enum value: values are siblings of their enum, as in C++.
function EnumConstName(AEnum: TRALProtoEnumDef; AValue: TRALProtoEnumValueDef): string;
var
  vInt: IntegerRAL;
begin
  if AEnum.Parent <> nil then
    Result := ScopedName(AEnum.Parent.Parent, AEnum.Parent.Name) + '_' + string(AValue.Name)
  else
    Result := string(AValue.Name);
  if AEnum.FileDef.Package = 'google.protobuf' then
    Result := 'Proto_' + Result;
  for vInt := Low(cBuiltIns) to High(cBuiltIns) do
    if SameText(cBuiltIns[vInt], Result) then
    begin
      Result := EnumFuncName(AEnum) + '_' + Result;
      Break;
    end;
  if IsReserved(Result) then
    Result := Result + '_';
end;

/// The list class of a message.
function ListClassName(AMessage: TRALProtoMessageDef): string;
begin
  Result := string(RALProtoClassName(AMessage)) + 'List';
end;

/// The Pascal type of a singular value of AField.
function ValueType(AField: TRALProtoFieldDef): string;
begin
  case AField.FieldType of
    rftDouble: Result := 'Double';
    rftFloat: Result := 'Single';
    rftInt64, rftSInt64, rftSFixed64: Result := 'Int64';
    rftUInt64, rftFixed64: Result := 'UInt64RAL';
    rftInt32, rftSInt32, rftSFixed32: Result := 'Integer';
    rftUInt32, rftFixed32: Result := 'Cardinal';
    rftBool: Result := 'Boolean';
    rftString: Result := 'StringRAL';
    rftBytes: Result := 'TBytes';
    rftEnum: Result := EnumTypeName(AField.EnumType);
    rftMessage: Result := string(RALProtoClassName(AField.MessageType));
  end;
end;

/// The array type of a repeated scalar.
function ArrayType(AField: TRALProtoFieldDef): string;
begin
  case AField.FieldType of
    rftDouble: Result := 'TRALProtoDoubleArray';
    rftFloat: Result := 'TRALProtoSingleArray';
    rftInt64, rftSInt64, rftSFixed64: Result := 'TRALProtoInt64Array';
    rftUInt64, rftFixed64: Result := 'TRALProtoUInt64Array';
    rftInt32, rftSInt32, rftSFixed32, rftEnum: Result := 'TRALProtoInt32Array';
    rftUInt32, rftFixed32: Result := 'TRALProtoUInt32Array';
    rftBool: Result := 'TRALProtoBoolArray';
    rftString: Result := 'TRALProtoStringArray';
    rftBytes: Result := 'TRALProtoBytesArray';
  else
    Result := '';
  end;
end;

/// The Pascal type of the field's property.
function FieldType(AField: TRALProtoFieldDef): string;
begin
  if AField.FieldLabel <> rflRepeated then
    Result := ValueType(AField)
  else if AField.FieldType = rftMessage then
    Result := ListClassName(AField.MessageType)
  else
    Result := ArrayType(AField);
end;

/// The reader/writer method suffix of a field type.
function WireMethod(AField: TRALProtoFieldDef): string;
const
  cNames: array[TRALProtoFieldType] of string = ('Double', 'Float', 'Int64', 'UInt64',
    'Int32', 'Fixed64', 'Fixed32', 'Bool', 'String', 'Bytes', 'UInt32', 'SFixed32',
    'SFixed64', 'SInt32', 'SInt64', 'Enum', 'Message');
begin
  Result := cNames[AField.FieldType];
end;

/// The wire type constant of a singular value.
function WireConst(AField: TRALProtoFieldDef): string;
begin
  case AField.FieldType of
    rftDouble, rftFixed64, rftSFixed64: Result := 'rpwFixed64';
    rftFloat, rftFixed32, rftSFixed32: Result := 'rpwFixed32';
    rftString, rftBytes, rftMessage: Result := 'rpwLengthDelimited';
  else
    Result := 'rpwVarint';
  end;
end;

/// The TRALProtoScalar constant of a numeric field.
function ScalarConst(AField: TRALProtoFieldDef): string;
const
  cNames: array[TRALProtoFieldType] of string = ('rpsDouble', 'rpsFloat', 'rpsInt64',
    'rpsUInt64', 'rpsInt32', 'rpsFixed64', 'rpsFixed32', 'rpsBool', '', '',
    'rpsUInt32', 'rpsSFixed32', 'rpsSFixed64', 'rpsSInt32', 'rpsSInt64', 'rpsEnum', '');
begin
  Result := cNames[AField.FieldType];
end;

/// The repeated reader/writer suffix of a scalar field.
function RepeatedMethod(AField: TRALProtoFieldDef): string;
begin
  case AField.FieldType of
    rftDouble: Result := 'Double';
    rftFloat: Result := 'Single';
    rftInt64, rftSInt64, rftSFixed64: Result := 'Int64';
    rftUInt64, rftFixed64: Result := 'UInt64';
    rftInt32, rftSInt32, rftSFixed32, rftEnum: Result := 'Int32';
    rftUInt32, rftFixed32: Result := 'UInt32';
    rftBool: Result := 'Bool';
    rftString: Result := 'String';
    rftBytes: Result := 'Bytes';
  else
    Result := '';
  end;
end;

/// Whether the repeated reader/writer of the field takes a kind.
function RepeatedTakesKind(AField: TRALProtoFieldDef): Boolean;
begin
  Result := (RepeatedMethod(AField) = 'Int32') or (RepeatedMethod(AField) = 'Int64') or
    (RepeatedMethod(AField) = 'UInt32') or (RepeatedMethod(AField) = 'UInt64');
end;

/// The Pascal name of an rpc: a name the base classes or Pascal take gets an underscore.
function RpcName(const AName: StringRAL): string;
const
  cTaken: array[0..17] of string = ('execute', 'methodkind', 'servicename', 'callunary',
    'callserverstream', 'callclientstream', 'callbidistream', 'invoke', 'client',
    'compress', 'metadata', 'prefix', 'protocol', 'timeout', 'laststatus',
    'laststatusmessage', 'lastmetadata', 'maxmessagesize');
var
  vInt: IntegerRAL;
begin
  Result := string(AName);
  for vInt := Low(cTaken) to High(cTaken) do
    if SameText(cTaken[vInt], Result) then
    begin
      Result := Result + '_';
      Exit;
    end;
  if IsReserved(Result) then
    Result := Result + '_';
end;

/// The JSON maker of a singular value AExpr.
function JSONOf(AField: TRALProtoFieldDef; const AExpr: string): string;
begin
  case AField.FieldType of
    rftDouble: Result := 'RALProtoJSONDouble(' + AExpr + ')';
    rftFloat: Result := 'RALProtoJSONSingle(' + AExpr + ')';
    rftInt64, rftSInt64, rftSFixed64: Result := 'RALProtoJSONInt64(' + AExpr + ')';
    rftUInt64, rftFixed64: Result := 'RALProtoJSONUInt64(' + AExpr + ')';
    rftInt32, rftSInt32, rftSFixed32: Result := 'RALProtoJSONInt32(' + AExpr + ')';
    rftUInt32, rftFixed32: Result := 'RALProtoJSONUInt32(' + AExpr + ')';
    rftBool: Result := 'RALProtoJSONBool(' + AExpr + ')';
    rftString: Result := 'RALProtoJSONString(' + AExpr + ')';
    rftBytes: Result := 'RALProtoJSONBytes(' + AExpr + ')';
    rftEnum:
      if AField.EnumType.FullName = 'google.protobuf.NullValue' then
        Result := 'TRALJSONNull.Create'
      else
        Result := 'RALProtoJSONEnum(' + AExpr + ', ' + EnumFuncName(AField.EnumType) +
          'ToName(' + AExpr + '), AOptions)';
    rftMessage: Result := AExpr + '.ToJSONValue(AOptions)';
  end;
end;

/// The JSON reader of a singular scalar from AValue (enums are read apart).
function ReadOf(AField: TRALProtoFieldDef; const AValue: string): string;
begin
  case AField.FieldType of
    rftDouble: Result := 'RALProtoReadDouble(' + AValue + ')';
    rftFloat: Result := 'RALProtoReadSingle(' + AValue + ')';
    rftInt64, rftSInt64, rftSFixed64: Result := 'RALProtoReadInt64(' + AValue + ')';
    rftUInt64, rftFixed64: Result := 'RALProtoReadUInt64(' + AValue + ')';
    rftInt32, rftSInt32, rftSFixed32: Result := 'RALProtoReadInt32(' + AValue + ')';
    rftUInt32, rftFixed32: Result := 'RALProtoReadUInt32(' + AValue + ')';
    rftBool: Result := 'RALProtoReadBool(' + AValue + ')';
    rftString: Result := 'RALProtoReadString(' + AValue + ')';
    rftBytes: Result := 'RALProtoReadBytes(' + AValue + ')';
  else
    Result := '';
  end;
end;

/// The test that a singular implicit scalar AExpr holds a value other than the default.
function NotDefault(AField: TRALProtoFieldDef; const AExpr: string): string;
begin
  case AField.FieldType of
    rftDouble: Result := 'RALProtoDoubleBits(' + AExpr + ') <> 0';
    rftFloat: Result := 'RALProtoSingleBits(' + AExpr + ') <> 0';
    rftBool: Result := AExpr;
    rftString: Result := AExpr + ' <> ''''';
    rftBytes: Result := 'Length(' + AExpr + ') > 0';
    rftMessage: Result := AExpr + ' <> nil';
  else
    Result := AExpr + ' <> 0';
  end;
end;

/// A Pascal literal of a .proto default value.
function DefaultLiteral(AField: TRALProtoFieldDef): string;
var
  vText: string;
  vInt: IntegerRAL;
  vASCII: Boolean;
  vValue: TRALProtoEnumValueDef;
begin
  vText := string(AField.DefaultValue);
  case AField.FieldType of
    rftString:
      begin
        vASCII := True;
        for vInt := 1 to Length(AField.DefaultValue) do
          if (Ord(AField.DefaultValue[vInt]) < 32) or (Ord(AField.DefaultValue[vInt]) > 126) then
            vASCII := False;
        if vASCII then
          Result := QuotedStr(vText)
        else
        begin
          Result := '';
          for vInt := 1 to Length(AField.DefaultValue) do
            Result := Result + IntToHex(Ord(AField.DefaultValue[vInt]), 2);
          Result := 'RALProtoHexText(''' + Result + ''')';
        end;
      end;
    rftBool: Result := vText;
    rftEnum:
      begin
        vValue := AField.EnumType.FindValue(AField.DefaultValue);
        if vValue = nil then
          raise ERALException.Create('ralprotoc: unknown default ' + vText);
        Result := EnumConstName(AField.EnumType, vValue);
      end;
    rftDouble, rftFloat:
      if SameText(vText, 'inf') then
        Result := 'Math.Infinity'
      else if SameText(vText, '-inf') then
        Result := 'Math.NegInfinity'
      else if SameText(vText, 'nan') then
        Result := 'Math.NaN'
      else
        Result := vText;
    rftBytes: Result := 'nil';
  else
    if (Length(vText) > 2) and (vText[1] = '0') and CharInSet(vText[2], ['x', 'X']) then
      Result := '$' + Copy(vText, 3, MaxInt)
    else if (Length(vText) > 3) and (vText[1] = '-') and (vText[2] = '0') and
       CharInSet(vText[3], ['x', 'X']) then
      Result := '-$' + Copy(vText, 4, MaxInt)
    else
      Result := vText;
  end;
end;

/// The Pascal zero of a singular value.
function ZeroOf(AField: TRALProtoFieldDef): string;
begin
  if AField.HasDefault and (AField.FieldType <> rftMessage) then
  begin
    Result := DefaultLiteral(AField);
    Exit;
  end;
  case AField.FieldType of
    rftBool: Result := 'False';
    rftString: Result := '''''';
    rftBytes, rftMessage: Result := 'nil';
  else
    Result := '0';
  end;
end;

/// Whether the field has a presence flag of its own (not oneof, not message).
function HasFlag(AField: TRALProtoFieldDef): Boolean;
begin
  Result := (AField.FieldLabel in [rflOptional, rflRequired]) and (AField.OneofIndex < 0) and
    (AField.FieldType <> rftMessage);
end;

/// Whether reading null into the field is a value (google.protobuf.Value).
function TakesNull(AField: TRALProtoFieldDef): Boolean;
begin
  Result := ((AField.FieldType = rftMessage) and
    (AField.MessageType.FullName = 'google.protobuf.Value') and
    (AField.FieldLabel <> rflRepeated)) or
    ((AField.FieldType = rftEnum) and (AField.EnumType.FullName = 'google.protobuf.NullValue'));
end;

function RALProtoCodeGenOptions: TRALProtoCodeGenOptions;
begin
  Result.PascalUnit := '';
  Result.Grpc := True;
end;

function RALProtoToPascal(ASchema: TRALProtoSchema; AFile: TRALProtoFileDef;
  const AOptions: TRALProtoCodeGenOptions): StringRAL;
var
  vGen: TRALProtoWriterGen;
begin
  vGen := TRALProtoWriterGen.Create(ASchema, AFile, AOptions);
  try
    Result := vGen.Generate;
  finally
    vGen.Free;
  end;
end;

{ TRALProtoWriterGen }

constructor TRALProtoWriterGen.Create(ASchema: TRALProtoSchema;
  AFile: TRALProtoFileDef; const AOptions: TRALProtoCodeGenOptions);
begin
  inherited Create;
  FSchema := ASchema;
  FFile := AFile;
  FOptions := AOptions;
  FLines := TStringList.Create;
  FMessages := TList.Create;
  FEnums := TList.Create;
end;

destructor TRALProtoWriterGen.Destroy;
begin
  FLines.Free;
  FMessages.Free;
  FEnums.Free;
  inherited Destroy;
end;

procedure TRALProtoWriterGen.Add(const ALine: string);
begin
  FLines.Add(ALine);
end;

procedure TRALProtoWriterGen.Add(const AFormat: string; const AArgs: array of const);
begin
  FLines.Add(Format(AFormat, AArgs));
end;

procedure TRALProtoWriterGen.Collect;

  procedure AddMessage(AMessage: TRALProtoMessageDef);
  var
    vInt: IntegerRAL;
  begin
    FMessages.Add(AMessage);
    for vInt := 0 to AMessage.EnumCount - 1 do
      FEnums.Add(AMessage.Enums[vInt]);
    for vInt := 0 to AMessage.MessageCount - 1 do
      AddMessage(AMessage.Messages[vInt]);
  end;

var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FFile.EnumCount - 1 do
    FEnums.Add(FFile.Enums[vInt]);
  for vInt := 0 to FFile.MessageCount - 1 do
    AddMessage(FFile.Messages[vInt]);
end;

function TRALProtoWriterGen.FieldName(AMessage: TRALProtoMessageDef;
  AField: TRALProtoFieldDef): string;
var
  vInt: IntegerRAL;
  vOther: string;
begin
  Result := PascalCase(AField.Name);
  if IsReserved(Result) then
    Result := Result + '_';
  // two .proto names that meet in Pascal (foo_bar and fooBar): the later gets its number
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    if AMessage.Fields[vInt] = AField then
      Break;
    vOther := PascalCase(AMessage.Fields[vInt].Name);
    if SameText(vOther, PascalCase(AField.Name)) then
    begin
      Result := Result + IntToStr(AField.Number);
      Break;
    end;
  end;
end;

function TRALProtoWriterGen.Generate: StringRAL;
var
  vInt, vValue, vField: IntegerRAL;
  vUnitName: string;
  vUses: TStringList;
  vFile: TRALProtoFileDef;
  vEnum: TRALProtoEnumDef;
  vMessage: TRALProtoMessageDef;
  vCases: string;
  vHasOneof: Boolean;
begin
  Collect;
  vUnitName := string(FOptions.PascalUnit);
  if vUnitName = '' then
    vUnitName := string(FFile.PascalUnit);
  Add('/// Generated by ralprotoc from %s. Do not edit: run ralprotoc again.', [string(FFile.FileName)]);
  Add('unit %s;', [vUnitName]);
  Add('');
  Add('{$IFDEF FPC}');
  Add('  {$MODE DELPHI}');
  Add('{$ENDIF}');
  Add('');
  Add('interface');
  Add('');
  vUses := TStringList.Create;
  try
    vUses.Add('Classes');
    vUses.Add('SysUtils');
    vUses.Add('Math');
    vUses.Add('RALTypes');
    vUses.Add('RALJson');
    vUses.Add('RALExceptions');
    vUses.Add('RALProtobuf');
    for vInt := 0 to FFile.Imports.Count - 1 do
    begin
      vFile := FSchema.FindFile(StringRAL(FFile.Imports[vInt]));
      if (vFile <> nil) and (vUses.IndexOf(string(vFile.PascalUnit)) < 0) and
         (string(vFile.PascalUnit) <> vUnitName) then
        vUses.Add(string(vFile.PascalUnit));
    end;
    if FOptions.Grpc and (FFile.ServiceCount > 0) then
    begin
      vUses.Add('RALGrpc');
      vUses.Add('RALGrpcModule');
      vUses.Add('RALGrpcClient');
    end;
    Add('uses');
    for vInt := 0 to vUses.Count - 1 do
      if vInt < vUses.Count - 1 then
        Add('  %s,', [vUses[vInt]])
      else
        Add('  %s;', [vUses[vInt]]);
  finally
    vUses.Free;
  end;
  Add('');
  // enums and oneof cases first: the constants need them
  vHasOneof := False;
  for vInt := 0 to FMessages.Count - 1 do
    if TRALProtoMessageDef(FMessages[vInt]).Oneofs.Count > 0 then
      vHasOneof := True;
  if (FEnums.Count > 0) or vHasOneof then
  begin
    Add('type');
    for vInt := 0 to FEnums.Count - 1 do
    begin
      vEnum := TRALProtoEnumDef(FEnums[vInt]);
      WriteComment('  ', string(vEnum.Comment));
      if vEnum.Comment = '' then
        Add('  /// The .proto enum %s; values outside the constants are kept.', [string(vEnum.FullName)]);
      Add('  %s = type Integer;', [EnumTypeName(vEnum)]);
    end;
    for vInt := 0 to FMessages.Count - 1 do
    begin
      vMessage := TRALProtoMessageDef(FMessages[vInt]);
      for vValue := 0 to vMessage.Oneofs.Count - 1 do
      begin
        Add('  /// Which field of the oneof %s is set.', [vMessage.Oneofs[vValue]]);
        vCases := Format('  %s_%sCase = (%s_%s_NotSet', [RALProtoClassName(vMessage),
          PascalCase(StringRAL(vMessage.Oneofs[vValue])),
          Copy(string(RALProtoClassName(vMessage)), 2, MaxInt),
          PascalCase(StringRAL(vMessage.Oneofs[vValue]))]);
        for vField := 0 to vMessage.FieldCount - 1 do
          if vMessage.Fields[vField].OneofIndex = vValue then
            vCases := vCases + Format(', %s_%s_%s', [Copy(string(RALProtoClassName(vMessage)), 2, MaxInt),
              PascalCase(StringRAL(vMessage.Oneofs[vValue])), FieldName(vMessage, vMessage.Fields[vField])]);
        Add(vCases + ');');
      end;
    end;
    Add('');
  end;
  if FEnums.Count > 0 then
  begin
    Add('const');
    for vInt := 0 to FEnums.Count - 1 do
    begin
      vEnum := TRALProtoEnumDef(FEnums[vInt]);
      for vValue := 0 to vEnum.Count - 1 do
      begin
        WriteComment('  ', string(vEnum.Values[vValue].Comment));
        if vEnum.Values[vValue].Comment = '' then
          Add('  /// %s.%s.', [string(vEnum.FullName), string(vEnum.Values[vValue].Name)]);
        Add('  %s = %s(%d);', [EnumConstName(vEnum, vEnum.Values[vValue]), EnumTypeName(vEnum),
          vEnum.Values[vValue].Number]);
      end;
    end;
    Add('');
  end;
  if FMessages.Count > 0 then
  begin
    Add('type');
    for vInt := 0 to FMessages.Count - 1 do
      Add('  %s = class;', [RALProtoClassName(TRALProtoMessageDef(FMessages[vInt]))]);
    Add('');
    // lists first: a message may hold a list of a message declared after it
    for vInt := 0 to FMessages.Count - 1 do
      WriteListClass(TRALProtoMessageDef(FMessages[vInt]), True);
    for vInt := 0 to FMessages.Count - 1 do
      WriteClass(TRALProtoMessageDef(FMessages[vInt]));
  end;
  if FOptions.Grpc then
    WriteGrpcInterface;
  for vInt := 0 to FEnums.Count - 1 do
    WriteEnumFunctions(TRALProtoEnumDef(FEnums[vInt]), True);
  Add('implementation');
  Add('');
  for vInt := 0 to FEnums.Count - 1 do
    WriteEnumFunctions(TRALProtoEnumDef(FEnums[vInt]), False);
  for vInt := 0 to FMessages.Count - 1 do
  begin
    WriteListClass(TRALProtoMessageDef(FMessages[vInt]), False);
    WriteClassBody(TRALProtoMessageDef(FMessages[vInt]));
  end;
  if FOptions.Grpc then
    WriteGrpcImplementation;
  if FMessages.Count > 0 then
  begin
    Add('initialization');
    for vInt := 0 to FMessages.Count - 1 do
      Add('  RALProtoRegisterMessage(%s);', [RALProtoClassName(TRALProtoMessageDef(FMessages[vInt]))]);
    Add('');
    Add('finalization');
    for vInt := 0 to FMessages.Count - 1 do
      Add('  RALProtoUnregisterMessage(%s);', [RALProtoClassName(TRALProtoMessageDef(FMessages[vInt]))]);
    Add('');
  end;
  Add('end.');
  Result := StringRAL(FLines.Text);
end;

procedure TRALProtoWriterGen.WriteClass(AMessage: TRALProtoMessageDef);
var
  vInt: IntegerRAL;
  vField: TRALProtoFieldDef;
  vName, vClass, vCase: string;
begin
  vClass := string(RALProtoClassName(AMessage));
  WriteComment('  ', string(AMessage.Comment));
  if AMessage.Comment = '' then
    Add('  /// The .proto message %s.', [string(AMessage.FullName)]);
  Add('  %s = class(TRALProtoMessage)', [vClass]);
  Add('  private');
  for vInt := 0 to AMessage.Oneofs.Count - 1 do
    Add('    F%sCase: %s_%sCase;', [PascalCase(StringRAL(AMessage.Oneofs[vInt])), vClass,
      PascalCase(StringRAL(AMessage.Oneofs[vInt]))]);
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    vName := FieldName(AMessage, vField);
    Add('    F%s: %s;', [vName, FieldType(vField)]);
    if HasFlag(vField) then
      Add('    FHas%s: Boolean;', [vName]);
  end;
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    vName := FieldName(AMessage, vField);
    if vField.FieldLabel = rflRepeated then
      Continue;
    if (vField.OneofIndex >= 0) or HasFlag(vField) or (vField.FieldType = rftMessage) then
    begin
      if vField.FieldType in [rftString, rftBytes] then
        Add('    procedure Set%s(const AValue: %s);', [vName, ValueType(vField)])
      else
        Add('    procedure Set%s(AValue: %s);', [vName, ValueType(vField)]);
    end;
  end;
  Add('  protected');
  Add('    function DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;');
  Add('      AWire: TRALProtoWireType): Boolean; override;');
  Add('    procedure EncodeFields(AWriter: TRALProtoWriter); override;');
  Add('    function ReadJSONField(const AName: StringRAL; AValue: TRALJSON;');
  Add('      AOptions: TRALProtoJSONOptions): Boolean; override;');
  Add('    procedure ResetFields; override;');
  Add('    procedure WriteJSONFields(AObject: TRALJSONObject;');
  Add('      AOptions: TRALProtoJSONOptions); override;');
  Add('  public');
  Add('    constructor Create; override;');
  Add('    destructor Destroy; override;');
  Add('    /// ''%s''.', [string(AMessage.FullName)]);
  Add('    class function ProtoName: StringRAL; override;');
  for vInt := 0 to AMessage.Oneofs.Count - 1 do
  begin
    Add('    /// Clears whichever field of the oneof %s is set.', [AMessage.Oneofs[vInt]]);
    Add('    procedure Clear%s;', [PascalCase(StringRAL(AMessage.Oneofs[vInt]))]);
  end;
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    if HasFlag(vField) then
    begin
      vName := FieldName(AMessage, vField);
      Add('    /// Makes %s absent again.', [vName]);
      Add('    procedure Clear%s;', [vName]);
    end;
  end;
  for vInt := 0 to AMessage.Oneofs.Count - 1 do
  begin
    vCase := PascalCase(StringRAL(AMessage.Oneofs[vInt]));
    Add('    /// Which field of the oneof %s is set.', [AMessage.Oneofs[vInt]]);
    Add('    property %sCase: %s_%sCase read F%sCase;', [vCase, vClass, vCase, vCase]);
  end;
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    vName := FieldName(AMessage, vField);
    WriteComment('    ', string(vField.Comment));
    if vField.Comment = '' then
      Add('    /// Field %d, %s.', [vField.Number, string(vField.Name)]);
    if vField.FieldLabel = rflRepeated then
    begin
      if vField.FieldType = rftMessage then
        Add('    property %s: %s read F%s;', [vName, FieldType(vField), vName])
      else
        Add('    property %s: %s read F%s write F%s;', [vName, FieldType(vField), vName, vName]);
    end
    else if (vField.OneofIndex >= 0) or HasFlag(vField) or (vField.FieldType = rftMessage) then
      Add('    property %s: %s read F%s write Set%s;', [vName, FieldType(vField), vName, vName])
    else
      Add('    property %s: %s read F%s write F%s;', [vName, FieldType(vField), vName, vName]);
    if HasFlag(vField) then
    begin
      Add('    /// Whether %s was set.', [vName]);
      Add('    property Has%s: Boolean read FHas%s;', [vName, vName]);
    end;
  end;
  Add('  end;');
  Add('');
end;

procedure TRALProtoWriterGen.WriteClassBody(AMessage: TRALProtoMessageDef);
var
  vInt, vOther: IntegerRAL;
  vField: TRALProtoFieldDef;
  vName, vClass, vCase: string;
begin
  vClass := string(RALProtoClassName(AMessage));
  Add('{ %s }', [vClass]);
  Add('');
  Add('constructor %s.Create;', [vClass]);
  Add('begin');
  Add('  inherited Create;');
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    if (vField.FieldLabel = rflRepeated) and (vField.FieldType = rftMessage) then
      Add('  F%s := %s.Create;', [FieldName(AMessage, vField), ListClassName(vField.MessageType)]);
  end;
  Add('  ResetFields;');
  Add('end;');
  Add('');
  Add('class function %s.ProtoName: StringRAL;', [vClass]);
  Add('begin');
  Add('  Result := ''%s'';', [string(AMessage.FullName)]);
  Add('end;');
  Add('');
  Add('destructor %s.Destroy;', [vClass]);
  Add('begin');
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    if vField.FieldType = rftMessage then
      Add('  F%s.Free;', [FieldName(AMessage, vField)]);
  end;
  Add('  inherited Destroy;');
  Add('end;');
  Add('');
  // oneof clearing
  for vInt := 0 to AMessage.Oneofs.Count - 1 do
  begin
    vCase := PascalCase(StringRAL(AMessage.Oneofs[vInt]));
    Add('procedure %s.Clear%s;', [vClass, vCase]);
    Add('begin');
    for vOther := 0 to AMessage.FieldCount - 1 do
    begin
      vField := AMessage.Fields[vOther];
      if vField.OneofIndex <> vInt then
        Continue;
      vName := FieldName(AMessage, vField);
      if vField.FieldType = rftMessage then
        Add('  FreeAndNil(F%s);', [vName])
      else
        Add('  F%s := %s;', [vName, ZeroOf(vField)]);
    end;
    Add('  F%sCase := %s_%s_NotSet;', [vCase, Copy(vClass, 2, MaxInt), vCase]);
    Add('end;');
    Add('');
  end;
  // presence clearing
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    if not HasFlag(vField) then
      Continue;
    vName := FieldName(AMessage, vField);
    Add('procedure %s.Clear%s;', [vClass, vName]);
    Add('begin');
    Add('  F%s := %s;', [vName, ZeroOf(vField)]);
    Add('  FHas%s := False;', [vName]);
    Add('end;');
    Add('');
  end;
  WriteDecode(AMessage);
  WriteEncode(AMessage);
  WriteJSONRead(AMessage);
  // reset
  Add('procedure %s.ResetFields;', [vClass]);
  Add('begin');
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    vName := FieldName(AMessage, vField);
    if vField.FieldLabel = rflRepeated then
    begin
      if vField.FieldType = rftMessage then
        Add('  F%s.Clear;', [vName])
      else
        Add('  F%s := nil;', [vName]);
    end
    else if vField.FieldType = rftMessage then
      Add('  FreeAndNil(F%s);', [vName])
    else
      Add('  F%s := %s;', [vName, ZeroOf(vField)]);
    if HasFlag(vField) then
      Add('  FHas%s := False;', [vName]);
  end;
  for vInt := 0 to AMessage.Oneofs.Count - 1 do
  begin
    vCase := PascalCase(StringRAL(AMessage.Oneofs[vInt]));
    Add('  F%sCase := %s_%s_NotSet;', [vCase, Copy(vClass, 2, MaxInt), vCase]);
  end;
  Add('end;');
  Add('');
  // setters
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    vName := FieldName(AMessage, vField);
    if vField.FieldLabel = rflRepeated then
      Continue;
    if not ((vField.OneofIndex >= 0) or HasFlag(vField) or (vField.FieldType = rftMessage)) then
      Continue;
    if vField.FieldType in [rftString, rftBytes] then
      Add('procedure %s.Set%s(const AValue: %s);', [vClass, vName, ValueType(vField)])
    else
      Add('procedure %s.Set%s(AValue: %s);', [vClass, vName, ValueType(vField)]);
    Add('begin');
    if vField.OneofIndex >= 0 then
    begin
      vCase := PascalCase(StringRAL(AMessage.Oneofs[vField.OneofIndex]));
      if vField.FieldType = rftMessage then
      begin
        Add('  if AValue = F%s then', [vName]);
        Add('    Exit;');
        Add('  Clear%s;', [vCase]);
        Add('  F%s := AValue;', [vName]);
        Add('  if AValue <> nil then');
        Add('    F%sCase := %s_%s_%s;', [vCase, Copy(vClass, 2, MaxInt), vCase, vName]);
      end
      else
      begin
        Add('  Clear%s;', [vCase]);
        Add('  F%s := AValue;', [vName]);
        Add('  F%sCase := %s_%s_%s;', [vCase, Copy(vClass, 2, MaxInt), vCase, vName]);
      end;
    end
    else if vField.FieldType = rftMessage then
    begin
      Add('  if AValue = F%s then', [vName]);
      Add('    Exit;');
      Add('  F%s.Free;', [vName]);
      Add('  F%s := AValue;', [vName]);
    end
    else
    begin
      Add('  F%s := AValue;', [vName]);
      Add('  FHas%s := True;', [vName]);
    end;
    Add('end;');
    Add('');
  end;
  WriteJSONWrite(AMessage);
end;

procedure TRALProtoWriterGen.WriteComment(const AIndent, AComment: string);
var
  vText, vLine: string;
  vPos: IntegerRAL;
begin
  if AComment = '' then
    Exit;
  // the .proto comment may hold braces: a /// line comment never closes on them
  vText := AComment;
  while vText <> '' do
  begin
    if Length(vText) <= 90 then
    begin
      vLine := vText;
      vText := '';
    end
    else
    begin
      vPos := 90;
      while (vPos > 1) and (vText[vPos] <> ' ') do
        Dec(vPos);
      if vPos <= 1 then
        vPos := 90;
      vLine := Copy(vText, 1, vPos);
      vText := Copy(vText, vPos + 1, MaxInt);
    end;
    Add(AIndent + '/// ' + TrimRight(vLine));
  end;
end;

procedure TRALProtoWriterGen.WriteDecode(AMessage: TRALProtoMessageDef);
var
  vInt: IntegerRAL;
  vField: TRALProtoFieldDef;
  vName, vClass, vRead: string;
  vClosed: Boolean;
begin
  vClass := string(RALProtoClassName(AMessage));
  vClosed := False;
  for vInt := 0 to AMessage.FieldCount - 1 do
    if (AMessage.Fields[vInt].FieldType = rftEnum) and AMessage.Fields[vInt].EnumType.Closed and
       (AMessage.Fields[vInt].FieldLabel <> rflRepeated) then
      vClosed := True;
  Add('function %s.DecodeField(AReader: TRALProtoReader; AField: IntegerRAL;', [vClass]);
  Add('  AWire: TRALProtoWireType): Boolean;');
  if vClosed then
  begin
    Add('var');
    Add('  vEnum: Integer;');
  end;
  Add('begin');
  if AMessage.FieldCount = 0 then
  begin
    Add('  Result := False;');
    Add('end;');
    Add('');
    Exit;
  end;
  Add('  Result := True;');
  Add('  case AField of');
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    vName := FieldName(AMessage, vField);
    Add('    %d:', [vField.Number]);
    if vField.FieldLabel = rflRepeated then
    begin
      if vField.FieldType = rftMessage then
      begin
        Add('      if AWire = rpwLengthDelimited then');
        if vField.IsMap then
        begin
          Add('      begin');
          Add('        F%s.ReadItem(AReader);', [vName]);
          Add('        F%s.DropDuplicateKey;', [vName]);
          Add('      end');
        end
        else
          Add('        F%s.ReadItem(AReader)', [vName]);
      end
      else if vField.FieldType in [rftString, rftBytes] then
      begin
        Add('      if AWire = rpwLengthDelimited then');
        Add('        RALProtoAdd%s(F%s, AReader.Read%s)', [RepeatedMethod(vField), vName, WireMethod(vField)]);
      end
      else
      begin
        Add('      if AWire in [rpwLengthDelimited, %s] then', [WireConst(vField)]);
        if RepeatedTakesKind(vField) then
          Add('        AReader.ReadRepeated%s(AWire, %s, F%s)', [RepeatedMethod(vField),
            ScalarConst(vField), vName])
        else
          Add('        AReader.ReadRepeated%s(AWire, F%s)', [RepeatedMethod(vField), vName]);
      end;
      Add('      else');
      Add('        Result := False;');
      Continue;
    end;
    Add('      if AWire = %s then', [WireConst(vField)]);
    if vField.FieldType = rftMessage then
    begin
      Add('      begin');
      if vField.OneofIndex >= 0 then
      begin
        Add('        if F%s = nil then', [vName]);
        Add('          Set%s(%s.Create);', [vName, ValueType(vField)]);
      end
      else
      begin
        Add('        if F%s = nil then', [vName]);
        Add('          F%s := %s.Create;', [vName, ValueType(vField)]);
      end;
      Add('        AReader.ReadMessage(F%s);', [vName]);
      Add('      end');
    end
    else
    begin
      vRead := 'AReader.Read' + WireMethod(vField);
      if (vField.FieldType = rftEnum) and vField.EnumType.Closed then
      begin
        Add('      begin');
        Add('        vEnum := %s;', [vRead]);
        Add('        if %sToName(vEnum) = '''' then', [EnumFuncName(vField.EnumType)]);
        Add('          AddUnknownVarint(%d, UInt64RAL(Int64(vEnum)))', [vField.Number]);
        if (vField.OneofIndex >= 0) or HasFlag(vField) then
          Add('        else Set%s(vEnum);', [vName])
        else
          Add('        else F%s := vEnum;', [vName]);
        Add('      end');
      end
      else if (vField.OneofIndex >= 0) or HasFlag(vField) then
        Add('        Set%s(%s)', [vName, vRead])
      else
        Add('        F%s := %s', [vName, vRead]);
    end;
    Add('      else');
    Add('        Result := False;');
  end;
  Add('  else');
  Add('    Result := False;');
  Add('  end;');
  Add('end;');
  Add('');
end;

procedure TRALProtoWriterGen.WriteEncode(AMessage: TRALProtoMessageDef);
var
  vInt, vIndex: IntegerRAL;
  vField: TRALProtoFieldDef;
  vName, vClass, vCase: string;
  vOrder: TList;
begin
  vClass := string(RALProtoClassName(AMessage));
  Add('procedure %s.EncodeFields(AWriter: TRALProtoWriter);', [vClass]);
  Add('begin');
  vOrder := TList.Create;
  try
    // the canonical order is by field number
    for vInt := 0 to AMessage.FieldCount - 1 do
    begin
      vIndex := 0;
      while (vIndex < vOrder.Count) and
        (TRALProtoFieldDef(vOrder[vIndex]).Number < AMessage.Fields[vInt].Number) do
        Inc(vIndex);
      vOrder.Insert(vIndex, AMessage.Fields[vInt]);
    end;
    for vInt := 0 to vOrder.Count - 1 do
    begin
      vField := TRALProtoFieldDef(vOrder[vInt]);
      vName := FieldName(AMessage, vField);
      if vField.FieldLabel = rflRepeated then
      begin
        if vField.FieldType = rftMessage then
          Add('  F%s.EncodeTo(AWriter, %d);', [vName, vField.Number])
        else if vField.FieldType in [rftString, rftBytes] then
          Add('  AWriter.WriteRepeated%s(%d, F%s);', [RepeatedMethod(vField), vField.Number, vName])
        else if RepeatedTakesKind(vField) then
          Add('  AWriter.WriteRepeated%s(%d, %s, F%s, %s);', [RepeatedMethod(vField),
            vField.Number, ScalarConst(vField), vName, BoolToStr(vField.PackedEncoding, True)])
        else
          Add('  AWriter.WriteRepeated%s(%d, F%s, %s);', [RepeatedMethod(vField), vField.Number,
            vName, BoolToStr(vField.PackedEncoding, True)]);
        Continue;
      end;
      if AMessage.IsMapEntry and (vField.FieldType <> rftMessage) then
        Add('  begin')
      else if vField.OneofIndex >= 0 then
      begin
        vCase := PascalCase(StringRAL(AMessage.Oneofs[vField.OneofIndex]));
        Add('  if F%sCase = %s_%s_%s then', [vCase, Copy(vClass, 2, MaxInt), vCase, vName]);
        Add('  begin');
      end
      else if HasFlag(vField) then
      begin
        Add('  if FHas%s then', [vName]);
        Add('  begin');
      end
      else
      begin
        Add('  if %s then', [NotDefault(vField, 'F' + vName)]);
        Add('  begin');
      end;
      Add('    AWriter.WriteTag(%d, %s);', [vField.Number, WireConst(vField)]);
      Add('    AWriter.Write%s(F%s);', [WireMethod(vField), vName]);
      Add('  end;');
    end;
  finally
    vOrder.Free;
  end;
  Add('end;');
  Add('');
end;

procedure TRALProtoWriterGen.WriteEnumFunctions(AEnum: TRALProtoEnumDef;
  AInterface: Boolean);
var
  vInt, vPrior: IntegerRAL;
  vFunc: string;
  vDuplicate: Boolean;
begin
  vFunc := EnumFuncName(AEnum);
  if AInterface then
  begin
    Add('/// The name of a %s value, empty for a number without one.', [string(AEnum.Name)]);
    Add('function %sToName(AValue: Integer): StringRAL;', [vFunc]);
    Add('/// The value of a %s name; False when there is none.', [string(AEnum.Name)]);
    Add('function %sFromName(const AName: StringRAL; out AValue: Integer): Boolean;', [vFunc]);
    Add('/// A %s from JSON (a name or a number); False for an unknown name the options let pass.', [string(AEnum.Name)]);
    Add('function %sFromJSON(AValue: TRALJSON; AOptions: TRALProtoJSONOptions;', [vFunc]);
    Add('  out AResult: Integer): Boolean;');
    Add('');
    Exit;
  end;
  Add('function %sToName(AValue: Integer): StringRAL;', [vFunc]);
  Add('begin');
  Add('  case AValue of');
  for vInt := 0 to AEnum.Count - 1 do
  begin
    // an alias (allow_alias) answers with the first name
    vDuplicate := False;
    for vPrior := 0 to vInt - 1 do
      if AEnum.Values[vPrior].Number = AEnum.Values[vInt].Number then
        vDuplicate := True;
    if not vDuplicate then
      Add('    %d: Result := ''%s'';', [AEnum.Values[vInt].Number, string(AEnum.Values[vInt].Name)]);
  end;
  Add('  else');
  Add('    Result := '''';');
  Add('  end;');
  Add('end;');
  Add('');
  Add('function %sFromName(const AName: StringRAL; out AValue: Integer): Boolean;', [vFunc]);
  Add('begin');
  Add('  Result := True;');
  for vInt := 0 to AEnum.Count - 1 do
  begin
    if vInt = 0 then
      Add('  if AName = ''%s'' then', [string(AEnum.Values[vInt].Name)])
    else
      Add('  else if AName = ''%s'' then', [string(AEnum.Values[vInt].Name)]);
    Add('    AValue := %d', [AEnum.Values[vInt].Number]);
  end;
  Add('  else');
  Add('  begin');
  Add('    AValue := 0;');
  Add('    Result := False;');
  Add('  end;');
  Add('end;');
  Add('');
  Add('function %sFromJSON(AValue: TRALJSON; AOptions: TRALProtoJSONOptions;', [vFunc]);
  Add('  out AResult: Integer): Boolean;');
  Add('begin');
  Add('  Result := True;');
  Add('  if (AValue <> nil) and (AValue.Kind = rjkString) then');
  Add('  begin');
  Add('    Result := %sFromName(TRALJSONString(AValue).Value, AResult);', [vFunc]);
  Add('    if not Result and not (rpjIgnoreUnknown in AOptions) then');
  Add('      raise ERALException.Create(''unknown %s name '' + string(TRALJSONString(AValue).Value));', [string(AEnum.Name)]);
  Add('  end');
  if AEnum.FullName = 'google.protobuf.NullValue' then
  begin
    Add('  else if (AValue <> nil) and (AValue.Kind = rjkNull) then');
    Add('    AResult := 0');
  end;
  Add('  else');
  Add('    AResult := RALProtoReadInt32(AValue);');
  Add('end;');
  Add('');
end;

procedure TRALProtoWriterGen.WriteGrpcImplementation;
var
  vInt, vMethod: IntegerRAL;
  vService: TRALProtoServiceDef;
  vMeth: TRALProtoMethodDef;
  vClient, vBase, vPath: string;
begin
  for vInt := 0 to FFile.ServiceCount - 1 do
  begin
    vService := FFile.Services[vInt];
    vClient := 'T' + string(vService.Name) + 'Client';
    vBase := 'T' + string(vService.Name) + 'ServiceBase';
    Add('{ %s }', [vClient]);
    Add('');
    for vMethod := 0 to vService.Count - 1 do
    begin
      vMeth := vService.Methods[vMethod];
      vPath := '/' + string(vService.FullName) + '/' + string(vMeth.Name);
      if not vMeth.ClientStreaming and not vMeth.ServerStreaming then
      begin
        Add('function %s.%s(ARequest: %s): %s;', [vClient, RpcName(vMeth.Name),
          RALProtoClassName(vMeth.InputType), RALProtoClassName(vMeth.OutputType)]);
        Add('begin');
        Add('  Result := %s(CallUnary(''%s'', ARequest, %s));', [RALProtoClassName(vMeth.OutputType),
          vPath, RALProtoClassName(vMeth.OutputType)]);
        Add('end;');
      end
      else if not vMeth.ClientStreaming then
      begin
        Add('procedure %s.%s(ARequest: %s; AOnMessage: TRALGrpcMessageEvent);', [vClient,
          RpcName(vMeth.Name), RALProtoClassName(vMeth.InputType)]);
        Add('begin');
        Add('  CallServerStream(''%s'', ARequest, %s, AOnMessage);', [vPath,
          RALProtoClassName(vMeth.OutputType)]);
        Add('end;');
      end
      else if not vMeth.ServerStreaming then
      begin
        Add('function %s.%s(ARequests: TRALProtoMessageList): %s;', [vClient,
          RpcName(vMeth.Name), RALProtoClassName(vMeth.OutputType)]);
        Add('begin');
        Add('  Result := %s(CallClientStream(''%s'', ARequests, %s));', [RALProtoClassName(vMeth.OutputType),
          vPath, RALProtoClassName(vMeth.OutputType)]);
        Add('end;');
      end
      else
      begin
        Add('procedure %s.%s(ARequests: TRALProtoMessageList; AOnMessage: TRALGrpcMessageEvent);',
          [vClient, RpcName(vMeth.Name)]);
        Add('begin');
        Add('  CallBidiStream(''%s'', ARequests, %s, AOnMessage);', [vPath,
          RALProtoClassName(vMeth.OutputType)]);
        Add('end;');
      end;
      Add('');
    end;
    Add('{ %s }', [vBase]);
    Add('');
    Add('procedure %s.Execute(ACall: TRALGrpcServerCall);', [vBase]);
    Add('var');
    Add('  vRequest: TRALProtoMessage;');
    Add('  vReply: TRALProtoMessage;');
    Add('begin');
    Add('  vRequest := nil;');
    Add('  vReply := nil;');
    Add('  try');
    for vMethod := 0 to vService.Count - 1 do
    begin
      vMeth := vService.Methods[vMethod];
      if vMethod = 0 then
        Add('    if ACall.Method = ''%s'' then', [string(vMeth.Name)])
      else
        Add('    else if ACall.Method = ''%s'' then', [string(vMeth.Name)]);
      Add('    begin');
      if not vMeth.ClientStreaming then
      begin
        Add('      vRequest := ACall.ReadRequest(%s);', [RALProtoClassName(vMeth.InputType)]);
        if not vMeth.ServerStreaming then
        begin
          Add('      vReply := %s(%s(vRequest), ACall);', [RpcName(vMeth.Name),
            RALProtoClassName(vMeth.InputType)]);
          Add('      ACall.SendReply(vReply);');
        end
        else
          Add('      %s(%s(vRequest), ACall);', [RpcName(vMeth.Name),
            RALProtoClassName(vMeth.InputType)]);
      end
      else
      begin
        Add('      ACall.RequestClass := %s;', [RALProtoClassName(vMeth.InputType)]);
        if not vMeth.ServerStreaming then
        begin
          Add('      vReply := %s(ACall);', [RpcName(vMeth.Name)]);
          Add('      ACall.SendReply(vReply);');
        end
        else
          Add('      %s(ACall);', [RpcName(vMeth.Name)]);
      end;
      Add('    end');
    end;
    Add('    else');
    Add('      ACall.Fail(RAL_GRPC_UNIMPLEMENTED, ''unknown method '' + ACall.Method);');
    Add('  finally');
    Add('    vRequest.Free;');
    Add('    vReply.Free;');
    Add('  end;');
    Add('end;');
    Add('');
    Add('function %s.MethodKind(const AMethod: StringRAL): TRALGrpcMethodKind;', [vBase]);
    Add('begin');
    for vMethod := 0 to vService.Count - 1 do
    begin
      vMeth := vService.Methods[vMethod];
      if vMethod = 0 then
        Add('  if AMethod = ''%s'' then', [string(vMeth.Name)])
      else
        Add('  else if AMethod = ''%s'' then', [string(vMeth.Name)]);
      if vMeth.ClientStreaming and vMeth.ServerStreaming then
        Add('    Result := gmkBidiStream')
      else if vMeth.ClientStreaming then
        Add('    Result := gmkClientStream')
      else if vMeth.ServerStreaming then
        Add('    Result := gmkServerStream')
      else
        Add('    Result := gmkUnary');
    end;
    Add('  else');
    Add('    Result := gmkUnknown;');
    Add('end;');
    Add('');
    Add('function %s.ServiceName: StringRAL;', [vBase]);
    Add('begin');
    Add('  Result := ''%s'';', [string(vService.FullName)]);
    Add('end;');
    Add('');
  end;
end;

procedure TRALProtoWriterGen.WriteGrpcInterface;
var
  vInt, vMethod: IntegerRAL;
  vService: TRALProtoServiceDef;
  vMeth: TRALProtoMethodDef;
begin
  if FFile.ServiceCount = 0 then
    Exit;
  Add('type');
  for vInt := 0 to FFile.ServiceCount - 1 do
  begin
    vService := FFile.Services[vInt];
    WriteComment('  ', string(vService.Comment));
    Add('  /// A client of the gRPC service %s.', [string(vService.FullName)]);
    Add('  T%sClient = class(TRALGrpcClient)', [string(vService.Name)]);
    Add('  public');
    for vMethod := 0 to vService.Count - 1 do
    begin
      vMeth := vService.Methods[vMethod];
      WriteComment('    ', string(vMeth.Comment));
      if not vMeth.ClientStreaming and not vMeth.ServerStreaming then
      begin
        Add('    /// Calls %s; the caller frees the reply.', [string(vMeth.Name)]);
        Add('    function %s(ARequest: %s): %s;', [RpcName(vMeth.Name),
          RALProtoClassName(vMeth.InputType), RALProtoClassName(vMeth.OutputType)]);
      end
      else if not vMeth.ClientStreaming then
      begin
        Add('    /// Calls %s; each reply goes to AOnMessage, which may cancel.', [string(vMeth.Name)]);
        Add('    procedure %s(ARequest: %s; AOnMessage: TRALGrpcMessageEvent);',
          [RpcName(vMeth.Name), RALProtoClassName(vMeth.InputType)]);
      end
      else if not vMeth.ServerStreaming then
      begin
        Add('    /// Calls %s with the messages of ARequests; the caller frees the reply.', [string(vMeth.Name)]);
        Add('    function %s(ARequests: TRALProtoMessageList): %s;', [RpcName(vMeth.Name),
          RALProtoClassName(vMeth.OutputType)]);
      end
      else
      begin
        Add('    /// Calls %s: sends ARequests, each reply goes to AOnMessage.', [string(vMeth.Name)]);
        Add('    procedure %s(ARequests: TRALProtoMessageList; AOnMessage: TRALGrpcMessageEvent);',
          [RpcName(vMeth.Name)]);
      end;
    end;
    Add('  end;');
    Add('');
    Add('  { The server side of %s: override the methods and link the', [string(vService.FullName)]);
    Add('    service to a TRALGrpcModule (Module.AddService). }');
    Add('  T%sServiceBase = class(TRALGrpcService)', [string(vService.Name)]);
    Add('  protected');
    for vMethod := 0 to vService.Count - 1 do
    begin
      vMeth := vService.Methods[vMethod];
      WriteComment('    ', string(vMeth.Comment));
      if not vMeth.ClientStreaming and not vMeth.ServerStreaming then
      begin
        Add('    /// Answers %s: return a new reply (the service frees it), or raise.', [string(vMeth.Name)]);
        Add('    function %s(ARequest: %s; ACall: TRALGrpcServerCall): %s; virtual; abstract;',
          [RpcName(vMeth.Name), RALProtoClassName(vMeth.InputType), RALProtoClassName(vMeth.OutputType)]);
      end
      else if not vMeth.ClientStreaming then
      begin
        Add('    /// Answers %s: send each reply with ACall.Send.', [string(vMeth.Name)]);
        Add('    procedure %s(ARequest: %s; ACall: TRALGrpcServerCall); virtual; abstract;',
          [RpcName(vMeth.Name), RALProtoClassName(vMeth.InputType)]);
      end
      else if not vMeth.ServerStreaming then
      begin
        Add('    /// Answers %s: read the requests with ACall.Receive, return a new reply.', [string(vMeth.Name)]);
        Add('    function %s(ACall: TRALGrpcServerCall): %s; virtual; abstract;',
          [RpcName(vMeth.Name), RALProtoClassName(vMeth.OutputType)]);
      end
      else
      begin
        Add('    /// Answers %s: ACall.Receive reads, ACall.Send answers.', [string(vMeth.Name)]);
        Add('    procedure %s(ACall: TRALGrpcServerCall); virtual; abstract;', [RpcName(vMeth.Name)]);
      end;
    end;
    Add('  public');
    Add('    /// Runs the method of ACall.');
    Add('    procedure Execute(ACall: TRALGrpcServerCall); override;');
    Add('    /// The kind of a method, gmkUnknown for none.');
    Add('    function MethodKind(const AMethod: StringRAL): TRALGrpcMethodKind; override;');
    Add('    /// The full name of the service, as gRPC paths carry it.');
    Add('    function ServiceName: StringRAL; override;');
    Add('  end;');
    Add('');
  end;
end;

procedure TRALProtoWriterGen.WriteJSONRead(AMessage: TRALProtoMessageDef);
var
  vInt: IntegerRAL;
  vField, vKey, vValue: TRALProtoFieldDef;
  vName, vClass, vCond, vKeyRead, vValueClass: string;
  vNeedArray, vNeedObject, vNeedEnum, vFirst: Boolean;
begin
  vClass := string(RALProtoClassName(AMessage));
  vNeedArray := False;
  vNeedObject := False;
  vNeedEnum := False;
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    if vField.IsMap then
      vNeedObject := True
    else if (vField.FieldLabel = rflRepeated) and (vField.FieldType <> rftMessage) then
      vNeedArray := True;
    if (vField.FieldType = rftEnum) or
       (vField.IsMap and (vField.MapEntry.Fields[1].FieldType = rftEnum)) then
      vNeedEnum := True;
  end;
  Add('function %s.ReadJSONField(const AName: StringRAL; AValue: TRALJSON;', [vClass]);
  Add('  AOptions: TRALProtoJSONOptions): Boolean;');
  if vNeedArray or vNeedObject or vNeedEnum then
  begin
    Add('var');
    if vNeedArray or vNeedObject then
      Add('  vInt: IntegerRAL;');
    if vNeedArray then
      Add('  vArray: TRALJSONArray;');
    if vNeedObject then
      Add('  vObject: TRALJSONObject;');
    if vNeedEnum then
      Add('  vEnum: Integer;');
  end;
  Add('begin');
  if AMessage.FieldCount = 0 then
  begin
    Add('  Result := False;');
    Add('end;');
    Add('');
    Exit;
  end;
  Add('  Result := True;');
  vFirst := True;
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    vName := FieldName(AMessage, vField);
    if vField.JSONName <> vField.Name then
      vCond := Format('(AName = ''%s'') or (AName = ''%s'')', [string(vField.JSONName), string(vField.Name)])
    else
      vCond := Format('AName = ''%s''', [string(vField.Name)]);
    if vFirst then
      Add('  if %s then', [vCond])
    else
      Add('  else if %s then', [vCond]);
    vFirst := False;
    Add('  begin');
    if not TakesNull(vField) then
    begin
      Add('    if AValue.Kind = rjkNull then');
      Add('      Exit;');
    end;
    if vField.IsMap then
    begin
      vKey := vField.MapEntry.Fields[0];
      vValue := vField.MapEntry.Fields[1];
      case vKey.FieldType of
        rftString: vKeyRead := 'vObject.Names[vInt]';
        rftBool: vKeyRead := 'RALProtoKeyToBool(vObject.Names[vInt])';
        rftInt64, rftSInt64, rftSFixed64: vKeyRead := 'RALProtoKeyToInt64(vObject.Names[vInt])';
        rftUInt64, rftFixed64: vKeyRead := 'RALProtoKeyToUInt64(vObject.Names[vInt])';
        rftUInt32, rftFixed32: vKeyRead := 'RALProtoKeyToUInt32(vObject.Names[vInt])';
      else
        vKeyRead := 'RALProtoKeyToInt32(vObject.Names[vInt])';
      end;
      Add('    vObject := RALProtoObjectOf(AValue);');
      Add('    for vInt := 0 to vObject.Count - 1 do');
      if vValue.FieldType = rftMessage then
      begin
        vValueClass := string(RALProtoClassName(vValue.MessageType));
        Add('    begin');
        Add('      with F%s.Put(%s) do', [vName, vKeyRead]);
        Add('      begin');
        Add('        Value := %s.Create;', [vValueClass]);
        Add('        Value.ReadJSONValue(vObject.Items[vInt], AOptions);');
        Add('      end;');
        Add('    end;');
      end
      else if vValue.FieldType = rftEnum then
      begin
        Add('      if %sFromJSON(vObject.Items[vInt], AOptions, vEnum) then', [EnumFuncName(vValue.EnumType)]);
        Add('        F%s.Put(%s).Value := vEnum;', [vName, vKeyRead]);
      end
      else
        Add('      F%s.Put(%s).Value := %s;', [vName, vKeyRead, ReadOf(vValue, 'vObject.Items[vInt]')]);
    end
    else if vField.FieldLabel = rflRepeated then
    begin
      if vField.FieldType = rftMessage then
        Add('    F%s.ReadJSON(AValue, AOptions);', [vName])
      else
      begin
        Add('    vArray := RALProtoArrayOf(AValue);');
        Add('    for vInt := 0 to vArray.Count - 1 do');
        if vField.FieldType = rftEnum then
        begin
          Add('      if %sFromJSON(vArray.Items[vInt], AOptions, vEnum) then', [EnumFuncName(vField.EnumType)]);
          Add('        RALProtoAddInt32(F%s, vEnum);', [vName]);
        end
        else
          Add('      RALProtoAdd%s(F%s, %s);', [RepeatedMethod(vField), vName, ReadOf(vField, 'vArray.Items[vInt]')]);
      end;
    end
    else if vField.FieldType = rftMessage then
    begin
      Add('    if F%s = nil then', [vName]);
      Add('      Set%s(%s.Create);', [vName, ValueType(vField)]);
      Add('    F%s.ReadJSONValue(AValue, AOptions);', [vName]);
    end
    else if vField.FieldType = rftEnum then
    begin
      Add('    if %sFromJSON(AValue, AOptions, vEnum) then', [EnumFuncName(vField.EnumType)]);
      if (vField.OneofIndex >= 0) or HasFlag(vField) then
        Add('      Set%s(vEnum);', [vName])
      else
        Add('      F%s := vEnum;', [vName]);
    end
    else if (vField.OneofIndex >= 0) or HasFlag(vField) then
      Add('    Set%s(%s);', [vName, ReadOf(vField, 'AValue')])
    else
      Add('    F%s := %s;', [vName, ReadOf(vField, 'AValue')]);
    Add('  end');
  end;
  if vFirst then
    Add('  Result := False;')
  else
  begin
    Add('  else');
    Add('    Result := False;');
  end;
  Add('end;');
  Add('');
end;

procedure TRALProtoWriterGen.WriteJSONWrite(AMessage: TRALProtoMessageDef);
var
  vInt: IntegerRAL;
  vField, vKey, vValue: TRALProtoFieldDef;
  vName, vClass, vCase, vKeyText, vKeyExpr: string;
  vNeedArray, vNeedMap: Boolean;
begin
  vClass := string(RALProtoClassName(AMessage));
  vNeedArray := False;
  vNeedMap := False;
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    if vField.IsMap then
      vNeedMap := True
    else if (vField.FieldLabel = rflRepeated) and (vField.FieldType <> rftMessage) then
      vNeedArray := True;
  end;
  Add('procedure %s.WriteJSONFields(AObject: TRALJSONObject;', [vClass]);
  Add('  AOptions: TRALProtoJSONOptions);');
  if vNeedArray or vNeedMap then
  begin
    Add('var');
    Add('  vInt: IntegerRAL;');
    if vNeedArray then
      Add('  vArray: TRALJSONArray;');
    if vNeedMap then
      Add('  vMap: TRALJSONObject;');
  end;
  Add('begin');
  for vInt := 0 to AMessage.FieldCount - 1 do
  begin
    vField := AMessage.Fields[vInt];
    vName := FieldName(AMessage, vField);
    vKeyExpr := Format('RALProtoKey(AOptions, ''%s'', ''%s'')', [string(vField.JSONName),
      string(vField.Name)]);
    if vField.IsMap then
    begin
      vKey := vField.MapEntry.Fields[0];
      vValue := vField.MapEntry.Fields[1];
      case vKey.FieldType of
        rftString: vKeyText := 'F%s[vInt].Key';
        rftBool: vKeyText := 'StringRAL(LowerCase(BoolToStr(F%s[vInt].Key, True)))';
        rftUInt64, rftFixed64: vKeyText := 'RALProtoUInt64Text(F%s[vInt].Key)';
        rftUInt32, rftFixed32: vKeyText := 'StringRAL(IntToStr(Int64(F%s[vInt].Key)))';
      else
        vKeyText := 'StringRAL(IntToStr(F%s[vInt].Key))';
      end;
      vKeyText := Format(vKeyText, [vName]);
      Add('  if (F%s.Count > 0) or (rpjEmitDefaults in AOptions) then', [vName]);
      Add('  begin');
      Add('    vMap := TRALJSONObject.Create;');
      Add('    AObject.Add(%s, vMap);', [vKeyExpr]);
      Add('    for vInt := 0 to F%s.Count - 1 do', [vName]);
      if vValue.FieldType = rftMessage then
      begin
        Add('      if F%s[vInt].Value <> nil then', [vName]);
        Add('        vMap.Add(%s, F%s[vInt].Value.ToJSONValue(AOptions))', [vKeyText, vName]);
        Add('      else');
        Add('        vMap.Add(%s, TRALJSONObject.Create);', [vKeyText]);
      end
      else
        Add('      vMap.Add(%s, %s);', [vKeyText, JSONOf(vValue, Format('F%s[vInt].Value', [vName]))]);
      Add('  end;');
    end
    else if vField.FieldLabel = rflRepeated then
    begin
      if vField.FieldType = rftMessage then
      begin
        Add('  if (F%s.Count > 0) or (rpjEmitDefaults in AOptions) then', [vName]);
        Add('    AObject.Add(%s, F%s.ToJSONValue(AOptions));', [vKeyExpr, vName]);
      end
      else
      begin
        Add('  if (Length(F%s) > 0) or (rpjEmitDefaults in AOptions) then', [vName]);
        Add('  begin');
        Add('    vArray := TRALJSONArray.Create;');
        Add('    AObject.Add(%s, vArray);', [vKeyExpr]);
        Add('    for vInt := 0 to High(F%s) do', [vName]);
        Add('      vArray.Add(%s);', [JSONOf(vField, Format('F%s[vInt]', [vName]))]);
        Add('  end;');
      end;
    end
    else if vField.OneofIndex >= 0 then
    begin
      vCase := PascalCase(StringRAL(AMessage.Oneofs[vField.OneofIndex]));
      Add('  if F%sCase = %s_%s_%s then', [vCase, Copy(vClass, 2, MaxInt), vCase, vName]);
      Add('    AObject.Add(%s, %s);', [vKeyExpr, JSONOf(vField, 'F' + vName)]);
    end
    else if vField.FieldType = rftMessage then
    begin
      Add('  if F%s <> nil then', [vName]);
      Add('    AObject.Add(%s, %s);', [vKeyExpr, JSONOf(vField, 'F' + vName)]);
    end
    else if HasFlag(vField) then
    begin
      Add('  if FHas%s then', [vName]);
      Add('    AObject.Add(%s, %s);', [vKeyExpr, JSONOf(vField, 'F' + vName)]);
    end
    else
    begin
      Add('  if (%s) or (rpjEmitDefaults in AOptions) then', [NotDefault(vField, 'F' + vName)]);
      Add('    AObject.Add(%s, %s);', [vKeyExpr, JSONOf(vField, 'F' + vName)]);
    end;
  end;
  Add('end;');
  Add('');
end;

procedure TRALProtoWriterGen.WriteListClass(AMessage: TRALProtoMessageDef;
  AInterface: Boolean);
var
  vClass, vList, vKeyType: string;
  vKey: TRALProtoFieldDef;
begin
  vClass := string(RALProtoClassName(AMessage));
  vList := ListClassName(AMessage);
  vKey := nil;
  vKeyType := '';
  if AMessage.IsMapEntry then
  begin
    vKey := AMessage.Fields[0];
    vKeyType := ValueType(vKey);
  end;
  if AInterface then
  begin
    Add('  /// A repeated %s field.', [string(AMessage.Name)]);
    Add('  %s = class(TRALProtoMessageList)', [vList]);
    Add('  private');
    Add('    function GetItem(AIndex: IntegerRAL): %s;', [vClass]);
    Add('  public');
    Add('    /// Creates an empty list.');
    Add('    constructor Create;');
    Add('    /// Adds a new empty item and returns it.');
    Add('    function Add: %s;', [vClass]);
    if vKey <> nil then
    begin
      Add('    /// Removes an earlier entry with the key of the last one (the last wins).');
      Add('    procedure DropDuplicateKey;');
      Add('    /// The entry of AKey, nil when there is none.');
      if vKeyType = 'StringRAL' then
        Add('    function Find(const AKey: %s): %s;', [vKeyType, vClass])
      else
        Add('    function Find(AKey: %s): %s;', [vKeyType, vClass]);
      Add('    /// The entry of AKey, added when there is none.');
      if vKeyType = 'StringRAL' then
        Add('    function Put(const AKey: %s): %s;', [vKeyType, vClass])
      else
        Add('    function Put(AKey: %s): %s;', [vKeyType, vClass]);
    end;
    Add('    /// The items.');
    Add('    property Items[AIndex: IntegerRAL]: %s read GetItem; default;', [vClass]);
    Add('  end;');
    Add('');
    Exit;
  end;
  Add('{ %s }', [vList]);
  Add('');
  Add('constructor %s.Create;', [vList]);
  Add('begin');
  Add('  inherited Create(%s);', [vClass]);
  Add('end;');
  Add('');
  Add('function %s.Add: %s;', [vList, vClass]);
  Add('begin');
  Add('  Result := %s(AddNew);', [vClass]);
  Add('end;');
  Add('');
  Add('function %s.GetItem(AIndex: IntegerRAL): %s;', [vList, vClass]);
  Add('begin');
  Add('  Result := %s(Messages[AIndex]);', [vClass]);
  Add('end;');
  Add('');
  if vKey = nil then
    Exit;
  Add('procedure %s.DropDuplicateKey;', [vList]);
  Add('var');
  Add('  vInt: IntegerRAL;');
  Add('begin');
  Add('  for vInt := Count - 2 downto 0 do');
  Add('    if Items[vInt].Key = Items[Count - 1].Key then');
  Add('      Delete(vInt);');
  Add('end;');
  Add('');
  if vKeyType = 'StringRAL' then
    Add('function %s.Find(const AKey: %s): %s;', [vList, vKeyType, vClass])
  else
    Add('function %s.Find(AKey: %s): %s;', [vList, vKeyType, vClass]);
  Add('var');
  Add('  vInt: IntegerRAL;');
  Add('begin');
  Add('  for vInt := 0 to Count - 1 do');
  Add('    if Items[vInt].Key = AKey then');
  Add('    begin');
  Add('      Result := Items[vInt];');
  Add('      Exit;');
  Add('    end;');
  Add('  Result := nil;');
  Add('end;');
  Add('');
  if vKeyType = 'StringRAL' then
    Add('function %s.Put(const AKey: %s): %s;', [vList, vKeyType, vClass])
  else
    Add('function %s.Put(AKey: %s): %s;', [vList, vKeyType, vClass]);
  Add('begin');
  Add('  Result := Find(AKey);');
  Add('  if Result = nil then');
  Add('  begin');
  Add('    Result := Add;');
  Add('    Result.Key := AKey;');
  Add('  end;');
  Add('end;');
  Add('');
end;

end.
