/// Storage of datasets in XML: RAL's own format, plain XML, the MIDAS datapacket and ADO.
unit RALStorageXML;

{$I ../../base/PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, DateUtils, FMTBcd, Math,
  RALTypes, RALConsts, RALStorage, RALStorageJSON, RALTools, RALBase64, RALStream,
  RALMIMETypes, RALDBTypes, RALDBBase, RALXMLReader, RALXMLWriter;

type
  { Format of the XML: xtAuto lets a DBWare answer follow the client's dataset and is
    RAL's own format anywhere else; xtDBWare is RAL's own, with the schema; xtRAW
    plain elements for third parties; xtDataPacket the TClientDataSet and TBufDataset
    XML; xtADO the XML of ADO and of the Devart DACs. }
  TRALXMLType = (xtAuto, xtDBWare, xtRAW, xtDataPacket, xtADO);

  /// Dataset that reads a DBWare answer on the client.
  TRALXMLClientReader = (xcrUnknown, xcrFireDAC, xcrZeos, xcrBufDataset);

  { Datapacket as TClientDataSet writes it (fixed formats) or as TBufDataset writes
    it (the values in the locale of the machine). }
  TRALDataPacketDialect = (dpdMidas, dpdFPC);

  /// How the XML storage writes values and names.
  TRALXMLFormatOptions = class(TPersistent)
  private
    FDataPacketDialect: TRALDataPacketDialect;
    FDateTimeIsUTC: boolean;
    FIndent: boolean;
    FRecordName: StringRAL;
    FRootName: StringRAL;
  protected
    procedure AssignTo(ADest: TPersistent); override;
  public
    constructor Create;
  published
    /// Dialect of the datapacket written; both are read.
    property DataPacketDialect: TRALDataPacketDialect read FDataPacketDialect
      write FDataPacketDialect default dpdMidas;
    { The date values are UTC and are written with Z; False writes local time with
      its offset. }
    property DateTimeIsUTC: boolean read FDateTimeIsUTC write FDateTimeIsUTC default False;
    /// Puts each element on a line of its own.
    property Indent: boolean read FIndent write FIndent default False;
    /// Element of each record in the RAW format.
    property RecordName: StringRAL read FRecordName write FRecordName;
    /// Root element of the RAW format.
    property RootName: StringRAL read FRootName write FRootName;
  end;

  /// Reads and writes a dataset in one of the XML formats; reading knows them all.
  TRALStorageXML = class(TRALStorage)
  private
    /// Type of each field def, kept while the defs are read.
    FFieldDefTypes: array of TFieldType;
    FFormatOptions: TRALXMLFormatOptions;
    FXMLType: TRALXMLType;

    /// AValue in the date format of the storage, for a field of AType.
    function DateText(AValue: TDateTime; AType: TFieldType): StringRAL;
    /// Text of AField in the storage, for a field of the RAL type AType.
    function FieldText(AField: TField; AType: TRALFieldType): StringRAL;
    /// Loads the ADO rowset AReader is reading.
    procedure LoadADO(ADataset: TDataSet; AReader: TRALXMLReader);
    /// Loads the MIDAS or TBufDataset datapacket AReader is reading.
    procedure LoadDataPacket(ADataset: TDataSet; AReader: TRALXMLReader);
    /// Loads the DBWare document AReader is reading.
    procedure LoadDBWare(ADataset: TDataSet; AReader: TRALXMLReader);
    /// Loads the RAW document AReader is reading, guessing the types.
    procedure LoadRAW(ADataset: TDataSet; AReader: TRALXMLReader);
    /// Matches the fields of ADataset to the defs read.
    procedure MapFields(ADataset: TDataSet);
    /// Creates the defs read in ADataset and opens it.
    procedure OpenDefs(ADataset: TDataSet);
    /// Writes ADataset as an ADO rowset.
    procedure SaveADO(ADataset: TDataSet; AWriter: TRALXMLWriter);
    /// Writes ADataset as a datapacket of DataPacketDialect.
    procedure SaveDataPacket(ADataset: TDataSet; AWriter: TRALXMLWriter);
    /// Writes ADataset as a DBWare document.
    procedure SaveDBWare(ADataset: TDataSet; AWriter: TRALXMLWriter);
    /// Writes ADataset as a RAW document.
    procedure SaveRAW(ADataset: TDataSet; AWriter: TRALXMLWriter);
    /// Sets the field AIndex of the record being loaded from AText.
    procedure SetFieldText(AIndex: IntegerRAL; const AText: StringRAL);
    /// Writes the records of ADataset in AFormat.
    procedure WalkRecords(ADataset: TDataSet; AWriter: TRALXMLWriter; AFormat: TRALXMLType);
  public
    constructor Create;
    destructor Destroy; override;

    /// Reads ADataset from the XML in AStream, in any of the formats.
    procedure LoadFromStream(ADataset: TDataSet; AStream: TStream); override;
    /// Writes ADataset into AStream in XMLType (RAL's own for xtAuto).
    procedure SaveToStream(ADataset: TDataSet; AStream: TStream); override;
  published
    /// How values and names are written.
    property FormatOptions: TRALXMLFormatOptions read FFormatOptions;
    /// Format written.
    property XMLType: TRALXMLType read FXMLType write FXMLType;
  end;

  /// Storage link of the XML formats.
  TRALStorageXMLLink = class(TRALStorageLink)
  private
    FClientReader: TRALXMLClientReader;
    FFormatOptions: TRALXMLFormatOptions;
    FXMLType: TRALXMLType;

    /// XMLType, with xtAuto resolved from ClientReader.
    function EffectiveType: TRALXMLType;
    procedure SetFormatOptions(AValue: TRALXMLFormatOptions);
  protected
    function GetContentType: StringRAL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { With xtAuto, a FireDAC client of a FireDAC server and a TBufDataset of a sqldb
      one get the native format. }
    function AllowNative(AServerDriver, AClientDriver: Byte): boolean; override;
    function Clone: TRALStorageLink; override;
    /// Returns ADataSet as XML text.
    function DataSetToXML(ADataSet: TDataSet): StringRAL;
    function GetStorage: TRALStorage; override;
    procedure LoadPropsFromStream(AWriter: TRALBinaryWriter); override;
    /// The native XML of the server's driver.
    function NativeContentType: StringRAL; override;
    /// Takes note of the dataset that reads the answer, to pick the format.
    procedure PrepareForDataSet(ADataSet: TDataSet); override;
    procedure SavePropsToStream(AWriter: TRALBinaryWriter); override;
    /// Loads ADataSet from the XML text AXML.
    procedure XMLToDataSet(ADataSet: TDataSet; const AXML: StringRAL);

    /// Dataset of the client, sent with each DBWare request.
    property ClientReader: TRALXMLClientReader read FClientReader write FClientReader;
  published
    /// How values and names are written.
    property FormatOptions: TRALXMLFormatOptions read FFormatOptions write SetFormatOptions;
    /// Format written; xtAuto follows the client's dataset in DBWare.
    property XMLType: TRALXMLType read FXMLType write FXMLType default xtAuto;
  end;

  {$IF Defined(FPC) or Defined(Delphi2005UP)}
  { XML for any dataset, together with ToJSON: it extends the JSON helper, since only
    one helper of a class is seen at a time. }
  TRALStorageXMLHelper = class helper(TRALStorageJSONHelper) for TDataSet
  public
    /// Loads the dataset from AXML, in any of the XML formats.
    procedure LoadFromXML(const AXML: StringRAL);
    /// Returns the dataset as XML of AXMLType.
    function ToXML(AXMLType: TRALXMLType = xtDBWare): StringRAL;
  end;
  {$IFEND}

/// Writes ADataSet into AStream as XML of AXMLType.
procedure RALStorageXMLSave(ADataSet: TDataSet; AStream: TStream;
  AXMLType: TRALXMLType = xtDBWare);
/// Loads ADataSet from the XML in AStream, in any of the formats.
procedure RALStorageXMLLoad(ADataSet: TDataSet; AStream: TStream);
/// True when AName is the root element of a format the storage writes.
function RALStorageXMLIsRoot(const AName: StringRAL): boolean;

implementation

const
  /// Root element of the DBWare format.
  cRootDBWare = 'ralstorage';
  /// Root element of the datapacket format.
  cRootDataPacket = 'DATAPACKET';
  /// Namespace of the ADO rowset.
  cNsRowset = 'urn:schemas-microsoft-com:rowset';
  /// Namespace of the ADO schema.
  cNsSchema = 'uuid:BDC6E3F0-6DA3-11d1-A2A3-00AA00C14882';
  /// Namespace of the ADO data types.
  cNsDataType = 'uuid:C2F41010-65B3-11d1-A29F-00AA00C14882';
  /// Namespace of the XSD instance attributes.
  cNsXSI = 'http://www.w3.org/2001/XMLSchema-instance';
  /// Lower case hexadecimal digits.
  cHexDigits: array[0..15] of AnsiChar = '0123456789abcdef';

type
  /// A column of the RAW format, guessed from its values
  TRALXMLColumn = record
    /// Name of the column.
    Name: StringRAL;
    /// Type named by an xsi:type or a type attribute, or empty.
    TypeHint: StringRAL;
  end;

  /// A value of the RAW format: Present False for a field the record did not write
  TRALXMLRawValue = record
    /// The record wrote the field.
    Present: boolean;
    /// The field is null (xsi:nil).
    IsNull: boolean;
    /// Text of the value.
    Text: StringRAL;
  end;
  /// Values of a RAW record, by column.
  TRALXMLRawRow = array of TRALXMLRawValue;

/// Value of the attribute ALocal in the namespace ANamespace of the current element
function AttrNS(AReader: TRALXMLReader; const ANamespace, ALocal: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
  vAttr: TRALXMLAttribute;
begin
  Result := '';
  for vInt := 0 to AReader.AttributeCount - 1 do
  begin
    vAttr := RALXMLReaderAttribute(AReader, vInt);
    if (vAttr.LocalName = ALocal) and (vAttr.NamespaceURI = ANamespace) then
    begin
      Result := vAttr.Value;
      Exit;
    end;
  end;
end;

/// 'true' or 'false'.
function BoolText(AValue: boolean): StringRAL;
begin
  if AValue then
    Result := 'true'
  else
    Result := 'false';
end;

/// AValue with a dot and every significant digit.
function FloatText(AValue: Double): StringRAL;
begin
  if IsNan(AValue) then
    Result := 'NaN'
  else if IsInfinite(AValue) then
  begin
    if AValue > 0 then
      Result := 'INF'
    else
      Result := '-INF';
  end
  else
    Result := StringRAL(FloatToStr(AValue, RALInvariantFormat));
end;

/// Reads a number with a dot, whatever the locale.
function TextFloat(const AText: StringRAL; out AValue: Double): boolean;
begin
  Result := True;
  if AText = 'INF' then
    AValue := Infinity
  else if AText = '-INF' then
    AValue := NegInfinity
  else if AText = 'NaN' then
    AValue := NaN
  else
    Result := RALTryStrToFloat(AText, AValue);
end;

/// A number written with the locale's separators, rewritten with "." and no thousands
function DotDecimal(const AText: StringRAL): StringRAL;
var
  vInt, vLastDot, vLastComma, vDecimal: IntegerRAL;
begin
  vLastDot := 0;
  vLastComma := 0;
  for vInt := 1 to Length(AText) do
    if AText[vInt] = '.' then
      vLastDot := vInt
    else if AText[vInt] = ',' then
      vLastComma := vInt;
  vDecimal := vLastDot;
  if vLastComma > vLastDot then
    vDecimal := vLastComma;

  Result := '';
  for vInt := 1 to Length(AText) do
    if vInt = vDecimal then
      Result := Result + '.'
    else if (AText[vInt] <> '.') and (AText[vInt] <> ',') then
      Result := Result + Copy(AText, vInt, 1);
end;

/// True for 'true', 'TRUE', 'True' and '1'.
function TextBool(const AText: StringRAL): boolean;
begin
  Result := (AText = 'true') or (AText = 'True') or (AText = 'TRUE') or (AText = '1') or
    (AText = '-1');
end;

/// The bytes of the bin.hex text AText; raises on an odd length or a bad digit.
function HexToBytes(const AText: StringRAL): TBytes;
var
  vInt: IntegerRAL;

  function Nibble(AChr: AnsiChar): Byte;
  begin
    case AChr of
      '0'..'9': Result := Ord(AChr) - Ord('0');
      'a'..'f': Result := Ord(AChr) - Ord('a') + 10;
      'A'..'F': Result := Ord(AChr) - Ord('A') + 10;
    else
      raise EConvertError.Create(Format(emXMLValueType, [string(AText), 'bin.hex']));
    end;
  end;

begin
  if Odd(Length(AText)) then
    raise EConvertError.Create(Format(emXMLValueType, [string(AText), 'bin.hex']));
  Result := nil;
  SetLength(Result, Length(AText) div 2);
  for vInt := 0 to Length(Result) - 1 do
    Result[vInt] := (Nibble(AnsiChar(AText[vInt * 2 + 1])) shl 4) or
      Nibble(AnsiChar(AText[vInt * 2 + 2]));
end;

/// ABytes in bin.hex.
function BytesToHex(const ABytes: TBytes): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  SetLength(Result, Length(ABytes) * 2);
  for vInt := 0 to Length(ABytes) - 1 do
  begin
    Result[vInt * 2 + 1] := cHexDigits[ABytes[vInt] shr 4];
    Result[vInt * 2 + 2] := cHexDigits[ABytes[vInt] and 15];
  end;
end;

/// Bytes of AField, blob or not.
function FieldBytes(AField: TField): TBytes;
var
  vStream: TMemoryStream;
begin
  if not (AField is TBlobField) then
  begin
    Result := AField.AsBytes;
    Exit;
  end;
  vStream := TMemoryStream.Create;
  try
    TBlobField(AField).SaveToStream(vStream);
    SetLength(Result, vStream.Size);
    if vStream.Size > 0 then
      Move(vStream.Memory^, Result[0], vStream.Size);
  finally
    vStream.Free;
  end;
end;

/// Sets AField from ABytes, blob or not.
procedure SetFieldBytes(AField: TField; const ABytes: TBytes);
var
  vStream: TMemoryStream;
begin
  if AField = nil then
    Exit;
  if not (AField is TBlobField) then
  begin
    AField.AsBytes := ABytes;
    Exit;
  end;
  vStream := TMemoryStream.Create;
  try
    if Length(ABytes) > 0 then
      vStream.WriteBuffer(ABytes[0], Length(ABytes));
    vStream.Position := 0;
    TBlobField(AField).LoadFromStream(vStream);
  finally
    vStream.Free;
  end;
end;

/// AValue in two digits.
function Digits2(AValue: Word): StringRAL;
begin
  Result := StringRAL(IntToStr(AValue));
  if Length(Result) < 2 then
    Result := '0' + Result;
end;

/// AValue in three digits.
function Digits3(AValue: Word): StringRAL;
begin
  Result := StringRAL(IntToStr(AValue));
  while Length(Result) < 3 do
    Result := '0' + Result;
end;

/// AValue in four digits.
function Digits4(AValue: Word): StringRAL;
begin
  Result := StringRAL(IntToStr(AValue));
  while Length(Result) < 4 do
    Result := '0' + Result;
end;

/// Dates in the forms of the datapacket: yyyymmdd, hh:nn:sszzz, yyyymmddThh:nn:sszzz
function MidasDate(AValue: TDateTime; AType: TFieldType): StringRAL;
var
  vYear, vMonth, vDay, vHour, vMin, vSec, vMSec: Word;
begin
  DecodeDateTime(AValue, vYear, vMonth, vDay, vHour, vMin, vSec, vMSec);
  case AType of
    ftDate:
      Result := Digits4(vYear) + Digits2(vMonth) + Digits2(vDay);
    ftTime:
      Result := Digits2(vHour) + ':' + Digits2(vMin) + ':' + Digits2(vSec) + Digits3(vMSec);
  else
    Result := Digits4(vYear) + Digits2(vMonth) + Digits2(vDay) + 'T' + Digits2(vHour) + ':' +
      Digits2(vMin) + ':' + Digits2(vSec) + Digits3(vMSec);
  end;
end;

/// AText is not empty and holds only digits.
function AllDigitsText(const AText: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  Result := AText <> '';
  for vInt := 1 to Length(AText) do
    if (AText[vInt] < '0') or (AText[vInt] > '9') then
    begin
      Result := False;
      Exit;
    end;
end;

/// Reads the datapacket forms, then ISO 8601, then a time alone, then the locale
function TryParseDate(const AText: StringRAL; out AValue: TDateTime): boolean;
var
  vText, vTime: StringRAL;
  vPos: IntegerRAL;
  vHour, vMin, vSec, vMSec: IntegerRAL;
  vDate: TDateTime;
begin
  Result := False;
  vText := RALTrim(AText);
  if vText = '' then
    Exit;

  // yyyymmdd[Thh:nn:sszzz]
  if (Length(vText) >= 8) and AllDigitsText(Copy(vText, 1, 8)) and
     ((Length(vText) = 8) or (vText[9] = 'T')) and
     ((Length(vText) = 8) or (Pos(StringRAL('-'), vText) = 0)) then
  begin
    if not TryEncodeDate(StrToInt(string(Copy(vText, 1, 4))), StrToInt(string(Copy(vText, 5, 2))),
      StrToInt(string(Copy(vText, 7, 2))), vDate) then
      Exit;
    AValue := vDate;
    if Length(vText) = 8 then
    begin
      Result := True;
      Exit;
    end;
    vTime := Copy(vText, 10, MaxInt);
    Result := (Length(vTime) >= 8) and TryStrToInt(string(Copy(vTime, 1, 2)), vHour) and
      TryStrToInt(string(Copy(vTime, 4, 2)), vMin) and TryStrToInt(string(Copy(vTime, 7, 2)), vSec);
    vMSec := 0;
    if Result and (Length(vTime) > 8) then
      Result := TryStrToInt(string(Copy(vTime, 9, 3)), vMSec);
    if Result then
      Result := TryEncodeTime(vHour, vMin, vSec, vMSec, vDate);
    if Result then
      AValue := AValue + vDate;
    if Result then
      Exit;
  end;

  if RALTryISO8601ToDateTime(vText, AValue) then
  begin
    Result := True;
    Exit;
  end;

  // hh:nn:ss, hh:nn:ss.zzz or the datapacket's hh:nn:sszzz
  if (Length(vText) >= 8) and (vText[3] = ':') and (vText[6] = ':') then
  begin
    vPos := 9;
    if (Length(vText) > 8) and (vText[9] = '.') then
      vPos := 10;
    vMSec := 0;
    Result := TryStrToInt(string(Copy(vText, 1, 2)), vHour) and
      TryStrToInt(string(Copy(vText, 4, 2)), vMin) and
      TryStrToInt(string(Copy(vText, 7, 2)), vSec);
    if Result and (Length(vText) >= vPos) then
      Result := TryStrToInt(string(Copy(Copy(vText, vPos, MaxInt) + '00', 1, 3)), vMSec);
    if Result then
      Result := TryEncodeTime(vHour, vMin, vSec, vMSec, AValue);
    if Result then
      Exit;
  end;

  Result := TryStrToDateTime(string(vText), AValue);
end;

/// AText is an integer: an optional sign and digits.
function IsIntegerText(const AText: StringRAL): boolean;
var
  vText: StringRAL;
begin
  vText := AText;
  if (vText <> '') and (vText[1] = '-') then
    vText := Copy(vText, 2, MaxInt);
  // a leading zero says the value is a code, not a number
  Result := AllDigitsText(vText) and (Length(vText) <= 18) and
    ((Length(vText) = 1) or (vText[1] <> '0'));
end;

/// AText is a decimal with a dot; APrecision and AScale are its digits.
function IsDecimalText(const AText: StringRAL; out APrecision, AScale: IntegerRAL): boolean;
var
  vText: StringRAL;
  vPos: IntegerRAL;
begin
  vText := AText;
  if (vText <> '') and (vText[1] = '-') then
    vText := Copy(vText, 2, MaxInt);
  vPos := Pos(StringRAL('.'), vText);
  Result := (vPos > 1) and (vPos < Length(vText)) and AllDigitsText(Copy(vText, 1, vPos - 1)) and
    AllDigitsText(Copy(vText, vPos + 1, MaxInt)) and
    ((vPos = 2) or (vText[1] <> '0'));
  APrecision := Length(vText) - 1;
  AScale := Length(vText) - vPos;
end;

{ TRALXMLFormatOptions }

constructor TRALXMLFormatOptions.Create;
begin
  inherited Create;
  FDataPacketDialect := dpdMidas;
  FRootName := 'dataset';
  FRecordName := 'record';
end;

procedure TRALXMLFormatOptions.AssignTo(ADest: TPersistent);
begin
  if ADest is TRALXMLFormatOptions then
  begin
    TRALXMLFormatOptions(ADest).FDataPacketDialect := FDataPacketDialect;
    TRALXMLFormatOptions(ADest).FDateTimeIsUTC := FDateTimeIsUTC;
    TRALXMLFormatOptions(ADest).FIndent := FIndent;
    TRALXMLFormatOptions(ADest).FRecordName := FRecordName;
    TRALXMLFormatOptions(ADest).FRootName := FRootName;
  end
  else
    inherited AssignTo(ADest);
end;

{ TRALStorageXML }

constructor TRALStorageXML.Create;
begin
  inherited Create;
  FFormatOptions := TRALXMLFormatOptions.Create;
  FXMLType := xtDBWare;
end;

destructor TRALStorageXML.Destroy;
begin
  FreeAndNil(FFormatOptions);
  inherited Destroy;
end;

function TRALStorageXML.DateText(AValue: TDateTime; AType: TFieldType): StringRAL;
var
  vYear, vMonth, vDay, vHour, vMin, vSec, vMSec: Word;
begin
  // xsd:date and xsd:time for the fields that hold only one of them
  DecodeDateTime(AValue, vYear, vMonth, vDay, vHour, vMin, vSec, vMSec);
  case AType of
    ftDate:
      Result := Digits4(vYear) + '-' + Digits2(vMonth) + '-' + Digits2(vDay);
    ftTime:
      Result := Digits2(vHour) + ':' + Digits2(vMin) + ':' + Digits2(vSec) + '.' + Digits3(vMSec);
  else
    Result := RALDateTimeToISO8601(AValue, FFormatOptions.DateTimeIsUTC);
  end;
end;

function TRALStorageXML.FieldText(AField: TField; AType: TRALFieldType): StringRAL;
begin
  case AType of
    sftShortInt, sftSmallInt, sftInteger, sftInt64, sftByte, sftWord, sftCardinal, sftQWord:
      Result := StringRAL(IntToStr(AField.AsLargeInt));
    sftDouble:
      Result := FloatText(AField.AsFloat);
    sftBCD:
      Result := RALBCDToText(AField.AsBCD);
    sftBoolean:
      Result := BoolText(AField.AsBoolean);
    sftBlob:
      Result := TRALBase64.Encode(FieldBytes(AField));
    sftDateTime:
      Result := DateText(AField.AsDateTime, AField.DataType);
  else
    Result := StringRAL(AField.AsString);
  end;
end;

procedure TRALStorageXML.SetFieldText(AIndex: IntegerRAL; const AText: StringRAL);
var
  vField: TField;
  vFloat: Double;
  vBcd: TBcd;
  vDate: TDateTime;
begin
  vField := FFoundFields[AIndex];
  if vField = nil then
    Exit;
  case FFieldTypes[AIndex] of
    sftShortInt, sftSmallInt, sftInteger, sftByte, sftWord:
      ReadFieldInteger(vField, StrToInt(string(RALTrim(AText))));
    sftInt64, sftCardinal, sftQWord:
      ReadFieldInt64(vField, StrToInt64(string(RALTrim(AText))));
    sftDouble:
      begin
        if not TextFloat(RALTrim(AText), vFloat) then
          raise EConvertError.Create(Format(emXMLValueType, [string(AText), 'double']));
        ReadFieldFloat(vField, vFloat);
      end;
    sftBCD:
      // the text of the locale (TBufDataset writes AsString) keeps every digit too
      if RALTryTextToBCD(RALTrim(AText), vBcd) or
         RALTryTextToBCD(DotDecimal(RALTrim(AText)), vBcd) then
        ReadFieldBCD(vField, vBcd)
      else if TextFloat(RALTrim(AText), vFloat) then
        ReadFieldFloat(vField, vFloat)
      else
        raise EConvertError.Create(Format(emDecimalInvalid, [string(AText)]));
    sftBoolean:
      ReadFieldBoolean(vField, TextBool(RALTrim(AText)));
    sftBlob:
      begin
        if RALTrim(AText) = '' then
          SetFieldBytes(vField, nil)
        else
          SetFieldBytes(vField, TRALBase64.DecodeAsBytes(RALTrim(AText)));
      end;
    sftDateTime:
      begin
        if not TryParseDate(AText, vDate) then
          raise EConvertError.Create(Format(emXMLValueType, [string(AText), 'dateTime']));
        ReadFieldDateTime(vField, vDate);
      end;
  else
    ReadFieldString(vField, AText);
  end;
end;

procedure TRALStorageXML.OpenDefs(ADataset: TDataSet);
begin
  ADataset.Open;
  MapFields(ADataset);
end;

procedure TRALStorageXML.MapFields(ADataset: TDataSet);
var
  vInt, vIdx: IntegerRAL;
begin
  SetLength(FFoundFields, Length(FFieldNames));
  for vIdx := 0 to High(FFieldNames) do
  begin
    FFoundFields[vIdx] := nil;
    for vInt := 0 to ADataset.FieldCount - 1 do
      if RALSameName(StringRAL(ADataset.Fields[vInt].FieldName), FFieldNames[vIdx]) then
      begin
        FFoundFields[vIdx] := ADataset.Fields[vInt];
        Break;
      end;
  end;
end;

procedure TRALStorageXML.WalkRecords(ADataset: TDataSet; AWriter: TRALXMLWriter;
  AFormat: TRALXMLType);
var
  vBookMark: TBookMark;
  vInt, vRecord: IntegerRAL;
  vField: TField;
  vText, vName: StringRAL;
  vIsWide: boolean;
begin
  ADataset.DisableControls;
  try
    vBookMark := nil;
    if not ADataset.IsUniDirectional then
    begin
      vBookMark := ADataset.GetBookmark;
      ADataset.First;
    end;
    try
      vRecord := 0;
      while not ADataset.EOF do
      begin
        Inc(vRecord);
        case AFormat of
          xtDBWare:
            begin
              AWriter.StartElement('r');
              for vInt := 0 to ADataset.FieldCount - 1 do
              begin
                vField := ADataset.Fields[vInt];
                AWriter.StartElement('v');
                if vField.IsNull then
                  AWriter.Attribute('n', '1')
                else
                begin
                  vText := FieldText(vField, FFieldTypes[vInt]);
                  // a character XML 1.0 cannot carry goes as base64 of the text
                  if not RALXMLIsValidText(vText) then
                  begin
                    AWriter.Attribute('e', 'b64');
                    vText := TRALBase64.Encode(vText);
                  end;
                  AWriter.Text(vText);
                end;
                AWriter.EndElement;
              end;
              AWriter.EndElement;
            end;
          xtRAW:
            begin
              AWriter.StartElement(FFormatOptions.RecordName);
              for vInt := 0 to ADataset.FieldCount - 1 do
              begin
                vField := ADataset.Fields[vInt];
                AWriter.StartElement(FFieldNames[vInt]);
                if vField.IsNull then
                  AWriter.Attribute('xsi:nil', 'true')
                else
                begin
                  if FFieldTypes[vInt] = sftBlob then
                    AWriter.Attribute('xsi:type', 'xs:base64Binary');
                  vText := FieldText(vField, FFieldTypes[vInt]);
                  if not RALXMLIsValidText(vText) then
                    raise ERALXMLError.CreateAt(Format(emXMLInvalidFieldText,
                      [vField.FieldName, vRecord]), 0, 0);
                  AWriter.Text(vText);
                end;
                AWriter.EndElement;
              end;
              AWriter.EndElement;
            end;
          xtDataPacket:
            begin
              AWriter.StartElement('ROW');
              for vInt := 0 to ADataset.FieldCount - 1 do
              begin
                vField := ADataset.Fields[vInt];
                if vField.IsNull then
                  Continue;
                vName := FFieldNames[vInt];
                if FFormatOptions.DataPacketDialect = dpdFPC then
                begin
                  if FFieldTypes[vInt] = sftBlob then
                    vText := TRALBase64.Encode(FieldBytes(vField))
                  else
                    vText := StringRAL(vField.AsString);
                end
                else
                  case FFieldTypes[vInt] of
                    sftBoolean:
                      if vField.AsBoolean then vText := 'TRUE' else vText := 'FALSE';
                    sftDateTime:
                      vText := MidasDate(vField.AsDateTime, vField.DataType);
                    sftBlob:
                      vText := TRALBase64.Encode(FieldBytes(vField));
                  else
                    vText := FieldText(vField, FFieldTypes[vInt]);
                  end;
                if not RALXMLIsValidText(vText) then
                  raise ERALXMLError.CreateAt(Format(emXMLInvalidFieldText,
                    [vField.FieldName, vRecord]), 0, 0);
                AWriter.Attribute(vName, vText);
              end;
              AWriter.EndElement;
            end;
          xtADO:
            begin
              AWriter.StartElement('z:row');
              for vInt := 0 to ADataset.FieldCount - 1 do
              begin
                vField := ADataset.Fields[vInt];
                if vField.IsNull then
                  Continue;
                case FFieldTypes[vInt] of
                  sftBoolean:
                    if vField.AsBoolean then vText := 'True' else vText := 'False';
                  sftBlob:
                    vText := BytesToHex(FieldBytes(vField));
                  sftDateTime:
                    vText := RALDateTimeToISO8601(vField.AsDateTime, True);
                else
                  vText := FieldText(vField, FFieldTypes[vInt]);
                end;
                if FFieldTypes[vInt] = sftDateTime then
                begin
                  // ADO writes no zone: the value as it is, the date or the time alone
                  vIsWide := vField.DataType in [ftDate, ftTime];
                  if vIsWide then
                    vText := DateText(vField.AsDateTime, vField.DataType)
                  else
                    vText := Copy(vText, 1, 23);
                end;
                if not RALXMLIsValidText(vText) then
                  raise ERALXMLError.CreateAt(Format(emXMLInvalidFieldText,
                    [vField.FieldName, vRecord]), 0, 0);
                AWriter.Attribute(FFieldNames[vInt], vText);
              end;
              AWriter.EndElement;
            end;
        end;
        ADataset.Next;
      end;
    finally
      if vBookMark <> nil then
      begin
        ADataset.GotoBookmark(vBookMark);
        ADataset.FreeBookmark(vBookMark);
      end;
    end;
  finally
    ADataset.EnableControls;
  end;
end;

procedure TRALStorageXML.SaveDBWare(ADataset: TDataSet; AWriter: TRALXMLWriter);
var
  vInt: IntegerRAL;
  vField: TField;
  vType: TRALFieldType;
begin
  AWriter.StartElement(cRootDBWare);
  AWriter.Attribute('version', StringRAL(IntToStr(GetStoreVersion)));

  SetLength(FFieldNames, ADataset.FieldCount);
  SetLength(FFieldTypes, ADataset.FieldCount);
  AWriter.StartElement('fields');
  for vInt := 0 to ADataset.FieldCount - 1 do
  begin
    vField := ADataset.Fields[vInt];
    vType := TRALDB.FieldTypeToRALFieldType(vField.DataType);
    FFieldNames[vInt] := CharCaseValue(StringRAL(vField.FieldName));
    FFieldTypes[vInt] := vType;

    AWriter.StartElement('field');
    AWriter.Attribute('name', FFieldNames[vInt]);
    AWriter.Attribute('type', StringRAL(IntToStr(Ord(vType))));
    AWriter.Attribute('flags', StringRAL(IntToStr(TRALDB.GetFieldProviderFlags(vField))));
    AWriter.Attribute('size', StringRAL(IntToStr(vField.Size)));
    if vType = sftBCD then
      AWriter.Attribute('precision', StringRAL(IntToStr(RALFieldPrecision(vField))));
    if vType = sftDateTime then
      AWriter.Attribute('kind', StringRAL(IntToStr(RALDateTimeKind(vField.DataType))));
    AWriter.EndElement;
  end;
  AWriter.EndElement;

  AWriter.StartElement('rows');
  WalkRecords(ADataset, AWriter, xtDBWare);
  AWriter.EndElement;
  AWriter.EndElement;
end;

procedure TRALStorageXML.SaveRAW(ADataset: TDataSet; AWriter: TRALXMLWriter);
var
  vInt: IntegerRAL;
begin
  AWriter.StartElement(FFormatOptions.RootName);
  AWriter.Attribute('xmlns:xsi', cNsXSI);
  AWriter.Attribute('xmlns:xs', 'http://www.w3.org/2001/XMLSchema');
  SetLength(FFieldNames, ADataset.FieldCount);
  SetLength(FFieldTypes, ADataset.FieldCount);
  for vInt := 0 to ADataset.FieldCount - 1 do
  begin
    FFieldNames[vInt] := RALXMLEncodeName(CharCaseValue(StringRAL(ADataset.Fields[vInt].FieldName)));
    FFieldTypes[vInt] := TRALDB.FieldTypeToRALFieldType(ADataset.Fields[vInt].DataType);
  end;
  WalkRecords(ADataset, AWriter, xtRAW);
  AWriter.EndElement;
end;

/// The datapacket type of a field: fieldtype, SUBTYPE, WIDTH and DECIMALS
procedure DataPacketType(AField: TField; AFPC: boolean; out AType, ASubType: StringRAL;
  out AWidth, ADecimals: IntegerRAL);
begin
  AType := 'string';
  ASubType := '';
  AWidth := 0;
  ADecimals := -1;
  case AField.DataType of
    ftString, ftFixedChar:
      AWidth := AField.Size;
    ftWideString, ftFixedWideChar:
      begin
        AType := 'string.uni';
        if AFPC then
          AWidth := AField.Size
        else
          AWidth := AField.Size * 2;
      end;
    ftGuid:
      begin
        ASubType := 'Guid';
        AWidth := 38;
      end;
    ftSmallint: AType := 'i2';
    ftInteger: AType := 'i4';
    ftWord: if AFPC then AType := 'i4' else AType := 'ui2';
    ftAutoInc:
      begin
        AType := 'i4';
        ASubType := 'Autoinc';
      end;
    ftLargeint: AType := 'i8';
    ftShortint: AType := 'i1';
    ftByte: AType := 'ui1';
    ftLongWord: AType := 'ui4';
    ftBoolean: AType := 'boolean';
    ftFloat, ftExtended: AType := 'r8';
    ftSingle: AType := 'r4';
    ftCurrency:
      begin
        AType := 'r8';
        ASubType := 'Money';
      end;
    ftBCD:
      begin
        AType := 'fixed';
        ADecimals := AField.Size;
        AWidth := RALFieldPrecision(AField);
      end;
    ftFMTBcd:
      begin
        AType := 'fixedFMT';
        ADecimals := AField.Size;
        AWidth := RALFieldPrecision(AField);
      end;
    ftDate: AType := 'date';
    ftTime: AType := 'time';
    ftDateTime, ftTimeStamp:
      if AFPC then AType := 'datetime' else AType := 'dateTime';
    ftMemo:
      begin
        AType := 'bin.hex';
        ASubType := 'Text';
      end;
    ftWideMemo:
      begin
        AType := 'bin.hex';
        ASubType := 'WideText';
      end;
    ftBytes, ftVarBytes:
      begin
        AType := 'bin.hex';
        AWidth := AField.Size;
      end;
    ftBlob, ftGraphic, ftTypedBinary, ftOraBlob:
      begin
        AType := 'bin.hex';
        ASubType := 'Binary';
      end;
  end;
end;

/// A datapacket attribute name: the field's own when it is a valid name, else with "_"
function DataPacketAttrName(const AName: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := AName;
  if RALXMLIsName(Result, True) then
    Exit;
  for vInt := 1 to Length(Result) do
    if not (AnsiChar(Result[vInt]) in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-', '.']) and
       (Byte(Result[vInt]) < $80) then
      Result[vInt] := '_';
  if (Result = '') or not RALXMLIsName(Result, True) then
    Result := '_' + Result;
end;

procedure TRALStorageXML.SaveDataPacket(ADataset: TDataSet; AWriter: TRALXMLWriter);
var
  vInt, vWidth, vDecimals: IntegerRAL;
  vField: TField;
  vType, vSubType, vName: StringRAL;
  vFPC: boolean;
begin
  vFPC := FFormatOptions.DataPacketDialect = dpdFPC;
  AWriter.StartElement(cRootDataPacket);
  AWriter.Attribute('Version', '2.0');
  AWriter.StartElement('METADATA');
  AWriter.StartElement('FIELDS');

  SetLength(FFieldNames, ADataset.FieldCount);
  SetLength(FFieldTypes, ADataset.FieldCount);
  for vInt := 0 to ADataset.FieldCount - 1 do
  begin
    vField := ADataset.Fields[vInt];
    vName := CharCaseValue(StringRAL(vField.FieldName));
    FFieldNames[vInt] := DataPacketAttrName(vName);
    FFieldTypes[vInt] := TRALDB.FieldTypeToRALFieldType(vField.DataType);
    if vField.DataType in [ftBytes, ftVarBytes] then
      FFieldTypes[vInt] := sftBlob;
    DataPacketType(vField, vFPC, vType, vSubType, vWidth, vDecimals);

    AWriter.StartElement('FIELD');
    if vFPC or (FFieldNames[vInt] <> vName) then
      AWriter.Attribute('fieldname', vName);
    AWriter.Attribute('attrname', FFieldNames[vInt]);
    AWriter.Attribute('fieldtype', vType);
    if vFPC then
    begin
      if vSubType <> '' then
        AWriter.Attribute('subtype', vSubType);
      if vWidth > 0 then
        AWriter.Attribute('width', StringRAL(IntToStr(vWidth)));
    end
    else
    begin
      if vDecimals >= 0 then
        AWriter.Attribute('DECIMALS', StringRAL(IntToStr(vDecimals)));
      if vSubType <> '' then
        AWriter.Attribute('SUBTYPE', vSubType);
      if vWidth > 0 then
        AWriter.Attribute('WIDTH', StringRAL(IntToStr(vWidth)));
    end;
    if vField.ReadOnly then
      AWriter.Attribute('readonly', 'true');
    if vField.Required then
      AWriter.Attribute('required', 'true');
    AWriter.EndElement;
  end;

  AWriter.EndElement;
  AWriter.StartElement('PARAMS');
  AWriter.EndElement;
  AWriter.EndElement;
  AWriter.StartElement('ROWDATA');
  WalkRecords(ADataset, AWriter, xtDataPacket);
  AWriter.EndElement;
  AWriter.EndElement;
end;

/// The ADO type of a field: dt:type, rs:dbtype, dt:maxLength and the long flag
procedure ADOType(AField: TField; out AType, ADBType: StringRAL; out AMaxLength: Int64RAL;
  out ALong: boolean);
begin
  AType := 'string';
  ADBType := '';
  AMaxLength := 0;
  ALong := False;
  case AField.DataType of
    ftString, ftFixedChar:
      begin
        ADBType := 'str';
        AMaxLength := AField.Size;
      end;
    ftWideString, ftFixedWideChar:
      AMaxLength := AField.Size;
    ftGuid:
      AType := 'uuid';
    ftSmallint: AType := 'i2';
    ftInteger, ftAutoInc: AType := 'int';
    ftWord: AType := 'ui2';
    ftLargeint: AType := 'i8';
    ftShortint: AType := 'i1';
    ftByte: AType := 'ui1';
    ftLongWord: AType := 'ui4';
    ftBoolean: AType := 'boolean';
    ftFloat, ftExtended: AType := 'float';
    ftSingle: AType := 'r4';
    ftCurrency:
      begin
        AType := 'number';
        ADBType := 'currency';
      end;
    ftBCD, ftFMTBcd:
      begin
        AType := 'number';
        ADBType := 'numeric';
      end;
    ftDate: AType := 'date';
    ftTime: AType := 'time';
    ftDateTime, ftTimeStamp:
      begin
        AType := 'dateTime';
        ADBType := 'timestamp';
      end;
    ftMemo:
      begin
        ADBType := 'str';
        AMaxLength := 2147483647;
        ALong := True;
      end;
    ftWideMemo:
      begin
        AMaxLength := 2147483647;
        ALong := True;
      end;
    ftBytes, ftVarBytes:
      begin
        AType := 'bin.hex';
        AMaxLength := AField.Size;
      end;
    ftBlob, ftGraphic, ftTypedBinary, ftOraBlob:
      begin
        AType := 'bin.hex';
        AMaxLength := 2147483647;
        ALong := True;
      end;
  end;
end;

procedure TRALStorageXML.SaveADO(ADataset: TDataSet; AWriter: TRALXMLWriter);
var
  vInt: IntegerRAL;
  vField: TField;
  vType, vDBType, vName: StringRAL;
  vMaxLength: Int64RAL;
  vLong: boolean;
begin
  AWriter.StartElement('xml');
  AWriter.Attribute('xmlns:s', cNsSchema);
  AWriter.Attribute('xmlns:dt', cNsDataType);
  AWriter.Attribute('xmlns:rs', cNsRowset);
  AWriter.Attribute('xmlns:z', '#RowsetSchema');
  AWriter.StartElement('s:Schema');
  AWriter.Attribute('id', 'RowsetSchema');
  AWriter.StartElement('s:ElementType');
  AWriter.Attribute('name', 'row');
  AWriter.Attribute('content', 'eltOnly');
  AWriter.Attribute('rs:updatable', 'true');

  SetLength(FFieldNames, ADataset.FieldCount);
  SetLength(FFieldTypes, ADataset.FieldCount);
  for vInt := 0 to ADataset.FieldCount - 1 do
  begin
    vField := ADataset.Fields[vInt];
    vName := CharCaseValue(StringRAL(vField.FieldName));
    FFieldTypes[vInt] := TRALDB.FieldTypeToRALFieldType(vField.DataType);
    if vField.DataType in [ftBytes, ftVarBytes] then
      FFieldTypes[vInt] := sftBlob;

    // ADO names a column that is not an XML name cN and gives its name in rs:name
    if RALXMLIsName(vName, True) then
      FFieldNames[vInt] := vName
    else
      FFieldNames[vInt] := StringRAL('c' + IntToStr(vInt));

    AWriter.StartElement('s:AttributeType');
    AWriter.Attribute('name', FFieldNames[vInt]);
    if FFieldNames[vInt] <> vName then
      AWriter.Attribute('rs:name', vName);
    AWriter.Attribute('rs:number', StringRAL(IntToStr(vInt + 1)));
    if not vField.Required then
      AWriter.Attribute('rs:nullable', 'true');
    AWriter.Attribute('rs:write', 'true');

    ADOType(vField, vType, vDBType, vMaxLength, vLong);
    AWriter.StartElement('s:datatype');
    AWriter.Attribute('dt:type', vType);
    if vDBType <> '' then
      AWriter.Attribute('rs:dbtype', vDBType);
    if vMaxLength > 0 then
      AWriter.Attribute('dt:maxLength', StringRAL(IntToStr(vMaxLength)));
    if vField.DataType in [ftBCD, ftFMTBcd] then
    begin
      AWriter.Attribute('rs:scale', StringRAL(IntToStr(vField.Size)));
      AWriter.Attribute('rs:precision', StringRAL(IntToStr(RALFieldPrecision(vField))));
    end;
    if vLong then
      AWriter.Attribute('rs:long', 'true');
    AWriter.EndElement;
    AWriter.EndElement;
  end;

  AWriter.StartElement('s:extends');
  AWriter.Attribute('type', 'rs:rowbase');
  AWriter.EndElement;
  AWriter.EndElement;
  AWriter.EndElement;

  AWriter.StartElement('rs:data');
  WalkRecords(ADataset, AWriter, xtADO);
  AWriter.EndElement;
  AWriter.EndElement;
end;

procedure TRALStorageXML.SaveToStream(ADataset: TDataSet; AStream: TStream);
var
  vWriter: TRALXMLWriter;
  vType: TRALXMLType;
begin
  vType := FXMLType;
  if vType = xtAuto then
    vType := xtDBWare;

  vWriter := TRALXMLWriter.Create(AStream, FFormatOptions.Indent);
  try
    case vType of
      xtDataPacket:
        vWriter.WriteDeclaration('yes');
      xtADO:
        ; // ADO writes no declaration
    else
      vWriter.WriteDeclaration;
    end;

    case vType of
      xtRAW:
        SaveRAW(ADataset, vWriter);
      xtDataPacket:
        SaveDataPacket(ADataset, vWriter);
      xtADO:
        SaveADO(ADataset, vWriter);
    else
      SaveDBWare(ADataset, vWriter);
    end;
  finally
    vWriter.Free;
  end;
  SetLength(FFieldNames, 0);
  SetLength(FFieldTypes, 0);
end;

{ reading }

/// Moves AReader to the next start or end of an element, skipping what is between
function NextTag(AReader: TRALXMLReader): boolean;
begin
  Result := False;
  while AReader.Read do
    if AReader.Token in [xtkStartElement, xtkEndElement] then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TRALStorageXML.LoadDBWare(ADataset: TDataSet; AReader: TRALXMLReader);
var
  vCount, vValue: IntegerRAL;
  vTypeOrd: Int64RAL;
  vName: StringRAL;
  vField: TFieldDef;
  vFlags: IntegerRAL;
  vSize, vPrecision: IntegerRAL;
  vText: StringRAL;
  vType: TRALFieldType;
begin
  if (AReader.AttributeValue('version') <> StringRAL(IntToStr(GetStoreVersion))) then
    raise ERALXMLError.CreateAt(emXMLStorageFormat, 0, 0);

  if ADataset.Active then
    ADataset.Close;
  ADataset.FieldDefs.Clear;
  vCount := 0;
  SetLength(FFieldNames, 0);
  SetLength(FFieldTypes, 0);

  while NextTag(AReader) do
  begin
    if AReader.Token = xtkEndElement then
    begin
      if AReader.Depth = 0 then
        Break;
      Continue;
    end;

    if AReader.Name = 'field' then
    begin
      vName := AReader.AttributeValue('name');
      if not TryStrToInt64(string(AReader.AttributeValue('type')), vTypeOrd) or
         not RALIsFieldTypeOrdinal(vTypeOrd) then
        raise ERALXMLError.CreateAt(emXMLStorageFormat, 0, 0);
      vType := TRALFieldType(vTypeOrd);
      vFlags := StrToIntDef(string(AReader.AttributeValue('flags')), 0);
      vSize := StrToIntDef(string(AReader.AttributeValue('size')), 0);
      vPrecision := StrToIntDef(string(AReader.AttributeValue('precision')), 0);

      SetLength(FFieldNames, vCount + 1);
      SetLength(FFieldTypes, vCount + 1);
      FFieldNames[vCount] := vName;
      FFieldTypes[vCount] := vType;
      Inc(vCount);

      vField := ADataset.FieldDefs.AddFieldDef;
      vField.Name := string(vName);
      vField.DataType := TRALDB.RALFieldTypeToFieldType(vType);
      if vType in [sftString, sftBCD] then
        vField.Size := vSize;
      if vType = sftBCD then
        vField.Precision := RALDecimalPrecision(vPrecision);
      if vFlags and 1 > 0 then
        vField.Attributes := vField.Attributes + [faReadonly];
      vField.Required := vFlags and 2 > 0;
    end
    else if AReader.Name = 'rows' then
    begin
      OpenDefs(ADataset);
      SetLength(FFieldDefTypes, vCount);
      for vValue := 0 to vCount - 1 do
        if FFoundFields[vValue] <> nil then
          FFieldDefTypes[vValue] := FFoundFields[vValue].DataType
        else
          FFieldDefTypes[vValue] := ftUnknown;

      ADataset.DisableControls;
      LiftReadOnly;
      try
        // <r> per record, <v> per field in the order of the fields
        while NextTag(AReader) do
        begin
          if AReader.Token = xtkEndElement then
            Break; // </rows>
          if AReader.Name <> 'r' then
            raise ERALXMLError.CreateAt(emXMLStorageFormat, 0, 0);
          ADataset.Append;
          vValue := 0;
          while NextTag(AReader) do
          begin
            if AReader.Token = xtkEndElement then
              Break; // </r>
            if AReader.AttributeValue('n') = '1' then
            begin
              AReader.Skip;
              Inc(vValue);
              Continue;
            end;
            if AReader.AttributeValue('e') = 'b64' then
              vText := TRALBase64.Decode(AReader.ReadElementText)
            else
              vText := AReader.ReadElementText;
            if vValue < vCount then
              SetFieldText(vValue, vText);
            Inc(vValue);
          end;
          ADataset.Post;
          RALStoragePosted(ADataset);
        end;
      finally
        RestoreReadOnly;
        ADataset.EnableControls;
      end;
    end;
  end;

  if not ADataset.Active then
    OpenDefs(ADataset);
  SetLength(FFieldNames, 0);
  SetLength(FFieldTypes, 0);
  SetLength(FFoundFields, 0);
end;

procedure TRALStorageXML.LoadRAW(ADataset: TDataSet; AReader: TRALXMLReader);
var
  vColumns: array of TRALXMLColumn;
  vRows: array of TRALXMLRawRow;
  vRowCount, vColCount, vInt, vRow, vIdx: IntegerRAL;
  vName, vText, vNil, vHint: StringRAL;
  vType: TFieldType;
  vSize, vPrecision, vScale, vMaxLen, vP, vS: IntegerRAL;
  vAllInt, vAllDec, vAllBool, vAllDate, vAny: boolean;
  vDate: TDateTime;
  vFloat: Double;
  vField: TField;
  vDef: TFieldDef;

  function ColumnOf(const AName: StringRAL): IntegerRAL;
  var
    vCol: IntegerRAL;
  begin
    for vCol := 0 to vColCount - 1 do
      if vColumns[vCol].Name = AName then
      begin
        Result := vCol;
        Exit;
      end;
    if vColCount >= Length(vColumns) then
      SetLength(vColumns, vColCount * 2 + 8);
    vColumns[vColCount].Name := AName;
    vColumns[vColCount].TypeHint := '';
    Result := vColCount;
    Inc(vColCount);
  end;

begin
  if ADataset.Active then
    ADataset.Close;

  // first pass: every record in memory, so each column's type comes from all its values
  vRows := nil;
  vColumns := nil;
  vRowCount := 0;
  vColCount := 0;
  while NextTag(AReader) do
  begin
    if AReader.Token = xtkEndElement then
      Break; // the root
    if vRowCount >= Length(vRows) then
      SetLength(vRows, vRowCount * 2 + 16);
    SetLength(vRows[vRowCount], vColCount);
    while NextTag(AReader) do
    begin
      if AReader.Token = xtkEndElement then
        Break; // the record
      vName := RALXMLDecodeName(AReader.LocalName);
      vIdx := ColumnOf(vName);
      if vIdx >= Length(vRows[vRowCount]) then
        SetLength(vRows[vRowCount], vColCount);
      vNil := AttrNS(AReader, cNsXSI, 'nil');
      vHint := AttrNS(AReader, cNsXSI, 'type');
      if Pos(StringRAL(':'), vHint) > 0 then
        vHint := 'xs:' + Copy(vHint, Pos(StringRAL(':'), vHint) + 1, MaxInt);
      if vHint <> '' then
        vColumns[vIdx].TypeHint := vHint;
      vRows[vRowCount][vIdx].Present := True;
      if (vNil = 'true') or (vNil = '1') then
      begin
        vRows[vRowCount][vIdx].IsNull := True;
        AReader.Skip;
      end
      else
        vRows[vRowCount][vIdx].Text := AReader.ReadElementText;
    end;
    Inc(vRowCount);
  end;

  // the type of each column: a persistent field decides, then xsi:type, then the values
  ADataset.FieldDefs.Clear;
  SetLength(FFieldNames, vColCount);
  SetLength(FFieldTypes, vColCount);
  SetLength(FFieldDefTypes, vColCount);
  for vInt := 0 to vColCount - 1 do
  begin
    vName := vColumns[vInt].Name;
    FFieldNames[vInt] := vName;
    vField := ADataset.Fields.FindField(string(vName));
    vSize := 0;
    vPrecision := 0;
    vScale := 0;
    vHint := vColumns[vInt].TypeHint;
    if vField <> nil then
    begin
      vType := vField.DataType;
      vSize := vField.Size;
    end
    else if vHint = 'xs:base64Binary' then
      vType := ftBlob
    else
    begin
      vAllInt := True;
      vAllDec := True;
      vAllBool := True;
      vAllDate := True;
      vAny := False;
      vMaxLen := 0;
      for vRow := 0 to vRowCount - 1 do
      begin
        if (vInt >= Length(vRows[vRow])) or not vRows[vRow][vInt].Present or
           vRows[vRow][vInt].IsNull then
          Continue;
        vText := vRows[vRow][vInt].Text;
        vAny := True;
        if Length(vText) > vMaxLen then
          vMaxLen := Length(vText);
        if vAllInt and not IsIntegerText(vText) then
          vAllInt := False;
        if vAllDec then
        begin
          if IsIntegerText(vText) then
          begin
            vP := Length(vText);
            vS := 0;
          end
          else if not IsDecimalText(vText, vP, vS) then
            vAllDec := False;
          if vAllDec then
          begin
            if vP - vS > vPrecision - vScale then
              vPrecision := vScale + (vP - vS);
            if vS > vScale then
            begin
              vPrecision := vPrecision + (vS - vScale);
              vScale := vS;
            end;
          end;
        end;
        if vAllBool and (vText <> 'true') and (vText <> 'false') then
          vAllBool := False;
        if vAllDate and ((Length(vText) < 8) or not TryParseDate(vText, vDate) or
           AllDigitsText(vText)) then
          vAllDate := False;
      end;

      if not vAny then
      begin
        vType := ftWideString;
        vSize := 255;
      end
      else if vAllBool then
        vType := ftBoolean
      else if vAllInt then
        vType := ftLargeint
      else if vAllDec then
      begin
        vType := ftFMTBcd;
        vSize := vScale;
      end
      else if vAllDate then
        vType := ftDateTime
      else if vMaxLen > 255 then
        vType := ftWideMemo
      else
      begin
        vType := ftWideString;
        vSize := 255;
      end;
    end;

    FFieldTypes[vInt] := TRALDB.FieldTypeToRALFieldType(vType);
    FFieldDefTypes[vInt] := vType;
    vDef := ADataset.FieldDefs.AddFieldDef;
    vDef.Name := string(vName);
    vDef.DataType := vType;
    if vType in [ftString, ftWideString, ftFixedChar, ftFixedWideChar, ftBytes, ftVarBytes] then
      vDef.Size := vSize
    else if vType in [ftBCD, ftFMTBcd] then
    begin
      vDef.Size := vSize;
      vDef.Precision := RALDecimalPrecision(vPrecision);
    end;
  end;

  OpenDefs(ADataset);
  ADataset.DisableControls;
  LiftReadOnly;
  try
    for vRow := 0 to vRowCount - 1 do
    begin
      ADataset.Append;
      for vInt := 0 to vColCount - 1 do
      begin
        if (vInt >= Length(vRows[vRow])) or not vRows[vRow][vInt].Present or
           vRows[vRow][vInt].IsNull then
          Continue;
        // a value the guessed number type cannot hold is a text column
        if (FFieldTypes[vInt] = sftDouble) and not TextFloat(vRows[vRow][vInt].Text, vFloat) then
          Continue;
        SetFieldText(vInt, vRows[vRow][vInt].Text);
      end;
      ADataset.Post;
      RALStoragePosted(ADataset);
    end;
  finally
    RestoreReadOnly;
    ADataset.EnableControls;
  end;
  SetLength(FFieldNames, 0);
  SetLength(FFieldTypes, 0);
  SetLength(FFoundFields, 0);
end;

/// The field type of a datapacket field: fieldtype plus SUBTYPE
function DataPacketFieldType(const AType, ASubType: StringRAL): TFieldType;
var
  vType: StringRAL;
begin
  vType := AType;
  if (vType = 'string') and (ASubType = 'Guid') then
    Result := ftGuid
  else if vType = 'string' then
    Result := ftWideString
  else if vType = 'string.uni' then
    Result := ftWideString
  else if vType = 'i1' then
    Result := ftShortint
  else if vType = 'ui1' then
    Result := ftByte
  else if vType = 'i2' then
    Result := ftSmallint
  else if vType = 'ui2' then
    Result := ftWord
  else if (vType = 'i4') and (ASubType = 'Autoinc') then
    Result := ftAutoInc
  else if vType = 'i4' then
    Result := ftInteger
  else if vType = 'ui4' then
    Result := ftLongWord
  else if (vType = 'i8') or (vType = 'ui8') then
    Result := ftLargeint
  else if vType = 'boolean' then
    Result := ftBoolean
  else if (vType = 'r8') and (ASubType = 'Money') then
    Result := ftCurrency
  else if (vType = 'r8') or (vType = 'r4') then
    Result := ftFloat
  else if (vType = 'fixed') or (vType = 'fixedFMT') then
    Result := ftFMTBcd
  else if vType = 'date' then
    Result := ftDate
  else if vType = 'time' then
    Result := ftTime
  else if (LowerCase(string(vType)) = 'datetime') or (vType = 'SQLdateTime') then
    Result := ftDateTime
  else if (vType = 'bin.hex') and (ASubType = 'Text') then
    Result := ftWideMemo
  else if (vType = 'bin.hex') and (ASubType = 'WideText') then
    Result := ftWideMemo
  else if (vType = 'bin.hex') and (ASubType = '') then
    Result := ftBlob
  else if vType = 'bin.hex' then
    Result := ftBlob
  else
    Result := ftWideString;
end;

procedure TRALStorageXML.LoadDataPacket(ADataset: TDataSet; AReader: TRALXMLReader);
var
  vCount, vInt, vRowState: IntegerRAL;
  vName, vAttrName, vType, vSubType, vText: StringRAL;
  vWidth, vDecimals: IntegerRAL;
  vDef: TFieldDef;
  vFieldType: TFieldType;
  vFPC, vInRows: boolean;
  vAttrs: array of StringRAL;
  vAttr: TRALXMLAttribute;
  vIdx, vScan: IntegerRAL;
begin
  if ADataset.Active then
    ADataset.Close;
  ADataset.FieldDefs.Clear;
  vCount := 0;
  vFPC := False;
  vInRows := False;
  SetLength(FFieldNames, 0);

  while NextTag(AReader) do
  begin
    if AReader.Token = xtkEndElement then
    begin
      if AReader.Name = 'ROWDATA' then
        vInRows := False;
      Continue;
    end;

    if AReader.Name = 'FIELD' then
    begin
      // TBufDataset writes fieldname and lowercase width and subtype
      vName := AReader.AttributeValue('fieldname');
      vAttrName := AReader.AttributeValue('attrname');
      if vName = '' then
        vName := vAttrName
      else if AReader.HasAttribute('width') or AReader.HasAttribute('subtype') or
              (vName = vAttrName) then
        vFPC := True;
      if AReader.HasAttribute('width') or AReader.HasAttribute('subtype') then
        vFPC := True;
      vType := AReader.AttributeValue('fieldtype');
      vSubType := AReader.AttributeValue('SUBTYPE');
      if vSubType = '' then
        vSubType := AReader.AttributeValue('subtype');
      if (vSubType = '') and (Pos(StringRAL(':'), vType) > 0) then
      begin
        vSubType := Copy(vType, Pos(StringRAL(':'), vType) + 1, MaxInt);
        vType := Copy(vType, 1, Pos(StringRAL(':'), vType) - 1);
      end;
      vWidth := StrToIntDef(string(AReader.AttributeValue('WIDTH')), -1);
      if vWidth < 0 then
        vWidth := StrToIntDef(string(AReader.AttributeValue('width')), 0);
      vDecimals := StrToIntDef(string(AReader.AttributeValue('DECIMALS')), 0);

      SetLength(FFieldNames, vCount + 1);
      SetLength(vAttrs, vCount + 1);
      SetLength(FFieldDefTypes, vCount + 1);
      FFieldNames[vCount] := vName;
      vAttrs[vCount] := vAttrName;
      vFieldType := DataPacketFieldType(vType, vSubType);
      FFieldDefTypes[vCount] := vFieldType;

      vDef := ADataset.FieldDefs.AddFieldDef;
      vDef.Name := string(vName);
      vDef.DataType := vFieldType;
      case vFieldType of
        ftWideString:
          if (vType = 'string.uni') and not vFPC then
            vDef.Size := Max(1, vWidth div 2)
          else
            vDef.Size := Max(1, vWidth);
        ftGuid:
          vDef.Size := 38;
        ftFMTBcd:
          begin
            vDef.Size := vDecimals;
            vDef.Precision := RALDecimalPrecision(vWidth);
          end;
      end;
      if AReader.AttributeValue('readonly') = 'true' then
        vDef.Attributes := vDef.Attributes + [faReadonly];
      Inc(vCount);
    end
    else if AReader.Name = 'ROWDATA' then
    begin
      vInRows := not AReader.IsEmptyElement;
      SetLength(FFieldTypes, vCount);
      for vInt := 0 to vCount - 1 do
        FFieldTypes[vInt] := TRALDB.FieldTypeToRALFieldType(FFieldDefTypes[vInt]);
      OpenDefs(ADataset);
    end
    else if (AReader.Name = 'ROW') and vInRows then
    begin
      // the change log: an original (1) is superseded, a deleted row (2) is gone
      vRowState := StrToIntDef(string(AReader.AttributeValue('RowState')), 0);
      if (vRowState and 3) <> 0 then
        Continue;

      ADataset.DisableControls;
      LiftReadOnly;
      try
        ADataset.Append;
        for vInt := 0 to AReader.AttributeCount - 1 do
        begin
          vAttr := RALXMLReaderAttribute(AReader, vInt);
          vIdx := -1;
          for vScan := 0 to High(vAttrs) do
            if vAttrs[vScan] = vAttr.Name then
            begin
              vIdx := vScan;
              Break;
            end;
          if vIdx < 0 then
            Continue;
          vText := vAttr.Value;
          if FFieldDefTypes[vIdx] = ftBoolean then
            ReadFieldBoolean(FFoundFields[vIdx], TextBool(vText))
          else if FFieldDefTypes[vIdx] = ftWideMemo then
          begin
            // TClientDataSet writes a memo as its text; TBufDataset as well
            ReadFieldString(FFoundFields[vIdx], vText);
          end
          else
            SetFieldText(vIdx, vText);
        end;
        ADataset.Post;
        RALStoragePosted(ADataset);
      finally
        RestoreReadOnly;
        ADataset.EnableControls;
      end;
    end;
  end;

  if not ADataset.Active then
  begin
    SetLength(FFieldTypes, vCount);
    for vInt := 0 to vCount - 1 do
      FFieldTypes[vInt] := TRALDB.FieldTypeToRALFieldType(FFieldDefTypes[vInt]);
    OpenDefs(ADataset);
  end;
  SetLength(FFieldNames, 0);
  SetLength(FFieldTypes, 0);
  SetLength(FFoundFields, 0);
end;

/// The field type of an ADO column
function ADOFieldType(const AType, ADBType: StringRAL; ALong: boolean): TFieldType;
begin
  if AType = 'int' then
    Result := ftInteger
  else if AType = 'i2' then
    Result := ftSmallint
  else if AType = 'i1' then
    Result := ftShortint
  else if AType = 'ui1' then
    Result := ftByte
  else if AType = 'ui2' then
    Result := ftWord
  else if AType = 'ui4' then
    Result := ftLongWord
  else if (AType = 'i8') or (AType = 'ui8') then
    Result := ftLargeint
  else if AType = 'boolean' then
    Result := ftBoolean
  else if (AType = 'float') or (AType = 'r8') or (AType = 'r4') then
    Result := ftFloat
  else if (AType = 'number') and (ADBType = 'currency') then
    Result := ftCurrency
  else if AType = 'number' then
    Result := ftFMTBcd
  else if AType = 'date' then
    Result := ftDate
  else if AType = 'time' then
    Result := ftTime
  else if (AType = 'dateTime') or (AType = 'dateTime.tz') then
    Result := ftDateTime
  else if AType = 'bin.hex' then
    Result := ftBlob
  else if AType = 'uuid' then
    Result := ftGuid
  else if ALong then
    Result := ftWideMemo
  else
    Result := ftWideString;
end;

procedure TRALStorageXML.LoadADO(ADataset: TDataSet; AReader: TRALXMLReader);
var
  vCount, vInt, vIdx, vSkipDepth, vScan: IntegerRAL;
  vName, vRealName, vType, vDBType, vText: StringRAL;
  vMaxLength, vPrecision, vScale: Int64RAL;
  vLong, vInData, vOpened: boolean;
  vAttrs: array of StringRAL;
  vDef: TFieldDef;
  vAttr: TRALXMLAttribute;
  vFieldType: TFieldType;
begin
  // ADO writes line breaks literally inside its attributes and reads them back
  AReader.Options.NormalizeAttributeBlanks := False;
  if ADataset.Active then
    ADataset.Close;
  ADataset.FieldDefs.Clear;
  vCount := 0;
  vInData := False;
  vOpened := False;
  vSkipDepth := 0;
  SetLength(FFieldNames, 0);

  while NextTag(AReader) do
  begin
    if AReader.Token = xtkEndElement then
    begin
      if (vSkipDepth > 0) and (AReader.Depth < vSkipDepth) then
        vSkipDepth := 0;
      if (AReader.LocalName = 'data') and (AReader.NamespaceURI = cNsRowset) then
        vInData := False;
      Continue;
    end;

    if (AReader.LocalName = 'AttributeType') and (AReader.NamespaceURI = cNsSchema) then
    begin
      vName := AReader.AttributeValue('name');
      vRealName := AReader.AttributeValue('rs:name');
      if vRealName = '' then
        vRealName := vName;
      SetLength(FFieldNames, vCount + 1);
      SetLength(vAttrs, vCount + 1);
      SetLength(FFieldDefTypes, vCount + 1);
      FFieldNames[vCount] := vRealName;
      vAttrs[vCount] := vName;
      FFieldDefTypes[vCount] := ftWideString;
      Inc(vCount);
    end
    else if (AReader.LocalName = 'datatype') and (AReader.NamespaceURI = cNsSchema) and
            (vCount > 0) then
    begin
      vType := AReader.AttributeValue('dt:type');
      vDBType := AReader.AttributeValue('rs:dbtype');
      vMaxLength := StrToInt64Def(string(AReader.AttributeValue('dt:maxLength')), 0);
      vPrecision := StrToInt64Def(string(AReader.AttributeValue('rs:precision')), 0);
      vScale := StrToInt64Def(string(AReader.AttributeValue('rs:scale')), 0);
      vLong := AReader.AttributeValue('rs:long') = 'true';
      vFieldType := ADOFieldType(vType, vDBType, vLong);
      FFieldDefTypes[vCount - 1] := vFieldType;

      vDef := ADataset.FieldDefs.AddFieldDef;
      vDef.Name := string(FFieldNames[vCount - 1]);
      vDef.DataType := vFieldType;
      case vFieldType of
        ftWideString:
          vDef.Size := Max(1, Min(vMaxLength, 8000));
        ftGuid:
          vDef.Size := 38;
        ftFMTBcd:
          begin
            vDef.Size := vScale;
            vDef.Precision := RALDecimalPrecision(vPrecision);
          end;
      end;
    end
    else if (AReader.LocalName = 'data') and (AReader.NamespaceURI = cNsRowset) then
    begin
      vInData := True;
      if not vOpened then
      begin
        SetLength(FFieldTypes, vCount);
        for vInt := 0 to vCount - 1 do
          FFieldTypes[vInt] := TRALDB.FieldTypeToRALFieldType(FFieldDefTypes[vInt]);
        OpenDefs(ADataset);
        vOpened := True;
      end;
    end
    else if vInData and (AReader.NamespaceURI = cNsRowset) and
            ((AReader.LocalName = 'delete') or (AReader.LocalName = 'original')) then
    begin
      // a deleted row, or the original of an updated one, is not current data
      AReader.Skip;
    end
    else if vInData and (AReader.LocalName = 'row') then
    begin
      ADataset.DisableControls;
      LiftReadOnly;
      try
        ADataset.Append;
        for vInt := 0 to AReader.AttributeCount - 1 do
        begin
          vAttr := RALXMLReaderAttribute(AReader, vInt);
          vIdx := -1;
          for vScan := 0 to High(vAttrs) do
            if vAttrs[vScan] = vAttr.LocalName then
            begin
              vIdx := vScan;
              Break;
            end;
          if vIdx < 0 then
            Continue;
          vText := vAttr.Value;
          case FFieldDefTypes[vIdx] of
            ftBoolean:
              ReadFieldBoolean(FFoundFields[vIdx], TextBool(vText));
            ftBlob:
              SetFieldBytes(FFoundFields[vIdx], HexToBytes(vText));
          else
            SetFieldText(vIdx, vText);
          end;
        end;
        ADataset.Post;
        RALStoragePosted(ADataset);
      finally
        RestoreReadOnly;
        ADataset.EnableControls;
      end;
    end;
  end;

  if not vOpened then
  begin
    SetLength(FFieldTypes, vCount);
    for vInt := 0 to vCount - 1 do
      FFieldTypes[vInt] := TRALDB.FieldTypeToRALFieldType(FFieldDefTypes[vInt]);
    OpenDefs(ADataset);
  end;
  SetLength(FFieldNames, 0);
  SetLength(FFieldTypes, 0);
  SetLength(FFoundFields, 0);
end;

procedure TRALStorageXML.LoadFromStream(ADataset: TDataSet; AStream: TStream);
var
  vReader: TRALXMLReader;
  vInt: IntegerRAL;
  vRowset: boolean;
begin
  vReader := TRALXMLReader.Create;
  try
    vReader.SetInputStream(AStream);
    // the root says the format
    while vReader.Read do
      if vReader.Token = xtkStartElement then
        Break;
    if vReader.Token <> xtkStartElement then
      raise ERALXMLError.CreateAt(emXMLNoRoot, 0, 0);

    vRowset := False;
    for vInt := 0 to vReader.AttributeCount - 1 do
      if RALXMLReaderAttribute(vReader, vInt).Value = cNsRowset then
        vRowset := True;

    if vReader.Name = cRootDBWare then
      LoadDBWare(ADataset, vReader)
    else if vReader.Name = cRootDataPacket then
      LoadDataPacket(ADataset, vReader)
    else if (vReader.LocalName = 'xml') and vRowset then
      LoadADO(ADataset, vReader)
    else
      LoadRAW(ADataset, vReader);
    RALStorageLoaded(ADataset);
  finally
    vReader.Free;
  end;
end;

{ TRALStorageXMLLink }

constructor TRALStorageXMLLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetStorageFormat(rsfXML);
  FFormatOptions := TRALXMLFormatOptions.Create;
  FXMLType := xtAuto;
  FClientReader := xcrUnknown;
end;

destructor TRALStorageXMLLink.Destroy;
begin
  FreeAndNil(FFormatOptions);
  inherited Destroy;
end;

procedure TRALStorageXMLLink.SetFormatOptions(AValue: TRALXMLFormatOptions);
begin
  RALAssignOwned(FFormatOptions, AValue);
end;

function TRALStorageXMLLink.GetContentType: StringRAL;
begin
  Result := rctAPPLICATIONXML;
end;

function TRALStorageXMLLink.EffectiveType: TRALXMLType;
begin
  Result := FXMLType;
  if Result = xtAuto then
  begin
    // a TBufDataset reads the datapacket its own reader knows; the rest read RAL's own
    if FClientReader = xcrBufDataset then
      Result := xtDataPacket
    else
      Result := xtDBWare;
  end;
end;

function TRALStorageXMLLink.AllowNative(AServerDriver, AClientDriver: Byte): boolean;
begin
  Result := False;
  if FXMLType <> xtAuto then
    Exit;
  case FClientReader of
    xcrFireDAC:
      Result := AServerDriver = Ord(qtFiredac);
    xcrBufDataset:
      Result := AServerDriver = Ord(qtLazSQL);
  end;
end;

function TRALStorageXMLLink.NativeContentType: StringRAL;
begin
  Result := rctAPPLICATIONXML;
end;

procedure TRALStorageXMLLink.PrepareForDataSet(ADataSet: TDataSet);
var
  vClass: TClass;
  vName: string;
begin
  FClientReader := xcrUnknown;
  if ADataSet = nil then
    Exit;
  vClass := ADataSet.ClassType;
  while vClass <> nil do
  begin
    vName := vClass.ClassName;
    if SameText(vName, 'TFDDataSet') or SameText(vName, 'TADDataSet') then
    begin
      FClientReader := xcrFireDAC;
      Exit;
    end;
    if SameText(vName, 'TZAbstractRODataset') or SameText(vName, 'TZAbstractMemTable') then
    begin
      FClientReader := xcrZeos;
      Exit;
    end;
    if SameText(vName, 'TCustomBufDataset') then
    begin
      FClientReader := xcrBufDataset;
      Exit;
    end;
    vClass := vClass.ClassParent;
  end;
end;

function TRALStorageXMLLink.GetStorage: TRALStorage;
var
  vStorage: TRALStorageXML;
begin
  vStorage := TRALStorageXML.Create;
  vStorage.FieldCharCase := FieldCharCase;
  vStorage.FormatOptions.Assign(FFormatOptions);
  vStorage.XMLType := EffectiveType;
  Result := vStorage;
end;

function TRALStorageXMLLink.Clone: TRALStorageLink;
begin
  Result := inherited Clone;
  if Result = nil then
    Exit;
  TRALStorageXMLLink(Result).XMLType := FXMLType;
  TRALStorageXMLLink(Result).ClientReader := FClientReader;
  TRALStorageXMLLink(Result).FormatOptions.Assign(FFormatOptions);
end;

procedure TRALStorageXMLLink.SavePropsToStream(AWriter: TRALBinaryWriter);
begin
  inherited SavePropsToStream(AWriter);
  AWriter.WriteByte(Ord(FXMLType));
  AWriter.WriteByte(Ord(FClientReader));
  AWriter.WriteBoolean(FFormatOptions.DateTimeIsUTC);
  AWriter.WriteByte(Ord(FFormatOptions.DataPacketDialect));
end;

procedure TRALStorageXMLLink.LoadPropsFromStream(AWriter: TRALBinaryWriter);
begin
  inherited LoadPropsFromStream(AWriter);
  FXMLType := TRALXMLType(AWriter.ReadEnum(Ord(High(TRALXMLType))));
  FClientReader := TRALXMLClientReader(AWriter.ReadEnum(Ord(High(TRALXMLClientReader))));
  FFormatOptions.DateTimeIsUTC := AWriter.ReadBoolean;
  FFormatOptions.DataPacketDialect :=
    TRALDataPacketDialect(AWriter.ReadEnum(Ord(High(TRALDataPacketDialect))));
end;

function TRALStorageXMLLink.DataSetToXML(ADataSet: TDataSet): StringRAL;
var
  vStream: TMemoryStream;
begin
  vStream := TMemoryStream.Create;
  try
    SaveToStream(ADataSet, vStream);
    Result := RALXMLMakeString(vStream.Memory, vStream.Size);
  finally
    vStream.Free;
  end;
end;

procedure TRALStorageXMLLink.XMLToDataSet(ADataSet: TDataSet; const AXML: StringRAL);
var
  vStream: TRALStringStream;
begin
  vStream := TRALStringStream.Create(AXML);
  try
    LoadFromStream(ADataSet, vStream);
  finally
    vStream.Free;
  end;
end;

{ functions }

procedure RALStorageXMLSave(ADataSet: TDataSet; AStream: TStream; AXMLType: TRALXMLType);
var
  vStorage: TRALStorageXML;
begin
  vStorage := TRALStorageXML.Create;
  try
    vStorage.XMLType := AXMLType;
    vStorage.SaveToStream(ADataSet, AStream);
  finally
    vStorage.Free;
  end;
end;

procedure RALStorageXMLLoad(ADataSet: TDataSet; AStream: TStream);
var
  vStorage: TRALStorageXML;
begin
  vStorage := TRALStorageXML.Create;
  try
    vStorage.LoadFromStream(ADataSet, AStream);
  finally
    vStorage.Free;
  end;
end;

function RALStorageXMLIsRoot(const AName: StringRAL): boolean;
begin
  Result := (AName = cRootDBWare) or (AName = cRootDataPacket) or (AName = 'xml');
end;

{$IF Defined(FPC) or Defined(Delphi2005UP)}

{ TRALStorageXMLHelper }

procedure TRALStorageXMLHelper.LoadFromXML(const AXML: StringRAL);
var
  vStream: TRALStringStream;
begin
  vStream := TRALStringStream.Create(AXML);
  try
    RALStorageXMLLoad(Self, vStream);
  finally
    vStream.Free;
  end;
end;

function TRALStorageXMLHelper.ToXML(AXMLType: TRALXMLType): StringRAL;
var
  vStream: TMemoryStream;
begin
  vStream := TMemoryStream.Create;
  try
    RALStorageXMLSave(Self, vStream, AXMLType);
    Result := RALXMLMakeString(vStream.Memory, vStream.Size);
  finally
    vStream.Free;
  end;
end;
{$IFEND}

initialization
  RegisterClass(TRALStorageXMLLink);

finalization
  UnregisterStorageLink(TRALStorageXMLLink);

end.
