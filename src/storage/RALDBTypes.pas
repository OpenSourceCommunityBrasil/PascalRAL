/// Former name of RALStorageTypes, kept so code written for 1.x still compiles.
unit RALDBTypes deprecated 'use RALStorageTypes';

{$I ..\core\PascalRAL.inc}

interface

uses
  DB, FMTBcd,
  RALTypes, RALResponse, RALStorageTypes;

type
  /// Same as RALStorageTypes.TRALFieldType.
  TRALFieldType = RALStorageTypes.TRALFieldType;
  /// Same as RALStorageTypes.TRALDBTableOnError.
  TRALDBTableOnError = RALStorageTypes.TRALDBTableOnError;
  /// Same as RALStorageTypes.TRALDB.
  TRALDB = RALStorageTypes.TRALDB;
  /// Same as RALStorageTypes.TRALDBUpdateSQL.
  TRALDBUpdateSQL = RALStorageTypes.TRALDBUpdateSQL;
  /// Same as RALStorageTypes.TRALDBInfoField.
  TRALDBInfoField = RALStorageTypes.TRALDBInfoField;
  /// Same as RALStorageTypes.TRALDBInfoFields.
  TRALDBInfoFields = RALStorageTypes.TRALDBInfoFields;
  /// Same as RALStorageTypes.TRALDBInfoTable.
  TRALDBInfoTable = RALStorageTypes.TRALDBInfoTable;
  /// Same as RALStorageTypes.TRALDBInfoTables.
  TRALDBInfoTables = RALStorageTypes.TRALDBInfoTables;

const
  /// Same as RALStorageTypes.sftShortInt.
  sftShortInt = RALStorageTypes.sftShortInt;
  /// Same as RALStorageTypes.sftSmallInt.
  sftSmallInt = RALStorageTypes.sftSmallInt;
  /// Same as RALStorageTypes.sftInteger.
  sftInteger = RALStorageTypes.sftInteger;
  /// Same as RALStorageTypes.sftInt64.
  sftInt64 = RALStorageTypes.sftInt64;
  /// Same as RALStorageTypes.sftByte.
  sftByte = RALStorageTypes.sftByte;
  /// Same as RALStorageTypes.sftWord.
  sftWord = RALStorageTypes.sftWord;
  /// Same as RALStorageTypes.sftCardinal.
  sftCardinal = RALStorageTypes.sftCardinal;
  /// Same as RALStorageTypes.sftQWord.
  sftQWord = RALStorageTypes.sftQWord;
  /// Same as RALStorageTypes.sftDouble.
  sftDouble = RALStorageTypes.sftDouble;
  /// Same as RALStorageTypes.sftBoolean.
  sftBoolean = RALStorageTypes.sftBoolean;
  /// Same as RALStorageTypes.sftString.
  sftString = RALStorageTypes.sftString;
  /// Same as RALStorageTypes.sftBlob.
  sftBlob = RALStorageTypes.sftBlob;
  /// Same as RALStorageTypes.sftMemo.
  sftMemo = RALStorageTypes.sftMemo;
  /// Same as RALStorageTypes.sftDateTime.
  sftDateTime = RALStorageTypes.sftDateTime;
  /// Same as RALStorageTypes.sftBCD.
  sftBCD = RALStorageTypes.sftBCD;

/// Same as RALStorageTypes.RALFieldTypeName.
function RALFieldTypeName(AFieldType: TFieldType): StringRAL; overload;
/// Same as RALStorageTypes.RALFieldTypeName.
function RALFieldTypeName(AFieldType: TRALFieldType): StringRAL; overload;
/// Same as RALStorageTypes.RALNameToFieldType.
function RALNameToFieldType(const AName: StringRAL): TFieldType;
/// Same as RALStorageTypes.RALIsFieldTypeOrdinal.
function RALIsFieldTypeOrdinal(AValue: Int64RAL): boolean;
/// Same as RALStorageTypes.RALDecimalFieldType.
function RALDecimalFieldType: TRALFieldType;
/// Same as RALStorageTypes.RALBCDToText.
function RALBCDToText(const AValue: TBcd): StringRAL;
/// Same as RALStorageTypes.RALTextToBCD.
function RALTextToBCD(const AValue: StringRAL): TBcd;
/// Same as RALStorageTypes.RALTryTextToBCD.
function RALTryTextToBCD(const AValue: StringRAL; out ABcd: TBcd): boolean;
/// Same as RALStorageTypes.RALDecimalPrecision.
function RALDecimalPrecision(APrecision: IntegerRAL): IntegerRAL;
/// Same as RALStorageTypes.RALFieldPrecision.
function RALFieldPrecision(AField: TField): IntegerRAL;
/// Same as RALStorageTypes.RALDateTimeToUnixText.
function RALDateTimeToUnixText(const AValue: TDateTime): StringRAL;
/// Same as RALStorageTypes.RALUnixSecondsToDateTime.
function RALUnixSecondsToDateTime(const ASeconds: Double): TDateTime;
/// Same as RALStorageTypes.RALDateTimeKind.
function RALDateTimeKind(AType: TFieldType): IntegerRAL;
/// Same as RALStorageTypes.RALDateTimeKindType.
function RALDateTimeKindType(AKind: IntegerRAL): TFieldType;
/// Same as RALStorageTypes.RALDBResponseError.
function RALDBResponseError(AResponse: TRALResponse): StringRAL;

var
  /// The same variable as RALStorageTypes.RALLegacyWire.
  RALLegacyWire: boolean absolute RALStorageTypes.RALLegacyWire;

implementation

function RALFieldTypeName(AFieldType: TFieldType): StringRAL;
begin
  Result := RALStorageTypes.RALFieldTypeName(AFieldType);
end;

function RALFieldTypeName(AFieldType: TRALFieldType): StringRAL;
begin
  Result := RALStorageTypes.RALFieldTypeName(AFieldType);
end;

function RALNameToFieldType(const AName: StringRAL): TFieldType;
begin
  Result := RALStorageTypes.RALNameToFieldType(AName);
end;

function RALIsFieldTypeOrdinal(AValue: Int64RAL): boolean;
begin
  Result := RALStorageTypes.RALIsFieldTypeOrdinal(AValue);
end;

function RALDecimalFieldType: TRALFieldType;
begin
  Result := RALStorageTypes.RALDecimalFieldType;
end;

function RALBCDToText(const AValue: TBcd): StringRAL;
begin
  Result := RALStorageTypes.RALBCDToText(AValue);
end;

function RALTextToBCD(const AValue: StringRAL): TBcd;
begin
  Result := RALStorageTypes.RALTextToBCD(AValue);
end;

function RALTryTextToBCD(const AValue: StringRAL; out ABcd: TBcd): boolean;
begin
  Result := RALStorageTypes.RALTryTextToBCD(AValue, ABcd);
end;

function RALDecimalPrecision(APrecision: IntegerRAL): IntegerRAL;
begin
  Result := RALStorageTypes.RALDecimalPrecision(APrecision);
end;

function RALFieldPrecision(AField: TField): IntegerRAL;
begin
  Result := RALStorageTypes.RALFieldPrecision(AField);
end;

function RALDateTimeToUnixText(const AValue: TDateTime): StringRAL;
begin
  Result := RALStorageTypes.RALDateTimeToUnixText(AValue);
end;

function RALUnixSecondsToDateTime(const ASeconds: Double): TDateTime;
begin
  Result := RALStorageTypes.RALUnixSecondsToDateTime(ASeconds);
end;

function RALDateTimeKind(AType: TFieldType): IntegerRAL;
begin
  Result := RALStorageTypes.RALDateTimeKind(AType);
end;

function RALDateTimeKindType(AKind: IntegerRAL): TFieldType;
begin
  Result := RALStorageTypes.RALDateTimeKindType(AKind);
end;

function RALDBResponseError(AResponse: TRALResponse): StringRAL;
begin
  Result := RALStorageTypes.RALDBResponseError(AResponse);
end;

end.
