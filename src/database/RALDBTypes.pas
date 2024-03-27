unit RALDBTypes;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB;

type
  {
    ShortInt : 1 - Low: -128                 High: 127
    Byte     : 1 - Low: 0                    High: 255
    SmallInt : 2 - Low: -32768               High: 32767
    Word     : 2 - Low: 0                    High: 65535
    Integer  : 4 - Low: -2147483648          High: 2147483647
    LongInt  : 4 - Low: -2147483648          High: 2147483647
    Cardinal : 4 - Low: 0                    High: 4294967295
    LongWord : 4 - Low: 0                    High: 4294967295
    Int64    : 8 - Low: -9223372036854775808 High: 9223372036854775807
    QWord    : 8 - Low: 0                    High: 18446744073709551615
  }

  TRALFieldType = (sftShortInt, sftSmallInt, sftInteger, sftInt64, sftByte,
                   sftWord, sftCardinal, sftQWord, sftDouble, sftBoolean,
                   sftString, sftBlob, sftMemo, sftDateTime);

  { TRALDB }

  TRALDB = class
  public
    class function FieldTypeToRALFieldType(AFieldType : TFieldType) : TRALFieldType;
    class function RALFieldTypeToFieldType(AFieldType : TRALFieldType) : TFieldType;
  end;

implementation

{ TRALDB }

class function TRALDB.FieldTypeToRALFieldType(AFieldType: TFieldType): TRALFieldType;
begin
  case AFieldType of
    ftFixedWideChar,
    ftGuid,
    ftFixedChar,
    ftWideString,
    ftString   : Result := sftString;

    {$IFNDEF FPC}
      ftShortint : Result := sftShortInt;
      ftLongWord : Result := sftCardinal;
      ftByte     : Result := sftByte;
    {$ENDIF}
    ftSmallint : Result := sftSmallInt;
    ftWord     : Result := sftWord;
    ftInteger  : Result := sftInteger;
    ftLargeint,
    ftAutoInc  : Result := sftInt64;

    ftBoolean  : Result := sftBoolean;

    {$IFNDEF FPC}
      ftSingle,
      ftExtended,
    {$ENDIF}
    ftFMTBcd,
    ftFloat,
    ftCurrency,
    ftBCD      : Result := sftDouble;

    {$IFNDEF FPC}
      ftTimeStampOffset,
      ftOraTimeStamp,
      ftOraInterval,
    {$ENDIF}
    ftTimeStamp,
    ftDate,
    ftTime,
    ftDateTime : Result := sftDateTime;

    {$IFNDEF FPC}
      ftStream,
    {$ENDIF}
    ftOraBlob,
    ftTypedBinary,
    ftGraphic,
    ftBlob,
    ftBytes,
    ftVarBytes : Result := sftBlob;

    ftWideMemo,
    ftOraClob,
    ftMemo,
    ftFmtMemo  : Result := sftMemo;

// ignorados
{
    ftObject: ;
    ftConnection: ;
    ftParams: ;
    ftParadoxOle: ;
    ftDBaseOle: ;
    ftCursor: ;
    ftADT: ;
    ftArray: ;
    ftReference: ;
    ftDataSet: ;
    ftVariant: ;
    ftInterface: ;
    ftIDispatch: ;
}
  end;
end;

class function TRALDB.RALFieldTypeToFieldType(AFieldType: TRALFieldType): TFieldType;
begin
  case AFieldType of
    {$IFNDEF FPC}
      sftShortInt : Result := ftShortint;
      sftByte     : Result := ftByte;
      sftCardinal : Result := ftLongWord;
    {$ELSE}
      sftShortInt : Result := ftSmallint;
      sftByte     : Result := ftSmallint;
      sftCardinal : Result := ftLargeint;
    {$ENDIF}
    sftSmallInt : Result := ftSmallint;
    sftInteger  : Result := ftInteger;
    sftInt64    : Result := ftLargeint;
    sftWord     : Result := ftWord;
    sftQWord    : Result := ftLargeint;
    sftDouble   : Result := ftFloat;
    sftBoolean  : Result := ftBoolean;
    sftString   : Result := ftString;
    sftBlob     : Result := ftBlob;
    sftMemo     : Result := ftMemo;
    sftDateTime : Result := ftDateTime;
  end;
end;

end.

