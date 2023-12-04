unit RALDB;

{
  Laz - Win32/Win64 and Delphi Win32/Win64
  Byte     : Size=1 | 0 - 255
  ShortInt : Size=1 | -128 - 127
  Word     : Size=2 | 0 - 65535
  Smallint : Size=2 | -32768 - 32767
  Cardinal : Size=4 | 0 - 4294967295
  LongWord : Size=4 | 0 - 4294967295
  Integer  : Size=4 | -2147483648 - 2147483647
  LongInt  : Size=4 | -2147483648 - 2147483647
  Int64    : Size=8 | -9223372036854775808 - 9223372036854775807
  Double   : Size=8 | 0 - 0
  DateTime : Size=8 | 0 - 0
  Boolean  : Size=1 | 0 - 0
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB;

type
  TRALFieldType = (rftUnknown, rftByte, rftShortInt, rftWord, rftSmallInt,
                   rftLongWord, rtfInteger, rftLargeInt, rtfString, rtfMemo,
                   rtfBlob, rtfFloat, rtfDate, rtfTime, rtfDateTime,
                   rtfBoolean, rtfBytes);

  TRALDB = class
  public
    class function DataTypeToRALFieldType(ADataType : TFieldType) : TRALFieldType;
    class function RALDataTypeToFieldType(ADataType : TRALFieldType) : TFieldType;

    class function DataTypeToByte(ADataType : TFieldType) : Byte;
    class function ByteToDataType(AByte : Byte) : TFieldType;

    class function RALDataTypeSize(ADataType : TRALFieldType) : Int64;
  end;

implementation

class function TRALDB.DataTypeToRALFieldType(ADataType : TFieldType) : TRALFieldType;
begin
  case ADataType of
    ftUnknown         : Result := rftUnknown;
    ftString          : Result := rtfString;
    ftSmallint        : Result := rftSmallInt;
    ftInteger         : Result := rtfInteger;
    ftWord            : Result := rftWord;
    ftBoolean         : Result := rtfBoolean;
    ftFloat           : Result := rtfFloat;
    ftCurrency        : Result := rtfFloat;
    ftBCD             : Result := rtfFloat;
    ftDate            : Result := rtfDate;
    ftTime            : Result := rtfTime;
    ftDateTime        : Result := rtfDateTime;
    ftBytes           : Result := rtfBytes;
    ftVarBytes        : Result := rtfBytes;
    ftAutoInc         : Result := rftLargeInt;
    ftBlob            : Result := rtfBlob;
    ftMemo            : Result := rtfMemo;
    ftGraphic         : Result := rtfBlob;
    ftFmtMemo         : Result := rtfMemo;
    ftParadoxOle      : Result := rftUnknown;
    ftDBaseOle        : Result := rftUnknown;
    ftTypedBinary     : Result := rftUnknown;
    ftCursor          : Result := rftUnknown;
    ftFixedChar       : Result := rtfString;
    ftWideString      : Result := rtfString;
    ftLargeint        : Result := rftLargeInt;
    ftADT             : Result := rftUnknown;
    ftArray           : Result := rftUnknown;
    ftReference       : Result := rftUnknown;
    ftDataSet         : Result := rftUnknown;
    ftOraBlob         : Result := rtfBlob;
    ftOraClob         : Result := rtfMemo;
    ftVariant         : Result := rtfBytes;
    ftInterface       : Result := rftUnknown;
    ftIDispatch       : Result := rftUnknown;
    ftGuid            : Result := rtfString;
    ftTimeStamp       : Result := rtfDateTime;
    ftFMTBcd          : Result := rtfFloat;
    ftFixedWideChar   : Result := rtfString;
    ftWideMemo        : Result := rtfString;
    {$IFNDEF FPC}
      ftOraTimeStamp    : Result := rtfDateTime;
      ftOraInterval     : Result := rtfDateTime;
      ftLongWord        : Result := rftLongWord;
      ftShortint        : Result := rftShortInt;
      ftByte            : Result := rftByte;
      ftExtended        : Result := rtfFloat;
      ftConnection      : Result := rftUnknown;
      ftParams          : Result := rftUnknown;
      ftStream          : Result := rtfBlob;
      ftTimeStampOffset : Result := rtfDateTime;
      ftObject          : Result := rftUnknown;
      ftSingle          : Result := rtfFloat;
    {$ENDIF}
  end;
end;

class function TRALDB.RALDataTypeToFieldType(ADataType: TRALFieldType): TFieldType;
begin
  case ADataType of
    rftUnknown  : Result := ftUnknown;
    rftByte     : Result := ftBytes;
    rftWord     : Result := ftWord;
    rftSmallInt : Result := ftSmallint;
    rtfInteger  : Result := ftInteger;
    rftLargeInt : Result := ftLargeint;
    rtfString   : Result := ftSmallint;
    rtfMemo     : Result := ftMemo;
    rtfBlob     : Result := ftBlob;
    rtfFloat    : Result := ftFloat;
    rtfDate     : Result := ftDate;
    rtfTime     : Result := ftTime;
    rtfDateTime : Result := ftDateTime;
    rtfBoolean  : Result := ftBoolean;
    rtfBytes    : Result := ftBytes;
    {$IFNDEF FPC}
      rftShortInt : Result := ftShortint;
      rftLongWord : Result := ftLongWord;
    {$ELSE}
      rftShortInt : Result := ftSmallint;
      rftLongWord : Result := ftLargeint;
    {$ENDIF}
  end;
end;

class function TRALDB.DataTypeToByte(ADataType: TFieldType): Byte;
begin
  Result := Ord(DataTypeToRALFieldType(ADataType));
end;

class function TRALDB.ByteToDataType(AByte: Byte): TFieldType;
begin
  Result := RALDataTypeToFieldType(TRALFieldType(AByte));
end;

class function TRALDB.RALDataTypeSize(ADataType: TRALFieldType): Int64;
begin
  case ADataType of
    rftUnknown  : Result := -1;
    rftByte     : Result := SizeOf(Byte);
    rftWord     : Result := SizeOf(Word);
    rftSmallInt : Result := SizeOf(SmallInt);
    rtfInteger  : Result := SizeOf(Integer);
    rftLargeInt : Result := SizeOf(Int64);
    rtfString   : Result := -1;
    rtfMemo     : Result := -1;
    rtfBlob     : Result := -1;
    rtfFloat    : Result := SizeOf(Double);
    rtfDate     : Result := SizeOf(TDateTime);
    rtfTime     : Result := SizeOf(TDateTime);
    rtfDateTime : Result := SizeOf(TDateTime);
    rtfBoolean  : Result := SizeOf(Boolean);
    rtfBytes    : Result := -1;
    rftShortInt : Result := SizeOf(ShortInt);
    rftLongWord : Result := SizeOf(LongWord);
  end;
end;


end.

