unit RALDBTypes;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB,
  RALTypes;

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

  TRALDBOnError = procedure(Sender : TObject; AException : StringRAL) of object;

  { TRALDB }

  TRALDB = class
  public
    class function FieldTypeToRALFieldType(AFieldType : TFieldType) : TRALFieldType;
    class function RALFieldTypeToFieldType(AFieldType : TRALFieldType) : TFieldType;
    class function FieldProviderFlags(AField : TField) : Byte;
    class procedure ParseSQLParams(ASQL : StringRAL; AParams : TParams);
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

class function TRALDB.FieldProviderFlags(AField: TField): Byte;
begin
  Result := 0;
  if AField.ReadOnly then
    Result := Result + 1;
  if AField.Required then
    Result := Result + 2;
  if pfHidden in AField.ProviderFlags then
    Result := Result + 4;
  if pfInKey in AField.ProviderFlags then
    Result := Result + 8;
  if pfInUpdate in AField.ProviderFlags then
    Result := Result + 16;
  if pfInWhere in AField.ProviderFlags then
    Result := Result + 32;
  {$IFDEF FPC}
  if pfRefreshOnInsert in AField.ProviderFlags then
    Result := Result + 64;
  if pfRefreshOnUpdate in AField.ProviderFlags then
    Result := Result + 128;
  {$ENDIF}
end;

class procedure TRALDB.ParseSQLParams(ASQL: StringRAL; AParams: TParams);
var
  vParamName: StringRAL;
  vEscapeQuote, vEspaceDoubleQuote : boolean;
  vParam: boolean;
  vChar: UTF8Char;
  vInt: IntegerRAL;
  vOldParams: TStringList;
  vObjParam: TParam;
const
  cEndParam : set of Char = [';', '=', '>', '<', ' ', ',', '(', ')', '-', '+',
                            '/', '*', '!', '''', '"', '|', #0..#31, #127..#255];

  procedure AddParamSQL;
  var
    vIdxParam : IntegerRAL;
  begin
    vParamName := Trim(vParamName);
    if vParamName <> '' then
    begin
      if AParams.FindParam(vParamName) = nil then
        AParams.CreateParam(ftUnknown, vParamName, ptInput);

      vIdxParam := vOldParams.IndexOf(vParamName);
      if vIdxParam >= 0 then
        vOldParams.Delete(vIdxParam);
    end;
    vParam := False;
    vParamName := '';
  end;
begin
  vOldParams := TStringList.Create;
  try
    AParams.BeginUpdate;
    for vInt := 0 to Pred(AParams.Count) do
      vOldParams.Add(AParams.Items[vInt].Name);

    vEscapeQuote := False;
    vEspaceDoubleQuote := False;
    vParam := False;
    vChar := #0;
    vParamName := '';

    for vInt := POSINISTR to RALHighStr(ASQL) do
    begin
      if (ASQL[vInt] = '''') and (not vEspaceDoubleQuote) and
         (not (vEscapeQuote and (vChar = '\'))) then
      begin
        AddParamSQL;
        vEscapeQuote := not vEscapeQuote;
      end
      else if (ASQL[vInt] = '"') and (not vEscapeQuote) and
              (not (vEspaceDoubleQuote and (vChar = '\'))) then
      begin
        AddParamSQL;
        vEspaceDoubleQuote := not vEspaceDoubleQuote;
      end
      else if (ASQL[vInt] = ':') and (not vEscapeQuote) and
              (not vEspaceDoubleQuote) then
      begin
        AddParamSQL;
        vParam := CharInSet(vChar, cEndParam);
      end
      else if (vParam) then
      begin
        if (not CharInSet(ASQL[vInt], cEndParam)) then
          vParamName := vParamName + ASQL[vInt]
        else
          AddParamSQL;
      end;
      vChar := ASQL[vInt];
    end;
    AddParamSQL;

    for vInt := 0 to Pred(vOldParams.Count) do
    begin
      vObjParam := AParams.FindParam(vOldParams.Strings[vInt]);
      if vObjParam <> nil then
      begin
        AParams.RemoveParam(vObjParam);
        FreeAndNil(vObjParam);
      end;
    end;
    AParams.EndUpdate;
  finally
    FreeAndNil(vOldParams)
  end;
end;

end.

