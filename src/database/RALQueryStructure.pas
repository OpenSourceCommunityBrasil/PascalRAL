unit RALQueryStructure;

interface

uses
  Classes, SysUtils, DB, TypInfo,
  RALTypes, RALDBBase, RALJSON, RALBase64, RALDBStorage;

type
  { TRALQueryStructure }

  TRALQueryStructure = class
  protected
    function GetQuerySQL(ADataset : TDataSet): StringRAL;
    function GetQueryClass(ADataset : TDataSet): TRALDBDriverType;
    function GetQueryParams(ADataset : TDataSet): TParams;

    procedure SetQuerySQL(ADataset : TDataSet; AString: StringRAL);
    procedure SetQueryParams(ADataset : TDataSet; AParams: TParams);
  public
    function ExportToJSON(ADataset : TDataSet): TStream;
    procedure ImportFromJSON(AStream: TStream; var ASQL : StringRAL;
                             var AParams : TParams;
                             var AQueryType : TRALDBDriverType); overload;
    procedure ImportFromJSON(AStream: TStream; ADataset : TDataSet); overload;

    function ExportToBinary(ADataset : TDataSet) : TStream;
    procedure ImportFromBinary(AStream: TStream; var ASQL : StringRAL;
                               var AParams : TParams;
                               var AQueryType : TRALDBDriverType); overload;
    procedure ImportFromBinary(AStream: TStream; ADataset : TDataSet); overload;
  end;

implementation

const
  cStructureVersion: byte = 1;

{ TRALQueryStructure }

function TRALQueryStructure.GetQuerySQL(ADataset : TDataSet): StringRAL;
var
  vStrings : TStrings;
begin
  vStrings := TStrings(GetObjectProp(ADataset, 'SQL'));
  if vStrings <> nil then
    Result := Trim(vStrings.Text);
end;

function TRALQueryStructure.GetQueryClass(ADataset : TDataSet): TRALDBDriverType;
begin
  if SameText(ADataset.ClassName, 'TFDQuery') then
    Result := qtFiredac
  else if SameText(ADataset.ClassName, 'TZQuery') or
          SameText(ADataset.ClassName, 'TZReadOnlyQuery') then
    Result := qtZeos
  {$IFDEF FPC}
    // evitando conflito com dbexpress
    else if SameText(ADataset.ClassName, 'TSQLQuery') then
      Result := qtLazSQL
  {$ENDIF}
  else
    Result := qtOther;
end;

function TRALQueryStructure.GetQueryParams(ADataset : TDataSet): TParams;
var
  vColetion : TCollection;
  vColetItem : TCollectionItem;
  vInt : IntegerRAL;
  vType : StringRAL;
  vParam : TParam;
begin
  vColetion := TCollection(GetObjectProp(ADataset,'Params'));
  if vColetion <> nil then
  begin
    Result := TParams.Create;
    for vInt := 0 to Pred(vColetion.Count) do
    begin
      vColetItem := vColetion.Items[vInt];

      vParam := TParam(Result.Add);
      vParam.Name := GetStrProp(vColetItem, 'Name');

      vType := GetEnumProp(vColetItem, 'DataType');
      vParam.DataType := TFieldType(GetEnumValue(TypeInfo(TFieldType), vType));

      vParam.Size := GetInt64Prop(vColetItem, 'Size');
      vParam.Value := GetVariantProp(vColetItem, 'Value');
    end;
  end;
end;

procedure TRALQueryStructure.SetQueryParams(ADataset: TDataSet;
  AParams: TParams);
begin

end;

procedure TRALQueryStructure.SetQuerySQL(ADataset : TDataSet; AString: StringRAL);
var
  vStrings : TStrings;
begin
  vStrings := TStrings(GetObjectProp(ADataset, 'SQL'));
  if vStrings <> nil then
    vStrings.Text := AString;
end;

function TRALQueryStructure.ExportToJSON(ADataset : TDataSet): TStream;
var
  vjObj : TRALJSONObject;
  vjArr : TRALJSONArray;
  vjParam : TRALJSONObject;
  vParams: TParams;
  vInt : IntegerRAL;
  vString : StringRAL;
  vStorType : TRALStorageFieldType;
begin
  Result := TMemoryStream.Create;
  vString := '';

  vjObj := TRALJSONObject.Create;
  try
    vjObj.Add('version', cStructureVersion);
    vjObj.Add('class', Ord(GetQueryClass(ADataset)));
    vjObj.Add('sql', GetQuerySQL(ADataset));

    // params
    vParams := GetQueryParams(ADataset);
    try
      vjArr := TRALJSONArray.Create;
      for vInt := 0 to Pred(vParams.Count) do
      begin
        vjParam := TRALJSONObject.Create;

        vjParam.Add('name', vParams.Items[vInt].Name);

        vStorType := TRALDBStorage.FieldTypeToStorageFieldType(vParams.Items[vInt].DataType);
        vjParam.Add('datatype', Ord(vStorType));

        vjParam.Add('size', vParams.Items[vInt].Size);

        // values
        case vStorType of
          sftInt1,
          sftInt2,
          sftInt4,
          sftInt8,
          sftuInt1,
          sftuInt2,
          sftuInt4,
          sftuInt8   : vjParam.Add('value', vParams.Items[vInt].AsLargeInt);
          sftDouble  : vjParam.Add('value', vParams.Items[vInt].AsFloat);
          sftBoolean : vjParam.Add('value', vParams.Items[vInt].AsBoolean);
          sftString  : vjParam.Add('value', vParams.Items[vInt].AsString);
          sftBlob    : vjParam.Add('value', TRALBase64.Encode(vParams.Items[vInt].AsBlob));
          sftMemo    : vjParam.Add('value', vParams.Items[vInt].AsString);
          sftDateTime: vjParam.Add('value', vParams.Items[vInt].AsDateTime);
        end;

        vjArr.Add(vjParam);
      end;
    finally
      FreeAndNil(vParams);
    end;

    vjObj.Add('params', vjArr);
    vString := vjObj.ToJSON;

    Result.Write(vString[PosIniStr], Length(vString));
  finally
    FreeAndNil(vjObj);
  end;
end;

function TRALQueryStructure.ExportToBinary(ADataset : TDataSet): TStream;
var
  vString: StringRAL;
  vInt, vSize: IntegerRAL;
  vByte: Byte;
  vParams: TParams;
  vFloat: Double;
  vInt64 : Int64RAL;
  vBytes : TBytes;
  vStorType : TRALStorageFieldType;
  vBool : boolean;
  vShort : ShortInt;
  vSmallInt : SmallInt;
  vWord : Word;
  vCardinal : Cardinal;
  vQWord : UInt64;
begin
  Result := TMemoryStream.Create;
  // version
  Result.Write(cStructureVersion, SizeOf(cStructureVersion));

  // class dataset
  vByte := Ord(GetQueryClass(ADataset));
  Result.Write(vByte, SizeOf(vByte));

  // sql dataset
  vString := GetQuerySQL(ADataset);
  vInt := Length(vString);
  Result.Write(vInt, SizeOf(vInt));
  Result.Write(vString[PosIniStr], vInt);

  // params
  vParams := GetQueryParams(ADataset);
  try
    vByte := vParams.Count;
    Result.Write(vByte, SizeOf(vByte));
    for vInt := 0 to Pred(vParams.Count) do
    begin
      // name
      vString := vParams.Items[vInt].Name;
      vByte := Length(vString);
      Result.Write(vByte, SizeOf(vByte));
      Result.Write(vString[PosIniStr], vByte);

      // datatype
      vStorType := TRALDBStorage.FieldTypeToStorageFieldType(vParams.Items[vInt].DataType);
      vByte := Ord(vStorType);
      Result.Write(vByte, SizeOf(vByte));

      // size
      vSize := vParams.Items[vInt].Size;
      Result.Write(vSize, SizeOf(vSize));

      // values
      case vStorType of
        sftInt1    : begin
          vShort := vParams.Items[vInt].AsInteger;
          Result.Write(vShort, SizeOf(vShort));
        end;
        sftInt2    : begin
          vSmallInt := vParams.Items[vInt].AsInteger;
          Result.Write(vSmallInt, SizeOf(vSmallInt));
        end;
        sftInt4    : begin
          vSize := vParams.Items[vInt].AsInteger;
          Result.Write(vSize, SizeOf(vSize));
        end;
        sftInt8    : begin
          vInt64 := vParams.Items[vInt].AsLargeInt;
          Result.Write(vInt64, SizeOf(vInt64));
        end;
        sftuInt1   : begin
          vByte := vParams.Items[vInt].AsInteger;
          Result.Write(vByte, SizeOf(vByte));
        end;
        sftuInt2   : begin
          vWord := vParams.Items[vInt].AsInteger;
          Result.Write(vWord, SizeOf(vWord));
        end;
        sftuInt4   : begin
          {$IFDEF FPC}
            vCardinal := vParams.Items[vInt].AsLargeInt;
          {$ELSE}
            vCardinal := vParams.Items[vInt].AsLongWord;
          {$ENDIF}
          Result.Write(vCardinal, SizeOf(vCardinal));
        end;
        sftuInt8   : begin
          vQWord := vParams.Items[vInt].AsLargeInt;
          Result.Write(vQWord, SizeOf(vQWord));
        end;
        sftDouble  : begin
          vFloat := vParams.Items[vInt].AsFloat;
          Result.Write(vFloat, SizeOf(vFloat));
        end;
        sftBoolean : begin
          vBool := vParams.Items[vInt].AsBoolean;
          Result.Write(vBool, SizeOf(vBool));
        end;
        sftString  : begin
          vString := vParams.Items[vInt].AsString;
          vInt64 := Length(vString);
          Result.Write(vInt64, SizeOf(vInt64));
          Result.Write(vString[PosIniStr], vByte);
        end;
        sftBlob    : begin
          vBytes := vParams.Items[vInt].AsBlob;
          vInt64 := Length(vBytes);
          Result.Write(vInt64, SizeOf(vInt64));
          Result.Write(vBytes[0], vInt64);
        end;
        sftMemo    : begin
          vString := vParams.Items[vInt].AsString;
          vInt64 := Length(vString);
          Result.Write(vInt64, SizeOf(vInt64));
          Result.Write(vString[PosIniStr], vInt64);
        end;
        sftDateTime: begin
          vFloat := vParams.Items[vInt].AsDateTime;
          Result.Write(vFloat, SizeOf(vFloat));
        end;
      end;
    end;
  finally
    FreeAndNil(vParams);
  end;
end;

procedure TRALQueryStructure.ImportFromBinary(AStream: TStream; var ASQL : StringRAL;
                             var AParams : TParams;
                             var AQueryType : TRALDBDriverType);
var
  vByte, vTotParam: Byte;
  vString: StringRAL;
  vInt, vSize: IntegerRAL;
  vFloat: Double;
  vInt64 : Int64RAL;
  vBytes : TBytes;
  vParam : TParam;
  vStorType : TRALStorageFieldType;
  vBool : boolean;
  vShort : ShortInt;
  vSmallInt : SmallInt;
  vWord : Word;
  vCardinal : Cardinal;
  vQWord : UInt64;
begin
  AStream.Position := 0;

  // version
  AStream.Read(vByte, SizeOf(vByte));
  if vByte <> cStructureVersion then
    raise Exception.Create('Versão da estrutura não confere');

  // class dataset
  AStream.Read(vByte, SizeOf(vByte));
  AQueryType := TRALDBDriverType(vByte);

  // sql dataset
  AStream.Read(vInt, SizeOf(vInt));
  SetLength(vString, vInt);
  AStream.Read(vString[PosIniStr], vInt);
  ASQL := vString;

  AStream.Read(vTotParam, SizeOf(vTotParam));

  AParams := TParams.Create(nil);
  for vInt := 1 to vTotParam do
  begin
    vParam := TParam(AParams.Add);

    // name
    AStream.Read(vByte, SizeOf(vByte));
    SetLength(vString, vByte);
    AStream.Read(vString[PosIniStr], vByte);
    vParam.Name := vString;

    // datatype
    AStream.Read(vByte, SizeOf(vByte));
    vStorType := TRALStorageFieldType(vByte);
    vParam.DataType := TRALDBStorage.StorageFieldTypeToFieldType(vStorType);

    // size
    AStream.Read(vSize, SizeOf(vSize));
    vParam.Size := vSize;

    // values
    case vStorType of
      sftInt1    : begin
        AStream.Read(vShort, SizeOf(vShort));
        vParam.AsInteger := vShort;
      end;
      sftInt2    : begin
        AStream.Read(vSmallInt, SizeOf(vSmallInt));
        vParam.AsInteger := vSmallInt;
      end;
      sftInt4    : begin
        AStream.Read(vSize, SizeOf(vSize));
        vParam.AsInteger := vSize;
      end;
      sftInt8    : begin
        AStream.Read(vInt64, SizeOf(vInt64));
        vParam.AsLargeInt := vInt64;
      end;
      sftuInt1   : begin
        AStream.Read(vByte, SizeOf(vByte));
        vParam.AsInteger := vByte;
      end;
      sftuInt2   : begin
        AStream.Read(vWord, SizeOf(vWord));
        vParam.AsInteger := vWord;
      end;
      sftuInt4   : begin
        AStream.Read(vCardinal, SizeOf(vCardinal));
        vParam.AsLargeInt := vCardinal;
      end;
      sftuInt8   : begin
        AStream.Read(vQWord, SizeOf(vQWord));
        vParam.AsLargeInt := vQWord;
      end;
      sftDouble  : begin
        AStream.Read(vFloat, SizeOf(vFloat));
        vParam.AsFloat := vFloat;
      end;
      sftBoolean : begin
        AStream.Read(vBool, SizeOf(vBool));
        vParam.AsBoolean := vBool;
      end;
      sftString  : begin
        AStream.Read(vInt64, SizeOf(vInt64));
        SetLength(vString, vInt64);
        AStream.Read(vString[PosIniStr], vInt64);
        vParam.AsString := vString;
      end;
      sftBlob    : begin
        AStream.Read(vInt64, SizeOf(vInt64));
        SetLength(vBytes, vInt64);
        AStream.Read(vBytes[0], vInt64);
        vParam.AsBlob := vBytes;
        SetLength(vBytes, 0);
      end;
      sftMemo    : begin
        AStream.Read(vInt64, SizeOf(vInt64));
        SetLength(vString, vInt64);
        AStream.Read(vString[PosIniStr], vInt64);
        vParam.AsMemo := vString;
      end;
      sftDateTime: begin
        AStream.Read(vFloat, SizeOf(vFloat));
        vParam.AsDateTime := vFloat;
      end;
    end;
  end;
end;

procedure TRALQueryStructure.ImportFromBinary(AStream: TStream;
  ADataset: TDataSet);
var
  vSQL : StringRAL;
  vParams : TParams;
  vQueryType : TRALDBDriverType;
begin
  ImportFromBinary(AStream, vSQL, vParams, vQueryType);
  SetQuerySQL(ADataset, vSQL);
  //TODO SetQueryParams
end;

procedure TRALQueryStructure.ImportFromJSON(AStream: TStream;
  ADataset: TDataSet);
var
  vSQL : StringRAL;
  vParams : TParams;
  vQueryType : TRALDBDriverType;
begin
  ImportFromJSON(AStream, vSQL, vParams, vQueryType);
  SetQuerySQL(ADataset, vSQL);
  //TODO SetQueryParams
end;

procedure TRALQueryStructure.ImportFromJSON(AStream: TStream;
  var ASQL: StringRAL; var AParams: TParams; var AQueryType: TRALDBDriverType);
var
  vjObj : TRALJSONObject;
  vInt : IntegerRAL;
  vjArr : TRALJSONArray;
  vjParam : TRALJSONObject;
  vParam : TParam;
  vStorType : TRALStorageFieldType;
  vBytes : TBytes;
begin
  vjObj := TRALJSONObject(TRALJSON.ParseJSON(AStream));
  try
    if vjObj <> nil then
    begin
      vInt := vjObj.Get('version').AsInteger;
      if vInt <> cStructureVersion then
        raise Exception.Create('Versão da estrutura não confere');

      vInt := vjObj.Get('class').AsInteger;
      AQueryType := TRALDBDriverType(vInt);

      ASQL := vjObj.Get('sql').AsString;

      AParams := TParams.Create(nil);
      vjArr := TRALJSONArray(vjObj.Get('params'));
      if vjArr <> nil then
      begin
        for vInt := 0 to Pred(vjArr.Count) do
        begin
          vjParam := TRALJSONObject(vjArr.Get(vInt));

          vParam := TParam(AParams.Add);
          vParam.Name := vjParam.Get('name').AsString;

          vStorType := TRALStorageFieldType(vjParam.Get('datatype').AsInteger);
          vParam.DataType := TRALDBStorage.StorageFieldTypeToFieldType(vStorType);

          vParam.Size := vjParam.Get('size').AsInteger;

          case vStorType of
            sftInt1,
            sftInt2,
            sftInt4,
            sftuInt1,
            sftuInt2    : vParam.AsInteger := vjParam.Get('value').AsInteger;
            sftInt8,
            sftuInt4,
            sftuInt8    : vParam.AsLargeInt := vjParam.Get('value').AsInteger;
            sftDouble   : vParam.AsFloat := vjParam.Get('value').AsFloat;
            sftBoolean  : vParam.AsBoolean := vjParam.Get('value').AsBoolean;
            sftString   : vParam.AsString := vjParam.Get('value').AsString;
            sftBlob     : begin
              vBytes := TRALBase64.DecodeAsBytes(vjParam.Get('value').AsString);
              vParam.AsBlob := vBytes;
              SetLength(vBytes, 0);
            end;
            sftMemo     : vParam.AsMemo := vjParam.Get('value').AsString;
            sftDateTime : vParam.AsDateTime := vjParam.Get('value').AsFloat;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(vjObj);
  end;
end;

end.

