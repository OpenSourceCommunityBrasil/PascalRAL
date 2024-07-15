/// Base unit for the module component that will enable DBWare on the server
unit RALDBModule;

interface

uses
  Classes, SysUtils, DB, TypInfo,
  RALServer, RALRequest, RALResponse, RALDBBase, RALParams, RALMIMETypes,
  RALConsts, RALTypes, RALDBStorage, RALBase64, RALRoutes, RALJSON, RALDBTypes,
  RALDBSQLCache;

type
  { TRALDBModule }

  TRALDBModule = class(TRALModuleRoutes)
  private
    FDatabase: StringRAL;
    FDatabaseLink: TRALDBLink;
    FDatabaseType: TRALDatabaseType;
    FHostname: StringRAL;
    FPassword: StringRAL;
    FPort: IntegerRAL;
    FStorageOutPut: TRALDBStorageLink;
    FUsername: StringRAL;
  protected
    procedure ApplyUpdates(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure ExecSQL(ARequest: TRALRequest; AResponse: TRALResponse);
    function FindDatabaseDriver: TRALDBBase;
    procedure GetFields(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure GetTables(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OpenSQL(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure SetDataBaseLink(AValue: TRALDBLink);
    procedure SetStorageOutPut(AValue: TRALDBStorageLink);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Database: StringRAL read FDatabase write FDatabase;
    property DatabaseLink: TRALDBLink read FDataBaseLink write SetDataBaseLink;
    property DatabaseType: TRALDatabaseType read FDatabaseType write FDatabaseType;
    property Hostname: StringRAL read FHostname write FHostname;
    property Password: StringRAL read FPassword write FPassword;
    property Port: IntegerRAL read FPort write FPort;
    property StorageOutPut: TRALDBStorageLink read FStorageOutPut write SetStorageOutPut;
    property Username: StringRAL read FUsername write FUsername;
  end;

implementation

{ TRALDBModule }

procedure TRALDBModule.SetStorageOutPut(AValue: TRALDBStorageLink);
begin
  if FStorageOutPut <> nil then
    FStorageOutPut.RemoveFreeNotification(Self);

  if AValue <> FStorageOutPut then
    FStorageOutPut := AValue;

  if FStorageOutPut <> nil then
    FStorageOutPut.FreeNotification(Self);
end;

procedure TRALDBModule.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FDataBaseLink) then
    FDataBaseLink := nil
  else if (Operation = opRemove) and (AComponent = FStorageOutPut) then
    FStorageOutPut := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TRALDBModule.SetDataBaseLink(AValue: TRALDBLink);
begin
  if FDataBaseLink <> nil then
    FDataBaseLink.RemoveFreeNotification(Self);

  if AValue <> FDataBaseLink then
    FDataBaseLink := AValue;

  if FDataBaseLink <> nil then
    FDataBaseLink.FreeNotification(Self);
end;

function TRALDBModule.FindDatabaseDriver: TRALDBBase;
var
  vClass: TRALDBClass;
  vUnit: StringRAL;
begin
  Result := nil;
  vClass := nil;

  if FDataBaseLink <> nil then
    vClass := FDataBaseLink.GetDBClass;

  if vClass <> nil then
  begin
    Result := vClass.Create;
    Result.Database := FDatabase;
    Result.Hostname := FHostname;
    Result.Username := FUsername;
    Result.Password := FPassword;
    Result.Port := FPort;
    Result.DatabaseType := FDatabaseType;
    Result.StorageOutPut := FStorageOutPut;
  end
  else
  begin
    raise Exception.Create(emDBLinkMissing);
  end;
end;

procedure TRALDBModule.OpenSQL(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vDB: TRALDBBase;
  vMem, vResult: TStream;
  vSQLCache: TRALDBSQLCache;
  vDBSQL : TRALDBSQL;
  vQuery: TDataSet;
  vNative: boolean;
  vContentType: StringRAL;
begin
  vDB := FindDatabaseDriver;
  try
    try
      if vDB <> nil then
      begin
        vMem := ARequest.Body.AsStream;
        try
          if (vMem <> nil) and (vMem.Size > 0) then
          begin
            vSQLCache := TRALDBSQLCache.Create;
            try
              vSQLCache.LoadFromStream(vMem);
              vDBSQL := vSQLCache.SQLList[0];

              if vDBSQL.DriverType = vDB.DriverType then
                vQuery := vDB.OpenNative(vDBSQL.SQL, vDBSQL.Params)
              else
                vQuery := vDB.OpenCompatible(vDBSQL.SQL, vDBSQL.Params);

              vResult := TMemoryStream.Create;
              try
                if (vDB.CanExportNative) and (vDBSQL.DriverType = vDB.DriverType) then
                begin
                  vContentType := rctAPPLICATIONOCTETSTREAM;
                  vNative := True;
                  vDB.SaveToStream(vQuery, vResult, vContentType, vNative);
                end
                else
                begin
                  vNative := False;
                  vContentType := FStorageOutPut.ContentType;
                  FStorageOutPut.SaveToStream(vQuery, vResult);
                end;
                vDBSQL.Response.Native := vNative;
                vDBSQL.Response.ContentType := vContentType;
                vDBSQL.Response.RowsAffected := 0;
                vDBSQL.Response.LastId := 0;
                vDBSQL.Response.Stream := vResult;
              finally
                FreeAndNil(vResult);
              end;

              vResult := vSQLCache.ResponseToStream;
              try
                AResponse.ContentType := rctAPPLICATIONOCTETSTREAM;
                AResponse.Params.AddParam('Stream', vResult, rpkBODY);
              finally
                FreeAndNil(vResult);
              end;
            finally
              FreeAndNil(vSQLCache);
            end;
          end
          else
          begin
            raise Exception.Create('Body está vazio');
          end;
        finally
          FreeAndNil(vMem);
        end;
      end
      else
      begin
        raise Exception.Create('Driver da Conexão não encontrado');
      end;
    except
      on e: Exception do
      begin
        AResponse.StatusCode := 500;
        AResponse.ContentType := rctTEXTPLAIN;
        AResponse.Params.AddParam('Exception', e.Message, rpkBODY);
      end;
    end;
  finally
    FreeAndNil(vDB);
  end;
end;

procedure TRALDBModule.ApplyUpdates(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vDB: TRALDBBase;
  vMem, vResult: TStream;
  vSQLCache: TRALDBSQLCache;
  vDBSQL: TRALDBSQL;
  vRowsAffect, vLastId: Int64RAL;
  vInt: IntegerRAL;
begin
  vRowsAffect := 0;
  vLastId := 0;
  vDB := FindDatabaseDriver;
  try
    try
      if vDB <> nil then
      begin
        vMem := ARequest.Body.AsStream;
        try
          if (vMem <> nil) and (vMem.Size > 0) then
          begin
            vSQLCache := TRALDBSQLCache.Create;
            try
              vSQLCache.LoadFromStream(vMem);
              for vInt := 0 to Pred(vSQLCache.Count) do
              begin
                vDBSQL := vSQLCache.SQLList[vInt];
                vDBSQL.Response.Clear;

                try
                  if vDBSQL.ExecType = etExecute then
                  begin
                    vDB.ExecSQL(vDBSQL.SQL, vDBSQL.Params, vRowsAffect, vLastId);

                    vDBSQL.Response.Native := False;
                    vDBSQL.Response.ContentType := rctAPPLICATIONOCTETSTREAM;
                    vDBSQL.Response.RowsAffected := vRowsAffect;
                    vDBSQL.Response.LastId := vLastId;
                  end
                  else
                  begin
                    if vDBSQL.DriverType = vDB.DriverType then
                      vQuery := vDB.OpenNative(vDBSQL.SQL, vDBSQL.Params)
                    else
                      vQuery := vDB.OpenCompatible(vDBSQL.SQL, vDBSQL.Params);

                    vResult := TMemoryStream.Create;
                    try
                      if (vDB.CanExportNative) and (vDBSQL.DriverType = vDB.DriverType) then
                      begin
                        vContentType := rctAPPLICATIONOCTETSTREAM;
                        vNative := True;
                        vDB.SaveToStream(vQuery, vResult, vContentType, vNative);
                      end
                      else
                      begin
                        vNative := False;
                        vContentType := FStorageOutPut.ContentType;
                        FStorageOutPut.SaveToStream(vQuery, vResult);
                      end;
                      vDBSQL.Response.Native := vNative;
                      vDBSQL.Response.ContentType := vContentType;
                      vDBSQL.Response.RowsAffected := 0;
                      vDBSQL.Response.LastId := 0;
                      vDBSQL.Response.Stream := vResult;
                    finally
                      FreeAndNil(vResult);
                    end;
                  end;
                except
                  on e : Exception do
                    vDBSQL.Response.StrError := e.Message;
                end;
              end;

              vResult := vSQLCache.ResponseToStream;
              try
                AResponse.ContentType := rctAPPLICATIONOCTETSTREAM;
                AResponse.Params.AddParam('Stream', vResult, rpkBODY);
              finally
                FreeAndNil(vResult);
              end;
            finally
              FreeAndNil(vSQLCache);
            end;
          end
          else
          begin
            raise Exception.Create('Body está vazio');
          end;
        finally
          FreeAndNil(vMem);
        end;
      end
      else
      begin
        raise Exception.Create('Driver da Conexão não encontrado');
      end;
    except
      on e: Exception do
      begin
        AResponse.StatusCode := 500;
        AResponse.ContentType := rctTEXTPLAIN;
        AResponse.Params.AddParam('Exception', e.Message, rpkBODY);
      end;
    end;
  finally
    FreeAndNil(vDB);
  end;
end;

procedure TRALDBModule.ExecSQL(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vDB: TRALDBBase;
  vMem, vResult: TStream;
  vSQLCache: TRALDBSQLCache;
  vDBSQL: TRALDBSQL;
  vRowsAffect, vLastId: Int64RAL;
begin
  vRowsAffect := 0;
  vLastId := 0;
  vDB := FindDatabaseDriver;
  try
    try
      if vDB <> nil then
      begin
        vMem := ARequest.Body.AsStream;
        try
          if (vMem <> nil) and (vMem.Size > 0) then
          begin
            vSQLCache := TRALDBSQLCache.Create;
            try
              vSQLCache.LoadFromStream(vMem);
              vDBSQL := vSQLCache.SQLList[0];
              vDBSQL.Response.Clear;

              vDB.ExecSQL(vDBSQL.SQL, vDBSQL.Params, vRowsAffect, vLastId);

              vDBSQL.Response.RowsAffected := vRowsAffect;
              vDBSQL.Response.LastId := vLastId;

              vResult := vSQLCache.ResponseToStream;
              try
                AResponse.ContentType := rctAPPLICATIONOCTETSTREAM;
                AResponse.Params.AddParam('Stream', vResult, rpkBODY);
              finally
                FreeAndNil(vResult);
              end;
            finally
              FreeAndNil(vSQLCache);
            end;
          end
          else
          begin
            raise Exception.Create('Body está vazio');
          end;
        finally
          FreeAndNil(vMem);
        end;
      end
      else
      begin
        raise Exception.Create('Driver da Conexão não encontrado');
      end;
    except
      on e: Exception do
      begin
        AResponse.StatusCode := 500;
        AResponse.ContentType := rctTEXTPLAIN;
        AResponse.Params.AddParam('Exception', e.Message, rpkBODY);
      end;
    end;
  finally
    FreeAndNil(vDB);
  end;
end;

procedure TRALDBModule.GetTables(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vDB: TRALDBBase;
  vSQL: TStringList;
  vSchema : StringRAL;
  vSystem: boolean;
  vQuery: TDataSet;
  vJSON: TRALJSONArray;
  vjObj: TRALJSONObject;
begin
  vDB := FindDatabaseDriver;
  try
    try
      if vDB <> nil then
      begin
        vSchema := ARequest.ParamByName('schema').AsString;
        vSystem := ARequest.ParamByName('system').AsBoolean;
        vQuery := nil;
        vJSON := nil;

        vSQL := TStringList.Create;
        try
          case FDatabaseType of
            dtFirebird : begin
              vSQL.Add('select rdb$relation_name, rdb$system_flag from rdb$relations');
              if not vSystem then
                vSQL.Add('where rdb$system_flag = 0');
              vSQL.Add('order by rdb$relation_name');
            end;
            dtSQLite : begin
              vSQL.Add('select name from sqlite_master');
              vSQL.Add('where type = ''table''');
            end;
            dtMySQL : begin
              vSQL.Add('show tables');
            end;
            dtPostgreSQL : begin
              vSQL.Add('select c.relname,');
              vSQL.Add('       case');
              vSQL.Add('         when (n.nspname = ''information_schema'') or');
              vSQL.Add('              (n.nspname = ''pg_catalog'') or (n.nspname = ''dbo'') or');
              vSQL.Add('              (n.nspname = ''sys'') or');
              vSQL.Add('              (substr(c.relname, 1, 3) = ''pg_'') then 1');
              vSQL.Add('         else 0');
              vSQL.Add('       end as systable, n.nspname');
              vSQL.Add('from pg_catalog.pg_class c');
              vSQL.Add('inner join pg_catalog.pg_namespace n on n.oid = c.relnamespace');
              vSQL.Add('where c.relkind = ''r''');
              if not vSystem then begin
                vSQL.Add('  and n.nspname <> ''information_schema'' and ');
                vSQL.Add('      n.nspname <> ''pg_catalog'' and n.nspname <> ''dbo'' and');
                vSQL.Add('      n.nspname <> ''sys'' and substr(c.relname, 1, 3) <> ''pg_''');
              end;
              if vSchema <> '' then
                vSQL.Add('  and lower(n.nspname) = '+QuotedStr(LowerCase(vSchema)));
            end;
          end;

          vQuery := vDB.OpenNative(vSQL.Text, nil);
          try
            AResponse.ContentType := rctAPPLICATIONJSON;
            vJSON := TRALJSONArray.Create;
            try
              if not vQuery.IsUniDirectional then
                vQuery.First;

              while not vQuery.Eof do begin
                vjObj := TRALJSONObject.Create;
                vjObj.Add('table_name', vQuery.Fields[0].AsString);
                if vQuery.FieldCount > 1 then
                  vjObj.Add('system_table', vQuery.Fields[1].AsInteger = 1)
                else
                  vjObj.Add('system_table', False);

                if vQuery.FieldCount > 2 then
                  vjObj.Add('schema_name', vQuery.Fields[2].AsString)
                else
                  vjObj.Add('schema_name', '');

                vJSON.Add(vjObj);

                vQuery.Next;
              end;

              AResponse.ResponseText := vJSON.ToJson;
            finally
              FreeAndNil(vJSON);
            end;
          finally
            FreeAndNil(vQuery);
          end;
        finally
          FreeAndNil(vSQL);
        end;
      end;
    except
      on e : Exception do
      begin
        AResponse.StatusCode := 500;
        AResponse.ContentType := rctTEXTPLAIN;
        AResponse.Params.AddParam('Exception', e.Message, rpkBODY);
      end;
    end;
  finally
    FreeAndNil(vDB);
  end;
end;

procedure TRALDBModule.GetFields(ARequest: TRALRequest; AResponse: TRALResponse);
type
  TInfoField = record
    column_name: StringRAL;
    schema_name: StringRAL;
    column_datatype: IntegerRAL;
    column_typename: StringRAL;
    column_attributes: StringRAL;
    column_precision: IntegerRAL;
    column_scale: IntegerRAL;
    column_length: IntegerRAL;
  end;
var
  vDB: TRALDBBase;
  vSQL: TStringList;
  vSchema, vTable: StringRAL;
  vQuery: TDataSet;
  vJSON: TRALJSONArray;
  vjObj: TRALJSONObject;
  vField: TInfoField;

  procedure AddFieldAttribute(AAttribute: StringRAL);
  begin
    if vField.column_attributes <> '' then
      vField.column_attributes := vField.column_attributes + ',';
    vField.column_attributes := vField.column_attributes + AAttribute;
  end;

  procedure AssignOthersDateTypeField(AType: StringRAL);
  var
    vInt: IntegerRAL;
  begin
    AType := LowerCase(AType);
    if (Pos('varchar', AType) > 0) or (Pos('char', AType) > 0) then
    begin
      vField.column_datatype := Ord(sftString);
      vField.column_typename := 'sftString';

      vInt := Pos('(', AType);
      if vInt > 0 then
      begin
        Delete(AType, 1, vInt);
        vInt := Pos(')', AType);
        vField.column_length := StrToInt(Copy(AType, 1, vInt-1));
      end
      else begin
        vField.column_length := 255;
      end;
    end
    else if (Pos('text', AType) > 0) or (Pos('json', AType) > 0) or
            (Pos('uuid', AType) > 0) then
    begin
      vField.column_datatype := Ord(sftMemo);
      vField.column_typename := 'sftMemo';
    end
    else if (Pos('binary', AType) > 0) or (Pos('blob', AType) > 0) then
    begin
      vField.column_datatype := Ord(sftBlob);
      vField.column_typename := 'sftBlob';
    end
    else if (Pos('date', AType) > 0) or (Pos('time', AType) > 0) then
    begin
      vField.column_datatype := Ord(sftDateTime);
      vField.column_typename := 'sftDateTime';
    end
    else if (Pos('double', AType) > 0) or (Pos('numeric', AType) > 0) or
            (Pos('decimal', AType) > 0) then
    begin
      vField.column_datatype := Ord(sftDouble);
      vField.column_typename := 'sftDouble';

      vInt := Pos('(', AType);
      if vInt > 0 then
      begin
        Delete(AType, 1, vInt);
        vInt := Pos(',', AType);

        vField.column_precision := StrToInt(Copy(AType, 1, vInt-1));

        Delete(AType, 1, vInt);
        vInt := Pos(')', AType);

        vField.column_scale := StrToInt(Copy(AType, 1, vInt-1));
      end
      else begin
        vField.column_precision := 15;
        vField.column_scale := 2;
      end;
    end
    else if (Pos('tinyint', AType) > 0) then
    begin
      if Pos('unsigned', AType) > 0 then
      begin
        vField.column_datatype := Ord(sftByte);
        vField.column_typename := 'sftByte';
      end
      else
      begin
        vField.column_datatype := Ord(sftShortInt);
        vField.column_typename := 'sftShortInt';
      end;
    end
    else if (Pos('smallint', AType) > 0) then
    begin
      if Pos('unsigned', AType) > 0 then
      begin
        vField.column_datatype := Ord(sftWord);
        vField.column_typename := 'sftWord';
      end
      else
      begin
        vField.column_datatype := Ord(sftSmallInt);
        vField.column_typename := 'sftSmallInt';
      end;
    end
    else if (Pos('bigint', AType) > 0) then
    begin
      if Pos('unsigned', AType) > 0 then
      begin
        vField.column_datatype := Ord(sftQWord);
        vField.column_typename := 'sftQWord';
      end
      else
      begin
        vField.column_datatype := Ord(sftInt64);
        vField.column_typename := 'sftInt64';
      end;
    end
    else if (Pos('int', AType) > 0) or (Pos('integer', AType) > 0) then
    begin
      if Pos('unsigned', AType) > 0 then
      begin
        vField.column_datatype := Ord(sftCardinal);
        vField.column_typename := 'sftCardinal';
      end
      else
      begin
        vField.column_datatype := Ord(sftInteger);
        vField.column_typename := 'sftInteger';
      end;
    end;
  end;

  procedure AssignFirebirdDateTypeField;
  var
    vfbType : TRALFieldType;
  begin
    case vQuery.FieldByName('rdb$field_type').AsInteger of
      007: begin
            vfbType := sftSmallInt;
            if vQuery.FieldByName('rdb$field_sub_type').AsInteger > 0 then
              vfbType := sftDouble;
      end;
      008: begin
            vfbType := sftInteger;
            if vQuery.FieldByName('rdb$field_sub_type').AsInteger > 0 then
              vfbType := sftDouble;
      end;
      009: vfbType := sftInt64;
      010: vfbType := sftDouble;
      011: vfbType := sftDouble;
      012: vfbType := sftDateTime;
      013: vfbType := sftDateTime;
      014: vfbType := sftString;
      016: begin
            vfbType := sftInt64;
            if vQuery.FieldByName('rdb$field_sub_type').AsInteger > 0 then
              vfbType := sftDouble;
      end;
      027: vfbType := sftDouble;
      035: vfbType := sftDateTime;
      037: vfbType := sftString;
      040: vfbType := sftString;
      261: begin
        vfbType := sftBlob;
        if vQuery.FieldByName('rdb$field_sub_type').AsInteger = 1 then
          vfbType := sftMemo;
      end;
    end;

    vField.column_datatype := Ord(vfbType);
    vField.column_typename := GetEnumName(TypeInfo(TRALFieldType), Ord(vfbType));

    if vQuery.FieldByName('rdb$null_flag').AsString = '0' then
      AddFieldAttribute('not_null');

    if vQuery.FieldByName('pk').AsInteger > 0 then
      AddFieldAttribute('pk');

    if vQuery.FieldByName('rdb$field_type').AsInteger in [14, 37, 40] then
    begin
      vField.column_length := vQuery.FieldByName('rdb$field_length').AsInteger;
      // field com charset e colation
      if (vQuery.FieldByName('rdb$character_length').AsInteger > 0) and
         (vQuery.FieldByName('rdb$character_length').AsInteger < vField.column_length) then
        vField.column_length := vQuery.FieldByName('rdb$character_length').AsInteger;
    end
    else if vQuery.FieldByName('rdb$field_type').AsInteger in [7, 8, 16, 27] then
    begin
      // numeric
      if vfbType = sftDouble then
        vField.column_precision := vQuery.FieldByName('rdb$field_precision').AsInteger;

      if (vQuery.FieldByName('rdb$field_scale').AsInteger < 0) then
      begin
        vField.column_precision := 15;
        if (vQuery.FieldByName('rdb$field_precision').AsInteger > 0) then
          vField.column_precision := vQuery.FieldByName('rdb$field_precision').AsInteger;
        vField.column_scale := Abs(vQuery.FieldByName('rdb$field_scale').AsInteger);
      end;
    end;
  end;

  procedure AssignPostgresDateTypeField;
  var
    vpgType: TRALFieldType;
    vType, vTypMod: IntegerRAL;
  begin
    if vQuery.FieldByName('attnotnull').AsBoolean then
      AddFieldAttribute('not_null');

    if vQuery.FieldByName('pk').AsInteger > 0 then
      AddFieldAttribute('pk');

    vType := vQuery.FieldByName('typbasetype').AsInteger; // campos com domains
    vTypMod := vQuery.FieldByName('typtypmod').AsInteger;
    if vType = 0 then
    begin
      vType := vQuery.FieldByName('atttypid').AsInteger; // campos sem domain
      vTypMod := vQuery.FieldByName('atttypmod').AsInteger;
    end;

    case vType of
      23   : vpgType := sftInteger;
      21   : vpgType := sftSmallInt;
      20   : vpgType := sftInt64;
      16   : vpgType := sftBoolean;
      26   : vpgType := sftInt64;
      1700 : vpgType := sftDouble;
      701  : vpgType := sftDouble;
      1043 : vpgType := sftString;
      1042 : vpgType := sftString;
      1114 : vpgType := sftDateTime;
      17   : vpgType := sftBlob;
      25   : vpgType := sftMemo;
      1082 : vpgType := sftDateTime;
      1184 : vpgType := sftDateTime;
    end;

    vField.column_datatype := Ord(vpgType);
    vField.column_typename := GetEnumName(TypeInfo(TRALFieldType), Ord(vpgType));

    if (vType <> 1700) and (vTypMod > 0) then begin
      vField.column_length := vTypMod - 4;
      if vField.column_length < 0 then
        vField.column_length := 0;
    end
    else if (vType = 1700) then begin
      vField.column_precision := (vTypMod - 4) mod 65536;
      vField.column_scale := (vTypMod - 4) div 65536;
    end;
  end;

begin
  vDB := FindDatabaseDriver;
  try
    try
      if vDB <> nil then
      begin
        vSchema := ARequest.ParamByName('schema').AsString;
        vTable := ARequest.ParamByName('table').AsString;
        vQuery := nil;
        vJSON := nil;

        vSQL := TStringList.Create;
        try
          case FDatabaseType of
            dtFirebird: begin
              vTable := UpperCase(vTable);

              vSQL.Add('select f.rdb$field_type, f.rdb$field_sub_type, f.rdb$field_length,');
              vSQL.Add('       f.rdb$character_length, f.rdb$field_precision,');
              vSQL.Add('       f.rdb$field_scale, rf.rdb$field_name, rf.rdb$null_flag,');
              vSQL.Add('       rf.rdb$default_source, cs.rdb$character_set_name,');
              vSQL.Add('       cl.rdb$collation_name, fd.rdb$lower_bound, fd.rdb$upper_bound,');
              vSQL.Add('      (select count(*) as conta');
              vSQL.Add('       from rdb$relation_constraints c');
              vSQL.Add('       inner join rdb$index_segments s on s.rdb$index_name = c.rdb$index_name');
              vSQL.Add('       where c.rdb$relation_name = rf.rdb$relation_name and');
              vSQL.Add('             s.rdb$field_name = rf.rdb$field_name and');
              vSQL.Add('             c.rdb$constraint_type = ''PRIMARY KEY'') as pk');
              vSQL.Add('from rdb$fields f');
              vSQL.Add('left join rdb$relation_fields rf on rf.rdb$field_source = f.rdb$field_name');
              vSQL.Add('left join rdb$character_sets cs on cs.rdb$character_set_id = f.rdb$character_set_id');
              vSQL.Add('left join rdb$collations cl on cl.rdb$character_set_id = f.rdb$character_set_id and');
              vSQL.Add('     cl.rdb$collation_id = coalesce(f.rdb$collation_id,rf.rdb$collation_id)');
              vSQL.Add('left join rdb$field_dimensions fd on fd.rdb$field_name = f.rdb$field_name');
              vSQL.Add('where rf.rdb$relation_name = '+QuotedStr(vTable));
              vSQL.Add('order by rf.rdb$field_position');
            end;
            dtSQLite: begin
              vSQL.Add('pragma table_info('+vTable+')');
            end;
            dtMySQL: begin
              vSQL.Add('show columns from '+vTable);
            end;
            dtPostgreSQL: begin
              vSQL.Add('select t.typbasetype, t.typtypmod, a.atttypid, a.atttypmod,');
              vSQL.Add('       a.attname, a.attnotnull, n.nspname,');
              vSQL.Add('       pg_get_expr(d.adbin, d.adrelid) as pg_default,');
              vSQL.Add('	    (select count(*) from pg_catalog.pg_index i  ');
              vSQL.Add('       inner join pg_catalog.pg_attribute aa on aa.attrelid = i.indrelid and');
              vSQL.Add('		         aa.attnum = any(i.indkey) and aa.attname = a.attname');
              vSQL.Add('		   where i.indrelid = c.oid and i.indisprimary) as pk');
              vSQL.Add('from pg_catalog.pg_class c');
              vSQL.Add('inner join pg_catalog.pg_namespace n on n.oid = c.relnamespace');
              vSQL.Add('inner join pg_catalog.pg_attribute a on a.attrelid = c.oid');
              vSQL.Add('inner join pg_catalog.pg_type t on a.atttypid = t.oid');
              vSQL.Add('left join pg_catalog.pg_attrdef d on d.adnum = a.attnum and d.adrelid = c.oid');
              vSQL.Add('where a.attnum > 0 and not a.attisdropped and');
              vSQL.Add('      lower(c.relname) = '+QuotedStr(LowerCase(vTable)));
              if vSchema <> '' then
                vSQL.Add('  and lower(n.nspname) = '+QuotedStr(LowerCase(vSchema)));
            end;
          end;

          vQuery := vDB.OpenNative(vSQL.Text, nil);
          try
            AResponse.ContentType := rctAPPLICATIONJSON;
            vJSON := TRALJSONArray.Create;
            try
              if not vQuery.IsUniDirectional then
                vQuery.First;

              while not vQuery.Eof do begin
                vjObj := TRALJSONObject.Create;

                vField.column_name := '';
                vField.schema_name := '';
                vField.column_datatype := -1;
                vField.column_typename := '';
                vField.column_attributes := '';
                vField.column_precision := 0;
                vField.column_scale := 0;
                vField.column_length := 0;

                case FDatabaseType of
                  dtFirebird: begin
                    vField.column_name := vQuery.FieldByName('rdb$field_name').AsString;
                    AssignFirebirdDateTypeField;
                  end;
                  dtSQLite: begin
                    vField.column_name := vQuery.Fields[1].AsString;
                    AssignOthersDateTypeField(vQuery.Fields[2].AsString);
                    if vQuery.Fields[3].AsInteger = 1 then
                      AddFieldAttribute('not_null');
                    if vQuery.Fields[5].AsInteger = 1 then
                      AddFieldAttribute('pk');
                  end;
                  dtMySQL: begin
                    vField.column_name := vQuery.Fields[0].AsString;
                    AssignOthersDateTypeField(vQuery.Fields[1].AsString);
                    if vQuery.Fields[2].AsString = 'NO' then
                      AddFieldAttribute('not_null');
                    if vQuery.Fields[3].AsString = 'PRI' then
                      AddFieldAttribute('pk');
                  end;
                  dtPostgreSQL : begin
                    vField.column_name := vQuery.FieldByName('attname').AsString;
                    vField.schema_name := vQuery.FieldByName('nspname').AsString;
                    AssignPostgresDateTypeField;
                  end;
                end;

                vjObj.Add('table_name', vTable);
                vjObj.Add('column_name', vField.column_name);
                vjObj.Add('schema_name', vField.schema_name);
                vjObj.Add('column_datatype', vField.column_datatype);
                vjObj.Add('column_typename', vField.column_typename);
                vjObj.Add('column_attributes', vField.column_attributes);
                vjObj.Add('column_precision', vField.column_precision);
                vjObj.Add('column_scale', vField.column_scale);
                vjObj.Add('column_length', vField.column_length);

                vJSON.Add(vjObj);

                vQuery.Next;
              end;

              AResponse.ResponseText := vJSON.ToJson;
            finally
              FreeAndNil(vJSON);
            end;
          finally
            FreeAndNil(vQuery);
          end;
        finally
          FreeAndNil(vSQL);
        end;
      end;
    except
      on e : Exception do
      begin
        AResponse.StatusCode := 500;
        AResponse.ContentType := rctTEXTPLAIN;
        AResponse.Params.AddParam('Exception', e.Message, rpkBODY);
      end;
    end;
  finally
    FreeAndNil(vDB);
  end;
end;

constructor TRALDBModule.Create(AOwner: TComponent);
var
  vRoute: TRALRoute;
  vParam: TRALRouteParam;
begin
  inherited Create(AOwner);
  vRoute := CreateRoute('opensql', {$IFDEF FPC}@{$ENDIF}OpenSQL);
  vRoute.Name := 'opensql';
  vRoute.AllowedMethods := [amPOST, amOPTIONS];
  vRoute.Description.Add('Open a SQL from a client http');

  vRoute := CreateRoute('execsql', {$IFDEF FPC}@{$ENDIF}ExecSQL);
  vRoute.Name := 'execsql';
  vRoute.AllowedMethods := [amPOST, amOPTIONS];
  vRoute.Description.Add('Execute a SQL from a client http');

  vRoute := CreateRoute('applyupdates', {$IFDEF FPC}@{$ENDIF}ApplyUpdates);
  vRoute.Name := 'applyupdates';
  vRoute.AllowedMethods := [amPOST, amOPTIONS];
  vRoute.Description.Add('Apply Update from a client http');

  vRoute := CreateRoute('gettables', {$IFDEF FPC}@{$ENDIF}GetTables);
  vRoute.Name := 'gettables';
  vRoute.AllowedMethods := [amGET, amOPTIONS];
  vRoute.Description.Add('List all tables in a database');

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := 'Schema of database';
  vParam.ParamName := 'schema';
  vParam.ParamType := prtString;
  vParam.Required := False;

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := 'Include system tables';
  vParam.ParamName := 'system';
  vParam.ParamType := prtBoolean;
  vParam.Required := False;

  vRoute := CreateRoute('getfields', {$IFDEF FPC}@{$ENDIF}GetFields);
  vRoute.Name := 'getfields';
  vRoute.AllowedMethods := [amGET, amOPTIONS];
  vRoute.Description.Add('List all fields in a table');

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := 'Schema of database';
  vParam.ParamName := 'schema';
  vParam.ParamType := prtString;
  vParam.Required := False;

  vParam := TRALRouteParam(vRoute.InputParams.Add);
  vParam.Description.Text := 'Table name';
  vParam.ParamName := 'table';
  vParam.ParamType := prtString;
  vParam.Required := True;
end;

end.
