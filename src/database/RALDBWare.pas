unit RALDBWare;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB,
  RALServer, RALRequest, RALResponse, RALDBBase, RALParams, RALMIMETypes,
  RALConsts, RALTypes, RALJSON, RALDatasetStorage, RALDatasetStorageJson,
  RALBase64;

type
  {$IFDEF FPC}
    TRALConnectionType = (ctLazarus, ctZeos);
  {$ELSE}
    TRALConnectionType = (ctFiredac, ctZeos);
  {$ENDIF}

  { TRALDBWare }

  TRALDBWare = class(TRALSubRoutes)
  private
    FDatabase : StringRAL;
    FHostname : StringRAL;
    FUsername : StringRAL;
    FPassword : StringRAL;
    FPort     : IntegerRAL;

    FConnectionType : TRALConnectionType;
    FDatabaseType : TRALDatabaseType;
    FDatabaseOutPut : TRALDatabaseOutPut;
  protected
    function FindDatabaseDriver : TRALDBBase;
    procedure RALParamJSONToQuery(ARALParam : TRALParam; var ASQL : StringRAL; var AParams : TParams);
    procedure RALParamBinaryToQuery(ARALParam : TRALParam; var ASQL : StringRAL; var AParams : TParams);

    procedure OpenSQL(Sender : TObject; ARequest : TRALRequest; AResponse : TRALResponse);
    procedure ExecSQL(Sender : TObject; ARequest : TRALRequest; AResponse : TRALResponse);
  public
    constructor Create(AOwner : TComponent); override;
  published
    property Database : StringRAL read FDatabase write FDatabase;
    property Hostname : StringRAL read FHostname write FHostname;
    property Username : StringRAL read FUsername write FUsername;
    property Password : StringRAL read FPassword write FPassword;
    property Port     : IntegerRAL read FPort write FPort;

    property ConnectionType : TRALConnectionType read FConnectionType write FConnectionType;
    property DatabaseType   : TRALDatabaseType read FDatabaseType write FDatabaseType;
    property DatabaseOutPut : TRALDatabaseOutPut read FDatabaseOutPut write FDatabaseOutPut;
  end;

implementation

{ TRALDBWare }

function TRALDBWare.FindDatabaseDriver : TRALDBBase;
var
  vClass : TRALDBClass;
  vUnit : StringRAL;
begin
  Result := nil;
  vClass := nil;
  case FConnectionType of
    {$IFDEF FPC}
      ctLazarus : begin
        vClass := TRALDBClass(GetClass('TRALDBLazarus'));
        vUnit  := 'RALDBLazarus';
      end;
    {$ELSE}
      ctFiredac : begin
        vClass := TRALDBClass(GetClass('TRALDBFiredac'));
        vUnit  := 'RALDBFiredac';
      end;
    {$ENDIF}
    ctZeos    : begin
      vClass := TRALDBClass(GetClass('TRALDBZeos'));
      vUnit  := 'RALDBZeos';
    end;
  end;

  if vClass <> nil then
  begin
    Result := vClass.Create;
    Result.Database := FDatabase;
    Result.Hostname := FHostname;
    Result.Username := FUsername;
    Result.Password := FPassword;
    Result.Port     := FPort;
    Result.DatabaseType := FDatabaseType;
    Result.DatabaseOutPut := FDatabaseOutPut;
  end
  else begin
    raise Exception.Create(Format('Unit %s must be inserted',[vUnit]));
  end;
end;

procedure TRALDBWare.RALParamJSONToQuery(ARALParam : TRALParam; var ASQL : StringRAL; var AParams : TParams);
var
  vJSON, vObj : TRALJSONObject;
  vParams : TRALJSONArray;
  vInt : integer;
  vParam : TParam;
begin
  vJSON := TRALJSONObject(TRALJSON.ParseJSON(ARALParam.AsString));
  try
    ASQL := vJSON.Get('sql').AsString;

    AParams := TParams.Create(nil);

    vParams := TRALJSONArray(vJSON.Get('params'));
    for vInt := 0 to Pred(vParams.Count) do begin
      vObj := TRALJSONObject(vParams.Get(vInt));

      vParam := TParam(AParams.Add);
      vParam.Name := vObj.Get('name').AsString;
      vParam.Size := vObj.Get('size').AsInteger;
      vParam.DataType := TFieldType(vObj.Get('type').AsInteger);
    end;
  finally
    vJSON.Free;
  end;
end;

procedure TRALDBWare.RALParamBinaryToQuery(ARALParam : TRALParam; var ASQL : StringRAL; var AParams : TParams);
begin

end;

procedure TRALDBWare.OpenSQL(Sender : TObject; ARequest : TRALRequest; AResponse : TRALResponse);
var
  vDB : TRALDBBase;
  vParam : TRALParam;
  vSQL : StringRAL;
  vParams : TParams;
  vQuery : TDataSet;
  vStor : TRALDatasetStorage;
  vResult : TStream;
  vString : StringRAL;
  vInt : IntegerRAL;
begin
  vDB := FindDatabaseDriver;
  if vDB <> nil then
  begin
    vParam := ARequest.ParamByName('query');

    if vParam = nil then
      vParam := ARequest.Body;

    if vParam <> nil then
    begin
      vSQL := '';
      vParams := nil;
      vQuery := nil;
      vString := '';
      vResult := nil;

      try
        if vParam.ContentType = rctAPPLICATIONJSON then
          RALParamJSONToQuery(vParam, vSQL, vParams)
        else
          RALParamBinaryToQuery(vParam, vSQL, vParams);

        try
          vQuery := vDB.Open(vSQL, vParams);
        except
          on e : Exception do
          begin
            vString := TRALBase64.Encode(e.Message);
          end;
        end;

        vResult := TMemoryStream.Create;
        if FDatabaseOutPut = dopJSON then
        begin
          AResponse.ContentType := rctAPPLICATIONJSON;
          if vString <> '' then
          begin
            vString := Format('{"erro":"%s"}', [vString]);
            vResult.Write(vString[PosIniStr], Length(vString));
          end
          else
          begin
            vString := '{"result":';
            vResult.Write(vString[PosIniStr], Length(vString));

            vStor := TRALDatasetStorageJSON.Create(nil);
            try
              vStor.SaveToStream(vQuery, vResult);
            finally
              FreeAndNil(vStor);
            end;

            vString := '}';
            vResult.Write(vString[PosIniStr], Length(vString));
          end;
        end
        else
        begin
          AResponse.ContentType := rctAPPLICATIONOCTETSTREAM;

          if vString <> '' then
          begin
            vInt := Length(vString);
            vResult.Write(vInt, SizeOf(vInt));
            vResult.Write(vString[PosIniStr], Length(vString));
          end
          else begin
            vInt := 0;
            vResult.Write(vInt, SizeOf(vInt));

            vStor := TRALDatasetStorageJSON.Create(nil);
            try
              vStor.SaveToStream(vQuery, vResult);
            finally
              FreeAndNil(vStor);
            end;
          end;
        end;

        vResult.Position := 0;
        AResponse.ResponseStream := vResult;
      finally
        FreeAndNil(vParams);
        FreeAndNil(vQuery);
        FreeAndNil(vResult)
      end;
    end
    else begin
      AResponse.Answer(404, RAL404Page);
    end;
  end;
end;

procedure TRALDBWare.ExecSQL(Sender : TObject; ARequest : TRALRequest; AResponse : TRALResponse);
var
  vDB : TRALDBBase;
  vParam : TRALParam;
  vSQL : StringRAL;
  vParams : TParams;
  vString : StringRAL;
  vResult : TStream;
  vInt : IntegerRAL;
  vRowsAffect, vLastId : Int64RAL;
begin
  vDB := FindDatabaseDriver;
  if vDB <> nil then
  begin
    vParam := ARequest.ParamByName('query');

    if vParam = nil then
      vParam := ARequest.Body;

    if vParam <> nil then
    begin
      vString := '';
      vRowsAffect := 0;
      vLastId := 0;
      try
        if vParam.ContentType = rctAPPLICATIONJSON then
          RALParamJSONToQuery(vParam, vSQL, vParams)
        else
          RALParamBinaryToQuery(vParam, vSQL, vParams);

        try
          vDB.ExecSQL(vSQL, vParams, vRowsAffect, vLastId);
        except
          on e : Exception do
          begin
            vString := TRALBase64.Encode(e.Message);
          end;
        end;

        vResult := TMemoryStream.Create;
        if FDatabaseOutPut = dopJSON then
        begin
          AResponse.ContentType := rctAPPLICATIONJSON;
          if vString <> '' then
          begin
            vString := Format('{"erro":"%s"}', [vString]);
            vResult.Write(vString[PosIniStr], Length(vString));
          end
          else
          begin
            vString := Format('{"rows":%d,"lastid":%d}', [vRowsAffect, vLastId]);
            vResult.Write(vString[PosIniStr], Length(vString));
          end;
        end
        else
        begin
          AResponse.ContentType := rctAPPLICATIONOCTETSTREAM;

          if vString <> '' then
          begin
            vInt := Length(vString);
            vResult.Write(vInt, SizeOf(vInt));
            vResult.Write(vString[PosIniStr], Length(vString));
          end
          else begin
            vInt := 0;
            vResult.Write(vInt, SizeOf(vInt));
          end;
          vResult.Write(vRowsAffect, SizeOf(vRowsAffect));
          vResult.Write(vLastId, SizeOf(vLastId));
        end;

        vResult.Position := 0;
        AResponse.ResponseStream := vResult;
      finally
        vParams.Free;
      end;
    end
    else begin
      AResponse.Answer(404, RAL404Page);
    end;
  end;
end;

constructor TRALDBWare.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  CreateRoute('opensql',{$IFDEF FPC}@{$ENDIF}OpenSQL);
  CreateRoute('execsql',{$IFDEF FPC}@{$ENDIF}ExecSQL);
end;

end.

