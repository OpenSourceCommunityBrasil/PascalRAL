unit RALDBWare;

interface

uses
  Classes, SysUtils, DB,
  RALServer, RALRequest, RALResponse, RALDBBase, RALParams, RALMIMETypes,
  RALConsts, RALTypes, RALJSON, RALStorage, RALBase64;

type
  { TRALDBWare }

  TRALDBWare = class(TRALSubRoutes)
  private
    FDatabase : StringRAL;
    FHostname : StringRAL;
    FUsername : StringRAL;
    FPassword : StringRAL;
    FPort     : IntegerRAL;

    FDataBaseLink : TRALDBLink;
    FDatabaseType : TRALDatabaseType;
    FStorageOutPut : TRALStorageLink;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDataBaseLink(AValue : TRALDBLink);
    procedure SetStorageOutPut(AValue : TRALStorageLink);

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

    property DataBaseLink : TRALDBLink read FDataBaseLink write SetDataBaseLink;
    property DatabaseType  : TRALDatabaseType read FDatabaseType write FDatabaseType;
    property StorageOutPut : TRALStorageLink read FStorageOutPut write SetStorageOutPut;
  end;

implementation

{ TRALDBWare }

procedure TRALDBWare.SetStorageOutPut(AValue : TRALStorageLink);
begin
  if AValue <> FStorageOutPut then
    FStorageOutPut := AValue;

  if FStorageOutPut <> nil then
    FStorageOutPut.FreeNotification(Self);
end;

procedure TRALDBWare.Notification(AComponent : TComponent; Operation : TOperation);
begin
  if (Operation = opRemove) and (AComponent = FDataBaseLink) then
    FDataBaseLink := nil
  else if (Operation = opRemove) and (AComponent = FStorageOutPut) then
    FStorageOutPut := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TRALDBWare.SetDataBaseLink(AValue : TRALDBLink);
begin
  if AValue <> FDataBaseLink then
    FDataBaseLink := AValue;

  if FDataBaseLink <> nil then
    FDataBaseLink.FreeNotification(Self);
end;

function TRALDBWare.FindDatabaseDriver : TRALDBBase;
var
  vClass : TRALDBClass;
  vUnit : StringRAL;
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
    Result.Port     := FPort;
    Result.DatabaseType := FDatabaseType;
    Result.StorageOutPut := FStorageOutPut;
  end
  else begin
    raise Exception.Create('Propriedade DBLink deve ser informada');
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
  vResult : TStream;
  vString : StringRAL;
  vInt : IntegerRAL;
begin
  vDB := FindDatabaseDriver;
  try
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
          try
            if vString <> '' then
            begin
              AResponse.ContentType := rctAPPLICATIONJSON;
              vString := Format('{"erro":"%s"}', [vString]);
              vResult.Write(vString[PosIniStr], Length(vString));
            end
            else
            begin
              AResponse.ContentType := FStorageOutPut.ContentType;
              FStorageOutPut.SaveDataset(vQuery, vResult);
            end;
            vResult.Position := 0;
            AResponse.ResponseStream := vResult;
          finally
            FreeAndNil(vResult)
          end;
        finally
          FreeAndNil(vParams);
          FreeAndNil(vQuery);
        end;
      end
      else begin
        AResponse.Answer(404, RAL404Page);
      end;
    end;
  finally
    FreeAndNil(vDB);
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
  try
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
          try
            if vParam.ContentType = rctAPPLICATIONJSON then
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
            FreeAndNil(vResult);
          end;
        finally
          FreeAndNil(vParams);
        end;
      end
      else begin
        AResponse.Answer(404, RAL404Page);
      end;
    end;
  finally
    FreeAndNil(vDB);
  end;
end;

constructor TRALDBWare.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  CreateRoute('opensql',{$IFDEF FPC}@{$ENDIF}OpenSQL);
  CreateRoute('execsql',{$IFDEF FPC}@{$ENDIF}ExecSQL);
end;

end.

