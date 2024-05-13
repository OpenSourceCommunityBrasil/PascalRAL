unit RALDBModule;

interface

uses
  Classes, SysUtils, DB,
  RALServer, RALRequest, RALResponse, RALDBBase, RALParams, RALMIMETypes,
  RALConsts, RALTypes, RALDBStorage, RALBase64, RALQueryStructure,
  RALRoutes;

type
  { TRALDBModule }

  TRALDBModule = class(TRALModuleRoutes)
  private
    FDatabase: StringRAL;
    FHostname: StringRAL;
    FUsername: StringRAL;
    FPassword: StringRAL;
    FPort: IntegerRAL;

    FDataBaseLink: TRALDBLink;
    FDatabaseType: TRALDatabaseType;
    FStorageOutPut: TRALDBStorageLink;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDataBaseLink(AValue: TRALDBLink);
    procedure SetStorageOutPut(AValue: TRALDBStorageLink);

    function FindDatabaseDriver: TRALDBBase;
    procedure RALParamJSONToQuery(ARALParam: TRALParam; var ASQL: StringRAL;
                                  var AParams: TParams; var AType : TRALDBDriverType);
    procedure RALParamBinaryToQuery(ARALParam: TRALParam; var ASQL: StringRAL;
                                    var AParams: TParams; var AType : TRALDBDriverType);

    procedure OpenSQL(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure ExecSQL(ARequest: TRALRequest; AResponse: TRALResponse);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Database: StringRAL read FDatabase write FDatabase;
    property Hostname: StringRAL read FHostname write FHostname;
    property Username: StringRAL read FUsername write FUsername;
    property Password: StringRAL read FPassword write FPassword;
    property Port: IntegerRAL read FPort write FPort;

    property DataBaseLink: TRALDBLink read FDataBaseLink write SetDataBaseLink;
    property DatabaseType: TRALDatabaseType read FDatabaseType write FDatabaseType;
    property StorageOutPut: TRALDBStorageLink read FStorageOutPut write SetStorageOutPut;
  end;

implementation

{ TRALDBModule }

procedure TRALDBModule.SetStorageOutPut(AValue: TRALDBStorageLink);
begin
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

procedure TRALDBModule.RALParamJSONToQuery(ARALParam: TRALParam; var ASQL: StringRAL;
                                           var AParams: TParams; var AType : TRALDBDriverType);
var
  vQryStruc : TRALQueryStructure;
begin
  vQryStruc := TRALQueryStructure.Create;
  try
    vQryStruc.ImportFromJSON(ARALParam.AsStream, ASQL, AParams, AType);
  finally
    FreeAndNil(vQryStruc);
  end;
end;

procedure TRALDBModule.RALParamBinaryToQuery(ARALParam: TRALParam; var ASQL: StringRAL;
                                             var AParams: TParams; var AType : TRALDBDriverType);
var
  vQryStruc : TRALQueryStructure;
begin
  vQryStruc := TRALQueryStructure.Create;
  try
    vQryStruc.ImportFromBinary(ARALParam.AsStream, ASQL, AParams, AType);
  finally
    FreeAndNil(vQryStruc);
  end;
end;

procedure TRALDBModule.OpenSQL(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vDB: TRALDBBase;
  vParam: TRALParam;
  vType : TRALDBDriverType;
  vSQL: StringRAL;
  vParams: TParams;
  vQuery: TDataSet;
  vResult: TStream;
  vString, vContentType: StringRAL;
  vNative: boolean;
begin
  vDB := FindDatabaseDriver;
  try
    if vDB <> nil then
    begin
      vParam := ARequest.Body;

      if vParam <> nil then
      begin
        vSQL := '';
        vParams := nil;
        vQuery := nil;
        vString := '';
        vResult := nil;

        try
          if Pos(rctAPPLICATIONJSON, vParam.ContentType) > 0 then
            RALParamJSONToQuery(vParam, vSQL, vParams, vType)
          else
            RALParamBinaryToQuery(vParam, vSQL, vParams, vType);

          try
            if vType = vDB.DriverName then
              vQuery := vDB.OpenNative(vSQL, vParams)
            else
              vQuery := vDB.OpenCompatible(vSQL, vParams);
          except
            on e: Exception do
            begin
              vString := TRALBase64.Encode(e.Message);
            end;
          end;

          try
            if vString <> '' then
            begin
              AResponse.StatusCode := 500;
              AResponse.ContentType := rctAPPLICATIONJSON;
              AResponse.Params.AddParam('Exception', vString, rpkBODY);
            end
            else
            begin
              vResult := TMemoryStream.Create;
              if (vDB.CanExportNative) and (vType = vDB.DriverName) then
              begin
                vContentType := vParam.ContentType;
                vNative := True;
                vDB.SaveToStream(vQuery, vResult, vContentType, vNative);
              end
              else
              begin
                vNative := False;
                vContentType := FStorageOutPut.ContentType;
                FStorageOutPut.SaveToStream(vQuery, vResult);
              end;
              AResponse.ContentType := vContentType;
              AResponse.Params.AddParam('Stream', vResult, rpkBODY);
              AResponse.Params.AddParam('RowsAffected', '0', rpkBODY);
              AResponse.Params.AddParam('ResultType', IntToStr(Ord(vNative)), rpkBODY);
            end;
          finally
            FreeAndNil(vResult);
          end;
        finally
          FreeAndNil(vParams);
          FreeAndNil(vQuery);
        end;
      end
      else
      begin
        AResponse.Answer(404);
      end;
    end;
  finally
    FreeAndNil(vDB);
  end;
end;

procedure TRALDBModule.ExecSQL(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vDB: TRALDBBase;
  vParam: TRALParam;
  vType: TRALDBDriverType;
  vSQL: StringRAL;
  vParams: TParams;
  vString: StringRAL;
  vInt: IntegerRAL;
  vRowsAffect, vLastId: Int64RAL;
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
          if Pos(rctAPPLICATIONJSON, vParam.ContentType) > 0 then
            RALParamJSONToQuery(vParam, vSQL, vParams, vType)
          else
            RALParamBinaryToQuery(vParam, vSQL, vParams, vType);

          try
            vDB.ExecSQL(vSQL, vParams, vRowsAffect, vLastId);
          except
            on e: Exception do
            begin
              vString := e.Message;
            end;
          end;

          if vString <> '' then
          begin
            AResponse.StatusCode := 500;
            AResponse.ContentType := rctAPPLICATIONJSON;
            AResponse.Params.AddParam('Exception', vString, rpkBODY);
          end
          else
          begin
            AResponse.ContentType := rctAPPLICATIONOCTETSTREAM;
            AResponse.StatusCode := 200;

            AResponse.Params.AddParam('RowsAffected', IntToStr(vRowsAffect), rpkBODY);
            AResponse.Params.AddParam('LastID', IntToStr(vLastId), rpkBODY);
          end;
        finally
          FreeAndNil(vParams);
        end;
      end
      else
      begin
        AResponse.Answer(404);
      end;
    end;
  finally
    FreeAndNil(vDB);
  end;
end;

constructor TRALDBModule.Create(AOwner: TComponent);
var
  vRoute : TRALRoute;
begin
  inherited Create(AOwner);
  vRoute := CreateRoute('opensql', {$IFDEF FPC}@{$ENDIF}OpenSQL);
  vRoute.Name := 'opensql';
  vRoute.AllowedMethods := [amPOST];
  vRoute.Description.Add('Open a SQL from a client http');

  vRoute := CreateRoute('execsql', {$IFDEF FPC}@{$ENDIF}ExecSQL);
  vRoute.Name := 'execsql';
  vRoute.AllowedMethods := [amPOST];
  vRoute.Description.Add('Execute a SQL from a client http');
end;

end.
