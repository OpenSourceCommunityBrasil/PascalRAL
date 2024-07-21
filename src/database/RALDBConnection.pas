unit RALDBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  RALCustomObjects, RALTypes, RALTools, RALDBSQLCache, RALClient, RALResponse,
  RALDBTypes, RALRequest, RALMimeTypes;

type

  { TRALDBConnection }

  TRALDBConnection = class(TRALComponent)
  private
    FClient: TRALClientMT;
    FModuleRoute: StringRAL;
  protected
    procedure SetClient(AValue: TRALClientMT);
    procedure SetModuleRoute(AValue: StringRAL);
  public
    constructor Create(AOwner : TComponent);

    procedure ApplyUpdatesRemote(ACache: TRALDBSQLCache; AResp: TRALThreadClientResponse);
    procedure OpenRemote(AQuery: TDataset; AResp: TRALThreadClientResponse);
    procedure ExecSQLRemote(AQuery: TDataset; AResp: TRALThreadClientResponse);

    function InfoFieldsFromSQL(ASQL: StringRAL): TRALDBInfoFields;
    function GetTables: TRALDBInfoTables;
  published
    property Client : TRALClientMT read FClient write SetClient;
    property ModuleRoute : StringRAL read FModuleRoute write SetModuleRoute;
  end;

implementation

{ TRALDBConnection }

procedure TRALDBConnection.SetClient(AValue: TRALClientMT);
begin
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);

  if AValue <> FClient then
    FClient := AValue;

  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALDBConnection.SetModuleRoute(AValue: StringRAL);
begin
  if FModuleRoute = AValue then
    Exit;

  FModuleRoute := FixRoute(AValue);
end;

constructor TRALDBConnection.Create(AOwner: TComponent);
begin
  FClient := nil;
  FModuleRoute := '/';
end;

procedure TRALDBConnection.ApplyUpdatesRemote(ACache: TRALDBSQLCache; AResp: TRALThreadClientResponse);
var
  vMem : TStream;
  vReq : TRALRequest;
  vUrl : StringRAL;
begin
  if ACache.Count = 0 then
    Exit;

  vMem := ACache.SaveToStream;
  vReq := FClient.NewRequest;
  try
    vReq.Clear;
    vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
    vReq.AddFile(vMem);

    vUrl := FModuleRoute + '/applyupdates';
    FClient.Post(vUrl, vReq, AResp, ebSingleThread);
  finally
    if FClient.RequestLifeCicle then
      FreeAndNil(vReq);

    FreeAndNil(vMem);
  end;
end;

procedure TRALDBConnection.OpenRemote(AQuery : TDataset; AResp : TRALThreadClientResponse);
var
  vMem: TStream;
  vReq: TRALRequest;
  vUrl: StringRAL;
  vSQLCache: TRALDBSQLCache;
begin
  vSQLCache := TRALDBSQLCache.Create;
  try
    vSQLCache.Add(AQuery);

    vMem := vSQLCache.SaveToStream;
    vReq := FClient.NewRequest;
    try
      vReq.Clear;
      vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
      vReq.AddFile(vMem);

      vUrl := FModuleRoute + '/opensql';
      FClient.Post(vUrl, vReq, AResp);
    finally
      if FClient.RequestLifeCicle then
        FreeAndNil(vReq);

      FreeAndNil(vMem);
    end;
  finally
    FreeAndNil(vSQLCache);
  end;
end;

procedure TRALDBConnection.ExecSQLRemote(AQuery : TDataset; AResp : TRALThreadClientResponse);
var
  vMem : TStream;
  vReq : TRALRequest;
  vUrl : StringRAL;
  vSQLCache: TRALDBSQLCache;
begin
  vSQLCache := TRALDBSQLCache.Create;
  try
    vSQLCache.Add(AQuery);

    vMem := vSQLCache.SaveToStream;
    vReq := FClient.NewRequest;
    try
      vReq.Clear;
      vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
      vReq.AddFile(vMem);

      vUrl := FModuleRoute + '/execsql';
      FClient.Post(vUrl, vReq, AResp, ebSingleThread);
    finally
      if FClient.RequestLifeCicle then
        FreeAndNil(vReq);

      FreeAndNil(vMem);
    end;
  finally
    FreeAndNil(vSQLCache);
  end;
end;

function TRALDBConnection.InfoFieldsFromSQL(ASQL: StringRAL): TRALDBInfoFields;
var
  vReq : TRALRequest;
  vUrl : StringRAL;
  vResponse: TRALResponse;
begin
  Result := nil;
  vResponse := nil;

  vReq := FClient.NewRequest;
  try
    vReq.Clear;
    vReq.ContentType := rctAPPLICATIONJSON;
    vReq.Params.AddParam('sql', ASQL, rpkQUERY);

    vUrl := FModuleRoute + '/getsqlfields';

    FClient.Get(vUrl, vReq, vResponse);
    try
      if vResponse.StatusCode = 200 then begin
        Result := TRALDBInfoFields.Create;
        Result.AsJSON := vResponse.Body.AsString;
      end;
    finally
      FreeAndNil(vResponse);
    end;
  finally
    if FClient.RequestLifeCicle then
      FreeAndNil(vReq);
  end;
end;

function TRALDBConnection.GetTables: TRALDBInfoTables;
var
  vReq : TRALRequest;
  vUrl : StringRAL;
  vResponse: TRALResponse;
begin
  Result := nil;
  vResponse := nil;

  vReq := FClient.NewRequest;
  try
    vReq.Clear;
    vReq.ContentType := rctAPPLICATIONJSON;

    vUrl := FModuleRoute + '/gettables';

    FClient.Get(vUrl, vReq, vResponse);
    try
      if vResponse.StatusCode = 200 then begin
        Result := TRALDBInfoTables.Create;
        Result.AsJSON := vResponse.Body.AsString;
      end;
    finally
      FreeAndNil(vResponse);
    end;
  finally
    if FClient.RequestLifeCicle then
      FreeAndNil(vReq);
  end;
end;

end.

