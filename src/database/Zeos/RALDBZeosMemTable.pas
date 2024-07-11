/// Unit for ZMemTable wrapping
unit RALDBZeosMemTable;

{$IFNDEF FPC}
  {$I ZComponent.inc}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB,
  ZDataset,
  RALDBStorage, RALRequest, RALClient, RALTypes, RALQueryStructure,
  RALResponse, RALMIMETypes, RALDBTypes, RALTools;

type
  { TRALDBZMemTable }

  TRALDBZMemTable = class(TZMemTable)
  private
    FClient: TRALClientMT;
    FLoading : boolean;
    FModuleRoute: StringRAL;
    FSQL: TStrings;
    FParamCheck: boolean;
    FParams: TParams;
    FStorage: TRALDBStorageLink;
    FUpdateSQL: TRALDBUpdateSQL;

    FOnError: TRALDBOnError;
  protected
    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalPost; override;
    procedure InternalDelete; override;

    procedure SetSQL(AValue: TStrings);
    procedure SetClient(AValue: TRALClientMT);
    procedure SetStorage(AValue: TRALDBStorageLink);
    procedure OnChangeSQL(Sender : TObject);

    procedure OnQueryResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure ZeosLoadFromStream(AStream : TStream);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure ApplyUpdatesRemote;
    procedure ExecSQLRemote;
    procedure OpenRemote;

    function ParamByName(const AValue: StringRAL): TParam; reintroduce;
  published
    property Client: TRALClientMT read FClient write SetClient;
    property ModuleRoute: StringRAL read FModuleRoute write FModuleRoute;
    property ParamCheck : boolean read FParamCheck write FParamCheck;
    property Params: TParams read FParams write FParams;
    property SQL: TStrings read FSQL write SetSQL;
    property Storage: TRALDBStorageLink read FStorage write SetStorage;
    property UpdateSQL: TRALDBUpdateSQL read FUpdateSQL write FUpdateSQL;

    property OnError: TRALDBOnError read FOnError write FOnError;
  end;

implementation

{ TRALDBZMemTable }

procedure TRALDBZMemTable.SetStorage(AValue: TRALDBStorageLink);
begin
  if FStorage <> nil then
    FStorage.RemoveFreeNotification(Self);

  if AValue <> FStorage then
    FStorage := AValue;

  if FStorage <> nil then
    FStorage.FreeNotification(Self);
end;

procedure TRALDBZMemTable.InternalPost;
begin
  if FLoading then begin
    inherited InternalPost;
  end
  else begin
    // cacheupdate or send
    inherited InternalPost;
  end;
end;

procedure TRALDBZMemTable.InternalClose;
begin
  inherited;

end;

procedure TRALDBZMemTable.InternalDelete;
begin
  inherited InternalDelete;
end;

procedure TRALDBZMemTable.InternalOpen;
begin
  inherited;

end;

procedure TRALDBZMemTable.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil
  else if (Operation = opRemove) and (AComponent = FStorage) then
    FStorage := nil;
  inherited;
end;

procedure TRALDBZMemTable.SetSQL(AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;

procedure TRALDBZMemTable.SetClient(AValue: TRALClientMT);
begin
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);

  if AValue <> FClient then
    FClient := AValue;

  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALDBZMemTable.OnChangeSQL(Sender: TObject);
var
  vSQL : StringRAL;
begin
  if FParamCheck then
  begin
    vSQL := TStringList(Sender).Text;
    TRALDB.ParseSQLParams(vSQL, FParams);
  end
  else
  begin
    FParams.Clear;
  end;
end;

procedure TRALDBZMemTable.OnQueryResponse(Sender: TObject;
  AResponse: TRALResponse; AException: StringRAL);
var
  vMem : TStream;
  vNative : Boolean;
  vException : StringRAL;
begin
  if AResponse.StatusCode = 200 then
  begin
    vNative := AResponse.ParamByName('ResultType').AsInteger = 1;
    vMem := AResponse.ParamByName('Stream').AsStream;
    try
      FLoading := True;

      if vNative then
        ZeosLoadFromStream(vMem)
      else
        FStorage.LoadFromStream(Self, vMem);
    finally
      FreeAndNil(vMem);
      FLoading := False;
      Resync([rmExact]);
    end;
  end
  else if AResponse.StatusCode = 500 then
  begin
    vException := AResponse.ParamByName('Exception').AsString;
    if Assigned(FOnError) then
      FOnError(Self, vException);
  end
  else
  begin
    if Assigned(FOnError) then
      FOnError(Self, AException);
  end;
end;

procedure TRALDBZMemTable.OnExecSQLResponse(Sender: TObject;
  AResponse: TRALResponse; AException: StringRAL);
var
  vException : StringRAL;
begin
  if AResponse.StatusCode = 500 then
  begin
    vException := AResponse.ParamByName('Exception').AsString;
    if Assigned(FOnError) then
      FOnError(Self, vException);
  end
  else if AException <> '' then
  begin
    if Assigned(FOnError) then
      FOnError(Self, AException);
  end;
end;

procedure TRALDBZMemTable.ZeosLoadFromStream(AStream: TStream);
{$IFDEF FPC}
  type
    TLoadFromStream = procedure (AStream: TStream) of object;
  var
    vMethod: TMethod;
    vProc: TLoadFromStream;
{$ENDIF}
begin
  {$IFNDEF FPC}
    {$IFDEF ZMEMTABLE_ENABLE_STREAM_EXPORT_IMPORT}
      Self.LoadFromStream(AStream);
    {$ENDIF}
  {$ELSE}
    vMethod.Data := Pointer(Self);
    vMethod.Code := Self.MethodAddress('LoadFromStream');
    if vMethod.Code <> nil then
    begin
      vProc := TLoadFromStream(vMethod);
      vProc(AStream);
    end;
  {$ENDIF}
end;

constructor TRALDBZMemTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := {$IFDEF FPC}@{$ENDIF}OnChangeSQL;

  FParamCheck := True;
  FParams := TParams.Create(Self);
  FUpdateSQL := TRALDBUpdateSQL.Create;
  CachedUpdates := False;
end;

destructor TRALDBZMemTable.Destroy;
begin
  FreeAndNil(FSQL);
  FreeAndNil(FParams);
  FreeAndNil(FUpdateSQL);
  inherited Destroy;
end;

function TRALDBZMemTable.ParamByName(const AValue: StringRAL): TParam;
begin
  Result := FParams.FindParam(AValue);
end;

procedure TRALDBZMemTable.OpenRemote;
var
  vQueryStructure: TRALQueryStructure;
  vMem: TStream;
  vReq: TRALRequest;
  vUrl: StringRAL;
begin
  vQueryStructure := TRALQueryStructure.Create;
  try
    vMem := vQueryStructure.ExportToBinary(Self);
    try
      vReq := FClient.NewRequest;
      vReq.Clear;
      vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
      vReq.AddFile(vMem);

      vUrl := FixRoute(FModuleRoute + '/opensql');
      FClient.Post(vUrl, vReq, {$IFDEF FPC}@{$ENDIF}OnQueryResponse);
    finally
      if FClient.RequestLifeCicle then
        FreeAndNil(vReq);
    end;
  finally
    FreeAndNil(vQueryStructure);
  end;
end;

procedure TRALDBZMemTable.ExecSQLRemote;
var
  vQueryStructure: TRALQueryStructure;
  vMem: TStream;
  vReq: TRALRequest;
  vUrl: StringRAL;
begin
  vQueryStructure := TRALQueryStructure.Create;
  try
    vMem := vQueryStructure.ExportToBinary(Self);
    try
      vReq := FClient.NewRequest;
      vReq.Clear;
      vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
      vReq.AddFile(vMem);

      vUrl := FixRoute(FModuleRoute + '/execsql');
      FClient.Post(vUrl, vReq, {$IFDEF FPC}@{$ENDIF}OnExecSQLResponse);
    finally
      if FClient.RequestLifeCicle then
        FreeAndNil(vReq);
    end;
  finally
    FreeAndNil(vQueryStructure);
  end;
end;

procedure TRALDBZMemTable.ApplyUpdatesRemote;
begin

end;

end.

