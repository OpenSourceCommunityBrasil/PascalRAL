/// Unit for FireDAC MemTable wrapper
unit RALDBFiredacMemTable;

interface

uses
  Classes, SysUtils, DB,
  FireDAC.Comp.Client, FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageBin,
  RALDBStorage, RALRequest, RALClient, RALTypes, RALQueryStructure,
  RALResponse, RALMIMETypes, RALDBTypes;

type
  { TRALDBFDMemTable }

  TRALDBFDMemTable = class(TFDMemTable)
  private
    FClient: TRALClientMT;
    FModuleRoute: StringRAL;
    FSQL: TStrings;
    FParams: TParams;
    FStorage: TRALDBStorageLink;
    FOnError: TRALDBOnError;
  protected
    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetSQL(AValue: TStrings);
    procedure SetClient(AValue: TRALClientMT);
    procedure SetStorage(const AValue: TRALDBStorageLink);
    procedure OnChangeSQL(Sender: TObject);

    procedure OnQueryResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ExecSQLRemote;
    procedure OpenRemote;
    function ParamByName(const AValue: StringRAL): TParam;
  published
    property Client: TRALClientMT read FClient write SetClient;
    property ModuleRoute: StringRAL read FModuleRoute write FModuleRoute;
    property Params: TParams read FParams write FParams;
    property SQL: TStrings read FSQL write SetSQL;
    property Storage: TRALDBStorageLink read FStorage write SetStorage;
    property OnError: TRALDBOnError read FOnError write FOnError;
  end;

implementation

{ TRALDBFDMemTable }

constructor TRALDBFDMemTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := OnChangeSQL;

  FParams := TParams.Create(Self);
end;

destructor TRALDBFDMemTable.Destroy;
begin
  FreeAndNil(FSQL);
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TRALDBFDMemTable.ExecSQLRemote;
var
  vQueryStructure: TRALQueryStructure;
  vMem: TStream;
  vReq: TRALRequest;
begin
  vQueryStructure := TRALQueryStructure.Create;
  try
    vMem := vQueryStructure.ExportToBinary(Self);
    try
      vReq := FClient.NewRequest;
      vReq.Clear;
      vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
      vReq.AddFile(vMem);

      FClient.Post(FModuleRoute + '/execsql', vReq, OnExecSQLResponse);
    finally
      if FClient.RequestLifeCicle then
        FreeAndNil(vReq);
    end;
  finally
    FreeAndNil(vQueryStructure);
  end;
end;

procedure TRALDBFDMemTable.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil
  else if (Operation = opRemove) and (AComponent = FStorage) then
    FStorage := nil;
  inherited;
end;

procedure TRALDBFDMemTable.OnChangeSQL(Sender: TObject);
var
  vSQL: StringRAL;
begin
  vSQL := TStringList(Sender).Text;
  TRALDB.ParseSQLParams(vSQL, FParams);
end;

procedure TRALDBFDMemTable.OnExecSQLResponse(Sender: TObject;
  AResponse: TRALResponse; AException: StringRAL);
var
  vException: StringRAL;
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

procedure TRALDBFDMemTable.OnQueryResponse(Sender: TObject;
  AResponse: TRALResponse; AException: StringRAL);
var
  vMem: TStream;
  vNative: Boolean;
  vException: StringRAL;
  vStor: TRALDBStorage;
begin
  if AResponse.StatusCode = 200 then
  begin
    vNative := AResponse.ParamByName('ResultType').AsInteger = 1;
    vMem := AResponse.ParamByName('Stream').AsStream;
    try
      if vNative then
        Self.LoadFromStream(vMem)
      else
        FStorage.LoadFromStream(Self, vMem);
    finally
      FreeAndNil(vMem);
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

procedure TRALDBFDMemTable.OpenRemote;
var
  vQueryStructure: TRALQueryStructure;
  vMem: TStream;
  vReq: TRALRequest;
begin
  vQueryStructure := TRALQueryStructure.Create;
  try
    vMem := vQueryStructure.ExportToBinary(Self);
    try
      vReq := FClient.NewRequest;
      vReq.Clear;
      vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
      vReq.AddFile(vMem);

      FClient.Post(FModuleRoute + '/opensql', vReq, OnQueryResponse);
    finally
      if FClient.RequestLifeCicle then
        FreeAndNil(vReq);
    end;
  finally
    FreeAndNil(vQueryStructure);
  end;
end;

function TRALDBFDMemTable.ParamByName(const AValue: StringRAL): TParam;
begin
  Result := FParams.FindParam(AValue);
end;

procedure TRALDBFDMemTable.SetClient(AValue: TRALClientMT);
begin
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);

  if AValue <> FClient then
    FClient := AValue;

  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALDBFDMemTable.SetSQL(AValue: TStrings);
begin
  if FSQL = AValue then
    Exit;

  FSQL.Text := AValue.Text;
end;

procedure TRALDBFDMemTable.SetStorage(const AValue: TRALDBStorageLink);
begin
  if FStorage <> nil then
    FStorage.RemoveFreeNotification(Self);

  if AValue <> FStorage then
    FStorage := AValue;

  if FStorage <> nil then
    FStorage.FreeNotification(Self);
end;

end.
