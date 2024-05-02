unit RALDBBufDataset;

interface

uses
  Classes, SysUtils, DB,
  BufDataset,
  RALDBStorage, RALRequest, RALClient, RALTypes, RALQueryStructure,
  RALResponse, RALMIMETypes, RALDBStorageBIN, RALDBStorageJSON, RALDBTypes;

type

  { TRALDBBufDataset }

  TRALDBBufDataset = class(TBufDataset)
  private
    FClient: TRALClientMT;
    FModuleRoute: StringRAL;
    FSQL: TStrings;
    FParams: TParams;
    FParamCheck: boolean;
    FStorage: TRALDBStorageLink;
    FOnError: TRALDBOnError;
  protected
    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetSQL(AValue: TStrings);
    procedure SetClient(AValue: TRALClientMT);
    procedure SetStorage(AValue: TRALDBStorageLink);
    procedure OnChangeSQL(Sender : TObject);

    procedure OnQueryResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function ParamByName(const AValue: StringRAL): TParam; reintroduce;

    procedure OpenRemote;
    procedure ExecSQLRemote;
  published
    property Client : TRALClientMT read FClient write SetClient;
    property ModuleRoute : StringRAL read FModuleRoute write FModuleRoute;
    property SQL : TStrings read FSQL write SetSQL;
    property Storage : TRALDBStorageLink read FStorage write SetStorage;
    property ParamCheck : boolean read FParamCheck write FParamCheck;
    property Params : TParams read FParams write FParams;
    property OnError : TRALDBOnError read FOnError write FOnError;
  end;


implementation

{ TRALDBBufDataset }

procedure TRALDBBufDataset.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil
  else if (Operation = opRemove) and (AComponent = FStorage) then
    FStorage := nil;
  inherited;
end;

procedure TRALDBBufDataset.SetSQL(AValue: TStrings);
begin
  if FSQL = AValue then
    Exit;

  FSQL.Text := AValue.Text;
end;

procedure TRALDBBufDataset.SetClient(AValue: TRALClientMT);
begin
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);

  if AValue <> FClient then
    FClient := AValue;

  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALDBBufDataset.SetStorage(AValue: TRALDBStorageLink);
begin
  if FStorage <> nil then
    FStorage.RemoveFreeNotification(Self);

  if AValue <> FStorage then
    FStorage := AValue;

  if FStorage <> nil then
    FStorage.FreeNotification(Self);
end;

procedure TRALDBBufDataset.OnChangeSQL(Sender: TObject);
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

procedure TRALDBBufDataset.OnQueryResponse(Sender: TObject;
  AResponse: TRALResponse; AException: StringRAL);
var
  vMem : TStream;
  vNative : Boolean;
  vException : StringRAL;
  vStor : TRALDBStorage;
begin
  if AResponse.StatusCode = 200 then
  begin
    vNative := AResponse.ParamByName('ResultType').AsInteger = 1;
    vMem := AResponse.ParamByName('Stream').AsStream;
    try
      if vNative then
        Self.LoadFromStream(vMem, dfBinary)
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

procedure TRALDBBufDataset.OnExecSQLResponse(Sender: TObject;
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

constructor TRALDBBufDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := @OnChangeSQL;

  FParamCheck := True;
  FParams := TParams.Create(Self);
end;

destructor TRALDBBufDataset.Destroy;
begin
  FreeAndNil(FSQL);
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TRALDBBufDataset.ParamByName(const AValue: StringRAL): TParam;
begin
  Result := FParams.FindParam(AValue);
end;

procedure TRALDBBufDataset.OpenRemote;
var
  vQueryStructure: TRALQueryStructure;
  vMem : TStream;
  vReq : TRALRequest;
begin
  vQueryStructure := TRALQueryStructure.Create;
  try
    vMem := vQueryStructure.ExportToBinary(Self);
    try
      vReq := FClient.NewRequest;
      vReq.Clear;
      vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
      vReq.AddFile(vMem);

      FClient.Post(FModuleRoute + '/opensql', vReq, @OnQueryResponse);
    finally
      if FClient.RequestLifeCicle then
        FreeAndNil(vReq);
    end;
  finally
    FreeAndNil(vQueryStructure);
  end;
end;

procedure TRALDBBufDataset.ExecSQLRemote;
var
  vQueryStructure: TRALQueryStructure;
  vMem : TStream;
  vReq : TRALRequest;
begin
  vQueryStructure := TRALQueryStructure.Create;
  try
    vMem := vQueryStructure.ExportToBinary(Self);
    try
      vReq := FClient.NewRequest;
      vReq.Clear;
      vReq.ContentType := rctAPPLICATIONOCTETSTREAM;
      vReq.AddFile(vMem);

      FClient.Post(FModuleRoute + '/execsql', vReq, @OnExecSQLResponse);
    finally
      if FClient.RequestLifeCicle then
        FreeAndNil(vReq);
    end;
  finally
    FreeAndNil(vQueryStructure);
  end;
end;

end.

