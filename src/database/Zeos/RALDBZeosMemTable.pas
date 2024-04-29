unit RALDBZeosMemTable;

interface

uses
  Classes, SysUtils, DB,
  ZDataset,
  RALDBStorage, RALRequest, RALClient, RALTypes, RALQueryStructure,
  RALResponse, RALMIMETypes, RALDBStorageBIN, RALDBStorageJSON, RALDBTypes;

type
  TRALDBOnError = procedure(Sender : TObject; AException : StringRAL) of object;

  { TRALDBZMemTable }

  TRALDBZMemTable = class(TZMemTable)
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
    property Storage : TRALDBStorageLink read FStorage write FStorage;
    property Params : TParams read FParams write FParams;
    property OnError : TRALDBOnError read FOnError write FOnError;
  end;

implementation

{ TRALDBZMemTable }

procedure TRALDBZMemTable.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
  inherited;
end;

procedure TRALDBZMemTable.SetSQL(AValue: TStrings);
begin
  if FSQL = AValue then
    Exit;

  FSQL.Text := AValue.Text;
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
  vSQL := TStringList(Sender).Text;
  TRALDB.ParseSQLParams(vSQL, FParams);
end;

procedure TRALDBZMemTable.OnQueryResponse(Sender: TObject;
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
      begin
        if Pos(rctAPPLICATIONJSON, AResponse.ContentType) > 0 then
          vStor := TRALDBStorageJSON_DBWare.Create
        else
          vStor := TRALDBStorageBIN.Create;

        try
          vStor.LoadFromStream(Self, vMem);
        finally
          FreeAndNil(vStor);
        end;
      end
      else
      begin
        FStorage.LoadFromStream(Self, vMem);
      end;
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

constructor TRALDBZMemTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := {$IFDEF FPC}@{$ENDIF}OnChangeSQL;

  FParams := TParams.Create(Self);
end;

destructor TRALDBZMemTable.Destroy;
begin
  FreeAndNil(FSQL);
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TRALDBZMemTable.ParamByName(const AValue: StringRAL): TParam;
begin
  Result := FParams.FindParam(AValue);
end;

procedure TRALDBZMemTable.OpenRemote;
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

      FClient.Post(FModuleRoute + '/opensql', vReq, {$IFDEF FPC}@{$ENDIF}OnQueryResponse);
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

      FClient.Post(FModuleRoute + '/execsql', vReq, {$IFDEF FPC}@{$ENDIF}OnExecSQLResponse);
    finally
      if FClient.RequestLifeCicle then
        FreeAndNil(vReq);
    end;
  finally
    FreeAndNil(vQueryStructure);
  end;
end;

end.

