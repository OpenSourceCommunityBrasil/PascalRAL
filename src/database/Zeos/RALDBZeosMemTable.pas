unit RALDBZeosMemTable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB,
  ZDataset,
  RALDBStorage, RALRequest, RALClient, RALTypes, RALQueryStructure,
  RALResponse, RALMIMETypes, RALDBStorageBIN, RALDBStorageJSON;

type

  { TRALDBZeosMemTable }

  TRALDBZeosMemTable = class(TZMemTable)
  private
    FClient: TRALClientMT;
    FModuleRoute: StringRAL;
    FSQL: TStrings;
    FParams: TParams;
    FStorage: TRALDBStorageLink;
  protected
    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetSQL(AValue: TStrings);
    procedure SetClient(AValue: TRALClientMT);

    procedure OnQueryResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
    procedure OnExecSQLResponse(Sender: TObject; AResponse: TRALResponse; AException: StringRAL);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure OpenRemote;
    procedure ExecSQLRemote;
  published
    property Client : TRALClientMT read FClient write SetClient;
    property ModuleRoute : StringRAL read FModuleRoute write FModuleRoute;
    property SQL : TStrings read FSQL write SetSQL;
    property Storage : TRALDBStorageLink read FStorage write FStorage;
    property Params : TParams read FParams write FParams;
  end;

implementation

{ TRALDBZeosMemTable }

procedure TRALDBZeosMemTable.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
  inherited;
end;

procedure TRALDBZeosMemTable.SetSQL(AValue: TStrings);
begin
  if FSQL = AValue then
    Exit;

  FSQL.Text := AValue.Text;
end;

procedure TRALDBZeosMemTable.SetClient(AValue: TRALClientMT);
begin
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);

  if AValue <> FClient then
    FClient := AValue;

  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALDBZeosMemTable.OnQueryResponse(Sender: TObject;
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
          vStor := TRALDBStorageJSON.Create
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
    if vException <> '' then
      raise Exception.Create(vException)
  end
  else
  begin
    if AException <> '' then
      raise Exception.Create(AException)
  end;
end;

procedure TRALDBZeosMemTable.OnExecSQLResponse(Sender: TObject;
  AResponse: TRALResponse; AException: StringRAL);
begin
  if AException <> '' then
    raise Exception.Create(AException)
end;

constructor TRALDBZeosMemTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FParams := TParams.Create(Self);
end;

destructor TRALDBZeosMemTable.Destroy;
begin
  FreeAndNil(FSQL);
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TRALDBZeosMemTable.OpenRemote;
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

procedure TRALDBZeosMemTable.ExecSQLRemote;
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

