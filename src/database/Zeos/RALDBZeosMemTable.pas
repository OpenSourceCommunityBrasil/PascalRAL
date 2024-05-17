unit RALDBZeosMemTable;

{$IFNDEF FPC}
  {$I ZComponent.inc}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB,
  ZDataset,
  RALDBStorage, RALRequest, RALClient, RALTypes, RALQueryStructure,
  RALResponse, RALMIMETypes, RALDBTypes;

type
  { TRALDBZMemTable }

  TRALDBZMemTable = class(TZMemTable)
  private
    FClient: TRALClientMT;
    FModuleRoute: StringRAL;
    FSQL: TStrings;
    FParams: TParams;
    FStorage: TRALDBStorageLink;
    FOnError: TRALDBOnError;
    FUpdateSQL: TRALDBUpdateSQL;
  protected
    procedure InternalPost; override;
    procedure InternalDelete; override;
  protected
    /// needed to properly remove assignment in design-time.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

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

    function ParamByName(const AValue: StringRAL): TParam; reintroduce;

    procedure OpenRemote;
    procedure ExecSQLRemote;
    procedure ApplyUpdatesRemote;
  published
//    property Active;
//    property FieldDefs;

    property Client : TRALClientMT read FClient write SetClient;
    property ModuleRoute : StringRAL read FModuleRoute write FModuleRoute;
    property SQL : TStrings read FSQL write SetSQL;
    property Storage : TRALDBStorageLink read FStorage write SetStorage;
    property Params : TParams read FParams write FParams;
    property UpdateSQL : TRALDBUpdateSQL read FUpdateSQL write FUpdateSQL;

    property OnError : TRALDBOnError read FOnError write FOnError;
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
  inherited InternalPost;

  if not CachedUpdates then
    ApplyUpdatesRemote;
end;

procedure TRALDBZMemTable.InternalDelete;
begin
  inherited InternalDelete;
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
        ZeosLoadFromStream(vMem)
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
    vMethod : TMethod;
    vProc : TLoadFromStream;
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

  FParams := TParams.Create(Self);
  FUpdateSQL := TRALDBUpdateSQL.Create;
  CachedUpdates := True;
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

procedure TRALDBZMemTable.ApplyUpdatesRemote;
var
  vBook: TBookMark;
begin
  Self.DisableControls;
  vBook := Self.GetBookmark;

  Self.First;
  while not Self.Eof do begin
    if Self.UpdateStatus in [usUnmodified] then
    begin

    end;
    Self.Next;
  end;

  Self.GotoBookmark(vBook);
  Self.FreeBookmark(vBook);
  Self.DisableControls;
end;

end.

