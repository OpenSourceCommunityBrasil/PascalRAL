/// Unit for the server side database connection pool
unit RALDBPool;

interface

uses
  Classes, SysUtils, DateUtils,
  RALTypes, RALConsts, RALThreadSafe, RALDBBase, RALRequest, RALResponse;

const
  /// Interval, in milliseconds, between checks while waiting for a free connection
  cRALPoolWaitStep = 5;

type
  { Action taken when every connection is busy and the wait timeout expires.
    peRaiseError raises ERALDBPoolTimeout, answered as HTTP 503 by TRALDBModule.
    peOverflow creates a temporary connection, discarded as soon as it is released }
  TRALDBPoolExhausted = (peRaiseError, peOverflow);

  /// Raised when the pool cannot deliver a connection within WaitTimeout
  ERALDBPoolTimeout = class(Exception);

  { TRALDBPoolOptions }

  // Configuration of a TRALDBConnectionPool, published by TRALDBModule
  TRALDBPoolOptions = class(TPersistent)
  private
    FEnabled: boolean;
    FIdleTimeout: IntegerRAL;
    FMaxLifetime: IntegerRAL;
    FMaxOverflow: IntegerRAL;
    FMaxSize: IntegerRAL;
    FMaxUses: IntegerRAL;
    FMinSize: IntegerRAL;
    FOnExhausted: TRALDBPoolExhausted;
    FQueueSize: IntegerRAL;
    FValidateOnAcquire: boolean;
    FWaitTimeout: IntegerRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetMaxSize(const AValue: IntegerRAL);
    procedure SetMinSize(const AValue: IntegerRAL);
  public
    constructor Create;
  published
    // Turns pooling on. When false the module keeps creating one connection per request
    property Enabled: boolean read FEnabled write FEnabled default False;
    { Milliseconds an idle connection is kept before being closed. The pool never
      shrinks below MinSize. Zero keeps idle connections forever }
    property IdleTimeout: IntegerRAL read FIdleTimeout write FIdleTimeout default 300000;
    { Milliseconds a connection may live before being recycled on release.
      Zero disables recycling by age }
    property MaxLifetime: IntegerRAL read FMaxLifetime write FMaxLifetime default 0;
    { Maximum simultaneous temporary connections created when OnExhausted is
      peOverflow. Zero means no limit }
    property MaxOverflow: IntegerRAL read FMaxOverflow write FMaxOverflow default 0;
    // Maximum number of pooled connections
    property MaxSize: IntegerRAL read FMaxSize write SetMaxSize default 10;
    { How many times a connection may be handed out before being recycled on
      release. Zero disables recycling by usage }
    property MaxUses: IntegerRAL read FMaxUses write FMaxUses default 0;
    // Number of connections the pool keeps open once created
    property MinSize: IntegerRAL read FMinSize write SetMinSize default 0;
    // What to do when the pool is exhausted and WaitTimeout expires
    property OnExhausted: TRALDBPoolExhausted read FOnExhausted write FOnExhausted
                          default peRaiseError;
    { Maximum number of requests allowed to wait for a connection. Further requests
      fail immediately instead of waiting. Zero means no limit }
    property QueueSize: IntegerRAL read FQueueSize write FQueueSize default 0;
    { Runs a lightweight query before handing a connection to the request, so a
      connection dropped by the server does not break it. Costs one round trip }
    property ValidateOnAcquire: boolean read FValidateOnAcquire
                                write FValidateOnAcquire default False;
    // Milliseconds a request waits for a free connection
    property WaitTimeout: IntegerRAL read FWaitTimeout write FWaitTimeout default 5000;
  end;

  { TRALDBPoolItem }

  // Single connection held by the pool, along with its usage bookkeeping
  TRALDBPoolItem = class
  private
    FCreatedAt: TDateTime;
    FDatabase: TRALDBBase;
    FInUse: boolean;
    FLastUsed: TDateTime;
    FOverflow: boolean;
    FUseCount: Int64RAL;
  public
    constructor Create(ADatabase: TRALDBBase);
    destructor Destroy; override;

    property CreatedAt: TDateTime read FCreatedAt;
    property Database: TRALDBBase read FDatabase;
    property InUse: boolean read FInUse;
    property LastUsed: TDateTime read FLastUsed;
    property Overflow: boolean read FOverflow;
    property UseCount: Int64RAL read FUseCount;
  end;

  /// Called by the pool whenever it needs a brand new, already configured driver
  TRALDBOnPoolCreate = function(ASender: TObject): TRALDBBase of object;
  /// Called when a request gave up waiting for a free connection
  TRALDBOnPoolTimeout = procedure(ASender: TObject; ARequest: TRALRequest;
                                  AWaitTime: IntegerRAL) of object;

  { TRALDBConnectionPool }

  { Keeps a set of open TRALDBBase connections and hands them out one request at a
    time, so the driver does not have to connect and disconnect on every call.
    Works with any driver registered through RegisterDatabase, since everything it
    needs is declared on TRALDBBase }
  TRALDBConnectionPool = class(TRALThreadSafe)
  private
    FItems: TList;
    FOptions: TRALDBPoolOptions;
    FTotalAcquired: Int64RAL;
    FTotalCreated: Int64RAL;
    FTotalTimeouts: Int64RAL;
    FWaiting: IntegerRAL;

    FOnCreateConnection: TRALDBOnPoolCreate;
    FOnError: TRALDBOnError;
    FOnTimeout: TRALDBOnPoolTimeout;
  protected
    // every routine below marked "locked" expects the caller to hold the lock
    function CountLocked(AOverflow: boolean; AInUseOnly: boolean): IntegerRAL;
    function CreateItemLocked(AOverflow: boolean): TRALDBPoolItem;
    procedure DropExpiredLocked;
    function FindItemLocked(ADatabase: TRALDBBase): TRALDBPoolItem;
    function GetFreeItemLocked: TRALDBPoolItem;
    function IsExpiredLocked(AItem: TRALDBPoolItem): boolean;
    procedure RemoveItemLocked(AItem: TRALDBPoolItem);

    function GetAvailableCount: IntegerRAL;
    function GetInUseCount: IntegerRAL;
    function GetOverflowCount: IntegerRAL;
    function GetPooledCount: IntegerRAL;
    function GetWaitingCount: IntegerRAL;

    function DoCreateConnection: TRALDBBase;
    function ExhaustedItem(ARequest: TRALRequest; AWaited: IntegerRAL): TRALDBPoolItem;
    function PrepareItem(AItem: TRALDBPoolItem; ARequest: TRALRequest;
                         AResponse: TRALResponse): TRALDBBase;
  public
    constructor Create; override;
    destructor Destroy; override;

    { Returns a connection ready to be used. When pooling is disabled it simply
      creates a new driver, exactly like the module used to do }
    function Acquire(ARequest: TRALRequest; AResponse: TRALResponse): TRALDBBase;
    { Gives a connection back. Connections that did not come from the pool, and
      pooled ones that expired, are destroyed here }
    procedure Release(ADatabase: TRALDBBase);
    // Closes and destroys every connection that is not in use
    procedure Clear;
    { Opens MinSize connections up front. Call it once the server is up so the
      first requests do not pay for connecting }
    procedure Prepare;

    property Options: TRALDBPoolOptions read FOptions;

    // connections free to be handed out right now
    property AvailableCount: IntegerRAL read GetAvailableCount;
    // connections currently serving a request
    property InUseCount: IntegerRAL read GetInUseCount;
    // temporary connections created past MaxSize
    property OverflowCount: IntegerRAL read GetOverflowCount;
    // connections held by the pool, overflow aside
    property PooledCount: IntegerRAL read GetPooledCount;
    // requests waiting for a free connection
    property WaitingCount: IntegerRAL read GetWaitingCount;
    // connections handed out since the pool was created
    property TotalAcquired: Int64RAL read FTotalAcquired;
    // connections opened since the pool was created
    property TotalCreated: Int64RAL read FTotalCreated;
    // requests that gave up waiting since the pool was created
    property TotalTimeouts: Int64RAL read FTotalTimeouts;

    property OnCreateConnection: TRALDBOnPoolCreate read FOnCreateConnection
                                 write FOnCreateConnection;
    property OnError: TRALDBOnError read FOnError write FOnError;
    property OnTimeout: TRALDBOnPoolTimeout read FOnTimeout write FOnTimeout;
  end;

implementation

{ TRALDBPoolOptions }

constructor TRALDBPoolOptions.Create;
begin
  inherited Create;
  FEnabled := False;
  FIdleTimeout := 300000;
  FMaxLifetime := 0;
  FMaxOverflow := 0;
  FMaxSize := 10;
  FMaxUses := 0;
  FMinSize := 0;
  FOnExhausted := peRaiseError;
  FQueueSize := 0;
  FValidateOnAcquire := False;
  FWaitTimeout := 5000;
end;

procedure TRALDBPoolOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALDBPoolOptions then
  begin
    TRALDBPoolOptions(Dest).FEnabled := FEnabled;
    TRALDBPoolOptions(Dest).FIdleTimeout := FIdleTimeout;
    TRALDBPoolOptions(Dest).FMaxLifetime := FMaxLifetime;
    TRALDBPoolOptions(Dest).FMaxOverflow := FMaxOverflow;
    TRALDBPoolOptions(Dest).FMaxSize := FMaxSize;
    TRALDBPoolOptions(Dest).FMaxUses := FMaxUses;
    TRALDBPoolOptions(Dest).FMinSize := FMinSize;
    TRALDBPoolOptions(Dest).FOnExhausted := FOnExhausted;
    TRALDBPoolOptions(Dest).FQueueSize := FQueueSize;
    TRALDBPoolOptions(Dest).FValidateOnAcquire := FValidateOnAcquire;
    TRALDBPoolOptions(Dest).FWaitTimeout := FWaitTimeout;
  end
  else
  begin
    inherited AssignTo(Dest);
  end;
end;

procedure TRALDBPoolOptions.SetMaxSize(const AValue: IntegerRAL);
begin
  if AValue < 1 then
    FMaxSize := 1
  else
    FMaxSize := AValue;

  if FMinSize > FMaxSize then
    FMinSize := FMaxSize;
end;

procedure TRALDBPoolOptions.SetMinSize(const AValue: IntegerRAL);
begin
  if AValue < 0 then
    FMinSize := 0
  else if AValue > FMaxSize then
    FMinSize := FMaxSize
  else
    FMinSize := AValue;
end;

{ TRALDBPoolItem }

constructor TRALDBPoolItem.Create(ADatabase: TRALDBBase);
begin
  inherited Create;
  FCreatedAt := Now;
  FDatabase := ADatabase;
  FInUse := False;
  FLastUsed := FCreatedAt;
  FOverflow := False;
  FUseCount := 0;
end;

destructor TRALDBPoolItem.Destroy;
begin
  if FDatabase <> nil then
  begin
    try
      FDatabase.Disconnect;
    except
      // a broken connection must not stop the item from being destroyed
    end;
    FreeAndNil(FDatabase);
  end;
  inherited Destroy;
end;

{ TRALDBConnectionPool }

constructor TRALDBConnectionPool.Create;
begin
  inherited Create;
  FItems := TList.Create;
  FOptions := TRALDBPoolOptions.Create;
  FTotalAcquired := 0;
  FTotalCreated := 0;
  FTotalTimeouts := 0;
  FWaiting := 0;
end;

destructor TRALDBConnectionPool.Destroy;
var
  vInt: IntegerRAL;
begin
  Lock;
  try
    for vInt := Pred(FItems.Count) downto 0 do
      TRALDBPoolItem(FItems.Items[vInt]).Free;
    FItems.Clear;
  finally
    Unlock;
  end;

  FreeAndNil(FItems);
  FreeAndNil(FOptions);
  inherited Destroy;
end;

function TRALDBConnectionPool.CountLocked(AOverflow: boolean;
  AInUseOnly: boolean): IntegerRAL;
var
  vInt: IntegerRAL;
  vItem: TRALDBPoolItem;
begin
  Result := 0;
  for vInt := 0 to Pred(FItems.Count) do
  begin
    vItem := TRALDBPoolItem(FItems.Items[vInt]);
    if vItem.FOverflow <> AOverflow then
      Continue;
    if AInUseOnly and (not vItem.FInUse) then
      Continue;
    Inc(Result);
  end;
end;

function TRALDBConnectionPool.CreateItemLocked(AOverflow: boolean): TRALDBPoolItem;
var
  vDatabase: TRALDBBase;
begin
  vDatabase := DoCreateConnection;

  Result := TRALDBPoolItem.Create(vDatabase);
  Result.FOverflow := AOverflow;
  Result.FInUse := True;

  FItems.Add(Result);
  Inc(FTotalCreated);
end;

procedure TRALDBConnectionPool.DropExpiredLocked;
var
  vInt: IntegerRAL;
  vItem: TRALDBPoolItem;
begin
  if FOptions.IdleTimeout <= 0 then
    Exit;

  for vInt := Pred(FItems.Count) downto 0 do
  begin
    vItem := TRALDBPoolItem(FItems.Items[vInt]);
    if vItem.FInUse or vItem.FOverflow then
      Continue;
    if CountLocked(False, False) <= FOptions.MinSize then
      Break;
    if MilliSecondsBetween(Now, vItem.FLastUsed) >= FOptions.IdleTimeout then
      RemoveItemLocked(vItem);
  end;
end;

function TRALDBConnectionPool.FindItemLocked(ADatabase: TRALDBBase): TRALDBPoolItem;
var
  vInt: IntegerRAL;
  vItem: TRALDBPoolItem;
begin
  Result := nil;
  for vInt := 0 to Pred(FItems.Count) do
  begin
    vItem := TRALDBPoolItem(FItems.Items[vInt]);
    if vItem.FDatabase = ADatabase then
    begin
      Result := vItem;
      Break;
    end;
  end;
end;

function TRALDBConnectionPool.GetFreeItemLocked: TRALDBPoolItem;
var
  vInt: IntegerRAL;
  vItem: TRALDBPoolItem;
begin
  Result := nil;
  for vInt := Pred(FItems.Count) downto 0 do
  begin
    vItem := TRALDBPoolItem(FItems.Items[vInt]);
    if vItem.FInUse then
      Continue;

    // an expired connection is dropped instead of handed out again
    if IsExpiredLocked(vItem) then
    begin
      RemoveItemLocked(vItem);
      Continue;
    end;

    vItem.FInUse := True;
    Result := vItem;
    Break;
  end;
end;

function TRALDBConnectionPool.IsExpiredLocked(AItem: TRALDBPoolItem): boolean;
begin
  Result := ((FOptions.MaxUses > 0) and (AItem.FUseCount >= FOptions.MaxUses)) or
            ((FOptions.MaxLifetime > 0) and
             (MilliSecondsBetween(Now, AItem.FCreatedAt) >= FOptions.MaxLifetime));
end;

procedure TRALDBConnectionPool.RemoveItemLocked(AItem: TRALDBPoolItem);
var
  vPos: IntegerRAL;
begin
  vPos := FItems.IndexOf(AItem);
  if vPos >= 0 then
    FItems.Delete(vPos);

  AItem.Free;
end;

function TRALDBConnectionPool.GetAvailableCount: IntegerRAL;
begin
  Lock;
  try
    Result := CountLocked(False, False) - CountLocked(False, True);
  finally
    Unlock;
  end;
end;

function TRALDBConnectionPool.GetInUseCount: IntegerRAL;
begin
  Lock;
  try
    Result := CountLocked(False, True) + CountLocked(True, True);
  finally
    Unlock;
  end;
end;

function TRALDBConnectionPool.GetOverflowCount: IntegerRAL;
begin
  Lock;
  try
    Result := CountLocked(True, False);
  finally
    Unlock;
  end;
end;

function TRALDBConnectionPool.GetPooledCount: IntegerRAL;
begin
  Lock;
  try
    Result := CountLocked(False, False);
  finally
    Unlock;
  end;
end;

function TRALDBConnectionPool.GetWaitingCount: IntegerRAL;
begin
  Lock;
  try
    Result := FWaiting;
  finally
    Unlock;
  end;
end;

function TRALDBConnectionPool.DoCreateConnection: TRALDBBase;
begin
  Result := nil;
  if Assigned(FOnCreateConnection) then
    Result := FOnCreateConnection(Self);

  if Result = nil then
    raise Exception.Create(emDBDriverMissing);
end;

function TRALDBConnectionPool.ExhaustedItem(ARequest: TRALRequest;
  AWaited: IntegerRAL): TRALDBPoolItem;
begin
  Result := nil;

  Lock;
  try
    Inc(FTotalTimeouts);

    if FOptions.OnExhausted = peOverflow then
    begin
      if (FOptions.MaxOverflow <= 0) or
         (CountLocked(True, False) < FOptions.MaxOverflow) then
        Result := CreateItemLocked(True);
    end;
  finally
    Unlock;
  end;

  if Assigned(FOnTimeout) then
    FOnTimeout(Self, ARequest, AWaited);

  if Result = nil then
    raise ERALDBPoolTimeout.Create(emDBPoolTimeout);
end;

function TRALDBConnectionPool.PrepareItem(AItem: TRALDBPoolItem;
  ARequest: TRALRequest; AResponse: TRALResponse): TRALDBBase;
begin
  Result := AItem.FDatabase;
  Result.Request := ARequest;
  Result.Response := AResponse;

  // connecting is done outside the lock, the item is already reserved for us
  try
    if FOptions.ValidateOnAcquire and Result.IsConnected and
       (not Result.TestConnection) then
      Result.Disconnect;

    Result.Connect;
  except
    on e: Exception do
    begin
      Lock;
      try
        RemoveItemLocked(AItem);
      finally
        Unlock;
      end;

      if Assigned(FOnError) then
        FOnError(Self, e.Message, ARequest);
      raise;
    end;
  end;

  Lock;
  try
    Inc(AItem.FUseCount);
    Inc(FTotalAcquired);
    AItem.FLastUsed := Now;
  finally
    Unlock;
  end;
end;

function TRALDBConnectionPool.Acquire(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALDBBase;
var
  vItem: TRALDBPoolItem;
  vStart: TDateTime;
  vWaited: Int64RAL;
  vQueued, vFull: boolean;
begin
  { pooling off keeps the original behaviour: a fresh driver per request, connected
    lazily by the first Open/ExecSQL }
  if not FOptions.Enabled then
  begin
    Result := DoCreateConnection;
    Result.Request := ARequest;
    Result.Response := AResponse;
    Exit;
  end;

  vItem := nil;
  vStart := Now;
  vQueued := False;
  vFull := False;

  try
    repeat
      Lock;
      try
        DropExpiredLocked;

        vItem := GetFreeItemLocked;
        if (vItem = nil) and (CountLocked(False, False) < FOptions.MaxSize) then
          vItem := CreateItemLocked(False);

        if (vItem = nil) and (not vQueued) then
        begin
          // the waiting queue is full, no point in waiting at all
          if (FOptions.QueueSize > 0) and (FWaiting >= FOptions.QueueSize) then
            vFull := True
          else
          begin
            Inc(FWaiting);
            vQueued := True;
          end;
        end;
      finally
        Unlock;
      end;

      if (vItem <> nil) or vFull then
        Break;

      vWaited := MilliSecondsBetween(Now, vStart);
      if vWaited >= FOptions.WaitTimeout then
        Break;

      { plain sleep instead of an event: waits only happen once the pool is full,
        and this keeps the unit portable across every supported Delphi and FPC }
      Sleep(cRALPoolWaitStep);
    until False;
  finally
    if vQueued then
    begin
      Lock;
      try
        Dec(FWaiting);
      finally
        Unlock;
      end;
    end;
  end;

  if vItem = nil then
    vItem := ExhaustedItem(ARequest, MilliSecondsBetween(Now, vStart));

  Result := PrepareItem(vItem, ARequest, AResponse);
end;

procedure TRALDBConnectionPool.Release(ADatabase: TRALDBBase);
var
  vItem: TRALDBPoolItem;
  vBroken: boolean;
begin
  if ADatabase = nil then
    Exit;

  // undo whatever the request left behind before anyone else gets this connection
  vBroken := False;
  try
    ADatabase.ResetSession;
  except
    vBroken := True;
  end;

  ADatabase.Request := nil;
  ADatabase.Response := nil;

  Lock;
  try
    vItem := FindItemLocked(ADatabase);
    if vItem <> nil then
    begin
      vItem.FInUse := False;
      vItem.FLastUsed := Now;

      if vBroken or vItem.FOverflow or IsExpiredLocked(vItem) then
        RemoveItemLocked(vItem);
    end;
  finally
    Unlock;
  end;

  // connections created with pooling off are not tracked, so they die here
  if vItem = nil then
    ADatabase.Free;
end;

procedure TRALDBConnectionPool.Clear;
var
  vInt: IntegerRAL;
  vItem: TRALDBPoolItem;
begin
  Lock;
  try
    for vInt := Pred(FItems.Count) downto 0 do
    begin
      vItem := TRALDBPoolItem(FItems.Items[vInt]);
      if not vItem.FInUse then
        RemoveItemLocked(vItem);
    end;
  finally
    Unlock;
  end;
end;

procedure TRALDBConnectionPool.Prepare;
var
  vList: TList;
  vInt: IntegerRAL;
  vItem: TRALDBPoolItem;
  vError: StringRAL;
begin
  if not FOptions.Enabled then
    Exit;

  vList := TList.Create;
  try
    Lock;
    try
      // reserved while being created, so no request takes them half open
      while CountLocked(False, False) < FOptions.MinSize do
        vList.Add(CreateItemLocked(False));
    finally
      Unlock;
    end;

    // opening the connections happens outside the lock
    vError := '';
    for vInt := Pred(vList.Count) downto 0 do
    begin
      vItem := TRALDBPoolItem(vList.Items[vInt]);
      try
        vItem.FDatabase.Connect;

        Lock;
        try
          vItem.FInUse := False;
        finally
          Unlock;
        end;
      except
        on e: Exception do
        begin
          if vError = '' then
            vError := e.Message;

          Lock;
          try
            RemoveItemLocked(vItem);
          finally
            Unlock;
          end;
        end;
      end;
      vList.Delete(vInt);
    end;

    if vError <> '' then
    begin
      if Assigned(FOnError) then
        FOnError(Self, vError, nil);
      raise Exception.Create(vError);
    end;
  finally
    FreeAndNil(vList);
  end;
end;

end.
