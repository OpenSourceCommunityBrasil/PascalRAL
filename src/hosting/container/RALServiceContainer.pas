{ A container of services: the application lists in the Object Inspector which class
  gives each service (by name or by interface) and how long an instance lives - one
  for the whole program, one per request, or a new one each time - and asks the
  container for it where it needs it, a route handler included. }
unit RALServiceContainer;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, TypInfo,
  RALTypes, RALConsts, RALExceptions, RALCustomObjects, RALRequest;

type
  TRALServiceContainer = class;
  TRALServiceItem = class;

  /// How long an instance of a service lives.
  TRALServiceLifetime = (
    /// One instance for the whole program, made on first use, freed with the container.
    slSingleton,
    /// One instance per request, freed when the server is done with the request.
    slPerRequest,
    /// A new instance each time it is asked for.
    slTransient
  );

  { The base of a service made by the container: reference counted, with the
    constructor the container calls, which may ask the container for the services it
    needs (ARequest is nil outside a request). }
  TRALService = class(TInterfacedObject)
  private
    FContainer: TRALServiceContainer;
    FRequest: TRALRequest;
  public
    /// Made by the container: ARequest is the request of a per-request service, else nil.
    constructor Create(AContainer: TRALServiceContainer; ARequest: TRALRequest); virtual;

    /// The container that made it.
    property Container: TRALServiceContainer read FContainer;
    /// The request it was made for (slPerRequest), else nil.
    property Request: TRALRequest read FRequest;
  end;

  /// A class of service the container can make.
  TRALServiceClass = class of TRALService;

  /// Makes the instance of AItem in code; AInstance left nil lets the container make it.
  TRALServiceCreateEvent = procedure(Sender: TObject; AItem: TRALServiceItem;
    ARequest: TRALRequest; var AInstance: TObject) of object;

  /// One service: its name, the class that gives it, its interface and lifetime.
  TRALServiceItem = class(TCollectionItem)
  private
    FInterfaceGUID: string;
    FLifetime: TRALServiceLifetime;
    FName: string;
    FOnCreate: TRALServiceCreateEvent;
    FServiceClass: string;
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(ASource: TPersistent); override;
    /// The interface as a TGUID; False when InterfaceGUID is empty or not a GUID.
    function GetGUID(out AGUID: TGUID): Boolean;
  published
    /// The interface it is asked by, as {XXXXXXXX-...}; empty = only by name.
    property InterfaceGUID: string read FInterfaceGUID write FInterfaceGUID;
    /// How long an instance lives.
    property Lifetime: TRALServiceLifetime read FLifetime write FLifetime
      default slSingleton;
    /// The name it is asked by.
    property Name: string read FName write FName;
    /// Makes the instance in code instead of from ServiceClass.
    property OnCreate: TRALServiceCreateEvent read FOnCreate write FOnCreate;
    { The class that gives the service: a TRALService or TComponent descendant made
      known by RALRegisterService, by name (TMyMailer). Empty = OnCreate makes it. }
    property ServiceClass: string read FServiceClass write FServiceClass;
  end;

  /// The services of a container.
  TRALServiceItems = class(TOwnedCollection)
  private
    function GetItem(AIndex: Integer): TRALServiceItem;
  public
    /// Creates the TRALServiceItems.
    constructor Create(AOwner: TPersistent);

    /// Adds a service.
    function Add: TRALServiceItem;
    /// The service named AName (case ignored), nil when none.
    function Find(const AName: string): TRALServiceItem;
    /// The service whose interface is AGUID, nil when none.
    function FindGUID(const AGUID: TGUID): TRALServiceItem;

    /// The service at AIndex.
    property Items[AIndex: Integer]: TRALServiceItem read GetItem; default;
  end;

  { The instances one request made: kept until the server is done with the request,
    then freed. It frees itself, so a container freed first does not leave the
    request a dangling handler. }
  TRALServiceScope = class
  private
    /// The container, nil once it is freed.
    FContainer: TRALServiceContainer;
    /// Interfaces kept alive (TRALService instances).
    FInterfaces: array of IInterface;
    /// The names of the instances, the object in Objects.
    FNames: TStringList;
    /// The request it belongs to.
    FRequest: TRALRequest;
  public
    /// Creates the scope of ARequest.
    constructor Create(AContainer: TRALServiceContainer; ARequest: TRALRequest);
    destructor Destroy; override;

    /// Keeps AInstance under AName.
    procedure Add(const AName: string; AInstance: TObject);
    /// The instance kept under AName, nil when none.
    function Find(const AName: string): TObject;
    /// Frees the instances and itself: the request's finish handler.
    procedure RequestFinished(ARequest: TRALRequest);
  end;

  { Called after the container makes an instance, so another unit can fill it
    (RALServiceInject fills the fields marked [Inject] where the compiler has the
    RTTI for it). }
  TRALServiceAfterCreate = procedure(AContainer: TRALServiceContainer; AInstance: TObject;
    ARequest: TRALRequest);

  { The container. Ask by name (Resolve, ResolveObject) or by interface (Resolve
    with a GUID, ResolveAs); singletons are made once under a lock, per-request
    instances go with the request (ARequest is the request of the route handler),
    and a transient object belongs to the caller. A TRALService is counted by
    reference: ask for it as an interface. }
  TRALServiceContainer = class(TRALComponent)
  private
    /// Guards the singletons and the scopes.
    FLock: TCriticalSection;
    FOnCreateService: TRALServiceCreateEvent;
    /// The scopes of the requests in course, sorted by request.
    FScopes: TList;
    FServices: TRALServiceItems;
    /// Interfaces of the singletons kept alive.
    FSingletonIntfs: array of IInterface;
    /// The singletons made, by name; the object in Objects.
    FSingletons: TStringList;

    /// Makes an instance of AItem.
    function CreateInstance(AItem: TRALServiceItem; ARequest: TRALRequest): TObject;
    /// The instance of AItem for ARequest, by its lifetime.
    function Instance(AItem: TRALServiceItem; ARequest: TRALRequest): TObject;
    /// The scope of ARequest, made when ACreate; nil otherwise.
    function ScopeOf(ARequest: TRALRequest; ACreate: Boolean): TRALServiceScope;
    procedure SetServices(AValue: TRALServiceItems);
  protected
    /// A scope is done: forgets it.
    procedure ScopeFinished(AScope: TRALServiceScope);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Adds a service in code: AServiceClass is a registered class name, AGUID may
      be empty. }
    function AddService(const AName, AServiceClass: string;
      ALifetime: TRALServiceLifetime; const AGUID: string = ''): TRALServiceItem;
    /// Frees the singletons; the next Resolve makes them again.
    procedure Reset;
    { The service AName as an interface (a TRALService, or an object that implements
      interfaces); raises when it is unknown or the instance has no interface. }
    function Resolve(const AName: string; ARequest: TRALRequest = nil): IInterface; overload;
    { The service whose interface is AGUID (or the first whose instance supports it),
      in AService; False when none. }
    function Resolve(const AGUID: TGUID; out AService; ARequest: TRALRequest = nil): Boolean;
      overload;
    {$IFDEF RALHasGenerics}
    /// The service of interface T (an interface with a GUID); raises when none.
    function ResolveAs<T: IInterface>(ARequest: TRALRequest = nil): T;
    {$ENDIF}
    { The service AName as an object: a singleton or per-request one belongs to the
      container, a transient one to the caller, who frees it. }
    function ResolveObject(const AName: string; ARequest: TRALRequest = nil): TObject;
    /// Whether a service named AName is listed.
    function Supports(const AName: string): Boolean;
  published
    /// Makes the instances of every service whose own OnCreate did not.
    property OnCreateService: TRALServiceCreateEvent read FOnCreateService
      write FOnCreateService;
    /// The services.
    property Services: TRALServiceItems read FServices write SetServices;
  end;

{ Makes AClass (a TRALService or TComponent descendant) known by its name to every
  container; call it in the initialization section of the unit that declares it. }
procedure RALRegisterService(AClass: TClass);
/// Forgets AClass.
procedure RALUnregisterService(AClass: TClass);
/// The registered class named AName, nil when none.
function RALFindService(const AName: string): TClass;

var
  { Set by RALServiceInject (where the compiler has attributes and extended RTTI):
    fills the fields of a new instance marked [Inject]. }
  RALServiceAfterCreateHook: TRALServiceAfterCreate = nil;

implementation

const
  /// How deep services may ask for services while they are made.
  cMaxDepth = 32;

var
  /// The registered classes, by name.
  GClasses: TStringList = nil;
  /// Guards GClasses.
  GClassLock: TCriticalSection = nil;

threadvar
  /// How deep the current thread is in making services.
  GDepth: Integer;

procedure RALRegisterService(AClass: TClass);
var
  vIdx: Integer;
begin
  if AClass = nil then
    Exit;
  GClassLock.Enter;
  try
    vIdx := GClasses.IndexOf(AClass.ClassName);
    if vIdx < 0 then
      GClasses.AddObject(AClass.ClassName, TObject(AClass))
    else
      GClasses.Objects[vIdx] := TObject(AClass);
  finally
    GClassLock.Leave;
  end;
end;

procedure RALUnregisterService(AClass: TClass);
var
  vIdx: Integer;
begin
  if (AClass = nil) or (GClassLock = nil) then
    Exit;
  GClassLock.Enter;
  try
    vIdx := GClasses.IndexOf(AClass.ClassName);
    if (vIdx >= 0) and (GClasses.Objects[vIdx] = TObject(AClass)) then
      GClasses.Delete(vIdx);
  finally
    GClassLock.Leave;
  end;
end;

function RALFindService(const AName: string): TClass;
var
  vIdx: Integer;
begin
  Result := nil;
  GClassLock.Enter;
  try
    vIdx := GClasses.IndexOf(Trim(AName));
    if vIdx >= 0 then
      Result := TClass(GClasses.Objects[vIdx]);
  finally
    GClassLock.Leave;
  end;
end;

{ TRALService }

constructor TRALService.Create(AContainer: TRALServiceContainer; ARequest: TRALRequest);
begin
  inherited Create;
  FContainer := AContainer;
  FRequest := ARequest;
end;

{ TRALServiceItem }

procedure TRALServiceItem.Assign(ASource: TPersistent);
var
  vSource: TRALServiceItem;
begin
  if ASource is TRALServiceItem then
  begin
    vSource := TRALServiceItem(ASource);
    FServiceClass := vSource.FServiceClass;
    FInterfaceGUID := vSource.FInterfaceGUID;
    FLifetime := vSource.FLifetime;
    FName := vSource.FName;
    FOnCreate := vSource.FOnCreate;
  end
  else
    inherited Assign(ASource);
end;

function TRALServiceItem.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := inherited GetDisplayName;
end;

function TRALServiceItem.GetGUID(out AGUID: TGUID): Boolean;
var
  vText: string;
begin
  vText := Trim(FInterfaceGUID);
  Result := vText <> '';
  if not Result then
    Exit;
  if vText[1] <> '{' then
    vText := '{' + vText + '}';
  try
    AGUID := StringToGUID(vText);
  except
    Result := False;
  end;
end;

{ TRALServiceItems }

function TRALServiceItems.Add: TRALServiceItem;
begin
  Result := TRALServiceItem(inherited Add);
end;

constructor TRALServiceItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALServiceItem);
end;

function TRALServiceItems.Find(const AName: string): TRALServiceItem;
var
  vIdx: Integer;
begin
  Result := nil;
  for vIdx := 0 to Count - 1 do
    if SameText(Items[vIdx].Name, AName) then
    begin
      Result := Items[vIdx];
      Exit;
    end;
end;

function TRALServiceItems.FindGUID(const AGUID: TGUID): TRALServiceItem;
var
  vIdx: Integer;
  vGUID: TGUID;
begin
  Result := nil;
  for vIdx := 0 to Count - 1 do
    if Items[vIdx].GetGUID(vGUID) and IsEqualGUID(vGUID, AGUID) then
    begin
      Result := Items[vIdx];
      Exit;
    end;
end;

function TRALServiceItems.GetItem(AIndex: Integer): TRALServiceItem;
begin
  Result := TRALServiceItem(inherited Items[AIndex]);
end;

{ TRALServiceScope }

procedure TRALServiceScope.Add(const AName: string; AInstance: TObject);
var
  vIntf: IInterface;
begin
  FNames.AddObject(AName, AInstance);
  // a reference-counted instance lives while its interface is kept here
  if (AInstance is TInterfacedObject) and
     SysUtils.Supports(AInstance, IInterface, vIntf) then
  begin
    SetLength(FInterfaces, Length(FInterfaces) + 1);
    FInterfaces[High(FInterfaces)] := vIntf;
  end;
end;

constructor TRALServiceScope.Create(AContainer: TRALServiceContainer; ARequest: TRALRequest);
begin
  inherited Create;
  FContainer := AContainer;
  FRequest := ARequest;
  FNames := TStringList.Create;
end;

destructor TRALServiceScope.Destroy;
var
  vIdx: Integer;
begin
  // the interfaces free their objects; the others are freed here, newest first
  for vIdx := FNames.Count - 1 downto 0 do
    if not (FNames.Objects[vIdx] is TInterfacedObject) then
      FNames.Objects[vIdx].Free;
  for vIdx := High(FInterfaces) downto 0 do
    FInterfaces[vIdx] := nil;
  FInterfaces := nil;
  FNames.Free;
  inherited Destroy;
end;

function TRALServiceScope.Find(const AName: string): TObject;
var
  vIdx: Integer;
begin
  vIdx := FNames.IndexOf(AName);
  if vIdx >= 0 then
    Result := FNames.Objects[vIdx]
  else
    Result := nil;
end;

procedure TRALServiceScope.RequestFinished(ARequest: TRALRequest);
begin
  if FContainer <> nil then
    FContainer.ScopeFinished(Self);
  Free;
end;

{ TRALServiceContainer }

function TRALServiceContainer.AddService(const AName, AServiceClass: string;
  ALifetime: TRALServiceLifetime; const AGUID: string): TRALServiceItem;
begin
  Result := FServices.Add;
  Result.Name := AName;
  Result.ServiceClass := AServiceClass;
  Result.Lifetime := ALifetime;
  Result.InterfaceGUID := AGUID;
end;

constructor TRALServiceContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FScopes := TList.Create;
  FServices := TRALServiceItems.Create(Self);
  FSingletons := TStringList.Create;
end;

function TRALServiceContainer.CreateInstance(AItem: TRALServiceItem;
  ARequest: TRALRequest): TObject;
var
  vClass: TClass;
begin
  Result := nil;
  if GDepth >= cMaxDepth then
    raise ERALException.CreateFmt(emContainerTooDeep, [AItem.Name]);
  Inc(GDepth);
  try
    if Assigned(AItem.OnCreate) then
      AItem.OnCreate(Self, AItem, ARequest, Result);
    if (Result = nil) and (AItem.ServiceClass <> '') then
    begin
      vClass := RALFindService(AItem.ServiceClass);
      if vClass = nil then
        raise ERALException.CreateFmt(emContainerUnknownClass,
          [AItem.ServiceClass, AItem.Name]);
      if vClass.InheritsFrom(TRALService) then
        Result := TRALServiceClass(vClass).Create(Self, ARequest)
      else if vClass.InheritsFrom(TComponent) then
        Result := TComponentClass(vClass).Create(nil)
      else
        raise ERALException.CreateFmt(emContainerBadClass, [AItem.ServiceClass]);
    end;
    if (Result = nil) and Assigned(FOnCreateService) then
      FOnCreateService(Self, AItem, ARequest, Result);
    if Result = nil then
      raise ERALException.CreateFmt(emContainerNoFactory, [AItem.Name]);
    if Assigned(RALServiceAfterCreateHook) then
      RALServiceAfterCreateHook(Self, Result, ARequest);
  finally
    Dec(GDepth);
  end;
end;

destructor TRALServiceContainer.Destroy;
var
  vIdx: Integer;
begin
  // the scopes of requests still running free themselves when they finish
  FLock.Enter;
  try
    for vIdx := 0 to FScopes.Count - 1 do
      TRALServiceScope(FScopes[vIdx]).FContainer := nil;
    FScopes.Clear;
  finally
    FLock.Leave;
  end;
  Reset;
  FreeAndNil(FSingletons);
  FreeAndNil(FServices);
  FreeAndNil(FScopes);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TRALServiceContainer.Instance(AItem: TRALServiceItem;
  ARequest: TRALRequest): TObject;
var
  vIdx: Integer;
  vScope: TRALServiceScope;
  vIntf: IInterface;
begin
  case AItem.Lifetime of
    slSingleton:
    begin
      FLock.Enter;
      try
        vIdx := FSingletons.IndexOf(AItem.Name);
        if vIdx >= 0 then
        begin
          Result := FSingletons.Objects[vIdx];
          Exit;
        end;
        // made under the lock: two threads never make two singletons
        Result := CreateInstance(AItem, nil);
        FSingletons.AddObject(AItem.Name, Result);
        if (Result is TInterfacedObject) and
           SysUtils.Supports(Result, IInterface, vIntf) then
        begin
          SetLength(FSingletonIntfs, Length(FSingletonIntfs) + 1);
          FSingletonIntfs[High(FSingletonIntfs)] := vIntf;
        end;
      finally
        FLock.Leave;
      end;
    end;
    slPerRequest:
    begin
      if ARequest = nil then
        raise ERALException.CreateFmt(emContainerNeedsRequest, [AItem.Name]);
      vScope := ScopeOf(ARequest, True);
      Result := vScope.Find(AItem.Name);
      if Result = nil then
      begin
        Result := CreateInstance(AItem, ARequest);
        vScope.Add(AItem.Name, Result);
      end;
    end;
  else
    Result := CreateInstance(AItem, ARequest);
  end;
end;

procedure TRALServiceContainer.Reset;
var
  vIdx: Integer;
begin
  FLock.Enter;
  try
    for vIdx := FSingletons.Count - 1 downto 0 do
      if not (FSingletons.Objects[vIdx] is TInterfacedObject) then
        FSingletons.Objects[vIdx].Free;
    FSingletons.Clear;
    for vIdx := High(FSingletonIntfs) downto 0 do
      FSingletonIntfs[vIdx] := nil;
    FSingletonIntfs := nil;
  finally
    FLock.Leave;
  end;
end;

function TRALServiceContainer.Resolve(const AName: string; ARequest: TRALRequest): IInterface;
var
  vItem: TRALServiceItem;
  vObject: TObject;
begin
  vItem := FServices.Find(AName);
  if vItem = nil then
    raise ERALException.CreateFmt(emContainerUnknown, [AName]);
  vObject := Instance(vItem, ARequest);
  if not SysUtils.Supports(vObject, IInterface, Result) then
  begin
    if vItem.Lifetime = slTransient then
      vObject.Free;
    raise ERALException.CreateFmt(emContainerNoInterface, [AName]);
  end;
end;

function TRALServiceContainer.Resolve(const AGUID: TGUID; out AService;
  ARequest: TRALRequest): Boolean;
var
  vItem: TRALServiceItem;
  vObject: TObject;
  vIdx: Integer;
  vClass: TClass;
begin
  Pointer(AService) := nil;
  vItem := FServices.FindGUID(AGUID);
  if vItem = nil then
    // no GUID listed: the first service whose class implements it
    for vIdx := 0 to FServices.Count - 1 do
    begin
      vClass := RALFindService(FServices[vIdx].ServiceClass);
      if (vClass <> nil) and (vClass.GetInterfaceEntry(AGUID) <> nil) then
      begin
        vItem := FServices[vIdx];
        Break;
      end;
    end;
  Result := vItem <> nil;
  if not Result then
    Exit;
  vObject := Instance(vItem, ARequest);
  Result := SysUtils.Supports(vObject, AGUID, AService);
  if (not Result) and (vItem.Lifetime = slTransient) then
    vObject.Free;
end;

{$IFDEF RALHasGenerics}
function TRALServiceContainer.ResolveAs<T>(ARequest: TRALRequest): T;
var
  vData: PTypeData;
begin
  vData := GetTypeData(PTypeInfo(TypeInfo(T)));
  if not Resolve(vData^.GUID, Result, ARequest) then
    raise ERALException.CreateFmt(emContainerUnknown, [GUIDToString(vData^.GUID)]);
end;
{$ENDIF}

function TRALServiceContainer.ResolveObject(const AName: string;
  ARequest: TRALRequest): TObject;
var
  vItem: TRALServiceItem;
begin
  vItem := FServices.Find(AName);
  if vItem = nil then
    raise ERALException.CreateFmt(emContainerUnknown, [AName]);
  Result := Instance(vItem, ARequest);
end;

procedure TRALServiceContainer.ScopeFinished(AScope: TRALServiceScope);
var
  vIdx: Integer;
begin
  FLock.Enter;
  try
    vIdx := FScopes.IndexOf(AScope);
    if vIdx >= 0 then
      FScopes.Delete(vIdx);
  finally
    FLock.Leave;
  end;
end;

function TRALServiceContainer.ScopeOf(ARequest: TRALRequest;
  ACreate: Boolean): TRALServiceScope;
var
  vIdx: Integer;
begin
  Result := nil;
  FLock.Enter;
  try
    for vIdx := FScopes.Count - 1 downto 0 do
      if TRALServiceScope(FScopes[vIdx]).FRequest = ARequest then
      begin
        Result := TRALServiceScope(FScopes[vIdx]);
        Exit;
      end;
    if not ACreate then
      Exit;
    Result := TRALServiceScope.Create(Self, ARequest);
    FScopes.Add(Result);
  finally
    FLock.Leave;
  end;
  // outside the lock: a request already finished calls the handler at once
  ARequest.AddFinishHandler(Result.RequestFinished);
end;

procedure TRALServiceContainer.SetServices(AValue: TRALServiceItems);
begin
  FServices.Assign(AValue);
end;

function TRALServiceContainer.Supports(const AName: string): Boolean;
begin
  Result := FServices.Find(AName) <> nil;
end;

initialization
  GClasses := TStringList.Create;
  GClasses.Sorted := True;
  GClasses.CaseSensitive := False;
  GClassLock := TCriticalSection.Create;

finalization
  FreeAndNil(GClassLock);
  FreeAndNil(GClasses);

end.
