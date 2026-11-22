{ Resources with state: routes written as methods of a class (TRALStatefulResource)
  whose instance lives for the server, one user session or one request (Lifetime).
  Calls to one instance run one at a time, so its fields need no lock. }
unit RALStatefulModule;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALExceptions, RALTools, RALServer, RALRoutes, RALRequest,
  RALResponse, RALParams;

type
  TRALStatefulModule = class;

  /// How long an instance of the resource lives.
  TRALModuleLifetime = (
    /// One instance for every request, made when the server starts.
    mlShared,
    { One instance per user session (a cookie), freed SessionTimeout seconds after
      the session's last request. }
    mlPerSession,
    /// A new instance for each request, freed when it ends.
    mlPerRequest
  );

  { The base of a resource: declare its routes in DefineRoutes with Route, write the
    handlers as methods, keep state in fields. }
  TRALStatefulResource = class(TComponent)
  private
    FModule: TRALStatefulModule;
    FSessionId: StringRAL;
  protected
    { Declares the routes of the resource with Route; called on a first instance when
      the server starts. }
    procedure DefineRoutes; virtual; abstract;
    { A route of the resource: APath under the module's Domain, answered by AHandler,
      a method of this class (the module calls it on the instance of the request). }
    procedure Route(const APath: StringRAL; AHandler: TRALOnReply;
      const ADescription: StringRAL = '');
  public
    /// The module that made it.
    property Module: TRALStatefulModule read FModule;
    /// The session it belongs to (mlPerSession); '' otherwise.
    property SessionId: StringRAL read FSessionId;
  end;

  /// A class of resource.
  TRALStatefulResourceClass = class of TRALStatefulResource;

  /// An instance was made (AResource) or is about to be freed.
  TRALStatefulResourceEvent = procedure(Sender: TObject;
    AResource: TRALStatefulResource) of object;

  /// One instance and what the module knows about it.
  TRALStatefulHolder = class
  public
    /// When it was last used (ms tick).
    LastUse: Int64;
    /// Serializes the calls to the instance.
    Lock: TCriticalSection;
    /// The instance.
    Resource: TRALStatefulResource;
    /// Calls running or waiting on it; it is never freed while above 0.
    Users: Integer;

    /// Holds AResource (it owns it).
    constructor Create(AResource: TRALStatefulResource);
    destructor Destroy; override;
  end;

  /// One route of the resource: which method answers it.
  TRALStatefulRoute = class
  private
    /// The method's code; the instance is the request's.
    FCode: Pointer;
    /// The module.
    FModule: TRALStatefulModule;
    /// The route made on the module.
    FRoute: TRALRoute;
  public
    /// Runs the method on the instance of ARequest.
    procedure Handle(ARequest: TRALRequest; AResponse: TRALResponse);
  end;

  { The module. Set ResourceClass to a class made known with RALRegisterResource and
    choose Lifetime; the routes of the class appear under Domain when the server starts. }
  TRALStatefulModule = class(TRALModuleRoutes)
  private
    FCookieName: StringRAL;
    /// Whether DefineRoutes is running (Route adds routes then).
    FDefining: Boolean;
    /// Instances by session id; the objects are TRALStatefulHolder.
    FHolders: TStringList;
    /// When the expired sessions were last looked for (ms tick).
    FLastSweep: Int64;
    FLifetime: TRALModuleLifetime;
    /// Guards the holders.
    FLock: TCriticalSection;
    FMaxSessions: Integer;
    FOnCreateResource: TRALStatefulResourceEvent;
    FOnFreeResource: TRALStatefulResourceEvent;
    FResourceClass: string;
    /// The routes made from the resource.
    FResourceRoutes: TList;
    FSessionTimeout: Integer;
    /// The instance of mlShared.
    FShared: TRALStatefulHolder;

    /// Takes the holder of ARequest (a user counted on it), making it as needed.
    function Acquire(ARequest: TRALRequest; AResponse: TRALResponse): TRALStatefulHolder;
    /// Adds a route for the method ACode at APath.
    procedure AddResourceRoute(const APath: StringRAL; ACode: Pointer;
      const ADescription: StringRAL);
    /// Frees AHolder and its instance (OnFreeResource first).
    procedure FreeHolder(AHolder: TRALStatefulHolder);
    /// A new instance of ResourceClass.
    function NewResource(const ASessionId: StringRAL): TRALStatefulResource;
    /// A new session id: 32 hex digits from the system's random source.
    function NewSessionId: StringRAL;
    /// Gives back a holder Acquire took; frees a per-request one.
    procedure Release(AHolder: TRALStatefulHolder);
    /// Frees the routes made from the resource.
    procedure RemoveResourceRoutes;
    /// Sends the session cookie.
    procedure SendCookie(AResponse: TRALResponse; const AId: StringRAL);
    /// Frees the routes, the sessions and the shared instance.
    procedure Shutdown;
    /// Frees the sessions idle longer than SessionTimeout; under the lock.
    procedure Sweep(ANow: Int64);
  protected
    procedure ServerActivating; override;
    procedure ServerDeactivating; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Sessions alive now (mlPerSession).
    function SessionCount: Integer;
  published
    /// The cookie of the session (mlPerSession).
    property CookieName: StringRAL read FCookieName write FCookieName;
    /// How long an instance lives.
    property Lifetime: TRALModuleLifetime read FLifetime write FLifetime default mlPerSession;
    { Most sessions alive at once; past it the session idle the longest is freed.
      0 = no limit. }
    property MaxSessions: Integer read FMaxSessions write FMaxSessions default 10000;
    /// An instance was made (and is about to answer its first request).
    property OnCreateResource: TRALStatefulResourceEvent read FOnCreateResource
      write FOnCreateResource;
    /// An instance is about to be freed (its session expired, or the server stopped).
    property OnFreeResource: TRALStatefulResourceEvent read FOnFreeResource
      write FOnFreeResource;
    /// The class of the resource, by name (made known with RALRegisterResource).
    property ResourceClass: string read FResourceClass write FResourceClass;
    /// Seconds a session lives after its last request (mlPerSession).
    property SessionTimeout: Integer read FSessionTimeout write FSessionTimeout default 1200;
  end;

{ Makes AClass known by its name to TRALStatefulModule.ResourceClass; call it in the
  initialization section of the unit that declares it. }
procedure RALRegisterResource(AClass: TRALStatefulResourceClass);
/// The registered class named AName, nil when none.
function RALFindResource(const AName: string): TRALStatefulResourceClass;

implementation

const
  /// The default cookie name.
  cDefaultCookie = 'ralres';

var
  /// The registered classes by name.
  GClasses: TStringList = nil;
  /// Guards GClasses.
  GClassLock: TCriticalSection = nil;

procedure RALRegisterResource(AClass: TRALStatefulResourceClass);
var
  vIdx: Integer;
begin
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

function RALFindResource(const AName: string): TRALStatefulResourceClass;
var
  vIdx: Integer;
begin
  Result := nil;
  GClassLock.Enter;
  try
    vIdx := GClasses.IndexOf(Trim(AName));
    if vIdx >= 0 then
      Result := TRALStatefulResourceClass(GClasses.Objects[vIdx]);
  finally
    GClassLock.Leave;
  end;
end;

{ TRALStatefulResource }

procedure TRALStatefulResource.Route(const APath: StringRAL; AHandler: TRALOnReply;
  const ADescription: StringRAL);
begin
  if (FModule = nil) or not FModule.FDefining then
    raise ERALException.CreateFmt(emStatefulRouteOutside, [ClassName]);
  if TMethod(AHandler).Data <> Pointer(Self) then
    raise ERALException.CreateFmt(emStatefulForeignHandler, [ClassName, string(APath)]);
  FModule.AddResourceRoute(APath, TMethod(AHandler).Code, ADescription);
end;

{ TRALStatefulHolder }

constructor TRALStatefulHolder.Create(AResource: TRALStatefulResource);
begin
  inherited Create;
  Lock := TCriticalSection.Create;
  Resource := AResource;
  LastUse := RALTickMicro div 1000;
end;

destructor TRALStatefulHolder.Destroy;
begin
  FreeAndNil(Resource);
  FreeAndNil(Lock);
  inherited Destroy;
end;

{ TRALStatefulRoute }

procedure TRALStatefulRoute.Handle(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vHolder: TRALStatefulHolder;
  vMethod: TMethod;
begin
  vHolder := FModule.Acquire(ARequest, AResponse);
  try
    vHolder.Lock.Enter;
    try
      vMethod.Code := FCode;
      vMethod.Data := vHolder.Resource;
      TRALOnReply(vMethod)(ARequest, AResponse);
    finally
      vHolder.Lock.Leave;
    end;
  finally
    FModule.Release(vHolder);
  end;
end;

{ TRALStatefulModule }

function TRALStatefulModule.Acquire(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALStatefulHolder;
var
  vId: StringRAL;
  vIdx: Integer;
  vNow: Int64;
  vNew: Boolean;
begin
  case FLifetime of
    mlShared:
    begin
      Result := FShared;
      FLock.Enter;
      try
        Inc(Result.Users);
      finally
        FLock.Leave;
      end;
    end;
    mlPerRequest:
    begin
      Result := TRALStatefulHolder.Create(NewResource(''));
      Result.Users := 1;
    end;
  else
    vId := ARequest.Params.GetKind[FCookieName, rpkCOOKIE].AsString;
    vNow := RALTickMicro div 1000;
    vNew := False;
    FLock.Enter;
    try
      Sweep(vNow);
      vIdx := -1;
      if vId <> '' then
        vIdx := FHolders.IndexOf(string(vId));
      if vIdx >= 0 then
        Result := TRALStatefulHolder(FHolders.Objects[vIdx])
      else
      begin
        // a cookie the module does not know (expired, forged) starts a new session
        vId := NewSessionId;
        Result := TRALStatefulHolder.Create(NewResource(vId));
        FHolders.AddObject(string(vId), Result);
        vNew := True;
      end;
      Inc(Result.Users);
      Result.LastUse := vNow;
    finally
      FLock.Leave;
    end;
    if vNew then
      SendCookie(AResponse, vId);
  end;
end;

procedure TRALStatefulModule.AddResourceRoute(const APath: StringRAL; ACode: Pointer;
  const ADescription: StringRAL);
var
  vRoute: TRALStatefulRoute;
begin
  vRoute := TRALStatefulRoute.Create;
  vRoute.FModule := Self;
  vRoute.FCode := ACode;
  vRoute.FRoute := CreateRoute(APath, vRoute.Handle, ADescription);
  FResourceRoutes.Add(vRoute);
end;

constructor TRALStatefulModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCookieName := cDefaultCookie;
  FHolders := TStringList.Create;
  FHolders.Sorted := True;
  FLifetime := mlPerSession;
  FLock := TCriticalSection.Create;
  FMaxSessions := 10000;
  FResourceRoutes := TList.Create;
  FSessionTimeout := 1200;
end;

destructor TRALStatefulModule.Destroy;
begin
  Shutdown;
  FreeAndNil(FResourceRoutes);
  FreeAndNil(FHolders);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALStatefulModule.FreeHolder(AHolder: TRALStatefulHolder);
begin
  if Assigned(FOnFreeResource) and (AHolder.Resource <> nil) then
  try
    FOnFreeResource(Self, AHolder.Resource);
  except
    // the instance goes anyway
  end;
  AHolder.Free;
end;

function TRALStatefulModule.NewResource(const ASessionId: StringRAL): TRALStatefulResource;
var
  vClass: TRALStatefulResourceClass;
begin
  vClass := RALFindResource(FResourceClass);
  if vClass = nil then
    raise ERALException.CreateFmt(emStatefulUnknownClass, [FResourceClass]);
  Result := vClass.Create(nil);
  Result.FModule := Self;
  Result.FSessionId := ASessionId;
  if Assigned(FOnCreateResource) then
    FOnCreateResource(Self, Result);
end;

function TRALStatefulModule.NewSessionId: StringRAL;
var
  vBytes: TBytes;
  vIdx: Integer;
begin
  vBytes := RandomBytes(16);
  Result := '';
  for vIdx := 0 to High(vBytes) do
    Result := Result + StringRAL(IntToHex(vBytes[vIdx], 2));
end;

procedure TRALStatefulModule.Release(AHolder: TRALStatefulHolder);
begin
  if FLifetime = mlPerRequest then
  begin
    FreeHolder(AHolder);
    Exit;
  end;
  FLock.Enter;
  try
    Dec(AHolder.Users);
    AHolder.LastUse := RALTickMicro div 1000;
  finally
    FLock.Leave;
  end;
end;

procedure TRALStatefulModule.RemoveResourceRoutes;
var
  vIdx: Integer;
begin
  for vIdx := FResourceRoutes.Count - 1 downto 0 do
  begin
    TRALStatefulRoute(FResourceRoutes[vIdx]).FRoute.Free;
    TRALStatefulRoute(FResourceRoutes[vIdx]).Free;
  end;
  FResourceRoutes.Clear;
end;

procedure TRALStatefulModule.SendCookie(AResponse: TRALResponse; const AId: StringRAL);
var
  vCookie: TRALCookie;
begin
  Finalize(vCookie);
  FillChar(vCookie, SizeOf(vCookie), 0);
  vCookie.Name := FCookieName;
  vCookie.Value := AId;
  vCookie.Path := '/';
  vCookie.HttpOnly := True;
  vCookie.SessionOnly := True;
  vCookie.Secure := (Server <> nil) and Server.SSLEnabled;
  vCookie.SameSite := cssLax;
  AResponse.AddCookie(vCookie);
end;

procedure TRALStatefulModule.ServerActivating;
var
  vFirst: TRALStatefulResource;
begin
  inherited ServerActivating;
  if FResourceRoutes.Count > 0 then
    Exit;
  vFirst := NewResource('');
  FDefining := True;
  try
    vFirst.DefineRoutes;
  finally
    FDefining := False;
  end;
  if FLifetime = mlShared then
    FShared := TRALStatefulHolder.Create(vFirst)
  else
  begin
    if Assigned(FOnFreeResource) then
      FOnFreeResource(Self, vFirst);
    vFirst.Free;
  end;
end;

procedure TRALStatefulModule.ServerDeactivating;
begin
  Shutdown;
  inherited ServerDeactivating;
end;

procedure TRALStatefulModule.Shutdown;
var
  vIdx: Integer;
begin
  if FLock = nil then
    Exit;
  RemoveResourceRoutes;
  FLock.Enter;
  try
    for vIdx := FHolders.Count - 1 downto 0 do
      FreeHolder(TRALStatefulHolder(FHolders.Objects[vIdx]));
    FHolders.Clear;
    if FShared <> nil then
      FreeHolder(FShared);
    FShared := nil;
  finally
    FLock.Leave;
  end;
end;

function TRALStatefulModule.SessionCount: Integer;
begin
  FLock.Enter;
  try
    Result := FHolders.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TRALStatefulModule.Sweep(ANow: Int64);
var
  vIdx, vOldest: Integer;
  vHolder: TRALStatefulHolder;
begin
  if ANow - FLastSweep >= 1000 then
  begin
    FLastSweep := ANow;
    for vIdx := FHolders.Count - 1 downto 0 do
    begin
      vHolder := TRALStatefulHolder(FHolders.Objects[vIdx]);
      if (vHolder.Users = 0) and (ANow - vHolder.LastUse >= Int64(FSessionTimeout) * 1000) then
      begin
        FHolders.Delete(vIdx);
        FreeHolder(vHolder);
      end;
    end;
  end;
  // at the limit, the session idle the longest goes
  while (FMaxSessions > 0) and (FHolders.Count >= FMaxSessions) do
  begin
    vOldest := -1;
    for vIdx := 0 to FHolders.Count - 1 do
      if (TRALStatefulHolder(FHolders.Objects[vIdx]).Users = 0) and ((vOldest < 0) or
         (TRALStatefulHolder(FHolders.Objects[vIdx]).LastUse <
          TRALStatefulHolder(FHolders.Objects[vOldest]).LastUse)) then
        vOldest := vIdx;
    if vOldest < 0 then
      Break;
    vHolder := TRALStatefulHolder(FHolders.Objects[vOldest]);
    FHolders.Delete(vOldest);
    FreeHolder(vHolder);
  end;
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
