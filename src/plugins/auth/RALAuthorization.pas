/// Authorization policies: what a route asks of an identity an authenticator
/// already accepted (roles, claims, scopes, an event), by name.
unit RALAuthorization;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools, RALRequest, RALResponse, RALRoutes, RALPlugin,
  RALCustomObjects, RALAuthBase;

type
  TRALAuthPolicy = class;

  /// Decides a policy for AIdentity; AAllowed comes in with what the rules said.
  TRALOnEvaluatePolicy = procedure(APolicy: TRALAuthPolicy; AIdentity: TRALIdentity;
    ARequest: TRALRequest; var AAllowed: boolean) of object;

  { A named policy: everything set must hold. Roles (any, or all with
    RequireAllRoles), Claims (one 'name=value' per line; 'name=*' only asks the
    claim to exist; several values separated by '|'), Scopes (every one, read
    from the scope or scp claim), then OnEvaluate with the result. }
  TRALAuthPolicy = class(TCollectionItem)
  private
    FClaims: TStrings;
    FName: StringRAL;
    FOnEvaluate: TRALOnEvaluatePolicy;
    FRequireAllRoles: boolean;
    FRoles: StringRAL;
    FScopes: StringRAL;

    procedure SetClaims(const AValue: TStrings);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    /// Whether AIdentity passes the policy, for ARequest.
    function Evaluate(AIdentity: TRALIdentity; ARequest: TRALRequest): boolean;
  published
    /// Claims the identity must carry, one 'name=value' per line ('*' = any value, '|' separates values).
    property Claims: TStrings read FClaims write SetClaims;
    /// Name routes and groups give in Policy.
    property Name: StringRAL read FName write FName;
    /// Fired last, with the verdict of the rules above; may change it.
    property OnEvaluate: TRALOnEvaluatePolicy read FOnEvaluate write FOnEvaluate;
    /// With Roles, every role is needed, not just one.
    property RequireAllRoles: boolean read FRequireAllRoles write FRequireAllRoles default False;
    /// Roles (separated by commas) of which the identity needs one, or all.
    property Roles: StringRAL read FRoles write FRoles;
    /// Scopes (separated by blanks or commas) the scope claim must have, all of them.
    property Scopes: StringRAL read FScopes write FScopes;
  end;

  /// The policies of a TRALAuthorizationPlugin.
  TRALAuthPolicies = class(TOwnedCollection)
  private
    function GetPolicy(AIndex: IntegerRAL): TRALAuthPolicy;
  public
    constructor Create(AOwner: TPersistent);

    /// Adds a policy named AName asking ARoles (any of them).
    function AddPolicy(const AName: StringRAL; const ARoles: StringRAL = ''): TRALAuthPolicy;
    /// The policy named AName (case-insensitive), or nil.
    function Find(const AName: StringRAL): TRALAuthPolicy;

    /// Policies by index.
    property Policies[AIndex: IntegerRAL]: TRALAuthPolicy read GetPolicy; default;
  end;

  { Judges the Policy of the route (or of its group) once an authenticator
    accepted the request, in the ppAuthResult phase: a policy that fails, or
    that does not exist, is 403. The route's Roles are judged by the core
    itself; a route that names a policy and finds no plugin to judge it is 403.
    Link it to the server like any plugin. }
  TRALAuthorizationPlugin = class(TRALPlugin)
  private
    FPolicies: TRALAuthPolicies;

    procedure SetPolicies(const AValue: TRALAuthPolicies);
  protected
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// ppAuthResult: an accepted request whose route names a policy must pass it.
    procedure AfterAuthenticate(ARequest: TRALRequest; AResponse: TRALResponse;
      var AResult: TRALAuthResult); override;
    /// Whether AIdentity passes the policy named APolicy; False when it does not exist.
    function Authorize(const APolicy: StringRAL; AIdentity: TRALIdentity;
      ARequest: TRALRequest): boolean;
  published
    /// The policies, by name.
    property Policies: TRALAuthPolicies read FPolicies write SetPolicies;
  end;

/// Whether every scope of ANeeded (blanks or commas) is in AHave.
function RALHasAllScopes(const AHave, ANeeded: StringRAL): boolean;

implementation

function RALHasAllScopes(const AHave, ANeeded: StringRAL): boolean;
var
  vHave: TRALIdentity;
begin
  { an identity is a handy list of words: both lists are read the same way }
  vHave := TRALIdentity.Create;
  try
    vHave.AddRoles(RALRolesOfClaim(AHave));
    Result := vHave.HasRoles(RALRolesOfClaim(ANeeded), True);
  finally
    vHave.Free;
  end;
end;

{ TRALAuthPolicy }

constructor TRALAuthPolicy.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FClaims := TStringList.Create;
end;

destructor TRALAuthPolicy.Destroy;
begin
  FreeAndNil(FClaims);
  inherited;
end;

procedure TRALAuthPolicy.SetClaims(const AValue: TStrings);
begin
  FClaims.Assign(AValue);
end;

procedure TRALAuthPolicy.AssignTo(Dest: TPersistent);
var
  vDest: TRALAuthPolicy;
begin
  if not (Dest is TRALAuthPolicy) then
  begin
    inherited AssignTo(Dest);
    Exit;
  end;
  vDest := TRALAuthPolicy(Dest);
  vDest.Claims := FClaims;
  vDest.Name := FName;
  vDest.RequireAllRoles := FRequireAllRoles;
  vDest.Roles := FRoles;
  vDest.Scopes := FScopes;
  vDest.OnEvaluate := FOnEvaluate;
end;

function TRALAuthPolicy.GetDisplayName: string;
begin
  Result := string(FName);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TRALAuthPolicy.Evaluate(AIdentity: TRALIdentity; ARequest: TRALRequest): boolean;
var
  vInt: IntegerRAL;
  vName, vWanted, vHave, vScope: StringRAL;
begin
  Result := AIdentity.HasRoles(FRoles, FRequireAllRoles);

  vInt := 0;
  while Result and (vInt < FClaims.Count) do
  begin
    vName := StringRAL(Trim(FClaims.Names[vInt]));
    vWanted := StringRAL(Trim(FClaims.ValueFromIndex[vInt]));
    Inc(vInt);
    if vName = '' then
      Continue;
    vHave := AIdentity.Claim(vName);
    if vWanted = '*' then
      Result := vHave <> ''
    else
      Result := Pos('|' + vHave + '|', '|' + vWanted + '|') > 0;
  end;

  if Result and (RALTrim(FScopes) <> '') then
  begin
    vScope := AIdentity.Claim('scope');
    if vScope = '' then
      vScope := AIdentity.Claim('scp');
    Result := RALHasAllScopes(vScope, FScopes);
  end;

  if Assigned(FOnEvaluate) then
    FOnEvaluate(Self, AIdentity, ARequest, Result);
end;

{ TRALAuthPolicies }

constructor TRALAuthPolicies.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALAuthPolicy);
end;

function TRALAuthPolicies.GetPolicy(AIndex: IntegerRAL): TRALAuthPolicy;
begin
  Result := TRALAuthPolicy(Items[AIndex]);
end;

function TRALAuthPolicies.AddPolicy(const AName, ARoles: StringRAL): TRALAuthPolicy;
begin
  Result := TRALAuthPolicy(Add);
  Result.Name := AName;
  Result.Roles := ARoles;
end;

function TRALAuthPolicies.Find(const AName: StringRAL): TRALAuthPolicy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
  begin
    Result := Policies[vInt];
    if RALSameName(Result.Name, AName) then
      Exit;
  end;
  Result := nil;
end;

{ TRALAuthorizationPlugin }

constructor TRALAuthorizationPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPolicies := TRALAuthPolicies.Create(Self);
end;

destructor TRALAuthorizationPlugin.Destroy;
begin
  FreeAndNil(FPolicies);
  inherited;
end;

procedure TRALAuthorizationPlugin.SetPolicies(const AValue: TRALAuthPolicies);
begin
  FPolicies.Assign(AValue);
end;

function TRALAuthorizationPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppAuthResult];
end;

function TRALAuthorizationPlugin.Authorize(const APolicy: StringRAL;
  AIdentity: TRALIdentity; ARequest: TRALRequest): boolean;
var
  vPolicy: TRALAuthPolicy;
begin
  vPolicy := FPolicies.Find(APolicy);
  Result := (vPolicy <> nil) and vPolicy.Evaluate(AIdentity, ARequest);
end;

procedure TRALAuthorizationPlugin.AfterAuthenticate(ARequest: TRALRequest;
  AResponse: TRALResponse; var AResult: TRALAuthResult);
var
  vRoute: TRALBaseRoute;
  vPolicy: StringRAL;
begin
  if AResult <> arAccepted then
    Exit;
  vRoute := TRALBaseRoute(ARequest.Route);
  if vRoute = nil then
    Exit;
  vPolicy := vRoute.RequiredPolicy;
  if vPolicy = '' then
    Exit;
  { a policy nobody declared refuses: a typo must not open the route }
  if Authorize(vPolicy, ARequest.Identity, ARequest) then
    ARequest.Identity.PolicyPassed := True
  else
    AResult := arForbidden;
end;

end.
