{ Feature flags: a feature turned on or off without a new build - for everyone, for a
  share of the users that stays the same for each user, for a list of users or roles,
  or between two dates. The flags come from the Object Inspector and from the external
  configuration (TRALConfig: file, environment, command line), read again when the
  file changes; the routes listed in a flag answer 404 while it is off for the
  request, and the code asks IsEnabled. }
unit RALFeatureFlags;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALConsts, RALTools, RALPlugin, RALRequest, RALResponse, RALRoutes, RALConfig;

const
  /// Priority of the flags: after authentication, so the user is known.
  RALPriorityFeatureFlags = 480;

type
  TRALFeatureFlags = class;

  /// One flag.
  TRALFeatureFlag = class(TCollectionItem)
  private
    FDescription: string;
    FEnabled: Boolean;
    FEndDate: TDateTime;
    FName: string;
    FPaths: TStrings;
    FPercentage: Integer;
    FRoles: TStrings;
    FStartDate: TDateTime;
    FUsers: TStrings;

    /// A list changed: the flags are built again.
    procedure ListChanged(Sender: TObject);
    procedure SetEnabled(AValue: Boolean);
    procedure SetEndDate(AValue: TDateTime);
    procedure SetName(const AValue: string);
    procedure SetPaths(AValue: TStrings);
    procedure SetPercentage(AValue: Integer);
    procedure SetRoles(AValue: TStrings);
    procedure SetStartDate(AValue: TDateTime);
    procedure SetUsers(AValue: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
  published
    /// What the flag is for.
    property Description: string read FDescription write FDescription;
    /// The main switch: off, the flag is off for everyone.
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    /// Until when it is on (local time, the moment itself excluded); 0 = no limit.
    property EndDate: TDateTime read FEndDate write SetEndDate;
    /// The name the code and the configuration use.
    property Name: string read FName write SetName;
    { Routes that exist only while the flag is on for the request, one path per line
      ('*' takes one segment; a path covers what is below it); off, they answer 404. }
    property Paths: TStrings read FPaths write SetPaths;
    { Share of the users that have it, 0 to 100; each user always falls on the same side
      (a hash of the flag and the user), so raising it only adds users. }
    property Percentage: Integer read FPercentage write SetPercentage default 100;
    /// Roles that always have it, one per line.
    property Roles: TStrings read FRoles write SetRoles;
    /// From when it is on (local time); 0 = no limit.
    property StartDate: TDateTime read FStartDate write SetStartDate;
    /// Users that always have it, one per line.
    property Users: TStrings read FUsers write SetUsers;
  end;

  /// The flags of the Object Inspector.
  TRALFeatureFlagItems = class(TOwnedCollection)
  private
    function GetItem(AIndex: Integer): TRALFeatureFlag;
  protected
    procedure Update(AItem: TCollectionItem); override;
  public
    /// Creates the TRALFeatureFlagItems.
    constructor Create(AOwner: TPersistent);

    /// Adds a flag.
    function Add: TRALFeatureFlag;
    /// The flag named AName (case ignored), nil when none.
    function Find(const AName: string): TRALFeatureFlag;

    /// The flag at AIndex.
    property Items[AIndex: Integer]: TRALFeatureFlag read GetItem; default;
  end;

  /// A flag as evaluated: the Object Inspector's merged with the configuration.
  TRALFeatureRule = class
  public
    /// The main switch.
    Enabled: Boolean;
    /// Until when (0 = none).
    EndDate: TDateTime;
    /// The name, as written.
    Name: string;
    /// Paths in segments.
    Paths: TRALPathPatterns;
    /// 0 to 100.
    Percentage: Integer;
    /// Roles, sorted, case ignored.
    Roles: TStringList;
    /// From when (0 = none).
    StartDate: TDateTime;
    /// Users, sorted, case ignored.
    Users: TStringList;

    constructor Create;
    destructor Destroy; override;
  end;

  /// Decides a flag in code: AEnabled comes with the rules' answer and may be changed.
  TRALFeatureEvaluateEvent = procedure(Sender: TObject; const AFlag, AUser: string;
    ARequest: TRALRequest; var AEnabled: Boolean) of object;

  { The feature flags of a server (a plugin: link it to the server) or of any program
    (IsEnabled works without a server). With Config, the keys <ConfigPrefix>.<flag>.
    Enabled, Percentage, Users, Roles, StartDate, EndDate and Paths (lists separated
    by commas, dates as yyyy-mm-dd or yyyy-mm-ddThh:nn:ss) add flags or change those
    of Flags, and a change in the configuration file applies at the next question,
    at most every ReloadInterval milliseconds, without a restart. }
  TRALFeatureFlags = class(TRALPlugin)
  private
    FConfig: TRALConfig;
    FConfigPrefix: string;
    /// Whether the rules must be built again.
    FDirty: Boolean;
    FFlags: TRALFeatureFlagItems;
    /// When the configuration file was last looked at (ms tick).
    FLastCheck: Int64;
    /// Guards the rules.
    FLock: TCriticalSection;
    FOnChange: TNotifyEvent;
    FOnEvaluate: TRALFeatureEvaluateEvent;
    FReloadInterval: Integer;
    /// The rules, by name, sorted; the objects are TRALFeatureRule.
    FRules: TStringList;

    /// Builds the rules from Flags and Config; under the lock.
    procedure Build;
    /// Rebuilds when dirty or when the configuration file changed; under the lock.
    procedure Check;
    /// Frees the rules; under the lock.
    procedure ClearRules;
    /// Whether ARule is on for AUser with ARoles at ANow; under the lock.
    function Decide(ARule: TRALFeatureRule; const AUser: string; ARoles: TStrings;
      ANow: TDateTime): Boolean;
    /// Reads the configuration's keys of the flag AName into ARule.
    procedure ReadConfig(const AName: string; ARule: TRALFeatureRule);
    procedure SetConfig(AValue: TRALConfig);
    procedure SetFlags(AValue: TRALFeatureFlagItems);
    /// The user a request is judged as: its identity's name, else the client address.
    function UserOf(ARequest: TRALRequest): string;
  protected
    class function DefaultPriority: IntegerRAL; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// The names of the flags known now, from Flags and Config.
    procedure GetNames(AList: TStrings);
    /// Marks the rules to be built again at the next question (Flags changed in code).
    procedure Invalidate;
    /// Whether the flag AName is on for AUser (and the roles ARoles); unknown = off.
    function IsEnabled(const AName: string; const AUser: string = '';
      ARoles: TStrings = nil): Boolean; overload;
    /// Whether the flag AName is on for the user of ARequest.
    function IsEnabled(const AName: string; ARequest: TRALRequest): Boolean; overload;
    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: Boolean); override;
    /// Reads the configuration again now; True when the file had changed.
    function Reload: Boolean;
  published
    /// The external configuration the flags also come from.
    property Config: TRALConfig read FConfig write SetConfig;
    /// The first part of the keys in Config.
    property ConfigPrefix: string read FConfigPrefix write FConfigPrefix;
    /// The flags of the Object Inspector.
    property Flags: TRALFeatureFlagItems read FFlags write SetFlags;
    /// The rules were built again (Flags or the configuration changed).
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    /// Decides a flag in code, after the rules.
    property OnEvaluate: TRALFeatureEvaluateEvent read FOnEvaluate write FOnEvaluate;
    /// Milliseconds between two looks at the configuration file; 0 = only on Reload.
    property ReloadInterval: Integer read FReloadInterval write FReloadInterval
      default 2000;
  end;

{ The share bucket of AUser in AFlag, 0 to 99: the same for the same pair, spread
  evenly (FNV-1a of the lower-case text). }
function RALFeatureBucket(const AFlag, AUser: string): Integer;

implementation

const
  /// The default of ConfigPrefix.
  cDefaultPrefix = 'Flags';

function RALFeatureBucket(const AFlag, AUser: string): Integer;
var
  vText: UTF8String;
  vHash: Cardinal;
  vIdx: Integer;
begin
  vText := UTF8String(LowerCase(AFlag) + #0 + LowerCase(AUser));
  vHash := 2166136261;
  for vIdx := 1 to Length(vText) do
  begin
    vHash := vHash xor Ord(vText[vIdx]);
    {$Q-}{$R-}
    vHash := vHash * 16777619;
    {$IFDEF RAL_DEBUG}{$Q+}{$R+}{$ENDIF}
  end;
  Result := Integer(vHash mod 100);
end;

/// AText as a date (yyyy-mm-dd, yyyy-mm-ddThh:nn[:ss] or local); 0 when empty or bad.
function ParseFlagDate(const AText: string): TDateTime;
var
  vText: string;
  vY, vM, vD, vH, vN, vS: Integer;
begin
  Result := 0;
  vText := Trim(AText);
  if vText = '' then
    Exit;
  if (Length(vText) >= 10) and (vText[5] = '-') and (vText[8] = '-') then
  begin
    vY := StrToIntDef(Copy(vText, 1, 4), 0);
    vM := StrToIntDef(Copy(vText, 6, 2), 0);
    vD := StrToIntDef(Copy(vText, 9, 2), 0);
    vH := 0;
    vN := 0;
    vS := 0;
    if (Length(vText) >= 16) and ((vText[11] = 'T') or (vText[11] = ' ')) then
    begin
      vH := StrToIntDef(Copy(vText, 12, 2), 0);
      vN := StrToIntDef(Copy(vText, 15, 2), 0);
      if Length(vText) >= 19 then
        vS := StrToIntDef(Copy(vText, 18, 2), 0);
    end;
    if not TryEncodeDateTime(vY, vM, vD, vH, vN, vS, 0, Result) then
      Result := 0;
  end
  else if not TryStrToDateTime(vText, Result) then
    Result := 0;
end;

/// Fills AList (sorted, case ignored) with the lines of ASource and the items of AComma.
procedure FillList(AList: TStringList; ASource: TStrings; const AComma: string);
var
  vIdx: Integer;
  vParts: TStringList;
begin
  AList.Clear;
  if ASource <> nil then
    for vIdx := 0 to ASource.Count - 1 do
      if Trim(ASource[vIdx]) <> '' then
        AList.Add(Trim(ASource[vIdx]));
  if AComma <> '' then
  begin
    vParts := TStringList.Create;
    try
      vParts.Delimiter := ',';
      vParts.StrictDelimiter := True;
      vParts.DelimitedText := AComma;
      for vIdx := 0 to vParts.Count - 1 do
        if Trim(vParts[vIdx]) <> '' then
          AList.Add(Trim(vParts[vIdx]));
    finally
      vParts.Free;
    end;
  end;
end;

/// The paths of AList (lines) and of AComma (a comma list) in segments.
function PathsOf(AList: TStrings; const AComma: string): TRALPathPatterns;
var
  vAll: TStringList;
  vIdx: Integer;
begin
  vAll := TStringList.Create;
  try
    FillList(vAll, AList, AComma);
    SetLength(Result, vAll.Count);
    for vIdx := 0 to vAll.Count - 1 do
      Result[vIdx] := RALPathSegments(StringRAL(vAll[vIdx]));
  finally
    vAll.Free;
  end;
end;

/// A sorted, case-ignoring list.
function NewSortedList: TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
end;

{ TRALFeatureFlag }

procedure TRALFeatureFlag.Assign(ASource: TPersistent);
var
  vSource: TRALFeatureFlag;
begin
  if ASource is TRALFeatureFlag then
  begin
    vSource := TRALFeatureFlag(ASource);
    FDescription := vSource.FDescription;
    FEnabled := vSource.FEnabled;
    FEndDate := vSource.FEndDate;
    FName := vSource.FName;
    FPaths.Assign(vSource.FPaths);
    FPercentage := vSource.FPercentage;
    FRoles.Assign(vSource.FRoles);
    FStartDate := vSource.FStartDate;
    FUsers.Assign(vSource.FUsers);
    Changed(False);
  end
  else
    inherited Assign(ASource);
end;

constructor TRALFeatureFlag.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FEnabled := True;
  FPercentage := 100;
  FPaths := TStringList.Create;
  FRoles := TStringList.Create;
  FUsers := TStringList.Create;
  TStringList(FPaths).OnChange := ListChanged;
  TStringList(FRoles).OnChange := ListChanged;
  TStringList(FUsers).OnChange := ListChanged;
end;

destructor TRALFeatureFlag.Destroy;
begin
  FreeAndNil(FUsers);
  FreeAndNil(FRoles);
  FreeAndNil(FPaths);
  inherited Destroy;
end;

function TRALFeatureFlag.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := inherited GetDisplayName;
end;

procedure TRALFeatureFlag.ListChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TRALFeatureFlag.SetEnabled(AValue: Boolean);
begin
  FEnabled := AValue;
  Changed(False);
end;

procedure TRALFeatureFlag.SetEndDate(AValue: TDateTime);
begin
  FEndDate := AValue;
  Changed(False);
end;

procedure TRALFeatureFlag.SetName(const AValue: string);
begin
  FName := AValue;
  Changed(False);
end;

procedure TRALFeatureFlag.SetPaths(AValue: TStrings);
begin
  FPaths.Assign(AValue);
end;

procedure TRALFeatureFlag.SetPercentage(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0
  else if AValue > 100 then
    AValue := 100;
  FPercentage := AValue;
  Changed(False);
end;

procedure TRALFeatureFlag.SetRoles(AValue: TStrings);
begin
  FRoles.Assign(AValue);
end;

procedure TRALFeatureFlag.SetStartDate(AValue: TDateTime);
begin
  FStartDate := AValue;
  Changed(False);
end;

procedure TRALFeatureFlag.SetUsers(AValue: TStrings);
begin
  FUsers.Assign(AValue);
end;

{ TRALFeatureFlagItems }

function TRALFeatureFlagItems.Add: TRALFeatureFlag;
begin
  Result := TRALFeatureFlag(inherited Add);
end;

constructor TRALFeatureFlagItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALFeatureFlag);
end;

function TRALFeatureFlagItems.Find(const AName: string): TRALFeatureFlag;
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

function TRALFeatureFlagItems.GetItem(AIndex: Integer): TRALFeatureFlag;
begin
  Result := TRALFeatureFlag(inherited Items[AIndex]);
end;

procedure TRALFeatureFlagItems.Update(AItem: TCollectionItem);
begin
  inherited Update(AItem);
  if Owner is TRALFeatureFlags then
    TRALFeatureFlags(Owner).Invalidate;
end;

{ TRALFeatureRule }

constructor TRALFeatureRule.Create;
begin
  inherited Create;
  Roles := NewSortedList;
  Users := NewSortedList;
end;

destructor TRALFeatureRule.Destroy;
begin
  Users.Free;
  Roles.Free;
  inherited Destroy;
end;

{ TRALFeatureFlags }

procedure TRALFeatureFlags.Build;
var
  vIdx: Integer;
  vItem: TRALFeatureFlag;
  vRule: TRALFeatureRule;
  vNames: TStringList;
begin
  ClearRules;
  for vIdx := 0 to FFlags.Count - 1 do
  begin
    vItem := FFlags[vIdx];
    if Trim(vItem.Name) = '' then
      Continue;
    vRule := TRALFeatureRule.Create;
    vRule.Name := Trim(vItem.Name);
    vRule.Enabled := vItem.Enabled;
    vRule.Percentage := vItem.Percentage;
    vRule.StartDate := vItem.StartDate;
    vRule.EndDate := vItem.EndDate;
    FillList(vRule.Users, vItem.Users, '');
    FillList(vRule.Roles, vItem.Roles, '');
    vRule.Paths := PathsOf(vItem.Paths, '');
    FRules.AddObject(vRule.Name, vRule);
  end;
  if FConfig <> nil then
  begin
    vNames := TStringList.Create;
    try
      FConfig.GetKeys(vNames);
      for vIdx := 0 to vNames.Count - 1 do
        ReadConfig(vNames[vIdx], nil);
    finally
      vNames.Free;
    end;
  end;
  FDirty := False;
end;

procedure TRALFeatureFlags.Check;
var
  vNow: Int64;
  vChanged: Boolean;
begin
  vChanged := FDirty;
  if (FConfig <> nil) and (FReloadInterval > 0) then
  begin
    vNow := RALTickMicro div 1000;
    if vNow - FLastCheck >= FReloadInterval then
    begin
      FLastCheck := vNow;
      if FConfig.ReloadIfChanged then
        vChanged := True;
    end;
  end;
  if vChanged then
  begin
    Build;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TRALFeatureFlags.ClearRules;
var
  vIdx: Integer;
begin
  for vIdx := 0 to FRules.Count - 1 do
    FRules.Objects[vIdx].Free;
  FRules.Clear;
end;

constructor TRALFeatureFlags.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfigPrefix := cDefaultPrefix;
  FFlags := TRALFeatureFlagItems.Create(Self);
  FLock := TCriticalSection.Create;
  FReloadInterval := 2000;
  FRules := TStringList.Create;
  FRules.CaseSensitive := False;
  FRules.Sorted := True;
  FDirty := True;
end;

function TRALFeatureFlags.Decide(ARule: TRALFeatureRule; const AUser: string;
  ARoles: TStrings; ANow: TDateTime): Boolean;
var
  vIdx: Integer;
begin
  Result := False;
  if not ARule.Enabled then
    Exit;
  if (ARule.StartDate <> 0) and (ANow < ARule.StartDate) then
    Exit;
  if (ARule.EndDate <> 0) and (ANow >= ARule.EndDate) then
    Exit;
  if (AUser <> '') and (ARule.Users.IndexOf(AUser) >= 0) then
  begin
    Result := True;
    Exit;
  end;
  if ARoles <> nil then
    for vIdx := 0 to ARoles.Count - 1 do
      if ARule.Roles.IndexOf(Trim(ARoles[vIdx])) >= 0 then
      begin
        Result := True;
        Exit;
      end;
  if ARule.Percentage >= 100 then
    Result := True
  else if ARule.Percentage > 0 then
    Result := RALFeatureBucket(ARule.Name, AUser) < ARule.Percentage;
end;

class function TRALFeatureFlags.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityFeatureFlags;
end;

destructor TRALFeatureFlags.Destroy;
begin
  SetConfig(nil);
  if FRules <> nil then
    ClearRules;
  FreeAndNil(FRules);
  FreeAndNil(FLock);
  FreeAndNil(FFlags);
  inherited Destroy;
end;

procedure TRALFeatureFlags.GetNames(AList: TStrings);
var
  vIdx: Integer;
begin
  AList.Clear;
  FLock.Enter;
  try
    Check;
    for vIdx := 0 to FRules.Count - 1 do
      AList.Add(TRALFeatureRule(FRules.Objects[vIdx]).Name);
  finally
    FLock.Leave;
  end;
end;

procedure TRALFeatureFlags.Invalidate;
begin
  // FLock is nil while the collection is made in the constructor
  if FLock = nil then
    Exit;
  FLock.Enter;
  try
    FDirty := True;
  finally
    FLock.Leave;
  end;
end;

function TRALFeatureFlags.IsEnabled(const AName, AUser: string; ARoles: TStrings): Boolean;
var
  vIdx: Integer;
begin
  FLock.Enter;
  try
    Check;
    vIdx := FRules.IndexOf(AName);
    Result := (vIdx >= 0) and
      Decide(TRALFeatureRule(FRules.Objects[vIdx]), AUser, ARoles, Now);
  finally
    FLock.Leave;
  end;
  if Assigned(FOnEvaluate) then
    FOnEvaluate(Self, AName, AUser, nil, Result);
end;

function TRALFeatureFlags.IsEnabled(const AName: string; ARequest: TRALRequest): Boolean;
var
  vIdx: Integer;
  vUser: string;
  vRoles: TStrings;
begin
  vUser := UserOf(ARequest);
  vRoles := nil;
  if ARequest.HasIdentity then
    vRoles := ARequest.Identity.Roles;
  FLock.Enter;
  try
    Check;
    vIdx := FRules.IndexOf(AName);
    Result := (vIdx >= 0) and
      Decide(TRALFeatureRule(FRules.Objects[vIdx]), vUser, vRoles, Now);
  finally
    FLock.Leave;
  end;
  if Assigned(FOnEvaluate) then
    FOnEvaluate(Self, AName, vUser, ARequest, Result);
end;

procedure TRALFeatureFlags.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConfig) then
  begin
    FConfig := nil;
    Invalidate;
  end;
end;

function TRALFeatureFlags.Phases: TRALPluginPhases;
begin
  Result := [ppProcess];
end;

procedure TRALFeatureFlags.ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
  var AHandled: Boolean);
var
  vRaw, vPath: TRALRouteSegments;
  vIdx, vJdx: Integer;
  vRule: TRALFeatureRule;
  vGuarded: TStringList;
begin
  vGuarded := nil;
  FLock.Enter;
  try
    Check;
    RALSplitRequestPath(ARequest, vRaw, vPath);
    for vIdx := 0 to FRules.Count - 1 do
    begin
      vRule := TRALFeatureRule(FRules.Objects[vIdx]);
      for vJdx := 0 to High(vRule.Paths) do
        if RALPathUnder(vPath, vRule.Paths[vJdx]) then
        begin
          if vGuarded = nil then
            vGuarded := TStringList.Create;
          vGuarded.Add(vRule.Name);
          Break;
        end;
    end;
  finally
    FLock.Leave;
  end;
  if vGuarded = nil then
    Exit;
  try
    // every flag that guards the path must be on for the request
    for vIdx := 0 to vGuarded.Count - 1 do
      if not IsEnabled(vGuarded[vIdx], ARequest) then
      begin
        AResponse.AnswerError(HTTP_NotFound, '');
        AHandled := True;
        Exit;
      end;
  finally
    vGuarded.Free;
  end;
end;

procedure TRALFeatureFlags.ReadConfig(const AName: string; ARule: TRALFeatureRule);
var
  vPrefix, vFlag, vKey, vField: string;
  vIdx, vDot: Integer;
  vRule: TRALFeatureRule;
begin
  // AName is a key of the configuration: <prefix>.<flag>.<field>
  vPrefix := FConfigPrefix + '.';
  if not SameText(Copy(AName, 1, Length(vPrefix)), vPrefix) then
    Exit;
  vKey := Copy(AName, Length(vPrefix) + 1, MaxInt);
  vDot := LastDelimiter('.', vKey);
  if vDot <= 1 then
    Exit;
  vFlag := Copy(vKey, 1, vDot - 1);
  vField := LowerCase(Copy(vKey, vDot + 1, MaxInt));
  vRule := ARule;
  if vRule = nil then
  begin
    vIdx := FRules.IndexOf(vFlag);
    if vIdx >= 0 then
      vRule := TRALFeatureRule(FRules.Objects[vIdx])
    else
    begin
      // a flag only in the configuration starts on for everyone, like a new item
      vRule := TRALFeatureRule.Create;
      vRule.Name := vFlag;
      vRule.Enabled := True;
      vRule.Percentage := 100;
      FRules.AddObject(vFlag, vRule);
    end;
  end;
  if vField = 'enabled' then
    vRule.Enabled := FConfig.GetBool(AName, vRule.Enabled)
  else if vField = 'percentage' then
  begin
    vRule.Percentage := FConfig.GetInt(AName, vRule.Percentage);
    if vRule.Percentage < 0 then
      vRule.Percentage := 0
    else if vRule.Percentage > 100 then
      vRule.Percentage := 100;
  end
  else if vField = 'users' then
    FillList(vRule.Users, nil, FConfig.Get(AName))
  else if vField = 'roles' then
    FillList(vRule.Roles, nil, FConfig.Get(AName))
  else if vField = 'startdate' then
    vRule.StartDate := ParseFlagDate(FConfig.Get(AName))
  else if vField = 'enddate' then
    vRule.EndDate := ParseFlagDate(FConfig.Get(AName))
  else if vField = 'paths' then
    vRule.Paths := PathsOf(nil, FConfig.Get(AName));
end;

function TRALFeatureFlags.Reload: Boolean;
begin
  FLock.Enter;
  try
    Result := (FConfig <> nil) and FConfig.Reload;
    FLastCheck := RALTickMicro div 1000;
    Build;
  finally
    FLock.Leave;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TRALFeatureFlags.SetConfig(AValue: TRALConfig);
begin
  if FConfig = AValue then
    Exit;
  if FConfig <> nil then
    FConfig.RemoveFreeNotification(Self);
  FConfig := AValue;
  if FConfig <> nil then
    FConfig.FreeNotification(Self);
  Invalidate;
end;

procedure TRALFeatureFlags.SetFlags(AValue: TRALFeatureFlagItems);
begin
  FFlags.Assign(AValue);
end;

function TRALFeatureFlags.UserOf(ARequest: TRALRequest): string;
begin
  if ARequest.HasIdentity and (ARequest.Identity.Name <> '') then
    Result := string(ARequest.Identity.Name)
  else
    Result := string(ARequest.ClientInfo.IP);
end;

end.
