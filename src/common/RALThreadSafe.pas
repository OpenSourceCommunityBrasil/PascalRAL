/// Class for Threading definitions and critical session controllers
unit RALThreadSafe;

{$I ..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes;

type

  /// Base of the objects guarded by their own critical section.
  TRALThreadSafe = class
  protected
    /// The lock.
    FCriticalSection: TCriticalSection;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    /// Takes the lock.
    procedure Lock;
    /// Releases the lock.
    procedure Unlock;
  end;

  /// A string list whose every operation takes its lock.
  TRALStringListSafe = class(TRALThreadSafe)
  private
    /// The list.
    FValue: TStringList;
  protected
    function GetValue(const AName: StringRAL): StringRAL;
    procedure SetValue(const AName: StringRAL; const AValue: StringRAL);
  public
    constructor Create; override;
    destructor Destroy; override;

    /// Adds AItem.
    procedure Add(const AItem: StringRAL);
    { Inserts AItem with AObject. Returns False when AItem was already there:
      the list is sorted with dupIgnore, so nothing is inserted and AObject
      stays with the caller, who must free it or it leaks. }
    function AddObject(const AItem: StringRAL; AObject: TObject): boolean;
    /// Removes every item, freeing the objects with AFreeObjects.
    procedure Clear(AFreeObjects: boolean = false);
    /// Number of items.
    function Count: IntegerRAL;
    /// Whether AItem is in the list.
    function Exists(const AItem: StringRAL): boolean;
    /// The item at AIndex.
    function Get(const AIndex: IntegerRAL): StringRAL;
    /// The name of the name=value item at AIndex.
    function GetName(const AIndex: IntegerRAL): StringRAL;
    /// The object of the item at AIndex.
    function GetObject(const AIndex: IntegerRAL): TObject;
    /// Whether the list is empty; read without the lock.
    function IsEmpty: boolean;
    /// Takes the lock and returns the list; call Unlock when done.
    function Lock: TStringList; reintroduce;
    /// The object of AItem, or nil.
    function ObjectByItem(const AItem: StringRAL): TObject;
    /// Removes AItem, freeing its object with AFreeObjects.
    procedure Remove(const AItem: StringRAL; AFreeObjects: boolean = false); overload;
    /// Removes the item at AItem, freeing its object with AFreeObjects.
    procedure Remove(const AItem: IntegerRAL; AFreeObjects: boolean = false); overload;
    /// Releases the lock Lock took.
    procedure Unlock; reintroduce;

    /// The value of the name=value item AName.
    property Values[const AName: StringRAL]: StringRAL read GetValue write SetValue;
  end;

  { The current version of something every request reads while the
    configuration may replace it - read with no lock at all. Publish puts a
    new version in place with one atomic write, and the version it replaces
    is kept, not freed, until this object goes: a request that took it a
    moment before is still reading it. A version is never changed after it is
    published, so a request sees one consistent whole, never half of each.
    What is kept grows by one per change, and a change is someone editing the
    configuration, not a request. }
  TRALSnapshots = class(TRALThreadSafe)
  private
    FCurrent: TObject;
    /// Snapshots replaced, kept alive until the owner is freed.
    FRetired: TList;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// Makes AValue the current version, owned by this object from here on
    procedure Publish(AValue: TObject);

    { The current version, nil before the first Publish. Never freed while
      this object lives, whatever is published after it. }
    property Current: TObject read FCurrent;
  end;

implementation

{ The pointer swapped with a full barrier, so a reader on another core that
  sees the new version also sees everything written into it before }
function ExchangePointer(var ATarget: Pointer; AValue: Pointer): Pointer;
begin
  {$IFDEF FPC}
  Result := InterlockedExchange(ATarget, AValue);
  {$ELSE}
  {$IFDEF DELPHIXE3UP}
  Result := AtomicExchange(ATarget, AValue);
  {$ELSE}
  Result := TInterlocked.Exchange(ATarget, AValue);
  {$ENDIF}
  {$ENDIF}
end;

{ TRALSnapshots }

constructor TRALSnapshots.Create;
begin
  inherited Create;
  FRetired := TList.Create;
end;

destructor TRALSnapshots.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Pred(FRetired.Count) do
    TObject(FRetired[vInt]).Free;
  FreeAndNil(FRetired);
  FreeAndNil(FCurrent);
  inherited Destroy;
end;

procedure TRALSnapshots.Publish(AValue: TObject);
var
  vOld: TObject;
begin
  { one writer at a time, for the list of retired versions; the readers never
    take this lock }
  Lock;
  try
    vOld := TObject(ExchangePointer(Pointer(FCurrent), Pointer(AValue)));
    if vOld <> nil then
      FRetired.Add(vOld);
  finally
    Unlock;
  end;
end;

{ TRALStringListSafe }

procedure TRALStringListSafe.SetValue(const AName: StringRAL; const AValue: StringRAL);
begin
  Lock;
  try
    FValue.Values[AName] := AValue;
  finally
    Unlock;
  end;
end;

function TRALStringListSafe.Get(const AIndex: IntegerRAL): StringRAL;
begin
  Lock;
  try
    Result := FValue.Strings[AIndex];
  finally
    Unlock;
  end;
end;

function TRALStringListSafe.GetName(const AIndex: IntegerRAL): StringRAL;
begin
  Lock;
  try
    Result := FValue.Names[AIndex];
  finally
    Unlock;
  end;
end;

function TRALStringListSafe.GetObject(const AIndex: IntegerRAL): TObject;
begin
  Lock;
  try
    Result := FValue.Objects[AIndex];
  finally
    Unlock;
  end;
end;

function TRALStringListSafe.GetValue(const AName: StringRAL): StringRAL;
begin
  Lock;
  try
    Result := FValue.Values[AName];
  finally
    Unlock;
  end;
end;

constructor TRALStringListSafe.Create;
begin
  inherited Create;
  FValue := TStringList.Create;
  FValue.Sorted := True;
end;

destructor TRALStringListSafe.Destroy;
begin
  inherited Lock;
  try
    Clear(True);
    FreeAndNil(FValue);
  finally
    inherited Unlock;
  end;
  inherited Destroy;
end;

procedure TRALStringListSafe.Add(const AItem: StringRAL);
begin
  Lock;
  try
    FValue.Add(AItem);
  finally
    Unlock;
  end;
end;

function TRALStringListSafe.AddObject(const AItem: StringRAL; AObject: TObject): boolean;
begin
  Lock;
  try
    { FValue is Sorted with Duplicates at its default, dupIgnore: AddObject on a
      key that is already there inserts nothing and reports nothing, so AObject
      is orphaned on the spot. Saying whether the insert happened is what lets a
      caller free what it built for nothing - and a caller that has to check and
      then insert should hold the lock across both through Lock/Unlock, not call
      this after a separate lookup. }
    Result := FValue.IndexOf(AItem) < 0;
    if Result then
      FValue.AddObject(AItem, AObject);
  finally
    Unlock;
  end;
end;

procedure TRALStringListSafe.Clear(AFreeObjects: boolean);
begin
  Lock;
  try
    if AFreeObjects then
    begin
      while FValue.Count > 0 do
      begin
        FValue.Objects[FValue.Count - 1].Free;
        FValue.Delete(FValue.Count - 1);
      end;
    end
    else
    begin
      FValue.Clear;
    end;
  finally
    Unlock;
  end;
end;

function TRALStringListSafe.Count: IntegerRAL;
begin
  Lock;
  try
    Result := FValue.Count;
  finally
    Unlock;
  end;
end;

function TRALStringListSafe.IsEmpty: boolean;
begin
  { outside the lock: Count is a plain field read and stale the instant it
    returns either way, so a caller takes it as a hint. The security lists,
    empty on most servers, are asked this on every request }
  Result := (FValue = nil) or (FValue.Count = 0);
end;

function TRALStringListSafe.Lock: TStringList;
begin
  inherited Lock;
  Result := FValue;
end;

function TRALStringListSafe.Exists(const AItem: StringRAL): boolean;
begin
  Result := false;
  Lock;
  try
    Result := FValue.IndexOf(AItem) >= 0;
  finally
    Unlock;
  end;
end;

function TRALStringListSafe.ObjectByItem(const AItem: StringRAL): TObject;
var
  i: Integer;
begin
  Result := nil;
  Lock;
  try
    i := FValue.IndexOf(AItem);
    if i > -1 then
      Result := FValue.Objects[i];
  finally
    Unlock;
  end;
end;

procedure TRALStringListSafe.Remove(const AItem: IntegerRAL; AFreeObjects: boolean);
begin
  Lock;
  try
    if (AItem > -1) and (AItem < FValue.Count) then
    begin
      if (AFreeObjects) then
        FValue.Objects[AItem].Free;
      FValue.Delete(AItem);
    end;
  finally
    Unlock;
  end;
end;

procedure TRALStringListSafe.Remove(const AItem: StringRAL; AFreeObjects: boolean);
var
  i: Integer;
begin
  Lock;
  try
    i := FValue.IndexOf(AItem);
    if i > -1 then
    begin
      if (AFreeObjects) then
        FValue.Objects[i].Free;
      FValue.Delete(i);
    end;
  finally
    Unlock;
  end;
end;

procedure TRALStringListSafe.Unlock;
begin
  inherited Unlock;
end;

{ TRALThreadSafe }

constructor TRALThreadSafe.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TRALThreadSafe.Destroy;
begin
  FreeAndNil(FCriticalSection);
  inherited Destroy;
end;

procedure TRALThreadSafe.Lock;
begin
  FCriticalSection.Enter;
end;

procedure TRALThreadSafe.Unlock;
begin
  FCriticalSection.Leave;
end;

end.
