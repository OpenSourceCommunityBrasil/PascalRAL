/// Class for Threading definitions and critical session controllers
unit RALThreadSafe;

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes;

type

  { TRALThreadSafe }

  TRALThreadSafe = class
  protected
    FCriticalSection: TCriticalSection;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;
  end;

  { TRALStringListSafe }

  TRALStringListSafe = class(TRALThreadSafe)
  private
    FValue: TStringList;
  protected
    function GetValue(const AName: StringRAL): StringRAL;
    procedure SetValue(const AName: StringRAL; const AValue: StringRAL);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Add(const AItem: StringRAL);
    /// Inserts AItem with AObject. Returns False when AItem was already there:
    /// the list is sorted with dupIgnore, so nothing is inserted and AObject
    /// stays with the caller, who must free it or it leaks
    function AddObject(const AItem: StringRAL; AObject: TObject): boolean;
    procedure Clear(AFreeObjects: boolean = false);
    function Count: IntegerRAL;
    function Exists(const AItem: StringRAL): boolean;
    function Get(const AIndex: IntegerRAL): StringRAL;
    function GetName(const AIndex: IntegerRAL): StringRAL;
    function GetObject(const AIndex: IntegerRAL): TObject;
    function IsEmpty: boolean;
    function Lock: TStringList; reintroduce;
    function ObjectByItem(const AItem: StringRAL): TObject;
    procedure Remove(const AItem: StringRAL; AFreeObjects: boolean = false); overload;
    procedure Remove(const AItem: IntegerRAL; AFreeObjects: boolean = false); overload;
    procedure Unlock; reintroduce;

    property Values[const AName: StringRAL]: StringRAL read GetValue write SetValue;
  end;

implementation

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
  { deliberately outside the lock. TStringList.Count is a plain field read, and
    the answer is already stale the instant it returns whether we take the lock
    or not - a caller can only use it as a hint, never as a decision it then
    acts on without re-checking. What skipping the lock buys is the hot path:
    the security lists are empty on almost every server, and ValidateRequest
    consults two of them on every single request. Taking a critical section to
    read one integer is free while requests are rare and turns into a convoy
    once hundreds of threads do it thousands of times a second. }
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
