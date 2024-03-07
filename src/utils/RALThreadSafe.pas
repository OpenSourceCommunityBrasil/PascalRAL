/// Class for Threading definitions and critical session controllers
unit RALThreadSafe;

interface

uses
  Classes, SysUtils, syncobjs, RALTypes;

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
    procedure AddObject(const AItem: StringRAL; AObject: TObject);
    procedure Clear(AFreeObjects : boolean = false);
    function Empty: Boolean;
    function Exists(const AItem : StringRAL) : boolean;
    function Lock: TStringList; reintroduce;
    function ObjectByItem(const AItem: StringRAL): TObject;
    procedure Remove(const AItem: StringRAL; AFreeObjects : boolean = false);
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

procedure TRALStringListSafe.AddObject(const AItem: StringRAL; AObject: TObject);
begin
  Lock;
  try
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
    else begin
      FValue.Clear;
    end;
  finally
    Unlock;
  end;
end;

function TRALStringListSafe.Empty: Boolean;
begin
  Lock;
  try
    Result := FValue.Count = 0;
  finally
    Unlock;
  end;
end;

function TRALStringListSafe.Lock: TStringList;
begin
  inherited Lock;
  Result := FValue;
end;

function TRALStringListSafe.Exists(const AItem: StringRAL): boolean;
begin
  Result := False;
  Lock;
  try
    Result := FValue.IndexOf(AItem) > 0;
  finally
    Unlock;
  end;
end;

function TRALStringListSafe.ObjectByItem(const AItem: StringRAL): TObject;
var
  i : Integer;
begin
  Result := nil;
  Lock;
  try
    i := FValue.IndexOf(AItem);
    if i > -1 then begin
      Result := FValue.Objects[i];
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
    if i > -1 then begin
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

