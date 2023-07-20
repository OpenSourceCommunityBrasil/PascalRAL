unit RALThreadSafe;

{
  ver fonts unit : IdThreadSafe
}


interface

uses
  Classes, SysUtils, syncobjs;

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
    function GetValue(const AName: string): string;
    procedure SetValue(const AName: string; AValue: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Add(const AItem: string);
    procedure AddObject(const AItem: string; AObject: TObject);
    procedure Clear(AFreeObjects : boolean = false);
    function Empty: Boolean;
    function Lock: TStringList; reintroduce;
    function Exists(AItem : string) : boolean;
    function ObjectByItem(const AItem: string): TObject;
    procedure Remove(const AItem: string; AFreeObjects : boolean = false);
    procedure Unlock; reintroduce;
    property Values[const AName: string]: string read GetValue write SetValue;
  end;

implementation

{ TRALStringListSafe }

procedure TRALStringListSafe.SetValue(const AName: string; AValue: string);
begin
  Lock;
  try
    FValue.Values[AName] := AValue;
  finally
    Unlock;
  end;
end;

function TRALStringListSafe.GetValue(const AName: string): string;
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
end;

destructor TRALStringListSafe.Destroy;
begin
  inherited Lock;
  try
    FreeAndNil(FValue);
  finally
    inherited Unlock;
  end;
  inherited Destroy;
end;

procedure TRALStringListSafe.Add(const AItem: string);
begin
  Lock;
  try
    FValue.Add(AItem);
  finally
    Unlock;
  end;
end;

procedure TRALStringListSafe.AddObject(const AItem: string; AObject: TObject);
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
        FValue.Objects[FValue.Count-1].Free;
        FValue.Delete(FValue.Count-1);
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

function TRALStringListSafe.Exists(AItem: string): boolean;
begin
  Result := False;
  Lock;
  try
    Result := FValue.IndexOf(AItem) > 0;
  finally
    Unlock;
  end;
end;

function TRALStringListSafe.ObjectByItem(const AItem: string): TObject;
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

procedure TRALStringListSafe.Remove(const AItem: string; AFreeObjects: boolean);
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

