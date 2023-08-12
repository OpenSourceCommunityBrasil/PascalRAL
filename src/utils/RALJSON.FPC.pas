unit RALJSON.FPC;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, Variants, RALTypes;

type

  { TRALJSONValueBase }

  TRALJSONValueBase = class
  private
    FJsonObj : TJSONData;
    FFreeObj : boolean;
  protected
    procedure SetJsonObj(AObj : TJSONData; AFree : boolean = True);
    function GetJsonObj : TJSONData;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function JsonType : TRALJSONType;
    function IsNull : boolean;

    function AsValue: variant;
    function AsString : string;
    function AsInteger : int64;
    function AsFloat : double;
    function AsBoolean : boolean;

    function ToJson : string;
  end;

  { TRALJSONObjectBase }

  TRALJSONObjectBase = class(TRALJSONValueBase)
  protected
    function JsonObject : TJSONObject;
  public
    constructor Create; override;

    function Add(const AStr: string; const AValue: TRALJSONValueBase): TRALJSONObjectBase; overload;
    function Add(const AStr: string; const AValue: string): TRALJSONObjectBase; overload;
    function Add(const AStr: string; const AValue: int64): TRALJSONObjectBase; overload;
    function Add(const AStr: string; const AValue: integer): TRALJSONObjectBase; overload;
    function Add(const AStr: string; const AValue: double): TRALJSONObjectBase; overload;
    function Add(const AStr: string; const AValue: boolean): TRALJSONObjectBase; overload;
    function Add(const AStr: string): TRALJSONObjectBase; overload;

    function Count : integer;

    function Get(AName : string) : TRALJSONValueBase; overload;
    function Get(AIndex : integer) : TRALJSONValueBase; overload;
    function GetName(AIndex : integer) : string;
  end;

  { TRALJSONArrayBase }

  TRALJSONArrayBase = class(TRALJSONValueBase)
  protected
    function JsonObject : TJSONArray;
  public
    constructor Create; override;

    function Add(const AValue: TRALJSONValueBase): TRALJSONArrayBase; overload;
    function Add(const AValue: string): TRALJSONArrayBase; overload;
    function Add(const AValue: int64): TRALJSONArrayBase; overload;
    function Add(const AValue: integer): TRALJSONArrayBase; overload;
    function Add(const AValue: double): TRALJSONArrayBase; overload;
    function Add(const AValue: boolean): TRALJSONArrayBase; overload;
    function Add : TRALJSONArrayBase; overload;

    function Get(AIndex : integer) : TRALJSONValueBase;

    function Count : integer;
  end;

  { TRALJSONFPC }

  TRALJSONBase = class
  public
    class function ParseJSON(AJson : String) : TRALJSONValueBase;
  end;

implementation

{ TRALJSONBase }

class function TRALJSONBase.ParseJSON(AJson: String): TRALJSONValueBase;
var
  jObj : TJSONData;
begin
  Result := nil;

  jObj := GetJSON(AJson);

  if jObj <> nil then
  begin
    if jObj is TJSONObject then
      Result := TRALJSONObjectBase.Create
    else if jObj is TJSONArray then
      Result := TRALJSONArrayBase.Create
    else
      Result := TRALJSONValueBase.Create;
    Result.SetJsonObj(jObj);
  end;
end;

{ TRALJSONArrayBase }

function TRALJSONArrayBase.JsonObject: TJSONArray;
begin
  Result := TJSONArray(GetJsonObj);
end;

constructor TRALJSONArrayBase.Create;
var
  jObj : TJSONArray;
begin
  inherited;
  jObj := TJSONArray.Create;
  SetJsonObj(jObj);
end;

function TRALJSONArrayBase.Add(const AValue: TRALJSONValueBase): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue: string): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue: int64): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue: integer): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue: double): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue: boolean): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add: TRALJSONArrayBase;
begin
  JsonObject.Add(TJSONNull.Create);
  Result := Self;
end;

function TRALJSONArrayBase.Get(AIndex: integer): TRALJSONValueBase;
var
  vObj : TJSONData;
begin
  inherited;
  Result := nil;

  vObj := JsonObject.Items[AIndex];
  if vObj <> nil then
  begin
    if vObj is TJSONObject then
      Result := TRALJSONObjectBase.Create
    else if vObj is TJSONArray then
      Result := TRALJSONArrayBase.Create
    else
      Result := TRALJSONValueBase.Create;
    Result.SetJsonObj(vObj,False);
  end;
end;

function TRALJSONArrayBase.Count: integer;
begin
  Result := JsonObject.Count;
end;

{ TRALJSONObjectBase }

function TRALJSONObjectBase.JsonObject: TJSONObject;
begin
  Result := TJSONObject(GetJsonObj);
end;

constructor TRALJSONObjectBase.Create;
var
  jObj : TJSONObject;
begin
  inherited;
  jObj := TJSONObject.Create;
  SetJsonObj(jObj);
end;

function TRALJSONObjectBase.Add(const AStr: string; const AValue: TRALJSONValueBase): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string; const AValue: string): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string; const AValue: int64): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string; const AValue: integer): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string; const AValue: double): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string; const AValue: boolean): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,TJSONNull.Create);
  Result := Self;
end;

function TRALJSONObjectBase.Count: integer;
begin
  Result := JsonObject.Count;
end;

function TRALJSONObjectBase.Get(AName: string): TRALJSONValueBase;
var
  vObj : TJSONData;
begin
  Result := nil;
  vObj := JsonObject.Find(AName);
  if vObj <> nil then
  begin
    if vObj is TJSONobject then
      Result := TRALJSONObjectBase.Create
    else if vObj is TJSONArray then
      Result := TRALJSONArrayBase.Create
    else
      Result := TRALJSONValueBase.Create;
    Result.SetJsonObj(vObj,False);
  end;
end;

function TRALJSONObjectBase.Get(AIndex: integer): TRALJSONValueBase;
var
  vObj : TJSONData;
begin
  Result := nil;
  vObj := JsonObject.Items[AIndex];
  if vObj <> nil then
  begin
    if vObj is TJSONobject then
      Result := TRALJSONObjectBase.Create
    else if vObj is TJSONArray then
      Result := TRALJSONArrayBase.Create
    else
      Result := TRALJSONValueBase.Create;
    Result.SetJsonObj(vObj,False);
  end;
end;

function TRALJSONObjectBase.GetName(AIndex: integer): string;
begin
  Result := JsonObject.Names[AIndex];
end;

{ TRALJSONValueBase }

procedure TRALJSONValueBase.SetJsonObj(AObj: TJSONData; AFree: boolean);
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);
  FJsonObj := AObj;
  FFreeObj := AFree;
end;

function TRALJSONValueBase.GetJsonObj: TJSONData;
begin
  Result := FJsonObj;
end;

constructor TRALJSONValueBase.Create;
begin
  inherited;
  FJsonObj := nil;
  FFreeObj := True;
end;

destructor TRALJSONValueBase.Destroy;
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);
  inherited;
end;

function TRALJSONValueBase.JsonType : TRALJSONType;
begin
  Result := rjtString;
  if FJsonObj = nil then
    Exit;

  if FJsonObj is TJSONNumber then
    Result := rjtNumber
  else if FJsonObj is TJSONBoolean then
    Result := rjtBoolean
  else if FJsonObj is TJSONObject then
    Result := rjtObject
  else if FJsonObj is TJSONArray then
    Result := rjtArray;
end;

function TRALJSONValueBase.IsNull : boolean;
begin
  if FJsonObj <> nil then
    Result := FJsonObj is TJSONNull
  else
    Result := True;
end;

function TRALJSONValueBase.ToJson: string;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.AsJSON
  else
    Result := '';
end;

function TRALJSONValueBase.AsValue: variant;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.Value
  else
    Result := Variants.Null;
end;

function TRALJSONValueBase.AsString : string;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.Value
  else
    Result := 'null';
end;

function TRALJSONValueBase.AsInteger : int64;
begin
  Result := -1;
  if FJsonObj = nil then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := TJSONNumber(FJsonObj).AsInt64;
  end
  else if JsonType = rjtString then
  begin
    Result := StrToInt64Def(FJsonObj.Value,-1)
  end
  else if JsonType = rjtBoolean then
  begin
    if TJSONBoolean(FJsonObj).AsBoolean then
      Result := 1
    else
      Result := 0;
  end;
end;

function TRALJSONValueBase.AsFloat : double;
begin
  Result := -1;
  if FJsonObj = nil then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := TJSONNumber(FJsonObj).AsFloat;
  end
  else if JsonType = rjtString then
  begin
    Result := StrToFloatDef(FJsonObj.Value,-1)
  end;
end;

function TRALJSONValueBase.AsBoolean : boolean;
begin
  Result := False;
  if FJsonObj = nil then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := TJSONNumber(FJsonObj).AsFloat = 1;
  end
  else if JsonType = rjtString then
  begin
    Result := (FJsonObj.AsString = '1') or (FJsonObj.AsString = 'S') or
              (FJsonObj.AsString = 's') or (SameText(FJsonObj.AsString,'true'));
  end
  else if JsonType = rjtBoolean then
  begin
    Result := TJSONBoolean(FJsonObj).AsBoolean;
  end;
end;

end.

