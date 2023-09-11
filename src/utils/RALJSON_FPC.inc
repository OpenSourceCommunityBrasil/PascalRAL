{
  RALJSON_FPC
  Unit criada para ser utilizar RAL com Lazarus
}

type

  { TRALJSONValue }

  TRALJSONValue = class
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

  { TRALJSONObject }

  TRALJSONObject = class(TRALJSONValue)
  protected
    function JsonObject : TJSONObject;
  public
    constructor Create; override;

    function Add(const AStr: string; const AValue: TRALJSONValue): TRALJSONObject; overload;
    function Add(const AStr: string; const AValue: string): TRALJSONObject; overload;
    function Add(const AStr: string; const AValue: int64): TRALJSONObject; overload;
    function Add(const AStr: string; const AValue: integer): TRALJSONObject; overload;
    function Add(const AStr: string; const AValue: double): TRALJSONObject; overload;
    function Add(const AStr: string; const AValue: boolean): TRALJSONObject; overload;
    function Add(const AStr: string): TRALJSONObject; overload;

    function Count : integer;

    function Get(AName : string) : TRALJSONValue; overload;
    function Get(AIndex : integer) : TRALJSONValue; overload;
    function GetName(AIndex : integer) : string;
  end;

  { TRALJSONArray }

  TRALJSONArray = class(TRALJSONValue)
  protected
    function JsonObject : TJSONArray;
  public
    constructor Create; override;

    function Add(const AValue: TRALJSONValue): TRALJSONArray; overload;
    function Add(const AValue: string): TRALJSONArray; overload;
    function Add(const AValue: int64): TRALJSONArray; overload;
    function Add(const AValue: integer): TRALJSONArray; overload;
    function Add(const AValue: double): TRALJSONArray; overload;
    function Add(const AValue: boolean): TRALJSONArray; overload;
    function Add : TRALJSONArray; overload;

    function Get(AIndex : integer) : TRALJSONValue;

    function Count : integer;
  end;

  { TRALJSONFPC }

  TRALJSON = class
  public
    class function ParseJSON(AJson : String) : TRALJSONValue;
    class function ParseJSON(AStream : TStream) : TRALJSONValue;
  end;

implementation

{ TRALJSON }

class function TRALJSON.ParseJSON(AJson: String): TRALJSONValue;
var
  jObj : TJSONData;
begin
  Result := nil;

  jObj := GetJSON(AJson);

  if jObj <> nil then
  begin
    if jObj is TJSONObject then
      Result := TRALJSONObject.Create
    else if jObj is TJSONArray then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(jObj);
  end;
end;

class function TRALJSON.ParseJSON(AStream : TStream) : TRALJSONValue;
var
  jObj : TJSONData;
begin
  Result := nil;

  jObj := GetJSON(AStream);

  if jObj <> nil then
  begin
    if jObj is TJSONObject then
      Result := TRALJSONObject.Create
    else if jObj is TJSONArray then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(jObj);
  end;
end;

{ TRALJSONArray }

function TRALJSONArray.JsonObject: TJSONArray;
begin
  Result := TJSONArray(GetJsonObj);
end;

constructor TRALJSONArray.Create;
var
  jObj : TJSONArray;
begin
  inherited;
  jObj := TJSONArray.Create;
  SetJsonObj(jObj);
end;

function TRALJSONArray.Add(const AValue: TRALJSONValue): TRALJSONArray;
begin
  JsonObject.Add(AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: string): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: int64): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: integer): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: double): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: boolean): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add: TRALJSONArray;
begin
  JsonObject.Add(TJSONNull.Create);
  Result := Self;
end;

function TRALJSONArray.Get(AIndex: integer): TRALJSONValue;
var
  vObj : TJSONData;
begin
  inherited;
  Result := nil;

  vObj := JsonObject.Items[AIndex];
  if vObj <> nil then
  begin
    if vObj is TJSONObject then
      Result := TRALJSONObject.Create
    else if vObj is TJSONArray then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(vObj,False);
  end;
end;

function TRALJSONArray.Count: integer;
begin
  Result := JsonObject.Count;
end;

{ TRALJSONObject }

function TRALJSONObject.JsonObject: TJSONObject;
begin
  Result := TJSONObject(GetJsonObj);
end;

constructor TRALJSONObject.Create;
var
  jObj : TJSONObject;
begin
  inherited;
  jObj := TJSONObject.Create;
  SetJsonObj(jObj);
end;

function TRALJSONObject.Add(const AStr: string; const AValue: TRALJSONValue): TRALJSONObject;
begin
  JsonObject.Add(AStr,AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string; const AValue: string): TRALJSONObject;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string; const AValue: int64): TRALJSONObject;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string; const AValue: integer): TRALJSONObject;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string; const AValue: double): TRALJSONObject;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string; const AValue: boolean): TRALJSONObject;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string): TRALJSONObject;
begin
  JsonObject.Add(AStr,TJSONNull.Create);
  Result := Self;
end;

function TRALJSONObject.Count: integer;
begin
  Result := JsonObject.Count;
end;

function TRALJSONObject.Get(AName: string): TRALJSONValue;
var
  vObj : TJSONData;
begin
  Result := nil;
  vObj := JsonObject.Find(AName);
  if vObj <> nil then
  begin
    if vObj is TJSONobject then
      Result := TRALJSONObject.Create
    else if vObj is TJSONArray then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(vObj,False);
  end;
end;

function TRALJSONObject.Get(AIndex: integer): TRALJSONValue;
var
  vObj : TJSONData;
begin
  Result := nil;
  vObj := JsonObject.Items[AIndex];
  if vObj <> nil then
  begin
    if vObj is TJSONobject then
      Result := TRALJSONObject.Create
    else if vObj is TJSONArray then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(vObj,False);
  end;
end;

function TRALJSONObject.GetName(AIndex: integer): string;
begin
  Result := JsonObject.Names[AIndex];
end;

{ TRALJSONValue }

procedure TRALJSONValue.SetJsonObj(AObj: TJSONData; AFree: boolean);
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);
  FJsonObj := AObj;
  FFreeObj := AFree;
end;

function TRALJSONValue.GetJsonObj: TJSONData;
begin
  Result := FJsonObj;
end;

constructor TRALJSONValue.Create;
begin
  inherited;
  FJsonObj := nil;
  FFreeObj := True;
end;

destructor TRALJSONValue.Destroy;
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);
  inherited;
end;

function TRALJSONValue.JsonType : TRALJSONType;
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

function TRALJSONValue.IsNull : boolean;
begin
  if FJsonObj <> nil then
    Result := FJsonObj is TJSONNull
  else
    Result := True;
end;

function TRALJSONValue.ToJson: string;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.AsJSON
  else
    Result := '';
end;

function TRALJSONValue.AsValue: variant;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.Value
  else
    Result := Variants.Null;
end;

function TRALJSONValue.AsString : string;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.Value
  else
    Result := 'null';
end;

function TRALJSONValue.AsInteger : int64;
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

function TRALJSONValue.AsFloat : double;
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

function TRALJSONValue.AsBoolean : boolean;
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
