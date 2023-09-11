{
  RALJSON_uJSON
  SouceForce: https://sourceforge.net/projects/is-webstart/
  Version: 1.06 de 25/03/2010

  Unit criada para ser utilizar RAL com Delphi antigos, especificadamente
  Delphi 7 a Delphi 2009
}

type

  { TRALJSONValue }

  TRALJSONValue = class
  private
    FJsonObj : TZAbstractObject;
    FFreeObj : boolean;
  protected
    procedure SetJsonObj(AObj : TZAbstractObject; AFree : boolean = True);
    function GetJsonObj : TZAbstractObject;
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

  { TRALJSON }

  TRALJSON = class
  public
    class function ParseJSON(AJson : String) : TRALJSONValue;
    class function ParseJSON(AStream : TStream) : TRALJSONValue;
  end;

implementation

{ TRALJSON }

class function TRALJSON.ParseJSON(AJson : String) : TRALJSONValue;
var
  vTok : JSONTokener;
  vObj : TZAbstractObject;
begin
  Result := nil;

  vTok := JSONTokener.create(AJson);
  try
    if vTok <> nil then
    begin
      vObj := vTok.nextValue;
      if vObj <> nil then
      begin
        if vObj is TJSONobject then
          Result := TRALJSONObject.Create
        else if vObj is TJSONArray then
          Result := TRALJSONArray.Create
        else
          Result := TRALJSONValue.Create;
        Result.SetJsonObj(vObj);
      end
    end;
  finally
    FreeAndNil(vTok);
  end;
end;

class function ParseJSON(AStream : TStream) : TRALJSONValue;
var
  vJson : string;
begin
  Result := nil;
  if not Assigned(AStream) then
    Exit;

  SetLength(vJson, AStream.Size);
  AStream.Read(PChar(vJson)^, AStream.Size);
  Result := ParseJSON(vJson);
end;

{ TRALJSONArray }

function TRALJSONArray.JsonObject : TJSONArray;
begin
  Result := TJSONArray(GetJsonObj);
end;

constructor TRALJSONArray.Create;
var
  vObj : TJSONArray;
begin
  inherited;
  vObj := TJSONArray.Create;
  SetJsonObj(vObj);
end;

function TRALJSONArray.Add(const AValue : TRALJSONValue) : TRALJSONArray;
begin
  JsonObject.put(AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue : string) : TRALJSONArray;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue : int64) : TRALJSONArray;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue : integer) : TRALJSONArray;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue : double) : TRALJSONArray;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue : boolean) : TRALJSONArray;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArray.Add : TRALJSONArray;
begin
  JsonObject.put(uJSON.NULL.Create);
  Result := Self;
end;

function TRALJSONArray.Get(AIndex : integer) : TRALJSONValue;
var
  vObj : TZAbstractObject;
begin
  vObj := JsonObject.opt(AIndex);
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

function TRALJSONArray.Count : integer;
begin
  Result := JsonObject.length;
end;

{ TRALJSONObject }

function TRALJSONObject.JsonObject : TJSONObject;
begin
  Result := TJSONObject(GetJsonObj);
end;

constructor TRALJSONObject.Create;
var
  vObj : TJSONObject;
begin
  inherited;
  vObj := TJSONObject.Create;
  SetJsonObj(vObj);
end;

function TRALJSONObject.Add(const AStr : string; const AValue : TRALJSONValue) : TRALJSONObject;
begin
  JsonObject.put(AStr,AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr : string; const AValue : string) : TRALJSONObject;
begin
  JsonObject.put(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr : string; const AValue : int64) : TRALJSONObject;
begin
  JsonObject.put(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr : string; const AValue : integer) : TRALJSONObject;
begin
  JsonObject.put(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr : string; const AValue : double) : TRALJSONObject;
begin
  JsonObject.put(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr : string; const AValue : boolean) : TRALJSONObject;
begin
  JsonObject.put(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr : string) : TRALJSONObject;
begin
  JsonObject.put(AStr,uJSON.NULL.Create);
  Result := Self;
end;

function TRALJSONObject.Count : integer;
begin
  Result := JsonObject.length;
end;

function TRALJSONObject.Get(AName : string) : TRALJSONValue;
var
  vObj : TZAbstractObject;
begin
  Result := nil;
  vObj := JsonObject.opt(AName);
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

function TRALJSONObject.Get(AIndex : integer) : TRALJSONValue;
var
  vObj : TZAbstractObject;
  vName : string;
begin
  Result := nil;
  aName := GetName(AIndex);
  if vName <> '' then begin
    vObj := JsonObject.opt(vName);
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
end;

function TRALJSONObject.GetName(AIndex : integer) : string;
var
  vKey : TStringList;
begin
  vKey := JsonObject.keys;
  try
    if (AIndex >= 0) and (AIndex < vKey.Count) then
      Result := vKey.Strings[AIndex]
    else
      Result := '';
  finally
    FreeAndNil(vKey);
  end;
end;

{ TRALJSONValue }

procedure TRALJSONValue.SetJsonObj(AObj : TZAbstractObject; AFree : boolean);
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);

  FJsonObj := AObj;
  FFreeObj := AFree;
end;

function TRALJSONValue.GetJsonObj : TZAbstractObject;
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

  if FJsonObj is _Number then
    Result := rjtNumber
  else if FJsonObj is _Boolean then
    Result := rjtBoolean
  else if FJsonObj is TJSONObject then
    Result := rjtObject
  else if FJsonObj is TJSONArray then
    Result := rjtArray;
end;

function TRALJSONValue.IsNull : boolean;
begin
  if FJsonObj <> nil then
    Result := FJsonObj is uJSON.NULL
  else
    Result := True;
end;

function TRALJSONValue.AsValue : variant;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.toString
  else
    Result := Variants.Null;
end;

function TRALJSONValue.AsString : string;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.toString
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
    Result := _Number(FJsonObj).intValue;
  end
  else if JsonType = rjtString then
  begin
    Result := StrToInt64Def(FJsonObj.toString,-1)
  end
  else if JsonType = rjtBoolean then
  begin
    if _Boolean(FJsonObj).toString = 'true' then
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
    Result := _Number(FJsonObj).doubleValue;
  end
  else if JsonType = rjtString then
  begin
    Result := StrToFloatDef(FJsonObj.toString,-1)
  end;
end;

function TRALJSONValue.AsBoolean : boolean;
begin
  Result := False;
  if FJsonObj = nil then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := _Number(FJsonObj).doubleValue;
  end
  else if JsonType = rjtString then
  begin
    Result := (FJsonObj.toString = '1') or (FJsonObj.toString = 'S') or
              (FJsonObj.toString = 's') or (SameText(FJsonObj.toString,'true'));
  end
  else if JsonType = rjtBoolean then
  begin
    Result := FJsonObj.toString = 'true';
  end;
end;

function TRALJSONValue.ToJson : string;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.toString;
  else
    Result := '';
end;
