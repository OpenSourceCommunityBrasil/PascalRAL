{
  RALJSON_lkJSON
  SouceForce: https://sourceforge.net/projects/lkjson
  Version: 1.07 de 06/11/2009

  Unit criada para ser utilizar RAL com Delphi antigos, especificadamente
  Delphi 7 a Delphi 2009
}

type

  { TRALJSONValue }

  TRALJSONValue = class
  private
    FJsonObj : TlkJSON;
    FFreeObj : boolean;
  protected
    procedure SetJsonObj(AObj : TlkJSON; AFree : boolean = True);
    function GetJsonObj : TlkJSON;
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
    function JsonObject : TlkJSONobject;
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
    function JsonObject : TlkJSONlist;
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
  end;

implementation

{ TRALJSONValue }

procedure TRALJSONValue.SetJsonObj(AObj: TlkJSON; AFree : boolean = True);
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);
  FJsonObj := AObj;
  FFreeObj := AFree;
end;

function TRALJSONValue.GetJsonObj : TlkJSON;
begin
  Result := FJsonObj;
end;

function TRALJSONValue.AsBoolean: boolean;
begin
  Result := False;
  if FJsonObj = nil then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := TlkJSONNumber(FJsonObj).Value = 1;
  end
  else if JsonType = rjtString then
  begin
    Result := (FJsonObj.Value = '1') or (FJsonObj.Value = 'S') or
              (FJsonObj.Value = 's') or (SameText(FJsonObj.Value,'true'));
  end
  else if JsonType = rjtBoolean then
  begin
    Result := TlkJSONboolean(FJsonObj).Value;
  end;
end;

function TRALJSONValue.AsFloat: double;
begin
  Result := -1;
  if FJsonObj = nil then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := Extended(TlkJSONNumber(FJsonObj).Value);
  end
  else if JsonType = rjtString then
  begin
    Result := StrToFloatDef(FJsonObj.Value,-1)
  end;
end;

function TRALJSONValue.AsInteger: int64;
begin
  Result := -1;
  if FJsonObj = nil then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := Trunc(Extended(TlkJSONNumber(FJsonObj).Value));
  end
  else if JsonType = rjtString then
  begin
    Result := StrToInt64Def(FJsonObj.Value,-1)
  end
  else if JsonType = rjtBoolean then
  begin
    if Boolean(TlkJSONboolean(FJsonObj).Value) then
      Result := 1
    else
      Result := 0;
  end;
end;

function TRALJSONValue.AsString: string;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.Value
  else
    Result := 'null';
end;

function TRALJSONValue.IsNull: boolean;
begin
  if FJsonObj <> nil then
    Result := FJsonObj is TlkJSONNull
  else
    Result := True;
end;

function TRALJSONValue.JsonType: TRALJSONType;
begin
  Result := rjtString;
  if FJsonObj = nil then
    Exit;

  if FJsonObj is TlkJSONnumber then
    Result := rjtNumber
  else if FJsonObj is TlkJSONboolean then
    Result := rjtBoolean
  else if FJsonObj is TlkJSONObject then
    Result := rjtObject
  else if FJsonObj is TlkJSONlist then
    Result := rjtArray;
end;

function TRALJSONValue.AsValue: variant;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.Value
  else
    Result := Variants.Null;
end;

function TRALJSONValue.ToJson: string;
begin
  if FJsonObj <> nil then
    Result := TlkJSON.GenerateText(FJsonObj)
  else
    Result := '';
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

{ TRALJSONObject }

function TRALJSONObject.Add(const AStr: string; const AValue: integer): TRALJSONObject;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string; const AValue: int64): TRALJSONObject;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string; const AValue: string
  ): TRALJSONObject;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string): TRALJSONObject;
begin
  JsonObject.Add(AStr,TlkJSONnull.Create)
end;

function TRALJSONObject.Count: integer;
begin
  Result := JsonObject.Count;
end;

function TRALJSONObject.Add(const AStr: string; const AValue: TRALJSONValue): TRALJSONObject;
begin
  JsonObject.Add(AStr,AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string; const AValue: boolean): TRALJSONObject;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string; const AValue: double): TRALJSONObject;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

constructor TRALJSONObject.Create;
var
  jObj : TlkJSONObject;
begin
  inherited;
  jObj := TlkJSONObject.Create;
  SetJsonObj(jObj);
end;

function TRALJSONObject.Get(AName: string): TRALJSONValue;
var
  vObj : TlkJSON;
begin
  Result := nil;
  vObj := JsonObject.Field[AName];
  if vObj <> nil then
  begin
    if vObj is TlkJSONobject then
      Result := TRALJSONObject.Create
    else if vObj is TlkJSONlist then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(vObj,False);
  end;
end;

function TRALJSONObject.Get(AIndex: integer): TRALJSONValue;
var
  vObj : TlkJSON;
begin
  Result := nil;
  vObj := JsonObject.FieldByIndex[AIndex];
  if vObj <> nil then
  begin
    if vObj is TlkJSONobject then
      Result := TRALJSONObject.Create
    else if vObj is TlkJSONlist then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(vObj,False);
  end;
end;

function TRALJSONObject.GetName(AIndex: integer): string;
begin
  Result := JsonObject.NameOf[AIndex];
end;

function TRALJSONObject.JsonObject: TlkJSONobject;
begin
  Result := TlkJSONobject(GetJsonObj);
end;

{ TRALJSON }

class function TRALJSON.ParseJSON(AJson: String): TRALJSONValue;
var
  jObj : TlkJSON;
begin
  Result := nil;

  jObj := TlkJSON.ParseText(AJson);

  if jobj <> nil then
  begin
    if jObj is TlkJSONobject then
      Result := TRALJSONObject.Create
    else if jObj is TlkJSONlist then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(jObj);
  end;
end;

{ TRALJSONArray }

function TRALJSONArray.Add(const AValue: int64): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: string): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: TRALJSONValue): TRALJSONArray;
begin
  JsonObject.Add(AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: integer): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add: TRALJSONArray;
begin
  JsonObject.Add(TlkJSONnull.Create);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: boolean): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: double): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Count: integer;
begin
  Result := JsonObject.Count
end;

constructor TRALJSONArray.Create;
var
  jObj : TlkJSONlist;
begin
  inherited;
  jObj := TlkJSONlist.Create;
  SetJsonObj(jObj);
end;

function TRALJSONArray.Get(AIndex: integer): TRALJSONValue;
var
  vObj : TlkJSON;
begin
  inherited;
  Result := nil;
  vObj := JsonObject.Child[AIndex];
  if vObj <> nil then
  begin
    if vObj is TlkJSONobject then
      Result := TRALJSONObject.Create
    else if vObj is TlkJSONlist then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(vObj,False);
  end;
end;

function TRALJSONArray.JsonObject: TlkJSONlist;
begin
  Result := TlkJSONlist(GetJsonObj);
end;
