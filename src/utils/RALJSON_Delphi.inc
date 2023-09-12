{
  RALJSON_Delphi

  Unit criada para ser utilizar RAL com Delphi, especificadamente
  Delphi 2010 ou superior
}

type
  { TRALJSONValue }

  TRALJSONValue = class
  private
    FJsonObj : TJSONValue;
    FFreeObj : boolean;
  protected
    procedure SetJsonObj(AObj : TJSONValue; AFree : boolean = True);
    function GetJsonObj : TJSONValue;
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

    function ToJSON : string;
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
    class function ParseJSON(AJSON : String) : TRALJSONValue; overload;
    class function ParseJSON(AStream : TStream) : TRALJSONValue; overload;
  end;

implementation

{ TRALJSONValue }

procedure TRALJSONValue.SetJsonObj(AObj: TJSONValue; AFree : boolean = true);
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);
  FJsonObj := AObj;
  FFreeObj := AFree;
end;

function TRALJSONValue.GetJsonObj : TJSONValue;
begin
  Result := FJsonObj;
end;

function TRALJSONValue.IsNull: boolean;
begin
  if (Self <> nil) and (FJsonObj <> nil) then
    Result := FJsonObj is TJSONNull
  else
    Result := True;
end;

function TRALJSONValue.JsonType: TRALJSONType;
begin
  Result := rjtString;
  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if FJsonObj is TJSONNumber then
    Result := rjtNumber
  {$IFDEF DELPHI10_0UP}
    else if FJsonObj is TJSONBool then
      Result := rjtBoolean
  {$ELSE}
    else if (FJsonObj is TJSONTrue) or
            (FJsonObj is TJSONFalse) then
      Result := rjtBoolean
  {$ENDIF}
  else if FJsonObj is TJSONObject then
    Result := rjtObject
  else if FJsonObj is TJSONArray then
    Result := rjtArray;
end;

function TRALJSONValue.AsBoolean: boolean;
begin
  Result := False;
  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := TJSONNumber(FJsonObj).AsDouble = 1;
  end
  else if JsonType = rjtString then
  begin
    Result := (FJsonObj.Value = '1') or (FJsonObj.Value = 'S') or
              (FJsonObj.Value = 's') or (SameText(FJsonObj.Value,'true'));
  end
  else if JsonType = rjtBoolean then
  begin
    {$IFDEF DELPHI10_0UP}
      Result := TJSONBool(FJsonObj).AsBoolean;
    {$ELSE}
      Result := FJsonObj is TJSONTrue
    {$ENDIF}
  end;
end;

function TRALJSONValue.AsFloat: double;
begin
  Result := -1;
  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if JsonType = rjtNumber then
    Result := TJSONNumber(FJsonObj).AsDouble
  else if JsonType = rjtString then
    Result := StrToFloatDef(FJsonObj.Value,-1);
end;

function TRALJSONValue.AsInteger: int64;
begin
  Result := -1;
  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := TJSONNumber(FJsonObj).AsInt64
  end
  else if JsonType = rjtString then
  begin
    Result := StrToInt64Def(FJsonObj.Value,-1)
  end
  else if JsonType = rjtBoolean then
  begin
    {$IFDEF DELPHI10_0UP}
      if TJSONBool(FJsonObj).AsBoolean then
        Result := 1
      else
        Result := 0;
    {$ELSE}
      if (FJsonObj is TJSONTrue) then
        Result := 1
      else
        Result := 0;
    {$ENDIF}
  end;
end;

function TRALJSONValue.ToJSON: string;
begin
  if (Self <> nil) and (FJsonObj <> nil) then
    {$IFDEF DELPHIXE7UP}
      Result := FJsonObj.ToJSON
    {$ELSE}
      Result := FJsonObj.ToString
    {$ENDIF}
  else
    Result := '';
end;

function TRALJSONValue.AsString: string;
begin
  if Self = nil then
  begin
    Result := '';
  end
  else if FJsonObj <> nil then
  begin
    if (FJsonObj is TJSONObject) or (FJsonObj is TJSONArray) then
      Result := ToJSON
    else
      Result := FJsonObj.Value
  end
  else
  begin
    Result := 'null';
  end;
end;

function TRALJSONValue.AsValue: variant;
begin
  if (Self <> nil) and (FJsonObj <> nil) then
    Result := FJsonObj.Value
  else
    Result := Variants.Null;
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
    FJsonObj.Free;
  inherited;
end;

{ TRALJSONObject }

function TRALJSONObject.Add(const AStr: string;
                                  const AValue: integer): TRALJSONObject;
begin
  JsonObject.AddPair(AStr,TJSONNumber.Create(AValue));
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string;
                              const AValue: int64): TRALJSONObject;
begin
  JsonObject.AddPair(AStr,TJSONNumber.Create(AValue));
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string; const AValue: string
  ): TRALJSONObject;
begin
  {$IFDEF DELPHIXEUP}
    JsonObject.AddPair(AStr,AValue);
  {$ELSE}
    JsonObject.AddPair(AStr,TJSONString.Create(AValue));
  {$ENDIF}
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string): TRALJSONObject;
begin
  JsonObject.AddPair(AStr,TJSONNull.Create);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string;
  const AValue: TRALJSONValue): TRALJSONObject;
begin
  JsonObject.AddPair(AStr,AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string;
                            const AValue: boolean): TRALJSONObject;
begin
  {$IFDEF DELPHI10_0UP}
    JsonObject.AddPair(AStr,TJSONBool.Create(AValue));
  {$ELSE}
    if AValue then
      JsonObject.AddPair(AStr,TJSONTrue.Create)
    else
      JsonObject.AddPair(AStr,TJSONFalse.Create);
  {$ENDIF}

  Result := Self;
end;

function TRALJSONObject.Add(const AStr: string;
                              const AValue: double): TRALJSONObject;
begin
  JsonObject.AddPair(AStr,TJSONNumber.Create(AValue));
  Result := Self;
end;

function TRALJSONObject.Count: integer;
begin
  if Self <> nil then
    {$IFDEF DELPHIXE6UP}
      Result := JsonObject.Count
    {$ELSE}
      Result := JsonObject.Size
    {$ENDIF}
  else
    Result := 0;
end;

constructor TRALJSONObject.Create;
var
  vObj : TJSONObject;
begin
  inherited;
  vObj := TJSONObject.Create;
  SetJsonObj(vObj);
end;

function TRALJSONObject.Get(AIndex: integer): TRALJSONValue;
var
  vObj : TJSONPair;
begin
  Result := nil;

  if Self = nil then
    Exit;

  {$IFDEF DELPHIXE6UP}
    vObj := JsonObject.Pairs[AIndex];
  {$ELSE}
    vObj := JsonObject.Get(AIndex);
  {$ENDIF}
  if vObj <> nil then
  begin
    if vObj.JsonValue is TJSONObject then
      Result := TRALJSONObject.Create
    else if vObj.JsonValue is TJSONArray then
      Result := TRALJSONObject.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(vObj.JsonValue,False);
  end;
end;

function TRALJSONObject.GetName(AIndex: integer): string;
var
  vObj : TJSONPair;
begin
  Result := '';

  if Self = nil then
    Exit;

  {$IFDEF DELPHIXE6UP}
    vObj := JsonObject.Pairs[AIndex];
  {$ELSE}
    vObj := JsonObject.Get(AIndex);
  {$ENDIF}
  if vObj <> nil then
    Result := vObj.JsonString.Value
end;

function TRALJSONObject.Get(AName: string): TRALJSONValue;
var
  {$IFNDEF DELPHI10_0UP}
    vPair : TJSONPair;
  {$ENDIF}
  {$IFNDEF DELPHIXEUP}
    vInt : integer;
  {$ENDIF}
  vObj : TJSONValue;
begin
  Result := nil;

  if Self = nil then
    Exit;

  {$IFDEF DELPHI10_0UP}
    vObj := JsonObject.FindValue(AName);
  {$ELSE}
    vObj := nil;
    {$IFNDEF DELPHIXEUP}
      for vInt := 0 to JsonObject.Size - 1 do begin
        vPair := JsonObject.Get(vInt);
        if LowerCase(vPair.JsonString.Value) = LowerCase(AName) then begin
          vObj := vPair.JsonValue;
          Break;
        end;
      end;
    {$ELSE}
      vPair := JsonObject.Get(AName);
      if vPair <> nil then
        vObj := vPair.JsonValue;
    {$ENDIF}
  {$ENDIF}
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

function TRALJSONObject.JsonObject: TJSONObject;
begin
  Result := TJSONObject(GetJsonObj);
end;

{ TRALJSON }

class function TRALJSON.ParseJSON(AJSON: String): TRALJSONValue;
var
  vObj : TJSONValue;
begin
  Result := nil;
  {$IFDEF DELPHIXEUP}
    vObj := TJSONObject.ParseJSONValue(AJSON);
  {$ELSE}
    // copiado codigo de conversao do 10.3 RIO
    vObj:= TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(AJSON),0);
  {$ENDIF}
  if vObj <> nil then
  begin
    if vObj is TJSONObject then
      Result := TRALJSONObject.Create
    else if vObj is TJSONArray then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(vObj);
  end;
end;

class function TRALJSON.ParseJSON(AStream : TStream) : TRALJSONValue;
var
  vJson: string;
begin
  Result := nil;
  if not Assigned(AStream) then
    Exit;
  SetLength(vJson, AStream.Size);
  AStream.Read(PChar(vJson)^, AStream.Size);
  Result := ParseJSON(vJson);
end;

{ TRALJSONArray }

function TRALJSONArray.Add(const AValue: int64): TRALJSONArray;
begin
  JsonObject.Add(AValue);
end;

function TRALJSONArray.Add(const AValue: string): TRALJSONArray;
begin
  JsonObject.Add(AValue);
end;

function TRALJSONArray.Add(const AValue: TRALJSONValue): TRALJSONArray;
var
  vObj : TJSONValue;
begin
  vObj := AValue.GetJsonObj;
  if vObj is TJSONObject then
    JsonObject.Add(TJSONObject(vObj))
  else if vObj is TJSONArray then
    JsonObject.Add(TJSONArray(vObj))
  else
    JsonObject.AddElement(vObj)
end;

function TRALJSONArray.Add(const AValue: integer): TRALJSONArray;
begin
  JsonObject.Add(AValue);
end;

function TRALJSONArray.Add: TRALJSONArray;
begin
  JsonObject.AddElement(TJSONNull.Create);
end;

function TRALJSONArray.Add(const AValue: boolean): TRALJSONArray;
begin
  JsonObject.Add(AValue);
end;

function TRALJSONArray.Add(const AValue: double): TRALJSONArray;
begin
  JsonObject.Add(AValue);
end;

function TRALJSONArray.Count: integer;
begin
  if Self <> nil then
    {$IFDEF DELPHIXE6UP}
      Result := JsonObject.Count
    {$ELSE}
      Result := JsonObject.Size
    {$ENDIF}
  else
    Result := 0;
end;

constructor TRALJSONArray.Create;
var
  vObj : TJSONArray;
begin
  inherited;
  vObj := TJSONArray.Create;
  SetJsonObj(vObj);
end;

function TRALJSONArray.Get(AIndex: integer): TRALJSONValue;
var
  vObj : TJSONValue;
begin
  inherited;
  Result := nil;

  if Self = nil then
    Exit;

  {$IFDEF DELPHIXE6UP}
    vObj := JsonObject.Items[AIndex];
  {$ELSE}
    vObj := JsonObject.Get(AIndex);
  {$ENDIF}
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

function TRALJSONArray.JsonObject: TJSONArray;
begin
  Result := TJSONArray(GetJsonObj);
end;
