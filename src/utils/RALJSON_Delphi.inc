{ @abstract Partial unit used to map JSON with the System.JSON library
  This unit is defined under RALJson.pas unit
}

uses
  {$IFDEF DELPHIXE6UP}
  System.JSON,
  {$ELSE}
  DBXJSON,
  {$ENDIF}
  Variants, Classes, SysUtils, RALTypes;

type
  { TRALJSONValue }

  TRALJSONValue = class
  private
    FJsonObj: TJSONValue;
    FFreeObj: Boolean;
    FListObj: TList;
    FMarkFree: Boolean;
  protected
    procedure AddObj(AObj: TRALJSONValue);
    procedure ClearObjs;
    function GetJsonObj: TJSONValue;
    procedure SetJsonObj(AObj: TJSONValue; AFree: Boolean = True);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function AsValue: Variant;
    function AsString: StringRAL;
    function AsInteger: Int64RAL;
    function AsFloat: DoubleRAL;
    function AsBoolean: Boolean;
    function JsonType: TRALJSONType;
    function IsNull: Boolean;
    function ToJSON: StringRAL;
  end;

  { TRALJSONObject }

  TRALJSONObject = class(TRALJSONValue)
  protected
    function JsonObject: TJSONObject;
  public
    constructor Create; override;

    function Add(const AStr: StringRAL; const AValue: TRALJSONValue): TRALJSONObject; overload;
    function Add(const AStr: StringRAL; const AValue: StringRAL): TRALJSONObject; overload;
    function Add(const AStr: StringRAL; const AValue: Int64RAL): TRALJSONObject; overload;
    function Add(const AStr: StringRAL; const AValue: IntegerRAL): TRALJSONObject; overload;
    function Add(const AStr: StringRAL; const AValue: DoubleRAL): TRALJSONObject; overload;
    function Add(const AStr: StringRAL; const AValue: Boolean): TRALJSONObject; overload;
    function Add(const AStr: StringRAL): TRALJSONObject; overload;
    function Count: integer;
    function Get(const AName: StringRAL): TRALJSONValue; overload;
    function Get(AIndex: IntegerRAL): TRALJSONValue; overload;
    function GetName(AIndex: IntegerRAL): StringRAL;
  end;

  { TRALJSONArray }

  TRALJSONArray = class(TRALJSONValue)
  protected
    function JsonObject: TJSONArray;
  public
    constructor Create; override;
    function Add(const AValue: TRALJSONValue): TRALJSONArray; overload;
    function Add(const AValue: StringRAL): TRALJSONArray; overload;
    function Add(const AValue: Int64RAL): TRALJSONArray; overload;
    function Add(const AValue: IntegerRAL): TRALJSONArray; overload;
    function Add(const AValue: DoubleRAL): TRALJSONArray; overload;
    function Add(const AValue: Boolean): TRALJSONArray; overload;
    function Add: TRALJSONArray; overload;
    function Count: IntegerRAL;
    function Get(AIndex: IntegerRAL): TRALJSONValue;
  end;

  { TRALJSON }

  TRALJSON = class
  public
    class function ParseJSON(const AJSON: StringRAL): TRALJSONValue; overload;
    class function ParseJSON(AStream: TStream): TRALJSONValue; overload;
  end;

implementation

{ TRALJSONValue }

procedure TRALJSONValue.SetJsonObj(AObj: TJSONValue; AFree: Boolean = True);
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);
  FJsonObj := AObj;
  FFreeObj := AFree;
end;

function TRALJSONValue.GetJsonObj: TJSONValue;
begin
  Result := FJsonObj;
end;

function TRALJSONValue.IsNull: Boolean;
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
  {$ELSE}
  else if (FJsonObj is TJSONTrue) or (FJsonObj is TJSONFalse) then
  {$ENDIF}
    Result := rjtBoolean
  else if FJsonObj is TJSONObject then
    Result := rjtObject
  else if FJsonObj is TJSONArray then
    Result := rjtArray;
end;

function TRALJSONValue.AsBoolean: Boolean;
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
    Result := (FJsonObj.Value = '1') or (FJsonObj.Value = 'S') or (FJsonObj.Value = 's')
      or (SameText(FJsonObj.Value, 'true'));
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

function TRALJSONValue.AsFloat: DoubleRAL;
begin
  Result := -1;
  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if JsonType = rjtNumber then
    Result := TJSONNumber(FJsonObj).AsDouble
  else if JsonType = rjtString then
    Result := StrToFloatDef(FJsonObj.Value, -1);
end;

function TRALJSONValue.AsInteger: Int64RAL;
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
    Result := StrToInt64Def(FJsonObj.Value, -1)
  end
  else if JsonType = rjtBoolean then
  begin
  {$IFDEF DELPHI10_0UP}
    if TJSONBool(FJsonObj).AsBoolean then
  {$ELSE}
    if (FJsonObj is TJSONTrue) then
  {$ENDIF}
      Result := 1
    else
      Result := 0;
  end;
end;

function TRALJSONValue.ToJSON: StringRAL;
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

function TRALJSONValue.AsString: StringRAL;
begin
  Result := '';
  if (Self <> nil) and (FJsonObj <> nil) then
  begin
    if (FJsonObj is TJSONObject) or (FJsonObj is TJSONArray) then
      Result := ToJSON
    else
      Result := FJsonObj.Value
  end;
end;

function TRALJSONValue.AsValue: Variant;
begin
  if (Self <> nil) and (FJsonObj <> nil) then
    Result := FJsonObj.Value
  else
    Result := Variants.Null;
end;

procedure TRALJSONValue.AddObj(AObj: TRALJSONValue);
begin
  if AObj <> nil then
  begin
    AObj.FFreeObj := False;
    if not AObj.FMarkFree then
      FListObj.Add(AObj);
    AObj.FMarkFree := True;
  end;
end;

procedure TRALJSONValue.ClearObjs;
begin
  while FListObj.Count > 0 do
  begin
    TObject(FListObj.Items[FListObj.Count - 1]).Free;
    FListObj.Delete(FListObj.Count - 1);
  end;
end;

constructor TRALJSONValue.Create;
begin
  inherited;
  FJsonObj := nil;
  FFreeObj := True;
  FListObj := TList.Create;
  FMarkFree := False;
end;

destructor TRALJSONValue.Destroy;
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FJsonObj.Free;
  ClearObjs;
  FListObj.Free;
  inherited;
end;

{ TRALJSONObject }

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: IntegerRAL): TRALJSONObject;
begin
  Result := Self;
  JsonObject.AddPair(AStr, TJSONNumber.Create(AValue));
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: Int64RAL): TRALJSONObject;
begin
  Result := Self;
  JsonObject.AddPair(AStr, TJSONNumber.Create(AValue));
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: StringRAL): TRALJSONObject;
begin
  Result := Self;
  {$IFDEF DELPHIXEUP}
  JsonObject.AddPair(AStr, AValue);
  {$ELSE}
  JsonObject.AddPair(AStr, TJSONString.Create(AValue));
  {$ENDIF}
end;

function TRALJSONObject.Add(const AStr: StringRAL): TRALJSONObject;
begin
  Result := Self;
  JsonObject.AddPair(AStr, TJSONNull.Create);
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: TRALJSONValue): TRALJSONObject;
begin
  Result := Self;
  if AValue <> nil then
  begin
    JsonObject.AddPair(AStr, AValue.GetJsonObj);
    AddObj(AValue);
  end
  else
  begin
    JsonObject.AddPair(AStr, TJSONNull.Create);
  end;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: Boolean): TRALJSONObject;
begin
  Result := Self;
  {$IFDEF DELPHI10_0UP}
  JsonObject.AddPair(AStr, TJSONBool.Create(AValue));
  {$ELSE}
  if AValue then
    JsonObject.AddPair(AStr, TJSONTrue.Create)
  else
    JsonObject.AddPair(AStr, TJSONFalse.Create);
  {$ENDIF}
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: DoubleRAL): TRALJSONObject;
begin
  Result := Self;
  JsonObject.AddPair(AStr, TJSONNumber.Create(AValue));
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
  vObj: TJSONObject;
begin
  inherited;
  vObj := TJSONObject.Create;
  SetJsonObj(vObj);
end;

function TRALJSONObject.Get(AIndex: IntegerRAL): TRALJSONValue;
var
  vObj: TJSONPair;
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
    Result.SetJsonObj(vObj.JsonValue, False);
    AddObj(Result);
  end;
end;

function TRALJSONObject.GetName(AIndex: IntegerRAL): StringRAL;
var
  vObj: TJSONPair;
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

function TRALJSONObject.Get(const AName: StringRAL): TRALJSONValue;
var
  {$IFNDEF DELPHI10_1UP}
  vPair: TJSONPair;
  {$ENDIF}
  {$IFNDEF DELPHIXEUP}
  vInt: IntegerRAL;
  {$ENDIF}
  vObj: TJSONValue;
begin
  Result := nil;

  if Self = nil then
    Exit;

  {$IFDEF DELPHI10_1UP}
    vObj := JsonObject.FindValue(AName);
  {$ELSE}
    vObj := nil;
    {$IFNDEF DELPHIXEUP}
      for vInt := 0 to pred(JsonObject.Size) do
      begin
        vPair := JsonObject.Get(vInt);
        if LowerCase(vPair.JsonString.Value) = LowerCase(AName) then
        begin
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
    Result.SetJsonObj(vObj, False);
    AddObj(Result);
  end;
end;

function TRALJSONObject.JsonObject: TJSONObject;
begin
  Result := TJSONObject(GetJsonObj);
end;

{ TRALJSON }

class function TRALJSON.ParseJSON(const AJSON: StringRAL): TRALJSONValue;
var
  vObj: TJSONValue;
begin
  Result := nil;
  {$IFDEF DELPHIXEUP}
    vObj := TJSONObject.ParseJSONValue(AJSON);
  {$ELSE}
    // copiado codigo de conversao do 10.3 RIO
    vObj := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(AJSON), 0);
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

class function TRALJSON.ParseJSON(AStream: TStream): TRALJSONValue;
var
  vJson: StringRAL;
begin
  Result := nil;
  if not Assigned(AStream) then
    Exit;
  SetLength(vJson, AStream.Size);
  AStream.Read(PChar(vJson)^, AStream.Size);
  Result := ParseJSON(vJson);
end;

{ TRALJSONArray }

function TRALJSONArray.Add(const AValue: Int64RAL): TRALJSONArray;
begin
  Result := Self;
  JsonObject.Add(AValue);
end;

function TRALJSONArray.Add(const AValue: StringRAL): TRALJSONArray;
begin
  Result := Self;
  JsonObject.Add(AValue);
end;

function TRALJSONArray.Add(const AValue: TRALJSONValue): TRALJSONArray;
var
  vObj: TJSONValue;
begin
  Result := Self;
  if AValue <> nil then
  begin
    vObj := AValue.GetJsonObj;
    if vObj is TJSONObject then
      JsonObject.Add(TJSONObject(vObj))
    else if vObj is TJSONArray then
      JsonObject.Add(TJSONArray(vObj))
    else
      JsonObject.AddElement(vObj);

    AddObj(AValue);
  end
  else
  begin
    JsonObject.AddElement(TJSONNull.Create);
  end;
end;

function TRALJSONArray.Add(const AValue: IntegerRAL): TRALJSONArray;
begin
  Result := Self;
  JsonObject.Add(AValue);
end;

function TRALJSONArray.Add: TRALJSONArray;
begin
  Result := Self;
  JsonObject.AddElement(TJSONNull.Create);
end;

function TRALJSONArray.Add(const AValue: Boolean): TRALJSONArray;
begin
  Result := Self;
  JsonObject.Add(AValue);
end;

function TRALJSONArray.Add(const AValue: DoubleRAL): TRALJSONArray;
begin
  Result := Self;
  JsonObject.Add(AValue);
end;

function TRALJSONArray.Count: IntegerRAL;
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
  vObj: TJSONArray;
begin
  inherited;
  vObj := TJSONArray.Create;
  SetJsonObj(vObj);
end;

function TRALJSONArray.Get(AIndex: IntegerRAL): TRALJSONValue;
var
  vObj: TJSONValue;
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
    Result.SetJsonObj(vObj, False);
    AddObj(Result);
  end;
end;

function TRALJSONArray.JsonObject: TJSONArray;
begin
  Result := TJSONArray(GetJsonObj);
end;
