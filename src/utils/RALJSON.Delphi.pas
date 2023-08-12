unit RALJSON.Delphi;

interface

{$I ../base/PascalRAL.inc}

// DBXJSON
// 2009  - CompVersion 20 - NOT Tested
// 2010  - CompVersion 21 - Tested OK
// XE    - CompVersion 22 - Tested OK
// XE2   - CompVersion 23 - Tested OK
// XE3   - CompVersion 24 - NOT Tested
// XE4   - CompVersion 25 - Tested OK
// XE5   - CompVersion 26 - Tested OK

// System.JSON
// XE6   - CompVersion 27 - Tested OK
// XE7   - CompVersion 28 - Tested OK
// XE8   - CompVersion 29 - Tested OK

uses
  {$IFDEF DELPHIXE6UP}
    System.JSON,
  {$ELSE}
    DBXJSON,
  {$ENDIF}
  Variants, SysUtils, Classes, RALTypes;

type
  { TRALJSONValueBase }

  TRALJSONValueBase = class
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

  { TRALJSONBase }

  TRALJSONBase = class
  public
    class function ParseJSON(AJSON : String) : TRALJSONValueBase;
  end;

implementation

{ TRALJSONValueBase }

procedure TRALJSONValueBase.SetJsonObj(AObj: TJSONValue; AFree : boolean = true);
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);
  FJsonObj := AObj;
  FFreeObj := AFree;
end;

function TRALJSONValueBase.GetJsonObj : TJSONValue;
begin
  Result := FJsonObj;
end;

function TRALJSONValueBase.IsNull: boolean;
begin
  if FJsonObj <> nil then
    Result := FJsonObj is TJSONNull
  else
    Result := True;
end;

function TRALJSONValueBase.JsonType: TRALJSONType;
begin
  Result := rjtString;
  if FJsonObj = nil then
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

function TRALJSONValueBase.AsBoolean: boolean;
begin
  Result := False;
  if FJsonObj = nil then
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

function TRALJSONValueBase.AsFloat: double;
begin
  Result := -1;
  if FJsonObj = nil then
    Exit;

  if JsonType = rjtNumber then
    Result := TJSONNumber(FJsonObj).AsDouble
  else if JsonType = rjtString then
    Result := StrToFloatDef(FJsonObj.Value,-1);
end;

function TRALJSONValueBase.AsInteger: int64;
begin
  Result := -1;
  if FJsonObj = nil then
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

function TRALJSONValueBase.ToJSON: string;
begin
  if FJsonObj <> nil then
    {$IFDEF DELPHIXE7UP}
      Result := FJsonObj.ToJSON
    {$ELSE}
      Result := FJsonObj.ToString
    {$ENDIF}
  else
    Result := '';
end;

function TRALJSONValueBase.AsString: string;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.Value
  else
    Result := 'null';
end;

function TRALJSONValueBase.AsValue: variant;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.Value
  else
    Result := Variants.Null;
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
    FJsonObj.Free;
  inherited;
end;

{ TRALJSONObjectBase }

function TRALJSONObjectBase.Add(const AStr: string;
                                  const AValue: integer): TRALJSONObjectBase;
begin
  JsonObject.AddPair(AStr,TJSONNumber.Create(AValue));
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string;
                              const AValue: int64): TRALJSONObjectBase;
begin
  JsonObject.AddPair(AStr,TJSONNumber.Create(AValue));
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string; const AValue: string
  ): TRALJSONObjectBase;
begin
  {$IFDEF DELPHIXEUP}
    JsonObject.AddPair(AStr,AValue);
  {$ELSE}
    JsonObject.AddPair(AStr,TJSONString.Create(AValue));
  {$ENDIF}
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string): TRALJSONObjectBase;
begin
  JsonObject.AddPair(AStr,TJSONNull.Create);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string;
  const AValue: TRALJSONValueBase): TRALJSONObjectBase;
begin
  JsonObject.AddPair(AStr,AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string;
                              const AValue: boolean): TRALJSONObjectBase;
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

function TRALJSONObjectBase.Add(const AStr: string;
                              const AValue: double): TRALJSONObjectBase;
begin
  JsonObject.AddPair(AStr,TJSONNumber.Create(AValue));
  Result := Self;
end;

function TRALJSONObjectBase.Count: integer;
begin
  {$IFDEF DELPHIXE6UP}
    Result := JsonObject.Count;
  {$ELSE}
    Result := JsonObject.Size;
  {$ENDIF}
end;

constructor TRALJSONObjectBase.Create;
var
  vObj : TJSONObject;
begin
  inherited;
  vObj := TJSONObject.Create;
  SetJsonObj(vObj);
end;

function TRALJSONObjectBase.Get(AIndex: integer): TRALJSONValueBase;
var
  vObj : TJSONPair;
begin
  Result := nil;
  {$IFDEF DELPHIXE6UP}
    vObj := JsonObject.Pairs[AIndex];
  {$ELSE}
    vObj := JsonObject.Get(AIndex);
  {$ENDIF}
  if vObj <> nil then
  begin
    if vObj.JsonValue is TJSONObject then
      Result := TRALJSONObjectBase.Create
    else if vObj.JsonValue is TJSONArray then
      Result := TRALJSONObjectBase.Create
    else
      Result := TRALJSONValueBase.Create;
    Result.SetJsonObj(vObj.JsonValue,False);
  end;
end;

function TRALJSONObjectBase.GetName(AIndex: integer): string;
var
  vObj : TJSONPair;
begin
  Result := '';
  {$IFDEF DELPHIXE6UP}
    vObj := JsonObject.Pairs[AIndex];
  {$ELSE}
    vObj := JsonObject.Get(AIndex);
  {$ENDIF}
  if vObj <> nil then
    Result := vObj.JsonString.Value
end;

function TRALJSONObjectBase.Get(AName: string): TRALJSONValueBase;
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
      Result := TRALJSONObjectBase.Create
    else if vObj is TJSONArray then
      Result := TRALJSONArrayBase.Create
    else
      Result := TRALJSONValueBase.Create;
    Result.SetJsonObj(vObj,False);
  end;
end;

function TRALJSONObjectBase.JsonObject: TJSONObject;
begin
  Result := TJSONObject(GetJsonObj);
end;

{ TRALJSONBase }

class function TRALJSONBase.ParseJSON(AJSON: String): TRALJSONValueBase;
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
      Result := TRALJSONObjectBase.Create
    else if vObj is TJSONArray then
      Result := TRALJSONArrayBase.Create;
    Result.SetJsonObj(vObj);
  end;
end;

{ TRALJSONArrayBase }

function TRALJSONArrayBase.Add(const AValue: int64): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
end;

function TRALJSONArrayBase.Add(const AValue: string): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
end;

function TRALJSONArrayBase.Add(const AValue: TRALJSONValueBase): TRALJSONArrayBase;
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

function TRALJSONArrayBase.Add(const AValue: integer): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
end;

function TRALJSONArrayBase.Add: TRALJSONArrayBase;
begin
  JsonObject.AddElement(TJSONNull.Create);
end;

function TRALJSONArrayBase.Add(const AValue: boolean): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
end;

function TRALJSONArrayBase.Add(const AValue: double): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
end;

function TRALJSONArrayBase.Count: integer;
begin
  {$IFDEF DELPHIXE6UP}
    Result := JsonObject.Count;
  {$ELSE}
    Result := JsonObject.Size;
  {$ENDIF}
end;

constructor TRALJSONArrayBase.Create;
var
  vObj : TJSONArray;
begin
  inherited;
  vObj := TJSONArray.Create;
  SetJsonObj(vObj);
end;

function TRALJSONArrayBase.Get(AIndex: integer): TRALJSONValueBase;
var
  vObj : TJSONValue;
begin
  inherited;
  Result := nil;
  {$IFDEF DELPHIXE6UP}
    vObj := JsonObject.Items[AIndex];
  {$ELSE}
    vObj := JsonObject.Get(AIndex);
  {$ENDIF}
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

function TRALJSONArrayBase.JsonObject: TJSONArray;
begin
  Result := TJSONArray(GetJsonObj);
end;

end.
