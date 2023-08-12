{
  RALJSON_uJSON
  SouceForce: https://sourceforge.net/projects/is-webstart/
  Version: 1.06 de 25/03/2010

  Unit criada para ser utilizar RAL com Delphi antigos, especificadamente
  Delphi 7 a Delphi 2009
}

unit RALJSON_uJSON;

interface

uses
  uJSON,
  SysUtils, Classes, Variants, RALTypes;

type

  { TRALJSONValueBase }

  TRALJSONValueBase = class
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
    class function ParseJSON(AJson : String) : TRALJSONValueBase;
  end;

implementation

{ TRALJSONBase }

class function TRALJSONBase.ParseJSON(AJson : String) : TRALJSONValueBase;
var
  jTok : JSONTokener;
  jObj : TZAbstractObject;
begin
  Result := nil;

  jTok := JSONTokener.create(AJson);
  try
    if jTok <> nil then
    begin
      jObj := jTok.nextValue;
      if jObj <> nil then
      begin
        if jObj is TJSONobject then
          Result := TRALJSONObjectBase.Create
        else if jObj is TJSONArray then
          Result := TRALJSONArrayBase.Create
        else
          Result := TRALJSONValueBase.Create;
        Result.SetJsonObj(jObj);
      end
    end;
  finally
    FreeAndNil(jTok);
  end;
end;

{ TRALJSONArrayBase }

function TRALJSONArrayBase.JsonObject : TJSONArray;
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

function TRALJSONArrayBase.Add(const AValue : TRALJSONValueBase) : TRALJSONArrayBase;
begin
  JsonObject.put(AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue : string) : TRALJSONArrayBase;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue : int64) : TRALJSONArrayBase;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue : integer) : TRALJSONArrayBase;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue : double) : TRALJSONArrayBase;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue : boolean) : TRALJSONArrayBase;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add : TRALJSONArrayBase;
begin
  JsonObject.put(uJSON.NULL.Create);
  Result := Self;
end;

function TRALJSONArrayBase.Get(AIndex : integer) : TRALJSONValueBase;
var
  vObj : TZAbstractObject;
begin
  vObj := JsonObject.opt(AIndex);
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

function TRALJSONArrayBase.Count : integer;
begin
  Result := JsonObject.length;
end;

{ TRALJSONObjectBase }

function TRALJSONObjectBase.JsonObject : TJSONObject;
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

function TRALJSONObjectBase.Add(const AStr : string; const AValue : TRALJSONValueBase) : TRALJSONObjectBase;
begin
  JsonObject.put(AStr,AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr : string; const AValue : string) : TRALJSONObjectBase;
begin
  JsonObject.put(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr : string; const AValue : int64) : TRALJSONObjectBase;
begin
  JsonObject.put(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr : string; const AValue : integer) : TRALJSONObjectBase;
begin
  JsonObject.put(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr : string; const AValue : double) : TRALJSONObjectBase;
begin
  JsonObject.put(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr : string; const AValue : boolean) : TRALJSONObjectBase;
begin
  JsonObject.put(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr : string) : TRALJSONObjectBase;
begin
  JsonObject.put(AStr,uJSON.NULL.Create);
  Result := Self;
end;

function TRALJSONObjectBase.Count : integer;
begin
  Result := JsonObject.length;
end;

function TRALJSONObjectBase.Get(AName : string) : TRALJSONValueBase;
var
  vObj : TZAbstractObject;
begin
  Result := nil;
  vObj := JsonObject.opt(AName);
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

function TRALJSONObjectBase.Get(AIndex : integer) : TRALJSONValueBase;
var
  vObj : TZAbstractObject;
  aName : string;
begin
  Result := nil;
  aName := GetName(AIndex);
  if aName <> '' then begin
    vObj := JsonObject.opt(AName);
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
end;

function TRALJSONObjectBase.GetName(AIndex : integer) : string;
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

{ TRALJSONValueBase }

procedure TRALJSONValueBase.SetJsonObj(AObj : TZAbstractObject; AFree : boolean);
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);

  FJsonObj := AObj;
  FFreeObj := AFree;
end;

function TRALJSONValueBase.GetJsonObj : TZAbstractObject;
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

  if FJsonObj is _Number then
    Result := rjtNumber
  else if FJsonObj is _Boolean then
    Result := rjtBoolean
  else if FJsonObj is TJSONObject then
    Result := rjtObject
  else if FJsonObj is TJSONArray then
    Result := rjtArray;
end;

function TRALJSONValueBase.IsNull : boolean;
begin
  if FJsonObj <> nil then
    Result := FJsonObj is uJSON.NULL
  else
    Result := True;
end;

function TRALJSONValueBase.AsValue : variant;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.toString
  else
    Result := Variants.Null;
end;

function TRALJSONValueBase.AsString : string;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.toString
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

function TRALJSONValueBase.AsFloat : double;
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

function TRALJSONValueBase.AsBoolean : boolean;
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

function TRALJSONValueBase.ToJson : string;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.toString;
  else
    Result := '';
end;

end.

