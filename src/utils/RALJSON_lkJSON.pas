{
  RALJSON_lkJSON
  SouceForce: https://sourceforge.net/projects/lkjson
  Version: 1.07 de 06/11/2009

  Unit criada para ser utilizar RAL com Delphi antigos, especificadamente
  Delphi 7 a Delphi 2009
}

unit RALJSON_lkJSON;

interface

uses
  ulkJSON,
  SysUtils, Classes, Variants, RALTypes;

type

  { TRALJSONValueBase }

  TRALJSONValueBase = class
  private
    FJsonObj : TlkJSONbase;
    FFreeObj : boolean;
  protected
    procedure SetJsonObj(AObj : TlkJSONbase; AFree : boolean = True);
    function GetJsonObj : TlkJSONbase;
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
    function JsonObject : TlkJSONobject;
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
    function JsonObject : TlkJSONlist;
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

{ TRALJSONValueBase }

procedure TRALJSONValueBase.SetJsonObj(AObj: TlkJSONbase; AFree : boolean = True);
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);
  FJsonObj := AObj;
  FFreeObj := AFree;
end;

function TRALJSONValueBase.GetJsonObj : TlkJSONbase;
begin
  Result := FJsonObj;
end;

function TRALJSONValueBase.AsBoolean: boolean;
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

function TRALJSONValueBase.AsFloat: double;
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

function TRALJSONValueBase.AsInteger: int64;
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

function TRALJSONValueBase.AsString: string;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.Value
  else
    Result := 'null';
end;

function TRALJSONValueBase.IsNull: boolean;
begin
  if FJsonObj <> nil then
    Result := FJsonObj is TlkJSONNull
  else
    Result := True;
end;

function TRALJSONValueBase.JsonType: TRALJSONType;
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

function TRALJSONValueBase.AsValue: variant;
begin
  if FJsonObj <> nil then
    Result := FJsonObj.Value
  else
    Result := Variants.Null;
end;

function TRALJSONValueBase.ToJson: string;
begin
  if FJsonObj <> nil then
    Result := TlkJSON.GenerateText(FJsonObj)
  else
    Result := '';
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

{ TRALJSONObjectBase }

function TRALJSONObjectBase.Add(const AStr: string; const AValue: integer): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string; const AValue: int64): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string; const AValue: string
  ): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,TlkJSONnull.Create)
end;

function TRALJSONObjectBase.Count: integer;
begin
  Result := JsonObject.Count;
end;

function TRALJSONObjectBase.Add(const AStr: string; const AValue: TRALJSONValueBase): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string; const AValue: boolean): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

function TRALJSONObjectBase.Add(const AStr: string; const AValue: double): TRALJSONObjectBase;
begin
  JsonObject.Add(AStr,AValue);
  Result := Self;
end;

constructor TRALJSONObjectBase.Create;
var
  jObj : TlkJSONObject;
begin
  inherited;
  jObj := TlkJSONObject.Create;
  SetJsonObj(jObj);
end;

function TRALJSONObjectBase.Get(AName: string): TRALJSONValueBase;
var
  vObj : TlkJSONbase;
begin
  Result := nil;
  vObj := JsonObject.Field[AName];
  if vObj <> nil then
  begin
    if vObj is TlkJSONobject then
      Result := TRALJSONObjectBase.Create
    else if vObj is TlkJSONlist then
      Result := TRALJSONArrayBase.Create
    else
      Result := TRALJSONValueBase.Create;
    Result.SetJsonObj(vObj,False);
  end;
end;

function TRALJSONObjectBase.Get(AIndex: integer): TRALJSONValueBase;
var
  vObj : TlkJSONbase;
begin
  Result := nil;
  vObj := JsonObject.FieldByIndex[AIndex];
  if vObj <> nil then
  begin
    if vObj is TlkJSONobject then
      Result := TRALJSONObjectBase.Create
    else if vObj is TlkJSONlist then
      Result := TRALJSONArrayBase.Create
    else
      Result := TRALJSONValueBase.Create;
    Result.SetJsonObj(vObj,False);
  end;
end;

function TRALJSONObjectBase.GetName(AIndex: integer): string;
begin
  Result := JsonObject.NameOf[AIndex];
end;

function TRALJSONObjectBase.JsonObject: TlkJSONobject;
begin
  Result := TlkJSONobject(GetJsonObj);
end;

{ TRALJSON }

class function TRALJSONBase.ParseJSON(AJson: String): TRALJSONValueBase;
var
  jObj : TlkJSONbase;
begin
  Result := nil;

  jObj := TlkJSON.ParseText(AJson);

  if jobj <> nil then
  begin
    if jObj is TlkJSONobject then
      Result := TRALJSONObjectBase.Create
    else if jObj is TlkJSONlist then
      Result := TRALJSONArrayBase.Create
    else
      Result := TRALJSONValueBase.Create;
    Result.SetJsonObj(jObj);
  end;
end;

{ TRALJSONArrayBase }

function TRALJSONArrayBase.Add(const AValue: int64): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue: string): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue: TRALJSONValueBase): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue: integer): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add: TRALJSONArrayBase;
begin
  JsonObject.Add(TlkJSONnull.Create);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue: boolean): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Add(const AValue: double): TRALJSONArrayBase;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArrayBase.Count: integer;
begin
  Result := JsonObject.Count
end;

constructor TRALJSONArrayBase.Create;
var
  jObj : TlkJSONlist;
begin
  inherited;
  jObj := TlkJSONlist.Create;
  SetJsonObj(jObj);
end;

function TRALJSONArrayBase.Get(AIndex: integer): TRALJSONValueBase;
var
  vObj : TlkJSONbase;
begin
  inherited;
  Result := nil;
  vObj := JsonObject.Child[AIndex];
  if vObj <> nil then
  begin
    if vObj is TlkJSONobject then
      Result := TRALJSONObjectBase.Create
    else if vObj is TlkJSONlist then
      Result := TRALJSONArrayBase.Create
    else
      Result := TRALJSONValueBase.Create;
    Result.SetJsonObj(vObj,False);
  end;
end;

function TRALJSONArrayBase.JsonObject: TlkJSONlist;
begin
  Result := TlkJSONlist(GetJsonObj);
end;

end.
