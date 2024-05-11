{ @abstract Partial unit used to map JSON with the uJSON library
  This unit is defined under RALJson.pas unit.
  - uJSON can be found at SouceForge: https://sourceforge.net/projects/is-webstart/
  - latest version used:
    Version: 1.06 of 25/03/2010

  This partial Unit is recomended to keep code compatibility with older versions of Delphi
  from Delphi 7 to Delphi 2009
}

uses
  uJSON, Variants, Classes, SysUtils, RALTypes;

type

  { TRALJSONValue }

  TRALJSONValue = class
  private
    FJsonObj: TZAbstractObject;
    FFreeObj: Boolean;
    FListObj: TList;
    FMarkFree: Boolean;
  protected
    procedure SetJsonObj(AObj: TZAbstractObject; AFree: Boolean = True);
    function GetJsonObj: TZAbstractObject;

    procedure AddObj(AObj: TRALJSONValue);
    procedure ClearObjs;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function JsonType: TRALJSONType;
    function IsNull: Boolean;

    function AsValue: variant;
    function AsString: StringRAL;
    function AsInteger: Int64RAL;
    function AsFloat: DoubleRAL;
    function AsBoolean: Boolean;

    function ToJson: StringRAL;
  end;

  { TRALJSONObject }

  TRALJSONObject = class(TRALJSONValue)
  protected
    function JsonObject: TJSONObject;
  public
    constructor Create; override;

    function Add(const AStr: StringRAL; const AValue: TRALJSONValue): TRALJSONObject; overload;
    function Add(const AStr: StringRAL; const AValue: StringRAL): TRALJSONObject;overload;
    function Add(const AStr: StringRAL; const AValue: Int64RAL): TRALJSONObject; overload;
    function Add(const AStr: StringRAL; const AValue: IntegerRAL): TRALJSONObject; overload;
    function Add(const AStr: StringRAL; const AValue: DoubleRAL): TRALJSONObject; overload;
    function Add(const AStr: StringRAL; const AValue: Boolean): TRALJSONObject; overload;
    function Add(const AStr: StringRAL): TRALJSONObject; overload;

    function Count: IntegerRAL;

    function Get(AName: StringRAL): TRALJSONValue; overload;
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
    class function ParseJSON(AJson: StringRAL): TRALJSONValue;
    class function ParseJSON(AStream: TStream): TRALJSONValue;
  end;

implementation

{ TRALJSON }

class function TRALJSON.ParseJSON(AJson: StringRAL): TRALJSONValue;
var
  vTok: JSONTokener;
  vObj: TZAbstractObject;
begin
  Result := nil;

  vTok := JSONTokener.Create(AJson);
  try
    if vTok <> nil then
    begin
      vObj := vTok.nextValue;
      if vObj <> nil then
      begin
        if vObj is TJSONObject then
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

function TRALJSONArray.JsonObject: TJSONArray;
begin
  Result := TJSONArray(GetJsonObj);
end;

constructor TRALJSONArray.Create;
var
  vObj: TJSONArray;
begin
  inherited;
  vObj := TJSONArray.Create;
  SetJsonObj(vObj);
end;

function TRALJSONArray.Add(const AValue: TRALJSONValue): TRALJSONArray;
begin
  if AValue <> nil then
  begin
    JsonObject.put(AValue.GetJsonObj);
    AddObj(AValue);
  end
  else
  begin
    JsonObject.put(uJSON.NULL.Create);
  end;
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: StringRAL): TRALJSONArray;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: Int64RAL): TRALJSONArray;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: IntegerRAL): TRALJSONArray;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: DoubleRAL): TRALJSONArray;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: Boolean): TRALJSONArray;
begin
  JsonObject.put(AValue);
  Result := Self;
end;

function TRALJSONArray.Add: TRALJSONArray;
begin
  JsonObject.put(uJSON.NULL.Create);
  Result := Self;
end;

function TRALJSONArray.Get(AIndex: IntegerRAL): TRALJSONValue;
var
  vObj: TZAbstractObject;
begin
  Result := nil;

  if Self = nil then
    Exit;

  vObj := JsonObject.opt(AIndex);
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

function TRALJSONArray.Count: IntegerRAL;
begin
  if (Self <> nil) and (JsonObject <> nil) then
    Result := JsonObject.length
  else
    Result := 0;
end;

{ TRALJSONObject }

function TRALJSONObject.JsonObject: TJSONObject;
begin
  Result := TJSONObject(GetJsonObj);
end;

constructor TRALJSONObject.Create;
var
  vObj: TJSONObject;
begin
  inherited;
  vObj := TJSONObject.Create;
  SetJsonObj(vObj);
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: TRALJSONValue)
  : TRALJSONObject;
begin
  if AValue <> nil then
  begin
    JsonObject.put(AStr, AValue.GetJsonObj);
    AddObj(AValue);
  end
  else
  begin
    JsonObject.put(AStr, uJSON.NULL.Create);
  end;
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: StringRAL)
  : TRALJSONObject;
begin
  JsonObject.put(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: Int64RAL)
  : TRALJSONObject;
begin
  JsonObject.put(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: IntegerRAL)
  : TRALJSONObject;
begin
  JsonObject.put(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: DoubleRAL)
  : TRALJSONObject;
begin
  JsonObject.put(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: Boolean): TRALJSONObject;
begin
  JsonObject.put(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL): TRALJSONObject;
begin
  JsonObject.put(AStr, uJSON.NULL.Create);
  Result := Self;
end;

function TRALJSONObject.Count: IntegerRAL;
begin
  if (Self <> nil) and (JsonObject <> nil) then
    Result := JsonObject.length
  else
    Result := 0;
end;

function TRALJSONObject.Get(AName: StringRAL): TRALJSONValue;
var
  vObj: TZAbstractObject;
begin
  Result := nil;

  if Self = nil then
    Exit;

  vObj := JsonObject.opt(AName);
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

function TRALJSONObject.Get(AIndex: IntegerRAL): TRALJSONValue;
var
  vObj: TZAbstractObject;
  vName: StringRAL;
begin
  Result := nil;

  if Self = nil then
    Exit;

  AName := GetName(AIndex);
  if vName <> '' then
  begin
    vObj := JsonObject.opt(vName);
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
end;

function TRALJSONObject.GetName(AIndex: IntegerRAL): StringRAL;
var
  vKey: TStringList;
begin
  Result := '';

  if Self = nil then
    Exit;

  vKey := JsonObject.keys;
  try
    if (AIndex >= 0) and (AIndex < vKey.Count) then
      Result := vKey.Strings[AIndex];
  finally
    FreeAndNil(vKey);
  end;
end;

{ TRALJSONValue }

procedure TRALJSONValue.SetJsonObj(AObj: TZAbstractObject; AFree: Boolean);
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);

  FJsonObj := AObj;
  FFreeObj := AFree;
end;

function TRALJSONValue.GetJsonObj: TZAbstractObject;
begin
  Result := FJsonObj;
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
    FreeAndNil(FJsonObj);
  ClearObjs;
  FListObj.Free;
  inherited;
end;

function TRALJSONValue.JsonType: TRALJSONType;
begin
  Result := rjtString;
  if (Self = nil) or (FJsonObj = nil) then
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

function TRALJSONValue.IsNull: Boolean;
begin
  if (Self <> nil) and (FJsonObj <> nil) then
    Result := FJsonObj is uJSON.NULL
  else
    Result := True;
end;

function TRALJSONValue.AsValue: variant;
begin
  if (Self <> nil) and (FJsonObj <> nil) then
    Result := FJsonObj.toString
  else
    Result := Variants.NULL;
end;

function TRALJSONValue.AsString: StringRAL;
begin
  Result := '';
  if FJsonObj <> nil then
    Result := FJsonObj.toString
end;

function TRALJSONValue.AsInteger: Int64RAL;
begin
  Result := -1;
  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := _Number(FJsonObj).intValue;
  end
  else if JsonType = rjtString then
  begin
    Result := StrToInt64Def(FJsonObj.toString, -1)
  end
  else if JsonType = rjtBoolean then
  begin
    if _Boolean(FJsonObj).toString = 'true' then
      Result := 1
    else
      Result := 0;
  end;
end;

function TRALJSONValue.AsFloat: DoubleRAL;
begin
  Result := -1;
  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := _Number(FJsonObj).doubleValue;
  end
  else if JsonType = rjtString then
  begin
    Result := StrToFloatDef(FJsonObj.toString, -1)
  end;
end;

function TRALJSONValue.AsBoolean: Boolean;
begin
  Result := False;
  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := _Number(FJsonObj).doubleValue;
  end
  else if JsonType = rjtString then
  begin
    Result := (FJsonObj.toString = '1') or (FJsonObj.toString = 'S') or
      (FJsonObj.toString = 's') or (SameText(FJsonObj.toString, 'true'));
  end
  else if JsonType = rjtBoolean then
  begin
    Result := FJsonObj.toString = 'true';
  end;
end;

function TRALJSONValue.ToJson: StringRAL;
begin
  if (Self <> nil) and (FJsonObj <> nil) then
    Result := FJsonObj.toString
  else
    Result := '';
end;
