{ @abstract Partial unit used to map JSON with the lkJSON library
  This unit is defined under RALJson.pas unit.
  - lkJSON can be found at SouceForge: https://sourceforge.net/projects/lkjson
  - latest version used:
  Version: 1.07 of 06/11/2009

  This partial Unit is recomended to keep code compatibility with older versions of Delphi
  from Delphi 7 to Delphi 2009
}

uses
  ulkJSON, Variants, Classes, SysUtils, RALTypes;

type

  { TRALJSONValue }

  TRALJSONValue = class
  private
    FJsonObj: TlkJSON;
    FFreeObj: Boolean;
    FListObj: TList;
    FMarkFree: Boolean;
  protected
    procedure AddObj(AObj: TRALJSONValue);
    procedure ClearObjs;
    function GetJsonObj: TlkJSON;
    procedure SetJsonObj(AObj: TlkJSON; AFree: Boolean = True);
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
    function ToJson: StringRAL;
  end;

  { TRALJSONObject }

  TRALJSONObject = class(TRALJSONValue)
  protected
    function JsonObject: TlkJSONobject;
  public
    constructor Create; override;

    function Add(const AStr: StringRAL; const AValue: TRALJSONValue): TRALJSONObject; overload;
    function Add(const AStr: StringRAL; const AValue: StringRAL): TRALJSONObject; overload;
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
    function JsonObject: TlkJSONlist;
  public
    constructor Create; override;

    function Add(const AValue: TRALJSONValue): TRALJSONArray; overload;
    function Add(const AValue: StringRAL): TRALJSONArray; overload;
    function Add(const AValue: Int64RAL): TRALJSONArray; overload;
    function Add(const AValue: IntegerRAL): TRALJSONArray; overload;
    function Add(const AValue: DoubleRAL): TRALJSONArray; overload;
    function Add(const AValue: Boolean): TRALJSONArray; overload;
    function Add: TRALJSONArray; overload;

    function Get(AIndex: IntegerRAL): TRALJSONValue;

    function Count: IntegerRAL;
  end;

  { TRALJSON }

  TRALJSON = class
  public
    class function ParseJSON(AJson: StringRAL): TRALJSONValue;
    class function ParseJSON(AStream: TStream): TRALJSONValue;
  end;

implementation

{ TRALJSONValue }

procedure TRALJSONValue.SetJsonObj(AObj: TlkJSON; AFree: Boolean = True);
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);
  FJsonObj := AObj;
  FFreeObj := AFree;
end;

function TRALJSONValue.GetJsonObj: TlkJSON;
begin
  Result := FJsonObj;
end;

function TRALJSONValue.AsBoolean: Boolean;
begin
  Result := False;
  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := TlkJSONNumber(FJsonObj).Value = 1;
  end
  else if JsonType = rjtString then
  begin
    Result := (FJsonObj.Value = '1') or (FJsonObj.Value = 'S') or (FJsonObj.Value = 's')
      or (SameText(FJsonObj.Value, 'true'));
  end
  else if JsonType = rjtBoolean then
  begin
    Result := TlkJSONboolean(FJsonObj).Value;
  end;
end;

function TRALJSONValue.AsFloat: DoubleRAL;
begin
  Result := -1;
  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := Extended(TlkJSONNumber(FJsonObj).Value);
  end
  else if JsonType = rjtString then
  begin
    Result := StrToFloatDef(FJsonObj.Value, -1)
  end;
end;

function TRALJSONValue.AsInteger: Int64RAL;
begin
  Result := -1;
  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := Trunc(Extended(TlkJSONNumber(FJsonObj).Value));
  end
  else if JsonType = rjtString then
  begin
    Result := StrToInt64Def(FJsonObj.Value, -1)
  end
  else if JsonType = rjtBoolean then
  begin
    if Boolean(TlkJSONboolean(FJsonObj).Value) then
      Result := 1
    else
      Result := 0;
  end;
end;

function TRALJSONValue.AsString: StringRAL;
begin
  if (Self = nil) then
    Result := ''
  else if FJsonObj <> nil then
    Result := FJsonObj.Value
  else
    Result := 'null';
end;

function TRALJSONValue.IsNull: Boolean;
begin
  if (Self <> nil) and (FJsonObj <> nil) then
    Result := FJsonObj is TlkJSONNull
  else
    Result := True;
end;

function TRALJSONValue.JsonType: TRALJSONType;
begin
  Result := rjtString;
  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if FJsonObj is TlkJSONNumber then
    Result := rjtNumber
  else if FJsonObj is TlkJSONboolean then
    Result := rjtBoolean
  else if FJsonObj is TlkJSONobject then
    Result := rjtObject
  else if FJsonObj is TlkJSONlist then
    Result := rjtArray;
end;

function TRALJSONValue.AsValue: Variant;
begin
  if (Self <> nil) and (FJsonObj <> nil) then
    Result := FJsonObj.Value
  else
    Result := Variants.Null;
end;

function TRALJSONValue.ToJson: StringRAL;
begin
  if (Self <> nil) and (FJsonObj <> nil) then
    Result := TlkJSON.GenerateText(FJsonObj)
  else
    Result := '';
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

{ TRALJSONObject }

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: IntegerRAL)
  : TRALJSONObject;
begin
  JsonObject.Add(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: Int64RAL)
  : TRALJSONObject;
begin
  JsonObject.Add(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: StringRAL)
  : TRALJSONObject;
begin
  JsonObject.Add(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL): TRALJSONObject;
begin
  JsonObject.Add(AStr, TlkJSONNull.Create)
end;

function TRALJSONObject.Count: IntegerRAL;
begin
  if (Self <> nil) and (JsonObject <> nil) then
    Result := JsonObject.Count
  else
    Result := 0;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: TRALJSONValue)
  : TRALJSONObject;
begin
  if AValue <> nil then
  begin
    JsonObject.Add(AStr, AValue.GetJsonObj);
    AddObj(AValue);
  end
  else
  begin
    JsonObject.Add(AStr, TlkJSONNull.Create);
  end;
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: Boolean): TRALJSONObject;
begin
  JsonObject.Add(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: DoubleRAL)
  : TRALJSONObject;
begin
  JsonObject.Add(AStr, AValue);
  Result := Self;
end;

constructor TRALJSONObject.Create;
var
  vObj: TlkJSONobject;
begin
  inherited;
  vObj := TlkJSONobject.Create;
  SetJsonObj(vObj);
end;

function TRALJSONObject.Get(AName: StringRAL): TRALJSONValue;
var
  vObj: TlkJSON;
begin
  Result := nil;

  if Self = nil then
    Exit;

  vObj := JsonObject.Field[AName];
  if vObj <> nil then
  begin
    if vObj is TlkJSONobject then
      Result := TRALJSONObject.Create
    else if vObj is TlkJSONlist then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(vObj, False);
    AddObj(Result);
  end;
end;

function TRALJSONObject.Get(AIndex: IntegerRAL): TRALJSONValue;
var
  vObj: TlkJSON;
begin
  Result := nil;

  if Self = nil then
    Exit;

  vObj := JsonObject.FieldByIndex[AIndex];
  if vObj <> nil then
  begin
    if vObj is TlkJSONobject then
      Result := TRALJSONObject.Create
    else if vObj is TlkJSONlist then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(vObj, False);
    AddObj(Result);
  end;
end;

function TRALJSONObject.GetName(AIndex: IntegerRAL): StringRAL;
begin
  if Self = nil then
    Result := ''
  else
    Result := JsonObject.NameOf[AIndex];
end;

function TRALJSONObject.JsonObject: TlkJSONobject;
begin
  Result := TlkJSONobject(GetJsonObj);
end;

{ TRALJSON }

class function TRALJSON.ParseJSON(AJson: StringRAL): TRALJSONValue;
var
  vObj: TlkJSONbase;
begin
  Result := nil;

  vObj := TlkJSON.ParseText(AJson);

  if vObj <> nil then
  begin
    if vObj is TlkJSONobject then
      Result := TRALJSONObject.Create
    else if vObj is TlkJSONlist then
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
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: StringRAL): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: TRALJSONValue): TRALJSONArray;
begin
  JsonObject.Add(AValue.GetJsonObj);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: IntegerRAL): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add: TRALJSONArray;
begin
  JsonObject.Add(TlkJSONNull.Create);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: Boolean): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: DoubleRAL): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Count: IntegerRAL;
begin
  if (Self <> nil) and (JsonObject <> nil) then
    Result := JsonObject.Count
  else
    Result := 0;
end;

constructor TRALJSONArray.Create;
var
  vObj: TlkJSONlist;
begin
  inherited;
  vObj := TlkJSONlist.Create;
  SetJsonObj(vObj);
end;

function TRALJSONArray.Get(AIndex: IntegerRAL): TRALJSONValue;
var
  vObj: TlkJSON;
begin
  Result := nil;

  if Self = nil then
    Exit;

  vObj := JsonObject.Child[AIndex];
  if vObj <> nil then
  begin
    if vObj is TlkJSONobject then
      Result := TRALJSONObject.Create
    else if vObj is TlkJSONlist then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(vObj, False);
    AddObj(Result);
  end;
end;

function TRALJSONArray.JsonObject: TlkJSONlist;
begin
  Result := TlkJSONlist(GetJsonObj);
end;
