unit RALJson;

interface

uses
  Classes, SysUtils,
  {$IFNDEF FPC}
    System.JSON
  {$ELSE}
    fpjson
  {$ENDIF};

type
  TRALJSONValue = {$IFDEF FPC}TJSONData{$ELSE}TJSONValue{$ENDIF};
  TRALJSONNull = TJSONNull;
  TRALJSONString = TJSONString;
  TRALJSONBoolean = {$IFDEF FPC}TJSONBoolean{$ELSE}TJSONBool{$ENDIF};
  TRALJSONNumber = TJSONNumber;
  TRALJSONObject = TJSONObject;
  TRALJSONArray = TJSONArray;

  { THelpRALJSONValue }

  THelpRALJSONValue = class helper for TRALJSONValue
  public
    function AsInt64 : int64;
    function AsString : string;
    function AsFloat : double;
    function AsBoolean : boolean;
    {$IFDEF FPC}
      function ToJSON : string;
    {$ENDIF}
  end;

  { THelpRALJSONObject }

  THelpRALJSONObject = class helper for TRALJSONObject
  public
    function Add(const AStr: TRALJSONString; const AValue: TRALJSONValue): TRALJSONObject; overload;
    function Add(const AStr: string; const AValue: TRALJSONValue): TRALJSONObject; overload;
    function Add(const AStr: string; const AValue: string): TRALJSONObject; overload;
    function Add(const AStr: string; const AValue: int64): TRALJSONObject; overload;
    function Add(const AStr: string; const AValue: integer): TRALJSONObject; overload;
    function Add(const AStr: string; const AValue: double): TRALJSONObject; overload;
    function Add(const AStr: string; const AValue: boolean): TRALJSONObject; overload;
    function Add(const AStr: string): TRALJSONObject; overload;

    function Get(AStr : string) : TRALJSONValue; overload;
    function Get(AIndex : integer) : TRALJSONValue; overload;

    {$IFNDEF FPC}
      function GetNames(Index : integer) : string;
      property Names[Index : Integer] : string read GetNames;
    {$ENDIF}
  end;

  function ParseJson(AStr : string) : TRALJSONValue;

implementation

function ParseJson(AStr : string) : TRALJSONValue;
begin
  {$IFDEF FPC}
    Result := GetJSON(AStr);
  {$ELSE}
    Result := TJSONObject.ParseJSONValue(AStr);
  {$ENDIF}
end;

{ THelpRALJSONValue }

{$IFDEF FPC}
  function THelpRALJSONValue.ToJSON : string;
  begin
    Result := Self.AsJSON;
  end;
{$ENDIF}

function THelpRALJSONValue.AsBoolean: boolean;
begin
  {$IFDEF FPC}
    Result := inherited AsBoolean;
  {$ELSE}
    Result := Self.GetValue<Boolean>;
  {$ENDIF}
end;

function THelpRALJSONValue.AsFloat: double;
begin
  {$IFDEF FPC}
    Result := inherited AsFloat;
  {$ELSE}
    Result := Self.GetValue<Double>;
  {$ENDIF}
end;

function THelpRALJSONValue.AsInt64: int64;
begin
  {$IFDEF FPC}
    Result := inherited AsInt64;
  {$ELSE}
    Result := Self.GetValue<Int64>;
  {$ENDIF}
end;

function THelpRALJSONValue.AsString: string;
begin
  {$IFDEF FPC}
    Result := inherited AsString;
  {$ELSE}
    Result := Self.GetValue<string>;
  {$ENDIF}
end;

{ THelpRALJSONObject }

function THelpRALJSONObject.Add(const AStr : string; const AValue : string) : TRALJSONObject;
begin
  {$IFDEF FPC}
    inherited Add(AStr,AValue);
    Result := Self;
  {$ELSE}
    Result := AddPair(AStr,AValue);
  {$ENDIF}
end;

function THelpRALJSONObject.Add(const AStr: string;
  const AValue: TRALJSONValue): TRALJSONObject;
begin
  {$IFDEF FPC}
    inherited Add(AStr,AValue);
    Result := Self;
  {$ELSE}
    Result := AddPair(AStr,AValue);
  {$ENDIF}
end;

function THelpRALJSONObject.Add(const AStr: TRALJSONString;
  const AValue: TRALJSONValue): TRALJSONObject;
begin
  {$IFDEF FPC}
    inherited Add(AStr.AsString,AValue);
    Result := Self;
  {$ELSE}
    Result := AddPair(AStr,AValue);
  {$ENDIF}
end;

function THelpRALJSONObject.Add(const AStr: string;
  const AValue: int64): TRALJSONObject;
begin
  {$IFDEF FPC}
    inherited Add(AStr,AValue);
    Result := Self;
  {$ELSE}
    Result := AddPair(AStr,TRALJSONNumber.Create(AValue));
  {$ENDIF}
end;

function THelpRALJSONObject.Add(const AStr: string;
  const AValue: boolean): TRALJSONObject;
begin
  {$IFDEF FPC}
    inherited Add(AStr,AValue);
    Result := Self;
  {$ELSE}
    Result := AddPair(AStr,TRALJSONBoolean.Create(AValue));
  {$ENDIF}
end;

function THelpRALJSONObject.Add(const AStr : string) : TRALJSONObject;
begin
  {$IFDEF FPC}
    inherited Add(AStr);
    Result := Self;
  {$ELSE}
    Result := AddPair(AStr,TJSONNull.Create);
  {$ENDIF}
end;

function THelpRALJSONObject.Get(AStr : string) : TRALJSONValue;
begin
  {$IFDEF FPC}
    Result := Find(AStr);
  {$ELSE}
    Result := Values[AStr];
  {$ENDIF}
end;

function THelpRALJSONObject.Get(AIndex : integer) : TRALJSONValue;
{$IFNDEF FPC}
  var
    vPair : TJSONPair;
{$ENDIF}
begin
  {$IFDEF FPC}
    Result := Items[AIndex];
  {$ELSE}
    vPair := Pairs[AIndex];
    Result := nil;
    if vPair <> nil then
      Result := vPair.JsonValue;
  {$ENDIF}
end;

{$IFNDEF FPC}
  function THelpRALJSONObject.GetNames(Index: integer): string;
  var
    vPair : TJSONPair;
  begin
    Result := '';
    vPair := Pairs[Index];
    if vPair <> nil then
      Result := vPair.JsonString.Value;
  end;
{$ENDIF}

function THelpRALJSONObject.Add(const AStr: string;
  const AValue: double): TRALJSONObject;
begin
  {$IFDEF FPC}
    inherited Add(AStr,AValue);
    Result := Self;
  {$ELSE}
    Result := AddPair(AStr,TRALJSONNumber.Create(AValue));
  {$ENDIF}
end;

function THelpRALJSONObject.Add(const AStr: string;
  const AValue: integer): TRALJSONObject;
begin
  {$IFDEF FPC}
    inherited Add(AStr,AValue);
    Result := Self;
  {$ELSE}
    Result := AddPair(AStr,TRALJSONNumber.Create(AValue));
  {$ENDIF}
end;

end.
