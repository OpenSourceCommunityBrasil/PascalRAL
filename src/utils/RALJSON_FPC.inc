{ @abstract Partial unit used to map JSON with the fpJSON library
  This unit is defined under RALJson.pas unit to be used with Lazarus/FPC
}

uses
  fpjson, Variants, Classes, SysUtils, RALTypes;

type

  { TRALJSONValue }

  TRALJSONValue = class
  private
    FJsonObj: TJSONData;
    FFreeObj: Boolean;
    FListObj: TList;
    FMarkFree: Boolean;
  protected
    procedure AddObj(AObj: TRALJSONValue);
    procedure ClearObjs;
    function GetJsonObj: TJSONData;
    procedure SetJsonObj(AObj: TJSONData; AFree: Boolean = True);
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

  { TRALJSONFPC }

  TRALJSON = class
  public
    class function ParseJSON(AJson: StringRAL): TRALJSONValue;
    class function ParseJSON(AStream: TStream): TRALJSONValue;
  end;

implementation

{ FPC 3.2.2's jsonscanner decodes two \u escapes in a row into a String[4]:
  two ideographs, six bytes of UTF-8, came out cut to four, and so did an
  accented letter next to one - a column of Chinese text arrived as garbage.
  RAL writes every character past ASCII that way (StringToJSONString), and so
  does Delphi's JSON. They are decoded here, into the UTF-8 fpjson reads
  whole, a surrogate pair into its one character. What a string cannot hold
  raw - a control character, the quote, the backslash - goes through as it
  came, and so does anything that is not the escape of a character: a lone
  surrogate, or the "u" after an escaped backslash. Decoding only shrinks the
  text, six bytes or twelve into four at most }
function DecodeUnicodeEscapes(const AJson: StringRAL): StringRAL;
var
  vIn, vLen, vOut: IntegerRAL;
  vCode, vLow: Cardinal;

  function HexAt(APos: IntegerRAL; out AValue: Cardinal): Boolean;
  var
    vInt: IntegerRAL;
  begin
    AValue := 0;
    Result := APos + 3 <= vLen;
    if Result then
      for vInt := APos to APos + 3 do
        case AJson[vInt] of
          '0'..'9': AValue := AValue * 16 + Cardinal(Ord(AJson[vInt]) - Ord('0'));
          'a'..'f': AValue := AValue * 16 + Cardinal(Ord(AJson[vInt]) - Ord('a') + 10);
          'A'..'F': AValue := AValue * 16 + Cardinal(Ord(AJson[vInt]) - Ord('A') + 10);
        else
          Result := False;
        end;
  end;

  procedure Put(AByte: Cardinal);
  begin
    Inc(vOut);
    Result[vOut] := AnsiChar(AByte);
  end;

begin
  Result := AJson;
  if Pos('\u', AJson) = 0 then
    Exit;

  vLen := Length(AJson);
  SetLength(Result, vLen);
  vOut := 0;
  vIn := 1;
  while vIn <= vLen do
  begin
    if (AJson[vIn] = '\') and (vIn < vLen) then
    begin
      if (AJson[vIn + 1] = 'u') and HexAt(vIn + 2, vCode) and (vCode >= $80) then
      begin
        if (vCode >= $D800) and (vCode <= $DBFF) then
        begin
          if (vIn + 11 <= vLen) and (AJson[vIn + 6] = '\') and (AJson[vIn + 7] = 'u') and
             HexAt(vIn + 8, vLow) and (vLow >= $DC00) and (vLow <= $DFFF) then
          begin
            vCode := $10000 + ((vCode - $D800) shl 10) + (vLow - $DC00);
            Put($F0 or (vCode shr 18));
            Put($80 or ((vCode shr 12) and $3F));
            Put($80 or ((vCode shr 6) and $3F));
            Put($80 or (vCode and $3F));
            Inc(vIn, 12);
            Continue;
          end;
        end
        else if (vCode < $DC00) or (vCode > $DFFF) then
        begin
          if vCode < $800 then
          begin
            Put($C0 or (vCode shr 6));
            Put($80 or (vCode and $3F));
          end
          else
          begin
            Put($E0 or (vCode shr 12));
            Put($80 or ((vCode shr 6) and $3F));
            Put($80 or (vCode and $3F));
          end;
          Inc(vIn, 6);
          Continue;
        end;
      end;
      // both characters of any other escape, so an escaped backslash never
      // lends its second half to what follows
      Put(Ord(AJson[vIn]));
      Put(Ord(AJson[vIn + 1]));
      Inc(vIn, 2);
      Continue;
    end;
    Put(Ord(AJson[vIn]));
    Inc(vIn);
  end;
  SetLength(Result, vOut);
end;

{ TRALJSON }

class function TRALJSON.ParseJSON(AJson: StringRAL): TRALJSONValue;
var
  jObj: TJSONData;
begin
  Result := nil;

  jObj := GetJSON(DecodeUnicodeEscapes(AJson));

  if jObj <> nil then
  begin
    if jObj is TJSONObject then
      Result := TRALJSONObject.Create
    else if jObj is TJSONArray then
      Result := TRALJSONArray.Create
    else
      Result := TRALJSONValue.Create;
    Result.SetJsonObj(jObj);
  end;
end;

class function TRALJSON.ParseJSON(AStream: TStream): TRALJSONValue;
var
  vJson: StringRAL;
begin
  Result := nil;
  if not Assigned(AStream) then
    Exit;
  { read here, the whole stream from its start as the Delphi backend does,
    so that the text goes through DecodeUnicodeEscapes too }
  AStream.Position := 0;
  SetLength(vJson, AStream.Size);
  if vJson <> '' then
    AStream.ReadBuffer(vJson[1], Length(vJson));
  Result := ParseJSON(vJson);
end;

{ TRALJSONArray }

function TRALJSONArray.JsonObject: TJSONArray;
begin
  Result := TJSONArray(GetJsonObj);
end;

constructor TRALJSONArray.Create;
var
  jObj: TJSONArray;
begin
  inherited;
  jObj := TJSONArray.Create;
  SetJsonObj(jObj);
end;

function TRALJSONArray.Add(const AValue: TRALJSONValue): TRALJSONArray;
begin
  if AValue <> nil then
  begin
    JsonObject.Add(AValue.GetJsonObj);
    AddObj(AValue);
  end
  else
  begin
    JsonObject.Add(TJSONNull.Create);
  end;
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: StringRAL): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: Int64RAL): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: IntegerRAL): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: DoubleRAL): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add(const AValue: Boolean): TRALJSONArray;
begin
  JsonObject.Add(AValue);
  Result := Self;
end;

function TRALJSONArray.Add: TRALJSONArray;
begin
  JsonObject.Add(TJSONNull.Create);
  Result := Self;
end;

function TRALJSONArray.Get(AIndex: IntegerRAL): TRALJSONValue;
var
  vObj: TJSONData;
begin
  Result := nil;

  if Self = nil then
    Exit;

  vObj := JsonObject.Items[AIndex];
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
    Result := JsonObject.Count
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
  jObj: TJSONObject;
begin
  inherited;
  jObj := TJSONObject.Create;
  SetJsonObj(jObj);
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
    JsonObject.Add(AStr, TJSONNull.Create);
  end;
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: StringRAL): TRALJSONObject;
begin
  JsonObject.Add(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: Int64RAL): TRALJSONObject;
begin
  JsonObject.Add(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: IntegerRAL): TRALJSONObject;
begin
  JsonObject.Add(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: DoubleRAL): TRALJSONObject;
begin
  JsonObject.Add(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL; const AValue: Boolean): TRALJSONObject;
begin
  JsonObject.Add(AStr, AValue);
  Result := Self;
end;

function TRALJSONObject.Add(const AStr: StringRAL): TRALJSONObject;
begin
  JsonObject.Add(AStr, TJSONNull.Create);
  Result := Self;
end;

function TRALJSONObject.Count: IntegerRAL;
begin
  if (Self <> nil) and (JsonObject <> nil) then
    Result := JsonObject.Count
  else
    Result := 0;
end;

function TRALJSONObject.Get(AName: StringRAL): TRALJSONValue;
var
  vObj: TJSONData;
begin
  Result := nil;

  if Self = nil then
    Exit;

  vObj := JsonObject.Find(AName);
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
  vObj: TJSONData;
begin
  Result := nil;

  if Self = nil then
    Exit;

  vObj := JsonObject.Items[AIndex];
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

function TRALJSONObject.GetName(AIndex: IntegerRAL): StringRAL;
begin
  if (Self <> nil) and (JsonObject <> nil) then
    Result := JsonObject.Names[AIndex]
  else
    Result := '';
end;

{ TRALJSONValue }

procedure TRALJSONValue.SetJsonObj(AObj: TJSONData; AFree: Boolean);
begin
  if (FJsonObj <> nil) and (FFreeObj) then
    FreeAndNil(FJsonObj);
  FJsonObj := AObj;
  FFreeObj := AFree;
end;

function TRALJSONValue.GetJsonObj: TJSONData;
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

  if FJsonObj is TJSONNumber then
    Result := rjtNumber
  else if FJsonObj is TJSONBoolean then
    Result := rjtBoolean
  else if FJsonObj is TJSONObject then
    Result := rjtObject
  else if FJsonObj is TJSONArray then
    Result := rjtArray;
end;

function TRALJSONValue.IsNull: Boolean;
begin
  if (Self <> nil) and (FJsonObj <> nil) then
    Result := FJsonObj is TJSONNull
  else
    Result := True;
end;

function TRALJSONValue.ToJson: StringRAL;
begin
  if (Self <> nil) and (FJsonObj <> nil) then
    Result := FJsonObj.AsJSON
  else
    Result := '';
end;

function TRALJSONValue.AsValue: Variant;
begin
  if (Self <> nil) and (FJsonObj <> nil) then
    Result := FJsonObj.Value
  else
    Result := Variants.Null;
end;

function TRALJSONValue.AsString: StringRAL;
begin
  Result := '';

  if (Self = nil) then
    Exit;

  if (FJsonObj <> nil) and (not FJsonObj.IsNull) then
    Result := FJsonObj.AsString;
end;

function TRALJSONValue.AsInteger: Int64RAL;
begin
  Result := -1;

  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := TJSONNumber(FJsonObj).AsInt64;
  end
  else if JsonType = rjtString then
  begin
    Result := StrToInt64Def(FJsonObj.Value, -1)
  end
  else if JsonType = rjtBoolean then
  begin
    if TJSONBoolean(FJsonObj).AsBoolean then
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
    Result := TJSONNumber(FJsonObj).AsFloat;
  end
  else if JsonType = rjtString then
  begin
    Result := StrToFloatDef(FJsonObj.Value, -1)
  end;
end;

function TRALJSONValue.AsBoolean: Boolean;
begin
  Result := False;
  if (Self = nil) or (FJsonObj = nil) then
    Exit;

  if JsonType = rjtNumber then
  begin
    Result := TJSONNumber(FJsonObj).AsFloat = 1;
  end
  else if JsonType = rjtString then
  begin
    Result := (FJsonObj.AsString = '1') or (FJsonObj.AsString = 'S') or
      (FJsonObj.AsString = 's') or (SameText(FJsonObj.AsString, 'true'));
  end
  else if JsonType = rjtBoolean then
  begin
    Result := TJSONBoolean(FJsonObj).AsBoolean;
  end;
end;
