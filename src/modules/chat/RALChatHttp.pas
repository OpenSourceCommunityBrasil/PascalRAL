{ What the platform channels share to talk HTTP: a client (the one linked to the
  channel or one made from an engine name), one request at a time on it, the
  Retry-After of the answer, a download capped in size, and the small JSON, URL and
  hex helpers every platform's answers and bodies need. }
unit RALChatHttp;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALJson, RALClient, RALRequest, RALResponse, RALParams,
  RALChatMessage;

type
  /// What one HTTP call came to.
  TRALChatHttpReply = record
    /// True when no answer came (connection, timeout).
    Failed: Boolean;
    /// The HTTP status; 0 when Failed.
    Status: Integer;
    /// The body of the answer.
    Text: StringRAL;
    /// The Retry-After header in milliseconds; 0 when there is none.
    RetryAfterMs: Int64RAL;
    /// Why no answer came.
    Failure: StringRAL;
  end;

  { Runs the requests of a channel one at a time on a client: the linked one, or one
    of its own made from EngineType on first use. }
  TRALChatHttp = class
  private
    /// Lets one request at a time use the client.
    FCallLock: TCriticalSection;
    FChannelName: StringRAL;
    FClient: TRALClient;
    FEngineType: string;
    /// Guards the lazy and shared state.
    FLock: TCriticalSection;
    /// The channel's own client, made on first use.
    FOwnClient: TRALClient;

    /// The client in use: the linked one or the channel's own.
    function Acquire: TRALClient;
  public
    constructor Create;
    destructor Destroy; override;

    { Sends one request and says how it came out; never raises. AHeaders are Name=Value
      lines; with AFileField the fields of AFields and the stream of AMedia go as a
      multipart, otherwise ABody goes as it is with AContentType. }
    procedure Call(AMethod: TRALMethod; const AUrl: StringRAL; AHeaders: TStrings;
      const ABody, AContentType: StringRAL; AFields: TRALJSONObject;
      const AFileField: StringRAL; AMedia: TRALChatMedia; out AReply: TRALChatHttpReply);
    { Gets AUrl and returns a copy of the body, which the caller frees; raises when the
      answer is not 200 or the body is above AMaxSize. }
    function Download(const AUrl: StringRAL; AHeaders: TStrings; AMaxSize: Int64RAL): TStream;
    { A client of its own for a thread of the channel (a long poll), like the linked one;
      the caller frees it. }
    function NewClient: TRALClient;

    /// The name of the channel, for the error texts.
    property ChannelName: StringRAL read FChannelName write FChannelName;
    /// The linked client; nil to use the channel's own.
    property Client: TRALClient read FClient write FClient;
    /// The engine of the channel's own client; empty = the first registered.
    property EngineType: string read FEngineType write FEngineType;
  end;

/// The member AName of AParent as an object, or nil.
function RALChatJObj(AParent: TRALJSON; const AName: StringRAL): TRALJSONObject;
/// The member AName of AParent as an array, or nil.
function RALChatJArr(AParent: TRALJSON; const AName: StringRAL): TRALJSONArray;
/// A scalar value as text.
function RALChatJText(AValue: TRALJSON): StringRAL;
/// The member AName of AParent as text; empty when missing.
function RALChatJStr(AParent: TRALJSON; const AName: StringRAL): StringRAL;
/// The member AName of AParent as an integer; ADefault when missing.
function RALChatJInt(AParent: TRALJSON; const AName: StringRAL; ADefault: Int64): Int64;
/// Parses AText; nil when it is not JSON.
function RALChatTryParse(const AText: StringRAL): TRALJSON;
/// A JSON object of AText (a JSON object), or nil.
function RALChatParseObject(const AText: StringRAL): TRALJSONObject;
/// Percent-encodes AText (UTF-8 bytes) for a URL or a form.
function RALChatUrlEncode(const AText: StringRAL): StringRAL;
/// Decodes a percent-encoded form value (plus is a space).
function RALChatUrlDecode(const AText: StringRAL): StringRAL;
/// The bytes of a hexadecimal text; empty when it is not hexadecimal of even length.
function RALChatHexToBytes(const AText: StringRAL): TBytes;
/// The text of AMs milliseconds of a Retry-After in seconds (a number, maybe fractional).
function RALChatSecondsToMs(const AText: StringRAL): Int64RAL;

implementation

{ ---- helpers ---- }

function RALChatJObj(AParent: TRALJSON; const AName: StringRAL): TRALJSONObject;
var
  vItem: TRALJSON;
begin
  Result := nil;
  if (AParent <> nil) and (AParent.Kind = rjkObject) then
  begin
    vItem := TRALJSONObject(AParent).Find(AName);
    if (vItem <> nil) and (vItem.Kind = rjkObject) then
      Result := TRALJSONObject(vItem);
  end;
end;

function RALChatJArr(AParent: TRALJSON; const AName: StringRAL): TRALJSONArray;
var
  vItem: TRALJSON;
begin
  Result := nil;
  if (AParent <> nil) and (AParent.Kind = rjkObject) then
  begin
    vItem := TRALJSONObject(AParent).Find(AName);
    if (vItem <> nil) and (vItem.Kind = rjkArray) then
      Result := TRALJSONArray(vItem);
  end;
end;

function RALChatJText(AValue: TRALJSON): StringRAL;
begin
  Result := '';
  if AValue = nil then
    Exit;
  case AValue.Kind of
    rjkString:
      Result := AValue.AsString;
    rjkNumber:
      Result := TRALJSONNumber(AValue).Value;
    rjkBoolean:
      if AValue.AsBoolean then
        Result := 'true'
      else
        Result := 'false';
  end;
end;

function RALChatJStr(AParent: TRALJSON; const AName: StringRAL): StringRAL;
begin
  Result := '';
  if (AParent <> nil) and (AParent.Kind = rjkObject) then
    Result := RALChatJText(TRALJSONObject(AParent).Find(AName));
end;

function RALChatJInt(AParent: TRALJSON; const AName: StringRAL; ADefault: Int64): Int64;
begin
  Result := StrToInt64Def(string(RALChatJStr(AParent, AName)), ADefault);
end;

function RALChatTryParse(const AText: StringRAL): TRALJSON;
begin
  try
    Result := TRALJSON.ParseJSON(AText);
  except
    Result := nil;
  end;
end;

function RALChatParseObject(const AText: StringRAL): TRALJSONObject;
var
  vRoot: TRALJSON;
begin
  Result := nil;
  vRoot := RALChatTryParse(AText);
  if vRoot = nil then
    Exit;
  if vRoot.Kind = rjkObject then
    Result := TRALJSONObject(vRoot)
  else
    vRoot.Free;
end;

function RALChatUrlEncode(const AText: StringRAL): StringRAL;
const
  cHex: array[0..15] of AnsiChar = '0123456789ABCDEF';
var
  vIdx, vOut: Integer;
  vByte: Byte;
begin
  SetLength(Result, Length(AText) * 3);
  vOut := 0;
  for vIdx := 1 to Length(AText) do
  begin
    vByte := Byte(AText[vIdx]);
    if ((vByte >= Ord('0')) and (vByte <= Ord('9'))) or ((vByte >= Ord('A')) and (vByte <= Ord('Z'))) or
       ((vByte >= Ord('a')) and (vByte <= Ord('z'))) or (vByte = Ord('-')) or (vByte = Ord('_')) or
       (vByte = Ord('.')) or (vByte = Ord('~')) then
    begin
      Inc(vOut);
      Result[vOut] := AnsiChar(vByte);
    end
    else
    begin
      Inc(vOut);
      Result[vOut] := '%';
      Inc(vOut);
      Result[vOut] := cHex[vByte shr 4];
      Inc(vOut);
      Result[vOut] := cHex[vByte and 15];
    end;
  end;
  SetLength(Result, vOut);
end;

/// The value of a hexadecimal digit, or -1.
function HexVal(AChar: AnsiChar): Integer;
begin
  case AChar of
    '0'..'9': Result := Ord(AChar) - Ord('0');
    'a'..'f': Result := Ord(AChar) - Ord('a') + 10;
    'A'..'F': Result := Ord(AChar) - Ord('A') + 10;
  else
    Result := -1;
  end;
end;

function RALChatUrlDecode(const AText: StringRAL): StringRAL;
var
  vIdx, vOut: Integer;
  vHigh, vLow: Integer;
begin
  SetLength(Result, Length(AText));
  vOut := 0;
  vIdx := 1;
  while vIdx <= Length(AText) do
  begin
    Inc(vOut);
    if AText[vIdx] = '+' then
      Result[vOut] := ' '
    else if (AText[vIdx] = '%') and (vIdx + 2 <= Length(AText)) then
    begin
      vHigh := HexVal(AText[vIdx + 1]);
      vLow := HexVal(AText[vIdx + 2]);
      if (vHigh >= 0) and (vLow >= 0) then
      begin
        Result[vOut] := AnsiChar(vHigh * 16 + vLow);
        Inc(vIdx, 2);
      end
      else
        Result[vOut] := '%';
    end
    else
      Result[vOut] := AText[vIdx];
    Inc(vIdx);
  end;
  SetLength(Result, vOut);
end;

function RALChatHexToBytes(const AText: StringRAL): TBytes;
var
  vIdx, vHigh, vLow: Integer;
begin
  Result := nil;
  if (AText = '') or Odd(Length(AText)) then
    Exit;
  SetLength(Result, Length(AText) div 2);
  for vIdx := 0 to High(Result) do
  begin
    vHigh := HexVal(AText[vIdx * 2 + 1]);
    vLow := HexVal(AText[vIdx * 2 + 2]);
    if (vHigh < 0) or (vLow < 0) then
    begin
      Result := nil;
      Exit;
    end;
    Result[vIdx] := Byte(vHigh * 16 + vLow);
  end;
end;

function RALChatSecondsToMs(const AText: StringRAL): Int64RAL;
var
  vValue: Double;
  vCode: Integer;
begin
  Result := 0;
  if AText = '' then
    Exit;
  Val(string(Trim(string(AText))), vValue, vCode);
  if (vCode = 0) and (vValue > 0) then
    Result := Round(vValue * 1000);
end;

{ ---- TRALChatHttp ---- }

constructor TRALChatHttp.Create;
begin
  inherited Create;
  FCallLock := TCriticalSection.Create;
  FLock := TCriticalSection.Create;
end;

destructor TRALChatHttp.Destroy;
begin
  FOwnClient.Free;
  FLock.Free;
  FCallLock.Free;
  inherited Destroy;
end;

function TRALChatHttp.Acquire: TRALClient;
begin
  Result := FClient;
  if Result <> nil then
    Exit;
  FLock.Enter;
  try
    if FOwnClient = nil then
    begin
      FOwnClient := NewClient;
      FOwnClient.RequestTimeout := 30000;
    end;
    Result := FOwnClient;
  finally
    FLock.Leave;
  end;
end;

function TRALChatHttp.NewClient: TRALClient;
var
  vList: TStringList;
begin
  Result := TRALClient.Create(nil);
  try
    if FClient <> nil then
    begin
      Result.EngineType := FClient.EngineType;
      Result.ConnectTimeout := FClient.ConnectTimeout;
      Result.SSL := FClient.SSL;
      Result.OnValidateServerCert := FClient.OnValidateServerCert;
    end
    else if FEngineType <> '' then
      Result.EngineType := FEngineType
    else
    begin
      vList := TStringList.Create;
      try
        GetEngineList(vList);
        if vList.Count = 0 then
          raise ERALChatError.CreateFmt(emChatHttpNoClient, [string(FChannelName)]);
        Result.EngineType := vList.Names[0];
        if Result.EngineType = '' then
          Result.EngineType := vList[0];
      finally
        vList.Free;
      end;
    end;
    Result.KeepCookies := False;
  except
    Result.Free;
    raise;
  end;
end;

procedure TRALChatHttp.Call(AMethod: TRALMethod; const AUrl: StringRAL; AHeaders: TStrings;
  const ABody, AContentType: StringRAL; AFields: TRALJSONObject; const AFileField: StringRAL;
  AMedia: TRALChatMedia; out AReply: TRALChatHttpReply);
var
  vClient: TRALClient;
  vResponse: TRALResponse;
  vStream: TStream;
  vParam: TRALParam;
  vIdx: Integer;
  vItem: TRALJSON;
begin
  AReply.Failed := True;
  AReply.Status := 0;
  AReply.Text := '';
  AReply.RetryAfterMs := 0;
  AReply.Failure := '';
  vResponse := nil;
  vStream := nil;
  FCallLock.Enter;
  try
    try
      vClient := Acquire;
      vClient.Request.Clear;
      if AHeaders <> nil then
        for vIdx := 0 to AHeaders.Count - 1 do
          vClient.Request.AddHeader(StringRAL(AHeaders.Names[vIdx]),
            StringRAL(AHeaders.ValueFromIndex[vIdx]));
      if AFileField <> '' then
      begin
        if AFields <> nil then
          for vIdx := 0 to AFields.Count - 1 do
          begin
            vItem := AFields.Items[vIdx];
            if vItem.Kind in [rjkString, rjkNumber, rjkBoolean] then
              vClient.Request.AddField(AFields.Names[vIdx], RALChatJText(vItem))
            else
              vClient.Request.AddField(AFields.Names[vIdx], vItem.ToJSON);
          end;
        vStream := AMedia.Open;
        vParam := vClient.Request.Params.AddParam(AFileField, vStream, rpkBODY);
        vParam.FileName := AMedia.FileName;
        if AMedia.MimeType <> '' then
          vParam.ContentType := AMedia.MimeType
        else
          vParam.ContentType := 'application/octet-stream';
      end
      else if ABody <> '' then
        vClient.Request.AddBody(ABody, AContentType);
      vClient.Execute(AUrl, AMethod, vResponse);
      AReply.Failed := False;
      AReply.Status := vResponse.StatusCode;
      AReply.Text := vResponse.ResponseText;
      AReply.RetryAfterMs := RALChatSecondsToMs(vResponse.GetHeader('Retry-After'));
    except
      on E: Exception do
      begin
        AReply.Failed := True;
        AReply.Failure := StringRAL(E.ClassName + ': ' + E.Message);
      end;
    end;
  finally
    FCallLock.Leave;
    vStream.Free;
    vResponse.Free;
  end;
end;

function TRALChatHttp.Download(const AUrl: StringRAL; AHeaders: TStrings;
  AMaxSize: Int64RAL): TStream;
var
  vClient: TRALClient;
  vResponse: TRALResponse;
  vData: TStream;
  vIdx: Integer;
begin
  Result := nil;
  vResponse := nil;
  FCallLock.Enter;
  try
    try
      vClient := Acquire;
      vClient.Request.Clear;
      if AHeaders <> nil then
        for vIdx := 0 to AHeaders.Count - 1 do
          vClient.Request.AddHeader(StringRAL(AHeaders.Names[vIdx]),
            StringRAL(AHeaders.ValueFromIndex[vIdx]));
      vClient.Get(AUrl, vResponse);
      if vResponse.StatusCode <> 200 then
        raise ERALChatError.CreateFmt(emChatHttpStatus, [vResponse.StatusCode]);
      vData := vResponse.ResponseStream;
      if (vData <> nil) and (AMaxSize > 0) and (vData.Size > AMaxSize) then
        raise ERALChatError.CreateFmt(emChatHttpTooBig, [AMaxSize]);
      Result := TMemoryStream.Create;
      try
        if vData <> nil then
        begin
          vData.Position := 0;
          Result.CopyFrom(vData, 0);
        end;
        Result.Position := 0;
      except
        FreeAndNil(Result);
        raise;
      end;
    finally
      FCallLock.Leave;
      vResponse.Free;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

end.
