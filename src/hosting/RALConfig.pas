/// External configuration in layers, the last one winning: a file (JSON, INI
/// or .env, by extension), the environment (RAL_SERVER__PORT is Server.Port)
/// and the command line (--Server.Port=8080). Bind applies the keys to the
/// published properties of a component and of its sub-objects.
unit RALConfig;

{$I ..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, TypInfo,
  RALTypes, RALCustomObjects, RALJson, RALYAML, RALExceptions, RALErrorCodes;

type
  /// Where a key's value came from.
  TRALConfigSource = (csNone, csDefault, csFile, csEnvironment, csCommandLine);

  { The keys of every source and the objects bound to them. Keys are
    case-insensitive and dotted (Server.Port, Server.SSL.CertFile). }
  TRALConfig = class(TRALComponent)
  private
    FArgs: TStringList;
    FBindings: TStringList;
    FDefaults: TStringList;
    FEnvPrefix: string;
    FFile: TStringList;
    FFileAge: TDateTime;
    FFileName: string;
    FReloadableProperties: TStrings;
    FUseCommandLine: boolean;
    FUseEnvironment: boolean;
    FOnChange: TNotifyEvent;
    procedure SetReloadableProperties(const AValue: TStrings);
  protected
    /// Applies the keys under APrefix to AObject; AOnlyReloadable leaves the rest.
    procedure ApplyTo(AObject: TPersistent; const APrefix: string; AOnlyReloadable: boolean);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ReadCommandLine;
    procedure ReadFile;
    /// The value of the key in the environment ('' and False when absent).
    function ReadEnvironment(const AKey: string; out AValue: string): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// The environment variable of a key: Server.Port -> RAL_SERVER__PORT.
    function EnvName(const AKey: string): string;
    /// Reads the file and the command line again (the environment is read per key).
    procedure Load;
    /// Fills AList with name=value pairs from a JSON text (nested objects are dotted).
    class procedure ParseJSON(const AText: string; AList: TStrings);
    /// Fills AList from INI text: [Server] Port=1 is Server.Port=1.
    class procedure ParseINI(const AText: string; AList: TStrings);
    /// Fills AList from .env text: SERVER__PORT=1 is SERVER.PORT=1.
    procedure ParseDotEnv(const AText: string; AList: TStrings);
    { Loads again and applies to the bound objects the keys of
      ReloadableProperties (all when the list is empty); True when a value
      changed. OnChange fires then. }
    function Reload: boolean;
    /// Reload when the file changed on disk since the last read.
    function ReloadIfChanged: boolean;

    /// True when some source has the key.
    function Has(const AKey: string): boolean;
    /// The value of the key, or ADefault.
    function Get(const AKey: string; const ADefault: string = ''): string;
    function GetInt(const AKey: string; ADefault: Int64 = 0): Int64;
    function GetBool(const AKey: string; ADefault: boolean = False): boolean;
    function GetFloat(const AKey: string; ADefault: Double = 0): Double;
    /// Which source gave the key's value.
    function SourceOf(const AKey: string): TRALConfigSource;
    /// A default, below every source.
    procedure SetDefault(const AKey, AValue: string);
    /// The keys known from the defaults, the file and the command line.
    procedure GetKeys(AList: TStrings);
    { The value as a log may show it: keys with Password, Secret, Token or Key
      in the name come out as '***'. }
    function SafeValue(const AKey: string): string;

    { Applies the keys under APrefix ('Server' -> Server.Port...) to the
      published properties of AObject and its sub-objects, and remembers it
      for Reload. An empty prefix uses the bare property names. }
    procedure Bind(AObject: TPersistent; const APrefix: string);
    /// Forgets the binding of AObject.
    procedure Unbind(AObject: TPersistent);
  published
    /// Prefix of the environment variables.
    property EnvPrefix: string read FEnvPrefix write FEnvPrefix;
    { The file: .json, .ini or .env (by extension). --config=<file> on the
      command line replaces it. }
    property FileName: string read FFileName write FFileName;
    /// Keys that Reload applies (Server.MaxConnections); empty = all.
    property ReloadableProperties: TStrings read FReloadableProperties write SetReloadableProperties;
    /// Read --Key=Value arguments.
    property UseCommandLine: boolean read FUseCommandLine write FUseCommandLine default True;
    /// Read the environment variables.
    property UseEnvironment: boolean read FUseEnvironment write FUseEnvironment default True;
    /// Reload changed something.
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

/// True when a key names a secret (Password, Secret, Token, Key, Pwd).
function RALIsSecretKey(const AKey: string): boolean;

implementation

const
  cDefaultEnvPrefix = 'RAL_';

var
  gDot: TFormatSettings;

function RALIsSecretKey(const AKey: string): boolean;
var
  vKey: string;
begin
  vKey := LowerCase(AKey);
  Result := (Pos('password', vKey) > 0) or (Pos('secret', vKey) > 0) or
    (Pos('token', vKey) > 0) or (Pos('pwd', vKey) > 0) or
    (Copy(vKey, Length(vKey) - 2, 3) = 'key');
end;

function NewList: TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  Result.NameValueSeparator := '=';
end;

{ a value of a list, the name compared without case }
function FindValue(AList: TStrings; const AKey: string; out AValue: string): boolean;
var
  vIdx: IntegerRAL;
begin
  vIdx := AList.IndexOfName(AKey);
  Result := vIdx >= 0;
  if Result then
    AValue := AList.ValueFromIndex[vIdx]
  else
    AValue := '';
end;

procedure PutValue(AList: TStrings; const AKey, AValue: string);
var
  vIdx: IntegerRAL;
begin
  vIdx := AList.IndexOfName(AKey);
  if vIdx >= 0 then
    AList[vIdx] := AKey + '=' + AValue
  else
    AList.Add(AKey + '=' + AValue);
end;

function Unquote(const AText: string): string;
begin
  Result := Trim(AText);
  if (Length(Result) >= 2) and CharInSet(Result[1], ['"', '''']) and
    (Result[Length(Result)] = Result[1]) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

{ TRALConfig }

constructor TRALConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArgs := NewList;
  FFile := NewList;
  FDefaults := NewList;
  FBindings := TStringList.Create;
  FReloadableProperties := TStringList.Create;
  TStringList(FReloadableProperties).CaseSensitive := False;
  FEnvPrefix := cDefaultEnvPrefix;
  FUseCommandLine := True;
  FUseEnvironment := True;
end;

destructor TRALConfig.Destroy;
begin
  FreeAndNil(FReloadableProperties);
  FreeAndNil(FBindings);
  FreeAndNil(FDefaults);
  FreeAndNil(FFile);
  FreeAndNil(FArgs);
  inherited;
end;

procedure TRALConfig.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FBindings <> nil) then
    Unbind(AComponent);
end;

procedure TRALConfig.SetReloadableProperties(const AValue: TStrings);
begin
  FReloadableProperties.Assign(AValue);
end;

function TRALConfig.EnvName(const AKey: string): string;
begin
  Result := FEnvPrefix + UpperCase(StringReplace(AKey, '.', '__', [rfReplaceAll]));
end;

class procedure TRALConfig.ParseJSON(const AText: string; AList: TStrings);

  procedure Walk(AValue: TRALJSONValue; const APath: string);
  var
    vInt: IntegerRAL;
    vText: string;
    vItem: TRALJSONValue;
    vAllSimple: boolean;
  begin
    if AValue = nil then
      Exit;
    case AValue.JsonType of
      rjtObject:
        for vInt := 0 to TRALJSONObject(AValue).Count - 1 do
        begin
          if APath = '' then
            Walk(TRALJSONObject(AValue).Get(vInt), string(TRALJSONObject(AValue).GetName(vInt)))
          else
            Walk(TRALJSONObject(AValue).Get(vInt),
              APath + '.' + string(TRALJSONObject(AValue).GetName(vInt)));
        end;
      rjtArray:
      begin
        { a list of plain values is one comma-separated value; anything else
          is indexed: Hosts.0.Name }
        vAllSimple := True;
        for vInt := 0 to TRALJSONArray(AValue).Count - 1 do
          if TRALJSONArray(AValue).Get(vInt).JsonType in [rjtObject, rjtArray] then
            vAllSimple := False;
        if vAllSimple then
        begin
          vText := '';
          for vInt := 0 to TRALJSONArray(AValue).Count - 1 do
          begin
            vItem := TRALJSONArray(AValue).Get(vInt);
            if vInt > 0 then
              vText := vText + ',';
            if vItem.JsonType = rjtNumber then
              vText := vText + string(RALJSONNumberText(vItem))
            else
              vText := vText + string(vItem.AsString);
          end;
          PutValue(AList, APath, vText);
        end
        else
          for vInt := 0 to TRALJSONArray(AValue).Count - 1 do
            Walk(TRALJSONArray(AValue).Get(vInt), APath + '.' + IntToStr(vInt));
      end;
      rjtBoolean:
        if AValue.AsBoolean then
          PutValue(AList, APath, 'true')
        else
          PutValue(AList, APath, 'false');
      { a number as JSON writes it, never by the machine's locale }
      rjtNumber:
        PutValue(AList, APath, string(RALJSONNumberText(AValue)));
    else
      if not AValue.IsNull then
        PutValue(AList, APath, string(AValue.AsString));
    end;
  end;

var
  vJson: TRALJSONValue;
begin
  vJson := TRALJSON.ParseJSON(StringRAL(AText));
  if vJson = nil then
    raise ERALException.CreateFmt(ecConfigInvalidJSON, [Copy(AText, 1, 60)]);
  try
    Walk(vJson, '');
  finally
    vJson.Free;
  end;
end;

class procedure TRALConfig.ParseINI(const AText: string; AList: TStrings);
var
  vLines: TStringList;
  vInt, vPos: IntegerRAL;
  vLine, vSection, vName: string;
begin
  vLines := TStringList.Create;
  try
    vLines.Text := AText;
    vSection := '';
    for vInt := 0 to vLines.Count - 1 do
    begin
      vLine := Trim(vLines[vInt]);
      if (vLine = '') or CharInSet(vLine[1], [';', '#']) then
        Continue;
      if (vLine[1] = '[') and (vLine[Length(vLine)] = ']') then
      begin
        vSection := Trim(Copy(vLine, 2, Length(vLine) - 2));
        Continue;
      end;
      vPos := Pos('=', vLine);
      if vPos = 0 then
        Continue;
      vName := Trim(Copy(vLine, 1, vPos - 1));
      if vSection <> '' then
        vName := vSection + '.' + vName;
      PutValue(AList, vName, Unquote(Copy(vLine, vPos + 1, Length(vLine))));
    end;
  finally
    vLines.Free;
  end;
end;

procedure TRALConfig.ParseDotEnv(const AText: string; AList: TStrings);
var
  vLines: TStringList;
  vInt, vPos: IntegerRAL;
  vLine, vName: string;
begin
  vLines := TStringList.Create;
  try
    vLines.Text := AText;
    for vInt := 0 to vLines.Count - 1 do
    begin
      vLine := Trim(vLines[vInt]);
      if (vLine = '') or (vLine[1] = '#') then
        Continue;
      if SameText(Copy(vLine, 1, 7), 'export ') then
        vLine := Trim(Copy(vLine, 8, Length(vLine)));
      vPos := Pos('=', vLine);
      if vPos = 0 then
        Continue;
      vName := Trim(Copy(vLine, 1, vPos - 1));
      if (FEnvPrefix <> '') and SameText(Copy(vName, 1, Length(FEnvPrefix)), FEnvPrefix) then
        Delete(vName, 1, Length(FEnvPrefix));
      vName := StringReplace(vName, '__', '.', [rfReplaceAll]);
      PutValue(AList, vName, Unquote(Copy(vLine, vPos + 1, Length(vLine))));
    end;
  finally
    vLines.Free;
  end;
end;

procedure TRALConfig.ReadFile;
var
  vText: TStringList;
  vExt: string;
begin
  FFile.Clear;
  FFileAge := 0;
  if (FFileName = '') or not FileAge(FFileName, FFileAge) then
    Exit;
  vText := TStringList.Create;
  try
    vText.LoadFromFile(FFileName);
    vExt := LowerCase(ExtractFileExt(FFileName));
    if vExt = '.json' then
      ParseJSON(vText.Text, FFile)
    else if vExt = '.ini' then
      ParseINI(vText.Text, FFile)
    else
      ParseDotEnv(vText.Text, FFile);
  finally
    vText.Free;
  end;
end;

procedure TRALConfig.ReadCommandLine;
var
  vInt, vPos: IntegerRAL;
  vArg, vName: string;
begin
  FArgs.Clear;
  if not FUseCommandLine then
    Exit;
  for vInt := 1 to ParamCount do
  begin
    vArg := ParamStr(vInt);
    if Copy(vArg, 1, 2) <> '--' then
      Continue;
    Delete(vArg, 1, 2);
    vPos := Pos('=', vArg);
    if vPos = 0 then
      Continue;
    vName := Copy(vArg, 1, vPos - 1);
    if SameText(vName, 'config') then
      FFileName := Copy(vArg, vPos + 1, Length(vArg))
    else
      PutValue(FArgs, vName, Copy(vArg, vPos + 1, Length(vArg)));
  end;
end;

function TRALConfig.ReadEnvironment(const AKey: string; out AValue: string): boolean;
begin
  AValue := '';
  Result := False;
  if not FUseEnvironment then
    Exit;
  AValue := GetEnvironmentVariable(EnvName(AKey));
  Result := AValue <> '';
end;

procedure TRALConfig.Load;
begin
  { the command line first: it may name the file }
  ReadCommandLine;
  ReadFile;
end;

function TRALConfig.SourceOf(const AKey: string): TRALConfigSource;
var
  vValue: string;
begin
  if FindValue(FArgs, AKey, vValue) then
    Result := csCommandLine
  else if ReadEnvironment(AKey, vValue) then
    Result := csEnvironment
  else if FindValue(FFile, AKey, vValue) then
    Result := csFile
  else if FindValue(FDefaults, AKey, vValue) then
    Result := csDefault
  else
    Result := csNone;
end;

function TRALConfig.Has(const AKey: string): boolean;
begin
  Result := SourceOf(AKey) <> csNone;
end;

function TRALConfig.Get(const AKey: string; const ADefault: string): string;
begin
  if FindValue(FArgs, AKey, Result) then
    Exit;
  if ReadEnvironment(AKey, Result) then
    Exit;
  if FindValue(FFile, AKey, Result) then
    Exit;
  if FindValue(FDefaults, AKey, Result) then
    Exit;
  Result := ADefault;
end;

function TRALConfig.GetInt(const AKey: string; ADefault: Int64): Int64;
begin
  Result := StrToInt64Def(Trim(Get(AKey)), ADefault);
end;

function TRALConfig.GetBool(const AKey: string; ADefault: boolean): boolean;
var
  vValue: string;
begin
  vValue := LowerCase(Trim(Get(AKey)));
  if (vValue = 'true') or (vValue = '1') or (vValue = 'yes') or (vValue = 'on') then
    Result := True
  else if (vValue = 'false') or (vValue = '0') or (vValue = 'no') or (vValue = 'off') then
    Result := False
  else
    Result := ADefault;
end;

function TRALConfig.GetFloat(const AKey: string; ADefault: Double): Double;
begin
  Result := StrToFloatDef(Trim(Get(AKey)), ADefault, gDot);
end;

procedure TRALConfig.SetDefault(const AKey, AValue: string);
begin
  PutValue(FDefaults, AKey, AValue);
end;

procedure TRALConfig.GetKeys(AList: TStrings);
var
  vAll: TStringList;
  vInt: IntegerRAL;

  procedure AddFrom(ASource: TStrings);
  var
    vIdx: IntegerRAL;
  begin
    for vIdx := 0 to ASource.Count - 1 do
      if vAll.IndexOf(ASource.Names[vIdx]) < 0 then
        vAll.Add(ASource.Names[vIdx]);
  end;

begin
  vAll := TStringList.Create;
  try
    vAll.CaseSensitive := False;
    AddFrom(FDefaults);
    AddFrom(FFile);
    AddFrom(FArgs);
    AList.Clear;
    for vInt := 0 to vAll.Count - 1 do
      AList.Add(vAll[vInt]);
  finally
    vAll.Free;
  end;
end;

function TRALConfig.SafeValue(const AKey: string): string;
begin
  if RALIsSecretKey(AKey) then
    Result := '***'
  else
    Result := Get(AKey);
end;

procedure TRALConfig.ApplyTo(AObject: TPersistent; const APrefix: string;
  AOnlyReloadable: boolean);
var
  vList: PPropList;
  vCount, vInt: IntegerRAL;
  vInfo: PPropInfo;
  vKey, vValue, vName: string;
  vSub: TObject;
begin
  if (AObject = nil) or (AObject.ClassInfo = nil) then
    Exit;
  vCount := GetPropList(AObject.ClassInfo, tkProperties, nil);
  if vCount = 0 then
    Exit;
  GetMem(vList, vCount * SizeOf(PPropInfo));
  try
    GetPropList(AObject.ClassInfo, tkProperties, vList);
    for vInt := 0 to vCount - 1 do
    begin
      vInfo := vList^[vInt];
      vName := string(vInfo^.Name);
      if APrefix <> '' then
        vKey := APrefix + '.' + vName
      else
        vKey := vName;

      if vInfo^.PropType^{$IFNDEF FPC}^{$ENDIF}.Kind = tkClass then
      begin
        vSub := GetObjectProp(AObject, vInfo);
        if vSub is TStrings then
        begin
          if Has(vKey) and ((not AOnlyReloadable) or (FReloadableProperties.Count = 0) or
            (FReloadableProperties.IndexOf(vKey) >= 0)) then
            TStrings(vSub).CommaText := Get(vKey);
        end
        { a sub-object (SSL, IPConfig), never a linked component }
        else if (vSub is TPersistent) and not (vSub is TComponent) then
          ApplyTo(TPersistent(vSub), vKey, AOnlyReloadable);
        Continue;
      end;

      if (vInfo^.SetProc = nil) or not Has(vKey) then
        Continue;
      if AOnlyReloadable and (FReloadableProperties.Count > 0) and
        (FReloadableProperties.IndexOf(vKey) < 0) then
        Continue;
      vValue := Trim(Get(vKey));
      case vInfo^.PropType^{$IFNDEF FPC}^{$ENDIF}.Kind of
        tkInteger, tkChar, tkWChar:
          SetOrdProp(AObject, vInfo, StrToInt(vValue));
        tkInt64:
          SetInt64Prop(AObject, vInfo, StrToInt64(vValue));
        tkEnumeration{$IFDEF FPC}, tkBool{$ENDIF}:
          if SameText(string(vInfo^.PropType^{$IFNDEF FPC}^{$ENDIF}.Name), 'Boolean') then
            SetOrdProp(AObject, vInfo, Ord(GetBool(vKey)))
          else if (vValue <> '') and CharInSet(vValue[1], ['0'..'9']) then
            SetOrdProp(AObject, vInfo, StrToInt(vValue))
          else
            SetEnumProp(AObject, vInfo, vValue);
        tkSet:
          SetSetProp(AObject, vInfo, vValue);
        tkFloat:
          SetFloatProp(AObject, vInfo, StrToFloat(vValue, gDot));
        tkString, tkLString, tkWString{$IFNDEF FPC}{$IFDEF UNICODE}, tkUString{$ENDIF}{$ENDIF}
          {$IFDEF FPC}, tkAString, tkUString{$ENDIF}:
          SetStrProp(AObject, vInfo, Get(vKey));
      end;
    end;
  finally
    FreeMem(vList);
  end;
end;

procedure TRALConfig.Bind(AObject: TPersistent; const APrefix: string);
begin
  if AObject = nil then
    Exit;
  Unbind(AObject);
  FBindings.AddObject(APrefix, AObject);
  if AObject is TComponent then
    TComponent(AObject).FreeNotification(Self);
  ApplyTo(AObject, APrefix, False);
end;

procedure TRALConfig.Unbind(AObject: TPersistent);
var
  vIdx: IntegerRAL;
begin
  vIdx := FBindings.IndexOfObject(AObject);
  while vIdx >= 0 do
  begin
    FBindings.Delete(vIdx);
    vIdx := FBindings.IndexOfObject(AObject);
  end;
end;

function TRALConfig.Reload: boolean;
var
  vBefore, vAfter: TStringList;
  vInt: IntegerRAL;
begin
  vBefore := TStringList.Create;
  vAfter := TStringList.Create;
  try
    vBefore.AddStrings(FFile);
    vBefore.AddStrings(FArgs);
    Load;
    vAfter.AddStrings(FFile);
    vAfter.AddStrings(FArgs);
    Result := vBefore.Text <> vAfter.Text;
    if Result then
    begin
      for vInt := 0 to FBindings.Count - 1 do
        ApplyTo(TPersistent(FBindings.Objects[vInt]), FBindings[vInt], True);
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
  finally
    vAfter.Free;
    vBefore.Free;
  end;
end;

function TRALConfig.ReloadIfChanged: boolean;
var
  vAge: TDateTime;
begin
  Result := (FFileName <> '') and FileAge(FFileName, vAge) and (vAge <> FFileAge) and Reload;
end;

initialization
  gDot := {$IFDEF FPC}DefaultFormatSettings{$ELSE}FormatSettings{$ENDIF};
  gDot.DecimalSeparator := '.';
  gDot.ThousandSeparator := #0;

end.
