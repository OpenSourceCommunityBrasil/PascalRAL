unit delphiutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Registry,
  ideutils;

type
  TDelphiPlatforms = (pfWin32, pfWin64, pfAndroid, pfAndroid64, pfiOSDevice32,
                     pfiOSDevice64, pfiOSSimARM64, pfiOSSimulator, pfOSX32,
                     pfOSX64, pfOSXARM64, pfLinux64);

  { TDelphiObjectData }

  TDelphiObjectData = class(TIDEObjectData)
  private
    FRegPath : string;
    FRegVersion : string;
    FProductVersion : integer;
  protected
    procedure SetExeFile(AValue: string); override;
    procedure SetRegPath(AValue: string);
  published
    property RegPath : string read FRegPath write SetRegPath;
    property ProductVersion : integer read FProductVersion write FProductVersion;
    property RegVersion : string read FRegVersion write FRegVersion;
  end;

  { TDelphiFinder }

  TDelphiFinder = class(TIDEFinder)
  protected
    function GetObjectDelphi(RegPath: string) : TDelphiObjectData;
  public
    procedure BuscarIDE; override;
  end;

implementation

const
  DelphisRegKey: array [0..3] of string = (
    '\Software\Borland\Delphi',
    '\Software\Borland\BDS',
    '\Software\CodeGear\BDS',
    '\Software\Embarcadero\BDS'
  );

function RegKeyExists(const RegPath: string; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result := Reg.KeyExists(RegPath);
    finally
      Reg.Free;
    end;
  except

  end;
end;

function RegReadStr(const RegPath, RegValue: string; var Str: string; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Str := '';
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      if Reg.KeyExists(RegPath) and
         Reg.OpenKeyReadOnly(RegPath) then
      begin
        Str := Reg.ReadString(RegValue);
        Result := True;
      end;
    finally
      Reg.Free;
    end;
  except

  end;
end;

function RegReadList(const RegPath: string; AList: TStringList; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  AList.Clear;
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      if Reg.KeyExists(RegPath) and Reg.OpenKeyReadOnly(RegPath) then
      begin
        Reg.GetKeyNames(AList);
        Result := True;
      end;
    finally
      Reg.Free;
    end;
  except

  end;
end;


{ TDelphiObjectData }

procedure TDelphiObjectData.SetRegPath(AValue: string);
var
  vKey, vStr: String;
begin
  if FRegPath = AValue then
    Exit;

  FRegPath := AValue;
  vStr := '';

  // nome da delphi
  vKey := AValue;
  RegReadStr(vKey, 'Personalities', vStr, HKEY_CURRENT_USER);
  if vStr = '' then
    RegReadStr(vKey, 'Personalities', vStr, HKEY_LOCAL_MACHINE);

  if vStr = '' then
  begin
    vKey := AValue + '\Personalities';
    RegReadStr(vKey, '', vStr, HKEY_CURRENT_USER);
    if vStr = '' then
      RegReadStr(vKey, '', vStr, HKEY_LOCAL_MACHINE);
    if vStr = '' then
      RegReadStr(vKey, 'Delphi.Win32', vKey, HKEY_CURRENT_USER);
    if vStr = '' then
      RegReadStr(vKey, 'Delphi.Win32', vKey, HKEY_LOCAL_MACHINE);
  end;

  if vStr <> '' then
    Self.Name := vStr;

  // exe do delphi
  vKey := AValue;
  RegReadStr(vKey, 'App', vStr, HKEY_CURRENT_USER);
  if vStr = '' then
    RegReadStr(vKey, 'App', vStr, HKEY_LOCAL_MACHINE);

  Self.ExeFile := vStr;

  // productversion
  vKey := AValue;
  RegReadStr(vKey, 'ProductVersion', vStr, HKEY_CURRENT_USER);
  if vStr = '' then
    RegReadStr(vKey, 'ProductVersion', vStr, HKEY_LOCAL_MACHINE);

  if vStr <> '' then
    Self.ProductVersion := StrToIntDef(vStr, -1);

  // delphis antigos
  if Self.Name = '' then
  begin
    if SameText(AValue, '\Software\Borland\Delphi\7.0') then
    begin
      Self.Name := 'Delphi 7.0';
      Self.ProductVersion := 7;
    end;
  end;
end;

procedure TDelphiObjectData.SetExeFile(AValue: string);
begin
  inherited;
end;

{ TDelphiFinder }

function TDelphiFinder.GetObjectDelphi(RegPath: string): TDelphiObjectData;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(Count) do
  begin
    if TDelphiObjectData(ObjectData[vInt]).RegPath = RegPath then begin
      Result := TDelphiObjectData(ObjectData[vInt]);
      Break;
    end;
  end;
end;

procedure TDelphiFinder.BuscarIDE;
var
  vInt1, vInt2: integer;
  vLst: TStringList;
  vObj: TDelphiObjectData;
  vKey: string;
  vFormat: TFormatSettings;
begin
  vLst := TStringList.Create;

  vFormat.DecimalSeparator := '.';
  vFormat.ThousandSeparator := ',';

  try
    for vInt1 := 0 to High(DelphisRegKey) do
    begin
      if RegKeyExists(DelphisRegKey[vInt1], HKEY_CURRENT_USER) then
      begin
        RegReadList(DelphisRegKey[vInt1], vLst, HKEY_CURRENT_USER);
        for vInt2 := 0 to Pred(vLst.Count) do
        begin
          if StrToFloatDef(vLst.Strings[vInt2], -1, vFormat) > 0 then
          begin
            vKey := DelphisRegKey[vInt1] + '\' + vLst.Strings[vInt2];

            vObj := TDelphiObjectData.Create;
            vObj.RegVersion := vLst.Strings[vInt2];
            vObj.RegPath := vKey;

            List.Add(vObj);
          end;
        end;
      end;

      if RegKeyExists(DelphisRegKey[vInt1], HKEY_LOCAL_MACHINE) then
      begin
        RegReadList(DelphisRegKey[vInt1], vLst, HKEY_LOCAL_MACHINE);
        for vInt2 := 0 to Pred(vLst.Count) do
        begin
          if StrToFloatDef(vLst.Strings[vInt2], -1, vFormat) > 0 then
          begin
            vKey := DelphisRegKey[vInt1] + '\' + vLst.Strings[vInt2];
            vObj := GetObjectDelphi(vKey);
            if vObj = nil then begin
              vObj := TDelphiObjectData.Create;
              vObj.RegVersion := vLst.Strings[vInt2];
              vObj.RegPath := vKey;

              List.Add(vObj);
            end;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(vLst);
  end;
end;

end.

