unit delphiutils;

{$mode ObjFPC}{$H+}

interface

uses
  Generics.Defaults,
  Generics.Collections,
  Graphics, SysUtils, Classes, ComCtrls, Controls, ImgList, CommCtrl, ShellAPI,
  ShlObj, Windows, Registry;

type
  TDelphiVersions = (Delphi7, Delphi8, Delphi2005, Delphi2006, Delphi2007,
    Delphi2009, Delphi2010, DelphiXE, DelphiXE2, DelphiXE3, DelphiXE4, DelphiXE5,
    DelphiXE6, DelphiXE7, DelphiXE8, Delphi10Seattle, Delphi10Berlin,
    Delphi10Tokyo, Delphi10Rio, Delphi10Sydney, Delphi11Alexandria);

  TDelphiObjectData = class
  private
    FVersion: TDelphiVersions;
    FName: string;
    FPath: string;
    FIcon: TIcon;
    FRegKey: string;
  public
    property Version: TDelphiVersions read FVersion write FVersion;
    property Path: string read FPath write FPath;
    property Name: string read FName write FName;
    property Icon: TIcon read FIcon write FIcon;
    property RegKey: string read FRegKey;
    constructor Create;
    destructor Destroy; override;
  end;

  { TDelphiInstaller }

  TDelphiInstaller = class
  private
    procedure AddLibraryPath(const APath: string); overload;
    procedure AddLibraryPath(const APaths: array of string); overload;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TDelphiObjectList = TList;

{$REGION 'Consts}
const
  DelphiRegKey: array [TDelphiVersions] of string = (
    '\Software\Borland\Delphi\7.0',
    '\Software\Borland\BDS\2.0',
    '\Software\Borland\BDS\3.0',
    '\Software\Borland\BDS\4.0',
    '\Software\Borland\BDS\5.0',
    '\Software\CodeGear\BDS\6.0',
    '\Software\CodeGear\BDS\7.0',
    '\Software\Embarcadero\BDS\8.0',
    '\Software\Embarcadero\BDS\9.0',
    '\Software\Embarcadero\BDS\10.0',
    '\Software\Embarcadero\BDS\11.0',
    '\Software\Embarcadero\BDS\12.0',
    '\Software\Embarcadero\BDS\14.0',
    '\Software\Embarcadero\BDS\15.0',
    '\Software\Embarcadero\BDS\16.0',
    '\Software\Embarcadero\BDS\17.0',
    '\Software\Embarcadero\BDS\18.0',
    '\Software\Embarcadero\BDS\19.0',
    '\Software\Embarcadero\BDS\20.0',
    '\Software\Embarcadero\BDS\21.0',
    '\Software\Embarcadero\BDS\22.0'
    );

  DelphiCustomRegPaths: array [TDelphiVersions] of string = (
    '\Software\Borland\%s\7.0', // Delphi
    '\Software\Borland\%s\2.0', // BDS
    '\Software\Borland\%s\3.0', // BDS
    '\Software\Borland\%s\4.0', // BDS
    '\Software\Borland\%s\5.0', // BDS
    '\Software\CodeGear\%s\6.0', // BDS
    '\Software\CodeGear\%s\7.0', // BDS
    '\Software\Embarcadero\%s\8.0', // BDS
    '\Software\Embarcadero\%s\9.0', // BDS
    '\Software\Embarcadero\%s\10.0', // BDS
    '\Software\Embarcadero\%s\11.0', // BDS
    '\Software\Embarcadero\%s\12.0', // BDS
    '\Software\Embarcadero\%s\14.0', // BDS
    '\Software\Embarcadero\%s\15.0', // BDS
    '\Software\Embarcadero\%s\16.0', // BDS
    '\Software\Embarcadero\%s\17.0', // BDS
    '\Software\Embarcadero\%s\18.0', // BDS
    '\Software\Embarcadero\%s\19.0', // BDS
    '\Software\Embarcadero\%s\20.0', // BDS
    '\Software\Embarcadero\%s\21.0', // BDS
    '\Software\Embarcadero\%s\22.0'  // BDS
    );

  DelphiRegPathNumbers: array [TDelphiVersions] of integer = (
    7, // 'Delphi 7',
    2, // 'Delphi 8',
    3, // 'BDS 2005',
    4, // 'BDS 2006',
    5, // 'RAD Studio 2007',
    6, // 'RAD Studio 2009',
    7, // 'RAD Studio 2010',
    8, // 'RAD Studio XE'
    9, // 'RAD Studio XE2'
    10, // 'RAD Studio XE3'
    11, // 'RAD Studio XE4'
    12, // 'RAD Studio XE5'
    14, // 'RAD Studio XE6'
    15, // 'RAD Studio XE7'
    16, // 'RAD Studio XE8'
    17, // 'RAD Studio 10 Seattle'
    18, // 'RAD Studio 10.1 Berlin'
    19, // 'RAD Studio 10.2 Tokyo'
    20, // 'RAD Studio 10.3 Rio'
    21, // 'RAD Studio 10.4 Sydney'
    22  // 'RAD Studio 11.0 Alexandria'
    );

  DelphiVersionsNames: array [TDelphiVersions] of string = (
    'Delphi 7', 'Delphi 8', 'BDS 2005', 'BDS 2006', 'RAD Studio 2007',
    'RAD Studio 2009', 'RAD Studio 2010', 'RAD Studio XE', 'RAD Studio XE2',
    'RAD Studio XE3', 'RAD Studio XE4', 'RAD Studio XE5',
    'RAD Studio XE6/Appmethod 1.14', 'RAD Studio XE7/Appmethod 1.15',
    'RAD Studio XE8', 'RAD Studio 10 Seattle', 'RAD Studio 10.1 Berlin',
    'RAD Studio 10.2 Tokyo', 'RAD Studio 10.3 Rio', 'RAD Studio 10.4 Sydney',
    'RAD Studio 11.0 Alexandria');

  DelphiVersionNumbers: array [TDelphiVersions] of double = (
    15, // 'Delphi 7',
    16, // 'Delphi 8',
    17, // 'BDS 2005',
    18, // 'BDS 2006',
    18.5, // 'RAD Studio 2007',
    20, // 'RAD Studio 2009',
    21, // 'RAD Studio 2010',
    22, // 'RAD Studio XE'
    23, // 'RAD Studio XE2'
    24, // 'RAD Studio XE3'
    25, // 'RAD Studio XE4'
    26, // 'RAD Studio XE5'
    27, // 'RAD Studio XE6'
    28, // 'RAD Studio XE7'
    29, // 'RAD Studio XE8'
    30, // 'RAD Studio 10 Seattle'
    31, // 'RAD Studio 10.1 Berlin'
    32, // 'RAD Studio 10.2 Tokyo'
    33, // 'RAD Studio 10.3 Rio'
    34, // 'RAD Studio 10.4 Sydney'
    35  // 'RAD Studio 11.0 Alexandria'
    );
{$ENDREGION}

procedure FillCurrentDelphiVersion(Data: TDelphiObjectData);
procedure FillListDelphiVersions(AList: TList);
function ListInstalledDelphiVersions: TDelphiObjectList;

implementation

{ TDelphiInstaller }

procedure TDelphiInstaller.AddLibraryPath(const APath: string);
begin

end;

procedure TDelphiInstaller.AddLibraryPath(const APaths: array of string);
var
  I: integer;
begin
  for I := 0 to pred(Length(APaths)) do
    AddLibraryPath(Apaths[I]);
end;

constructor TDelphiInstaller.Create;
begin

end;

destructor TDelphiInstaller.Destroy;
begin
  inherited Destroy;
end;

{$REGION 'Custom Functions'}

function RegKeyExists(const RegPath: string; const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result := Reg.KeyExists(RegPath);
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

function RegReadStr(const RegPath, RegValue: string; var Str: string;
  const RootKey: HKEY): boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      Result := Reg.OpenKey(RegPath, True);
      if Result then  Str := Reg.ReadString(RegValue);
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

procedure ScanRootKey(const AKey: string; AList: TStrings; AMin, AMax: integer);
var
  s: string;
  RootKey: HKEY;
  LList: TStrings;

  procedure GetItems();
  var
    LRegistry: TRegistry;
  begin
    LRegistry := TRegistry.Create;
    try
      LRegistry.RootKey := RootKey;
      if LRegistry.OpenKeyReadOnly(AKey) then
        LRegistry.GetKeyNames(LList);
    finally
      LRegistry.Free;
    end;
  end;

  function IsValidKey(const ASubKey: string): boolean;
  var
    LVersion: integer;
    FullKey, FileName: string;
  begin
    Result := False;
    for LVersion := AMin to AMax do
    begin
      FullKey := Format('%s\%s\%d.0', [AKey, ASubKey, LVersion]);
      if RegKeyExists(FullKey, RootKey) then
      begin
        Result := RegReadStr(FullKey, 'App', FileName, RootKey) and FileExists(FileName);
        if Result then
          break;
      end;
    end;
  end;

begin
  RootKey := HKEY_CURRENT_USER;
  LList := TStringList.Create;
  try
    AList.Clear;
    GetItems();
    if LList.Count > 0 then
      for s in LList do
        if { (s <> 'BDS') and } IsValidKey(s) then
          AList.Add(s);
  finally
    LList.Free;
  end;
end;

{$ENDREGION}

function ListInstalledDelphiVersions: TDelphiObjectList;
var
  item: TDelphiObjectData;
  DelphiComp: TDelphiVersions;
  FileName: string;
  ImageIndex: integer;
  Found: boolean;
begin
  for DelphiComp := Low(TDelphiVersions) to High(TDelphiVersions) do
  begin
    Found := RegKeyExists(DelphiRegKey[DelphiComp], HKEY_CURRENT_USER);
    if Found then
      Found := RegReadStr(DelphiRegKey[DelphiComp], 'App', FileName,
        HKEY_CURRENT_USER) and FileExists(FileName);

    if not Found then
    begin
      Found := RegKeyExists(DelphiRegKey[DelphiComp], HKEY_LOCAL_MACHINE);
      if Found then
        Found := RegReadStr(DelphiRegKey[DelphiComp], 'App', FileName,
          HKEY_LOCAL_MACHINE) and FileExists(FileName);
    end;

    if Found then
    begin
      item := TDelphiObjectData.Create;
      with item do
      begin
        Name := DelphiVersionsNames[DelphiComp];
        Path := FileName;
        Version := DelphiComp;
        Icon := TIcon.Create;
        Icon.LoadFromFile(FileName);
      end;
      Result.Add(item);

      //item := ListViewIDEs.Items.Add;
      //item.Caption := DelphiVersionsNames[DelphiComp];
      //item.SubItems.Add(FileName);
      //ExtractIconFileToImageList(ImageList1, Filename);
      //ImageIndex := ImageList1.Count - 1;
      //item.ImageIndex := ImageIndex;
    end;
  end;
end;

procedure FillCurrentDelphiVersion(Data: TDelphiObjectData);
var
  List: TList;
  LData: TDelphiObjectData;
  s: string;
begin
  s := ParamStr(0);
  List := TList.Create;
  try
    FillListDelphiVersions(List);
    //for LData in List do
      if SameText(LData.Path, s) then
      begin
        Data.FVersion := LData.Version;
        Data.Path := LData.Path;
        Data.Name := LData.Name;
        if (Data.Icon = nil) then
          Data.Icon := TIcon.Create;
        Data.Icon.Assign(LData.Icon);
        Data.FRegKey := LData.FRegKey;
        //break;
      end;
  finally
    List.Free;
  end;
end;

procedure FillListDelphiVersions(AList: TList);
type
  TBDSKeysItem = record
    MinValue, MaxValue: integer;
    Key: string;
  end;
const
  MaxBDSKeysItem = 4;

var
  VersionData: TDelphiObjectData;
  DelphiComp: TDelphiVersions;
  LKey, FileName: string;
  Found: boolean;
  RootKey: HKEY;
  BDSKeys: TStrings;
  i, j: integer;
  BDSKeysItems: array [0 .. MaxBDSKeysItem - 1] of TBDSKeysItem;
begin
  BDSKeys := TStringList.Create;
  try
    BDSKeysItems[0].MinValue := 7;
    BDSKeysItems[0].MaxValue := 7;
    BDSKeysItems[0].Key := '\Software\Borland';

    BDSKeysItems[1].MinValue := 2;
    BDSKeysItems[1].MaxValue := 5;
    BDSKeysItems[1].Key := '\Software\Borland';

    BDSKeysItems[2].MinValue := 6;
    BDSKeysItems[2].MaxValue := 7;
    BDSKeysItems[2].Key := '\Software\CodeGear';

    BDSKeysItems[3].MinValue := DelphiRegPathNumbers[DelphiXE];
    BDSKeysItems[3].MaxValue := DelphiRegPathNumbers[Delphi11Alexandria];
    BDSKeysItems[3].Key := '\Software\Embarcadero';

    for j := 0 to MaxBDSKeysItem - 1 do
    begin
      BDSKeys.Clear;
      ScanRootKey(BDSKeysItems[j].Key, BDSKeys, BDSKeysItems[j].MinValue,
        BDSKeysItems[j].MaxValue);

      for i := 0 to BDSKeys.Count - 1 do
      begin
        for DelphiComp := Low(TDelphiVersions) to High(TDelphiVersions) do
          if (DelphiRegPathNumbers[DelphiComp] >= BDSKeysItems[j].MinValue) and
            (DelphiRegPathNumbers[DelphiComp] <= BDSKeysItems[j].MaxValue) then
          begin
            RootKey := HKEY_CURRENT_USER;
            // LKey    := DelphiRegPaths[DelphiComp];
            LKey := Format(DelphiCustomRegPaths[DelphiComp], [BDSKeys[i]]);
            Found := RegKeyExists(LKey, RootKey);

            FileName := '';

            if Found then
              Found := RegReadStr(LKey, 'App', FileName, RootKey) and
                FileExists(FileName);

            if (DelphiComp >= DelphiXE6) and not Found then
            begin
              FileName := StringReplace(FileName, 'bds.exe', 'appmethod.exe',
                [rfReplaceAll]);
              Found := FileExists(FileName);
            end;

            if not Found then
            begin
              RootKey := HKEY_LOCAL_MACHINE;
              Found := RegKeyExists(LKey, RootKey);
              if Found then
                Found := RegReadStr(LKey, 'App', FileName, RootKey) and
                  FileExists(FileName);
            end;

            if Found then
            begin
              VersionData := TDelphiObjectData.Create;
              VersionData.FPath := FileName;
              VersionData.FRegKey := LKey;
              VersionData.FVersion := DelphiComp;
              VersionData.FName := DelphiVersionsNames[DelphiComp];
              if not SameText(BDSKeys[i], 'BDS') then
                VersionData.FName :=
                  DelphiVersionsNames[DelphiComp] + ' (' + BDSKeys[i] + ')';

              VersionData.Icon := TIcon.Create;
              AList.Add(VersionData);
            end;
          end;
      end;
    end;
  finally
    BDSKeys.Free;
  end;
end;

{ TDelphiObjectData }

constructor TDelphiObjectData.Create;
begin
  inherited;
  FIcon := nil;
end;

destructor TDelphiObjectData.Destroy;
begin
  FreeAndNil(FIcon);
  inherited;
end;

end.
