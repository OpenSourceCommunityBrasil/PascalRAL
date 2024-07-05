unit frideversion_lazarus;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, frideversions,
  FileInfo, process, install_types;

type

  { Tfframe_ide_version_lazarus }

  Tfframe_ide_version_lazarus = class(Tfframe_ide_versions)
    prcBuildIDE: TProcess;
  private
    FLazExecFile : string;
    FLazBuildFile : string;
    FPackages : TStringList;
  protected
    procedure SetPath(AValue: string); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure addPackage(APack : string);
    procedure buildIDE;
  end;

implementation

{$R *.lfm}

{ Tfframe_ide_version_lazarus }

procedure Tfframe_ide_version_lazarus.SetPath(AValue: string);
var
  FileVerInfo: TFileVersionInfo;
begin
  inherited SetPath(AValue);

  FLazExecFile := ExtractFileDir(AValue) + PathDelim + LazExecFile;
  FLazBuildFile := ExtractFileDir(AValue) + PathDelim + LazBuildFile;

  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName := FLazExecFile;
    FileVerInfo.ReadFileInfo;

    lbName.Caption := FileVerInfo.VersionStrings.Values['ProductName'] + ' ' +
                      FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

constructor Tfframe_ide_version_lazarus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPackages := TStringList.Create;
end;

destructor Tfframe_ide_version_lazarus.Destroy;
begin
  FreeAndNil(FPackages);
  inherited Destroy;
end;

procedure Tfframe_ide_version_lazarus.addPackage(APack: string);
begin
  FPackages.Add(APack);
end;

procedure Tfframe_ide_version_lazarus.buildIDE;
var
  vInt : integer;
begin
  prcBuildIDE.Executable := FLazBuildFile;
  prcBuildIDE.Parameters.Add('--build-ide=');

  if FPackages.Count > 0 then
    prcBuildIDE.Parameters.Add('--add-package');

  for vInt := 0 to Pred(FPackages.Count) do
    prcBuildIDE.Parameters.Add(FPackages.Strings[vInt]);

  prcBuildIDE.Execute;

  ShowMessage('Pronto');
end;

initialization
  RegisterClass(Tfframe_ide_version_lazarus);

end.

