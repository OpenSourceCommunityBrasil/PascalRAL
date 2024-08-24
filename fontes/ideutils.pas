unit ideutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, Forms, process,
  utools, installparser;

type
  TOnIDEFind = procedure(APath: String; var ACancel : boolean) of object;

  { TIDEObjectData }

  TIDEObjectData = class
  private
    FVersion: string;
    FName: string;
    FBuildFile: string;
    FExeFile: string;
    FIcon: TGraphic;
    FProcessBuild: TProcess;
  protected
    procedure SetExeFile(AValue: string); virtual;

    function logar(ALog : TMemo; AStr : string) : boolean;

    procedure Install(ALog: TMemo; AInstaller: TInstaller; APathDownload: string); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure InstallRAL(ALog: TMemo; AInstaller: TInstaller; APathDownload: string);

    property ProcessBuild : TProcess read FProcessBuild;
  published
    property Version: string read FVersion write FVersion;
    property BuildFile: string read FBuildFile write FBuildFile;
    property ExeFile: string read FExeFile write SetExeFile;
    property Name: string read FName write FName;
    property Icon: TGraphic read FIcon write FIcon;
  end;

  { TIDEObjectList }

  TIDEObjectList = class(TList)
  public
    procedure Clear; override;
  end;

  { TIDEFinder }

  TIDEFinder = class
  private
    FList : TIDEObjectList;
    FOnIDEFind : TOnIDEFind;
  protected
    function GetObjectData(AIndex : integer): TIDEObjectData;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BuscarIDE; virtual; abstract;
    function Count : integer;

    property List : TIDEObjectList read FList;
    property ObjectData[AIndex : integer] : TIDEObjectData read GetObjectData;
  published
    property OnIDEFind : TOnIDEFind read FOnIDEFind write FOnIDEFind;
  end;

implementation

{ TIDEObjectData }

procedure TIDEObjectData.SetExeFile(AValue: string);
begin
  if FExeFile = AValue then
    Exit;

  FExeFile := AValue;
  FIcon := GetIconExeFile(FExeFile);
end;

function TIDEObjectData.logar(ALog: TMemo; AStr: string): boolean;
begin
  Result := ALog <> nil;
  if Result then
  begin
    ALog.Lines.Add(AStr);
    Application.ProcessMessages;
  end;
end;

constructor TIDEObjectData.Create;
begin
  inherited;
  FProcessBuild := TProcess.Create(nil);
  FProcessBuild.Options := [poWaitOnExit, poNewConsole];
  FProcessBuild.ShowWindow := swoHIDE;
end;

destructor TIDEObjectData.Destroy;
begin
  FreeAndNil(FProcessBuild);
  FreeAndNil(FIcon);

  inherited Destroy;
end;

procedure TIDEObjectData.InstallRAL(ALog: TMemo; AInstaller: TInstaller; APathDownload: string);
begin
  logar(ALog, 'Iniciando instalação em: ' + FName);

  Install(ALog, AInstaller, APathDownload);

  logar(ALog, 'Finalizado instalação em: ' + FName);
  logar(ALog, '');
end;

{ TIDEObjectList }

procedure TIDEObjectList.Clear;
begin
//  while Self.Count > 0 do begin
//    TObject(Self.Items[Self.Count - 1]).Free;
//    Self.Delete(Self.Count - 1);
//  end;
  inherited Clear;
end;

{ TIDEFinder }

function TIDEFinder.GetObjectData(AIndex: integer): TIDEObjectData;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FList.Count) then
    Result := TIDEObjectData(FList.Items[AIndex]);
end;

constructor TIDEFinder.Create;
begin
  inherited;
  FList := TIDEObjectList.Create;
end;

destructor TIDEFinder.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TIDEFinder.Count: integer;
begin
  Result := FList.Count;
end;

end.

