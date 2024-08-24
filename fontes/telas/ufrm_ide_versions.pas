unit ufrm_ide_versions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ufrm_modelo, ufrm_ide_version, installparser;

type

  { Tfrm_ide_versions }

  Tfrm_ide_versions = class(Tfrm_modelo)
    dirSelect: TSelectDirectoryDialog;
    lbFind: TLabel;
    bAutoBusca: TSpeedButton;
    lbFind1: TLabel;
    lbSubTitle: TLabel;
    sbIDEVersions: TScrollBox;
    bAddVersion: TSpeedButton;
    bStopBusca: TSpeedButton;
    procedure bAddVersionClick(Sender: TObject);
    procedure bAutoBuscaClick(Sender: TObject);
    procedure bStopBuscaClick(Sender: TObject);
  private
    FTop: integer;
    FCancelBusca: boolean;
    FFilesFind: integer;

    procedure clearVersions;
    procedure showDelphiVersions;

    function BuscarFrameInstall(APath: string): Tfrm_ide_version;

    procedure OnIDEFind(APath: string; var ACancel: boolean);
  protected
    procedure SetIDE(AValue: integer); override;
    function validatePageNext : boolean; override;
    function validatePagePrior : boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure installRAL(ALog: TMemo; AInstaller: TInstaller; APathDownload: string);
  end;

implementation

{$R *.lfm}

uses
  udm, delphiutils, lazarusutils, ideutils;

{ Tfrm_ide_versions }

procedure Tfrm_ide_versions.bAutoBuscaClick(Sender: TObject);
var
  vList: TLazarusFinder;
  vInt: Integer;
  vItem: TIDEObjectData;
  vFrm: Tfrm_ide_version;
begin
  FFilesFind := 0;
  bAutoBusca.Enabled := False;
  bAddVersion.Enabled := False;
  bStopBusca.Visible := True;
  FCancelBusca := False;

  vList := TLazarusFinder.Create;
  try
    vList.OnIDEFind := @OnIDEFind;
    vList.BuscarIDE;

    for vInt := 0 to Pred(vList.Count) do
    begin
      vItem := vList.ObjectData[vInt];
      vFrm := BuscarFrameInstall(vItem.ExeFile);
      if vFrm = nil then
      begin
        vFrm := Tfrm_ide_version.Create(Self, vItem);
        vFrm.Name := 'ide_version_' + IntToStr(vInt);
        vFrm.Parent := sbIDEVersions;
        vFrm.Top := FTop;

        Application.ProcessMessages;

        vFrm.Align := alTop;

        FTop := FTop + vFrm.Height;
      end;
    end;
  finally
    FreeAndNil(vList);

    bAutoBusca.Enabled := True;
    bAddVersion.Enabled := True;
    bStopBusca.Visible := False;
    lbFind.Caption := '';
  end;
end;

procedure Tfrm_ide_versions.bStopBuscaClick(Sender: TObject);
begin
  FCancelBusca := True;
end;

procedure Tfrm_ide_versions.bAddVersionClick(Sender: TObject);
var
  vPasta: string;
  vFrm: Tfrm_ide_version;
  vObj: TLazarusObjectData;
begin
  if dirSelect.Execute then begin
    vPasta := IncludeTrailingPathDelimiter(dirSelect.FileName);

    if FileExists(vPasta + LazBuildFile) and
       FileExists(vPasta + LazExecFile) then
    begin
      vObj := TLazarusObjectData.Create;
      vObj.ExeFile := vPasta + LazExecFile;
      vObj.BuildFile := vPasta + LazBuildFile;

      vFrm := BuscarFrameInstall(vObj.ExeFile);
      if vFrm = nil then
      begin
        vFrm := Tfrm_ide_version.Create(Self, vObj);
        vFrm.Name := 'ide_version_' + FormatDateTime('ddmmyyyyhhnnss', Now);
        vFrm.Parent := sbIDEVersions;
        vFrm.Top := FTop;

        Application.ProcessMessages;

        vFrm.Align := alTop;

        FTop := FTop + vFrm.Height;
      end;
    end;
  end;
end;

procedure Tfrm_ide_versions.clearVersions;
var
  vInt: Integer;
begin
  for vInt := Pred(sbIDEVersions.ControlCount) downto 0 do
  begin
    if sbIDEVersions.Controls[vInt] is Tfrm_ide_version then
      sbIDEVersions.Controls[vInt].Free;
  end;
  FTop := 0;
end;

procedure Tfrm_ide_versions.showDelphiVersions;
var
  vList: TDelphiFinder;
  vInt: Integer;
  vItem: TIDEObjectData;
  vFrm: Tfrm_ide_version;
begin
  vList := TDelphiFinder.Create;
  try
    vList.OnIDEFind := @OnIDEFind;
    vList.BuscarIDE;

    for vInt := 0 to Pred(vList.Count) do
    begin
      vItem := vList.ObjectData[vInt];

      // produto desinstalado
      if vItem.ExeFile = '' then
      begin
        FreeAndNil(vItem);
        Continue;
      end;

      vFrm := Tfrm_ide_version.Create(Self, vItem);
      vFrm.Name := 'ide_version_' + IntToStr(vInt);
      vFrm.Parent := sbIDEVersions;
      vFrm.Top := FTop;

      Application.ProcessMessages;

      vFrm.Align := alTop;

      FTop := FTop + vFrm.Height;
    end;
  finally
    FreeAndNil(vList);
  end;
end;

function Tfrm_ide_versions.BuscarFrameInstall(APath: string): Tfrm_ide_version;
var
  vInt: Integer;
  vFrm: Tfrm_ide_version;
begin
  Result := nil;
  for vInt := 0 to Pred(sbIDEVersions.ControlCount) do
  begin
    if sbIDEVersions.Controls[vInt] is Tfrm_ide_version then
    begin
      vFrm := Tfrm_ide_version(sbIDEVersions.Controls[vInt]);
      if ExtractFilePath(vFrm.ObjectData.ExeFile) = APath then
      begin
        Result := vFrm;
        Break;
      end;
    end;
  end;
  FTop := 0;
end;

procedure Tfrm_ide_versions.OnIDEFind(APath: string; var ACancel: boolean);

  function CortePath(APasta: string) : string;
  var
    vPos, vIni: integer;
  begin
    if Length(APasta) > 90 then
    begin
      vIni := 0;
      APasta := ExcludeTrailingPathDelimiter(APasta);
      repeat
        vPos := Pos(PathDelim, APasta, vIni + 1);
        if vPos > 0 then
          vIni := vPos;
      until (Length(Copy(APasta, vPos, Length(APath))) <= 87) or (vPos = 0);
      if vPos = 0 then
        vPos := vIni;
      Result := '...' + Copy(APasta, vPos, Length(APasta));
      Result := IncludeTrailingPathDelimiter(Result);
    end
    else
    begin
      Result := APasta;
    end;
  end;
begin
  ACancel := FCancelBusca;
  FFilesFind := FFilesFind + 1;
  if FFilesFind = 1000 then
  begin
    lbFind.Caption := CortePath(APath);
    Application.ProcessMessages;
    FFilesFind := 0;
  end;
end;

procedure Tfrm_ide_versions.SetIDE(AValue: integer);
begin
  // 0 - Delphi
  // 1 - Lazarus

  bAddVersion.Visible := AValue = 1;
  bAutoBusca.Visible := AValue = 1;

  if IDE <> AValue then
    clearVersions;

  if (IDE <> AValue) and (AValue = 0) then
    showDelphiVersions;

  inherited SetIDE(AValue);
end;

function Tfrm_ide_versions.validatePageNext: boolean;
begin
  Result := not bStopBusca.Visible;
end;

function Tfrm_ide_versions.validatePagePrior: boolean;
begin
  Result := not bStopBusca.Visible;
end;

constructor Tfrm_ide_versions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  lbFind.Caption := '';
  FFilesFind := 0;
  FCancelBusca := False;
end;

procedure Tfrm_ide_versions.installRAL(ALog: TMemo; AInstaller: TInstaller; APathDownload : string);
var
  vInt: Integer;
  vFrm: Tfrm_ide_version;
begin
  for vInt := 0 to Pred(sbIDEVersions.ControlCount) do
  begin
    if sbIDEVersions.Controls[vInt] is Tfrm_ide_version then
    begin
      vFrm := Tfrm_ide_version(sbIDEVersions.Controls[vInt]);
      vFrm.installRAL(ALog, AInstaller, APathDownload);
    end;
  end;
end;

end.

