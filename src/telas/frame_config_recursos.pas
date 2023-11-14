unit frame_config_recursos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, EditBtn, frame_modelo, frame_ide_versions;

type

  { Tfframe_config_recursos }

  Tfframe_config_recursos = class(Tfframe_modelo)
    ePathFolder : TDirectoryEdit;
    lLatestVersion : TLabel;
    lPathIDEVersion : TLabel;
    lRepoVersion : TLabel;
    lVersionNotes : TLabel;
    lVersionNotes1 : TLabel;
    mmRepoNotes : TMemo;
    sbVersion : TScrollBox;
    bAddLazPath : TSpeedButton;
    bFindLazPaths : TSpeedButton;
    diagDir : TSelectDirectoryDialog;
    procedure bFindLazPathsClick(Sender : TObject);
    procedure lbBackClick(Sender : TObject);
    procedure lbNextClick(Sender : TObject);
  private
    FTotThread : integer;
    procedure schFileFound(Sender : TObject; AFound : string);
    procedure schTerminate(Sender : TObject);
    function CountIDESelect : integer;
    procedure ClearIDEVersion;
  public
    constructor Create(TheOwner : TComponent); override;
    procedure validaControls; override;
  end;

implementation

{$R *.lfm}

uses
  principal, lclfunctions;

{ Tfframe_config_recursos }

procedure Tfframe_config_recursos.bFindLazPathsClick(Sender : TObject);
var
  vSearch : TFileSearch;
  vDrivers : TStringList;
  vInt : integer;
begin
  vDrivers := TStringList.Create;

  bAddLazPath.Enabled := False;
  bFindLazPaths.Enabled := False;
  lbBack.Enabled := False;
  lbNext.Enabled := False;

  try
    try
      ListDrivers(vDrivers);
      for vInt := 0 to Pred(vDrivers.Count) do
      begin
        vSearch := TFileSearch.Create(vDrivers.Names[vInt], 'lazarus.exe');
        vSearch.OnFileFound := @schFileFound;
        vSearch.OnTerminate := @schTerminate;
        vSearch.Resume;

        FTotThread := FTotThread + 1;
      end;
    except
      if FTotThread = 0 then
      begin
        bAddLazPath.Enabled := True;
        bFindLazPaths.Enabled := True;
        lbBack.Enabled := True;
        lbNext.Enabled := True;
      end;
    end;
  finally
    FreeAndNil(vDrivers);
  end;
end;

procedure Tfframe_config_recursos.lbBackClick(Sender : TObject);
begin
  if not lbBack.Enabled then
    Exit;

  ClearIDEVersion;

  inherited;
end;

procedure Tfframe_config_recursos.lbNextClick(Sender : TObject);
begin
  if not lbNext.Enabled then
    Exit;

  if (CountIDESelect = 0) then begin
    ShowMessage('Selecione um IDE para instalar');
    Exit;
  end;

  inherited;
end;

procedure Tfframe_config_recursos.schFileFound(Sender : TObject; AFound : string);
var
  vFrame : Tfframe_ide_versions;
begin
  vFrame := Tfframe_ide_versions.Create(Self);
  vFrame.Parent := sbVersion;
  vFrame.Top := 30000;
  vFrame.Align := alTop;
  vFrame.lbPath.Caption := AFound;
  vFrame.Name := 'frmVersion'+FormatDateTime('ddmmyyyyhhnnsszzz',Now);
end;

procedure Tfframe_config_recursos.schTerminate(Sender : TObject);
begin
  if FTotThread > 0 then
    FTotThread := FTotThread - 1;
  validaControls;
end;

function Tfframe_config_recursos.CountIDESelect : integer;
var
  vInt : integer;
begin
  Result := 0;
  for vInt := 0 to Pred(sbVersion.ControlCount) do
  begin
    if (sbVersion.Controls[vInt] is Tfframe_ide_versions) and
       (Tfframe_ide_versions(sbVersion.Controls[vInt]).ckSelected.Checked) then
      Result := Result + 1;
  end;
end;

procedure Tfframe_config_recursos.ClearIDEVersion;
var
  vInt : integer;
begin
  vInt := sbVersion.ControlCount - 1;
  while vInt >= 0 do
  begin
    if (sbVersion.Controls[vInt] is Tfframe_ide_versions) then
      Tfframe_ide_versions(sbVersion.Controls[vInt]).Free;
    vInt := vInt - 1;
  end;
end;

constructor Tfframe_config_recursos.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
  IdTela := 3;
  FTotThread := 0;
end;

procedure Tfframe_config_recursos.validaControls;
begin
  inherited validaControls;
  bAddLazPath.Visible := fprincipal.GetIDESelected = 0;
  bFindLazPaths.Visible := fprincipal.GetIDESelected = 0;
  lbNext.Enabled := FTotThread = 0;
  lbBack.Enabled := FTotThread = 0;
  bAddLazPath.Enabled := FTotThread = 0;
  bFindLazPaths.Enabled := FTotThread = 0;
end;

end.

