unit frconfigrecursos;

{$mode ObjFPC}{$H+}

interface

uses
  {$IF Defined(Windows)}
    winpeimagereader,
  {$ELSEIF Defined(Linux)}
    elfreader,
  {$ELSEIF Defined(Darwin)}
    machoreader,
  {$IFEND}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  frmodelo, frideversions, install_types, lclfunctions,
  fileinfo, ghrepofunctions;

type

  { Tfconfigrecursos }

  Tfconfigrecursos = class(Tfmodelo)
    ePathFolder: TEdit;
    ImageList1: TImageList;
    lLatestVersion: TLabel;
    lPathIDEVersion: TLabel;
    lRepoVersion: TLabel;
    lVersionNotes: TLabel;
    lVersionNotes1: TLabel;
    mmRepoNotes: TMemo;
    sbInstalledIDEs: TScrollBox;
    bAddLazPath: TSpeedButton;
    bFindLazPaths: TSpeedButton;
    DirDialog: TSelectDirectoryDialog;
    sbChoosePath: TSpeedButton;
    procedure bAddLazPathClick(Sender: TObject);
    procedure bFindLazPathsClick(Sender: TObject);
    procedure lbBackClick(Sender: TObject);
    procedure lbNextClick(Sender: TObject);
    procedure sbChoosePathClick(Sender: TObject);
  private
    FTotThread: integer;
    procedure schFileFound(Sender: TObject; AFound: string);
    procedure schTerminate(Sender: TObject);
    function CountIDESelect: integer;
    procedure ClearIDEVersion;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure validaControls; override;
    procedure doShow(Sender: TObject);
  end;

implementation

{$R *.lfm}

uses principal;

{ Tfconfigrecursos }

procedure Tfconfigrecursos.bFindLazPathsClick(Sender: TObject);
var
  vSearch: TFileSearch;
  vDrivers: TStringList;
  vInt: integer;
begin
  with TTaskDialog.Create(self) do
    try
      Title := 'Confirma varredura';
      Caption := 'RAL Installer';
      Text := 'Esse processo demora bastante pra ser concluído, tem certeza que quer' +
        ' continuar?';
      CommonButtons := [];
      with TTaskDialogButtonItem(Buttons.Add) do
      begin
        Caption := 'Buscar';
        ModalResult := mrYes;
      end;
      with TTaskDialogButtonItem(Buttons.Add) do
      begin
        Caption := 'Cancelar';
        ModalResult := mrNo;
      end;
      MainIcon := tdiWarning;
      if Execute then
        if ModalResult = mrYes then
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
                vSearch := TFileSearch.Create(vDrivers.Names[vInt], LazBuildFile);
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
    finally
      Free;
    end;
end;

procedure Tfconfigrecursos.bAddLazPathClick(Sender: TObject);
var
  vSearch: TFileSearch;
begin
  if DirDialog.Execute then
  begin
    vSearch := TFileSearch.Create(DirDialog.FileName, LazBuildFile);
    vSearch.OnFileFound := @schFileFound;
    vSearch.Resume;
  end;
end;

procedure Tfconfigrecursos.lbBackClick(Sender: TObject);
begin
  if not lbBack.Enabled then
    Exit;

  ClearIDEVersion;

  inherited;
end;

procedure Tfconfigrecursos.lbNextClick(Sender: TObject);
begin
  if not lbNext.Enabled then
    Exit;

  if (CountIDESelect = 0) then
  begin
    ShowMessage('Selecione um IDE para instalar');
    Exit;
  end;

  inherited;
end;

procedure Tfconfigrecursos.sbChoosePathClick(Sender: TObject);
begin
  if DirDialog.Execute then
    ePathFolder.Text := DirDialog.FileName;
end;

procedure Tfconfigrecursos.schFileFound(Sender: TObject; AFound: string);
var
  vFrame: Tfframe_ide_versions;
  I: integer;
  pathexists: boolean;
  FileVerInfo: TFileVersionInfo;
begin
  pathexists := False;
  for I := 0 to pred(sbInstalledIDEs.ControlCount) do
    if sbInstalledIDEs.Controls[I].InheritsFrom(Tfframe_ide_versions) then
    begin
      pathexists := Tfframe_ide_versions(sbInstalledIDEs.Controls[I]).lbPath.Caption = aFound;
      if pathexists then Break;
    end;

  if not pathexists then
  begin
    vFrame := Tfframe_ide_versions.Create(Self);
    vFrame.Parent := sbInstalledIDEs;
    //vFrame.Top := 30000;
    vFrame.Align := alTop;
    vFrame.lbPath.Caption := AFound;

    FileVerInfo := TFileVersionInfo.Create(nil);
    try
      FileVerInfo.FileName := ExtractFileDir(aFound) + '\' + LazExecFile;
      FileVerInfo.ReadFileInfo;
      vFrame.lbName.Caption :=
        FileVerInfo.VersionStrings.Values['ProductName'] + ' ' +
        FileVerInfo.VersionStrings.Values['FileVersion'];
      vFrame.Name := 'frmVersion' + FormatDateTime('ddmmyyyyhhnnsszzz', Now);
    finally
      FileVerInfo.Free;
    end;
  end;
end;

procedure Tfconfigrecursos.schTerminate(Sender: TObject);
begin
  if FTotThread > 0 then
    FTotThread := FTotThread - 1;
  validaControls;
end;

function Tfconfigrecursos.CountIDESelect: integer;
var
  vInt: integer;
begin
  Result := 0;
  for vInt := 0 to Pred(sbInstalledIDEs.ControlCount) do
  begin
    if (sbInstalledIDEs.Controls[vInt] is Tfframe_ide_versions) and
      (Tfframe_ide_versions(sbInstalledIDEs.Controls[vInt]).ckSelected.Checked) then
      Result := Result + 1;
  end;
end;

procedure Tfconfigrecursos.ClearIDEVersion;
var
  I: integer;
begin
  for I := pred(sbInstalledIDEs.ControlCount) downto 0 do
    sbInstalledIDEs.Controls[I].Free;
end;

constructor Tfconfigrecursos.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IdTela := 3;
  FTotThread := 0;
end;

procedure Tfconfigrecursos.validaControls;
begin
  inherited validaControls;
  bAddLazPath.Visible := fprincipal.GetIDESelected = 0;
  bFindLazPaths.Visible := fprincipal.GetIDESelected = 0;
  lbNext.Enabled := FTotThread = 0;
  lbBack.Enabled := FTotThread = 0;
  bAddLazPath.Enabled := FTotThread = 0;
  bFindLazPaths.Enabled := FTotThread = 0;
end;

procedure Tfconfigrecursos.doShow(Sender: TObject);
var
  Client: TRESTClient;
  LRelease: TRelease;
begin
  Client := TRESTClient.Create;
  try
    LRelease := Client.getLatestRelease;
    lRepoVersion.Caption := LRelease.Version;
    mmRepoNotes.Lines.Text := LRelease.Notes;
  finally
    LRelease.Free;
    Client.Free;
  end;
end;

end.
