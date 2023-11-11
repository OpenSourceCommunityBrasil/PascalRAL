unit lclfunctions;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF Windows}
    Windows,
  {$ENDIF}
  Classes, SysUtils, StdCtrls, ExtCtrls, CheckLst, Controls;

type
  TOnAnyFound = procedure(Sender: TObject; AFound: string) of object;

  { TLCLFunctions }

  TLCLFunctions = class
  private
    procedure SetControlState(Args: array of TComponent; EnabledState: boolean);
    procedure SetControlVisibility(Args: array of TComponent; VisibleState: boolean);
  public
    procedure DesativaControles(Args: array of TComponent);
    procedure AtivaControles(Args: array of TComponent);
    procedure EscondeControles(Args: array of TComponent);
    procedure MostraControles(Args: array of TComponent);
  end;

  { TFileSearch }

  TFileSearch = class(TThread)
  private
    FFind: string;
    FDirectory: string;
    FFileName: string;
    FOnFileFound: TOnAnyFound;
    FInitialDirectory: string;
  public
    constructor Create(ADirectory, AFileName: string);
  protected
    procedure Execute; override;
    procedure DoFileFound;
    procedure Search(ADirectory: string);
  published
    property OnFileFound: TOnAnyFound read FOnFileFound write FOnFileFound;
  end;

procedure ListDrivers(aDriverList: TStringList);

var
  LCLFunc: TLCLFunctions;

implementation

procedure ListDrivers(aDriverList: TStringList);
{$IFDEF Windows}
var
  OldMode: word;
  Drive: char;
  DriveLetter: string;
begin
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    // Search all drive letters
    for Drive := 'A' to 'Z' do
    begin
      DriveLetter := Drive + ':\';
      case GetDriveType(PChar(DriveLetter)) of
        DRIVE_REMOVABLE: aDriverList.AddPair(DriveLetter, ' Floppy Drive');
        DRIVE_FIXED: aDriverList.AddPair(DriveLetter, ' Fixed Drive');
        DRIVE_REMOTE: aDriverList.AddPair(DriveLetter, ' Network Drive');
        DRIVE_CDROM: aDriverList.AddPair(DriveLetter, ' CD-ROM Drive');
        DRIVE_RAMDISK: aDriverList.AddPair(DriveLetter, ' RAM Disk');
      end;
    end;

  finally
    // Restores previous Windows error mode.
    SetErrorMode(OldMode);
  end;
end;
{$ELSE}
begin

end;

{$IFEND}

{ TFileSearch }

constructor TFileSearch.Create(ADirectory, AFileName: string);
begin
  FInitialDirectory := ADirectory;
  FDirectory := ADirectory;
  FFileName := AFileName;
  FreeOnTerminate := True;
  inherited Create(True);
end;

procedure TFileSearch.Execute;
begin
  Search(FDirectory);
end;

procedure TFileSearch.DoFileFound;
begin
  if Assigned(FOnFileFound) then
    FOnFileFound(Self, FFind);
end;

procedure TFileSearch.Search(ADirectory: string);
var
  vStatus: integer;
  vFile: TSearchRec;

  procedure FindFile;
  var
    vFileDir: TSearchRec;
    vStatDir: integer;
  begin
    vStatDir := FindFirst(ADirectory + FFileName, faArchive, vFileDir);
    try
      if vStatDir = 0 then
      begin
        FFind := ADirectory + vFileDir.Name;
        Synchronize(@DoFileFound);
      end;
    finally
      FindClose(vFileDir);
    end;
  end;

begin
  ADirectory := IncludeTrailingPathDelimiter(ADirectory);
  FindFile;
  // buscando somente os diretórios
  vStatus := FindFirst(ADirectory + '*.', faDirectory, vFile);
  try
    while vStatus = 0 do
    begin
      if (vFile.Name <> '.') and (vFile.Name <> '..') and (Trim(vFile.Name) <> '') then
      begin
        FFind := ADirectory + vFile.Name;
        if not Terminated then
          Search(FFind);
      end;
      vStatus := FindNext(vFile);
    end;
  finally
    FindClose(vFile);
  end;
end;

{ TLCLFunctions }

procedure TLCLFunctions.SetControlState(Args: array of TComponent;
  EnabledState: boolean);
var
  Component: TComponent;
begin
  for Component in Args do
    if Component is TLabel then
      TLabel(Component).Enabled := EnabledState
    else if Component is TCheckGroup then
      TCheckGroup(Component).Enabled := EnabledState
    else if Component is TCheckListBox then
      TCheckListBox(Component).Enabled := EnabledState
    else if Component is TShape then
      TShape(Component).Enabled := EnabledState;
end;

procedure TLCLFunctions.SetControlVisibility(Args: array of TComponent;
  VisibleState: boolean);
var
  Component: TComponent;
begin
  for Component in Args do
    if Component.InheritsFrom(TControl) then
      TControl(Component).Visible := VisibleState;
end;

procedure TLCLFunctions.DesativaControles(Args: array of TComponent);
begin
  SetControlState(Args, False);
end;

procedure TLCLFunctions.AtivaControles(Args: array of TComponent);
begin
  SetControlState(Args, True);
end;

procedure TLCLFunctions.EscondeControles(Args: array of TComponent);
begin
  SetControlVisibility(Args, False);
end;

procedure TLCLFunctions.MostraControles(Args: array of TComponent);
begin
  SetControlVisibility(Args, True);
end;

end.
