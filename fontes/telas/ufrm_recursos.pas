unit ufrm_recursos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Buttons, Menus, ufrm_modelo, installparser, LCLType;

type

  { Tfrm_recursos }

  Tfrm_recursos = class(Tfrm_modelo)
    bAddVersion: TSpeedButton;
    dirSelect: TSelectDirectoryDialog;
    ePathDownload: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbSubTitle: TLabel;
    tvRecursos: TTreeView;
    procedure bAddVersionClick(Sender: TObject);
    procedure tvRecursosDblClick(Sender: TObject);
    procedure tvRecursosKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FInstall : TInstaller;
    function GetPathDownload: string;
  protected
    procedure SetIDE(AValue: integer); override;
    procedure listInstall(AIDE : integer);
    function validatePageNext : boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Install: TInstaller read FInstall;
    property PathDownload : string read GetPathDownload;
  end;

implementation

{$R *.lfm}

uses
  udm;

{ Tfrm_recursos }

procedure Tfrm_recursos.bAddVersionClick(Sender: TObject);
begin
  if dirSelect.Execute then
    ePathDownload.Text := dirSelect.FileName;
end;

procedure Tfrm_recursos.tvRecursosDblClick(Sender: TObject);
var
  vNode: TTreeNode;
  vObj: TSelected;
begin
  vNode := tvRecursos.Selected;
  if vNode = nil then
    Exit;

  vObj := TSelected(vNode.Data);
  if vObj = nil then
    Exit;

  vObj.Selected := not vObj.Selected;

  if vObj.Selected then
  begin
    vNode.ImageIndex := 3;
    vNode.SelectedIndex := 3;
  end
  else
  begin
    vNode.ImageIndex := 4;
    vNode.SelectedIndex := 4;
  end;

  tvRecursos.FullExpand;
end;

procedure Tfrm_recursos.tvRecursosKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  vNode: TTreeNode;
  vObj: TSelected;
begin
  vNode := tvRecursos.Selected;
  if vNode = nil then
    Exit;

  vObj := TSelected(vNode.Data);
  if vObj = nil then
    Exit;

  if Key = VK_INSERT then
    vObj.Selected := True
  else if Key = VK_DELETE then
    vObj.Selected := False;

  if vObj.Selected then
  begin
    vNode.ImageIndex := 3;
    vNode.SelectedIndex := 3;
  end
  else
  begin
    vNode.ImageIndex := 4;
    vNode.SelectedIndex := 4;
  end;

  tvRecursos.FullExpand;
end;

function Tfrm_recursos.GetPathDownload: string;
begin
  Result := ePathDownload.Text;
end;

procedure Tfrm_recursos.SetIDE(AValue: integer);
begin
  if AValue <> IDE then
    listInstall(AValue);
  inherited SetIDE(AValue);
end;

procedure Tfrm_recursos.listInstall(AIDE: integer);
var
  vNode, vNodeAux1, vNodeAux2 : TTreeNode;
  vPascalRAL : TTreeNode;
  vInt: integer;
begin
  tvRecursos.Items.Clear;

  vNode := tvRecursos.Items.Add(nil, 'Packages');

  vPascalRAL := tvRecursos.Items.AddChild(vNode, FInstall.Packages.Name);
  vPascalRAL.Data := FInstall.Packages;
  vPascalRAL.SelectedIndex := 3;
  vPascalRAL.ImageIndex := 3;

  vNodeAux2 := tvRecursos.Items.AddChild(vPascalRAL, 'Engines');

  for vInt := 0 to Pred(FInstall.Packages.Engines.Count) do
  begin
    if ((FInstall.Packages.Engines.Engine[vInt].Install.DPK.Count = 0) and (AIDE = 0)) or
       (FInstall.Packages.Engines.Engine[vInt].Install.LPK = '') and (AIDE = 1) then
      Continue;
    vNodeAux1 := tvRecursos.Items.AddChild(vNodeAux2, FInstall.Packages.Engines.Engine[vInt].Name);
    vNodeAux1.Data := FInstall.Packages.Engines.Engine[vInt];
    vNodeAux1.SelectedIndex := 4;
    vNodeAux1.ImageIndex := 4;
  end;

  vNodeAux2 := tvRecursos.Items.AddChild(vPascalRAL, 'Databases');

  vNodeAux1 := tvRecursos.Items.AddChild(vNodeAux2, FInstall.Packages.Databases.Name);
  vNodeAux1.Data := FInstall.Packages.Databases;
  vNodeAux1.SelectedIndex := 4;
  vNodeAux1.ImageIndex := 4;

  for vInt := 0 to Pred(FInstall.Packages.Databases.Packages.Count) do
  begin
    if ((FInstall.Packages.Databases.Packages.Database[vInt].Install.DPK.Count = 0) and (AIDE = 0)) or
       (FInstall.Packages.Databases.Packages.Database[vInt].Install.LPK = '') and (AIDE = 1) then
      Continue;

    vNodeAux2 := tvRecursos.Items.AddChild(vNodeAux1, FInstall.Packages.Databases.Packages.Database[vInt].Name);
    vNodeAux2.Data := FInstall.Packages.Databases.Packages.Database[vInt];
    vNodeAux2.SelectedIndex := 4;
    vNodeAux2.ImageIndex := 4;
  end;

  vNodeAux1 := tvRecursos.Items.AddChild(vPascalRAL, 'Compression');

  for vInt := 0 to Pred(FInstall.Packages.Compression.Count) do
  begin
    if ((FInstall.Packages.Compression.Compression[vInt].Install.DPK.Count = 0) and (AIDE = 0)) or
       (FInstall.Packages.Compression.Compression[vInt].Install.LPK = '') and (AIDE = 1) then
      Continue;

    vNodeAux2 := tvRecursos.Items.AddChild(vNodeAux1, FInstall.Packages.Compression.Compression[vInt].Name);
    vNodeAux2.Data := FInstall.Packages.Compression.Compression[vInt];
    vNodeAux2.SelectedIndex := 4;
    vNodeAux2.ImageIndex := 4;
  end;

  vNodeAux1 := tvRecursos.Items.AddChild(vNode, 'Depedancies');

  for vInt := 0 to Pred(FInstall.Depedancies.Count) do
  begin
    if (((FInstall.Depedancies.Depedancy[vInt].Install.DPK.Count = 0) and (AIDE = 0)) or
       (FInstall.Depedancies.Depedancy[vInt].Install.LPK = '') and (AIDE = 1)) and
       (not (FInstall.Depedancies.Depedancy[vInt].Install.IsEmpty)) then
      Continue;

    vNodeAux2 := tvRecursos.Items.AddChild(vNodeAux1, FInstall.Depedancies.Depedancy[vInt].Name);
    vNodeAux2.Data := FInstall.Depedancies.Depedancy[vInt];
    vNodeAux2.SelectedIndex := 4;
    vNodeAux2.ImageIndex := 4;
  end;

  tvRecursos.FullExpand;
end;

function Tfrm_recursos.validatePageNext: boolean;
begin
  Result := DirectoryExists(ePathDownload.Text);
end;

constructor Tfrm_recursos.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInstall := TInstaller.Create;
end;

destructor Tfrm_recursos.Destroy;
begin
  FreeAndNil(FInstall);
  inherited Destroy;
end;

end.

