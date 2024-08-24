unit ufrm_ide_version;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  delphiutils, lazarusutils, installparser, process, ideutils;

type

  { Tfrm_ide_version }

  Tfrm_ide_version = class(TFrame)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    ckSelecionado: TCheckBox;
    imIcone: TImage;
    lbIDEName: TLabel;
    lbPath: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
  private
    FObjectData : TIDEObjectData;
  protected
    procedure SetObjectData(AObjectData : TIDEObjectData);
  public
    constructor Create(AOwner : TComponent; AObjectData : TIDEObjectData);
    destructor Destroy; override;

    procedure installRAL(ALog: TMemo; AInstaller: TInstaller; APathDownload: string);

    property ObjectData : TIDEObjectData read FObjectData;
  end;

implementation

{$R *.lfm}

{ Tfrm_ide_version }

procedure Tfrm_ide_version.SetObjectData(AObjectData: TIDEObjectData);
begin
  FObjectData := AObjectData;

  lbIDEName.Caption := FObjectData.Name;
  lbPath.Caption := FObjectData.ExeFile;
  imIcone.Picture.Assign(FObjectData.Icon);
end;

constructor Tfrm_ide_version.Create(AOwner: TComponent; AObjectData: TIDEObjectData);
begin
  inherited Create(AOwner);
  SetObjectData(AObjectData);
end;

destructor Tfrm_ide_version.Destroy;
begin
  FreeAndNil(FObjectData);
  inherited Destroy;
end;

procedure Tfrm_ide_version.installRAL(ALog: TMemo; AInstaller: TInstaller; APathDownload : string);
begin
  if not ckSelecionado.Checked then
    Exit;

  FObjectData.installRAL(ALog, AInstaller, APathDownload);
end;

end.

