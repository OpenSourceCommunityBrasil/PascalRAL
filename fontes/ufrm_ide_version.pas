unit ufrm_ide_version;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  delphiutils, lazarusutils, installparser;

type

  { Tfrm_ide_version }

  Tfrm_ide_version = class(TFrame)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    ckSelecionado: TCheckBox;
    lbIDEName: TLabel;
    lbPath: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
  private
    FExecFile : string;
    FBuildFile : string;
    FDelphiVerion : TDelphiVersions;
    function GetIDEName: string;
    procedure SetExecFile(AValue: string);
    procedure SetIDEName(AValue: string);
  public
    procedure installRAL(ALog: TMemo; AInstaller: TInstaller; APathDownload: string);
  published
    property IDEName : string read GetIDEName write SetIDEName;
    property ExecFile : string read FExecFile write SetExecFile;
    property BuildFile : string read FBuildFile write FBuildFile;
    property DelphiVerion : TDelphiVersions read FDelphiVerion write FDelphiVerion;
  end;

implementation

{$R *.lfm}

{ Tfrm_ide_version }

procedure Tfrm_ide_version.SetExecFile(AValue: string);
begin
  if FExecFile = AValue then
    Exit;

  FExecFile := AValue;
  lbPath.Caption := ExtractFilePath(FExecFile);
end;

function Tfrm_ide_version.GetIDEName: string;
begin
  Result := lbIDEName.Caption;
end;

procedure Tfrm_ide_version.SetIDEName(AValue: string);
begin
  lbIDEName.Caption := AValue;
end;

procedure Tfrm_ide_version.installRAL(ALog: TMemo; AInstaller: TInstaller; APathDownload : string);
begin
  if not ckSelecionado.Checked then
    Exit;

  ALog.Lines.Add('Iniciando instalação em: ' + IDEName);
end;

end.

