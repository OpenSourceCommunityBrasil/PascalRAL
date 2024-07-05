unit frideversions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tfframe_ide_versions }

  Tfframe_ide_versions = class(TFrame)
    Bevel1: TBevel;
    ckSelected: TCheckBox;
    lbName: TLabel;
    lbPath: TLabel;
  private
    FPath : string;
  protected
    procedure SetPath(AValue: string); virtual;
  public

  published
    property Path : string read FPath write SetPath;
  end;

  Tfframe_ide_versions_class = class of Tfframe_ide_versions;

implementation

{$R *.lfm}

{ Tfframe_ide_versions }

procedure Tfframe_ide_versions.SetPath(AValue: string);
begin
  if FPath = AValue then
    Exit;

  FPath := AValue;
  lbPath.Caption := FPath;
end;

end.
