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

  public

  end;

implementation

{$R *.lfm}

end.
