unit frame_install_recursos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CheckLst, StdCtrls,
  ComCtrls, frame_modelo;

type

  { Tfframe_install_recursos }

  Tfframe_install_recursos = class(Tfframe_modelo)
    CheckBox1 : TCheckBox;
    clbDataEngine : TCheckListBox;
    tvResources : TTreeView;
  private

  public
    constructor Create(TheOwner : TComponent); override;
  end;

implementation

{$R *.lfm}

{ Tfframe_install_recursos }

constructor Tfframe_install_recursos.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
  IdTela := 4;
end;

end.

