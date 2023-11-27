unit frinstallrecursos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CheckLst, StdCtrls,
  ComCtrls, Buttons, frmodelo;

type

  { Tfinstallrecursos }

  Tfinstallrecursos = class(Tfmodelo)
    CheckBox1: TCheckBox;
    clbDataEngine: TCheckListBox;
    ePathFolder: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    lVersionNotes1: TLabel;
    sbChoosePath: TSpeedButton;
    sbChoosePath1: TSpeedButton;
  private

  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ Tfinstallrecursos }

constructor Tfinstallrecursos.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IdTela := 4;
end;

end.
