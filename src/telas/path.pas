unit Path;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Buttons, CheckLst;

type

  { TFrame2 }

  TFrame2 = class(TFrame)
    CheckListBox1: TCheckListBox;
    ePathFolder: TLabeledEdit;
    ePathLazarusSearch: TEdit;
    imBack: TImage;
    imNext: TImage;
    lLatestVersion: TLabel;
    lNext: TLabel;
    lPathIDEVersion: TLabel;
    lPrevious: TLabel;
    lRepoVersion: TLabel;
    lSubTitle: TLabel;
    lVersionNotes: TLabel;
    mmRepoNotes: TMemo;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
  private

  public

  end;

implementation

{$R *.lfm}

end.

