unit dmcomponents;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Dialogs;

type

  { TDM }

  TDM = class(TDataModule)
    DirDialog: TSelectDirectoryDialog;
    ImageList1: TImageList;
  private

  public

  end;

var
  DM: TDM;

implementation

{$R *.lfm}

end.

