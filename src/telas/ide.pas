unit ide;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls;

type

  { TfrIDE }

  TfrIDE = class(TFrame)
    imDelphi: TImage;
    imDelphiFundo: TImage;
    imBack: TImage;
    imNext: TImage;
    imLazarus: TImage;
    imLazarusFundo: TImage;
    lDelphi: TLabel;
    lLazarus: TLabel;
    lNext: TLabel;
    lPrevious: TLabel;
    lSubTitle: TLabel;
  private

  public

  end;

implementation

{$R *.lfm}

end.

