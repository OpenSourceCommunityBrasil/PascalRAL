unit language;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type

  { TFrame1 }

  TFrame1 = class(TFrame)
    imBack: TImage;
    imNext: TImage;
    imLangBR: TImage;
    imLangES: TImage;
    imLangUS: TImage;
    lNext: TLabel;
    lPrevious: TLabel;
    lSubTitle: TLabel;
    selectionbox: TImage;
  private

  public

  end;

implementation

{$R *.lfm}

end.

