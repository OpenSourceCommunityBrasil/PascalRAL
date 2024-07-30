unit ufrm_idioma;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ufrm_modelo, utools;

type

  { Tfrm_idioma }

  Tfrm_idioma = class(Tfrm_modelo)
    imUSA: TImage;
    imEspanha: TImage;
    imBrasil: TImage;
    imSelect: TImage;
    lbSubTitle: TLabel;
    procedure imUSAClick(Sender: TObject);
  protected
    procedure SetLanguage(AValue: TLanguages); override;
  public

  end;

implementation

{$R *.lfm}

uses
  umain;

{ Tfrm_idioma }

procedure Tfrm_idioma.imUSAClick(Sender: TObject);
var
  vImage : TImage;
begin
  vImage := TImage(Sender);
  fmain.Language := TLanguages(vImage.Tag);
end;

procedure Tfrm_idioma.SetLanguage(AValue: TLanguages);
var
  vImage : TImage;
begin
  case AValue of
    lPortuguese : vImage := imBrasil;
    lEnglish    : vImage := imUSA;
    lSpanish    : vImage := imEspanha;
  end;
  imSelect.Left := vImage.Left - 4;
  imSelect.Top := vImage.Top - 4;
  imSelect.Visible := True;
end;

end.

