unit fridioma;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  frmodelo, install_types;

type

  { Tfidioma }

  Tfidioma = class(Tfmodelo)
    imLangBR: TImage;
    imLangES: TImage;
    imLangUS: TImage;
    selectionbox: TImage;
    procedure imLangBRClick(Sender: TObject);
  private
    FLanguage: TLanguages;
    FOnChangeLang: TNotifyEvent;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure recalcAligns; override;
    procedure validaControls; override;
  published
    property OnChangeLang: TNotifyEvent read FOnChangeLang write FOnChangeLang;
    property Language: TLanguages read FLanguage write FLanguage;
  end;

implementation

{$R *.lfm}

{ Tfidioma }

procedure Tfidioma.imLangBRClick(Sender: TObject);
begin
  selectionbox.Visible := False;
  selectionbox.Left := TImage(Sender).Left - 4;
  selectionbox.Top := TImage(Sender).Top - 4;
  selectionbox.Visible := True;

  validaControls;

  FLanguage := TLanguages(TImage(Sender).Tag);
  if Assigned(FOnChangeLang) then
    FOnChangeLang(Self);
end;

constructor Tfidioma.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IdTela := 1;
end;

procedure Tfidioma.recalcAligns;
var
  vWidth: integer;
begin
  inherited recalcAligns;

  vWidth := imLangBR.Width + imLangUS.Width + imLangES.Width + 90;
  imLangBR.Left := (Self.Width div 2) - (vWidth div 2);
  imLangUS.Left := imLangBR.Left + imLangBR.Width + 45;
  imLangES.Left := imLangUS.Left + imLangUS.Width + 45;

  imLangBR.Top := (Self.Height div 2) - (imLangBR.Height div 2);
  imLangUS.Top := imLangBR.Top;
  imLangES.Top := imLangBR.Top;

  imLangBRClick(imLangBR);
end;

procedure Tfidioma.validaControls;
begin
  inherited validaControls;
  lbNext.Enabled := Ord(FLanguage) >= 0;
end;

end.
