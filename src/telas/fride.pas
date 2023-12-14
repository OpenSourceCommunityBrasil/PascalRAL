unit fride;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  frmodelo, ghrepofunctions, ralinstallparser;

type

  { Tfide }

  Tfide = class(Tfmodelo)
    imDelphi: TImage;
    imDelphiFundo: TImage;
    imLazarus: TImage;
    imLazarusFundo: TImage;
    lbDelphi: TLabel;
    lbLazarus: TLabel;
    procedure imLazarusClick(Sender: TObject);
    procedure lbNextClick(Sender: TObject);
  private
    FIDE: integer;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure recalcAligns; override;
    procedure validaControls; override;
  published
    property IDE: integer read FIDE write FIDE;
  end;

implementation

{$R *.lfm}

{ Tfide }

procedure Tfide.imLazarusClick(Sender: TObject);
begin
  FIDE := TImage(Sender).Tag;
  imLazarusFundo.Visible := FIDE = 0;
  imDelphiFundo.Visible := FIDE = 1;
  validaControls;
end;

procedure Tfide.lbNextClick(Sender: TObject);
var
  install: TJSONInstaller;
begin
  install := TJSONInstaller.Create;
  inherited;
end;

constructor Tfide.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IdTela := 2;
  imDelphiFundo.Visible := False;
  imLazarusFundo.Visible := False;
  FIDE := -1;
end;

procedure Tfide.recalcAligns;
var
  vWidth: integer;
begin
  inherited recalcAligns;

  vWidth := imDelphi.Width + imLazarus.Width + 90;
  imDelphi.Left := (Self.Width div 2) - (vWidth div 2);
  imLazarus.Left := imDelphi.Left + imDelphi.Width + 90;

  imDelphi.Top := (Self.Height div 2) - (imDelphi.Height div 2);
  imLazarus.Top := imDelphi.Top;

  imDelphiFundo.Left := imDelphi.Left - 4;
  imDelphiFundo.Top := imDelphi.Top - 4;

  imLazarusFundo.Left := imLazarus.Left - 2;
  imLazarusFundo.Top := imLazarus.Top - 2;

  lbLazarus.Top := imLazarus.Top + imLazarus.Height + 10;
  lbDelphi.Top := lbLazarus.Top;

  lbLazarus.Left := imLazarus.Left + (imLazarus.Width div 2) - (lbLazarus.Width div 2);
  lbDelphi.Left := imDelphi.Left + (imDelphi.Width div 2) - (lbDelphi.Width div 2);
end;

procedure Tfide.validaControls;
begin
  inherited validaControls;
  lbNext.Enabled := FIDE >= 0;
end;

end.
