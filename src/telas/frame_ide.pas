unit frame_ide;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  frame_modelo;

type

  { Tfframe_ide }

  Tfframe_ide = class(Tfframe_modelo)
    imDelphi : TImage;
    imDelphiFundo : TImage;
    imLazarus : TImage;
    imLazarusFundo : TImage;
    lbDelphi : TLabel;
    lbLazarus : TLabel;
    procedure imLazarusClick(Sender : TObject);
  private
    FIDE : integer;
  public
    constructor Create(TheOwner : TComponent); override;
    procedure recalcAligns; override;
    procedure validaControls; override;
  published
    property IDE : integer read FIDE write FIDE;
  end;

implementation

{$R *.lfm}

{ Tfframe_ide }

procedure Tfframe_ide.imLazarusClick(Sender : TObject);
begin
  FIDE := TImage(Sender).Tag;
  imLazarusFundo.Visible := FIDE = 0;
  imDelphiFundo.Visible := FIDE = 1;
  validaControls;
end;

constructor Tfframe_ide.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
  IdTela := 2;
  imDelphiFundo.Visible := False;
  imLazarusFundo.Visible := False;
  FIDE := -1;
end;

procedure Tfframe_ide.recalcAligns;
var
  vWidth : integer;
begin
  inherited recalcAligns;

  vWidth := imDelphi.Width + imLazarus.Width  + 90;
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

procedure Tfframe_ide.validaControls;
begin
  inherited validaControls;
  lbNext.Enabled := FIDE >= 0;
end;

end.

