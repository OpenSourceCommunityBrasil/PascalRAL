unit ufrm_ide;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ufrm_modelo;

type

  { Tfrm_ide }

  Tfrm_ide = class(Tfrm_modelo)
    imDelphi: TImage;
    imLazarus: TImage;
    imSelLazarus: TImage;
    imSelDelphi: TImage;
    lbSubTitle: TLabel;
    procedure imDelphiClick(Sender: TObject);
    procedure imLazarusClick(Sender: TObject);
  private

  protected
    procedure SetIDE(AValue: integer); override;
    function validatePageNext : boolean; override;
  public

  end;

implementation

{$R *.lfm}

uses
  umain;

{ Tfrm_ide }

procedure Tfrm_ide.imLazarusClick(Sender: TObject);
begin
  fmain.IDE := 1;
end;

procedure Tfrm_ide.imDelphiClick(Sender: TObject);
begin
  fmain.IDE := 0;
end;

procedure Tfrm_ide.SetIDE(AValue: integer);
begin
  inherited SetIDE(AValue);
  imSelDelphi.Visible := False;
  imSelLazarus.Visible := False;

  {$IFDEF LINUX}
    imDelphi.Visible := False;
    imLazarus.Left := (Self.Width div 2) - (imLazarus.Width div 2);
  {$ENDIF}

  case AValue of
    0 : begin
      imSelLazarus.Visible := False;

      imSelDelphi.Left := imDelphi.Left - 4;
      imSelDelphi.Top := imDelphi.Top - 4;
      imSelDelphi.Visible := True;
    end;
    1 : begin
      imSelDelphi.Visible := False;

      imSelLazarus.Left := imLazarus.Left - 2;
      imSelLazarus.Top := imLazarus.Top - 4;
      imSelLazarus.Visible := True;
    end;
  end;
end;

function Tfrm_ide.validatePageNext: boolean;
begin
  if IDE < 0 then
  begin
    ShowMessage('Escolha uma IDE');
    Result := False;
    Exit;
  end;
  inherited;
end;

end.

