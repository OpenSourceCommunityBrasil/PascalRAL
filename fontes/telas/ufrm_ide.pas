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
    lbAmbos: TLabel;
    procedure imDelphiClick(Sender: TObject);
    procedure lbAmbosClick(Sender: TObject);
    procedure imLazarusClick(Sender: TObject);
  private
    // fora do Windows: diz que o Delphi nao existe ali
    FAvisoDelphi: TLabel;
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

procedure Tfrm_ide.lbAmbosClick(Sender: TObject);
begin
  // F9: as duas numa rodada so
  fmain.IDE := 2;
end;

procedure Tfrm_ide.SetIDE(AValue: integer);
begin
  inherited SetIDE(AValue);
  imSelDelphi.Visible := False;
  imSelLazarus.Visible := False;
  if AValue = 2 then
    lbAmbos.Font.Style := [fsUnderline, fsBold]
  else
    lbAmbos.Font.Style := [fsUnderline];

  // o Delphi so existe no Windows: fora dele a opcao nem aparece — e a tela
  // diz por que, para ninguem procurar o que nao esta faltando (F11)
  {$IFNDEF MSWINDOWS}
    imDelphi.Visible := False;
    lbAmbos.Visible := False;
    imLazarus.Left := (Self.Width div 2) - (imLazarus.Width div 2);
    if FAvisoDelphi = nil then
    begin
      FAvisoDelphi := TLabel.Create(Self);
      FAvisoDelphi.Parent := Self;
      FAvisoDelphi.AutoSize := False;
      FAvisoDelphi.Alignment := taCenter;
      FAvisoDelphi.WordWrap := True;
      FAvisoDelphi.Left := 40;
      FAvisoDelphi.Width := Self.Width - 80;
      FAvisoDelphi.Top := imLazarus.Top + imLazarus.Height + 60;
      FAvisoDelphi.Height := 40;
      FAvisoDelphi.Anchors := [akLeft, akRight, akTop];
      FAvisoDelphi.Caption := 'O Delphi só existe no Windows: este instalador instala o ' +
                              'PascalRAL no Lazarus.';
      FAvisoDelphi.Font.Color := lbSubTitle.Font.Color;
    end;
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
    // F9: Delphi e Lazarus juntos
    2 : begin
      imSelDelphi.Left := imDelphi.Left - 4;
      imSelDelphi.Top := imDelphi.Top - 4;
      imSelDelphi.Visible := True;
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

