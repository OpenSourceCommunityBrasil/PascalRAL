unit ufrm_modelo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls,
  utools;

type

  { Tfrm_modelo }

  Tfrm_modelo = class(TFrame)
    imBack: TImage;
    imFundo: TImage;
    imBanner: TImage;
    imClose: TImage;
    imNext: TImage;
    imTheme: TImage;
    lbBack: TLabel;
    lbNext: TLabel;
    lVersion: TLabel;
    procedure imBannerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imBannerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imBannerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imCloseClick(Sender: TObject);
    procedure imThemeClick(Sender: TObject);
    procedure lbBackClick(Sender: TObject);
    procedure lbNextClick(Sender: TObject);
  private
    FIDE: integer;
    FLanguage: TLanguages;
    FTheme: TThemes;

    FPointMove: TPoint;
    FMouseDown: boolean;
  protected
    procedure SetTheme(AValue: TThemes); virtual;
    procedure SetLanguage(AValue: TLanguages); virtual;
    procedure SetIDE(AValue: integer); virtual;

    function validatePageNext : boolean; virtual;
    function validatePagePrior : boolean; virtual;
  public

  published
    property Theme : TThemes read FTheme write SetTheme;
    property Language : TLanguages read FLanguage write SetLanguage;
    property IDE : integer read FIDE write SetIDE;
  end;

  Tfrm_modelo_class = class of Tfrm_modelo;

implementation

{$R *.lfm}

uses
  umain;

{ Tfrm_modelo }

procedure Tfrm_modelo.imCloseClick(Sender: TObject);
begin
  fmain.Close;
end;

procedure Tfrm_modelo.imBannerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FPointMove.X := X;
  FPointMove.Y := Y;
  FMouseDown := True;
end;

procedure Tfrm_modelo.imBannerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FMouseDown then
  begin
    fmain.Left := fmain.Left + X - FPointMove.X;
    fmain.Top := fmain.Top + Y - FPointMove.Y;
  end;
end;

procedure Tfrm_modelo.imBannerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := False;
end;

procedure Tfrm_modelo.imThemeClick(Sender: TObject);
begin
  if imTheme.Tag = 0 then
    fmain.Theme:= TThemes(1)
  else
    fmain.Theme:= TThemes(0);
end;

procedure Tfrm_modelo.lbBackClick(Sender: TObject);
begin
  if validatePagePrior then
    fmain.PriorPage;
end;

procedure Tfrm_modelo.lbNextClick(Sender: TObject);
begin
  if validatePageNext then
    fmain.NextPage;
end;

procedure Tfrm_modelo.SetIDE(AValue: integer);
begin
  if FIDE = AValue then
    Exit;

  FIDE := AValue;
end;

procedure Tfrm_modelo.SetLanguage(AValue: TLanguages);
begin
  if FLanguage = AValue then
    Exit;

  FLanguage := AValue;
end;

function Tfrm_modelo.validatePageNext: boolean;
begin
  Result := True;
end;

function Tfrm_modelo.validatePagePrior: boolean;
begin
  Result := True;
end;

procedure Tfrm_modelo.SetTheme(AValue: TThemes);
var
  vInt: Integer;
begin
  if FTheme = AValue then
    Exit;

  FTheme := AValue;
  imTheme.Tag := Ord(AValue);

  GetResourceImage(Themes[AValue].Background, imFundo);
  GetResourceImage(Themes[AValue].Button, imNext);
  GetResourceImage(Themes[AValue].Button, imBack);
  GetResourceImage(Themes[AValue].Theme, imTheme);

  for vInt := 0 to Pred(ComponentCount) do
  begin
    if (Components[vInt] is TLabel) and (TLabel(Components[vInt]).Tag >= 0) then
      TLabel(Components[vInt]).Font.Color := Themes[AValue].FontColor;
  end;
end;

end.

