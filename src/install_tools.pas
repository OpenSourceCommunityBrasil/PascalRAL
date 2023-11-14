unit install_tools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  install_types;

var
  Themes: array[TThemes] of TSplashFormStyle;
  ImgBackground : TStream;

procedure initThemes;
function getThemeCaption(ATheme : TThemes) : string;

implementation

procedure initThemes;
var
  vTheme: TSplashFormStyle;
begin
  // light
  vTheme.Background := 'LIGHTBG';
  vTheme.Banner := 'BANNER';
  vTheme.FontColor := clBlack;
  vTheme.Theme := 'LIGHTICON';
  vTheme.Subtitle := 'Light';
  vTheme.Button := 'LIGHTBTN';
  Themes[tLight] := vTheme;

  // dark
  vTheme.Background := 'DARKBG';
  vTheme.Banner := 'BANNER';
  vTheme.FontColor := clWhite;
  vTheme.Theme := 'DARKICON';
  vTheme.Subtitle := 'Dark';
  vTheme.Button := 'DARKBTN';
  Themes[tDark] := vTheme;
end;

function getThemeCaption(ATheme : TThemes) : string;
begin
  Result := '';
  case ATheme of
    tLight : Result := PResStringRec(@ThemeLight)^;
    tDark  : Result := PResStringRec(@ThemeDark)^;
  end;
end;

initialization
  initThemes;
  ImgBackground := nil;

end.

