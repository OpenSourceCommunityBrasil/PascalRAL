unit utools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLType, ExtCtrls;

type
  TSplashFormStyle = record
    Background: string;
    Banner: string;
    Subtitle: string;
    Theme: string;
    Button: string;
    FontColor: TColor;
  end;

  TSteps = (sLanguage, sIDE, sPath, sResources, sConfirm, sInstall, sFinish);
  TThemes = (tLight, tDark);
  TLanguages = (lPortuguese, lEnglish, lSpanish);

var
  Themes: array[TThemes] of TSplashFormStyle;

resourcestring
  ThemeLight = 'Claro';
  ThemeDark = 'Escuro';

function GetThemeCaption(ATheme: TThemes): string;
function GetResource(AResource: string): TStream;
procedure GetResourceImage(AResource: string; AImage: TImage);

implementation

procedure InitThemes;
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

function GetThemeCaption(ATheme: TThemes): string;
begin
  Result := '';
  case ATheme of
    tLight: Result := PResStringRec(@ThemeLight)^;
    tDark: Result := PResStringRec(@ThemeDark)^;
  end;
end;

function GetResource(AResource: string): TStream;
begin
  Result := nil;
  if FindResource(HINSTANCE, AResource, RT_RCDATA) <> 0 then
  begin
    Result := TResourceStream.Create(HINSTANCE, AResource, RT_RCDATA);
    Result.Position := 0;
  end;
end;

procedure GetResourceImage(AResource: string; AImage: TImage);
var
  vStream: TStream;
begin
  vStream := GetResource(AResource);
  try
    AImage.Picture.LoadFromStream(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

initialization
  InitThemes;

end.

