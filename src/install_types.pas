unit install_types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

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

resourcestring
  ThemeLight = 'Claro';
  ThemeDark = 'Escuro';

implementation

end.

