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

const
  {$IF Defined(Windows)}
    LazBuildFile = 'lazbuild.exe';
    LazExecFile = 'lazarus.exe';
  {$ELSEIF Defined(Linux)}
    LazBuildFile = 'lazbuild';
    LazExecFile = 'lazarus';
  {$IFEND}

resourcestring
  ThemeLight = 'Claro';
  ThemeDark = 'Escuro';

implementation

end.
