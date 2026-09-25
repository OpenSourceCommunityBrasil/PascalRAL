/// Themes of the installer screens (light and dark) and the images embedded in
/// the executable.
unit RALInst.Tela.Temas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLType, ExtCtrls;

type
  /// Images and colors of one theme.
  TEstiloTema = record
    /// RCDATA of the page background
    Background: string;
    /// RCDATA of the Next/Previous buttons
    Button: string;
    /// Color of the banner strip at the top
    CorBanner: TColor;
    /// Color of the texts over the background
    FontColor: TColor;
    /// RCDATA of the theme switch icon
    Theme: string;
  end;

  /// Screen languages (the Tag of each flag on the language page).
  TLanguages = (lPortuguese, lEnglish, lSpanish);
  /// Screen themes.
  TThemes = (tLight, tDark);

var
  /// Look of each theme
  Themes: array[TThemes] of TEstiloTema;

/// Language code of a TLanguages ('en-US', 'es-ES', 'pt-BR')
function CodigoIdioma(ALanguage: TLanguages): string;
/// Icon of an executable (MAINICON); nil when there is none or outside Windows
function GetIconExeFile(AExe: string): TGraphic;
/// An embedded RCDATA resource; nil when it does not exist
function GetResource(AResource: string): TStream;
/// Loads an embedded image into a TImage
procedure GetResourceImage(AResource: string; AImage: TImage);
/// TLanguages of a language code; en-US for an unknown one
function IdiomaDoCodigo(const ACodigo: string): TLanguages;

implementation

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

procedure InitThemes;
begin
  Themes[tLight].Background := 'LIGHTBG';
  Themes[tLight].Button := 'LIGHTBTN';
  Themes[tLight].CorBanner := clWhite;
  Themes[tLight].FontColor := clBlack;
  Themes[tLight].Theme := 'LIGHTICON';

  // a faixa do banner um tom abaixo do fundo, que e RGB(53,53,53)
  Themes[tDark].Background := 'DARKBG';
  Themes[tDark].Button := 'DARKBTN';
  Themes[tDark].CorBanner := RGBToColor(38, 38, 38);
  Themes[tDark].FontColor := clWhite;
  Themes[tDark].Theme := 'DARKICON';
end;

function CodigoIdioma(ALanguage: TLanguages): string;
begin
  case ALanguage of
    lPortuguese: Result := 'pt-BR';
    lSpanish:    Result := 'es-ES';
  else
    Result := 'en-US';
  end;
end;

function IdiomaDoCodigo(const ACodigo: string): TLanguages;
begin
  if SameText(ACodigo, 'pt-BR') then
    Result := lPortuguese
  else if SameText(ACodigo, 'es-ES') then
    Result := lSpanish
  else
    Result := lEnglish;
end;

function GetResource(AResource: string): TStream;
begin
  Result := nil;
  if FindResource(HINSTANCE, PChar(AResource), RT_RCDATA) <> 0 then
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
  if vStream = nil then
    Exit;
  try
    AImage.Picture.LoadFromStream(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

function GetIconExeFile(AExe: string): TGraphic;
{$IFDEF MSWINDOWS}
const
  LOAD_LIBRARY_AS_DATAFILE = $00000002;
  LOAD_LIBRARY_AS_IMAGE_RESOURCE = $00000020;
var
  vHandle: THandle;
  vSrc: TFPResourceHandle;
{$ENDIF}
begin
  Result := nil;
  {$IFDEF MSWINDOWS}
  // so os recursos: o bds.exe e 32 bits, e um LoadLibrary comum dele num
  // instalador de 64 bits falha e deixa a lista sem icone
  vHandle := Windows.LoadLibraryExW(PWideChar(UnicodeString(AExe)), 0,
               LOAD_LIBRARY_AS_DATAFILE or LOAD_LIBRARY_AS_IMAGE_RESOURCE);
  if vHandle = 0 then
    Exit;
  try
    vSrc := FindResource(vHandle, PChar('MAINICON'), RT_GROUP_ICON);
    if vSrc > 0 then
    begin
      Result := TIcon.Create;
      try
        TIcon(Result).LoadFromResourceHandle(vHandle, vSrc);
      except
        FreeAndNil(Result);
      end;
    end;
  finally
    Windows.FreeLibrary(vHandle);
  end;
  {$ENDIF}
end;

initialization
  InitThemes;

end.
