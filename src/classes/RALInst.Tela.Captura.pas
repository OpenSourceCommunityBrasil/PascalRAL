/// Hidden --capturar=<folder> switch of the two windows (installer and
/// migrator): every page, in both themes and the three languages, saved into
/// PNG files, so the pages can be compared side by side without clicking
/// through them.
unit RALInst.Tela.Captura;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  RALInst.Tela.Temas;

/// File name of one capture: <program>-<page>-<theme>-<language>.png
function NomeCaptura(const APrograma: string; APagina: integer; ATema: TThemes;
  AIdioma: TLanguages): string;
/// The folder of --capturar=<folder> (with a trailing delimiter); '' without
/// the switch
function PastaCaptura: string;
/// Saves the window, as the screen shows it now, into a PNG file
procedure SalvarCaptura(AJanela: TCustomForm; const AArquivo: string);

implementation

uses
  Graphics;

const
  NOMES_TEMA: array[TThemes] of string = ('claro', 'escuro');

function NomeCaptura(const APrograma: string; APagina: integer; ATema: TThemes;
  AIdioma: TLanguages): string;
begin
  Result := Format('%s-%d-%s-%s.png', [APrograma, APagina, NOMES_TEMA[ATema],
    CodigoIdioma(AIdioma)]);
end;

function PastaCaptura: string;
var
  vInt: integer;
  vArg: string;
begin
  Result := '';
  for vInt := 1 to ParamCount do
  begin
    vArg := ParamStr(vInt);
    if SameText(Copy(vArg, 1, 11), '--capturar=') and (Length(vArg) > 11) then
      Result := IncludeTrailingPathDelimiter(ExpandFileName(Copy(vArg, 12, MaxInt)));
  end;
end;

procedure SalvarCaptura(AJanela: TCustomForm; const AArquivo: string);
var
  vBitmap: TBitmap;
  vPNG: TPortableNetworkGraphic;
  vFim: QWord;
begin
  // a pagina nova precisa de alguns ciclos para se desenhar inteira
  vFim := GetTickCount64 + 300;
  repeat
    Application.ProcessMessages;
    Sleep(10);
  until GetTickCount64 >= vFim;
  vBitmap := AJanela.GetFormImage;
  try
    vPNG := TPortableNetworkGraphic.Create;
    try
      vPNG.Assign(vBitmap);
      ForceDirectories(ExtractFilePath(AArquivo));
      vPNG.SaveToFile(AArquivo);
    finally
      vPNG.Free;
    end;
  finally
    vBitmap.Free;
  end;
end;

end.
