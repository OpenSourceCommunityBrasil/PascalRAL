/// Applies a language to the whole GUI: the resourcestrings (core and screens)
/// and the texts of every form and frame loaded from an .lfm.
unit RALInst.Tela.Traducao;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

/// Translates the program into the language ('pt-BR', 'en-US', 'es-ES'). The
/// forms already created are translated now, and the ones created later (a
/// row of the IDE list) when their .lfm is read
procedure AplicarIdioma(const AIdioma: string);

implementation

uses
  Forms, LCLTranslator, LResources, Translations,
  RALInst.Traducao;

procedure AplicarIdioma(const AIdioma: string);
var
  vTexto: string;
  vStream: TStringStream;
  vTradutor: TPOTranslator;
  vInt: integer;
begin
  TraduzirMensagens(AIdioma);

  // o .po do portugues tem msgstr vazio: o tradutor devolve o msgid, que e o
  // texto original de cada .lfm
  vTexto := TextoPO(AIdioma);
  vStream := TStringStream.Create(vTexto);
  try
    vTradutor := TPOTranslator.Create(TPOFile.Create(vStream, True));
  finally
    vStream.Free;
  end;
  if Assigned(LRSTranslator) then
    LRSTranslator.Free;
  LRSTranslator := vTradutor;
  for vInt := 0 to Pred(Screen.CustomFormCount) do
    vTradutor.UpdateTranslation(Screen.CustomForms[vInt]);
  for vInt := 0 to Pred(Screen.DataModuleCount) do
    vTradutor.UpdateTranslation(Screen.DataModules[vInt]);
end;

end.
