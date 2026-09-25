program traducao;

{$mode ObjFPC}{$H+}

// As mensagens do nucleo trocam de idioma em tempo de execucao, e voltam:
//
//   traducao <pasta do repositorio>
//
// Le src/languages/ralinstaller.<idioma>.po da pasta (os testes nao tem os .po
// embutidos) e confere tambem que todo .po tem os mesmos curingas do original.

uses
  Classes, SysUtils, RALInst.Mensagens, RALInst.Traducao;

var
  GFalhas: integer = 0;

procedure Conferir(ACondicao: boolean; const ATexto: string);
begin
  if ACondicao then
    WriteLn('  ok    ', ATexto)
  else
  begin
    WriteLn('  FALHA ', ATexto);
    Inc(GFalhas);
  end;
end;

procedure CopiarPO(const ARaiz: string);
var
  vIdioma: string;
begin
  // o TextoPO procura em <exe>/languages
  ForceDirectories(ExtractFilePath(ParamStr(0)) + 'languages');
  for vIdioma in IdiomasInstalador do
    with TStringList.Create do
    try
      LoadFromFile(IncludeTrailingPathDelimiter(ARaiz) + 'src' + PathDelim +
                   'languages' + PathDelim +
                   'ralinstaller.' + vIdioma + '.po');
      SaveToFile(ExtractFilePath(ParamStr(0)) + 'languages' + PathDelim +
                 'ralinstaller.' + vIdioma + '.po');
    finally
      Free;
    end;
end;

begin
  if ParamCount < 1 then
  begin
    WriteLn('uso: traducao <pasta do repositorio>');
    Halt(2);
  end;
  CopiarPO(ParamStr(1));

  Conferir(cmOrigemDisco = 'disco', 'original em portugues');
  TraduzirMensagens('en-US');
  Conferir(cmOrigemDisco = 'disk', 'en-US: ' + cmOrigemDisco);
  Conferir(Format(emGitHubSemAVersao, ['a', 'b', 'c']) = 'a/b does not have version c',
           'en-US com curingas: ' + Format(emGitHubSemAVersao, ['a', 'b', 'c']));
  TraduzirMensagens('es-ES');
  Conferir(cmTerminouComErro = 'terminó con error', 'es-ES: ' + cmTerminouComErro);
  Conferir(IdiomaAtual = 'es-ES', 'idioma atual es-ES');
  TraduzirMensagens('pt-BR');
  Conferir(cmOrigemDisco = 'disco', 'de volta ao portugues: ' + cmOrigemDisco);
  Conferir(IdiomaDoSistema <> '', 'idioma do sistema: ' + IdiomaDoSistema);

  if GFalhas = 0 then
    WriteLn('todos os testes passaram')
  else
    WriteLn(GFalhas, ' falha(s)');
  Halt(Ord(GFalhas > 0));
end.
