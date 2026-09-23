program recibos;

{$mode ObjFPC}{$H+}

// F10: testes do caminho de volta sem IDE nenhuma.
//
//   recibos [--config=<pasta de configuracao do Lazarus>]
//   recibos --mostrar=<pasta de configuracao>   as duas listas, ordenadas
//
// A configuracao dada (padrao: a do Lazarus 4.6 desta maquina) so e LIDA: os
// dois arquivos sao copiados para uma pasta temporaria, e e la que se grava.

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, RALInst.Config.Lazarus
  {$IFDEF MSWINDOWS}, Windows{$ENDIF};

var
  GFalhas: integer = 0;

procedure Conferir(ACondicao: boolean; const ADescricao: string);
begin
  if ACondicao then
    WriteLn('  ok    ', ADescricao)
  else
  begin
    WriteLn('  FALHA ', ADescricao);
    Inc(GFalhas);
  end;
end;

function Lista(const ATexto: string): TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  Result.CommaText := ATexto;
end;

procedure TestesDesfazer;
var
  vAntes, vDepois, vAtual: TStringList;
begin
  WriteLn('desfazer a mudanca antes->depois sobre o estado atual:');
  vAntes := Lista('a=1,b=2');
  vDepois := Lista('a=1,b=3,c=4');
  vAtual := Lista('a=1,b=3,c=4,d=5');
  try
    DesfazerMudanca(vAtual, vAntes, vDepois, True);
    Conferir(vAtual.CommaText = 'a=1,b=2,d=5',
             'link novo sai, link trocado volta, o do usuario fica: ' + vAtual.CommaText);

    vAtual.CommaText := 'a=1,b=7,c=9';
    DesfazerMudanca(vAtual, vAntes, vDepois, True);
    Conferir(vAtual.CommaText = 'a=1,b=7,c=9',
             'o que o usuario trocou depois nao e desfeito: ' + vAtual.CommaText);

    vAtual.CommaText := 'a=1';
    DesfazerMudanca(vAtual, vAntes, vDepois, True);
    Conferir(vAtual.CommaText = 'a=1', 'o que o usuario ja tirou continua fora');

    vAntes.CommaText := 'x,y';
    vDepois.CommaText := 'x,y,z';
    vAtual.CommaText := 'x,y,z,w';
    DesfazerMudanca(vAtual, vAntes, vDepois, False);
    Conferir(vAtual.CommaText = 'x,y,w', 'instalados: o da instalacao sai, o do usuario fica: ' +
             vAtual.CommaText);

    vAntes.CommaText := 'x,z';
    vAtual.CommaText := 'x,z';
    DesfazerMudanca(vAtual, vAntes, vDepois, False);
    Conferir(vAtual.CommaText = 'x,z', 'o que ja estava antes nao sai');
  finally
    vAtual.Free;
    vDepois.Free;
    vAntes.Free;
  end;
end;

procedure TestesArquivos(const AConfig: string);
var
  vPasta, vErro: string;
  vLinks, vDeNovo, vInst, vInstDeNovo: TStringList;
  vArquivo: TStringList;
begin
  WriteLn('packagefiles.xml e miscellaneousoptions.xml (copias de ', AConfig, '):');
  if not FileExists(IncludeTrailingPathDelimiter(AConfig) + 'packagefiles.xml') then
  begin
    WriteLn('  --    sem packagefiles.xml ali; teste pulado');
    Exit;
  end;
  vPasta := IncludeTrailingPathDelimiter(GetTempDir) + 'ralinst-recibos-teste' + PathDelim;
  ForceDirectories(vPasta);
  vArquivo := TStringList.Create;
  vLinks := TStringList.Create;
  vDeNovo := TStringList.Create;
  vInst := TStringList.Create;
  vInstDeNovo := TStringList.Create;
  try
    vArquivo.LoadFromFile(IncludeTrailingPathDelimiter(AConfig) + 'packagefiles.xml');
    vArquivo.SaveToFile(vPasta + 'packagefiles.xml');
    vArquivo.LoadFromFile(IncludeTrailingPathDelimiter(AConfig) + 'miscellaneousoptions.xml');
    vArquivo.SaveToFile(vPasta + 'miscellaneousoptions.xml');

    LerLinks(vPasta, vLinks);
    LerInstalados(vPasta, vInst);
    Conferir(vLinks.Count > 0, Format('%d link(s) lidos', [vLinks.Count]));
    Conferir(vInst.Count > 0, Format('%d pacote(s) instalados lidos', [vInst.Count]));

    // gravar a mesma lista nao muda nada
    Conferir(GravarLinks(vPasta, vLinks, vErro), 'grava os links de volta ' + vErro);
    Conferir(GravarInstalados(vPasta, vInst, vErro), 'grava os instalados de volta ' + vErro);
    LerLinks(vPasta, vDeNovo);
    LerInstalados(vPasta, vInstDeNovo);
    Conferir(vDeNovo.Text = vLinks.Text, 'links iguais depois de regravar');
    Conferir(vInstDeNovo.Text = vInst.Text, 'instalados iguais depois de regravar');

    // acrescentar e tirar, como a instalacao e a desinstalacao fazem
    vDeNovo.Add('ralteste=C:\RAL\pkg\ralteste.lpk');
    vInstDeNovo.Add('ralteste');
    GravarLinks(vPasta, vDeNovo, vErro);
    GravarInstalados(vPasta, vInstDeNovo, vErro);
    LerLinks(vPasta, vDeNovo);
    LerInstalados(vPasta, vInstDeNovo);
    Conferir(vDeNovo.Values['ralteste'] = 'C:\RAL\pkg\ralteste.lpk', 'link novo gravado');
    Conferir(vInstDeNovo.IndexOf('ralteste') >= 0, 'instalado novo gravado');
    DesfazerMudanca(vDeNovo, vLinks, vDeNovo, True);
    vInstDeNovo.Delete(vInstDeNovo.IndexOf('ralteste'));
    GravarLinks(vPasta, vDeNovo, vErro);
    GravarInstalados(vPasta, vInstDeNovo, vErro);
    LerLinks(vPasta, vDeNovo);
    LerInstalados(vPasta, vInstDeNovo);
    Conferir(vDeNovo.Text = vLinks.Text, 'depois de tirar, os links sao os de antes');
    Conferir(vInstDeNovo.Text = vInst.Text, 'depois de tirar, os instalados sao os de antes');

    vArquivo.LoadFromFile(vPasta + 'packagefiles.xml');
    Conferir(Pos('Count="' + IntToStr(vLinks.Count) + '"', vArquivo.Text) > 0,
             'Count do UserPkgLinks acompanha a lista');
    Conferir(Pos('<Item' + IntToStr(vLinks.Count) + '>', vArquivo.Text) > 0,
             'itens renumerados de 1 a Count');
  finally
    vInstDeNovo.Free;
    vInst.Free;
    vDeNovo.Free;
    vLinks.Free;
    vArquivo.Free;
  end;
end;

var
  GConfig: string;
  GLista: TStringList;

begin
  {$IFDEF MSWINDOWS}SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  // --mostrar=<pcp>: as duas listas, ordenadas, para comparar antes e depois
  if Copy(ParamStr(1), 1, 10) = '--mostrar=' then
  begin
    GLista := TStringList.Create;
    try
      LerLinks(Copy(ParamStr(1), 11, MaxInt), GLista);
      GLista.Sort;
      WriteLn('[links]');
      Write(GLista.Text);
      LerInstalados(Copy(ParamStr(1), 11, MaxInt), GLista);
      GLista.Sort;
      WriteLn('[instalados]');
      Write(GLista.Text);
    finally
      GLista.Free;
    end;
    Halt(0);
  end;
  GConfig := 'D:\IDE\lazarus\4.6\config_lazarus';
  if Copy(ParamStr(1), 1, 9) = '--config=' then
    GConfig := Copy(ParamStr(1), 10, MaxInt);
  TestesDesfazer;
  TestesArquivos(GConfig);
  WriteLn;
  if GFalhas = 0 then
    WriteLn('todos os testes passaram')
  else
    WriteLn(GFalhas, ' falha(s)');
  Halt(Ord(GFalhas > 0));
end.
