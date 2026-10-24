program apidiff_m9;

{$mode ObjFPC}{$H+}

// "Pronto quando" da M9: toda quebra nova traz a regra.
// Pega migrador/api/api-dev.json, renomeia de proposito uma propriedade published
// (TRALServer.CookieLife -> CookieLifeX) numa copia em memoria e confere que:
//   - o diff contra o original lista published-sumiu:...CookieLife com o nome;
//   - uma regra na cobertura (em memoria) tira a diferenca da lista;
//   - o criterio do --pre-lancamento (lista nao vazia) falha enquanto ha
//     pendente e passa quando a lista esta vazia;
//   - o texto do aviso sai como ::warning:: no GitHub Actions e como texto
//     simples fora dele.
//   apidiff_m9 [--raiz=<pasta migrador>]
//
// Compilar (da pasta do teste):
//   fpc -B -Fu../../src -Fu../../../src/classes apidiff_m9.lpr

uses
  Classes, SysUtils,
  RALMig.API, RALMig.Inventario;

const
  PROPRIEDADE = 'CookieLife';
  RENOMEADA = 'CookieLifeX';
  TEXTO_COBERTURA = '{"passos":[{"de":"dev","para":"dev-alterada","fechado":false,"itens":[' +
    '{"chave":"published-sumiu:*.CookieLife","regra":"teste/cookielife-renomear"}]}]}';

var
  GFalhas: integer = 0;
  GRaiz: string = '';

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

/// Renames a member in every class that has it (the flattened published list
/// of each descendant follows); returns how many classes changed
function Renomear(AApi: TApi; const ADe, APara: string): integer;
var
  vInt, vIdx: integer;
  vCls: TApiClasse;
  vAntigo, vNovo: TApiMembro;
begin
  Result := 0;
  for vInt := 0 to Pred(AApi.Classes.Count) do
  begin
    vCls := TApiClasse(AApi.Classes.Objects[vInt]);
    vAntigo := vCls.Buscar(ADe);
    if vAntigo = nil then
      Continue;
    vNovo := vCls.Obter(APara);
    vNovo.Cat := vAntigo.Cat;
    vNovo.Forma := vAntigo.Forma;
    vNovo.Tipo := vAntigo.Tipo;
    vNovo.Padrao := vAntigo.Padrao;
    vNovo.Armazenado := vAntigo.Armazenado;
    vIdx := vCls.Membros.IndexOf(ADe);
    vCls.Membros.Delete(vIdx);
    Inc(Result);
  end;
end;

function ChavesCom(AResultado: TResultadoPasso; const ANome: string): integer;
var
  vInt: integer;
begin
  Result := 0;
  for vInt := 0 to Pred(AResultado.Pendentes.Count) do
    if Pos(ANome, TDiferenca(AResultado.Pendentes[vInt]).Chave) > 0 then
      Inc(Result);
end;

procedure Teste;
var
  vOriginal, vAlterada: TApi;
  vCob: TCobertura;
  vRes: TResultadoPasso;
  vArq, vTexto: string;
  vTrocadas: integer;
begin
  vArq := ArquivoApi(GRaiz + PathDelim + 'api', 'dev');
  if not FileExists(vArq) then
  begin
    Conferir(False, 'falta ' + vArq);
    Exit;
  end;
  vOriginal := TApi.Create;
  vAlterada := TApi.Create;
  vCob := TCobertura.Create;
  try
    vOriginal.CarregarArquivo(vArq);
    vAlterada.CarregarArquivo(vArq);
    vOriginal.Rotulo := 'dev';
    vAlterada.Rotulo := 'dev-alterada';
    vTrocadas := Renomear(vAlterada, PROPRIEDADE, RENOMEADA);
    Conferir(vTrocadas > 0, 'a propriedade existe em ' + IntToStr(vTrocadas) + ' classe(s) da API dev');

    WriteLn('sem regra: a quebra aparece com o nome');
    vRes := CompararComCobertura(vOriginal, vAlterada, vCob);
    try
      Conferir(vRes.Pendentes.Count > 0, 'ha pendentes');
      Conferir(vRes.Diferencas.Buscar('published-sumiu:TRALServer.' + PROPRIEDADE) <> nil,
               'published-sumiu:TRALServer.' + PROPRIEDADE + ' esta na lista');
      Conferir(ChavesCom(vRes, 'published-sumiu:') >= vTrocadas,
               'uma entrada por classe achatada (' + IntToStr(vTrocadas) + ')');
      Conferir(ChavesCom(vRes, '.' + PROPRIEDADE) = vRes.Pendentes.Count,
               'so a propriedade renomeada pende (' + IntToStr(vRes.Pendentes.Count) + ')');
      vTexto := TextoAviso(vRes, True);
      Conferir((Pos('::warning::published-sumiu:', vTexto) = 1) and
               (Pos('::warning::published-sumiu:TRALServer.' + PROPRIEDADE, vTexto) > 0),
               'texto do GitHub Actions: uma ::warning:: por linha, com o nome');
      Conferir(Pos('::warning::', TextoAviso(vRes, False)) = 0, 'fora do GitHub e texto simples');
      Conferir(Pos('TRALServer.' + PROPRIEDADE, TextoAviso(vRes, False)) > 0, 'texto simples traz o nome');
      Conferir(vRes.Pendentes.Count > 0, '--pre-lancamento falharia (lista nao vazia)');
    finally
      vRes.Free;
    end;

    WriteLn('com a regra: sai da lista');
    Conferir(vCob.CarregarTexto(TEXTO_COBERTURA), 'cobertura em memoria valida: ' + vCob.Erros.Text);
    vRes := CompararComCobertura(vOriginal, vAlterada, vCob);
    try
      Conferir(vRes.Passo.Rotulo = 'dev>dev-alterada', 'usou o passo da cobertura cujo de/para batem');
      Conferir(vRes.Pendentes.Count = 0, 'sem pendentes: ' + TextoAviso(vRes, False));
      Conferir(vRes.Cobertas.Count > 0, 'as diferencas estao cobertas pela regra');
      Conferir(TextoAviso(vRes, True) = '', 'nenhum aviso');
      Conferir(vRes.Pendentes.Count = 0, '--pre-lancamento passaria (lista vazia)');
    finally
      vRes.Free;
    end;

    WriteLn('a regra so cobre o que diz: outra quebra continua pendente');
    vTrocadas := Renomear(vAlterada, 'MaxConnections', 'MaxConnectionsX');
    vRes := CompararComCobertura(vOriginal, vAlterada, vCob);
    try
      if vTrocadas > 0 then
      begin
        Conferir(ChavesCom(vRes, '.MaxConnections') > 0, 'a nova quebra aparece com o nome');
        Conferir(ChavesCom(vRes, '.' + PROPRIEDADE) = 0, 'a coberta continua fora da lista');
      end
      else
        WriteLn('  (MaxConnections nao existe nesta API: caso pulado)');
    finally
      vRes.Free;
    end;
  finally
    vCob.Free;
    vAlterada.Free;
    vOriginal.Free;
  end;
end;

procedure LerArgumentos;
var
  vInt: integer;
  vArg: string;
begin
  for vInt := 1 to ParamCount do
  begin
    vArg := ParamStr(vInt);
    if Copy(vArg, 1, 7) = '--raiz=' then
      GRaiz := ExcludeTrailingPathDelimiter(ExpandFileName(Copy(vArg, 8, MaxInt)));
  end;
  if GRaiz = '' then
    GRaiz := AcharPastaMigrador;
end;

begin
  LerArgumentos;
  if GRaiz = '' then
  begin
    WriteLn('Nao achei a pasta migrador (use --raiz=<pasta>).');
    Halt(2);
  end;
  Teste;
  WriteLn;
  if GFalhas = 0 then
    WriteLn('tudo certo')
  else
    WriteLn(GFalhas, ' falha(s)');
  if GFalhas > 0 then
    Halt(1);
end.
