program regras;

{$mode ObjFPC}{$H+}

// Regras de migracao do RAL (migrador, etapa M2).
//   regras --testes                    autoteste com regras sinteticas em memoria
//   regras --pasta=<migrador/regras>   modo real: carrega os arquivos de regras
//                                      (modo estrito: os tres idiomas em todo
//                                      texto), confere que todo id citado em
//                                      cobertura.json existe e lista os ids que
//                                      sobram; sai com 1 se algo falhou
//   --cobertura=<arquivo>              outro cobertura.json (padrao: o da pasta)
//
// Compilar (da pasta do teste):
//   fpc -B -MObjFPC -Sh -Fu../../src -Fu../../../src/classes regras.lpr

uses
  Classes, SysUtils, Contnrs,
  RALMig.Regras, RALMig.Inventario;

var
  GFalhas: integer = 0;
  GPasta: string = '';
  GCobertura: string = '';
  GSoTestes: boolean = False;

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

{ JSON sintetico: aspas simples viram aspas duplas }

function Q(const ATexto: string): string;
begin
  Result := StringReplace(ATexto, '''', '"', [rfReplaceAll]);
end;

function Tx(const ATexto: string): string;
begin
  Result := '{''pt-BR'':''' + ATexto + ''',''en-US'':''' + ATexto + ''',''es-ES'':''' +
    ATexto + '''}';
end;

function TxPtEn(const APt, AEn: string): string;
begin
  Result := '{''pt-BR'':''' + APt + ''',''en-US'':''' + AEn + '''}';
end;

function Regra(const AId, AAcao, AExtra: string; const ATexto: string = ''): string;
var
  vTexto: string;
begin
  vTexto := ATexto;
  if vTexto = '' then
    vTexto := Tx('texto');
  Result := '{''id'':''' + AId + ''',''acao'':''' + AAcao + ''',''onde'':[''dproj''],' +
    '''descricao'':' + vTexto + ',''manual'':' + Tx('manual') + ',''wiki'':''a.md#x''';
  if AExtra <> '' then
    Result := Result + ',' + AExtra;
  Result := Result + '}';
end;

function RegraPacote(const AId: string): string;
begin
  Result := Regra(AId, 'pacote-renomear', '''ide'':''delphi'',''de'':''A'',''para'':''B''');
end;

function Passo(const ADe, APara, ARegras: string; const AComp: string = ''): string;
begin
  Result := '{''formato'':1,''origem'':''RAL'',''de'':''' + ADe + ''',''para'':''' + APara +
    ''',''regras'':[' + ARegras + ']';
  if AComp <> '' then
    Result := Result + ',''comportamento'':[' + AComp + ']';
  Result := Result + '}';
end;

function Carrega(AConjunto: TConjuntoRegras; const AJson: string;
  AEstrito: boolean = True): boolean;
begin
  Result := AConjunto.CarregarTexto(Q(AJson), 'teste.json', AEstrito);
end;

function ErrosTem(AConjunto: TConjuntoRegras; const ATrecho: string): boolean;
begin
  Result := Pos(ATrecho, AConjunto.Erros.Text) > 0;
end;

function ConjuntoTresPassos: TConjuntoRegras;
begin
  Result := TConjuntoRegras.Create;
  Carrega(Result, Passo('1.0', '1.1', RegraPacote('1.1/a')));
  Carrega(Result, Passo('1.1', '1.2', RegraPacote('1.2/a')));
  Carrega(Result, Passo('1.2', '2.0', RegraPacote('2.0/a')));
end;

{ testes }

procedure TesteVersoes;
begin
  WriteLn('versoes e idiomas');
  Conferir(NormalizarVersao('v1.0') = '1.0', 'v1.0 -> 1.0');
  Conferir(NormalizarVersao('dev') = '1.2', 'dev -> 1.2');
  Conferir(NormalizarVersao('1.3-plugin') = '2.0', '1.3-plugin -> 2.0');
  Conferir(NormalizarVersao('2.0') = '2.0', '2.0 -> 2.0');
  Conferir(NormalizarVersao('1.1.3') = '1.1', '1.1.3 -> 1.1');
  Conferir(NormalizarVersao(' V2.0.0-rc1 ') = '2.0', 'V2.0.0-rc1 -> 2.0');
  Conferir(NormalizarVersao('xyz') = '', 'texto que nao e versao -> vazio');
  Conferir(NormalizarVersao('1.') = '', '1. -> vazio');
  Conferir(NormalizarVersao('') = '', 'vazio -> vazio');
  Conferir(CompararVersoes('1.1', '1.2') < 0, '1.1 < 1.2');
  Conferir(CompararVersoes('2.0', 'dev') > 0, '2.0 > dev');
  Conferir(CompararVersoes('1.10', '1.9') > 0, '1.10 > 1.9 (numerico)');
  Conferir(CompararVersoes('v1.0', '1.0') = 0, 'v1.0 = 1.0');
  Conferir(NormalizarIdioma('pt_br') = 'pt-BR', 'pt_br -> pt-BR');
  Conferir(NormalizarIdioma('EN') = 'en-US', 'EN -> en-US');
  Conferir(NormalizarIdioma('es-MX') = 'es-ES', 'es-MX -> es-ES');
  Conferir(NormalizarIdioma('fr') = '', 'fr nao suportado');
  Conferir(RecursoDoPasso('v1.0', '1.1') = 'REGRAS_1_0__1_1', 'nome do recurso 1.0>1.1');
  Conferir(RecursoDoPasso('1.2', 'dev') = 'REGRAS_1_2__1_2', 'nome do recurso usa a versao normalizada');
end;

procedure TesteValido;
var
  vSet: TConjuntoRegras;
  vRegra: TRegraMigracao;
begin
  WriteLn('regra valida');
  vSet := TConjuntoRegras.Create;
  try
    Conferir(Carrega(vSet, Passo('1.0', '1.1', RegraPacote('1.1/pacote-a'),
      '{''id'':''1.1/comp'',''descricao'':' + Tx('d') + ',''fazer'':' + Tx('f') + '}')),
      'passo valido carrega');
    Conferir(vSet.PassoCount = 1, 'um passo');
    vRegra := vSet.BuscarRegra('1.1/pacote-a');
    Conferir(vRegra <> nil, 'BuscarRegra acha pelo id');
    if vRegra <> nil then
    begin
      Conferir(vRegra.Acao = arPacoteRenomear, 'acao');
      Conferir(vRegra.NomeAcao = 'pacote-renomear', 'nome da acao');
      Conferir(vRegra.Param('de') = 'A', 'parametro de');
      Conferir(vRegra.Param('ide') = 'delphi', 'enum ide');
      Conferir(vRegra.AplicaEm('dproj') and not vRegra.AplicaEm('pas'), 'onde');
      Conferir(vRegra.Wiki = 'a.md#x', 'wiki');
      Conferir(vRegra.Texto('descricao', 'es-ES') = 'texto', 'texto por idioma');
      Conferir(vRegra.Condicao = nil, 'sem condicao = sempre');
    end;
    Conferir(vSet.BuscarComportamento('1.1/comp') <> nil, 'comportamento carregado');
    Conferir(vSet.BuscarPasso('RAL', '1.0', '1.1') <> nil, 'BuscarPasso');
  finally
    vSet.Free;
  end;
end;

procedure TesteRecusas;
var
  vSet: TConjuntoRegras;
begin
  WriteLn('recusas');
  vSet := TConjuntoRegras.Create;
  try
    Conferir(not Carrega(vSet, Passo('1.0', '1.1', RegraPacote('1.1/a') + ',' +
      Regra('1.1/b', 'formatar-disco', ''))), 'acao desconhecida recusa o arquivo');
    Conferir(ErrosTem(vSet, 'formatar-disco') and ErrosTem(vSet, '1.1/b'),
      'a mensagem cita a acao e o id');
    Conferir(vSet.PassoCount = 0, 'nada foi carregado (a regra boa tambem fica de fora)');
  finally
    vSet.Free;
  end;
  vSet := TConjuntoRegras.Create;
  try
    Conferir(not Carrega(vSet, Passo('1.0', '1.1', Regra('1.1/a', 'pacote-renomear',
      '''ide'':''delphi'',''de'':''A'',''para'':''B'',''comando'':''del *.*'''))),
      'campo desconhecido recusa');
    Conferir(ErrosTem(vSet, 'comando') and ErrosTem(vSet, '1.1/a'), 'cita o campo e o id');
  finally
    vSet.Free;
  end;
  vSet := TConjuntoRegras.Create;
  try
    Conferir(not Carrega(vSet, Passo('1.0', '1.1', Regra('1.1/a', 'pacote-renomear',
      '''ide'':''delphi'',''de'':''A'''))), 'falta o campo obrigatorio');
    Conferir(ErrosTem(vSet, 'falta o campo "para"'), 'cita o campo que falta');
  finally
    vSet.Free;
  end;
  vSet := TConjuntoRegras.Create;
  try
    Conferir(not Carrega(vSet, Passo('1.0', '1.1', Regra('1.1/a', 'pacote-renomear',
      '''ide'':''turbo'',''de'':''A'',''para'':''B'''))), 'enum invalido recusa');
    Conferir(not Carrega(vSet, Passo('1.0', '1.1', Regra('1.1/a', 'pacote-renomear',
      '''ide'':''ambos'',''de'':''A'',''para'':''B'''))), 'ide ambos sem os nomes do Lazarus recusa');
    Conferir(not Carrega(vSet, Passo('1.0', '1.1', RegraPacote('1.1/a') + ',' +
      RegraPacote('1.1/a'))), 'id repetido no arquivo recusa');
    Conferir(not Carrega(vSet, Passo('1.0', '1.1', RegraPacote('1.2/a'))),
      'estrito: prefixo do id diferente do destino recusa');
    Conferir(Carrega(vSet, Passo('1.0', '1.1', RegraPacote('1.2/a')), False),
      'nao estrito: aceita o prefixo');
    Conferir(not Carrega(vSet, Passo('1.1', '1.0', RegraPacote('1.0/a'))),
      'passo para tras recusa');
    Conferir(not Carrega(vSet, 'isto nao e json'), 'json invalido recusa');
    Conferir(not Carrega(vSet, '[1,2]'), 'raiz que nao e objeto recusa');
  finally
    vSet.Free;
  end;
end;

procedure TesteIdiomas;
var
  vSet: TConjuntoRegras;
  vRegra: TRegraMigracao;
  vJson: string;
begin
  WriteLn('textos por idioma');
  vSet := TConjuntoRegras.Create;
  try
    vJson := Passo('1.0', '1.1', Regra('1.1/a', 'pacote-renomear',
      '''ide'':''delphi'',''de'':''A'',''para'':''B''', TxPtEn('so pt', 'only en')));
    Conferir(not Carrega(vSet, vJson, True), 'estrito: regra sem es-ES recusada');
    Conferir(ErrosTem(vSet, 'es-ES'), 'a mensagem cita o idioma que falta');
    Conferir(Carrega(vSet, vJson, False), 'nao estrito: aceita');
    vRegra := vSet.BuscarRegra('1.1/a');
    Conferir((vRegra <> nil) and (vRegra.Texto('descricao', 'es-ES') = 'only en'),
      'reserva: es-ES cai para en-US');
    Conferir((vRegra <> nil) and (vRegra.Texto('descricao', 'pt-BR') = 'so pt'),
      'o idioma pedido vence');
  finally
    vSet.Free;
  end;
  vSet := TConjuntoRegras.Create;
  try
    vJson := Passo('1.0', '1.1', Regra('1.1/a', 'pacote-renomear',
      '''ide'':''delphi'',''de'':''A'',''para'':''B''', '{''pt-BR'':''so pt''}'));
    Conferir(Carrega(vSet, vJson, False), 'so pt-BR vale no modo nao estrito');
    vRegra := vSet.BuscarRegra('1.1/a');
    Conferir((vRegra <> nil) and (vRegra.Texto('descricao', 'en-US') = 'so pt'),
      'reserva: en-US cai para pt-BR');
    Conferir((vRegra <> nil) and (vRegra.Texto('inexistente', 'pt-BR') = ''),
      'campo de texto que nao existe = vazio');
    Conferir(not Carrega(vSet, Passo('1.0', '1.1', Regra('1.1/b', 'pacote-renomear',
      '''ide'':''delphi'',''de'':''A'',''para'':''B''', '{''fr-FR'':''x''}')), False),
      'idioma que nao existe recusa');
  finally
    vSet.Free;
  end;
  vSet := TConjuntoRegras.Create;
  try
    // 'ç' em UTF-8 (#195#167) atravessa o leitor sem virar outra coisa
    vJson := Passo('1.0', '1.1', Regra('1.1/a', 'pacote-renomear',
      '''ide'':''delphi'',''de'':''A'',''para'':''B''', Tx('a' + #195#167 + 'ao')));
    Conferir(Carrega(vSet, vJson), 'texto com acento carrega');
    vRegra := vSet.BuscarRegra('1.1/a');
    Conferir((vRegra <> nil) and (vRegra.Texto('manual', 'pt-BR') = 'manual') and
      (vRegra.Texto('descricao', 'pt-BR') = 'a' + #195#167 + 'ao'), 'acento preservado byte a byte');
  finally
    vSet.Free;
  end;
end;

procedure TesteParametros;
var
  vSet: TConjuntoRegras;
  vRegra: TRegraMigracao;
begin
  WriteLn('parametros, padrao e condicoes');
  vSet := TConjuntoRegras.Create;
  try
    Conferir(Carrega(vSet, Passo('1.2', '2.0', Regra('2.0/comp', 'propriedade-para-componente',
      '''classe-dona'':''TRAL*Server'',''propriedades'':[''CORSOptions.AllowOrigin'',''CORSOptions.MaxAge''],' +
      '''mapeamento'':{''CORSOptions.AllowOrigin'':''Options.AllowOrigin''},''componente'':''TRALCORSPlugin'',' +
      '''unit'':''RALCORS'',''nome-base'':''RALCORSPlugin'',''ligacao'':''Server'',''descartar'':[''X''],' +
      '''criar-quando'':{''tipo'':''alguma'',''condicoes'':[{''tipo'':''valor-diferente'',''propriedade'':''P'',''valor'':''crNone''},' +
      '{''tipo'':''set-contem'',''propriedade'':''Security.Options'',''opcao'':''rsoFloodProtection'',''negar'':true}]},' +
      '''condicao'':{''tipo'':''classe-descende-de'',''classe'':''TRALServer''}'))), 'componente completo carrega');
    vRegra := vSet.BuscarRegra('2.0/comp');
    if vRegra <> nil then
    begin
      Conferir(vRegra.Lista('propriedades').Count = 2, 'lista de propriedades');
      Conferir(vRegra.Mapa('mapeamento').Values['CORSOptions.AllowOrigin'] = 'Options.AllowOrigin', 'mapeamento');
      Conferir(vRegra.Lista('inexistente').Count = 0, 'lista ausente e vazia, nunca nil');
      Conferir((vRegra.CriarQuando <> nil) and (vRegra.CriarQuando.Tipo = tcAlguma) and
        (vRegra.CriarQuando.FilhoCount = 2), 'criar-quando composto');
      Conferir((vRegra.CriarQuando <> nil) and (vRegra.CriarQuando.Descrever =
        'alguma(valor-diferente(P,crNone);nao set-contem(Security.Options,rsoFloodProtection))'),
        'Descrever da condicao');
      Conferir((vRegra.Condicao <> nil) and (vRegra.Condicao.Tipo = tcClasseDescendeDe) and
        (vRegra.Condicao.Classe = 'TRALServer'), 'condicao classe-descende-de');
    end;
    Conferir(not Carrega(vSet, Passo('1.2', '2.0', Regra('2.0/x', 'valor-padrao-materializar',
      '''classe'':''C'',''propriedade'':''P'',''valor'':''1'',''condicao'':{''tipo'':''inventada''}'))),
      'condicao desconhecida recusa');
    Conferir(not Carrega(vSet, Passo('1.2', '2.0', Regra('2.0/x', 'valor-padrao-materializar',
      '''classe'':''C'',''propriedade'':''P'',''valor'':''1'',''condicao'':{''tipo'':''valor-igual'',''propriedade'':''P''}'))),
      'condicao sem o valor recusa');
    Conferir(not Carrega(vSet, Passo('1.2', '2.0', Regra('2.0/x', 'padrao-avisar',
      '''padrao'':{''tipo'':''conversao-tipo'',''tipo-alvo'':''TMemoryStream''}'))),
      'padrao sem o membro recusa');
    Conferir(not Carrega(vSet, Passo('1.2', '2.0', Regra('2.0/x', 'padrao-avisar',
      '''padrao'':{''tipo'':''regex'',''expressao'':''.*''}'))), 'padrao de tipo desconhecido recusa');
    Conferir(Carrega(vSet, Passo('1.2', '2.0', Regra('2.0/p', 'padrao-avisar',
      '''padrao'':{''tipo'':''conversao-tipo'',''tipo-alvo'':''TMemoryStream'',''membro'':''Content''},' +
      '''trecho'':''x''') + ',' + Regra('2.0/q', 'caminho-trocar',
      '''de'':''base'',''modo'':''trocar'',''para'':[''core'',''storage''],' +
      '''para-se-unit'':{''plugins\\auth'':[''RALAuthentication'',''RALDigest'']}'))), 'padrao e caminho validos');
    vRegra := vSet.BuscarRegra('2.0/p');
    Conferir((vRegra <> nil) and (vRegra.PadraoTipo = 'conversao-tipo'), 'PadraoTipo');
    vRegra := vSet.BuscarRegra('2.0/q');
    Conferir((vRegra <> nil) and (vRegra.Mapa('para-se-unit').Values['plugins\auth'] =
      'RALAuthentication,RALDigest'), 'lista dentro do mapa vira texto separado por virgula');
  finally
    vSet.Free;
  end;
end;

procedure TesteCadeia;
var
  vSet: TConjuntoRegras;
  vLista: TListaPassos;
  vErro: TCadeiaErro;
  vMsg: string;
begin
  WriteLn('cadeia de passos');
  vSet := ConjuntoTresPassos;
  try
    vLista := vSet.Cadeia('RAL', '1.1', '2.0', vErro, vMsg);
    Conferir((vErro = ceNenhum) and (vLista.Count = 2) and (vLista[0].Rotulo = '1.1>1.2') and
      (vLista[1].Rotulo = '1.2>2.0'), '1.1 -> 2.0 = 1.1>1.2 + 1.2>2.0');
    vLista.Free;
    vLista := vSet.Cadeia('RAL', 'v1.0', '2.0', vErro, vMsg);
    Conferir((vErro = ceNenhum) and (vLista.Count = 3), '1.0 -> 2.0 = tres passos');
    vLista.Free;
    vLista := vSet.Cadeia('RAL', 'dev', '1.3-plugin', vErro, vMsg);
    Conferir((vErro = ceNenhum) and (vLista.Count = 1) and (vLista[0].Rotulo = '1.2>2.0'),
      'dev -> 1.3-plugin = 1.2>2.0');
    vLista.Free;
    vLista := vSet.Cadeia('RAL', '1.2', '1.2', vErro, vMsg);
    Conferir((vErro = ceNenhum) and (vLista.Count = 0), 'mesma versao = cadeia vazia, sem erro');
    vLista.Free;
    vLista := vSet.Cadeia('RAL', '2.0', '1.1', vErro, vMsg);
    Conferir((vErro = ceRetrocesso) and (vLista.Count = 0) and (vMsg <> ''),
      '2.0 -> 1.1 recusado (so para a frente) com o motivo');
    vLista.Free;
    vLista := vSet.Cadeia('RAL', '9.9', '2.0', vErro, vMsg);
    Conferir((vErro = ceVersaoDesconhecida) and (vMsg <> ''), 'origem desconhecida e erro');
    vLista.Free;
    vLista := vSet.Cadeia('RAL', '1.1', 'banana', vErro, vMsg);
    Conferir(vErro = ceVersaoDesconhecida, 'destino que nao e versao e erro');
    vLista.Free;
    vLista := vSet.Cadeia('RESTDW', '1.1', '2.0', vErro, vMsg);
    Conferir(vErro = ceVersaoDesconhecida, 'outra origem sem regras e erro');
    vLista.Free;
  finally
    vSet.Free;
  end;
  vSet := TConjuntoRegras.Create;
  try
    Carrega(vSet, Passo('1.0', '1.1', RegraPacote('1.1/a')));
    Carrega(vSet, Passo('1.2', '2.0', RegraPacote('2.0/a')));
    vLista := vSet.Cadeia('RAL', '1.0', '2.0', vErro, vMsg);
    Conferir((vErro = ceSemPasso) and (vLista.Count = 0), 'buraco na cadeia = erro');
    vLista.Free;
    vLista := vSet.Cadeia('RAL', '1.0', '1.1', vErro, vMsg);
    Conferir((vErro = ceNenhum) and (vLista.Count = 1), 'o pedaco que existe funciona');
    vLista.Free;
  finally
    vSet.Free;
  end;
end;

procedure TesteSobreposicao;
var
  vSet: TConjuntoRegras;
  vLista: TListaPassos;
  vErro: TCadeiaErro;
  vMsg, vPasta: string;
  vArquivo: TStringList;
begin
  WriteLn('ralmigracao.json (sobreposicao)');
  vSet := ConjuntoTresPassos;
  try
    Conferir(vSet.BuscarRegra('2.0/a').Wiki = 'a.md#x', 'regra embutida');
    Conferir(vSet.AplicarSobreposicaoTexto(Q('{''passos'':[' +
      Passo('1.2', '2.0', StringReplace(RegraPacote('2.0/a'), 'a.md#x', 'nova.md', []) + ',' +
      RegraPacote('2.0/nova'), '{''id'':''2.0/cn'',''descricao'':' + Tx('d') + ',''fazer'':' + Tx('f') + '}') + ',' +
      Passo('2.0', '2.1', RegraPacote('2.1/z')) + ']}'), 'ralmigracao.json'),
      'sobreposicao em {"passos": [...]} carrega');
    Conferir(vSet.PassoCount = 4, 'passo novo acrescentado');
    Conferir(vSet.BuscarPasso('RAL', '1.2', '2.0').RegraCount = 2, 'regra nova acrescentada, a mesma id nao duplica');
    Conferir(vSet.BuscarRegra('2.0/a').Wiki = 'nova.md', 'mesmo id substitui');
    Conferir(vSet.BuscarComportamento('2.0/cn') <> nil, 'comportamento acrescentado');
    vLista := vSet.Cadeia('RAL', '1.1', '2.1', vErro, vMsg);
    Conferir((vErro = ceNenhum) and (vLista.Count = 3), 'a cadeia passa a chegar na 2.1');
    vLista.Free;
    // a sobreposicao com erro nao muda nada
    Conferir(not vSet.AplicarSobreposicaoTexto(Q(Passo('1.2', '2.0', Regra('2.0/a', 'rm', ''))), 'ruim.json'),
      'sobreposicao com acao desconhecida recusada');
    Conferir(vSet.BuscarRegra('2.0/a').Wiki = 'nova.md', 'recusada nao muda nada');
  finally
    vSet.Free;
  end;
  // arquivo na raiz do RAL de destino
  vSet := ConjuntoTresPassos;
  vArquivo := TStringList.Create;
  try
    vPasta := IncludeTrailingPathDelimiter(GetTempDir) + 'ralmig_regras_teste';
    ForceDirectories(vPasta);
    Conferir(vSet.AplicarSobreposicaoDaRaiz(vPasta), 'raiz sem ralmigracao.json = nada a fazer');
    vArquivo.Text := Q(Passo('2.0', '2.1', RegraPacote('2.1/z')));
    vArquivo.SaveToFile(IncludeTrailingPathDelimiter(vPasta) + ArquivoSobreposicao);
    Conferir(vSet.AplicarSobreposicaoDaRaiz(vPasta) and (vSet.PassoCount = 4),
      'ralmigracao.json da raiz e lido (um passo so, sem "passos")');
    DeleteFile(IncludeTrailingPathDelimiter(vPasta) + ArquivoSobreposicao);
    RemoveDir(vPasta);
  finally
    vArquivo.Free;
    vSet.Free;
  end;
end;

procedure AutoTeste;
begin
  TesteVersoes;
  TesteValido;
  TesteRecusas;
  TesteIdiomas;
  TesteParametros;
  TesteCadeia;
  TesteSobreposicao;
end;

{ modo real }


procedure Resumo(ASet: TConjuntoRegras);
var
  vInt, vIt, vInd: integer;
  vPasso: TPassoMigracao;
  vContagem: TStringList;
begin
  vContagem := TStringList.Create;
  try
    vContagem.Sorted := True;
    for vInt := 0 to Pred(ASet.PassoCount) do
    begin
      vPasso := ASet.Passos[vInt];
      vContagem.Clear;
      for vIt := 0 to Pred(vPasso.RegraCount) do
      begin
        vInd := vContagem.IndexOf(vPasso.Regras[vIt].NomeAcao);
        if vInd < 0 then
          vContagem.AddObject(vPasso.Regras[vIt].NomeAcao, TObject(PtrUInt(1)))
        else
          vContagem.Objects[vInd] := TObject(PtrUInt(vContagem.Objects[vInd]) + 1);
      end;
      WriteLn('  ', vPasso.Origem, ' ', vPasso.Rotulo, ': ', vPasso.RegraCount,
              ' regras, ', vPasso.CompCount, ' itens de comportamento');
      for vIt := 0 to Pred(vContagem.Count) do
        WriteLn('      ', vContagem[vIt], ': ', PtrUInt(vContagem.Objects[vIt]));
    end;
  finally
    vContagem.Free;
  end;
end;

procedure CitarRegras(const ALista: string; ACitados: TStringList);
var
  vItens: TStringList;
  vInt: integer;
begin
  vItens := TStringList.Create;
  try
    vItens.CommaText := StringReplace(ALista, ' ', '', [rfReplaceAll]);
    for vInt := 0 to Pred(vItens.Count) do
      if vItens[vInt] <> '' then
        ACitados.Add(vItens[vInt]);
  finally
    vItens.Free;
  end;
end;

procedure ConferirCobertura(ASet: TConjuntoRegras; const AArquivo: string);
var
  vCob: TCobertura;
  vCitadas, vCitadasComp: TStringList;
  vPasso: TCobPasso;
  vItem: TCobItem;
  vInt, vIt, vPos: integer;
  vId: string;
  vPassoRegras: TPassoMigracao;
begin
  vCob := TCobertura.Create;
  vCitadas := TStringList.Create;
  vCitadasComp := TStringList.Create;
  try
    vCitadas.Sorted := True;
    vCitadas.Duplicates := dupIgnore;
    vCitadasComp.Sorted := True;
    vCitadasComp.Duplicates := dupIgnore;
    if not vCob.CarregarArquivo(AArquivo) then
    begin
      Conferir(False, 'cobertura.json carrega: ' + vCob.Erros.Text);
      Exit;
    end;
    for vInt := 0 to Pred(vCob.PassoCount) do
    begin
      vPasso := vCob.Passos[vInt];
      for vIt := 0 to Pred(vPasso.Itens.Count) do
      begin
        vItem := TCobItem(vPasso.Itens[vIt]);
        if vItem.Regra <> '' then
          CitarRegras(vItem.Regra, vCitadas);
      end;
    end;
    for vInt := 0 to Pred(vCitadas.Count) do
      Conferir(ASet.BuscarRegra(vCitadas[vInt]) <> nil, 'cobertura cita ' + vCitadas[vInt]);
    for vInt := 0 to Pred(vCob.CompCount) do
    begin
      vId := vCob.Comportamento[vInt].Id;
      vCitadasComp.Add(vId);
      Conferir(ASet.BuscarComportamento(vId) <> nil, 'comportamento da cobertura ' + vId);
    end;
    // o que sobra: regras e itens que nenhuma entrada da cobertura cita
    WriteLn('ids que sobram (aviso):');
    vPos := 0;
    for vInt := 0 to Pred(ASet.PassoCount) do
    begin
      vPassoRegras := ASet.Passos[vInt];
      for vIt := 0 to Pred(vPassoRegras.RegraCount) do
        if vCitadas.IndexOf(vPassoRegras.Regras[vIt].Id) < 0 then
        begin
          WriteLn('  regra ', vPassoRegras.Regras[vIt].Id);
          Inc(vPos);
        end;
      for vIt := 0 to Pred(vPassoRegras.CompCount) do
        if vCitadasComp.IndexOf(vPassoRegras.Comportamento[vIt].Id) < 0 then
        begin
          WriteLn('  comportamento ', vPassoRegras.Comportamento[vIt].Id);
          Inc(vPos);
        end;
    end;
    if vPos = 0 then
      WriteLn('  (nenhum)');
  finally
    vCitadasComp.Free;
    vCitadas.Free;
    vCob.Free;
  end;
end;

procedure ModoReal;
var
  vSet: TConjuntoRegras;
  vArq: string;
begin
  WriteLn('regras reais em ', GPasta);
  vSet := TConjuntoRegras.Create;
  try
    Conferir(vSet.CarregarDePasta(GPasta, True), 'os arquivos de regras carregam (estrito: tres idiomas em todo texto)');
    if vSet.Erros.Count > 0 then
      WriteLn(vSet.Erros.Text);
    Conferir(vSet.PassoCount >= 3, 'ha pelo menos os tres passos');
    Resumo(vSet);
    vArq := GCobertura;
    if vArq = '' then
      vArq := IncludeTrailingPathDelimiter(GPasta) + 'cobertura.json';
    if FileExists(vArq) then
      ConferirCobertura(vSet, vArq)
    else
      Conferir(False, 'cobertura.json existe: ' + vArq);
  finally
    vSet.Free;
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
    if vArg = '--testes' then
      GSoTestes := True
    else if Copy(vArg, 1, 8) = '--pasta=' then
      GPasta := Copy(vArg, 9, MaxInt)
    else if Copy(vArg, 1, 12) = '--cobertura=' then
      GCobertura := Copy(vArg, 13, MaxInt);
  end;
end;

begin
  DefaultSystemCodePage := CP_UTF8;
  LerArgumentos;
  if GSoTestes or (GPasta = '') then
    AutoTeste;
  if GPasta <> '' then
    ModoReal;
  if GFalhas > 0 then
  begin
    WriteLn(GFalhas, ' falha(s)');
    Halt(1);
  end;
  WriteLn('tudo certo');
end.
