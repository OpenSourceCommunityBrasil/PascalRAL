program inventario;

{$mode ObjFPC}{$H+}

// Inventario das quebras do RAL (migrador, etapa M0).
//   inventario --testes              autoteste com APIs sinteticas em memoria
//   inventario [--pre-lancamento]    modo real: le migrador/api/*.json e
//                                    migrador/regras/cobertura.json, lista toda
//                                    diferenca sem regra nem justificativa e sai
//                                    com 1 se houver pendente num passo fechado
//                                    (em qualquer passo com --pre-lancamento)
//   --raiz=<pasta migrador>          onde procurar api/ e regras/
//
// Compilar (da pasta do teste):
//   fpc -B -Fu../../src -Fu../../../src/classes inventario.lpr

uses
  Classes, SysUtils, Contnrs,
  RALMig.API, RALMig.Inventario;

var
  GFalhas: integer = 0;
  GRaiz: string = '';
  GPreLancamento: boolean = False;
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

{ APIs sinteticas }

procedure Membro(AClasse: TApiClasse; const ANome, ACat, AForma, ATipo: string;
  const APadrao: string = ''; const AAssinatura: string = '');
var
  vMembro: TApiMembro;
begin
  vMembro := AClasse.Obter(ANome);
  vMembro.Cat := ACat;
  vMembro.Forma := AForma;
  vMembro.Tipo := ATipo;
  vMembro.Padrao := APadrao;
  if AAssinatura <> '' then
    vMembro.Assinaturas.Add(AAssinatura);
end;

function ApiBase: TApi;
var
  vCls: TApiClasse;
  vTipo: TApiTipo;
begin
  Result := TApi.Create;
  Result.Rotulo := 'a';
  Result.AddUnidade('UnitA', 'src/UnitA.pas');
  Result.AddUnidade('UnitB', 'src/UnitB.pas');
  Result.AddUnidade('UnitVelha', 'src/UnitVelha.pas');

  vCls := Result.AddClasse('TBase', 'UnitA', 'TComponent', 'class');
  Membro(vCls, 'Ativo', 'published', 'prop', 'Boolean', 'False');
  Membro(vCls, 'Nome', 'published', 'prop', 'string');
  Membro(vCls, 'Abrir', 'public', 'metodo', '', '', 'procedure(string)');
  Membro(vCls, 'Fechar', 'public', 'metodo', '', '', 'procedure()');

  vCls := Result.AddClasse('TFilho', 'UnitA', 'TBase', 'class');
  Membro(vCls, 'Tempo', 'published', 'prop', 'Integer', 'DEFTEMPO');
  Membro(vCls, 'OnFim', 'published', 'prop', 'TOnFim');
  Membro(vCls, 'Cor', 'published', 'prop', 'TCor');

  Result.AddClasse('TSumida', 'UnitA', 'TObject', 'class');
  Result.AddClasse('TMovida', 'UnitA', 'TObject', 'class');
  vCls := Result.AddClasse('TComAncestral', 'UnitB', 'TObject', 'class');
  Membro(vCls, 'Valor', 'public', 'prop', 'Integer');
  vCls := Result.AddClasse('TNaPaleta', 'UnitB', 'TComponent', 'class');
  Membro(vCls, 'X', 'public', 'campo', 'Integer');
  Result.AddClasse('TDaUnitVelha', 'UnitVelha', 'TObject', 'class');
  Result.Registradas.Add('TNaPaleta=RAL - Server');

  vTipo := Result.AddTipo('TCor', 'UnitA', 'enum', '');
  vTipo.Valores.Add('cRed');
  vTipo.Valores.Add('cGreen');
  Result.AddTipo('TOnFim', 'UnitA', 'proc', 'procedure(TObject) of object');
  Result.AddTipo('TTipoSumido', 'UnitA', 'outro', 'Integer');

  Result.AddRotina('UnitA', 'Faz', 'procedure(string)');
  Result.AddRotina('UnitA', 'RotinaSumida', 'procedure()');
  Result.AddConstante('UnitA', 'DEFTEMPO', '5000');
  Result.AddConstante('UnitA', 'CSumida', '1');

  Result.AddPacote('delphi', 'PacA').Unidades.CommaText := 'UnitA,UnitB,UnitVelha';
  Result.AddPacote('delphi', 'PacSumido').Unidades.CommaText := 'UnitB';
  Result.AddPacote('lazarus', 'pacsumido').Unidades.CommaText := 'UnitB';
end;

/// B: what changed relative to ApiBase, one thing per kind
function ApiNova: TApi;
var
  vCls: TApiClasse;
  vTipo: TApiTipo;
begin
  Result := TApi.Create;
  Result.Rotulo := 'b';
  Result.AddUnidade('UnitA', 'src/novo/UnitA.pas');
  Result.AddUnidade('UnitB', 'src/UnitB.pas');
  Result.AddUnidade('UnitNova', 'src/UnitNova.pas');

  vCls := Result.AddClasse('TBase', 'UnitA', 'TPersistent', 'class');
  Membro(vCls, 'Ativo', 'published', 'prop', 'Boolean', 'True');
  Membro(vCls, 'Abrir', 'public', 'metodo', '', '', 'procedure(string; Integer)');

  vCls := Result.AddClasse('TFilho', 'UnitA', 'TBase', 'class');
  Membro(vCls, 'Tempo', 'published', 'prop', 'Int64', '5000');
  Membro(vCls, 'OnFim', 'published', 'prop', 'TOnFim');
  Membro(vCls, 'Cor', 'published', 'prop', 'TCor');

  Result.AddClasse('TMovida', 'UnitNova', 'TObject', 'class');
  vCls := Result.AddClasse('TComAncestral', 'UnitB', 'TObject', 'class');
  Membro(vCls, 'Valor', 'public', 'prop', 'Integer');
  Result.AddClasse('TNaPaleta', 'UnitB', 'TComponent', 'class');
  Result.AddClasse('TDaUnitVelha', 'UnitB', 'TObject', 'class');

  vTipo := Result.AddTipo('TCor', 'UnitA', 'enum', '');
  vTipo.Valores.Add('cRed');
  Result.AddTipo('TOnFim', 'UnitA', 'proc', 'procedure(TObject; Integer) of object');

  Result.AddRotina('UnitA', 'Faz', 'procedure(string; Integer)');
  Result.AddConstante('UnitA', 'DEFTEMPO', '5000');

  Result.AddPacote('delphi', 'PacA').Unidades.CommaText := 'UnitA';
  Result.AddPacote('delphi', 'PacNovo').Unidades.CommaText := 'UnitB';
end;

function TemChave(ADif: TDiferencas; const AChave: string): boolean;
begin
  Result := ADif.Buscar(AChave) <> nil;
end;

procedure TesteDiferencas;
var
  vA, vB: TApi;
  vDif: TDiferencas;
begin
  WriteLn('diferencas entre duas APIs');
  vA := ApiBase;
  vB := ApiNova;
  vDif := TDiferencas.Create;
  try
    CompararAPIs(vA, vB, vDif);
    Conferir(TemChave(vDif, 'unit-sumiu:UnitVelha'), 'unit-sumiu');
    Conferir((vDif.Buscar('unit-pasta-mudou:UnitA') <> nil) and
             (vDif.Buscar('unit-pasta-mudou:UnitA').Detalhe = 'src -> src/novo'), 'unit-pasta-mudou');
    Conferir(not TemChave(vDif, 'unit-pasta-mudou:UnitB'), 'pasta igual nao aparece');
    Conferir(TemChave(vDif, 'classe-sumiu:TSumida'), 'classe-sumiu');
    Conferir(TemChave(vDif, 'classe-mudou-unit:TMovida'), 'classe-mudou-unit');
    Conferir((vDif.Buscar('classe-mudou-unit:TMovida') <> nil) and
             (vDif.Buscar('classe-mudou-unit:TMovida').Detalhe = 'UnitA -> UnitNova'),
             'classe-mudou-unit traz "UnitA -> UnitNova"');
    Conferir(not TemChave(vDif, 'classe-sumiu:TMovida'), 'classe movida nao gera classe-sumiu');
    Conferir(TemChave(vDif, 'ancestral-mudou:TBase'), 'ancestral-mudou');
    Conferir(TemChave(vDif, 'published-sumiu:TBase.Nome'), 'published-sumiu na propria classe');
    Conferir(TemChave(vDif, 'published-sumiu:TFilho.Nome'),
             'published-sumiu herdado pelo descendente (achatado)');
    Conferir(TemChave(vDif, 'published-tipo:TFilho.Tempo'), 'published-tipo');
    Conferir(TemChave(vDif, 'default-mudou:TBase.Ativo'), 'default-mudou');
    Conferir(not TemChave(vDif, 'default-mudou:TFilho.Tempo'),
             'default dado por constante vale o valor da constante');
    Conferir(TemChave(vDif, 'evento-mudou:TOnFim'), 'evento-mudou');
    Conferir(TemChave(vDif, 'publico-sumiu:TBase.Fechar'), 'publico-sumiu');
    Conferir(TemChave(vDif, 'assinatura-mudou:TBase.Abrir'), 'assinatura-mudou');
    Conferir(TemChave(vDif, 'registro-sumiu:TNaPaleta'), 'registro-sumiu');
    Conferir(TemChave(vDif, 'tipo-sumiu:TTipoSumido'), 'tipo-sumiu');
    Conferir(TemChave(vDif, 'enum-valor-sumiu:TCor.cGreen'), 'enum-valor-sumiu');
    Conferir(TemChave(vDif, 'rotina-sumiu:UnitA.RotinaSumida'), 'rotina-sumiu');
    Conferir(TemChave(vDif, 'rotina-mudou:UnitA.Faz'), 'rotina-mudou');
    Conferir(TemChave(vDif, 'constante-sumiu:UnitA.CSumida'), 'constante-sumiu');
    Conferir(TemChave(vDif, 'pacote-sumiu:delphi/PacSumido'), 'pacote-sumiu delphi');
    Conferir(TemChave(vDif, 'pacote-sumiu:lazarus/pacsumido'), 'pacote-sumiu lazarus');
    Conferir(TemChave(vDif, 'pacote-perdeu-unit:delphi/PacA/UnitB'), 'pacote-perdeu-unit');
    Conferir((vDif.Buscar('pacote-perdeu-unit:delphi/PacA/UnitB') <> nil) and
             (Pos('PacNovo', vDif.Buscar('pacote-perdeu-unit:delphi/PacA/UnitB').Detalhe) > 0),
             'pacote-perdeu-unit diz em que pacote a unit esta agora');
    Conferir(not TemChave(vDif, 'pacote-perdeu-unit:delphi/PacA/UnitVelha'),
             'unit que sumiu nao gera pacote-perdeu-unit (unit-sumiu cobre)');
    Conferir(not TemChave(vDif, 'tipo-sumiu:TCor') and not TemChave(vDif, 'published-sumiu:TFilho.Cor'),
             'o que continua igual nao aparece');
  finally
    vDif.Free;
    vA.Free;
    vB.Free;
  end;
end;

procedure TesteClasseSumida;
var
  vA, vB: TApi;
  vDif: TDiferencas;
  vInt: integer;
  vMembros: boolean;
begin
  WriteLn('classe que sumiu nao lista os membros de novo');
  vA := ApiBase;
  vB := ApiNova;
  vDif := TDiferencas.Create;
  try
    vA.AddClasse('TGrande', 'UnitA', 'TComponent', 'class');
    Membro(vA.BuscarClasse('TGrande'), 'Prop', 'published', 'prop', 'Integer');
    Membro(vA.BuscarClasse('TGrande'), 'Metodo', 'public', 'metodo', '', '', 'procedure()');
    CompararAPIs(vA, vB, vDif);
    vMembros := False;
    for vInt := 0 to Pred(vDif.Count) do
      if Pos('TGrande.', vDif[vInt].Chave) > 0 then
        vMembros := True;
    Conferir(TemChave(vDif, 'classe-sumiu:TGrande'), 'classe-sumiu');
    Conferir(not vMembros, 'nenhum membro de TGrande listado');
  finally
    vDif.Free;
    vA.Free;
    vB.Free;
  end;
end;

procedure TesteIgual;
var
  vA, vB: TApi;
  vDif: TDiferencas;
begin
  WriteLn('duas APIs iguais');
  vA := ApiBase;
  vB := ApiBase;
  vDif := TDiferencas.Create;
  try
    CompararAPIs(vA, vB, vDif);
    Conferir(vDif.Count = 0, 'nenhuma diferenca');
  finally
    vDif.Free;
    vA.Free;
    vB.Free;
  end;
end;

procedure TesteJSON;
var
  vA, vB: TApi;
  vTexto: string;
  vDif: TDiferencas;
begin
  WriteLn('JSON da API');
  vA := ApiBase;
  vB := TApi.Create;
  vDif := TDiferencas.Create;
  try
    vTexto := vA.ParaJSON;
    vB.CarregarJSON(vTexto);
    CompararAPIs(vA, vB, vDif);
    Conferir(vDif.Count = 0, 'ler o que foi gravado nao muda nada');
    Conferir(vB.ParaJSON = vTexto, 'gravar de novo da o mesmo texto (estavel)');
    Conferir(Pos(#13, vTexto) = 0, 'fim de linha LF');
  finally
    vDif.Free;
    vA.Free;
    vB.Free;
  end;
end;

procedure TesteGlob;
begin
  WriteLn('glob');
  Conferir(CasaGlob('published-sumiu:TRAL*Server.CompressType',
                    'published-sumiu:TRALIndyServer.CompressType'), '* no meio');
  Conferir(CasaGlob('CLASSE-SUMIU:trali*', 'classe-sumiu:TRALInternalX'), 'sem diferenciar caixa');
  Conferir(CasaGlob('a?c', 'abc') and not CasaGlob('a?c', 'ac'), '? e um caractere');
  Conferir(not CasaGlob('published-sumiu:TRAL*Server.CompressType',
                        'published-sumiu:TRALIndyServer.CORSOptions'), 'nao casa outro alvo');
  Conferir(CasaGlob('*', '') and CasaGlob('a*', 'a') and not CasaGlob('a', 'ab'), 'bordas');
  Conferir(CasaGlob('*:T*.Prop', 'x:TFoo.Prop') and not CasaGlob('*:T*.Prop', 'x:TFoo.Prop2'),
           'varios curingas');
end;

const
  CCobertura =
    '{"passos":[{"de":"a","para":"b","fechado":true,"itens":[' +
    '{"chave":"classe-sumiu:TSum*","justificativa":"interna"},' +
    '{"chave":"published-sumiu:TBase.*","regra":"2.0/x"},' +
    '{"chave":"nada-casa:*","justificativa":"obsoleto"}]}],' +
    '"comportamento":[{"passo":"a>b","id":"1.2/cors","descricao":"CORS | fechado",' +
    '"fazer":"abrir","wiki":"atualizacao.md#cors"}]}';

procedure TesteCobertura;
var
  vCob: TCobertura;
begin
  WriteLn('arquivo de cobertura');
  vCob := TCobertura.Create;
  try
    Conferir(vCob.CarregarTexto(CCobertura), 'cobertura valida carrega');
    Conferir((vCob.PassoCount = 1) and vCob.Passos[0].Fechado and (vCob.CompCount = 1),
             'passo fechado e um comportamento');
    Conferir(not vCob.CarregarTexto(
      '{"passos":[{"de":"a","para":"b","itens":[{"chave":"x:y","regra":"r","justificativa":"j"}]}]}'),
      'item com regra e justificativa juntos e recusado');
    Conferir(vCob.Erros.Count > 0, 'o erro e dito');
    Conferir(not vCob.CarregarTexto(
      '{"passos":[{"de":"a","para":"b","itens":[{"chave":"x:y"}]}]}'),
      'item sem regra nem justificativa e recusado');
    Conferir(not vCob.CarregarTexto('{"passos":'), 'JSON quebrado e recusado');
    Conferir(not vCob.CarregarTexto(
      '{"comportamento":[{"passo":"a>b","id":"x","descricao":"d"},{"passo":"a>b","id":"x","descricao":"d"}]}'),
      'id de comportamento repetido e recusado');
  finally
    vCob.Free;
  end;
end;

procedure TesteSeparar;
var
  vCob: TCobertura;
  vA, vB: TApi;
  vDif: TDiferencas;
  vRes: TResultadoPasso;
begin
  WriteLn('cobertas, pendentes e obsoletos');
  vCob := TCobertura.Create;
  vA := ApiBase;
  vB := ApiNova;
  vDif := TDiferencas.Create;
  try
    vCob.CarregarTexto(CCobertura);
    CompararAPIs(vA, vB, vDif);
    vRes := TResultadoPasso.Create(vCob.Passos[0]);
    try
      Cobrir(vCob.Passos[0], vDif, vRes);
      Conferir(vRes.Cobertas.Count = 2, 'cobertas: classe-sumiu:TSumida e published-sumiu:TBase.Nome');
      Conferir(vRes.Pendentes.Count = vDif.Count - 2, 'o resto fica pendente');
      Conferir(vRes.Obsoletos.IndexOf('nada-casa:*') >= 0, 'item sem correspondencia e avisado');
      Conferir(vRes.Obsoletos.Count = 1, 'so ele');
    finally
      vRes.Free;
    end;
  finally
    vDif.Free;
    vA.Free;
    vB.Free;
    vCob.Free;
  end;
end;

procedure TesteRelatorio;
var
  vCob: TCobertura;
  vA, vB: TApi;
  vDif: TDiferencas;
  vRes: TResultadoPasso;
  vLista: TObjectList;
  vTexto: string;
begin
  WriteLn('relatorio INVENTARIO.md');
  vCob := TCobertura.Create;
  vA := ApiBase;
  vB := ApiNova;
  vDif := TDiferencas.Create;
  vLista := TObjectList.Create(True);
  try
    vCob.CarregarTexto(CCobertura);
    CompararAPIs(vA, vB, vDif);
    vRes := TResultadoPasso.Create(vCob.Passos[0]);
    Cobrir(vCob.Passos[0], vDif, vRes);
    vRes.Diferencas.Free;
    vRes.Diferencas := vDif;
    vDif := nil;
    vLista.Add(vRes);
    vTexto := GerarInventario(vCob, vLista);
    Conferir(Pos('### Pendentes', vTexto) > 0, 'secao de pendentes');
    Conferir(Pos('### Pendentes', vTexto) < Pos('### Cobertas', vTexto), 'pendentes antes das cobertas');
    Conferir(Pos('regra `2.0/x`', vTexto) > 0, 'regra aparece na tabela');
    Conferir(Pos('interna', vTexto) > 0, 'justificativa aparece na tabela');
    Conferir(Pos('CORS \| fechado', vTexto) > 0, 'a barra dentro da celula e protegida');
    Conferir(Pos('1.2/cors', vTexto) > 0, 'comportamento aparece');
    Conferir(Pos(#13, vTexto) = 0, 'fim de linha LF');
  finally
    vLista.Free;
    vDif.Free;
    vA.Free;
    vB.Free;
    vCob.Free;
  end;
end;

procedure TesteRegistros;
var
  vLista: TStringList;
begin
  WriteLn('RegisterComponents');
  vLista := TStringList.Create;
  try
    AcharRegistros(
      'procedure Register;'#10'begin'#10 +
      '  // RegisterComponents(''Comentario'', [TFalso]);'#10 +
      '  {RegisterComponents(''Chave'', [TFalso2]);}'#10 +
      '  RegisterComponents(''RAL - Server'', [TRALA,'#10'    TRALB]);'#10 +
      '  registercomponents(''RAL - Client'', [TRALC]);'#10'end;'#10, vLista);
    Conferir(vLista.Values['TRALA'] = 'RAL - Server', 'primeira classe e a pagina');
    Conferir(vLista.Values['TRALB'] = 'RAL - Server', 'lista em duas linhas');
    Conferir(vLista.Values['TRALC'] = 'RAL - Client', 'sem diferenciar caixa');
    Conferir((vLista.IndexOfName('TFalso') < 0) and (vLista.IndexOfName('TFalso2') < 0),
             'comentario nao conta');
  finally
    vLista.Free;
  end;
end;

procedure Autoteste;
begin
  TesteDiferencas;
  TesteClasseSumida;
  TesteIgual;
  TesteJSON;
  TesteGlob;
  TesteCobertura;
  TesteSeparar;
  TesteRelatorio;
  TesteRegistros;
end;

{ Modo real }

procedure ModoReal;
var
  vCob: TCobertura;
  vRes: TResultadoPasso;
  vInt, vPend: integer;
  vArquivo: string;
begin
  WriteLn('modo real: ', GRaiz);
  vCob := TCobertura.Create;
  try
    vArquivo := GRaiz + PathDelim + 'regras' + PathDelim + 'cobertura.json';
    if not FileExists(vArquivo) then
    begin
      Conferir(False, 'existe ' + vArquivo);
      Exit;
    end;
    if not vCob.CarregarArquivo(vArquivo) then
    begin
      for vInt := 0 to Pred(vCob.Erros.Count) do
        WriteLn('  ', vCob.Erros[vInt]);
      Conferir(False, 'cobertura.json e valida');
      Exit;
    end;
    for vInt := 0 to Pred(vCob.PassoCount) do
    begin
      vRes := ProcessarPasso(vCob.Passos[vInt], GRaiz + PathDelim + 'api');
      try
        if vRes.Erro <> '' then
        begin
          Conferir(False, vCob.Passos[vInt].Rotulo + ': ' + vRes.Erro);
          Continue;
        end;
        WriteLn(Format('[%s] %s: %d diferencas, %d cobertas, %d pendentes',
                [vCob.Passos[vInt].Rotulo, BoolToStr(vCob.Passos[vInt].Fechado, 'fechado', 'aberto'),
                 vRes.Diferencas.Count, vRes.Cobertas.Count, vRes.Pendentes.Count]));
        for vPend := 0 to Pred(vRes.Pendentes.Count) do
          WriteLn('  pendente ', TDiferenca(vRes.Pendentes[vPend]).Chave, #9,
                  TDiferenca(vRes.Pendentes[vPend]).Detalhe);
        for vPend := 0 to Pred(vRes.Obsoletos.Count) do
          WriteLn('  aviso: cobertura sem correspondencia: ', vRes.Obsoletos[vPend]);
        if vCob.Passos[vInt].Fechado or GPreLancamento then
          Conferir(vRes.Pendentes.Count = 0, vCob.Passos[vInt].Rotulo + ': sem pendentes')
        else if vRes.Pendentes.Count > 0 then
          WriteLn('  aviso: passo aberto com pendentes');
      finally
        vRes.Free;
      end;
    end;
  finally
    vCob.Free;
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
    else if vArg = '--pre-lancamento' then
      GPreLancamento := True
    else if Copy(vArg, 1, 7) = '--raiz=' then
      GRaiz := ExcludeTrailingPathDelimiter(ExpandFileName(Copy(vArg, 8, MaxInt)));
  end;
  if GRaiz = '' then
    GRaiz := AcharPastaMigrador;
end;

begin
  LerArgumentos;
  if GSoTestes then
    Autoteste
  else
  begin
    if GRaiz = '' then
    begin
      WriteLn('Nao achei a pasta migrador (use --raiz=<pasta>).');
      Halt(2);
    end;
    ModoReal;
  end;
  WriteLn;
  if GFalhas = 0 then
    WriteLn('tudo certo')
  else
    WriteLn(GFalhas, ' falha(s)');
  if GFalhas > 0 then
    Halt(1);
end.
