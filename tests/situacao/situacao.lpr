program situacao;

{$mode ObjFPC}{$H+}

// O que uma rodada muda numa IDE que ja tem o RAL (RALInst.Situacao), sem IDE
// nenhuma: pastas, marcas e recibos montados numa pasta temporaria.
//
//   situacao --testes
//
// Confere a comparacao de fontes (a mesma versao e nada; outra e atualizar,
// voltar ou trocar; pasta local mudada e recompilar), de onde vem o RAL da IDE
// (o recibo que ainda vale, senao a pasta apontada) e o que o instalador ja
// compilou daqueles fontes (os recibos do mais novo ao mais velho, ate o
// primeiro de outros fontes).

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DateUtils,
  RALInst.Existente, RALInst.Fontes, RALInst.Recibos, RALInst.Situacao
  {$IFDEF MSWINDOWS}, Windows{$ENDIF};

var
  GFalhas, GTotal: integer;
  GBase: string;

procedure Conferir(ACondicao: boolean; const ADescricao: string);
begin
  Inc(GTotal);
  if ACondicao then
    WriteLn('ok    ', ADescricao)
  else
  begin
    Inc(GFalhas);
    WriteLn('FALHA ', ADescricao);
  end;
end;

procedure Gravar(const AArquivo, ATexto: string);
var
  vLista: TStringList;
begin
  ForceDirectories(ExtractFilePath(AArquivo));
  vLista := TStringList.Create;
  try
    vLista.Text := ATexto;
    vLista.SaveToFile(AArquivo);
  finally
    vLista.Free;
  end;
end;

// uma pasta de fontes do RAL com a marca do instalador (sem versao: pasta local)
function CriarFontes(const ANome, AVersao, ACommit: string): string;
begin
  Result := GBase + ANome + PathDelim;
  Gravar(Result + 'src' + PathDelim + 'base' + PathDelim + 'RALServer.pas', 'unit RALServer;');
  Gravar(Result + 'pkg' + PathDelim + 'Delphi' + PathDelim + 'PascalRAL.dpk', 'package PascalRAL;');
  if AVersao <> '' then
    Gravar(Result + ArquivoMarca, Format('{"repositorio":"x/PascalRAL","versao":"%s",' +
                                         '"commit":"%s"}', [AVersao, ACommit]));
end;

function Fontes(const APasta, AVersao, ACommit: string; AData: TDateTime;
  ADoInstalador: boolean = True): TFontesRAL;
begin
  Result := Default(TFontesRAL);
  Result.Existe := True;
  Result.Pasta := APasta;
  Result.Versao := AVersao;
  Result.Commit := ACommit;
  Result.Data := AData;
  Result.DoInstalador := ADoInstalador;
end;

function Barras(const ATexto: string): string;
begin
  Result := StringReplace(ATexto, '\', '\\', [rfReplaceAll]);
end;

// recibo de Delphi: fontes, commit, resultados por plataforma e arquivos
procedure GravarRecibo(const APastaRecibos, ANome, AData, AFontes, ACommit: string;
  const APacotes, AArquivos: array of string);
var
  vTexto, vLista: string;
  vInt: integer;
begin
  vLista := '';
  for vInt := Low(APacotes) to High(APacotes) do
  begin
    if vLista <> '' then
      vLista := vLista + ',';
    vLista := vLista + '"' + APacotes[vInt] + '"';
  end;
  vTexto := Format('{"instalador":"RALInstaller","data":"%s",' +
    '"ide":{"tipo":"delphi","nome":"Delphi Teste","raiz":"%s"},"fontes":"%s",' +
    '"ral":{"versao":"","commit":"%s"},"pacotes":[%s],"arquivos":[',
    [AData, Barras(GBase + 'ide' + PathDelim), Barras(AFontes), ACommit, vLista]);
  for vInt := Low(AArquivos) to High(AArquivos) do
  begin
    if vInt > Low(AArquivos) then
      vTexto := vTexto + ',';
    vTexto := vTexto + AArquivos[vInt];
  end;
  Gravar(IncludeTrailingPathDelimiter(APastaRecibos) + ANome, vTexto + '],"registro":[]}');
end;

function InfoArquivo(const AArquivo: string): string;
var
  vBusca: TSearchRec;
begin
  Result := '';
  if FindFirst(AArquivo, faAnyFile, vBusca) = 0 then
  begin
    Result := Format('{"arquivo":"%s","tamanho":%d,"data":"%s"}',
      [Barras(AArquivo), vBusca.Size,
       FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', vBusca.TimeStamp)]);
    SysUtils.FindClose(vBusca);
  end;
end;

procedure TestarComparacao;
var
  vA, vB, vC, vLocal: string;
  vAgora: TDateTime;
begin
  WriteLn('== comparação de fontes');
  vA := CriarFontes('ral-1.1', '1.1', 'aaaaaaa1111');
  vB := CriarFontes('ral-1.2', '1.2', 'bbbbbbb2222');
  vC := CriarFontes('ral-dev', 'dev', 'ccccccc3333');
  vLocal := CriarFontes('ral-local', '', '');
  vAgora := Now + 1;

  Conferir(MudancaDeFontes(Default(TFontesRAL), FontesDaPasta(vA, '', '')) = mrInstalar,
           'sem RAL: instalar');
  Conferir(MudancaDeFontes(Fontes('', '', '', 0, False), FontesDaPasta(vA, '', '')) =
           mrTrocar, 'RAL de pasta desconhecida: trocar');
  Conferir(MudancaDeFontes(Fontes(vA, '1.1', 'aaaaaaa1111', vAgora),
                           FontesDaPasta(vA, '', '')) = mrNada,
           'mesma pasta e commit (lido da marca): nada');
  Conferir(MudancaDeFontes(Fontes(vA, '1.1', 'aaaaaaa1111', vAgora),
                           FontesDaPasta(vA, '1.1', 'aaaaaaa1111')) = mrNada,
           'mesma pasta e commit (dados antes do download): nada');
  Conferir(MudancaDeFontes(Fontes(vC, 'dev', 'ccccccc0000', vAgora),
                           FontesDaPasta(vC, 'dev', 'ccccccc3333')) = mrAtualizar,
           'o mesmo ramo com outro commit: atualizar');
  Conferir(MudancaDeFontes(Fontes(vA, '1.1', 'aaaaaaa1111', 0, False),
                           FontesDaPasta(vA, '', '')) = mrTrocar,
           'mesma pasta instalada à mão: trocar (não se sabe o que foi compilado)');
  Conferir(MudancaDeFontes(Fontes(vA, '1.1', 'aaaaaaa1111', vAgora),
                           FontesDaPasta(vB, '', '')) = mrAtualizar, '1.1 -> 1.2: atualizar');
  Conferir(MudancaDeFontes(Fontes(vB, '1.2', 'bbbbbbb2222', vAgora),
                           FontesDaPasta(vA, '', '')) = mrVoltar, '1.2 -> 1.1: voltar');
  Conferir(MudancaDeFontes(Fontes(vA, 'v1.0', 'x', vAgora),
                           FontesDaPasta(vB, '', '')) = mrAtualizar, 'v1.0 -> 1.2: atualizar');
  Conferir(MudancaDeFontes(Fontes(vA, '1.1', 'aaaaaaa1111', vAgora),
                           FontesDaPasta(vC, '', '')) = mrTrocar, '1.1 -> dev: trocar');
  Conferir(MudancaDeFontes(Fontes(vC, 'dev', 'ccccccc0000', vAgora),
                           FontesDaPasta(GBase + 'outra-dev', 'dev', 'ccccccc3333')) =
           mrAtualizar, 'dev em outra pasta, outro commit: atualizar');
  Conferir(MudancaDeFontes(Fontes(vC, 'dev', 'ccccccc3333', vAgora),
                           FontesDaPasta(GBase + 'outra-dev', 'dev', 'ccccccc3333')) =
           mrTrocar, 'a mesma versão em outra pasta: trocar');
  Conferir(MudancaDeFontes(Fontes(vLocal, '', '', vAgora),
                           FontesDaPasta(vA, '', '')) = mrTrocar,
           'pasta local -> GitHub: trocar');

  // pasta local: vale a data do ultimo fonte contra a do recibo
  Conferir(MudancaDeFontes(Fontes(vLocal, '', '', vAgora),
                           FontesDaPasta(vLocal, '', '')) = mrNada,
           'pasta local sem fonte mudado depois da instalação: nada');
  Conferir(MudancaDeFontes(Fontes(vLocal, '', '', Now - 1),
                           FontesDaPasta(vLocal, '', '')) = mrRecompilar,
           'pasta local com fonte mudado depois da instalação: recompilar');

  Conferir(MudancaCompleta(mrAtualizar) and MudancaCompleta(mrInstalar) and
           not MudancaCompleta(mrNada) and not MudancaCompleta(mrModificar),
           'só nada e modificar não recompilam tudo');
  Conferir(Pos('1.1 (aaaaaaa)', DescreverMudanca(mrAtualizar,
             Fontes(vA, '1.1', 'aaaaaaa1111', 0), FontesDaPasta(vB, '', ''))) > 0,
           'a descrição diz a versão de antes: ' + DescreverMudanca(mrAtualizar,
             Fontes(vA, '1.1', 'aaaaaaa1111', 0), FontesDaPasta(vB, '', '')));
  Conferir(Pos('1.2 (bbbbbbb)', DescreverMudanca(mrAtualizar,
             Fontes(vA, '1.1', 'aaaaaaa1111', 0), FontesDaPasta(vB, '', ''))) > 0,
           'e a de depois');
end;

procedure TestarDataDosFontes;
var
  vPasta: string;
  vAntes: TDateTime;
begin
  WriteLn('== data dos fontes');
  vPasta := CriarFontes('ral-datas', '', '');
  vAntes := DataDosFontes(vPasta);
  Conferir(vAntes > 0, 'acha a data de um .pas');
  Sleep(1100);
  // o que o compilador e o editor gravam nao conta
  Gravar(vPasta + 'pkg' + PathDelim + 'Lazarus' + PathDelim + 'lib' + PathDelim + 'x.pas', 'x');
  Gravar(vPasta + 'src' + PathDelim + 'compiled' + PathDelim + 'y.pas', 'y');
  Gravar(vPasta + 'src' + PathDelim + 'backup' + PathDelim + 'z.pas', 'z');
  Gravar(vPasta + 'pkg' + PathDelim + 'Delphi' + PathDelim + 'PascalRAL.res', 'r');
  Conferir(DataDosFontes(vPasta) = vAntes, 'lib, compiled, backup e .res não contam');
  Gravar(vPasta + 'src' + PathDelim + 'base' + PathDelim + 'RALConsts.inc', 'i');
  Conferir(DataDosFontes(vPasta) > vAntes, 'um .inc novo conta');
end;

procedure TestarRecibos;
var
  vRecibos, vFontes, vBpl, vDcp, vInfoBpl, vInfoDcp: string;
  vRecibo: TRecibo;
  vResultados, vArquivos: TStringList;
  vNovas: TFontesRAL;
begin
  WriteLn('== recibos');
  vRecibos := GBase + 'recibos' + PathDelim;
  vFontes := CriarFontes('ral-rec', '1.1', 'aaaaaaa1111');
  vBpl := GBase + 'bpl' + PathDelim + 'IndyRAL.bpl';
  vDcp := GBase + 'dcp' + PathDelim + 'IndyRAL.dcp';
  Gravar(vBpl, 'bpl');
  Gravar(vDcp, 'dcp');
  vInfoBpl := InfoArquivo(vBpl);
  vInfoDcp := InfoArquivo(vDcp);

  Conferir(DataDoRecibo('2026-09-25T20:48:53') =
           EncodeDateTime(2026, 9, 25, 20, 48, 53, 0), 'data do recibo');
  Conferir(DataDoRecibo('lixo') = 0, 'data que não é de recibo');

  // o mais novo (commit do 1.1): IndyRAL; o do meio (mesmo commit): PascalRAL e
  // IndyRAL falhou antes; o mais velho, de outro commit, nao conta
  GravarRecibo(vRecibos, 'delphi-t-3.json', '2026-09-27T10:00:00', vFontes, 'aaaaaaa1111',
               ['win32 IndyRAL: ok', 'win64 IndyRAL: pulado — o .dproj'],
               [vInfoBpl, vInfoDcp]);
  GravarRecibo(vRecibos, 'delphi-t-2.json', '2026-09-26T10:00:00', vFontes, 'aaaaaaa1111',
               ['win32 PascalRAL: ok', 'win32 IndyRAL: falhou — E2003', 'X: pulado — y'], []);
  GravarRecibo(vRecibos, 'delphi-t-1.json', '2026-09-25T10:00:00', vFontes, '0000000',
               ['win32 SaguiRAL: ok'], []);

  vRecibo := TRecibo.Create;
  try
    Conferir(vRecibo.Carregar(vRecibos + 'delphi-t-2.json'), 'lê o recibo');
    Conferir(vRecibo.CommitRAL = 'aaaaaaa1111', 'commit do recibo');
    Conferir(vRecibo.Resultados.Values['win32 IndyRAL'] = 'falhou', 'resultado que falhou');
    Conferir(vRecibo.Resultados.IndexOfName('X') < 0,
             'pulado sem plataforma (compatibilidade) não é resultado de compilação');
    Conferir(vRecibo.Pacotes.CommaText = 'PascalRAL', 'pacotes: só os que deram ok');
  finally
    vRecibo.Free;
  end;

  vResultados := TStringList.Create;
  vArquivos := TStringList.Create;
  try
    vNovas := FontesDaPasta(vFontes, '', '');
    CompiladoDasFontes(vRecibos, GBase + 'ide', vNovas, vResultados, vArquivos);
    Conferir(vResultados.Values['win32 IndyRAL'] = 'ok',
             'o mais novo vale (IndyRAL ok, e não o falhou de antes)');
    Conferir(vResultados.Values['win32 PascalRAL'] = 'ok',
             'o recibo anterior dos mesmos fontes soma');
    Conferir(vResultados.Values['win64 IndyRAL'] = 'pulado', 'pulado por plataforma');
    Conferir(vResultados.IndexOfName('win32 SaguiRAL') < 0,
             'o recibo de outro commit para a busca');
    Conferir(IgualAoCompilado(vBpl, vArquivos), 'o .bpl ainda é o compilado');
    Sleep(1100);
    Gravar(vBpl, 'bpl recompilado pelo usuário');
    Conferir(not IgualAoCompilado(vBpl, vArquivos), '.bpl mudado não é o compilado');
    Conferir(not IgualAoCompilado(GBase + 'nao-existe.bpl', vArquivos),
             'arquivo fora do recibo');

    // outros fontes: nada do que foi compilado vale
    CompiladoDasFontes(vRecibos, GBase + 'ide', FontesDaPasta(vFontes, '1.1', 'outro'),
                       vResultados, vArquivos);
    Conferir(vResultados.Count = 0, 'outro commit: nada compilado dele');
  finally
    vArquivos.Free;
    vResultados.Free;
  end;
end;

procedure TestarFontesDaIDE;
var
  vRecibos, vFontes, vOutra: string;
  vExistente: TInstalacaoExistente;
  vAtual: TFontesRAL;
begin
  WriteLn('== fontes da IDE');
  vRecibos := GBase + 'recibos-ide' + PathDelim;
  vFontes := CriarFontes('ral-ide', '1.1', 'aaaaaaa1111');
  vOutra := CriarFontes('ral-ide-outra', '1.2', 'bbbbbbb2222');
  GravarRecibo(vRecibos, 'delphi-i-1.json', '2026-09-27T10:00:00', vFontes, 'fffffff',
               ['win32 PascalRAL: ok'], []);

  vExistente := TInstalacaoExistente.Create;
  try
    Conferir(not FontesDaIDE(GBase + 'ide', vExistente, vRecibos).Existe,
             'IDE sem RAL: não existe (o recibo sozinho não basta)');
    vExistente.Pacotes.Add('PascalRALDsgn=x.bpl');
    vExistente.Fontes := vFontes;
    vAtual := FontesDaIDE(GBase + 'ide', vExistente, vRecibos);
    Conferir(vAtual.DoInstalador and (vAtual.Commit = 'fffffff'),
             'o recibo que bate com a pasta apontada diz o commit instalado');
    Conferir(vAtual.Data = EncodeDateTime(2026, 9, 27, 10, 0, 0, 0), 'e a data');

    // a IDE passou a apontar outra pasta (a mao): o recibo nao vale mais
    vExistente.Fontes := vOutra;
    vAtual := FontesDaIDE(GBase + 'ide', vExistente, vRecibos);
    Conferir(not vAtual.DoInstalador and (vAtual.Versao = '1.2') and
             (vAtual.Commit = 'bbbbbbb2222'),
             'pasta que não é a do recibo: a marca dela, como instalação à mão');
  finally
    vExistente.Free;
  end;
end;

begin
  {$IFDEF MSWINDOWS}SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  if ParamStr(1) <> '--testes' then
  begin
    WriteLn('uso: situacao --testes');
    Halt(2);
  end;
  GBase := IncludeTrailingPathDelimiter(GetTempDir) + 'ralinstaller-situacao-' +
           FormatDateTime('hhnnsszzz', Now) + PathDelim;
  try
    TestarComparacao;
    TestarDataDosFontes;
    TestarRecibos;
    TestarFontesDaIDE;
  finally
    ApagarPasta(GBase);
  end;
  WriteLn;
  WriteLn(Format('%d de %d', [GTotal - GFalhas, GTotal]));
  Halt(Ord(GFalhas > 0));
end.
