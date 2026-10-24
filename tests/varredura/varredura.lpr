program varredura;

{$mode ObjFPC}{$H+}

// F16: a verificacao por amostragem (decisao D-I5). O projeto testa por
// amostra, nao todas as combinacoes: cada versao do RAL e compilada na IDE mais
// velha que se tem (XE2), na mais nova (Delphi 13) e numa sorteada no meio —
// entre as que ainda nao foram verificadas para aquela versao, para que as
// rodadas seguidas cubram o meio aos poucos —, e no Lazarus no FPC 3.2.x mais
// velho, no trunk mais novo, num sorteado no meio e no FPC Unleashed (que e
// outro compilador, nao uma versao). Compila numa pasta temporaria, sem
// registro e sem configuracao de verdade (uma copia), como os testes da F3.
//
//   varredura [--ral=<ref|pasta>]... [--delphi=amostra|todas|<bds,bds>]
//             [--lazarus=amostra|todas|<raiz,raiz>] [--buscar-em=<pasta>]...
//             [--verificadas=<arquivo>] [--saida=<pasta>] [--pasta=<pasta curta>]
//             [--receitas=<pasta>] [--todas] [--semente=<n>]
//
//   --ral          versao do GitHub (1.1, v1.0, dev) ou pasta local com os
//                  fontes; repetivel (padrao: a estavel mais recente)
//   --delphi       amostra (padrao), todas, ou os BDS da lista (37.0,9.0); nenhum
//                  com --delphi=
//   --lazarus      amostra (padrao), todas, ou as raizes (a pasta lazarus de
//                  cada instalacao); nenhum com --lazarus=
//   --todas        a matriz inteira (o modo do ambiente de testes com todas as
//                  versoes do Delphi)
//   --verificadas  a lista que acumula (padrao: manifesto/verificadas.json do
//                  repositorio)
//   --saida        onde fica o dossie (padrao: a pasta temporaria)
//   --pasta        onde a versao do GitHub e baixada (curta: o FPC nao aceita
//                  caminho longo)
//   --semente      repete o sorteio de uma rodada
//
// O que passa entra na lista de verificadas ("ok"); o que falha entra como
// "falhou", com o primeiro erro, e o manifesto o le como bloqueio daquela
// versao naquela IDE. O dossie, no formato do orquestrador, diz o que rodou,
// a linha de comando, a saida crua, as versoes e o que ficou de fora e por que.
//
// Codigo de saida: 0 todas as escolhidas verificadas; 1 alguma falhou; 2 uso.

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  {$IFDEF MSWINDOWS} Windows, RALInst.IDE.Delphi, RALInst.Build.Delphi,
  RALInst.Instalar.Delphi, {$ENDIF}
  Classes, SysUtils, StrUtils, Math, zipper,
  RALInst.Catalogo, RALInst.Compatibilidade, RALInst.Config.Lazarus, RALInst.Fontes,
  RALInst.GitHub, RALInst.IDE, RALInst.IDE.Lazarus, RALInst.Motores, RALInst.Processo,
  RALInst.Promocao, RALInst.Receitas, RALInst.Verificadas, RALInst.Zip;

type
  /// Writes to the console and keeps the lines of the current IDE.
  TSaida = class
  public
    Atual: TStringList;
    Comandos: TStringList;
    procedure Linha(const ALinha: string);
  end;

procedure TSaida.Linha(const ALinha: string);
begin
  WriteLn(ALinha);
  if Atual <> nil then
    Atual.Add(ALinha);
  // a linha de comando de cada compilacao vai para o dossie
  if (Comandos <> nil) and ((Pos('dcc32.exe', ALinha) > 0) or
     (Pos('dcc64.exe', ALinha) > 0) or (Pos('lazbuild', ALinha) > 0)) and
     (Pos('> ', ALinha) > 0) then
    Comandos.Add(Trim(ALinha));
end;

var
  GSaida: TSaida;
  GFalhas: integer = 0;

function Opcao(const ANome, APadrao: string): string;
var
  vInt: integer;
begin
  Result := APadrao;
  for vInt := 1 to ParamCount do
    if SameText(Copy(ParamStr(vInt), 1, Length(ANome) + 3), '--' + ANome + '=') then
      Result := Copy(ParamStr(vInt), Length(ANome) + 4, MaxInt);
end;

function TemOpcao(const ANome: string): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := 1 to ParamCount do
    if SameText(Copy(ParamStr(vInt), 1, Length(ANome) + 2), '--' + ANome) then
      Exit(True);
end;

procedure Opcoes(const ANome: string; ALista: TStrings);
var
  vInt: integer;
begin
  for vInt := 1 to ParamCount do
    if SameText(Copy(ParamStr(vInt), 1, Length(ANome) + 3), '--' + ANome + '=') then
      ALista.Add(Copy(ParamStr(vInt), Length(ANome) + 4, MaxInt));
end;

function CompararVersaoIDE(AItem1, AItem2: Pointer): integer;
begin
  Result := CompararVersoes(TIDEInstance(AItem1).Versao, TIDEInstance(AItem2).Versao);
  if Result = 0 then
    Result := CompararVersoes(TIDEInstance(AItem1).VersaoCompilador,
                              TIDEInstance(AItem2).VersaoCompilador);
end;

// a sorteada no meio: entre as que ainda nao foram verificadas para a versao
function SortearMeio(AMeio: TList; const AVersaoRAL: string): TIDEInstance;
var
  vNaoVistas: TList;
  vInt: integer;
begin
  Result := nil;
  if AMeio.Count = 0 then
    Exit;
  vNaoVistas := TList.Create;
  try
    for vInt := 0 to Pred(AMeio.Count) do
      if VerificadasPadrao.Buscar(AVersaoRAL, TIDEInstance(AMeio[vInt])) = nil then
        vNaoVistas.Add(AMeio[vInt]);
    if vNaoVistas.Count > 0 then
      Result := TIDEInstance(vNaoVistas[Random(vNaoVistas.Count)])
    else
      Result := TIDEInstance(AMeio[Random(AMeio.Count)]);
  finally
    vNaoVistas.Free;
  end;
end;

// as Delphi da rodada: amostra (a mais velha, a mais nova, uma no meio), todas
// ou as pedidas pelo BDS
procedure EscolherDelphi(AIDEs: TIDEList; const AModo, AVersaoRAL: string;
  AResultado: TList);
var
  vCand, vMeio: TList;
  vPedidos: TStringList;
  vInt: integer;
  vIDE, vSorteada: TIDEInstance;
  vBDS: TStringList;
begin
  AResultado.Clear;
  if AModo = '' then
    Exit;
  vCand := TList.Create;
  vMeio := TList.Create;
  vPedidos := TStringList.Create;
  vBDS := TStringList.Create;
  try
    // uma pasta por BDS (duas copias do XE2 dividem a mesma chave): fica a
    // registrada, que vem primeiro
    for vInt := 0 to Pred(AIDEs.Count) do
    begin
      vIDE := AIDEs[vInt];
      if (vIDE.Tipo <> tiDelphi) or not (ciCompilar in vIDE.Capacidades) or
         (vIDE.BDSVersao = '') or (MotivoMinimoIDE(vIDE) <> '') or
         (vBDS.IndexOf(vIDE.BDSVersao) >= 0) then
        Continue;
      vBDS.Add(vIDE.BDSVersao);
      vCand.Add(vIDE);
    end;
    vCand.Sort(@CompararVersaoIDE);
    if SameText(AModo, 'todas') then
    begin
      AResultado.Assign(vCand);
      Exit;
    end;
    if not SameText(AModo, 'amostra') then
    begin
      vPedidos.CommaText := AModo;
      for vInt := 0 to Pred(vCand.Count) do
        if vPedidos.IndexOf(TIDEInstance(vCand[vInt]).BDSVersao) >= 0 then
          AResultado.Add(vCand[vInt]);
      Exit;
    end;
    if vCand.Count = 0 then
      Exit;
    AResultado.Add(vCand[0]);
    if vCand.Count > 1 then
      AResultado.Add(vCand[Pred(vCand.Count)]);
    for vInt := 1 to vCand.Count - 2 do
      vMeio.Add(vCand[vInt]);
    vSorteada := SortearMeio(vMeio, AVersaoRAL);
    if vSorteada <> nil then
      AResultado.Insert(1, vSorteada);
  finally
    vBDS.Free;
    vPedidos.Free;
    vMeio.Free;
    vCand.Free;
  end;
end;

// os Lazarus da rodada: o FPC 3.2.x mais velho, o trunk mais novo, um sorteado
// no meio e o Unleashed sempre
procedure EscolherLazarus(AIDEs: TIDEList; const AModo, AVersaoRAL: string;
  AResultado: TList);
var
  vOficiais, vMeio, vUnleashed: TList;
  vPedidos: TStringList;
  vInt: integer;
  vIDE, vVelho, vTrunk, vSorteado: TIDEInstance;
begin
  AResultado.Clear;
  if AModo = '' then
    Exit;
  vOficiais := TList.Create;
  vMeio := TList.Create;
  vUnleashed := TList.Create;
  vPedidos := TStringList.Create;
  try
    for vInt := 0 to Pred(AIDEs.Count) do
    begin
      vIDE := AIDEs[vInt];
      if (vIDE.Tipo <> tiLazarus) or not (ciCompilar in vIDE.Capacidades) or
         (vIDE.ConfigDir = '') or (vIDE.VersaoCompilador = '') or
         (MotivoMinimoIDE(vIDE) <> '') then
        Continue;
      if SameText(vIDE.VarianteFPC, 'unleashed') then
        vUnleashed.Add(vIDE)
      else
        vOficiais.Add(vIDE);
    end;
    vOficiais.Sort(@CompararVersaoIDE);
    if SameText(AModo, 'todas') then
    begin
      AResultado.Assign(vOficiais);
      for vInt := 0 to Pred(vUnleashed.Count) do
        AResultado.Add(vUnleashed[vInt]);
      Exit;
    end;
    if not SameText(AModo, 'amostra') then
    begin
      vPedidos.Delimiter := ',';
      vPedidos.StrictDelimiter := True;
      vPedidos.DelimitedText := AModo;
      for vInt := 0 to Pred(AIDEs.Count) do
        if (AIDEs[vInt].Tipo = tiLazarus) and
           ((vPedidos.IndexOf(ExcludeTrailingPathDelimiter(AIDEs[vInt].RootDir)) >= 0) or
            (vPedidos.IndexOf(AIDEs[vInt].RootDir) >= 0)) then
          AResultado.Add(AIDEs[vInt]);
      Exit;
    end;
    vVelho := nil;
    vTrunk := nil;
    for vInt := 0 to Pred(vOficiais.Count) do
    begin
      vIDE := TIDEInstance(vOficiais[vInt]);
      if (CompararVersoes(vIDE.VersaoCompilador, '3.2.x') <= 0) and
         ((vVelho = nil) or
          (CompararVersoes(vIDE.VersaoCompilador, vVelho.VersaoCompilador) < 0)) then
        vVelho := vIDE;
      if (vTrunk = nil) or
         (CompararVersoes(vIDE.VersaoCompilador, vTrunk.VersaoCompilador) > 0) or
         ((CompararVersoes(vIDE.VersaoCompilador, vTrunk.VersaoCompilador) = 0) and
          (CompararVersoes(vIDE.Versao, vTrunk.Versao) > 0)) then
        vTrunk := vIDE;
    end;
    if vVelho <> nil then
      AResultado.Add(vVelho);
    for vInt := 0 to Pred(vOficiais.Count) do
      if (vOficiais[vInt] <> pointer(vVelho)) and (vOficiais[vInt] <> pointer(vTrunk)) then
        vMeio.Add(vOficiais[vInt]);
    vSorteado := SortearMeio(vMeio, AVersaoRAL);
    if vSorteado <> nil then
      AResultado.Add(vSorteado);
    if (vTrunk <> nil) and (vTrunk <> vVelho) then
      AResultado.Add(vTrunk);
    for vInt := 0 to Pred(vUnleashed.Count) do
      AResultado.Add(vUnleashed[vInt]);
  finally
    vPedidos.Free;
    vUnleashed.Free;
    vMeio.Free;
    vOficiais.Free;
  end;
end;

function NomeArquivo(const ATexto: string): string;
var
  vInt: integer;
begin
  Result := ATexto;
  for vInt := 1 to Length(Result) do
    if not (Result[vInt] in ['A'..'Z', 'a'..'z', '0'..'9', '.', '-', '_']) then
      Result[vInt] := '_';
end;

{$IFDEF MSWINDOWS}
// compila no Delphi numa pasta temporaria, sem registro: o conjunto
// funcional (nucleo, design e um motor) decide se a versao foi verificada
function VerificarDelphi(AIDE: TIDEInstance; ACatalogo: TCatalogo;
  ACompat: TCompatibilidade; const APasta: string; out AMotivo: string;
  ATabela: TStrings): boolean;
var
  vTodos, vLista: TList;
  vFora, vOk, vMotivos: TStringList;
  vBuild: TBuildDelphi;
  vInt: integer;
  vRes: TResultadoPacote;
begin
  Result := False;
  AMotivo := '';
  vTodos := TList.Create;
  vLista := TList.Create;
  vFora := TStringList.Create;
  vOk := TStringList.Create;
  vOk.CaseSensitive := False;
  vMotivos := TStringList.Create;
  vMotivos.CaseSensitive := False;
  vBuild := TBuildDelphi.Create(AIDE, ACatalogo);
  try
    ACatalogo.Listar(tpDelphi, vTodos);
    vLista.Assign(vTodos);
    ACompat.Filtrar(vLista, AIDE, vFora);
    vMotivos.AddStrings(vFora);
    for vInt := 0 to Pred(vFora.Count) do
      ATabela.Add(Format('    %-24s fora   %s', [vFora.Names[vInt], vFora.ValueFromIndex[vInt]]));
    vBuild.Log := @GSaida.Linha;
    vBuild.PastaBpl := APasta + 'bpl';
    vBuild.PastaDcp := APasta + 'dcp';
    vBuild.PastaDcu := APasta + 'dcu';
    vBuild.Compilar(vLista);
    for vInt := 0 to Pred(vBuild.Total) do
    begin
      vRes := vBuild.Resultados[vInt];
      if vRes.Ok then
        vOk.Add(vRes.Nome)
      else if vRes.Motivo <> '' then
        vMotivos.Values[vRes.Nome] := vRes.Motivo;
      ATabela.Add(Format('    %-24s %-6s %s', [vRes.Nome,
        IfThen(vRes.Ok, 'ok', IfThen(vRes.Pulado, 'pulado', 'FALHOU')), vRes.Motivo]));
    end;
    AMotivo := FaltaConjunto(vTodos, vOk, vMotivos, True);
    Result := AMotivo = '';
  finally
    vBuild.Free;
    vMotivos.Free;
    vOk.Free;
    vFora.Free;
    vLista.Free;
    vTodos.Free;
  end;
end;
{$ENDIF}

// compila no Lazarus contra uma COPIA da configuracao, pacote a pacote, como
// a F14 faz antes de tocar na verdadeira
function VerificarLazarus(AIDE: TIDEInstance; ACatalogo: TCatalogo;
  ACompat: TCompatibilidade; const ARaiz, APasta: string; out AMotivo: string;
  ATabela: TStrings): boolean;
var
  vTodos, vLista: TList;
  vFora, vOk, vMotivos, vFalhou, vParams: TStringList;
  vExec: TExecucao;
  vBusca: TSearchRec;
  vCopia, vMotivo, vLinha: string;
  vInt, vDep: integer;
  vPacote: TPacote;

  procedure Base;
  begin
    vParams.Clear;
    vParams.Add('--primary-config-path=' + ExcludeTrailingPathDelimiter(vCopia));
    vParams.Add('--lazarusdir=' + ExcludeTrailingPathDelimiter(AIDE.RootDir));
  end;

  function Primeiro(ASaida: TStrings): string;
  var
    vI, vPos: integer;
  begin
    Result := '';
    for vI := 0 to Pred(ASaida.Count) do
    begin
      vPos := Pos(' Error: ', ASaida[vI]);
      if vPos = 0 then
        vPos := Pos(' Fatal: ', ASaida[vI]);
      if vPos > 0 then
        Exit(ExtractFileName(Trim(Copy(ASaida[vI], 1, vPos - 1))) + ': ' +
             Trim(Copy(ASaida[vI], vPos + 8, MaxInt)));
    end;
  end;

begin
  Result := False;
  AMotivo := '';
  vCopia := APasta + 'config' + PathDelim;
  vTodos := TList.Create;
  vLista := TList.Create;
  vFora := TStringList.Create;
  vOk := TStringList.Create;
  vOk.CaseSensitive := False;
  vMotivos := TStringList.Create;
  vMotivos.CaseSensitive := False;
  vFalhou := TStringList.Create;
  vFalhou.CaseSensitive := False;
  vParams := TStringList.Create;
  vExec := TExecucao.Create;
  try
    ForceDirectories(vCopia);
    if FindFirst(AIDE.ConfigDir + AllFilesMask, faAnyFile, vBusca) = 0 then
    try
      repeat
        if (vBusca.Attr and faDirectory) = 0 then
          CopiarArquivo(AIDE.ConfigDir + vBusca.Name, vCopia + vBusca.Name);
      until FindNext(vBusca) <> 0;
    finally
      SysUtils.FindClose(vBusca);
    end;
    ACatalogo.Listar(tpLazarus, vTodos);
    vLista.Assign(vTodos);
    ACompat.Filtrar(vLista, AIDE, vFora);
    vMotivos.AddStrings(vFora);
    for vInt := 0 to Pred(vFora.Count) do
      ATabela.Add(Format('    %-24s fora   %s', [vFora.Names[vInt], vFora.ValueFromIndex[vInt]]));

    vExec.Executavel := AIDE.BuildFile;
    vExec.Log := @GSaida.Linha;
    // todos conhecidos pela copia, depois um por um
    Base;
    vParams.Add('--add-package-link');
    for vInt := 0 to Pred(vLista.Count) do
      vParams.Add(ARaiz + StringReplace(TPacote(vLista[vInt]).ArquivoRelativo, '/',
                                        PathDelim, [rfReplaceAll]));
    vExec.Parametros.Assign(vParams);
    vExec.Executar;
    for vInt := 0 to Pred(vLista.Count) do
    begin
      vPacote := TPacote(vLista[vInt]);
      vMotivo := '';
      for vDep := 0 to Pred(vPacote.Internos.Count) do
        if vFalhou.IndexOf(vPacote.Internos[vDep]) >= 0 then
          vMotivo := 'depende de ' + vPacote.Internos[vDep];
      if vMotivo = '' then
      begin
        Base;
        vParams.Add(ARaiz + StringReplace(vPacote.ArquivoRelativo, '/', PathDelim,
                                          [rfReplaceAll]));
        vExec.Parametros.Assign(vParams);
        if not vExec.Executar then
        begin
          vMotivo := Primeiro(vExec.Saida);
          if vMotivo = '' then
            vMotivo := 'lazbuild terminou com código ' + IntToStr(vExec.CodigoSaida);
        end;
      end;
      if vMotivo = '' then
        vOk.Add(vPacote.Nome)
      else
      begin
        vFalhou.Add(vPacote.Nome);
        vMotivos.Values[vPacote.Nome] := vMotivo;
      end;
      vLinha := Format('    %-24s %-6s %s', [vPacote.Nome,
                       IfThen(vMotivo = '', 'ok', 'FALHOU'), vMotivo]);
      ATabela.Add(vLinha);
    end;
    AMotivo := FaltaConjunto(vTodos, vOk, vMotivos, True);
    Result := AMotivo = '';
  finally
    vExec.Free;
    vParams.Free;
    vFalhou.Free;
    vMotivos.Free;
    vOk.Free;
    vFora.Free;
    vLista.Free;
    vTodos.Free;
    ApagarPasta(vCopia);
  end;
end;

// a versao do RAL na pasta (local, ou baixada do GitHub); '' quando falhou
function PrepararVersao(const ASpec, ABase: string; out ACommit: string): string;
var
  vRepo: TRepoGitHub;
  vVersoes: TVersoesRAL;
  vVersao: TVersaoRAL;
  vPreparo: TPreparoFontes;
  vCatalogo: TCatalogo;
  vLista: TList;
  vZip: string;
  vInt, vSub: integer;
  vTipo: TTipoPacote;
begin
  Result := '';
  ACommit := '';
  if DirectoryExists(ASpec) then
    Exit(IncludeTrailingPathDelimiter(ExpandFileName(ASpec)));
  vRepo := TRepoGitHub.Create(DonoRAL, RepoRAL);
  vVersoes := TVersoesRAL.Create(True);
  vCatalogo := TCatalogo.Create;
  vLista := TList.Create;
  try
    if not vRepo.ListarVersoes(vVersoes) then
    begin
      WriteLn('ERRO: ', vRepo.Erro);
      Exit;
    end;
    if (ASpec = '') or SameText(ASpec, 'estavel') then
      vVersao := vVersoes.Recomendada
    else
      vVersao := vVersoes.Buscar(ASpec);
    if vVersao = nil then
    begin
      WriteLn('ERRO: versão desconhecida: ', ASpec);
      Exit;
    end;
    if not vRepo.BaixarZip(vVersao.Ref, vVersao.Tipo = tvRamo, vZip) then
    begin
      WriteLn('ERRO: ', vRepo.Erro);
      Exit;
    end;
    vCatalogo.Carregar(TOrigemZip.Create(vZip, 'PascalRAL ' + vVersao.Ref));
    vPreparo := TPreparoFontes.Create(vRepo, vVersao.Ref, vVersao.Tipo = tvRamo);
    try
      vPreparo.PastaBase := ABase;
      vPreparo.Log := @GSaida.Linha;
      // todos os submodulos: a varredura compila tudo
      for vTipo := Low(TTipoPacote) to High(TTipoPacote) do
      begin
        vCatalogo.Listar(vTipo, vLista);
        for vInt := 0 to Pred(vLista.Count) do
          for vSub := 0 to Pred(TPacote(vLista[vInt]).Submodulos.Count) do
            vPreparo.Submodulos.Add(TPacote(vLista[vInt]).Submodulos[vSub]);
      end;
      if not vPreparo.Executar then
      begin
        WriteLn('ERRO: ', vPreparo.Erro);
        Exit;
      end;
      ACommit := vPreparo.Commit;
      Result := vPreparo.PastaDestino;
    finally
      vPreparo.Free;
    end;
  finally
    vLista.Free;
    vCatalogo.Free;
    vVersoes.Free;
    vRepo.Free;
  end;
end;

procedure ZiparPasta(const APasta, AZip: string);
var
  vZip: TZipper;
  vArquivos: TStringList;

  procedure Listar(const ASub: string);
  var
    vBusca: TSearchRec;
  begin
    if FindFirst(APasta + ASub + AllFilesMask, faAnyFile, vBusca) = 0 then
    try
      repeat
        if (vBusca.Name = '.') or (vBusca.Name = '..') then
          Continue;
        if (vBusca.Attr and faDirectory) <> 0 then
          Listar(ASub + vBusca.Name + PathDelim)
        else
          vArquivos.Add(ASub + vBusca.Name);
      until FindNext(vBusca) <> 0;
    finally
      SysUtils.FindClose(vBusca);
    end;
  end;

var
  vArq: string;
begin
  vZip := TZipper.Create;
  vArquivos := TStringList.Create;
  try
    Listar('');
    vZip.FileName := AZip;
    for vArq in vArquivos do
      vZip.Entries.AddFileEntry(APasta + vArq, StringReplace(vArq, PathDelim, '/',
                                                              [rfReplaceAll]));
    vZip.ZipAllFiles;
  finally
    vArquivos.Free;
    vZip.Free;
  end;
end;

var
  GIDEs: TIDEList;
  GVersoes, GExtras, GRelatorio, GAmbiente, GNaoRodou, GTabela: TStringList;
  GEscolhidas, GLazarus: TList;
  GCatalogo: TCatalogo;
  GManifesto: TManifesto;
  GReceitas: TReceitas;
  GCompat: TCompatibilidade;
  GSpec, GRaiz, GCommit, GVersaoRAL, GDossie, GPastaIDE, GMotivo, GArqVerificadas,
    GBase, GReceitasPasta, GPasta, GNomeLog: string;
  GInt, GSemente, GVerificadas: integer;
  GIDE: TIDEInstance;
  GOk: boolean;
  {$IFDEF MSWINDOWS}
  GBuscaD: TBuscaDelphi;
  {$ENDIF}
  GBuscaL: TBuscaLazarus;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  DefaultSystemCodePage := CP_UTF8;
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  if TemOpcao('ajuda') then
  begin
    WriteLn('uso: varredura [--ral=<ref|pasta>]... [--delphi=amostra|todas|<bds,...>]');
    WriteLn('               [--lazarus=amostra|todas|<raiz,...>] [--buscar-em=<pasta>]...');
    WriteLn('               [--verificadas=<arquivo>] [--saida=<pasta>] [--pasta=<pasta>]');
    WriteLn('               [--todas] [--semente=<n>]');
    Halt(2);
  end;
  GSemente := StrToIntDef(Opcao('semente', ''), -1);
  if GSemente < 0 then
  begin
    Randomize;
    GSemente := Random(1000000);
  end;
  RandSeed := GSemente;
  GVerificadas := 0;

  GSaida := TSaida.Create;
  GIDEs := TIDEList.Create(True);
  GVersoes := TStringList.Create;
  GExtras := TStringList.Create;
  GRelatorio := TStringList.Create;
  GAmbiente := TStringList.Create;
  GNaoRodou := TStringList.Create;
  GTabela := TStringList.Create;
  GSaida.Comandos := TStringList.Create;
  GEscolhidas := TList.Create;
  GLazarus := TList.Create;
  GReceitas := TReceitas.Create;
  try
    GArqVerificadas := Opcao('verificadas', ExpandFileName(ExtractFilePath(ParamStr(0)) +
                             '..' + PathDelim + '..' + PathDelim + 'manifesto' + PathDelim +
                             'verificadas.json'));
    // a lista que acumula: a do repositorio, nao a embutida
    VerificadasPadrao.Clear;
    VerificadasPadrao.CarregarArquivo(GArqVerificadas);
    GReceitasPasta := Opcao('receitas', ExpandFileName(ExtractFilePath(ParamStr(0)) +
                            '..' + PathDelim + '..' + PathDelim + 'receitas'));
    GReceitas.CarregarPasta(GReceitasPasta);
    GBase := Opcao('pasta', GetTempDir + 'ralvar');
    GDossie := IncludeTrailingPathDelimiter(Opcao('saida', GetTempDir)) + 'varredura-' +
               FormatDateTime('yyyymmdd-hhnnss', Now) + PathDelim;
    ForceDirectories(GDossie + 'saida');

    // as IDEs da maquina
    Opcoes('buscar-em', GExtras);
    {$IFDEF MSWINDOWS}
    GBuscaD := TBuscaDelphi.Create;
    try
      GBuscaD.BuscarPadrao(GIDEs);
      for GPasta in GExtras do
        GBuscaD.BuscarEm(GIDEs, GPasta, 3);
      GBuscaD.Finalizar(GIDEs);
    finally
      GBuscaD.Free;
    end;
    {$ENDIF}
    GBuscaL := TBuscaLazarus.Create;
    try
      GBuscaL.BuscarPadrao(GIDEs);
      for GPasta in GExtras do
        GBuscaL.BuscarEm(GIDEs, GPasta, 3);
      GBuscaL.Finalizar(GIDEs);
    finally
      GBuscaL.Free;
    end;

    Opcoes('ral', GVersoes);
    if GVersoes.Count = 0 then
      GVersoes.Add('estavel');

    GAmbiente.Add('AMBIENTE');
    GAmbiente.Add('gerado em      : ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    GAmbiente.Add('maquina        : ' + GetEnvironmentVariable('COMPUTERNAME'));
    GAmbiente.Add('semente        : ' + IntToStr(GSemente) +
                  ' (--semente= repete o sorteio)');
    GAmbiente.Add('verificadas    : ' + GArqVerificadas);
    GAmbiente.Add('versoes do RAL : ' + GVersoes.CommaText);
    GAmbiente.Add('');
    GAmbiente.Add('IDEs da maquina');
    for GInt := 0 to Pred(GIDEs.Count) do
      GAmbiente.Add(Format('  %-42s %-8s %s %s', [GIDEs[GInt].Nome,
        GIDEs[GInt].VersaoCompilador, GIDEs[GInt].RootDir,
        IfThen(GIDEs[GInt].VarianteFPC <> '', '(' + GIDEs[GInt].VarianteFPC + ')', '')]));

    for GSpec in GVersoes do
    begin
      WriteLn('==== RAL ', GSpec);
      GRaiz := PrepararVersao(GSpec, GBase, GCommit);
      if GRaiz = '' then
      begin
        GNaoRodou.Add('RAL ' + GSpec + ': não foi possível obter os fontes');
        Inc(GFalhas);
        Continue;
      end;
      GCatalogo := TCatalogo.Create;
      GManifesto := TManifesto.Create;
      try
        GCatalogo.Carregar(GRaiz);
        GManifesto.CarregarPadrao(GCatalogo.Origem);
        // fora do executavel do instalador nao ha o embutido: o do repositorio
        if (GManifesto.Origem = '') or (GManifesto.TotalRegras = 0) then
        begin
          GVersaoRAL := GManifesto.VersaoRAL;
          GManifesto.CarregarArquivo(ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' +
            PathDelim + '..' + PathDelim + 'manifesto' + PathDelim + 'ral.json'));
          GManifesto.VersaoRAL := GVersaoRAL;
          GManifesto.AcrescentarFalhas(VerificadasPadrao);
        end;
        GVersaoRAL := GManifesto.VersaoRAL;
        if GVersaoRAL = '' then
        begin
          GNaoRodou.Add('RAL ' + GSpec + ': a árvore não declara RALVERSION');
          Continue;
        end;
        GRelatorio.Add('');
        GRelatorio.Add(Format('RAL %s (RALVERSION %s%s) — %s', [GSpec, GVersaoRAL,
          IfThen(GCommit <> '', ', ' + Copy(GCommit, 1, 7), ''), GRaiz]));
        GCompat := TCompatibilidade.Create(GCatalogo, GManifesto, GReceitas);
        try
          // a amostra (ou a matriz inteira): Delphi, depois Lazarus
          {$IFDEF MSWINDOWS}
          if TemOpcao('todas') then
            EscolherDelphi(GIDEs, 'todas', GVersaoRAL, GEscolhidas)
          else
            EscolherDelphi(GIDEs, Opcao('delphi', 'amostra'), GVersaoRAL, GEscolhidas);
          {$ELSE}
          GEscolhidas.Clear;
          {$ENDIF}
          if TemOpcao('todas') then
            EscolherLazarus(GIDEs, 'todas', GVersaoRAL, GLazarus)
          else
            EscolherLazarus(GIDEs, Opcao('lazarus', 'amostra'), GVersaoRAL, GLazarus);
          for GInt := 0 to Pred(GLazarus.Count) do
            GEscolhidas.Add(GLazarus[GInt]);

          for GInt := 0 to Pred(GEscolhidas.Count) do
          begin
            GIDE := TIDEInstance(GEscolhidas[GInt]);
            WriteLn('==== ', GIDE.Nome, ' (', ExcludeTrailingPathDelimiter(GIDE.RootDir), ')');
            GTabela.Clear;
            GSaida.Atual := TStringList.Create;
            try
              // o manifesto ja recusa esta versao aqui: nao ha o que verificar
              GMotivo := GCompat.MotivoVersao(GIDE);
              if GMotivo <> '' then
              begin
                GRelatorio.Add(Format('  %-44s bloqueada: %s', [GIDE.Nome, GMotivo]));
                GNaoRodou.Add(Format('%s em %s: o manifesto já bloqueia (%s)',
                                     [GSpec, GIDE.Nome, GMotivo]));
                Continue;
              end;
              GPastaIDE := IncludeTrailingPathDelimiter(GetTempDir + 'ralvar-obj') +
                           NomeArquivo(GVersaoRAL + '-' + GIDE.Nome) + PathDelim;
              if DirectoryExists(GPastaIDE) then
                ApagarPasta(GPastaIDE);
              ForceDirectories(GPastaIDE);
              {$IFDEF MSWINDOWS}
              if GIDE.Tipo = tiDelphi then
                GOk := VerificarDelphi(GIDE, GCatalogo, GCompat, GPastaIDE, GMotivo, GTabela)
              else
              {$ENDIF}
                GOk := VerificarLazarus(GIDE, GCatalogo, GCompat, GRaiz, GPastaIDE, GMotivo,
                                        GTabela);
              VerificadasPadrao.Registrar(GVersaoRAL, GCommit, GIDE, GOk, GMotivo);
              if GOk then
                Inc(GVerificadas)
              else
                Inc(GFalhas);
              GRelatorio.Add(Format('  %-44s %s', [GIDE.Nome,
                IfThen(GOk, 'VERIFICADA', 'FALHOU: ' + GMotivo)]));
              GRelatorio.AddStrings(GTabela);
              GNomeLog := GDossie + 'saida' + PathDelim +
                          NomeArquivo(GVersaoRAL + '-' + GIDE.Nome) + '.log';
              GSaida.Atual.SaveToFile(GNomeLog);
              ApagarPasta(GPastaIDE);
            finally
              FreeAndNil(GSaida.Atual);
            end;
          end;
        finally
          GCompat.Free;
        end;
      finally
        GManifesto.Free;
        GCatalogo.Free;
      end;
    end;

    // a lista acumula entre as rodadas
    VerificadasPadrao.Salvar(GArqVerificadas);

    // o dossie, no formato do orquestrador
    GAmbiente.SaveToFile(GDossie + 'ambiente.txt');
    GRelatorio.Insert(0, Format('VARREDURA — %d verificada(s), %d falha(s)',
                                [GVerificadas, GFalhas]));
    GRelatorio.SaveToFile(GDossie + 'relatorio.txt');
    GSaida.Comandos.SaveToFile(GDossie + 'comandos.txt');
    if GNaoRodou.Count = 0 then
      GNaoRodou.Add('(tudo o que foi escolhido rodou)');
    GNaoRodou.Insert(0, 'O QUE NAO RODOU E POR QUE');
    GNaoRodou.SaveToFile(GDossie + 'nao-rodou.txt');
    ZiparPasta(GDossie, ExcludeTrailingPathDelimiter(GDossie) + '.zip');
    WriteLn;
    WriteLn(GRelatorio.Text);
    WriteLn('verificadas: ', GArqVerificadas);
    WriteLn('dossiê: ', ExcludeTrailingPathDelimiter(GDossie) + '.zip');
  finally
    GReceitas.Free;
    GLazarus.Free;
    GEscolhidas.Free;
    GSaida.Comandos.Free;
    GTabela.Free;
    GNaoRodou.Free;
    GAmbiente.Free;
    GRelatorio.Free;
    GExtras.Free;
    GVersoes.Free;
    GIDEs.Free;
    GSaida.Free;
  end;
  Halt(Ord(GFalhas > 0));
end.
