program ralmigra;

{$mode ObjFPC}{$H+}

// Migrador do RAL na linha de comando (M2 a M8). Traduzida como a janela: os
// textos fixos estao em RALMig.Console.Mensagens e o .po embutido e o mesmo
// (languages/ralmigrador.<idioma>.po).
//
//   ralmigra detectar <pasta>
//   ralmigra previa   <pasta> [--de=<versao>] [--para=<versao>]
//   ralmigra aplicar  <pasta> [--de=] [--para=] [--backup] [--converter-binarios]
//                     [--forcar-de]  (aplica desde --de mesmo se o projeto ja
//                     esta numa versao mais nova; sem ela esses passos pulam)
//   ralmigra desfazer <pasta> [--zip=<copia>]
//   ralmigra conferir <pasta> [--ide=<raiz da IDE>] [--ral=<raiz do RAL>] [--para=]
//   ralmigra inventario
//   ralmigra --ajuda
//
// Opcoes: --idioma=pt-BR|en-US|es-ES (padrao: o do sistema),
//   --raiz-ral=<pasta do RAL de destino> (le o ralmigracao.json dela).
// A previa e o aplicar validam o resultado contra a API do destino
// (RALMig.Validacao) depois de simular; o que a validacao achar entra no
// relatorio e no codigo de saida. O conferir compila o projeto (a pasta ja
// migrada) com a IDE dada, ou com as IDEs que os recibos do instalador dizem ter
// o RAL de destino; mostra o primeiro erro de cada arquivo e sai 0 se tudo
// compilou, 2 se nao.
// Sem --de a versao e detectada; sem --para vale a mais nova. A previa nao
// grava nada e mostra o relatorio em texto. O aplicar grava o relatorio
// (MIGRACAO-RAL-<de>-<para>.md e .html na pasta do projeto; com --backup o desfazer
// tira tambem esses relatorios, se ninguem os mudou, e apaga a copia usada) e
// mostra o resumo; avisa se a pasta e um repositorio git com mudancas nao
// commitadas e se nao ha copia de seguranca (sem --backup nao ha desfazer). Saida: 0 nada manual, 1 sobrou item manual, 2 erro, 3 recusado
// (destino mais velho que a origem).
//
// Recursos embutidos (RCDATA) no .lpi: REGRAS_1_0__1_1, REGRAS_1_1__1_2,
// REGRAS_1_2__2_0 (regras/*.json) e API_V1_0, API_1_1, API_DEV, API_2_0
// (api/api-v1.0.json, api-1.1.json, api-dev.json, api-2.0.json). Sem eles o
// programa procura migrador/regras e migrador/api subindo da pasta do exe.

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Classes, SysUtils,
  RALInst.Traducao, RALInst.IDE,
  RALMig.Texto, RALMig.Tipos, RALMig.Regras, RALMig.Inventario, RALMig.API,
  RALMig.Previa, RALMig.Copia, RALMig.Motor, RALMig.Deteccao, RALMig.Projeto,
  RALMig.Acoes.Projeto, RALMig.Acoes.Formulario, RALMig.Acoes.Codigo,
  RALMig.Mensagens, RALMig.Console.Mensagens, RALMig.Relatorio,
  RALMig.Validacao, RALMig.Conferir;

var
  GPosicionais: TStringList;
  GOpcoes: TStringList;
  /// Language of the run (--idioma= or the system's), also the report's
  GIdioma: string;

function Opcao(const ANome, APadrao: string): string;
begin
  Result := GOpcoes.Values[ANome];
  if Result = '' then
    Result := APadrao;
end;

function TemOpcao(const ANome: string): boolean;
begin
  Result := GOpcoes.IndexOfName(ANome) >= 0;
end;

procedure LerArgumentos;
var
  vInt: integer;
  vArg: string;
begin
  for vInt := 1 to ParamCount do
  begin
    vArg := ParamStr(vInt);
    if Copy(vArg, 1, 2) = '--' then
    begin
      Delete(vArg, 1, 2);
      if Pos('=', vArg) = 0 then
        vArg := vArg + '=1';
      GOpcoes.Add(vArg);
    end
    else
      GPosicionais.Add(vArg);
  end;
end;

function NomeConfianca(AConf: TConfianca): string;
begin
  case AConf of
    cfAlta: Result := cmCliConfAlta;
    cfMedia: Result := cmCliConfMedia;
    cfBaixa: Result := cmCliConfBaixa;
  else
    Result := '-';
  end;
end;

procedure ConfigurarMotor(AMotor: TMotorMigracao);
begin
  AMotor.Pasta := GPosicionais[1];
  AMotor.De := Opcao('de', '');
  AMotor.Para := Opcao('para', '');
  AMotor.Idioma := GIdioma;
  AMotor.ConverterBinarios := TemOpcao('converter-binarios');
  AMotor.ForcarDe := TemOpcao('forcar-de');
  AMotor.RaizRAL := Opcao('raiz-ral', '');
  AMotor.PastaRegras := Opcao('regras', '');
  AMotor.PastaApi := Opcao('api', '');
end;

function CmdDetectar: integer;
var
  vMotor: TMotorMigracao;
  vEvid: TStrings;
  vVersao: string;
  vInt: integer;
begin
  vMotor := TMotorMigracao.Create;
  try
    ConfigurarMotor(vMotor);
    vVersao := vMotor.Detectar(vEvid);
    try
      for vInt := 0 to vEvid.Count - 1 do
        WriteLn('  ', vEvid[vInt]);
      if vVersao = '' then
        WriteLn(cmCliNenhuma)
      else
        WriteLn(Format(cmCliDetectada, [vVersao, NomeConfianca(vMotor.Confianca)]));
    finally
      vEvid.Free;
    end;
    Result := SAIDA_OK;
  finally
    vMotor.Free;
  end;
end;

function VersaoDeOrigem(AMotor: TMotorMigracao): boolean;
var
  vEvid: TStrings;
begin
  Result := True;
  if AMotor.De <> '' then
    Exit;
  AMotor.De := AMotor.Detectar(vEvid);
  vEvid.Free;
  Result := AMotor.De <> '';
end;

procedure ListarDiffs(AMotor: TMotorMigracao);
var
  vLista: TList;
  vInt: integer;
  vArq: TArquivoMigrado;
  vAntes: RawByteString;
  vDiff: string;
begin
  vLista := TList.Create;
  try
    AMotor.Conjunto.ListarAlterados(vLista);
    for vInt := 0 to vLista.Count - 1 do
    begin
      vArq := TArquivoMigrado(vLista[vInt]);
      vAntes := '';
      if FileExists(vArq.Caminho) then
        vAntes := LerBytes(vArq.Caminho);
      vDiff := DiffUnificado(vArq.Relativo, vAntes, vArq.Atual);
      if vDiff = '' then
        Continue;
      WriteLn(vDiff);
    end;
    if vLista.Count = 0 then
      WriteLn(cmCliPreviaNada);
  finally
    vLista.Free;
  end;
end;

procedure ListarFora(AMotor: TMotorMigracao);
var
  vInt: integer;
begin
  if AMotor.UnitsFora <> nil then
    for vInt := 0 to AMotor.UnitsFora.Count - 1 do
      WriteLn(Format(cmCliFora, [AMotor.UnitsFora[vInt]]));
end;

/// The report as text on the screen (preview, refused run, errors)
procedure MostrarRelatorio(AMotor: TMotorMigracao; AGravado: boolean; const AZip: string);
var
  vDados: TDadosRelatorio;
begin
  vDados := DadosRelatorio(AMotor.Resultado, AMotor.Conjunto, '', AZip, AGravado);
  if (vDados.Pasta = '') and (AMotor.Conjunto = nil) then
    vDados.Pasta := GPosicionais[1];
  Write(GerarMarkdown(vDados));
  ListarFora(AMotor);
end;

function ContarAlterados(AMotor: TMotorMigracao): integer;
var
  vLista: TList;
begin
  vLista := TList.Create;
  try
    AMotor.Conjunto.ListarAlterados(vLista);
    Result := vLista.Count;
  finally
    vLista.Free;
  end;
end;

procedure AvisarGit(AMotor: TMotorMigracao);
var
  vDetalhe: string;
begin
  if GitComMudancas(AMotor.Conjunto.Pasta, vDetalhe) then
    WriteLn(Format(wmGitComMudancas, [vDetalhe]));
end;

/// Tells the safety copy about the report files just written, so that undo
/// removes them too (when nobody changed them since)
procedure RegistrarRelatoriosNaCopia(AMotor: TMotorMigracao; const AMd, AHtml: string);
var
  vRelativos: TStringList;
  vErro: string;
begin
  if AMotor.ZipCopia = '' then
    Exit;
  vRelativos := TStringList.Create;
  try
    vRelativos.Add(ExtractFileName(AMd));
    vRelativos.Add(ExtractFileName(AHtml));
    if not RegistrarRelatorios(AMotor.ZipCopia, AMotor.Conjunto.Pasta, vRelativos, vErro) then
      WriteLn(Format(wmCliRelatorioSemRegistro, [vErro]));
  finally
    vRelativos.Free;
  end;
end;

/// Writes the report files; with nothing changed the report of the earlier run
/// (which did the work) is kept and the result only goes to the screen
procedure GravarRelatorioDaRodada(AMotor: TMotorMigracao; AAlterados: integer);
var
  vDados: TDadosRelatorio;
  vMd, vHtml, vErro: string;
begin
  vDados := DadosRelatorio(AMotor.Resultado, AMotor.Conjunto, AMotor.Conjunto.Pasta,
    AMotor.ZipCopia, True);
  vMd := AMotor.Conjunto.Pasta + NomeRelatorio(AMotor.Resultado.De, AMotor.Resultado.Para, 'md');
  if (AAlterados = 0) and FileExists(vMd) then
  begin
    WriteLn(Format(cmCliRelatorioMantido, [vMd]));
    Write(GerarMarkdown(vDados));
  end
  else if GravarRelatorio(AMotor.Conjunto.Pasta, vDados, vMd, vHtml, vErro) then
  begin
    WriteLn(Format(cmCliRelatorioMd, [vMd]));
    WriteLn(Format(cmCliRelatorioHtml, [vHtml]));
    RegistrarRelatoriosNaCopia(AMotor, vMd, vHtml);
  end
  else
    WriteLn(Format(emCliRelatorio, [vErro]));
end;

function AplicarMigracao(AMotor: TMotorMigracao; ACodigo: integer): integer;
var
  vAlterados: integer;
begin
  Result := ACodigo;
  vAlterados := ContarAlterados(AMotor);
  if vAlterados > 0 then
    AvisarGit(AMotor);
  if not AMotor.Gravar(TemOpcao('backup')) then
  begin
    WriteLn(Format(emCliGravar, [AMotor.ErroGravacao]));
    Exit(SAIDA_ERRO);
  end;
  WriteLn(Format(cmCliGravados, [vAlterados]));
  if AMotor.ZipCopia <> '' then
    WriteLn(Format(cmCliCopia, [AMotor.ZipCopia]))
  else if vAlterados > 0 then
    WriteLn(wmCliSemCopia);
  GravarRelatorioDaRodada(AMotor, vAlterados);
  WriteLn(LinhaResumo(DadosRelatorio(AMotor.Resultado, AMotor.Conjunto,
    AMotor.Conjunto.Pasta, AMotor.ZipCopia, True)));
  ListarFora(AMotor);
end;

/// Checks the migrated texts against the API of the target version; what it
/// finds becomes manual items of the result (report and exit code)
procedure ValidarDestino(AMotor: TMotorMigracao);
var
  vApi: TApi;
begin
  if AMotor.Recusado or (AMotor.Conjunto = nil) or (AMotor.Resultado.Erros.Count > 0) then
    Exit;
  vApi := CarregarApiDaVersao(AMotor.Resultado.Para, AMotor.PastaApi);
  if vApi = nil then
  begin
    WriteLn(Format(wmCliSemApi, [AMotor.Resultado.Para]));
    Exit;
  end;
  try
    ValidarResultado(AMotor.Conjunto, vApi, AMotor.Resultado, AMotor.PastaApi);
  finally
    vApi.Free;
  end;
end;

function CmdMigrar(AAplicar: boolean): integer;
var
  vMotor: TMotorMigracao;
begin
  vMotor := TMotorMigracao.Create;
  try
    ConfigurarMotor(vMotor);
    if not VersaoDeOrigem(vMotor) then
    begin
      WriteLn(emCliSemVersao);
      Exit(SAIDA_ERRO);
    end;
    if vMotor.Para = '' then
      vMotor.Para := VersoesDeApi[High(VersoesDeApi)];
    vMotor.Simular;
    ValidarDestino(vMotor);
    Result := vMotor.CodigoSaida;
    if vMotor.Recusado or (vMotor.Conjunto = nil) then
      MostrarRelatorio(vMotor, False, '')
    else if not AAplicar then
    begin
      ListarDiffs(vMotor);
      MostrarRelatorio(vMotor, False, '');
    end
    else if (vMotor.Resultado.Erros.Count > 0) and not TemOpcao('ignorar-erros') then
      MostrarRelatorio(vMotor, False, '')
    else
      Result := AplicarMigracao(vMotor, Result);
  finally
    vMotor.Free;
  end;
end;

function CmdDesfazer: integer;
var
  vCopias: TStringList;
  vZip, vErro: string;
  vMudados, vMantidos: TStringList;
  vInt: integer;
begin
  vZip := Opcao('zip', '');
  if vZip = '' then
  begin
    vCopias := ListarCopias(GPosicionais[1]);
    try
      if vCopias.Count > 0 then
        vZip := vCopias[0];
    finally
      vCopias.Free;
    end;
  end;
  if vZip = '' then
  begin
    WriteLn(Format(emCliSemCopia, [GPosicionais[1]]));
    Exit(SAIDA_ERRO);
  end;
  vMudados := TStringList.Create;
  vMantidos := TStringList.Create;
  try
    ListarRelatoriosMantidos(vZip, GPosicionais[1], vMantidos);
    if Desfazer(vZip, GPosicionais[1], vMudados, vErro) then
    begin
      for vInt := 0 to vMudados.Count - 1 do
        WriteLn(Format(wmCopiaMudadoDepois, [vMudados[vInt]]));
      for vInt := 0 to vMantidos.Count - 1 do
        WriteLn(Format(wmCliRelatorioMantido, [vMantidos[vInt]]));
      DescartarCopia(vZip, GPosicionais[1]);
      WriteLn(Format(cmCliDesfeito, [vZip]));
      Result := SAIDA_OK;
    end
    else
    begin
      WriteLn(vErro);
      Result := SAIDA_ERRO;
    end;
  finally
    vMantidos.Free;
    vMudados.Free;
  end;
end;

function CmdInventario: integer;
var
  vCob: TCobertura;
  vPasta, vRes: string;
  vInt, vPen: integer;
  vResultado: TResultadoPasso;
begin
  Result := SAIDA_ERRO;
  vPasta := AcharPastaMigrador;
  vCob := TCobertura.Create;
  try
    vRes := vPasta + PathDelim + 'regras' + PathDelim + 'cobertura.json';
    if (vPasta = '') or not vCob.CarregarArquivo(vRes) then
    begin
      WriteLn(vRes);
      Exit;
    end;
    vPen := 0;
    for vInt := 0 to vCob.PassoCount - 1 do
    begin
      vResultado := ProcessarPasso(vCob.Passos[vInt], vPasta + PathDelim + 'api');
      try
        WriteLn(Format(cmCliInventarioPasso, [vCob.Passos[vInt].Rotulo,
          vResultado.Diferencas.Count, vResultado.Pendentes.Count]));
        Inc(vPen, vResultado.Pendentes.Count);
      finally
        vResultado.Free;
      end;
    end;
    if vPen > 0 then
      Result := SAIDA_MANUAL
    else
      Result := SAIDA_OK;
  finally
    vCob.Free;
  end;
end;

/// The IDEs that will compile: the one given by hand (--ide with --ral) or the
/// ones the installer's receipts say have the RAL of the target version
function IDEsParaConferir(ALista: TIDEsConferencia; const APara: string): boolean;
begin
  Result := False;
  if TemOpcao('ide') then
  begin
    if Opcao('ral', '') = '' then
      WriteLn(emCliConferirSemRal)
    else if AdicionarIDEManual(ALista, Opcao('ide', ''), Opcao('ral', '')) = nil then
      WriteLn(Format(emCliConferirIDEInvalida, [Opcao('ide', '')]))
    else
      Result := True;
    Exit;
  end;
  IDEsComRAL(APara, ALista);
  Result := ALista.Count > 0;
  if not Result then
    WriteLn(Format(emCliConferirSemIDE, [APara]));
end;

/// The project files of the folder one IDE can compile: .dpr (or the .dproj with
/// no .dpr beside it) for Delphi, .lpi for Lazarus
procedure ProjetosDaIDE(AConjunto: TConjuntoArquivos; ATipo: TTipoIDE; ADest: TStringList);
var
  vLista: TList;
  vInt: integer;
  vCaminho: string;
begin
  vLista := TList.Create;
  try
    if ATipo = tiDelphi then
      AConjunto.Listar([taDpr, taDproj], vLista)
    else
      AConjunto.Listar([taLpi], vLista);
    for vInt := 0 to vLista.Count - 1 do
    begin
      vCaminho := TArquivoMigrado(vLista[vInt]).Caminho;
      if (TArquivoMigrado(vLista[vInt]).Tipo = taDproj) and
         FileExists(ChangeFileExt(vCaminho, '.dpr')) then
        Continue;
      ADest.Add(vCaminho);
    end;
  finally
    vLista.Free;
  end;
end;

/// Compiles the projects of one IDE; counts the ones that compiled and failed
procedure ConferirNaIDE(ACand: TIDEConferencia; AProjetos: TStringList;
  var AOk, AFalhas: integer);
var
  vInt: integer;
  vErro: string;
begin
  for vInt := 0 to AProjetos.Count - 1 do
  begin
    WriteLn(Format(cmCliConferirCompilando, [ExtractFileName(AProjetos[vInt]),
      ACand.Descricao]));
    if CompilarProjeto(ACand.IDE, AProjetos[vInt], ACand.RaizRAL, vErro) then
    begin
      Inc(AOk);
      WriteLn(Format(cmCliConferirOk, [AProjetos[vInt]]));
    end
    else
    begin
      Inc(AFalhas);
      WriteLn(Format(cmCliConferirFalhou, [AProjetos[vInt]]));
      if vErro <> '' then
        WriteLn('  ' + StringReplace(vErro, LineEnding, LineEnding + '  ', [rfReplaceAll]));
    end;
  end;
end;

function CmdConferir: integer;
var
  vLocal: TLocalizadorArquivos;
  vIDEs: TIDEsConferencia;
  vProjetos: TStringList;
  vInt, vOk, vFalhas: integer;
begin
  Result := SAIDA_ERRO;
  vIDEs := TIDEsConferencia.Create;
  vProjetos := TStringList.Create;
  vLocal := TLocalizadorArquivos.Create(GPosicionais[1], False);
  try
    if not IDEsParaConferir(vIDEs, NormalizarVersao(Opcao('para', VersoesDeApi[High(VersoesDeApi)]))) then
      Exit;
    vLocal.Executar;
    vOk := 0;
    vFalhas := 0;
    for vInt := 0 to vIDEs.Count - 1 do
    begin
      vProjetos.Clear;
      ProjetosDaIDE(vLocal.Conjunto, vIDEs[vInt].IDE.Tipo, vProjetos);
      if vProjetos.Count = 0 then
        WriteLn(Format(emCliConferirSemProjeto, [vIDEs[vInt].Descricao, GPosicionais[1]]))
      else
        ConferirNaIDE(vIDEs[vInt], vProjetos, vOk, vFalhas);
    end;
    WriteLn(Format(cmCliConferirResumo, [vOk, vFalhas]));
    if (vOk > 0) and (vFalhas = 0) then
      Result := SAIDA_OK;
  finally
    vLocal.Free;
    vProjetos.Free;
    vIDEs.Free;
  end;
end;

procedure CmdAjuda;
begin
  WriteLn(cmAjudaTitulo);
  WriteLn;
  WriteLn(cmAjudaComandos);
  WriteLn(cmAjudaDetectar);
  WriteLn(cmAjudaPrevia);
  WriteLn(cmAjudaAplicar);
  WriteLn(cmAjudaDesfazer);
  WriteLn(cmAjudaConferir);
  WriteLn(cmAjudaInventario);
  WriteLn;
  WriteLn(cmAjudaOpcoes);
  WriteLn(cmAjudaDe);
  WriteLn(cmAjudaPara);
  WriteLn(cmAjudaIdioma);
  WriteLn(cmAjudaBackup);
  WriteLn(cmAjudaConverter);
  WriteLn(cmAjudaForcar);
  WriteLn(cmAjudaIgnorarErros);
  WriteLn(cmAjudaZip);
  WriteLn(cmAjudaIDE);
  WriteLn(cmAjudaRal);
  WriteLn;
  WriteLn(cmAjudaSaida);
end;

var
  GVerbo: string;

function Despachar: integer;
begin
  Result := SAIDA_ERRO;
  GVerbo := LowerCase(GPosicionais[0]);
  if (GVerbo <> 'inventario') and (GPosicionais.Count < 2) then
    WriteLn(emCliSemPasta)
  else if GVerbo = 'detectar' then
    Result := CmdDetectar
  else if GVerbo = 'previa' then
    Result := CmdMigrar(False)
  else if GVerbo = 'aplicar' then
    Result := CmdMigrar(True)
  else if GVerbo = 'desfazer' then
    Result := CmdDesfazer
  else if GVerbo = 'conferir' then
    Result := CmdConferir
  else if GVerbo = 'inventario' then
    Result := CmdInventario
  else
    WriteLn(Format(emCliVerboDesconhecido, [GPosicionais[0]]));
end;

begin
  DefaultSystemCodePage := CP_UTF8;
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  {$ENDIF}
  PrefixoPO := 'ralmigrador';
  GPosicionais := TStringList.Create;
  GOpcoes := TStringList.Create;
  try
    LerArgumentos;
    GIdioma := NormalizarIdioma(Opcao('idioma', ''));
    if GIdioma = '' then
      GIdioma := IdiomaDoSistema;
    TraduzirMensagens(GIdioma);
    ExitCode := SAIDA_ERRO;
    if TemOpcao('ajuda') or TemOpcao('help') or
       ((GPosicionais.Count > 0) and (LowerCase(GPosicionais[0]) = 'ajuda')) then
    begin
      CmdAjuda;
      ExitCode := SAIDA_OK;
    end
    else if GPosicionais.Count = 0 then
      WriteLn(emCliUso)
    else
      ExitCode := Despachar;
  finally
    GPosicionais.Free;
    GOpcoes.Free;
  end;
end.
