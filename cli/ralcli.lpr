/// The installer from the command line: the same steps as the screens, for
/// remote sessions, build servers and whoever prefers the terminal. The texts
/// are Portuguese.
program ralcli;

{$mode ObjFPC}{$H+}

// o instalador pela linha de comando:
//
//   ralcli ides [--buscar-em=<pasta>]...
//   ralcli versoes
//   ralcli pacotes  [--versao=<ref>|--local=<pasta>]
//   ralcli plano    --ide=<raiz> [--ide=<raiz>]... [opcoes]
//   ralcli instalar --ide=<raiz> [--ide=<raiz>]... [opcoes] [--sim]
//   ralcli recibos  [--todos]
//   ralcli desinstalar --ide=<raiz> [--ide=<raiz>]... [--sim] [--sem-reconstruir]
//                      [--limpar [--pasta=<pasta>] [--manter=<n,m>]]
//   ralcli desinstalar --desfazer=<registro> [--sim]
//   ralcli limpar   [--pasta=<pasta>] [--manter=<n,m>] [--sim]
//   ralcli versao
//   ralcli atualizar [--verificar]
//
// opcoes de plano/instalar:
//   --versao=<ref>        estavel (padrao), uma tag, um release ou um ramo
//   --local=<pasta>       os fontes do RAL ja estao aqui (nada e baixado)
//   --pasta=<pasta>       onde o RAL e as dependencias ficam (padrao: ~/RAL)
//   --pacotes=<a,b,...>   o que instalar, pelo nome (IndyRAL vale para o
//                         indyral do Lazarus); sem isso, o nucleo do RAL
//   --win64               Delphi: tambem compila o runtime de Win64
//   --somente-paths       Delphi: so library path, sem compilar
//   --so-runtime          compila os pacotes de runtime (Delphi: .bpl/.dcp e
//                         library path; Lazarus: so os links), nada vai para a
//                         IDE
//   --ignorar-existentes  baixa as dependencias mesmo que a IDE ja tenha
//   --reinstalar          compila e registra tudo, mesmo o que ja esta la
//   --remover-outros      tira da IDE os pacotes do RAL que nao foram pedidos
//                         (sem isso, os que ja estao ficam, e numa troca de
//                         versao sao recompilados junto)
//   --sem-reconstruir     Lazarus: nao reconstroi a IDE (ela pede ao abrir)
//
// A mesma versao com os mesmos pacotes nao faz nada; a mesma versao com
// pacotes a mais so instala esses; outra versao atualiza (o plano diz qual).
//   --buscar-em=<pasta>   onde mais procurar IDEs
//   --sim                 nao pergunta antes de mexer nas IDEs
//
// --ide aceita a pasta raiz da IDE, ou #<n> da lista de 'ralcli ides'.
// Codigo de saida: 0 deu certo, 1 algo falhou, 2 uso errado, 3 nada mudou numa
// IDE porque o conjunto minimo (nucleo, design e um motor) nao compilou (F14:
// a IDE ficou exatamente como estava).
//
// limpar (e desinstalar --limpar): apaga o que o instalador baixou e nenhuma
// instalacao usa mais — versoes do RAL, dependencias, DLLs em redist, o cache
// de downloads. Lista antes, com o tamanho; --manter=<n,m> guarda os itens n e
// m; --sim nao pergunta. Pasta sem a marca do instalador nunca e apagada, nem
// dependencia que ja era do usuario.
//
// Nunca se atualiza sozinho: numa rodada de servidor, trocar de versao no meio
// e defeito, nao recurso (F12). So 'ralcli atualizar' troca o binario.

uses
  {$IFDEF UNIX} cthreads, {$ENDIF}
  {$IFDEF MSWINDOWS} Windows, RALInst.IDE.Delphi, RALInst.Instalar.Delphi, {$ENDIF}
  Classes, SysUtils,
  RALInst.AutoAtualizacao, RALInst.Catalogo, RALInst.GitHub, RALInst.IDE,
  RALInst.IDE.Lazarus, RALInst.Instalar.Lazarus, RALInst.Limpeza, RALInst.Processo,
  RALInst.Recibos, RALInst.Rodada, RALInst.Versao;

// as receitas e o manifesto embutidos (ralcli.lpi, como no instalador grafico)
{$R *.res}

type
  /// Writes the core log lines to the console.
  TSaida = class
  public
    procedure Linha(const ALinha: string);
  end;

procedure TSaida.Linha(const ALinha: string);
begin
  WriteLn(ALinha);
end;

var
  GSaida: TSaida;
  GIDEs: TIDEList;
  GExtras: TStringList;

procedure Uso;
begin
  WriteLn('RAL Installer ', VersaoInstalador, ' — linha de comando');
  WriteLn;
  WriteLn('  ralcli ides [--buscar-em=<pasta>]');
  WriteLn('  ralcli versoes');
  WriteLn('  ralcli pacotes [--versao=<ref>|--local=<pasta>]');
  WriteLn('  ralcli plano --ide=<raiz|#n> [--versao=<ref>|--local=<pasta>]');
  WriteLn('               [--pasta=<pasta>] [--pacotes=a,b] [--win64] [--somente-paths]');
  WriteLn('               [--so-runtime] [--ignorar-existentes] [--reinstalar]');
  WriteLn('               [--remover-outros] [--sem-reconstruir]');
  WriteLn('  ralcli instalar (as mesmas opções do plano) [--sim]');
  WriteLn('  ralcli recibos [--todos]');
  WriteLn('  ralcli desinstalar --ide=<raiz|#n> [--sim] [--sem-reconstruir]');
  WriteLn('                     [--limpar [--pasta=<pasta>] [--manter=n,m]]');
  WriteLn('  ralcli desinstalar --desfazer=<registro em recibos\desinstalacoes> [--sim]');
  WriteLn('  ralcli limpar [--pasta=<pasta>] [--manter=n,m] [--sim]');
  WriteLn('  ralcli versao');
  WriteLn('  ralcli atualizar [--verificar]');
  Halt(2);
end;

// o valor de --nome=valor (o ultimo, se repetido); '' se nao veio
function Opcao(const ANome: string): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 2 to ParamCount do
    if SameText(Copy(ParamStr(vInt), 1, Length(ANome) + 3), '--' + ANome + '=') then
      Result := Copy(ParamStr(vInt), Length(ANome) + 4, MaxInt);
end;

procedure Opcoes(const ANome: string; ALista: TStrings);
var
  vInt: integer;
begin
  for vInt := 2 to ParamCount do
    if SameText(Copy(ParamStr(vInt), 1, Length(ANome) + 3), '--' + ANome + '=') then
      ALista.Add(Copy(ParamStr(vInt), Length(ANome) + 4, MaxInt));
end;

function Chave(const ANome: string): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := 2 to ParamCount do
    if SameText(ParamStr(vInt), '--' + ANome) then
      Exit(True);
end;

procedure ConferirOpcoes(const APermitidas: array of string);
var
  vInt, vPerm: integer;
  vParam, vNome: string;
  vAchou: boolean;
begin
  for vInt := 2 to ParamCount do
  begin
    vParam := ParamStr(vInt);
    if Copy(vParam, 1, 2) <> '--' then
    begin
      WriteLn('argumento inesperado: ', vParam);
      Halt(2);
    end;
    vNome := Copy(vParam, 3, MaxInt);
    if Pos('=', vNome) > 0 then
      vNome := Copy(vNome, 1, Pos('=', vNome) - 1);
    vAchou := False;
    for vPerm := Low(APermitidas) to High(APermitidas) do
      vAchou := vAchou or SameText(vNome, APermitidas[vPerm]);
    if not vAchou then
    begin
      WriteLn('opção desconhecida para ', ParamStr(1), ': ', vParam);
      Halt(2);
    end;
  end;
end;

procedure BuscarIDEs;
var
  vBusca: TBuscaIDE;
  vPasta: string;
begin
  GExtras := TStringList.Create;
  Opcoes('buscar-em', GExtras);
  {$IFDEF MSWINDOWS}
  vBusca := TBuscaDelphi.Create;
  try
    vBusca.BuscarPadrao(GIDEs);
    for vPasta in GExtras do
      vBusca.BuscarEm(GIDEs, vPasta, 2);
    vBusca.Finalizar(GIDEs);
  finally
    vBusca.Free;
  end;
  {$ENDIF}
  vBusca := TBuscaLazarus.Create;
  try
    vBusca.BuscarPadrao(GIDEs);
    for vPasta in GExtras do
      vBusca.BuscarEm(GIDEs, vPasta, 2);
    vBusca.Finalizar(GIDEs);
  finally
    vBusca.Free;
  end;
end;

// --ide=<raiz> ou --ide=#<n>; uma pasta fora da lista e inspecionada
function ResolverIDE(const AValor: string): TIDEInstance;
var
  vBusca: TBuscaIDE;
  vNum: integer;
begin
  Result := nil;
  if Copy(AValor, 1, 1) = '#' then
  begin
    vNum := StrToIntDef(Copy(AValor, 2, MaxInt), 0);
    if (vNum >= 1) and (vNum <= GIDEs.Count) then
      Result := GIDEs[vNum - 1];
    Exit;
  end;
  Result := GIDEs.BuscarPorRaiz(AValor);
  if Result <> nil then
    Exit;
  // pasta que a busca padrao nao acha (Lazarus num lugar incomum)
  vBusca := TBuscaLazarus.Create;
  try
    Result := vBusca.InspecionarPasta(AValor);
  finally
    vBusca.Free;
  end;
  {$IFDEF MSWINDOWS}
  if Result = nil then
  begin
    vBusca := TBuscaDelphi.Create;
    try
      Result := vBusca.InspecionarPasta(AValor);
    finally
      vBusca.Free;
    end;
  end;
  {$ENDIF}
  if Result <> nil then
    Result := GIDEs.Adicionar(Result);
end;

procedure ListarIDEs;
var
  vInt: integer;
  vIDE: TIDEInstance;
begin
  WriteLn(Format('%d IDE(s):', [GIDEs.Count]));
  for vInt := 0 to Pred(GIDEs.Count) do
  begin
    vIDE := GIDEs[vInt];
    WriteLn(Format('  #%-3d %-26s %s',
                   [vInt + 1, vIDE.Nome, ExcludeTrailingPathDelimiter(vIDE.RootDir)]));
    if vIDE.Tipo = tiLazarus then
      WriteLn(Format('       FPC %s, configuração %s',
                     [vIDE.VersaoCompilador, vIDE.ConfigDir]))
    else if vIDE.RegKey = '' then
      WriteLn('       não registrada em HKCU: abra a IDE uma vez antes de instalar');
    if vIDE.Avisos.Count > 0 then
      WriteLn('       aviso: ', vIDE.Avisos[0]);
  end;
  {$IFNDEF MSWINDOWS}
  WriteLn('(o Delphi só existe no Windows: este binário instala apenas no Lazarus)');
  {$ENDIF}
end;

function Confirmar(const APergunta: string): boolean;
var
  vResposta: string;
begin
  if Chave('sim') then
    Exit(True);
  Write(APergunta, ' [s/N] ');
  vResposta := '';
  if not EOF(Input) then
    ReadLn(vResposta);
  Result := SameText(Trim(vResposta), 's') or SameText(Trim(vResposta), 'sim');
end;

// monta a rodada com as opcoes; Halt se algo nao serve
function MontarRodada(AComIDE: boolean): TRodada;
var
  vIDEs: TStringList;
  vValor: string;
  vIDE: TIDEInstance;
begin
  Result := TRodada.Create;
  Result.Log := @GSaida.Linha;
  if Opcao('pasta') <> '' then
    Result.PastaBase := Opcao('pasta');
  if Opcao('local') <> '' then
    Result.UsarPastaLocal(Opcao('local'))
  else if not Result.EscolherVersao(Opcao('versao')) then
  begin
    WriteLn('ERRO: ', Result.Erro);
    Halt(1);
  end;
  if not Result.CarregarCatalogo then
  begin
    WriteLn('ERRO: ', Result.Erro);
    Halt(1);
  end;
  if Opcao('pacotes') <> '' then
    Result.Pacotes.CommaText := Opcao('pacotes');
  Result.Win64 := Chave('win64');
  Result.SomenteLibraryPath := Chave('somente-paths');
  Result.SoRuntime := Chave('so-runtime');
  if Result.SoRuntime and Result.SomenteLibraryPath then
  begin
    WriteLn('ERRO: --so-runtime compila os pacotes; --somente-paths não compila nada. ' +
            'Escolha um.');
    Halt(2);
  end;
  Result.IgnorarExistentes := Chave('ignorar-existentes');
  Result.Reinstalar := Chave('reinstalar');
  Result.ManterInstalados := not Chave('remover-outros');
  Result.ConstruirIDE := not Chave('sem-reconstruir');
  if not AComIDE then
    Exit;
  vIDEs := TStringList.Create;
  try
    Opcoes('ide', vIDEs);
    if vIDEs.Count = 0 then
    begin
      WriteLn('ERRO: diga em qual IDE: --ide=<raiz> (veja ralcli ides)');
      Halt(2);
    end;
    for vValor in vIDEs do
    begin
      vIDE := ResolverIDE(vValor);
      if vIDE = nil then
      begin
        WriteLn('ERRO: não é uma IDE conhecida: ', vValor);
        Halt(1);
      end;
      Result.AdicionarIDE(vIDE);
    end;
  finally
    vIDEs.Free;
  end;
end;

procedure MostrarVersoes;
var
  vRodada: TRodada;
  vInt: integer;
  vOutras: boolean;
begin
  vRodada := TRodada.Create;
  try
    vRodada.Log := @GSaida.Linha;
    if not vRodada.ListarVersoes then
    begin
      WriteLn('ERRO: ', vRodada.Erro);
      Halt(1);
    end;
    // F16: as estaveis primeiro; pre-lancamentos, tags antigas e ramos depois
    WriteLn('Estáveis:');
    vOutras := False;
    for vInt := 0 to Pred(vRodada.Versoes.Count) do
    begin
      if vRodada.Versoes[vInt].Outra and not vOutras then
      begin
        WriteLn('Outras (pré-lançamentos, tags antigas e ramos):');
        vOutras := True;
      end;
      WriteLn('  ', vRodada.Versoes[vInt].Descricao);
    end;
  finally
    vRodada.Free;
  end;
end;

// F19: o que o instalador baixou e ninguem usa; True quando nada falhou
function LimparSobras: boolean;
var
  vSobras: TSobras;
  vBases, vManter: TStringList;
  vInt, vNum: integer;
  vPasta: string;
begin
  Result := True;
  vSobras := TSobras.Create(True);
  vBases := TStringList.Create;
  vManter := TStringList.Create;
  try
    vPasta := Opcao('pasta');
    if vPasta = '' then
      vPasta := IncludeTrailingPathDelimiter(GetUserDir) + 'RAL';
    vBases.Add(vPasta);
    vSobras.Listar(vBases, PastaDadosInstalador + 'recibos');
    if vSobras.Count = 0 then
    begin
      WriteLn('Nada a limpar: tudo o que o instalador baixou ainda é usado por alguma ' +
              'IDE.');
      Exit;
    end;
    vManter.CommaText := Opcao('manter');
    for vInt := 0 to Pred(vManter.Count) do
    begin
      vNum := StrToIntDef(vManter[vInt], 0);
      if (vNum >= 1) and (vNum <= vSobras.Count) then
        vSobras[vNum - 1].Manter := True;
    end;
    WriteLn('O instalador baixou e nenhuma IDE usa mais:');
    for vInt := 0 to Pred(vSobras.Count) do
      WriteLn(Format('  %s %2d. %-34s %10s  %s',
        [BoolToStr(vSobras[vInt].Manter, '[ ]', '[x]'), vInt + 1,
         vSobras[vInt].Descricao, DescreverTamanho(vSobras[vInt].Tamanho),
         vSobras[vInt].Caminho]));
    WriteLn('Total a apagar: ', DescreverTamanho(vSobras.TotalBytes),
            ' (--manter=n,m guarda itens)');
    if not Confirmar('Apagar os itens marcados? Não há como desfazer.') then
    begin
      WriteLn('Nada foi apagado.');
      Exit;
    end;
    Result := vSobras.Apagar(PastaDadosInstalador + 'recibos', @GSaida.Linha);
  finally
    vManter.Free;
    vBases.Free;
    vSobras.Free;
  end;
end;

procedure MostrarPacotes;
var
  vRodada: TRodada;
  vLista: TList;
  vTipo: TTipoPacote;
  vInt: integer;
  vPacote: TPacote;
begin
  vRodada := MontarRodada(False);
  vLista := TList.Create;
  try
    for vTipo := Low(TTipoPacote) to High(TTipoPacote) do
    begin
      vRodada.Catalogo.Listar(vTipo, vLista);
      WriteLn(NomeTipoPacote(vTipo), ':');
      for vInt := 0 to Pred(vLista.Count) do
      begin
        vPacote := TPacote(vLista[vInt]);
        WriteLn(Format('  %-24s %-14s %s',
                       [vPacote.Nome, vPacote.Grupo, vPacote.Descricao]));
      end;
    end;
  finally
    vLista.Free;
    vRodada.Free;
  end;
end;

procedure Planejar(AExecutar: boolean);
var
  vRodada: TRodada;
  vOk: boolean;
  vInt: integer;
begin
  vRodada := MontarRodada(True);
  try
    WriteLn(vRodada.Plano);
    if not AExecutar then
      Exit;
    // sem motor numa IDE nao instala: recusa antes de perguntar ou baixar
    if Trim(vRodada.SemMotor) <> '' then
    begin
      WriteLn('ERRO: ', Trim(vRodada.SemMotor));
      Halt(1);
    end;
    // a mesma versao com os mesmos pacotes em todas: nada a perguntar
    if vRodada.NadaAFazer then
    begin
      WriteLn('Nada a fazer: as IDEs já têm esta versão com estes pacotes ' +
              '(--reinstalar refaz tudo).');
      Halt(0);
    end;
    if not Confirmar('As IDEs acima terão a configuração alterada; feche-as antes. ' +
                     'Instalar?') then
    begin
      WriteLn('Nada foi feito.');
      Halt(1);
    end;
    vOk := vRodada.Executar;
    WriteLn;
    WriteLn('Resumo:');
    for vInt := 0 to Pred(vRodada.Relatorio.Count) do
      WriteLn('  ', vRodada.Relatorio[vInt]);
    if vRodada.Erro <> '' then
      WriteLn('ERRO: ', vRodada.Erro);
    // F14: alguma IDE ficou exatamente como estava: o conjunto minimo falhou
    if not vOk and (vRodada.IDEsIntactas.Count > 0) then
      Halt(3);
    Halt(Ord(not vOk));
  finally
    vRodada.Free;
  end;
end;

procedure MostrarRecibos;
var
  vRecibos: TRecibos;
  vInt: integer;
begin
  vRecibos := TRecibos.Create(True);
  try
    vRecibos.Carregar(PastaDadosInstalador + 'recibos', Chave('todos'));
    WriteLn(Format('%d instalação(ões) registrada(s):', [vRecibos.Count]));
    for vInt := 0 to Pred(vRecibos.Count) do
    begin
      WriteLn('  ', vRecibos[vInt].Descricao);
      WriteLn('      ', ExcludeTrailingPathDelimiter(vRecibos[vInt].IDERaiz), ': ',
              vRecibos[vInt].Pacotes.CommaText);
    end;
  finally
    vRecibos.Free;
  end;
end;

// o motor de cada tipo de IDE, sem catalogo: os nomes do RAL vem da arvore
// que a IDE aponta e dos pacotes que toda versao tem
function PlanoDesinstalarIDE(AIDE: TIDEInstance): string;
begin
  {$IFDEF MSWINDOWS}
  if AIDE.Tipo = tiDelphi then
    with TInstalacaoDelphi.Create(AIDE, nil) do
    try
      Result := PlanoDesinstalar;
    finally
      Free;
    end
  else
  {$ENDIF}
    with TInstalacaoLazarus.Create(AIDE, nil) do
    try
      ConstruirIDE := not Chave('sem-reconstruir');
      Result := PlanoDesinstalar;
    finally
      Free;
    end;
end;

function DesinstalarDaIDE(AIDE: TIDEInstance): boolean;
var
  vRelatorio: string;
begin
  {$IFDEF MSWINDOWS}
  if AIDE.Tipo = tiDelphi then
    with TInstalacaoDelphi.Create(AIDE, nil) do
    try
      Log := @GSaida.Linha;
      Result := Desinstalar;
      vRelatorio := Relatorio.Text;
    finally
      Free;
    end
  else
  {$ENDIF}
    with TInstalacaoLazarus.Create(AIDE, nil) do
    try
      Log := @GSaida.Linha;
      ConstruirIDE := not Chave('sem-reconstruir');
      Result := Desinstalar;
      vRelatorio := Relatorio.Text;
    finally
      Free;
    end;
  if Trim(vRelatorio) <> '' then
    WriteLn(TrimRight(vRelatorio));
end;

procedure Desinstalar;
var
  vIDEs: TStringList;
  vValor: string;
  vIDE: TIDEInstance;
  vOk: boolean;
begin
  // desfazer uma desinstalacao: o registro que ela deixou
  vValor := Opcao('desfazer');
  if vValor <> '' then
  begin
    if not Confirmar('Devolver o que a desinstalação tirou (' + vValor + ')? ' +
                     'Feche a IDE antes.') then
    begin
      WriteLn('Nada foi feito.');
      Halt(1);
    end;
    Halt(Ord(not DesfazerDesinstalacao(vValor, @GSaida.Linha)));
  end;

  vIDEs := TStringList.Create;
  try
    Opcoes('ide', vIDEs);
    if vIDEs.Count = 0 then
    begin
      WriteLn('ERRO: diga de qual IDE: --ide=<raiz> (veja ralcli ides)');
      Halt(2);
    end;
    for vValor in vIDEs do
    begin
      vIDE := ResolverIDE(vValor);
      if vIDE <> nil then
        WriteLn(PlanoDesinstalarIDE(vIDE));
    end;
    if not Confirmar('Tirar o RAL destas IDEs (o que o instalador fez e o que foi ' +
                     'instalado à mão)? Feche-as antes.') then
    begin
      WriteLn('Nada foi feito.');
      Halt(1);
    end;
    vOk := True;
    for vValor in vIDEs do
    begin
      vIDE := ResolverIDE(vValor);
      if vIDE = nil then
      begin
        WriteLn('ERRO: não é uma IDE conhecida: ', vValor);
        vOk := False;
        Continue;
      end;
      WriteLn('==== ', vIDE.Nome);
      if not DesinstalarDaIDE(vIDE) then
        vOk := False;
    end;
    // F19: o que ficou para tras e ninguem usa
    if Chave('limpar') then
    begin
      WriteLn;
      if not LimparSobras then
        vOk := False;
    end;
    Halt(Ord(not vOk));
  finally
    vIDEs.Free;
  end;
end;
procedure Atualizar;
var
  vAtu: TAtualizacao;
  vNovo, vNovoMigrador: string;
begin
  // a CLI troca a si mesma (o ralcli do release, nao o instalador grafico) e o
  // migrador (F21)
  vAtu := TAtualizacao.Create(ProgramaCLI);
  try
    vAtu.Log := @GSaida.Linha;
    case vAtu.Verificar of
      rvAtualizado:
        begin
          WriteLn('Atualizado: ', VersaoInstalador, ' é a versão mais nova.');
          Exit;
        end;
      rvNaoVerificou:
        begin
          // nao e "esta atualizado"
          WriteLn('Não deu para verificar: ', vAtu.Erro);
          Halt(1);
        end;
    end;
    WriteLn('Versão nova: ', vAtu.VersaoNova, ' (esta é a ', VersaoInstalador, ')');
    if Chave('verificar') then
      Exit;
    // a CLI e o executavel em uso: troca o arquivo, e a proxima chamada ja e
    // a versao nova (o .old sai quando o grafico abrir, ou na proxima troca)
    if not vAtu.Baixar(vNovo) or not vAtu.BaixarMigrador(vNovoMigrador) or
       not vAtu.Trocar(vNovo) then
    begin
      WriteLn('ERRO: ', vAtu.Erro);
      SysUtils.DeleteFile(vNovo);
      SysUtils.DeleteFile(vNovoMigrador);
      Halt(1);
    end;
    if not vAtu.TrocarMigrador(vNovoMigrador) and vAtu.MigradorPendente then
      WriteLn('aviso: ', vAtu.Erro);
    WriteLn('Atualizado para a versão ', vAtu.VersaoNova, '.');
  finally
    vAtu.Free;
  end;
end;

var
  GVerbo: string;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  // os textos do nucleo sao UTF-8, como na tela (a LazUTF8 faz o mesmo la):
  // sem isto o WriteLn os converteria de novo, como se fossem da pagina do sistema
  DefaultSystemCodePage := CP_UTF8;
  SetTextCodePage(Output, CP_UTF8);
  SetTextCodePage(StdErr, CP_UTF8);
  {$ENDIF}
  if ParamCount = 0 then
    Uso;
  GVerbo := LowerCase(ParamStr(1));
  GSaida := TSaida.Create;
  GIDEs := TIDEList.Create(True);
  try
    if GVerbo = 'versao' then
      WriteLn('RAL Installer ', VersaoInstalador)
    else if GVerbo = 'atualizar' then
    begin
      ConferirOpcoes(['verificar']);
      Atualizar;
    end
    else if GVerbo = 'ides' then
    begin
      ConferirOpcoes(['buscar-em']);
      BuscarIDEs;
      ListarIDEs;
    end
    else if GVerbo = 'versoes' then
    begin
      ConferirOpcoes([]);
      MostrarVersoes;
    end
    else if GVerbo = 'pacotes' then
    begin
      ConferirOpcoes(['versao', 'local']);
      MostrarPacotes;
    end
    else if (GVerbo = 'plano') or (GVerbo = 'instalar') then
    begin
      ConferirOpcoes(['ide', 'versao', 'local', 'pasta', 'pacotes', 'win64',
                      'somente-paths', 'so-runtime', 'ignorar-existentes',
                      'reinstalar', 'remover-outros', 'sem-reconstruir', 'buscar-em',
                      'sim']);
      BuscarIDEs;
      Planejar(GVerbo = 'instalar');
    end
    else if GVerbo = 'recibos' then
    begin
      ConferirOpcoes(['todos']);
      MostrarRecibos;
    end
    else if GVerbo = 'desinstalar' then
    begin
      ConferirOpcoes(['ide', 'sim', 'sem-reconstruir', 'buscar-em', 'desfazer',
                      'limpar', 'pasta', 'manter']);
      BuscarIDEs;
      Desinstalar;
    end
    else if GVerbo = 'limpar' then
    begin
      ConferirOpcoes(['pasta', 'manter', 'sim']);
      Halt(Ord(not LimparSobras));
    end
    else
      Uso;
  finally
    GExtras.Free;
    GIDEs.Free;
    GSaida.Free;
  end;
end.
