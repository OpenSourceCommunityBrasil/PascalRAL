/// User-facing texts of the installer core, kept in one place for translation.
unit RALInst.Mensagens;

{$mode ObjFPC}{$H+}

{ Como no RALConsts do PascalRAL, o prefixo diz o peso da mensagem:
    em = erro: algo nao foi feito
    wm = aviso: foi feito, mas o usuario precisa saber de algo
    cm = texto comum: rotulos, plano, relatorio e log

  O texto original e o portugues; as traducoes ficam em
  languages/ralinstaller.<idioma>.po (embutidas no executavel) e entram em
  tempo de execucao por RALInst.Traducao. Nao troque os curingas '%': a
  traducao precisa dos mesmos, na mesma ordem. }

interface

resourcestring
  // RALInst.IDE
  cmOrigemDisco = 'disco';
  cmOrigemManual = 'manual';
  cmOrigemRegistro = 'registro';

  // RALInst.IDE.Delphi
  cmDelphiNaoReconhecido = 'Delphi não reconhecido';
  wmDelphiDesconhecido = 'Versão do Delphi que este instalador não conhece: atualize o ' +
    'instalador.';
  wmDelphiDivideRegistro = 'Divide a configuração do registro com %s: o que for ' +
    'instalado numa aparece na outra.';
  wmDelphiForaDoRegistro = 'Instalação fora do registro: ela usa a configuração de ' +
    'HKCU%s, que é compartilhada por toda instalação desta mesma versão.';
  wmDelphiNuncaAberta = 'Esta versão nunca foi aberta nesta conta de usuário: abra a ' +
    'IDE uma vez antes de instalar nela. Compilar os pacotes funciona.';
  wmDelphiSufixo = 'O compilador desta pasta tem sufixo %s, mas %s usa %s: confira se ' +
    'a pasta não mistura versões.';

  // gerais
  cmPrefixoAviso = '  aviso: ';
  cmPrefixoErro = 'ERRO: ';
  emArquivoNaoExiste = '%s não existe';
  emCriar = 'não foi possível criar %s';
  emLimpar = 'não foi possível limpar %s';

  // RALInst.Dependencias
  cmBaixandoArquivo = '  baixando %s';
  cmExtraindoEm = '  extraindo %s em %s';
  emExtraSemRelease = '%s: o extra %s vem de um release, e a versão %s não é um release';
  emFalhaBaixar = '%s: falha ao baixar %s: %s';
  emFormatoExtra = 'formato do extra não suportado (só .zip, .tgz, .tar.gz): %s';
  emSemDownload = '%s não tem download';
  emSemReleaseExtra = '%s: não achei release estável para o extra %s';
  emSemVersaoEstavel = '%s: não achei versão estável em %s';

  // RALInst.Fontes
  cmExtraindo = '  extraindo %s';
  cmFontes = 'Fontes: %s %s em %s';
  cmFontesJaNaPasta = '  já está na pasta (commit %s)';
  cmFontesPronto = '  pronto: commit %s, %d submódulo(s)';
  cmSubmodulo = '  submódulo %s: %s/%s @ %s';
  emPastaDeOutro = 'a pasta %s já existe e não foi criada pelo instalador; ' +
    'escolha outra pasta de instalação ou esvazie esta';
  emSubmoduloForaGitHub = 'submódulo %s: endereço fora do GitHub (%s)';
  emSubmoduloNaoDeclarado = 'a versão %s não declara o submódulo %s';
  emSubmodulosCommit = 'não deu para saber o commit dos submódulos: %s';
  emSubstituirPasta = 'não foi possível substituir %s (algum arquivo aberto? ' +
    'feche as IDEs). A versão nova ficou em %s';
  wmSubmoduloNaoExiste = '  submódulo %s não existe na versão %s; fica de fora';
  wmVersaoAnteriorFicou = '  aviso: a versão anterior ficou em %s; pode ser apagada ' +
    'à mão';

  // RALInst.AutoAtualizacao
  cmHashConferido = 'hash conferido: %s';
  emAtuApagar = 'não consegui apagar %s';
  emAtuDownload = 'o download falhou: %s';
  emAtuHash = 'o hash do arquivo baixado não confere com o SHA256SUMS do release';
  emAtuNenhumaVersao = 'nenhuma versão nova escolhida (chame Verificar antes)';
  emAtuNovoNaoExiste = 'o arquivo novo não existe: %s';
  emAtuReiniciar = 'não consegui reiniciar: %s';
  emAtuRenomear = 'não consegui renomear %s';
  emAtuSemBinario = 'a versão %s não tem binário para este sistema (%s)';
  emAtuSemEscrita = 'sem permissão de escrita na pasta do instalador (%s): %s';
  emAtuSomas = 'não consegui baixar o SHA256SUMS do release: %s';
  emAtuSomasSemAsset = 'o SHA256SUMS do release não tem %s';
  emAtuTamanho = 'o arquivo baixado não tem o tamanho do release';
  emAtuTrocar = 'não consegui pôr o arquivo novo no lugar; o instalador continua o de ' +
    'antes';
  emAtuVerificar = 'não foi possível verificar: %s';
  emGitHubRespostaInesperada = 'resposta inesperada da API do GitHub';

  // RALInst.Build.Delphi
  cmBuildCodigo = 'dcc terminou com código %d';
  cmBuildDependeAusente = 'depende de %s, que não existe em %s';
  cmBuildDependeFalhou = 'depende de %s, que falhou';
  cmBuildDesignWin32 = 'pacote de design-time: só existe em Win32';
  cmBuildSemDesignide = 'exige o designide, que o Delphi não tem em %s (não há IDE ' +
    'de 64 bits)';
  cmBuildErroCompilacao = 'erro de compilação: %s';
  cmBuildFalhou = '  FALHOU: %s';
  cmBuildFonteAusente = 'fonte ausente na árvore: %s';
  cmBuildPlataformaDproj = 'o .dproj não habilita %s';
  cmBuildPulado = '  pulado: %s';
  cmBuildSemArquivo = 'arquivo não encontrado: %s';
  cmBuildSemBpl = 'compilou, mas o .bpl não apareceu em %s';
  cmBuildSemRes = 'não foi possível gerar o .res';
  cmBuildSemSubmodulo = 'submódulo não baixado: %s';
  cmBuildUnidadeAusente = 'dependência externa ausente: unidade %s não foi encontrada';
  cmColunaObservacao = 'observação';
  cmColunaPacote = 'pacote';
  cmColunaResultado = 'resultado';
  cmColunaTamanho = 'tamanho';
  cmCompilando = 'Compilando %d pacote(s) em %s (%s)';
  cmEstadoFalhou = 'FALHOU';
  cmEstadoOk = 'ok';
  cmEstadoPulado = 'pulado';
  cmGerandoRes = '  gerando %s (não vem no repositório)';
  cmSimulacao = 'simulação';
  cmSimulacaoComando = '  (simulação) %s';
  emBrcc32 = '  ERRO: brcc32 não gerou %s';
  emBuildCompiladorAusente = 'ERRO: compilador não encontrado: %s';
  emBuildPlataforma = 'ERRO: %s não compila para %s';
  emBuildSemCommonDir = 'ERRO: não sei onde gravar .bpl/.dcp desta IDE (BDSCOMMONDIR ' +
    'vazio).';
  emBuildSemCompilador = 'ERRO: %s não tem compilador de linha de comando utilizável.';
  emBuildSemFontes = 'ERRO: os fontes do RAL precisam estar em disco para compilar.';
  wmBplGrande = '  AVISO: %s tem %d KB para %d unidade(s): confira o -LU, pode ter ' +
    'linkado biblioteca de terceiro dentro do pacote';
  wmBuildSemSubmodulo = '. O zip do GitHub não traz submódulo: baixe-o antes (o ' +
    'instalador faz isso na etapa de download).';
  wmBuildUnidadeAusente = '. Instale a biblioteca do recurso, aponte a pasta dela, ou ' +
    'desmarque este pacote.';
  wmSemDcp = '  AVISO: pacote exigido sem .dcp em disco: %s (o compilador vai linkar ' +
    'as unidades dele dentro do .bpl)';

  // RALInst.Catalogo
  cmCatalogoBaixar = '  (não está na origem: baixar)';
  cmCatalogoCaminhoBusca = '      caminho de busca: %s';
  cmCatalogoDepoisDe = '      depois de: %s';
  cmCatalogoDesconhecidos = 'Pedidos que o catálogo não conhece: %s';
  cmCatalogoExternos = 'Pacotes externos exigidos: %s';
  cmCatalogoFontesAusentes = '      FONTES AUSENTES: %s';
  cmCatalogoImplicitos = '      implícitos (.dproj, fora do requires): %s';
  cmCatalogoPlano = 'Plano %s: %d pacote(s), a partir de %s';
  cmCatalogoSubmodulo = '      submódulo: %s  <- %s';
  cmCatalogoSubmodulos = 'Submódulos a baixar junto com o RAL: %s';
  cmCatalogoVariaveis = '      variáveis exigidas: %s';
  cmCatalogoVazio = '      VAZIO: o pacote não contém nenhuma unidade';
  cmCompilarInstalar = 'compilar e instalar';
  cmSoCompilar = 'só compilar';
  emCatalogoCiclo = 'Dependência circular entre pacotes %s: %s';
  emCatalogoSemNoPackage = '%s: nó <Package> não encontrado';
  emCatalogoSemPackage = '%s: cláusula "package" não encontrada';

  // RALInst.Compatibilidade
  cmManifestoDaVersao = 'ralinstaller.json de %s';
  cmManifestoEmbutido = 'manifesto embutido';
  cmMotivoComercial = '%s é comercial e não está instalado no %s';
  cmMotivoDepende = 'depende de %s, que não cabe nesta IDE';
  cmMotivoExige = 'exige %s';
  cmMotivoInstaladaNaoCompila = 'o %s instalado nesta IDE (%s) não compila aqui: %s';
  cmMotivoSemPacote = 'o %s não tem o pacote %s';
  cmMotivoSemUnidade = 'usa %s, que o %s não tem';
  emManifestoFormato = 'formato %d desconhecido (este instalador lê o 1)';
  emManifestoNaoObjeto = '%s: o manifesto deveria ser um objeto JSON';
  emManifestoRecusado = '%s: manifesto recusado: %s';
  emManifestoSemPacote = 'regra de pacote sem "pacote"';
  emManifestoSemReceita = 'regra de dependência sem "receita"';

  // RALInst.Config.Lazarus
  emSemAutoInstall = '%s: sem StaticAutoInstallPackages';

  // RALInst.GitHub
  cmBaixando = 'baixando %s/%s @ %s';
  cmVersaoEstavel = '%s  —  estável';
  cmVersaoEstavelRecente = '%s  —  estável mais recente';
  cmVersaoPreLancamento = '%s  —  pré-lançamento';
  cmVersaoRamo = '%s  —  ramo (muda a cada commit)';
  cmVersaoTag = '%s  —  tag';
  emGitHubArvore = 'resposta inesperada da árvore de %s';
  emGitHubFalhaBaixar = 'falha ao baixar %s/%s @ %s: %s';
  emGitHubInvalida = 'resposta inválida do GitHub: %s';
  emGitHubLimite = 'limite de pedidos à API do GitHub esgotado (volta em %d min; ' +
    'defina GITHUB_TOKEN para um limite maior)';
  emGitHubNaoExiste = 'não existe no GitHub: %s';
  emGitHubNaoZip = 'o GitHub não devolveu um zip para %s/%s @ %s';
  emGitHubRecusou = 'a API do GitHub recusou o pedido (%s)';
  emGitHubSemAVersao = '%s/%s não tem a versão %s';
  emGitHubSemResposta = 'sem resposta do GitHub: %s';
  emGitHubSemVersoes = 'o repositório %s/%s não tem nenhuma versão publicada';
  emGravar = 'não foi possível gravar %s';
  wmGitHubArvoreTruncada = 'a árvore de %s veio truncada pelo GitHub';
  wmGitHubCache = '%s; usando a consulta guardada de %s';

  // RALInst.HTTP
  cmErroCodigo = '%s (erro %d)';

  // RALInst.IDE.Lazarus
  cmConfigPadraoSistema = 'padrão do sistema';
  cmLazarusVersaoDesconhecida = 'Lazarus (versão desconhecida)';
  wmLazarusConfigDeOutro = 'A configuração em %s pertence a outro Lazarus (%s).';
  wmLazarusDivideConfig = 'Divide a pasta de configuração %s com %s: pacotes ' +
    'instalados numa entram na outra.';
  wmLazarusSemEscrita = 'Sem permissão de escrita na pasta do Lazarus: reconstruir a ' +
    'IDE vai exigir executar como administrador.';
  wmLazarusSemFPC = 'Compilador FPC não identificado: o lazbuild vai usar o que ' +
    'estiver configurado no ambiente.';

  // RALInst.Instalar.Delphi e RALInst.Instalar.Lazarus
  cmArquivoEm = '  %s em %s';
  cmCompilaContra = '  compila contra %s';
  cmDepComercial = '%s é comercial e não está instalado nesta IDE; instale-o (%s) e ' +
    'rode o instalador de novo';
  cmDependeDeFora = 'depende de %s, que ficou de fora';
  cmDependenciaPara = 'Dependência %s (para %s)';
  cmDepFalhou = 'a instalação de %s falhou (detalhes acima)';
  cmDepInstalandoDe = '  instalando de %s';
  cmDepJaInstalada = '  já instalada, usando a que está lá: %s';
  cmDepNaoBaixada = '%s não está instalado nesta IDE e a versão %s não foi baixada';
  cmDepSemAcoes = '%s: a receita não diz como instalar no %s';
  cmFicaDeFora = '%s fica de fora: %s';
  cmJaTinhaCaminhos = '  %s Search Path: já tinha todos os caminhos';
  cmNadaCompilou = 'Nada compilou: o registro da IDE não foi alterado.';
  cmNaoCompilouNaoRegistrado = '%s não compilou e não foi registrado; se havia uma ' +
    'versão anterior registrada, ela continua lá.';
  cmNenhumCabeDelphi = 'Nenhum pacote escolhido cabe nesta IDE: o registro não foi ' +
    'alterado.';
  cmNenhumSobrou = 'Nenhum pacote sobrou para instalar: faltam as dependências acima.';
  cmNoLibraryPath = '%s no library path (%s)';
  cmOutraCopiaRAL = '%s: outra cópia do RAL no library path: %s';
  cmPlanoAberta = '  ATENÇÃO: a IDE está aberta agora; feche antes de instalar';
  cmPlanoBplEm = '  %s: .bpl em %s';
  cmPlanoCompilar = '  compilar, na ordem:';
  cmPlanoDepComercial = '  FALTA %s (%s): é comercial; instale-o antes (%s). Sem ele, ' +
    '%s fica de fora';
  cmPlanoDepFalta = '  FALTA %s (%s): não está instalado. Sem ele, %s fica de fora';
  cmPlanoDepInstalar = '  dependência %s %s (%s): instalar de %s';
  cmPlanoDepJaInstalada = '  dependência %s (%s): já instalada — %s';
  cmPlanoFicaDeFora = '    fica de fora: %s — %s';
  cmPlanoInstalarNaIDE = '    %s  (instalar na IDE)';
  cmPlanoLibraryPath = '  library path (%s):';
  cmPlanoNuncaAberta = '  ATENÇÃO: a IDE nunca foi aberta; abra e feche uma vez antes ' +
    'de instalar';
  cmPlanoRuntime = '    %s  (runtime)';
  cmPlanoSomenteLibraryPath = '  modo: somente library path (nenhum pacote compilado ' +
    'ou instalado)';
  cmPlanoVariavelMuda = '  ATENÇÃO: $(%s) hoje aponta para %s; os projetos que usam a ' +
    'variável passam a ver a pasta nova';
  cmPlataformaIgnorada = '%s não compila para %s; plataforma ignorada.';
  cmPrefixoAvisoRelatorio = 'AVISO: ';
  cmRecibo = 'Recibo: %s';
  cmRegistro = 'Registro: HKCU%s';
  cmRegistroAlteracoes = 'Registro: %d alteração(ões) em HKCU%s%s';
  cmSimulado = ' (simulado)';
  emDelphiAberto = 'ERRO: %s está aberto. Feche a IDE antes de instalar: ela regrava o ' +
    'registro ao fechar e a instalação se perderia.';
  emDelphiNuncaAberto = 'ERRO: %s nunca foi aberto (não há HKCU%s). Abra a IDE uma ' +
    'vez, feche, e rode o instalador de novo.';
  emDepSemDpk = 'ERRO: nenhum .dpk de %s em %s';
  emDepSemPacotesBDS = 'ERRO: %s não tem pacotes para %s (BDS %s)';
  emGravarRecibo = 'não foi possível gravar o recibo: %s';
  emInstalarSemFontes = 'ERRO: os fontes do RAL precisam estar em disco para instalar.';
  emNenhumPacote = 'ERRO: nenhum pacote escolhido.';
  emPacotesDesconhecidos = 'ERRO: pacotes que não existem nesta versão do RAL: %s';
  wmBuscaForaDoPath = '  AVISO: %s não está no library path da IDE; a compilação pode ' +
    'não achá-lo';

  // RALInst.Instalar.Lazarus
  cmConfiguracaoDevolvida = 'A configuração do Lazarus voltou ao que era antes desta ' +
    'rodada.';
  cmFicaDeForaLinha = 'fica de fora: %s';
  cmFonteAusente = 'fonte ausente: %s';
  cmForaLinha = 'fora: %s';
  cmLazarusInstalado = 'instalado';
  cmLazarusMarcado = 'marcado, IDE não reconstruída';
  cmLazarusNaoInstalado = 'não instalado (configuração devolvida ao que era)';
  cmLazarusRegistrado = 'registrado';
  cmLpkNaoExiste = '%s: %s não existe na versão baixada';
  cmNadaAInstalarLazarus = 'Nada a instalar: a configuração da IDE não foi alterada.';
  cmPacoteJaRegistrado = 'pacote %s já registrado nesta IDE';
  cmPlanoConfiguracao = '  configuração: %s';
  cmPlanoExternosAusentes = '  ATENÇÃO: não encontrei nesta IDE %s — instale antes, ou ' +
    'a reconstrução da IDE vai falhar';
  cmPlanoFicaDeForaLinha = '    fica de fora: %s';
  cmPlanoPacotes = '  pacotes, na ordem:';
  cmPlanoReconstroi = '  reconstrói a IDE no fim (lazbuild --build-ide)';
  cmPlanoSoRegistrar = '    %s  (só registrar)';
  cmSubmoduloAusente = 'submódulo ausente: %s';
  emDevolverConfiguracao = 'ERRO: não consegui devolver a configuração: %s';
  emLazarusAberto = 'ERRO: %s está aberto. Feche a IDE antes de instalar: ela regrava ' +
    'a configuração ao fechar e a instalação se perderia.';
  emLazbuildAusente = 'ERRO: lazbuild não encontrado: %s';
  emLazbuildCodigo = 'ERRO: lazbuild terminou com código %d';
  emLazbuildMarcar = 'ERRO: falha ao marcar os pacotes para instalar.';
  emLazbuildReconstruir = 'ERRO: falha ao reconstruir a IDE (o erro do compilador está ' +
    'acima).';
  emLazbuildRegistrar = 'ERRO: falha ao registrar os pacotes.';
  wmExternosAusentes = 'não encontrei nesta IDE %s; se não estiverem instalados, a ' +
    'reconstrução da IDE falha';
  wmIDENaoReconstruida = 'IDE não reconstruída: ela pede para reconstruir ao abrir.';

  // RALInst.Processo
  emExecutar = 'ERRO: não foi possível executar %s: %s';

  // RALInst.Receitas
  cmFaixaDeA = '%s de %s a %s';
  cmFaixaMax = '%s até %s';
  cmFaixaMin = '%s %s ou mais novo';
  cmFaixaSistemas = 'só em %s';
  cmRecurso = 'recurso %s';
  emAcaoDesconhecida = 'ação desconhecida "%s" em %s: o instalador não executa ação ' +
    'que não conhece';
  emAcaoDpk = 'acao dpk sem "pastas" (versao do BDS -> pasta dos .dpk)';
  emAcaoLpk = 'acao lpk sem arquivo';
  emAcaoVariavel = 'acao variavel sem nome ou valor';
  emCampoDesconhecido = 'campo desconhecido "%s" em %s';
  emDelphiDesconhecido = 'versão do Delphi desconhecida: %s';
  emExtraSemAsset = 'extra sem asset';
  emJSONInvalido = '%s: JSON inválido: %s';
  emReceitaBusca = 'busca só existe no Delphi (no Lazarus o .lpk da dependência já diz ' +
    'as pastas)';
  emReceitaItemObjeto = '%s: cada item e um objeto';
  emReceitaLista = '%s deveria ser uma lista';
  emReceitaNaoObjeto = '%s: a receita deveria ser um objeto JSON';
  emReceitaObjeto = '%s deveria ser um objeto';
  emReceitaPacotesLigados = 'pacotes-ligados só existe no Delphi (é o -LU do dcc32)';
  emReceitaRecusada = '%s: receita recusada: %s';
  emReceitaSemNome = 'receita sem nome';
  emRegraGithub = '%s: github deveria ser "dono/repositorio"';
  emRegraVersao = '%s: cada regra tem "versao" ou "incompativel", e só um dos dois';
  emSistemaDesconhecido = 'sistema desconhecido: %s (windows, linux, darwin)';
  emVerificacaoArquivo = 'verificacao: o arquivo e relativo a raiz da dependencia: %s';
  emVerificacaoExpressao = 'verificacao: expressao invalida: %s';
  emVerificacaoIncompleta = 'verificacao sem arquivo, expressao ou aviso';

  // RALInst.Recibos
  cmConfiguracaoLimpa = '  configuração %s: links e pacotes da instalação removidos';
  cmDesfazendo = 'Desfazendo %s';
  cmDesfeito = 'Desfeito.';
  cmIDENaoReconstruidaDesinstalar = '  a IDE não foi reconstruída: ela pede para ' +
    'reconstruir ao abrir';
  cmMantidoMudou = '  mantido (mudou depois da instalação): %s';
  cmNenhumaInstalacao = 'Nenhuma instalação do RAL registrada para %s';
  cmReciboDesfeito = ' [desfeito]';
  cmReciboJaDesfeito = 'Recibo já desfeito: %s';
  cmReciboPacotes = ' (%d pacote(s))';
  emApagarArquivo = '  ERRO: não consegui apagar %s (em uso?)';
  emIDEAbertaDesinstalar = 'ERRO: %s está aberto; feche a IDE antes de desinstalar';
  emReciboDelphiWindows = 'ERRO: recibo do Delphi só se desfaz no Windows';
  emReciboMaisNovo = 'ERRO: há uma instalação mais nova nesta IDE (%s); desfaça ela ' +
    'antes, ou desinstale a IDE inteira';
  emReciboSemListas = 'ERRO: recibo do Lazarus sem as listas da configuração';
  emReciboTipo = 'ERRO: tipo de recibo desconhecido: %s';
  emReconstrucaoFalhou = 'ERRO: a reconstrução da IDE falhou; a configuração já está ' +
    'sem os pacotes, e a IDE pede para reconstruir ao abrir';

  // RALInst.Registro.Delphi
  cmRegistroApagado = '  apagado: %s';
  cmRegistroJaRegistrado = '  %s: %s já registrado';
  cmRegistroOutraPasta = '  %s -= %s  (mesmo pacote em outra pasta)';
  cmRegistroRestaurado = '  restaurado: %s';
  cmRegistroVariavel = '  variável $(%s) = %s';
  cmRegistroVariavelEra = '  variável $(%s) = %s  (era %s)';
  emRegistroAbrir = 'ERRO: não foi possível abrir %s';
  emRegistroGravar = 'ERRO: não foi possível gravar %s: %s';

  // RALInst.Rodada (e o plano da tela)
  cmBaixandoDep = '== Baixando %s';
  cmBaixandoRAL = '== Baixando o PascalRAL %s';
  cmDelphiSoWindows = 'o Delphi só existe no Windows';
  cmDesinstalacaoComErro = '%s: a desinstalação terminou com erro';
  cmDesinstalado = '%s: desinstalado';
  cmNadaAInstalar = '%s: nada a instalar';
  cmNenhumaIDEAlterada = 'Nenhuma IDE foi alterada.';
  cmNenhumPedidoExiste = '%s: nenhum dos pacotes pedidos existe no %s';
  cmPlanoBaixarDep = 'Baixar %s %s%s para %s (para %s)';
  cmPlanoBaixarRAL = 'Baixar o PascalRAL %s';
  cmPlanoCommit = ' (commit %s)';
  cmPlanoDe = ' de %s';
  cmPlanoErroDep = 'ERRO: %s — sem ela, %s fica de fora';
  cmPlanoFontesLocal = 'Fontes do RAL: %s (pasta local, nada é baixado)';
  cmPlanoNaoExistem = 'ATENÇÃO: não existem nesta versão do RAL: %s';
  cmPlanoPara = ' para %s';
  cmPlanoSubmodulos = '  com os submódulos: %s';
  cmRelatorioAvisos = '%s: %s; %d aviso(s): %s';
  cmTerminouComErro = 'terminou com erro';
  emBaixadosSemPacotes = 'os fontes baixados em %s não têm pacotes';
  emEscolhaVersaoOuPasta = 'escolha a versão do RAL ou uma pasta local';
  emLerVersao = 'não foi possível ler a versão %s: %s';
  emNenhumaIDE = 'nenhuma IDE escolhida';
  emSemPacotesNaPasta = 'nenhum pacote do RAL em %s (a pasta deve conter pkg e src)';
  emVersaoDesconhecida = 'versão desconhecida: %s';
  emVersaoSemPacotes = 'a versão %s não tem pacotes do RAL';

  // RALInst.Existente
  cmExistenteSoPaths = 'só o library path';

  // RALInst.Instalar.Delphi / Lazarus: desinstalar
  cmApagarVariavel = '  apagar a variável $(PascalRAL) (%s)';
  cmBplsFicam = '  os .bpl de uma instalação feita à mão ficam no disco';
  cmCaminhoRemovido = '  %s: tirado do library path %s';
  cmDesfazerDesinstalacao = '  para desfazer: %s';
  cmDesfazerRecibos = '  desfazer %d instalação(ões) feitas pelo instalador';
  cmDesinstaladoSemResto = '  nada mais do RAL na IDE';
  cmDesinstalarNadaAFazer = '  nenhum RAL instalado nesta IDE: nada a desinstalar';
  cmLinkRemovido = '  link tirado: %s';
  cmOutrosUsamRAL = '  continuam na IDE, e sem o RAL não carregam: %s';
  cmOutrosUsamRALAviso = 'continuam na IDE e sem o RAL não carregam: %s';
  cmPacoteRemovidoLazarus = '  tirado da IDE: %s';
  cmPlanoDesinstalar = 'Desinstalar o RAL:';
  cmPlanoIDE64 = '  IDE de 64 bits: os pacotes de design também em Win64 ' +
    '(Known Packages x64)';
  cmTirarCaminhos = '  tirar %d entrada(s) do RAL do library path';
  cmTirarDaIDE = '  tirar da IDE: %s';
  cmTirarDaIDE64 = '  tirar da IDE de 64 bits: %s';
  cmTirarLinks = '  tirar os links dos pacotes: %s';
  cmVariavelRemovida = '  variável $(PascalRAL) apagada (%s)';

  // RALInst.Tar
  emTarAbrir = 'não foi possível abrir %s';
  emTarTruncado = 'tar truncado';
  emTarTruncadoEm = 'tar truncado em %s';
  wmTarEntradaSuspeita = 'entrada suspeita no tar, ignorada: %s';

  // RALInst.Zip
  emZipExtrair = 'não foi possível extrair %s: %s';
  emZipNaoExiste = '%s não existe em %s';
  emZipNaoLido = '%s não pôde ser lido de %s';
  wmZipEntradaSuspeita = 'entrada suspeita no zip, ignorada: %s';

implementation

end.
