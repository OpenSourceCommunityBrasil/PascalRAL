/// Texts written by the screens in code, kept in one place for translation.
unit RALInst.Tela.Mensagens;

{$mode ObjFPC}{$H+}

{ Mesmos prefixos de RALInst.Mensagens (em = erro, wm = aviso, cm = texto
  comum). O texto original e o portugues; as traducoes ficam em
  languages/ralinstaller.<idioma>.po, embutidas no executavel. }

interface

resourcestring
  // RALInst.Tela.Modelo
  cmTemaClaro = 'Claro';
  cmTemaEscuro = 'Escuro';
  cmVersaoInstalador = 'Instalador versão %s';

  // RALInst.Tela.IDE
  cmDelphiSoNoWindows = 'O Delphi só existe no Windows: este instalador instala o ' +
    'PascalRAL no Lazarus.';
  cmEscolhaIDE = 'Escolha uma IDE.';

  // RALInst.Tela.ItemIDE
  cmAvisosIDE = '  (%d aviso(s))';
  cmDicaConfiguracao = 'Configuração: %s (%s)';
  cmDicaOrigem = 'Origem: %s';
  cmDicaRAL = 'RAL instalado: %s';
  cmDicaRegistro = 'Registro: HKCU%s';
  cmRALNaIDE = '  — RAL instalado';

  // RALInst.Tela.Instalacao, RALInst.Tela.Delphi e RALInst.Tela.Lazarus
  cmIDEComErro = '==== %s: terminou com erro (detalhes acima)';
  cmIDEConcluida = '==== %s: concluído';
  cmNadaAInstalarTipo = '%s: nada a instalar (os recursos escolhidos não existem no %s)';
  cmNaoSuportada = 'ERRO: instalação ainda não suportada nesta IDE: %s';
  cmNaoSuportadaPlano = '%s: instalação ainda não suportada';
  cmNenhumRecursoExiste = 'Nenhum dos recursos escolhidos existe no %s.';
  cmNenhumRecursoExistePlano = '%s: nenhum dos recursos escolhidos existe no %s';
  cmRALPorRecibos = 'pelo instalador (%d instalação(ões))';
  cmResumoComErro = '%s: terminou com erro (veja o log acima)';
  cmResumoDesinstalado = '%s: RAL desinstalado';
  cmResumoDesinstalarErro = '%s: a desinstalação terminou com erro (veja o log ' +
    'acima)';
  cmResumoInstalado = '%s: instalado';
  cmResumoSemRAL = '%s: sem RAL, nada a desinstalar';

  // RALInst.Tela.VersoesIDE
  cmAguardeBusca = 'Aguarde a busca das IDEs terminar (ou cancele a busca).';
  cmErroBusca = 'A busca das IDEs parou com erro: %s';
  cmMarqueUmaIDE = 'Marque ao menos uma IDE para instalar.';
  cmNenhumaIDENaPasta = 'Nenhuma instalação encontrada em %s';
  cmProcurandoIDEs = 'Procurando as IDEs instaladas...';

  // RALInst.Tela.Recursos
  cmAguardeCarga = 'Aguarde: a versão do PascalRAL ainda está sendo lida.';
  cmBaixandoDeMB = 'baixando... %.1f de %.1f MB';
  cmBaixandoMB = 'baixando... %.1f MB';
  cmCarregando = 'Carregando...';
  cmCatalogoAvisos = ' — %d aviso(s)';
  cmConsultandoVersoes = 'Consultando as versões no GitHub...';
  cmContinuaMarcado = '%s continua marcado: é necessário para %s.';
  cmDependenciaCircular = 'dependência circular';
  cmDependeDe = 'depende de %s, que não serve nas IDEs escolhidas';
  cmDestinoEscolha = 'Escolha a pasta onde o PascalRAL será instalado.';
  cmDestinoLocal = 'Os fontes são usados onde estão; nada é baixado.';
  cmDestinoPasta = 'O RAL fica em %s. Pasta permanente: as IDEs passam a apontar ' +
    'para ela.';
  cmEscolhaPastaFontes = 'Escolha a pasta dos fontes do PascalRAL.';
  cmEscolhaPastaInstalacao = 'Escolha a pasta de instalação.';
  cmEscolhaVersao = 'Escolha a versão do PascalRAL.';
  cmFaltaFonte = 'falta o fonte %s';
  cmFaltaSubmodulo = 'falta o submódulo %s (git submodule update --init)';
  cmFicaDeForaEm = '  [fica de fora em %s]';
  cmGrupoBase = 'Pacotes base';
  cmGrupoCompressao = 'Bibliotecas de compressão';
  cmGrupoDBWare = 'Módulos DBWare';
  cmGrupoMotores = 'Motores (engines)';
  cmGrupoOutros = 'Outros';
  cmGrupoStorage = 'Storage';
  cmGrupoSwagger = 'Swagger';
  cmLendoVersao = 'Lendo a versão %s...';
  cmInstaladoEm = '  (instalado em %s)';
  cmInstaladoNaIDE = '  (instalado)';
  cmMarqueUmRecurso = 'Marque ao menos um recurso. Sem nenhum marcado, o ' +
    'instalador desinstala o RAL das IDEs que já o têm, e nenhuma das IDEs ' +
    'marcadas tem.';
  cmNadaMarcadoDesinstala = 'Nenhum recurso marcado: o RAL será desinstalado ' +
    'destas IDEs:' + LineEnding + LineEnding + '%s' + LineEnding + 'Continuar?';
  cmNecessario = '  (necessário)';
  cmOcultosDica = 'Não aparecem na lista porque nenhuma IDE escolhida os aceita:';
  cmPastaDeOutro = 'A pasta %s já existe e não foi criada pelo instalador. Escolha ' +
    'outra pasta de instalação.';
  cmPastaFontesLocal = 'Pasta dos fontes do PascalRAL (a que contém pkg e src)';
  cmPastaInstalacao = 'Pasta de instalação (os fontes do RAL e das dependências ficam ' +
    'nela)';
  cmPastaLocalFontes = 'Pasta local com os fontes (desenvolvimento do RAL)';
  cmPastaNaoEncontrada = 'Pasta não encontrada.';
  cmPastaSemPacotes = 'Nenhum pacote do RAL nesta pasta (ela deve conter pkg e src).';
  cmRecursosNaVersao = '%d recurso(s); %d marcado(s)';
  cmRecursosOcultos = '; %d oculto(s)';
  cmSemVersoes = 'Não foi possível consultar as versões do PascalRAL: %s' + LineEnding +
    LineEnding + 'Só dá para instalar a partir de uma pasta local.';
  cmSoDoTipo = '  (só %s)';

  // RALInst.Tela.Instalar
  cmBotaoDesinstalar = 'Desinstalar';
  cmBotaoInstalar = 'Instalar';
  cmCliqueDesinstalar = 'Clique em Desinstalar para executar.';
  cmCliqueInstalar = 'Clique em Instalar para executar.';
  cmDesinstalacaoComErros = 'Desinstalação terminou com erros — veja as linhas ERRO ' +
    'acima.';
  cmDesinstalacaoConcluida = 'Desinstalação concluída.';
  cmDesinstalarPergunta = 'Remover o RAL destas IDEs:' + LineEnding + LineEnding +
    '%s' + LineEnding + 'O que o instalador fez é desfeito: o registro, o library ' +
    'path e a configuração voltam ao que eram e os .bpl gravados são apagados. O ' +
    'que foi instalado à mão sai do registro e da configuração (os .bpl ficam no ' +
    'disco), com um registro para desfazer. O Lazarus é reconstruído sem os ' +
    'pacotes; as dependências não são tocadas. Feche as IDEs antes.' + LineEnding +
    LineEnding + 'Desinstalar agora?';
  cmDesinstalarTitulo = 'Desinstalar o PascalRAL';
  cmInstalacaoComErros = 'Instalação terminou com erros — veja as linhas ERRO acima.';
  cmInstalacaoConcluida = 'Instalação concluída.';
  cmInstalarPergunta = 'As IDEs marcadas terão a configuração alterada (pacotes, ' +
    'library path e, no Delphi, o registro). Feche-as antes de continuar.' +
    LineEnding + LineEnding + 'Instalar agora?';
  cmInstalarTitulo = 'Instalar o PascalRAL';
  cmJaInstalado = 'O RAL já está nestas IDEs (instalar de novo passa por cima; ' +
    '"Desinstalar" remove):';
  cmLog = 'Log: %s';
  cmOQueSeraFeito = 'O que será feito:';
  cmPlanoTitulo = 'Plano:';

  // RALInst.Tela.Principal
  cmAbraDeNovo = 'Atualizado para a versão %s; abra o instalador de novo. (%s)';
  cmAtualizacaoNaoFeita = 'A atualização não foi feita: %s' + LineEnding +
    'O instalador continua na versão %s.';
  cmAtualizadoPara = 'O RAL Installer foi atualizado para a versão %s.';
  cmBaixarReiniciar = 'Baixar e reiniciar agora?';
  cmMaisInstalacoes = ' (e mais %d instalação(ões) antes)';
  cmNaoVerificouVersao = ' — não foi possível verificar se há versão nova';
  cmResumoTitulo = '==== Resumo';
  cmVersaoNovaDisponivel = 'A versão %s está disponível (esta é a %s).';
  cmVersaoNovaTitulo = 'Versão nova do RAL Installer';

implementation

end.
