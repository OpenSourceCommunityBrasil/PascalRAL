/// Texts the pages of the migrator window write in code, kept in one place for
/// translation.
unit RALMig.Tela.Mensagens;

{$mode ObjFPC}{$H+}

{ Mesmos prefixos de RALMig.Mensagens (em = erro, wm = aviso, cm = texto comum).
  O texto original e o portugues; as traducoes ficam em
  languages/ralmigrador.<idioma>.po, embutidas no executavel. Nao troque os
  curingas '%': a traducao precisa dos mesmos, na mesma ordem. }

interface

resourcestring
  // RALMig.Tela.Principal
  cmTituloJanela = 'Migrador do RAL';
  cmVersaoMigrador = 'Migrador %s';
  cmTemaClaro = 'Claro';
  cmTemaEscuro = 'Escuro';
  cmAvancar = 'Próximo >';
  cmVoltar = '< Anterior';
  cmAplicar = 'Aplicar';
  cmConferir = 'Conferir >';
  cmConcluir = 'Concluir';

  // RALMig.Tela.Modelo
  cmSim = 'Sim';
  cmNao = 'Não';

  // RALMig.Tela.Idioma
  cmIdiomaTitulo = 'Idioma';
  cmIdiomaSub = 'Escolha o idioma do migrador.';

  // RALMig.Tela.Projeto
  cmProjetoTitulo = 'Projeto';
  cmProjetoSub = 'Escolha a pasta (ou o arquivo de projeto) que será migrada.';
  cmProjetoCaminho = 'Pasta ou arquivo de projeto:';
  cmEscolherPasta = 'Pasta...';
  cmEscolherArquivo = 'Arquivo...';
  cmEscolherPastaTitulo = 'Pasta do projeto';
  cmFiltroProjetos = 'Projetos Delphi e Lazarus';
  cmFiltroTodos = 'Todos os arquivos';
  cmProjetosAchados = 'Projetos encontrados: %d';
  cmSemProjetos = 'Nenhum projeto ou unit Pascal foi encontrado neste caminho.';
  cmForaTitulo = 'Fora da pasta (marque o que deve entrar na migração):';
  cmForaProjeto = 'projeto: %s';
  cmForaUnit = 'units: %s';
  cmCopiaAnterior = 'Esta pasta tem %d migração(ões) anterior(es) com cópia de segurança.';
  cmDesfazerAnterior = 'Desfazer a última migração';
  cmDesfazerTitulo = 'Desfazer a migração';
  cmDesfazerPergunta = 'Os arquivos voltam ao que eram antes da última migração e os ' +
    'arquivos que ela criou são apagados. Continuar?';

  // RALMig.Tela.Versoes
  cmVersoesTitulo = 'Versões';
  cmVersoesSub = 'De que versão do RAL o projeto vem e para qual ele vai.';
  cmVersaoDe = 'Versão atual do projeto:';
  cmVersaoPara = 'Migrar para:';
  cmEvidencias = 'Evidências:';
  cmForcarDe = 'Aplicar a partir da versão escolhida, mesmo que o projeto pareça já ' +
    'estar numa mais nova';
  cmNaoDetectada = 'Nenhuma pista da versão foi encontrada: escolha a versão de origem.';
  cmConfAlta = 'alta';
  cmConfMedia = 'média';
  cmConfBaixa = 'baixa';
  cmDetectada = 'Versão detectada: %s (confiança: %s)';
  cmDeInformado = '(a origem escolhida é outra)';
  emEscolhaVersoes = 'Escolha a versão de origem e a de destino.';
  emDestinoMaisVelho = 'O destino (RAL %s) é mais velho que a origem (RAL %s): o ' +
    'migrador não volta versões.';
  emMesmaVersao = 'A origem e o destino são a mesma versão: não há o que migrar.';

  // RALMig.Tela.Versoes (formulários binários)
  cmBinTitulo = 'Formulários binários';
  cmBinSub = 'Alguns formulários estão salvos no formato binário.';
  cmBinExplicacao = 'O migrador só edita formulários em texto. Estes formulários podem ' +
    'ser convertidos para texto agora, ou ficar de fora da migração.';
  cmBinConverter = 'Converter para texto (recomendado)';
  cmBinDeixar = 'Deixar de fora (estes formulários não serão migrados)';
  cmBinNota = 'O formulário convertido tem o mesmo conteúdo, só muda a forma de ' +
    'gravar; a cópia de segurança guarda o binário original.';

  // RALMig.Tela.Previa
  cmPreviaTitulo = 'Prévia';
  cmPreviaSub = 'Nada foi gravado ainda. Confira o que vai mudar.';
  cmIgnorarErros = 'Ignorar os erros e aplicar assim mesmo';
  cmItemFazer = 'O que fazer: %s';
  cmItemWiki = 'Wiki: %s';
  cmItemLocal = '%s (linha %d)';
  cmPreviaSemConjunto = 'A migração não pôde ser simulada: veja os erros.';
  cmPreviaErros = 'A simulação teve %d erro(s): corrija-os, ou marque para ignorar.';
  cmArquivoNovo = '%s (novo)';
  cmAbaArquivos = 'Arquivos (%d)';
  cmAbaManual = 'Precisa de você (%d)';
  cmAbaComportamento = 'Comportamento (%d)';
  cmAbaErros = 'Erros (%d)';
  cmNenhumAlterado = 'Nenhum arquivo precisa mudar.';

  // RALMig.Tela.Aplicar
  cmAplicarTitulo = 'Aplicar';
  cmAplicarSub = 'A migração só grava no disco quando você confirmar.';
  cmAplicarResumo = '%d arquivo(s) serão gravados em %s';
  cmAplicarNada = 'Nenhum arquivo muda: só o relatório será gravado.';
  cmFazerCopia = 'Fazer cópia de segurança antes de gravar (recomendado)';
  cmCopiaOnde = 'A cópia fica em %s (um .zip por migração) e permite desfazer.';
  cmSemCopiaAviso = 'Sem a cópia de segurança não há como desfazer a migração.';
  cmConfirmarTitulo = 'Aplicar a migração';
  cmConfirmarCom = 'Gravar %d arquivo(s) em %s? A cópia de segurança é feita antes.';
  cmConfirmarSem = 'Gravar %d arquivo(s) SEM cópia de segurança? Sem ela não há como ' +
    'desfazer.';
  emTelaGravar = 'Não foi possível gravar: %s';

  // RALMig.Tela.Aplicar (relatório)
  cmRelTitulo = 'Relatório';
  cmRelSub = 'O que foi feito e o que ainda precisa de você.';
  cmRelNaoAplicada = 'A migração não está aplicada.';
  cmRelMd = 'Relatório: %s';
  cmRelHtml = 'Relatório (HTML): %s';
  cmRelMantido = 'Nada foi alterado; o relatório da migração anterior foi mantido: %s';
  cmRelCopia = 'Cópia de segurança: %s';
  cmRelSemCopia = 'Sem cópia de segurança: não há como desfazer.';
  cmAbrirRelatorio = 'Abrir o relatório (HTML)';
  cmDesfazer = 'Desfazer a migração';
  cmRelNota = 'A próxima etapa é opcional: compilar o projeto migrado contra o RAL de ' +
    'destino. Pode fechar a janela se não quiser.';

  // RALMig.Tela.Conferir
  cmConfTitulo = 'Conferir compilando';
  cmConfSub = 'Opcional: compila o projeto migrado numa pasta temporária.';
  cmConfDica = 'Nada é registrado na IDE nem é gravado no projeto: a compilação ' +
    'usa uma cópia dele e os fontes do RAL de destino.';
  cmConfIDEs = 'IDEs com o RAL %s instalado pelo instalador:';
  cmConfNenhuma = 'Nenhuma IDE com o RAL %s nos recibos do instalador. Escolha uma IDE ' +
    'e a pasta do RAL de destino.';
  cmConfOutraIDE = 'Outra IDE...';
  cmConfProjeto = 'Projeto a compilar:';
  cmCompilar = 'Compilar';
  cmCompilando = 'Compilando...';
  cmCompilou = 'Compilou sem erros.';
  cmNaoCompilou = 'Não compilou. O primeiro erro de cada arquivo:';
  cmEscolherRaizIDE = 'Pasta de instalação da IDE';
  cmEscolherRaizRAL = 'Pasta do RAL de destino (a que tem src e pkg)';
  emConfSemIDE = 'Escolha uma IDE na lista.';
  emConfSemProjeto = 'Esta IDE não tem projeto correspondente (Delphi: .dpr ou .dproj; ' +
    'Lazarus: .lpi).';
  emIDEInvalida = 'Esta pasta não é uma IDE, ou a pasta do RAL não tem src.';

  // RALMig.Tela.Estado
  emTelaSemCopia = 'Não há cópia de segurança em %s: não há como desfazer.';
  cmTelaDesfeito = 'Migração desfeita com %s.';
  emTelaRelatorio = 'Não foi possível gravar o relatório: %s';

implementation

end.
