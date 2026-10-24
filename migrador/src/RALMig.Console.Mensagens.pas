/// The texts of the command line (ralmigra). Kept apart from RALMig.Mensagens
/// because the console is the only one that uses them, but the window links this
/// unit too: the .po generator collects every resourcestring it finds in the
/// executable, so a single languages/ralmigrador.<lang>.po serves both programs.
unit RALMig.Console.Mensagens;

{$mode ObjFPC}{$H+}

interface

resourcestring
  // usage and help
  emCliUso = 'Uso: ralmigra detectar|previa|aplicar|desfazer|conferir|inventario ' +
    '<pasta> [opções]. Use ralmigra --ajuda para ver tudo.';
  emCliVerboDesconhecido = 'Comando desconhecido: %s';
  emCliSemPasta = 'Informe a pasta do projeto.';
  cmAjudaTitulo = 'ralmigra: migra projetos Delphi e Lazarus entre versões do PascalRAL.';
  cmAjudaComandos = 'Comandos:';
  cmAjudaDetectar = '  detectar <pasta>    mostra a versão do RAL que o projeto usa e as pistas';
  cmAjudaPrevia = '  previa <pasta>      mostra o que mudaria e o relatório, sem gravar nada';
  cmAjudaAplicar = '  aplicar <pasta>     grava a migração e o relatório (MIGRACAO-RAL-<de>-<para>.md e .html)';
  cmAjudaDesfazer = '  desfazer <pasta>    volta ao que havia antes, pela cópia de segurança';
  cmAjudaConferir = '  conferir <pasta>    compila o projeto migrado contra o RAL de destino';
  cmAjudaInventario = '  inventario          confere a cobertura das regras com a API de cada versão';
  cmAjudaOpcoes = 'Opções:';
  cmAjudaDe = '  --de=<versão>       versão de origem (sem ela, é detectada)';
  cmAjudaPara = '  --para=<versão>     versão de destino (sem ela, a mais nova)';
  cmAjudaIdioma = '  --idioma=<idioma>   pt-BR, en-US ou es-ES (sem ele, o do sistema)';
  cmAjudaBackup = '  --backup            aplicar: faz a cópia de segurança em _ral-migracao (sem ela não há desfazer)';
  cmAjudaConverter = '  --converter-binarios  converte formulários binários para texto em vez de deixá-los de fora';
  cmAjudaForcar = '  --forcar-de         aplica os passos desde --de mesmo se o projeto já estiver mais novo';
  cmAjudaIgnorarErros = '  --ignorar-erros     aplicar: grava mesmo se a migração teve erros';
  cmAjudaZip = '  --zip=<cópia>       desfazer: a cópia a restaurar (sem ela, a mais recente)';
  cmAjudaIDE = '  --ide=<raiz da IDE> conferir: a IDE que compila (sem ela, as dos recibos do instalador)';
  cmAjudaRal = '  --ral=<raiz do RAL> conferir: os fontes do RAL de destino (com --ide); ' +
    'nos outros comandos, lê o ralmigracao.json dele (--raiz-ral)';
  cmAjudaSaida = 'Código de saída: 0 nada a fazer à mão, 1 sobrou item manual, ' +
    '2 erro, 3 recusado (o destino é mais velho que a origem).';
  // detection
  cmCliConfAlta = 'alta';
  cmCliConfMedia = 'média';
  cmCliConfBaixa = 'baixa';
  cmCliDetectada = 'Versão detectada: %s (confiança: %s)';
  cmCliNenhuma = 'Nenhuma pista encontrada: informe a versão com --de=.';
  emCliSemVersao = 'Não foi possível detectar a versão de origem; informe --de=.';
  // preview and apply
  cmCliPreviaNada = 'Nada a mudar.';
  cmCliFora = 'Fora da pasta (não entram sozinhos): %s';
  wmCliSemApi = 'Sem a API da versão %s: o resultado não foi validado.';
  emCliGravar = 'Falha ao gravar: %s';
  emCliRelatorio = 'Não foi possível gravar o relatório: %s';
  wmCliSemCopia = 'Sem cópia de segurança (--backup): não há como desfazer.';
  cmCliGravados = '%d arquivo(s) gravado(s).';
  cmCliRelatorioMd = 'Relatório: %s';
  cmCliRelatorioHtml = 'Relatório (HTML): %s';
  cmCliRelatorioMantido = 'Nada foi alterado; o relatório da migração anterior foi mantido: %s';
  wmCliRelatorioSemRegistro = 'O relatório não foi registrado na cópia de segurança (o desfazer não o removerá): %s';
  cmCliCopia = 'Cópia de segurança: %s';
  // undo
  emCliSemCopia = 'Não há cópia de segurança em %s: não há como desfazer.';
  wmCliRelatorioMantido = 'Relatório alterado depois da migração, mantido: %s';
  cmCliDesfeito = 'Migração desfeita com %s.';
  // inventory
  cmCliInventarioPasso = '[%s] %d diferenças, %d pendentes';
  // check by compiling
  emCliConferirSemRal = 'Com --ide informe também --ral=<raiz do RAL de destino>.';
  emCliConferirIDEInvalida = 'Não é uma IDE, ou o RAL informado não tem a pasta src: %s';
  emCliConferirSemIDE = 'Nenhuma IDE com o RAL %s nos recibos do instalador; ' +
    'informe --ide=<raiz da IDE> e --ral=<raiz do RAL>.';
  emCliConferirSemProjeto = 'Nenhum projeto que %s compile em %s.';
  cmCliConferirCompilando = 'Compilando %s com %s...';
  cmCliConferirOk = 'Compilou: %s';
  cmCliConferirFalhou = 'Não compilou: %s';
  cmCliConferirResumo = '%d projeto(s) compilado(s), %d com erro.';

implementation

end.
