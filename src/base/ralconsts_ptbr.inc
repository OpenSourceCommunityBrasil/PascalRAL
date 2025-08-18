
{ @abstract constant internal messages
  am = alert messages: middle severity messages, might raise exception
  em = error messages: high severity messages, raises exception
  wm = warning messages: low severity messages, doesn't raise exception
  cm = common messages: usually used in descriptions or labels
  these are strings that are present through the internal code of RAL, stored here
  for localization purposes, allowing the user to customise their side of the component

  Do NOT replace wildcards starting with '%' or your package will throw errors
}
  // general error messages
  emCompressInvalidFormat = 'Formato de compressão inválido.';
  emCompressLibFilesError = 'Os arquivos binários das seguintes classes não foram encontrados: %s.';
  emContentCheckError = 'Erro de verificação de conteúdo.';
  emCryptEmptyKey = 'A chave deve ser fornecida.';
  emHMACEmptyText = 'Texto de entrada não pode ser vazio.';
  emDBConnectionUndefined = 'Conexão não definida.';
  emDBDriverMissing = 'Driver de conexão não encontrado.';
  emDBEmptyBody = 'O "body" está vazio.';
  emDBLinkMissing = 'Propriedade DBLink ausente.';
  emDBUpdateSQLMissing = 'SQL para UpdateTable/UpdateSQL não pode estar vazio.';
  emInvalidFormat = 'Formato Inválido.';
  emInvalidJSONFormat = 'Formato JSON inválido.';
  emQueryVersionError = 'Versão de estrutura inválida.';
  emRouteAlreadyExists = 'A rota já existe.';
  emStorageClassNotFound = 'Classe do Storage não encontrada.';
  emStorageInvalidBinary = 'Formato binário inválido.';
  emStorageLinkNotFound = 'Storage %s não está declarado em "uses".';
  emStorageNotFound = 'Nenhum TRALStorageLink encontrado.';

  // sagui error messages
  emSaguiLibraryLoadError = 'Erro ao tentar carregar libsagui.';
  emSaguiServerCreateError = 'Erro ao tentar criar o servidor.';
  emSaguiServerUnsupportedTLS = 'Esta versão do TLS não é suportada pela lib.';

  // ralserver error messages
  wmIPv6notImplemented = 'IPv6 não está implementado.';

  // ralserver page codes
  SLangHTTP = 'pt-BR';
  SServerOnline = 'Servidor Online';
  SWordVersion = 'Versão';
  SWordEngine = 'Motor';
  SError400 = 'Solicitação Incorreta';
  SError400Page = 'O servidor informa que não conhece os parâmetros de entrada';
  SError401 = 'Não autorizado';
  SError401Page = 'O servidor informa que não conhece você';
  SError403 = 'Proibido';
  SError403Page = 'O servidor informa que não quer que você acesse';
  SError404 = 'Não encontrado';
  SError404Page = 'O servidor informa que a página que você está solicitando não existe nesta realidade';
  SError415 = 'Tipo de mídia não suportado';
  SError415Page = 'O servidor informa que desconhece o que você está perguntando';
  SError500 = 'Erro interno do servidor';
  SError500Page = 'O servidor fez algo que não deveria';
  SError501 = 'Não implementado';
  SError501Page = 'O servidor informa que você procura algo que não existe';
  SError503 = 'Serviço indisponível';
  SError503Page = 'O servidor informa que está descansando e não quer funcionar agora, tente mais tarde';

  // RALDBDescriptions
  cmDBApplyUpdDescription = 'Grava as alterações pelo cliente HTTP.';
  cmDBExecSQLDescription = 'Executa um SQL pelo cliente HTTP.';
  cmDBGetFieldsDescription = 'Lista todos os campos da tabela.';
  cmDBGetSQLFieldsDescription = 'Lista todos os campos do SQL.';
  cmDBGetTablesDescription = 'Lista todas as tabelas do banco de dados.';
  cmDBOpenSQLDescription = 'Abre uma consulta SQL pelo cliente HTTP.';
  cmDBParamSchemaDescription = 'Schema do banco de dados.';
  cmDBParamSystemDescription = 'Inclui tabelas de sistema.';
  cmDBBinary = 'Formato Binário.';
  cmDBSQL = 'SQL';
  cmDBTableName = 'Nome da tabela.';
