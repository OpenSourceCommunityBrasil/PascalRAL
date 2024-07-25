
{ @abstract constant internal messages
  am = alert messages
  em = error messages
  wm = warning messages
  these are strings that are present through the internal code of RAL, stored here
  for localization purposes, allowing the user to customise their side of the component

  Do NOT replace wildcards starting with '%' or your package will throw errors
}
  // general error messages
  emContentCheckError = 'Erro de verificação de conteúdo.';
  emCompressLibFilesError = 'As classes %s - arquivos de biblioteca não foram encontrados.';
  emCompressInvalidFormat = 'Formato inválido.';
  emCryptEmptyKey = 'A chave deve ser fornecida.';
  emDBConnectionUndefined = 'Conexão não definida.';
  emDBDriverMissing = 'Driver de conexão não encontrado.';
  emDBEmptyBody = 'O corpo está vazio.';
  emDBLinkMissing = 'Propriedade DBLink ausente.';
  emDBUpdateSQLMissing = 'SQL para UpdateTable/UpdateSQL não pode estar vazio.';
  emInvalidJSONFormat = 'Formato JSON inválido.';
  emQueryVersionError = 'Versão de estrutura inválida.';
  emRouteAlreadyExists = 'A rota já existe.';
  emStorageNotFound = 'Nenhum TRALStorageLink encontrado.';
  emStorageInvalidBinary = 'Formato binário inválido.';

  // sagui error messages
  emSaguiServerCreateError = 'Erro ao tentar criar o servidor.';
  emSaguiServerUnsupportedTLS = 'Esta versão do TLS não é suportada pela lib.';
  emSaguiLibraryLoadError = 'Erro ao tentar carregar libsagui.';

  // ralserver error messages
  wmIPv6notImplemented = 'IPv6 não está implementado neste mecanismo.';

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
  SError501Page = 'O servidor informa que não existe';
  SError503 = 'Serviço indisponível';
  SError503Page = 'O servidor informa que não quer funcionar agora, tente mais tarde';
