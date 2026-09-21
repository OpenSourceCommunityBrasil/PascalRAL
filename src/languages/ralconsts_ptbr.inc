
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
  emCertNotInspectable = 'O engine %s não conseguiu inspecionar o certificado do servidor nesta conexão, então SSL.Pins e OnValidateServerCert não podem ser aplicados. Use o engine Indy.';
  emCertPinInvalid = 'Toda linha de SSL.Pins deve terminar no SHA-256 do certificado: 64 dígitos hexadecimais, com ou sem separadores.';
  emCertPinUnsupported = 'SSL.Pins não é suportado pelo engine %s nesta plataforma: ele não consegue ler a impressão digital do certificado. Use o engine Indy ou decida no OnValidateServerCert.';
  emRequestCancelled = 'A requisição foi recusada pelo OnBeforeExecute: %s.';
  emCertRejected = 'Certificado do servidor recusado.';
  emCertRequiresTLS = 'SSL.Required está definido, ou uma linha de SSL.Pins vale para este host, então http puro é recusado: %s.';
  emHTTP2Unsupported = 'HTTPVersion está em rhv2 e o engine %s não fala HTTP/2 nesta plataforma. Use o engine netHTTP, ou deixe HTTPVersion em rhvDefault.';
  emOkHttpAndroidOnly = 'O engine OkHttp só tem implementação no Android: ele é registrado nas outras plataformas para poder ser escolhido, mas não consegue enviar requisição. Use o engine netHTTP nesta plataforma.';
  emKwikAndroidOnly = 'O engine Kwik só tem implementação no Android: ele é registrado nas outras plataformas para poder ser escolhido, mas não consegue enviar requisição. Use o engine MsQuic nesta plataforma.';
  emKwikCryptoMissing = 'Esta versão do Android não consegue rodar o TLS 1.3 do Kwik, falta: %s.';
  emKwikBridgeMissing = 'A ponte do Kwik não respondeu: %s. Confira se os jars kwik, agent15, hkdf, siphash e ralkwik estão todos declarados no projeto.';
  emHttpSysAddUrl = 'O http.sys recusou escutar na porta %d (código %d). Código 5 significa que a porta não está reservada para este usuário: rode uma vez, como Administrador, "netsh http add urlacl url=http://+:%0:d/ user=<usuário>", ou suba o servidor elevado.';
  emHttpSysWindowsOnly = 'Mode está em smHttpSys, que é servido pelo kernel do Windows (http.sys) e não tem implementação nesta plataforma. Use smThreads ou smAsync, lembrando que nenhum dos dois serve HTTP/2.';
  emHTTP10NotRequestable = 'HTTPVersion está em rhv10, e HTTP/1.0 não pode ser solicitado: nenhum engine tem essa opção. O rhv10 só aparece na volta, numa requisição que o servidor recebeu. Deixe HTTPVersion em rhvDefault.';
  emServerListenFailed = 'O servidor %s não conseguiu escutar na porta %d.';
  emQuicRequiresTLS = 'QUIC não tem modo sem criptografia: defina SSL.CertificateFile e SSL.PrivateKeyFile antes de ativar o servidor.';
  emQuicFileNotFound = 'QUIC: arquivo não encontrado: %s.';
  emQuicLibrary = 'Não foi possível carregar o MsQuic: %s.';
  emQuicApiFailed = 'MsQuic %s falhou: %s.';
  emQuicListenFailed = 'O MsQuic não conseguiu escutar na porta UDP %d: %s.';
  emQuicHandshakeTimeout = 'O handshake QUIC com %s:%d estourou o tempo.';
  emQuicConnectFailed = 'A conexão QUIC com %s:%d falhou: %s.';
  emQuicFrameMalformed = 'Frame QUIC malformado.';
  emQuicAnswerTimeout = 'QUIC: tempo esgotado esperando a resposta.';
  emQuicPeerAborted = 'QUIC: o outro lado abortou o stream.';
  emQuicPlatformUnsupported = 'O engine MsQuic só traduz os códigos de status do Windows e do Linux; ainda não roda nesta plataforma.';
  emCompressInvalidFormat = 'Formato de compressão inválido.';
  emCompressLibFilesError = 'Os arquivos binários das seguintes classes não foram encontrados: %s.';
  emContentCheckError = 'Erro de verificação de conteúdo.';
  emCryptEmptyKey = 'A chave deve ser fornecida.';
  emCryptInvalidLength = 'Dados cifrados com tamanho inválido: precisam de IV e blocos inteiros.';
  emCryptInvalidPadding = 'Não foi possível decifrar: chave errada ou conteúdo alterado.';
  emCryptInvalidMAC = 'Dados cifrados falharam na verificação de integridade: chave errada ou conteúdo alterado.';
  emHMACEmptyText = 'Texto de entrada não pode ser vazio.';
  emDBConnectionUndefined = 'Conexão não definida.';
  emDBInvalidTableName = 'Nome de tabela inválido: só letras, dígitos, _, $ e . são aceitos.';
  emDBSQLRejected = 'SQL recusado pelo OnValidateSQL.';
  emStreamSizeBeyondEnd = 'O stream binário anuncia mais bytes do que contém.';
  emDecompressLimit = 'Os dados descomprimidos excedem RALMaxDecompressedSize.';
  emParamFileNameEmpty = 'Não sobrou nome de arquivo para gravar o parâmetro.';
  emDBDriverMissing = 'Driver de conexão não encontrado.';
  emDBEmptyBody = 'O "body" está vazio.';
  emDBLinkMissing = 'Propriedade DBLink ausente.';
  emDBPoolTimeout = 'Tempo de espera por uma conexão livre com o banco esgotado.';
  emDBStatusNoMessage = 'O servidor respondeu HTTP %d e não enviou mensagem.';
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
  SError429 = 'Muitas requisições';
  SError429Page = 'O servidor informa que você está pedindo demais de uma só vez, tente novamente mais tarde';
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
