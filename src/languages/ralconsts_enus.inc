
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
  emCertNotInspectable = 'The %s engine could not inspect the server certificate on this connection, so SSL.Pins and OnValidateServerCert cannot be applied. Use the Indy engine.';
  emCertPinInvalid = 'Every SSL.Pins line must end in the SHA-256 of the certificate: 64 hex digits, with or without separators.';
  emCertPinUnsupported = 'SSL.Pins is not supported by the %s engine on this platform: it cannot read the certificate fingerprint. Use the Indy engine, or decide in OnValidateServerCert.';
  emRequestCancelled = 'The request was refused by OnBeforeExecute: %s.';
  emCertRejected = 'Server certificate refused.';
  emCertRequiresTLS = 'SSL.Required is set, or a line of SSL.Pins applies to this host, so plain http is refused: %s.';
  emHTTP2Unsupported = 'HTTPVersion is rhv2, and the %s engine cannot speak HTTP/2 on this platform. Use the netHTTP engine, or leave HTTPVersion at rhvDefault.';
  emOkHttpAndroidOnly = 'The OkHttp engine only has an implementation on Android: it is registered elsewhere so that it can be chosen, but it cannot send a request. Use the netHTTP engine on this platform.';
  emKwikAndroidOnly = 'The Kwik engine only has an implementation on Android: it is registered elsewhere so that it can be chosen, but it cannot send a request. Use the MsQuic engine on this platform.';
  emKwikCryptoMissing = 'This Android version cannot run the TLS 1.3 of Kwik, it has no: %s.';
  emKwikBridgeMissing = 'The Kwik bridge did not answer: %s. Check that kwik, agent15, hkdf, siphash and ralkwik jars are all declared in the project.';
  emHttpSysAddUrl = 'http.sys refused to listen on port %d (code %d). Code 5 means the port is not reserved for this user: run "netsh http add urlacl url=http://+:%0:d/ user=<user>" once, as Administrator, or start the server elevated.';
  emHttpSysWindowsOnly = 'Mode is smHttpSys, which is served by the Windows kernel (http.sys) and has no implementation on this platform. Use smThreads or smAsync - bearing in mind that neither of them serves HTTP/2.';
  emHTTP10NotRequestable = 'HTTPVersion is rhv10, and HTTP/1.0 cannot be asked for: no engine exposes a switch for it. rhv10 only ever comes back, on a request a server received. Leave HTTPVersion at rhvDefault.';
  emServerListenFailed = 'The %s server could not start listening on port %d.';
  emQuicRequiresTLS = 'QUIC has no unencrypted mode: set SSL.CertificateFile and SSL.PrivateKeyFile before activating the server.';
  emQuicFileNotFound = 'QUIC: file not found: %s.';
  emQuicLibrary = 'MsQuic could not be loaded: %s.';
  emQuicApiFailed = 'MsQuic %s failed: %s.';
  emQuicListenFailed = 'MsQuic could not listen on UDP port %d: %s.';
  emQuicHandshakeTimeout = 'QUIC handshake with %s:%d timed out.';
  emQuicConnectFailed = 'QUIC connection to %s:%d failed: %s.';
  emQuicFrameMalformed = 'Malformed QUIC frame.';
  emQuicAnswerTimeout = 'QUIC: timed out waiting for the answer.';
  emQuicPeerAborted = 'QUIC: the peer aborted the stream.';
  emQuicPlatformUnsupported = 'The MsQuic engine only translates the status codes of Windows and Linux; it cannot run on this platform yet.';
  emCompressInvalidFormat = 'Invalid compression Format.';
  emCompressLibFilesError = 'The library files of the following classes were not found: %s.';
  emContentCheckError = 'Content check error.';
  emCryptEmptyKey = 'Key must be provided.';
  emCryptInvalidLength = 'Encrypted data has an invalid length: it needs an IV and whole blocks.';
  emCryptInvalidPadding = 'Encrypted data could not be decoded: wrong key or altered content.';
  emCryptInvalidMAC = 'Encrypted data failed integrity check: wrong key or altered content.';
  emDBConnectionUndefined = 'Connection not set.';
  emDBInvalidTableName = 'Invalid table name: only letters, digits, _, $ and . are accepted.';
  emDBSQLRejected = 'SQL rejected by OnValidateSQL.';
  emStreamSizeBeyondEnd = 'Binary stream announces more bytes than it holds.';
  emDecompressLimit = 'Decompressed data exceeds RALMaxDecompressedSize.';
  emParamFileNameEmpty = 'No file name left to save the param to.';
  emDBDriverMissing = 'Connection driver not found.';
  emDBEmptyBody = 'Body is empty.';
  emDBLinkMissing = 'DBLink Property missing.';
  emDBPoolTimeout = 'Timed out waiting for a free database connection.';
  emDBStatusNoMessage = 'The server answered HTTP %d and sent no message.';
  emDBUpdateSQLMissing = 'SQL for UpdateTable/UpdateSQL cannot be empty.';
  emHMACEmptyText = 'Input text must be provided.';
  emInvalidFormat = 'Invalid Format.';
  emInvalidJSONFormat = 'Invalid JSON Format.';
  emQueryVersionError = 'Invalid structure version.';
  emRouteAlreadyExists = 'Route already exists.';
  emStorageClassNotFound = 'Storage class not found';
  emStorageInvalidBinary = 'Invalid Binary Format.';
  emStorageLinkNotFound = 'Storage %s not declared in uses';
  emStorageNotFound = 'No TRALStorageLink found.';

  emEngineNotFound = 'Engine %s not found: the unit that registers it is not in the uses.';
  emDBFieldLoad = 'Could not load the field %s: %s';
  emHTTPDateInvalidMonth = 'Invalid month in the HTTP date: %s.';
  emISO8601Invalid = '"%s" is not an ISO 8601 date.';
  emLibraryNotLoaded = 'The library was not loaded.';
  emOpenSSLCallFailed = 'OpenSSL failed in %s.';
  emOpenSSLNotLoaded = 'OpenSSL is not loaded.';
  emRandomBytesFailed = 'The system could not generate random bytes.';

  // server certificate (TRALCertInfo.Error)
  wmCertMsQuicStatus = 'MsQuic did not validate the certificate: %s (flags 0x%x).';
  wmCertNotValidatedByEngine = 'The engine did not validate the certificate.';
  wmCertNotValidatedByPlatform = 'The platform did not validate the certificate.';
  wmCertOpenSSLVerify = 'OpenSSL did not validate the certificate (error %d).';
  wmCertUnavailable = 'The certificate is not available on this socket handler.';

  // jwt messages (sent as error_description in WWW-Authenticate)
  wmJWTExpired = 'The token expired';
  wmJWTInvalid = 'The token signature or format is invalid';
  wmJWTNotYetValid = 'The token is not valid yet';
  wmJWTRefused = 'The token was refused by the server';

  // sagui error messages
  emSaguiLibraryLoadError = 'Error while trying to load libsagui.';
  emSaguiServerCreateError = 'Error while attempting to create the server.';
  emSaguiServerUnsupportedTLS = 'This version of TLS is not supported by the lib.';

  // ralserver error messages
  wmIPv6notImplemented = 'IPv6 is not implemented in this engine.';

  // ralserver page codes
  SLangHTTP = 'en-US';
  SServerOnline = 'Server Online';
  SWordVersion = 'Version';
  SWordEngine = 'Engine';
  SError400 = 'BadRequest';
  SError400Page = 'The server informs that it doesn''t like the input params';
  SError401 = 'Unauthorized';
  SError401Page = 'The server informs that it doesn''t know you';
  SError403 = 'Forbidden';
  SError403Page = 'The server informs that it doesn''t want you to access';
  SError404 = 'Not Found';
  SError404Page = 'The server informs that the page you''re requesting doesn''t exist in this reality';
  SError406 = 'Not Acceptable';
  SError406Page = 'The server informs that it cannot answer in any encoding you accept';
  SError415 = 'Unsuported Media Type';
  SError415Page = 'The server informs that it doesn''t know what you''re asking';
  SError429 = 'Too Many Requests';
  SError429Page = 'The server informs that you''re asking too much at once. You should try again later';
  SError500 = 'Internal Server Error';
  SError500Page = 'The server made something that it shouldn''t';
  SError501 = 'Not Implemented';
  SError501Page = 'The server informs that it doesn''t exist';
  SError503 = 'Service Unavailable';
  SError503Page = 'The server informs that it''s resting now and doesn''t want to work. You should try later';

  // RALDBDescriptions
  cmDBApplyUpdDescription = 'Apply Update from a client http';
  cmDBBinary = 'Binary Format';
  cmDBExecSQLDescription = 'Execute a SQL from a client http';
  cmDBGetFieldsDescription = 'List all fields in a table';
  cmDBGetSQLFieldsDescription = 'List all fields from a SQL';
  cmDBGetTablesDescription = 'List all tables in a database';
  cmDBOpenSQLDescription = 'Open a SQL from a client http';
  cmDBParamSchemaDescription = 'Schema of database';
  cmDBParamSystemDescription = 'Include system tables';
  cmDBSQL = 'SQL';
  cmDBTableName = 'Table name';

  // wizard messages
  emWizardDirEmpty = 'The directory must be filled in.';
  emWizardDirNotFound = 'The directory does not exist.';
