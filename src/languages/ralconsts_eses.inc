
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
  emCertNotInspectable = 'El motor %s no pudo inspeccionar el certificado del servidor en esta conexión, por lo que SSL.Pins y OnValidateServerCert no pueden aplicarse. Use el motor Indy.';
  emCertPinInvalid = 'Cada línea de SSL.Pins debe terminar en el SHA-256 del certificado: 64 dígitos hexadecimales, con o sin separadores.';
  emCertPinUnsupported = 'SSL.Pins no es compatible con el motor %s en esta plataforma: no puede leer la huella digital del certificado. Use el motor Indy o decida en OnValidateServerCert.';
  emRequestCancelled = 'La solicitud fue rechazada por OnBeforeExecute: %s.';
  emConnectionClosedNoResponse = 'El servidor cerró la conexión sin responder: %s.';
  emCertRejected = 'Certificado del servidor rechazado.';
  emCertRequiresTLS = 'SSL.Required está definido, o una línea de SSL.Pins aplica a este host, por lo que se rechaza http sin cifrar: %s.';
  emHTTP2Unsupported = 'HTTPVersion es rhv2 y el motor %s no habla HTTP/2 en esta plataforma. Use el motor netHTTP, o deje HTTPVersion en rhvDefault.';
  emOkHttpAndroidOnly = 'El motor OkHttp solo tiene implementacion en Android: se registra en las demas plataformas para poder elegirlo, pero no puede enviar una peticion. Use el motor netHTTP en esta plataforma.';
  emKwikAndroidOnly = 'El motor Kwik solo tiene implementacion en Android: se registra en las demas plataformas para poder elegirlo, pero no puede enviar una peticion. Use el motor MsQuic en esta plataforma.';
  emKwikCryptoMissing = 'Esta version de Android no puede ejecutar el TLS 1.3 de Kwik, le falta: %s.';
  emKwikBridgeMissing = 'El puente de Kwik no respondio: %s. Verifique que los jars kwik, agent15, hkdf, siphash y ralkwik esten todos declarados en el proyecto.';
  emHttpSysAddUrl = 'http.sys rechazo escuchar en el puerto %d (codigo %d). El codigo 5 significa que el puerto no esta reservado para este usuario: ejecute una vez, como Administrador, "netsh http add urlacl url=http://+:%0:d/ user=<usuario>", o inicie el servidor elevado.';
  emHttpSysWindowsOnly = 'Mode es smHttpSys, servido por el kernel de Windows (http.sys), y no tiene implementación en esta plataforma. Use smThreads o smAsync, teniendo en cuenta que ninguno de los dos sirve HTTP/2.';
  emHTTP10NotRequestable = 'HTTPVersion es rhv10, y HTTP/1.0 no se puede solicitar: ningún motor expone esa opción. rhv10 solo aparece de vuelta, en una petición que el servidor recibió. Deje HTTPVersion en rhvDefault.';
  emServerListenFailed = 'El servidor %s no pudo escuchar en el puerto %d.';
  emQuicRequiresTLS = 'QUIC no tiene modo sin cifrado: defina SSL.CertificateFile y SSL.PrivateKeyFile antes de activar el servidor.';
  emQuicFileNotFound = 'QUIC: archivo no encontrado: %s.';
  emQuicLibrary = 'No se pudo cargar MsQuic: %s.';
  emQuicApiFailed = 'MsQuic %s falló: %s.';
  emQuicListenFailed = 'MsQuic no pudo escuchar en el puerto UDP %d: %s.';
  emQuicHandshakeTimeout = 'El handshake QUIC con %s:%d agotó el tiempo.';
  emQuicConnectFailed = 'La conexión QUIC con %s:%d falló: %s.';
  emQuicFrameMalformed = 'Frame QUIC malformado.';
  emQuicFrameTooLarge = 'QUIC: un cuerpo de %d bytes no cabe en un frame; el límite es de %d bytes.';
  emQuicAnswerTimeout = 'QUIC: tiempo agotado esperando la respuesta.';
  emQuicPeerAborted = 'QUIC: el otro extremo abortó el stream.';
  emQuicPlatformUnsupported = 'El motor MsQuic solo traduce los códigos de estado de Windows y Linux; todavía no funciona en esta plataforma.';
  emCompressInvalidFormat = 'Formato de compressión no és válido.';
  emCompressLibFilesError = 'No se encontraron los archivos binarios de biblioteca de las clases: %s.';
  emCompressFailed = 'El compresor %s ha fallado.';
  emContentCheckError = 'Error de comprobación de contenido.';
  emCryptEmptyKey = 'Se debe proporcionar la clave.';
  emCryptInvalidLength = 'Los datos cifrados tienen un tamaño inválido: necesitan IV y bloques enteros.';
  emCryptInvalidPadding = 'No se pudo descifrar: clave incorrecta o contenido alterado.';
  emCryptInvalidMAC = 'Los datos cifrados fallaron la verificación de integridad: clave incorrecta o contenido alterado.';
  emHMACEmptyText = 'Se debe proporcionar lo texto.';
  emDBConnectionUndefined = 'Conexión no establecida.';
  emDBInvalidTableName = 'Nombre de tabla inválido: solo letras, dígitos, _, $ y . son aceptados.';
  emDBSQLRejected = 'SQL rechazado por OnValidateSQL.';
  emStreamSizeBeyondEnd = 'El stream binario anuncia más bytes de los que contiene.';
  emStreamReadOnly = 'Este stream es de solo lectura: es una vista sobre memoria que pertenece a otro.';
  emStreamReleased = 'Esta parte del stream ya fue liberada: se lee una vez, del inicio al final.';
  emDecompressLimit = 'Los datos descomprimidos exceden RALMaxDecompressedSize.';
  emParamFileNameEmpty = 'No queda nombre de archivo para guardar el parámetro.';
  emDBDriverMissing = 'No se encontró el controlador de conexión.';
  emDBEmptyBody = 'El cuerpo está vacío.';
  emDBLinkMissing = 'Falta la propiedad DBLink.';
  emDBPoolTimeout = 'Se agotó el tiempo de espera por una conexión libre a la base de datos.';
  emDBRowsAffected = 'ApplyUpdates: una instrucción afectó %d registros en lugar de 1 - el registro fue modificado o eliminado por otro usuario, o el criterio de actualización no identifica exactamente una fila.';
  emDBStatusNoMessage = 'El servidor respondió HTTP %d y no envió ningún mensaje.';
  emDBUpdateSQLMissing = 'SQL para UpdateTable/UpdateSQL no puede estar vacío.';
  emInvalidFormat = 'Formato no és válido.';
  emInvalidJSONFormat = 'Formato JSON no és válido.';
  emQueryVersionError = 'Versión de estructura no és válida.';
  emRouteAlreadyExists = 'La ruta ya existe.';
  emStorageClassNotFound = 'Classe del Storage no fue encontrada.';
  emStorageInvalidBinary = 'Formato binario no és válido.';
  emStorageLinkNotFound = 'Lo Storage %s no ha declarado en "uses".';
  emStorageNotFound = 'No se encontró TRALStorageLink.';

  emEngineNotFound = 'Motor %s no encontrado: la unit que lo registra no está en las uses.';
  emDBFieldLoad = 'No se pudo cargar el campo %s: %s';
  emHTTPDateInvalid = '"%s" no es una fecha HTTP.';
  emMultipartNoPart = 'Cuerpo multipart mal formado: ninguna parte completa delimitada por su boundary.';
  emBase64Invalid = 'Texto base64 no válido.';
  emDecimalInvalid = '"%s" no es un número decimal.';
  emISO8601Invalid = '"%s" no es una fecha ISO 8601.';
  emLibraryNotLoaded = 'La biblioteca no fue cargada.';
  emOpenSSLCallFailed = 'OpenSSL falló en %s.';
  emOpenSSLNotLoaded = 'OpenSSL no está cargado.';
  emRandomBytesFailed = 'El sistema no pudo generar bytes aleatorios.';

  // server certificate (TRALCertInfo.Error)
  wmCertMsQuicStatus = 'MsQuic no validó el certificado: %s (flags 0x%x).';
  wmCertNotValidatedByEngine = 'El motor no validó el certificado.';
  wmCertNotValidatedByPlatform = 'La plataforma no validó el certificado.';
  wmCertOpenSSLVerify = 'OpenSSL no validó el certificado (error %d).';
  wmCertUnavailable = 'El certificado no está disponible en este socket handler.';

  // jwt messages (sent as error_description in WWW-Authenticate)
  wmJWTExpired = 'El token expiró';
  wmJWTInvalid = 'La firma o el formato del token no es válido';
  wmJWTNotYetValid = 'El token aún no es válido';
  wmJWTRefused = 'El token fue rechazado por el servidor';
  emJWTNoSecretKey = 'JWT: la SignSecretKey está vacía, así que el servidor no firma ni acepta tokens';

  // jws messages (RS/ES tokens and JWK)
  emJWSInvalidKey = 'No se pudo leer la clave.';
  emJWSNoPrivateKey = 'Firmar exige una clave privada: SignKey está vacía o es solo pública.';
  emJWSOpenSSL = 'Los tokens RS y ES exigen OpenSSL 1.1.1 o posterior, que no se pudo cargar.';
  emJWSUnsupportedKey = 'Solo se admiten claves RSA y claves EC en P-256 o P-384.';

  // certificate messages (RALBigInt, RALRSA, RALX509, RALSelfSigned)
  emBigNumInvalidHex = 'Número hexadecimal no válido: %s.';
  emBigNumNegative = 'El resultado sería negativo, y el número no tiene signo.';
  emBigNumDivByZero = 'División por cero.';
  emBigNumNoInverse = 'El número no tiene inverso modular.';
  emBigNumPrimeSize = 'Un primo de %d bits es demasiado pequeño.';
  emASN1Negative = 'No se esperaba un INTEGER negativo aquí.';
  emASN1InvalidOID = 'Identificador de objeto no válido: %s.';
  emASN1Truncated = 'Los datos DER terminan antes que el elemento.';
  emASN1Unsupported = 'Forma DER no admitida: etiqueta mayor que 30 o longitud mayor que 2 GB.';
  emASN1UnexpectedTag = 'Se esperaba la etiqueta DER %d, se encontró %d en la posición %d.';
  emASN1InvalidTime = 'Fecha DER no válida: %s.';
  emASN1Trailing = 'Hay bytes después del final del elemento DER.';
  emRSAInvalidKey = 'Clave RSA no válida.';
  emRSANoPrimes = 'La clave privada RSA no trae sus primos.';
  emRSAKeySize = 'Clave RSA de %d bits no admitida: 1024 o más, par.';
  emRSAExponent = 'Exponente público RSA no válido: %d.';
  emRSANotRSA = 'La clave no es RSA.';
  emRSAEncryptedKey = 'La clave privada está protegida por contraseña, y RAL no descifra claves: guárdela sin contraseña.';
  emRSANoKeyInPEM = 'No se encontró ninguna clave RSA en el texto PEM.';
  emRSANotPrivate = 'Firmar exige la clave privada.';
  emRSADigestSize = 'El resumen a firmar debe ser un SHA-256 (32 bytes).';
  emRSASignFault = 'La firma RSA no se verificó: el cálculo falló.';
  emX509NoCertificate = 'No se encontró ningún certificado en el texto PEM.';
  emX509Invalid = 'Certificado X.509 no válido.';
  emX509InvalidDetail = 'Certificado X.509 no válido: %s';
  emX509InvalidAddress = 'Dirección IP no válida para el certificado: %s.';
  emX509NoKey = 'El certificado necesita una clave privada para ser firmado.';
  emX509IssuerKeyMismatch = 'La clave del emisor no pertenece al certificado del emisor.';
  emX509InvalidValidity = 'El certificado debe terminar después de empezar.';
  emX509AESKey = 'AES-256 exige una clave de 32 bytes y un IV de 16.';
  emX509KeyMismatch = 'La clave privada no pertenece al certificado.';
  emHttpSysCallFailed = 'http.sys: %s falló (%d): %s';
  emHttpSysNoCertificate = 'http.sys: no hay certificado para vincular al puerto.';
  emHttpSysImportFailed = 'http.sys: no se pudo importar el PFX en el conjunto de claves de la máquina: %s. Se necesitan derechos de administrador, como para netsh.';
  emHttpSysNoKey = 'http.sys: el PFX no tiene un certificado con clave privada.';
  emHttpSysStoreFailed = 'http.sys: no se pudo escribir el certificado en LocalMachine\My: %s. Se necesitan derechos de administrador, como para netsh.';
  emSynopseNoTls = 'mORMot2 no tiene capa TLS en este build: en POSIX, agregue mormot.lib.openssl11 al proyecto.';
  emSelfSignedWriteFailed = 'No se pudo guardar el certificado en %s: %s';
  emSelfSignedNoCertificate = 'Todavía no hay certificado: llame a Generate o LoadOrGenerate, o inicie el servidor.';

  // oauth2 messages (error_description and client errors)
  emOAuth2Fetch = 'No se pudo leer %s: %s';
  emOAuth2LoopbackTimeout = 'Ninguna respuesta de autorización llegó a tiempo.';
  emOAuth2NoToken = 'El servidor de autorización respondió sin access_token.';
  emOAuth2StateMismatch = 'El state de la respuesta de autorización no es el enviado.';
  emOAuth2TokenFailed = 'El servidor de autorización rechazó la solicitud de token: %s';
  wmOAuth2AccessDenied = 'La autorización fue denegada';
  wmOAuth2InvalidClient = 'La autenticación del cliente falló';
  wmOAuth2InvalidGrant = 'La concesión es inválida, vencida o revocada';
  wmOAuth2InvalidRequest = 'Falta un parámetro o está mal formado';
  wmOAuth2InvalidScope = 'El alcance solicitado no está permitido';
  wmOAuth2PKCE = 'El code_verifier no coincide con el code_challenge';
  wmOAuth2RedirectURI = 'El redirect_uri no es el de la autorización';
  wmOAuth2Revoked = 'El token fue revocado';
  wmOAuth2Scope = 'El token no tiene el alcance que exige esta ruta';
  wmOAuth2UnsupportedGrant = 'El tipo de concesión no es compatible';

  // sagui error messages
  emSaguiLibraryLoadError = 'Error al intentar cargar libsagui.';
  emSaguiServerCreateError = 'Error al intentar crear el servidor.';
  emSaguiServerUnsupportedTLS = 'Esta versión de TLS no es compatible con la lib.';

  // ralserver error messages
  wmIPv6notImplemented = 'IPv6 no está implementado en este motor.';

  // ralserver page codes
  SLangHTTP = 'es-ES';
  SServerOnline = 'Servidor activo';
  SWordVersion = 'Versión';
  SWordEngine = 'Motor';
  SError400 = 'SolicitudIncorrecta';
  SError400Page = 'El servidor informa que no le gustan los parámetros de entrada.';
  SError401 = 'No autorizado';
  SError401Page = 'El servidor informa que no te conoce.';
  SError403 = 'Prohibido';
  SError403Page = 'El servidor informa que no quiere que accedas.';
  SError404 = 'No encontrado';
  SError404Page = 'El servidor informa que la página que estás solicitando no existe en esta realidad.';
  SError406 = 'No aceptable';
  SError406Page = 'El servidor informa que no puede responder en ninguna codificación que usted acepte';
  SError415 = 'Tipo de medio no soportado';
  SError415Page = 'El servidor informa que no sabe lo que estás preguntando.';
  SError429 = 'Demasiadas solicitudes';
  SError429Page = 'El servidor informa que estás pidiendo demasiado a la vez, inténtalo de nuevo más tarde.';
  SError500 = 'Error interno del servidor';
  SError500Page = 'El servidor hizo algo que no debería.';
  SError501 = 'No implementado';
  SError501Page = 'El servidor informa que estás buscando algo que no existe.';
  SError503 = 'Servicio no disponible';
  SError503Page = 'El servidor informa que está descansando y no quiere trabajar ahora, intente más tarde.';

  // RALDBDescriptions
  cmDBApplyUpdDescription = 'Registra los cambios del cliente HTTP.';
  cmDBExecSQLDescription = 'Ejecuta un SQL por el cliente HTTP.';
  cmDBGetFieldsDescription = 'Enumera todos los campos de la tabla.';
  cmDBGetSQLFieldsDescription = 'Enumera todos los campos SQL.';
  cmDBGetTablesDescription = 'Enumera todas las tablas de la base de datos.';
  cmDBOpenSQLDescription = 'Abre una consulta SQL por el cliente HTTP.';
  cmDBParamSchemaDescription = 'Esquema de base de datos.';
  cmDBParamSystemDescription = 'Incluye tablas del sistema.';
  cmDBBinary = 'Formato Binario.';
  cmDBSQL = 'SQL';
  cmDBTableName = 'Nombre de la tabla.';

  // wizard messages
  emWizardDirEmpty = 'El directorio debe completarse.';
  emWizardDirNotFound = 'El directorio no existe.';
