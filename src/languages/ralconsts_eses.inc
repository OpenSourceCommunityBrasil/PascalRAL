
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
  emCertRejected = 'Certificado del servidor rechazado.';
  emCertRequiresTLS = 'SSL.Required está definido, o una línea de SSL.Pins aplica a este host, por lo que se rechaza http sin cifrar: %s.';
  emHTTP2Unsupported = 'HTTPVersion es rhv2 y el motor %s no habla HTTP/2 en esta plataforma. Use el motor netHTTP, o deje HTTPVersion en rhvDefault.';
  emOkHttpAndroidOnly = 'El motor OkHttp solo tiene implementacion en Android: se registra en las demas plataformas para poder elegirlo, pero no puede enviar una peticion. Use el motor netHTTP en esta plataforma.';
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
  emQuicAnswerTimeout = 'QUIC: tiempo agotado esperando la respuesta.';
  emQuicPeerAborted = 'QUIC: el otro extremo abortó el stream.';
  emQuicPlatformUnsupported = 'El motor MsQuic solo traduce los códigos de estado de Windows y Linux; todavía no funciona en esta plataforma.';
  emCompressInvalidFormat = 'Formato de compressión no és válido.';
  emCompressLibFilesError = 'No se encontraron los archivos binarios de biblioteca de las clases: %s.';
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
  emDecompressLimit = 'Los datos descomprimidos exceden RALMaxDecompressedSize.';
  emParamFileNameEmpty = 'No queda nombre de archivo para guardar el parámetro.';
  emDBDriverMissing = 'No se encontró el controlador de conexión.';
  emDBEmptyBody = 'El cuerpo está vacío.';
  emDBLinkMissing = 'Falta la propiedad DBLink.';
  emDBPoolTimeout = 'Se agotó el tiempo de espera por una conexión libre a la base de datos.';
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
