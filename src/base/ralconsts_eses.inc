
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
  emCompressInvalidFormat = 'Formato de compressión no és válido.';
  emCompressLibFilesError = 'No se encontraron los archivos binarios de biblioteca de las clases: %s.';
  emContentCheckError = 'Error de comprobación de contenido.';
  emCryptEmptyKey = 'Se debe proporcionar la clave.';
  emDBConnectionUndefined = 'Conexión no establecida.';
  emDBDriverMissing = 'No se encontró el controlador de conexión.';
  emDBEmptyBody = 'El cuerpo está vacío.';
  emDBLinkMissing = 'Falta la propiedad DBLink.';
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
