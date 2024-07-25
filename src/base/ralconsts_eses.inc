
{ @abstract constant internal messages
  am = alert messages
  em = error messages
  wm = warning messages
  these are strings that are present through the internal code of RAL, stored here
  for localization purposes, allowing the user to customise their side of the component

  Do NOT replace wildcards starting with '%' or your package will throw errors
}
  // general error messages
  emContentCheckError = 'Error de comprobación de contenido.';
  emCompressLibFilesError = 'No se encontraron las clases %s - archivos de biblioteca.';
  emCompressInvalidFormat = 'Formato no válido.';
  emCryptEmptyKey = 'Se debe proporcionar la clave.';
  emDBConnectionUndefined = 'Conexión no establecida.';
  emDBDriverMissing = 'No se encontró el controlador de conexión.';
  emDBEmptyBody = 'El cuerpo está vacío.';
  emDBLinkMissing = 'Falta la propiedad DBLink.';
  emDBUpdateSQLMissing = 'SQL para UpdateTable/UpdateSQL no puede estar vacío.';
  emInvalidJSONFormat = 'Formato JSON no válido.';
  emQueryVersionError = 'Versión de estructura no válida.';
  emRouteAlreadyExists = 'La ruta ya existe.';
  emStorageNotFound = 'No se encontró TRALStorageLink.';
  emStorageInvalidBinary = 'Formato binario no válido.';

  // sagui error messages
  emSaguiServerCreateError = 'Error al intentar crear el servidor.';
  emSaguiServerUnsupportedTLS = 'Esta versión de TLS no es compatible con la lib.';
  emSaguiLibraryLoadError = 'Error al intentar cargar libsagui.';

  // ralserver error messages
  wmIPv6notImplemented = 'IPv6 no está implementado en este motor.';

  // ralserver page codes
  SLangHTTP = 'es-ES';
  SServerOnline = 'Servidor en línea';
  SWordVersion = 'Versión';
  SWordEngine = 'Motor';
  SError400 = 'SolicitudIncorrecta';
  SError400Page = 'El servidor informa que no le gustan los parámetros de entrada';
  SError401 = 'No autorizado';
  SError401Page = 'El servidor informa que no te conoce';
  SError403 = 'Prohibido';
  SError403Page = 'El servidor informa que no quiere que accedas';
  SError404 = 'No encontrado';
  SError404Page = 'El servidor informa que la página que estás solicitando no existe en esta realidad';
  SError415 = 'Tipo de medio no soportado';
  SError415Page = 'El servidor informa que no sabe lo que estás preguntando';
  SError500 = 'Error interno del servidor';
  SError500Page = 'El servidor hizo algo que no debería';
  SError501 = 'No implementado';
  SError501Page = 'El servidor informa que no existe';
  SError503 = 'Servicio no disponible';
  SError503Page = 'El servidor informa que no quiere trabajar ahora y que debe intentarlo más tarde';
