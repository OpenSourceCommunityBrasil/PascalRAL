
{ @abstract constant internal messages
  am = alert messages
  em = error messages
  wm = warning messages
  these are strings that are present through the internal code of RAL, stored here
  for localization purposes, allowing the user to customise their side of the component

  Do NOT replace wildcards starting with '%' or your package will throw errors
}
  // general error messages
  emContentCheckError = 'Content check error.';
  emCompressLibFilesError = 'The classes %s - library files were not found.';
  emCompressInvalidFormat = 'Invalid Format.';
  emCryptEmptyKey = 'Key must be provided.';
  emDBConnectionUndefined = 'Connection not set.';
  emDBDriverMissing = 'Connection driver not found.';
  emDBEmptyBody = 'Body is empty.';
  emDBLinkMissing = 'DBLink Property missing.';
  emDBUpdateSQLMissing = 'SQL for UpdateTable/UpdateSQL cannot be empty.';
  emInvalidJSONFormat = 'Invalid JSON Format.';
  emQueryVersionError = 'Invalid structure version.';
  emRouteAlreadyExists = 'Route already exists.';
  emStorageNotFound = 'No TRALStorageLink found.';
  emStorageInvalidBinary = 'Invalid Binary Format.';

  // sagui error messages
  emSaguiServerCreateError = 'Error while attempting to create the server.';
  emSaguiServerUnsupportedTLS = 'This version of TLS is not supported by the lib.';
  emSaguiLibraryLoadError = 'Error while trying to load libsagui.';

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
  SError415 = 'Unsuported Media Type';
  SError415Page = 'The server informs that it doesn''t know what you''re asking';
  SError500 = 'Internal Server Error';
  SError500Page = 'The server made something that it shouldn''t';
  SError501 = 'Not Implemented';
  SError501Page = 'The server informs that it doesn''t exist';
  SError503 = 'Service Unavailable';
  SError503Page = 'The server informs that it doesn''t want to work now and you should try later';
