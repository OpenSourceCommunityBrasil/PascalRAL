
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
  emCompressInvalidFormat = 'Invalid compression Format.';
  emCompressLibFilesError = 'The library files of the following classes were not found: %s.';
  emContentCheckError = 'Content check error.';
  emCryptEmptyKey = 'Key must be provided.';
  emHMACEmptyText = 'Input text must be provided.';
  emDBConnectionUndefined = 'Connection not set.';
  emDBDriverMissing = 'Connection driver not found.';
  emDBEmptyBody = 'Body is empty.';
  emDBLinkMissing = 'DBLink Property missing.';
  emDBUpdateSQLMissing = 'SQL for UpdateTable/UpdateSQL cannot be empty.';
  emInvalidFormat = 'Invalid Format.';
  emInvalidJSONFormat = 'Invalid JSON Format.';
  emQueryVersionError = 'Invalid structure version.';
  emRouteAlreadyExists = 'Route already exists.';
  emStorageClassNotFound = 'Storage class not found';
  emStorageInvalidBinary = 'Invalid Binary Format.';
  emStorageLinkNotFound = 'Storage %s not declared in uses';
  emStorageNotFound = 'No TRALStorageLink found.';

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
  SError415 = 'Unsuported Media Type';
  SError415Page = 'The server informs that it doesn''t know what you''re asking';
  SError500 = 'Internal Server Error';
  SError500Page = 'The server made something that it shouldn''t';
  SError501 = 'Not Implemented';
  SError501Page = 'The server informs that it doesn''t exist';
  SError503 = 'Service Unavailable';
  SError503Page = 'The server informs that it''s resting now and doesn''t want to work. You should try later';

  // RALDBDescriptions
  cmDBApplyUpdDescription = 'Apply Update from a client http';
  cmDBExecSQLDescription = 'Execute a SQL from a client http';
  cmDBGetFieldsDescription = 'List all fields in a table';
  cmDBGetSQLFieldsDescription = 'List all fields from a SQL';
  cmDBGetTablesDescription = 'List all tables in a database';
  cmDBOpenSQLDescription = 'Open a SQL from a client http';
  cmDBParamSchemaDescription = 'Schema of database';
  cmDBParamSystemDescription = 'Include system tables';
  cmDBBinary = 'Binary Format';
  cmDBSQL = 'SQL';
  cmDBTableName = 'Table name';
