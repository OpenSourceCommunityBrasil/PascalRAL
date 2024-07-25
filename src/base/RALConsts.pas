/// Unit that stores all constant values, version configuration and i18n message strings
unit RALConsts;

interface

uses
  Classes, SysUtils;

type
  TRALAuthTypes = (ratNone, ratBasic, ratBearer, ratOAuth, ratOAuth2, ratDigest);

const
  // Versionamento
  RALVERSION = '0.9.8 - alpha';
  // IOTA Constants
  RALPACKAGENAME           = 'Pascal REST API Lite (RAL) Components';
  RALPACKAGESHORT          = 'PascalRAL';
  RALPACKAGESHORTLICENSE   = 'PascalRAL v' + RALVERSION;
  RALPACKAGESITE           = 'https://github.com/OpenSourceCommunityBrasil/PascalRAL';
  RALPACKAGELICENSE        = 'OpenSource';
  RALPACKAGELICENSEVERSION = 'OpenSource - v' + RALVERSION;

  // html pages
  RALDefaultPage = '<!DOCTYPE html>'
                 + '<html lang="en-us">'
                 + '<head><title>RALServer - ' + RALVERSION + '</title>'
                 + '</head><body><h1>Server OnLine</h1>'
                 + '<h4>Version: ' + RALVERSION + '</h4>'
                 + '<h4>Engine: %ralengine%</h4>'
                 + '</body></html>';
  RALPage = '<!DOCTYPE html>'
          + '<html lang="%s">'
          + '<head><title>RALServer - ' + RALVERSION + '</title>'
          + '</head><body><h1>%d - %s</h1>'
          + '<p>%s</p></body></html>';

  SupportedEncriptKind = 'aes128cbc_pkcs7, aes192cbc_pkcs7, aes256cbc_pkcs7';
  MultipartLineLength = 500;
  DEFAULTBUFFERSTREAMSIZE = 52428800;
  DEFAULTDECODERBUFFERSIZE = 65536;
  HTTPLineBreak = #13#10;

resourcestring
{
{$IF DEFINED(LANG_PTBR)}
  {$I ralconsts_prbr.inc}
{$ELSEIF DEFINED(LANG_ESES)}
  {$I ralconsts_eses.inc}
{$ELSE}
  {$I ralconsts_enus.inc}
{$IFEND}
}

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

implementation

end.
