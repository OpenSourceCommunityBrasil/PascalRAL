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
  RAL400Page = '<!DOCTYPE html>'
             + '<html lang="en-us">'
             + '<head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>400 - BadRequest</h1>'
             + '<p>The server informs that it doesn''t like the input params</p>'
             + '</body></html>';
  RAL401Page = '<!DOCTYPE html>'
             + '<html lang="en-us">'
             + '<head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>401 - Unauthorized</h1>'
             + '<p>The server informs that it doesn''t know you</p>'
             + '</body></html>';
  RAL403Page = '<!DOCTYPE html>'
             + '<html lang="en-us">'
             + '<head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>403 - Forbidden</h1>'
             + '<p>The server informs that it doesn''t want you to access'
             + ' this page</p>'
             + '</body></html>';
  RAL404Page = '<!DOCTYPE html>'
             + '<html lang="en-us">'
             + '<head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>404 - Not Found</h1>'
             + '<p>The server informs that the page you''re requesting doesn''t'
             + ' exist in this reality</p>'
             + '</body></html>';
  RAL415Page = '<!DOCTYPE html>'
             + '<html lang="en-us">'
             + '<head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>415 - Unsuported Media Type</h1>'
             + '<p>The server informs that it doesn''t know what you''re asking</p>'
             + '</body></html>';
  RAL500Page = '<!DOCTYPE html>'
             + '<html lang="en-us">'
             + '<head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>500 - Internal Server Error</h1>'
             + '<p>The server made something that it shouldn''t</p>'
             + '</body></html>';
  RAL501Page = '<!DOCTYPE html>'
             + '<html lang="en-us">'
             + '<head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>501 - Not Implemented</h1>'
             + '<p>The server informs that it doesn''t exist</p>'
             + '</body></html>';
  RAL503Page = '<!DOCTYPE html>'
             + '<html lang="en-us">'
             + '<head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>503 - Service Unavailable</h1>'
             + '<p>The server informs that it doesn''t want to work now and you'
             + ' should try later</p>'
             + '</body></html>';

  SupportedEncriptKind = 'aes128cbc_pkcs7, aes192cbc_pkcs7, aes256cbc_pkcs7';
  MultipartLineLength = 500;
  DEFAULTBUFFERSTREAMSIZE = 52428800;
  DEFAULTDECODERBUFFERSIZE = 65536;
  HTTPLineBreak = #13#10;

resourcestring
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
  emDBLinkMissing = 'DBLink Property missing.';
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

implementation

end.
