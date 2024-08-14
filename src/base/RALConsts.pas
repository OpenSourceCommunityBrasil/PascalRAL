/// Unit that stores all constant values, version configuration and i18n message strings
unit RALConsts;

interface

{$I PascalRAL.inc}

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
  // HTTP Codes
  HTTP_OK                  = 200;
  HTTP_Created             = 201;
  HTTP_NoContent           = 204;
  HTTP_Moved               = 301;
  HTTP_Found               = 302;
  HTTP_BadRequest          = 400;
  HTTP_Unauthorized        = 401;
  HTTP_Forbidden           = 403;
  HTTP_NotFound            = 404;
  HTTP_MethodNotAllowed    = 405;
  HTTP_RequestTimeout      = 408;
  HTTP_UnsupportedMedia    = 415;
  HTTP_InternalError       = 500;
  HTTP_NotImplemented      = 501;
  HTTP_BadGateway          = 502;
  HTTP_ServiceUnavailable  = 503;
  HTTP_VersionNotSupported = 505;

resourcestring
  {$IF DEFINED(LANG_PTBR)}
    {$I ralconsts_prbr.inc}
  {$ELSEIF DEFINED(LANG_ESES)}
    {$I ralconsts_eses.inc}
  {$ELSE}
    {$I ralconsts_enus.inc}
  {$IFEND}

implementation

end.
