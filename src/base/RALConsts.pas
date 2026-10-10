/// Constants, version numbers and the translated messages of PascalRAL.
unit RALConsts;

interface

{$I PascalRAL.inc}

// the language files are UTF-8 with BOM: FPC includes them only under this directive
{$IFDEF FPC}
  {$CODEPAGE UTF8}
{$ENDIF}

uses
  Classes, SysUtils;

type
  /// Authentication scheme of a request.
  TRALAuthTypes = (ratNone, ratBasic, ratBearer, ratOAuth2, ratDigest);

const
  /// PascalRAL version, as text.
  RALVERSION = '1.2.0-1';
  /// Major part of the version.
  RALVERSION_MAJOR = 1;
  /// Minor part of the version.
  RALVERSION_MINOR = 2;
  /// Patch part of the version.
  RALVERSION_PATCH = 0;
  /// Version as one number: major * 10000 + minor * 100 + patch.
  RALVERSION_FULL  = RALVERSION_MAJOR * 10000
                   + RALVERSION_MINOR * 100
                   + RALVERSION_PATCH;

  /// Package name shown in the IDE.
  RALPACKAGENAME           = 'Pascal REST API Lite (RAL) Components';
  /// Short package name.
  RALPACKAGESHORT          = 'PascalRAL';
  /// Short name with the version, shown in the IDE splash and about box.
  RALPACKAGESHORTLICENSE   = 'PascalRAL v' + RALVERSION;
  /// Project site.
  RALPACKAGESITE           = 'https://github.com/OpenSourceCommunityBrasil/PascalRAL';
  /// License shown in the IDE about box.
  RALPACKAGELICENSE        = 'OpenSource';
  /// License with the version.
  RALPACKAGELICENSEVERSION = 'OpenSource - v' + RALVERSION;
  /// Name of the mORMot2 engine.
  ENGINESYNOPSE            = 'mORMot2';
  /// Name of the Indy engine.
  ENGINEINDY               = 'Indy';
  /// Name of the Sagui engine.
  ENGINESAGUI              = 'Sagui';
  /// Name of the netHTTP engine.
  ENGINENETHTTP            = 'netHttp';
  /// Name of the fpHTTP engine.
  ENGINEFPHTTP             = 'fpHttp';
  /// Name of the OkHttp engine.
  ENGINEOKHTTP             = 'OkHttp';
  /// Name of the MsQuic engine.
  ENGINEMSQUIC             = 'MsQuic';
  /// Name of the Kwik engine.
  ENGINEKWIK               = 'Kwik';

  /// Server status page; %ralengine% is replaced by the engine name.
  RALDefaultPage = '<!DOCTYPE html>'
                 + '<html lang="en-us">'
                 + '<head><title>RALServer - ' + RALVERSION + '</title>'
                 + '</head><body><h1>Server OnLine</h1>'
                 + '<h4>Version: ' + RALVERSION + '</h4>'
                 + '<h4>Engine: %ralengine%</h4>'
                 + '</body></html>';
  /// Error page for Format: language, status code, title and message.
  RALPage = '<!DOCTYPE html>'
          + '<html lang="%s">'
          + '<head><title>RALServer - ' + RALVERSION + '</title>'
          + '</head><body><h1>%d - %s</h1>'
          + '<p>%s</p></body></html>';

  /// Ciphers a client accepts in an answer, sent in Accept-Encription.
  SupportedEncriptKind = 'aes128cbc_pkcs7, aes192cbc_pkcs7, aes256cbc_pkcs7';
  /// Longest line the multipart decoder reads as a boundary or a part header.
  MultipartLineLength = 500;
  /// Largest work buffer of a transform (compression, cipher, hash, encoders).
  DEFAULTBUFFERSTREAMSIZE = 65536;
  /// Buffer of the multipart decoder.
  DEFAULTDECODERBUFFERSIZE = 65536;
  /// Largest body kept in one block; a larger one is split into DEFAULTCHUNKSIZE blocks.
  DEFAULTCHUNKABOVE = 8388608;
  /// Usable size of each block of a chunked body (1 MB minus 1 KB).
  DEFAULTCHUNKSIZE = 1047552;
  /// Work buffer the compressors read and write through.
  DEFAULTCOMPRESSBUFFERSIZE = 65536;

  /// Default TRALClient.ConnectTimeout, in milliseconds.
  DEFAULTCONNECTTIMEOUT = 30000;
  /// Most idle engines, and so open connections, a TRALClient keeps for reuse.
  RALMAXIDLEENGINES = 32;
  /// Milliseconds an idle engine stays in the TRALClient pool; 0 keeps it forever.
  RALENGINEIDLETIMEOUT = 300000;
  /// Milliseconds a thread's own TRALClient.Request may stay unused before it is dropped.
  RALTHREADREQUESTTIMEOUT = 1800000;
  /// Port of a server when none is set, and of a QUIC BaseURL without one.
  DEFAULTSERVERPORT = 8000;
  /// ALPN of the QUIC engines; client and server must offer the same value.
  RALQUICALPN = 'ralq1';
  /// Milliseconds a QUIC connection may stay idle before it is closed.
  RALQUICIDLETIMEOUT = 30000;
  /// Default TRALClient.RequestTimeout, in milliseconds.
  DEFAULTREQUESTTIMEOUT = 10000;
  /// Default number of consecutive redirects a client follows.
  DEFAULTMAXREDIRECTS = 3;
  /// Default TRALWebModule.SessionTimeout: 30 minutes, in milliseconds.
  DEFAULTWEBSESSIONTIMEOUT = 1800000;
  /// Smallest TRALClient.KeepAliveInterval WinHTTP accepts, in milliseconds.
  MINKEEPALIVEMS = 5000;
  /// Attempts to obtain a token in the JWT and OAuth2 client authenticators.
  RALMAXTOKENTRIES = 4;
  /// Line break of the HTTP protocol (CR LF).
  HTTPLineBreak = #13#10;
  /// HTTP status 200 OK.
  HTTP_OK                  = 200;
  /// HTTP status 201 Created.
  HTTP_Created             = 201;
  /// HTTP status 204 No Content.
  HTTP_NoContent           = 204;
  /// HTTP status 206 Partial Content.
  HTTP_PartialContent      = 206;
  /// HTTP status 301 Moved Permanently.
  HTTP_Moved               = 301;
  /// HTTP status 302 Found.
  HTTP_Found               = 302;
  /// HTTP status 304 Not Modified.
  HTTP_NotModified         = 304;
  /// HTTP status 400 Bad Request.
  HTTP_BadRequest          = 400;
  /// HTTP status 401 Unauthorized.
  HTTP_Unauthorized        = 401;
  /// HTTP status 403 Forbidden.
  HTTP_Forbidden           = 403;
  /// HTTP status 404 Not Found.
  HTTP_NotFound            = 404;
  /// HTTP status 405 Method Not Allowed.
  HTTP_MethodNotAllowed    = 405;
  /// HTTP status 406 Not Acceptable.
  HTTP_NotAcceptable       = 406;
  /// HTTP status 408 Request Timeout.
  HTTP_RequestTimeout      = 408;
  /// HTTP status 413 Content Too Large.
  HTTP_RequestEntityTooLarge = 413;
  /// HTTP status 415 Unsupported Media Type.
  HTTP_UnsupportedMedia    = 415;
  /// HTTP status 416 Range Not Satisfiable.
  HTTP_RangeNotSatisfiable = 416;
  /// HTTP status 429 Too Many Requests.
  HTTP_TooManyRequests     = 429;
  /// HTTP status 500 Internal Server Error.
  HTTP_InternalError       = 500;
  /// HTTP status 501 Not Implemented.
  HTTP_NotImplemented      = 501;
  /// HTTP status 502 Bad Gateway.
  HTTP_BadGateway          = 502;
  /// HTTP status 503 Service Unavailable.
  HTTP_ServiceUnavailable  = 503;
  /// HTTP status 505 HTTP Version Not Supported.
  HTTP_VersionNotSupported = 505;

resourcestring
  {$IF DEFINED(LANG_PTBR)}
    {$I ..\languages\ralconsts_ptbr.inc}
  {$ELSEIF DEFINED(LANG_ESES)}
    {$I ..\languages\ralconsts_eses.inc}
  {$ELSE}
    {$I ..\languages\ralconsts_enus.inc}
  {$IFEND}

implementation

end.
