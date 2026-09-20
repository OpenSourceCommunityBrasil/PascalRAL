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
  RALVERSION = '1.2.0-1';
  RALVERSION_MAJOR = 1;
  RALVERSION_MINOR = 1;
  RALVERSION_PATCH = 0;
  RALVERSION_FULL  = RALVERSION_MAJOR * 10000
                   + RALVERSION_MINOR * 100
                   + RALVERSION_PATCH;

  // IOTA Constants
  RALPACKAGENAME           = 'Pascal REST API Lite (RAL) Components';
  RALPACKAGESHORT          = 'PascalRAL';
  RALPACKAGESHORTLICENSE   = 'PascalRAL v' + RALVERSION;
  RALPACKAGESITE           = 'https://github.com/OpenSourceCommunityBrasil/PascalRAL';
  RALPACKAGELICENSE        = 'OpenSource';
  RALPACKAGELICENSEVERSION = 'OpenSource - v' + RALVERSION;
  ENGINESYNOPSE            = 'mORMot2';
  ENGINEINDY               = 'Indy';
  ENGINESAGUI              = 'Sagui';
  ENGINENETHTTP            = 'netHttp';
  ENGINEFPHTTP             = 'fpHttp';
  ENGINEOKHTTP             = 'OkHttp';
  ENGINEMSQUIC             = 'MsQuic';

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

  // Client defaults and limits.
  // The two timeouts must be written both in the constructor and in the
  // published property's "default" directive: they used to disagree (the
  // directive said 5000/30000 while the constructor set 30000/10000), and
  // "default" is what tells the streaming system not to write the property to
  // the dfm/lfm - so typing exactly 5000 in the Object Inspector produced a
  // component that ran with 30000.
  DEFAULTCONNECTTIMEOUT = 30000;
  { How many idle client engines TRALClient keeps for reuse. An engine owns a
    connection, so this is also the ceiling on connections one client holds
    open while nothing is in flight. Past it a released engine is closed
    instead of pooled, which is what the client did for EVERY engine before
    the pool existed. }
  RALMAXIDLEENGINES = 32;
  { Milliseconds an engine may sit idle in TRALClient's pool before it is
    closed instead of handed out again. Zero keeps it forever. Five minutes
    is below what a server usually allows a kept-alive connection. }
  RALENGINEIDLETIMEOUT = 300000;
  { Milliseconds a thread's own Request - TRALClient.Request is one object per
    thread - may sit untouched before the client discards it. Deliberately NOT
    the pool's idle timeout: a thread that set a header once and calls every
    ten minutes must not lose it in between. Thirty minutes. }
  RALTHREADREQUESTTIMEOUT = 1800000;
  { The port a TRALServer listens on when nobody chose one, and the port a
    client assumes for a BaseURL without one on the engines that have to know
    - the QUIC engine has no 80/443 convention to fall back on. }
  DEFAULTSERVERPORT = 8000;
  { ALPN the QUIC engine offers on both ends. A server and a client with
    different values never complete a handshake. }
  RALQUICALPN = 'ralq1';
  { Milliseconds a QUIC connection may sit idle before either end closes it.
    QUIC negotiates the smaller of the two peers' values, so the client and
    the server start from the same one. }
  RALQUICIDLETIMEOUT = 30000;
  DEFAULTREQUESTTIMEOUT = 10000;
  // Consecutive redirects a client follows. Engines used to disagree without
  // anyone choosing it: Indy 3, mORMot2 3, fpHTTP 255, netHTTP whatever
  // THTTPClient defaults to.
  DEFAULTMAXREDIRECTS = 3;
  { Floor for TRALClient.KeepAliveInterval while it is on. It comes from
    WinHTTP, which refuses WINHTTP_OPTION_HTTP2_KEEPALIVE below 5000 ms with
    ERROR_INVALID_PARAMETER. Engines with no floor of their own answer 0 to
    MinKeepAliveInterval and are not held to this one. }
  MINKEEPALIVEMS = 5000;
  // Attempts to obtain a token, in SetTokenDigest/SetTokenJWT/SetTokenOAuth1.
  RALMAXTOKENTRIES = 4;
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
  HTTP_RequestEntityTooLarge = 413;
  HTTP_UnsupportedMedia    = 415;
  HTTP_TooManyRequests     = 429;
  HTTP_InternalError       = 500;
  HTTP_NotImplemented      = 501;
  HTTP_BadGateway          = 502;
  HTTP_ServiceUnavailable  = 503;
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
