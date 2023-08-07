unit RALConsts;

interface

uses
  Classes, SysUtils;

type
  TRALAuthTypes = (ratNone, ratBasic, ratBearer);

const
  // Versionamento
  VERSION_MAJOR = 0;
  VERSION_MINOR = 2;
  VERSION_RELEASE = 0;
  VERSION_ADDITIVE = ' - alpha';
  RALVERSION = char(48 + VERSION_MAJOR) + '.'
             + char(48 + VERSION_MINOR) + '.'
             + char(48 + VERSION_RELEASE)
             + VERSION_ADDITIVE;

  // respostas de erro
  RALDefaultPage = '<html><head><title>RALServer - ' + RALVERSION + '</title>'
                 + '</head><body><h1>Server OnLine</h1>'
                 + '<h4>Version: ' + RALVERSION + '</h4>'
                 + '<h4>Engine: %ralengine%</h4>'
                 + '</body></html>';
  RAL400Page = '<html><head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>400 - BadRequest</h1>'
             + '<p>The server informs that it doesn''t like the input params</p>'
             + '</body></html>';
  RAL401Page = '<html><head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>401 - Unauthorized</h1>'
             + '<p>The server informs that it doesn''t know you</p>'
             + '</body></html>';
  RAL403Page = '<html><head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>403 - Forbidden</h1>'
             + '<p>The server informs that it doesn''t want you to access'
             + ' this page</p>'
             + '</body></html>';
  RAL404Page = '<html><head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>404 - Not Found</h1>'
             + '<p>The server informs that the page you''re requesting doesn''t'
             + ' exist in this reality</p>'
             + '</body></html>';
  RAL500Page = '<html><head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>500 - Internal Server Error</h1>'
             + '<p>The server made something that it shouldn''t</p>'
             + '</body></html>';
  RAL501Page = '<html><head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>501 - Not Implemented</h1>'
             + '<p>The server informs that it doesn''t exist</p>'
             + '</body></html>';
  RAL503Page = '<html><head><title>RALServer - ' + RALVERSION + '</title>'
             + '</head><body><h1>503 - Service Unavailable</h1>'
             + '<p>The server informs that it doesn''t want to work now and you'
             + ' should try later</p>'
             + '</body></html>';

  wmIPv6notImplemented = 'IPv6 not is implemented in this engine!';

implementation

end.
