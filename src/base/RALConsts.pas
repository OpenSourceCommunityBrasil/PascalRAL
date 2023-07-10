unit RALConsts;

interface

uses
  Classes, SysUtils;

Type
  TRALMethod = (amGET, amPOST, amPUT, amPATCH, amDELETE, amOPTION, amALL);
  TRALMethods = set of TRALMethod;
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

implementation

end.
