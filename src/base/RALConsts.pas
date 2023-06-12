unit RALConsts;

interface

uses
  Classes, SysUtils;

const
  //Versionamento
  VERSION_MAJOR = 1;
  VERSION_MINOR = 0;
  VERSION_RELEASE = 0;
  RALVERSION = char(48 + VERSION_MAJOR) + '.'
             + char(48 + VERSION_MINOR) + '.'
             + char(48 + VERSION_RELEASE);

implementation

end.
