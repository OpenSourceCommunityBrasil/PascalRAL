/// Registers the response cache plugin in the IDE.
unit RALPluginCacheReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALResponseCache;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Plugins', [TRALResponseCachePlugin]);
end;

{$IFDEF FPC}
initialization
{$I ralplugincachepackage.lrs}
{$ENDIF}

end.
