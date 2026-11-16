/// Registers the feature flags plugin in the IDE.
unit RALFeatureFlagsReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALFeatureFlags;

/// Registers TRALFeatureFlags on the palette.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Plugins', [TRALFeatureFlags]);
end;

{$IFDEF FPC}
initialization
{$I ralpluginfeatureflagspackage.lrs}
{$ENDIF}

end.
