/// Registers the service container in the IDE.
unit RALContainerReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALServiceContainer;

/// Registers TRALServiceContainer on the palette.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALServiceContainer]);
end;

{$IFDEF FPC}
initialization
{$I ralhostingcontainerpackage.lrs}
{$ENDIF}

end.
