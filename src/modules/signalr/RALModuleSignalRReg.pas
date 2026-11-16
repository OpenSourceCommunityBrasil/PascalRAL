/// Registers the SignalR components in the IDE.
unit RALModuleSignalRReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALSignalR;

/// Registers the components of the package.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALSignalRModule]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulesignalrpackage.lrs}
{$ENDIF}

end.
