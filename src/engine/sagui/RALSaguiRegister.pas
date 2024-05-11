unit RALSaguiRegister;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALSaguiServer;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALSaguiServer]);
end;

{$IFDEF FPC}
initialization
{$I SaguiRAL.lrs}
{$ENDIF}

end.
