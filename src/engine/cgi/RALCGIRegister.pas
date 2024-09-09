unit RALCGIRegister;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes, SysUtils,
  RALCGIServer;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALCGIServer]);
end;

{$IFDEF FPC}
initialization
{$I cgiRAL.lrs}
{$ENDIF}

end.

