unit RALCGIRegister;

{$mode objfpc}{$H+}

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

end.

