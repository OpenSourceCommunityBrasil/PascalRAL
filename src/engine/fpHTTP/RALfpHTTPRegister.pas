unit RALfpHTTPRegister;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALfpHTTPServer, RALfpHTTPClient;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALfpHTTPServer]);
  RegisterComponents('RAL - Client', [TRALfpHTTPClient]);
end;

//{$IFDEF FPC}
initialization
//{$I IndyRAL.lrs}
//{$ENDIF}

end.
