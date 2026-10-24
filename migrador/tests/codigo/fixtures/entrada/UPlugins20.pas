unit UPlugins20;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALTypes, RALfpHTTPServer;

type
  TServidorSeguro = class
  private
    FServer: TRALfpHTTPServer;
  public
    procedure Ligar(AModo: integer);
  end;

implementation

procedure TServidorSeguro.Ligar(AModo: integer);
begin
  FServer := TRALfpHTTPServer.Create(nil);
  FServer.Security.Options := [rsoBruteForceProtection, rsoFloodProtection,
    rsoPathTransvBlackList];
  FServer.Security.BruteForce.MaxTry := 5;
  FServer.Security.BruteForce.ExpirationTime := 60000;
  FServer.Security.FloodTimeInterval := 100;
  case AModo of
    1: FServer.CriptoOptions.CriptType := crAES128;
    2: FServer.CriptoOptions.CriptType := crAES256;
  else
    FServer.CriptoOptions.CriptType := crNone;
  end;
  FServer.Port := 8080;
end;

end.
