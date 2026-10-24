unit UPlugins20;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALTypes, RALfpHTTPServer, RALSecurity;

type
  TServidorSeguro = class
  private
    FServer: TRALfpHTTPServer;
    FBruteForce: TRALBruteForcePlugin;
    FFlood: TRALFloodPlugin;
    FPathTraversal: TRALPathTraversalPlugin;
  public
    procedure Ligar(AModo: integer);
  end;

implementation

procedure TServidorSeguro.Ligar(AModo: integer);
begin
  FServer := TRALfpHTTPServer.Create(nil);
  FBruteForce := TRALBruteForcePlugin.Create(nil);
  FBruteForce.Server := FServer;
  FFlood := TRALFloodPlugin.Create(nil);
  FFlood.Server := FServer;
  FPathTraversal := TRALPathTraversalPlugin.Create(nil);
  FPathTraversal.Server := FServer;
  FBruteForce.MaxTry := 5;
  FBruteForce.ExpirationTime := 60000;
  FFlood.Interval := 100;
  FServer.Port := 8080;
end;

end.
