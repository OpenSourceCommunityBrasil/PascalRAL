unit UPadroes20;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALfpHTTPServer, RALStorageTypes, RALIndyReg, RALDigest,
  RALOAuth2, RALCORS, RALSecurity, RALPlugin;

type
  TAntigo = class
  private
    FServer: TRALfpHTTPServer;
    FDigest: TRALServerDigest;
    FOAuth: TRALServerOAuth2;
    FOpcoes: TRALCORSOptions;
    FLista: TRALClientList;
    FTentativa: TRALAuthAttempt;
    FVelho: TRALClientOAuth;
    FOld: TRALOAuth;
    FSeg: TRALSecurity;
  public
    procedure Ligar;
  end;

implementation

procedure TAntigo.Ligar;
begin
  FServer := TRALfpHTTPServer.Create(nil);
  FServer.Security.WhiteIPList.Add('127.0.0.1');
  FServer.Security.BlackIPList.Add('10.0.0.1');
  FServer.SecurityHeaders.Add('X-Frame-Options', 'DENY');
end;

end.
