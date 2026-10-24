unit USagui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALSaguiServer, RALIndyServer, RALClient, RALParams;

type
  TPrincipal = class
  private
    FSagui: TRALSaguiServer;
    FIndy: TRALIndyServer;
    FCli: TRALClient;
    procedure Ligar(AOutro: TObject);
    procedure Usar;
  end;

implementation

procedure TPrincipal.Ligar(AOutro: TObject);
begin
  FSagui := TRALSaguiServer.Create(nil);
  FSagui.ConnectionLimit := 200;
  AOutro.ConnectionLimit := 5;
  Desconhecido.ConnectionLimit := 7;
  // FSagui.ConnectionLimit := 1 in a comment stays
  FIndy := TRALIndyServer.Create(nil);
  FIndy.CORSOptions.AllowOrigin := '*';
end;

procedure TPrincipal.Usar;
var
  vLista: TStringList;
  vPar: TRALParams;
  vModulo: TRALWebModule;
begin
  FCli.Get('http://x', nil);
  vLista.Get(0);
  vPar.GetKind(1);
  vModulo.Domain := 'api';
  Result := vPar.Body;
end;

end.
