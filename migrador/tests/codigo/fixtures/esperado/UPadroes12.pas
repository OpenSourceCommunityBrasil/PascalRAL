unit UPadroes12;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALClient, RALTypes, RALResponse, RALIndyServer, RALCGIServer,
  RALAuthentication, RALDBFireDAC;

type
  TTela = class
  private
    FCli: TRALClient;
    FMem: TRALDBFDMemTable;
    FLista: TStringList;
    FServer: TRALIndyServer;
    FAuth: TRALServerBasicAuth;
    FCGI: TRALCGIServer;
    FPronto: TRALIndyServer;
    procedure Retorno(AResp: TRALResponse);
    procedure Cliente;
    procedure Gravar;
    procedure Servidores;
  end;

implementation

procedure TTela.Cliente;
var
  vResp: TRALResponse;
  vEscopo: TRALCookieSiteScope;
begin
  FCli.Get('/a', vResp);
  FCli.Get('/b', @Retorno);
  FCli.Post('/c', @Retorno, ebMultiThread);
  FCli.Put('/d', nil, ebSingleThread);
  FCli.Delete('/e', @Retorno);
  vEscopo := cssLax;
end;

procedure TTela.Gravar;
begin
  FMem.ApplyUpdates;
  FLista.ApplyUpdates;
end;

procedure TTela.Servidores;
begin
  FServer := TRALIndyServer.Create(nil);
  FServer.CORSOptions.AllowOrigin := '*';
  FAuth := TRALServerBasicAuth.Create(nil, 'a', 'b');
  if FAuth <> nil then FCGI := TRALCGIServer.Create(nil);
  FPronto := TRALIndyServer.Create(nil);
  FPronto.CORSOptions.AllowOrigin := 'https://example.org';
end;

end.
