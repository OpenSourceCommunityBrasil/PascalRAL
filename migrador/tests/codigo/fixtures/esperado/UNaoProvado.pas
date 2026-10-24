unit UNaoProvado;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALTypes, RALCompress, RALfpHTTPServer;

type
  TNao = class
  private
    FOutra: TOutraClasse;
    FServer: TRALfpHTTPServer;
  public
    procedure Config(AServer: TRALfpHTTPServer);
    procedure Mais;
  end;

implementation

procedure TNao.Config(AServer: TRALfpHTTPServer);
begin
  AServer.CompressType := ctZLib;
  AServer.CORSOptions.AllowOrigin := 'https://example.org';
end;

procedure TNao.Mais;
begin
  FOutra.Servidor.CompressType := ctGZip;
  if FOutra <> nil then FServer := TRALfpHTTPServer.Create(nil);
  FServer.CompressType := ctDeflate;
  FServer.Port := 1;
end;

end.
