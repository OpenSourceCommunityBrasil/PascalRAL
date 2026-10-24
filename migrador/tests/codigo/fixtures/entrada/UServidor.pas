unit UServidor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALTypes, RALCompress, RALfpHTTPServer;

type
  TServidor = class
  private
    FServer: TRALfpHTTPServer;
  public
    procedure Ligar(AModo: integer; const AChave: string);
  end;

implementation

procedure TServidor.Ligar(AModo: integer; const AChave: string);
begin
  FServer := TRALfpHTTPServer.Create(nil);
  // FServer.CompressType := ctDeflate; in a comment stays
  case AModo of
    1: FServer.CompressType := ctGZip;
    2: FServer.CompressType := ctDeflate;
  else
    FServer.CompressType := ctNone;
  end;
  FServer.CORSOptions.AllowOrigin := '*';
  FServer.CORSOptions.MaxAge := 3600;
  FServer.CriptoOptions.CriptType := crAES128;
  FServer.CriptoOptions.Key := AChave;
  FServer.MaxRequestSize := 1024;
  FServer.JSONBodyToParams := True;
  FServer.Port := 8080;
  FServer.Active := True;
end;

end.
