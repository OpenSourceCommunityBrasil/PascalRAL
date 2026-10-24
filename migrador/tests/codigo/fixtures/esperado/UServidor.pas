unit UServidor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALTypes, RALCompress, RALfpHTTPServer, RALContent,
  RALCORS;

type
  TServidor = class
  private
    FServer: TRALfpHTTPServer;
    FCompress: TRALCompressPlugin;
    FCripto: TRALCriptoPlugin;
    FLimits: TRALLimitsPlugin;
    FJSONBody: TRALJSONBodyPlugin;
    FCORS: TRALCORSPlugin;
  public
    procedure Ligar(AModo: integer; const AChave: string);
  end;

implementation

procedure TServidor.Ligar(AModo: integer; const AChave: string);
begin
  FServer := TRALfpHTTPServer.Create(nil);
  FCompress := TRALCompressPlugin.Create(nil);
  FCompress.Server := FServer;
  FCripto := TRALCriptoPlugin.Create(nil);
  FCripto.Server := FServer;
  FLimits := TRALLimitsPlugin.Create(nil);
  FLimits.Server := FServer;
  FJSONBody := TRALJSONBodyPlugin.Create(nil);
  FJSONBody.Server := FServer;
  FCORS := TRALCORSPlugin.Create(nil);
  FCORS.Server := FServer;
  // FServer.CompressType := ctDeflate; in a comment stays
  case AModo of
    1: FCompress.CompressType := ctGZip;
    2: FCompress.CompressType := ctDeflate;
  else
    FCompress.CompressType := ctNone;
  end;
  FCORS.Options.AllowOrigin := '*';
  FCORS.Options.MaxAge := 3600;
  FCripto.Key := AChave;
  FLimits.MaxRequestSize := 1024;
  FServer.Port := 8080;
  FServer.Active := True;
end;

end.
