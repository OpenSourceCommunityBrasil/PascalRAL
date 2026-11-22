unit UEnumerado;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALTypes, RALfpHTTPServer, RALContent;

type
  TModo = (tmUm, tmDois, tmTres);

  TEnumerado = class
  private
    FServer: TRALfpHTTPServer;
    FJSONBody: TRALJSONBodyPlugin;
  public
    procedure Ligar(AModo: TModo);
  end;

implementation

procedure TEnumerado.Ligar(AModo: TModo);
begin
  FServer := TRALfpHTTPServer.Create(nil);
  FJSONBody := TRALJSONBodyPlugin.Create(nil);
  FJSONBody.Server := FServer;
  case AModo of
    tmDois: FServer.Port := 8081;
  else
    FServer.Port := 8080;
  end;
  FServer.Active := True;
end;

end.
