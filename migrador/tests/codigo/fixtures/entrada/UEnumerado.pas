unit UEnumerado;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALTypes, RALfpHTTPServer;

type
  TModo = (tmUm, tmDois, tmTres);

  TEnumerado = class
  private
    FServer: TRALfpHTTPServer;
  public
    procedure Ligar(AModo: TModo);
  end;

implementation

procedure TEnumerado.Ligar(AModo: TModo);
begin
  FServer := TRALfpHTTPServer.Create(nil);
  case AModo of
    tmUm: FServer.JSONBodyToParams := True;
    tmDois: FServer.Port := 8081;
  else
    FServer.Port := 8080;
  end;
  FServer.Active := True;
end;

end.
