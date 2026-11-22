unit UChamadas12;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALClient, RALResponse;

type
  TTela = class
  private
    FCli: TRALClient;
    procedure Retorno(AResp: TRALResponse);
    procedure Cliente;
  end;

implementation

procedure TTela.Cliente;
begin
  FCli.Get('/a');
  FCli.Get('/b', Retorno);
  FCli.Get('/c', @Retorno);
  FCli.Post('/d', nil);
  FCli.Put('/e', @Retorno, ebMultiThread);
end;

end.
