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

uses
  RALTypes;

procedure TTela.Cliente;
begin
  FCli.Get('/a');
  FCli.Get('/b', Retorno);
  FCli.Get('/c', @Retorno, ebMultiThread);
  FCli.Post('/d', nil, ebMultiThread);
  FCli.Put('/e', @Retorno, ebMultiThread);
end;

end.
