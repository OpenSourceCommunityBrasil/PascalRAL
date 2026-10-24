unit UAvisos10;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, RALResponse, RALAuthentication, RALDBBase;

type
  TMeuBanco = class(TRALDBBase)
  public
    class function DatabaseName: string; override;
  end;

  TTela = class
  private
    FJWT: TRALServerJWTAuth;
    FBotao: TButton;
    procedure Config;
    procedure Responder(AResp: TRALResponse);
  end;

implementation

procedure TTela.Config;
begin
  FJWT.OnGetToken := @TokenSolto;
  FJWT.OnValidate := @ValidarSolto;
  FBotao.OnGetToken := @TokenSolto;
end;

procedure TTela.Responder(AResp: TRALResponse);
var
  vTexto: string;
begin
  // AResp.AddCookie('a', 'b') in a comment is not a call
  vTexto := 'AResp.AddCookie in a string';
  AResp.AddCookie('sessao', '123');
end;

end.
