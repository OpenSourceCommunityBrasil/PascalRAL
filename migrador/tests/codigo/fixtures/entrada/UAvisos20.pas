unit UAvisos20;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RALRequest, RALResponse, RALParams, RALToken, RALAuthentication;

type
  TMeuAuth = class(TRALServerJWTAuth)
  public
    function AttemptOf(ARequest: TRALRequest; AResponse: TRALResponse;
      ASafe: Boolean): TRALAuthAttempt; override;
  end;

  TUso = class
  private
    FDigest: TRALDigestParams;
    FAuth: TRALAuthentication;
    procedure Ler(AParam: TRALParam);
  end;

implementation

procedure TUso.Ler(AParam: TRALParam);
var
  vMem: TMemoryStream;
  vLista: TStringList;
begin
  vMem := TMemoryStream(AParam.Content);
  FDigest.Stale := 'true';
  vLista.Stale := 'x';
  // TMemoryStream(AParam.Content) in a comment is not a cast
end;

end.
