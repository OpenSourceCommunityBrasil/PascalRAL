unit uRotas;

interface

uses
  SysUtils,
  RALServer, RALRequest, RALResponse, RALMIMETypes;

type
  TRotas = class
  private
    procedure TextRoute(Sender: TObject; ARequest: TRALRequest;
      AResponse: TRALResponse);
    procedure FileRoute(Sender: TObject; ARequest: TRALRequest;
      AResponse: TRALResponse);
    procedure PingRoute(Sender: TObject; ARequest: TRALRequest;
      AResponse: TRALResponse);
  public
    procedure CreateRoutes(AServer: TRALServer);
  end;

var
  Rotas: TRotas;

implementation

procedure TRotas.CreateRoutes(AServer: TRALServer);
begin
  AServer.CreateRoute('text', TextRoute);
  AServer.CreateRoute('file', FileRoute);
  AServer.CreateRoute('ping', PingRoute);
end;

procedure TRotas.TextRoute(Sender: TObject; ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  case ARequest.Method of
    amGET:
      AResponse.Answer(200, 'teste de texto com UTF8 e acentuação',
        rctAPPLICATIONJSON);

    amPOST, amPUT, amPATCH:
      AResponse.Answer(201, Format('os params enviados: %s',
        [ARequest.Params.AsString]), rctAPPLICATIONJSON);

    amDELETE:
      AResponse.Answer(200, 'teste de texto após deleção com UTF8 e acentuação',
        rctAPPLICATIONJSON);
  end;
end;

procedure TRotas.FileRoute(Sender: TObject; ARequest: TRALRequest;
  AResponse: TRALResponse);
begin

end;

procedure TRotas.PingRoute(Sender: TObject; ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  AResponse.Answer(200, 'pong', rctTEXTPLAIN);
end;

end.
