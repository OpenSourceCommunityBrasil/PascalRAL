unit uRotas;

interface

uses
  SysUtils,
  RALServer, RALRequest, RALResponse, RALMIMETypes, RALTypes, RALParams, RALUrlCoder;

type
  TRotas = class
  private
    procedure TextRoute(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure FileRoute(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure PingRoute(ARequest: TRALRequest; AResponse: TRALResponse);
  public
    procedure CreateRoutes(AServer: TRALServer);
  end;

var
  Rotas: TRotas;

implementation

uses
  System.Classes;

procedure TRotas.CreateRoutes(AServer: TRALServer);
begin
  AServer.CreateRoute('text', TextRoute);
  AServer.CreateRoute('file', FileRoute);
  AServer.CreateRoute('ping', PingRoute);
end;

procedure TRotas.TextRoute(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  case ARequest.Method of
    amGET:
      AResponse.Answer(200, 'teste de texto com UTF8 e acentuação', rctTEXTPLAIN);

    amPOST, amPUT, amPATCH:
      AResponse.Answer(201, Format('os params enviados: %s', [ARequest.Params.AsString]),
        rctTEXTPLAIN);

    amDELETE:
      AResponse.Answer(200, 'teste de texto após deleção com UTF8 e acentuação',
        rctTEXTPLAIN);
  end;
end;

procedure TRotas.FileRoute(ARequest: TRALRequest; AResponse: TRALResponse);
var
  I: Integer;
  vBody: TList;
  filename: string;
begin
  case ARequest.Method of
    amPOST, amPUT, amPATCH:
      begin
        filename := TRALHTTPCoder.DecodeURL(ARequest.ParamByName('filename').AsString);
        vBody := ARequest.Params.Body;
        for I := 0 to pred(vBody.Count) do
          TRALParam(vBody.Items[I]).SaveToFile(filename);
        vBody.Free;
      end;
  end;
end;

procedure TRotas.PingRoute(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(200, 'pong', rctTEXTPLAIN);
end;

end.
