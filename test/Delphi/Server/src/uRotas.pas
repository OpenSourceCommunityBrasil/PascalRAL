unit uRotas;

interface

uses
  SysUtils,
  RALServer, RALRequest, RALResponse, RALMIMETypes, RALTypes, RALParams;

type
  TRotas = class
  private
    procedure TextRoute(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
    procedure FileRoute(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
    procedure PingRoute(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
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

procedure TRotas.TextRoute(Sender: TObject; ARequest: TRALRequest;
  AResponse: TRALResponse);
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

procedure TRotas.FileRoute(Sender: TObject; ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  I: Integer;
  vBody: TList;
  test: string;
begin
  test := ARequest.Params.ParamByName['filename'].AsString;
  test := RALHTTPDecode(test);
  case ARequest.Method of
    amPOST, amPUT, amPATCH:
      begin
        vBody := ARequest.Params.Body;
        for I := 0 to pred(vBody.Count) do
          TRALParam(vBody.Items[I]).SaveToFile(test);
        vBody.Free;
      end;
  end;
end;

procedure TRotas.PingRoute(Sender: TObject; ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  AResponse.Answer(200, 'pong', rctTEXTPLAIN);
end;

end.
