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
  filestr: TFileStream;
begin
  case ARequest.Method of
    amPOST, amPUT, amPATCH:
      for I := 0 to pred(ARequest.Params.Body.Count) do
      begin
        filestr := TFileStream.Create(ExtractFileDir(ParamStr(0)) +
          TRALParam(ARequest.Params.Body.Items[I]).AsFile.FileName, fmOpenWrite);
        filestr.Free;
      end;
  end;
end;

procedure TRotas.PingRoute(Sender: TObject; ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  AResponse.Answer(200, 'pong', rctTEXTPLAIN);
end;

end.
