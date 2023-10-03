unit uRoutes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALServer, RALRequest, RALResponse, RALMIMETypes, RALTypes, RALUrlCoder, RALParams;

type

  { TRoutes }

  TRoutes = class
  private
    procedure PingRoute(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
    procedure FileRoute(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
    procedure TextRoute(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
  public
    procedure CreateRoutes(AServer: TRALServer);
  end;

var
  Routes: TRoutes;

implementation

{ TRoutes }

procedure TRoutes.PingRoute(Sender: TObject; ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  AResponse.Answer(200, 'pong', rctTEXTPLAIN);
end;

procedure TRoutes.FileRoute(Sender: TObject; ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  I: integer;
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

    amGET:
      AResponse.Params.AddFile('', '.\test.pdf');
  end;

end;

procedure TRoutes.TextRoute(Sender: TObject; ARequest: TRALRequest;
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

procedure TRoutes.CreateRoutes(AServer: TRALServer);
begin
  AServer.CreateRoute('ping', @PingRoute);
  AServer.CreateRoute('text', @TextRoute);
  AServer.CreateRoute('file', @FileRoute);
end;

end.
