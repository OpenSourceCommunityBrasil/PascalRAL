unit uRoutes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALServer, RALRequest, RALResponse, RALMIMETypes;

type

  { TRoutes }

  TRoutes = class
  private
    procedure Ping(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
    procedure Archive(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
    procedure Test(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
  public
    procedure CreateRoutes(AServer: TRALServer);
  end;

var
  Routes: TRoutes;

implementation

{ TRoutes }

procedure TRoutes.Ping(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(200, 'pong', rctTEXTPLAIN);
end;

procedure TRoutes.Archive(Sender: TObject; ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  AResponse.Params.AddFile('', '.\test.pdf');
end;

procedure TRoutes.Test(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
begin
  case ARequest.Method of
    amGET, amDELETE:
      AResponse.Answer(200, 'teste de texto com acentuação', rctTEXTPLAIN);
    amPOST, amPUT, amPATCH:
      AResponse.Answer(201, 'teste de texto com acentuação', rctTEXTPLAIN);
  end;
end;

procedure TRoutes.CreateRoutes(AServer: TRALServer);
begin
  AServer.CreateRoute('ping', @Ping);
  AServer.CreateRoute('test', @Test);
  AServer.CreateRoute('file', @Archive);
end;

end.
