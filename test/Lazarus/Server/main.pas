unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls,
  RALServer, RALIndyServer, RALFpHttpServer, RALSynopseServer, RALResponse,
  RALRequest, RALRoutes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    LabeledEdit1: TLabeledEdit;
    Engine: TRadioGroup;
    ListView1: TListView;
    Memo1: TMemo;
    ToggleBox1: TToggleBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ToggleBox1Click(Sender: TObject);
  private
    FServer: TRALServer;
    procedure SetupServer;
    procedure Ping(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
    procedure Archive(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
    procedure Test(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FServer) then
  begin
    if FServer.Active then
    begin
      FServer.Active := False;
      Sleep(500);
    end;
    FreeAndNil(FServer);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FServer := nil;
end;

procedure TForm1.ToggleBox1Click(Sender: TObject);
begin
  if (FServer = nil) or ((FServer <> nil) and (not FServer.Active)) then
  begin
    SetupServer;
    ToggleBox1.Caption := 'http://localhost:' + FServer.Port.ToString;
  end
  else
  begin
    ToggleBox1.Caption := 'Offline';
    FServer.Active := False;
    FreeAndNil(FServer);
  end;
end;

procedure TForm1.SetupServer;
var
  I: integer;
begin
  case Engine.ItemIndex of
    0: FServer := TRALfpHttpServer.Create(Self);
    1: FServer := TRALIndyServer.Create(Self);
    2: FServer := TRALSynopseServer.Create(Self);
  end;
  FServer.Port := StrToInt(LabeledEdit1.Text);
  FServer.CreateRoute('ping', @Ping);
  FServer.CreateRoute('test', @Test);
  FServer.CreateRoute('file', @Archive);

  ListView1.Clear;
  for I := 0 to pred(FServer.Routes.Count) do
    ListView1.Items.Add.Caption := TRALRoute(FServer.Routes.Items[I]).RouteName;

  FServer.Active := True;
end;

procedure TForm1.Ping(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.ResponseText := 'pong';
end;

procedure TForm1.Archive(Sender: TObject; ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  TMemoryStream(AResponse.ResponseStream).LoadFromFile('.\test.pdf');
end;

procedure TForm1.Test(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.ContentType := 'text/plain';
  AResponse.ResponseText := 'pong';
  case ARequest.Method of
    amGET, amDELETE:
      AResponse.RespCode := 200;
    amPOST, amPUT, amPATCH:
      AResponse.RespCode := 201;
  end;
end;

end.
