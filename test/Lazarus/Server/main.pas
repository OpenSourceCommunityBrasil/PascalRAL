unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Grids, LazUTF8,
  RALServer, RALIndyServer, RALFpHttpServer, RALSynopseServer, RALResponse,
  RALRequest, RALRoutes, RALAuthentication, RALTools,
  uroutes;

type

  { TForm1 }

  TForm1 = class(TForm)
    bAdd: TButton;
    bDelete: TButton;
    cbUseSSL: TCheckBox;
    cbGET: TCheckBox;
    cbPOST: TCheckBox;
    cbPUT: TCheckBox;
    cbPATCH: TCheckBox;
    cbDELETE: TCheckBox;
    cbALL: TCheckBox;
    cbLog: TCheckBox;
    saAuthType: TComboBox;
    cbRoute: TComboBox;
    Engine: TRadioGroup;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    esslCertFile: TLabeledEdit;
    esslCertKeyFile: TLabeledEdit;
    LabeledEdit8: TLabeledEdit;
    spPort: TLabeledEdit;
    saUser: TLabeledEdit;
    saPassword: TLabeledEdit;
    saToken: TLabeledEdit;
    eRouteDescription: TLabeledEdit;
    mLog: TMemo;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel2: TPanel;
    Panel3: TPanel;
    sgRoutes: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    bStart: TToggleBox;
    procedure bAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bStartClick(Sender: TObject);
  private
    FServer: TRALServer;
    procedure SetupServer;
    procedure ClearControls(AControlName: array of TComponent);
    function GetSelectedMethods: string;
    procedure LogRequest(Sender: TObject; ARequest: TRALRequest;
      AResponse: TRALResponse);
    procedure LogResponse(Sender: TObject; ARequest: TRALRequest;
      AResponse: TRALResponse);
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
var
  leaktest: TForm;
begin
  FServer := nil;
  sgRoutes.RowCount := 1;
  sgRoutes.Cols[0].Text := 'Route';
  sgRoutes.Cols[1].Text := 'Description';
  sgRoutes.Cols[2].Text := 'SkipAuth';

  leaktest := TForm.Create(nil);
end;

procedure TForm1.bAddClick(Sender: TObject);
begin
  with sgRoutes do
  begin
    RowCount := RowCount + 1;
    Cells[0, pred(RowCount)] := cbRoute.Text;
    Cells[1, pred(RowCount)] := eRouteDescription.Text;
    Cells[2, pred(RowCount)] := GetSelectedMethods;
  end;
  ClearControls([eRouteDescription, cbGET, cbPOST, cbPUT, cbPATCH,
    cbDELETE, cbALL, cbRoute]);
end;

procedure TForm1.bStartClick(Sender: TObject);
begin
  if not (spPort.Text = EmptyStr) then
    if FServer = nil then
    begin
      SetupServer;
      bStart.Caption := Format('http://localhost:%d', [FServer.Port]);
    end
    else if FServer.Active then
    begin
      FServer.Active := False;
      FreeAndNil(FServer);
      bStart.Caption := 'Start Server';
    end;
end;

procedure TForm1.SetupServer;
var
  I, J: integer;
  SkipMethods: TStrings;
begin
  case Engine.ItemIndex of
    0: begin
      FServer := TRALfpHttpServer.Create(Self);
      TRALfpHTTPSSL(FServer.SSL).SSLOptions.CertificateFile := esslCertFile.Text;
    end;
    1: begin
      FServer := TRALIndyServer.Create(Self);
      TRALIndySSL(FServer.SSL).SSLOptions.CertFile := esslCertFile.Text;
      TRALIndySSL(FServer.SSL).SSLOptions.KeyFile := esslCertKeyFile.Text;
      //TRALIndySSL(FServer.SSL).SSLOptions.Method := sslvTLSv1_2;
    end;
    2: begin
      FServer := TRALSynopseServer.Create(Self);
    end;
  end;
  FServer.SSL.Enabled := cbUseSSL.Checked;

  case saAuthType.ItemIndex of
    0:
    begin
      FServer.Authentication.Free;
      FServer.Authentication := nil;
    end;
    1:
      FServer.Authentication :=
        TRALServerBasicAuth.Create(nil, saUser.Text, saPassword.Text);
  end;

  FServer.Port := StrToInt(spPort.Text);
  Routes.CreateRoutes(FServer);

  SkipMethods := TStringList.Create;
  SkipMethods.Delimiter := ',';
  for I := 1 to pred(sgRoutes.RowCount) do
  begin
    SkipMethods.DelimitedText := sgRoutes.Cells[2, I];
    for J := 0 to pred(SkipMethods.Count) do
      FServer.Routes.RouteAddress[sgRoutes.Cells[0, I]].SkipAuthMethods :=
        FServer.Routes.RouteAddress[sgRoutes.Cells[0, I]].SkipAuthMethods +
        [HTTPMethodToRALMethod(SkipMethods.Strings[J])];
    SkipMethods.Clear;
  end;
  SkipMethods.Free;

  if cblog.Checked then
  begin
    FServer.OnRequest := @LogRequest;
    FServer.OnResponse := @LogResponse;
  end;

  FServer.Active := True;
end;

procedure TForm1.ClearControls(AControlName: array of TComponent);
var
  I: integer;
  comp: TComponent;
begin
  for I := 0 to pred(Length(AControlName)) do
  begin
    comp := FindComponent(AControlName[I].Name);
    if comp is TEdit then
      TEdit(comp).Text := ''
    else if comp is TCheckBox then
      TCheckBox(comp).Checked := False
    else if comp is TComboBox then
      TComboBox(comp).ItemIndex := 0;
  end;
end;

function TForm1.GetSelectedMethods: string;
var
  I: integer;
begin
  Result := '';
  for I := 0 to pred(Groupbox4.ControlCount) do
    if TCheckBox(Groupbox4.Controls[I]).Checked then
      Result := Result + ', ' + TCheckBox(Groupbox4.Controls[I]).Caption;

  if pos('ALL', Result) > 0 then
    Result := 'ALL'
  else if pos(', ', Result) = 1 then
    Delete(Result, 1, 2);
end;

procedure TForm1.LogRequest(Sender: TObject; ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  mLog.Lines.Append('REQUEST: ' + ARequest.Query);
  mLog.Lines.Append('--------------------------------------------------');
end;

procedure TForm1.LogResponse(Sender: TObject; ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  mLog.Lines.Append('RESPONSE: ' + AResponse.ResponseText);
end;

end.
