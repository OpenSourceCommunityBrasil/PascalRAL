unit ServerTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Rtti,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Edit, FMX.Layouts,
  FMX.Controls.Presentation, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.TabControl, FMX.Grid.Style,
  FMX.Grid,

  uRotas,

  RALServer, RALRequest, RALResponse, RALRoutes, RALIndyServer, RALSynopseServer,
  RALTools, RALAuthentication, RALConsts

    ;

type
  TForm1 = class(TForm)
    bStart: TButton;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    GroupBox4: TGroupBox;
    mLog: TMemo;
    Layout1: TLayout;
    SpeedButton1: TSpeedButton;
    sgRoutes: TStringGrid;
    eRouteDescription: TEdit;
    Label7: TLabel;
    GroupBox5: TGroupBox;
    FlowLayout4: TFlowLayout;
    cbGET: TCheckBox;
    cbPOST: TCheckBox;
    cbPUT: TCheckBox;
    cbPATCH: TCheckBox;
    cbDELETE: TCheckBox;
    cbALL: TCheckBox;
    Layout5: TLayout;
    bAdd: TButton;
    bDelete: TButton;
    sgcRouteName: TStringColumn;
    sgcDescription: TStringColumn;
    sgcSkipAuth: TStringColumn;
    cbRoute: TComboBox;
    Label6: TLabel;
    TabControl2: TTabControl;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    TabItem5: TTabItem;
    GroupBox3: TGroupBox;
    FlowLayout3: TFlowLayout;
    saAuthType: TComboBox;
    Label5: TLabel;
    saUser: TEdit;
    Label2: TLabel;
    saPassword: TEdit;
    Label3: TLabel;
    saToken: TEdit;
    Label4: TLabel;
    GroupBox6: TGroupBox;
    FlowLayout5: TFlowLayout;
    esslCertFile: TEdit;
    Label9: TLabel;
    esslCertKeyFile: TEdit;
    Label10: TLabel;
    Edit3: TEdit;
    Label11: TLabel;
    cbUseSSL: TCheckBox;
    GroupBox1: TGroupBox;
    FlowLayout1: TFlowLayout;
    spPort: TEdit;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    FlowLayout2: TFlowLayout;
    rbIndy: TRadioButton;
    rbSynopse: TRadioButton;
    cbLog: TCheckBox;
    procedure bStartClick(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FServer: TRALServer;
    procedure ConfigureServer;
    procedure ClearControls(AControlName: array of TComponent);
    function GetSelectedMethods: string;
    procedure LogRequest(Sender: TObject; ARequest: TRALRequest; AResponse: TRALResponse);
    procedure LogResponse(Sender: TObject; ARequest: TRALRequest;
      AResponse: TRALResponse);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{ TForm1 }

procedure TForm1.bAddClick(Sender: TObject);
begin
  with sgRoutes do
  begin
    RowCount := RowCount + 1;
    Cells[0, pred(RowCount)] := cbRoute.Selected.Text;
    Cells[1, pred(RowCount)] := eRouteDescription.Text;
    Cells[2, pred(RowCount)] := GetSelectedMethods;
  end;
  ClearControls([eRouteDescription, cbGET, cbPOST, cbPUT, cbPATCH, cbDELETE, cbALL,
    cbRoute]);
end;

procedure TForm1.bStartClick(Sender: TObject);
begin
  if not(spPort.Text.IsEmpty) then
    if FServer = nil then
    begin
      ConfigureServer;
      bStart.Text := Format('http://localhost:%d', [FServer.Port]);
    end
    else if FServer.Active then
    begin
      FServer.Active := false;
      FreeAndNil(FServer);
      bStart.Text := 'Start Server';
    end;
end;

procedure TForm1.ClearControls(AControlName: array of TComponent);
var
  I: integer;
  compname: string;
  comp: TComponent;
begin
  for I := 0 to pred(Length(AControlName)) do
  begin
    comp := FindComponent(AControlName[I].Name);
    if comp is TEdit then
      TEdit(comp).Text := ''
    else if comp is TCheckBox then
      TCheckBox(comp).IsChecked := false
    else if comp is TComboBox then
      TComboBox(comp).ItemIndex := 0;
  end;
end;

procedure TForm1.ConfigureServer;
var
  I, J: integer;
  SkipMethods: TStrings;
begin
  if rbIndy.IsChecked then
  begin
    FServer := TRALIndyServer.Create(self);
    TRALIndyServer(FServer).SSL.Enabled := cbUseSSL.IsChecked;
    TRALIndyServer(FServer).SSL.SSLOptions.CertFile := esslCertFile.Text;
    TRALIndyServer(FServer).SSL.SSLOptions.KeyFile := esslCertKeyFile.Text;
    // TRALIndyServer(FServer).SSL.SSLOptions.Method := sslvTLSv1_2;
  end
  else if rbSynopse.IsChecked then
  begin
    FServer := TRALSynopseServer.Create(self);
    TRALSynopseServer(FServer).SSL.Enabled := cbUseSSL.IsChecked;
  end;

  case saAuthType.ItemIndex of
    0:
      FreeAndNil(FServer.Authentication);
    1:
      FServer.Authentication := TRALServerBasicAuth.Create(nil, saUser.Text,
        saPassword.Text);
  end;

  FServer.Port := spPort.Text.ToInteger;
  Rotas.CreateRoutes(FServer);

  SkipMethods := TStringList.Create;
  SkipMethods.Delimiter := ',';
  for I := 0 to pred(sgRoutes.RowCount) do
  begin
    SkipMethods.DelimitedText := sgRoutes.Cells[2, I];
    for J := 0 to pred(SkipMethods.Count) do
      FServer.Routes.RouteAddress[sgRoutes.Cells[0, I]].SkipAuthMethods :=
        FServer.Routes.RouteAddress[sgRoutes.Cells[0, I]].SkipAuthMethods +
        [HTTPMethodToRALMethod(SkipMethods.Strings[J])];
    SkipMethods.Clear;
  end;
  SkipMethods.Free;

  if cbLog.IsChecked then
  begin
    FServer.OnRequest := LogRequest;
    FServer.OnResponse := LogResponse;
  end;

  FServer.Active := true;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Self.Caption := 'RAL Test Server v' + RALVERSION;
  sgRoutes.RowCount := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FServer) then
  begin
    FServer.Active := false;
    FreeAndNil(FServer);
    sleep(500);
  end;
end;

function TForm1.GetSelectedMethods: string;
var
  I: integer;
begin
  Result := '';
  for I := 0 to pred(FlowLayout4.ChildrenCount) do
    if TCheckBox(FlowLayout4.Children[I]).IsChecked then
      Result := Result + ', ' + TCheckBox(FlowLayout4.Children[I]).Text;

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
