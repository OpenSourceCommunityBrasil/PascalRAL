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
  RALTools, RALAuthentication

    ;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    FlowLayout1: TFlowLayout;
    FlowLayout2: TFlowLayout;
    FlowLayout3: TFlowLayout;
    spPort: TEdit;
    Label1: TLabel;
    spUseSSL: TCheckBox;
    saAuthType: TComboBox;
    saPassword: TEdit;
    saToken: TEdit;
    saUser: TEdit;
    bStart: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    rbIndy: TRadioButton;
    rbSynopse: TRadioButton;
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
    procedure bStartClick(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FServer: TRALServer;
    procedure ConfigureServer;
    procedure ClearControls(AControlName: array of string);
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

procedure TForm1.ClearControls(AControlName: array of string);
var
  I: integer;
  compname: string;
  comp: TComponent;
begin
  for I := 0 to pred(Length(AControlName)) do
  begin
    comp := FindComponent(AControlName[I]);
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
    FServer := TRALIndyServer.Create(self)
  else if rbSynopse.IsChecked then
    FServer := TRALSynopseServer.Create(self);

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

  FServer.OnRequest := LogRequest;
  FServer.OnResponse := LogResponse;

  FServer.Active := true;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  sgRoutes.RowCount := 0;
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
