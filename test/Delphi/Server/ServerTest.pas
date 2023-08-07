unit ServerTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Edit, FMX.Layouts,
  FMX.Controls.Presentation, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView,

  uRotas,

  RALServer, RALRequest, RALResponse, RALRoutes,
  RALIndyServer, RALSynopseServer

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
    GroupBox4: TGroupBox;
    mLog: TMemo;
    bStart: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    SpeedButton1: TSpeedButton;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
    Label6: TLabel;
    ListView1: TListView;
    rbIndy: TRadioButton;
    rbSynopse: TRadioButton;
    procedure bStartClick(Sender: TObject);
  private
    { Private declarations }
    FServer: TRALServer;
    procedure ConfigureServer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{ TForm1 }

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
      FreeAndNil(FServer);
      bStart.Text := 'Start Server';
    end;
end;

procedure TForm1.ConfigureServer;
var
  I: Integer;
begin
  if rbIndy.IsChecked then
    FServer := TRALIndyServer.Create(self)
  else if rbSynopse.IsChecked then
    FServer := TRALSynopseServer.Create(self);

  FServer.Port := spPort.Text.ToInteger;
  Rotas.CreateRoutes(FServer);

  ListView1.Items.Clear;
  for I := 0 to pred(FServer.Routes.Count) do
    ListView1.Items.Add.Text := TRALRoute(FServer.Routes.Items[I]).RouteName;

  FServer.Active := true;
end;

end.
