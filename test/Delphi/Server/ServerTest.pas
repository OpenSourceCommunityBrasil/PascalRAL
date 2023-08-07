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
  RALIndyServer, RALSynopseServer, FMX.TabControl, System.Rtti, FMX.Grid.Style,
  FMX.Grid

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
    StringGrid1: TStringGrid;
    Edit1: TEdit;
    Label6: TLabel;
    Edit2: TEdit;
    Label7: TLabel;
    GroupBox5: TGroupBox;
    FlowLayout4: TFlowLayout;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Layout5: TLayout;
    Button1: TButton;
    Button2: TButton;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
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

  FServer.Active := true;
end;

end.
