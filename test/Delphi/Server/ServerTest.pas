unit ServerTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Edit, FMX.Layouts,
  FMX.Controls.Presentation;

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
    seIndy: TCheckBox;
    seSynopse: TCheckBox;
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

end.
