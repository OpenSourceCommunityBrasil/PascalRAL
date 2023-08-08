unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Grids, LazUTF8,
  RALServer, RALIndyServer, RALFpHttpServer, RALSynopseServer, RALResponse,
  RALRequest, RALRoutes, RALMIMETypes,
  uroutes
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Engine: TRadioGroup;
    Engine1: TRadioGroup;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    LabeledEdit6: TLabeledEdit;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    StringGrid1: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ToggleBox1: TToggleBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ToggleBox1Click(Sender: TObject);
  private
    FServer: TRALServer;
    procedure SetupServer;

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
    ToggleBox1.Caption := Format('http://localhost:%d', [FServer.Port]);
  end
  else
  begin
    FServer.Active := False;
    FreeAndNil(FServer);
    ToggleBox1.Caption := 'Start Server';
  end;
end;

procedure TForm1.SetupServer;
var
  I: integer;
begin
  if not (LabeledEdit1.Text = '') then
  begin
    case Engine.ItemIndex of
      0: FServer := TRALfpHttpServer.Create(Self);
      1: FServer := TRALIndyServer.Create(Self);
      2: FServer := TRALSynopseServer.Create(Self);
    end;
    FServer.Port := StrToInt(LabeledEdit1.Text);

    Routes.CreateRoutes(FServer);

    FServer.Active := True;
  end;
end;

end.
