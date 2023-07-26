unit uResultado;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts;

type
  TfResultado = class(TForm)
    Memo1: TMemo;
    Layout1: TLayout;
    Button3: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LogMessage(aMessage: string); overload;
    procedure LogMessage(aMessage: string; aParams: array of const); overload;
  end;

var
  FResultado: TfResultado;

implementation

{$R *.fmx}

procedure TfResultado.Button3Click(Sender: TObject);
begin
  Memo1.Lines.SaveToFile(ExtractFileDir(ParamStr(0)) + '\logTestTool.txt');
  LogMessage('Log salvo no arquivo: ' + ExtractFileDir(ParamStr(0)) +
    '\logTestTool.txt');
end;

procedure TfResultado.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
  Memo1.Lines.Clear;
end;

procedure TfResultado.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TfResultado.FormShow(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TfResultado.LogMessage(aMessage: string; aParams: array of const);
var
  textvalue: string;
begin
  textvalue := Format(aMessage, aParams);
  TThread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Add(textvalue);
      Memo1.GoToTextEnd;
    end);
end;

procedure TfResultado.LogMessage(aMessage: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Add(aMessage);
      Memo1.GoToTextEnd;
    end);
end;

end.
