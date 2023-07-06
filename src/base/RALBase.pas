unit RALBase;

interface

uses
  Classes, SysUtils,
  RALConsts, RALTypes
//  ,Vcl.Forms, Vcl.StdCtrls, Vcl.Graphics, Vcl.ExtCtrls, Vcl.Controls,
//  Vcl.Buttons
  ;

type
  TRALAboutInfo = (RALAbout);

  TRALComponent = class(TComponent)
  private
    FAbout : TRALAboutInfo;
  published
    property About : TRALAboutInfo read FAbout write FAbout stored false ;
  end;

procedure ShowRALAboutForm;

implementation

procedure ShowRALAboutForm;
{
var
  vForm : TForm;
  vLabel : TLabel;
  vPanel : TPanel;
  vInt : IntegerRAL;
  vBevel : TBevel;
  vButton : TBitBtn;
}
begin
{
  vForm := TForm.Create(nil);
  vForm.FormStyle := fsNormal;
  vForm.BorderStyle := bsDialog;
  vForm.Position := poScreenCenter;
  vForm.Width := 340;

  vLabel := TLabel.Create(vForm);
  vLabel.Parent := vForm;
  vLabel.Left := 20;
  vLabel.Top := 10;
  vLabel.Font.Size := 15;
  vLabel.Font.Style := [fsBold];
  vLabel.Caption := 'RAL - REST API Lite';

  vInt := vLabel.Width + 20;

  vLabel := TLabel.Create(vForm);
  vLabel.Parent := vForm;
  vLabel.Alignment := taRightJustify;
  vLabel.Left := vInt;
  vLabel.Top := 10;
  vLabel.Font.Size := 15;
  vLabel.Font.Style := [fsBold];
  vLabel.Font.Color := clRed;
  vLabel.Caption := 'version: '+RALVERSION;

  vPanel := TPanel.Create(vForm);
  vPanel.Parent := vForm;
  vPanel.Caption := '';
  vPanel.Align := alBottom;

  vBevel := TBevel.Create(vForm);
  vBevel.Parent := vPanel;
  vBevel.Height := 2;
  vBevel.Align := alTop;
  vBevel.AlignWithMargins := True;
  vBevel.Margins.Top := 0;
  vBevel.Margins.Bottom := 0;
  vBevel.Margins.Left := 10;
  vBevel.Margins.Right := 10;

  vButton := TBitBtn.Create(vForm);
  vButton.Align := alRight;
  vButton.AlignWithMargins := True;
  vButton.Kind := bkClose;
  vButton.Caption := 'Fechar';
  vButton.Width := 95;

  vForm.ShowModal;
}
end;

end.
