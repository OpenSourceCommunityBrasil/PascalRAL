unit frinstall;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  frmodelo, imagefunctions;

type

  { Tfinstall }

  Tfinstall = class(Tfmodelo)
    Button1: TButton;
    imlogoBG: TImage;
    mmLogInstall: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure lbNextClick(Sender: TObject);
  private

  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ Tfinstall }

procedure Tfinstall.Button1Click(Sender: TObject);
begin
  TImgUtils.AnimaImagemSurgir(imlogoBG, 1.0);
end;

procedure Tfinstall.lbNextClick(Sender: TObject);
begin
  ShowMessage('Install');
end;

constructor Tfinstall.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IdTela := 6;
end;

end.
