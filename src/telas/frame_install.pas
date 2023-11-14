unit frame_install;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  frame_modelo, imagefunctions;

type

  { Tfframe_install }

  Tfframe_install = class(Tfframe_modelo)
    Button1 : TButton;
    imlogoBG : TImage;
    mmLogInstall : TMemo;
    procedure Button1Click(Sender : TObject);
    procedure lbNextClick(Sender : TObject);
  private

  public
    constructor Create(TheOwner : TComponent); override;
  end;

implementation

{$R *.lfm}

{ Tfframe_install }

procedure Tfframe_install.Button1Click(Sender : TObject);
begin
  TImgUtils.AnimaImagemSurgir(imlogoBG, 1.0);
end;

procedure Tfframe_install.lbNextClick(Sender : TObject);
begin
  ShowMessage('Install');
end;

constructor Tfframe_install.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
  IdTela := 6;
end;

end.

