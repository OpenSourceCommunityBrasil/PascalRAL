unit ufrm_install;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ufrm_modelo;

type

  { Tfrm_install }

  Tfrm_install = class(Tfrm_modelo)
    lbSubTitle: TLabel;
    mLogInstall: TMemo;
    procedure lbNextClick(Sender: TObject);
  private
    FInstalling : boolean;
  protected
    function validatePagePrior : boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
  end;

implementation

{$R *.lfm}

uses
  umain;

{ Tfrm_install }

procedure Tfrm_install.lbNextClick(Sender: TObject);
begin
  if FInstalling then
    Exit;

  FInstalling := True;

  mLogInstall.Clear;
  Application.ProcessMessages;

  if fmain.checkDepedancy(mLogInstall) then
  begin
    fmain.downloadRAL(mLogInstall);
    fmain.installRAL(mLogInstall);
  end;

  FInstalling := False;
end;

function Tfrm_install.validatePagePrior: boolean;
begin
  Result := not FInstalling;
end;

constructor Tfrm_install.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInstalling := False;
end;

end.

