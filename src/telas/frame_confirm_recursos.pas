unit frame_confirm_recursos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, frame_modelo;

type

  { Tfframe_confirm_recursos }

  Tfframe_confirm_recursos = class(Tfframe_modelo)
    mmConfirm : TMemo;
  private

  public
    constructor Create(TheOwner : TComponent); override;
  end;

implementation

{$R *.lfm}

{ Tfframe_confirm_recursos }

constructor Tfframe_confirm_recursos.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
  IdTela := 5;
end;

end.

