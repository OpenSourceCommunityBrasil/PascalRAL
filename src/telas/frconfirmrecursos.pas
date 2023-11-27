unit frconfirmrecursos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, frmodelo;

type

  { Tfconfirmrecursos }

  Tfconfirmrecursos = class(Tfmodelo)
    mmConfirm: TMemo;
  private

  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ Tfconfirmrecursos }

constructor Tfconfirmrecursos.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IdTela := 5;
end;

end.
