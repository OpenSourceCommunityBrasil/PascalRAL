/// Registers the hypermedia plugin in the IDE.
unit RALHypermediaReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALHypermedia;

/// Registers TRALHypermediaPlugin on the palette.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Plugins', [TRALHypermediaPlugin]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulehypermediapackage.lrs}
{$ENDIF}

end.
