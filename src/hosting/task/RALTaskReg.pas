/// Registers TRALTask in the IDE.
unit RALTaskReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALTask;

/// Registers TRALTask on the palette.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALTask]);
end;

{$IFDEF FPC}
initialization
{$I ralhostingtaskpackage.lrs}
{$ENDIF}

end.
