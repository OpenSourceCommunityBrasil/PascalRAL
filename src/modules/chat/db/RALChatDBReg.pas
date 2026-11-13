/// Registers the database store of the chat bot in the IDE.
unit RALChatDBReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALChatDBStore;

/// Registers TRALChatDBStore on the palette.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALChatDBStore]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulechatdbpackage.lrs}
{$ENDIF}

end.
