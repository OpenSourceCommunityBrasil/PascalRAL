unit RALUniGUIRegister;

interface

uses
  Classes,
  RALUniGUIServer;

procedure Register;

implementation

{$IFDEF FPC}
  // todo - verificar se nao existe no FPC
  // https://forum.lazarus.freepascal.org/index.php?topic=30518.0
  procedure UnlistPublishedProperty(ComponentClass: TPersistentClass; const PropertyName: string);
  var
    pi :PPropInfo;
  begin
    pi := TypInfo.GetPropInfo (ComponentClass, PropertyName);
    if (pi <> nil) then
      RegisterPropertyEditor(pi^.PropType, ComponentClass, PropertyName, PropEdits.THiddenPropertyEditor);
  end;
{$ENDIF}

procedure Register;
begin
  UnlistPublishedProperty(TRALUniGUIServer, 'Port');
  UnlistPublishedProperty(TRALUniGUIServer, 'SessionTimeout');
  UnlistPublishedProperty(TRALUniGUIServer, 'IPConfig');
  RegisterComponents('RAL - Server', [TRALUniGUIServer]);
end;

end.
