/// Unit that register RAL UniGUI Engine components in the IDE
unit RALUniGUIReg;

interface

uses
  Classes,
  {$IFNDEF FPC}
    DesignEditors, DesignIntf,
  {$ENDIF}
  RALUniGUIServer;

/// Registers the UniGUI components.
procedure Register;

implementation

{$IFDEF FPC}
  { Hides a published property in the Object Inspector (no such call on FPC; see
    forum.lazarus.freepascal.org, topic 30518). }
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
