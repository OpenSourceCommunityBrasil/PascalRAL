/// Unit that register RAL Kwik Engine components in the IDE
///
/// There is no component to put on a palette - Kwik is a client engine and
/// TRALClient chooses it by name - so this unit exists only to give the
/// package a registration point. RALKwikClient registers the engine itself,
/// from its own initialization, on every platform: the property editor behind
/// EngineType lists what RegisterEngine filled in, and a name missing from
/// that list can never be chosen, not even for an Android target built on a
/// Windows IDE.
unit RALKwikRegister;

interface

uses
  Classes,
  RALKwikClient;

procedure Register;

implementation

procedure Register;
begin
  { nothing on the palette: registering the engine is RALKwikClient's own
    initialization, which runs as soon as the unit is linked }
end;

end.
