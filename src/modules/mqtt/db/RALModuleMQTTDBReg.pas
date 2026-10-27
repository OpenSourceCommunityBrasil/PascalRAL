/// Registers TRALMQTTDataSetLink in the IDE.
unit RALModuleMQTTDBReg;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes, SysUtils,
  RALMQTTDataSetLink;

/// Registers the component.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - DAO', [TRALMQTTDataSetLink]);
end;

{$IFDEF FPC}
initialization
{$I ralmodulemqttdbpackage.lrs}
{$ENDIF}

end.
