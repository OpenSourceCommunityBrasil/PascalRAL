/// Registers the XML storage link in the IDE.
unit RALXMLReg;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes, SysUtils,
  RALStorageXML;

/// Puts TRALStorageXMLLink on the RAL - Storage palette.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Storage', [TRALStorageXMLLink]);
end;

{$IFDEF FPC}
initialization
{$I ralxml.lrs}
{$ENDIF}

end.
