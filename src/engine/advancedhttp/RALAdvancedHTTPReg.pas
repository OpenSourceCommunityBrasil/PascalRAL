/// Registers the AdvancedHTTPServer engine components in the IDE.
unit RALAdvancedHTTPReg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALAdvancedHTTPServer;

/// Registers the AdvancedHTTPServer components.
procedure Register;

implementation

procedure Register;
begin
  {$IFDEF RALAdvancedHTTP}
  RegisterComponents('RAL - Server', [TRALAdvancedHTTPServer]);
  {$ENDIF}
end;

{$IFDEF FPC}
initialization
{$I advancedhttpral.lrs}
{$ENDIF}

end.
