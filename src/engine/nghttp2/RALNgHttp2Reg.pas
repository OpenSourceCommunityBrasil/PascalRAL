/// Registers the nghttp2 engine components in the IDE.
unit RALNgHttp2Reg;

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Classes,
  RALNgHttp2Server, RALNgHttp2Client;

/// Registers the nghttp2 components.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Server', [TRALNgHttp2Server]);
end;

{$IFDEF FPC}
initialization
{$I nghttp2ral.lrs}
{$ENDIF}

end.
