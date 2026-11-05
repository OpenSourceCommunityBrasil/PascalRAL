unit LetsEncryptReg;

{$IFDEF FPC}
{$mode delphi}
{$H+}
{$ENDIF}

interface

procedure Register;

implementation

uses
  Classes,
  LetsEncrypt;

procedure Register;
begin
  RegisterComponents('ACME', [TLetsEncrypt]);
end;

end.
