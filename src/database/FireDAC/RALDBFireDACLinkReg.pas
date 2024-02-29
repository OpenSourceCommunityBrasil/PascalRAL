unit RALDBFireDACLinkReg;

interface

uses
  Classes, RALDBFireDAC;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('RAL - DBWare', [TRALDBFireDACLink])
end;

end.
