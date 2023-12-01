unit RALDBFireDACLinkReg;

interface

uses
  Classes, RALDBFireDAC;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('RAL - DBWare Links', [TRALDBFireDACLink])
end;

end.
