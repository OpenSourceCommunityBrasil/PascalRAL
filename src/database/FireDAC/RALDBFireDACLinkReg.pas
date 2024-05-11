unit RALDBFireDACLinkReg;

interface

uses
  Classes, RALDBFireDAC, RALDBFiredacMemTable;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('RAL - DBWare', [TRALDBFireDACLink]);
  RegisterComponents('RAL - DBWare', [TRALDBFDMemTable]);
end;

end.
