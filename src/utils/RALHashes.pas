unit RALHashes;

interface

uses
  Classes, SysUtils;

type
  TRALHashOutputType = (rhotHex,rhotBase64);

  TRALHashes = class
  private
    FOutputType : TRALHashOutputType;
  public
    constructor Create;
  published
    property OutputType : TRALHashOutputType read FOutputType write FOutputType;
  end;

implementation

{ TRALHashes }

constructor TRALHashes.Create;
begin
  FOutputType := rhotHex;
end;

end.
