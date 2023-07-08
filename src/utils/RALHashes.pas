unit RALHashes;

interface

uses
  Classes, SysUtils,
  RALBase64, RALTypes;

type
  TRALHashOutputType = (rhotHex,rhotBase64);

  TRALHashes = class
  private
    FOutputType : TRALHashOutputType;
  protected
    function DigestToHex(AValue : TBytes) : StringRAL;
    function DigestToBase64(AValue : TBytes) : StringRAL;
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

function TRALHashes.DigestToBase64(AValue: TBytes): StringRAL;
begin
  Result := TRALBase64.Encode(AValue);
  if (Result <> '') and (Result[Length(Result)] = '=') then
    Delete(Result,Length(Result),1);
end;

function TRALHashes.DigestToHex(AValue: TBytes): StringRAL;
const
  HexChar : array[0..15] of CharRAL = ('0','1','2','3','4','5','6','7','8','9',
                                       'a','b','c','d','e','f');
var
  vInt : IntegerRAL;
begin
  vInt := 0;
  while vInt < Length(AValue) do
  begin
    Result := Result + HexChar[(AValue[vInt] shr 4) and $0f];
    Result := Result + HexChar[(AValue[vInt] and $0f)];
    Inc(vInt);
  end;
end;

end.
