unit RALToken;

interface

uses
  Classes;

type
  TTokenCryptTypes = (tctHSHA256, tctHSHA512);

  TJWT = class
  private
    FHeader: string;
    FPayload: string;
    FSignature: string;
    FToken: string;
    FClaims: TStringList;
  public
    property Header: string read FHeader;
    property Payload: string read FPayload;
    property Token: string read FToken;
  end;

implementation

end.
