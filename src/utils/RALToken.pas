unit RALToken;

interface

uses
  Classes, RALTypes;

type
  TTokenCryptTypes = (tctHSHA256, tctHSHA512);

  TJWT = class
  private
    FHeader: StringRAL;
    FPayload: StringRAL;
    FSignature: StringRAL;
    FToken: StringRAL;
    FClaims: TStringList;
  public
    property Header: StringRAL read FHeader;
    property Payload: StringRAL read FPayload;
    property Token: StringRAL read FToken;
  end;

implementation

end.
