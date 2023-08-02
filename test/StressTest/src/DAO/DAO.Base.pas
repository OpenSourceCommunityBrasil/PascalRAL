unit DAO.Base;

interface

type
  TTestRequestMethod = (rtmGET, rtmPOST, rtmPUT, rtmPATCH, rtmDELETE);

  TDAOBase = Class
  protected
    FServer: String;
    FCurrentMethod: String;
    FExpectedCode: integer;
    function MethodToString(aMethod: TTestRequestMethod): string;
  public
    constructor Create(aServer, aPort: string); virtual; abstract;
    procedure SetBasicAuth(user, password: string); virtual; abstract;
    function TesteEndpoint(aEndpoint: string; aMethod: TTestRequestMethod;
      out erro: string): boolean; overload; virtual; abstract;
    function TesteEndpoint(aEndpoint: string; aMethod: TTestRequestMethod;
      AExpectedResponse: string; out erro: string): boolean; overload;
      virtual; abstract;
  End;

implementation

{ TDAOBase }

function TDAOBase.MethodToString(aMethod: TTestRequestMethod): string;
begin
  case aMethod of
    rtmGET: Result := 'GET';
    rtmPOST: Result := 'POST';
    rtmPUT: Result := 'PUT';
    rtmPATCH: Result := 'PATCH';
    rtmDELETE: Result := 'DELETE';
  end;
end;

end.
