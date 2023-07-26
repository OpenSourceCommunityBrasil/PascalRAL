unit DAOBase;

interface

type
  TTestRequestMethod = (rtmGET, rtmPOST, rtmPUT, rtmPATCH, rtmDELETE);

  TDAOBase = Class
  private
    FServer: String;
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

end.
