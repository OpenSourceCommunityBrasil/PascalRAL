unit RALfpHTTPServer;

interface

uses
  Classes, SysUtils,
  RALServer, RALTypes, RALConsts, RALMIMETypes, RALRoutes;

type
  TRALfpHTTPSSL = class(TRALSSL)
  private
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  TRALfpHTTPServer = class(TRALServer)
  private
  protected
    procedure SetActive(const Value: boolean); override;
    procedure SetPort(const Value: IntegerRAL); override;
    function CreateRALSSL: TRALSSL; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRALfpHTTPSSL }

constructor TRALfpHTTPSSL.Create;
begin

end;

destructor TRALfpHTTPSSL.Destroy;
begin

  inherited;
end;

{ TRALfpHTTPServer }

constructor TRALfpHTTPServer.Create(AOwner: TComponent);
begin
  inherited;

end;

function TRALfpHTTPServer.CreateRALSSL: TRALSSL;
begin

end;

destructor TRALfpHTTPServer.Destroy;
begin

  inherited;
end;

procedure TRALfpHTTPServer.SetActive(const Value: boolean);
begin
  inherited;

end;

procedure TRALfpHTTPServer.SetPort(const Value: IntegerRAL);
begin
  inherited;

end;

end.
