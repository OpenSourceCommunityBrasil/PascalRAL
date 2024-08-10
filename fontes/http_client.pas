unit http_client;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, Forms;

const
  USERAGENT = 'Pascal REST Api Lite (RAL) Installer Tool v1.0';

type

  { THttpClient }

  THttpClient = class
  private
    FHttp: TFPHTTPClient;
  protected
    procedure SetDefaultHeaders;
    procedure OnData(Sender : TObject; Const ContentLength, CurrentPos : Int64);
  public
    constructor Create;
    destructor Destroy; override;

    function DownloadLink(ALink : string) : TStream;
  end;

implementation

{ THttpClient }

procedure THttpClient.SetDefaultHeaders;
begin
  FHttp.RequestHeaders.Clear;
  FHttp.RequestHeaders.AddPair('User-Agent', USERAGENT);
  FHttp.RequestHeaders.AddPair('Accept', '*/*');
  FHttp.RequestHeaders.AddPair('Cache-Control', 'no-cache');
end;

procedure THttpClient.OnData(Sender: TObject; const ContentLength, CurrentPos: Int64);
begin
  Application.ProcessMessages;
end;

constructor THttpClient.Create;
begin
  inherited;
  FHttp := TFPHTTPClient.Create(nil);
  FHttp.AllowRedirect := True;
  FHttp.OnDataReceived := @OnData;
end;

destructor THttpClient.Destroy;
begin
  FreeAndNil(FHttp);
  inherited Destroy;
end;

function THttpClient.DownloadLink(ALink: string): TStream;
begin
  SetDefaultHeaders;

  Result := TMemoryStream.Create;
  FHttp.Get(ALink, Result);
  Result.Position := 0;
end;

end.

