unit githubutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson, base64,
  Dialogs;

const
  APIURL = 'https://api.github.com/repos/';
  USERAGENT = 'Pascal REST Api Lite (RAL) Installer Tool v1.0';

type

  { TGitHubAPI }

  TGitHubAPI = class
  private
    FHttp: TFPHTTPClient;
    FRepoitory: string;
  protected
    procedure SetDefaultHeaders;
  public
    constructor Create;
    destructor Destroy; override;

    function DownloadZipRepository(AVersion : string) : TStream;
  published
    property Repository : string read FRepoitory write FRepoitory;
  end;

implementation

{ TGitHubAPI }

procedure TGitHubAPI.SetDefaultHeaders;
begin
  FHttp.RequestHeaders.Clear;
  FHttp.RequestHeaders.AddPair('User-Agent', USERAGENT);
  FHttp.RequestHeaders.AddPair('Accept', '*/*');
  FHttp.RequestHeaders.AddPair('Cache-Control', 'no-cache');
end;

constructor TGitHubAPI.Create;
begin
  FHttp := TFPHTTPClient.Create(nil);
  FHttp.AllowRedirect := True;
end;

destructor TGitHubAPI.Destroy;
begin
  FreeAndNil(FHttp);
  inherited Destroy;
end;

function TGitHubAPI.DownloadZipRepository(AVersion: string): TStream;
var
  vUrl: String;
begin
  SetDefaultHeaders;
  vUrl := Format(APIURL + '%s/zipball/%s',[FRepoitory, AVersion]);

  Result := TMemoryStream.Create;
  FHttp.Get(vUrl, Result);
  Result.Position := 0;
end;

end.

