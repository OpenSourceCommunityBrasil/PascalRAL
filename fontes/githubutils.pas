unit githubutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  http_client;

const
  APIURL = 'https://api.github.com/repos/';

type

  { TGitHubAPI }

  TGitHubAPI = class
  private
    FRepoitory: string;
  public
    function DownloadZipRepository(AVersion : string) : TStream;
  published
    property Repository : string read FRepoitory write FRepoitory;
  end;

implementation

{ TGitHubAPI }

function TGitHubAPI.DownloadZipRepository(AVersion: string): TStream;
var
  vUrl: String;
  vHttp : THttpClient;
begin
  vUrl := Format(APIURL + '%s/zipball/%s',[FRepoitory, AVersion]);

  vHttp := THttpClient.Create;
  try
    Result := vHttp.DownloadLink(vUrl);
  finally
    FreeAndNil(vHttp);
  end;
end;

end.

