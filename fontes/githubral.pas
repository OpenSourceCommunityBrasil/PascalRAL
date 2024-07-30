unit githubral;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson, jsonparser;

const
  RepoName = 'OpenSourceCommunityBrasil/REST-DataWare/';
  APIURL = 'https://api.github.com/repos/' + RepoName;
  SiteURL = 'https://github.com/' + RepoName;
  TagsAPI = APIURL + 'tags';
  BranchesAPI = APIURL + 'branches';
  ZipDownloadAPI = APIURL + 'zipball/%s';
  FileDownloadAPI = SiteURL + 'raw/installer/%s';
  ReleaseAPI = APIURL + 'releases/latest';
  USERAGENT = 'Pascal REST Api Lite (RAL) Installer Tool v1.0';

type

  { TRelease }

  TRelease = class
  private
    FDownloadLink: string;
    FDownloadLocation: string;
    FName: string;
    FNotes: string;
    FVersion: string;
    procedure SetDownloadLocation(AValue: string);
    procedure SetName(AValue: string);
    procedure SetNotes(AValue: string);
    procedure SetVersion(AValue: string);
  published
    property Name: string read FName write SetName;
    property Notes: string read FNotes write SetNotes;
    property Version: string read FVersion write SetVersion;
    property DownloadLink: string read FDownloadLink;
    property DownloadLocation: string read FDownloadLocation write SetDownloadLocation;
  end;

  { TRESTClient }

  TRESTClient = class
  private
    FHTTPREST: TFPHTTPClient;
    procedure setDefaultHeaders;
  public
    constructor Create;
    destructor Destroy; override;
    function getLatestRelease: TRelease;
    function getTagsList: string;
    function getBranchesList: string;
    function getFileStream(aFileName: string): TFileStream;
    function Download(aDirectory: string; aVersion: string): boolean;
  end;

implementation

{ TRelease }

procedure TRelease.SetName(AValue: string);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

procedure TRelease.SetDownloadLocation(AValue: string);
begin
  if FDownloadLocation = AValue then Exit;
  FDownloadLocation := AValue;
end;

procedure TRelease.SetNotes(AValue: string);
begin
  if FNotes = AValue then Exit;
  FNotes := AValue;
end;

procedure TRelease.SetVersion(AValue: string);
begin
  if FVersion = AValue then Exit;
  FVersion := AValue;
end;

{ TRESTClient }

constructor TRESTClient.Create;
begin
  FHTTPREST := TFPHTTPClient.Create(nil);
  FHTTPREST.AllowRedirect := True;
  setDefaultHeaders;
end;

destructor TRESTClient.Destroy;
begin
  FHTTPREST.Free;
  inherited Destroy;
end;

procedure TRESTClient.setDefaultHeaders;
begin
  FHTTPREST.RequestHeaders.Clear;
  FHTTPREST.RequestHeaders.AddPair('User-Agent', USERAGENT);
  //  FHTTPREST.RequestHeaders.AddPair('Accept', 'application/json');
  //  FHTTPREST.RequestHeaders.AddPair('Accept-Encoding', 'gzip, deflate, br');
  //  FHTTPREST.RequestHeaders.AddPair('Connection', 'keep-alive');
  FHTTPREST.RequestHeaders.AddPair('Content-Type', 'application/json; charset=UTF-8');
end;

function TRESTClient.getLatestRelease: TRelease;
var
  sstr: TStringStream;
  JSONObj: TJSONObject;
begin
  sstr := TStringStream.Create(FHTTPREST.Get(ReleaseAPI));
  try
    Result := TRelease.Create;
    JSONObj := TJSONObject(GetJSON(sstr));
    Result.Name := JSONObj.Get('name', '');
    Result.Notes := JSONObj.Get('body', '');
    Result.Version := JSONObj.Get('tag_name', '');
    Result.FDownloadLink := JSONObj.Get('zipball_url', '');
  finally
    JSONObj.Free;
    sstr.Free;
  end;
end;

function TRESTClient.getTagsList: string;
var
  Tags: TJSONArray;
  slistTags: TStringList;
  strResposta: TStream;
  I: integer;
begin
  slistTags := TStringList.Create;
  Result := '';
  try
    strResposta := TStringStream.Create(FHTTPREST.Get(TagsAPI), TEncoding.UTF8);
    Tags := TJSONArray(GetJSON(strResposta));
    for I := 0 to pred(Tags.Count) do
      slistTags.Add(TJSONObject(Tags.Items[i]).Get('name'));
    Result := slistTags.DelimitedText;
  finally
    slistTags.Free;
    strResposta.Free;
    Tags.Free;
  end;
end;

function TRESTClient.getBranchesList: string;
var
  Tags: TJSONArray;
  slistBranches: TStringList;
  strResposta: TStream;
  I: integer;
begin
  slistBranches := TStringList.Create;
  Result := '';
  try
    strResposta := TStringStream.Create(FHTTPREST.Get(BranchesAPI), TEncoding.UTF8);
    Tags := TJSONArray(GetJSON(strResposta));
    for I := 0 to pred(Tags.Count) do
      slistBranches.Add(TJSONObject(Tags.Items[i]).Get('name'));
    Result := slistBranches.DelimitedText;
  finally
    slistBranches.Free;
    strResposta.Free;
    Tags.Free;
  end;
end;

function TRESTClient.getFileStream(aFileName: string): TFileStream;
begin
  FHTTPREST.Get(Format(FileDownloadAPI, [aFileName]), Result);
end;

function TRESTClient.Download(aDirectory: string; aVersion: string): boolean;
var
  fstr: TFileStream;
begin
  fstr := TFileStream.Create(aDirectory + 'PascalRAL - ' + aVersion + '.zip', fmCreate);
  FHTTPREST.Get(Format(ZipDownloadAPI, [aVersion]), fstr);
  fstr.Free;
end;

end.
