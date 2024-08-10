unit installparser;

interface

uses
  Classes, SysUtils, Dialogs, StdCtrls, Forms, githubutils, fpjson, ralzipper,
  http_client, tar_gzip;

type

  { TSelected }

  TSelected = class
  private
    FSelected : boolean;
  protected
    procedure SetSelected(AValue: boolean); virtual;
  published
    property Selected : boolean read FSelected write SetSelected;
  end;

  { TDPK }

  TDPK = class
  private
    FVersionCode: integer;
    FLocation: string;
  protected
    function GetAsJSON: TJSONObject;
    procedure SetAsJSON(AValue: TJSONObject);
  published
    property AsJSON : TJSONObject read GetAsJSON write SetAsJSON;
  end;

  { TDPKList }

  TDPKList = class
  private
    FList: TList;
  protected
    function GetAsJSON: TJSONArray;
    procedure SetAsJSON(AValue: TJSONArray);
    function GetDPK(AIndex : integer): TDPK;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function NewDPK : TDPK;
    function Count : integer;

    property DPK[AIndex : integer] : TDPK read GetDPK;
  published
    property AsJSON : TJSONArray read GetAsJSON write SetAsJSON;
  end;

  { TInstall }

  TInstall = class
  private
    FDPK: TDPKList;
    FLPK: string;
    FLPKIsLink : boolean;
    FIsEmpty : boolean;
  protected
    function GetAsJSON: TJSONObject;
    procedure SetAsJSON(AValue: TJSONObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AsJSON : TJSONObject read GetAsJSON write SetAsJSON;
    property DPK : TDPKList read FDPK write FDPK;
    property LPK : string read FLPK write FLPK;
    property LPKIsLink : boolean read FLPKIsLink write FLPKIsLink;
    property IsEmpty : boolean read FIsEmpty write FIsEmpty;
  end;

  TPackages = class;

  { TEngine }

  TEngine = class(TSelected)
  private
    FOwner : TPackages;
    FName: string;
    FInstall: TInstall;
    FDepedancy: TStringList;
  protected
    function GetAsJSON: TJSONObject;
    procedure SetAsJSON(AValue: TJSONObject);
  public
    constructor Create(AOwner : TPackages);
    destructor Destroy; override;
  published
    property AsJSON : TJSONObject read GetAsJSON write SetAsJSON;
    property Name : string read FName write FName;
    property Install : TInstall read FInstall write FInstall;
    property Depedancy : TStringList read FDepedancy write FDepedancy;
  end;

  { TEngineList }

  TEngineList = class
  private
    FOwner : TPackages;
    FList : TList;
  protected
    function GetAsJSON: TJSONArray;
    procedure SetAsJSON(AValue: TJSONArray);
    function GetEngine(AIndex : integer): TEngine;
  public
    constructor Create(AOwner : TPackages);
    destructor Destroy; override;

    procedure Clear;
    function NewEngine : TEngine;

    function Count : integer;

    property Engine[AIndex : integer] : TEngine read GetEngine;
  published
    property AsJSON : TJSONArray read GetAsJSON write SetAsJSON;
  end;

  { TDatabase }

  TDatabase = class(TSelected)
  private
    FOwner : TPackages;
    FName: string;
    FInstall: TInstall;
    FDepedancy: TStringList;
  protected
    function GetAsJSON: TJSONObject;
    procedure SetAsJSON(AValue: TJSONObject);
  public
    constructor Create(AOwner : TPackages);
    destructor Destroy; override;
  published
    property AsJSON : TJSONObject read GetAsJSON write SetAsJSON;

    property Name : string read FName write FName;
    property Install : TInstall read FInstall write FInstall;
    property Depedancy : TStringList read FDepedancy write FDepedancy;
  end;

  { TDatabasesList }

  TDatabasesList = class
  private
    FOwner : TPackages;
    FList : TList;
  protected
    function GetAsJSON: TJSONArray;
    function GetDatabase(AIndex : integer): TDatabase;
    procedure SetAsJSON(AValue: TJSONArray);
  public
    constructor Create(AOwner : TPackages);
    destructor Destroy; override;

    procedure Clear;
    function NewDatabase : TDatabase;

    function Count : integer;

    property Database[AIndex : integer] : TDatabase read GetDatabase;
  published
    property AsJSON : TJSONArray read GetAsJSON write SetAsJSON;
  end;

  { TDatabases }

  TDatabases = class(TSelected)
  private
    FOwner : TPackages;
    FName: string;
    FInstall: TInstall;
    FPackages: TDatabasesList;
  protected
    function GetAsJSON: TJSONObject;
    procedure SetAsJSON(AValue: TJSONObject);
  public
    constructor Create(AOwner : TPackages);
    destructor Destroy; override;
  published
    property AsJSON : TJSONObject read GetAsJSON write SetAsJSON;
    property Name : string read FName write FName;
    property Install : TInstall read FInstall write FInstall;
    property Packages : TDatabasesList read FPackages write FPackages;
  end;

  { TCompression }

  TCompression = class(TSelected)
  private
    FOwner : TPackages;
    FName: string;
    FInstall: TInstall;
    FDepedancy: TStringList;
  protected
    function GetAsJSON: TJSONObject;
    procedure SetAsJSON(AValue: TJSONObject);
  public
    constructor Create(AOwner : TPackages);
    destructor Destroy; override;
  published
    property AsJSON : TJSONObject read GetAsJSON write SetAsJSON;

    property Name : string read FName write FName;
    property Install : TInstall read FInstall write FInstall;
    property Depedancy : TStringList read FDepedancy write FDepedancy;
  end;

  { TCompressionList }

  TCompressionList = class
  private
    FOwner : TPackages;
    FList : TList;
  protected
    function GetAsJSON: TJSONArray;
    function GetCompression(AIndex : integer): TCompression;
    procedure SetAsJSON(AValue: TJSONArray);
  public
    constructor Create(AOwner : TPackages);
    destructor Destroy; override;

    procedure Clear;
    function NewCompression : TCompression;

    function Count : integer;

    property Compression[AIndex : integer] : TCompression read GetCompression;
  published
    property AsJSON : TJSONArray read GetAsJSON write SetAsJSON;
  end;

  { TPackages }

  TPackages = class(TSelected)
  private
    FName: string;
    FRepository: string;
    FRepoVersion: string;
    FFolder: string;
    FInstall: TInstall;
    FEngines: TEngineList;
    FDatabases: TDatabases;
    FCompression: TCompressionList;
  protected
    function GetAsJSON: TJSONObject;
    procedure SetAsJSON(AValue: TJSONObject);
    procedure SetSelected(AValue: boolean); override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AsJSON : TJSONObject read GetAsJSON write SetAsJSON;

    property Name : string read FName write FName;
    property Repository : string read FRepository write FRepository;
    property RepoVersion : string read FRepoVersion write FRepoVersion;
    property Folder : string read FFolder write FFolder;
    property Install : TInstall read FInstall write FInstall;
    property Engines : TEngineList read FEngines write FEngines;
    property Databases : TDatabases read FDatabases write FDatabases;
    property Compression : TCompressionList read FCompression write FCompression;
  end;

  { TDownload }

  TDownload = class
  private
    FLink : string;
    FFormat : string;
    FFolder : string;
  protected
    function GetAsJSON: TJSONObject;
    procedure SetAsJSON(AValue: TJSONObject);
  published
    property AsJSON : TJSONObject read GetAsJSON write SetAsJSON;

    property Link : string read FLink write FLink;
    property Format : string read FFormat write FFormat;
    property Folder : string read FFolder write FFolder;
  end;

  { TDownloadList }

  TDownloadList = class
  private
    FList : TList;
  protected
    function GetAsJSON: TJSONArray;
    function GetDownload(AIndex : integer): TDownload;
    procedure SetAsJSON(AValue: TJSONArray);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function NewDownload : TDownload;

    function Count : integer;

    property Download[AIndex : integer] : TDownload read GetDownload;
  published
    property AsJSON : TJSONArray read GetAsJSON write SetAsJSON;
  end;

  { TDepedancy }

  TDepedancy = class(TSelected)
  private
    FName: string;
    FRepository: string;
    FRepoVersion: string;
    FFolder: string;
    FInstall: TInstall;
    FDownloads : TDownloadList;
  protected
    function GetAsJSON: TJSONObject;
    procedure SetAsJSON(AValue: TJSONObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AsJSON : TJSONObject read GetAsJSON write SetAsJSON;

    property Name : string read FName write FName;
    property Install : TInstall read FInstall write FInstall;
    property Repository : string read FRepository write FRepository;
    property RepoVersion : string read FRepoVersion write FRepoVersion;
    property Folder : string read FFolder write FFolder;
    property Downloads : TDownloadList read FDownloads write FDownloads;
  end;

  { TDepedancyList }

  TDepedancyList = class
  private
    FList : TList;
  protected
    function GetAsJSON: TJSONArray;
    function GetDepedancy(AIndex : integer): TDepedancy;
    procedure SetAsJSON(AValue: TJSONArray);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function NewDepedancy : TDepedancy;

    function Count : integer;

    property Depedancy[AIndex : integer] : TDepedancy read GetDepedancy;
  published
    property AsJSON : TJSONArray read GetAsJSON write SetAsJSON;
  end;

  { TInstaller }

  TInstaller = class
  private
    FEula: string;
    FVersion: integer;
    FPackages: TPackages;
    FDepedancies: TDepedancyList;
  protected
    function downloadRepository(ARepository, AVersion : string) : TStream;
    function downloadLink(ALink : string) : TStream;

    procedure descompressRepository(AFile, APath, AFolder : string); overload;
    procedure descompressRepository(AStream : TStream; APath, AFolder : string); overload;
    procedure downloadToFolder(ALink, AFormat, AFolder, APath: string);

    function logar(ALog : TMemo; AStr : string) : boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function getDepedancy(ADepedancy : string) : TDepedancy;

    procedure download(APath : string; ALog : TMemo);
    function checkDepedancy(ALog : TMemo): boolean;
  published
    property Version: integer read FVersion write FVersion;
    property Eula: string read FEula write FEula;
    property Packages: TPackages read FPackages write FPackages;
    property Depedancies: TDepedancyList read FDepedancies write FDepedancies;
  end;

implementation

{ TSelected }

procedure TSelected.SetSelected(AValue: boolean);
begin
  if AValue = FSelected then
    Exit;

  FSelected := AValue;
end;

{ TDPK }

function TDPK.GetAsJSON: TJSONObject;
begin
  Result := nil;
end;

procedure TDPK.SetAsJSON(AValue: TJSONObject);
begin
  if AValue = nil then
    Exit;

  FVersionCode := AValue.Get('version-code', -1);
  FLocation := AValue.Get('location', '');
end;

{ TDPKList }

function TDPKList.GetDPK(AIndex : integer): TDPK;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FList.Count) then
    Result := TDPK(FList.Items[AIndex]);
end;

function TDPKList.GetAsJSON: TJSONArray;
begin
  Result := nil;
end;

procedure TDPKList.SetAsJSON(AValue: TJSONArray);
var
  vInt: Integer;
begin
  if AValue = nil then
    Exit;

  for vInt := 0 to Pred(AValue.Count) do
    NewDPK.AsJSON := TJSONObject(AValue.Items[vInt]);
end;

procedure TDPKList.Clear;
begin
  while FList.Count > 0 do
  begin
    TObject(FList.Items[FList.Count - 1]).Free;
    FList.Delete(FList.Count - 1);
  end;
end;

function TDPKList.NewDPK: TDPK;
begin
  Result := TDPK.Create;
  FList.Add(Result);
end;

function TDPKList.Count: integer;
begin
  Result := FList.Count;
end;

constructor TDPKList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TDPKList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

{ TInstall }

function TInstall.GetAsJSON: TJSONObject;
begin
  Result := nil;
end;

procedure TInstall.SetAsJSON(AValue: TJSONObject);
begin
  FIsEmpty := AValue = nil;
  if FIsEmpty then
    Exit;

  FDPK.AsJSON := AValue.Get('dpk', TJSONArray(nil));
  FLPK := AValue.Get('lpk', '');
  FLPKIsLink := AValue.Get('lpk_islink', False);
end;

constructor TInstall.Create;
begin
  inherited;
  FDPK := TDPKList.Create;
  FLPK := '';
  FIsEmpty := True;
end;

destructor TInstall.Destroy;
begin
  FreeAndNil(FDPK);
  inherited Destroy;
end;

{ TEngine }

function TEngine.GetAsJSON: TJSONObject;
begin
  Result := nil;
end;

procedure TEngine.SetAsJSON(AValue: TJSONObject);
var
  vInt: integer;
  vArray : TJSONArray;
begin
  FName := AValue.Get('name', '');
  FInstall.AsJSON := AValue.Get('install', TJSONObject(nil));

  vArray := AValue.Get('depedancy', TJSONArray(nil));
  if vArray <> nil then
  begin
    for vInt := 0 to Pred(vArray.Count) do
      FDepedancy.Add(vArray.Items[vInt].AsString);
  end;
end;

constructor TEngine.Create(AOwner: TPackages);
begin
  inherited Create;
  FOwner := AOwner;
  FInstall := TInstall.Create;
  FDepedancy := TStringList.Create;
  FSelected := False;
end;

destructor TEngine.Destroy;
begin
  FreeAndNil(FInstall);
  FreeAndNil(FDepedancy);
  inherited Destroy;
end;

{ TEngineList }

function TEngineList.GetEngine(AIndex: integer): TEngine;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FList.Count) then
    Result := TEngine(FList.Items[AIndex]);
end;

function TEngineList.GetAsJSON: TJSONArray;
begin
  Result := nil;
end;

procedure TEngineList.SetAsJSON(AValue: TJSONArray);
var
  vInt: integer;
begin
  if AValue = nil then
    Exit;

  for vInt := 0 to Pred(AValue.Count) do
    NewEngine.AsJSON := TJSONObject(AValue.Items[vInt]);
end;

procedure TEngineList.Clear;
begin
  while FList.Count > 0 do
  begin
    TObject(FList.Items[FList.Count - 1]).Free;
    FList.Delete(FList.Count - 1);
  end;
end;

function TEngineList.NewEngine: TEngine;
begin
  Result := TEngine.Create(FOwner);
  FList.Add(Result);
end;

function TEngineList.Count: integer;
begin
  Result := FList.Count;
end;

constructor TEngineList.Create(AOwner: TPackages);
begin
  inherited Create;
  FOwner := AOwner;
  FList := TList.Create;
end;

destructor TEngineList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

{ TDatabase }

function TDatabase.GetAsJSON: TJSONObject;
begin
  Result := nil;
end;

procedure TDatabase.SetAsJSON(AValue: TJSONObject);
var
  vInt: Integer;
  vArray: TJSONArray;
begin
  if AValue = nil then
    Exit;

  FName := AValue.Get('name', '');
  FInstall.AsJSON := AValue.Get('install', TJSONObject(nil));

  vArray := AValue.Get('depedancy', TJSONArray(nil));
  if vArray <> nil then
  begin
    for vInt := 0 to Pred(vArray.Count) do
      FDepedancy.Add(vArray.Items[vInt].AsString);
  end;
end;

constructor TDatabase.Create(AOwner: TPackages);
begin
  inherited Create;
  FOwner := AOwner;
  FInstall := TInstall.Create;
  FDepedancy := TStringList.Create;
end;

destructor TDatabase.Destroy;
begin
  FreeAndNil(FInstall);
  FreeAndNil(FDepedancy);
  inherited Destroy;
end;

{ TDatabasesList }

function TDatabasesList.GetAsJSON: TJSONArray;
begin
  Result := nil;
end;

function TDatabasesList.GetDatabase(AIndex : integer): TDatabase;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FList.Count) then
    Result := TDatabase(FList.Items[AIndex]);
end;

procedure TDatabasesList.SetAsJSON(AValue: TJSONArray);
var
  vInt: integer;
begin
  if AValue = nil then
    Exit;

  for vInt := 0 to Pred(AValue.Count) do
    NewDatabase.AsJSON := TJSONObject(AValue.Items[vInt]);
end;

constructor TDatabasesList.Create(AOwner: TPackages);
begin
  inherited Create;
  FOwner := AOwner;
  FList := TList.Create;
end;

destructor TDatabasesList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TDatabasesList.Clear;
begin
  while FList.Count > 0 do
  begin
    TObject(FList.Items[FList.Count - 1]).Free;
    FList.Delete(FList.Count - 1);
  end;
end;

function TDatabasesList.NewDatabase: TDatabase;
begin
  Result := TDatabase.Create(FOwner);
  FList.Add(Result);
end;

function TDatabasesList.Count: integer;
begin
  Result := FList.Count;
end;

{ TDatabases }

function TDatabases.GetAsJSON: TJSONObject;
begin
  Result := nil;
end;

procedure TDatabases.SetAsJSON(AValue: TJSONObject);
begin
  if AValue = nil then
    Exit;

  FName := AValue.Get('name', '');
  FInstall.AsJSON := AValue.Get('install', TJSONObject(nil));
  FPackages.AsJSON := AValue.Get('packages', TJSONArray(nil));;
end;

constructor TDatabases.Create(AOwner: TPackages);
begin
  inherited Create;
  FOwner := AOwner;
  FInstall := TInstall.Create;
  FPackages := TDatabasesList.Create(FOwner);
end;

destructor TDatabases.Destroy;
begin
  FreeAndNil(FInstall);
  FreeAndNil(FPackages);
  inherited Destroy;
end;

{ TCompression }

function TCompression.GetAsJSON: TJSONObject;
begin
  Result := nil;
end;

procedure TCompression.SetAsJSON(AValue: TJSONObject);
var
  vArray: TJSONArray;
  vInt: Integer;
begin
  if AValue = nil then
    Exit;

  FName := AValue.Get('name', '');
  FInstall.AsJSON := AValue.Get('install', TJSONObject(nil));

  vArray := AValue.Get('depedancy', TJSONArray(nil));
  if vArray <> nil then
  begin
    for vInt := 0 to Pred(vArray.Count) do
      FDepedancy.Add(vArray.Items[vInt].AsString);
  end;
end;

constructor TCompression.Create(AOwner: TPackages);
begin
  inherited Create;
  FOwner := AOwner;
  FInstall := TInstall.Create;
  FDepedancy := TStringList.Create;
end;

destructor TCompression.Destroy;
begin
  FreeAndNil(FInstall);
  FreeAndNil(FDepedancy);
  inherited Destroy;
end;

{ TCompressionList }

function TCompressionList.GetAsJSON: TJSONArray;
begin
  Result := nil;
end;

function TCompressionList.GetCompression(AIndex : integer): TCompression;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FList.Count) then
    Result := TCompression(FList.Items[AIndex]);
end;

procedure TCompressionList.SetAsJSON(AValue: TJSONArray);
var
  vInt: integer;
begin
  if AValue = nil then
    Exit;

  for vInt := 0 to Pred(AValue.Count) do
    NewCompression.AsJSON := TJSONObject(AValue.Items[vInt]);
end;

constructor TCompressionList.Create(AOwner: TPackages);
begin
  inherited Create;
  FOwner := AOwner;
  FList := TList.Create;
end;

destructor TCompressionList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TCompressionList.Clear;
begin
  while FList.Count > 0 do
  begin
    TObject(FList.Items[FList.Count - 1]).Free;
    FList.Delete(FList.Count - 1);
  end;
end;

function TCompressionList.NewCompression: TCompression;
begin
  Result := TCompression.Create(FOwner);
  FList.Add(Result);
end;

function TCompressionList.Count: integer;
begin
  Result := FList.Count;
end;

{ TPackages }

procedure TPackages.SetSelected(AValue: boolean);
begin
  if FSelected = AValue then
    Exit;
  FSelected := True;
end;

function TPackages.GetAsJSON: TJSONObject;
begin
  Result := nil;
end;

procedure TPackages.SetAsJSON(AValue: TJSONObject);
begin
  if AValue = nil then
    Exit;

  FName := AValue.Get('name', '');
  FRepository := AValue.Get('repository', '');
  FRepoVersion := AValue.Get('repo-version', '');
  FFolder := AValue.Get('folder', '');
  FInstall.AsJSON := AValue.Get('install', TJSONObject(nil));
  FEngines.AsJSON := AValue.Get('engines', TJSONArray(nil));
  FDatabases.AsJSON := AValue.Get('databases', TJSONObject(nil));
  FCompression.AsJSON := AValue.Get('compression', TJSONArray(nil));
end;

constructor TPackages.Create;
begin
  inherited;
  FInstall := TInstall.Create;
  FEngines := TEngineList.Create(Self);
  FDatabases := TDatabases.Create(Self);
  FCompression := TCompressionList.Create(Self);
  FSelected := True;
end;

destructor TPackages.Destroy;
begin
  FreeAndNil(FInstall);
  FreeAndNil(FEngines);
  FreeAndNil(FDatabases);
  FreeAndNil(FCompression);
  inherited Destroy;
end;

{ TDownload }

function TDownload.GetAsJSON: TJSONObject;
begin
  Result := nil;
end;

procedure TDownload.SetAsJSON(AValue: TJSONObject);
begin
  if AValue = nil then
    Exit;

  FLink := AValue.Get('link', '');
  FFormat := AValue.Get('format', '');
  FFolder := AValue.Get('folder', '');
end;

{ TDownloadList }

function TDownloadList.GetDownload(AIndex : integer): TDownload;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FList.Count) then
    Result := TDownload(FList.Items[AIndex]);
end;

function TDownloadList.GetAsJSON: TJSONArray;
begin
  Result := nil;
end;

procedure TDownloadList.SetAsJSON(AValue: TJSONArray);
var
  vInt: integer;
begin
  if AValue = nil then
    Exit;

  for vInt := 0 to Pred(AValue.Count) do
    NewDownload.AsJSON := TJSONObject(AValue.Items[vInt]);
end;

constructor TDownloadList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TDownloadList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TDownloadList.Clear;
begin
  while FList.Count > 0 do
  begin
    TObject(FList.Items[FList.Count - 1]).Free;
    FList.Delete(FList.Count - 1);
  end;
end;

function TDownloadList.NewDownload: TDownload;
begin
  Result := TDownload.Create;
  FList.Add(Result);
end;

function TDownloadList.Count: integer;
begin
  Result := FList.Count;
end;

{ TDepedancy }

function TDepedancy.GetAsJSON: TJSONObject;
begin
  Result := nil;
end;

procedure TDepedancy.SetAsJSON(AValue: TJSONObject);
begin
  if AValue = nil then
    Exit;

  FName := AValue.Get('name', '');
  FRepository := AValue.Get('repository', '');
  FRepoVersion := AValue.Get('repo-version', '');
  FFolder := AValue.Get('folder', '');
  FInstall.AsJSON := AValue.Get('install', TJSONObject(nil));
  FDownloads.AsJSON := AValue.Get('downloads', TJSONArray(nil));
end;

constructor TDepedancy.Create;
begin
  inherited;
  FInstall := TInstall.Create;
  FDownloads := TDownloadList.Create;
end;

destructor TDepedancy.Destroy;
begin
  FreeAndNil(FInstall);
  FreeAndNil(FDownloads);
  inherited Destroy;
end;

{ TDepedancyList }

function TDepedancyList.GetAsJSON: TJSONArray;
begin
  Result := nil;
end;

function TDepedancyList.GetDepedancy(AIndex : integer): TDepedancy;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FList.Count) then
    Result := TDepedancy(FList.Items[AIndex]);
end;

procedure TDepedancyList.SetAsJSON(AValue: TJSONArray);
var
  vInt: integer;
begin
  if AValue = nil then
    Exit;

  for vInt := 0 to Pred(AValue.Count) do
    NewDepedancy.AsJSON := TJSONObject(AValue.Items[vInt]);
end;

constructor TDepedancyList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TDepedancyList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TDepedancyList.Clear;
begin
  while FList.Count > 0 do
  begin
    TObject(FList.Items[FList.Count - 1]).Free;
    FList.Delete(FList.Count - 1);
  end;
end;

function TDepedancyList.NewDepedancy: TDepedancy;
begin
  Result := TDepedancy.Create;
  FList.Add(Result);
end;

function TDepedancyList.Count: integer;
begin
  Result := FList.Count;
end;

{ TInstaller }

function TInstaller.downloadRepository(ARepository, AVersion: string): TStream;
var
  vGit : TGitHubAPI;
begin
  Result := nil;

  vGit := TGitHubAPI.Create;
  try
    vGit.Repository := ARepository;
    Result := vGit.DownloadZipRepository(AVersion);
  finally
    FreeAndNil(vGit);
  end;
end;

function TInstaller.downloadLink(ALink: string): TStream;
var
  vHttp: THttpClient;
begin
  vHttp := THttpClient.Create;
  try
    Result := vHttp.DownloadLink(ALink);
  finally
    FreeAndNil(vHttp);
  end;
end;

procedure TInstaller.descompressRepository(AFile, APath, AFolder: string);
var
  vUnZip: TRALUnZipper;
begin
  APath := IncludeTrailingPathDelimiter(APath);

  vUnZip := TRALUnZipper.Create;
  try
    vUnZip.FileName := AFile;
    vUnZip.OutputPath := APath;
    vUnZip.Folder := AFolder;
    vUnZip.UnZipAllFiles;
  finally
    FreeAndNil(vUnZip);
  end;
end;

procedure TInstaller.descompressRepository(AStream: TStream; APath, AFolder: string);
var
  vUnZip: TRALUnZipper;
begin
  APath := IncludeTrailingPathDelimiter(APath);

  vUnZip := TRALUnZipper.Create;
  try
    vUnZip.ZipStream := AStream;
    vUnZip.OutputPath := APath;
    vUnZip.Folder := AFolder;
    vUnZip.UnZipAllFiles;
  finally
    FreeAndNil(vUnZip);
  end;
end;

procedure TInstaller.downloadToFolder(ALink, AFormat, AFolder, APath: string);
var
  vStream: TStream;
  vFileStream: TFileStream;
  vFile, vOutput: string;
  vTGZ : TTGZDecompress;
begin
  vStream := downloadLink(ALink);
  try
    if SameText(AFormat, 'tar_gzip') then
    begin
      vFile := APath + 'temp.tgz';
      vFileStream := TFileStream.Create(vFile, fmCreate);
      try
        vFileStream.CopyFrom(vStream, vStream.Size);
      finally
        FreeAndNil(vFileStream);
      end;

      vOutput := IncludeTrailingPathDelimiter(APath) + AFolder;
      vOutput := IncludeTrailingPathDelimiter(vOutput);

      vTGZ := TTGZDecompress.Create(vFile);
      try
        vTGZ.OutputFolder := vOutput;
        vTGZ.ExtractAll;
      finally
        FreeAndNil(vTGZ);
      end;

      DeleteFile(vFile);
    end;
  finally
    FreeAndNil(vStream);
  end;
end;

function TInstaller.logar(ALog: TMemo; AStr: string): boolean;
begin
  Result := ALog <> nil;
  if Result then
  begin
    ALog.Lines.Add(AStr);
    Application.ProcessMessages;
  end;
end;

constructor TInstaller.Create;
var
  vPath, vStrJSON: String;
  vStream: TStream;
  vJSON: TJSONObject;
begin
  inherited;
  FPackages := TPackages.Create;
  FDepedancies := TDepedancyList.Create;

  vStrJSON := '';

  {$IFDEF INSTALL_TEST}
    vPath := ExtractFilePath(ParamStr(0));
    vPath := IncludeTrailingPathDelimiter(vPath);
    vPath := vPath + 'tests/install2.json';
    if FileExists(vPath) then
    begin
      vStream := TMemoryStream.Create;
      try
        TMemoryStream(vStream).LoadFromFile(vPath);
        vStream.Position := 0;

        SetLength(vStrJSON, vStream.Size);
        vStream.Read(vStrJSON[1], vStream.Size);
      finally
        FreeAndNil(vStream);
      end;
    end;
  {$ELSE}
    // baixar do github
  {$ENDIF}

  vJSON := TJSONObject(GetJSON(vStrJSON));
  try
    FVersion := vJSON.Get('version', 1);
    FEula := vJSON.Get('eula', '');

    FPackages.AsJSON := vJSON.Get('packages', TJSONObject(nil));
    FDepedancies.AsJSON := vJSON.Get('depedancies', TJSONArray(nil));
  finally
    FreeAndNil(vJSON);
  end;
end;

destructor TInstaller.Destroy;
begin
  FreeAndNil(FPackages);
  inherited Destroy;
end;

function TInstaller.getDepedancy(ADepedancy: string): TDepedancy;
var
  vInt : integer;
begin
  Result := nil;
  for vInt := 0 to Pred(FDepedancies.Count) do
  begin
    if SameText(FDepedancies.Depedancy[vInt].Name, ADepedancy) then begin
      Result := FDepedancies.Depedancy[vInt];
      Break;
    end;
  end;
end;

procedure TInstaller.download(APath: string; ALog: TMemo);
var
  vFile: String;
  vInt, vInt1: integer;
  vStream: TStream;
begin
  APath := IncludeTrailingPathDelimiter(APath);
  ForceDirectories(APath);

  logar(ALog, 'Downloading Packages');
  logar(ALog, 'Downloading: '+FPackages.Name);

  {$IFDEF INSTALL_TEST}
    vFile := ExtractFilePath(ParamStr(0));
    vFile := IncludeTrailingPathDelimiter(vFile);
    vFile := vFile + 'tests/pascalral.zip';
    if FileExists(vFile) then
      descompressRepository(vFile, APath, 'pascalral');
  {$ELSE}
    vStream := downloadRepository(FPackages.Repository, FPackages.RepoVersion);
    descompressRepository(vStream, APath, 'pascalral');
  {$ENDIF}

  for vInt := 0 to Pred(FDepedancies.Count) do
  begin
    if (FDepedancies.Depedancy[vInt].Selected) and
       (FDepedancies.Depedancy[vInt].Repository <> '') then
    begin
      logar(ALog, 'Downloading: '+FDepedancies.Depedancy[vInt].Name);

      vStream := downloadRepository(FDepedancies.Depedancy[vInt].Repository,
                                    FDepedancies.Depedancy[vInt].RepoVersion);
      descompressRepository(vStream, APath, FDepedancies.Depedancy[vInt].Folder);

      for vInt1 := 0 to Pred(FDepedancies.Depedancy[vInt].Downloads.Count) do
      begin
        logar(ALog, 'Downloading Link: '+FDepedancies.Depedancy[vInt].Downloads.Download[vInt1].Link);

        downloadToFolder(FDepedancies.Depedancy[vInt].Downloads.Download[vInt1].Link,
                         FDepedancies.Depedancy[vInt].Downloads.Download[vInt1].Format,
                         FDepedancies.Depedancy[vInt].Downloads.Download[vInt1].Folder,
                         APath);
      end;
    end;
  end;
  logar(ALog, '');
end;

function TInstaller.checkDepedancy(ALog: TMemo): boolean;
var
  vInt1, vInt2: Integer;
  vLst : TStringList;
  vDepedancy : TDepedancy;
  vAux1 : string;
begin
  Result := False;

  vLst := TStringList.Create;
  try
    // pegando dependencias do engines packges
    for vInt1 := 0 to Pred(FPackages.Engines.Count) do
    begin
      if FPackages.Engines.Engine[vInt1].Selected then
      begin
        // pegando as dependencias
        for vInt2 := 0 to Pred(FPackages.Engines.Engine[vInt1].Depedancy.Count) do
        begin
          vAux1 := FPackages.Engines.Engine[vInt1].Depedancy.Strings[vInt2];
          if vLst.IndexOf(vAux1) < 0 then
            vLst.Add(vAux1);
        end;
      end;
    end;

    // pegando dependencias do databases packages
    for vInt1 := 0 to Pred(FPackages.Databases.Packages.Count) do
    begin
      if FPackages.Databases.Packages.Database[vInt1].Selected then
      begin
        // pegando as dependencias
        for vInt2 := 0 to Pred(FPackages.Databases.Packages.Database[vInt1].Depedancy.Count) do begin
          vAux1 := FPackages.Databases.Packages.Database[vInt1].Depedancy.Strings[vInt2];
          if vLst.IndexOf(vAux1) < 0 then
            vLst.Add(vAux1);
        end;
      end;
    end;

    // pegando dependencias do compression packges
    for vInt1 := 0 to Pred(FPackages.Compression.Count) do
    begin
      if FPackages.Compression.Compression[vInt1].Selected then
      begin
        // pegando as dependencias
        for vInt2 := 0 to Pred(FPackages.Compression.Compression[vInt1].Depedancy.Count) do begin
          vAux1 := FPackages.Compression.Compression[vInt1].Depedancy.Strings[vInt2];
          if vLst.IndexOf(vAux1) < 0 then
            vLst.Add(vAux1);
        end;
      end;
    end;

    // pegando a dependencia da lista e adicionando e criando uma lista do package
    for vInt1 := 0 to Pred(vLst.Count) do
    begin
      vDepedancy := getDepedancy(vLst.Strings[vInt1]);
      if (vDepedancy <> nil) and (not vDepedancy.Selected) then begin
        vAux1 := 'Dependência: '+vDepedancy.Name+' não selecionada';
        if not logar(ALog, vAux1) then
          ShowMessage(vAux1);
        Exit;
      end
    end;

    // verificando se alguma database foi selecionado pra install o RALDB
    if not FPackages.Databases.Selected then
    begin
      for vInt1 := 0 to Pred(FPackages.Databases.Packages.Count) do
      begin
        if FPackages.Databases.Packages.Database[vInt1].Selected then
        begin
          vAux1 := 'Pacote: '+FPackages.Databases.Name+' não selecionado';
          if not logar(ALog, vAux1) then
            ShowMessage(vAux1);
          Exit;
        end;
      end;
    end;

    Result := True;
  finally
    FreeAndNil(vLst);
  end;
end;

end.

