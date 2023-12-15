unit ralinstallparser;

{$mode ObjFPC}{$H+}{$M+}

interface

uses
  Classes, SysUtils, fpJSON, fpjsonrtti, ghrepofunctions;

const
  FILEPATH = '\install.json';

type

  { TJSONLanguage }

  TJSONLanguage = class(TCollectionItem)
  private
    FName: string;
    FPicture: string;
    FPoFile: string;
  published
    property Name: string read FName write FName;
    property Picture: string read FPicture write FPicture;
    property PoFile: string read FPoFile write FPoFile;
  end;

  { TJSONDPK }

  TJSONDPK = class(TCollectionItem)
  private
    FIDEVersion: string;
    FLocation: string;
    FName: string;
    FVersionCode: integer;
  published
    property IDEVersion: string read FIDEVersion write FIDEVersion;
    property Location: string read FLocation write FLocation;
    property Name: string read FName write FName;
    property VersionCode: integer read FVersionCode write FVersionCode;
  end;

  { TJSONInstallOrder }

  TJSONInstallOrder = class(TCollectionItem)
  private
    FIsCompile: boolean;
    FIsInstall: boolean;
    FName: string;
  published
    property IsCompile: boolean read FIsCompile write FIsCompile;
    property IsInstall: boolean read FIsInstall write FIsInstall;
    property Name: string read FName write FName;
  end;

  { TJSONDepedancy }

  TJSONDepedancy = class(TPersistent)
  private
    FBasePath: string;
    FDPK: TCollection;
    FHasDPK: boolean;
    FHasLPK: boolean;
    FInstallOrder: TCollection;
    FLibraryPaths: TStrings;
    FLPKLocation: string;
    FMinVersion: string;
    FName: string;
    FPaid: boolean;
    FRepository: string;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property BasePath: string read FBasePath write FBasePath;
    property DPK: TCollection read FDPK;
    property HasDPK: boolean read FHasDPK write FHasDPK;
    property HasLPK: boolean read FHasLPK write FHasLPK;
    property InstallOrder: TCollection read FInstallOrder;
    property LibraryPaths: TStrings read FLibraryPaths;
    property LPKLocation: string read FLPKLocation write FLPKLocation;
    property MinVersion: string read FMinVersion write FMinVersion;
    property Name: string read FName write FName;
    property Paid: boolean read FPaid write FPaid;
    property Repository: string read FRepository write FRepository;
  end;

  { TJSONDataEngine }

  TJSONDataEngine = class(TCollectionItem)
  private
    FDepedancy: TJSONDepedancy;
    FHasDepedancy: boolean;
    FInstallOrder: TCollection;
    FIsClient: boolean;
    FIsServer: boolean;
    FLibraryPaths: TStrings;
    FName: string;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Depedancy: TJSONDepedancy read FDepedancy;
    property HasDepedancy: boolean read FHasDepedancy write FHasDepedancy;
    property InstallOrder: TCollection read FInstallOrder;
    property IsClient: boolean read FIsClient write FIsClient;
    property IsServer: boolean read FIsServer write FIsServer;
    property LibraryPaths: TStrings read FLibraryPaths;
    property Name: string read FName write FName;
  end;

  { TJSONBasePackage }

  TJSONBasePackage = class(TPersistent)
  private
    FInstallOrder: TCollection;
    FLibraryPaths: TStrings;
    FVersion: string;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Version: string read FVersion write FVersion;
    property InstallOrder: TCollection read FInstallOrder;
    property LibraryPaths: TStrings read FLibraryPaths;
  end;

  { TJSONIDE }

  TJSONIDE = class(TPersistent)
  private
    FBasePackage: TJSONBasePackage;
    FDataEngines: TCollection;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property BasePackage: TJSONBasePackage read FBasePackage;
    property DataEngines: TCollection read FDataEngines;
  end;

  { TJSONInstaller }

  TJSONInstaller = class
  private
    FDelphi: TJSONIDE;
    FEula: string;
    FLanguages: TCollection;
    FLazarus: TJSONIDE;
    FVersion: integer;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Version: integer read FVersion write FVersion;
    property Languages: TCollection read FLanguages;
    property Eula: string read FEula write FEula;
    property Delphi: TJSONIDE read FDelphi;
    property Lazarus: TJSONIDE read FLazarus;
  end;

implementation

{ TJSONInstaller }

constructor TJSONInstaller.Create;
var
  HTTPClient: TRESTClient;
  fs: TFileStream;
  json: TStringStream;
  jsonds: TJSONDeStreamer;
  teste: string;
begin
  FLanguages := TCollection.Create(TJSONLanguage);
  FDelphi := TJSONIDE.Create;
  FLazarus := TJSONIDE.Create;

  HTTPClient := TRESTClient.Create;
  try
    teste := ExtractFilePath(ParamStr(0)) + 'install.json';
    fs := TFileStream.Create(teste, fmOpenRead);
    //fs := HTTPClient.getFileStream('install.json');
    json := TStringStream.Create;
    json.CopyFrom(fs, fs.size);
    fs.Free;
    jsonds := TJSONDeStreamer.Create(nil);
    jsonds.JSONToObject(json.DataString, Self);
    jsonds.Free;
    json.Free;

    teste := TJSONLanguage(FLanguages.Items[0]).Name;
  finally
    HTTPClient.Free;
  end;
end;

destructor TJSONInstaller.Destroy;
begin
  FreeAndNil(FLanguages);
  FreeAndNil(FDelphi);
  FreeAndNil(FLazarus);
  inherited Destroy;
end;

{ TJSONIDE }

constructor TJSONIDE.Create;
begin
  FBasePackage := TJSONBasePackage.Create;
  FDataEngines := TCollection.Create(TJSONDataEngine);
end;

destructor TJSONIDE.Destroy;
begin
  FreeAndNil(FBasePackage);
  FreeAndNil(FDataEngines);
  inherited Destroy;
end;

{ TJSONBasePackage }

constructor TJSONBasePackage.Create;
begin
  FInstallOrder := TCollection.Create(TJSONInstallOrder);
  FLibraryPaths := TStringList.Create;
end;

destructor TJSONBasePackage.Destroy;
begin
  inherited Destroy;
end;

{ TJSONDataEngine }

constructor TJSONDataEngine.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FDepedancy := TJSONDepedancy.Create;
  FInstallOrder := TCollection.Create(TJSONInstallOrder);
  FLibraryPaths := TStringList.Create;
end;

destructor TJSONDataEngine.Destroy;
begin
  FreeAndNil(FDepedancy);
  FreeAndNil(FInstallOrder);
  FreeAndNil(FLibraryPaths);
  inherited Destroy;
end;

{ TJSONDepedancy }

constructor TJSONDepedancy.Create;
begin
  FDPK := TCollection.Create(TJSONDPK);
  FInstallOrder := TCollection.Create(TJSONInstallOrder);
  FLibraryPaths := TStringList.Create;
end;

destructor TJSONDepedancy.Destroy;
begin
  FreeAndNil(FDPK);
  FreeAndNil(FInstallOrder);
  FreeAndNil(FLibraryPaths);
  inherited Destroy;
end;

end.
