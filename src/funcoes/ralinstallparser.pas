unit ralinstallparser;

{$mode ObjFPC}{$H+}{$M+}

interface

uses
  Classes, SysUtils, fpJSON, fpjsonrtti, Contnrs, ghrepofunctions;

const
  FILEPATH = '\install.json';

type

  { TJSONLanguage }

  TJSONLanguage = class(TJSONObject)
  private
    FName: string;
    FPicture: string;
    FPoFile: string;
    procedure SetName(AValue: string);
    procedure SetPicture(AValue: string);
    procedure SetPoFile(AValue: string);
  public
    property Name: string read FName write SetName;
    property Picture: string read FPicture write SetPicture;
    property PoFile: string read FPoFile write SetPoFile;
  end;

  { TJSONDPK }

  TJSONDPK = class(TJSONObject)
  private
    FIDEVersion: string;
    FLocation: string;
    FName: string;
    FVersionCode: integer;
    procedure SetIDEVersion(AValue: string);
    procedure SetLocation(AValue: string);
    procedure SetName(AValue: string);
    procedure SetVersionCode(AValue: integer);
  public
    property IDEVersion: string read FIDEVersion write SetIDEVersion;
    property Location: string read FLocation write SetLocation;
    property Name: string read FName write SetName;
    property VersionCode: integer read FVersionCode write SetVersionCode;
  end;

  { TJSONInstallOrder }

  TJSONInstallOrder = class(TJSONObject)
  private
    FIsCompile: boolean;
    FIsInstall: boolean;
    FName: string;
    procedure SetIsCompile(AValue: boolean);
    procedure SetIsInstall(AValue: boolean);
    procedure SetName(AValue: string);
  public
    property IsCompile: boolean read FIsCompile write SetIsCompile;
    property IsInstall: boolean read FIsInstall write SetIsInstall;
    property Name: string read FName write SetName;
  end;

  { TJSONDepedancy }

  TJSONDepedancy = class(TJSONObject)
  private
    FBasePath: string;
    FDPK: TFPObjectList;
    FHasDPK: boolean;
    FHasLPK: boolean;
    FInstallOrder: TFPObjectList;
    FLibraryPaths: TJSONArray;
    FLpkLocation: string;
    FMinVersion: string;
    FName: string;
    FPaid: boolean;
    FRepository: string;
    procedure SetBasePath(AValue: string);
    procedure SetDPK(AValue: TFPObjectList);
    procedure SetHasDPK(AValue: boolean);
    procedure SetHasLPK(AValue: boolean);
    procedure SetInstallOrder(AValue: TFPObjectList);
    procedure SetLibraryPaths(AValue: TJSONArray);
    procedure SetLpkLocation(AValue: string);
    procedure SetMinVersion(AValue: string);
    procedure SetName(AValue: string);
    procedure SetPaid(AValue: boolean);
    procedure SetRepository(AValue: string);
  public
    property BasePath: string read FBasePath write SetBasePath;
    property DPK: TFPObjectList read FDPK write SetDPK;
    property HasDPK: boolean read FHasDPK write SetHasDPK;
    property HasLPK: boolean read FHasLPK write SetHasLPK;
    property InstallOrder: TFPObjectList read FInstallOrder write SetInstallOrder;
    property LibraryPaths: TJSONArray read FLibraryPaths write SetLibraryPaths;
    property LpkLocation: string read FLpkLocation write SetLpkLocation;
    property MinVersion: string read FMinVersion write SetMinVersion;
    property Name: string read FName write SetName;
    property Paid: boolean read FPaid write SetPaid;
    property Repository: string read FRepository write SetRepository;
  end;

  { TJSONDataEngine }

  TJSONDataEngine = class(TJSONObject)
  private
    FDepedancy: TJSONDepedancy;
    FHasDepedancy: boolean;
    FInstallOrder: TFPObjectList;
    FIsClient: boolean;
    FIsServer: boolean;
    FLibraryPaths: TJSONArray;
    FName: string;
    procedure SetDepedancy(AValue: TJSONDepedancy);
    procedure SetHasDepedancy(AValue: boolean);
    procedure SetInstallOrder(AValue: TFPObjectList);
    procedure SetIsClient(AValue: boolean);
    procedure SetIsServer(AValue: boolean);
    procedure SetLibraryPaths(AValue: TJSONArray);
    procedure SetName(AValue: string);
  public
    property Depedancy: TJSONDepedancy read FDepedancy write SetDepedancy;
    property HasDepedancy: boolean read FHasDepedancy write SetHasDepedancy;
    property InstallOrder: TFPObjectList read FInstallOrder write SetInstallOrder;
    property IsClient: boolean read FIsClient write SetIsClient;
    property IsServer: boolean read FIsServer write SetIsServer;
    property LibraryPaths: TJSONArray read FLibraryPaths write SetLibraryPaths;
    property Name: string read FName write SetName;
  end;

  { TJSONBasePackage }

  TJSONBasePackage = class(TJSONObject)
  private
    FInstallOrder: TFPObjectList;
    FLibraryPaths: TJSONArray;
    FVersion: string;
    procedure SetInstallOrder(AValue: TFPObjectList);
    procedure SetLibraryPaths(AValue: TJSONArray);
    procedure SetVersion(AValue: string);
  public
    property Version: string read FVersion write SetVersion;
    property InstallOrder: TFPObjectList read FInstallOrder write SetInstallOrder;
    property LibraryPaths: TJSONArray read FLibraryPaths write SetLibraryPaths;
  end;

  { TJSONIDE }

  TJSONIDE = class(TJSONObject)
  private
    FBasePackage: TJSONBasePackage;
    FDataEngines: TFPObjectList;
    procedure SetBasePackage(AValue: TJSONBasePackage);
    procedure SetDataEngines(AValue: TFPObjectList);
  public
    property BasePackage: TJSONBasePackage read FBasePackage write SetBasePackage;
    property DataEngines: TFPObjectList read FDataEngines write SetDataEngines;
  end;

  { TJSONInstaller }

  TJSONInstaller = class
  private
    FDelphi: TJSONIDE;
    FEula: string;
    FLanguages: TFPObjectList;
    FLazarus: TJSONIDE;
    FVersion: integer;
    procedure SetDelphi(AValue: TJSONIDE);
    procedure SetEula(AValue: string);
    procedure SetLanguages(AValue: TFPObjectList);
    procedure SetLazarus(AValue: TJSONIDE);
    procedure SetVersion(AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;
    property Version: integer read FVersion write SetVersion;
    property Languages: TFPObjectList read FLanguages write SetLanguages;
    property Eula: string read FEula write SetEula;
    property Delphi: TJSONIDE read FDelphi write SetDelphi;
    property Lazarus: TJSONIDE read FLazarus write SetLazarus;
  end;

implementation

{ TJSONInstaller }

procedure TJSONInstaller.SetDelphi(AValue: TJSONIDE);
begin
  if FDelphi = AValue then Exit;
  FDelphi := AValue;
end;

procedure TJSONInstaller.SetEula(AValue: string);
begin
  if FEula = AValue then Exit;
  FEula := AValue;
end;

procedure TJSONInstaller.SetLanguages(AValue: TFPObjectList);
begin
  if FLanguages = AValue then Exit;
  FLanguages := AValue;
end;

procedure TJSONInstaller.SetLazarus(AValue: TJSONIDE);
begin
  if FLazarus = AValue then Exit;
  FLazarus := AValue;
end;

procedure TJSONInstaller.SetVersion(AValue: integer);
begin
  if FVersion = AValue then Exit;
  FVersion := AValue;
end;

constructor TJSONInstaller.Create;
var
  HTTPClient: TRESTClient;
  fs: TFileStream;
  json: TStringStream;
  jsonds: TJSONDeStreamer;
  teste: string;
begin
  FLanguages := TFPObjectList.Create;
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
  finally
    HTTPClient.Free;
  end;
end;

destructor TJSONInstaller.Destroy;
begin
  FLanguages.Free;
  FDelphi.Free;
  FLazarus.Free;
  inherited Destroy;
end;

{ TJSONIDE }

procedure TJSONIDE.SetBasePackage(AValue: TJSONBasePackage);
begin
  if FBasePackage = AValue then Exit;
  FBasePackage := AValue;
end;

procedure TJSONIDE.SetDataEngines(AValue: TFPObjectList);
begin
  if FDataEngines = AValue then Exit;
  FDataEngines := AValue;
end;

{ TJSONBasePackage }

procedure TJSONBasePackage.SetInstallOrder(AValue: TFPObjectList);
begin
  if FInstallOrder = AValue then Exit;
  FInstallOrder := AValue;
end;

procedure TJSONBasePackage.SetLibraryPaths(AValue: TJSONArray);
begin
  if FLibraryPaths = AValue then Exit;
  FLibraryPaths := AValue;
end;

procedure TJSONBasePackage.SetVersion(AValue: string);
begin
  if FVersion = AValue then Exit;
  FVersion := AValue;
end;

{ TJSONDataEngine }

procedure TJSONDataEngine.SetDepedancy(AValue: TJSONDepedancy);
begin
  if FDepedancy = AValue then Exit;
  FDepedancy := AValue;
end;

procedure TJSONDataEngine.SetHasDepedancy(AValue: boolean);
begin
  if FHasDepedancy = AValue then Exit;
  FHasDepedancy := AValue;
end;

procedure TJSONDataEngine.SetInstallOrder(AValue: TFPObjectList);
begin
  if FInstallOrder = AValue then Exit;
  FInstallOrder := AValue;
end;

procedure TJSONDataEngine.SetIsClient(AValue: boolean);
begin
  if FIsClient = AValue then Exit;
  FIsClient := AValue;
end;

procedure TJSONDataEngine.SetIsServer(AValue: boolean);
begin
  if FIsServer = AValue then Exit;
  FIsServer := AValue;
end;

procedure TJSONDataEngine.SetLibraryPaths(AValue: TJSONArray);
begin
  if FLibraryPaths = AValue then Exit;
  FLibraryPaths := AValue;
end;

procedure TJSONDataEngine.SetName(AValue: string);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

{ TJSONDepedancy }

procedure TJSONDepedancy.SetBasePath(AValue: string);
begin
  if FBasePath = AValue then Exit;
  FBasePath := AValue;
end;

procedure TJSONDepedancy.SetDPK(AValue: TFPObjectList);
begin
  if FDPK = AValue then Exit;
  FDPK := AValue;
end;

procedure TJSONDepedancy.SetHasDPK(AValue: boolean);
begin
  if FHasDPK = AValue then Exit;
  FHasDPK := AValue;
end;

procedure TJSONDepedancy.SetHasLPK(AValue: boolean);
begin
  if FHasLPK = AValue then Exit;
  FHasLPK := AValue;
end;

procedure TJSONDepedancy.SetInstallOrder(AValue: TFPObjectList);
begin
  if FInstallOrder = AValue then Exit;
  FInstallOrder := AValue;
end;

procedure TJSONDepedancy.SetLibraryPaths(AValue: TJSONArray);
begin
  if FLibraryPaths = AValue then Exit;
  FLibraryPaths := AValue;
end;

procedure TJSONDepedancy.SetLpkLocation(AValue: string);
begin
  if FLpkLocation = AValue then Exit;
  FLpkLocation := AValue;
end;

procedure TJSONDepedancy.SetMinVersion(AValue: string);
begin
  if FMinVersion = AValue then Exit;
  FMinVersion := AValue;
end;

procedure TJSONDepedancy.SetName(AValue: string);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

procedure TJSONDepedancy.SetPaid(AValue: boolean);
begin
  if FPaid = AValue then Exit;
  FPaid := AValue;
end;

procedure TJSONDepedancy.SetRepository(AValue: string);
begin
  if FRepository = AValue then Exit;
  FRepository := AValue;
end;

{ TJSONInstallOrder }

procedure TJSONInstallOrder.SetIsCompile(AValue: boolean);
begin
  if FIsCompile = AValue then Exit;
  FIsCompile := AValue;
end;

procedure TJSONInstallOrder.SetIsInstall(AValue: boolean);
begin
  if FIsInstall = AValue then Exit;
  FIsInstall := AValue;
end;

procedure TJSONInstallOrder.SetName(AValue: string);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

{ TJSONDPK }

procedure TJSONDPK.SetIDEVersion(AValue: string);
begin
  if FIDEVersion = AValue then Exit;
  FIDEVersion := AValue;
end;

procedure TJSONDPK.SetLocation(AValue: string);
begin
  if FLocation = AValue then Exit;
  FLocation := AValue;
end;

procedure TJSONDPK.SetName(AValue: string);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

procedure TJSONDPK.SetVersionCode(AValue: integer);
begin
  if FVersionCode = AValue then Exit;
  FVersionCode := AValue;
end;

{ TJSONLanguage }

procedure TJSONLanguage.SetName(AValue: string);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

procedure TJSONLanguage.SetPicture(AValue: string);
begin
  if FPicture = AValue then Exit;
  FPicture := AValue;
end;

procedure TJSONLanguage.SetPoFile(AValue: string);
begin
  if FPoFile = AValue then Exit;
  FPoFile := AValue;
end;

end.
