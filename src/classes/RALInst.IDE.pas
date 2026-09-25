/// IDE installations found on the machine, as the rest of the installer sees
/// them: where they are, which version, where they keep their configuration and
/// which platforms they can compile for. LCL-free: the discovery runs the same
/// in the GUI, in the CLI and in the tests.
unit RALInst.IDE;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  /// Kind of IDE.
  TTipoIDE = (tiDelphi, tiLazarus);

  /// One Delphi release, as data: the jump from BDS 23.0 to 37.0 is why this
  /// cannot be a chain of ifs.
  TDelphiProduto = record
    /// BDS version, '23.0'; empty on Delphi 7
    BDS: string;
    /// Compiler symbol, 'VER360'
    Compilador: string;
    /// Name for humans, 'Delphi 12 Athens'
    Nome: string;
    /// Registry root, '\Software\Embarcadero\BDS'
    RegBase: string;
    /// Package suffix of the IDE's own .bpl files, '290'
    Sufixo: string;
    /// Compiler version, '36.0', used to sort and compare
    VersaoNum: string;
  end;

  /// Where an installation came from; the same folder found twice keeps the
  /// strongest origin (registry > disk > manual).
  TOrigemIDE = (oiRegistro, oiDisco, oiManual);

  /// What an installation can do.
  TCapacidadeIDE = (
    /// has a command-line compiler (dcc32, lazbuild)
    ciCompilar,
    /// has the IDE itself: design-time packages make sense
    ciInstalarNaIDE,
    /// sources can be pointed to in the library path
    ciLibraryPath
  );
  TCapacidadesIDE = set of TCapacidadeIDE;

  /// Called for every folder a search visits; ACancelar stops the search.
  TIDEBuscaEvento = procedure(const APasta: string; var ACancelar: boolean) of object;

  /// One IDE installation.
  TIDEInstance = class
  private
    FAvisos: TStringList;
    FBDSVersao: string;
    FBuildFile: string;
    FCapacidades: TCapacidadesIDE;
    FCommonDir: string;
    FCompilerFile: string;
    FConfigDir: string;
    FConfigOrigem: string;
    FExeFile: string;
    FIDE64: boolean;
    FNome: string;
    FOrigem: TOrigemIDE;
    FPlataformas: TStringList;
    FRegKey: string;
    FRootDir: string;
    FSufixoPacote: string;
    FTipo: TTipoIDE;
    FVersao: string;
    FVersaoCompilador: string;
    /// Stores the folder in its canonical form (see NormalizarPasta)
    procedure SetRootDir(const AValue: string);
  public
    constructor Create(ATipo: TTipoIDE);
    destructor Destroy; override;
    /// Copies every field of another instance
    procedure Assign(ASource: TIDEInstance);
    /// Name plus the platforms it compiles for
    function Descricao: string;

    /// What the user needs to know before choosing this installation
    property Avisos: TStringList read FAvisos;
    /// Delphi: '23.0'; empty on Delphi 7
    property BDSVersao: string read FBDSVersao write FBDSVersao;
    /// Build tool (dcc32.exe, lazbuild)
    property BuildFile: string read FBuildFile write FBuildFile;
    property Capacidades: TCapacidadesIDE read FCapacidades write FCapacidades;
    /// Delphi: BDSCOMMONDIR (where the user's Bpl and Dcp go)
    property CommonDir: string read FCommonDir write FCommonDir;
    /// Lazarus: the configured FPC compiler
    property CompilerFile: string read FCompilerFile write FCompilerFile;
    /// Lazarus: --primary-config-path; Delphi: empty (its configuration is the
    /// registry)
    property ConfigDir: string read FConfigDir write FConfigDir;
    /// Where ConfigDir was deduced from, for the log
    property ConfigOrigem: string read FConfigOrigem write FConfigOrigem;
    /// IDE executable (bds.exe, delphi32.exe, lazarus.exe); empty when there is
    /// only the compiler
    property ExeFile: string read FExeFile write FExeFile;
    /// Delphi: there is also the 64-bit IDE (bin64\bds.exe, from Delphi 12 on),
    /// which loads design packages compiled for Win64 (Known Packages x64)
    property IDE64: boolean read FIDE64 write FIDE64;
    /// Name for humans: 'Delphi 12 Athens', 'Lazarus 4.6'
    property Nome: string read FNome write FNome;
    property Origem: TOrigemIDE read FOrigem write FOrigem;
    /// Normalized names: win32, win64, linux64, osx64, osxarm64, android,
    /// android64, iosdevice64, iossimarm64 (Delphi); FPC cpu-os (Lazarus)
    property Plataformas: TStringList read FPlataformas;
    /// Delphi: registry key under HKCU, without the root
    property RegKey: string read FRegKey write FRegKey;
    /// Root folder, always with a trailing separator
    property RootDir: string read FRootDir write SetRootDir;
    /// Delphi: '290', suffix of the IDE's .bpl files and of the updated Indy
    property SufixoPacote: string read FSufixoPacote write FSufixoPacote;
    property Tipo: TTipoIDE read FTipo;
    /// Product version: '12', '4.6.0.0'
    property Versao: string read FVersao write FVersao;
    /// Delphi: 'VER360'; Lazarus: the FPC version, when known
    property VersaoCompilador: string read FVersaoCompilador write FVersaoCompilador;
  end;

  /// Installations found, owned by the list.
  TIDEList = class(TObjectList)
  private
    function GetItem(AIndex: integer): TIDEInstance;
  public
    /// Returns the instance kept in the list: the new one, or the one already
    /// there for the same folder (the new one is then freed)
    function Adicionar(AIDE: TIDEInstance): TIDEInstance;
    /// The installation rooted at the folder; nil if none
    function BuscarPorRaiz(const ARootDir: string): TIDEInstance;
    /// Delphi before Lazarus, newest first
    procedure Ordenar;

    property Items[AIndex: integer]: TIDEInstance read GetItem; default;
  end;

  /// Folder scan shared by Delphi and Lazarus: goes down to the requested
  /// depth, stops at an installation (does not enter it) and skips folders
  /// that never hold an IDE.
  TBuscaIDE = class
  private
    FCancelado: boolean;
    FOnBusca: TIDEBuscaEvento;
    FRaizesExtras: TStringList;
  protected
    /// Folders never worth visiting (system, hidden, node_modules...)
    function PastaIgnorada(const ANome: string): boolean; virtual;
    /// Visits a folder and its subfolders
    procedure Varrer(ALista: TIDEList; const APasta: string; AProfundidade: integer);
  public
    constructor Create;
    destructor Destroy; override;
    /// The folder may be an installation or a folder holding several
    /// (D:\IDE\lazarus); returns how many entered the list
    function BuscarEm(ALista: TIDEList; const APasta: string;
      AProfundidade: integer = 3): integer;
    /// Known system roots plus RaizesExtras; fast
    procedure BuscarPadrao(ALista: TIDEList); virtual; abstract;
    /// After the list is built: warnings that depend on comparing
    /// installations
    procedure Finalizar(ALista: TIDEList); virtual;
    /// nil when the folder is not the root of an installation
    function InspecionarPasta(const APasta: string): TIDEInstance; virtual; abstract;

    property Cancelado: boolean read FCancelado write FCancelado;
    property RaizesExtras: TStringList read FRaizesExtras;
    property OnBusca: TIDEBuscaEvento read FOnBusca write FOnBusca;
  end;

const
  /// Every Delphi the installer knows, oldest first
  DelphiProdutos: array[0..21] of TDelphiProduto = (
    (BDS: ''; Compilador: 'VER150'; Nome: 'Delphi 7';
     RegBase: '\Software\Borland\Delphi'; Sufixo: '70'; VersaoNum: '15.0'),
    (BDS: '3.0'; Compilador: 'VER170'; Nome: 'Delphi 2005';
     RegBase: '\Software\Borland\BDS'; Sufixo: '90'; VersaoNum: '17.0'),
    (BDS: '4.0'; Compilador: 'VER180'; Nome: 'Delphi 2006';
     RegBase: '\Software\Borland\BDS'; Sufixo: '100'; VersaoNum: '18.0'),
    (BDS: '5.0'; Compilador: 'VER185'; Nome: 'Delphi 2007';
     RegBase: '\Software\Borland\BDS'; Sufixo: '100'; VersaoNum: '18.5'),
    (BDS: '6.0'; Compilador: 'VER200'; Nome: 'Delphi 2009';
     RegBase: '\Software\CodeGear\BDS'; Sufixo: '120'; VersaoNum: '20.0'),
    (BDS: '7.0'; Compilador: 'VER210'; Nome: 'Delphi 2010';
     RegBase: '\Software\CodeGear\BDS'; Sufixo: '140'; VersaoNum: '21.0'),
    (BDS: '8.0'; Compilador: 'VER220'; Nome: 'Delphi XE';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '150'; VersaoNum: '22.0'),
    (BDS: '9.0'; Compilador: 'VER230'; Nome: 'Delphi XE2';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '160'; VersaoNum: '23.0'),
    (BDS: '10.0'; Compilador: 'VER240'; Nome: 'Delphi XE3';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '170'; VersaoNum: '24.0'),
    (BDS: '11.0'; Compilador: 'VER250'; Nome: 'Delphi XE4';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '180'; VersaoNum: '25.0'),
    (BDS: '12.0'; Compilador: 'VER260'; Nome: 'Delphi XE5';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '190'; VersaoNum: '26.0'),
    (BDS: '14.0'; Compilador: 'VER270'; Nome: 'Delphi XE6';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '200'; VersaoNum: '27.0'),
    (BDS: '15.0'; Compilador: 'VER280'; Nome: 'Delphi XE7';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '210'; VersaoNum: '28.0'),
    (BDS: '16.0'; Compilador: 'VER290'; Nome: 'Delphi XE8';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '220'; VersaoNum: '29.0'),
    (BDS: '17.0'; Compilador: 'VER300'; Nome: 'Delphi 10 Seattle';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '230'; VersaoNum: '30.0'),
    (BDS: '18.0'; Compilador: 'VER310'; Nome: 'Delphi 10.1 Berlin';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '240'; VersaoNum: '31.0'),
    (BDS: '19.0'; Compilador: 'VER320'; Nome: 'Delphi 10.2 Tokyo';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '250'; VersaoNum: '32.0'),
    (BDS: '20.0'; Compilador: 'VER330'; Nome: 'Delphi 10.3 Rio';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '260'; VersaoNum: '33.0'),
    (BDS: '21.0'; Compilador: 'VER340'; Nome: 'Delphi 10.4 Sydney';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '270'; VersaoNum: '34.0'),
    (BDS: '22.0'; Compilador: 'VER350'; Nome: 'Delphi 11 Alexandria';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '280'; VersaoNum: '35.0'),
    (BDS: '23.0'; Compilador: 'VER360'; Nome: 'Delphi 12 Athens';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '290'; VersaoNum: '36.0'),
    (BDS: '37.0'; Compilador: 'VER370'; Nome: 'Delphi 13 Florence';
     RegBase: '\Software\Embarcadero\BDS'; Sufixo: '370'; VersaoNum: '37.0')
  );

/// Compares versions number by number ('4.10' > '4.9'); 'x' and '*' match any
/// number, so '3.2.x' is the highest 3.2
function CompararVersoes(const A, B: string): integer;
/// Name of an origin, for humans
function NomeOrigem(AOrigem: TOrigemIDE): string;
/// Canonical form of a folder, to compare installations
function NormalizarPasta(const APasta: string): string;
/// Index in DelphiProdutos of a BDS version ('23.0'); -1 if unknown
function ProdutoPorBDS(const ABDS: string): integer;
/// Index in DelphiProdutos of 'XE8', '10.1', '12', 'Delphi 12 Athens', '2009',
/// '7'; -1 if unknown. It is how the manifest and the recipes name a Delphi
function ProdutoPorNome(const ANome: string): integer;
/// Index in DelphiProdutos of a package suffix ('290'); -1 if unknown
function ProdutoPorSufixo(const ASufixo: string): integer;

implementation

uses
  RALInst.Mensagens;

function ProdutoPorBDS(const ABDS: string): integer;
var
  vInt: integer;
begin
  Result := -1;
  if ABDS = '' then
    Exit;
  for vInt := Low(DelphiProdutos) to High(DelphiProdutos) do
    if SameText(DelphiProdutos[vInt].BDS, ABDS) then
      Exit(vInt);
end;

function ProdutoPorSufixo(const ASufixo: string): integer;
var
  vInt: integer;
begin
  // 2006 e 2007 dividem o sufixo 100; aqui ganha o primeiro, e quem tem o
  // BDS em maos nao chega a perguntar pelo sufixo
  Result := -1;
  if ASufixo = '' then
    Exit;
  for vInt := Low(DelphiProdutos) to High(DelphiProdutos) do
    if DelphiProdutos[vInt].Sufixo = ASufixo then
      Exit(vInt);
end;

function ProdutoPorNome(const ANome: string): integer;
var
  vInt: integer;
  vNome, vProduto: string;
begin
  Result := -1;
  vNome := Trim(ANome);
  if SameText(Copy(vNome, 1, 7), 'Delphi ') then
    vNome := Trim(Copy(vNome, 8, MaxInt));
  if vNome = '' then
    Exit;
  // 'Delphi 10 Seattle' responde a '10' e a '10 Seattle', mas nao a '10.1'
  for vInt := Low(DelphiProdutos) to High(DelphiProdutos) do
  begin
    vProduto := Copy(DelphiProdutos[vInt].Nome, 8, MaxInt);
    if SameText(vProduto, vNome) or
       SameText(Copy(vProduto, 1, Length(vNome) + 1), vNome + ' ') then
      Exit(vInt);
  end;
end;

function NormalizarPasta(const APasta: string): string;
begin
  Result := '';
  if Trim(APasta) = '' then
    Exit;
  Result := IncludeTrailingPathDelimiter(ExpandFileName(Trim(APasta)));
end;

function CompararVersoes(const A, B: string): integer;
var
  vA, vB: TStringArray;
  vInt, vNa, vNb: integer;
begin
  vA := A.Split(['.', '-', ' ']);
  vB := B.Split(['.', '-', ' ']);
  Result := 0;
  vInt := 0;
  while (Result = 0) and ((vInt < Length(vA)) or (vInt < Length(vB))) do
  begin
    vNa := 0;
    vNb := 0;
    // 'x' e '*' valem qualquer numero: '3.2.x' e o maior 3.2
    if vInt < Length(vA) then
      if (vA[vInt] = 'x') or (vA[vInt] = '*') then
        vNa := MaxInt
      else
        vNa := StrToIntDef(vA[vInt], 0);
    if vInt < Length(vB) then
      if (vB[vInt] = 'x') or (vB[vInt] = '*') then
        vNb := MaxInt
      else
        vNb := StrToIntDef(vB[vInt], 0);
    if vNa < vNb then
      Result := -1
    else if vNa > vNb then
      Result := 1;
    Inc(vInt);
  end;
end;

function NomeOrigem(AOrigem: TOrigemIDE): string;
begin
  Result := '';
  case AOrigem of
    oiRegistro: Result := cmOrigemRegistro;
    oiDisco:    Result := cmOrigemDisco;
    oiManual:   Result := cmOrigemManual;
  end;
end;

{ TIDEInstance }

procedure TIDEInstance.SetRootDir(const AValue: string);
begin
  FRootDir := NormalizarPasta(AValue);
end;

constructor TIDEInstance.Create(ATipo: TTipoIDE);
begin
  inherited Create;
  FTipo := ATipo;
  FOrigem := oiDisco;
  FPlataformas := TStringList.Create;
  FPlataformas.Duplicates := dupIgnore;
  FPlataformas.Sorted := True;
  // a mesma busca pode ser finalizada de novo depois de o usuario apontar
  // uma pasta: aviso repetido nao entra
  FAvisos := TStringList.Create;
  FAvisos.Sorted := True;
  FAvisos.Duplicates := dupIgnore;
end;

destructor TIDEInstance.Destroy;
begin
  FreeAndNil(FPlataformas);
  FreeAndNil(FAvisos);
  inherited Destroy;
end;

procedure TIDEInstance.Assign(ASource: TIDEInstance);
begin
  FTipo := ASource.FTipo;
  FNome := ASource.FNome;
  FVersao := ASource.FVersao;
  FRootDir := ASource.FRootDir;
  FExeFile := ASource.FExeFile;
  FIDE64 := ASource.FIDE64;
  FBuildFile := ASource.FBuildFile;
  FConfigDir := ASource.FConfigDir;
  FConfigOrigem := ASource.FConfigOrigem;
  FCommonDir := ASource.FCommonDir;
  FBDSVersao := ASource.FBDSVersao;
  FRegKey := ASource.FRegKey;
  FSufixoPacote := ASource.FSufixoPacote;
  FVersaoCompilador := ASource.FVersaoCompilador;
  FCompilerFile := ASource.FCompilerFile;
  FOrigem := ASource.FOrigem;
  FCapacidades := ASource.FCapacidades;
  FPlataformas.Assign(ASource.FPlataformas);
  FAvisos.Assign(ASource.FAvisos);
end;

function TIDEInstance.Descricao: string;
begin
  Result := FNome;
  if FPlataformas.Count > 0 then
    Result := Result + ' [' + StringReplace(Trim(FPlataformas.CommaText), ',', ', ',
                                            [rfReplaceAll]) + ']';
end;

{ TIDEList }

function TIDEList.GetItem(AIndex: integer): TIDEInstance;
begin
  Result := TIDEInstance(inherited Items[AIndex]);
end;

function TIDEList.BuscarPorRaiz(const ARootDir: string): TIDEInstance;
var
  vInt: integer;
  vRaiz: string;
begin
  Result := nil;
  vRaiz := NormalizarPasta(ARootDir);
  for vInt := 0 to Pred(Count) do
    if SameFileName(Items[vInt].RootDir, vRaiz) then
      Exit(Items[vInt]);
end;

function TIDEList.Adicionar(AIDE: TIDEInstance): TIDEInstance;
var
  vInt: integer;
begin
  Result := BuscarPorRaiz(AIDE.RootDir);
  if Result = nil then
  begin
    Add(AIDE);
    Exit(AIDE);
  end;

  // mesma pasta achada de novo: fica a origem mais forte e os avisos somados
  if AIDE.Origem < Result.Origem then
    Result.Origem := AIDE.Origem;
  if Result.RegKey = '' then
    Result.RegKey := AIDE.RegKey;
  for vInt := 0 to Pred(AIDE.Avisos.Count) do
    if Result.Avisos.IndexOf(AIDE.Avisos[vInt]) < 0 then
      Result.Avisos.Add(AIDE.Avisos[vInt]);
  AIDE.Free;
end;

function CompararIDE(AItem1, AItem2: Pointer): integer;
var
  vA, vB: TIDEInstance;
begin
  vA := TIDEInstance(AItem1);
  vB := TIDEInstance(AItem2);
  Result := Ord(vA.Tipo) - Ord(vB.Tipo);
  if Result = 0 then
    Result := -CompararVersoes(vA.Versao, vB.Versao);
  if Result = 0 then
    Result := CompareText(vA.RootDir, vB.RootDir);
end;

procedure TIDEList.Ordenar;
begin
  Sort(@CompararIDE);
end;

{ TBuscaIDE }

constructor TBuscaIDE.Create;
begin
  inherited Create;
  FRaizesExtras := TStringList.Create;
  FRaizesExtras.Duplicates := dupIgnore;
end;

destructor TBuscaIDE.Destroy;
begin
  FreeAndNil(FRaizesExtras);
  inherited Destroy;
end;

function TBuscaIDE.PastaIgnorada(const ANome: string): boolean;
begin
  Result := (ANome = '') or (ANome = '.') or (ANome = '..') or
            (ANome[1] = '.') or
            SameText(ANome, '$Recycle.Bin') or
            SameText(ANome, 'System Volume Information') or
            SameText(ANome, 'Windows') or
            SameText(ANome, 'node_modules') or
            SameText(ANome, '__history') or
            SameText(ANome, 'proc') or SameText(ANome, 'sys') or
            SameText(ANome, 'dev');
end;

{$WARN SYMBOL_PLATFORM OFF}
procedure TBuscaIDE.Varrer(ALista: TIDEList; const APasta: string;
  AProfundidade: integer);
var
  vPasta: string;
  vIDE: TIDEInstance;
  vSearch: TSearchRec;
begin
  if FCancelado then
    Exit;

  vPasta := IncludeTrailingPathDelimiter(APasta);
  if not DirectoryExists(vPasta) then
    Exit;

  if Assigned(FOnBusca) then
  begin
    FOnBusca(vPasta, FCancelado);
    if FCancelado then
      Exit;
  end;

  vIDE := InspecionarPasta(vPasta);
  if vIDE <> nil then
  begin
    ALista.Adicionar(vIDE);
    // dentro de uma instalacao nao ha outra: descer so gasta tempo
    Exit;
  end;

  // profundidade negativa e sem limite
  if AProfundidade = 0 then
    Exit;

  if FindFirst(vPasta + '*', faDirectory, vSearch) = 0 then
  try
    repeat
      if ((vSearch.Attr and faDirectory) <> 0) and
         ((vSearch.Attr and faSymLink) = 0) and
         not PastaIgnorada(vSearch.Name) then
        Varrer(ALista, vPasta + vSearch.Name, AProfundidade - 1);
    until FCancelado or (FindNext(vSearch) <> 0);
  finally
    FindClose(vSearch);
  end;
end;

{$WARN SYMBOL_PLATFORM ON}

function TBuscaIDE.BuscarEm(ALista: TIDEList; const APasta: string;
  AProfundidade: integer): integer;
var
  vAntes: integer;
begin
  vAntes := ALista.Count;
  Varrer(ALista, APasta, AProfundidade);
  Result := ALista.Count - vAntes;
end;

procedure TBuscaIDE.Finalizar(ALista: TIDEList);
begin
  ALista.Ordenar;
end;

end.
