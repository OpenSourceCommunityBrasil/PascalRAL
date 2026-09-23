unit RALInst.IDE;

{$mode ObjFPC}{$H+}

// Uma instalacao de IDE encontrada na maquina, do jeito que o resto do
// instalador precisa enxergar: onde esta, que versao e, onde guarda a
// configuracao e para quais plataformas consegue compilar.
// Nucleo sem LCL: a descoberta roda igual na GUI, na CLI e nos testes.

interface

uses
  Classes, SysUtils, Contnrs;

type
  TTipoIDE = (tiDelphi, tiLazarus);

  // uma versao do Delphi, como dado: o salto do BDS 23.0 para o 37.0 mostra
  // por que isto nao pode ser um if encadeado
  TDelphiProduto = record
    Nome: string;          // 'Delphi 12 Athens'
    BDS: string;           // '23.0'; vazio no Delphi 7
    Compilador: string;    // 'VER360'
    VersaoNum: string;     // '36.0', para ordenar e comparar
    Sufixo: string;        // '290'
    RegBase: string;       // '\Software\Embarcadero\BDS'
  end;

  // de onde a instalacao veio; uma mesma pasta achada por dois caminhos fica
  // com a origem mais forte (registro > disco > manual)
  TOrigemIDE = (oiRegistro, oiDisco, oiManual);

  TCapacidadeIDE = (
    ciCompilar,       // ha compilador de linha de comando (dcc32, lazbuild)
    ciInstalarNaIDE,  // ha a IDE em si: pacote de design-time faz sentido
    ciLibraryPath     // da para apontar fontes no library path
  );
  TCapacidadesIDE = set of TCapacidadeIDE;

  TIDEBuscaEvento = procedure(const APasta: string; var ACancelar: boolean) of object;

  { TIDEInstance }

  TIDEInstance = class
  private
    FTipo: TTipoIDE;
    FNome: string;
    FVersao: string;
    FRootDir: string;
    FExeFile: string;
    FBuildFile: string;
    FConfigDir: string;
    FConfigOrigem: string;
    FCommonDir: string;
    FBDSVersao: string;
    FRegKey: string;
    FSufixoPacote: string;
    FVersaoCompilador: string;
    FCompilerFile: string;
    FOrigem: TOrigemIDE;
    FCapacidades: TCapacidadesIDE;
    FPlataformas: TStringList;
    FAvisos: TStringList;
    procedure SetRootDir(const AValue: string);
  public
    constructor Create(ATipo: TTipoIDE);
    destructor Destroy; override;

    procedure Assign(ASource: TIDEInstance);
    function Descricao: string;

    property Tipo: TTipoIDE read FTipo;
    // nome para humanos: 'Delphi 12 Athens', 'Lazarus 4.6'
    property Nome: string read FNome write FNome;
    // versao do produto: '12', '4.6.0.0'
    property Versao: string read FVersao write FVersao;
    // pasta raiz, sempre com o separador no fim
    property RootDir: string read FRootDir write SetRootDir;
    // executavel da IDE (bds.exe, delphi32.exe, lazarus.exe); vazio quando so
    // ha o compilador
    property ExeFile: string read FExeFile write FExeFile;
    // ferramenta de build (dcc32.exe, lazbuild)
    property BuildFile: string read FBuildFile write FBuildFile;
    // Lazarus: --primary-config-path; Delphi: vazio (a configuracao e o registro)
    property ConfigDir: string read FConfigDir write FConfigDir;
    // de onde o ConfigDir foi deduzido, para o log
    property ConfigOrigem: string read FConfigOrigem write FConfigOrigem;
    // Delphi: BDSCOMMONDIR (onde vao Bpl e Dcp do usuario)
    property CommonDir: string read FCommonDir write FCommonDir;
    // Delphi: '23.0'; D7 fica vazio
    property BDSVersao: string read FBDSVersao write FBDSVersao;
    // Delphi: chave do registro em HKCU, sem a raiz
    property RegKey: string read FRegKey write FRegKey;
    // Delphi: '290' — sufixo dos .bpl da IDE e do Indy atualizado
    property SufixoPacote: string read FSufixoPacote write FSufixoPacote;
    // Delphi: 'VER360'; Lazarus: versao do FPC, quando conhecida
    property VersaoCompilador: string read FVersaoCompilador write FVersaoCompilador;
    // Lazarus: compilador FPC configurado
    property CompilerFile: string read FCompilerFile write FCompilerFile;
    property Origem: TOrigemIDE read FOrigem write FOrigem;
    property Capacidades: TCapacidadesIDE read FCapacidades write FCapacidades;
    // nomes normalizados: win32, win64, linux64, osx64, osxarm64, android,
    // android64, iosdevice64, iossimarm64 (Delphi); cpu-os do FPC (Lazarus)
    property Plataformas: TStringList read FPlataformas;
    // o que o usuario precisa saber antes de escolher esta instalacao
    property Avisos: TStringList read FAvisos;
  end;

  { TIDEList }

  TIDEList = class(TObjectList)
  private
    function GetItem(AIndex: integer): TIDEInstance;
  public
    function BuscarPorRaiz(const ARootDir: string): TIDEInstance;
    // devolve a instancia que ficou na lista: a nova, ou a que ja existia
    // para a mesma pasta (a nova e liberada)
    function Adicionar(AIDE: TIDEInstance): TIDEInstance;
    procedure Ordenar;
    property Items[AIndex: integer]: TIDEInstance read GetItem; default;
  end;

  { TBuscaIDE }

  // varredura de pastas comum ao Delphi e ao Lazarus: desce ate a
  // profundidade pedida, para ao achar uma instalacao (nao entra nela) e
  // pula o que nunca contem IDE
  TBuscaIDE = class
  private
    FOnBusca: TIDEBuscaEvento;
    FCancelado: boolean;
    FRaizesExtras: TStringList;
  protected
    function PastaIgnorada(const ANome: string): boolean; virtual;
    procedure Varrer(ALista: TIDEList; const APasta: string; AProfundidade: integer);
  public
    constructor Create;
    destructor Destroy; override;

    // nil quando a pasta nao e a raiz de uma instalacao
    function InspecionarPasta(const APasta: string): TIDEInstance; virtual; abstract;
    // raizes conhecidas do sistema + RaizesExtras; rapido
    procedure BuscarPadrao(ALista: TIDEList); virtual; abstract;
    // a pasta pode ser a propria instalacao ou uma pasta que contem varias
    // (D:\IDE\lazarus); devolve quantas entraram na lista
    function BuscarEm(ALista: TIDEList; const APasta: string;
      AProfundidade: integer = 3): integer;
    // depois de montada a lista: avisos que dependem de comparar instalacoes
    procedure Finalizar(ALista: TIDEList); virtual;

    property RaizesExtras: TStringList read FRaizesExtras;
    property Cancelado: boolean read FCancelado write FCancelado;
    property OnBusca: TIDEBuscaEvento read FOnBusca write FOnBusca;
  end;

const
  DelphiProdutos: array[0..21] of TDelphiProduto = (
    (Nome: 'Delphi 7';                BDS: '';     Compilador: 'VER150'; VersaoNum: '15.0'; Sufixo: '70';  RegBase: '\Software\Borland\Delphi'),
    (Nome: 'Delphi 2005';             BDS: '3.0';  Compilador: 'VER170'; VersaoNum: '17.0'; Sufixo: '90';  RegBase: '\Software\Borland\BDS'),
    (Nome: 'Delphi 2006';             BDS: '4.0';  Compilador: 'VER180'; VersaoNum: '18.0'; Sufixo: '100'; RegBase: '\Software\Borland\BDS'),
    (Nome: 'Delphi 2007';             BDS: '5.0';  Compilador: 'VER185'; VersaoNum: '18.5'; Sufixo: '100'; RegBase: '\Software\Borland\BDS'),
    (Nome: 'Delphi 2009';             BDS: '6.0';  Compilador: 'VER200'; VersaoNum: '20.0'; Sufixo: '120'; RegBase: '\Software\CodeGear\BDS'),
    (Nome: 'Delphi 2010';             BDS: '7.0';  Compilador: 'VER210'; VersaoNum: '21.0'; Sufixo: '140'; RegBase: '\Software\CodeGear\BDS'),
    (Nome: 'Delphi XE';               BDS: '8.0';  Compilador: 'VER220'; VersaoNum: '22.0'; Sufixo: '150'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi XE2';              BDS: '9.0';  Compilador: 'VER230'; VersaoNum: '23.0'; Sufixo: '160'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi XE3';              BDS: '10.0'; Compilador: 'VER240'; VersaoNum: '24.0'; Sufixo: '170'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi XE4';              BDS: '11.0'; Compilador: 'VER250'; VersaoNum: '25.0'; Sufixo: '180'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi XE5';              BDS: '12.0'; Compilador: 'VER260'; VersaoNum: '26.0'; Sufixo: '190'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi XE6';              BDS: '14.0'; Compilador: 'VER270'; VersaoNum: '27.0'; Sufixo: '200'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi XE7';              BDS: '15.0'; Compilador: 'VER280'; VersaoNum: '28.0'; Sufixo: '210'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi XE8';              BDS: '16.0'; Compilador: 'VER290'; VersaoNum: '29.0'; Sufixo: '220'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi 10 Seattle';       BDS: '17.0'; Compilador: 'VER300'; VersaoNum: '30.0'; Sufixo: '230'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi 10.1 Berlin';      BDS: '18.0'; Compilador: 'VER310'; VersaoNum: '31.0'; Sufixo: '240'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi 10.2 Tokyo';       BDS: '19.0'; Compilador: 'VER320'; VersaoNum: '32.0'; Sufixo: '250'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi 10.3 Rio';         BDS: '20.0'; Compilador: 'VER330'; VersaoNum: '33.0'; Sufixo: '260'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi 10.4 Sydney';      BDS: '21.0'; Compilador: 'VER340'; VersaoNum: '34.0'; Sufixo: '270'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi 11 Alexandria';    BDS: '22.0'; Compilador: 'VER350'; VersaoNum: '35.0'; Sufixo: '280'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi 12 Athens';        BDS: '23.0'; Compilador: 'VER360'; VersaoNum: '36.0'; Sufixo: '290'; RegBase: '\Software\Embarcadero\BDS'),
    (Nome: 'Delphi 13 Florence';      BDS: '37.0'; Compilador: 'VER370'; VersaoNum: '37.0'; Sufixo: '370'; RegBase: '\Software\Embarcadero\BDS')
  );

function ProdutoPorBDS(const ABDS: string): integer;
function ProdutoPorSufixo(const ASufixo: string): integer;
// 'XE8', '10.1', '12', 'Delphi 12 Athens', '2009', '7' -> indice; -1 se nao conhece.
// E como o manifesto e as receitas dizem uma versao do Delphi
function ProdutoPorNome(const ANome: string): integer;

// forma canonica de uma pasta para comparar instalacoes
function NormalizarPasta(const APasta: string): string;

// compara versoes numero a numero ('4.10' > '4.9')
function CompararVersoes(const A, B: string): integer;

function NomeOrigem(AOrigem: TOrigemIDE): string;

implementation

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
    if SameText(vProduto, vNome) or SameText(Copy(vProduto, 1, Length(vNome) + 1), vNome + ' ') then
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
    oiRegistro: Result := 'registro';
    oiDisco:    Result := 'disco';
    oiManual:   Result := 'manual';
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
