/// Catalog of the packages of a RAL tree, read from the packages themselves.
/// Nothing here comes from a manifest: the list comes from pkg/Delphi/**/*.dpk
/// and pkg/Lazarus/**/*.lpk, the dependencies of each one from requires (.dpk)
/// and RequiredPkgs (.lpk), and the installation order is the topological sort
/// of that graph. A new package dropped into pkg/ enters the catalog, in the
/// right place, with nobody editing anything. What a package does not declare
/// cannot be deduced and stays as separate data: IndyRAL's Indy, for instance,
/// is not in requires (its suffix changes per Delphi version); the .dproj keeps
/// a trace of it in DCC_UsePackage, which enters here as an *implicit*
/// dependency. The catalog does not read the disk directly but a
/// TOrigemArquivos: the user chooses the features *before* any download, so in
/// the GUI the catalog comes from the version zip; a tree on disk is the case of
/// whoever develops RAL and of a folder already downloaded.
unit RALInst.Catalogo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  /// Source of the repository files; paths are always relative to the
  /// repository root, separated by '/'.
  TOrigemArquivos = class
  public
    /// Submodules are never in this source and arrive by a download of their
    /// own (the GitHub zip): a missing submodule does not make the package
    /// unavailable
    function BaixaSubmodulos: boolean; virtual;
    /// For the log and the plan: the folder, or the repository and ref
    function Descricao: string; virtual; abstract;
    function Existe(const ACaminho: string): boolean; virtual; abstract;
    /// Is there any file under the folder
    function ExistePasta(const APasta: string): boolean; virtual;
    /// Contents of the file; raises an exception when it does not exist
    function Ler(const ACaminho: string): string; virtual; abstract;
    /// Every file under APasta, recursively
    procedure Listar(const APasta: string; ALista: TStrings); virtual; abstract;
    /// Where the file really is (disk path or URL)
    function Localizar(const ACaminho: string): string; virtual; abstract;
  end;

  /// Source on disk.
  TOrigemLocal = class(TOrigemArquivos)
  private
    FRaiz: string;
    /// Recursive part of Listar
    procedure ListarPasta(const APasta, ARelativo: string; ALista: TStrings);
  public
    constructor Create(const ARaiz: string);
    function Descricao: string; override;
    function Existe(const ACaminho: string): boolean; override;
    function ExistePasta(const APasta: string): boolean; override;
    function Ler(const ACaminho: string): string; override;
    procedure Listar(const APasta: string; ALista: TStrings); override;
    function Localizar(const ACaminho: string): string; override;

    property Raiz: string read FRaiz;
  end;

  /// IDE a package is for.
  TTipoPacote = (tpDelphi, tpLazarus);

  /// Delphi: {$RUNONLY} -> runtime, {$DESIGNONLY} -> design, none -> both.
  /// Lazarus: RunTime/RunTimeOnly -> runtime, DesignTime -> design,
  /// RunAndDesignTime -> both. Without <Type>, runtime: Lazarus only writes
  /// what differs from the default, and the default is RunTime.
  TUsoPacote = (upRuntime, upDesign, upAmbos);

  /// One package of the catalog.
  TPacote = class
  private
    FArquivo: string;
    FArquivoRelativo: string;
    FCaminhosBusca: TStringList;
    FDescricao: string;
    FExternos: TStringList;
    FFontesAusentes: TStringList;
    FGrupo: string;
    FImplicitos: TStringList;
    FInternos: TStringList;
    FLazRuntimeOnly: boolean;
    FLibSuffix: string;
    FNome: string;
    FOrdem: integer;
    FPlataformasDproj: TStringList;
    FRequires: TStringList;
    FSubmodulos: TStringList;
    FSubmodulosAusentes: TStringList;
    FTipo: TTipoPacote;
    FUnidades: TStringList;
    FUso: TUsoPacote;
    FVariaveis: TStringList;
    FVersoesMinimas: TStringList;
    function GetInstalavel: boolean;
  public
    constructor Create(ATipo: TTipoPacote);
    destructor Destroy; override;

    /// Where the source says the package is (disk or URL)
    property Arquivo: string read FArquivo;
    /// Relative to the repository root, with '/'; what counts after download
    property ArquivoRelativo: string read FArquivoRelativo;
    /// Repository folders the package puts in its search path
    /// (DCC_UnitSearchPath of the .dproj, OtherUnitFiles of the .lpk), with '/',
    /// only those that exist: it is where what the package uses without listing
    /// lives (SaguiRAL's libsagui.pas)
    property CaminhosBusca: TStringList read FCaminhosBusca;
    property Descricao: string read FDescricao;
    /// Of the Requires, those that are not in this catalog (rtl, designide,
    /// FireDAC, indylaz, mormot2)
    property Externos: TStringList read FExternos;
    /// Declared units the source lacks and no submodule explains
    property FontesAusentes: TStringList read FFontesAusentes;
    /// Subfolder of pkg/<IDE>: '', 'Engine', 'Database', 'compression/zstd'
    property Grupo: string read FGrupo;
    /// Delphi: packages of the .dproj DCC_UsePackage that requires lacks,
    /// without the numeric suffix (IndyCore160 -> IndyCore)
    property Implicitos: TStringList read FImplicitos;
    /// Goes into the IDE (design or both); pure runtime is only compiled
    property Instalavel: boolean read GetInstalavel;
    /// Of the Requires, those that are packages of this catalog
    property Internos: TStringList read FInternos;
    /// Lazarus RunTimeOnly: never goes into the IDE, not even as a dependency
    property LazRuntimeOnly: boolean read FLazRuntimeOnly;
    property LibSuffix: string read FLibSuffix;
    /// Name declared in the package ('IndyRAL'), which is what others require
    property Nome: string read FNome;
    /// Position in the installation order of its kind (0 = first); -1 in a cycle
    property Ordem: integer read FOrdem;
    /// Delphi: platforms the .dproj enables (<Platform value="Win64">True), in
    /// lowercase ('win64'); empty when the .dproj does not say. A platform out
    /// of the list is not for the package (XSocketRAL only has Win32)
    property PlataformasDproj: TStringList read FPlataformasDproj;
    /// Everything the package declares to require, as declared
    property Requires: TStringList read FRequires;
    /// Submodule paths (.gitmodules) holding units of the package: the GitHub
    /// zipball does not bring them, and the download fetches each one
    property Submodulos: TStringList read FSubmodulos;
    /// Of the Submodulos, those the source lacks (checkout without submodule
    /// update, GitHub zip)
    property SubmodulosAusentes: TStringList read FSubmodulosAusentes;
    property Tipo: TTipoPacote read FTipo;
    /// Unit files of the package, relative to the root, with '/'
    property Unidades: TStringList read FUnidades;
    property Uso: TUsoPacote read FUso;
    /// Variables the search path requires ($(mormot2) in SynopseRAL.dproj): it
    /// is how a Delphi package declares a dependency that has no package
    property Variaveis: TStringList read FVariaveis;
    /// Lazarus: name=minimum version required ('indylaz=10.6')
    property VersoesMinimas: TStringList read FVersoesMinimas;
  end;

  /// The packages of one RAL tree.
  TCatalogo = class
  private
    FErros: TStringList;
    FOrigem: TOrigemArquivos;
    FPacotes: TObjectList;
    FPastaDelphi: string;
    FPastaLazarus: string;
    FSubmodulos: TStringList;
    function GetCount: integer;
    function GetPacote(AIndex: integer): TPacote;
    function GetRaiz: string;
    /// Folders of the package search path that exist in the source
    procedure LerCaminhosBusca(APacote: TPacote; const APastaPacote, ALista: string);
    /// Reads one .dpk (and its .dproj)
    procedure LerDpk(const AArquivo: string);
    /// Reads one .lpk
    procedure LerLpk(const AArquivo: string);
    /// Reads .gitmodules
    procedure LerSubmodulos;
    /// Package files of one kind under a folder
    procedure ListarPacotes(const APasta, AExtensao: string; ALista: TStrings);
    /// Topological sort of one kind (Kahn)
    procedure Ordenar(ATipo: TTipoPacote);
    /// Splits requires into internal and external, finds submodules and
    /// missing sources
    procedure Resolver;
  public
    constructor Create;
    destructor Destroy; override;
    /// A package by kind and name; nil if unknown
    function Buscar(ATipo: TTipoPacote; const ANome: string): TPacote;
    /// Reads the packages from the source, which becomes the catalog's;
    /// returns False when there is no package at all
    function Carregar(AOrigem: TOrigemArquivos): boolean; overload;
    /// Shortcut for a tree on disk (the folder holding pkg/ and src/)
    function Carregar(const ARaizRepo: string): boolean; overload;
    /// The requested ones plus everything they depend on, in installation
    /// order; names the catalog does not know go to ADesconhecidos
    procedure Fechamento(ATipo: TTipoPacote; ANomes: TStrings; ALista: TList;
      ADesconhecidos: TStrings = nil);
    procedure Limpar;
    /// Packages of one kind, in installation order
    procedure Listar(ATipo: TTipoPacote; ALista: TList);
    /// Readable plan, before any write
    function Plano(ATipo: TTipoPacote; ANomes: TStrings = nil): string;

    property Count: integer read GetCount;
    /// Read failures and cycles: the catalog stays usable without those
    /// packages
    property Erros: TStringList read FErros;
    property Origem: TOrigemArquivos read FOrigem;
    property Pacotes[AIndex: integer]: TPacote read GetPacote; default;
    /// Where the packages are, relative to the root: 'pkg/Delphi' and
    /// 'pkg/Lazarus' in RAL; a dependency (Zeos) sets its own before Carregar
    property PastaDelphi: string read FPastaDelphi write FPastaDelphi;
    property PastaLazarus: string read FPastaLazarus write FPastaLazarus;
    /// Description of the source
    property Raiz: string read GetRaiz;
    /// path=url of every submodule declared in .gitmodules
    property Submodulos: TStringList read FSubmodulos;
  end;

/// Name of a package kind: 'Delphi', 'Lazarus'
function NomeTipoPacote(ATipo: TTipoPacote): string;
/// Name of a package use: 'runtime', 'design', 'runtime+design'
function NomeUso(AUso: TUsoPacote): string;

implementation

uses
  RegExpr, DOM, XMLRead,
  RALInst.Mensagens;

function NomeUso(AUso: TUsoPacote): string;
begin
  case AUso of
    upRuntime: Result := 'runtime';
    upDesign:  Result := 'design';
    upAmbos:   Result := 'runtime+design';
  else
    Result := '';
  end;
end;

function NomeTipoPacote(ATipo: TTipoPacote): string;
begin
  case ATipo of
    tpDelphi:  Result := 'Delphi';
    tpLazarus: Result := 'Lazarus';
  else
    Result := '';
  end;
end;

// 'a,b,c' -> 'a, b, c'
function Juntar(ALista: TStrings): string;
begin
  Result := StringReplace(ALista.CommaText, ',', ', ', [rfReplaceAll]);
end;

function NovaLista: TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  Result.Duplicates := dupIgnore;
end;

function Contem(ALista: TStrings; const ANome: string): boolean;
begin
  Result := ALista.IndexOf(ANome) >= 0;
end;

// pasta de um caminho relativo com '/': 'pkg/Delphi/X.dpk' -> 'pkg/Delphi'
function PastaRelativa(const ACaminho: string): string;
var
  vPos: integer;
begin
  vPos := LastDelimiter('/', ACaminho);
  Result := Copy(ACaminho, 1, vPos - 1);
end;

// caminho escrito dentro de um pacote ('..\..\src\x.pas'), resolvido contra a
// pasta do pacote e devolvido relativo a raiz, com '/' e sem '.' nem '..';
// vazio se sai da raiz
function ResolverRelativo(const APastaPacote, ACaminho: string): string;
var
  vPartes: TStringList;
  vItem: string;
begin
  Result := '';
  vPartes := TStringList.Create;
  try
    for vItem in (APastaPacote + '/' +
                  StringReplace(Trim(ACaminho), '\', '/', [rfReplaceAll])).Split(['/']) do
    begin
      if (vItem = '') or (vItem = '.') then
        Continue;
      if vItem = '..' then
      begin
        if vPartes.Count = 0 then
          Exit;
        vPartes.Delete(Pred(vPartes.Count));
      end
      else
        vPartes.Add(vItem);
    end;
    for vItem in vPartes do
    begin
      if Result <> '' then
        Result := Result + '/';
      Result := Result + vItem;
    end;
  finally
    vPartes.Free;
  end;
end;

function SemBOM(const ATexto: string): string;
begin
  // BOM UTF-8 (o XSocketRAL.dpk tem)
  Result := ATexto;
  if Copy(Result, 1, 3) = #$EF#$BB#$BF then
    Delete(Result, 1, 3);
end;

{ TOrigemArquivos }

function TOrigemArquivos.BaixaSubmodulos: boolean;
begin
  Result := False;
end;

function TOrigemArquivos.ExistePasta(const APasta: string): boolean;
var
  vLista: TStringList;
begin
  vLista := TStringList.Create;
  try
    Listar(APasta, vLista);
    Result := vLista.Count > 0;
  finally
    vLista.Free;
  end;
end;

{ TOrigemLocal }

constructor TOrigemLocal.Create(const ARaiz: string);
begin
  inherited Create;
  FRaiz := IncludeTrailingPathDelimiter(ExpandFileName(ARaiz));
end;

function TOrigemLocal.Descricao: string;
begin
  Result := FRaiz;
end;

function TOrigemLocal.Localizar(const ACaminho: string): string;
begin
  Result := FRaiz + StringReplace(ACaminho, '/', PathDelim, [rfReplaceAll]);
end;

procedure TOrigemLocal.ListarPasta(const APasta, ARelativo: string; ALista: TStrings);
var
  vSearch: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(APasta) + '*', faAnyFile, vSearch) = 0 then
  try
    repeat
      if (vSearch.Name = '.') or (vSearch.Name = '..') then
        Continue;
      if (vSearch.Attr and faDirectory) <> 0 then
        ListarPasta(IncludeTrailingPathDelimiter(APasta) + vSearch.Name,
                    ARelativo + vSearch.Name + '/', ALista)
      else
        ALista.Add(ARelativo + vSearch.Name);
    until FindNext(vSearch) <> 0;
  finally
    FindClose(vSearch);
  end;
end;

procedure TOrigemLocal.Listar(const APasta: string; ALista: TStrings);
var
  vPasta: string;
begin
  vPasta := StringReplace(APasta, '\', '/', [rfReplaceAll]);
  if (vPasta <> '') and (vPasta[Length(vPasta)] <> '/') then
    vPasta := vPasta + '/';
  ListarPasta(Localizar(vPasta), vPasta, ALista);
end;

function TOrigemLocal.Existe(const ACaminho: string): boolean;
begin
  Result := FileExists(Localizar(ACaminho));
end;

function TOrigemLocal.ExistePasta(const APasta: string): boolean;
begin
  Result := DirectoryExists(Localizar(APasta));
end;

function TOrigemLocal.Ler(const ACaminho: string): string;
var
  vLista: TStringList;
begin
  vLista := TStringList.Create;
  try
    vLista.LoadFromFile(Localizar(ACaminho));
    Result := SemBOM(vLista.Text);
  finally
    vLista.Free;
  end;
end;

{ TPacote }

constructor TPacote.Create(ATipo: TTipoPacote);
begin
  inherited Create;
  FTipo := ATipo;
  FUso := upAmbos;
  FOrdem := -1;
  FRequires := NovaLista;
  FInternos := NovaLista;
  FExternos := NovaLista;
  FImplicitos := NovaLista;
  FVersoesMinimas := NovaLista;
  FUnidades := NovaLista;
  FFontesAusentes := NovaLista;
  FSubmodulos := NovaLista;
  FSubmodulosAusentes := NovaLista;
  FCaminhosBusca := NovaLista;
  FVariaveis := NovaLista;
  FPlataformasDproj := NovaLista;
end;

destructor TPacote.Destroy;
begin
  FCaminhosBusca.Free;
  FPlataformasDproj.Free;
  FVariaveis.Free;
  FRequires.Free;
  FInternos.Free;
  FExternos.Free;
  FImplicitos.Free;
  FVersoesMinimas.Free;
  FUnidades.Free;
  FFontesAusentes.Free;
  FSubmodulos.Free;
  FSubmodulosAusentes.Free;
  inherited Destroy;
end;

function TPacote.GetInstalavel: boolean;
begin
  Result := FUso <> upRuntime;
end;

{ TCatalogo }

constructor TCatalogo.Create;
begin
  inherited Create;
  FPacotes := TObjectList.Create(True);
  FErros := TStringList.Create;
  FSubmodulos := TStringList.Create;
  FPastaDelphi := 'pkg/Delphi';
  FPastaLazarus := 'pkg/Lazarus';
end;

procedure TCatalogo.LerCaminhosBusca(APacote: TPacote;
  const APastaPacote, ALista: string);
const
  // variaveis da propria IDE ou do projeto: nao sao dependencia de ninguem
  DaIDE: array[0..14] of string = (
    'DCC_UnitSearchPath', 'BDS', 'BDSLIB', 'BDSBIN', 'BDSINCLUDE', 'BDSCOMMONDIR',
    'BDSUSERDIR', 'Platform', 'Config', 'ProjectDir', 'OUTPUTDIR', 'PkgDir',
    'LazarusDir', 'TargetCPU', 'TargetOS');
var
  vItem, vCaminho, vVar: string;
  vRegex: TRegExpr;
  vInt: integer;
  vDaIDE: boolean;
begin
  vRegex := TRegExpr.Create('\$\(([\w.]+)\)');
  try
    for vItem in ALista.Split([';']) do
    begin
      vCaminho := Trim(vItem);
      if vCaminho = '' then
        Continue;
      if Pos('$(', vCaminho) > 0 then
      begin
        if vRegex.Exec(vCaminho) then
          repeat
            vVar := vRegex.Match[1];
            vDaIDE := False;
            for vInt := Low(DaIDE) to High(DaIDE) do
              if SameText(vVar, DaIDE[vInt]) then
                vDaIDE := True;
            if not vDaIDE and not Contem(APacote.FVariaveis, vVar) then
              APacote.FVariaveis.Add(vVar);
          until not vRegex.ExecNext;
        Continue;
      end;
      // caminhos errados sao comuns (..\..\src a partir de pkg\Delphi\Engine):
      // so vale o que existe
      vCaminho := ResolverRelativo(APastaPacote, vCaminho);
      if (vCaminho <> '') and not Contem(APacote.FCaminhosBusca, vCaminho) and
         FOrigem.ExistePasta(vCaminho) then
        APacote.FCaminhosBusca.Add(vCaminho);
    end;
  finally
    vRegex.Free;
  end;
end;

destructor TCatalogo.Destroy;
begin
  Limpar;
  FPacotes.Free;
  FErros.Free;
  FSubmodulos.Free;
  inherited Destroy;
end;

function TCatalogo.GetRaiz: string;
begin
  Result := '';
  if FOrigem <> nil then
    Result := FOrigem.Descricao;
end;

function TCatalogo.GetCount: integer;
begin
  Result := FPacotes.Count;
end;

function TCatalogo.GetPacote(AIndex: integer): TPacote;
begin
  Result := TPacote(FPacotes[AIndex]);
end;

procedure TCatalogo.Limpar;
begin
  FPacotes.Clear;
  FErros.Clear;
  FSubmodulos.Clear;
  FreeAndNil(FOrigem);
end;

procedure TCatalogo.ListarPacotes(const APasta, AExtensao: string; ALista: TStrings);
var
  vTodos: TStringList;
  vArquivo, vPasta: string;
begin
  vTodos := TStringList.Create;
  try
    FOrigem.Listar(APasta, vTodos);
    for vArquivo in vTodos do
    begin
      if not SameText(ExtractFileExt(vArquivo), AExtensao) then
        Continue;
      // copias de seguranca das IDEs tem pacotes com o mesmo nome
      vPasta := '/' + LowerCase(PastaRelativa(vArquivo)) + '/';
      if (Pos('/__history/', vPasta) > 0) or (Pos('/__recovery/', vPasta) > 0) or
         (Pos('/backup/', vPasta) > 0) or (Pos('/lib/', vPasta) > 0) or
         (Pos('/.', vPasta) > 0) then
        Continue;
      ALista.Add(vArquivo);
    end;
  finally
    vTodos.Free;
  end;
end;

procedure TCatalogo.LerSubmodulos;
var
  vLinhas: TStringList;
  vLinha, vChave, vCaminho, vUrl: string;
  vInt, vPos: integer;

  procedure Fechar;
  begin
    if vCaminho <> '' then
      FSubmodulos.Values[StringReplace(vCaminho, '\', '/', [rfReplaceAll])] := vUrl;
    vCaminho := '';
    vUrl := '';
  end;

begin
  FSubmodulos.Clear;
  if not FOrigem.Existe('.gitmodules') then
    Exit;

  // cada bloco [submodule "..."] tem path e url, em qualquer ordem
  vCaminho := '';
  vUrl := '';
  vLinhas := TStringList.Create;
  try
    vLinhas.Text := FOrigem.Ler('.gitmodules');
    for vInt := 0 to Pred(vLinhas.Count) do
    begin
      vLinha := Trim(vLinhas[vInt]);
      if (vLinha <> '') and (vLinha[1] = '[') then
      begin
        Fechar;
        Continue;
      end;
      vPos := Pos('=', vLinha);
      if vPos = 0 then
        Continue;
      vChave := LowerCase(Trim(Copy(vLinha, 1, vPos - 1)));
      if vChave = 'path' then
        vCaminho := Trim(Copy(vLinha, vPos + 1, MaxInt))
      else if vChave = 'url' then
        vUrl := Trim(Copy(vLinha, vPos + 1, MaxInt));
    end;
    Fechar;
  finally
    vLinhas.Free;
  end;
end;

procedure TCatalogo.LerDpk(const AArquivo: string);
var
  vTexto, vLimpo, vGrupo, vDproj, vNome, vPastaPkg: string;
  vRegex: TRegExpr;
  vPacote: TPacote;
  vItens: TStringArray;
  vItem: string;
  vPasta: string;
begin
  vPacote := TPacote.Create(tpDelphi);
  try
    vTexto := FOrigem.Ler(AArquivo);
    vPacote.FArquivo := FOrigem.Localizar(AArquivo);
    vPacote.FArquivoRelativo := AArquivo;
    vPastaPkg := PastaRelativa(AArquivo);

    vRegex := TRegExpr.Create;
    try
      vRegex.ModifierI := True;

      // diretivas antes de tirar os comentarios, que e onde elas moram
      vRegex.Expression := '\{\$RUNONLY\}';
      if vRegex.Exec(vTexto) then
        vPacote.FUso := upRuntime;
      vRegex.Expression := '\{\$DESIGNONLY\}';
      if vRegex.Exec(vTexto) then
        vPacote.FUso := upDesign;
      vRegex.Expression := '\{\$DESCRIPTION\s+''([^'']*)''\}';
      if vRegex.Exec(vTexto) then
        vPacote.FDescricao := vRegex.Match[1];
      vRegex.Expression := '\{\$LIBSUFFIX\s+(''[^'']*''|AUTO)\}';
      if vRegex.Exec(vTexto) then
        vPacote.FLibSuffix := StringReplace(vRegex.Match[1], '''', '', [rfReplaceAll]);

      // sem comentarios de nenhum dos tres tipos; strings ficam, porque os
      // caminhos do contains estao nelas
      vRegex.Expression := '\{[^}]*\}|\(\*.*?\*\)|//[^\n]*';
      vRegex.ModifierS := True;
      vLimpo := vRegex.Replace(vTexto, ' ', False);
      vRegex.ModifierS := False;

      vRegex.Expression := '\bpackage\s+([\w.]+)\s*;';
      if not vRegex.Exec(vLimpo) then
      begin
        FErros.Add(Format(emCatalogoSemPackage, [AArquivo]));
        FreeAndNil(vPacote);
        Exit;
      end;
      vPacote.FNome := vRegex.Match[1];

      vRegex.ModifierS := True;
      vRegex.Expression := '\brequires\b(.*?);';
      if vRegex.Exec(vLimpo) then
      begin
        vItens := vRegex.Match[1].Split([',']);
        for vItem in vItens do
          if Trim(vItem) <> '' then
            vPacote.FRequires.Add(Trim(vItem));
      end;

      vRegex.Expression := '\bcontains\b(.*?);';
      if vRegex.Exec(vLimpo) then
      begin
        vItens := vRegex.Match[1].Split([',']);
        for vItem in vItens do
        begin
          vPasta := vItem;
          if Pos('''', vPasta) > 0 then
          begin
            vPasta := Copy(vPasta, Pos('''', vPasta) + 1, MaxInt);
            vPasta := Copy(vPasta, 1, Pos('''', vPasta) - 1);
            vPasta := ResolverRelativo(vPastaPkg, vPasta);
            if vPasta <> '' then
              vPacote.FUnidades.Add(vPasta);
          end;
        end;
      end;
    finally
      vRegex.Free;
    end;

    // rastro do que o requires nao declara (o Indy, com ou sem sufixo)
    vDproj := ChangeFileExt(AArquivo, '.dproj');
    if FOrigem.Existe(vDproj) then
    begin
      // o caminho de busca de todas as configuracoes (Base, Win32, Release...)
      vRegex := TRegExpr.Create('<DCC_UnitSearchPath>([^<]*)</DCC_UnitSearchPath>');
      try
        if vRegex.Exec(FOrigem.Ler(vDproj)) then
          repeat
            LerCaminhosBusca(vPacote, vPastaPkg, vRegex.Match[1]);
          until not vRegex.ExecNext;
      finally
        vRegex.Free;
      end;

      vRegex := TRegExpr.Create('<DCC_UsePackage>([^<]*)</DCC_UsePackage>');
      try
        if vRegex.Exec(FOrigem.Ler(vDproj)) then
          repeat
            for vItem in vRegex.Match[1].Split([';']) do
            begin
              vNome := Trim(vItem);
              if (vNome = '') or (Pos('$(', vNome) > 0) then
                Continue;
              vNome := ReplaceRegExpr('^(.*[A-Za-z_])\d{2,3}$', vNome, '$1', True);
              if not SameText(vNome, vPacote.FNome) and
                 not Contem(vPacote.FRequires, vNome) and
                 not Contem(vPacote.FImplicitos, vNome) then
                vPacote.FImplicitos.Add(vNome);
            end;
          until not vRegex.ExecNext;
      finally
        vRegex.Free;
      end;
      vPacote.FImplicitos.Sort;

      // plataformas que o .dproj habilita: o XSocketRAL so tem Win32, o
      // KwikRAL e o OkHttpRAL nem citam o Win64
      vRegex := TRegExpr.Create('<Platform value="(\w+)">True</Platform>');
      try
        vRegex.ModifierI := True;
        if vRegex.Exec(FOrigem.Ler(vDproj)) then
          repeat
            if not Contem(vPacote.FPlataformasDproj, LowerCase(vRegex.Match[1])) then
              vPacote.FPlataformasDproj.Add(LowerCase(vRegex.Match[1]));
          until not vRegex.ExecNext;
      finally
        vRegex.Free;
      end;
    end;

    vGrupo := Copy(vPastaPkg, Length(FPastaDelphi + '/') + 1, MaxInt);
    vPacote.FGrupo := vGrupo;

    FPacotes.Add(vPacote);
  except
    on E: Exception do
    begin
      FErros.Add(AArquivo + ': ' + E.Message);
      vPacote.Free;
    end;
  end;
end;

procedure TCatalogo.LerLpk(const AArquivo: string);
var
  vDoc: TXMLDocument;
  vStream: TStringStream;
  vPkg, vNo, vItem, vFilho: TDOMNode;
  vPacote: TPacote;
  vInt: integer;
  vTipo, vNome, vVersao, vGrupo, vArq: string;

  function Filho(ANo: TDOMNode; const ANome: string): TDOMNode;
  begin
    Result := nil;
    if ANo <> nil then
      Result := ANo.FindNode(DOMString(ANome));
  end;

  function Valor(ANo: TDOMNode; const ANome: string = 'Value'): string;
  begin
    Result := '';
    if (ANo <> nil) and (ANo is TDOMElement) then
      Result := string(TDOMElement(ANo).GetAttribute(DOMString(ANome)));
  end;

begin
  vPacote := nil;
  vDoc := nil;
  try
    vStream := TStringStream.Create(FOrigem.Ler(AArquivo));
    try
      ReadXMLFile(vDoc, vStream);
    finally
      vStream.Free;
    end;
    try
      vPkg := Filho(vDoc.DocumentElement, 'Package');
      if vPkg = nil then
      begin
        FErros.Add(Format(emCatalogoSemNoPackage, [AArquivo]));
        Exit;
      end;

      vPacote := TPacote.Create(tpLazarus);
      vPacote.FArquivo := FOrigem.Localizar(AArquivo);
      vPacote.FArquivoRelativo := AArquivo;
      vPacote.FNome := Valor(Filho(vPkg, 'Name'));
      if vPacote.FNome = '' then
        vPacote.FNome := ChangeFileExt(ExtractFileName(AArquivo), '');
      vPacote.FDescricao := Valor(Filho(vPkg, 'Description'));

      vTipo := Valor(Filho(vPkg, 'Type'));
      if SameText(vTipo, 'RunTime') then
        vPacote.FUso := upRuntime
      else if SameText(vTipo, 'RunTimeOnly') then
      begin
        vPacote.FUso := upRuntime;
        vPacote.FLazRuntimeOnly := True;
      end
      else if SameText(vTipo, 'DesignTime') then
        vPacote.FUso := upDesign
      else if SameText(vTipo, 'RunAndDesignTime') then
        vPacote.FUso := upAmbos
      else
        vPacote.FUso := upRuntime;

      vNo := Filho(vPkg, 'RequiredPkgs');
      if vNo <> nil then
        for vInt := 0 to Pred(vNo.ChildNodes.Count) do
        begin
          vItem := vNo.ChildNodes[vInt];
          vNome := Valor(Filho(vItem, 'PackageName'));
          if vNome = '' then
            Continue;
          vPacote.FRequires.Add(vNome);
          vFilho := Filho(vItem, 'MinVersion');
          if (vFilho <> nil) and SameText(Valor(vFilho, 'Valid'), 'True') then
          begin
            vVersao := Valor(vFilho, 'Major');
            if vVersao = '' then
              vVersao := '0';
            if Valor(vFilho, 'Minor') <> '' then
              vVersao := vVersao + '.' + Valor(vFilho, 'Minor');
            if Valor(vFilho, 'Release') <> '' then
              vVersao := vVersao + '.' + Valor(vFilho, 'Release');
            vPacote.FVersoesMinimas.Values[vNome] := vVersao;
          end;
        end;

      vNo := Filho(vPkg, 'Files');
      if vNo <> nil then
        for vInt := 0 to Pred(vNo.ChildNodes.Count) do
        begin
          vItem := vNo.ChildNodes[vInt];
          vArq := Valor(Filho(vItem, 'Filename'));
          // include e .lrs tambem sao fonte, mas o que falta em submodulo
          // aparece nas unidades; so elas contam
          if (vArq <> '') and (SameText(ExtractFileExt(vArq), '.pas') or
                               SameText(ExtractFileExt(vArq), '.pp')) then
          begin
            vArq := ResolverRelativo(PastaRelativa(AArquivo), vArq);
            if vArq <> '' then
              vPacote.FUnidades.Add(vArq);
          end;
        end;
      // <CompilerOptions><SearchPaths><OtherUnitFiles Value="a;b"/>
      vNo := Filho(Filho(vPkg, 'CompilerOptions'), 'SearchPaths');
      if vNo <> nil then
        LerCaminhosBusca(vPacote, PastaRelativa(AArquivo),
                         Valor(Filho(vNo, 'OtherUnitFiles')));
    finally
      vDoc.Free;
    end;

    vGrupo := Copy(PastaRelativa(AArquivo), Length(FPastaLazarus + '/') + 1, MaxInt);
    vPacote.FGrupo := vGrupo;

    FPacotes.Add(vPacote);
    vPacote := nil;
  except
    on E: Exception do
    begin
      FErros.Add(AArquivo + ': ' + E.Message);
      vPacote.Free;
    end;
  end;
end;

procedure TCatalogo.Resolver;
var
  vInt, vReq: integer;
  vPacote: TPacote;
  vUnidade, vSub: string;
  vHerdados, vVisitados: TStringList;

  function SubmoduloDe(const AUnidade: string): string;
  var
    vIdx: integer;
  begin
    Result := '';
    for vIdx := 0 to Pred(FSubmodulos.Count) do
      if SameText(Copy(AUnidade, 1, Length(FSubmodulos.Names[vIdx]) + 1),
                  FSubmodulos.Names[vIdx] + '/') then
        Exit(FSubmodulos.Names[vIdx]);
  end;

  function DividemUnidade(A, B: TPacote): boolean;
  var
    vUni: string;
  begin
    Result := False;
    for vUni in A.FUnidades do
      if (SubmoduloDe(vUni) = '') and Contem(B.FUnidades, vUni) then
        Exit(True);
  end;

  function SubmoduloPresente(const ASub: string): boolean;
  var
    vArquivos: TStringList;
  begin
    vArquivos := TStringList.Create;
    try
      FOrigem.Listar(ASub, vArquivos);
      Result := vArquivos.Count > 0;
    finally
      vArquivos.Free;
    end;
  end;

  // requires de todas as dependencias internas, transitivamente; o conjunto
  // de visitados impede que um ciclo gire para sempre
  procedure Herdar(APacote: TPacote);
  var
    vDep: integer;
    vFilho: TPacote;
  begin
    for vDep := 0 to Pred(APacote.FInternos.Count) do
    begin
      if Contem(vVisitados, APacote.FInternos[vDep]) then
        Continue;
      vVisitados.Add(APacote.FInternos[vDep]);
      vFilho := Buscar(APacote.Tipo, APacote.FInternos[vDep]);
      if vFilho = nil then
        Continue;
      vHerdados.AddStrings(vFilho.FRequires);
      Herdar(vFilho);
    end;
  end;

begin
  for vInt := 0 to Pred(Count) do
  begin
    vPacote := Pacotes[vInt];
    vPacote.FInternos.Clear;
    vPacote.FExternos.Clear;
    for vReq := 0 to Pred(vPacote.FRequires.Count) do
      if Buscar(vPacote.Tipo, vPacote.FRequires[vReq]) <> nil then
        vPacote.FInternos.Add(Buscar(vPacote.Tipo, vPacote.FRequires[vReq]).Nome)
      else
        vPacote.FExternos.Add(vPacote.FRequires[vReq]);

    // unidade dentro de submodulo nao e "ausente": e um download a mais, que
    // vale inclusive quando a arvore local ja o tem (o zipball nao trara)
    vPacote.FFontesAusentes.Clear;
    vPacote.FSubmodulos.Clear;
    vPacote.FSubmodulosAusentes.Clear;
    for vUnidade in vPacote.FUnidades do
    begin
      vSub := SubmoduloDe(vUnidade);
      if vSub <> '' then
      begin
        if not Contem(vPacote.FSubmodulos, vSub) then
          vPacote.FSubmodulos.Add(vSub);
        if not FOrigem.Existe(vUnidade) and
           not Contem(vPacote.FSubmodulosAusentes, vSub) then
          vPacote.FSubmodulosAusentes.Add(vSub);
      end
      else if not FOrigem.Existe(vUnidade) then
        vPacote.FFontesAusentes.Add(vUnidade);
    end;
  end;

  // o .dpk nem sempre lista as unidades dos submodulos: o RALBSONStorage.dpk
  // nao lista o kxBSON, que o raldbbson.lpk lista. Pacote que divide uma
  // unidade com um pacote da outra IDE herda os submodulos dele — sem isso o
  // download da versao nao traria o kxBSON para o Delphi
  vHerdados := NovaLista;
  try
    for vInt := 0 to Pred(Count) do
    begin
      vPacote := Pacotes[vInt];
      vHerdados.Clear;
      for vReq := 0 to Pred(Count) do
        if (Pacotes[vReq].Tipo <> vPacote.Tipo) and
           DividemUnidade(vPacote, Pacotes[vReq]) then
          for vSub in Pacotes[vReq].FSubmodulos do
            if not Contem(vPacote.FSubmodulos, vSub) and not Contem(vHerdados, vSub) then
              vHerdados.Add(vSub);
      for vSub in vHerdados do
      begin
        vPacote.FSubmodulos.Add(vSub);
        if not SubmoduloPresente(vSub) then
          vPacote.FSubmodulosAusentes.Add(vSub);
      end;
    end;
  finally
    vHerdados.Free;
  end;

  // implicito so interessa quando nada no grafo o traz: o rtl que o
  // PascalRAL exige ja chega a todo pacote que depende dele, e pacote do
  // proprio catalogo chega pela ordem
  vHerdados := NovaLista;
  vVisitados := NovaLista;
  try
    for vInt := 0 to Pred(Count) do
    begin
      vPacote := Pacotes[vInt];
      if vPacote.FImplicitos.Count = 0 then
        Continue;
      vHerdados.Clear;
      vVisitados.Clear;
      Herdar(vPacote);
      for vReq := Pred(vPacote.FImplicitos.Count) downto 0 do
        if Contem(vHerdados, vPacote.FImplicitos[vReq]) or
           (Buscar(vPacote.Tipo, vPacote.FImplicitos[vReq]) <> nil) then
          vPacote.FImplicitos.Delete(vReq);
    end;
  finally
    vHerdados.Free;
    vVisitados.Free;
  end;
end;

function CompararPacotes(AItem1, AItem2: Pointer): integer;
var
  vA, vB: TPacote;
begin
  // desempate estavel da ordem topologica: raiz antes de subpasta, depois nome
  vA := TPacote(AItem1);
  vB := TPacote(AItem2);
  Result := Ord(vA.Grupo <> '') - Ord(vB.Grupo <> '');
  if Result = 0 then
    Result := CompareText(vA.Grupo, vB.Grupo);
  if Result = 0 then
    Result := CompareText(vA.Nome, vB.Nome);
end;

procedure TCatalogo.Ordenar(ATipo: TTipoPacote);
var
  vPendentes, vProntos: TList;
  vFeitos: TStringList;
  vInt, vDep, vOrdem: integer;
  vPacote: TPacote;
  vLivre: boolean;
  vCiclo: string;
begin
  // Kahn: a cada passada entram os pacotes cujas dependencias internas ja
  // entraram; entre os que ficam livres na mesma passada vale o desempate
  vPendentes := TList.Create;
  vProntos := TList.Create;
  vFeitos := NovaLista;
  try
    for vInt := 0 to Pred(Count) do
      if Pacotes[vInt].Tipo = ATipo then
      begin
        Pacotes[vInt].FOrdem := -1;
        vPendentes.Add(Pacotes[vInt]);
      end;

    vOrdem := 0;
    while vPendentes.Count > 0 do
    begin
      vProntos.Clear;
      for vInt := 0 to Pred(vPendentes.Count) do
      begin
        vPacote := TPacote(vPendentes[vInt]);
        vLivre := True;
        for vDep := 0 to Pred(vPacote.FInternos.Count) do
          if not Contem(vFeitos, vPacote.FInternos[vDep]) then
          begin
            vLivre := False;
            Break;
          end;
        if vLivre then
          vProntos.Add(vPacote);
      end;

      if vProntos.Count = 0 then
      begin
        // sobrou ciclo: fica sem ordem e o plano recusa esses pacotes
        vCiclo := '';
        for vInt := 0 to Pred(vPendentes.Count) do
          vCiclo := vCiclo + TPacote(vPendentes[vInt]).Nome + ' ';
        FErros.Add(Format(emCatalogoCiclo,
                          [NomeTipoPacote(ATipo), Trim(vCiclo)]));
        Break;
      end;

      vProntos.Sort(@CompararPacotes);
      for vInt := 0 to Pred(vProntos.Count) do
      begin
        vPacote := TPacote(vProntos[vInt]);
        vPacote.FOrdem := vOrdem;
        Inc(vOrdem);
        vFeitos.Add(vPacote.Nome);
        vPendentes.Remove(vPacote);
      end;
    end;
  finally
    vPendentes.Free;
    vProntos.Free;
    vFeitos.Free;
  end;
end;

function TCatalogo.Carregar(const ARaizRepo: string): boolean;
begin
  Result := Carregar(TOrigemLocal.Create(ARaizRepo));
end;

function TCatalogo.Carregar(AOrigem: TOrigemArquivos): boolean;
var
  vArquivos: TStringList;
  vArquivo: string;
begin
  Limpar;
  FOrigem := AOrigem;

  LerSubmodulos;

  vArquivos := TStringList.Create;
  try
    ListarPacotes(FPastaDelphi, '.dpk', vArquivos);
    vArquivos.Sort;
    for vArquivo in vArquivos do
      LerDpk(vArquivo);

    vArquivos.Clear;
    ListarPacotes(FPastaLazarus, '.lpk', vArquivos);
    vArquivos.Sort;
    for vArquivo in vArquivos do
      LerLpk(vArquivo);
  finally
    vArquivos.Free;
  end;

  Resolver;
  Ordenar(tpDelphi);
  Ordenar(tpLazarus);
  Result := Count > 0;
end;

function TCatalogo.Buscar(ATipo: TTipoPacote; const ANome: string): TPacote;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(Count) do
    if (Pacotes[vInt].Tipo = ATipo) and SameText(Pacotes[vInt].Nome, ANome) then
      Exit(Pacotes[vInt]);
end;

function CompararOrdem(AItem1, AItem2: Pointer): integer;
begin
  Result := TPacote(AItem1).Ordem - TPacote(AItem2).Ordem;
end;

procedure TCatalogo.Listar(ATipo: TTipoPacote; ALista: TList);
var
  vInt: integer;
begin
  ALista.Clear;
  for vInt := 0 to Pred(Count) do
    if (Pacotes[vInt].Tipo = ATipo) and (Pacotes[vInt].Ordem >= 0) then
      ALista.Add(Pacotes[vInt]);
  ALista.Sort(@CompararOrdem);
end;

procedure TCatalogo.Fechamento(ATipo: TTipoPacote; ANomes: TStrings; ALista: TList;
  ADesconhecidos: TStrings);
var
  vFila: TStringList;
  vInt: integer;
  vPacote: TPacote;
begin
  ALista.Clear;
  vFila := NovaLista;
  try
    vFila.AddStrings(ANomes);
    vInt := 0;
    // a fila cresce enquanto anda: cada pacote traz as dependencias dele
    while vInt < vFila.Count do
    begin
      vPacote := Buscar(ATipo, vFila[vInt]);
      if vPacote = nil then
      begin
        if ADesconhecidos <> nil then
          ADesconhecidos.Add(vFila[vInt]);
      end
      else if ALista.IndexOf(vPacote) < 0 then
      begin
        ALista.Add(vPacote);
        vFila.AddStrings(vPacote.Internos);
      end;
      Inc(vInt);
    end;
  finally
    vFila.Free;
  end;
  ALista.Sort(@CompararOrdem);
end;

function TCatalogo.Plano(ATipo: TTipoPacote; ANomes: TStrings): string;
var
  vLista: TList;
  vDesconhecidos, vExternos, vSubmodulosUsados, vSaida: TStringList;
  vInt: integer;
  vPacote: TPacote;
  vLinha, vSub: string;
begin
  vLista := TList.Create;
  vDesconhecidos := NovaLista;
  vExternos := NovaLista;
  vSubmodulosUsados := NovaLista;
  vSaida := TStringList.Create;
  try
    vExternos.Sorted := True;
    vSubmodulosUsados.Sorted := True;
    if ANomes = nil then
      Listar(ATipo, vLista)
    else
      Fechamento(ATipo, ANomes, vLista, vDesconhecidos);

    vSaida.Add(Format(cmCatalogoPlano,
                      [NomeTipoPacote(ATipo), vLista.Count, Raiz]));
    vSaida.Add('');
    for vInt := 0 to Pred(vLista.Count) do
    begin
      vPacote := TPacote(vLista[vInt]);
      if vPacote.Instalavel then
        vLinha := cmCompilarInstalar
      else
        vLinha := cmSoCompilar;
      vSaida.Add(Format('%2d. %-22s %-15s %-20s %s',
        [vInt + 1, vPacote.Nome, NomeUso(vPacote.Uso), vLinha,
         vPacote.ArquivoRelativo]));
      if vPacote.Internos.Count > 0 then
        vSaida.Add(Format(cmCatalogoDepoisDe, [Juntar(vPacote.Internos)]));
      if vPacote.Implicitos.Count > 0 then
        vSaida.Add(Format(cmCatalogoImplicitos, [Juntar(vPacote.Implicitos)]));
      if vPacote.Variaveis.Count > 0 then
        vSaida.Add(Format(cmCatalogoVariaveis,
          ['$(' + StringReplace(vPacote.Variaveis.CommaText, ',', '), $(',
                                [rfReplaceAll]) + ')']));
      if vPacote.CaminhosBusca.Count > 0 then
        vSaida.Add(Format(cmCatalogoCaminhoBusca, [Juntar(vPacote.CaminhosBusca)]));
      if vPacote.Unidades.Count = 0 then
        vSaida.Add(cmCatalogoVazio);
      for vSub in vPacote.Submodulos do
      begin
        vLinha := Format(cmCatalogoSubmodulo, [vSub, Submodulos.Values[vSub]]);
        if Contem(vPacote.SubmodulosAusentes, vSub) then
          vLinha := vLinha + cmCatalogoBaixar;
        vSaida.Add(vLinha);
        vSubmodulosUsados.Add(vSub);
      end;
      if vPacote.FontesAusentes.Count > 0 then
        vSaida.Add(Format(cmCatalogoFontesAusentes, [Juntar(vPacote.FontesAusentes)]));
      vExternos.AddStrings(vPacote.Externos);
    end;

    vSaida.Add('');
    if vExternos.Count > 0 then
      vSaida.Add(Format(cmCatalogoExternos, [Juntar(vExternos)]));
    if vSubmodulosUsados.Count > 0 then
      vSaida.Add(Format(cmCatalogoSubmodulos, [Juntar(vSubmodulosUsados)]));
    if vDesconhecidos.Count > 0 then
      vSaida.Add(Format(cmCatalogoDesconhecidos, [Juntar(vDesconhecidos)]));
    for vInt := 0 to Pred(FErros.Count) do
      vSaida.Add(cmPrefixoErro + FErros[vInt]);

    Result := vSaida.Text;
  finally
    vLista.Free;
    vDesconhecidos.Free;
    vExternos.Free;
    vSubmodulosUsados.Free;
    vSaida.Free;
  end;
end;

end.
