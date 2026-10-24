/// The RAL an IDE already has, whoever installed it: the installer (there is a
/// receipt) or the user by hand (Tools > Options, Install Packages, the OPM).
/// It is read from the IDE itself, the same places an installation writes:
/// - Delphi: the $(PascalRAL) variable, the Known Packages (32 and 64-bit IDE)
///   and the library path entries that are RAL's;
/// - Lazarus: the package links (packagefiles.xml) and the packages built into
///   the IDE (StaticAutoInstallPackages).
/// A package counts as RAL's by name: the names of the chosen version's
/// catalog, the names in the pkg folder of the RAL tree the IDE points to,
/// PascalRAL/PascalRALDsgn, which every version has, and every 1.x name
/// (RALInst.Heranca), which a 2.0 no longer has. RALRESTDW and other
/// packages built on RAL are not RAL: they are never counted, nor removed.
unit RALInst.Existente;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALInst.Catalogo, RALInst.IDE;

type
  /// What of RAL one IDE already has.
  TInstalacaoExistente = class
  private
    FCaminhos: TStringList;
    FFontes: string;
    FLinks: TStringList;
    FNomes: TStringList;
    FPacotes: TStringList;
    FPacotes64: TStringList;
    FSoRuntime: boolean;
    FVariavel: string;
    FVersaoRAL: string;
    /// Adds the names of the packages in <root>/pkg/<kind>
    procedure NomesDaPasta(const ARaiz: string; ATipo: TTipoPacote);
  public
    constructor Create;
    destructor Destroy; override;
    /// Is the name a RAL package?
    function DoRAL(const ANome: string): boolean;
    /// Anything of RAL at all
    function Existe: boolean;
    /// Package names found, without repetition (32 and 64-bit, links)
    procedure ListarNomes(ALista: TStrings);
    /// One line for humans: 'PascalRAL, IndyRAL (D:\...\PascalRAL-dev)'
    function Resumo: string;

    /// Delphi: 'Win32|$(PascalRAL)\base' for every RAL library path entry
    property Caminhos: TStringList read FCaminhos;
    /// Root of the RAL tree the IDE points to ('' when unknown)
    property Fontes: string read FFontes write FFontes;
    /// Lazarus: name=lpk of every RAL package link
    property Links: TStringList read FLinks;
    /// The names that count as RAL's
    property Nomes: TStringList read FNomes;
    /// Delphi: name=bpl in Known Packages; Lazarus: names built into the IDE
    property Pacotes: TStringList read FPacotes;
    /// Delphi: name=bpl in Known Packages x64 (the 64-bit IDE)
    property Pacotes64: TStringList read FPacotes64;
    /// F18: RAL is there for the user's projects only (compiled packages and
    /// the library path in Delphi, the links in Lazarus) and nothing of it is
    /// in the IDE
    property SoRuntime: boolean read FSoRuntime write FSoRuntime;
    /// Delphi: the value of $(PascalRAL) ('' when there is none)
    property Variavel: string read FVariavel write FVariavel;
    /// The RAL version the tree declares (RALVERSION: '1.2.0', '2.0.0'); '' when
    /// unknown
    property VersaoRAL: string read FVersaoRAL write FVersaoRAL;
  end;

{$IFDEF MSWINDOWS}
/// What of RAL a Delphi has. AChave: HKCU key ('' = the IDE's; the tests use
/// a copy). ANomes: the catalog names (may be nil)
function DetectarDelphi(AIDE: TIDEInstance; const AChave: string;
  ANomes: TStrings): TInstalacaoExistente;
{$ENDIF}
/// What of RAL a Lazarus has, in its configuration (AIDE.ConfigDir).
/// ANomes: the catalog names (may be nil)
function DetectarLazarus(AIDE: TIDEInstance; ANomes: TStrings): TInstalacaoExistente;
/// Package name of a .bpl: 'C:\...\IndyRAL290.bpl' -> 'IndyRAL' when that is
/// a known name, else the file name without extension
function NomeDoBpl(const ABpl: string; ANomes: TStrings): string;

implementation

uses
  {$IFDEF MSWINDOWS} RALInst.Registro.Delphi, {$ENDIF}
  RALInst.Compatibilidade, RALInst.Config.Lazarus, RALInst.Heranca, RALInst.Mensagens;

// a versao que a arvore declara (RALConsts.pas); '' quando nao diz
function VersaoDaArvore(const ARaiz: string): string;
var
  vOrigem: TOrigemLocal;
begin
  Result := '';
  if (ARaiz = '') or not DirectoryExists(ARaiz) then
    Exit;
  vOrigem := TOrigemLocal.Create(ARaiz);
  try
    Result := VersaoRALDaOrigem(vOrigem);
  finally
    vOrigem.Free;
  end;
end;

const
  // o nucleo existe em toda versao do RAL, com estes nomes
  NomesBase: array[0..1] of string = ('PascalRAL', 'PascalRALDsgn');

procedure ListarArquivos(const APasta, AMascara: string; ALista: TStrings);
var
  vBusca: TSearchRec;
  vPasta: string;
begin
  vPasta := IncludeTrailingPathDelimiter(APasta);
  if FindFirst(vPasta + AMascara, faAnyFile, vBusca) = 0 then
  try
    repeat
      if (vBusca.Attr and faDirectory) = 0 then
        ALista.Add(vPasta + vBusca.Name);
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
  if FindFirst(vPasta + '*', faDirectory, vBusca) = 0 then
  try
    repeat
      if ((vBusca.Attr and faDirectory) <> 0) and (vBusca.Name <> '.') and
         (vBusca.Name <> '..') then
        ListarArquivos(vPasta + vBusca.Name, AMascara, ALista);
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

function NomeDoBpl(const ABpl: string; ANomes: TStrings): string;
var
  vNome: string;
begin
  vNome := ChangeFileExt(ExtractFileName(ABpl), '');
  Result := vNome;
  if (ANomes = nil) or (ANomes.IndexOf(vNome) >= 0) then
    Exit;
  // {$LIBSUFFIX}: IndyRAL290.bpl e o IndyRAL
  while (vNome <> '') and (vNome[Length(vNome)] in ['0'..'9']) do
    Delete(vNome, Length(vNome), 1);
  if ANomes.IndexOf(vNome) >= 0 then
    Result := vNome;
end;

{ TInstalacaoExistente }

constructor TInstalacaoExistente.Create;

  function NovaLista: TStringList;
  begin
    Result := TStringList.Create;
    Result.CaseSensitive := False;
  end;

var
  vNome: string;
  vLista: TStringList;
begin
  inherited Create;
  FCaminhos := NovaLista;
  FLinks := NovaLista;
  FNomes := NovaLista;
  FNomes.Sorted := True;
  FNomes.Duplicates := dupIgnore;
  FPacotes := NovaLista;
  FPacotes64 := NovaLista;
  for vNome in NomesBase do
    FNomes.Add(vNome);
  // os pacotes da 1.x: atualizada para a 2.0, uma IDE ainda os tem, e a
  // arvore antiga pode nem existir mais para dizer que sao do RAL
  vLista := TStringList.Create;
  try
    NomesDa1x(vLista);
    FNomes.AddStrings(vLista);
  finally
    vLista.Free;
  end;
end;

destructor TInstalacaoExistente.Destroy;
begin
  FVariavel := '';
  FPacotes64.Free;
  FPacotes.Free;
  FNomes.Free;
  FLinks.Free;
  FCaminhos.Free;
  inherited Destroy;
end;

function TInstalacaoExistente.DoRAL(const ANome: string): boolean;
begin
  Result := FNomes.IndexOf(ANome) >= 0;
end;

function TInstalacaoExistente.Existe: boolean;
begin
  Result := (FPacotes.Count > 0) or (FPacotes64.Count > 0) or (FLinks.Count > 0) or
            (FCaminhos.Count > 0) or (FVariavel <> '');
end;

procedure TInstalacaoExistente.ListarNomes(ALista: TStrings);
var
  vInt: integer;

  procedure Juntar(const ANome: string);
  begin
    if ALista.IndexOf(ANome) < 0 then
      ALista.Add(ANome);
  end;

begin
  ALista.Clear;
  for vInt := 0 to Pred(FPacotes.Count) do
    Juntar(FPacotes.Names[vInt]);
  for vInt := 0 to Pred(FPacotes64.Count) do
    Juntar(FPacotes64.Names[vInt]);
end;

procedure TInstalacaoExistente.NomesDaPasta(const ARaiz: string; ATipo: TTipoPacote);
var
  vArquivos: TStringList;
  vArquivo, vPasta, vMascara: string;
begin
  if ATipo = tpDelphi then
  begin
    vPasta := 'Delphi';
    vMascara := '*.dpk';
  end
  else
  begin
    vPasta := 'Lazarus';
    vMascara := '*.lpk';
  end;
  vPasta := IncludeTrailingPathDelimiter(ARaiz) + 'pkg' + PathDelim + vPasta;
  if not DirectoryExists(vPasta) then
    Exit;
  vArquivos := TStringList.Create;
  try
    ListarArquivos(vPasta, vMascara, vArquivos);
    for vArquivo in vArquivos do
      FNomes.Add(ChangeFileExt(ExtractFileName(vArquivo), ''));
  finally
    vArquivos.Free;
  end;
end;

function TInstalacaoExistente.Resumo: string;
var
  vNomes: TStringList;
  vInt: integer;
begin
  vNomes := TStringList.Create;
  try
    ListarNomes(vNomes);
    if vNomes.Count = 0 then
      for vInt := 0 to Pred(FLinks.Count) do
        vNomes.Add(FLinks.Names[vInt]);
    Result := StringReplace(vNomes.CommaText, ',', ', ', [rfReplaceAll]);
    // so a variavel e o library path: o RAL usado como fontes, sem pacotes
    if (Result = '') and FSoRuntime then
      Result := cmExistenteSoRuntime
    else if (Result = '') and ((FCaminhos.Count > 0) or (FVariavel <> '')) then
      Result := cmExistenteSoPaths;
    if (FVersaoRAL <> '') and (FFontes <> '') then
      Result := Trim(Result + ' (RAL ' + FVersaoRAL + ', ' +
                     ExcludeTrailingPathDelimiter(FFontes) + ')')
    else if FFontes <> '' then
      Result := Trim(Result + ' (' + ExcludeTrailingPathDelimiter(FFontes) + ')');
  finally
    vNomes.Free;
  end;
end;

{$IFDEF MSWINDOWS}
function DetectarDelphi(AIDE: TIDEInstance; const AChave: string;
  ANomes: TStrings): TInstalacaoExistente;
var
  vReg: TRegistroDelphi;
  vValores, vPlataformas, vItens: TStringList;
  vInt, vPlat: integer;
  vRaiz, vNome, vValor, vExpandido, vSub: string;
  vDoRAL: boolean;
begin
  Result := TInstalacaoExistente.Create;
  if ANomes <> nil then
    Result.Nomes.AddStrings(ANomes);
  vReg := TRegistroDelphi.Create(AIDE);
  vValores := TStringList.Create;
  vPlataformas := TStringList.Create;
  vItens := TStringList.Create;
  try
    if AChave <> '' then
      vReg.Chave := AChave;
    if (vReg.Chave = '') or not vReg.ChaveExiste then
      Exit;

    // $(PascalRAL) aponta para <raiz>\src: a raiz diz os pacotes daquela arvore
    Result.Variavel := vReg.LerValor('Environment Variables', 'PascalRAL');
    if Result.Variavel <> '' then
    begin
      vRaiz := ExcludeTrailingPathDelimiter(vReg.Expandir(Result.Variavel));
      if SameText(ExtractFileName(vRaiz), 'src') then
        vRaiz := ExtractFileDir(vRaiz);
      if DirectoryExists(IncludeTrailingPathDelimiter(vRaiz) + 'pkg') then
      begin
        Result.Fontes := IncludeTrailingPathDelimiter(vRaiz);
        Result.NomesDaPasta(vRaiz, tpDelphi);
      end;
    end;

    // pacotes registrados, nas duas IDEs
    vReg.ListarValores('Known Packages', vValores);
    for vValor in vValores do
    begin
      vNome := NomeDoBpl(vReg.Expandir(vValor), Result.Nomes);
      if Result.DoRAL(vNome) then
        Result.Pacotes.Add(vNome + '=' + vValor);
    end;
    vReg.ListarValores('Known Packages x64', vValores);
    for vValor in vValores do
    begin
      vNome := NomeDoBpl(vReg.Expandir(vValor), Result.Nomes);
      if Result.DoRAL(vNome) then
        Result.Pacotes64.Add(vNome + '=' + vValor);
    end;

    // library path: o que usa $(PascalRAL) ou fica dentro da arvore do RAL
    if vReg.ChaveLibrary('win64') = 'Library' then
      vPlataformas.Add('')
    else
      vReg.ListarSubchaves('Library', vPlataformas);
    for vPlat := 0 to Pred(vPlataformas.Count) do
    begin
      if vPlataformas[vPlat] = '' then
        vSub := 'Library'
      else
        vSub := 'Library\' + vPlataformas[vPlat];
      vItens.StrictDelimiter := True;
      vItens.Delimiter := ';';
      vItens.DelimitedText := vReg.LerValor(vSub, 'Search Path');
      for vInt := 0 to Pred(vItens.Count) do
      begin
        vValor := Trim(vItens[vInt]);
        if vValor = '' then
          Continue;
        vDoRAL := Pos('$(PASCALRAL)', UpperCase(vValor)) > 0;
        if not vDoRAL and (Result.Fontes <> '') then
        begin
          vExpandido := IncludeTrailingPathDelimiter(vReg.Expandir(vValor));
          vDoRAL := SameText(Copy(vExpandido, 1, Length(Result.Fontes)), Result.Fontes);
        end;
        if vDoRAL and (Result.Caminhos.IndexOf(vSub + '|' + vValor) < 0) then
          Result.Caminhos.Add(vSub + '|' + vValor);
      end;
    end;

    // F18: nada na IDE, mas o library path e o .dcp do nucleo onde a IDE
    // procura pacotes: o RAL so para os projetos que compilam com pacotes
    Result.SoRuntime := (Result.Pacotes.Count = 0) and (Result.Pacotes64.Count = 0) and
      (Result.Caminhos.Count > 0) and
      (FileExists(AIDE.CommonDir + 'Dcp' + PathDelim + 'PascalRAL.dcp') or
       FileExists(vReg.Expandir(vReg.LerValor(vReg.ChaveLibrary('win32'),
                                              'Package DCP Output')) + PathDelim +
                  'PascalRAL.dcp'));
    // F21: a versao que a arvore declara (RALVERSION 2.0.0 numa 2.0 a mao)
    Result.VersaoRAL := VersaoDaArvore(Result.Fontes);
  finally
    vItens.Free;
    vPlataformas.Free;
    vValores.Free;
    vReg.Free;
  end;
end;
{$ENDIF}

function DetectarLazarus(AIDE: TIDEInstance; ANomes: TStrings): TInstalacaoExistente;
var
  vLinks, vInstalados: TStringList;
  vInt: integer;
  vArquivo, vRaiz, vFim: string;
begin
  Result := TInstalacaoExistente.Create;
  if ANomes <> nil then
    Result.Nomes.AddStrings(ANomes);
  if AIDE.ConfigDir = '' then
    Exit;
  vLinks := TStringList.Create;
  vInstalados := TStringList.Create;
  try
    LerLinks(AIDE.ConfigDir, vLinks);
    LerInstalados(AIDE.ConfigDir, vInstalados);

    // o link do pascalral.lpk diz a raiz: <raiz>/pkg/Lazarus/pascalral.lpk
    vFim := PathDelim + 'pkg' + PathDelim + 'Lazarus' + PathDelim + 'pascalral.lpk';
    vInt := vLinks.IndexOfName('PascalRAL');
    if vInt >= 0 then
    begin
      vArquivo := SetDirSeparators(StringReplace(vLinks.ValueFromIndex[vInt],
        '$(LazarusDir)', ExcludeTrailingPathDelimiter(AIDE.RootDir),
        [rfReplaceAll, rfIgnoreCase]));
      if SameText(Copy(vArquivo, Length(vArquivo) - Length(vFim) + 1, MaxInt), vFim) then
      begin
        vRaiz := Copy(vArquivo, 1, Length(vArquivo) - Length(vFim));
        Result.Fontes := IncludeTrailingPathDelimiter(vRaiz);
        Result.NomesDaPasta(vRaiz, tpLazarus);
      end;
    end;

    for vInt := 0 to Pred(vLinks.Count) do
      if Result.DoRAL(vLinks.Names[vInt]) then
        Result.Links.Add(vLinks[vInt]);
    for vInt := 0 to Pred(vInstalados.Count) do
      if Result.DoRAL(vInstalados[vInt]) then
        Result.Pacotes.Add(vInstalados[vInt] + '=');
    // F18: so os links, nada na IDE: os projetos exigem os pacotes
    Result.SoRuntime := (Result.Links.Count > 0) and (Result.Pacotes.Count = 0);
    Result.VersaoRAL := VersaoDaArvore(Result.Fontes);
  finally
    vInstalados.Free;
    vLinks.Free;
  end;
end;

end.
