/// Compiles the RAL packages in a Delphi from the command line, from the FPC
/// executable: dcc32/dcc64 for the packages and brcc32 for the missing .res. No
/// Delphi project, no msbuild, the IDE is never opened. Three things make the
/// difference between a .bpl the IDE accepts and one it refuses:
/// - --no-config ignores the machine's dcc32.cfg, which makes the compilation
///   the same on every machine; in exchange, everything must be said in the
///   command line.
/// - without -LU the compiler links Indy, FireDAC and the like *inside* the
///   .bpl: it compiles clean and the IDE refuses it with a duplicate unit error.
///   -LU comes from requires plus the implicit dependencies (IndyRAL's Indy),
///   each confirmed by the .dcp on disk, and the updated Indy suffix comes from
///   the file name itself, never from a guess.
/// - design-time in Delphi is always Win32.
unit RALInst.Build.Delphi;

{$mode ObjFPC}{$H+}

{$IFNDEF MSWINDOWS}
  {$FATAL RALInst.Build.Delphi so compila no Windows}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALInst.Catalogo, RALInst.IDE, RALInst.Processo;

type
  /// Outcome of one package.
  TResultadoPacote = record
    Bpl: string;
    Dcp: string;
    Motivo: string;
    Nome: string;
    Ok: boolean;
    Pulado: boolean;
    Segundos: double;
    TamanhoBpl: int64;
  end;

  /// Compiles a list of packages for one IDE and platform.
  TBuildDelphi = class
  private
    FCaminhosExtras: TStringList;
    FCatalogo: TCatalogo;
    FDesign64: boolean;
    FIDE: TIDEInstance;
    FLog: TLogLinha;
    FPacotesExtras: TStringList;
    FPastaBpl: string;
    FPastaDcp: string;
    FPastaDcu: string;
    FPlataforma: string;
    FRaizFontes: string;
    FResultados: array of TResultadoPacote;
    FSimular: boolean;
    /// A .dcp in the output, the IDE, lib\release or the extra paths; the real
    /// name carries the IDE suffix when the file does
    function BuscarDcp(const ANome: string; out ANomeReal: string): boolean;
    /// dcc32.exe or dcc64.exe
    function Compilador: string;
    /// Compiles one package
    function CompilarPacote(APacote: TPacote; var AResultado: TResultadoPacote): boolean;
    /// Does the package use a third-party package (so a big .bpl is suspect)?
    function EsperaPacoteDeTerceiro(APacote: TPacote): boolean;
    /// A tool in the IDE bin folder
    function Ferramenta(const ANome: string): string;
    /// Creates the .res the repository does not version
    function GarantirRes(APacote: TPacote): boolean;
    function GetResultado(AIndex: integer): TResultadoPacote;
    function GetTotal: integer;
    /// lib\<platform>\release of the IDE
    function LibRelease: string;
    /// Passes the line to Log when assigned
    procedure Logar(const ALinha: string);
    /// Compiler output, indented
    procedure LogarProcesso(const ALinha: string);
    /// Unit, include and resource search path
    procedure MontarBusca(APacote: TPacote; ALista: TStrings);
    /// -LU: the packages the package is linked against
    procedure MontarLU(APacote: TPacote; ALista: TStrings);
    /// -NS: unit scope names
    procedure MontarNamespaces(APacote: TPacote; ALista: TStrings);
    /// -O: folders of the .obj files the units link
    procedure MontarObjetos(APacote: TPacote; ALista: TStrings);
    /// <BDSCOMMONDIR>\Bpl (plus the platform outside Win32)
    function PastaBplPadrao: string;
    /// <BDSCOMMONDIR>\Dcp (plus the platform when it exists)
    function PastaDcpPadrao: string;
  public
    constructor Create(AIDE: TIDEInstance; ACatalogo: TCatalogo);
    destructor Destroy; override;
    /// Packages in the catalog's order; returns False when one failed
    function Compilar(APacotes: TList): boolean;
    /// One line per package: result, size and remark
    function Relatorio: string;

    /// Library paths of the external dependencies (mORMot2, Zeos...)
    property CaminhosExtras: TStringList read FCaminhosExtras;
    /// Win64 also compiles the design packages, for the 64-bit IDE
    /// (bin64\bds.exe); without it design-time is Win32 only
    property Design64: boolean read FDesign64 write FDesign64;
    property IDE: TIDEInstance read FIDE;
    property Log: TLogLinha read FLog write FLog;
    /// Already resolved dependency .dcp files (AnyDAC_Comp_D16) that enter the
    /// -LU of every package: only what the package really uses is linked
    property PacotesExtras: TStringList read FPacotesExtras;
    /// Output: by default <BDSCOMMONDIR>\Bpl and \Dcp, where the IDE looks
    property PastaBpl: string read FPastaBpl write FPastaBpl;
    property PastaDcp: string read FPastaDcp write FPastaDcp;
    /// Intermediate .dcu; by default <sources>\compiled\<bds>\<platform>
    property PastaDcu: string read FPastaDcu write FPastaDcu;
    /// 'win32' (default) or 'win64'
    property Plataforma: string read FPlataforma write FPlataforma;
    property Resultados[AIndex: integer]: TResultadoPacote read GetResultado;
    /// Builds and shows the commands without running any
    property Simular: boolean read FSimular write FSimular;
    property Total: integer read GetTotal;
  end;

const
  /// Third-party package families that count as an implicit dependency: the
  /// DCC_UsePackage of RAL's .dproj files also carries junk from other projects
  /// (RESTDWCore, uniGUI26Core...), so the list is closed
  ImplicitosConhecidos: array[0..12] of string = (
    'IndyCore', 'IndySystem', 'IndyProtocols',
    'FireDAC', 'FireDACCommon', 'FireDACCommonDriver',
    // o cursor de espera do FireDAC (FireDAC.VCLUI.Wait / FMXUI.Wait)
    'vclFireDAC', 'fmxFireDAC',
    'ZComponent', 'ZCore', 'ZDbc', 'ZParseSql', 'ZPlain'
  );

  /// Scope names every package needs; the .dproj adds its own
  NamespacesPadrao = 'System;System.Win;Winapi;Vcl;Vcl.Imaging;Data;Data.Win;' +
                     'Xml;Web;Soap;Datasnap;Bde';

  /// Delphi's own runtime packages that -LU always offers
  /// (DbxCommonDriver: Data.DBXJSON, the JSON of RAL on Delphi before XE6)
  PacotesBase: array[0..9] of string = (
    'rtl', 'vcl', 'vclx', 'vclimg', 'vcldb', 'dbrtl', 'xmlrtl', 'soaprtl', 'inet',
    'DbxCommonDriver'
  );

implementation

uses
  StrUtils, RegExpr,
  RALInst.Mensagens;

function LerArquivoTexto(const AArquivo: string): string;
var
  vLista: TStringList;
begin
  Result := '';
  if not FileExists(AArquivo) then
    Exit;
  vLista := TStringList.Create;
  try
    vLista.LoadFromFile(AArquivo);
    Result := vLista.Text;
  finally
    vLista.Free;
  end;
end;

function TamanhoArquivo(const AArquivo: string): int64;
var
  vSearch: TSearchRec;
begin
  Result := 0;
  if FindFirst(AArquivo, faAnyFile, vSearch) = 0 then
  begin
    Result := vSearch.Size;
    FindClose(vSearch);
  end;
end;

// 'F2613 Unit 'libsagui' not found' / 'E1026 File not found: 'x.obj'' -> o nome
function UnidadeFaltando(ASaida: TStrings): string;
var
  vRegex: TRegExpr;
  vInt: integer;
begin
  Result := '';
  // F2063 ("could not compile used unit") nao entra: a unidade existe e tem
  // erro — nao e dependencia ausente
  vRegex := TRegExpr.Create('(?:F2613 Unit|E1026 File not found:|' +
                            'E2202 Required package)' +
                            '\s*''?([^'']+)''?');
  try
    for vInt := 0 to Pred(ASaida.Count) do
      if vRegex.Exec(ASaida[vInt]) then
        Exit(Trim(vRegex.Match[1]));
  finally
    vRegex.Free;
  end;
end;

// 'X.pas(219) Error: E2003 Undeclared identifier: 'scsCA'' -> 'X.pas(219): E2003 ...'
function PrimeiroErro(ASaida: TStrings): string;
var
  vInt, vPos: integer;
  vLinha: string;
begin
  Result := '';
  for vInt := 0 to Pred(ASaida.Count) do
  begin
    vLinha := ASaida[vInt];
    vPos := Pos(' Error: ', vLinha);
    if vPos = 0 then
      vPos := Pos(' Fatal: ', vLinha);
    if vPos > 0 then
      Exit(ExtractFileName(Trim(Copy(vLinha, 1, vPos - 1))) + ': ' +
           Trim(Copy(vLinha, vPos + 8, MaxInt)));
  end;
end;

function Juntar(ALista: TStrings; const ASeparador: string): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 0 to Pred(ALista.Count) do
  begin
    if Result <> '' then
      Result := Result + ASeparador;
    Result := Result + ALista[vInt];
  end;
end;

{ TBuildDelphi }

constructor TBuildDelphi.Create(AIDE: TIDEInstance; ACatalogo: TCatalogo);
begin
  inherited Create;
  FIDE := AIDE;
  FCatalogo := ACatalogo;
  FPlataforma := 'win32';
  FCaminhosExtras := TStringList.Create;
  FPacotesExtras := TStringList.Create;

  if (FCatalogo <> nil) and (FCatalogo.Origem is TOrigemLocal) then
    FRaizFontes := TOrigemLocal(FCatalogo.Origem).Raiz;

end;

destructor TBuildDelphi.Destroy;
begin
  FPacotesExtras.Free;
  FCaminhosExtras.Free;
  inherited Destroy;
end;

function TBuildDelphi.GetResultado(AIndex: integer): TResultadoPacote;
begin
  Result := FResultados[AIndex];
end;

function TBuildDelphi.GetTotal: integer;
begin
  Result := Length(FResultados);
end;

procedure TBuildDelphi.Logar(const ALinha: string);
begin
  if Assigned(FLog) then
    FLog(ALinha);
end;

procedure TBuildDelphi.LogarProcesso(const ALinha: string);
begin
  Logar('    ' + ALinha);
end;

function TBuildDelphi.Ferramenta(const ANome: string): string;
begin
  Result := FIDE.RootDir + 'bin' + PathDelim + ANome;
end;

function TBuildDelphi.Compilador: string;
begin
  if SameText(FPlataforma, 'win64') then
    Result := Ferramenta('dcc64.exe')
  else
    Result := Ferramenta('dcc32.exe');
end;

function TBuildDelphi.PastaBplPadrao: string;
begin
  // Win32 fica na raiz; as outras plataformas ganham subpasta, do mesmo jeito
  // que a IDE organiza
  Result := FIDE.CommonDir + 'Bpl';
  if not SameText(FPlataforma, 'win32') then
    Result := Result + PathDelim + FPlataforma;
end;

function TBuildDelphi.PastaDcpPadrao: string;
begin
  Result := FIDE.CommonDir + 'Dcp';
  if not SameText(FPlataforma, 'win32') and
     DirectoryExists(Result + PathDelim + FPlataforma) then
    Result := Result + PathDelim + FPlataforma;
end;

function TBuildDelphi.LibRelease: string;
begin
  Result := FIDE.RootDir + 'lib' + PathDelim + LowerCase(FPlataforma) +
            PathDelim + 'release';
end;

function TBuildDelphi.BuscarDcp(const ANome: string; out ANomeReal: string): boolean;
var
  vPastas: TStringList;
  vPasta: string;
begin
  // o Indy atualizado pelo usuario ganha o sufixo da IDE (IndyCore290.dcp); o
  // nativo nao tem sufixo. Quem decide e o disco, nos dois casos.
  Result := False;
  ANomeReal := '';
  vPastas := TStringList.Create;
  try
    if FPastaDcp <> '' then
      vPastas.Add(FPastaDcp);
    // a pasta padrao da IDE entra mesmo quando a saida foi desviada: e onde
    // moram os .dcp de terceiros que o usuario ja instalou (Zeos, Indy novo)
    if FIDE.CommonDir <> '' then
      vPastas.Add(PastaDcpPadrao);
    vPastas.Add(LibRelease);
    vPastas.AddStrings(FCaminhosExtras);

    for vPasta in vPastas do
    begin
      if vPasta = '' then
        Continue;
      if (FIDE.SufixoPacote <> '') and
         FileExists(IncludeTrailingPathDelimiter(vPasta) + ANome + FIDE.SufixoPacote +
                    '.dcp') then
      begin
        ANomeReal := ANome + FIDE.SufixoPacote;
        Exit(True);
      end;
      if FileExists(IncludeTrailingPathDelimiter(vPasta) + ANome + '.dcp') then
      begin
        ANomeReal := ANome;
        Exit(True);
      end;
    end;
  finally
    vPastas.Free;
  end;
end;

function TBuildDelphi.GarantirRes(APacote: TPacote): boolean;
var
  vRes, vRc: string;
  vLista: TStringList;
  vParams: TStringList;
begin
  // o repositorio do RAL nao versiona os .res; sobre um zip recem-baixado o
  // dcc32 morre em E1026. Um .rc vazio vira um .res valido de 32 bytes.
  Result := True;
  vRes := ChangeFileExt(APacote.Arquivo, '.res');
  if FileExists(vRes) then
    Exit;

  vRc := ChangeFileExt(APacote.Arquivo, '.rc');
  Logar(Format(cmGerandoRes, [ExtractFileName(vRes)]));
  if FSimular then
    Exit;

  vLista := TStringList.Create;
  vParams := TStringList.Create;
  try
    if not FileExists(vRc) then
    begin
      vLista.Add('// gerado pelo RAL Installer: o repositório não versiona os .res');
      vLista.SaveToFile(vRc);
    end;
    vParams.Add('-fo' + vRes);
    vParams.Add(vRc);
    Result := RALInst.Processo.Executar(Ferramenta('brcc32.exe'), vParams,
                                        @LogarProcesso);
    if not Result then
      Logar(Format(emBrcc32, [ExtractFileName(vRes)]));
  finally
    vParams.Free;
    vLista.Free;
  end;
end;

function TBuildDelphi.EsperaPacoteDeTerceiro(APacote: TPacote): boolean;
var
  vInt: integer;
  vNome: string;
begin
  Result := False;
  for vInt := 0 to Pred(APacote.Implicitos.Count) do
  begin
    vNome := APacote.Implicitos[vInt];
    if (AnsiIndexText(vNome, ImplicitosConhecidos) >= 0) or
       (Copy(LowerCase(vNome), 1, 7) = 'firedac') then
      Exit(True);
  end;
  for vInt := 0 to Pred(APacote.Externos.Count) do
    if AnsiIndexText(APacote.Externos[vInt], ImplicitosConhecidos) >= 0 then
      Exit(True);
end;

procedure TBuildDelphi.MontarLU(APacote: TPacote; ALista: TStrings);
var
  vNome, vReal: string;
  vInt: integer;

  procedure Tentar(const ANome: string; AObrigatorio: boolean);
  begin
    if ALista.IndexOf(ANome) >= 0 then
      Exit;
    if BuscarDcp(ANome, vReal) then
      ALista.Add(vReal)
    else if AObrigatorio then
      Logar(Format(wmSemDcp, [ANome]));
  end;

begin
  // dependencias internas entram sempre: o .dcp delas e gerado nesta mesma
  // rodada, antes deste pacote, e nao adianta perguntar ao disco
  for vInt := 0 to Pred(APacote.Internos.Count) do
    if ALista.IndexOf(APacote.Internos[vInt]) < 0 then
      ALista.Add(APacote.Internos[vInt]);

  // externas declaradas no requires: rtl, dbrtl, vcl, designide, dcldb...
  for vInt := 0 to Pred(APacote.Externos.Count) do
    Tentar(APacote.Externos[vInt], True);

  // implicitas: so as conhecidas, e so as que existem em disco
  for vInt := 0 to Pred(APacote.Implicitos.Count) do
  begin
    vNome := APacote.Implicitos[vInt];
    // o FireDAC e uma familia grande (um pacote por driver) e todos os
    // pedacos precisam entrar, senao o resto vem estatico no .bpl
    if (AnsiIndexText(vNome, ImplicitosConhecidos) >= 0) or
       (Copy(LowerCase(vNome), 1, 7) = 'firedac') then
      Tentar(vNome, False);
  end;

  // os pacotes-base do proprio Delphi: pacote que usa uma unidade deles sem
  // declarar (o ZPlain do Zeos usa Xml.XMLDoc e nao exige xmlrtl) a IDE
  // resolve sozinha ("implicitly imported"); o dcc32 puro linka a unidade
  // dentro do .bpl, e o proximo pacote cai em E2199 (duas copias). No -LU so
  // entra no .bpl o que for de fato usado
  for vInt := Low(PacotesBase) to High(PacotesBase) do
    Tentar(PacotesBase[vInt], False);

  // os que as dependencias pedem (pacotes-ligados das receitas: o AnyDAC)
  for vInt := 0 to Pred(FPacotesExtras.Count) do
    if ALista.IndexOf(FPacotesExtras[vInt]) < 0 then
      ALista.Add(FPacotesExtras[vInt]);
end;

procedure TBuildDelphi.MontarBusca(APacote: TPacote; ALista: TStrings);
var
  vUnidade, vPasta, vSub, vCandidata: string;
  vInt: integer;
begin
  ALista.Add(LibRelease);
  if FPastaDcp <> '' then
    ALista.Add(FPastaDcp);
  // a pasta padrao da IDE tem os .dcp de terceiros que o usuario instalou
  // (Zeos, Indy atualizado) e nao pode sair da busca nem quando a saida desta
  // rodada foi desviada para outro lugar
  if (FIDE.CommonDir <> '') and (ALista.IndexOf(PastaDcpPadrao) < 0) then
    ALista.Add(PastaDcpPadrao);
  if FPastaDcu <> '' then
    ALista.Add(FPastaDcu);

  // as pastas dos fontes do proprio pacote: o .dpk traz o caminho de cada
  // unidade, mas as unidades usam outras que ele nao lista (o kxBSON, por
  // exemplo, so aparece no .lpk)
  for vUnidade in APacote.Unidades do
  begin
    vPasta := ExcludeTrailingPathDelimiter(ExtractFilePath(
                FRaizFontes + StringReplace(vUnidade, '/', PathDelim,
                                            [rfReplaceAll])));
    if ALista.IndexOf(vPasta) < 0 then
      ALista.Add(vPasta);
  end;

  // o caminho de busca que o .dproj declara: o SaguiRAL acha o libsagui.pas
  // em src\others, que o .dpk nao lista
  for vUnidade in APacote.CaminhosBusca do
  begin
    vPasta := ExcludeTrailingPathDelimiter(FRaizFontes +
                StringReplace(vUnidade, '/', PathDelim, [rfReplaceAll]));
    if ALista.IndexOf(vPasta) < 0 then
      ALista.Add(vPasta);
  end;

  // os submodulos fazem parte da arvore do RAL, mas nem todo pacote Delphi
  // lista as unidades deles (o RALBSONStorage.dpk nao lista o kxBSON): as
  // pastas entram na busca de qualquer jeito
  for vInt := 0 to Pred(FCatalogo.Submodulos.Count) do
  begin
    vPasta := IncludeTrailingPathDelimiter(FRaizFontes +
                StringReplace(FCatalogo.Submodulos.Names[vInt], '/', PathDelim,
                              [rfReplaceAll]));
    for vSub in TStringArray.Create('', 'Source', 'src') do
    begin
      vCandidata := ExcludeTrailingPathDelimiter(vPasta + vSub);
      if DirectoryExists(vCandidata) and (ALista.IndexOf(vCandidata) < 0) then
        ALista.Add(vCandidata);
    end;
  end;

  ALista.AddStrings(FCaminhosExtras);
end;

procedure TBuildDelphi.MontarObjetos(APacote: TPacote; ALista: TStrings);
var
  vUnidade, vPasta, vCandidata: string;

  procedure Tentar(const APastaObj: string);
  begin
    if DirectoryExists(APastaObj) and (ALista.IndexOf(APastaObj) < 0) then
      ALista.Add(APastaObj);
  end;

begin
  // {$L 'libbrotli.obj'}: o .obj nao fica ao lado da unidade, e sim numa
  // subpasta por compilador e plataforma. Sem isto o brotli morre em E1026.
  for vUnidade in APacote.Unidades do
  begin
    vPasta := IncludeTrailingPathDelimiter(ExtractFilePath(
                FRaizFontes + StringReplace(vUnidade, '/', PathDelim,
                                            [rfReplaceAll])));
    for vCandidata in TStringArray.Create(
          'static_libs' + PathDelim + 'delphi' + PathDelim + LowerCase(FPlataforma),
          'static_libs' + PathDelim + LowerCase(FPlataforma),
          'obj' + PathDelim + LowerCase(FPlataforma),
          LowerCase(FPlataforma)) do
      Tentar(ExcludeTrailingPathDelimiter(vPasta + vCandidata));
  end;
end;

procedure TBuildDelphi.MontarNamespaces(APacote: TPacote; ALista: TStrings);
var
  vDproj, vItem: string;
  vRegex: TRegExpr;
begin
  ALista.Delimiter := ';';
  ALista.StrictDelimiter := True;
  ALista.DelimitedText := NamespacesPadrao;

  vDproj := ChangeFileExt(APacote.Arquivo, '.dproj');
  if not FileExists(vDproj) then
    Exit;

  vRegex := TRegExpr.Create('<DCC_Namespace>([^<]*)</DCC_Namespace>');
  try
    if vRegex.Exec(LerArquivoTexto(vDproj)) then
      repeat
        for vItem in vRegex.Match[1].Split([';']) do
          if (Trim(vItem) <> '') and (Pos('$(', vItem) = 0) and
             (ALista.IndexOf(Trim(vItem)) < 0) then
            ALista.Add(Trim(vItem));
      until not vRegex.ExecNext;
  finally
    vRegex.Free;
  end;
end;

function ExigeDesignide(APacote: TPacote): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := 0 to Pred(APacote.Externos.Count) do
    if SameText(APacote.Externos[vInt], 'designide') then
      Exit(True);
end;

function TBuildDelphi.CompilarPacote(APacote: TPacote;
  var AResultado: TResultadoPacote): boolean;
var
  vParams, vLU, vBusca, vObjetos, vNamespaces: TStringList;
  vExec: TExecucao;
  vInicio: QWord;
  vArquivoDpk, vFaltando, vSufixo, vDcpIDE: string;
begin
  AResultado := Default(TResultadoPacote);
  AResultado.Nome := APacote.Nome;

  vArquivoDpk := APacote.Arquivo;
  if not FileExists(vArquivoDpk) then
  begin
    AResultado.Motivo := Format(cmBuildSemArquivo, [vArquivoDpk]);
    Exit(False);
  end;

  // F6: o .dproj nao habilita esta plataforma (o XSocketRAL so tem Win32)
  if (APacote.PlataformasDproj.Count > 0) and
     (APacote.PlataformasDproj.IndexOf(LowerCase(FPlataforma)) < 0) then
  begin
    AResultado.Pulado := True;
    AResultado.Ok := False;
    AResultado.Motivo := Format(cmBuildPlataformaDproj, [FPlataforma]);
    Exit(True);
  end;

  // design-time no Delphi e sempre Win32: para as outras plataformas so faz
  // sentido o pacote de runtime
  if not SameText(FPlataforma, 'win32') and (APacote.Uso = upDesign) and
     not (FDesign64 and SameText(FPlataforma, 'win64')) then
  begin
    AResultado.Pulado := True;
    AResultado.Ok := True;
    AResultado.Motivo := cmBuildDesignWin32;
    Exit(True);
  end;

  // o PascalRALDsgn exige o designide sem ser so de design: compila onde a
  // plataforma tem o designide.dcp (o Win64 do Delphi 12 em diante) e, fora
  // dele, nao tem contra o que compilar, nem quem depende dele
  if not SameText(FPlataforma, 'win32') and
     ExigeDesignide(APacote) and
     not BuscarDcp('designide', vDcpIDE) then
  begin
    AResultado.Pulado := True;
    AResultado.Ok := False;
    AResultado.Motivo := Format(cmBuildSemDesignide, [FPlataforma]);
    Exit(True);
  end;

  // o catalogo ja sabe o que falta; chamar o compilador para ouvir F1026 e
  // desperdicio, e a mensagem dele nao diz o que fazer
  if APacote.SubmodulosAusentes.Count > 0 then
  begin
    AResultado.Motivo := Format(cmBuildSemSubmodulo,
      [StringReplace(APacote.SubmodulosAusentes.CommaText, ',', ', ', [rfReplaceAll])]);
    Logar('  ' + AResultado.Motivo + wmBuildSemSubmodulo);
    Exit(False);
  end;
  if APacote.FontesAusentes.Count > 0 then
  begin
    AResultado.Motivo := Format(cmBuildFonteAusente,
      [StringReplace(APacote.FontesAusentes.CommaText, ',', ', ', [rfReplaceAll])]);
    Exit(False);
  end;

  if not GarantirRes(APacote) then
  begin
    AResultado.Motivo := cmBuildSemRes;
    Exit(False);
  end;

  vParams := TStringList.Create;
  vLU := TStringList.Create;
  vBusca := TStringList.Create;
  vObjetos := TStringList.Create;
  vNamespaces := TStringList.Create;
  vExec := TExecucao.Create;
  try
    MontarLU(APacote, vLU);
    MontarBusca(APacote, vBusca);
    MontarObjetos(APacote, vObjetos);
    MontarNamespaces(APacote, vNamespaces);

    // --no-config: nada da maquina entra sem estar escrito aqui
    vParams.Add('--no-config');
    vParams.Add('-B');
    vParams.Add('-Q');
    vParams.Add('-$D-');
    vParams.Add('-$L-');
    vParams.Add('-$Y-');
    vParams.Add('-NS' + Juntar(vNamespaces, ';'));
    vParams.Add('-U' + Juntar(vBusca, ';'));
    vParams.Add('-I' + Juntar(vBusca, ';'));
    vParams.Add('-R' + Juntar(vBusca, ';'));
    if vObjetos.Count > 0 then
      vParams.Add('-O' + Juntar(vObjetos, ';'));
    if vLU.Count > 0 then
      vParams.Add('-LU' + Juntar(vLU, ';'));
    vParams.Add('-LE' + ExcludeTrailingPathDelimiter(FPastaBpl));
    vParams.Add('-LN' + ExcludeTrailingPathDelimiter(FPastaDcp));
    vParams.Add('-N0' + ExcludeTrailingPathDelimiter(FPastaDcu));
    vParams.Add(ExtractFileName(vArquivoDpk));

    vExec.Executavel := Compilador;
    vExec.Parametros.Assign(vParams);
    vExec.PastaTrabalho := ExtractFilePath(vArquivoDpk);
    vExec.Log := @LogarProcesso;

    if FSimular then
    begin
      Logar(Format(cmSimulacaoComando, [vExec.LinhaComando]));
      AResultado.Ok := True;
      AResultado.Pulado := True;
      AResultado.Motivo := cmSimulacao;
      Exit(True);
    end;

    ForceDirectories(FPastaBpl);
    ForceDirectories(FPastaDcp);
    ForceDirectories(FPastaDcu);

    vInicio := GetTickCount64;
    Result := vExec.Executar;
    AResultado.Segundos := (GetTickCount64 - vInicio) / 1000;

    // {$LIBSUFFIX AUTO} (o Zeos) poe o sufixo da IDE no nome do .bpl
    // (ZCore290.bpl); o .dcp continua sem ele
    vSufixo := APacote.LibSuffix;
    if SameText(vSufixo, 'AUTO') then
      vSufixo := FIDE.SufixoPacote;
    AResultado.Bpl := IncludeTrailingPathDelimiter(FPastaBpl) +
                      ChangeFileExt(ExtractFileName(vArquivoDpk), '') + vSufixo +
                      '.bpl';
    AResultado.Dcp := IncludeTrailingPathDelimiter(FPastaDcp) +
                      ChangeFileExt(ExtractFileName(vArquivoDpk), '.dcp');

    if not Result then
    begin
      AResultado.Motivo := Format(cmBuildCodigo, [vExec.CodigoSaida]);
      if vExec.Erro <> '' then
        AResultado.Motivo := vExec.Erro
      else
      begin
        // unidade que falta quase nunca e defeito do RAL: e a biblioteca de
        // terceiro que o recurso exige e que ninguem instalou ainda
        vFaltando := UnidadeFaltando(vExec.Saida);
        if vFaltando <> '' then
        begin
          AResultado.Motivo := Format(cmBuildUnidadeAusente, [vFaltando]);
          Logar('  ' + AResultado.Motivo + wmBuildUnidadeAusente);
        end
        else
        begin
          // a unidade existe e nao compila: quase sempre versao da biblioteca
          // de terceiro diferente da que o RAL espera
          vFaltando := PrimeiroErro(vExec.Saida);
          if vFaltando <> '' then
            AResultado.Motivo := Format(cmBuildErroCompilacao, [vFaltando]);
        end;
      end;
      Exit(False);
    end;

    // o compilador pode terminar em 0 e nao ter gravado nada onde esperamos
    if not FileExists(AResultado.Bpl) then
    begin
      AResultado.Motivo := Format(cmBuildSemBpl, [FPastaBpl]);
      Exit(False);
    end;

    AResultado.TamanhoBpl := TamanhoArquivo(AResultado.Bpl);
    AResultado.Ok := True;

    // .bpl gordo demais para o tamanho do pacote quase sempre significa
    // terceiro linkado estaticamente — a IDE recusa isso com erro de unidade
    // duplicada, e e melhor saber agora
    // ... mas so quando o terceiro *deveria* ter vindo como pacote. O mORMot2
    // e usado por library path, sem pacote nenhum: ali um .bpl grande e o
    // esperado, e avisar seria mentira.
    if (APacote.Unidades.Count <= 6) and (AResultado.TamanhoBpl > 500 * 1024) and
       EsperaPacoteDeTerceiro(APacote) then
      Logar(Format(wmBplGrande,
                   [ExtractFileName(AResultado.Bpl), AResultado.TamanhoBpl div 1024,
                    APacote.Unidades.Count]));
  finally
    vExec.Free;
    vNamespaces.Free;
    vObjetos.Free;
    vBusca.Free;
    vLU.Free;
    vParams.Free;
  end;
end;

function TBuildDelphi.Compilar(APacotes: TList): boolean;
var
  vInt, vDep: integer;
  vPacote: TPacote;
  vResultado: TResultadoPacote;
  vFalhados, vAusentes: TStringList;
  vCulpado: string;
begin
  Result := True;
  SetLength(FResultados, 0);

  if FRaizFontes = '' then
  begin
    Logar(emBuildSemFontes);
    Exit(False);
  end;
  if not (ciCompilar in FIDE.Capacidades) then
  begin
    Logar(Format(emBuildSemCompilador, [FIDE.Nome]));
    Exit(False);
  end;
  if not FileExists(Compilador) then
  begin
    Logar(Format(emBuildCompiladorAusente, [Compilador]));
    Exit(False);
  end;
  if FIDE.Plataformas.IndexOf(LowerCase(FPlataforma)) < 0 then
  begin
    Logar(Format(emBuildPlataforma, [FIDE.Nome, FPlataforma]));
    Exit(False);
  end;

  // saida padrao: onde a IDE procura o que o usuario instalou
  if (FPastaBpl = '') and (FIDE.CommonDir <> '') then
    FPastaBpl := PastaBplPadrao;
  if (FPastaDcp = '') and (FIDE.CommonDir <> '') then
    FPastaDcp := PastaDcpPadrao;
  if (FPastaBpl = '') or (FPastaDcp = '') then
  begin
    Logar(emBuildSemCommonDir);
    Exit(False);
  end;

  if FPastaDcu = '' then
    FPastaDcu := FRaizFontes + 'compiled' + PathDelim + 'delphi' + PathDelim +
                 FIDE.BDSVersao + PathDelim + LowerCase(FPlataforma);

  Logar(Format(cmCompilando,
               [APacotes.Count, FIDE.Nome, FPlataforma]));
  Logar('  .bpl -> ' + FPastaBpl);
  Logar('  .dcp -> ' + FPastaDcp);

  vFalhados := TStringList.Create;
  vAusentes := TStringList.Create;
  try
    vFalhados.CaseSensitive := False;
    vAusentes.CaseSensitive := False;
    for vInt := 0 to Pred(APacotes.Count) do
    begin
      vPacote := TPacote(APacotes[vInt]);
      if vPacote.Tipo <> tpDelphi then
        Continue;

      Logar(Format('[%d/%d] %s', [vInt + 1, APacotes.Count, vPacote.Nome]));

      // pacote que falhou derruba quem depende dele, e só quem depende dele:
      // compilar o resto gera erro em cascata e esconde a causa, mas parar a
      // rodada inteira perde os pacotes que não têm nada com isso
      vCulpado := '';
      for vDep := 0 to Pred(vPacote.Internos.Count) do
        if vFalhados.IndexOf(vPacote.Internos[vDep]) >= 0 then
        begin
          vCulpado := vPacote.Internos[vDep];
          Break;
        end;

      if vCulpado <> '' then
      begin
        vResultado := Default(TResultadoPacote);
        vResultado.Nome := vPacote.Nome;
        vResultado.Pulado := True;
        // o que so faltou nesta plataforma nao e falha de ninguem
        if vAusentes.IndexOf(vCulpado) >= 0 then
        begin
          vResultado.Motivo := Format(cmBuildDependeAusente, [vCulpado, FPlataforma]);
          vAusentes.Add(vPacote.Nome);
        end
        else
          vResultado.Motivo := Format(cmBuildDependeFalhou, [vCulpado]);
        Logar(Format(cmBuildPulado, [vResultado.Motivo]));
        vFalhados.Add(vPacote.Nome);
      end
      else if not CompilarPacote(vPacote, vResultado) then
      begin
        Logar(Format(cmBuildFalhou, [vResultado.Motivo]));
        vFalhados.Add(vPacote.Nome);
        Result := False;
      end
      else if vResultado.Pulado then
      begin
        Logar(Format(cmBuildPulado, [vResultado.Motivo]));
        // pulado sem .dcp (plataforma que o .dproj desliga): quem depende
        // dele nesta plataforma tambem nao tem contra o que compilar
        if not vResultado.Ok then
        begin
          vFalhados.Add(vPacote.Nome);
          vAusentes.Add(vPacote.Nome);
        end;
      end
      else
        Logar(Format('  ok: %s (%d KB, %.1f s)',
                     [ExtractFileName(vResultado.Bpl),
                      vResultado.TamanhoBpl div 1024, vResultado.Segundos]));

      SetLength(FResultados, Length(FResultados) + 1);
      FResultados[High(FResultados)] := vResultado;
    end;
  finally
    vAusentes.Free;
    vFalhados.Free;
  end;
end;

function TBuildDelphi.Relatorio: string;
var
  vInt: integer;
  vLista: TStringList;
  vEstado: string;
begin
  vLista := TStringList.Create;
  try
    vLista.Add(Format('%-24s %-10s %-10s %s',
      [cmColunaPacote, cmColunaResultado, cmColunaTamanho, cmColunaObservacao]));
    for vInt := 0 to Pred(Total) do
    begin
      // pulado (plataforma que o .dproj nao habilita, dependencia que falhou)
      // nao e falha deste pacote: o motivo diz de quem e
      if FResultados[vInt].Pulado then
        vEstado := cmEstadoPulado
      else if not FResultados[vInt].Ok then
        vEstado := cmEstadoFalhou
      else
        vEstado := cmEstadoOk;
      vLista.Add(Format('%-24s %-10s %7d KB %s',
        [FResultados[vInt].Nome, vEstado, FResultados[vInt].TamanhoBpl div 1024,
         FResultados[vInt].Motivo]));
    end;
    Result := vLista.Text;
  finally
    vLista.Free;
  end;
end;

end.
