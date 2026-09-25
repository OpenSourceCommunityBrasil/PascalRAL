/// Complete installation in a Delphi IDE: compiles, writes the registry and
/// saves the receipt. The same run for the GUI, the CLI and the tests. Order:
/// check everything that can be checked before touching anything (IDE closed,
/// key exists, known packages), compile, and only then register, and register
/// only what compiled.
unit RALInst.Instalar.Delphi;

{$mode ObjFPC}{$H+}

{$IFNDEF MSWINDOWS}
  {$FATAL RALInst.Instalar.Delphi so compila no Windows}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALInst.Build.Delphi, RALInst.Catalogo, RALInst.Compatibilidade, RALInst.Existente,
  RALInst.IDE,
  RALInst.Processo, RALInst.Receitas, RALInst.Registro.Delphi;

type
  /// Installs the chosen RAL packages (and their dependencies) in one Delphi.
  TInstalacaoDelphi = class
  private
    /// What the receipt keeps besides the registry: written files
    /// (file=size|date)
    FArquivosRecibo: TStringList;
    FAvisos: TStringList;
    FCaminhosExtras: TStringList;
    FCatalogo: TCatalogo;
    FChaveRegistro: string;
    FCompat: TCompatibilidade;
    /// Dependencies for the receipt (name=instalada|... or name=encontrada|...)
    FDepsRecibo: TStringList;
    FExigirIDEFechada: boolean;
    FIDE: TIDEInstance;
    /// The 64-bit IDE gets the design packages in this run
    FIDE64: boolean;
    FIgnorarExistentes: boolean;
    FLog: TLogLinha;
    FManifesto: TManifesto;
    FNomeVariavel: string;
    FPacotes: TStringList;
    /// Dependency .dcp files for -LU (the recipes' pacotes-ligados)
    FPacotesLigados: TStringList;
    FPastaBpl: string;
    FPastaDcp: string;
    FPastaRecibos: string;
    FPastasDependencias: TStringList;
    FPlataformas: TStringList;
    FRaizFontes: string;
    FReceitas: TReceitas;
    FRecibo: string;
    FRegistro: TRegistroDelphi;
    FRelatorio: TStringList;
    FSimular: boolean;
    FSomenteLibraryPath: boolean;
    FUsarIDE64: boolean;
    /// Variables the recipes defined in this run: they expand the paths even
    /// when simulating (when the registry was not written)
    FVariaveisDeps: TStringList;
    /// Notes the .bpl/.dcp of a result for the receipt
    procedure AnotarArquivos(const AResultado: TResultadoPacote);
    /// Runs the recipe actions for this IDE
    function AplicarDependencia(AReceita: TReceita; const ARaiz: string): boolean;
    /// Library path folders of a recipe that exist
    procedure CaminhosDaReceita(AReceita: TReceita; const ARaiz: string;
      ALista: TStrings);
    /// $(PascalRAL)\base, as the wiki teaches to do by hand
    function CaminhoParaRegistro(const ARelativo: string): string;
    /// The compatibility check, created on first use
    function Compat: TCompatibilidade;
    /// Compiles the .dpk files of a recipe action
    function CompilarDpk(AReceita: TReceita; AAcao: TAcaoReceita;
      const ARaiz: string): boolean;
    /// Everything that can be checked before touching anything
    function Conferir(APacotes: TList): boolean;
    /// TDetectarDependencia for the compatibility check
    function DetectarNaIDE(AIDE: TIDEInstance; AReceita: TReceita): string;
    /// {raiz}, this run's variables and the IDE's
    function ExpandirDep(const ACaminho, ARaiz: string): string;
    /// Removes from the list what does not fit this IDE; AFora gets name=reason
    procedure FiltrarCompativeis(ALista: TList; AFora: TStrings);
    /// Creates FRegistro when needed
    procedure GarantirRegistro;
    /// Passes the line to Log when assigned
    procedure Logar(const ALinha: string);
    /// Library path entries and one unit per folder (to spot another RAL copy)
    procedure MontarCaminhos(APacotes: TList; ACaminhos, AUnidades: TStrings);
    /// Names of the Delphi packages of the catalog
    function NomesDoCatalogo: TStringList;
    /// Names of the packages of a list
    function NomesDaLista(ALista: TList): TStringList;
    /// Output folder the IDE asks (Tools > Options > Library), else APadrao
    function PastaSaida(const APlataforma, AValor, APadrao: string): string;
    /// Win32 library path folders holding the file; expanded
    procedure PastasNoLibraryPath(const AArquivo: string; APastas: TStrings);
    /// Installs or finds each required dependency; leaves out what lacks one
    procedure PrepararDependencias(ALista: TList; AResultados: TStrings);
    /// Decides whether the 64-bit IDE is served (and Win64 enters the run)
    procedure PrepararIDE64;
    /// The .dcp files matching a recipe's pacotes-ligados
    procedure ResolverLigados(AReceita: TReceita);
    /// Writes the undo record of an uninstall (the registry values before)
    function SalvarDesinstalacao: string;
    /// Writes the receipt of the run; returns its file
    function SalvarRecibo(APacotes: TList; AResultados: TStrings): string;
    procedure SetRaizFontes(const AValor: string);
  public
    constructor Create(AIDE: TIDEInstance; ACatalogo: TCatalogo);
    destructor Destroy; override;
    /// Where the dependency already is in this IDE ('' if it is not): IDE
    /// variable, unit in the library path or .dcp
    function DependenciaInstalada(AReceita: TReceita): string;
    /// Removes RAL from the IDE: undoes the installer's receipts, then takes out
    /// what is left of a hand installation (Known Packages of both IDEs,
    /// library path, $(PascalRAL)). The .bpl files of a hand installation stay
    /// on disk; an undo record of the registry goes to <receipts>\desinstalacoes
    function Desinstalar: boolean;
    /// Returns False when something failed; the report says what entered and
    /// what did not
    function Executar: boolean;
    /// What RAL the IDE already has, by hand or by the installer (the caller
    /// frees it)
    function Existente: TInstalacaoExistente;
    /// What the run will do, without doing anything
    function Plano: string;
    /// What Desinstalar will do, without doing anything
    function PlanoDesinstalar: string;
    /// The dependency version this IDE asks (manifest, recipe range or the
    /// default); '' and AMotivo when none works
    function VersaoDependencia(AReceita: TReceita; out AMotivo: string): string;

    property Avisos: TStringList read FAvisos;
    property CaminhosExtras: TStringList read FCaminhosExtras;
    /// Relative to HKCU; empty = the IDE's. The tests point it to a copy
    property ChaveRegistro: string read FChaveRegistro write FChaveRegistro;
    /// Refuses to write with the IDE open (it rewrites everything on close)
    property ExigirIDEFechada: boolean read FExigirIDEFechada write FExigirIDEFechada;
    property IDE: TIDEInstance read FIDE;
    /// Installs the downloaded dependency even if the IDE already has one
    property IgnorarExistentes: boolean read FIgnorarExistentes write FIgnorarExistentes;
    property Log: TLogLinha read FLog write FLog;
    /// The RAL version manifest (not owned; nil = only what the disk tells)
    property Manifesto: TManifesto read FManifesto write FManifesto;
    /// The IDE variable pointing to <root>\src ('PascalRAL', as in the wiki)
    property NomeVariavel: string read FNomeVariavel write FNomeVariavel;
    /// Names the user asked; internal dependencies enter by themselves
    property Pacotes: TStringList read FPacotes;
    /// Empty = what the IDE uses (Package DPL/DCP Output, or
    /// <BDSCOMMONDIR>\Bpl)
    property PastaBpl: string read FPastaBpl write FPastaBpl;
    property PastaDcp: string read FPastaDcp write FPastaDcp;
    property PastaRecibos: string read FPastaRecibos write FPastaRecibos;
    /// Where each dependency was downloaded (name@version=folder)
    property PastasDependencias: TStringList read FPastasDependencias;
    /// 'win32' always; 'win64' compiles the runtime and sets the Win64 path
    property Plataformas: TStringList read FPlataformas;
    /// Where the sources are (or will be, after download); by default the
    /// catalog root, when it comes from disk
    property RaizFontes: string read FRaizFontes write SetRaizFontes;
    /// The known recipes (not owned)
    property Receitas: TReceitas read FReceitas write FReceitas;
    /// Receipt file of this run ('' when none was written)
    property Recibo: string read FRecibo;
    property Relatorio: TStringList read FRelatorio;
    property Simular: boolean read FSimular write FSimular;
    /// Only points the sources in the library path, compiling nothing
    property SomenteLibraryPath: boolean read FSomenteLibraryPath
      write FSomenteLibraryPath;
    /// Also serves the 64-bit IDE when there is one (default)
    property UsarIDE64: boolean read FUsarIDE64 write FUsarIDE64;
  end;

implementation

uses
  StrUtils, fpjson,
  RALInst.Fontes, RALInst.Mensagens, RALInst.Recibos;

// 'a, b, c' (os nomes de uma lista nome=valor quando ANomes)
function Juntar(ALista: TStrings; ANomes: boolean): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 0 to Pred(ALista.Count) do
  begin
    if Result <> '' then
      Result := Result + ', ';
    if ANomes then
      Result := Result + ALista.Names[vInt]
    else
      Result := Result + ALista[vInt];
  end;
end;

function TemPas(const APasta: string): boolean;
var
  vBusca: TSearchRec;
begin
  Result := FindFirst(IncludeTrailingPathDelimiter(APasta) + '*.pas', faAnyFile,
                      vBusca) = 0;
  if Result then
    SysUtils.FindClose(vBusca);
end;

{ TInstalacaoDelphi }

procedure TInstalacaoDelphi.SetRaizFontes(const AValor: string);
begin
  FRaizFontes := '';
  if AValor <> '' then
    FRaizFontes := IncludeTrailingPathDelimiter(AValor);
end;

constructor TInstalacaoDelphi.Create(AIDE: TIDEInstance; ACatalogo: TCatalogo);
begin
  inherited Create;
  FIDE := AIDE;
  FCatalogo := ACatalogo;
  if (FCatalogo <> nil) and (FCatalogo.Origem is TOrigemLocal) then
    FRaizFontes := IncludeTrailingPathDelimiter(TOrigemLocal(FCatalogo.Origem).Raiz);
  FPacotes := TStringList.Create;
  FPacotes.CaseSensitive := False;
  FPlataformas := TStringList.Create;
  FPlataformas.Add('win32');
  FUsarIDE64 := True;
  FCaminhosExtras := TStringList.Create;
  FRelatorio := TStringList.Create;
  FAvisos := TStringList.Create;
  FExigirIDEFechada := True;
  FNomeVariavel := 'PascalRAL';
  FPastaRecibos := PastaDadosInstalador + 'recibos';
  FPastasDependencias := TStringList.Create;
  FPastasDependencias.CaseSensitive := False;
  FVariaveisDeps := TStringList.Create;
  FVariaveisDeps.CaseSensitive := False;
  FPacotesLigados := TStringList.Create;
  FPacotesLigados.CaseSensitive := False;
  FArquivosRecibo := TStringList.Create;
  FDepsRecibo := TStringList.Create;
end;

function TInstalacaoDelphi.Compat: TCompatibilidade;
begin
  if FCompat = nil then
  begin
    FCompat := TCompatibilidade.Create(FCatalogo, FManifesto, FReceitas);
    FCompat.Detectar := @DetectarNaIDE;
  end;
  Result := FCompat;
end;

function TInstalacaoDelphi.DetectarNaIDE(AIDE: TIDEInstance;
  AReceita: TReceita): string;
begin
  Result := DependenciaInstalada(AReceita);
end;

function TInstalacaoDelphi.VersaoDependencia(AReceita: TReceita;
  out AMotivo: string): string;
begin
  Result := Compat.VersaoDependencia(AReceita, FIDE, AMotivo);
end;

procedure TInstalacaoDelphi.FiltrarCompativeis(ALista: TList; AFora: TStrings);
begin
  AFora.Clear;
  Compat.Filtrar(ALista, FIDE, AFora);
end;

procedure TInstalacaoDelphi.PastasNoLibraryPath(const AArquivo: string;
  APastas: TStrings);
var
  vItens: TStringList;
  vPasta, vValor: string;
begin
  GarantirRegistro;
  vItens := TStringList.Create;
  try
    vItens.StrictDelimiter := True;
    vItens.Delimiter := ';';
    vItens.DelimitedText := FRegistro.LerValor(FRegistro.ChaveLibrary('win32'),
                                               'Search Path');
    for vPasta in vItens do
    begin
      vValor := ExcludeTrailingPathDelimiter(FRegistro.Expandir(Trim(vPasta)));
      if (vValor <> '') and (Pos('$(', vValor) = 0) and
         FileExists(IncludeTrailingPathDelimiter(vValor) + AArquivo) and
         (APastas.IndexOf(vValor) < 0) then
        APastas.Add(vValor);
    end;
  finally
    vItens.Free;
  end;
end;

procedure TInstalacaoDelphi.AnotarArquivos(const AResultado: TResultadoPacote);
var
  vArquivo: string;
  vBusca: TSearchRec;
  vVez: integer;
begin
  if not AResultado.Ok or AResultado.Pulado then
    Exit;
  for vVez := 1 to 2 do
  begin
    if vVez = 1 then
      vArquivo := AResultado.Bpl
    else
      vArquivo := AResultado.Dcp;
    if (vArquivo <> '') and (FindFirst(vArquivo, faAnyFile, vBusca) = 0) then
    begin
      // tamanho e data: desinstalar so apaga o que ainda e o que foi gerado
      FArquivosRecibo.Values[vArquivo] := IntToStr(vBusca.Size) + '|' +
        FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss',
                       vBusca.TimeStamp);
      SysUtils.FindClose(vBusca);
    end;
  end;
end;

procedure TInstalacaoDelphi.ResolverLigados(AReceita: TReceita);
var
  vPastas: TStringList;
  vPasta, vPadrao, vNome: string;
  vBusca: TSearchRec;
begin
  if AReceita.Delphi.PacotesLigados.Count = 0 then
    Exit;
  vPastas := TStringList.Create;
  try
    vPastas.Add(PastaSaida('win32', 'Package DCP Output', ''));
    vPastas.Add(FIDE.CommonDir + 'Dcp');
    vPastas.Add(FIDE.RootDir + 'lib' + PathDelim + 'win32' + PathDelim + 'release');
    for vPadrao in AReceita.Delphi.PacotesLigados do
      for vPasta in vPastas do
      begin
        if (vPasta = '') or not DirectoryExists(vPasta) then
          Continue;
        if FindFirst(IncludeTrailingPathDelimiter(vPasta) + vPadrao + '.dcp',
                     faAnyFile, vBusca) = 0 then
        try
          repeat
            vNome := ChangeFileExt(vBusca.Name, '');
            if FPacotesLigados.IndexOf(vNome) < 0 then
              FPacotesLigados.Add(vNome);
          until FindNext(vBusca) <> 0;
        finally
          SysUtils.FindClose(vBusca);
        end;
      end;
  finally
    vPastas.Free;
  end;
  if FPacotesLigados.Count > 0 then
    Logar(Format(cmCompilaContra, [FPacotesLigados.CommaText]));
end;

function TInstalacaoDelphi.NomesDaLista(ALista: TList): TStringList;
var
  vInt: integer;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  for vInt := 0 to Pred(ALista.Count) do
    Result.Add(TPacote(ALista[vInt]).Nome);
end;

procedure TInstalacaoDelphi.GarantirRegistro;
begin
  if FRegistro <> nil then
    Exit;
  FRegistro := TRegistroDelphi.Create(FIDE);
  FRegistro.Log := FLog;
  FRegistro.Simular := FSimular;
  if FChaveRegistro <> '' then
    FRegistro.Chave := FChaveRegistro;
end;

function TInstalacaoDelphi.DependenciaInstalada(AReceita: TReceita): string;
var
  vInt: integer;
  vTipo, vNome, vValor, vPasta: string;
  vPastas, vItens: TStringList;
  vBusca: TSearchRec;
begin
  Result := '';
  if not AReceita.Delphi.Existe then
    Exit;
  GarantirRegistro;
  vPastas := TStringList.Create;
  vItens := TStringList.Create;
  try
    for vInt := 0 to Pred(AReceita.Delphi.Deteccao.Count) do
    begin
      vTipo := AReceita.Delphi.Deteccao.Names[vInt];
      vNome := AReceita.Delphi.Deteccao.ValueFromIndex[vInt];

      if vTipo = 'variavel' then
      begin
        // a variavel da IDE existe e aponta para uma pasta que existe
        vValor := FRegistro.LerValor('Environment Variables', vNome);
        if (vValor <> '') and DirectoryExists(FRegistro.Expandir(vValor)) then
          Exit(Format('$(%s) = %s', [vNome, vValor]));
      end
      else if vTipo = 'unidade' then
      begin
        // a unidade esta numa pasta do library path (Win32)
        vItens.StrictDelimiter := True;
        vItens.Delimiter := ';';
        vItens.DelimitedText := FRegistro.LerValor(FRegistro.ChaveLibrary('win32'),
                                                   'Search Path');
        for vPasta in vItens do
        begin
          vValor := FRegistro.Expandir(Trim(vPasta));
          if (vValor <> '') and (Pos('$(', vValor) = 0) and
             FileExists(IncludeTrailingPathDelimiter(vValor) + vNome) then
            Exit(Format(cmNoLibraryPath, [vNome, Trim(vPasta)]));
        end;
      end
      else if vTipo = 'dcp' then
      begin
        // o .dcp onde a IDE procura pacotes de terceiros: com o sufixo da IDE,
        // sem ele, ou pelo curinga da receita (uniGUI*Core)
        vPastas.Clear;
        vPastas.Add(PastaSaida('win32', 'Package DCP Output', ''));
        vPastas.Add(FIDE.CommonDir + 'Dcp');
        vPastas.Add(FIDE.RootDir + 'lib' + PathDelim + 'win32' + PathDelim + 'release');
        for vPasta in vPastas do
        begin
          if (vPasta = '') or not DirectoryExists(vPasta) then
            Continue;
          if Pos('*', vNome) > 0 then
          begin
            if FindFirst(IncludeTrailingPathDelimiter(vPasta) + vNome + '.dcp',
                         faAnyFile, vBusca) = 0 then
            begin
              vValor := vBusca.Name;
              SysUtils.FindClose(vBusca);
              Exit(IncludeTrailingPathDelimiter(vPasta) + vValor);
            end;
          end
          else if FileExists(IncludeTrailingPathDelimiter(vPasta) + vNome + '.dcp') then
            Exit(IncludeTrailingPathDelimiter(vPasta) + vNome + '.dcp')
          else if FileExists(IncludeTrailingPathDelimiter(vPasta) + vNome +
                             FIDE.SufixoPacote + '.dcp') then
            Exit(IncludeTrailingPathDelimiter(vPasta) + vNome + FIDE.SufixoPacote +
                 '.dcp');
        end;
      end;
    end;
  finally
    vItens.Free;
    vPastas.Free;
  end;
end;

function TInstalacaoDelphi.ExpandirDep(const ACaminho, ARaiz: string): string;
var
  vInt: integer;
begin
  Result := ExpandirRaiz(ACaminho, ARaiz);
  // primeiro o que esta rodada definiu (vale simulando), depois a IDE
  for vInt := 0 to Pred(FVariaveisDeps.Count) do
    Result := StringReplace(Result, '$(' + FVariaveisDeps.Names[vInt] + ')',
                            ExcludeTrailingPathDelimiter(
                              FVariaveisDeps.ValueFromIndex[vInt]),
                            [rfReplaceAll, rfIgnoreCase]);
  Result := FRegistro.Expandir(Result);
end;

procedure TInstalacaoDelphi.CaminhosDaReceita(AReceita: TReceita;
  const ARaiz: string; ALista: TStrings);
var
  vInt: integer;
  vAcao: TAcaoReceita;
  vCaminho, vPasta: string;
begin
  for vInt := 0 to Pred(AReceita.Delphi.Acoes.Count) do
  begin
    vAcao := AReceita.Delphi.Acao(vInt);
    if vAcao.Tipo <> taLibPath then
      Continue;
    for vCaminho in vAcao.Caminhos do
    begin
      vPasta := ExcludeTrailingPathDelimiter(ExpandirDep(vCaminho, ARaiz));
      if (Pos('$(', vPasta) = 0) and DirectoryExists(vPasta) and
         (ALista.IndexOf(vPasta) < 0) then
        ALista.Add(vPasta);
    end;
  end;
end;

function TInstalacaoDelphi.CompilarDpk(AReceita: TReceita; AAcao: TAcaoReceita;
  const ARaiz: string): boolean;
var
  vPasta, vPlat: string;
  vCatalogo: TCatalogo;
  vLista: TList;
  vBuild: TBuildDelphi;
  vInt: integer;
  vResultado: TResultadoPacote;
  vBpls, vBpls64: TStringList;
  vPacote: TPacote;
begin
  Result := False;
  vPasta := AAcao.Pastas.Values[FIDE.BDSVersao];
  if vPasta = '' then
  begin
    Logar(Format(emDepSemPacotesBDS,
                 [AReceita.Nome, FIDE.Nome, FIDE.BDSVersao]));
    Exit;
  end;

  vCatalogo := TCatalogo.Create;
  vLista := TList.Create;
  vBpls := TStringList.Create;
  vBpls64 := TStringList.Create;
  try
    vCatalogo.PastaDelphi := vPasta;
    if not vCatalogo.Carregar(ARaiz) then
    begin
      Logar(Format(emDepSemDpk, [AReceita.Nome, ARaiz + vPasta]));
      Exit;
    end;
    vCatalogo.Listar(tpDelphi, vLista);
    Result := True;

    for vPlat in FPlataformas do
    begin
      if FIDE.Plataformas.IndexOf(LowerCase(vPlat)) < 0 then
        Continue;
      vBuild := TBuildDelphi.Create(FIDE, vCatalogo);
      try
        vBuild.Plataforma := vPlat;
        vBuild.Log := FLog;
        vBuild.Simular := FSimular;
        // a IDE de 64 bits carrega os de design de Win64, como os do RAL
        vBuild.Design64 := FIDE64 and SameText(vPlat, 'win64');
        if FPastaBpl <> '' then
          vBuild.PastaBpl := FPastaBpl + IfThen(SameText(vPlat, 'win32'), '', '\' + vPlat)
        else
          vBuild.PastaBpl := PastaSaida(vPlat, 'Package DPL Output', '');
        if FPastaDcp <> '' then
          vBuild.PastaDcp := FPastaDcp + IfThen(SameText(vPlat, 'win32'), '', '\' + vPlat)
        else
          vBuild.PastaDcp := PastaSaida(vPlat, 'Package DCP Output', '');
        if not vBuild.Compilar(vLista) then
          Result := False;
        FRelatorio.Add('== ' + AReceita.Nome + ' ' + PlataformaRegistro(vPlat) + ' ==');
        FRelatorio.Add(vBuild.Relatorio);
        for vInt := 0 to Pred(vBuild.Total) do
        begin
          vResultado := vBuild.Resultados[vInt];
          AnotarArquivos(vResultado);
          if vResultado.Ok and SameText(vPlat, 'win32') then
            vBpls.Values[vResultado.Nome] := vResultado.Bpl;
          if FIDE64 and vResultado.Ok and not vResultado.Pulado and
             SameText(vPlat, 'win64') then
            vBpls64.Values[vResultado.Nome] := vResultado.Bpl;
        end;
      finally
        vBuild.Free;
      end;
    end;

    // os de design vao para a IDE, como os do RAL
    if AAcao.Instalar then
      for vInt := 0 to Pred(vLista.Count) do
      begin
        vPacote := TPacote(vLista[vInt]);
        if vPacote.Instalavel and (vBpls.Values[vPacote.Nome] <> '') then
          if not FRegistro.RegistrarPacote(vBpls.Values[vPacote.Nome],
               IfThen(vPacote.Descricao <> '', vPacote.Descricao, vPacote.Nome)) then
            Result := False;
        if vPacote.Instalavel and (vBpls64.Values[vPacote.Nome] <> '') then
          if not FRegistro.RegistrarPacote(vBpls64.Values[vPacote.Nome],
               IfThen(vPacote.Descricao <> '', vPacote.Descricao, vPacote.Nome),
               True) then
            Result := False;
      end;
  finally
    vBpls64.Free;
    vBpls.Free;
    vLista.Free;
    vCatalogo.Free;
  end;
end;

function TInstalacaoDelphi.AplicarDependencia(AReceita: TReceita;
  const ARaiz: string): boolean;
var
  vInt, vAdicionados: integer;
  vAcao: TAcaoReceita;
  vPlat, vValor: string;
  vCaminhos: TStringList;
begin
  Result := True;
  vCaminhos := TStringList.Create;
  try
    for vInt := 0 to Pred(AReceita.Delphi.Acoes.Count) do
    begin
      vAcao := AReceita.Delphi.Acao(vInt);
      case vAcao.Tipo of
        taVariavel:
          begin
            vValor := ExcludeTrailingPathDelimiter(ExpandirRaiz(vAcao.Valor, ARaiz));
            FVariaveisDeps.Values[vAcao.Nome] := vValor;
            if not FRegistro.AceitaVariaveis then
              Continue;
            if not FRegistro.DefinirVariavel(vAcao.Nome, vValor) then
              Result := False;
          end;
        taLibPath:
          begin
            vCaminhos.Clear;
            for vValor in vAcao.Caminhos do
              if FRegistro.AceitaVariaveis then
                vCaminhos.Add(ExpandirRaiz(vValor, ARaiz))
              else
                vCaminhos.Add(ExpandirDep(vValor, ARaiz));
            for vPlat in FPlataformas do
            begin
              vAdicionados := FRegistro.AdicionarCaminhos(vPlat, 'Search Path',
                                                          vCaminhos);
              if vAdicionados < 0 then
                Result := False;
            end;
          end;
        taDpk:
          if not FSomenteLibraryPath then
            if not CompilarDpk(AReceita, vAcao, ARaiz) then
              Result := False;
      end;
    end;
  finally
    vCaminhos.Free;
  end;
end;

procedure TInstalacaoDelphi.PrepararDependencias(ALista: TList; AResultados: TStrings);
var
  vExigidas, vFaltando, vFora, vNomes: TStringList;
  vInt, vDep, vRec, vAntes: integer;
  vArquivo: string;
  vReceita: TReceita;
  vOnde, vRaiz, vMotivo, vVersao, vMotivoVersao: string;
  vPacote: TPacote;
begin
  if FReceitas = nil then
    Exit;
  vExigidas := TStringList.Create;
  // nome da receita que faltou = por que
  vFaltando := TStringList.Create;
  vFaltando.CaseSensitive := False;
  // pacotes do RAL que ficam de fora
  vFora := TStringList.Create;
  vFora.CaseSensitive := False;
  // so o que sobrou da conferencia de compatibilidade (F6)
  vNomes := NomesDaLista(ALista);
  try
    FReceitas.Exigidas(FCatalogo, tpDelphi, vNomes, vExigidas);
    for vInt := 0 to Pred(vExigidas.Count) do
    begin
      vReceita := TReceita(vExigidas.Objects[vInt]);
      Logar(Format(cmDependenciaPara, [vReceita.Nome, vExigidas[vInt]]));
      // F6: a versao que esta IDE pede; a pasta e a daquela versao
      vVersao := VersaoDependencia(vReceita, vMotivoVersao);
      vRaiz := '';
      if vMotivoVersao = '' then
        vRaiz := FPastasDependencias.Values[ChaveDependencia(vReceita.Nome, vVersao)];

      vOnde := '';
      if not (FIgnorarExistentes and (vRaiz <> '')) then
        vOnde := DependenciaInstalada(vReceita);
      if vOnde <> '' then
      begin
        Logar(Format(cmDepJaInstalada, [vOnde]));
        CaminhosDaReceita(vReceita, '', FCaminhosExtras);
        ResolverLigados(vReceita);
        // o que o RAL inclui dela (ZComponent.inc) vem de onde a IDE a acha
        for vArquivo in vReceita.Delphi.Busca do
        begin
          vAntes := FCaminhosExtras.Count;
          PastasNoLibraryPath(vArquivo, FCaminhosExtras);
          if FCaminhosExtras.Count = vAntes then
            Logar(Format(wmBuscaForaDoPath, [vArquivo]))
          else
            Logar(Format(cmArquivoEm,
                         [vArquivo, FCaminhosExtras[Pred(FCaminhosExtras.Count)]]));
        end;
        // encontrada: desinstalar nao a leva junto
        FDepsRecibo.Values[vReceita.Nome] := 'encontrada|' + vOnde;
        Continue;
      end;

      if vReceita.Pago then
        vMotivo := Format(cmDepComercial, [vReceita.Nome, vReceita.Site])
      else if vMotivoVersao <> '' then
        vMotivo := vMotivoVersao
      else if vRaiz = '' then
        vMotivo := Format(cmDepNaoBaixada, [vReceita.Nome, vVersao])
      else if vReceita.Delphi.Acoes.Count = 0 then
        vMotivo := Format(cmDepSemAcoes, [vReceita.Nome, 'Delphi'])
      else
      begin
        Logar(Format(cmDepInstalandoDe, [vRaiz]));
        if AplicarDependencia(vReceita, vRaiz) then
        begin
          CaminhosDaReceita(vReceita, vRaiz, FCaminhosExtras);
          ResolverLigados(vReceita);
          FDepsRecibo.Values[vReceita.Nome] := 'instalada|' + vRaiz + ' (' + vVersao +
                                               ')';
          Continue;
        end;
        vMotivo := Format(cmDepFalhou, [vReceita.Nome]);
      end;
      Logar('  ' + vMotivo);
      vFaltando.Values[vReceita.Nome] := vMotivo;
    end;

    if vFaltando.Count = 0 then
      Exit;

    // quem precisava do que faltou fica de fora, e quem depende dele tambem;
    // a lista esta em ordem de dependencia, entao uma passada basta
    for vInt := 0 to Pred(ALista.Count) do
    begin
      vPacote := TPacote(ALista[vInt]);
      vMotivo := '';
      for vRec := 0 to Pred(vFaltando.Count) do
        if FReceitas.Buscar(vFaltando.Names[vRec]).Atende(vPacote) then
          vMotivo := vFaltando.ValueFromIndex[vRec];
      if vMotivo = '' then
        for vDep := 0 to Pred(vPacote.Internos.Count) do
          if vFora.IndexOf(vPacote.Internos[vDep]) >= 0 then
            vMotivo := Format(cmDependeDeFora, [vPacote.Internos[vDep]]);
      if vMotivo <> '' then
      begin
        vFora.Add(vPacote.Nome);
        FAvisos.Add(Format(cmFicaDeFora, [vPacote.Nome, vMotivo]));
        AResultados.Add(vPacote.Nome + ': pulado — ' + vMotivo);
      end;
    end;
    for vInt := Pred(ALista.Count) downto 0 do
      if vFora.IndexOf(TPacote(ALista[vInt]).Nome) >= 0 then
        ALista.Delete(vInt);
  finally
    vNomes.Free;
    vFora.Free;
    vFaltando.Free;
    vExigidas.Free;
  end;
end;

destructor TInstalacaoDelphi.Destroy;
begin
  FCompat.Free;
  FDepsRecibo.Free;
  FArquivosRecibo.Free;
  FPacotesLigados.Free;
  FVariaveisDeps.Free;
  FPastasDependencias.Free;
  FRegistro.Free;
  FAvisos.Free;
  FRelatorio.Free;
  FCaminhosExtras.Free;
  FPlataformas.Free;
  FPacotes.Free;
  inherited Destroy;
end;

procedure TInstalacaoDelphi.Logar(const ALinha: string);
begin
  if Assigned(FLog) then
    FLog(ALinha);
end;

function TInstalacaoDelphi.PastaSaida(const APlataforma, AValor,
  APadrao: string): string;
begin
  // a IDE diz onde quer os pacotes (Tools > Options > Library); o padrao so
  // vale quando ela nao diz
  Result := Trim(FRegistro.LerValor(FRegistro.ChaveLibrary(APlataforma), AValor));
  if Result <> '' then
    Result := FRegistro.Expandir(Result, APlataforma);
  if (Result = '') or (Pos('$(', Result) > 0) then
    Result := APadrao;
end;

function TInstalacaoDelphi.CaminhoParaRegistro(const ARelativo: string): string;
var
  vRel: string;
begin
  // $(PascalRAL)\base, como a wiki ensina a fazer a mao: trocar a pasta do RAL
  // depois e mudar uma variavel, nao reescrever o library path
  vRel := StringReplace(ExcludeTrailingPathDelimiter(ARelativo), '/', '\',
                        [rfReplaceAll]);
  if FRegistro.AceitaVariaveis and SameText(Copy(vRel, 1, 4), 'src\') then
    Result := '$(' + FNomeVariavel + ')\' + Copy(vRel, 5, MaxInt)
  else if SameText(vRel, 'src') and FRegistro.AceitaVariaveis then
    Result := '$(' + FNomeVariavel + ')'
  else
    Result := FRaizFontes + vRel;
end;

procedure TInstalacaoDelphi.MontarCaminhos(APacotes: TList;
  ACaminhos, AUnidades: TStrings);
var
  vInt, vSub: integer;
  vPacote: TPacote;
  vUnidade, vRel, vCaminho, vSubmodulo: string;
  vVariante: string;
begin
  ACaminhos.Clear;
  AUnidades.Clear;
  begin
    for vInt := 0 to Pred(APacotes.Count) do
    begin
      vPacote := TPacote(APacotes[vInt]);
      for vUnidade in vPacote.Unidades do
      begin
        vRel := ExtractFilePath(StringReplace(vUnidade, '/', '\', [rfReplaceAll]));
        vCaminho := CaminhoParaRegistro(vRel);
        if ACaminhos.IndexOf(vCaminho) < 0 then
        begin
          ACaminhos.Add(vCaminho);
          // uma unidade por pasta basta para achar outra copia do RAL
          AUnidades.Add(ExtractFileName(StringReplace(vUnidade, '/', '\',
                                                      [rfReplaceAll])));
        end;
      end;
    end;

    // o que o .dproj poe no caminho de busca (src\others do SaguiRAL)
    for vInt := 0 to Pred(APacotes.Count) do
      for vSub := 0 to Pred(TPacote(APacotes[vInt]).CaminhosBusca.Count) do
      begin
        vCaminho := CaminhoParaRegistro(TPacote(APacotes[vInt]).CaminhosBusca[vSub]);
        if ACaminhos.IndexOf(vCaminho) < 0 then
          ACaminhos.Add(vCaminho);
      end;

    // submodulos que os pacotes usam (kxBSON, ZSTD, brotli): o catalogo ja
    // soma os que o .dpk nao lista mas o .lpk irmao lista. A pasta com .pas
    // (a raiz, Source ou src) e a que entra no path
    for vInt := 0 to Pred(APacotes.Count) do
      for vSub := 0 to Pred(TPacote(APacotes[vInt]).Submodulos.Count) do
      begin
        vSubmodulo := StringReplace(TPacote(APacotes[vInt]).Submodulos[vSub], '/', '\',
                                    [rfReplaceAll]);
        for vVariante in TStringArray.Create('', '\Source', '\src') do
          if DirectoryExists(FRaizFontes + vSubmodulo + vVariante) and
             TemPas(FRaizFontes + vSubmodulo + vVariante) then
          begin
            vCaminho := CaminhoParaRegistro(vSubmodulo + vVariante);
            if ACaminhos.IndexOf(vCaminho) < 0 then
              ACaminhos.Add(vCaminho);
          end;
      end;
  end;
end;

function TInstalacaoDelphi.Conferir(APacotes: TList): boolean;
var
  vDesconhecidos: TStringList;
begin
  Result := False;

  if FRaizFontes = '' then
  begin
    Logar(emInstalarSemFontes);
    Exit;
  end;

  vDesconhecidos := TStringList.Create;
  try
    FCatalogo.Fechamento(tpDelphi, FPacotes, APacotes, vDesconhecidos);
    if vDesconhecidos.Count > 0 then
    begin
      Logar(Format(emPacotesDesconhecidos, [vDesconhecidos.CommaText]));
      Exit;
    end;
  finally
    vDesconhecidos.Free;
  end;
  if APacotes.Count = 0 then
  begin
    Logar(emNenhumPacote);
    Exit;
  end;

  if not FRegistro.ChaveExiste then
  begin
    Logar(Format(emDelphiNuncaAberto, [FIDE.Nome, FRegistro.Chave]));
    Exit;
  end;

  if FExigirIDEFechada and FRegistro.IDEEmExecucao then
  begin
    Logar(Format(emDelphiAberto, [FIDE.Nome]));
    Exit;
  end;

  Result := True;
end;

function TInstalacaoDelphi.Plano: string;
var
  vLista: TList;
  vPlano, vCaminhos, vUnidades: TStringList;
  vInt: integer;
  vPlat, vAtual, vRaiz, vVersao, vMotivo: string;
  vExigidas, vFora, vNomes: TStringList;
  vReceita: TReceita;
begin
  vLista := TList.Create;
  vPlano := TStringList.Create;
  vCaminhos := TStringList.Create;
  vUnidades := TStringList.Create;
  vFora := TStringList.Create;
  vNomes := nil;
  FreeAndNil(FRegistro);
  FRegistro := TRegistroDelphi.Create(FIDE);
  PrepararIDE64;
  try
    if FChaveRegistro <> '' then
      FRegistro.Chave := FChaveRegistro;
    FCatalogo.Fechamento(tpDelphi, FPacotes, vLista);
    // F6: o que nao cabe nesta IDE sai antes de tudo, com o motivo
    FiltrarCompativeis(vLista, vFora);
    vNomes := NomesDaLista(vLista);

    vPlano.Add(FIDE.Nome + '  (' + ExcludeTrailingPathDelimiter(FIDE.RootDir) + ')');
    if not FRegistro.ChaveExiste then
      vPlano.Add(cmPlanoNuncaAberta)
    else if FExigirIDEFechada and FRegistro.IDEEmExecucao then
      vPlano.Add(cmPlanoAberta);
    if FSomenteLibraryPath then
      vPlano.Add(cmPlanoSomenteLibraryPath)
    else
    begin
      vPlano.Add(cmPlanoCompilar);
      for vInt := 0 to Pred(vLista.Count) do
        with TPacote(vLista[vInt]) do
          if Instalavel then
            vPlano.Add(Format(cmPlanoInstalarNaIDE, [Nome]))
          else
            vPlano.Add(Format(cmPlanoRuntime, [Nome]));
      for vInt := 0 to Pred(vFora.Count) do
        vPlano.Add(Format(cmPlanoFicaDeFora,
                          [vFora.Names[vInt], vFora.ValueFromIndex[vInt]]));
      if FIDE64 then
        vPlano.Add(cmPlanoIDE64);
      for vPlat in FPlataformas do
        vPlano.Add(Format(cmPlanoBplEm, [vPlat,
          PastaSaida(vPlat, 'Package DPL Output', FIDE.CommonDir + 'Bpl' +
                     IfThen(SameText(vPlat, 'win32'), '', '\' + vPlat))]));
    end;

    // dependencias de terceiros: o que ja esta, o que vai ser instalado e o
    // que falta (e deixa pacote do RAL de fora)
    if FReceitas <> nil then
    begin
      vExigidas := TStringList.Create;
      try
        FReceitas.Exigidas(FCatalogo, tpDelphi, vNomes, vExigidas);
        for vInt := 0 to Pred(vExigidas.Count) do
        begin
          vReceita := TReceita(vExigidas.Objects[vInt]);
          vVersao := VersaoDependencia(vReceita, vMotivo);
          vRaiz := '';
          if vMotivo = '' then
            vRaiz := FPastasDependencias.Values[ChaveDependencia(vReceita.Nome, vVersao)];
          vAtual := '';
          if not (FIgnorarExistentes and (vRaiz <> '')) then
            vAtual := DependenciaInstalada(vReceita);
          if vAtual <> '' then
            vPlano.Add(Format(cmPlanoDepJaInstalada,
                              [vReceita.Nome, vExigidas[vInt], vAtual]))
          else if vReceita.Pago then
            vPlano.Add(Format(cmPlanoDepComercial,
                              [vReceita.Nome, vExigidas[vInt], vReceita.Site,
                               vExigidas[vInt]]))
          else if (vRaiz <> '') and (vReceita.Delphi.Acoes.Count > 0) then
            vPlano.Add(Format(cmPlanoDepInstalar,
                              [vReceita.Nome, vVersao, vExigidas[vInt], vRaiz]))
          else
            vPlano.Add(Format(cmPlanoDepFalta,
                              [vReceita.Nome, vExigidas[vInt], vExigidas[vInt]]));
        end;
      finally
        vExigidas.Free;
      end;
    end;

    MontarCaminhos(vLista, vCaminhos, vUnidades);
    if FRegistro.AceitaVariaveis then
    begin
      vPlano.Add(Format(cmRegistroVariavel, [FNomeVariavel, FRaizFontes + 'src']));
      // a variavel e da IDE, nao do instalador: os projetos do usuario que usam
      // $(PascalRAL) passam a ver a pasta nova
      vAtual := FRegistro.LerValor('Environment Variables', FNomeVariavel);
      if (vAtual <> '') and
         not SameText(ExcludeTrailingPathDelimiter(vAtual),
                      ExcludeTrailingPathDelimiter(FRaizFontes + 'src')) then
        vPlano.Add(Format(cmPlanoVariavelMuda, [FNomeVariavel, vAtual]));
    end;
    vPlano.Add(Format(cmPlanoLibraryPath, [FPlataformas.CommaText]));
    for vInt := 0 to Pred(vCaminhos.Count) do
      vPlano.Add('    ' + vCaminhos[vInt]);
    Result := vPlano.Text;
  finally
    vNomes.Free;
    vFora.Free;
    vUnidades.Free;
    vCaminhos.Free;
    vPlano.Free;
    vLista.Free;
  end;
end;

function TInstalacaoDelphi.SalvarRecibo(APacotes: TList;
  AResultados: TStrings): string;
var
  vRaiz, vIDE, vRAL, vObj: TJSONObject;
  vLista: TJSONArray;
  vInt: integer;
  vArquivo: TStringList;
  vRepo, vVersao, vCommit: string;
begin
  Result := '';
  vRaiz := TJSONObject.Create;
  try
    vRaiz.Add('instalador', 'RALInstaller');
    vRaiz.Add('data', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', Now));
    vIDE := TJSONObject.Create;
    vIDE.Add('tipo', 'delphi');
    vIDE.Add('nome', FIDE.Nome);
    vIDE.Add('versao', FIDE.Versao);
    vIDE.Add('bds', FIDE.BDSVersao);
    vIDE.Add('raiz', FIDE.RootDir);
    vIDE.Add('chave', 'HKCU' + FRegistro.Chave);
    vRaiz.Add('ide', vIDE);
    vRaiz.Add('fontes', FRaizFontes);
    LerMarca(FRaizFontes, vRepo, vVersao, vCommit);
    vRAL := TJSONObject.Create;
    vRAL.Add('repositorio', vRepo);
    vRAL.Add('versao', vVersao);
    vRAL.Add('commit', vCommit);
    vRaiz.Add('ral', vRAL);
    vRaiz.Add('somente-library-path', FSomenteLibraryPath);

    // o que o instalador instalou e o que so encontrou
    vLista := TJSONArray.Create;
    for vInt := 0 to Pred(FDepsRecibo.Count) do
    begin
      vObj := TJSONObject.Create;
      vObj.Add('nome', FDepsRecibo.Names[vInt]);
      vObj.Add('origem', Copy(FDepsRecibo.ValueFromIndex[vInt], 1,
                              Pos('|', FDepsRecibo.ValueFromIndex[vInt]) - 1));
      vObj.Add('onde', Copy(FDepsRecibo.ValueFromIndex[vInt],
                            Pos('|', FDepsRecibo.ValueFromIndex[vInt]) + 1, MaxInt));
      vLista.Add(vObj);
    end;
    vRaiz.Add('dependencias', vLista);

    // os .bpl e .dcp gravados, com tamanho e data
    vLista := TJSONArray.Create;
    for vInt := 0 to Pred(FArquivosRecibo.Count) do
    begin
      vObj := TJSONObject.Create;
      vObj.Add('arquivo', FArquivosRecibo.Names[vInt]);
      vObj.Add('tamanho', StrToInt64Def(Copy(FArquivosRecibo.ValueFromIndex[vInt], 1,
        Pos('|', FArquivosRecibo.ValueFromIndex[vInt]) - 1), 0));
      vObj.Add('data', Copy(FArquivosRecibo.ValueFromIndex[vInt],
        Pos('|', FArquivosRecibo.ValueFromIndex[vInt]) + 1, MaxInt));
      vLista.Add(vObj);
    end;
    vRaiz.Add('arquivos', vLista);

    vLista := TJSONArray.Create;
    for vInt := 0 to Pred(AResultados.Count) do
      vLista.Add(AResultados[vInt]);
    vRaiz.Add('pacotes', vLista);
    vRaiz.Add('registro', FRegistro.AlteracoesJSON);

    ForceDirectories(FPastaRecibos);
    Result := IncludeTrailingPathDelimiter(FPastaRecibos) +
              Format('delphi-%s-%s.json',
                     [FIDE.BDSVersao, FormatDateTime('yyyymmdd-hhnnss', Now)]);
    vArquivo := TStringList.Create;
    try
      vArquivo.Text := vRaiz.FormatJSON;
      vArquivo.SaveToFile(Result);
    finally
      vArquivo.Free;
    end;
  finally
    vRaiz.Free;
  end;
end;

function TInstalacaoDelphi.Executar: boolean;
var
  vLista, vRegistrar: TList;
  vBuild: TBuildDelphi;
  vPlat: string;
  vInt, vRes, vAdicionados: integer;
  vOkWin32, vOkWin64, vResultados, vCaminhos, vUnidades, vConflitos: TStringList;
  vResultado: TResultadoPacote;
  vPacote: TPacote;
begin
  Result := False;
  PrepararIDE64;
  FRelatorio.Clear;
  FAvisos.Clear;
  FRecibo := '';
  FreeAndNil(FRegistro);
  FRegistro := TRegistroDelphi.Create(FIDE);
  FRegistro.Log := FLog;
  FRegistro.Simular := FSimular;
  if FChaveRegistro <> '' then
    FRegistro.Chave := FChaveRegistro;

  vLista := TList.Create;
  vRegistrar := TList.Create;
  vOkWin32 := TStringList.Create;
  vOkWin32.CaseSensitive := False;
  vOkWin64 := TStringList.Create;
  vOkWin64.CaseSensitive := False;
  vResultados := TStringList.Create;
  vCaminhos := TStringList.Create;
  vUnidades := TStringList.Create;
  vConflitos := TStringList.Create;
  try
    if not Conferir(vLista) then
      Exit;

    Result := True;

    // F6: o que nao cabe nesta IDE (unidade ou pacote que ela nao tem, faixa
    // do manifesto, dependencia sem versao para ela) sai antes de compilar
    FiltrarCompativeis(vLista, vConflitos);
    for vInt := 0 to Pred(vConflitos.Count) do
    begin
      Logar(Format(cmFicaDeFora,
                   [vConflitos.Names[vInt], vConflitos.ValueFromIndex[vInt]]));
      FAvisos.Add(Format(cmFicaDeFora,
                         [vConflitos.Names[vInt], vConflitos.ValueFromIndex[vInt]]));
      vResultados.Add(vConflitos.Names[vInt] + ': pulado — ' +
                      vConflitos.ValueFromIndex[vInt]);
    end;
    vConflitos.Clear;
    if vLista.Count = 0 then
    begin
      Logar(cmNenhumCabeDelphi);
      Exit(False);
    end;

    // 0. dependencias de terceiros (F7): antes de compilar, porque o RAL
    // compila contra elas; o que depende do que faltou sai da lista
    FVariaveisDeps.Clear;
    FPacotesLigados.Clear;
    FArquivosRecibo.Clear;
    FDepsRecibo.Clear;
    PrepararDependencias(vLista, vResultados);
    if vLista.Count = 0 then
    begin
      Logar(cmNenhumSobrou);
      Exit(False);
    end;
    if FAvisos.Count > 0 then
      Result := False;

    // 1. compilar, uma plataforma por vez; so o que compilou em Win32 vai
    // para a IDE (design-time e sempre Win32)
    if FSomenteLibraryPath then
    begin
      for vInt := 0 to Pred(vLista.Count) do
        vRegistrar.Add(vLista[vInt]);
    end
    else
    begin
      for vPlat in FPlataformas do
      begin
        if FIDE.Plataformas.IndexOf(LowerCase(vPlat)) < 0 then
        begin
          FAvisos.Add(Format(cmPlataformaIgnorada, [FIDE.Nome, vPlat]));
          Continue;
        end;

        vBuild := TBuildDelphi.Create(FIDE, FCatalogo);
        try
          vBuild.Plataforma := vPlat;
          vBuild.Log := FLog;
          vBuild.Simular := FSimular;
          // a IDE de 64 bits carrega os pacotes de design de Win64
          vBuild.Design64 := FIDE64 and SameText(vPlat, 'win64');
          vBuild.CaminhosExtras.AddStrings(FCaminhosExtras);
          vBuild.PacotesExtras.AddStrings(FPacotesLigados);
          if FPastaBpl <> '' then
            vBuild.PastaBpl := FPastaBpl +
                               IfThen(SameText(vPlat, 'win32'), '', '\' + vPlat)
          else
            vBuild.PastaBpl := PastaSaida(vPlat, 'Package DPL Output', '');
          if FPastaDcp <> '' then
            vBuild.PastaDcp := FPastaDcp +
                               IfThen(SameText(vPlat, 'win32'), '', '\' + vPlat)
          else
            vBuild.PastaDcp := PastaSaida(vPlat, 'Package DCP Output', '');
          // Win64 e as outras: o Package DPL Output de cada plataforma ja vem
          // com a subpasta; vazio cai no padrao do motor de build

          if not vBuild.Compilar(vLista) then
            Result := False;

          FRelatorio.Add('== ' + PlataformaRegistro(vPlat) + ' ==');
          FRelatorio.Add(vBuild.Relatorio);
          for vRes := 0 to Pred(vBuild.Total) do
          begin
            vResultado := vBuild.Resultados[vRes];
            AnotarArquivos(vResultado);
            vResultados.Add(Format('%s %s: %s%s', [vPlat, vResultado.Nome,
              IfThen(vResultado.Ok, 'ok', IfThen(vResultado.Pulado, 'pulado', 'falhou')),
              IfThen(vResultado.Motivo <> '', ' — ' + vResultado.Motivo, '')]));
            if vResultado.Ok and SameText(vPlat, 'win32') then
              vOkWin32.Values[vResultado.Nome] := vResultado.Bpl;
            if vResultado.Ok and not vResultado.Pulado and SameText(vPlat, 'win64') then
              vOkWin64.Values[vResultado.Nome] := vResultado.Bpl;
          end;
        finally
          vBuild.Free;
        end;
      end;

      for vInt := 0 to Pred(vLista.Count) do
        if vOkWin32.IndexOfName(TPacote(vLista[vInt]).Nome) >= 0 then
          vRegistrar.Add(vLista[vInt]);
    end;

    if vRegistrar.Count = 0 then
    begin
      Logar(cmNadaCompilou);
      Exit(False);
    end;

    // 2. library path e variavel, so do que vai ficar utilizavel
    Logar(Format(cmRegistro, [FRegistro.Chave]));
    MontarCaminhos(vRegistrar, vCaminhos, vUnidades);
    if FRegistro.AceitaVariaveis then
      if not FRegistro.DefinirVariavel(FNomeVariavel, FRaizFontes + 'src') then
        Result := False;

    for vPlat in FPlataformas do
    begin
      FRegistro.CaminhosConflitantes(vPlat, vCaminhos, vUnidades, vConflitos);
      for vInt := 0 to Pred(vConflitos.Count) do
        FAvisos.Add(Format(cmOutraCopiaRAL,
                           [PlataformaRegistro(vPlat), vConflitos[vInt]]));

      vAdicionados := FRegistro.AdicionarCaminhos(vPlat, 'Search Path', vCaminhos);
      if vAdicionados < 0 then
        Result := False
      else if vAdicionados = 0 then
        Logar(Format(cmJaTinhaCaminhos, [PlataformaRegistro(vPlat)]));
    end;

    // 3. pacotes na IDE: so design-time (ou runtime+design) que compilou
    if not FSomenteLibraryPath then
      for vInt := 0 to Pred(vRegistrar.Count) do
      begin
        vPacote := TPacote(vRegistrar[vInt]);
        if not vPacote.Instalavel then
          Continue;
        if not FRegistro.RegistrarPacote(vOkWin32.Values[vPacote.Nome],
             IfThen(vPacote.Descricao <> '', vPacote.Descricao, vPacote.Nome)) then
          Result := False;
        // e na IDE de 64 bits, o que compilou em Win64
        if FIDE64 and (vOkWin64.Values[vPacote.Nome] <> '') then
          if not FRegistro.RegistrarPacote(vOkWin64.Values[vPacote.Nome],
               IfThen(vPacote.Descricao <> '', vPacote.Descricao, vPacote.Nome),
               True) then
            Result := False;
      end;

    for vInt := 0 to Pred(vLista.Count) do
      if vRegistrar.IndexOf(vLista[vInt]) < 0 then
        FAvisos.Add(Format(cmNaoCompilouNaoRegistrado, [TPacote(vLista[vInt]).Nome]));

    // 4. recibo: o que mudou, com o valor de antes
    if not FSimular then
    begin
      try
        FRecibo := SalvarRecibo(vLista, vResultados);
        Logar(Format(cmRecibo, [FRecibo]));
      except
        on E: Exception do
          FAvisos.Add(Format(emGravarRecibo, [E.Message]));
      end;
    end;

    FRelatorio.Add(Format(cmRegistroAlteracoes,
      [FRegistro.TotalAlteracoes, FRegistro.Chave, IfThen(FSimular, cmSimulado, '')]));
    for vInt := 0 to Pred(FAvisos.Count) do
      FRelatorio.Add(cmPrefixoAvisoRelatorio + FAvisos[vInt]);
  finally
    vConflitos.Free;
    vUnidades.Free;
    vCaminhos.Free;
    vResultados.Free;
    vOkWin64.Free;
    vOkWin32.Free;
    vRegistrar.Free;
    vLista.Free;
  end;
end;

function TInstalacaoDelphi.NomesDoCatalogo: TStringList;
var
  vLista: TList;
  vInt: integer;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  if FCatalogo = nil then
    Exit;
  vLista := TList.Create;
  try
    FCatalogo.Listar(tpDelphi, vLista);
    for vInt := 0 to Pred(vLista.Count) do
      Result.Add(TPacote(vLista[vInt]).Nome);
  finally
    vLista.Free;
  end;
end;

procedure TInstalacaoDelphi.PrepararIDE64;
begin
  // a IDE de 64 bits (Delphi 12 em diante) carrega pacotes de design de Win64:
  // o Win64 entra na rodada sozinho, e so modo library path nao compila nada
  FIDE64 := FUsarIDE64 and FIDE.IDE64 and not FSomenteLibraryPath and
            (FIDE.Plataformas.IndexOf('win64') >= 0);
  if FIDE64 and (FPlataformas.IndexOf('win64') < 0) then
    FPlataformas.Add('win64');
end;

function TInstalacaoDelphi.Existente: TInstalacaoExistente;
var
  vNomes: TStringList;
begin
  vNomes := NomesDoCatalogo;
  try
    Result := DetectarDelphi(FIDE, FChaveRegistro, vNomes);
  finally
    vNomes.Free;
  end;
end;

// os pacotes registrados que parecem usar o RAL e nao sao dele (RALRESTDW):
// continuam na IDE depois de desinstalar, e sem o RAL nao carregam
procedure OutrosComRAL(ARegistro: TRegistroDelphi; AExistente: TInstalacaoExistente;
  ALista: TStrings);
var
  vValores: TStringList;
  vValor, vNome, vLista: string;
begin
  ALista.Clear;
  vValores := TStringList.Create;
  try
    for vLista in TStringArray.Create('Known Packages', 'Known Packages x64') do
    begin
      ARegistro.ListarValores(vLista, vValores);
      for vValor in vValores do
      begin
        vNome := ChangeFileExt(ExtractFileName(vValor), '');
        if (Pos('RAL', UpperCase(vNome)) > 0) and not AExistente.DoRAL(vNome) and
           (ALista.IndexOf(vNome) < 0) then
          ALista.Add(vNome);
      end;
    end;
  finally
    vValores.Free;
  end;
end;

function TInstalacaoDelphi.PlanoDesinstalar: string;
var
  vExistente: TInstalacaoExistente;
  vPlano, vOutros: TStringList;
  vRecibos: TRecibos;
  vLista: TList;
begin
  GarantirRegistro;
  vPlano := TStringList.Create;
  vOutros := TStringList.Create;
  vRecibos := TRecibos.Create(True);
  vLista := TList.Create;
  vExistente := Existente;
  try
    vPlano.Add(FIDE.Nome + '  (' + ExcludeTrailingPathDelimiter(FIDE.RootDir) + ')');
    if FExigirIDEFechada and FRegistro.IDEEmExecucao then
      vPlano.Add(cmPlanoAberta);
    vRecibos.Carregar(FPastaRecibos);
    vRecibos.DaIDE(FIDE.RootDir, vLista);
    if (vLista.Count = 0) and not vExistente.Existe then
    begin
      vPlano.Add(cmDesinstalarNadaAFazer);
      Exit(vPlano.Text);
    end;
    vPlano.Add(cmPlanoDesinstalar);
    if vLista.Count > 0 then
      vPlano.Add(Format(cmDesfazerRecibos, [vLista.Count]));
    if vExistente.Pacotes.Count > 0 then
      vPlano.Add(Format(cmTirarDaIDE, [Juntar(vExistente.Pacotes, True)]));
    if vExistente.Pacotes64.Count > 0 then
      vPlano.Add(Format(cmTirarDaIDE64, [Juntar(vExistente.Pacotes64, True)]));
    if vExistente.Caminhos.Count > 0 then
      vPlano.Add(Format(cmTirarCaminhos, [vExistente.Caminhos.Count]));
    if vExistente.Variavel <> '' then
      vPlano.Add(Format(cmApagarVariavel, [vExistente.Variavel]));
    if (vExistente.Pacotes.Count > 0) or (vExistente.Pacotes64.Count > 0) then
      vPlano.Add(cmBplsFicam);
    OutrosComRAL(FRegistro, vExistente, vOutros);
    if vOutros.Count > 0 then
      vPlano.Add(Format(cmOutrosUsamRAL, [Juntar(vOutros, False)]));
    Result := vPlano.Text;
  finally
    vExistente.Free;
    vLista.Free;
    vRecibos.Free;
    vOutros.Free;
    vPlano.Free;
  end;
end;

function TInstalacaoDelphi.SalvarDesinstalacao: string;
var
  vRaiz, vIDE: TJSONObject;
  vArquivo: TStringList;
  vPasta: string;
begin
  Result := '';
  vRaiz := TJSONObject.Create;
  try
    vRaiz.Add('instalador', 'RALInstaller');
    vRaiz.Add('acao', 'desinstalacao');
    vRaiz.Add('data', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', Now));
    vIDE := TJSONObject.Create;
    vIDE.Add('tipo', 'delphi');
    vIDE.Add('nome', FIDE.Nome);
    vIDE.Add('raiz', FIDE.RootDir);
    vIDE.Add('chave', 'HKCU' + FRegistro.Chave);
    vRaiz.Add('ide', vIDE);
    vRaiz.Add('registro', FRegistro.AlteracoesJSON);
    // fora da pasta dos recibos: nao e uma instalacao
    vPasta := IncludeTrailingPathDelimiter(FPastaRecibos) + 'desinstalacoes';
    ForceDirectories(vPasta);
    Result := IncludeTrailingPathDelimiter(vPasta) +
              Format('delphi-%s-%s.json',
                     [FIDE.BDSVersao, FormatDateTime('yyyymmdd-hhnnss', Now)]);
    vArquivo := TStringList.Create;
    try
      vArquivo.Text := vRaiz.FormatJSON;
      vArquivo.SaveToFile(Result);
    finally
      vArquivo.Free;
    end;
  finally
    vRaiz.Free;
  end;
end;

function TInstalacaoDelphi.Desinstalar: boolean;
var
  vExistente: TInstalacaoExistente;
  vRecibos: TRecibos;
  vLista: TList;
  vValores, vItens, vSubchaves: TStringList;
  vInt, vPos: integer;
  vSub, vEntrada, vValor, vNovo: string;
begin
  Result := True;
  FRelatorio.Clear;
  FAvisos.Clear;
  FreeAndNil(FRegistro);
  GarantirRegistro;
  if not FRegistro.ChaveExiste then
  begin
    Logar(Format(emDelphiNuncaAberto, [FIDE.Nome, FRegistro.Chave]));
    Exit(False);
  end;
  if FExigirIDEFechada and FRegistro.IDEEmExecucao then
  begin
    Logar(Format(emIDEAbertaDesinstalar, [FIDE.Nome]));
    Exit(False);
  end;

  // 1. o que o instalador fez: os recibos, do mais novo ao mais velho
  vRecibos := TRecibos.Create(True);
  vLista := TList.Create;
  try
    vRecibos.Carregar(FPastaRecibos);
    vRecibos.DaIDE(FIDE.RootDir, vLista);
    if (vLista.Count > 0) and not FSimular then
      if not DesinstalarIDE(FPastaRecibos, FIDE.RootDir, FLog, False,
                            FExigirIDEFechada) then
        Result := False;
  finally
    vLista.Free;
    vRecibos.Free;
  end;

  // 2. o que sobrou, feito a mao (ou antes do instalador)
  FreeAndNil(FRegistro);
  GarantirRegistro;
  vExistente := Existente;
  vValores := TStringList.Create;
  vItens := TStringList.Create;
  vSubchaves := TStringList.Create;
  try
    if not vExistente.Existe then
    begin
      Logar(cmDesinstaladoSemResto);
      Exit;
    end;
    for vInt := 0 to Pred(vExistente.Pacotes.Count) do
    begin
      vValor := vExistente.Pacotes.ValueFromIndex[vInt];
      Logar('  Known Packages -= ' + vValor);
      if not FRegistro.Remover('Known Packages', vValor) then
        Result := False;
    end;
    for vInt := 0 to Pred(vExistente.Pacotes64.Count) do
    begin
      vValor := vExistente.Pacotes64.ValueFromIndex[vInt];
      Logar('  Known Packages x64 -= ' + vValor);
      if not FRegistro.Remover('Known Packages x64', vValor) then
        Result := False;
    end;
    // o mesmo pacote desabilitado numa vez que falhou
    for vSub in TStringArray.Create('Disabled Packages', 'Disabled Packages x64') do
    begin
      FRegistro.ListarValores(vSub, vValores);
      for vValor in vValores do
        if vExistente.DoRAL(NomeDoBpl(FRegistro.Expandir(vValor), vExistente.Nomes)) then
        begin
          Logar('  ' + vSub + ' -= ' + vValor);
          FRegistro.Remover(vSub, vValor);
        end;
    end;

    // library path: cada plataforma perde so as entradas do RAL
    for vInt := 0 to Pred(vExistente.Caminhos.Count) do
    begin
      vSub := Copy(vExistente.Caminhos[vInt], 1, Pos('|', vExistente.Caminhos[vInt]) - 1);
      if vSubchaves.IndexOf(vSub) < 0 then
        vSubchaves.Add(vSub);
    end;
    for vSub in vSubchaves do
    begin
      vItens.StrictDelimiter := True;
      vItens.Delimiter := ';';
      vItens.DelimitedText := FRegistro.LerValor(vSub, 'Search Path');
      vNovo := '';
      for vPos := 0 to Pred(vItens.Count) do
      begin
        vEntrada := vItens[vPos];
        if vExistente.Caminhos.IndexOf(vSub + '|' + Trim(vEntrada)) >= 0 then
        begin
          Logar(Format(cmCaminhoRemovido, [vSub, Trim(vEntrada)]));
          Continue;
        end;
        if vNovo <> '' then
          vNovo := vNovo + ';';
        vNovo := vNovo + vEntrada;
      end;
      if not FRegistro.Escrever(vSub, 'Search Path', vNovo) then
        Result := False;
    end;

    if vExistente.Variavel <> '' then
    begin
      Logar(Format(cmVariavelRemovida, [vExistente.Variavel]));
      if not FRegistro.Remover('Environment Variables', 'PascalRAL') then
        Result := False;
    end;

    OutrosComRAL(FRegistro, vExistente, vItens);
    if vItens.Count > 0 then
      FAvisos.Add(Format(cmOutrosUsamRALAviso, [Juntar(vItens, False)]));

    if not FSimular and (FRegistro.TotalAlteracoes > 0) then
      try
        Logar(Format(cmDesfazerDesinstalacao, [SalvarDesinstalacao]));
      except
        on E: Exception do
          FAvisos.Add(Format(emGravarRecibo, [E.Message]));
      end;
    FRelatorio.Add(Format(cmRegistroAlteracoes,
      [FRegistro.TotalAlteracoes, FRegistro.Chave, IfThen(FSimular, cmSimulado, '')]));
    for vInt := 0 to Pred(FAvisos.Count) do
      FRelatorio.Add(cmPrefixoAvisoRelatorio + FAvisos[vInt]);
  finally
    vSubchaves.Free;
    vItens.Free;
    vValores.Free;
    vExistente.Free;
  end;
end;

end.