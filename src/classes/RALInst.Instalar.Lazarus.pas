/// Installation in a Lazarus IDE: lazbuild with that installation's
/// configuration, in the catalog graph order, and a single --build-ide at the
/// end:
///   1. --add-package-link with every .lpk of the run: Lazarus learns each
///      package, including those that only exist as a dependency;
///   2. --add-package with the design ones (or runtime+design): they enter the
///      IDE's installed package list;
///   3. --build-ide=, once: compiles everything the IDE now requires.
unit RALInst.Instalar.Lazarus;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALInst.Catalogo, RALInst.Compatibilidade, RALInst.Existente, RALInst.IDE,
  RALInst.Processo, RALInst.Receitas;

type
  /// Installs the chosen RAL packages (and their dependencies) in one Lazarus.
  TInstalacaoLazarus = class
  private
    FAvisos: TStringList;
    FCatalogo: TCatalogo;
    FCompat: TCompatibilidade;
    FConstruirIDE: boolean;
    /// name=encontrada|where or name=instalada|folder (version)
    FDepsRecibo: TStringList;
    FExigirIDEFechada: boolean;
    FIDE: TIDEInstance;
    FIgnorarExistentes: boolean;
    FInstaladosAntes: TStringList;
    FLinksAntes: TStringList;
    FLog: TLogLinha;
    FManifesto: TManifesto;
    FPacotes: TStringList;
    FPastaRecibos: string;
    FPastasDependencias: TStringList;
    FRaizFontes: string;
    FReceitas: TReceitas;
    FRecibo: string;
    FRelatorio: TStringList;
    FSimular: boolean;
    /// The .lpk file of a package in the sources folder
    function Arquivo(APacote: TPacote): string;
    /// The compatibility check, created on first use
    function Compat: TCompatibilidade;
    /// TDetectarDependencia for the compatibility check
    function DetectarNaIDE(AIDE: TIDEInstance; AReceita: TReceita): string;
    /// Packages from outside RAL the IDE does not know and no dependency of
    /// this run provides (AFornecidos)
    procedure ExternosAusentes(AInstalar: TList; AAusentes: TStrings;
      AFornecidos: TStrings = nil);
    /// Runs lazbuild; False when it failed
    function Lazbuild(AParams: TStrings): boolean;
    /// Passes the line to Log when assigned
    procedure Logar(const ALinha: string);
    /// The packages this IDE already knows: those shipped with it
    /// (packager/globallinks) and those registered in the configuration
    procedure PacotesConhecidos(AConhecidos: TStrings);
    /// --primary-config-path and --lazarusdir of this installation
    procedure ParametrosBase(AParams: TStrings);
    /// TPastaDependencia for the compatibility check
    function PastaNaIDE(AIDE: TIDEInstance; AReceita: TReceita): string;
    /// For each required dependency, uses the one already there, or puts its
    /// .lpk files before RAL's; what depends on a missing one goes to AFora
    procedure PrepararDependencias(AInstalar: TList; ALinks, AAdd, AFornecidos,
      AFora: TStrings);
    /// The configuration goes back to what it was before this run (lazbuild
    /// failed)
    procedure Reverter;
    /// Writes the undo record of an uninstall (the configuration before)
    function SalvarDesinstalacao: string;
    /// Writes the receipt of the run; returns its file
    function SalvarRecibo(AInstalar: TList; AReconstruiu: boolean): string;
    /// Splits what can be installed from what must stay out (missing source or
    /// submodule, or dependency of something left out)
    procedure Separar(ALista, AInstalar: TList; AFora: TStrings);
    procedure SetRaizFontes(const AValor: string);
  public
    constructor Create(AIDE: TIDEInstance; ACatalogo: TCatalogo);
    destructor Destroy; override;
    /// Where the dependency already is in this IDE ('' if it is not)
    function DependenciaInstalada(AReceita: TReceita): string;
    /// Removes RAL from the IDE: undoes the installer's receipts, then takes out
    /// what is left of a hand installation (package links, packages built into
    /// the IDE) and rebuilds the IDE when its package list changed. An undo
    /// record of the configuration goes to <receipts>\desinstalacoes
    function Desinstalar: boolean;
    /// What RAL the IDE already has, by hand or by the installer (the caller
    /// frees it)
    function Existente: TInstalacaoExistente;
    /// Returns False when something failed; the report says what entered
    function Executar: boolean;
    /// Root of the dependency copy registered in this IDE (the .lpk link in
    /// packagefiles.xml, minus the recipe's .lpk path); '' if unknown
    function PastaDependencia(AReceita: TReceita): string;
    /// What the run will do, without doing anything
    function Plano: string;
    /// What Desinstalar will do, without doing anything
    function PlanoDesinstalar: string;
    /// The dependency version this IDE asks; '' and AMotivo when none works
    /// (Zeos on FPC 3.2.3)
    function VersaoDependencia(AReceita: TReceita; out AMotivo: string): string;

    property Avisos: TStringList read FAvisos;
    /// False registers the packages without rebuilding the IDE (it asks when
    /// it opens)
    property ConstruirIDE: boolean read FConstruirIDE write FConstruirIDE;
    /// Refuses to touch the configuration with Lazarus open (it rewrites it on
    /// close)
    property ExigirIDEFechada: boolean read FExigirIDEFechada write FExigirIDEFechada;
    property IDE: TIDEInstance read FIDE;
    property IgnorarExistentes: boolean read FIgnorarExistentes write FIgnorarExistentes;
    property Log: TLogLinha read FLog write FLog;
    /// The RAL version manifest (not owned)
    property Manifesto: TManifesto read FManifesto write FManifesto;
    property Pacotes: TStringList read FPacotes;
    /// Where to write the receipt (what changed in the configuration)
    property PastaRecibos: string read FPastaRecibos write FPastaRecibos;
    /// Where each dependency was downloaded (name@version=folder)
    property PastasDependencias: TStringList read FPastasDependencias;
    /// Where the sources are (or will be, after download)
    property RaizFontes: string read FRaizFontes write SetRaizFontes;
    /// The known recipes (not owned)
    property Receitas: TReceitas read FReceitas write FReceitas;
    /// Receipt file of this run ('' when none was written)
    property Recibo: string read FRecibo;
    property Relatorio: TStringList read FRelatorio;
    property Simular: boolean read FSimular write FSimular;
  end;

const
  /// Executables of an open Lazarus
  {$IFDEF MSWINDOWS}
  ExecutaveisLazarus: array[0..1] of string = ('lazarus.exe', 'startlazarus.exe');
  {$ELSE}
  ExecutaveisLazarus: array[0..1] of string = ('lazarus', 'startlazarus');
  {$ENDIF}

implementation

uses
  StrUtils, RegExpr, fpjson,
  RALInst.Config.Lazarus, RALInst.Fontes, RALInst.Mensagens, RALInst.Recibos;

{ TInstalacaoLazarus }

// nome=arquivo dos links; o packagefiles.xml pode repetir um nome (duas copias
// do mesmo pacote), e o JSON nao aceita: fica o primeiro, que e o que o
// Lazarus usa
function ObjetoDe(ALista: TStrings): TJSONObject;
var
  vInt: integer;
begin
  Result := TJSONObject.Create;
  for vInt := 0 to Pred(ALista.Count) do
    if Result.IndexOfName(ALista.Names[vInt]) < 0 then
      Result.Add(ALista.Names[vInt], ALista.ValueFromIndex[vInt]);
end;

function ListaDe(ALista: TStrings): TJSONArray;
var
  vInt: integer;
begin
  Result := TJSONArray.Create;
  for vInt := 0 to Pred(ALista.Count) do
    Result.Add(ALista[vInt]);
end;

procedure TInstalacaoLazarus.SetRaizFontes(const AValor: string);
begin
  FRaizFontes := '';
  if AValor <> '' then
    FRaizFontes := IncludeTrailingPathDelimiter(AValor);
end;

constructor TInstalacaoLazarus.Create(AIDE: TIDEInstance; ACatalogo: TCatalogo);
begin
  inherited Create;
  FIDE := AIDE;
  FCatalogo := ACatalogo;
  if (FCatalogo <> nil) and (FCatalogo.Origem is TOrigemLocal) then
    FRaizFontes := IncludeTrailingPathDelimiter(TOrigemLocal(FCatalogo.Origem).Raiz);
  FPacotes := TStringList.Create;
  FPacotes.CaseSensitive := False;
  FRelatorio := TStringList.Create;
  FAvisos := TStringList.Create;
  FConstruirIDE := True;
  FPastasDependencias := TStringList.Create;
  FPastasDependencias.CaseSensitive := False;
  FPastaRecibos := PastaDadosInstalador + 'recibos';
  FExigirIDEFechada := True;
  FLinksAntes := TStringList.Create;
  FLinksAntes.CaseSensitive := False;
  FInstaladosAntes := TStringList.Create;
  FInstaladosAntes.CaseSensitive := False;
  FDepsRecibo := TStringList.Create;
end;

destructor TInstalacaoLazarus.Destroy;
begin
  FDepsRecibo.Free;
  FInstaladosAntes.Free;
  FLinksAntes.Free;
  FCompat.Free;
  FPastasDependencias.Free;
  FAvisos.Free;
  FRelatorio.Free;
  FPacotes.Free;
  inherited Destroy;
end;

function TInstalacaoLazarus.Compat: TCompatibilidade;
begin
  if FCompat = nil then
  begin
    FCompat := TCompatibilidade.Create(FCatalogo, FManifesto, FReceitas);
    FCompat.Detectar := @DetectarNaIDE;
    FCompat.PastaInstalada := @PastaNaIDE;
  end;
  Result := FCompat;
end;

function TInstalacaoLazarus.PastaNaIDE(AIDE: TIDEInstance;
  AReceita: TReceita): string;
begin
  Result := PastaDependencia(AReceita);
end;

function TInstalacaoLazarus.PastaDependencia(AReceita: TReceita): string;
var
  vTexto, vLinks: TStringList;
  vRegex: TRegExpr;
  vInt: integer;
  vRel, vNome, vArquivo: string;
begin
  Result := '';
  if (FIDE.ConfigDir = '') or not FileExists(FIDE.ConfigDir + 'packagefiles.xml') then
    Exit;
  vTexto := TStringList.Create;
  vLinks := TStringList.Create;
  vRegex := TRegExpr.Create('<Name Value="([^"]+)"/>.*?<Filename Value="([^"]+)"/>');
  try
    vLinks.CaseSensitive := False;
    vTexto.LoadFromFile(FIDE.ConfigDir + 'packagefiles.xml');
    // nome=arquivo de cada link (Item1, Item2...)
    vRegex.ModifierS := True;
    vRegex.ModifierG := False;
    if vRegex.Exec(vTexto.Text) then
      repeat
        vLinks.Values[vRegex.Match[1]] := StringReplace(vRegex.Match[2],
          '$(LazarusDir)', ExcludeTrailingPathDelimiter(FIDE.RootDir),
          [rfReplaceAll, rfIgnoreCase]);
      until not vRegex.ExecNext;

    // um .lpk da receita que esteja registrado diz onde fica a raiz
    for vInt := 0 to Pred(AReceita.Lazarus.Acoes.Count) do
    begin
      if AReceita.Lazarus.Acao(vInt).Tipo <> taLpk then
        Continue;
      vRel := StringReplace(AReceita.Lazarus.Acao(vInt).Arquivo, '/', PathDelim,
                            [rfReplaceAll]);
      vNome := ChangeFileExt(ExtractFileName(vRel), '');
      vArquivo := SetDirSeparators(vLinks.Values[vNome]);
      if (vArquivo <> '') and
         SameFileName(Copy(vArquivo, Length(vArquivo) - Length(vRel) + 1, MaxInt),
                      vRel) then
        Exit(Copy(vArquivo, 1, Length(vArquivo) - Length(vRel)));
    end;
  finally
    vRegex.Free;
    vLinks.Free;
    vTexto.Free;
  end;
end;

function TInstalacaoLazarus.DetectarNaIDE(AIDE: TIDEInstance;
  AReceita: TReceita): string;
begin
  Result := DependenciaInstalada(AReceita);
end;

function TInstalacaoLazarus.VersaoDependencia(AReceita: TReceita;
  out AMotivo: string): string;
begin
  Result := Compat.VersaoDependencia(AReceita, FIDE, AMotivo);
end;

procedure TInstalacaoLazarus.Logar(const ALinha: string);
begin
  if Assigned(FLog) then
    FLog(ALinha);
end;

procedure TInstalacaoLazarus.ParametrosBase(AParams: TStrings);
begin
  // cada Lazarus tem a sua configuracao; sem ela o lazbuild usa a padrao do
  // sistema e instala no Lazarus errado
  if FIDE.ConfigDir <> '' then
    AParams.Add('--primary-config-path=' + ExcludeTrailingPathDelimiter(FIDE.ConfigDir));
  AParams.Add('--lazarusdir=' + ExcludeTrailingPathDelimiter(FIDE.RootDir));
end;

function TInstalacaoLazarus.Lazbuild(AParams: TStrings): boolean;
var
  vExec: TExecucao;
begin
  vExec := TExecucao.Create;
  try
    vExec.Executavel := FIDE.BuildFile;
    vExec.Parametros.Assign(AParams);
    vExec.Log := FLog;
    if FSimular then
    begin
      Logar(Format(cmSimulacaoComando, [vExec.LinhaComando]));
      Exit(True);
    end;
    Result := vExec.Executar;
    if not Result and (vExec.Erro = '') then
      Logar(Format(emLazbuildCodigo, [vExec.CodigoSaida]));
  finally
    vExec.Free;
  end;
end;

function TInstalacaoLazarus.Arquivo(APacote: TPacote): string;
begin
  Result := FRaizFontes + StringReplace(APacote.ArquivoRelativo, '/', PathDelim,
                                        [rfReplaceAll]);
end;

procedure TInstalacaoLazarus.Separar(ALista, AInstalar: TList; AFora: TStrings);
var
  vInt, vDep: integer;
  vPacote: TPacote;
  vMotivo: string;
  vForaNomes: TStringList;
begin
  AInstalar.Clear;
  AFora.Clear;
  vForaNomes := TStringList.Create;
  try
    vForaNomes.CaseSensitive := False;
    for vInt := 0 to Pred(ALista.Count) do
    begin
      vPacote := TPacote(ALista[vInt]);
      vMotivo := '';
      // de uma origem que baixa os submodulos (o zip da versao), ausente
      // quer dizer "ainda nao baixado"
      if (vPacote.SubmodulosAusentes.Count > 0) and
         not FCatalogo.Origem.BaixaSubmodulos then
        vMotivo := Format(cmSubmoduloAusente, [vPacote.SubmodulosAusentes.CommaText])
      else if vPacote.FontesAusentes.Count > 0 then
        vMotivo := Format(cmFonteAusente, [vPacote.FontesAusentes[0]])
      else
        for vDep := 0 to Pred(vPacote.Internos.Count) do
          if vForaNomes.IndexOf(vPacote.Internos[vDep]) >= 0 then
          begin
            vMotivo := Format(cmDependeDeFora, [vPacote.Internos[vDep]]);
            Break;
          end;

      // F6: faixa do manifesto, dependencia sem versao para este FPC
      if vMotivo = '' then
        vMotivo := Compat.Motivo(vPacote, FIDE);

      if vMotivo = '' then
        AInstalar.Add(vPacote)
      else
      begin
        vForaNomes.Add(vPacote.Nome);
        AFora.Add(vPacote.Nome + ': ' + vMotivo);
      end;
    end;
  finally
    vForaNomes.Free;
  end;
end;

procedure TInstalacaoLazarus.PacotesConhecidos(AConhecidos: TStrings);
var
  vRegex: TRegExpr;
  vLinhas: TStringList;
  vBusca: TSearchRec;
  vNome, vArquivo: string;
  vInt, vPos: integer;
begin
  vLinhas := TStringList.Create;
  try
    // os que vem com o Lazarus: packager/globallinks/<nome>-<versao>.lpl
    if FindFirst(FIDE.RootDir + 'packager' + PathDelim + 'globallinks' + PathDelim +
                 '*.lpl', faAnyFile, vBusca) = 0 then
    try
      repeat
        vNome := ChangeFileExt(vBusca.Name, '');
        vPos := RPos('-', vNome);
        if vPos > 0 then
          vNome := Copy(vNome, 1, vPos - 1);
        AConhecidos.Add(vNome);
      until FindNext(vBusca) <> 0;
    finally
      SysUtils.FindClose(vBusca);
    end;

    // os que o usuario registrou (OPM, --add-package-link): packagefiles.xml.
    // Link para um .lpk que nao existe mais (pasta apagada, drive desmontado)
    // nao conta: a IDE nao consegue compilar o pacote
    vArquivo := FIDE.ConfigDir + 'packagefiles.xml';
    if (FIDE.ConfigDir <> '') and FileExists(vArquivo) then
    begin
      vLinhas.LoadFromFile(vArquivo);
      vRegex := TRegExpr.Create('<Name Value="([^"]+)"/>.*?<Filename Value="([^"]+)"/>');
      try
        vRegex.ModifierS := True;
        if vRegex.Exec(vLinhas.Text) then
          repeat
            vNome := StringReplace(vRegex.Match[2], '$(LazarusDir)',
              ExcludeTrailingPathDelimiter(FIDE.RootDir), [rfReplaceAll, rfIgnoreCase]);
            if FileExists(SetDirSeparators(vNome)) then
              AConhecidos.Add(vRegex.Match[1]);
          until not vRegex.ExecNext;
      finally
        vRegex.Free;
      end;
    end;
  finally
    vLinhas.Free;
  end;
end;

function TInstalacaoLazarus.DependenciaInstalada(AReceita: TReceita): string;
var
  vConhecidos: TStringList;
  vInt: integer;
begin
  Result := '';
  if not AReceita.Lazarus.Existe then
    Exit;
  vConhecidos := TStringList.Create;
  try
    vConhecidos.CaseSensitive := False;
    PacotesConhecidos(vConhecidos);
    for vInt := 0 to Pred(AReceita.Lazarus.Deteccao.Count) do
      if (AReceita.Lazarus.Deteccao.Names[vInt] = 'pacote') and
         (vConhecidos.IndexOf(AReceita.Lazarus.Deteccao.ValueFromIndex[vInt]) >= 0) then
        Exit(Format(cmPacoteJaRegistrado,
                    [AReceita.Lazarus.Deteccao.ValueFromIndex[vInt]]));
  finally
    vConhecidos.Free;
  end;
end;

procedure TInstalacaoLazarus.PrepararDependencias(AInstalar: TList; ALinks, AAdd,
  AFornecidos, AFora: TStrings);
var
  vExigidas, vFaltando, vForaNomes, vNomes: TStringList;
  vInt, vAcao, vRec, vDep: integer;
  vReceita: TReceita;
  vRaiz, vOnde, vMotivo, vArquivo, vVersao, vMotivoVersao: string;
  vPacote: TPacote;
begin
  if FReceitas = nil then
    Exit;
  vExigidas := TStringList.Create;
  vFaltando := TStringList.Create;
  vForaNomes := TStringList.Create;
  vForaNomes.CaseSensitive := False;
  // so o que sobrou da separacao (fonte ausente, compatibilidade)
  vNomes := TStringList.Create;
  vNomes.CaseSensitive := False;
  try
    for vInt := 0 to Pred(AInstalar.Count) do
      vNomes.Add(TPacote(AInstalar[vInt]).Nome);
    FReceitas.Exigidas(FCatalogo, tpLazarus, vNomes, vExigidas);
    for vInt := 0 to Pred(vExigidas.Count) do
    begin
      vReceita := TReceita(vExigidas.Objects[vInt]);
      Logar(Format(cmDependenciaPara, [vReceita.Nome, vExigidas[vInt]]));
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
        // o recibo diz que ela foi encontrada: desinstalar nao a leva junto
        FDepsRecibo.Values[vReceita.Nome] := 'encontrada|' + vOnde;
        Continue;
      end;

      vMotivo := '';
      if vReceita.Pago then
        vMotivo := Format(cmDepComercial, [vReceita.Nome, vReceita.Site])
      else if vMotivoVersao <> '' then
        vMotivo := vMotivoVersao
      else if vRaiz = '' then
        vMotivo := Format(cmDepNaoBaixada, [vReceita.Nome, vVersao])
      else
        for vAcao := 0 to Pred(vReceita.Lazarus.Acoes.Count) do
          if vReceita.Lazarus.Acao(vAcao).Tipo = taLpk then
          begin
            vArquivo := ExpandirRaiz('{raiz}/' + vReceita.Lazarus.Acao(vAcao).Arquivo,
                                     vRaiz);
            if not FileExists(vArquivo) then
            begin
              vMotivo := Format(cmLpkNaoExiste, [vReceita.Nome, vArquivo]);
              Break;
            end;
            ALinks.Add(vArquivo);
            if vReceita.Lazarus.Acao(vAcao).Instalar then
              AAdd.Add(vArquivo);
          end;

      if vMotivo = '' then
      begin
        Logar(Format(cmDepInstalandoDe, [vRaiz]));
        AFornecidos.AddStrings(vReceita.Lazarus.FornecePacotes);
        FDepsRecibo.Values[vReceita.Nome] := 'instalada|' + vRaiz + ' (' + vVersao +
                                             ')';
      end
      else
      begin
        Logar('  ' + vMotivo);
        vFaltando.AddObject(vMotivo, vReceita);
      end;
    end;

    // quem precisava do que faltou fica de fora, e quem depende dele tambem
    for vInt := 0 to Pred(AInstalar.Count) do
    begin
      vPacote := TPacote(AInstalar[vInt]);
      vMotivo := '';
      for vRec := 0 to Pred(vFaltando.Count) do
        if TReceita(vFaltando.Objects[vRec]).Atende(vPacote) then
          vMotivo := vFaltando[vRec];
      if vMotivo = '' then
        for vDep := 0 to Pred(vPacote.Internos.Count) do
          if vForaNomes.IndexOf(vPacote.Internos[vDep]) >= 0 then
            vMotivo := Format(cmDependeDeFora, [vPacote.Internos[vDep]]);
      if vMotivo <> '' then
      begin
        vForaNomes.Add(vPacote.Nome);
        AFora.Add(vPacote.Nome + ': ' + vMotivo);
      end;
    end;
    for vInt := Pred(AInstalar.Count) downto 0 do
      if vForaNomes.IndexOf(TPacote(AInstalar[vInt]).Nome) >= 0 then
        AInstalar.Delete(vInt);
  finally
    vNomes.Free;
    vForaNomes.Free;
    vFaltando.Free;
    vExigidas.Free;
  end;
end;

procedure TInstalacaoLazarus.ExternosAusentes(AInstalar: TList;
  AAusentes: TStrings; AFornecidos: TStrings);
var
  vConhecidos: TStringList;
  vNome: string;
  vInt, vExt: integer;
begin
  AAusentes.Clear;
  vConhecidos := TStringList.Create;
  try
    vConhecidos.CaseSensitive := False;
    vConhecidos.Sorted := True;
    vConhecidos.Duplicates := dupIgnore;
    PacotesConhecidos(vConhecidos);
    // o que as dependencias desta rodada vao instalar
    if AFornecidos <> nil then
      vConhecidos.AddStrings(AFornecidos);

    for vInt := 0 to Pred(AInstalar.Count) do
      for vExt := 0 to Pred(TPacote(AInstalar[vInt]).Externos.Count) do
      begin
        vNome := TPacote(AInstalar[vInt]).Externos[vExt];
        if (vConhecidos.IndexOf(vNome) < 0) and (AAusentes.IndexOf(vNome) < 0) then
          AAusentes.Add(vNome);
      end;
  finally
    vConhecidos.Free;
  end;
end;

function TInstalacaoLazarus.Plano: string;
var
  vLista, vInstalar: TList;
  vFora, vPlano, vExternos, vExigidas, vFornecidos: TStringList;
  vInt: integer;
  vPacote: TPacote;
  vReceita: TReceita;
  vRaiz, vOnde, vVersao, vMotivo: string;
  vNomes: TStringList;
begin
  vLista := TList.Create;
  vInstalar := TList.Create;
  vFora := TStringList.Create;
  vPlano := TStringList.Create;
  vExternos := TStringList.Create;
  vExigidas := TStringList.Create;
  vFornecidos := TStringList.Create;
  try
    FCatalogo.Fechamento(tpLazarus, FPacotes, vLista);
    Separar(vLista, vInstalar, vFora);

    vPlano.Add(FIDE.Nome + '  (' + ExcludeTrailingPathDelimiter(FIDE.RootDir) + ')');
    if FIDE.ConfigDir <> '' then
      vPlano.Add(Format(cmPlanoConfiguracao, [FIDE.ConfigDir]));
    vPlano.Add(cmPlanoPacotes);
    for vInt := 0 to Pred(vInstalar.Count) do
    begin
      vPacote := TPacote(vInstalar[vInt]);
      if vPacote.Instalavel and not vPacote.LazRuntimeOnly then
        vPlano.Add(Format(cmPlanoInstalarNaIDE, [vPacote.Nome]))
      else
        vPlano.Add(Format(cmPlanoSoRegistrar, [vPacote.Nome]));
    end;
    for vInt := 0 to Pred(vFora.Count) do
      vPlano.Add(Format(cmPlanoFicaDeForaLinha, [vFora[vInt]]));

    // dependencias de terceiros: o que ja esta, o que entra e o que falta
    vFornecidos.Clear;
    if FReceitas <> nil then
    begin
      vNomes := TStringList.Create;
      try
        for vInt := 0 to Pred(vInstalar.Count) do
          vNomes.Add(TPacote(vInstalar[vInt]).Nome);
        FReceitas.Exigidas(FCatalogo, tpLazarus, vNomes, vExigidas);
      finally
        vNomes.Free;
      end;
      for vInt := 0 to Pred(vExigidas.Count) do
      begin
        vReceita := TReceita(vExigidas.Objects[vInt]);
        vVersao := VersaoDependencia(vReceita, vMotivo);
        vRaiz := '';
        if vMotivo = '' then
          vRaiz := FPastasDependencias.Values[ChaveDependencia(vReceita.Nome, vVersao)];
        vOnde := '';
        if not (FIgnorarExistentes and (vRaiz <> '')) then
          vOnde := DependenciaInstalada(vReceita);
        if vOnde <> '' then
          vPlano.Add(Format(cmPlanoDepJaInstalada,
                            [vReceita.Nome, vExigidas[vInt], vOnde]))
        else if (vRaiz <> '') and not vReceita.Pago and
                (vReceita.Lazarus.Acoes.Count > 0) then
        begin
          vPlano.Add(Format(cmPlanoDepInstalar,
                            [vReceita.Nome, vVersao, vExigidas[vInt], vRaiz]));
          vFornecidos.AddStrings(vReceita.Lazarus.FornecePacotes);
        end
        else
          vPlano.Add(Format(cmPlanoDepFalta,
                            [vReceita.Nome, vExigidas[vInt], vExigidas[vInt]]));
      end;
    end;

    ExternosAusentes(vInstalar, vExternos, vFornecidos);
    if vExternos.Count > 0 then
      vPlano.Add(Format(cmPlanoExternosAusentes, [vExternos.CommaText]));
    if FConstruirIDE then
      vPlano.Add(cmPlanoReconstroi);
    Result := vPlano.Text;
  finally
    vFornecidos.Free;
    vExigidas.Free;
    vExternos.Free;
    vPlano.Free;
    vFora.Free;
    vInstalar.Free;
    vLista.Free;
  end;
end;

procedure TInstalacaoLazarus.Reverter;
var
  vLinks, vInstalados, vLinksDepois, vInstaladosDepois: TStringList;
  vErro: string;
begin
  if FSimular then
    Exit;
  vLinks := TStringList.Create;
  vInstalados := TStringList.Create;
  vLinksDepois := TStringList.Create;
  vInstaladosDepois := TStringList.Create;
  try
    LerLinks(FIDE.ConfigDir, vLinksDepois);
    LerInstalados(FIDE.ConfigDir, vInstaladosDepois);
    vLinks.Assign(vLinksDepois);
    vInstalados.Assign(vInstaladosDepois);
    DesfazerMudanca(vLinks, FLinksAntes, vLinksDepois, True);
    DesfazerMudanca(vInstalados, FInstaladosAntes, vInstaladosDepois, False);
    if GravarLinks(FIDE.ConfigDir, vLinks, vErro) and
       GravarInstalados(FIDE.ConfigDir, vInstalados, vErro) then
      Logar(cmConfiguracaoDevolvida)
    else
      Logar(Format(emDevolverConfiguracao, [vErro]));
  finally
    vInstaladosDepois.Free;
    vLinksDepois.Free;
    vInstalados.Free;
    vLinks.Free;
  end;
end;

function TInstalacaoLazarus.SalvarRecibo(AInstalar: TList;
  AReconstruiu: boolean): string;
var
  vRaiz, vIDE, vRAL, vLaz, vObj: TJSONObject;
  vLista: TJSONArray;
  vDepois: TStringList;
  vInt: integer;
  vArquivo: TStringList;
  vRepo, vVersao, vCommit: string;

begin
  vRaiz := TJSONObject.Create;
  vDepois := TStringList.Create;
  try
    vRaiz.Add('instalador', 'RALInstaller');
    vRaiz.Add('data', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', Now));
    vIDE := TJSONObject.Create;
    vIDE.Add('tipo', 'lazarus');
    vIDE.Add('nome', FIDE.Nome);
    vIDE.Add('versao', FIDE.Versao);
    vIDE.Add('fpc', FIDE.VersaoCompilador);
    vIDE.Add('raiz', FIDE.RootDir);
    vIDE.Add('config', FIDE.ConfigDir);
    vIDE.Add('lazbuild', FIDE.BuildFile);
    vRaiz.Add('ide', vIDE);

    vRaiz.Add('fontes', FRaizFontes);
    LerMarca(FRaizFontes, vRepo, vVersao, vCommit);
    vRAL := TJSONObject.Create;
    vRAL.Add('repositorio', vRepo);
    vRAL.Add('versao', vVersao);
    vRAL.Add('commit', vCommit);
    vRaiz.Add('ral', vRAL);

    vLista := TJSONArray.Create;
    for vInt := 0 to Pred(AInstalar.Count) do
      vLista.Add(TPacote(AInstalar[vInt]).Nome);
    vRaiz.Add('pacotes', vLista);

    // o que o instalador instalou e o que ele so encontrou: desinstalar nao
    // leva junto a dependencia que ja era do usuario
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
    vRaiz.Add('reconstruiu-ide', AReconstruiu);

    vLaz := TJSONObject.Create;
    vLaz.Add('links-antes', ObjetoDe(FLinksAntes));
    LerLinks(FIDE.ConfigDir, vDepois);
    vLaz.Add('links-depois', ObjetoDe(vDepois));
    vLaz.Add('instalados-antes', ListaDe(FInstaladosAntes));
    LerInstalados(FIDE.ConfigDir, vDepois);
    vLaz.Add('instalados-depois', ListaDe(vDepois));
    vRaiz.Add('lazarus', vLaz);

    ForceDirectories(FPastaRecibos);
    Result := IncludeTrailingPathDelimiter(FPastaRecibos) +
              Format('lazarus-%s-%s.json', [ReplaceStr(FIDE.Versao, '.', '_'),
                                           FormatDateTime('yyyymmdd-hhnnss', Now)]);
    vArquivo := TStringList.Create;
    try
      vArquivo.Text := vRaiz.FormatJSON;
      vArquivo.SaveToFile(Result);
    finally
      vArquivo.Free;
    end;
  finally
    vDepois.Free;
    vRaiz.Free;
  end;
end;

function TInstalacaoLazarus.Executar: boolean;
var
  vLista, vInstalar: TList;
  vFora, vLinks, vAdd, vDesconhecidos, vFornecidos: TStringList;
  vInt: integer;
  vPacote: TPacote;
begin
  Result := False;
  FRelatorio.Clear;
  FAvisos.Clear;

  if FRaizFontes = '' then
  begin
    Logar(emInstalarSemFontes);
    Exit;
  end;
  if not FileExists(FIDE.BuildFile) then
  begin
    Logar(Format(emLazbuildAusente, [FIDE.BuildFile]));
    Exit;
  end;
  // F10: com a IDE aberta, ela regrava a configuracao ao fechar e a
  // instalacao se perde
  if FExigirIDEFechada and not FSimular and
     ProgramaEmExecucao(ExecutaveisLazarus, FIDE.RootDir) then
  begin
    Logar(Format(emLazarusAberto, [FIDE.Nome]));
    Exit;
  end;
  FRecibo := '';
  FDepsRecibo.Clear;

  vLista := TList.Create;
  vInstalar := TList.Create;
  vFora := TStringList.Create;
  vLinks := TStringList.Create;
  vAdd := TStringList.Create;
  vDesconhecidos := TStringList.Create;
  vFornecidos := TStringList.Create;
  try
    FCatalogo.Fechamento(tpLazarus, FPacotes, vLista, vDesconhecidos);
    if vDesconhecidos.Count > 0 then
    begin
      Logar(Format(emPacotesDesconhecidos, [vDesconhecidos.CommaText]));
      Exit;
    end;

    Separar(vLista, vInstalar, vFora);

    // F7: os .lpk das dependencias vem antes dos do RAL, na mesma chamada do
    // lazbuild — continua havendo um --build-ide so
    PrepararDependencias(vInstalar, vLinks, vAdd, vFornecidos, vFora);

    for vInt := 0 to Pred(vFora.Count) do
    begin
      Logar(Format(cmFicaDeForaLinha, [vFora[vInt]]));
      FAvisos.Add(vFora[vInt]);
    end;
    ExternosAusentes(vInstalar, vDesconhecidos, vFornecidos);
    if vDesconhecidos.Count > 0 then
      FAvisos.Add(Format(wmExternosAusentes, [vDesconhecidos.CommaText]));
    if vInstalar.Count = 0 then
    begin
      Logar(cmNadaAInstalarLazarus);
      Exit;
    end;

    for vInt := 0 to Pred(vInstalar.Count) do
    begin
      vPacote := TPacote(vInstalar[vInt]);
      vLinks.Add(Arquivo(vPacote));
      if vPacote.Instalavel and not vPacote.LazRuntimeOnly then
        vAdd.Add(Arquivo(vPacote));
    end;

    Result := True;

    // F10: as duas listas da configuracao antes de mexer: e o que o recibo
    // guarda, e para onde a configuracao volta se o lazbuild falhar no meio
    LerLinks(FIDE.ConfigDir, FLinksAntes);
    LerInstalados(FIDE.ConfigDir, FInstaladosAntes);

    // 1. todos conhecidos pela IDE
    vDesconhecidos.Clear;
    ParametrosBase(vDesconhecidos);
    vDesconhecidos.Add('--add-package-link');
    vDesconhecidos.AddStrings(vLinks);
    if not Lazbuild(vDesconhecidos) then
    begin
      Logar(emLazbuildRegistrar);
      Reverter;
      Exit(False);
    end;

    // 2. os de design na lista de instalados
    if vAdd.Count > 0 then
    begin
      vDesconhecidos.Clear;
      ParametrosBase(vDesconhecidos);
      vDesconhecidos.Add('--add-package');
      vDesconhecidos.AddStrings(vAdd);
      if not Lazbuild(vDesconhecidos) then
      begin
        Logar(emLazbuildMarcar);
        Reverter;
        Exit(False);
      end;
    end;

    // 3. uma reconstrucao so. Falhou: o executavel da IDE continua o de
    // antes, e a configuracao volta ao que era — sem isso a IDE abriria
    // pedindo para reconstruir com os pacotes que nao compilam
    if FConstruirIDE and (vAdd.Count > 0) then
    begin
      vDesconhecidos.Clear;
      ParametrosBase(vDesconhecidos);
      vDesconhecidos.Add('--build-ide=');
      if not Lazbuild(vDesconhecidos) then
      begin
        Logar(emLazbuildReconstruir);
        Reverter;
        Result := False;
      end;
    end;

    // 4. recibo: so do que ficou na configuracao
    if Result and not FSimular then
      try
        FRecibo := SalvarRecibo(vInstalar, FConstruirIDE and (vAdd.Count > 0));
        Logar(Format(cmRecibo, [FRecibo]));
      except
        on E: Exception do
          FAvisos.Add(Format(emGravarRecibo, [E.Message]));
      end;

    for vInt := 0 to Pred(vInstalar.Count) do
      if not Result and (FRecibo = '') then
        FRelatorio.Add(Format('%-24s %s', [TPacote(vInstalar[vInt]).Nome,
          cmLazarusNaoInstalado]))
      else
        FRelatorio.Add(Format('%-24s %s', [TPacote(vInstalar[vInt]).Nome,
          IfThen(vAdd.IndexOf(Arquivo(TPacote(vInstalar[vInt]))) >= 0,
                 IfThen(Result and FConstruirIDE, cmLazarusInstalado,
                        cmLazarusMarcado),
                 cmLazarusRegistrado)]));
    for vInt := 0 to Pred(vFora.Count) do
      FRelatorio.Add(Format(cmForaLinha, [vFora[vInt]]));
    if not FConstruirIDE and (vAdd.Count > 0) then
      FAvisos.Add(wmIDENaoReconstruida);
    for vInt := 0 to Pred(FAvisos.Count) do
      FRelatorio.Add(cmPrefixoAvisoRelatorio + FAvisos[vInt]);
    if vFora.Count > 0 then
      Result := False;
  finally
    vFornecidos.Free;
    vDesconhecidos.Free;
    vAdd.Free;
    vLinks.Free;
    vFora.Free;
    vInstalar.Free;
    vLista.Free;
  end;
end;

function TInstalacaoLazarus.Existente: TInstalacaoExistente;
var
  vLista: TList;
  vNomes: TStringList;
  vInt: integer;
begin
  vNomes := TStringList.Create;
  vLista := TList.Create;
  try
    vNomes.CaseSensitive := False;
    if FCatalogo <> nil then
    begin
      FCatalogo.Listar(tpLazarus, vLista);
      for vInt := 0 to Pred(vLista.Count) do
        vNomes.Add(TPacote(vLista[vInt]).Nome);
    end;
    Result := DetectarLazarus(FIDE, vNomes);
  finally
    vLista.Free;
    vNomes.Free;
  end;
end;

function TInstalacaoLazarus.PlanoDesinstalar: string;
var
  vExistente: TInstalacaoExistente;
  vPlano, vNomes: TStringList;
  vRecibos: TRecibos;
  vLista: TList;
  vInt: integer;
begin
  vPlano := TStringList.Create;
  vNomes := TStringList.Create;
  vRecibos := TRecibos.Create(True);
  vLista := TList.Create;
  vExistente := Existente;
  try
    vPlano.Add(FIDE.Nome + '  (' + ExcludeTrailingPathDelimiter(FIDE.RootDir) + ')');
    if FIDE.ConfigDir <> '' then
      vPlano.Add(Format(cmPlanoConfiguracao, [FIDE.ConfigDir]));
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
    for vInt := 0 to Pred(vExistente.Pacotes.Count) do
      vNomes.Add(vExistente.Pacotes.Names[vInt]);
    if vNomes.Count > 0 then
      vPlano.Add(Format(cmTirarDaIDE, [StringReplace(vNomes.CommaText, ',', ', ',
                                                     [rfReplaceAll])]));
    vNomes.Clear;
    for vInt := 0 to Pred(vExistente.Links.Count) do
      vNomes.Add(vExistente.Links.Names[vInt]);
    if vNomes.Count > 0 then
      vPlano.Add(Format(cmTirarLinks, [StringReplace(vNomes.CommaText, ',', ', ',
                                                     [rfReplaceAll])]));
    if FConstruirIDE and (vExistente.Pacotes.Count > 0) then
      vPlano.Add(cmPlanoReconstroi);
    Result := vPlano.Text;
  finally
    vExistente.Free;
    vLista.Free;
    vRecibos.Free;
    vNomes.Free;
    vPlano.Free;
  end;
end;

function TInstalacaoLazarus.SalvarDesinstalacao: string;
var
  vRaiz, vIDE, vLaz: TJSONObject;
  vDepois, vArquivo: TStringList;
  vPasta: string;

begin
  vRaiz := TJSONObject.Create;
  vDepois := TStringList.Create;
  try
    vRaiz.Add('instalador', 'RALInstaller');
    vRaiz.Add('acao', 'desinstalacao');
    vRaiz.Add('data', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', Now));
    vIDE := TJSONObject.Create;
    vIDE.Add('tipo', 'lazarus');
    vIDE.Add('nome', FIDE.Nome);
    vIDE.Add('raiz', FIDE.RootDir);
    vIDE.Add('config', FIDE.ConfigDir);
    vRaiz.Add('ide', vIDE);
    // desfazer devolve pacotes a lista: a IDE pede para ser reconstruida
    vRaiz.Add('reconstruiu-ide', FConstruirIDE);
    vLaz := TJSONObject.Create;
    vLaz.Add('links-antes', ObjetoDe(FLinksAntes));
    LerLinks(FIDE.ConfigDir, vDepois);
    vLaz.Add('links-depois', ObjetoDe(vDepois));
    vLaz.Add('instalados-antes', ListaDe(FInstaladosAntes));
    LerInstalados(FIDE.ConfigDir, vDepois);
    vLaz.Add('instalados-depois', ListaDe(vDepois));
    vRaiz.Add('lazarus', vLaz);
    // fora da pasta dos recibos: nao e uma instalacao
    vPasta := IncludeTrailingPathDelimiter(FPastaRecibos) + 'desinstalacoes';
    ForceDirectories(vPasta);
    Result := IncludeTrailingPathDelimiter(vPasta) +
              Format('lazarus-%s-%s.json', [ReplaceStr(FIDE.Versao, '.', '_'),
                                           FormatDateTime('yyyymmdd-hhnnss', Now)]);
    vArquivo := TStringList.Create;
    try
      vArquivo.Text := vRaiz.FormatJSON;
      vArquivo.SaveToFile(Result);
    finally
      vArquivo.Free;
    end;
  finally
    vDepois.Free;
    vRaiz.Free;
  end;
end;

function TInstalacaoLazarus.Desinstalar: boolean;
var
  vExistente: TInstalacaoExistente;
  vRecibos: TRecibos;
  vLista: TList;
  vLinks, vInstalados, vParams: TStringList;
  vInt, vPos: integer;
  vErro: string;
  vMudouInstalados: boolean;
begin
  Result := True;
  FRelatorio.Clear;
  FAvisos.Clear;
  if FIDE.ConfigDir = '' then
    Exit;
  if FExigirIDEFechada and not FSimular and
     ProgramaEmExecucao(ExecutaveisLazarus, FIDE.RootDir) then
  begin
    Logar(Format(emLazarusAberto, [FIDE.Nome]));
    Exit(False);
  end;

  // 1. o que o instalador fez: os recibos (reconstroem a IDE se preciso)
  vRecibos := TRecibos.Create(True);
  vLista := TList.Create;
  try
    vRecibos.Carregar(FPastaRecibos);
    vRecibos.DaIDE(FIDE.RootDir, vLista);
    if (vLista.Count > 0) and not FSimular then
      if not DesinstalarIDE(FPastaRecibos, FIDE.RootDir, FLog, FConstruirIDE,
                            FExigirIDEFechada) then
        Result := False;
  finally
    vLista.Free;
    vRecibos.Free;
  end;

  // 2. o que sobrou, feito a mao (Install Packages, o OPM, uma copia do lpk)
  vExistente := Existente;
  vLinks := TStringList.Create;
  vInstalados := TStringList.Create;
  vParams := TStringList.Create;
  try
    if not vExistente.Existe then
    begin
      Logar(cmDesinstaladoSemResto);
      Exit;
    end;
    LerLinks(FIDE.ConfigDir, FLinksAntes);
    LerInstalados(FIDE.ConfigDir, FInstaladosAntes);
    vLinks.Assign(FLinksAntes);
    vInstalados.Assign(FInstaladosAntes);
    for vInt := 0 to Pred(vExistente.Links.Count) do
    begin
      vPos := vLinks.IndexOfName(vExistente.Links.Names[vInt]);
      if vPos >= 0 then
      begin
        Logar(Format(cmLinkRemovido, [vExistente.Links[vInt]]));
        vLinks.Delete(vPos);
      end;
    end;
    vMudouInstalados := False;
    for vInt := 0 to Pred(vExistente.Pacotes.Count) do
    begin
      vPos := vInstalados.IndexOf(vExistente.Pacotes.Names[vInt]);
      if vPos >= 0 then
      begin
        Logar(Format(cmPacoteRemovidoLazarus, [vInstalados[vPos]]));
        vInstalados.Delete(vPos);
        vMudouInstalados := True;
      end;
    end;
    if FSimular then
      Exit;
    if not (GravarLinks(FIDE.ConfigDir, vLinks, vErro) and
            GravarInstalados(FIDE.ConfigDir, vInstalados, vErro)) then
    begin
      Logar(Format(emDevolverConfiguracao, [vErro]));
      Reverter;
      Exit(False);
    end;

    // 3. sem os pacotes na lista, a IDE so se livra deles reconstruindo
    if vMudouInstalados and FConstruirIDE then
    begin
      ParametrosBase(vParams);
      vParams.Add('--build-ide=');
      if not Lazbuild(vParams) then
      begin
        Logar(emLazbuildReconstruir);
        Reverter;
        Exit(False);
      end;
    end
    else if vMudouInstalados then
      FAvisos.Add(wmIDENaoReconstruida);

    try
      Logar(Format(cmDesfazerDesinstalacao, [SalvarDesinstalacao]));
    except
      on E: Exception do
        FAvisos.Add(Format(emGravarRecibo, [E.Message]));
    end;
    for vInt := 0 to Pred(FAvisos.Count) do
      FRelatorio.Add(cmPrefixoAvisoRelatorio + FAvisos[vInt]);
  finally
    vParams.Free;
    vInstalados.Free;
    vLinks.Free;
    vExistente.Free;
  end;
end;

end.