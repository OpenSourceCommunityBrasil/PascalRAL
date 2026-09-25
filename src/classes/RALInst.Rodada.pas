/// A whole installation run, LCL-free: what the GUI does, for the CLI (and for
/// build servers and remote sessions).
///   RAL version (GitHub or local folder) -> catalog (from the zip, writing
///   nothing into the user's folder) -> IDEs (Delphi and Lazarus together) ->
///   packages -> plan -> run in two stages: DOWNLOAD (sources, submodules,
///   dependencies; when it fails, no IDE is touched) and INSTALL (each IDE with
///   its own engine).
/// Packages are asked by name, case-insensitive: 'IndyRAL' holds for
/// IndyRAL.dpk in Delphi and for indyral.lpk in Lazarus. A name that only
/// exists on one side (NetHttpRAL, fphttpral) holds only for that side.
unit RALInst.Rodada;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALInst.Catalogo, RALInst.Compatibilidade, RALInst.GitHub, RALInst.IDE,
  RALInst.Processo, RALInst.Receitas;

type
  /// One installation run over several IDEs.
  TRodada = class
  private
    FCatalogo: TCatalogo;
    FCatalogoInstalar: TCatalogo;
    FCompat: TCompatibilidade;
    FConstruirIDE: boolean;
    FDependencias: TStringList;
    FDeteccoes: TStringList;
    FErro: string;
    FExigirIDEFechada: boolean;
    FIDEs: TList;
    FIgnorarExistentes: boolean;
    FLog: TLogLinha;
    FManifesto: TManifesto;
    FPacotes: TStringList;
    FPastaBase: string;
    FPastaLocal: string;
    FPastaRecibos: string;
    FReceitas: TReceitas;
    FRelatorio: TStringList;
    FRepo: TRepoGitHub;
    FSomenteLibraryPath: boolean;
    FVersao: TVersaoRAL;
    FVersoes: TVersoesRAL;
    FWin64: boolean;
    /// The catalog of the downloaded folder when there is one, else the zip's
    function CatalogoDaExecucao: TCatalogo;
    /// The compatibility check, created on first use
    function Compat: TCompatibilidade;
    /// Where a dependency already is in the IDE (cached)
    function Detectar(AIDE: TIDEInstance; AReceita: TReceita): string;
    /// One IDE: its kind's engine, configured with the run's choice
    function InstalarNaIDE(AIDE: TIDEInstance): boolean;
    /// Passes the line to Log when assigned
    procedure Logar(const ALinha: string);
    /// Root of the installed copy of a dependency (Lazarus only)
    function PastaInstalada(AIDE: TIDEInstance; AReceita: TReceita): string;
    /// The plan of one IDE
    function PlanoDaIDE(AIDE: TIDEInstance): string;
    /// Submodules the chosen packages use, of every kind of IDE
    procedure SubmodulosDaEscolha(ALista: TStrings);
    /// Package kind of an IDE
    function TipoDe(AIDE: TIDEInstance): TTipoPacote;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AdicionarIDE(AIDE: TIDEInstance);
    /// The catalog of the chosen version: from the zip (cached) or the local
    /// folder
    function CarregarCatalogo: boolean;
    /// Undoes the installations recorded in the run's IDEs
    function Desinstalar(AReconstruirIDE: boolean): boolean;
    /// '' or 'estavel' = the newest stable one; else a tag, release or branch
    function EscolherVersao(const ARef: string): boolean;
    /// Download and install; False when something failed (the report says
    /// what)
    function Executar: boolean;
    /// RAL versions on GitHub (releases, tags, branches)
    function ListarVersoes: boolean;
    /// The asked names that exist in no kind of this version
    procedure PacotesDesconhecidos(ALista: TStrings);
    /// The asked names that exist in the IDE kind; with none asked, RAL's core
    procedure PacotesDoTipo(ATipo: TTipoPacote; ALista: TStrings);
    /// <folder>/PascalRAL/<version>/, or the local folder
    function PastaFontes: string;
    /// What will be done, without doing it (decides the dependencies to
    /// download)
    function Plano: string;
    /// The sources are already in a folder (who develops RAL): nothing is
    /// downloaded
    procedure UsarPastaLocal(const APasta: string);

    property Catalogo: TCatalogo read FCatalogo;
    /// Lazarus: rebuild the IDE at the end (default)
    property ConstruirIDE: boolean read FConstruirIDE write FConstruirIDE;
    property Erro: string read FErro;
    /// Refuses to touch an open IDE (default); the tests, on copies, turn it off
    property ExigirIDEFechada: boolean read FExigirIDEFechada write FExigirIDEFechada;
    property IDEs: TList read FIDEs;
    property IgnorarExistentes: boolean read FIgnorarExistentes write FIgnorarExistentes;
    property Log: TLogLinha read FLog write FLog;
    property Manifesto: TManifesto read FManifesto;
    /// Asked names, case-insensitive
    property Pacotes: TStringList read FPacotes;
    property PastaBase: string read FPastaBase write FPastaBase;
    property PastaLocal: string read FPastaLocal;
    /// Where the receipts live (default: the installer's data folder)
    property PastaRecibos: string read FPastaRecibos write FPastaRecibos;
    property Receitas: TReceitas read FReceitas;
    /// One line per IDE: what entered and what stayed out, and why
    property Relatorio: TStringList read FRelatorio;
    property Repo: TRepoGitHub read FRepo;
    property SomenteLibraryPath: boolean read FSomenteLibraryPath
      write FSomenteLibraryPath;
    property Versao: TVersaoRAL read FVersao;
    property Versoes: TVersoesRAL read FVersoes;
    property Win64: boolean read FWin64 write FWin64;
  end;

implementation

uses
  StrUtils,
  {$IFDEF MSWINDOWS} RALInst.Instalar.Delphi, {$ENDIF}
  RALInst.Dependencias, RALInst.Fontes, RALInst.Instalar.Lazarus, RALInst.Mensagens,
  RALInst.Recibos, RALInst.Zip;

{ TRodada }

constructor TRodada.Create;
begin
  inherited Create;
  FRepo := TRepoGitHub.Create(DonoRAL, RepoRAL);
  FVersoes := TVersoesRAL.Create(True);
  FCatalogo := TCatalogo.Create;
  FManifesto := TManifesto.Create;
  FReceitas := TReceitas.Create;
  FReceitas.CarregarPadrao;
  FIDEs := TList.Create;
  FPacotes := TStringList.Create;
  FPacotes.CaseSensitive := False;
  FDependencias := TStringList.Create;
  FDependencias.CaseSensitive := False;
  FDeteccoes := TStringList.Create;
  FRelatorio := TStringList.Create;
  FConstruirIDE := True;
  FPastaRecibos := PastaDadosInstalador + 'recibos';
  FExigirIDEFechada := True;
  FPastaBase := IncludeTrailingPathDelimiter(GetUserDir) + 'RAL';
end;

destructor TRodada.Destroy;
begin
  FCompat.Free;
  FRelatorio.Free;
  FDeteccoes.Free;
  FDependencias.Free;
  FPacotes.Free;
  FIDEs.Free;
  FReceitas.Free;
  FManifesto.Free;
  FCatalogoInstalar.Free;
  FCatalogo.Free;
  FVersoes.Free;
  FRepo.Free;
  inherited Destroy;
end;

procedure TRodada.Logar(const ALinha: string);
begin
  if Assigned(FLog) then
    FLog(ALinha);
end;

function TRodada.TipoDe(AIDE: TIDEInstance): TTipoPacote;
begin
  if AIDE.Tipo = tiDelphi then
    Result := tpDelphi
  else
    Result := tpLazarus;
end;

function TRodada.Compat: TCompatibilidade;
begin
  if FCompat = nil then
  begin
    FCompat := TCompatibilidade.Create(FCatalogo, FManifesto, FReceitas);
    FCompat.Detectar := @Detectar;
    FCompat.PastaInstalada := @PastaInstalada;
  end;
  Result := FCompat;
end;

function TRodada.Detectar(AIDE: TIDEInstance; AReceita: TReceita): string;
var
  vChave: string;
  vIdx: integer;
  vLaz: TInstalacaoLazarus;
  {$IFDEF MSWINDOWS}
  vDelphi: TInstalacaoDelphi;
  {$ENDIF}
begin
  vChave := AIDE.RootDir + '|' + AReceita.Nome;
  vIdx := FDeteccoes.IndexOfName(vChave);
  if vIdx >= 0 then
    Exit(FDeteccoes.ValueFromIndex[vIdx]);
  Result := '';
  if AIDE.Tipo = tiLazarus then
  begin
    vLaz := TInstalacaoLazarus.Create(AIDE, FCatalogo);
    try
      Result := vLaz.DependenciaInstalada(AReceita);
    finally
      vLaz.Free;
    end;
  end
  {$IFDEF MSWINDOWS}
  else
  begin
    vDelphi := TInstalacaoDelphi.Create(AIDE, FCatalogo);
    try
      Result := vDelphi.DependenciaInstalada(AReceita);
    finally
      vDelphi.Free;
    end;
  end
  {$ENDIF};
  FDeteccoes.Add(vChave + '=' + Result);
end;

function TRodada.PastaInstalada(AIDE: TIDEInstance; AReceita: TReceita): string;
var
  vLaz: TInstalacaoLazarus;
begin
  Result := '';
  if AIDE.Tipo <> tiLazarus then
    Exit;
  vLaz := TInstalacaoLazarus.Create(AIDE, FCatalogo);
  try
    Result := vLaz.PastaDependencia(AReceita);
  finally
    vLaz.Free;
  end;
end;

function TRodada.ListarVersoes: boolean;
begin
  Result := FRepo.ListarVersoes(FVersoes);
  if not Result then
    FErro := FRepo.Erro
  else if FRepo.Aviso <> '' then
    Logar(Trim(cmPrefixoAviso) + ' ' + FRepo.Aviso);
end;

function TRodada.EscolherVersao(const ARef: string): boolean;
begin
  Result := False;
  FErro := '';
  FPastaLocal := '';
  if (FVersoes.Count = 0) and not ListarVersoes then
    Exit;
  if (ARef = '') or SameText(ARef, 'estavel') then
    FVersao := FVersoes.Recomendada
  else
    FVersao := FVersoes.Buscar(ARef);
  if FVersao = nil then
  begin
    FErro := Format(emVersaoDesconhecida, [ARef]);
    Exit;
  end;
  Result := True;
end;

procedure TRodada.UsarPastaLocal(const APasta: string);
begin
  FVersao := nil;
  FPastaLocal := IncludeTrailingPathDelimiter(ExpandFileName(APasta));
end;

function TRodada.CarregarCatalogo: boolean;
var
  vZip: string;
begin
  Result := False;
  FErro := '';
  FreeAndNil(FCompat);
  FreeAndNil(FCatalogoInstalar);
  FDeteccoes.Clear;
  FCatalogo.Limpar;
  FManifesto.Limpar;
  if FPastaLocal <> '' then
  begin
    if not FCatalogo.Carregar(FPastaLocal) then
    begin
      FErro := Format(emSemPacotesNaPasta, [FPastaLocal]);
      Exit;
    end;
  end
  else if FVersao <> nil then
  begin
    // o zip da versao vai para o cache: nada na pasta do usuario ainda
    if not FRepo.BaixarZip(FVersao.Ref, FVersao.Tipo = tvRamo, vZip) then
    begin
      FErro := FRepo.Erro;
      Exit;
    end;
    try
      if not FCatalogo.Carregar(TOrigemZip.Create(vZip, 'PascalRAL ' + FVersao.Ref)) then
      begin
        FErro := Format(emVersaoSemPacotes, [FVersao.Ref]);
        Exit;
      end;
    except
      on E: Exception do
      begin
        FErro := Format(emLerVersao, [FVersao.Ref, E.Message]);
        Exit;
      end;
    end;
  end
  else
  begin
    FErro := emEscolhaVersaoOuPasta;
    Exit;
  end;
  FManifesto.CarregarPadrao(FCatalogo.Origem);
  Result := True;
end;

procedure TRodada.AdicionarIDE(AIDE: TIDEInstance);
begin
  if FIDEs.IndexOf(AIDE) < 0 then
    FIDEs.Add(AIDE);
end;

procedure TRodada.PacotesDoTipo(ATipo: TTipoPacote; ALista: TStrings);
var
  vLista: TList;
  vInt: integer;
  vPacote: TPacote;
begin
  ALista.Clear;
  if FPacotes.Count > 0 then
  begin
    for vInt := 0 to Pred(FPacotes.Count) do
    begin
      vPacote := FCatalogo.Buscar(ATipo, FPacotes[vInt]);
      if vPacote <> nil then
        ALista.Add(vPacote.Nome);
    end;
    Exit;
  end;
  // sem pedido: o nucleo (os da raiz de pkg/<IDE>), menos o assistente
  vLista := TList.Create;
  try
    FCatalogo.Listar(ATipo, vLista);
    for vInt := 0 to Pred(vLista.Count) do
    begin
      vPacote := TPacote(vLista[vInt]);
      if (vPacote.Grupo = '') and (Pos('wizard', LowerCase(vPacote.Nome)) = 0) then
        ALista.Add(vPacote.Nome);
    end;
  finally
    vLista.Free;
  end;
end;

procedure TRodada.PacotesDesconhecidos(ALista: TStrings);
var
  vInt: integer;
begin
  ALista.Clear;
  for vInt := 0 to Pred(FPacotes.Count) do
    if (FCatalogo.Buscar(tpDelphi, FPacotes[vInt]) = nil) and
       (FCatalogo.Buscar(tpLazarus, FPacotes[vInt]) = nil) then
      ALista.Add(FPacotes[vInt]);
end;

function TRodada.PastaFontes: string;
begin
  if FPastaLocal <> '' then
    Result := FPastaLocal
  else if (FVersao <> nil) and (FPastaBase <> '') then
    Result := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPastaBase) +
                                           'PascalRAL' + PathDelim + FVersao.NomePasta)
  else
    Result := '';
end;

function TRodada.CatalogoDaExecucao: TCatalogo;
begin
  if FCatalogoInstalar <> nil then
    Result := FCatalogoInstalar
  else
    Result := FCatalogo;
end;

procedure TRodada.SubmodulosDaEscolha(ALista: TStrings);
var
  vFechamento: TList;
  vNomes: TStringList;
  vTipo: TTipoPacote;
  vInt, vSub: integer;
begin
  ALista.Clear;
  vFechamento := TList.Create;
  vNomes := TStringList.Create;
  try
    for vTipo := Low(TTipoPacote) to High(TTipoPacote) do
    begin
      PacotesDoTipo(vTipo, vNomes);
      FCatalogo.Fechamento(vTipo, vNomes, vFechamento);
      for vInt := 0 to Pred(vFechamento.Count) do
        for vSub := 0 to Pred(TPacote(vFechamento[vInt]).Submodulos.Count) do
          if ALista.IndexOf(TPacote(vFechamento[vInt]).Submodulos[vSub]) < 0 then
            ALista.Add(TPacote(vFechamento[vInt]).Submodulos[vSub]);
    end;
  finally
    vNomes.Free;
    vFechamento.Free;
  end;
end;

function TRodada.PlanoDaIDE(AIDE: TIDEInstance): string;
var
  vNomes: TStringList;
  vLaz: TInstalacaoLazarus;
  {$IFDEF MSWINDOWS}
  vDelphi: TInstalacaoDelphi;
  {$ENDIF}
begin
  Result := '';
  vNomes := TStringList.Create;
  try
    PacotesDoTipo(TipoDe(AIDE), vNomes);
    if vNomes.Count = 0 then
      Exit(Format(cmNenhumPedidoExiste, [AIDE.Nome, NomeTipoPacote(TipoDe(AIDE))]) +
           LineEnding);
    if AIDE.Tipo = tiLazarus then
    begin
      vLaz := TInstalacaoLazarus.Create(AIDE, CatalogoDaExecucao);
      try
        vLaz.Pacotes.Assign(vNomes);
        vLaz.RaizFontes := PastaFontes;
        vLaz.Receitas := FReceitas;
        vLaz.Manifesto := FManifesto;
        vLaz.PastasDependencias.Assign(FDependencias);
        vLaz.IgnorarExistentes := FIgnorarExistentes;
        vLaz.ConstruirIDE := FConstruirIDE;
        Result := vLaz.Plano;
      finally
        vLaz.Free;
      end;
    end
    {$IFDEF MSWINDOWS}
    else
    begin
      vDelphi := TInstalacaoDelphi.Create(AIDE, CatalogoDaExecucao);
      try
        vDelphi.Pacotes.Assign(vNomes);
        vDelphi.RaizFontes := PastaFontes;
        vDelphi.Receitas := FReceitas;
        vDelphi.Manifesto := FManifesto;
        vDelphi.PastasDependencias.Assign(FDependencias);
        vDelphi.IgnorarExistentes := FIgnorarExistentes;
        vDelphi.SomenteLibraryPath := FSomenteLibraryPath;
        if FWin64 and (AIDE.Plataformas.IndexOf('win64') >= 0) then
          vDelphi.Plataformas.Add('win64');
        Result := vDelphi.Plano;
      finally
        vDelphi.Free;
      end;
    end
    {$ENDIF};
  finally
    vNomes.Free;
  end;
end;

function TRodada.Plano: string;
var
  vSubs, vDesconhecidos, vNomes, vExigidas, vFora, vPedidas: TStringList;
  vLista: TList;
  vInt, vIDE, vPac: integer;
  vAIDE: TIDEInstance;
  vReceita: TReceita;
  vBloco: TBlocoIDE;
  vVersao, vMotivo, vChave: string;
  vBaixa: TBaixaDependencia;
begin
  Result := '';
  FDependencias.Clear;
  vSubs := TStringList.Create;
  vDesconhecidos := TStringList.Create;
  vNomes := TStringList.Create;
  vExigidas := TStringList.Create;
  vFora := TStringList.Create;
  vPedidas := TStringList.Create;
  vLista := TList.Create;
  try
    // 1. de onde vem o RAL
    if FPastaLocal <> '' then
      Result := Format(cmPlanoFontesLocal, [ExcludeTrailingPathDelimiter(FPastaLocal)]) +
                LineEnding
    else if FVersao <> nil then
    begin
      SubmodulosDaEscolha(vSubs);
      Result := Format(cmPlanoBaixarRAL, [FVersao.Ref]);
      if (FCatalogo.Origem is TOrigemZip) and
         (TOrigemZip(FCatalogo.Origem).Commit <> '') then
        Result := Result + Format(cmPlanoCommit,
                                  [Copy(TOrigemZip(FCatalogo.Origem).Commit, 1, 7)]);
      Result := Result +
                Format(cmPlanoPara, [ExcludeTrailingPathDelimiter(PastaFontes)]) +
                LineEnding;
      if vSubs.Count > 0 then
        Result := Result + Format(cmPlanoSubmodulos, [vSubs.CommaText]) + LineEnding;
    end;
    PacotesDesconhecidos(vDesconhecidos);
    if vDesconhecidos.Count > 0 then
      Result := Result + Format(cmPlanoNaoExistem, [vDesconhecidos.CommaText]) +
                LineEnding;

    // 2. as dependencias: cada IDE pede o que nao tem, na versao que aceita
    for vIDE := 0 to Pred(FIDEs.Count) do
    begin
      vAIDE := TIDEInstance(FIDEs[vIDE]);
      PacotesDoTipo(TipoDe(vAIDE), vNomes);
      FCatalogo.Fechamento(TipoDe(vAIDE), vNomes, vLista);
      vFora.Clear;
      Compat.Filtrar(vLista, vAIDE, vFora);
      vNomes.Clear;
      for vPac := 0 to Pred(vLista.Count) do
        vNomes.Add(TPacote(vLista[vPac]).Nome);
      FReceitas.Exigidas(FCatalogo, TipoDe(vAIDE), vNomes, vExigidas);
      for vInt := 0 to Pred(vExigidas.Count) do
      begin
        vReceita := TReceita(vExigidas.Objects[vInt]);
        vBloco := vReceita.Bloco(TipoDe(vAIDE));
        if not vReceita.PodeBaixar or not vBloco.Existe or (vBloco.Acoes.Count = 0) then
          Continue;
        if not FIgnorarExistentes and (Detectar(vAIDE, vReceita) <> '') then
          Continue;
        vVersao := Compat.VersaoDependencia(vReceita, vAIDE, vMotivo);
        if vMotivo <> '' then
          Continue;
        vChave := ChaveDependencia(vReceita.Nome, vVersao);
        if vPedidas.IndexOfName(vChave) < 0 then
          vPedidas.AddObject(vChave + '=' + vExigidas[vInt], vReceita);
      end;
    end;
    for vInt := 0 to Pred(vPedidas.Count) do
    begin
      vReceita := TReceita(vPedidas.Objects[vInt]);
      vChave := vPedidas.Names[vInt];
      vBaixa := TBaixaDependencia.Create(vReceita);
      try
        vBaixa.PastaBase := FPastaBase;
        vBaixa.VersaoPedida := Copy(vChave, Pos('@', vChave) + 1, MaxInt);
        if vBaixa.ResolverVersao then
        begin
          FDependencias.Values[vChave] := vBaixa.PastaDestino;
          Result := Result + Format(cmPlanoBaixarDep,
            [vReceita.Nome, vBaixa.Ref,
             IfThen(SameText(vBaixa.Fonte, vReceita.Github), '',
                    Format(cmPlanoDe, [vBaixa.Fonte])),
             ExcludeTrailingPathDelimiter(vBaixa.PastaDestino),
             vPedidas.ValueFromIndex[vInt]]) + LineEnding;
        end
        else
          Result := Result + Format(cmPlanoErroDep,
                                    [vBaixa.Erro, vPedidas.ValueFromIndex[vInt]]) +
                  LineEnding;
      finally
        vBaixa.Free;
      end;
    end;

    // 3. cada IDE
    Result := Result + LineEnding;
    for vIDE := 0 to Pred(FIDEs.Count) do
      Result := Result + PlanoDaIDE(TIDEInstance(FIDEs[vIDE])) + LineEnding;
  finally
    vLista.Free;
    vPedidas.Free;
    vFora.Free;
    vExigidas.Free;
    vNomes.Free;
    vDesconhecidos.Free;
    vSubs.Free;
  end;
end;

function TRodada.InstalarNaIDE(AIDE: TIDEInstance): boolean;
var
  vNomes: TStringList;
  vLaz: TInstalacaoLazarus;
  {$IFDEF MSWINDOWS}
  vDelphi: TInstalacaoDelphi;
  {$ENDIF}

  procedure Relatar(ARelatorio, AAvisos: TStrings; AOk: boolean);
  var
    vI: integer;
  begin
    Logar('');
    for vI := 0 to Pred(ARelatorio.Count) do
      Logar(ARelatorio[vI]);
    if AAvisos.Count = 0 then
      FRelatorio.Add(Format('%s: %s',
                            [AIDE.Nome, IfThen(AOk, cmEstadoOk, cmTerminouComErro)]))
    else
      FRelatorio.Add(Format(cmRelatorioAvisos, [AIDE.Nome,
        IfThen(AOk, cmEstadoOk, cmTerminouComErro), AAvisos.Count,
        StringReplace(Trim(AAvisos.Text), LineEnding, ' | ', [rfReplaceAll])]));
  end;

begin
  Result := False;
  Logar('==== ' + AIDE.Nome + ' (' + ExcludeTrailingPathDelimiter(AIDE.RootDir) + ')');
  vNomes := TStringList.Create;
  try
    PacotesDoTipo(TipoDe(AIDE), vNomes);
    if vNomes.Count = 0 then
    begin
      Logar(Format(cmNenhumPedidoExiste, [AIDE.Nome, NomeTipoPacote(TipoDe(AIDE))]));
      FRelatorio.Add(Format(cmNadaAInstalar, [AIDE.Nome]));
      Exit(True);
    end;
    if AIDE.Tipo = tiLazarus then
    begin
      vLaz := TInstalacaoLazarus.Create(AIDE, CatalogoDaExecucao);
      try
        vLaz.Pacotes.Assign(vNomes);
        vLaz.RaizFontes := PastaFontes;
        vLaz.Receitas := FReceitas;
        vLaz.Manifesto := FManifesto;
        vLaz.PastasDependencias.Assign(FDependencias);
        vLaz.IgnorarExistentes := FIgnorarExistentes;
        vLaz.ConstruirIDE := FConstruirIDE;
        vLaz.PastaRecibos := FPastaRecibos;
        vLaz.ExigirIDEFechada := FExigirIDEFechada;
        vLaz.Log := FLog;
        Result := vLaz.Executar;
        Relatar(vLaz.Relatorio, vLaz.Avisos, Result);
      finally
        vLaz.Free;
      end;
    end
    {$IFDEF MSWINDOWS}
    else
    begin
      vDelphi := TInstalacaoDelphi.Create(AIDE, CatalogoDaExecucao);
      try
        vDelphi.Pacotes.Assign(vNomes);
        vDelphi.RaizFontes := PastaFontes;
        vDelphi.Receitas := FReceitas;
        vDelphi.Manifesto := FManifesto;
        vDelphi.PastasDependencias.Assign(FDependencias);
        vDelphi.IgnorarExistentes := FIgnorarExistentes;
        vDelphi.SomenteLibraryPath := FSomenteLibraryPath;
        if FWin64 and (AIDE.Plataformas.IndexOf('win64') >= 0) then
          vDelphi.Plataformas.Add('win64');
        vDelphi.PastaRecibos := FPastaRecibos;
        vDelphi.ExigirIDEFechada := FExigirIDEFechada;
        vDelphi.Log := FLog;
        Result := vDelphi.Executar;
        Relatar(vDelphi.Relatorio, vDelphi.Avisos, Result);
      finally
        vDelphi.Free;
      end;
    end
    {$ELSE}
    else
    begin
      Logar(cmDelphiSoWindows);
      FRelatorio.Add(AIDE.Nome + ': ' + cmDelphiSoWindows);
    end
    {$ENDIF};
  finally
    vNomes.Free;
  end;
  Logar('');
end;

function TRodada.Executar: boolean;
var
  vPreparo: TPreparoFontes;
  vInt: integer;
  vReceita: TReceita;
  vBaixa: TBaixaDependencia;
  vChave: string;
begin
  Result := False;
  FErro := '';
  FRelatorio.Clear;
  if FIDEs.Count = 0 then
  begin
    FErro := emNenhumaIDE;
    Exit;
  end;

  // o plano decide o que baixar: sem ele, as dependencias ficam sem pasta
  if FDependencias.Count = 0 then
    Plano;

  // etapa 1: os fontes na pasta final; se falhar, nenhuma IDE e tocada
  if (FPastaLocal = '') and (FVersao <> nil) then
  begin
    Logar(Format(cmBaixandoRAL, [FVersao.Ref]));
    FreeAndNil(FCatalogoInstalar);
    FRepo.Log := FLog;
    vPreparo := TPreparoFontes.Create(FRepo, FVersao.Ref, FVersao.Tipo = tvRamo);
    try
      vPreparo.PastaBase := FPastaBase;
      vPreparo.Log := FLog;
      SubmodulosDaEscolha(vPreparo.Submodulos);
      if not vPreparo.Executar then
      begin
        FErro := vPreparo.Erro;
        Logar(cmPrefixoErro + FErro);
        Logar(cmNenhumaIDEAlterada);
        Exit;
      end;
      FCatalogoInstalar := TCatalogo.Create;
      if not FCatalogoInstalar.Carregar(vPreparo.PastaDestino) then
      begin
        FErro := Format(emBaixadosSemPacotes, [vPreparo.PastaDestino]);
        Logar(cmPrefixoErro + FErro);
        FreeAndNil(FCatalogoInstalar);
        Exit;
      end;
    finally
      FRepo.Log := nil;
      vPreparo.Free;
    end;
  end;

  // etapa 1b: as dependencias; a que falhar tira da lista, e os pacotes que
  // precisavam dela ficam de fora na instalacao
  Result := True;
  for vInt := Pred(FDependencias.Count) downto 0 do
  begin
    vChave := FDependencias.Names[vInt];
    vReceita := FReceitas.Buscar(Copy(vChave, 1, Pos('@', vChave) - 1));
    if vReceita = nil then
      Continue;
    Logar(Format(cmBaixandoDep, [vReceita.Nome]));
    vBaixa := TBaixaDependencia.Create(vReceita);
    try
      vBaixa.PastaBase := FPastaBase;
      vBaixa.VersaoPedida := Copy(vChave, Pos('@', vChave) + 1, MaxInt);
      vBaixa.Log := FLog;
      if not vBaixa.Executar then
      begin
        Logar(cmPrefixoErro + vBaixa.Erro);
        FDependencias.Delete(vInt);
        Result := False;
      end;
    finally
      vBaixa.Free;
    end;
  end;
  Logar('');

  // etapa 2: cada IDE; uma que falha nao impede as outras
  for vInt := 0 to Pred(FIDEs.Count) do
    if not InstalarNaIDE(TIDEInstance(FIDEs[vInt])) then
      Result := False;
end;

function TRodada.Desinstalar(AReconstruirIDE: boolean): boolean;
var
  vInt: integer;
  vIDE: TIDEInstance;
begin
  Result := True;
  FRelatorio.Clear;
  for vInt := 0 to Pred(FIDEs.Count) do
  begin
    vIDE := TIDEInstance(FIDEs[vInt]);
    Logar('==== ' + vIDE.Nome + ' (' + ExcludeTrailingPathDelimiter(vIDE.RootDir) + ')');
    if DesinstalarIDE(FPastaRecibos, vIDE.RootDir, FLog, AReconstruirIDE,
                      FExigirIDEFechada) then
      FRelatorio.Add(Format(cmDesinstalado, [vIDE.Nome]))
    else
    begin
      FRelatorio.Add(Format(cmDesinstalacaoComErro, [vIDE.Nome]));
      Result := False;
    end;
    Logar('');
  end;
end;

end.
