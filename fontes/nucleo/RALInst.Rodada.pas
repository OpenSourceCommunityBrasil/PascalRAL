unit RALInst.Rodada;

{$mode ObjFPC}{$H+}

// F11: uma rodada inteira de instalacao, sem LCL — o que a GUI faz, para a CLI
// (e para servidor de build e sessao remota).
//
//   versao do RAL (GitHub ou pasta local) -> catalogo (do zip, sem gravar nada
//   na pasta do usuario) -> IDEs (Delphi e Lazarus juntos) -> pacotes -> plano
//   -> execucao em duas etapas: BAIXAR (fontes, submodulos, dependencias; se
//   falhar, nenhuma IDE e tocada) e INSTALAR (cada IDE com o seu motor).
//
// Os pacotes sao pedidos pelo nome, sem caixa: 'IndyRAL' vale para o
// IndyRAL.dpk no Delphi e para o indyral.lpk no Lazarus. Nome que so existe de
// um lado (NetHttpRAL, fphttpral) vale so para aquele.

interface

uses
  Classes, SysUtils, RALInst.Processo, RALInst.IDE, RALInst.Catalogo, RALInst.GitHub,
  RALInst.Receitas, RALInst.Compatibilidade;

type

  { TRodada }

  TRodada = class
  private
    FRepo: TRepoGitHub;
    FVersoes: TVersoesRAL;
    FVersao: TVersaoRAL;
    FPastaLocal: string;
    FPastaBase: string;
    FCatalogo: TCatalogo;
    FCatalogoInstalar: TCatalogo;
    FManifesto: TManifesto;
    FReceitas: TReceitas;
    FIDEs: TList;
    FPacotes: TStringList;
    FDependencias: TStringList;
    FDeteccoes: TStringList;
    FCompat: TCompatibilidade;
    FWin64: boolean;
    FSomenteLibraryPath: boolean;
    FIgnorarExistentes: boolean;
    FConstruirIDE: boolean;
    FPastaRecibos: string;
    FExigirIDEFechada: boolean;
    FLog: TLogLinha;
    FErro: string;
    FRelatorio: TStringList;
    procedure Logar(const ALinha: string);
    function TipoDe(AIDE: TIDEInstance): TTipoPacote;
    function Compat: TCompatibilidade;
    function Detectar(AIDE: TIDEInstance; AReceita: TReceita): string;
    function PastaInstalada(AIDE: TIDEInstance; AReceita: TReceita): string;
    function CatalogoDaExecucao: TCatalogo;
    procedure SubmodulosDaEscolha(ALista: TStrings);
    // uma IDE: o motor do tipo dela, configurado com a escolha da rodada
    function PlanoDaIDE(AIDE: TIDEInstance): string;
    function InstalarNaIDE(AIDE: TIDEInstance): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    // as versoes do RAL no GitHub (releases, tags, ramos)
    function ListarVersoes: boolean;
    // '' ou 'estavel' = a estavel mais recente; senao uma tag, release ou ramo
    function EscolherVersao(const ARef: string): boolean;
    // os fontes ja estao numa pasta (quem desenvolve o RAL): nada e baixado
    procedure UsarPastaLocal(const APasta: string);
    // o catalogo da versao escolhida: do zip (no cache) ou da pasta local
    function CarregarCatalogo: boolean;

    procedure AdicionarIDE(AIDE: TIDEInstance);
    // os pedidos que existem no tipo de IDE; sem pedido, o nucleo do RAL
    procedure PacotesDoTipo(ATipo: TTipoPacote; ALista: TStrings);
    // os pedidos que nao existem em tipo nenhum desta versao
    procedure PacotesDesconhecidos(ALista: TStrings);
    // <pasta>/PascalRAL/<versao>/, ou a pasta local
    function PastaFontes: string;

    // o que sera feito, sem fazer nada (decide as dependencias a baixar)
    function Plano: string;
    // baixar e instalar; False se algo falhou (o relatorio diz o que)
    function Executar: boolean;
    // desfaz as instalacoes registradas nas IDEs da rodada
    function Desinstalar(AReconstruirIDE: boolean): boolean;

    property Versoes: TVersoesRAL read FVersoes;
    property Versao: TVersaoRAL read FVersao;
    property PastaLocal: string read FPastaLocal;
    property PastaBase: string read FPastaBase write FPastaBase;
    property Catalogo: TCatalogo read FCatalogo;
    property Manifesto: TManifesto read FManifesto;
    property Receitas: TReceitas read FReceitas;
    property IDEs: TList read FIDEs;
    // nomes pedidos, sem caixa
    property Pacotes: TStringList read FPacotes;
    property Win64: boolean read FWin64 write FWin64;
    property SomenteLibraryPath: boolean read FSomenteLibraryPath write FSomenteLibraryPath;
    property IgnorarExistentes: boolean read FIgnorarExistentes write FIgnorarExistentes;
    // Lazarus: reconstruir a IDE no fim (padrao)
    property ConstruirIDE: boolean read FConstruirIDE write FConstruirIDE;
    // onde os recibos ficam (padrao: a pasta de dados do instalador)
    property PastaRecibos: string read FPastaRecibos write FPastaRecibos;
    // recusa mexer em IDE aberta (padrao); os testes, sobre copias, desligam
    property ExigirIDEFechada: boolean read FExigirIDEFechada write FExigirIDEFechada;
    property Repo: TRepoGitHub read FRepo;
    property Log: TLogLinha read FLog write FLog;
    property Erro: string read FErro;
    // uma linha por IDE: o que entrou e o que ficou de fora, e por que
    property Relatorio: TStringList read FRelatorio;
  end;

implementation

uses
  StrUtils, RALInst.Zip, RALInst.Fontes, RALInst.Dependencias, RALInst.Recibos,
  RALInst.Instalar.Lazarus
  {$IFDEF MSWINDOWS}, RALInst.Instalar.Delphi{$ENDIF};

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
    Logar('aviso: ' + FRepo.Aviso);
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
    FErro := 'versão desconhecida: ' + ARef;
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
      FErro := 'nenhum pacote do RAL em ' + FPastaLocal + ' (a pasta deve conter pkg e src)';
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
        FErro := 'a versão ' + FVersao.Ref + ' não tem pacotes do RAL';
        Exit;
      end;
    except
      on E: Exception do
      begin
        FErro := 'não foi possível ler a versão ' + FVersao.Ref + ': ' + E.Message;
        Exit;
      end;
    end;
  end
  else
  begin
    FErro := 'escolha a versão do RAL ou uma pasta local';
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
      Exit(AIDE.Nome + ': nenhum dos pacotes pedidos existe no ' + NomeTipoPacote(TipoDe(AIDE)) +
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
      Result := 'Fontes do RAL: ' + ExcludeTrailingPathDelimiter(FPastaLocal) +
                ' (pasta local, nada é baixado)' + LineEnding
    else if FVersao <> nil then
    begin
      SubmodulosDaEscolha(vSubs);
      Result := 'Baixar o PascalRAL ' + FVersao.Ref;
      if (FCatalogo.Origem is TOrigemZip) and (TOrigemZip(FCatalogo.Origem).Commit <> '') then
        Result := Result + ' (commit ' + Copy(TOrigemZip(FCatalogo.Origem).Commit, 1, 7) + ')';
      Result := Result + ' para ' + ExcludeTrailingPathDelimiter(PastaFontes) + LineEnding;
      if vSubs.Count > 0 then
        Result := Result + '  com os submódulos: ' + vSubs.CommaText + LineEnding;
    end;
    PacotesDesconhecidos(vDesconhecidos);
    if vDesconhecidos.Count > 0 then
      Result := Result + 'ATENÇÃO: não existem nesta versão do RAL: ' +
                vDesconhecidos.CommaText + LineEnding;

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
          Result := Result + Format('Baixar %s %s%s para %s (para %s)',
            [vReceita.Nome, vBaixa.Ref,
             IfThen(SameText(vBaixa.Fonte, vReceita.Github), '', ' de ' + vBaixa.Fonte),
             ExcludeTrailingPathDelimiter(vBaixa.PastaDestino), vPedidas.ValueFromIndex[vInt]]) +
            LineEnding;
        end
        else
          Result := Result + Format('ERRO: %s — sem ela, %s fica de fora',
                                    [vBaixa.Erro, vPedidas.ValueFromIndex[vInt]]) + LineEnding;
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
      FRelatorio.Add(Format('%s: %s', [AIDE.Nome, IfThen(AOk, 'ok', 'terminou com erro')]))
    else
      FRelatorio.Add(Format('%s: %s; %d aviso(s): %s', [AIDE.Nome,
        IfThen(AOk, 'ok', 'terminou com erro'), AAvisos.Count,
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
      Logar('nenhum dos pacotes pedidos existe no ' + NomeTipoPacote(TipoDe(AIDE)));
      FRelatorio.Add(AIDE.Nome + ': nada a instalar');
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
      Logar('o Delphi só existe no Windows');
      FRelatorio.Add(AIDE.Nome + ': o Delphi só existe no Windows');
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
    FErro := 'nenhuma IDE escolhida';
    Exit;
  end;

  // o plano decide o que baixar: sem ele, as dependencias ficam sem pasta
  if FDependencias.Count = 0 then
    Plano;

  // etapa 1: os fontes na pasta final; se falhar, nenhuma IDE e tocada
  if (FPastaLocal = '') and (FVersao <> nil) then
  begin
    Logar('== Baixando o PascalRAL ' + FVersao.Ref);
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
        Logar('ERRO: ' + FErro);
        Logar('Nenhuma IDE foi alterada.');
        Exit;
      end;
      FCatalogoInstalar := TCatalogo.Create;
      if not FCatalogoInstalar.Carregar(vPreparo.PastaDestino) then
      begin
        FErro := 'os fontes baixados em ' + vPreparo.PastaDestino + ' não têm pacotes';
        Logar('ERRO: ' + FErro);
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
    Logar('== Baixando ' + vReceita.Nome);
    vBaixa := TBaixaDependencia.Create(vReceita);
    try
      vBaixa.PastaBase := FPastaBase;
      vBaixa.VersaoPedida := Copy(vChave, Pos('@', vChave) + 1, MaxInt);
      vBaixa.Log := FLog;
      if not vBaixa.Executar then
      begin
        Logar('ERRO: ' + vBaixa.Erro);
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
    if DesinstalarIDE(FPastaRecibos, vIDE.RootDir, FLog, AReconstruirIDE, FExigirIDEFechada) then
      FRelatorio.Add(vIDE.Nome + ': desinstalado')
    else
    begin
      FRelatorio.Add(vIDE.Nome + ': a desinstalação terminou com erro');
      Result := False;
    end;
    Logar('');
  end;
end;

end.
