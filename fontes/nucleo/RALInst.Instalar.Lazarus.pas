unit RALInst.Instalar.Lazarus;

{$mode ObjFPC}{$H+}

// Instalacao numa IDE Lazarus (F5): lazbuild com a configuracao daquela
// instalacao, na ordem do grafo do catalogo (F2), e um unico --build-ide no
// fim.
//
//   1. --add-package-link com todos os .lpk da rodada: o Lazarus passa a
//      conhecer cada pacote, inclusive os que so existem como dependencia;
//   2. --add-package com os de design (ou runtime+design): entram na lista
//      de pacotes instalados da IDE;
//   3. --build-ide=, uma vez: compila tudo o que a IDE passou a exigir.

interface

uses
  Classes, SysUtils, RALInst.IDE, RALInst.Catalogo, RALInst.Processo, RALInst.Receitas,
  RALInst.Compatibilidade;

type

  { TInstalacaoLazarus }

  TInstalacaoLazarus = class
  private
    FIDE: TIDEInstance;
    FCatalogo: TCatalogo;
    FRaizFontes: string;
    FPacotes: TStringList;
    FConstruirIDE: boolean;
    FLog: TLogLinha;
    FSimular: boolean;
    FRelatorio: TStringList;
    FAvisos: TStringList;
    FReceitas: TReceitas;
    FPastasDependencias: TStringList;
    FIgnorarExistentes: boolean;
    FManifesto: TManifesto;
    FCompat: TCompatibilidade;
    // F10: recibo e reversao
    FPastaRecibos: string;
    FExigirIDEFechada: boolean;
    FRecibo: string;
    FLinksAntes, FInstaladosAntes: TStringList;
    // nome=encontrada|onde ou nome=instalada|pasta (versao)
    FDepsRecibo: TStringList;
    procedure Logar(const ALinha: string);
    // a configuracao volta ao que era antes desta rodada (lazbuild falhou)
    procedure Reverter;
    function SalvarRecibo(AInstalar: TList; AReconstruiu: boolean): string;
    function Compat: TCompatibilidade;
    function DetectarNaIDE(AIDE: TIDEInstance; AReceita: TReceita): string;
    function PastaNaIDE(AIDE: TIDEInstance; AReceita: TReceita): string;
    // os pacotes que esta IDE ja conhece: os que vem com ela
    // (packager/globallinks) e os registrados na configuracao
    procedure PacotesConhecidos(AConhecidos: TStrings);
    // F7: para cada dependencia exigida, usa a que ja esta, ou poe os .lpk
    // dela antes dos do RAL; o que depende do que faltou vai para AFora
    procedure PrepararDependencias(AInstalar: TList; ALinks, AAdd, AFornecidos,
      AFora: TStrings);
    procedure ParametrosBase(AParams: TStrings);
    function Lazbuild(AParams: TStrings): boolean;
    function Arquivo(APacote: TPacote): string;
    // separa o que da para instalar do que tem de ficar de fora (fonte ou
    // submodulo ausente, ou dependencia de quem ficou de fora)
    procedure Separar(ALista, AInstalar: TList; AFora: TStrings);
    procedure SetRaizFontes(const AValor: string);
    // pacotes de fora do RAL que a IDE nao conhece e que nenhuma dependencia
    // desta rodada fornece (AFornecidos)
    procedure ExternosAusentes(AInstalar: TList; AAusentes: TStrings;
      AFornecidos: TStrings = nil);
  public
    constructor Create(AIDE: TIDEInstance; ACatalogo: TCatalogo);
    destructor Destroy; override;

    function Executar: boolean;
    function Plano: string;
    // onde a dependencia ja esta nesta IDE ('' se nao esta)
    function DependenciaInstalada(AReceita: TReceita): string;
    // F6: a raiz da copia da dependencia registrada nesta IDE (o link do .lpk
    // em packagefiles.xml, menos o caminho do .lpk na receita); '' se nao sabe
    function PastaDependencia(AReceita: TReceita): string;
    // F6: a versao da dependencia que esta IDE pede; '' e AMotivo quando
    // nenhuma serve (o Zeos no FPC 3.2.3)
    function VersaoDependencia(AReceita: TReceita; out AMotivo: string): string;

    // F7: receitas conhecidas (nao pertencem a esta classe) e onde cada
    // dependencia foi baixada (nome=pasta)
    property Receitas: TReceitas read FReceitas write FReceitas;
    property PastasDependencias: TStringList read FPastasDependencias;
    property IgnorarExistentes: boolean read FIgnorarExistentes write FIgnorarExistentes;
    // F6: o manifesto da versao do RAL (nao pertence a esta classe)
    property Manifesto: TManifesto read FManifesto write FManifesto;

    property IDE: TIDEInstance read FIDE;
    property Pacotes: TStringList read FPacotes;
    // onde os fontes estao (ou vao estar, depois do download)
    property RaizFontes: string read FRaizFontes write SetRaizFontes;
    // False registra os pacotes sem reconstruir a IDE (a IDE pergunta ao abrir)
    property ConstruirIDE: boolean read FConstruirIDE write FConstruirIDE;
    property Log: TLogLinha read FLog write FLog;
    property Simular: boolean read FSimular write FSimular;
    property Relatorio: TStringList read FRelatorio;
    property Avisos: TStringList read FAvisos;
    // F10: onde gravar o recibo (o que mudou na configuracao, com o antes)
    property PastaRecibos: string read FPastaRecibos write FPastaRecibos;
    // recusa mexer na configuracao com o Lazarus aberto (ele regrava ao fechar)
    property ExigirIDEFechada: boolean read FExigirIDEFechada write FExigirIDEFechada;
    // arquivo do recibo desta rodada ('' se nao gravou)
    property Recibo: string read FRecibo;
  end;

const
  {$IFDEF MSWINDOWS}
  ExecutaveisLazarus: array[0..1] of string = ('lazarus.exe', 'startlazarus.exe');
  {$ELSE}
  ExecutaveisLazarus: array[0..1] of string = ('lazarus', 'startlazarus');
  {$ENDIF}

implementation

uses
  StrUtils, RegExpr, fpjson, RALInst.Config.Lazarus, RALInst.Fontes;

{ TInstalacaoLazarus }

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

function TInstalacaoLazarus.PastaNaIDE(AIDE: TIDEInstance; AReceita: TReceita): string;
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
        vLinks.Values[vRegex.Match[1]] := StringReplace(vRegex.Match[2], '$(LazarusDir)',
          ExcludeTrailingPathDelimiter(FIDE.RootDir), [rfReplaceAll, rfIgnoreCase]);
      until not vRegex.ExecNext;

    // um .lpk da receita que esteja registrado diz onde fica a raiz
    for vInt := 0 to Pred(AReceita.Lazarus.Acoes.Count) do
    begin
      if AReceita.Lazarus.Acao(vInt).Tipo <> taLpk then
        Continue;
      vRel := StringReplace(AReceita.Lazarus.Acao(vInt).Arquivo, '/', PathDelim, [rfReplaceAll]);
      vNome := ChangeFileExt(ExtractFileName(vRel), '');
      vArquivo := SetDirSeparators(vLinks.Values[vNome]);
      if (vArquivo <> '') and
         SameFileName(Copy(vArquivo, Length(vArquivo) - Length(vRel) + 1, MaxInt), vRel) then
        Exit(Copy(vArquivo, 1, Length(vArquivo) - Length(vRel)));
    end;
  finally
    vRegex.Free;
    vLinks.Free;
    vTexto.Free;
  end;
end;

function TInstalacaoLazarus.DetectarNaIDE(AIDE: TIDEInstance; AReceita: TReceita): string;
begin
  Result := DependenciaInstalada(AReceita);
end;

function TInstalacaoLazarus.VersaoDependencia(AReceita: TReceita; out AMotivo: string): string;
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
      Logar('(simulado) ' + vExec.LinhaComando);
      Exit(True);
    end;
    Result := vExec.Executar;
    if not Result and (vExec.Erro = '') then
      Logar(Format('ERRO: lazbuild terminou com código %d', [vExec.CodigoSaida]));
  finally
    vExec.Free;
  end;
end;

function TInstalacaoLazarus.Arquivo(APacote: TPacote): string;
begin
  Result := FRaizFontes + StringReplace(APacote.ArquivoRelativo, '/', PathDelim, [rfReplaceAll]);
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
      if (vPacote.SubmodulosAusentes.Count > 0) and not FCatalogo.Origem.BaixaSubmodulos then
        vMotivo := 'submódulo ausente: ' + vPacote.SubmodulosAusentes.CommaText
      else if vPacote.FontesAusentes.Count > 0 then
        vMotivo := 'fonte ausente: ' + vPacote.FontesAusentes[0]
      else
        for vDep := 0 to Pred(vPacote.Internos.Count) do
          if vForaNomes.IndexOf(vPacote.Internos[vDep]) >= 0 then
          begin
            vMotivo := 'depende de ' + vPacote.Internos[vDep] + ', que ficou de fora';
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
        Exit('pacote ' + AReceita.Lazarus.Deteccao.ValueFromIndex[vInt] + ' já registrado nesta IDE');
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
      Logar(Format('Dependência %s (para %s)', [vReceita.Nome, vExigidas[vInt]]));
      vVersao := VersaoDependencia(vReceita, vMotivoVersao);
      vRaiz := '';
      if vMotivoVersao = '' then
        vRaiz := FPastasDependencias.Values[ChaveDependencia(vReceita.Nome, vVersao)];
      vOnde := '';
      if not (FIgnorarExistentes and (vRaiz <> '')) then
        vOnde := DependenciaInstalada(vReceita);
      if vOnde <> '' then
      begin
        Logar('  já instalada, usando a que está lá: ' + vOnde);
        // o recibo diz que ela foi encontrada: desinstalar nao a leva junto
        FDepsRecibo.Values[vReceita.Nome] := 'encontrada|' + vOnde;
        Continue;
      end;

      vMotivo := '';
      if vReceita.Pago then
        vMotivo := Format('%s é comercial e não está instalado nesta IDE (%s)',
                          [vReceita.Nome, vReceita.Site])
      else if vMotivoVersao <> '' then
        vMotivo := vMotivoVersao
      else if vRaiz = '' then
        vMotivo := Format('%s não está instalado nesta IDE e a versão %s não foi baixada',
                          [vReceita.Nome, vVersao])
      else
        for vAcao := 0 to Pred(vReceita.Lazarus.Acoes.Count) do
          if vReceita.Lazarus.Acao(vAcao).Tipo = taLpk then
          begin
            vArquivo := ExpandirRaiz('{raiz}/' + vReceita.Lazarus.Acao(vAcao).Arquivo, vRaiz);
            if not FileExists(vArquivo) then
            begin
              vMotivo := vReceita.Nome + ': ' + vArquivo + ' não existe na versão baixada';
              Break;
            end;
            ALinks.Add(vArquivo);
            if vReceita.Lazarus.Acao(vAcao).Instalar then
              AAdd.Add(vArquivo);
          end;

      if vMotivo = '' then
      begin
        Logar('  instalando de ' + vRaiz);
        AFornecidos.AddStrings(vReceita.Lazarus.FornecePacotes);
        FDepsRecibo.Values[vReceita.Nome] := 'instalada|' + vRaiz + ' (' + vVersao + ')';
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
            vMotivo := 'depende de ' + vPacote.Internos[vDep] + ', que ficou de fora';
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

procedure TInstalacaoLazarus.ExternosAusentes(AInstalar: TList; AAusentes: TStrings;
  AFornecidos: TStrings);
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
      vPlano.Add('  configuração: ' + FIDE.ConfigDir);
    vPlano.Add('  pacotes, na ordem:');
    for vInt := 0 to Pred(vInstalar.Count) do
    begin
      vPacote := TPacote(vInstalar[vInt]);
      if vPacote.Instalavel and not vPacote.LazRuntimeOnly then
        vPlano.Add('    ' + vPacote.Nome + '  (instalar na IDE)')
      else
        vPlano.Add('    ' + vPacote.Nome + '  (só registrar)');
    end;
    for vInt := 0 to Pred(vFora.Count) do
      vPlano.Add('    fica de fora: ' + vFora[vInt]);

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
          vPlano.Add(Format('  dependência %s (%s): já instalada — %s',
                            [vReceita.Nome, vExigidas[vInt], vOnde]))
        else if (vRaiz <> '') and not vReceita.Pago and (vReceita.Lazarus.Acoes.Count > 0) then
        begin
          vPlano.Add(Format('  dependência %s %s (%s): instalar de %s',
                            [vReceita.Nome, vVersao, vExigidas[vInt], vRaiz]));
          vFornecidos.AddStrings(vReceita.Lazarus.FornecePacotes);
        end
        else
          vPlano.Add(Format('  FALTA %s (%s): não está instalado. Sem ele, %s fica de fora',
                            [vReceita.Nome, vExigidas[vInt], vExigidas[vInt]]));
      end;
    end;

    ExternosAusentes(vInstalar, vExternos, vFornecidos);
    if vExternos.Count > 0 then
      vPlano.Add('  ATENÇÃO: não encontrei nesta IDE ' + vExternos.CommaText +
                 ' — instale antes, ou a reconstrução da IDE vai falhar');
    if FConstruirIDE then
      vPlano.Add('  reconstrói a IDE no fim (lazbuild --build-ide)');
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
      Logar('A configuração do Lazarus voltou ao que era antes desta rodada.')
    else
      Logar('ERRO: não consegui devolver a configuração: ' + vErro);
  finally
    vInstaladosDepois.Free;
    vLinksDepois.Free;
    vInstalados.Free;
    vLinks.Free;
  end;
end;

function TInstalacaoLazarus.SalvarRecibo(AInstalar: TList; AReconstruiu: boolean): string;
var
  vRaiz, vIDE, vRAL, vLaz, vObj: TJSONObject;
  vLista: TJSONArray;
  vDepois: TStringList;
  vInt: integer;
  vArquivo: TStringList;
  vRepo, vVersao, vCommit: string;

  function ObjetoDe(ALista: TStrings): TJSONObject;
  var
    vI: integer;
  begin
    Result := TJSONObject.Create;
    for vI := 0 to Pred(ALista.Count) do
      Result.Add(ALista.Names[vI], ALista.ValueFromIndex[vI]);
  end;

  function ListaDe(ALista: TStrings): TJSONArray;
  var
    vI: integer;
  begin
    Result := TJSONArray.Create;
    for vI := 0 to Pred(ALista.Count) do
      Result.Add(ALista[vI]);
  end;

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
    Logar('ERRO: os fontes do RAL precisam estar em disco para instalar.');
    Exit;
  end;
  if not FileExists(FIDE.BuildFile) then
  begin
    Logar('ERRO: lazbuild não encontrado: ' + FIDE.BuildFile);
    Exit;
  end;
  // F10: com a IDE aberta, ela regrava a configuracao ao fechar e a
  // instalacao se perde
  if FExigirIDEFechada and not FSimular and
     ProgramaEmExecucao(ExecutaveisLazarus, FIDE.RootDir) then
  begin
    Logar(Format('ERRO: %s está aberto. Feche a IDE antes de instalar: ela regrava a ' +
                 'configuração ao fechar e a instalação se perderia.', [FIDE.Nome]));
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
      Logar('ERRO: pacotes que não existem nesta versão do RAL: ' + vDesconhecidos.CommaText);
      Exit;
    end;

    Separar(vLista, vInstalar, vFora);

    // F7: os .lpk das dependencias vem antes dos do RAL, na mesma chamada do
    // lazbuild — continua havendo um --build-ide so
    PrepararDependencias(vInstalar, vLinks, vAdd, vFornecidos, vFora);

    for vInt := 0 to Pred(vFora.Count) do
    begin
      Logar('fica de fora: ' + vFora[vInt]);
      FAvisos.Add(vFora[vInt]);
    end;
    ExternosAusentes(vInstalar, vDesconhecidos, vFornecidos);
    if vDesconhecidos.Count > 0 then
      FAvisos.Add('não encontrei nesta IDE ' + vDesconhecidos.CommaText +
                  '; se não estiverem instalados, a reconstrução da IDE falha');
    if vInstalar.Count = 0 then
    begin
      Logar('Nada a instalar: a configuração da IDE não foi alterada.');
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
      Logar('ERRO: falha ao registrar os pacotes.');
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
        Logar('ERRO: falha ao marcar os pacotes para instalar.');
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
        Logar('ERRO: falha ao reconstruir a IDE (o erro do compilador está acima).');
        Reverter;
        Result := False;
      end;
    end;

    // 4. recibo: so do que ficou na configuracao
    if Result and not FSimular then
      try
        FRecibo := SalvarRecibo(vInstalar, FConstruirIDE and (vAdd.Count > 0));
        Logar('Recibo: ' + FRecibo);
      except
        on E: Exception do
          FAvisos.Add('não foi possível gravar o recibo: ' + E.Message);
      end;

    for vInt := 0 to Pred(vInstalar.Count) do
      if not Result and (FRecibo = '') then
        FRelatorio.Add(Format('%-24s %s', [TPacote(vInstalar[vInt]).Nome,
          'não instalado (configuração devolvida ao que era)']))
      else
        FRelatorio.Add(Format('%-24s %s', [TPacote(vInstalar[vInt]).Nome,
          IfThen(vAdd.IndexOf(Arquivo(TPacote(vInstalar[vInt]))) >= 0,
                 IfThen(Result and FConstruirIDE, 'instalado', 'marcado, IDE não reconstruída'),
                 'registrado')]));
    for vInt := 0 to Pred(vFora.Count) do
      FRelatorio.Add('fora: ' + vFora[vInt]);
    if not FConstruirIDE and (vAdd.Count > 0) then
      FAvisos.Add('IDE não reconstruída: ela pede para reconstruir ao abrir.');
    for vInt := 0 to Pred(FAvisos.Count) do
      FRelatorio.Add('AVISO: ' + FAvisos[vInt]);
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

end.
