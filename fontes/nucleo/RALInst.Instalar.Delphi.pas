unit RALInst.Instalar.Delphi;

{$mode ObjFPC}{$H+}

// Instalacao completa numa IDE Delphi: compila (F3), escreve o registro (F4)
// e grava o recibo. E a mesma rodada para a GUI, para a CLI e para os testes.
//
// Ordem: conferir tudo o que da para conferir antes de tocar em qualquer
// coisa (IDE fechada, chave existe, pacotes conhecidos), compilar, e so entao
// registrar — e registrar apenas o que compilou.

{$IFNDEF MSWINDOWS}
  {$FATAL RALInst.Instalar.Delphi so compila no Windows}
{$ENDIF}

interface

uses
  Classes, SysUtils, RALInst.IDE, RALInst.Catalogo, RALInst.Processo,
  RALInst.Build.Delphi, RALInst.Registro.Delphi, RALInst.Receitas, RALInst.Compatibilidade;

type

  { TInstalacaoDelphi }
  TInstalacaoDelphi = class
  private
    FIDE: TIDEInstance;
    FCatalogo: TCatalogo;
    FRaizFontes: string;
    FPacotes: TStringList;
    FPlataformas: TStringList;
    FSomenteLibraryPath: boolean;
    FExigirIDEFechada: boolean;
    FChaveRegistro: string;
    FPastaBpl: string;
    FPastaDcp: string;
    FPastaRecibos: string;
    FCaminhosExtras: TStringList;
    FNomeVariavel: string;
    FLog: TLogLinha;
    FSimular: boolean;
    FRegistro: TRegistroDelphi;
    FRelatorio: TStringList;
    FAvisos: TStringList;
    FRecibo: string;
    FReceitas: TReceitas;
    FPastasDependencias: TStringList;
    FIgnorarExistentes: boolean;
    // variaveis que as receitas definiram nesta rodada: valem para expandir
    // os caminhos mesmo simulando (quando o registro nao foi escrito)
    FVariaveisDeps: TStringList;
    FManifesto: TManifesto;
    FCompat: TCompatibilidade;
    // .dcp das dependencias para o -LU (pacotes-ligados das receitas)
    FPacotesLigados: TStringList;
    // F10: o que o recibo guarda alem do registro: arquivos gerados
    // (arquivo=tamanho|data) e as dependencias (nome=instalada|... ou
    // nome=encontrada|...)
    FArquivosRecibo: TStringList;
    FDepsRecibo: TStringList;
    procedure ResolverLigados(AReceita: TReceita);
    procedure AnotarArquivos(const AResultado: TResultadoPacote);
    // as pastas do library path (Win32) que tem o arquivo; expandidas
    procedure PastasNoLibraryPath(const AArquivo: string; APastas: TStrings);
    procedure Logar(const ALinha: string);
    function Compat: TCompatibilidade;
    function DetectarNaIDE(AIDE: TIDEInstance; AReceita: TReceita): string;
    // F6: tira da lista o que nao cabe nesta IDE; AFora recebe nome=motivo
    procedure FiltrarCompativeis(ALista: TList; AFora: TStrings);
    function NomesDaLista(ALista: TList): TStringList;
    procedure GarantirRegistro;
    function ExpandirDep(const ACaminho, ARaiz: string): string;
    procedure CaminhosDaReceita(AReceita: TReceita; const ARaiz: string; ALista: TStrings);
    function CompilarDpk(AReceita: TReceita; AAcao: TAcaoReceita; const ARaiz: string): boolean;
    function AplicarDependencia(AReceita: TReceita; const ARaiz: string): boolean;
    procedure PrepararDependencias(ALista: TList; AResultados: TStrings);
    function PastaSaida(const APlataforma, AValor, APadrao: string): string;
    procedure MontarCaminhos(APacotes: TList; ACaminhos, AUnidades: TStrings);
    function CaminhoParaRegistro(const ARelativo: string): string;
    function Conferir(APacotes: TList): boolean;
    function SalvarRecibo(APacotes: TList; AResultados: TStrings): string;
    procedure SetRaizFontes(const AValor: string);
  public
    constructor Create(AIDE: TIDEInstance; ACatalogo: TCatalogo);
    destructor Destroy; override;

    // devolve False se algo falhou; o relatorio diz o que entrou e o que nao
    function Executar: boolean;
    // onde a dependencia ja esta nesta IDE ('' se nao esta): variavel da IDE,
    // unidade no library path ou .dcp
    function DependenciaInstalada(AReceita: TReceita): string;
    // F6: a versao da dependencia que esta IDE pede (manifesto, faixa da
    // receita ou a padrao); '' e AMotivo quando nenhuma serve
    function VersaoDependencia(AReceita: TReceita; out AMotivo: string): string;
    // o que a rodada vai fazer, sem fazer nada
    function Plano: string;

    property IDE: TIDEInstance read FIDE;
    // nomes pedidos pelo usuario; as dependencias internas entram sozinhas
    // onde os fontes estao (ou vao estar, depois do download); por padrao a
    // raiz do catalogo, quando ele vem do disco
    property RaizFontes: string read FRaizFontes write SetRaizFontes;
    property Pacotes: TStringList read FPacotes;
    // 'win32' sempre; 'win64' compila o runtime e poe o library path de Win64
    property Plataformas: TStringList read FPlataformas;
    // so aponta os fontes no library path, sem compilar nem instalar pacote
    property SomenteLibraryPath: boolean read FSomenteLibraryPath write FSomenteLibraryPath;
    // recusa escrever com a IDE aberta (ela regrava tudo ao fechar)
    property ExigirIDEFechada: boolean read FExigirIDEFechada write FExigirIDEFechada;
    // relativa a HKCU; vazio = a da IDE. Os testes apontam para uma copia
    property ChaveRegistro: string read FChaveRegistro write FChaveRegistro;
    // vazio = o que a IDE usa (Package DPL/DCP Output, ou <BDSCOMMONDIR>\Bpl)
    property PastaBpl: string read FPastaBpl write FPastaBpl;
    property PastaDcp: string read FPastaDcp write FPastaDcp;
    property PastaRecibos: string read FPastaRecibos write FPastaRecibos;
    property CaminhosExtras: TStringList read FCaminhosExtras;
    // a variavel da IDE que aponta para <raiz>\src ('PascalRAL', como na wiki)
    property NomeVariavel: string read FNomeVariavel write FNomeVariavel;
    property Log: TLogLinha read FLog write FLog;
    property Simular: boolean read FSimular write FSimular;
    property Relatorio: TStringList read FRelatorio;
    property Avisos: TStringList read FAvisos;
    // arquivo do recibo desta rodada ('' se nao gravou)
    property Recibo: string read FRecibo;
    // F7: as receitas conhecidas (nao pertencem a esta classe) e onde cada
    // dependencia foi baixada (nome=pasta)
    property Receitas: TReceitas read FReceitas write FReceitas;
    property PastasDependencias: TStringList read FPastasDependencias;
    // instala a dependencia baixada mesmo que a IDE ja tenha uma
    property IgnorarExistentes: boolean read FIgnorarExistentes write FIgnorarExistentes;
    // F6: o manifesto da versao do RAL (nao pertence a esta classe; nil = so o
    // que da para deduzir do disco)
    property Manifesto: TManifesto read FManifesto write FManifesto;
  end;

implementation

uses
  StrUtils, fpjson, RALInst.Fontes;

function TemPas(const APasta: string): boolean;
var
  vBusca: TSearchRec;
begin
  Result := FindFirst(IncludeTrailingPathDelimiter(APasta) + '*.pas', faAnyFile, vBusca) = 0;
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

function TInstalacaoDelphi.DetectarNaIDE(AIDE: TIDEInstance; AReceita: TReceita): string;
begin
  Result := DependenciaInstalada(AReceita);
end;

function TInstalacaoDelphi.VersaoDependencia(AReceita: TReceita; out AMotivo: string): string;
begin
  Result := Compat.VersaoDependencia(AReceita, FIDE, AMotivo);
end;

procedure TInstalacaoDelphi.FiltrarCompativeis(ALista: TList; AFora: TStrings);
begin
  AFora.Clear;
  Compat.Filtrar(ALista, FIDE, AFora);
end;

procedure TInstalacaoDelphi.PastasNoLibraryPath(const AArquivo: string; APastas: TStrings);
var
  vItens: TStringList;
  vPasta, vValor: string;
begin
  GarantirRegistro;
  vItens := TStringList.Create;
  try
    vItens.StrictDelimiter := True;
    vItens.Delimiter := ';';
    vItens.DelimitedText := FRegistro.LerValor(FRegistro.ChaveLibrary('win32'), 'Search Path');
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
        FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', FileDateToDateTime(vBusca.Time));
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
        if FindFirst(IncludeTrailingPathDelimiter(vPasta) + vPadrao + '.dcp', faAnyFile, vBusca) = 0 then
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
    Logar('  compila contra ' + FPacotesLigados.CommaText);
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
        vItens.DelimitedText := FRegistro.LerValor(FRegistro.ChaveLibrary('win32'), 'Search Path');
        for vPasta in vItens do
        begin
          vValor := FRegistro.Expandir(Trim(vPasta));
          if (vValor <> '') and (Pos('$(', vValor) = 0) and
             FileExists(IncludeTrailingPathDelimiter(vValor) + vNome) then
            Exit(Format('%s no library path (%s)', [vNome, Trim(vPasta)]));
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
            if FindFirst(IncludeTrailingPathDelimiter(vPasta) + vNome + '.dcp', faAnyFile, vBusca) = 0 then
            begin
              vValor := vBusca.Name;
              SysUtils.FindClose(vBusca);
              Exit(IncludeTrailingPathDelimiter(vPasta) + vValor);
            end;
          end
          else if FileExists(IncludeTrailingPathDelimiter(vPasta) + vNome + '.dcp') then
            Exit(IncludeTrailingPathDelimiter(vPasta) + vNome + '.dcp')
          else if FileExists(IncludeTrailingPathDelimiter(vPasta) + vNome + FIDE.SufixoPacote + '.dcp') then
            Exit(IncludeTrailingPathDelimiter(vPasta) + vNome + FIDE.SufixoPacote + '.dcp');
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
                            ExcludeTrailingPathDelimiter(FVariaveisDeps.ValueFromIndex[vInt]),
                            [rfReplaceAll, rfIgnoreCase]);
  Result := FRegistro.Expandir(Result);
end;

procedure TInstalacaoDelphi.CaminhosDaReceita(AReceita: TReceita; const ARaiz: string;
  ALista: TStrings);
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
      if (Pos('$(', vPasta) = 0) and DirectoryExists(vPasta) and (ALista.IndexOf(vPasta) < 0) then
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
  vBpls: TStringList;
  vPacote: TPacote;
begin
  Result := False;
  vPasta := AAcao.Pastas.Values[FIDE.BDSVersao];
  if vPasta = '' then
  begin
    Logar(Format('ERRO: %s não tem pacotes para %s (BDS %s)',
                 [AReceita.Nome, FIDE.Nome, FIDE.BDSVersao]));
    Exit;
  end;

  vCatalogo := TCatalogo.Create;
  vLista := TList.Create;
  vBpls := TStringList.Create;
  try
    vCatalogo.PastaDelphi := vPasta;
    if not vCatalogo.Carregar(ARaiz) then
    begin
      Logar(Format('ERRO: nenhum .dpk de %s em %s', [AReceita.Nome, ARaiz + vPasta]));
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
      end;
  finally
    vBpls.Free;
    vLista.Free;
    vCatalogo.Free;
  end;
end;

function TInstalacaoDelphi.AplicarDependencia(AReceita: TReceita; const ARaiz: string): boolean;
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
              vAdicionados := FRegistro.AdicionarCaminhos(vPlat, 'Search Path', vCaminhos);
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
      Logar(Format('Dependência %s (para %s)', [vReceita.Nome, vExigidas[vInt]]));
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
        Logar('  já instalada, usando a que está lá: ' + vOnde);
        CaminhosDaReceita(vReceita, '', FCaminhosExtras);
        ResolverLigados(vReceita);
        // o que o RAL inclui dela (ZComponent.inc) vem de onde a IDE a acha
        for vArquivo in vReceita.Delphi.Busca do
        begin
          vAntes := FCaminhosExtras.Count;
          PastasNoLibraryPath(vArquivo, FCaminhosExtras);
          if FCaminhosExtras.Count = vAntes then
            Logar('  AVISO: ' + vArquivo + ' não está no library path da IDE; ' +
                  'a compilação pode não achá-lo')
          else
            Logar('  ' + vArquivo + ' em ' + FCaminhosExtras[Pred(FCaminhosExtras.Count)]);
        end;
        // encontrada: desinstalar nao a leva junto
        FDepsRecibo.Values[vReceita.Nome] := 'encontrada|' + vOnde;
        Continue;
      end;

      if vReceita.Pago then
        vMotivo := Format('%s é comercial e não está instalado nesta IDE; instale-o ' +
                          '(%s) e rode o instalador de novo', [vReceita.Nome, vReceita.Site])
      else if vMotivoVersao <> '' then
        vMotivo := vMotivoVersao
      else if vRaiz = '' then
        vMotivo := Format('%s não está instalado nesta IDE e a versão %s não foi baixada',
                          [vReceita.Nome, vVersao])
      else if vReceita.Delphi.Acoes.Count = 0 then
        vMotivo := vReceita.Nome + ': a receita não diz como instalar no Delphi'
      else
      begin
        Logar('  instalando de ' + vRaiz);
        if AplicarDependencia(vReceita, vRaiz) then
        begin
          CaminhosDaReceita(vReceita, vRaiz, FCaminhosExtras);
          ResolverLigados(vReceita);
          FDepsRecibo.Values[vReceita.Nome] := 'instalada|' + vRaiz + ' (' + vVersao + ')';
          Continue;
        end;
        vMotivo := 'a instalação de ' + vReceita.Nome + ' falhou (detalhes acima)';
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
            vMotivo := 'depende de ' + vPacote.Internos[vDep] + ', que ficou de fora';
      if vMotivo <> '' then
      begin
        vFora.Add(vPacote.Nome);
        FAvisos.Add(vPacote.Nome + ' fica de fora: ' + vMotivo);
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

function TInstalacaoDelphi.PastaSaida(const APlataforma, AValor, APadrao: string): string;
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
  vRel := StringReplace(ExcludeTrailingPathDelimiter(ARelativo), '/', '\', [rfReplaceAll]);
  if FRegistro.AceitaVariaveis and SameText(Copy(vRel, 1, 4), 'src\') then
    Result := '$(' + FNomeVariavel + ')\' + Copy(vRel, 5, MaxInt)
  else if SameText(vRel, 'src') and FRegistro.AceitaVariaveis then
    Result := '$(' + FNomeVariavel + ')'
  else
    Result := FRaizFontes + vRel;
end;

procedure TInstalacaoDelphi.MontarCaminhos(APacotes: TList; ACaminhos, AUnidades: TStrings);
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
          AUnidades.Add(ExtractFileName(StringReplace(vUnidade, '/', '\', [rfReplaceAll])));
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
        vSubmodulo := StringReplace(TPacote(APacotes[vInt]).Submodulos[vSub], '/', '\', [rfReplaceAll]);
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
    Logar('ERRO: os fontes do RAL precisam estar em disco para instalar.');
    Exit;
  end;

  vDesconhecidos := TStringList.Create;
  try
    FCatalogo.Fechamento(tpDelphi, FPacotes, APacotes, vDesconhecidos);
    if vDesconhecidos.Count > 0 then
    begin
      Logar('ERRO: pacotes que não existem nesta versão do RAL: ' + vDesconhecidos.CommaText);
      Exit;
    end;
  finally
    vDesconhecidos.Free;
  end;
  if APacotes.Count = 0 then
  begin
    Logar('ERRO: nenhum pacote escolhido.');
    Exit;
  end;

  if not FRegistro.ChaveExiste then
  begin
    Logar(Format('ERRO: %s nunca foi aberto (não há HKCU%s). Abra a IDE uma vez, ' +
                 'feche, e rode o instalador de novo.', [FIDE.Nome, FRegistro.Chave]));
    Exit;
  end;

  if FExigirIDEFechada and FRegistro.IDEEmExecucao then
  begin
    Logar(Format('ERRO: %s está aberto. Feche a IDE antes de instalar: ela ' +
                 'regrava o registro ao fechar e a instalação se perderia.', [FIDE.Nome]));
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
  try
    if FChaveRegistro <> '' then
      FRegistro.Chave := FChaveRegistro;
    FCatalogo.Fechamento(tpDelphi, FPacotes, vLista);
    // F6: o que nao cabe nesta IDE sai antes de tudo, com o motivo
    FiltrarCompativeis(vLista, vFora);
    vNomes := NomesDaLista(vLista);

    vPlano.Add(FIDE.Nome + '  (' + ExcludeTrailingPathDelimiter(FIDE.RootDir) + ')');
    if not FRegistro.ChaveExiste then
      vPlano.Add('  ATENÇÃO: a IDE nunca foi aberta; abra e feche uma vez antes de instalar')
    else if FExigirIDEFechada and FRegistro.IDEEmExecucao then
      vPlano.Add('  ATENÇÃO: a IDE está aberta agora; feche antes de instalar');
    if FSomenteLibraryPath then
      vPlano.Add('  modo: somente library path (nenhum pacote compilado ou instalado)')
    else
    begin
      vPlano.Add('  compilar, na ordem:');
      for vInt := 0 to Pred(vLista.Count) do
        with TPacote(vLista[vInt]) do
          if Instalavel then
            vPlano.Add('    ' + Nome + '  (instalar na IDE)')
          else
            vPlano.Add('    ' + Nome + '  (runtime)');
      for vInt := 0 to Pred(vFora.Count) do
        vPlano.Add('    fica de fora: ' + vFora.Names[vInt] + ' — ' + vFora.ValueFromIndex[vInt]);
      for vPlat in FPlataformas do
        vPlano.Add(Format('  %s: .bpl em %s', [vPlat,
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
            vPlano.Add(Format('  dependência %s (%s): já instalada — %s',
                              [vReceita.Nome, vExigidas[vInt], vAtual]))
          else if vReceita.Pago then
            vPlano.Add(Format('  FALTA %s (%s): é comercial; instale-o antes (%s). ' +
                              'Sem ele, %s fica de fora',
                              [vReceita.Nome, vExigidas[vInt], vReceita.Site, vExigidas[vInt]]))
          else if (vRaiz <> '') and (vReceita.Delphi.Acoes.Count > 0) then
            vPlano.Add(Format('  dependência %s %s (%s): instalar de %s',
                              [vReceita.Nome, vVersao, vExigidas[vInt], vRaiz]))
          else
            vPlano.Add(Format('  FALTA %s (%s): não está instalado. Sem ele, %s fica de fora',
                              [vReceita.Nome, vExigidas[vInt], vExigidas[vInt]]));
        end;
      finally
        vExigidas.Free;
      end;
    end;

    MontarCaminhos(vLista, vCaminhos, vUnidades);
    if FRegistro.AceitaVariaveis then
    begin
      vPlano.Add(Format('  variável $(%s) = %s', [FNomeVariavel, FRaizFontes + 'src']));
      // a variavel e da IDE, nao do instalador: os projetos do usuario que usam
      // $(PascalRAL) passam a ver a pasta nova
      vAtual := FRegistro.LerValor('Environment Variables', FNomeVariavel);
      if (vAtual <> '') and not SameText(ExcludeTrailingPathDelimiter(vAtual),
                                         ExcludeTrailingPathDelimiter(FRaizFontes + 'src')) then
        vPlano.Add(Format('  ATENÇÃO: $(%s) hoje aponta para %s; os projetos que usam ' +
                          'a variável passam a ver a pasta nova', [FNomeVariavel, vAtual]));
    end;
    vPlano.Add('  library path (' + FPlataformas.CommaText + '):');
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

function TInstalacaoDelphi.SalvarRecibo(APacotes: TList; AResultados: TStrings): string;
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
              Format('delphi-%s-%s.json', [FIDE.BDSVersao, FormatDateTime('yyyymmdd-hhnnss', Now)]);
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
  vOkWin32, vResultados, vCaminhos, vUnidades, vConflitos: TStringList;
  vResultado: TResultadoPacote;
  vPacote: TPacote;
begin
  Result := False;
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
      Logar(Format('%s fica de fora: %s', [vConflitos.Names[vInt], vConflitos.ValueFromIndex[vInt]]));
      FAvisos.Add(vConflitos.Names[vInt] + ' fica de fora: ' + vConflitos.ValueFromIndex[vInt]);
      vResultados.Add(vConflitos.Names[vInt] + ': pulado — ' + vConflitos.ValueFromIndex[vInt]);
    end;
    vConflitos.Clear;
    if vLista.Count = 0 then
    begin
      Logar('Nenhum pacote escolhido cabe nesta IDE: o registro não foi alterado.');
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
      Logar('Nenhum pacote sobrou para instalar: faltam as dependências acima.');
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
          FAvisos.Add(Format('%s não compila para %s; plataforma ignorada.', [FIDE.Nome, vPlat]));
          Continue;
        end;

        vBuild := TBuildDelphi.Create(FIDE, FCatalogo);
        try
          vBuild.Plataforma := vPlat;
          vBuild.Log := FLog;
          vBuild.Simular := FSimular;
          vBuild.CaminhosExtras.AddStrings(FCaminhosExtras);
          vBuild.PacotesExtras.AddStrings(FPacotesLigados);
          if FPastaBpl <> '' then
            vBuild.PastaBpl := FPastaBpl + IfThen(SameText(vPlat, 'win32'), '', '\' + vPlat)
          else
            vBuild.PastaBpl := PastaSaida(vPlat, 'Package DPL Output', '');
          if FPastaDcp <> '' then
            vBuild.PastaDcp := FPastaDcp + IfThen(SameText(vPlat, 'win32'), '', '\' + vPlat)
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
      Logar('Nada compilou: o registro da IDE não foi alterado.');
      Exit(False);
    end;

    // 2. library path e variavel, so do que vai ficar utilizavel
    Logar('Registro: HKCU' + FRegistro.Chave);
    MontarCaminhos(vRegistrar, vCaminhos, vUnidades);
    if FRegistro.AceitaVariaveis then
      if not FRegistro.DefinirVariavel(FNomeVariavel, FRaizFontes + 'src') then
        Result := False;

    for vPlat in FPlataformas do
    begin
      FRegistro.CaminhosConflitantes(vPlat, vCaminhos, vUnidades, vConflitos);
      for vInt := 0 to Pred(vConflitos.Count) do
        FAvisos.Add(Format('%s: outra cópia do RAL no library path: %s',
                           [PlataformaRegistro(vPlat), vConflitos[vInt]]));

      vAdicionados := FRegistro.AdicionarCaminhos(vPlat, 'Search Path', vCaminhos);
      if vAdicionados < 0 then
        Result := False
      else if vAdicionados = 0 then
        Logar(Format('  %s Search Path: já tinha todos os caminhos', [PlataformaRegistro(vPlat)]));
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
      end;

    for vInt := 0 to Pred(vLista.Count) do
      if vRegistrar.IndexOf(vLista[vInt]) < 0 then
        FAvisos.Add(TPacote(vLista[vInt]).Nome + ' não compilou e não foi registrado; ' +
                    'se havia uma versão anterior registrada, ela continua lá.');

    // 4. recibo: o que mudou, com o valor de antes
    if not FSimular then
    begin
      try
        FRecibo := SalvarRecibo(vLista, vResultados);
        Logar('Recibo: ' + FRecibo);
      except
        on E: Exception do
          FAvisos.Add('não foi possível gravar o recibo: ' + E.Message);
      end;
    end;

    FRelatorio.Add(Format('Registro: %d alteração(ões) em HKCU%s%s',
      [FRegistro.TotalAlteracoes, FRegistro.Chave, IfThen(FSimular, ' (simulado)', '')]));
    for vInt := 0 to Pred(FAvisos.Count) do
      FRelatorio.Add('AVISO: ' + FAvisos[vInt]);
  finally
    vConflitos.Free;
    vUnidades.Free;
    vCaminhos.Free;
    vResultados.Free;
    vOkWin32.Free;
    vRegistrar.Free;
    vLista.Free;
  end;
end;

end.
