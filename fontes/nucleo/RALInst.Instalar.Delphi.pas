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
  RALInst.Build.Delphi, RALInst.Registro.Delphi;

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
    procedure Logar(const ALinha: string);
    function PastaSaida(const APlataforma, AValor, APadrao: string): string;
    procedure MontarCaminhos(APacotes: TList; ACaminhos, AUnidades: TStrings);
    function CaminhoParaRegistro(const ARelativo: string): string;
    function Conferir(APacotes: TList): boolean;
    function SalvarRecibo(APacotes: TList; AResultados: TStrings): string;
  public
    constructor Create(AIDE: TIDEInstance; ACatalogo: TCatalogo);
    destructor Destroy; override;

    // devolve False se algo falhou; o relatorio diz o que entrou e o que nao
    function Executar: boolean;
    // o que a rodada vai fazer, sem fazer nada
    function Plano: string;

    property IDE: TIDEInstance read FIDE;
    // nomes pedidos pelo usuario; as dependencias internas entram sozinhas
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
  end;

// pasta padrao dos recibos e logs do instalador
function PastaDadosInstalador: string;

implementation

uses
  StrUtils, fpjson;

function TemPas(const APasta: string): boolean;
var
  vBusca: TSearchRec;
begin
  Result := FindFirst(IncludeTrailingPathDelimiter(APasta) + '*.pas', faAnyFile, vBusca) = 0;
  if Result then
    SysUtils.FindClose(vBusca);
end;

function PastaDadosInstalador: string;
begin
  Result := IncludeTrailingPathDelimiter(GetAppConfigDir(False));
end;

{ TInstalacaoDelphi }

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
end;

destructor TInstalacaoDelphi.Destroy;
begin
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
  vUnidade, vRel, vCaminho, vSubmodulo, vPai: string;
  vVariante: string;
  vPastasPacote: TStringList;
begin
  ACaminhos.Clear;
  AUnidades.Clear;
  vPastasPacote := TStringList.Create;
  try
    vPastasPacote.CaseSensitive := False;
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
        if vPastasPacote.IndexOf(ExcludeTrailingPathDelimiter(vRel)) < 0 then
          vPastasPacote.Add(ExcludeTrailingPathDelimiter(vRel));
      end;
    end;

    // submodulos (kxBSON, ZSTD, brotli): nem todo .dpk lista as unidades
    // deles, mas quem usa o pacote precisa delas no path. Entra o submodulo
    // que mora ao lado das unidades de algum pacote escolhido
    for vSub := 0 to Pred(FCatalogo.Submodulos.Count) do
    begin
      vSubmodulo := StringReplace(FCatalogo.Submodulos.Names[vSub], '/', '\', [rfReplaceAll]);
      vPai := ExcludeTrailingPathDelimiter(ExtractFilePath(vSubmodulo));
      if vPastasPacote.IndexOf(vPai) < 0 then
        Continue;
      for vVariante in TStringArray.Create('', '\Source', '\src') do
        if DirectoryExists(FRaizFontes + vSubmodulo + vVariante) and
           TemPas(FRaizFontes + vSubmodulo + vVariante) then
        begin
          vCaminho := CaminhoParaRegistro(vSubmodulo + vVariante);
          if ACaminhos.IndexOf(vCaminho) < 0 then
            ACaminhos.Add(vCaminho);
        end;
    end;
  finally
    vPastasPacote.Free;
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
  vPlat: string;
begin
  vLista := TList.Create;
  vPlano := TStringList.Create;
  vCaminhos := TStringList.Create;
  vUnidades := TStringList.Create;
  FreeAndNil(FRegistro);
  FRegistro := TRegistroDelphi.Create(FIDE);
  try
    if FChaveRegistro <> '' then
      FRegistro.Chave := FChaveRegistro;
    FCatalogo.Fechamento(tpDelphi, FPacotes, vLista);

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
      for vPlat in FPlataformas do
        vPlano.Add(Format('  %s: .bpl em %s', [vPlat,
          PastaSaida(vPlat, 'Package DPL Output', FIDE.CommonDir + 'Bpl' +
                     IfThen(SameText(vPlat, 'win32'), '', '\' + vPlat))]));
    end;

    MontarCaminhos(vLista, vCaminhos, vUnidades);
    if FRegistro.AceitaVariaveis then
      vPlano.Add(Format('  variável $(%s) = %s', [FNomeVariavel, FRaizFontes + 'src']));
    vPlano.Add('  library path (' + FPlataformas.CommaText + '):');
    for vInt := 0 to Pred(vCaminhos.Count) do
      vPlano.Add('    ' + vCaminhos[vInt]);
    Result := vPlano.Text;
  finally
    vUnidades.Free;
    vCaminhos.Free;
    vPlano.Free;
    vLista.Free;
  end;
end;

function TInstalacaoDelphi.SalvarRecibo(APacotes: TList; AResultados: TStrings): string;
var
  vRaiz, vIDE: TJSONObject;
  vLista: TJSONArray;
  vInt: integer;
  vArquivo: TStringList;
begin
  Result := '';
  vRaiz := TJSONObject.Create;
  try
    vRaiz.Add('instalador', 'RALInstaller');
    vRaiz.Add('data', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', Now));
    vIDE := TJSONObject.Create;
    vIDE.Add('tipo', 'delphi');
    vIDE.Add('nome', FIDE.Nome);
    vIDE.Add('bds', FIDE.BDSVersao);
    vIDE.Add('raiz', FIDE.RootDir);
    vIDE.Add('chave', 'HKCU' + FRegistro.Chave);
    vRaiz.Add('ide', vIDE);
    vRaiz.Add('fontes', FRaizFontes);
    vRaiz.Add('somente-library-path', FSomenteLibraryPath);

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
