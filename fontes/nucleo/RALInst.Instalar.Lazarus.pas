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
  Classes, SysUtils, RALInst.IDE, RALInst.Catalogo, RALInst.Processo;

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
    procedure Logar(const ALinha: string);
    procedure ParametrosBase(AParams: TStrings);
    function Lazbuild(AParams: TStrings): boolean;
    function Arquivo(APacote: TPacote): string;
    // separa o que da para instalar do que tem de ficar de fora (fonte ou
    // submodulo ausente, ou dependencia de quem ficou de fora)
    procedure Separar(ALista, AInstalar: TList; AFora: TStrings);
    // pacotes de fora do RAL que a IDE nao conhece (nem os que vem com ela,
    // em packager/globallinks, nem os registrados na configuracao)
    procedure ExternosAusentes(AInstalar: TList; AAusentes: TStrings);
  public
    constructor Create(AIDE: TIDEInstance; ACatalogo: TCatalogo);
    destructor Destroy; override;

    function Executar: boolean;
    function Plano: string;

    property IDE: TIDEInstance read FIDE;
    property Pacotes: TStringList read FPacotes;
    // False registra os pacotes sem reconstruir a IDE (a IDE pergunta ao abrir)
    property ConstruirIDE: boolean read FConstruirIDE write FConstruirIDE;
    property Log: TLogLinha read FLog write FLog;
    property Simular: boolean read FSimular write FSimular;
    property Relatorio: TStringList read FRelatorio;
    property Avisos: TStringList read FAvisos;
  end;

implementation

uses
  StrUtils;

{ TInstalacaoLazarus }

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
end;

destructor TInstalacaoLazarus.Destroy;
begin
  FAvisos.Free;
  FRelatorio.Free;
  FPacotes.Free;
  inherited Destroy;
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
      if vPacote.SubmodulosAusentes.Count > 0 then
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

procedure TInstalacaoLazarus.ExternosAusentes(AInstalar: TList; AAusentes: TStrings);
var
  vConhecidos, vLinhas: TStringList;
  vBusca: TSearchRec;
  vNome, vArquivo: string;
  vInt, vExt, vPos: integer;
begin
  AAusentes.Clear;
  vConhecidos := TStringList.Create;
  vLinhas := TStringList.Create;
  try
    vConhecidos.CaseSensitive := False;
    vConhecidos.Sorted := True;
    vConhecidos.Duplicates := dupIgnore;

    // os que vem com o Lazarus: packager/globallinks/<nome>-<versao>.lpl
    if FindFirst(FIDE.RootDir + 'packager' + PathDelim + 'globallinks' + PathDelim +
                 '*.lpl', faAnyFile, vBusca) = 0 then
    try
      repeat
        vNome := ChangeFileExt(vBusca.Name, '');
        vPos := RPos('-', vNome);
        if vPos > 0 then
          vNome := Copy(vNome, 1, vPos - 1);
        vConhecidos.Add(vNome);
      until FindNext(vBusca) <> 0;
    finally
      SysUtils.FindClose(vBusca);
    end;

    // os que o usuario registrou (OPM, --add-package-link): packagefiles.xml
    vArquivo := FIDE.ConfigDir + 'packagefiles.xml';
    if (FIDE.ConfigDir <> '') and FileExists(vArquivo) then
    begin
      vLinhas.LoadFromFile(vArquivo);
      for vInt := 0 to Pred(vLinhas.Count) do
      begin
        vPos := Pos('<Name Value="', vLinhas[vInt]);
        if vPos > 0 then
        begin
          vNome := Copy(vLinhas[vInt], vPos + 13, MaxInt);
          vConhecidos.Add(Copy(vNome, 1, Pos('"', vNome) - 1));
        end;
      end;
    end;

    for vInt := 0 to Pred(AInstalar.Count) do
      for vExt := 0 to Pred(TPacote(AInstalar[vInt]).Externos.Count) do
      begin
        vNome := TPacote(AInstalar[vInt]).Externos[vExt];
        if (vConhecidos.IndexOf(vNome) < 0) and (AAusentes.IndexOf(vNome) < 0) then
          AAusentes.Add(vNome);
      end;
  finally
    vLinhas.Free;
    vConhecidos.Free;
  end;
end;

function TInstalacaoLazarus.Plano: string;
var
  vLista, vInstalar: TList;
  vFora, vPlano, vExternos: TStringList;
  vInt: integer;
  vPacote: TPacote;
begin
  vLista := TList.Create;
  vInstalar := TList.Create;
  vFora := TStringList.Create;
  vPlano := TStringList.Create;
  vExternos := TStringList.Create;
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
    ExternosAusentes(vInstalar, vExternos);
    if vExternos.Count > 0 then
      vPlano.Add('  ATENÇÃO: não encontrei nesta IDE ' + vExternos.CommaText +
                 ' — instale antes, ou a reconstrução da IDE vai falhar');
    if FConstruirIDE then
      vPlano.Add('  reconstrói a IDE no fim (lazbuild --build-ide)');
    Result := vPlano.Text;
  finally
    vExternos.Free;
    vPlano.Free;
    vFora.Free;
    vInstalar.Free;
    vLista.Free;
  end;
end;

function TInstalacaoLazarus.Executar: boolean;
var
  vLista, vInstalar: TList;
  vFora, vLinks, vAdd, vDesconhecidos: TStringList;
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

  vLista := TList.Create;
  vInstalar := TList.Create;
  vFora := TStringList.Create;
  vLinks := TStringList.Create;
  vAdd := TStringList.Create;
  vDesconhecidos := TStringList.Create;
  try
    FCatalogo.Fechamento(tpLazarus, FPacotes, vLista, vDesconhecidos);
    if vDesconhecidos.Count > 0 then
    begin
      Logar('ERRO: pacotes que não existem nesta versão do RAL: ' + vDesconhecidos.CommaText);
      Exit;
    end;

    Separar(vLista, vInstalar, vFora);
    for vInt := 0 to Pred(vFora.Count) do
    begin
      Logar('fica de fora: ' + vFora[vInt]);
      FAvisos.Add(vFora[vInt]);
    end;
    ExternosAusentes(vInstalar, vDesconhecidos);
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

    // 1. todos conhecidos pela IDE
    vDesconhecidos.Clear;
    ParametrosBase(vDesconhecidos);
    vDesconhecidos.Add('--add-package-link');
    vDesconhecidos.AddStrings(vLinks);
    if not Lazbuild(vDesconhecidos) then
    begin
      Logar('ERRO: falha ao registrar os pacotes; a IDE não foi alterada.');
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
        Logar('ERRO: falha ao marcar os pacotes para instalar; a IDE não foi reconstruída.');
        Exit(False);
      end;
    end;

    // 3. uma reconstrucao so
    if FConstruirIDE and (vAdd.Count > 0) then
    begin
      vDesconhecidos.Clear;
      ParametrosBase(vDesconhecidos);
      vDesconhecidos.Add('--build-ide=');
      if not Lazbuild(vDesconhecidos) then
      begin
        Logar('ERRO: falha ao reconstruir a IDE. Os pacotes ficaram marcados: ' +
              'abrir a IDE e mandar reconstruir mostra o erro com detalhe.');
        Result := False;
      end;
    end;

    for vInt := 0 to Pred(vInstalar.Count) do
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
    vDesconhecidos.Free;
    vAdd.Free;
    vLinks.Free;
    vFora.Free;
    vInstalar.Free;
    vLista.Free;
  end;
end;

end.
