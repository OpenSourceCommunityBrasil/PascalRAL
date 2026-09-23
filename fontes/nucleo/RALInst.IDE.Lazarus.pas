unit RALInst.IDE.Lazarus;

{$mode ObjFPC}{$H+}

// Descoberta das instalacoes do Lazarus, em qualquer sistema.
//
// O ponto que o instalador antigo nao via: cada Lazarus tem a sua pasta de
// configuracao (--primary-config-path). Numa maquina com varios, instalar
// pacote sem passar a PCP certa escreve todos no mesmo lugar. A PCP vem, em
// ordem: do lazarus.cfg ao lado do executavel (instalacao secundaria e
// fpcupdeluxe), do layout do fpcupdeluxe (config_lazarus ao lado da pasta
// lazarus) e, por ultimo, do padrao do sistema.

interface

uses
  Classes, SysUtils, RALInst.IDE;

type

  { TBuscaLazarus }

  TBuscaLazarus = class(TBuscaIDE)
  private
    procedure ResolverConfig(AIDE: TIDEInstance);
    procedure ResolverCompilador(AIDE: TIDEInstance);
  protected
    function PastaIgnorada(const ANome: string): boolean; override;
  public
    function InspecionarPasta(const APasta: string): TIDEInstance; override;
    procedure BuscarPadrao(ALista: TIDEList); override;
    // todos os discos (Windows) ou '/' inteiro: lento, so a pedido
    procedure BuscarCompleta(ALista: TIDEList);
    procedure Finalizar(ALista: TIDEList); override;
  end;

const
  {$IFDEF MSWINDOWS}
    LazBuildArquivo = 'lazbuild.exe';
    LazExecArquivo = 'lazarus.exe';
    FPCArquivo = 'fpc.exe';
  {$ELSE}
    LazBuildArquivo = 'lazbuild';
    LazExecArquivo = 'lazarus';
    FPCArquivo = 'fpc';
  {$ENDIF}

// PCP que o Lazarus usa quando nada diz outra coisa
function ConfigPadraoLazarus: string;

implementation

uses
  RegExpr, RALInst.Processo {$IFDEF MSWINDOWS}, FileInfo{$ENDIF};

function ConfigPadraoLazarus: string;
begin
  {$IFDEF MSWINDOWS}
    Result := GetEnvironmentVariable('LOCALAPPDATA');
    if Result = '' then
      Result := GetEnvironmentVariable('USERPROFILE') + '\Local Settings\Application Data';
    Result := IncludeTrailingPathDelimiter(Result) + 'lazarus';
  {$ELSE}
    Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) + '.lazarus';
  {$ENDIF}
  Result := NormalizarPasta(Result);
end;

function LerArquivo(const AArquivo: string): string;
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

// primeiro grupo da expressao no texto, ou vazio
function Extrair(const ATexto, AExpressao: string): string;
var
  vRegex: TRegExpr;
begin
  Result := '';
  vRegex := TRegExpr.Create(AExpressao);
  try
    vRegex.ModifierI := True;
    vRegex.ModifierM := True;
    if vRegex.Exec(ATexto) then
      Result := vRegex.Match[1];
  finally
    vRegex.Free;
  end;
end;

// caminho relativo e relativo a pasta base
function ResolverCaminho(const ACaminho, ABase: string): string;
var
  vCaminho: string;
begin
  vCaminho := Trim(ACaminho);
  if (Length(vCaminho) >= 2) and (vCaminho[1] = '"') and (vCaminho[Length(vCaminho)] = '"') then
    vCaminho := Copy(vCaminho, 2, Length(vCaminho) - 2);
  vCaminho := SetDirSeparators(vCaminho);
  if vCaminho = '' then
    Exit('');
  if (ExtractFileDrive(vCaminho) <> '') or (vCaminho[1] = PathDelim) then
    Result := ExpandFileName(vCaminho)
  else
    Result := ExpandFileName(IncludeTrailingPathDelimiter(ABase) + vCaminho);
end;

// 'fpc -iV' -> '3.2.3'; vazio se o compilador nao respondeu
function VersaoDoFPC(const AFPC: string): string;
var
  vExec: TExecucao;
begin
  Result := '';
  vExec := TExecucao.Create;
  try
    vExec.Executavel := AFPC;
    vExec.Parametros.Add('-iV');
    if vExec.Executar and (vExec.Saida.Count > 0) then
      Result := Extrair(vExec.Saida.Text, '^(\d+\.\d+(\.\d+)?)');
  finally
    vExec.Free;
  end;
end;

function VersaoCurta(const AVersao: string): string;
var
  vPartes: TStringArray;
begin
  // '4.6.0.0' -> '4.6'; '2.2.6' -> '2.2.6'
  vPartes := AVersao.Split(['.']);
  if Length(vPartes) < 2 then
    Exit(AVersao);
  Result := vPartes[0] + '.' + vPartes[1];
  if (Length(vPartes) > 2) and (StrToIntDef(vPartes[2], 0) <> 0) then
    Result := Result + '.' + vPartes[2];
end;

function PastaGravavel(const APasta: string): boolean;
var
  vArquivo: string;
  vHandle: THandle;
begin
  vArquivo := IncludeTrailingPathDelimiter(APasta) + '.ralinst_teste_escrita';
  vHandle := FileCreate(vArquivo);
  Result := vHandle <> THandle(-1);
  if Result then
  begin
    FileClose(vHandle);
    DeleteFile(vArquivo);
  end;
end;

{ TBuscaLazarus }

function TBuscaLazarus.PastaIgnorada(const ANome: string): boolean;
begin
  // pastas enormes do fpcupdeluxe e dos fontes, que nunca sao raiz de Lazarus
  Result := inherited PastaIgnorada(ANome) or
            SameText(ANome, 'fpcsrc') or SameText(ANome, 'fpcbootstrap') or
            SameText(ANome, 'ccr') or SameText(ANome, 'cross') or
            SameText(ANome, 'config_lazarus') or SameText(ANome, 'tmp') or
            SameText(ANome, 'patches') or SameText(ANome, 'fpc') or
            SameText(ANome, 'units') or SameText(ANome, 'fpcpkgconfig') or
            SameText(ANome, 'projects') or SameText(ANome, 'docs') or
            SameText(ANome, 'examples');
end;

procedure TBuscaLazarus.ResolverConfig(AIDE: TIDEInstance);
var
  vCfg, vPCP, vIrma: string;
begin
  // 1) lazarus.cfg ao lado do executavel
  vCfg := LerArquivo(AIDE.RootDir + 'lazarus.cfg');
  if vCfg <> '' then
  begin
    vPCP := Extrair(vCfg, '^\s*--(?:primary-config-path|pcp)\s*=\s*(.+?)\s*$');
    if vPCP <> '' then
    begin
      AIDE.ConfigDir := NormalizarPasta(ResolverCaminho(vPCP, AIDE.RootDir));
      AIDE.ConfigOrigem := 'lazarus.cfg';
      Exit;
    end;
  end;

  // 2) layout do fpcupdeluxe: <base>\lazarus e <base>\config_lazarus
  vIrma := ExtractFilePath(ExcludeTrailingPathDelimiter(AIDE.RootDir)) + 'config_lazarus';
  if DirectoryExists(vIrma) then
  begin
    AIDE.ConfigDir := NormalizarPasta(vIrma);
    AIDE.ConfigOrigem := 'fpcupdeluxe';
    Exit;
  end;

  // 3) padrao do sistema: compartilhado com todo Lazarus que nao diga outra
  AIDE.ConfigDir := ConfigPadraoLazarus;
  AIDE.ConfigOrigem := 'padrão do sistema';
end;

procedure TBuscaLazarus.ResolverCompilador(AIDE: TIDEInstance);
var
  vOpcoes, vFPC, vRaizFPC, vUnits, vLazDir: string;
  vSearch: TSearchRec;
begin
  vOpcoes := LerArquivo(AIDE.ConfigDir + 'environmentoptions.xml');
  vFPC := Extrair(vOpcoes, '<CompilerFilename\s+Value="([^"]*)"');
  if vFPC <> '' then
  begin
    vLazDir := Extrair(vOpcoes, '<LazarusDirectory\s+Value="([^"]*)"');
    vFPC := StringReplace(vFPC, '$(LazarusDir)', ExcludeTrailingPathDelimiter(AIDE.RootDir),
                          [rfReplaceAll, rfIgnoreCase]);
    vFPC := StringReplace(vFPC, '$Path($(CompPath))', '', [rfReplaceAll, rfIgnoreCase]);
    // caminhos relativos no environmentoptions.xml sao relativos a PCP
    vFPC := ResolverCaminho(vFPC, AIDE.ConfigDir);

    // configuracao de outro Lazarus: a PCP e compartilhada e aponta outra
    // pasta; o compilador dela pode nao ser o desta instalacao
    if (vLazDir <> '') and
       not SameFileName(NormalizarPasta(ResolverCaminho(vLazDir, AIDE.ConfigDir)), AIDE.RootDir) then
      AIDE.Avisos.Add('A configuração em ' + AIDE.ConfigDir + ' pertence a outro Lazarus (' +
        ResolverCaminho(vLazDir, AIDE.ConfigDir) + ').');
  end;

  if (vFPC = '') or not FileExists(vFPC) then
  begin
    {$IFDEF MSWINDOWS}
      vFPC := '';
    {$ELSE}
      if FileExists('/usr/bin/fpc') then
        vFPC := '/usr/bin/fpc'
      else if FileExists('/usr/local/bin/fpc') then
        vFPC := '/usr/local/bin/fpc'
      else
        vFPC := '';
    {$ENDIF}
  end;

  AIDE.CompilerFile := vFPC;
  if vFPC = '' then
  begin
    AIDE.Avisos.Add('Compilador FPC não identificado: o lazbuild vai usar o que estiver configurado no ambiente.');
    Exit;
  end;

  // versao do FPC: o caminho diz no instalador oficial (fpc\3.2.2\bin); no
  // fpcupdeluxe nao, e so perguntando ao compilador. Ela decide a versao das
  // dependencias (o Zeos 8.0 nao compila no FPC 3.2.3)
  AIDE.VersaoCompilador := Extrair(vFPC, '[\\/](\d+\.\d+\.\d+)[\\/]');
  if AIDE.VersaoCompilador = '' then
    AIDE.VersaoCompilador := VersaoDoFPC(vFPC);

  // plataformas: units compiladas de cada alvo, na raiz do FPC
  // (<raiz>\bin\<alvo>\fpc.exe -> <raiz>\units\<alvo>)
  vRaizFPC := ExtractFilePath(ExcludeTrailingPathDelimiter(
                ExtractFilePath(ExcludeTrailingPathDelimiter(ExtractFilePath(vFPC)))));
  vUnits := vRaizFPC + 'units' + PathDelim;
  {$IFNDEF MSWINDOWS}
    if not DirectoryExists(vUnits) and (AIDE.VersaoCompilador <> '') then
      vUnits := '/usr/lib/fpc/' + AIDE.VersaoCompilador + '/units/';
  {$ENDIF}
  if FindFirst(vUnits + '*', faDirectory, vSearch) = 0 then
  try
    repeat
      if ((vSearch.Attr and faDirectory) <> 0) and (Pos('-', vSearch.Name) > 1) then
        AIDE.Plataformas.Add(LowerCase(vSearch.Name));
    until FindNext(vSearch) <> 0;
  finally
    FindClose(vSearch);
  end;
end;

function TBuscaLazarus.InspecionarPasta(const APasta: string): TIDEInstance;
var
  vRaiz, vVersao, vExe: string;
  {$IFDEF MSWINDOWS}
    vInfo: TFileVersionInfo;
  {$ENDIF}
begin
  Result := nil;
  vRaiz := NormalizarPasta(APasta);

  if not FileExists(vRaiz + LazBuildArquivo) then
    Exit;

  vExe := '';
  if FileExists(vRaiz + LazExecArquivo) then
    vExe := vRaiz + LazExecArquivo
  {$IFDEF DARWIN}
  else if DirectoryExists(vRaiz + 'lazarus.app') then
    vExe := vRaiz + 'lazarus.app'
  {$ENDIF}
  ;

  // lazbuild solto, sem a arvore do Lazarus, nao instala pacote
  if not DirectoryExists(vRaiz + 'lcl') and not DirectoryExists(vRaiz + 'packager') then
    Exit;

  // versao: lazversion.pas (3.x em diante), ide/version.inc (antigos) e,
  // no Windows, o VerInfo do executavel
  vVersao := Extrair(LerArquivo(vRaiz + 'components' + PathDelim + 'lazutils' + PathDelim +
                                'lazversion.pas'),
                     'laz_version\s*=\s*''([^'']+)''');
  if vVersao = '' then
    vVersao := Extrair(LerArquivo(vRaiz + 'ide' + PathDelim + 'version.inc'), '''([^'']+)''');
  {$IFDEF MSWINDOWS}
    if (vVersao = '') and (vExe <> '') then
    begin
      vInfo := TFileVersionInfo.Create(nil);
      try
        try
          vInfo.FileName := vExe;
          vInfo.ReadFileInfo;
          vVersao := vInfo.VersionStrings.Values['FileVersion'];
        except
          vVersao := '';
        end;
      finally
        vInfo.Free;
      end;
    end;
  {$ENDIF}

  Result := TIDEInstance.Create(tiLazarus);
  Result.RootDir := vRaiz;
  Result.ExeFile := vExe;
  Result.BuildFile := vRaiz + LazBuildArquivo;
  Result.Versao := vVersao;
  if vVersao <> '' then
    Result.Nome := 'Lazarus ' + VersaoCurta(vVersao)
  else
  begin
    Result.Nome := 'Lazarus (versão desconhecida)';
    Result.Versao := '0';
  end;

  Result.Capacidades := [ciCompilar];
  if vExe <> '' then
    Result.Capacidades := Result.Capacidades + [ciInstalarNaIDE];

  ResolverConfig(Result);
  ResolverCompilador(Result);

  if not PastaGravavel(vRaiz) then
    Result.Avisos.Add('Sem permissão de escrita na pasta do Lazarus: reconstruir a IDE vai exigir executar como administrador.');
end;

procedure TBuscaLazarus.BuscarPadrao(ALista: TIDEList);
var
  vInt: integer;
  {$IFDEF MSWINDOWS}
    vBase: string;
  {$ELSE}
    vHome: string;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    BuscarEm(ALista, 'C:\lazarus', 1);
    BuscarEm(ALista, 'C:\fpcupdeluxe', 3);
    for vBase in TStringArray.Create(GetEnvironmentVariable('ProgramFiles'),
                                     GetEnvironmentVariable('ProgramFiles(x86)'),
                                     GetEnvironmentVariable('LOCALAPPDATA') + '\Programs',
                                     GetEnvironmentVariable('USERPROFILE')) do
      if vBase <> '' then
      begin
        BuscarEm(ALista, IncludeTrailingPathDelimiter(vBase) + 'lazarus', 1);
        BuscarEm(ALista, IncludeTrailingPathDelimiter(vBase) + 'fpcupdeluxe', 3);
      end;
  {$ELSE}
    // varrer '/' inteiro atravessa /proc, /sys e montagens de rede: so as
    // raizes onde pacote, instalador oficial e fpcupdeluxe deixam o Lazarus
    BuscarEm(ALista, '/usr/lib/lazarus', 2);
    BuscarEm(ALista, '/usr/share/lazarus', 2);
    BuscarEm(ALista, '/usr/local/lib/lazarus', 2);
    BuscarEm(ALista, '/usr/local/share/lazarus', 2);
    BuscarEm(ALista, '/opt', 4);
    {$IFDEF DARWIN}
      BuscarEm(ALista, '/Applications', 3);
      BuscarEm(ALista, '/Developer', 3);
    {$ENDIF}
    vHome := GetEnvironmentVariable('HOME');
    if vHome <> '' then
      BuscarEm(ALista, vHome, 4);
  {$ENDIF}

  for vInt := 0 to Pred(RaizesExtras.Count) do
    BuscarEm(ALista, RaizesExtras[vInt], 4);
end;

procedure TBuscaLazarus.BuscarCompleta(ALista: TIDEList);
{$IFDEF MSWINDOWS}
var
  vUnidade: char;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    for vUnidade := 'C' to 'Z' do
      if DirectoryExists(vUnidade + ':\') then
        BuscarEm(ALista, vUnidade + ':\', -1);
  {$ELSE}
    BuscarEm(ALista, '/', -1);
  {$ENDIF}
end;

procedure TBuscaLazarus.Finalizar(ALista: TIDEList);
var
  vInt, vOutra: integer;
begin
  for vInt := 0 to Pred(ALista.Count) do
  begin
    if ALista[vInt].Tipo <> tiLazarus then
      Continue;
    for vOutra := 0 to Pred(ALista.Count) do
      if (vOutra <> vInt) and (ALista[vOutra].Tipo = tiLazarus) and
         SameFileName(ALista[vOutra].ConfigDir, ALista[vInt].ConfigDir) then
        ALista[vInt].Avisos.Add('Divide a pasta de configuração ' + ALista[vInt].ConfigDir +
          ' com ' + ALista[vOutra].RootDir + ': pacotes instalados numa entram na outra.');
  end;

  inherited Finalizar(ALista);
end;

end.
