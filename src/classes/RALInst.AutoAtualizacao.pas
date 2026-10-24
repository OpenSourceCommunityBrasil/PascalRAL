/// The installer updates itself from its own repository releases. Installer
/// releases are tagged instalador-v<version> (RAL's are v1.0, 1.1...). The
/// newest that is neither a draft nor a pre-release is compared with
/// VersaoInstalador. When newer:
///   1. downloads this system's binary into <exe>.novo, BESIDE the current one
///      (the swap is a rename, and there is no rename across disks); checks the
///      size and, when the release has SHA256SUMS, the hash: a half download
///      never becomes the executable;
///   2. swaps: Windows does not delete an .exe in use but lets it be renamed;
///      the current becomes <exe>.old and the new one takes its name; when the
///      second part fails the first is undone (never be left without an
///      installer); outside Windows the executable bit is set again;
///   3. restarts, and the next run deletes the .old.
/// A network failure is not "up to date": without internet, with the API out
/// of quota, or with the list from an old cache, the result is rvNaoVerificou.
/// On macOS, swapping the binary this way invalidates the signature.
unit RALInst.AutoAtualizacao;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson,
  RALInst.Processo;

type
  /// Result of a check for a new version.
  TResultadoVerificacao = (rvAtualizado, rvNovaVersao, rvNaoVerificou);

  /// Checks, downloads and swaps in a new installer version. F21: a release
  /// carries two programs for this system, the installer (or the command line)
  /// and the project migrator, and both are swapped: a release without the
  /// migrator of this system does not count as a new version.
  TAtualizacao = class
  private
    FArquivoMigrador: string;
    FErro: string;
    FLog: TLogLinha;
    FMigradorPendente: boolean;
    FNotas: string;
    FPrograma: string;
    FTag: string;
    FTamanho: int64;
    FTamanhoMigrador: int64;
    FURLAsset: string;
    FURLMigrador: string;
    FURLSomas: string;
    FVersaoNova: string;
    /// Downloads one asset into ADestino and checks size and hash
    function BaixarAsset(const AURL, AAsset: string; ATamanho: int64;
      const ADestino: string): boolean;
    function GetArquivoMigrador: string;
    /// Passes the line to Log when assigned
    procedure Logar(const ALinha: string);
  public
    constructor Create(const APrograma: string = '');
    /// Downloads this system's binary into <exe>.novo and checks size and hash
    function Baixar(out AArquivo: string; const AExecutavel: string = ''): boolean;
    /// F21: downloads this system's migrator into <migrator>.novo, checked
    function BaixarMigrador(out AArquivo: string): boolean;
    /// The choice, apart from the network for the tests: the newest installer
    /// release (neither draft nor pre-release) in the API list
    function EscolherDe(AReleases: TJSONData;
      const AVersaoAtual: string): TResultadoVerificacao;
    /// Starts the executable again; the caller ends the current process
    function Reiniciar(const AArgumentos: array of string;
      const AExecutavel: string = ''): boolean;
    /// The executable (ParamStr(0) by default) becomes ANovo; the current one
    /// becomes .old
    function Trocar(const ANovo: string; const AExecutavel: string = ''): boolean;
    /// F21: the migrator becomes ANovo. An open migrator cannot be swapped:
    /// the .novo stays and the next opening of the installer swaps it
    /// (MigradorPendente); False then, with the reason in Erro
    function TrocarMigrador(const ANovo: string): boolean;
    /// Queries the releases of the installer repository
    function Verificar(const AVersaoAtual: string = ''): TResultadoVerificacao;

    /// Where the migrator is (or goes) beside this program; the tests point it
    /// elsewhere
    property ArquivoMigrador: string read GetArquivoMigrador write FArquivoMigrador;
    property Erro: string read FErro;
    property Log: TLogLinha read FLog write FLog;
    /// The migrator was open: its new version waits beside it for the next
    /// opening of the installer
    property MigradorPendente: boolean read FMigradorPendente;
    property Notas: string read FNotas;
    /// 'ralinstaller' (the screens) or 'ralcli': which binary of the release
    /// replaces this program
    property Programa: string read FPrograma;
    property Tag: string read FTag;
    property Tamanho: int64 read FTamanho;
    property URLAsset: string read FURLAsset;
    property VersaoNova: string read FVersaoNova;
  end;

/// Deletes the .old a previous update left (at startup), and swaps in a
/// migrator that was open then (its .novo waits beside it)
procedure LimparAtualizacaoAnterior(const AExecutavel: string = '';
  const AMigrador: string = '');

implementation

uses
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  Process,
  RALInst.GitHub, RALInst.HTTP, RALInst.IDE, RALInst.Mensagens, RALInst.Migrador,
  RALInst.SHA256, RALInst.Versao;

function Executavel(const AExecutavel: string): string;
begin
  if AExecutavel <> '' then
    Result := AExecutavel
  else
    Result := ParamStr(0);
end;

procedure LimparAtualizacaoAnterior(const AExecutavel, AMigrador: string);
var
  vAntigo, vMigrador: string;
  vAtu: TAtualizacao;
begin
  vAntigo := Executavel(AExecutavel) + '.old';
  // pode ainda estar em uso por um instante (o processo antigo saindo): fica
  // para a proxima vez
  if FileExists(vAntigo) then
    SysUtils.DeleteFile(vAntigo);
  // F21: o migrador que estava aberto na atualizacao: a versao nova esperou
  vMigrador := AMigrador;
  if vMigrador = '' then
  begin
    vMigrador := RALInst.Migrador.ArquivoMigrador;
    if vMigrador = '' then
      vMigrador := DestinoMigrador;
  end;
  if FileExists(vMigrador + '.old') then
    SysUtils.DeleteFile(vMigrador + '.old');
  if FileExists(vMigrador + '.novo') then
  begin
    vAtu := TAtualizacao.Create;
    try
      vAtu.ArquivoMigrador := vMigrador;
      vAtu.TrocarMigrador(vMigrador + '.novo');
    finally
      vAtu.Free;
    end;
  end;
end;

{ TAtualizacao }

constructor TAtualizacao.Create(const APrograma: string);
begin
  inherited Create;
  FPrograma := APrograma;
  if FPrograma = '' then
    FPrograma := ProgramaInstalador;
end;

function TAtualizacao.GetArquivoMigrador: string;
begin
  Result := FArquivoMigrador;
  if Result = '' then
    Result := RALInst.Migrador.ArquivoMigrador;
  if Result = '' then
    Result := DestinoMigrador;
end;

procedure TAtualizacao.Logar(const ALinha: string);
begin
  if Assigned(FLog) then
    FLog(ALinha);
end;

function TAtualizacao.EscolherDe(AReleases: TJSONData;
  const AVersaoAtual: string): TResultadoVerificacao;
var
  vInt, vAsset: integer;
  vItem, vMelhor, vArq: TJSONObject;
  vAssets: TJSONArray;
  vTag, vVersao, vMelhorVersao: string;
begin
  Result := rvNaoVerificou;
  FVersaoNova := '';
  FTag := '';
  FURLAsset := '';
  FURLSomas := '';
  FURLMigrador := '';
  FTamanho := 0;
  FTamanhoMigrador := 0;
  if not (AReleases is TJSONArray) then
  begin
    FErro := emGitHubRespostaInesperada;
    Exit;
  end;
  vMelhor := nil;
  vMelhorVersao := '';
  for vInt := 0 to Pred(TJSONArray(AReleases).Count) do
  begin
    if not (TJSONArray(AReleases).Items[vInt] is TJSONObject) then
      Continue;
    vItem := TJSONArray(AReleases).Objects[vInt];
    vTag := vItem.Get('tag_name', '');
    if not SameText(Copy(vTag, 1, Length(PrefixoTagInstalador)),
                    PrefixoTagInstalador) or
       vItem.Get('draft', False) or vItem.Get('prerelease', False) then
      Continue;
    vVersao := Copy(vTag, Length(PrefixoTagInstalador) + 1, MaxInt);
    if (vMelhor = nil) or (CompararVersoes(vVersao, vMelhorVersao) > 0) then
    begin
      vMelhor := vItem;
      vMelhorVersao := vVersao;
    end;
  end;

  // nenhum release do instalador, ou nenhum mais novo: esta atualizado
  if (vMelhor = nil) or (CompararVersoes(vMelhorVersao, AVersaoAtual) <= 0) then
    Exit(rvAtualizado);

  FVersaoNova := vMelhorVersao;
  FTag := vMelhor.Get('tag_name', '');
  FNotas := vMelhor.Get('body', '');
  vAssets := vMelhor.Get('assets', TJSONArray(nil));
  if vAssets <> nil then
    for vAsset := 0 to Pred(vAssets.Count) do
    begin
      vArq := vAssets.Objects[vAsset];
      if SameText(vArq.Get('name', ''), AssetDestaPlataforma(FPrograma)) then
      begin
        FURLAsset := vArq.Get('browser_download_url', '');
        FTamanho := vArq.Get('size', int64(0));
      end
      else if SameText(vArq.Get('name', ''), AssetDestaPlataforma(ProgramaMigrador)) then
      begin
        FURLMigrador := vArq.Get('browser_download_url', '');
        FTamanhoMigrador := vArq.Get('size', int64(0));
      end
      else if SameText(vArq.Get('name', ''), 'SHA256SUMS') then
        FURLSomas := vArq.Get('browser_download_url', '');
    end;
  if FURLAsset = '' then
  begin
    FErro := Format(emAtuSemBinario,
                    [FVersaoNova, AssetDestaPlataforma(FPrograma)]);
    Exit(rvNaoVerificou);
  end;
  // F21: o migrador sai no mesmo release; sem ele a versao nova nao conta
  if FURLMigrador = '' then
  begin
    FErro := Format(emAtuSemMigrador,
                    [FVersaoNova, AssetDestaPlataforma(ProgramaMigrador)]);
    Exit(rvNaoVerificou);
  end;
  Result := rvNovaVersao;
end;

function TAtualizacao.Verificar(const AVersaoAtual: string): TResultadoVerificacao;
var
  vRepo: TRepoGitHub;
  vJSON: TJSONData;
  vAtual: string;
begin
  Result := rvNaoVerificou;
  FErro := '';
  vAtual := AVersaoAtual;
  if vAtual = '' then
    vAtual := VersaoInstalador;
  vRepo := TRepoGitHub.Create(DonoInstalador, RepoInstalador);
  vJSON := nil;
  try
    if not vRepo.ObterAPI('releases?per_page=100', ValidadeCache, vJSON) then
    begin
      FErro := Format(emAtuVerificar, [vRepo.Erro]);
      Exit;
    end;
    // a lista veio do cache porque a rede falhou: nao da para dizer que esta
    // atualizado
    if vRepo.Aviso <> '' then
    begin
      FErro := Format(emAtuVerificar, [vRepo.Aviso]);
      Exit;
    end;
    Result := EscolherDe(vJSON, vAtual);
  finally
    vJSON.Free;
    vRepo.Free;
  end;
end;

function TAtualizacao.Baixar(out AArquivo: string; const AExecutavel: string): boolean;
begin
  FErro := '';
  AArquivo := Executavel(AExecutavel) + '.novo';
  if FURLAsset = '' then
  begin
    FErro := emAtuNenhumaVersao;
    Exit(False);
  end;
  Result := BaixarAsset(FURLAsset, AssetDestaPlataforma(FPrograma), FTamanho, AArquivo);
end;

function TAtualizacao.BaixarMigrador(out AArquivo: string): boolean;
begin
  FErro := '';
  AArquivo := ArquivoMigrador + '.novo';
  if FURLMigrador = '' then
  begin
    FErro := emAtuNenhumaVersao;
    Exit(False);
  end;
  Result := BaixarAsset(FURLMigrador, AssetDestaPlataforma(ProgramaMigrador),
                        FTamanhoMigrador, AArquivo);
end;

function TAtualizacao.BaixarAsset(const AURL, AAsset: string; ATamanho: int64;
  const ADestino: string): boolean;
var
  vCli: TClienteHTTP;
  vArq: TFileStream;
  vSomas: TStringList;
  vTexto, vHash, vLinha, vArquivo: string;
  vOk: boolean;
begin
  Result := False;
  FErro := '';
  vArquivo := ADestino;
  // ao lado do atual: a troca e um rename
  try
    vArq := TFileStream.Create(vArquivo, fmCreate);
  except
    on E: Exception do
    begin
      FErro := Format(emAtuSemEscrita, [ExtractFilePath(vArquivo), E.Message]);
      Exit;
    end;
  end;
  vCli := TClienteHTTP.Create;
  try
    Logar(Format(cmBaixandoArquivo, [AURL]));
    try
      vOk := vCli.Obter(AURL, vArq);
    finally
      vArq.Free;
    end;
    if not vOk then
    begin
      FErro := Format(emAtuDownload, [vCli.Erro]);
      SysUtils.DeleteFile(vArquivo);
      Exit;
    end;
  finally
    vCli.Free;
  end;

  // download pela metade nunca vira o executavel
  vArq := TFileStream.Create(vArquivo, fmOpenRead or fmShareDenyNone);
  try
    vOk := (ATamanho <= 0) or (vArq.Size = ATamanho);
  finally
    vArq.Free;
  end;
  if not vOk then
  begin
    FErro := emAtuTamanho;
    SysUtils.DeleteFile(vArquivo);
    Exit;
  end;

  if FURLSomas <> '' then
  begin
    vCli := TClienteHTTP.Create;
    vSomas := TStringList.Create;
    try
      if not vCli.ObterTexto(FURLSomas, vTexto) then
      begin
        FErro := Format(emAtuSomas, [vCli.Erro]);
        SysUtils.DeleteFile(vArquivo);
        Exit;
      end;
      vSomas.Text := vTexto;
      vHash := '';
      // "<hash>  <arquivo>", como o sha256sum escreve
      for vLinha in vSomas do
        if SameText(Trim(Copy(Trim(vLinha), Pos(' ', Trim(vLinha)) + 1, MaxInt)),
                    AAsset) then
          vHash := LowerCase(Copy(Trim(vLinha), 1, Pos(' ', Trim(vLinha)) - 1));
      if vHash = '' then
      begin
        FErro := Format(emAtuSomasSemAsset, [AAsset]);
        SysUtils.DeleteFile(vArquivo);
        Exit;
      end;
      if SHA256Arquivo(vArquivo) <> vHash then
      begin
        FErro := emAtuHash;
        SysUtils.DeleteFile(vArquivo);
        Exit;
      end;
      Logar(Format(cmHashConferido, [vHash]));
    finally
      vSomas.Free;
      vCli.Free;
    end;
  end;

  {$IFDEF UNIX}
  FpChmod(vArquivo, &755);
  {$ENDIF}
  Result := True;
end;

function TAtualizacao.Trocar(const ANovo: string; const AExecutavel: string): boolean;
var
  vExe, vAntigo: string;
begin
  Result := False;
  FErro := '';
  vExe := Executavel(AExecutavel);
  vAntigo := vExe + '.old';
  if not FileExists(ANovo) then
  begin
    FErro := Format(emAtuNovoNaoExiste, [ANovo]);
    Exit;
  end;
  // um .old de uma troca anterior que nao pode ser apagado ainda
  if FileExists(vAntigo) and not SysUtils.DeleteFile(vAntigo) then
  begin
    FErro := Format(emAtuApagar, [vAntigo]);
    Exit;
  end;
  // em uso, nao se apaga — mas se renomeia
  if not RenameFile(vExe, vAntigo) then
  begin
    FErro := Format(emAtuRenomear, [vExe]);
    Exit;
  end;
  if not RenameFile(ANovo, vExe) then
  begin
    // nunca ficar sem instalador
    RenameFile(vAntigo, vExe);
    FErro := emAtuTrocar;
    Exit;
  end;
  {$IFDEF UNIX}
  FpChmod(vExe, &755);
  {$ENDIF}
  Result := True;
end;

function TAtualizacao.TrocarMigrador(const ANovo: string): boolean;
var
  vMigrador: string;
begin
  Result := False;
  FErro := '';
  FMigradorPendente := False;
  vMigrador := ArquivoMigrador;
  if not FileExists(ANovo) then
  begin
    FErro := Format(emAtuNovoNaoExiste, [ANovo]);
    Exit;
  end;
  // o primeiro migrador desta pasta: nada a trocar
  if not FileExists(vMigrador) then
  begin
    Result := RenameFile(ANovo, vMigrador);
    if not Result then
      FErro := emAtuTrocar
    {$IFDEF UNIX}
    else
      FpChmod(vMigrador, &755)
    {$ENDIF};
    Exit;
  end;
  // aberto, nao se troca: o .novo espera a proxima abertura do instalador
  if ProgramaEmExecucao([ExtractFileName(vMigrador)], ExtractFilePath(vMigrador)) or
     not Trocar(ANovo, vMigrador) then
  begin
    FMigradorPendente := FileExists(ANovo);
    FErro := Format(wmMigradorTrocaDepois, [ExtractFileName(vMigrador)]);
    Exit;
  end;
  Result := True;
end;

function TAtualizacao.Reiniciar(const AArgumentos: array of string;
  const AExecutavel: string): boolean;
var
  vProcesso: TProcess;
  vInt: integer;
begin
  Result := False;
  vProcesso := TProcess.Create(nil);
  try
    vProcesso.Executable := Executavel(AExecutavel);
    for vInt := Low(AArgumentos) to High(AArgumentos) do
      vProcesso.Parameters.Add(AArgumentos[vInt]);
    vProcesso.Options := [];
    try
      vProcesso.Execute;
      Result := True;
    except
      on E: Exception do
        FErro := Format(emAtuReiniciar, [E.Message]);
    end;
  finally
    vProcesso.Free;
  end;
end;

end.
