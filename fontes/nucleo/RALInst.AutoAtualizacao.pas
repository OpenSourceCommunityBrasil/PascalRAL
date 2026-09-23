unit RALInst.AutoAtualizacao;

{$mode ObjFPC}{$H+}

// F12: o instalador se atualiza a partir dos releases do proprio repositorio.
//
// Os releases do instalador tem tag instalador-v<versao> (os do RAL sao v1.0,
// 1.1...). O mais novo que nao e rascunho nem pre-lancamento e comparado com
// VersaoInstalador (RALInst.Versao). Sendo mais novo:
//   1. baixa o binario deste sistema para <exe>.novo, AO LADO do atual (a troca
//      e um rename, e rename entre discos nao existe); confere o tamanho e,
//      havendo SHA256SUMS no release, o hash — download pela metade nunca vira
//      o executavel;
//   2. troca: o Windows nao deixa apagar um .exe em uso, mas deixa renomear —
//      o atual vira <exe>.old e o novo toma o nome; falhando a segunda parte,
//      a primeira e desfeita (nunca ficar sem instalador); fora do Windows o
//      bit de execucao e reposto;
//   3. reinicia, e a proxima execucao apaga o .old.
//
// Falha de rede nao e "esta atualizado": sem internet, com a API sem cota, ou
// com a lista vinda de um cache velho, o resultado e rvNaoVerificou.
//
// No macOS, trocar o binario assim invalida a assinatura: a troca funciona,
// mas ainda nao foi conferida la (§8 do plano).

interface

uses
  Classes, SysUtils, fpjson, RALInst.Processo;

type
  TResultadoVerificacao = (rvAtualizado, rvNovaVersao, rvNaoVerificou);

  { TAtualizacao }

  TAtualizacao = class
  private
    FErro: string;
    FVersaoNova: string;
    FTag: string;
    FURLAsset: string;
    FURLSomas: string;
    FNotas: string;
    FTamanho: int64;
    FLog: TLogLinha;
    procedure Logar(const ALinha: string);
  public
    // consulta os releases do repositorio do instalador
    function Verificar(const AVersaoAtual: string = ''): TResultadoVerificacao;
    // a escolha, separada da rede para os testes: o release mais novo do
    // instalador (nem rascunho, nem pre-lancamento) na lista da API
    function EscolherDe(AReleases: TJSONData; const AVersaoAtual: string): TResultadoVerificacao;
    // baixa o binario deste sistema para <exe>.novo e confere tamanho e hash
    function Baixar(out AArquivo: string; const AExecutavel: string = ''): boolean;
    // o executavel (ParamStr(0) por padrao) passa a ser ANovo; o atual vira .old
    function Trocar(const ANovo: string; const AExecutavel: string = ''): boolean;
    // inicia o executavel de novo; quem chama encerra o processo atual
    function Reiniciar(const AArgumentos: array of string; const AExecutavel: string = ''): boolean;

    property VersaoNova: string read FVersaoNova;
    property Tag: string read FTag;
    property Notas: string read FNotas;
    property URLAsset: string read FURLAsset;
    property Tamanho: int64 read FTamanho;
    property Erro: string read FErro;
    property Log: TLogLinha read FLog write FLog;
  end;

// apaga o .old que uma atualizacao anterior deixou (na abertura)
procedure LimparAtualizacaoAnterior(const AExecutavel: string = '');

implementation

uses
  Process, RALInst.IDE, RALInst.GitHub, RALInst.HTTP, RALInst.SHA256, RALInst.Versao
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

function Executavel(const AExecutavel: string): string;
begin
  if AExecutavel <> '' then
    Result := AExecutavel
  else
    Result := ParamStr(0);
end;

procedure LimparAtualizacaoAnterior(const AExecutavel: string);
var
  vAntigo: string;
begin
  vAntigo := Executavel(AExecutavel) + '.old';
  // pode ainda estar em uso por um instante (o processo antigo saindo): fica
  // para a proxima vez
  if FileExists(vAntigo) then
    SysUtils.DeleteFile(vAntigo);
end;

{ TAtualizacao }

procedure TAtualizacao.Logar(const ALinha: string);
begin
  if Assigned(FLog) then
    FLog(ALinha);
end;

function TAtualizacao.EscolherDe(AReleases: TJSONData; const AVersaoAtual: string): TResultadoVerificacao;
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
  FTamanho := 0;
  if not (AReleases is TJSONArray) then
  begin
    FErro := 'resposta inesperada da API do GitHub';
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
    if not SameText(Copy(vTag, 1, Length(PrefixoTagInstalador)), PrefixoTagInstalador) or
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
      if SameText(vArq.Get('name', ''), AssetDestaPlataforma) then
      begin
        FURLAsset := vArq.Get('browser_download_url', '');
        FTamanho := vArq.Get('size', int64(0));
      end
      else if SameText(vArq.Get('name', ''), 'SHA256SUMS') then
        FURLSomas := vArq.Get('browser_download_url', '');
    end;
  if FURLAsset = '' then
  begin
    FErro := Format('a versão %s não tem binário para este sistema (%s)',
                    [FVersaoNova, AssetDestaPlataforma]);
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
      FErro := 'não foi possível verificar: ' + vRepo.Erro;
      Exit;
    end;
    // a lista veio do cache porque a rede falhou: nao da para dizer que esta
    // atualizado
    if vRepo.Aviso <> '' then
    begin
      FErro := 'não foi possível verificar: ' + vRepo.Aviso;
      Exit;
    end;
    Result := EscolherDe(vJSON, vAtual);
  finally
    vJSON.Free;
    vRepo.Free;
  end;
end;

function TAtualizacao.Baixar(out AArquivo: string; const AExecutavel: string): boolean;
var
  vCli: TClienteHTTP;
  vArq: TFileStream;
  vSomas: TStringList;
  vTexto, vHash, vLinha: string;
  vOk: boolean;
begin
  Result := False;
  FErro := '';
  AArquivo := Executavel(AExecutavel) + '.novo';
  if FURLAsset = '' then
  begin
    FErro := 'nenhuma versão nova escolhida (chame Verificar antes)';
    Exit;
  end;
  // ao lado do atual: a troca e um rename
  try
    vArq := TFileStream.Create(AArquivo, fmCreate);
  except
    on E: Exception do
    begin
      FErro := 'sem permissão de escrita na pasta do instalador (' +
               ExtractFilePath(AArquivo) + '): ' + E.Message;
      Exit;
    end;
  end;
  vCli := TClienteHTTP.Create;
  try
    Logar('baixando ' + FURLAsset);
    try
      vOk := vCli.Obter(FURLAsset, vArq);
    finally
      vArq.Free;
    end;
    if not vOk then
    begin
      FErro := 'o download falhou: ' + vCli.Erro;
      SysUtils.DeleteFile(AArquivo);
      Exit;
    end;
  finally
    vCli.Free;
  end;

  // download pela metade nunca vira o executavel
  vArq := TFileStream.Create(AArquivo, fmOpenRead or fmShareDenyNone);
  try
    vOk := (FTamanho <= 0) or (vArq.Size = FTamanho);
  finally
    vArq.Free;
  end;
  if not vOk then
  begin
    FErro := 'o arquivo baixado não tem o tamanho do release';
    SysUtils.DeleteFile(AArquivo);
    Exit;
  end;

  if FURLSomas <> '' then
  begin
    vCli := TClienteHTTP.Create;
    vSomas := TStringList.Create;
    try
      if not vCli.ObterTexto(FURLSomas, vTexto) then
      begin
        FErro := 'não consegui baixar o SHA256SUMS do release: ' + vCli.Erro;
        SysUtils.DeleteFile(AArquivo);
        Exit;
      end;
      vSomas.Text := vTexto;
      vHash := '';
      // "<hash>  <arquivo>", como o sha256sum escreve
      for vLinha in vSomas do
        if SameText(Trim(Copy(Trim(vLinha), Pos(' ', Trim(vLinha)) + 1, MaxInt)),
                    AssetDestaPlataforma) then
          vHash := LowerCase(Copy(Trim(vLinha), 1, Pos(' ', Trim(vLinha)) - 1));
      if vHash = '' then
      begin
        FErro := 'o SHA256SUMS do release não tem ' + AssetDestaPlataforma;
        SysUtils.DeleteFile(AArquivo);
        Exit;
      end;
      if SHA256Arquivo(AArquivo) <> vHash then
      begin
        FErro := 'o hash do arquivo baixado não confere com o SHA256SUMS do release';
        SysUtils.DeleteFile(AArquivo);
        Exit;
      end;
      Logar('hash conferido: ' + vHash);
    finally
      vSomas.Free;
      vCli.Free;
    end;
  end;

  {$IFDEF UNIX}
  FpChmod(AArquivo, &755);
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
    FErro := 'o arquivo novo não existe: ' + ANovo;
    Exit;
  end;
  // um .old de uma troca anterior que nao pode ser apagado ainda
  if FileExists(vAntigo) and not SysUtils.DeleteFile(vAntigo) then
  begin
    FErro := 'não consegui apagar ' + vAntigo;
    Exit;
  end;
  // em uso, nao se apaga — mas se renomeia
  if not RenameFile(vExe, vAntigo) then
  begin
    FErro := 'não consegui renomear ' + vExe;
    Exit;
  end;
  if not RenameFile(ANovo, vExe) then
  begin
    // nunca ficar sem instalador
    RenameFile(vAntigo, vExe);
    FErro := 'não consegui pôr o arquivo novo no lugar; o instalador continua o de antes';
    Exit;
  end;
  {$IFDEF UNIX}
  FpChmod(vExe, &755);
  {$ENDIF}
  Result := True;
end;

function TAtualizacao.Reiniciar(const AArgumentos: array of string; const AExecutavel: string): boolean;
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
        FErro := 'não consegui reiniciar: ' + E.Message;
    end;
  finally
    vProcesso.Free;
  end;
end;

end.
