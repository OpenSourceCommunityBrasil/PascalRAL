program atualizacao;

{$mode ObjFPC}{$H+}

// F12: testes da auto-atualizacao sem publicar release nenhum.
//
//   atualizacao            SHA-256, escolha do release (lista montada), o
//                          download com tamanho e hash (servidor HTTP local)
//                          e a troca do executavel (arquivos numa pasta temporaria)
//   atualizacao --github   consulta os releases de verdade e diz o que acharia

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, fpjson, jsonparser, ssockets, RALInst.SHA256, RALInst.AutoAtualizacao,
  RALInst.Versao
  {$IFDEF MSWINDOWS}, Windows{$ENDIF};

const
  PortaTeste = 18321;

type
  // um servidor HTTP de mentira, so para o teste: serve os arquivos de uma
  // pasta, como o GitHub serve os assets do release. So em 127.0.0.1:
  // escutar em todas as interfaces faz o firewall do Windows perguntar
  TServidor = class(TThread)
  private
    FServidor: TInetServer;
    FPasta: string;
    procedure Conexao(Sender: TObject; AConexao: TSocketStream);
  protected
    procedure Execute; override;
  public
    constructor Create(const APasta: string);
  end;

constructor TServidor.Create(const APasta: string);
begin
  FPasta := APasta;
  inherited Create(False);
  FreeOnTerminate := False;
end;

procedure TServidor.Conexao(Sender: TObject; AConexao: TSocketStream);
var
  vPedido, vLinha, vArquivo, vCabecalho: string;
  vBuf: array[0..4095] of char;
  vLidos: integer;
  vArq: TFileStream;
begin
  try
    // o cabecalho inteiro chega de uma vez num pedido GET pequeno
    vLidos := AConexao.Read(vBuf, SizeOf(vBuf));
    SetString(vPedido, vBuf, vLidos);
    vLinha := Copy(vPedido, 1, Pos(#13, vPedido + #13) - 1);   // GET /arquivo HTTP/1.1
    vArquivo := Copy(vLinha, Pos(' ', vLinha) + 1, MaxInt);
    vArquivo := Copy(vArquivo, 1, Pos(' ', vArquivo + ' ') - 1);
    vArquivo := FPasta + ExtractFileName(StringReplace(vArquivo, '/', PathDelim, [rfReplaceAll]));
    if FileExists(vArquivo) then
    begin
      vArq := TFileStream.Create(vArquivo, fmOpenRead or fmShareDenyNone);
      try
        vCabecalho := 'HTTP/1.1 200 OK'#13#10'Content-Length: ' + IntToStr(vArq.Size) +
                      #13#10'Content-Type: application/octet-stream'#13#10'Connection: close'#13#10#13#10;
        AConexao.WriteBuffer(vCabecalho[1], Length(vCabecalho));
        AConexao.CopyFrom(vArq, 0);
      finally
        vArq.Free;
      end;
    end
    else
    begin
      vCabecalho := 'HTTP/1.1 404 Not Found'#13#10'Content-Length: 0'#13#10'Connection: close'#13#10#13#10;
      AConexao.WriteBuffer(vCabecalho[1], Length(vCabecalho));
    end;
  finally
    AConexao.Free;
  end;
end;

procedure TServidor.Execute;
begin
  try
    FServidor := TInetServer.Create('127.0.0.1', PortaTeste);
    FServidor.OnConnect := @Conexao;
    FServidor.StartAccepting;
  except
    // porta ocupada: o teste de download diz que pulou
  end;
end;

var
  GFalhas: integer = 0;

procedure Conferir(ACondicao: boolean; const ADescricao: string);
begin
  if ACondicao then
    WriteLn('  ok    ', ADescricao)
  else
  begin
    WriteLn('  FALHA ', ADescricao);
    Inc(GFalhas);
  end;
end;

procedure Gravar(const AArquivo, ATexto: string);
var
  vLista: TStringList;
begin
  vLista := TStringList.Create;
  try
    vLista.Text := ATexto;
    vLista.SaveToFile(AArquivo);
  finally
    vLista.Free;
  end;
end;

function Ler(const AArquivo: string): string;
var
  vLista: TStringList;
begin
  vLista := TStringList.Create;
  try
    vLista.LoadFromFile(AArquivo);
    Result := vLista.Text;
  finally
    vLista.Free;
  end;
end;

function URLArquivo(const AArquivo: string): string;
begin
  Result := Format('http://127.0.0.1:%d/%s', [PortaTeste, ExtractFileName(AArquivo)]);
end;

function Release(const ATag: string; ADraft, APre: boolean; const AAssets: string): string;
begin
  Result := Format('{ "tag_name": "%s", "draft": %s, "prerelease": %s, "body": "notas %s", ' +
                   '"assets": [ %s ] }',
                   [ATag, BoolToStr(ADraft, 'true', 'false'), BoolToStr(APre, 'true', 'false'),
                    ATag, AAssets]);
end;

function Asset(const ANome, AURL: string; ATamanho: int64): string;
begin
  Result := Format('{ "name": "%s", "browser_download_url": "%s", "size": %d }',
                   [ANome, AURL, ATamanho]);
end;

procedure TestesSHA256;
var
  vMilhao: RawByteString;
begin
  WriteLn('SHA-256 (vetores do FIPS 180-4):');
  Conferir(SHA256Texto('') = 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855', 'vazio');
  Conferir(SHA256Texto('abc') = 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad', 'abc');
  Conferir(SHA256Texto('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq') =
           '248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1', '448 bits (dois blocos)');
  SetLength(vMilhao, 1000000);
  FillChar(vMilhao[1], 1000000, Ord('a'));
  Conferir(SHA256Texto(vMilhao) = 'cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0',
           'um milhão de "a"');
end;

procedure TestesEscolha;
var
  vAtu: TAtualizacao;
  vJSON: TJSONData;
  vLista: string;
  vOk: boolean;
begin
  WriteLn('escolha do release (asset deste sistema: ', AssetDestaPlataforma, '):');
  vLista := '[' +
    Release('1.1', False, False, Asset(AssetDestaPlataforma, 'http://x/ral', 10)) + ',' +
    Release('instalador-v0.9.0', False, False, Asset(AssetDestaPlataforma, 'http://x/090', 10)) + ',' +
    Release('instalador-v0.9.5', False, False, Asset(AssetDestaPlataforma, 'http://x/095', 20) + ',' +
            Asset('SHA256SUMS', 'http://x/somas', 1)) + ',' +
    Release('instalador-v0.10.0', False, True, Asset(AssetDestaPlataforma, 'http://x/0100pre', 30)) + ',' +
    Release('instalador-v0.11.0', True, False, Asset(AssetDestaPlataforma, 'http://x/0110draft', 40)) + ']';
  vJSON := GetJSON(vLista);
  vAtu := TAtualizacao.Create;
  try
    Conferir(vAtu.EscolherDe(vJSON, '0.9.0') = rvNovaVersao, 'da 0.9.0 ha versao nova');
    Conferir(vAtu.VersaoNova = '0.9.5', 'a 0.9.5: pre-lancamento, rascunho e tag do RAL ficam de fora (' +
             vAtu.VersaoNova + ')');
    Conferir(vAtu.URLAsset = 'http://x/095', 'o binario deste sistema');
    Conferir(vAtu.Tamanho = 20, 'com o tamanho');
    Conferir(vAtu.EscolherDe(vJSON, '0.9.5') = rvAtualizado, 'na 0.9.5 esta atualizado');
    Conferir(vAtu.EscolherDe(vJSON, '0.10.0') = rvAtualizado, 'uma versao mais nova que a publicada tambem');
  finally
    vAtu.Free;
    vJSON.Free;
  end;

  vJSON := GetJSON('[' + Release('instalador-v1.0.0', False, False, Asset('outro_sistema', 'http://x/o', 5)) + ']');
  vAtu := TAtualizacao.Create;
  try
    vOk := vAtu.EscolherDe(vJSON, '0.9.0') = rvNaoVerificou;
    Conferir(vOk, 'versao nova sem binario deste sistema nao vira "atualizado": ' + vAtu.Erro);
  finally
    vAtu.Free;
    vJSON.Free;
  end;

  vJSON := GetJSON('[]');
  vAtu := TAtualizacao.Create;
  try
    Conferir(vAtu.EscolherDe(vJSON, '0.9.0') = rvAtualizado, 'nenhum release do instalador: atualizado');
  finally
    vAtu.Free;
    vJSON.Free;
  end;
end;

procedure TestesDownloadETroca;
var
  vPasta, vExe, vNovo, vBinario, vSomas, vArquivo: string;
  vAtu: TAtualizacao;
  vJSON: TJSONData;
  vTamanho: int64;
  vBusca: TSearchRec;
  vOk: boolean;

  function Lista(const ASomas: string): TJSONData;
  var
    vAssets: string;
  begin
    vAssets := Asset(AssetDestaPlataforma, URLArquivo(vBinario), vTamanho);
    if ASomas <> '' then
      vAssets := vAssets + ',' + Asset('SHA256SUMS', URLArquivo(ASomas), 1);
    Result := GetJSON('[' + Release('instalador-v9.9.9', False, False, vAssets) + ']');
  end;

begin
  WriteLn('download (servidor local), tamanho, hash e troca:');
  vPasta := IncludeTrailingPathDelimiter(GetTempDir) + 'ralinst-atualizacao-teste' + PathDelim;
  ForceDirectories(vPasta);
  TServidor.Create(vPasta);
  Sleep(300);
  vExe := vPasta + 'instalador_de_teste.bin';
  vBinario := vPasta + 'publicado.bin';
  vSomas := vPasta + 'SHA256SUMS';
  Gravar(vExe, 'versao velha');
  Gravar(vBinario, 'versao nova do instalador');
  FindFirst(vBinario, faAnyFile, vBusca);
  vTamanho := vBusca.Size;
  SysUtils.FindClose(vBusca);

  // hash certo
  Gravar(vSomas, SHA256Arquivo(vBinario) + '  ' + AssetDestaPlataforma + LineEnding +
                 '0000  outro_arquivo');
  vAtu := TAtualizacao.Create;
  vJSON := Lista(vSomas);
  try
    vAtu.EscolherDe(vJSON, '0.9.0');
    if not vAtu.Baixar(vNovo, vExe) and (Pos('download', vAtu.Erro) > 0) then
    begin
      WriteLn('  --    o servidor de teste não respondeu; download pulado (', vAtu.Erro, ')');
      Exit;
    end;
    Conferir(FileExists(vNovo) and (Ler(vNovo) = Ler(vBinario)), 'baixado para <exe>.novo e conferido');
    Conferir(vAtu.Trocar(vNovo, vExe), 'trocado ' + vAtu.Erro);
    Conferir(Ler(vExe) = Ler(vBinario), 'o executavel agora e o novo');
    Conferir(FileExists(vExe + '.old') and (Ler(vExe + '.old') = 'versao velha' + LineEnding),
             'o antigo ficou como .old');
    LimparAtualizacaoAnterior(vExe);
    Conferir(not FileExists(vExe + '.old'), 'a proxima abertura apaga o .old');
  finally
    vJSON.Free;
    vAtu.Free;
  end;

  // hash errado: nada muda
  Gravar(vExe, 'versao velha');
  Gravar(vSomas, StringOfChar('a', 64) + '  ' + AssetDestaPlataforma);
  vAtu := TAtualizacao.Create;
  vJSON := Lista(vSomas);
  try
    vAtu.EscolherDe(vJSON, '0.9.0');
    vOk := not vAtu.Baixar(vNovo, vExe);
    Conferir(vOk, 'hash que nao confere: recusado (' + vAtu.Erro + ')');
    Conferir(not FileExists(vNovo), 'e o arquivo baixado some');
    Conferir(Ler(vExe) = 'versao velha' + LineEnding, 'o executavel continua o de antes');
  finally
    vJSON.Free;
    vAtu.Free;
  end;

  // tamanho errado (download pela metade)
  vTamanho := vTamanho + 100;
  vAtu := TAtualizacao.Create;
  vJSON := Lista('');
  try
    vAtu.EscolherDe(vJSON, '0.9.0');
    vOk := not vAtu.Baixar(vNovo, vExe);
    Conferir(vOk, 'tamanho diferente do release: recusado (' + vAtu.Erro + ')');
  finally
    vJSON.Free;
    vAtu.Free;
  end;

  // troca que falha no meio volta atras
  vAtu := TAtualizacao.Create;
  try
    Conferir(not vAtu.Trocar(vPasta + 'nao-existe.novo', vExe), 'arquivo novo que nao existe: recusado');
    Conferir(Ler(vExe) = 'versao velha' + LineEnding, 'o executavel continua la');
  finally
    vAtu.Free;
  end;
  for vArquivo in [vExe, vBinario, vSomas] do
    SysUtils.DeleteFile(vArquivo);
end;

procedure ConsultarGitHub;
var
  vAtu: TAtualizacao;
begin
  vAtu := TAtualizacao.Create;
  try
    case vAtu.Verificar of
      rvAtualizado:   WriteLn('atualizado: nenhum release do instalador mais novo que ', VersaoInstalador);
      rvNovaVersao:   WriteLn('versão nova: ', vAtu.VersaoNova, ' (', vAtu.URLAsset, ')');
      rvNaoVerificou: WriteLn('não deu para verificar: ', vAtu.Erro);
    end;
  finally
    vAtu.Free;
  end;
end;

begin
  {$IFDEF MSWINDOWS}SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  if ParamStr(1) = '--github' then
  begin
    ConsultarGitHub;
    Halt(0);
  end;
  TestesSHA256;
  TestesEscolha;
  TestesDownloadETroca;
  WriteLn;
  if GFalhas = 0 then
    WriteLn('todos os testes passaram')
  else
    WriteLn(GFalhas, ' falha(s)');
  Halt(Ord(GFalhas > 0));
end.
