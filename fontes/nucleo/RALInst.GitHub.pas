unit RALInst.GitHub;

{$mode ObjFPC}{$H+}

// F8: as versoes do RAL no GitHub e o download de cada uma.
//
// A API (60 pedidos por hora por IP, sem token) so e usada para listar
// releases, tags e ramos e, na execucao, para saber o commit de cada
// submodulo. Toda resposta fica em cache: uma consulta recente (ValidadeCache)
// nem vai a rede, e a arvore de uma tag, que nao muda, fica guardada para
// sempre. Depois disso o pedido leva o ETag (If-None-Match) e o 304 economiza
// a banda — mas, sem token, conta no limite do mesmo jeito (conferido em
// 2026-09-22). Sem rede ou sem cota, o cache serve, e o aviso diz isso.
//
// O codigo vem do codeload.github.com (o mesmo endereco para onde o zipball
// da API redireciona), que nao conta no limite. O zip fica em cache: e dele
// que o catalogo das telas de escolha e lido, e e ele que vai para a pasta
// escolhida — nada e baixado duas vezes.

interface

uses
  Classes, SysUtils, Contnrs, fpjson, RALInst.HTTP, RALInst.Processo;

type
  TTipoVersao = (tvEstavel, tvPreLancamento, tvTag, tvRamo);

  { TVersaoRAL }

  TVersaoRAL = class
  public
    // como o GitHub escreve: 'v1.0', '1.1', 'dev'
    Ref: string;
    Titulo: string;
    // aaaa-mm-dd, quando ha release
    Data: string;
    Tipo: TTipoVersao;
    // a estavel mais recente: o padrao da tela
    Recomendada: boolean;
    function Descricao: string;
    // nome da pasta desta versao dentro de <pasta>/PascalRAL
    function NomePasta: string;
  end;

  { TVersoesRAL }

  TVersoesRAL = class(TObjectList)
  private
    function GetItem(AIndex: integer): TVersaoRAL;
  public
    function Recomendada: TVersaoRAL;
    function Buscar(const ARef: string): TVersaoRAL;
    property Items[AIndex: integer]: TVersaoRAL read GetItem; default;
  end;

  { TRepoGitHub }

  TRepoGitHub = class
  private
    FDono: string;
    FRepo: string;
    FPastaCache: string;
    FErro: string;
    FAviso: string;
    FLog: TLogLinha;
    FOnProgresso: TProgressoHTTP;
    procedure Logar(const ALinha: string);
    function NovoCliente(AAPI: boolean): TClienteHTTP;
    function ArquivoCache(const ATipo, AChave, AExtensao: string): string;
  public
    constructor Create(const ADono, ARepo: string);

    // uma consulta a API do GitHub (caminho depois de /repos/<dono>/<repo>/),
    // com o cache em disco. AValidade em minutos: cache mais novo que isso nem
    // vai a rede; -1 = para sempre. Sem rede, serve o cache (Aviso diz de quando)
    function ObterAPI(const ACaminho: string; AValidade: integer; out AJSON: TJSONData): boolean;

    // releases, tags e ramos com codigo, na ordem de exibicao
    function ListarVersoes(AVersoes: TVersoesRAL): boolean;
    // o zip da referencia, no cache; ARecarregar baixa de novo mesmo havendo
    // copia (ramo muda a cada commit; tag e commit nao)
    function BaixarZip(const ARef: string; ARecarregar: boolean; out AArquivo: string): boolean;
    // commit de cada submodulo na referencia, 'caminho=sha'
    function Submodulos(const ARef: string; ARamo: boolean; ALista: TStrings): boolean;

    property Dono: string read FDono;
    property Repo: string read FRepo;
    property PastaCache: string read FPastaCache write FPastaCache;
    // por que a ultima operacao falhou
    property Erro: string read FErro;
    // a ultima operacao deu certo, mas com ressalva (lista vinda do cache)
    property Aviso: string read FAviso;
    property Log: TLogLinha read FLog write FLog;
    property OnProgresso: TProgressoHTTP read FOnProgresso write FOnProgresso;
  end;

const
  DonoRAL = 'OpenSourceCommunityBrasil';
  RepoRAL = 'PascalRAL';
  // ramos do repositorio do RAL que nao sao o codigo dos componentes
  RamosSemCodigo: array[0..4] of string = (
    'documentation', 'external', 'installer', 'tests', 'gh-pages');
  // minutos em que uma consulta a API vale sem perguntar de novo
  ValidadeCache = 10;

// compara versoes do jeito que as tags vem: 'v1.10' > '1.9' > '1.9-beta'
function CompararTags(const A, B: string): integer;

// 'https://github.com/dono/repo(.git)' -> dono e repo; False se nao e GitHub
function RepoDaURL(const AURL: string; out ADono, ARepo: string): boolean;

// pasta de cache do instalador
function PastaCacheInstalador: string;

implementation

uses
  jsonparser, md5, DateUtils, Math, StrUtils;

function PastaCacheInstalador: string;
begin
  Result := PastaDadosInstalador + 'cache' + PathDelim;
end;

function SemV(const ATag: string): string;
begin
  Result := Trim(ATag);
  if (Result <> '') and (Result[1] in ['v', 'V']) and (Length(Result) > 1) and
     (Result[2] in ['0'..'9']) then
    Delete(Result, 1, 1);
end;

function CompararTags(const A, B: string): integer;

  procedure Separar(const ATag: string; out ANumeros: TStringArray; out ASufixo: string);
  var
    vTexto: string;
    vPos: integer;
  begin
    vTexto := SemV(ATag);
    vPos := 1;
    while (vPos <= Length(vTexto)) and (vTexto[vPos] in ['0'..'9', '.']) do
      Inc(vPos);
    ANumeros := Copy(vTexto, 1, vPos - 1).Split('.');
    ASufixo := Copy(vTexto, vPos, MaxInt);
  end;

var
  vNumA, vNumB: TStringArray;
  vSufA, vSufB: string;
  vInt, vA, vB: integer;
begin
  Separar(A, vNumA, vSufA);
  Separar(B, vNumB, vSufB);
  vInt := 0;
  while (vInt < Length(vNumA)) or (vInt < Length(vNumB)) do
  begin
    vA := 0;
    vB := 0;
    if vInt < Length(vNumA) then
      vA := StrToIntDef(vNumA[vInt], 0);
    if vInt < Length(vNumB) then
      vB := StrToIntDef(vNumB[vInt], 0);
    if vA <> vB then
      Exit(vA - vB);
    Inc(vInt);
  end;
  // '1.0' vem depois de '1.0-beta'
  if (vSufA = '') and (vSufB <> '') then
    Exit(1);
  if (vSufA <> '') and (vSufB = '') then
    Exit(-1);
  Result := CompareText(vSufA, vSufB);
end;

function RepoDaURL(const AURL: string; out ADono, ARepo: string): boolean;
var
  vTexto: string;
  vPartes: TStringArray;
begin
  Result := False;
  ADono := '';
  ARepo := '';
  vTexto := Trim(AURL);
  if Pos('github.com', LowerCase(vTexto)) = 0 then
    Exit;
  // https://github.com/dono/repo(.git) ou git@github.com:dono/repo.git
  vTexto := Copy(vTexto, Pos('github.com', LowerCase(vTexto)) + 11, MaxInt);
  if (vTexto <> '') and (vTexto[Length(vTexto)] = '/') then
    Delete(vTexto, Length(vTexto), 1);
  if LowerCase(ExtractFileExt(vTexto)) = '.git' then
    vTexto := ChangeFileExt(vTexto, '');
  vPartes := vTexto.Split('/');
  if Length(vPartes) < 2 then
    Exit;
  ADono := vPartes[0];
  ARepo := vPartes[1];
  Result := (ADono <> '') and (ARepo <> '');
end;

function SemCaracteresRuins(const ATexto: string): string;
var
  vInt: integer;
begin
  Result := ATexto;
  for vInt := 1 to Length(Result) do
    if not (Result[vInt] in ['A'..'Z', 'a'..'z', '0'..'9', '.', '-', '_']) then
      Result[vInt] := '_';
end;

function DataCurta(const AData: string): string;
begin
  // '2026-09-19T15:47:55Z' -> '19/09/2026'
  if Length(AData) >= 10 then
    Result := Copy(AData, 9, 2) + '/' + Copy(AData, 6, 2) + '/' + Copy(AData, 1, 4)
  else
    Result := AData;
end;

{ TVersaoRAL }

function TVersaoRAL.Descricao: string;
begin
  case Tipo of
    tvEstavel:
      if Recomendada then
        Result := Ref + '  —  estável mais recente'
      else
        Result := Ref + '  —  estável';
    tvPreLancamento:
      Result := Ref + '  —  pré-lançamento';
    tvTag:
      Result := Ref + '  —  tag';
    tvRamo:
      Result := Ref + '  —  ramo (muda a cada commit)';
  end;
  if Data <> '' then
    Result := Result + ', ' + DataCurta(Data);
end;

function TVersaoRAL.NomePasta: string;
begin
  Result := SemCaracteresRuins(Ref);
end;

{ TVersoesRAL }

function TVersoesRAL.GetItem(AIndex: integer): TVersaoRAL;
begin
  Result := TVersaoRAL(inherited Items[AIndex]);
end;

function TVersoesRAL.Recomendada: TVersaoRAL;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(Count) do
    if Items[vInt].Recomendada then
      Exit(Items[vInt]);
end;

function TVersoesRAL.Buscar(const ARef: string): TVersaoRAL;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(Count) do
    if SameText(Items[vInt].Ref, ARef) then
      Exit(Items[vInt]);
end;

function OrdemVersoes(A, B: Pointer): integer;
var
  vA, vB: TVersaoRAL;

  function Peso(AVersao: TVersaoRAL): integer;
  begin
    if AVersao.Recomendada then
      Exit(0);
    case AVersao.Tipo of
      tvEstavel: Result := 1;
      tvPreLancamento: Result := 2;
      tvTag: Result := 3;
    else
      // dev e master antes dos outros ramos
      if SameText(AVersao.Ref, 'dev') or SameText(AVersao.Ref, 'master') then
        Result := 4
      else
        Result := 5;
    end;
  end;

begin
  vA := TVersaoRAL(A);
  vB := TVersaoRAL(B);
  Result := Peso(vA) - Peso(vB);
  if Result <> 0 then
    Exit;
  if vA.Tipo = tvRamo then
    Result := CompareText(vA.Ref, vB.Ref)
  else
    Result := CompararTags(vB.Ref, vA.Ref);
end;

{ TRepoGitHub }

constructor TRepoGitHub.Create(const ADono, ARepo: string);
begin
  inherited Create;
  FDono := ADono;
  FRepo := ARepo;
  FPastaCache := PastaCacheInstalador;
end;

procedure TRepoGitHub.Logar(const ALinha: string);
begin
  if Assigned(FLog) then
    FLog(ALinha);
end;

function TRepoGitHub.NovoCliente(AAPI: boolean): TClienteHTTP;
var
  vToken: string;
begin
  Result := TClienteHTTP.Create;
  Result.OnProgresso := FOnProgresso;
  if AAPI then
  begin
    Result.Cabecalhos.Add('Accept: application/vnd.github+json');
    Result.Cabecalhos.Add('X-GitHub-Api-Version: 2022-11-28');
    // com token o limite sobe de 60 para 5000 pedidos por hora
    vToken := GetEnvironmentVariable('GITHUB_TOKEN');
    if vToken <> '' then
      Result.Cabecalhos.Add('Authorization: Bearer ' + vToken);
  end;
end;

function TRepoGitHub.ArquivoCache(const ATipo, AChave, AExtensao: string): string;
begin
  Result := FPastaCache + ATipo + PathDelim + SemCaracteresRuins(AChave) + AExtensao;
end;

function TRepoGitHub.ObterAPI(const ACaminho: string; AValidade: integer;
  out AJSON: TJSONData): boolean;
var
  vCli: TClienteHTTP;
  vURL, vCache, vEtagArq, vTexto, vReset: string;
  vEtag: TStringList;
  vOk: boolean;
begin
  Result := False;
  AJSON := nil;
  vURL := Format('https://api.github.com/repos/%s/%s/%s', [FDono, FRepo, ACaminho]);
  vCache := ArquivoCache('api', MD5Print(MD5String(vURL)), '.json');
  vEtagArq := ChangeFileExt(vCache, '.etag');

  vEtag := TStringList.Create;
  vCli := NovoCliente(True);
  try
    vTexto := '';
    if FileExists(vCache) and ((AValidade < 0) or
       (MinutesBetween(Now, FileDateToDateTime(FileAge(vCache))) < AValidade)) then
    begin
      with TStringList.Create do
      try
        LoadFromFile(vCache);
        vTexto := Text;
      finally
        Free;
      end;
      try
        AJSON := GetJSON(vTexto);
        Exit(True);
      except
        // cache corrompido: pergunta de novo
        FreeAndNil(AJSON);
      end;
    end;

    if FileExists(vCache) and FileExists(vEtagArq) then
    begin
      vEtag.LoadFromFile(vEtagArq);
      if Trim(vEtag.Text) <> '' then
        vCli.Cabecalhos.Add('If-None-Match: ' + Trim(vEtag.Text));
    end;

    vOk := vCli.ObterTexto(vURL, vTexto);
    if vOk and (vCli.Status = 200) then
    begin
      ForceDirectories(ExtractFilePath(vCache));
      vEtag.Text := vCli.Cabecalho('ETag');
      with TStringList.Create do
      try
        Text := vTexto;
        SaveToFile(vCache);
      finally
        Free;
      end;
      vEtag.SaveToFile(vEtagArq);
    end
    else if vOk and (vCli.Status = 304) then
      // nada mudou desde a ultima vez: o cache vale, e o pedido nao contou
      vTexto := ''
    else
    begin
      if (vCli.Status = 403) or (vCli.Status = 429) then
      begin
        vReset := vCli.Cabecalho('X-RateLimit-Reset');
        if vCli.Cabecalho('X-RateLimit-Remaining') = '0' then
          FErro := Format('limite de pedidos à API do GitHub esgotado (volta em %d min; ' +
            'defina GITHUB_TOKEN para um limite maior)',
            [Max(1, (StrToInt64Def(vReset, 0) - DateTimeToUnix(Now, False)) div 60 + 1)])
        else
          FErro := 'a API do GitHub recusou o pedido (' + vCli.Erro + ')';
      end
      else if vCli.Status = 404 then
        FErro := 'não existe no GitHub: ' + ACaminho
      else
        FErro := 'sem resposta do GitHub: ' + vCli.Erro;

      // 404 e resposta de verdade; o resto pode ser rede ou cota, e o cache
      // de uma consulta anterior e melhor do que nada
      if (vCli.Status = 404) or not FileExists(vCache) then
        Exit;
      FAviso := FErro + '; usando a consulta guardada de ' +
                FormatDateTime('dd/mm/yyyy hh:nn', FileDateToDateTime(FileAge(vCache)));
      FErro := '';
      vTexto := '';
    end;

    if vTexto = '' then
      with TStringList.Create do
      try
        LoadFromFile(vCache);
        vTexto := Text;
      finally
        Free;
      end;

    try
      AJSON := GetJSON(vTexto);
      Result := True;
    except
      on E: Exception do
        FErro := 'resposta inválida do GitHub: ' + E.Message;
    end;
  finally
    vCli.Free;
    vEtag.Free;
  end;
end;

function TRepoGitHub.ListarVersoes(AVersoes: TVersoesRAL): boolean;
var
  vJSON: TJSONData;
  vInt, vRamo: integer;
  vItem: TJSONObject;
  vVersao, vMelhor: TVersaoRAL;
  vRef: string;
  vSemCodigo: boolean;
begin
  Result := False;
  FErro := '';
  FAviso := '';
  AVersoes.Clear;

  // releases: o que foi publicado de proposito, estavel ou pre-lancamento
  if not ObterAPI('releases?per_page=100', ValidadeCache, vJSON) then
    Exit;
  try
    if vJSON is TJSONArray then
      for vInt := 0 to Pred(TJSONArray(vJSON).Count) do
      begin
        vItem := TJSONArray(vJSON).Objects[vInt];
        if vItem.Get('draft', False) then
          Continue;
        vVersao := TVersaoRAL.Create;
        vVersao.Ref := vItem.Get('tag_name', '');
        vVersao.Titulo := vItem.Get('name', '');
        vVersao.Data := Copy(vItem.Get('published_at', ''), 1, 10);
        if vItem.Get('prerelease', False) then
          vVersao.Tipo := tvPreLancamento
        else
          vVersao.Tipo := tvEstavel;
        AVersoes.Add(vVersao);
      end;
  finally
    vJSON.Free;
  end;

  // tags sem release
  if ObterAPI('tags?per_page=100', ValidadeCache, vJSON) then
  try
    if vJSON is TJSONArray then
      for vInt := 0 to Pred(TJSONArray(vJSON).Count) do
      begin
        vRef := TJSONArray(vJSON).Objects[vInt].Get('name', '');
        if (vRef = '') or (AVersoes.Buscar(vRef) <> nil) then
          Continue;
        vVersao := TVersaoRAL.Create;
        vVersao.Ref := vRef;
        vVersao.Tipo := tvTag;
        AVersoes.Add(vVersao);
      end;
  finally
    vJSON.Free;
  end;

  // ramos, para quem acompanha o desenvolvimento
  if ObterAPI('branches?per_page=100', ValidadeCache, vJSON) then
  try
    if vJSON is TJSONArray then
      for vInt := 0 to Pred(TJSONArray(vJSON).Count) do
      begin
        vRef := TJSONArray(vJSON).Objects[vInt].Get('name', '');
        vSemCodigo := vRef = '';
        for vRamo := Low(RamosSemCodigo) to High(RamosSemCodigo) do
          if SameText(vRef, RamosSemCodigo[vRamo]) then
            vSemCodigo := True;
        if vSemCodigo or (AVersoes.Buscar(vRef) <> nil) then
          Continue;
        vVersao := TVersaoRAL.Create;
        vVersao.Ref := vRef;
        vVersao.Tipo := tvRamo;
        AVersoes.Add(vVersao);
      end;
  finally
    vJSON.Free;
  end;

  // a recomendada: a estavel de numero mais alto
  vMelhor := nil;
  for vInt := 0 to Pred(AVersoes.Count) do
    if (AVersoes[vInt].Tipo = tvEstavel) and
       ((vMelhor = nil) or (CompararTags(AVersoes[vInt].Ref, vMelhor.Ref) > 0)) then
      vMelhor := AVersoes[vInt];
  if vMelhor <> nil then
    vMelhor.Recomendada := True;

  AVersoes.Sort(@OrdemVersoes);
  Result := AVersoes.Count > 0;
  if not Result and (FErro = '') then
    FErro := 'o repositório ' + FDono + '/' + FRepo + ' não tem nenhuma versão publicada';
end;

function TRepoGitHub.BaixarZip(const ARef: string; ARecarregar: boolean;
  out AArquivo: string): boolean;
var
  vCli: TClienteHTTP;
  vTemp: string;
  vArq: TFileStream;
  vAssinatura: word;
begin
  Result := False;
  FErro := '';
  AArquivo := ArquivoCache('zip', FDono + '-' + FRepo + '-' + ARef, '.zip');
  if FileExists(AArquivo) and not ARecarregar then
    Exit(True);

  ForceDirectories(ExtractFilePath(AArquivo));
  vTemp := AArquivo + '.parcial';
  Logar(Format('baixando %s/%s @ %s', [FDono, FRepo, ARef]));
  vCli := NovoCliente(False);
  try
    vArq := TFileStream.Create(vTemp, fmCreate);
    try
      Result := vCli.Obter(Format('https://codeload.github.com/%s/%s/zip/%s',
                                  [FDono, FRepo, ARef]), vArq);
    finally
      vArq.Free;
    end;
    if not Result then
    begin
      if vCli.Status = 404 then
        FErro := Format('%s/%s não tem a versão %s', [FDono, FRepo, ARef])
      else
        FErro := Format('falha ao baixar %s/%s @ %s: %s', [FDono, FRepo, ARef, vCli.Erro]);
      DeleteFile(vTemp);
      Exit;
    end;

    // download interrompido nao vira o zip do cache
    vArq := TFileStream.Create(vTemp, fmOpenRead);
    try
      vAssinatura := 0;
      if vArq.Size >= 2 then
        vArq.ReadBuffer(vAssinatura, 2);
    finally
      vArq.Free;
    end;
    if vAssinatura <> $4B50 then
    begin
      FErro := Format('o GitHub não devolveu um zip para %s/%s @ %s', [FDono, FRepo, ARef]);
      DeleteFile(vTemp);
      Exit(False);
    end;

    if FileExists(AArquivo) then
      DeleteFile(AArquivo);
    Result := RenameFile(vTemp, AArquivo);
    if not Result then
      FErro := 'não foi possível gravar ' + AArquivo;
  finally
    vCli.Free;
  end;
end;

function TRepoGitHub.Submodulos(const ARef: string; ARamo: boolean; ALista: TStrings): boolean;
var
  vJSON: TJSONData;
  vArvore: TJSONArray;
  vInt: integer;
  vItem: TJSONObject;
begin
  Result := False;
  FErro := '';
  FAviso := '';
  ALista.Clear;
  if not ObterAPI('git/trees/' + ARef + '?recursive=1', IfThen(ARamo, ValidadeCache, -1), vJSON) then
    Exit;
  try
    if not (vJSON is TJSONObject) then
    begin
      FErro := 'resposta inesperada da árvore de ' + ARef;
      Exit;
    end;
    vArvore := TJSONObject(vJSON).Get('tree', TJSONArray(nil));
    if vArvore <> nil then
      for vInt := 0 to Pred(vArvore.Count) do
      begin
        vItem := vArvore.Objects[vInt];
        // submodulo: mode 160000, type commit, sha = commit fixado
        if vItem.Get('type', '') = 'commit' then
          ALista.Add(vItem.Get('path', '') + '=' + vItem.Get('sha', ''));
      end;
    if TJSONObject(vJSON).Get('truncated', False) then
      FAviso := 'a árvore de ' + ARef + ' veio truncada pelo GitHub';
    Result := True;
  finally
    vJSON.Free;
  end;
end;

end.
