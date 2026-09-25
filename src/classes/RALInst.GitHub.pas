/// RAL versions on GitHub and the download of each one. The API (60 requests
/// per hour per IP, without a token) is only used to list releases, tags and
/// branches and, at run time, to learn the commit of each submodule. Every
/// answer is cached: a recent query (ValidadeCache) does not even reach the
/// network, and the tree of a tag, which never changes, is kept forever. After
/// that the request carries the ETag (If-None-Match) and a 304 saves bandwidth,
/// but without a token it still counts against the limit (checked on
/// 2026-09-22). Without network or quota the cache serves, and the warning says
/// so. The code comes from codeload.github.com (where the API zipball
/// redirects), which does not count against the limit. The zip is cached: the
/// catalog of the choice screens is read from it, and it is what goes into the
/// chosen folder, so nothing is downloaded twice.
unit RALInst.GitHub;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, fpjson,
  RALInst.HTTP, RALInst.Processo;

type
  /// Kind of RAL version.
  TTipoVersao = (tvEstavel, tvPreLancamento, tvTag, tvRamo);

  /// One RAL version: release, tag or branch.
  TVersaoRAL = class
  public
    /// yyyy-mm-dd, when there is a release
    Data: string;
    /// The newest stable one: the screen's default
    Recomendada: boolean;
    /// As GitHub writes it: 'v1.0', '1.1', 'dev'
    Ref: string;
    Tipo: TTipoVersao;
    Titulo: string;
    /// Text for the version list
    function Descricao: string;
    /// Name of this version's folder inside <folder>/PascalRAL
    function NomePasta: string;
  end;

  /// The versions of a repository, owned by the list.
  TVersoesRAL = class(TObjectList)
  private
    function GetItem(AIndex: integer): TVersaoRAL;
  public
    /// The version with that ref; nil if none
    function Buscar(const ARef: string): TVersaoRAL;
    /// The recommended version; nil if none
    function Recomendada: TVersaoRAL;

    property Items[AIndex: integer]: TVersaoRAL read GetItem; default;
  end;

  /// Access to one GitHub repository: versions, zips and submodules.
  TRepoGitHub = class
  private
    FAviso: string;
    FDono: string;
    FErro: string;
    FLog: TLogLinha;
    FOnProgresso: TProgressoHTTP;
    FPastaCache: string;
    FRepo: string;
    /// Cache file of a kind (api, zip) and key
    function ArquivoCache(const ATipo, AChave, AExtensao: string): string;
    /// Passes the line to Log when assigned
    procedure Logar(const ALinha: string);
    /// HTTP client with the API headers (and GITHUB_TOKEN, when set)
    function NovoCliente(AAPI: boolean): TClienteHTTP;
  public
    constructor Create(const ADono, ARepo: string);
    /// The zip of the ref, in the cache; ARecarregar downloads again even with
    /// a copy (a branch changes with every commit; a tag or commit does not)
    function BaixarZip(const ARef: string; ARecarregar: boolean;
      out AArquivo: string): boolean;
    /// Releases, tags and branches with code, in display order
    function ListarVersoes(AVersoes: TVersoesRAL): boolean;
    /// One GitHub API query (path after /repos/<owner>/<repo>/), with the disk
    /// cache. AValidade in minutes: a cache newer than that does not reach the
    /// network; -1 = forever. Without network the cache serves (Aviso says
    /// from when)
    function ObterAPI(const ACaminho: string; AValidade: integer;
      out AJSON: TJSONData): boolean;
    /// Commit of every submodule at the ref, 'path=sha'
    function Submodulos(const ARef: string; ARamo: boolean; ALista: TStrings): boolean;

    /// The last operation worked, with a caveat (list from the cache)
    property Aviso: string read FAviso;
    property Dono: string read FDono;
    /// Why the last operation failed
    property Erro: string read FErro;
    property Log: TLogLinha read FLog write FLog;
    property PastaCache: string read FPastaCache write FPastaCache;
    property Repo: string read FRepo;
    property OnProgresso: TProgressoHTTP read FOnProgresso write FOnProgresso;
  end;

const
  DonoRAL = 'OpenSourceCommunityBrasil';
  /// Branches of the RAL repository that are not the components' code
  RamosSemCodigo: array[0..4] of string = (
    'documentation', 'external', 'installer', 'tests', 'gh-pages');
  RepoRAL = 'PascalRAL';
  /// Minutes an API query stays valid without asking again
  ValidadeCache = 10;

/// Compares versions the way tags come: 'v1.10' > '1.9' > '1.9-beta'
function CompararTags(const A, B: string): integer;
/// The installer's cache folder
function PastaCacheInstalador: string;
/// 'https://github.com/owner/repo(.git)' -> owner and repo; False if not GitHub
function RepoDaURL(const AURL: string; out ADono, ARepo: string): boolean;

implementation

uses
  jsonparser, md5, DateUtils, Math, StrUtils,
  RALInst.Mensagens;

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

  procedure Separar(const ATag: string; out ANumeros: TStringArray;
    out ASufixo: string);
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
  Result := Ref;
  case Tipo of
    tvEstavel:
      if Recomendada then
        Result := Format(cmVersaoEstavelRecente, [Ref])
      else
        Result := Format(cmVersaoEstavel, [Ref]);
    tvPreLancamento:
      Result := Format(cmVersaoPreLancamento, [Ref]);
    tvTag:
      Result := Format(cmVersaoTag, [Ref]);
    tvRamo:
      Result := Format(cmVersaoRamo, [Ref]);
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
          FErro := Format(emGitHubLimite,
            [Max(1, (StrToInt64Def(vReset, 0) - DateTimeToUnix(Now, False)) div 60 + 1)])
        else
          FErro := Format(emGitHubRecusou, [vCli.Erro]);
      end
      else if vCli.Status = 404 then
        FErro := Format(emGitHubNaoExiste, [ACaminho])
      else
        FErro := Format(emGitHubSemResposta, [vCli.Erro]);

      // 404 e resposta de verdade; o resto pode ser rede ou cota, e o cache
      // de uma consulta anterior e melhor do que nada
      if (vCli.Status = 404) or not FileExists(vCache) then
        Exit;
      FAviso := Format(wmGitHubCache, [FErro,
        FormatDateTime('dd/mm/yyyy hh:nn', FileDateToDateTime(FileAge(vCache)))]);
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
        FErro := Format(emGitHubInvalida, [E.Message]);
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
    FErro := Format(emGitHubSemVersoes, [FDono, FRepo]);
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
  Logar(Format(cmBaixando, [FDono, FRepo, ARef]));
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
        FErro := Format(emGitHubSemAVersao, [FDono, FRepo, ARef])
      else
        FErro := Format(emGitHubFalhaBaixar, [FDono, FRepo, ARef, vCli.Erro]);
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
      FErro := Format(emGitHubNaoZip, [FDono, FRepo, ARef]);
      DeleteFile(vTemp);
      Exit(False);
    end;

    if FileExists(AArquivo) then
      DeleteFile(AArquivo);
    Result := RenameFile(vTemp, AArquivo);
    if not Result then
      FErro := Format(emGravar, [AArquivo]);
  finally
    vCli.Free;
  end;
end;

function TRepoGitHub.Submodulos(const ARef: string; ARamo: boolean;
  ALista: TStrings): boolean;
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
  if not ObterAPI('git/trees/' + ARef + '?recursive=1', IfThen(ARamo, ValidadeCache, -1),
                  vJSON) then
    Exit;
  try
    if not (vJSON is TJSONObject) then
    begin
      FErro := Format(emGitHubArvore, [ARef]);
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
      FAviso := Format(wmGitHubArvoreTruncada, [ARef]);
    Result := True;
  finally
    vJSON.Free;
  end;
end;

end.
