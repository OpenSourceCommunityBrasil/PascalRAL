unit RALInst.Fontes;

{$mode ObjFPC}{$H+}

// Os fontes de uma versao do RAL na pasta que o usuario escolheu (§0 do
// plano): <pasta>/PascalRAL/<versao>/, com os submodulos que os pacotes
// escolhidos usam, cada um no commit que o RAL fixou.
//
// A pasta e permanente — o library path e os links das IDEs apontam para
// ela — e por isso a troca e cuidadosa: tudo e extraido numa pasta ao lado,
// e so no fim ela toma o lugar da anterior. Falha no meio deixa a instalacao
// que ja funcionava como estava. Uma pasta que ja existe e nao foi criada pelo
// instalador (sem o arquivo de marca) nunca e tocada.

interface

uses
  Classes, SysUtils, RALInst.GitHub, RALInst.Processo;

type

  { TPreparoFontes }

  TPreparoFontes = class
  private
    FRepo: TRepoGitHub;
    FRef: string;
    FRamo: boolean;
    FPastaBase: string;
    FSubmodulos: TStringList;
    FLog: TLogLinha;
    FErro: string;
    FCommit: string;
    FDestino: string;
    FNome: string;
    procedure Logar(const ALinha: string);
    function LerMarca(const APasta: string; out ACommit: string; ASubmodulos: TStrings): boolean;
    procedure GravarMarca(const APasta: string; ASubmodulos: TStrings);
    function BaixarSubmodulos(const APasta: string; AFaltando: TStrings;
      AInstalados: TStrings): boolean;
  public
    constructor Create(ARepo: TRepoGitHub; const ARef: string; ARamo: boolean);
    destructor Destroy; override;

    // <PastaBase>/PascalRAL/<versao>/
    function PastaDestino: string;
    function Executar: boolean;

    property PastaBase: string read FPastaBase write FPastaBase;
    // pasta final completa, no lugar de <PastaBase>/PascalRAL/<versao>: as
    // dependencias vao para <pasta>/dependencias/<nome>/<versao>
    property Destino: string read FDestino write FDestino;
    // para o log ('PascalRAL', 'mORMot2')
    property Nome: string read FNome write FNome;
    // caminhos dos submodulos que os pacotes escolhidos usam
    // ('src/others/ZSTD'); os outros nao sao baixados
    property Submodulos: TStringList read FSubmodulos;
    property Log: TLogLinha read FLog write FLog;
    property Erro: string read FErro;
    // commit dos fontes que ficaram na pasta
    property Commit: string read FCommit;
  end;

const
  // o arquivo que diz que a pasta foi criada pelo instalador, e com o que
  ArquivoMarca = '.ralinstaller.json';

// apaga uma pasta inteira; False se sobrou alguma coisa
function ApagarPasta(const APasta: string): boolean;

// a pasta existe, tem conteudo e nao foi criada pelo instalador: nunca e tocada
function PastaDeOutro(const APasta: string): boolean;

// F10: de onde vieram os fontes da pasta (a marca do instalador): repositorio,
// versao e commit; vazios se a pasta nao tem marca (pasta local do usuario)
procedure LerMarca(const APasta: string; out ARepositorio, AVersao, ACommit: string);

implementation

uses
  fpjson, jsonparser, RALInst.Zip;

procedure LerMarca(const APasta: string; out ARepositorio, AVersao, ACommit: string);
var
  vTexto: TStringList;
  vJSON: TJSONData;
begin
  ARepositorio := '';
  AVersao := '';
  ACommit := '';
  if not FileExists(IncludeTrailingPathDelimiter(APasta) + ArquivoMarca) then
    Exit;
  vTexto := TStringList.Create;
  vJSON := nil;
  try
    try
      vTexto.LoadFromFile(IncludeTrailingPathDelimiter(APasta) + ArquivoMarca);
      vJSON := GetJSON(vTexto.Text);
      if vJSON is TJSONObject then
      begin
        ARepositorio := TJSONObject(vJSON).Get('repositorio', '');
        AVersao := TJSONObject(vJSON).Get('versao', '');
        ACommit := TJSONObject(vJSON).Get('commit', '');
      end;
    except
      // marca ilegivel: sem o que dizer
    end;
  finally
    vJSON.Free;
    vTexto.Free;
  end;
end;

function ApagarPasta(const APasta: string): boolean;
var
  vBusca: TSearchRec;
  vDe: string;
begin
  vDe := IncludeTrailingPathDelimiter(APasta);
  if FindFirst(CaminhoLongo(vDe + AllFilesMask), faAnyFile, vBusca) = 0 then
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
        ApagarPasta(vDe + vBusca.Name)
      else
      begin
        FileSetAttr(CaminhoLongo(vDe + vBusca.Name), 0);
        SysUtils.DeleteFile(CaminhoLongo(vDe + vBusca.Name));
      end;
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
  Result := RemoveDir(CaminhoLongo(APasta)) or not DirectoryExists(CaminhoLongo(APasta));
end;

// url do submodulo de um caminho; path e url vem em qualquer ordem dentro
// da secao [submodule "..."]
function URLDoSubmodulo(AGitmodules: TStrings; const ACaminho: string): string;
var
  vLinha: integer;
  vTexto, vChave, vPath, vURL: string;

  function Achou: boolean;
  begin
    Result := (vPath <> '') and SameText(vPath, ACaminho) and (vURL <> '');
  end;

begin
  Result := '';
  vPath := '';
  vURL := '';
  for vLinha := 0 to Pred(AGitmodules.Count) do
  begin
    vTexto := Trim(AGitmodules[vLinha]);
    if Copy(vTexto, 1, 1) = '[' then
    begin
      if Achou then
        Exit(vURL);
      vPath := '';
      vURL := '';
    end
    else if Pos('=', vTexto) > 0 then
    begin
      vChave := LowerCase(Trim(Copy(vTexto, 1, Pos('=', vTexto) - 1)));
      if vChave = 'path' then
        vPath := Trim(Copy(vTexto, Pos('=', vTexto) + 1, MaxInt))
      else if vChave = 'url' then
        vURL := Trim(Copy(vTexto, Pos('=', vTexto) + 1, MaxInt));
    end;
  end;
  if Achou then
    Result := vURL;
end;

function PastaVazia(const APasta: string): boolean;
var
  vBusca: TSearchRec;
begin
  Result := True;
  if FindFirst(IncludeTrailingPathDelimiter(APasta) + AllFilesMask, faAnyFile, vBusca) = 0 then
  try
    repeat
      if (vBusca.Name <> '.') and (vBusca.Name <> '..') then
        Exit(False);
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

function PastaDeOutro(const APasta: string): boolean;
begin
  Result := (APasta <> '') and DirectoryExists(APasta) and not PastaVazia(APasta) and
            not FileExists(IncludeTrailingPathDelimiter(APasta) + ArquivoMarca);
end;

{ TPreparoFontes }

constructor TPreparoFontes.Create(ARepo: TRepoGitHub; const ARef: string; ARamo: boolean);
begin
  inherited Create;
  FRepo := ARepo;
  FRef := ARef;
  FRamo := ARamo;
  FNome := 'PascalRAL';
  FSubmodulos := TStringList.Create;
  FSubmodulos.CaseSensitive := False;
  FSubmodulos.Sorted := True;
  FSubmodulos.Duplicates := dupIgnore;
end;

destructor TPreparoFontes.Destroy;
begin
  FSubmodulos.Free;
  inherited Destroy;
end;

procedure TPreparoFontes.Logar(const ALinha: string);
begin
  if Assigned(FLog) then
    FLog(ALinha);
end;

function TPreparoFontes.PastaDestino: string;
var
  vVersao: TVersaoRAL;
begin
  if FDestino <> '' then
    Exit(IncludeTrailingPathDelimiter(FDestino));
  vVersao := TVersaoRAL.Create;
  try
    vVersao.Ref := FRef;
    Result := IncludeTrailingPathDelimiter(FPastaBase) + 'PascalRAL' + PathDelim +
              vVersao.NomePasta + PathDelim;
  finally
    vVersao.Free;
  end;
end;

function TPreparoFontes.LerMarca(const APasta: string; out ACommit: string;
  ASubmodulos: TStrings): boolean;
var
  vTexto: TStringList;
  vJSON: TJSONData;
  vSubs: TJSONObject;
  vInt: integer;
begin
  Result := False;
  ACommit := '';
  ASubmodulos.Clear;
  if not FileExists(APasta + ArquivoMarca) then
    Exit;
  vTexto := TStringList.Create;
  try
    vTexto.LoadFromFile(APasta + ArquivoMarca);
    try
      vJSON := GetJSON(vTexto.Text);
    except
      // marca ilegivel ainda e marca: a pasta e do instalador
      Exit(True);
    end;
    try
      if vJSON is TJSONObject then
      begin
        ACommit := TJSONObject(vJSON).Get('commit', '');
        vSubs := TJSONObject(vJSON).Get('submodulos', TJSONObject(nil));
        if vSubs <> nil then
          for vInt := 0 to Pred(vSubs.Count) do
            ASubmodulos.Add(vSubs.Names[vInt] + '=' + vSubs.Items[vInt].AsString);
      end;
      Result := True;
    finally
      vJSON.Free;
    end;
  finally
    vTexto.Free;
  end;
end;

procedure TPreparoFontes.GravarMarca(const APasta: string; ASubmodulos: TStrings);
var
  vRaiz, vSubs: TJSONObject;
  vInt: integer;
  vTexto: TStringList;
begin
  vRaiz := TJSONObject.Create;
  vTexto := TStringList.Create;
  try
    vRaiz.Add('repositorio', FRepo.Dono + '/' + FRepo.Repo);
    vRaiz.Add('versao', FRef);
    vRaiz.Add('commit', FCommit);
    vRaiz.Add('data', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', Now));
    vSubs := TJSONObject.Create;
    for vInt := 0 to Pred(ASubmodulos.Count) do
      vSubs.Add(ASubmodulos.Names[vInt], ASubmodulos.ValueFromIndex[vInt]);
    vRaiz.Add('submodulos', vSubs);
    vRaiz.Add('aviso', 'Pasta criada pelo RAL Installer. As IDEs apontam para ela: ' +
                       'apagar ou mover quebra a instalação.');
    vTexto.Text := vRaiz.FormatJSON;
    vTexto.SaveToFile(APasta + ArquivoMarca);
  finally
    vTexto.Free;
    vRaiz.Free;
  end;
end;

function TPreparoFontes.BaixarSubmodulos(const APasta: string; AFaltando: TStrings;
  AInstalados: TStrings): boolean;
var
  vCommits, vGitmodules: TStringList;
  vInt: integer;
  vCaminho, vSha, vURL, vDono, vNome, vZip, vErro: string;
  vSub: TRepoGitHub;
begin
  Result := True;
  if AFaltando.Count = 0 then
    Exit;

  vCommits := TStringList.Create;
  vGitmodules := TStringList.Create;
  try
    // o commit de cada submodulo esta na arvore da versao; o endereco, no
    // .gitmodules que veio no zip
    if not FRepo.Submodulos(FRef, FRamo, vCommits) then
    begin
      FErro := 'não deu para saber o commit dos submódulos: ' + FRepo.Erro;
      Exit(False);
    end;
    if FRepo.Aviso <> '' then
      Logar('  aviso: ' + FRepo.Aviso);
    if FileExists(APasta + '.gitmodules') then
      vGitmodules.LoadFromFile(APasta + '.gitmodules');

    for vInt := 0 to Pred(AFaltando.Count) do
    begin
      vCaminho := AFaltando[vInt];
      vSha := vCommits.Values[vCaminho];
      if vSha = '' then
      begin
        // vindo da versao anterior, pode simplesmente nao existir mais
        if FSubmodulos.IndexOf(vCaminho) < 0 then
        begin
          Logar('  submódulo ' + vCaminho + ' não existe na versão ' + FRef + '; fica de fora');
          Continue;
        end;
        FErro := 'a versão ' + FRef + ' não declara o submódulo ' + vCaminho;
        Exit(False);
      end;

      vURL := URLDoSubmodulo(vGitmodules, vCaminho);
      if not RepoDaURL(vURL, vDono, vNome) then
      begin
        FErro := Format('submódulo %s: endereço fora do GitHub (%s)', [vCaminho, vURL]);
        Exit(False);
      end;

      Logar(Format('  submódulo %s: %s/%s @ %s', [vCaminho, vDono, vNome, Copy(vSha, 1, 7)]));
      vSub := TRepoGitHub.Create(vDono, vNome);
      try
        vSub.PastaCache := FRepo.PastaCache;
        vSub.OnProgresso := FRepo.OnProgresso;
        // commit nao muda: o cache vale para sempre
        if not vSub.BaixarZip(vSha, False, vZip) then
        begin
          FErro := vSub.Erro;
          Exit(False);
        end;
      finally
        vSub.Free;
      end;

      if DirectoryExists(APasta + vCaminho) then
        ApagarPasta(APasta + vCaminho);
      if not ExtrairZip(vZip, APasta + StringReplace(vCaminho, '/', PathDelim, [rfReplaceAll]), vErro) then
      begin
        FErro := vErro;
        Exit(False);
      end;
      if vErro <> '' then
        Logar('  aviso: ' + vErro);
      AInstalados.Values[vCaminho] := vSha;
    end;
  finally
    vGitmodules.Free;
    vCommits.Free;
  end;
end;

function TPreparoFontes.Executar: boolean;
var
  vDestino, vNova, vAnterior, vZip, vErro, vCommitAtual: string;
  vInstalados, vFaltando: TStringList;
  vInt: integer;
begin
  Result := False;
  FErro := '';
  vDestino := PastaDestino;
  vNova := ExcludeTrailingPathDelimiter(vDestino) + '.novo' + PathDelim;
  vAnterior := ExcludeTrailingPathDelimiter(vDestino) + '.anterior' + PathDelim;

  vInstalados := TStringList.Create;
  vFaltando := TStringList.Create;
  try
    // 0. a pasta de destino e nossa?
    if DirectoryExists(vDestino) and not PastaVazia(vDestino) and
       not LerMarca(vDestino, vCommitAtual, vInstalados) then
    begin
      FErro := 'a pasta ' + vDestino + ' já existe e não foi criada pelo instalador; ' +
               'escolha outra pasta de instalação ou esvazie esta';
      Exit;
    end;

    // 1. o zip da versao (normalmente ja no cache, das telas de escolha)
    Logar('Fontes: ' + FNome + ' ' + FRef + ' em ' + vDestino);
    if not FRepo.BaixarZip(FRef, False, vZip) then
    begin
      FErro := FRepo.Erro;
      Exit;
    end;
    FCommit := ComentarioZip(vZip);

    // 2. mesma versao ja na pasta: so o que faltar de submodulo
    if DirectoryExists(vDestino) and (FCommit <> '') and SameText(vCommitAtual, FCommit) then
    begin
      Logar('  já está na pasta (commit ' + Copy(FCommit, 1, 7) + ')');
      for vInt := 0 to Pred(FSubmodulos.Count) do
        if (vInstalados.Values[FSubmodulos[vInt]] = '') or
           not DirectoryExists(vDestino + FSubmodulos[vInt]) then
          vFaltando.Add(FSubmodulos[vInt]);
      if not BaixarSubmodulos(vDestino, vFaltando, vInstalados) then
        Exit;
      if vFaltando.Count > 0 then
        GravarMarca(vDestino, vInstalados);
      Exit(True);
    end;

    // 3. versao nova (ou pasta nova): tudo numa pasta ao lado
    if DirectoryExists(vNova) and not ApagarPasta(vNova) then
    begin
      FErro := 'não foi possível limpar ' + vNova;
      Exit;
    end;
    Logar('  extraindo ' + ExtractFileName(vZip));
    if not ExtrairZip(vZip, vNova, vErro) then
    begin
      FErro := vErro;
      ApagarPasta(vNova);
      Exit;
    end;
    if vErro <> '' then
      Logar('  aviso: ' + vErro);

    // os submodulos pedidos agora e os que a versao anterior ja tinha: uma
    // IDE que instalou o ZSTD daquela pasta nao pode ficar sem ele porque
    // desta vez so o BSON foi pedido
    vFaltando.Assign(FSubmodulos);
    for vInt := 0 to Pred(vInstalados.Count) do
      if vFaltando.IndexOf(vInstalados.Names[vInt]) < 0 then
        vFaltando.Add(vInstalados.Names[vInt]);
    vInstalados.Clear;
    if not BaixarSubmodulos(vNova, vFaltando, vInstalados) then
    begin
      ApagarPasta(vNova);
      Exit;
    end;
    GravarMarca(vNova, vInstalados);

    // 4. a troca: a anterior sai de lado, a nova entra, a anterior e apagada
    if DirectoryExists(vDestino) then
    begin
      if DirectoryExists(vAnterior) then
        ApagarPasta(vAnterior);
      if not RenameFile(ExcludeTrailingPathDelimiter(vDestino), ExcludeTrailingPathDelimiter(vAnterior)) then
      begin
        FErro := 'não foi possível substituir ' + vDestino + ' (algum arquivo aberto? ' +
                 'feche as IDEs). A versão nova ficou em ' + vNova;
        Exit;
      end;
    end;
    if not RenameFile(ExcludeTrailingPathDelimiter(vNova), ExcludeTrailingPathDelimiter(vDestino)) then
    begin
      // devolve a anterior
      if DirectoryExists(vAnterior) then
        RenameFile(ExcludeTrailingPathDelimiter(vAnterior), ExcludeTrailingPathDelimiter(vDestino));
      FErro := 'não foi possível criar ' + vDestino;
      Exit;
    end;
    if DirectoryExists(vAnterior) and not ApagarPasta(vAnterior) then
      Logar('  aviso: a versão anterior ficou em ' + vAnterior + '; pode ser apagada à mão');

    Logar(Format('  pronto: commit %s, %d submódulo(s)', [Copy(FCommit, 1, 7), vInstalados.Count]));
    Result := True;
  finally
    vFaltando.Free;
    vInstalados.Free;
  end;
end;

end.
