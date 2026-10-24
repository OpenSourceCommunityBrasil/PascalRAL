/// Download of a dependency described by a recipe, into
/// <folder>/dependencias/<name>/<version>/, with the same care as the RAL
/// folder (mark, extraction beside it and swap at the end; a foreign folder is
/// never touched), plus the extras the recipe asks for (mORMot2's static files,
/// in a release asset). What to do with it in each IDE belongs to the
/// installation units (RALInst.Instalar.Delphi, RALInst.Instalar.Lazarus).
unit RALInst.Dependencias;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALInst.GitHub, RALInst.HTTP, RALInst.IDE, RALInst.Processo, RALInst.Receitas;

type
  /// Downloads one dependency, at the version an IDE asks for.
  TBaixaDependencia = class
  private
    FErro: string;
    FLog: TLogLinha;
    /// Another repository asked by the version rule ('frones/ZeosLib')
    FOutroRepo: string;
    FPastaBase: string;
    FRamo: boolean;
    FReceita: TReceita;
    FRef: string;
    FRelease: boolean;
    FRepo: TRepoGitHub;
    FVersaoPedida: string;
    /// Downloads and extracts the recipe extras (release assets)
    function BaixarExtras: boolean;
    /// F20: a recipe with no source code ("fonte.codigo": false) still gets
    /// its folder with the installer's mark; False when the folder is someone
    /// else's
    function GarantirPastaSemCodigo: boolean;
    /// Passes the line to Log when assigned
    procedure Logar(const ALinha: string);
    /// Parses 'owner/repo:ref' and recreates the repository when it changes
    procedure SetVersaoPedida(const AValor: string);
  public
    constructor Create(AReceita: TReceita);
    destructor Destroy; override;
    /// Downloads (or reuses) the dependency and its extras
    function Executar: boolean;
    /// 'owner/repo' this version comes from
    function Fonte: string;
    /// <PastaBase>/dependencias/<name>/<version>/; resolves the version if
    /// needed
    function PastaDestino: string;
    /// 'estavel' becomes the real version: the highest stable release; without
    /// releases, the highest numeric tag; without tags, the highest '-stable'
    /// branch (Zeos); last, master/main
    function ResolverVersao: boolean;

    property Erro: string read FErro;
    property Log: TLogLinha read FLog write FLog;
    property PastaBase: string read FPastaBase write FPastaBase;
    property Receita: TReceita read FReceita;
    property Ref: string read FRef;
    property Repo: TRepoGitHub read FRepo;
    /// The version asked ('estavel', branch or tag); by default the recipe's,
    /// but the IDE may ask another one (Zeos 8.0-patches for FPC 3.3), even
    /// from another repository: 'frones/ZeosLib:master' (VersaoComFonte)
    property VersaoPedida: string read FVersaoPedida write SetVersaoPedida;
  end;

/// F20: runs a copiar-arquivo action: the files (relative to the dependency
/// root ARaiz, '*' allowed in a part of the path) go to <ARedist>/<platform>/
/// when the action's platform is one of APlataformas and its IDE range holds;
/// ACopiados gets 'platform: destination' of each copy. False when a file is
/// missing or could not be copied
function CopiarArquivosDep(AReceita: TReceita; AAcao: TAcaoReceita;
  AIDE: TIDEInstance; const ARaiz, ARedist: string; APlataformas: TStrings;
  ALog: TLogLinha; ACopiados: TStrings): boolean;
/// F20: where the runtime files go: ARedist when given, else <installation
/// folder>/redist, the installation folder being the one holding
/// dependencias/<name>/<version> (ARaiz)
function PastaRedistDe(const ARaiz, ARedist: string): string;

implementation

uses
  RegExpr, fpjson,
  RALInst.Fontes, RALInst.Mensagens, RALInst.Promocao, RALInst.Tar, RALInst.Zip;

function PastaRedistDe(const ARaiz, ARedist: string): string;
begin
  if ARedist <> '' then
    Result := IncludeTrailingPathDelimiter(ARedist)
  else
    // <instalacao>/dependencias/<nome>/<versao>/ -> <instalacao>/redist/
    Result := IncludeTrailingPathDelimiter(ExpandFileName(
                IncludeTrailingPathDelimiter(ARaiz) + '..' + PathDelim + '..' +
                PathDelim + '..')) + 'redist' + PathDelim;
end;

// os arquivos que casam com um caminho relativo com '*' numa das partes
procedure BuscarArquivos(const APasta, ARelativo: string; ALista: TStrings);
var
  vParte, vResto: string;
  vPos: integer;
  vBusca: TSearchRec;
begin
  vPos := Pos('/', ARelativo);
  if vPos = 0 then
  begin
    vParte := ARelativo;
    vResto := '';
  end
  else
  begin
    vParte := Copy(ARelativo, 1, vPos - 1);
    vResto := Copy(ARelativo, vPos + 1, MaxInt);
  end;
  if FindFirst(IncludeTrailingPathDelimiter(APasta) + vParte, faAnyFile,
               vBusca) = 0 then
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if vResto = '' then
      begin
        if (vBusca.Attr and faDirectory) = 0 then
          ALista.Add(IncludeTrailingPathDelimiter(APasta) + vBusca.Name);
      end
      else if (vBusca.Attr and faDirectory) <> 0 then
        BuscarArquivos(IncludeTrailingPathDelimiter(APasta) + vBusca.Name, vResto,
                       ALista);
    until FindNext(vBusca) <> 0;
  finally
    SysUtils.FindClose(vBusca);
  end;
end;

// a plataforma da acao (nome do Delphi: win32, win64) e a do FPC no Lazarus
function PlataformaDaIDE(AIDE: TIDEInstance; const APlataforma: string;
  APlataformas: TStrings): boolean;
begin
  if AIDE.Tipo = tiDelphi then
    Result := APlataformas.IndexOf(APlataforma) >= 0
  else if SameText(APlataforma, 'win32') then
    Result := AIDE.Plataformas.IndexOf('i386-win32') >= 0
  else if SameText(APlataforma, 'win64') then
    Result := AIDE.Plataformas.IndexOf('x86_64-win64') >= 0
  else if SameText(APlataforma, 'linux64') then
    Result := AIDE.Plataformas.IndexOf('x86_64-linux') >= 0
  else
    Result := False;
end;

function CopiarArquivosDep(AReceita: TReceita; AAcao: TAcaoReceita;
  AIDE: TIDEInstance; const ARaiz, ARedist: string; APlataformas: TStrings;
  ALog: TLogLinha; ACopiados: TStrings): boolean;
var
  vArquivos: TStringList;
  vPadrao, vArquivo, vDestino: string;
begin
  Result := True;
  if not PlataformaDaIDE(AIDE, AAcao.Plataforma, APlataformas) then
    Exit;
  // a faixa da acao: a DLL do brotli no Win32 so antes do 10.1 (que liga o .obj)
  if AAcao.Condicao.Relevante(AIDE) and not AAcao.Condicao.Satisfaz(AIDE, True) then
    Exit;
  vArquivos := TStringList.Create;
  try
    for vPadrao in AAcao.Arquivos do
    begin
      vArquivos.Clear;
      BuscarArquivos(ExcludeTrailingPathDelimiter(ARaiz),
                     StringReplace(vPadrao, '\', '/', [rfReplaceAll]), vArquivos);
      if vArquivos.Count = 0 then
      begin
        if Assigned(ALog) then
          ALog('  ' + Format(emCopiarSemArquivo, [AReceita.Nome, vPadrao]));
        Result := False;
        Continue;
      end;
      for vArquivo in vArquivos do
      begin
        vDestino := IncludeTrailingPathDelimiter(ARedist) + AAcao.Plataforma +
                    PathDelim + ExtractFileName(vArquivo);
        ForceDirectories(ExtractFileDir(vDestino));
        if FileExists(vDestino) then
          SysUtils.DeleteFile(vDestino);
        if not CopiarArquivo(vArquivo, vDestino) then
        begin
          if Assigned(ALog) then
            ALog('  ' + Format(emCopiarArquivo, [AReceita.Nome, vArquivo, vDestino]));
          Result := False;
          Continue;
        end;
        if Assigned(ALog) then
          ALog(Format(cmCopiadoRedist, [vDestino]));
        if ACopiados <> nil then
          ACopiados.Add(AAcao.Plataforma + ': ' + vDestino);
      end;
    end;
  finally
    vArquivos.Free;
  end;
end;

{ TBaixaDependencia }

constructor TBaixaDependencia.Create(AReceita: TReceita);
var
  vDono, vNome: string;
begin
  inherited Create;
  FReceita := AReceita;
  FVersaoPedida := AReceita.Versao;
  vDono := Copy(AReceita.Github, 1, Pos('/', AReceita.Github) - 1);
  vNome := Copy(AReceita.Github, Pos('/', AReceita.Github) + 1, MaxInt);
  FRepo := TRepoGitHub.Create(vDono, vNome);
end;

destructor TBaixaDependencia.Destroy;
begin
  FRepo.Free;
  inherited Destroy;
end;

procedure TBaixaDependencia.SetVersaoPedida(const AValor: string);
var
  vGithub, vRef, vRepo: string;
begin
  SepararVersao(AValor, vGithub, vRef);
  if SameText(vRef, FVersaoPedida) and SameText(vGithub, FOutroRepo) then
    Exit;
  FVersaoPedida := vRef;
  // resolvida de novo na proxima vez
  FRef := '';
  if SameText(vGithub, FOutroRepo) then
    Exit;
  FOutroRepo := vGithub;
  if vGithub = '' then
    vRepo := FReceita.Github
  else
    vRepo := vGithub;
  FreeAndNil(FRepo);
  FRepo := TRepoGitHub.Create(Copy(vRepo, 1, Pos('/', vRepo) - 1),
                              Copy(vRepo, Pos('/', vRepo) + 1, MaxInt));
end;

procedure TBaixaDependencia.Logar(const ALinha: string);
begin
  if Assigned(FLog) then
    FLog(ALinha);
end;

function TBaixaDependencia.ResolverVersao: boolean;
var
  vVersoes: TVersoesRAL;
  vVersao, vMelhor: TVersaoRAL;
  vInt: integer;
  vNumerica: TRegExpr;
begin
  Result := FRef <> '';
  if Result then
    Exit;
  FErro := '';
  if not FReceita.PodeBaixar then
  begin
    FErro := Format(emSemDownload, [FReceita.Nome]);
    Exit;
  end;

  vVersoes := TVersoesRAL.Create(True);
  vNumerica := TRegExpr.Create('^[vV]?\d');
  try
    if not FRepo.ListarVersoes(vVersoes) then
    begin
      FErro := FReceita.Nome + ': ' + FRepo.Erro;
      Exit;
    end;

    if not SameText(FVersaoPedida, 'estavel') then
    begin
      // versao pedida: tag, release ou ramo
      vVersao := vVersoes.Buscar(FVersaoPedida);
      FRef := FVersaoPedida;
      FRamo := (vVersao = nil) or (vVersao.Tipo = tvRamo);
      FRelease := (vVersao <> nil) and (vVersao.Tipo in [tvEstavel, tvPreLancamento]);
      Exit(True);
    end;

    vMelhor := vVersoes.Recomendada;
    FRelease := vMelhor <> nil;
    if vMelhor = nil then
      for vInt := 0 to Pred(vVersoes.Count) do
        if (vVersoes[vInt].Tipo = tvTag) and vNumerica.Exec(vVersoes[vInt].Ref) and
           ((vMelhor = nil) or (CompararTags(vVersoes[vInt].Ref, vMelhor.Ref) > 0)) then
          vMelhor := vVersoes[vInt];
    if vMelhor = nil then
      for vInt := 0 to Pred(vVersoes.Count) do
        if (vVersoes[vInt].Tipo = tvRamo) and
           SameText(Copy(vVersoes[vInt].Ref, Length(vVersoes[vInt].Ref) - 6, 7),
                    '-stable') and
           ((vMelhor = nil) or (CompararTags(vVersoes[vInt].Ref, vMelhor.Ref) > 0)) then
          vMelhor := vVersoes[vInt];
    if vMelhor = nil then
    begin
      vMelhor := vVersoes.Buscar('master');
      if vMelhor = nil then
        vMelhor := vVersoes.Buscar('main');
    end;
    if vMelhor = nil then
    begin
      FErro := Format(emSemVersaoEstavel, [FReceita.Nome, FReceita.Github]);
      Exit;
    end;
    FRef := vMelhor.Ref;
    FRamo := vMelhor.Tipo = tvRamo;
    Result := True;
  finally
    vNumerica.Free;
    vVersoes.Free;
  end;
end;

function TBaixaDependencia.Fonte: string;
begin
  Result := FRepo.Dono + '/' + FRepo.Repo;
end;

function TBaixaDependencia.PastaDestino: string;
var
  vVersao: TVersaoRAL;
begin
  Result := '';
  if not ResolverVersao then
    Exit;
  vVersao := TVersaoRAL.Create;
  try
    // de outro repositorio, a pasta diz de qual: dois 'master' nao se misturam
    if FOutroRepo <> '' then
      vVersao.Ref := FRepo.Dono + '-' + FRef
    else
      vVersao.Ref := FRef;
    Result := IncludeTrailingPathDelimiter(FPastaBase) + 'dependencias' + PathDelim +
              FReceita.Nome + PathDelim + vVersao.NomePasta + PathDelim;
  finally
    vVersao.Free;
  end;
end;

function TBaixaDependencia.BaixarExtras: boolean;
var
  vInt: integer;
  vAsset, vDestino, vArquivo, vErro, vURL, vValor, vRelease, vNumero: string;
  vTemVersao: boolean;
  vCli: TClienteHTTP;
  vArq: TFileStream;

  function ReleaseEstavel: string;
  var
    vVersoes: TVersoesRAL;
  begin
    Result := '';
    vVersoes := TVersoesRAL.Create(True);
    try
      if FRepo.ListarVersoes(vVersoes) and (vVersoes.Recomendada <> nil) then
        Result := vVersoes.Recomendada.Ref
      else
        FErro := Format(emSemReleaseExtra, [FReceita.Nome, vAsset]);
    finally
      vVersoes.Free;
    end;
  end;

begin
  Result := True;
  for vInt := 0 to Pred(FReceita.Extras.Count) do
  begin
    vAsset := FReceita.Extras.Names[vInt];
    vValor := FReceita.Extras.ValueFromIndex[vInt];
    vRelease := Copy(vValor, Pos('|', vValor) + 1, MaxInt);
    vTemVersao := Pos('{versao}', LowerCase(vAsset)) > 0;
    vDestino := PastaDestino + ExpandirRaiz(Copy(vValor, 1, Pos('|', vValor) - 1), '');
    // ja extraido (mesma versao ja na pasta)
    if DirectoryExists(vDestino) and
       FileExists(IncludeTrailingPathDelimiter(vDestino) + '.ralinstaller-extra') then
      Continue;

    // de qual release vem o asset: o da propria versao, ou o estavel mais
    // recente quando a fonte e um ramo (o master do mORMot2 usa os estaticos
    // do ultimo release)
    if SameText(vRelease, 'estavel') then
    begin
      vRelease := ReleaseEstavel;
      if vRelease = '' then
        Exit(False);
    end
    else if FRelease then
      vRelease := FRef
    else
    begin
      FErro := Format(emExtraSemRelease, [FReceita.Nome, vAsset, FRef]);
      Exit(False);
    end;
    // F20: o nome do asset traz a versao (libsagui-3.5.2-windows_386.zip)
    if vTemVersao then
    begin
      vNumero := vRelease;
      if (vNumero <> '') and (vNumero[1] in ['v', 'V']) then
        Delete(vNumero, 1, 1);
      vAsset := StringReplace(vAsset, '{versao}', vNumero, [rfReplaceAll, rfIgnoreCase]);
    end;

    // o asset tambem fica no cache: release nao muda
    vArquivo := FRepo.PastaCache + 'zip' + PathDelim + FRepo.Dono + '-' + FRepo.Repo +
                '-' + vRelease + '-' + vAsset;
    if not FileExists(vArquivo) then
    begin
      vURL := Format('https://github.com/%s/releases/download/%s/%s',
                     [FReceita.Github, vRelease, vAsset]);
      Logar(Format(cmBaixandoArquivo, [vAsset]));
      ForceDirectories(ExtractFilePath(vArquivo));
      vCli := TClienteHTTP.Create;
      try
        vCli.OnProgresso := FRepo.OnProgresso;
        vArq := TFileStream.Create(vArquivo + '.parcial', fmCreate);
        try
          Result := vCli.Obter(vURL, vArq);
        finally
          vArq.Free;
        end;
        if not Result then
        begin
          DeleteFile(vArquivo + '.parcial');
          FErro := Format(emFalhaBaixar, [FReceita.Nome, vAsset, vCli.Erro]);
          Exit;
        end;
        RenameFile(vArquivo + '.parcial', vArquivo);
      finally
        vCli.Free;
      end;
    end;

    Logar(Format(cmExtraindoEm, [vAsset, vDestino]));
    if (LowerCase(ExtractFileExt(vAsset)) = '.tgz') or
       (LowerCase(Copy(vAsset, Length(vAsset) - 6, 7)) = '.tar.gz') then
      Result := ExtrairTgz(vArquivo, vDestino, 0, vErro)
    else if LowerCase(ExtractFileExt(vAsset)) = '.zip' then
      Result := ExtrairZip(vArquivo, vDestino, vErro, 0)
    else
    begin
      Result := False;
      vErro := Format(emFormatoExtra, [vAsset]);
    end;
    if not Result then
    begin
      FErro := FReceita.Nome + ': ' + vErro;
      Exit;
    end;
    if vErro <> '' then
      Logar(cmPrefixoAviso + vErro);
    with TStringList.Create do
    try
      Text := vAsset + ' ' + vRelease;
      SaveToFile(IncludeTrailingPathDelimiter(vDestino) + '.ralinstaller-extra');
    finally
      Free;
    end;
  end;
end;

function TBaixaDependencia.GarantirPastaSemCodigo: boolean;
var
  vPasta: string;
  vMarca: TJSONObject;
begin
  Result := False;
  vPasta := PastaDestino;
  if PastaDeOutro(vPasta) then
  begin
    FErro := Format(emPastaDeOutro, [vPasta]);
    Exit;
  end;
  if not FileExists(vPasta + ArquivoMarca) then
  begin
    ForceDirectories(vPasta);
    vMarca := TJSONObject.Create;
    try
      vMarca.Add('repositorio', Fonte);
      vMarca.Add('versao', FRef);
      vMarca.Add('commit', '');
      vMarca.Add('data', FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', Now));
      vMarca.Add('aviso', 'Pasta criada pelo RAL Installer.');
      with TStringList.Create do
      try
        Text := vMarca.FormatJSON;
        SaveToFile(vPasta + ArquivoMarca);
      finally
        Free;
      end;
    finally
      vMarca.Free;
    end;
  end;
  Result := True;
end;

function TBaixaDependencia.Executar: boolean;
var
  vPreparo: TPreparoFontes;
begin
  Result := False;
  FErro := '';
  if not ResolverVersao then
    Exit;

  // F20: so os arquivos do release (as DLLs), sem o codigo: a pasta ganha a
  // marca do instalador do mesmo jeito, para a limpeza (F19) a reconhecer
  if not FReceita.Codigo then
  begin
    if not GarantirPastaSemCodigo then
      Exit;
    Result := BaixarExtras;
    Exit;
  end;

  FRepo.Log := FLog;
  vPreparo := TPreparoFontes.Create(FRepo, FRef, FRamo);
  try
    vPreparo.Nome := FReceita.Nome;
    vPreparo.Destino := PastaDestino;
    vPreparo.Log := FLog;
    if not vPreparo.Executar then
    begin
      FErro := FReceita.Nome + ': ' + vPreparo.Erro;
      Exit;
    end;
  finally
    vPreparo.Free;
    FRepo.Log := nil;
  end;
  Result := BaixarExtras;
end;

end.
