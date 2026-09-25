/// The sources of a RAL version in the folder the user chose:
/// <folder>/PascalRAL/<version>/, with the submodules the chosen packages use,
/// each at the commit RAL pinned. The folder is permanent (the library path and
/// the IDE links point to it), so the swap is careful: everything is extracted
/// into a folder beside it, and only at the end it takes the place of the
/// previous one. A failure halfway leaves the working installation as it was. A
/// folder that already exists and was not created by the installer (without the
/// mark file) is never touched.
unit RALInst.Fontes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALInst.GitHub, RALInst.Processo;

type
  /// Puts one version of a GitHub repository (with its submodules) into its
  /// permanent folder.
  TPreparoFontes = class
  private
    FCommit: string;
    FDestino: string;
    FErro: string;
    FLog: TLogLinha;
    FNome: string;
    FPastaBase: string;
    FRamo: boolean;
    FRef: string;
    FRepo: TRepoGitHub;
    FSubmodulos: TStringList;
    /// Downloads and extracts the missing submodules into the folder
    function BaixarSubmodulos(const APasta: string; AFaltando: TStrings;
      AInstalados: TStrings): boolean;
    /// Writes the installer's mark file into the folder
    procedure GravarMarca(const APasta: string; ASubmodulos: TStrings);
    /// Reads the mark file: False when the folder has none
    function LerMarca(const APasta: string; out ACommit: string;
      ASubmodulos: TStrings): boolean;
    /// Passes the line to Log when assigned
    procedure Logar(const ALinha: string);
  public
    constructor Create(ARepo: TRepoGitHub; const ARef: string; ARamo: boolean);
    destructor Destroy; override;
    /// Downloads (or reuses) and swaps the folder in
    function Executar: boolean;
    /// <PastaBase>/PascalRAL/<version>/
    function PastaDestino: string;

    /// Commit of the sources left in the folder
    property Commit: string read FCommit;
    /// Complete final folder, instead of <PastaBase>/PascalRAL/<version>: the
    /// dependencies go to <folder>/dependencias/<name>/<version>
    property Destino: string read FDestino write FDestino;
    property Erro: string read FErro;
    property Log: TLogLinha read FLog write FLog;
    /// For the log ('PascalRAL', 'mORMot2')
    property Nome: string read FNome write FNome;
    property PastaBase: string read FPastaBase write FPastaBase;
    /// Paths of the submodules the chosen packages use ('src/others/ZSTD'); the
    /// others are not downloaded
    property Submodulos: TStringList read FSubmodulos;
  end;

const
  /// The file saying the folder was created by the installer, and with what
  ArquivoMarca = '.ralinstaller.json';

/// Deletes a whole folder; False when something was left
function ApagarPasta(const APasta: string): boolean;
/// Where the sources of the folder came from (the installer's mark):
/// repository, version and commit; empty when the folder has no mark (a local
/// folder of the user)
procedure LerMarca(const APasta: string; out ARepositorio, AVersao, ACommit: string);
/// The folder exists, has content and was not created by the installer: it is
/// never touched
function PastaDeOutro(const APasta: string): boolean;

implementation

uses
  fpjson, jsonparser,
  RALInst.Mensagens, RALInst.Zip;

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
  Result := RemoveDir(CaminhoLongo(APasta)) or
            not DirectoryExists(CaminhoLongo(APasta));
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
  if FindFirst(IncludeTrailingPathDelimiter(APasta) + AllFilesMask, faAnyFile,
               vBusca) = 0 then
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
  Result := (APasta <> '') and DirectoryExists(APasta) and
            not PastaVazia(APasta) and
            not FileExists(IncludeTrailingPathDelimiter(APasta) + ArquivoMarca);
end;

{ TPreparoFontes }

constructor TPreparoFontes.Create(ARepo: TRepoGitHub; const ARef: string;
  ARamo: boolean);
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
      FErro := Format(emSubmodulosCommit, [FRepo.Erro]);
      Exit(False);
    end;
    if FRepo.Aviso <> '' then
      Logar(cmPrefixoAviso + FRepo.Aviso);
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
          Logar(Format(wmSubmoduloNaoExiste, [vCaminho, FRef]));
          Continue;
        end;
        FErro := Format(emSubmoduloNaoDeclarado, [FRef, vCaminho]);
        Exit(False);
      end;

      vURL := URLDoSubmodulo(vGitmodules, vCaminho);
      if not RepoDaURL(vURL, vDono, vNome) then
      begin
        FErro := Format(emSubmoduloForaGitHub, [vCaminho, vURL]);
        Exit(False);
      end;

      Logar(Format(cmSubmodulo, [vCaminho, vDono, vNome, Copy(vSha, 1, 7)]));
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
      if not ExtrairZip(vZip, APasta + StringReplace(vCaminho, '/', PathDelim,
                                                     [rfReplaceAll]), vErro) then
      begin
        FErro := vErro;
        Exit(False);
      end;
      if vErro <> '' then
        Logar(cmPrefixoAviso + vErro);
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
      FErro := Format(emPastaDeOutro, [vDestino]);
      Exit;
    end;

    // 1. o zip da versao (normalmente ja no cache, das telas de escolha)
    Logar(Format(cmFontes, [FNome, FRef, vDestino]));
    if not FRepo.BaixarZip(FRef, False, vZip) then
    begin
      FErro := FRepo.Erro;
      Exit;
    end;
    FCommit := ComentarioZip(vZip);

    // 2. mesma versao ja na pasta: so o que faltar de submodulo
    if DirectoryExists(vDestino) and (FCommit <> '') and
       SameText(vCommitAtual, FCommit) then
    begin
      Logar(Format(cmFontesJaNaPasta, [Copy(FCommit, 1, 7)]));
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
      FErro := Format(emLimpar, [vNova]);
      Exit;
    end;
    Logar(Format(cmExtraindo, [ExtractFileName(vZip)]));
    if not ExtrairZip(vZip, vNova, vErro) then
    begin
      FErro := vErro;
      ApagarPasta(vNova);
      Exit;
    end;
    if vErro <> '' then
      Logar(cmPrefixoAviso + vErro);

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
      if not RenameFile(ExcludeTrailingPathDelimiter(vDestino),
                        ExcludeTrailingPathDelimiter(vAnterior)) then
      begin
        FErro := Format(emSubstituirPasta, [vDestino, vNova]);
        Exit;
      end;
    end;
    if not RenameFile(ExcludeTrailingPathDelimiter(vNova),
                      ExcludeTrailingPathDelimiter(vDestino)) then
    begin
      // devolve a anterior
      if DirectoryExists(vAnterior) then
        RenameFile(ExcludeTrailingPathDelimiter(vAnterior),
                   ExcludeTrailingPathDelimiter(vDestino));
      FErro := Format(emCriar, [vDestino]);
      Exit;
    end;
    if DirectoryExists(vAnterior) and not ApagarPasta(vAnterior) then
      Logar(Format(wmVersaoAnteriorFicou, [vAnterior]));

    Logar(Format(cmFontesPronto, [Copy(FCommit, 1, 7), vInstalados.Count]));
    Result := True;
  finally
    vFaltando.Free;
    vInstalados.Free;
  end;
end;

end.
