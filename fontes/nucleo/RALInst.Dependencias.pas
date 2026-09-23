unit RALInst.Dependencias;

{$mode ObjFPC}{$H+}

// F7: o download de uma dependencia descrita por receita, para
// <pasta>/dependencias/<nome>/<versao>/ — o mesmo cuidado da pasta do RAL
// (marca, extracao ao lado e troca no fim; pasta alheia nunca e tocada), mais
// os extras que a receita pede (os estaticos do mORMot2, num asset do release).
//
// O que fazer com ela em cada IDE e das unidades de instalacao
// (RALInst.Instalar.Delphi, RALInst.Instalar.Lazarus): e la que esta o
// registro e o lazbuild.

interface

uses
  Classes, SysUtils, RALInst.Receitas, RALInst.GitHub, RALInst.HTTP, RALInst.Processo;

type

  { TBaixaDependencia }

  TBaixaDependencia = class
  private
    FReceita: TReceita;
    FRepo: TRepoGitHub;
    FPastaBase: string;
    FRef: string;
    FVersaoPedida: string;
    // outro repositorio pedido pela regra de versao ('frones/ZeosLib')
    FOutroRepo: string;
    FRamo: boolean;
    FRelease: boolean;
    FErro: string;
    FLog: TLogLinha;
    procedure Logar(const ALinha: string);
    function BaixarExtras: boolean;
    procedure SetVersaoPedida(const AValor: string);
  public
    constructor Create(AReceita: TReceita);
    destructor Destroy; override;

    // 'estavel' vira a versao de verdade: a release estavel mais alta; sem
    // release, a tag numerica mais alta; sem tag, o ramo '-stable' mais alto
    // (o Zeos); por ultimo master/main
    function ResolverVersao: boolean;
    // <PastaBase>/dependencias/<nome>/<versao>/; resolve a versao se preciso
    function PastaDestino: string;
    function Executar: boolean;

    property Receita: TReceita read FReceita;
    property PastaBase: string read FPastaBase write FPastaBase;
    property Ref: string read FRef;
    // 'dono/repo' de onde vem esta versao
    function Fonte: string;
    // F6: a versao pedida ('estavel', ramo ou tag); por padrao a da receita,
    // mas a IDE pode pedir outra (o Zeos 8.0-patches para o FPC 3.3), e ate de
    // outro repositorio: 'frones/ZeosLib:master' (VersaoComFonte)
    property VersaoPedida: string read FVersaoPedida write SetVersaoPedida;
    property Erro: string read FErro;
    property Log: TLogLinha read FLog write FLog;
    property Repo: TRepoGitHub read FRepo;
  end;

implementation

uses
  RALInst.Fontes, RALInst.Zip, RALInst.Tar, RegExpr;

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
    FErro := FReceita.Nome + ' não tem download';
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
           SameText(Copy(vVersoes[vInt].Ref, Length(vVersoes[vInt].Ref) - 6, 7), '-stable') and
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
      FErro := FReceita.Nome + ': não achei versão estável em ' + FReceita.Github;
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
  vAsset, vDestino, vArquivo, vErro, vURL, vValor, vRelease: string;
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
        FErro := FReceita.Nome + ': não achei release estável para o extra ' + vAsset;
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
    vDestino := PastaDestino + ExpandirRaiz(Copy(vValor, 1, Pos('|', vValor) - 1), '');
    // ja extraido (mesma versao ja na pasta)
    if DirectoryExists(vDestino) and (FileExists(IncludeTrailingPathDelimiter(vDestino) +
       '.ralinstaller-extra')) then
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
      FErro := Format('%s: o extra %s vem de um release, e a versão %s não é um release',
                      [FReceita.Nome, vAsset, FRef]);
      Exit(False);
    end;

    // o asset tambem fica no cache: release nao muda
    vArquivo := FRepo.PastaCache + 'zip' + PathDelim + FRepo.Dono + '-' + FRepo.Repo +
                '-' + vRelease + '-' + vAsset;
    if not FileExists(vArquivo) then
    begin
      vURL := Format('https://github.com/%s/releases/download/%s/%s',
                     [FReceita.Github, vRelease, vAsset]);
      Logar('  baixando ' + vAsset);
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
          FErro := Format('%s: falha ao baixar %s: %s', [FReceita.Nome, vAsset, vCli.Erro]);
          Exit;
        end;
        RenameFile(vArquivo + '.parcial', vArquivo);
      finally
        vCli.Free;
      end;
    end;

    Logar('  extraindo ' + vAsset + ' em ' + vDestino);
    if (LowerCase(ExtractFileExt(vAsset)) = '.tgz') or
       (LowerCase(Copy(vAsset, Length(vAsset) - 6, 7)) = '.tar.gz') then
      Result := ExtrairTgz(vArquivo, vDestino, 0, vErro)
    else if LowerCase(ExtractFileExt(vAsset)) = '.zip' then
      Result := ExtrairZip(vArquivo, vDestino, vErro, 0)
    else
    begin
      Result := False;
      vErro := 'formato do extra não suportado (só .zip, .tgz, .tar.gz): ' + vAsset;
    end;
    if not Result then
    begin
      FErro := FReceita.Nome + ': ' + vErro;
      Exit;
    end;
    if vErro <> '' then
      Logar('  aviso: ' + vErro);
    with TStringList.Create do
    try
      Text := vAsset + ' ' + vRelease;
      SaveToFile(IncludeTrailingPathDelimiter(vDestino) + '.ralinstaller-extra');
    finally
      Free;
    end;
  end;
end;

function TBaixaDependencia.Executar: boolean;
var
  vPreparo: TPreparoFontes;
begin
  Result := False;
  FErro := '';
  if not ResolverVersao then
    Exit;

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
