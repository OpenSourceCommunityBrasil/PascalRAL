/// Safety copy of a migration (decision D-M3): before a single byte is
/// written, the files that will change go into
/// '<project>/_ral-migracao/<from>-<to>-<yyyymmdd-hhnnss>.zip', together with a
/// manifest (size and SHA-256 of each file, and the files that did not exist,
/// which the migration will create). Undo restores the bytes from the zip and
/// deletes the files the migration created. RegistrarDepois, called after the
/// migration wrote its files, keeps their hashes in a text file beside the zip,
/// so that Desfazer can tell which files were changed by hand after the
/// migration (they are overwritten anyway, but the caller can warn).
unit RALMig.Copia;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Zipper;

const
  /// Folder, inside the project, where the copies are kept
  PASTA_COPIAS = '_ral-migracao';
  /// Name of the manifest inside the zip
  NOME_MANIFESTO = '_manifesto.txt';

/// Creates the zip with the files (paths relative to the project folder; the
/// ones that do not exist yet are recorded as new). AZip receives its full
/// path. False (with AErro) when nothing was written: the caller must not go on
function CriarCopia(const APastaProjeto, ADe, APara: string; AArquivos: TStrings;
  out AZip, AErro: string): boolean;
/// Keeps the hashes of the files as they are now (after the migration)
function RegistrarDepois(const AZip, APastaProjeto: string; out AErro: string): boolean;
/// Keeps the hashes of the report files the migration wrote after the copy
/// (paths relative to the project folder), in the same text file as
/// RegistrarDepois, under a section of their own: older copies simply do not
/// have it. Call it after RegistrarDepois
function RegistrarRelatorios(const AZip, APastaProjeto: string; ARelativos: TStrings;
  out AErro: string): boolean;
/// The reports registered by RegistrarRelatorios that were changed after the
/// migration: Desfazer keeps them (it only removes the untouched ones)
procedure ListarRelatoriosMantidos(const AZip, APastaProjeto: string; AMantidos: TStrings);
/// Deletes the zip and its text file after a successful undo, and the copies
/// folder when nothing else is left in it
procedure DescartarCopia(const AZip, APastaProjeto: string);
/// The zips of the project, newest first (full paths); the caller frees the list
function ListarCopias(const APastaProjeto: string): TStringList;
/// Restores the files and deletes the new ones. AMudados (may be nil) receives
/// the files changed after the migration, already overwritten, when
/// RegistrarDepois was called. False (with AErro) when something was not
/// restored exactly
function Desfazer(const AZip, APastaProjeto: string; AMudados: TStrings;
  out AErro: string): boolean;
/// Is the folder inside a git repository with changes not committed yet? No git
/// installed or not a repository = False, with no error. ADetalhe lists the
/// first changed files
function GitComMudancas(const APasta: string; out ADetalhe: string): boolean;

implementation

uses
  RALInst.Processo, RALInst.SHA256, RALMig.Mensagens;

const
  ASSINATURA = 'ral-migracao 1';
  PREFIXO_ZIP = 'arquivos/';
  SECAO_EXISTENTES = '[existentes]';
  SECAO_NOVOS = '[novos]';
  SUFIXO_DEPOIS = '.depois.txt';
  SECAO_RELATORIOS = '[relatorios]';

{ Paths }

/// Relative path with '/', or '' when it is absolute or leaves the project
function CaminhoRelativo(const ACaminho: string): string;
begin
  Result := StringReplace(Trim(ACaminho), '\', '/', [rfReplaceAll]);
  while Copy(Result, 1, 2) = './' do
    Delete(Result, 1, 2);
  if (Result = '') or (Result[1] = '/') or (Pos(':', Result) > 0) or
     (Pos('..', Result) > 0) then
    Result := '';
end;

function CaminhoNoDisco(const APasta, ARelativo: string): string;
begin
  Result := IncludeTrailingPathDelimiter(APasta) +
            StringReplace(ARelativo, '/', PathDelim, [rfReplaceAll]);
end;

function PastaCopias(const APastaProjeto: string): string;
begin
  Result := IncludeTrailingPathDelimiter(APastaProjeto) + PASTA_COPIAS;
end;

/// Letters, digits, '.', '_' and '-' only: the version goes into a file name
function NomeSeguro(const ATexto: string): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 1 to Length(ATexto) do
    if ATexto[vInt] in ['0'..'9', 'a'..'z', 'A'..'Z', '.', '_', '-'] then
      Result := Result + ATexto[vInt]
    else
      Result := Result + '_';
  if Result = '' then
    Result := 'x';
end;

function CaminhoDoDepois(const AZip: string): string;
begin
  Result := ChangeFileExt(AZip, '') + SUFIXO_DEPOIS;
end;

function TamanhoDoArquivo(const AArquivo: string): Int64;
var
  vFluxo: TFileStream;
begin
  vFluxo := TFileStream.Create(CaminhoLongo(AArquivo), fmOpenRead or fmShareDenyNone);
  try
    Result := vFluxo.Size;
  finally
    vFluxo.Free;
  end;
end;

procedure GravarTexto(const AArquivo, ATexto: string);
var
  vFluxo: TFileStream;
begin
  vFluxo := TFileStream.Create(AArquivo, fmCreate);
  try
    if ATexto <> '' then
      vFluxo.WriteBuffer(ATexto[1], Length(ATexto));
  finally
    vFluxo.Free;
  end;
end;

{ Zip reading }

type
  /// Reads some entries of a zip into memory, by name
  TLeitorZip = class
  public
    Nomes: TStringList;
    Fluxos: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure CriarStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure FecharStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    function Texto(const ANome: string): string;
  end;

constructor TLeitorZip.Create;
begin
  inherited Create;
  Nomes := TStringList.Create;
  Fluxos := TStringList.Create;
  Fluxos.OwnsObjects := True;
end;

destructor TLeitorZip.Destroy;
begin
  Fluxos.Free;
  Nomes.Free;
  inherited Destroy;
end;

procedure TLeitorZip.CriarStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var
  vMem: TMemoryStream;
begin
  vMem := TMemoryStream.Create;
  Fluxos.AddObject(AItem.ArchiveFileName, vMem);
  AStream := vMem;
end;

procedure TLeitorZip.FecharStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  // o stream fica em Fluxos
  AStream := nil;
end;

function TLeitorZip.Texto(const ANome: string): string;
var
  vIdx: integer;
  vMem: TMemoryStream;
begin
  Result := '';
  vIdx := Fluxos.IndexOf(ANome);
  if vIdx < 0 then
    Exit;
  vMem := TMemoryStream(Fluxos.Objects[vIdx]);
  if vMem.Size > 0 then
    SetString(Result, PAnsiChar(vMem.Memory), vMem.Size);
end;

/// Text of one entry of the zip; raises when the zip cannot be read
function LerEntrada(const AZip, ANome: string): string;
var
  vZip: TUnZipper;
  vLeitor: TLeitorZip;
  vLista: TStringList;
begin
  vZip := TUnZipper.Create;
  vLeitor := TLeitorZip.Create;
  vLista := TStringList.Create;
  try
    vLista.Add(ANome);
    vZip.FileName := AZip;
    vZip.OnCreateStream := @vLeitor.CriarStream;
    vZip.OnDoneStream := @vLeitor.FecharStream;
    vZip.UnZipFiles(vLista);
    Result := vLeitor.Texto(ANome);
  finally
    vLista.Free;
    vLeitor.Free;
    vZip.Free;
  end;
end;

{ Manifest }

type
  /// What the manifest says: existing files ('rel' = 'size'#9'hash') and new ones
  TManifesto = class
  public
    Existentes: TStringList;
    Novos: TStringList;
    constructor Create;
    destructor Destroy; override;
    function Carregar(const ATexto: string): boolean;
  end;

constructor TManifesto.Create;
begin
  inherited Create;
  Existentes := TStringList.Create;
  Novos := TStringList.Create;
end;

destructor TManifesto.Destroy;
begin
  Novos.Free;
  Existentes.Free;
  inherited Destroy;
end;

function TManifesto.Carregar(const ATexto: string): boolean;
var
  vLinhas: TStringList;
  vInt, vTab1, vTab2: integer;
  vSecao, vLinha: string;
begin
  Result := False;
  vLinhas := TStringList.Create;
  try
    vLinhas.Text := ATexto;
    if (vLinhas.Count = 0) or (Trim(vLinhas[0]) <> ASSINATURA) then
      Exit;
    vSecao := '';
    for vInt := 1 to Pred(vLinhas.Count) do
    begin
      vLinha := vLinhas[vInt];
      if (vLinha = SECAO_EXISTENTES) or (vLinha = SECAO_NOVOS) then
        vSecao := vLinha
      else if (vLinha <> '') and (vSecao = SECAO_NOVOS) then
        Novos.Add(vLinha)
      else if (vLinha <> '') and (vSecao = SECAO_EXISTENTES) then
      begin
        vTab1 := Pos(#9, vLinha);
        vTab2 := Pos(#9, Copy(vLinha, vTab1 + 1, MaxInt)) + vTab1;
        if (vTab1 > 0) and (vTab2 > vTab1) then
          Existentes.Values[Copy(vLinha, vTab2 + 1, MaxInt)] :=
            Copy(vLinha, 1, vTab2 - 1);
      end;
    end;
    Result := vSecao <> '';
  finally
    vLinhas.Free;
  end;
end;

/// The manifest of the zip; False with the message in AErro
function LerManifesto(const AZip: string; AManifesto: TManifesto;
  out AErro: string): boolean;
var
  vTexto: string;
begin
  Result := False;
  AErro := '';
  if not FileExists(AZip) then
  begin
    AErro := Format(emCopiaNaoExiste, [AZip]);
    Exit;
  end;
  try
    vTexto := LerEntrada(AZip, NOME_MANIFESTO);
  except
    on E: Exception do
    begin
      AErro := Format(emCopiaIlegivel, [ExtractFileName(AZip), E.Message]);
      Exit;
    end;
  end;
  if (vTexto = '') or not AManifesto.Carregar(vTexto) then
    AErro := Format(emCopiaSemManifesto, [ExtractFileName(AZip)])
  else
    Result := True;
end;

{ CriarCopia }

/// Names of the files, normalized and without repeats; '' in AInvalido when ok
procedure NormalizarLista(AArquivos: TStrings; ALista: TStringList; out AInvalido: string);
var
  vInt: integer;
  vRel: string;
begin
  AInvalido := '';
  ALista.Sorted := True;
  ALista.CaseSensitive := {$IFDEF MSWINDOWS} False {$ELSE} True {$ENDIF};
  ALista.Duplicates := dupIgnore;
  for vInt := 0 to Pred(AArquivos.Count) do
  begin
    if Trim(AArquivos[vInt]) = '' then
      Continue;
    vRel := CaminhoRelativo(AArquivos[vInt]);
    if vRel = '' then
    begin
      AInvalido := AArquivos[vInt];
      Exit;
    end;
    ALista.Add(vRel);
  end;
end;

function TextoManifesto(const ADe, APara, APasta: string; ALista: TStrings): string;
var
  vInt: integer;
  vExistentes, vNovos, vArq: string;
begin
  vExistentes := '';
  vNovos := '';
  for vInt := 0 to Pred(ALista.Count) do
  begin
    vArq := CaminhoNoDisco(APasta, ALista[vInt]);
    if FileExists(vArq) then
      vExistentes := vExistentes + IntToStr(TamanhoDoArquivo(vArq)) + #9 +
                     SHA256Arquivo(vArq) + #9 + ALista[vInt] + #10
    else
      vNovos := vNovos + ALista[vInt] + #10;
  end;
  Result := ASSINATURA + #10 + 'de=' + ADe + #10 + 'para=' + APara + #10 +
            'criado=' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + #10 +
            SECAO_EXISTENTES + #10 + vExistentes + SECAO_NOVOS + #10 + vNovos;
end;

/// A free name for the zip; the second one in the same second gets '-2', etc.
function NomeDoZip(const APastaProjeto, ADe, APara: string): string;
var
  vBase: string;
  vNum: integer;
begin
  vBase := PastaCopias(APastaProjeto) + PathDelim + NomeSeguro(ADe) + '-' +
           NomeSeguro(APara) + '-' + FormatDateTime('yyyymmdd-hhnnss', Now);
  Result := vBase + '.zip';
  vNum := 1;
  while FileExists(Result) do
  begin
    Inc(vNum);
    Result := vBase + '-' + IntToStr(vNum) + '.zip';
  end;
end;

function CriarCopia(const APastaProjeto, ADe, APara: string; AArquivos: TStrings;
  out AZip, AErro: string): boolean;
var
  vLista: TStringList;
  vZip: TZipper;
  vManifesto: TStringStream;
  vInt: integer;
  vInvalido, vArq: string;
begin
  Result := False;
  AZip := '';
  AErro := '';
  if not DirectoryExists(APastaProjeto) then
  begin
    AErro := Format(emCopiaSemPasta, [APastaProjeto]);
    Exit;
  end;
  vLista := TStringList.Create;
  try
    NormalizarLista(AArquivos, vLista, vInvalido);
    if vInvalido <> '' then
      AErro := Format(emCopiaCaminhoInvalido, [vInvalido])
    else if vLista.Count = 0 then
      AErro := emCopiaSemArquivos;
    if AErro <> '' then
      Exit;

    AZip := NomeDoZip(APastaProjeto, ADe, APara);
    vZip := TZipper.Create;
    vManifesto := nil;
    try
      try
        ForceDirectories(PastaCopias(APastaProjeto));
        vManifesto := TStringStream.Create(TextoManifesto(ADe, APara, APastaProjeto, vLista));
        vZip.Entries.AddFileEntry(vManifesto, NOME_MANIFESTO);
        for vInt := 0 to Pred(vLista.Count) do
        begin
          vArq := CaminhoNoDisco(APastaProjeto, vLista[vInt]);
          if FileExists(vArq) then
            vZip.Entries.AddFileEntry(vArq, PREFIXO_ZIP + vLista[vInt]);
        end;
        vZip.FileName := AZip;
        vZip.ZipAllFiles;
        Result := FileExists(AZip);
        if not Result then
          AErro := Format(emCopiaNaoCriada, [AZip, '?']);
      except
        on E: Exception do
          AErro := Format(emCopiaNaoCriada, [AZip, E.Message]);
      end;
    finally
      vZip.Free;
      vManifesto.Free;
    end;
  finally
    vLista.Free;
  end;
  if not Result then
  begin
    if FileExists(AZip) then
      DeleteFile(AZip);
    AZip := '';
  end;
end;

{ RegistrarDepois }

function RegistrarDepois(const AZip, APastaProjeto: string; out AErro: string): boolean;
var
  vManifesto: TManifesto;
  vTexto, vArq: string;
  vInt: integer;

  procedure Registrar(const ARel: string);
  begin
    vArq := CaminhoNoDisco(APastaProjeto, ARel);
    if FileExists(vArq) then
      vTexto := vTexto + SHA256Arquivo(vArq) + #9 + ARel + #10
    else
      vTexto := vTexto + '-' + #9 + ARel + #10;
  end;

begin
  vManifesto := TManifesto.Create;
  try
    Result := LerManifesto(AZip, vManifesto, AErro);
    if not Result then
      Exit;
    vTexto := '';
    for vInt := 0 to Pred(vManifesto.Existentes.Count) do
      Registrar(vManifesto.Existentes.Names[vInt]);
    for vInt := 0 to Pred(vManifesto.Novos.Count) do
      Registrar(vManifesto.Novos[vInt]);
    try
      GravarTexto(CaminhoDoDepois(AZip), vTexto);
    except
      on E: Exception do
      begin
        AErro := Format(emCopiaSemRegistro, [E.Message]);
        Result := False;
      end;
    end;
  finally
    vManifesto.Free;
  end;
end;

{ ListarCopias }

/// Sort key of a copy: the date-time stamp and the repetition number
function ChaveDaCopia(const AArquivo: string): string;
var
  vNome: string;
  vInt, vPos, vNum: integer;

  function EhStamp(AIni: integer): boolean;
  var
    vJ: integer;
  begin
    Result := AIni + 14 <= Length(vNome);
    if not Result then
      Exit;
    for vJ := 0 to 14 do
      if vJ = 8 then
        Result := Result and (vNome[AIni + vJ] = '-')
      else
        Result := Result and (vNome[AIni + vJ] in ['0'..'9']);
  end;

begin
  vNome := ChangeFileExt(ExtractFileName(AArquivo), '');
  vPos := 0;
  for vInt := Length(vNome) - 14 downto 1 do
    if EhStamp(vInt) then
    begin
      vPos := vInt;
      Break;
    end;
  if vPos = 0 then
    Exit('0' + vNome);
  vNum := StrToIntDef(Copy(vNome, vPos + 16, MaxInt), 1);
  Result := '1' + Copy(vNome, vPos, 15) + Format('%.6d', [vNum]);
end;

function ListarCopias(const APastaProjeto: string): TStringList;
var
  vBusca: TSearchRec;
  vOrdenada: TStringList;
  vInt: integer;
  vPasta: string;
begin
  Result := TStringList.Create;
  vOrdenada := TStringList.Create;
  try
    vOrdenada.Sorted := True;
    vOrdenada.Duplicates := dupAccept;
    vPasta := PastaCopias(APastaProjeto) + PathDelim;
    if FindFirst(vPasta + '*.zip', faAnyFile and not faDirectory, vBusca) = 0 then
    begin
      repeat
        vOrdenada.Add(ChaveDaCopia(vBusca.Name) + #1 + vPasta + vBusca.Name);
      until FindNext(vBusca) <> 0;
      FindClose(vBusca);
    end;
    for vInt := Pred(vOrdenada.Count) downto 0 do
      Result.Add(Copy(vOrdenada[vInt], Pos(#1, vOrdenada[vInt]) + 1, MaxInt));
  finally
    vOrdenada.Free;
  end;
end;

{ Desfazer }

type
  /// Writes the 'arquivos/...' entries of the zip into the project
  TRestaurador = class
  public
    Pasta: string;
    Permitidos: TStringList;
    Erro: string;
    Fluxo: TFileStream;
    constructor Create;
    destructor Destroy; override;
    procedure CriarStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure FecharStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
  end;

constructor TRestaurador.Create;
begin
  inherited Create;
  Permitidos := TStringList.Create;
  Permitidos.CaseSensitive := {$IFDEF MSWINDOWS} False {$ELSE} True {$ENDIF};
end;

destructor TRestaurador.Destroy;
begin
  Permitidos.Free;
  inherited Destroy;
end;

procedure TRestaurador.CriarStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var
  vRel, vArq: string;
begin
  // o que nao e restaurado vai para a memoria: com AStream nil o TUnZipper
  // grava o arquivo ele mesmo, na pasta atual
  AStream := TMemoryStream.Create;
  Fluxo := nil;
  vRel := StringReplace(AItem.ArchiveFileName, '\', '/', [rfReplaceAll]);
  if Copy(vRel, 1, Length(PREFIXO_ZIP)) <> PREFIXO_ZIP then
    Exit;
  vRel := Copy(vRel, Length(PREFIXO_ZIP) + 1, MaxInt);
  if (CaminhoRelativo(vRel) = '') or (Permitidos.IndexOf(vRel) < 0) then
  begin
    if Erro = '' then
      Erro := Format(emCopiaCaminhoInvalido, [AItem.ArchiveFileName]);
    Exit;
  end;
  vArq := CaminhoNoDisco(Pasta, vRel);
  try
    CriarPastas(ExtractFilePath(vArq));
    Fluxo := TFileStream.Create(CaminhoLongo(vArq), fmCreate);
    AStream.Free;
    AStream := Fluxo;
  except
    on E: Exception do
      if Erro = '' then
        Erro := Format(emCopiaNaoRestaurada, [vRel, E.Message]);
  end;
end;

procedure TRestaurador.FecharStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  FreeAndNil(AStream);
  Fluxo := nil;
end;

/// Hashes recorded by RegistrarDepois that differ from the files now
procedure ListarMudadosDepois(const AZip, APastaProjeto: string; AMudados: TStrings);
var
  vLinhas: TStringList;
  vInt, vTab: integer;
  vRel, vHash, vAtual, vArq: string;
begin
  if (AMudados = nil) or not FileExists(CaminhoDoDepois(AZip)) then
    Exit;
  vLinhas := TStringList.Create;
  try
    vLinhas.LoadFromFile(CaminhoDoDepois(AZip));
    for vInt := 0 to Pred(vLinhas.Count) do
    begin
      if vLinhas[vInt] = SECAO_RELATORIOS then
        Break;
      vTab := Pos(#9, vLinhas[vInt]);
      if vTab = 0 then
        Continue;
      vHash := Copy(vLinhas[vInt], 1, vTab - 1);
      vRel := Copy(vLinhas[vInt], vTab + 1, MaxInt);
      vArq := CaminhoNoDisco(APastaProjeto, vRel);
      if FileExists(vArq) then
        vAtual := SHA256Arquivo(vArq)
      else
        vAtual := '-';
      if vAtual <> vHash then
        AMudados.Add(vRel);
    end;
  finally
    vLinhas.Free;
  end;
end;

/// Removes the folders the migration may have created, while they are empty
procedure LimparPastasVazias(const APasta, ARelativo: string);
var
  vDir, vRaiz: string;
begin
  vRaiz := ExcludeTrailingPathDelimiter(APasta);
  vDir := ExcludeTrailingPathDelimiter(ExtractFilePath(CaminhoNoDisco(APasta, ARelativo)));
  while (Length(vDir) > Length(vRaiz)) and RemoveDir(vDir) do
    vDir := ExcludeTrailingPathDelimiter(ExtractFilePath(vDir));
end;

/// The restored files must have the size and hash of the manifest
function ConferirRestaurados(const APasta: string; AManifesto: TManifesto;
  out AErro: string): boolean;
var
  vInt, vTab: integer;
  vRel, vInfo, vArq: string;
begin
  Result := True;
  AErro := '';
  for vInt := 0 to Pred(AManifesto.Existentes.Count) do
  begin
    vRel := AManifesto.Existentes.Names[vInt];
    vInfo := AManifesto.Existentes.ValueFromIndex[vInt];
    vTab := Pos(#9, vInfo);
    vArq := CaminhoNoDisco(APasta, vRel);
    if not FileExists(vArq) or (SHA256Arquivo(vArq) <> Copy(vInfo, vTab + 1, MaxInt)) then
    begin
      AErro := Format(emCopiaDiferente, [vRel]);
      Exit(False);
    end;
  end;
end;

{ Reports }

/// Lines 'hash'#9'rel' of the reports section of the text file of the zip
procedure LerRelatorios(const AZip: string; ALinhas: TStringList);
var
  vTodas: TStringList;
  vInt: integer;
  vNaSecao: boolean;
begin
  if not FileExists(CaminhoDoDepois(AZip)) then
    Exit;
  vTodas := TStringList.Create;
  try
    vTodas.LoadFromFile(CaminhoDoDepois(AZip));
    vNaSecao := False;
    for vInt := 0 to Pred(vTodas.Count) do
      if vTodas[vInt] = SECAO_RELATORIOS then
        vNaSecao := True
      else if vNaSecao and (Pos(#9, vTodas[vInt]) > 0) then
        ALinhas.Add(vTodas[vInt]);
  finally
    vTodas.Free;
  end;
end;

function RelatorioDaLinha(const ALinha: string): string;
begin
  Result := Copy(ALinha, Pos(#9, ALinha) + 1, MaxInt);
end;

/// Is the file still the one the migration wrote (same hash as registered)?
function RelatorioIntacto(const APasta, ALinha: string): boolean;
var
  vArq: string;
begin
  vArq := CaminhoNoDisco(APasta, RelatorioDaLinha(ALinha));
  Result := FileExists(vArq) and
    (SHA256Arquivo(vArq) = Copy(ALinha, 1, Pos(#9, ALinha) - 1));
end;

function RegistrarRelatorios(const AZip, APastaProjeto: string; ARelativos: TStrings;
  out AErro: string): boolean;
var
  vTexto, vArq, vRel: string;
  vTodas: TStringList;
  vInt: integer;
begin
  AErro := '';
  Result := FileExists(AZip);
  if not Result then
  begin
    AErro := Format(emCopiaNaoExiste, [AZip]);
    Exit;
  end;
  vTexto := '';
  vTodas := TStringList.Create;
  try
    if FileExists(CaminhoDoDepois(AZip)) then
      vTodas.LoadFromFile(CaminhoDoDepois(AZip));
    for vInt := 0 to Pred(vTodas.Count) do
      if vTodas[vInt] = SECAO_RELATORIOS then
        Break
      else
        vTexto := vTexto + vTodas[vInt] + #10;
  finally
    vTodas.Free;
  end;
  vTexto := vTexto + SECAO_RELATORIOS + #10;
  for vInt := 0 to Pred(ARelativos.Count) do
  begin
    vRel := CaminhoRelativo(ARelativos[vInt]);
    vArq := CaminhoNoDisco(APastaProjeto, vRel);
    if (vRel <> '') and FileExists(vArq) then
      vTexto := vTexto + SHA256Arquivo(vArq) + #9 + vRel + #10;
  end;
  try
    GravarTexto(CaminhoDoDepois(AZip), vTexto);
  except
    on E: Exception do
    begin
      AErro := Format(emCopiaSemRegistro, [E.Message]);
      Result := False;
    end;
  end;
end;

procedure ListarRelatoriosMantidos(const AZip, APastaProjeto: string; AMantidos: TStrings);
var
  vLinhas: TStringList;
  vInt: integer;
begin
  vLinhas := TStringList.Create;
  try
    LerRelatorios(AZip, vLinhas);
    for vInt := 0 to Pred(vLinhas.Count) do
      if not RelatorioIntacto(APastaProjeto, vLinhas[vInt]) and
         FileExists(CaminhoNoDisco(APastaProjeto, RelatorioDaLinha(vLinhas[vInt]))) then
        AMantidos.Add(RelatorioDaLinha(vLinhas[vInt]));
  finally
    vLinhas.Free;
  end;
end;

/// Deletes the reports the migration wrote and nobody touched since
procedure RemoverRelatorios(const AZip, APastaProjeto: string);
var
  vLinhas: TStringList;
  vInt: integer;
  vRel: string;
begin
  vLinhas := TStringList.Create;
  try
    LerRelatorios(AZip, vLinhas);
    for vInt := 0 to Pred(vLinhas.Count) do
      if RelatorioIntacto(APastaProjeto, vLinhas[vInt]) then
      begin
        vRel := RelatorioDaLinha(vLinhas[vInt]);
        DeleteFile(CaminhoNoDisco(APastaProjeto, vRel));
        LimparPastasVazias(APastaProjeto, vRel);
      end;
  finally
    vLinhas.Free;
  end;
end;

procedure DescartarCopia(const AZip, APastaProjeto: string);
begin
  if FileExists(CaminhoDoDepois(AZip)) then
    DeleteFile(CaminhoDoDepois(AZip));
  if FileExists(AZip) then
    DeleteFile(AZip);
  RemoveDir(PastaCopias(APastaProjeto));
end;

function Desfazer(const AZip, APastaProjeto: string; AMudados: TStrings;
  out AErro: string): boolean;
var
  vManifesto: TManifesto;
  vZip: TUnZipper;
  vRestaurador: TRestaurador;
  vInt: integer;
  vArq: string;
begin
  Result := False;
  vManifesto := TManifesto.Create;
  try
    if not LerManifesto(AZip, vManifesto, AErro) then
      Exit;
    if AMudados <> nil then
      AMudados.Clear;
    ListarMudadosDepois(AZip, APastaProjeto, AMudados);

    vZip := TUnZipper.Create;
    vRestaurador := TRestaurador.Create;
    try
      vRestaurador.Pasta := APastaProjeto;
      for vInt := 0 to Pred(vManifesto.Existentes.Count) do
        vRestaurador.Permitidos.Add(vManifesto.Existentes.Names[vInt]);
      vZip.FileName := AZip;
      vZip.OnCreateStream := @vRestaurador.CriarStream;
      vZip.OnDoneStream := @vRestaurador.FecharStream;
      try
        vZip.UnZipAllFiles;
      except
        on E: Exception do
          vRestaurador.Erro := Format(emCopiaIlegivel, [ExtractFileName(AZip), E.Message]);
      end;
      AErro := vRestaurador.Erro;
    finally
      vRestaurador.Free;
      vZip.Free;
    end;
    if AErro <> '' then
      Exit;
    if not ConferirRestaurados(APastaProjeto, vManifesto, AErro) then
      Exit;

    // o que a migracao criou nao existia antes: desfazer e apagar
    for vInt := 0 to Pred(vManifesto.Novos.Count) do
    begin
      vArq := CaminhoNoDisco(APastaProjeto, vManifesto.Novos[vInt]);
      if FileExists(vArq) and not DeleteFile(vArq) then
      begin
        AErro := Format(emCopiaNaoRestaurada, [vManifesto.Novos[vInt], SysErrorMessage(GetLastOSError)]);
        Exit;
      end;
      LimparPastasVazias(APastaProjeto, vManifesto.Novos[vInt]);
    end;
    RemoverRelatorios(AZip, APastaProjeto);
    Result := True;
  finally
    vManifesto.Free;
  end;
end;

{ Git }

function AcharGit: string;
begin
  {$IFDEF MSWINDOWS}
  Result := ExeSearch('git.exe', GetEnvironmentVariable('PATH'));
  {$ELSE}
  Result := ExeSearch('git', GetEnvironmentVariable('PATH'));
  {$ENDIF}
end;

function GitComMudancas(const APasta: string; out ADetalhe: string): boolean;
var
  vExec: TExecucao;
  vGit: string;
  vInt, vTotal: integer;
begin
  Result := False;
  ADetalhe := '';
  vGit := AcharGit;
  if (vGit = '') or not DirectoryExists(APasta) then
    Exit;
  vExec := TExecucao.Create;
  try
    vExec.Executavel := vGit;
    vExec.PastaTrabalho := APasta;
    vExec.Parametros.Add('rev-parse');
    vExec.Parametros.Add('--is-inside-work-tree');
    if not vExec.Executar then
      Exit;
    vExec.Parametros.Clear;
    vExec.Parametros.Add('status');
    vExec.Parametros.Add('--porcelain');
    if not vExec.Executar then
      Exit;
    vTotal := 0;
    for vInt := 0 to Pred(vExec.Saida.Count) do
      if (Trim(vExec.Saida[vInt]) <> '') and (Pos(PASTA_COPIAS + '/', vExec.Saida[vInt]) = 0) then
      begin
        Inc(vTotal);
        if vTotal <= 5 then
        begin
          if ADetalhe <> '' then
            ADetalhe := ADetalhe + '; ';
          ADetalhe := ADetalhe + Trim(vExec.Saida[vInt]);
        end;
      end;
    if vTotal > 5 then
      ADetalhe := ADetalhe + '; ... (' + IntToStr(vTotal) + ')';
    Result := vTotal > 0;
  finally
    vExec.Free;
  end;
end;

end.
