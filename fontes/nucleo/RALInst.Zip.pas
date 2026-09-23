unit RALInst.Zip;

{$mode ObjFPC}{$H+}

// O zip de uma versao do GitHub em dois papeis: origem do catalogo (F2) sem
// extrair nada, para as telas de escolha, e fonte da extracao para a pasta
// escolhida, na execucao. Todo zip do GitHub tem uma pasta de topo
// ('PascalRAL-1.1/'), que sai nos dois casos.

interface

uses
  Classes, SysUtils, Zipper, RALInst.Catalogo, RALInst.Processo;

type

  { TOrigemZip }

  TOrigemZip = class(TOrigemArquivos)
  private
    FArquivo: string;
    FDescricao: string;
    FCommit: string;
    // relativo (sem a pasta de topo, com '/') = nome dentro do zip
    FEntradas: TStringList;
    FLido: TMemoryStream;
    procedure CriarStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure FecharStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
  public
    // excecao se o arquivo nao e um zip legivel
    constructor Create(const AArquivo, ADescricao: string);
    destructor Destroy; override;

    function Descricao: string; override;
    procedure Listar(const APasta: string; ALista: TStrings); override;
    function Existe(const ACaminho: string): boolean; override;
    function Ler(const ACaminho: string): string; override;
    function Localizar(const ACaminho: string): string; override;
    function BaixaSubmodulos: boolean; override;

    property Arquivo: string read FArquivo;
    // commit do zipball (o GitHub grava no comentario do zip); '' se nao ha
    property Commit: string read FCommit;
  end;

// comentario do arquivo zip ('' se nao ha ou se nao e zip)
function ComentarioZip(const AArquivo: string): string;

// extrai tirando as primeiras ARetirarNiveis pastas de cada entrada (1 = a
// pasta de topo do zip do GitHub); recusa entrada que sai da pasta de destino
// ('..', caminho absoluto)
function ExtrairZip(const AArquivo, ADestino: string; out AErro: string;
  ARetirarNiveis: integer = 1): boolean;

implementation

function SemPastaDeTopo(const ANome: string): string;
var
  vPos: integer;
begin
  Result := StringReplace(ANome, '\', '/', [rfReplaceAll]);
  vPos := Pos('/', Result);
  if vPos > 0 then
    Result := Copy(Result, vPos + 1, MaxInt)
  else
    Result := '';
end;

function CaminhoSeguro(const ARelativo: string): boolean;
begin
  Result := (ARelativo <> '') and (Pos('..', ARelativo) = 0) and
            (ARelativo[1] <> '/') and (Pos(':', ARelativo) = 0);
end;

function ComentarioZip(const AArquivo: string): string;
var
  vArq: TFileStream;
  vBuf: array of byte;
  vTam, vInt, vLen: integer;
begin
  Result := '';
  if not FileExists(AArquivo) then
    Exit;
  vArq := TFileStream.Create(AArquivo, fmOpenRead or fmShareDenyWrite);
  try
    // o registro final do zip (assinatura PK 05 06) fica nos ultimos 22
    // bytes mais o comentario, que tem no maximo 64 KB
    vTam := vArq.Size;
    if vTam > 65535 + 22 then
      vTam := 65535 + 22;
    SetLength(vBuf, vTam);
    vArq.Position := vArq.Size - vTam;
    vArq.ReadBuffer(vBuf[0], vTam);
    for vInt := vTam - 22 downto 0 do
      if (vBuf[vInt] = $50) and (vBuf[vInt + 1] = $4B) and
         (vBuf[vInt + 2] = $05) and (vBuf[vInt + 3] = $06) then
      begin
        vLen := vBuf[vInt + 20] or (vBuf[vInt + 21] shl 8);
        if vInt + 22 + vLen <= vTam then
          SetString(Result, PChar(@vBuf[vInt + 22]), vLen);
        Break;
      end;
  finally
    vArq.Free;
  end;
  Result := Trim(Result);
end;

type

  { TExtrator }

  TExtrator = class
  public
    Destino: string;
    Erro: string;
    Niveis: integer;
    procedure CriarStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure FecharStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
  end;

procedure TExtrator.CriarStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var
  vRel, vArquivo: string;
  vNivel: integer;
begin
  AStream := nil;
  vRel := StringReplace(AItem.ArchiveFileName, '\', '/', [rfReplaceAll]);
  for vNivel := 1 to Niveis do
    vRel := SemPastaDeTopo(vRel);
  if vRel = '' then
    Exit;
  if not CaminhoSeguro(vRel) then
  begin
    Erro := 'entrada suspeita no zip, ignorada: ' + AItem.ArchiveFileName;
    Exit;
  end;
  vArquivo := Destino + StringReplace(vRel, '/', PathDelim, [rfReplaceAll]);
  if AItem.IsDirectory then
    CriarPastas(vArquivo)
  else
  begin
    CriarPastas(ExtractFilePath(vArquivo));
    AStream := TFileStream.Create(CaminhoLongo(vArquivo), fmCreate);
  end;
end;

procedure TExtrator.FecharStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  FreeAndNil(AStream);
end;

function ExtrairZip(const AArquivo, ADestino: string; out AErro: string;
  ARetirarNiveis: integer): boolean;
var
  vZip: TUnZipper;
  vExt: TExtrator;
begin
  Result := False;
  AErro := '';
  vZip := TUnZipper.Create;
  vExt := TExtrator.Create;
  try
    vExt.Niveis := ARetirarNiveis;
    vExt.Destino := IncludeTrailingPathDelimiter(ADestino);
    ForceDirectories(vExt.Destino);
    vZip.FileName := AArquivo;
    vZip.OnCreateStream := @vExt.CriarStream;
    vZip.OnDoneStream := @vExt.FecharStream;
    try
      vZip.UnZipAllFiles;
      Result := True;
      AErro := vExt.Erro;
    except
      on E: Exception do
        AErro := 'não foi possível extrair ' + ExtractFileName(AArquivo) + ': ' + E.Message;
    end;
  finally
    vExt.Free;
    vZip.Free;
  end;
end;

{ TOrigemZip }

constructor TOrigemZip.Create(const AArquivo, ADescricao: string);
var
  vZip: TUnZipper;
  vInt: integer;
  vRel: string;
begin
  inherited Create;
  FArquivo := AArquivo;
  FDescricao := ADescricao;
  FEntradas := TStringList.Create;
  FEntradas.CaseSensitive := False;
  FEntradas.Sorted := True;
  FEntradas.Duplicates := dupIgnore;
  FEntradas.NameValueSeparator := #1;

  vZip := TUnZipper.Create;
  try
    vZip.FileName := AArquivo;
    vZip.Examine;
    for vInt := 0 to Pred(vZip.Entries.Count) do
      if not vZip.Entries[vInt].IsDirectory then
      begin
        vRel := SemPastaDeTopo(vZip.Entries[vInt].ArchiveFileName);
        if vRel <> '' then
          FEntradas.Add(vRel + #1 + vZip.Entries[vInt].ArchiveFileName);
      end;
  finally
    vZip.Free;
  end;
  FCommit := ComentarioZip(AArquivo);
end;

destructor TOrigemZip.Destroy;
begin
  FLido.Free;
  FEntradas.Free;
  inherited Destroy;
end;

function TOrigemZip.Descricao: string;
begin
  Result := FDescricao;
end;

procedure TOrigemZip.Listar(const APasta: string; ALista: TStrings);
var
  vPasta: string;
  vInt: integer;
begin
  vPasta := StringReplace(APasta, '\', '/', [rfReplaceAll]);
  if (vPasta <> '') and (vPasta[Length(vPasta)] <> '/') then
    vPasta := vPasta + '/';
  for vInt := 0 to Pred(FEntradas.Count) do
    if (vPasta = '') or SameText(Copy(FEntradas.Names[vInt], 1, Length(vPasta)), vPasta) then
      ALista.Add(FEntradas.Names[vInt]);
end;

function TOrigemZip.Existe(const ACaminho: string): boolean;
begin
  Result := FEntradas.IndexOfName(StringReplace(ACaminho, '\', '/', [rfReplaceAll])) >= 0;
end;

procedure TOrigemZip.CriarStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  FreeAndNil(FLido);
  FLido := TMemoryStream.Create;
  AStream := FLido;
end;

procedure TOrigemZip.FecharStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  // o stream continua em FLido; o unzipper nao o libera
  AStream := nil;
end;

function TOrigemZip.Ler(const ACaminho: string): string;
var
  vIdx: integer;
  vZip: TUnZipper;
  vLista, vTexto: TStringList;
begin
  vIdx := FEntradas.IndexOfName(StringReplace(ACaminho, '\', '/', [rfReplaceAll]));
  if vIdx < 0 then
    raise EFileNotFoundException.Create(ACaminho + ' não existe em ' + FDescricao);

  vZip := TUnZipper.Create;
  vLista := TStringList.Create;
  vTexto := TStringList.Create;
  try
    FreeAndNil(FLido);
    vLista.Add(FEntradas.ValueFromIndex[vIdx]);
    vZip.FileName := FArquivo;
    vZip.OnCreateStream := @CriarStream;
    vZip.OnDoneStream := @FecharStream;
    vZip.UnZipFiles(vLista);
    if FLido = nil then
      raise EFileNotFoundException.Create(ACaminho + ' não pôde ser lido de ' + FDescricao);
    FLido.Position := 0;
    vTexto.LoadFromStream(FLido);
    Result := vTexto.Text;
    if Copy(Result, 1, 3) = #$EF#$BB#$BF then
      Delete(Result, 1, 3);
  finally
    FreeAndNil(FLido);
    vTexto.Free;
    vLista.Free;
    vZip.Free;
  end;
end;

function TOrigemZip.Localizar(const ACaminho: string): string;
begin
  Result := FDescricao + ':' + ACaminho;
end;

function TOrigemZip.BaixaSubmodulos: boolean;
begin
  Result := True;
end;

end.
