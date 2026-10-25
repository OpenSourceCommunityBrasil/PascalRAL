/// The text of a file as the migrator edits it: the original bytes (never
/// converted), the encoding (UTF-8 with BOM, UTF-8, ANSI), the dominant line
/// break, the lines with their offsets and a list of non-overlapping edits that
/// is applied in one go. What no edit touches leaves identical, byte by byte.
///
/// Conventions: an *offset* is a 1-based index into Bytes (like Copy and Pos);
/// a *line* number is 1-based too. The BOM, when there is one, is not part of
/// line 1 (see InicioConteudo). New text handed to the edits is UTF-8 and is
/// converted to the encoding of the file when the result is built.
unit RALMig.Texto;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  /// How the file is encoded.
  TCodificacao = (cdUtf8Bom, cdUtf8, cdAnsi);

  /// Raised when an edit is invalid or overlaps another one.
  ETextoEdicao = class(Exception);

  /// One edit: replaces Tamanho bytes at Offset by Novo (Tamanho = 0 inserts).
  TEdicao = class
  public
    Offset: integer;
    Tamanho: integer;
    /// Already in the encoding of the file
    Novo: AnsiString;
    Ordem: integer;
  end;

  /// The original text of a file plus the edits waiting to be applied.
  TTextoOriginal = class
  private
    FBytes: AnsiString;
    FBytesDisco: AnsiString;
    FCaminho: string;
    FCodificacao: TCodificacao;
    FEdicoes: TObjectList;
    FFimDeLinha: string;
    FInicio: integer;
    FLinhaCount: integer;
    FLinhaFim: array of integer;
    FLinhaIni: array of integer;
    FLinhaProx: array of integer;
    FProximaOrdem: integer;
    FResultado: AnsiString;
    FResultadoValido: boolean;
    FId: QWord;
    FGeracao: integer;
    function GetEdicaoCount: integer;
    function GetResultado: RawByteString;
    procedure Analisar;
    procedure AdicionarLinha(AIni, AFim, AProx: integer);
    procedure DetectarFimDeLinha;
    procedure ConferirEdicao(AOffset, ATamanho: integer);
    procedure ConferirSobreposicao(AOffset, ATamanho: integer);
    procedure ConferirLinha(ANumero: integer);
    function Montar: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    /// Reads the file as bytes. Raises when it cannot be read.
    procedure CarregarArquivo(const ACaminho: string);
    /// Takes the bytes as they are (ACaminho only names the file)
    procedure CarregarBytes(const ABytes: RawByteString; const ACaminho: string = '');
    /// Replaces the original (edits are discarded, the bytes on disk stay the
    /// reference of Alterado). Used when a binary form becomes text.
    procedure RedefinirOriginal(const ABytes: RawByteString);
    /// Makes the result the new original and clears the edits, so that the
    /// next analysis sees the edited text. Alterado keeps comparing with disk.
    procedure Consolidar;

    /// Replaces Tamanho bytes at Offset by ANovo (UTF-8; Tamanho = 0 inserts).
    /// Two edits may not overlap; inserts at the same offset keep their order.
    procedure AdicionarEdicao(AOffset, ATamanho: integer; const ANovo: RawByteString);
    /// Inserts ANovo before the byte at AOffset
    procedure Inserir(AOffset: integer; const ANovo: RawByteString);
    /// Removes the lines AIni..AFim with their line breaks
    procedure RemoverLinhas(AIni, AFim: integer);
    /// Inserts text (any line break, it gets the one of the file) as whole
    /// lines before the line ALinha (LinhaCount + 1 appends at the end)
    procedure InserirAntesDaLinha(ALinha: integer; const ATexto: RawByteString);

    /// Converts the line breaks of ATexto to the one of the file
    function NormalizarFimDeLinha(const ATexto: RawByteString): RawByteString;
    /// Converts UTF-8 text to the encoding of the file
    function ParaCodificacao(const AUtf8: RawByteString): RawByteString;
    /// Number of the line that holds the offset (the last one when beyond)
    function LinhaDoOffset(AOffset: integer): integer;
    /// 1-based column of the offset in its line
    function ColunaDoOffset(AOffset: integer): integer;
    /// Offset of the first byte of the line
    function LinhaInicio(ANumero: integer): integer;
    /// Offset just after the last byte of the line, before its line break
    function LinhaFim(ANumero: integer): integer;
    /// Offset of the next line (just after the line break of this one)
    function LinhaProxima(ANumero: integer): integer;
    /// The line without its line break
    function TextoDaLinha(ANumero: integer): string;
    /// The spaces and tabs at the start of the line
    function Indentacao(ANumero: integer): string;
    /// Writes the result when changed (or when AForcar) and tells if it wrote
    function Gravar(const ACaminho: string; AForcar: boolean = False): boolean;

    /// The original bytes (after RedefinirOriginal/Consolidar: the current ones)
    property Bytes: RawByteString read FBytes;
    property Caminho: string read FCaminho write FCaminho;
    /// Unique per instance, never reused - unlike the address of the object,
    /// which a later instance may get once this one is freed
    property Id: QWord read FId;
    /// Changes every time the original is (re)loaded or consolidated: what was
    /// asked of an older original is not pending any more
    property Geracao: integer read FGeracao;
    /// Settable, for text of a new file that has no clue
    property Codificacao: TCodificacao read FCodificacao write FCodificacao;
    property EdicaoCount: integer read GetEdicaoCount;
    /// Dominant line break of the file: #13#10, #10 or #13
    property FimDeLinha: string read FFimDeLinha;
    /// Offset of the first byte after the BOM (1 without BOM)
    property InicioConteudo: integer read FInicio;
    property LinhaCount: integer read FLinhaCount;
    /// The bytes with the edits applied
    property Resultado: RawByteString read GetResultado;
    /// True when the result differs from what was on disk
    function Alterado: boolean;
  end;

/// Guesses the encoding of the bytes; a file of plain ASCII is ANSI for
/// .pas/.dpr/.dpk/.inc/.dfm and UTF-8 for the others.
function DetectarCodificacao(const ABytes: RawByteString;
  const ACaminho: string): TCodificacao;

/// Reads a whole file as raw bytes
function LerBytes(const ACaminho: string): AnsiString;

implementation

resourcestring
  SEdicaoForaDoTexto = 'Edit outside of the text (offset %d, size %d, text of %d bytes)';
  SEdicaoSobreposta = 'Overlapping edits: offset %d (line %d) with size %d overlaps an edit already made';
  SLinhaInvalida = 'Line %d does not exist (the text has %d lines)';

function LerBytes(const ACaminho: string): AnsiString;
var
  vArq: TFileStream;
begin
  Result := '';
  vArq := TFileStream.Create(ACaminho, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, vArq.Size);
    if Length(Result) > 0 then
      vArq.ReadBuffer(Result[1], Length(Result));
  finally
    vArq.Free;
  end;
end;

// raw copy of the bytes, whatever code page the caller's string has
procedure CopiarBytes(const AOrigem: RawByteString; out ADestino: AnsiString);
begin
  SetString(ADestino, PAnsiChar(Pointer(AOrigem)), Length(AOrigem));
end;

function TemBom(const ABytes: RawByteString): boolean;
begin
  Result := (Length(ABytes) >= 3) and (ABytes[1] = #$EF) and
    (ABytes[2] = #$BB) and (ABytes[3] = #$BF);
end;

// 0 = not UTF-8, 1 = only ASCII, 2 = valid UTF-8 with multi-byte sequences
function ClasseUtf8(const ABytes: RawByteString; AInicio: integer): integer;
var
  i, vResto, vLen: integer;
  vB: byte;
begin
  Result := 1;
  i := AInicio;
  vLen := Length(ABytes);
  while i <= vLen do
  begin
    vB := Ord(ABytes[i]);
    if vB < $80 then
      vResto := 0
    else if (vB >= $C2) and (vB <= $DF) then
      vResto := 1
    else if (vB >= $E0) and (vB <= $EF) then
      vResto := 2
    else if (vB >= $F0) and (vB <= $F4) then
      vResto := 3
    else
      Exit(0);
    if vResto > 0 then
      Result := 2;
    Inc(i);
    while vResto > 0 do
    begin
      if (i > vLen) or ((Ord(ABytes[i]) and $C0) <> $80) then
        Exit(0);
      Inc(i);
      Dec(vResto);
    end;
  end;
end;

function DetectarCodificacao(const ABytes: RawByteString;
  const ACaminho: string): TCodificacao;
var
  vExt: string;
begin
  if TemBom(ABytes) then
    Exit(cdUtf8Bom);
  case ClasseUtf8(ABytes, 1) of
    0: Result := cdAnsi;
    2: Result := cdUtf8;
  else
    begin
      vExt := LowerCase(ExtractFileExt(ACaminho));
      if (vExt = '.pas') or (vExt = '.dpr') or (vExt = '.dpk') or
        (vExt = '.inc') or (vExt = '.dfm') then
        Result := cdAnsi
      else
        Result := cdUtf8;
    end;
  end;
end;

function SoAscii(const AUtf8: RawByteString): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 1 to Length(AUtf8) do
    if Ord(AUtf8[i]) >= $80 then
      Exit(False);
end;

{ TTextoOriginal }

var
  GProximoId: QWord = 0;

constructor TTextoOriginal.Create;
begin
  inherited Create;
  Inc(GProximoId);
  FId := GProximoId;
  FEdicoes := TObjectList.Create(True);
  FFimDeLinha := sLineBreak;
  FInicio := 1;
end;

destructor TTextoOriginal.Destroy;
begin
  FEdicoes.Free;
  inherited Destroy;
end;

procedure TTextoOriginal.AdicionarLinha(AIni, AFim, AProx: integer);
begin
  Inc(FLinhaCount);
  if FLinhaCount > Length(FLinhaIni) then
  begin
    SetLength(FLinhaIni, FLinhaCount + 256);
    SetLength(FLinhaFim, FLinhaCount + 256);
    SetLength(FLinhaProx, FLinhaCount + 256);
  end;
  FLinhaIni[FLinhaCount - 1] := AIni;
  FLinhaFim[FLinhaCount - 1] := AFim;
  FLinhaProx[FLinhaCount - 1] := AProx;
end;

procedure TTextoOriginal.Analisar;
var
  i, vLen, vIni: integer;
begin
  Inc(FGeracao);
  FLinhaCount := 0;
  FInicio := 1;
  if TemBom(FBytes) then
    FInicio := 4;
  vLen := Length(FBytes);
  vIni := FInicio;
  i := FInicio;
  while i <= vLen do
  begin
    if FBytes[i] = #10 then
    begin
      AdicionarLinha(vIni, i, i + 1);
      vIni := i + 1;
    end
    else if FBytes[i] = #13 then
    begin
      if (i < vLen) and (FBytes[i + 1] = #10) then
      begin
        AdicionarLinha(vIni, i, i + 2);
        Inc(i);
      end
      else
        AdicionarLinha(vIni, i, i + 1);
      vIni := i + 1;
    end;
    Inc(i);
  end;
  if vIni <= vLen then
    AdicionarLinha(vIni, vLen + 1, vLen + 1);
  DetectarFimDeLinha;
  FResultadoValido := False;
end;

procedure TTextoOriginal.DetectarFimDeLinha;
var
  i, vCrLf, vLf, vCr: integer;
begin
  vCrLf := 0;
  vLf := 0;
  vCr := 0;
  for i := 0 to FLinhaCount - 1 do
    case FLinhaProx[i] - FLinhaFim[i] of
      2: Inc(vCrLf);
      1: if FBytes[FLinhaFim[i]] = #10 then Inc(vLf) else Inc(vCr);
    end;
  if (vCrLf = 0) and (vLf = 0) and (vCr = 0) then
    Exit;
  if (vCrLf >= vLf) and (vCrLf >= vCr) then
    FFimDeLinha := #13#10
  else if vLf >= vCr then
    FFimDeLinha := #10
  else
    FFimDeLinha := #13;
end;

procedure TTextoOriginal.CarregarBytes(const ABytes: RawByteString;
  const ACaminho: string);
begin
  CopiarBytes(ABytes, FBytes);
  FBytesDisco := FBytes;
  FCaminho := ACaminho;
  FEdicoes.Clear;
  FProximaOrdem := 0;
  FCodificacao := DetectarCodificacao(FBytes, ACaminho);
  FFimDeLinha := sLineBreak;
  Analisar;
end;

procedure TTextoOriginal.CarregarArquivo(const ACaminho: string);
begin
  CarregarBytes(LerBytes(ACaminho), ACaminho);
end;

procedure TTextoOriginal.RedefinirOriginal(const ABytes: RawByteString);
begin
  CopiarBytes(ABytes, FBytes);
  FEdicoes.Clear;
  FProximaOrdem := 0;
  FCodificacao := DetectarCodificacao(FBytes, FCaminho);
  FFimDeLinha := sLineBreak;
  Analisar;
end;

procedure TTextoOriginal.Consolidar;
var
  vCodif: TCodificacao;
  vFim: string;
begin
  vCodif := FCodificacao;
  vFim := FFimDeLinha;
  FBytes := Resultado;
  FEdicoes.Clear;
  FProximaOrdem := 0;
  Analisar;
  FCodificacao := vCodif;
  FFimDeLinha := vFim;
end;

function TTextoOriginal.GetEdicaoCount: integer;
begin
  Result := FEdicoes.Count;
end;

procedure TTextoOriginal.ConferirLinha(ANumero: integer);
begin
  if (ANumero < 1) or (ANumero > FLinhaCount) then
    raise ETextoEdicao.CreateFmt(SLinhaInvalida, [ANumero, FLinhaCount]);
end;

procedure TTextoOriginal.ConferirEdicao(AOffset, ATamanho: integer);
begin
  if (AOffset < FInicio) or (ATamanho < 0) or
    (AOffset + ATamanho > Length(FBytes) + 1) then
    raise ETextoEdicao.CreateFmt(SEdicaoForaDoTexto,
      [AOffset, ATamanho, Length(FBytes)]);
end;

procedure TTextoOriginal.ConferirSobreposicao(AOffset, ATamanho: integer);
var
  i: integer;
  vEd: TEdicao;
  vConflito: boolean;
begin
  for i := 0 to FEdicoes.Count - 1 do
  begin
    vEd := TEdicao(FEdicoes[i]);
    if (ATamanho = 0) and (vEd.Tamanho = 0) then
      vConflito := False
    else if ATamanho = 0 then
      vConflito := (vEd.Offset < AOffset) and (AOffset < vEd.Offset + vEd.Tamanho)
    else if vEd.Tamanho = 0 then
      vConflito := (AOffset < vEd.Offset) and (vEd.Offset < AOffset + ATamanho)
    else
      vConflito := (AOffset < vEd.Offset + vEd.Tamanho) and
        (vEd.Offset < AOffset + ATamanho);
    if vConflito then
      raise ETextoEdicao.CreateFmt(SEdicaoSobreposta,
        [AOffset, LinhaDoOffset(AOffset), ATamanho]);
  end;
end;

procedure TTextoOriginal.AdicionarEdicao(AOffset, ATamanho: integer;
  const ANovo: RawByteString);
var
  vEd: TEdicao;
begin
  ConferirEdicao(AOffset, ATamanho);
  ConferirSobreposicao(AOffset, ATamanho);
  vEd := TEdicao.Create;
  vEd.Offset := AOffset;
  vEd.Tamanho := ATamanho;
  vEd.Novo := ParaCodificacao(ANovo);
  vEd.Ordem := FProximaOrdem;
  Inc(FProximaOrdem);
  FEdicoes.Add(vEd);
  FResultadoValido := False;
end;

procedure TTextoOriginal.Inserir(AOffset: integer; const ANovo: RawByteString);
begin
  AdicionarEdicao(AOffset, 0, ANovo);
end;

procedure TTextoOriginal.RemoverLinhas(AIni, AFim: integer);
begin
  ConferirLinha(AIni);
  ConferirLinha(AFim);
  AdicionarEdicao(LinhaInicio(AIni), LinhaProxima(AFim) - LinhaInicio(AIni), '');
end;

procedure TTextoOriginal.InserirAntesDaLinha(ALinha: integer;
  const ATexto: RawByteString);
var
  vTexto: RawByteString;
begin
  vTexto := NormalizarFimDeLinha(ATexto);
  if (vTexto = '') or (Copy(vTexto, Length(vTexto) - Length(FFimDeLinha) + 1,
    Length(FFimDeLinha)) <> FFimDeLinha) then
    vTexto := vTexto + FFimDeLinha;
  if ALinha = FLinhaCount + 1 then
  begin
    // appending: the last line may lack its line break
    if (FLinhaCount > 0) and (FLinhaProx[FLinhaCount - 1] = FLinhaFim[FLinhaCount - 1]) then
      vTexto := FFimDeLinha + Copy(vTexto, 1, Length(vTexto) - Length(FFimDeLinha));
    AdicionarEdicao(Length(FBytes) + 1, 0, vTexto);
  end
  else
  begin
    ConferirLinha(ALinha);
    AdicionarEdicao(FLinhaIni[ALinha - 1], 0, vTexto);
  end;
end;

function TTextoOriginal.NormalizarFimDeLinha(const ATexto: RawByteString): RawByteString;
var
  i, vLen: integer;
  vSaida: TMemoryStream;
  vTmp: AnsiString;
begin
  vTmp := '';
  vLen := Length(ATexto);
  vSaida := TMemoryStream.Create;
  try
    i := 1;
    while i <= vLen do
    begin
      if (ATexto[i] = #13) or (ATexto[i] = #10) then
      begin
        if (ATexto[i] = #13) and (i < vLen) and (ATexto[i + 1] = #10) then
          Inc(i);
        vSaida.WriteBuffer(FFimDeLinha[1], Length(FFimDeLinha));
      end
      else
        vSaida.WriteBuffer(ATexto[i], 1);
      Inc(i);
    end;
    SetLength(vTmp, vSaida.Size);
    if vSaida.Size > 0 then
      Move(vSaida.Memory^, vTmp[1], vSaida.Size);
    Result := vTmp;
  finally
    vSaida.Free;
  end;
end;

function TTextoOriginal.ParaCodificacao(const AUtf8: RawByteString): RawByteString;
var
  vUtf8: UTF8String;
  vTmp: AnsiString;
begin
  CopiarBytes(AUtf8, vTmp);
  if (FCodificacao = cdAnsi) and not SoAscii(AUtf8) then
  begin
    SetString(vUtf8, PAnsiChar(Pointer(AUtf8)), Length(AUtf8));
    vTmp := UTF8ToAnsi(vUtf8);
  end;
  Result := vTmp;
end;

function TTextoOriginal.LinhaDoOffset(AOffset: integer): integer;
var
  vIni, vFim, vMeio: integer;
begin
  if FLinhaCount = 0 then
    Exit(1);
  vIni := 0;
  vFim := FLinhaCount - 1;
  while vIni < vFim do
  begin
    vMeio := (vIni + vFim + 1) div 2;
    if FLinhaIni[vMeio] <= AOffset then
      vIni := vMeio
    else
      vFim := vMeio - 1;
  end;
  Result := vIni + 1;
end;

function TTextoOriginal.ColunaDoOffset(AOffset: integer): integer;
var
  vLinha: integer;
begin
  vLinha := LinhaDoOffset(AOffset);
  if FLinhaCount = 0 then
    Exit(1);
  Result := AOffset - FLinhaIni[vLinha - 1] + 1;
end;

function TTextoOriginal.LinhaInicio(ANumero: integer): integer;
begin
  ConferirLinha(ANumero);
  Result := FLinhaIni[ANumero - 1];
end;

function TTextoOriginal.LinhaFim(ANumero: integer): integer;
begin
  ConferirLinha(ANumero);
  Result := FLinhaFim[ANumero - 1];
end;

function TTextoOriginal.LinhaProxima(ANumero: integer): integer;
begin
  ConferirLinha(ANumero);
  Result := FLinhaProx[ANumero - 1];
end;

function TTextoOriginal.TextoDaLinha(ANumero: integer): string;
begin
  ConferirLinha(ANumero);
  Result := Copy(FBytes, FLinhaIni[ANumero - 1],
    FLinhaFim[ANumero - 1] - FLinhaIni[ANumero - 1]);
end;

function TTextoOriginal.Indentacao(ANumero: integer): string;
var
  i, vFim: integer;
begin
  ConferirLinha(ANumero);
  i := FLinhaIni[ANumero - 1];
  vFim := FLinhaFim[ANumero - 1];
  while (i < vFim) and (FBytes[i] in [' ', #9]) do
    Inc(i);
  Result := Copy(FBytes, FLinhaIni[ANumero - 1], i - FLinhaIni[ANumero - 1]);
end;

function EdicaoMenor(AA, AB: Pointer): integer;
var
  vA, vB: TEdicao;
begin
  vA := TEdicao(AA);
  vB := TEdicao(AB);
  Result := vA.Offset - vB.Offset;
  if Result = 0 then
    Result := Ord(vA.Tamanho > 0) - Ord(vB.Tamanho > 0);
  if Result = 0 then
    Result := vA.Ordem - vB.Ordem;
end;

function TTextoOriginal.Montar: AnsiString;
var
  vLista: TList;
  vSaida: TMemoryStream;
  i, vPos: integer;
  vEd: TEdicao;
begin
  Result := '';
  vLista := TList.Create;
  vSaida := TMemoryStream.Create;
  try
    for i := 0 to FEdicoes.Count - 1 do
      vLista.Add(FEdicoes[i]);
    vLista.Sort(@EdicaoMenor);
    vPos := 1;
    for i := 0 to vLista.Count - 1 do
    begin
      vEd := TEdicao(vLista[i]);
      if vEd.Offset > vPos then
        vSaida.WriteBuffer(FBytes[vPos], vEd.Offset - vPos);
      if Length(vEd.Novo) > 0 then
        vSaida.WriteBuffer(vEd.Novo[1], Length(vEd.Novo));
      vPos := vEd.Offset + vEd.Tamanho;
    end;
    if vPos <= Length(FBytes) then
      vSaida.WriteBuffer(FBytes[vPos], Length(FBytes) - vPos + 1);
    SetLength(Result, vSaida.Size);
    if vSaida.Size > 0 then
      Move(vSaida.Memory^, Result[1], vSaida.Size);
  finally
    vSaida.Free;
    vLista.Free;
  end;
end;

function TTextoOriginal.GetResultado: RawByteString;
begin
  if not FResultadoValido then
  begin
    if FEdicoes.Count = 0 then
      FResultado := FBytes
    else
      FResultado := Montar;
    FResultadoValido := True;
  end;
  Result := FResultado;
end;

function TTextoOriginal.Alterado: boolean;
begin
  Result := Resultado <> FBytesDisco;
end;

function TTextoOriginal.Gravar(const ACaminho: string; AForcar: boolean): boolean;
var
  vArq: TFileStream;
  vDados: RawByteString;
begin
  Result := AForcar or Alterado;
  if not Result then
    Exit;
  vDados := Resultado;
  vArq := TFileStream.Create(ACaminho, fmCreate);
  try
    if Length(vDados) > 0 then
      vArq.WriteBuffer(vDados[1], Length(vDados));
  finally
    vArq.Free;
  end;
end;

end.
