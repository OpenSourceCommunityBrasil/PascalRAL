/// Preview of a migration: the unified diff of one file (original x changed),
/// in text, before a single byte is written. Lines are compared without their
/// terminator (CRLF, LF and CR are the same line), so a file whose only change
/// is the line ending shows no diff. The output is UTF-8: a file that is not
/// valid UTF-8 is read as ANSI (system code page), only to be shown.
unit RALMig.Previa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

/// Unified diff ('--- a/<name>', '+++ b/<name>', '@@ -l,n +l,n @@'); '' when
/// the two texts have the same lines. AContexto = unchanged lines around each
/// change. Lines end with #10 whatever the file uses.
function DiffUnificado(const ANome: string; const AAntes, ADepois: RawByteString;
  AContexto: integer = 3): string;
/// Counts the added and removed lines of a diff made by DiffUnificado
procedure ResumoDiff(const ADiff: string; out AAcrescentadas, ATiradas: integer);
/// The same count as a sentence for the screen and the report
function TextoResumoDiff(const ADiff: string): string;

implementation

uses
  RALMig.Mensagens;

const
  /// Marks the last line of a text that has no terminator
  SEM_FIM = #0;
  /// Beyond this many edits the diff is shown as "all out, all in"
  LIMITE_EDICOES = 2500;
  SEM_FIM_TEXTO = '\ No newline at end of file';

type
  TLinhas = array of string;
  TOps = array of char;
  TTrilha = array of array of integer;

{ Text }

/// A copy of the bytes as a string, with no code page conversion
function BytesParaString(const ABytes: RawByteString): string;
begin
  Result := '';
  if Length(ABytes) > 0 then
    SetString(Result, PAnsiChar(Pointer(ABytes)), Length(ABytes));
end;

function Utf8Valido(const ATexto: string): boolean;
var
  vInt, vResto: integer;
  vByte: byte;
begin
  Result := False;
  vInt := 1;
  while vInt <= Length(ATexto) do
  begin
    vByte := Ord(ATexto[vInt]);
    if vByte < $80 then
      vResto := 0
    else if (vByte >= $C2) and (vByte <= $DF) then
      vResto := 1
    else if (vByte >= $E0) and (vByte <= $EF) then
      vResto := 2
    else if (vByte >= $F0) and (vByte <= $F4) then
      vResto := 3
    else
      Exit;
    Inc(vInt);
    while vResto > 0 do
    begin
      if (vInt > Length(ATexto)) or ((Ord(ATexto[vInt]) and $C0) <> $80) then
        Exit;
      Inc(vInt);
      Dec(vResto);
    end;
  end;
  Result := True;
end;

/// The text as UTF-8 bytes: unchanged when valid, converted from ANSI otherwise
function ParaUtf8(const ABytes: RawByteString): string;
var
  vTexto: string;
  vWide: UnicodeString;
  vUtf8: RawByteString;
begin
  vTexto := BytesParaString(ABytes);
  if (vTexto = '') or Utf8Valido(vTexto) then
    Exit(vTexto);
  vWide := UnicodeString(vTexto);
  vUtf8 := UTF8Encode(vWide);
  Result := BytesParaString(vUtf8);
end;

/// Splits at CRLF, LF or CR; the last line without terminator gets SEM_FIM
procedure Dividir(const ATexto: string; out ALinhas: TLinhas);
var
  vInt, vIni, vTotal: integer;
begin
  SetLength(ALinhas, 0);
  vTotal := 0;
  vIni := 1;
  vInt := 1;
  while vInt <= Length(ATexto) do
  begin
    if ATexto[vInt] in [#10, #13] then
    begin
      if vTotal >= Length(ALinhas) then
        SetLength(ALinhas, vTotal * 2 + 64);
      ALinhas[vTotal] := Copy(ATexto, vIni, vInt - vIni);
      Inc(vTotal);
      if (ATexto[vInt] = #13) and (vInt < Length(ATexto)) and (ATexto[vInt + 1] = #10) then
        Inc(vInt);
      vIni := vInt + 1;
    end;
    Inc(vInt);
  end;
  if vIni <= Length(ATexto) then
  begin
    if vTotal >= Length(ALinhas) then
      SetLength(ALinhas, vTotal + 1);
    ALinhas[vTotal] := Copy(ATexto, vIni, MaxInt) + SEM_FIM;
    Inc(vTotal);
  end;
  SetLength(ALinhas, vTotal);
end;

{ Myers }

/// Edit script of the middle part [AIni, AFim) x [BIni, BFim): '=' '-' '+' in
/// order; False when it needs more than LIMITE_EDICOES edits
function Myers(const A, B: TLinhas; AIni, AFim, BIni, BFim: integer;
  out AOps: TOps): boolean;
var
  vN, vM, vMax, vD, vK, vX, vY, vPrevK, vPrevX, vPrevY, vTotal, vPos: integer;
  vV: array of integer;
  vTrilha: TTrilha;
  vAchou: boolean;
  vRev: TOps;

  function Ler(AK: integer): integer;
  begin
    Result := vV[AK + vMax + 1];
  end;

  procedure Gravar(AK, AX: integer);
  begin
    vV[AK + vMax + 1] := AX;
  end;

  procedure Guardar(ADist: integer);
  var
    vI: integer;
  begin
    SetLength(vTrilha[ADist], 2 * ADist + 1);
    for vI := -ADist to ADist do
      if Abs(vI) <= vMax then
        vTrilha[ADist][vI + ADist] := Ler(vI);
  end;

  function Anterior(ADist, AKk: integer): integer;
  begin
    // k-1 or k+1 of the state saved before distance ADist
    if (AKk = -ADist) or ((AKk <> ADist) and
       (vTrilha[ADist][AKk - 1 + ADist] < vTrilha[ADist][AKk + 1 + ADist])) then
      Result := AKk + 1
    else
      Result := AKk - 1;
  end;

begin
  vN := AFim - AIni;
  vM := BFim - BIni;
  vMax := vN + vM;
  if vMax > LIMITE_EDICOES then
    vMax := LIMITE_EDICOES;
  SetLength(vV, 2 * vMax + 3);
  SetLength(vTrilha, vMax + 1);
  Gravar(1, 0);
  vAchou := False;
  vD := 0;
  while (vD <= vMax) and not vAchou do
  begin
    Guardar(vD);
    vK := -vD;
    while vK <= vD do
    begin
      if (vK = -vD) or ((vK <> vD) and (Ler(vK - 1) < Ler(vK + 1))) then
        vX := Ler(vK + 1)
      else
        vX := Ler(vK - 1) + 1;
      vY := vX - vK;
      while (vX < vN) and (vY < vM) and (A[AIni + vX] = B[BIni + vY]) do
      begin
        Inc(vX);
        Inc(vY);
      end;
      Gravar(vK, vX);
      if (vX >= vN) and (vY >= vM) then
      begin
        vAchou := True;
        Break;
      end;
      Inc(vK, 2);
    end;
    if not vAchou then
      Inc(vD);
  end;
  Result := vAchou;
  if not vAchou then
    Exit;

  // walks back from (n, m) to (0, 0)
  SetLength(vRev, vN + vM + 1);
  vTotal := 0;
  vX := vN;
  vY := vM;
  while vD >= 0 do
  begin
    if vD = 0 then
    begin
      vPrevX := 0;
      vPrevY := 0;
    end
    else
    begin
      vPrevK := Anterior(vD, vX - vY);
      vPrevX := vTrilha[vD][vPrevK + vD];
      vPrevY := vPrevX - vPrevK;
    end;
    while (vX > vPrevX) and (vY > vPrevY) do
    begin
      vRev[vTotal] := '=';
      Inc(vTotal);
      Dec(vX);
      Dec(vY);
    end;
    if vD > 0 then
    begin
      if vX = vPrevX then
        vRev[vTotal] := '+'
      else
        vRev[vTotal] := '-';
      Inc(vTotal);
    end;
    vX := vPrevX;
    vY := vPrevY;
    Dec(vD);
  end;
  SetLength(AOps, vTotal);
  for vPos := 0 to Pred(vTotal) do
    AOps[vPos] := vRev[vTotal - 1 - vPos];
end;

/// Edit script of the whole files: common head and tail first, Myers in between
procedure Roteiro(const A, B: TLinhas; out AOps: TOps);
var
  vIni, vFimA, vFimB, vInt, vTotal: integer;
  vMeio: TOps;
begin
  vIni := 0;
  while (vIni < Length(A)) and (vIni < Length(B)) and (A[vIni] = B[vIni]) do
    Inc(vIni);
  vFimA := Length(A);
  vFimB := Length(B);
  while (vFimA > vIni) and (vFimB > vIni) and (A[vFimA - 1] = B[vFimB - 1]) do
  begin
    Dec(vFimA);
    Dec(vFimB);
  end;
  if not Myers(A, B, vIni, vFimA, vIni, vFimB, vMeio) then
  begin
    SetLength(vMeio, (vFimA - vIni) + (vFimB - vIni));
    for vInt := 0 to Pred(vFimA - vIni) do
      vMeio[vInt] := '-';
    for vInt := 0 to Pred(vFimB - vIni) do
      vMeio[(vFimA - vIni) + vInt] := '+';
  end;
  SetLength(AOps, vIni + Length(vMeio) + (Length(A) - vFimA));
  vTotal := 0;
  for vInt := 1 to vIni do
  begin
    AOps[vTotal] := '=';
    Inc(vTotal);
  end;
  for vInt := 0 to High(vMeio) do
  begin
    AOps[vTotal] := vMeio[vInt];
    Inc(vTotal);
  end;
  for vInt := vFimA + 1 to Length(A) do
  begin
    AOps[vTotal] := '=';
    Inc(vTotal);
  end;
end;

{ Hunks }

/// The line as shown: without the marker, followed by the git note when needed
function LinhaVisivel(const APrefixo: char; const ALinha: string): string;
var
  vTam: integer;
begin
  vTam := Length(ALinha);
  if (vTam > 0) and (ALinha[vTam] = SEM_FIM) then
    Result := APrefixo + Copy(ALinha, 1, vTam - 1) + #10 + SEM_FIM_TEXTO + #10
  else
    Result := APrefixo + ALinha + #10;
end;

function Cabecalho(AIni, ACont: integer): string;
begin
  // a range with no lines points at the line before it
  if ACont = 0 then
    Dec(AIni);
  Result := IntToStr(AIni + 1) + ',' + IntToStr(ACont);
end;

/// One hunk: the ops [AIni, AFim), with the counters of lines consumed before
function Bloco(const A, B: TLinhas; const AOps: TOps; const APA, APB: array of integer;
  AIni, AFim: integer): string;
var
  vInt, vContA, vContB: integer;
begin
  vContA := 0;
  vContB := 0;
  Result := '';
  for vInt := AIni to Pred(AFim) do
  begin
    case AOps[vInt] of
      '=':
        begin
          Result := Result + LinhaVisivel(' ', A[APA[vInt]]);
          Inc(vContA);
          Inc(vContB);
        end;
      '-':
        begin
          Result := Result + LinhaVisivel('-', A[APA[vInt]]);
          Inc(vContA);
        end;
      '+':
        begin
          Result := Result + LinhaVisivel('+', B[APB[vInt]]);
          Inc(vContB);
        end;
    end;
  end;
  Result := '@@ -' + Cabecalho(APA[AIni], vContA) + ' +' +
            Cabecalho(APB[AIni], vContB) + ' @@' + #10 + Result;
end;

function DiffUnificado(const ANome: string; const AAntes, ADepois: RawByteString;
  AContexto: integer): string;
var
  vA, vB: TLinhas;
  vOps: TOps;
  vPA, vPB: array of integer;
  vInt, vIni, vFim, vFimMudanca, vIgual, vL: integer;
begin
  Result := '';
  Dividir(ParaUtf8(AAntes), vA);
  Dividir(ParaUtf8(ADepois), vB);
  Roteiro(vA, vB, vOps);
  vL := Length(vOps);
  SetLength(vPA, vL + 1);
  SetLength(vPB, vL + 1);
  vIni := 0;
  vFim := 0;
  for vInt := 0 to Pred(vL) do
  begin
    vPA[vInt] := vIni;
    vPB[vInt] := vFim;
    if vOps[vInt] <> '+' then
      Inc(vIni);
    if vOps[vInt] <> '-' then
      Inc(vFim);
  end;
  vPA[vL] := vIni;
  vPB[vL] := vFim;

  vInt := 0;
  while vInt < vL do
  begin
    if vOps[vInt] = '=' then
    begin
      Inc(vInt);
      Continue;
    end;
    vIni := vInt - AContexto;
    if vIni < 0 then
      vIni := 0;
    // extends while the next change is within twice the context
    vFimMudanca := vInt;
    repeat
      vInt := vFimMudanca;
      while (vInt < vL) and (vOps[vInt] <> '=') do
        Inc(vInt);
      vIgual := vInt;
      while (vIgual < vL) and (vOps[vIgual] = '=') do
        Inc(vIgual);
      vFimMudanca := vIgual;
    until (vIgual >= vL) or (vIgual - vInt > 2 * AContexto);
    vFim := vInt + AContexto;
    if vFim > vL then
      vFim := vL;
    if Result = '' then
      Result := '--- a/' + ANome + #10 + '+++ b/' + ANome + #10;
    Result := Result + Bloco(vA, vB, vOps, vPA, vPB, vIni, vFim);
    vInt := vFim;
  end;
end;

procedure ResumoDiff(const ADiff: string; out AAcrescentadas, ATiradas: integer);
var
  vLinhas: TStringList;
  vInt: integer;
  vNoBloco: boolean;
begin
  AAcrescentadas := 0;
  ATiradas := 0;
  vNoBloco := False;
  vLinhas := TStringList.Create;
  try
    vLinhas.Text := ADiff;
    for vInt := 0 to Pred(vLinhas.Count) do
    begin
      if Copy(vLinhas[vInt], 1, 2) = '@@' then
        vNoBloco := True
      else if vNoBloco and (vLinhas[vInt] <> '') then
        case vLinhas[vInt][1] of
          '+': Inc(AAcrescentadas);
          '-': Inc(ATiradas);
        end;
    end;
  finally
    vLinhas.Free;
  end;
end;

function TextoResumoDiff(const ADiff: string): string;
var
  vMais, vMenos: integer;
begin
  ResumoDiff(ADiff, vMais, vMenos);
  if (vMais = 0) and (vMenos = 0) then
    Result := cmPreviaSemMudanca
  else
    Result := Format(cmPreviaResumo, [vMais, vMenos]);
end;

end.
