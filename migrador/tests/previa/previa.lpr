program previa;

{$mode ObjFPC}{$H+}

// Prévia do migrador (etapa M2): diff unificado.
//   previa --testes
//
// Compilar (da pasta do teste):
//   fpc -B -Fu../../src -Fu../../../src/classes previa.lpr

uses
  Classes, SysUtils,
  RALMig.Previa;

var
  GFalhas: integer = 0;

procedure Conferir(ACondicao: boolean; const ADescricao: string);
begin
  if ACondicao then
    WriteLn('  ok    ', ADescricao)
  else
  begin
    WriteLn('  FALHA ', ADescricao);
    Inc(GFalhas);
  end;
end;

/// The lines of a text, whatever its terminator (what the diff compares)
function Linhas(const ATexto: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Text := ATexto;
end;

function Mesmo(AA, AB: TStrings): boolean;
var
  vInt: integer;
begin
  Result := AA.Count = AB.Count;
  if Result then
    for vInt := 0 to Pred(AA.Count) do
      if AA[vInt] <> AB[vInt] then
        Exit(False);
end;

/// Applies the diff to the lines of the original; the result must be the
/// lines of the changed text
function Aplicar(const AAntes, ADiff: string): TStringList;
var
  vDiff, vOrig: TStringList;
  vInt, vPos, vIni, vPosTexto: integer;
  vLinha: string;
begin
  Result := TStringList.Create;
  vDiff := TStringList.Create;
  vOrig := Linhas(AAntes);
  try
    vDiff.Text := ADiff;
    vPos := 0;
    for vInt := 0 to Pred(vDiff.Count) do
    begin
      vLinha := vDiff[vInt];
      if Copy(vLinha, 1, 2) = '@@' then
      begin
        vPosTexto := Pos('-', vLinha) + 1;
        vIni := StrToInt(Copy(vLinha, vPosTexto, Pos(',', vLinha) - vPosTexto));
        if (Pos(',0 ', vLinha) > 0) and (Pos(',0 ', vLinha) < Pos('+', vLinha)) then
          Inc(vIni);
        while vPos < vIni - 1 do
        begin
          Result.Add(vOrig[vPos]);
          Inc(vPos);
        end;
      end
      else if (Copy(vLinha, 1, 3) = '---') and (vInt = 0) then
        Continue
      else if (Copy(vLinha, 1, 3) = '+++') and (vInt = 1) then
        Continue
      else if vLinha = '' then
        Continue
      else if vLinha[1] = ' ' then
      begin
        Result.Add(vOrig[vPos]);
        Inc(vPos);
      end
      else if vLinha[1] = '-' then
        Inc(vPos)
      else if vLinha[1] = '+' then
        Result.Add(Copy(vLinha, 2, MaxInt));
    end;
    while vPos < vOrig.Count do
    begin
      Result.Add(vOrig[vPos]);
      Inc(vPos);
    end;
  finally
    vOrig.Free;
    vDiff.Free;
  end;
end;

/// Diff + apply = the changed text (the proof that the diff is right)
procedure ConferirRoundTrip(const ADescricao, AAntes, ADepois: string);
var
  vDiff: string;
  vRes, vEsperado: TStringList;
begin
  vDiff := DiffUnificado('a.pas', AAntes, ADepois);
  vRes := Aplicar(AAntes, vDiff);
  vEsperado := Linhas(ADepois);
  try
    Conferir(Mesmo(vRes, vEsperado), ADescricao + ': aplicar o diff dá o texto novo');
  finally
    vRes.Free;
    vEsperado.Free;
  end;
end;

procedure TesteSemMudanca;
var
  vMais, vMenos: integer;
begin
  WriteLn('sem mudanca');
  Conferir(DiffUnificado('a.pas', 'um'#10'dois'#10, 'um'#10'dois'#10) = '', 'iguais = diff vazio');
  Conferir(DiffUnificado('a.pas', '', '') = '', 'dois vazios = diff vazio');
  Conferir(DiffUnificado('a.pas', 'um'#13#10'dois'#13#10, 'um'#10'dois'#10) = '',
           'so o fim de linha mudou = diff vazio (compara sem o terminador)');
  Conferir(DiffUnificado('a.pas', 'um'#13'dois'#13, 'um'#10'dois'#10) = '', 'CR = LF = CRLF');
  ResumoDiff('', vMais, vMenos);
  Conferir((vMais = 0) and (vMenos = 0), 'resumo de diff vazio');
end;

procedure TesteCabecalho;
var
  vDiff: string;
begin
  WriteLn('cabecalho e blocos');
  vDiff := DiffUnificado('src/u.pas', 'a'#10'b'#10'c'#10, 'a'#10'B'#10'c'#10);
  Conferir(Copy(vDiff, 1, 16) = '--- a/src/u.pas'#10, 'linha ---');
  Conferir(Pos('+++ b/src/u.pas'#10, vDiff) = 17, 'linha +++');
  Conferir(Pos('@@ -1,3 +1,3 @@'#10, vDiff) > 0, 'cabecalho do bloco: ' + StringReplace(vDiff, #10, '|', [rfReplaceAll]));
  Conferir(Pos(#10'-b'#10'+B'#10, vDiff) > 0, 'a linha trocada sai como - e +');
  Conferir(Pos(#10' a'#10, vDiff) > 0, 'contexto antes');
  Conferir(Pos(#10' c'#10, vDiff) > 0, 'contexto depois');
end;

procedure TesteInsercoes;
var
  vDiff: string;
  vMais, vMenos: integer;
begin
  WriteLn('insercao no comeco, no fim, arquivo vazio');
  vDiff := DiffUnificado('a', 'b'#10'c'#10, 'a'#10'b'#10'c'#10);
  Conferir(Pos('@@ -1,2 +1,3 @@', vDiff) > 0, 'insercao no comeco: ' + StringReplace(vDiff, #10, '|', [rfReplaceAll]));
  Conferir(Pos(#10'+a'#10, vDiff) > 0, 'linha acrescentada');
  ConferirRoundTrip('comeco', 'b'#10'c'#10, 'a'#10'b'#10'c'#10);
  vDiff := DiffUnificado('a', 'a'#10'b'#10, 'a'#10'b'#10'c'#10);
  Conferir(Pos('@@ -1,2 +1,3 @@', vDiff) > 0, 'insercao no fim: ' + StringReplace(vDiff, #10, '|', [rfReplaceAll]));
  ConferirRoundTrip('fim', 'a'#10'b'#10, 'a'#10'b'#10'c'#10);
  vDiff := DiffUnificado('a', '', 'x'#10'y'#10);
  Conferir(Pos('@@ -0,0 +1,2 @@', vDiff) > 0, 'de vazio: ' + StringReplace(vDiff, #10, '|', [rfReplaceAll]));
  ResumoDiff(vDiff, vMais, vMenos);
  Conferir((vMais = 2) and (vMenos = 0), 'resumo de vazio -> 2 linhas');
  vDiff := DiffUnificado('a', 'x'#10'y'#10, '');
  Conferir(Pos('@@ -1,2 +0,0 @@', vDiff) > 0, 'para vazio: ' + StringReplace(vDiff, #10, '|', [rfReplaceAll]));
  ResumoDiff(vDiff, vMais, vMenos);
  Conferir((vMais = 0) and (vMenos = 2), 'resumo para vazio -> 2 tiradas');
  ConferirRoundTrip('para vazio', 'x'#10'y'#10, '');
end;

procedure TesteFimDeLinha;
var
  vDiff: string;
begin
  WriteLn('fim de linha e ultima linha sem terminador');
  vDiff := DiffUnificado('a', 'um'#13#10'dois'#13#10, 'um'#13#10'DOIS'#13#10);
  Conferir(Pos(#13, vDiff) = 0, 'a saida usa so LF');
  vDiff := DiffUnificado('a', 'um'#10'dois', 'um'#10'dois'#10);
  Conferir(Pos('-dois'#10'\ No newline at end of file'#10'+dois'#10, vDiff) > 0,
           'faltava o ultimo fim de linha: ' + StringReplace(vDiff, #10, '|', [rfReplaceAll]));
  vDiff := DiffUnificado('a', 'um'#10'dois'#10, 'um'#10'dois');
  Conferir(Pos('-dois'#10'+dois'#10'\ No newline at end of file'#10, vDiff) > 0,
           'passou a faltar o ultimo fim de linha');
  vDiff := DiffUnificado('a', 'um'#10'dois', 'um'#10'tres');
  Conferir(Pos('-dois'#10'\ No newline at end of file'#10'+tres'#10'\ No newline at end of file'#10, vDiff) > 0,
           'os dois sem terminador');
  ConferirRoundTrip('sem terminador', 'um'#10'dois', 'um'#10'tres');
end;

procedure TesteContexto;
var
  vAntes, vDepois, vDiff: string;
  vInt: integer;
begin
  WriteLn('contexto e varios blocos');
  vAntes := '';
  vDepois := '';
  for vInt := 1 to 40 do
  begin
    vAntes := vAntes + 'linha' + IntToStr(vInt) + #10;
    if (vInt = 5) or (vInt = 35) then
      vDepois := vDepois + 'MUDOU' + IntToStr(vInt) + #10
    else
      vDepois := vDepois + 'linha' + IntToStr(vInt) + #10;
  end;
  vDiff := DiffUnificado('a', vAntes, vDepois);
  Conferir(Pos('@@ -2,7 +2,7 @@', vDiff) > 0, 'bloco 1 com 3 linhas de contexto: ' + StringReplace(vDiff, #10, '|', [rfReplaceAll]));
  Conferir(Pos('@@ -32,7 +32,7 @@', vDiff) > 0, 'bloco 2 separado');
  ConferirRoundTrip('dois blocos', vAntes, vDepois);
  vDiff := DiffUnificado('a', vAntes, vDepois, 0);
  Conferir(Pos('@@ -5,1 +5,1 @@', vDiff) > 0, 'contexto 0');
  vDiff := DiffUnificado('a', vAntes, vDepois, 40);
  Conferir(Pos('@@ -1,40 +1,40 @@', vDiff) > 0, 'contexto grande junta os blocos');
  ConferirRoundTrip('blocos juntos', vAntes, vDepois);
end;

procedure TesteResumo;
var
  vDiff: string;
  vMais, vMenos: integer;
begin
  WriteLn('resumo');
  vDiff := DiffUnificado('a', 'a'#10'b'#10'c'#10'd'#10, 'a'#10'x'#10'y'#10'd'#10);
  ResumoDiff(vDiff, vMais, vMenos);
  Conferir((vMais = 2) and (vMenos = 2), 'troca de duas linhas: +2 -2');
  Conferir(TextoResumoDiff(vDiff) <> '', 'texto do resumo');
  vDiff := DiffUnificado('a', '-x'#10, '++y'#10);
  ResumoDiff(vDiff, vMais, vMenos);
  Conferir((vMais = 1) and (vMenos = 1), 'linhas que comecam com + e - contam uma vez');
end;

procedure TesteCodificacao;
var
  vDiff: string;
begin
  WriteLn('codificacao');
  vDiff := DiffUnificado('a', 'caf'#$C3#$A9#10, 'caf'#$C3#$A9' 2'#10);
  Conferir(Pos('-caf'#$C3#$A9#10, vDiff) > 0, 'UTF-8 valido sai como esta');
  vDiff := DiffUnificado('a', 'caf'#$E9#10, 'caf'#$E9' 2'#10);
  Conferir(Pos(#$E9, vDiff) = 0, 'ANSI nao sai como byte solto: vira UTF-8');
end;

procedure TesteAleatorio;
var
  vInt, vLinha, vN: integer;
  vAntes, vDepois: string;
begin
  WriteLn('aleatorio: aplicar o diff sempre da o texto novo');
  RandSeed := 12345;
  for vInt := 1 to 150 do
  begin
    vAntes := '';
    vDepois := '';
    for vN := 1 to Random(30) do
      vAntes := vAntes + Chr(Ord('a') + Random(5)) + #10;
    for vN := 1 to Random(30) do
      vDepois := vDepois + Chr(Ord('a') + Random(5)) + #10;
    vLinha := Random(2);
    if vLinha = 1 then
      vDepois := vDepois + 'fim';
    ConferirRoundTrip('caso ' + IntToStr(vInt), vAntes, vDepois);
  end;
end;

procedure TesteGrande;
var
  vAntes, vDepois: string;
  vInt: integer;
  vIni: QWord;
begin
  WriteLn('arquivo grande');
  vAntes := '';
  vDepois := '';
  for vInt := 1 to 8000 do
  begin
    vAntes := vAntes + 'linha ' + IntToStr(vInt) + #10;
    if vInt mod 500 = 0 then
      vDepois := vDepois + 'trocada ' + IntToStr(vInt) + #10
    else
      vDepois := vDepois + 'linha ' + IntToStr(vInt) + #10;
  end;
  vIni := GetTickCount64;
  ConferirRoundTrip('8000 linhas, 16 trocas', vAntes, vDepois);
  Conferir(GetTickCount64 - vIni < 10000, 'em menos de 10 s');
  vDepois := '';
  for vInt := 1 to 6000 do
    vDepois := vDepois + 'outra ' + IntToStr(vInt) + #10;
  ConferirRoundTrip('texto todo diferente (acima do limite de edicoes)', vAntes, vDepois);
end;

begin
  if (ParamCount < 1) or (ParamStr(1) <> '--testes') then
  begin
    WriteLn('previa --testes');
    Halt(2);
  end;
  TesteSemMudanca;
  TesteCabecalho;
  TesteInsercoes;
  TesteFimDeLinha;
  TesteContexto;
  TesteResumo;
  TesteCodificacao;
  TesteAleatorio;
  TesteGrande;
  WriteLn;
  if GFalhas = 0 then
    WriteLn('tudo certo')
  else
    WriteLn(GFalhas, ' falha(s)');
  if GFalhas > 0 then
    Halt(1);
end.
