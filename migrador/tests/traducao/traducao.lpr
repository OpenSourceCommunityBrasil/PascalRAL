program traducao;

{$mode ObjFPC}{$H+}

// Os .po do migrador cobrem todo texto das units do nucleo e da console.
//
//   traducao [--languages=<pasta com os ralmigrador.<idioma>.po>]
//            [--rsj=<pasta dos RALMig.*.rsj>]
//
// Sem --languages procura migrador/languages subindo da pasta do executavel.
// Os textos vem dos RALMig.*.rsj que o compilador grava junto dos .ppu (sem
// --rsj, na pasta do executavel: compile com -FU e -FE na mesma pasta, ou
// passe --rsj=<a pasta do -FU>). Todas as units com resourcestring, menos as
// telas (LCL), tem de estar no uses deste teste.
// Falha (saida 1) quando:
//  - falta um dos tres .po (pt-BR, en-US, es-ES);
//  - alguma resourcestring das units RALMig.* sem LCL (nucleo e console) nao tem
//    entrada no en-US ou no es-ES;
//  - algum msgstr do en-US ou do es-ES esta vazio;
//  - as marcas de um texto nao batem com as do original: %s %d %x... do Format e
//    %nome% (dois ou mais caracteres entre dois %, como %arquivo%);
//  - um texto do en-US ou do es-ES traz letra tipica do portugues: Ã£ Ãµ Ã§ Ãª
//    (nenhuma das duas lingua usa), e no en-US tambem Ã¡ seguido de letra
//    minuscula (o espanhol usa Ã¡). A lista e curta de proposito: pega o texto que
//    ficou em portugues sem dar falso alarme com nome proprio.
// As telas (RALMig.Tela.*) usam LCL e ficam de fora; o .po delas e conferido
// pelo gerador, que as coleta com a janela aberta.
//
// Compilar (da raiz do instalador):
//   fpc -B -MObjFPC -Sh -Fumigrador/src -Fusrc/classes -FUmigrador/lib/testes
//       -FEmigrador/lib/testes migrador/tests/traducao/traducao.lpr

uses
  Classes, SysUtils, StrUtils, fpjson, jsonparser,
  RALInst.Traducao,
  RALMig.API, RALMig.Texto, RALMig.Tokens, RALMig.&Uses, RALMig.Formulario,
  RALMig.ProjetoXML, RALMig.Regras, RALMig.Tipos, RALMig.Projeto, RALMig.Motor,
  RALMig.Deteccao, RALMig.Inventario, RALMig.Previa, RALMig.Copia,
  RALMig.Acoes.Projeto, RALMig.Acoes.Formulario, RALMig.Acoes.Codigo,
  RALMig.Validacao, RALMig.Conferir, RALMig.Relatorio,
  RALMig.Mensagens, RALMig.Console.Mensagens;

const
  IDIOMAS: array[0..2] of string = ('pt-BR', 'en-US', 'es-ES');
  /// Messages shown per kind of failure before "and N more"
  LIMITE_POR_TIPO = 12;
  // Ã£ Ãµ Ã§ Ãª Ã¡ in UTF-8
  A_TIL = #$C3#$A3;
  O_TIL = #$C3#$B5;
  C_CEDILHA = #$C3#$A7;
  E_CIRCUNFLEXO = #$C3#$AA;
  A_AGUDO = #$C3#$A1;

type
  /// One entry of a .po file as written, to see what TArquivoPO hides: a msgstr
  /// left empty becomes the original text there
  TEntradaBruta = record
    Id: string;
    Vazia: boolean;
  end;

var
  GFalhas: integer = 0;
  GPorTipo: TStringList;
  /// Name (unit.resourcestring, lowercase) and value of each collected text
  GNomes: TStringList;
  /// The .rsj files read
  GArquivosRsj: TStringList;
  GValores: TStringList;
  GPasta: string;

procedure Falhar(const ATipo, ATexto: string);
var
  vInd, vQtd: integer;
begin
  Inc(GFalhas);
  vInd := GPorTipo.IndexOf(ATipo);
  if vInd < 0 then
    vInd := GPorTipo.Add(ATipo);
  vQtd := PtrInt(GPorTipo.Objects[vInd]) + 1;
  GPorTipo.Objects[vInd] := TObject(PtrInt(vQtd));
  if vQtd <= LIMITE_POR_TIPO then
    WriteLn('  FALHA ', ATipo, ': ', ATexto);
end;

procedure ResumirTipo(const ATipo: string);
var
  vInd, vQtd: integer;
begin
  vInd := GPorTipo.IndexOf(ATipo);
  vQtd := 0;
  if vInd >= 0 then
    vQtd := PtrInt(GPorTipo.Objects[vInd]);
  if vQtd = 0 then
    WriteLn('  ok    ', ATipo)
  else if vQtd > LIMITE_POR_TIPO then
    WriteLn('        ... e mais ', vQtd - LIMITE_POR_TIPO, ' em "', ATipo, '"');
end;

function Opcao(const ANome, APadrao: string): string;
var
  vInt: integer;
begin
  Result := APadrao;
  for vInt := 1 to ParamCount do
    if Copy(ParamStr(vInt), 1, Length(ANome) + 3) = '--' + ANome + '=' then
      Result := Copy(ParamStr(vInt), Length(ANome) + 4, MaxInt);
end;

function ArquivoPO(const AIdioma: string): string;
begin
  Result := IncludeTrailingPathDelimiter(GPasta) + 'ralmigrador.' + AIdioma + '.po';
end;

function LerArquivo(const ACaminho: string): string;
var
  vLista: TStringList;
begin
  vLista := TStringList.Create;
  try
    vLista.LoadFromFile(ACaminho);
    Result := vLista.Text;
  finally
    vLista.Free;
  end;
end;

{ collecting the resourcestrings }

// The compiler writes <Unit>.rsj (every resourcestring of the unit, with the
// exact bytes of the source) beside the .ppu of each unit it compiles. They are
// read here instead of asking the running program (SetResourceStrings, as the
// .po generator does): the program only hands over the strings its reachable
// code uses, so a message of a path this test never takes would go unchecked.
// The uses clause above exists to make the compiler build every unit.

function BytesDoRsj(AItem: TJSONObject): string;
var
  vBytes: TJSONArray;
  vInt: integer;
begin
  Result := '';
  vBytes := AItem.Arrays['sourcebytes'];
  for vInt := 0 to vBytes.Count - 1 do
    Result := Result + Chr(vBytes.Integers[vInt]);
end;

procedure LerRsj(const AArquivo: string);
var
  vFluxo: TFileStream;
  vDados: TJSONData;
  vLista: TJSONArray;
  vInt: integer;
begin
  vFluxo := TFileStream.Create(AArquivo, fmOpenRead or fmShareDenyNone);
  try
    vDados := GetJSON(vFluxo);
  finally
    vFluxo.Free;
  end;
  try
    vLista := TJSONObject(vDados).Arrays['strings'];
    for vInt := 0 to vLista.Count - 1 do
    begin
      GNomes.Add(TJSONObject(vLista[vInt]).Strings['name']);
      GValores.Add(BytesDoRsj(TJSONObject(vLista[vInt])));
    end;
  finally
    vDados.Free;
  end;
end;

procedure ColetarMensagens;
var
  vBusca: TSearchRec;
  vPasta: string;
begin
  GNomes := TStringList.Create;
  GValores := TStringList.Create;
  vPasta := IncludeTrailingPathDelimiter(Opcao('rsj', ExtractFilePath(ParamStr(0))));
  if FindFirst(vPasta + 'RALMig.*.rsj', faAnyFile, vBusca) = 0 then
  try
    repeat
      if Copy(vBusca.Name, 1, 12) <> 'RALMig.Tela.' then
      begin
        GArquivosRsj.Add(vBusca.Name);
        LerRsj(vPasta + vBusca.Name);
      end;
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
end;

/// Every unit of migrador/src with a resourcestring (but the LCL ones) must
/// have been compiled by this test, or its texts would go unchecked
procedure ConferirUnitsCobertas;
var
  vBusca: TSearchRec;
  vPasta: string;
begin
  vPasta := AcharPastaMigrador + PathDelim + 'src' + PathDelim;
  if FindFirst(vPasta + 'RALMig.*.pas', faAnyFile, vBusca) = 0 then
  try
    repeat
      if (Copy(vBusca.Name, 1, 12) <> 'RALMig.Tela.') and
         (Pos(LineEnding + 'resourcestring' + LineEnding,
           LowerCase(LerArquivo(vPasta + vBusca.Name))) > 0) and
         (GArquivosRsj.IndexOf(ChangeFileExt(vBusca.Name, '.rsj')) < 0) then
        Falhar('unit fora do teste', vBusca.Name + ' tem resourcestring: ponha no uses ' +
          'deste teste');
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
  ResumirTipo('unit fora do teste');
end;
{ marks }

/// The marks of a text, one per line, sorted: Format specs ('%s', '%d', '%2:s',
/// '%0.2f'...) and '%name%'. '%%' is a literal and does not count.
function MarcasDe(const ATexto: string): string;
var
  vMarcas: TStringList;
  vInt, vFim: integer;

  function FimDoNome(AIni: integer): integer;
  begin
    Result := AIni;
    while (Result <= Length(ATexto)) and (ATexto[Result] in ['A'..'Z', 'a'..'z', '_']) do
      Inc(Result);
  end;

  function FimDoFormato(AIni: integer): integer;
  begin
    Result := AIni;
    while (Result <= Length(ATexto)) and
          (ATexto[Result] in ['0'..'9', ':', '.', '-', '*']) do
      Inc(Result);
  end;

begin
  vMarcas := TStringList.Create;
  try
    vMarcas.Sorted := True;
    vMarcas.Duplicates := dupAccept;
    vInt := 1;
    while vInt <= Length(ATexto) do
    begin
      if ATexto[vInt] <> '%' then
        Inc(vInt)
      else if (vInt < Length(ATexto)) and (ATexto[vInt + 1] = '%') then
        Inc(vInt, 2)
      else
      begin
        vFim := FimDoNome(vInt + 1);
        if (vFim - vInt > 2) and (vFim <= Length(ATexto)) and (ATexto[vFim] = '%') then
          Inc(vFim)
        else
          vFim := FimDoFormato(vInt + 1) + 1;
        vMarcas.Add(LowerCase(Copy(ATexto, vInt, vFim - vInt)));
        vInt := vFim;
      end;
    end;
    Result := vMarcas.Text;
  finally
    vMarcas.Free;
  end;
end;

{ the raw .po }

/// The entries whose msgstr is empty, by the '#:' reference
procedure EntradasVazias(const ATexto: string; ADest: TStringList);
var
  vLinhas: TStringList;
  vInt: integer;
  vLinha, vId, vOriginal, vTraducao: string;
  vCampo: char;

  procedure Fechar;
  begin
    if (vId <> '') and (vOriginal <> '') and (vTraducao = '') then
      ADest.Add(vId);
    vId := '';
    vOriginal := '';
    vTraducao := '';
    vCampo := ' ';
  end;

  function Aspas: string;
  var
    vIni: integer;
  begin
    vIni := Pos('"', vLinha);
    Result := Copy(vLinha, vIni + 1, LastDelimiter('"', vLinha) - vIni - 1);
  end;

begin
  vLinhas := TStringList.Create;
  try
    vLinhas.Text := ATexto;
    vCampo := ' ';
    vId := '';
    vOriginal := '';
    vTraducao := '';
    for vInt := 0 to vLinhas.Count - 1 do
    begin
      vLinha := Trim(vLinhas[vInt]);
      if vLinha = '' then
        Fechar
      else if Copy(vLinha, 1, 3) = '#: ' then
      begin
        if vOriginal <> '' then
          Fechar;
        vId := Trim(Copy(vLinha, 4, MaxInt));
      end
      else if Copy(vLinha, 1, 6) = 'msgid ' then
      begin
        vCampo := 'i';
        vOriginal := Aspas;
      end
      else if Copy(vLinha, 1, 7) = 'msgstr ' then
      begin
        vCampo := 's';
        vTraducao := Aspas;
      end
      else if (vLinha[1] = '"') and (vCampo = 'i') then
        vOriginal := vOriginal + Aspas
      else if (vLinha[1] = '"') and (vCampo = 's') then
        vTraducao := vTraducao + Aspas;
    end;
    Fechar;
  finally
    vLinhas.Free;
  end;
end;

{ the checks }

function TemPortugues(const ATexto, AIdioma: string): boolean;
var
  vPos: integer;
begin
  Result := (Pos(A_TIL, ATexto) > 0) or (Pos(O_TIL, ATexto) > 0) or
    (Pos(C_CEDILHA, ATexto) > 0) or (Pos(E_CIRCUNFLEXO, ATexto) > 0);
  if Result or (AIdioma <> 'en-US') then
    Exit;
  // a com agudo seguido de letra minuscula: "e" em espanhol, "a" em portugues
  vPos := Pos(A_AGUDO, ATexto);
  while vPos > 0 do
  begin
    if (Length(ATexto) >= vPos + 2) and (ATexto[vPos + 2] in ['a'..'z']) then
      Exit(True);
    vPos := PosEx(A_AGUDO, ATexto, vPos + 2);
  end;
end;

procedure ConferirTexto(const AIdioma, AId, AOriginal, ATraducao: string);
begin
  if MarcasDe(ATraducao) <> MarcasDe(AOriginal) then
    Falhar(AIdioma + ': marcas diferentes', AId + ' (original: ' +
      StringReplace(Trim(MarcasDe(AOriginal)), LineEnding, ' ', [rfReplaceAll]) +
      ' / traducao: ' + StringReplace(Trim(MarcasDe(ATraducao)), LineEnding, ' ',
      [rfReplaceAll]) + ')');
  if TemPortugues(ATraducao, AIdioma) then
    Falhar(AIdioma + ': texto em portugues', AId + ': ' + ATraducao);
end;

procedure ConferirIdioma(const AIdioma: string);
var
  vPO: TArquivoPO;
  vVazias: TStringList;
  vInt: integer;
  vId, vTraducao: string;
begin
  WriteLn(AIdioma);
  vPO := TArquivoPO.Create;
  vVazias := TStringList.Create;
  try
    vVazias.CaseSensitive := False;
    vPO.CarregarTexto(LerArquivo(ArquivoPO(AIdioma)));
    EntradasVazias(LerArquivo(ArquivoPO(AIdioma)), vVazias);
    for vInt := 0 to GNomes.Count - 1 do
    begin
      vId := GNomes[vInt];
      vTraducao := vPO.Traduzir(vId, '');
      if vTraducao = '' then
        Falhar(AIdioma + ': sem entrada', vId)
      else if vVazias.IndexOf(vId) >= 0 then
        Falhar(AIdioma + ': msgstr vazio', vId)
      else
        ConferirTexto(AIdioma, vId, GValores[vInt], vTraducao);
    end;
  finally
    vVazias.Free;
    vPO.Free;
  end;
  ResumirTipo(AIdioma + ': sem entrada');
  ResumirTipo(AIdioma + ': msgstr vazio');
  ResumirTipo(AIdioma + ': marcas diferentes');
  ResumirTipo(AIdioma + ': texto em portugues');
end;

function PastaDosPO: string;
begin
  Result := Opcao('languages', '');
  if Result = '' then
    Result := AcharPastaMigrador + PathDelim + 'languages';
end;

function ArquivosExistem: boolean;
var
  vIdioma: string;
begin
  Result := True;
  for vIdioma in IDIOMAS do
    if not FileExists(ArquivoPO(vIdioma)) then
    begin
      Falhar('arquivo ausente', ArquivoPO(vIdioma));
      Result := False;
    end;
end;

begin
  DefaultSystemCodePage := CP_UTF8;
  GPorTipo := TStringList.Create;
  GArquivosRsj := TStringList.Create;
  GArquivosRsj.CaseSensitive := False;
  ColetarMensagens;
  WriteLn(GNomes.Count, ' resourcestring(s) em ', GArquivosRsj.Count, ' unit(s) do nucleo e da console');
  if GNomes.Count = 0 then
    Falhar('coleta', 'nenhum RALMig.*.rsj em ' + Opcao('rsj', ExtractFilePath(ParamStr(0))) + ' (use --rsj=<pasta das units compiladas>)');
  ConferirUnitsCobertas;
  GPasta := PastaDosPO;
  WriteLn('pasta dos .po: ', GPasta);
  if ArquivosExistem then
  begin
    ConferirIdioma('en-US');
    ConferirIdioma('es-ES');
  end;
  ResumirTipo('arquivo ausente');
  if GFalhas = 0 then
    WriteLn('todos os testes passaram')
  else
    WriteLn(GFalhas, ' falha(s)');
  Halt(Ord(GFalhas > 0));
end.
