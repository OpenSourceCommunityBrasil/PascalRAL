program leitura;

{$mode ObjFPC}{$H+}

// Leitura e escrita sem estragar (migrador, etapa M1).
//   leitura --testes                 autoteste em memoria (Texto, Tokens, Uses,
//                                    Formulario, ProjetoXML), sem disco externo
//   leitura --samples=<pasta> [...]  ida e volta sem regra de todo .pas .dfm
//                                    .lfm .fmx .dproj .lpi .lpk .dpk .dpr .lpr
//                                    da pasta: bytes identicos ao original.
//                                    So le; a saida vai para a pasta temporaria.
//   leitura                          sem argumento: so --testes
//
// Sai com 1 se alguma verificacao falhou (formulario de texto que nao analisa
// e um achado nos samples, listado, e nao falha do leitor).
//
// Compilar (da raiz do instalador):
//   fpc -B -MObjFPC -Sh -Fumigrador/src -Fusrc/classes migrador/tests/leitura/leitura.lpr

uses
  Classes, SysUtils,
  RALMig.Texto, RALMig.Tokens, RALMig.&Uses, RALMig.Formulario, RALMig.ProjetoXML;

var
  GFalhas: integer = 0;
  GPastas: TStringList;
  GSoTestes: boolean = False;

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

function Visivel(const AText: string): string;
begin
  Result := StringReplace(StringReplace(AText, #13, '\r', [rfReplaceAll]),
    #10, '\n', [rfReplaceAll]);
end;

procedure ConferirTexto(const AObtido, AEsperado, ADescricao: string);
begin
  if AObtido = AEsperado then
    Conferir(True, ADescricao)
  else
  begin
    Conferir(False, ADescricao);
    WriteLn('        esperado: ', Visivel(AEsperado));
    WriteLn('        obtido:   ', Visivel(AObtido));
  end;
end;

// linhas separadas por LF, com LF no fim
function L(const ALinhas: array of string): string;
var
  i: integer;
begin
  Result := '';
  for i := Low(ALinhas) to High(ALinhas) do
    Result := Result + ALinhas[i] + #10;
end;

function CrLf(const AText: string): string;
begin
  Result := StringReplace(AText, #10, #13#10, [rfReplaceAll]);
end;

function NovoTexto(const AText: string; const ANome: string = 'x.pas'): TTextoOriginal;
begin
  Result := TTextoOriginal.Create;
  Result.CarregarBytes(AText, ANome);
end;

{ Texto }

procedure TesteTexto;
var
  vT: TTextoOriginal;
  vFalhou: boolean;
begin
  WriteLn('Texto');
  vT := NovoTexto('ab'#13#10'cd'#13#10'ef');
  try
    Conferir(vT.LinhaCount = 3, 'tres linhas (a ultima sem fim de linha)');
    Conferir(vT.FimDeLinha = #13#10, 'fim de linha dominante CRLF');
    Conferir(vT.LinhaDoOffset(5) = 2, 'offset 5 esta na linha 2');
    Conferir((vT.LinhaInicio(3) = 9) and (vT.LinhaFim(3) = 11), 'linha 3 vai de 9 a 11');
    Conferir(vT.TextoDaLinha(2) = 'cd', 'texto da linha 2');
    vT.AdicionarEdicao(1, 2, 'XY');
    vT.AdicionarEdicao(9, 0, '>');
    vT.AdicionarEdicao(9, 0, '>');
    ConferirTexto(vT.Resultado, 'XY'#13#10'cd'#13#10'>>ef', 'edicoes aplicadas de uma vez, na ordem');
    Conferir(vT.Alterado, 'alterado');
    vFalhou := False;
    try
      vT.AdicionarEdicao(2, 3, 'zzz');
    except
      on ETextoEdicao do
        vFalhou := True;
    end;
    Conferir(vFalhou, 'sobreposicao levanta ETextoEdicao');
  finally
    vT.Free;
  end;
  vT := NovoTexto('a'#10'b'#10);
  try
    Conferir((vT.LinhaCount = 2) and (vT.FimDeLinha = #10), 'LF: duas linhas');
    vT.InserirAntesDaLinha(2, 'novo'#13#10'outro');
    ConferirTexto(vT.Resultado, 'a'#10'novo'#10'outro'#10'b'#10, 'inserir linhas converte para o fim de linha do arquivo');
  finally
    vT.Free;
  end;
  vT := NovoTexto(#$EF#$BB#$BF'a'#13'b');
  try
    Conferir(vT.Codificacao = cdUtf8Bom, 'BOM detectado');
    Conferir((vT.LinhaCount = 2) and (vT.FimDeLinha = #13) and (vT.LinhaInicio(1) = 4),
      'CR sozinho e linha 1 depois do BOM');
  finally
    vT.Free;
  end;
  vT := NovoTexto('caf'#$E9'.');
  try
    Conferir(vT.Codificacao = cdAnsi, 'byte solto >= 128 e ANSI');
    Conferir(not vT.Alterado, 'sem edicao nao altera');
  finally
    vT.Free;
  end;
  vT := NovoTexto('caf'#$C3#$A9'.');
  try
    Conferir(vT.Codificacao = cdUtf8, 'UTF-8 sem BOM detectado');
  finally
    vT.Free;
  end;
end;

{ Tokens }

function ContarIdent(ALista: TListaTokens; const ANome: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to ALista.Count - 1 do
    if (ALista[i].Tipo = ttIdentificador) and SameText(ALista.Texto(i), ANome) then
      Inc(Result);
end;

function Reconstruir(ALista: TListaTokens; AInicio: integer): boolean;
var
  i, vPos: integer;
begin
  vPos := AInicio;
  Result := True;
  for i := 0 to ALista.Count - 1 do
  begin
    if ALista[i].Inicio <> vPos then
      Exit(False);
    Inc(vPos, ALista[i].Tamanho);
  end;
  Result := vPos = Length(ALista.Fonte) + 1;
end;

procedure TesteTokens;
var
  vL: TListaTokens;
  vSrc: string;
  i: integer;
begin
  WriteLn('Tokens');
  vSrc := 'var X: TRALServer; // TRALServer'#10 +
    '{ TRALServer } (* TRALServer *) ''TRALServer'''#10 +
    '{$IFDEF TRALServer} (*$IFDEF TRALServer*)'#10 +
    'Y := &begin; S := ''a''''b''#13#10''c''^M; N := $FF + 1.5e3; R := 1..2;'#10 +
    'Z := TRALServer.Create;';
  vL := TokenizarPascal(vSrc);
  try
    Conferir(Reconstruir(vL, 1), 'a soma dos tokens e o texto inteiro');
    Conferir(ContarIdent(vL, 'TRALServer') = 2, 'TRALServer so no codigo (2 vezes, fora de comentario/string/diretiva)');
    i := vL.AcharIdentificador('tralserver');
    Conferir((i >= 0) and (vL[i].Inicio = 8), 'achar identificador sem diferenciar caixa');
    Conferir(vL.AcharIdentificador('TRALServer', i + 1) > i, 'achar o seguinte');
    Conferir(vL.AcharIdentificador('&begin') >= 0, '&begin e identificador');
    Conferir(vL.AcharIdentificador('Z') >= 0, 'identificador no fim');
    Conferir(vL[vL.Proximo(-1)].Tipo = ttPalavraChave, 'var e palavra-chave');
    for i := 0 to vL.Count - 1 do
      if vL[i].Tipo = ttDiretiva then
        Break;
    Conferir((i < vL.Count) and (vL.NomeDiretiva(i) = 'IFDEF') and
      (vL.ArgumentoDiretiva(i) = 'TRALServer'), 'diretiva {$IFDEF X}: nome e argumento');
    for i := i + 1 to vL.Count - 1 do
      if vL[i].Tipo = ttDiretiva then
        Break;
    Conferir((i < vL.Count) and (vL.NomeDiretiva(i) = 'IFDEF') and
      (vL.ArgumentoDiretiva(i) = 'TRALServer'), 'diretiva (*$IFDEF X*)');
  finally
    vL.Free;
  end;
  vL := TokenizarPascal('a := ''a''''b''#13#10''c''^M; x := ''x');
  try
    i := vL.AcharIdentificador('a');
    Conferir(vL[vL.Proximo(vL.Proximo(i))].Tipo = ttString, 'string com '''', #13#10 e ^M e um token so');
    Conferir(Reconstruir(vL, 1), 'string sem fecho nao levanta e cobre tudo');
  finally
    vL.Free;
  end;
  vL := TokenizarPascal('x := (* sem fim');
  try
    Conferir(Reconstruir(vL, 1) and (vL[vL.Count - 1].Tipo = ttComentario), 'comentario sem fim');
  finally
    vL.Free;
  end;
end;

{ Uses }

type
  TOpUses = (opAcrescentar, opTirar, opTrocar, opVarias);

procedure CasoUses(const ADescricao, AOrigem, AEsperado: string; AOp: TOpUses;
  const A1: string; const A2: string = ''; ASecao: TSecaoUses = suInterface;
  const AArquivo: string = '');
var
  vT: TTextoOriginal;
  vE: TEditorUses;
  vLista: TStringList;
begin
  vT := NovoTexto(AOrigem);
  try
    vE := TEditorUses.Create(vT);
    try
      case AOp of
        opAcrescentar: vE.Acrescentar(A1, ASecao, AArquivo);
        opTirar: vE.Tirar(A1);
        opTrocar: vE.Trocar(A1, A2);
        opVarias:
          begin
            vLista := TStringList.Create;
            try
              vLista.CommaText := A1;
              vE.TirarVarias(vLista);
            finally
              vLista.Free;
            end;
          end;
      end;
    finally
      vE.Free;
    end;
    ConferirTexto(vT.Resultado, AEsperado, ADescricao);
  finally
    vT.Free;
  end;
end;

procedure TesteUses;
var
  vT: TTextoOriginal;
  vE: TEditorUses;
  vUnit, vProg, vUnitPorLinha: string;
begin
  WriteLn('Uses');
  vUnitPorLinha := L(['unit U;', '', 'interface', '', 'uses', '  Classes,', '  SysUtils;',
    '', 'implementation', '', 'uses', '  Math;', '', 'end.']);
  CasoUses('acrescentar, um por linha',
    vUnitPorLinha,
    L(['unit U;', '', 'interface', '', 'uses', '  Classes,', '  SysUtils,', '  RALServer;',
    '', 'implementation', '', 'uses', '  Math;', '', 'end.']),
    opAcrescentar, 'RALServer');
  CasoUses('acrescentar na implementation',
    vUnitPorLinha,
    L(['unit U;', '', 'interface', '', 'uses', '  Classes,', '  SysUtils;',
    '', 'implementation', '', 'uses', '  Math,', '  RALServer;', '', 'end.']),
    opAcrescentar, 'RALServer', '', suImplementation);
  CasoUses('acrescentar ja existente nao muda',
    vUnitPorLinha, vUnitPorLinha, opAcrescentar, 'sysutils');
  CasoUses('acrescentar na implementation o que ja esta na interface nao muda',
    vUnitPorLinha, vUnitPorLinha, opAcrescentar, 'Classes', '', suImplementation);
  CasoUses('acrescentar, um por linha, com CRLF',
    CrLf(vUnitPorLinha),
    CrLf(L(['unit U;', '', 'interface', '', 'uses', '  Classes,', '  SysUtils,', '  RALServer;',
    '', 'implementation', '', 'uses', '  Math;', '', 'end.'])),
    opAcrescentar, 'RALServer');
  CasoUses('acrescentar em linha',
    L(['unit U;', 'interface', 'uses Classes, SysUtils;', 'implementation', 'end.']),
    L(['unit U;', 'interface', 'uses Classes, SysUtils, RALServer;', 'implementation', 'end.']),
    opAcrescentar, 'RALServer');
  CasoUses('acrescentar em linha quebra nas 80 colunas',
    L(['unit U;', 'interface', 'uses Classes, SysUtils, Variants, Graphics, Controls, Forms, Dialogs;',
    'implementation', 'end.']),
    L(['unit U;', 'interface', 'uses Classes, SysUtils, Variants, Graphics, Controls, Forms, Dialogs,',
    '  RALServerHTTP;', 'implementation', 'end.']),
    opAcrescentar, 'RALServerHTTP');
  CasoUses('acrescentar com in ''arquivo'' em pacote (contains)',
    L(['package P;', '', 'requires', '  rtl,', '  vcl;', '', 'contains', '  U1 in ''U1.pas'';', '', 'end.']),
    L(['package P;', '', 'requires', '  rtl,', '  vcl;', '', 'contains', '  U1 in ''U1.pas'',',
    '  X in ''X.pas'';', '', 'end.']),
    opAcrescentar, 'X', '', suContains, 'X.pas');
  CasoUses('tirar o ultimo de um pacote (requires)',
    L(['package P;', '', 'requires', '  rtl,', '  vcl;', '', 'end.']),
    L(['package P;', '', 'requires', '  rtl;', '', 'end.']),
    opTirar, 'vcl');
  vProg := L(['program P;', '', 'uses', '  Forms,', '  {$IFDEF FPC}Interfaces,{$ENDIF}',
    '  Unit1 in ''Unit1.pas'' {Form1};', '', 'begin', 'end.']);
  CasoUses('trocar preserva in e comentario',
    vProg,
    L(['program P;', '', 'uses', '  Forms,', '  {$IFDEF FPC}Interfaces,{$ENDIF}',
    '  Unit2 in ''Unit1.pas'' {Form1};', '', 'begin', 'end.']),
    opTrocar, 'Unit1', 'Unit2');
  CasoUses('tirar o primeiro (linha inteira some)',
    vProg,
    L(['program P;', '', 'uses', '  {$IFDEF FPC}Interfaces,{$ENDIF}',
    '  Unit1 in ''Unit1.pas'' {Form1};', '', 'begin', 'end.']),
    opTirar, 'Forms');
  CasoUses('tirar item dentro de IFDEF',
    vProg,
    L(['program P;', '', 'uses', '  Forms,', '  {$IFDEF FPC}{$ENDIF}',
    '  Unit1 in ''Unit1.pas'' {Form1};', '', 'begin', 'end.']),
    opTirar, 'Interfaces');
  CasoUses('acrescentar no programa: depois do comentario do ultimo, sem tocar no IFDEF',
    vProg,
    L(['program P;', '', 'uses', '  Forms,', '  {$IFDEF FPC}Interfaces,{$ENDIF}',
    '  Unit1 in ''Unit1.pas'' {Form1},', '  RALServer;', '', 'begin', 'end.']),
    opAcrescentar, 'RALServer', '', suPrograma);
  CasoUses('tirar o ultimo com virgula dentro de IFDEF antes dele',
    vProg,
    L(['program P;', '', 'uses', '  Forms,', '  {$IFDEF FPC}Interfaces{$ENDIF}',
    '   {Form1};', '', 'begin', 'end.']),
    opTirar, 'Unit1');
  CasoUses('tirar o ultimo, um por linha',
    L(['uses', '  A,', '  B;']), L(['uses', '  A;']), opTirar, 'B');
  CasoUses('tirar o primeiro em linha', 'uses A, B, C;', 'uses B, C;', opTirar, 'A');
  CasoUses('tirar o do meio em linha', 'uses A, B, C;', 'uses A, C;', opTirar, 'B');
  CasoUses('tirar o ultimo em linha', 'uses A, B, C;', 'uses A, B;', opTirar, 'C');
  CasoUses('tirar vizinhos de uma vez (meio e fim)', 'uses A, B, C, D;', 'uses A, C;', opVarias, 'B,D');
  CasoUses('tirar vizinhos de uma vez (juntos no meio)', 'uses A, B, C, D;', 'uses A, D;', opVarias, 'B,C');
  CasoUses('tirar vizinhos de uma vez (juntos no fim)', 'uses A, B, C, D;', 'uses A, B;', opVarias, 'C,D');
  CasoUses('tirar o unico item some com a clausula',
    L(['interface', '', 'uses', '  A;', '', 'type']), L(['interface', '', '', 'type']), opTirar, 'A');
  CasoUses('trocar para um nome que ja existe tira o velho', 'uses A, B;', 'uses B;', opTrocar, 'A', 'B');
  CasoUses('criar uses na interface',
    L(['unit U;', '', 'interface', '', 'implementation', '', 'end.']),
    L(['unit U;', '', 'interface', '', 'uses', '  Foo;', '', 'implementation', '', 'end.']),
    opAcrescentar, 'Foo');
  CasoUses('criar uses na implementation',
    L(['unit U;', '', 'interface', '', 'implementation', '', 'end.']),
    L(['unit U;', '', 'interface', '', 'implementation', '', 'uses', '  Foo;', '', 'end.']),
    opAcrescentar, 'Foo', '', suImplementation);
  CasoUses('criar uses no programa',
    L(['program P;', '', 'begin', 'end.']),
    L(['program P;', '', 'uses', '  Foo;', '', 'begin', 'end.']),
    opAcrescentar, 'Foo', '', suPrograma);
  vUnit := L(['unit U;', 'interface', 'uses', '  {$IFDEF FPC}LCL,{$ENDIF}', '  System.SysUtils;', 'implementation', 'end.']);
  vT := NovoTexto(vUnit);
  try
    vE := TEditorUses.Create(vT);
    try
      Conferir(vE.Tipo = tuUnit, 'tipo unit');
      Conferir(vE.Contem('system.sysutils'), 'nome com ponto, sem diferenciar caixa');
      Conferir(vE.Achar('LCL').DentroIfDef and (vE.Achar('LCL').Condicao = 'IFDEF FPC'),
        'item dentro de IFDEF sabe a condicao');
      Conferir(not vE.Achar('System.SysUtils').DentroIfDef, 'item fora de IFDEF');
      Conferir((vE.Achar('System.SysUtils').Linha = 5), 'linha do item');
    finally
      vE.Free;
    end;
  finally
    vT.Free;
  end;
end;

{ Formulario }

const
  CFormulario: array[0..17] of string = (
    'object Form1: TForm1',
    '  Left = 0',
    '  Caption = ''a (b''',
    '  Items.Strings = (',
    '    ''x)''',
    '    ''y'')',
    '  Columns = <',
    '    item',
    '      Width = 5',
    '    end>',
    '  Data = {',
    '    0102}',
    '  object Btn: TButton',
    '    Left = 1',
    '  end',
    '  object Edit1: TEdit',
    '    Text = ''x''',
    '  end');

function TextoForm(AFim: boolean): string;
var
  i: integer;
begin
  Result := '';
  for i := Low(CFormulario) to High(CFormulario) do
    Result := Result + CFormulario[i] + #10;
  if AFim then
    Result := Result + 'end'#10;
end;

procedure CasoForm(const ADescricao, AOrigem, AEsperado: string; AOp: integer);
var
  vT: TTextoOriginal;
  vF: TFormulario;
begin
  vT := NovoTexto(AOrigem, 'x.dfm');
  try
    vF := TFormulario.Create(vT);
    try
      case AOp of
        1: vF.TirarPropriedade(vF.Raiz.Propriedade('Items.Strings'));
        2: vF.TirarPropriedade(vF.Raiz.Propriedade('Columns'));
        3: vF.InserirObjeto(vF.Raiz, vF.AcharObjeto('Btn'), 'object New: TLabel'#10'  Caption = ''z'''#10'end');
        4: vF.InserirObjeto(vF.Raiz, nil, 'object New: TLabel'#10'end');
        5: vF.TrocarValor(vF.Raiz.Propriedade('Caption'), ValorStringForm('n'#$C3#$A3'o'));
        6: vF.TrocarClasse(vF.AcharObjeto('Edit1'), 'TRALEdit');
        7: vF.RenomearPropriedade(vF.Raiz.Propriedade('Left'), 'Top');
        8: vF.TirarPropriedade(vF.Raiz.Propriedade('Data'));
        9: vF.InserirPropriedade(vF.AcharObjeto('Btn'), 'Caption = ''ok''');
      end;
    finally
      vF.Free;
    end;
    ConferirTexto(vT.Resultado, AEsperado, ADescricao);
  finally
    vT.Free;
  end;
end;

function SemLinhas(const AOrigem: string; AIni, AFim: integer): string;
var
  vLinhas: TStringList;
  i: integer;
begin
  vLinhas := TStringList.Create;
  try
    vLinhas.Text := AOrigem;
    for i := AFim downto AIni do
      vLinhas.Delete(i - 1);
    Result := '';
    for i := 0 to vLinhas.Count - 1 do
      Result := Result + vLinhas[i] + #10;
  finally
    vLinhas.Free;
  end;
end;

procedure TesteFormulario;
var
  vT: TTextoOriginal;
  vF: TFormulario;
  vOrig, vTexto: string;
  vFalhou: boolean;
  vEnt, vSai: TMemoryStream;
  vBytes: AnsiString;
  vLista: TList;
begin
  WriteLn('Formulario');
  vOrig := TextoForm(True);
  vT := NovoTexto(vOrig, 'x.dfm');
  try
    vF := TFormulario.Create(vT);
    try
      Conferir((vF.RaizCount = 1) and (vF.Raiz.Nome = 'Form1') and (vF.Raiz.Classe = 'TForm1'),
        'raiz Form1: TForm1');
      Conferir(vF.Raiz.FilhoCount = 2, 'dois filhos');
      Conferir((vF.Raiz.LinhaInicio = 1) and (vF.Raiz.LinhaFim = 19), 'linhas da raiz');
      Conferir(vF.Raiz.Propriedade('items.strings').EhLista and
        (vF.Raiz.Propriedade('Items.Strings').LinhaFim = 6), 'lista ( ) com parenteses dentro de string');
      Conferir(vF.Raiz.Propriedade('Columns').EhColecao and
        (vF.Raiz.Propriedade('Columns').LinhaFim = 10), 'colecao < >');
      Conferir(vF.Raiz.Propriedade('Data').EhBinario, 'binario { }');
      Conferir(vF.AcharObjeto('btn').Pai = vF.Raiz, 'achar objeto sem caixa, com pai');
      vLista := vF.ObjetosDaClasse('tedit');
      try
        Conferir((vLista.Count = 1) and (TNoFormulario(vLista[0]).Nome = 'Edit1'), 'objetos da classe');
      finally
        vLista.Free;
      end;
      Conferir(ValorComoTexto(vF.Raiz.Propriedade('Caption').ValorBruto, vTexto) and (vTexto = 'a (b'),
        'valor de string decodificado');
    finally
      vF.Free;
    end;
  finally
    vT.Free;
  end;
  CasoForm('tirar propriedade multi-linha ( )', vOrig, SemLinhas(vOrig, 4, 6), 1);
  CasoForm('tirar propriedade multi-linha < >', vOrig, SemLinhas(vOrig, 7, 10), 2);
  CasoForm('tirar propriedade { }', vOrig, SemLinhas(vOrig, 11, 12), 8);
  CasoForm('inserir objeto irmao, indentacao do irmao (LF)', vOrig,
    StringReplace(vOrig, '  end'#10'  object Edit1', '  end'#10'  object New: TLabel'#10 +
    '    Caption = ''z'''#10'  end'#10'  object Edit1', []), 3);
  CasoForm('inserir objeto irmao (CRLF)', CrLf(vOrig),
    CrLf(StringReplace(vOrig, '  end'#10'  object Edit1', '  end'#10'  object New: TLabel'#10 +
    '    Caption = ''z'''#10'  end'#10'  object Edit1', [])), 3);
  CasoForm('inserir objeto como ultimo filho', vOrig,
    Copy(vOrig, 1, Length(vOrig) - 4) + '  object New: TLabel'#10'  end'#10'end'#10, 4);
  CasoForm('trocar valor com #nnn', vOrig,
    StringReplace(vOrig, '''a (b''', '''n''#227''o''', []), 5);
  CasoForm('trocar classe', vOrig, StringReplace(vOrig, 'TEdit', 'TRALEdit', []), 6);
  CasoForm('renomear propriedade', vOrig, StringReplace(vOrig, '  Left = 0', '  Top = 0', []), 7);
  CasoForm('inserir propriedade', vOrig,
    StringReplace(vOrig, '    Left = 1'#10, '    Left = 1'#10'    Caption = ''ok'''#10, []), 9);
  ConferirTexto(ValorStringForm('abc'), '''abc''', 'ValorStringForm simples');
  ConferirTexto(ValorStringForm(''), '''''', 'ValorStringForm vazio');
  ConferirTexto(ValorStringForm('it''s'), '''it''''s''', 'ValorStringForm com apostrofo');
  ConferirTexto(ValorStringForm('a'#13#10'b'), '''a''#13#10''b''', 'ValorStringForm com CRLF');
  ConferirTexto(ValorStringForm('a'#$C3#$A7'o'), '''a''#231''o''', 'ValorStringForm acentuado');
  ConferirTexto(ValorStringForm('a'#$C3#$A7'o', True), '''a'#$C3#$A7'o''', 'ValorStringForm UTF-8 direto (.lfm)');
  ConferirTexto(ValorStringForm(#$F0#$9F#$98#$80), '#55357#56832', 'ValorStringForm par substituto');
  vFalhou := False;
  vT := NovoTexto(L(['object A: TA', '  X = 1', '  object B: TB', '  end']), 'x.dfm');
  try
    try
      vF := TFormulario.Create(vT);
      vF.Free;
    except
      on E: EFormularioInvalido do
        vFalhou := E.Linha = 1;
    end;
  finally
    vT.Free;
  end;
  Conferir(vFalhou, 'objeto sem end: erro claro com a linha do objeto');
  // binary form: text -> binary -> text
  vEnt := TMemoryStream.Create;
  vSai := TMemoryStream.Create;
  try
    vBytes := vOrig;
    vBytes := StringReplace(vBytes, '  Data = {'#10'    0102}'#10, '', []);
    vEnt.WriteBuffer(vBytes[1], Length(vBytes));
    vEnt.Position := 0;
    ObjectTextToBinary(vEnt, vSai);
    SetLength(vBytes, vSai.Size);
    Move(vSai.Memory^, vBytes[1], vSai.Size);
  finally
    vSai.Free;
    vEnt.Free;
  end;
  Conferir(EhFormularioBinario(vBytes), 'TPF0 e formulario binario');
  vT := NovoTexto(vBytes, 'x.dfm');
  try
    Conferir(ConverterBinarioParaTexto(vT), 'converter binario para texto');
    vF := TFormulario.Create(vT);
    try
      Conferir((vF.AcharObjeto('Btn') <> nil) and (vF.AcharObjeto('Edit1') <> nil),
        'o texto convertido analisa e traz os objetos');
      Conferir(vT.Alterado, 'a conversao conta como alteracao em relacao ao disco');
    finally
      vF.Free;
    end;
  finally
    vT.Free;
  end;

end;

{ ProjetoXML }

procedure TesteXml;
var
  vT: TTextoOriginal;
  vX: TProjetoXML;
  vLista: TList;
  vDproj, vLpi: string;
begin
  WriteLn('ProjetoXML');
  vDproj := L(['<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">',
    '    <PropertyGroup Condition="''$(Base)''!=''''">',
    '        <DCC_UsePackage>a;b</DCC_UsePackage>',
    '    </PropertyGroup>',
    '    <PropertyGroup Condition="''$(Cfg_1)''!=''''">',
    '        <DCC_UsePackage>a;b;$(DCC_UsePackage)</DCC_UsePackage>',
    '    </PropertyGroup>',
    '</Project>']);
  vT := NovoTexto(vDproj, 'x.dproj');
  try
    vX := TProjetoXML.Create(vT);
    try
      vLista := vX.Buscar('Project/PropertyGroup/DCC_UsePackage');
      try
        Conferir(vLista.Count = 2, 'todos os DCC_UsePackage (2)');
        Conferir(vX.ValorAtributo(TElementoXML(vLista[1]).Pai, 'Condition') = '''$(Cfg_1)''!=''''',
          'Condition do PropertyGroup do segundo');
        vX.TrocarTexto(TElementoXML(vLista[0]), 'a;b;RALCore');
        vX.TrocarTexto(TElementoXML(vLista[1]), vX.ValorTexto(TElementoXML(vLista[1])) + ';x<y');
      finally
        vLista.Free;
      end;
    finally
      vX.Free;
    end;
    ConferirTexto(vT.Resultado, StringReplace(StringReplace(vDproj, 'a;b</', 'a;b;RALCore</', []),
      '$(DCC_UsePackage)</', '$(DCC_UsePackage);x&lt;y</', []), 'trocar DCC_UsePackage (so a linha e escapa)');
  finally
    vT.Free;
  end;
  vLpi := L(['<?xml version="1.0" encoding="UTF-8"?>',
    '<CONFIG>',
    '  <ProjectOptions>',
    '    <RequiredPackages Count="2">',
    '      <Item1>',
    '        <PackageName Value="LCL"/>',
    '      </Item1>',
    '      <Item2>',
    '        <PackageName Value="FCL"/>',
    '      </Item2>',
    '    </RequiredPackages>',
    '  </ProjectOptions>',
    '  <CompilerOptions>',
    '    <SearchPaths>',
    '      <OtherUnitFiles Value="..\src;&amp;x"/>',
    '    </SearchPaths>',
    '  </CompilerOptions>',
    '</CONFIG>']);
  vT := NovoTexto(vLpi, 'x.lpi');
  try
    vX := TProjetoXML.Create(vT);
    try
      Conferir(vX.AcrescentarPacote(vX.ContainerDePacotes, 'RALServer'), 'acrescentar pacote');
      Conferir(not vX.AcrescentarPacote(vX.ContainerDePacotes, 'lcl'), 'pacote que ja existe nao entra');
      vX.TrocarAtributo(vX.Primeiro('//CompilerOptions/SearchPaths/OtherUnitFiles'), 'Value', '..\src;&y');
      Conferir(vX.ValorAtributo(vX.Primeiro('CONFIG/CompilerOptions/SearchPaths/OtherUnitFiles'), 'Value') =
        '..\src;&x', 'ler atributo com entidade');
    finally
      vX.Free;
    end;
    ConferirTexto(vT.Resultado, L(['<?xml version="1.0" encoding="UTF-8"?>',
      '<CONFIG>',
      '  <ProjectOptions>',
      '    <RequiredPackages Count="3">',
      '      <Item1>',
      '        <PackageName Value="LCL"/>',
      '      </Item1>',
      '      <Item2>',
      '        <PackageName Value="FCL"/>',
      '      </Item2>',
      '      <Item3>',
      '        <PackageName Value="RALServer"/>',
      '      </Item3>',
      '    </RequiredPackages>',
      '  </ProjectOptions>',
      '  <CompilerOptions>',
      '    <SearchPaths>',
      '      <OtherUnitFiles Value="..\src;&amp;y"/>',
      '    </SearchPaths>',
      '  </CompilerOptions>',
      '</CONFIG>']), 'RequiredPackages com Count e OtherUnitFiles trocado');
  finally
    vT.Free;
  end;
end;

{ samples }

type
  TContagem = class
  public
    Total: integer;
    Falhas: integer;
    Binarios: integer;
    Quebrados: integer;
  end;

var
  GContagens: TStringList;
  GAchados: TStringList;
  GTemp: string;
  GSeq: integer = 0;

function Contagem(const AExt: string): TContagem;
var
  i: integer;
begin
  i := GContagens.IndexOf(AExt);
  if i < 0 then
    i := GContagens.AddObject(AExt, TContagem.Create);
  Result := TContagem(GContagens.Objects[i]);
end;

function SoAscii(const ATexto: string): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 1 to Length(ATexto) do
    if Ord(ATexto[i]) >= 128 then
      Exit(False);
end;

function LinhasContiguas(AT: TTextoOriginal): boolean;
var
  i: integer;
begin
  Result := True;
  if AT.LinhaCount = 0 then
    Exit(Length(AT.Bytes) < AT.InicioConteudo);
  if AT.LinhaInicio(1) <> AT.InicioConteudo then
    Exit(False);
  for i := 1 to AT.LinhaCount - 1 do
    if AT.LinhaProxima(i) <> AT.LinhaInicio(i + 1) then
      Exit(False);
  Result := AT.LinhaProxima(AT.LinhaCount) = Length(AT.Bytes) + 1;
end;

// identity edits on tokens, uses and items
function AnalisarCodigo(AT: TTextoOriginal; var AMsg: string): boolean;
var
  vL: TListaTokens;
  vE: TEditorUses;
  i, j, vFeitas: integer;
begin
  Result := False;
  vL := TokenizarPascal(AT.Bytes, AT.InicioConteudo);
  try
    if not Reconstruir(vL, AT.InicioConteudo) then
    begin
      AMsg := 'tokens nao cobrem o arquivo';
      Exit;
    end;
    vFeitas := 0;
    for i := 0 to vL.Count - 1 do
      if (vL[i].Tipo = ttIdentificador) and SoAscii(vL.Texto(i)) and (vFeitas < 300) then
      begin
        AT.AdicionarEdicao(vL[i].Inicio, vL[i].Tamanho, vL.Texto(i));
        Inc(vFeitas);
      end;
  finally
    vL.Free;
  end;
  vE := TEditorUses.Create(AT);
  try
    for i := 0 to vE.Count - 1 do
      for j := 0 to vE.Clausulas[i].Count - 1 do
        if vE.Clausulas[i].Item(j).Nome = '' then
        begin
          AMsg := 'item de uses sem nome';
          Exit;
        end;
  finally
    vE.Free;
  end;
  Result := True;
end;

function ContarObjetos(ANo: TNoFormulario): integer;
var
  i: integer;
begin
  Result := 1;
  for i := 0 to ANo.FilhoCount - 1 do
    Inc(Result, ContarObjetos(ANo.Filhos[i]));
end;

procedure EditarPropriedades(AF: TFormulario; ANo: TNoFormulario);
var
  i: integer;
begin
  for i := 0 to ANo.PropriedadeCount - 1 do
    if SoAscii(ANo.Propriedades[i].ValorBruto) then
      AF.TrocarValor(ANo.Propriedades[i], ANo.Propriedades[i].ValorBruto);
  for i := 0 to ANo.FilhoCount - 1 do
    EditarPropriedades(AF, ANo.Filhos[i]);
end;

function AnalisarFormulario(AT: TTextoOriginal; const AArq: string; AC: TContagem;
  var AMsg: string): boolean;
var
  vF: TFormulario;
  i: integer;
begin
  Result := True;
  if EhFormularioBinario(AT.Bytes) then
  begin
    Inc(AC.Binarios);
    Exit;
  end;
  try
    vF := TFormulario.Create(AT);
  except
    on E: EFormularioInvalido do
    begin
      Inc(AC.Quebrados);
      GAchados.Add(AArq + ': ' + E.Message);
      Exit;
    end;
  end;
  try
    if vF.RaizCount = 0 then
    begin
      AMsg := 'formulario sem objeto';
      Exit(False);
    end;
    for i := 0 to vF.RaizCount - 1 do
      EditarPropriedades(vF, vF.Raizes[i]);
  finally
    vF.Free;
  end;
end;

procedure EditarXmlSemMudar(AX: TProjetoXML; AElem: TElementoXML; AT: TTextoOriginal);
var
  i: integer;
begin
  for i := 0 to AElem.AtributoCount - 1 do
    if SoAscii(AElem.Atributos[i].ValorBruto) then
      AT.AdicionarEdicao(AElem.Atributos[i].ValorInicio, AElem.Atributos[i].ValorTamanho,
        AElem.Atributos[i].ValorBruto);
  for i := 0 to AElem.FilhoCount - 1 do
    EditarXmlSemMudar(AX, AElem.Filhos[i], AT);
end;

// one real edit on a copy: exactly one line may differ
function EdicaoPontual(const AArq, AExt: string; var AMsg: string): boolean;
var
  vT: TTextoOriginal;
  vX: TProjetoXML;
  vEl: TElementoXML;
  vOrig, vNova: TStringList;
  i, vDif: integer;
begin
  Result := True;
  vT := TTextoOriginal.Create;
  vOrig := TStringList.Create;
  vNova := TStringList.Create;
  try
    vT.CarregarArquivo(AArq);
    vX := TProjetoXML.Create(vT);
    try
      if AExt = '.dproj' then
      begin
        vEl := vX.Primeiro('Project/PropertyGroup/DCC_UsePackage');
        if vEl = nil then
          Exit;
        vX.TrocarTexto(vEl, vX.ValorTexto(vEl) + ';RALTeste');
      end
      else
      begin
        vEl := vX.Primeiro('//OtherUnitFiles');
        if vEl = nil then
          Exit;
        vX.TrocarAtributo(vEl, 'Value', vX.ValorAtributo(vEl, 'Value') + ';ral_teste');
      end;
    finally
      vX.Free;
    end;
    vOrig.Text := string(vT.Bytes);
    vNova.Text := string(vT.Resultado);
    vDif := 0;
    if vOrig.Count <> vNova.Count then
      vDif := 99
    else
      for i := 0 to vOrig.Count - 1 do
        if vOrig[i] <> vNova[i] then
          Inc(vDif);
    Result := vDif = 1;
    if not Result then
      AMsg := Format('edicao pontual mudou %d linhas', [vDif]);
  finally
    vNova.Free;
    vOrig.Free;
    vT.Free;
  end;
end;

function ProcessarArquivo(const AArq: string): boolean;
var
  vExt, vMsg, vSaida: string;
  vT: TTextoOriginal;
  vX: TProjetoXML;
  vC: TContagem;
  vOriginal: AnsiString;
begin
  vExt := LowerCase(ExtractFileExt(AArq));
  vC := Contagem(vExt);
  Inc(vC.Total);
  Result := False;
  vMsg := '';
  vT := TTextoOriginal.Create;
  try
    try
      vT.CarregarArquivo(AArq);
      if not LinhasContiguas(vT) then
        vMsg := 'linhas nao cobrem o arquivo'
      else if (vExt = '.pas') or (vExt = '.dpr') or (vExt = '.lpr') or (vExt = '.dpk') then
      begin
        if not AnalisarCodigo(vT, vMsg) then
          Exit;
      end
      else if (vExt = '.dfm') or (vExt = '.lfm') or (vExt = '.fmx') then
      begin
        if not AnalisarFormulario(vT, AArq, vC, vMsg) then
          Exit;
      end
      else
      begin
        vX := TProjetoXML.Create(vT);
        try
          EditarXmlSemMudar(vX, vX.Raiz, vT);
        finally
          vX.Free;
        end;
        if (vExt <> '.lpk') and not EdicaoPontual(AArq, vExt, vMsg) then
          Exit;
      end;
      if vMsg = '' then
      begin
        Inc(GSeq);
        vSaida := GTemp + IntToStr(GSeq) + vExt;
        vT.Gravar(vSaida, True);
        vOriginal := LerBytes(AArq);
        if LerBytes(vSaida) <> vOriginal then
          vMsg := 'bytes gravados diferem do original'
        else if vT.Alterado then
          vMsg := 'identidade marcou o arquivo como alterado';
        DeleteFile(vSaida);
      end;
    except
      on E: Exception do
        vMsg := E.ClassName + ': ' + E.Message;
    end;
  finally
    vT.Free;
  end;
  Result := vMsg = '';
  if not Result then
  begin
    Inc(vC.Falhas);
    GAchados.Add('FALHA ' + AArq + ': ' + vMsg);
  end;
end;

procedure VarrerPasta(const APasta: string);
var
  vBusca: TSearchRec;
  vExt: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(APasta) + '*', faAnyFile, vBusca) <> 0 then
    Exit;
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') or (vBusca.Name = '.git') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
        VarrerPasta(IncludeTrailingPathDelimiter(APasta) + vBusca.Name)
      else
      begin
        vExt := LowerCase(ExtractFileExt(vBusca.Name));
        if (vExt <> '') and (Pos(';' + vExt + ';', ';.pas;.dfm;.lfm;.fmx;.dproj;.lpi;.lpk;.dpk;.dpr;.lpr;') > 0) then
          ProcessarArquivo(IncludeTrailingPathDelimiter(APasta) + vBusca.Name);
      end;
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
end;

procedure TesteSamples;
var
  i: integer;
  vC: TContagem;
  vTotal, vFalhas: integer;
begin
  GContagens := TStringList.Create;
  GContagens.Sorted := True;
  GAchados := TStringList.Create;
  GTemp := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'ralmig-leitura' + PathDelim;
  ForceDirectories(GTemp);
  for i := 0 to GPastas.Count - 1 do
  begin
    WriteLn('Samples: ', GPastas[i]);
    if not DirectoryExists(GPastas[i]) then
      Conferir(False, 'pasta nao existe')
    else
      VarrerPasta(GPastas[i]);
  end;
  vTotal := 0;
  vFalhas := 0;
  WriteLn('ext      total  falhas  binarios  quebrados');
  for i := 0 to GContagens.Count - 1 do
  begin
    vC := TContagem(GContagens.Objects[i]);
    WriteLn(Format('%-8s %5d  %6d  %8d  %9d', [GContagens[i], vC.Total, vC.Falhas, vC.Binarios, vC.Quebrados]));
    Inc(vTotal, vC.Total);
    Inc(vFalhas, vC.Falhas);
  end;
  for i := 0 to GAchados.Count - 1 do
    WriteLn('  ', GAchados[i]);
  Conferir(vTotal > 0, Format('arquivos lidos: %d', [vTotal]));
  Conferir(vFalhas = 0, 'ida e volta sem regra: bytes identicos em todos os arquivos');
  RemoveDir(GTemp);
end;

procedure LerArgumentos;
var
  i: integer;
  vArg: string;
begin
  GPastas := TStringList.Create;
  for i := 1 to ParamCount do
  begin
    vArg := ParamStr(i);
    if vArg = '--testes' then
      GSoTestes := True
    else if Copy(vArg, 1, 10) = '--samples=' then
      GPastas.Add(Copy(vArg, 11, MaxInt));
  end;
  if (GPastas.Count = 0) then
    GSoTestes := True;
end;

begin
  LerArgumentos;
  if GSoTestes then
  begin
    TesteTexto;
    TesteTokens;
    TesteUses;
    TesteFormulario;
    TesteXml;
  end;
  if GPastas.Count > 0 then
    TesteSamples;
  WriteLn;
  if GFalhas = 0 then
    WriteLn('Tudo certo.')
  else
    WriteLn('Falhas: ', GFalhas);
  if GFalhas > 0 then
    Halt(1);
end.
