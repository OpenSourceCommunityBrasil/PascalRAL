program compatibilidade;

{$mode ObjFPC}{$H+}

// F6 pela linha de comando.
//
//   compatibilidade --testes [--receitas=pasta]
//       testes sem IDE: leitura dos uses, nomes do Delphi, condicoes,
//       validacao do manifesto e das regras de versao
//   compatibilidade <raiz do RAL> [--manifesto=arq] [--receitas=pasta]
//                   [--lazarus=<pasta com Lazarus>] [--sem-deteccao]
//       para cada IDE achada: o que fica de fora e por que, e a versao de cada
//       dependencia que ela pediria
//
// Sem --manifesto e --receitas, usa manifesto/ral.json e receitas/ do
// repositorio do instalador. So le: nada e escrito em IDE nenhuma.

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, StrUtils, fpjson, jsonparser, RALInst.IDE, RALInst.IDE.Lazarus,
  RALInst.Catalogo, RALInst.Receitas, RALInst.Compatibilidade, RALInst.Instalar.Lazarus,
  RALInst.Zip
  {$IFDEF MSWINDOWS}, Windows, RALInst.IDE.Delphi, RALInst.Instalar.Delphi{$ENDIF};

type
  TDetector = class
  public
    Catalogo: TCatalogo;
    function Detectar(AIDE: TIDEInstance; AReceita: TReceita): string;
  end;

function TDetector.Detectar(AIDE: TIDEInstance; AReceita: TReceita): string;
begin
  Result := '';
  {$IFDEF MSWINDOWS}
  if AIDE.Tipo = tiDelphi then
  begin
    with TInstalacaoDelphi.Create(AIDE, Catalogo) do
    try
      Result := DependenciaInstalada(AReceita);
    finally
      Free;
    end;
    Exit;
  end;
  {$ENDIF}
  with TInstalacaoLazarus.Create(AIDE, Catalogo) do
  try
    Result := DependenciaInstalada(AReceita);
  finally
    Free;
  end;
end;

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

function Usos(const AFonte: string): string;
var
  vLista: TStringList;
begin
  vLista := TStringList.Create;
  try
    UsosIncondicionais(AFonte, vLista);
    Result := vLista.CommaText;
  finally
    vLista.Free;
  end;
end;

function UsosPara(AIDE: TIDEInstance; const AFonte: string): string;
var
  vLista, vDef, vNaoDef: TStringList;
begin
  vLista := TStringList.Create;
  vDef := TStringList.Create;
  vNaoDef := TStringList.Create;
  try
    SimbolosDelphi(AIDE, vDef, vNaoDef);
    UsosEfetivos(AFonte, vDef, vNaoDef, vLista);
    Result := vLista.CommaText;
  finally
    vNaoDef.Free;
    vDef.Free;
    vLista.Free;
  end;
end;

function IDEDe(ATipo: TTipoIDE; const AVersao, ACompilador: string): TIDEInstance;
begin
  Result := TIDEInstance.Create(ATipo);
  Result.Versao := AVersao;
  Result.VersaoCompilador := ACompilador;
  Result.Nome := 'teste ' + AVersao;
end;

function Regra(const AJSON: string; out AErro: string): TRegraVersao;
var
  vDados: TJSONData;
begin
  vDados := GetJSON(AJSON);
  try
    Result := LerRegraVersao(TJSONObject(vDados), 'teste', [], AErro);
  finally
    vDados.Free;
  end;
end;

// True se a regra e recusada; o motivo em AErro (antes de Conferir montar o
// texto, que o FPC pode avaliar antes da chamada)
function Recusada(const AJSON: string; out AErro: string): boolean;
var
  vRegra: TRegraVersao;
begin
  vRegra := Regra(AJSON, AErro);
  Result := vRegra = nil;
  vRegra.Free;
end;

// a receita do Zeos de verdade: versao por FPC e as verificacoes da copia
// instalada, com arquivos montados aqui e, se existirem, com as copias da
// maquina (so leitura)
procedure TestesZeos(const APastaReceitas: string);
var
  vReceitas: TReceitas;
  vZeos: TReceita;
  vCompat: TCompatibilidade;
  vD12, vL322, vL323, vL324, vL331: TIDEInstance;
  vMotivo, vPasta, vAviso: string;
  vArq: TStringList;

  procedure Gravar(const ARel, ATexto: string);
  begin
    ForceDirectories(ExtractFilePath(vPasta + ARel));
    vArq.Text := ATexto;
    vArq.SaveToFile(vPasta + ARel);
  end;

  procedure Copia(const ANome, ARaiz: string; AIDE: TIDEInstance; AEsperaAviso: boolean);
  var
    vTexto: string;
  begin
    if not DirectoryExists(ARaiz) then
    begin
      WriteLn('  --    ', ANome, ': não existe nesta máquina (', ARaiz, ')');
      Exit;
    end;
    vTexto := vCompat.AvisoInstalada(vZeos, AIDE, IncludeTrailingPathDelimiter(ARaiz));
    Conferir((vTexto <> '') = AEsperaAviso, Format('%s no FPC %s: %s', [ANome,
      AIDE.VersaoCompilador, IfThen(vTexto = '', 'sem aviso', 'aviso')]));
  end;

begin
  WriteLn('receita do Zeos (', APastaReceitas, '):');
  vReceitas := TReceitas.Create;
  vArq := TStringList.Create;
  vD12 := IDEDe(tiDelphi, '36.0', 'VER360');
  vL322 := IDEDe(tiLazarus, '3.2.0.0', '3.2.2');
  vL323 := IDEDe(tiLazarus, '4.6.0.0', '3.2.3');
  vL324 := IDEDe(tiLazarus, '4.0.0.2', '3.2.4');
  vL331 := IDEDe(tiLazarus, '4.99.0.0', '3.3.1');
  vCompat := TCompatibilidade.Create(nil, nil, vReceitas);
  try
    vReceitas.CarregarPasta(APastaReceitas);
    Conferir(vReceitas.Erros.Count = 0, 'receitas sem erro: ' + vReceitas.Erros.Text);
    vZeos := vReceitas.Buscar('Zeos');
    Conferir(vZeos <> nil, 'receita do Zeos carregada');
    if vZeos = nil then
      Exit;
    Conferir(CompararVersoes('3.2.4', '3.2.x') < 0, '3.2.4 <= 3.2.x');
    Conferir(CompararVersoes('3.3.1', '3.2.x') > 0, '3.3.1 > 3.2.x');
    Conferir(vCompat.VersaoDependencia(vZeos, vD12, vMotivo) = 'estavel', 'Delphi 12: estavel');
    Conferir(vCompat.VersaoDependencia(vZeos, vL322, vMotivo) = 'estavel', 'FPC 3.2.2: estavel');
    Conferir(vCompat.VersaoDependencia(vZeos, vL323, vMotivo) = 'frones/ZeosLib:master',
             'FPC 3.2.3: ' + vCompat.VersaoDependencia(vZeos, vL323, vMotivo));
    Conferir(vCompat.VersaoDependencia(vZeos, vL324, vMotivo) = 'frones/ZeosLib:master',
             'FPC 3.2.4: o master do frones');
    Conferir(vCompat.VersaoDependencia(vZeos, vL331, vMotivo) = '8.0-patches', 'FPC 3.3.1: 8.0-patches');

    // copias montadas: o define no lugar errado, e a 8.0.0 'release'
    vPasta := IncludeTrailingPathDelimiter(GetTempDir) + 'ralinst-zeos-teste' + PathDelim;
    Gravar('src' + PathDelim + 'ZeosLazarus.inc',
      '{$IF FPC_FULLVERSION>=30200}'#10'  {$DEFINE HAVE_TFORMATSETTINGS_CREATE}'#10 +
      '  {$IF FPC_FULLVERSION>=30203}'#10'  {$IFEND}'#10'{$IFEND}');
    vAviso := vCompat.AvisoInstalada(vZeos, vL322, vPasta);
    Conferir(Pos('ZCbor', vAviso) > 0, 'define no bloco do 3.2: aviso no FPC 3.2.2');
    Conferir(vCompat.AvisoInstalada(vZeos, vL331, vPasta) = '', 'e nenhum no FPC 3.3.1');
    Conferir(vCompat.AvisoInstalada(vZeos, vD12, vPasta) = '', 'nem no Delphi');
    Gravar('src' + PathDelim + 'ZeosLazarus.inc',
      '{$IF FPC_FULLVERSION>=30200}'#10'  {$IF FPC_FULLVERSION>=30203}'#10 +
      '    {$IF FPC_FULLVERSION >= 30300}'#10'    {$DEFINE HAVE_TFORMATSETTINGS_CREATE}');
    Conferir(vCompat.AvisoInstalada(vZeos, vL323, vPasta) = '', 'com a linha movida: sem aviso');
    Gravar('src' + PathDelim + 'core' + PathDelim + 'ZClasses.pas',
      'ZEOS_MAJOR_VERSION = 8;'#10'ZEOS_MINOR_VERSION = 0;'#10'ZEOS_SUB_VERSION = 0;'#10 +
      'ZEOS_STATUS = ''release'';');
    Conferir(Pos('8.0.0-stable', vCompat.AvisoInstalada(vZeos, vL323, vPasta)) > 0,
             '8.0.0 release: aviso no FPC 3.2.3');
    Conferir(vCompat.AvisoInstalada(vZeos, vL322, vPasta) = '', 'e nenhum no FPC 3.2.2');
    Conferir(vCompat.AvisoInstalada(vZeos, vL331, vPasta) = '', 'nem no FPC 3.3.1');
    Conferir(vCompat.AvisoInstalada(vZeos, vL323, '') = '', 'pasta desconhecida: sem aviso');
    SysUtils.DeleteFile(vPasta + 'src' + PathDelim + 'ZeosLazarus.inc');
    SysUtils.DeleteFile(vPasta + 'src' + PathDelim + 'core' + PathDelim + 'ZClasses.pas');

    WriteLn('as copias do Zeos desta maquina:');
    Copia('SVN 8.0-patches', 'D:\Projetos\Delphi\_Components\zeoslib svn', vL323, True);
    Copia('SVN 8.0-patches', 'D:\Projetos\Delphi\_Components\zeoslib svn', vL331, False);
    Copia('GitHub frones', 'D:\Projetos\Delphi\_Components\ZeosLib', vL323, False);
    Copia('GitHub frones', 'D:\Projetos\Delphi\_Components\ZeosLib', vL322, False);
    Copia('OPM 8.0.0-stable', 'D:\IDE\lazarus\3.0\config_lazarus\onlinepackagemanager\packages\zeosdbo',
          vL323, True);
    Copia('OPM 8.0.0-stable', 'D:\IDE\lazarus\3.0\config_lazarus\onlinepackagemanager\packages\zeosdbo',
          vL322, False);
  finally
    vCompat.Free;
    vL331.Free;
    vL324.Free;
    vL323.Free;
    vL322.Free;
    vD12.Free;
    vArq.Free;
    vReceitas.Free;
  end;
end;

procedure Testes;
var
  vXE2, vD12, vLaz322, vLaz323, vLaz331, vLazSemFPC: TIDEInstance;
  vRegra: TRegraVersao;
  vErro: string;
  vManifesto: TManifesto;
  vOk: boolean;
begin
  WriteLn('uses fora de {$IFDEF}:');
  Conferir(Usos('unit x; interface uses Classes, System.Net.HttpClient; implementation end.') =
           'Classes,System.Net.HttpClient', 'uses simples, nome com ponto');
  Conferir(Usos('uses A, {$IFDEF FPC} fphttp, {$ELSE} System.Net.X, {$ENDIF} B;') = 'A,B',
           'o que esta dentro de {$IFDEF}/{$ELSE} nao conta');
  Conferir(Usos('uses A {$IF CompilerVersion > 30}, C{$IFEND}; implementation uses D;') = 'A,D',
           '{$IF}...{$IFEND}, e os uses da implementation');
  Conferir(Usos('// uses Falso;'#10'{ uses Falso2; } (* uses Falso3; *) uses A;') = 'A',
           'comentarios dos tres tipos');
  Conferir(Usos('uses A in ''..\x\A.pas'', B;') = 'A,B', 'A in ''arquivo''');
  Conferir(Usos('{$IFDEF MSWINDOWS} uses Winapi.Windows; {$ENDIF} uses C;') = 'C',
           'uses inteiro dentro de {$IFDEF}');
  Conferir(Usos('{$I PascalRAL.inc} uses {$IFDEF X}{$IFDEF Y}Q,{$ENDIF}{$ENDIF} Z;') = 'Z',
           '{$IFDEF} aninhado');

  WriteLn('nomes do Delphi:');
  Conferir(ProdutoPorNome('XE8') >= 0, 'XE8');
  Conferir(DelphiProdutos[ProdutoPorNome('10')].BDS = '17.0', '10 = Seattle, nao 10.1');
  Conferir(DelphiProdutos[ProdutoPorNome('10.1')].BDS = '18.0', '10.1 = Berlin');
  Conferir(DelphiProdutos[ProdutoPorNome('XE')].BDS = '8.0', 'XE, nao XE2');
  Conferir(DelphiProdutos[ProdutoPorNome('Delphi 12 Athens')].BDS = '23.0', 'nome completo');
  Conferir(ProdutoPorNome('XE9') < 0, 'XE9 nao existe');

  vXE2 := IDEDe(tiDelphi, DelphiProdutos[ProdutoPorNome('XE2')].VersaoNum, 'VER230');
  vD12 := IDEDe(tiDelphi, DelphiProdutos[ProdutoPorNome('12')].VersaoNum, 'VER360');
  vLaz322 := IDEDe(tiLazarus, '3.2.0.0', '3.2.2');
  vLaz323 := IDEDe(tiLazarus, '4.6.0.0', '3.2.3');
  vLaz331 := IDEDe(tiLazarus, '4.99.0.0', '3.3.1');
  vLazSemFPC := IDEDe(tiLazarus, '4.6.0.0', '');
  try
    WriteLn('uses com os simbolos da IDE:');
    Conferir(UsosPara(vXE2, 'uses {$IFDEF DELPHIXE4UP} FireDAC.Stan.StorageBin, {$ELSE} ' +
             'uADStanStorage, {$ENDIF} DB;') = 'uADStanStorage,DB', 'XE2 pega o {$ELSE} do DELPHIXE4UP');
    Conferir(UsosPara(vD12, 'uses {$IFDEF DELPHIXE4UP} FireDAC.Stan.StorageBin, {$ELSE} ' +
             'uADStanStorage, {$ENDIF} DB;') = 'FireDAC.Stan.StorageBin,DB', 'Delphi 12 pega o {$IFDEF}');
    Conferir(UsosPara(vD12, 'uses {$IFNDEF FPC} Vcl.Forms, {$ENDIF} {$IFDEF RALlkJSON} lk, {$ENDIF} B;') =
             'Vcl.Forms,B', '{$IFNDEF FPC} conta; simbolo desconhecido nao');
    Conferir(UsosPara(vD12, '{$DEFINE MEU} uses {$IFDEF MEU} M, {$ENDIF} {$IF CompilerVersion > 30} C, {$IFEND} B;') =
             'M,B', '{$DEFINE} do proprio fonte vale; {$IF expressao} nao');
    Conferir(UsosPara(vXE2, 'uses {$IFDEF VER230} Q, {$ENDIF} {$IFDEF UNICODE} U, {$ENDIF} B;') = 'Q,U,B',
             'VER230 e UNICODE no XE2');

    WriteLn('condicoes e regras de versao:');
    vRegra := Regra('{ "delphi-min": "XE8", "versao": "x" }', vErro);
    Conferir(vRegra <> nil, 'regra delphi-min valida');
    if vRegra <> nil then
    try
      Conferir(not vRegra.Condicao.Satisfaz(vXE2, True), 'XE2 fora de XE8+');
      Conferir(vRegra.Condicao.Satisfaz(vD12, True), 'Delphi 12 dentro de XE8+');
      Conferir(not vRegra.Condicao.Relevante(vLaz323), 'regra de Delphi nao fala do Lazarus');
      Conferir(vRegra.Condicao.Satisfaz(vLaz323, True), 'e o Lazarus satisfaz (nao ha faixa dele)');
      Conferir(vRegra.Condicao.Descrever(vXE2) = 'Delphi XE8 ou mais novo',
               'descricao: ' + vRegra.Condicao.Descrever(vXE2));
    finally
      vRegra.Free;
    end;

    vRegra := Regra('{ "fpc-min": "3.2.3", "fpc-max": "3.2.3", "incompativel": "nao" }', vErro);
    Conferir(vRegra <> nil, 'regra fpc 3.2.3 valida');
    if vRegra <> nil then
    try
      Conferir(vRegra.Condicao.Satisfaz(vLaz323, False), 'FPC 3.2.3 casa');
      Conferir(not vRegra.Condicao.Satisfaz(vLaz322, False), 'FPC 3.2.2 nao casa');
      Conferir(not vRegra.Condicao.Satisfaz(vLaz331, False), 'FPC 3.3.1 nao casa');
      Conferir(not vRegra.Condicao.Satisfaz(vLazSemFPC, False), 'FPC desconhecido nao casa regra');
      Conferir(not vRegra.Condicao.Relevante(vD12), 'regra de FPC nao fala do Delphi');
      Conferir(vRegra.Condicao.Descrever(vLaz323) = 'FPC 3.2.3', 'descricao: ' +
               vRegra.Condicao.Descrever(vLaz323));
    finally
      vRegra.Free;
    end;

    vOk := Recusada('{ "delphi-min": "XE9", "versao": "x" }', vErro);
    Conferir(vOk, 'Delphi desconhecido recusado: ' + vErro);
    vOk := Recusada('{ "fpc-min": "3.3" }', vErro);
    Conferir(vOk, 'sem versao nem incompativel: ' + vErro);
    vOk := Recusada('{ "fpc-min": "3.3", "versao": "a", "incompativel": "b" }', vErro);
    Conferir(vOk, 'versao e incompativel juntos: ' + vErro);
    vOk := Recusada('{ "fpc-min": "3.3", "versao": "a", "executar": "rm -rf" }', vErro);
    Conferir(vOk, 'campo estranho: ' + vErro);
    vOk := Recusada('{ "sistemas": ["windos"], "versao": "a" }', vErro);
    Conferir(vOk, 'sistema desconhecido: ' + vErro);

    WriteLn('manifesto:');
    vManifesto := TManifesto.Create;
    try
      Conferir(vManifesto.CarregarTexto('{ "formato": 1, "pacotes": [ { "pacote": "P", ' +
        '"delphi-min": "10.4" } ], "dependencias": [ { "receita": "R", "versao": "master" } ] }',
        't1') and (vManifesto.TotalRegras = 1), 'manifesto valido');
      Conferir(not vManifesto.CarregarTexto('{ "formato": 2 }', 't2'), 'formato 2 recusado');
      Conferir(vManifesto.TotalRegras = 1, 'o recusado nao apaga o anterior');
      Conferir(not vManifesto.CarregarTexto('{ "formato": 1, "scripts": [] }', 't3'),
               'campo desconhecido na raiz recusado');
      Conferir(not vManifesto.CarregarTexto('{ "formato": 1, "pacotes": [ { "pacote": "P", ' +
        '"rodar": "x" } ] }', 't4'), 'campo desconhecido na regra recusado');
      Conferir(not vManifesto.CarregarTexto('{ "formato": 1, "dependencias": [ { "versao": "x" } ] }',
        't5'), 'dependencia sem receita recusada');
      Conferir(vManifesto.Erros.Count = 4, Format('%d erro(s) anotado(s)', [vManifesto.Erros.Count]));
    finally
      vManifesto.Free;
    end;
  finally
    vLazSemFPC.Free;
    vLaz331.Free;
    vLaz323.Free;
    vLaz322.Free;
    vD12.Free;
    vXE2.Free;
  end;
end;

var
  GRaiz, GParam, GArqManifesto, GPastaReceitas, GMotivo, GVersao, GOnde: string;
  GLazarus: TStringList;
  GCatalogo: TCatalogo;
  GManifesto: TManifesto;
  GReceitas: TReceitas;
  GCompat: TCompatibilidade;
  GDetector: TDetector;
  GIDEs: TIDEList;
  GLista: TList;
  GFora: TStringList;
  GIDE: TIDEInstance;
  GTipo: TTipoPacote;
  GBusca: TBuscaIDE;
  GSemDeteccao, GOk: boolean;
  GT0: QWord;
  vInt, vIDE, vRec: integer;
  vTexto: TStringList;
  vPasta: string;

begin
  {$IFDEF MSWINDOWS}SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  if (ParamCount >= 1) and (ParamStr(1) = '--testes') then
  begin
    Testes;
    // a receita do Zeos: --receitas=<pasta>, senao a do repositorio
    GPastaReceitas := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' + PathDelim + '..' +
                                     PathDelim + 'receitas');
    if Copy(ParamStr(2), 1, 11) = '--receitas=' then
      GPastaReceitas := Copy(ParamStr(2), 12, MaxInt);
    TestesZeos(GPastaReceitas);
    WriteLn;
    if GFalhas = 0 then
      WriteLn('todos os testes passaram')
    else
      WriteLn(GFalhas, ' falha(s)');
    Halt(Ord(GFalhas > 0));
  end;

  GArqManifesto := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' + PathDelim + '..' +
                                  PathDelim + 'manifesto' + PathDelim + 'ral.json');
  GPastaReceitas := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' + PathDelim + '..' +
                                   PathDelim + 'receitas');
  GLazarus := TStringList.Create;
  for vInt := 1 to ParamCount do
  begin
    GParam := ParamStr(vInt);
    if Copy(GParam, 1, 12) = '--manifesto=' then
      GArqManifesto := Copy(GParam, 13, MaxInt)
    else if Copy(GParam, 1, 11) = '--receitas=' then
      GPastaReceitas := Copy(GParam, 12, MaxInt)
    else if Copy(GParam, 1, 10) = '--lazarus=' then
      GLazarus.Add(Copy(GParam, 11, MaxInt))
    else if GParam = '--sem-deteccao' then
      GSemDeteccao := True
    else
      GRaiz := GParam;
  end;
  if GRaiz = '' then
  begin
    WriteLn('uso: compatibilidade --testes');
    WriteLn('     compatibilidade <raiz do RAL> [--manifesto=arq] [--receitas=pasta] ' +
            '[--lazarus=pasta] [--sem-deteccao]');
    Halt(2);
  end;

  GCatalogo := TCatalogo.Create;
  GManifesto := TManifesto.Create;
  GReceitas := TReceitas.Create;
  GIDEs := TIDEList.Create(True);
  GLista := TList.Create;
  GFora := TStringList.Create;
  GDetector := TDetector.Create;
  try
    // um .zip da versao (o cache do instalador) e lido como a tela le
    if SameText(ExtractFileExt(GRaiz), '.zip') then
      GOk := GCatalogo.Carregar(TOrigemZip.Create(GRaiz, ExtractFileName(GRaiz)))
    else
      GOk := GCatalogo.Carregar(GRaiz);
    if not GOk then
    begin
      WriteLn('nenhum pacote em ', GRaiz);
      Halt(1);
    end;
    GDetector.Catalogo := GCatalogo;

    // o do RAL, se ele tiver; senao o arquivo pedido (na GUI: o embutido)
    GManifesto.CarregarPadrao(GCatalogo.Origem);
    if (GManifesto.Origem = '') and FileExists(GArqManifesto) then
    begin
      vTexto := TStringList.Create;
      try
        vTexto.LoadFromFile(GArqManifesto);
        GManifesto.CarregarTexto(vTexto.Text, GArqManifesto);
      finally
        vTexto.Free;
      end;
    end;
    for vInt := 0 to Pred(GManifesto.Erros.Count) do
      WriteLn('ERRO: ', GManifesto.Erros[vInt]);
    WriteLn('manifesto: ', GManifesto.Origem, Format(' (%d regra(s) de pacote)', [GManifesto.TotalRegras]));
    GReceitas.CarregarPasta(GPastaReceitas);
    for vInt := 0 to Pred(GReceitas.Erros.Count) do
      WriteLn('ERRO: ', GReceitas.Erros[vInt]);

    GCompat := TCompatibilidade.Create(GCatalogo, GManifesto, GReceitas);
    if not GSemDeteccao then
      GCompat.Detectar := @GDetector.Detectar;

    {$IFDEF MSWINDOWS}
    GBusca := TBuscaDelphi.Create;
    try
      GBusca.BuscarPadrao(GIDEs);
      GBusca.Finalizar(GIDEs);
    finally
      GBusca.Free;
    end;
    {$ENDIF}
    GBusca := TBuscaLazarus.Create;
    try
      GBusca.BuscarPadrao(GIDEs);
      for vPasta in GLazarus do
        GBusca.BuscarEm(GIDEs, vPasta, 2);
      GBusca.Finalizar(GIDEs);
    finally
      GBusca.Free;
    end;

    for vIDE := 0 to Pred(GIDEs.Count) do
    begin
      GIDE := GIDEs[vIDE];
      if GIDE.Tipo = tiDelphi then
        GTipo := tpDelphi
      else
        GTipo := tpLazarus;
      WriteLn;
      WriteLn(Format('%s  (%s)  versão %s, compilador %s', [GIDE.Nome,
        ExcludeTrailingPathDelimiter(GIDE.RootDir), GIDE.Versao, GIDE.VersaoCompilador]));

      GLista.Clear;
      GFora.Clear;
      GCatalogo.Listar(GTipo, GLista);
      GT0 := GetTickCount64;
      GCompat.Filtrar(GLista, GIDE, GFora);
      WriteLn(Format('  cabem %d de %d pacote(s) (%d ms)', [GLista.Count, GLista.Count + GFora.Count,
        GetTickCount64 - GT0]));
      for vInt := 0 to Pred(GFora.Count) do
        WriteLn('  fora: ', GFora.Names[vInt], ' — ', GFora.ValueFromIndex[vInt]);

      for vRec := 0 to Pred(GReceitas.Count) do
      begin
        if not GReceitas[vRec].Bloco(GTipo).Existe then
          Continue;
        GOnde := '';
        if not GSemDeteccao then
          GOnde := GDetector.Detectar(GIDE, GReceitas[vRec]);
        GVersao := GCompat.VersaoDependencia(GReceitas[vRec], GIDE, GMotivo);
        if GOnde <> '' then
          WriteLn(Format('  %s: já instalada (%s)', [GReceitas[vRec].Nome, GOnde]))
        else if GMotivo <> '' then
          WriteLn(Format('  %s: nenhuma versão serve — %s', [GReceitas[vRec].Nome, GMotivo]))
        else if GReceitas[vRec].Pago then
          WriteLn(Format('  %s: comercial, não instalada', [GReceitas[vRec].Nome]))
        else
          WriteLn(Format('  %s: baixaria %s', [GReceitas[vRec].Nome, GVersao]));
      end;
    end;
  finally
    GCompat.Free;
    GDetector.Free;
    GFora.Free;
    GLista.Free;
    GIDEs.Free;
    GReceitas.Free;
    GManifesto.Free;
    GCatalogo.Free;
    GLazarus.Free;
  end;
end.
