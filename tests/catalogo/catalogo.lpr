program catalogo;

{$mode ObjFPC}{$H+}

// Catalogo de pacotes sem GUI.
//   catalogo --testes                          autoteste com uma arvore sintetica
//   catalogo <raiz do RAL> [delphi|lazarus] [pacote ...]
//                                              plano de instalacao da arvore

uses
  Classes, SysUtils, RALInst.Catalogo;

type

  { TOrigemMemoria }

  // origem sem disco: prova que o catalogo so fala com a TOrigemArquivos, que
  // e o que a origem do GitHub (F8) precisa
  TOrigemMemoria = class(TOrigemArquivos)
  private
    FCaminhos: TStringList;
    FConteudos: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Gravar(const ACaminho, ATexto: string);
    function Descricao: string; override;
    procedure Listar(const APasta: string; ALista: TStrings); override;
    function Existe(const ACaminho: string): boolean; override;
    function Ler(const ACaminho: string): string; override;
    function Localizar(const ACaminho: string): string; override;
  end;

var
  GFalhas: integer = 0;

constructor TOrigemMemoria.Create;
begin
  inherited Create;
  FCaminhos := TStringList.Create;
  FConteudos := TStringList.Create;
end;

destructor TOrigemMemoria.Destroy;
begin
  FCaminhos.Free;
  FConteudos.Free;
  inherited Destroy;
end;

procedure TOrigemMemoria.Gravar(const ACaminho, ATexto: string);
begin
  FCaminhos.Add(ACaminho);
  FConteudos.Add(ATexto);
end;

function TOrigemMemoria.Descricao: string;
begin
  Result := 'memoria';
end;

procedure TOrigemMemoria.Listar(const APasta: string; ALista: TStrings);
var
  vInt: integer;
begin
  for vInt := 0 to Pred(FCaminhos.Count) do
    if Copy(FCaminhos[vInt], 1, Length(APasta) + 1) = APasta + '/' then
      ALista.Add(FCaminhos[vInt]);
end;

function TOrigemMemoria.Existe(const ACaminho: string): boolean;
begin
  Result := FCaminhos.IndexOf(ACaminho) >= 0;
end;

function TOrigemMemoria.Ler(const ACaminho: string): string;
begin
  if not Existe(ACaminho) then
    raise Exception.Create('nao existe: ' + ACaminho);
  Result := FConteudos[FCaminhos.IndexOf(ACaminho)];
end;

function TOrigemMemoria.Localizar(const ACaminho: string): string;
begin
  Result := 'mem://' + ACaminho;
end;

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

function Dpk(const ANome, ARequires, AContains: string; ARunOnly: boolean = False): string;
begin
  Result := 'package ' + ANome + ';' + LineEnding +
            '{$R *.res}' + LineEnding +
            '{$DESCRIPTION ''Pacote ' + ANome + '''}' + LineEnding;
  if ARunOnly then
    Result := Result + '{$RUNONLY}' + LineEnding;
  Result := Result + '// comentario com requires falso;' + LineEnding;
  if ARequires <> '' then
    Result := Result + 'requires' + LineEnding + '  ' + ARequires + ';' + LineEnding;
  if AContains <> '' then
    Result := Result + 'contains' + LineEnding + '  ' + AContains + ';' + LineEnding;
  Result := Result + 'end.';
end;

function Lpk(const ANome, ATipo: string; ARequires: array of string): string;
var
  vInt: integer;
begin
  Result := '<?xml version="1.0" encoding="UTF-8"?><CONFIG><Package Version="5">' +
            '<Name Value="' + ANome + '"/>';
  if ATipo <> '' then
    Result := Result + '<Type Value="' + ATipo + '"/>';
  Result := Result + '<RequiredPkgs Count="' + IntToStr(Length(ARequires)) + '">';
  for vInt := 0 to High(ARequires) do
    Result := Result + '<Item' + IntToStr(vInt + 1) + '><PackageName Value="' + ARequires[vInt] +
              '"/><MinVersion Major="1" Minor="2" Valid="True"/></Item' + IntToStr(vInt + 1) + '>';
  Result := Result + '</RequiredPkgs></Package></CONFIG>';
end;

function Posicao(ACat: TCatalogo; ATipo: TTipoPacote; const ANome: string): integer;
var
  vPacote: TPacote;
begin
  vPacote := ACat.Buscar(ATipo, ANome);
  if vPacote = nil then
    Result := -99
  else
    Result := vPacote.Ordem;
end;

procedure MontarArvore(const ARaizDisco: string; AMemoria: TOrigemMemoria);

  procedure Gravar(const ACaminho, ATexto: string);
  var
    vLista: TStringList;
    vArquivo: string;
  begin
    AMemoria.Gravar(ACaminho, ATexto);
    vArquivo := ARaizDisco + SetDirSeparators(ACaminho);
    ForceDirectories(ExtractFilePath(vArquivo));
    vLista := TStringList.Create;
    try
      vLista.Text := ATexto;
      vLista.SaveToFile(vArquivo);
    finally
      vLista.Free;
    end;
  end;

begin
  Gravar('.gitmodules', '[submodule "src/others/SUB"]' + LineEnding +
                        #9'url = https://github.com/exemplo/sub' + LineEnding +
                        #9'path = src/others/SUB' + LineEnding);
  Gravar('src/base/Base.pas', 'unit Base; interface implementation end.');
  Gravar('pkg/Delphi/Base.dpk',
         Dpk('Base', 'rtl, dbrtl', 'Base in ''..\..\src\base\Base.pas''', True));
  Gravar('pkg/Delphi/BaseDsgn.dpk', Dpk('BaseDsgn', 'designide,' + LineEnding + '  Base', ''));
  // o pacote "novo": em subpasta, com nome que ordenaria antes dos outros; uma
  // unidade mora num submodulo (nao baixado), outra simplesmente falta
  Gravar('pkg/Delphi/Engine/AANovo.dpk',
         Dpk('AANovo', 'BaseDsgn', 'Sub in ''..\..\..\src\others\SUB\Sub.pas'',' + LineEnding +
                                   '  Falta in ''..\..\..\src\engine\Falta.pas'''));
  Gravar('pkg/Delphi/Engine/AANovo.dproj',
         '<Project><PropertyGroup><DCC_UsePackage>rtl;IndyCore;IndySystem160;Base;$(DCC_UsePackage)</DCC_UsePackage></PropertyGroup>' +
         '<PropertyGroup><DCC_UsePackage>IndyCore160;BaseDsgn</DCC_UsePackage></PropertyGroup></Project>');
  Gravar('pkg/Delphi/Engine/__history/AANovo.dpk', Dpk('Fantasma', 'Base', ''));
  Gravar('pkg/Delphi/Ciclo/CicloA.dpk', Dpk('CicloA', 'CicloB', ''));
  Gravar('pkg/Delphi/Ciclo/CicloB.dpk', Dpk('CicloB', 'CicloA', ''));
  Gravar('pkg/Delphi/Quebrado.dpk', 'isto nao e um pacote');

  Gravar('pkg/Lazarus/base.lpk', Lpk('Base', '', ['FCL']));
  Gravar('pkg/Lazarus/basedsgn.lpk', Lpk('basedsgn', 'DesignTime', ['base', 'IDEIntf']));
  Gravar('pkg/Lazarus/Engine/indyx.lpk', Lpk('IndyX', 'RunAndDesignTime', ['indylaz', 'Base']));
  Gravar('pkg/Lazarus/Engine/sorun.lpk', Lpk('sorun', 'RunTimeOnly', ['Base']));
end;

procedure Verificar(AOrigem: TOrigemArquivos);
var
  vCat: TCatalogo;
  vLista: TList;
  vNomes: TStringList;
  vPacote: TPacote;
begin
  vCat := TCatalogo.Create;
  vLista := TList.Create;
  vNomes := TStringList.Create;
  try
    Conferir(vCat.Carregar(AOrigem), 'carrega a arvore de ' + AOrigem.Descricao);

    WriteLn('Delphi');
    Conferir(vCat.Buscar(tpDelphi, 'Fantasma') = nil, '__history e ignorado');
    Conferir(vCat.Buscar(tpDelphi, 'aanovo') <> nil, 'pacote novo em subpasta entra sem manifesto (nome sem caixa)');
    Conferir(Posicao(vCat, tpDelphi, 'Base') < Posicao(vCat, tpDelphi, 'BaseDsgn'), 'Base antes de BaseDsgn');
    Conferir(Posicao(vCat, tpDelphi, 'BaseDsgn') < Posicao(vCat, tpDelphi, 'AANovo'), 'AANovo depois de BaseDsgn, apesar do nome');
    Conferir((Posicao(vCat, tpDelphi, 'CicloA') = -1) and (Posicao(vCat, tpDelphi, 'CicloB') = -1), 'ciclo fica sem ordem');
    Conferir(Pos('circular', vCat.Erros.Text) > 0, 'ciclo vira erro');
    Conferir(Pos('Quebrado.dpk', vCat.Erros.Text) > 0, 'pacote ilegivel vira erro, sem derrubar o resto');
    Conferir(vCat.Submodulos.Values['src/others/SUB'] = 'https://github.com/exemplo/sub', '.gitmodules lido (url antes de path)');

    vPacote := vCat.Buscar(tpDelphi, 'Base');
    Conferir(vPacote.Uso = upRuntime, '{$RUNONLY} -> runtime');
    Conferir(not vPacote.Instalavel, 'runtime so e compilado');
    Conferir(vPacote.Descricao = 'Pacote Base', 'DESCRIPTION lido');
    Conferir(vPacote.Externos.CommaText = 'rtl,dbrtl', 'externos: rtl, dbrtl');
    Conferir(vPacote.Unidades.CommaText = 'src/base/Base.pas', 'unidade relativa a raiz, com /: ' + vPacote.Unidades.CommaText);
    Conferir(vPacote.FontesAusentes.Count = 0, 'fonte presente nao e ausente');

    vPacote := vCat.Buscar(tpDelphi, 'BaseDsgn');
    Conferir(vPacote.Requires.Count = 2, 'requires em varias linhas, comentario com "requires" ignorado');
    Conferir(vPacote.Instalavel, 'sem RUNONLY e instalavel');

    vPacote := vCat.Buscar(tpDelphi, 'AANovo');
    Conferir(vPacote.Grupo = 'Engine', 'grupo pela subpasta');
    Conferir(vPacote.ArquivoRelativo = 'pkg/Delphi/Engine/AANovo.dpk', 'arquivo relativo');
    Conferir(vPacote.Submodulos.CommaText = 'src/others/SUB', 'unidade em submodulo aponta o submodulo');
    Conferir(vPacote.SubmodulosAusentes.CommaText = 'src/others/SUB', 'submodulo nao presente na origem');
    Conferir(vPacote.FontesAusentes.CommaText = 'src/engine/Falta.pas',
             'so a unidade fora de submodulo e fonte ausente: ' + vPacote.FontesAusentes.CommaText);
    Conferir(vPacote.Implicitos.CommaText = 'IndyCore,IndySystem',
             'implicitos do .dproj sem sufixo, sem repetir e sem pacote do catalogo: ' + vPacote.Implicitos.CommaText);

    vNomes.CommaText := 'AANovo,Inexistente';
    vCat.Fechamento(tpDelphi, vNomes, vLista, vNomes);
    Conferir((vLista.Count = 3) and (TPacote(vLista[0]).Nome = 'Base') and
             (TPacote(vLista[2]).Nome = 'AANovo'), 'fechamento de AANovo traz Base e BaseDsgn, em ordem');
    Conferir(vNomes.IndexOf('Inexistente') >= 0, 'pedido desconhecido e reportado');

    WriteLn('Lazarus');
    Conferir(Posicao(vCat, tpLazarus, 'Base') < Posicao(vCat, tpLazarus, 'basedsgn'), 'Base antes de basedsgn (caixa diferente)');
    Conferir(vCat.Buscar(tpLazarus, 'Base').Uso = upRuntime, 'Type ausente -> runtime (padrao do Lazarus)');
    Conferir(vCat.Buscar(tpLazarus, 'basedsgn').Uso = upDesign, 'DesignTime -> design');
    Conferir(vCat.Buscar(tpLazarus, 'sorun').LazRuntimeOnly, 'RunTimeOnly marcado');
    Conferir(vCat.Buscar(tpLazarus, 'IndyX').Externos.CommaText = 'indylaz', 'indylaz e externo');
    Conferir(vCat.Buscar(tpLazarus, 'IndyX').VersoesMinimas.Values['indylaz'] = '1.2', 'versao minima lida');
    Conferir(vCat.Buscar(tpLazarus, 'IndyX').Grupo = 'Engine', 'grupo Lazarus');

    WriteLn;
    WriteLn(vCat.Plano(tpDelphi));
  finally
    vNomes.Free;
    vLista.Free;
    // a origem pertence ao catalogo
    vCat.Free;
  end;
end;

procedure Testes;
var
  vRaiz: string;
  vMemoria: TOrigemMemoria;
begin
  vRaiz := IncludeTrailingPathDelimiter(GetTempDir(False)) + 'ralinst_catalogo_' +
           IntToStr(GetTickCount64) + PathDelim;
  vMemoria := TOrigemMemoria.Create;
  MontarArvore(vRaiz, vMemoria);

  WriteLn('== origem em disco: ', vRaiz);
  Verificar(TOrigemLocal.Create(vRaiz));
  WriteLn('== origem em memoria');
  Verificar(vMemoria);

  if GFalhas = 0 then
    WriteLn('todos os testes passaram')
  else
    WriteLn(GFalhas, ' teste(s) falharam');
end;

var
  vCat: TCatalogo;
  vNomes: TStringList;
  vTipo: TTipoPacote;
  vInt: integer;
begin
  if (ParamCount = 0) or (ParamStr(1) = '--testes') then
  begin
    Testes;
    Halt(Ord(GFalhas > 0));
  end;

  vCat := TCatalogo.Create;
  vNomes := TStringList.Create;
  try
    if not vCat.Carregar(ParamStr(1)) then
    begin
      WriteLn('nenhum pacote em ', ParamStr(1));
      Halt(1);
    end;

    vTipo := tpDelphi;
    if (ParamCount >= 2) and SameText(ParamStr(2), 'lazarus') then
      vTipo := tpLazarus;
    for vInt := 3 to ParamCount do
      vNomes.Add(ParamStr(vInt));

    if vNomes.Count = 0 then
      WriteLn(vCat.Plano(vTipo))
    else
      WriteLn(vCat.Plano(vTipo, vNomes));
  finally
    vNomes.Free;
    vCat.Free;
  end;
end.
