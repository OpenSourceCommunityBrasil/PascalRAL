unit RALInst.Catalogo;

{$mode ObjFPC}{$H+}

// Catalogo dos pacotes de uma arvore do RAL, lido dos proprios pacotes.
//
// Nada aqui vem de manifesto: a lista sai de pkg/Delphi/**/*.dpk e
// pkg/Lazarus/**/*.lpk, a dependencia de cada um sai do requires (.dpk) e do
// RequiredPkgs (.lpk), e a ordem de instalacao e a ordenacao topologica desse
// grafo. Pacote novo colocado em pkg/ entra no catalogo, na posicao certa,
// sem ninguem editar nada.
//
// O que o pacote nao declara nao da para deduzir, e fica como dado a parte:
// o Indy do IndyRAL, por exemplo, nao esta no requires (o sufixo muda por
// versao do Delphi); o .dproj guarda um rastro dele em DCC_UsePackage, que
// entra aqui como dependencia *implicita* e e resolvido na F3.
//
// O catalogo nao le o disco diretamente, e sim uma TOrigemArquivos: o usuario
// escolhe os recursos *antes* de qualquer download, entao na GUI o catalogo
// vem do GitHub (lista da arvore + arquivos crus, F8); a arvore em disco e o
// caso de quem desenvolve o RAL e o de uma pasta ja baixada.

interface

uses
  Classes, SysUtils, Contnrs;

type
  { TOrigemArquivos }

  // caminhos sempre relativos a raiz do repositorio, separados por '/'
  TOrigemArquivos = class
  public
    // para o log e para o plano: a pasta, ou o repositorio e a referencia
    function Descricao: string; virtual; abstract;
    // todos os arquivos sob APasta, recursivamente
    procedure Listar(const APasta: string; ALista: TStrings); virtual; abstract;
    function Existe(const ACaminho: string): boolean; virtual; abstract;
    // conteudo do arquivo; excecao se nao existe
    function Ler(const ACaminho: string): string; virtual; abstract;
    // onde o arquivo esta de verdade (caminho em disco ou URL)
    function Localizar(const ACaminho: string): string; virtual; abstract;
  end;

  { TOrigemLocal }

  TOrigemLocal = class(TOrigemArquivos)
  private
    FRaiz: string;
    procedure ListarPasta(const APasta, ARelativo: string; ALista: TStrings);
  public
    constructor Create(const ARaiz: string);
    function Descricao: string; override;
    procedure Listar(const APasta: string; ALista: TStrings); override;
    function Existe(const ACaminho: string): boolean; override;
    function Ler(const ACaminho: string): string; override;
    function Localizar(const ACaminho: string): string; override;
    property Raiz: string read FRaiz;
  end;

  TTipoPacote = (tpDelphi, tpLazarus);

  // Delphi: {$RUNONLY} -> runtime, {$DESIGNONLY} -> design, nenhum -> ambos.
  // Lazarus: RunTime/RunTimeOnly -> runtime, DesignTime -> design,
  // RunAndDesignTime -> ambos. Sem <Type>, runtime: o Lazarus so grava o que
  // difere do padrao, e o padrao e RunTime (o pascalral.lpk e assim).
  TUsoPacote = (upRuntime, upDesign, upAmbos);

  { TPacote }

  TPacote = class
  private
    FTipo: TTipoPacote;
    FNome: string;
    FArquivo: string;
    FArquivoRelativo: string;
    FGrupo: string;
    FDescricao: string;
    FUso: TUsoPacote;
    FLazRuntimeOnly: boolean;
    FLibSuffix: string;
    FRequires: TStringList;
    FInternos: TStringList;
    FExternos: TStringList;
    FImplicitos: TStringList;
    FVersoesMinimas: TStringList;
    FUnidades: TStringList;
    FFontesAusentes: TStringList;
    FSubmodulos: TStringList;
    FSubmodulosAusentes: TStringList;
    FOrdem: integer;
    function GetInstalavel: boolean;
  public
    constructor Create(ATipo: TTipoPacote);
    destructor Destroy; override;

    property Tipo: TTipoPacote read FTipo;
    // nome declarado no pacote ('IndyRAL'), que e o que os outros exigem
    property Nome: string read FNome;
    // onde a origem diz que o pacote esta (disco ou URL)
    property Arquivo: string read FArquivo;
    // relativo a raiz do repositorio, com '/'; e o que vale depois do download
    property ArquivoRelativo: string read FArquivoRelativo;
    // subpasta de pkg/<IDE>: '', 'Engine', 'Database', 'compression/zstd'
    property Grupo: string read FGrupo;
    property Descricao: string read FDescricao;
    property Uso: TUsoPacote read FUso;
    // Lazarus RunTimeOnly: nunca entra na IDE, nem como dependencia
    property LazRuntimeOnly: boolean read FLazRuntimeOnly;
    property LibSuffix: string read FLibSuffix;
    // tudo o que o pacote declara exigir, como declarado
    property Requires: TStringList read FRequires;
    // dos Requires, os que sao pacotes deste catalogo
    property Internos: TStringList read FInternos;
    // dos Requires, os que nao sao (rtl, designide, FireDAC, indylaz, mormot2)
    property Externos: TStringList read FExternos;
    // Delphi: pacotes do DCC_UsePackage do .dproj que o requires nao traz,
    // sem sufixo numerico (IndyCore160 -> IndyCore)
    property Implicitos: TStringList read FImplicitos;
    // Lazarus: nome=versao minima exigida ('indylaz=10.6')
    property VersoesMinimas: TStringList read FVersoesMinimas;
    // arquivos de unidade do pacote, relativos a raiz, com '/'
    property Unidades: TStringList read FUnidades;
    // unidades declaradas que a origem nao tem e que nenhum submodulo explica
    property FontesAusentes: TStringList read FFontesAusentes;
    // caminhos dos submodulos (.gitmodules) onde moram unidades do pacote: o
    // zipball do GitHub nao os traz, e o download tem de buscar cada um
    property Submodulos: TStringList read FSubmodulos;
    // dos Submodulos, os que a origem nao tem (checkout sem submodule update,
    // zip do GitHub)
    property SubmodulosAusentes: TStringList read FSubmodulosAusentes;
    // posicao na ordem de instalacao do seu tipo (0 = primeiro); -1 em ciclo
    property Ordem: integer read FOrdem;
    // vai para a IDE (design ou ambos); runtime puro so e compilado
    property Instalavel: boolean read GetInstalavel;
  end;

  { TCatalogo }

  TCatalogo = class
  private
    FOrigem: TOrigemArquivos;
    FPacotes: TObjectList;
    FErros: TStringList;
    FSubmodulos: TStringList;
    function GetCount: integer;
    function GetPacote(AIndex: integer): TPacote;
    function GetRaiz: string;
    procedure LerDpk(const AArquivo: string);
    procedure LerLpk(const AArquivo: string);
    procedure LerSubmodulos;
    procedure ListarPacotes(const APasta, AExtensao: string; ALista: TStrings);
    procedure Resolver;
    procedure Ordenar(ATipo: TTipoPacote);
  public
    constructor Create;
    destructor Destroy; override;

    // le os pacotes a partir da origem, que passa a pertencer ao catalogo;
    // devolve False se nao ha pacote nenhum
    function Carregar(AOrigem: TOrigemArquivos): boolean; overload;
    // atalho para uma arvore em disco (a pasta que contem pkg/ e src/)
    function Carregar(const ARaizRepo: string): boolean; overload;
    procedure Limpar;

    function Buscar(ATipo: TTipoPacote; const ANome: string): TPacote;
    // pacotes de um tipo, na ordem de instalacao
    procedure Listar(ATipo: TTipoPacote; ALista: TList);
    // os pedidos mais tudo de que dependem, na ordem de instalacao; nomes que
    // o catalogo nao conhece vao para ADesconhecidos
    procedure Fechamento(ATipo: TTipoPacote; ANomes: TStrings; ALista: TList;
      ADesconhecidos: TStrings = nil);
    // plano legivel, antes de qualquer escrita
    function Plano(ATipo: TTipoPacote; ANomes: TStrings = nil): string;

    // descricao da origem
    property Raiz: string read GetRaiz;
    property Origem: TOrigemArquivos read FOrigem;
    // caminho=url de cada submodulo declarado no .gitmodules
    property Submodulos: TStringList read FSubmodulos;
    property Count: integer read GetCount;
    property Pacotes[AIndex: integer]: TPacote read GetPacote; default;
    // falhas de leitura e ciclos: o catalogo continua usavel sem esses pacotes
    property Erros: TStringList read FErros;
  end;

function NomeUso(AUso: TUsoPacote): string;
function NomeTipoPacote(ATipo: TTipoPacote): string;

implementation

uses
  RegExpr, DOM, XMLRead;

function NomeUso(AUso: TUsoPacote): string;
begin
  case AUso of
    upRuntime: Result := 'runtime';
    upDesign:  Result := 'design';
    upAmbos:   Result := 'runtime+design';
  else
    Result := '';
  end;
end;

function NomeTipoPacote(ATipo: TTipoPacote): string;
begin
  case ATipo of
    tpDelphi:  Result := 'Delphi';
    tpLazarus: Result := 'Lazarus';
  else
    Result := '';
  end;
end;

function NovaLista: TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  Result.Duplicates := dupIgnore;
end;

function Contem(ALista: TStrings; const ANome: string): boolean;
begin
  Result := ALista.IndexOf(ANome) >= 0;
end;

// pasta de um caminho relativo com '/': 'pkg/Delphi/X.dpk' -> 'pkg/Delphi'
function PastaRelativa(const ACaminho: string): string;
var
  vPos: integer;
begin
  vPos := LastDelimiter('/', ACaminho);
  Result := Copy(ACaminho, 1, vPos - 1);
end;

// caminho escrito dentro de um pacote ('..\..\src\x.pas'), resolvido contra a
// pasta do pacote e devolvido relativo a raiz, com '/' e sem '.' nem '..';
// vazio se sai da raiz
function ResolverRelativo(const APastaPacote, ACaminho: string): string;
var
  vPartes: TStringList;
  vItem: string;
begin
  Result := '';
  vPartes := TStringList.Create;
  try
    for vItem in (APastaPacote + '/' + StringReplace(Trim(ACaminho), '\', '/', [rfReplaceAll])).Split(['/']) do
    begin
      if (vItem = '') or (vItem = '.') then
        Continue;
      if vItem = '..' then
      begin
        if vPartes.Count = 0 then
          Exit;
        vPartes.Delete(Pred(vPartes.Count));
      end
      else
        vPartes.Add(vItem);
    end;
    for vItem in vPartes do
    begin
      if Result <> '' then
        Result := Result + '/';
      Result := Result + vItem;
    end;
  finally
    vPartes.Free;
  end;
end;

function SemBOM(const ATexto: string): string;
begin
  // BOM UTF-8 (o XSocketRAL.dpk tem)
  Result := ATexto;
  if Copy(Result, 1, 3) = #$EF#$BB#$BF then
    Delete(Result, 1, 3);
end;

{ TOrigemLocal }

constructor TOrigemLocal.Create(const ARaiz: string);
begin
  inherited Create;
  FRaiz := IncludeTrailingPathDelimiter(ExpandFileName(ARaiz));
end;

function TOrigemLocal.Descricao: string;
begin
  Result := FRaiz;
end;

function TOrigemLocal.Localizar(const ACaminho: string): string;
begin
  Result := FRaiz + StringReplace(ACaminho, '/', PathDelim, [rfReplaceAll]);
end;

procedure TOrigemLocal.ListarPasta(const APasta, ARelativo: string; ALista: TStrings);
var
  vSearch: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(APasta) + '*', faAnyFile, vSearch) = 0 then
  try
    repeat
      if (vSearch.Name = '.') or (vSearch.Name = '..') then
        Continue;
      if (vSearch.Attr and faDirectory) <> 0 then
        ListarPasta(IncludeTrailingPathDelimiter(APasta) + vSearch.Name,
                    ARelativo + vSearch.Name + '/', ALista)
      else
        ALista.Add(ARelativo + vSearch.Name);
    until FindNext(vSearch) <> 0;
  finally
    FindClose(vSearch);
  end;
end;

procedure TOrigemLocal.Listar(const APasta: string; ALista: TStrings);
var
  vPasta: string;
begin
  vPasta := StringReplace(APasta, '\', '/', [rfReplaceAll]);
  if (vPasta <> '') and (vPasta[Length(vPasta)] <> '/') then
    vPasta := vPasta + '/';
  ListarPasta(Localizar(vPasta), vPasta, ALista);
end;

function TOrigemLocal.Existe(const ACaminho: string): boolean;
begin
  Result := FileExists(Localizar(ACaminho));
end;

function TOrigemLocal.Ler(const ACaminho: string): string;
var
  vLista: TStringList;
begin
  vLista := TStringList.Create;
  try
    vLista.LoadFromFile(Localizar(ACaminho));
    Result := SemBOM(vLista.Text);
  finally
    vLista.Free;
  end;
end;

{ TPacote }

constructor TPacote.Create(ATipo: TTipoPacote);
begin
  inherited Create;
  FTipo := ATipo;
  FUso := upAmbos;
  FOrdem := -1;
  FRequires := NovaLista;
  FInternos := NovaLista;
  FExternos := NovaLista;
  FImplicitos := NovaLista;
  FVersoesMinimas := NovaLista;
  FUnidades := NovaLista;
  FFontesAusentes := NovaLista;
  FSubmodulos := NovaLista;
  FSubmodulosAusentes := NovaLista;
end;

destructor TPacote.Destroy;
begin
  FRequires.Free;
  FInternos.Free;
  FExternos.Free;
  FImplicitos.Free;
  FVersoesMinimas.Free;
  FUnidades.Free;
  FFontesAusentes.Free;
  FSubmodulos.Free;
  FSubmodulosAusentes.Free;
  inherited Destroy;
end;

function TPacote.GetInstalavel: boolean;
begin
  Result := FUso <> upRuntime;
end;

{ TCatalogo }

constructor TCatalogo.Create;
begin
  inherited Create;
  FPacotes := TObjectList.Create(True);
  FErros := TStringList.Create;
  FSubmodulos := TStringList.Create;
end;

destructor TCatalogo.Destroy;
begin
  Limpar;
  FPacotes.Free;
  FErros.Free;
  FSubmodulos.Free;
  inherited Destroy;
end;

function TCatalogo.GetRaiz: string;
begin
  Result := '';
  if FOrigem <> nil then
    Result := FOrigem.Descricao;
end;

function TCatalogo.GetCount: integer;
begin
  Result := FPacotes.Count;
end;

function TCatalogo.GetPacote(AIndex: integer): TPacote;
begin
  Result := TPacote(FPacotes[AIndex]);
end;

procedure TCatalogo.Limpar;
begin
  FPacotes.Clear;
  FErros.Clear;
  FSubmodulos.Clear;
  FreeAndNil(FOrigem);
end;

procedure TCatalogo.ListarPacotes(const APasta, AExtensao: string; ALista: TStrings);
var
  vTodos: TStringList;
  vArquivo, vPasta: string;
begin
  vTodos := TStringList.Create;
  try
    FOrigem.Listar(APasta, vTodos);
    for vArquivo in vTodos do
    begin
      if not SameText(ExtractFileExt(vArquivo), AExtensao) then
        Continue;
      // copias de seguranca das IDEs tem pacotes com o mesmo nome
      vPasta := '/' + LowerCase(PastaRelativa(vArquivo)) + '/';
      if (Pos('/__history/', vPasta) > 0) or (Pos('/__recovery/', vPasta) > 0) or
         (Pos('/backup/', vPasta) > 0) or (Pos('/lib/', vPasta) > 0) or
         (Pos('/.', vPasta) > 0) then
        Continue;
      ALista.Add(vArquivo);
    end;
  finally
    vTodos.Free;
  end;
end;

procedure TCatalogo.LerSubmodulos;
var
  vLinhas: TStringList;
  vLinha, vChave, vCaminho, vUrl: string;
  vInt, vPos: integer;

  procedure Fechar;
  begin
    if vCaminho <> '' then
      FSubmodulos.Values[StringReplace(vCaminho, '\', '/', [rfReplaceAll])] := vUrl;
    vCaminho := '';
    vUrl := '';
  end;

begin
  FSubmodulos.Clear;
  if not FOrigem.Existe('.gitmodules') then
    Exit;

  // cada bloco [submodule "..."] tem path e url, em qualquer ordem
  vCaminho := '';
  vUrl := '';
  vLinhas := TStringList.Create;
  try
    vLinhas.Text := FOrigem.Ler('.gitmodules');
    for vInt := 0 to Pred(vLinhas.Count) do
    begin
      vLinha := Trim(vLinhas[vInt]);
      if (vLinha <> '') and (vLinha[1] = '[') then
      begin
        Fechar;
        Continue;
      end;
      vPos := Pos('=', vLinha);
      if vPos = 0 then
        Continue;
      vChave := LowerCase(Trim(Copy(vLinha, 1, vPos - 1)));
      if vChave = 'path' then
        vCaminho := Trim(Copy(vLinha, vPos + 1, MaxInt))
      else if vChave = 'url' then
        vUrl := Trim(Copy(vLinha, vPos + 1, MaxInt));
    end;
    Fechar;
  finally
    vLinhas.Free;
  end;
end;

procedure TCatalogo.LerDpk(const AArquivo: string);
var
  vTexto, vLimpo, vGrupo, vDproj, vNome, vPastaPkg: string;
  vRegex: TRegExpr;
  vPacote: TPacote;
  vItens: TStringArray;
  vItem: string;
  vPasta: string;
begin
  vPacote := TPacote.Create(tpDelphi);
  try
    vTexto := FOrigem.Ler(AArquivo);
    vPacote.FArquivo := FOrigem.Localizar(AArquivo);
    vPacote.FArquivoRelativo := AArquivo;
    vPastaPkg := PastaRelativa(AArquivo);

    vRegex := TRegExpr.Create;
    try
      vRegex.ModifierI := True;

      // diretivas antes de tirar os comentarios, que e onde elas moram
      vRegex.Expression := '\{\$RUNONLY\}';
      if vRegex.Exec(vTexto) then
        vPacote.FUso := upRuntime;
      vRegex.Expression := '\{\$DESIGNONLY\}';
      if vRegex.Exec(vTexto) then
        vPacote.FUso := upDesign;
      vRegex.Expression := '\{\$DESCRIPTION\s+''([^'']*)''\}';
      if vRegex.Exec(vTexto) then
        vPacote.FDescricao := vRegex.Match[1];
      vRegex.Expression := '\{\$LIBSUFFIX\s+(''[^'']*''|AUTO)\}';
      if vRegex.Exec(vTexto) then
        vPacote.FLibSuffix := StringReplace(vRegex.Match[1], '''', '', [rfReplaceAll]);

      // sem comentarios de nenhum dos tres tipos; strings ficam, porque os
      // caminhos do contains estao nelas
      vRegex.Expression := '\{[^}]*\}|\(\*.*?\*\)|//[^\n]*';
      vRegex.ModifierS := True;
      vLimpo := vRegex.Replace(vTexto, ' ', False);
      vRegex.ModifierS := False;

      vRegex.Expression := '\bpackage\s+([\w.]+)\s*;';
      if not vRegex.Exec(vLimpo) then
      begin
        FErros.Add(AArquivo + ': cláusula "package" não encontrada');
        FreeAndNil(vPacote);
        Exit;
      end;
      vPacote.FNome := vRegex.Match[1];

      vRegex.ModifierS := True;
      vRegex.Expression := '\brequires\b(.*?);';
      if vRegex.Exec(vLimpo) then
      begin
        vItens := vRegex.Match[1].Split([',']);
        for vItem in vItens do
          if Trim(vItem) <> '' then
            vPacote.FRequires.Add(Trim(vItem));
      end;

      vRegex.Expression := '\bcontains\b(.*?);';
      if vRegex.Exec(vLimpo) then
      begin
        vItens := vRegex.Match[1].Split([',']);
        for vItem in vItens do
        begin
          vPasta := vItem;
          if Pos('''', vPasta) > 0 then
          begin
            vPasta := Copy(vPasta, Pos('''', vPasta) + 1, MaxInt);
            vPasta := Copy(vPasta, 1, Pos('''', vPasta) - 1);
            vPasta := ResolverRelativo(vPastaPkg, vPasta);
            if vPasta <> '' then
              vPacote.FUnidades.Add(vPasta);
          end;
        end;
      end;
    finally
      vRegex.Free;
    end;

    // rastro do que o requires nao declara (o Indy, com ou sem sufixo)
    vDproj := ChangeFileExt(AArquivo, '.dproj');
    if FOrigem.Existe(vDproj) then
    begin
      vRegex := TRegExpr.Create('<DCC_UsePackage>([^<]*)</DCC_UsePackage>');
      try
        if vRegex.Exec(FOrigem.Ler(vDproj)) then
          repeat
            for vItem in vRegex.Match[1].Split([';']) do
            begin
              vNome := Trim(vItem);
              if (vNome = '') or (Pos('$(', vNome) > 0) then
                Continue;
              vNome := ReplaceRegExpr('^(.*[A-Za-z_])\d{2,3}$', vNome, '$1', True);
              if not SameText(vNome, vPacote.FNome) and not Contem(vPacote.FRequires, vNome) and
                 not Contem(vPacote.FImplicitos, vNome) then
                vPacote.FImplicitos.Add(vNome);
            end;
          until not vRegex.ExecNext;
      finally
        vRegex.Free;
      end;
      vPacote.FImplicitos.Sort;
    end;

    vGrupo := Copy(vPastaPkg, Length('pkg/Delphi/') + 1, MaxInt);
    vPacote.FGrupo := vGrupo;

    FPacotes.Add(vPacote);
  except
    on E: Exception do
    begin
      FErros.Add(AArquivo + ': ' + E.Message);
      vPacote.Free;
    end;
  end;
end;

procedure TCatalogo.LerLpk(const AArquivo: string);
var
  vDoc: TXMLDocument;
  vStream: TStringStream;
  vPkg, vNo, vItem, vFilho: TDOMNode;
  vPacote: TPacote;
  vInt: integer;
  vTipo, vNome, vVersao, vGrupo, vArq: string;

  function Filho(ANo: TDOMNode; const ANome: string): TDOMNode;
  begin
    Result := nil;
    if ANo <> nil then
      Result := ANo.FindNode(DOMString(ANome));
  end;

  function Valor(ANo: TDOMNode; const ANome: string = 'Value'): string;
  begin
    Result := '';
    if (ANo <> nil) and (ANo is TDOMElement) then
      Result := string(TDOMElement(ANo).GetAttribute(DOMString(ANome)));
  end;

begin
  vPacote := nil;
  vDoc := nil;
  try
    vStream := TStringStream.Create(FOrigem.Ler(AArquivo));
    try
      ReadXMLFile(vDoc, vStream);
    finally
      vStream.Free;
    end;
    try
      vPkg := Filho(vDoc.DocumentElement, 'Package');
      if vPkg = nil then
      begin
        FErros.Add(AArquivo + ': nó <Package> não encontrado');
        Exit;
      end;

      vPacote := TPacote.Create(tpLazarus);
      vPacote.FArquivo := FOrigem.Localizar(AArquivo);
      vPacote.FArquivoRelativo := AArquivo;
      vPacote.FNome := Valor(Filho(vPkg, 'Name'));
      if vPacote.FNome = '' then
        vPacote.FNome := ChangeFileExt(ExtractFileName(AArquivo), '');
      vPacote.FDescricao := Valor(Filho(vPkg, 'Description'));

      vTipo := Valor(Filho(vPkg, 'Type'));
      if SameText(vTipo, 'RunTime') then
        vPacote.FUso := upRuntime
      else if SameText(vTipo, 'RunTimeOnly') then
      begin
        vPacote.FUso := upRuntime;
        vPacote.FLazRuntimeOnly := True;
      end
      else if SameText(vTipo, 'DesignTime') then
        vPacote.FUso := upDesign
      else if SameText(vTipo, 'RunAndDesignTime') then
        vPacote.FUso := upAmbos
      else
        vPacote.FUso := upRuntime;

      vNo := Filho(vPkg, 'RequiredPkgs');
      if vNo <> nil then
        for vInt := 0 to Pred(vNo.ChildNodes.Count) do
        begin
          vItem := vNo.ChildNodes[vInt];
          vNome := Valor(Filho(vItem, 'PackageName'));
          if vNome = '' then
            Continue;
          vPacote.FRequires.Add(vNome);
          vFilho := Filho(vItem, 'MinVersion');
          if (vFilho <> nil) and SameText(Valor(vFilho, 'Valid'), 'True') then
          begin
            vVersao := Valor(vFilho, 'Major');
            if vVersao = '' then
              vVersao := '0';
            if Valor(vFilho, 'Minor') <> '' then
              vVersao := vVersao + '.' + Valor(vFilho, 'Minor');
            if Valor(vFilho, 'Release') <> '' then
              vVersao := vVersao + '.' + Valor(vFilho, 'Release');
            vPacote.FVersoesMinimas.Values[vNome] := vVersao;
          end;
        end;

      vNo := Filho(vPkg, 'Files');
      if vNo <> nil then
        for vInt := 0 to Pred(vNo.ChildNodes.Count) do
        begin
          vItem := vNo.ChildNodes[vInt];
          vArq := Valor(Filho(vItem, 'Filename'));
          // include e .lrs tambem sao fonte, mas o que falta em submodulo
          // aparece nas unidades; so elas contam
          if (vArq <> '') and (SameText(ExtractFileExt(vArq), '.pas') or
                               SameText(ExtractFileExt(vArq), '.pp')) then
          begin
            vArq := ResolverRelativo(PastaRelativa(AArquivo), vArq);
            if vArq <> '' then
              vPacote.FUnidades.Add(vArq);
          end;
        end;
    finally
      vDoc.Free;
    end;

    vGrupo := Copy(PastaRelativa(AArquivo), Length('pkg/Lazarus/') + 1, MaxInt);
    vPacote.FGrupo := vGrupo;

    FPacotes.Add(vPacote);
    vPacote := nil;
  except
    on E: Exception do
    begin
      FErros.Add(AArquivo + ': ' + E.Message);
      vPacote.Free;
    end;
  end;
end;

procedure TCatalogo.Resolver;
var
  vInt, vReq: integer;
  vPacote: TPacote;
  vUnidade, vSub: string;
  vHerdados, vVisitados: TStringList;

  function SubmoduloDe(const AUnidade: string): string;
  var
    vIdx: integer;
  begin
    Result := '';
    for vIdx := 0 to Pred(FSubmodulos.Count) do
      if SameText(Copy(AUnidade, 1, Length(FSubmodulos.Names[vIdx]) + 1),
                  FSubmodulos.Names[vIdx] + '/') then
        Exit(FSubmodulos.Names[vIdx]);
  end;

  // requires de todas as dependencias internas, transitivamente; o conjunto
  // de visitados impede que um ciclo gire para sempre
  procedure Herdar(APacote: TPacote);
  var
    vDep: integer;
    vFilho: TPacote;
  begin
    for vDep := 0 to Pred(APacote.FInternos.Count) do
    begin
      if Contem(vVisitados, APacote.FInternos[vDep]) then
        Continue;
      vVisitados.Add(APacote.FInternos[vDep]);
      vFilho := Buscar(APacote.Tipo, APacote.FInternos[vDep]);
      if vFilho = nil then
        Continue;
      vHerdados.AddStrings(vFilho.FRequires);
      Herdar(vFilho);
    end;
  end;

begin
  for vInt := 0 to Pred(Count) do
  begin
    vPacote := Pacotes[vInt];
    vPacote.FInternos.Clear;
    vPacote.FExternos.Clear;
    for vReq := 0 to Pred(vPacote.FRequires.Count) do
      if Buscar(vPacote.Tipo, vPacote.FRequires[vReq]) <> nil then
        vPacote.FInternos.Add(Buscar(vPacote.Tipo, vPacote.FRequires[vReq]).Nome)
      else
        vPacote.FExternos.Add(vPacote.FRequires[vReq]);

    // unidade dentro de submodulo nao e "ausente": e um download a mais, que
    // vale inclusive quando a arvore local ja o tem (o zipball nao trara)
    vPacote.FFontesAusentes.Clear;
    vPacote.FSubmodulos.Clear;
    vPacote.FSubmodulosAusentes.Clear;
    for vUnidade in vPacote.FUnidades do
    begin
      vSub := SubmoduloDe(vUnidade);
      if vSub <> '' then
      begin
        if not Contem(vPacote.FSubmodulos, vSub) then
          vPacote.FSubmodulos.Add(vSub);
        if not FOrigem.Existe(vUnidade) and not Contem(vPacote.FSubmodulosAusentes, vSub) then
          vPacote.FSubmodulosAusentes.Add(vSub);
      end
      else if not FOrigem.Existe(vUnidade) then
        vPacote.FFontesAusentes.Add(vUnidade);
    end;
  end;

  // implicito so interessa quando nada no grafo o traz: o rtl que o
  // PascalRAL exige ja chega a todo pacote que depende dele, e pacote do
  // proprio catalogo chega pela ordem
  vHerdados := NovaLista;
  vVisitados := NovaLista;
  try
    for vInt := 0 to Pred(Count) do
    begin
      vPacote := Pacotes[vInt];
      if vPacote.FImplicitos.Count = 0 then
        Continue;
      vHerdados.Clear;
      vVisitados.Clear;
      Herdar(vPacote);
      for vReq := Pred(vPacote.FImplicitos.Count) downto 0 do
        if Contem(vHerdados, vPacote.FImplicitos[vReq]) or
           (Buscar(vPacote.Tipo, vPacote.FImplicitos[vReq]) <> nil) then
          vPacote.FImplicitos.Delete(vReq);
    end;
  finally
    vHerdados.Free;
    vVisitados.Free;
  end;
end;

function CompararPacotes(AItem1, AItem2: Pointer): integer;
var
  vA, vB: TPacote;
begin
  // desempate estavel da ordem topologica: raiz antes de subpasta, depois nome
  vA := TPacote(AItem1);
  vB := TPacote(AItem2);
  Result := Ord(vA.Grupo <> '') - Ord(vB.Grupo <> '');
  if Result = 0 then
    Result := CompareText(vA.Grupo, vB.Grupo);
  if Result = 0 then
    Result := CompareText(vA.Nome, vB.Nome);
end;

procedure TCatalogo.Ordenar(ATipo: TTipoPacote);
var
  vPendentes, vProntos: TList;
  vFeitos: TStringList;
  vInt, vDep, vOrdem: integer;
  vPacote: TPacote;
  vLivre: boolean;
  vCiclo: string;
begin
  // Kahn: a cada passada entram os pacotes cujas dependencias internas ja
  // entraram; entre os que ficam livres na mesma passada vale o desempate
  vPendentes := TList.Create;
  vProntos := TList.Create;
  vFeitos := NovaLista;
  try
    for vInt := 0 to Pred(Count) do
      if Pacotes[vInt].Tipo = ATipo then
      begin
        Pacotes[vInt].FOrdem := -1;
        vPendentes.Add(Pacotes[vInt]);
      end;

    vOrdem := 0;
    while vPendentes.Count > 0 do
    begin
      vProntos.Clear;
      for vInt := 0 to Pred(vPendentes.Count) do
      begin
        vPacote := TPacote(vPendentes[vInt]);
        vLivre := True;
        for vDep := 0 to Pred(vPacote.FInternos.Count) do
          if not Contem(vFeitos, vPacote.FInternos[vDep]) then
          begin
            vLivre := False;
            Break;
          end;
        if vLivre then
          vProntos.Add(vPacote);
      end;

      if vProntos.Count = 0 then
      begin
        // sobrou ciclo: fica sem ordem e o plano recusa esses pacotes
        vCiclo := '';
        for vInt := 0 to Pred(vPendentes.Count) do
          vCiclo := vCiclo + TPacote(vPendentes[vInt]).Nome + ' ';
        FErros.Add(Format('Dependência circular entre pacotes %s: %s',
                          [NomeTipoPacote(ATipo), Trim(vCiclo)]));
        Break;
      end;

      vProntos.Sort(@CompararPacotes);
      for vInt := 0 to Pred(vProntos.Count) do
      begin
        vPacote := TPacote(vProntos[vInt]);
        vPacote.FOrdem := vOrdem;
        Inc(vOrdem);
        vFeitos.Add(vPacote.Nome);
        vPendentes.Remove(vPacote);
      end;
    end;
  finally
    vPendentes.Free;
    vProntos.Free;
    vFeitos.Free;
  end;
end;

function TCatalogo.Carregar(const ARaizRepo: string): boolean;
begin
  Result := Carregar(TOrigemLocal.Create(ARaizRepo));
end;

function TCatalogo.Carregar(AOrigem: TOrigemArquivos): boolean;
var
  vArquivos: TStringList;
  vArquivo: string;
begin
  Limpar;
  FOrigem := AOrigem;

  LerSubmodulos;

  vArquivos := TStringList.Create;
  try
    ListarPacotes('pkg/Delphi', '.dpk', vArquivos);
    vArquivos.Sort;
    for vArquivo in vArquivos do
      LerDpk(vArquivo);

    vArquivos.Clear;
    ListarPacotes('pkg/Lazarus', '.lpk', vArquivos);
    vArquivos.Sort;
    for vArquivo in vArquivos do
      LerLpk(vArquivo);
  finally
    vArquivos.Free;
  end;

  Resolver;
  Ordenar(tpDelphi);
  Ordenar(tpLazarus);
  Result := Count > 0;
end;

function TCatalogo.Buscar(ATipo: TTipoPacote; const ANome: string): TPacote;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(Count) do
    if (Pacotes[vInt].Tipo = ATipo) and SameText(Pacotes[vInt].Nome, ANome) then
      Exit(Pacotes[vInt]);
end;

function CompararOrdem(AItem1, AItem2: Pointer): integer;
begin
  Result := TPacote(AItem1).Ordem - TPacote(AItem2).Ordem;
end;

procedure TCatalogo.Listar(ATipo: TTipoPacote; ALista: TList);
var
  vInt: integer;
begin
  ALista.Clear;
  for vInt := 0 to Pred(Count) do
    if (Pacotes[vInt].Tipo = ATipo) and (Pacotes[vInt].Ordem >= 0) then
      ALista.Add(Pacotes[vInt]);
  ALista.Sort(@CompararOrdem);
end;

procedure TCatalogo.Fechamento(ATipo: TTipoPacote; ANomes: TStrings; ALista: TList;
  ADesconhecidos: TStrings);
var
  vFila: TStringList;
  vInt: integer;
  vPacote: TPacote;
begin
  ALista.Clear;
  vFila := NovaLista;
  try
    vFila.AddStrings(ANomes);
    vInt := 0;
    // a fila cresce enquanto anda: cada pacote traz as dependencias dele
    while vInt < vFila.Count do
    begin
      vPacote := Buscar(ATipo, vFila[vInt]);
      if vPacote = nil then
      begin
        if ADesconhecidos <> nil then
          ADesconhecidos.Add(vFila[vInt]);
      end
      else if ALista.IndexOf(vPacote) < 0 then
      begin
        ALista.Add(vPacote);
        vFila.AddStrings(vPacote.Internos);
      end;
      Inc(vInt);
    end;
  finally
    vFila.Free;
  end;
  ALista.Sort(@CompararOrdem);
end;

function TCatalogo.Plano(ATipo: TTipoPacote; ANomes: TStrings): string;
var
  vLista: TList;
  vDesconhecidos, vExternos, vSubmodulosUsados, vSaida: TStringList;
  vInt: integer;
  vPacote: TPacote;
  vLinha, vSub: string;
begin
  vLista := TList.Create;
  vDesconhecidos := NovaLista;
  vExternos := NovaLista;
  vSubmodulosUsados := NovaLista;
  vSaida := TStringList.Create;
  try
    vExternos.Sorted := True;
    vSubmodulosUsados.Sorted := True;
    if ANomes = nil then
      Listar(ATipo, vLista)
    else
      Fechamento(ATipo, ANomes, vLista, vDesconhecidos);

    vSaida.Add(Format('Plano %s: %d pacote(s), a partir de %s',
                      [NomeTipoPacote(ATipo), vLista.Count, Raiz]));
    vSaida.Add('');
    for vInt := 0 to Pred(vLista.Count) do
    begin
      vPacote := TPacote(vLista[vInt]);
      if vPacote.Instalavel then
        vLinha := 'compilar e instalar'
      else
        vLinha := 'só compilar';
      vSaida.Add(Format('%2d. %-22s %-15s %-20s %s',
        [vInt + 1, vPacote.Nome, NomeUso(vPacote.Uso), vLinha, vPacote.ArquivoRelativo]));
      if vPacote.Internos.Count > 0 then
        vSaida.Add('      depois de: ' + StringReplace(vPacote.Internos.CommaText, ',', ', ', [rfReplaceAll]));
      if vPacote.Implicitos.Count > 0 then
        vSaida.Add('      implícitos (.dproj, fora do requires): ' +
                   StringReplace(vPacote.Implicitos.CommaText, ',', ', ', [rfReplaceAll]));
      if vPacote.Unidades.Count = 0 then
        vSaida.Add('      VAZIO: o pacote não contém nenhuma unidade');
      for vSub in vPacote.Submodulos do
      begin
        vLinha := '      submódulo: ' + vSub + '  <- ' + Submodulos.Values[vSub];
        if Contem(vPacote.SubmodulosAusentes, vSub) then
          vLinha := vLinha + '  (não está na origem: baixar)';
        vSaida.Add(vLinha);
        vSubmodulosUsados.Add(vSub);
      end;
      if vPacote.FontesAusentes.Count > 0 then
        vSaida.Add('      FONTES AUSENTES: ' +
                   StringReplace(vPacote.FontesAusentes.CommaText, ',', ', ', [rfReplaceAll]));
      vExternos.AddStrings(vPacote.Externos);
    end;

    vSaida.Add('');
    if vExternos.Count > 0 then
      vSaida.Add('Pacotes externos exigidos: ' +
                 StringReplace(vExternos.CommaText, ',', ', ', [rfReplaceAll]));
    if vSubmodulosUsados.Count > 0 then
      vSaida.Add('Submódulos a baixar junto com o RAL: ' +
                 StringReplace(vSubmodulosUsados.CommaText, ',', ', ', [rfReplaceAll]));
    if vDesconhecidos.Count > 0 then
      vSaida.Add('Pedidos que o catálogo não conhece: ' +
                 StringReplace(vDesconhecidos.CommaText, ',', ', ', [rfReplaceAll]));
    for vInt := 0 to Pred(FErros.Count) do
      vSaida.Add('ERRO: ' + FErros[vInt]);

    Result := vSaida.Text;
  finally
    vLista.Free;
    vDesconhecidos.Free;
    vExternos.Free;
    vSubmodulosUsados.Free;
    vSaida.Free;
  end;
end;

end.
