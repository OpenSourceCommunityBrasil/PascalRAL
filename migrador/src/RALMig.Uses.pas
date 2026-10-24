/// Reads the uses clauses of a unit/program/library (interface, implementation,
/// main program) and the requires/contains clauses of a package, and edits them
/// with the style of the file: items one per line stay one per line, items in a
/// line stay in a line (wrapped at 80 columns with the indentation of the
/// continuation), `in 'file.pas'` is preserved, and items inside {$IFDEF} are
/// known as such. Every operation only adds edits to the TTextoOriginal, always
/// against the offsets of the original; two operations may not touch the same
/// bytes (use TirarVarias to remove several neighbours at once).
unit RALMig.&Uses;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  RALMig.Texto, RALMig.Tokens;

type
  /// Which clause: uses of the interface, of the implementation, of a program
  /// or library, requires or contains of a package.
  TSecaoUses = (suInterface, suImplementation, suPrograma, suRequires, suContains);

  /// What kind of file the text is.
  TTipoUnidade = (tuDesconhecido, tuUnit, tuPrograma, tuPacote);

  EUsesErro = class(Exception);

  /// One item of a clause.
  TItemUses = class
  public
    /// Name as written, with dots ('System.SysUtils')
    Nome: string;
    /// Offset and size of the name (dots included)
    NomeInicio: integer;
    NomeTamanho: integer;
    /// Offset and size of the whole item (name and `in '...'`)
    Inicio: integer;
    Tamanho: integer;
    /// Text of `in '...'` without the quotes ('' when there is none)
    Arquivo: string;
    /// Offset of the comma that follows the item (0 for the last one)
    Virgula: integer;
    /// True when a conditional directive is open at the item
    DentroIfDef: boolean;
    /// The innermost open directive, 'IFDEF FPC' for instance
    Condicao: string;
    /// Line of the name
    Linha: integer;
    /// Offset just after the item
    function Fim: integer;
  end;

  /// One clause with its items.
  TClausulaUses = class
  public
    Secao: TSecaoUses;
    /// Offsets of the keyword (uses/requires/contains)
    PalavraInicio: integer;
    PalavraFim: integer;
    /// Offset of the `;` that ends the clause (0 when it is missing)
    PontoEVirgula: integer;
    Itens: TObjectList;
    /// Length of the line where appends go (-1 until the first append)
    ComprimentoAtual: integer;
    constructor Create;
    destructor Destroy; override;
    function Item(AIndex: integer): TItemUses;
    function Count: integer;
  end;

  /// Reader and editor of the clauses of one file.
  TEditorUses = class
  private
    FClausulas: TObjectList;
    FTexto: TTextoOriginal;
    FTipo: TTipoUnidade;
    FTokens: TListaTokens;
    function GetCount: integer;
    function GetClausula(AIndex: integer): TClausulaUses;
    procedure Diretiva(AIdx: integer; APilha: TStringList);
    function Avancar(AIdx: integer; APilha: TStringList): integer;
    procedure DetectarTipo;
    procedure LerClausula(var AIdx: integer; ASecao: TSecaoUses; APilha: TStringList);
    function LerItem(var AIdx: integer; APilha: TStringList): TItemUses;
    procedure LerArquivo(var AIdx: integer; AItem: TItemUses; APilha: TStringList);
    function SoEspacosAntes(AOffset: integer): boolean;
    function SoEspacosDepois(AOffset: integer): boolean;
    function PorLinha(ACl: TClausulaUses): boolean;
    function IndentacaoDeLinha(ACl: TClausulaUses): string;
    function IndentacaoContinuacao(ACl: TClausulaUses): string;
    procedure AcrescentarEm(ACl: TClausulaUses; const ATexto: string);
    procedure CriarClausula(ASecao: TSecaoUses; const ATexto: string);
    function LinhaDaAncora(ASecao: TSecaoUses): integer;
    function LinhaAntesDoFim(ASecao: TSecaoUses): integer;
    procedure TirarClausulaInteira(ACl: TClausulaUses);
    procedure TirarGrupo(ACl: TClausulaUses; ADe, APara: integer);
    procedure TirarGrupoNoFim(ACl: TClausulaUses; ADe, APara: integer);
    procedure TirarGrupoNoMeio(ACl: TClausulaUses; ADe, APara: integer);
    function ApenasVirgulaEEspaco(AIni, AFim, AVirgula: integer): boolean;
    function ProximoNaoEspaco(AOffset: integer): integer;
    function PontoDeInsercao(AItem: TItemUses): integer;
    function AnteriorNaoEspaco(AOffset: integer): integer;
  public
    constructor Create(ATexto: TTextoOriginal);
    destructor Destroy; override;
    /// Reads the clauses again from Texto.Bytes (the original)
    procedure Ler;
    /// The clause of the section, nil when the file has none (the first one
    /// when a conditional directive makes two)
    function Clausula(ASecao: TSecaoUses): TClausulaUses;
    /// The item with that unit name (no case) in any clause; nil if none
    function Achar(const AUnit: string): TItemUses;
    /// True when the unit is in any clause
    function Contem(const AUnit: string): boolean;
    /// True when the unit is in the clause of that section
    function ContemEm(const AUnit: string; ASecao: TSecaoUses): boolean;
    /// Adds the unit at the end of the clause; creates the clause when the
    /// section has none. Does nothing when the unit is already there (or, for
    /// the implementation, in the interface). AArquivo gives `in '...'`.
    procedure Acrescentar(const AUnit: string; ASecao: TSecaoUses;
      const AArquivo: string = '');
    /// Removes the unit from every clause with the right comma; returns how
    /// many items were removed
    function Tirar(const AUnit: string): integer;
    /// Removes all of the units at once (neighbours are fine)
    function TirarVarias(ANomes: TStrings): integer;
    /// Renames the unit in every clause, keeping `in '...'`; when the clause
    /// already has the new name the old item is removed. Returns the count.
    function Trocar(const AVelha, ANova: string): integer;
    property Count: integer read GetCount;
    property Clausulas[AIndex: integer]: TClausulaUses read GetClausula;
    property Texto: TTextoOriginal read FTexto;
    property Tokens: TListaTokens read FTokens;
    property Tipo: TTipoUnidade read FTipo;
  end;

implementation

resourcestring
  SSemSecao = 'The section of the file where the unit would go was not found';
  SSemPontoEVirgula = 'The uses clause at line %d has no semicolon';

const
  CLargura = 80;

{ TItemUses }

function TItemUses.Fim: integer;
begin
  Result := Inicio + Tamanho;
end;

{ TClausulaUses }

constructor TClausulaUses.Create;
begin
  inherited Create;
  Itens := TObjectList.Create(True);
  ComprimentoAtual := -1;
end;

destructor TClausulaUses.Destroy;
begin
  Itens.Free;
  inherited Destroy;
end;

function TClausulaUses.Item(AIndex: integer): TItemUses;
begin
  Result := TItemUses(Itens[AIndex]);
end;

function TClausulaUses.Count: integer;
begin
  Result := Itens.Count;
end;

{ TEditorUses }

constructor TEditorUses.Create(ATexto: TTextoOriginal);
begin
  inherited Create;
  FTexto := ATexto;
  FClausulas := TObjectList.Create(True);
  Ler;
end;

destructor TEditorUses.Destroy;
begin
  FTokens.Free;
  FClausulas.Free;
  inherited Destroy;
end;

function TEditorUses.GetCount: integer;
begin
  Result := FClausulas.Count;
end;

function TEditorUses.GetClausula(AIndex: integer): TClausulaUses;
begin
  Result := TClausulaUses(FClausulas[AIndex]);
end;

procedure TEditorUses.Diretiva(AIdx: integer; APilha: TStringList);
var
  vNome, vArg: string;
begin
  vNome := FTokens.NomeDiretiva(AIdx);
  vArg := FTokens.ArgumentoDiretiva(AIdx);
  if (vNome = 'IFDEF') or (vNome = 'IFNDEF') or (vNome = 'IF') or (vNome = 'IFOPT') then
    APilha.Add(vNome + ' ' + vArg)
  else if ((vNome = 'ELSE') or (vNome = 'ELSEIF')) and (APilha.Count > 0) then
  begin
    if Copy(APilha[APilha.Count - 1], 1, 5) <> 'ELSE ' then
      APilha[APilha.Count - 1] := 'ELSE ' + APilha[APilha.Count - 1];
  end
  else if ((vNome = 'ENDIF') or (vNome = 'IFEND')) and (APilha.Count > 0) then
    APilha.Delete(APilha.Count - 1);
end;

function TEditorUses.Avancar(AIdx: integer; APilha: TStringList): integer;
begin
  Result := AIdx + 1;
  while Result < FTokens.Count do
  begin
    if FTokens[Result].Tipo = ttDiretiva then
      Diretiva(Result, APilha)
    else if not FTokens.EhIgnoravel(Result) then
      Exit;
    Inc(Result);
  end;
end;

procedure TEditorUses.DetectarTipo;
var
  i: integer;
begin
  FTipo := tuDesconhecido;
  i := FTokens.Proximo(-1);
  if i >= FTokens.Count then
    Exit;
  if FTokens.EhNomeIgual(i, 'unit') then
    FTipo := tuUnit
  else if FTokens.EhNomeIgual(i, 'program') or FTokens.EhNomeIgual(i, 'library') then
    FTipo := tuPrograma
  else if FTokens.EhNomeIgual(i, 'package') then
    FTipo := tuPacote;
end;

function TEditorUses.LerItem(var AIdx: integer; APilha: TStringList): TItemUses;
var
  vFim: integer;
begin
  Result := TItemUses.Create;
  Result.NomeInicio := FTokens[AIdx].Inicio;
  vFim := FTokens[AIdx].Fim;
  while (AIdx + 2 < FTokens.Count) and FTokens.EhSimbolo(AIdx + 1, '.') and
    FTokens.EhNome(AIdx + 2) do
  begin
    Inc(AIdx, 2);
    vFim := FTokens[AIdx].Fim;
  end;
  Result.NomeTamanho := vFim - Result.NomeInicio;
  Result.Nome := Copy(FTexto.Bytes, Result.NomeInicio, Result.NomeTamanho);
  Result.Inicio := Result.NomeInicio;
  Result.Tamanho := Result.NomeTamanho;
  Result.Linha := FTexto.LinhaDoOffset(Result.NomeInicio);
  Result.DentroIfDef := APilha.Count > 0;
  if APilha.Count > 0 then
    Result.Condicao := APilha[APilha.Count - 1];
end;

// after the name: optional `in 'file'`; AIdx ends on the last token of the item
procedure TEditorUses.LerArquivo(var AIdx: integer; AItem: TItemUses;
  APilha: TStringList);
var
  vIn, vStr: integer;
  vTexto: string;
begin
  vIn := Avancar(AIdx, APilha);
  if (vIn >= FTokens.Count) or not FTokens.EhNomeIgual(vIn, 'in') then
    Exit;
  vStr := Avancar(vIn, APilha);
  if (vStr >= FTokens.Count) or (FTokens[vStr].Tipo <> ttString) then
    Exit;
  vTexto := FTokens.Texto(vStr);
  if (Length(vTexto) >= 2) and (vTexto[1] = '''') then
    AItem.Arquivo := Copy(vTexto, 2, Length(vTexto) - 2)
  else
    AItem.Arquivo := vTexto;
  AItem.Tamanho := FTokens[vStr].Fim - AItem.Inicio;
  AIdx := vStr;
end;

procedure TEditorUses.LerClausula(var AIdx: integer; ASecao: TSecaoUses;
  APilha: TStringList);
var
  vCl: TClausulaUses;
  vItem: TItemUses;
  j: integer;
begin
  vCl := TClausulaUses.Create;
  vCl.Secao := ASecao;
  vCl.PalavraInicio := FTokens[AIdx].Inicio;
  vCl.PalavraFim := FTokens[AIdx].Fim;
  FClausulas.Add(vCl);
  j := Avancar(AIdx, APilha);
  while (j < FTokens.Count) and FTokens.EhNome(j) do
  begin
    vItem := LerItem(j, APilha);
    vCl.Itens.Add(vItem);
    LerArquivo(j, vItem, APilha);
    j := Avancar(j, APilha);
    if (j < FTokens.Count) and FTokens.EhSimbolo(j, ',') then
    begin
      vItem.Virgula := FTokens[j].Inicio;
      j := Avancar(j, APilha);
    end
    else
      Break;
  end;
  if (j < FTokens.Count) and FTokens.EhSimbolo(j, ';') then
    vCl.PontoEVirgula := FTokens[j].Inicio;
  AIdx := j;
end;

procedure TEditorUses.Ler;
var
  i: integer;
  vSecao: TSecaoUses;
  vPilha: TStringList;
begin
  FreeAndNil(FTokens);
  FClausulas.Clear;
  FTokens := TokenizarPascal(FTexto.Bytes, FTexto.InicioConteudo);
  DetectarTipo;
  if FTipo = tuPrograma then
    vSecao := suPrograma
  else
    vSecao := suInterface;
  vPilha := TStringList.Create;
  try
    i := 0;
    while i < FTokens.Count do
    begin
      if FTokens[i].Tipo = ttDiretiva then
        Diretiva(i, vPilha)
      else if FTokens.EhNomeIgual(i, 'implementation') and (FTipo = tuUnit) then
        vSecao := suImplementation
      else if (FTokens[i].Tipo = ttPalavraChave) and FTokens.EhNomeIgual(i, 'uses') then
      begin
        LerClausula(i, vSecao, vPilha);
        Continue;
      end
      else if (FTipo = tuPacote) and FTokens.EhNomeIgual(i, 'requires') then
      begin
        LerClausula(i, suRequires, vPilha);
        Continue;
      end
      else if (FTipo = tuPacote) and FTokens.EhNomeIgual(i, 'contains') then
      begin
        LerClausula(i, suContains, vPilha);
        Continue;
      end;
      Inc(i);
    end;
  finally
    vPilha.Free;
  end;
end;

function TEditorUses.Clausula(ASecao: TSecaoUses): TClausulaUses;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Clausulas[i].Secao = ASecao then
      Exit(Clausulas[i]);
  Result := nil;
end;

function TEditorUses.Achar(const AUnit: string): TItemUses;
var
  i, j: integer;
begin
  for i := 0 to Count - 1 do
    for j := 0 to Clausulas[i].Count - 1 do
      if SameText(Clausulas[i].Item(j).Nome, AUnit) then
        Exit(Clausulas[i].Item(j));
  Result := nil;
end;

function TEditorUses.Contem(const AUnit: string): boolean;
begin
  Result := Achar(AUnit) <> nil;
end;

function TEditorUses.ContemEm(const AUnit: string; ASecao: TSecaoUses): boolean;
var
  i, j: integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if Clausulas[i].Secao = ASecao then
      for j := 0 to Clausulas[i].Count - 1 do
        if SameText(Clausulas[i].Item(j).Nome, AUnit) then
          Exit(True);
end;

{ text helpers }

function TEditorUses.SoEspacosAntes(AOffset: integer): boolean;
var
  i: integer;
begin
  Result := True;
  for i := FTexto.LinhaInicio(FTexto.LinhaDoOffset(AOffset)) to AOffset - 1 do
    if not (FTexto.Bytes[i] in [' ', #9]) then
      Exit(False);
end;

// AOffset is the first byte after the thing; true if only blanks follow on the line
function TEditorUses.SoEspacosDepois(AOffset: integer): boolean;
var
  i, vLinha: integer;
begin
  Result := True;
  if AOffset > Length(FTexto.Bytes) then
    Exit;
  vLinha := FTexto.LinhaDoOffset(AOffset);
  for i := AOffset to FTexto.LinhaFim(vLinha) - 1 do
    if not (FTexto.Bytes[i] in [' ', #9]) then
      Exit(False);
end;

// first offset >= AOffset that is not a space or tab
function TEditorUses.ProximoNaoEspaco(AOffset: integer): integer;
begin
  Result := AOffset;
  while (Result <= Length(FTexto.Bytes)) and (FTexto.Bytes[Result] in [' ', #9]) do
    Inc(Result);
end;

// first offset after the blanks that end right before AOffset
function TEditorUses.AnteriorNaoEspaco(AOffset: integer): integer;
begin
  Result := AOffset;
  while (Result > FTexto.InicioConteudo) and (FTexto.Bytes[Result - 1] in [' ', #9]) do
    Dec(Result);
end;

function TEditorUses.PorLinha(ACl: TClausulaUses): boolean;
var
  i, vPrimeiros: integer;
begin
  vPrimeiros := 0;
  for i := 0 to ACl.Count - 1 do
    if SoEspacosAntes(ACl.Item(i).Inicio) then
      Inc(vPrimeiros);
  Result := vPrimeiros * 2 >= ACl.Count;
end;

function TEditorUses.IndentacaoDeLinha(ACl: TClausulaUses): string;
var
  i: integer;
begin
  for i := ACl.Count - 1 downto 0 do
    if SoEspacosAntes(ACl.Item(i).Inicio) then
      Exit(FTexto.Indentacao(FTexto.LinhaDoOffset(ACl.Item(i).Inicio)));
  Result := '  ';
end;

function TEditorUses.IndentacaoContinuacao(ACl: TClausulaUses): string;
var
  vLinhaPalavra, vLinhaUltimo: integer;
begin
  vLinhaPalavra := FTexto.LinhaDoOffset(ACl.PalavraInicio);
  vLinhaUltimo := FTexto.LinhaDoOffset(ACl.Item(ACl.Count - 1).Inicio);
  if vLinhaUltimo <> vLinhaPalavra then
    Result := FTexto.Indentacao(vLinhaUltimo)
  else
    Result := FTexto.Indentacao(vLinhaPalavra) + '  ';
end;

{ Acrescentar }

// after the item and the {comments} that trail it (the `{Form1}` of the IDE)
function TEditorUses.PontoDeInsercao(AItem: TItemUses): integer;
var
  q: integer;
begin
  Result := AItem.Fim;
  repeat
    q := ProximoNaoEspaco(Result);
    if (q < Length(FTexto.Bytes)) and (FTexto.Bytes[q] = '{') and
      (FTexto.Bytes[q + 1] <> '$') then
    begin
      while (q <= Length(FTexto.Bytes)) and (FTexto.Bytes[q] <> '}') do
        Inc(q);
      if q > Length(FTexto.Bytes) then
        Break;
      Result := q + 1;
    end
    else
      Break;
  until False;
end;

procedure TEditorUses.AcrescentarEm(ACl: TClausulaUses; const ATexto: string);
var
  vRef: TItemUses;
  i: integer;
  vNovo, vIndent: string;
begin
  vRef := ACl.Item(ACl.Count - 1);
  for i := ACl.Count - 1 downto 0 do
    if not ACl.Item(i).DentroIfDef then
    begin
      vRef := ACl.Item(i);
      Break;
    end;
  if PorLinha(ACl) then
    vNovo := ',' + FTexto.FimDeLinha + IndentacaoDeLinha(ACl) + ATexto
  else
  begin
    if ACl.ComprimentoAtual < 0 then
      ACl.ComprimentoAtual := PontoDeInsercao(vRef) -
        FTexto.LinhaInicio(FTexto.LinhaDoOffset(PontoDeInsercao(vRef)));
    if ACl.ComprimentoAtual + 2 + Length(ATexto) + 1 > CLargura then
    begin
      vIndent := IndentacaoContinuacao(ACl);
      vNovo := ',' + FTexto.FimDeLinha + vIndent + ATexto;
      ACl.ComprimentoAtual := Length(vIndent) + Length(ATexto);
    end
    else
    begin
      vNovo := ', ' + ATexto;
      Inc(ACl.ComprimentoAtual, 2 + Length(ATexto));
    end;
  end;
  FTexto.Inserir(PontoDeInsercao(vRef), vNovo);
end;

// line (1-based) of the token that ends the header of the section
function TEditorUses.LinhaDaAncora(ASecao: TSecaoUses): integer;
var
  i: integer;
  vAlvo: string;
begin
  Result := 0;
  case ASecao of
    suInterface: vAlvo := 'interface';
    suImplementation: vAlvo := 'implementation';
  else
    vAlvo := '';
  end;
  for i := 0 to FTokens.Count - 1 do
  begin
    if vAlvo <> '' then
    begin
      if FTokens.EhNomeIgual(i, vAlvo) and (FTokens[i].Tipo = ttPalavraChave) and
        not ((FTokens.Anterior(i) >= 0) and FTokens.EhSimbolo(FTokens.Anterior(i), '=')) then
        Exit(FTexto.LinhaDoOffset(FTokens[i].Inicio));
    end
    else if (FTokens.EhNomeIgual(i, 'program') or FTokens.EhNomeIgual(i, 'library')) and
      (FTokens.Anterior(i) < 0) then
    begin
      Result := i;
      while (Result < FTokens.Count) and not FTokens.EhSimbolo(Result, ';') do
        Inc(Result);
      if Result < FTokens.Count then
        Exit(FTexto.LinhaDoOffset(FTokens[Result].Inicio));
      Exit(0);
    end;
  end;
end;

// line of the `contains`/`end` that a new requires/contains goes before
function TEditorUses.LinhaAntesDoFim(ASecao: TSecaoUses): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FTokens.Count - 1 do
    if (ASecao = suRequires) and FTokens.EhNomeIgual(i, 'contains') then
      Exit(FTexto.LinhaDoOffset(FTokens[i].Inicio));
  for i := FTokens.Count - 1 downto 0 do
    if FTokens.EhNomeIgual(i, 'end') then
      Exit(FTexto.LinhaDoOffset(FTokens[i].Inicio));
end;

procedure TEditorUses.CriarClausula(ASecao: TSecaoUses; const ATexto: string);
var
  vLinha: integer;
  vPalavra: string;
begin
  case ASecao of
    suRequires: vPalavra := 'requires';
    suContains: vPalavra := 'contains';
  else
    vPalavra := 'uses';
  end;
  if ASecao in [suRequires, suContains] then
  begin
    vLinha := LinhaAntesDoFim(ASecao);
    if vLinha = 0 then
      raise EUsesErro.Create(SSemSecao);
    FTexto.InserirAntesDaLinha(vLinha, vPalavra + #10'  ' + ATexto + ';'#10);
  end
  else
  begin
    vLinha := LinhaDaAncora(ASecao);
    if vLinha = 0 then
      raise EUsesErro.Create(SSemSecao);
    FTexto.InserirAntesDaLinha(vLinha + 1, #10 + vPalavra + #10'  ' + ATexto + ';'#10);
  end;
end;

procedure TEditorUses.Acrescentar(const AUnit: string; ASecao: TSecaoUses;
  const AArquivo: string);
var
  vCl: TClausulaUses;
  vTexto: string;
begin
  if ContemEm(AUnit, ASecao) then
    Exit;
  if (ASecao = suImplementation) and ContemEm(AUnit, suInterface) then
    Exit;
  vTexto := AUnit;
  if AArquivo <> '' then
    vTexto := vTexto + ' in ''' + AArquivo + '''';
  vCl := Clausula(ASecao);
  if (vCl <> nil) and (vCl.Count > 0) then
    AcrescentarEm(vCl, vTexto)
  else if vCl <> nil then
    raise EUsesErro.CreateFmt(SSemPontoEVirgula, [FTexto.LinhaDoOffset(vCl.PalavraInicio)])
  else
    CriarClausula(ASecao, vTexto);
end;

{ Tirar }

procedure TEditorUses.TirarClausulaInteira(ACl: TClausulaUses);
var
  vIni, vFim: integer;
begin
  if ACl.PontoEVirgula = 0 then
    raise EUsesErro.CreateFmt(SSemPontoEVirgula, [FTexto.LinhaDoOffset(ACl.PalavraInicio)]);
  vIni := ACl.PalavraInicio;
  vFim := ACl.PontoEVirgula + 1;
  if SoEspacosAntes(vIni) and SoEspacosDepois(vFim) then
    FTexto.RemoverLinhas(FTexto.LinhaDoOffset(vIni), FTexto.LinhaDoOffset(vFim - 1))
  else
    FTexto.AdicionarEdicao(vIni, vFim - vIni, '');
end;

// gap between two items holds nothing but blanks and the comma at AVirgula
function TEditorUses.ApenasVirgulaEEspaco(AIni, AFim, AVirgula: integer): boolean;
var
  i: integer;
begin
  Result := (AVirgula >= AIni) and (AVirgula < AFim);
  if not Result then
    Exit;
  for i := AIni to AFim - 1 do
    if (i <> AVirgula) and not (FTexto.Bytes[i] in [' ', #9, #10, #13]) then
      Exit(False);
end;

// items ADe..APara (consecutive) are removed and an item follows them
procedure TEditorUses.TirarGrupoNoMeio(ACl: TClausulaUses; ADe, APara: integer);
var
  vIni, vFim: integer;
begin
  vIni := ACl.Item(ADe).Inicio;
  vFim := ACl.Item(APara).Virgula + 1;
  if SoEspacosDepois(vFim) then
  begin
    if SoEspacosAntes(vIni) then
    begin
      FTexto.RemoverLinhas(FTexto.LinhaDoOffset(vIni), FTexto.LinhaDoOffset(vFim - 1));
      Exit;
    end;
    vIni := AnteriorNaoEspaco(vIni);
  end
  else
    vFim := ProximoNaoEspaco(vFim);
  FTexto.AdicionarEdicao(vIni, vFim - vIni, '');
end;

// items ADe..APara are the end of the clause
procedure TEditorUses.TirarGrupoNoFim(ACl: TClausulaUses; ADe, APara: integer);
var
  vAnt: TItemUses;
begin
  vAnt := ACl.Item(ADe - 1);
  if ApenasVirgulaEEspaco(vAnt.Fim, ACl.Item(ADe).Inicio, vAnt.Virgula) then
    FTexto.AdicionarEdicao(vAnt.Fim, ACl.Item(APara).Fim - vAnt.Fim, '')
  else
  begin
    FTexto.AdicionarEdicao(vAnt.Virgula, 1, '');
    FTexto.AdicionarEdicao(ACl.Item(ADe).Inicio,
      ACl.Item(APara).Fim - ACl.Item(ADe).Inicio, '');
  end;
end;

procedure TEditorUses.TirarGrupo(ACl: TClausulaUses; ADe, APara: integer);
begin
  if APara < ACl.Count - 1 then
    TirarGrupoNoMeio(ACl, ADe, APara)
  else
    TirarGrupoNoFim(ACl, ADe, APara);
end;

function TEditorUses.TirarVarias(ANomes: TStrings): integer;
var
  i, j, vDe, vAte: integer;
  vCl: TClausulaUses;
  vMarca: array of boolean;

  function Tira(AIdx: integer): boolean;
  begin
    Result := vMarca[AIdx];
  end;

begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    vCl := Clausulas[i];
    vMarca := nil;
    SetLength(vMarca, vCl.Count);
    vDe := 0;
    for j := 0 to vCl.Count - 1 do
    begin
      vMarca[j] := ANomes.IndexOf(vCl.Item(j).Nome) >= 0;
      if vMarca[j] then
        Inc(vDe);
    end;
    if vDe = 0 then
      Continue;
    Inc(Result, vDe);
    if vDe = vCl.Count then
    begin
      TirarClausulaInteira(vCl);
      Continue;
    end;
    j := 0;
    while j < vCl.Count do
    begin
      if not Tira(j) then
      begin
        Inc(j);
        Continue;
      end;
      vAte := j;
      while (vAte + 1 < vCl.Count) and Tira(vAte + 1) and
        ApenasVirgulaEEspaco(vCl.Item(vAte).Fim, vCl.Item(vAte + 1).Inicio,
        vCl.Item(vAte).Virgula) do
        Inc(vAte);
      TirarGrupo(vCl, j, vAte);
      j := vAte + 1;
    end;
  end;
end;

function TEditorUses.Tirar(const AUnit: string): integer;
var
  vLista: TStringList;
begin
  vLista := TStringList.Create;
  try
    vLista.CaseSensitive := False;
    vLista.Add(AUnit);
    Result := TirarVarias(vLista);
  finally
    vLista.Free;
  end;
end;

function TEditorUses.Trocar(const AVelha, ANova: string): integer;
var
  i, j: integer;
  vItem: TItemUses;
  vRemover: TStringList;
begin
  Result := 0;
  vRemover := TStringList.Create;
  try
    vRemover.CaseSensitive := False;
    for i := 0 to Count - 1 do
      for j := 0 to Clausulas[i].Count - 1 do
      begin
        vItem := Clausulas[i].Item(j);
        if not SameText(vItem.Nome, AVelha) then
          Continue;
        if vItem.Nome = ANova then
        begin
          Inc(Result);
          Continue;
        end;
        if ContemEm(ANova, Clausulas[i].Secao) then
          vRemover.Add(AVelha)
        else
          FTexto.AdicionarEdicao(vItem.NomeInicio, vItem.NomeTamanho, ANova);
        Inc(Result);
      end;
    if vRemover.Count > 0 then
      TirarVarias(vRemover);
  finally
    vRemover.Free;
  end;
end;

end.
