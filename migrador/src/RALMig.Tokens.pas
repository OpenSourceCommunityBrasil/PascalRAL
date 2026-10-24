/// A tolerant Pascal tokenizer over the bytes of a source file. It never
/// raises: code that does not compile still gives tokens (what it does not
/// understand becomes a symbol). Every byte of the input belongs to exactly one
/// token, so the concatenation of the tokens is the input, and an edit can
/// replace just the bytes of a token.
unit RALMig.Tokens;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TTipoToken = (ttIdentificador, ttPalavraChave, ttNumero, ttString,
    ttComentario, ttDiretiva, ttSimbolo, ttEspaco);

  /// One token: its kind and where it is in the source (1-based offset).
  TToken = class
  public
    Tipo: TTipoToken;
    Inicio: integer;
    Tamanho: integer;
    /// Offset just after the token
    function Fim: integer;
  end;

  /// The tokens of a source, in order. Owns the tokens.
  TListaTokens = class
  private
    FFonte: AnsiString;
    FItens: TObjectList;
    function GetCount: integer;
    function GetItem(AIndex: integer): TToken;
  public
    constructor Create(const AFonte: RawByteString);
    destructor Destroy; override;
    procedure Adicionar(ATipo: TTipoToken; AInicio, ATamanho: integer);
    /// The text of the token
    function Texto(AIndex: integer): string;
    /// True for identifiers and keywords (the tokens that carry a name)
    function EhNome(AIndex: integer): boolean;
    /// True when the token is an identifier or keyword equal to ANome (no case)
    function EhNomeIgual(AIndex: integer; const ANome: string): boolean;
    /// True when the token is the symbol ASimbolo
    function EhSimbolo(AIndex: integer; const ASimbolo: string): boolean;
    /// Space, comment or directive: what a reader skips
    function EhIgnoravel(AIndex: integer): boolean;
    /// Index of the next token that is not space/comment/directive, after
    /// AIndex; Count when there is none
    function Proximo(AIndex: integer): integer;
    /// Same, going backwards; -1 when there is none
    function Anterior(AIndex: integer): integer;
    /// Index of the first identifier (not a keyword, not in a comment, string
    /// or directive) called ANome (no case), from AIndex on; -1 if none
    function AcharIdentificador(const ANome: string; ADesde: integer = 0): integer;
    /// Index of the token that holds the offset; -1 if none
    function IndiceDoOffset(AOffset: integer): integer;
    /// First word of a directive token in upper case ('IFDEF', 'ENDIF', ...)
    function NomeDiretiva(AIndex: integer): string;
    /// What follows the first word of a directive token, trimmed
    function ArgumentoDiretiva(AIndex: integer): string;
    property Count: integer read GetCount;
    property Fonte: RawByteString read FFonte;
    property Itens[AIndex: integer]: TToken read GetItem; default;
  end;

/// Tokenizes the source from the offset AInicio (use the offset after the BOM).
function TokenizarPascal(const ABytes: RawByteString; AInicio: integer = 1): TListaTokens;

/// True when AWord is a reserved word of Object Pascal (no case)
function EhPalavraChave(const AWord: string): boolean;

implementation

const
  CPalavras: array[0..62] of string = (
    'and', 'array', 'as', 'asm', 'begin', 'case', 'class', 'const',
    'constructor', 'destructor', 'dispinterface', 'div', 'do', 'downto',
    'else', 'end', 'except', 'exports', 'file', 'finalization', 'finally',
    'for', 'function', 'goto', 'if', 'implementation', 'in', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library', 'mod',
    'nil', 'not', 'object', 'of', 'or', 'out', 'packed', 'procedure',
    'program', 'property', 'raise', 'record', 'repeat', 'resourcestring',
    'set', 'shl', 'shr', 'string', 'then', 'threadvar', 'to', 'try', 'type',
    'unit', 'until', 'uses', 'var', 'while');

var
  GPalavras: TStringList = nil;

function EhPalavraChave(const AWord: string): boolean;
var
  i: integer;
begin
  if GPalavras = nil then
  begin
    GPalavras := TStringList.Create;
    GPalavras.Sorted := True;
    GPalavras.CaseSensitive := False;
    for i := Low(CPalavras) to High(CPalavras) do
      GPalavras.Add(CPalavras[i]);
    GPalavras.Add('with');
    GPalavras.Add('xor');
  end;
  Result := GPalavras.IndexOf(AWord) >= 0;
end;

{ TToken }

function TToken.Fim: integer;
begin
  Result := Inicio + Tamanho;
end;

{ TListaTokens }

constructor TListaTokens.Create(const AFonte: RawByteString);
begin
  inherited Create;
  SetString(FFonte, PAnsiChar(Pointer(AFonte)), Length(AFonte));
  FItens := TObjectList.Create(True);
end;

destructor TListaTokens.Destroy;
begin
  FItens.Free;
  inherited Destroy;
end;

function TListaTokens.GetCount: integer;
begin
  Result := FItens.Count;
end;

function TListaTokens.GetItem(AIndex: integer): TToken;
begin
  Result := TToken(FItens[AIndex]);
end;

procedure TListaTokens.Adicionar(ATipo: TTipoToken; AInicio, ATamanho: integer);
var
  vTok: TToken;
begin
  vTok := TToken.Create;
  vTok.Tipo := ATipo;
  vTok.Inicio := AInicio;
  vTok.Tamanho := ATamanho;
  FItens.Add(vTok);
end;

function TListaTokens.Texto(AIndex: integer): string;
begin
  Result := Copy(FFonte, Itens[AIndex].Inicio, Itens[AIndex].Tamanho);
end;

function TListaTokens.EhNome(AIndex: integer): boolean;
begin
  Result := Itens[AIndex].Tipo in [ttIdentificador, ttPalavraChave];
end;

function TListaTokens.EhNomeIgual(AIndex: integer; const ANome: string): boolean;
begin
  Result := EhNome(AIndex) and SameText(Texto(AIndex), ANome);
end;

function TListaTokens.EhSimbolo(AIndex: integer; const ASimbolo: string): boolean;
begin
  Result := (Itens[AIndex].Tipo = ttSimbolo) and (Texto(AIndex) = ASimbolo);
end;

function TListaTokens.EhIgnoravel(AIndex: integer): boolean;
begin
  Result := Itens[AIndex].Tipo in [ttEspaco, ttComentario, ttDiretiva];
end;

function TListaTokens.Proximo(AIndex: integer): integer;
begin
  Result := AIndex + 1;
  while (Result < Count) and EhIgnoravel(Result) do
    Inc(Result);
end;

function TListaTokens.Anterior(AIndex: integer): integer;
begin
  Result := AIndex - 1;
  while (Result >= 0) and EhIgnoravel(Result) do
    Dec(Result);
end;

function TListaTokens.AcharIdentificador(const ANome: string; ADesde: integer): integer;
var
  i: integer;
begin
  for i := ADesde to Count - 1 do
    if (Itens[i].Tipo = ttIdentificador) and SameText(Texto(i), ANome) then
      Exit(i);
  Result := -1;
end;

function TListaTokens.IndiceDoOffset(AOffset: integer): integer;
var
  vIni, vFim, vMeio: integer;
begin
  Result := -1;
  vIni := 0;
  vFim := Count - 1;
  while vIni <= vFim do
  begin
    vMeio := (vIni + vFim) div 2;
    if AOffset < Itens[vMeio].Inicio then
      vFim := vMeio - 1
    else if AOffset >= Itens[vMeio].Fim then
      vIni := vMeio + 1
    else
      Exit(vMeio);
  end;
end;

function TListaTokens.ArgumentoDiretiva(AIndex: integer): string;
var
  vTexto: string;
  i, vFim: integer;
begin
  Result := '';
  if Itens[AIndex].Tipo <> ttDiretiva then
    Exit;
  vTexto := Texto(AIndex);
  // {$ ... } or (*$ ... *)
  if Copy(vTexto, 1, 1) = '{' then
    vTexto := Copy(vTexto, 3, Length(vTexto) - 3)
  else
    vTexto := Copy(vTexto, 4, Length(vTexto) - 5);
  vTexto := Trim(vTexto);
  i := 1;
  while (i <= Length(vTexto)) and not (vTexto[i] in [' ', #9, #13, #10]) do
    Inc(i);
  vFim := Length(vTexto);
  Result := Trim(Copy(vTexto, i, vFim - i + 1));
end;

function TListaTokens.NomeDiretiva(AIndex: integer): string;
var
  vTexto: string;
  i: integer;
begin
  Result := '';
  if Itens[AIndex].Tipo <> ttDiretiva then
    Exit;
  vTexto := Texto(AIndex);
  if Copy(vTexto, 1, 1) = '{' then
    vTexto := Copy(vTexto, 3, Length(vTexto) - 3)
  else
    vTexto := Copy(vTexto, 4, Length(vTexto) - 5);
  vTexto := Trim(vTexto);
  i := 1;
  while (i <= Length(vTexto)) and not (vTexto[i] in [' ', #9, #13, #10]) do
    Inc(i);
  Result := UpperCase(Copy(vTexto, 1, i - 1));
end;

{ tokenizer }

type
  TTokenizador = class
  private
    FFonte: AnsiString;
    FLen: integer;
    FPos: integer;
    FLista: TListaTokens;
    function Olhar(ADelta: integer = 0): char;
    procedure Emitir(ATipo: TTipoToken; AInicio: integer);
    procedure LerEspaco;
    procedure LerChave;
    procedure LerParenteses;
    procedure LerLinha;
    procedure LerString;
    procedure LerNumero;
    procedure LerIdentificador;
    procedure LerSimbolo;
    function PecaDeString: boolean;
  public
    constructor Create(const AFonte: RawByteString; AInicio: integer);
    destructor Destroy; override;
    function Executar: TListaTokens;
  end;

function EhLetra(C: char): boolean;
begin
  Result := (C in ['A'..'Z', 'a'..'z', '_']) or (C >= #128);
end;

function EhDigito(C: char): boolean;
begin
  Result := C in ['0'..'9'];
end;

function EhHex(C: char): boolean;
begin
  Result := C in ['0'..'9', 'A'..'F', 'a'..'f'];
end;

constructor TTokenizador.Create(const AFonte: RawByteString; AInicio: integer);
begin
  inherited Create;
  SetString(FFonte, PAnsiChar(Pointer(AFonte)), Length(AFonte));
  FLen := Length(AFonte);
  FPos := AInicio;
  FLista := TListaTokens.Create(AFonte);
end;

destructor TTokenizador.Destroy;
begin
  FLista.Free;
  inherited Destroy;
end;

function TTokenizador.Olhar(ADelta: integer): char;
begin
  if (FPos + ADelta >= 1) and (FPos + ADelta <= FLen) then
    Result := FFonte[FPos + ADelta]
  else
    Result := #0;
end;

procedure TTokenizador.Emitir(ATipo: TTipoToken; AInicio: integer);
begin
  FLista.Adicionar(ATipo, AInicio, FPos - AInicio);
end;

procedure TTokenizador.LerEspaco;
var
  vIni: integer;
begin
  vIni := FPos;
  while (FPos <= FLen) and (FFonte[FPos] in [#1..#32]) do
    Inc(FPos);
  Emitir(ttEspaco, vIni);
end;

procedure TTokenizador.LerChave;
var
  vIni: integer;
  vTipo: TTipoToken;
begin
  vIni := FPos;
  if Olhar(1) = '$' then
    vTipo := ttDiretiva
  else
    vTipo := ttComentario;
  while (FPos <= FLen) and (FFonte[FPos] <> '}') do
    Inc(FPos);
  if FPos <= FLen then
    Inc(FPos);
  Emitir(vTipo, vIni);
end;

procedure TTokenizador.LerParenteses;
var
  vIni: integer;
  vTipo: TTipoToken;
begin
  vIni := FPos;
  if Olhar(2) = '$' then
    vTipo := ttDiretiva
  else
    vTipo := ttComentario;
  Inc(FPos, 2);
  while (FPos <= FLen) and not ((FFonte[FPos] = '*') and (Olhar(1) = ')')) do
    Inc(FPos);
  if FPos <= FLen then
    Inc(FPos, 2);
  Emitir(vTipo, vIni);
end;

procedure TTokenizador.LerLinha;
var
  vIni: integer;
begin
  vIni := FPos;
  while (FPos <= FLen) and not (FFonte[FPos] in [#10, #13]) do
    Inc(FPos);
  Emitir(ttComentario, vIni);
end;

// A piece of a string literal starts at ', at # and, after a piece, at ^X
function TTokenizador.PecaDeString: boolean;
begin
  Result := False;
  case Olhar of
    '''':
      begin
        Inc(FPos);
        while FPos <= FLen do
        begin
          if FFonte[FPos] = '''' then
          begin
            if Olhar(1) = '''' then
              Inc(FPos, 2)
            else
            begin
              Inc(FPos);
              Break;
            end;
          end
          else if FFonte[FPos] in [#10, #13] then
            Break
          else
            Inc(FPos);
        end;
        Result := True;
      end;
    '#':
      begin
        Inc(FPos);
        if Olhar = '$' then
        begin
          Inc(FPos);
          while EhHex(Olhar) do
            Inc(FPos);
        end
        else
          while EhDigito(Olhar) do
            Inc(FPos);
        Result := True;
      end;
  end;
end;

procedure TTokenizador.LerString;
var
  vIni: integer;
begin
  vIni := FPos;
  repeat
    if not PecaDeString then
    begin
      // ^M is a control char only glued to a string
      if (Olhar = '^') and (Olhar(1) in ['@'..'_', 'a'..'z']) then
        Inc(FPos, 2)
      else
        Break;
    end;
  until False;
  Emitir(ttString, vIni);
end;

procedure TTokenizador.LerNumero;
var
  vIni: integer;
begin
  vIni := FPos;
  case Olhar of
    '$':
      begin
        Inc(FPos);
        while EhHex(Olhar) or (Olhar = '_') do
          Inc(FPos);
      end;
    '%':
      begin
        Inc(FPos);
        while Olhar in ['0', '1', '_'] do
          Inc(FPos);
      end;
    '&':
      begin
        Inc(FPos);
        while Olhar in ['0'..'7'] do
          Inc(FPos);
      end;
  else
    begin
      while EhDigito(Olhar) or (Olhar = '_') do
        Inc(FPos);
      if (Olhar = '.') and EhDigito(Olhar(1)) then
      begin
        Inc(FPos);
        while EhDigito(Olhar) do
          Inc(FPos);
      end;
      if (Olhar in ['e', 'E']) and (EhDigito(Olhar(1)) or
        ((Olhar(1) in ['+', '-']) and EhDigito(Olhar(2)))) then
      begin
        Inc(FPos, 2);
        while EhDigito(Olhar) do
          Inc(FPos);
      end;
    end;
  end;
  Emitir(ttNumero, vIni);
end;

procedure TTokenizador.LerIdentificador;
var
  vIni: integer;
  vTipo: TTipoToken;
begin
  vIni := FPos;
  if Olhar = '&' then
    Inc(FPos);
  while EhLetra(Olhar) or EhDigito(Olhar) do
    Inc(FPos);
  vTipo := ttIdentificador;
  if (FFonte[vIni] <> '&') and EhPalavraChave(Copy(FFonte, vIni, FPos - vIni)) then
    vTipo := ttPalavraChave;
  Emitir(vTipo, vIni);
end;

procedure TTokenizador.LerSimbolo;
var
  vIni: integer;
  vDois: string;
begin
  vIni := FPos;
  vDois := Olhar + Olhar(1);
  if (vDois = ':=') or (vDois = '<>') or (vDois = '<=') or (vDois = '>=') or
    (vDois = '..') or (vDois = '+=') or (vDois = '-=') or (vDois = '*=') or
    (vDois = '/=') or (vDois = '**') or (vDois = '><') or (vDois = '@@') then
    Inc(FPos, 2)
  else
    Inc(FPos);
  Emitir(ttSimbolo, vIni);
end;

function TTokenizador.Executar: TListaTokens;
var
  vC: char;
begin
  while FPos <= FLen do
  begin
    vC := FFonte[FPos];
    if vC in [#1..#32] then
      LerEspaco
    else if vC = '{' then
      LerChave
    else if (vC = '(') and (Olhar(1) = '*') then
      LerParenteses
    else if (vC = '/') and (Olhar(1) = '/') then
      LerLinha
    else if (vC = '''') or (vC = '#') then
      LerString
    else if EhDigito(vC) or ((vC = '$') and EhHex(Olhar(1))) or
      ((vC = '%') and (Olhar(1) in ['0', '1'])) or
      ((vC = '&') and (Olhar(1) in ['0'..'7'])) then
      LerNumero
    else if EhLetra(vC) or ((vC = '&') and EhLetra(Olhar(1))) then
      LerIdentificador
    else
      LerSimbolo;
  end;
  Result := FLista;
  FLista := nil;
end;

function TokenizarPascal(const ABytes: RawByteString; AInicio: integer): TListaTokens;
var
  vTok: TTokenizador;
begin
  vTok := TTokenizador.Create(ABytes, AInicio);
  try
    Result := vTok.Executar;
  finally
    vTok.Free;
  end;
end;

finalization
  GPalavras.Free;

end.
