/// Reader and editor of form files in text (.dfm, .lfm, .fmx): a tree of
/// object/inherited/inline nodes with their properties, built over the original
/// lines. Nothing is rebuilt: every operation adds an edit to the TTextoOriginal
/// that touches only the lines it names, so the rest of the file leaves
/// byte for byte. The tree always describes the original; after a round of
/// edits call Texto.Consolidar and Ler to see the edited form.
///
/// A form that does not parse (an object without `end`, a value that never
/// closes) raises EFormularioInvalido with the line; nothing is "fixed".
unit RALMig.Formulario;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  RALMig.Texto;

type
  TTipoNo = (tnObject, tnInherited, tnInline);

  /// Raised when the text is not a form that can be analyzed.
  EFormularioInvalido = class(Exception)
  private
    FLinha: integer;
  public
    constructor CriarNaLinha(ALinha: integer; const AMensagem: string);
    /// Line of the problem (1-based)
    property Linha: integer read FLinha;
  end;

  TNoFormulario = class;

  /// One property of an object. Valor spans one or more lines.
  TPropriedadeForm = class
  public
    /// Full name with dots ('Font.Name', 'Items.Strings')
    Nome: string;
    NomeInicio: integer;
    NomeTamanho: integer;
    /// The value as written, with its original line breaks
    ValorBruto: string;
    /// Offset of the value and just after it (trailing blanks excluded)
    ValorInicio: integer;
    ValorFim: integer;
    LinhaInicio: integer;
    LinhaFim: integer;
    Pai: TNoFormulario;
    /// ( ... ) string list
    function EhLista: boolean;
    /// < ... > collection
    function EhColecao: boolean;
    /// { ... } binary data
    function EhBinario: boolean;
    /// [ ... ] set
    function EhConjunto: boolean;
    /// 'text' / #nnn, possibly joined by +
    function EhTexto: boolean;
    function MultiLinha: boolean;
  end;

  /// An object/inherited/inline of the form.
  TNoFormulario = class
  private
    FFilhos: TObjectList;
    FPropriedades: TObjectList;
    function GetFilho(AIndex: integer): TNoFormulario;
    function GetFilhoCount: integer;
    function GetPropriedade(AIndex: integer): TPropriedadeForm;
    function GetPropriedadeCount: integer;
  public
    Tipo: TTipoNo;
    Nome: string;
    Classe: string;
    /// The `[n]` that follows the class in .lfm ('' when there is none)
    Indice: string;
    NomeInicio: integer;
    NomeTamanho: integer;
    ClasseInicio: integer;
    ClasseTamanho: integer;
    /// Line of the `object` and of its `end`
    LinhaInicio: integer;
    LinhaFim: integer;
    /// Blanks at the start of the `object` line
    Indentacao: string;
    Pai: TNoFormulario;
    constructor Create;
    destructor Destroy; override;
    /// The property with that full name (no case); nil if none
    function Propriedade(const ANome: string): TPropriedadeForm;
    /// The child object with that name (no case); nil if none
    function Filho(const ANome: string): TNoFormulario;
    /// Value of the property as written; '' when it does not exist
    function Valor(const ANome: string): string;
    property FilhoCount: integer read GetFilhoCount;
    property Filhos[AIndex: integer]: TNoFormulario read GetFilho;
    property PropriedadeCount: integer read GetPropriedadeCount;
    property Propriedades[AIndex: integer]: TPropriedadeForm read GetPropriedade;
  end;

  /// The tree of a form file and the operations over it.
  TFormulario = class
  private
    FRaizes: TObjectList;
    FTexto: TTextoOriginal;
    function GetRaiz: TNoFormulario;
    function GetRaizCount: integer;
    function GetRaizItem(AIndex: integer): TNoFormulario;
    function ProcurarNo(ANo: TNoFormulario; const ANome: string): TNoFormulario;
    procedure ColetarClasse(ANo: TNoFormulario; const AClasse: string; ALista: TList);
    function LerCabecalho(ALinha: integer; APai: TNoFormulario): TNoFormulario;
    procedure LerObjeto(var ALinha: integer; APai: TNoFormulario);
    procedure LerPropriedade(var ALinha: integer; ANo: TNoFormulario);
    function FimDoValor(var ALinha: integer; var APos: integer): integer;
    function PrimeiroNaoEspaco(ALinha: integer): integer;
    function EhInicioDeObjeto(ALinha: integer): boolean;
    function EhFim(ALinha: integer): boolean;
    function IndentacaoDosFilhos(APai: TNoFormulario; ADepoisDe: TNoFormulario): string;
    function IndentarTexto(const ATexto, AIndent: string): string;
  public
    constructor Create(ATexto: TTextoOriginal);
    destructor Destroy; override;
    /// Reads the form again from Texto.Bytes. Raises EFormularioInvalido.
    procedure Ler;
    /// Finds an object by name (no case) in all of the tree
    function AcharObjeto(const ANome: string): TNoFormulario;
    /// The objects of the class (no case); the caller frees the list
    function ObjetosDaClasse(const AClasse: string): TList;
    /// Removes the property with all of its lines
    procedure TirarPropriedade(AProp: TPropriedadeForm);
    /// Replaces the value (UTF-8; line breaks are converted to the file's)
    procedure TrocarValor(AProp: TPropriedadeForm; const ANovoValor: string);
    /// Renames the property (full name)
    procedure RenomearPropriedade(AProp: TPropriedadeForm; const ANovoNome: string);
    /// Changes the class in `object Name: TClass`
    procedure TrocarClasse(ANo: TNoFormulario; const ANovaClasse: string);
    /// Changes the name in `object Name: TClass`
    procedure RenomearObjeto(ANo: TNoFormulario; const ANovoNome: string);
    /// Removes the object with all of its lines
    procedure TirarObjeto(ANo: TNoFormulario);
    /// Adds the property text ('Name = value', maybe several lines, without
    /// indentation) after the last property of the object
    procedure InserirPropriedade(ANo: TNoFormulario; const ATexto: string);
    /// Inserts an object (its text arrives without indentation, any line
    /// break) as a child of APai, after the sibling ADepoisDe; nil sibling =
    /// last child. APai = nil inserts at the end of the file.
    procedure InserirObjeto(APai, ADepoisDe: TNoFormulario; const ATextoDoObjeto: string);
    property Raiz: TNoFormulario read GetRaiz;
    property RaizCount: integer read GetRaizCount;
    property Raizes[AIndex: integer]: TNoFormulario read GetRaizItem;
    property Texto: TTextoOriginal read FTexto;
  end;

/// True when the bytes are a binary form (starts with TPF0)
function EhFormularioBinario(const ABytes: RawByteString): boolean;

/// Converts the binary form held by ATexto to text (ObjectBinaryToText) and
/// makes the text the new original. False when it was not binary.
function ConverterBinarioParaTexto(ATexto: TTextoOriginal): boolean;

/// The Pascal literal the IDE writes for the text: quoted ASCII and #nnn for
/// everything else (control characters and non-ASCII, as UTF-16 units). With
/// AUtf8Direto (.lfm) non-ASCII goes in the quotes as UTF-8.
function ValorStringForm(const AUtf8: string; AUtf8Direto: boolean = False): string;

/// Decodes a string value of a form ('abc'#233'd' + 'e') to UTF-8; false when
/// the value is not a string literal.
function ValorComoTexto(const AValor: string; out ATexto: string): boolean;

implementation

resourcestring
  SSemFim = 'Form line %d: the object "%s" has no matching "end"';
  SNaoEhPropriedade = 'Form line %d: this is not a property (no "=")';
  SValorSemFim = 'Form line %d: the value of the property "%s" never ends';
  SCabecalhoInvalido = 'Form line %d: unexpected text outside of an object';

{ EFormularioInvalido }

constructor EFormularioInvalido.CriarNaLinha(ALinha: integer; const AMensagem: string);
begin
  inherited Create(AMensagem);
  FLinha := ALinha;
end;

{ TPropriedadeForm }

function TPropriedadeForm.EhLista: boolean;
begin
  Result := Copy(ValorBruto, 1, 1) = '(';
end;

function TPropriedadeForm.EhColecao: boolean;
begin
  Result := Copy(ValorBruto, 1, 1) = '<';
end;

function TPropriedadeForm.EhBinario: boolean;
begin
  Result := Copy(ValorBruto, 1, 1) = '{';
end;

function TPropriedadeForm.EhConjunto: boolean;
begin
  Result := Copy(ValorBruto, 1, 1) = '[';
end;

function TPropriedadeForm.EhTexto: boolean;
begin
  Result := (ValorBruto <> '') and (ValorBruto[1] in ['''', '#']);
end;

function TPropriedadeForm.MultiLinha: boolean;
begin
  Result := LinhaFim > LinhaInicio;
end;

{ TNoFormulario }

constructor TNoFormulario.Create;
begin
  inherited Create;
  FFilhos := TObjectList.Create(True);
  FPropriedades := TObjectList.Create(True);
end;

destructor TNoFormulario.Destroy;
begin
  FFilhos.Free;
  FPropriedades.Free;
  inherited Destroy;
end;

function TNoFormulario.GetFilho(AIndex: integer): TNoFormulario;
begin
  Result := TNoFormulario(FFilhos[AIndex]);
end;

function TNoFormulario.GetFilhoCount: integer;
begin
  Result := FFilhos.Count;
end;

function TNoFormulario.GetPropriedade(AIndex: integer): TPropriedadeForm;
begin
  Result := TPropriedadeForm(FPropriedades[AIndex]);
end;

function TNoFormulario.GetPropriedadeCount: integer;
begin
  Result := FPropriedades.Count;
end;

function TNoFormulario.Propriedade(const ANome: string): TPropriedadeForm;
var
  i: integer;
begin
  for i := 0 to PropriedadeCount - 1 do
    if SameText(Propriedades[i].Nome, ANome) then
      Exit(Propriedades[i]);
  Result := nil;
end;

function TNoFormulario.Filho(const ANome: string): TNoFormulario;
var
  i: integer;
begin
  for i := 0 to FilhoCount - 1 do
    if SameText(Filhos[i].Nome, ANome) then
      Exit(Filhos[i]);
  Result := nil;
end;

function TNoFormulario.Valor(const ANome: string): string;
var
  vProp: TPropriedadeForm;
begin
  vProp := Propriedade(ANome);
  if vProp = nil then
    Result := ''
  else
    Result := vProp.ValorBruto;
end;

{ helpers }

function EhFormularioBinario(const ABytes: RawByteString): boolean;
begin
  Result := (Length(ABytes) >= 4) and (ABytes[1] = 'T') and (ABytes[2] = 'P') and
    (ABytes[3] = 'F') and (ABytes[4] in ['0'..'9']);
end;

function ConverterBinarioParaTexto(ATexto: TTextoOriginal): boolean;
var
  vEntrada, vSaida: TMemoryStream;
  vBytes: AnsiString;
begin
  Result := EhFormularioBinario(ATexto.Bytes);
  if not Result then
    Exit;
  vBytes := '';
  vEntrada := TMemoryStream.Create;
  vSaida := TMemoryStream.Create;
  try
    vEntrada.WriteBuffer(ATexto.Bytes[1], Length(ATexto.Bytes));
    vEntrada.Position := 0;
    ObjectBinaryToText(vEntrada, vSaida);
    SetLength(vBytes, vSaida.Size);
    if vSaida.Size > 0 then
      Move(vSaida.Memory^, vBytes[1], vSaida.Size);
    ATexto.RedefinirOriginal(vBytes);
  finally
    vSaida.Free;
    vEntrada.Free;
  end;
end;

// next code point of a UTF-8 string; invalid bytes come out as themselves
function ProximoCodigo(const AUtf8: string; var APos: integer): cardinal;
var
  vB: byte;
  vResto, i: integer;
begin
  vB := Ord(AUtf8[APos]);
  Inc(APos);
  if vB < $80 then
    Exit(vB);
  if (vB >= $C2) and (vB <= $DF) then
  begin
    Result := vB and $1F;
    vResto := 1;
  end
  else if (vB >= $E0) and (vB <= $EF) then
  begin
    Result := vB and $0F;
    vResto := 2;
  end
  else if (vB >= $F0) and (vB <= $F4) then
  begin
    Result := vB and $07;
    vResto := 3;
  end
  else
    Exit(vB);
  for i := 1 to vResto do
    if (APos <= Length(AUtf8)) and ((Ord(AUtf8[APos]) and $C0) = $80) then
    begin
      Result := (Result shl 6) or (Ord(AUtf8[APos]) and $3F);
      Inc(APos);
    end;
end;

function CodigoParaUtf8(ACodigo: cardinal): string;
begin
  if ACodigo < $80 then
    Result := Chr(ACodigo)
  else if ACodigo < $800 then
    Result := Chr($C0 or (ACodigo shr 6)) + Chr($80 or (ACodigo and $3F))
  else if ACodigo < $10000 then
    Result := Chr($E0 or (ACodigo shr 12)) + Chr($80 or ((ACodigo shr 6) and $3F)) +
      Chr($80 or (ACodigo and $3F))
  else
    Result := Chr($F0 or (ACodigo shr 18)) + Chr($80 or ((ACodigo shr 12) and $3F)) +
      Chr($80 or ((ACodigo shr 6) and $3F)) + Chr($80 or (ACodigo and $3F));
end;

function ValorStringForm(const AUtf8: string; AUtf8Direto: boolean): string;
var
  i, vIni: integer;
  vCodigo: cardinal;
  vAberto: boolean;
  procedure Unidade(AUnidade: cardinal);
  begin
    if vAberto then
    begin
      Result := Result + '''';
      vAberto := False;
    end;
    Result := Result + '#' + IntToStr(AUnidade);
  end;
begin
  Result := '';
  vAberto := False;
  i := 1;
  while i <= Length(AUtf8) do
  begin
    vIni := i;
    vCodigo := ProximoCodigo(AUtf8, i);
    if (vCodigo >= 32) and (vCodigo <= 126) or (AUtf8Direto and (vCodigo >= 160)) then
    begin
      if not vAberto then
      begin
        Result := Result + '''';
        vAberto := True;
      end;
      if vCodigo = 39 then
        Result := Result + ''''''
      else
        Result := Result + Copy(AUtf8, vIni, i - vIni);
    end
    else if vCodigo >= $10000 then
    begin
      Unidade($D800 + ((vCodigo - $10000) shr 10));
      Unidade($DC00 + ((vCodigo - $10000) and $3FF));
    end
    else
      Unidade(vCodigo);
  end;
  if vAberto then
    Result := Result + ''''
  else if Result = '' then
    Result := '''''';
end;

function ValorComoTexto(const AValor: string; out ATexto: string): boolean;
var
  i, vLen: integer;
  vNum: cardinal;
  vAlto: cardinal;
begin
  ATexto := '';
  Result := False;
  vLen := Length(AValor);
  i := 1;
  vAlto := 0;
  while i <= vLen do
  begin
    case AValor[i] of
      ' ', #9, #10, #13, '+': Inc(i);
      '''':
        begin
          Inc(i);
          while i <= vLen do
          begin
            if AValor[i] = '''' then
            begin
              if (i < vLen) and (AValor[i + 1] = '''') then
              begin
                ATexto := ATexto + '''';
                Inc(i, 2);
                Continue;
              end;
              Break;
            end;
            ATexto := ATexto + AValor[i];
            Inc(i);
          end;
          if i > vLen then
            Exit;
          Inc(i);
        end;
      '#':
        begin
          Inc(i);
          vNum := 0;
          if (i <= vLen) and (AValor[i] = '$') then
          begin
            Inc(i);
            while (i <= vLen) and (AValor[i] in ['0'..'9', 'A'..'F', 'a'..'f']) do
            begin
              vNum := vNum * 16 + cardinal(StrToInt('$' + AValor[i]));
              Inc(i);
            end;
          end
          else
            while (i <= vLen) and (AValor[i] in ['0'..'9']) do
            begin
              vNum := vNum * 10 + cardinal(Ord(AValor[i]) - 48);
              Inc(i);
            end;
          if (vNum >= $D800) and (vNum <= $DBFF) then
            vAlto := vNum
          else
          begin
            if (vNum >= $DC00) and (vNum <= $DFFF) and (vAlto <> 0) then
              vNum := $10000 + ((vAlto - $D800) shl 10) + (vNum - $DC00);
            vAlto := 0;
            ATexto := ATexto + CodigoParaUtf8(vNum);
          end;
        end;
    else
      Exit;
    end;
  end;
  Result := True;
end;

{ TFormulario }

constructor TFormulario.Create(ATexto: TTextoOriginal);
begin
  inherited Create;
  FTexto := ATexto;
  FRaizes := TObjectList.Create(True);
  Ler;
end;

destructor TFormulario.Destroy;
begin
  FRaizes.Free;
  inherited Destroy;
end;

function TFormulario.GetRaiz: TNoFormulario;
begin
  if FRaizes.Count > 0 then
    Result := TNoFormulario(FRaizes[0])
  else
    Result := nil;
end;

function TFormulario.GetRaizCount: integer;
begin
  Result := FRaizes.Count;
end;

function TFormulario.GetRaizItem(AIndex: integer): TNoFormulario;
begin
  Result := TNoFormulario(FRaizes[AIndex]);
end;

// offset of the first byte of the line that is not a blank
function TFormulario.PrimeiroNaoEspaco(ALinha: integer): integer;
begin
  Result := FTexto.LinhaInicio(ALinha) + Length(FTexto.Indentacao(ALinha));
end;

function TFormulario.EhInicioDeObjeto(ALinha: integer): boolean;
var
  vIni, vFim, vPos: integer;
  vPalavra: string;
begin
  Result := False;
  vIni := PrimeiroNaoEspaco(ALinha);
  vFim := FTexto.LinhaFim(ALinha);
  vPos := vIni;
  while (vPos < vFim) and not (FTexto.Bytes[vPos] in [' ', #9]) do
    Inc(vPos);
  vPalavra := LowerCase(Copy(FTexto.Bytes, vIni, vPos - vIni));
  if (vPalavra <> 'object') and (vPalavra <> 'inherited') and (vPalavra <> 'inline') then
    Exit;
  while (vPos < vFim) and (FTexto.Bytes[vPos] in [' ', #9]) do
    Inc(vPos);
  Result := (vPos < vFim) and (FTexto.Bytes[vPos] <> '=');
end;

function TFormulario.EhFim(ALinha: integer): boolean;
var
  vTexto: string;
begin
  vTexto := LowerCase(Trim(FTexto.TextoDaLinha(ALinha)));
  Result := vTexto = 'end';
end;

// object Name: TClass [n]
function TFormulario.LerCabecalho(ALinha: integer; APai: TNoFormulario): TNoFormulario;
var
  vPos, vFim, vIni: integer;
  vPalavra: string;

  procedure PularEspaco;
  begin
    while (vPos < vFim) and (FTexto.Bytes[vPos] in [' ', #9]) do
      Inc(vPos);
  end;

  function Palavra(const AParar: string): string;
  var
    vComeco: integer;
  begin
    vComeco := vPos;
    while (vPos < vFim) and (Pos(FTexto.Bytes[vPos], AParar) = 0) do
      Inc(vPos);
    Result := Copy(FTexto.Bytes, vComeco, vPos - vComeco);
  end;

begin
  Result := TNoFormulario.Create;
  Result.Pai := APai;
  Result.LinhaInicio := ALinha;
  Result.Indentacao := FTexto.Indentacao(ALinha);
  vPos := PrimeiroNaoEspaco(ALinha);
  vFim := FTexto.LinhaFim(ALinha);
  vPalavra := LowerCase(Palavra(' '#9));
  if vPalavra = 'inherited' then
    Result.Tipo := tnInherited
  else if vPalavra = 'inline' then
    Result.Tipo := tnInline
  else
    Result.Tipo := tnObject;
  PularEspaco;
  vIni := vPos;
  Result.Nome := Trim(Palavra(': '#9));
  Result.NomeInicio := vIni;
  Result.NomeTamanho := Length(Result.Nome);
  PularEspaco;
  if (vPos < vFim) and (FTexto.Bytes[vPos] = ':') then
  begin
    Inc(vPos);
    PularEspaco;
    Result.ClasseInicio := vPos;
    Result.Classe := Palavra(' '#9'[');
  end
  else
  begin
    // `object TClass` without a name
    Result.Classe := Result.Nome;
    Result.ClasseInicio := Result.NomeInicio;
    Result.Nome := '';
    Result.NomeTamanho := 0;
  end;
  Result.ClasseTamanho := Length(Result.Classe);
  PularEspaco;
  Result.Indice := Trim(Copy(FTexto.Bytes, vPos, vFim - vPos));
end;

// scans a value from APos on ALinha across lines; returns the last line
function TFormulario.FimDoValor(var ALinha: integer; var APos: integer): integer;
var
  vProf: integer;
  vTexto: boolean;
  vFim, vUltimo: integer;
  vC: char;
begin
  vProf := 0;
  vTexto := False;
  repeat
    vFim := FTexto.LinhaFim(ALinha);
    vUltimo := 0;
    while APos < vFim do
    begin
      vC := FTexto.Bytes[APos];
      if vTexto then
        vTexto := vC <> ''''
      else
        case vC of
          '''': vTexto := True;
          '(', '[', '<', '{': Inc(vProf);
          ')', ']', '>', '}': if vProf > 0 then Dec(vProf);
        end;
      if not (vC in [' ', #9]) then
        vUltimo := Ord(vC);
      Inc(APos);
    end;
    vTexto := False;
    if (vProf = 0) and (vUltimo <> Ord('+')) then
      Break;
    Inc(ALinha);
    if ALinha > FTexto.LinhaCount then
    begin
      Result := -1;
      Exit;
    end;
    APos := FTexto.LinhaInicio(ALinha);
  until False;
  Result := ALinha;
end;

procedure TFormulario.LerPropriedade(var ALinha: integer; ANo: TNoFormulario);
var
  vProp: TPropriedadeForm;
  vIni, vFim, vIgual, vPos, vLinha, vUltima, vFimValor: integer;
begin
  vIni := PrimeiroNaoEspaco(ALinha);
  vFim := FTexto.LinhaFim(ALinha);
  vIgual := vIni;
  while (vIgual < vFim) and (FTexto.Bytes[vIgual] <> '=') do
    Inc(vIgual);
  if vIgual >= vFim then
    raise EFormularioInvalido.CriarNaLinha(ALinha, Format(SNaoEhPropriedade, [ALinha]));
  vProp := TPropriedadeForm.Create;
  ANo.FPropriedades.Add(vProp);
  vProp.Pai := ANo;
  vProp.Nome := Trim(Copy(FTexto.Bytes, vIni, vIgual - vIni));
  vProp.NomeInicio := vIni;
  vProp.NomeTamanho := Length(vProp.Nome);
  vProp.LinhaInicio := ALinha;
  vPos := vIgual + 1;
  while (vPos < vFim) and (FTexto.Bytes[vPos] in [' ', #9]) do
    Inc(vPos);
  vLinha := ALinha;
  if vPos >= vFim then
  begin
    // `Caption = ` with the value on the next line
    Inc(vLinha);
    if vLinha > FTexto.LinhaCount then
      raise EFormularioInvalido.CriarNaLinha(ALinha,
        Format(SValorSemFim, [ALinha, vProp.Nome]));
    vPos := PrimeiroNaoEspaco(vLinha);
  end;
  vProp.ValorInicio := vPos;
  vUltima := FimDoValor(vLinha, vPos);
  if vUltima < 0 then
    raise EFormularioInvalido.CriarNaLinha(ALinha,
      Format(SValorSemFim, [ALinha, vProp.Nome]));
  vFimValor := FTexto.LinhaFim(vUltima);
  while (vFimValor > vProp.ValorInicio) and (FTexto.Bytes[vFimValor - 1] in [' ', #9]) do
    Dec(vFimValor);
  vProp.ValorFim := vFimValor;
  vProp.LinhaFim := vUltima;
  vProp.ValorBruto := Copy(FTexto.Bytes, vProp.ValorInicio, vFimValor - vProp.ValorInicio);
  ALinha := vUltima + 1;
end;

procedure TFormulario.LerObjeto(var ALinha: integer; APai: TNoFormulario);
var
  vNo: TNoFormulario;
begin
  vNo := LerCabecalho(ALinha, APai);
  if APai = nil then
    FRaizes.Add(vNo)
  else
    APai.FFilhos.Add(vNo);
  Inc(ALinha);
  while ALinha <= FTexto.LinhaCount do
  begin
    if Trim(FTexto.TextoDaLinha(ALinha)) = '' then
      Inc(ALinha)
    else if EhFim(ALinha) then
    begin
      vNo.LinhaFim := ALinha;
      Inc(ALinha);
      Exit;
    end
    else if EhInicioDeObjeto(ALinha) then
      LerObjeto(ALinha, vNo)
    else
      LerPropriedade(ALinha, vNo);
  end;
  raise EFormularioInvalido.CriarNaLinha(vNo.LinhaInicio,
    Format(SSemFim, [vNo.LinhaInicio, vNo.Nome]));
end;

procedure TFormulario.Ler;
var
  vLinha: integer;
begin
  FRaizes.Clear;
  vLinha := 1;
  while vLinha <= FTexto.LinhaCount do
  begin
    if Trim(FTexto.TextoDaLinha(vLinha)) = '' then
      Inc(vLinha)
    else if EhInicioDeObjeto(vLinha) then
      LerObjeto(vLinha, nil)
    else
      raise EFormularioInvalido.CriarNaLinha(vLinha, Format(SCabecalhoInvalido, [vLinha]));
  end;
end;

{ queries }

function TFormulario.ProcurarNo(ANo: TNoFormulario; const ANome: string): TNoFormulario;
var
  i: integer;
begin
  if SameText(ANo.Nome, ANome) then
    Exit(ANo);
  for i := 0 to ANo.FilhoCount - 1 do
  begin
    Result := ProcurarNo(ANo.Filhos[i], ANome);
    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

function TFormulario.AcharObjeto(const ANome: string): TNoFormulario;
var
  i: integer;
begin
  for i := 0 to RaizCount - 1 do
  begin
    Result := ProcurarNo(Raizes[i], ANome);
    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

procedure TFormulario.ColetarClasse(ANo: TNoFormulario; const AClasse: string;
  ALista: TList);
var
  i: integer;
begin
  if SameText(ANo.Classe, AClasse) then
    ALista.Add(ANo);
  for i := 0 to ANo.FilhoCount - 1 do
    ColetarClasse(ANo.Filhos[i], AClasse, ALista);
end;

function TFormulario.ObjetosDaClasse(const AClasse: string): TList;
var
  i: integer;
begin
  Result := TList.Create;
  for i := 0 to RaizCount - 1 do
    ColetarClasse(Raizes[i], AClasse, Result);
end;

{ edits }

procedure TFormulario.TirarPropriedade(AProp: TPropriedadeForm);
begin
  FTexto.RemoverLinhas(AProp.LinhaInicio, AProp.LinhaFim);
end;

procedure TFormulario.TrocarValor(AProp: TPropriedadeForm; const ANovoValor: string);
begin
  FTexto.AdicionarEdicao(AProp.ValorInicio, AProp.ValorFim - AProp.ValorInicio,
    FTexto.NormalizarFimDeLinha(ANovoValor));
end;

procedure TFormulario.RenomearPropriedade(AProp: TPropriedadeForm; const ANovoNome: string);
begin
  FTexto.AdicionarEdicao(AProp.NomeInicio, AProp.NomeTamanho, ANovoNome);
end;

procedure TFormulario.TrocarClasse(ANo: TNoFormulario; const ANovaClasse: string);
begin
  FTexto.AdicionarEdicao(ANo.ClasseInicio, ANo.ClasseTamanho, ANovaClasse);
end;

procedure TFormulario.RenomearObjeto(ANo: TNoFormulario; const ANovoNome: string);
begin
  FTexto.AdicionarEdicao(ANo.NomeInicio, ANo.NomeTamanho, ANovoNome);
end;

procedure TFormulario.TirarObjeto(ANo: TNoFormulario);
begin
  FTexto.RemoverLinhas(ANo.LinhaInicio, ANo.LinhaFim);
end;

function TFormulario.IndentarTexto(const ATexto, AIndent: string): string;
var
  vLinhas: TStringList;
  i: integer;
begin
  vLinhas := TStringList.Create;
  try
    vLinhas.Text := StringReplace(ATexto, #13#10, #10, [rfReplaceAll]);
    Result := '';
    for i := 0 to vLinhas.Count - 1 do
    begin
      if vLinhas[i] <> '' then
        Result := Result + AIndent + vLinhas[i];
      Result := Result + #10;
    end;
  finally
    vLinhas.Free;
  end;
end;

procedure TFormulario.InserirPropriedade(ANo: TNoFormulario; const ATexto: string);
var
  vIndent: string;
  vLinha: integer;
begin
  if ANo.PropriedadeCount > 0 then
  begin
    vIndent := FTexto.Indentacao(ANo.Propriedades[0].LinhaInicio);
    vLinha := ANo.Propriedades[ANo.PropriedadeCount - 1].LinhaFim + 1;
  end
  else
  begin
    vIndent := ANo.Indentacao + '  ';
    vLinha := ANo.LinhaInicio + 1;
  end;
  FTexto.InserirAntesDaLinha(vLinha, IndentarTexto(ATexto, vIndent));
end;

function TFormulario.IndentacaoDosFilhos(APai, ADepoisDe: TNoFormulario): string;
begin
  if ADepoisDe <> nil then
    Result := ADepoisDe.Indentacao
  else if APai = nil then
    Result := ''
  else if APai.FilhoCount > 0 then
    Result := APai.Filhos[APai.FilhoCount - 1].Indentacao
  else if APai.PropriedadeCount > 0 then
    Result := FTexto.Indentacao(APai.Propriedades[0].LinhaInicio)
  else
    Result := APai.Indentacao + '  ';
end;

procedure TFormulario.InserirObjeto(APai, ADepoisDe: TNoFormulario;
  const ATextoDoObjeto: string);
var
  vIndent: string;
  vLinha: integer;
begin
  vIndent := IndentacaoDosFilhos(APai, ADepoisDe);
  if ADepoisDe <> nil then
    vLinha := ADepoisDe.LinhaFim + 1
  else if APai <> nil then
  begin
    if APai.FilhoCount > 0 then
      vLinha := APai.Filhos[APai.FilhoCount - 1].LinhaFim + 1
    else
      vLinha := APai.LinhaFim;
  end
  else
    vLinha := FTexto.LinhaCount + 1;
  FTexto.InserirAntesDaLinha(vLinha, IndentarTexto(ATextoDoObjeto, vIndent));
end;

end.
