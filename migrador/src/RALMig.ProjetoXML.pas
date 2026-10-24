/// Project files in XML (.dproj, .lpi, .lpk) read and edited in place. There
/// is no DOM that writes the file again: the reader only *locates* elements and
/// attributes in the text (offsets), and an edit replaces the bytes of one
/// value or inserts one sibling, so the rest of the file leaves identical.
///
/// Paths are element names separated by '/', starting at the root element
/// ('Project/PropertyGroup/DCC_UsePackage'); a path that starts with '//'
/// starts at any element ('//CompilerOptions/SearchPaths/OtherUnitFiles'); the
/// segment '*' is any name. A search returns *all* elements that match.
unit RALMig.ProjetoXML;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  RALMig.Texto;

type
  /// The XML is not well formed.
  EXmlInvalido = class(Exception);

  TElementoXML = class;

  /// An attribute: where its value sits (between the quotes).
  TAtributoXML = class
  public
    Nome: string;
    /// The value as written (still escaped)
    ValorBruto: string;
    ValorInicio: integer;
    ValorTamanho: integer;
    /// The quote character: " or '
    Aspa: char;
    /// Offset just after the closing quote
    function Fim: integer;
  end;

  /// An element with its place in the text.
  TElementoXML = class
  private
    FAtributos: TObjectList;
    FFilhos: TObjectList;
    function GetAtributo(AIndex: integer): TAtributoXML;
    function GetAtributoCount: integer;
    function GetFilho(AIndex: integer): TElementoXML;
    function GetFilhoCount: integer;
  public
    Nome: string;
    Pai: TElementoXML;
    /// Offset of the `<` and just after the `>` of the start tag
    InicioTag: integer;
    FimTag: integer;
    /// True for <a/>; BarraFim is then the offset of the `/` of `/>`
    AutoFechado: boolean;
    BarraFim: integer;
    /// Offset of the `</` and just after the `>` of the end tag (0 when
    /// self-closed)
    FechaInicio: integer;
    FechaFim: integer;
    /// Blanks at the start of the line when the element opens it, else ''
    Indentacao: string;
    constructor Create;
    destructor Destroy; override;
    /// Offset just after the whole element
    function Fim: integer;
    function Atributo(const ANome: string): TAtributoXML;
    function Filho(const ANome: string): TElementoXML;
    property AtributoCount: integer read GetAtributoCount;
    property Atributos[AIndex: integer]: TAtributoXML read GetAtributo;
    property FilhoCount: integer read GetFilhoCount;
    property Filhos[AIndex: integer]: TElementoXML read GetFilho;
  end;

  /// The elements of a project file and the operations over them.
  TProjetoXML = class
  private
    FRaiz: TElementoXML;
    FTexto: TTextoOriginal;
    procedure Descer(AElementos: TList; const ANome: string; AResultado: TList);
    procedure Todos(AElemento: TElementoXML; AResultado: TList);
    function PassoDoItem(AItem: TElementoXML): integer;
    function IndentacaoDeFilho(APai: TElementoXML): string;
  public
    constructor Create(ATexto: TTextoOriginal);
    destructor Destroy; override;
    /// Reads the elements again from Texto.Bytes. Raises EXmlInvalido.
    procedure Ler;
    /// All elements that match the path; the caller frees the list
    function Buscar(const ACaminho: string): TList;
    /// The first element that matches the path; nil if none
    function Primeiro(const ACaminho: string): TElementoXML;
    /// Text content, unescaped (UTF-8)
    function ValorTexto(AElemento: TElementoXML): string;
    /// Value of the attribute, unescaped; '' when it does not exist
    function ValorAtributo(AElemento: TElementoXML; const ANome: string): string;
    /// Replaces the text content (escaped here)
    procedure TrocarTexto(AElemento: TElementoXML; const ANovo: string);
    /// Replaces the value of the attribute (escaped here); adds the attribute
    /// when the element does not have it
    procedure TrocarAtributo(AElemento: TElementoXML; const ANome, ANovo: string);
    /// Inserts the XML text (one element, may have several lines) after the
    /// element, with the same indentation
    procedure InserirIrmaoDepois(AElemento: TElementoXML; const AXml: string);
    /// Inserts the XML text as the last child of the element
    procedure InserirFilho(APai: TElementoXML; const AXml: string);
    /// Packages of a RequiredPackages (.lpi) or RequiredPkgs (.lpk) list
    procedure ListarPacotes(AContainer: TElementoXML; AResultado: TStrings);
    /// Appends ItemN+1/PackageName (or a plain Item when the list has no Count)
    /// to the list and updates Count; false (and no
    /// edit) when the package is already there
    function AcrescentarPacote(AContainer: TElementoXML; const APacote: string): boolean;
    /// The list of required packages of an .lpi or .lpk; nil if none
    function ContainerDePacotes: TElementoXML;
    property Raiz: TElementoXML read FRaiz;
    property Texto: TTextoOriginal read FTexto;
  end;

/// Escapes & < > and " for XML
function EscaparXml(const ATexto: string): string;

/// Resolves the five entities and &#nn; / &#xhh; to UTF-8
function DecodificarXml(const ATexto: string): string;

implementation

resourcestring
  SXmlSemFim = 'XML: the element "%s" (line %d) is never closed';
  SXmlTagErrada = 'XML: line %d closes "%s" but "%s" was open';
  SXmlMalFormado = 'XML: malformed text at line %d';
  SXmlConteudo = 'XML: the element "%s" has content that is not text';

function EscaparXml(const ATexto: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(ATexto) do
    case ATexto[i] of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
    else
      Result := Result + ATexto[i];
    end;
end;

function CodigoUtf8(ACodigo: cardinal): string;
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

function EntidadeNumerica(const ANome: string; out AValor: string): boolean;
var
  vNum: integer;
begin
  Result := False;
  if (Length(ANome) < 2) or (ANome[1] <> '#') then
    Exit;
  if ANome[2] in ['x', 'X'] then
    Result := TryStrToInt('$' + Copy(ANome, 3, MaxInt), vNum)
  else
    Result := TryStrToInt(Copy(ANome, 2, MaxInt), vNum);
  if Result then
    AValor := CodigoUtf8(vNum);
end;

function DecodificarXml(const ATexto: string): string;
var
  i, vFim: integer;
  vNome, vValor: string;
begin
  Result := '';
  i := 1;
  while i <= Length(ATexto) do
  begin
    if ATexto[i] <> '&' then
    begin
      Result := Result + ATexto[i];
      Inc(i);
      Continue;
    end;
    vFim := i + 1;
    while (vFim <= Length(ATexto)) and (ATexto[vFim] <> ';') and (vFim - i < 12) do
      Inc(vFim);
    vNome := Copy(ATexto, i + 1, vFim - i - 1);
    if (vFim > Length(ATexto)) or (ATexto[vFim] <> ';') then
      vValor := ''
    else if vNome = 'amp' then
      vValor := '&'
    else if vNome = 'lt' then
      vValor := '<'
    else if vNome = 'gt' then
      vValor := '>'
    else if vNome = 'quot' then
      vValor := '"'
    else if vNome = 'apos' then
      vValor := ''''
    else if not EntidadeNumerica(vNome, vValor) then
      vValor := '';
    if vValor = '' then
    begin
      Result := Result + ATexto[i];
      Inc(i);
    end
    else
    begin
      Result := Result + vValor;
      i := vFim + 1;
    end;
  end;
end;

{ TAtributoXML }

function TAtributoXML.Fim: integer;
begin
  Result := ValorInicio + ValorTamanho + 1;
end;

{ TElementoXML }

constructor TElementoXML.Create;
begin
  inherited Create;
  FAtributos := TObjectList.Create(True);
  FFilhos := TObjectList.Create(True);
end;

destructor TElementoXML.Destroy;
begin
  FAtributos.Free;
  FFilhos.Free;
  inherited Destroy;
end;

function TElementoXML.GetAtributo(AIndex: integer): TAtributoXML;
begin
  Result := TAtributoXML(FAtributos[AIndex]);
end;

function TElementoXML.GetAtributoCount: integer;
begin
  Result := FAtributos.Count;
end;

function TElementoXML.GetFilho(AIndex: integer): TElementoXML;
begin
  Result := TElementoXML(FFilhos[AIndex]);
end;

function TElementoXML.GetFilhoCount: integer;
begin
  Result := FFilhos.Count;
end;

function TElementoXML.Fim: integer;
begin
  if AutoFechado then
    Result := FimTag
  else
    Result := FechaFim;
end;

function TElementoXML.Atributo(const ANome: string): TAtributoXML;
var
  i: integer;
begin
  for i := 0 to AtributoCount - 1 do
    if Atributos[i].Nome = ANome then
      Exit(Atributos[i]);
  Result := nil;
end;

function TElementoXML.Filho(const ANome: string): TElementoXML;
var
  i: integer;
begin
  for i := 0 to FilhoCount - 1 do
    if Filhos[i].Nome = ANome then
      Exit(Filhos[i]);
  Result := nil;
end;

{ scanner }

type
  TLeitorXml = class
  private
    FTexto: TTextoOriginal;
    FBytes: AnsiString;
    FLen: integer;
    FPos: integer;
    FRaiz: TElementoXML;
    FPilha: TList;
    function PularAte(const AFim: string): boolean;
    procedure PularEspaco;
    function LerNome: string;
    procedure LerAtributos(AElem: TElementoXML);
    procedure AbrirTag;
    procedure FecharTag;
    function IndentacaoDe(AOffset: integer): string;
    procedure Erro(const AFmt: string; const AArgs: array of const);
  public
    constructor Create(ATexto: TTextoOriginal);
    destructor Destroy; override;
    function Executar: TElementoXML;
  end;

constructor TLeitorXml.Create(ATexto: TTextoOriginal);
begin
  inherited Create;
  FTexto := ATexto;
  FBytes := ATexto.Bytes;
  FLen := Length(FBytes);
  FPos := ATexto.InicioConteudo;
  FPilha := TList.Create;
end;

destructor TLeitorXml.Destroy;
begin
  FPilha.Free;
  FRaiz.Free;
  inherited Destroy;
end;

procedure TLeitorXml.Erro(const AFmt: string; const AArgs: array of const);
begin
  raise EXmlInvalido.CreateFmt(AFmt, AArgs);
end;

function TLeitorXml.PularAte(const AFim: string): boolean;
var
  vAchou: integer;
begin
  vAchou := Pos(AFim, Copy(FBytes, FPos, MaxInt));
  Result := vAchou > 0;
  if Result then
    Inc(FPos, vAchou - 1 + Length(AFim))
  else
    FPos := FLen + 1;
end;

procedure TLeitorXml.PularEspaco;
begin
  while (FPos <= FLen) and (FBytes[FPos] in [' ', #9, #10, #13]) do
    Inc(FPos);
end;

function TLeitorXml.LerNome: string;
var
  vIni: integer;
begin
  vIni := FPos;
  while (FPos <= FLen) and not (FBytes[FPos] in [' ', #9, #10, #13, '/', '>', '=', '<']) do
    Inc(FPos);
  Result := Copy(FBytes, vIni, FPos - vIni);
end;

function TLeitorXml.IndentacaoDe(AOffset: integer): string;
var
  vLinha, vIni: integer;
begin
  Result := '';
  vLinha := FTexto.LinhaDoOffset(AOffset);
  vIni := FTexto.LinhaInicio(vLinha);
  if (AOffset - vIni = Length(FTexto.Indentacao(vLinha))) then
    Result := FTexto.Indentacao(vLinha);
end;

procedure TLeitorXml.LerAtributos(AElem: TElementoXML);
var
  vAtr: TAtributoXML;
  vAspa: char;
  vIni: integer;
begin
  repeat
    PularEspaco;
    if (FPos > FLen) or (FBytes[FPos] in ['>', '/']) then
      Break;
    vAtr := TAtributoXML.Create;
    AElem.FAtributos.Add(vAtr);
    vAtr.Nome := LerNome;
    PularEspaco;
    if (FPos > FLen) or (FBytes[FPos] <> '=') then
      Erro(SXmlMalFormado, [FTexto.LinhaDoOffset(FPos)]);
    Inc(FPos);
    PularEspaco;
    if (FPos > FLen) or not (FBytes[FPos] in ['"', '''']) then
      Erro(SXmlMalFormado, [FTexto.LinhaDoOffset(FPos)]);
    vAspa := FBytes[FPos];
    Inc(FPos);
    vIni := FPos;
    while (FPos <= FLen) and (FBytes[FPos] <> vAspa) do
      Inc(FPos);
    if FPos > FLen then
      Erro(SXmlMalFormado, [FTexto.LinhaDoOffset(vIni)]);
    vAtr.Aspa := vAspa;
    vAtr.ValorInicio := vIni;
    vAtr.ValorTamanho := FPos - vIni;
    vAtr.ValorBruto := Copy(FBytes, vIni, FPos - vIni);
    Inc(FPos);
  until False;
end;

procedure TLeitorXml.AbrirTag;
var
  vElem, vPai: TElementoXML;
begin
  vElem := TElementoXML.Create;
  vElem.InicioTag := FPos;
  vElem.Indentacao := IndentacaoDe(FPos);
  Inc(FPos);
  vElem.Nome := LerNome;
  if vElem.Nome = '' then
  begin
    vElem.Free;
    Erro(SXmlMalFormado, [FTexto.LinhaDoOffset(FPos)]);
  end;
  if FPilha.Count > 0 then
  begin
    vPai := TElementoXML(FPilha[FPilha.Count - 1]);
    vElem.Pai := vPai;
    vPai.FFilhos.Add(vElem);
  end
  else if FRaiz = nil then
    FRaiz := vElem
  else
  begin
    vElem.Free;
    Erro(SXmlMalFormado, [FTexto.LinhaDoOffset(FPos)]);
  end;
  LerAtributos(vElem);
  if (FPos <= FLen) and (FBytes[FPos] = '/') then
  begin
    vElem.AutoFechado := True;
    vElem.BarraFim := FPos;
    Inc(FPos);
  end;
  if (FPos > FLen) or (FBytes[FPos] <> '>') then
    Erro(SXmlMalFormado, [FTexto.LinhaDoOffset(FPos)]);
  Inc(FPos);
  vElem.FimTag := FPos;
  if not vElem.AutoFechado then
    FPilha.Add(vElem);
end;

procedure TLeitorXml.FecharTag;
var
  vElem: TElementoXML;
  vIni: integer;
  vNome: string;
begin
  vIni := FPos;
  Inc(FPos, 2);
  vNome := LerNome;
  PularEspaco;
  if (FPos > FLen) or (FBytes[FPos] <> '>') then
    Erro(SXmlMalFormado, [FTexto.LinhaDoOffset(vIni)]);
  Inc(FPos);
  if FPilha.Count = 0 then
    Erro(SXmlMalFormado, [FTexto.LinhaDoOffset(vIni)]);
  vElem := TElementoXML(FPilha[FPilha.Count - 1]);
  if vElem.Nome <> vNome then
    Erro(SXmlTagErrada, [FTexto.LinhaDoOffset(vIni), vNome, vElem.Nome]);
  vElem.FechaInicio := vIni;
  vElem.FechaFim := FPos;
  FPilha.Delete(FPilha.Count - 1);
end;

function TLeitorXml.Executar: TElementoXML;
begin
  while FPos <= FLen do
  begin
    if FBytes[FPos] <> '<' then
      Inc(FPos)
    else if Copy(FBytes, FPos, 4) = '<!--' then
      PularAte('-->')
    else if Copy(FBytes, FPos, 9) = '<![CDATA[' then
      PularAte(']]>')
    else if Copy(FBytes, FPos, 2) = '<?' then
      PularAte('?>')
    else if Copy(FBytes, FPos, 2) = '<!' then
      PularAte('>')
    else if Copy(FBytes, FPos, 2) = '</' then
      FecharTag
    else
      AbrirTag;
  end;
  if FPilha.Count > 0 then
    Erro(SXmlSemFim, [TElementoXML(FPilha[FPilha.Count - 1]).Nome,
      FTexto.LinhaDoOffset(TElementoXML(FPilha[FPilha.Count - 1]).InicioTag)]);
  if FRaiz = nil then
    Erro(SXmlMalFormado, [1]);
  Result := FRaiz;
  FRaiz := nil;
end;

{ TProjetoXML }

constructor TProjetoXML.Create(ATexto: TTextoOriginal);
begin
  inherited Create;
  FTexto := ATexto;
  Ler;
end;

destructor TProjetoXML.Destroy;
begin
  FRaiz.Free;
  inherited Destroy;
end;

procedure TProjetoXML.Ler;
var
  vLeitor: TLeitorXml;
  vNova: TElementoXML;
begin
  vLeitor := TLeitorXml.Create(FTexto);
  try
    vNova := vLeitor.Executar;
  finally
    vLeitor.Free;
  end;
  FRaiz.Free;
  FRaiz := vNova;
end;

procedure TProjetoXML.Todos(AElemento: TElementoXML; AResultado: TList);
var
  i: integer;
begin
  AResultado.Add(AElemento);
  for i := 0 to AElemento.FilhoCount - 1 do
    Todos(AElemento.Filhos[i], AResultado);
end;

// replaces AElementos by the children that match the name
procedure TProjetoXML.Descer(AElementos: TList; const ANome: string; AResultado: TList);
var
  i, j: integer;
  vElem: TElementoXML;
begin
  for i := 0 to AElementos.Count - 1 do
  begin
    vElem := TElementoXML(AElementos[i]);
    for j := 0 to vElem.FilhoCount - 1 do
      if (ANome = '*') or (vElem.Filhos[j].Nome = ANome) then
        AResultado.Add(vElem.Filhos[j]);
  end;
end;

function TProjetoXML.Buscar(const ACaminho: string): TList;
var
  vPartes: TStringList;
  vAtual, vProx: TList;
  vCaminho: string;
  i, vPrimeiro: integer;
  vElem: TElementoXML;
begin
  Result := TList.Create;
  vPartes := TStringList.Create;
  vAtual := TList.Create;
  try
    vCaminho := ACaminho;
    vPartes.Delimiter := '/';
    vPartes.StrictDelimiter := True;
    if Copy(vCaminho, 1, 2) = '//' then
    begin
      Delete(vCaminho, 1, 2);
      vPartes.DelimitedText := vCaminho;
      Todos(FRaiz, vAtual);
      // keep only the elements that match the first segment
      for i := vAtual.Count - 1 downto 0 do
      begin
        vElem := TElementoXML(vAtual[i]);
        if (vPartes[0] <> '*') and (vElem.Nome <> vPartes[0]) then
          vAtual.Delete(i);
      end;
    end
    else
    begin
      vPartes.DelimitedText := vCaminho;
      if (vPartes.Count > 0) and ((vPartes[0] = '*') or (vPartes[0] = FRaiz.Nome)) then
        vAtual.Add(FRaiz);
    end;
    vPrimeiro := 1;
    for i := vPrimeiro to vPartes.Count - 1 do
    begin
      vProx := TList.Create;
      try
        Descer(vAtual, vPartes[i], vProx);
        vAtual.Assign(vProx);
      finally
        vProx.Free;
      end;
    end;
    Result.Assign(vAtual);
  finally
    vAtual.Free;
    vPartes.Free;
  end;
end;

function TProjetoXML.Primeiro(const ACaminho: string): TElementoXML;
var
  vLista: TList;
begin
  vLista := Buscar(ACaminho);
  try
    if vLista.Count > 0 then
      Result := TElementoXML(vLista[0])
    else
      Result := nil;
  finally
    vLista.Free;
  end;
end;

function TProjetoXML.ValorTexto(AElemento: TElementoXML): string;
begin
  if AElemento.AutoFechado then
    Exit('');
  Result := DecodificarXml(Copy(FTexto.Bytes, AElemento.FimTag,
    AElemento.FechaInicio - AElemento.FimTag));
end;

function TProjetoXML.ValorAtributo(AElemento: TElementoXML; const ANome: string): string;
var
  vAtr: TAtributoXML;
begin
  vAtr := AElemento.Atributo(ANome);
  if vAtr = nil then
    Result := ''
  else
    Result := DecodificarXml(vAtr.ValorBruto);
end;

{ edits }

procedure TProjetoXML.TrocarTexto(AElemento: TElementoXML; const ANovo: string);
var
  vConteudo: string;
  i: integer;
begin
  if AElemento.AutoFechado then
  begin
    FTexto.AdicionarEdicao(AElemento.BarraFim, 2,
      '>' + EscaparXml(ANovo) + '</' + AElemento.Nome + '>');
    Exit;
  end;
  vConteudo := Copy(FTexto.Bytes, AElemento.FimTag, AElemento.FechaInicio - AElemento.FimTag);
  for i := 1 to Length(vConteudo) do
    if vConteudo[i] = '<' then
      raise EXmlInvalido.CreateFmt(SXmlConteudo, [AElemento.Nome]);
  FTexto.AdicionarEdicao(AElemento.FimTag, AElemento.FechaInicio - AElemento.FimTag,
    EscaparXml(ANovo));
end;

procedure TProjetoXML.TrocarAtributo(AElemento: TElementoXML; const ANome, ANovo: string);
var
  vAtr: TAtributoXML;
  vPos: integer;
begin
  vAtr := AElemento.Atributo(ANome);
  if vAtr <> nil then
  begin
    FTexto.AdicionarEdicao(vAtr.ValorInicio, vAtr.ValorTamanho, EscaparXml(ANovo));
    Exit;
  end;
  if AElemento.AtributoCount > 0 then
    vPos := AElemento.Atributos[AElemento.AtributoCount - 1].Fim
  else
    vPos := AElemento.InicioTag + 1 + Length(AElemento.Nome);
  FTexto.Inserir(vPos, ' ' + ANome + '="' + EscaparXml(ANovo) + '"');
end;

function TProjetoXML.IndentacaoDeFilho(APai: TElementoXML): string;
var
  i: integer;
begin
  for i := APai.FilhoCount - 1 downto 0 do
    if APai.Filhos[i].Indentacao <> '' then
      Exit(APai.Filhos[i].Indentacao);
  Result := APai.Indentacao + '  ';
end;

// the lines of AXml after the first get the indentation AIndent
function Indentar(const AXml, AIndent, AEol: string): string;
var
  vLinhas: TStringList;
  i: integer;
begin
  vLinhas := TStringList.Create;
  try
    vLinhas.Text := StringReplace(AXml, #13#10, #10, [rfReplaceAll]);
    Result := '';
    for i := 0 to vLinhas.Count - 1 do
    begin
      if i > 0 then
        Result := Result + AEol + AIndent;
      Result := Result + vLinhas[i];
    end;
  finally
    vLinhas.Free;
  end;
end;

procedure TProjetoXML.InserirIrmaoDepois(AElemento: TElementoXML; const AXml: string);
var
  vIndent: string;
begin
  vIndent := AElemento.Indentacao;
  FTexto.Inserir(AElemento.Fim,
    FTexto.FimDeLinha + vIndent + Indentar(AXml, vIndent, FTexto.FimDeLinha));
end;

procedure TProjetoXML.InserirFilho(APai: TElementoXML; const AXml: string);
var
  vIndent: string;
  vEol: string;
begin
  vEol := FTexto.FimDeLinha;
  if APai.FilhoCount > 0 then
  begin
    InserirIrmaoDepois(APai.Filhos[APai.FilhoCount - 1], AXml);
    Exit;
  end;
  vIndent := IndentacaoDeFilho(APai);
  if APai.AutoFechado then
    FTexto.AdicionarEdicao(APai.BarraFim, 2,
      '>' + vEol + vIndent + Indentar(AXml, vIndent, vEol) + vEol + APai.Indentacao +
      '</' + APai.Nome + '>')
  else
    FTexto.AdicionarEdicao(APai.FimTag, APai.FechaInicio - APai.FimTag,
      vEol + vIndent + Indentar(AXml, vIndent, vEol) + vEol + APai.Indentacao);
end;

{ packages }

function TProjetoXML.ContainerDePacotes: TElementoXML;
begin
  Result := Primeiro('//RequiredPackages');
  if Result = nil then
    Result := Primeiro('//RequiredPkgs');
end;

procedure TProjetoXML.ListarPacotes(AContainer: TElementoXML; AResultado: TStrings);
var
  i: integer;
  vNome: TElementoXML;
begin
  for i := 0 to AContainer.FilhoCount - 1 do
  begin
    vNome := AContainer.Filhos[i].Filho('PackageName');
    if vNome <> nil then
      AResultado.Add(ValorAtributo(vNome, 'Value'));
  end;
end;

// 'Item3' -> 3; 0 when it is not an item
function TProjetoXML.PassoDoItem(AItem: TElementoXML): integer;
begin
  Result := 0;
  if Copy(AItem.Nome, 1, 4) = 'Item' then
    Result := StrToIntDef(Copy(AItem.Nome, 5, MaxInt), 0);
end;

function TProjetoXML.AcrescentarPacote(AContainer: TElementoXML;
  const APacote: string): boolean;
var
  vLista: TStringList;
  vNovo, vMax, i: integer;
  vPasso, vXml, vNome: string;
  vUltimo: TElementoXML;
  vNumerado: boolean;
begin
  vLista := TStringList.Create;
  try
    ListarPacotes(AContainer, vLista);
    vLista.CaseSensitive := False;
    Result := vLista.IndexOf(APacote) < 0;
  finally
    vLista.Free;
  end;
  if not Result then
    Exit;
  vMax := 0;
  vUltimo := nil;
  vNumerado := AContainer.Atributo('Count') <> nil;
  for i := 0 to AContainer.FilhoCount - 1 do
    if (AContainer.Filhos[i].Nome = 'Item') or (PassoDoItem(AContainer.Filhos[i]) > 0) then
    begin
      vUltimo := AContainer.Filhos[i];
      if PassoDoItem(vUltimo) > vMax then
        vMax := PassoDoItem(vUltimo);
    end;
  // Lazarus writes <Item1> plus Count, or (newer) a plain <Item> without Count;
  // the new item follows the style the list already has
  vNumerado := vNumerado or (vMax > 0);
  vNovo := vMax + 1;
  vNome := 'Item';
  if vNumerado then
    vNome := 'Item' + IntToStr(vNovo);
  vPasso := '  ';
  if (vUltimo <> nil) and (vUltimo.FilhoCount > 0) and (vUltimo.Filhos[0].Indentacao <> '') then
    vPasso := Copy(vUltimo.Filhos[0].Indentacao, Length(vUltimo.Indentacao) + 1, MaxInt);
  vXml := '<' + vNome + '>' + #10 + vPasso + '<PackageName Value="' +
    EscaparXml(APacote) + '"/>' + #10 + '</' + vNome + '>';
  if vUltimo <> nil then
    InserirIrmaoDepois(vUltimo, vXml)
  else
    InserirFilho(AContainer, vXml);
  if vNumerado then
    TrocarAtributo(AContainer, 'Count', IntToStr(vNovo));
end;

end.
