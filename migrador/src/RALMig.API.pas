/// The public API of one RAL version: units, classes (with the published
/// members a form file can store), types, routines, constants, the classes on
/// the component palette and the packages with their units. It is read from the
/// sources with fcl-passrc and saved as a stable JSON (sorted keys, so a git
/// diff of two extractions shows only what changed). The comparison between two
/// versions lives in RALMig.Inventario.
unit RALMig.API;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, StrUtils, fpjson, jsonparser,
  RALInst.Catalogo;

type
  /// One member of a class (field, method or property).
  TApiMembro = class
  public
    Nome: string;
    /// 'published' or 'public'
    Cat: string;
    /// 'prop', 'metodo' or 'campo'
    Forma: string;
    /// Type of a property or field
    Tipo: string;
    /// Property: text of the default clause ('' when none or nodefault)
    Padrao: string;
    /// Property: stored clause
    Armazenado: string;
    /// Methods: one normalized signature per overload
    Assinaturas: TStringList;
    constructor Create(const ANome: string);
    destructor Destroy; override;
  end;

  /// A class, record or interface of the interface section of a unit.
  TApiClasse = class
  private
    FMembros: TStringList;
    FLeAntigos: TStringList;
  public
    Nome: string;
    Unidade: string;
    Ancestral: string;
    /// 'class', 'record', 'interface', 'object' or 'helper'
    Forma: string;
    constructor Create(const ANome: string);
    destructor Destroy; override;
    function Buscar(const ANome: string): TApiMembro;
    /// Finds or creates the member
    function Obter(const ANome: string): TApiMembro;
    property Membros: TStringList read FMembros;
    /// Names the class still accepts when a form is read (Filer.DefineProperty
    /// in its DefineProperties), though they are no longer published
    property LeAntigos: TStringList read FLeAntigos;
  end;

  /// A type that is not a class: enum, set, procedural type or anything else.
  TApiTipo = class
  public
    Nome: string;
    Unidade: string;
    /// 'enum', 'set', 'proc' or 'outro'
    Forma: string;
    /// Enum values, in declaration order
    Valores: TStringList;
    /// proc: normalized signature; set: 'set of X'; outro: the declaration
    Detalhe: string;
    constructor Create(const ANome: string);
    destructor Destroy; override;
  end;

  /// A routine of the interface section; overloads share the entry.
  TApiRotina = class
  public
    Unidade: string;
    Nome: string;
    Assinaturas: TStringList;
    constructor Create(const AUnidade, ANome: string);
    destructor Destroy; override;
  end;

  /// A constant of the interface section.
  TApiConstante = class
  public
    Unidade: string;
    Nome: string;
    Valor: string;
  end;

  /// A package of the catalog with its units (names, without extension).
  TApiPacote = class
  public
    /// 'delphi' or 'lazarus'
    Tipo: string;
    Nome: string;
    Unidades: TStringList;
    constructor Create(const ATipo, ANome: string);
    destructor Destroy; override;
  end;

  /// The API of one version.
  TApi = class
  private
    FClasses: TStringList;
    FConstantes: TStringList;
    FErros: TStringList;
    FPacotes: TObjectList;
    FRegistradas: TStringList;
    FRotinas: TStringList;
    FTipos: TStringList;
    FUnidades: TStringList;
    function GetPacote(AIndex: integer): TApiPacote;
    function GetPacoteCount: integer;
  public
    Rotulo: string;
    Origem: string;
    Commit: string;
    Data: string;
    constructor Create;
    destructor Destroy; override;
    function AddClasse(const ANome, AUnidade, AAncestral, AForma: string): TApiClasse;
    function AddConstante(const AUnidade, ANome, AValor: string): TApiConstante;
    function AddPacote(const ATipo, ANome: string): TApiPacote;
    function AddRotina(const AUnidade, ANome, AAssinatura: string): TApiRotina;
    function AddTipo(const ANome, AUnidade, AForma, ADetalhe: string): TApiTipo;
    procedure AddUnidade(const ANome, AArquivo: string);
    function BuscarClasse(const ANome: string): TApiClasse;
    function BuscarConstante(const AUnidade, ANome: string): TApiConstante;
    function BuscarPacote(const ATipo, ANome: string): TApiPacote;
    function BuscarRotina(const AUnidade, ANome: string): TApiRotina;
    function BuscarTipo(const ANome: string): TApiTipo;
    /// Published properties of the class and of its ancestors that are in this
    /// same API (what a form file can store); the list is sorted, owns the
    /// copies it holds and the caller creates it with CriarListaMembros
    procedure Publicadas(AClasse: TApiClasse; ARes: TStringList);
    /// Where the unit is: the file, '' when the API does not have it
    function ArquivoDaUnidade(const AUnidade: string): string;
    /// Folder of the unit, relative to the root, with / and no trailing /
    function PastaDaUnidade(const AUnidade: string): string;
    function ParaJSON: string;
    procedure SalvarArquivo(const AArquivo: string);
    procedure CarregarJSON(const ATexto: string);
    procedure CarregarArquivo(const AArquivo: string);

    property Classes: TStringList read FClasses;
    property Constantes: TStringList read FConstantes;
    property Erros: TStringList read FErros;
    property PacoteCount: integer read GetPacoteCount;
    property Pacotes[AIndex: integer]: TApiPacote read GetPacote;
    /// Class name = page of the palette
    property Registradas: TStringList read FRegistradas;
    property Rotinas: TStringList read FRotinas;
    property Tipos: TStringList read FTipos;
    /// Unit name = file, relative to the root
    property Unidades: TStringList read FUnidades;
  end;

/// A sorted list, case-insensitive, no duplicates
function NovaOrdenada(ADono: boolean): TStringList;
/// For Publicadas: an empty list that owns what it holds
function CriarListaMembros: TStringList;
/// Reads the source and returns its API. The catalog takes ownership of
/// AOrigem (it frees it with itself), so the caller must not use or free it
/// afterwards. A unit that does not parse goes to Erros and the run goes on.
function ExtrairAPI(AOrigem: TOrigemArquivos; const ARotulo: string): TApi;
/// Text of the RegisterComponents calls: class name = page
procedure AcharRegistros(const ATexto: string; ALista: TStrings);

implementation

uses
  PasTree, PScanner, PParser, RALInst.Zip;

function NovaOrdenada(ADono: boolean): TStringList;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  Result.OwnsObjects := ADono;
end;

function CriarListaMembros: TStringList;
begin
  Result := NovaOrdenada(True);
end;

{ TApiMembro }

constructor TApiMembro.Create(const ANome: string);
begin
  inherited Create;
  Nome := ANome;
  Assinaturas := NovaOrdenada(False);
end;

destructor TApiMembro.Destroy;
begin
  Assinaturas.Free;
  inherited Destroy;
end;

{ TApiClasse }

constructor TApiClasse.Create(const ANome: string);
begin
  inherited Create;
  Nome := ANome;
  Forma := 'class';
  FMembros := NovaOrdenada(True);
  FLeAntigos := NovaOrdenada(True);
end;

destructor TApiClasse.Destroy;
begin
  FLeAntigos.Free;
  FMembros.Free;
  inherited Destroy;
end;

function TApiClasse.Buscar(const ANome: string): TApiMembro;
var
  vIdx: integer;
begin
  vIdx := FMembros.IndexOf(ANome);
  if vIdx < 0 then
    Result := nil
  else
    Result := TApiMembro(FMembros.Objects[vIdx]);
end;

function TApiClasse.Obter(const ANome: string): TApiMembro;
begin
  Result := Buscar(ANome);
  if Result = nil then
  begin
    Result := TApiMembro.Create(ANome);
    FMembros.AddObject(ANome, Result);
  end;
end;

{ TApiTipo }

constructor TApiTipo.Create(const ANome: string);
begin
  inherited Create;
  Nome := ANome;
  Valores := TStringList.Create;
end;

destructor TApiTipo.Destroy;
begin
  Valores.Free;
  inherited Destroy;
end;

{ TApiRotina }

constructor TApiRotina.Create(const AUnidade, ANome: string);
begin
  inherited Create;
  Unidade := AUnidade;
  Nome := ANome;
  Assinaturas := NovaOrdenada(False);
end;

destructor TApiRotina.Destroy;
begin
  Assinaturas.Free;
  inherited Destroy;
end;

{ TApiPacote }

constructor TApiPacote.Create(const ATipo, ANome: string);
begin
  inherited Create;
  Tipo := ATipo;
  Nome := ANome;
  Unidades := NovaOrdenada(False);
end;

destructor TApiPacote.Destroy;
begin
  Unidades.Free;
  inherited Destroy;
end;

{ TApi }

constructor TApi.Create;
begin
  inherited Create;
  FClasses := NovaOrdenada(True);
  FConstantes := NovaOrdenada(True);
  FErros := TStringList.Create;
  FPacotes := TObjectList.Create(True);
  FRegistradas := NovaOrdenada(False);
  FRotinas := NovaOrdenada(True);
  FTipos := NovaOrdenada(True);
  FUnidades := NovaOrdenada(False);
  FUnidades.NameValueSeparator := '=';
end;

destructor TApi.Destroy;
begin
  FClasses.Free;
  FConstantes.Free;
  FErros.Free;
  FPacotes.Free;
  FRegistradas.Free;
  FRotinas.Free;
  FTipos.Free;
  FUnidades.Free;
  inherited Destroy;
end;

function TApi.GetPacote(AIndex: integer): TApiPacote;
begin
  Result := TApiPacote(FPacotes[AIndex]);
end;

function TApi.GetPacoteCount: integer;
begin
  Result := FPacotes.Count;
end;

function TApi.BuscarClasse(const ANome: string): TApiClasse;
var
  vIdx: integer;
begin
  vIdx := FClasses.IndexOf(ANome);
  if vIdx < 0 then
    Result := nil
  else
    Result := TApiClasse(FClasses.Objects[vIdx]);
end;

function TApi.AddClasse(const ANome, AUnidade, AAncestral, AForma: string): TApiClasse;
begin
  Result := BuscarClasse(ANome);
  if Result = nil then
  begin
    Result := TApiClasse.Create(ANome);
    FClasses.AddObject(ANome, Result);
  end;
  if Result.Unidade = '' then
    Result.Unidade := AUnidade;
  if Result.Ancestral = '' then
    Result.Ancestral := AAncestral;
  if AForma <> '' then
    Result.Forma := AForma;
end;

function TApi.BuscarTipo(const ANome: string): TApiTipo;
var
  vIdx: integer;
begin
  vIdx := FTipos.IndexOf(ANome);
  if vIdx < 0 then
    Result := nil
  else
    Result := TApiTipo(FTipos.Objects[vIdx]);
end;

function TApi.AddTipo(const ANome, AUnidade, AForma, ADetalhe: string): TApiTipo;
begin
  Result := BuscarTipo(ANome);
  if Result = nil then
  begin
    Result := TApiTipo.Create(ANome);
    Result.Unidade := AUnidade;
    Result.Forma := AForma;
    Result.Detalhe := ADetalhe;
    FTipos.AddObject(ANome, Result);
  end;
end;

function TApi.BuscarRotina(const AUnidade, ANome: string): TApiRotina;
var
  vIdx: integer;
begin
  vIdx := FRotinas.IndexOf(AUnidade + '.' + ANome);
  if vIdx < 0 then
    Result := nil
  else
    Result := TApiRotina(FRotinas.Objects[vIdx]);
end;

function TApi.AddRotina(const AUnidade, ANome, AAssinatura: string): TApiRotina;
begin
  Result := BuscarRotina(AUnidade, ANome);
  if Result = nil then
  begin
    Result := TApiRotina.Create(AUnidade, ANome);
    FRotinas.AddObject(AUnidade + '.' + ANome, Result);
  end;
  if AAssinatura <> '' then
    Result.Assinaturas.Add(AAssinatura);
end;

function TApi.BuscarConstante(const AUnidade, ANome: string): TApiConstante;
var
  vIdx: integer;
begin
  vIdx := FConstantes.IndexOf(AUnidade + '.' + ANome);
  if vIdx < 0 then
    Result := nil
  else
    Result := TApiConstante(FConstantes.Objects[vIdx]);
end;

function TApi.AddConstante(const AUnidade, ANome, AValor: string): TApiConstante;
begin
  Result := BuscarConstante(AUnidade, ANome);
  if Result = nil then
  begin
    Result := TApiConstante.Create;
    Result.Unidade := AUnidade;
    Result.Nome := ANome;
    Result.Valor := AValor;
    FConstantes.AddObject(AUnidade + '.' + ANome, Result);
  end;
end;

function TApi.BuscarPacote(const ATipo, ANome: string): TApiPacote;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(FPacotes.Count) do
    if SameText(Pacotes[vInt].Tipo, ATipo) and SameText(Pacotes[vInt].Nome, ANome) then
      Exit(Pacotes[vInt]);
end;

function TApi.AddPacote(const ATipo, ANome: string): TApiPacote;
begin
  Result := BuscarPacote(ATipo, ANome);
  if Result = nil then
  begin
    Result := TApiPacote.Create(ATipo, ANome);
    FPacotes.Add(Result);
  end;
end;

procedure TApi.AddUnidade(const ANome, AArquivo: string);
begin
  if FUnidades.IndexOfName(ANome) < 0 then
    FUnidades.Add(ANome + '=' + AArquivo);
end;

function TApi.ArquivoDaUnidade(const AUnidade: string): string;
var
  vIdx: integer;
begin
  vIdx := FUnidades.IndexOfName(AUnidade);
  if vIdx < 0 then
    Result := ''
  else
    Result := FUnidades.ValueFromIndex[vIdx];
end;

function TApi.PastaDaUnidade(const AUnidade: string): string;
begin
  Result := StringReplace(ExtractFilePath(StringReplace(ArquivoDaUnidade(AUnidade),
                                                        '/', PathDelim, [rfReplaceAll])),
                          PathDelim, '/', [rfReplaceAll]);
  if (Result <> '') and (Result[Length(Result)] = '/') then
    Delete(Result, Length(Result), 1);
end;

procedure CopiarMembro(ADestino, AOrigem: TApiMembro);
begin
  ADestino.Cat := AOrigem.Cat;
  ADestino.Forma := AOrigem.Forma;
  if AOrigem.Tipo <> '' then
    ADestino.Tipo := AOrigem.Tipo;
  ADestino.Padrao := AOrigem.Padrao;
  ADestino.Armazenado := AOrigem.Armazenado;
end;

procedure TApi.Publicadas(AClasse: TApiClasse; ARes: TStringList);
var
  vCadeia: TList;
  vAtual: TApiClasse;
  vInt, vMem: integer;
  vMembro, vNovo: TApiMembro;
begin
  vCadeia := TList.Create;
  try
    vAtual := AClasse;
    while (vAtual <> nil) and (vCadeia.IndexOf(vAtual) < 0) and (vCadeia.Count < 60) do
    begin
      vCadeia.Add(vAtual);
      vAtual := BuscarClasse(vAtual.Ancestral);
    end;
    // da raiz para a classe: o descendente redeclara e vence
    for vInt := Pred(vCadeia.Count) downto 0 do
    begin
      vAtual := TApiClasse(vCadeia[vInt]);
      for vMem := 0 to Pred(vAtual.Membros.Count) do
      begin
        vMembro := TApiMembro(vAtual.Membros.Objects[vMem]);
        if (vMembro.Cat <> 'published') or (vMembro.Forma <> 'prop') then
          Continue;
        vNovo := TApiMembro(nil);
        if ARes.IndexOf(vMembro.Nome) >= 0 then
          vNovo := TApiMembro(ARes.Objects[ARes.IndexOf(vMembro.Nome)])
        else
        begin
          vNovo := TApiMembro.Create(vMembro.Nome);
          ARes.AddObject(vMembro.Nome, vNovo);
        end;
        CopiarMembro(vNovo, vMembro);
      end;
    end;
  finally
    vCadeia.Free;
  end;
end;

{ JSON }

function JStr(AObj: TJSONObject; const AChave: string): string;
begin
  Result := AObj.Get(AChave, '');
end;

procedure PorTexto(AObj: TJSONObject; const AChave, AValor: string);
begin
  if AValor <> '' then
    AObj.Add(AChave, AValor);
end;

function ListaJSON(ALista: TStrings): TJSONArray;
var
  vInt: integer;
begin
  Result := TJSONArray.Create;
  for vInt := 0 to Pred(ALista.Count) do
    Result.Add(ALista[vInt]);
end;

function MembroJSON(AMembro: TApiMembro): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('cat', AMembro.Cat);
  Result.Add('forma', AMembro.Forma);
  PorTexto(Result, 'tipo', AMembro.Tipo);
  PorTexto(Result, 'padrao', AMembro.Padrao);
  PorTexto(Result, 'armazenado', AMembro.Armazenado);
  if AMembro.Assinaturas.Count > 0 then
    Result.Add('assinaturas', ListaJSON(AMembro.Assinaturas));
end;

function PublicadasJSON(AApi: TApi; AClasse: TApiClasse): TJSONObject;
var
  vLista: TStringList;
  vInt: integer;
  vMembro: TApiMembro;
  vItem: TJSONObject;
begin
  Result := TJSONObject.Create;
  vLista := CriarListaMembros;
  try
    AApi.Publicadas(AClasse, vLista);
    for vInt := 0 to Pred(vLista.Count) do
    begin
      vMembro := TApiMembro(vLista.Objects[vInt]);
      vItem := TJSONObject.Create;
      PorTexto(vItem, 'tipo', vMembro.Tipo);
      PorTexto(vItem, 'padrao', vMembro.Padrao);
      PorTexto(vItem, 'armazenado', vMembro.Armazenado);
      Result.Add(vMembro.Nome, vItem);
    end;
  finally
    vLista.Free;
  end;
end;

function ClasseJSON(AApi: TApi; AClasse: TApiClasse): TJSONObject;
var
  vMembros: TJSONObject;
  vInt: integer;
begin
  Result := TJSONObject.Create;
  Result.Add('unit', AClasse.Unidade);
  PorTexto(Result, 'ancestral', AClasse.Ancestral);
  Result.Add('forma', AClasse.Forma);
  vMembros := TJSONObject.Create;
  for vInt := 0 to Pred(AClasse.Membros.Count) do
    vMembros.Add(AClasse.Membros[vInt], MembroJSON(TApiMembro(AClasse.Membros.Objects[vInt])));
  Result.Add('membros', vMembros);
  if AClasse.LeAntigos.Count > 0 then
    Result.Add('le-antigos', ListaJSON(AClasse.LeAntigos));
  Result.Add('published', PublicadasJSON(AApi, AClasse));
end;

function TipoJSON(ATipo: TApiTipo): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('unit', ATipo.Unidade);
  Result.Add('forma', ATipo.Forma);
  PorTexto(Result, 'detalhe', ATipo.Detalhe);
  if ATipo.Valores.Count > 0 then
    Result.Add('valores', ListaJSON(ATipo.Valores));
end;

function PacotesJSON(AApi: TApi; const ATipo: string): TJSONObject;
var
  vLista: TStringList;
  vInt: integer;
  vPacote: TApiPacote;
begin
  Result := TJSONObject.Create;
  vLista := NovaOrdenada(False);
  try
    for vInt := 0 to Pred(AApi.PacoteCount) do
      if SameText(AApi.Pacotes[vInt].Tipo, ATipo) then
        vLista.AddObject(AApi.Pacotes[vInt].Nome, AApi.Pacotes[vInt]);
    for vInt := 0 to Pred(vLista.Count) do
    begin
      vPacote := TApiPacote(vLista.Objects[vInt]);
      Result.Add(vPacote.Nome, ListaJSON(vPacote.Unidades));
    end;
  finally
    vLista.Free;
  end;
end;

function TApi.ParaJSON: string;
var
  vRaiz, vObj, vItem: TJSONObject;
  vInt: integer;
  vRot: TApiRotina;
  vCon: TApiConstante;
begin
  vRaiz := TJSONObject.Create;
  try
    vRaiz.Add('rotulo', Rotulo);
    vRaiz.Add('origem', Origem);
    PorTexto(vRaiz, 'commit', Commit);
    vRaiz.Add('data', Data);

    vObj := TJSONObject.Create;
    for vInt := 0 to Pred(FUnidades.Count) do
      vObj.Add(FUnidades.Names[vInt], FUnidades.ValueFromIndex[vInt]);
    vRaiz.Add('units', vObj);
    vObj := TJSONObject.Create;
    for vInt := 0 to Pred(FUnidades.Count) do
      vObj.Add(FUnidades.Names[vInt], PastaDaUnidade(FUnidades.Names[vInt]));
    vRaiz.Add('pastas', vObj);

    vObj := TJSONObject.Create;
    for vInt := 0 to Pred(FClasses.Count) do
      vObj.Add(FClasses[vInt], ClasseJSON(Self, TApiClasse(FClasses.Objects[vInt])));
    vRaiz.Add('classes', vObj);

    vObj := TJSONObject.Create;
    for vInt := 0 to Pred(FTipos.Count) do
      vObj.Add(FTipos[vInt], TipoJSON(TApiTipo(FTipos.Objects[vInt])));
    vRaiz.Add('tipos', vObj);

    vObj := TJSONObject.Create;
    for vInt := 0 to Pred(FRotinas.Count) do
    begin
      vRot := TApiRotina(FRotinas.Objects[vInt]);
      vObj.Add(FRotinas[vInt], ListaJSON(vRot.Assinaturas));
    end;
    vRaiz.Add('rotinas', vObj);

    vObj := TJSONObject.Create;
    for vInt := 0 to Pred(FConstantes.Count) do
    begin
      vCon := TApiConstante(FConstantes.Objects[vInt]);
      vObj.Add(FConstantes[vInt], vCon.Valor);
    end;
    vRaiz.Add('constantes', vObj);

    vObj := TJSONObject.Create;
    for vInt := 0 to Pred(FRegistradas.Count) do
      vObj.Add(FRegistradas.Names[vInt], FRegistradas.ValueFromIndex[vInt]);
    vRaiz.Add('registradas', vObj);

    vItem := TJSONObject.Create;
    vItem.Add('delphi', PacotesJSON(Self, 'delphi'));
    vItem.Add('lazarus', PacotesJSON(Self, 'lazarus'));
    vRaiz.Add('pacotes', vItem);

    vRaiz.Add('erros', ListaJSON(FErros));
    Result := StringReplace(vRaiz.FormatJSON([foSingleLineArray], 2),
                            #13#10, #10, [rfReplaceAll]) + #10;
  finally
    vRaiz.Free;
  end;
end;

procedure TApi.SalvarArquivo(const AArquivo: string);
var
  vStream: TFileStream;
  vTexto: string;
begin
  vTexto := ParaJSON;
  ForceDirectories(ExtractFilePath(ExpandFileName(AArquivo)));
  vStream := TFileStream.Create(AArquivo, fmCreate);
  try
    if vTexto <> '' then
      vStream.WriteBuffer(vTexto[1], Length(vTexto));
  finally
    vStream.Free;
  end;
end;

procedure LerMembros(AClasse: TApiClasse; AObj: TJSONObject);
var
  vInt, vAss: integer;
  vMembro: TApiMembro;
  vItem: TJSONObject;
  vArr: TJSONArray;
begin
  for vInt := 0 to Pred(AObj.Count) do
  begin
    vItem := AObj.Items[vInt] as TJSONObject;
    vMembro := AClasse.Obter(AObj.Names[vInt]);
    vMembro.Cat := JStr(vItem, 'cat');
    vMembro.Forma := JStr(vItem, 'forma');
    vMembro.Tipo := JStr(vItem, 'tipo');
    vMembro.Padrao := JStr(vItem, 'padrao');
    vMembro.Armazenado := JStr(vItem, 'armazenado');
    vArr := vItem.Get('assinaturas', TJSONArray(nil));
    if vArr <> nil then
      for vAss := 0 to Pred(vArr.Count) do
        vMembro.Assinaturas.Add(vArr.Strings[vAss]);
  end;
end;

procedure LerClasses(AApi: TApi; AObj: TJSONObject);
var
  vInt, vAnt: integer;
  vItem: TJSONObject;
  vClasse: TApiClasse;
begin
  for vInt := 0 to Pred(AObj.Count) do
  begin
    vItem := AObj.Items[vInt] as TJSONObject;
    vClasse := AApi.AddClasse(AObj.Names[vInt], JStr(vItem, 'unit'),
                              JStr(vItem, 'ancestral'), JStr(vItem, 'forma'));
    if vItem.Find('membros') is TJSONObject then
      LerMembros(vClasse, vItem.Objects['membros']);
    if vItem.Find('le-antigos') is TJSONArray then
      for vAnt := 0 to Pred(vItem.Arrays['le-antigos'].Count) do
        vClasse.LeAntigos.Add(vItem.Arrays['le-antigos'].Strings[vAnt]);
  end;
end;

procedure LerTipos(AApi: TApi; AObj: TJSONObject);
var
  vInt, vVal: integer;
  vItem: TJSONObject;
  vTipo: TApiTipo;
  vArr: TJSONArray;
begin
  for vInt := 0 to Pred(AObj.Count) do
  begin
    vItem := AObj.Items[vInt] as TJSONObject;
    vTipo := AApi.AddTipo(AObj.Names[vInt], JStr(vItem, 'unit'),
                          JStr(vItem, 'forma'), JStr(vItem, 'detalhe'));
    vArr := vItem.Get('valores', TJSONArray(nil));
    if vArr <> nil then
      for vVal := 0 to Pred(vArr.Count) do
        vTipo.Valores.Add(vArr.Strings[vVal]);
  end;
end;

procedure LerRotinas(AApi: TApi; AObj: TJSONObject);
var
  vInt, vAss: integer;
  vPonto: integer;
  vNome: string;
  vArr: TJSONArray;
begin
  for vInt := 0 to Pred(AObj.Count) do
  begin
    vNome := AObj.Names[vInt];
    vPonto := Pos('.', vNome);
    vArr := AObj.Items[vInt] as TJSONArray;
    if vArr.Count = 0 then
      AApi.AddRotina(Copy(vNome, 1, vPonto - 1), Copy(vNome, vPonto + 1, MaxInt), '');
    for vAss := 0 to Pred(vArr.Count) do
      AApi.AddRotina(Copy(vNome, 1, vPonto - 1), Copy(vNome, vPonto + 1, MaxInt),
                     vArr.Strings[vAss]);
  end;
end;

procedure LerPacotes(AApi: TApi; AObj: TJSONObject; const ATipo: string);
var
  vInt, vUni: integer;
  vPacote: TApiPacote;
  vArr: TJSONArray;
begin
  for vInt := 0 to Pred(AObj.Count) do
  begin
    vPacote := AApi.AddPacote(ATipo, AObj.Names[vInt]);
    vArr := AObj.Items[vInt] as TJSONArray;
    for vUni := 0 to Pred(vArr.Count) do
      vPacote.Unidades.Add(vArr.Strings[vUni]);
  end;
end;

procedure TApi.CarregarJSON(const ATexto: string);
var
  vDados: TJSONData;
  vRaiz, vObj: TJSONObject;
  vInt: integer;
  vNome: string;
begin
  vDados := GetJSON(ATexto);
  try
    vRaiz := vDados as TJSONObject;
    Rotulo := JStr(vRaiz, 'rotulo');
    Origem := JStr(vRaiz, 'origem');
    Commit := JStr(vRaiz, 'commit');
    Data := JStr(vRaiz, 'data');
    vObj := vRaiz.Objects['units'];
    for vInt := 0 to Pred(vObj.Count) do
      AddUnidade(vObj.Names[vInt], vObj.Items[vInt].AsString);
    LerClasses(Self, vRaiz.Objects['classes']);
    LerTipos(Self, vRaiz.Objects['tipos']);
    LerRotinas(Self, vRaiz.Objects['rotinas']);
    vObj := vRaiz.Objects['constantes'];
    for vInt := 0 to Pred(vObj.Count) do
    begin
      vNome := vObj.Names[vInt];
      AddConstante(Copy(vNome, 1, Pos('.', vNome) - 1),
                   Copy(vNome, Pos('.', vNome) + 1, MaxInt), vObj.Items[vInt].AsString);
    end;
    vObj := vRaiz.Objects['registradas'];
    for vInt := 0 to Pred(vObj.Count) do
      FRegistradas.Add(vObj.Names[vInt] + '=' + vObj.Items[vInt].AsString);
    LerPacotes(Self, vRaiz.Objects['pacotes'].Objects['delphi'], 'delphi');
    LerPacotes(Self, vRaiz.Objects['pacotes'].Objects['lazarus'], 'lazarus');
    for vInt := 0 to Pred(vRaiz.Arrays['erros'].Count) do
      FErros.Add(vRaiz.Arrays['erros'].Strings[vInt]);
  finally
    vDados.Free;
  end;
end;

procedure TApi.CarregarArquivo(const AArquivo: string);
var
  vStream: TFileStream;
  vTexto: string;
begin
  vStream := TFileStream.Create(AArquivo, fmOpenRead or fmShareDenyNone);
  try
    SetLength(vTexto, vStream.Size);
    if vStream.Size > 0 then
      vStream.ReadBuffer(vTexto[1], vStream.Size);
  finally
    vStream.Free;
  end;
  CarregarJSON(vTexto);
end;

{ Registros da paleta }

/// Source without comments (replaced by spaces, so positions hold); strings
/// are kept as they are
function SemComentarios(const ATexto: string): string;
var
  vInt, vTam, vFim: integer;
  vFecha: string;
begin
  Result := ATexto;
  vTam := Length(Result);
  vInt := 1;
  while vInt <= vTam do
  begin
    if Result[vInt] = '''' then
    begin
      Inc(vInt);
      while (vInt <= vTam) and (Result[vInt] <> '''') and (Result[vInt] <> #10) do
        Inc(vInt);
      Inc(vInt);
      Continue;
    end;
    vFecha := '';
    if Result[vInt] = '{' then
      vFecha := '}'
    else if (Result[vInt] = '(') and (vInt < vTam) and (Result[vInt + 1] = '*') then
      vFecha := '*)'
    else if (Result[vInt] = '/') and (vInt < vTam) and (Result[vInt + 1] = '/') then
      vFecha := #10;
    if vFecha = '' then
    begin
      Inc(vInt);
      Continue;
    end;
    while (vInt <= vTam) and (Copy(Result, vInt, Length(vFecha)) <> vFecha) do
    begin
      if Result[vInt] <> #10 then
        Result[vInt] := ' ';
      Inc(vInt);
    end;
    if vFecha <> #10 then
    begin
      for vFim := vInt to vInt + Length(vFecha) - 1 do
        if vFim <= vTam then
          Result[vFim] := ' ';
      Inc(vInt, Length(vFecha));
    end;
  end;
end;

/// Text the parser reads: only up to 'implementation' (the API is the
/// interface section; routine bodies hold things the parser does not know) and
/// without resource directives
function PrepararFonte(const ATexto: string): string;
var
  vLimpo, vBaixo: string;
  vPos, vFim, vIdx: integer;
begin
  vLimpo := SemComentarios(ATexto);
  vBaixo := LowerCase(vLimpo);
  vPos := Pos(#10 + 'implementation', vBaixo);
  while (vPos > 0) and (vPos + 15 <= Length(vBaixo)) and
        (vBaixo[vPos + 15] in ['a'..'z', '0'..'9', '_']) do
    vPos := PosEx(#10 + 'implementation', vBaixo, vPos + 1);
  Result := ATexto;
  if vPos > 0 then
    Result := Copy(ATexto, 1, vPos) + 'implementation' + #10 + 'end.' + #10;
  vBaixo := LowerCase(Result);
  vPos := Pos('{$r ', vBaixo);
  while vPos > 0 do
  begin
    vFim := PosEx('}', Result, vPos);
    if vFim = 0 then
      Break;
    for vIdx := vPos to vFim do
      Result[vIdx] := ' ';
    vPos := PosEx('{$r ', vBaixo, vFim);
  end;
end;

/// Text between the parenthesis that opens at APos and its pair
function ArgumentosDe(const ATexto: string; APos: integer): string;
var
  vProf, vInt: integer;
  vAspas: boolean;
begin
  Result := '';
  vProf := 0;
  vAspas := False;
  for vInt := APos to Length(ATexto) do
  begin
    if ATexto[vInt] = '''' then
      vAspas := not vAspas;
    if vAspas then
      Continue;
    if ATexto[vInt] = '(' then
      Inc(vProf)
    else if ATexto[vInt] = ')' then
    begin
      Dec(vProf);
      if vProf = 0 then
      begin
        Result := Copy(ATexto, APos + 1, vInt - APos - 1);
        Exit;
      end;
    end;
  end;
end;

function SemAspas(const ATexto: string): string;
begin
  Result := Trim(ATexto);
  if (Length(Result) >= 2) and (Result[1] = '''') and (Result[Length(Result)] = '''') then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

procedure TratarRegistro(const AArgs: string; ALista: TStrings);
var
  vVirgula, vIni, vFim, vInt: integer;
  vPagina, vMiolo: string;
  vPartes: TStringList;
begin
  vVirgula := Pos(',', AArgs);
  vIni := Pos('[', AArgs);
  vFim := LastDelimiter(']', AArgs);
  if (vVirgula = 0) or (vIni = 0) or (vFim <= vIni) then
    Exit;
  vPagina := SemAspas(Copy(AArgs, 1, vVirgula - 1));
  vMiolo := Copy(AArgs, vIni + 1, vFim - vIni - 1);
  vPartes := TStringList.Create;
  try
    vPartes.StrictDelimiter := True;
    vPartes.Delimiter := ',';
    vPartes.DelimitedText := StringReplace(vMiolo, '"', '', [rfReplaceAll]);
    for vInt := 0 to Pred(vPartes.Count) do
      if Trim(vPartes[vInt]) <> '' then
        ALista.Add(Trim(vPartes[vInt]) + '=' + vPagina);
  finally
    vPartes.Free;
  end;
end;

procedure AcharRegistros(const ATexto: string; ALista: TStrings);
var
  vLimpo, vBaixo: string;
  vPos, vParen: integer;
const
  CChave = 'registercomponents';
begin
  vLimpo := SemComentarios(ATexto);
  vBaixo := LowerCase(vLimpo);
  vPos := Pos(CChave, vBaixo);
  while vPos > 0 do
  begin
    vParen := vPos + Length(CChave);
    while (vParen <= Length(vLimpo)) and (vLimpo[vParen] in [' ', #9, #10, #13]) do
      Inc(vParen);
    if (vParen <= Length(vLimpo)) and (vLimpo[vParen] = '(') then
      TratarRegistro(ArgumentosDe(vLimpo, vParen), ALista);
    vPos := PosEx(CChave, vBaixo, vPos + Length(CChave));
  end;
end;

{ fcl-passrc }

type
  /// Line source over a text already in memory
  TLinhasTexto = class(TLineReader)
  private
    FLinhas: TStringList;
    FPos: integer;
  public
    constructor Criar(const ANome, ATexto: string);
    destructor Destroy; override;
    function IsEOF: boolean; override;
    function ReadLine: string; override;
  end;

  /// Text read once from the source
  TTextoLido = class
  public
    Texto: string;
  end;

  /// Finds sources and includes in the RAL tree through the TOrigemArquivos.
  TResolverOrigem = class(TBaseFileResolver)
  private
    FArquivos: TStringList;
    FCache: TStringList;
    FOrigem: TOrigemArquivos;
    FPastaAtual: string;
    function Escolher(const ABase: string): string;
    function Reader(const ACaminho: string): TLineReader;
  public
    constructor Create(AOrigem: TOrigemArquivos); reintroduce;
    destructor Destroy; override;
    function FindSourceFile(const AName: string): TLineReader; override;
    function FindIncludeFile(const AName: string): TLineReader; override;
    function FindIncludeFileName(const AName: string): string; override;
    function FindResourceFileName(const AName: string): string; override;
    /// Whole text of a file of the source, cached
    function Texto(const ACaminho: string): string;
    property PastaAtual: string read FPastaAtual write FPastaAtual;
  end;

  /// The tree is only read, never resolved: types stay as references by name.
  TMotorSimples = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: string;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: string; ASourceLinenumber: integer): TPasElement;
      override;
    function FindElement(const AName: string): TPasElement; override;
  end;

constructor TLinhasTexto.Criar(const ANome, ATexto: string);
begin
  inherited Create(ANome);
  FLinhas := TStringList.Create;
  FLinhas.Text := ATexto;
end;

destructor TLinhasTexto.Destroy;
begin
  FLinhas.Free;
  inherited Destroy;
end;

function TLinhasTexto.IsEOF: boolean;
begin
  Result := FPos >= FLinhas.Count;
end;

function TLinhasTexto.ReadLine: string;
begin
  Result := FLinhas[FPos];
  Inc(FPos);
end;

constructor TResolverOrigem.Create(AOrigem: TOrigemArquivos);
var
  vTodos: TStringList;
  vInt: integer;
begin
  inherited Create;
  FOrigem := AOrigem;
  FArquivos := TStringList.Create;
  FCache := NovaOrdenada(True);
  vTodos := TStringList.Create;
  try
    FOrigem.Listar('', vTodos);
    for vInt := 0 to Pred(vTodos.Count) do
      FArquivos.Add(LowerCase(ExtractFileName(vTodos[vInt])) + '=' + vTodos[vInt]);
  finally
    vTodos.Free;
  end;
end;

destructor TResolverOrigem.Destroy;
begin
  FCache.Free;
  FArquivos.Free;
  inherited Destroy;
end;

function TResolverOrigem.Texto(const ACaminho: string): string;
var
  vIdx: integer;
  vLido: TTextoLido;
begin
  vIdx := FCache.IndexOf(ACaminho);
  if vIdx < 0 then
  begin
    vLido := TTextoLido.Create;
    vLido.Texto := FOrigem.Ler(ACaminho);
    FCache.AddObject(ACaminho, vLido);
  end
  else
    vLido := TTextoLido(FCache.Objects[vIdx]);
  Result := vLido.Texto;
end;

function TResolverOrigem.Reader(const ACaminho: string): TLineReader;
begin
  Result := TLinhasTexto.Criar(ACaminho, Texto(ACaminho));
end;

/// Among the files with that name, the one in the folder of the unit being read
/// wins; otherwise the first
function TResolverOrigem.Escolher(const ABase: string): string;
var
  vInt: integer;
  vChave: string;
begin
  Result := '';
  vChave := LowerCase(ABase);
  for vInt := 0 to Pred(FArquivos.Count) do
    if FArquivos.Names[vInt] = vChave then
    begin
      if Result = '' then
        Result := FArquivos.ValueFromIndex[vInt];
      if SameText(ExtractFilePath(StringReplace(FArquivos.ValueFromIndex[vInt], '/',
                                                PathDelim, [rfReplaceAll])),
                  FPastaAtual) then
        Exit(FArquivos.ValueFromIndex[vInt]);
    end;
end;

function TResolverOrigem.FindSourceFile(const AName: string): TLineReader;
begin
  Result := nil;
  if FOrigem.Existe(AName) then
    Result := TLinhasTexto.Criar(AName, PrepararFonte(Texto(AName)));
end;

function TResolverOrigem.FindIncludeFile(const AName: string): TLineReader;
var
  vCaminho: string;
begin
  Result := nil;
  vCaminho := Escolher(ExtractFileName(StringReplace(AName, '\', '/', [rfReplaceAll])));
  if vCaminho <> '' then
    Result := Reader(vCaminho);
end;

function TResolverOrigem.FindIncludeFileName(const AName: string): string;
begin
  Result := Escolher(ExtractFileName(StringReplace(AName, '\', '/', [rfReplaceAll])));
end;

function TResolverOrigem.FindResourceFileName(const AName: string): string;
begin
  Result := Escolher(ExtractFileName(StringReplace(AName, '\', '/', [rfReplaceAll])));
end;

function TMotorSimples.CreateElement(AClass: TPTreeElement; const AName: string;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: string; ASourceLinenumber: integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TMotorSimples.FindElement(const AName: string): TPasElement;
begin
  Result := nil;
end;

{ Extraction of one parsed unit }

function Compacto(const ATexto: string): string;
var
  vInt: integer;
  vEspaco: boolean;
begin
  Result := '';
  vEspaco := False;
  for vInt := 1 to Length(ATexto) do
    if ATexto[vInt] in [' ', #9, #10, #13] then
      vEspaco := True
    else
    begin
      if vEspaco and (Result <> '') then
        Result := Result + ' ';
      vEspaco := False;
      Result := Result + ATexto[vInt];
    end;
end;

function TipoTexto(ATipo: TPasType): string;
begin
  Result := '';
  if ATipo = nil then
    Exit;
  try
    if ATipo.Name <> '' then
      Result := ATipo.Name
    else
      Result := ATipo.GetDeclaration(True);
  except
    Result := ATipo.ClassName;
  end;
  Result := Compacto(Result);
end;

function ExprTexto(AExpr: TPasExpr): string;
begin
  Result := '';
  if AExpr = nil then
    Exit;
  try
    Result := Compacto(AExpr.GetDeclaration(True));
  except
    Result := '?';
  end;
end;

function ArgsTexto(AArgs: TFPList): string;
var
  vInt: integer;
  vArg: TPasArgument;
  vTxt: string;
begin
  Result := '';
  for vInt := 0 to Pred(AArgs.Count) do
  begin
    vArg := TPasArgument(AArgs[vInt]);
    case vArg.Access of
      argConst: vTxt := 'const ';
      argVar: vTxt := 'var ';
      argOut: vTxt := 'out ';
      argConstRef: vTxt := 'constref ';
    else
      vTxt := '';
    end;
    vTxt := vTxt + TipoTexto(vArg.ArgType);
    if Result <> '' then
      Result := Result + '; ';
    Result := Result + Trim(vTxt);
  end;
end;

function SufixoProc(AProc: TPasProcedureType): string;
begin
  Result := '';
  if AProc is TPasFunctionType then
    Result := ': ' + TipoTexto(TPasFunctionType(AProc).ResultEl.ResultType);
  if AProc.IsOfObject then
    Result := Result + ' of object';
  if AProc.IsReferenceTo then
    Result := Result + ' reference to';
end;

/// 'procedure(const string; Integer)', 'function(): Boolean of object'
function AssinaturaTipo(AProc: TPasProcedureType): string;
var
  vKind: string;
begin
  if AProc is TPasFunctionType then
    vKind := 'function'
  else
    vKind := 'procedure';
  Result := vKind + '(' + ArgsTexto(AProc.Args) + ')' + SufixoProc(AProc);
end;

function AssinaturaRotina(AProc: TPasProcedure): string;
begin
  Result := LowerCase(AProc.TypeName) + '(' + ArgsTexto(AProc.ProcType.Args) + ')' +
            SufixoProc(AProc.ProcType);
end;

function FormaDaClasse(AClasse: TPasClassType): string;
begin
  case AClasse.ObjKind of
    okInterface, okDispInterface: Result := 'interface';
    okObject: Result := 'object';
    okClassHelper, okRecordHelper, okTypeHelper: Result := 'helper';
  else
    Result := 'class';
  end;
end;

type
  /// Reads the interface section of one unit into the API
  TLeitorUnit = class
  private
    FApi: TApi;
    FUnidade: string;
    procedure LerMembro(AClasse: TApiClasse; AEl: TPasElement);
    procedure LerPropriedade(AClasse: TApiClasse; AProp: TPasProperty; const ACat: string);
    procedure LerMembros(AClasse: TApiClasse; AMembros: TFPList);
    procedure LerClasse(AClasse: TPasClassType);
    procedure LerRegistro(ARegistro: TPasRecordType);
    procedure LerTipo(ATipo: TPasType);
    procedure LerElemento(AEl: TPasElement);
  public
    constructor Create(AApi: TApi; const AUnidade: string);
    procedure Ler(AModulo: TPasModule);
  end;

constructor TLeitorUnit.Create(AApi: TApi; const AUnidade: string);
begin
  inherited Create;
  FApi := AApi;
  FUnidade := AUnidade;
end;

function CategoriaDe(AEl: TPasElement): string;
begin
  case AEl.Visibility of
    visPublished: Result := 'published';
    visPublic, visDefault, visAutomated: Result := 'public';
  else
    Result := '';
  end;
end;

procedure TLeitorUnit.LerPropriedade(AClasse: TApiClasse; AProp: TPasProperty;
  const ACat: string);
var
  vMembro: TApiMembro;
begin
  vMembro := AClasse.Obter(AProp.Name);
  vMembro.Cat := ACat;
  vMembro.Forma := 'prop';
  if vMembro.Tipo = '' then
    vMembro.Tipo := TipoTexto(AProp.VarType);
  if (AProp.DefaultValue <> '') and not AProp.IsNodefault then
    vMembro.Padrao := Compacto(AProp.DefaultValue);
  vMembro.Armazenado := Compacto(AProp.StoredAccessorName);
  if (AProp.Args <> nil) and (AProp.Args.Count > 0) then
    vMembro.Assinaturas.Add('[' + ArgsTexto(AProp.Args) + ']');
end;

procedure TLeitorUnit.LerMembro(AClasse: TApiClasse; AEl: TPasElement);
var
  vCat: string;
  vMembro: TApiMembro;
begin
  vCat := CategoriaDe(AEl);
  if (vCat = '') or (AEl.Name = '') then
    Exit;
  if AEl is TPasProperty then
    LerPropriedade(AClasse, TPasProperty(AEl), vCat)
  else if AEl is TPasProcedure then
  begin
    vMembro := AClasse.Obter(AEl.Name);
    if vMembro.Cat <> 'published' then
      vMembro.Cat := vCat;
    vMembro.Forma := 'metodo';
    vMembro.Assinaturas.Add(AssinaturaRotina(TPasProcedure(AEl)));
  end
  else if AEl is TPasVariable then
  begin
    vMembro := AClasse.Obter(AEl.Name);
    vMembro.Cat := vCat;
    vMembro.Forma := 'campo';
    vMembro.Tipo := TipoTexto(TPasVariable(AEl).VarType);
  end;
end;

procedure TLeitorUnit.LerMembros(AClasse: TApiClasse; AMembros: TFPList);
var
  vInt: integer;
begin
  for vInt := 0 to Pred(AMembros.Count) do
    LerMembro(AClasse, TPasElement(AMembros[vInt]));
end;

procedure TLeitorUnit.LerClasse(AClasse: TPasClassType);
var
  vAnc: string;
  vCls: TApiClasse;
begin
  if AClasse.IsForward or (AClasse.Name = '') then
    Exit;
  vAnc := TipoTexto(AClasse.AncestorType);
  vCls := FApi.AddClasse(AClasse.Name, FUnidade, vAnc, FormaDaClasse(AClasse));
  LerMembros(vCls, AClasse.Members);
end;

procedure TLeitorUnit.LerRegistro(ARegistro: TPasRecordType);
var
  vCls: TApiClasse;
begin
  vCls := FApi.AddClasse(ARegistro.Name, FUnidade, '', 'record');
  LerMembros(vCls, ARegistro.Members);
end;

procedure TLeitorUnit.LerTipo(ATipo: TPasType);
var
  vTipo: TApiTipo;
  vInt: integer;
  vValores: TFPList;
begin
  if ATipo.Name = '' then
    Exit;
  if ATipo is TPasEnumType then
  begin
    vTipo := FApi.AddTipo(ATipo.Name, FUnidade, 'enum', '');
    vValores := TPasEnumType(ATipo).Values;
    for vInt := 0 to Pred(vValores.Count) do
      if vTipo.Valores.IndexOf(TPasElement(vValores[vInt]).Name) < 0 then
        vTipo.Valores.Add(TPasElement(vValores[vInt]).Name);
  end
  else if ATipo is TPasSetType then
    FApi.AddTipo(ATipo.Name, FUnidade, 'set',
                 'set of ' + TipoTexto(TPasSetType(ATipo).EnumType))
  else if ATipo is TPasProcedureType then
    FApi.AddTipo(ATipo.Name, FUnidade, 'proc', AssinaturaTipo(TPasProcedureType(ATipo)))
  else
  begin
    try
      FApi.AddTipo(ATipo.Name, FUnidade, 'outro', Compacto(ATipo.GetDeclaration(True)));
    except
      FApi.AddTipo(ATipo.Name, FUnidade, 'outro', ATipo.ClassName);
    end;
  end;
end;

procedure TLeitorUnit.LerElemento(AEl: TPasElement);
var
  vRot: TApiRotina;
begin
  if AEl is TPasClassType then
    LerClasse(TPasClassType(AEl))
  else if AEl is TPasRecordType then
    LerRegistro(TPasRecordType(AEl))
  else if AEl is TPasType then
    LerTipo(TPasType(AEl))
  else if AEl is TPasProcedure then
  begin
    vRot := FApi.AddRotina(FUnidade, AEl.Name, AssinaturaRotina(TPasProcedure(AEl)));
    if vRot = nil then
      Exit;
  end
  else if (AEl is TPasConst) and not (AEl is TPasProperty) then
    FApi.AddConstante(FUnidade, AEl.Name, ExprTexto(TPasConst(AEl).Expr));
end;

procedure TLeitorUnit.Ler(AModulo: TPasModule);
var
  vInt: integer;
  vDecl: TFPList;
begin
  if AModulo.InterfaceSection = nil then
    Exit;
  vDecl := AModulo.InterfaceSection.Declarations;
  for vInt := 0 to Pred(vDecl.Count) do
    LerElemento(TPasElement(vDecl[vInt]));
end;

{ Extraction of the whole tree }

type
  /// Runs the parse of every unit of src/ in the passes and fills the API
  TExtrator = class
  private
    FApi: TApi;
    FOrigem: TOrigemArquivos;
    FResolver: TResolverOrigem;
    procedure DefinirPasso(AScanner: TPascalScanner; AFpc: boolean);
    /// Parses one file in one pass; '' when it went well, else the message
    function ParsearArquivo(const AArquivo: string; AFpc: boolean): string;
    procedure LerArquivo(const AArquivo: string);
    procedure AcharLeAntigos(const ATexto: string);
    procedure ListarFontes(ALista: TStrings);
  public
    constructor Create(AOrigem: TOrigemArquivos; AApi: TApi);
    destructor Destroy; override;
    procedure Executar;
    /// Hands the origin to the catalog and fills the packages
    procedure ExtrairPacotes;
  end;

constructor TExtrator.Create(AOrigem: TOrigemArquivos; AApi: TApi);
begin
  inherited Create;
  FApi := AApi;
  FOrigem := AOrigem;
  FResolver := TResolverOrigem.Create(AOrigem);
end;

destructor TExtrator.Destroy;
begin
  FResolver.Free;
  inherited Destroy;
end;

procedure TExtrator.DefinirPasso(AScanner: TPascalScanner; AFpc: boolean);
var
  vDef: TStrings;
begin
  vDef := AScanner.Defines;
  if AFpc then
  begin
    vDef.Add('FPC');
    vDef.Add('_FIXINSIGHT_');
    vDef.Add('VER3');
    vDef.Add('VER3_2');
    vDef.Add('WINDOWS');
    vDef.Add('MSWINDOWS');
    vDef.Add('WIN64');
    vDef.Add('CPUX86_64');
    vDef.Add('CPU64');
    vDef.Add('ENDIAN_LITTLE');
    AScanner.AddMacro('FPC_FULLVERSION', '30203');
  end
  else
  begin
    vDef.Add('MSWINDOWS');
    vDef.Add('WIN32');
    vDef.Add('_FIXINSIGHT_');
    vDef.Add('CPU386');
    vDef.Add('UNICODE');
    vDef.Add('CONDITIONALEXPRESSIONS');
    vDef.Add('VER370');
    AScanner.AddMacro('CompilerVersion', '37.0');
  end;
end;

function TExtrator.ParsearArquivo(const AArquivo: string; AFpc: boolean): string;
var
  vScanner: TPascalScanner;
  vParser: TPasParser;
  vMotor: TMotorSimples;
  vModulo: TPasModule;
begin
  Result := '';
  vModulo := nil;
  vMotor := TMotorSimples.Create;
  vScanner := TPascalScanner.Create(FResolver);
  vParser := nil;
  try
    try
      FResolver.PastaAtual := StringReplace(ExtractFilePath(AArquivo), '/', PathDelim,
                                            [rfReplaceAll]);
      DefinirPasso(vScanner, AFpc);
      vScanner.OpenFile(AArquivo);
      vParser := TPasParser.Create(vScanner, FResolver, vMotor);
      if not AFpc then
        vParser.Options := vParser.Options + [po_delphi];
      vParser.Options := vParser.Options + [po_nooverloadedprocs];
      vParser.ParseMain(vModulo);
      TLeitorUnit.Create(FApi, vModulo.Name).Ler(vModulo);
      FApi.AddUnidade(vModulo.Name, AArquivo);
    except
      on E: Exception do
        Result := E.Message;
    end;
  finally
    vParser.Free;
    vScanner.Free;
    vMotor.Free;
  end;
end;

/// Text scan of the implementation: the literals given to Filer.DefineProperty
/// inside `TClasse.DefineProperties` are names the class still reads
procedure TExtrator.AcharLeAntigos(const ATexto: string);
var
  vBaixo, vCorpo, vNome: string;
  vPos, vFim, vPonto, vAbre, vFecha, vIni: integer;
  vClasse: TApiClasse;
begin
  vBaixo := LowerCase(ATexto);
  vIni := 1;
  while True do
  begin
    vPos := PosEx('.defineproperties(', vBaixo, vIni);
    if vPos = 0 then
      Break;
    vIni := vPos + 1;
    vPonto := vPos;
    while (vPonto > 1) and (vBaixo[vPonto - 1] in ['a'..'z', '0'..'9', '_']) do
      Dec(vPonto);
    vClasse := FApi.BuscarClasse(Copy(ATexto, vPonto, vPos - vPonto));
    if vClasse = nil then
      Continue;
    vFim := PosEx(#10'end;', vBaixo, vPos);
    if vFim = 0 then
      vFim := Length(ATexto);
    vCorpo := Copy(ATexto, vPos, vFim - vPos);
    vAbre := Pos('.defineproperty(''', LowerCase(vCorpo));
    while vAbre > 0 do
    begin
      vFecha := PosEx('''', vCorpo, vAbre + 17);
      vNome := Copy(vCorpo, vAbre + 17, vFecha - vAbre - 17);
      if (vNome <> '') and (vClasse.LeAntigos.IndexOf(vNome) < 0) then
        vClasse.LeAntigos.Add(vNome);
      vAbre := PosEx('.defineproperty(''', LowerCase(vCorpo), vFecha);
    end;
  end;
end;

procedure TExtrator.LerArquivo(const AArquivo: string);
var
  vDelphi, vFpc: string;
  vLista: TStringList;
  vInt: integer;
begin
  vDelphi := ParsearArquivo(AArquivo, False);
  vFpc := ParsearArquivo(AArquivo, True);
  if (vDelphi <> '') and (vFpc <> '') then
    FApi.Erros.Add(AArquivo + ' [delphi] ' + vDelphi + ' | [fpc] ' + vFpc);
  vLista := TStringList.Create;
  try
    AcharLeAntigos(FResolver.Texto(AArquivo));
    AcharRegistros(FResolver.Texto(AArquivo), vLista);
    for vInt := 0 to Pred(vLista.Count) do
      if FApi.Registradas.IndexOfName(vLista.Names[vInt]) < 0 then
        FApi.Registradas.Add(vLista[vInt]);
  finally
    vLista.Free;
  end;
end;

procedure TExtrator.ListarFontes(ALista: TStrings);
var
  vTodos: TStringList;
  vInt: integer;
begin
  vTodos := TStringList.Create;
  try
    FOrigem.Listar('src', vTodos);
    vTodos.Sort;
    for vInt := 0 to Pred(vTodos.Count) do
      if SameText(ExtractFileExt(vTodos[vInt]), '.pas') then
        ALista.Add(vTodos[vInt]);
  finally
    vTodos.Free;
  end;
end;

procedure TExtrator.Executar;
var
  vFontes: TStringList;
  vInt: integer;
begin
  vFontes := TStringList.Create;
  try
    ListarFontes(vFontes);
    for vInt := 0 to Pred(vFontes.Count) do
      LerArquivo(vFontes[vInt]);
  finally
    vFontes.Free;
  end;
end;

procedure TExtrator.ExtrairPacotes;
var
  vCat: TCatalogo;
  vTipo: TTipoPacote;
  vLista: TList;
  vInt, vUni: integer;
  vPacote: TPacote;
  vApiPac: TApiPacote;
  vNome: string;
begin
  vCat := TCatalogo.Create;
  try
    // o catalogo passa a ser dono da origem
    FResolver.Free;
    FResolver := nil;
    vCat.Carregar(FOrigem);
    vLista := TList.Create;
    try
      for vTipo := Low(TTipoPacote) to High(TTipoPacote) do
      begin
        vLista.Clear;
        vCat.Listar(vTipo, vLista);
        for vInt := 0 to Pred(vLista.Count) do
        begin
          vPacote := TPacote(vLista[vInt]);
          if vTipo = tpDelphi then
            vNome := 'delphi'
          else
            vNome := 'lazarus';
          vApiPac := FApi.AddPacote(vNome, vPacote.Nome);
          for vUni := 0 to Pred(vPacote.Unidades.Count) do
            vApiPac.Unidades.Add(ChangeFileExt(ExtractFileName(vPacote.Unidades[vUni]), ''));
        end;
      end;
    finally
      vLista.Free;
    end;
  finally
    vCat.Free;
  end;
end;

function ExtrairAPI(AOrigem: TOrigemArquivos; const ARotulo: string): TApi;
var
  vExtrator: TExtrator;
begin
  Result := TApi.Create;
  Result.Rotulo := ARotulo;
  Result.Origem := AOrigem.Descricao;
  if AOrigem is TOrigemZip then
    Result.Commit := TOrigemZip(AOrigem).Commit;
  Result.Data := FormatDateTime('yyyy-mm-dd', Now);
  vExtrator := TExtrator.Create(AOrigem, Result);
  try
    vExtrator.Executar;
    vExtrator.ExtrairPacotes;
  finally
    vExtrator.Free;
  end;
end;

end.
