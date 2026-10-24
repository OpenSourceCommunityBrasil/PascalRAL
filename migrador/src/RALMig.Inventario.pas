/// Inventory of the breaking differences between two versions of the RAL API
/// (RALMig.API), the coverage file that links each difference to a migration
/// rule or to a justification, and the INVENTARIO.md report. Every difference
/// has a stable key '<kind>:<target>' and a readable detail; the coverage file
/// matches keys with * and ? (no case). A difference with neither a rule nor a
/// justification is *pending*: a warning day to day and an error in the
/// pre-release check.
unit RALMig.Inventario;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, fpjson, jsonparser,
  RALMig.API;

type
  /// One breaking difference.
  TDiferenca = class
  public
    /// '<kind>:<target>', for instance 'published-sumiu:TRALServer.CompressType'
    Chave: string;
    Detalhe: string;
    /// The part of the key before the first ':'
    function Tipo: string;
  end;

  /// The differences of one step, sorted by key.
  TDiferencas = class
  private
    FLista: TObjectList;
    function GetCount: integer;
    function GetItem(AIndex: integer): TDiferenca;
  public
    constructor Create;
    destructor Destroy; override;
    /// Adds the difference; a key that already exists is ignored
    procedure Add(const AChave, ADetalhe: string);
    function Buscar(const AChave: string): TDiferenca;
    procedure Ordenar;
    property Count: integer read GetCount;
    property Itens[AIndex: integer]: TDiferenca read GetItem; default;
  end;

  /// One line of the coverage file: a key pattern and what covers it.
  TCobItem = class
  public
    Chave: string;
    Regra: string;
    Justificativa: string;
  end;

  /// A step of the chain (1.0 -> 1.1) with its covered items.
  TCobPasso = class
  public
    De: string;
    Para: string;
    Fechado: boolean;
    Itens: TObjectList;
    constructor Create;
    destructor Destroy; override;
    /// 'de>para'
    function Rotulo: string;
  end;

  /// A break of behavior, which the API does not show.
  TCobComportamento = class
  public
    Passo: string;
    Id: string;
    Descricao: string;
    Fazer: string;
    Wiki: string;
  end;

  /// The coverage file.
  TCobertura = class
  private
    FComportamento: TObjectList;
    FErros: TStringList;
    FPassos: TObjectList;
    function GetPasso(AIndex: integer): TCobPasso;
    function GetPassoCount: integer;
    function GetComp(AIndex: integer): TCobComportamento;
    function GetCompCount: integer;
    procedure LerPasso(AObj: TJSONObject; AIndice: integer);
    procedure LerComportamento(AObj: TJSONObject; AIndice: integer);
  public
    constructor Create;
    destructor Destroy; override;
    /// False when the file is invalid; Erros says why
    function CarregarTexto(const ATexto: string): boolean;
    function CarregarArquivo(const AArquivo: string): boolean;
    function BuscarPasso(const ARotulo: string): TCobPasso;

    property CompCount: integer read GetCompCount;
    property Comportamento[AIndex: integer]: TCobComportamento read GetComp;
    property Erros: TStringList read FErros;
    property PassoCount: integer read GetPassoCount;
    property Passos[AIndex: integer]: TCobPasso read GetPasso;
  end;

  /// A difference and the coverage item that matched it.
  TLinhaCob = class
  public
    Dif: TDiferenca;
    Item: TCobItem;
  end;

  /// The result of one step: the differences split into covered and pending.
  TResultadoPasso = class
  public
    Passo: TCobPasso;
    Diferencas: TDiferencas;
    Cobertas: TObjectList;
    Pendentes: TList;
    /// Patterns of the coverage that match no difference
    Obsoletos: TStringList;
    /// Why the step could not be compared ('' when it could)
    Erro: string;
    /// True when Passo was made for this result alone (ad hoc comparison)
    PassoProprio: boolean;
    constructor Create(APasso: TCobPasso);
    destructor Destroy; override;
  end;

/// Compares two APIs and lists everything that breaks a user project
procedure CompararAPIs(AAntiga, ANova: TApi; ADif: TDiferencas);
/// * and ? (any run, any one character), no case
function CasaGlob(const APadrao, ATexto: string): boolean;
/// Splits the differences by the coverage of the step; the result owns them
procedure Cobrir(APasso: TCobPasso; ADif: TDiferencas; AResultado: TResultadoPasso);
/// Loads api-<rotulo>.json of the two sides of the step from the folder and
/// compares them; the result carries Erro when a file is missing
function ProcessarPasso(APasso: TCobPasso; const APastaApi: string): TResultadoPasso;
/// Compares two APIs ad hoc (no files, no step needed): the step of the coverage
/// whose from/to are the labels of the APIs covers the differences; with no such
/// step, nothing covers them. The result owns what it holds
function CompararComCobertura(AAntiga, ANova: TApi; ACob: TCobertura): TResultadoPasso;
/// The pending differences, one per line: '::warning::<key> - <detail>' when
/// AGitHub (GitHub Actions annotation), plain text otherwise
function TextoAviso(AResultado: TResultadoPasso; AGitHub: boolean): string;
/// The INVENTARIO.md text, in Portuguese
function GerarInventario(ACob: TCobertura; AResultados: TObjectList): string;
/// The migrador folder, found from the executable or the current folder
function AcharPastaMigrador: string;
/// File of the API of a version inside the folder
function ArquivoApi(const APasta, ARotulo: string): string;

implementation

{ TDiferenca }

function TDiferenca.Tipo: string;
var
  vPos: integer;
begin
  vPos := Pos(':', Chave);
  if vPos = 0 then
    Result := Chave
  else
    Result := Copy(Chave, 1, vPos - 1);
end;

{ TDiferencas }

constructor TDiferencas.Create;
begin
  inherited Create;
  FLista := TObjectList.Create(True);
end;

destructor TDiferencas.Destroy;
begin
  FLista.Free;
  inherited Destroy;
end;

function TDiferencas.GetCount: integer;
begin
  Result := FLista.Count;
end;

function TDiferencas.GetItem(AIndex: integer): TDiferenca;
begin
  Result := TDiferenca(FLista[AIndex]);
end;

function TDiferencas.Buscar(const AChave: string): TDiferenca;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(FLista.Count) do
    if SameText(Itens[vInt].Chave, AChave) then
      Exit(Itens[vInt]);
end;

procedure TDiferencas.Add(const AChave, ADetalhe: string);
var
  vDif: TDiferenca;
begin
  if Buscar(AChave) <> nil then
    Exit;
  vDif := TDiferenca.Create;
  vDif.Chave := AChave;
  vDif.Detalhe := ADetalhe;
  FLista.Add(vDif);
end;

function ComparaChave(AItem1, AItem2: Pointer): integer;
begin
  Result := CompareText(TDiferenca(AItem1).Chave, TDiferenca(AItem2).Chave);
end;

procedure TDiferencas.Ordenar;
begin
  FLista.Sort(@ComparaChave);
end;

{ TCobPasso }

constructor TCobPasso.Create;
begin
  inherited Create;
  Itens := TObjectList.Create(True);
end;

destructor TCobPasso.Destroy;
begin
  Itens.Free;
  inherited Destroy;
end;

function TCobPasso.Rotulo: string;
begin
  Result := De + '>' + Para;
end;

{ TCobertura }

constructor TCobertura.Create;
begin
  inherited Create;
  FComportamento := TObjectList.Create(True);
  FErros := TStringList.Create;
  FPassos := TObjectList.Create(True);
end;

destructor TCobertura.Destroy;
begin
  FComportamento.Free;
  FErros.Free;
  FPassos.Free;
  inherited Destroy;
end;

function TCobertura.GetPasso(AIndex: integer): TCobPasso;
begin
  Result := TCobPasso(FPassos[AIndex]);
end;

function TCobertura.GetPassoCount: integer;
begin
  Result := FPassos.Count;
end;

function TCobertura.GetComp(AIndex: integer): TCobComportamento;
begin
  Result := TCobComportamento(FComportamento[AIndex]);
end;

function TCobertura.GetCompCount: integer;
begin
  Result := FComportamento.Count;
end;

function TCobertura.BuscarPasso(const ARotulo: string): TCobPasso;
var
  vInt: integer;
begin
  Result := nil;
  for vInt := 0 to Pred(FPassos.Count) do
    if SameText(Passos[vInt].Rotulo, ARotulo) then
      Exit(Passos[vInt]);
end;

procedure TCobertura.LerPasso(AObj: TJSONObject; AIndice: integer);
var
  vPasso: TCobPasso;
  vItens: TJSONArray;
  vItem: TJSONObject;
  vCob: TCobItem;
  vInt: integer;
  vOnde: string;
begin
  vPasso := TCobPasso.Create;
  FPassos.Add(vPasso);
  vPasso.De := AObj.Get('de', '');
  vPasso.Para := AObj.Get('para', '');
  vPasso.Fechado := AObj.Get('fechado', False);
  vOnde := Format('passos[%d]', [AIndice]);
  if (vPasso.De = '') or (vPasso.Para = '') then
    FErros.Add(vOnde + ': "de" e "para" sao obrigatorios');
  vItens := AObj.Get('itens', TJSONArray(nil));
  if vItens = nil then
    Exit;
  for vInt := 0 to Pred(vItens.Count) do
  begin
    vItem := vItens.Objects[vInt];
    vCob := TCobItem.Create;
    vPasso.Itens.Add(vCob);
    vCob.Chave := vItem.Get('chave', '');
    vCob.Regra := vItem.Get('regra', '');
    vCob.Justificativa := vItem.Get('justificativa', '');
    if vCob.Chave = '' then
      FErros.Add(Format('%s.itens[%d]: sem "chave"', [vOnde, vInt]))
    else if (vCob.Regra <> '') and (vCob.Justificativa <> '') then
      FErros.Add(Format('%s: "%s" tem regra e justificativa; so uma das duas',
                        [vPasso.Rotulo, vCob.Chave]))
    else if (vCob.Regra = '') and (vCob.Justificativa = '') then
      FErros.Add(Format('%s: "%s" nao tem regra nem justificativa',
                        [vPasso.Rotulo, vCob.Chave]));
  end;
end;

procedure TCobertura.LerComportamento(AObj: TJSONObject; AIndice: integer);
var
  vComp: TCobComportamento;
  vInt: integer;
begin
  vComp := TCobComportamento.Create;
  FComportamento.Add(vComp);
  vComp.Passo := AObj.Get('passo', '');
  vComp.Id := AObj.Get('id', '');
  vComp.Descricao := AObj.Get('descricao', '');
  vComp.Fazer := AObj.Get('fazer', '');
  vComp.Wiki := AObj.Get('wiki', '');
  if (vComp.Passo = '') or (vComp.Id = '') or (vComp.Descricao = '') then
    FErros.Add(Format('comportamento[%d]: "passo", "id" e "descricao" sao obrigatorios',
                      [AIndice]));
  for vInt := 0 to Pred(FComportamento.Count - 1) do
    if SameText(Comportamento[vInt].Id, vComp.Id) and (vComp.Id <> '') then
      FErros.Add('comportamento: id repetido ' + vComp.Id);
end;

function TCobertura.CarregarTexto(const ATexto: string): boolean;
var
  vDados: TJSONData;
  vRaiz: TJSONObject;
  vArr: TJSONArray;
  vInt: integer;
begin
  FErros.Clear;
  FPassos.Clear;
  FComportamento.Clear;
  try
    vDados := GetJSON(ATexto);
  except
    on E: Exception do
    begin
      FErros.Add('JSON invalido: ' + E.Message);
      Exit(False);
    end;
  end;
  try
    if not (vDados is TJSONObject) then
    begin
      FErros.Add('a raiz tem de ser um objeto');
      Exit(False);
    end;
    vRaiz := TJSONObject(vDados);
    vArr := vRaiz.Get('passos', TJSONArray(nil));
    if vArr <> nil then
      for vInt := 0 to Pred(vArr.Count) do
        LerPasso(vArr.Objects[vInt], vInt);
    vArr := vRaiz.Get('comportamento', TJSONArray(nil));
    if vArr <> nil then
      for vInt := 0 to Pred(vArr.Count) do
        LerComportamento(vArr.Objects[vInt], vInt);
  finally
    vDados.Free;
  end;
  Result := FErros.Count = 0;
end;

function TCobertura.CarregarArquivo(const AArquivo: string): boolean;
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
  Result := CarregarTexto(vTexto);
end;

{ TResultadoPasso }

constructor TResultadoPasso.Create(APasso: TCobPasso);
begin
  inherited Create;
  Passo := APasso;
  Diferencas := TDiferencas.Create;
  Cobertas := TObjectList.Create(True);
  Pendentes := TList.Create;
  Obsoletos := TStringList.Create;
end;

destructor TResultadoPasso.Destroy;
begin
  if PassoProprio then
    Passo.Free;
  Diferencas.Free;
  Cobertas.Free;
  Pendentes.Free;
  Obsoletos.Free;
  inherited Destroy;
end;

{ Glob }

function CasaGlob(const APadrao, ATexto: string): boolean;
var
  vPad, vTxt: string;
  vP, vT, vEstrela, vMarca: integer;
begin
  vPad := LowerCase(APadrao);
  vTxt := LowerCase(ATexto);
  vP := 1;
  vT := 1;
  vEstrela := 0;
  vMarca := 0;
  while vT <= Length(vTxt) do
  begin
    if (vP <= Length(vPad)) and ((vPad[vP] = '?') or (vPad[vP] = vTxt[vT])) then
    begin
      Inc(vP);
      Inc(vT);
    end
    else if (vP <= Length(vPad)) and (vPad[vP] = '*') then
    begin
      vEstrela := vP;
      vMarca := vT;
      Inc(vP);
    end
    else if vEstrela > 0 then
    begin
      vP := vEstrela + 1;
      Inc(vMarca);
      vT := vMarca;
    end
    else
      Exit(False);
  end;
  while (vP <= Length(vPad)) and (vPad[vP] = '*') do
    Inc(vP);
  Result := vP > Length(vPad);
end;

procedure Cobrir(APasso: TCobPasso; ADif: TDiferencas; AResultado: TResultadoPasso);
var
  vDif, vIt: integer;
  vItem: TCobItem;
  vUsado: array of boolean;
  vLinha: TLinhaCob;
  vAchou: boolean;
begin
  SetLength(vUsado, APasso.Itens.Count);
  for vDif := 0 to Pred(ADif.Count) do
  begin
    vAchou := False;
    for vIt := 0 to Pred(APasso.Itens.Count) do
    begin
      vItem := TCobItem(APasso.Itens[vIt]);
      if (vItem.Chave <> '') and CasaGlob(vItem.Chave, ADif[vDif].Chave) then
      begin
        vUsado[vIt] := True;
        if not vAchou then
        begin
          vAchou := True;
          vLinha := TLinhaCob.Create;
          vLinha.Dif := ADif[vDif];
          vLinha.Item := vItem;
          AResultado.Cobertas.Add(vLinha);
        end;
      end;
    end;
    if not vAchou then
      AResultado.Pendentes.Add(ADif[vDif]);
  end;
  for vIt := 0 to Pred(APasso.Itens.Count) do
    if not vUsado[vIt] then
      AResultado.Obsoletos.Add(TCobItem(APasso.Itens[vIt]).Chave);
end;

{ Comparison }

/// The member in the class or, failing that, in an ancestor of the API
function AcharMembro(AApi: TApi; AClasse: TApiClasse; const ANome: string): TApiMembro;
var
  vNivel: integer;
begin
  Result := nil;
  vNivel := 0;
  while (AClasse <> nil) and (vNivel < 60) do
  begin
    Result := AClasse.Buscar(ANome);
    if Result <> nil then
      Exit;
    AClasse := AApi.BuscarClasse(AClasse.Ancestral);
    Inc(vNivel);
  end;
end;

function OuNenhum(const ATexto: string): string;
begin
  if ATexto = '' then
    Result := '(nenhum)'
  else
    Result := ATexto;
end;

function JuntarLista(ALista: TStrings): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 0 to Pred(ALista.Count) do
  begin
    if Result <> '' then
      Result := Result + ' | ';
    Result := Result + ALista[vInt];
  end;
end;

/// True when some signature of A is not among those of B
function AssinaturaMudou(AAntiga, ANova: TStrings): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := 0 to Pred(AAntiga.Count) do
    if ANova.IndexOf(AAntiga[vInt]) < 0 then
      Exit(True);
end;

/// A default written as the name of a constant of the API counts as its value
function ValorDoPadrao(AApi: TApi; const APadrao: string): string;
var
  vInt: integer;
  vCon: TApiConstante;
begin
  Result := APadrao;
  if (APadrao = '') or not (APadrao[1] in ['A'..'Z', 'a'..'z', '_']) then
    Exit;
  for vInt := 0 to Pred(AApi.Constantes.Count) do
  begin
    vCon := TApiConstante(AApi.Constantes.Objects[vInt]);
    if SameText(vCon.Nome, APadrao) then
      Exit(vCon.Valor);
  end;
end;

procedure CompararPublicadas(AAntiga, ANova: TApi; ACA, ACN: TApiClasse;
  ADif: TDiferencas);
var
  vA, vN: TStringList;
  vInt, vPos: integer;
  vMA, vMN, vPub: TApiMembro;
  vAlvo: string;
begin
  vA := CriarListaMembros;
  vN := CriarListaMembros;
  try
    AAntiga.Publicadas(ACA, vA);
    ANova.Publicadas(ACN, vN);
    for vInt := 0 to Pred(vA.Count) do
    begin
      vMA := TApiMembro(vA.Objects[vInt]);
      vAlvo := ACA.Nome + '.' + vMA.Nome;
      vPos := vN.IndexOf(vMA.Nome);
      if vPos < 0 then
      begin
        vPub := AcharMembro(ANova, ACN, vMA.Nome);
        if vPub <> nil then
          ADif.Add('published-sumiu:' + vAlvo, 'agora ' + vPub.Cat + ', nao published')
        else
          ADif.Add('published-sumiu:' + vAlvo, 'tipo ' + OuNenhum(vMA.Tipo));
        Continue;
      end;
      vMN := TApiMembro(vN.Objects[vPos]);
      if (vMA.Tipo <> '') and (vMN.Tipo <> '') and not SameText(vMA.Tipo, vMN.Tipo) then
        ADif.Add('published-tipo:' + vAlvo, vMA.Tipo + ' -> ' + vMN.Tipo);
      if not SameText(ValorDoPadrao(AAntiga, vMA.Padrao), ValorDoPadrao(ANova, vMN.Padrao)) or
         not SameText(vMA.Armazenado, vMN.Armazenado) then
        ADif.Add('default-mudou:' + vAlvo,
                 'default ' + OuNenhum(ValorDoPadrao(AAntiga, vMA.Padrao)) + ' -> ' +
                 OuNenhum(ValorDoPadrao(ANova, vMN.Padrao)) +
                 '; stored ' + OuNenhum(vMA.Armazenado) + ' -> ' + OuNenhum(vMN.Armazenado));
    end;
  finally
    vA.Free;
    vN.Free;
  end;
end;

procedure CompararMembros(AAntiga, ANova: TApi; ACA, ACN: TApiClasse;
  ADif: TDiferencas);
var
  vInt: integer;
  vMA, vMN: TApiMembro;
  vAlvo: string;
begin
  for vInt := 0 to Pred(ACA.Membros.Count) do
  begin
    vMA := TApiMembro(ACA.Membros.Objects[vInt]);
    if vMA.Cat <> 'public' then
      Continue;
    vAlvo := ACA.Nome + '.' + vMA.Nome;
    vMN := AcharMembro(ANova, ACN, vMA.Nome);
    if vMN = nil then
      ADif.Add('publico-sumiu:' + vAlvo, vMA.Forma + ' ' + JuntarLista(vMA.Assinaturas) + vMA.Tipo)
    else if (vMA.Forma = 'metodo') and (vMN.Forma = 'metodo') then
    begin
      if AssinaturaMudou(vMA.Assinaturas, vMN.Assinaturas) then
        ADif.Add('assinatura-mudou:' + vAlvo,
                 JuntarLista(vMA.Assinaturas) + ' -> ' + JuntarLista(vMN.Assinaturas));
    end
    else if (vMA.Tipo <> '') and (vMN.Tipo <> '') and not SameText(vMA.Tipo, vMN.Tipo) then
      ADif.Add('assinatura-mudou:' + vAlvo, vMA.Forma + ' ' + vMA.Tipo + ' -> ' + vMN.Tipo)
    else if vMA.Forma <> vMN.Forma then
      ADif.Add('assinatura-mudou:' + vAlvo, vMA.Forma + ' -> ' + vMN.Forma);
  end;
end;

procedure CompararClasse(AAntiga, ANova: TApi; ACA: TApiClasse; ADif: TDiferencas);
var
  vCN: TApiClasse;
begin
  vCN := ANova.BuscarClasse(ACA.Nome);
  if vCN = nil then
  begin
    ADif.Add('classe-sumiu:' + ACA.Nome, 'unit ' + ACA.Unidade);
    Exit;
  end;
  if not SameText(ACA.Unidade, vCN.Unidade) then
    ADif.Add('classe-mudou-unit:' + ACA.Nome, ACA.Unidade + ' -> ' + vCN.Unidade);
  if not SameText(ACA.Ancestral, vCN.Ancestral) then
    ADif.Add('ancestral-mudou:' + ACA.Nome,
             OuNenhum(ACA.Ancestral) + ' -> ' + OuNenhum(vCN.Ancestral));
  CompararPublicadas(AAntiga, ANova, ACA, vCN, ADif);
  CompararMembros(AAntiga, ANova, ACA, vCN, ADif);
  if (AAntiga.Registradas.IndexOfName(ACA.Nome) >= 0) and
     (ANova.Registradas.IndexOfName(ACA.Nome) < 0) then
    ADif.Add('registro-sumiu:' + ACA.Nome,
             'estava na pagina ' + AAntiga.Registradas.Values[ACA.Nome]);
end;

/// Where the classes of a unit that went away are now
function OndeForamAsClasses(AAntiga, ANova: TApi; const AUnidade: string): string;
var
  vInt: integer;
  vCA, vCN: TApiClasse;
begin
  Result := 'sem equivalente';
  for vInt := 0 to Pred(AAntiga.Classes.Count) do
  begin
    vCA := TApiClasse(AAntiga.Classes.Objects[vInt]);
    if not SameText(vCA.Unidade, AUnidade) then
      Continue;
    vCN := ANova.BuscarClasse(vCA.Nome);
    if vCN <> nil then
      Exit('classes agora em ' + vCN.Unidade + ' (por exemplo ' + vCA.Nome + ')');
  end;
end;

procedure CompararUnits(AAntiga, ANova: TApi; ADif: TDiferencas);
var
  vInt: integer;
  vNome: string;
begin
  for vInt := 0 to Pred(AAntiga.Unidades.Count) do
  begin
    vNome := AAntiga.Unidades.Names[vInt];
    if ANova.Unidades.IndexOfName(vNome) < 0 then
      ADif.Add('unit-sumiu:' + vNome, OndeForamAsClasses(AAntiga, ANova, vNome));
  end;
end;

procedure CompararPastas(AAntiga, ANova: TApi; ADif: TDiferencas);
var
  vInt: integer;
  vNome, vVelha, vNova: string;
begin
  for vInt := 0 to Pred(AAntiga.Unidades.Count) do
  begin
    vNome := AAntiga.Unidades.Names[vInt];
    if ANova.Unidades.IndexOfName(vNome) < 0 then
      Continue;
    vVelha := AAntiga.PastaDaUnidade(vNome);
    vNova := ANova.PastaDaUnidade(vNome);
    if not SameText(vVelha, vNova) then
      ADif.Add('unit-pasta-mudou:' + vNome, vVelha + ' -> ' + vNova);
  end;
end;

procedure CompararTipos(AAntiga, ANova: TApi; ADif: TDiferencas);
var
  vInt, vVal: integer;
  vTA, vTN: TApiTipo;
begin
  for vInt := 0 to Pred(AAntiga.Tipos.Count) do
  begin
    vTA := TApiTipo(AAntiga.Tipos.Objects[vInt]);
    vTN := ANova.BuscarTipo(vTA.Nome);
    if vTN = nil then
    begin
      if ANova.BuscarClasse(vTA.Nome) = nil then
        ADif.Add('tipo-sumiu:' + vTA.Nome, vTA.Forma + ' da unit ' + vTA.Unidade);
      Continue;
    end;
    if (vTA.Forma = 'enum') and (vTN.Forma = 'enum') then
    begin
      for vVal := 0 to Pred(vTA.Valores.Count) do
        if vTN.Valores.IndexOf(vTA.Valores[vVal]) < 0 then
          ADif.Add('enum-valor-sumiu:' + vTA.Nome + '.' + vTA.Valores[vVal], 'valor removido');
    end
    else if (vTA.Forma = 'proc') and (vTN.Forma = 'proc') and
            not SameText(vTA.Detalhe, vTN.Detalhe) then
      ADif.Add('evento-mudou:' + vTA.Nome, vTA.Detalhe + ' -> ' + vTN.Detalhe);
  end;
end;

/// A routine with this name in another unit of the new API
function RotinaEmOutraUnit(ANova: TApi; const AUnidade, ANome: string): string;
var
  vInt, vQtd: integer;
  vRot: TApiRotina;
begin
  Result := '';
  vQtd := 0;
  for vInt := 0 to Pred(ANova.Rotinas.Count) do
  begin
    vRot := TApiRotina(ANova.Rotinas.Objects[vInt]);
    if SameText(vRot.Nome, ANome) and not SameText(vRot.Unidade, AUnidade) then
    begin
      Inc(vQtd);
      Result := 'agora em ' + vRot.Unidade;
    end;
  end;
  // um nome comum (Register) existe em muitas units: nao e prova de que mudou
  if vQtd > 1 then
    Result := Format('mesmo nome em %d outras units', [vQtd]);
end;

procedure CompararRotinas(AAntiga, ANova: TApi; ADif: TDiferencas);
var
  vInt: integer;
  vRA, vRN: TApiRotina;
  vOnde: string;
begin
  for vInt := 0 to Pred(AAntiga.Rotinas.Count) do
  begin
    vRA := TApiRotina(AAntiga.Rotinas.Objects[vInt]);
    vRN := ANova.BuscarRotina(vRA.Unidade, vRA.Nome);
    if vRN = nil then
    begin
      vOnde := RotinaEmOutraUnit(ANova, vRA.Unidade, vRA.Nome);
      if vOnde = '' then
        vOnde := 'sem equivalente';
      ADif.Add('rotina-sumiu:' + vRA.Unidade + '.' + vRA.Nome, vOnde);
    end
    else if AssinaturaMudou(vRA.Assinaturas, vRN.Assinaturas) then
      ADif.Add('rotina-mudou:' + vRA.Unidade + '.' + vRA.Nome,
               JuntarLista(vRA.Assinaturas) + ' -> ' + JuntarLista(vRN.Assinaturas));
  end;
end;

procedure CompararConstantes(AAntiga, ANova: TApi; ADif: TDiferencas);
var
  vInt, vOut, vQtd: integer;
  vCA, vCN: TApiConstante;
  vOnde: string;
begin
  for vInt := 0 to Pred(AAntiga.Constantes.Count) do
  begin
    vCA := TApiConstante(AAntiga.Constantes.Objects[vInt]);
    if ANova.BuscarConstante(vCA.Unidade, vCA.Nome) <> nil then
      Continue;
    vOnde := 'sem equivalente';
    vQtd := 0;
    for vOut := 0 to Pred(ANova.Constantes.Count) do
    begin
      vCN := TApiConstante(ANova.Constantes.Objects[vOut]);
      if SameText(vCN.Nome, vCA.Nome) then
      begin
        Inc(vQtd);
        vOnde := 'agora em ' + vCN.Unidade;
      end;
    end;
    if vQtd > 1 then
      vOnde := Format('mesmo nome em %d outras units', [vQtd]);
    ADif.Add('constante-sumiu:' + vCA.Unidade + '.' + vCA.Nome, vOnde);
  end;
end;

/// The packages of the new API that hold the unit, as text
function PacotesDaUnit(ANova: TApi; const ATipo, AUnidade: string): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 0 to Pred(ANova.PacoteCount) do
    if SameText(ANova.Pacotes[vInt].Tipo, ATipo) and
       (ANova.Pacotes[vInt].Unidades.IndexOf(AUnidade) >= 0) then
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + ANova.Pacotes[vInt].Nome;
    end;
end;

/// Where the units of a package that went away are now
function ParaOndeFoiOPacote(ANova: TApi; APacote: TApiPacote): string;
var
  vInt: integer;
  vOnde, vTodos: string;
begin
  vTodos := '';
  for vInt := 0 to Pred(APacote.Unidades.Count) do
  begin
    vOnde := PacotesDaUnit(ANova, APacote.Tipo, APacote.Unidades[vInt]);
    if (vOnde <> '') and (Pos(vOnde, vTodos) = 0) then
    begin
      if vTodos <> '' then
        vTodos := vTodos + '; ';
      vTodos := vTodos + vOnde;
    end;
  end;
  if vTodos = '' then
    Result := 'unidades em nenhum pacote'
  else
    Result := 'unidades agora em: ' + vTodos;
end;

procedure CompararPacote(AAntiga, ANova: TApi; APA: TApiPacote; ADif: TDiferencas);
var
  vPN: TApiPacote;
  vInt: integer;
  vOnde: string;
begin
  vPN := ANova.BuscarPacote(APA.Tipo, APA.Nome);
  if vPN = nil then
  begin
    ADif.Add('pacote-sumiu:' + APA.Tipo + '/' + APA.Nome, ParaOndeFoiOPacote(ANova, APA));
    Exit;
  end;
  for vInt := 0 to Pred(APA.Unidades.Count) do
  begin
    if (vPN.Unidades.IndexOf(APA.Unidades[vInt]) >= 0) or
       (ANova.Unidades.IndexOfName(APA.Unidades[vInt]) < 0) then
      Continue;
    vOnde := PacotesDaUnit(ANova, APA.Tipo, APA.Unidades[vInt]);
    if vOnde = '' then
      vOnde := 'em nenhum pacote'
    else
      vOnde := 'agora em ' + vOnde;
    ADif.Add('pacote-perdeu-unit:' + APA.Tipo + '/' + APA.Nome + '/' + APA.Unidades[vInt], vOnde);
  end;
end;

procedure CompararAPIs(AAntiga, ANova: TApi; ADif: TDiferencas);
var
  vInt: integer;
begin
  CompararUnits(AAntiga, ANova, ADif);
  CompararPastas(AAntiga, ANova, ADif);
  for vInt := 0 to Pred(AAntiga.Classes.Count) do
    CompararClasse(AAntiga, ANova, TApiClasse(AAntiga.Classes.Objects[vInt]), ADif);
  CompararTipos(AAntiga, ANova, ADif);
  CompararRotinas(AAntiga, ANova, ADif);
  CompararConstantes(AAntiga, ANova, ADif);
  for vInt := 0 to Pred(AAntiga.PacoteCount) do
    CompararPacote(AAntiga, ANova, AAntiga.Pacotes[vInt], ADif);
  ADif.Ordenar;
end;

{ Files }

function ArquivoApi(const APasta, ARotulo: string): string;
begin
  Result := IncludeTrailingPathDelimiter(APasta) + 'api-' + ARotulo + '.json';
end;

function ProcessarPasso(APasso: TCobPasso; const APastaApi: string): TResultadoPasso;
var
  vA, vN: TApi;
begin
  Result := TResultadoPasso.Create(APasso);
  if not FileExists(ArquivoApi(APastaApi, APasso.De)) then
    Result.Erro := 'falta ' + ArquivoApi(APastaApi, APasso.De)
  else if not FileExists(ArquivoApi(APastaApi, APasso.Para)) then
    Result.Erro := 'falta ' + ArquivoApi(APastaApi, APasso.Para);
  if Result.Erro <> '' then
    Exit;
  vA := TApi.Create;
  vN := TApi.Create;
  try
    vA.CarregarArquivo(ArquivoApi(APastaApi, APasso.De));
    vN.CarregarArquivo(ArquivoApi(APastaApi, APasso.Para));
    CompararAPIs(vA, vN, Result.Diferencas);
    Cobrir(APasso, Result.Diferencas, Result);
  finally
    vA.Free;
    vN.Free;
  end;
end;

function CompararComCobertura(AAntiga, ANova: TApi; ACob: TCobertura): TResultadoPasso;
var
  vPasso: TCobPasso;
begin
  vPasso := nil;
  if ACob <> nil then
    vPasso := ACob.BuscarPasso(AAntiga.Rotulo + '>' + ANova.Rotulo);
  if vPasso = nil then
  begin
    vPasso := TCobPasso.Create;
    vPasso.De := AAntiga.Rotulo;
    vPasso.Para := ANova.Rotulo;
    Result := TResultadoPasso.Create(vPasso);
    Result.PassoProprio := True;
  end
  else
    Result := TResultadoPasso.Create(vPasso);
  CompararAPIs(AAntiga, ANova, Result.Diferencas);
  Cobrir(vPasso, Result.Diferencas, Result);
end;

function TextoAviso(AResultado: TResultadoPasso; AGitHub: boolean): string;
var
  vInt: integer;
  vDif: TDiferenca;
  vLinha: string;
begin
  Result := '';
  for vInt := 0 to Pred(AResultado.Pendentes.Count) do
  begin
    vDif := TDiferenca(AResultado.Pendentes[vInt]);
    vLinha := vDif.Chave + ' - ' + vDif.Detalhe;
    if AGitHub then
    begin
      vLinha := StringReplace(vLinha, '%', '%25', [rfReplaceAll]);
      vLinha := StringReplace(vLinha, #13, '%0D', [rfReplaceAll]);
      vLinha := StringReplace(vLinha, #10, '%0A', [rfReplaceAll]);
      vLinha := '::warning::' + vLinha;
    end;
    Result := Result + vLinha + #10;
  end;
end;

function AcharPastaMigrador: string;

  function Subir(const AInicio: string): string;
  var
    vDir, vAnterior: string;
  begin
    Result := '';
    vDir := ExcludeTrailingPathDelimiter(ExpandFileName(AInicio));
    repeat
      if SameText(ExtractFileName(vDir), 'migrador') and
         DirectoryExists(vDir + PathDelim + 'regras') then
        Exit(vDir);
      if DirectoryExists(vDir + PathDelim + 'migrador' + PathDelim + 'regras') then
        Exit(vDir + PathDelim + 'migrador');
      vAnterior := vDir;
      vDir := ExcludeTrailingPathDelimiter(ExtractFilePath(vDir));
    until (vDir = '') or (vDir = vAnterior);
  end;

begin
  Result := Subir(ExtractFilePath(ParamStr(0)));
  if Result = '' then
    Result := Subir(GetCurrentDir);
end;

{ Report }

function Celula(const ATexto: string): string;
begin
  Result := StringReplace(ATexto, '|', '\|', [rfReplaceAll]);
  Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
end;

procedure Linha(ASaida: TStrings; const ATexto: string = '');
begin
  ASaida.Add(ATexto);
end;

procedure TabelaPendentes(ASaida: TStrings; AResultado: TResultadoPasso);
var
  vInt: integer;
  vDif: TDiferenca;
begin
  Linha(ASaida, '| chave | detalhe |');
  Linha(ASaida, '| --- | --- |');
  for vInt := 0 to Pred(AResultado.Pendentes.Count) do
  begin
    vDif := TDiferenca(AResultado.Pendentes[vInt]);
    Linha(ASaida, '| `' + Celula(vDif.Chave) + '` | ' + Celula(vDif.Detalhe) + ' |');
  end;
end;

procedure TabelaCobertas(ASaida: TStrings; AResultado: TResultadoPasso);
var
  vInt: integer;
  vLinha: TLinhaCob;
  vCobre: string;
begin
  Linha(ASaida, '| chave | detalhe | regra ou justificativa |');
  Linha(ASaida, '| --- | --- | --- |');
  for vInt := 0 to Pred(AResultado.Cobertas.Count) do
  begin
    vLinha := TLinhaCob(AResultado.Cobertas[vInt]);
    if vLinha.Item.Regra <> '' then
      vCobre := 'regra `' + vLinha.Item.Regra + '`'
    else
      vCobre := vLinha.Item.Justificativa;
    Linha(ASaida, '| `' + Celula(vLinha.Dif.Chave) + '` | ' + Celula(vLinha.Dif.Detalhe) +
                  ' | ' + Celula(vCobre) + ' |');
  end;
end;

procedure SecaoPasso(ASaida: TStrings; AResultado: TResultadoPasso);
var
  vInt: integer;
  vEstado: string;
begin
  if AResultado.Passo.Fechado then
    vEstado := 'fechado'
  else
    vEstado := 'aberto';
  Linha(ASaida, '## ' + AResultado.Passo.De + ' -> ' + AResultado.Passo.Para + ' (' + vEstado + ')');
  Linha(ASaida);
  if AResultado.Erro <> '' then
  begin
    Linha(ASaida, 'Nao foi possivel comparar: ' + AResultado.Erro + '.');
    Linha(ASaida);
    Exit;
  end;
  Linha(ASaida, Format('%d diferencas: %d cobertas, **%d pendentes**, %d itens de cobertura sem correspondencia.',
        [AResultado.Diferencas.Count, AResultado.Cobertas.Count,
         AResultado.Pendentes.Count, AResultado.Obsoletos.Count]));
  Linha(ASaida);
  if AResultado.Pendentes.Count > 0 then
  begin
    Linha(ASaida, '### Pendentes (' + IntToStr(AResultado.Pendentes.Count) + ')');
    Linha(ASaida);
    TabelaPendentes(ASaida, AResultado);
    Linha(ASaida);
  end;
  if AResultado.Cobertas.Count > 0 then
  begin
    Linha(ASaida, '### Cobertas (' + IntToStr(AResultado.Cobertas.Count) + ')');
    Linha(ASaida);
    TabelaCobertas(ASaida, AResultado);
    Linha(ASaida);
  end;
  if AResultado.Obsoletos.Count > 0 then
  begin
    Linha(ASaida, '### Itens de cobertura sem correspondencia (aviso)');
    Linha(ASaida);
    for vInt := 0 to Pred(AResultado.Obsoletos.Count) do
      Linha(ASaida, '- `' + AResultado.Obsoletos[vInt] + '`');
    Linha(ASaida);
  end;
end;

procedure SecaoComportamento(ASaida: TStrings; ACob: TCobertura);
var
  vInt: integer;
  vComp: TCobComportamento;
begin
  Linha(ASaida, '## Quebras de comportamento');
  Linha(ASaida);
  Linha(ASaida, 'Nao aparecem na API; vem da wiki e viram itens do relatorio do migrador.');
  Linha(ASaida);
  if ACob.CompCount = 0 then
  begin
    Linha(ASaida, '(nenhuma registrada)');
    Linha(ASaida);
    Exit;
  end;
  Linha(ASaida, '| passo | id | o que muda | o que fazer | wiki |');
  Linha(ASaida, '| --- | --- | --- | --- | --- |');
  for vInt := 0 to Pred(ACob.CompCount) do
  begin
    vComp := ACob.Comportamento[vInt];
    Linha(ASaida, '| ' + Celula(vComp.Passo) + ' | `' + Celula(vComp.Id) + '` | ' +
                  Celula(vComp.Descricao) + ' | ' + Celula(vComp.Fazer) + ' | ' +
                  Celula(vComp.Wiki) + ' |');
  end;
  Linha(ASaida);
end;

function GerarInventario(ACob: TCobertura; AResultados: TObjectList): string;
var
  vSaida: TStringList;
  vInt: integer;
begin
  vSaida := TStringList.Create;
  try
    Linha(vSaida, '# Inventario das quebras do RAL');
    Linha(vSaida);
    Linha(vSaida, 'Gerado por `apidiff inventario` a partir de `migrador/api/api-*.json` e ' +
                  '`migrador/regras/cobertura.json`. Nao edite a mao: mude a cobertura e gere de novo.');
    Linha(vSaida);
    for vInt := 0 to Pred(AResultados.Count) do
      SecaoPasso(vSaida, TResultadoPasso(AResultados[vInt]));
    SecaoComportamento(vSaida, ACob);
    Result := StringReplace(vSaida.Text, #13#10, #10, [rfReplaceAll]);
  finally
    vSaida.Free;
  end;
end;

end.
