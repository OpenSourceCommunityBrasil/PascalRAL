/// Offline validation of a migration result (stage M6): what the IDE would
/// refuse when it opened the migrated project, checked before it does. Works on
/// the migrated text (the Atual of each file, never the disk) against the API of
/// the target version (api-<version>.json):
/// - every `object X: TClass` whose class is RAL's exists there and is on the
///   palette ("Class not found");
/// - every property stored in the form of such an object is a published
///   property of the class ("Property X does not exist"), down to the second
///   level when the API has the class of the type;
/// - every RAL unit in a uses clause exists in the target version;
/// - every RAL package a project file names exists in the target catalog.
/// "Of the RAL" means: the name exists in the API of some version (1.0, 1.1,
/// 1.2 or 2.0). Anything else (a TButton, a VCL unit, a third party package) is
/// ignored. Each failure is a manual item of the rule `validacao`: it means a
/// rule is missing in the step files, and the project should be told.
unit RALMig.Validacao;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALMig.API, RALMig.Tipos;

const
  /// Id of the "rule" of every item this unit reports
  REGRA_VALIDACAO = 'validacao';

/// Validates the migrated texts of AConjunto against AApiPara and adds one
/// manual item per failure to AResultado. The APIs of the other versions (to
/// tell what belongs to the RAL) come from the embedded resources or from
/// APastaApi. Returns how many items it added. Forms that were left out are
/// ignored.
function ValidarResultado(AConjunto: TConjuntoArquivos; AApiPara: TApi;
  AResultado: TResultadoMigracao; const APastaApi: string = ''): integer;

/// Same, with the APIs that tell what is RAL's given by the caller (not owned)
function ValidarResultadoCom(AConjunto: TConjuntoArquivos; AApiPara: TApi;
  const AApisRAL: array of TApi; AResultado: TResultadoMigracao): integer;

implementation

uses
  RALMig.Texto, RALMig.Tokens, RALMig.Formulario, RALMig.ProjetoXML, RALMig.&Uses,
  RALMig.Deteccao;

resourcestring
  cmValClasse = 'Class %s not found: a classe %s do objeto %s não existe no RAL %s.';
  cmValNaoRegistrada = 'Class %s not found: a classe %s do objeto %s existe no RAL %s, ' +
    'mas não está registrada na paleta, e a IDE não a cria.';
  cmValPropriedade = 'Property %s does not exist: o objeto %s (%s) não tem a ' +
    'propriedade publicada %s no RAL %s.';
  cmValUnit = 'A unit %s é do RAL, mas não existe no RAL %s.';
  cmValPacote = 'O pacote %s é do RAL, mas não existe no RAL %s.';
  cmValFazer = 'Provável regra faltando: informe ao projeto (qual propriedade, ' +
    'classe, unit ou pacote mudou e para quê) e acerte esta ocorrência à mão.';

type
  { TValidador }

  TValidador = class
  private
    FApiPara: TApi;
    FCacheMembros: TStringList;
    FClasses: TStringList;
    FDeclaradas: TStringList;
    FConjunto: TConjuntoArquivos;
    FPacotes: TStringList;
    FResultado: TResultadoMigracao;
    FRotulo: string;
    FTotal: integer;
    FUnits: TStringList;
    function Membros(AClasse: TApiClasse): TStringList;
    function TerminalDaCadeia(AClasse: TApiClasse): string;
    function AceitaPropriedadeComum(const ATerminal, ANome: string): boolean;
    function LidaPeloFiler(AClasse: TApiClasse; const ANome: string): boolean;
    function VerificarSegmentos(AClasse: TApiClasse; const ANome: string): boolean;
    function EhClasseRAL(const ANome: string): boolean;
    function UnidadeDoProjeto(const ANome: string): boolean;
    procedure Falha(const AArquivo: string; ALinha: integer; const AProblema: string);
    procedure ValidarNo(AArq: TArquivoMigrado; ANo: TNoFormulario);
    procedure ValidarPropriedades(AArq: TArquivoMigrado; ANo: TNoFormulario;
      AClasse: TApiClasse);
    procedure ValidarFormulario(AArq: TArquivoMigrado);
    procedure ValidarUses(AArq: TArquivoMigrado);
    procedure ValidarPacotesXML(AArq: TArquivoMigrado);
    procedure ValidarPacotesDproj(AArq: TArquivoMigrado);
    procedure ValidarPacotesPastas(AArq: TArquivoMigrado);
    procedure ValidarPacotesDpk(AArq: TArquivoMigrado);
    procedure ValidarPacoteNome(AArq: TArquivoMigrado; ALinha: integer;
      const ANome: string);
    procedure ValidarArquivo(AArq: TArquivoMigrado);
    function NovoTexto(AArq: TArquivoMigrado): TTextoOriginal;
    procedure Acumular(AApi: TApi);
    procedure ColetarDeclaradas;
  public
    constructor Create(AConjunto: TConjuntoArquivos; AApiPara: TApi;
      const AApisRAL: array of TApi; AResultado: TResultadoMigracao);
    destructor Destroy; override;
    function Executar: integer;
  end;

const
  /// Classes at the root of the chain that a form file may store with the
  /// properties of their own: TComponent also stores Left/Top (DefineProperties)
  RAIZES_SEM_PROPRIEDADES: array[0..3] of string = (
    'TPersistent', 'TObject', 'TInterfacedPersistent', 'TCollectionItem');

function NovaSet: TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.CaseSensitive := False;
  Result.Duplicates := dupIgnore;
end;

function EhUmDe(const ANome: string; const ALista: array of string): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := Low(ALista) to High(ALista) do
    if SameText(ANome, ALista[vInt]) then
      Exit(True);
end;

{ TValidador }

constructor TValidador.Create(AConjunto: TConjuntoArquivos; AApiPara: TApi;
  const AApisRAL: array of TApi; AResultado: TResultadoMigracao);
var
  vInt: integer;
begin
  inherited Create;
  FConjunto := AConjunto;
  FApiPara := AApiPara;
  FResultado := AResultado;
  FCacheMembros := TStringList.Create;
  FCacheMembros.OwnsObjects := True;
  FCacheMembros.CaseSensitive := False;
  FClasses := NovaSet;
  FUnits := NovaSet;
  FPacotes := NovaSet;
  FRotulo := AResultado.Para;
  if FRotulo = '' then
    FRotulo := AApiPara.Rotulo;
  Acumular(AApiPara);
  for vInt := Low(AApisRAL) to High(AApisRAL) do
    Acumular(AApisRAL[vInt]);
end;

procedure TValidador.Acumular(AApi: TApi);
var
  vInt: integer;
begin
  if AApi = nil then
    Exit;
  FClasses.AddStrings(AApi.Classes);
  for vInt := 0 to AApi.Unidades.Count - 1 do
    FUnits.Add(AApi.Unidades.Names[vInt]);
  for vInt := 0 to AApi.PacoteCount - 1 do
    FPacotes.Add(AApi.Pacotes[vInt].Nome);
end;

destructor TValidador.Destroy;
begin
  FCacheMembros.Free;
  FClasses.Free;
  FDeclaradas.Free;
  FUnits.Free;
  FPacotes.Free;
  inherited Destroy;
end;

function TValidador.NovoTexto(AArq: TArquivoMigrado): TTextoOriginal;
begin
  Result := TTextoOriginal.Create;
  Result.CarregarBytes(AArq.Atual, AArq.Caminho);
end;

procedure TValidador.Falha(const AArquivo: string; ALinha: integer;
  const AProblema: string);
begin
  FResultado.AdicionarManual(REGRA_VALIDACAO, AArquivo, ALinha, AProblema,
    cmValFazer, '', '');
  Inc(FTotal);
end;

/// Classes the project's own sources declare (`TName = class`): a TRALGrid of
/// the user is not the RAL's
procedure TValidador.ColetarDeclaradas;
var
  vInt, vTok, vProx: integer;
  vTokens: TListaTokens;
begin
  FDeclaradas := NovaSet;
  for vInt := 0 to FConjunto.Count - 1 do
  begin
    if FConjunto[vInt].Ignorado or not (FConjunto[vInt].Tipo in [taPas, taDpr, taLpr]) then
      Continue;
    vTokens := TokenizarPascal(FConjunto[vInt].Atual);
    try
      for vTok := 0 to vTokens.Count - 1 do
      begin
        if not (vTokens[vTok].Tipo = ttIdentificador) then
          Continue;
        vProx := vTokens.Proximo(vTok);
        if (vProx < vTokens.Count) and vTokens.EhSimbolo(vProx, '=') then
        begin
          vProx := vTokens.Proximo(vProx);
          if (vProx < vTokens.Count) and vTokens.EhNomeIgual(vProx, 'packed') then
            vProx := vTokens.Proximo(vProx);
          if (vProx < vTokens.Count) and vTokens.EhNomeIgual(vProx, 'class') then
            FDeclaradas.Add(vTokens.Texto(vTok));
        end;
      end;
    finally
      vTokens.Free;
    end;
  end;
end;

function TValidador.EhClasseRAL(const ANome: string): boolean;
begin
  if FDeclaradas = nil then
    ColetarDeclaradas;
  if FDeclaradas.IndexOf(ANome) >= 0 then
    Exit(False);
  Result := (Copy(ANome, 1, 4) = 'TRAL') or (FClasses.IndexOf(ANome) >= 0) or
            (FApiPara.BuscarClasse(ANome) <> nil);
end;

function TValidador.UnidadeDoProjeto(const ANome: string): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := 0 to FConjunto.Count - 1 do
    if (FConjunto[vInt].Tipo = taPas) and
       SameText(ChangeFileExt(ExtractFileName(FConjunto[vInt].Caminho), ''), ANome) then
      Exit(True);
end;

function TValidador.Membros(AClasse: TApiClasse): TStringList;
var
  vIdx: integer;
begin
  vIdx := FCacheMembros.IndexOf(AClasse.Nome);
  if vIdx >= 0 then
    Exit(TStringList(FCacheMembros.Objects[vIdx]));
  Result := CriarListaMembros;
  FApiPara.Publicadas(AClasse, Result);
  FCacheMembros.AddObject(AClasse.Nome, Result);
end;

/// Name of the first ancestor that the API does not describe ('TComponent'...)
function TValidador.TerminalDaCadeia(AClasse: TApiClasse): string;
var
  vAtual: TApiClasse;
  vPassos: integer;
begin
  Result := '';
  vAtual := AClasse;
  vPassos := 0;
  while (vAtual <> nil) and (vPassos < 60) do
  begin
    Result := vAtual.Ancestral;
    vAtual := FApiPara.BuscarClasse(Result);
    Inc(vPassos);
  end;
end;

function TValidador.AceitaPropriedadeComum(const ATerminal, ANome: string): boolean;
begin
  Result := False;
  if SameText(ATerminal, 'TComponent') then
    Result := EhUmDe(ANome, ['Name', 'Tag', 'Left', 'Top'])
  else if (ATerminal <> '') and not EhUmDe(ATerminal, RAIZES_SEM_PROPRIEDADES) then
    // an ancestor outside the RAL that this unit knows nothing about (a
    // control, a data module): it may store anything
    Result := True;
end;

/// True when the class (or an ancestor) still reads the name through
/// DefineProperties: a form from before the rename opens without error
function TValidador.LidaPeloFiler(AClasse: TApiClasse; const ANome: string): boolean;
var
  vAtual: TApiClasse;
  vPassos: integer;
begin
  Result := False;
  vAtual := AClasse;
  vPassos := 0;
  while (vAtual <> nil) and (vPassos < 60) do
  begin
    if vAtual.LeAntigos.IndexOf(ANome) >= 0 then
      Exit(True);
    vAtual := FApiPara.BuscarClasse(vAtual.Ancestral);
    Inc(vPassos);
  end;
end;

/// True when the first segment of the name is a published property of the
/// class (or one every component has); the next segments are looked up in the
/// class of the type, when the API has it
function TValidador.VerificarSegmentos(AClasse: TApiClasse; const ANome: string): boolean;
var
  vPonto, vIdx: integer;
  vPrimeiro, vResto: string;
  vLista: TStringList;
  vSub: TApiClasse;
begin
  vPonto := Pos('.', ANome);
  if vPonto > 0 then
  begin
    vPrimeiro := Copy(ANome, 1, vPonto - 1);
    vResto := Copy(ANome, vPonto + 1, MaxInt);
  end
  else
  begin
    vPrimeiro := ANome;
    vResto := '';
  end;
  vLista := Membros(AClasse);
  vIdx := vLista.IndexOf(vPrimeiro);
  if (vIdx < 0) and LidaPeloFiler(AClasse, vPrimeiro) then
    Exit(True);
  if vIdx < 0 then
    Exit(AceitaPropriedadeComum(TerminalDaCadeia(AClasse), vPrimeiro));
  Result := True;
  if vResto = '' then
    Exit;
  vSub := FApiPara.BuscarClasse(TApiMembro(vLista.Objects[vIdx]).Tipo);
  if vSub <> nil then
    Result := VerificarSegmentos(vSub, vResto);
end;

procedure TValidador.ValidarPropriedades(AArq: TArquivoMigrado; ANo: TNoFormulario;
  AClasse: TApiClasse);
var
  vInt: integer;
  vProp: TPropriedadeForm;
begin
  for vInt := 0 to ANo.PropriedadeCount - 1 do
  begin
    vProp := ANo.Propriedades[vInt];
    if not VerificarSegmentos(AClasse, vProp.Nome) then
      Falha(AArq.Relativo, vProp.LinhaInicio,
        Format(cmValPropriedade, [vProp.Nome, ANo.Nome, ANo.Classe, vProp.Nome, FRotulo]));
  end;
end;

procedure TValidador.ValidarNo(AArq: TArquivoMigrado; ANo: TNoFormulario);
var
  vInt: integer;
  vClasse: TApiClasse;
begin
  if (ANo.Tipo = tnObject) and (ANo.Pai <> nil) and (ANo.Classe <> '') and
     EhClasseRAL(ANo.Classe) then
  begin
    vClasse := FApiPara.BuscarClasse(ANo.Classe);
    if vClasse = nil then
      Falha(AArq.Relativo, ANo.LinhaInicio,
        Format(cmValClasse, [ANo.Classe, ANo.Classe, ANo.Nome, FRotulo]))
    else
    begin
      if FApiPara.Registradas.IndexOfName(ANo.Classe) < 0 then
        Falha(AArq.Relativo, ANo.LinhaInicio,
          Format(cmValNaoRegistrada, [ANo.Classe, ANo.Classe, ANo.Nome, FRotulo]));
      ValidarPropriedades(AArq, ANo, vClasse);
    end;
  end;
  for vInt := 0 to ANo.FilhoCount - 1 do
    ValidarNo(AArq, ANo.Filhos[vInt]);
end;

procedure TValidador.ValidarFormulario(AArq: TArquivoMigrado);
var
  vTexto: TTextoOriginal;
  vForm: TFormulario;
  vInt: integer;
begin
  vTexto := NovoTexto(AArq);
  try
    try
      vForm := TFormulario.Create(vTexto);
    except
      on EFormularioInvalido do
        Exit;
    end;
    try
      for vInt := 0 to vForm.RaizCount - 1 do
        ValidarNo(AArq, vForm.Raizes[vInt]);
    finally
      vForm.Free;
    end;
  finally
    vTexto.Free;
  end;
end;

procedure TValidador.ValidarUses(AArq: TArquivoMigrado);
var
  vTexto: TTextoOriginal;
  vEditor: TEditorUses;
  vCl, vIt: integer;
  vItem: TItemUses;
begin
  vTexto := NovoTexto(AArq);
  try
    try
      vEditor := TEditorUses.Create(vTexto);
    except
      on EUsesErro do
        Exit;
    end;
    try
      for vCl := 0 to vEditor.Count - 1 do
      begin
        if not (vEditor.Clausulas[vCl].Secao in [suInterface, suImplementation, suPrograma]) then
          Continue;
        for vIt := 0 to vEditor.Clausulas[vCl].Count - 1 do
        begin
          vItem := vEditor.Clausulas[vCl].Item(vIt);
          if (FUnits.IndexOf(vItem.Nome) >= 0) and (FApiPara.ArquivoDaUnidade(vItem.Nome) = '')
             and not UnidadeDoProjeto(vItem.Nome) then
            Falha(AArq.Relativo, vItem.Linha, Format(cmValUnit, [vItem.Nome, FRotulo]));
        end;
      end;
    finally
      vEditor.Free;
    end;
  finally
    vTexto.Free;
  end;
end;

procedure TValidador.ValidarPacoteNome(AArq: TArquivoMigrado; ALinha: integer;
  const ANome: string);
begin
  if (Pos('$(', ANome) = 0) and (FPacotes.IndexOf(ANome) >= 0) and
     (FApiPara.BuscarPacote('delphi', ANome) = nil) and
     (FApiPara.BuscarPacote('lazarus', ANome) = nil) then
    Falha(AArq.Relativo, ALinha, Format(cmValPacote, [ANome, FRotulo]));
end;

procedure TValidador.ValidarPacotesDproj(AArq: TArquivoMigrado);
var
  vXml: TProjetoXML;
  vTexto: TTextoOriginal;
  vLista: TList;
  vNomes: TStringList;
  vInt, vNome: integer;
begin
  vTexto := NovoTexto(AArq);
  try
    try
      vXml := TProjetoXML.Create(vTexto);
    except
      on EXmlInvalido do
        Exit;
    end;
    vLista := vXml.Buscar('//DCC_UsePackage');
    vNomes := TStringList.Create;
    try
      vNomes.Delimiter := ';';
      vNomes.StrictDelimiter := True;
      for vInt := 0 to vLista.Count - 1 do
      begin
        vNomes.DelimitedText := vXml.ValorTexto(TElementoXML(vLista[vInt]));
        for vNome := 0 to vNomes.Count - 1 do
          ValidarPacoteNome(AArq, vTexto.LinhaDoOffset(TElementoXML(vLista[vInt]).InicioTag),
            Trim(vNomes[vNome]));
      end;
    finally
      vNomes.Free;
      vLista.Free;
      vXml.Free;
    end;
  finally
    vTexto.Free;
  end;
end;

procedure TValidador.ValidarPacotesPastas(AArq: TArquivoMigrado);
var
  vXml: TProjetoXML;
  vTexto: TTextoOriginal;
  vCont, vNome: TElementoXML;
  vInt: integer;
begin
  vTexto := NovoTexto(AArq);
  try
    try
      vXml := TProjetoXML.Create(vTexto);
    except
      on EXmlInvalido do
        Exit;
    end;
    try
      vCont := vXml.ContainerDePacotes;
      if vCont = nil then
        Exit;
      for vInt := 0 to vCont.FilhoCount - 1 do
      begin
        vNome := vCont.Filhos[vInt].Filho('PackageName');
        if vNome <> nil then
          ValidarPacoteNome(AArq, vTexto.LinhaDoOffset(vNome.InicioTag),
            vXml.ValorAtributo(vNome, 'Value'));
      end;
    finally
      vXml.Free;
    end;
  finally
    vTexto.Free;
  end;
end;

procedure TValidador.ValidarPacotesDpk(AArq: TArquivoMigrado);
var
  vTexto: TTextoOriginal;
  vEditor: TEditorUses;
  vCl, vIt: integer;
begin
  vTexto := NovoTexto(AArq);
  try
    try
      vEditor := TEditorUses.Create(vTexto);
    except
      on EUsesErro do
        Exit;
    end;
    try
      for vCl := 0 to vEditor.Count - 1 do
        if vEditor.Clausulas[vCl].Secao = suRequires then
          for vIt := 0 to vEditor.Clausulas[vCl].Count - 1 do
            ValidarPacoteNome(AArq, vEditor.Clausulas[vCl].Item(vIt).Linha,
              vEditor.Clausulas[vCl].Item(vIt).Nome);
    finally
      vEditor.Free;
    end;
  finally
    vTexto.Free;
  end;
end;

procedure TValidador.ValidarPacotesXML(AArq: TArquivoMigrado);
begin
  if AArq.Tipo = taDproj then
    ValidarPacotesDproj(AArq)
  else
    ValidarPacotesPastas(AArq);
end;

procedure TValidador.ValidarArquivo(AArq: TArquivoMigrado);
begin
  case AArq.Tipo of
    taForm: ValidarFormulario(AArq);
    taPas, taDpr, taLpr:
      ValidarUses(AArq);
    taDpk:
      begin
        ValidarUses(AArq);
        ValidarPacotesDpk(AArq);
      end;
    taDproj, taLpi, taLpk:
      ValidarPacotesXML(AArq);
  end;
end;

function TValidador.Executar: integer;
var
  vInt: integer;
begin
  FTotal := 0;
  for vInt := 0 to FConjunto.Count - 1 do
    if not FConjunto[vInt].Ignorado then
      ValidarArquivo(FConjunto[vInt]);
  Result := FTotal;
end;

function ValidarResultadoCom(AConjunto: TConjuntoArquivos; AApiPara: TApi;
  const AApisRAL: array of TApi; AResultado: TResultadoMigracao): integer;
var
  vValidador: TValidador;
begin
  vValidador := TValidador.Create(AConjunto, AApiPara, AApisRAL, AResultado);
  try
    Result := vValidador.Executar;
  finally
    vValidador.Free;
  end;
end;

function ValidarResultado(AConjunto: TConjuntoArquivos; AApiPara: TApi;
  AResultado: TResultadoMigracao; const APastaApi: string): integer;
var
  vApis: array[0..3] of TApi;
  vInt: integer;
begin
  for vInt := Low(VersoesDeApi) to High(VersoesDeApi) do
    vApis[vInt] := CarregarApiDaVersao(VersoesDeApi[vInt], APastaApi);
  try
    Result := ValidarResultadoCom(AConjunto, AApiPara, vApis, AResultado);
  finally
    for vInt := Low(vApis) to High(vApis) do
      vApis[vInt].Free;
  end;
end;

end.
