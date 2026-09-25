/// What each IDE can install, before trying. Most of it comes from the disk,
/// with no manifest at all, so that a new package does not depend on someone
/// remembering to write a rule:
/// - Delphi, packages: what requires asks (and the Indy/FireDAC the .dproj uses)
///   must exist as a .dcp in the IDE. XE2 has no FireDAC.dcp.
/// - Delphi, units: the RTL units the package units use must exist as .dcu in
///   lib\win32\release. NetHttpRAL uses System.Net.HttpClient, which only exists
///   from XE8 on. {$IFDEF}s are evaluated with that IDE's symbols (DELPHIXE4UP
///   of PascalRAL.inc, VER230, MSWINDOWS). Unknown symbols and {$IF expression}
///   leave the block out: when in doubt, nothing is blamed.
/// - Dependency: the version per compiler comes from the recipe
///   ("fonte.versoes"); when no version works, the package that needs it stays
///   out, unless the IDE already has the dependency.
/// What cannot be deduced comes from the manifest: an IDE range per package and
/// the dependency version *this* RAL version requires. The manifest belongs to
/// the RAL version: a ralinstaller.json at its root wins; without it, the copy
/// embedded in the installer (RCDATA MANIFESTO_RAL, from manifesto/ral.json).
///
///   {
///     "formato": 1,
///     "pacotes": [ { "pacote": "SynopseRAL", "delphi-min": "2009",
///                    "motivo": "..." } ],
///     "dependencias": [ { "receita": "mORMot2", "versao": "master" } ]
///   }
unit RALInst.Compatibilidade;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, fpjson,
  RALInst.Catalogo, RALInst.IDE, RALInst.Receitas;

type
  /// The package only holds inside the range.
  TRegraPacote = class
  public
    Condicao: TCondicaoIDE;
    Motivo: string;
    Pacote: string;
    constructor Create;
    destructor Destroy; override;
  end;

  /// Rules of one RAL version that cannot be read from its packages.
  TManifesto = class
  private
    FDependencias: TObjectList;
    FErros: TStringList;
    FOrigem: string;
    FRegras: TObjectList;
    function GetRegra(AIndex: integer): TRegraPacote;
    function GetTotalRegras: integer;
  public
    constructor Create;
    destructor Destroy; override;
    /// A file on disk (the test tools, which have no resource)
    function CarregarArquivo(const AArquivo: string): boolean;
    /// The RAL version's own (ralinstaller.json at its root), else the embedded
    procedure CarregarPadrao(AOrigem: TOrigemArquivos);
    /// The embedded copy (RCDATA MANIFESTO_RAL)
    procedure CarregarRecurso;
    /// False (and the reason in Erros) when it is not a valid manifest; nothing
    /// of a refused manifest is kept
    function CarregarTexto(const ATexto, AOrigem: string): boolean;
    procedure Limpar;
    /// Version rules of this recipe, in order (TRegraVersao)
    procedure RegrasDependencia(const AReceita: string; ALista: TList);

    property Erros: TStringList read FErros;
    /// Where it came from: 'ralinstaller.json of the version', 'embedded'
    property Origem: string read FOrigem;
    property Regras[AIndex: integer]: TRegraPacote read GetRegra;
    property TotalRegras: integer read GetTotalRegras;
  end;

  /// Where the dependency already is in this IDE ('' if it is not): the
  /// detection of each kind of IDE's engine.
  TDetectarDependencia = function(AIDE: TIDEInstance;
    AReceita: TReceita): string of object;
  /// Root of the installed copy of the dependency in this IDE ('' if unknown).
  TPastaDependencia = function(AIDE: TIDEInstance; AReceita: TReceita): string of object;

  /// Decides what fits in each IDE and which dependency version it takes.
  TCompatibilidade = class
  private
    /// file -> exists (FileExists cache per IDE)
    FArquivos: TStringList;
    FCatalogo: TCatalogo;
    FDetectar: TDetectarDependencia;
    FManifesto: TManifesto;
    FPastaInstalada: TPastaDependencia;
    FReceitas: TReceitas;
    /// unit file -> RTL units used outside {$IFDEF}
    FUsos: TStringList;
    /// FileExists with a cache
    function Existe(const AArquivo: string): boolean;
    /// Does a recipe provide this package name?
    function FornecidoPorReceita(ATipo: TTipoPacote; const ANome: string): boolean;
    /// Missing .dcp or RTL .dcu in a Delphi
    function MotivoDelphi(APacote: TPacote; AIDE: TIDEInstance): string;
    /// A dependency that no version satisfies, or an unusable installed copy
    function MotivoDependencias(APacote: TPacote; AIDE: TIDEInstance): string;
    /// RTL units a unit uses with this IDE's symbols (cached)
    function UsosDaUnidade(const AArquivo: string; AIDE: TIDEInstance): TStrings;
  public
    /// Owns nothing; manifest and recipes may be nil
    constructor Create(ACatalogo: TCatalogo; AManifesto: TManifesto;
      AReceitas: TReceitas);
    destructor Destroy; override;
    /// Does the installed copy (in APasta) work in this IDE? '' when it does,
    /// or no check blames it; else the recipe's warning
    function AvisoInstalada(AReceita: TReceita; AIDE: TIDEInstance;
      const APasta: string): string;
    /// Removes from the list (in dependency order) what does not fit the IDE
    /// and whoever depends on it; AFora gets name=reason
    procedure Filtrar(ALista: TList; AIDE: TIDEInstance; AFora: TStrings);
    /// '' when the package itself fits the IDE; else why not. What it
    /// requires from RAL itself is Filtrar's business
    function Motivo(APacote: TPacote; AIDE: TIDEInstance): string;
    /// The dependency version for this IDE: the manifest's (of this RAL
    /// version), the recipe's by range, or the recipe default. '' and AMotivo
    /// set when no version works
    function VersaoDependencia(AReceita: TReceita; AIDE: TIDEInstance;
      out AMotivo: string): string;

    property Detectar: TDetectarDependencia read FDetectar write FDetectar;
    property PastaInstalada: TPastaDependencia read FPastaInstalada write FPastaInstalada;
  end;

/// Key of PastasDependencias: the same dependency may be downloaded in two
/// versions (stable Zeos for Delphi, 8.0-patches for FPC 3.3)
function ChaveDependencia(const AReceita, AVersao: string): string;
/// The symbols PascalRAL.inc and the compiler define for this Delphi IDE
/// (DELPHIXE4UP, VER360, MSWINDOWS...), for Win32
procedure SimbolosDelphi(AIDE: TIDEInstance; ADefinidos, ANaoDefinidos: TStrings);
/// The units the source uses with the known symbols: {$IFDEF X} with X in
/// ADefinidos counts, in ANaoDefinidos does not (and its {$ELSE} counts); a
/// symbol in neither, and {$IF expression}, leave the block out
procedure UsosEfetivos(const AFonte: string; ADefinidos, ANaoDefinidos: TStrings;
  ALista: TStrings);
/// The units the source uses outside any {$IF...}: comments, strings and
/// directives are respected
procedure UsosIncondicionais(const AFonte: string; ALista: TStrings);

implementation

uses
  jsonparser, StrUtils,
  RALInst.Mensagens;

const
  // so unidades destes namespaces sao conferidas contra a IDE: o resto e do
  // RAL ou de terceiros (mormot.*, Z*, Id*), e isso e assunto da F7
  NamespacesRTL: array[0..14] of string = (
    'System', 'Winapi', 'Vcl', 'Data', 'FireDAC', 'Xml', 'Web', 'Soap', 'REST',
    'Datasnap', 'FMX', 'Posix', 'Androidapi', 'Macapi', 'iOSapi'
  );

function ChaveDependencia(const AReceita, AVersao: string): string;
begin
  Result := AReceita + '@' + AVersao;
end;

procedure UsosIncondicionais(const AFonte: string; ALista: TStrings);
begin
  UsosEfetivos(AFonte, nil, nil, ALista);
end;

procedure UsosEfetivos(const AFonte: string; ADefinidos, ANaoDefinidos: TStrings;
  ALista: TStrings);
var
  vPos, vTam, vInicio: integer;
  vEmUses: boolean;
  vToken, vDiretiva, vArgumento: string;
  // um caractere por {$IF...} aberto: A ativo, I inativo, D desconhecido
  vPilha: string;
  vDef, vNaoDef: TStringList;

  function Estado(const ASimbolo: string; ANegado: boolean): char;
  begin
    if vDef.IndexOf(ASimbolo) >= 0 then
      Result := 'A'
    else if vNaoDef.IndexOf(ASimbolo) >= 0 then
      Result := 'I'
    else
      Exit('D');
    if ANegado then
      if Result = 'A' then
        Result := 'I'
      else
        Result := 'A';
  end;

  function Ativo: boolean;
  begin
    Result := StringReplace(vPilha, 'A', '', [rfReplaceAll]) = '';
  end;

begin
  vTam := Length(AFonte);
  vPos := 1;
  vPilha := '';
  vEmUses := False;
  vDef := TStringList.Create;
  vNaoDef := TStringList.Create;
  try
    vDef.CaseSensitive := False;
    vNaoDef.CaseSensitive := False;
    if ADefinidos <> nil then
      vDef.AddStrings(ADefinidos);
    if ANaoDefinidos <> nil then
      vNaoDef.AddStrings(ANaoDefinidos);
  while vPos <= vTam do
  begin
    case AFonte[vPos] of
      '{':
        begin
          vInicio := vPos;
          while (vPos <= vTam) and (AFonte[vPos] <> '}') do
            Inc(vPos);
          if (vInicio < vTam) and (AFonte[vInicio + 1] = '$') then
          begin
            // {$IFDEF X}, {$IF Defined(X)}, {$ENDIF}: nome e primeiro argumento
            vDiretiva := Trim(StringReplace(Copy(AFonte, vInicio + 2,
                                                 vPos - vInicio - 2),
                                            #9, ' ', [rfReplaceAll]));
            vArgumento := Trim(Copy(vDiretiva, Pos(' ', vDiretiva + ' ') + 1, MaxInt));
            vArgumento := Copy(vArgumento, 1, Pos(' ', vArgumento + ' ') - 1);
            vDiretiva := UpperCase(Copy(vDiretiva, 1, Pos(' ', vDiretiva + ' ') - 1));
            if vDiretiva = 'IFDEF' then
              vPilha := vPilha + Estado(vArgumento, False)
            else if vDiretiva = 'IFNDEF' then
              vPilha := vPilha + Estado(vArgumento, True)
            else if (vDiretiva = 'IF') or (vDiretiva = 'IFOPT') then
              // expressao: o instalador nao avalia
              vPilha := vPilha + 'D'
            else if (vDiretiva = 'ELSE') and (vPilha <> '') then
            begin
              case vPilha[Length(vPilha)] of
                'A': vPilha[Length(vPilha)] := 'I';
                'I': vPilha[Length(vPilha)] := 'A';
              end;
            end
            else if (vDiretiva = 'ELSEIF') and (vPilha <> '') then
              vPilha[Length(vPilha)] := 'D'
            else if ((vDiretiva = 'ENDIF') or (vDiretiva = 'IFEND')) and
                    (vPilha <> '') then
              Delete(vPilha, Length(vPilha), 1)
            else if (vDiretiva = 'DEFINE') and Ativo and (vArgumento <> '') then
            begin
              vDef.Add(vArgumento);
              if vNaoDef.IndexOf(vArgumento) >= 0 then
                vNaoDef.Delete(vNaoDef.IndexOf(vArgumento));
            end
            else if (vDiretiva = 'UNDEF') and Ativo and (vArgumento <> '') then
            begin
              vNaoDef.Add(vArgumento);
              if vDef.IndexOf(vArgumento) >= 0 then
                vDef.Delete(vDef.IndexOf(vArgumento));
            end;
          end;
          Inc(vPos);
        end;
      '(':
        if (vPos < vTam) and (AFonte[vPos + 1] = '*') then
        begin
          vPos := PosEx('*)', AFonte, vPos + 2);
          if vPos = 0 then
            Exit;
          Inc(vPos, 2);
        end
        else
          Inc(vPos);
      '/':
        if (vPos < vTam) and (AFonte[vPos + 1] = '/') then
        begin
          while (vPos <= vTam) and not (AFonte[vPos] in [#10, #13]) do
            Inc(vPos);
        end
        else
          Inc(vPos);
      '''':
        begin
          Inc(vPos);
          while (vPos <= vTam) and (AFonte[vPos] <> '''') do
            Inc(vPos);
          Inc(vPos);
        end;
      ';':
        begin
          vEmUses := False;
          Inc(vPos);
        end;
      'A'..'Z', 'a'..'z', '_':
        begin
          vInicio := vPos;
          while (vPos <= vTam) and
                (AFonte[vPos] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) do
            Inc(vPos);
          vToken := Copy(AFonte, vInicio, vPos - vInicio);
          if SameText(vToken, 'uses') then
            vEmUses := True
          else if vEmUses and Ativo and not SameText(vToken, 'in') and
                  (ALista.IndexOf(vToken) < 0) then
            ALista.Add(vToken);
        end;
    else
      Inc(vPos);
    end;
  end;
  finally
    vNaoDef.Free;
    vDef.Free;
  end;
end;

procedure SimbolosDelphi(AIDE: TIDEInstance; ADefinidos, ANaoDefinidos: TStrings);
var
  vInt: integer;
  vNome: string;
begin
  // os DELPHI<x>UP do PascalRAL.inc, pela versao da IDE: 'Delphi 10 Seattle'
  // -> DELPHI10_0UP, 'Delphi 10.1 Berlin' -> DELPHI10_1UP, 'Delphi XE2' ->
  // DELPHIXE2UP. O Delphi 8 (.NET) nao esta na tabela, mas o .inc o define do
  // 2005 em diante
  for vInt := Low(DelphiProdutos) to High(DelphiProdutos) do
  begin
    vNome := Copy(DelphiProdutos[vInt].Nome, 8, MaxInt);
    vNome := Copy(vNome, 1, Pos(' ', vNome + ' ') - 1);
    vNome := StringReplace(vNome, '.', '_', [rfReplaceAll]);
    if vNome = '10' then
      vNome := '10_0';
    if CompararVersoes(DelphiProdutos[vInt].VersaoNum, AIDE.Versao) <= 0 then
      ADefinidos.Add('DELPHI' + vNome + 'UP')
    else
      ANaoDefinidos.Add('DELPHI' + vNome + 'UP');
  end;
  if CompararVersoes(AIDE.Versao, '17.0') >= 0 then
    ADefinidos.Add('DELPHI8UP')
  else
    ANaoDefinidos.Add('DELPHI8UP');
  if CompararVersoes(AIDE.Versao, '20.0') >= 0 then
    ADefinidos.Add('UNICODE')
  else
    ANaoDefinidos.Add('UNICODE');
  if AIDE.VersaoCompilador <> '' then
    ADefinidos.Add(AIDE.VersaoCompilador);
  // a conferencia e da plataforma Win32, a do design-time
  ADefinidos.AddStrings(['MSWINDOWS', 'WINDOWS', 'WIN32', 'CPUX86', 'CPU386',
                         'CONDITIONALEXPRESSIONS']);
  ANaoDefinidos.AddStrings(['FPC', 'LINUX', 'UNIX', 'POSIX', 'DARWIN', 'MACOS',
                            'ANDROID', 'IOS', 'WIN64', 'CPUX64', 'CPU64', 'CLR',
                            'LAZARUS']);
end;

function DaRTL(const AUnidade: string): boolean;
var
  vPrefixo: string;
begin
  Result := False;
  if Pos('.', AUnidade) = 0 then
    Exit;
  vPrefixo := Copy(AUnidade, 1, Pos('.', AUnidade) - 1);
  Result := AnsiIndexText(vPrefixo, NamespacesRTL) >= 0;
end;

{ TRegraPacote }

constructor TRegraPacote.Create;
begin
  inherited Create;
  Condicao := TCondicaoIDE.Create;
end;

destructor TRegraPacote.Destroy;
begin
  Condicao.Free;
  inherited Destroy;
end;

{ TManifesto }

constructor TManifesto.Create;
begin
  inherited Create;
  FRegras := TObjectList.Create(True);
  FDependencias := TObjectList.Create(True);
  FErros := TStringList.Create;
end;

destructor TManifesto.Destroy;
begin
  FErros.Free;
  FDependencias.Free;
  FRegras.Free;
  inherited Destroy;
end;

function TManifesto.GetRegra(AIndex: integer): TRegraPacote;
begin
  Result := TRegraPacote(FRegras[AIndex]);
end;

function TManifesto.GetTotalRegras: integer;
begin
  Result := FRegras.Count;
end;

procedure TManifesto.Limpar;
begin
  FRegras.Clear;
  FDependencias.Clear;
  FOrigem := '';
end;

function TManifesto.CarregarTexto(const ATexto, AOrigem: string): boolean;
var
  vJSON: TJSONData;
  vRaiz, vItem: TJSONObject;
  vLista: TJSONArray;
  vInt, vCampo: integer;
  vErro, vNome: string;
  vRegra: TRegraPacote;
  vVersao: TRegraVersao;
  vRegras, vDeps: TObjectList;
  vAchou: boolean;
begin
  Result := False;
  vErro := '';
  vJSON := nil;
  vRegras := TObjectList.Create(True);
  vDeps := TObjectList.Create(True);
  try
    try
      vJSON := GetJSON(ATexto);
    except
      on E: Exception do
      begin
        FErros.Add(Format(emJSONInvalido, [AOrigem, E.Message]));
        Exit;
      end;
    end;
    if not (vJSON is TJSONObject) then
    begin
      FErros.Add(Format(emManifestoNaoObjeto, [AOrigem]));
      Exit;
    end;
    vRaiz := TJSONObject(vJSON);
    for vInt := 0 to Pred(vRaiz.Count) do
      if AnsiIndexStr(vRaiz.Names[vInt],
                      ['formato', 'pacotes', 'dependencias', 'comentario']) < 0 then
        vErro := Format(emCampoDesconhecido, [vRaiz.Names[vInt], 'manifesto']);
    if (vErro = '') and (vRaiz.Get('formato', 0) <> 1) then
      vErro := Format(emManifestoFormato, [vRaiz.Get('formato', 0)]);

    vLista := vRaiz.Get('pacotes', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
      begin
        if vErro <> '' then
          Break;
        if not (vLista.Items[vInt] is TJSONObject) then
        begin
          vErro := Format(emReceitaItemObjeto, ['pacotes']);
          Break;
        end;
        vItem := vLista.Objects[vInt];
        for vCampo := 0 to Pred(vItem.Count) do
        begin
          vNome := vItem.Names[vCampo];
          vAchou := AnsiIndexStr(vNome, ['pacote', 'motivo', 'comentario']) >= 0;
          vAchou := vAchou or (AnsiIndexStr(vNome, CamposCondicao) >= 0);
          if not vAchou then
            vErro := Format(emCampoDesconhecido, [vNome, 'pacotes']);
        end;
        vRegra := TRegraPacote.Create;
        vRegras.Add(vRegra);
        vRegra.Pacote := vItem.Get('pacote', '');
        vRegra.Motivo := vItem.Get('motivo', '');
        if vRegra.Pacote = '' then
          vErro := emManifestoSemPacote
        else if vErro = '' then
          vRegra.Condicao.Ler(vItem, vErro);
      end;

    vLista := vRaiz.Get('dependencias', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
      begin
        if vErro <> '' then
          Break;
        if not (vLista.Items[vInt] is TJSONObject) then
        begin
          vErro := Format(emReceitaItemObjeto, ['dependencias']);
          Break;
        end;
        vItem := vLista.Objects[vInt];
        vVersao := LerRegraVersao(vItem, 'dependencias', ['receita'], vErro);
        if vVersao = nil then
          Break;
        // a receita a que a regra se refere vai no campo Origem da regra
        vVersao.Origem := vItem.Get('receita', '');
        vDeps.Add(vVersao);
        if vVersao.Origem = '' then
          vErro := emManifestoSemReceita;
      end;

    if vErro <> '' then
    begin
      FErros.Add(Format(emManifestoRecusado, [AOrigem, vErro]));
      Exit;
    end;

    Limpar;
    FOrigem := AOrigem;
    vRegras.OwnsObjects := False;
    for vInt := 0 to Pred(vRegras.Count) do
      FRegras.Add(vRegras[vInt]);
    vDeps.OwnsObjects := False;
    for vInt := 0 to Pred(vDeps.Count) do
      FDependencias.Add(vDeps[vInt]);
    Result := True;
  finally
    vDeps.Free;
    vRegras.Free;
    vJSON.Free;
  end;
end;

const
  RecursoDados = PChar(10);

procedure TManifesto.CarregarRecurso;
var
  vStream: TResourceStream;
  vTexto: TStringList;
begin
  if FindResource(HINSTANCE, 'MANIFESTO_RAL', RecursoDados) = 0 then
    Exit;
  vStream := TResourceStream.Create(HINSTANCE, 'MANIFESTO_RAL', RecursoDados);
  vTexto := TStringList.Create;
  try
    vTexto.LoadFromStream(vStream);
    CarregarTexto(vTexto.Text, cmManifestoEmbutido);
  finally
    vTexto.Free;
    vStream.Free;
  end;
end;

function TManifesto.CarregarArquivo(const AArquivo: string): boolean;
var
  vTexto: TStringList;
begin
  Result := False;
  if not FileExists(AArquivo) then
  begin
    FErros.Add(Format(emArquivoNaoExiste, [AArquivo]));
    Exit;
  end;
  vTexto := TStringList.Create;
  try
    vTexto.LoadFromFile(AArquivo);
    Result := CarregarTexto(vTexto.Text, AArquivo);
  finally
    vTexto.Free;
  end;
end;

procedure TManifesto.CarregarPadrao(AOrigem: TOrigemArquivos);
begin
  Limpar;
  if (AOrigem <> nil) and AOrigem.Existe('ralinstaller.json') then
    try
      if CarregarTexto(AOrigem.Ler('ralinstaller.json'),
                       Format(cmManifestoDaVersao, [AOrigem.Descricao])) then
        Exit;
    except
      on E: Exception do
        FErros.Add('ralinstaller.json: ' + E.Message);
    end;
  CarregarRecurso;
end;

procedure TManifesto.RegrasDependencia(const AReceita: string; ALista: TList);
var
  vInt: integer;
begin
  for vInt := 0 to Pred(FDependencias.Count) do
    if SameText(TRegraVersao(FDependencias[vInt]).Origem, AReceita) then
      ALista.Add(FDependencias[vInt]);
end;

{ TCompatibilidade }

constructor TCompatibilidade.Create(ACatalogo: TCatalogo; AManifesto: TManifesto;
  AReceitas: TReceitas);
begin
  inherited Create;
  FCatalogo := ACatalogo;
  FManifesto := AManifesto;
  FReceitas := AReceitas;
  FUsos := TStringList.Create;
  FUsos.OwnsObjects := True;
  FUsos.Sorted := True;
  FArquivos := TStringList.Create;
  FArquivos.Sorted := True;
end;

destructor TCompatibilidade.Destroy;
begin
  FArquivos.Free;
  FUsos.Free;
  inherited Destroy;
end;

function TCompatibilidade.Existe(const AArquivo: string): boolean;
var
  vIdx: integer;
begin
  vIdx := FArquivos.IndexOf(AArquivo);
  if vIdx >= 0 then
    Exit(FArquivos.Objects[vIdx] <> nil);
  Result := FileExists(AArquivo);
  FArquivos.AddObject(AArquivo, TObject(PtrInt(Ord(Result))));
end;

function TCompatibilidade.UsosDaUnidade(const AArquivo: string;
  AIDE: TIDEInstance): TStrings;
var
  vIdx: integer;
  vLista, vTodos, vDef, vNaoDef: TStringList;
  vNome, vChave: string;
begin
  // o resultado depende da versao: {$IFDEF DELPHIXE4UP} escolhe outro uses
  vChave := AIDE.Versao + '|' + AArquivo;
  vIdx := FUsos.IndexOf(vChave);
  if vIdx >= 0 then
    Exit(TStrings(FUsos.Objects[vIdx]));
  vLista := TStringList.Create;
  vLista.CaseSensitive := False;
  FUsos.AddObject(vChave, vLista);
  Result := vLista;
  // unidade de submodulo ainda nao baixado: sem fonte, sem conferencia
  if not FCatalogo.Origem.Existe(AArquivo) then
    Exit;
  vTodos := TStringList.Create;
  vDef := TStringList.Create;
  vNaoDef := TStringList.Create;
  try
    vTodos.CaseSensitive := False;
    SimbolosDelphi(AIDE, vDef, vNaoDef);
    try
      UsosEfetivos(FCatalogo.Origem.Ler(AArquivo), vDef, vNaoDef, vTodos);
    except
      Exit;
    end;
    for vNome in vTodos do
      if DaRTL(vNome) then
        vLista.Add(vNome);
  finally
    vNaoDef.Free;
    vDef.Free;
    vTodos.Free;
  end;
end;

function TCompatibilidade.FornecidoPorReceita(ATipo: TTipoPacote;
  const ANome: string): boolean;
var
  vInt: integer;
begin
  Result := False;
  if FReceitas = nil then
    Exit;
  for vInt := 0 to Pred(FReceitas.Count) do
    if FReceitas[vInt].Bloco(ATipo).FornecePacotes.IndexOf(ANome) >= 0 then
      Exit(True);
end;

function TCompatibilidade.MotivoDelphi(APacote: TPacote; AIDE: TIDEInstance): string;
var
  vLib, vDcp, vNome, vUnidade: string;
  vPastas: array of string;

  function TemDcp(const ANome: string): boolean;
  var
    vPasta: string;
  begin
    Result := False;
    for vPasta in vPastas do
      if (vPasta <> '') and
         (Existe(vPasta + ANome + '.dcp') or
          ((AIDE.SufixoPacote <> '') and
           Existe(vPasta + ANome + AIDE.SufixoPacote + '.dcp'))) then
        Exit(True);
  end;

begin
  Result := '';
  // as bibliotecas da propria IDE: lib\win32\release do XE em diante, lib\ antes
  vLib := AIDE.RootDir + 'lib' + PathDelim + 'win32' + PathDelim + 'release' +
          PathDelim;
  if not DirectoryExists(vLib) then
    vLib := AIDE.RootDir + 'lib' + PathDelim;
  if not DirectoryExists(vLib) then
    Exit;
  vDcp := '';
  if AIDE.CommonDir <> '' then
    vDcp := AIDE.CommonDir + 'Dcp' + PathDelim;
  SetLength(vPastas, 2);
  vPastas[0] := vLib;
  vPastas[1] := vDcp;

  // pacotes que o requires exige, e o Indy/FireDAC que o .dproj usa: o que
  // uma receita fornece e assunto da F7 (baixa, ou diz que falta)
  for vNome in APacote.Externos do
    if not FornecidoPorReceita(tpDelphi, vNome) and not TemDcp(vNome) then
      Exit(Format(cmMotivoSemPacote, [AIDE.Nome, vNome]));
  for vNome in APacote.Implicitos do
    if (AnsiStartsText('Indy', vNome) or AnsiStartsText('FireDAC', vNome)) and
       not FornecidoPorReceita(tpDelphi, vNome) and not TemDcp(vNome) then
      Exit(Format(cmMotivoSemPacote, [AIDE.Nome, vNome]));

  // as unidades da RTL que o pacote usa sem {$IFDEF}
  for vUnidade in APacote.Unidades do
    for vNome in UsosDaUnidade(vUnidade, AIDE) do
      if not Existe(vLib + vNome + '.dcu') then
        Exit(Format(cmMotivoSemUnidade, [vNome, AIDE.Nome]));
end;

function TCompatibilidade.AvisoInstalada(AReceita: TReceita; AIDE: TIDEInstance;
  const APasta: string): string;
var
  vInt: integer;
  vVerif: TVerificacao;
  vBloco: TBlocoIDE;
  vArquivo: string;
  vTexto: TStringList;
begin
  Result := '';
  if APasta = '' then
    Exit;
  if AIDE.Tipo = tiDelphi then
    vBloco := AReceita.Delphi
  else
    vBloco := AReceita.Lazarus;
  for vInt := 0 to Pred(vBloco.Verificacoes.Count) do
  begin
    vVerif := TVerificacao(vBloco.Verificacoes[vInt]);
    // FPC de versao desconhecida nao acusa: na duvida, nao se acusa nada
    if not vVerif.Condicao.Relevante(AIDE) or
       not vVerif.Condicao.Satisfaz(AIDE, False) then
      Continue;
    vArquivo := ExpandirRaiz('{raiz}/' + vVerif.Arquivo, APasta);
    if not FileExists(vArquivo) then
      Continue;
    vTexto := TStringList.Create;
    try
      vTexto.LoadFromFile(vArquivo);
      if CasaExpressao(vTexto.Text, vVerif.Expressao) then
        Exit(vVerif.Aviso);
    finally
      vTexto.Free;
    end;
  end;
end;

function TCompatibilidade.MotivoDependencias(APacote: TPacote;
  AIDE: TIDEInstance): string;
var
  vInt: integer;
  vReceita: TReceita;
  vTipo: TTipoPacote;
  vPasta: string;
begin
  Result := '';
  if FReceitas = nil then
    Exit;
  if AIDE.Tipo = tiDelphi then
    vTipo := tpDelphi
  else
    vTipo := tpLazarus;
  for vInt := 0 to Pred(FReceitas.Count) do
  begin
    vReceita := FReceitas[vInt];
    if not vReceita.Bloco(vTipo).Existe or not vReceita.Atende(APacote) then
      Continue;
    // a que ja esta na IDE vale, seja qual for a versao (§8) — a menos que a
    // receita saiba que aquela copia nao compila nesta IDE
    if Assigned(FDetectar) and (FDetectar(AIDE, vReceita) <> '') then
    begin
      if Assigned(FPastaInstalada) then
      begin
        vPasta := FPastaInstalada(AIDE, vReceita);
        Result := AvisoInstalada(vReceita, AIDE, vPasta);
        if Result <> '' then
          Exit(Format(cmMotivoInstaladaNaoCompila,
                      [vReceita.Nome, ExcludeTrailingPathDelimiter(vPasta), Result]));
      end;
      Continue;
    end;
    // comercial (uniGUI, AnyDAC) e sem download: nao estando, nao ha o que
    // fazer. Sem deteccao nao da para saber, e nao se acusa
    if Assigned(FDetectar) and not vReceita.PodeBaixar then
      Exit(Format(cmMotivoComercial, [vReceita.Nome, AIDE.Nome]));
    VersaoDependencia(vReceita, AIDE, Result);
    if Result <> '' then
      Exit;
  end;
end;

function TCompatibilidade.Motivo(APacote: TPacote; AIDE: TIDEInstance): string;
var
  vInt: integer;
  vRegra: TRegraPacote;
  vFaixa: string;
begin
  Result := '';
  if FManifesto <> nil then
    for vInt := 0 to Pred(FManifesto.TotalRegras) do
    begin
      vRegra := FManifesto.Regras[vInt];
      if not SameText(vRegra.Pacote, APacote.Nome) or
         vRegra.Condicao.Satisfaz(AIDE, True) then
        Continue;
      vFaixa := vRegra.Condicao.Descrever(AIDE);
      Result := Format(cmMotivoExige, [vFaixa]);
      if vRegra.Motivo <> '' then
        Result := Result + ' (' + vRegra.Motivo + ')';
      Exit;
    end;

  if (APacote.Tipo = tpDelphi) and (AIDE.Tipo = tiDelphi) then
    Result := MotivoDelphi(APacote, AIDE);
  if Result = '' then
    Result := MotivoDependencias(APacote, AIDE);
end;

procedure TCompatibilidade.Filtrar(ALista: TList; AIDE: TIDEInstance; AFora: TStrings);
var
  vInt, vDep: integer;
  vPacote: TPacote;
  vMotivo: string;
begin
  // a lista esta em ordem de dependencia: quem depende de algo que saiu vem
  // depois, e uma passada basta
  for vInt := 0 to Pred(ALista.Count) do
  begin
    vPacote := TPacote(ALista[vInt]);
    vMotivo := '';
    for vDep := 0 to Pred(vPacote.Internos.Count) do
      if AFora.IndexOfName(vPacote.Internos[vDep]) >= 0 then
      begin
        vMotivo := Format(cmMotivoDepende, [vPacote.Internos[vDep]]);
        Break;
      end;
    if vMotivo = '' then
      vMotivo := Motivo(vPacote, AIDE);
    if vMotivo <> '' then
      AFora.Values[vPacote.Nome] := vMotivo;
  end;
  for vInt := Pred(ALista.Count) downto 0 do
    if AFora.IndexOfName(TPacote(ALista[vInt]).Nome) >= 0 then
      ALista.Delete(vInt);
end;

function TCompatibilidade.VersaoDependencia(AReceita: TReceita; AIDE: TIDEInstance;
  out AMotivo: string): string;
var
  vRegras: TList;
  vInt: integer;
  vRegra: TRegraVersao;
begin
  AMotivo := '';
  Result := AReceita.Versao;
  vRegras := TList.Create;
  try
    // primeiro o que esta versao do RAL exige, depois o que a dependencia diz
    // de si mesma
    if FManifesto <> nil then
      FManifesto.RegrasDependencia(AReceita.Nome, vRegras);
    for vInt := 0 to Pred(AReceita.Versoes.Count) do
      vRegras.Add(AReceita.Versoes[vInt]);

    for vInt := 0 to Pred(vRegras.Count) do
    begin
      vRegra := TRegraVersao(vRegras[vInt]);
      // FPC de versao desconhecida nao entra em regra nenhuma: fica a padrao
      if not vRegra.Condicao.Relevante(AIDE) or
         not vRegra.Condicao.Satisfaz(AIDE, False) then
        Continue;
      if vRegra.Incompativel <> '' then
      begin
        AMotivo := AReceita.Nome + ': ' + vRegra.Incompativel;
        Exit('');
      end;
      // outro repositorio para esta faixa: 'dono/repo:ref'
      Exit(VersaoComFonte(vRegra.Github, vRegra.Versao));
    end;
  finally
    vRegras.Free;
  end;
end;

end.
