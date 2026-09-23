unit RALInst.Compatibilidade;

{$mode ObjFPC}{$H+}

// F6: o que cada IDE consegue instalar, antes de tentar.
//
// A maior parte sai do disco, sem manifesto nenhum — pelo mesmo motivo da F2:
// pacote novo nao pode depender de alguem lembrar de escrever a regra.
// - Delphi, pacotes: o que o requires exige (e o Indy/FireDAC que o .dproj
//   usa) tem de existir como .dcp na IDE. O XE2 nao tem FireDAC.dcp.
// - Delphi, unidades: as da RTL que as unidades do pacote usam tem de existir
//   como .dcu em lib\win32\release. O NetHttpRAL usa System.Net.HttpClient,
//   que so existe do XE8 em diante. Os {$IFDEF} sao avaliados com os simbolos
//   daquela IDE (DELPHIXE4UP do PascalRAL.inc, VER230, MSWINDOWS): o
//   RALDBFireDAC usa FireDAC.Stan.StorageBin sob {$IFDEF DELPHIXE4UP}, e o XE5
//   entra ali sem ter a unidade. Simbolo desconhecido e {$IF expressao} deixam
//   o trecho de fora — na duvida, nao se acusa nada.
// - Dependencia (F7): a versao por compilador vem da receita ("fonte.versoes"
//   — o Zeos 8.0 nao compila no FPC 3.2.3); nenhuma versao serve, o pacote que
//   precisa dela fica de fora, a menos que a IDE ja tenha a dependencia.
//
// O que nao da para deduzir vem do manifesto: faixa de IDE por pacote e a
// versao de dependencia que *esta* versao do RAL exige (o RAL 1.1 quer o
// mORMot2 do master). O manifesto e da versao do RAL: um ralinstaller.json na
// raiz dela vale; sem ele, a copia embutida no instalador (RCDATA
// MANIFESTO_RAL, de manifesto/ral.json).
//
//   {
//     "formato": 1,
//     "pacotes": [ { "pacote": "SynopseRAL", "delphi-min": "2009", "motivo": "..." } ],
//     "dependencias": [ { "receita": "mORMot2", "versao": "master" } ]
//   }

interface

uses
  Classes, SysUtils, Contnrs, fpjson, RALInst.IDE, RALInst.Catalogo, RALInst.Receitas;

type
  { TRegraPacote }

  // o pacote so vale dentro da faixa
  TRegraPacote = class
  public
    Pacote: string;
    Condicao: TCondicaoIDE;
    Motivo: string;
    constructor Create;
    destructor Destroy; override;
  end;

  { TManifesto }

  TManifesto = class
  private
    FRegras: TObjectList;
    FDependencias: TObjectList;
    FErros: TStringList;
    FOrigem: string;
    function GetRegra(AIndex: integer): TRegraPacote;
    function GetTotalRegras: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Limpar;
    // False (e o motivo em Erros) se nao e um manifesto valido; nada e mantido
    // de um manifesto recusado
    function CarregarTexto(const ATexto, AOrigem: string): boolean;
    // o da versao do RAL (ralinstaller.json na raiz), senao o embutido
    procedure CarregarPadrao(AOrigem: TOrigemArquivos);
    procedure CarregarRecurso;
    // um arquivo em disco (as ferramentas de teste, que nao tem o recurso)
    function CarregarArquivo(const AArquivo: string): boolean;

    // regras de versao desta receita, na ordem (TRegraVersao)
    procedure RegrasDependencia(const AReceita: string; ALista: TList);

    property Regras[AIndex: integer]: TRegraPacote read GetRegra;
    property TotalRegras: integer read GetTotalRegras;
    property Erros: TStringList read FErros;
    // de onde veio: 'ralinstaller.json da versao', 'embutido'
    property Origem: string read FOrigem;
  end;

  // onde a dependencia ja esta nesta IDE ('' se nao esta): a detecção da F7,
  // que mora no motor de cada tipo de IDE
  TDetectarDependencia = function(AIDE: TIDEInstance; AReceita: TReceita): string of object;
  // a raiz da copia instalada da dependencia nesta IDE ('' se nao se sabe)
  TPastaDependencia = function(AIDE: TIDEInstance; AReceita: TReceita): string of object;

  { TCompatibilidade }

  TCompatibilidade = class
  private
    FCatalogo: TCatalogo;
    FManifesto: TManifesto;
    FReceitas: TReceitas;
    FDetectar: TDetectarDependencia;
    FPastaInstalada: TPastaDependencia;
    // arquivo da unidade -> unidades da RTL usadas fora de {$IFDEF}
    FUsos: TStringList;
    // pasta -> existe (cache de FileExists por IDE)
    FArquivos: TStringList;
    function Existe(const AArquivo: string): boolean;
    function UsosDaUnidade(const AArquivo: string; AIDE: TIDEInstance): TStrings;
    function FornecidoPorReceita(ATipo: TTipoPacote; const ANome: string): boolean;
    function MotivoDelphi(APacote: TPacote; AIDE: TIDEInstance): string;
    function MotivoDependencias(APacote: TPacote; AIDE: TIDEInstance): string;
  public
    // nada pertence a esta classe; manifesto e receitas podem ser nil
    constructor Create(ACatalogo: TCatalogo; AManifesto: TManifesto; AReceitas: TReceitas);
    destructor Destroy; override;

    // '' se o pacote em si cabe na IDE; senao, por que nao. O que ele exige do
    // proprio RAL e com Filtrar
    function Motivo(APacote: TPacote; AIDE: TIDEInstance): string;
    // tira da lista (em ordem de dependencia) o que nao cabe na IDE e quem
    // depende disso; AFora recebe nome=motivo
    procedure Filtrar(ALista: TList; AIDE: TIDEInstance; AFora: TStrings);
    // a versao da dependencia para esta IDE: a do manifesto (desta versao do
    // RAL), a da receita por faixa, ou a padrao da receita. '' e AMotivo
    // preenchido quando nenhuma versao serve
    function VersaoDependencia(AReceita: TReceita; AIDE: TIDEInstance;
      out AMotivo: string): string;

    // a copia ja instalada (em APasta) serve nesta IDE? '' se serve, ou nao ha
    // verificacao que a acuse; senao o aviso da receita
    function AvisoInstalada(AReceita: TReceita; AIDE: TIDEInstance;
      const APasta: string): string;

    property Detectar: TDetectarDependencia read FDetectar write FDetectar;
    property PastaInstalada: TPastaDependencia read FPastaInstalada write FPastaInstalada;
  end;

// as unidades que o fonte usa fora de qualquer {$IF...}: comentario, string
// e diretiva sao respeitados
procedure UsosIncondicionais(const AFonte: string; ALista: TStrings);
// as unidades que o fonte usa com os simbolos conhecidos: {$IFDEF X} com X em
// ADefinidos conta, em ANaoDefinidos nao conta (e o {$ELSE} dele conta); simbolo
// que nao esta em nenhuma das duas, e {$IF expressao}, deixa o trecho de fora
procedure UsosEfetivos(const AFonte: string; ADefinidos, ANaoDefinidos: TStrings;
  ALista: TStrings);
// os simbolos que o PascalRAL.inc e o compilador definem para esta IDE Delphi
// (DELPHIXE4UP, VER360, MSWINDOWS...), para Win32
procedure SimbolosDelphi(AIDE: TIDEInstance; ADefinidos, ANaoDefinidos: TStrings);

// chave de PastasDependencias: a mesma dependencia pode estar baixada em duas
// versoes (o Zeos estavel para o Delphi, o 8.0-patches para o FPC 3.3)
function ChaveDependencia(const AReceita, AVersao: string): string;

implementation

uses
  jsonparser, StrUtils;

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
            vDiretiva := Trim(StringReplace(Copy(AFonte, vInicio + 2, vPos - vInicio - 2),
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
            else if ((vDiretiva = 'ENDIF') or (vDiretiva = 'IFEND')) and (vPilha <> '') then
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
          while (vPos <= vTam) and (AFonte[vPos] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) do
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
  ANaoDefinidos.AddStrings(['FPC', 'LINUX', 'UNIX', 'POSIX', 'DARWIN', 'MACOS', 'ANDROID',
                            'IOS', 'WIN64', 'CPUX64', 'CPU64', 'CLR', 'LAZARUS']);
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
        FErros.Add(AOrigem + ': JSON inválido: ' + E.Message);
        Exit;
      end;
    end;
    if not (vJSON is TJSONObject) then
    begin
      FErros.Add(AOrigem + ': o manifesto deveria ser um objeto JSON');
      Exit;
    end;
    vRaiz := TJSONObject(vJSON);
    for vInt := 0 to Pred(vRaiz.Count) do
      if AnsiIndexStr(vRaiz.Names[vInt], ['formato', 'pacotes', 'dependencias', 'comentario']) < 0 then
        vErro := 'campo desconhecido "' + vRaiz.Names[vInt] + '"';
    if (vErro = '') and (vRaiz.Get('formato', 0) <> 1) then
      vErro := 'formato ' + IntToStr(vRaiz.Get('formato', 0)) + ' desconhecido (este instalador lê o 1)';

    vLista := vRaiz.Get('pacotes', TJSONArray(nil));
    if vLista <> nil then
      for vInt := 0 to Pred(vLista.Count) do
      begin
        if vErro <> '' then
          Break;
        if not (vLista.Items[vInt] is TJSONObject) then
        begin
          vErro := 'pacotes: cada item é um objeto';
          Break;
        end;
        vItem := vLista.Objects[vInt];
        for vCampo := 0 to Pred(vItem.Count) do
        begin
          vNome := vItem.Names[vCampo];
          vAchou := AnsiIndexStr(vNome, ['pacote', 'motivo', 'comentario']) >= 0;
          vAchou := vAchou or (AnsiIndexStr(vNome, CamposCondicao) >= 0);
          if not vAchou then
            vErro := Format('campo desconhecido "%s" em pacotes', [vNome]);
        end;
        vRegra := TRegraPacote.Create;
        vRegras.Add(vRegra);
        vRegra.Pacote := vItem.Get('pacote', '');
        vRegra.Motivo := vItem.Get('motivo', '');
        if vRegra.Pacote = '' then
          vErro := 'regra de pacote sem "pacote"'
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
          vErro := 'dependencias: cada item é um objeto';
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
          vErro := 'regra de dependência sem "receita"';
      end;

    if vErro <> '' then
    begin
      FErros.Add(AOrigem + ': manifesto recusado: ' + vErro);
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
    CarregarTexto(vTexto.Text, 'manifesto embutido');
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
    FErros.Add(AArquivo + ': não existe');
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
                       'ralinstaller.json de ' + AOrigem.Descricao) then
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

function TCompatibilidade.UsosDaUnidade(const AArquivo: string; AIDE: TIDEInstance): TStrings;
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

function TCompatibilidade.FornecidoPorReceita(ATipo: TTipoPacote; const ANome: string): boolean;
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
          ((AIDE.SufixoPacote <> '') and Existe(vPasta + ANome + AIDE.SufixoPacote + '.dcp'))) then
        Exit(True);
  end;

begin
  Result := '';
  // as bibliotecas da propria IDE: lib\win32\release do XE em diante, lib\ antes
  vLib := AIDE.RootDir + 'lib' + PathDelim + 'win32' + PathDelim + 'release' + PathDelim;
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
      Exit(Format('o %s não tem o pacote %s', [AIDE.Nome, vNome]));
  for vNome in APacote.Implicitos do
    if (AnsiStartsText('Indy', vNome) or AnsiStartsText('FireDAC', vNome)) and
       not FornecidoPorReceita(tpDelphi, vNome) and not TemDcp(vNome) then
      Exit(Format('o %s não tem o pacote %s', [AIDE.Nome, vNome]));

  // as unidades da RTL que o pacote usa sem {$IFDEF}
  for vUnidade in APacote.Unidades do
    for vNome in UsosDaUnidade(vUnidade, AIDE) do
      if not Existe(vLib + vNome + '.dcu') then
        Exit(Format('usa %s, que o %s não tem', [vNome, AIDE.Nome]));
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
    if not vVerif.Condicao.Relevante(AIDE) or not vVerif.Condicao.Satisfaz(AIDE, False) then
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

function TCompatibilidade.MotivoDependencias(APacote: TPacote; AIDE: TIDEInstance): string;
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
          Exit(Format('o %s instalado nesta IDE (%s) não compila aqui: %s',
                      [vReceita.Nome, ExcludeTrailingPathDelimiter(vPasta), Result]));
      end;
      Continue;
    end;
    // comercial (uniGUI, AnyDAC) e sem download: nao estando, nao ha o que
    // fazer. Sem deteccao nao da para saber, e nao se acusa
    if Assigned(FDetectar) and not vReceita.PodeBaixar then
      Exit(Format('%s é comercial e não está instalado no %s', [vReceita.Nome, AIDE.Nome]));
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
      Result := 'exige ' + vFaixa;
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
        vMotivo := 'depende de ' + vPacote.Internos[vDep] + ', que não cabe nesta IDE';
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
      if not vRegra.Condicao.Relevante(AIDE) or not vRegra.Condicao.Satisfaz(AIDE, False) then
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
