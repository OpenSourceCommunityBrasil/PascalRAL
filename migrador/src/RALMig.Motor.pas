/// The migration engine (stage M2): finds the files of a folder, builds the
/// chain of steps from one RAL version to another, applies every rule in memory
/// through the registered actions, and writes the result (with the safety copy
/// when asked). Nothing reaches the disk before Gravar. Running Simular over a
/// folder that was already migrated changes nothing.
unit RALMig.Motor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  RALMig.API, RALMig.Regras, RALMig.Tipos, RALMig.Projeto, RALMig.Deteccao,
  RALMig.Formulario;

const
  /// Exit codes of the command line (M7)
  SAIDA_OK = 0;
  SAIDA_MANUAL = 1;
  SAIDA_ERRO = 2;
  SAIDA_RECUSADO = 3;

type
  { TMotorMigracao }

  TMotorMigracao = class
  private
    FApis: TStringList;
    FConfianca: TConfianca;
    FConjunto: TConjuntoArquivos;
    FConverterBinarios: boolean;
    FDe: string;
    FErroGravacao: string;
    FExtras: TStringList;
    FIdioma: string;
    FLocalizador: TLocalizadorArquivos;
    FPara: string;
    FPasta: string;
    FPastaApi: string;
    FPastaRegras: string;
    FRaizRAL: string;
    FRecusado: boolean;
    FRegras: TConjuntoRegras;
    FRegrasCarregadas: boolean;
    /// the last CarregarRegras worked (a refused ralmigracao.json stops Simular)
    FRegrasOk: boolean;
    FForcarDe: boolean;
    FResultado: TResultadoMigracao;
    FZipCopia: string;
    function GetRegras: TConjuntoRegras;
    function ApiDaVersao(const AVersao: string): TApi;
    function IdiomaEfetivo: string;
    procedure AplicarPasso(APasso: TPassoMigracao; AAcoes: TStringList);
    procedure AplicarRegra(ARegra: TRegraMigracao; APasso: TPassoMigracao;
      AAcoes: TStringList);
    procedure AdicionarComportamentos(APasso: TPassoMigracao);
    procedure AdicionarIgnorados;
    procedure PularPassosJaFeitos(ACadeia: TListaPassos);
    procedure Reiniciar;
    function ListaParaCopia(out AForaDaPasta: string): TStringList;
    function GravarArquivos(ALista: TList): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    /// Loads the embedded rules; without the resources, the files of
    /// PastaRegras or of migrador/regras above the executable. Plus the
    /// ralmigracao.json of RaizRAL when it is set. True when all loaded
    function CarregarRegras: boolean;
    /// Applies the chain De -> Para in memory. The result belongs to the
    /// engine and is replaced by the next call
    function Simular: TResultadoMigracao;
    /// Writes the changed and new files; with ACopia, the safety copy first
    /// (ZipCopia) and RegistrarDepois after. False with ErroGravacao
    function Gravar(ACopia: boolean): boolean;
    /// Exit code of the last Simular: 3 refused, 2 error, 1 manual items, 0
    function CodigoSaida: integer;
    /// Detects the version of the folder (see RALMig.Deteccao)
    function Detectar(out AEvidencias: TStrings): string;
    /// Projects and units outside the folder (to ask the user); they only
    /// enter the set through Incluir
    function ProjetosFora: TStrings;
    function UnitsFora: TStrings;
    procedure Incluir(const ACaminho: string);

    /// Folder (or project file) being migrated
    property Pasta: string read FPasta write FPasta;
    property De: string read FDe write FDe;
    property Para: string read FPara write FPara;
    /// Applies every step from De even when the detection proves (high
    /// confidence) that the project is already newer; by default those steps
    /// are skipped, so running again changes nothing
    property ForcarDe: boolean read FForcarDe write FForcarDe;
    /// 'pt-BR', 'en-US', 'es-ES'; empty = the system's
    property Idioma: string read FIdioma write FIdioma;
    /// D-M6: convert binary forms to text instead of leaving them out
    property ConverterBinarios: boolean read FConverterBinarios write FConverterBinarios;
    /// Root of the target RAL (reads its ralmigracao.json); optional
    property RaizRAL: string read FRaizRAL write FRaizRAL;
    /// Folder of the rule files when they are not embedded (tests)
    property PastaRegras: string read FPastaRegras write FPastaRegras;
    /// Folder of the api-*.json when they are not embedded (tests)
    property PastaApi: string read FPastaApi write FPastaApi;
    property Regras: TConjuntoRegras read GetRegras;
    property Conjunto: TConjuntoArquivos read FConjunto;
    /// Confidence of the last Detectar
    property Confianca: TConfianca read FConfianca;
    property Resultado: TResultadoMigracao read FResultado;
    /// Last Simular was refused because the target is older (D-M8)
    property Recusado: boolean read FRecusado;
    property ZipCopia: string read FZipCopia;
    property ErroGravacao: string read FErroGravacao;
  end;

implementation

uses
  RALInst.Traducao, RALMig.Copia, RALMig.Inventario,
  // as acoes se registram no initialization: o motor traz todas
  RALMig.Acoes.Projeto, RALMig.Acoes.Formulario, RALMig.Acoes.Codigo;

resourcestring
  emMotorSemClasse = 'Regra %s: a ação %s não tem implementação nesta versão do migrador.';
  emMotorFalhaRegra = 'Regra %s: %s';
  emMotorRegras = 'Regras inválidas ou ausentes: %s';
  cmMotorJaEm = 'O projeto já está no RAL %s (detectado com segurança): os passos ' +
    'a partir da %s que ele já tinha foram pulados.';
  cmMotorJaEmFazer = 'Nada a fazer. Se o projeto ainda não está nessa versão, rode ' +
    'de novo com --forcar-de (na janela: "aplicar desde a versão escolhida").';
  emMotorSemPasta = 'Informe a pasta (ou o projeto) a migrar.';
  emMotorForaDaPasta = 'Arquivo fora da pasta do projeto, sem cópia de segurança possível: %s';
  emMotorGravar = 'Não foi possível gravar %s: %s';
  wmMotorIgnorado = 'O arquivo %s não foi migrado: %s';
  cmMotorIgnoradoFazer = 'Converta o formulário para texto e rode o migrador de novo.';
  cmMotorIgnoradoCorrigir = 'Abra %s na IDE ou num editor, corrija o que o motivo aponta (linha %d) e rode o migrador de novo.';

{ TMotorMigracao }

constructor TMotorMigracao.Create;
begin
  inherited Create;
  FApis := TStringList.Create;
  FApis.OwnsObjects := True;
  FApis.CaseSensitive := False;
  FExtras := TStringList.Create;
  FResultado := TResultadoMigracao.Create;
end;

destructor TMotorMigracao.Destroy;
begin
  FResultado.Free;
  FLocalizador.Free;
  FRegras.Free;
  FApis.Free;
  FExtras.Free;
  inherited Destroy;
end;

procedure TMotorMigracao.Reiniciar;
begin
  FreeAndNil(FResultado);
  FResultado := TResultadoMigracao.Create;
  FreeAndNil(FLocalizador);
  FConjunto := nil;
  FRecusado := False;
  FZipCopia := '';
  FErroGravacao := '';
end;

function TMotorMigracao.IdiomaEfetivo: string;
begin
  Result := NormalizarIdioma(FIdioma);
  if Result = '' then
    Result := IdiomaDoSistema;
end;

function TMotorMigracao.CarregarRegras: boolean;
var
  vPasta: string;
begin
  FreeAndNil(FRegras);
  FRegras := TConjuntoRegras.Create;
  Result := FRegras.CarregarEmbutidas;
  if not Result then
  begin
    FreeAndNil(FRegras);
    FRegras := TConjuntoRegras.Create;
    vPasta := FPastaRegras;
    if vPasta = '' then
    begin
      vPasta := AcharPastaMigrador;
      if vPasta <> '' then
        vPasta := vPasta + PathDelim + 'regras';
    end;
    Result := (vPasta <> '') and FRegras.CarregarDePasta(vPasta, True);
  end;
  if Result and (FRaizRAL <> '') then
    Result := FRegras.AplicarSobreposicaoDaRaiz(FRaizRAL);
  FRegrasCarregadas := True;
  FRegrasOk := Result;
end;

function TMotorMigracao.GetRegras: TConjuntoRegras;
begin
  if not FRegrasCarregadas then
    CarregarRegras;
  Result := FRegras;
end;

function TMotorMigracao.ApiDaVersao(const AVersao: string): TApi;
var
  vInd: integer;
begin
  vInd := FApis.IndexOf(AVersao);
  if vInd >= 0 then
    Exit(TApi(FApis.Objects[vInd]));
  Result := CarregarApiDaVersao(AVersao, FPastaApi);
  FApis.AddObject(AVersao, Result);
end;

procedure TMotorMigracao.AdicionarComportamentos(APasso: TPassoMigracao);
var
  vInt: integer;
  vComp: TComportamentoMigracao;
  vIdioma: string;
begin
  vIdioma := IdiomaEfetivo;
  for vInt := 0 to APasso.CompCount - 1 do
  begin
    vComp := APasso.Comportamento[vInt];
    FResultado.AdicionarComportamento(vComp.Id, APasso.Rotulo,
      vComp.Texto('descricao', vIdioma), vComp.Texto('fazer', vIdioma), vComp.Wiki);
  end;
end;

procedure TMotorMigracao.AplicarRegra(ARegra: TRegraMigracao; APasso: TPassoMigracao;
  AAcoes: TStringList);
var
  vClasse: TClasseAcaoMigracao;
  vInd: integer;
  vAcao: TAcaoMigracao;
  vCtx: TContextoMigracao;
begin
  vClasse := ClasseDaAcao(ARegra.Acao);
  if vClasse = nil then
  begin
    FResultado.Erros.Add(Format(emMotorSemClasse, [ARegra.Id, ARegra.NomeAcao]));
    Exit;
  end;
  vInd := AAcoes.IndexOf(ARegra.NomeAcao);
  if vInd < 0 then
    vInd := AAcoes.AddObject(ARegra.NomeAcao, vClasse.Create);
  vAcao := TAcaoMigracao(AAcoes.Objects[vInd]);
  vCtx := TContextoMigracao.Create;
  try
    vCtx.Conjunto := FConjunto;
    vCtx.Resultado := FResultado;
    vCtx.Passo := APasso;
    vCtx.ApiDe := ApiDaVersao(APasso.De);
    vCtx.ApiPara := ApiDaVersao(APasso.Para);
    vCtx.Idioma := IdiomaEfetivo;
    vCtx.ConverterBinarios := FConverterBinarios;
    try
      vAcao.Aplicar(ARegra, vCtx);
    except
      on E: Exception do
        FResultado.Erros.Add(Format(emMotorFalhaRegra, [ARegra.Id, E.Message]));
    end;
  finally
    vCtx.Free;
  end;
  FConjunto.Consolidar;
end;

procedure TMotorMigracao.AplicarPasso(APasso: TPassoMigracao; AAcoes: TStringList);
var
  vInt: integer;
begin
  for vInt := 0 to APasso.RegraCount - 1 do
    AplicarRegra(APasso.Regras[vInt], APasso, AAcoes);
  AdicionarComportamentos(APasso);
end;

/// Line where a text form stops being readable (0 when it is another problem,
/// like a binary form, which has no line)
function LinhaDoFormularioInvalido(AArq: TArquivoMigrado): integer;
begin
  Result := 0;
  if AArq.Tipo <> taForm then
    Exit;
  try
    TFormulario.Create(AArq.Texto).Free;
  except
    on E: EFormularioInvalido do
      Result := E.Linha;
    on E: Exception do
      Result := 0;
  end;
end;

procedure TMotorMigracao.AdicionarIgnorados;
var
  vInt, vLinha: integer;
  vFazer: string;
begin
  for vInt := 0 to FConjunto.Count - 1 do
    if FConjunto[vInt].Ignorado then
    begin
      vLinha := LinhaDoFormularioInvalido(FConjunto[vInt]);
      vFazer := cmMotorIgnoradoFazer;
      if vLinha > 0 then
        vFazer := Format(cmMotorIgnoradoCorrigir, [FConjunto[vInt].Relativo, vLinha]);
      FResultado.AdicionarManual('ignorado', FConjunto[vInt].Relativo, vLinha,
        Format(wmMotorIgnorado, [FConjunto[vInt].Relativo, FConjunto[vInt].Motivo]),
        vFazer, '', '');
    end;
end;

procedure TMotorMigracao.PularPassosJaFeitos(ACadeia: TListaPassos);
var
  vEvid: TStrings;
  vConf: TConfianca;
  vVersao: string;
  vInt: integer;
begin
  // idempotencia: rodar de novo com o mesmo --de sobre um projeto ja migrado
  // nao pode reaplicar os passos antigos (a regra de CORS da 1.2 escreveria
  // de novo o que o passo da 2.0 levou para o plugin). So a confianca alta
  // pula: pular por engano um passo de um projeto 1.x seria pior
  if FForcarDe then
    Exit;
  vVersao := DetectarVersaoEx(FConjunto, vEvid, vConf);
  try
    if (vConf <> cfAlta) or (vVersao = '') or
       (CompararVersoes(NormalizarVersao(vVersao), NormalizarVersao(FDe)) <= 0) then
      Exit;
    vInt := 0;
    while vInt < ACadeia.Count do
      if CompararVersoes(ACadeia[vInt].Para, NormalizarVersao(vVersao)) <= 0 then
        ACadeia.Remover(vInt)
      else
        Inc(vInt);
    FResultado.AdicionarComportamento('deteccao', '',
      Format(cmMotorJaEm, [NormalizarVersao(vVersao), NormalizarVersao(FDe)]),
      cmMotorJaEmFazer, '');
  finally
    vEvid.Free;
  end;
end;

function TMotorMigracao.Simular: TResultadoMigracao;
var
  vCadeia: TListaPassos;
  vErro: TCadeiaErro;
  vMsg: string;
  vAcoes: TStringList;
  vInt: integer;
begin
  Reiniciar;
  Result := FResultado;
  FResultado.De := NormalizarVersao(FDe);
  FResultado.Para := NormalizarVersao(FPara);
  FResultado.Idioma := IdiomaEfetivo;
  if FPasta = '' then
  begin
    FResultado.Erros.Add(emMotorSemPasta);
    Exit;
  end;
  if not FRegrasCarregadas then
    CarregarRegras;
  // regra recusada (arquivo embutido ou ralmigracao.json do RAL de destino):
  // migrar com parte das regras seria pior do que nao migrar
  if not FRegrasOk then
  begin
    FResultado.Erros.Add(Format(emMotorRegras, [FRegras.Erros.Text]));
    Exit;
  end;
  if FRegras.PassoCount = 0 then
    Exit;
  vCadeia := FRegras.Cadeia(OrigemRAL, FDe, FPara, vErro, vMsg);
  try
    if vErro <> ceNenhum then
    begin
      FRecusado := vErro = ceRetrocesso;
      FResultado.Erros.Add(vMsg);
      Exit;
    end;
    FLocalizador := TLocalizadorArquivos.Create(FPasta, FConverterBinarios);
    FLocalizador.Executar;
    for vInt := 0 to FExtras.Count - 1 do
      FLocalizador.IncluirFora(FExtras[vInt]);
    FConjunto := FLocalizador.Conjunto;
    FResultado.Erros.AddStrings(FLocalizador.Erros);
    AdicionarIgnorados;
    PularPassosJaFeitos(vCadeia);
    vAcoes := TStringList.Create;
    try
      vAcoes.OwnsObjects := True;
      for vInt := 0 to vCadeia.Count - 1 do
        AplicarPasso(vCadeia[vInt], vAcoes);
    finally
      vAcoes.Free;
    end;
  finally
    vCadeia.Free;
  end;
end;

function TMotorMigracao.CodigoSaida: integer;
begin
  if FRecusado then
    Result := SAIDA_RECUSADO
  else if (FResultado.Erros.Count > 0) then
    Result := SAIDA_ERRO
  else if FResultado.ManualCount > 0 then
    Result := SAIDA_MANUAL
  else
    Result := SAIDA_OK;
end;

function TMotorMigracao.ListaParaCopia(out AForaDaPasta: string): TStringList;
var
  vAlterados: TList;
  vInt: integer;
  vArq: TArquivoMigrado;
begin
  Result := TStringList.Create;
  AForaDaPasta := '';
  vAlterados := TList.Create;
  try
    FConjunto.ListarAlterados(vAlterados);
    for vInt := 0 to vAlterados.Count - 1 do
    begin
      vArq := TArquivoMigrado(vAlterados[vInt]);
      if not CaminhoDentro(FConjunto.Pasta, vArq.Caminho) then
        AForaDaPasta := vArq.Caminho
      else
        Result.Add(vArq.Relativo);
    end;
  finally
    vAlterados.Free;
  end;
end;

function TMotorMigracao.GravarArquivos(ALista: TList): boolean;
var
  vInt: integer;
  vArq: TArquivoMigrado;
begin
  Result := True;
  for vInt := 0 to ALista.Count - 1 do
  begin
    vArq := TArquivoMigrado(ALista[vInt]);
    try
      ForceDirectories(ExtractFilePath(vArq.Caminho));
      vArq.Texto.Gravar(vArq.Caminho, True);
    except
      on E: Exception do
      begin
        FErroGravacao := Format(emMotorGravar, [vArq.Relativo, E.Message]);
        Exit(False);
      end;
    end;
  end;
end;

function TMotorMigracao.Gravar(ACopia: boolean): boolean;
var
  vLista: TList;
  vCopiar: TStringList;
  vFora, vErro: string;
begin
  Result := False;
  FErroGravacao := '';
  FZipCopia := '';
  if FConjunto = nil then
    Exit;
  vCopiar := ListaParaCopia(vFora);
  vLista := TList.Create;
  try
    FConjunto.ListarAlterados(vLista);
    if vLista.Count = 0 then
      Exit(True);
    if ACopia then
    begin
      if vFora <> '' then
      begin
        FErroGravacao := Format(emMotorForaDaPasta, [vFora]);
        Exit;
      end;
      if not CriarCopia(FConjunto.Pasta, NormalizarVersao(FDe), NormalizarVersao(FPara),
                        vCopiar, FZipCopia, FErroGravacao) then
        Exit;
    end;
    if not GravarArquivos(vLista) then
      Exit;
    if ACopia and not RegistrarDepois(FZipCopia, FConjunto.Pasta, vErro) then
    begin
      FErroGravacao := vErro;
      Exit;
    end;
    Result := True;
  finally
    vLista.Free;
    vCopiar.Free;
  end;
end;

function TMotorMigracao.Detectar(out AEvidencias: TStrings): string;
var
  vLoc: TLocalizadorArquivos;
begin
  if FPastaApi <> '' then
    PastaApiAlternativa := FPastaApi;
  vLoc := TLocalizadorArquivos.Create(FPasta, FConverterBinarios);
  try
    vLoc.Executar;
    Result := DetectarVersaoEx(vLoc.Conjunto, AEvidencias, FConfianca);
  finally
    vLoc.Free;
  end;
end;

function TMotorMigracao.ProjetosFora: TStrings;
begin
  Result := nil;
  if FLocalizador <> nil then
    Result := FLocalizador.ProjetosFora;
end;

function TMotorMigracao.UnitsFora: TStrings;
begin
  Result := nil;
  if FLocalizador <> nil then
    Result := FLocalizador.UnitsFora;
end;

procedure TMotorMigracao.Incluir(const ACaminho: string);
begin
  if FExtras.IndexOf(ACaminho) < 0 then
    FExtras.Add(ACaminho);
  if FLocalizador <> nil then
    FLocalizador.IncluirFora(ACaminho);
end;

end.
