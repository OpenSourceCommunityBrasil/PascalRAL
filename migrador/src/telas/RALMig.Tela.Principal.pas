/// The migrator window, in the installer's frame (the same TNotebook, size,
/// banner, image buttons, theme switch and DPI scaling): the pages in order
/// (language, project, versions, binary forms, preview, apply, report, compile
/// to check), each on the installer's page base, and the Next/Previous walk
/// that skips the pages with nothing to ask. The language page is the
/// installer's. The self-test (--autotest) walks the pages without interaction;
/// --gerar-po writes the translation files; --capturar=<folder> saves every
/// page in PNG.
unit RALMig.Tela.Principal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Graphics,
  RALInst.Tela.Idioma, RALInst.Tela.Modelo, RALInst.Tela.Temas,
  RALMig.Tela.Aplicar, RALMig.Tela.Binarios, RALMig.Tela.Conferir,
  RALMig.Tela.Estado, RALMig.Tela.Pagina, RALMig.Tela.Previa, RALMig.Tela.Projeto,
  RALMig.Tela.Relatorio, RALMig.Tela.Versoes;

type
  /// The migrator window.
  TTelaMigPrincipal = class(TForm, IJanelaModelo)
    ntPages: TNotebook;
    procedure FormCreate(Sender: TObject);
  private
    FArgs: TArgumentosMig;
    FEstado: TEstadoMig;
    FIdioma: TLanguages;
    FPagina: integer;
    FPaginas: TList;
    FPgAplicar: TTelaAplicar;
    FPgBinarios: TTelaBinarios;
    FPgConferir: TTelaConferir;
    FPgIdioma: TTelaIdioma;
    FPgPrevia: TTelaPrevia;
    FPgProjeto: TTelaProjeto;
    FPgRelatorio: TTelaRelatorio;
    FPgVersoes: TTelaVersoes;
    FTema: TThemes;
    procedure AutoTeste(AData: PtrInt);
    /// --capturar=<folder>: every page in both themes and the three languages,
    /// into PNG files, then closes
    procedure Capturar(AData: PtrInt);
    /// Creates a page inside the notebook
    function CriarPagina(AClasse: TTelaModeloClass): TTelaModelo;
    procedure Mostrar(AIndice: integer);
    function Pagina(AIndice: integer): TTelaModelo;
    procedure PaginaMudou(Sender: TObject);
    function ProximaVisivel(AAPartirDe, ADirecao: integer): integer;
    procedure SetLanguage(AValue: TLanguages);
    procedure SetTheme(AValue: TThemes);
    procedure TratarExcecao(Sender: TObject; E: Exception);
  public
    destructor Destroy; override;
    /// Updates the Previous/Next buttons of the page and the reason for a
    /// blocked Next
    procedure AtualizarNavegacao;
    /// Hint of the theme button while ATema is on
    function DicaTema(ATema: TThemes): string;
    /// Developer tool: writes languages/ralmigrador.<lang>.po
    procedure GerarPO(const ALang: string);
    /// After creating: applies the command line and shows the first page
    procedure Iniciar(const AArgs: TArgumentosMig);
    /// The page said Next: moves to the next page that skips nothing, or closes
    /// after the last
    procedure NextPage;
    /// Moves to the previous page that skips nothing
    procedure PriorPage;
    /// The line under the banner
    function TextoVersao: string;
    /// Applies a language to every text of the window and of the core
    procedure TrocarIdioma(const AIdioma: string);

    property Estado: TEstadoMig read FEstado;
    property Tema: TThemes read FTema write SetTheme;
  end;

var
  TelaMigPrincipal: TTelaMigPrincipal;

implementation

{$R *.lfm}

uses
  RALInst.Traducao, RALInst.Versao, RALInst.Tela.Captura, RALInst.Tela.Traducao,
  RALMig.Regras, RALMig.Tela.GeradorPO, RALMig.Tela.Mensagens;

const
  // a ordem das paginas
  PG_IDIOMA = 0;
  PG_PROJETO = 1;
  PG_VERSOES = 2;
  PG_BINARIOS = 3;
  PG_PREVIA = 4;
  PG_APLICAR = 5;
  PG_RELATORIO = 6;
  PG_CONFERIR = 7;

{ TTelaMigPrincipal }

procedure TTelaMigPrincipal.FormCreate(Sender: TObject);
begin
  FEstado := TEstadoMig.Create;
  FPaginas := TList.Create;
  FPagina := -1;
  FPgIdioma := TTelaIdioma(CriarPagina(TTelaIdioma));
  FPgProjeto := TTelaProjeto(CriarPagina(TTelaProjeto));
  FPgVersoes := TTelaVersoes(CriarPagina(TTelaVersoes));
  FPgBinarios := TTelaBinarios(CriarPagina(TTelaBinarios));
  FPgPrevia := TTelaPrevia(CriarPagina(TTelaPrevia));
  FPgAplicar := TTelaAplicar(CriarPagina(TTelaAplicar));
  FPgRelatorio := TTelaRelatorio(CriarPagina(TTelaRelatorio));
  FPgConferir := TTelaConferir(CriarPagina(TTelaConferir));
  FTema := tLight;
  SetTheme(tDark);
end;

destructor TTelaMigPrincipal.Destroy;
begin
  inherited Destroy;
  // as paginas (do formulario) ja se foram: o estado que elas usavam vai depois
  FPaginas.Free;
  FEstado.Free;
end;

procedure TTelaMigPrincipal.AtualizarNavegacao;
var
  vPagina: TTelaModelo;
begin
  if FPagina < 0 then
    Exit;
  vPagina := Pagina(FPagina);
  if vPagina is TTelaMig then
    TTelaMig(vPagina).AtualizarNavegacao(ProximaVisivel(FPagina - 1, -1) >= 0);
end;

procedure TTelaMigPrincipal.AutoTeste(AData: PtrInt);
var
  vLog: TStringList;
  vInt: integer;
  vErro: boolean;

  procedure Anotar(const ALinha: string);
  begin
    vLog.Add(ALinha);
  end;

begin
  vLog := TStringList.Create;
  vErro := False;
  try
    try
      Anotar('pasta: ' + FEstado.Pasta);
      Anotar(Format('achados: %d arquivo(s), %d projeto(s), %d binario(s)',
        [FEstado.ArquivosAchados, FEstado.Projetos.Count, FEstado.Binarios.Count]));
      if FEstado.ArquivosAchados = 0 then
      begin
        Anotar('ERRO: nada encontrado na pasta');
        vErro := True;
      end;
      // as paginas ate a previa, como o usuario as veria; nada e gravado
      for vInt := PG_PROJETO to PG_APLICAR do
      begin
        if (Pagina(vInt) is TTelaMig) and TTelaMig(Pagina(vInt)).Pular then
          Continue;
        Mostrar(vInt);
        Application.ProcessMessages;
        Anotar('pagina ' + IntToStr(vInt) + ' ok');
        if vInt = PG_VERSOES then
          Anotar(Format('de=%s para=%s detectada=%s', [FEstado.De, FEstado.Para,
            FEstado.VersaoDetectada]));
        if vInt = PG_PREVIA then
        begin
          Anotar(Format('previa: %d alterado(s), %d erro(s), %d manual(is)',
            [FEstado.ContarAlterados, FPgPrevia.Resultado.Erros.Count,
            FPgPrevia.Resultado.ManualCount]));
          vErro := vErro or (FPgPrevia.Resultado.Erros.Count > 0);
        end;
      end;
      Mostrar(PG_RELATORIO);
      Mostrar(PG_CONFERIR);
      Anotar('relatorio e conferir ok');
    except
      on E: Exception do
      begin
        Anotar('EXCECAO: ' + E.ClassName + ': ' + E.Message);
        vErro := True;
      end;
    end;
    vLog.SaveToFile(IncludeTrailingPathDelimiter(GetTempDir) + 'ralmigrador-autoteste.log');
  finally
    vLog.Free;
  end;
  if vErro then
    ExitCode := 1
  else
    ExitCode := 0;
  Application.Terminate;
end;

procedure TTelaMigPrincipal.Capturar(AData: PtrInt);
var
  vTema: TThemes;
  vIdioma: TLanguages;
  vInt: integer;
  vPasta: string;
  vErros: TStringList;
begin
  vPasta := PastaCaptura;
  vErros := TStringList.Create;
  try
    for vTema := Low(TThemes) to High(TThemes) do
    begin
      SetTheme(vTema);
      for vIdioma := Low(TLanguages) to High(TLanguages) do
      begin
        TrocarIdioma(CodigoIdioma(vIdioma));
        // todas as paginas, ate as que a rodada pularia; nada e gravado (o
        // Aplicar so grava ao avancar)
        for vInt := 0 to FPaginas.Count - 1 do
        begin
          try
            Mostrar(vInt);
          except
            on E: Exception do
              vErros.Add(NomeCaptura('migrador', vInt, vTema, vIdioma) + ': ' +
                E.Message);
          end;
          SalvarCaptura(Self, vPasta + NomeCaptura('migrador', vInt, vTema, vIdioma));
        end;
      end;
    end;
    if vErros.Count > 0 then
      vErros.SaveToFile(vPasta + 'migrador-erros.txt');
  finally
    vErros.Free;
    Application.Terminate;
  end;
end;

function TTelaMigPrincipal.CriarPagina(AClasse: TTelaModeloClass): TTelaModelo;
var
  vPage: TPage;
begin
  vPage := TPage.Create(Self);
  vPage.Parent := ntPages;

  Result := AClasse.Create(Self);
  Result.Parent := vPage;
  Result.Align := alClient;
  if Result is TTelaMig then
  begin
    TTelaMig(Result).Estado := FEstado;
    TTelaMig(Result).OnMudou := @PaginaMudou;
  end;
  FPaginas.Add(Result);
end;

function TTelaMigPrincipal.DicaTema(ATema: TThemes): string;
begin
  if ATema = tDark then
    Result := cmTemaClaro
  else
    Result := cmTemaEscuro;
end;

procedure TTelaMigPrincipal.GerarPO(const ALang: string);
var
  vGerador: TGeradorPO;
begin
  vGerador := TGeradorPO.Create(ALang);
  try
    vGerador.AdicionarComponentes(Self);
    vGerador.AdicionarMensagens;
    vGerador.Salvar('languages' + PathDelim + 'ralmigrador.' + ALang + '.po');
  finally
    vGerador.Free;
  end;
end;

procedure TTelaMigPrincipal.Iniciar(const AArgs: TArgumentosMig);
var
  vIdioma: string;
begin
  FArgs := AArgs;
  FEstado.De := VersaoDeLista(AArgs.De);
  FEstado.DeInformado := FEstado.De <> '';
  FEstado.Para := VersaoDeLista(AArgs.Para);
  FEstado.ParaInformado := FEstado.Para <> '';
  vIdioma := NormalizarIdioma(AArgs.Idioma);
  if vIdioma = '' then
    vIdioma := IdiomaDoSistema;
  TrocarIdioma(vIdioma);
  if AArgs.Pasta <> '' then
    FPgProjeto.DefinirCaminho(AArgs.Pasta);
  if PastaCaptura <> '' then
  begin
    // so as capturas: a primeira pagina mostrada e a que elas escolherem
    Application.OnException := @TratarExcecao;
    Application.QueueAsyncCall(@Capturar, 0);
  end
  else if AArgs.AutoTeste then
  begin
    Application.OnException := @TratarExcecao;
    Application.QueueAsyncCall(@AutoTeste, 0);
  end;
  // o idioma que veio da linha de comando dispensa a primeira pagina
  if NormalizarIdioma(AArgs.Idioma) <> '' then
    Mostrar(PG_PROJETO)
  else
    Mostrar(PG_IDIOMA);
end;

procedure TTelaMigPrincipal.Mostrar(AIndice: integer);
begin
  FPagina := AIndice;
  ntPages.PageIndex := AIndice;
  Pagina(AIndice).AoMostrar;
  AtualizarNavegacao;
end;

procedure TTelaMigPrincipal.NextPage;
var
  vProxima: integer;
begin
  if FPagina < 0 then
    Exit;
  vProxima := ProximaVisivel(FPagina + 1, 1);
  if vProxima < 0 then
    Close
  else
    Mostrar(vProxima);
end;

function TTelaMigPrincipal.Pagina(AIndice: integer): TTelaModelo;
begin
  Result := TTelaModelo(FPaginas[AIndice]);
end;

procedure TTelaMigPrincipal.PaginaMudou(Sender: TObject);
begin
  AtualizarNavegacao;
end;

procedure TTelaMigPrincipal.PriorPage;
var
  vAnterior: integer;
begin
  if FPagina <= 0 then
    Exit;
  vAnterior := ProximaVisivel(FPagina - 1, -1);
  if vAnterior >= 0 then
    Mostrar(vAnterior);
end;

function TTelaMigPrincipal.ProximaVisivel(AAPartirDe, ADirecao: integer): integer;
var
  vInt: integer;
begin
  Result := -1;
  vInt := AAPartirDe;
  while (vInt >= 0) and (vInt < FPaginas.Count) do
  begin
    // a primeira pagina so entra quando o idioma nao veio da linha de comando
    if (vInt = PG_IDIOMA) and (NormalizarIdioma(FArgs.Idioma) <> '') then
      Exit;
    if not ((Pagina(vInt) is TTelaMig) and TTelaMig(Pagina(vInt)).Pular) then
      Exit(vInt);
    Inc(vInt, ADirecao);
  end;
end;

procedure TTelaMigPrincipal.SetLanguage(AValue: TLanguages);
begin
  TrocarIdioma(CodigoIdioma(AValue));
end;

procedure TTelaMigPrincipal.SetTheme(AValue: TThemes);
var
  vInt: integer;
begin
  if FTema = AValue then
    Exit;
  FTema := AValue;
  for vInt := 0 to FPaginas.Count - 1 do
    Pagina(vInt).Theme := AValue;
end;

function TTelaMigPrincipal.TextoVersao: string;
begin
  Result := Format(cmVersaoMigrador, [VersaoInstalador]);
end;

procedure TTelaMigPrincipal.TratarExcecao(Sender: TObject; E: Exception);
var
  vLog: TStringList;
begin
  // no autoteste uma excecao nao pode abrir uma caixa e esperar por ninguem
  vLog := TStringList.Create;
  try
    vLog.Add('EXCECAO: ' + E.ClassName + ': ' + E.Message);
    vLog.SaveToFile(IncludeTrailingPathDelimiter(GetTempDir) +
      'ralmigrador-autoteste.log');
  finally
    vLog.Free;
  end;
  ExitCode := 1;
  Application.Terminate;
end;

procedure TTelaMigPrincipal.TrocarIdioma(const AIdioma: string);
var
  vInt: integer;
begin
  FIdioma := IdiomaDoCodigo(AIdioma);
  // as resourcestring e os textos dos .lfm; cada pagina reescreve os seus
  AplicarIdioma(AIdioma);
  FEstado.Idioma := AIdioma;
  Caption := cmTituloJanela;
  for vInt := 0 to FPaginas.Count - 1 do
  begin
    Pagina(vInt).Language := FIdioma;
    Pagina(vInt).AtualizarTextos;
  end;
  if FPagina >= 0 then
    AtualizarNavegacao;
end;

end.
