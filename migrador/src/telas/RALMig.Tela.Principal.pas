/// The migrator window: banner, theme and language, the pages in order (language,
/// project, versions, binary forms, preview, apply, report, compile to check)
/// and the Previous/Next buttons that skip the pages with nothing to ask. Built
/// in code, in the look of the installer. The self-test (--autotest) walks the
/// pages without interaction; --gerar-po writes the translation files.
unit RALMig.Tela.Principal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics,
  RALInst.Tela.Temas,
  RALMig.Tela.Estado, RALMig.Tela.Modelo, RALMig.Tela.Idioma, RALMig.Tela.Projeto,
  RALMig.Tela.Versoes, RALMig.Tela.Previa, RALMig.Tela.Aplicar, RALMig.Tela.Conferir;

type
  { TTelaMigPrincipal }

  /// The migrator window.
  TTelaMigPrincipal = class(TForm)
  private
    FArgs: TArgumentosMig;
    FBanner: TPanel;
    FBotaoBack: TBotaoMig;
    FBotaoNext: TBotaoMig;
    FBotaoPic: TImage;
    FEstado: TEstadoMig;
    FFundoPic: TImage;
    FImBanner: TImage;
    FImClose: TImage;
    FImTema: TImage;
    FLbMotivo: TLabel;
    FLbVersao: TLabel;
    FMovendo: boolean;
    FPagina: integer;
    FPaginas: TList;
    FPgAplicar: TPaginaAplicar;
    FPgBinarios: TPaginaBinarios;
    FPgConferir: TPaginaConferir;
    FPgIdioma: TPaginaIdioma;
    FPgPrevia: TPaginaPrevia;
    FPgProjeto: TPaginaProjeto;
    FPgRelatorio: TPaginaRelatorio;
    FPgVersoes: TPaginaVersoes;
    FPonto: TPoint;
    FTema: TThemes;
    procedure AutoTeste(AData: PtrInt);
    procedure AvancarClick(Sender: TObject);
    procedure BannerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure BannerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure BannerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure CriarBanner;
    procedure CriarPaginas;
    procedure EscolherIdioma(Sender: TObject);
    procedure FecharClick(Sender: TObject);
    function Pagina(AIndice: integer): TPaginaMig;
    function ProximaVisivel(AAPartirDe, ADirecao: integer): integer;
    procedure Mostrar(AIndice: integer);
    procedure PaginaMudou(Sender: TObject);
    procedure TemaClick(Sender: TObject);
    procedure TratarExcecao(Sender: TObject; E: Exception);
    procedure VoltarClick(Sender: TObject);
    procedure SetTema(AValue: TThemes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// Moves to the next page (the one that skips nothing)
    procedure Avancar;
    /// Applies a language to every text of the window and of the core
    procedure AplicarIdioma(const AIdioma: string);
    /// Updates the Previous/Next buttons and the reason for a blocked Next
    procedure AtualizarNavegacao;
    /// Developer tool: writes languages/ralmigrador.<lang>.po
    procedure GerarPO(const ALang: string);
    /// After creating: applies the command line and shows the first page
    procedure Iniciar(const AArgs: TArgumentosMig);
    /// Moves to the previous page
    procedure Voltar;

    property Estado: TEstadoMig read FEstado;
    property Tema: TThemes read FTema write SetTema;
  end;

var
  TelaMigPrincipal: TTelaMigPrincipal;

implementation

uses
  RALInst.Traducao, RALInst.Versao, RALMig.Regras, RALMig.Tela.GeradorPO,
  RALMig.Tela.Mensagens;

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

constructor TTelaMigPrincipal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  Position := poScreenCenter;
  ClientWidth := Esc(LARGURA_JANELA);
  ClientHeight := Esc(ALTURA_JANELA);
  FEstado := TEstadoMig.Create;
  FPaginas := TList.Create;
  FPagina := -1;
  FFundoPic := TImage.Create(Self);
  FBotaoPic := TImage.Create(Self);
  CriarPaginas;
  CriarBanner;
  FTema := tLight;
  SetTema(tDark);
end;

destructor TTelaMigPrincipal.Destroy;
begin
  FPaginas.Free;
  FEstado.Free;
  inherited Destroy;
end;

procedure TTelaMigPrincipal.AplicarIdioma(const AIdioma: string);
var
  vInt: integer;
begin
  TraduzirMensagens(AIdioma);
  FEstado.Idioma := AIdioma;
  Caption := cmTituloJanela;
  FLbVersao.Caption := Format(cmVersaoMigrador, [VersaoInstalador]);
  if FTema = tDark then
    FImTema.Hint := cmTemaClaro
  else
    FImTema.Hint := cmTemaEscuro;
  for vInt := 0 to FPaginas.Count - 1 do
    Pagina(vInt).AtualizarTextos;
  if FPagina >= 0 then
    AtualizarNavegacao;
end;

procedure TTelaMigPrincipal.AtualizarNavegacao;
var
  vPagina: TPaginaMig;
  vMotivo: string;
  vVoltar: integer;
begin
  if FPagina < 0 then
    Exit;
  vPagina := Pagina(FPagina);
  FBotaoNext.Texto := vPagina.RotuloAvancar;
  FBotaoNext.Habilitar(vPagina.PodeAvancar(vMotivo));
  FLbMotivo.Caption := vMotivo;
  FLbMotivo.Visible := vMotivo <> '';
  vVoltar := ProximaVisivel(FPagina - 1, -1);
  FBotaoBack.Visible := (vVoltar >= 0) and vPagina.PodeVoltar;
  FBotaoBack.Texto := cmVoltar;
end;

procedure TTelaMigPrincipal.Avancar;
var
  vPagina: TPaginaMig;
  vMotivo: string;
  vProxima: integer;
begin
  if FPagina < 0 then
    Exit;
  vPagina := Pagina(FPagina);
  if not vPagina.PodeAvancar(vMotivo) then
    Exit;
  if not vPagina.AoAvancar then
    Exit;
  vProxima := ProximaVisivel(FPagina + 1, 1);
  if vProxima < 0 then
    Close
  else
    Mostrar(vProxima);
end;

procedure TTelaMigPrincipal.AvancarClick(Sender: TObject);
begin
  if FBotaoNext.Enabled then
    Avancar;
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
        if Pagina(vInt).Pular then
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

procedure TTelaMigPrincipal.BannerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FPonto.X := X;
  FPonto.Y := Y;
  FMovendo := True;
end;

procedure TTelaMigPrincipal.BannerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FMovendo then
  begin
    Left := Left + X - FPonto.X;
    Top := Top + Y - FPonto.Y;
  end;
end;

procedure TTelaMigPrincipal.BannerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMovendo := False;
end;

procedure TTelaMigPrincipal.CriarBanner;
begin
  FBanner := TPanel.Create(Self);
  FBanner.Parent := Self;
  FBanner.BevelOuter := bvNone;
  FBanner.ParentBackground := False;
  FBanner.SetBounds(0, 0, ClientWidth, Esc(110));
  FImBanner := TImage.Create(Self);
  FImBanner.Parent := FBanner;
  FImBanner.Align := alClient;
  FImBanner.Center := True;
  FImBanner.OnMouseDown := @BannerMouseDown;
  FImBanner.OnMouseMove := @BannerMouseMove;
  FImBanner.OnMouseUp := @BannerMouseUp;
  GetResourceImage('BANNER', FImBanner);
  FImClose := TImage.Create(Self);
  FImClose.Parent := FBanner;
  FImClose.Stretch := True;
  FImClose.Cursor := crHandPoint;
  FImClose.SetBounds(FBanner.Width - Esc(20), 0, Esc(20), Esc(20));
  FImClose.Anchors := [akTop, akRight];
  FImClose.OnClick := @FecharClick;
  GetResourceImage('CLOSEICON', FImClose);
  FImTema := TImage.Create(Self);
  FImTema.Parent := FBanner;
  FImTema.Stretch := True;
  FImTema.Cursor := crHandPoint;
  FImTema.ShowHint := True;
  FImTema.SetBounds(Esc(10), Esc(8), Esc(30), Esc(28));
  FImTema.OnClick := @TemaClick;
  FLbVersao := TLabel.Create(Self);
  FLbVersao.Parent := FBanner;
  FLbVersao.AutoSize := False;
  FLbVersao.Alignment := taCenter;
  FLbVersao.SetBounds(0, Esc(88), FBanner.Width, Esc(18));
  FBotaoNext := TBotaoMig.Create(Self);
  FBotaoNext.Parent := Self;
  FBotaoNext.SetBounds(ClientWidth - Esc(131), ClientHeight - Esc(47), Esc(121), Esc(37));
  FBotaoNext.OnClick := @AvancarClick;
  FBotaoBack := TBotaoMig.Create(Self);
  FBotaoBack.Parent := Self;
  FBotaoBack.SetBounds(Esc(10), ClientHeight - Esc(47), Esc(121), Esc(37));
  FBotaoBack.OnClick := @VoltarClick;
  FLbMotivo := TLabel.Create(Self);
  FLbMotivo.Parent := Self;
  FLbMotivo.AutoSize := False;
  FLbMotivo.WordWrap := True;
  FLbMotivo.Alignment := taCenter;
  FLbMotivo.Layout := tlCenter;
  FLbMotivo.Tag := -1;
  FLbMotivo.Font.Color := RGBToColor(255, 170, 0);
  FLbMotivo.SetBounds(Esc(150), ClientHeight - Esc(50), ClientWidth - Esc(300), Esc(44));
  FLbMotivo.Visible := False;
end;

procedure TTelaMigPrincipal.CriarPaginas;

  function Nova(AClasse: TPaginaMigClasse): TPaginaMig;
  begin
    Result := AClasse.Criar(Self, FEstado);
    Result.Parent := Self;
    Result.OnMudou := @PaginaMudou;
    FPaginas.Add(Result);
  end;

begin
  FPgIdioma := TPaginaIdioma(Nova(TPaginaIdioma));
  FPgIdioma.OnEscolher := @EscolherIdioma;
  FPgProjeto := TPaginaProjeto(Nova(TPaginaProjeto));
  FPgVersoes := TPaginaVersoes(Nova(TPaginaVersoes));
  FPgBinarios := TPaginaBinarios(Nova(TPaginaBinarios));
  FPgPrevia := TPaginaPrevia(Nova(TPaginaPrevia));
  FPgAplicar := TPaginaAplicar(Nova(TPaginaAplicar));
  FPgRelatorio := TPaginaRelatorio(Nova(TPaginaRelatorio));
  FPgConferir := TPaginaConferir(Nova(TPaginaConferir));
end;

procedure TTelaMigPrincipal.EscolherIdioma(Sender: TObject);
begin
  AplicarIdioma(CodigoIdioma(FPgIdioma.Idioma));
end;

procedure TTelaMigPrincipal.FecharClick(Sender: TObject);
begin
  Close;
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
  FPgIdioma.Idioma := IdiomaDoCodigo(vIdioma);
  AplicarIdioma(vIdioma);
  if AArgs.Pasta <> '' then
    FPgProjeto.DefinirCaminho(AArgs.Pasta);
  if AArgs.AutoTeste then
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
var
  vInt: integer;
begin
  for vInt := 0 to FPaginas.Count - 1 do
    Pagina(vInt).Visible := vInt = AIndice;
  FPagina := AIndice;
  Pagina(AIndice).AoMostrar;
  AtualizarNavegacao;
end;

function TTelaMigPrincipal.Pagina(AIndice: integer): TPaginaMig;
begin
  Result := TPaginaMig(FPaginas[AIndice]);
end;

procedure TTelaMigPrincipal.PaginaMudou(Sender: TObject);
begin
  AtualizarNavegacao;
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
    if not Pagina(vInt).Pular then
      Exit(vInt);
    Inc(vInt, ADirecao);
  end;
end;

procedure TTelaMigPrincipal.SetTema(AValue: TThemes);
var
  vInt: integer;
begin
  if FTema = AValue then
    Exit;
  FTema := AValue;
  GetResourceImage(Themes[AValue].Background, FFundoPic);
  GetResourceImage(Themes[AValue].Button, FBotaoPic);
  GetResourceImage(Themes[AValue].Theme, FImTema);
  FBanner.Color := Themes[AValue].CorBanner;
  FLbVersao.Font.Color := Themes[AValue].FontColor;
  for vInt := 0 to FPaginas.Count - 1 do
    Pagina(vInt).DefinirTema(AValue, FFundoPic.Picture, FBotaoPic.Picture);
  FBotaoNext.Picture.Assign(FBotaoPic.Picture);
  FBotaoNext.CorTexto := Themes[AValue].FontColor;
  FBotaoBack.Picture.Assign(FBotaoPic.Picture);
  FBotaoBack.CorTexto := Themes[AValue].FontColor;
  if FTema = tDark then
    FImTema.Hint := cmTemaClaro
  else
    FImTema.Hint := cmTemaEscuro;
end;

procedure TTelaMigPrincipal.TemaClick(Sender: TObject);
begin
  if FTema = tDark then
    Tema := tLight
  else
    Tema := tDark;
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

procedure TTelaMigPrincipal.Voltar;
var
  vAnterior: integer;
begin
  if FPagina <= 0 then
    Exit;
  if not Pagina(FPagina).PodeVoltar then
    Exit;
  vAnterior := ProximaVisivel(FPagina - 1, -1);
  if vAnterior >= 0 then
    Mostrar(vAnterior);
end;

procedure TTelaMigPrincipal.VoltarClick(Sender: TObject);
begin
  Voltar;
end;

end.
