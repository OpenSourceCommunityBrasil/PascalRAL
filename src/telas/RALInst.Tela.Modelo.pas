/// Base of every installer page: banner, theme switch, Next and Previous
/// buttons, and the language, theme and IDE kind each page follows. The
/// migrator window (migrador/) builds its pages on it too: a page talks to its
/// window only through IJanelaModelo.
unit RALInst.Tela.Modelo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics,
  RALInst.Tela.Temas;

type
  /// What a page asks of the window it lives in (the installer's and the
  /// migrator's).
  IJanelaModelo = interface
    ['{AA077F63-2B52-40E9-BE4F-8A7A3F899D81}']
    /// Hint of the theme button while ATema is on: the theme it leads to
    function DicaTema(ATema: TThemes): string;
    procedure NextPage;
    procedure PriorPage;
    procedure SetLanguage(AValue: TLanguages);
    procedure SetTheme(AValue: TThemes);
    /// The line under the banner (program and version)
    function TextoVersao: string;
  end;

  /// Base frame of the installer pages.
  TTelaModelo = class(TFrame)
    imBack: TImage;
    imBanner: TImage;
    imClose: TImage;
    imFundo: TImage;
    imNext: TImage;
    imTheme: TImage;
    lbBack: TLabel;
    lbNext: TLabel;
    lVersion: TLabel;
    Panel1: TPanel;
    procedure imBannerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imBannerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imBannerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imCloseClick(Sender: TObject);
    procedure imThemeClick(Sender: TObject);
    procedure lbBackClick(Sender: TObject);
    procedure lbNextClick(Sender: TObject);
  private
    FIDE: integer;
    FLanguage: TLanguages;
    FMouseDown: boolean;
    FPointMove: TPoint;
    FTheme: TThemes;
  protected
    /// Places the controls side by side, centered in the page, AEspaco apart
    /// (in 96-DPI pixels); invisible ones are skipped
    procedure CentralizarLadoALado(const AControles: array of TControl;
      AEspaco: integer);
    /// The theme button hint names the theme it leads to
    procedure DicaDoTema;
    /// Makes an image draw its picture scaled to its own size (AWidth x
    /// AHeight at 96 DPI), so it follows the screen scale
    procedure DimensionarImagem(AImagem: TImage; AWidth, AHeight: integer);
    /// F15: the next button lights up or not (a choice that would install a
    /// broken RAL leaves it off; the page says why)
    procedure PermitirAvancar(APermitido: boolean);
    /// Puts AAnel (a ring picture) centered around AAlvo
    procedure EnvolverImagem(AAnel, AAlvo: TImage);
    /// The window the page lives in; nil while it has none
    function Janela: IJanelaModelo;
    /// Keeps a text exactly over its button picture, at any screen scale
    procedure SobreporRotulo(ARotulo: TLabel; AImagem: TImage);
    procedure SetIDE(AValue: integer); virtual;
    procedure SetLanguage(AValue: TLanguages); virtual;
    procedure SetTheme(AValue: TThemes); virtual;
    /// May the user leave to the next page?
    function ValidatePageNext: boolean; virtual;
    /// May the user go back to the previous page?
    function ValidatePagePrior: boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    /// The page has just appeared: time to load what depends on the previous
    /// pages (versions, plan)
    procedure AoMostrar; virtual;
    /// Rewrites the texts set in code; called after every language change,
    /// once the .lfm texts are already translated
    procedure AtualizarTextos; virtual;

  published
    /// 0 - Delphi, 1 - Lazarus, 2 - both
    property IDE: integer read FIDE write SetIDE;
    property Language: TLanguages read FLanguage write SetLanguage;
    property Theme: TThemes read FTheme write SetTheme;
  end;

  /// Class of a page, to create them all the same way.
  TTelaModeloClass = class of TTelaModelo;

implementation

{$R *.lfm}

{ TTelaModelo }

constructor TTelaModelo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // o banner acompanha a largura da pagina; o texto dos botoes fica sempre
  // sobre a figura deles, qualquer que seja a escala da tela
  imBanner.Align := alClient;
  imBanner.Center := True;
  imNext.Stretch := True;
  imBack.Stretch := True;
  SobreporRotulo(lbNext, imNext);
  SobreporRotulo(lbBack, imBack);
end;

procedure TTelaModelo.imCloseClick(Sender: TObject);
begin
  if GetParentForm(Self) <> nil then
    GetParentForm(Self).Close;
end;

procedure TTelaModelo.imBannerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FPointMove.X := X;
  FPointMove.Y := Y;
  FMouseDown := True;
end;

procedure TTelaModelo.imBannerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  vJanela: TCustomForm;
begin
  vJanela := GetParentForm(Self);
  if FMouseDown and (vJanela <> nil) then
  begin
    vJanela.Left := vJanela.Left + X - FPointMove.X;
    vJanela.Top := vJanela.Top + Y - FPointMove.Y;
  end;
end;

procedure TTelaModelo.imBannerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := False;
end;

procedure TTelaModelo.imThemeClick(Sender: TObject);
begin
  if Janela = nil then
    Exit;
  if FTheme = tDark then
    Janela.SetTheme(tLight)
  else
    Janela.SetTheme(tDark);
end;

procedure TTelaModelo.lbBackClick(Sender: TObject);
begin
  if (Janela <> nil) and ValidatePagePrior then
    Janela.PriorPage;
end;

procedure TTelaModelo.lbNextClick(Sender: TObject);
begin
  if not lbNext.Enabled then
    Exit;
  if (Janela <> nil) and ValidatePageNext then
    Janela.NextPage;
end;

procedure TTelaModelo.PermitirAvancar(APermitido: boolean);
begin
  lbNext.Enabled := APermitido;
  imNext.Enabled := APermitido;
  // a figura do botao nao fica cinza sozinha
  if APermitido then
    imNext.Cursor := crHandPoint
  else
    imNext.Cursor := crNo;
  lbNext.Cursor := imNext.Cursor;
end;

procedure TTelaModelo.CentralizarLadoALado(const AControles: array of TControl;
  AEspaco: integer);
var
  vInt, vTotal, vLeft, vVisiveis: integer;
begin
  vTotal := 0;
  vVisiveis := 0;
  for vInt := Low(AControles) to High(AControles) do
    if AControles[vInt].Visible then
    begin
      Inc(vTotal, AControles[vInt].Width);
      Inc(vVisiveis);
    end;
  if vVisiveis = 0 then
    Exit;
  Inc(vTotal, Scale96ToForm(AEspaco) * (vVisiveis - 1));
  vLeft := (ClientWidth - vTotal) div 2;
  for vInt := Low(AControles) to High(AControles) do
    if AControles[vInt].Visible then
    begin
      AControles[vInt].Left := vLeft;
      Inc(vLeft, AControles[vInt].Width + Scale96ToForm(AEspaco));
    end;
end;

procedure TTelaModelo.DimensionarImagem(AImagem: TImage; AWidth, AHeight: integer);
begin
  AImagem.AutoSize := False;
  AImagem.Stretch := True;
  AImagem.Proportional := True;
  AImagem.Center := True;
  AImagem.Width := Scale96ToForm(AWidth);
  AImagem.Height := Scale96ToForm(AHeight);
end;

procedure TTelaModelo.DicaDoTema;
begin
  if Janela <> nil then
    imTheme.Hint := Janela.DicaTema(FTheme);
end;

procedure TTelaModelo.EnvolverImagem(AAnel, AAlvo: TImage);
begin
  AAnel.Left := AAlvo.Left + (AAlvo.Width - AAnel.Width) div 2;
  AAnel.Top := AAlvo.Top + (AAlvo.Height - AAnel.Height) div 2;
end;

function TTelaModelo.Janela: IJanelaModelo;
var
  vForm: TCustomForm;
begin
  Result := nil;
  // a pagina fica num TPage do TNotebook da janela
  vForm := GetParentForm(Self);
  if vForm <> nil then
    Supports(vForm, IJanelaModelo, Result);
end;

procedure TTelaModelo.SobreporRotulo(ARotulo: TLabel; AImagem: TImage);
begin
  ARotulo.Anchors := [];
  ARotulo.AutoSize := False;
  ARotulo.BorderSpacing.Around := 0;
  ARotulo.AnchorSideLeft.Control := AImagem;
  ARotulo.AnchorSideLeft.Side := asrLeft;
  ARotulo.AnchorSideTop.Control := AImagem;
  ARotulo.AnchorSideTop.Side := asrTop;
  ARotulo.AnchorSideRight.Control := AImagem;
  ARotulo.AnchorSideRight.Side := asrRight;
  ARotulo.AnchorSideBottom.Control := AImagem;
  ARotulo.AnchorSideBottom.Side := asrBottom;
  ARotulo.Anchors := [akLeft, akTop, akRight, akBottom];
  ARotulo.Alignment := taCenter;
  ARotulo.Layout := tlCenter;
end;

procedure TTelaModelo.AoMostrar;
begin
end;

procedure TTelaModelo.AtualizarTextos;
begin
  DicaDoTema;
  if Janela <> nil then
    lVersion.Caption := Janela.TextoVersao;
end;

procedure TTelaModelo.SetIDE(AValue: integer);
begin
  FIDE := AValue;
end;

procedure TTelaModelo.SetLanguage(AValue: TLanguages);
begin
  FLanguage := AValue;
end;

procedure TTelaModelo.SetTheme(AValue: TThemes);
var
  vInt: integer;
begin
  FTheme := AValue;

  GetResourceImage(Themes[AValue].Background, imFundo);
  GetResourceImage(Themes[AValue].Button, imNext);
  GetResourceImage(Themes[AValue].Button, imBack);
  GetResourceImage(Themes[AValue].Theme, imTheme);
  // a faixa do banner acompanha o tema: o logo tem fundo transparente
  Panel1.Color := Themes[AValue].CorBanner;

  for vInt := 0 to Pred(ComponentCount) do
    if (Components[vInt] is TLabel) and (TLabel(Components[vInt]).Tag >= 0) then
      TLabel(Components[vInt]).Font.Color := Themes[AValue].FontColor;
  DicaDoTema;
end;

function TTelaModelo.ValidatePageNext: boolean;
begin
  Result := True;
end;

function TTelaModelo.ValidatePagePrior: boolean;
begin
  Result := True;
end;

end.
