/// Pieces every page of the migrator window is made of: the image button, the
/// scale helper and the base page (background, title, subtitle, theme colors).
/// The window is built in code, in the look of the installer (themes, banner,
/// image buttons); the texts come from RALMig.Tela.Mensagens.
unit RALMig.Tela.Modelo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics, Dialogs,
  RALInst.Tela.Temas, RALMig.Tela.Estado;

const
  /// Design size of the window, in 96-DPI pixels
  LARGURA_JANELA = 860;
  ALTURA_JANELA = 640;
  /// Left margin and width of the content area of a page
  MARGEM = 28;
  LARGURA_UTIL = 804;

type
  /// A button drawn over a picture (the installer's), with its own text.
  TBotaoMig = class(TImage)
  private
    FCorTexto: TColor;
    FTexto: string;
    procedure SetCorTexto(AValue: TColor);
    procedure SetTexto(const AValue: string);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    /// Enables or disables it (the text goes gray and the cursor says no)
    procedure Habilitar(AValor: boolean);

    property CorTexto: TColor read FCorTexto write SetCorTexto;
    property Texto: string read FTexto write SetTexto;
  end;

  { TPaginaMig }

  /// Base of the pages: the page covers the window, paints the theme background
  /// and keeps its controls in the content area (below the banner, above the
  /// buttons).
  TPaginaMig = class(TPanel)
  private
    FEstado: TEstadoMig;
    FFundo: TImage;
    FOnMudou: TNotifyEvent;
    FSubtitulo: TLabel;
    FTitulo: TLabel;
    procedure PintarCores(AControle: TWinControl; ACor: TColor);
  protected
    /// Checks a CheckBox/RadioButton/Label the page created, with the theme color
    function NovaCaixa(const AEsq, ATopo, ALargura: integer): TCheckBox;
    function NovaLista(const AEsq, ATopo, ALargura, AAltura: integer): TListBox;
    function NovoBotao(const AEsq, ATopo, ALargura, AAltura: integer): TBotaoMig;
    function NovoMemo(AOnde: TWinControl; const AEsq, ATopo, ALargura,
      AAltura: integer): TMemo;
    function NovoRotulo(const AEsq, ATopo, ALargura: integer; ATamanho: integer = 10;
      ANegrito: boolean = False): TLabel;
    /// The page says the buttons must be recomputed
    procedure Mudou;
    /// The two lines at the top of the page
    procedure Titular(const ATitulo, ASubtitulo: string);
    /// Sets the bounds from 96-DPI design pixels
    procedure Colocar(AControle: TControl; const AEsq, ATopo, ALargura, AAltura: integer);
  public
    constructor Criar(AOwner: TComponent; AEstado: TEstadoMig); virtual;
    /// The page has just appeared: time to load what depends on the earlier ones
    procedure AoMostrar; virtual;
    /// May the user leave to the next page? On False, AMotivo says why (or '')
    function PodeAvancar(out AMotivo: string): boolean; virtual;
    /// Leaving to the next page: does what the page does (apply...); False stays
    function AoAvancar: boolean; virtual;
    /// Rewrites every text (language change)
    procedure AtualizarTextos; virtual;
    /// Colors the texts and the background with the theme
    procedure DefinirTema(ATema: TThemes; AFundo: TPicture; ABotao: TPicture); virtual;
    function PodeVoltar: boolean; virtual;
    /// The page is not shown (nothing to ask)
    function Pular: boolean; virtual;
    /// Text of the Next button on this page
    function RotuloAvancar: string; virtual;

    property Estado: TEstadoMig read FEstado;
    property OnMudou: TNotifyEvent read FOnMudou write FOnMudou;
  end;

  TPaginaMigClasse = class of TPaginaMig;

/// 96-DPI design pixels to screen pixels
function Esc(AValor: integer): integer;
/// Monospace font of this system
function FonteMono: string;
/// Asks a yes/no question with the window's own button texts
function Confirmar(const ATitulo, ATexto: string): boolean;

implementation

uses
  RALMig.Tela.Mensagens;

function Esc(AValor: integer): integer;
begin
  Result := (AValor * Screen.PixelsPerInch + 48) div 96;
end;

function FonteMono: string;
begin
  {$IFDEF MSWINDOWS}
  Result := 'Consolas';
  {$ELSE}{$IFDEF DARWIN}
  Result := 'Menlo';
  {$ELSE}
  Result := 'Monospace';
  {$ENDIF}{$ENDIF}
end;

function Confirmar(const ATitulo, ATexto: string): boolean;
begin
  Result := QuestionDlg(ATitulo, ATexto, mtConfirmation,
    [mrYes, cmSim, 'IsDefault', mrNo, cmNao], 0) = mrYes;
end;

{ TBotaoMig }

constructor TBotaoMig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Stretch := True;
  Cursor := crHandPoint;
  FCorTexto := clBlack;
end;

procedure TBotaoMig.Habilitar(AValor: boolean);
begin
  Enabled := AValor;
  if AValor then
    Cursor := crHandPoint
  else
    Cursor := crNo;
  Invalidate;
end;

procedure TBotaoMig.Paint;
var
  vLargura, vAltura: integer;
begin
  inherited Paint;
  if FTexto = '' then
    Exit;
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Style := [fsBold];
  Canvas.Font.Height := -Esc(15);
  if Enabled then
    Canvas.Font.Color := FCorTexto
  else
    Canvas.Font.Color := clGray;
  vLargura := Canvas.TextWidth(FTexto);
  vAltura := Canvas.TextHeight(FTexto);
  Canvas.TextOut((Width - vLargura) div 2, (Height - vAltura) div 2, FTexto);
end;

procedure TBotaoMig.SetCorTexto(AValue: TColor);
begin
  FCorTexto := AValue;
  Invalidate;
end;

procedure TBotaoMig.SetTexto(const AValue: string);
begin
  FTexto := AValue;
  Invalidate;
end;

{ TPaginaMig }

constructor TPaginaMig.Criar(AOwner: TComponent; AEstado: TEstadoMig);
begin
  inherited Create(AOwner);
  FEstado := AEstado;
  BevelOuter := bvNone;
  Caption := '';
  ParentBackground := False;
  Align := alClient;
  Visible := False;
  FFundo := TImage.Create(Self);
  FFundo.Parent := Self;
  FFundo.Align := alClient;
  FFundo.Stretch := True;
  FTitulo := NovoRotulo(MARGEM, 122, LARGURA_UTIL, 18, True);
  FSubtitulo := NovoRotulo(MARGEM, 154, LARGURA_UTIL, 10);
end;

procedure TPaginaMig.AoMostrar;
begin
end;

function TPaginaMig.AoAvancar: boolean;
begin
  Result := True;
end;

procedure TPaginaMig.AtualizarTextos;
begin
end;

procedure TPaginaMig.Colocar(AControle: TControl; const AEsq, ATopo, ALargura,
  AAltura: integer);
begin
  AControle.SetBounds(Esc(AEsq), Esc(ATopo), Esc(ALargura), Esc(AAltura));
end;

procedure TPaginaMig.DefinirTema(ATema: TThemes; AFundo: TPicture; ABotao: TPicture);
var
  vInt: integer;
begin
  FFundo.Picture.Assign(AFundo);
  PintarCores(Self, Themes[ATema].FontColor);
  for vInt := 0 to ControlCount - 1 do
    if Controls[vInt] is TBotaoMig then
    begin
      TBotaoMig(Controls[vInt]).Picture.Assign(ABotao);
      TBotaoMig(Controls[vInt]).CorTexto := Themes[ATema].FontColor;
    end;
end;

procedure TPaginaMig.Mudou;
begin
  if Assigned(FOnMudou) then
    FOnMudou(Self);
end;

function TPaginaMig.NovaCaixa(const AEsq, ATopo, ALargura: integer): TCheckBox;
begin
  Result := TCheckBox.Create(Self);
  Result.Parent := Self;
  Colocar(Result, AEsq, ATopo, ALargura, 24);
end;

function TPaginaMig.NovaLista(const AEsq, ATopo, ALargura, AAltura: integer): TListBox;
begin
  Result := TListBox.Create(Self);
  Result.Parent := Self;
  Colocar(Result, AEsq, ATopo, ALargura, AAltura);
end;

function TPaginaMig.NovoBotao(const AEsq, ATopo, ALargura, AAltura: integer): TBotaoMig;
begin
  Result := TBotaoMig.Create(Self);
  Result.Parent := Self;
  Colocar(Result, AEsq, ATopo, ALargura, AAltura);
end;

function TPaginaMig.NovoMemo(AOnde: TWinControl; const AEsq, ATopo, ALargura,
  AAltura: integer): TMemo;
begin
  Result := TMemo.Create(Self);
  Result.Parent := AOnde;
  if AOnde = Self then
    Colocar(Result, AEsq, ATopo, ALargura, AAltura)
  else
    Result.Align := alClient;
  Result.ReadOnly := True;
  Result.ScrollBars := ssAutoBoth;
  Result.WordWrap := False;
  Result.Font.Name := FonteMono;
  Result.Font.Size := 9;
end;

function TPaginaMig.NovoRotulo(const AEsq, ATopo, ALargura: integer; ATamanho: integer;
  ANegrito: boolean): TLabel;
begin
  Result := TLabel.Create(Self);
  Result.Parent := Self;
  Result.AutoSize := False;
  Result.WordWrap := True;
  Result.Font.Size := ATamanho;
  if ANegrito then
    Result.Font.Style := [fsBold];
  Colocar(Result, AEsq, ATopo, ALargura, ATamanho * 2 + 8);
end;

function TPaginaMig.PodeAvancar(out AMotivo: string): boolean;
begin
  AMotivo := '';
  Result := True;
end;

function TPaginaMig.PodeVoltar: boolean;
begin
  Result := True;
end;

procedure TPaginaMig.PintarCores(AControle: TWinControl; ACor: TColor);
var
  vInt: integer;
  vFilho: TControl;
begin
  for vInt := 0 to AControle.ControlCount - 1 do
  begin
    vFilho := AControle.Controls[vInt];
    // so o que fica direto sobre o fundo; os campos de entrada tem cor propria
    if (vFilho is TLabel) and (vFilho.Tag >= 0) then
      TLabel(vFilho).Font.Color := ACor
    else if vFilho is TCheckBox then
      TCheckBox(vFilho).Font.Color := ACor
    else if vFilho is TRadioButton then
      TRadioButton(vFilho).Font.Color := ACor;
  end;
end;

procedure TPaginaMig.Titular(const ATitulo, ASubtitulo: string);
begin
  FTitulo.Caption := ATitulo;
  FSubtitulo.Caption := ASubtitulo;
end;

function TPaginaMig.Pular: boolean;
begin
  Result := False;
end;

function TPaginaMig.RotuloAvancar: string;
begin
  Result := cmAvancar;
end;

end.
