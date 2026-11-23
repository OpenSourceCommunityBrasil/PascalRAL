/// Base of the migrator pages, on the installer's page (RALInst.Tela.Modelo:
/// background, banner, theme switch, image buttons): the two title lines, the
/// reason a blocked Next shows, and what the window asks of a page to walk the
/// migration (skip it, may it go on, what leaving it does, the text of Next).
unit RALMig.Tela.Pagina;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Dialogs,
  RALInst.Tela.Modelo, RALMig.Tela.Estado;

type
  /// Base frame of the migrator pages.
  TTelaMig = class(TTelaModelo)
    lbDescricao: TLabel;
    lbMotivo: TLabel;
    lbTitulo: TLabel;
    /// A label beside a check box or radio button (its FocusControl): a click
    /// on the text is a click on the box, as in the installer
    procedure RotuloDaCaixaClick(Sender: TObject);
  private
    FEstado: TEstadoMig;
    FOnMudou: TNotifyEvent;
  protected
    /// Turns a link (an underlined label that acts as a button) on or off
    procedure HabilitarLink(ALink: TLabel; AValor: boolean);
    /// The page says the buttons must be recomputed
    procedure Mudou;
    /// The two lines at the top of the page
    procedure Titular(const ATitulo, ADescricao: string);
    /// Next: the page may go on and did what leaving it does
    function ValidatePageNext: boolean; override;
    function ValidatePagePrior: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    /// Leaving to the next page: does what the page does (apply...); False stays
    function AoAvancar: boolean; virtual;
    /// Next and Previous of the page: text, enabled and the reason a blocked
    /// Next shows; AVoltar says whether there is a page to go back to
    procedure AtualizarNavegacao(AVoltar: boolean);
    /// May the user leave to the next page? On False, AMotivo says why (or '')
    function PodeAvancar(out AMotivo: string): boolean; virtual;
    function PodeVoltar: boolean; virtual;
    /// The page is not shown (nothing to ask)
    function Pular: boolean; virtual;
    /// Text of the Next button on this page
    function RotuloAvancar: string; virtual;

    /// The migration the window prepares (set by the window after creating)
    property Estado: TEstadoMig read FEstado write FEstado;
    property OnMudou: TNotifyEvent read FOnMudou write FOnMudou;
  end;

/// Asks a yes/no question with the window's own button texts
function Confirmar(const ATitulo, ATexto: string): boolean;
/// Monospace font of this system
function FonteMono: string;

implementation

{$R *.lfm}

uses
  RALMig.Tela.Mensagens;

function Confirmar(const ATitulo, ATexto: string): boolean;
begin
  Result := QuestionDlg(ATitulo, ATexto, mtConfirmation,
    [mrYes, cmSim, 'IsDefault', mrNo, cmNao], 0) = mrYes;
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

{ TTelaMig }

constructor TTelaMig.Create(AOwner: TComponent);
var
  vInt: integer;
begin
  inherited Create(AOwner);
  // diff, evidencias e saida do compilador sao texto alinhado por coluna
  for vInt := 0 to Pred(ComponentCount) do
    if Components[vInt] is TMemo then
    begin
      TMemo(Components[vInt]).Font.Name := FonteMono;
      TMemo(Components[vInt]).Font.Size := 9;
    end;
end;

function TTelaMig.AoAvancar: boolean;
begin
  Result := True;
end;

procedure TTelaMig.AtualizarNavegacao(AVoltar: boolean);
var
  vMotivo: string;
begin
  lbNext.Caption := RotuloAvancar;
  PermitirAvancar(PodeAvancar(vMotivo));
  lbMotivo.Caption := vMotivo;
  lbMotivo.Visible := vMotivo <> '';
  lbBack.Caption := cmVoltar;
  lbBack.Visible := AVoltar and PodeVoltar;
  imBack.Visible := lbBack.Visible;
end;

procedure TTelaMig.HabilitarLink(ALink: TLabel; AValor: boolean);
begin
  ALink.Enabled := AValor;
  if AValor then
    ALink.Cursor := crHandPoint
  else
    ALink.Cursor := crNo;
end;

procedure TTelaMig.Mudou;
begin
  if Assigned(FOnMudou) then
    FOnMudou(Self);
end;

function TTelaMig.PodeAvancar(out AMotivo: string): boolean;
begin
  AMotivo := '';
  Result := True;
end;

function TTelaMig.PodeVoltar: boolean;
begin
  Result := True;
end;

function TTelaMig.Pular: boolean;
begin
  Result := False;
end;

function TTelaMig.RotuloAvancar: string;
begin
  Result := cmAvancar;
end;

procedure TTelaMig.RotuloDaCaixaClick(Sender: TObject);
var
  vCaixa: TWinControl;
begin
  vCaixa := TLabel(Sender).FocusControl;
  if (vCaixa = nil) or not vCaixa.Enabled or not vCaixa.Visible then
    Exit;
  if vCaixa is TCheckBox then
    TCheckBox(vCaixa).Checked := not TCheckBox(vCaixa).Checked
  else if vCaixa is TRadioButton then
    TRadioButton(vCaixa).Checked := True;
end;

procedure TTelaMig.Titular(const ATitulo, ADescricao: string);
begin
  lbTitulo.Caption := ATitulo;
  lbDescricao.Caption := ADescricao;
end;

function TTelaMig.ValidatePageNext: boolean;
var
  vMotivo: string;
begin
  Result := PodeAvancar(vMotivo) and AoAvancar;
end;

function TTelaMig.ValidatePagePrior: boolean;
begin
  Result := PodeVoltar;
end;

end.
