/// Second page: which IDE kind to install into (Delphi, Lazarus or both).
unit RALInst.Tela.IDE;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  RALInst.Tela.Modelo;

type
  /// IDE kind page.
  TTelaIDE = class(TTelaModelo)
    imDelphi: TImage;
    imLazarus: TImage;
    imSelDelphi: TImage;
    imSelLazarus: TImage;
    lbAmbos: TLabel;
    lbSubTitle: TLabel;
    procedure imDelphiClick(Sender: TObject);
    procedure imLazarusClick(Sender: TObject);
    procedure lbAmbosClick(Sender: TObject);
  private
    /// Outside Windows: says Delphi does not exist there
    FAvisoDelphi: TLabel;
    /// Icons centered in the page and the rings around the chosen ones
    procedure Posicionar;
  protected
    procedure Resize; override;
    procedure SetIDE(AValue: integer); override;
    function ValidatePageNext: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AtualizarTextos; override;
  end;

implementation

{$R *.lfm}

uses
  RALInst.Tela.Mensagens, RALInst.Tela.Principal;

{ TTelaIDE }

constructor TTelaIDE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DimensionarImagem(imDelphi, 104, 104);
  DimensionarImagem(imLazarus, 104, 104);
  DimensionarImagem(imSelDelphi, 112, 112);
  DimensionarImagem(imSelLazarus, 112, 112);
  // o Delphi so existe no Windows: fora dele a opcao nem aparece, e a tela
  // diz por que, para ninguem procurar o que nao esta faltando
  {$IFNDEF MSWINDOWS}
  imDelphi.Visible := False;
  lbAmbos.Visible := False;
  FAvisoDelphi := TLabel.Create(Self);
  FAvisoDelphi.Parent := Self;
  FAvisoDelphi.AutoSize := False;
  FAvisoDelphi.Alignment := taCenter;
  FAvisoDelphi.WordWrap := True;
  FAvisoDelphi.Anchors := [akLeft, akRight, akTop];
  FAvisoDelphi.Font.Color := lbSubTitle.Font.Color;
  {$ENDIF}
end;

procedure TTelaIDE.imDelphiClick(Sender: TObject);
begin
  TelaPrincipal.IDE := 0;
end;

procedure TTelaIDE.imLazarusClick(Sender: TObject);
begin
  TelaPrincipal.IDE := 1;
end;

procedure TTelaIDE.lbAmbosClick(Sender: TObject);
begin
  TelaPrincipal.IDE := 2;
end;

procedure TTelaIDE.AtualizarTextos;
begin
  inherited AtualizarTextos;
  if FAvisoDelphi <> nil then
    FAvisoDelphi.Caption := cmDelphiSoNoWindows;
end;

procedure TTelaIDE.Posicionar;
begin
  // o Resize chega durante a leitura do .lfm, antes dos icones existirem
  if (imSelDelphi = nil) or (imSelLazarus = nil) or (imDelphi = nil) or
     (imLazarus = nil) or (csLoading in ComponentState) then
    Exit;
  CentralizarLadoALado([imDelphi, imLazarus], 80);
  EnvolverImagem(imSelDelphi, imDelphi);
  EnvolverImagem(imSelLazarus, imLazarus);
  if FAvisoDelphi <> nil then
    FAvisoDelphi.SetBounds(Scale96ToForm(40), imLazarus.Top + imLazarus.Height +
                           Scale96ToForm(60), ClientWidth - Scale96ToForm(80),
                           Scale96ToForm(40));
end;

procedure TTelaIDE.Resize;
begin
  inherited Resize;
  Posicionar;
end;

procedure TTelaIDE.SetIDE(AValue: integer);
begin
  inherited SetIDE(AValue);
  // 0 - Delphi, 1 - Lazarus, 2 - os dois
  imSelDelphi.Visible := (AValue = 0) or (AValue = 2);
  imSelLazarus.Visible := (AValue = 1) or (AValue = 2);
  if AValue = 2 then
    lbAmbos.Font.Style := [fsUnderline, fsBold]
  else
    lbAmbos.Font.Style := [fsUnderline];
  Posicionar;
end;

function TTelaIDE.ValidatePageNext: boolean;
begin
  Result := IDE >= 0;
  if not Result then
    ShowMessage(cmEscolhaIDE);
end;

end.
