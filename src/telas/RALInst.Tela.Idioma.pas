/// First page: the installer language.
unit RALInst.Tela.Idioma;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, StdCtrls,
  RALInst.Tela.Modelo, RALInst.Tela.Temas;

type
  /// Language page: one flag per language.
  TTelaIdioma = class(TTelaModelo)
    imBrasil: TImage;
    imEspanha: TImage;
    imSelect: TImage;
    imUSA: TImage;
    lbSubTitle: TLabel;
    procedure imUSAClick(Sender: TObject);
  private
    /// Flags centered in the page and the ring around the chosen one
    procedure Posicionar;
  protected
    procedure Resize; override;
    procedure SetLanguage(AValue: TLanguages); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

uses
  RALInst.Tela.Principal;

{ TTelaIdioma }

constructor TTelaIdioma.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DimensionarImagem(imUSA, 90, 60);
  DimensionarImagem(imEspanha, 90, 60);
  DimensionarImagem(imBrasil, 90, 60);
  DimensionarImagem(imSelect, 98, 68);
end;

procedure TTelaIdioma.imUSAClick(Sender: TObject);
begin
  // o Tag de cada bandeira e o TLanguages dela
  TelaPrincipal.Language := TLanguages(TImage(Sender).Tag);
end;

procedure TTelaIdioma.Posicionar;
var
  vImagem: TImage;
begin
  // o Resize chega durante a leitura do .lfm, antes das bandeiras existirem
  if (imSelect = nil) or (imUSA = nil) or (imEspanha = nil) or (imBrasil = nil) or
     (csLoading in ComponentState) then
    Exit;
  CentralizarLadoALado([imUSA, imEspanha, imBrasil], 74);
  case Language of
    lPortuguese: vImagem := imBrasil;
    lSpanish:    vImagem := imEspanha;
  else
    vImagem := imUSA;
  end;
  EnvolverImagem(imSelect, vImagem);
  imSelect.Visible := True;
end;

procedure TTelaIdioma.Resize;
begin
  inherited Resize;
  Posicionar;
end;

procedure TTelaIdioma.SetLanguage(AValue: TLanguages);
begin
  inherited SetLanguage(AValue);
  Posicionar;
end;

end.
