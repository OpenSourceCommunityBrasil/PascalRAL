/// First page: the language of the migrator, one flag per language.
unit RALMig.Tela.Idioma;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics,
  RALInst.Tela.Temas, RALMig.Tela.Estado, RALMig.Tela.Modelo;

type
  /// Language page.
  TPaginaIdioma = class(TPaginaMig)
  private
    FAnel: TShape;
    FBandeiras: array[TLanguages] of TImage;
    FIdioma: TLanguages;
    FOnEscolher: TNotifyEvent;
    procedure BandeiraClick(Sender: TObject);
    procedure Posicionar;
    procedure SetIdioma(AValue: TLanguages);
  public
    constructor Criar(AOwner: TComponent; AEstado: TEstadoMig); override;
    procedure AtualizarTextos; override;

    /// The chosen language
    property Idioma: TLanguages read FIdioma write SetIdioma;
    /// A flag was clicked
    property OnEscolher: TNotifyEvent read FOnEscolher write FOnEscolher;
  end;

implementation

uses
  RALMig.Tela.Mensagens;

const
  RECURSOS_BANDEIRA: array[TLanguages] of string = ('FLAG_BR', 'FLAG_US', 'FLAG_ES');
  // a ordem na tela: ingles, espanhol, portugues
  ORDEM_BANDEIRAS: array[0..2] of TLanguages = (lEnglish, lSpanish, lPortuguese);

{ TPaginaIdioma }

constructor TPaginaIdioma.Criar(AOwner: TComponent; AEstado: TEstadoMig);
var
  vInt: integer;
  vLang: TLanguages;
begin
  inherited Criar(AOwner, AEstado);
  FIdioma := lEnglish;
  FAnel := TShape.Create(Self);
  FAnel.Parent := Self;
  FAnel.Shape := stRoundRect;
  FAnel.Brush.Style := bsClear;
  FAnel.Pen.Width := 3;
  FAnel.Pen.Color := RGBToColor(46, 170, 220);
  for vInt := Low(ORDEM_BANDEIRAS) to High(ORDEM_BANDEIRAS) do
  begin
    vLang := ORDEM_BANDEIRAS[vInt];
    FBandeiras[vLang] := TImage.Create(Self);
    FBandeiras[vLang].Parent := Self;
    FBandeiras[vLang].Stretch := True;
    FBandeiras[vLang].Proportional := True;
    FBandeiras[vLang].Center := True;
    FBandeiras[vLang].Cursor := crHandPoint;
    // o Tag de cada bandeira e o TLanguages dela
    FBandeiras[vLang].Tag := Ord(vLang);
    FBandeiras[vLang].OnClick := @BandeiraClick;
    GetResourceImage(RECURSOS_BANDEIRA[vLang], FBandeiras[vLang]);
    Colocar(FBandeiras[vLang], 221 + vInt * 164, 290, 90, 60);
  end;
  Posicionar;
end;

procedure TPaginaIdioma.AtualizarTextos;
begin
  Titular(cmIdiomaTitulo, cmIdiomaSub);
  Posicionar;
end;

procedure TPaginaIdioma.BandeiraClick(Sender: TObject);
begin
  FIdioma := TLanguages(TImage(Sender).Tag);
  Posicionar;
  if Assigned(FOnEscolher) then
    FOnEscolher(Self);
end;

procedure TPaginaIdioma.Posicionar;
var
  vFolga: integer;
begin
  vFolga := Esc(8);
  FAnel.SetBounds(FBandeiras[FIdioma].Left - vFolga, FBandeiras[FIdioma].Top - vFolga,
    FBandeiras[FIdioma].Width + 2 * vFolga, FBandeiras[FIdioma].Height + 2 * vFolga);
  FAnel.BringToFront;
end;

procedure TPaginaIdioma.SetIdioma(AValue: TLanguages);
begin
  FIdioma := AValue;
  Posicionar;
end;

end.
