unit frmodelo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Dialogs, Graphics,
  install_types, install_tools;

type

  { Tfmodelo }

  Tfmodelo = class(TFrame)
    imgFundo: TImage;
    imBack: TImage;
    imNext: TImage;
    lbNext: TLabel;
    lbBack: TLabel;
    lbSubTitle: TLabel;
    procedure FrameResize(Sender: TObject);
    procedure lbNextClick(Sender: TObject);
    procedure lbBackClick(Sender: TObject);
  private
    FIdTela: integer;
    FOnNext: TNotifyEvent;
    FOnBack: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FTheme: TThemes;
    procedure SetTheme(AValue: TThemes);
  public
    constructor Create(TheOwner: TComponent); override;

    procedure recalcAligns; virtual;
    procedure validaControls; virtual;
  published
    property OnNext: TNotifyEvent read FOnNext write FOnNext;
    property OnBack: TNotifyEvent read FOnBack write FOnBack;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property IdTela: integer read FIdTela write FIdTela;
    property Theme: TThemes read FTheme write SetTheme;
  end;

  TCmodelo = class of Tfmodelo;

implementation

{$R *.lfm}

{ Tfmodelo }

procedure Tfmodelo.lbNextClick(Sender: TObject);
begin
  if lbNext.Enabled and Assigned(FOnNext) then
    FOnNext(Self);
end;

procedure Tfmodelo.FrameResize(Sender: TObject);
begin
  recalcAligns;
end;

procedure Tfmodelo.lbBackClick(Sender: TObject);
begin
  if lbBack.Enabled and Assigned(FOnBack) then
    FOnBack(Self);
end;

procedure Tfmodelo.SetTheme(AValue: TThemes);
var
  vInt: integer;
begin
  if ImgBackground <> nil then
  begin
    ImgBackground.Position := 0;
    imgFundo.Picture.LoadFromStream(ImgBackground);
  end;

  for vInt := 0 to Pred(ComponentCount) do
  begin
    if (Components[vInt] is TLabel) and (TLabel(Components[vInt]).Tag = 0) then
      TLabel(Components[vInt]).Font.Color := Themes[AValue].FontColor
    else if (Components[vInt] is TLabel) and (TLabel(Components[vInt]).Tag = 1) then
      TLabel(Components[vInt]).Font.Color := clWhite
    else if (Components[vInt] is TLabeledEdit) and
      (TLabeledEdit(Components[vInt]).Tag = 0) then
      TLabeledEdit(Components[vInt]).EditLabel.Font.Color := Themes[AValue].FontColor
    else if (Components[vInt] is TImage) and (TImage(Components[vInt]).Tag = -1) then
      TImage(Components[vInt]).Picture.LoadFromResourceName(HInstance,
        Themes[AValue].Button);
  end;

  FTheme := AValue;
end;

constructor Tfmodelo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  recalcAligns;
end;

procedure Tfmodelo.recalcAligns;
begin
  lbSubTitle.Left := (Self.Width div 2) - (lbSubTitle.Width div 2);
end;

procedure Tfmodelo.validaControls;
begin

end;

end.
