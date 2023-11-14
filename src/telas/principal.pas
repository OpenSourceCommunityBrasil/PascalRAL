unit principal;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  delphiutils,
  {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, LResources, LCLType, fpJSON, jsonparser,
  DefaultTranslator, LCLTranslator, FPWritePNG,

  frame_modelo, frame_idioma, frame_ide, frame_config_recursos,
  frame_install_recursos, frame_confirm_recursos, frame_install,
  install_types, install_tools, imagefunctions;

type
  { Tfprincipal }

  Tfprincipal = class(TForm)
    imClose: TImage;
    imBackground : TImage;
    imBanner: TImage;
    imTheme : TImage;
    lTheme : TLabel;
    lVersion: TLabel;
    pBanner: TPanel;
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure imBannerMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    procedure imBannerMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
    procedure imBannerMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    procedure imCloseClick(Sender : TObject);
    procedure imThemeClick(Sender : TObject);
  protected
    frm_idioma : Tfframe_idioma;
    frm_ide : Tfframe_ide;
    frm_path : Tfframe_config_recursos;
    frm_resources : Tfframe_install_recursos;
    frm_confirm : Tfframe_confirm_recursos;
    frm_install : Tfframe_install;
  private
    FTheme : TThemes;
    FMouseClick: TPoint;

    function CreateFrameStep(frame_class : Tfframe_modelo_class) : Tfframe_modelo;

    procedure frmBack(Sender : TObject);
    procedure frmNext(Sender : TObject);
    procedure frmChangeLang(Sender : TObject);

    procedure BringToFrontFrame(AIdTela : integer);
    procedure SetTheme(ATheme: TThemes);
    function ImageBackgroundFrames(AResource: string): TStream;
    procedure Translate(ALang: TLanguages);
  public
    function GetIDESelected : integer;
  end;

var
  fprincipal: Tfprincipal;

implementation

{$R *.lfm}

{ Tfprincipal }

procedure Tfprincipal.FormCreate(Sender : TObject);
begin
  imBackground.Left := 0;
  imBackground.Top := 0;
  imBackground.Width := Self.ClientWidth;
  imBackground.Height := Self.ClientHeight;

  frm_idioma    := Tfframe_idioma(CreateFrameStep(Tfframe_idioma));
  frm_ide       := Tfframe_ide(CreateFrameStep(Tfframe_ide));
  frm_path      := Tfframe_config_recursos(CreateFrameStep(Tfframe_config_recursos));
  frm_resources := Tfframe_install_recursos(CreateFrameStep(Tfframe_install_recursos));
  frm_confirm   := Tfframe_confirm_recursos(CreateFrameStep(Tfframe_confirm_recursos));
  frm_install   := Tfframe_install(CreateFrameStep(Tfframe_install));

  frm_idioma.OnChangeLang := @frmChangeLang;
end;

procedure Tfprincipal.FormShow(Sender : TObject);
begin
  BringToFrontFrame(1);
  SetTheme(tLight);
end;

procedure Tfprincipal.imBannerMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  FMouseClick := Mouse.CursorPos;
  imBanner.Tag := 1;
end;

procedure Tfprincipal.imBannerMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
begin
  if imBanner.Tag = 1 then
  begin
    Self.Left := Self.Left + (Mouse.CursorPos.X - FMouseClick.X);
    Self.Top := Self.Top + (Mouse.CursorPos.Y - FMouseClick.Y);
    FMouseClick := Mouse.CursorPos;
  end;
end;

procedure Tfprincipal.imBannerMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  imBanner.Tag := 0;
end;

procedure Tfprincipal.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  Application.Terminate;
end;

procedure Tfprincipal.imCloseClick(Sender : TObject);
begin
  Close;
end;

procedure Tfprincipal.imThemeClick(Sender : TObject);
begin
  if imTheme.Tag = 1 then
    SetTheme(tLight)
  else
    SetTheme(tDark);

  imTheme.Tag := (imTheme.Tag + 1) mod Length(Themes);
end;

function Tfprincipal.CreateFrameStep(frame_class : Tfframe_modelo_class) : Tfframe_modelo;
begin
  Result := frame_class.Create(Self);
  Result.Parent := Self;
  Result.Top := imBanner.Height;
  Result.Align := alClient;
  Result.OnBack := @frmBack;
  Result.OnNext := @frmNext;
  Result.recalcAligns;
end;

procedure Tfprincipal.frmBack(Sender : TObject);
var
  vFrame : Tfframe_modelo;
begin
  vFrame := Tfframe_modelo(Sender);
  BringToFrontFrame(vFrame.IdTela - 1);
end;

procedure Tfprincipal.frmNext(Sender : TObject);
var
  vFrame : Tfframe_modelo;
begin
  vFrame := Tfframe_modelo(Sender);
  BringToFrontFrame(vFrame.IdTela + 1);
end;

procedure Tfprincipal.frmChangeLang(Sender : TObject);
var
  vFrame : Tfframe_idioma;
begin
  vFrame := Tfframe_idioma(Sender);
  Translate(vFrame.Language);
  lTheme.Caption := getThemeCaption(FTheme);
end;

procedure Tfprincipal.BringToFrontFrame(AIdTela : integer);
var
  vInt : integer;
begin
  for vInt := 0 to Pred(ComponentCount) do begin
    if (Components[vInt].InheritsFrom(Tfframe_modelo)) and
       (Tfframe_modelo(Components[vInt]).IdTela = AIdTela) then
    begin
      Tfframe_modelo(Components[vInt]).BringToFront;
      Tfframe_modelo(Components[vInt]).validaControls;
    end;
  end;
end;

procedure Tfprincipal.SetTheme(ATheme : TThemes);
var
  vInt : integer;
begin
  imBanner.Picture.LoadFromResourceName(HInstance, Themes[ATheme].Banner);
  imTheme.Picture.LoadFromResourceName(HInstance, Themes[ATheme].Theme);
  imBackground.Picture.LoadFromResourceName(HInstance, Themes[ATheme].Background);

  {$IFDEF LINUX}
    ImgBackground := ImageBackgroundFrames(Themes[ATheme].Background);
  {$ENDIF}

  for vInt := 0 to Pred(ComponentCount) do
  begin
    if (Components[vInt] is TLabel) and (TLabel(Components[vInt]).Tag = 0) then
      TLabel(Components[vInt]).Font.Color := Themes[ATheme].FontColor
    else if (Components[vInt] is TLabel) and (TLabel(Components[vInt]).Tag = 1) then
      TLabel(Components[vInt]).Font.Color := clWhite
    else if (Components[vInt] is TLabeledEdit) and (TLabeledEdit(Components[vInt]).Tag = 0) then
      TLabeledEdit(Components[vInt]).EditLabel.Font.Color := Themes[ATheme].FontColor
    else if (Components[vInt] is TImage) and (TImage(Components[vInt]).Tag = -1) then
      TImage(Components[vInt]).Picture.LoadFromResourceName(HInstance, Themes[ATheme].Button);
  end;

  for vInt := 0 to Pred(ComponentCount) do begin
    if (Components[vInt].InheritsFrom(Tfframe_modelo)) then
      Tfframe_modelo(Components[vInt]).Theme := ATheme;
  end;

  lTheme.Caption := getThemeCaption(ATheme);
  FTheme := ATheme;
end;

function Tfprincipal.ImageBackgroundFrames(AResource : string) : TStream;
var
  res: TResourceStream;
  img1, img2, img3: TFCLImage;
  rc1: TRect;
  wpng: TFPWriterPNG;
begin
  res := TResourceStream.Create(HInstance, AResource, RT_RCDATA);
  try
    img1 := TFCLImage.Create(0, 0);
    try
      img1.LoadFromStream(res);
      img2 := img1.Canvas.Stretched(Self.Width, Self.Height);
      try
        rc1 := TRect.Create(0, imBanner.Height, Self.ClientWidth, Self.ClientHeight);
        img3 := img2.Canvas.CopyRect(rc1);
        try
          Result := TMemoryStream.Create;
          wpng := TFPWriterPNG.Create;
          try
            wpng.UseAlpha := True;
            img3.SaveToStream(Result, wpng);
            Result.Position := 0;
          finally
            FreeAndNil(wpng);
          end;
        finally
          FreeAndNil(img3);
        end;
      finally
        FreeAndNil(img2);
      end;
    finally
      FreeAndNil(img1);
    end;
  finally
    FreeAndNil(res);
  end;
end;

procedure Tfprincipal.Translate(ALang : TLanguages);
begin
  case ALang of
    lPortuguese : SetDefaultLang('pt_BR', 'lang');
    lEnglish    : SetDefaultLang('en_US', 'lang');
    lSpanish    : SetDefaultLang('es_ES', 'lang');
  end;
end;

function Tfprincipal.GetIDESelected : integer;
begin
  Result := frm_ide.IDE;
end;

end.
