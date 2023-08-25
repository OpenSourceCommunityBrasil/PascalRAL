unit uprincipal;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, CheckLst, ValEdit, Buttons, fpJSON, jsonparser,
  urestfunctions, uconsts, lclfunctions, DefaultTranslator, LCLTranslator,
  imagefunctions, LResources, fclimage, LCLType, FPWritePNG;

type
  TSplashFormStyle = record
    Background: string;
    Banner: string;
    subtitle: string;
    Theme: string;
    button: string;
    FontColor: TColor;
  end;

  TSteps = (sLanguage, sIDE, sPath, sResources, sConfirm, sInstall, sFinish);
  TThemes = (tLight, tDark);

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckListBox1: TCheckListBox;
    clbDataEngine: TCheckListBox;
    Image1 : TImage;
    imBanner : TImage;
    imPathNext: TImage;
    imPathPrevious: TImage;
    ImageList1: TImageList;
    imConfirmBack: TImage;
    imInstallBack: TImage;
    imConfirmNext: TImage;
    imInstallClose: TImage;
    imLazarus: TImage;
    imDelphi: TImage;
    imIDEBack: TImage;
    imlogoBG: TImage;
    imResourceBack: TImage;
    imLanguageNext: TImage;
    imLangBR: TImage;
    imLangUS: TImage;
    imLangES: TImage;
    imIDENext: TImage;
    imResourceNext: TImage;
    imTheme: TImage;
    lDataEngine1: TLabel;
    lPathIDEVersion: TLabel;
    ePathFolder: TLabeledEdit;
    lPathNext: TLabel;
    lPathSubTitle: TLabel;
    lPathPrevious: TLabel;
    lDataEngine: TLabel;
    lResourcesNext: TLabel;
    lConfirmNext: TLabel;
    lInstallClose: TLabel;
    lResourcesPrevious: TLabel;
    lLanguageNext: TLabel;
    lIDEPrevious: TLabel;
    lIDESubTitle: TLabel;
    lIDENext: TLabel;
    lConfirmBack: TLabel;
    lInstallBack: TLabel;
    lResourcesSubTitle: TLabel;
    lConfirmSubTitle: TLabel;
    lInstallSubTitle: TLabel;
    lLanguageSubTitle: TLabel;
    lTheme: TLabel;
    lVersion : TLabel;
    mmConfirm: TMemo;
    mmLogInstall: TMemo;
    pBanner : TPanel;
    pConfirmaRecursos: TPanel;
    pInstall: TPanel;
    pPath: TPanel;
    pRecursos: TPanel;
    pIDE: TPanel;
    pLanguage: TPanel;
    FolderDialog: TSelectDirectoryDialog;
    selectionbox: TShape;
    IDESelector: TShape;
    SpeedButton1: TSpeedButton;
    Timer1: TTimer;
    tvResources: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure imBannerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure imBannerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure imBannerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure imConfirmBackClick(Sender: TObject);
    procedure imConfirmNextClick(Sender: TObject);
    procedure imPathNextClick(Sender: TObject);
    procedure imPathPreviousClick(Sender: TObject);
    procedure imResourceBackClick(Sender: TObject);
    procedure imResourceNextClick(Sender: TObject);
    procedure imIDEBackClick(Sender: TObject);
    procedure imLanguageNextClick(Sender: TObject);
    procedure imIDENextClick(Sender: TObject);
    procedure imThemeClick(Sender: TObject);
    procedure ImageSelect(Sender: TObject);
    procedure IDESelect(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FThemeIndex: integer;
    FIDE: integer;
    IgnoredLabels: TStrings;
    FMouseClick: TPoint;
    procedure SetTheme(aTheme: TThemes);
    procedure ConfigThemes;
    procedure SetIgnoredLabels;
    procedure Translate(aLangIndex: integer);
    procedure ShowStep(aStep: TSteps);
    procedure PreparaVersoes;
    procedure ConfiguraOpcoes;
    procedure RevisarConfiguracoes;

    procedure ImageToPanel(AStream : TStream; APanel : TPanel);
    function ImageBackground(AResource : string; APanel : TPanel) : TStream;
  public
    FPanelSteps: array of TComponent;
  end;

resourcestring
  ThemeLight = 'Claro';
  ThemeDark = 'Escuro';

var
  Form1: TForm1;
  Themes: array of TSplashFormStyle;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.imThemeClick(Sender: TObject);
begin
  if FThemeIndex = 0 then
    SetTheme(tDark)
  else
    SetTheme(tLight);
end;

procedure TForm1.SetTheme(aTheme: TThemes);
var
  I: integer;
  mem : TStream;
begin
  imBanner.Picture.LoadFromResourceName(HInstance, Themes[Ord(aTheme)].Banner);
//  imBackground.Picture.LoadFromResourceName(HInstance, Themes[Ord(aTheme)].Background);
  imTheme.Picture.LoadFromResourceName(HInstance, Themes[Ord(aTheme)].Theme);

  mem := ImageBackground(Themes[Ord(aTheme)].Background, pBanner);
  try
    ImageToPanel(mem,pBanner);
  finally
    mem.Free;
  end;

  // panels do mesmo tamanho
  mem := ImageBackground(Themes[Ord(aTheme)].Background, pLanguage);
  try
    ImageToPanel(mem, pLanguage);
    ImageToPanel(mem, pIDE);
    ImageToPanel(mem, pPath);
    ImageToPanel(mem, pRecursos);
    ImageToPanel(mem, pConfirmaRecursos);
    ImageToPanel(mem, pInstall);
  finally
    mem.Free;
  end;

  for I := 0 to pred(ComponentCount) do
  begin
    if (Components[I] is TLabel) and (IgnoredLabels.IndexOf(
      TLabel(Components[I]).Name) < 0) then
      TLabel(Components[I]).Font.Color := Themes[Ord(aTheme)].FontColor;

    if (Components[I] is TLabeledEdit) and (IgnoredLabels.IndexOf(
      TLabeledEdit(Components[I]).Name) < 0) then
      TLabeledEdit(Components[I]).EditLabel.Font.Color := Themes[Ord(aTheme)].FontColor;

    if (Components[I] is TImage) and (TImage(Components[I]).Tag = -1) then
      TImage(Components[I]).Picture.LoadFromResourceName(HInstance,
        Themes[Ord(aTheme)].button);
  end;

  if Ord(aTheme) = 0 then
    lTheme.Caption := ThemeLight
  else
    lTheme.Caption := ThemeDark;

  FThemeIndex := Ord(aTheme);
end;

procedure TForm1.ConfigThemes;
var
  theme: TSplashFormStyle;
begin
  SetLength(Themes, 2);
  // light
  theme.Background := 'LIGHTBG';
  theme.Banner := 'BANNER';
  theme.FontColor := clBlack;
  theme.Theme := 'LIGHTICON';
  theme.subtitle := 'Light';
  theme.button := 'LIGHTBTN';
  Themes[0] := theme;

  // dark
  theme.Background := 'DARKBG';
  theme.Banner := 'BANNER';
  theme.FontColor := clWhite;
  theme.Theme := 'DARKICON';
  theme.subtitle := 'Dark';
  theme.button := 'DARKBTN';
  Themes[1] := theme;
end;

procedure TForm1.SetIgnoredLabels;
var
  I: integer;
begin
  if not Assigned(IgnoredLabels) then
    IgnoredLabels := TStringList.Create
  else
    IgnoredLabels.Clear;

  for I := 0 to pred(ComponentCount) do
    if (Components[I] is TLabel) and (TLabel(Components[I]).Tag = 1) then
      IgnoredLabels.Add(TLabel(Components[I]).Name);
end;

procedure TForm1.Translate(aLangIndex: integer);
begin
  case aLangIndex of
    //PT-BR
    0: SetDefaultLang('pt_BR', 'lang');

    //EN-US
    1: SetDefaultLang('en_US', 'lang');

    //ES-ES
    2: SetDefaultLang('es_ES', 'lang');
  end;
end;

procedure TForm1.ShowStep(aStep: TSteps);
begin
  LCLFunc.EscondeControles(FPanelSteps);
  pBanner.Visible := True;

  case aStep of
    sLanguage: pLanguage.Visible := True;
    sIDE: pIDE.Visible := True;
    sPath: pPath.Visible := True;
    sResources: pRecursos.Visible := True;
    sConfirm: pConfirmaRecursos.Visible := True;
    sInstall: pInstall.Visible := True;
    sFinish: ;
  end;
end;

procedure TForm1.PreparaVersoes;
begin

end;

procedure TForm1.ConfiguraOpcoes;
begin
  //clbDataEngine.Items.Clear;
end;

procedure TForm1.RevisarConfiguracoes;
begin

end;

procedure TForm1.ImageToPanel(AStream : TStream; APanel : TPanel);
var
  timg : TImage;
begin
  AStream.Position := 0;
  timg := TImage(APanel.FindChildControl('fnd_' + APanel.Name));
  if timg = nil then
  begin
    timg := TImage.Create(Self);
    timg.Parent := APanel;
    timg.Name := 'fnd_' + APanel.Name;
  end;
  timg.Left := 0;
  timg.Top := 0;
  timg.Width := APanel.Width;
  timg.Height := APanel.Height;
  timg.Picture.LoadFromStream(AStream);
  timg.SendToBack;
end;

function TForm1.ImageBackground(AResource : string; APanel : TPanel) : TStream;
var
  res : TResourceStream;
  img1, img2, img3 : TFCLImage;
  rc1 : TRect;
  wpng : TFPWriterPNG;
begin
  res := TResourceStream.Create(HInstance, AResource, RT_RCDATA);
  try
    img1 := TFCLImage.Create(0, 0);
    try
      img1.LoadFromStream(res);
      img2 := img1.Canvas.Stretched(Self.Width, Self.Height);
      try
        rc1 := TRect.Create(APanel.Left, APanel.Top, APanel.Width+APanel.Left,
                            APanel.Height+APanel.Top);
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

procedure TForm1.ImageSelect(Sender: TObject);
begin
  selectionbox.Visible := True;
  selectionbox.Left := TImage(Sender).Left - 2;
  selectionbox.Top := TImage(Sender).Top + 13;
  selectionbox.Visible := True;
  Translate(TImage(Sender).Tag);
  lLanguageNext.Enabled := True;
end;

procedure TForm1.IDESelect(Sender: TObject);
begin
  IDESelector.Visible := True;
  IDESelector.Left := TImage(Sender).Left + 3;
  IDESelector.Top := TImage(Sender).Top;
  IDESelector.Visible := True;
  FIde := TImage(Sender).Tag;
  lIDENext.Enabled := FIde > -1;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if FolderDialog.Execute then
    ePathFolder.Text := FolderDialog.FileName;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I, J: integer;
begin
  {$IFNDEF MSWindows}
  imDelphi.Enabled := False;
  imDelphi.Visible := False;
  {$ENDIF}
  SetDefaultLang('en_US', 'lang');
  SetIgnoredLabels;
  ConfigThemes;
  FIDE := -1;
  LCLFunc.DesativaControles([lIDENext, lLanguageNext, lInstallClose]);
  LCLFunc.EscondeControles([IDESelector, selectionbox]);

  J := 0;
  SetLength(FPanelSteps, 0);
  for I := 0 to pred(ComponentCount) do
    if Components[I] is TPanel then
    begin
      SetLength(FPanelSteps, Length(FPanelSteps) + 1);
      FPanelSteps[J] := TPanel(Components[I]);
      Inc(J);
    end;

  ShowStep(sLanguage);
  SetTheme(tLight);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TImgUtils.AnimaImagemSurgir(imlogoBG, 1.0);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(IgnoredLabels) then
    IgnoredLabels.Free;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.imBannerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FMouseClick := Mouse.CursorPos;
  imBanner.Tag := 1;
end;

procedure TForm1.imBannerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if imBanner.Tag = 1 then
  begin
    Self.Left := Self.Left + (Mouse.CursorPos.X - FMouseClick.X);
    Self.Top := Self.Top + (Mouse.CursorPos.Y - FMouseClick.Y);
    FMouseClick := Mouse.CursorPos;
  end;
end;

procedure TForm1.imBannerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  imBanner.Tag := 0;
end;

procedure TForm1.imConfirmBackClick(Sender: TObject);
begin
  ShowStep(sResources);
end;

procedure TForm1.imConfirmNextClick(Sender: TObject);
begin
  ShowStep(sInstall);
end;

procedure TForm1.imPathNextClick(Sender: TObject);
begin
  ShowStep(sResources);
end;

procedure TForm1.imPathPreviousClick(Sender: TObject);
begin
  ShowStep(sIDE);
end;

procedure TForm1.imResourceBackClick(Sender: TObject);
begin
  ShowStep(sPath);
end;

procedure TForm1.imResourceNextClick(Sender: TObject);
begin
  RevisarConfiguracoes;
  ShowStep(sConfirm);
end;

procedure TForm1.imIDEBackClick(Sender: TObject);
begin
  ShowStep(sLanguage);
end;

procedure TForm1.imLanguageNextClick(Sender: TObject);
begin
  ShowStep(sIDE);
end;

procedure TForm1.imIDENextClick(Sender: TObject);
begin
  ShowStep(sPath);
end;

end.
