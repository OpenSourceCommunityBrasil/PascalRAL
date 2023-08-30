unit imagefunctions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IntfGraphics, ExtCtrls, Graphics, Forms,
  FPImage, FPWritePNG, FPReadPNG, Dialogs, FPCanvas, FPImgCanv;

type
  TFCLImage = class;

  { TFCLCanvas }

  TFCLCanvas = class(TFPImageCanvas)
  public
    procedure Gradient(ColorStart, ColorEnd: TFPColor);
    function Stretched(AWidth, AHeight: integer): TFCLImage;
    procedure StretchImage(Source: TFPCustomImage);
    function CopyRect(ARect: TRect): TFCLImage;
  end;

  { TFCLImage }

  TFCLImage = class(TFPMemoryImage)
  private
    FCanvas: TFCLCanvas;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
  published
    property Canvas: TFCLCanvas read FCanvas write FCanvas;
  end;

  TImageAnimDirection = (iadUP, iadDOWN, iadRIGHT, iadLEFT);

  { TImgUtils }

  TImgUtils = class
  private
    FPorc: integer;
  public
    procedure pintaImagemBaixoCima(aImage: TImage; aProgress: double);
    procedure pintaImagemCimaBaixo(aImage: TImage; aProgress: double);
    class procedure AnimaImagemFade(aImage: TImage; aOpacity: byte;
      aDuracao: single = 2.0; aDirecao: TImageAnimDirection = iadUP);
    class procedure AnimaImagemSurgir(aImage: TImage; aDuracao: single = 2.0);

    class function Ofuscar(aImage: TImage; aAlphaPercent: integer = 100): TStream;
    class function Ofuscar(aImage: TStream; aAlphaPercent: integer = 100): TStream;
  published
  end;

implementation

{ TFCLCanvas }

procedure TFCLCanvas.Gradient(ColorStart, ColorEnd: TFPColor);
var
  i, j: integer;

  difR, difG, difB: integer;
  somR, somG, somB: real;
  rred, rgre, rblu: real;
  r, g, b: word;
begin
  difR := ColorStart.Red - ColorEnd.Red;
  difG := ColorStart.Green - ColorEnd.Green;
  difB := ColorStart.Blue - ColorEnd.Blue;

  somR := difR / Height;
  somG := difG / Height;
  somB := difB / Height;

  rred := ColorStart.Red;
  rgre := ColorStart.Green;
  rblu := ColorStart.Blue;

  for i := 0 to Height - 1 do
  begin
    for j := 0 to Width - 1 do
    begin
      r := Round(rred);
      g := Round(rgre);
      b := Round(rblu);

      Colors[j, i] := FPColor(r, g, b);
    end;
    rred := rred - somR;
    rgre := rgre - somG;
    rblu := rblu - somB;
  end;
end;

function TFCLCanvas.Stretched(AWidth, AHeight: integer): TFCLImage;
var
  dx, dy, iw, ih: integer;
begin
  Result := TFCLImage.Create(AWidth, AHeight);

  iw := Self.Width;
  ih := Self.Height;

  for dx := 0 to AWidth - 1 do
  begin
    for dy := 0 to AHeight - 1 do
    begin
      Result.Colors[dx, dy] := Colors[dx * iw div AWidth, dy * ih div AHeight];
    end;
  end;
end;

procedure TFCLCanvas.StretchImage(Source: TFPCustomImage);
var
  dx, dy, iw, ih: integer;
begin
  iw := Source.Width;
  ih := Source.Height;

  for dx := 0 to Width - 1 do
  begin
    for dy := 0 to Height - 1 do
    begin
      Colors[dx, dy] := Source.Colors[dx * iw div Width, dy * ih div Height];
    end;
  end;
end;

function TFCLCanvas.CopyRect(ARect: TRect): TFCLImage;
var
  dx, dy, ix, iy: integer;
begin
  if ARect.Left < 0 then
  begin
    ARect.Right := ARect.Right + ARect.Left;
    ARect.Left := 0;
  end;
  if ARect.Top < 0 then
  begin
    ARect.Bottom := ARect.Bottom + ARect.Top;
    ARect.Top := 0;
  end;
  if ARect.Width > Width then
    ARect.Width := Width;
  if ARect.Height > Height then
    ARect.Height := Height;

  Result := TFCLImage.Create(ARect.Width, ARect.Height);

  iy := 0;
  for dy := ARect.Top to ARect.Bottom - 1 do
  begin
    ix := 0;
    for dx := ARect.Left to ARect.Right - 1 do
    begin
      Result.Colors[ix, iy] := Colors[dx, dy];
      ix := ix + 1;
    end;
    iy := iy + 1;
  end;
end;

{ TFCLImage }

constructor TFCLImage.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth, AHeight);
  UsePalette := False;

  FCanvas := TFCLCanvas.Create(Self);
  FCanvas.Brush.FPColor := colBlack;
  FCanvas.Pen.FPColor := colBlack;
  FCanvas.Pen.Style := psSolid;
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Pen.Width := 1;
end;

destructor TFCLImage.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

{ TImgUtils }

procedure TImgUtils.pintaImagemBaixoCima(aImage: TImage; aProgress: double);
var
  i, j, hporc: integer;
  png1, png2: TPortableNetworkGraphic;
  Opacity: byte;
  t: TLazIntfImage;
  col: TFPColor;
begin
  png1 := TPortableNetworkGraphic.Create;
  png1.Assign(aImage.Picture.Graphic);

  png2 := TPortableNetworkGraphic.Create;
  png2.Width := png1.Width;
  png2.Height := png1.Height;
  png2.PixelFormat := pf32bit;
  Opacity := 220;

  try
    t := png1.CreateIntfImage;

    hporc := Trunc(aProgress * t.Height / 100);
    hporc := t.Height - hporc;

    for I := 0 to t.Height - 1 do
      for J := 0 to t.Width - 1 do
      begin
        col := t.Colors[j, i];
        if i <= hporc then
        begin
          col.Red := (col.Red * Opacity) div $FF;
          col.Green := (col.Green * Opacity) div $FF;
          col.Blue := (col.Blue * Opacity) div $FF;
        end;
        t.Colors[j, i] := col;
      end;
    png2.LoadFromIntfImage(t);
    aImage.Picture.Assign(png2);

  finally
    png1.Free;
    png2.Free;
  end;
end;

procedure TImgUtils.pintaImagemCimaBaixo(aImage: TImage; aProgress: double);
var
  i, j, hporc: integer;
  png1, png2: TPortableNetworkGraphic;
  Opacity: byte;
  t: TLazIntfImage;
  col: TFPColor;
begin
  png1 := TPortableNetworkGraphic.Create;
  png1.Assign(aImage.Picture.Graphic);

  png2 := TPortableNetworkGraphic.Create;
  png2.Width := png1.Width;
  png2.Height := png1.Height;
  png2.PixelFormat := pf32bit;
  Opacity := 225;

  try
    t := png1.CreateIntfImage;

    hporc := Trunc(aProgress * t.Height / 100);

    for I := 0 to t.Height - 1 do
      for J := 0 to t.Width - 1 do
      begin
        col := t.Colors[j, i];
        if i >= hporc then
        begin
          col.Red := (col.Red * Opacity) div $FF;
          col.Green := (col.Green * Opacity) div $FF;
          col.Blue := (col.Blue * Opacity) div $FF;
        end;
        t.Colors[j, i] := col;
      end;
    png2.LoadFromIntfImage(t);
    aImage.Picture.Assign(png2);
  finally
    png1.Free;
    png2.Free;
  end;
end;

class procedure TImgUtils.AnimaImagemFade(aImage: TImage; aOpacity: byte;
  aDuracao: single; aDirecao: TImageAnimDirection);
var
  i, j, hporc, aprogress: integer;
  png1, png2: TPortableNetworkGraphic;
  t: TLazIntfImage;
  col: TFPColor;
begin
  png1 := TPortableNetworkGraphic.Create;
  png1.Assign(aImage.Picture.Graphic);

  png2 := TPortableNetworkGraphic.Create;
  png2.Width := png1.Width;
  png2.Height := png1.Height;
  png2.PixelFormat := pf32bit;

  aprogress := 1;
  t := png1.CreateIntfImage;
  try
    while aprogress < t.Height - 1 do
    begin
      hporc := Trunc(aProgress * t.Height / 100);
      if aDirecao = iadUP then
        hporc := t.Height - hporc;

      for I := 0 to hporc - 1 do
        for J := 0 to t.Width - 1 do
        begin
          col := t.Colors[j, i];
          col.Red := (col.Red * aOpacity) div $FF;
          col.Green := (col.Green * aOpacity) div $FF;
          col.Blue := (col.Blue * aOpacity) div $FF;
          t.Colors[j, i] := col;
        end;
      png2.LoadFromIntfImage(t);
      aImage.Picture.Assign(png2);
      Application.ProcessMessages;
      Sleep(1);
      Inc(aprogress);
    end;
  finally
    t.Free;
    png1.Free;
    png2.Free;
  end;
end;

class procedure TImgUtils.AnimaImagemSurgir(aImage: TImage; aDuracao: single);
var
  BaseImage, OfuscImage: TMemoryStream;
  porc, inc_porc: double;
  tick_ini, tick_fim: QWord;
  time_duracao, tempo_exec: int64;
begin
  BaseImage := TMemoryStream.Create;
  try
    aImage.Picture.SaveToStream(BaseImage);
    time_duracao := Trunc(aDuracao * 1000);
    porc := 40;
    inc_porc := 0;
    repeat
      tick_ini := GetTickCount64;
      porc := porc + inc_porc;
      BaseImage.Position := 0;
      OfuscImage := TMemoryStream(Ofuscar(BaseImage, Trunc(porc)));
      try
        aImage.Picture.LoadFromStream(OfuscImage);
      finally
        FreeAndNil(OfuscImage);
      end;
      Application.ProcessMessages;
      tick_fim := GetTickCount64;
      tempo_exec := tick_fim - tick_ini;
      time_duracao := time_duracao - tempo_exec;
      if time_duracao > 0 then
        inc_porc := (100 - porc) / (time_duracao / tempo_exec)
      else if porc < 100 then
        inc_porc := (100 - porc);
    until (porc >= 100.0);
  finally
    BaseImage.Free;
  end;
end;

class function TImgUtils.Ofuscar(aImage: TImage; aAlphaPercent: integer): TStream;
var
  mem: TMemoryStream;
begin
  mem := TMemoryStream.Create;
  try
    aImage.Picture.SaveToStream(mem);
    Result := Ofuscar(mem, aAlphaPercent);
  finally
    FreeAndNil(mem);
  end;
end;

class function TImgUtils.Ofuscar(aImage: TStream; aAlphaPercent: integer): TStream;
var
  img: TFPMemoryImage;
  i, j: integer;
  fp, fpf: TFPColor;
  wr: TFPWriterPNG;
begin
  if aAlphaPercent > 100 then
    aAlphaPercent := 100
  else if aAlphaPercent < 0 then
    aAlphaPercent := 0;

  img := TFPMemoryImage.Create(0, 0);
  try
    img.LoadFromStream(aImage);

    fpf := TColorToFPColor(clWhite);
    fpf.Alpha := round($FFFF * ((100 - aAlphaPercent) / 100));

    for i := 0 to img.Height - 1 do
      for j := 0 to img.Width - 1 do
      begin
        fp := img.Colors[j, i];
        if fp.Alpha > 30 shl 8 then
        begin
          fp := FPImage.AlphaBlend(fp, fpf);
          img.Colors[j, i] := fp;
        end
        else
          img.Colors[j, i] := colTransparent;
      end;

    Result := TMemoryStream.Create;
    wr := TFPWriterPNG.Create;
    try
      wr.UseAlpha := True;
      img.SaveToStream(Result, wr);
    finally
      FreeAndNil(wr);
    end;
    Result.Position := 0;
  finally
    FreeAndNil(img);
  end;
end;

end.
