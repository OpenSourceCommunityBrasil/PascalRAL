unit imagefunctions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IntfGraphics, ExtCtrls, Graphics, Forms,
  FPImage, FPWritePNG, FPReadPNG, dialogs;

type
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
  ini, fin, di, i: double;
  tci, tcf: QWord;
  td, tt, tm: int64;
begin
  BaseImage := TMemoryStream.Create;
  try
    aImage.Picture.SaveToStream(BaseImage);
    ini := now;
    td := Trunc(aDuracao * 1000) - 100;
    tt := td;
    i := 40;
    di := 0;
    repeat
      tci := GetTickCount64;
      i := i + di;
      BaseImage.Position := 0;
      OfuscImage := TMemoryStream(Ofuscar(BaseImage, Trunc(i)));
      try
        aImage.Picture.LoadFromStream(OfuscImage);
      finally
        FreeAndNil(OfuscImage);
      end;
      Application.ProcessMessages;
      tcf := GetTickCount64;
      tm := tcf - tci;
      if tt > 0 then
        di := (100 - i) / (tt / tm);
      tt := tt - tm;
    until (i >= 100.0);

    fin := now;
  finally
    BaseImage.Free;
  end;

  ShowMessage(FormatDateTime('ss:zzz', fin - ini));
end;

class function TImgUtils.Ofuscar(aImage: TImage; aAlphaPercent: integer): TStream;
var
  mem : TMemoryStream;
begin
  mem := TMemoryStream.Create;
  try
    aImage.Picture.SaveToStream(mem);
    Result := Ofuscar(mem,aAlphaPercent);
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
