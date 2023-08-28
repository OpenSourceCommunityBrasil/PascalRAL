unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  FPImage, FPWritePNG, FPReadPNG;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Panel1: TPanel;
    procedure BitBtn1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}


{ TForm1 }

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  mem : TMemoryStream;
  img : TFPMemoryImage;
  i,j : integer;
  fp, fpf : TFPColor;
  wr : TFPWriterPNG;
begin
  mem := TMemoryStream.Create;
  Image1.Picture.SaveToStream(mem);
  mem.Position := 0;

  img := TFPMemoryImage.Create(512,512);
  img.LoadFromStream(mem);

  mem.Free;

  fpf := TColorToFPColor(clRed);
  fpf.Alpha := 255 shl 8;

  for i := 0 to img.Height-1 do begin
    for j := 0 to img.Width-1 do begin
      fp := img.Colors[j,i];
      if fp.Alpha > 30 shl 8 then begin
        fp := FPImage.AlphaBlend(fp,fpf);
        img.Colors[j,i] := fp;
      end
      else begin
        img.Colors[j,i] := colTransparent;
      end;
    end;
  end;

  mem := TMemoryStream.Create;
  wr := TFPWriterPNG.create;
  wr.UseAlpha := True;
  img.SaveToStream(mem,wr);

  wr.Free;
  img.Free;

  mem.Position := 0;
  Image3.Picture.LoadFromStream(mem);
  mem.Free;
end;

end.

