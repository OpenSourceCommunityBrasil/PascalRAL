unit fclimage;

interface

uses
  Classes, SysUtils, FPImage, FPCanvas, FPImgCanv;

type
  TFCLImage = class;

  { TFCLCanvas }

  TFCLCanvas = class(TFPImageCanvas)
  public
    procedure Gradient(ColorStart,ColorEnd : TFPColor);
    function Stretched(AWidth,AHeight : integer) : TFCLImage;
    procedure StretchImage(Source : TFPCustomImage);
    function CopyRect(ARect : TRect) : TFCLImage;
  end;

  { TFCLImage }

  TFCLImage = class(TFPMemoryImage)
  private
    FCanvas: TFCLCanvas;
  public
    constructor Create(AWidth,AHeight:integer); override;
    destructor Destroy; override;
  published
    property Canvas : TFCLCanvas read FCanvas write FCanvas;
  end;

implementation

{ TFCLCanvas }

procedure TFCLCanvas.Gradient(ColorStart, ColorEnd: TFPColor);
var
  i, j : integer;

  difR, difG, difB : integer;
  somR, somG, somB : real;
  rred, rgre, rblu : real;
  r,g,b : word;
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

  for i := 0 to Height-1 do begin
    for j := 0 to Width-1 do begin
      r := Round(rred);
      g := Round(rgre);
      b := Round(rblu);

      Colors[j,i] := FPColor(r,g,b);
    end;
    rred := rred - somR;
    rgre := rgre - somG;
    rblu := rblu - somB;
  end;
end;

function TFCLCanvas.Stretched(AWidth, AHeight: integer): TFCLImage;
var
  dx, dy, iw, ih: Integer;
begin
  Result := TFCLImage.Create(AWidth,AHeight);

  iw := Self.Width;
  ih := Self.Height;

  for dx := 0 to AWidth-1 do begin
    for dy := 0 to AHeight-1 do begin
      Result.Colors[dx,dy] := Colors[dx*iw div AWidth, dy*ih div AHeight];
    end;
  end;
end;

procedure TFCLCanvas.StretchImage(Source: TFPCustomImage);
var
  dx, dy, iw, ih: Integer;
begin
  iw := Source.Width;
  ih := Source.Height;

  for dx := 0 to Width-1 do begin
    for dy := 0 to Height-1 do begin
      Colors[dx,dy] := Source.Colors[dx*iw div Width, dy*ih div Height];
    end;
  end;
end;

function TFCLCanvas.CopyRect(ARect : TRect) : TFCLImage;
var
  dx, dy, ix, iy: Integer;
begin
  if ARect.Left < 0 then begin
    ARect.Right := ARect.Right + ARect.Left;
    ARect.Left := 0;
  end;
  if ARect.Top < 0 then begin
    ARect.Bottom := ARect.Bottom + ARect.Top;
    ARect.Top := 0;
  end;
  if ARect.Width > Width then
    ARect.Width := Width;
  if ARect.Height > Height then
    ARect.Height := Height;

  Result := TFCLImage.Create(ARect.Width,ARect.Height);
  ix := 0;
  iy := 0;

  for dy := ARect.Top to ARect.Bottom-1 do begin
    ix := 0;
    for dx := ARect.Left to ARect.Right-1 do begin
      Result.Colors[ix,iy] := Colors[dx,dy];
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
  FCanvas.Brush.FPColor:= colBlack;
  FCanvas.Pen.FPColor:= colBlack;
  FCanvas.Pen.Style:= psSolid;
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Pen.Width := 1;
end;

destructor TFCLImage.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

end.

