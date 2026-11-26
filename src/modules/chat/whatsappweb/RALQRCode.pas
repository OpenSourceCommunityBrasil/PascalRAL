{ A QR code generator (ISO/IEC 18004): text in byte mode, versions 1 to 40, the four
  error correction levels and the mask the penalty rules choose. It follows the
  algorithm of Project Nayuki's QR Code generator (MIT licence). The code is drawn as a
  1-bit BMP (TBitmap.LoadFromStream reads it on VCL, FMX and LCL), as SVG or as text for
  a console. }
unit RALQRCode;

{$I ..\..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALExceptions;

type
  /// Raised when a text does not fit a QR code.
  ERALQRCode = class(ERALException);

  /// How much of the code can be lost and still be read.
  TRALQRErrorLevel = (
    /// About 7%.
    qelLow,
    /// About 15%.
    qelMedium,
    /// About 25%.
    qelQuartile,
    /// About 30%.
    qelHigh
  );

  { A QR code: a square of Size modules, made once by the constructor and then only
    read or drawn. }
  TRALQRCode = class
  private
    /// The modules that belong to a function pattern (finder, timing, format...).
    FFunction: array of Boolean;
    FLevel: TRALQRErrorLevel;
    FMask: IntegerRAL;
    /// The dark modules, row after row.
    FModules: array of Boolean;
    FSize: IntegerRAL;
    FVersion: IntegerRAL;

    /// XORs the data modules with mask AMask (applying it twice undoes it).
    procedure ApplyMask(AMask: IntegerRAL);
    /// Draws one alignment pattern centred on AX, AY.
    procedure DrawAlignment(AX, AY: IntegerRAL);
    /// Puts the codewords in the zigzag order over the modules that are free.
    procedure DrawCodewords(const AData: TBytes);
    /// Draws a finder pattern and its separator centred on AX, AY.
    procedure DrawFinder(AX, AY: IntegerRAL);
    /// Draws both copies of the format information for the level and AMask.
    procedure DrawFormat(AMask: IntegerRAL);
    /// Draws every function pattern (the format with a placeholder mask).
    procedure DrawFunctions;
    /// Draws the two copies of the version information (version 7 and above).
    procedure DrawVersion;
    /// The data codewords with their error correction, in blocks and interleaved.
    function Interleave(const AData: TBytes): TBytes;
    /// The penalty of the current modules under the four rules.
    function Penalty: IntegerRAL;
    /// Sets a function module.
    procedure SetFunction(AX, AY: IntegerRAL; ADark: Boolean);
  public
    { The code of AText (its UTF-8 bytes) at the smallest version that holds it with
      ALevel, the level raised when it costs no extra version; raises ERALQRCode when it
      does not fit version 40. }
    constructor Create(const AText: StringRAL; ALevel: TRALQRErrorLevel = qelMedium);
    /// The code of the bytes AData, as Create does for a text.
    constructor CreateBytes(const AData: TBytes; ALevel: TRALQRErrorLevel = qelMedium);

    /// Whether the module of column AX and row AY is dark; outside the code it is light.
    function IsDark(AX, AY: IntegerRAL): Boolean;
    { Writes the code as a 1-bit BMP: AScale pixels per module and ABorder light modules
      around it (the quiet zone a reader needs is 4). }
    procedure SaveToBMP(AStream: TStream; AScale: IntegerRAL = 8; ABorder: IntegerRAL = 4);
    /// Writes the BMP of SaveToBMP to the file AFileName.
    procedure SaveToFile(const AFileName: string; AScale: IntegerRAL = 8;
      ABorder: IntegerRAL = 4);
    /// The code as an SVG document, one unit per module and ABorder modules around it.
    function ToSVG(ABorder: IntegerRAL = 4): StringRAL;
    { The code as UTF-8 text for a console, two rows per line with half-block
      characters. The light modules are the ones drawn, which reads on a dark
      background; ABorder light modules go around it. }
    function ToText(ABorder: IntegerRAL = 2): StringRAL;

    /// The error correction level, after raising.
    property Level: TRALQRErrorLevel read FLevel;
    /// The mask chosen (0 to 7).
    property Mask: IntegerRAL read FMask;
    /// The side, in modules (21 to 177).
    property Size: IntegerRAL read FSize;
    /// The version (1 to 40).
    property Version: IntegerRAL read FVersion;
  end;

implementation

resourcestring
  /// A text too long for a QR code.
  rsQRTooLong = 'The text does not fit a QR code (%d bytes).';

type
  /// Bits appended one at a time.
  TRALQRBits = record
    Bits: array of Byte;
    Count: IntegerRAL;
  end;

const
  /// The error correction codewords of a block, by level and version.
  cEccPerBlock: array[TRALQRErrorLevel, 1..40] of Byte = (
    (7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28,
     28, 28, 30, 30, 26, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),
    (10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26,
     26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28),
    (13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30,
     28, 30, 30, 30, 30, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),
    (17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28,
     30, 24, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30));
  /// The error correction blocks, by level and version.
  cEccBlocks: array[TRALQRErrorLevel, 1..40] of Byte = (
    (1, 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 4, 6, 6, 6, 6, 7, 8,
     8, 9, 9, 10, 12, 12, 12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25),
    (1, 1, 1, 2, 2, 4, 4, 4, 5, 5, 5, 8, 9, 9, 10, 10, 11, 13, 14, 16,
     17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49),
    (1, 1, 2, 2, 4, 4, 6, 6, 8, 8, 8, 10, 12, 16, 12, 17, 16, 18, 21, 20,
     23, 23, 25, 27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68),
    (1, 1, 2, 4, 4, 4, 5, 6, 8, 8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25,
     25, 34, 30, 32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81));
  /// The two format bits of each level.
  cFormatBits: array[TRALQRErrorLevel] of IntegerRAL = (1, 0, 3, 2);
  /// The penalty weights of the four rules.
  cPenaltyN1 = 3;
  cPenaltyN2 = 3;
  cPenaltyN3 = 40;
  cPenaltyN4 = 10;

/// The modules of AVersion left for data and error correction, in bits.
function RawDataModules(AVersion: IntegerRAL): IntegerRAL;
var
  vAlign: IntegerRAL;
begin
  Result := (16 * AVersion + 128) * AVersion + 64;
  if AVersion >= 2 then
  begin
    vAlign := AVersion div 7 + 2;
    Dec(Result, (25 * vAlign - 10) * vAlign - 55);
    if AVersion >= 7 then
      Dec(Result, 36);
  end;
end;

/// The data codewords of AVersion at ALevel.
function DataCodewords(AVersion: IntegerRAL; ALevel: TRALQRErrorLevel): IntegerRAL;
begin
  Result := RawDataModules(AVersion) div 8 -
    cEccPerBlock[ALevel, AVersion] * cEccBlocks[ALevel, AVersion];
end;

/// The bits of the byte count in byte mode for AVersion.
function CountBits(AVersion: IntegerRAL): IntegerRAL;
begin
  if AVersion <= 9 then
    Result := 8
  else
    Result := 16;
end;

/// The product of AX and AY in GF(2^8) modulo x^8 + x^4 + x^3 + x^2 + 1.
function GFMul(AX, AY: Byte): Byte;
var
  vZ, vInt: IntegerRAL;
begin
  vZ := 0;
  for vInt := 7 downto 0 do
  begin
    vZ := (vZ shl 1) xor ((vZ shr 7) * $11D);
    vZ := vZ xor (((AY shr vInt) and 1) * AX);
  end;
  Result := Byte(vZ);
end;

/// The Reed-Solomon generator polynomial of ADegree (without its leading 1).
function RSDivisor(ADegree: IntegerRAL): TBytes;
var
  vRoot: Byte;
  vInt, vJdx: IntegerRAL;
begin
  SetLength(Result, ADegree);
  FillChar(Result[0], ADegree, 0);
  Result[ADegree - 1] := 1;
  vRoot := 1;
  for vInt := 0 to ADegree - 1 do
  begin
    for vJdx := 0 to ADegree - 1 do
    begin
      Result[vJdx] := GFMul(Result[vJdx], vRoot);
      if vJdx + 1 < ADegree then
        Result[vJdx] := Result[vJdx] xor Result[vJdx + 1];
    end;
    vRoot := GFMul(vRoot, 2);
  end;
end;

/// The Reed-Solomon remainder of AData by ADivisor: the error correction codewords.
function RSRemainder(const AData: TBytes; AStart, ACount: IntegerRAL;
  const ADivisor: TBytes): TBytes;
var
  vInt, vJdx: IntegerRAL;
  vFactor: Byte;
begin
  SetLength(Result, Length(ADivisor));
  FillChar(Result[0], Length(Result), 0);
  for vInt := AStart to AStart + ACount - 1 do
  begin
    vFactor := AData[vInt] xor Result[0];
    Move(Result[1], Result[0], Length(Result) - 1);
    Result[Length(Result) - 1] := 0;
    for vJdx := 0 to Length(Result) - 1 do
      Result[vJdx] := Result[vJdx] xor GFMul(ADivisor[vJdx], vFactor);
  end;
end;

/// Appends the ACount low bits of AValue, the highest first.
procedure AppendBits(var ABits: TRALQRBits; AValue: Cardinal; ACount: IntegerRAL);
var
  vInt: IntegerRAL;
begin
  if ABits.Count + ACount > Length(ABits.Bits) then
    SetLength(ABits.Bits, (ABits.Count + ACount) * 2);
  for vInt := ACount - 1 downto 0 do
  begin
    ABits.Bits[ABits.Count] := (AValue shr vInt) and 1;
    Inc(ABits.Count);
  end;
end;

{ TRALQRCode }

constructor TRALQRCode.Create(const AText: StringRAL; ALevel: TRALQRErrorLevel);
var
  vData: TBytes;
begin
  SetLength(vData, Length(AText));
  if Length(vData) > 0 then
    Move(Pointer(AText)^, vData[0], Length(vData));
  CreateBytes(vData, ALevel);
end;

constructor TRALQRCode.CreateBytes(const AData: TBytes; ALevel: TRALQRErrorLevel);
var
  vBits: TRALQRBits;
  vUsed, vCapacity, vInt, vBest, vScore, vMin: IntegerRAL;
  vLevel: TRALQRErrorLevel;
  vCodewords: TBytes;
  vPad: Byte;
begin
  inherited Create;
  // the smallest version that holds the data
  FVersion := 0;
  for vInt := 1 to 40 do
  begin
    vUsed := 4 + CountBits(vInt) + 8 * Length(AData);
    if (Length(AData) < (1 shl CountBits(vInt))) and
       (vUsed <= DataCodewords(vInt, ALevel) * 8) then
    begin
      FVersion := vInt;
      Break;
    end;
  end;
  if FVersion = 0 then
    raise ERALQRCode.CreateFmt(rsQRTooLong, [Length(AData)]);
  // a higher level when it fits the same version
  FLevel := ALevel;
  for vLevel := ALevel to High(TRALQRErrorLevel) do
    if vUsed <= DataCodewords(FVersion, vLevel) * 8 then
      FLevel := vLevel;

  // the segment: mode 0100 (byte), the count, the bytes, the terminator and the pad
  vCapacity := DataCodewords(FVersion, FLevel) * 8;
  vBits.Count := 0;
  SetLength(vBits.Bits, vCapacity);
  AppendBits(vBits, 4, 4);
  AppendBits(vBits, Length(AData), CountBits(FVersion));
  for vInt := 0 to Length(AData) - 1 do
    AppendBits(vBits, AData[vInt], 8);
  vInt := vCapacity - vBits.Count;
  if vInt > 4 then
    vInt := 4;
  AppendBits(vBits, 0, vInt);
  AppendBits(vBits, 0, (8 - vBits.Count mod 8) mod 8);
  vPad := $EC;
  while vBits.Count < vCapacity do
  begin
    AppendBits(vBits, vPad, 8);
    vPad := vPad xor ($EC xor $11);
  end;
  SetLength(vCodewords, vCapacity div 8);
  FillChar(vCodewords[0], Length(vCodewords), 0);
  for vInt := 0 to vBits.Count - 1 do
    vCodewords[vInt shr 3] := vCodewords[vInt shr 3] or (vBits.Bits[vInt] shl (7 - (vInt and 7)));

  FSize := FVersion * 4 + 17;
  SetLength(FModules, FSize * FSize);
  SetLength(FFunction, FSize * FSize);
  DrawFunctions;
  DrawCodewords(Interleave(vCodewords));

  // the mask with the lowest penalty (each tried and undone)
  vBest := 0;
  vMin := MaxInt;
  for vInt := 0 to 7 do
  begin
    ApplyMask(vInt);
    DrawFormat(vInt);
    vScore := Penalty;
    if vScore < vMin then
    begin
      vMin := vScore;
      vBest := vInt;
    end;
    ApplyMask(vInt);
  end;
  FMask := vBest;
  ApplyMask(FMask);
  DrawFormat(FMask);
end;

procedure TRALQRCode.ApplyMask(AMask: IntegerRAL);
var
  vX, vY: IntegerRAL;
  vInvert: Boolean;
begin
  for vY := 0 to FSize - 1 do
    for vX := 0 to FSize - 1 do
    begin
      case AMask of
        0: vInvert := (vX + vY) mod 2 = 0;
        1: vInvert := vY mod 2 = 0;
        2: vInvert := vX mod 3 = 0;
        3: vInvert := (vX + vY) mod 3 = 0;
        4: vInvert := (vX div 3 + vY div 2) mod 2 = 0;
        5: vInvert := vX * vY mod 2 + vX * vY mod 3 = 0;
        6: vInvert := (vX * vY mod 2 + vX * vY mod 3) mod 2 = 0;
      else
        vInvert := ((vX + vY) mod 2 + vX * vY mod 3) mod 2 = 0;
      end;
      if vInvert and not FFunction[vY * FSize + vX] then
        FModules[vY * FSize + vX] := not FModules[vY * FSize + vX];
    end;
end;

procedure TRALQRCode.DrawAlignment(AX, AY: IntegerRAL);
var
  vDX, vDY, vDist: IntegerRAL;
begin
  for vDY := -2 to 2 do
    for vDX := -2 to 2 do
    begin
      vDist := Abs(vDX);
      if Abs(vDY) > vDist then
        vDist := Abs(vDY);
      SetFunction(AX + vDX, AY + vDY, vDist <> 1);
    end;
end;

procedure TRALQRCode.DrawCodewords(const AData: TBytes);
var
  vBit, vRight, vVert, vJdx, vX, vY: IntegerRAL;
  vUpward: Boolean;
begin
  vBit := 0;
  vRight := FSize - 1;
  while vRight >= 1 do
  begin
    // the vertical timing pattern takes column 6
    if vRight = 6 then
      vRight := 5;
    for vVert := 0 to FSize - 1 do
      for vJdx := 0 to 1 do
      begin
        vX := vRight - vJdx;
        vUpward := ((vRight + 1) and 2) = 0;
        if vUpward then
          vY := FSize - 1 - vVert
        else
          vY := vVert;
        if not FFunction[vY * FSize + vX] and (vBit < Length(AData) * 8) then
        begin
          FModules[vY * FSize + vX] := ((AData[vBit shr 3] shr (7 - (vBit and 7))) and 1) = 1;
          Inc(vBit);
        end;
      end;
    Dec(vRight, 2);
  end;
end;

procedure TRALQRCode.DrawFinder(AX, AY: IntegerRAL);
var
  vDX, vDY, vDist: IntegerRAL;
begin
  for vDY := -4 to 4 do
    for vDX := -4 to 4 do
    begin
      vDist := Abs(vDX);
      if Abs(vDY) > vDist then
        vDist := Abs(vDY);
      if (AX + vDX >= 0) and (AX + vDX < FSize) and (AY + vDY >= 0) and (AY + vDY < FSize) then
        SetFunction(AX + vDX, AY + vDY, (vDist <> 2) and (vDist <> 4));
    end;
end;

procedure TRALQRCode.DrawFormat(AMask: IntegerRAL);
var
  vData, vRem, vBits, vInt: IntegerRAL;

  function Bit(AIndex: IntegerRAL): Boolean;
  begin
    Result := ((vBits shr AIndex) and 1) = 1;
  end;

begin
  // 5 bits of level and mask, 10 of BCH, masked by 101010000010010
  vData := cFormatBits[FLevel] shl 3 or AMask;
  vRem := vData;
  for vInt := 0 to 9 do
    vRem := (vRem shl 1) xor ((vRem shr 9) * $537);
  vBits := ((vData shl 10) or vRem) xor $5412;
  // the copy around the top left finder
  for vInt := 0 to 5 do
    SetFunction(8, vInt, Bit(vInt));
  SetFunction(8, 7, Bit(6));
  SetFunction(8, 8, Bit(7));
  SetFunction(7, 8, Bit(8));
  for vInt := 9 to 14 do
    SetFunction(14 - vInt, 8, Bit(vInt));
  // the copy split between the other two finders, and the dark module
  for vInt := 0 to 7 do
    SetFunction(FSize - 1 - vInt, 8, Bit(vInt));
  for vInt := 8 to 14 do
    SetFunction(8, FSize - 15 + vInt, Bit(vInt));
  SetFunction(8, FSize - 8, True);
end;

procedure TRALQRCode.DrawFunctions;
var
  vInt, vJdx, vAlign, vStep, vPos: IntegerRAL;
  vPositions: array of IntegerRAL;
begin
  for vInt := 0 to FSize - 1 do
  begin
    SetFunction(6, vInt, vInt mod 2 = 0);
    SetFunction(vInt, 6, vInt mod 2 = 0);
  end;
  DrawFinder(3, 3);
  DrawFinder(FSize - 4, 3);
  DrawFinder(3, FSize - 4);
  if FVersion > 1 then
  begin
    vAlign := FVersion div 7 + 2;
    vStep := ((FVersion * 8 + vAlign * 3 + 5) div (vAlign * 4 - 4)) * 2;
    SetLength(vPositions, vAlign);
    vPositions[0] := 6;
    vPos := FSize - 7;
    for vInt := vAlign - 1 downto 1 do
    begin
      vPositions[vInt] := vPos;
      Dec(vPos, vStep);
    end;
    for vInt := 0 to vAlign - 1 do
      for vJdx := 0 to vAlign - 1 do
        // not over the three finder corners
        if not (((vInt = 0) and (vJdx = 0)) or ((vInt = 0) and (vJdx = vAlign - 1)) or
           ((vInt = vAlign - 1) and (vJdx = 0))) then
          DrawAlignment(vPositions[vInt], vPositions[vJdx]);
  end;
  // the format modules are reserved now and drawn for real once the mask is known
  DrawFormat(0);
  DrawVersion;
end;

procedure TRALQRCode.DrawVersion;
var
  vRem, vBits, vInt, vA, vB: IntegerRAL;
  vDark: Boolean;
begin
  if FVersion < 7 then
    Exit;
  vRem := FVersion;
  for vInt := 0 to 11 do
    vRem := (vRem shl 1) xor ((vRem shr 11) * $1F25);
  vBits := (FVersion shl 12) or vRem;
  for vInt := 0 to 17 do
  begin
    vDark := ((vBits shr vInt) and 1) = 1;
    vA := FSize - 11 + vInt mod 3;
    vB := vInt div 3;
    SetFunction(vA, vB, vDark);
    SetFunction(vB, vA, vDark);
  end;
end;

function TRALQRCode.Interleave(const AData: TBytes): TBytes;
var
  vBlocks, vEccLen, vRaw, vShort, vShortLen, vInt, vJdx, vPos, vLen, vOut: IntegerRAL;
  vDivisor, vEcc: TBytes;
  vParts: array of TBytes;
begin
  vBlocks := cEccBlocks[FLevel, FVersion];
  vEccLen := cEccPerBlock[FLevel, FVersion];
  vRaw := RawDataModules(FVersion) div 8;
  vShort := vBlocks - vRaw mod vBlocks;
  vShortLen := vRaw div vBlocks;
  vDivisor := RSDivisor(vEccLen);
  SetLength(vParts, vBlocks);
  vPos := 0;
  for vInt := 0 to vBlocks - 1 do
  begin
    vLen := vShortLen - vEccLen;
    if vInt >= vShort then
      Inc(vLen);
    vEcc := RSRemainder(AData, vPos, vLen, vDivisor);
    // every block is the length of a long one: a short one has a hole before its ECC
    SetLength(vParts[vInt], vShortLen + 1);
    FillChar(vParts[vInt][0], vShortLen + 1, 0);
    Move(AData[vPos], vParts[vInt][0], vLen);
    Move(vEcc[0], vParts[vInt][vShortLen + 1 - vEccLen], vEccLen);
    Inc(vPos, vLen);
  end;
  SetLength(Result, vRaw);
  vOut := 0;
  for vInt := 0 to vShortLen do
    for vJdx := 0 to vBlocks - 1 do
      if (vInt <> vShortLen - vEccLen) or (vJdx >= vShort) then
      begin
        Result[vOut] := vParts[vJdx][vInt];
        Inc(vOut);
      end;
end;

function TRALQRCode.IsDark(AX, AY: IntegerRAL): Boolean;
begin
  Result := (AX >= 0) and (AX < FSize) and (AY >= 0) and (AY < FSize) and
    FModules[AY * FSize + AX];
end;

function TRALQRCode.Penalty: IntegerRAL;
var
  vHistory: array[0..6] of IntegerRAL;

  procedure AddHistory(ARun: IntegerRAL);
  begin
    // the light border counts in front of the first run
    if vHistory[0] = 0 then
      Inc(ARun, FSize);
    Move(vHistory[0], vHistory[1], 6 * SizeOf(IntegerRAL));
    vHistory[0] := ARun;
  end;

  function CountPatterns: IntegerRAL;
  var
    vN: IntegerRAL;
    vCore: Boolean;
  begin
    vN := vHistory[1];
    vCore := (vN > 0) and (vHistory[2] = vN) and (vHistory[3] = vN * 3) and
      (vHistory[4] = vN) and (vHistory[5] = vN);
    Result := 0;
    if vCore and (vHistory[0] >= vN * 4) and (vHistory[6] >= vN) then
      Inc(Result);
    if vCore and (vHistory[6] >= vN * 4) and (vHistory[0] >= vN) then
      Inc(Result);
  end;

  function TerminateAndCount(AColor: Boolean; ARun: IntegerRAL): IntegerRAL;
  begin
    if AColor then
    begin
      AddHistory(ARun);
      ARun := 0;
    end;
    Inc(ARun, FSize);
    AddHistory(ARun);
    Result := CountPatterns;
  end;

var
  vX, vY, vRun, vDark, vTotal, vK: IntegerRAL;
  vColor, vCell: Boolean;
begin
  Result := 0;
  // runs of one colour and finder-like patterns, in rows then in columns
  for vY := 0 to FSize - 1 do
  begin
    vColor := False;
    vRun := 0;
    FillChar(vHistory, SizeOf(vHistory), 0);
    for vX := 0 to FSize - 1 do
    begin
      vCell := FModules[vY * FSize + vX];
      if vCell = vColor then
      begin
        Inc(vRun);
        if vRun = 5 then
          Inc(Result, cPenaltyN1)
        else if vRun > 5 then
          Inc(Result);
      end
      else
      begin
        AddHistory(vRun);
        if not vColor then
          Inc(Result, CountPatterns * cPenaltyN3);
        vColor := vCell;
        vRun := 1;
      end;
    end;
    Inc(Result, TerminateAndCount(vColor, vRun) * cPenaltyN3);
  end;
  for vX := 0 to FSize - 1 do
  begin
    vColor := False;
    vRun := 0;
    FillChar(vHistory, SizeOf(vHistory), 0);
    for vY := 0 to FSize - 1 do
    begin
      vCell := FModules[vY * FSize + vX];
      if vCell = vColor then
      begin
        Inc(vRun);
        if vRun = 5 then
          Inc(Result, cPenaltyN1)
        else if vRun > 5 then
          Inc(Result);
      end
      else
      begin
        AddHistory(vRun);
        if not vColor then
          Inc(Result, CountPatterns * cPenaltyN3);
        vColor := vCell;
        vRun := 1;
      end;
    end;
    Inc(Result, TerminateAndCount(vColor, vRun) * cPenaltyN3);
  end;
  // 2x2 blocks of one colour
  for vY := 0 to FSize - 2 do
    for vX := 0 to FSize - 2 do
    begin
      vCell := FModules[vY * FSize + vX];
      if (vCell = FModules[vY * FSize + vX + 1]) and (vCell = FModules[(vY + 1) * FSize + vX]) and
         (vCell = FModules[(vY + 1) * FSize + vX + 1]) then
        Inc(Result, cPenaltyN2);
    end;
  // the balance of dark and light, in steps of 5% away from half
  vDark := 0;
  for vX := 0 to Length(FModules) - 1 do
    if FModules[vX] then
      Inc(vDark);
  vTotal := FSize * FSize;
  vK := (Abs(vDark * 20 - vTotal * 10) + vTotal - 1) div vTotal - 1;
  Inc(Result, vK * cPenaltyN4);
end;

procedure TRALQRCode.SaveToBMP(AStream: TStream; AScale, ABorder: IntegerRAL);
var
  vSide, vStride, vRow, vCol, vX, vY: IntegerRAL;
  vLine: TBytes;
  vHead: array[0..61] of Byte;

  procedure PutLE(AOffset: IntegerRAL; AValue: Cardinal; AWidth: IntegerRAL);
  var
    vInt: IntegerRAL;
  begin
    for vInt := 0 to AWidth - 1 do
      vHead[AOffset + vInt] := Byte(AValue shr (8 * vInt));
  end;

begin
  if AScale < 1 then
    AScale := 1;
  if ABorder < 0 then
    ABorder := 0;
  vSide := (FSize + 2 * ABorder) * AScale;
  // a 1-bit row is padded to 4 bytes
  vStride := ((vSide + 31) div 32) * 4;
  FillChar(vHead, SizeOf(vHead), 0);
  vHead[0] := Ord('B');
  vHead[1] := Ord('M');
  PutLE(2, 62 + Cardinal(vStride) * Cardinal(vSide), 4);
  PutLE(10, 62, 4);
  PutLE(14, 40, 4);
  PutLE(18, Cardinal(vSide), 4);
  PutLE(22, Cardinal(vSide), 4);
  PutLE(26, 1, 2);
  PutLE(28, 1, 2);
  PutLE(38, 2835, 4);
  PutLE(42, 2835, 4);
  PutLE(46, 2, 4);
  // palette: 0 black, 1 white
  PutLE(58, $00FFFFFF, 4);
  AStream.WriteBuffer(vHead[0], SizeOf(vHead));
  SetLength(vLine, vStride);
  // the rows go bottom up
  for vRow := vSide - 1 downto 0 do
  begin
    FillChar(vLine[0], vStride, 0);
    vY := vRow div AScale - ABorder;
    for vCol := 0 to vSide - 1 do
    begin
      vX := vCol div AScale - ABorder;
      if not IsDark(vX, vY) then
        vLine[vCol shr 3] := vLine[vCol shr 3] or ($80 shr (vCol and 7));
    end;
    AStream.WriteBuffer(vLine[0], vStride);
  end;
end;

procedure TRALQRCode.SaveToFile(const AFileName: string; AScale, ABorder: IntegerRAL);
var
  vFile: TFileStream;
begin
  vFile := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToBMP(vFile, AScale, ABorder);
  finally
    vFile.Free;
  end;
end;

procedure TRALQRCode.SetFunction(AX, AY: IntegerRAL; ADark: Boolean);
begin
  FModules[AY * FSize + AX] := ADark;
  FFunction[AY * FSize + AX] := True;
end;

function TRALQRCode.ToSVG(ABorder: IntegerRAL): StringRAL;
var
  vX, vY, vSide: IntegerRAL;
  vPath: TStringBuilder;
begin
  if ABorder < 0 then
    ABorder := 0;
  vSide := FSize + 2 * ABorder;
  vPath := TStringBuilder.Create;
  try
    for vY := 0 to FSize - 1 do
      for vX := 0 to FSize - 1 do
        if FModules[vY * FSize + vX] then
        begin
          if vPath.Length > 0 then
            vPath.Append(' ');
          vPath.Append('M').Append(vX + ABorder).Append(',').Append(vY + ABorder).Append('h1v1h-1z');
        end;
    Result := StringRAL('<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak +
      '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="0 0 ' +
      IntToStr(vSide) + ' ' + IntToStr(vSide) + '" stroke="none">' + sLineBreak +
      '<rect width="100%" height="100%" fill="#FFFFFF"/>' + sLineBreak +
      '<path d="' + vPath.ToString + '" fill="#000000"/>' + sLineBreak + '</svg>' + sLineBreak);
  finally
    vPath.Free;
  end;
end;

function TRALQRCode.ToText(ABorder: IntegerRAL): StringRAL;
const
  // UTF-8 of U+2588, U+2580 and U+2584: full, upper half and lower half blocks
  cFull: array[0..2] of Byte = ($E2, $96, $88);
  cUpper: array[0..2] of Byte = ($E2, $96, $80);
  cLower: array[0..2] of Byte = ($E2, $96, $84);
var
  vX, vY, vLen: IntegerRAL;
  vTop, vBottom: Boolean;
  vOut: TBytes;

  procedure Put(const AChars: array of Byte);
  var
    vInt: IntegerRAL;
  begin
    if vLen + Length(AChars) > Length(vOut) then
      SetLength(vOut, (vLen + Length(AChars)) * 2);
    for vInt := 0 to High(AChars) do
    begin
      vOut[vLen] := AChars[vInt];
      Inc(vLen);
    end;
  end;

begin
  if ABorder < 0 then
    ABorder := 0;
  vLen := 0;
  SetLength(vOut, 1024);
  vY := -ABorder;
  while vY < FSize + ABorder do
  begin
    for vX := -ABorder to FSize + ABorder - 1 do
    begin
      // what is drawn is light: the two rows of the line pick the glyph
      vTop := not IsDark(vX, vY);
      vBottom := (vY + 1 < FSize + ABorder) and not IsDark(vX, vY + 1);
      if vTop and vBottom then
        Put(cFull)
      else if vTop then
        Put(cUpper)
      else if vBottom then
        Put(cLower)
      else
        Put([Ord(' ')]);
    end;
    Put([13, 10]);
    Inc(vY, 2);
  end;
  SetLength(Result, vLen);
  if vLen > 0 then
    Move(vOut[0], Pointer(Result)^, vLen);
end;

end.
