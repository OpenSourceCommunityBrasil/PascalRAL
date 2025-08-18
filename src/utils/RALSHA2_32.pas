/// Unit for SHA2 Hash - 32 bits sources
unit RALSHA2_32;

interface

uses
  Classes, SysUtils,
  RALHashBase, RALTypes, RALTools, RALBase64;

type
  TRALSHA32Versions = (rsv224, rsv256);

  TRALSHA2_32 = class(TRALHashBase)
  private
    FBuffer: array[0..63] of byte;
    FHash: array[0..7] of cardinal;
    FHashSize: byte;
    FVersion: TRALSHA32Versions;
  protected
    procedure Compress; override;
    function Finalize: TBytes; override;
    function GetBuffer(AIndex: IntegerRAL): Pointer; override;
    function GetBufLength: IntegerRAL; override;
    procedure Initialize; override;
    procedure SetVersion(const Value: TRALSHA32Versions);
    /// returns swaps bits of a value
    function Swap(AValue: cardinal): cardinal;
  public
    constructor Create;
  published
    property Version: TRALSHA32Versions read FVersion write SetVersion;
  end;

implementation

const
  K: array[0..63] of cardinal = (
    $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1,
    $923f82a4, $ab1c5ed5, $d807aa98, $12835b01, $243185be, $550c7dc3,
    $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174, $e49b69c1, $efbe4786,
    $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
    $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147,
    $06ca6351, $14292967, $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13,
    $650a7354, $766a0abb, $81c2c92e, $92722c85, $a2bfe8a1, $a81a664b,
    $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
    $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a,
    $5b9cca4f, $682e6ff3, $748f82ee, $78a5636f, $84c87814, $8cc70208,
    $90befffa, $a4506ceb, $bef9a3f7, $c67178f2);


  { TRALSHA2_32 }

procedure TRALSHA2_32.Compress;
var
  i: integer;
  s0, s1, m0, c0, t1, t2: cardinal;
  a, b, c, d, e, f, g, h: cardinal;
  W: array[0..63] of cardinal;
begin
  a := FHash[0];
  b := FHash[1];
  c := FHash[2];
  d := FHash[3];
  e := FHash[4];
  f := FHash[5];
  g := FHash[6];
  h := FHash[7];

  Move(FBuffer, W, GetBufLength);

  for i := 0 to 15 do
    W[i] := Swap(W[i]);

  for i := 16 to 63 do
  begin
    s0 := ((W[i - 15] shr 7) or (W[i - 15] shl 25))
      xor ((W[i - 15] shr 18) or (W[i - 15] shl 14)) 
      xor (W[i - 15] shr 3);

    s1 := ((W[i - 2] shr 17) or (W[i - 2] shl 15)) 
      xor ((W[i - 2] shr 19) or (W[i - 2] shl 13)) 
      xor (W[i - 2] shr 10);
    {$Q-}
    W[I] := W[I - 16] + s0 + W[I - 7] + s1;
    {$Q+}
  end;

  for i := 0 to 63 do
  begin
    s0 := ((a shr 2) or (a shl 30)) 
      xor ((a shr 13) or (a shl 19))
      xor ((a shr 22) xor (a shl 10));
    s1 := ((e shr 6) or (e shl 26)) 
      xor ((e shr 11) or (e shl 21))
      xor ((e shr 25) or (e shl 7));
    m0 := (a and b) xor (a and c) xor (b and c);
    c0 := (e and f) xor (not e and g);
    {$Q-}
    t1 := h + s1 + c0 + K[i] + W[i];
    t2 := s0 + m0;

    h := g;
    g := f;
    f := e;
    e := d + t1;
    d := c;
    c := b;
    b := a;
    a := t1 + t2;
    {$Q+}
  end;

  {$Q-}
  FHash[0] := FHash[0] + a;
  FHash[1] := FHash[1] + b;
  FHash[2] := FHash[2] + c;
  FHash[3] := FHash[3] + d;
  FHash[4] := FHash[4] + e;
  FHash[5] := FHash[5] + f;
  FHash[6] := FHash[6] + g;
  FHash[7] := FHash[7] + h;
  {$Q+}

  FillChar(FBuffer, Sizeof(FBuffer), 0);
  inherited;
end;

constructor TRALSHA2_32.Create;
begin
  SetVersion(rsv256);
end;

function TRALSHA2_32.Finalize: TBytes;
var
  vIndex: IntegerRAL;
  vLenBit: UInt64RAL;
begin
  vIndex := GetIndex;
  vLenBit := GetLenBit;

  FBuffer[vIndex] := $80;
  if vIndex >= 56 then
    Compress;

  PCardinal(@FBuffer[56])^ := 0;
  PCardinal(@FBuffer[60])^ := Swap(cardinal(vLenBit));
  Compress;

  FHash[0] := Swap(FHash[0]);
  FHash[1] := Swap(FHash[1]);
  FHash[2] := Swap(FHash[2]);
  FHash[3] := Swap(FHash[3]);
  FHash[4] := Swap(FHash[4]);
  FHash[5] := Swap(FHash[5]);
  FHash[6] := Swap(FHash[6]);
  FHash[7] := Swap(FHash[7]);

  SetLength(Result, FHashSize);
  Move(FHash, Result[0], FHashSize);

  inherited Finalize;
end;

function TRALSHA2_32.GetBuffer(AIndex: IntegerRAL): Pointer;
begin
  Result := Pointer(@FBuffer[AIndex]);
end;

function TRALSHA2_32.GetBufLength: IntegerRAL;
begin
  Result := 64;
end;

procedure TRALSHA2_32.Initialize;
begin
  case FVersion of
    rsv224: begin
      FHash[0] := $c1059ed8;
      FHash[1] := $367cd507;
      FHash[2] := $3070dd17;
      FHash[3] := $f70e5939;
      FHash[4] := $ffc00b31;
      FHash[5] := $68581511;
      FHash[6] := $64f98fa7;
      FHash[7] := $befa4fa4;

      FHashSize := 28;
    end;
    rsv256: begin
      FHash[0] := $6a09e667;
      FHash[1] := $bb67ae85;
      FHash[2] := $3c6ef372;
      FHash[3] := $a54ff53a;
      FHash[4] := $510e527f;
      FHash[5] := $9b05688c;
      FHash[6] := $1f83d9ab;
      FHash[7] := $5be0cd19;

      FHashSize := 32;
    end;
  end;

  FillChar(FBuffer, Sizeof(FBuffer), 0);
  inherited;
end;

procedure TRALSHA2_32.SetVersion(const Value: TRALSHA32Versions);
begin
  FVersion := Value;
  Initialize;
end;

function TRALSHA2_32.Swap(AValue: cardinal): cardinal;
begin
  Result := ((AValue and $FF) shl 24) 
         or ((AValue and $FF00) shl 8) 
         or ((AValue and $FF0000) shr 8) 
         or ((AValue and $FF000000) shr 24);
end;

end.
