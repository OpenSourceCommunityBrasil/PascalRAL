unit RALSHA1;

interface

uses
  Classes, SysUtils,
  RALHashes, RALTypes, RALTools, RALBase64;

type

  { TRALSHA1 }

  TRALSHA1 = class(TRALHashes)
  private
    FHash : array[0..4] of Cardinal;
    FBuffer : array[0..63] of Byte;
    FHashSize : Byte;
  protected
    function Swap(AValue : Cardinal) : Cardinal;

    function GetBufLength : IntegerRAL; override;
    function GetBuffer(AIndex : IntegerRAL) : Pointer; override;

    procedure Compress; override;
    procedure Initialize; override;

    function Finalize : TBytes; override;
  public
    constructor Create;
  end;

implementation

{ TRALSHA1 }

function TRALSHA1.Swap(AValue : Cardinal) : Cardinal;
begin
  Result:= ((AValue and $FF) shl 24) or
           ((AValue and $FF00) shl 8) or
           ((AValue and $FF0000) shr 8) or
           ((AValue and $FF000000) shr 24);
end;

function TRALSHA1.GetBufLength : IntegerRAL;
begin
  Result := 64;
end;

function TRALSHA1.GetBuffer(AIndex : IntegerRAL) : Pointer;
begin
  Result := Pointer(@FBuffer[AIndex]);
end;

procedure TRALSHA1.Compress;
const
  K : array[0..3] of Cardinal = ($5A827999,$6ED9EBA1,$8F1BBCDC,$CA62C1D6);
var
  A, B, C, D, E: Cardinal;
  W: array[0..79] of Cardinal;
  i: Integer;

  procedure F1(x1,x2,x3 : Cardinal;var x4 : Cardinal; var x5 : Cardinal);
  begin
    case i of
      00..19 : Inc(x4,((x1 shl 5) or (x1 shr 27)) + (x2 xor (x5 and (x3 xor x2))) + K[0] + W[i]);
      20..39 : Inc(x4,((x1 shl 5) or (x1 shr 27)) + (x5 xor x3 xor x2) + K[1] + W[i]);
      40..59 : Inc(x4,((x1 shl 5) or (x1 shr 27)) + ((x5 and x3) or (x2 and (x5 or x3))) + K[2]+ W[i]);
      60..79 : Inc(x4,((x1 shl 5) or (x1 shr 27)) + (x5 xor x3 xor x2) + K[3] + W[i]);
    end;
    x5 := (x5 shl 30) or (x5 shr 2);
  end;
begin
  FillChar(W, SizeOf(W), 0);
  Move(FBuffer,W,GetBufLength);
  for i := 0 to 15 do
    W[i] := Swap(W[i]);

  for i := 16 to 79 do
    W[i] := ((W[i-3] xor W[i-8] xor W[i-14] xor W[i-16]) shl 1) or
            ((W[i-3] xor W[i-8] xor W[i-14] xor W[i-16]) shr 31);

  A := FHash[0];
  B := FHash[1];
  C := FHash[2];
  D := FHash[3];
  E := FHash[4];

  for i := 0 to 79 do
  begin
    case i mod 5 of
      0 : F1(A,D,C,E,B);
      1 : F1(E,C,B,D,A);
      2 : F1(D,B,A,C,E);
      3 : F1(C,A,E,B,D);
      4 : F1(B,E,D,A,C);
    end;
  end;

  FHash[0]:= FHash[0] + A;
  FHash[1]:= FHash[1] + B;
  FHash[2]:= FHash[2] + C;
  FHash[3]:= FHash[3] + D;
  FHash[4]:= FHash[4] + E;

  FillChar(FBuffer, Sizeof(FBuffer),0);

  inherited Compress;
end;

procedure TRALSHA1.Initialize;
begin
  FHash[0] := $67452301;
  FHash[1] := $EFCDAB89;
  FHash[2] := $98BADCFE;
  FHash[3] := $10325476;
  FHash[4] := $C3D2E1F0;

  FHashSize := 20;
  FillChar(FBuffer, Sizeof(FBuffer),0);
  inherited Initialize;
end;

function TRALSHA1.Finalize : TBytes;
var
  vIndex : IntegerRAL;
  vLenBit : UInt64;
begin
  vIndex := GetIndex;
  vLenBit := GetLenBit;

  FBuffer[vIndex]:= $80;
  if vIndex >= 56 then
    Compress;

  PCardinal(@FBuffer[56])^:= 0;
  PCardinal(@FBuffer[60])^:= Swap(Cardinal(vLenBit));
  Compress;

  FHash[0] := Swap(FHash[0]);
  FHash[1] := Swap(FHash[1]);
  FHash[2] := Swap(FHash[2]);
  FHash[3] := Swap(FHash[3]);
  FHash[4] := Swap(FHash[4]);

  SetLength(Result, FHashSize);
  Move(FHash,Result[0], FHashSize);
end;

constructor TRALSHA1.Create;
begin
  Initialize;
end;

end.

