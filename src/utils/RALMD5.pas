unit RALMD5;

interface

uses
  Classes, SysUtils,
  RALHashes, RALTypes;

type
  TRALMD5 = class(TRALHashes)
  private
    FHash : array[0..3] of Cardinal;
    FBuffer : array[0..63] of Byte;
    FHashSize : Byte;
  protected
    function GetBufLength : IntegerRAL; override;
    function GetBuffer(AIndex : IntegerRAL) : Pointer; override;

    function LRot32(AValue1, AValue2: Cardinal): Cardinal;

    procedure Initialize; override;
    procedure Compress; override;

    function Finalize : TBytes; override;
  public
    constructor Create;
  end;

implementation

{ TRALMD5 }

procedure TRALMD5.Compress;
var
  W : array[0..15] of Cardinal;
  A, B, C, D, Z: Cardinal;

  function R1(x, y, z: Cardinal): Cardinal;
  begin
    Result := (x and y) or ((not x) and z);
  end;

  function R2(x, y, z: Cardinal): Cardinal;
  begin
    Result := (x and z) or (y and (not z));
  end;

  function R3(x, y, z: Cardinal): Cardinal;
  begin
    Result := x xor y xor z;
  end;

  function R4(x, y, z: Cardinal): Cardinal;
  begin
    Result := y xor (x or (not z));
  end;
begin
  FillChar(W, SizeOf(W), 0);

  Move(FBuffer,W,SizeOf(W));
  A:= FHash[0];
  B:= FHash[1];
  C:= FHash[2];
  D:= FHash[3];

  A:= B + LRot32(A + R1(B,C,D) + W[00] + $d76aa478,7);
  D:= A + LRot32(D + R1(A,B,C) + W[01] + $e8c7b756,12);
  C:= D + LRot32(C + R1(D,A,B) + W[02] + $242070db,17);
  B:= C + LRot32(B + R1(C,D,A) + W[03] + $c1bdceee,22);
  A:= B + LRot32(A + R1(B,C,D) + W[04] + $f57c0faf,7);
  D:= A + LRot32(D + R1(A,B,C) + W[05] + $4787c62a,12);
  C:= D + LRot32(C + R1(D,A,B) + W[06] + $a8304613,17);
  B:= C + LRot32(B + R1(C,D,A) + W[07] + $fd469501,22);
  A:= B + LRot32(A + R1(B,C,D) + W[08] + $698098d8,7);
  D:= A + LRot32(D + R1(A,B,C) + W[09] + $8b44f7af,12);
  C:= D + LRot32(C + R1(D,A,B) + W[10] + $ffff5bb1,17);
  B:= C + LRot32(B + R1(C,D,A) + W[11] + $895cd7be,22);
  A:= B + LRot32(A + R1(B,C,D) + W[12] + $6b901122,7);
  D:= A + LRot32(D + R1(A,B,C) + W[13] + $fd987193,12);
  C:= D + LRot32(C + R1(D,A,B) + W[14] + $a679438e,17);
  B:= C + LRot32(B + R1(C,D,A) + W[15] + $49b40821,22);

  A:= B + LRot32(A + R2(B,C,D) + W[01] + $f61e2562,5);
  D:= A + LRot32(D + R2(A,B,C) + W[06] + $c040b340,9);
  C:= D + LRot32(C + R2(D,A,B) + W[11] + $265e5a51,14);
  B:= C + LRot32(B + R2(C,D,A) + W[00] + $e9b6c7aa,20);
  A:= B + LRot32(A + R2(B,C,D) + W[05] + $d62f105d,5);
  D:= A + LRot32(D + R2(A,B,C) + W[10] + $02441453,9);
  C:= D + LRot32(C + R2(D,A,B) + W[15] + $d8a1e681,14);
  B:= C + LRot32(B + R2(C,D,A) + W[04] + $e7d3fbc8,20);
  A:= B + LRot32(A + R2(B,C,D) + W[09] + $21e1cde6,5);
  D:= A + LRot32(D + R2(A,B,C) + W[14] + $c33707d6,9);
  C:= D + LRot32(C + R2(D,A,B) + W[03] + $f4d50d87,14);
  B:= C + LRot32(B + R2(C,D,A) + W[08] + $455a14ed,20);
  A:= B + LRot32(A + R2(B,C,D) + W[13] + $a9e3e905,5);
  D:= A + LRot32(D + R2(A,B,C) + W[02] + $fcefa3f8,9);
  C:= D + LRot32(C + R2(D,A,B) + W[07] + $676f02d9,14);
  B:= C + LRot32(B + R2(C,D,A) + W[12] + $8d2a4c8a,20);

  A:= B + LRot32(A + R3(B,C,D) + W[05] + $fffa3942,4);
  D:= A + LRot32(D + R3(A,B,C) + W[08] + $8771f681,11);
  C:= D + LRot32(C + R3(D,A,B) + W[11] + $6d9d6122,16);
  B:= C + LRot32(B + R3(C,D,A) + W[14] + $fde5380c,23);
  A:= B + LRot32(A + R3(B,C,D) + W[01] + $a4beea44,4);
  D:= A + LRot32(D + R3(A,B,C) + W[04] + $4bdecfa9,11);
  C:= D + LRot32(C + R3(D,A,B) + W[07] + $f6bb4b60,16);
  B:= C + LRot32(B + R3(C,D,A) + W[10] + $bebfbc70,23);
  A:= B + LRot32(A + R3(B,C,D) + W[13] + $289b7ec6,4);
  D:= A + LRot32(D + R3(A,B,C) + W[00] + $eaa127fa,11);
  C:= D + LRot32(C + R3(D,A,B) + W[03] + $d4ef3085,16);
  B:= C + LRot32(B + R3(C,D,A) + W[06] + $04881d05,23);
  A:= B + LRot32(A + R3(B,C,D) + W[09] + $d9d4d039,4);
  D:= A + LRot32(D + R3(A,B,C) + W[12] + $e6db99e5,11);
  C:= D + LRot32(C + R3(D,A,B) + W[15] + $1fa27cf8,16);
  B:= C + LRot32(B + R3(C,D,A) + W[02] + $c4ac5665,23);

  A:= B + LRot32(A + R4(B,C,D) + W[00] + $f4292244,6);
  D:= A + LRot32(D + R4(A,B,C) + W[07] + $432aff97,10);
  C:= D + LRot32(C + R4(D,A,B) + W[14] + $ab9423a7,15);
  B:= C + LRot32(B + R4(C,D,A) + W[05] + $fc93a039,21);
  A:= B + LRot32(A + R4(B,C,D) + W[12] + $655b59c3,6);
  D:= A + LRot32(D + R4(A,B,C) + W[03] + $8f0ccc92,10);
  C:= D + LRot32(C + R4(D,A,B) + W[10] + $ffeff47d,15);
  B:= C + LRot32(B + R4(C,D,A) + W[01] + $85845dd1,21);
  A:= B + LRot32(A + R4(B,C,D) + W[08] + $6fa87e4f,6);
  D:= A + LRot32(D + R4(A,B,C) + W[15] + $fe2ce6e0,10);
  C:= D + LRot32(C + R4(D,A,B) + W[06] + $a3014314,15);
  B:= C + LRot32(B + R4(C,D,A) + W[13] + $4e0811a1,21);
  A:= B + LRot32(A + R4(B,C,D) + W[04] + $f7537e82,6);
  D:= A + LRot32(D + R4(A,B,C) + W[11] + $bd3af235,10);
  C:= D + LRot32(C + R4(D,A,B) + W[02] + $2ad7d2bb,15);
  B:= C + LRot32(B + R4(C,D,A) + W[09] + $eb86d391,21);

  FHash[0] := FHash[0] + A;
  FHash[1] := FHash[1] + B;
  FHash[2] := FHash[2] + C;
  FHash[3] := FHash[3] + D;

  FillChar(FBuffer,Sizeof(FBuffer),0);
end;

constructor TRALMD5.Create;
begin
  FHashSize := 64;
  Initialize;
end;

function TRALMD5.Finalize: TBytes;
var
  vIndex : IntegerRAL;
  vLenBit : UInt64;
begin
  vIndex := GetIndex;
  vLenBit := GetLenBit;

  FBuffer[vIndex]:= $80;
  if vIndex >= 56 then
    Compress;

  PCardinal(@FBuffer[56])^:= Cardinal(vLenBit);
  Compress;

  SetLength(Result,FHashSize);
  Move(FHash,Result[0],FHashSize);
end;

function TRALMD5.GetBuffer(AIndex: IntegerRAL): Pointer;
begin
  Result := Pointer(@FBuffer[AIndex]);
end;

function TRALMD5.GetBufLength: IntegerRAL;
begin
  Result := 64;
end;

procedure TRALMD5.Initialize;
begin
  FHash[0]:= $67452301;
  FHash[1]:= $efcdab89;
  FHash[2]:= $98badcfe;
  FHash[3]:= $10325476;

  FillChar(FBuffer,Sizeof(FBuffer),0);
  FHashSize := 16;
  inherited;
end;

function TRALMD5.LRot32(AValue1, AValue2 : Cardinal): Cardinal;
begin
 Result:= (AValue1 shl AValue2) or (AValue1 shr (32-AValue2));
end;

end.
