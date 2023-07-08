unit RALSHA2_64;

interface

uses
  Classes, SysUtils,
  RALHashes, RALTypes, RALTools, RALBase64;

type
  TRALSHA64Versions = (rsv384,rsv512,rsv512_224,rsv512_256);

  TRALSHA2_64 = class(TRALHashes)
  private
    FVersion : TRALSHA64Versions;
    FHash : array[0..7] of UInt64;
    FBufLength : Byte;
    FBuffer : array[0..127] of Byte;
    FIndex : IntegerRAL;
    FHashSize : Byte;
    FLenHi : UInt64;
    FLenLo : UInt64;
    procedure SetVersion(const Value: TRALSHA64Versions);
  protected
    procedure HashSHA2(AData : PByte; ALength : IntegerRAL);
    procedure Compress;
    procedure Initialize;

    function Swap(AValue : UInt64) : UInt64;
    function Finalize : TBytes;

    procedure UpdateBuffer(AValue: TStream); overload;
    procedure UpdateBuffer(AValue: StringRAL); overload;
    procedure UpdateBuffer(AValue: TBytes); overload;

    function HMACAsDigest(AValue: TStream; AKey: TBytes): TBytes;

    function GetDigest(AValue: TStream) : TBytes; overload;
    function GetDigest(AValue: StringRAL) : TBytes; overload;
    function GetDigest(AValue: TBytes) : TBytes; overload;
  public
    constructor Create;

    function HashAsString(AValue: StringRAL): StringRAL; overload;
    function HashAsString(AValue: TStream): StringRAL; overload;

    function HashAsStream(AValue: TStream): TStringStream;

    function HMACAsString(AValue, AKey: StringRAL): StringRAL; overload;
    function HMACAsString(AValue : TStream; AKey: StringRAL): StringRAL; overload;
    function HMACAsString(AValue : TBytes; AKey: StringRAL): StringRAL; overload;
  published
    property Version : TRALSHA64Versions read FVersion write SetVersion;
  end;

implementation

const
  K : array[0..79] of UInt64 = (
   $428a2f98d728ae22, $7137449123ef65cd, $b5c0fbcfec4d3b2f, $e9b5dba58189dbbc,
   $3956c25bf348b538, $59f111f1b605d019, $923f82a4af194f9b, $ab1c5ed5da6d8118,
   $d807aa98a3030242, $12835b0145706fbe, $243185be4ee4b28c, $550c7dc3d5ffb4e2,
   $72be5d74f27b896f, $80deb1fe3b1696b1, $9bdc06a725c71235, $c19bf174cf692694,
   $e49b69c19ef14ad2, $efbe4786384f25e3, $0fc19dc68b8cd5b5, $240ca1cc77ac9c65,
   $2de92c6f592b0275, $4a7484aa6ea6e483, $5cb0a9dcbd41fbd4, $76f988da831153b5,
   $983e5152ee66dfab, $a831c66d2db43210, $b00327c898fb213f, $bf597fc7beef0ee4,
   $c6e00bf33da88fc2, $d5a79147930aa725, $06ca6351e003826f, $142929670a0e6e70,
   $27b70a8546d22ffc, $2e1b21385c26c926, $4d2c6dfc5ac42aed, $53380d139d95b3df,
   $650a73548baf63de, $766a0abb3c77b2a8, $81c2c92e47edaee6, $92722c851482353b,
   $a2bfe8a14cf10364, $a81a664bbc423001, $c24b8b70d0f89791, $c76c51a30654be30,
   $d192e819d6ef5218, $d69906245565a910, $f40e35855771202a, $106aa07032bbd1b8,
   $19a4c116b8d2d0c8, $1e376c085141ab53, $2748774cdf8eeb99, $34b0bcb5e19b48a8,
   $391c0cb3c5c95a63, $4ed8aa4ae3418acb, $5b9cca4f7763e373, $682e6ff3d6b2b8a3,
   $748f82ee5defb2fc, $78a5636f43172f60, $84c87814a1f0ab72, $8cc702081a6439ec,
   $90befffa23631e28, $a4506cebde82bde9, $bef9a3f7b2c67915, $c67178f2e372532b,
   $ca273eceea26619c, $d186b8c721c0c207, $eada7dd6cde0eb1e, $f57d4f7fee6ed178,
   $06f067aa72176fba, $0a637dc5a2c898a6, $113f9804bef90dae, $1b710b35131c471b,
   $28db77f523047d84, $32caab7b40c72493, $3c9ebe0a15c9bebc, $431d67c49c100d4c,
   $4cc5d4becb3e42b6, $597f299cfc657e2a, $5fcb6fab3ad6faec, $6c44198c4a475817);


{ TRALSHA2_64 }

procedure TRALSHA2_64.Compress;
var
  s0, s1, m0, c0, t1, t2: UInt64;
  a, b, c, d, e, f, g, h : UInt64;
  W: array[0..79] of UInt64;
  I: IntegerRAL;
begin
  FillChar(W, SizeOf(W), 0);
  a:= FHash[0];
  b:= FHash[1];
  c:= FHash[2];
  d:= FHash[3];
  e:= FHash[4];
  f:= FHash[5];
  g:= FHash[6];
  h:= FHash[7];

  Move(FBuffer,W,Sizeof(FBufLength));

  for i:= 0 to 15 do
    W[i]:= Swap(W[i]);


  for i:= 16 to 79 do
  begin
    s0 := ((W[i-15] shr 1) or (W[i-15] shl 63)) xor
          ((W[i-15] shr 8) or (W[i-15] shl 56)) xor
          (W[i-15] shr 7);
    s1 := ((W[i-2] shr 19) or (W[i-2] shl 45)) xor
          ((W[i-2] shr 61) or (W[i-2] shl 3)) xor
          (W[i-2] shr 6);
    W[I] := W[I - 16] + s0 + W[I - 7] + s1;
  end;

  for i:= 0 to 79 do
  begin
    s1 := ((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor
          ((e shr 41) or (e shl 23));
    c0 := (e and f) xor (not e and g);
    t1:= h + s1 + c0 + K[i] + W[i];

    s0 := ((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor
          ((a shr 39) or (a shl 25));
    m0 := (a and b) xor (a and c) xor (b and c);
    t2 := s0 + m0;

    h:= g;
    g:= f;
    f:= e;
    e:= d + t1;
    d:= c;
    c:= b;
    b:= a;
    a:= t1 + t2;
  end;

  FHash[0]:= FHash[0] + a;
  FHash[1]:= FHash[1] + b;
  FHash[2]:= FHash[2] + c;
  FHash[3]:= FHash[3] + d;
  FHash[4]:= FHash[4] + e;
  FHash[5]:= FHash[5] + f;
  FHash[6]:= FHash[6] + g;
  FHash[7]:= FHash[7] + h;

  FillChar(FBuffer,Sizeof(FBuffer),0);
  FIndex := 0;
end;

constructor TRALSHA2_64.Create;
begin
  FBufLength := 128;
  SetVersion(rsv512);
end;

function TRALSHA2_64.Finalize: TBytes;
begin
  FBuffer[FIndex]:= $80;
  if FIndex >= 112 then
    Compress;

  PUInt64(@FBuffer[120])^:= Swap(FLenHi);
  PUInt64(@FBuffer[124])^:= Swap(FLenLo);
  Compress;

  FHash[0]:= Swap(FHash[0]);
  FHash[1]:= Swap(FHash[1]);
  FHash[2]:= Swap(FHash[2]);
  FHash[3]:= Swap(FHash[3]);
  FHash[4]:= Swap(FHash[4]);
  FHash[5]:= Swap(FHash[5]);
  FHash[6]:= Swap(FHash[6]);
  FHash[7]:= Swap(FHash[7]);

  SetLength(Result,FHashSize);
  Move(FHash,Result[0],FHashSize);
end;

function TRALSHA2_64.GetDigest(AValue: TBytes): TBytes;
begin
  Initialize;
  UpdateBuffer(AValue);
  Result := Finalize;
end;

function TRALSHA2_64.GetDigest(AValue: StringRAL): TBytes;
begin
  Initialize;
  UpdateBuffer(AValue);
  Result := Finalize;
end;

function TRALSHA2_64.GetDigest(AValue: TStream): TBytes;
begin
  Initialize;
  UpdateBuffer(AValue);
  Result := Finalize;
end;

procedure TRALSHA2_64.HashSHA2(AData: PByte; ALength: IntegerRAL);
var
  vBufSize : Integer;
begin
  Inc(FLenLo,ALength*8);

  while ALength > 0 do
  begin
    vBufSize := FBufLength - FIndex;
    if ALength < vBufSize then
      vBufSize := ALength;

    Move(AData^,FBuffer[FIndex],vBufSize);
    Inc(AData,vBufSize);

    if vBufSize + FIndex = FBufLength then
      Compress
    else
      FIndex := vBufSize;

    Dec(ALength,vBufSize);
  end;
end;

function TRALSHA2_64.HashAsStream(AValue: TStream): TStringStream;
var
  vDigest : TBytes;
  vResult : StringRAL;
begin
  Initialize;
  UpdateBuffer(AValue);
  vDigest := Finalize;

  case OutputType of
    rhotHex    : vResult := DigestToHex(vDigest);
    rhotBase64 : vResult := DigestToBase64(vDigest);
  end;

  Result := TStringStream.Create(vResult);
  Result.Position := 0;
end;

function TRALSHA2_64.HashAsString(AValue: StringRAL): StringRAL;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := HashAsString(vStream);
  finally
    vStream.Free;
  end;
end;

function TRALSHA2_64.HashAsString(AValue: TStream): StringRAL;
var
  vResult : TStringStream;
begin
  vResult := HashAsStream(AValue);
  try
    Result := vResult.DataString
  finally
    vResult.Free;
  end;
end;

function TRALSHA2_64.HMACAsDigest(AValue: TStream; AKey: TBytes): TBytes;
var
  vKey : TBytes;
  vTemp1 : TBytes;
  vTemp2 : TBytes;
  vInt : integer;
begin
  if Length(AKey) > FBufLength then
    vKey := GetDigest(AKey)
  else
    vKey := AKey;

  SetLength(vKey, FBufLength);
  SetLength(vTemp1, FBufLength);
  for vInt := Low(vKey) to High(vKey) do
    vTemp1[vInt] := vKey[vInt] xor $36;

  Initialize;
  UpdateBuffer(vTemp1);
  UpdateBuffer(AValue);
  vTemp2 := Finalize;

  SetLength(vTemp1, FBufLength);
  for vInt := Low(vKey) to High(vKey) do
    vTemp1[vInt] := vKey[vInt] xor $5C;

  Initialize;
  UpdateBuffer(vTemp1);
  UpdateBuffer(vTemp2);
  Result := Finalize;
end;

function TRALSHA2_64.HMACAsString(AValue, AKey: StringRAL): StringRAL;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := HMACAsString(vStream,AKey);
  finally
    vStream.Free;
  end;
end;

function TRALSHA2_64.HMACAsString(AValue: TBytes; AKey: StringRAL): StringRAL;
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    Result := HMACAsString(vStream,AKey);
  finally
    vStream.Free;
  end;
end;

function TRALSHA2_64.HMACAsString(AValue: TStream; AKey: StringRAL): StringRAL;
var
  vKey, vDigest : TBytes;
begin
  vKey := VarToBytes(AKey);
  vDigest := HMACAsDigest(AValue,vKey);

  case OutputType of
    rhotHex    : Result := DigestToHex(vDigest);
    rhotBase64 : Result := DigestToBase64(vDigest);
  end;
end;

procedure TRALSHA2_64.Initialize;
begin
  case FVersion of
    rsv384: begin
      FHash[0]:= $cbbb9d5dc1059ed8;
      FHash[1]:= $629a292a367cd507;
      FHash[2]:= $9159015a3070dd17;
      FHash[3]:= $152fecd8f70e5939;
      FHash[4]:= $67332667ffc00b31;
      FHash[5]:= $8eb44a8768581511;
      FHash[6]:= $db0c2e0d64f98fa7;
      FHash[7]:= $47b5481dbefa4fa4;

      FHashSize := 48;
    end;
    rsv512: begin
      FHash[0]:= $6a09e667f3bcc908;
      FHash[1]:= $bb67ae8584caa73b;
      FHash[2]:= $3c6ef372fe94f82b;
      FHash[3]:= $a54ff53a5f1d36f1;
      FHash[4]:= $510e527fade682d1;
      FHash[5]:= $9b05688c2b3e6c1f;
      FHash[6]:= $1f83d9abfb41bd6b;
      FHash[7]:= $5be0cd19137e2179;

      FHashSize := 64;
    end;
    rsv512_224: begin
      FHash[0]:= $8C3D37C819544DA2;
      FHash[1]:= $73E1996689DCD4D6;
      FHash[2]:= $1DFAB7AE32FF9C82;
      FHash[3]:= $679DD514582F9FCF;
      FHash[4]:= $0F6D2B697BD44DA8;
      FHash[5]:= $77E36F7304C48942;
      FHash[6]:= $3F9D85A86A1D36C8;
      FHash[7]:= $1112E6AD91D692A1;

      FHashSize := 28;
    end;
    rsv512_256: begin
      FHash[0]:= $22312194FC2BF72C;
      FHash[1]:= $9F555FA3C84C64C2;
      FHash[2]:= $2393B86B6F53B151;
      FHash[3]:= $963877195940EABD;
      FHash[4]:= $96283EE2A88EFFE3;
      FHash[5]:= $BE5E1E2553863992;
      FHash[6]:= $2B0199FC2C85B8AA;
      FHash[7]:= $0EB72DDC81C52CA2;

      FHashSize := 32;
    end;
  end;

  FillChar(FBuffer,Sizeof(FBuffer),0);
  FIndex := 0;
  FLenHi := 0;
  FLenLo := 0;
end;

procedure TRALSHA2_64.SetVersion(const Value: TRALSHA64Versions);
begin
  FVersion := Value;
  Initialize;
end;

function TRALSHA2_64.Swap(AValue: UInt64): UInt64;
begin
  Result:= ((AValue and $FF) shl 56) or ((AValue and $FF00) shl 40) or
           ((AValue and $FF0000) shl 24) or ((AValue and $FF000000) shl 8) or
           ((AValue and $FF00000000) shr 8) or
           ((AValue and $FF0000000000) shr 24) or
           ((AValue and $FF000000000000) shr 40) or
           ((AValue and $FF00000000000000) shr 56);
end;

procedure TRALSHA2_64.UpdateBuffer(AValue: TStream);
var
  vInBuf: array[0..4095] of Byte;
  vBytesRead : IntegerRAL;
  vPosition, vSize : Int64RAL;
begin
  AValue.Position := 0;
  vPosition := 0;
  vSize := AValue.Size;

  while vPosition < vSize do
  begin
    vBytesRead := AValue.Read(vInBuf[0], Length(vInBuf));
    HashSHA2(@vInBuf[0], vBytesRead);

    vPosition := vPosition + vBytesRead;
  end;
end;

procedure TRALSHA2_64.UpdateBuffer(AValue: StringRAL);
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    UpdateBuffer(vStream);
  finally
    vStream.Free;
  end;
end;

procedure TRALSHA2_64.UpdateBuffer(AValue: TBytes);
var
  vStream : TStringStream;
begin
  vStream := TStringStream.Create(AValue);
  try
    UpdateBuffer(vStream);
  finally
    vStream.Free;
  end;
end;

end.
