unit RALSHA2_256;

interface

uses
  Classes, SysUtils,
  RALHashes, RALTypes, RALTools, RALBase64;

type
  TRALSHA2_256 = class(TRALHashes)
  private
    FHash : array[0..7] of Cardinal;
    FBufLength : Byte;
    FBuffer : array[0..63] of Byte;
    FIndex : IntegerRAL;
    FLenHi : Cardinal;
    FLenLo : Cardinal;
  protected
    procedure Hash256(AData : PByte; ALength : IntegerRAL);
    procedure Compress;
    procedure Initialize;

    function DigestToHex(AValue : TBytes) : StringRAL;
    function DigestToBase64(AValue : TBytes) : StringRAL;

    function Swap(AValue : Cardinal) : Cardinal;
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
  end;

implementation

const
  K : array[0..63] of Cardinal = (
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


{ TRALSHA2_256 }

function TRALSHA2_256.DigestToBase64(AValue: TBytes): StringRAL;
begin
  Result := TRALBase64.Encode(AValue);
  if (Result <> '') and (Result[Length(Result)] = '=') then
    Delete(Result,Length(Result),1);
end;

function TRALSHA2_256.DigestToHex(AValue: TBytes): StringRAL;
const
  HexChar : array[0..15] of CharRAL = ('0','1','2','3','4','5','6','7','8','9',
                                       'a','b','c','d','e','f');
var
  vInt : IntegerRAL;
begin
  vInt := 0;
  while vInt < Length(AValue) do
  begin
    Result := Result + HexChar[(AValue[vInt] shr 4) and $0f];
    Result := Result + HexChar[(AValue[vInt] and $0f)];
    Inc(vInt);
  end;
end;

procedure TRALSHA2_256.Compress;
var
  I: Integer;
  s0, s1, m0, c0, t1, t2: Cardinal;
  w: array[0..63] of Cardinal;
  a, b, c, d, e, f, g, h: Cardinal;
begin
  a := FHash[0];
  b := FHash[1];
  c := FHash[2];
  d := FHash[3];
  e := FHash[4];
  f := FHash[5];
  g := FHash[6];
  h := FHash[7];

  Move(FBuffer, W, FBufLength);

  for I := 0 to 15 do
    W[I] := Swap(W[I]);

  for i:= 16 to 63 do
  begin
    s0:= ((W[i-15] shr 7) or (W[i-15] shl 25)) xor
         ((W[i-15] shr 18) or (W[i-15] shl 14)) xor
         (W[i-15] shr 3);

    s1 := ((W[i-2] shr 17) or (W[i-2] shl 15)) xor
          ((W[i-2] shr 19) or (W[i-2] shl 13)) xor
          (W[i-2] shr 10);

    W[I] := W[I - 16] + s0 + W[I - 7] + s1;
  end;

  for i:= 0 to 63 do
  begin
    s0 := ((a shr 2) or (a shl 30)) xor
          ((a shr 13) or (a shl 19)) xor
          ((a shr 22) xor (a shl 10));
    s1 := ((e shr 6) or (e shl 26)) xor
          ((e shr 11) or (e shl 21)) xor
          ((e shr 25) or (e shl 7));
    m0 := (a and b) xor (a and c) xor (b and c);
    c0 := (e and f) xor (not e and g);
    t1 := h + s1 + c0 + K[I] + W[I];
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

  FHash[0] := FHash[0] + a;
  FHash[1] := FHash[1] + b;
  FHash[2] := FHash[2] + c;
  FHash[3] := FHash[3] + d;
  FHash[4] := FHash[4] + e;
  FHash[5] := FHash[5] + f;
  FHash[6] := FHash[6] + g;
  FHash[7] := FHash[7] + h;

  FillChar(FBuffer,Sizeof(FBuffer),0);
  FIndex := 0;
end;

constructor TRALSHA2_256.Create;
begin
  FBufLength := 64;
  Initialize;
end;

function TRALSHA2_256.Finalize: TBytes;
begin
  FBuffer[FIndex]:= $80;
  if FIndex >= 56 then
    Compress;
  PCardinal(@FBuffer[56])^:= Swap(FLenHi);
  PCardinal(@FBuffer[60])^:= Swap(FLenLo);
  Compress;

  FHash[0]:= Swap(FHash[0]);
  FHash[1]:= Swap(FHash[1]);
  FHash[2]:= Swap(FHash[2]);
  FHash[3]:= Swap(FHash[3]);
  FHash[4]:= Swap(FHash[4]);
  FHash[5]:= Swap(FHash[5]);
  FHash[6]:= Swap(FHash[6]);
  FHash[7]:= Swap(FHash[7]);

  SetLength(Result,32);
  Move(FHash,Result[0],Sizeof(FHash));
end;

function TRALSHA2_256.GetDigest(AValue: TBytes): TBytes;
begin
  Initialize;
  UpdateBuffer(AValue);
  Result := Finalize;
end;

function TRALSHA2_256.GetDigest(AValue: StringRAL): TBytes;
begin
  Initialize;
  UpdateBuffer(AValue);
  Result := Finalize;
end;

function TRALSHA2_256.GetDigest(AValue: TStream): TBytes;
begin
  Initialize;
  UpdateBuffer(AValue);
  Result := Finalize;
end;

procedure TRALSHA2_256.Hash256(AData: PByte; ALength: IntegerRAL);
var
  vBufSize : Integer;
begin
  Inc(FLenHi,ALength shr 29);
  Inc(FLenLo,ALength*8);
  if FLenLo < (ALength*8) then
    Inc(FLenHi);

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

function TRALSHA2_256.HashAsStream(AValue: TStream): TStringStream;
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

function TRALSHA2_256.HashAsString(AValue: TStream): StringRAL;
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

function TRALSHA2_256.HMACAsDigest(AValue: TStream; AKey: TBytes): TBytes;
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

function TRALSHA2_256.HMACAsString(AValue: TStream; AKey: StringRAL): StringRAL;
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

function TRALSHA2_256.HMACAsString(AValue, AKey: StringRAL): StringRAL;
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

function TRALSHA2_256.HashAsString(AValue: StringRAL): StringRAL;
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

procedure TRALSHA2_256.Initialize;
begin
  FHash[0]:= $6a09e667;
  FHash[1]:= $bb67ae85;
  FHash[2]:= $3c6ef372;
  FHash[3]:= $a54ff53a;
  FHash[4]:= $510e527f;
  FHash[5]:= $9b05688c;
  FHash[6]:= $1f83d9ab;
  FHash[7]:= $5be0cd19;

  FillChar(FBuffer,Sizeof(FBuffer),0);
  FIndex := 0;
  FLenHi := 0;
  FLenLo := 0;
end;

function TRALSHA2_256.Swap(AValue: Cardinal): Cardinal;
begin
  Result:= ((AValue and $FF) shl 24) or ((AValue and $FF00) shl 8) or
           ((AValue and $FF0000) shr 8) or ((AValue and $FF000000) shr 24);
end;

procedure TRALSHA2_256.UpdateBuffer(AValue: TBytes);
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

procedure TRALSHA2_256.UpdateBuffer(AValue: StringRAL);
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

procedure TRALSHA2_256.UpdateBuffer(AValue: TStream);
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
    Hash256(@vInBuf[0], vBytesRead);

    vPosition := vPosition + vBytesRead;
  end;
end;

function TRALSHA2_256.HMACAsString(AValue: TBytes; AKey: StringRAL): StringRAL;
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

end.
