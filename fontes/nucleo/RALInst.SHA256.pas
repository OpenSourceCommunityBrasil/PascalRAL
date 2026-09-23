unit RALInst.SHA256;

{$mode ObjFPC}{$H+}
{$Q-}{$R-}

// SHA-256 (FIPS 180-4) para conferir o binario baixado na auto-atualizacao
// (F12) contra o SHA256SUMS do release. O fpsha256 do FPC so existe do 3.2.3
// em diante, e o Lazarus oficial vem com o 3.2.2.

interface

uses
  Classes, SysUtils;

// hash em hexadecimal minusculo, como o sha256sum escreve
function SHA256Texto(const ATexto: RawByteString): string;
function SHA256Arquivo(const AArquivo: string): string;

implementation

const
  K: array[0..63] of longword = (
    $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
    $d807aa98, $12835b01, $243185be, $550c7dc3, $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
    $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
    $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147, $06ca6351, $14292967,
    $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13, $650a7354, $766a0abb, $81c2c92e, $92722c85,
    $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
    $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
    $748f82ee, $78a5636f, $84c87814, $8cc70208, $90befffa, $a4506ceb, $bef9a3f7, $c67178f2);

type
  TEstado = record
    H: array[0..7] of longword;
    Bloco: array[0..63] of byte;
    NoBloco: integer;
    Total: qword;
  end;

function RotD(AValor: longword; AN: byte): longword; inline;
begin
  Result := (AValor shr AN) or (AValor shl (32 - AN));
end;

procedure Iniciar(var AEstado: TEstado);
begin
  AEstado.H[0] := $6a09e667; AEstado.H[1] := $bb67ae85;
  AEstado.H[2] := $3c6ef372; AEstado.H[3] := $a54ff53a;
  AEstado.H[4] := $510e527f; AEstado.H[5] := $9b05688c;
  AEstado.H[6] := $1f83d9ab; AEstado.H[7] := $5be0cd19;
  AEstado.NoBloco := 0;
  AEstado.Total := 0;
end;

procedure Comprimir(var AEstado: TEstado);
var
  W: array[0..63] of longword;
  A, B, C, D, E, F, G, H, T1, T2, S0, S1: longword;
  vInt: integer;
begin
  for vInt := 0 to 15 do
    W[vInt] := (longword(AEstado.Bloco[vInt * 4]) shl 24) or
               (longword(AEstado.Bloco[vInt * 4 + 1]) shl 16) or
               (longword(AEstado.Bloco[vInt * 4 + 2]) shl 8) or
               longword(AEstado.Bloco[vInt * 4 + 3]);
  for vInt := 16 to 63 do
  begin
    S0 := RotD(W[vInt - 15], 7) xor RotD(W[vInt - 15], 18) xor (W[vInt - 15] shr 3);
    S1 := RotD(W[vInt - 2], 17) xor RotD(W[vInt - 2], 19) xor (W[vInt - 2] shr 10);
    W[vInt] := W[vInt - 16] + S0 + W[vInt - 7] + S1;
  end;
  A := AEstado.H[0]; B := AEstado.H[1]; C := AEstado.H[2]; D := AEstado.H[3];
  E := AEstado.H[4]; F := AEstado.H[5]; G := AEstado.H[6]; H := AEstado.H[7];
  for vInt := 0 to 63 do
  begin
    S1 := RotD(E, 6) xor RotD(E, 11) xor RotD(E, 25);
    T1 := H + S1 + ((E and F) xor ((not E) and G)) + K[vInt] + W[vInt];
    S0 := RotD(A, 2) xor RotD(A, 13) xor RotD(A, 22);
    T2 := S0 + ((A and B) xor (A and C) xor (B and C));
    H := G; G := F; F := E; E := D + T1;
    D := C; C := B; B := A; A := T1 + T2;
  end;
  Inc(AEstado.H[0], A); Inc(AEstado.H[1], B); Inc(AEstado.H[2], C); Inc(AEstado.H[3], D);
  Inc(AEstado.H[4], E); Inc(AEstado.H[5], F); Inc(AEstado.H[6], G); Inc(AEstado.H[7], H);
end;

procedure Acrescentar(var AEstado: TEstado; ADados: PByte; ATamanho: integer);
var
  vInt: integer;
begin
  for vInt := 0 to ATamanho - 1 do
  begin
    AEstado.Bloco[AEstado.NoBloco] := ADados[vInt];
    Inc(AEstado.NoBloco);
    if AEstado.NoBloco = 64 then
    begin
      Comprimir(AEstado);
      AEstado.NoBloco := 0;
    end;
  end;
  Inc(AEstado.Total, ATamanho);
end;

function Finalizar(var AEstado: TEstado): string;
var
  vBits: qword;
  vInt: integer;
  vUm: byte;
begin
  vBits := AEstado.Total * 8;
  vUm := $80;
  Acrescentar(AEstado, @vUm, 1);
  vUm := 0;
  while AEstado.NoBloco <> 56 do
    Acrescentar(AEstado, @vUm, 1);
  for vInt := 7 downto 0 do
  begin
    vUm := byte(vBits shr (vInt * 8));
    Acrescentar(AEstado, @vUm, 1);
  end;
  Result := '';
  for vInt := 0 to 7 do
    Result := Result + LowerCase(IntToHex(AEstado.H[vInt], 8));
end;

function SHA256Texto(const ATexto: RawByteString): string;
var
  vEstado: TEstado;
begin
  Iniciar(vEstado);
  if ATexto <> '' then
    Acrescentar(vEstado, PByte(PAnsiChar(ATexto)), Length(ATexto));
  Result := Finalizar(vEstado);
end;

function SHA256Arquivo(const AArquivo: string): string;
var
  vEstado: TEstado;
  vArquivo: TFileStream;
  vBuf: array[0..65535] of byte;
  vLidos: integer;
begin
  Iniciar(vEstado);
  vArquivo := TFileStream.Create(AArquivo, fmOpenRead or fmShareDenyWrite);
  try
    repeat
      vLidos := vArquivo.Read(vBuf[0], SizeOf(vBuf));
      if vLidos > 0 then
        Acrescentar(vEstado, @vBuf[0], vLidos);
    until vLidos <= 0;
  finally
    vArquivo.Free;
  end;
  Result := Finalizar(vEstado);
end;

end.
