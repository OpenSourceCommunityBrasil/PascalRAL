{ One-time passwords: HOTP (RFC 4226) and TOTP (RFC 6238), the codes of
  authenticator apps, with base32 secrets, the otpauth:// URI of their QR
  code, a tolerance window and protection against reusing a code. }
unit RALOTP;

{$I ..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, DateUtils,
  RALTypes, RALTools, RALSCRAM, RALKeyValueStore;

type
  /// Hash of the HMAC under the codes; SHA-1 is what every app reads.
  TRALOTPAlgorithm = (rotSHA1, rotSHA256, rotSHA512);

  { Time-based codes for many users: makes secrets and their QR URI, checks a
    code with a window of steps around now, and (with Store) refuses a code
    already accepted for the same user. }
  TRALTOTP = class
  private
    FAlgorithm: TRALOTPAlgorithm;
    FDigits: IntegerRAL;
    FIssuer: StringRAL;
    FPeriod: IntegerRAL;
    FStore: TRALKeyValueStore;
    FWindow: IntegerRAL;
  public
    constructor Create;

    /// The code of ASecret (base32) at ATime (UTC Unix seconds; 0 = now).
    function Code(const ASecret: StringRAL; ATime: Int64RAL = 0): StringRAL;
    /// A new random secret, base32 (20 bytes, the size of SHA-1).
    function NewSecret: StringRAL;
    /// The otpauth://totp/ URI for AAccount, to show as a QR code.
    function URI(const AAccount, ASecret: StringRAL): StringRAL;
    { Whether ACode is a code of ASecret within Window steps of now. With Store
      and AUser, a code (or an older one) accepted before for AUser is refused. }
    function Verify(const ASecret, ACode: StringRAL; const AUser: StringRAL = ''): boolean;

    /// HMAC hash.
    property Algorithm: TRALOTPAlgorithm read FAlgorithm write FAlgorithm;
    /// Digits of a code (6 or 8).
    property Digits: IntegerRAL read FDigits write FDigits;
    /// Name of the service, shown by the app.
    property Issuer: StringRAL read FIssuer write FIssuer;
    /// Seconds of a step.
    property Period: IntegerRAL read FPeriod write FPeriod;
    { Where the last accepted step of each user is kept (not owned); nil = no reuse
      check. }
    property Store: TRALKeyValueStore read FStore write FStore;
    /// Steps tolerated before and after now (clock drift).
    property Window: IntegerRAL read FWindow write FWindow;
  end;

/// The HOTP code of AKey for ACounter, ADigits long.
function RALHOTP(const AKey: TBytes; ACounter: UInt64RAL; ADigits: IntegerRAL = 6;
  AAlgorithm: TRALOTPAlgorithm = rotSHA1): StringRAL;
/// The TOTP code of AKey at AUnixTime (UTC seconds), with steps of APeriod seconds.
function RALTOTPCode(const AKey: TBytes; AUnixTime: Int64RAL; APeriod: IntegerRAL = 30;
  ADigits: IntegerRAL = 6; AAlgorithm: TRALOTPAlgorithm = rotSHA1): StringRAL;
/// AData in base32 (RFC 4648), without padding.
function RALBase32Encode(const AData: TBytes): StringRAL;
/// Decodes base32, ignoring blanks, padding and case; False on a foreign character.
function RALBase32Decode(const AText: StringRAL; out AData: TBytes): boolean;
/// Current UTC time in Unix seconds.
function RALUnixNow: Int64RAL;

implementation

const
  /// The RFC 4648 base32 alphabet of TOTP secrets.
  cBase32: StringRAL = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';

{ RFC 3986 percent-encoding: everything but the unreserved characters, so a
  blank is %20 (authenticator apps show a '+' as it is) }
function PercentEncode(const AText: StringRAL): StringRAL;
const
  cHex: StringRAL = '0123456789ABCDEF';
var
  vInt: IntegerRAL;
  vChar: AnsiChar;
begin
  Result := '';
  for vInt := POSINISTR to RALHighStr(AText) do
  begin
    vChar := AText[vInt];
    if vChar in ['A'..'Z', 'a'..'z', '0'..'9', '-', '.', '_', '~'] then
      Result := Result + vChar
    else
      Result := Result + '%' + cHex[POSINISTR + (Ord(vChar) shr 4)] +
        cHex[POSINISTR + (Ord(vChar) and 15)];
  end;
end;

function RALUnixNow: Int64RAL;
begin
  Result := DateTimeToUnix(RALDateTimeToGMT(Now));
end;

function RALBase32Encode(const AData: TBytes): StringRAL;
var
  vInt, vBits: IntegerRAL;
  vBuffer: Cardinal;
begin
  Result := '';
  vBuffer := 0;
  vBits := 0;
  for vInt := 0 to High(AData) do
  begin
    vBuffer := (vBuffer shl 8) or AData[vInt];
    Inc(vBits, 8);
    while vBits >= 5 do
    begin
      Result := Result + cBase32[POSINISTR + Integer((vBuffer shr (vBits - 5)) and 31)];
      Dec(vBits, 5);
    end;
    vBuffer := vBuffer and ((Cardinal(1) shl vBits) - 1);
  end;
  if vBits > 0 then
    Result := Result + cBase32[POSINISTR + Integer((vBuffer shl (5 - vBits)) and 31)];
end;

function RALBase32Decode(const AText: StringRAL; out AData: TBytes): boolean;
var
  vInt, vBits, vValue, vLen: IntegerRAL;
  vBuffer: Cardinal;
  vChar: AnsiChar;
begin
  Result := False;
  SetLength(AData, (Length(AText) * 5) div 8 + 1);
  vLen := 0;
  vBuffer := 0;
  vBits := 0;
  for vInt := POSINISTR to RALHighStr(AText) do
  begin
    vChar := AText[vInt];
    if vChar in [' ', '-', '=', #9] then
      Continue;
    if vChar in ['a'..'z'] then
      vChar := AnsiChar(Ord(vChar) - 32);
    { a loop, not Pos: XE2's Pos takes no AnsiChar }
    vValue := 31;
    while (vValue >= 0) and (cBase32[POSINISTR + vValue] <> vChar) do
      Dec(vValue);
    if vValue < 0 then
      Exit;
    vBuffer := (vBuffer shl 5) or Cardinal(vValue);
    Inc(vBits, 5);
    if vBits >= 8 then
    begin
      AData[vLen] := Byte(vBuffer shr (vBits - 8));
      Inc(vLen);
      Dec(vBits, 8);
      vBuffer := vBuffer and ((Cardinal(1) shl vBits) - 1);
    end;
  end;
  SetLength(AData, vLen);
  Result := True;
end;

function RALHOTP(const AKey: TBytes; ACounter: UInt64RAL; ADigits: IntegerRAL;
  AAlgorithm: TRALOTPAlgorithm): StringRAL;
const
  cAlgs: array[TRALOTPAlgorithm] of TRALSCRAMAlg = (rsaSHA1, rsaSHA256, rsaSHA512);
var
  vMsg, vMac: TBytes;
  vInt, vOffset: IntegerRAL;
  vCode: Int64RAL;
  vMod: Int64RAL;
begin
  SetLength(vMsg, 8);
  for vInt := 7 downto 0 do
  begin
    vMsg[vInt] := Byte(ACounter and $FF);
    ACounter := ACounter shr 8;
  end;
  vMac := RALSCRAMHMAC(cAlgs[AAlgorithm], AKey, vMsg);
  { dynamic truncation (RFC 4226 5.3) }
  vOffset := vMac[High(vMac)] and $0F;
  vCode := (Int64RAL(vMac[vOffset] and $7F) shl 24) or (Int64RAL(vMac[vOffset + 1]) shl 16) or
    (Int64RAL(vMac[vOffset + 2]) shl 8) or Int64RAL(vMac[vOffset + 3]);
  vMod := 1;
  for vInt := 1 to ADigits do
    vMod := vMod * 10;
  Result := StringRAL(IntToStr(vCode mod vMod));
  while Length(Result) < ADigits do
    Result := '0' + Result;
end;

function RALTOTPCode(const AKey: TBytes; AUnixTime: Int64RAL; APeriod: IntegerRAL;
  ADigits: IntegerRAL; AAlgorithm: TRALOTPAlgorithm): StringRAL;
begin
  Result := RALHOTP(AKey, UInt64RAL(AUnixTime div APeriod), ADigits, AAlgorithm);
end;

{ TRALTOTP }

constructor TRALTOTP.Create;
begin
  inherited Create;
  FAlgorithm := rotSHA1;
  FDigits := 6;
  FPeriod := 30;
  FWindow := 1;
end;

function TRALTOTP.NewSecret: StringRAL;
begin
  Result := RALBase32Encode(RandomBytes(20));
end;

function TRALTOTP.Code(const ASecret: StringRAL; ATime: Int64RAL): StringRAL;
var
  vKey: TBytes;
begin
  if not RALBase32Decode(ASecret, vKey) then
    raise EConvertError.Create('TOTP: the secret is not base32');
  if ATime <= 0 then
    ATime := RALUnixNow;
  Result := RALTOTPCode(vKey, ATime, FPeriod, FDigits, FAlgorithm);
end;

function TRALTOTP.URI(const AAccount, ASecret: StringRAL): StringRAL;
const
  cNames: array[TRALOTPAlgorithm] of StringRAL = ('SHA1', 'SHA256', 'SHA512');
var
  vLabel: StringRAL;
begin
  vLabel := PercentEncode(AAccount);
  if FIssuer <> '' then
    vLabel := PercentEncode(FIssuer) + ':' + vLabel;
  Result := 'otpauth://totp/' + vLabel + '?secret=' + ASecret +
    '&algorithm=' + cNames[FAlgorithm] +
    '&digits=' + StringRAL(IntToStr(FDigits)) +
    '&period=' + StringRAL(IntToStr(FPeriod));
  if FIssuer <> '' then
    Result := Result + '&issuer=' + PercentEncode(FIssuer);
end;

function TRALTOTP.Verify(const ASecret, ACode: StringRAL; const AUser: StringRAL): boolean;
var
  vKey: TBytes;
  vStep, vNow, vLast, vFound: Int64RAL;
  vDelta: IntegerRAL;
  vValue: RawByteString;
begin
  Result := False;
  if (Length(ACode) <> FDigits) or not RALBase32Decode(ASecret, vKey) then
    Exit;
  vNow := RALUnixNow div FPeriod;
  vFound := -1;
  { every step of the window is computed, found or not: the time taken does
    not tell which step matched }
  for vDelta := -FWindow to FWindow do
  begin
    vStep := vNow + vDelta;
    if RALSameSecret(RALHOTP(vKey, UInt64RAL(vStep), FDigits, FAlgorithm), ACode) and
       (vFound < 0) then
      vFound := vStep;
  end;
  if vFound < 0 then
    Exit;
  if (FStore <> nil) and (AUser <> '') then
  begin
    { the step of the last code accepted: that one and older ones are spent }
    if FStore.Get('totp.last:' + AUser, vValue) then
    begin
      vLast := StrToInt64Def(string(vValue), -1);
      if vFound <= vLast then
        Exit;
    end;
    FStore.Put('totp.last:' + AUser, RawByteString(IntToStr(vFound)),
      Int64RAL(FPeriod) * (2 * FWindow + 2) * 1000);
  end;
  Result := True;
end;

end.
