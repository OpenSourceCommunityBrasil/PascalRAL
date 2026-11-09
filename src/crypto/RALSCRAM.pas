/// SCRAM (RFC 5802, RFC 7677): the Salted Challenge Response Authentication
/// Mechanism of SASL, on both sides, over SHA-1, SHA-256 and SHA-512, with the
/// HMAC and PBKDF2 it is made of. The password never travels and the client
/// checks that the server knows it too. Pure calculation, no network: MQTT 5.0
/// enhanced authentication, SMTP, IMAP and POP3 (AUTH SCRAM-SHA-256) carry the
/// messages each in its own way.
unit RALSCRAM;

{$I ..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALExceptions, RALTypes;

type
  /// Hash under SCRAM, HMAC and PBKDF2.
  TRALSCRAMAlg = (rsaSHA1, rsaSHA256, rsaSHA512);

  /// A SCRAM message that breaks the RFC, or an exchange used out of order.
  ERALSCRAMError = class(ERALException);

  /// What a SCRAM server knows of a user: the salted keys, or the password.
  TRALSCRAMCredential = record
    /// Salt.
    Salt: TBytes;
    /// Iterations of PBKDF2.
    Iterations: IntegerRAL;
    /// StoredKey (H(ClientKey)); empty to derive it from Password.
    StoredKey: TBytes;
    /// ServerKey; empty to derive it from Password.
    ServerKey: TBytes;
    /// Password, when the keys are not given.
    Password: StringRAL;
  end;

  /// Asks the server application for a user's credential; False: no such user.
  TRALSCRAMLookup = function(const AUserName: StringRAL;
    out ACredential: TRALSCRAMCredential): boolean of object;

  { The client side of one SCRAM exchange: ClientFirst, then ClientFinal with
    the server-first message, then CheckServerFinal with the server-final one.
    Raises ERALSCRAMError on a malformed or hostile server message }
  TRALSCRAMClient = class
  private
    FAlg: TRALSCRAMAlg;
    FClientFirstBare: StringRAL;
    FNonce: StringRAL;
    FPassword: StringRAL;
    FServerSignature: TBytes;
    FUserName: StringRAL;
  public
    { An exchange for AUserName and APassword; AClientNonce empty makes a random
      one (a fixed one is only for the RFC test vectors) }
    constructor Create(AAlg: TRALSCRAMAlg; const AUserName, APassword: StringRAL;
      const AClientNonce: StringRAL = '');

    { True when AServerFinal proves the server knows the password; raises with
      the server's error ('e=...') }
    function CheckServerFinal(const AServerFinal: StringRAL): boolean;
    { The client-final message (with the proof) answering AServerFirst; raises
      when the server sent an error, or a nonce that does not extend ours }
    function ClientFinal(const AServerFirst: StringRAL): StringRAL;
    /// The client-first message, with the gs2 header ('n,,n=user,r=nonce').
    function ClientFirst: StringRAL;

    /// Hash of the mechanism.
    property Alg: TRALSCRAMAlg read FAlg;
    /// Client nonce.
    property Nonce: StringRAL read FNonce;
  end;

  { The server side of one SCRAM exchange. Step takes the client's message and
    answers the next one; Done says it is over, Accepted whether the client
    proved the password. An unknown user goes through the same steps with
    made-up values, so the answers do not tell which users exist }
  TRALSCRAMServer = class
  private
    FAccepted: boolean;
    FAlg: TRALSCRAMAlg;
    FAuthMessage: StringRAL;
    FClientFirstBare: StringRAL;
    FCredential: TRALSCRAMCredential;
    FDone: boolean;
    FLookup: TRALSCRAMLookup;
    FNonce: StringRAL;
    FServerFirst: StringRAL;
    FServerNonce: StringRAL;
    FStage: IntegerRAL;
    FUserName: StringRAL;
  public
    { A server exchange of AAlg asking ALookup for users; AServerNonce empty
      makes a random one (a fixed one is only for the RFC test vectors) }
    constructor Create(AAlg: TRALSCRAMAlg; ALookup: TRALSCRAMLookup;
      const AServerNonce: StringRAL = '');

    { Answers the client's AMessage: the server-first message after the
      client-first, the server-final after the client-final. Raises
      ERALSCRAMError on a malformed message; a wrong password sets Done, leaves
      Accepted False and answers 'e=invalid-proof' }
    function Step(const AMessage: StringRAL): StringRAL;

    /// The client proved the password.
    property Accepted: boolean read FAccepted;
    /// Hash of the mechanism.
    property Alg: TRALSCRAMAlg read FAlg;
    /// The exchange is over.
    property Done: boolean read FDone;
    /// User name the client gave (unescaped).
    property UserName: StringRAL read FUserName;
  end;

/// Digest of AData.
function RALSCRAMHash(AAlg: TRALSCRAMAlg; const AData: TBytes): TBytes;
/// HMAC of AData with AKey.
function RALSCRAMHMAC(AAlg: TRALSCRAMAlg; const AKey, AData: TBytes): TBytes;
/// PBKDF2 (RFC 8018) with HMAC of AAlg.
function RALSCRAMPBKDF2(AAlg: TRALSCRAMAlg; const APassword, ASalt: TBytes;
  AIterations, ALength: IntegerRAL): TBytes;
/// Length in bytes of the digest of AAlg.
function RALSCRAMHashLength(AAlg: TRALSCRAMAlg): IntegerRAL;
/// The SASL mechanism name of AAlg ('SCRAM-SHA-256').
function RALSCRAMName(AAlg: TRALSCRAMAlg): StringRAL;
/// The hash of a SASL mechanism name; False when it is not a SCRAM one.
function RALSCRAMAlgOf(const AMechanism: StringRAL; out AAlg: TRALSCRAMAlg): boolean;
{ The keys a server stores for APassword, so the password itself need not be
  kept: fills StoredKey and ServerKey of ACredential from its Salt and
  Iterations (a random 16-byte salt and 4096 when missing) }
procedure RALSCRAMDeriveKeys(AAlg: TRALSCRAMAlg; const APassword: StringRAL;
  var ACredential: TRALSCRAMCredential);
/// The text as its bytes (no conversion: SCRAM text is UTF-8).
function RALSCRAMBytes(const AText: StringRAL): TBytes;
/// The bytes as text.
function RALSCRAMText(const ABytes: TBytes): StringRAL;

implementation

uses
  RALHashBase, RALSHA1, RALSHA2_32, RALSHA2_64, RALBase64, RALTools;

const
  cIterations = 4096;
  cSaltSize = 16;
  cNonceSize = 18;

function NewHash(AAlg: TRALSCRAMAlg): TRALHashBase;
begin
  case AAlg of
    rsaSHA1: Result := TRALSHA1.Create;
    rsaSHA256: Result := TRALSHA2_32.Create;
  else
    Result := TRALSHA2_64.Create;
    TRALSHA2_64(Result).Version := rsv512;
  end;
end;

function RALSCRAMBytes(const AText: StringRAL): TBytes;
begin
  SetLength(Result, Length(AText));
  if Result <> nil then
    Move(Pointer(AText)^, Result[0], Length(Result));
end;

function RALSCRAMText(const ABytes: TBytes): StringRAL;
begin
  SetLength(Result, Length(ABytes));
  if ABytes <> nil then
    Move(ABytes[0], Pointer(Result)^, Length(ABytes));
end;

function RALSCRAMHashLength(AAlg: TRALSCRAMAlg): IntegerRAL;
begin
  case AAlg of
    rsaSHA1: Result := 20;
    rsaSHA256: Result := 32;
  else
    Result := 64;
  end;
end;

function RALSCRAMHash(AAlg: TRALSCRAMAlg; const AData: TBytes): TBytes;
var
  vHash: TRALHashBase;
begin
  vHash := NewHash(AAlg);
  try
    vHash.HashBegin;
    if AData <> nil then
      vHash.HashUpdate(@AData[0], Length(AData));
    Result := vHash.HashEnd;
  finally
    vHash.Free;
  end;
end;

function RALSCRAMHMAC(AAlg: TRALSCRAMAlg; const AKey, AData: TBytes): TBytes;
var
  vHash: TRALHashBase;
begin
  vHash := NewHash(AAlg);
  try
    vHash.HMACBegin(AKey);
    if AData <> nil then
      vHash.HMACUpdate(@AData[0], Length(AData));
    Result := vHash.HMACEnd;
  finally
    vHash.Free;
  end;
end;

function RALSCRAMPBKDF2(AAlg: TRALSCRAMAlg; const APassword, ASalt: TBytes;
  AIterations, ALength: IntegerRAL): TBytes;
var
  vHash: TRALHashBase;
  vBlock, vIter, vInt, vDone, vTake: IntegerRAL;
  vU, vT, vSalt: TBytes;
begin
  SetLength(Result, ALength);
  vHash := NewHash(AAlg);
  try
    vDone := 0;
    vBlock := 1;
    while vDone < ALength do
    begin
      // U1 = PRF(P, S || INT(i))
      vSalt := Copy(ASalt, 0, Length(ASalt));
      SetLength(vSalt, Length(ASalt) + 4);
      vSalt[Length(ASalt)] := Byte(vBlock shr 24);
      vSalt[Length(ASalt) + 1] := Byte(vBlock shr 16);
      vSalt[Length(ASalt) + 2] := Byte(vBlock shr 8);
      vSalt[Length(ASalt) + 3] := Byte(vBlock);
      vHash.HMACBegin(APassword);
      vHash.HMACUpdate(@vSalt[0], Length(vSalt));
      vU := vHash.HMACEnd;
      vT := Copy(vU, 0, Length(vU));
      for vIter := 2 to AIterations do
      begin
        vHash.HMACBegin(APassword);
        vHash.HMACUpdate(@vU[0], Length(vU));
        vU := vHash.HMACEnd;
        for vInt := 0 to High(vT) do
          vT[vInt] := vT[vInt] xor vU[vInt];
      end;
      vTake := Length(vT);
      if vTake > ALength - vDone then
        vTake := ALength - vDone;
      Move(vT[0], Result[vDone], vTake);
      Inc(vDone, vTake);
      Inc(vBlock);
    end;
  finally
    vHash.Free;
  end;
end;

function RALSCRAMName(AAlg: TRALSCRAMAlg): StringRAL;
begin
  case AAlg of
    rsaSHA1: Result := 'SCRAM-SHA-1';
    rsaSHA256: Result := 'SCRAM-SHA-256';
  else
    Result := 'SCRAM-SHA-512';
  end;
end;

function RALSCRAMAlgOf(const AMechanism: StringRAL; out AAlg: TRALSCRAMAlg): boolean;
var
  vName: StringRAL;
begin
  Result := True;
  vName := StringRAL(UpperCase(string(AMechanism)));
  if vName = 'SCRAM-SHA-1' then
    AAlg := rsaSHA1
  else if vName = 'SCRAM-SHA-256' then
    AAlg := rsaSHA256
  else if vName = 'SCRAM-SHA-512' then
    AAlg := rsaSHA512
  else
  begin
    AAlg := rsaSHA256;
    Result := False;
  end;
end;

procedure RALSCRAMDeriveKeys(AAlg: TRALSCRAMAlg; const APassword: StringRAL;
  var ACredential: TRALSCRAMCredential);
var
  vSalted: TBytes;
begin
  if ACredential.Salt = nil then
    ACredential.Salt := RandomBytes(cSaltSize);
  if ACredential.Iterations < 1 then
    ACredential.Iterations := cIterations;
  vSalted := RALSCRAMPBKDF2(AAlg, RALSCRAMBytes(APassword), ACredential.Salt,
    ACredential.Iterations, RALSCRAMHashLength(AAlg));
  ACredential.StoredKey := RALSCRAMHash(AAlg, RALSCRAMHMAC(AAlg, vSalted,
    RALSCRAMBytes('Client Key')));
  ACredential.ServerKey := RALSCRAMHMAC(AAlg, vSalted, RALSCRAMBytes('Server Key'));
end;

procedure Fail(const AWhat: string);
begin
  raise ERALSCRAMError.Create('SCRAM: ' + AWhat);
end;

{ a SCRAM saslname: ',' and '=' escaped (RFC 5802 5.1) }
function SaslName(const AName: StringRAL): StringRAL;
begin
  Result := StringRAL(StringReplace(StringReplace(string(AName), '=', '=3D', [rfReplaceAll]),
    ',', '=2C', [rfReplaceAll]));
end;

function UnSaslName(const AName: StringRAL): StringRAL;
begin
  Result := StringRAL(StringReplace(StringReplace(string(AName), '=2C', ',', [rfReplaceAll]),
    '=3D', '=', [rfReplaceAll]));
end;

{ the value of attribute AName ('r', 's') in a SCRAM message }
function Attribute(const AMessage: StringRAL; AName: AnsiChar): StringRAL;
var
  vStart, vEnd, vLen: IntegerRAL;
begin
  Result := '';
  vLen := Length(AMessage);
  vStart := 1;
  while vStart <= vLen do
  begin
    vEnd := vStart;
    while (vEnd <= vLen) and (AMessage[vEnd] <> ',') do
      Inc(vEnd);
    if (vEnd - vStart >= 2) and (AMessage[vStart] = AName) and
       (AMessage[vStart + 1] = '=') then
    begin
      Result := Copy(AMessage, vStart + 2, vEnd - vStart - 2);
      Exit;
    end;
    vStart := vEnd + 1;
  end;
end;

function XorBytes(const A, B: TBytes): TBytes;
var
  vInt: IntegerRAL;
begin
  SetLength(Result, Length(A));
  for vInt := 0 to High(A) do
    Result[vInt] := A[vInt] xor B[vInt];
end;

{ TRALSCRAMClient }

constructor TRALSCRAMClient.Create(AAlg: TRALSCRAMAlg; const AUserName,
  APassword: StringRAL; const AClientNonce: StringRAL);
begin
  inherited Create;
  FAlg := AAlg;
  FUserName := AUserName;
  FPassword := APassword;
  FNonce := AClientNonce;
  if FNonce = '' then
    FNonce := TRALBase64.Encode(RandomBytes(cNonceSize));
end;

function TRALSCRAMClient.ClientFirst: StringRAL;
begin
  FClientFirstBare := 'n=' + SaslName(FUserName) + ',r=' + FNonce;
  Result := 'n,,' + FClientFirstBare;
end;

function TRALSCRAMClient.ClientFinal(const AServerFirst: StringRAL): StringRAL;
var
  vNonce, vSalt, vFinalBare, vAuth: StringRAL;
  vIter: IntegerRAL;
  vSalted, vClientKey, vStored, vSig, vProof, vServerKey: TBytes;
begin
  if FClientFirstBare = '' then
    Fail('client-first-message not sent');
  if Attribute(AServerFirst, 'e') <> '' then
    Fail(string(Attribute(AServerFirst, 'e')));
  vNonce := Attribute(AServerFirst, 'r');
  vSalt := Attribute(AServerFirst, 's');
  vIter := StrToIntDef(string(Attribute(AServerFirst, 'i')), 0);
  // the server's nonce extends ours; anything else is someone else's exchange
  if (Copy(vNonce, 1, Length(FNonce)) <> FNonce) or (Length(vNonce) <= Length(FNonce)) or
     (vSalt = '') or (vIter < 1) then
    Fail('server-first-message');
  vSalted := RALSCRAMPBKDF2(FAlg, RALSCRAMBytes(FPassword), TRALBase64.DecodeAsBytes(vSalt),
    vIter, RALSCRAMHashLength(FAlg));
  vClientKey := RALSCRAMHMAC(FAlg, vSalted, RALSCRAMBytes('Client Key'));
  vStored := RALSCRAMHash(FAlg, vClientKey);
  vFinalBare := 'c=biws,r=' + vNonce; // biws = base64('n,,')
  vAuth := FClientFirstBare + ',' + AServerFirst + ',' + vFinalBare;
  vSig := RALSCRAMHMAC(FAlg, vStored, RALSCRAMBytes(vAuth));
  vProof := XorBytes(vClientKey, vSig);
  vServerKey := RALSCRAMHMAC(FAlg, vSalted, RALSCRAMBytes('Server Key'));
  FServerSignature := RALSCRAMHMAC(FAlg, vServerKey, RALSCRAMBytes(vAuth));
  Result := vFinalBare + ',p=' + TRALBase64.Encode(vProof);
end;

function TRALSCRAMClient.CheckServerFinal(const AServerFinal: StringRAL): boolean;
begin
  if Attribute(AServerFinal, 'e') <> '' then
    Fail(string(Attribute(AServerFinal, 'e')));
  // the server proves it knows the password too, or it is not the server
  Result := (FServerSignature <> nil) and
    RALSameBytes(TRALBase64.DecodeAsBytes(Attribute(AServerFinal, 'v')), FServerSignature);
end;

{ TRALSCRAMServer }

constructor TRALSCRAMServer.Create(AAlg: TRALSCRAMAlg; ALookup: TRALSCRAMLookup;
  const AServerNonce: StringRAL);
begin
  inherited Create;
  FAlg := AAlg;
  FLookup := ALookup;
  FServerNonce := AServerNonce;
end;

function TRALSCRAMServer.Step(const AMessage: StringRAL): StringRAL;
var
  vNonce, vFinalBare, vProofText: StringRAL;
  vClientKey, vClientSig, vProof, vServerSig: TBytes;
  vPos: IntegerRAL;
  vKnown: boolean;
begin
  Result := '';
  case FStage of
    0:
    begin
      // gs2 header 'n,,' (no channel binding, no authzid), then the bare message
      if Copy(AMessage, 1, 3) <> 'n,,' then
        Fail('client-first-message');
      FClientFirstBare := Copy(AMessage, 4, MaxInt);
      FUserName := UnSaslName(Attribute(FClientFirstBare, 'n'));
      vNonce := Attribute(FClientFirstBare, 'r');
      if (FUserName = '') or (vNonce = '') then
        Fail('client-first-message');
      FillChar(FCredential, SizeOf(FCredential), 0);
      vKnown := Assigned(FLookup) and FLookup(FUserName, FCredential);
      if not vKnown then
      begin
        FCredential.Salt := RandomBytes(cSaltSize);
        FCredential.Iterations := cIterations;
        FCredential.StoredKey := RandomBytes(RALSCRAMHashLength(FAlg));
        FCredential.ServerKey := RandomBytes(RALSCRAMHashLength(FAlg));
        FCredential.Password := '';
      end
      else if (FCredential.StoredKey = nil) or (FCredential.ServerKey = nil) then
        RALSCRAMDeriveKeys(FAlg, FCredential.Password, FCredential);
      if FServerNonce = '' then
        FServerNonce := TRALBase64.Encode(RandomBytes(cNonceSize));
      FNonce := vNonce + FServerNonce;
      FServerFirst := 'r=' + FNonce + ',s=' + TRALBase64.Encode(FCredential.Salt) +
        ',i=' + StringRAL(IntToStr(FCredential.Iterations));
      FStage := 1;
      Result := FServerFirst;
    end;
    1:
    begin
      vPos := Pos(StringRAL(',p='), AMessage);
      if vPos <= 0 then
        Fail('client-final-message');
      vFinalBare := Copy(AMessage, 1, vPos - 1);
      vProofText := Copy(AMessage, vPos + 3, MaxInt);
      if Attribute(vFinalBare, 'r') <> FNonce then
        Fail('client-final-message');
      FAuthMessage := FClientFirstBare + ',' + FServerFirst + ',' + vFinalBare;
      vClientSig := RALSCRAMHMAC(FAlg, FCredential.StoredKey, RALSCRAMBytes(FAuthMessage));
      vProof := TRALBase64.DecodeAsBytes(vProofText);
      FDone := True;
      FStage := 2;
      if Length(vProof) <> Length(vClientSig) then
      begin
        Result := 'e=invalid-proof';
        Exit;
      end;
      vClientKey := XorBytes(vProof, vClientSig);
      FAccepted := RALSameBytes(RALSCRAMHash(FAlg, vClientKey), FCredential.StoredKey);
      if FAccepted then
      begin
        vServerSig := RALSCRAMHMAC(FAlg, FCredential.ServerKey, RALSCRAMBytes(FAuthMessage));
        Result := 'v=' + TRALBase64.Encode(vServerSig);
      end
      else
        Result := 'e=invalid-proof';
    end;
  else
    Fail('exchange already over');
  end;
end;

end.
