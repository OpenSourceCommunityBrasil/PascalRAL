/// Authentication for MQTT: HMAC and PBKDF2 over SHA-1, SHA-256 and SHA-512,
/// SCRAM (RFC 5802, RFC 7677) on both sides of the enhanced authentication of
/// MQTT 5.0, and the components a client takes its credentials from - fixed,
/// SCRAM, a token fetched from a server, a token signed on the device, an
/// Azure shared access signature, or the application's own events. A token
/// that expires is renewed before the next connection, and one component may
/// serve many clients: they fetch one token, not one each.
unit RALMQTTAuth;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALConsts, RALCustomObjects, RALAuthBase, RALClient, RALResponse, RALToken,
  RALJWS, RALAuthentication, RALOAuth2, RALMQTTTypes;

type
  /// Hash of HMAC, PBKDF2 and SCRAM.
  TRALMQTTHashAlg = (mhaSHA1, mhaSHA256, mhaSHA512);

  /// One run of an enhanced authentication (the AUTH exchange), per connection.
  TRALMQTTAuthExchange = class
  public
    /// Authentication Data of the CONNECT (or of an AUTH 0x19 to re-authenticate).
    function Start: TBytes; virtual; abstract;
    { The next answer to the server's challenge (an AUTH 0x18). Raises
      ERALMQTTError to give up }
    function Step(const AServerData: TBytes): TBytes; virtual; abstract;
    { The server accepted (CONNACK or AUTH 0x00) with AServerData; raises when
      what the server proves about itself is wrong }
    procedure Finish(const AServerData: TBytes); virtual;
  end;

  { Base of the components a client takes its credentials from, linked by
    TRALMQTTClient.Authentication. Credentials is asked before every connection
    and reconnection, so a token close to expiring is renewed there. Shared by
    many clients: the lock makes N clients that find the token expired fetch it
    once }
  TRALMQTTAuthenticator = class(TRALComponent)
  private
    FLock: TCriticalSection;
    FRenewMargin: IntegerRAL;
  protected
    /// Moment (RALMQTTNow) the credential handed out last stops being valid; 0: never.
    FExpiresAt: Int64RAL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Fills the user name and password of a CONNECT for AClientID; a flag
      left False sends no field. May wait on the network (a token fetch) }
    procedure Credentials(const AClientID: StringRAL; var AUserName: StringRAL;
      var AHasUserName: boolean; var APassword: TBytes; var AHasPassword: boolean); virtual;
    /// Authentication Method of the enhanced authentication (5.0); empty for none.
    function AuthMethod: StringRAL; virtual;
    /// A new exchange for one connection; nil without enhanced authentication.
    function NewExchange: TRALMQTTAuthExchange; virtual;
    { Milliseconds until the credential should be renewed (its expiry minus
      RenewMargin); -1 when it does not expire }
    function RenewIn: Int64RAL;

    procedure Lock;
    procedure Unlock;
  published
    { Seconds before expiry when a token is renewed: before the next
      connection, or by AUTH re-authentication while connected (5.0) }
    property RenewMargin: IntegerRAL read FRenewMargin write FRenewMargin default 60;
  end;

  /// A fixed user name and password.
  TRALMQTTBasicAuth = class(TRALMQTTAuthenticator)
  private
    FPassword: StringRAL;
    FUserName: StringRAL;
  public
    procedure Credentials(const AClientID: StringRAL; var AUserName: StringRAL;
      var AHasUserName: boolean; var APassword: TBytes; var AHasPassword: boolean); override;
  published
    /// Password.
    property Password: StringRAL read FPassword write FPassword;
    /// User name.
    property UserName: StringRAL read FUserName write FUserName;
  end;

  /// SCRAM mechanism.
  TRALMQTTSCRAMMechanism = (msmSHA1, msmSHA256, msmSHA512);

  { SCRAM-SHA-1, SCRAM-SHA-256 or SCRAM-SHA-512 through the enhanced
    authentication of MQTT 5.0: the password never travels, and the client
    checks that the server knows it too }
  TRALMQTTSCRAMAuth = class(TRALMQTTAuthenticator)
  private
    FMechanism: TRALMQTTSCRAMMechanism;
    FPassword: StringRAL;
    FUserName: StringRAL;
  public
    constructor Create(AOwner: TComponent); override;
    function AuthMethod: StringRAL; override;
    function NewExchange: TRALMQTTAuthExchange; override;
  published
    /// Hash of the mechanism.
    property Mechanism: TRALMQTTSCRAMMechanism read FMechanism write FMechanism default msmSHA256;
    /// Password.
    property Password: StringRAL read FPassword write FPassword;
    /// User name.
    property UserName: StringRAL read FUserName write FUserName;
  end;

  /// Where a token goes in the CONNECT.
  TRALMQTTTokenAs = (
    /// The token is the password; UserName is the user name.
    mtaPassword,
    /// The token is the user name.
    mtaUserName,
    /// The token is both.
    mtaBoth);

  { A token fetched from a server: a RAL server with JWT (Source a
    TRALClientJWTAuth) or an OAuth2 provider (Source a TRALClientOAuth2), by
    the TRALClient in TokenClient, the way that client always gets its token.
    Renewed before it expires }
  TRALMQTTNetTokenAuth = class(TRALMQTTAuthenticator)
  private
    FProbeRoute: StringRAL;
    FSource: TRALAuthClient;
    FToken: StringRAL;
    FTokenAs: TRALMQTTTokenAs;
    FTokenClient: TRALClient;
    FUserName: StringRAL;

    procedure SetSource(AValue: TRALAuthClient);
    procedure SetTokenClient(AValue: TRALClient);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// The token now, fetching it when missing or about to expire.
    function FetchToken: StringRAL;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Credentials(const AClientID: StringRAL; var AUserName: StringRAL;
      var AHasUserName: boolean; var APassword: TBytes; var AHasPassword: boolean); override;
    /// The token handed out last.
    property Token: StringRAL read FToken;
  published
    { Route the TokenClient calls to make its authenticator fetch the token;
      what it answers does not matter }
    property ProbeRoute: StringRAL read FProbeRoute write FProbeRoute;
    /// The RAL authenticator that knows how to get the token.
    property Source: TRALAuthClient read FSource write SetSource;
    /// Where the token goes.
    property TokenAs: TRALMQTTTokenAs read FTokenAs write FTokenAs default mtaPassword;
    /// The client that reaches the token server.
    property TokenClient: TRALClient read FTokenClient write SetTokenClient;
    /// User name sent with the token as password.
    property UserName: StringRAL read FUserName write FUserName;
  end;

  { A JWT signed here, with a key of the device (HS*, RS*, ES*): the way
    several IoT clouds authenticate a device. Renewed before its exp }
  TRALMQTTSelfTokenAuth = class(TRALMQTTAuthenticator)
  private
    FAlgorithm: TRALJWTAlgorithm;
    FAudience: StringRAL;
    FClaims: TStrings;
    FIssuer: StringRAL;
    FKeyFile: StringRAL;
    FKeyPEM: StringRAL;
    FLifetime: IntegerRAL;
    FSecret: StringRAL;
    FSubject: StringRAL;
    FToken: StringRAL;
    FTokenAs: TRALMQTTTokenAs;
    FUserName: StringRAL;

    procedure SetClaims(AValue: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Credentials(const AClientID: StringRAL; var AUserName: StringRAL;
      var AHasUserName: boolean; var APassword: TBytes; var AHasPassword: boolean); override;
    /// A new token, now.
    function NewToken: StringRAL;
    /// The token handed out last.
    property Token: StringRAL read FToken;
  published
    /// Signature algorithm.
    property Algorithm: TRALJWTAlgorithm read FAlgorithm write FAlgorithm default tjaHSHA256;
    /// aud claim.
    property Audience: StringRAL read FAudience write FAudience;
    /// More claims, name=value.
    property Claims: TStrings read FClaims write SetClaims;
    /// iss claim.
    property Issuer: StringRAL read FIssuer write FIssuer;
    /// Private key file (PEM) for RS* and ES*.
    property KeyFile: StringRAL read FKeyFile write FKeyFile;
    /// Private key as PEM text, instead of KeyFile.
    property KeyPEM: StringRAL read FKeyPEM write FKeyPEM;
    /// Seconds a token lasts.
    property Lifetime: IntegerRAL read FLifetime write FLifetime default 3600;
    /// Secret of HS*.
    property Secret: StringRAL read FSecret write FSecret;
    /// sub claim.
    property Subject: StringRAL read FSubject write FSubject;
    /// Where the token goes.
    property TokenAs: TRALMQTTTokenAs read FTokenAs write FTokenAs default mtaPassword;
    /// User name sent with the token as password.
    property UserName: StringRAL read FUserName write FUserName;
  end;

  { A shared access signature (HMAC-SHA256 of the resource and the expiry with
    a key), the format of Azure IoT Hub and Event Grid. Made here, without
    network, and renewed before it expires }
  TRALMQTTSASAuth = class(TRALMQTTAuthenticator)
  private
    FKey: StringRAL;
    FKeyName: StringRAL;
    FLifetime: IntegerRAL;
    FResourceURI: StringRAL;
    FToken: StringRAL;
    FUserName: StringRAL;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Credentials(const AClientID: StringRAL; var AUserName: StringRAL;
      var AHasUserName: boolean; var APassword: TBytes; var AHasPassword: boolean); override;
    /// The signature for AExpiry (Unix seconds).
    function Signature(AExpiry: Int64RAL): StringRAL;
  published
    /// The key, in base64, as the portal shows it.
    property Key: StringRAL read FKey write FKey;
    /// Name of the policy (skn); empty for a device key.
    property KeyName: StringRAL read FKeyName write FKeyName;
    /// Seconds a signature lasts.
    property Lifetime: IntegerRAL read FLifetime write FLifetime default 3600;
    /// The resource signed ('myhub.azure-devices.net/devices/dev1').
    property ResourceURI: StringRAL read FResourceURI write FResourceURI;
    /// User name sent with it ('myhub.azure-devices.net/dev1/?api-version=2021-04-12').
    property UserName: StringRAL read FUserName write FUserName;
  end;

  /// Credentials asked from the application.
  TRALMQTTOnGetCredentials = procedure(Sender: TObject; const AClientID: StringRAL;
    var AUserName, APassword: StringRAL; var AExpiresIn: IntegerRAL) of object;
  { A challenge of the enhanced authentication: AStep counts from 0 (the
    data of the CONNECT, AServerData empty); answer in AResponse, or set
    AAccept False to give up. AStep -1 is the server's final data }
  TRALMQTTOnAuthChallenge = procedure(Sender: TObject; AStep: IntegerRAL;
    const AServerData: TBytes; var AResponse: TBytes; var AAccept: boolean) of object;

  /// Credentials from the application's events, for anything else.
  TRALMQTTCustomAuth = class(TRALMQTTAuthenticator)
  private
    FAuthMethod: StringRAL;
    FOnAuthChallenge: TRALMQTTOnAuthChallenge;
    FOnGetCredentials: TRALMQTTOnGetCredentials;
  public
    function AuthMethod: StringRAL; override;
    procedure Credentials(const AClientID: StringRAL; var AUserName: StringRAL;
      var AHasUserName: boolean; var APassword: TBytes; var AHasPassword: boolean); override;
    function NewExchange: TRALMQTTAuthExchange; override;
  published
    /// Authentication Method of the enhanced authentication; empty for none.
    property Method: StringRAL read FAuthMethod write FAuthMethod;
    /// Each step of the enhanced authentication.
    property OnAuthChallenge: TRALMQTTOnAuthChallenge read FOnAuthChallenge write FOnAuthChallenge;
    /// Before each connection.
    property OnGetCredentials: TRALMQTTOnGetCredentials read FOnGetCredentials write FOnGetCredentials;
  end;

  /// What a SCRAM server knows of a user: the salted keys, or the password.
  TRALMQTTSCRAMCredential = record
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
  TRALMQTTSCRAMLookup = function(const AUserName: StringRAL;
    out ACredential: TRALMQTTSCRAMCredential): boolean of object;

  { The server side of SCRAM, for the broker. Step takes the client's message
    and answers the next one; Done says it is over, Accepted whether the
    client proved the password }
  TRALMQTTSCRAMServer = class
  private
    FAlg: TRALMQTTHashAlg;
    FAuthMessage: StringRAL;
    FClientFirstBare: StringRAL;
    FCredential: TRALMQTTSCRAMCredential;
    FDone: boolean;
    FAccepted: boolean;
    FLookup: TRALMQTTSCRAMLookup;
    FNonce: StringRAL;
    FServerFirst: StringRAL;
    FStage: IntegerRAL;
    FUserName: StringRAL;
  public
    /// A server exchange of AMechanism, asking ALookup for users.
    constructor Create(AAlg: TRALMQTTHashAlg; ALookup: TRALMQTTSCRAMLookup);
    { Answers the client's AData: the server-first message after the
      client-first, the server-final after the client-final. Raises on a
      malformed message; a wrong password sets Done and leaves Accepted False }
    function Step(const AData: TBytes): TBytes;

    /// The client proved the password.
    property Accepted: boolean read FAccepted;
    /// The exchange is over.
    property Done: boolean read FDone;
    /// User name the client gave.
    property UserName: StringRAL read FUserName;
  end;

/// Digest of AData.
function RALMQTTHash(AAlg: TRALMQTTHashAlg; const AData: TBytes): TBytes;
/// HMAC of AData with AKey.
function RALMQTTHMAC(AAlg: TRALMQTTHashAlg; const AKey, AData: TBytes): TBytes;
/// PBKDF2 (RFC 8018) with HMAC of AAlg.
function RALMQTTPBKDF2(AAlg: TRALMQTTHashAlg; const APassword, ASalt: TBytes;
  AIterations, ALength: IntegerRAL): TBytes;
/// The SCRAM mechanism of an Authentication Method name; False when it is not one.
function RALMQTTSCRAMAlg(const AMethod: StringRAL; out AAlg: TRALMQTTHashAlg): boolean;
/// The Authentication Method name of a SCRAM hash ('SCRAM-SHA-256').
function RALMQTTSCRAMName(AAlg: TRALMQTTHashAlg): StringRAL;
{ A SCRAM client exchange for AUserName and APassword; AClientNonce empty
  makes a random one (fixed only for the RFC test vectors) }
function RALMQTTNewSCRAMClient(AAlg: TRALMQTTHashAlg; const AUserName,
  APassword: StringRAL; const AClientNonce: StringRAL = ''): TRALMQTTAuthExchange;

implementation

uses
  RALSHA1, RALSHA2_32, RALSHA2_64, RALHashBase, RALBase64, RALTools, RALUrlCoder;

type
  { the client side of SCRAM }
  TRALMQTTSCRAMClient = class(TRALMQTTAuthExchange)
  private
    FAlg: TRALMQTTHashAlg;
    FClientFirstBare: StringRAL;
    FNonce: StringRAL;
    FPassword: StringRAL;
    FServerSignature: TBytes;
    FUserName: StringRAL;
  public
    constructor Create(AAlg: TRALMQTTHashAlg; const AUserName, APassword,
      AClientNonce: StringRAL);
    function Start: TBytes; override;
    function Step(const AServerData: TBytes): TBytes; override;
    procedure Finish(const AServerData: TBytes); override;
  end;

  { the exchange of TRALMQTTCustomAuth: the events }
  TRALMQTTCustomExchange = class(TRALMQTTAuthExchange)
  private
    FOwner: TRALMQTTCustomAuth;
    FStep: IntegerRAL;
    function Ask(AStep: IntegerRAL; const AData: TBytes): TBytes;
  public
    function Start: TBytes; override;
    function Step(const AServerData: TBytes): TBytes; override;
    procedure Finish(const AServerData: TBytes); override;
  end;

function NewHash(AAlg: TRALMQTTHashAlg): TRALHashBase;
begin
  case AAlg of
    mhaSHA1: Result := TRALSHA1.Create;
    mhaSHA256: Result := TRALSHA2_32.Create;
  else
    Result := TRALSHA2_64.Create;
    TRALSHA2_64(Result).Version := rsv512;
  end;
end;

function RALMQTTHash(AAlg: TRALMQTTHashAlg; const AData: TBytes): TBytes;
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

function RALMQTTHMAC(AAlg: TRALMQTTHashAlg; const AKey, AData: TBytes): TBytes;
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

function RALMQTTPBKDF2(AAlg: TRALMQTTHashAlg; const APassword, ASalt: TBytes;
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

function RALMQTTSCRAMName(AAlg: TRALMQTTHashAlg): StringRAL;
begin
  case AAlg of
    mhaSHA1: Result := 'SCRAM-SHA-1';
    mhaSHA256: Result := 'SCRAM-SHA-256';
  else
    Result := 'SCRAM-SHA-512';
  end;
end;

function RALMQTTSCRAMAlg(const AMethod: StringRAL; out AAlg: TRALMQTTHashAlg): boolean;
begin
  Result := True;
  if AMethod = 'SCRAM-SHA-1' then
    AAlg := mhaSHA1
  else if AMethod = 'SCRAM-SHA-256' then
    AAlg := mhaSHA256
  else if AMethod = 'SCRAM-SHA-512' then
    AAlg := mhaSHA512
  else
  begin
    AAlg := mhaSHA256;
    Result := False;
  end;
end;

function RALMQTTNewSCRAMClient(AAlg: TRALMQTTHashAlg; const AUserName,
  APassword: StringRAL; const AClientNonce: StringRAL): TRALMQTTAuthExchange;
begin
  Result := TRALMQTTSCRAMClient.Create(AAlg, AUserName, APassword, AClientNonce);
end;

procedure AuthFail(const AReason: string);
begin
  raise ERALMQTTError.CreateReason(MQTTRC_NotAuthorized, Format(emMQTTAuthFailed, [AReason]));
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
  vParts: TStringList;
  vInt: IntegerRAL;
  vPart: string;
begin
  Result := '';
  vParts := TStringList.Create;
  try
    vParts.Delimiter := ',';
    vParts.StrictDelimiter := True;
    vParts.DelimitedText := string(AMessage);
    for vInt := 0 to vParts.Count - 1 do
    begin
      vPart := vParts[vInt];
      if (Length(vPart) >= 2) and (vPart[1] = Char(AName)) and (vPart[2] = '=') then
      begin
        Result := StringRAL(Copy(vPart, 3, MaxInt));
        Exit;
      end;
    end;
  finally
    vParts.Free;
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

function HashLen(AAlg: TRALMQTTHashAlg): IntegerRAL;
begin
  case AAlg of
    mhaSHA1: Result := 20;
    mhaSHA256: Result := 32;
  else
    Result := 64;
  end;
end;

{ TRALMQTTAuthExchange }

procedure TRALMQTTAuthExchange.Finish(const AServerData: TBytes);
begin
  // nothing to check by default
end;

{ TRALMQTTSCRAMClient }

constructor TRALMQTTSCRAMClient.Create(AAlg: TRALMQTTHashAlg; const AUserName,
  APassword, AClientNonce: StringRAL);
begin
  inherited Create;
  FAlg := AAlg;
  FUserName := AUserName;
  FPassword := APassword;
  FNonce := AClientNonce;
  if FNonce = '' then
    FNonce := TRALBase64.Encode(RandomBytes(18));
end;

function TRALMQTTSCRAMClient.Start: TBytes;
begin
  FClientFirstBare := 'n=' + SaslName(FUserName) + ',r=' + FNonce;
  Result := RALMQTTBytes('n,,' + FClientFirstBare);
end;

function TRALMQTTSCRAMClient.Step(const AServerData: TBytes): TBytes;
var
  vServerFirst, vNonce, vSalt, vFinalBare, vAuth: StringRAL;
  vIter: IntegerRAL;
  vSalted, vClientKey, vStored, vSig, vProof, vServerKey: TBytes;
begin
  vServerFirst := RALMQTTText(AServerData);
  if Attribute(vServerFirst, 'e') <> '' then
    AuthFail(string(Attribute(vServerFirst, 'e')));
  vNonce := Attribute(vServerFirst, 'r');
  vSalt := Attribute(vServerFirst, 's');
  vIter := StrToIntDef(string(Attribute(vServerFirst, 'i')), 0);
  // the server's nonce extends ours; anything else is someone else's exchange
  if (Copy(vNonce, 1, Length(FNonce)) <> FNonce) or (Length(vNonce) <= Length(FNonce)) or
     (vSalt = '') or (vIter < 1) then
    AuthFail('server-first-message');
  vSalted := RALMQTTPBKDF2(FAlg, RALMQTTBytes(FPassword), TRALBase64.DecodeAsBytes(vSalt),
    vIter, HashLen(FAlg));
  vClientKey := RALMQTTHMAC(FAlg, vSalted, RALMQTTBytes('Client Key'));
  vStored := RALMQTTHash(FAlg, vClientKey);
  vFinalBare := 'c=biws,r=' + vNonce; // biws = base64('n,,')
  vAuth := FClientFirstBare + ',' + vServerFirst + ',' + vFinalBare;
  vSig := RALMQTTHMAC(FAlg, vStored, RALMQTTBytes(vAuth));
  vProof := XorBytes(vClientKey, vSig);
  vServerKey := RALMQTTHMAC(FAlg, vSalted, RALMQTTBytes('Server Key'));
  FServerSignature := RALMQTTHMAC(FAlg, vServerKey, RALMQTTBytes(vAuth));
  Result := RALMQTTBytes(vFinalBare + ',p=' + TRALBase64.Encode(vProof));
end;

procedure TRALMQTTSCRAMClient.Finish(const AServerData: TBytes);
var
  vFinal: StringRAL;
begin
  vFinal := RALMQTTText(AServerData);
  if Attribute(vFinal, 'e') <> '' then
    AuthFail(string(Attribute(vFinal, 'e')));
  // the server proves it knows the password too, or it is not the server
  if (FServerSignature = nil) or
     not RALSameBytes(TRALBase64.DecodeAsBytes(Attribute(vFinal, 'v')), FServerSignature) then
    AuthFail(emMQTTSCRAMServerProof);
end;

{ TRALMQTTSCRAMServer }

constructor TRALMQTTSCRAMServer.Create(AAlg: TRALMQTTHashAlg; ALookup: TRALMQTTSCRAMLookup);
begin
  inherited Create;
  FAlg := AAlg;
  FLookup := ALookup;
end;

function TRALMQTTSCRAMServer.Step(const AData: TBytes): TBytes;
var
  vMessage, vNonce, vFinalBare, vProofText: StringRAL;
  vSalted, vClientKey, vClientSig, vProof, vServerSig: TBytes;
  vPos: IntegerRAL;
  vKnown: boolean;
begin
  Result := nil;
  vMessage := RALMQTTText(AData);
  case FStage of
    0:
    begin
      // gs2 header 'n,,' (no channel binding), then the bare message
      if Copy(vMessage, 1, 3) <> 'n,,' then
        AuthFail('client-first-message');
      FClientFirstBare := Copy(vMessage, 4, MaxInt);
      FUserName := UnSaslName(Attribute(FClientFirstBare, 'n'));
      vNonce := Attribute(FClientFirstBare, 'r');
      if (FUserName = '') or (vNonce = '') then
        AuthFail('client-first-message');
      vKnown := Assigned(FLookup) and FLookup(FUserName, FCredential);
      if not vKnown then
      begin
        { an unknown user goes through the same steps with made-up values:
          the answer does not tell which users exist }
        FCredential.Salt := RandomBytes(16);
        FCredential.Iterations := 4096;
        FCredential.StoredKey := RandomBytes(HashLen(FAlg));
        FCredential.ServerKey := RandomBytes(HashLen(FAlg));
        FCredential.Password := '';
      end
      else if (FCredential.StoredKey = nil) or (FCredential.ServerKey = nil) then
      begin
        if FCredential.Salt = nil then
          FCredential.Salt := RandomBytes(16);
        if FCredential.Iterations < 1 then
          FCredential.Iterations := 4096;
        vSalted := RALMQTTPBKDF2(FAlg, RALMQTTBytes(FCredential.Password),
          FCredential.Salt, FCredential.Iterations, HashLen(FAlg));
        FCredential.StoredKey := RALMQTTHash(FAlg, RALMQTTHMAC(FAlg, vSalted,
          RALMQTTBytes('Client Key')));
        FCredential.ServerKey := RALMQTTHMAC(FAlg, vSalted, RALMQTTBytes('Server Key'));
      end;
      FNonce := vNonce + TRALBase64.Encode(RandomBytes(18));
      FServerFirst := 'r=' + FNonce + ',s=' + TRALBase64.Encode(FCredential.Salt) +
        ',i=' + StringRAL(IntToStr(FCredential.Iterations));
      FStage := 1;
      Result := RALMQTTBytes(FServerFirst);
    end;
    1:
    begin
      vPos := Pos(StringRAL(',p='), vMessage);
      if vPos <= 0 then
        AuthFail('client-final-message');
      vFinalBare := Copy(vMessage, 1, vPos - 1);
      vProofText := Copy(vMessage, vPos + 3, MaxInt);
      if Attribute(vFinalBare, 'r') <> FNonce then
        AuthFail('client-final-message');
      FAuthMessage := FClientFirstBare + ',' + FServerFirst + ',' + vFinalBare;
      vClientSig := RALMQTTHMAC(FAlg, FCredential.StoredKey, RALMQTTBytes(FAuthMessage));
      vProof := TRALBase64.DecodeAsBytes(vProofText);
      FDone := True;
      FStage := 2;
      if Length(vProof) <> Length(vClientSig) then
      begin
        Result := RALMQTTBytes('e=invalid-proof');
        Exit;
      end;
      vClientKey := XorBytes(vProof, vClientSig);
      FAccepted := RALSameBytes(RALMQTTHash(FAlg, vClientKey), FCredential.StoredKey);
      if FAccepted then
      begin
        vServerSig := RALMQTTHMAC(FAlg, FCredential.ServerKey, RALMQTTBytes(FAuthMessage));
        Result := RALMQTTBytes('v=' + TRALBase64.Encode(vServerSig));
      end
      else
        Result := RALMQTTBytes('e=invalid-proof');
    end;
  else
    AuthFail('SCRAM');
  end;
end;

{ TRALMQTTAuthenticator }

constructor TRALMQTTAuthenticator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FRenewMargin := 60;
end;

destructor TRALMQTTAuthenticator.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TRALMQTTAuthenticator.AuthMethod: StringRAL;
begin
  Result := '';
end;

procedure TRALMQTTAuthenticator.Credentials(const AClientID: StringRAL;
  var AUserName: StringRAL; var AHasUserName: boolean; var APassword: TBytes;
  var AHasPassword: boolean);
begin
  // the client's own UserName and Password stay
end;

procedure TRALMQTTAuthenticator.Lock;
begin
  FLock.Acquire;
end;

function TRALMQTTAuthenticator.NewExchange: TRALMQTTAuthExchange;
begin
  Result := nil;
end;

function TRALMQTTAuthenticator.RenewIn: Int64RAL;
begin
  if FExpiresAt = 0 then
    Result := -1
  else
  begin
    Result := FExpiresAt - Int64RAL(FRenewMargin) * 1000 - RALMQTTNow;
    if Result < 0 then
      Result := 0;
  end;
end;

procedure TRALMQTTAuthenticator.Unlock;
begin
  FLock.Release;
end;

{ TRALMQTTBasicAuth }

procedure TRALMQTTBasicAuth.Credentials(const AClientID: StringRAL;
  var AUserName: StringRAL; var AHasUserName: boolean; var APassword: TBytes;
  var AHasPassword: boolean);
begin
  AUserName := FUserName;
  AHasUserName := FUserName <> '';
  APassword := RALMQTTBytes(FPassword);
  AHasPassword := FPassword <> '';
end;

{ TRALMQTTSCRAMAuth }

constructor TRALMQTTSCRAMAuth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMechanism := msmSHA256;
end;

function TRALMQTTSCRAMAuth.AuthMethod: StringRAL;
begin
  Result := RALMQTTSCRAMName(TRALMQTTHashAlg(Ord(FMechanism)));
end;

function TRALMQTTSCRAMAuth.NewExchange: TRALMQTTAuthExchange;
begin
  Result := TRALMQTTSCRAMClient.Create(TRALMQTTHashAlg(Ord(FMechanism)), FUserName,
    FPassword, '');
end;

{ token placement shared by the token authenticators }
procedure PlaceToken(ATokenAs: TRALMQTTTokenAs; const AToken, AUser: StringRAL;
  var AUserName: StringRAL; var AHasUserName: boolean; var APassword: TBytes;
  var AHasPassword: boolean);
begin
  case ATokenAs of
    mtaUserName:
    begin
      AUserName := AToken;
      AHasUserName := True;
    end;
    mtaBoth:
    begin
      AUserName := AToken;
      AHasUserName := True;
      APassword := RALMQTTBytes(AToken);
      AHasPassword := True;
    end;
  else
    if AUser <> '' then
      AUserName := AUser;
    // 3.1.1 refuses a password without a user name
    AHasUserName := True;
    APassword := RALMQTTBytes(AToken);
    AHasPassword := True;
  end;
end;

{ TRALMQTTNetTokenAuth }

constructor TRALMQTTNetTokenAuth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProbeRoute := '/';
end;

procedure TRALMQTTNetTokenAuth.Credentials(const AClientID: StringRAL;
  var AUserName: StringRAL; var AHasUserName: boolean; var APassword: TBytes;
  var AHasPassword: boolean);
var
  vToken: StringRAL;
begin
  vToken := FetchToken;
  PlaceToken(FTokenAs, vToken, FUserName, AUserName, AHasUserName, APassword, AHasPassword);
end;

function TRALMQTTNetTokenAuth.FetchToken: StringRAL;
var
  vExpires: TDateTime;
  vResponse: TRALResponse;
  vToken: StringRAL;
begin
  if (FSource = nil) or (FTokenClient = nil) then
    raise ERALMQTTError.CreateReason(MQTTRC_NotAuthorized, emMQTTNoTokenSource);
  Lock;
  try
    // one fetch for every client that finds the token old at the same time
    if (FToken <> '') and ((FExpiresAt = 0) or (RenewIn > 0)) then
    begin
      Result := FToken;
      Exit;
    end;
    if FSource is TRALClientJWTAuth then
      TRALClientJWTAuth(FSource).Token := '';
    if FTokenClient.Authentication <> FSource then
      FTokenClient.Authentication := FSource;
    vResponse := nil;
    try
      FTokenClient.Get(FProbeRoute, vResponse);
    finally
      FreeAndNil(vResponse);
    end;
    vExpires := 0;
    vToken := '';
    if FSource is TRALClientJWTAuth then
    begin
      FSource.Lock;
      try
        vToken := TRALClientJWTAuth(FSource).Token;
        vExpires := TRALClientJWTAuth(FSource).Payload.Expiration;
      finally
        FSource.Unlock;
      end;
    end
    else if FSource is TRALClientOAuth2 then
    begin
      vToken := TRALClientOAuth2(FSource).AccessToken;
      vExpires := TRALClientOAuth2(FSource).ExpiresAt;
    end;
    if vToken = '' then
      raise ERALMQTTError.CreateReason(MQTTRC_NotAuthorized, emMQTTTokenFailed);
    FToken := vToken;
    if vExpires > 0 then
      FExpiresAt := RALMQTTNow + MilliSecondsBetween(Now, vExpires)
    else
      FExpiresAt := 0;
    Result := FToken;
  finally
    Unlock;
  end;
end;

procedure TRALMQTTNetTokenAuth.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FSource then
      FSource := nil;
    if AComponent = FTokenClient then
      FTokenClient := nil;
  end;
end;

procedure TRALMQTTNetTokenAuth.SetSource(AValue: TRALAuthClient);
begin
  if FSource = AValue then
    Exit;
  if FSource <> nil then
    FSource.RemoveFreeNotification(Self);
  FSource := AValue;
  if FSource <> nil then
    FSource.FreeNotification(Self);
end;

procedure TRALMQTTNetTokenAuth.SetTokenClient(AValue: TRALClient);
begin
  if FTokenClient = AValue then
    Exit;
  if FTokenClient <> nil then
    FTokenClient.RemoveFreeNotification(Self);
  FTokenClient := AValue;
  if FTokenClient <> nil then
    FTokenClient.FreeNotification(Self);
end;

{ TRALMQTTSelfTokenAuth }

constructor TRALMQTTSelfTokenAuth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClaims := TStringList.Create;
  FLifetime := 3600;
  FAlgorithm := tjaHSHA256;
end;

destructor TRALMQTTSelfTokenAuth.Destroy;
begin
  FreeAndNil(FClaims);
  inherited Destroy;
end;

procedure TRALMQTTSelfTokenAuth.Credentials(const AClientID: StringRAL;
  var AUserName: StringRAL; var AHasUserName: boolean; var APassword: TBytes;
  var AHasPassword: boolean);
var
  vToken: StringRAL;
begin
  Lock;
  try
    if (FToken = '') or (RenewIn = 0) then
      FToken := NewToken;
    vToken := FToken;
  finally
    Unlock;
  end;
  PlaceToken(FTokenAs, vToken, FUserName, AUserName, AHasUserName, APassword, AHasPassword);
end;

function TRALMQTTSelfTokenAuth.NewToken: StringRAL;
var
  vJWT: TRALJWT;
  vKey: TRALJWSKey;
  vPEM: StringRAL;
  vInt: IntegerRAL;
  vFile: TStringList;
begin
  vJWT := TRALJWT.Create;
  vKey := nil;
  try
    vJWT.Header.Algorithm := FAlgorithm;
    if FAlgorithm in [tjaHSHA256, tjaHSHA384, tjaHSHA512] then
      vJWT.SignSecretKey := FSecret
    else
    begin
      vPEM := FKeyPEM;
      if (vPEM = '') and (FKeyFile <> '') then
      begin
        vFile := TStringList.Create;
        try
          vFile.LoadFromFile(string(FKeyFile));
          vPEM := StringRAL(vFile.Text);
        finally
          vFile.Free;
        end;
      end;
      vKey := TRALJWSKey.Create;
      vKey.LoadPrivateKeyPEM(vPEM);
      vJWT.SignKey := vKey;
    end;
    vJWT.Payload.IssuedAt := Now;
    vJWT.Payload.Expiration := IncSecond(Now, FLifetime);
    vJWT.Payload.Issuer := FIssuer;
    vJWT.Payload.Subject := FSubject;
    vJWT.Payload.Audience := FAudience;
    for vInt := 0 to FClaims.Count - 1 do
      if FClaims.Names[vInt] <> '' then
        vJWT.Payload.AddClaim(StringRAL(FClaims.Names[vInt]),
          StringRAL(FClaims.ValueFromIndex[vInt]));
    Result := vJWT.Token;
    FExpiresAt := RALMQTTNow + Int64RAL(FLifetime) * 1000;
  finally
    vJWT.Free;
    vKey.Free;
  end;
end;

procedure TRALMQTTSelfTokenAuth.SetClaims(AValue: TStrings);
begin
  FClaims.Assign(AValue);
end;

{ TRALMQTTSASAuth }

constructor TRALMQTTSASAuth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLifetime := 3600;
end;

procedure TRALMQTTSASAuth.Credentials(const AClientID: StringRAL;
  var AUserName: StringRAL; var AHasUserName: boolean; var APassword: TBytes;
  var AHasPassword: boolean);
var
  vToken: StringRAL;
begin
  Lock;
  try
    if (FToken = '') or (RenewIn = 0) then
    begin
      FToken := Signature(DateTimeToUnix(RALDateTimeToGMT(Now)) + FLifetime);
      FExpiresAt := RALMQTTNow + Int64RAL(FLifetime) * 1000;
    end;
    vToken := FToken;
  finally
    Unlock;
  end;
  AUserName := FUserName;
  AHasUserName := True;
  APassword := RALMQTTBytes(vToken);
  AHasPassword := True;
end;

function TRALMQTTSASAuth.Signature(AExpiry: Int64RAL): StringRAL;
var
  vResource, vToSign, vSig: StringRAL;
begin
  vResource := TRALHTTPCoder.EncodeURL(FResourceURI);
  vToSign := vResource + #10 + StringRAL(IntToStr(AExpiry));
  vSig := TRALBase64.Encode(RALMQTTHMAC(mhaSHA256, TRALBase64.DecodeAsBytes(FKey),
    RALMQTTBytes(vToSign)));
  Result := 'SharedAccessSignature sr=' + vResource + '&sig=' +
    TRALHTTPCoder.EncodeURL(vSig) + '&se=' + StringRAL(IntToStr(AExpiry));
  if FKeyName <> '' then
    Result := Result + '&skn=' + TRALHTTPCoder.EncodeURL(FKeyName);
end;

{ TRALMQTTCustomAuth }

function TRALMQTTCustomAuth.AuthMethod: StringRAL;
begin
  Result := FAuthMethod;
end;

procedure TRALMQTTCustomAuth.Credentials(const AClientID: StringRAL;
  var AUserName: StringRAL; var AHasUserName: boolean; var APassword: TBytes;
  var AHasPassword: boolean);
var
  vUser, vPass: StringRAL;
  vExpires: IntegerRAL;
begin
  if not Assigned(FOnGetCredentials) then
    Exit;
  vUser := AUserName;
  vPass := RALMQTTText(APassword);
  vExpires := 0;
  Lock;
  try
    FOnGetCredentials(Self, AClientID, vUser, vPass, vExpires);
    if vExpires > 0 then
      FExpiresAt := RALMQTTNow + Int64RAL(vExpires) * 1000
    else
      FExpiresAt := 0;
  finally
    Unlock;
  end;
  AUserName := vUser;
  AHasUserName := vUser <> '';
  APassword := RALMQTTBytes(vPass);
  AHasPassword := vPass <> '';
end;

function TRALMQTTCustomAuth.NewExchange: TRALMQTTAuthExchange;
begin
  Result := nil;
  if FAuthMethod = '' then
    Exit;
  Result := TRALMQTTCustomExchange.Create;
  TRALMQTTCustomExchange(Result).FOwner := Self;
end;

{ TRALMQTTCustomExchange }

function TRALMQTTCustomExchange.Ask(AStep: IntegerRAL; const AData: TBytes): TBytes;
var
  vAccept: boolean;
begin
  Result := nil;
  vAccept := True;
  if Assigned(FOwner.OnAuthChallenge) then
    FOwner.OnAuthChallenge(FOwner, AStep, AData, Result, vAccept);
  if not vAccept then
    AuthFail(string(FOwner.AuthMethod));
end;

procedure TRALMQTTCustomExchange.Finish(const AServerData: TBytes);
begin
  Ask(-1, AServerData);
end;

function TRALMQTTCustomExchange.Start: TBytes;
begin
  FStep := 0;
  Result := Ask(0, nil);
end;

function TRALMQTTCustomExchange.Step(const AServerData: TBytes): TBytes;
begin
  Inc(FStep);
  Result := Ask(FStep, AServerData);
end;

end.
