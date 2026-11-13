{ The verifiers of a chat webhook: what proves a request comes from the platform.
  Every channel has one - a shared secret in a header, an HMAC of the raw body, or
  a signature check of its own (Ed25519, JWT) through an event - and a request
  that fails it is refused before anything else reads the body. }
unit RALChatVerify;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALTools, RALHashBase, RALSHA2_32;

type
  /// Reads a header (or query param) of the request by name; empty when absent.
  TRALChatHeaderReader = function(const AName: StringRAL): StringRAL of object;

  { Decides whether a webhook request is genuine. ABody is the raw body exactly as
    received (never re-serialized: the platforms sign it with their own escaping);
    ATimestamp comes back as Unix seconds when the request carries one the channel
    wants checked against the replay window, else 0. }
  TRALChatVerifier = class(TPersistent)
  public
    { True for a verifier that checks nothing (a platform that signs nothing): the
      webhook module warns in OnLog at every request. }
    function IsOpen: Boolean; virtual;
    /// Whether the request is genuine.
    function Verify(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
      out ATimestamp: Int64RAL): Boolean; virtual; abstract;
  end;

  /// Accepts everything, for a platform that signs nothing; the module warns each time.
  TRALChatOpenVerifier = class(TRALChatVerifier)
  public
    function IsOpen: Boolean; override;
    function Verify(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
      out ATimestamp: Int64RAL): Boolean; override;
  end;

  /// A shared secret in a header (Telegram's secret token), compared in constant time.
  TRALChatSecretVerifier = class(TRALChatVerifier)
  private
    FHeaderName: StringRAL;
    FSecret: StringRAL;
  public
    function Verify(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
      out ATimestamp: Int64RAL): Boolean; override;

    /// The header that carries the secret.
    property HeaderName: StringRAL read FHeaderName write FHeaderName;
    /// The secret; empty refuses everything.
    property Secret: StringRAL read FSecret write FSecret;
  end;

  /// How an HMAC is written in the header.
  TRALChatSignatureEncoding = (
    /// Lowercase hexadecimal.
    seHex,
    /// Base64.
    seBase64
  );

  { An HMAC-SHA256 of the raw body (Meta's X-Hub-Signature-256, Slack's v0): the
    signed text is Template with its body and ts placeholders replaced; the header
    value is Prefix + the signature. }
  TRALChatHMACVerifier = class(TRALChatVerifier)
  private
    FEncoding: TRALChatSignatureEncoding;
    FHeaderName: StringRAL;
    FPrefix: StringRAL;
    FSecret: StringRAL;
    FTemplate: StringRAL;
    FTimestampHeader: StringRAL;
  public
    constructor Create;

    function Verify(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
      out ATimestamp: Int64RAL): Boolean; override;

    /// How the signature is written.
    property Encoding: TRALChatSignatureEncoding read FEncoding write FEncoding;
    /// The header that carries the signature.
    property HeaderName: StringRAL read FHeaderName write FHeaderName;
    /// Text before the signature in the header ('sha256=').
    property Prefix: StringRAL read FPrefix write FPrefix;
    /// The key; empty refuses everything.
    property Secret: StringRAL read FSecret write FSecret;
    { The signed text: the placeholder body in braces is the raw body and ts the
      timestamp header's value; the body alone by default, Slack's is v0, ts, body
      joined by colons. }
    property Template: StringRAL read FTemplate write FTemplate;
    { The header with the Unix timestamp (replay window); empty when the platform
      sends none. }
    property TimestampHeader: StringRAL read FTimestampHeader write FTimestampHeader;
  end;

  /// What a verifier of the application decides: the whole request.
  TRALChatVerifyEvent = procedure(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
    var ATimestamp: Int64RAL; var AValid: Boolean) of object;

  { Hands the decision to an event: for Ed25519 (Discord), JWT (Teams) and anything
    the application knows better. No event, no request passes. }
  TRALChatEventVerifier = class(TRALChatVerifier)
  private
    FOnVerify: TRALChatVerifyEvent;
  public
    function Verify(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
      out ATimestamp: Int64RAL): Boolean; override;

    /// Decides.
    property OnVerify: TRALChatVerifyEvent read FOnVerify write FOnVerify;
  end;

implementation

{ TRALChatVerifier }

function TRALChatVerifier.IsOpen: Boolean;
begin
  Result := False;
end;

{ TRALChatOpenVerifier }

function TRALChatOpenVerifier.IsOpen: Boolean;
begin
  Result := True;
end;

function TRALChatOpenVerifier.Verify(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
  out ATimestamp: Int64RAL): Boolean;
begin
  ATimestamp := 0;
  Result := True;
end;

{ TRALChatSecretVerifier }

function TRALChatSecretVerifier.Verify(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
  out ATimestamp: Int64RAL): Boolean;
begin
  ATimestamp := 0;
  Result := (FSecret <> '') and Assigned(AHeaders) and
    RALSameSecret(AHeaders(FHeaderName), FSecret);
end;

{ TRALChatHMACVerifier }

constructor TRALChatHMACVerifier.Create;
begin
  inherited Create;
  FTemplate := '{body}';
end;

function TRALChatHMACVerifier.Verify(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
  out ATimestamp: Int64RAL): Boolean;
var
  vHash: TRALSHA2_32;
  vGot, vTs, vSigned, vWant: StringRAL;
  vPos: Integer;
begin
  ATimestamp := 0;
  Result := False;
  if (FSecret = '') or not Assigned(AHeaders) then
    Exit;
  vGot := AHeaders(FHeaderName);
  if (vGot = '') or (Copy(vGot, 1, Length(FPrefix)) <> FPrefix) then
    Exit;
  vGot := Copy(vGot, Length(FPrefix) + 1, MaxInt);
  vTs := '';
  if FTimestampHeader <> '' then
  begin
    vTs := AHeaders(FTimestampHeader);
    ATimestamp := StrToInt64Def(string(vTs), 0);
    if ATimestamp = 0 then
      Exit;
  end;
  // the template is expanded around the body without touching the body's bytes
  vSigned := FTemplate;
  vPos := Pos('{ts}', string(vSigned));
  if vPos > 0 then
    vSigned := Copy(vSigned, 1, vPos - 1) + vTs + Copy(vSigned, vPos + 4, MaxInt);
  vPos := Pos('{body}', string(vSigned));
  if vPos > 0 then
    vSigned := Copy(vSigned, 1, vPos - 1) + ABody + Copy(vSigned, vPos + 6, MaxInt)
  else
    vSigned := vSigned + ABody;
  vHash := TRALSHA2_32.Create;
  try
    vHash.Version := rsv256;
    if FEncoding = seBase64 then
      vHash.OutputType := rhotBase64
    else
      vHash.OutputType := rhotHex;
    vWant := vHash.HMACAsString(vSigned, FSecret);
  finally
    vHash.Free;
  end;
  if FEncoding = seHex then
  begin
    vWant := StringRAL(LowerCase(string(vWant)));
    vGot := StringRAL(LowerCase(string(vGot)));
  end;
  Result := RALSameSecret(vGot, vWant);
end;

{ TRALChatEventVerifier }

function TRALChatEventVerifier.Verify(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
  out ATimestamp: Int64RAL): Boolean;
begin
  ATimestamp := 0;
  Result := False;
  if Assigned(FOnVerify) then
    FOnVerify(ABody, AHeaders, ATimestamp, Result);
end;

end.
