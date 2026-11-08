/// WS-Security of RAL: the UsernameToken profile, read on the server and written by the client.
unit RALWSSecurity;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALXML;

const
  /// Namespace of the wsse elements (WS-Security 1.0 and 1.1).
  RALWSSENamespace = 'http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd';
  /// Namespace of the wsu elements and attributes (Id, Timestamp, Created).
  RALWSUNamespace = 'http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd';
  /// Type of a password sent as it is.
  RALWSSPasswordTextType = 'http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText';
  /// Type of a password sent as Base64(SHA-1(nonce + created + password)).
  RALWSSPasswordDigestType = 'http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordDigest';
  /// Encoding of a nonce in base64.
  RALWSSBase64Binary = 'http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-soap-message-security-1.0#Base64Binary';

type
  /// How a UsernameToken carries the password.
  TRALWSSPasswordType = (wptText, wptDigest);
  /// Set of TRALWSSPasswordType.
  TRALWSSPasswordTypes = set of TRALWSSPasswordType;

  /// What the wsse:Security block of an envelope carried.
  TRALWSSUsernameToken = record
    /// wsu:Created of the token, as written; '' when absent.
    Created: StringRAL;
    /// wsu:Expires of the wsu:Timestamp, as written; '' when absent.
    Expires: StringRAL;
    /// wsse:Nonce in base64, as written; '' when absent.
    Nonce: StringRAL;
    /// wsse:Password: the text, or the digest in base64.
    Password: StringRAL;
    /// The Type attribute of wsse:Password; '' means text.
    PasswordType: StringRAL;
    /// The wsse:Security element itself.
    Security: TRALXMLNode;
    /// wsse:Username.
    UserName: StringRAL;
  end;

  { The UsernameToken a TRALSOAPClient puts in the header of every envelope: the
    user, the password as text or digest, a nonce and a creation time against
    replays, and an optional wsu:Timestamp. Nothing is sent while UserName is empty. }
  TRALSOAPClientSecurity = class(TPersistent)
  private
    FAddCreated: boolean;
    FAddNonce: boolean;
    FMustUnderstand: boolean;
    FPassword: StringRAL;
    FPasswordType: TRALWSSPasswordType;
    FTimestamp: boolean;
    FTimestampTTL: IntegerRAL;
    FUserName: StringRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;

    { Adds the wsse:Security block to AHeader, the Header element of an envelope;
      does nothing while UserName is empty. }
    procedure AddTo(AHeader: TRALXMLNode);
    /// True when there is a user to send.
    function Enabled: boolean;
  published
    /// Sends wsu:Created; a digest always carries it.
    property AddCreated: boolean read FAddCreated write FAddCreated default True;
    /// Sends a random wsse:Nonce; a digest always carries one.
    property AddNonce: boolean read FAddNonce write FAddNonce default True;
    /// Marks the block mustUnderstand, so a server that does not check it refuses.
    property MustUnderstand: boolean read FMustUnderstand write FMustUnderstand default True;
    /// Password of the user.
    property Password: StringRAL read FPassword write FPassword;
    /// Password as text (over TLS) or as a digest that never shows it.
    property PasswordType: TRALWSSPasswordType read FPasswordType write FPasswordType
      default wptText;
    /// Sends a wsu:Timestamp with Created and Expires.
    property Timestamp: boolean read FTimestamp write FTimestamp default False;
    /// Seconds between Created and Expires of the wsu:Timestamp.
    property TimestampTTL: IntegerRAL read FTimestampTTL write FTimestampTTL default 300;
    /// User sent in wsse:Username; empty sends no block.
    property UserName: StringRAL read FUserName write FUserName;
  end;

/// Base64(SHA-1(ANonce + ACreated + APassword)), the digest of the UsernameToken profile.
function RALWSSPasswordDigest(const ANonce: TBytes; const ACreated, APassword: StringRAL): StringRAL;
{ Reads the first UsernameToken of the wsse:Security blocks in the Header of
  AEnvelope (the Envelope element); False when there is none. }
function RALWSSReadToken(AEnvelope: TRALXMLNode; out AToken: TRALWSSUsernameToken): boolean;
/// AValue (UTC) as an xsd:dateTime with milliseconds and Z.
function RALWSSFormatTime(AValue: TDateTime): StringRAL;
{ Reads an xsd:dateTime into AValue, in UTC: a Z or an offset is applied, a time
  without a zone is taken as UTC; False when it is not one. }
function RALWSSParseTime(const AText: StringRAL; out AValue: TDateTime): boolean;

implementation

uses
  DateUtils,
  RALTools, RALBase64, RALSHA1;

function RALWSSPasswordDigest(const ANonce: TBytes; const ACreated, APassword: StringRAL): StringRAL;
var
  vSHA: TRALSHA1;
begin
  vSHA := TRALSHA1.Create;
  try
    vSHA.HashBegin;
    if Length(ANonce) > 0 then
      vSHA.HashUpdate(@ANonce[0], Length(ANonce));
    if ACreated <> '' then
      vSHA.HashUpdate(Pointer(ACreated), Length(ACreated));
    if APassword <> '' then
      vSHA.HashUpdate(Pointer(APassword), Length(APassword));
    Result := TRALBase64.Encode(vSHA.HashEnd);
  finally
    vSHA.Free;
  end;
end;

function RALWSSFormatTime(AValue: TDateTime): StringRAL;
var
  vYear, vMonth, vDay, vHour, vMin, vSec, vMSec: Word;
begin
  DecodeDateTime(AValue, vYear, vMonth, vDay, vHour, vMin, vSec, vMSec);
  Result := StringRAL(Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%.3dZ',
    [vYear, vMonth, vDay, vHour, vMin, vSec, vMSec]));
end;

function RALWSSParseTime(const AText: StringRAL; out AValue: TDateTime): boolean;
var
  vPos, vLen: IntegerRAL;
  vYear, vMonth, vDay, vHour, vMin, vSec, vMSec, vDigits: IntegerRAL;
  vOffset, vOffHour, vOffMin, vSign: IntegerRAL;

  function Digits(ACount: IntegerRAL; out AResult: IntegerRAL): boolean;
  var
    vInt: IntegerRAL;
  begin
    Result := False;
    AResult := 0;
    if vPos + ACount - 1 > vLen then
      Exit;
    for vInt := 0 to ACount - 1 do
    begin
      if not (AText[vPos + vInt] in ['0'..'9']) then
        Exit;
      AResult := AResult * 10 + Ord(AText[vPos + vInt]) - Ord('0');
    end;
    Inc(vPos, ACount);
    Result := True;
  end;

  function Expect(AChar: AnsiChar): boolean;
  begin
    Result := (vPos <= vLen) and (AnsiChar(AText[vPos]) = AChar);
    if Result then
      Inc(vPos);
  end;

begin
  Result := False;
  AValue := 0;
  vPos := 1;
  vLen := Length(AText);
  if not (Digits(4, vYear) and Expect('-') and Digits(2, vMonth) and Expect('-') and
     Digits(2, vDay) and Expect('T') and Digits(2, vHour) and Expect(':') and
     Digits(2, vMin) and Expect(':') and Digits(2, vSec)) then
    Exit;

  // fractions: milliseconds are kept, finer digits read and dropped
  vMSec := 0;
  if Expect('.') then
  begin
    vDigits := 0;
    while (vPos <= vLen) and (AText[vPos] in ['0'..'9']) do
    begin
      if vDigits < 3 then
        vMSec := vMSec * 10 + Ord(AText[vPos]) - Ord('0');
      Inc(vDigits);
      Inc(vPos);
    end;
    if vDigits = 0 then
      Exit;
    while vDigits < 3 do
    begin
      vMSec := vMSec * 10;
      Inc(vDigits);
    end;
  end;

  vOffset := 0;
  if vPos <= vLen then
  begin
    if Expect('Z') then
      vOffset := 0
    else if (AText[vPos] = '+') or (AText[vPos] = '-') then
    begin
      if AText[vPos] = '-' then
        vSign := -1
      else
        vSign := 1;
      Inc(vPos);
      if not (Digits(2, vOffHour) and Expect(':') and Digits(2, vOffMin)) then
        Exit;
      vOffset := vSign * (vOffHour * 60 + vOffMin);
    end
    else
      Exit;
  end;
  if vPos <= vLen then
    Exit;

  if not TryEncodeDateTime(vYear, vMonth, vDay, vHour, vMin, vSec, vMSec, AValue) then
    Exit;
  // the offset says how far the written time is ahead of UTC
  AValue := IncMinute(AValue, -vOffset);
  Result := True;
end;

function RALWSSReadToken(AEnvelope: TRALXMLNode; out AToken: TRALWSSUsernameToken): boolean;
var
  vHeader, vBlock, vToken, vNode: TRALXMLNode;
begin
  Result := False;
  AToken.Created := '';
  AToken.Expires := '';
  AToken.Nonce := '';
  AToken.Password := '';
  AToken.PasswordType := '';
  AToken.Security := nil;
  AToken.UserName := '';
  if AEnvelope = nil then
    Exit;

  vHeader := AEnvelope.FirstElement;
  while (vHeader <> nil) and (vHeader.LocalName <> 'Header') do
    vHeader := vHeader.NextElement;
  if vHeader = nil then
    Exit;

  vBlock := vHeader.FirstElement;
  while vBlock <> nil do
  begin
    if (vBlock.LocalName = 'Security') and (vBlock.NamespaceURI = RALWSSENamespace) then
    begin
      vToken := vBlock.ChildNS(RALWSSENamespace, 'UsernameToken');
      if vToken <> nil then
      begin
        AToken.Security := vBlock;
        vNode := vToken.ChildNS(RALWSSENamespace, 'Username');
        if vNode <> nil then
          AToken.UserName := RALTrim(vNode.Text);
        vNode := vToken.ChildNS(RALWSSENamespace, 'Password');
        if vNode <> nil then
        begin
          AToken.Password := vNode.Text;
          AToken.PasswordType := RALTrim(vNode.Attribute('Type'));
        end;
        vNode := vToken.ChildNS(RALWSSENamespace, 'Nonce');
        if vNode <> nil then
          AToken.Nonce := RALTrim(vNode.Text);
        // Created is wsu; some old stacks wrote it in wsse
        vNode := vToken.ChildNS(RALWSUNamespace, 'Created');
        if vNode = nil then
          vNode := vToken.ChildNS(RALWSSENamespace, 'Created');
        if vNode <> nil then
          AToken.Created := RALTrim(vNode.Text);
        vNode := vBlock.ChildNS(RALWSUNamespace, 'Timestamp');
        if vNode <> nil then
        begin
          vNode := vNode.ChildNS(RALWSUNamespace, 'Expires');
          if vNode <> nil then
            AToken.Expires := RALTrim(vNode.Text);
        end;
        Result := True;
        Exit;
      end;
    end;
    vBlock := vBlock.NextElement;
  end;
end;

{ TRALSOAPClientSecurity }

constructor TRALSOAPClientSecurity.Create;
begin
  inherited Create;
  FAddCreated := True;
  FAddNonce := True;
  FMustUnderstand := True;
  FPasswordType := wptText;
  FTimestamp := False;
  FTimestampTTL := 300;
end;

procedure TRALSOAPClientSecurity.AssignTo(Dest: TPersistent);
var
  vDest: TRALSOAPClientSecurity;
begin
  if Dest is TRALSOAPClientSecurity then
  begin
    vDest := TRALSOAPClientSecurity(Dest);
    vDest.FAddCreated := FAddCreated;
    vDest.FAddNonce := FAddNonce;
    vDest.FMustUnderstand := FMustUnderstand;
    vDest.FPassword := FPassword;
    vDest.FPasswordType := FPasswordType;
    vDest.FTimestamp := FTimestamp;
    vDest.FTimestampTTL := FTimestampTTL;
    vDest.FUserName := FUserName;
  end
  else
    inherited AssignTo(Dest);
end;

function TRALSOAPClientSecurity.Enabled: boolean;
begin
  Result := FUserName <> '';
end;

procedure TRALSOAPClientSecurity.AddTo(AHeader: TRALXMLNode);
var
  vSecurity, vToken, vStamp: TRALXMLNode;
  vNow: TDateTime;
  vCreated, vSoapPrefix: StringRAL;
  vNonce: TBytes;
  vDigest: boolean;
begin
  if (AHeader = nil) or not Enabled then
    Exit;

  vDigest := FPasswordType = wptDigest;
  vNow := RALDateTimeToGMT(Now);
  vCreated := RALWSSFormatTime(vNow);

  vSecurity := AHeader.AddChild('wsse:Security')
    .AddNamespace('wsse', RALWSSENamespace)
    .AddNamespace('wsu', RALWSUNamespace);
  if FMustUnderstand then
  begin
    // the attribute belongs to the envelope's namespace, with its prefix
    vSoapPrefix := AHeader.Prefix;
    if vSoapPrefix <> '' then
      vSecurity.AddAttribute(vSoapPrefix + ':mustUnderstand', '1')
    else
      vSecurity.AddNamespace('wssoap', AHeader.NamespaceURI)
        .AddAttribute('wssoap:mustUnderstand', '1');
  end;

  if FTimestamp then
  begin
    vStamp := vSecurity.AddChild('wsu:Timestamp').AddAttribute('wsu:Id', 'TS-1');
    vStamp.AddChild('wsu:Created').WithText(vCreated);
    vStamp.AddChild('wsu:Expires').WithText(RALWSSFormatTime(IncSecond(vNow, FTimestampTTL)));
  end;

  vToken := vSecurity.AddChild('wsse:UsernameToken').AddAttribute('wsu:Id', 'UsernameToken-1');
  vToken.AddChild('wsse:Username').WithText(FUserName);
  SetLength(vNonce, 0);
  if FAddNonce or vDigest then
    vNonce := RandomBytes(16);
  if vDigest then
    vToken.AddChild('wsse:Password').AddAttribute('Type', RALWSSPasswordDigestType)
      .WithText(RALWSSPasswordDigest(vNonce, vCreated, FPassword))
  else
    vToken.AddChild('wsse:Password').AddAttribute('Type', RALWSSPasswordTextType)
      .WithText(FPassword);
  if Length(vNonce) > 0 then
    vToken.AddChild('wsse:Nonce').AddAttribute('EncodingType', RALWSSBase64Binary)
      .WithText(TRALBase64.Encode(vNonce));
  if FAddCreated or vDigest then
    vToken.AddChild('wsu:Created').WithText(vCreated);
end;

end.
