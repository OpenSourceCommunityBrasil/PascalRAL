/// WS-Security on the server: an authenticator of the UsernameToken of SOAP envelopes.
unit RALWSSecurityAuth;

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
  RALTypes, RALConsts, RALRequest, RALResponse, RALParams, RALAuthBase, RALPlugin,
  RALKeyValueStore, RALXML, RALSOAPTypes, RALSOAPModule, RALWSSecurity;

type
  { Gives the password of AUserName (AFound True), for both password types: a
    digest can only be checked against the password itself. }
  TRALWSSOnGetPassword = procedure(ARequest: TRALRequest; const AUserName: StringRAL;
    var APassword: StringRAL; var AFound: boolean) of object;
  { Decides a text password (a hash kept in a database, for instance); when
    assigned, OnGetPassword is not asked for text passwords. }
  TRALWSSOnValidate = procedure(ARequest: TRALRequest; const AUserName, APassword: StringRAL;
    var AResult: boolean) of object;

  { Authenticator of SOAP requests by the WS-Security UsernameToken profile 1.1
    (password as text or digest, nonce, creation time), linked to the server like
    any other. It reads the envelope the TRALSOAPModule parsed (once, shared with
    the operation), refuses stale or replayed tokens, marks the wsse:Security
    block understood, and answers a refusal as the fault wsse:FailedAuthentication. }
  TRALServerWSSecurityAuth = class(TRALAuthServer)
  private
    FClockSkew: IntegerRAL;
    /// Nonces seen, when ReplayStore is not assigned.
    FInternalStore: TRALMemoryStore;
    FMaxAge: IntegerRAL;
    FOnGetPassword: TRALWSSOnGetPassword;
    FOnGetRoles: TRALOnGetRoles;
    FOnValidate: TRALWSSOnValidate;
    FPassword: StringRAL;
    FPasswordTypes: TRALWSSPasswordTypes;
    FReplayProtection: boolean;
    FReplayStore: TRALKeyValueStore;
    FUserName: StringRAL;

    /// Leaves the 401 that refuses ARequest.
    procedure Refuse(AResponse: TRALResponse);
    procedure SetReplayStore(const AValue: TRALKeyValueStore);
    /// Store of the nonces: ReplayStore, or the internal one.
    function Store: TRALKeyValueStore;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { A refused SOAP request that came without HTTP credentials is answered with the
      fault wsse:FailedAuthentication in the version of its envelope. }
    function AnswerRefusal(ARequest: TRALRequest; AResponse: TRALResponse;
      AResult: TRALAuthResult): boolean; override;
    /// Checks the UsernameToken of the envelope; a request without one is refused.
    procedure Validate(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    /// Seconds of tolerance between the clocks of client and server.
    property ClockSkew: IntegerRAL read FClockSkew write FClockSkew default 60;
    { Seconds a token stays valid after its wsu:Created; 0 does not check the age
      (nor remember nonces for long: an hour). }
    property MaxAge: IntegerRAL read FMaxAge write FMaxAge default 300;
    /// Gives the password of a user (see TRALWSSOnGetPassword).
    property OnGetPassword: TRALWSSOnGetPassword read FOnGetPassword write FOnGetPassword;
    { Fired after the user was accepted, to fill the roles of ARequest.Identity
      (route Roles, policies); the name and scheme are already there. }
    property OnGetRoles: TRALOnGetRoles read FOnGetRoles write FOnGetRoles;
    /// Decides a text password itself (see TRALWSSOnValidate).
    property OnValidate: TRALWSSOnValidate read FOnValidate write FOnValidate;
    /// Password of UserName, when OnGetPassword is not assigned.
    property Password: StringRAL read FPassword write FPassword;
    /// Password types accepted.
    property PasswordTypes: TRALWSSPasswordTypes read FPasswordTypes write FPasswordTypes
      default [wptText, wptDigest];
    { Refuses a nonce already seen and a digest without nonce or creation time,
      which could be replayed as it is. }
    property ReplayProtection: boolean read FReplayProtection write FReplayProtection
      default True;
    { Keeps the nonces seen; several servers behind a balancer share one
      (TRALFileStore). Unassigned: a store in memory of this component. }
    property ReplayStore: TRALKeyValueStore read FReplayStore write SetReplayStore;
    /// The one user accepted when OnGetPassword is not assigned.
    property UserName: StringRAL read FUserName write FUserName;
  end;

implementation

uses
  DateUtils,
  RALTools, RALBase64, RALMIMETypes;

{ TRALServerWSSecurityAuth }

constructor TRALServerWSSecurityAuth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetAuthType(ratWSSecurity);
  FClockSkew := 60;
  FMaxAge := 300;
  FPasswordTypes := [wptText, wptDigest];
  FReplayProtection := True;
  FInternalStore := TRALMemoryStore.Create(nil);
end;

destructor TRALServerWSSecurityAuth.Destroy;
begin
  FreeAndNil(FInternalStore);
  inherited Destroy;
end;

procedure TRALServerWSSecurityAuth.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FReplayStore) then
    FReplayStore := nil;
end;

procedure TRALServerWSSecurityAuth.SetReplayStore(const AValue: TRALKeyValueStore);
begin
  if FReplayStore = AValue then
    Exit;
  if FReplayStore <> nil then
    FReplayStore.RemoveFreeNotification(Self);
  FReplayStore := AValue;
  if FReplayStore <> nil then
    FReplayStore.FreeNotification(Self);
end;

function TRALServerWSSecurityAuth.Store: TRALKeyValueStore;
begin
  if FReplayStore <> nil then
    Result := FReplayStore
  else
    Result := FInternalStore;
end;

procedure TRALServerWSSecurityAuth.Refuse(AResponse: TRALResponse);
begin
  if AResponse.StatusCode < HTTP_BadRequest then
    AResponse.Answer(HTTP_Unauthorized);
end;

procedure TRALServerWSSecurityAuth.Validate(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vInfo: TRALSOAPEnvelopeInfo;
  vDocument: TRALXMLDocument;
  vToken: TRALWSSUsernameToken;
  vType: TRALWSSPasswordType;
  vNow, vCreated, vExpires: TDateTime;
  vHasCreated, vAccepted, vFound: boolean;
  vExpected: StringRAL;
  vNonce: TBytes;
  vTTL: Int64RAL;
begin
  AResponse.StatusCode := HTTP_OK;
  if not (ARequest.RouteData is TRALSOAPEnvelopeInfo) then
  begin
    Refuse(AResponse);
    Exit;
  end;
  vInfo := TRALSOAPEnvelopeInfo(ARequest.RouteData);

  try
    vDocument := vInfo.LoadDocument(ARequest);
  except
    // an envelope that does not parse carries no credentials
    Refuse(AResponse);
    Exit;
  end;
  if not RALWSSReadToken(vDocument.Root, vToken) then
  begin
    Refuse(AResponse);
    Exit;
  end;
  // credentials of this scheme came: brute force counts a refusal from here on
  if ARequest.Authorization.AuthType = ratNone then
    ARequest.Authorization.AuthType := ratWSSecurity;

  if (vToken.PasswordType = '') or (vToken.PasswordType = RALWSSPasswordTextType) then
    vType := wptText
  else if vToken.PasswordType = RALWSSPasswordDigestType then
    vType := wptDigest
  else
  begin
    Refuse(AResponse);
    Exit;
  end;
  if (not (vType in FPasswordTypes)) or (vToken.UserName = '') then
  begin
    Refuse(AResponse);
    Exit;
  end;

  // freshness: the creation time of the token, and the Timestamp's end
  vNow := RALDateTimeToGMT(Now);
  vHasCreated := vToken.Created <> '';
  vCreated := 0;
  if vHasCreated and not RALWSSParseTime(vToken.Created, vCreated) then
  begin
    Refuse(AResponse);
    Exit;
  end;
  if vHasCreated and (FMaxAge > 0) and
     ((SecondsBetween(vNow, vCreated) > FMaxAge + FClockSkew) and (vCreated < vNow) or
      (vCreated > IncSecond(vNow, FClockSkew))) then
  begin
    Refuse(AResponse);
    Exit;
  end;
  if vToken.Expires <> '' then
  begin
    if (not RALWSSParseTime(vToken.Expires, vExpires)) or
       (IncSecond(vExpires, FClockSkew) < vNow) then
    begin
      Refuse(AResponse);
      Exit;
    end;
  end;
  if FReplayProtection and (vType = wptDigest) and
     ((vToken.Nonce = '') or not vHasCreated) then
  begin
    Refuse(AResponse);
    Exit;
  end;

  // the password
  SetLength(vNonce, 0);
  if vToken.Nonce <> '' then
    vNonce := TRALBase64.DecodeAsBytes(vToken.Nonce);
  vAccepted := False;
  if (vType = wptText) and Assigned(FOnValidate) then
    FOnValidate(ARequest, vToken.UserName, vToken.Password, vAccepted)
  else
  begin
    vFound := False;
    vExpected := '';
    if Assigned(FOnGetPassword) then
      FOnGetPassword(ARequest, vToken.UserName, vExpected, vFound)
    else if (RALTrim(FUserName) <> '') and (FPassword <> '') and
            RALSameSecret(vToken.UserName, FUserName) then
    begin
      vExpected := FPassword;
      vFound := True;
    end;
    if vFound then
    begin
      if vType = wptText then
        vAccepted := RALSameSecret(vToken.Password, vExpected)
      else
        vAccepted := RALSameSecret(RALTrim(vToken.Password),
          RALWSSPasswordDigest(vNonce, vToken.Created, vExpected));
    end;
  end;
  if not vAccepted then
  begin
    Refuse(AResponse);
    Exit;
  end;

  // a nonce counts once while a token with it could still be accepted
  if FReplayProtection and (vToken.Nonce <> '') then
  begin
    if FMaxAge > 0 then
      vTTL := Int64RAL(FMaxAge + 2 * FClockSkew) * 1000
    else
      vTTL := 3600000;
    if Store.Increment('wss.nonce:' + vToken.UserName + ':' + vToken.Nonce, 1, vTTL) > 1 then
    begin
      Refuse(AResponse);
      Exit;
    end;
  end;

  vInfo.MarkUnderstood(RALWSSENamespace, 'Security');
  ARequest.Identity.Clear;
  ARequest.Identity.AuthScheme := ratWSSecurity;
  ARequest.Identity.Name := vToken.UserName;
  if Assigned(FOnGetRoles) then
    FOnGetRoles(ARequest, ARequest.Identity.Name, ARequest.Identity);
end;

function TRALServerWSSecurityAuth.AnswerRefusal(ARequest: TRALRequest;
  AResponse: TRALResponse; AResult: TRALAuthResult): boolean;
var
  vInfo: TRALSOAPEnvelopeInfo;
  vDocument: TRALXMLDocument;
  vFault, vCode: TRALXMLNode;
  vStream: TMemoryStream;
  vType: StringRAL;
begin
  Result := False;
  // a client that used HTTP authentication hears that scheme's 401 and its challenge
  if not (ARequest.RouteData is TRALSOAPEnvelopeInfo) or
     not (ARequest.Authorization.AuthType in [ratNone, ratWSSecurity]) then
    Exit;
  vInfo := TRALSOAPEnvelopeInfo(ARequest.RouteData);

  vDocument := TRALXMLDocument.Create;
  vStream := TMemoryStream.Create;
  try
    vFault := vDocument.CreateRoot('soap:Envelope')
      .AddNamespace('soap', RALSOAPNamespace(vInfo.Version))
      .AddChild('soap:Body')
      .AddChild('soap:Fault');
    if vInfo.Version = sv12 then
    begin
      vCode := vFault.AddChild('soap:Code');
      vCode.AddChild('soap:Value').WithText('soap:Sender');
      vCode.AddChild('soap:Subcode').AddChild('soap:Value')
        .AddNamespace('wsse', RALWSSENamespace).WithText('wsse:FailedAuthentication');
      vFault.AddChild('soap:Reason').AddChild('soap:Text')
        .AddAttribute('xml:lang', 'en').WithText(StringRAL(emWSSFailedAuthentication));
    end
    else
    begin
      vFault.AddChild('faultcode').AddNamespace('wsse', RALWSSENamespace)
        .WithText('wsse:FailedAuthentication');
      vFault.AddChild('faultstring').WithText(StringRAL(emWSSFailedAuthentication));
    end;
    vDocument.SaveToStream(vStream, True);
    vStream.Position := 0;

    vType := '';
    if vInfo.Module <> nil then
      vType := vInfo.Module.EnvelopeContentType;
    if vType = '' then
      vType := RALSOAPContentType(vInfo.Version);
    AResponse.Params.ClearParams(rpkBODY);
    AResponse.Answer(RALSOAPFaultStatus(vInfo.Version, sfcSender), vStream, vType, True);
    vStream := nil;
    Result := True;
  finally
    vStream.Free;
    vDocument.Free;
  end;
end;

end.
