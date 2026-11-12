/// Protection against cross-site request forgery (CSRF) for pages served with cookies.
unit RALCSRF;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALTools, RALRequest, RALResponse, RALParams, RALPlugin,
  RALHashes, RALServer, RALWebModule;

const
  /// Priority of TRALCSRFPlugin: after CORS, before authentication.
  RALPriorityCSRF = 650;
  /// TRALCSRFPlugin.CookieName when nobody says otherwise.
  cRALCSRFCookie = 'ral_csrf';
  /// TRALCSRFPlugin.FieldName when nobody says otherwise.
  cRALCSRFField = '_csrf';
  /// TRALCSRFPlugin.HeaderName when nobody says otherwise.
  cRALCSRFHeader = 'X-CSRF-Token';

type
  { Refuses, with 403, a request that changes state (any method but GET, HEAD,
    OPTIONS, TRACE and QUERY) unless it carries the token of its own cookie:
    the double-submit pattern, the token signed with an HMAC of Secret and, with
    WebModule linked, bound to the browser's session. A page puts the token in
    its forms (FieldHTML, or csrf_field in a TRALViewModule template) or in the
    header HeaderName (HTMX: hx-headers). An Origin or Referer of another site
    not in AllowedOrigins is refused too. IncludePaths and ExcludePaths limit the
    plugin to the pages that need it. }
  TRALCSRFPlugin = class(TRALPlugin)
  private
    FAllowedOrigins: TStrings;
    /// The secret used while Secret is empty: random, for this process only.
    FAutoSecret: StringRAL;
    FCookieName: StringRAL;
    FFieldName: StringRAL;
    FHeaderName: StringRAL;
    FSecret: StringRAL;
    FSkipBearer: boolean;
    FWebModule: TRALWebModule;

    /// What a token of ARequest is bound to: its session id, or ''.
    function BindingOf(ARequest: TRALRequest): StringRAL;
    /// Whether the Origin, else the Referer, of ARequest is this site or allowed.
    function CheckOrigin(ARequest: TRALRequest): boolean;
    function IsCookieNameStored: boolean;
    function IsFieldNameStored: boolean;
    function IsHeaderNameStored: boolean;
    /// The HMAC of ANonce for ABinding, in base64url.
    function Mac(const ANonce, ABinding: StringRAL): StringRAL;
    /// Secret, or the random one of the process.
    function SecretKey: StringRAL;
    procedure SetAllowedOrigins(AValue: TStrings);
    procedure SetWebModule(AValue: TRALWebModule);
    /// Whether AToken was made by this plugin for ABinding.
    function ValidToken(const AToken, ABinding: StringRAL): boolean;
  protected
    class function DefaultPriority: IntegerRAL; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { A hidden input with the token, for a form:
      <input type="hidden" name="_csrf" value="...">. }
    function FieldHTML(ARequest: TRALRequest; AResponse: TRALResponse): StringRAL;
    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
    { The token of the browser: the one of its cookie while it is valid for its
      session, otherwise a new one, whose cookie goes out with AResponse. }
    function Token(ARequest: TRALRequest; AResponse: TRALResponse): StringRAL;
  published
    { Origins (scheme://host[:port], one per line) of other sites allowed to
      send requests that change state; the site's own is always allowed. }
    property AllowedOrigins: TStrings read FAllowedOrigins write SetAllowedOrigins;
    { The cookie holding the token. Scripts can read it (not HttpOnly), so a
      page can copy it into the header. }
    property CookieName: StringRAL read FCookieName write FCookieName
      stored IsCookieNameStored;
    /// The form field the token may come in.
    property FieldName: StringRAL read FFieldName write FFieldName
      stored IsFieldNameStored;
    /// The header the token may come in.
    property HeaderName: StringRAL read FHeaderName write FHeaderName
      stored IsHeaderNameStored;
    { The key of the HMAC that signs the tokens. Empty uses a random one made
      when the plugin is created: tokens then die with the process, and servers
      behind a balancer must share one. }
    property Secret: StringRAL read FSecret write FSecret;
    { Lets a request with an Authorization: Bearer header through: a browser
      never adds one by itself, so such a request is not forged. }
    property SkipBearer: boolean read FSkipBearer write FSkipBearer default True;
    { The WebModule whose sessions the tokens are bound to: a token taken from
      another session is refused. Nil binds them to nothing. }
    property WebModule: TRALWebModule read FWebModule write SetWebModule;
  end;

implementation

const
  /// HTTP status of a refused request.
  cStatusForbidden = 403;

/// AText in base64 turned into base64url without padding.
function Base64Url(const AText: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := AText;
  for vInt := POSINISTR to Length(Result) - 1 + POSINISTR do
    case Result[vInt] of
      '+': Result[vInt] := '-';
      '/': Result[vInt] := '_';
    end;
  while (Result <> '') and (Result[Length(Result) - 1 + POSINISTR] = '=') do
    SetLength(Result, Length(Result) - 1);
end;

/// The bytes ABytes in lowercase hex.
function HexOf(const ABytes: TBytes): StringRAL;
const
  cHex: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
                                   '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  vInt: IntegerRAL;
begin
  SetLength(Result, Length(ABytes) * 2);
  for vInt := 0 to High(ABytes) do
  begin
    Result[vInt * 2 + POSINISTR] := cHex[ABytes[vInt] shr 4];
    Result[vInt * 2 + 1 + POSINISTR] := cHex[ABytes[vInt] and 15];
  end;
end;

/// scheme://host[:port] of the URL AURL, lowercase; '' when it has no scheme.
function OriginOfURL(const AURL: StringRAL): StringRAL;
var
  vPos, vEnd, vStop: IntegerRAL;
  vRest: StringRAL;
begin
  Result := '';
  vPos := Pos('://', AURL);
  if vPos <= 1 then
    Exit;
  vRest := Copy(AURL, vPos + 3, MaxInt);
  vEnd := Length(vRest) + 1;
  vStop := Pos('/', vRest);
  if (vStop > 0) and (vStop < vEnd) then
    vEnd := vStop;
  vStop := Pos('?', vRest);
  if (vStop > 0) and (vStop < vEnd) then
    vEnd := vStop;
  vStop := Pos('#', vRest);
  if (vStop > 0) and (vStop < vEnd) then
    vEnd := vStop;
  Result := StringRAL(LowerCase(string(Copy(AURL, 1, vPos + 2 + vEnd - 1))));
end;

/// The host[:port] of an origin.
function AuthorityOf(const AOrigin: StringRAL): StringRAL;
var
  vPos: IntegerRAL;
begin
  vPos := Pos('://', AOrigin);
  if vPos > 0 then
    Result := Copy(AOrigin, vPos + 3, MaxInt)
  else
    Result := AOrigin;
end;

/// The value of the param AName of kind AKind in ARequest, '' when missing.
function ParamOf(ARequest: TRALRequest; const AName: StringRAL;
  AKind: TRALParamKind): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := ARequest.Params.GetKind[AName, AKind];
  if vParam <> nil then
    Result := vParam.AsString;
end;

{ TRALCSRFPlugin }

constructor TRALCSRFPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowedOrigins := TStringList.Create;
  FCookieName := cRALCSRFCookie;
  FFieldName := cRALCSRFField;
  FHeaderName := cRALCSRFHeader;
  FSkipBearer := True;
  FAutoSecret := HexOf(RandomBytes(32));
end;

destructor TRALCSRFPlugin.Destroy;
begin
  FreeAndNil(FAllowedOrigins);
  inherited Destroy;
end;

function TRALCSRFPlugin.BindingOf(ARequest: TRALRequest): StringRAL;
var
  vSession: TRALWebSession;
begin
  Result := '';
  if FWebModule = nil then
    Exit;
  vSession := FWebModule.Session[ARequest];
  if vSession <> nil then
    Result := vSession.Name;
end;

function TRALCSRFPlugin.CheckOrigin(ARequest: TRALRequest): boolean;
var
  vOrigin: StringRAL;
  vInt: IntegerRAL;
begin
  vOrigin := ParamOf(ARequest, 'Origin', rpkHEADER);
  // a privacy setting may send "null": judged by the token alone
  if (vOrigin = '') or (vOrigin = 'null') then
    vOrigin := OriginOfURL(ParamOf(ARequest, 'Referer', rpkHEADER))
  else
    vOrigin := StringRAL(LowerCase(string(vOrigin)));
  Result := True;
  if (vOrigin = '') or (vOrigin = 'null') then
    Exit;
  if RALSameName(AuthorityOf(vOrigin), ARequest.Host) then
    Exit;
  for vInt := 0 to Pred(FAllowedOrigins.Count) do
    if RALSameName(vOrigin, StringRAL(Trim(FAllowedOrigins[vInt]))) then
      Exit;
  Result := False;
end;

class function TRALCSRFPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityCSRF;
end;

function TRALCSRFPlugin.FieldHTML(ARequest: TRALRequest;
  AResponse: TRALResponse): StringRAL;
begin
  Result := '<input type="hidden" name="' + FFieldName + '" value="' +
    Token(ARequest, AResponse) + '">';
end;

function TRALCSRFPlugin.IsCookieNameStored: boolean;
begin
  Result := FCookieName <> cRALCSRFCookie;
end;

function TRALCSRFPlugin.IsFieldNameStored: boolean;
begin
  Result := FFieldName <> cRALCSRFField;
end;

function TRALCSRFPlugin.IsHeaderNameStored: boolean;
begin
  Result := FHeaderName <> cRALCSRFHeader;
end;

function TRALCSRFPlugin.Mac(const ANonce, ABinding: StringRAL): StringRAL;
begin
  Result := Base64Url(TRALHashes.GetHash(ANonce + '|' + ABinding, SecretKey, htSHA256));
end;

procedure TRALCSRFPlugin.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FWebModule) then
    FWebModule := nil;
  inherited Notification(AComponent, Operation);
end;

function TRALCSRFPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppProcess];
end;

procedure TRALCSRFPlugin.ProcessRequest(ARequest: TRALRequest;
  AResponse: TRALResponse; var AHandled: boolean);
var
  vCookie, vSent: StringRAL;
begin
  if ARequest.Method in [amGET, amHEAD, amOPTIONS, amTRACE, amQUERY] then
    Exit;
  if FSkipBearer and (RALPosText('bearer ', ParamOf(ARequest, 'Authorization',
     rpkHEADER)) = 1) then
    Exit;

  if not CheckOrigin(ARequest) then
  begin
    AResponse.AnswerProblem(cStatusForbidden, 'The request comes from another site.');
    AHandled := True;
    Exit;
  end;

  vCookie := ParamOf(ARequest, FCookieName, rpkCOOKIE);
  vSent := ParamOf(ARequest, FHeaderName, rpkHEADER);
  if vSent = '' then
    vSent := ParamOf(ARequest, FFieldName, rpkFIELD);
  if (vCookie = '') or (vSent = '') or (not RALSameSecret(vCookie, vSent)) or
     (not ValidToken(vCookie, BindingOf(ARequest))) then
  begin
    AResponse.AnswerProblem(cStatusForbidden, 'The CSRF token is missing or not valid.');
    AHandled := True;
  end;
end;

function TRALCSRFPlugin.SecretKey: StringRAL;
begin
  Result := FSecret;
  if Result = '' then
    Result := FAutoSecret;
end;

procedure TRALCSRFPlugin.SetAllowedOrigins(AValue: TStrings);
begin
  FAllowedOrigins.Assign(AValue);
end;

procedure TRALCSRFPlugin.SetWebModule(AValue: TRALWebModule);
begin
  if AValue = FWebModule then
    Exit;
  if FWebModule <> nil then
    FWebModule.RemoveFreeNotification(Self);
  FWebModule := AValue;
  if FWebModule <> nil then
    FWebModule.FreeNotification(Self);
end;

function TRALCSRFPlugin.Token(ARequest: TRALRequest; AResponse: TRALResponse): StringRAL;
var
  vBinding, vNonce: StringRAL;
  vCookie: TRALCookie;
begin
  vBinding := BindingOf(ARequest);
  Result := ParamOf(ARequest, FCookieName, rpkCOOKIE);
  if ValidToken(Result, vBinding) then
    Exit;

  { the token already issued for this response, when asked twice }
  vCookie := GetRALCookieFromParam(FCookieName, AResponse.Params);
  if (vCookie.Value <> '') and ValidToken(vCookie.Value, vBinding) then
  begin
    Result := vCookie.Value;
    Exit;
  end;

  vNonce := HexOf(RandomBytes(16));
  Result := vNonce + '.' + Mac(vNonce, vBinding);

  Finalize(vCookie);
  FillChar(vCookie, SizeOf(vCookie), 0);
  vCookie.Name := FCookieName;
  vCookie.Value := Result;
  vCookie.Path := '/';
  vCookie.SessionOnly := True;
  vCookie.Secure := (Server is TRALServer) and TRALServer(Server).SSLEnabled;
  vCookie.SameSite := cssLax;
  AResponse.AddCookie(vCookie);
end;

function TRALCSRFPlugin.ValidToken(const AToken, ABinding: StringRAL): boolean;
var
  vDot: IntegerRAL;
begin
  vDot := Pos('.', AToken);
  Result := (vDot = 33) and
    RALSameSecret(Copy(AToken, vDot + 1, MaxInt), Mac(Copy(AToken, 1, vDot - 1), ABinding));
end;

end.
