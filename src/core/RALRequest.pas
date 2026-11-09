/// The request of a client or a server: route, query, credentials, body and client data.
unit RALRequest;

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALParams, RALBase64, RALCustomObjects, RALTools,
  RALMIMETypes, RALStream, RALCompress, RALToken;

type
  /// Segments of a path: '/api/users/1' as api, users, 1.
  TRALPathSegments = array of StringRAL;

  /// Data about the client of a request: address, port, connection and user agent.
  TRALClientInfo = class(TPersistent)
  private
    FConnectionID: Int64RAL;
    FIP: StringRAL;
    FMACAddress: StringRAL;
    FPort: IntegerRAL;
    FUserAgent: StringRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    { Connection that carried the request, as the engine identifies it; 0 when the
      engine cannot tell. Comparable only within one running server. }
    property ConnectionID: Int64RAL read FConnectionID write FConnectionID;
    /// IP address of the client.
    property IP: StringRAL read FIP write FIP;
    /// MAC address of the client; empty when the engine cannot tell.
    property MACAddress: StringRAL read FMACAddress write FMACAddress;
    /// Port of the client.
    property Port: IntegerRAL read FPort write FPort;
    /// User-Agent the client sent.
    property UserAgent: StringRAL read FUserAgent write FUserAgent;
  end;

  /// Credentials a request carries: the scheme and its text.
  TRALAuthorization = class(TPersistent)
  private
    FAuthString: StringRAL;
    FAuthType: TRALAuthTypes;
    /// Decoded credentials (TRALAuthBasic or TRALJWT), made on first read.
    FObjAuth: TObject;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    /// Makes FObjAuth from AuthType and AuthString.
    procedure CreateObjAuth;
    function GetAuthBasic: TRALAuthBasic;
    function GetAuthBearer: TRALJWT;
    procedure SetAuthString(const AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;
  published
    /// The Basic credentials, decoded; nil for another scheme.
    property AsAuthBasic: TRALAuthBasic read GetAuthBasic;
    /// The Bearer token, parsed; nil for another scheme.
    property AsAuthBearer: TRALJWT read GetAuthBearer;
    /// Credentials text of the Authorization header, after the scheme name.
    property AuthString: StringRAL read FAuthString write SetAuthString;
    /// Authentication scheme of the credentials.
    property AuthType: TRALAuthTypes read FAuthType write FAuthType;
  end;

  { Who the request is, once an authenticator accepted it: the name, the roles
    and the claims, the same for every scheme (JWT, Basic, Digest, OAuth2).
    Route and group Roles and Policy are judged against it. }
  TRALIdentity = class
  private
    FAuthScheme: TRALAuthTypes;
    FClaims: TStringList;
    FName: StringRAL;
    FPolicyPassed: boolean;
    FRoles: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    { Adds every role of AText, separated by commas or blanks; repeated ones are kept
      once. }
    procedure AddRoles(const AText: StringRAL);
    /// The value of the claim AName, or ''.
    function Claim(const AName: StringRAL): StringRAL;
    /// Forgets everything: no name, no role, no claim.
    procedure Clear;
    /// Whether the identity has the role ARole (case-insensitive).
    function HasRole(const ARole: StringRAL): boolean;
    { Whether the identity has any role of ARoles (a list separated by commas),
      or all of them with AAll; an empty list is always True. }
    function HasRoles(const ARoles: StringRAL; AAll: boolean): boolean;
    /// Sets the claim AName to AValue.
    procedure SetClaim(const AName, AValue: StringRAL);

    /// Scheme of the authenticator that accepted the request.
    property AuthScheme: TRALAuthTypes read FAuthScheme write FAuthScheme;
    /// Claims of the identity, as name=value lines.
    property Claims: TStringList read FClaims;
    /// Name of the user (the JWT sub, the Basic user...).
    property Name: StringRAL read FName write FName;
    { Set by the authorization plugin when the route's Policy passed; a route
      that names a policy is refused (403) without it. }
    property PolicyPassed: boolean read FPolicyPassed write FPolicyPassed;
    /// Roles of the identity, one per line.
    property Roles: TStringList read FRoles;
  end;

  TRALRequest = class;

  /// Runs once when the server is done with a request (TRALRequest.AddFinishHandler).
  TRALOnRequestFinish = procedure(ARequest: TRALRequest) of object;

  /// A request: method, route, query, headers, body, credentials and client data.
  TRALRequest = class(TRALHTTPHeaderInfo)
  private
    FApiVersion: StringRAL;
    /// Credentials; created on first read.
    FAuthorization: TRALAuthorization;
    FBytesSent: Int64RAL;
    FClientInfo: TRALClientInfo;
    FContentSize: Int64RAL;
    /// The first two finish handlers, kept in the request itself.
    FFinish: array[0..1] of TRALOnRequestFinish;
    /// Number of finish handlers.
    FFinishCount: IntegerRAL;
    FFinished: boolean;
    /// Finish handlers past the first two.
    FFinishMore: array of TRALOnRequestFinish;
    FHost: StringRAL;
    FHttpVersion: StringRAL;
    /// Who the request is, created on first read.
    FIdentity: TRALIdentity;
    FMethod: TRALMethod;
    /// The path FPathRaw and FPathTrimmed were split from.
    FPathKey: StringRAL;
    /// The path split as it came, for the route lookup.
    FPathRaw: TRALPathSegments;
    /// The path split and each segment trimmed.
    FPathTrimmed: TRALPathSegments;
    FQuery: StringRAL;
    FResolvedRoute: TObject;
    FResponseStatus: IntegerRAL;
    FRouteData: TObject;
    FRouteOwner: TObject;
    FRouteResolved: boolean;
    FStartTick: Int64RAL;
    FTraceId: StringRAL;
    FTrusted: boolean;

    function GetAuthorization: TRALAuthorization;
    function GetIdentity: TRALIdentity;
    function GetRoute: TCollectionItem;
    /// Adds the params of a query string, URL-decoded.
    procedure ParseQueryParams(const AValue: StringRAL);
    procedure SetAuthorization(const AValue: TRALAuthorization);
    procedure SetClientInfo(const AValue: TRALClientInfo);
    procedure SetRoute(AValue: TCollectionItem);
    procedure SetRouteData(AValue: TObject);
  protected
    function GetRequestStream: TStream;
    function GetRequestText: StringRAL;
    function GetURL: StringRAL;
    procedure SetQuery(const AValue: StringRAL);
    procedure SetRequestStream(const AValue: TStream); virtual; abstract;
    procedure SetRequestText(const AValue: StringRAL); virtual; abstract;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    /// Adds AText as the body, of AContextType; returns the request.
    function AddBody(const AText: StringRAL; const AContextType: StringRAL = rctTEXTPLAIN): TRALRequest; reintroduce;
    /// Adds a cookie; returns the request.
    function AddCookie(const AName: StringRAL; const AValue: StringRAL): TRALRequest; reintroduce; overload;
    /// Adds the name and value of ACookie as a cookie; returns the request.
    function AddCookie(const ACookie: TRALCookie): TRALRequest; reintroduce; overload;
    /// Adds a form field; returns the request.
    function AddField(const AName: StringRAL; const AValue: StringRAL): TRALRequest; reintroduce;
    /// Adds the file AFileName to the body; returns the request.
    function AddFile(const AFileName: StringRAL): TRALRequest; reintroduce; overload;
    /// Adds AStream to the body as a file named AFileName; returns the request.
    function AddFile(AStream: TStream; const AFileName: StringRAL = ''): TRALRequest; reintroduce; overload;
    { AHandler runs once when the server is done with the request (Finish), in
      reverse order of addition; added after Finish, it runs at once. }
    procedure AddFinishHandler(AHandler: TRALOnRequestFinish);
    /// Adds a header; returns the request.
    function AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALRequest; reintroduce;
    /// Copies the data of this request into ASource.
    procedure Clone(ASource: TRALRequest); reintroduce;
    { Runs the finish handlers, once; every server engine calls it after taking
      the response body. The first exception of a handler goes up after all ran. }
    procedure Finish;
    /// The body as a stream; on a client, encoded for the wire when AEncode.
    function GetRequestEncStream(const AEncode: boolean = true): TStream; virtual; abstract;
    /// The body as text; on a client, encoded for the wire when AEncode.
    function GetRequestEncText(const AEncode: boolean = true): StringRAL; virtual; abstract;
    /// True when an Identity was created (an authenticator filled it).
    function HasIdentity: boolean;
    /// Keeps the path APath split as ARaw and ATrimmed, for the next route lookup.
    procedure SetPathSegments(const APath: StringRAL; const ARaw, ATrimmed: TRALPathSegments);
    /// Records the route that answers the request, and its owner; called once.
    procedure SetResolvedRoute(ARoute, AOwner: TObject);
    { The segments SetPathSegments kept, when they were split from APath: every
      module looks the route up, and the path is split once per request. }
    function TryGetPathSegments(const APath: StringRAL; out ARaw,
      ATrimmed: TRALPathSegments): boolean;

    { Version of the API the request asked for (TRALServer.ApiVersioning), or the
      default one; empty when the server does not version its API. }
    property ApiVersion: StringRAL read FApiVersion write FApiVersion;
    { Bytes of the response body that went out, known when the finish handlers
      run; -1 for a streamed response (Send), still going. }
    property BytesSent: Int64RAL read FBytesSent write FBytesSent;
    /// True once Finish ran.
    property Finished: boolean read FFinished;
    { Who the request is, filled by the authenticator that accepted it; created
      on first read, empty when nobody authenticated the request. }
    property Identity: TRALIdentity read GetIdentity;
    /// The body: on a server the one that arrived, decoded; on a client the one to send.
    property RequestStream: TStream read GetRequestStream write SetRequestStream;
    /// The body as text.
    property RequestText: StringRAL read GetRequestText write SetRequestText;
    /// Route (a TRALRoute) that answers the request, or nil; valid once RouteResolved.
    property ResolvedRoute: TObject read FResolvedRoute;
    /// Status of the response, known when the finish handlers run; 0 before.
    property ResponseStatus: IntegerRAL read FResponseStatus write FResponseStatus;
    /// The answering route as a collection item (a TRALBaseRoute), or nil.
    property Route: TCollectionItem read GetRoute write SetRoute;
    /// What the answering module keeps for its handler; owned by the request.
    property RouteData: TObject read FRouteData write SetRouteData;
    /// Module or plugin that answers ResolvedRoute.
    property RouteOwner: TObject read FRouteOwner;
    /// True once the route was looked up.
    property RouteResolved: boolean read FRouteResolved;
    /// RALTickMicro when the server took the request (ValidateRequest); 0 on a client.
    property StartTick: Int64RAL read FStartTick write FStartTick;
    { Trace id of the request (W3C trace context, set by a tracing plugin), used by
      the log and the error body; empty without one. }
    property TraceId: StringRAL read FTraceId write FTraceId;
    /// Set by the white list: the protections after it leave the request alone.
    property Trusted: boolean read FTrusted write FTrusted;
    /// Full URL of the request: scheme, host and path.
    property URL: StringRAL read GetURL;
  published
    /// Credentials of the request; created on first read.
    property Authorization: TRALAuthorization read GetAuthorization write SetAuthorization;
    /// Data about the client.
    property ClientInfo: TRALClientInfo read FClientInfo write SetClientInfo;
    /// Size of the body, as declared by the client.
    property ContentSize: Int64RAL read FContentSize write FContentSize;
    /// Host the request was sent to.
    property Host: StringRAL read FHost write FHost;
    /// Scheme of the request, 'HTTP' or 'HTTPS'; the version is ProtocolVersion.
    property HttpVersion: StringRAL read FHttpVersion write FHttpVersion;
    /// HTTP method.
    property Method: TRALMethod read FMethod write FMethod;
    /// Path of the request; the query string after '?' goes to the params.
    property Query: StringRAL read FQuery write SetQuery;
  end;


  /// Request as a server receives it.
  TRALServerRequest = class(TRALRequest)
  private
    /// Body assembled from the params when none went through the decoder.
    FStream: TStream;
  protected
    procedure SetRequestStream(const AValue: TStream); override;
    procedure SetRequestText(const AValue: StringRAL); override;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    /// The body that arrived, decoded and owned by the request; AEncode is ignored.
    function GetRequestEncStream(const AEncode: boolean = true): TStream; override;
    /// The body that arrived, decoded, as text.
    function GetRequestEncText(const AEncode: boolean = true): StringRAL; override;
    procedure SetWireBody(AStream: TStream; AOwnership: TRALBodyOwnership); override;
  end;


  /// Request as a client sends it.
  TRALClientRequest = class(TRALRequest)
  protected
    procedure SetRequestStream(const AValue: TStream); override;
    procedure SetRequestText(const AValue: StringRAL); override;
    /// False: a form or multipart request goes out uncompressed.
    function WireCompressMultipart: boolean; override;
    /// False: the params stay, so a retry sends them again.
    function WireConsume: boolean; override;
  public
    /// The body for the wire (plain unless AEncode), as a new stream the caller frees.
    function GetRequestEncStream(const AEncode: boolean = true): TStream; override;
    function GetRequestEncText(const AEncode: boolean = true): StringRAL; override;
  end;

implementation

{ TRALIdentity }

constructor TRALIdentity.Create;
begin
  inherited Create;
  FRoles := TStringList.Create;
  FClaims := TStringList.Create;
  FAuthScheme := ratNone;
end;

destructor TRALIdentity.Destroy;
begin
  FreeAndNil(FRoles);
  FreeAndNil(FClaims);
  inherited;
end;

procedure TRALIdentity.AddRoles(const AText: StringRAL);
var
  vInt, vStart: IntegerRAL;
  vRole: StringRAL;
begin
  vStart := POSINISTR;
  for vInt := POSINISTR to RALHighStr(AText) + 1 do
  begin
    if (vInt > RALHighStr(AText)) or (AText[vInt] in [',', ' ', ';', #9]) then
    begin
      vRole := Copy(AText, vStart, vInt - vStart);
      if (vRole <> '') and not HasRole(vRole) then
        FRoles.Add(string(vRole));
      vStart := vInt + 1;
    end;
  end;
end;

function TRALIdentity.Claim(const AName: StringRAL): StringRAL;
begin
  Result := StringRAL(FClaims.Values[string(AName)]);
end;

procedure TRALIdentity.Clear;
begin
  FName := '';
  FAuthScheme := ratNone;
  FPolicyPassed := False;
  FRoles.Clear;
  FClaims.Clear;
end;

function TRALIdentity.HasRole(const ARole: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 0 to FRoles.Count - 1 do
    if RALSameName(StringRAL(FRoles[vInt]), ARole) then
    begin
      Result := True;
      Exit;
    end;
end;

function TRALIdentity.HasRoles(const ARoles: StringRAL; AAll: boolean): boolean;
var
  vInt, vStart: IntegerRAL;
  vRole: StringRAL;
  vAny: boolean;
begin
  Result := True;
  vAny := False;
  vStart := POSINISTR;
  for vInt := POSINISTR to RALHighStr(ARoles) + 1 do
  begin
    if (vInt > RALHighStr(ARoles)) or (ARoles[vInt] in [',', ' ', ';', #9]) then
    begin
      vRole := Copy(ARoles, vStart, vInt - vStart);
      vStart := vInt + 1;
      if vRole = '' then
        Continue;
      vAny := True;
      if HasRole(vRole) <> AAll then
      begin
        { any: the first one found is enough; all: the first one missing is
          enough to refuse }
        Result := not AAll;
        Exit;
      end;
    end;
  end;
  { all: none was missing; any: none was found, unless the list was empty }
  Result := AAll or not vAny;
end;

procedure TRALIdentity.SetClaim(const AName, AValue: StringRAL);
begin
  FClaims.Values[string(AName)] := string(AValue);
end;

{ TRALRequest }

function TRALRequest.GetRequestStream: TStream;
begin
  Result := GetRequestEncStream;
end;

function TRALRequest.GetRequestText: StringRAL;
begin
  // the plain body as text; an engine reads RequestStream
  Result := GetRequestEncText(False);
end;

function TRALRequest.GetAuthorization: TRALAuthorization;
begin
  if FAuthorization = nil then
    FAuthorization := TRALAuthorization.Create;
  Result := FAuthorization;
end;

procedure TRALRequest.SetAuthorization(const AValue: TRALAuthorization);
begin
  if AValue <> nil then
    RALAssignOwned(GetAuthorization, AValue);
end;

procedure TRALRequest.SetClientInfo(const AValue: TRALClientInfo);
begin
  RALAssignOwned(FClientInfo, AValue);
end;

function TRALRequest.GetRoute: TCollectionItem;
begin
  if FResolvedRoute is TCollectionItem then
    Result := TCollectionItem(FResolvedRoute)
  else
    Result := nil;
end;

procedure TRALRequest.SetRoute(AValue: TCollectionItem);
begin
  FResolvedRoute := AValue;
end;

procedure TRALRequest.SetRouteData(AValue: TObject);
begin
  if AValue = FRouteData then
    Exit;
  FRouteData.Free;
  FRouteData := AValue;
end;

procedure TRALRequest.ParseQueryParams(const AValue: StringRAL);
begin
  // the parser of every query string: '&' only, decoded, empty segments skipped
  Params.AppendParamsText(AValue, rpkQUERY);
end;

function TRALRequest.GetURL: StringRAL;
begin
  // scheme://host/path; no scheme means http
  if FHttpVersion = '' then
    Result := 'http://'
  else
    Result := LowerCase(FHttpVersion) + '://';
  Result := Result + FHost + FQuery;
end;

procedure TRALRequest.SetPathSegments(const APath: StringRAL; const ARaw,
  ATrimmed: TRALPathSegments);
begin
  FPathKey := APath;
  FPathRaw := ARaw;
  FPathTrimmed := ATrimmed;
end;

function TRALRequest.TryGetPathSegments(const APath: StringRAL; out ARaw,
  ATrimmed: TRALPathSegments): boolean;
begin
  // the same string, the common case, before comparing the text
  Result := ((Pointer(APath) = Pointer(FPathKey)) and (FPathRaw <> nil)) or
    ((FPathRaw <> nil) and (APath = FPathKey));
  if Result then
  begin
    ARaw := FPathRaw;
    ATrimmed := FPathTrimmed;
  end;
end;

procedure TRALRequest.SetQuery(const AValue: StringRAL);
var
  vInt: IntegerRAL;
begin
  FQuery := AValue;

  // StringRAL('?'): a Char literal takes Delphi's UTF-16 Pos
  vInt := Pos(StringRAL('?'), FQuery);
  if vInt > 0 then
  begin
    ParseQueryParams(Copy(FQuery, vInt + 1, Length(FQuery)));
    Delete(FQuery, vInt, Length(FQuery));
  end;

  FQuery := FixRoute(FQuery);
end;

procedure TRALRequest.SetResolvedRoute(ARoute, AOwner: TObject);
begin
  FResolvedRoute := ARoute;
  FRouteOwner := AOwner;
  FRouteResolved := True;
end;

procedure TRALRequest.Clone(ASource: TRALRequest);
begin
  inherited Clone(ASource);

  ASource.Authorization.Assign(Self.Authorization);
  ASource.ClientInfo.Assign(Self.ClientInfo);
  ASource.ContentSize := Self.ContentSize;
  ASource.Host := Self.Host;
  ASource.HttpVersion := Self.HttpVersion;
  ASource.Method := Self.Method;
  // Protocol is a face of ProtocolVersion, copied by the inherited Clone
  ASource.Query := Self.Query;
  ASource.Route := Self.Route;
end;

constructor TRALRequest.Create(AOwner: TObject);
begin
  inherited;
  FAuthorization := nil; // on first use, see the property
  FClientInfo := TRALClientInfo.Create;
  FContentSize := 0;
end;

destructor TRALRequest.Destroy;
begin
  { the safety net: an engine that raised before Finish still gives back what
    the plugins took. A destructor must not raise }
  if not FFinished then
    try
      Finish;
    except
      // the request is going away; nothing is left to answer it
    end;
  FreeAndNil(FRouteData);
  FreeAndNil(FClientInfo);
  FreeAndNil(FAuthorization);
  FreeAndNil(FIdentity);
  inherited;
end;

function TRALRequest.GetIdentity: TRALIdentity;
begin
  if FIdentity = nil then
    FIdentity := TRALIdentity.Create;
  Result := FIdentity;
end;

function TRALRequest.HasIdentity: boolean;
begin
  Result := FIdentity <> nil;
end;

procedure TRALRequest.AddFinishHandler(AHandler: TRALOnRequestFinish);
begin
  if not Assigned(AHandler) then
    Exit;
  if FFinished then
  begin
    AHandler(Self);
    Exit;
  end;
  if FFinishCount <= High(FFinish) then
    FFinish[FFinishCount] := AHandler
  else
  begin
    SetLength(FFinishMore, FFinishCount - Length(FFinish) + 1);
    FFinishMore[High(FFinishMore)] := AHandler;
  end;
  Inc(FFinishCount);
end;

procedure TRALRequest.Finish;
var
  vInt: IntegerRAL;
  vHandler: TRALOnRequestFinish;
  vError: TObject;
begin
  if FFinished then
    Exit;
  { set first: a handler that frees the request, or calls Finish again, must
    not run the list twice }
  FFinished := True;
  vError := nil;
  for vInt := FFinishCount - 1 downto 0 do
  begin
    if vInt <= High(FFinish) then
      vHandler := FFinish[vInt]
    else
      vHandler := FFinishMore[vInt - Length(FFinish)];
    try
      vHandler(Self);
    except
      { every handler runs - each gives back something of its own - and the
        first failure is the one that goes up }
      if vError = nil then
        vError := TObject(AcquireExceptionObject);
    end;
  end;
  FFinishCount := 0;
  FFinishMore := nil;
  if vError <> nil then
    raise vError;
end;

function TRALRequest.AddHeader(const AName: StringRAL; const AValue: StringRAL
  ): TRALRequest;
begin
  inherited AddHeader(AName, AValue);
  Result := Self;
end;

function TRALRequest.AddBody(const AText: StringRAL; const AContextType: StringRAL)
  : TRALRequest;
begin
  inherited AddBody(AText, AContextType);
  Result := Self;
end;

function TRALRequest.AddField(const AName: StringRAL; const AValue: StringRAL
  ): TRALRequest;
begin
  inherited AddField(AName, AValue);
  Result := Self;
end;

function TRALRequest.AddCookie(const AName: StringRAL; const AValue: StringRAL
  ): TRALRequest;
begin
  inherited AddCookie(AName, AValue);
  Result := Self;
end;

function TRALRequest.AddCookie(const ACookie: TRALCookie): TRALRequest;
begin
  // a Cookie header carries name and value only; the attributes are a server's
  Params.AddParam(ACookie.Name, ACookie.Value, rpkCOOKIE);
  Result := Self;
end;

function TRALRequest.AddFile(const AFileName: StringRAL): TRALRequest;
begin
  inherited AddFile(AFileName);
  Result := Self;
end;

function TRALRequest.AddFile(AStream: TStream; const AFileName: StringRAL): TRALRequest;
begin
  inherited AddFile(AStream, AFileName);
  Result := Self;
end;

{ TRALAuthorization }

procedure TRALAuthorization.AssignTo(Dest: TPersistent);
var
  vDest : TRALAuthorization;
begin
  vDest := TRALAuthorization(Dest);
  vDest.AuthType := Self.AuthType;
  vDest.AuthString := Self.AuthString;
end;

constructor TRALAuthorization.Create;
begin
  inherited;
  FAuthType := ratNone;
  FAuthString := '';
  FObjAuth := nil;
end;

procedure TRALAuthorization.CreateObjAuth;
begin
  if FObjAuth <> nil then
    FreeAndNil(FObjAuth);

  if FAuthType = ratBasic then begin
    FObjAuth := TRALAuthBasic.Create;
    TRALAuthBasic(FObjAuth).AuthString := FAuthString;
  end
  else if FAuthType = ratBearer then begin
    FObjAuth := TRALJWT.Create;
    TRALJWT(FObjAuth).Token := FAuthString;
  end;
end;

destructor TRALAuthorization.Destroy;
begin
  if FObjAuth <> nil then
    FreeAndNil(FObjAuth);

  inherited;
end;

// the decoded credentials are made when first asked for
function TRALAuthorization.GetAuthBasic: TRALAuthBasic;
begin
  Result := nil;
  if FAuthType <> ratBasic then
    Exit;
  if not (FObjAuth is TRALAuthBasic) then
    CreateObjAuth;
  Result := TRALAuthBasic(FObjAuth);
end;

function TRALAuthorization.GetAuthBearer: TRALJWT;
begin
  Result := nil;
  if FAuthType <> ratBearer then
    Exit;
  if not (FObjAuth is TRALJWT) then
    CreateObjAuth;
  Result := TRALJWT(FObjAuth);
end;

procedure TRALAuthorization.SetAuthString(const AValue: StringRAL);
begin
  FAuthString := AValue;
  { made again from the new string the next time it is asked for }
  FreeAndNil(FObjAuth);
end;

{ TRALServerRequest }

constructor TRALServerRequest.Create(AOwner: TObject);
begin
  inherited;
  FStream := nil;
end;

destructor TRALServerRequest.Destroy;
begin
  if FStream <> nil then
    FreeAndNil(FStream);
  inherited;
end;

function TRALServerRequest.GetRequestEncStream(const AEncode: boolean): TStream;
var
  vContentType, vContentDisposition: StringRAL;
  vCompress: TRALCompressType;
  vCripto: TRALCriptoType;
begin
  { the body as it came, decoded once: no copy, and not a multipart put back
    together with another boundary - the bytes that arrived }
  Result := Params.Decoded;
  if Result <> nil then
  begin
    Result.Position := 0;
    Exit;
  end;

  { No body went through the decoder: the engine split it itself (Sagui hands
    over the form fields) or somebody filled the params by hand. Assembled
    from the params, once, and kept - a copy, so it survives whatever the
    handler does to them afterwards }
  if FStream = nil then
  begin
    vCompress := Params.CompressType;
    vCripto := Params.CriptoOptions.CriptType;

    Params.CompressType := ctNone;
    Params.CriptoOptions.CriptType := crNone;
    try
      FStream := Params.EncodeBody(vContentType, vContentDisposition);
    finally
      // in a finally, so the params keep their compression and cipher
      Params.CompressType := vCompress;
      Params.CriptoOptions.CriptType := vCripto;
    end;
  end;
  Result := FStream;
end;

function TRALServerRequest.GetRequestEncText(const AEncode: boolean): StringRAL;
begin
  // a body delivered as a string comes back as that string
  Result := RALStreamText(GetRequestEncStream(AEncode));
end;

procedure TRALServerRequest.SetWireBody(AStream: TStream;
  AOwnership: TRALBodyOwnership);
begin
  FreeAndNil(FStream);
  inherited;
end;

procedure TRALServerRequest.SetRequestStream(const AValue: TStream);
begin
  // decoded with what Params says; AValue is copied (SetWireBody lends)
  FreeAndNil(FStream);
  Params.DecodeBody(AValue, ContentType, ContentDisposition);
end;

procedure TRALServerRequest.SetRequestText(const AValue: StringRAL);
begin
  FreeAndNil(FStream);
  Params.DecodeBody(AValue, ContentType, ContentDisposition);
end;

{ TRALClientRequest }

function TRALClientRequest.GetRequestEncStream(const AEncode: boolean): TStream;
var
  vContentType, vContentDisposition: StringRAL;
  vCompress: TRALCompressType;
  vCripto: TRALCriptoType;
begin
  if not AEncode then
  begin
    { the plain body, and nothing touched: no cipher key wiped, no
      ContentType or ContentCompress rewritten }
    vCompress := Params.CompressType;
    vCripto := Params.CriptoOptions.CriptType;
    Params.CompressType := ctNone;
    Params.CriptoOptions.CriptType := crNone;
    try
      Result := Params.EncodeBody(vContentType, vContentDisposition, False);
    finally
      Params.CompressType := vCompress;
      Params.CriptoOptions.CriptType := vCripto;
    end;
    Exit;
  end;

  Params.CriptoOptions.CriptType := ContentCripto;
  Params.CriptoOptions.Key := CriptoKey;
  Params.CompressType := ContentCompress;
  // False: a form or multipart request goes uncompressed (TRALParams.EffectiveCompress)
  Result := Params.EncodeBody(vContentType, vContentDisposition, False);
  ContentType := vContentType;
  ContentDisposition := vContentDisposition;
  { and the header says what was actually done, not what was asked for }
  ContentCompress := Params.CompressType;
end;

function TRALClientRequest.WireCompressMultipart: boolean;
begin
  Result := False;
end;

function TRALClientRequest.WireConsume: boolean;
begin
  Result := False;
end;

function TRALClientRequest.GetRequestEncText(const AEncode: boolean): StringRAL;
var
  vStream: TStream;
begin
  vStream := GetRequestEncStream(AEncode);
  try
    Result := StreamToString(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure TRALClientRequest.SetRequestStream(const AValue: TStream);
var
  vParam: TRALParam;
begin
  Params.ClearParams(rpkBODY);
  if AValue.Size > 0 then
  begin
    vParam := Params.AddValue(AValue, rpkBODY);
    vParam.ContentType := ContentType;
  end;
end;

procedure TRALClientRequest.SetRequestText(const AValue: StringRAL);
var
  vParam: TRALParam;
begin
  Params.ClearParams(rpkBODY);
  if AValue <> '' then
  begin
    vParam := Params.AddValue(AValue);
    vParam.ContentType := ContentType;
    vParam.Kind := rpkBODY;
  end;
end;

{ TRALClientInfo }

procedure TRALClientInfo.AssignTo(Dest: TPersistent);
var
  vDest : TRALClientInfo;
begin
  vDest := TRALClientInfo(Dest);
  vDest.ConnectionID := Self.ConnectionID;
  vDest.IP := Self.IP;
  vDest.MACAddress := Self.MACAddress;
  vDest.Port := Self.Port;
  vDest.UserAgent := Self.UserAgent;
end;

end.
