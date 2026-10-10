/// Unit that contains everything related to the "Request" part of the communication.
unit RALRequest;

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALParams, RALBase64, RALCustomObjects, RALTools,
  RALMIMETypes, RALStream, RALCompress, RALToken;

type

  { TRALClientInfo }

  /// Class that stores information from the client. Some of them can only be obtained with RALClient
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
    /// Which CONNECTION carried this request, as the engine identifies it.
    ///
    /// It is not an address and not a client: it is the transport underneath,
    /// and its whole point is that SEVERAL requests can share one. Under
    /// HTTP/1.1 a kept-alive socket serves them one after the other; under
    /// HTTP/2 and QUIC they travel at the same time, multiplexed, and telling
    /// the two apart is otherwise impossible from inside a handler - the
    /// requests look identical. Counting distinct values against the number of
    /// requests is what says whether multiplexing actually happened, which is
    /// also how a server tells one busy client from many.
    ///
    /// The value is only unique and only comparable WITHIN one running server:
    /// each engine hands over whatever it already has - http.sys the peer's
    /// address and port (its own ConnectionId is per stream under HTTP/2, and
    /// its RawConnectionId is not filled on every Windows), mORMot2's socket
    /// modes a counter of their own, the other engines the connection object
    /// or its handle with the peer's port above it - the handle alone came
    /// back for the next connection, and a hundred connections in a row
    /// counted as one - so it must never be persisted, sent to a client, or
    /// compared across servers. A reused connection keeps the same value for
    /// its whole life; a value may be reused after its connection is gone,
    /// once the client's port comes round again.
    ///
    /// ZERO means the engine cannot tell, which is a legitimate answer and not
    /// an error - CGI has no connection of its own, and UniGUI's belongs to
    /// UniGUI. Code that counts must skip it rather than treat it as one more.
    property ConnectionID: Int64RAL read FConnectionID write FConnectionID;
    property IP: StringRAL read FIP write FIP;
    property MACAddress: StringRAL read FMACAddress write FMACAddress;
    property Port: IntegerRAL read FPort write FPort;
    property UserAgent: StringRAL read FUserAgent write FUserAgent;
  end;

  { TRALAuthorization }

  /// Class to define which kind of authentication is used on the data
  TRALAuthorization = class(TPersistent)
  private
    FAuthString: StringRAL;
    FAuthType: TRALAuthTypes;
    FObjAuth: TObject;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CreateObjAuth;
    function GetAuthBasic: TRALAuthBasic;
    function GetAuthBearer: TRALJWT;
    procedure SetAuthString(const AValue: StringRAL);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AsAuthBasic: TRALAuthBasic read GetAuthBasic;
    property AsAuthBearer: TRALJWT read GetAuthBearer;
    property AuthString: StringRAL read FAuthString write SetAuthString;
    property AuthType: TRALAuthTypes read FAuthType write FAuthType;
  end;

  TRALRequest = class;

  /// Runs once, when the server is done with a request - see
  /// TRALRequest.AddFinishHandler
  TRALOnRequestFinish = procedure(ARequest: TRALRequest) of object;

  { TRALRequest }

  /// Class that stores everything regarding REQUEST data
  TRALRequest = class(TRALHTTPHeaderInfo)
  private
    FAuthorization: TRALAuthorization;
    FClientInfo: TRALClientInfo;
    FContentSize: Int64RAL;
    { the first two handlers live in the request itself: a request with one
      or two of them - the concurrency limit - allocates nothing for them }
    FFinish: array[0..1] of TRALOnRequestFinish;
    FFinishCount: IntegerRAL;
    FFinished: boolean;
    FFinishMore: array of TRALOnRequestFinish;
    FHost: StringRAL;
    FHttpVersion: StringRAL;
    FMethod: TRALMethod;
    FQuery: StringRAL;
    FResolvedRoute: TObject;
    FRouteData: TObject;
    FRouteOwner: TObject;
    FRouteResolved: boolean;
    FTrusted: boolean;

    function GetAuthorization: TRALAuthorization;
    function GetRoute: TCollectionItem;
    procedure ParseQueryParams(const AValue: StringRAL);
    procedure SetAuthorization(const AValue: TRALAuthorization);
    procedure SetClientInfo(const AValue: TRALClientInfo);
    procedure SetRoute(AValue: TCollectionItem);
    procedure SetRouteData(AValue: TObject);
  protected
    function GetRequestStream: TStream;
    function GetRequestText: StringRAL;
    /// Grabs the full URL of the request
    function GetURL: StringRAL;
    /// Grabs only the params after the "?" key and records it in FQuery attribute
    procedure SetQuery(const AValue: StringRAL);
    procedure SetRequestStream(const AValue: TStream); virtual; abstract;
    procedure SetRequestText(const AValue: StringRAL); virtual; abstract;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    /// Adds an UTF8 String to the body of the request.
    function AddBody(const AText: StringRAL; const AContextType: StringRAL = rctTEXTPLAIN): TRALRequest; reintroduce;
    /// Adds a string cookie to the body of the request.
    function AddCookie(const AName: StringRAL; const AValue: StringRAL): TRALRequest; reintroduce; overload;
    /// Adds a cookie to the Cookie header of the request: its Name and Value,
    /// the only part of a TRALCookie a request carries.
    function AddCookie(const ACookie: TRALCookie): TRALRequest; reintroduce; overload;
    /// Adds a string param with the "Field" kind to the request.
    function AddField(const AName: StringRAL; const AValue: StringRAL): TRALRequest; reintroduce;
    /// Adds a file to the body of the request based on the given AFileName.
    function AddFile(const AFileName: StringRAL): TRALRequest; reintroduce; overload;
    /// Adds a custom file to the body of the request from the given AStream.
    function AddFile(AStream: TStream; const AFileName: StringRAL = ''): TRALRequest; reintroduce; overload;
    /// AHandler runs when the server is done with this request: once the answer
    /// is built - encoded, compressed, encrypted - and before the engine sends
    /// it (Finish), or when the request is freed if the engine never got there
    /// (an exception). What a plugin takes for a request and must give back,
    /// whatever happens to the request, is given back here - the slot of
    /// TRALConcurrencyPlugin. Handlers run in reverse order of addition, each
    /// once; one added after Finish runs at once
    procedure AddFinishHandler(AHandler: TRALOnRequestFinish);
    /// Adds an UTF8 String to the header of the request.
    function AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALRequest; reintroduce;
    procedure Clone(ASource: TRALRequest); reintroduce;
    /// The server is done with this request: runs the handlers of
    /// AddFinishHandler, once. Every server engine calls it right after it
    /// took the response's body (TakeWireStream), so what follows - the bytes
    /// going over the network - is outside anything a handler limits. Calling
    /// it again does nothing. The first exception of a handler goes up, after
    /// all of them ran
    procedure Finish;
    /// Returns the request data in TStream format
    function GetRequestEncStream(const AEncode: boolean = true): TStream; virtual; abstract;
    /// Returns the request data in UTF8String format
    function GetRequestEncText(const AEncode: boolean = true): StringRAL; virtual; abstract;
    /// Records the route the server resolved for this request, and who
    /// answers it. Called by TRALServer.FindRoute, once per request
    procedure SetResolvedRoute(ARoute, AOwner: TObject);

    /// Whether Finish already ran
    property Finished: boolean read FFinished;
    property RequestStream: TStream read GetRequestStream write SetRequestStream;
    property RequestText: StringRAL read GetRequestText write SetRequestText;
    /// The TRALRoute that answers this request, or nil when none does. Only
    /// meaningful once RouteResolved is True: the route is looked up the first
    /// time someone asks (TRALServer.FindRoute) and kept for the rest of the
    /// request, so plugins and modules share one lookup
    property ResolvedRoute: TObject read FResolvedRoute;
    /// The route answering this request: a TRALBaseRoute of RALRoutes, which
    /// cannot be named here because RALRoutes uses this unit. It is
    /// ResolvedRoute seen as a collection item: the server resolves it -
    /// before OnRequest - and it stays nil when none answers. Its InputParams
    /// and OutputParams carry the order the route declares, which the params
    /// themselves, kept in the order they arrived, do not.
    property Route: TCollectionItem read GetRoute write SetRoute;
    /// What the module answering the request worked out while deciding to
    /// answer it, for its handler to use instead of working it out again - the
    /// WebModule keeps the file it resolved here. OWNED by the request: freed
    /// with it, or when another object is assigned. Server side only, and
    /// Clone leaves it behind
    property RouteData: TObject read FRouteData write SetRouteData;
    /// Who answers ResolvedRoute: the module (TRALModuleRoutes) that owns it,
    /// or the plugin that offered it
    property RouteOwner: TObject read FRouteOwner;
    /// Whether the route was already looked up
    property RouteResolved: boolean read FRouteResolved;
    /// Set by a white list plugin: the address is trusted, and the protections
    /// that run after it (black list, brute force, flood) leave the request alone
    property Trusted: boolean read FTrusted write FTrusted;
    property URL: StringRAL read GetURL;
  published
    /// Created the first time it is asked for: a server without
    /// authentication never decodes credentials, and never needs one
    property Authorization: TRALAuthorization read GetAuthorization write SetAuthorization;
    property ClientInfo: TRALClientInfo read FClientInfo write SetClientInfo;
    property ContentSize: Int64RAL read FContentSize write FContentSize;
    property Host: StringRAL read FHost write FHost;
    /// The SCHEME, not the version: 'HTTP' or 'HTTPS'. Which version carried
    /// the request is Protocol/ProtocolVersion, inherited from
    /// TRALHTTPHeaderInfo - the two names have always meant different things.
    property HttpVersion: StringRAL read FHttpVersion write FHttpVersion;
    property Method: TRALMethod read FMethod write FMethod;
    property Query: StringRAL read FQuery write SetQuery;
  end;


  { TRALServerRequest }

  /// Derived class to handle ServerRequest
  TRALServerRequest = class(TRALRequest)
  private
    FStream: TStream;
  protected
    procedure SetRequestStream(const AValue: TStream); override;
    procedure SetRequestText(const AValue: StringRAL); override;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    { The body that arrived, decoded - the one stream the body params read
      from, owned by the request (RequestStream). AEncode means nothing here:
      a server never re-encodes what it received }
    function GetRequestEncStream(const AEncode: boolean = true): TStream; override;
    /// The body that arrived, decoded, as text (RequestText)
    function GetRequestEncText(const AEncode: boolean = true): StringRAL; override;
    procedure SetWireBody(AStream: TStream; AOwnership: TRALBodyOwnership); override;
  end;


  { TRALClientRequest }

  /// Derived class to handle ClientRequest
  TRALClientRequest = class(TRALRequest)
  protected
    procedure SetRequestStream(const AValue: TStream); override;
    procedure SetRequestText(const AValue: StringRAL); override;
    { a form or multipart request goes out uncompressed (see
      TRALParams.EffectiveCompress), and the params stay: a retry, on another
      BaseURL or after a 401, sends them again }
    function WireCompressMultipart: boolean; override;
    function WireConsume: boolean; override;
  public
    { The body encoded for the wire, as a new stream the caller frees - what
      the engines used before TakeWireStream, which does the same without
      copying the body when there is nothing to transform }
    function GetRequestEncStream(const AEncode: boolean = true): TStream; override;
    function GetRequestEncText(const AEncode: boolean = true): StringRAL; override;
  end;

implementation

{ TRALRequest }

function TRALRequest.GetRequestStream: TStream;
begin
  Result := GetRequestEncStream;
end;

function TRALRequest.GetRequestText: StringRAL;
begin
  { the body as text, as on the response: on the client this ran gzip and AES
    and rewrote ContentType. An engine wants RequestStream }
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
  { the same parser as everywhere else (AppendParamsText): '&' only, decoded,
    empty segments skipped. A TStringList's DelimitedText also split on spaces
    and read quotes - two parsers with two answers for one query string - and
    the engines that set Query then parsed it a second time }
  Params.AppendParamsText(AValue, rpkQUERY);
end;

function TRALRequest.GetURL: StringRAL;
begin
  { scheme://host/path - the path is fixed already (SetQuery). It wrote one
    slash, 'http:/host/route', and TRALServerOAuth takes its signature base
    string over this; and an engine that left HttpVersion empty, as the CGIs
    did, made it ':/' }
  if FHttpVersion = '' then
    Result := 'http://'
  else
    Result := LowerCase(FHttpVersion) + '://';
  Result := Result + FHost + FQuery;
end;

procedure TRALRequest.SetQuery(const AValue: StringRAL);
var
  vInt: IntegerRAL;
begin
  FQuery := AValue;

  { the '?' as a StringRAL: a Char literal picked the UnicodeString overload
    of Pos on Delphi, and the whole path went to UTF-16 to be searched }
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
  { Protocol is not copied here: it is a face of ProtocolVersion, which the
    inherited Clone above already carried over }
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
  inherited;
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
  { a Cookie header is name=value pairs: Path, Expires, HttpOnly and the rest
    are what a server tells a browser. The inherited method keeps the whole
    Set-Cookie line, right for a response, and here the cookie went out as
    "Cookie: Set-Cookie=name=value" on the engines that join the params
    themselves, while GetRALCookie could not find it by its name }
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

{ The object of the credentials is made when asked for, from what AuthType
  and AuthString say then. It used to be made on every AuthString assigned -
  that is, for every request that carried credentials - and the JWT one
  parses the whole token: the authenticator parses it again to check it, so
  every Bearer request was decoded twice, a hundred allocations for nothing }
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
      { in a finally: an EncodeBody that raised left the request's params
        with no compression and no cipher for the rest of their life }
      Params.CompressType := vCompress;
      Params.CriptoOptions.CriptType := vCripto;
    end;
  end;
  Result := FStream;
end;

function TRALServerRequest.GetRequestEncText(const AEncode: boolean): StringRAL;
begin
  { a body an engine delivered as a string comes back as that string; any
    other is read once. It used to be copied into a TRALStringStream first and
    then read out of it }
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
  { the old engine entry: decoded with whatever Params says, and AValue is
    copied (SetWireBody is the one that lends) }
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
  { False: a multipart REQUEST goes out uncompressed. The server is the one that
    parses it, and a server that reads multipart natively - libmicrohttpd, under
    the Sagui engine - parses before any decompression layer, so it saw gzip
    bytes, found no parts and dropped the body without an error. Responses are
    not affected: what reads those is RAL's own client, which decompresses
    first, so they keep compressing normally. }
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
