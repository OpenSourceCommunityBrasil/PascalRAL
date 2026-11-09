{ The response cache and ETags of routes (TRALResponseCachePlugin): GET and
  HEAD answers of the routes in CacheRoutes are kept for a while and answered
  again without running the route; with DynamicETag every GET answer gets a
  strong ETag and a matching If-None-Match gets 304. }
unit RALResponseCache;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALPlugin, RALRequest, RALResponse,
  RALRoutes, RALParams, RALKeyValueStore, RALSHA1, RALHashBase;

const
  /// Priority of the response cache: after authentication, before the routes.
  RALPriorityResponseCache = 150;

type
  /// The routes under Path are cached for TTL seconds.
  TRALCacheRoute = class(TCollectionItem)
  private
    FPath: StringRAL;
    /// Route split into segments, for the match.
    FPattern: TRALRouteSegments;
    FTTL: IntegerRAL;

    procedure SetPath(const AValue: StringRAL);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
  published
    /// Paths of the cached routes ('/products', '/api/*/list').
    property Path: StringRAL read FPath write SetPath;
    /// Seconds an answer is kept.
    property TTL: IntegerRAL read FTTL write FTTL default 60;
  end;

  /// The cached routes of a TRALResponseCachePlugin.
  TRALCacheRoutes = class(TOwnedCollection)
  public
    /// Adds the routes under APath, kept ATTL seconds.
    function AddRoute(const APath: StringRAL; ATTL: IntegerRAL): TRALCacheRoute;
  end;

  { Keeps the 200 answers of GET and HEAD to CacheRoutes in Store and answers them
    from there (X-Cache: HIT, Age) until their TTL ends. The key is the path, the
    query and the headers in VaryHeaders. A request with Authorization or a
    Cookie is never served nor kept (CacheAuthorized changes that for the routes
    that answer every user alike). DynamicETag gives every GET answer of up to
    MaxBodySize bytes a strong ETag (SHA-1 of the body) and answers 304 to an
    If-None-Match that names it. }
  TRALResponseCachePlugin = class(TRALPlugin)
  private
    FCacheAuthorized: boolean;
    FCacheRoutes: TRALCacheRoutes;
    FDynamicETag: boolean;
    FMaxBodySize: IntegerRAL;
    /// Store used when Store is not assigned.
    FOwnStore: TRALMemoryStore;
    FPrefix: StringRAL;
    FStore: TRALKeyValueStore;
    FVaryHeaders: TStrings;

    /// Store, or the internal one.
    function ActiveStore: TRALKeyValueStore;
    /// Whether ARequest may be served from and kept in the cache.
    function Cacheable(ARequest: TRALRequest): boolean;
    /// The cached route ARequest falls under, or nil.
    function CacheRouteOf(ARequest: TRALRequest): TRALCacheRoute;
    /// Key of ARequest in the store.
    function KeyOf(ARequest: TRALRequest): StringRAL;
    { Keeps the answer, sets the ETag, turns it into a 304 (AContext: what the request
      carried). }
    procedure ResponseReady(AResponse: TRALResponse; AContext: TObject);
    procedure SetCacheRoutes(const AValue: TRALCacheRoutes);
    procedure SetStore(const AValue: TRALKeyValueStore);
    procedure SetVaryHeaders(const AValue: TStrings);
  protected
    class function DefaultPriority: IntegerRAL; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Forgets every kept answer of this plugin's Store (its own memory store).
    procedure Clear;
    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
  published
    /// Serves and keeps answers of requests with Authorization or a Cookie.
    property CacheAuthorized: boolean read FCacheAuthorized write FCacheAuthorized default False;
    /// The routes whose answers are kept.
    property CacheRoutes: TRALCacheRoutes read FCacheRoutes write SetCacheRoutes;
    /// A strong ETag on every GET answer, and 304 for If-None-Match.
    property DynamicETag: boolean read FDynamicETag write FDynamicETag default True;
    /// Bytes of a body the cache keeps and the ETag hashes; larger ones are left alone.
    property MaxBodySize: IntegerRAL read FMaxBodySize write FMaxBodySize default 1048576;
    /// Prefix of the keys in Store.
    property Prefix: StringRAL read FPrefix write FPrefix;
    /// Where the answers live; empty = memory of this plugin.
    property Store: TRALKeyValueStore read FStore write SetStore;
    /// Request headers that are part of the key (Accept, Accept-Language...).
    property VaryHeaders: TStrings read FVaryHeaders write SetVaryHeaders;
  end;

implementation

/// Text of the header AName of AParams; empty when absent.
function HeaderText(AParams: TRALParams; const AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := AParams.GetKind[AName, rpkHEADER];
  if vParam <> nil then
    Result := vParam.AsString;
end;

/// Whether the If-None-Match of a request names AETag (or is *).
function ETagMatches(const AIfNoneMatch, AETag: StringRAL): boolean;
var
  vList, vTag: StringRAL;
  vPos: IntegerRAL;
begin
  Result := False;
  vList := RALTrim(AIfNoneMatch);
  if vList = '' then
    Exit;
  if vList = '*' then
  begin
    Result := True;
    Exit;
  end;
  while vList <> '' do
  begin
    vPos := Pos(StringRAL(','), vList);
    if vPos = 0 then
      vPos := Length(vList) + 1;
    vTag := RALTrim(Copy(vList, 1, vPos - 1));
    Delete(vList, 1, vPos);
    // the weak comparison of RFC 9110 13.1.2: W/ does not count
    if Copy(vTag, 1, 2) = 'W/' then
      Delete(vTag, 1, 2);
    if vTag = AETag then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

{ An entry as kept in the store: status, content type, ETag, time kept (unix ms)
  and body, the first four ending in #10. }
function PackEntry(AStatus: IntegerRAL; const AType, AETag: StringRAL; AKept: Int64RAL;
  const ABody: RawByteString): RawByteString;
begin
  Result := RawByteString(IntToStr(AStatus)) + #10 + RawByteString(AType) + #10 +
    RawByteString(AETag) + #10 + RawByteString(IntToStr(AKept)) + #10 + ABody;
end;

/// Reads a stored answer: status, type, ETag, when kept and body; False when damaged.
function UnpackEntry(const AValue: RawByteString; out AStatus: IntegerRAL;
  out AType, AETag: StringRAL; out AKept: Int64RAL; out ABody: RawByteString): boolean;
var
  vRest: RawByteString;
  vParts: array[0..3] of RawByteString;
  vInt, vPos: IntegerRAL;
begin
  Result := False;
  vRest := AValue;
  for vInt := 0 to 3 do
  begin
    vPos := Pos(#10, string(vRest));
    if vPos = 0 then
      Exit;
    vParts[vInt] := Copy(vRest, 1, vPos - 1);
    Delete(vRest, 1, vPos);
  end;
  AStatus := StrToIntDef(string(vParts[0]), 0);
  AType := StringRAL(vParts[1]);
  AETag := StringRAL(vParts[2]);
  AKept := StrToInt64Def(string(vParts[3]), 0);
  ABody := vRest;
  Result := AStatus > 0;
end;

{ TRALCacheRoute }

constructor TRALCacheRoute.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FTTL := 60;
end;

procedure TRALCacheRoute.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALCacheRoute then
  begin
    TRALCacheRoute(Dest).Path := FPath;
    TRALCacheRoute(Dest).TTL := FTTL;
  end
  else
    inherited AssignTo(Dest);
end;

function TRALCacheRoute.GetDisplayName: string;
begin
  Result := string(FPath) + ' = ' + IntToStr(FTTL) + 's';
end;

procedure TRALCacheRoute.SetPath(const AValue: StringRAL);
begin
  FPath := AValue;
  FPattern := RALPathSegments(AValue);
end;

{ TRALCacheRoutes }

function TRALCacheRoutes.AddRoute(const APath: StringRAL; ATTL: IntegerRAL): TRALCacheRoute;
begin
  Result := TRALCacheRoute(Add);
  Result.Path := APath;
  Result.TTL := ATTL;
end;

{ TRALResponseCachePlugin }

constructor TRALResponseCachePlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwnStore := TRALMemoryStore.Create(nil);
  FCacheRoutes := TRALCacheRoutes.Create(Self, TRALCacheRoute);
  FVaryHeaders := TStringList.Create;
  FVaryHeaders.Add('Accept');
  FDynamicETag := True;
  FMaxBodySize := 1048576;
  FPrefix := 'ral:cache:';
end;

destructor TRALResponseCachePlugin.Destroy;
begin
  FreeAndNil(FVaryHeaders);
  FreeAndNil(FCacheRoutes);
  FreeAndNil(FOwnStore);
  inherited Destroy;
end;

class function TRALResponseCachePlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityResponseCache;
end;

function TRALResponseCachePlugin.Phases: TRALPluginPhases;
begin
  Result := [ppProcess];
end;

procedure TRALResponseCachePlugin.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FStore) then
    FStore := nil;
  inherited;
end;

procedure TRALResponseCachePlugin.SetCacheRoutes(const AValue: TRALCacheRoutes);
begin
  FCacheRoutes.Assign(AValue);
end;

procedure TRALResponseCachePlugin.SetStore(const AValue: TRALKeyValueStore);
begin
  if FStore <> nil then
    FStore.RemoveFreeNotification(Self);
  FStore := AValue;
  if FStore <> nil then
    FStore.FreeNotification(Self);
end;

procedure TRALResponseCachePlugin.SetVaryHeaders(const AValue: TStrings);
begin
  FVaryHeaders.Assign(AValue);
end;

function TRALResponseCachePlugin.ActiveStore: TRALKeyValueStore;
begin
  if FStore <> nil then
    Result := FStore
  else
    Result := FOwnStore;
end;

procedure TRALResponseCachePlugin.Clear;
begin
  ActiveStore.Clear;
end;

function TRALResponseCachePlugin.CacheRouteOf(ARequest: TRALRequest): TRALCacheRoute;
var
  vRaw, vPath: TRALRouteSegments;
  vInt: IntegerRAL;
begin
  Result := nil;
  if FCacheRoutes.Count = 0 then
    Exit;
  RALSplitRequestPath(ARequest, vRaw, vPath);
  for vInt := 0 to FCacheRoutes.Count - 1 do
    if RALPathUnder(vPath, TRALCacheRoute(FCacheRoutes.Items[vInt]).FPattern) then
    begin
      Result := TRALCacheRoute(FCacheRoutes.Items[vInt]);
      Exit;
    end;
end;

function TRALResponseCachePlugin.Cacheable(ARequest: TRALRequest): boolean;
begin
  Result := (ARequest.Method in [amGET, amHEAD]) and
    (FCacheAuthorized or ((HeaderText(ARequest.Params, 'Authorization') = '') and
     (HeaderText(ARequest.Params, 'Cookie') = '')));
end;

function TRALResponseCachePlugin.KeyOf(ARequest: TRALRequest): StringRAL;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  // HEAD reads the answer of GET
  Result := FPrefix + ARequest.Query + '?';
  for vInt := 0 to ARequest.Params.Count - 1 do
  begin
    vParam := ARequest.Params.Index[vInt];
    if vParam.Kind = rpkQUERY then
      Result := Result + vParam.ParamName + '=' + vParam.AsString + '&';
  end;
  for vInt := 0 to FVaryHeaders.Count - 1 do
    Result := Result + '|' + HeaderText(ARequest.Params, StringRAL(FVaryHeaders[vInt]));
end;

type
  { What the answer of a request needs from it, taken in ProcessRequest: the
    server clears the request params before the answer goes out. Its wire
    handler does the work, its finish handler frees it. }
  TRALCachePending = class
  public
    /// The answer may be stored.
    Cacheable: boolean;
    /// If-None-Match the request carried.
    IfNoneMatch: StringRAL;
    /// Cache key of the request.
    Key: StringRAL;
    /// The plugin that took the request.
    Plugin: TRALResponseCachePlugin;
    /// Cache rule that matched.
    Route: TRALCacheRoute;

    /// Finish handler: frees the object.
    procedure Done(ARequest: TRALRequest);
    /// Wire handler: stores the answer or turns it into a 304.
    procedure Ready(AResponse: TRALResponse);
  end;

procedure TRALCachePending.Done(ARequest: TRALRequest);
begin
  Free;
end;

procedure TRALCachePending.Ready(AResponse: TRALResponse);
begin
  Plugin.ResponseReady(AResponse, Self);
end;

procedure TRALResponseCachePlugin.ProcessRequest(ARequest: TRALRequest;
  AResponse: TRALResponse; var AHandled: boolean);
var
  vRoute: TRALCacheRoute;
  vValue, vBody: RawByteString;
  vStatus: IntegerRAL;
  vType, vETag: StringRAL;
  vKept: Int64RAL;
  vPending: TRALCachePending;
begin
  if not (ARequest.Method in [amGET, amHEAD]) then
    Exit;
  vRoute := CacheRouteOf(ARequest);
  if (vRoute <> nil) and Cacheable(ARequest) and
     ActiveStore.Get(KeyOf(ARequest), vValue) and
     UnpackEntry(vValue, vStatus, vType, vETag, vKept, vBody) then
  begin
    AHandled := True;
    if (vETag <> '') and ETagMatches(HeaderText(ARequest.Params, 'If-None-Match'), vETag) then
      AResponse.Answer(HTTP_NotModified, '', vType)
    else
      AResponse.Answer(vStatus, StringRAL(vBody), vType);
    if vETag <> '' then
      AResponse.AddHeader('ETag', vETag);
    AResponse.AddHeader('X-Cache', 'HIT');
    AResponse.AddHeader('Age', StringRAL(IntToStr((ActiveStore.Current - vKept) div 1000)));
    Exit;
  end;
  // the answer the route gives: kept, tagged, or turned into a 304
  if (vRoute = nil) and not FDynamicETag then
    Exit;
  vPending := TRALCachePending.Create;
  vPending.Plugin := Self;
  vPending.Route := vRoute;
  vPending.Cacheable := (vRoute <> nil) and Cacheable(ARequest);
  if vPending.Cacheable then
    vPending.Key := KeyOf(ARequest);
  vPending.IfNoneMatch := HeaderText(ARequest.Params, 'If-None-Match');
  AResponse.AddWireHandler({$IFDEF FPC}@{$ENDIF}vPending.Ready);
  ARequest.AddFinishHandler({$IFDEF FPC}@{$ENDIF}vPending.Done);
end;

procedure TRALResponseCachePlugin.ResponseReady(AResponse: TRALResponse; AContext: TObject);
var
  vPending: TRALCachePending;
  vBody: StringRAL;
  vETag, vType: StringRAL;
  vHash: TRALSHA1;
  vParam: TRALParam;
begin
  vPending := TRALCachePending(AContext);
  if (AResponse.StatusCode <> HTTP_OK) or AResponse.Committed then
    Exit;
  // a file or a large body is not hashed nor kept
  vParam := AResponse.Params.SingleBody;
  if (vParam = nil) or (not vParam.IsText) or (vParam.Size > FMaxBodySize) then
    Exit;
  vBody := vParam.AsString;
  vETag := HeaderText(AResponse.Params, 'ETag');
  if FDynamicETag and (vETag = '') then
  begin
    vHash := TRALSHA1.Create;
    try
      vHash.OutputType := rhotHex;
      vETag := '"' + Copy(vHash.HashAsString(vBody), 1, 32) + '"';
    finally
      vHash.Free;
    end;
    AResponse.AddHeader('ETag', vETag);
  end;
  vType := AResponse.ContentType;
  if vPending.Cacheable then
  begin
    ActiveStore.Put(vPending.Key, PackEntry(HTTP_OK, vType, vETag, ActiveStore.Current,
      RawByteString(vBody)), Int64RAL(vPending.Route.TTL) * 1000);
    AResponse.AddHeader('X-Cache', 'MISS');
  end;
  if (vETag <> '') and ETagMatches(vPending.IfNoneMatch, vETag) then
    AResponse.Answer(HTTP_NotModified, '', vType);
end;

end.
