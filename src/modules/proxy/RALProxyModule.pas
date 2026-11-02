/// A reverse proxy inside any TRALServer (RALModuleProxyPackage): rules send the
/// requests under a path (and a host) to a TRALUpstream, over a TRALClient per
/// server, with the server's plugins in front as for any route.
unit RALProxyModule;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALTools, RALConsts, RALMIMETypes, RALServer, RALRoutes, RALRequest,
  RALResponse, RALParams, RALClient, RALProxyCore;

type
  TRALProxyModule = class;

  { A rule: the requests under Path (after the module's Domain), for Host when
    set, go to Upstream. StripPrefix takes Path off before passing them on }
  TRALProxyRule = class(TCollectionItem)
  private
    FHost: StringRAL;
    FPath: StringRAL;
    FStripPrefix: boolean;
    FUpstream: TRALUpstream;

    procedure SetUpstream(const AValue: TRALUpstream);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  published
    /// Host the rule is for (the Host header, without the port); empty is any.
    property Host: StringRAL read FHost write FHost;
    /// Path prefix ('/api'); empty or '/' is every path.
    property Path: StringRAL read FPath write FPath;
    /// Passes '/api/users' on as '/users'.
    property StripPrefix: boolean read FStripPrefix write FStripPrefix default False;
    /// The servers the requests go to.
    property Upstream: TRALUpstream read FUpstream write SetUpstream;
  end;

  /// The rules of a TRALProxyModule; the first that matches wins.
  TRALProxyRules = class(TOwnedCollection)
  public
    /// Adds a rule sending APath to AUpstream.
    function AddRule(const APath: StringRAL; AUpstream: TRALUpstream): TRALProxyRule;
  end;

  { A reverse proxy as a module: the server's engine, TLS and plugins in front
    (limits, authentication, the forwarded plugin...), and the requests a rule
    matches passed on to a server of its upstream. The answer is relayed in
    pieces as it arrives where the engine streams; the request body is passed
    on as the engine received it. Hop-by-hop headers are not passed on,
    X-Forwarded-For/-Proto/-Host are added (AddForwarded), a Location of the
    server behind is made relative. A request that reached no server goes to
    the next one (MaxAttempts) when its method is idempotent and nothing was
    sent yet; 502 when none answered, 504 on a timeout, 503 with no server }
  TRALProxyModule = class(TRALModuleRoutes)
  private
    FAddForwarded: boolean;
    FInternal: TRALRoutes;
    FKeepForwardedChain: boolean;
    FMaxAttempts: IntegerRAL;
    FProxyRoute: TRALRoute;
    FRules: TRALProxyRules;
    FStatsPath: StringRAL;
    FVia: StringRAL;

    function FindRule(ARequest: TRALRequest): TRALProxyRule;
    function PathMatches(const APath, APrefix: StringRAL): boolean;
    procedure ProxyRequest(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure SetRules(const AValue: TRALProxyRules);
    procedure StatsRequest(ARequest: TRALRequest; AResponse: TRALResponse);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ServerActivating; override;
    procedure ServerDeactivating; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute; override;
  published
    { Sends X-Forwarded-For (the client: ClientInfo.IP, which a
      TRALForwardedPlugin in front makes the real one), -Proto and -Host to
      the servers }
    property AddForwarded: boolean read FAddForwarded write FAddForwarded default True;
    { Appends the client to the X-Forwarded-For that came instead of replacing
      it. Only behind a proxy that writes that header itself: from the outside
      it is whatever the client wants, and the servers behind trust this proxy }
    property KeepForwardedChain: boolean read FKeepForwardedChain
      write FKeepForwardedChain default False;
    /// Servers tried for one request (only idempotent methods try a second).
    property MaxAttempts: IntegerRAL read FMaxAttempts write FMaxAttempts default 2;
    /// The rules.
    property Rules: TRALProxyRules read FRules write SetRules;
    { Path of the statistics of every upstream (JSON; ?format=prometheus for
      the Prometheus text); empty is none }
    property StatsPath: StringRAL read FStatsPath write FStatsPath;
    /// A Via header (RFC 9110 7.6.3) with this name, on both sides; empty is none.
    property Via: StringRAL read FVia write FVia;
  end;

implementation

type
  { one call to a server behind: relays the pieces of a 2xx answer as they come }
  TRALProxyCall = class
  public
    Committed: boolean;
    Failed: boolean;
    Module: TRALProxyModule;
    Response: TRALResponse;
    Rule: TRALProxyRule;
    Server: TRALUpstreamServer;
    Sticky: StringRAL;

    procedure CopyHead(AFrom: TRALResponse);
    procedure OnData(ASender: TObject; AResponse: TRALResponse; const ABuffer;
      ASize: IntegerRAL; var ACancel: boolean);
  end;

{ the Location of the server behind, made relative to the proxy }
function RewriteLocation(const ALocation: StringRAL; ACall: TRALProxyCall): StringRAL;
var
  vBase: StringRAL;
begin
  Result := ALocation;
  vBase := ACall.Server.URL;
  while (vBase <> '') and (vBase[POSINISTR + Length(vBase) - 1] = '/') do
    vBase := Copy(vBase, 1, Length(vBase) - 1);
  if (vBase = '') or (Length(ALocation) < Length(vBase)) or
     not RALSameName(Copy(ALocation, 1, Length(vBase)), vBase) then
    Exit;
  Result := Copy(ALocation, Length(vBase) + 1, MaxInt);
  if Result = '' then
    Result := '/';
  if ACall.Rule.StripPrefix then
    Result := FixRoute(ACall.Rule.Path) + Result;
end;

procedure TRALProxyCall.CopyHead(AFrom: TRALResponse);
var
  vInt: IntegerRAL;
  vParam, vNew: TRALParam;
  vName: StringRAL;
begin
  for vInt := 0 to AFrom.Params.Count - 1 do
  begin
    vParam := AFrom.Params.Index[vInt];
    vName := vParam.ParamName;
    if vParam.Kind = rpkHEADER then
    begin
      // what the engines and this answer set again
      if RALIsHopHeader(vName) or RALSameName(vName, 'Content-Length') or
         RALSameName(vName, 'Content-Encoding') or RALSameName(vName, 'Content-Type') or
         RALSameName(vName, 'Date') or RALSameName(vName, 'Server') or
         RALSameName(vName, 'Set-Cookie') or RALSameName(vName, 'Status') or
         RALSameName(vName, 'RALEngine') then
        Continue;
      if RALSameName(vName, 'Location') then
      begin
        Response.AddHeader('Location', RewriteLocation(vParam.AsString, Self));
        Continue;
      end;
      vNew := Response.Params.NewParam;
      vNew.ParamName := vName;
      vNew.Kind := rpkHEADER;
      vNew.AsString := vParam.AsString;
    end
  end;
  // every Set-Cookie line whole, attributes and all: the answer writes each on its own
  for vInt := 0 to AFrom.Params.SetCookieLines.Count - 1 do
  begin
    vNew := Response.Params.NewParam;
    vNew.ParamName := 'Set-Cookie';
    vNew.Kind := rpkCOOKIE;
    vNew.AsString := StringRAL(AFrom.Params.SetCookieLines[vInt]);
  end;
  if Module.FVia <> '' then
    Response.AddHeader('Via', '1.1 ' + Module.FVia);
  if Sticky <> '' then
  begin
    vNew := Response.Params.NewParam;
    vNew.ParamName := 'Set-Cookie';
    vNew.Kind := rpkCOOKIE;
    vNew.AsString := Sticky;
  end;
end;

procedure TRALProxyCall.OnData(ASender: TObject; AResponse: TRALResponse;
  const ABuffer; ASize: IntegerRAL; var ACancel: boolean);
begin
  if not Committed then
  begin
    // the head goes with the first piece
    Committed := True;
    CopyHead(AResponse);
    Response.BeginStream(AResponse.StatusCode, AResponse.ContentType);
  end;
  if (ASize > 0) and not Response.Send(ABuffer, ASize) then
  begin
    // the client went away: the server behind stops too
    Failed := True;
    ACancel := True;
  end;
end;

{ TRALProxyRule }

procedure TRALProxyRule.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALProxyRule then
  begin
    TRALProxyRule(Dest).Host := FHost;
    TRALProxyRule(Dest).Path := FPath;
    TRALProxyRule(Dest).StripPrefix := FStripPrefix;
    TRALProxyRule(Dest).Upstream := FUpstream;
  end
  else
    inherited AssignTo(Dest);
end;

function TRALProxyRule.GetDisplayName: string;
begin
  Result := string(FHost + FixRoute(FPath));
end;

procedure TRALProxyRule.SetUpstream(const AValue: TRALUpstream);
begin
  FUpstream := AValue;
  // the module hears when the upstream goes
  if (AValue <> nil) and (Collection <> nil) and (Collection.Owner is TComponent) then
    AValue.FreeNotification(TComponent(Collection.Owner));
end;

{ TRALProxyRules }

function TRALProxyRules.AddRule(const APath: StringRAL; AUpstream: TRALUpstream): TRALProxyRule;
begin
  Result := TRALProxyRule(Add);
  Result.Path := APath;
  Result.Upstream := AUpstream;
end;

{ TRALProxyModule }

constructor TRALProxyModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRules := TRALProxyRules.Create(Self, TRALProxyRule);
  FAddForwarded := True;
  FMaxAttempts := 2;
  // the route every rule answers through, out of Routes
  FInternal := TRALRoutes.Create(Self, TRALRoute);
  FProxyRoute := TRALRoute(FInternal.Add);
  FProxyRoute.Route := '';
  FProxyRoute.AllowedMethods := [amALL];
  FProxyRoute.OnReply := {$IFDEF FPC}@{$ENDIF}ProxyRequest;
end;

destructor TRALProxyModule.Destroy;
begin
  FreeAndNil(FInternal);
  FreeAndNil(FRules);
  inherited Destroy;
end;

procedure TRALProxyModule.Notification(AComponent: TComponent; Operation: TOperation);
var
  vInt: IntegerRAL;
begin
  if (Operation = opRemove) and (AComponent is TRALUpstream) and (FRules <> nil) then
    for vInt := 0 to FRules.Count - 1 do
      if TRALProxyRule(FRules.Items[vInt]).FUpstream = AComponent then
        TRALProxyRule(FRules.Items[vInt]).FUpstream := nil;
  inherited;
end;

procedure TRALProxyModule.SetRules(const AValue: TRALProxyRules);
begin
  FRules.Assign(AValue);
end;

procedure TRALProxyModule.ServerActivating;
var
  vInt: IntegerRAL;
begin
  inherited;
  if FStatsPath <> '' then
  begin
    if Routes.Find[FStatsPath] = nil then
      CreateRoute(FStatsPath, {$IFDEF FPC}@{$ENDIF}StatsRequest).Allow([amGET]);
  end;
  for vInt := 0 to FRules.Count - 1 do
    if TRALProxyRule(FRules.Items[vInt]).Upstream <> nil then
      TRALProxyRule(FRules.Items[vInt]).Upstream.StartHealthCheck;
end;

procedure TRALProxyModule.ServerDeactivating;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FRules.Count - 1 do
    if TRALProxyRule(FRules.Items[vInt]).Upstream <> nil then
      TRALProxyRule(FRules.Items[vInt]).Upstream.StopHealthCheck;
  inherited;
end;

function TRALProxyModule.PathMatches(const APath, APrefix: StringRAL): boolean;
var
  vPrefix: StringRAL;
begin
  vPrefix := FixRoute(APrefix);
  if (vPrefix = '') or (vPrefix = '/') then
  begin
    Result := True;
    Exit;
  end;
  // whole segments: /api takes /api and /api/x, never /apix
  Result := RALSameName(Copy(APath, 1, Length(vPrefix)), vPrefix) and
    ((Length(APath) = Length(vPrefix)) or (APath[POSINISTR + Length(vPrefix)] = '/'));
end;

function TRALProxyModule.FindRule(ARequest: TRALRequest): TRALProxyRule;
var
  vInt, vPos: IntegerRAL;
  vRule: TRALProxyRule;
  vHost: StringRAL;
begin
  Result := nil;
  vHost := ARequest.Host;
  if vHost = '' then
    vHost := ARequest.GetHeader('Host');
  // the host without its port
  vPos := Pos(StringRAL(']'), vHost);
  if vPos = 0 then
    vPos := 1;
  vInt := Pos(StringRAL(':'), Copy(vHost, vPos, MaxInt));
  if vInt > 0 then
    vHost := Copy(vHost, 1, vPos + vInt - 2);
  for vInt := 0 to FRules.Count - 1 do
  begin
    vRule := TRALProxyRule(FRules.Items[vInt]);
    if (vRule.Upstream = nil) or
       ((vRule.Host <> '') and not RALSameName(vRule.Host, vHost)) then
      Continue;
    if PathMatches(ARequest.Query, Domain + vRule.Path) then
    begin
      Result := vRule;
      Exit;
    end;
  end;
end;

function TRALProxyModule.CanAnswerRoute(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALRoute;
begin
  // the module's own routes (the statistics) first
  Result := inherited CanAnswerRoute(ARequest, AResponse);
  if (Result = nil) and (FindRule(ARequest) <> nil) then
    Result := FProxyRoute;
end;

procedure TRALProxyModule.StatsRequest(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vInt: IntegerRAL;
  vText, vName: StringRAL;
  vUpstream: TRALUpstream;
  vSeen: TList;
begin
  vSeen := TList.Create;
  try
    vText := '';
    for vInt := 0 to FRules.Count - 1 do
    begin
      vUpstream := TRALProxyRule(FRules.Items[vInt]).Upstream;
      if (vUpstream = nil) or (vSeen.IndexOf(vUpstream) >= 0) then
        Continue;
      vSeen.Add(vUpstream);
      vName := StringRAL(vUpstream.Name);
      if vName = '' then
        vName := StringRAL('upstream' + IntToStr(vSeen.Count));
      if RALSameName(ARequest.ParamByName('format').AsString, 'prometheus') then
        vText := vText + vUpstream.StatsPrometheus(vName)
      else
      begin
        if vText <> '' then
          vText := vText + ',';
        vText := vText + '"' + vName + '":' + vUpstream.StatsJSON;
      end;
    end;
    if RALSameName(ARequest.ParamByName('format').AsString, 'prometheus') then
      AResponse.Answer(HTTP_OK, vText, 'text/plain; version=0.0.4')
    else
      AResponse.Answer(HTTP_OK, '{' + vText + '}', rctAPPLICATIONJSON);
  finally
    vSeen.Free;
  end;
end;

procedure TRALProxyModule.ProxyRequest(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vRule: TRALProxyRule;
  vExclude: TList;
  vServer: TRALUpstreamServer;
  vCall: TRALProxyCall;
  vClient: TRALClient;
  vReq: TRALRequest;
  vResp: TRALResponse;
  vParam, vNew: TRALParam;
  vInt, vAttempt: IntegerRAL;
  vPath, vName, vForwarded, vProto: StringRAL;
  vStart: Int64RAL;
  vFailed, vRetry: boolean;
  vError: TRALTransportError;
  vHop: TStringList;
begin
  vRule := FindRule(ARequest);
  if vRule = nil then
  begin
    AResponse.AnswerError(HTTP_NotFound);
    Exit;
  end;
  vPath := ARequest.Query;
  if vRule.StripPrefix then
  begin
    vPath := Copy(vPath, Length(FixRoute(Domain + vRule.Path)) + 1, MaxInt);
    if (vPath = '') or (vPath[POSINISTR] <> '/') then
      vPath := '/' + vPath;
  end;
  vProto := 'http';
  if RALSameName(ARequest.HttpVersion, 'HTTPS') then
    vProto := 'https';

  // the headers Connection names are of this hop too (RFC 9110 7.6.1)
  vHop := TStringList.Create;
  vExclude := TList.Create;
  try
    vHop.CaseSensitive := False;
    vHop.Delimiter := ',';
    vHop.StrictDelimiter := True;
    vHop.DelimitedText := string(ARequest.GetHeader('Connection'));
    for vInt := 0 to vHop.Count - 1 do
      vHop[vInt] := Trim(vHop[vInt]);
    vAttempt := 0;
    repeat
      Inc(vAttempt);
      vServer := vRule.Upstream.Choose(ARequest, vExclude);
      if vServer = nil then
      begin
        AResponse.AnswerError(HTTP_ServiceUnavailable);
        Exit;
      end;
      vCall := TRALProxyCall.Create;
      try
        vCall.Module := Self;
        vCall.Response := AResponse;
        vCall.Rule := vRule;
        vCall.Server := vServer;
        if vRule.Upstream.Sticky.Enabled then
          vCall.Sticky := vRule.Upstream.Sticky.CookieName + '=' +
            vRule.Upstream.StickyValue(vServer) + '; Path=/; HttpOnly';
        vClient := vServer.Client;
        vReq := vClient.Request; // this thread's request of the shared client
        vReq.Clear;
        // the headers, less what is of this hop or set again by the client
        for vInt := 0 to ARequest.Params.Count - 1 do
        begin
          vParam := ARequest.Params.Index[vInt];
          vName := vParam.ParamName;
          case vParam.Kind of
            rpkHEADER:
              begin
                if RALIsHopHeader(vName) or (vHop.IndexOf(string(vName)) >= 0) or
                   RALSameName(vName, 'Host') or
                   RALSameName(vName, 'Content-Length') or RALSameName(vName, 'Content-Type') or
                   RALSameName(vName, 'Content-Encoding') or RALSameName(vName, 'Accept-Encoding') or
                   RALSameName(vName, 'User-Agent') or RALSameName(vName, 'Cookie') or
                   RALSameName(vName, 'RALEngine') then
                  Continue;
                if FAddForwarded and (RALSameName(vName, 'X-Forwarded-For') or
                   RALSameName(vName, 'X-Forwarded-Proto') or RALSameName(vName, 'X-Forwarded-Host')) then
                  Continue;
                vNew := vReq.Params.NewParam;
                vNew.ParamName := vName;
                vNew.Kind := rpkHEADER;
                vNew.AsString := vParam.AsString;
              end;
            rpkQUERY, rpkCOOKIE:
              begin
                // the sticky cookie is the proxy's, not the server's
                if (vParam.Kind = rpkCOOKIE) and vRule.Upstream.Sticky.Enabled and
                   RALSameName(vName, vRule.Upstream.Sticky.CookieName) then
                  Continue;
                vNew := vReq.Params.NewParam;
                vNew.ParamName := vName;
                vNew.Kind := vParam.Kind;
                vNew.AsString := vParam.AsString;
              end;
          end;
        end;
        if FAddForwarded then
        begin
          // a chain from the outside is the client's to forge: replaced, unless asked
          vForwarded := '';
          if FKeepForwardedChain then
            vForwarded := ARequest.GetHeader('X-Forwarded-For');
          if vForwarded <> '' then
            vForwarded := vForwarded + ', ';
          vReq.AddHeader('X-Forwarded-For', vForwarded + ARequest.ClientInfo.IP);
          vReq.AddHeader('X-Forwarded-Proto', vProto);
          if ARequest.Host <> '' then
            vReq.AddHeader('X-Forwarded-Host', ARequest.Host);
        end;
        if FVia <> '' then
          vReq.AddHeader('Via', '1.1 ' + FVia);
        // the body as the engine received it, lent, never copied
        if (ARequest.RequestStream <> nil) and (ARequest.RequestStream.Size > 0) then
        begin
          vNew := vReq.Params.NewParam;
          vNew.Kind := rpkBODY;
          vNew.BorrowStream(ARequest.RequestStream);
          vNew.ContentType := ARequest.ContentType;
        end;

        vStart := RALTickMicro;
        vResp := nil;
        vError := rteNone;
        try
          try
            vClient.Execute(vPath, ARequest.Method, vResp,
              {$IFDEF FPC}@{$ENDIF}vCall.OnData);
            vError := vResp.TransportError;
          except
            vError := rteOther;
          end;
          vFailed := (vError <> rteNone) and not vCall.Committed;
          vRule.Upstream.Done(vServer, vFailed,
            (vResp <> nil) and (vResp.StatusCode >= 500),
            IntegerRAL((RALTickMicro - vStart) div 1000));
          vReq.Clear;
          if vCall.Committed then
            Exit; // relayed in pieces (or cut by the client)
          vRetry := vFailed and (vAttempt < FMaxAttempts) and
            (ARequest.Method in RALIdempotentMethods);
          if vRetry then
          begin
            vExclude.Add(vServer);
            Continue;
          end;
          if vFailed then
          begin
            if vError = rteTimeout then
              AResponse.AnswerError(HTTP_GatewayTimeout)
            else
              AResponse.AnswerError(HTTP_BadGateway);
            Exit;
          end;
          // an answer that did not come in pieces (not 2xx, or no body)
          vCall.Committed := True;
          vCall.CopyHead(vResp);
          if (vResp.ResponseStream <> nil) and (vResp.ResponseStream.Size > 0) then
            AResponse.Answer(vResp.StatusCode, vResp.ResponseStream, vResp.ContentType)
          else
          begin
            AResponse.StatusCode := vResp.StatusCode;
            AResponse.ContentType := vResp.ContentType;
          end;
          Exit;
        finally
          vResp.Free;
        end;
      finally
        vCall.Free;
      end;
    until False;
  finally
    vExclude.Free;
    vHop.Free;
  end;
end;

end.
