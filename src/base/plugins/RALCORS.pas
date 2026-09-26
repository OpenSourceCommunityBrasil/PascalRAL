/// CORS plugin of the server, and TRALCORSOptions, its configuration
unit RALCORS;

{ CORS answers from the route of the request: the headers depend on the methods
  that route allows, and they are only sent for a route that takes OPTIONS. The
  preflight itself (OPTIONS) is answered by the module that owns the route, as
  any other method; this plugin only adds the headers. A server without it
  sends no CORS header at all. }

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools, RALPlugin, RALRequest, RALResponse, RALRoutes;

type
  { TRALCORSOptions }

  /// Who may call the server from a browser, and with what
  TRALCORSOptions = class(TPersistent)
  private
    FAllowCredentials: boolean;
    FAllowHeaders: TStringList;
    FAllowOrigin: StringRAL;
    FMaxAge: IntegerRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetAllowHeaders(AValue: TStringList);
    procedure SetDefaultHeaders;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAllowHeader(AValue: StringRAL);
    function GetAllowHeaders: StringRAL;
    /// The Access-Control-Allow-Origin to answer a request coming from
    /// ARequestOrigin: '*', the configured origin, or the request's own origin
    /// when it is one of a list. '' means "this origin is not allowed"
    function OriginFor(const ARequestOrigin: StringRAL): StringRAL;
  published
    /// Sends Access-Control-Allow-Credentials: true, which a browser needs to
    /// let fetch(..., {credentials: 'include'}) through - cookies or an
    /// Authorization header on a cross-origin call. Browsers refuse it next to
    /// AllowOrigin = '*', and so does the server: with '*' the header is not
    /// sent. Name the origins instead
    property AllowCredentials: boolean read FAllowCredentials write FAllowCredentials
      default False;
    /// List of headers that are allowed in the CORS configuration
    property AllowHeaders: TStringList read FAllowHeaders write SetAllowHeaders;
    /// Who may call the server from a browser: '*' (anyone, the default), one
    /// origin ('https://app.example.com'), or several separated by spaces or
    /// commas - then the request's Origin is answered back when it is one of
    /// them, with Vary: Origin, and nothing is answered when it is not
    property AllowOrigin: StringRAL read FAllowOrigin write FAllowOrigin;
    /// Time in seconds a browser may keep the preflight answer
    property MaxAge: IntegerRAL read FMaxAge write FMaxAge;
  end;

  { TRALCORSPlugin }

  /// Answers the CORS headers of every route that accepts OPTIONS
  TRALCORSPlugin = class(TRALPlugin)
  private
    FOptions: TRALCORSOptions;
    procedure SetOptions(AValue: TRALCORSOptions);
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// Writes the CORS headers for a route that allows AAllowMethods
    procedure AnswerCORS(const AAllowMethods: StringRAL; ARequest: TRALRequest;
      AResponse: TRALResponse);
    /// ppProcess: the headers for the route of the request, when it takes
    /// OPTIONS. Never answers the request itself
    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
  published
    property Options: TRALCORSOptions read FOptions write SetOptions;
  end;

implementation

{ TRALCORSOptions }

constructor TRALCORSOptions.Create;
begin
  inherited;
  FAllowOrigin := '*';
  FMaxAge := 86400;

  FAllowHeaders := TStringList.Create;
  SetDefaultHeaders;
end;

destructor TRALCORSOptions.Destroy;
begin
  FreeAndNil(FAllowHeaders);
  inherited;
end;

procedure TRALCORSOptions.AddAllowHeader(AValue: StringRAL);
begin
  FAllowHeaders.Add(AValue);
end;

procedure TRALCORSOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TRALCORSOptions then
  begin
    TRALCORSOptions(Dest).AllowCredentials := FAllowCredentials;
    TRALCORSOptions(Dest).AllowHeaders := FAllowHeaders;
    TRALCORSOptions(Dest).AllowOrigin := FAllowOrigin;
    TRALCORSOptions(Dest).MaxAge := FMaxAge;
  end
  else
    inherited AssignTo(Dest);
end;

function TRALCORSOptions.GetAllowHeaders: StringRAL;
begin
  FAllowHeaders.Delimiter := ',';
  Result := FAllowHeaders.DelimitedText;
end;

function TRALCORSOptions.OriginFor(const ARequestOrigin: StringRAL): StringRAL;
var
  vList, vItem: StringRAL;
  vInt: IntegerRAL;
begin
  vList := Trim(FAllowOrigin);
  if (vList = '*') or (vList = '') then
    Exit(vList);

  // a single origin is answered as configured, whoever asks - as before
  if (Pos(StringRAL(' '), vList) = 0) and (Pos(StringRAL(','), vList) = 0) then
    Exit(vList);

  Result := '';
  if ARequestOrigin = '' then
    Exit;
  vList := StringReplace(vList, ',', ' ', [rfReplaceAll]);
  while vList <> '' do
  begin
    vInt := Pos(StringRAL(' '), vList);
    if vInt = 0 then
      vInt := Length(vList) + 1;
    vItem := Trim(Copy(vList, 1, vInt - 1));
    Delete(vList, 1, vInt);
    // scheme and host are case-insensitive; a trailing slash is not part of
    // an origin, but a configured one with it should still match
    if Copy(vItem, Length(vItem), 1) = '/' then
      vItem := Copy(vItem, 1, Length(vItem) - 1);
    if (vItem <> '') and RALSameName(vItem, ARequestOrigin) then
      Exit(ARequestOrigin);
  end;
end;

procedure TRALCORSOptions.SetAllowHeaders(AValue: TStringList);
begin
  if FAllowHeaders = AValue then
    Exit;

  if Trim(AValue.Text) <> '' then
    FAllowHeaders.Text := AValue.Text
  else
    SetDefaultHeaders;
end;

procedure TRALCORSOptions.SetDefaultHeaders;
begin
  FAllowHeaders.Add('Content-Type');
  FAllowHeaders.Add('Origin');
  FAllowHeaders.Add('Accept');
  FAllowHeaders.Add('Authorization');
  FAllowHeaders.Add('Content-Encoding');
  FAllowHeaders.Add('Accept-Encoding');
end;

{ TRALCORSPlugin }

constructor TRALCORSPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TRALCORSOptions.Create;
end;

destructor TRALCORSPlugin.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TRALCORSPlugin.AnswerCORS(const AAllowMethods: StringRAL;
  ARequest: TRALRequest; AResponse: TRALResponse);
var
  vOrigin: StringRAL;
begin
  vOrigin := FOptions.OriginFor(ARequest.Params.GetKind['Origin', rpkHEADER].AsString);
  if vOrigin <> '' then
    AResponse.Params.AddParam('Access-Control-Allow-Origin', vOrigin, rpkHEADER);
  { the answer depends on who asked whenever it is not one fixed value, and a
    cache in the middle must not hand one origin's answer to another }
  if (vOrigin <> Trim(FOptions.AllowOrigin)) or (vOrigin = '') then
    AResponse.Params.AddParam('Vary', 'Origin', rpkHEADER);
  if FOptions.AllowCredentials and (vOrigin <> '') and (vOrigin <> '*') then
    AResponse.Params.AddParam('Access-Control-Allow-Credentials', 'true', rpkHEADER);
  AResponse.Params.AddParam('Access-Control-Allow-Methods', AAllowMethods, rpkHEADER);
  AResponse.Params.AddParam('Access-Control-Allow-Headers', FOptions.GetAllowHeaders,
    rpkHEADER);

  if FOptions.MaxAge > 0 then
    AResponse.Params.AddParam('Access-Control-Max-Age', IntToStr(FOptions.MaxAge),
      rpkHEADER);
end;

class function TRALCORSPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityCORS;
end;

function TRALCORSPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppProcess];
end;

procedure TRALCORSPlugin.ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
  var AHandled: boolean);
var
  vRoute: TRALRoute;
begin
  vRoute := Host.FindRoute(ARequest, AResponse);
  { GetAllowMethods walks the nine methods and concatenates: only asked when
    the route takes OPTIONS, the one case the headers are sent }
  if (vRoute <> nil) and vRoute.IsMethodAllowed(amOPTIONS) then
    AnswerCORS(vRoute.GetAllowMethods, ARequest, AResponse);
end;

procedure TRALCORSPlugin.SetOptions(AValue: TRALCORSOptions);
begin
  if (AValue <> nil) and (AValue <> FOptions) then
    FOptions.Assign(AValue);
end;

end.
