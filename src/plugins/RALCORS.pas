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
  RALTypes, RALConsts, RALTools, RALThreadSafe, RALPlugin, RALRequest, RALResponse,
  RALRoutes;

type
  /// Who may call the server from a browser, and with which headers.
  TRALCORSOptions = class(TPersistent)
  private
    FAllowCredentials: boolean;
    FAllowHeaders: TStringList;
    /// AllowHeaders as the header carries it, rebuilt when the list changes.
    FAllowHeadersText: TRALSnapshots;
    FAllowOrigin: StringRAL;
    FMaxAge: IntegerRAL;

    /// Rebuilds the header text of AllowHeaders.
    procedure AllowHeadersChanged(Sender: TObject);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    /// Copies AValue into AllowHeaders.
    procedure SetAllowHeaders(AValue: TStringList);
    /// Fills AllowHeaders with the headers a browser asks for by default.
    procedure SetDefaultHeaders;
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds AValue to AllowHeaders.
    procedure AddAllowHeader(AValue: StringRAL);
    /// AllowHeaders joined by commas, as the header carries it.
    function GetAllowHeaders: StringRAL;
    { The Access-Control-Allow-Origin for a request from ARequestOrigin: '*', the
      configured origin, or the request's own one when it is in the list; '' refuses. }
    function OriginFor(const ARequestOrigin: StringRAL): StringRAL;
  published
    { Sends Access-Control-Allow-Credentials: true (cookies or Authorization on a
      cross-origin call); not sent with AllowOrigin '*', which browsers refuse. }
    property AllowCredentials: boolean read FAllowCredentials write FAllowCredentials
      default False;
    /// Headers a browser may send on a cross-origin call.
    property AllowHeaders: TStringList read FAllowHeaders write SetAllowHeaders;
    { Origins allowed: empty is nobody, '*' anyone, one origin that one; several,
      separated by blanks or commas, answer the request's Origin with Vary: Origin. }
    property AllowOrigin: StringRAL read FAllowOrigin write FAllowOrigin;
    /// Seconds a browser may keep the preflight answer.
    property MaxAge: IntegerRAL read FMaxAge write FMaxAge;
  end;

  /// Adds the CORS headers to every route that accepts OPTIONS.
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

    /// Writes the CORS headers for a route that allows AAllowMethods.
    procedure AnswerCORS(const AAllowMethods: StringRAL; ARequest: TRALRequest;
      AResponse: TRALResponse);
    /// Adds the headers when the route of the request takes OPTIONS; never answers.
    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
  published
    /// Origins, headers, credentials and preflight cache of the answers.
    property Options: TRALCORSOptions read FOptions write SetOptions;
  end;

implementation

var
  { names and fixed values of the CORS headers, filled once at initialization
    (no literal copied per request) }
  gHdrAllowOrigin, gHdrVary, gValOrigin, gHdrAllowCredentials, gValTrue,
  gHdrAllowMethods, gHdrAllowHeaders, gHdrMaxAge: StringRAL;

/// Fills the header names and values.
procedure FillCORSNames;
begin
  gHdrAllowOrigin := 'Access-Control-Allow-Origin';
  gHdrVary := 'Vary';
  gValOrigin := 'Origin';
  gHdrAllowCredentials := 'Access-Control-Allow-Credentials';
  gValTrue := 'true';
  gHdrAllowMethods := 'Access-Control-Allow-Methods';
  gHdrAllowHeaders := 'Access-Control-Allow-Headers';
  gHdrMaxAge := 'Access-Control-Max-Age';
end;

{ TRALCORSOptions }

constructor TRALCORSOptions.Create;
begin
  inherited;
  { no origin by default: '*' let any web page call the server from a
    visitor's browser unless someone remembered to close it. Empty is also the
    one value streaming leaves out of a form, so it is what a form without
    AllowOrigin has always meant - '*' was always written out explicitly }
  FAllowOrigin := '';
  FMaxAge := 86400;

  FAllowHeadersText := TRALSnapshots.Create;
  FAllowHeaders := TStringList.Create;
  { every change rebuilds the text: Add, Assign, the Object Inspector and a
    form being read all end in OnChange }
  FAllowHeaders.OnChange := {$IFDEF FPC}@{$ENDIF}AllowHeadersChanged;
  SetDefaultHeaders;
end;

destructor TRALCORSOptions.Destroy;
begin
  FreeAndNil(FAllowHeaders);
  FreeAndNil(FAllowHeadersText);
  inherited;
end;

type
  /// One version of the header text of TRALCORSOptions (see TRALSnapshots).
  TRALTextVersion = class
  public
    /// The header text.
    Text: StringRAL;
  end;

procedure TRALCORSOptions.AllowHeadersChanged(Sender: TObject);
var
  vInt: IntegerRAL;
  vVersion: TRALTextVersion;
begin
  // one line, the items joined by commas as they are (no quoting)
  vVersion := TRALTextVersion.Create;
  for vInt := 0 to Pred(FAllowHeaders.Count) do
  begin
    if vInt > 0 then
      vVersion.Text := vVersion.Text + ',';
    vVersion.Text := vVersion.Text + StringRAL(FAllowHeaders.Strings[vInt]);
  end;
  FAllowHeadersText.Publish(vVersion);
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
  Result := TRALTextVersion(FAllowHeadersText.Current).Text;
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

  if (AValue <> nil) and (Trim(AValue.Text) <> '') then
    FAllowHeaders.Text := AValue.Text
  else
    SetDefaultHeaders;
end;

procedure TRALCORSOptions.SetDefaultHeaders;
begin
  { the defaults replace the list: added to what was there, every empty
    assignment appended the six again, and all the copies went out in
    Access-Control-Allow-Headers }
  FAllowHeaders.BeginUpdate;
  try
    FAllowHeaders.Clear;
    FAllowHeaders.Add('Content-Type');
    FAllowHeaders.Add('Origin');
    FAllowHeaders.Add('Accept');
    FAllowHeaders.Add('Authorization');
    FAllowHeaders.Add('Content-Encoding');
    FAllowHeaders.Add('Accept-Encoding');
  finally
    FAllowHeaders.EndUpdate;
  end;
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
  vOrigin := FOptions.OriginFor(ARequest.Params.GetKind[gValOrigin, rpkHEADER].AsString);
  if vOrigin <> '' then
    AResponse.Params.AddParam(gHdrAllowOrigin, vOrigin, rpkHEADER);
  { the answer depends on who asked whenever it is not one fixed value, and a
    cache in the middle must not hand one origin's answer to another. RALTrim:
    Trim over a StringRAL goes through UTF-16 on Delphi }
  if (vOrigin <> RALTrim(FOptions.AllowOrigin)) or (vOrigin = '') then
    AResponse.Params.AddParam(gHdrVary, gValOrigin, rpkHEADER);
  if FOptions.AllowCredentials and (vOrigin <> '') and (vOrigin <> '*') then
    AResponse.Params.AddParam(gHdrAllowCredentials, gValTrue, rpkHEADER);
  AResponse.Params.AddParam(gHdrAllowMethods, AAllowMethods, rpkHEADER);
  AResponse.Params.AddParam(gHdrAllowHeaders, FOptions.GetAllowHeaders, rpkHEADER);

  if FOptions.MaxAge > 0 then
    AResponse.Params.AddParam(gHdrMaxAge, IntToStr(FOptions.MaxAge), rpkHEADER);
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

initialization
  FillCORSNames;

end.
