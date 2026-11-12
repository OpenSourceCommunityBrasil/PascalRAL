/// Views: routes that answer with a template, with no code beyond the data they show.
unit RALViewModule;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, Contnrs,
  RALTypes, RALConsts, RALTools, RALServer, RALRoutes, RALRequest, RALResponse,
  RALParams, RALTemplate;

type
  { The data of one view render, searched before what the module adds on its
    own (params, htmx, csrf_token, csrf_field, status, message). Objects bound
    with Add must outlive the render; AddOwned hands one over to be freed after it. }
  TRALViewContext = class(TRALTemplateMap)
  private
    /// Objects freed with the context.
    FOwned: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    /// Binds AName to AObject and frees AObject after the render; returns the context.
    function AddOwned(const AName: StringRAL; AObject: TObject): TRALViewContext;
  end;

  { Fills the data of a view. Set ARender to False when the handler answered
    by itself (a redirect, an error): the template is then not rendered. }
  TRALViewContextEvent = procedure(Sender: TObject; ARequest: TRALRequest;
    AResponse: TRALResponse; AContext: TRALViewContext; var ARender: boolean) of object;

  { A route of a TRALViewModule: renders Template with the data OnGetContext
    gives, inside the engine's Layout unless the request wants a fragment. A
    route that also has OnReply runs it instead, as any route. }
  TRALViewRoute = class(TRALRoute)
  private
    FOnGetContext: TRALViewContextEvent;
    FStatusCode: IntegerRAL;
    FTemplate: StringRAL;
    FUseLayout: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ACollection: TCollection); override;

    /// Renders the view (see the class).
    procedure Execute(ARequest: TRALRequest; AResponse: TRALResponse); override;
    { The template the route renders: Template, else its Route without the
      leading slash ('index' for '/'). }
    function TemplateName: StringRAL;
  published
    /// Fills the data of the view, after the module's own OnGetContext.
    property OnGetContext: TRALViewContextEvent read FOnGetContext write FOnGetContext;
    /// Status the view answers with.
    property StatusCode: IntegerRAL read FStatusCode write FStatusCode default 200;
    /// Name of the template rendered; empty takes the route (see TemplateName).
    property Template: StringRAL read FTemplate write FTemplate;
    { Renders inside the engine's Layout. An HTMX request that is not boosted
      always gets the fragment alone. }
    property UseLayout: boolean read FUseLayout write FUseLayout default True;
  end;

  { A module of views (TRALViewRoute): each route renders a template of Engine.
    Every view answers with Vary: HX-Request, and an HTMX request (not boosted,
    not a history restore) gets the page without the layout. With a
    TRALCSRFPlugin on the server, csrf_token and csrf_field are in every
    context. An exception in a view answers ErrorTemplate when there is one. }
  TRALViewModule = class(TRALModuleRoutes)
  private
    FEngine: TRALTemplateEngine;
    FErrorTemplate: StringRAL;
    FOnGetContext: TRALViewContextEvent;

    { the module's own data for a render: params, htmx, csrf_token, csrf_field,
      and AStatus/AMessage when above 0; the caller frees it }
    function BaseContext(ARequest: TRALRequest; AResponse: TRALResponse;
      AStatus: IntegerRAL; const AMessage: StringRAL): TRALTemplateMap;
    /// Raises when no Engine is linked.
    procedure CheckEngine;
    procedure SetEngine(AValue: TRALTemplateEngine);
  protected
    function HandleException(ARequest: TRALRequest; AResponse: TRALResponse;
      AException: Exception): boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    class function RouteClass: TRALRouteClass; override;
  public
    /// Adds a view of ARoute rendering ATemplate ('' = from the route); returns it.
    function CreateView(const ARoute: StringRAL; const ATemplate: StringRAL = '';
      AOnGetContext: TRALViewContextEvent = nil): TRALViewRoute;
    function DefaultContentType: StringRAL; override;
    { Renders the view ARoute: the module's OnGetContext, the route's, then the
      template with Engine. }
    procedure RenderView(ARoute: TRALViewRoute; ARequest: TRALRequest;
      AResponse: TRALResponse);
  published
    /// The engine that finds and renders the templates.
    property Engine: TRALTemplateEngine read FEngine write SetEngine;
    { Template rendered when a view raises, with status and message in its
      data (the message as TRALServer.ErrorText gives it); empty answers the
      server's usual error. }
    property ErrorTemplate: StringRAL read FErrorTemplate write FErrorTemplate;
    /// Fills the data of every view, before the route's own OnGetContext.
    property OnGetContext: TRALViewContextEvent read FOnGetContext write FOnGetContext;
    property Routes;
  end;

implementation

uses
  RALMIMETypes, RALHtmx, RALCSRF, RALExceptions, RALErrorCodes, RALPlugin;

{ Adds HX-Request to the Vary header of AResponse: caches keep the page and
  its fragment apart. }
procedure AddVaryHtmx(AResponse: TRALResponse);
var
  vParam: TRALParam;
begin
  vParam := AResponse.Params.GetKind['Vary', rpkHEADER];
  if vParam = nil then
    AResponse.Params.AddParam('Vary', 'HX-Request', rpkHEADER)
  else if RALPosText('hx-request', vParam.AsString) = 0 then
    vParam.AsString := vParam.AsString + ', HX-Request';
end;

{ TRALViewContext }

constructor TRALViewContext.Create;
begin
  inherited Create;
  FOwned := TObjectList.Create(True);
end;

destructor TRALViewContext.Destroy;
begin
  FreeAndNil(FOwned);
  inherited Destroy;
end;

function TRALViewContext.AddOwned(const AName: StringRAL; AObject: TObject): TRALViewContext;
begin
  FOwned.Add(AObject);
  Add(AName, AObject);
  Result := Self;
end;

{ TRALViewRoute }

constructor TRALViewRoute.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FStatusCode := HTTP_OK;
  FUseLayout := True;
  AllowedMethods := [amGET, amHEAD, amPOST];
end;

procedure TRALViewRoute.AssignTo(Dest: TPersistent);
var
  vDest: TRALViewRoute;
begin
  inherited AssignTo(Dest);
  if not (Dest is TRALViewRoute) then
    Exit;
  vDest := TRALViewRoute(Dest);
  vDest.OnGetContext := FOnGetContext;
  vDest.StatusCode := FStatusCode;
  vDest.Template := FTemplate;
  vDest.UseLayout := FUseLayout;
end;

procedure TRALViewRoute.Execute(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  if HasCoreHandler or (not (Collection.Owner is TRALViewModule)) then
    inherited Execute(ARequest, AResponse)
  else
    TRALViewModule(Collection.Owner).RenderView(Self, ARequest, AResponse);
end;

function TRALViewRoute.TemplateName: StringRAL;
begin
  Result := FTemplate;
  if Result <> '' then
    Exit;
  Result := Route;
  while (Result <> '') and (Result[POSINISTR] = '/') do
    Delete(Result, 1, 1);
  if Result = '' then
    Result := 'index';
end;

{ TRALViewModule }

function TRALViewModule.BaseContext(ARequest: TRALRequest; AResponse: TRALResponse;
  AStatus: IntegerRAL; const AMessage: StringRAL): TRALTemplateMap;
var
  vParams: TRALViewContext;
  vInt: IntegerRAL;
  vParam: TRALParam;
  vPlugin: TRALServerPlugin;
  vCSRF: TRALCSRFPlugin;
begin
  Result := TRALViewContext.Create;
  try
    // the query, the route params and the form fields, by name
    vParams := TRALViewContext.Create;
    TRALViewContext(Result).AddOwned('params', vParams);
    for vInt := 0 to Pred(ARequest.Params.Count) do
    begin
      vParam := ARequest.Params.Index[vInt];
      if (vParam.Kind in [rpkQUERY, rpkFIELD]) and (vParam.FileName = '') then
        vParams.Add(vParam.ParamName, vParam.AsString);
    end;
    Result.AddBoolean('htmx', TRALHtmx.IsHtmx(ARequest));
    if AStatus > 0 then
    begin
      Result.Add('status', StringRAL(IntToStr(AStatus)));
      Result.Add('message', AMessage);
    end;

    vCSRF := nil;
    if Server <> nil then
    begin
      vPlugin := Server.FindPlugin(TRALCSRFPlugin);
      if vPlugin is TRALCSRFPlugin then
        vCSRF := TRALCSRFPlugin(vPlugin);
    end;
    if vCSRF <> nil then
    begin
      Result.Add('csrf_token', vCSRF.Token(ARequest, AResponse));
      Result.Add('csrf_field', vCSRF.FieldHTML(ARequest, AResponse));
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TRALViewModule.CheckEngine;
begin
  if FEngine = nil then
    raise ERALException.Create(ecViewNoEngine);
end;

function TRALViewModule.CreateView(const ARoute, ATemplate: StringRAL;
  AOnGetContext: TRALViewContextEvent): TRALViewRoute;
begin
  Result := TRALViewRoute(NewRoute(ARoute));
  Result.Template := ATemplate;
  Result.OnGetContext := AOnGetContext;
end;

function TRALViewModule.DefaultContentType: StringRAL;
begin
  Result := rctTEXTHTML + '; charset=utf-8';
end;

function TRALViewModule.HandleException(ARequest: TRALRequest;
  AResponse: TRALResponse; AException: Exception): boolean;
var
  vBase: TRALTemplateMap;
begin
  Result := False;
  if (FErrorTemplate = '') or (FEngine = nil) or AResponse.Committed then
    Exit;
  try
    vBase := BaseContext(ARequest, AResponse, HTTP_InternalError, ErrorText(AException));
    try
      AddVaryHtmx(AResponse);
      FEngine.Render(AResponse, FErrorTemplate, [vBase], HTTP_InternalError,
        not TRALHtmx.WantsFragment(ARequest));
      Result := True;
    finally
      FreeAndNil(vBase);
    end;
  except
    // the error page failed too: the server answers its usual error
    Result := False;
  end;
end;

procedure TRALViewModule.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FEngine) then
    FEngine := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TRALViewModule.RenderView(ARoute: TRALViewRoute; ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vBase: TRALTemplateMap;
  vContext: TRALViewContext;
  vRender: boolean;
begin
  CheckEngine;
  vBase := BaseContext(ARequest, AResponse, 0, '');
  try
    vContext := TRALViewContext.Create;
    try
      vRender := True;
      if Assigned(FOnGetContext) then
        FOnGetContext(ARoute, ARequest, AResponse, vContext, vRender);
      if vRender and Assigned(ARoute.OnGetContext) then
        ARoute.OnGetContext(ARoute, ARequest, AResponse, vContext, vRender);
      if not vRender then
        Exit;
      AddVaryHtmx(AResponse);
      FEngine.Render(AResponse, ARoute.TemplateName, [vBase, vContext],
        ARoute.StatusCode, ARoute.UseLayout and (not TRALHtmx.WantsFragment(ARequest)));
    finally
      FreeAndNil(vContext);
    end;
  finally
    FreeAndNil(vBase);
  end;
end;

class function TRALViewModule.RouteClass: TRALRouteClass;
begin
  Result := TRALViewRoute;
end;

procedure TRALViewModule.SetEngine(AValue: TRALTemplateEngine);
begin
  if AValue = FEngine then
    Exit;
  if FEngine <> nil then
    FEngine.RemoveFreeNotification(Self);
  FEngine := AValue;
  if FEngine <> nil then
    FEngine.FreeNotification(Self);
end;

end.
