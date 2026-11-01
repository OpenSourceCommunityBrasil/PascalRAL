/// Routes with anonymous handlers: Server.Get('/ping', procedure(...) begin ... end).
/// Sugar over CreateRoute; the whole unit is empty where the compiler has no
/// anonymous methods (FPC 3.2), and the handlers of object stay the RAD way.
unit RALRoutesAnon;

{$I PascalRAL.inc}

{$IFDEF FPC}
  {$IFDEF RALHasAnonMethods}
    {$MODESWITCH FUNCTIONREFERENCES}
    {$MODESWITCH ANONYMOUSFUNCTIONS}
  {$ENDIF}
{$ENDIF}

interface

{$IFDEF RALHasAnonMethods}

uses
  Classes, SysUtils,
  RALTypes, RALRoutes, RALRequest, RALResponse, RALServer;

type
  /// Handler of a route, as an anonymous method.
  TRALOnReplyRef = reference to procedure(ARequest: TRALRequest; AResponse: TRALResponse);

  { Holds an anonymous handler and answers for it as a method: what the route's
    OnReply points to. Owned by the server or module, so it lives as long as it. }
  TRALAnonReply = class(TComponent)
  private
    FHandler: TRALOnReplyRef;
  public
    /// A holder of AHandler, owned by AOwner.
    constructor CreateFor(AOwner: TComponent; const AHandler: TRALOnReplyRef);

    /// Calls the anonymous handler.
    procedure Reply(ARequest: TRALRequest; AResponse: TRALResponse);

    /// The anonymous handler.
    property Handler: TRALOnReplyRef read FHandler;
  end;

  /// Anonymous handlers on the server's own routes.
  TRALServerAnonHelper = class helper for TRALServer
  public
    /// A DELETE route answered by AHandler.
    function Delete(const ARoute: StringRAL; const AHandler: TRALOnReplyRef): TRALRoute;
    /// A GET route answered by AHandler.
    function Get(const ARoute: StringRAL; const AHandler: TRALOnReplyRef): TRALRoute;
    /// A route of AMethods answered by AHandler.
    function Map(const ARoute: StringRAL; AMethods: TRALMethods;
      const AHandler: TRALOnReplyRef): TRALRoute;
    /// A PATCH route answered by AHandler.
    function Patch(const ARoute: StringRAL; const AHandler: TRALOnReplyRef): TRALRoute;
    /// A POST route answered by AHandler.
    function Post(const ARoute: StringRAL; const AHandler: TRALOnReplyRef): TRALRoute;
    /// A PUT route answered by AHandler.
    function Put(const ARoute: StringRAL; const AHandler: TRALOnReplyRef): TRALRoute;
  end;

  /// Anonymous handlers on the routes of a module.
  TRALModuleAnonHelper = class helper for TRALModuleRoutes
  public
    /// A DELETE route answered by AHandler.
    function Delete(const ARoute: StringRAL; const AHandler: TRALOnReplyRef): TRALRoute;
    /// A GET route answered by AHandler.
    function Get(const ARoute: StringRAL; const AHandler: TRALOnReplyRef): TRALRoute;
    /// A route of AMethods answered by AHandler.
    function Map(const ARoute: StringRAL; AMethods: TRALMethods;
      const AHandler: TRALOnReplyRef): TRALRoute;
    /// A PATCH route answered by AHandler.
    function Patch(const ARoute: StringRAL; const AHandler: TRALOnReplyRef): TRALRoute;
    /// A POST route answered by AHandler.
    function Post(const ARoute: StringRAL; const AHandler: TRALOnReplyRef): TRALRoute;
    /// A PUT route answered by AHandler.
    function Put(const ARoute: StringRAL; const AHandler: TRALOnReplyRef): TRALRoute;
  end;

{$ENDIF}

implementation

{$IFDEF RALHasAnonMethods}

{ TRALAnonReply }

constructor TRALAnonReply.CreateFor(AOwner: TComponent; const AHandler: TRALOnReplyRef);
begin
  inherited Create(AOwner);
  FHandler := AHandler;
end;

procedure TRALAnonReply.Reply(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  if Assigned(FHandler) then
    FHandler(ARequest, AResponse);
end;

{ TRALServerAnonHelper }

function TRALServerAnonHelper.Map(const ARoute: StringRAL; AMethods: TRALMethods;
  const AHandler: TRALOnReplyRef): TRALRoute;
var
  vReply: TRALAnonReply;
begin
  vReply := TRALAnonReply.CreateFor(Self, AHandler);
  Result := CreateRoute(ARoute, {$IFDEF FPC}@{$ENDIF}vReply.Reply);
  Result.AllowedMethods := AMethods;
end;

function TRALServerAnonHelper.Delete(const ARoute: StringRAL;
  const AHandler: TRALOnReplyRef): TRALRoute;
begin
  Result := Map(ARoute, [amDELETE], AHandler);
end;

function TRALServerAnonHelper.Get(const ARoute: StringRAL;
  const AHandler: TRALOnReplyRef): TRALRoute;
begin
  Result := Map(ARoute, [amGET], AHandler);
end;

function TRALServerAnonHelper.Patch(const ARoute: StringRAL;
  const AHandler: TRALOnReplyRef): TRALRoute;
begin
  Result := Map(ARoute, [amPATCH], AHandler);
end;

function TRALServerAnonHelper.Post(const ARoute: StringRAL;
  const AHandler: TRALOnReplyRef): TRALRoute;
begin
  Result := Map(ARoute, [amPOST], AHandler);
end;

function TRALServerAnonHelper.Put(const ARoute: StringRAL;
  const AHandler: TRALOnReplyRef): TRALRoute;
begin
  Result := Map(ARoute, [amPUT], AHandler);
end;

{ TRALModuleAnonHelper }

function TRALModuleAnonHelper.Map(const ARoute: StringRAL; AMethods: TRALMethods;
  const AHandler: TRALOnReplyRef): TRALRoute;
var
  vReply: TRALAnonReply;
begin
  vReply := TRALAnonReply.CreateFor(Self, AHandler);
  Result := CreateRoute(ARoute, {$IFDEF FPC}@{$ENDIF}vReply.Reply);
  Result.AllowedMethods := AMethods;
end;

function TRALModuleAnonHelper.Delete(const ARoute: StringRAL;
  const AHandler: TRALOnReplyRef): TRALRoute;
begin
  Result := Map(ARoute, [amDELETE], AHandler);
end;

function TRALModuleAnonHelper.Get(const ARoute: StringRAL;
  const AHandler: TRALOnReplyRef): TRALRoute;
begin
  Result := Map(ARoute, [amGET], AHandler);
end;

function TRALModuleAnonHelper.Patch(const ARoute: StringRAL;
  const AHandler: TRALOnReplyRef): TRALRoute;
begin
  Result := Map(ARoute, [amPATCH], AHandler);
end;

function TRALModuleAnonHelper.Post(const ARoute: StringRAL;
  const AHandler: TRALOnReplyRef): TRALRoute;
begin
  Result := Map(ARoute, [amPOST], AHandler);
end;

function TRALModuleAnonHelper.Put(const ARoute: StringRAL;
  const AHandler: TRALOnReplyRef): TRALRoute;
begin
  Result := Map(ARoute, [amPUT], AHandler);
end;

{$ENDIF}

end.
