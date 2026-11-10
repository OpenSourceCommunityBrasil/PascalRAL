/// The JSON-RPC 2.0 module: one POST route that answers calls with a dispatcher.
unit RALJSONRPCModule;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALMIMETypes, RALServer, RALRoutes, RALRequest, RALResponse,
  RALJSONRPC;

const
  /// TRALJSONRPCModule.Path when nobody says otherwise.
  cRALJSONRPCPath = '/rpc';

type
  { Answers JSON-RPC 2.0 over HTTP on one POST route (Path, under Domain):
    single calls, batches and notifications (204 when nothing is answered).
    The TRALRequest reaches the methods as TRALJSONRPCRequest.Context. }
  TRALJSONRPCModule = class(TRALModuleRoutes)
  private
    FDispatcher: TRALJSONRPCDispatcher;
    FPath: StringRAL;
    /// The route of Path.
    FRoute: TRALRoute;

    function GetErrorDetails: Boolean;
    function GetMaxBatch: IntegerRAL;
    function GetMethods: TRALJSONRPCMethods;
    function GetOnUnknownMethod: TRALJSONRPCUnknownEvent;
    /// Answers the calls of a request.
    procedure Reply(ARequest: TRALRequest; AResponse: TRALResponse);
    procedure SetErrorDetails(AValue: Boolean);
    procedure SetMaxBatch(AValue: IntegerRAL);
    procedure SetMethods(AValue: TRALJSONRPCMethods);
    procedure SetOnUnknownMethod(AValue: TRALJSONRPCUnknownEvent);
    procedure SetPath(const AValue: StringRAL);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function DefaultContentType: StringRAL; override;

    /// The dispatcher that runs the calls; other transports may share it.
    property Dispatcher: TRALJSONRPCDispatcher read FDispatcher;
  published
    /// Puts the message of an exception a method raised in the error data.
    property ErrorDetails: Boolean read GetErrorDetails write SetErrorDetails default False;
    /// The most calls one batch may hold; more is Invalid Request. 0 = no limit.
    property MaxBatch: IntegerRAL read GetMaxBatch write SetMaxBatch default 0;
    /// The methods answered.
    property Methods: TRALJSONRPCMethods read GetMethods write SetMethods;
    /// Answers calls to methods not in Methods.
    property OnUnknownMethod: TRALJSONRPCUnknownEvent read GetOnUnknownMethod
      write SetOnUnknownMethod;
    /// The route that takes the calls, under Domain; empty is taken as /.
    property Path: StringRAL read FPath write SetPath;
  end;

implementation

{ TRALJSONRPCModule }

constructor TRALJSONRPCModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDispatcher := TRALJSONRPCDispatcher.Create(Self);
  FPath := cRALJSONRPCPath;
  FRoute := CreateRoute(FPath, {$IFDEF FPC}@{$ENDIF}Reply, 'JSON-RPC 2.0');
  FRoute.AllowedMethods := [amPOST];
end;

destructor TRALJSONRPCModule.Destroy;
begin
  FDispatcher.Free;
  inherited Destroy;
end;

function TRALJSONRPCModule.DefaultContentType: StringRAL;
begin
  Result := rctAPPLICATIONJSON;
end;

function TRALJSONRPCModule.GetErrorDetails: Boolean;
begin
  Result := FDispatcher.ErrorDetails;
end;

function TRALJSONRPCModule.GetMaxBatch: IntegerRAL;
begin
  Result := FDispatcher.MaxBatch;
end;

function TRALJSONRPCModule.GetMethods: TRALJSONRPCMethods;
begin
  Result := FDispatcher.Methods;
end;

function TRALJSONRPCModule.GetOnUnknownMethod: TRALJSONRPCUnknownEvent;
begin
  Result := FDispatcher.OnUnknownMethod;
end;

procedure TRALJSONRPCModule.Reply(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vAnswer: StringRAL;
begin
  vAnswer := FDispatcher.Execute(ARequest.RequestText, ARequest);
  if vAnswer = '' then
    AResponse.Answer(HTTP_NoContent, '', rctAPPLICATIONJSON)
  else
    AResponse.Answer(HTTP_OK, vAnswer, rctAPPLICATIONJSON);
end;

procedure TRALJSONRPCModule.SetErrorDetails(AValue: Boolean);
begin
  FDispatcher.ErrorDetails := AValue;
end;

procedure TRALJSONRPCModule.SetMaxBatch(AValue: IntegerRAL);
begin
  FDispatcher.MaxBatch := AValue;
end;

procedure TRALJSONRPCModule.SetMethods(AValue: TRALJSONRPCMethods);
begin
  FDispatcher.Methods := AValue;
end;

procedure TRALJSONRPCModule.SetOnUnknownMethod(AValue: TRALJSONRPCUnknownEvent);
begin
  FDispatcher.OnUnknownMethod := AValue;
end;

procedure TRALJSONRPCModule.SetPath(const AValue: StringRAL);
begin
  FPath := AValue;
  if FPath = '' then
    FRoute.Route := '/'
  else
    FRoute.Route := FPath;
end;

end.
