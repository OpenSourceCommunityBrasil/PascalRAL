/// Actions that connect, disconnect and publish with an MQTT or MQTT-SN client
/// from a button or a menu, with no code.
unit RALMQTTActions;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  ActnList,
  {$ELSE}
  {$IFDEF DELPHIXE3UP}System.Actions,{$ELSE}ActnList,{$ENDIF}
  {$ENDIF}
  RALTypes, RALMQTTClient;

type
  {$IF Defined(FPC) or not Defined(DELPHIXE3UP)}
  /// Base of the actions: TCustomAction of the ActnList unit.
  TRALMQTTActionBase = TCustomAction;
  {$ELSE}
  /// Base of the actions: TContainedAction, shared by VCL and FMX action lists.
  TRALMQTTActionBase = TContainedAction;
  {$IFEND}

  { Base of the MQTT actions: the client they act on. With an OnExecute of its
    own, the handler runs instead }
  TRALMQTTClientAction = class(TRALMQTTActionBase)
  private
    FClient: TRALMQTTCustomClient;

    procedure SetClient(const AValue: TRALMQTTCustomClient);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// Does the action; called by Execute when there is no OnExecute.
    procedure Act; virtual; abstract;
    /// Whether the action can be done now.
    function CanAct: boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    /// Does the action; True when it was done.
    function Execute: boolean; override;
    /// Enables the action when it can be done.
    function Update: boolean; override;
  published
    property Caption;
    /// Client the action acts on (TRALMQTTClient or TRALMQTTSNClient).
    property Client: TRALMQTTCustomClient read FClient write SetClient;
    property Enabled;
    property Hint;
    property ImageIndex;
    property OnExecute;
    property OnUpdate;
    property ShortCut;
    property Visible;
  end;

  /// Connects the client (Active := True); disabled while it is active.
  TRALMQTTConnect = class(TRALMQTTClientAction)
  protected
    procedure Act; override;
    function CanAct: boolean; override;
  end;

  /// Disconnects the client (Active := False); disabled while it is not active.
  TRALMQTTDisconnect = class(TRALMQTTClientAction)
  protected
    procedure Act; override;
    function CanAct: boolean; override;
  end;

  { Publishes the item of Client.Publications named Publication; disabled
    while the client is not active }
  TRALMQTTPublish = class(TRALMQTTClientAction)
  private
    FPublication: StringRAL;
  protected
    procedure Act; override;
    function CanAct: boolean; override;
  published
    /// Name of the item of Client.Publications published.
    property Publication: StringRAL read FPublication write FPublication;
  end;

implementation

{ TRALMQTTClientAction }

constructor TRALMQTTClientAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // the action works without a handler of its own
  DisableIfNoHandler := False;
end;

procedure TRALMQTTClientAction.SetClient(const AValue: TRALMQTTCustomClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALMQTTClientAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
end;

function TRALMQTTClientAction.CanAct: boolean;
begin
  Result := FClient <> nil;
end;

function TRALMQTTClientAction.Execute: boolean;
begin
  if Assigned(OnExecute) then
  begin
    Result := inherited Execute;
    Exit;
  end;
  Result := CanAct;
  if Result then
    Act;
end;

function TRALMQTTClientAction.Update: boolean;
begin
  Result := inherited Update;
  if not Assigned(OnUpdate) then
  begin
    Enabled := CanAct;
    Result := True;
  end;
end;

{ TRALMQTTConnect }

procedure TRALMQTTConnect.Act;
begin
  Client.Active := True;
end;

function TRALMQTTConnect.CanAct: boolean;
begin
  Result := (Client <> nil) and not Client.Active;
end;

{ TRALMQTTDisconnect }

procedure TRALMQTTDisconnect.Act;
begin
  Client.Active := False;
end;

function TRALMQTTDisconnect.CanAct: boolean;
begin
  Result := (Client <> nil) and Client.Active;
end;

{ TRALMQTTPublish }

procedure TRALMQTTPublish.Act;
var
  vItem: TRALMQTTPublicationItem;
begin
  vItem := Client.Publications.Find(FPublication);
  if vItem <> nil then
    vItem.Publish;
end;

function TRALMQTTPublish.CanAct: boolean;
begin
  Result := (Client <> nil) and Client.Active and (FPublication <> '') and
    (Client.Publications.Find(FPublication) <> nil);
end;

end.
