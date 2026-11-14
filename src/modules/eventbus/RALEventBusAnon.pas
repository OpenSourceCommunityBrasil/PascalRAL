{ Anonymous handlers for the event bus, where the compiler has anonymous methods:
  Bus.SubscribeProc('pedido/#', procedure(AEvent: TRALEvent) begin ... end). Elsewhere
  the unit is empty and the handlers are methods (Subscribe). }
unit RALEventBusAnon;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$IFDEF RALHasAnonMethods}
    {$MODESWITCH FUNCTIONREFERENCES}
    {$MODESWITCH ANONYMOUSFUNCTIONS}
  {$ENDIF}
{$ENDIF}

interface

{$IFDEF RALHasAnonMethods}
uses
  Classes, SysUtils,
  RALTypes, RALEventBus;

type
  /// An anonymous handler.
  TRALEventProc = reference to procedure(AEvent: TRALEvent);

  { Holds an anonymous handler as a method the bus can call; owned by the bus, so it
    goes away with it, or with UnsubscribeProc. }
  TRALEventProcAdapter = class(TComponent)
  private
    /// The anonymous handler.
    FProc: TRALEventProc;
  public
    /// Calls the anonymous handler.
    procedure Handle(Sender: TObject; AEvent: TRALEvent);
  end;

  /// The anonymous subscriptions of TRALEventBus.
  TRALEventBusHelper = class helper for TRALEventBus
  public
    /// Subscribes AProc to AFilter; returns the number UnsubscribeProc takes.
    function SubscribeProc(const AFilter: StringRAL; AProc: TRALEventProc): Integer;
    /// Removes a subscription SubscribeProc made, and its handler.
    procedure UnsubscribeProc(AId: Integer);
  end;
{$ENDIF}

implementation

{$IFDEF RALHasAnonMethods}

{ TRALEventProcAdapter }

procedure TRALEventProcAdapter.Handle(Sender: TObject; AEvent: TRALEvent);
begin
  if Assigned(FProc) then
    FProc(AEvent);
end;

{ TRALEventBusHelper }

function TRALEventBusHelper.SubscribeProc(const AFilter: StringRAL;
  AProc: TRALEventProc): Integer;
var
  vAdapter: TRALEventProcAdapter;
begin
  vAdapter := TRALEventProcAdapter.Create(Self);
  vAdapter.FProc := AProc;
  try
    Result := Subscribe(AFilter, vAdapter.Handle);
  except
    vAdapter.Free;
    raise;
  end;
  vAdapter.Tag := Result;
end;

procedure TRALEventBusHelper.UnsubscribeProc(AId: Integer);
var
  vIdx: Integer;
begin
  Unsubscribe(AId);
  for vIdx := ComponentCount - 1 downto 0 do
    if (Components[vIdx] is TRALEventProcAdapter) and (Components[vIdx].Tag = AId) then
    begin
      Components[vIdx].Free;
      Break;
    end;
end;

{$ENDIF}

end.
