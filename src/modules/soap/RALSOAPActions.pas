/// Action that calls an operation of a TRALSOAPClient from a button or a menu.
unit RALSOAPActions;

{$I ../../base/PascalRAL.inc}

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
  RALTypes, RALSOAPClient;

type
  {$IF Defined(FPC) or not Defined(DELPHIXE3UP)}
  /// Base of the action: TCustomAction of the ActnList unit.
  TRALSOAPActionBase = TCustomAction;
  {$ELSE}
  /// Base of the action: TContainedAction, shared by VCL and FMX action lists.
  TRALSOAPActionBase = TContainedAction;
  {$IFEND}

  { Calls Operation of Client when executed; with an OnExecute of its own, the
    handler runs instead. Enabled only with both set. }
  TRALSOAPExecute = class(TRALSOAPActionBase)
  private
    FClient: TRALSOAPClient;
    FExecBehavior: TRALExecBehavior;
    FOperation: StringRAL;

    procedure SetClient(const AValue: TRALSOAPClient);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;

    /// Calls the operation; True when it was called.
    function Execute: boolean; override;
    /// Enables the action when Client and Operation are set.
    function Update: boolean; override;
  published
    property Caption;
    /// Client whose operation is called.
    property Client: TRALSOAPClient read FClient write SetClient;
    property Enabled;
    /// ebMultiThread returns at once; the answer arrives in the main thread.
    property ExecBehavior: TRALExecBehavior read FExecBehavior write FExecBehavior
      default ebSingleThread;
    property Hint;
    property ImageIndex;
    property OnExecute;
    property OnUpdate;
    /// Name of the operation called.
    property Operation: StringRAL read FOperation write FOperation;
    property ShortCut;
    property Visible;
  end;

implementation

{ TRALSOAPExecute }

constructor TRALSOAPExecute.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExecBehavior := ebSingleThread;
  // the action works without a handler of its own
  DisableIfNoHandler := False;
end;

procedure TRALSOAPExecute.SetClient(const AValue: TRALSOAPClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALSOAPExecute.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
end;

function TRALSOAPExecute.Execute: boolean;
begin
  if Assigned(OnExecute) then
  begin
    Result := inherited Execute;
    Exit;
  end;
  Result := (FClient <> nil) and (FOperation <> '');
  if Result then
    FClient.Execute(FOperation, FExecBehavior);
end;

function TRALSOAPExecute.Update: boolean;
begin
  Result := inherited Update;
  if not Assigned(OnUpdate) then
  begin
    Enabled := (FClient <> nil) and (FOperation <> '');
    Result := True;
  end;
end;

end.
