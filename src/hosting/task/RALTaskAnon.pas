{ Anonymous work for TRALTask, where the compiler has anonymous methods:
  TRALTask.FromProc(procedure(ATask: TRALTask) begin ... end).ContinueWith(...).Run.
  Elsewhere the unit is empty and the work is a method (OnExecute). }
unit RALTaskAnon;

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
  RALTask;

type
  /// Anonymous work.
  TRALTaskProc = reference to procedure(ATask: TRALTask);

  /// Holds anonymous work as the method a task calls; owned by the task.
  TRALTaskProcAdapter = class(TComponent)
  private
    /// The work.
    FProc: TRALTaskProc;
  public
    /// Runs the work.
    procedure Handle(Sender: TRALTask);
  end;

  /// The anonymous side of TRALTask.
  TRALTaskHelper = class helper for TRALTask
  public
    { Adds AProc at the end of the chain of Next (it receives the Output before it as
      Input) and returns the first task, so calls chain: .ContinueWith(a).Run. }
    function ContinueWith(AProc: TRALTaskProc): TRALTask;
    { A new task (not running) whose work is AProc; the caller frees it, which frees
      the tasks ContinueWith added. }
    class function FromProc(AProc: TRALTaskProc): TRALTask;
  end;
{$ENDIF}

implementation

{$IFDEF RALHasAnonMethods}

/// A task owned by AOwner whose work is AProc.
function NewProcTask(AOwner: TComponent; AProc: TRALTaskProc): TRALTask;
var
  vAdapter: TRALTaskProcAdapter;
begin
  Result := TRALTask.Create(AOwner);
  vAdapter := TRALTaskProcAdapter.Create(Result);
  vAdapter.FProc := AProc;
  Result.OnExecute := vAdapter.Handle;
end;

{ TRALTaskProcAdapter }

procedure TRALTaskProcAdapter.Handle(Sender: TRALTask);
begin
  if Assigned(FProc) then
    FProc(Sender);
end;

{ TRALTaskHelper }

function TRALTaskHelper.ContinueWith(AProc: TRALTaskProc): TRALTask;
var
  vLast: TRALTask;
begin
  vLast := Self;
  while vLast.Next <> nil do
    vLast := vLast.Next;
  vLast.Next := NewProcTask(Self, AProc);
  Result := Self;
end;

class function TRALTaskHelper.FromProc(AProc: TRALTaskProc): TRALTask;
begin
  Result := NewProcTask(nil, AProc);
end;

{$ENDIF}

end.
