/// Runs a slow job (IDE search, download) outside the main thread, so the
/// window keeps answering; the end is reported back in the main thread.
unit RALInst.Tela.Tarefa;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  /// A job run in a background thread. The job must not touch the screen: it
  /// reports progress with TThread.Queue, and the result in AoTerminar, which
  /// runs in the main thread.
  TTarefa = class(TThread)
  private
    FAoTerminar: TNotifyEvent;
    FErro: string;
    FTrabalho: TThreadMethod;
    /// Calls AoTerminar in the main thread
    procedure Avisar;
  protected
    procedure Execute; override;
  public
    /// Starts at once; the owner frees it after AoTerminar (never inside it)
    constructor Create(ATrabalho: TThreadMethod; AAoTerminar: TNotifyEvent);

    /// Message of the exception that stopped the job ('' when none)
    property Erro: string read FErro;
  end;

implementation

{ TTarefa }

constructor TTarefa.Create(ATrabalho: TThreadMethod; AAoTerminar: TNotifyEvent);
begin
  FTrabalho := ATrabalho;
  FAoTerminar := AAoTerminar;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TTarefa.Avisar;
begin
  if Assigned(FAoTerminar) then
    FAoTerminar(Self);
end;

procedure TTarefa.Execute;
begin
  try
    FTrabalho;
  except
    on E: Exception do
      FErro := E.Message;
  end;
  Synchronize(@Avisar);
end;

end.
