program TestServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, pascalral, indyral, synopseral, fphttpral, saguiral, uroutes
  { you can add units after this };

{$R *.res}

begin
  {$ifopt D+}
  SetHeapTraceOutput('debug.trc');
  GlobalSkipIfNoLeaks := True; // supported as of debugger version 3.2.0
  {$endif}
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

