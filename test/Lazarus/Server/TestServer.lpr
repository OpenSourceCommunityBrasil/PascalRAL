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
  Forms, Main, pascalral, indyral, synopseral, fphttpral, uroutes
  { you can add units after this };

{$R *.res}

begin
  {$IFDEF DEBUG}
  SetHeapTraceOutput('debug.trc');
  GlobalSkipIfNoLeaks := True; // supported as of debugger version 3.2.0
  {$endIf}
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

