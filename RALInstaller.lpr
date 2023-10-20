program RALInstaller;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}cthreads, {$ENDIF}
 {$IFDEF HASAMIGA}athreads, {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  principal in 'src\telas\principal.pas',
  ghrepofunctions in 'src\funcoes\ghrepofunctions.pas',
  configdatabase in 'src\funcoes\configdatabase.pas',
  imagefunctions in 'src\funcoes\imagefunctions.pas',
  lclfunctions in 'src\funcoes\lclfunctions.pas';

{$R *.res}

begin
  {$ifopt D+}
  SetHeapTraceOutput('debug.trc');
    {$if declared(UseHeapTrace)}
    GlobalSkipIfNoLeaks := True; // supported as of debugger version 3.2.0
    {$endIf}
  {$IFEND}
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(Tfprincipal, fprincipal);
  Application.Run;
end.
