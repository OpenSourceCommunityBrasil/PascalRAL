program RALInstaller;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}cthreads,  {$ENDIF}
 {$IFDEF HASAMIGA}athreads,  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  principal,
  ghrepofunctions,
  configdatabase,
  imagefunctions,
  lclfunctions,
  frideversions,
  frmodelo,
  fride,
  fridioma,
  frconfigrecursos,
  frinstallrecursos,
  frinstall,
  frconfirmrecursos,
  install_types,
  install_tools;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(Tfprincipal, fprincipal);
  Application.Run;
end.
