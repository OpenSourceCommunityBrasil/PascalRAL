program RALInstaller;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}cthreads, {$ENDIF}
 {$IFDEF HASAMIGA}athreads, {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, principal, ghrepofunctions, configdatabase, imagefunctions,
  lclfunctions, frame_ide_versions, frame_modelo, frame_ide, frame_idioma,
  frame_config_recursos, frame_install_recursos,
  frame_install, frame_confirm_recursos, install_types, install_tools;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled :=True;
  Application.Initialize;
  Application.CreateForm(Tfprincipal, fprincipal);
  Application.Run;
end.
