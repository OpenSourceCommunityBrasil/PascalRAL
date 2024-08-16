program ralinstaller;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  winpeimagereader, elfreader, machoreader, Forms, umain,
  ufrm_modelo, ufrm_idioma, utools, ufrm_ide, ufrm_ide_versions,
  ufrm_ide_version, delphiutils, lazarusutils, ufrm_recursos, udm, githubral,
  ufrm_install, installparser, githubutils, ralzipper, i18n_utils;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='RAL Installer';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(Tdm, dm);
  Application.CreateForm(Tfmain, fmain);
  Application.Run;
end.

