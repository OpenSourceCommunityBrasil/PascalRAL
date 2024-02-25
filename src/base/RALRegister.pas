unit RALRegister;
{$I PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  {$IFDEF DELPHI2005UP}
  ToolsAPI,
  {$ENDIF}
  {$IFDEF RALWindows}
  Windows,
  {$ENDIF}
  Classes, SysUtils,
  RALConsts, RALAuthentication, RALWebModule;

procedure Register;

implementation

// this allow to put a nice entry in the delphi
// ide splash screen and about box

procedure Register;
{$IFDEF DELPHI2005UP}
var
  AboutSvcs: IOTAAboutBoxServices;
  {$ENDIF}
begin
  {$IFDEF DELPHI2005UP}
  // add project info to IDE's splash screen
  if Assigned(SplashScreenServices) then
    SplashScreenServices.AddPluginBitmap(RALPACKAGENAME,
      loadbitmap(HInstance, RALPACKAGESHORT), false, RALPACKAGELICENSEVERSION);

  // add project info to IDE's help panels
  if (BorlandIDEServices <> nil) and supports(BorlandIDEServices, IOTAAboutBoxServices,
    AboutSvcs) then
    AboutSvcs.AddPluginInfo(RALPACKAGESHORTLICENSE, RALPACKAGESHORT + sLineBreak +
      RALPACKAGENAME + sLineBreak + sLineBreak + RALPACKAGESITE,
      loadbitmap(HInstance, RALPACKAGESHORT), false, RALPACKAGELICENSE);
  {$ENDIF}
  RegisterComponents('RAL - Server', [TRALServerBasicAuth, TRALServerJWTAuth, TRALWebModule]);
  RegisterComponents('RAL - Client', [TRALClientBasicAuth, TRALClientJWTAuth]);
end;

{$IFDEF FPC}
initialization
{$I pascalral.lrs}
{$ENDIF}

end.
