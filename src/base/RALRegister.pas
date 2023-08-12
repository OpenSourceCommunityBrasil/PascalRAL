unit RALRegister;
{$I PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  {$IFDEF DELPHIXE3UP}
  DesignIntf, ToolsAPI,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils,
  RALConsts, RALAuthentication;

procedure Register;

implementation

// this allow to put a nice entry in the delphi
// ide splash screen and about box

procedure Register;
{$IFDEF DELPHIXE3UP}
var
  AboutSvcs: IOTAAboutBoxServices;
  {$ENDIF}
begin
  {$IFDEF DELPHIXE3UP}
  if Assigned(SplashScreenServices) then
    SplashScreenServices.AddPluginBitmap('Pascal REST API Lite (RAL) Components',
      loadbitmap(HInstance, 'PascalRAL'), false, 'OpenSource - v' + RALVersion);
  if (BorlandIDEServices <> nil) and supports(BorlandIDEServices, IOTAAboutBoxServices,
    AboutSvcs) then
    AboutSvcs.AddPluginInfo('PascalRAL v' + RALVersion, 'PascalRAL' + sLineBreak +
      'OpenSource REST API Components' + sLineBreak + sLineBreak +
      'https://github.com/OpenSourceCommunityBrasil/PascalRAL',
      loadbitmap(HInstance, 'PascalRAL'), False, 'OpenSource');
  {$ENDIF}
  RegisterComponents('RAL - ServerAuths', [TRALServerBasicAuth, TRALServerJWTAuth]);
  RegisterComponents('RAL - ClientAuths', [TRALClientBasicAuth, TRALClientJWTAuth]);
end;

{$IFDEF FPC}

initialization

{$I pascalral.lrs}
{$ENDIF}

end.
