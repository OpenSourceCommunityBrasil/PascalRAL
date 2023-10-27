unit RALCGIServer;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  {$IFDEF FPC}
    fpCGI, fphttp, httpdefs,
  {$ELSE}
    Web.WebBroker, CGIApp, HTTPApp,
  {$ENDIF}
  RALServer, RALCGIServerDatamodule;

type
  { TRALServerCGI }

  TRALServerCGI = class(TRALServer)
  public
    constructor Create(AOwner : TComponent); override;
  end;

  { TRALCGI }

  TRALCGI = class
  private
    class var FServer : TRALServerCGI;
    {$IFDEF FPC}
      class function GetApplication: TCGIApplication;
      class procedure OnGetModule(Sender: TObject; ARequest: TRequest;
                                  var ModuleClass: TCustomHTTPModuleClass);
    {$ENDIF}
  public
    class procedure Initialize;
    class procedure Finalize;
    class function GetServer : TRALServerCGI;
  end;

implementation

{ TRALCGI }

class function TRALCGI.GetServer: TRALServerCGI;
begin
  if FServer = nil then begin
    FServer := TRALServerCGI.Create(nil);
    FServer.BruteForceProtection.Enabled := False;
  end;
  Result := FServer;
end;

class procedure TRALCGI.Finalize;
begin
  if FServer <> nil then
    FServer.Free;
end;

{$IFDEF FPC}
  class function TRALCGI.GetApplication : TCGIApplication;
  begin
    Result := Application;
  end;

  class procedure TRALCGI.OnGetModule(Sender : TObject; ARequest : TRequest;
                                      var ModuleClass : TCustomHTTPModuleClass);
  begin
    ModuleClass := TRALWebModule;
  end;

  class procedure TRALCGI.Initialize;
  begin
    Application.AllowDefaultModule := True;
    Application.OnGetModule := @OnGetModule;
    Application.LegacyRouting := True;
    Application.Initialize;
    Application.Run;
  end;
{$ELSE}
  class procedure TRALCGI.Initialize;
  begin
    Application.Initialize;
    Application.WebModuleClass := RALWebModuleClass;
    Application.Run;
  end;
{$ENDIF}

{ TRALServerCGI }

constructor TRALServerCGI.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF FPC}
    SetEngine('CGI Lazarus');
  {$ELSE}
    SetEngine('CGI Delphi');
  {$ENDIF}
end;

initialization

//finalization
//  TRALCGI.Finalize;

end.
