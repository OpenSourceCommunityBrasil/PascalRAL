unit RALCGIServer;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  {$IFDEF FPC}
    fpCGI, fphttp, httpdefs
  {$ELSE}
    WebBroker, CGIApp, HTTPApp
  {$ENDIF};

type
  { TRALCGI }

  TRALCGI = class
  private
    {$IFDEF FPC}
      class var FModule : TCustomHTTPModuleClass;
      class function GetApplication: TCGIApplication;
      class procedure OnGetModule(Sender: TObject; ARequest: TRequest;
                                  var ModuleClass: TCustomHTTPModuleClass);
    {$ELSE}
      class var FModule : TComponentClass;
    {$ENDIF}
  public
    class procedure Initialize;
    {$IFDEF FPC}
      class procedure SetModule(AModule : TCustomHTTPModuleClass);
    {$ELSE}
      class procedure SetModule(AModule : TComponentClass);
    {$ENDIF}
  end;

implementation

{ TRALCGI }

{$IFDEF FPC}
  class function TRALCGI.GetApplication : TCGIApplication;
  begin
    Result := Application;
  end;

  class procedure TRALCGI.OnGetModule(Sender : TObject; ARequest : TRequest;
                                      var ModuleClass : TCustomHTTPModuleClass);
  begin
    ModuleClass := FModule;
  end;

  class procedure TRALCGI.Initialize;
  begin
    Application.AllowDefaultModule := True;
    Application.OnGetModule := @OnGetModule;
    Application.LegacyRouting := True;
    Application.Initialize;
    Application.Run;
  end;

  class procedure TRALCGI.SetModule(AModule : TCustomHTTPModuleClass);
  begin
    FModule := AModule;
  end;

{$ELSE}
  class procedure TRALCGI.SetModule(AModule : TComponentClass);
  begin
    FModule := AModule;
  end;

  class procedure TRALCGI.Initialize;
  begin
    Application.Initialize;
    Application.WebModuleClass := FModule;
    Application.Run;
  end;
{$ENDIF}

end.
