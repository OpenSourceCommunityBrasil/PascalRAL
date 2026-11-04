/// Registers the reverse proxy and the load balancer (TRALUpstream,
/// TRALProxyModule, TRALBalancer) in the IDE, with the verbs "Test servers"
/// (upstream: asks each server now) and "Test port" (balancer: whether its port
/// opens and its certificates load).
unit RALModuleProxyReg;

{$I ..\..\..\core\PascalRAL.inc}

interface

uses
  {$IFDEF FPC}
  LResources, ComponentEditors, Dialogs,
  {$ELSE}
  DesignEditors, DesignIntf,
  {$IFDEF DELPHIXE2UP}Vcl.Dialogs,{$ELSE}Dialogs,{$ENDIF}
  {$ENDIF}
  Classes, SysUtils,
  RALTypes, RALConsts, RALProxyCore, RALProxyModule, RALBalancer;

type
  /// Verb of TRALUpstream: asks every server for the health check path now.
  TRALUpstreamEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  /// Verb of TRALBalancer: whether its port is free and its certificates load.
  TRALBalancerEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

/// Registers the components and their editors.
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RAL - Modules', [TRALUpstream, TRALProxyModule]);
  RegisterComponents('RAL - Server', [TRALBalancer]);
  RegisterComponentEditor(TRALUpstream, TRALUpstreamEditor);
  RegisterComponentEditor(TRALBalancer, TRALBalancerEditor);
end;

{ TRALUpstreamEditor }

procedure TRALUpstreamEditor.ExecuteVerb(AIndex: Integer);
var
  vReport: StringRAL;
begin
  if AIndex <> 0 then
    Exit;
  TRALUpstream(Component).TestServers(vReport);
  ShowMessage(string(vReport));
end;

function TRALUpstreamEditor.GetVerb(AIndex: Integer): string;
begin
  Result := cmProxyTestServers;
end;

function TRALUpstreamEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TRALBalancerEditor }

procedure TRALBalancerEditor.ExecuteVerb(AIndex: Integer);
var
  vReport: StringRAL;
begin
  if AIndex <> 0 then
    Exit;
  TRALBalancer(Component).TestPort(vReport);
  ShowMessage(string(vReport));
end;

function TRALBalancerEditor.GetVerb(AIndex: Integer): string;
begin
  Result := cmProxyTestPort;
end;

function TRALBalancerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF FPC}
initialization
{$I ralmoduleproxydsgn.lrs}
{$ENDIF}

end.
