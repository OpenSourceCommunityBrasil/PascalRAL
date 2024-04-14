unit RALWebModule;

interface

uses
  Classes, SysUtils,
  RALServer, RALTypes, RALRoutes, RALRequest, RALResponse;

type
  { TRALWebModule }

  TRALWebModule = class(TRALModuleRoutes)
  private
    FDefautRoute : TRALRoute;
    FDocumentRoot : StringRAL;
  protected
    procedure WebModFile(ARequest : TRALRequest; AResponse : TRALResponse);
    function GetFileRoute(ARequest: TRALRequest) : StringRAL;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function CanResponseRoute(ARequest : TRALRequest) : TRALRoute; override;
    function IsDomain : boolean; override;
  published
    property DocumentRoot : StringRAL read FDocumentRoot write FDocumentRoot;
    property Routes;
  end;

implementation

{ TRALWebModule }

function TRALWebModule.CanResponseRoute(ARequest: TRALRequest): TRALRoute;
var
  vRoute : StringRAL;
begin
  vRoute := ARequest.Query;
  if (vRoute <> '') and (vRoute[PosIniStr] = '/') then
    Delete(vRoute, 1, 1);

  Result := Routes.CanResponseRoute(ARequest);

  if (Result = nil) and (GetFileRoute(ARequest) <> '') then
    Result := FDefautRoute
  else if (Result <> nil) and (not Assigned(Result.OnReply)) then
    Result.OnReply := {$IFDEF FPC}@{$ENDIF}WebModFile;
end;

constructor TRALWebModule.Create(AOwner: TComponent);
begin
  inherited;
  FDefautRoute := TRALRoute.Create(nil);
  FDefautRoute.SkipAuthMethods := [amALL];
  FDefautRoute.OnReply := {$IFDEF FPC}@{$ENDIF}WebModFile;
end;

destructor TRALWebModule.Destroy;
begin
  FreeAndNil(FDefautRoute);
  inherited;
end;

function TRALWebModule.GetFileRoute(ARequest: TRALRequest): StringRAL;
var
  vDir, vFile : StringRAL;
  vStream : TFileStream;
begin
  Result := '';
  vDir := FDocumentRoot;
  if (Trim(vDir) = '') or (not DirectoryExists(vDir)) then
    vDir := ExtractFilePath(ParamStr(0));

  vDir := IncludeTrailingPathDelimiter(vDir);

  SetCurrentDir(vDir);
  vFile := ARequest.Query;
  Delete(vFile, 1, 1);
  vFile := ExpandFileName(vFile);

  // verificando se a base folder (document root) é o mesmo
  if (SameText(Copy(vFile, 1, Length(vDir)), vDir)) and
     (FileExists(vFile)) then
    Result := vFile;
end;

function TRALWebModule.IsDomain: boolean;
begin
  Result := False;
end;

procedure TRALWebModule.WebModFile(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vFile : StringRAL;
begin
  vFile := GetFileRoute(ARequest);
  if vFile <> '' then
    AResponse.Answer(vFile)
  else
    AResponse.Answer(404);
end;

end.
