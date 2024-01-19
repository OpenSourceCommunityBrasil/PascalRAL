unit RALWebModule;

interface

uses
  Classes, SysUtils,
  RALServer, RALTypes, RALRoutes, RALRequest, RALResponse;

type
  { TRALWebModule }

  TRALWebModule = class(TRALModuleRoutes)
  private
    FDocumentRoot : StringRAL;
  protected
    procedure WebModFile(ARequest : TRALRequest; AResponse : TRALResponse);
    function GetFileRoute(ARequest: TRALRequest) : StringRAL;
  public
    constructor Create(AOwner : TComponent); override;

    function CanResponseRoute(ARequest : TRALRequest) : TRALRoute; override;
  published
    property DocumentRoot : StringRAL read FDocumentRoot write FDocumentRoot;
  end;

implementation

{ TRALWebModule }

function TRALWebModule.CanResponseRoute(ARequest: TRALRequest): TRALRoute;
begin
  Result := nil;
  if GetFileRoute(ARequest) <> '' then
    Result := TRALRoute(Routes.Items[0]);
end;

constructor TRALWebModule.Create(AOwner: TComponent);
begin
  inherited;
  CreateRoute('webmdfile', {$IFDEF FPC}@{$ENDIF}WebModFile);
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

procedure TRALWebModule.WebModFile(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vFile : StringRAL;
  vStream : TFileStream;
begin
  vFile := GetFileRoute(ARequest);
  if vFile <> '' then
  begin
    vStream := TFileStream.Create(vFile, fmOpenRead or fmShareDenyWrite);
    try
      AResponse.ResponseStream := vStream;
    finally
      FreeAndNil(vStream);
    end;
  end
  else begin
    AResponse.Answer(404);
  end;
end;

end.
