unit RALWEBModule;

interface

uses
  Classes, SysUtils,
  RALServer, RALTypes, RALRoutes, RALRequest, RALResponse;

type
  { TRALWebModule }

  TRALWebModule = class(TRALSubRoutes)
  private
    FDocumentRoot : StringRAL;
  protected
    function GetRouteAddress(ARoute: StringRAL): TRALRoute; override;
    procedure WebModFile(ARequest : TRALRequest; AResponse : TRALResponse);
  public
    constructor Create(AOwner : TComponent); override;
  published
    property DocumentRoot : StringRAL read FDocumentRoot write FDocumentRoot;
  end;

implementation

{ TRALWebModule }

constructor TRALWebModule.Create(AOwner: TComponent);
begin
  inherited;
  CreateRoute('webmdfile', {$IFDEF FPC}@{$ENDIF}WebModFile);
end;

function TRALWebModule.GetRouteAddress(ARoute: StringRAL): TRALRoute;
begin
  Result := TRALRoute(Routes.Items[0]);
end;

procedure TRALWebModule.WebModFile(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vDir, vFile : StringRAL;
  vStream : TFileStream;
begin
  vDir := FDocumentRoot;
  if (Trim(vDir) = '') or (not DirectoryExists(vDir)) then
    vDir := ExtractFilePath(ParamStr(0));

  vDir := IncludeTrailingPathDelimiter(vDir);

  SetCurrentDir(vDir);
  vFile := ExpandFileName(ARequest.Query);

  // verificando se a base folder (document root) é o mesmo
  if (SameText(Copy(vFile, 1, Length(vDir)), vDir)) and
     (FileExists(vFile)) then
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
