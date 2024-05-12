unit RALSwaggerModule;

interface

uses
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  Classes, SysUtils,
  RALServer, RALTypes, RALRoutes, RALRequest, RALResponse;

type
  { TRALSwaggerModule }

  TRALSwaggerModule = class(TRALModuleRoutes)
  private
    FDocumentRoot : StringRAL;
    FExported : boolean;
    FDefautRoute : TRALRoute;
  protected
    procedure SwaggerFile(ARequest : TRALRequest; AResponse : TRALResponse);
    procedure SwaggerIndex(ARequest : TRALRequest; AResponse : TRALResponse);
    function GetFileRoute(ARequest: TRALRequest) : StringRAL;
    procedure CreateSwaggerDirectory;
    procedure SaveResourceStream(AResourceName, AFileName : StringRAL);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function CanAnswerRoute(ARequest : TRALRequest; AResponse : TRALResponse) : TRALRoute; override;
    function IsDomain : boolean; override;
    function GetListRoutes : TList; override;
  published
    property DocumentRoot : StringRAL read FDocumentRoot write FDocumentRoot;
  end;

implementation

{$IFNDEF FPC}
  {$R RALSwaggerModule.res}
{$ENDIF}

uses
  RALSwaggerExporter;

{ TRALSwaggerModule }

function TRALSwaggerModule.CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
begin
  Result := Routes.CanAnswerRoute(ARequest);

  if (Result = nil) and (GetFileRoute(ARequest) <> '') then
    Result := FDefautRoute
  else if (Result <> nil) and (not Assigned(Result.OnReply)) then
    Result.OnReply := {$IFDEF FPC}@{$ENDIF}SwaggerFile;

  if Result <> nil then begin
    ARequest.ContentDispositionInline := True;
    AResponse.ContentDispositionInline := True;
  end;
end;

constructor TRALSwaggerModule.Create(AOwner: TComponent);
var
  vRoute : TRALRoute;
begin
  inherited;
  vRoute := CreateRoute('/swagger', @SwaggerIndex);
  vRoute.AllowedMethods := [amGET];
  vRoute.SkipAuthMethods := [amALL];

  FDefautRoute := TRALRoute.Create(nil);
  FDefautRoute.SkipAuthMethods := [amALL];
  FDefautRoute.OnReply := {$IFDEF FPC}@{$ENDIF}SwaggerFile;

  FDocumentRoot := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)) + 'swagger');
  FExported := False;
end;

destructor TRALSwaggerModule.Destroy;
begin
  FreeAndNil(FDefautRoute);
  inherited;
end;

procedure TRALSwaggerModule.SwaggerFile(ARequest: TRALRequest;
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

procedure TRALSwaggerModule.SwaggerIndex(ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  CreateSwaggerDirectory;
  AResponse.Answer(FDocumentRoot + 'swagger.html');
end;

function TRALSwaggerModule.GetFileRoute(ARequest: TRALRequest): StringRAL;
var
  vDir, vFile : StringRAL;
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

procedure TRALSwaggerModule.CreateSwaggerDirectory;
var
  vSwagger : TRALSwaggerExporter;
begin
  if not FExported then
  begin
    ForceDirectories(FDocumentRoot);

    SaveResourceStream('SWAGGER_INDEX', 'swagger.html');
    SaveResourceStream('SWAGGER_CSS', 'swagger.css');
    SaveResourceStream('SWAGGER_UI_CSS', 'swagger-ui.css');
    SaveResourceStream('SWAGGER_UI_BUNDLE_JS', 'swagger-ui-bundle.js');
    SaveResourceStream('SWAGGER_UI_BUNDLE_MAP', 'swagger-ui-bundle.js.map');
    SaveResourceStream('SWAGGER_UI_STANDALONE', 'swagger-ui-standalone-preset.js');
    SaveResourceStream('SWAGGER_INITIALIZER', 'swagger-initializer.js');

    vSwagger := TRALSwaggerExporter.Create;
    try
      vSwagger.ExportToFile(Server, FDocumentRoot + 'swagger.json');
    finally
      vSwagger.Free;
    end;

    FExported := True;
  end;
end;

procedure TRALSwaggerModule.SaveResourceStream(AResourceName,
  AFileName: StringRAL);
var
  vRes : TStream;
begin
  {$IFDEF FPC}
    vRes := TLazarusResourceStream.Create(AResourceName, 'RCDATA');
    try
      TLazarusResourceStream(vRes).SaveToFile(FDocumentRoot + AFileName);
    finally
      vRes.Free;
    end;
  {$ELSE}
    vRes := TResourceStream.Create(HINSTANCE, AResourceName, 'RCDATA');
    try
      TResourceStream(vRes).SaveToFile(FDocumentRoot + AFileName);
    finally
      vRes.Free;
    end;
  {$ENDIF}
end;

function TRALSwaggerModule.IsDomain: boolean;
begin
  Result := False;
end;

function TRALSwaggerModule.GetListRoutes: TList;
begin
  // nao devolse as rotas
  Result := TList.Create;
end;

{$IFDEF FPC}
initialization
  {$I RALSwaggerModule.lrs}
{$ENDIF}


end.
