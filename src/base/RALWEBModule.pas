/// Module unit with definitions of the web section of the package.
unit RALWebModule;

interface

uses
  Classes, SysUtils,
  RALServer, RALTypes, RALConsts, RALTools, RALRoutes, RALRequest, RALResponse, RALParams,
  RALThreadSafe;

type

  { TRALWebSession }

  /// Class for the Session object
  TRALWebSession = class
  private
    FLastDate: TDateTime;
    FObjects: TStringList;
  protected
    procedure ClearObjects;
    function GetObjects(AName: StringRAL): TObject;
    procedure SetObjects(AName: StringRAL; AValue: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    function DeleteObject(AName: StringRAL; AFree: boolean = True): boolean;

    property Objects[AName: StringRAL]: TObject read GetObjects write SetObjects;
  published
    property LastDate: TDateTime read FLastDate write FLastDate;
  end;

  { TRALWebModule }

  /// Class for the base object of the module
  TRALWebModule = class(TRALModuleRoutes)
  private
    FDefaultRoute: TRALRoute;
    FDocumentRoot: StringRAL;
    FIndexFile: StringRAL;
    FSessions: TRALStringListSafe;
  protected
    procedure CreateSession(ARequest: TRALRequest; AResponse: TRALResponse);
    function GetFileRoute(ARequest: TRALRequest): StringRAL;
    function GetWebSession(ARequest: TRALRequest): TRALWebSession;
    function NewSessionName: StringRAL;
    procedure SetDocumentRoot(AValue: StringRAL);
    procedure SetServer(AValue: TRALServer); override;
    procedure WebModFile(ARequest: TRALRequest; AResponse: TRALResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
      override;

    property Session[ARequest: TRALRequest]: TRALWebSession read GetWebSession;
  published
    property DocumentRoot: StringRAL read FDocumentRoot write SetDocumentRoot;
    property IndexFile: StringRAL read FIndexFile write FIndexFile;
    property Routes;
  end;

implementation

const
  RAL_SESSION: StringRAL = 'ral_websession';

{ TRALWebSession }

function TRALWebSession.GetObjects(AName: StringRAL): TObject;
var
  vInt: IntegerRAL;
begin
  Result := nil;
  vInt := FObjects.IndexOf(AName);
  if vInt >= 0 then
    Result := FObjects.Objects[vInt];
end;

procedure TRALWebSession.SetObjects(AName: StringRAL; AValue: TObject);
var
  vInt: IntegerRAL;
begin
  vInt := FObjects.IndexOf(AName);
  if vInt >= 0 then
  begin
    if AValue <> FObjects.Objects[vInt] then
      FObjects.Objects[vInt].Free;
    FObjects.Objects[vInt] := AValue;
  end
  else
  begin
    FObjects.AddObject(AName, AValue);
  end;
end;

procedure TRALWebSession.ClearObjects;
begin
  while FObjects.Count > 0 do
  begin
    FObjects.Objects[FObjects.Count - 1].Free;
    FObjects.Delete(FObjects.Count - 1);
  end;
end;

constructor TRALWebSession.Create;
begin
  inherited;
  FLastDate := Now;
  FObjects := TStringList.Create;
  FObjects.Sorted := True;
end;

destructor TRALWebSession.Destroy;
begin
  ClearObjects;
  FreeAndNil(FObjects);
  inherited Destroy;
end;

function TRALWebSession.DeleteObject(AName: StringRAL; AFree: boolean): boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  vInt := FObjects.IndexOf(AName);
  if vInt >= 0 then
  begin
    Result := True;
    FObjects.Objects[vInt].Free;
    FObjects.Delete(vInt);
  end;
end;

{ TRALWebModule }

function TRALWebModule.CanAnswerRoute(ARequest: TRALRequest; AResponse: TRALResponse): TRALRoute;
begin
  Result := Routes.CanAnswerRoute(ARequest);

  if (Result = nil) and (GetFileRoute(ARequest) <> '') then
    Result := FDefaultRoute
  else if (Result <> nil) and (not Assigned(Result.OnReply)) then
    Result.OnReply := {$IFDEF FPC}@{$ENDIF}WebModFile;

  //if Result <> nil then begin
  //  ARequest.ContentDispositionInline := True;
  //  AResponse.ContentDispositionInline := True;
  //  CreateSession(ARequest, AResponse);
  //end;
end;

constructor TRALWebModule.Create(AOwner: TComponent);
begin
  inherited;
  FIndexFile := '';
  FDefaultRoute := TRALRoute.Create(nil);
  FDefaultRoute.Route := '/';
  FDefaultRoute.SkipAuthMethods := [amALL];
  FDefaultRoute.OnReply := {$IFDEF FPC}@{$ENDIF}WebModFile;

  FSessions := TRALStringListSafe.Create;
end;

destructor TRALWebModule.Destroy;
begin
  FSessions.Clear(True);
  FreeAndNil(FSessions);
  FreeAndNil(FDefaultRoute);
  inherited;
end;

function TRALWebModule.GetFileRoute(ARequest: TRALRequest): StringRAL;
var
  vFile: StringRAL;
begin
  Result := '';
  //vDir := FDocumentRoot;
  //if (Trim(vDir) = '') or (not DirectoryExists(vDir)) then
  //  vDir := ExtractFilePath(ParamStr(0));

  //vDir := IncludeTrailingPathDelimiter(vDir);

  //SetCurrentDir(vDir);

  vFile := ARequest.Query;
  Delete(vFile, 1, 1);
  if vFile <> '' then
    vFile := DocumentRoot + vFile;
  //vFile := ExpandFileName(vFile);
  if FileExists(vFile) then Result := vFile;

  // verificando se a base folder (document root) é o mesmo
  //if (SameText(Copy(vFile, 1, Length(DocumentRoot)), DocumentRoot)) and (FileExists(vFile)) then
  //  Result := vFile;
end;

function TRALWebModule.NewSessionName: StringRAL;
var
  vGuid: TGuid;
begin
  repeat
    CreateGUID(vGuid);
    Result := GUIDToString(vGuid);
  until not FSessions.Exists(Result);
end;

procedure TRALWebModule.SetServer(AValue: TRALServer);
begin
  inherited SetServer(AValue);
  AValue.CreateRoute('/', {$IFDEF FPC}@{$ENDIF}WebModFile, 'index page').SkipAuthMethods := [amALL];
end;

procedure TRALWebModule.SetDocumentRoot(AValue: StringRAL);
begin
  if FDocumentRoot = AValue then exit;
  FDocumentRoot := AValue;
  if (Trim(AValue) = '') or (not DirectoryExists(AValue)) then
    AValue := ExtractFilePath(ParamStr(0));

  AValue := IncludeTrailingPathDelimiter(AValue);

  //SetCurrentDir(AValue);
end;

function TRALWebModule.GetWebSession(ARequest: TRALRequest): TRALWebSession;
var
  vSession: TRALParam;
begin
  Result := nil;
  vSession := ARequest.Params.GetKind[RAL_SESSION, rpkCOOKIE];
  if vSession <> nil then
    Result := TRALWebSession(FSessions.ObjectByItem(vSession.AsString));
end;

procedure TRALWebModule.CreateSession(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vSession: TRALParam;
  vWebSession: TRALWebSession;
  vWesSessionName: StringRAL;
begin
  vSession := ARequest.Params.GetKind[RAL_SESSION, rpkCOOKIE];
  if vSession <> nil then
  begin
    vWesSessionName := vSession.AsString;
    vWebSession := TRALWebSession(FSessions.ObjectByItem(vWesSessionName));
    if vWebSession <> nil then
    begin
      vWebSession.LastDate := Now;
    end
    else
    begin
      vWesSessionName := NewSessionName;
      vWebSession := TRALWebSession.Create;
      FSessions.AddObject(vWesSessionName, vWebSession);
    end;
  end
  else
  begin
    vWesSessionName := NewSessionName;
    vWebSession := TRALWebSession.Create;
    FSessions.AddObject(vWesSessionName, vWebSession);
  end;

  AResponse.AddCookie(RAL_SESSION, vWesSessionName);
end;

procedure TRALWebModule.WebModFile(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vFile: StringRAL;
begin
  vFile := GetFileRoute(ARequest);
  if vFile <> '' then
    AResponse.Answer(vFile)
  else if IndexFile <> '' then
    AResponse.Answer(IndexFile)
  else
    AResponse.Answer(404);
end;

end.
