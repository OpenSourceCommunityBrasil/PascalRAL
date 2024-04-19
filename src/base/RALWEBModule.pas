unit RALWebModule;

interface

uses
  Classes, SysUtils,
  RALServer, RALTypes, RALRoutes, RALRequest, RALResponse, RALParams,
  RALThreadSafe;

type

  { TRALWebSession }

  TRALWebSession = class
  private
    FLastDate : TDateTime;
    FObjects : TStringList;
  protected
    procedure ClearObjects;
    function GetObjects(AName : StringRAL): TObject;
    procedure SetObjects(AName : StringRAL; AValue: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    function DeleteObject(AName : StringRAL; AFree : boolean = true) : boolean;

    property Objects[AName : StringRAL] : TObject read GetObjects write SetObjects;
  published
    property LastDate : TDateTime read FLastDate write FLastDate;
  end;

  { TRALWebModule }

  TRALWebModule = class(TRALModuleRoutes)
  private
    FDefautRoute : TRALRoute;
    FDocumentRoot : StringRAL;
    FSessions : TRALStringListSafe;
  protected
    procedure WebModFile(ARequest : TRALRequest; AResponse : TRALResponse);
    function GetFileRoute(ARequest: TRALRequest) : StringRAL;
    function GetWebSession(ARequest : TRALRequest): TRALWebSession;
    procedure CreateSession(ARequest : TRALRequest; AResponse : TRALResponse);
    function NewSessionName : StringRAL;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function CanAnswerRoute(ARequest : TRALRequest; AResponse : TRALResponse) : TRALRoute; override;
    function IsDomain : boolean; override;

    property Session[ARequest : TRALRequest] : TRALWebSession read GetWebSession;
  published
    property DocumentRoot : StringRAL read FDocumentRoot write FDocumentRoot;
    property Routes;
  end;

implementation

const
  RAL_SESSION : StringRAL = 'ral_websession';

{ TRALWebSession }

function TRALWebSession.GetObjects(AName : StringRAL): TObject;
var
  vInt : IntegerRAL;
begin
  Result := nil;
  vInt := FObjects.IndexOf(AName);
  if vInt >= 0 then
    Result := FObjects.Objects[vInt];
end;

procedure TRALWebSession.SetObjects(AName : StringRAL; AValue: TObject);
var
  vInt : IntegerRAL;
begin
  vInt := FObjects.IndexOf(AName);
  if vInt >= 0 then
  begin
    if AValue <> FObjects.Objects[vInt] then
      FObjects.Objects[vInt].Free;
    FObjects.Objects[vInt] := AValue;
  end
  else begin
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
  vInt : IntegerRAL;
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
  Result := Routes.CanAnswerRoute(ARequest, True);

  if (Result = nil) and (GetFileRoute(ARequest) <> '') then
    Result := FDefautRoute
  else if (Result <> nil) and (not Assigned(Result.OnReply)) then
    Result.OnReply := {$IFDEF FPC}@{$ENDIF}WebModFile;

  if Result <> nil then begin
    ARequest.ContentDispositionInline := True;
    AResponse.ContentDispositionInline := True;
    CreateSession(ARequest, AResponse);
  end;
end;

constructor TRALWebModule.Create(AOwner: TComponent);
begin
  inherited;
  FDefautRoute := TRALRoute.Create(nil);
  FDefautRoute.SkipAuthMethods := [amALL];
  FDefautRoute.OnReply := {$IFDEF FPC}@{$ENDIF}WebModFile;

  FSessions := TRALStringListSafe.Create;
end;

destructor TRALWebModule.Destroy;
begin
  FSessions.Clear(True);
  FreeAndNil(FSessions);
  FreeAndNil(FDefautRoute);
  inherited;
end;

function TRALWebModule.GetFileRoute(ARequest: TRALRequest): StringRAL;
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

function TRALWebModule.NewSessionName: StringRAL;
var
  vGuid : TGuid;
begin
  repeat
    CreateGUID(vGuid);
    Result := GUIDToString(vGuid);
  until not FSessions.Exists(Result);
end;

function TRALWebModule.IsDomain: boolean;
begin
  Result := False;
end;

function TRALWebModule.GetWebSession(ARequest : TRALRequest): TRALWebSession;
var
  vSession : TRALParam;
begin
  Result := nil;
  vSession := ARequest.Params.GetKind[RAL_SESSION, rpkCOOKIE];
  if vSession <> nil then
    Result := TRALWebSession(FSessions.ObjectByItem(vSession.AsString));
end;

procedure TRALWebModule.CreateSession(ARequest: TRALRequest;
  AResponse: TRALResponse);
var
  vSession : TRALParam;
  vWebSession : TRALWebSession;
  vWesSessionName : StringRAL;
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
    else begin
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
