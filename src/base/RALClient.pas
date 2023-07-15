unit RALClient;

interface

uses
  Classes, SysUtils, StrUtils,
  RALTypes, RALConsts, RALAuthentication, RALRoutes;

type
  TRALClient = class(TRALComponent)
  private
    FAuthentication: TRALAuthentication;
    FBaseURL : StringRAL;
    FUseSSL: boolean;

    FResponseCode : IntegerRAL;
    FResponseStream : TStream;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAuthentication(const AValue: TRALAuthentication);

    procedure SetBaseURL(const AValue: StringRAL);
    procedure SetUseSSL(const AValue: boolean); virtual;
    procedure SetResponse(AStream : TStream);
    function SendUrl(AURL : StringRAL; AMethod : TRALMethod;
                     AHeaders: TStringList = nil;
                     ABody: TRALParams = nil) : IntegerRAL; virtual;
    function GetURL(ARoute : StringRAL) : StringRAL;
    function GetResponseText: StringRAL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Delete(ARoute : StringRAL; AHeaders: TStringList = nil): IntegerRAL;
    function Get(ARoute : StringRAL; AHeaders: TStringList = nil): IntegerRAL;
    function Post(ARoute : StringRAL; AHeaders: TStringList = nil; ABody: TRALParams = nil): IntegerRAL;
    function Put(ARoute : StringRAL; AHeaders: TStringList = nil; ABody: TRALParams = nil): IntegerRAL;
    function Patch(ARoute : StringRAL; AHeaders: TStringList = nil; ABody: TRALParams = nil): IntegerRAL;
    property ResponseCode : IntegerRAL read FResponseCode write FResponseCode;
    property ResponseText : StringRAL read GetResponseText;
  published
    property Authentication: TRALAuthentication read FAuthentication write SetAuthentication;
    property BaseURL: StringRAL read FBaseURL write SetBaseURL;
    property UseSSL: boolean read FUseSSL write SetUseSSL;
  end;

implementation

{ TRALClient }

constructor TRALClient.Create(AOwner: TComponent);
begin
  inherited;
  FAuthentication := nil;
  FBaseURL := '';
  FUseSSL := False;
  FResponseCode := 0;
  FResponseStream := nil;
end;

function TRALClient.Delete(ARoute: StringRAL; AHeaders: TStringList): IntegerRAL;
begin
  Result := SendUrl(GetURL(ARoute),amDELETE,AHeaders,nil);
end;

destructor TRALClient.Destroy;
begin
  if Assigned(FAuthentication) then
    FreeAndNil(FAuthentication);

  inherited;
end;

function TRALClient.Get(ARoute: StringRAL; AHeaders: TStringList): IntegerRAL;
begin
  Result := SendUrl(GetURL(ARoute),amGET,AHeaders,nil);
end;

function TRALClient.GetResponseText: StringRAL;
begin
  Result := '';
  if (FResponseStream <> nil) and (FResponseStream.Size > 0) then
  begin
    if FResponseStream.ClassType = TMemoryStream then begin
      SetLength(Result,FResponseStream.Size);
      FResponseStream.Read(Result[PosIniStr],FResponseStream.Size);
    end
    else if FResponseStream.ClassType = TStringStream then begin
      Result := TStringStream(FResponseStream).DataString;
    end;
  end;
end;

function TRALClient.GetURL(ARoute: StringRAL): StringRAL;
begin
  ARoute := FBaseURL+'/'+ARoute+'/';
  while Pos('//',ARoute) > 0 do
    ReplaceText(ARoute,'//','/');

  Result := 'http';
  if FUseSSL then
    Result := Result + 's';
  Result := Result + '://'+ARoute;
end;

procedure TRALClient.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation  = opRemove) and
     (AComponent = FAuthentication) then
    FAuthentication := nil;
  inherited;
end;

function TRALClient.Patch(ARoute: StringRAL; AHeaders: TStringList;
  ABody: TRALParams): IntegerRAL;
begin
  Result := SendUrl(GetURL(ARoute),amPATCH,AHeaders,ABody);
end;

function TRALClient.Post(ARoute: StringRAL; AHeaders: TStringList;
  ABody: TRALParams): IntegerRAL;
begin
  Result := SendUrl(GetURL(ARoute),amPOST,AHeaders,ABody);
end;

function TRALClient.Put(ARoute: StringRAL; AHeaders: TStringList;
  ABody: TRALParams): IntegerRAL;
begin
  Result := SendUrl(GetURL(ARoute),amPUT,AHeaders,ABody);
end;

function TRALClient.SendUrl(AURL: StringRAL; AMethod: TRALMethod;
  AHeaders: TStringList; ABody: TRALParams) : IntegerRAL;
begin
  if FAuthentication <> nil then
    FAuthentication.GetHeader(AHeaders);
  Result := 0;
end;

procedure TRALClient.SetAuthentication(const AValue: TRALAuthentication);
begin
  if AValue <> FAuthentication then
    FAuthentication := AValue;
  if FAuthentication <> nil then
    FAuthentication.FreeNotification(Self);
end;

procedure TRALClient.SetBaseURL(const AAValue: StringRAL);
var
  vInt : IntegerRAL;
  vProtocol : StringRAL;
begin
  vInt := Pos(':\\',AValue);
  if vInt > 0 then
  begin
    FBaseURL := Copy(AValue, vInt+3, Length(AValue));
    vProtocol := Copy(AValue, 1, vInt - 1);
    FUseSSL := False;
    if SameText(vProtocol,'https') then
      FUseSSL := True;
  end
  else
  begin
    FBaseURL := AValue;
  end;
end;

procedure TRALClient.SetResponse(AStream: TStream);
begin
  if FResponseStream <> nil then
    FreeAndNil(FResponseStream);
  FResponseStream := AStream;
end;

procedure TRALClient.SetUseSSL(const AValue: boolean);
begin
  FUseSSL := AValue;
end;

end.
