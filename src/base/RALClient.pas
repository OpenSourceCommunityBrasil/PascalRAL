unit RALClient;

interface

uses
  Classes, SysUtils, StrUtils,
  RALTypes, RALAuthentication, RALRoutes;

type
  TRALClient = class(TComponent)
  private
    FAuthentication: TRALAuthentication;
    FHost: StringRAL;
    FPort: IntegerRAL;
    FUseSSL: boolean;

    FResponseCode : IntegerRAL;
    FResponseStream : TStream;
  protected
    procedure SetUseSSL(const Value: boolean); virtual;
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
    property Authentication: TRALAuthentication read FAuthentication write FAuthentication;
    property Host: StringRAL read FHost write FHost;
    property Port: IntegerRAL read FPort write FPort;
    property UseSSL: boolean read FUseSSL write SetUseSSL;
  end;

implementation

{ TRALClient }

constructor TRALClient.Create(AOwner: TComponent);
begin
  inherited;
  FAuthentication := nil;
  FHost := '';
  FPort := 8000;
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
  ARoute := '/'+ARoute+'/';
  while Pos('//',ARoute) > 0 do
    ReplaceText(ARoute,'//','/');

  Result := 'http';
  if FUseSSL then
    Result := Result + 's';
  Result := Result + '://'+FHost+':'+IntToStr(FPort)+ARoute;
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
  Result := 0;
end;

procedure TRALClient.SetResponse(AStream: TStream);
begin
  if FResponseStream <> nil then
    FreeAndNil(FResponseStream);
  FResponseStream := AStream;
end;

procedure TRALClient.SetUseSSL(const Value: boolean);
begin
  FUseSSL := Value;
end;

end.
