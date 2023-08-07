unit RALRequest;

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALParams, RALBase64;

type
  TRALMethod = (amGET, amPOST, amPUT, amPATCH, amDELETE, amOPTION, amHEAD, amTRACE, amALL);
  TRALMethods = set of TRALMethod;

  { TRALClientInfo }

  TRALClientInfo = class
  private
    FMACAddress: StringRAL;
    FIP: StringRAL;
    FUserAgent: StringRAL;
  public
    property IP: StringRAL read FIP write FIP;
    property MACAddress: StringRAL read FMACAddress write FMACAddress;
    property UserAgent: StringRAL read FUserAgent write FUserAgent;
  end;

  { TRALAuthorization }

  TRALAuthorization = class
  private
    FAuthType: TRALAuthTypes;
    FAuthString: StringRAL;
  protected
    function GetPassword: StringRAL;
    function GetUserName: StringRAL;
  public
    constructor Create;
  published
    property AuthType: TRALAuthTypes read FAuthType write FAuthType;
    property AuthString: StringRAL read FAuthString write FAuthString;
    property UserName: StringRAL read GetUserName;
    property Password: StringRAL read GetPassword;
  end;

  { TRALRequest }

  TRALRequest = class
  private
    FAuthorization : TRALAuthorization;
    FContentType: StringRAL;
    FContentSize: Int64RAL;
    FClientInfo: TRALClientInfo;
    FMethod: TRALMethod;
    FParams: TRALParams;
    FQuery: StringRAL;
  public
    constructor Create;
    destructor Destroy; override;

    property ClientInfo: TRALClientInfo read FClientInfo write FClientInfo;
    property ContentType: StringRAL read FContentType write FContentType;
    property ContentSize: Int64RAL read FContentSize write FContentSize;
    property Params: TRALParams read FParams;
    property Method: TRALMethod read FMethod write FMethod;
    property Query: StringRAL read FQuery write FQuery;
    property Authorization : TRALAuthorization read FAuthorization write FAuthorization;
  end;

implementation

{ TRALRequest }

constructor TRALRequest.Create;
begin
  inherited;
  FAuthorization := TRALAuthorization.Create;
  FClientInfo := TRALClientInfo.Create;
  FContentSize := 0;
  FParams := TRALParams.Create;
end;

destructor TRALRequest.Destroy;
begin
  FreeAndNil(FClientInfo);
  FreeAndNil(FParams);
  FreeAndNil(FAuthorization);
  inherited;
end;

{ TRALAuthorization }

function TRALAuthorization.GetPassword: StringRAL;
var
  vString: StringRAL;
  vInt: IntegerRAL;
begin
  Result := '';
  if FAuthType = ratBasic then
  begin
    vString := TRALBase64.Decode(FAuthString);
    vInt := Pos(':', vString);
    if vInt > 0 then
      Result := Copy(vString, vInt + 1, Length(vString));
  end;
end;

function TRALAuthorization.GetUserName: StringRAL;
var
  vString: StringRAL;
  vInt: IntegerRAL;
begin
  Result := '';
  if FAuthType = ratBasic then
  begin
    vString := TRALBase64.Decode(FAuthString);
    vInt := Pos(':', vString);
    if vInt > 0 then
      Result := Copy(vString, 1, vInt - 1);
  end;
end;

constructor TRALAuthorization.Create;
begin
  FAuthType := ratNone;
  FAuthString := '';
end;

end.

