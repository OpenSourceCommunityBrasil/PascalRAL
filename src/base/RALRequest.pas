unit RALRequest;

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALParams, RALBase64, RALCustomObjects;

type

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

  TRALRequest = class(TRALHTTPHeaderInfo)
  private
    FAuthorization: TRALAuthorization;
    FContentType: StringRAL;
    FContentSize: Int64RAL;
    FClientInfo: TRALClientInfo;
    FMethod: TRALMethod;
    FQuery: StringRAL;
  public
    constructor Create;
    destructor Destroy; override;

    function AddHeader(AName, AValue : StringRAL) : TRALRequest; reintroduce;
    function AddField(AName, AValue : StringRAL) : TRALRequest; reintroduce;
    function AddCookie(AName, AValue : StringRAL) : TRALRequest; reintroduce;
    function AddFile(AFileName : StringRAL) : TRALRequest; reintroduce;
    function AddFile(AStream : TStream; AFileName : StringRAL = '') : TRALRequest; reintroduce;
  published
    property ClientInfo: TRALClientInfo read FClientInfo write FClientInfo;
    property ContentType: StringRAL read FContentType write FContentType;
    property ContentSize: Int64RAL read FContentSize write FContentSize;
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
end;

destructor TRALRequest.Destroy;
begin
  FreeAndNil(FClientInfo);
  FreeAndNil(FAuthorization);
  inherited;
end;

function TRALRequest.AddHeader(AName, AValue : StringRAL) : TRALRequest;
begin
  inherited AddHeader(AName, AValue);
  Result := Self;
end;

function TRALRequest.AddField(AName, AValue : StringRAL) : TRALRequest;
begin
  inherited AddField(AName, AValue);
  Result := Self;
end;

function TRALRequest.AddCookie(AName, AValue : StringRAL) : TRALRequest;
begin
  inherited AddCookie(AName, AValue);
  Result := Self;
end;

function TRALRequest.AddFile(AFileName : StringRAL) : TRALRequest;
begin
  inherited AddFile(AFileName);
  Result := Self;
end;

function TRALRequest.AddFile(AStream : TStream; AFileName : StringRAL) : TRALRequest;
begin
  inherited AddFile(AStream,AFileName);
  Result := Self;
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

