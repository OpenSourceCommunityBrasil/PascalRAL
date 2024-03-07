/// Unit that contains everything related to the "Request" part of the communication.
unit RALRequest;

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALParams, RALBase64, RALCustomObjects, RALTools,
  RALMIMETypes;

type

  { TRALClientInfo }

  /// Class that stores information from the client. Some of them can only be obtained with RALClient
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

  /// Class to define which kind of authentication is used on the data
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

  /// Class that stores everything regarding REQUEST data
  TRALRequest = class(TRALHTTPHeaderInfo)
  private
    FAuthorization: TRALAuthorization;
    FContentType: StringRAL;
    FContentSize: Int64RAL;
    FClientInfo: TRALClientInfo;
    FMethod: TRALMethod;
    FQuery: StringRAL;
    FHost: StringRAL;
    FProtocol: StringRAL;
    FHttpVersion: StringRAL;
    FStream: TStream;
  protected
    /// Grabs the full URL of the request
    function GetURL: StringRAL;
    /// Grabs only the params after the "?" key and records it in FQuery attribute
    procedure SetQuery(const AValue: StringRAL);
    procedure SetStream(const AValue: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    /// Adds an UTF8 String to the body of the request.
    function AddBody(const AText: StringRAL; const AContextType: StringRAL = rctTEXTPLAIN): TRALRequest; reintroduce;
    /// Adds a string cookie to the body of the request.
    function AddCookie(const AName: StringRAL; const AValue: StringRAL): TRALRequest; reintroduce;
    /// Adds a string param with the "Field" kind to the request.
    function AddField(const AName: StringRAL; const AValue: StringRAL): TRALRequest; reintroduce;
    /// Adds a file to the body of the request based on the given AFileName.
    function AddFile(const AFileName: StringRAL): TRALRequest; reintroduce; overload;
    /// Adds a custom file to the body of the request from the given AStream.
    function AddFile(AStream: TStream; const AFileName: StringRAL = ''): TRALRequest; reintroduce; overload;
    /// Adds an UTF8 String to the header of the request.
    function AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALRequest; reintroduce;

    property URL: StringRAL read GetURL;
  published
    property Authorization: TRALAuthorization read FAuthorization write FAuthorization;
    property ClientInfo: TRALClientInfo read FClientInfo write FClientInfo;
    property ContentSize: Int64RAL read FContentSize write FContentSize;
    property ContentType: StringRAL read FContentType write FContentType;
    property Host: StringRAL read FHost write FHost;
    property HttpVersion: StringRAL read FHttpVersion write FHttpVersion;
    property Method: TRALMethod read FMethod write FMethod;
    property Protocol: StringRAL read FProtocol write FProtocol;
    property Stream: TStream read FStream write SetStream;
    property Query: StringRAL read FQuery write SetQuery;
  end;

implementation

{ TRALRequest }

function TRALRequest.GetURL: StringRAL;
begin
  Result := LowerCase(FHttpVersion) + ':/' + FixRoute(FHost + '/' + FQuery);
end;

procedure TRALRequest.SetQuery(const AValue: StringRAL);
var
  vInt: IntegerRAL;
begin
  FQuery := AValue;
  vInt := Pos('?', FQuery);
  if vInt > 0 then
    Delete(FQuery, vInt, Length(FQuery));
end;

procedure TRALRequest.SetStream(const AValue: TStream);
begin
  if FStream <> nil then
    FreeAndNil(FStream);

  FStream := Params.DecodeBody(AValue, FContentType);
end;

constructor TRALRequest.Create;
begin
  inherited;
  FAuthorization := TRALAuthorization.Create;
  FClientInfo := TRALClientInfo.Create;
  FContentSize := 0;
  FStream := nil;
end;

destructor TRALRequest.Destroy;
begin
  FreeAndNil(FClientInfo);
  FreeAndNil(FAuthorization);
  if FStream <> nil then
    FreeAndNil(FStream);
  inherited;
end;

function TRALRequest.AddHeader(const AName, AValue: StringRAL): TRALRequest;
begin
  inherited AddHeader(AName, AValue);
  Result := Self;
end;

function TRALRequest.AddBody(const AText: StringRAL; const AContextType: StringRAL)
  : TRALRequest;
begin
  inherited AddBody(AText, AContextType);
  Result := Self;
end;

function TRALRequest.AddField(const AName, AValue: StringRAL): TRALRequest;
begin
  inherited AddField(AName, AValue);
  Result := Self;
end;

function TRALRequest.AddCookie(const AName, AValue: StringRAL): TRALRequest;
begin
  inherited AddCookie(AName, AValue);
  Result := Self;
end;

function TRALRequest.AddFile(const AFileName: StringRAL): TRALRequest;
begin
  inherited AddFile(AFileName);
  Result := Self;
end;

function TRALRequest.AddFile(AStream: TStream; const AFileName: StringRAL): TRALRequest;
begin
  inherited AddFile(AStream, AFileName);
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
