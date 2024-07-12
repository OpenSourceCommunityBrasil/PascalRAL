/// Unit that contains everything related to the "Request" part of the communication.
unit RALRequest;

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALParams, RALBase64, RALCustomObjects, RALTools,
  RALMIMETypes, RALStream, RALCompress, RALToken;

type

  { TRALClientInfo }

  /// Class that stores information from the client. Some of them can only be obtained with RALClient
  TRALClientInfo = class
  private
    FIP: StringRAL;
    FMACAddress: StringRAL;
    FPort: IntegerRAL;
    FUserAgent: StringRAL;
  public
    property IP: StringRAL read FIP write FIP;
    property MACAddress: StringRAL read FMACAddress write FMACAddress;
    property Port: IntegerRAL read FPort write FPort;
    property UserAgent: StringRAL read FUserAgent write FUserAgent;
  end;

  { TRALAuthorization }

  /// Class to define which kind of authentication is used on the data
  TRALAuthorization = class
  private
    FAuthString: StringRAL;
    FAuthType: TRALAuthTypes;
    FObjAuth : TObject;
  protected
    procedure SetAuthString(const AValue: StringRAL);
    function GetAuthBasic: TRALAuthBasic;
    function GetAuthBearer: TRALJWT;
    procedure CreateObjAuth;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AuthString: StringRAL read FAuthString write SetAuthString;
    property AuthType: TRALAuthTypes read FAuthType write FAuthType;

    property AsAuthBasic : TRALAuthBasic read GetAuthBasic;
    property AsAuthBearer : TRALJWT read GetAuthBearer;
  end;

  { TRALRequest }

  /// Class that stores everything regarding REQUEST data
  TRALRequest = class(TRALHTTPHeaderInfo)
  private
    FAuthorization: TRALAuthorization;
    FContentSize: Int64RAL;
    FClientInfo: TRALClientInfo;
    FHost: StringRAL;
    FHttpVersion: StringRAL;
    FMethod: TRALMethod;
    FProtocol: StringRAL;
    FQuery: StringRAL;
  protected
    /// Grabs the full URL of the request
    function GetURL: StringRAL;
    /// Grabs only the params after the "?" key and records it in FQuery attribute
    procedure SetQuery(const AValue: StringRAL);

    function GetRequestStream: TStream;
    function GetRequestText: StringRAL;
    procedure SetRequestStream(const AValue: TStream); virtual; abstract;
    procedure SetRequestText(const AValue: StringRAL); virtual; abstract;
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
    procedure Clone(ASource: TRALRequest); reintroduce;
    /// Returns the request data in TStream format
    function GetRequestEncStream(const AEncode: boolean = true): TStream; virtual; abstract;
    /// Returns the request data in UTF8String format
    function GetRequestEncText(const AEncode: boolean = true): StringRAL; virtual; abstract;

    property URL: StringRAL read GetURL;
    property RequestStream: TStream read GetRequestStream write SetRequestStream;
    property RequestText: StringRAL read GetRequestText write SetRequestText;
  published
    property Authorization: TRALAuthorization read FAuthorization write FAuthorization;
    property ClientInfo: TRALClientInfo read FClientInfo write FClientInfo;
    property ContentSize: Int64RAL read FContentSize write FContentSize;
    property Host: StringRAL read FHost write FHost;
    property HttpVersion: StringRAL read FHttpVersion write FHttpVersion;
    property Method: TRALMethod read FMethod write FMethod;
    property Protocol: StringRAL read FProtocol write FProtocol;
    property Query: StringRAL read FQuery write SetQuery;
  end;

  /// Derived class to handle ServerRequest
  TRALServerRequest = class(TRALRequest)
  private
    FStream: TStream;
  public
    constructor Create;
    destructor Destroy; override;
    function GetRequestEncStream(const AEncode: boolean = true): TStream; override;
    function GetRequestEncText(const AEncode: boolean = true): StringRAL; override;
  protected
    procedure SetRequestStream(const AValue: TStream); override;
    procedure SetRequestText(const AValue: StringRAL); override;
  end;

  /// Derived class to handle ClientRequest
  TRALClientRequest = class(TRALRequest)
  public
    function GetRequestEncStream(const AEncode: boolean = true): TStream; override;
    function GetRequestEncText(const AEncode: boolean = true): StringRAL; override;
  protected
    procedure SetRequestStream(const AValue: TStream); override;
    procedure SetRequestText(const AValue: StringRAL); override;
  end;

implementation

{ TRALRequest }

function TRALRequest.GetRequestStream: TStream;
begin
  Result := GetRequestEncStream;
end;

function TRALRequest.GetRequestText: StringRAL;
begin
  Result := GetRequestEncText;
end;

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

  FQuery := FixRoute(FQuery);
end;

procedure TRALRequest.Clone(ASource: TRALRequest);
begin
  inherited Clone(ASource);

  ASource.Authorization := Self.Authorization;
  ASource.ClientInfo := Self.ClientInfo;
  ASource.ContentSize := Self.ContentSize;
  ASource.Host := Self.Host;
  ASource.HttpVersion := Self.HttpVersion;
  ASource.Method := Self.Method;
  ASource.Protocol := Self.Protocol;
  ASource.Query := Self.Query;
end;

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

constructor TRALAuthorization.Create;
begin
  FAuthType := ratNone;
  FAuthString := '';
  FObjAuth := nil;
end;

procedure TRALAuthorization.CreateObjAuth;
begin
  if FObjAuth <> nil then
    FreeAndNil(FObjAuth);

  if FAuthType = ratBasic then begin
    FObjAuth := TRALAuthBasic.Create;
    TRALAuthBasic(FObjAuth).AuthString := FAuthString;
  end
  else if FAuthType = ratBearer then begin
    FObjAuth := TRALJWT.Create;
    TRALJWT(FObjAuth).Token := FAuthString;
  end;
end;

destructor TRALAuthorization.Destroy;
begin
  if FObjAuth <> nil then
    FreeAndNil(FObjAuth);

  inherited;
end;

function TRALAuthorization.GetAuthBasic: TRALAuthBasic;
begin
  Result := nil;
  if FAuthType = ratBasic then
    Result := TRALAuthBasic(FObjAuth);
end;

function TRALAuthorization.GetAuthBearer: TRALJWT;
begin
  Result := nil;
  if FAuthType = ratBearer then
    Result := TRALJWT(FObjAuth);
end;

procedure TRALAuthorization.SetAuthString(const AValue: StringRAL);
begin
  FAuthString := AValue;
  CreateObjAuth;
end;

{ TRALServerRequest }

constructor TRALServerRequest.Create;
begin
  inherited;
  FStream := nil;
end;

destructor TRALServerRequest.Destroy;
begin
  if FStream <> nil then
    FreeAndNil(FStream);
  inherited;
end;

function TRALServerRequest.GetRequestEncStream(const AEncode: boolean): TStream;
var
  vContentType, vContentDisposition: StringRAL;
  vCompress: TRALCompressType;
  vCripto: TRALCriptoType;
begin
  // caso acontece com a Sagui, pois a mesma ja vem params separados
  if FStream = nil then
  begin
    vCompress := Params.CompressType;
    vCripto := Params.CriptoOptions.CriptType;

    Params.CompressType := ctNone;
    Params.CriptoOptions.CriptType := crNone;

    FStream := Params.EncodeBody(vContentType, vContentDisposition);

    Params.CompressType := vCompress;
    Params.CriptoOptions.CriptType := vCripto;
  end;
  Result := FStream;
end;

function TRALServerRequest.GetRequestEncText(const AEncode: boolean): StringRAL;
begin
  Result := StreamToString(FStream);
end;

procedure TRALServerRequest.SetRequestStream(const AValue: TStream);
begin
  if FStream <> nil then
    FreeAndNil(FStream);

  FStream := Params.DecodeBody(AValue, ContentType, ContentDisposition)
end;

procedure TRALServerRequest.SetRequestText(const AValue: StringRAL);
begin
  if FStream <> nil then
    FreeAndNil(FStream);

  FStream := Params.DecodeBody(AValue, ContentType, ContentDisposition)
end;

{ TRALClientRequest }

function TRALClientRequest.GetRequestEncStream(const AEncode: boolean): TStream;
var
  vContentType, vContentDisposition: StringRAL;
begin
  if not AEncode then
  begin
    Params.CriptoOptions.CriptType := crNone;
    Params.CriptoOptions.Key := '';
    Params.CompressType := ctNone;
  end
  else
  begin
    Params.CriptoOptions.CriptType := ContentCripto;
    Params.CriptoOptions.Key := CriptoKey;
    Params.CompressType := ContentCompress;
  end;
  Result := Params.EncodeBody(vContentType, vContentDisposition);
  ContentType := vContentType;
  ContentDisposition := vContentDisposition;
end;

function TRALClientRequest.GetRequestEncText(const AEncode: boolean): StringRAL;
var
  vStream: TStream;
begin
  vStream := GetRequestEncStream(AEncode);
  try
    Result := StreamToString(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure TRALClientRequest.SetRequestStream(const AValue: TStream);
var
  vParam: TRALParam;
begin
  Params.ClearParams(rpkBODY);
  if AValue.Size > 0 then
  begin
    vParam := Params.AddValue(AValue, rpkBODY);
    vParam.ContentType := ContentType;
  end;
end;

procedure TRALClientRequest.SetRequestText(const AValue: StringRAL);
var
  vParam: TRALParam;
begin
  Params.ClearParams(rpkBODY);
  if AValue <> '' then
  begin
    vParam := Params.AddValue(AValue);
    vParam.ContentType := ContentType;
    vParam.Kind := rpkBODY;
  end;
end;

end.
