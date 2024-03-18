/// Unit that contains everything related to the HTTP response of the traffic
unit RALResponse;

interface

uses
  Classes, SysUtils,
  RALTypes, RALParams, RALMIMETypes, RALCustomObjects, RALStream, RALConsts,
  RALCompress;

type

  { TRALResponse }
  /// Base class for everything related to data response
  TRALResponse = class(TRALHTTPHeaderInfo)
  private
    FStatusCode: IntegerRAL;
  protected
    /// Returns the response in TStream format
    function GetResponseStream: TStream;
    /// Returns the response in UTF8String format
    function GetResponseText: StringRAL;
    /// Assign a Stream into the Response
    procedure SetResponseStream(const AValue: TStream); virtual; abstract;
    /// Assign an UTF8String into the Response
    procedure SetResponseText(const AValue: StringRAL); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    /// Append an UTF8 String to the response
    function AddBody(const AText: StringRAL; const AContextType: StringRAL = rctTEXTPLAIN): TRALResponse; reintroduce;
    /// Append a name:value cookie to the response
    function AddCookie(const AName: StringRAL; const AValue: StringRAL): TRALResponse; reintroduce;
    /// Append a custom param of type "Field" to the response
    function AddField(const AName: StringRAL; const AValue: StringRAL): TRALResponse; reintroduce;
    /// Loads and append a file to the response from given AFileName
    function AddFile(const AFileName: StringRAL): TRALResponse; reintroduce; overload;
    /// Append a file to the response from given AStream
    function AddFile(AStream: TStream; const AFileName: StringRAL = ''): TRALResponse; reintroduce; overload;
    /// Append a name:value param to the header of the response
    function AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALResponse; reintroduce;
    /// Sets the response with the given status code, UTF8 String and Content-Type
    procedure Answer(AStatusCode: IntegerRAL; const AMessage: StringRAL;
                     const AContentType: StringRAL = rctAPPLICATIONJSON); overload;
    /// Sets the response with the given status code
    procedure Answer(AStatusCode: IntegerRAL); overload;
    /// Loads and set a file to the response with the given AFileName
    procedure Answer(const AFileName: StringRAL); overload;
    /// Sets a file to the response with the given status code, FileStream and an AFileName
    procedure Answer(AStatusCode: IntegerRAL; AFile: TStream; const AFileName: StringRAL); overload;

    /// Returns the response in TStream format
    function GetResponseEncStream(const AEncode : boolean = true): TStream; virtual; abstract;
    /// Returns the response in UTF8String format
    function GetResponseEncText(const AEncode : boolean = true): StringRAL; virtual; abstract;

    property ResponseText: StringRAL read GetResponseText write SetResponseText;
    property ResponseStream: TStream read GetResponseStream write SetResponseStream;
  published
    property StatusCode: IntegerRAL read FStatusCode write FStatusCode;
  end;

  /// Derived class to handle ServerResponse
  TRALServerResponse = class(TRALResponse)
  public
    function GetResponseEncStream(const AEncode : boolean = true): TStream; override;
    function GetResponseEncText(const AEncode : boolean = true): StringRAL; override;
  protected
    procedure SetResponseStream(const AValue: TStream); override;
    procedure SetResponseText(const AValue: StringRAL); override;
  end;

  /// Derived class to handle ClientResponse
  TRALClientResponse = class(TRALResponse)
  private
    FStream: TStream;
  public
    constructor Create;
    destructor Destroy; override;

    function GetResponseEncStream(const AEncode : boolean = true): TStream; override;
    function GetResponseEncText(const AEncode : boolean = true): StringRAL; override;
  protected
    procedure SetResponseStream(const AValue: TStream); override;
    procedure SetResponseText(const AValue: StringRAL); override;
  end;

implementation

{ TRALResponse }

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; const AMessage: StringRAL;
  const AContentType: StringRAL);
begin
  StatusCode := AStatusCode;
  ContentType := AContentType;
  ResponseText := AMessage;
end;

procedure TRALResponse.Answer(const AFileName: StringRAL);
begin
  AddFile(AFileName);
end;

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; AFile: TStream;
  const AFileName: StringRAL);
begin
  StatusCode := AStatusCode;
  AddFile(AFile, AFileName);
end;

procedure TRALResponse.Answer(AStatusCode: IntegerRAL);
begin
  StatusCode := AStatusCode;
  ContentType := rctTEXTHTML;
  case AStatusCode of
    400 : ResponseText := RAL400Page;
    401 : ResponseText := RAL401Page;
    403 : ResponseText := RAL403Page;
    404 : ResponseText := RAL404Page;
    415 : ResponseText := RAL415Page;
    500 : ResponseText := RAL500Page;
    501 : ResponseText := RAL501Page;
    503 : ResponseText := RAL503Page;
  end;
end;

function TRALResponse.AddHeader(const AName, AValue: StringRAL): TRALResponse;
begin
  inherited AddHeader(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddField(const AName, AValue: StringRAL): TRALResponse;
begin
  inherited AddField(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddBody(const AText, AContextType: StringRAL): TRALResponse;
begin
  inherited AddBody(AText, AContextType);
  Result := Self;
end;

function TRALResponse.AddCookie(const AName, AValue: StringRAL): TRALResponse;
begin
  inherited AddCookie(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddFile(const AFileName: StringRAL): TRALResponse;
begin
  inherited AddFile(AFileName);
  Result := Self;
end;

function TRALResponse.AddFile(AStream: TStream; const AFileName: StringRAL): TRALResponse;
begin
  inherited AddFile(AStream, AFileName);
  Result := Self;
end;

constructor TRALResponse.Create;
begin
  inherited Create;
  ContentType := rctAPPLICATIONJSON;
end;

destructor TRALResponse.Destroy;
begin
  inherited;
end;

function TRALResponse.GetResponseStream: TStream;
begin
  Result := GetResponseEncStream;
end;

function TRALResponse.GetResponseText: StringRAL;
begin
  Result := GetResponseEncText;
end;

{ TRALServerResponse }

function TRALServerResponse.GetResponseEncStream(const AEncode: boolean): TStream;
var
  vContentType, vContentDisposition : StringRAL;
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

function TRALServerResponse.GetResponseEncText(
  const AEncode: boolean): StringRAL;
var
  vStream: TStream;
begin
  vStream := GetResponseEncStream(AEncode);
  try
    Result := StreamToString(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure TRALServerResponse.SetResponseStream(const AValue: TStream);
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

procedure TRALServerResponse.SetResponseText(const AValue: StringRAL);
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

{ TRALClientResponse }

constructor TRALClientResponse.Create;
begin
  inherited;
  FStream := nil;
end;

destructor TRALClientResponse.Destroy;
begin
  if FStream <> nil then
    FreeAndNil(FStream);
  inherited;
end;

function TRALClientResponse.GetResponseEncStream(
  const AEncode: boolean): TStream;
begin
  Result := FStream;
  Result.Position := 0;
end;

function TRALClientResponse.GetResponseEncText(
  const AEncode: boolean): StringRAL;
begin
  Result := StreamToString(FStream);
end;

procedure TRALClientResponse.SetResponseStream(const AValue: TStream);
begin
  if FStream <> nil then
    FreeAndNil(FStream);

  FStream := Params.DecodeBody(AValue, ContentType, ContentDisposition);
end;

procedure TRALClientResponse.SetResponseText(const AValue: StringRAL);
begin
  if FStream <> nil then
    FreeAndNil(FStream);

  FStream := Params.DecodeBody(AValue, ContentType, ContentDisposition)
end;

end.
