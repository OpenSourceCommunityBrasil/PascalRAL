/// Unit with the definitions of base classes of PascalRAL
unit RALCustomObjects;

interface

uses
  Classes, SysUtils,
  RALParams, RALTypes, RALConsts, RALMIMETypes, RALTools;

type

  { TRALComponent }

  /// Base class of visual components
  TRALComponent = class(TComponent)
  private
    function getVersion: string;
  published
    property Version: string read getVersion;
  end;

  { TRALHTTPHeaderInfo }

  /// Base class of REQUEST and RESPONSE classes
  TRALHTTPHeaderInfo = class
  private
    FAcceptEncoding: StringRAL;
    FAcceptEncription: StringRAL;
    FContentEncoding: StringRAL;
    FContentEncription: StringRAL;
    FParams: TRALParams;
  protected
    /// Grabs the kind of compression that will be accepted on the traffic
    function GetAcceptCompress: TRALCompressType;
    /// Grabs the kind of criptography that will be accepted on the traffic
    function GetAcceptCripto: TRALCriptoType;
    /// Grabs the kind of compression that will be used on the traffic
    function GetContentCompress: TRALCompressType;
    /// Grabs the kind of criptography that will be used on the traffic
    function GetContentCripto: TRALCriptoType;
    function GetParams: TRALParams;
    procedure SetContentCompress(const AValue: TRALCompressType);
    procedure SetContentCripto(AValue: TRALCriptoType);
  public
    constructor Create;
    destructor Destroy; override;

    function AddBody(const AText: StringRAL; const AContextType: StringRAL = rctAPPLICATIONJSON): TRALHTTPHeaderInfo; virtual;
    function AddCookie(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddField(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddFile(const AFileName: StringRAL): TRALHTTPHeaderInfo; overload; virtual;
    function AddFile(AStream: TStream; const AFileName: StringRAL = ''): TRALHTTPHeaderInfo; overload; virtual;
    function AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddQuery(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    /// Grabs the body of either the request or the response
    function Body: TRALParam;
    procedure Clear;
    function GetBody(AIdx: IntegerRAL): TRALParam; virtual;
    function GetCookie(const AName: StringRAL): StringRAL; virtual;
    function GetField(const AName: StringRAL): StringRAL; virtual;
    function GetHeader(const AName: StringRAL): StringRAL; virtual;
    function GetQuery(const AName: StringRAL): StringRAL; virtual;
    function HasValidAcceptEncoding: boolean;
    function HasValidContentEncoding: boolean;
    /// Grabs the param either on request or response by its name
    function ParamByName(const AParamName: StringRAL): TRALParam;
  published
    property AcceptCompress: TRALCompressType read GetAcceptCompress;
    property AcceptCripto: TRALCriptoType read GetAcceptCripto;
    property AcceptEncoding: StringRAL read FAcceptEncoding write FAcceptEncoding;
    property AcceptEncription: StringRAL read FAcceptEncription write FAcceptEncription;
    property ContentCompress: TRALCompressType read GetContentCompress write SetContentCompress;
    property ContentCripto: TRALCriptoType read GetContentCripto write SetContentCripto;
    property ContentEncoding: StringRAL read FContentEncoding write FContentEncoding;
    property ContentEncription: StringRAL read FContentEncription write FContentEncription;
    property Params: TRALParams read GetParams;
  end;

implementation

{ TRALComponent }

function TRALComponent.getVersion: string;
begin
  Result := RALVERSION;
end;

{ TRALHTTPHeaderInfo }

function TRALHTTPHeaderInfo.GetAcceptCompress: TRALCompressType;
var
  vStr: StringRAL;
begin
  vStr := LowerCase(FAcceptEncoding);
  if (Pos('gzip', vStr) > 0) then
    Result := ctGZip
  else if (Pos('deflate', vStr) > 0) then
    Result := ctDeflate
  else if (Pos('zlib', vStr) > 0) then
    Result := ctZLib
  else
    Result := ctNone;
end;

function TRALHTTPHeaderInfo.GetContentCripto: TRALCriptoType;
var
  vStr: StringRAL;
begin
  vStr := LowerCase(FContentEncription);
  if (Pos('aes256cbc_pkcs7', vStr) > 0) then
    Result := crAES256
  else if (Pos('aes192cbc_pkcs7', vStr) > 0) then
    Result := crAES192
  else if (Pos('aes128cbc_pkcs7', vStr) > 0) then
    Result := crAES128
  else
    Result := crNone;
end;

procedure TRALHTTPHeaderInfo.SetContentCripto(AValue: TRALCriptoType);
begin
  FContentEncription := CriptoToStrCripto(AValue);
end;

function TRALHTTPHeaderInfo.GetAcceptCripto: TRALCriptoType;
var
  vStr: StringRAL;
begin
  vStr := LowerCase(FAcceptEncription);
  if (Pos('aes256cbc_pkcs7', vStr) > 0) then
    Result := crAES256
  else if (Pos('aes192cbc_pkcs7', vStr) > 0) then
    Result := crAES192
  else if (Pos('aes128cbc_pkcs7', vStr) > 0) then
    Result := crAES128
  else
    Result := crNone;
end;

function TRALHTTPHeaderInfo.GetParams: TRALParams;
begin
  Result := FParams;
end;

procedure TRALHTTPHeaderInfo.Clear;
begin
  FParams.ClearParams;
end;

constructor TRALHTTPHeaderInfo.Create;
begin
  inherited;
  FParams := TRALParams.Create;
end;

destructor TRALHTTPHeaderInfo.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TRALHTTPHeaderInfo.AddHeader(const AName, AValue: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkHEADER);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddQuery(const AName, AValue: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkQUERY);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddBody(const AText: StringRAL; const AContextType: StringRAL)
  : TRALHTTPHeaderInfo;
var
  vParam: TRALParam;
begin
  vParam := FParams.AddValue(AText, rpkBODY);
  vParam.ContentType := AContextType;
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddField(const AName, AValue: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkFIELD);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddCookie(const AName, AValue: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkCOOKIE);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddFile(const AFileName: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddFile(AFileName);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddFile(AStream: TStream; const AFileName: StringRAL)
  : TRALHTTPHeaderInfo;
var
  vParam: TRALParam;
begin
  vParam := FParams.AddValue(AStream, rpkBODY);
  vParam.FileName := AFileName;
  Result := Self;
end;

function TRALHTTPHeaderInfo.GetHeader(const AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := FParams.GetKind[AName, rpkHEADER];
  if vParam <> nil then
    Result := vParam.AsString;
end;

function TRALHTTPHeaderInfo.GetQuery(const AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := FParams.GetKind[AName, rpkQUERY];
  if vParam <> nil then
    Result := vParam.AsString;
end;

function TRALHTTPHeaderInfo.GetField(const AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := FParams.GetKind[AName, rpkFIELD];
  if vParam <> nil then
    Result := vParam.AsString;
end;

function TRALHTTPHeaderInfo.GetContentCompress: TRALCompressType;
var
  vStr: StringRAL;
begin
  vStr := LowerCase(FContentEncoding);
  if (Pos('gzip', vStr) > 0) then
    Result := ctGZip
  else if (Pos('deflate', vStr) > 0) then
    Result := ctDeflate
  else if (Pos('zlib', vStr) > 0) then
    Result := ctZLib
  else
    Result := ctNone;
end;

function TRALHTTPHeaderInfo.GetCookie(const AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := FParams.GetKind[AName, rpkCOOKIE];
  if vParam <> nil then
    Result := vParam.AsString;
end;

function TRALHTTPHeaderInfo.GetBody(AIdx: IntegerRAL): TRALParam;
var
  vParam: TRALParam;
  vInt: IntegerRAL;
begin
  Result := nil;
  for vInt := 0 to FParams.Count do
  begin
    vParam := FParams.Param[vInt];
    if vParam.Kind = rpkBODY then
    begin
      if AIdx > 0 then
      begin
        AIdx := AIdx - 1;
      end
      else
      begin
        Result := vParam;
        Break;
      end;
    end;
  end;
end;

function TRALHTTPHeaderInfo.ParamByName(const AParamName: StringRAL): TRALParam;
begin
  Result := FParams.Get[AParamName];
end;

procedure TRALHTTPHeaderInfo.SetContentCompress(const AValue: TRALCompressType);
begin
  FContentEncoding := CompressToStrCompress(AValue);
end;

function TRALHTTPHeaderInfo.Body: TRALParam;
begin
  Result := ParamByName('ral_body');
end;

function TRALHTTPHeaderInfo.HasValidContentEncoding: boolean;
var
  vStr, vEnc: StringRAL;
  vInt: integer;
begin
  Result := (Trim(FContentEncoding) = '');

  if Result then
    Exit;

  vStr := LowerCase(Trim(FContentEncoding));
  while vStr <> '' do
  begin
    vInt := Pos(',', vStr);
    if vInt <= 0 then
      vInt := Length(vStr) + 1;
    vEnc := Trim(Copy(vStr, 1, vInt - 1));

    if StrCompressToCompress(vEnc) <> ctNone then
    begin
      Result := True;
      Break;
    end;

    Delete(vStr, 1, vInt);
  end;
end;

function TRALHTTPHeaderInfo.HasValidAcceptEncoding: boolean;
var
  vStr, vEnc: StringRAL;
  vInt: integer;
begin
  Result := (Trim(FAcceptEncoding) = '');

  if Result then
    Exit;

  vStr := LowerCase(Trim(FAcceptEncoding));
  while vStr <> '' do
  begin
    vInt := Pos(',', vStr);
    if vInt <= 0 then
      vInt := Length(vStr) + 1;
    vEnc := Trim(Copy(vStr, 1, vInt - 1));

    if StrCompressToCompress(vEnc) <> ctNone then
    begin
      Result := True;
      Break;
    end;

    Delete(vStr, 1, vInt);
  end;
end;

end.
