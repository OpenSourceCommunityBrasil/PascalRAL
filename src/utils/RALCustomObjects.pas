/// Unit with the definitions of base classes of PascalRAL
unit RALCustomObjects;

interface

uses
  Classes, SysUtils,
  RALParams, RALTypes, RALConsts, RALMIMETypes, RALTools, RALCompress;

type

  { TRALComponent }

  /// Base class of visual components
  TRALComponent = class(TComponent)
  private
    function GetVersion: string;
  published
    property Version: string read GetVersion;
  end;

  { TRALHTTPHeaderInfo }

  /// Base class of REQUEST and RESPONSE classes
  TRALHTTPHeaderInfo = class
  private
    FParent: TObject;
    FAcceptEncoding: StringRAL;
    FAcceptEncription: StringRAL;
    FContentDisposition: StringRAL;
    FContentDispositionInline: Boolean;
    FContentEncoding: StringRAL;
    FContentEncription: StringRAL;
    FContentType: StringRAL;
    FCriptoKey: StringRAL;
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
    procedure SetContentType(const AValue: StringRAL);
  public
    constructor Create(AOwner : TObject); virtual;
    destructor Destroy; override;

    function AddBody(const AText: StringRAL; const AContextType: StringRAL = rctAPPLICATIONJSON): TRALHTTPHeaderInfo; virtual;
    function AddCookie(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddCookies(ACookies: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddField(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddFile(const AFileName: StringRAL): TRALHTTPHeaderInfo; overload; virtual;
    function AddFile(AStream: TStream; const AFileName: StringRAL = ''): TRALHTTPHeaderInfo; overload; virtual;
    function AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddQuery(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    /// Grabs the body of either the request or the response
    function Body: TRALParam;
    procedure Clear; virtual;
    procedure Clone(ASource: TRALHTTPHeaderInfo);
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
    property ContentType: StringRAL read FContentType write SetContentType;
    property ContentDisposition: StringRAL read FContentDisposition write FContentDisposition;
    property CriptoKey: StringRAL read FCriptoKey write FCriptoKey;
    property ContentDispositionInline: boolean read FContentDispositionInline write FContentDispositionInline;
    property Params: TRALParams read GetParams;
    property Parent: TObject read FParent;
  end;

implementation

{ TRALComponent }

function TRALComponent.getVersion: string;
begin
  Result := RALVERSION;
end;

{ TRALHTTPHeaderInfo }

function TRALHTTPHeaderInfo.GetAcceptCompress: TRALCompressType;
begin
  Result := TRALCompress.GetBestCompress(FAcceptEncoding);
end;

function TRALHTTPHeaderInfo.GetContentCripto: TRALCriptoType;
var
  vStr: StringRAL;
begin
  vStr := LowerCase(FContentEncription);
  if (Pos(StringRAL('aes256cbc_pkcs7'), vStr) > 0) then
    Result := crAES256
  else if (Pos(StringRAL('aes192cbc_pkcs7'), vStr) > 0) then
    Result := crAES192
  else if (Pos(StringRAL('aes128cbc_pkcs7'), vStr) > 0) then
    Result := crAES128
  else
    Result := crNone;
end;

procedure TRALHTTPHeaderInfo.SetContentCripto(AValue: TRALCriptoType);
begin
  FContentEncription := CriptoToStrCripto(AValue);
end;

procedure TRALHTTPHeaderInfo.SetContentType(const AValue: StringRAL);
begin
  FContentType := AValue;
  if Pos(StringRAL('charset='), FContentType) = 0 then
    FContentType := FContentType + '; charset=utf-8';
end;

function TRALHTTPHeaderInfo.GetAcceptCripto: TRALCriptoType;
var
  vStr: StringRAL;
begin
  vStr := LowerCase(FAcceptEncription);
  if (Pos(StringRAL('aes256cbc_pkcs7'), vStr) > 0) then
    Result := crAES256
  else if (Pos(StringRAL('aes192cbc_pkcs7'), vStr) > 0) then
    Result := crAES192
  else if (Pos(StringRAL('aes128cbc_pkcs7'), vStr) > 0) then
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

procedure TRALHTTPHeaderInfo.Clone(ASource: TRALHTTPHeaderInfo);
var
  vInt: IntegerRAL;
  vParamSource, vParamTarget: TRALParam;
begin
  if ASource = nil then
    Exit;

  ASource.AcceptEncoding := Self.AcceptEncoding;
  ASource.ContentCompress := Self.ContentCompress;
  ASource.AcceptEncription := Self.AcceptEncription;
  ASource.ContentCripto := Self.ContentCripto;
  ASource.ContentEncoding := Self.ContentEncoding;
  ASource.ContentEncription := Self.ContentEncription;
  ASource.ContentType := Self.ContentType;
  ASource.ContentDisposition := Self.ContentDisposition;
  ASource.CriptoKey := Self.CriptoKey;

  for vInt := 0 to Pred(FParams.Count) do begin
    vParamSource := FParams.Index[vInt];
    vParamTarget := ASource.Params.NewParam;
    vParamSource.Clone(vParamTarget);
  end;
end;

constructor TRALHTTPHeaderInfo.Create(AOwner : TObject);
begin
  inherited Create;
  FParent := AOwner;
  FParams := TRALParams.Create;
  FContentDispositionInline := False;
end;

destructor TRALHTTPHeaderInfo.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TRALHTTPHeaderInfo.AddHeader(const AName: StringRAL;
  const AValue: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkHEADER);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddQuery(const AName: StringRAL;
  const AValue: StringRAL): TRALHTTPHeaderInfo;
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

function TRALHTTPHeaderInfo.AddField(const AName: StringRAL;
  const AValue: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkFIELD);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddCookie(const AName: StringRAL;
  const AValue: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkCOOKIE);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddCookies(ACookies: StringRAL): TRALHTTPHeaderInfo;
var
  vInt1: IntegerRAL;
  vStr: StringRAL;
begin
  while Trim(ACookies) <> '' do
  begin
    vInt1 := Pos(';', ACookies);
    if vInt1 = 0 then
      vInt1 := Length(ACookies) + 1;

    vStr := Copy(ACookies, 1, vInt1 - 1);
    Delete(ACookies, 1, vInt1);

    vInt1 := Pos('=', vStr);
    AddCookie(Trim(Copy(vStr, 1, vInt1 - 1)), Trim(Copy(vStr, vInt1 + 1, Length(vStr))));
  end;
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
begin
  Result := TRALCompress.GetBestCompress(FContentEncoding);
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
    vParam := FParams.Index[vInt];
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
  FContentEncoding := TRALCompress.CompressToString(AValue);
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

    if TRALCompress.StringToCompress(vEnc) <> ctNone then
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

    if TRALCompress.StringToCompress(vEnc) <> ctNone then
    begin
      Result := True;
      Break;
    end;

    Delete(vStr, 1, vInt);
  end;
end;

end.
