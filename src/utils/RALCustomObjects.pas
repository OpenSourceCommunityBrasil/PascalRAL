unit RALCustomObjects;

interface

uses
  Classes, SysUtils,
  RALParams, RALTypes, RALConsts, RALMIMETypes;

type

  { TRALComponent }

  TRALComponent = class(TComponent)
  private
    function getVersion: string;
  published
    property Version: string read getVersion;
  end;

  { TRALHTTPHeaderInfo }

  TRALHTTPHeaderInfo = class
  private
    FContentEncoding : StringRAL;
    FAcceptEncoding : StringRAL;
    FParams: TRALParams;
  protected
    function GetParams: TRALParams;
    function GetContentCompress : TRALCompressType;
    procedure SetContentCompress(const AValue: TRALCompressType);
    function GetAcceptCompress : TRALCompressType;
  public
    constructor Create;
    destructor Destroy; override;

    function AddHeader(AName, AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddQuery(AName, AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddField(AName, AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddCookie(AName, AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddFile(AFileName: StringRAL): TRALHTTPHeaderInfo; overload; virtual;
    function AddFile(AStream: TStream; AFileName: StringRAL = ''): TRALHTTPHeaderInfo; overload; virtual;
    function AddBody(AText: StringRAL; AContextType: StringRAL = rctAPPLICATIONJSON): TRALHTTPHeaderInfo; virtual;
    procedure Clear;
    function GetHeader(AName: StringRAL): StringRAL; virtual;
    function GetQuery(AName: StringRAL): StringRAL; virtual;
    function GetField(AName: StringRAL): StringRAL; virtual;
    function GetCookie(AName: StringRAL): StringRAL; virtual;
    function GetBody(AIdx: IntegerRAL): TRALParam; virtual;

    function ParamByName(AParamName: StringRAL): TRALParam;
    function Body : TRALParam;

    function HasValidContentEncoding: boolean;
    function HasValidAcceptEncoding: boolean;
  published
    property Params: TRALParams read GetParams;
    property ContentEncoding: StringRAL read FContentEncoding write FContentEncoding;
    property ContentCompress: TRALCompressType read GetContentCompress write SetContentCompress;
    property AcceptEncoding: StringRAL read FAcceptEncoding write FAcceptEncoding;
    property AcceptCompress: TRALCompressType read GetAcceptCompress;
  end;

implementation

{ TRALComponent }

function TRALComponent.getVersion: string;
begin
  Result := RALVERSION;
end;

{ TRALHTTPHeaderInfo }

function TRALHTTPHeaderInfo.GetAcceptCompress : TRALCompressType;
var
  vStr : StringRAL;
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

function TRALHTTPHeaderInfo.AddHeader(AName, AValue: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkHEADER);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddQuery(AName, AValue : StringRAL) : TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkQUERY);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddBody(AText : StringRAL; AContextType : StringRAL) : TRALHTTPHeaderInfo;
var
  vParam: TRALParam;
begin
  vParam := FParams.AddValue(AText,rpkBODY);
  vParam.ContentType := AContextType;
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddField(AName, AValue: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkFIELD);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddCookie(AName, AValue: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddParam(AName, AValue, rpkCOOKIE);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddFile(AFileName: StringRAL): TRALHTTPHeaderInfo;
begin
  FParams.AddFile(AFileName);
  Result := Self;
end;

function TRALHTTPHeaderInfo.AddFile(AStream: TStream; AFileName: StringRAL): TRALHTTPHeaderInfo;
var
  vParam: TRALParam;
begin
  vParam := FParams.AddValue(AStream, rpkBODY);
  vParam.FileName := AFileName;
  Result := Self;
end;

function TRALHTTPHeaderInfo.GetHeader(AName: StringRAL): StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := FParams.GetKind[AName, rpkHEADER];
  if vParam <> nil then
    Result := vParam.AsString;
end;

function TRALHTTPHeaderInfo.GetQuery(AName : StringRAL) : StringRAL;
var
  vParam: TRALParam;
begin
  Result := '';
  vParam := FParams.GetKind[AName, rpkQUERY];
  if vParam <> nil then
    Result := vParam.AsString;
end;

function TRALHTTPHeaderInfo.GetField(AName: StringRAL): StringRAL;
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
  vStr : StringRAL;
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

function TRALHTTPHeaderInfo.GetCookie(AName: StringRAL): StringRAL;
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

function TRALHTTPHeaderInfo.ParamByName(AParamName: StringRAL): TRALParam;
begin
  Result := FParams.Get[aParamName];
end;

procedure TRALHTTPHeaderInfo.SetContentCompress(const AValue: TRALCompressType);
begin
  case AValue of
    ctNone    : FContentEncoding := '';
    ctDeflate : FContentEncoding := 'deflate';
    ctZLib    : FContentEncoding := 'zlib';
    ctGZip    : FContentEncoding := 'gzip';
  end;
end;

function TRALHTTPHeaderInfo.Body: TRALParam;
begin
  Result := ParamByName('ral_body');
end;

function TRALHTTPHeaderInfo.HasValidContentEncoding : boolean;
var
  vStr, vEnc : StringRAL;
  vInt: integer;
begin
  Result := (Trim(FContentEncoding) = '');

  if Result then
    Exit;

  vStr := LowerCase(Trim(FContentEncoding));
  while vStr <> '' do begin
    vInt := Pos(',', vStr);
    if vInt <= 0 then
      vInt := Length(vStr) + 1;
    vEnc := Trim(Copy(vStr, 1, vInt - 1));

    if (vEnc = 'gzip') or (vEnc = 'deflate') or
       (vEnc = 'zlib') then
    begin
      Result := True;
      Break;
    end;

    Delete(vStr, 1, vInt);
  end;
end;

function TRALHTTPHeaderInfo.HasValidAcceptEncoding : boolean;
var
  vStr, vEnc : StringRAL;
  vInt: integer;
begin
  Result := (Trim(FAcceptEncoding) = '');

  if Result then
    Exit;

  vStr := LowerCase(Trim(FAcceptEncoding));
  while vStr <> '' do begin
    vInt := Pos(',', vStr);
    if vInt <= 0 then
      vInt := Length(vStr) + 1;
    vEnc := Trim(Copy(vStr, 1, vInt - 1));

    if (vEnc = 'gzip') or (vEnc = 'deflate') or
       (vEnc = 'zlib') then
    begin
      Result := True;
      Break;
    end;

    Delete(vStr, 1, vInt);
  end;
end;

end.
