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
  public
    { Whether a published property still means anything with the component
      configured as it is RIGHT NOW - the mORMot2 server's HttpSysDomain says
      nothing outside smHttpSys, and ShareConnection says nothing on an engine
      whose transport is one socket per object.

      It is what the Object Inspector filter asks (see TRALSelectionEditor in
      RALRegister), and it lives here, on the component, so that the design
      package never has to know one engine from another.

      HIDDEN IS NOT FORBIDDEN, and nothing may be built on top of this: a
      property left over from another configuration has to stay harmless, so
      an irrelevant value is ignored and never raises. What does raise is an
      explicit choice that cannot work - a Mode this platform has no server
      for, an HTTPVersion this engine cannot ask for - and those are never
      hidden. }
    function IsPropertyRelevant(const AName: StringRAL): boolean; virtual;
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
    FProtocolVersion: TRALHTTPVersion;
    FParams: TRALParams;
    { what AcceptCompress and ContentCompress answered last, for which text and
      under which set of registered compressors: a request reads each several
      times, and the header only changes when somebody writes it. The text is
      compared by reference - kept here, it cannot be freed and its address
      taken by another string - so a hit costs two comparisons }
    FAcceptKey: StringRAL;
    FAcceptGen: IntegerRAL;
    FAcceptValue: TRALCompressType;
    FContentKey: StringRAL;
    FContentGen: IntegerRAL;
    FContentValue: TRALCompressType;
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
    function GetProtocol: StringRAL;
    procedure SetContentCompress(const AValue: TRALCompressType);
    procedure SetContentCripto(AValue: TRALCriptoType);
    procedure SetContentDisposition(const AValue: StringRAL);
    procedure SetContentType(const AValue: StringRAL);
    procedure SetProtocol(const AValue: StringRAL);
  public
    constructor Create(AOwner : TObject); virtual;
    destructor Destroy; override;

    /// True for the media types a charset parameter applies to (text/*, JSON, XML, JavaScript, forms)
    class function IsTextualType(const AContentType: StringRAL): boolean;

    function AddBody(const AText: StringRAL; const AContextType: StringRAL = rctAPPLICATIONJSON): TRALHTTPHeaderInfo; virtual;
    function AddCookie(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; overload; virtual; deprecated 'use AddCookie(ACookie:TRALCookie) instead';
    function AddCookie(const ACookie: TRALCookie):TRALHTTPHeaderInfo; overload; virtual;
    function AddCookies(ACookies: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddField(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddFile(const AFileName: StringRAL): TRALHTTPHeaderInfo; overload; virtual;
    function AddFile(AStream: TStream; const AFileName: StringRAL = ''): TRALHTTPHeaderInfo; overload; virtual;
    function AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    function AddQuery(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    /// The body of the request or the response, when it is ONE value - raw text,
    /// JSON, a file (the param named 'ral_body'). A body the engine already split
    /// into params is not here: form fields (x-www-form-urlencoded) and the parts
    /// of a multipart are rpkFIELD/rpkBODY params, read them with ParamByName,
    /// and Params.AssignParamsUrl(rpkFIELD) gives the form back as text. For such
    /// a body this answers nil - Body.AsString is still safe and gives ''
    function Body: TRALParam;
    procedure Clear; virtual;
    procedure Clone(ASource: TRALHTTPHeaderInfo);
    function GetBody(AIdx: IntegerRAL): TRALParam; virtual;
    function GetCookie(const AName: StringRAL): StringRAL; virtual; deprecated 'use GetRALCookie(AName): TRALCookie instead';
    function GetRALCookie(const AName: StringRAL): TRALCookie; virtual;
    function GetField(const AName: StringRAL): StringRAL; virtual;
    function GetHeader(const AName: StringRAL): StringRAL; virtual;
    function GetQuery(const AName: StringRAL): StringRAL; virtual;
    function HasValidAcceptEncoding: boolean;
    function HasValidContentEncoding: boolean;
    /// The param with this name, of ANY kind - query string, header, cookie, form
    /// field - and the first one added when there are several: the query string
    /// and the headers come in before any authentication event runs, so a value
    /// added later with AddParam does not override one the client sent under the
    /// same name (use Params.ReplaceParam for that, or Params.GetKind to ask for
    /// one kind). A JSON body is not split into params unless the server has
    /// JSONBodyToParams on; read it from Body otherwise
    function ParamByName(const AParamName: StringRAL): TRALParam;
    /// fills the body with the String AContent
    procedure SetBody(AContent: StringRAL); overload; virtual;
    /// fills the body with the Stream AContent
    procedure SetBody(AContent: TStream); overload; virtual;
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
    property ContentDisposition: StringRAL read FContentDisposition write SetContentDisposition;
    property CriptoKey: StringRAL read FCriptoKey write FCriptoKey;
    /// Which HTTP version carried this message, as the transport REPORTS it -
    /// never what was asked for, since ALPN settles that during the TLS
    /// handshake. A transport that cannot tell leaves rhvDefault.
    ///
    /// On a RESPONSE it is what the client engine could read back. On a
    /// REQUEST it is what the server saw the client arrive on, which is the
    /// only place the answer is reliable: the http.sys server reads it from
    /// the driver, while a client on Windows has to guess from the status
    /// line - and an HTTP/2 response does not have one, so it under-reports.
    property ProtocolVersion: TRALHTTPVersion read FProtocolVersion write FProtocolVersion;
    /// The SAME fact as ProtocolVersion, spelled the way the wire spells it -
    /// '1.0', '1.1', '2.0', or '' when the transport could not tell.
    ///
    /// There is ONE field behind the two, on purpose: an engine that fills
    /// either one has filled both, and they can never disagree. Writing text
    /// a version cannot be parsed out of leaves rhvDefault, so Protocol reads
    /// back as '' - it is not a free-text field.
    property Protocol: StringRAL read GetProtocol write SetProtocol stored False;
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

function TRALComponent.IsPropertyRelevant(const AName: StringRAL): boolean;
begin
  Result := True; // everything shows unless a descendant says otherwise
end;

{ TRALHTTPHeaderInfo }

function TRALHTTPHeaderInfo.GetAcceptCompress: TRALCompressType;
begin
  if (Pointer(FAcceptEncoding) <> Pointer(FAcceptKey)) or
     (FAcceptGen <> CompressRegistration) then
  begin
    FAcceptValue := TRALCompress.GetBestCompress(FAcceptEncoding);
    FAcceptKey := FAcceptEncoding;
    FAcceptGen := CompressRegistration;
  end;
  Result := FAcceptValue;
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

procedure TRALHTTPHeaderInfo.SetContentDisposition(const AValue: StringRAL);
begin
  { the file name in it is often the client's own: kept to one line }
  FContentDisposition := RALSafeHeaderText(AValue);
end;

procedure TRALHTTPHeaderInfo.SetContentType(const AValue: StringRAL);
begin
  FContentType := RALSafeHeaderText(AValue); // one header line, whatever AValue held
  { Never on a multipart container. RFC 2046 puts the charset on each part, so
    the parameter means nothing here - and appending anything after "boundary="
    breaks every parser that reads the boundary as the rest of the header value.
    libmicrohttpd, under the Sagui engine, is one of those: it took the boundary
    as "ralNNN; charset=utf-8", matched no delimiter, and discarded the whole
    body without an error - requests reached the handler with no params, no body
    and no cookies, on a server that answered 200 and looked healthy. }
  { and never on an empty type: "Content-Type: ; charset=utf-8" is what a
    bodiless error answer used to carry }
  { and only on textual types: a charset means nothing on image/png or
    application/octet-stream, and "octet-stream; charset=utf-8" made browsers
    and proxies treat binary downloads as text. Multipart is not textual
    either, which keeps the boundary guard above. }
  if (FContentType <> '') and
     (Pos(StringRAL('charset='), FContentType) = 0) and
     IsTextualType(FContentType) then
    FContentType := FContentType + '; charset=utf-8';
end;

class function TRALHTTPHeaderInfo.IsTextualType(const AContentType: StringRAL): boolean;
var
  vType: StringRAL;
begin
  vType := LowerCase(AContentType);
  Result := (Pos(StringRAL('text/'), vType) = 1) or
            (Pos(StringRAL('json'), vType) > 0) or
            (Pos(StringRAL('xml'), vType) > 0) or
            (Pos(StringRAL('javascript'), vType) > 0) or
            (Pos(StringRAL('x-www-form-urlencoded'), vType) > 0);
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

function TRALHTTPHeaderInfo.GetProtocol: StringRAL;
begin
  Result := RALHTTPVersionToStr(FProtocolVersion);
end;

procedure TRALHTTPHeaderInfo.SetProtocol(const AValue: StringRAL);
begin
  FProtocolVersion := StrToRALHTTPVersion(AValue);
end;

procedure TRALHTTPHeaderInfo.Clear;
begin
  FParams.ClearParams;
  FProtocolVersion := rhvDefault;
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
  ASource.ProtocolVersion := Self.ProtocolVersion;

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

function TRALHTTPHeaderInfo.AddCookie(const ACookie: TRALCookie
  ): TRALHTTPHeaderInfo;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
  vPrefix: StringRAL;
begin
  Result := Self;
  { each cookie is a Set-Cookie line of its own. AddParam finds a param by name
    and kind, and all of these are named Set-Cookie, so the second cookie of a
    response overwrote the first - the JWT raltoken and a cookie of the
    application could not go out together. A cookie of the same name still
    replaces the one already there }
  vPrefix := ACookie.Name + '=';
  vParam := nil;
  for vInt := 0 to Pred(FParams.Count) do
    if (FParams.Index[vInt].Kind = rpkCOOKIE) and
       RALSameName(FParams.Index[vInt].ParamName, 'Set-Cookie') and
       (Pos(vPrefix, FParams.Index[vInt].AsString) = 1) then
    begin
      vParam := FParams.Index[vInt];
      Break;
    end;
  if vParam = nil then
  begin
    vParam := FParams.NewParam;
    vParam.ParamName := 'Set-Cookie';
    vParam.ContentType := rctTEXTPLAIN;
    vParam.Kind := rpkCOOKIE;
  end;
  vParam.AsString := GetCookieText(ACookie);
end;

function TRALHTTPHeaderInfo.AddCookies(ACookies: StringRAL): TRALHTTPHeaderInfo;
var
  vInt, vStart: IntegerRAL;

  procedure AddPair(AEnd: IntegerRAL);
  var
    vPair, vName, vValue: StringRAL;
    vEquals: IntegerRAL;
  begin
    vPair := Copy(ACookies, vStart, AEnd - vStart);
    vEquals := Pos(StringRAL('='), vPair);
    // RALTrim: SysUtils.Trim over a StringRAL converts to UTF-16 and back
    vName := RALTrim(Copy(vPair, 1, vEquals - 1));
    vValue := RALTrim(Copy(vPair, vEquals + 1, Length(vPair)));
    // an empty segment ("a=1;;b=2") is no cookie at all
    if (vName <> '') or (vValue <> '') then
      AddCookie(vName, vValue);
  end;

begin
  { Result first: this is a fluent method, and it never assigned it - the
    caller got whatever the register held. Then ONE scan, a Copy per cookie:
    it used to Trim and Delete off the front of the whole remaining header
    once per cookie, quadratic in the number of cookies of a header that comes
    straight from the network, before any authentication - mORMot2 and MsQuic
    hand it over whole }
  Result := Self;
  vStart := 1;
  for vInt := 1 to Length(ACookies) do
    if ACookies[POSINISTR - 1 + vInt] = ';' then
    begin
      AddPair(vInt);
      vStart := vInt + 1;
    end;
  AddPair(Length(ACookies) + 1);
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
  { the same memory as GetAcceptCompress: ProcessCommands, the engine and the
    body encoder each ask }
  if (Pointer(FContentEncoding) <> Pointer(FContentKey)) or
     (FContentGen <> CompressRegistration) then
  begin
    FContentValue := TRALCompress.GetBestCompress(FContentEncoding);
    FContentKey := FContentEncoding;
    FContentGen := CompressRegistration;
  end;
  Result := FContentValue;
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

function TRALHTTPHeaderInfo.GetRALCookie(const AName: StringRAL): TRALCookie;
begin
  Result := GetRALCookieFromParam(AName, FParams);
end;

function TRALHTTPHeaderInfo.GetBody(AIdx: IntegerRAL): TRALParam;
var
  vParam: TRALParam;
  vInt: IntegerRAL;
begin
  Result := nil;
  { Pred: "0 to Count" read one item past the list on every body lookup }
  for vInt := 0 to Pred(FParams.Count) do
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

procedure TRALHTTPHeaderInfo.SetBody(AContent: StringRAL);
begin
  Params.ClearParams(rpkBODY); // the body only: headers, cookies and query stay
  Params.AddValue(AContent, rpkBODY);
end;

procedure TRALHTTPHeaderInfo.SetBody(AContent: TStream);
begin
  Params.ClearParams(rpkBODY);
  Params.AddValue(AContent, rpkBODY);
end;

procedure TRALHTTPHeaderInfo.SetContentCompress(const AValue: TRALCompressType);
begin
  { only a coding this program can produce. ContentCompress already reads back
    ctNone for one whose unit is not linked, so the body went out as it was -
    while Content-Encoding, written from here, still named it: a server with
    CompressType = ctZStd and no zstd linked answered "zstd" over plain bytes }
  if GetCompressClass(AValue) <> nil then
    FContentEncoding := TRALCompress.CompressToString(AValue)
  else
    FContentEncoding := '';
end;

function TRALHTTPHeaderInfo.Body: TRALParam;
begin
  Result := ParamByName('ral_body');
end;

{ The next comma-separated entry of AText from APos on, trimmed, with APos left
  past it; False once the text is over. A scan instead of a Delete off the
  front of what is left: these headers come straight from the network, before
  any authentication, and cutting them entry by entry was quadratic in the
  number of entries. }
function NextListEntry(const AText: StringRAL; var APos: IntegerRAL;
  out AEntry: StringRAL): boolean;
var
  vEnd: IntegerRAL;
begin
  Result := APos <= Length(AText);
  if not Result then
    Exit;
  vEnd := APos;
  while (vEnd <= Length(AText)) and (AText[POSINISTR - 1 + vEnd] <> ',') do
    Inc(vEnd);
  AEntry := RALTrim(Copy(AText, APos, vEnd - APos));
  APos := vEnd + 1;
end;

function TRALHTTPHeaderInfo.HasValidContentEncoding: boolean;
var
  vStr, vEnc: StringRAL;
  vPos: IntegerRAL;
  vSupported: TRALCompressTypes;
begin
  { no LowerCase of the whole header - on Delphi two UTF-8/UTF-16 conversions:
    StringToCompress lowercases each name already, and identity is compared
    with RALSameName }
  vStr := RALTrim(FContentEncoding);
  Result := vStr = '';
  if Result then
    Exit;

  vSupported := GetSuportedCompress;
  vPos := 1;
  while NextListEntry(vStr, vPos, vEnc) do
  begin
    { identity is "not encoded" (RFC 9110 8.4.1), so a body declared with it
      is one this server can read. A known coding whose compressor was not
      linked in (br without RALCompressBrotli) is not: the body could not be
      decoded, and it used to go on and vanish in the decoder - 415 says why }
    if (TRALCompress.StringToCompress(vEnc) in vSupported) or
       RALSameName(Copy(vEnc, 1, 8), 'identity') then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ RFC 9110 12.5.3: without the header, and with any list that does not refuse
  it, identity - the body as it is - stays acceptable. So "identity", "gzip;q=1"
  or a coding this build did not link are all fine: the answer simply goes out
  uncompressed. The request is only unanswerable when identity is refused on
  purpose ("identity;q=0", or "*;q=0" with no identity entry) and none of the
  codings still accepted is one this server has. Everything else used to be
  answered 415, which speaks of the request body and sent whoever was debugging
  after the Content-Type. }
function TRALHTTPHeaderInfo.HasValidAcceptEncoding: boolean;
var
  vStr, vEnc, vName: StringRAL;
  vPos: IntegerRAL;
  vQuality, vIdentity, vStar: Double;
  vSupported: TRALCompressTypes;
  vHasCoding, vRefused: boolean;
begin
  Result := True;
  vStr := RALTrim(FAcceptEncoding);
  if vStr = '' then
    Exit;

  vIdentity := -1;
  vStar := -1;
  vHasCoding := False;
  vSupported := GetSuportedCompress;
  vPos := 1;
  while NextListEntry(vStr, vPos, vEnc) do
  begin
    if vEnc = '' then
      Continue;

    RALSplitCoding(vEnc, vName, vQuality);
    if vName = 'identity' then
      vIdentity := vQuality
    else if vName = '*' then
    begin
      vStar := vQuality;
      if (vQuality > 0) and (vSupported <> []) then
        vHasCoding := True;
    end
    else if (vQuality > 0) and
            (TRALCompress.NameToCompress(vName) in vSupported) then
      vHasCoding := True;
  end;

  vRefused := (vIdentity = 0) or ((vIdentity < 0) and (vStar = 0));
  Result := (not vRefused) or vHasCoding;
end;

end.
