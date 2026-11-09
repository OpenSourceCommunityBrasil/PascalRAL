/// Unit with the definitions of base classes of PascalRAL
unit RALCustomObjects;

interface

uses
  Classes, SysUtils,
  RALParams, RALTypes, RALConsts, RALMIMETypes, RALTools, RALCompress, RALStream;

type

  /// Base class of visual components
  TRALComponent = class(TComponent)
  private
    function GetVersion: string;
  public
    { Whether a published property still means anything with the component
      configured as it is now - the mORMot2 server's HttpSysDomain says
      nothing outside smHttpSys, and ShareConnection says nothing on an engine
      whose transport is one socket per object.

      It is what the Object Inspector filter asks (see TRALSelectionEditor in
      RALRegister), and it lives here, on the component, so that the design
      package never has to know one engine from another.

      Hidden is not forbidden, and nothing may be built on top of this: a
      property left over from another configuration has to stay harmless, so
      an irrelevant value is ignored and never raises. What does raise is an
      explicit choice that cannot work - a Mode this platform has no server
      for, an HTTPVersion this engine cannot ask for - and those are never
      hidden. }
    function IsPropertyRelevant(const AName: StringRAL): boolean; virtual;
  published
    /// The RAL version.
    property Version: string read GetVersion;
  end;

  /// Base class of REQUEST and RESPONSE classes
  TRALHTTPHeaderInfo = class
  private
    FAcceptEncoding: StringRAL;
    FAcceptEncription: StringRAL;
    /// The compressor registration FAcceptValue was read under.
    FAcceptGen: IntegerRAL;
    { what AcceptCompress and ContentCompress answered last, for which text and
      under which set of registered compressors: a request reads each several
      times, and the header only changes when somebody writes it. The text is
      compared by reference - kept here, it cannot be freed and its address
      taken by another string - so a hit costs two comparisons }
    FAcceptKey: StringRAL;
    /// AcceptCompress, cached.
    FAcceptValue: TRALCompressType;
    FContentDisposition: StringRAL;
    FContentDispositionInline: Boolean;
    FContentEncoding: StringRAL;
    FContentEncription: StringRAL;
    /// The compressor registration FContentValue was read under.
    FContentGen: IntegerRAL;
    /// The ContentEncoding FContentValue was read from.
    FContentKey: StringRAL;
    FContentType: StringRAL;
    /// ContentCompress, cached.
    FContentValue: TRALCompressType;
    FCriptoKey: StringRAL;
    /// The params.
    FParams: TRALParams;
    FParent: TObject;
    FProtocolVersion: TRALHTTPVersion;
  protected
    /// Hands the cipher (and with AKey the key) to the params.
    procedure ApplyCripto(AKey: boolean);
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
    { How TakeWireStream builds the body. The request side of a client keeps
      a form or a multipart uncompressed (see TRALParams.EffectiveCompress) and
      leaves the params as they are, since a retry sends them again; the
      response of a server may compress anything and gives its streams away,
      because the engine may send after the response is gone (Sagui does) }
    function WireCompressMultipart: boolean; virtual;
    /// Whether TakeWire* gives the streams away (False on a client request).
    function WireConsume: boolean; virtual;
  public
    /// Creates the object, owned by AOwner.
    constructor Create(AOwner : TObject); virtual;
    destructor Destroy; override;

    /// Adds a body param with AText. Returns Self.
    function AddBody(const AText: StringRAL; const AContextType: StringRAL = rctAPPLICATIONJSON): TRALHTTPHeaderInfo; virtual;
    /// Adds a cookie. Returns Self.
    function AddCookie(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; overload; virtual; deprecated 'use AddCookie(ACookie:TRALCookie) instead';
    /// Adds a cookie with its attributes. Returns Self.
    function AddCookie(const ACookie: TRALCookie):TRALHTTPHeaderInfo; overload; virtual;
    /// Adds the cookies of a Cookie header. Returns Self.
    function AddCookies(ACookies: StringRAL): TRALHTTPHeaderInfo; virtual;
    /// Adds a form field. Returns Self.
    function AddField(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    /// Adds the file AFileName as a body param. Returns Self.
    function AddFile(const AFileName: StringRAL): TRALHTTPHeaderInfo; overload; virtual;
    /// Adds a stream as a file body param. Returns Self.
    function AddFile(AStream: TStream; const AFileName: StringRAL = ''): TRALHTTPHeaderInfo; overload; virtual;
    /// Adds a header. Returns Self.
    function AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    /// Adds a query param. Returns Self.
    function AddQuery(const AName: StringRAL; const AValue: StringRAL): TRALHTTPHeaderInfo; virtual;
    { The body of the request or the response, when it is one value - raw text,
      JSON, a file (the param named 'ral_body'). A body the engine already split
      into params is not here: form fields (x-www-form-urlencoded) and the parts
      of a multipart are rpkFIELD/rpkBODY params, read them with ParamByName,
      and Params.AssignParamsUrl(rpkFIELD) gives the form back as text. For such
      a body this answers nil - Body.AsString is still safe and gives '' }
    function Body: TRALParam;
    /// Empties the params and the headers it keeps.
    procedure Clear; virtual;
    /// Copies this object into ASource.
    procedure Clone(ASource: TRALHTTPHeaderInfo);
    /// The body param at AIdx, or nil.
    function GetBody(AIdx: IntegerRAL): TRALParam; virtual;
    /// The value of the cookie AName.
    function GetCookie(const AName: StringRAL): StringRAL; virtual; deprecated 'use GetRALCookie(AName): TRALCookie instead';
    /// The value of the form field AName.
    function GetField(const AName: StringRAL): StringRAL; virtual;
    /// The value of the header AName.
    function GetHeader(const AName: StringRAL): StringRAL; virtual;
    /// The value of the query param AName.
    function GetQuery(const AName: StringRAL): StringRAL; virtual;
    { The cookie of that name, received (Name and Value; the attributes of a
      Set-Cookie stay with the browser) or set with AddCookie(TRALCookie) on
      this response (the whole record). An empty record when there is none. }
    function GetRALCookie(const AName: StringRAL): TRALCookie; virtual;
    /// Whether the answer can go out in a coding the client accepts.
    function HasValidAcceptEncoding: boolean;
    /// Whether this build can decode the body as Content-Encoding says.
    function HasValidContentEncoding: boolean;
    { True for the media types a charset parameter applies to (text/*, JSON, XML,
      JavaScript, forms) }
    class function IsTextualType(const AContentType: StringRAL): boolean;
    { The param with this name, of any kind - query string, header, cookie, form
      field - and the first one added when there are several: the query string
      and the headers come in before any authentication event runs, so a value
      added later with AddParam does not override one the client sent under the
      same name (use Params.ReplaceParam for that, or Params.GetKind to ask for
      one kind). A JSON body is not split into params unless the server has
      JSONBodyToParams on; read it from Body otherwise. }
    function ParamByName(const AParamName: StringRAL): TRALParam;
    /// fills the body with the String AContent
    procedure SetBody(AContent: StringRAL); overload; virtual;
    /// fills the body with the Stream AContent
    procedure SetBody(AContent: TStream); overload; virtual;
    { ENGINE API, receiving: the body as it came off the wire, compressed and/or
      encrypted as ContentEncoding/ContentEncription say - so those must be
      filled first; the key is the one already in Params.CriptoOptions (the
      crypto plugin's on a server, the client's own on a client). It is decoded
      once, into Params, and with AOwnership other than boCopy the engine's
      buffer is not copied at all: see TRALBodyOwnership for which to pass. An
      empty body adds nothing (an owned one is freed). RequestStream (server)
      and ResponseStream (client) are the assignments that copy }
    procedure SetWireBody(AStream: TStream; AOwnership: TRALBodyOwnership); overload; virtual;
    { The body as a string the engine holds: a view that keeps the reference, nothing
      copied }
    procedure SetWireBody(const ABody: RawByteString); overload;
    { The body in memory the engine keeps for as long as this object lives
      (Sagui's payload, a QUIC frame): a read-only view, nothing copied }
    procedure SetWireBody(ABuffer: Pointer; ASize: Int64RAL); overload;
    { ENGINE API, sending: the body for the wire, compressed and/or encrypted
      as ContentCompress/ContentCripto/CriptoKey ask, as one stream the engine
      frees; nil when there is no body. With no transform it is the body
      itself, nothing copied. ContentType, ContentDisposition and
      ContentCompress are set to what was really done - read ContentEncoding
      after this call: a type already compressed goes out as it is.
      On a server response the params give their streams away; call it once }
    function TakeWireStream: TStream; virtual;
    { TakeWireStream as a string, for the engines that send one (mORMot2): a
      lone text body with no transform is the handler's own string }
    function TakeWireString: RawByteString; virtual;
  published
    /// The best coding of AcceptEncoding this build has.
    property AcceptCompress: TRALCompressType read GetAcceptCompress;
    /// The cipher of AcceptEncription.
    property AcceptCripto: TRALCriptoType read GetAcceptCripto;
    /// The Accept-Encoding header.
    property AcceptEncoding: StringRAL read FAcceptEncoding write FAcceptEncoding;
    /// The Accept-Encription header.
    property AcceptEncription: StringRAL read FAcceptEncription write FAcceptEncription;
    /// The coding of the body (Content-Encoding).
    property ContentCompress: TRALCompressType read GetContentCompress write SetContentCompress;
    /// The cipher of the body (Content-Encription).
    property ContentCripto: TRALCriptoType read GetContentCripto write SetContentCripto;
    /// The Content-Disposition header.
    property ContentDisposition: StringRAL read FContentDisposition write SetContentDisposition;
    /// Whether Content-Disposition is inline rather than attachment.
    property ContentDispositionInline: boolean read FContentDispositionInline write FContentDispositionInline;
    /// The Content-Encoding header.
    property ContentEncoding: StringRAL read FContentEncoding write FContentEncoding;
    /// The Content-Encription header.
    property ContentEncription: StringRAL read FContentEncription write FContentEncription;
    /// The Content-Type header; the charset is added for textual types.
    property ContentType: StringRAL read FContentType write SetContentType;
    /// The key of the body cipher.
    property CriptoKey: StringRAL read FCriptoKey write FCriptoKey;
    /// The params: query, headers, body, fields, cookies, files.
    property Params: TRALParams read GetParams;
    /// The object that owns this one.
    property Parent: TObject read FParent;
    { The same fact as ProtocolVersion, spelled the way the wire spells it -
      '1.0', '1.1', '2.0', or '' when the transport could not tell.
      One field is behind the two: an engine that fills either one has filled
      both, and they can never disagree. Writing text
      a version cannot be parsed out of leaves rhvDefault, so Protocol reads
      back as '' - it is not a free-text field. }
    property Protocol: StringRAL read GetProtocol write SetProtocol stored False;
    { Which HTTP version carried this message, as the transport REPORTS it -
      never what was asked for, since ALPN settles that during the TLS
      handshake. A transport that cannot tell leaves rhvDefault.
      On a RESPONSE it is what the client engine could read back. On a
      REQUEST it is what the server saw the client arrive on, which is the
      only place the answer is reliable: the http.sys server reads it from
      the driver, while a client on Windows has to guess from the status
      line - and an HTTP/2 response does not have one, so it under-reports. }
    property ProtocolVersion: TRALHTTPVersion read FProtocolVersion write FProtocolVersion;
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
begin
  { read where it is, with no lower-case copy (a UTF-16 trip on Delphi): this
    runs for every request and every response }
  if FContentEncription = '' then
    Result := crNone
  else if RALPosText('aes256cbc_pkcs7', FContentEncription) > 0 then
    Result := crAES256
  else if RALPosText('aes192cbc_pkcs7', FContentEncription) > 0 then
    Result := crAES192
  else if RALPosText('aes128cbc_pkcs7', FContentEncription) > 0 then
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

var
  { The types every response and every param start with, and what
    SetContentType makes of each - computed once. On Delphi a literal assigned
    to a field is copied to the heap every time; a string held here has a
    reference count, so handing it to a field costs none }
  gKnownGiven: array[0..5] of StringRAL;
  /// The Content-Type SetContentType stores for each of gKnownGiven.
  gKnownFinal: array[0..5] of StringRAL;

/// AValue as one header line, with the charset where it belongs.
function SafeContentType(const AValue: StringRAL): StringRAL;
begin
  Result := RALSafeHeaderText(AValue); // one header line, whatever AValue held
  { Never on a multipart container. RFC 2046 puts the charset on each part, so
    the parameter means nothing here - and appending anything after "boundary="
    breaks every parser that reads the boundary as the rest of the header value
    (libmicrohttpd, under the Sagui engine, then drops the whole body). }
  { and never on an empty type ("Content-Type: ; charset=utf-8") }
  { and only on textual types: a charset means nothing on image/png or
    application/octet-stream, and "octet-stream; charset=utf-8" makes browsers
    and proxies treat binary downloads as text. Multipart is not textual
    either, which keeps the boundary guard above. }
  if (Result <> '') and
     (RALPosText('charset=', Result) = 0) and
     TRALHTTPHeaderInfo.IsTextualType(Result) then
    Result := Result + '; charset=utf-8';
end;

procedure TRALHTTPHeaderInfo.SetContentType(const AValue: StringRAL);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to High(gKnownGiven) do
    if AValue = gKnownGiven[vInt] then
    begin
      FContentType := gKnownFinal[vInt];
      Exit;
    end;
  FContentType := SafeContentType(AValue);
end;

class function TRALHTTPHeaderInfo.IsTextualType(const AContentType: StringRAL): boolean;
begin
  { compared where it is, with no lower-case copy (a UTF-16 trip and three
    allocations on Delphi): this runs every time a content type is set }
  Result := (RALPosText('text/', AContentType) = 1) or
            (RALPosText('json', AContentType) > 0) or
            (RALPosText('xml', AContentType) > 0) or
            (RALPosText('javascript', AContentType) > 0) or
            (RALPosText('x-www-form-urlencoded', AContentType) > 0);
end;

function TRALHTTPHeaderInfo.GetAcceptCripto: TRALCriptoType;
begin
  { read where it is, with no lower-case copy (a UTF-16 trip on Delphi): this
    runs for every request and every response }
  if FAcceptEncription = '' then
    Result := crNone
  else if RALPosText('aes256cbc_pkcs7', FAcceptEncription) > 0 then
    Result := crAES256
  else if RALPosText('aes192cbc_pkcs7', FAcceptEncription) > 0 then
    Result := crAES192
  else if RALPosText('aes128cbc_pkcs7', FAcceptEncription) > 0 then
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

function TRALHTTPHeaderInfo.WireCompressMultipart: boolean;
begin
  Result := True;
end;

function TRALHTTPHeaderInfo.WireConsume: boolean;
begin
  Result := True;
end;

procedure TRALHTTPHeaderInfo.SetWireBody(AStream: TStream;
  AOwnership: TRALBodyOwnership);
begin
  if (AStream = nil) or (AStream.Size = 0) then
  begin
    if AOwnership = boOwned then
      AStream.Free;
    Exit;
  end;
  { from the headers, every time: TRALParams is born with gzip, and decoding
    by that default instead of by what the other side said is how a plain body
    gets inflated }
  FParams.CompressType := ContentCompress;
  ApplyCripto(False);
  FParams.DecodeBody(AStream, FContentType, FContentDisposition, AOwnership);
end;

procedure TRALHTTPHeaderInfo.SetWireBody(const ABody: RawByteString);
begin
  if ABody = '' then
    Exit;
  SetWireBody(TRALStringView.Create(ABody), boOwned);
end;

procedure TRALHTTPHeaderInfo.SetWireBody(ABuffer: Pointer; ASize: Int64RAL);
begin
  if (ABuffer = nil) or (ASize <= 0) then
    Exit;
  { the view is ours, the memory under it is not: owned view, lent memory -
    and a view is never written to, so the cipher works on a copy }
  SetWireBody(TRALMemoryView.Create(ABuffer, ASize), boOwned);
end;

{ The cipher of the body, from the headers, handed to the params - which create
  their options only when there is a cipher to hold: a plain request makes
  none. AKey also hands over the key this side holds (the way out); on the way
  in the key is already in the params, put there by the crypto plugin }
procedure TRALHTTPHeaderInfo.ApplyCripto(AKey: boolean);
var
  vCripto: TRALCriptoType;
begin
  vCripto := ContentCripto;
  if (vCripto = crNone) and not FParams.HasCriptoOptions then
    Exit;
  FParams.CriptoOptions.CriptType := vCripto;
  if AKey then
    FParams.CriptoOptions.Key := FCriptoKey;
end;

function TRALHTTPHeaderInfo.TakeWireStream: TStream;
var
  vContentType, vContentDisposition: StringRAL;
begin
  FParams.CompressType := ContentCompress;
  ApplyCripto(True);
  FParams.ContentDispositionInline := FContentDispositionInline;

  vContentType := '';
  vContentDisposition := '';
  Result := FParams.TakeWireStream(vContentType, vContentDisposition,
    WireCompressMultipart, WireConsume);
  ContentType := vContentType;
  FContentDisposition := vContentDisposition;
  { what was done, not what was asked: a JPEG goes out uncompressed, and so
    does a body whose compressor is not linked }
  ContentCompress := FParams.CompressType;
end;

function TRALHTTPHeaderInfo.TakeWireString: RawByteString;
var
  vContentType, vContentDisposition: StringRAL;
begin
  FParams.CompressType := ContentCompress;
  ApplyCripto(True);
  FParams.ContentDispositionInline := FContentDispositionInline;

  vContentType := '';
  vContentDisposition := '';
  Result := FParams.TakeWireString(vContentType, vContentDisposition,
    WireCompressMultipart, WireConsume);
  ContentType := vContentType;
  FContentDisposition := vContentDisposition;
  ContentCompress := FParams.CompressType;
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
  { Result first: this is a fluent method. Then one scan, a Copy per cookie:
    linear in the number of cookies of a header that comes straight from the
    network, before any authentication - mORMot2 and MsQuic hand it over
    whole }
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
    Result := vParam.AsString
  else
    // one AddCookie(TRALCookie) set on a response sits inside a Set-Cookie line
    Result := GetRALCookie(AName).Value;
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
  { only a coding this program can produce: ContentCompress reads back ctNone
    for one whose unit is not linked, and Content-Encoding must not name a
    coding the body does not have }
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
      linked in (br without RALCompressBrotli) is not: the body cannot be
      decoded, and 415 says why }
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
  uncompressed. The request is only unanswerable when identity is refused
  explicitly ("identity;q=0", or "*;q=0" with no identity entry) and none of the
  codings still accepted is one this server has. A 415 would speak of the
  request body, which is not the problem. }
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

/// Fills gKnownGiven and gKnownFinal.
procedure FillKnownTypes;
var
  vInt: IntegerRAL;
begin
  gKnownGiven[0] := rctTEXTPLAIN;
  gKnownGiven[1] := rctAPPLICATIONJSON;
  gKnownGiven[2] := rctTEXTHTML;
  gKnownGiven[3] := rctAPPLICATIONOCTETSTREAM;
  gKnownGiven[4] := rctAPPLICATIONXML;
  gKnownGiven[5] := rctMULTIPARTFORMDATA;
  { Delphi copies a literal into a global, as into any field: what is kept
    here is a heap string, a type with a charset or not }
  for vInt := 0 to High(gKnownGiven) do
    gKnownFinal[vInt] := SafeContentType(gKnownGiven[vInt]);
end;

initialization
  FillKnownTypes;

end.
