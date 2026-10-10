/// Unit that contains everything related to the HTTP response of the traffic
unit RALResponse;

interface

uses
  Classes, SysUtils,
  RALTypes, RALParams, RALMIMETypes, RALCustomObjects, RALStream, RALConsts,
  RALCompress, RALTools;

type

  { TRALResponse }

  /// Base class for everything related to data response
  TRALResponse = class(TRALHTTPHeaderInfo)
  private
    FBodyStream: TStream;
    FContentEncoded: boolean;
    FErrorCode: IntegerRAL;
    FStatusCode: IntegerRAL;
    FTransportError: TRALTransportError;
  protected
    /// The body, decoded (see ResponseStream)
    function GetResponseStream: TStream;
    /// The body, decoded, as text (see ResponseText)
    function GetResponseText: StringRAL;
    /// Assign a Stream into the Response
    procedure SetResponseStream(const AValue: TStream); virtual; abstract;
    /// Assign an UTF8String into the Response
    procedure SetResponseText(const AValue: StringRAL); virtual; abstract;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    /// Append an UTF8 String to the response
    function AddBody(const AText: StringRAL; const AContextType: StringRAL = rctTEXTPLAIN): TRALResponse; reintroduce;
    /// Append a name:value cookie to the response
    function AddCookie(const AName: StringRAL; const AValue: StringRAL): TRALResponse; reintroduce; overload; deprecated 'use AddCookie(ACookie: TRALCookie) instead';
    /// Append a TRALCookie to the response
    function AddCookie(const ACookie: TRALCookie): TRALResponse; reintroduce; overload;
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
    /// Sets the response with the given status code, Data Stream and Content-Type.
    /// AStream is copied: the caller still owns it
    procedure Answer(AStatusCode: IntegerRAL; const AStream: TStream;
                     const AContentType: StringRAL = rctAPPLICATIONJSON); overload;
    { The same, and with AOwnsStream the response TAKES AStream instead of
      copying it: it is sent as it is and freed with the response, so the
      caller must not touch it again. A stream built only to answer - a
      file, a query saved to memory - costs its size once, not twice.
      False copies, like the overload above }
    procedure Answer(AStatusCode: IntegerRAL; AStream: TStream;
                     const AContentType: StringRAL; AOwnsStream: boolean); overload;
    /// Sets the response with the given status code
    procedure Answer(AStatusCode: IntegerRAL); overload;
    /// Loads and set a file to the response with the given AFileName and sets the disposition
    /// to inline by default.
    procedure Answer(const AFileName: StringRAL; const DispositionInline: boolean = true); overload;
    { A stream to WRITE the body into, owned by the response and sent as it
      is - Storage.SaveToStream(Query, AResponse.BodyStream) answers a query
      with no intermediate copy. It is the body until something else replaces
      it (Answer, ResponseText :=); asking again returns the same stream, so
      several writes append. The body goes out with the response's
      ContentType, whenever that is set }
    function BodyStream: TStream;
    /// Empties out the Response and sets default values
    procedure Clear; override;
    /// Fills the 'ADest' Strings with RALParams Cookies' Headers
    procedure GetParamsCookies(ADest: TStringList; ADateTime: TDateTime);
    /// Returns an UTF8 String with RALParams Cookies' Headers
    function GetParamsCookiesText(ADateTime: TDateTime; AHeader: StringRAL = 'Set-Cookie: ') : StringRAL;
    { The body, encoded for the wire when AEncode (a new stream the caller
      frees): what the server engines used before TakeWireStream, which does
      the same without copying a body that has nothing to transform. Without
      AEncode, the body as it is }
    function GetResponseEncStream(const AEncode: boolean = true): TStream; virtual; abstract;
    function GetResponseEncText(const AEncode: boolean = true): StringRAL; virtual; abstract;
    function TakeWireStream: TStream; override;
    function TakeWireString: RawByteString; override;

    /// The body already carries the coding ContentEncoding names - a file kept
    /// compressed on disk, which the WebModule serves as it is - so it goes out
    /// without being compressed again. ContentEncoding is written as text then,
    /// since the coding need not be one this program can produce
    property ContentEncoded: boolean read FContentEncoded write FContentEncoded;
    property ResponseStream: TStream read GetResponseStream write SetResponseStream;
    { The body, DECODED - never compressed or encrypted, whatever the headers
      say. On a server it is what the handler answered, as a new stream the
      caller frees (up to 1.2 it was the encoded body - an engine that read it
      for the wire must use TakeWireStream). On a client it is the body that
      arrived, owned by the response, and a multipart comes back as the bytes
      received, not put back together with another boundary }
    property ResponseText: StringRAL read GetResponseText write SetResponseText;
  published
    /// TCP Client Connection Error
    property ErrorCode: IntegerRAL read FErrorCode write FErrorCode;
    /// HTTP StatusCode
    property StatusCode: IntegerRAL read FStatusCode write FStatusCode;
    /// How the send attempt ended for the transport. rteNone means an HTTP
    /// response arrived (even a 4xx/5xx one); anything else means it did not,
    /// and then StatusCode carries no meaning. Filled by the client engines,
    /// and read by TRALClientHTTP.BeforeSendUrl to decide whether resending on
    /// another BaseURL is safe.
    property TransportError: TRALTransportError read FTransportError
                                                write FTransportError;
  end;

  /// Derived class to handle ServerResponse
  TRALServerResponse = class(TRALResponse)
  public
    function GetResponseEncStream(const AEncode: boolean = true): TStream; override;
    function GetResponseEncText(const AEncode: boolean = true): StringRAL; override;
  protected
    procedure SetResponseStream(const AValue: TStream); override;
    procedure SetResponseText(const AValue: StringRAL); override;
  end;

  /// Derived class to handle ClientResponse
  TRALClientResponse = class(TRALResponse)
  private
    FStream: TStream;
  public
    constructor Create(AOwner : TObject); override;
    destructor Destroy; override;

    procedure Clear; override;
    { The body that arrived, decoded - the one stream the body params read
      from, owned by the response. AEncode means nothing here }
    function GetResponseEncStream(const AEncode: boolean = true): TStream; override;
    function GetResponseEncText(const AEncode: boolean = true): StringRAL; override;
    procedure SetWireBody(AStream: TStream; AOwnership: TRALBodyOwnership); override;
  protected
    procedure SetResponseStream(const AValue: TStream); override;
    procedure SetResponseText(const AValue: StringRAL); override;
  end;

implementation

uses
  RALServer;

{ TRALResponse }

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; const AMessage: StringRAL;
  const AContentType: StringRAL);
begin
  StatusCode := AStatusCode;
  ContentType := AContentType;
  ResponseText := AMessage;
end;

procedure TRALResponse.Answer(const AFileName: StringRAL;
  const DispositionInline: boolean);
begin
  AddFile(AFileName);
  ContentDispositionInline := DispositionInline;
end;

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; const AStream: TStream;
  const AContentType: StringRAL);
begin
  StatusCode := AStatusCode;
  ContentType := AContentType;
  ResponseStream := AStream;
end;

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; AStream: TStream;
  const AContentType: StringRAL; AOwnsStream: boolean);
var
  vParam: TRALParam;
begin
  if not AOwnsStream then
  begin
    Answer(AStatusCode, AStream, AContentType);
    Exit;
  end;

  StatusCode := AStatusCode;
  ContentType := AContentType;
  Params.ClearParams(rpkBODY);
  if (AStream = nil) or (AStream.Size = 0) then
  begin
    AStream.Free;
    Exit;
  end;
  vParam := Params.NewParam;
  vParam.ParamName := 'ral_body';
  vParam.AdoptStream(AStream);
  vParam.ContentType := ContentType;
  vParam.Kind := rpkBODY;
end;

function TRALResponse.BodyStream: TStream;
var
  vParam: TRALParam;
begin
  { the same stream while it is still the body: several writes append }
  vParam := Body;
  if (FBodyStream <> nil) and (vParam <> nil) and (not vParam.IsText) and
     (vParam.Content = FBodyStream) and (Params.Count(rpkBODY) = 1) then
  begin
    Result := FBodyStream;
    Exit;
  end;

  Params.ClearParams(rpkBODY);
  vParam := Params.NewParam;
  vParam.ParamName := 'ral_body';
  vParam.AdoptStream(TMemoryStream.Create);
  vParam.ContentType := ContentType;
  vParam.Kind := rpkBODY;
  FBodyStream := vParam.Content;
  Result := FBodyStream;
end;

function TRALResponse.TakeWireStream: TStream;
var
  vParam: TRALParam;
  vEncoding: StringRAL;
begin
  { BodyStream may have been asked before the handler set the content type }
  vParam := Body;
  if (FBodyStream <> nil) and (vParam <> nil) and (not vParam.IsText) and
     (vParam.Content = FBodyStream) then
    vParam.ContentType := ContentType;
  FBodyStream := nil;
  { coded already (ContentEncoded): nothing compresses it again, and
    ContentEncoding goes out as it was written - the coding need not be one
    this program can produce }
  if FContentEncoded then
  begin
    vEncoding := ContentEncoding;
    ContentCompress := ctNone;
    Result := inherited TakeWireStream;
    ContentEncoding := vEncoding;
  end
  else
    Result := inherited TakeWireStream;
end;

function TRALResponse.TakeWireString: RawByteString;
var
  vParam: TRALParam;
  vEncoding: StringRAL;
begin
  vParam := Body;
  if (FBodyStream <> nil) and (vParam <> nil) and (not vParam.IsText) and
     (vParam.Content = FBodyStream) then
    vParam.ContentType := ContentType;
  FBodyStream := nil;
  if FContentEncoded then
  begin
    vEncoding := ContentEncoding;
    ContentCompress := ctNone;
    Result := inherited TakeWireString;
    ContentEncoding := vEncoding;
  end
  else
    Result := inherited TakeWireString;
end;

procedure TRALResponse.GetParamsCookies(ADest: TStringList; ADateTime: TDateTime);
var
  vInt: integer;
  vAttrs: StringRAL;
  vParam: TRALParam;
begin
  { what a plain name=value cookie carries: the server's CookieLife, as a date
    that does not depend on the locale (FormatDateTime wrote its time separator
    where ':' stood), and Path=/ so the browser sends it to every route - with
    no Path it kept the cookie for the folder of the URL that set it }
  vAttrs := '; Expires=' + RALHTTPDate(RALDateTimeToGMT(ADateTime)) + '; Path=/';

  for vInt := 0 to Pred(Params.Count) do
  begin
    vParam := TRALParam(Params.Index[vInt]);
    if (vParam <> nil) and (vParam.Kind = rpkCOOKIE) then
    begin
      { AddCookie(TRALCookie) stores the whole Set-Cookie value - name,
        value, Expires, Path, HttpOnly, Secure - in a param named Set-Cookie:
        that one goes out as it is. A plain name=value param gets the
        attributes above. Every engine builds its cookies from this list,
        so this is where a CR or LF in one is taken out (RALSafeHeaderText) }
      if RALSameName(vParam.ParamName, 'Set-Cookie') then
        ADest.Add(RALSafeHeaderText(vParam.AsString))
      else
        ADest.Add(RALSafeHeaderText(vParam.ParamName + '=' + vParam.AsString) + vAttrs);
    end;
  end;
end;

function TRALResponse.GetParamsCookiesText(ADateTime: TDateTime; AHeader: StringRAL): StringRAL;
var
  vDest: TStringList;
  vInt: integer;
begin
  Result := '';
  vDest := TStringList.Create;
  try
    GetParamsCookies(vDest, ADateTime);
    for vInt := 0 to Pred(vDest.Count) do
    begin
      if Result <> '' then
        Result := Result + HTTPLineBreak;
      Result := Result + AHeader + vDest.Strings[vInt];
    end;
  finally
    FreeAndNil(vDest);
  end;
end;

procedure TRALResponse.Clear;
begin
  inherited Clear;
  FBodyStream := nil;
  FStatusCode := -1;
  FErrorCode := 0;
  FTransportError := rteNone;
  FContentEncoded := False;
end;

procedure TRALResponse.Answer(AStatusCode: IntegerRAL);
var
  vResp: StringRAL;
begin
  FStatusCode := AStatusCode;
  if AStatusCode >= HTTP_BadRequest then
    ContentType := rctTEXTHTML;

  if Parent.InheritsFrom(TRALServer) then begin
    vResp := TRALServer(Parent).ResponsePages.HTMLPage[AStatusCode];
    if vResp <> '' then
      ResponseText := vResp;
  end;
end;

function TRALResponse.AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALResponse;
begin
  inherited AddHeader(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddField(const AName: StringRAL; const AValue: StringRAL
  ): TRALResponse;
begin
  inherited AddField(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddBody(const AText: StringRAL;
  const AContextType: StringRAL): TRALResponse;
begin
  inherited AddBody(AText, AContextType);
  Result := Self;
end;

function TRALResponse.AddCookie(const AName: StringRAL; const AValue: StringRAL
  ): TRALResponse;
begin
  inherited AddCookie(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddCookie(const ACookie: TRALCookie): TRALResponse;
begin
  inherited AddCookie(ACookie);
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

constructor TRALResponse.Create(AOwner: TObject);
begin
  inherited;
  ContentType := rctAPPLICATIONJSON;
end;

destructor TRALResponse.Destroy;
begin
  inherited;
end;

function TRALResponse.GetResponseStream: TStream;
begin
  { decoded on both sides: on a server, what the handler answered - an
    OnResponse reading it gets the text, not gzip }
  Result := GetResponseEncStream(False);
end;

function TRALResponse.GetResponseText: StringRAL;
begin
  { the body as text, never what goes on the wire: on the server that used to
    run the whole encoding - multipart, gzip, AES - to be thrown away, and
    rewrote ContentType on the way. An engine wants GetResponseEncText }
  Result := GetResponseEncText(False);
end;

{ TRALServerResponse }

function TRALServerResponse.GetResponseEncStream(const AEncode: boolean): TStream;
var
  vContentType, vContentDisposition: StringRAL;
  vSource: TStream;
  vBody: TRALBodyStream;
  vCompress: TRALCompressType;
  vCripto: TRALCriptoType;
  vKey: StringRAL;
begin
  Result := nil;
  vContentType := '';
  vContentDisposition := '';
  if not AEncode then
  begin
    { what the handler answered, as the caller's own copy - the contract
      ResponseStream always had. Nothing here is changed }
    vSource := Params.PlainBody(vContentType, vContentDisposition);
    if vSource = nil then
      Exit;
    try
      vBody := TRALBodyStream.Create(vSource.Size);
      try
        vSource.Position := 0;
        RALCopyStream(vSource, vBody, vSource.Size);
        Result := vBody.Detach;
      finally
        vBody.Free;
      end;
    finally
      vSource.Free;
    end;
    Result.Position := 0;
    Exit;
  end;

  { encoded, for an engine written before TakeWireStream. The params are left
    as they were: this used to set the response's key and compression on them
    and leave them there, so a later read of the body saw them too }
  vCompress := Params.CompressType;
  vCripto := Params.CriptoOptions.CriptType;
  vKey := Params.CriptoOptions.Key;
  try
    Params.CriptoOptions.CriptType := ContentCripto;
    Params.CriptoOptions.Key := CriptoKey;
    if FContentEncoded then
      Params.CompressType := ctNone // coded already - see ContentEncoded
    else
      Params.CompressType := ContentCompress;
    Params.ContentDispositionInline := ContentDispositionInline;

    Result := Params.EncodeBody(vContentType, vContentDisposition);
    ContentType := vContentType;
    ContentDisposition := vContentDisposition;
    { what was done, not what was asked - but a body coded already keeps the
      ContentEncoding it was written with }
    if Result = nil then
      ContentCompress := ctNone
    else if not FContentEncoded then
      ContentCompress := Params.CompressType;
  finally
    Params.CompressType := vCompress;
    Params.CriptoOptions.CriptType := vCripto;
    Params.CriptoOptions.Key := vKey;
  end;
end;

function TRALServerResponse.GetResponseEncText(
  const AEncode: boolean): StringRAL;
var
  vStream: TStream;
  vContentType, vContentDisposition: StringRAL;
begin
  if not AEncode then
  begin
    { a text answer is the handler's own string, not a copy of it }
    vContentType := '';
    vContentDisposition := '';
    vStream := Params.PlainBody(vContentType, vContentDisposition);
    try
      Result := RALStreamText(vStream);
    finally
      vStream.Free;
    end;
    Exit;
  end;

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
  if Assigned(Avalue) and (AValue.Size > 0) then
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

constructor TRALClientResponse.Create(AOwner : TObject);
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

procedure TRALClientResponse.Clear;
begin
  { a response is reused across the attempts of one call: the body assembled
    for the previous one must not answer for this one }
  FreeAndNil(FStream);
  inherited;
end;

procedure TRALClientResponse.SetWireBody(AStream: TStream;
  AOwnership: TRALBodyOwnership);
begin
  FreeAndNil(FStream);
  inherited;
end;

function TRALClientResponse.GetResponseEncStream(
  const AEncode: boolean): TStream;
var
  vContentType, vContentDisposition: StringRAL;
  vCompress: TRALCompressType;
  vCripto: TRALCriptoType;
begin
  { the body as it arrived, decoded once - no copy }
  Result := Params.Decoded;
  if Result <> nil then
  begin
    Result.Position := 0;
    Exit;
  end;

  { no body went through the decoder (params filled by hand): assembled from
    the params, once, and only when asked }
  if FStream = nil then
  begin
    vCompress := Params.CompressType;
    vCripto := Params.CriptoOptions.CriptType;
    Params.CompressType := ctNone;
    Params.CriptoOptions.CriptType := crNone;
    try
      FStream := Params.EncodeBody(vContentType, vContentDisposition);
    finally
      Params.CompressType := vCompress;
      Params.CriptoOptions.CriptType := vCripto;
    end;
  end;
  Result := FStream;
  if Result <> nil then
    Result.Position := 0;
end;

function TRALClientResponse.GetResponseEncText(
  const AEncode: boolean): StringRAL;
begin
  { a body an engine delivered as a string comes back as that string; any
    other is read once. It was copied into a TRALStringStream first, then
    read out of it: two copies to read a body as text }
  Result := RALStreamText(GetResponseEncStream(AEncode));
end;

procedure TRALClientResponse.SetResponseStream(const AValue: TStream);
begin
  { the old engine entry: decoded with whatever Params says, and AValue is
    copied (SetWireBody is the one that adopts) }
  FreeAndNil(FStream);
  if Assigned(AValue) and (AValue.size > 0) then
    Params.DecodeBody(AValue, ContentType, ContentDisposition);
end;

procedure TRALClientResponse.SetResponseText(const AValue: StringRAL);
begin
  FreeAndNil(FStream);
  Params.DecodeBody(AValue, ContentType, ContentDisposition);
end;

end.
