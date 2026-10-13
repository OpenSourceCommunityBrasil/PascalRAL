/// The response of a server or the answer a client receives.
unit RALResponse;

interface

uses
  Classes, SysUtils,
  RALTypes, RALParams, RALMIMETypes, RALCustomObjects, RALStream, RALConsts,
  RALCompress, RALTools;

type
  /// A response: status, headers, cookies and body.
  TRALResponse = class(TRALHTTPHeaderInfo)
  private
    /// Stream handed out by BodyStream while it is the body.
    FBodyStream: TStream;
    FContentEncoded: boolean;
    FErrorCode: IntegerRAL;
    FStatusCode: IntegerRAL;
    FTransportError: TRALTransportError;
  protected
    function GetResponseStream: TStream;
    function GetResponseText: StringRAL;
    procedure SetResponseStream(const AValue: TStream); virtual; abstract;
    procedure SetResponseText(const AValue: StringRAL); virtual; abstract;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    /// Adds AText as the body, of AContextType; returns the response.
    function AddBody(const AText: StringRAL; const AContextType: StringRAL = rctTEXTPLAIN): TRALResponse; reintroduce;
    { Adds a name=value cookie; returns the response.
      @deprecated Use AddCookie(ACookie: TRALCookie). }
    function AddCookie(const AName: StringRAL; const AValue: StringRAL): TRALResponse; reintroduce; overload; deprecated 'use AddCookie(ACookie: TRALCookie) instead';
    /// Adds ACookie, with its attributes; returns the response.
    function AddCookie(const ACookie: TRALCookie): TRALResponse; reintroduce; overload;
    /// Adds a form field; returns the response.
    function AddField(const AName: StringRAL; const AValue: StringRAL): TRALResponse; reintroduce;
    /// Adds the file AFileName to the body; returns the response.
    function AddFile(const AFileName: StringRAL): TRALResponse; reintroduce; overload;
    /// Adds AStream to the body as a file named AFileName; returns the response.
    function AddFile(AStream: TStream; const AFileName: StringRAL = ''): TRALResponse; reintroduce; overload;
    /// Adds a header; returns the response.
    function AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALResponse; reintroduce;
    /// Answers AStatusCode with the text AMessage, of AContentType.
    procedure Answer(AStatusCode: IntegerRAL; const AMessage: StringRAL;
                     const AContentType: StringRAL = rctAPPLICATIONJSON); overload;
    /// Answers AStatusCode with a copy of AStream; the caller keeps AStream.
    procedure Answer(AStatusCode: IntegerRAL; const AStream: TStream;
                     const AContentType: StringRAL = rctAPPLICATIONJSON); overload;
    { Answers AStatusCode with AStream; with AOwnsStream the response takes it,
      sends it as it is and frees it, otherwise it is copied. }
    procedure Answer(AStatusCode: IntegerRAL; AStream: TStream;
                     const AContentType: StringRAL; AOwnsStream: boolean); overload;
    /// Answers AStatusCode, with the server's page for it when there is one.
    procedure Answer(AStatusCode: IntegerRAL); overload;
    /// Answers with the file AFileName, inline unless DispositionInline is False.
    procedure Answer(const AFileName: StringRAL; const DispositionInline: boolean = true); overload;
    { A stream to write the body into, owned by the response and sent as it is;
      asking again returns the same stream while it is still the body. }
    function BodyStream: TStream;
    /// Empties the response and restores its defaults.
    procedure Clear; override;
    /// Adds the Set-Cookie values to ADest; ADateTime expires the plain cookies.
    procedure GetParamsCookies(ADest: TStringList; ADateTime: TDateTime);
    /// The Set-Cookie header lines of the cookies, each starting with AHeader.
    function GetParamsCookiesText(ADateTime: TDateTime; AHeader: StringRAL = 'Set-Cookie: ') : StringRAL;
    /// The body, encoded for the wire when AEncode, as a stream (see the descendants).
    function GetResponseEncStream(const AEncode: boolean = true): TStream; virtual; abstract;
    /// The body as text, encoded for the wire when AEncode.
    function GetResponseEncText(const AEncode: boolean = true): StringRAL; virtual; abstract;
    function TakeWireStream: TStream; override;
    function TakeWireString: RawByteString; override;

    /// The body is already coded as ContentEncoding says, and is not compressed again.
    property ContentEncoded: boolean read FContentEncoded write FContentEncoded;
    /// The body, decoded: on a server a copy the caller frees, on a client the one kept.
    property ResponseStream: TStream read GetResponseStream write SetResponseStream;
    /// The body, decoded, as text.
    property ResponseText: StringRAL read GetResponseText write SetResponseText;
  published
    /// Error of the transport on a client, 0 when an HTTP response arrived.
    property ErrorCode: IntegerRAL read FErrorCode write FErrorCode;
    /// HTTP status code.
    property StatusCode: IntegerRAL read FStatusCode write FStatusCode;
    /// How the send attempt ended for the transport; rteNone when a response arrived.
    property TransportError: TRALTransportError read FTransportError
                                                write FTransportError;
  end;

  /// Response a server sends.
  TRALServerResponse = class(TRALResponse)
  protected
    procedure SetResponseStream(const AValue: TStream); override;
    procedure SetResponseText(const AValue: StringRAL); override;
  public
    /// The handler's body: encoded for the wire when AEncode, a plain copy otherwise.
    function GetResponseEncStream(const AEncode: boolean = true): TStream; override;
    function GetResponseEncText(const AEncode: boolean = true): StringRAL; override;
  end;

  /// Response a client receives.
  TRALClientResponse = class(TRALResponse)
  private
    /// Body assembled from the params when none went through the decoder.
    FStream: TStream;
  protected
    procedure SetResponseStream(const AValue: TStream); override;
    procedure SetResponseText(const AValue: StringRAL); override;
  public
    constructor Create(AOwner : TObject); override;
    destructor Destroy; override;

    procedure Clear; override;
    /// The body that arrived, decoded and owned by the response; AEncode is ignored.
    function GetResponseEncStream(const AEncode: boolean = true): TStream; override;
    function GetResponseEncText(const AEncode: boolean = true): StringRAL; override;
    procedure SetWireBody(AStream: TStream; AOwnership: TRALBodyOwnership); override;
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
  // the same stream while it is still the body: several writes append
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
  // BodyStream may have been asked before the handler set the content type
  vParam := Body;
  if (FBodyStream <> nil) and (vParam <> nil) and (not vParam.IsText) and
     (vParam.Content = FBodyStream) then
    vParam.ContentType := ContentType;
  FBodyStream := nil;
  { coded already: nothing compresses it again, and ContentEncoding goes out as
    written, even for a coding this program cannot produce }
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
  { A plain name=value cookie expires at ADateTime and carries Path=/, so the
    browser sends it to every route. }
  vAttrs := '; Expires=' + RALHTTPDate(RALDateTimeToGMT(ADateTime)) + '; Path=/';

  for vInt := 0 to Pred(Params.Count) do
  begin
    vParam := TRALParam(Params.Index[vInt]);
    if (vParam <> nil) and (vParam.Kind = rpkCOOKIE) then
    begin
      { A Set-Cookie param holds a whole cookie line and goes out as it is; every
        engine builds its cookies from here, so CR and LF are taken out here. }
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
  // decoded on both sides: on a server, what the handler answered
  Result := GetResponseEncStream(False);
end;

function TRALResponse.GetResponseText: StringRAL;
begin
  // the body as text, never what goes on the wire (that is GetResponseEncText)
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
    // what the handler answered, as the caller's own copy
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

  // encoded, for an engine without TakeWireStream; the params are restored after
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
    // what was done, not what was asked; a body coded already keeps its ContentEncoding
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
  // a response is reused across the attempts of one call
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
  // the body as it arrived, decoded once, with no copy
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
  // a body delivered as a string comes back as that string
  Result := RALStreamText(GetResponseEncStream(AEncode));
end;

procedure TRALClientResponse.SetResponseStream(const AValue: TStream);
begin
  // decoded with what Params says; AValue is copied (SetWireBody adopts)
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
