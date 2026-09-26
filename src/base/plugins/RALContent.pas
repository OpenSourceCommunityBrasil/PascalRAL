/// Plugins of the body of a request and of its response: size limit,
/// compression, encryption, and a JSON body as params
unit RALContent;

{ What used to be the server's MaxRequestSize, CompressType, CriptoOptions and
  JSONBodyToParams. A server without them answers as it is asked: no size limit,
  responses neither compressed nor encrypted, and a JSON body only in Body. A
  compressed or encrypted request body is still read by the engines - that is
  the protocol, not a choice of the server - but an encrypted one needs the key,
  which only TRALCriptoPlugin has. }

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALCompress, RALCripto, RALPlugin, RALRequest, RALResponse;

type
  { TRALLimitsPlugin }

  /// Refuses (413) a request body larger than MaxRequestSize, before it is
  /// decoded
  TRALLimitsPlugin = class(TRALPlugin)
  private
    FMaxRequestSize: Int64RAL;
    procedure SetMaxRequestSize(const AValue: Int64RAL);
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    /// Largest request body accepted, in bytes; zero means no limit. The check
    /// runs after the engine has read the body, so it protects the handlers and
    /// the decoders, not the engine's own buffer - msquic is the exception, it
    /// also refuses while receiving
    property MaxRequestSize: Int64RAL read FMaxRequestSize write SetMaxRequestSize
      default 0;
  end;

  { TRALCompressPlugin }

  /// Compresses the responses, and refuses the encodings the server cannot
  /// read (415) or answer (406)
  TRALCompressPlugin = class(TRALPlugin)
  private
    FCompressType: TRALCompressType;
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    /// ppProcess: how the response will be compressed
    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
    /// ppValidate: 415 for a body in an encoding the server cannot read, 406
    /// for a client that refuses every encoding the server has
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    /// A fixed compression for every response, which the client cannot opt out
    /// of. ctNone (the default) follows the client's Accept-Encoding, limited
    /// to what is registered
    property CompressType: TRALCompressType read FCompressType write FCompressType
      default ctNone;
  end;

  { TRALCriptoPlugin }

  /// Reads encrypted request bodies and encrypts the responses of a client
  /// that asks for it (Accept-Encription), with Key
  TRALCriptoPlugin = class(TRALPlugin)
  private
    FKey: StringRAL;
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    /// ppProcess: the response is encrypted with the cipher the client accepts
    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
    /// ppValidate: hands the key to the request, before the engine decodes the
    /// body with it
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    /// The key shared with the clients. Empty, nothing is encrypted
    property Key: StringRAL read FKey write FKey;
  end;

  { TRALJSONBodyPlugin }

  /// A request body that is a JSON object also becomes params: each member of
  /// the first level is an rpkFIELD param, the same as a form field, so
  /// ParamByName('field') reads a field posted as JSON too (a nested object or
  /// array arrives as its JSON text). The body stays in Body. A name also
  /// present in the query string keeps the query's value first, as it does
  /// for a form
  TRALJSONBodyPlugin = class(TRALPlugin)
  protected
    class function DefaultPriority: IntegerRAL; override;
    function Phases: TRALPluginPhases; override;
  public
    procedure ProcessRequest(ARequest: TRALRequest; AResponse: TRALResponse;
      var AHandled: boolean); override;
  end;

implementation

uses
  RALJson;

{ TRALLimitsPlugin }

class function TRALLimitsPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityLimits;
end;

function TRALLimitsPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate];
end;

procedure TRALLimitsPlugin.SetMaxRequestSize(const AValue: Int64RAL);
begin
  if AValue < 0 then
    FMaxRequestSize := 0
  else
    FMaxRequestSize := AValue;
end;

procedure TRALLimitsPlugin.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  { on the raw size: the engines only decode the body (decompress, decrypt,
    split the multipart) when this leaves the status below 400 }
  if (FMaxRequestSize > 0) and (ARequest.ContentSize > FMaxRequestSize) then
    AResponse.Answer(HTTP_RequestEntityTooLarge);
end;

{ TRALCompressPlugin }

class function TRALCompressPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityCompress;
end;

function TRALCompressPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate, ppProcess];
end;

procedure TRALCompressPlugin.ProcessRequest(ARequest: TRALRequest;
  AResponse: TRALResponse; var AHandled: boolean);
begin
  { a fixed CompressType always wins: it is an explicit choice by whoever set up
    the server. With none, the server follows the client, limited to what is
    registered - AcceptCompress only returns a type whose class is in
    CompressDefs, and ctNone when nothing matches }
  if FCompressType <> ctNone then
    AResponse.ContentCompress := FCompressType
  else
    AResponse.ContentCompress := ARequest.AcceptCompress;
end;

procedure TRALCompressPlugin.ValidateRequest(ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  if not ARequest.HasValidContentEncoding then
  begin
    AResponse.Answer(HTTP_UnsupportedMedia);
    AResponse.ContentEncoding := ARequest.ContentEncoding;
    AResponse.AcceptEncoding := GetAcceptCompress;
  end
  else if not ARequest.HasValidAcceptEncoding then
  begin
    { 406, not 415: the problem is what the client ACCEPTS, not the body it
      sent - and it only happens when it refuses identity on purpose }
    AResponse.Answer(HTTP_NotAcceptable);
    AResponse.ContentEncoding := ARequest.AcceptEncoding;
    AResponse.AcceptEncoding := GetAcceptCompress;
  end;
end;

{ TRALCriptoPlugin }

class function TRALCriptoPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityCripto;
end;

function TRALCriptoPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate, ppProcess];
end;

procedure TRALCriptoPlugin.ProcessRequest(ARequest: TRALRequest;
  AResponse: TRALResponse; var AHandled: boolean);
begin
  if FKey = '' then
    Exit;
  AResponse.ContentCripto := ARequest.AcceptCripto;
  AResponse.CriptoKey := FKey;
end;

procedure TRALCriptoPlugin.ValidateRequest(ARequest: TRALRequest;
  AResponse: TRALResponse);
begin
  { the engines decode the body right after ValidateRequest, with the cipher
    the request names (Content-Encription) and this key: without it the body
    reached the handler still as ciphertext }
  ARequest.Params.CriptoOptions.Key := FKey;
end;

{ TRALJSONBodyPlugin }

class function TRALJSONBodyPlugin.DefaultPriority: IntegerRAL;
begin
  Result := RALPriorityJSONBody;
end;

function TRALJSONBodyPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppProcess];
end;

procedure TRALJSONBodyPlugin.ProcessRequest(ARequest: TRALRequest;
  AResponse: TRALResponse; var AHandled: boolean);
var
  vText, vName: StringRAL;
  vValue, vMember: TRALJSONValue;
  vObject: TRALJSONObject;
  vInt: IntegerRAL;
begin
  { a body that is not an object, or not JSON at all, is left alone: the route
    still has it in Body, and answering 400 is its call }
  if Pos(StringRAL('json'), LowerCase(ARequest.ContentType)) = 0 then
    Exit;
  vText := Trim(ARequest.Body.AsString);
  if Copy(vText, 1, 1) <> '{' then
    Exit;

  vValue := nil;
  try
    try
      vValue := TRALJSON.ParseJSON(vText);
    except
      Exit;
    end;
    if not (vValue is TRALJSONObject) then
      Exit;
    vObject := TRALJSONObject(vValue);
    for vInt := 0 to Pred(vObject.Count) do
    begin
      vName := vObject.GetName(vInt);
      vMember := vObject.Get(vInt);
      if (vName = '') or vMember.IsNull then
        Continue;
      { a nested object or array goes as its JSON text. Not through AsString:
        the FPC backend hands that to fpjson, which raises for an object - the
        request died with 500 there, while Delphi's backend answered the JSON }
      if vMember.JsonType in [rjtObject, rjtArray] then
        ARequest.Params.AddParam(vName, vMember.ToJSON, rpkFIELD)
      else
        ARequest.Params.AddParam(vName, vMember.AsString, rpkFIELD);
    end;
  finally
    FreeAndNil(vValue);
  end;
end;

end.
