/// Params of a request or response: values, headers, cookies, the body and its encoding.
unit RALParams;

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  RALHashes,
  RALTypes, RALMIMETypes, RALMultipartCoder, RALTools, RALUrlCoder,
  RALCripto, RALCriptoAES, RALStream, RALCompress, RALConsts;

type
  /// SameSite of a cookie; cssDefault writes none and leaves the browser's own rule.
  TRALCookieSiteScope = (cssDefault, cssLax, cssNone, cssStrict);

  /// A cookie and its attributes.
  TRALCookie = record
    /// Name of the cookie.
    Name: StringRAL;
    /// Value of the cookie.
    Value: StringRAL;
    /// Domain attribute; empty sends none.
    Domain: StringRAL;
    /// Path attribute; empty sends none.
    Path: StringRAL;
    /// Expiration, in local time; 0 sends none.
    Expires: TDateTime;
    /// Seconds until it expires (Max-Age); 0 is not set, below 0 deletes the cookie.
    MaxAge: Int64;
    /// Hides the cookie from scripts.
    HttpOnly: Boolean;
    /// Cookie of the browser session: Expires is not written.
    SessionOnly: Boolean;
    /// Sent over HTTPS only.
    Secure: Boolean;
    /// SameSite attribute.
    SameSite: TRALCookieSiteScope;
  end;

  TRALParams = class;

  { One param of a request or response: a name, a kind, and a value kept as text or
    as a stream. }
  TRALParam = class
  private
    /// The value when it is a stream; nil while it is text.
    FContent: TStream;
    /// Not used: ContentDisposition is built from FileName.
    FContentDisposition: StringRAL;
    FContentDispositionInline: Boolean;
    FContentType: StringRAL;
    /// Made from a pending header block and not in the list yet.
    FDetached: Boolean;
    FFileName: StringRAL;
    /// Hash of the name, for the index of the owning list.
    FHash: Cardinal;
    /// Whether the param is in the name index of its list.
    FIndexed: Boolean;
    FIsText: Boolean;
    FKind: TRALParamKind;
    /// Next detached param of the owning list.
    FNextDetached: TRALParam;
    /// Next param of the same bucket of the name index, in creation order.
    FNextSame: TRALParam;
    /// List the param belongs to, or nil.
    FOwner: TRALParams;
    FOwnsContent: Boolean;
    FParamName: StringRAL;
    /// Creation order of the param in its list.
    FSeq: Cardinal;
    /// The value while it is text.
    FText: StringRAL;

    procedure SetParamName(const AValue: StringRAL);
  protected
    /// The value as text, wherever it is kept.
    function ContentText: StringRAL;
    /// Drops the value, freeing it when the param owns it.
    procedure FreeContent;
    function GetAsBoolean: Boolean;
    function GetAsDouble: DoubleRAL;
    function GetAsInt64: Int64;
    function GetAsInteger: IntegerRAL;
    function GetAsStream: TStream;
      {$IFDEF FPC}deprecated 'AsStream builds a copy the caller must free: read Content, or call SaveToStream';{$ENDIF}
    function GetAsString: StringRAL;
    function GetContent: TStream;
    function GetContentDisposition: StringRAL;
    function GetContentSize: Int64RAL;
    /// Reads a typed payload of AType into ABuffer; False when the type or size differ.
    function GetTypedValue(const AType: StringRAL; var ABuffer; ASize: Integer): Boolean;
    /// Reads the typed payload the param holds, of any type; False for a text value.
    function GetTypedVariant(out AValue: Variant): Boolean;
    /// ContentType without its parameters and the blanks around it.
    function MediaType: StringRAL;
    /// Moves a text value into a stream.
    procedure NeedStream;
    procedure SetAsBoolean(const AValue: Boolean);
    procedure SetAsDouble(const AValue: DoubleRAL);
    procedure SetAsInt64(const AValue: Int64);
    procedure SetAsInteger(const AValue: IntegerRAL);
    procedure SetAsStream(const AValue: TStream);
    procedure SetAsString(const AValue: StringRAL);
    procedure SetContentDisposition(AValue: StringRAL);
    /// Writes a little-endian payload and sets ContentType to AType.
    procedure SetTypedValue(const AType: StringRAL; const ABuffer; ASize: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    /// Takes AStream as the value without copying it; the param frees it.
    procedure AdoptStream(AStream: TStream);
    /// The value as Currency; 0 when it is not a number.
    function AsCurrency: Currency;
    /// The value as a date and time; 0 when it is not one.
    function AsDateTime: TDateTime; overload;
    /// The value as a date and time, text read with ACustomFormat; 0 when it is not one.
    function AsDateTime(ACustomFormat: TFormatSettings): TDateTime; overload;
    /// The value as a number, or ADefault when it is not one.
    function AsDoubleDef(const ADefault: DoubleRAL): DoubleRAL;
    /// Uses AStream as the value without owning it: read-only, freed by its lender.
    procedure BorrowStream(AStream: TStream);
    /// Copies the name, kind, file name, value and type of this param into ASource.
    procedure Clone(ASource: TRALParam);
    /// True when the param is nil or its value is empty.
    function IsNilOrEmpty: Boolean;
    /// True when the value is a typed binary payload (an rctRAL* content type).
    function IsTyped: Boolean;
    /// Makes the file AFileName the value, read when sent; empty if it does not exist.
    procedure OpenFile(const AFileName: StringRAL);
    /// Saves the value next to the executable, named after FileName or the param.
    procedure SaveToFile; overload;
    /// Saves the value to AFileName.
    procedure SaveToFile(const AFileName: StringRAL); overload;
    { Saves the value in AFolderName (the executable's folder when empty) as
      AFileName, or FileName; only the last path component of the name is used. }
    procedure SaveToFile(AFolderName, AFileName: StringRAL); overload;
    /// Returns a new stream with a copy of the value; the caller frees it.
    function SaveToStream: TStream; overload;
    /// Writes the value to AStream.
    procedure SaveToStream(AStream: TStream); overload;
    /// Sets the value as a typed boolean.
    procedure SetTypedBoolean(const AValue: Boolean);
    /// Sets the value as a typed Currency.
    procedure SetTypedCurrency(const AValue: Currency);
    /// Sets the value as a typed TDateTime (also for a TDate or a TTime).
    procedure SetTypedDateTime(const AValue: TDateTime);
    /// Sets the value as a typed Double.
    procedure SetTypedDouble(const AValue: DoubleRAL);
    /// Sets the value as a typed Int64.
    procedure SetTypedInt64(const AValue: Int64RAL);
    /// Sets the value as a typed Integer.
    procedure SetTypedInteger(const AValue: IntegerRAL);
    /// Size of the value, in bytes.
    function Size: Int64;
    { The value as a stream the caller owns: with AConsume the param's own stream
      moves out; without, a view valid while the param lives. }
    function TakeContent(AConsume: Boolean): TStream;
    /// The value as a number, and whether it was one; text takes either separator.
    function TryAsDouble(out AValue: DoubleRAL): Boolean;

    /// The value as a boolean: '1' or 'true', in any case, is True.
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    /// The value as a number; 0 when it is not one. Text takes either separator.
    property AsDouble: DoubleRAL read GetAsDouble write SetAsDouble;
    /// The value as a 64-bit integer; 0 when it is not one.
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    /// The value as an integer; 0 when it is not one.
    property AsInteger: IntegerRAL read GetAsInteger write SetAsInteger;
    { Writing copies the stream into the param; reading returns a new copy that
      the caller frees. @deprecated Read Content, or call SaveToStream. }
    property AsStream: TStream read GetAsStream write SetAsStream;
    /// The value as text; a typed value is written in an invariant format.
    property AsString: StringRAL read GetAsString write SetAsString;
    /// The value as a stream the param keeps; a text value is converted on first read.
    property Content: TStream read GetContent;
    /// Content-Disposition of the param: built from FileName, read for name and filename.
    property ContentDisposition: StringRAL read GetContentDisposition write SetContentDisposition;
    /// Content-Disposition is inline instead of attachment.
    property ContentDispositionInline: Boolean read FContentDispositionInline write FContentDispositionInline;
    /// Size of the value, in bytes.
    property ContentSize: Int64RAL read GetContentSize;
    /// Content type of the value.
    property ContentType: StringRAL read FContentType write FContentType;
    /// File name of the value, sent in Content-Disposition.
    property FileName: StringRAL read FFileName write FFileName;
    /// True while the value is kept as text, False when it is a stream.
    property IsText: Boolean read FIsText;
    /// Where the param travels.
    property Kind: TRALParamKind read FKind write FKind;
    /// False for a lent value (BorrowStream): read-only, freed by its lender.
    property OwnsContent: Boolean read FOwnsContent;
    /// Name of the param.
    property ParamName: StringRAL read FParamName write SetParamName;
  end;

  /// One line of a header block kept as text: where its name and value are.
  TRALPendingLine = record
    /// Index of the first character of the name.
    NameStart: IntegerRAL;
    /// Length of the name.
    NameLen: IntegerRAL;
    /// Index of the first character of the value.
    ValueStart: IntegerRAL;
    /// Length of the value.
    ValueLen: IntegerRAL;
    /// The TRALParam a lookup made from this line, or nil.
    Owner: TObject;
  end;

  /// List of params of a request or response, indexed by name.
  TRALParams = class
  public const
    /// Most lines a header block may have to be kept as text; a longer one is parsed.
    PendingLinesMax = 32;
  public type
    /// Enumerator of the params, for for..in loops.
    TEnumerator = class
    private
      /// List being enumerated.
      FArray: TRALParams;
      /// Index of the current param.
      FIndex: Integer;
    public
      /// Enumerator over AArray.
      constructor Create(const AArray: TRALParams);

      /// The current param.
      function GetCurrent: TRALParam; inline;
      /// Moves to the next param; False past the last one.
      function MoveNext: Boolean; inline;

      /// The current param.
      property Current: TRALParam read GetCurrent;
    end;
  private
    FBodyError: StringRAL;
    /// Buckets of the name index; each chains its params through FNextSame.
    FBuckets: array of TRALParam;
    /// Streams the body params are windows over, freed after them.
    FBuffers: TList;
    FCompressType: TRALCompressType;
    FContentDispositionInline: Boolean;
    /// Cipher settings; created when first asked for.
    FCriptoOptions: TRALCriptoOptions;
    /// The body decoded by DecodeBody.
    FDecoded: TStream;
    /// Whether FDecoded is the value of the lone body param.
    FDecodedIsParam: Boolean;
    /// Params made from the pending block and not in the list yet, chained.
    FDetached: TRALParam;
    /// Number of detached params.
    FDetachedCount: IntegerRAL;
    /// Creation order reserved for the header block being parsed.
    FFlushBase: Cardinal;
    /// True while the pending block is parsed.
    FFlushing: Boolean;
    /// Creation order of the line of the pending block being parsed.
    FFlushSeq: Cardinal;
    /// The params in creation order; read through FParams.
    FList: TList;
    /// Counter of the generated names (ral_param1, ral_param2...).
    FNextParam: IntegerRAL;
    /// Header block kept as text until something needs the list.
    FPending: StringRAL;
    /// Number of lines of the pending block.
    FPendingCount: IntegerRAL;
    /// Kind of the params of the pending block.
    FPendingKind: TRALParamKind;
    /// Lines of the pending block.
    FPendingLines: array[0..PendingLinesMax - 1] of TRALPendingLine;
    /// Creation order reserved for the first line of the pending block.
    FPendingSeq: Cardinal;
    /// Creation order of the next param.
    FSeqNext: Cardinal;
    FSkipCompressedTypes: Boolean;
    FSkipCompressTypes: TStrings;
    FSpoolAbove: Int64RAL;

    /// Decodes a name and a value already split, and stores them.
    procedure AppendParamPair(AName, AValue: StringRAL; AKind: TRALParamKind);
    /// Frees FBuffers and forgets FDecoded.
    procedure ClearBuffers;
    { Keeps a header block as text instead of parsing it, when it has at most
      PendingLinesMax lines, no Set-Cookie and no name already in the list. }
    function DeferBlock(const ASource, ANameSeparator: StringRAL;
      AKind: TRALParamKind): boolean;
    { Compression the body really gets: CompressType, or none for a form or a
      multipart on the request path, a type already compressed or a compressor
      not linked. Written back to CompressType. }
    function EffectiveCompress(const AContentType: StringRAL;
      ACompressMultipart: boolean): TRALCompressType;
    /// First param with no name, of AKind unless AAnyKind.
    function FindNameless(AKind: TRALParamKind; AAnyKind: Boolean): TRALParam;
    /// The param of AName and AKind, created when there is none.
    function FindOrNewParam(const AName: StringRAL; AKind: TRALParamKind): TRALParam;
    /// Parses the pending block into the list, each param at its reserved place.
    procedure FlushPending;
    function GetDecoded: TStream;
    function GetList: TList;
    /// Adds AParam to the name index, keeping its chain in creation order.
    procedure IndexAdd(AParam: TRALParam; AHash: Cardinal);
    /// Rebuilds the name index at a size for the current list.
    procedure IndexBuild;
    /// First param of AName (and of AKind, unless AAnyKind), pending block included.
    function IndexFind(const AName: StringRAL; AHash: Cardinal; AKind: TRALParamKind;
      AAnyKind: Boolean): TRALParam;
    /// Removes AParam from the name index.
    procedure IndexRemove(AParam: TRALParam);
    /// Cipher for CriptoOptions, or nil when there is none; the caller frees it.
    function NewCipher(AEncoding: boolean): TRALCriptoAES;
    /// Param of AName made from the pending block; nil if none comes before ABefore.
    function PendingMake(const AName: StringRAL; ABefore: Cardinal): TRALParam;
    { The plain body as a stream the caller owns, or nil: the lone body param, the
      form as text or a multipart. Sets AContentType and AContentDisposition. }
    function PrepareBody(var AContentType, AContentDisposition: StringRAL;
      ACompressMultipart, AConsume: boolean): TStream;
    /// Writes ASource into ADest, compressed and/or encrypted.
    procedure WriteTransformed(ASource, ADest: TStream; ACompress: TRALCompressType;
      var AContentType: StringRAL; ACompressMultipart: boolean);

    /// The list, with the pending block parsed into it first.
    property FParams: TList read GetList;
  protected
    /// Adds the name=value pair of a Set-Cookie header as an rpkCOOKIE param.
    procedure AddSetCookie(const AValue: StringRAL);
    /// Adds the param of one name/value line, URL-decoded unless it is a header.
    procedure AppendParamLine(const ALine: StringRAL; const ANameSeparator: StringRAL;
      AKind: TRALParamKind);
    /// AppendParamLine over ALen characters of ASource from AStart, with no copy.
    procedure AppendParamSpan(const ASource: StringRAL; AStart, ALen: IntegerRAL;
      const ANameSeparator: StringRAL; AKind: TRALParamKind);
    /// Compresses AStream with CompressType into a new stream; nil if not linked.
    function Compress(AStream: TStream): TStream;
    /// Decompresses ASource with CompressType.
    function Decompress(const ASource: StringRAL): StringRAL; overload;
    /// Decompresses AStream with CompressType into a new stream; nil if not linked.
    function Decompress(AStream: TStream): TStream; overload;
    /// Decrypts AStream with CriptoOptions into a new stream.
    function Decrypt(AStream: TStream): TStream; overload;
    /// Decrypts ASource, Base64 text, with CriptoOptions.
    function Decrypt(const ASource: StringRAL): StringRAL; overload;
    /// Encrypts AStream with CriptoOptions into a new stream.
    function Encrypt(AStream: TStream): TStream;
    /// '=' or ': ', whichever ASource uses as its name separator.
    function FindBodyNameSeparator(const ASource: StringRAL): StringRAL;
    /// ': ' or '=', whichever comes first in ASource.
    function FindHeaderNameSeparator(const ASource: StringRAL): StringRAL;
    function GetBody: TList;
      {$IFDEF FPC}deprecated 'Body builds a list the caller must free: read Count(rpkBODY), IndexKind or SingleBody';{$ENDIF}
    function GetCriptoOptions: TRALCriptoOptions;
    function GetParam(AIndex: IntegerRAL; AKind: TRALParamKind): TRALParam; overload;
    function GetParam(AIndex: IntegerRAL): TRALParam; overload;
    function GetParam(const AName: StringRAL): TRALParam; overload;
    function GetParam(const AName: StringRAL; AKind: TRALParamKind): TRALParam; overload;
    /// Advances the counter of generated names and returns it.
    function NextParamInt: IntegerRAL;
    /// Advances the counter and returns the next generated name (ral_paramN).
    function NextParamStr: StringRAL;
    procedure SetCriptoOptions(const AValue: TRALCriptoOptions);
    /// Adds each part the multipart decoder completes as an rpkBODY param.
    procedure OnFormBodyData(Sender: TObject; AFormData: TRALMultipartFormData;
      var AFreeData: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds or replaces the body param AParamName with the file AFileName.
    function AddFile(const AParamName: StringRAL; const AFileName: StringRAL): TRALParam; overload;
    /// Adds a body param with the file AFileName, under a generated name.
    function AddFile(const AFileName: StringRAL): TRALParam; overload;
    /// Adds a received header; a Set-Cookie also becomes an rpkCOOKIE param.
    procedure AddHeader(const AName, AValue: StringRAL);
    { Adds or replaces the param of AName and AKind with the text AValue; nothing
      when AName or AValue is empty. Another kind of the same name is another param. }
    function AddParam(const AName: StringRAL; const AValue: StringRAL;
                      AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    /// Adds or replaces the param of AName and AKind with a copy of AContent.
    function AddParam(const AName: StringRAL; AContent: TStream;
                      AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    /// Adds or replaces the param of AName and AKind with AValue, typed as AType.
    function AddParam(const AName: StringRAL; const AValue: Variant;
                      AKind: TRALParamKind; AType: TRALParamType): TRALParam; overload;
    /// Adds a param with a generated name and the text AContent; rpkNONE is not sent.
    function AddValue(const AContent: StringRAL; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    /// Adds a param with a generated name and a copy of AContent; rpkNONE is not sent.
    function AddValue(AContent: TStream; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    /// Adds the name=value lines of ASource (form fields), URL-decoded, as params.
    procedure AppendBodyParams(ASource: TStrings; AKind: TRALParamKind);
    /// Adds the name/value lines of ASource as params of AKind.
    procedure AppendParams(ASource: TStringList; AKind: TRALParamKind); overload;
    /// Adds the name/value lines of ASource as params of AKind.
    procedure AppendParams(ASource: TStrings; AKind: TRALParamKind); overload;
    /// Adds the lines of ASource (CR or LF separated) as params of AKind.
    procedure AppendParamsListText(ASource: StringRAL; AKind: TRALParamKind;
                                   ANameSeparator: StringRAL = '');
    /// Adds the name=value pairs of AText, split at ALineSeparator, URL-decoded.
    procedure AppendParamsText(AText: StringRAL; AKind: TRALParamKind;
                               const ANameSeparator: StringRAL = '=';
                               const ALineSeparator: StringRAL = '&');
    /// Adds the path segments of AFullURI past APartialURI as ral_uriparam1, 2...
    procedure AppendParamsUri(AFullURI, APartialURI: StringRAL; AKind: TRALParamKind);
    /// Adds the query string of AUrlQuery, after its '?', as params of AKind.
    procedure AppendParamsUrl(AUrlQuery: StringRAL; AKind: TRALParamKind);
    /// The params as a JSON object of names and text values; '' when there are none.
    function AsJSON: StringRAL;
    /// Adds the params of AKind to ADest as lines of name, ASeparator and value.
    procedure AssignParams(ADest: TStringList; AKind: TRALParamKind;
                           ASeparator: StringRAL = '='); overload;
    /// The same for TStrings; headers and cookies lose CR, LF and NUL.
    procedure AssignParams(ADest: TStrings; AKind: TRALParamKind;
                           ASeparator: StringRAL = '='); overload;
    /// The params of AKind as CRLF lines of name, ANameSeparator and value.
    function AssignParamsListText(AKind: TRALParamKind;
                                  const ANameSeparator: StringRAL = '='): StringRAL;
    { The params of AKind as text: name, ANameSeparator and value, joined by
      ALineSeparator and URL-encoded on request. Headers and cookies lose CR, LF, NUL. }
    function AssignParamsText(AKind: TRALParamKind; AUrlEncoded: boolean = False;
                              const ANameSeparator: StringRAL = '=';
                              const ALineSeparator: StringRAL = '&'): StringRAL;
    /// The params of AKind as a URL-encoded query string.
    function AssignParamsUrl(AKind: TRALParamKind): StringRAL;
    /// The values of all params, separated by ', '.
    function AsString: StringRAL;
    /// Frees every param.
    procedure ClearParams; overload;
    /// Frees the params of AKind.
    procedure ClearParams(AKind: TRALParamKind); overload;
    /// Value of a request's Cookie header, built from the rpkCOOKIE params.
    function CookieHeaderText: StringRAL; overload;
    { The same merged with the cookies of a jar (AJarCookies): the jar's first, then
      the params; a param wins over a jar cookie of the same name. }
    function CookieHeaderText(const AJarCookies: StringRAL): StringRAL; overload;
    /// Number of params.
    function Count: IntegerRAL; overload;
    /// Number of params of AKind.
    function Count(AKind: TRALParamKind): IntegerRAL; overload;
    /// Number of params of any of AKinds.
    function Count(AKinds: TRALParamKinds): IntegerRAL; overload;
    /// Decodes a copy of ASource into the params; returns nil (see Decoded).
    function DecodeBody(ASource: TStream; const AContentType: StringRAL;
                        const AContentDisposition: StringRAL = ''): TStream; overload;
    { Decrypts and decompresses ASource into the params: one body param, or a
      window per multipart part. AOwnership says whose ASource is. Returns nil. }
    function DecodeBody(ASource: TStream; const AContentType, AContentDisposition: StringRAL;
                        AOwnership: TRALBodyOwnership): TStream; overload;
    /// Decodes ASource, held as a view, into the params; returns nil (see Decoded).
    function DecodeBody(const ASource, AContentType: StringRAL;
                        const AContentDisposition: StringRAL = ''): TStream; overload;
    /// Adds the fields of an x-www-form-urlencoded text as params of AKind.
    procedure DecodeFields(const ASource: StringRAL; AKind: TRALParamKind = rpkFIELD);
    /// Frees every param named AName.
    procedure DelParam(const AName: StringRAL); overload;
    /// Frees the params named AName of AKind.
    procedure DelParam(const AName: StringRAL; AKind: TRALParamKind); overload;
    { The body params encoded for the wire, compressed and/or encrypted, as a new
      stream the caller frees; ACompressMultipart False sends forms uncompressed. }
    function EncodeBody(var AContentType, AContentDisposition: StringRAL;
      ACompressMultipart: boolean = True): TStream;
    /// Writes the encoded body into ADest; the params stay as they are.
    procedure EncodeInto(ADest: TStream; var AContentType, AContentDisposition: StringRAL;
      ACompressMultipart: boolean = True);
    /// Enumerator of the params, for for..in loops.
    function GetEnumerator: TEnumerator; inline;
    /// True when a cipher with a key is set; ATrimKey counts a key of blanks as none.
    function HasCipher(ATrimKey: boolean): boolean;
    /// True when CriptoOptions exists; reading CriptoOptions would create it.
    function HasCriptoOptions: boolean;
    /// Adds an empty param and returns it.
    function NewParam: TRALParam;
    /// The body, not compressed nor encrypted, as a view the caller frees; nil if none.
    function PlainBody(var AContentType, AContentDisposition: StringRAL): TStream;
    { Frees every param named AName, of any kind, and adds this one, even empty:
      how a server imposes a value over one the client sent. }
    function ReplaceParam(const AName: StringRAL; const AValue: StringRAL;
                          AKind: TRALParamKind = rpkNONE): TRALParam;
    /// The body param when it is the only one and there are no fields; nil otherwise.
    function SingleBody: TRALParam;
    { The body for the wire, as a stream the caller owns. AConsume moves the
      params' streams into it; without it, it is valid while the params live. }
    function TakeWireStream(var AContentType, AContentDisposition: StringRAL;
      ACompressMultipart, AConsume: boolean): TStream;
    /// TakeWireStream as a RawByteString; a lone text body is the param's string itself.
    function TakeWireString(var AContentType, AContentDisposition: StringRAL;
      ACompressMultipart, AConsume: boolean): RawByteString;
    /// Splits a URL-encoded text at each '&' into a new list the caller frees.
    function URLEncodedToList(ASource: StringRAL): TStringList;

    { A new list of the rpkBODY params, which the caller frees.
      @deprecated Read Count(rpkBODY), IndexKind or SingleBody. }
    property Body: TList read GetBody;
    /// Why the last DecodeBody could not split the body; '' when it could.
    property BodyError: StringRAL read FBodyError;
    /// The body received, decoded by DecodeBody; owned by the params.
    property Decoded: TStream read GetDecoded;
    /// First param named AName, of any kind, or nil.
    property Get[const AName: StringRAL]: TRALParam read GetParam;
    /// First param named AName of AKind, or nil.
    property GetKind[const AName: StringRAL; AKind: TRALParamKind]: TRALParam read GetParam;
    /// Param at AIndex, or nil.
    property Index[AIndex: IntegerRAL]: TRALParam read GetParam;
    /// The AIndex-th param of AKind, or nil.
    property IndexKind[AIndex: IntegerRAL; AKind: TRALParamKind]: TRALParam read GetParam;
    /// Encoding leaves already compressed types (images, archives...) uncompressed.
    property SkipCompressedTypes: Boolean read FSkipCompressedTypes write FSkipCompressedTypes;
    /// More content types encoding does not compress; referenced, not owned.
    property SkipCompressTypes: TStrings read FSkipCompressTypes write FSkipCompressTypes;
    /// Bodies above this size are spooled to a temporary file; 0 never.
    property SpoolAbove: Int64RAL read FSpoolAbove write FSpoolAbove;
  published
    /// Compression of the body.
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    /// A lone body param goes out with an inline Content-Disposition.
    property ContentDispositionInline: Boolean read FContentDispositionInline
      write FContentDispositionInline;
    /// Cipher of the body; created the first time it is read.
    property CriptoOptions: TRALCriptoOptions read GetCriptoOptions write SetCriptoOptions;
  end;

/// Set-Cookie text of ACookie, with its attributes.
function GetCookieText(ACookie: TRALCookie): StringRAL;
/// Parses a Set-Cookie text into a cookie record.
function GetRALCookieFromText(ACookieString: StringRAL): TRALCookie;
/// Cookie named AParamName in AParams, received or set; empty when absent.
function GetRALCookieFromParam(AParamName: StringRAL; AParams: TRALParams): TRALCookie;

implementation

{ TRALParam }

uses
  RALJson;

const
  /// Number of generated names kept ready (ral_param1 to ral_param32).
  cRALParamNames = 32;

var
  /// 'text/plain', handed over by reference instead of copying the literal.
  gTextPlain: StringRAL;
  /// 'application/octet-stream', shared.
  gOctetStream: StringRAL;
  /// 'ral_body', shared.
  gRalBody: StringRAL;
  /// 'inline', shared.
  gInline: StringRAL;
  /// ': ', shared.
  gColonSpace: StringRAL;
  /// '=', shared.
  gEquals: StringRAL;
  /// The generated names ral_param1 to ral_param32, shared.
  gParamNames: array[1..cRALParamNames] of StringRAL;

{ FNV-1a of AName with $20 set in every byte, so names RALSameName calls equal hash
  alike. The arithmetic wraps: overflow and range checks are off here. }
{$IFOPT Q+}{$DEFINE RALPARAMS_Q}{$Q-}{$ENDIF}
{$IFOPT R+}{$DEFINE RALPARAMS_R}{$R-}{$ENDIF}
function ParamNameHash(const AName: StringRAL): Cardinal;
var
  vByte: PByte;
  vInt: IntegerRAL;
begin
  Result := 2166136261;
  vByte := PByte(Pointer(AName));
  for vInt := 1 to Length(AName) do
  begin
    Result := (Result xor Cardinal(vByte^ or $20)) * Cardinal(16777619);
    Inc(vByte);
  end;
end;
{$IFDEF RALPARAMS_Q}{$Q+}{$UNDEF RALPARAMS_Q}{$ENDIF}
{$IFDEF RALPARAMS_R}{$R+}{$UNDEF RALPARAMS_R}{$ENDIF}

/// ADateTime, in local time, as an HTTP date in GMT.
function DateTimeToCookieExpireDate(ADateTime: TDateTime): StringRAL;
begin
  // RALHTTPDate: FormatDateTime would write the locale's time separator
  Result := RALHTTPDate(RALDateTimeToGMT(ADateTime));
end;

function GetCookieText(ACookie: TRALCookie): StringRAL;
begin
  Result := ACookie.Name + '=' + ACookie.Value;

  if ACookie.Domain <> '' then
    Result := Result + '; Domain=' + ACookie.Domain;

  if ACookie.Path <> '' then
    Result := Result + '; Path=' + ACookie.Path;

  if (not ACookie.SessionOnly) and (ACookie.Expires <> 0) then
    Result := Result + '; Expires=' + DateTimeToCookieExpireDate(ACookie.Expires);

  // Max-Age as given, or 0 to expire the cookie now
  if ACookie.MaxAge > 0 then
    Result := Result + '; Max-Age=' + IntToStr(ACookie.MaxAge)
  else if ACookie.MaxAge < 0 then
    Result := Result + '; Max-Age=0';

  if ACookie.Secure then
    Result := Result + '; Secure';

  if ACookie.HttpOnly then
    Result := Result + '; HttpOnly';

  case ACookie.SameSite of
    cssLax:
      Result := Result + '; SameSite=Lax';
    { a browser refuses SameSite=None without Secure }
    cssNone:
      if ACookie.Secure then
        Result := Result + '; SameSite=None';
    cssStrict:
      Result := Result + '; SameSite=Strict';
  end;
end;

function GetRALCookieFromText(ACookieString: StringRAL): TRALCookie;
var
  Start, P, EqPos, Len: Integer;
  S, Part, Name, Value: StringRAL;
  vDate: TDateTime;
begin
  // the record holds strings: finalized before zeroing, or they leak
  Finalize(Result);
  FillChar(Result, SizeOf(Result), 0);

  S := StringReplace(ACookieString, '; ', ';', [rfReplaceAll]);
  Len := Length(S);
  if Len = 0 then
    Exit;

  Start := 1;
  while Start <= Len do
  begin
    // up to the next ';'
    P := Start;
    while (P <= Len) and (S[P] <> ';') do
      Inc(P);

    Part := Copy(S, Start, P - Start);
    Start := P + 1;

    if Part = '' then
      Continue;

    EqPos := Pos('=', Part);
    if EqPos > 0 then
    begin
      Name  := Copy(Part, 1, EqPos - 1);
      Value := Copy(Part, EqPos + 1, MaxInt);
    end
    else
    begin
      Name  := Part;
      Value := '';
    end;

    // attribute names are compared without case
    if RALSameName(Name, 'HttpOnly') then
      Result.HttpOnly := True
    else if RALSameName(Name, 'Secure') then
      Result.Secure := True
    else if RALSameName(Name, 'Path') then
      Result.Path := Value
    else if RALSameName(Name, 'Domain') then
      Result.Domain := Value
    else if RALSameName(Name, 'SameSite') then
    begin
      if RALSameName(Value, 'None') then
        Result.SameSite := cssNone
      else if RALSameName(Value, 'Lax') then
        Result.SameSite := cssLax
      else if RALSameName(Value, 'Strict') then
        Result.SameSite := cssStrict;
    end
    else if RALSameName(Name, 'Expires') then
    begin
      { A date that does not parse is ignored (RFC 6265 5.2.1). The text is GMT
        and Expires is local time, as GetCookieText writes it. }
      if RALTryHTTPDate(Value, vDate) then
        Result.Expires := RALGMTToDateTime(vDate)
      else
        Result.Expires := 0;
    end
    else if RALSameName(Name, 'Max-Age') then
    begin
      // 0 in the record means "not set": an expiring Max-Age comes back as -1
      if not TryStrToInt64(Value, Result.MaxAge) then
        Result.MaxAge := 0
      else if Result.MaxAge <= 0 then
        Result.MaxAge := -1;
    end
    else
    begin
      // the name=value that is no attribute is the cookie itself
      Result.Name  := Name;
      Result.Value := Value;
    end;
  end;
end;

/// The name=value pair of a cookie line: what comes before the first ';'.
function CookiePairOf(const ALine: StringRAL): StringRAL;
var
  vPos: IntegerRAL;
begin
  vPos := Pos(StringRAL(';'), ALine);
  if vPos > 0 then
    Result := RALTrim(Copy(ALine, 1, vPos - 1))
  else
    Result := RALTrim(ALine);
end;

{ A cookie that arrived is a param named after it, holding its value; one set
  with AddCookie(TRALCookie) is a Set-Cookie param holding the whole line. }
function GetRALCookieFromParam(AParamName: StringRAL; AParams: TRALParams
  ): TRALCookie;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
  vPrefix: StringRAL;
begin
  Finalize(Result);
  FillChar(Result, SizeOf(Result), 0);
  if (AParams = nil) or (AParamName = '') then
    Exit;

  vParam := AParams.GetKind[AParamName, rpkCOOKIE];
  if (vParam <> nil) and (not RALSameName(vParam.ParamName, 'Set-Cookie')) then
  begin
    Result.Name := vParam.ParamName;
    Result.Value := vParam.AsString;
    Exit;
  end;

  vPrefix := AParamName + '=';
  for vInt := 0 to Pred(AParams.Count) do
  begin
    vParam := AParams.Index[vInt];
    if (vParam.Kind = rpkCOOKIE) and RALSameName(vParam.ParamName, 'Set-Cookie') and
       (Pos(vPrefix, vParam.AsString) = 1) then
    begin
      Result := GetRALCookieFromText(vParam.AsString);
      Exit;
    end;
  end;
end;

procedure TRALParam.Clone(ASource: TRALParam);
begin
  ASource.ContentDispositionInline := Self.ContentDispositionInline;
  ASource.FileName := Self.FileName;
  ASource.Kind := Self.Kind;
  ASource.ParamName := Self.ParamName;

  // content first: writing it drops a typed marker, so the type goes after
  if FIsText then
    ASource.AsString := FText
  else
    ASource.AsStream := FContent;
  ASource.ContentType := Self.ContentType;
end;

procedure TRALParam.SetParamName(const AValue: StringRAL);
begin
  // the owning list indexes by name; a param enters the index once it has one
  if FIndexed then
    FOwner.IndexRemove(Self);
  FParamName := AValue;
  if (FOwner <> nil) and (AValue <> '') then
    FOwner.IndexAdd(Self, ParamNameHash(AValue));
end;

constructor TRALParam.Create;
begin
  inherited;
  FContent := nil;
  FOwnsContent := True;
  FText := '';
  FIsText := False;
  FContentType := gTextPlain;
  FKind := rpkNONE;
end;

destructor TRALParam.Destroy;
begin
  FreeContent;
  inherited;
end;

procedure TRALParam.FreeContent;
begin
  if FOwnsContent then
    FreeAndNil(FContent)
  else
    FContent := nil;
  FOwnsContent := True;
end;

procedure TRALParam.BorrowStream(AStream: TStream);
begin
  FreeContent;
  FText := '';
  FIsText := False;
  FContent := AStream;
  FOwnsContent := False;
  if FContent <> nil then
    FContent.Position := 0;

  // same rule as SetAsStream: new content, no stale typed marker
  if IsTyped then
    FContentType := gOctetStream;
end;

function TRALParam.TakeContent(AConsume: Boolean): TStream;
var
  vBody: TRALBodyStream;
begin
  if FIsText then
    Result := TRALStringView.Create(FText)
  else if FContent = nil then
    Result := TMemoryStream.Create
  else if AConsume and FOwnsContent then
  begin
    Result := FContent;
    // a file goes as it is; the param keeps an unopened twin for later readers
    if Result is TRALFileStream then
      FContent := TRALFileStream(Result).Twin
    else
      FContent := nil;
  end
  else if AConsume then
  begin
    // lent by someone who may not outlive the caller: copied
    vBody := TRALBodyStream.Create(FContent.Size);
    try
      FContent.Position := 0;
      RALCopyStream(FContent, vBody, FContent.Size);
      Result := vBody.Detach;
    finally
      vBody.Free;
    end;
  end
  else if FContent is TRALFileStream then
    Result := TRALFileStream(FContent).Twin
  else
    Result := RALStreamSlice(FContent, 0, FContent.Size);
  Result.Position := 0;
end;

function TRALParam.AsDateTime: TDateTime;
var
  vVar: Variant;
begin
  Result := 0;
  if Self = nil then
    Exit;

  if GetTypedVariant(vVar) then
    Result := vVar
  else
    Result := StrToDateTimeDef(ContentText, 0);
end;

function TRALParam.AsDateTime(ACustomFormat: TFormatSettings): TDateTime;
var
  vVar: Variant;
begin
  Result := 0;
  if Self = nil then
    Exit;

  // the format applies to text only, never to a typed payload
  if GetTypedVariant(vVar) then
    Result := vVar
  else
    Result := StrToDateTimeDef(ContentText, 0, ACustomFormat);
end;


/// Swaps ABuffer between memory and the little-endian wire on big-endian targets.
procedure RALSwapBytes(var ABuffer; ASize: Integer);
{$IF Defined(FPC) and Defined(ENDIAN_BIG)}
var
  vBytes: PByte;
  vInt, vLast: Integer;
  vTmp: Byte;
begin
  vBytes := @ABuffer;
  vLast := ASize - 1;
  for vInt := 0 to (ASize div 2) - 1 do
  begin
    vTmp := vBytes[vInt];
    vBytes[vInt] := vBytes[vLast - vInt];
    vBytes[vLast - vInt] := vTmp;
  end;
end;
{$ELSE}
begin
  // little-endian: the wire format matches memory
end;
{$IFEND}

function TRALParam.IsTyped: Boolean;
var
  vType: StringRAL;
begin
  Result := False;
  if Self = nil then
    Exit;

  // MediaType once, compared byte by byte: this runs for every value received
  vType := MediaType;
  Result := RALSameName(vType, rctRALINT32) or
            RALSameName(vType, rctRALINT64) or
            RALSameName(vType, rctRALDOUBLE) or
            RALSameName(vType, rctRALCURRENCY) or
            RALSameName(vType, rctRALBOOLEAN) or
            RALSameName(vType, rctRALDATETIME);
end;

procedure TRALParam.SetTypedValue(const AType: StringRAL; const ABuffer;
  ASize: Integer);
var
  vBuf: TBytes;
begin
  SetLength(vBuf, ASize);
  Move(ABuffer, vBuf[0], ASize);
  RALSwapBytes(vBuf[0], ASize);

  if FContent <> nil then
    FreeAndNil(FContent);

  FText := '';
  FIsText := False;
  FContent := TMemoryStream.Create;
  FContent.WriteBuffer(vBuf[0], ASize);
  FContent.Position := 0;

  FContentType := AType;
end;

function TRALParam.MediaType: StringRAL;
var
  vIni, vFim: IntegerRAL;
begin
  Result := '';
  if Self = nil then
    Exit;

  // up to ';', without the blanks RFC 9110 allows around it; offsets count from 0
  vFim := Pos(StringRAL(';'), FContentType) - 1;
  if vFim < 0 then
    vFim := Length(FContentType);
  vIni := 0;
  while (vIni < vFim) and (FContentType[POSINISTR + vIni] in [' ', #9]) do
    Inc(vIni);
  while (vFim > vIni) and (FContentType[POSINISTR + vFim - 1] in [' ', #9]) do
    Dec(vFim);
  if (vIni = 0) and (vFim = Length(FContentType)) then
    Result := FContentType
  else
    Result := Copy(FContentType, POSINISTR + vIni, vFim - vIni);
end;

function TRALParam.GetTypedValue(const AType: StringRAL; var ABuffer;
  ASize: Integer): Boolean;
begin
  { The size first, the cheap test, then the type; a payload of the wrong size
    reads as text. }
  Result := (Self <> nil) and (FContent <> nil) and (FContent.Size = ASize) and
            RALSameName(MediaType, AType);

  if not Result then
    Exit;

  FContent.Position := 0;
  FContent.ReadBuffer(ABuffer, ASize);
  RALSwapBytes(ABuffer, ASize);
end;

function TRALParam.GetTypedVariant(out AValue: Variant): Boolean;
var
  vInt32: IntegerRAL;
  vInt64: Int64RAL;
  vDouble: DoubleRAL;
  vCur: Currency;
  vByte: Byte;
begin
  Result := True;

  if GetTypedValue(rctRALINT32, vInt32, SizeOf(vInt32)) then
    AValue := vInt32
  else if GetTypedValue(rctRALINT64, vInt64, SizeOf(vInt64)) then
    AValue := vInt64
  else if GetTypedValue(rctRALDOUBLE, vDouble, SizeOf(vDouble)) then
    AValue := vDouble
  else if GetTypedValue(rctRALCURRENCY, vCur, SizeOf(vCur)) then
    AValue := vCur
  else if GetTypedValue(rctRALDATETIME, vDouble, SizeOf(vDouble)) then
    AValue := vDouble
  else if GetTypedValue(rctRALBOOLEAN, vByte, SizeOf(vByte)) then
    AValue := vByte <> 0
  else
  begin
    AValue := Null;
    Result := False;
  end;
end;

procedure TRALParam.SetTypedInteger(const AValue: IntegerRAL);
begin
  SetTypedValue(rctRALINT32, AValue, SizeOf(AValue));
end;

procedure TRALParam.SetTypedInt64(const AValue: Int64RAL);
begin
  SetTypedValue(rctRALINT64, AValue, SizeOf(AValue));
end;

procedure TRALParam.SetTypedDouble(const AValue: DoubleRAL);
begin
  SetTypedValue(rctRALDOUBLE, AValue, SizeOf(AValue));
end;

procedure TRALParam.SetTypedCurrency(const AValue: Currency);
begin
  // Currency is a scaled Int64: its 8 bytes round-trip exactly
  SetTypedValue(rctRALCURRENCY, AValue, SizeOf(AValue));
end;

procedure TRALParam.SetTypedBoolean(const AValue: Boolean);
var
  vByte: Byte;
begin
  if AValue then
    vByte := 1
  else
    vByte := 0;

  SetTypedValue(rctRALBOOLEAN, vByte, SizeOf(vByte));
end;

procedure TRALParam.SetTypedDateTime(const AValue: TDateTime);
var
  vDouble: Double;
begin
  // TDateTime is a Double, sent raw: no date format involved
  vDouble := AValue;
  SetTypedValue(rctRALDATETIME, vDouble, SizeOf(vDouble));
end;

function TRALParam.AsCurrency: Currency;
var
  vVar: Variant;
begin
  Result := 0;
  if Self = nil then
    Exit;

  if GetTypedVariant(vVar) then
    Result := vVar
  else
    RALTryStrToCurr(ContentText, Result);
end;
function TRALParam.IsNilOrEmpty: Boolean;
begin
  Result := (Self = nil) or ((Self <> nil) and (Self.Size = 0));
end;

function TRALParam.Size: Int64;
begin
  if FIsText then
    Result := Length(FText)
  else if FContent <> nil then
    Result := FContent.Size
  else
    Result := 0;
end;

procedure TRALParam.OpenFile(const AFileName: StringRAL);
begin
  FreeContent;

  FText := '';
  FIsText := False;
  if FileExists(AFileName) then
  begin
    { Opened now, so an unreadable file raises here, shared with writers, and
      sent from disk as it is. }
    FContent := TRALFileStream.Create(string(AFileName), 0, -1, True);
  end
  else
  begin
    FContent := TMemoryStream.Create;
  end;

  // new content drops a typed marker, or a file of the right size reads as a number
  if IsTyped then
    FContentType := gOctetStream;
end;

function TRALParam.GetAsInt64: Int64;
var
  vVar: Variant;
begin
  Result := 0;
  if Self = nil then
    Exit;

  if GetTypedVariant(vVar) then
    Result := vVar
  else
    Result := StrToInt64Def(ContentText, 0);
end;

procedure TRALParam.SetAsInt64(const AValue: Int64);
begin
  SetAsString(IntToStr(AValue));
end;

function TRALParam.GetAsBoolean: Boolean;
var
  vStr: StringRAL;
  vVar: Variant;
begin
  Result := False;
  if Self = nil then
    Exit;

  if GetTypedVariant(vVar) then
    Result := vVar
  else
  begin
    vStr := ContentText;
    Result := (vStr = '1') or (SameText(vStr, 'true'));
  end;
end;

function TRALParam.GetAsDouble: DoubleRAL;
var
  vVar: Variant;
begin
  Result := 0;
  if Self = nil then
    Exit;

  // a typed payload converts; text is parsed
  if GetTypedVariant(vVar) then
    Result := vVar
  else
    RALTryStrToFloat(ContentText, Result);
end;

function TRALParam.TryAsDouble(out AValue: DoubleRAL): Boolean;
var
  vVar: Variant;
begin
  AValue := 0;
  Result := False;
  if Self = nil then
    Exit;

  if GetTypedVariant(vVar) then
  begin
    AValue := vVar;
    Result := True;
  end
  else
    Result := RALTryStrToFloat(ContentText, AValue);
end;

function TRALParam.AsDoubleDef(const ADefault: DoubleRAL): DoubleRAL;
begin
  if not TryAsDouble(Result) then
    Result := ADefault;
end;

function TRALParam.GetAsInteger: IntegerRAL;
var
  vVar: Variant;
begin
  Result := 0;
  if Self = nil then
    Exit;

  if GetTypedVariant(vVar) then
    Result := vVar
  else
    Result := StrToIntDef(ContentText, 0);
end;

function TRALParam.GetAsStream: TStream;
begin
  Result := nil;

  if Self <> nil then
    Result := SaveToStream;
end;

function TRALParam.GetAsString: StringRAL;
var
  vVar: Variant;
begin
  Result := '';
  if Self = nil then
    Exit;

  { A typed value as invariant text: a boolean as 1 or 0, a date and time as its
    TDateTime number. }
  if GetTypedVariant(vVar) then
  begin
    if VarIsType(vVar, varBoolean) then
    begin
      if vVar then
        Result := '1'
      else
        Result := '0';
    end
    else if VarIsType(vVar, varCurrency) then
      Result := StringRAL(CurrToStr(vVar, RALInvariantFormat))
    else if VarIsType(vVar, varDouble) then
      Result := StringRAL(FloatToStr(Double(vVar), RALInvariantFormat))
    else
      Result := StringRAL(VarToStr(vVar));
  end
  else
    Result := ContentText;
end;

function TRALParam.ContentText: StringRAL;
begin
  if FIsText then
    Result := FText
  else
    Result := RALStreamText(FContent);
end;

procedure TRALParam.NeedStream;
begin
  if not FIsText then
    Exit;
  FIsText := False;
  FContent := StringToStreamUTF8(FText);
  FOwnsContent := True;
  FText := '';
end;

function TRALParam.GetContent: TStream;
begin
  Result := nil;
  if Self = nil then
    Exit;
  NeedStream;
  Result := FContent;
end;

function TRALParam.GetContentDisposition: StringRAL;
var
  vName: StringRAL;
  vInt: IntegerRAL;
begin
  // quotes and backslashes leave the name: it goes inside a quoted string
  vName := FFileName;
  vInt := POSINISTR;
  while vInt <= RALHighStr(vName) do
    if (vName[vInt] = '"') or (vName[vInt] = '\') then
      Delete(vName, vInt - POSINISTR + 1, 1) // Delete counts from 1 everywhere
    else
      Inc(vInt);

  // inline carries the file name but no name=
  if vName = '' then
    Result := gInline
  else if FContentDispositionInline then
    Result := 'inline; filename="' + vName + '"'
  else
    Result := 'attachment; name="' + FParamName + '"; filename="' + vName + '"';
end;

function TRALParam.GetContentSize: Int64RAL;
begin
  Result := Size;
end;

procedure TRALParam.SaveToFile(const AFileName: StringRAL);
begin
  NeedStream;
  SaveStream(FContent, AFileName);
end;

procedure TRALParam.SaveToFile;
begin
  SaveToFile('', '');
end;

procedure TRALParam.SaveToStream(AStream: TStream);
begin
  // a text value is written straight from the string
  if FIsText then
  begin
    // Pointer(FText)^: FText[1] makes Delphi copy a shared string first
    if FText <> '' then
      AStream.WriteBuffer(Pointer(FText)^, Length(FText));
    Exit;
  end;

  if (FContent = nil) or (FContent.Size = 0) then
    Exit;

  FContent.Position := 0;
  AStream.CopyFrom(FContent, FContent.Size);
end;

function TRALParam.SaveToStream: TStream;
begin
  // copied in one go (TRALStringStream)
  if FIsText then
    Result := TRALStringStream.Create(FText)
  else if (FContent <> nil) and (FContent.Size > 0) then
    Result := TRALStringStream.Create(FContent)
  else
    Result := TRALStringStream.Create;

  Result.Position := 0;
end;

procedure TRALParam.SaveToFile(AFolderName, AFileName: StringRAL);
var
  vMime: TRALMIMEType;
  vExt: StringRAL;
begin
  if AFolderName = '' then
    AFolderName := ExtractFileDir(ParamStr(0));

  AFolderName := IncludeTrailingPathDelimiter(AFolderName);

  if AFileName = '' then
  begin
    if FFileName = '' then
    begin
      vMime := TRALMIMEType.GetInstance;
      try
        vExt := vMime.GetMIMEContentExt(FContentType);
      finally
      end;

      AFileName := FParamName + vExt;
    end
    else
    begin
      AFileName := FFileName;
    end;
  end;

  // only the last path component: the name may come from the wire
  AFileName := ExtractFileName(StringReplace(StringReplace(AFileName,
    '\', PathDelim, [rfReplaceAll]), '/', PathDelim, [rfReplaceAll]));
  if AFileName = '' then
    raise Exception.Create(emParamFileNameEmpty);

  SaveToFile(AFolderName + AFileName);
end;

procedure TRALParam.SetAsBoolean(const AValue: Boolean);
var
  vStr: StringRAL;
begin
  vStr := IntToStr(Integer(AValue));
  SetAsString(vStr);
end;

procedure TRALParam.SetAsDouble(const AValue: DoubleRAL);
var
  vStr: StringRAL;
begin
  vStr := FloatToStr(AValue);
  SetAsString(vStr);
end;

procedure TRALParam.SetAsInteger(const AValue: IntegerRAL);
var
  vStr: StringRAL;
begin
  vStr := IntToStr(AValue);
  SetAsString(vStr);
end;

procedure TRALParam.SetAsStream(const AValue: TStream);
begin
  // AValue may be this param's own content: copied before it is released
  if (AValue <> nil) and (AValue = FContent) then
  begin
    AValue.Position := 0;
    AdoptStream(TRALStringStream.Create(AValue));
    Exit;
  end;
  FreeContent;

  FText := '';
  FIsText := False;
  if AValue <> nil then
  begin
    AValue.Position := 0;
    FContent := TRALStringStream.Create(AValue);
    FContent.Position := 0;
  end;

  // new content drops a typed marker; the decoder sets ContentType after
  if IsTyped then
    FContentType := gOctetStream;
end;

procedure TRALParam.AdoptStream(AStream: TStream);
begin
  if AStream <> FContent then
    FreeContent;
  FOwnsContent := True;

  FText := '';
  FIsText := False;
  FContent := AStream;
  if FContent <> nil then
    FContent.Position := 0;

  // same rule as SetAsStream: new content, no stale typed marker
  if IsTyped then
    FContentType := gOctetStream;
end;

procedure TRALParam.SetAsString(const AValue: StringRAL);
begin
  FreeContent;

  FText := AValue;
  FIsText := True;

  // text drops a typed marker, or text of the right size would read as that type
  if IsTyped then
    FContentType := gTextPlain;
end;

procedure TRALParam.SetContentDisposition(AValue: StringRAL);
var
  vStr: StringRAL;

  function GetWord(var AStr: StringRAL): StringRAL;
  var
    vInt, vLen: Integer;
    vQuoted: Boolean;
    vChr: CharRAL;
  begin
    Result := '';
    vLen := Length(AStr);
    vQuoted := False;
    for vInt := 1 to vLen do
    begin
      vChr := CharRAL(AStr[vInt]);
      if (vChr = '"') then
      begin
        vQuoted := not vQuoted;
      end
      else if not (CharInSet(vChr, [' ', '=', ';', ':'])) or vQuoted then
      begin
        Result := Result + vChr;
      end
      else if (CharInSet(vChr, [';', ':', '='])) and (not vQuoted) then
      begin
        Delete(AStr, 1, vInt);
        Exit;
      end;
    end;
    AStr := '';
  end;

  function ProcessVar(const AHeader, AValue: StringRAL): Boolean;
  begin
    Result := True;
    // through the setter, which keeps the name index right
    if RALSameName(AHeader, 'name') then
      ParamName := AValue
    else if RALSameName(AHeader, 'filename') then
      FFileName := AValue
    else
      Result := False;
  end;

begin
  AValue := Trim(AValue);
  // the disposition type (inline, attachment, form-data)
  vStr := GetWord(AValue);
  // the first parameter
  vStr := GetWord(AValue);
  while (vStr <> '') do
  begin
    ProcessVar(vStr, GetWord(AValue));
    vStr := GetWord(AValue);
  end;
end;

{ TRALParams }

function TRALParams.ReplaceParam(const AName, AValue: StringRAL;
  AKind: TRALParamKind): TRALParam;
begin
  Result := nil;
  if AName = '' then
    Exit;
  DelParam(AName);
  Result := NewParam;
  Result.ParamName := AName;
  Result.AsString := AValue;
  Result.ContentType := gTextPlain;
  Result.Kind := AKind;
end;

function TRALParams.AddParam(const AName, AValue: StringRAL; AKind: TRALParamKind): TRALParam;
begin
  Result := nil;
  if (AName <> '') and (AValue <> '') then
  begin
    Result := FindOrNewParam(AName, AKind);
    Result.AsString := AValue;
    Result.ContentType := gTextPlain;
    Result.Kind := AKind;
  end;
end;

function TRALParams.AddParam(const AName: StringRAL; const AValue: Variant;
  AKind: TRALParamKind; AType: TRALParamType): TRALParam;
begin
  Result := nil;
  if AName = '' then
    Exit;

  Result := FindOrNewParam(AName, AKind);
  Result.Kind := AKind;

  // the Variant conversions are numeric: no locale involved
  case AType of
    rptInteger:
      Result.SetTypedInteger(AValue);
    rptInt64:
      Result.SetTypedInt64(AValue);
    rptDouble:
      Result.SetTypedDouble(AValue);
    rptCurrency:
      Result.SetTypedCurrency(AValue);
    rptBoolean:
      Result.SetTypedBoolean(AValue);
    rptDateTime:
      Result.SetTypedDateTime(AValue);
  else
    begin
      Result.AsString := VarToStr(AValue);
      Result.ContentType := gTextPlain;
    end;
  end;
end;

function TRALParams.AddParam(const AName: StringRAL; AContent: TStream;
  AKind: TRALParamKind): TRALParam;
begin
  Result := FindOrNewParam(AName, AKind);
  Result.AsStream := AContent;
  Result.ContentType := gOctetStream;
  Result.Kind := AKind;
end;

function TRALParams.AddFile(const AParamName, AFileName: StringRAL): TRALParam;
begin
  Result := nil;
  if (AParamName = '') or (AFileName = '') then
    Exit;

  Result := FindOrNewParam(AParamName, rpkBODY);
  Result.FileName := ExtractFileName(AFileName);
  Result.OpenFile(AFileName);
  Result.Kind := rpkBODY;

  // the MIME table is a singleton, never freed here
  Result.ContentType := TRALMIMEType.GetInstance.GetMIMEType(AFileName);
  if Result.ContentType = '' then
    Result.ContentType := gOctetStream;
end;

function TRALParams.AddFile(const AFileName: StringRAL): TRALParam;
begin
  Result := nil;
  if AFileName = '' then
    Exit;

  Result := NewParam;
  Result.ParamName := NextParamStr;
  Result.FileName := ExtractFileName(AFileName);
  Result.OpenFile(AFileName);
  Result.Kind := rpkBODY;

  Result.ContentType := TRALMIMEType.GetInstance.GetMIMEType(AFileName);
  if Result.ContentType = '' then
    Result.ContentType := gOctetStream;
end;

function TRALParams.AddValue(const AContent: StringRAL; AKind: TRALParamKind = rpkNONE)
  : TRALParam;
begin
  Result := NewParam;
  Result.ParamName := NextParamStr;
  Result.AsString := AContent;
  Result.ContentType := gTextPlain;
  Result.Kind := AKind;
end;

function TRALParams.AddValue(AContent: TStream; AKind: TRALParamKind = rpkNONE): TRALParam;
begin
  Result := NewParam;
  Result.ParamName := NextParamStr;
  Result.AsStream := AContent;
  Result.ContentType := gOctetStream;
  Result.Kind := AKind;
end;

procedure TRALParams.ClearParams;
var
  vParam, vNext: TRALParam;
begin
  { a block still pending goes as text, nothing parsed; its detached params
    are in no list but this chain }
  FPending := '';
  FPendingCount := 0;
  vParam := FDetached;
  FDetached := nil;
  FDetachedCount := 0;
  while vParam <> nil do
  begin
    vNext := vParam.FNextDetached;
    vParam.Free;
    vParam := vNext;
  end;

  FBuckets := nil; // every param goes, and the index with them
  while FList.Count > 0 do
  begin
    TObject(FList.Items[FList.Count - 1]).Free;
    FList.Delete(FList.Count - 1);
  end;
  { after the params: they may be windows over these }
  ClearBuffers;
end;

function TRALParams.GetDecoded: TStream;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  Result := FDecoded;
  if (Result = nil) or not FDecodedIsParam then
    Exit;
  for vInt := 0 to Pred(FParams.Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if (not vParam.FIsText) and (vParam.FContent = Result) then
      Exit;
  end;
  Result := nil;
end;

procedure TRALParams.ClearBuffers;
begin
  FDecoded := nil;
  FDecodedIsParam := False;
  if FBuffers = nil then
    Exit;
  while FBuffers.Count > 0 do
  begin
    TObject(FBuffers.Items[FBuffers.Count - 1]).Free;
    FBuffers.Delete(FBuffers.Count - 1);
  end;
end;

procedure TRALParams.ClearParams(AKind: TRALParamKind);
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  vInt := FParams.Count - 1;
  while vInt >= 0 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind = AKind then
    begin
      if vParam.FIndexed then
        IndexRemove(vParam);
      vParam.Free;
      FParams.Delete(vInt);
    end;
    vInt := vInt - 1;
  end;
  { the body params were the only readers of the decoded body }
  if AKind = rpkBODY then
    ClearBuffers;
end;

procedure TRALParams.AppendParams(ASource: TStringList; AKind: TRALParamKind);
begin
  AppendParams(TStrings(ASource), AKind);
end;

procedure TRALParams.AppendParams(ASource: TStrings; AKind: TRALParamKind);
var
  vInt: Integer;
  vSeparator: StringRAL;
begin
  vSeparator := '';

  { Headers: the separator is read from the first line, since
    TStrings.NameValueSeparator is a Char that is never empty. }
  if (AKind = rpkHEADER) and (ASource.Count > 0) then
    vSeparator := FindHeaderNameSeparator(ASource.Strings[0]);

  if vSeparator = '' then
    vSeparator := ASource.NameValueSeparator;

  for vInt := 0 to Pred(ASource.Count) do
    AppendParamLine(ASource.Strings[vInt], vSeparator, AKind);
end;

/// True when every byte of AText is below 128.
function IsAsciiText(const AText: StringRAL): Boolean;
var
  vByte: PByte;
  vInt: IntegerRAL;
begin
  Result := False;
  vByte := PByte(Pointer(AText));
  for vInt := 1 to Length(AText) do
  begin
    if vByte^ > 127 then
      Exit;
    Inc(vByte);
  end;
  Result := True;
end;

procedure TRALParams.AppendParamsListText(ASource: StringRAL; AKind: TRALParamKind;
  ANameSeparator: StringRAL);
var
  vInt, vStart: IntegerRAL;
  vIs13: Boolean;
begin
  // an ASCII block, as headers are, skips the round trip through UTF-16
  if not IsAsciiText(ASource) then
  begin
    {$IFDEF FPC}
      ASource := UTF8Decode(ASource);
    {$ELSE}
      ASource := UTF8ToString(ASource);
    {$ENDIF}
  end;

  if (ASource <> '') and (ANameSeparator = '') then
    ANameSeparator := FindHeaderNameSeparator(ASource);

  { kept as text when it can be - see FPending }
  if (AKind = rpkHEADER) and (ASource <> '') and (ANameSeparator <> '') and
     (FPending = '') and DeferBlock(ASource, ANameSeparator, AKind) then
    Exit;

  // CR and LF each end a line, CRLF only one; the tail counts when not empty
  vStart := POSINISTR;
  vIs13 := False;
  for vInt := POSINISTR to RALHighStr(ASource) do
  begin
    if ASource[vInt] = #13 then
    begin
      AppendParamSpan(ASource, vStart, vInt - vStart, ANameSeparator, AKind);
      vIs13 := True;
      vStart := vInt + 1;
    end
    else if ASource[vInt] = #10 then
    begin
      if not vIs13 then
        AppendParamSpan(ASource, vStart, vInt - vStart, ANameSeparator, AKind);
      vIs13 := False;
      vStart := vInt + 1;
    end
    else
      vIs13 := False;
  end;

  if vStart <= RALHighStr(ASource) then
    AppendParamSpan(ASource, vStart, RALHighStr(ASource) - vStart + 1, ANameSeparator, AKind);
end;

procedure TRALParams.AppendParamsText(AText: StringRAL; AKind: TRALParamKind;
  const ANameSeparator: StringRAL; const ALineSeparator: StringRAL);
var
  vText, vLine, vName: PByte;
  vLen, vLineLen, vNameLen, vStart, vInt: IntegerRAL;

  { The segment [vStart, AEnd) holds name, separator and value, cut straight
    from the text; a segment without the separator is skipped. }
  procedure AppendUpTo(AEnd: IntegerRAL);
  var
    vPos: IntegerRAL;
  begin
    vPos := vStart;
    while vPos <= AEnd - vNameLen do
    begin
      if (vText[vPos] = vName^) and
         ((vNameLen = 1) or CompareMem(@vText[vPos], vName, vNameLen)) then
      begin
        AppendParamPair(Copy(AText, vStart + 1, vPos - vStart),
          Copy(AText, vPos + vNameLen + 1, AEnd - vPos - vNameLen), AKind);
        Exit;
      end;
      Inc(vPos);
    end;
  end;

begin
  { One scan through a pointer, offsets 0-based (Copy takes them plus one).
    Empty segments are skipped, and so is everything without a name separator. }
  vLen := Length(AText);
  vLineLen := Length(ALineSeparator);
  vNameLen := Length(ANameSeparator);
  if (vLen = 0) or (vNameLen = 0) then
    Exit;
  vText := PByte(Pointer(AText));
  vName := PByte(Pointer(ANameSeparator));
  vStart := 0;
  if vLineLen > 0 then
  begin
    vLine := PByte(Pointer(ALineSeparator));
    vInt := 0;
    while vInt <= vLen - vLineLen do
      if (vText[vInt] = vLine^) and
         ((vLineLen = 1) or CompareMem(@vText[vInt], vLine, vLineLen)) then
      begin
        AppendUpTo(vInt);
        Inc(vInt, vLineLen);
        vStart := vInt;
      end
      else
        Inc(vInt);
  end;
  AppendUpTo(vLen);
end;

procedure TRALParams.AppendParamsUri(AFullURI, APartialURI: StringRAL; AKind: TRALParamKind);
var
  vInt, vIdx, vStart, vLen: IntegerRAL;
  vName: StringRAL;
  vParam: TRALParam;
begin
  AFullURI := FixRoute(AFullURI);
  APartialURI := FixRoute(APartialURI);

  { The segments of AFullURI past APartialURI, as ral_uriparam1, 2...; the
    prefix has to be whole segments at the start. }
  vLen := Length(APartialURI);
  if APartialURI = '/' then
    vLen := 0
  else if (not RALSameName(Copy(AFullURI, 1, vLen), APartialURI)) or
          ((Length(AFullURI) > vLen) and (AFullURI[POSINISTR + vLen] <> '/')) then
    Exit;

  vIdx := 1;
  vStart := vLen + 2; // 1-based, as Copy counts: past the '/' after the prefix
  for vInt := vStart to Length(AFullURI) + 1 do
    if (vInt > Length(AFullURI)) or (AFullURI[POSINISTR - 1 + vInt] = '/') then
    begin
      if vInt > vStart then
      begin
        vName := 'ral_uriparam' + IntToStr(vIdx);
        vParam := FindOrNewParam(vName, AKind);
        vParam.AsString := Copy(AFullURI, vStart, vInt - vStart);
        vParam.Kind := AKind;
        Inc(vIdx);
      end;
      vStart := vInt + 1;
    end;
end;

procedure TRALParams.AppendParamsUrl(AUrlQuery: StringRAL; AKind: TRALParamKind);
var
  vInt: IntegerRAL;
begin
  vInt := Pos('?', AUrlQuery);
  if vInt > 0 then
    System.Delete(AUrlQuery, 1, vInt);

  AppendParamsText(AUrlQuery, AKind);
end;

procedure TRALParams.AssignParams(ADest: TStringList; AKind: TRALParamKind;
  ASeparator: StringRAL);
begin
  AssignParams(TStrings(ADest), AKind, ASeparator);
end;

function TRALParams.AsJSON: StringRAL;
var
  I: IntegerRAL;
  JSON: TRALJSONObject;
begin
  Result := '';
  if (FParams <> nil) and (FParams.Count > 0) then
  begin
    JSON := TRALJSONObject.Create;
    try
      for I := 0 to Pred(FParams.Count) do
        JSON.Add(TRALParam(FParams.Items[I]).ParamName, TRALParam(FParams.Items[I]).AsString);

      Result := JSON.ToJSON;
    finally
      JSON.Free;
    end;
  end;
end;

procedure TRALParams.AssignParams(ADest: TStrings; AKind: TRALParamKind;
  ASeparator: StringRAL);
var
  vInt: IntegerRAL;
  vParam: TRALParam;
  vHeader: boolean;
begin
  // a header or a cookie is one line: CR or LF in it would end the line
  vHeader := AKind in [rpkHEADER, rpkCOOKIE];
  for vInt := 0 to Pred(FParams.Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind <> AKind then
      Continue;
    if vHeader then
      ADest.Add(RALSafeHeaderText(vParam.ParamName + ASeparator + vParam.AsString))
    else
      ADest.Add(vParam.ParamName + ASeparator + vParam.AsString);
  end;
end;

function TRALParams.AssignParamsListText(AKind: TRALParamKind;
  const ANameSeparator: StringRAL): StringRAL;
begin
  Result := AssignParamsText(AKind, False, ANameSeparator, HTTPLineBreak);
end;

{ Built in a buffer that grows geometrically. Headers and cookies are sized first
  and written at their exact length, CR, LF and NUL turned into blanks. }
function TRALParams.AssignParamsText(AKind: TRALParamKind; AUrlEncoded: boolean;
  const ANameSeparator: StringRAL; const ALineSeparator: StringRAL): StringRAL;
var
  vInt: integer;
  vParam: TRALParam;
  vUsed, vCap: IntegerRAL;
  vHeader: boolean;
  vValue: StringRAL;

  { copies AText at vUsed; ASafe turns CR, LF and NUL into a space, as
    RALSafeHeaderText does - for names and values, never for the separators
    the caller chose }
  procedure PutAt(const AText: StringRAL; ASafe: boolean);
  var
    vLen, vPos: IntegerRAL;
    vChar: PAnsiChar;
  begin
    vLen := Length(AText);
    if vLen = 0 then
      Exit;
    // a value longer than its first reading must not write past the string
    if vUsed + vLen > Length(Result) then
      SetLength(Result, vUsed + vLen);
    Move(Pointer(AText)^, Result[POSINISTR + vUsed], vLen);
    if ASafe then
    begin
      vChar := @Result[POSINISTR + vUsed];
      for vPos := 1 to vLen do
      begin
        if vChar^ in [#0, #10, #13] then
          vChar^ := ' ';
        Inc(vChar);
      end;
    end;
    Inc(vUsed, vLen);
  end;

  procedure Put(const AText: StringRAL);
  var
    vNeed: IntegerRAL;
  begin
    if AText = '' then
      Exit;
    vNeed := vUsed + Length(AText);
    if vNeed > vCap then
    begin
      if vCap = 0 then
        vCap := 256;
      while vCap < vNeed do
        vCap := vCap * 2;
      SetLength(Result, vCap);
    end;
    Move(AText[POSINISTR], Result[POSINISTR + vUsed], Length(AText));
    vUsed := vNeed;
  end;

begin
  Result := '';
  vUsed := 0;
  vCap := 0;
  { the same rule as AssignParams - a URL-encoded text is never a header }
  vHeader := (AKind in [rpkHEADER, rpkCOOKIE]) and (not AUrlEncoded);
  if vHeader then
  begin
    for vInt := 0 to Pred(Count) do
    begin
      vParam := TRALParam(FParams.Items[vInt]);
      if vParam.Kind <> AKind then
        Continue;
      vValue := vParam.AsString;
      if vCap > 0 then
        Inc(vCap, Length(ALineSeparator));
      Inc(vCap, Length(vParam.ParamName) + Length(ANameSeparator) + Length(vValue));
    end;
    if vCap = 0 then
      Exit;
    SetLength(Result, vCap);
    for vInt := 0 to Pred(Count) do
    begin
      vParam := TRALParam(FParams.Items[vInt]);
      if vParam.Kind <> AKind then
        Continue;
      vValue := vParam.AsString;
      if vUsed > 0 then
        PutAt(ALineSeparator, False);
      PutAt(vParam.ParamName, True);
      PutAt(ANameSeparator, False);
      PutAt(vValue, True);
    end;
    if vUsed < Length(Result) then
      SetLength(Result, vUsed);
    Result := RALTrimRight(Result);
    Exit;
  end;
  for vInt := 0 to Pred(Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind <> AKind then
      Continue;
    if vUsed > 0 then
      Put(ALineSeparator);
    if vHeader then
    begin
      Put(RALSafeHeaderText(vParam.ParamName));
      Put(ANameSeparator);
      Put(RALSafeHeaderText(vParam.AsString));
    end
    else
    begin
      Put(vParam.ParamName);
      Put(ANameSeparator);
      if AUrlEncoded then
        Put(TRALHTTPCoder.EncodeURL(vParam.AsString))
      else
        Put(vParam.AsString);
    end;
  end;
  SetLength(Result, vUsed);

  Result := RALTrimRight(Result);
end;

function TRALParams.AssignParamsUrl(AKind: TRALParamKind): StringRAL;
begin
  Result := AssignParamsText(AKind, True);
end;

{ Every client engine builds its Cookie header here; a Set-Cookie param gives
  its name=value pair alone, since a Cookie header carries no attributes. }
function TRALParams.CookieHeaderText: StringRAL;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
  vPair: StringRAL;
begin
  Result := '';
  for vInt := 0 to Pred(Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind <> rpkCOOKIE then
      Continue;
    if RALSameName(vParam.ParamName, 'Set-Cookie') then
    begin
      vPair := CookiePairOf(vParam.AsString);
      // a line with no name=value in it is no cookie to send
      if Pos(StringRAL('='), vPair) <= 1 then
        Continue;
    end
    else
      vPair := vParam.ParamName + '=' + vParam.AsString;
    if Result <> '' then
      Result := Result + '; ';
    Result := Result + RALSafeHeaderText(vPair);
  end;
end;

{ For the engines whose library keeps a jar (Indy, netHTTP): one header with the
  jar's cookies and the params'. Names compare case-sensitively (RFC 6265). }
function TRALParams.CookieHeaderText(const AJarCookies: StringRAL): StringRAL;
var
  vInt, vPos: IntegerRAL;
  vParam: TRALParam;
  vApp, vText, vPair: StringRAL;
  vNames: TStringList;

  function NameOf(const APair: StringRAL): StringRAL;
  var
    vEq: IntegerRAL;
  begin
    vEq := Pos(StringRAL('='), APair);
    if vEq > 0 then
      Result := RALTrim(Copy(APair, 1, vEq - 1))
    else
      Result := RALTrim(APair);
  end;

begin
  { the parentheses are the call: in ObjFPC mode the bare name of the function
    being written is its Result }
  vApp := CookieHeaderText();
  if AJarCookies = '' then
  begin
    Result := vApp;
    Exit;
  end;

  vNames := TStringList.Create;
  try
    vNames.CaseSensitive := True;
    for vInt := 0 to Pred(Count) do
    begin
      vParam := TRALParam(FParams.Items[vInt]);
      if vParam.Kind <> rpkCOOKIE then
        Continue;
      if RALSameName(vParam.ParamName, 'Set-Cookie') then
        vNames.Add(NameOf(CookiePairOf(vParam.AsString)))
      else
        vNames.Add(vParam.ParamName);
    end;

    Result := '';
    vText := AJarCookies;
    while vText <> '' do
    begin
      vPos := Pos(StringRAL(';'), vText);
      if vPos > 0 then
      begin
        vPair := RALTrim(Copy(vText, 1, vPos - 1));
        Delete(vText, 1, vPos);
      end
      else
      begin
        vPair := RALTrim(vText);
        vText := '';
      end;
      if (Pos(StringRAL('='), vPair) <= 1) or
         (vNames.IndexOf(NameOf(vPair)) >= 0) then
        Continue;
      if Result <> '' then
        Result := Result + '; ';
      Result := Result + RALSafeHeaderText(vPair);
    end;
  finally
    vNames.Free;
  end;

  if vApp <> '' then
  begin
    if Result <> '' then
      Result := Result + '; ';
    Result := Result + vApp;
  end;
end;

function TRALParams.AsString: StringRAL;
var
  I: IntegerRAL;
begin
  Result := '';
  if FParams <> nil then
    for I := 0 to Pred(FParams.Count) do
    begin
      if I > 0 then // between the values, not after the last one
        Result := Result + ', ';
      Result := Result + TRALParam(FParams.Items[I]).AsString;
    end;
end;

{ Content type of a multipart body, rebuilt from its first line, the delimiter:
  an encrypted multipart travels declared as octet-stream. }
function BodyContentType(AStream: TStream): StringRAL;
const
  { RFC 2046 bchars; anything else on the first line means it is not a
    delimiter, whatever it starts with }
  BCHARS = ['0'..'9', 'a'..'z', 'A'..'Z', '''', '(', ')', '+', '_', ',', '-',
    '.', '/', ':', '=', '?'];
var
  vByte: Byte;
  vPos: Int64RAL;
  vBoundary, vClose, vTail: StringRAL;
  vSize: IntegerRAL;
begin
  Result := '';
  vBoundary := '';
  if (AStream = nil) or (AStream.Size < 3) then
    Exit;
  vPos := AStream.Position;
  try
    AStream.Position := 2; // past the two dashes
    while AStream.Position < AStream.Size do
    begin
      AStream.ReadBuffer(vByte, 1);
      if (vByte = 13) or (vByte = 10) then
        Break;
      if (vByte > 127) or not (Chr(vByte) in BCHARS) then
        Exit; // not a delimiter: an ordinary body that starts with two dashes
      vBoundary := vBoundary + StringRAL(Chr(vByte));
    end;
    if (vBoundary = '') or (Length(vBoundary) > 70) then
      Exit;

    // a real multipart ends with the close delimiter: only the tail is read
    vClose := '--' + vBoundary + '--';
    vSize := Length(vClose) + 4; // room for a CRLF after the close
    if AStream.Size < vSize then
      vSize := AStream.Size;
    SetLength(vTail, vSize);
    AStream.Position := AStream.Size - vSize;
    AStream.ReadBuffer(vTail[1], vSize);
    if Pos(vClose, vTail) = 0 then
      Exit;
  finally
    AStream.Position := vPos;
  end;
  Result := rctMULTIPARTFORMDATA + '; boundary=' + vBoundary;
end;

{ True when AStream starts with '--', as a plain multipart does; no compressed
  body starts that way. }
function StartsWithDelim(AStream: TStream): boolean;
var
  vDashes: array [0 .. 1] of Byte;
  vPos: Int64RAL;
begin
  Result := False;
  if (AStream = nil) or (AStream.Size < 2) then
    Exit;
  vPos := AStream.Position;
  try
    AStream.Position := 0;
    AStream.ReadBuffer(vDashes[0], 2);
    Result := (vDashes[0] = Ord('-')) and (vDashes[1] = Ord('-'));
  finally
    AStream.Position := vPos;
  end;
end;

function TRALParams.NewCipher(AEncoding: boolean): TRALCriptoAES;
begin
  Result := nil;
  if not HasCipher(AEncoding) then
    Exit;
  Result := TRALCriptoAES.Create;
  case FCriptoOptions.CriptType of
    crAES128: Result.AESType := tAES128;
    crAES192: Result.AESType := tAES192;
    crAES256: Result.AESType := tAES256;
  end;
  Result.Key := FCriptoOptions.Key;
end;

/// True when the decoder may overwrite AStream: it is not a view of other memory.
function CanWriteBody(AStream: TStream): boolean;
begin
  Result := not ((AStream is TRALMemoryView) or (AStream is TRALStreamSlice) or
    (AStream is TRALConcatStream));
end;

function TRALParams.DecodeBody(ASource: TStream;
  const AContentType, AContentDisposition: StringRAL): TStream;
begin
  Result := DecodeBody(ASource, AContentType, AContentDisposition, boCopy);
end;

function TRALParams.DecodeBody(ASource: TStream;
  const AContentType, AContentDisposition: StringRAL;
  AOwnership: TRALBodyOwnership): TStream;
var
  vParam: TRALParam;
  vDecoder: TRALMultipartDecoder;
  vCur, vNew: TStream;
  vBody: TRALBodyStream;
  vChain: TList;
  vCurOwned, vWritable, vHanded: Boolean;
  vCTMultipart: StringRAL;
  vCipher: TRALCriptoAES;
  vClass: TRALCompressClass;
  vComp: TRALCompress;
  vLen: Int64RAL;

  // frees the streams vCur depends on (the buffer a window was decrypted in)
  procedure DropChain;
  begin
    while vChain.Count > 0 do
    begin
      TObject(vChain.Items[vChain.Count - 1]).Free;
      vChain.Delete(vChain.Count - 1);
    end;
  end;

  procedure KeepChain;
  begin
    if FBuffers = nil then
      FBuffers := TList.Create;
    while vChain.Count > 0 do
    begin
      FBuffers.Add(vChain.Items[0]);
      vChain.Delete(0);
    end;
  end;

  // vCur becomes ANew, which depends on nothing before it
  procedure Replace(ANew: TStream);
  begin
    if vCurOwned then
      vChain.Add(vCur);
    DropChain;
    vCur := ANew;
    vCurOwned := True;
    vWritable := True;
  end;

begin
  // returns nil: the body ends up in the params, read where it is
  Result := nil;
  vParam := nil;
  FBodyError := '';
  if ASource = nil then
    Exit;

  if AOwnership = boCopy then
  begin
    vBody := TRALBodyStream.Create(ASource.Size, FSpoolAbove);
    try
      ASource.Position := 0;
      RALCopyStream(ASource, vBody, ASource.Size);
      vCur := vBody.Detach;
    finally
      vBody.Free;
    end;
    vCurOwned := True;
  end
  else
  begin
    vCur := ASource;
    vCurOwned := AOwnership = boOwned;
  end;
  vWritable := (AOwnership <> boBorrowed) and CanWriteBody(vCur);
  vHanded := False;

  vChain := TList.Create;
  try
    try
      vCur.Position := 0;

      vCipher := NewCipher(False);
      if vCipher <> nil then
        try
          if vWritable then
          begin
            { in place: the plaintext lands on the ciphertext, and the body is
              a window over it from after the IV }
            vLen := vCipher.DecryptInPlace(vCur);
            if vCurOwned then
              vChain.Add(vCur);
            vCur := RALStreamSlice(vCur, 16, vLen);
            vCurOwned := True;
            vWritable := False;
          end
          else
          begin
            vBody := TRALBodyStream.Create(vCur.Size, FSpoolAbove);
            try
              vCipher.DecryptTo(vCur, vBody);
              vNew := vBody.Detach;
            finally
              vBody.Free;
            end;
            Replace(vNew);
          end;
        finally
          vCipher.Free;
        end;

      { A body that starts with a multipart delimiter was never compressed: the
        bytes decide, not the header. }
      if (FCompressType <> ctNone) and not StartsWithDelim(vCur) then
      begin
        vClass := GetCompressClass(FCompressType);
        if vClass <> nil then
        begin
          vComp := vClass.Create;
          try
            vComp.Format := FCompressType;
            { sized once when gzip says how big it inflates to }
            vBody := TRALBodyStream.Create(RALInflatedSizeHint(vCur, FCompressType),
              FSpoolAbove);
            try
              vComp.DecompressTo(vCur, vBody);
              vNew := vBody.Detach;
            finally
              vBody.Free;
            end;
          finally
            vComp.Free;
          end;
          Replace(vNew);
        end;
      end;

      { Multipart by the header, or by the bytes for an encrypted body, which
        travels declared as octet-stream. }
      vCTMultipart := '';
      if Pos(rctMULTIPARTFORMDATA, LowerCase(AContentType)) > 0 then
        vCTMultipart := AContentType
      else if (FCriptoOptions <> nil) and (FCriptoOptions.CriptType <> crNone) and StartsWithDelim(vCur) then
        vCTMultipart := BodyContentType(vCur);

      if vCTMultipart <> '' then
      begin
        { each part is a window over the decoded body, which stays alive with
          the params }
        vDecoder := TRALMultipartDecoder.Create;
        try
          vDecoder.ContentType := vCTMultipart;
          vDecoder.SliceParts := True;
          vDecoder.OnFormDataComplete := {$IFDEF FPC}@{$ENDIF}OnFormBodyData;
          vDecoder.ProcessMultiPart(vCur);
          // bytes with no part and no close delimiter: refused through BodyError
          if (vDecoder.PartCount = 0) and (not vDecoder.Closed) and (vCur.Size > 0) then
            FBodyError := emMultipartNoPart;
        finally
          FreeAndNil(vDecoder);
        end;
        if vCurOwned then
          vChain.Add(vCur);
      end
      else if Pos(rctAPPLICATIONXWWWFORMURLENCODED, LowerCase(AContentType)) > 0 then
      begin
        DecodeFields(StreamToString(vCur));
        if vCurOwned then
          vChain.Add(vCur);
      end
      else
      begin
        vParam := NewParam;
        vParam.ParamName := gRalBody;
        vParam.FileName := '';
        vParam.ContentDisposition := AContentDisposition;

        // content first: it drops a typed marker, which the type then restores
        if vCurOwned then
          vParam.AdoptStream(vCur)
        else
          vParam.BorrowStream(vCur);
        vParam.ContentType := AContentType;
        vParam.Kind := rpkBODY;
      end;
      vHanded := True;
      KeepChain;
      FDecoded := vCur;
      FDecodedIsParam := vParam <> nil;
    except
      // what was built stays with the params: parts may be windows over it
      if vCurOwned and not vHanded then
        vChain.Add(vCur);
      KeepChain;
      raise;
    end;
  finally
    vChain.Free;
  end;
end;

function TRALParams.DecodeBody(
  const ASource, AContentType, AContentDisposition: StringRAL): TStream;
var
  vStream: TStream;
begin
  Result := nil;
  if ASource = '' then
    Exit;

  // a view that holds the string: no copy and no code page conversion
  vStream := TRALStringView.Create(ASource);
  Result := DecodeBody(vStream, AContentType, AContentDisposition, boOwned);
end;

function TRALParams.PrepareBody(var AContentType, AContentDisposition: StringRAL;
  ACompressMultipart, AConsume: boolean): TStream;
var
  vMultPart: TRALMultipartEncoder;
  vInt, vInt1, vInt2: integer;
  vItem, vBody: TRALParam;
  vString, vValor, vFile: StringRAL;
  vFormAsMultipart, vSingle: boolean;
begin
  Result := nil;

  // one walk counts body and field params and finds the first body one
  vInt1 := 0;
  vInt2 := 0;
  vBody := nil;
  for vInt := 0 to Pred(FParams.Count) do
  begin
    vItem := TRALParam(FParams.Items[vInt]);
    if vItem.Kind = rpkBODY then
    begin
      if vBody = nil then
        vBody := vItem;
      Inc(vInt1);
    end
    else if vItem.Kind = rpkFIELD then
      Inc(vInt2);
  end;
  vSingle := (vInt1 = 1) and (vInt2 = 0);

  { An encrypted form request goes as multipart, declared octet-stream: Indy and
    libmicrohttpd parse a urlencoded body themselves and would drop the ciphertext. }
  vFormAsMultipart := (not ACompressMultipart) and (vInt1 = 0) and (vInt2 > 0) and
    HasCipher(True);

  AContentDisposition := '';

  // only a lone rpkBODY is the whole body; form fields are always name=value
  if vSingle then
  begin
    vItem := vBody;

    vItem.ContentDispositionInline := FContentDispositionInline;

    if Pos(StringRAL('ral_param'), vItem.ParamName) > 0 then
      vItem.ParamName := gRalBody;

    Result := vItem.TakeContent(AConsume);

    AContentType := vItem.ContentType;
    AContentDisposition := vItem.ContentDisposition;
  end
  else if (vInt2 > 0) and (vInt1 = 0) and (not vFormAsMultipart) then
  begin
    vString := '';
    for vInt1 := 0 to Pred(Count) do
    begin
      vItem := Index[vInt1];
      if vItem.Kind in [rpkFIELD] then
      begin
        if vString <> '' then
          vString := vString + '&';

        // name and value form-encoded
        vValor := TRALHTTPCoder.EncodeURL(vItem.ParamName) + '=' +
          TRALHTTPCoder.EncodeURL(vItem.AsString);

        vString := vString + vValor;
      end;
    end;
    Result := TRALStringView.Create(vString);

    AContentType := rctAPPLICATIONXWWWFORMURLENCODED;
  end
  else if (vInt1 + vInt2 > 1) or vFormAsMultipart then
  begin
    vMultPart := TRALMultipartEncoder.Create;
    try
      for vInt1 := 0 to Pred(Count) do
      begin
        vItem := Index[vInt1];
        if vItem.Kind in [rpkBODY, rpkFIELD] then
        begin
          { A body part without a file name is named after itself (libmicrohttpd
            drops unnamed parts); a field part stays unnamed, or other servers
            take it as an upload. }
          vFile := vItem.FileName;
          if (vFile = '') and (vItem.Kind = rpkBODY) then
            vFile := vItem.ParamName;
          { every part is a stream of its own - a view, or the param's stream
            moved out - owned by the multipart it goes into }
          vMultPart.AddStream(vItem.ParamName, vItem.TakeContent(AConsume),
            vFile, vItem.ContentType, True);
        end;
      end;
      Result := vMultPart.AsConcatStream;
      AContentType := vMultPart.ContentType;
    finally
      FreeAndNil(vMultPart);
    end;
  end;
end;

function TRALParams.PlainBody(var AContentType, AContentDisposition: StringRAL): TStream;
begin
  Result := PrepareBody(AContentType, AContentDisposition, True, False);
end;

function TRALParams.EffectiveCompress(const AContentType: StringRAL;
  ACompressMultipart: boolean): TRALCompressType;
begin
  Result := FCompressType;

  { On the request path a multipart or urlencoded body goes uncompressed: Indy
    and libmicrohttpd parse them before anything decompresses. }
  if (not ACompressMultipart) and (Result <> ctNone) and
     ((Pos(StringRAL(rctMULTIPARTFORMDATA), LowerCase(AContentType)) > 0) or
      (Pos(StringRAL(rctAPPLICATIONXWWWFORMURLENCODED), LowerCase(AContentType)) > 0)) then
    Result := ctNone;

  // already compressed (a JPEG, a zip): nothing to gain
  if (Result <> ctNone) and (FSkipCompressedTypes or (FSkipCompressTypes <> nil)) and
     RALIsCompressedType(AContentType, FSkipCompressedTypes, FSkipCompressTypes) then
    Result := ctNone;

  // a compressor that is not linked: the body goes uncompressed
  if (Result <> ctNone) and (GetCompressClass(Result) = nil) then
    Result := ctNone;

  // written back: Content-Encoding is filled from CompressType
  FCompressType := Result;
end;

procedure TRALParams.WriteTransformed(ASource, ADest: TStream;
  ACompress: TRALCompressType; var AContentType: StringRAL; ACompressMultipart: boolean);
var
  vCipher: TRALCriptoAES;
  vComp: TRALCompress;
  vStart: Int64RAL;
  vIV: array[0..15] of Byte;
begin
  vCipher := NewCipher(True);
  try
    if ACompress <> ctNone then
    begin
      vComp := GetCompressClass(ACompress).Create;
      try
        vComp.Format := ACompress;
        if vCipher <> nil then
        begin
          { room for the IV first: the compressor writes after it and the AES
            then ciphers that same stream in place - no buffer in between }
          vStart := ADest.Position;
          FillChar(vIV, SizeOf(vIV), 0);
          ADest.WriteBuffer(vIV, SizeOf(vIV));
          vComp.CompressTo(ASource, ADest);
          vCipher.EncryptInPlace(ADest, vStart);
        end
        else
          vComp.CompressTo(ASource, ADest);
      finally
        vComp.Free;
      end;
    end
    else if vCipher <> nil then
      vCipher.EncryptTo(ASource, ADest)
    else
    begin
      ASource.Position := 0;
      RALCopyStream(ASource, ADest, ASource.Size);
    end;

    { An encrypted multipart request is declared octet-stream: a server that
      parses multipart itself would drop it. DecodeBody finds the boundary. }
    if (vCipher <> nil) and (not ACompressMultipart) and
       (Pos(StringRAL(rctMULTIPARTFORMDATA), LowerCase(AContentType)) > 0) then
      AContentType := gOctetStream;
  finally
    vCipher.Free;
  end;
end;

procedure TRALParams.EncodeInto(ADest: TStream; var AContentType,
  AContentDisposition: StringRAL; ACompressMultipart: boolean);
var
  vSource: TStream;
begin
  vSource := PrepareBody(AContentType, AContentDisposition, ACompressMultipart, False);
  if vSource = nil then
    Exit;
  try
    WriteTransformed(vSource, ADest,
      EffectiveCompress(AContentType, ACompressMultipart), AContentType,
      ACompressMultipart);
  finally
    vSource.Free;
  end;
end;

function TRALParams.EncodeBody(var AContentType, AContentDisposition: StringRAL;
  ACompressMultipart: boolean): TStream;
var
  vBody: TRALBodyStream;
begin
  // a new stream the caller owns; the params stay as they are
  Result := nil;
  if Count([rpkBODY, rpkFIELD]) = 0 then
  begin
    AContentDisposition := '';
    Exit;
  end;
  vBody := TRALBodyStream.Create(-1, FSpoolAbove);
  try
    EncodeInto(vBody, AContentType, AContentDisposition, ACompressMultipart);
    Result := vBody.Detach;
  finally
    vBody.Free;
  end;
end;

function TRALParams.TakeWireStream(var AContentType, AContentDisposition: StringRAL;
  ACompressMultipart, AConsume: boolean): TStream;
var
  vSource: TStream;
  vCompress: TRALCompressType;
  vExpected: Int64RAL;
  vBody: TRALBodyStream;
begin
  Result := nil;
  vSource := PrepareBody(AContentType, AContentDisposition, ACompressMultipart, AConsume);
  if vSource = nil then
  begin
    // no body, nothing compressed: no Content-Encoding over zero bytes
    FCompressType := ctNone;
    Exit;
  end;

  vCompress := EffectiveCompress(AContentType, ACompressMultipart);
  if (vCompress = ctNone) and
     (not HasCipher(True)) then
  begin
    { nothing to transform: the body itself goes out }
    Result := vSource;
    Result.Position := 0;
    Exit;
  end;

  try
    { the cipher knows its size in advance (IV, padding, MAC); a compressor
      does not }
    vExpected := -1;
    if vCompress = ctNone then
      vExpected := vSource.Size + 64;
    vBody := TRALBodyStream.Create(vExpected, FSpoolAbove);
    try
      WriteTransformed(vSource, vBody, vCompress, AContentType, ACompressMultipart);
      Result := vBody.Detach;
    finally
      vBody.Free;
    end;
  finally
    vSource.Free;
  end;
end;

function TRALParams.TakeWireString(var AContentType, AContentDisposition: StringRAL;
  ACompressMultipart, AConsume: boolean): RawByteString;
var
  vStream: TStream;
  vSize, vDone: Int64RAL;
  vRead: IntegerRAL;
begin
  Result := '';
  vStream := TakeWireStream(AContentType, AContentDisposition, ACompressMultipart, AConsume);
  if vStream = nil then
    Exit;
  try
    { a lone text body with no transform: the string itself, not a copy }
    if vStream is TRALStringView then
    begin
      Result := TRALStringView(vStream).Text;
      Exit;
    end;
    vSize := vStream.Size;
    SetLength(Result, vSize);
    vStream.Position := 0;
    vDone := 0;
    while vDone < vSize do
    begin
      vRead := vStream.Read((PAnsiChar(Pointer(Result)) + vDone)^, vSize - vDone);
      if vRead <= 0 then
        Break;
      Inc(vDone, vRead);
    end;
    if vDone < vSize then
      SetLength(Result, vDone);
  finally
    vStream.Free;
  end;
end;

function TRALParams.SingleBody: TRALParam;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  Result := nil;
  for vInt := 0 to Pred(FParams.Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind = rpkFIELD then
    begin
      Result := nil;
      Exit;
    end
    else if vParam.Kind = rpkBODY then
    begin
      if Result <> nil then
      begin
        Result := nil;
        Exit;
      end;
      Result := vParam;
    end;
  end;
end;

function TRALParams.URLEncodedToList(ASource: StringRAL): TStringList;
begin
  Result := TStringList.Create;
  if Trim(ASource) = '' then
    Exit;

  ASource := StringReplace(ASource, '&amp;', '%26', [rfReplaceAll]);
  ASource := StringReplace(ASource, '&', HTTPLineBreak, [rfReplaceAll]);
  Result.Text := ASource;
end;

procedure TRALParams.DecodeFields(const ASource: StringRAL; AKind: TRALParamKind = rpkFIELD);
var
  vStringList: TStringList;
begin
  vStringList := URLEncodedToList(ASource);
  try
    AppendBodyParams(vStringList, AKind);
  finally
    FreeAndNil(vStringList);
  end;
end;

function TRALParams.Count: IntegerRAL;
begin
  Result := FParams.Count;
end;

function TRALParams.Count(AKind: TRALParamKind): IntegerRAL;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  Result := 0;
  for vInt := 0 to Pred(FParams.Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind = AKind then
      Result := Result + 1;
  end;
end;

function TRALParams.Count(AKinds: TRALParamKinds): IntegerRAL;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  Result := 0;
  for vInt := 0 to Pred(FParams.Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind in AKinds then
      Result := Result + 1;
  end;
end;

function TRALParams.GetCriptoOptions: TRALCriptoOptions;
begin
  if FCriptoOptions = nil then
    FCriptoOptions := TRALCriptoOptions.Create;
  Result := FCriptoOptions;
end;

function TRALParams.HasCriptoOptions: boolean;
begin
  Result := FCriptoOptions <> nil;
end;

function TRALParams.HasCipher(ATrimKey: boolean): boolean;
begin
  Result := (FCriptoOptions <> nil) and (FCriptoOptions.CriptType <> crNone);
  if Result then
  begin
    if ATrimKey then
      Result := Trim(FCriptoOptions.Key) <> ''
    else
      Result := FCriptoOptions.Key <> '';
  end;
end;

procedure TRALParams.SetCriptoOptions(const AValue: TRALCriptoOptions);
begin
  if AValue <> nil then
    RALAssignOwned(GetCriptoOptions, AValue);
end;

constructor TRALParams.Create;
begin
  inherited;
  FList := TList.Create;
  { FBuffers and FCriptoOptions are made when first needed: most requests
    never keep a buffer and never cipher }
  FBuffers := nil;
  FDecoded := nil;
  FDecodedIsParam := False;
  FSpoolAbove := 0;
  FSkipCompressedTypes := False;
  FSkipCompressTypes := nil;
  FCriptoOptions := nil;

  FCompressType := ctGZip;
  FNextParam := 0;
end;

destructor TRALParams.Destroy;
begin
  ClearParams;
  FreeAndNil(FList);
  FreeAndNil(FBuffers);
  FreeAndNil(FCriptoOptions);
  inherited;
end;

function TRALParams.GetParam(const AName: StringRAL; AKind: TRALParamKind): TRALParam;
begin
  if AName <> '' then
    Result := IndexFind(AName, ParamNameHash(AName), AKind, False)
  else
    Result := FindNameless(AKind, False);
end;

function TRALParams.GetParam(AIndex: IntegerRAL; AKind: TRALParamKind): TRALParam;
var
  vInt, vIdxParam: IntegerRAL;
  vParam: TRALParam;
begin
  Result := nil;
  vIdxParam := 0;

  for vInt := 0 to FParams.Count - 1 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if (vParam.Kind = AKind) and (vIdxParam = AIndex) then
    begin
      Result := vParam;
      Break;
    end
    else if (vParam.Kind = AKind) then
    begin
      vIdxParam := vIdxParam + 1;
    end;
  end;
end;

function TRALParams.GetBody: TList;
var
  I: IntegerRAL;
begin
  { a new list on every read, the caller's to free - see the property }
  Result := TList.Create;
  for I := 0 to Pred(FParams.Count) do
    if TRALParam(FParams.Items[I]).Kind = rpkBODY then
      Result.Add(TRALParam(FParams.Items[I]));
end;

function TRALParams.GetParam(AIndex: IntegerRAL): TRALParam;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FParams.Count) then
    Result := TRALParam(FParams.Items[AIndex]);
end;

function TRALParams.GetParam(const AName: StringRAL): TRALParam;
begin
  if AName <> '' then
    Result := IndexFind(AName, ParamNameHash(AName), rpkNONE, True)
  else
    Result := FindNameless(rpkNONE, True);
end;

function TRALParams.NewParam: TRALParam;
begin
  // room for a request's usual params in one allocation
  if FParams.Capacity < 8 then
    FParams.Capacity := 8;
  Result := TRALParam.Create;
  Result.Kind := rpkNONE;
  Result.FOwner := Self;
  if FFlushing then
    Result.FSeq := FFlushSeq // the place its line had in a pending block
  else
  begin
    Result.FSeq := FSeqNext;
    Inc(FSeqNext);
  end;
  FParams.Add(Result);
  // indexed once it has a name; the index grows with the list
  if FList.Count + FDetachedCount > Length(FBuckets) then
    IndexBuild;
end;

function TRALParams.FindNameless(AKind: TRALParamKind; AAnyKind: Boolean): TRALParam;
var
  vInt: IntegerRAL;
begin
  { a param enters the index only once it has a name, so the empty name -
    asked by nothing on the request path - walks the list }
  for vInt := 0 to FParams.Count - 1 do
  begin
    Result := TRALParam(FParams.Items[vInt]);
    if (Result.FParamName = '') and (AAnyKind or (Result.FKind = AKind)) then
      Exit;
  end;
  Result := nil;
end;

function TRALParams.FindOrNewParam(const AName: StringRAL; AKind: TRALParamKind): TRALParam;
var
  vHash: Cardinal;
begin
  { an insertion: the block goes into the list first, so the new param comes
    after its headers, as it would have }
  if FPending <> '' then
    FlushPending;
  if AName = '' then
  begin
    Result := FindNameless(AKind, False);
    if Result = nil then
      Result := NewParam;
    Exit;
  end;

  // the name is hashed once
  vHash := ParamNameHash(AName);
  Result := IndexFind(AName, vHash, AKind, False);
  { parsing a pending block: past what is older than the block, see FFlushBase.
    The chain is in creation order, so the rest of it is younger }
  if FFlushing then
    while (Result <> nil) and (Result.FSeq < FFlushBase) do
    begin
      Result := Result.FNextSame;
      while (Result <> nil) and
            ((Result.FHash <> vHash) or (Result.FKind <> AKind) or
             (not RALSameName(Result.FParamName, AName))) do
        Result := Result.FNextSame;
    end;
  if Result = nil then
  begin
    Result := NewParam;
    Result.FParamName := AName;
    IndexAdd(Result, vHash);
  end
  else
    { the same name but for the case of its letters, so the same hash and the
      same place in the index: only the text changes, to the last one given }
    Result.FParamName := AName;
end;

procedure TRALParams.IndexAdd(AParam: TRALParam; AHash: Cardinal);
var
  vIdx: IntegerRAL;
  vAt: TRALParam;
begin
  AParam.FHash := AHash;
  vIdx := AHash and Cardinal(High(FBuckets));
  { each chain is kept in the order the params were created, so the first
    param of a name is still the first one found. Chains hold about one
    param, so walking one is cheaper than keeping a second link per param }
  vAt := FBuckets[vIdx];
  if (vAt = nil) or (vAt.FSeq > AParam.FSeq) then
  begin
    AParam.FNextSame := vAt;
    FBuckets[vIdx] := AParam;
  end
  else
  begin
    while (vAt.FNextSame <> nil) and (vAt.FNextSame.FSeq < AParam.FSeq) do
      vAt := vAt.FNextSame;
    AParam.FNextSame := vAt.FNextSame;
    vAt.FNextSame := AParam;
  end;
  AParam.FIndexed := True;
end;

procedure TRALParams.IndexRemove(AParam: TRALParam);
var
  vIdx: IntegerRAL;
  vAt: TRALParam;
begin
  vIdx := AParam.FHash and Cardinal(High(FBuckets));
  vAt := FBuckets[vIdx];
  if vAt = AParam then
    FBuckets[vIdx] := AParam.FNextSame
  else
  begin
    while (vAt <> nil) and (vAt.FNextSame <> AParam) do
      vAt := vAt.FNextSame;
    if vAt <> nil then
      vAt.FNextSame := AParam.FNextSame;
  end;
  AParam.FNextSame := nil;
  AParam.FIndexed := False;
end;

procedure TRALParams.IndexBuild;
var
  vSize, vInt, vIdx: IntegerRAL;
  vParam: TRALParam;
begin
  { two buckets per param or more, so a chain stays about one long; small to
    start, since most lists are }
  vSize := 8;
  while vSize < 2 * (FList.Count + FDetachedCount) do
    vSize := vSize * 2;
  FBuckets := nil;
  SetLength(FBuckets, vSize);
  { growing is only redistributing: every indexed param keeps its hash. The
    list is in creation order - params are only ever appended - so walking it
    backwards and putting each one at the head of its chain leaves every
    chain in creation order too, with no hash and no comparison }
  for vInt := FList.Count - 1 downto 0 do
  begin
    vParam := TRALParam(FList.Items[vInt]);
    if vParam.FIndexed then
    begin
      vIdx := vParam.FHash and Cardinal(vSize - 1);
      vParam.FNextSame := FBuckets[vIdx];
      FBuckets[vIdx] := vParam;
    end;
  end;
  { the detached ones, in no list: IndexAdd keeps each chain in creation
    order. They are few, and only while a header block is pending - or being
    parsed, when the ones that joined the list already were placed above }
  vParam := FDetached;
  while vParam <> nil do
  begin
    if vParam.FDetached and vParam.FIndexed then
    begin
      vParam.FNextSame := nil;
      IndexAdd(vParam, vParam.FHash);
    end;
    vParam := vParam.FNextDetached;
  end;
end;

function TRALParams.IndexFind(const AName: StringRAL; AHash: Cardinal;
  AKind: TRALParamKind; AAnyKind: Boolean): TRALParam;
var
  vEarlier: TRALParam;
begin
  { no table only while the list has had no param since it was created or
    cleared }
  Result := nil;
  if FBuckets <> nil then
  begin
    Result := FBuckets[AHash and Cardinal(High(FBuckets))];
    while (Result <> nil) and
          ((Result.FHash <> AHash) or ((not AAnyKind) and (Result.FKind <> AKind)) or
           (not RALSameName(Result.FParamName, AName))) do
      Result := Result.FNextSame;
  end;
  { A param in the list is older than every line of a pending block (an
    insertion parses the block first): found, it is the answer. A detached one
    stands for lines of the block, and was found by the name it has now - it
    may have been renamed since - so a line of the block with that name, if
    one comes before it, is the earlier param. Not found, the block may hold
    the name }
  if (FPending <> '') and (AAnyKind or (AKind = FPendingKind)) then
  begin
    if Result = nil then
      Result := PendingMake(AName, High(Cardinal))
    else if Result.FDetached then
    begin
      vEarlier := PendingMake(AName, Result.FSeq);
      if vEarlier <> nil then
        Result := vEarlier;
    end;
  end;
end;

function TRALParams.NextParamStr: StringRAL;
begin
  FNextParam := FNextParam + 1;
  if FNextParam <= cRALParamNames then
    Result := gParamNames[FNextParam]
  else
    Result := 'ral_param' + StringRAL(IntToStr(FNextParam));
end;

function TRALParams.FindBodyNameSeparator(const ASource: StringRAL): StringRAL;
var
  vPos, vMin: IntegerRAL;
begin
  begin
    vMin := Length(ASource);
    vPos := Pos('=', ASource);
    if (vPos > 0) and (vPos <= vMin) then
      Result := '='
    else
    begin
      vPos := Pos(StringRAL(': '), ASource);
      if (vPos > 0) and (vPos <= vMin) then
        Result := ': ';
    end;
  end;
end;

function TRALParams.FindHeaderNameSeparator(const ASource: StringRAL): StringRAL;
var
  vPos, vMin: IntegerRAL;
  Engine: StringRAL;
begin
  { Decided by the data: whichever of ': ' and '=' comes first. Engines hand over
    'Name: Value' lines (Indy, Synopse) or name=value lists (fpHTTP); the engine
    name decides only a line with neither. }
  vPos := Pos(StringRAL(': '), ASource);
  vMin := Pos(StringRAL('='), ASource);

  if (vPos > 0) and ((vMin = 0) or (vPos < vMin)) then
    Result := gColonSpace
  else if vMin > 0 then
    Result := gEquals
  else
  begin
    Engine := Self.GetParam('RALEngine').AsString;
    if SameText(Engine, ENGINESYNOPSE) or SameText(Engine, ENGINEINDY) then
      Result := gColonSpace
    else
      Result := gEquals;
  end;
end;

procedure TRALParams.AppendBodyParams(ASource: TStrings; AKind: TRALParamKind);
var
  vInt: Integer;
  vSeparator: StringRAL;
begin
  if ASource.Count > 0 then
    vSeparator := FindBodyNameSeparator(ASource.Strings[0]);

  for vInt := 0 to Pred(ASource.Count) do
    AppendParamLine(ASource.Strings[vInt], vSeparator, AKind);
end;

procedure TRALParams.AppendParamLine(const ALine, ANameSeparator: StringRAL;
  AKind: TRALParamKind);
var
  vPos: IntegerRAL;
begin
  if ALine = '' then
    Exit;

  vPos := Pos(ANameSeparator, ALine);
  if vPos > 0 then
    AppendParamPair(Copy(ALine, POSINISTR, vPos - 1),
      Copy(ALine, vPos + Length(ANameSeparator), Length(ALine)), AKind);
end;

/// Position of the first ANameSeparator in the span, 0 when there is none.
function SeparatorInSpan(const ASource: StringRAL; AStart, ALen: IntegerRAL;
  const ANameSeparator: StringRAL): IntegerRAL;
var
  vSep, vInt, vSub: IntegerRAL;
begin
  Result := 0;
  vSep := Length(ANameSeparator);
  if (ALen <= 0) or (vSep = 0) then
    Exit;
  for vInt := AStart to AStart + ALen - vSep do
  begin
    vSub := 0;
    while (vSub < vSep) and (ASource[vInt + vSub] = ANameSeparator[POSINISTR + vSub]) do
      Inc(vSub);
    if vSub = vSep then
    begin
      Result := vInt;
      Exit;
    end;
  end;
end;

{ AName against ALen characters of ASource from AStart, by RALSameName's rule:
  ASCII letters without case, every other byte as it is }
function SameNameAt(const ASource: StringRAL; AStart, ALen: IntegerRAL;
  const AName: StringRAL): boolean;
var
  vInt: IntegerRAL;
  vA, vB: Byte;
begin
  Result := False;
  if ALen <> Length(AName) then
    Exit;
  for vInt := 0 to ALen - 1 do
  begin
    vA := Ord(ASource[AStart + vInt]);
    vB := Ord(AName[POSINISTR + vInt]);
    if vA <> vB then
    begin
      if (vA >= Ord('a')) and (vA <= Ord('z')) then
        Dec(vA, 32);
      if (vB >= Ord('a')) and (vB <= Ord('z')) then
        Dec(vB, 32);
      if vA <> vB then
        Exit;
    end;
  end;
  Result := True;
end;

procedure TRALParams.AppendParamSpan(const ASource: StringRAL; AStart, ALen: IntegerRAL;
  const ANameSeparator: StringRAL; AKind: TRALParamKind);
var
  vPos, vSep: IntegerRAL;
begin
  vPos := SeparatorInSpan(ASource, AStart, ALen, ANameSeparator);
  if vPos > 0 then
  begin
    vSep := Length(ANameSeparator);
    AppendParamPair(Copy(ASource, AStart, vPos - AStart),
      Copy(ASource, vPos + vSep, AStart + ALen - vPos - vSep), AKind);
  end;
end;

function TRALParams.DeferBlock(const ASource, ANameSeparator: StringRAL;
  AKind: TRALParamKind): boolean;
var
  vInt, vStart, vPos, vSep: IntegerRAL;
  vIs13: Boolean;
  vParam: TRALParam;

  { the span AppendParamsListText would parse, recorded instead; False when
    the table is full }
  function Keep(AStart, ALen: IntegerRAL): boolean;
  begin
    Result := True;
    vPos := SeparatorInSpan(ASource, AStart, ALen, ANameSeparator);
    if vPos = 0 then
      Exit; // a line with no separator adds nothing, here as there
    if FPendingCount >= PendingLinesMax then
    begin
      Result := False;
      Exit;
    end;
    with FPendingLines[FPendingCount] do
    begin
      NameStart := AStart;
      NameLen := vPos - AStart;
      ValueStart := vPos + vSep;
      ValueLen := AStart + ALen - vPos - vSep;
      Owner := nil;
    end;
    Inc(FPendingCount);
  end;

begin
  Result := False;
  { a Set-Cookie also makes a cookie param (AppendParamPair): parsed at once }
  if RALPosText('set-cookie', ASource) > 0 then
    Exit;

  vSep := Length(ANameSeparator);
  FPendingCount := 0;
  { the very line breaking of AppendParamsListText: CR and LF each end a line,
    CRLF only one, and the tail counts when it is not empty }
  vStart := POSINISTR;
  vIs13 := False;
  for vInt := POSINISTR to RALHighStr(ASource) do
  begin
    if ASource[vInt] = #13 then
    begin
      if not Keep(vStart, vInt - vStart) then
        Break;
      vIs13 := True;
      vStart := vInt + 1;
    end
    else if ASource[vInt] = #10 then
    begin
      if (not vIs13) and not Keep(vStart, vInt - vStart) then
        Break;
      vIs13 := False;
      vStart := vInt + 1;
    end
    else
      vIs13 := False;
  end;
  if FPendingCount >= PendingLinesMax then
  begin
    FPendingCount := 0;
    Exit;
  end;
  if (vStart <= RALHighStr(ASource)) and
     not Keep(vStart, RALHighStr(ASource) - vStart + 1) then
  begin
    FPendingCount := 0;
    Exit;
  end;

  { a header of the same name already in the list - the engine's RALEngine,
    if a client sent one too - would have taken the block's value: parsed at
    once, so that it does }
  for vInt := 0 to FList.Count - 1 do
  begin
    vParam := TRALParam(FList.Items[vInt]);
    if vParam.FKind <> AKind then
      Continue;
    for vPos := 0 to FPendingCount - 1 do
      if SameNameAt(ASource, FPendingLines[vPos].NameStart,
           FPendingLines[vPos].NameLen, vParam.FParamName) then
      begin
        FPendingCount := 0;
        Exit;
      end;
  end;

  Result := True;
  if FPendingCount = 0 then
    Exit; // nothing to add, as the parse would have added nothing
  FPending := ASource; // the engine's own string, with a reference: no copy
  FPendingKind := AKind;
  FPendingSeq := FSeqNext;
  Inc(FSeqNext, FPendingCount);
end;

function TRALParams.GetList: TList;
begin
  if FPending <> '' then
    FlushPending;
  Result := FList;
end;

procedure TRALParams.FlushPending;
var
  vText: StringRAL;
  vKind: TRALParamKind;
  vSeq: Cardinal;
  vCount, vInt, vKept: IntegerRAL;
  vParam, vNext, vOwner: TRALParam;
  vIndexed: array[0..PendingLinesMax - 1] of TRALParam;
begin
  { taken off first: everything below reaches FParams again }
  vText := FPending;
  vKind := FPendingKind;
  vSeq := FPendingSeq;
  vCount := FPendingCount;
  FPending := '';
  FPendingCount := 0;

  { A detached param already says what its lines said, and since then the
    application may have changed its value or its name - which, parsed at once,
    it would have done after the parse. So its lines are not parsed again, it
    only takes its place in the list, and it leaves the index meanwhile so that
    no other line merges into it under a name it was given later }
  vKept := 0;
  vParam := FDetached;
  while vParam <> nil do
  begin
    if vParam.FIndexed then
    begin
      IndexRemove(vParam);
      vIndexed[vKept] := vParam;
      Inc(vKept);
    end;
    vParam := vParam.FNextDetached;
  end;

  FFlushBase := vSeq;
  FFlushing := True;
  try
    for vInt := 0 to vCount - 1 do
    begin
      vOwner := TRALParam(FPendingLines[vInt].Owner);
      if vOwner <> nil then
      begin
        { the first line of its name is where it stands }
        if vOwner.FDetached then
        begin
          vOwner.FDetached := False;
          FList.Add(vOwner);
        end;
        Continue;
      end;
      FFlushSeq := vSeq + Cardinal(vInt);
      with FPendingLines[vInt] do
        AppendParamPair(Copy(vText, NameStart, NameLen),
          Copy(vText, ValueStart, ValueLen), vKind);
    end;
  finally
    FFlushing := False;
    { every detached param owns a line of this block and joined the list
      there; one that did not is still not lost }
    vParam := FDetached;
    FDetached := nil;
    FDetachedCount := 0;
    while vParam <> nil do
    begin
      vNext := vParam.FNextDetached;
      vParam.FNextDetached := nil;
      if vParam.FDetached then
      begin
        vParam.FDetached := False;
        FList.Add(vParam);
      end;
      vParam := vNext;
    end;
    { back in the index, each chain in creation order }
    for vInt := 0 to vKept - 1 do
      IndexAdd(vIndexed[vInt], vIndexed[vInt].FHash);
  end;
end;

function TRALParams.PendingMake(const AName: StringRAL; ABefore: Cardinal): TRALParam;
var
  vInt, vFirst, vLast, vValue: IntegerRAL;
begin
  Result := nil;
  { the param the parse would have made: placed by its first line, with the
    name as the last line writes it and the last value that was not empty -
    a repeated header merges into one param, see AppendParamPair }
  vFirst := -1;
  vLast := -1;
  vValue := -1;
  { a line that already made a param is that param's, whatever the param was
    renamed to since: asking again for the line's name finds nothing, as it
    would have after the rename }
  for vInt := 0 to FPendingCount - 1 do
    with FPendingLines[vInt] do
      if (Owner = nil) and SameNameAt(FPending, NameStart, NameLen, AName) then
      begin
        if vFirst < 0 then
          vFirst := vInt;
        vLast := vInt;
        if ValueLen > 0 then
          vValue := vInt;
      end;
  if (vFirst < 0) or (FPendingSeq + Cardinal(vFirst) >= ABefore) then
    Exit;

  Result := TRALParam.Create;
  Result.FKind := FPendingKind;
  Result.FOwner := Self;
  Result.FSeq := FPendingSeq + Cardinal(vFirst);
  Result.FDetached := True;
  Result.FNextDetached := FDetached;
  FDetached := Result;
  Inc(FDetachedCount);

  Result.FParamName := Copy(FPending, FPendingLines[vLast].NameStart,
    FPendingLines[vLast].NameLen);
  if FList.Count + FDetachedCount > Length(FBuckets) then
    IndexBuild;
  IndexAdd(Result, ParamNameHash(Result.FParamName));
  if vValue >= 0 then
    Result.AsString := Copy(FPending, FPendingLines[vValue].ValueStart,
      FPendingLines[vValue].ValueLen);
  Result.ContentType := gTextPlain;
  for vInt := vFirst to vLast do
    with FPendingLines[vInt] do
      if (Owner = nil) and SameNameAt(FPending, NameStart, NameLen, AName) then
        Owner := Result;
end;

procedure TRALParams.AppendParamPair(AName, AValue: StringRAL; AKind: TRALParamKind);
var
  vParam: TRALParam;
begin
  // a header is not URL-encoded; query, field and cookie values are
  if AKind <> rpkHEADER then
  begin
    AName := TRALHTTPCoder.DecodeURL(AName);
    AValue := TRALHTTPCoder.DecodeURL(AValue);
  end;

  vParam := FindOrNewParam(AName, AKind);
  if AValue <> '' then
    vParam.AsString := AValue;
  vParam.ContentType := gTextPlain;
  vParam.Kind := AKind;

  { the Indy and mORMot2 clients feed their response headers through here }
  if (AKind = rpkHEADER) and (AValue <> '') and RALSameName(AName, 'Set-Cookie') then
    AddSetCookie(AValue);
end;

{ A Set-Cookie is also a cookie param of its own, name and value only, on
  every engine. }
procedure TRALParams.AddSetCookie(const AValue: StringRAL);
var
  vPos: IntegerRAL;
  vPair, vName: StringRAL;
begin
  vPos := Pos(StringRAL(';'), AValue);
  if vPos > 0 then
    vPair := Copy(AValue, POSINISTR, vPos - 1)
  else
    vPair := AValue;

  vPos := Pos(StringRAL('='), vPair);
  if vPos <= 0 then
    Exit;
  vName := RALTrim(Copy(vPair, POSINISTR, vPos - 1));
  if vName <> '' then
    AddParam(vName, RALTrim(Copy(vPair, vPos + 1, Length(vPair))), rpkCOOKIE);
end;

procedure TRALParams.AddHeader(const AName, AValue: StringRAL);
begin
  AddParam(AName, AValue, rpkHEADER);
  if RALSameName(AName, 'Set-Cookie') then
    AddSetCookie(AValue);
end;

function TRALParams.NextParamInt: IntegerRAL;
begin
  FNextParam := FNextParam + 1;
  Result := FNextParam;
end;

procedure TRALParams.OnFormBodyData(Sender: TObject; AFormData: TRALMultipartFormData;
  var AFreeData: boolean);
var
  vParam: TRALParam;
begin
  vParam := NewParam;
  if AFormData.Name = '' then
    vParam.ParamName := 'ral_body' + IntToStr(NextParamInt)
  else
    vParam.ParamName := AFormData.Name;

  // the part's content moves into the param: a window over the body, or its buffer
  vParam.AdoptStream(AFormData.ReleaseBuffer);
  vParam.FileName := AFormData.FileName;

  if AFormData.ContentType <> '' then
    vParam.ContentType := AFormData.ContentType
  else
    vParam.ContentType := gTextPlain;

  if AFormData.Disposition <> '' then
    vParam.ContentDisposition := AFormData.Disposition;

  vParam.Kind := rpkBODY;

  AFreeData := True;
end;

function TRALParams.Compress(AStream: TStream): TStream;
var
  vCompress: TRALCompress;
  vClass: TRALCompressClass;
begin
  Result := nil;

  vClass := GetCompressClass(FCompressType);
  if vClass <> nil then
  begin
    vCompress := vClass.Create;
    try
      vCompress.Format := FCompressType;
      Result := vCompress.Compress(AStream);
    finally
      vCompress.Free;
    end;
  end;
end;

function TRALParams.Encrypt(AStream: TStream): TStream;
begin
  Result := TRALHashes.Encrypt(AStream, CriptoOptions.Key, CriptoOptions.CriptType);
end;

function TRALParams.Decompress(AStream: TStream): TStream;
var
  vCompress: TRALCompress;
  vClass: TRALCompressClass;
begin
  Result := nil;

  vClass := GetCompressClass(FCompressType);
  if vClass <> nil then
  begin
    vCompress := vClass.Create;
    try
      vCompress.Format := FCompressType;
      Result := vCompress.Decompress(AStream);
    finally
      vCompress.Free;
    end;
  end;
end;

function TRALParams.Decompress(const ASource: StringRAL): StringRAL;
var
  vStream, vResult: TStream;
begin
  Result := '';
  if ASource <> '' then
  begin
    vStream := StringToStream(ASource);
    try
      vStream.Position := 0;
      vResult := Decompress(vStream);
      try
        Result := StreamToString(vResult);
      finally
        vResult.Free;
      end;
    finally
      vStream.Free;
    end;
  end;
end;

function TRALParams.Decrypt(AStream: TStream): TStream;
begin
  Result := TRALHashes.Decrypt(AStream, CriptoOptions.Key, CriptoOptions.CriptType);
end;

function TRALParams.Decrypt(const ASource: StringRAL): StringRAL;
begin
  Result := TRALHashes.Decrypt(ASource, CriptoOptions.Key, CriptoOptions.CriptType);
end;

procedure TRALParams.DelParam(const AName: StringRAL; AKind: TRALParamKind);
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  for vInt := Pred(FParams.Count) downto 0 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if (vParam.Kind = AKind) and RALSameName(vParam.ParamName, AName) then
    begin
      if vParam.FIndexed then
        IndexRemove(vParam);
      vParam.Free;
      FParams.Delete(vInt);
    end;
  end;
end;

procedure TRALParams.DelParam(const AName: StringRAL);
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  for vInt := Pred(FParams.Count) downto 0 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if RALSameName(vParam.ParamName, AName) then
    begin
      if vParam.FIndexed then
        IndexRemove(vParam);
      vParam.Free;
      FParams.Delete(vInt);
    end;
  end;
end;

{ TRALParams.TEnumerator }

constructor TRALParams.TEnumerator.Create(const AArray: TRALParams);
begin
  inherited Create;
  FIndex := -1;
  FArray := AArray;
end;

function TRALParams.TEnumerator.GetCurrent: TRALParam;
begin
  Result := TRALParam(FArray.FParams[FIndex]);
end;

function TRALParams.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FArray.FParams.Count - 1;
  if Result then
    Inc(FIndex);
end;

function TRALParams.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

/// Fills the shared strings, once, in initialization.
procedure FillSharedStrings;
var
  vInt: IntegerRAL;
begin
  // on Delphi the one copy of each literal; from here on handed over by reference
  gTextPlain := rctTEXTPLAIN;
  gOctetStream := rctAPPLICATIONOCTETSTREAM;
  gRalBody := 'ral_body';
  gInline := 'inline';
  gColonSpace := ': ';
  gEquals := '=';
  for vInt := 1 to cRALParamNames do
    gParamNames[vInt] := 'ral_param' + StringRAL(IntToStr(vInt));
end;

initialization
  FillSharedStrings;

end.
