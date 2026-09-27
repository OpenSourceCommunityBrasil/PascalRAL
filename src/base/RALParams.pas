/// Unit that contains everything related to Params from either the query request
/// or response.
unit RALParams;

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  RALHashes,
  RALTypes, RALMIMETypes, RALMultipartCoder, RALTools, RALUrlCoder,
  RALCripto, RALCriptoAES, RALStream, RALCompress, RALConsts;

type
  TRALCookieSiteScope = (cssLax, cssNone, cssStrict);

  TRALCookie = record
    Name: StringRAL;
    Value: StringRAL;
    Domain: StringRAL;
    Path: StringRAL;
    Expires: TDateTime;
    /// Seconds until the cookie expires, sent as Max-Age. 0 means not set - the
    /// record starts zeroed - and anything below zero expires it at once
    /// (Max-Age=0, the way to delete a cookie). Browsers let it win over Expires
    MaxAge: Int64;
    HttpOnly: Boolean;
    SessionOnly: Boolean;
    Secure: Boolean;
    SameSite: TRALCookieSiteScope;
  end;

  { TRALParam }

  /// This is the object of all the data that is traded between request and response.
  /// each RALParam has a name, a kind and a content that can either be a text
  /// (String) or a bytearray (Stream)
  TRALParam = class
  private
    { THE VALUE LIVES IN FText WHEN IT WAS SET AS TEXT, and in FContent when it
      is a stream: a body adopted from the engine, an open file, or a typed
      binary payload. FIsText says which of the two is live; the other is empty.

      It used to be always a stream, so every header cost a heap object for its
      value on top of the TRALParam itself, and a request carries dozens of
      them. That allocation is what caps a RAL server - measured on the QUIC
      engine, where MsQuic moves a whole request for 18 us of CPU while the RAL
      pipeline around it spends 250, and where a second dispatch thread makes
      things slower instead of faster because the threads queue on the memory
      manager. A stream is now built only when somebody actually asks for one. }
    FContent: TStream;
    { False when FContent was lent (BorrowStream): a view over the engine's
      buffer or over the request's decoded body, which this param must not
      free - and which is read-only }
    FOwnsContent: Boolean;
    FText: StringRAL;
    FIsText: Boolean;
    FContentType: StringRAL;
    FContentDisposition: StringRAL;
    FContentDispositionInline: Boolean;
    FFileName: StringRAL;
    FKind: TRALParamKind;
    FParamName: StringRAL;
  protected
    function GetAsBoolean: Boolean;
    function GetAsDouble: DoubleRAL;
    function GetAsInteger: IntegerRAL;
    function GetAsInt64: Int64;
    function GetAsStream: TStream;
    function GetAsString: StringRAL;
    function GetContent: TStream;
    function GetContentDisposition: StringRAL;
    function GetContentSize: Int64RAL;
    /// The value as text, wherever it is being kept.
    function ContentText: StringRAL;
    /// Moves a text value into a stream, for the few callers that need one.
    procedure NeedStream;
    /// Drops the content: frees it when it is this param's own
    procedure FreeContent;
    procedure SetAsBoolean(const AValue: Boolean);
    procedure SetAsDouble(const AValue: DoubleRAL);
    procedure SetAsInteger(const AValue: IntegerRAL);
    procedure SetAsInt64(const AValue: Int64);
    procedure SetAsString(const AValue: StringRAL);
    procedure SetAsStream(const AValue: TStream);
    procedure SetContentDisposition(AValue: StringRAL);

    { ContentType without its parameters: 'application/x-ral-double; charset=utf-8'
      answers 'application/x-ral-double'. A lone body param travels as the HTTP
      Content-Type header, and TRALHTTPHeaderInfo.SetContentType appends the
      charset on the way, so comparing the whole string would miss every marker
      that crossed a real connection - it only ever matched in-process. }
    function MediaType: StringRAL;
    /// Writes a raw little-endian payload and stamps ContentType with AType.
    procedure SetTypedValue(const AType: StringRAL; const ABuffer; ASize: Integer);
    /// Reads a raw payload back; False when the marker or the size do not match.
    function GetTypedValue(const AType: StringRAL; var ABuffer; ASize: Integer): Boolean;
    { Reads whatever typed payload the param carries, whichever one it is.

      Every accessor goes through this instead of asking only for its own
      marker: reading an rptInt64 param with AsInteger has to convert the value,
      not fall through to the text branch, where the raw bytes would parse as 0
      and hand back silently wrong data. }
    function GetTypedVariant(out AValue: Variant): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function AsDateTime: TDateTime; overload;
    function AsDateTime(ACustomFormat: TFormatSettings): TDateTime; overload;
    function AsCurrency: Currency;
    /// The value as a number, and whether it was one: AsDouble answers 0 for
    /// both "0" and "abc". Text is read with either separator, never with the
    /// locale of the server (see RALTryStrToFloat)
    function TryAsDouble(out AValue: DoubleRAL): Boolean;
    /// AsDouble with the value to answer when the param is not a number
    function AsDoubleDef(const ADefault: DoubleRAL): DoubleRAL;

    { Typed binary writers - see the rctRAL* constants in RALMIMETypes.

      They are new methods instead of a change to AsInteger/AsDouble/..., so
      existing code keeps producing exactly the same bytes on the wire. The
      readers are the ordinary AsInteger/AsInt64/AsDouble/AsCurrency/AsBoolean/
      AsDateTime: they look at ContentType first and fall back to parsing text,
      so an old writer still talks to a new reader unchanged.

      Works with any number of params. With two or more the multipart encoder
      copies the stream verbatim and the decoder restores name and content type;
      with a single body param the value travels as the whole body and the type
      still survives in the HTTP Content-Type header - only the name is replaced
      by 'ral_body', which is how a lone body param already behaves today,
      independently of this.

      Payload is little-endian and fixed size; on big-endian FPC targets the
      bytes are swapped at both ends so the wire format is the same everywhere.
      TDate and TTime are TDateTime in Object Pascal, so SetTypedDateTime covers
      the three of them. }
    procedure SetTypedInteger(const AValue: IntegerRAL);
    procedure SetTypedInt64(const AValue: Int64RAL);
    procedure SetTypedDouble(const AValue: DoubleRAL);
    procedure SetTypedCurrency(const AValue: Currency);
    procedure SetTypedBoolean(const AValue: Boolean);
    procedure SetTypedDateTime(const AValue: TDateTime);

    /// True when this param carries a typed binary payload instead of text.
    function IsTyped: Boolean;

    procedure Clone(ASource: TRALParam);
    function IsNilOrEmpty: Boolean;
    /// Clears and assign a file to the FContent.
    procedure OpenFile(const AFileName: StringRAL);
    /// Saves FContent to the default executable location.
    procedure SaveToFile; overload;
    /// Save FContent with the given Filename.
    procedure SaveToFile(const AFileName: StringRAL); overload;
    /// Save FContent with the given Filename and the foldername.
    procedure SaveToFile(AFolderName, AFileName: StringRAL); overload;
    { Takes AStream as the content WITHOUT copying it: the param owns it from
      here on and frees it. AsStream := X copies X; this is for a stream that
      was created for the param anyway (DecodeBody's decrypted or inflated
      body), where the copy only cost memory and time }
    procedure AdoptStream(AStream: TStream);
    { Takes AStream as the content WITHOUT owning it: the lender keeps it alive
      for as long as this param lives, and nobody writes to it. How a received
      body reaches the params without a copy (TRALParams.DecodeBody) }
    procedure BorrowStream(AStream: TStream);
    { The content as a stream the CALLER owns, for the body that goes on the
      wire (TRALParams.TakeWireStream): a text value comes as a view that holds
      the string, nothing copied. With AConsume the param's own stream moves
      out (the param is left empty) and a lent one is copied, so the result
      stands on its own; without, it is a view that is valid while this param
      lives }
    function TakeContent(AConsume: Boolean): TStream;
    function SaveToStream: TStream; overload;
    procedure SaveToStream(AStream: TStream); overload;
    function Size: Int64;

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDouble: DoubleRAL read GetAsDouble write SetAsDouble;
    property AsInteger: IntegerRAL read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsStream: TStream read GetAsStream write SetAsStream;
    property AsString: StringRAL read GetAsString write SetAsString;
    { The value as a stream. Asking for it on a param that holds text BUILDS
      one - the param keeps it from then on - so read it only when a stream is
      really what is wanted; AsString costs nothing on the common case. }
    property Content: TStream read GetContent;
    property ContentDisposition: StringRAL read GetContentDisposition write SetContentDisposition;
    property ContentDispositionInline: Boolean read FContentDispositionInline write FContentDispositionInline;
    property ContentSize: Int64RAL read GetContentSize;
    property ContentType: StringRAL read FContentType write FContentType;
    property FileName: StringRAL read FFileName write FFileName;
    /// True while the value is kept as text (AsString), False when it is a stream
    property IsText: Boolean read FIsText;
    property Kind: TRALParamKind read FKind write FKind;
    /// False for a lent content (BorrowStream): read-only, freed by its lender
    property OwnsContent: Boolean read FOwnsContent;
    property ParamName: StringRAL read FParamName write FParamName;
  end;

  { TRALParams }

  /// Collection of TRALParam objects
  TRALParams = class
  public type
    /// Support enumeration of values in TRALParams.
    TEnumerator = class
    private
      FIndex: Integer;
      FArray: TRALParams;
    public
      constructor Create(const AArray: TRALParams);
      function GetCurrent: TRALParam; inline;
      function MoveNext: Boolean; inline;
      property Current: TRALParam read GetCurrent;
    end;
  private
    FCompressType: TRALCompressType;
    FContentDispositionInline: Boolean;
    FCriptoOptions: TRALCriptoOptions;
    FNextParam: IntegerRAL;
    FParams: TList;
    { the streams the body params are windows of, freed after them: the
      received body, or the buffer it was decrypted in }
    FBuffers: TList;
    FDecoded: TStream;
    { FDecoded is the content of the lone body param: whoever writes that param
      again frees it, so it is looked up before being handed out }
    FDecodedIsParam: Boolean;
    FSpoolAbove: Int64RAL;
    FSkipCompressedTypes: Boolean;
    FSkipCompressTypes: TStrings;
    /// frees FBuffers and forgets FDecoded
    procedure ClearBuffers;
    function GetDecoded: TStream;
    { a cipher on CriptoOptions, nil when there is none. Encoding ignores a key
      of blanks, as it always did; decoding only an empty one }
    function NewCipher(AEncoding: boolean): TRALCriptoAES;
    { The plain body as a stream the CALLER owns, or nil when there is none:
      the lone body param itself, the form as text, or the multipart as a
      TRALConcatStream. Nothing is copied - see TRALParam.TakeContent for
      AConsume. Sets AContentType/AContentDisposition as EncodeBody always did }
    function PrepareBody(var AContentType, AContentDisposition: StringRAL;
      ACompressMultipart, AConsume: boolean): TStream;
    { The compression the body will really get: CompressType, except none for
      a form or multipart on the request path (see EncodeBody), for a type
      already compressed (SkipCompressedTypes/SkipCompressTypes) and when the
      compressor is not linked. Writes it back to CompressType, which is where
      the caller learns what to put in Content-Encoding }
    function EffectiveCompress(const AContentType: StringRAL;
      ACompressMultipart: boolean): TRALCompressType;
    { ASource, plain, compressed and/or encrypted into ADest - straight into
      it, with the AES working in place after the compressor }
    procedure WriteTransformed(ASource, ADest: TStream; ACompress: TRALCompressType;
      var AContentType: StringRAL; ACompressMultipart: boolean);
  protected
    /// Decodes the ALine URL and adds it to the param list.
    procedure AppendParamLine(const ALine: StringRAL; const ANameSeparator: StringRAL;
      AKind: TRALParamKind);
    /// The name=value pair of a Set-Cookie header, as an rpkCOOKIE param.
    procedure AddSetCookie(const AValue: StringRAL);
    /// Compresses the input stream into a TStream.
    function Compress(AStream: TStream): TStream;
    /// Decompresses the input string into an UTF8 String.
    function Decompress(const ASource: StringRAL): StringRAL; overload;
    /// Decompresses the input stream into a TStream.
    function Decompress(AStream: TStream): TStream; overload;
    /// Decrypts the input stream into a TStream.
    function Decrypt(AStream: TStream): TStream; overload;
    /// Decrypts the input string into an UTF8 String.
    function Decrypt(const ASource: StringRAL): StringRAL; overload;
    /// Encrypts the whole class instead of each individual object.
    function Encrypt(AStream: TStream): TStream;

    /// Results either = or : if found on the input text.
    function FindHeaderNameSeparator(const ASource: StringRAL): StringRAL;
    function FindBodyNameSeparator(const ASource: StringRAL): StringRAL;
    function GetBody: TList;
    function GetParam(AIndex: IntegerRAL; AKind: TRALParamKind): TRALParam; overload;
    function GetParam(AIndex: IntegerRAL): TRALParam; overload;
    function GetParam(AName: StringRAL): TRALParam; overload;
    function GetParam(AName: StringRAL; AKind: TRALParamKind): TRALParam; overload;
    /// Moves to the next param and returns its index.
    function NextParamInt: IntegerRAL;
    /// Moves to the next param and returns its internal name.
    function NextParamStr: StringRAL;
    /// Event to be called during the processing of FormData.
    procedure OnFormBodyData(Sender: TObject; AFormData: TRALMultipartFormData;
      var AFreeData: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    /// Locate the RALParam with the given AParamName and fills it with a file from the AFileName.
    function AddFile(const AParamName: StringRAL; const AFileName: StringRAL): TRALParam; overload;
    /// Creates a new RALParam in the internal list and fills it with a file from the AFileName.
    function AddFile(const AFileName: StringRAL): TRALParam; overload;
    /// A header received from the wire. Same as AddParam with rpkHEADER, plus
    /// what every engine owes the application: a Set-Cookie also lands as an
    /// rpkCOOKIE param, so cookies a server sets read the same whatever the
    /// transport was.
    procedure AddHeader(const AName, AValue: StringRAL);
    /// AddParam is used to include a TRALParam Object into the internal list.
    /// It only replaces a param of the SAME name and kind: a query param 'loja'
    /// and an AddParam('loja', ..., rpkHEADER) end up as two params, and
    /// ParamByName answers the one added first - see ReplaceParam
    function AddParam(const AName: StringRAL; const AValue: StringRAL;
                      AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    /// Removes every param named AName, whatever its kind, and adds this one -
    /// even with an empty value. The way for a server to impose a value that
    /// ParamByName must return: one derived from the token in OnValidate, say,
    /// over whatever the client put in the query string under the same name
    function ReplaceParam(const AName: StringRAL; const AValue: StringRAL;
                          AKind: TRALParamKind = rpkNONE): TRALParam;
    /// AddParam is used to include a TRALParam Object into the internal list.
    function AddParam(const AName: StringRAL; AContent: TStream;
                      AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    { Adds a param stating how the value should travel - see TRALParamType.

        Params.AddParam('quantidade', 2.5, rpkBODY, rptDouble);
        Params.AddParam('datacoleta', Now, rpkBODY, rptDateTime);

      With rptText it behaves like the string overload, so one call site can
      switch between text and typed without changing shape. Unlike that
      overload it does NOT reject an empty value: a typed param still has a
      value when its text form would be empty. }
    function AddParam(const AName: StringRAL; const AValue: Variant;
                      AKind: TRALParamKind; AType: TRALParamType): TRALParam; overload;
    /// AddValue creates a new RALParam in the internal list and fills it with the given parameters.
    function AddValue(const AContent: StringRAL; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    /// AddValue creates a new RALParam in the internal list and fills it with the given parameters.
    function AddValue(AContent: TStream; AKind: TRALParamKind = rpkNONE): TRALParam; overload;
    /// Used to append a list of params (ASource) to the current params list.
    procedure AppendParams(ASource: TStringList; AKind: TRALParamKind); overload;
    /// Used to append a list of params (ASource) to the current params list.
    procedure AppendParams(ASource: TStrings; AKind: TRALParamKind); overload;
    /// Used to append a list of params (ASource) from the body to the current params list.
    procedure AppendBodyParams(ASource: TStrings; AKind: TRALParamKind);
    /// Used to append a list of params in a string to the current params list.
    procedure AppendParamsListText(ASource: StringRAL; AKind: TRALParamKind;
                                   ANameSeparator: StringRAL = '');
    /// Appends params based on a string 'AText'.
    procedure AppendParamsText(AText: StringRAL; AKind: TRALParamKind;
                               const ANameSeparator: StringRAL = '=';
                               const ALineSeparator: StringRAL = '&');
    /// Appends params based on the full URL given.
    procedure AppendParamsUrl(AUrlQuery: StringRAL; AKind: TRALParamKind);
    /// Appends params based on the full URL given separated by '/'.
    procedure AppendParamsUri(AFullURI, APartialURI: StringRAL; AKind: TRALParamKind);
    /// Fills the 'ADest' StringList with RALParams matching 'AKind'.
    procedure AssignParams(ADest: TStringList; AKind: TRALParamKind;
                           ASeparator: StringRAL = '='); overload;
    /// Fills the 'ADest' Strings with RALParams matching 'AKind'.
    procedure AssignParams(ADest: TStrings; AKind: TRALParamKind;
                           ASeparator: StringRAL = '='); overload;
    /// Returns an UTF8 String with RALParams matching 'AKind'.
    function AssignParamsListText(AKind: TRALParamKind;
                                  const ANameSeparator: StringRAL = '='): StringRAL;
    /// Returns an UTF8 String with RALParams matching 'AKind'. Can accept a different Line Separator than CRLF.
    function AssignParamsText(AKind: TRALParamKind; AUrlEncoded: boolean = False;
                              const ANameSeparator: StringRAL = '=';
                              const ALineSeparator: StringRAL = '&'): StringRAL;
    /// Returns an UTF8 String with RALParams matching 'AKind' using default URL separators.
    function AssignParamsUrl(AKind: TRALParamKind): StringRAL;
    /// Clears all params.
    procedure ClearParams; overload;
    /// Clears all params matching AKind.
    procedure ClearParams(AKind: TRALParamKind); overload;
    /// Returns total ammount of RALParams.
    function Count: IntegerRAL; overload;
    /// Returns total ammount of RALParams matching AKind.
    function Count(AKind: TRALParamKind): IntegerRAL; overload;
    /// Returns total ammount of RALParams matching multiple kinds.
    function Count(AKinds: TRALParamKinds): IntegerRAL; overload;
    /// Returns a TStream with the filtered Stream body contents. ASource is
    /// copied once (boCopy): the caller keeps it and may free it right after
    function DecodeBody(ASource: TStream; const AContentType: StringRAL;
                        const AContentDisposition: StringRAL = ''): TStream; overload;
    { The body the engine received, decrypted and decompressed into ONE stream
      that the body params read from - a window for each multipart part, the
      stream itself for a lone body - with no copy of ASource unless
      AOwnership is boCopy (see TRALBodyOwnership). Decrypts in place when the
      stream may be written. Returns nil; Decoded is the decoded body }
    function DecodeBody(ASource: TStream; const AContentType, AContentDisposition: StringRAL;
                        AOwnership: TRALBodyOwnership): TStream; overload;
    /// Returns a TStream with the filtered String body contents.
    function DecodeBody(const ASource, AContentType: StringRAL;
                        const AContentDisposition: StringRAL = ''): TStream; overload;
    /// Decode and append RALParams based on the ASource input.
    procedure DecodeFields(const ASource: StringRAL; AKind: TRALParamKind = rpkFIELD);
    /// Removes a RALParam matching the given AName.
    procedure DelParam(const AName: StringRAL); overload;
    /// Removes a RALParam matching the given AName and AKind.
    procedure DelParam(const AName: StringRAL; AKind: TRALParamKind); overload;
    /// Returns a TStream with all RALParams that matches 'Body' Kind.
    { ACompressMultipart False leaves a multipart body uncompressed - only the
      client request path asks for that, and the reason is written where the
      flag is read. Everything else keeps compressing as it always did. }
    function EncodeBody(var AContentType, AContentDisposition: StringRAL;
      ACompressMultipart: boolean = True): TStream;
    { The body, compressed and/or encrypted, written into ADest where it is.
      The params are left as they are }
    procedure EncodeInto(ADest: TStream; var AContentType, AContentDisposition: StringRAL;
      ACompressMultipart: boolean = True);
    { The body as it is, neither compressed nor encrypted, as a stream the
      CALLER frees - a view: valid while the params live, and nothing copied.
      nil when there is no body. What reads back what a handler answered }
    function PlainBody(var AContentType, AContentDisposition: StringRAL): TStream;
    { The body for the wire, as a stream the CALLER owns, built once: with no
      compression and no cipher it is the body itself - the lone param's
      stream moved out, a view on its text, the multipart as a
      TRALConcatStream - and nothing is copied. With AConsume the params give
      their streams away and the result stands on its own (a server response,
      which an engine may send after the response is freed); without, it is
      valid while the params live (a client request, resent on a retry) }
    function TakeWireStream(var AContentType, AContentDisposition: StringRAL;
      ACompressMultipart, AConsume: boolean): TStream;
    /// TakeWireStream as a RawByteString, for the engines that send one:
    /// a lone text body with no transform is the param's string itself
    function TakeWireString(var AContentType, AContentDisposition: StringRAL;
      ACompressMultipart, AConsume: boolean): RawByteString;
    /// Retuns the internal Enumerator type to allow for..in loops
    function GetEnumerator: TEnumerator; inline;
    /// creates and returns an empty param for a more flexible way of coding.
    function NewParam: TRALParam;
    /// converts a HTML encoded URL into a TStringList.
    function URLEncodedToList(ASource: StringRAL): TStringList;
    /// returns all the params in a comma separated UTF8string.
    function AsString: StringRAL;
    /// returns all the params in a JSON UTF8string format.
    function AsJSON: StringRAL;

    /// Grabs only the body kind of params, excluding headers and cookies.
    property Body: TList read GetBody;
    /// Grabs a param by its index on the TRALParams list.
    property Index[AIndex: IntegerRAL]: TRALParam read GetParam;
    /// Grabs a param by its index on the TRALParams list.
    property IndexKind[AIndex: IntegerRAL; AKind: TRALParamKind]: TRALParam read GetParam;
    /// Grabs a param by its name.
    property Get[AName: StringRAL]: TRALParam read GetParam;
    /// Grabs a param by its name and kind since you can have multiple kinds with same name.
    property GetKind[AName: StringRAL; AKind: TRALParamKind]: TRALParam read GetParam;
    { The body received, decoded (DecodeBody): what RequestStream on the
      server and ResponseStream on the client read. Owned by the params }
    property Decoded: TStream read GetDecoded;
    /// Bodies above this size are spooled to a temporary file (0: never)
    property SpoolAbove: Int64RAL read FSpoolAbove write FSpoolAbove;
    /// Encoding skips the built-in list of already compressed types
    property SkipCompressedTypes: Boolean read FSkipCompressedTypes write FSkipCompressedTypes;
    /// More types encoding does not compress; referenced, not owned
    property SkipCompressTypes: TStrings read FSkipCompressTypes write FSkipCompressTypes;
  published
    /// Which algorithm to compress the content of params.
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    /// Configuration of the cryptography used on params for a secure P2P traffic.
    property CriptoOptions: TRALCriptoOptions read FCriptoOptions write FCriptoOptions;
    property ContentDispositionInline: Boolean read FContentDispositionInline
      write FContentDispositionInline;
  end;

function GetCookieText(ACookie: TRALCookie): StringRAL;
function GetRALCookieFromText(ACookieString: StringRAL): TRALCookie;
function GetRALCookieFromParam(AParamName: StringRAL; AParams: TRALParams): TRALCookie;

implementation

{ TRALParam }

uses
  RALJson;

function DateTimeToCookieExpireDate(ADateTime: TDateTime): StringRAL;
const
  HTTPMonths: array[1..12] of string[3] = (
    'Jan', 'Feb', 'Mar', 'Apr',
    'May', 'Jun', 'Jul', 'Aug',
    'Sep', 'Oct', 'Nov', 'Dec');
  HTTPDays: array[1..7] of string[3] = (
    'Sun', 'Mon', 'Tue', 'Wed',
    'Thu', 'Fri', 'Sat');

  DateFormat = '"%s", dd "%s" yyyy hh:nn:ss';
  Expire     = '%s GMT';
var
  vInt: integer;
  vYear, vMonth, vDay: Word;
  vExpire, vValue : StringRAL;
  test: String;
begin
  // Dia da semana e nome do mês precisam ter a 1a letra maiúscula
  ADateTime := RALDateTimeToGMT(ADateTime);
  DecodeDate(ADateTime, vYear, vMonth, vDay);

  vExpire := FormatDateTime(DateFormat, ADateTime);
  vExpire := Format(vExpire, [HTTPDays[DayOfWeek(ADateTime)], HTTPMonths[vMonth]]);
  vExpire := Format(Expire, [vExpire]);
  Result := vExpire;
  //Result := 'Mon, 27 Jul 2026 14:00:00 GMT'
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

  { Max-Age was in the record and never written, so a cookie "deleted" with it
    stayed as a session cookie. Written as it came, or as 0 to expire now }
  if ACookie.MaxAge > 0 then
    Result := Result + '; Max-Age=' + IntToStr(ACookie.MaxAge)
  else if ACookie.MaxAge < 0 then
    Result := Result + '; Max-Age=0';

  if ACookie.Secure then
    Result := Result + '; Secure';

  if ACookie.HttpOnly then
    Result := Result + '; HttpOnly';

  case ACookie.SameSite of
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
begin
  // the record holds strings: release whatever the caller's variable still
  // referenced before zeroing it, or those strings leak
  Finalize(Result);
  FillChar(Result, SizeOf(Result), 0);

  S := StringReplace(ACookieString, '; ', ';', [rfReplaceAll]);
  Len := Length(S);
  if Len = 0 then
    Exit;

  Start := 1;
  while Start <= Len do
  begin
    // Encontra o próximo ';'
    P := Start;
    while (P <= Len) and (S[P] <> ';') do
      Inc(P);

    // Extrai o trecho atual (já sem espaço extra por causa do Replace)
    Part := Copy(S, Start, P - Start);

    // Avança para o próximo
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

    // Comparações case-sensitive como no original (pode trocar por SameText se quiser case-insensitive)
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
      Result.Expires := HTTPDateTimeToDateTime(Value)
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
      // Primeiro (e único) name=value que sobra é o cookie propriamente dito
      Result.Name  := Name;
      Result.Value := Value;
    end;
  end;
end;

function GetRALCookieFromParam(AParamName: StringRAL; AParams: TRALParams
  ): TRALCookie;
var
  vCookieStr: StringRAL;
begin
  vCookieStr := AParams.GetKind[AParamName, rpkCOOKIE].AsString;
  Result := GetRALCookieFromText(vCookieStr);
end;

procedure TRALParam.Clone(ASource: TRALParam);
begin
  ASource.ContentDispositionInline := Self.ContentDispositionInline;
  ASource.FileName := Self.FileName;
  ASource.Kind := Self.Kind;
  ASource.ParamName := Self.ParamName;

  { Content first, ContentType after: writing content drops a typed marker (see
    SetAsStream), so assigning the type before the stream would clear it again
    and a cloned typed param would come out as a plain octet-stream. The
    multipart decoder already assigns in this order. }
  if FIsText then
    ASource.AsString := FText
  else
    ASource.AsStream := FContent;
  ASource.ContentType := Self.ContentType;
end;

constructor TRALParam.Create;
begin
  inherited;
  FContent := nil;
  FOwnsContent := True;
  FText := '';
  FIsText := False;
  FContentType := rctTEXTPLAIN;
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
    FContentType := rctAPPLICATIONOCTETSTREAM;
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
    FContent := nil;
  end
  else if AConsume then
  begin
    { lent by somebody who may not outlive the caller: the one copy left }
    vBody := TRALBodyStream.Create(FContent.Size);
    try
      FContent.Position := 0;
      RALCopyStream(FContent, vBody, FContent.Size);
      Result := vBody.Detach;
    finally
      vBody.Free;
    end;
  end
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

  { A typed payload has no format to interpret, so the custom settings simply do
    not apply to it - they still drive the text fallback. }
  if GetTypedVariant(vVar) then
    Result := vVar
  else
    Result := StrToDateTimeDef(ContentText, 0, ACustomFormat);
end;


{ Typed binary payloads ------------------------------------------------------

  The wire format is little-endian and fixed size. Object Pascal targets are
  little-endian in practice, but FPC also builds for big-endian machines, so the
  bytes are swapped there on both write and read - the format on the wire never
  changes, only the in-memory representation does. }

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
  { little-endian target: the wire format already matches memory }
end;
{$IFEND}

function TRALParam.IsTyped: Boolean;
var
  vType: StringRAL;
begin
  Result := False;
  if Self = nil then
    Exit;

  { MediaType is a function and was being called SIX times - once per
    comparison - and each call redoes the Pos and the Copy. On top of that,
    SameText on Delphi converts both sides from UTF-8 to UTF-16 every call:
    twelve conversions for a param that is not typed, which is the normal case.
    And this runs in every SetAsString/SetAsStream/AdoptStream/OpenFile, that
    is, once per value received on every request }
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
  vPos: IntegerRAL;
begin
  Result := '';
  if Self = nil then
    Exit;

  Result := FContentType;
  vPos := Pos(StringRAL(';'), Result);
  if vPos > 0 then
    Result := Copy(Result, POSINISTR, vPos - 1);
end;

function TRALParam.GetTypedValue(const AType: StringRAL; var ABuffer;
  ASize: Integer): Boolean;
begin
  { Size is checked as well as the marker: a truncated or padded payload is
    treated as "not typed" and falls through to the text reader, which is the
    safe direction - better to try parsing than to hand back garbage. }
  { size before type: an integer test that discards most params without
    calling MediaType (Pos + Copy) or comparing any string }
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
  { Currency is a scaled Int64 in Object Pascal, so the raw 8 bytes round-trip
    it exactly - which text never guarantees for money. }
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
  { TDateTime is a Double; sending it raw removes the date-format ambiguity
    entirely (03/04 being March 4th or April 3rd depending on the machine). }
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
    FContent := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    FContent.Position := 0;
  end
  else
  begin
    FContent := TMemoryStream.Create;
  end;

  { Same guard as SetAsString/SetAsStream: file content must not inherit a typed
    marker from whatever the param held before, or a file that happens to be the
    right size would be read as a number. }
  if IsTyped then
    FContentType := rctAPPLICATIONOCTETSTREAM;
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

  { Any typed payload converts; only an untyped one falls back to parsing text,
    which is what keeps an old client working against a new server. }
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
  vFmt: TFormatSettings;
begin
  Result := '';
  if Self = nil then
    Exit;

  { A typed param renders as text instead of handing back its raw bytes, which
    would come out as mojibake. Rendering is invariant so whatever reads it
    afterwards - a log, generic code, another param - gets something it can
    parse back. Boolean renders as '1'/'0', which is what GetAsBoolean already
    accepts, and a date/time renders as its TDateTime number, the same shape it
    travels in. }
  if GetTypedVariant(vVar) then
  begin
    { built by hand instead of TFormatSettings.Invariant, which does not exist
      on the oldest IDEs RAL still compiles on }
    vFmt.DecimalSeparator := '.';
    vFmt.ThousandSeparator := ',';

    if VarIsType(vVar, varBoolean) then
    begin
      if vVar then
        Result := '1'
      else
        Result := '0';
    end
    else if VarIsType(vVar, varCurrency) then
      Result := StringRAL(CurrToStr(vVar, vFmt))
    else if VarIsType(vVar, varDouble) then
      Result := StringRAL(FloatToStr(Double(vVar), vFmt))
    else
      Result := StringRAL(VarToStr(vVar));
  end
  else
    Result := ContentText;
end;

function TRALParam.ContentText: StringRAL;
begin
  { a body the engine delivered as a string is read back as that string }
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
begin
  if (FFileName <> '') and (not FContentDispositionInline) then
    Result := Format('attachment; name="%s"; filename="%s"', [FParamName, FFileName])
  else
//    Result := Format('inline; name="%s"', [FParamName]);
// pode cagar o módulo web
    Result := 'inline';
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
  { a text value goes straight out of the string - building a stream for it
    first would be the allocation this class now exists to avoid }
  if FIsText then
  begin
    { Pointer(FText)^, not FText[1]: indexing the string for an untyped
      argument makes Delphi copy the whole string first when it is shared -
      and a text body always is, with the application's own variable.
      Measured: 8 MB more for an 8 MB body }
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
  Result := TRALStringStream.Create;
  SaveToStream(Result);

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
//        FreeAndNil(vMime);
      end;

      AFileName := FParamName + vExt;
    end
    else
    begin
      AFileName := FFileName;
    end;
  end;

  { the name may have come from the wire (the multipart filename, or a value
    the caller took from a param): "..\..\x" or "C:\x" must not leave the
    folder. Only the last path component survives, on either separator }
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
  { AValue may be this param's own lent content (Content): copy it before
    letting go of it }
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

  { Same reason as SetAsString: arbitrary content must not keep a typed marker
    that no longer describes it. The decoder assigns AsStream and only then sets
    ContentType, so restoring a typed param over the wire still works. }
  if IsTyped then
    FContentType := rctAPPLICATIONOCTETSTREAM;
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
    FContentType := rctAPPLICATIONOCTETSTREAM;
end;

procedure TRALParam.SetAsString(const AValue: StringRAL);
begin
  FreeContent;

  FText := AValue;
  FIsText := True;

  { Writing text over a typed param has to drop the marker, otherwise the value
    is text while ContentType still claims a binary type - and a payload that
    happens to match the expected size gets read as that type. '12345678'
    assigned over an rctRALDOUBLE param is eight bytes, so it would come back as
    6.82E-38 instead of 12345678. Only SetTypedValue may set these markers. }
  if IsTyped then
    FContentType := rctTEXTPLAIN;
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
    if RALSameName(AHeader, 'name') then
      FParamName := AValue
    else if RALSameName(AHeader, 'filename') then
      FFileName := AValue
    else
      Result := False;
  end;

begin
  AValue := Trim(AValue);
  // captura o tipo de content-disposition (inline, attachment, form-data)
  vStr := GetWord(AValue);
  // captura o primeiro param
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
  Result.ContentType := rctTEXTPLAIN;
  Result.Kind := AKind;
end;

function TRALParams.AddParam(const AName, AValue: StringRAL; AKind: TRALParamKind): TRALParam;
begin
  Result := nil;
  if (AName <> '') and (AValue <> '') then
  begin
    Result := GetKind[AName, AKind];
    if Result = nil then
      Result := NewParam;

    Result.ParamName := AName;
    Result.AsString := AValue;
    Result.ContentType := rctTEXTPLAIN;
    Result.Kind := AKind;
  end;
end;

function TRALParams.AddParam(const AName: StringRAL; const AValue: Variant;
  AKind: TRALParamKind; AType: TRALParamType): TRALParam;
begin
  Result := nil;
  if AName = '' then
    Exit;

  Result := GetKind[AName, AKind];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AName;
  Result.Kind := AKind;

  { The Variant conversions below are numeric, not textual, so no locale is
    involved on this side either. }
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
      Result.ContentType := rctTEXTPLAIN;
    end;
  end;
end;

function TRALParams.AddParam(const AName: StringRAL; AContent: TStream;
  AKind: TRALParamKind): TRALParam;
begin
  Result := GetKind[AName, AKind];
  if Result = nil then
    Result := NewParam;

  Result.ParamName := AName;
  Result.AsStream := AContent;
  Result.ContentType := rctAPPLICATIONOCTETSTREAM;
  Result.Kind := AKind;
end;

function TRALParams.AddFile(const AParamName, AFileName: StringRAL): TRALParam;
var
  vMime: TRALMIMEType;
begin
  if (AParamName <> '') and (AFileName <> '') then
  begin
    Result := GetKind[AParamName, rpkBODY];
    if Result = nil then
      Result := NewParam;

    Result.ParamName := AParamName;
    Result.FileName := ExtractFileName(AFileName);
    Result.OpenFile(AFileName);
    Result.Kind := rpkBODY;

    vMime := TRALMIMEType.GetInstance;
    try
      Result.ContentType := vMime.GetMIMEType(AFileName);
      if Result.ContentType = '' then
        Result.ContentType := rctAPPLICATIONOCTETSTREAM;
    finally
//      FreeAndNil(vMime);
    end;
  end;
end;

function TRALParams.AddFile(const AFileName: StringRAL): TRALParam;
var
  vMime: TRALMIMEType;
begin
  if AFileName <> '' then
  begin
    Result := NewParam;
    Result.ParamName := NextParamStr;
    Result.FileName := ExtractFileName(AFileName);
    Result.OpenFile(AFileName);
    Result.Kind := rpkBODY;

    vMime := TRALMIMEType.GetInstance;
    try
      Result.ContentType := vMime.GetMIMEType(AFileName);
      if Result.ContentType = '' then
        Result.ContentType := rctAPPLICATIONOCTETSTREAM;
    finally
//      FreeAndNil(vMime);
    end;
  end;
end;

function TRALParams.AddValue(const AContent: StringRAL; AKind: TRALParamKind = rpkNONE)
  : TRALParam;
begin
  Result := NewParam;
  Result.ParamName := NextParamStr;
  Result.AsString := AContent;
  Result.ContentType := rctTEXTPLAIN;
  Result.Kind := AKind;
end;

function TRALParams.AddValue(AContent: TStream; AKind: TRALParamKind = rpkNONE): TRALParam;
begin
  Result := NewParam;
  Result.ParamName := NextParamStr;
  Result.AsStream := AContent;
  Result.ContentType := rctAPPLICATIONOCTETSTREAM;
  Result.Kind := AKind;
end;

procedure TRALParams.ClearParams;
begin
  while FParams.Count > 0 do
  begin
    TObject(FParams.Items[FParams.Count - 1]).Free;
    FParams.Delete(FParams.Count - 1);
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

  { A header list holds 'Name: Value' lines, but TStrings.NameValueSeparator is
    a Char that defaults to '=' and can never be empty, so the sniffer below was
    unreachable and every header got split on the first '=' found anywhere in
    the value: 'Content-Type: multipart/form-data; boundary=ral01' came back
    named 'Content-Type: multipart/form-data; boundary'. Indy's TIdHeaderList
    does declare ': ', but on a property of its own that is invisible through
    this TStrings reference. So headers ask the sniffer; everything else - query
    params, which really are 'name=value' - keeps using NameValueSeparator. }
  if (AKind = rpkHEADER) and (ASource.Count > 0) then
    vSeparator := FindHeaderNameSeparator(ASource.Strings[0]);

  if vSeparator = '' then
    vSeparator := ASource.NameValueSeparator;

  for vInt := 0 to Pred(ASource.Count) do
    AppendParamLine(ASource.Strings[vInt], vSeparator, AKind);
end;

procedure TRALParams.AppendParamsListText(ASource: StringRAL; AKind: TRALParamKind;
  ANameSeparator: StringRAL);
var
  vInt, vStart: IntegerRAL;
  vIs13: Boolean;
begin
  {$IFDEF FPC}
    ASource := UTF8Decode(ASource);
  {$ELSE}
    ASource := UTF8ToString(ASource);
  {$ENDIF}

  if (ASource <> '') and (ANameSeparator = '') then
    ANameSeparator := FindHeaderNameSeparator(ASource);

  { The line used to be built one character at a time - "vLine := vLine +
    ASource[vInt]" - which reallocates the growing string on EVERY character.
    A two hundred byte header block is then two hundred allocations per
    request, and every allocation takes the memory manager's lock: with twenty
    threads the requests queue behind each other and adding threads stops
    adding throughput. Measured on the QUIC engine, twenty threads against one
    server: this call went from 0.151 ms to 4.330 ms per request and became 61%
    of the whole request. Now the line is delimited by index and cut once with
    Copy, so a header block costs one allocation per line instead of one per
    character. Every engine parses its response headers through here.

    The line breaking is unchanged, deliberately: CR and LF each end a line,
    CRLF ends only one, and the tail is emitted when it is not empty. }
  vStart := POSINISTR;
  vIs13 := False;
  for vInt := POSINISTR to RALHighStr(ASource) do
  begin
    if ASource[vInt] = #13 then
    begin
      AppendParamLine(Copy(ASource, vStart, vInt - vStart), ANameSeparator, AKind);
      vIs13 := True;
      vStart := vInt + 1;
    end
    else if ASource[vInt] = #10 then
    begin
      if not vIs13 then
        AppendParamLine(Copy(ASource, vStart, vInt - vStart), ANameSeparator, AKind);
      vIs13 := False;
      vStart := vInt + 1;
    end
    else
      vIs13 := False;
  end;

  if vStart <= RALHighStr(ASource) then
    AppendParamLine(Copy(ASource, vStart, MaxInt), ANameSeparator, AKind);
end;

procedure TRALParams.AppendParamsText(AText: StringRAL; AKind: TRALParamKind;
  const ANameSeparator: StringRAL; const ALineSeparator: StringRAL);
var
  vLine: StringRAL;
  vIndex: IntegerRAL;
begin
  repeat
    vIndex := Pos(ALineSeparator, AText);
    if vIndex > 0 then
      vLine := Copy(AText, POSINISTR, vIndex - 1)
    else
      vLine := AText;
    if vLine <> '' then
    begin
      AppendParamLine(vLine, ANameSeparator, AKind);
      Delete(AText, POSINISTR, vIndex);
    end
  until vIndex = 0;
end;

procedure TRALParams.AppendParamsUri(AFullURI, APartialURI: StringRAL; AKind: TRALParamKind);
var
  vInt, vIdx: IntegerRAL;
  vParam: TRALParam;
begin
  if SameText(AFullURI, APartialURI) then
    Exit;

  AFullURI := FixRoute(AFullURI);
  APartialURI := FixRoute(APartialURI);

  if Pos(LowerCase(APartialURI), LowerCase(AFullURI)) > 0 then
  begin
    Delete(AFullURI, 1, Length(APartialURI)); // removendo partialuri
    vIdx := 1;
    repeat
      vInt := Pos('/', AFullURI);
      if vInt > 0 then
      begin
        vParam := GetKind['ral_uriparam' + IntToStr(vIdx), AKind];
        if vParam = nil then
        begin
          vParam := NewParam;
          vParam.ParamName := 'ral_uriparam' + IntToStr(vIdx);
        end;
        vParam.AsString := Copy(AFullURI, 1, vInt - 1);
        vParam.Kind := AKind;

        Delete(AFullURI, 1, vInt);
        vIdx := vIdx + 1;
      end;
    until vInt = 0;
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
begin
  for vInt := 0 to Pred(FParams.Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind = AKind then
      ADest.Add(vParam.ParamName + ASeparator + vParam.AsString);
  end;
end;

function TRALParams.AssignParamsListText(AKind: TRALParamKind;
  const ANameSeparator: StringRAL): StringRAL;
begin
  Result := AssignParamsText(AKind, False, ANameSeparator, HTTPLineBreak);
end;

{ Built into a buffer that grows geometrically instead of by concatenation.
  Every "Result := Result + x" reallocates the whole string and copies it, so a
  response with eight headers reallocated two dozen times - and each
  reallocation takes the memory manager's lock, which is what turns into a
  queue once twenty threads are answering at once. Measured on the QUIC engine:
  building the response headers was 0.155 ms of a 0.433 ms request.

  The result is identical, deliberately: the line separator still goes in only
  before a param that is not the first to produce output, and the whole thing
  is still TrimRight'ed at the end. }
function TRALParams.AssignParamsText(AKind: TRALParamKind; AUrlEncoded: boolean;
  const ANameSeparator: StringRAL; const ALineSeparator: StringRAL): StringRAL;
var
  vInt: integer;
  vParam: TRALParam;
  vUsed, vCap: IntegerRAL;

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
  for vInt := 0 to Pred(Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind = AKind then
    begin
      if vUsed > 0 then
        Put(ALineSeparator);
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

function TRALParams.AsString: StringRAL;
var
  I: IntegerRAL;
begin
  Result := '';
  if (FParams <> nil) and (FParams.Count > 0) then
    for I := 0 to Pred(FParams.Count) do
    begin
      Result := Result + TRALParam(FParams.Items[I]).AsString;
      if FParams.Count > 0 then
        Result := Result + ', ';
    end;
end;

{ The content type a multipart body would have declared, rebuilt from the body
  itself: its first line is the delimiter, so the boundary is what follows the
  two dashes. Used when the header cannot carry it - an encrypted multipart
  travels declared as octet-stream, see EncodeBody. Hands back the whole header
  on purpose: the decoder sets itself up from ContentType. }
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

    { Starting with "--" is not enough: an encrypted plain text that happens to
      open with two dashes - a comment, a command line - would be torn apart as
      multipart and lost. A real multipart always ends with the closing
      delimiter, so the tail of the body has to carry it. Only the tail is
      read: the body may be large, and the close is always at the end. }
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

{ True when the stream opens with the two dashes that start a multipart
  delimiter. Enough to tell a plain multipart body from a compressed one: every
  compressor RAL uses writes a header of its own first, and none of them starts
  with "--" (deflate opens with 0x1F 0x8B). }
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
  if (FCriptoOptions.CriptType = crNone) or (FCriptoOptions.Key = '') or
     (AEncoding and (Trim(FCriptoOptions.Key) = '')) then
    Exit;
  Result := TRALCriptoAES.Create;
  case FCriptoOptions.CriptType of
    crAES128: Result.AESType := tAES128;
    crAES192: Result.AESType := tAES192;
    crAES256: Result.AESType := tAES256;
  end;
  Result.Key := FCriptoOptions.Key;
end;

{ a stream the body decoder may overwrite: not a view of someone else's memory }
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

  { the streams vCur depends on (the buffer a window was decrypted in) }
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
    while vChain.Count > 0 do
    begin
      FBuffers.Add(vChain.Items[0]);
      vChain.Delete(0);
    end;
  end;

  { vCur is replaced by a stream that depends on nothing before it }
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
  { Returns nil: the body ends up in the params, nothing else. It used to be
    copied into a fresh stream, decrypted into another, inflated into
    another and copied into the body param; since 1.3 the engine's buffer is
    read where it is (boBorrowed), the cipher works on it in place when it may
    be written, and the one decoded body is shared by the params as windows
    (.agents/PLANO_STREAM_UNICO.md) }
  Result := nil;
  vParam := nil;
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

      { A body that is ALREADY multipart is not decompressed, whatever the
        settings say. EncodeBody stopped compressing multipart (the reason is
        written there), and this side has no header to learn that from when
        both ends are two plain TRALParams in the same process - they are born
        with CompressType = gzip, so it would try to inflate a body that was
        never deflated. The bytes decide, not the header: a body that opens
        with a delimiter was never compressed, whatever Content-Encoding
        claims - an encrypted multipart travels as octet-stream, and only a
        plain one starts with the two dashes of a delimiter }
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

      { Multipart is recognised by the header OR, when it was encrypted, by
        the bytes: such a body travels declared as octet-stream, because
        announcing multipart over ciphertext makes a server that parses
        natively read the ciphertext as parts and drop everything. The header
        is gone, the boundary is not - the first line of the plaintext is the
        delimiter. }
      vCTMultipart := '';
      if Pos(rctMULTIPARTFORMDATA, LowerCase(AContentType)) > 0 then
        vCTMultipart := AContentType
      else if (FCriptoOptions.CriptType <> crNone) and StartsWithDelim(vCur) then
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
        vParam.ParamName := 'ral_body';
        vParam.FileName := '';
        vParam.ContentDisposition := AContentDisposition;

        { Content first, ContentType after - the order is load-bearing. A single
          body param travels with its own content type as the HTTP header, so
          this is what restores a typed marker on the way in; assigning the
          type before the stream would clear it again (SetAsStream drops it).
          Same ordering as TRALParam.Clone }
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
      { what was built stays with the params until they go, never freed here:
        a multipart that failed half way has already handed windows over it
        to the parts it read }
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

  { a view that holds the string: nothing copied and no code page on the way
    (a TStringStream converted on the compilers without its RawByteString
    overload, and copied on all). The params own the view }
  vStream := TRALStringView.Create(ASource);
  Result := DecodeBody(vStream, AContentType, AContentDisposition, boOwned);
end;

function TRALParams.PrepareBody(var AContentType, AContentDisposition: StringRAL;
  ACompressMultipart, AConsume: boolean): TStream;
var
  vMultPart: TRALMultipartEncoder;
  vInt1, vInt2: integer;
  vItem: TRALParam;
  vString, vValor, vFile: StringRAL;
  vFormAsMultipart: boolean;
begin
  Result := nil;

  vInt1 := Count(rpkBODY);
  vInt2 := Count(rpkFIELD);

  { An encrypted form request goes out as multipart. A server that parses
    application/x-www-form-urlencoded natively - Indy's TIdHTTPServer and
    libmicrohttpd under Sagui both do, before any RAL layer runs - reads the
    ciphertext as the form, finds no field and throws the body away: Indy frees
    PostStream, Sagui hands over an empty payload. An encrypted multipart is
    already declared as octet-stream (see WriteTransformed), so nobody parses it
    natively, and DecodeBody finds the delimiter once it has decrypted. RAL's
    cipher only ever talks to RAL, so nothing outside loses the urlencoded
    form it would have expected. }
  vFormAsMultipart := (not ACompressMultipart) and (vInt1 = 0) and (vInt2 > 0) and
    (FCriptoOptions.CriptType <> crNone) and (Trim(FCriptoOptions.Key) <> '');

  AContentDisposition := '';

  { the shortcut "the body IS this value" is for a lone rpkBODY only. It used to
    take a lone rpkFIELD too, by counting params instead of looking at their
    kind: AddField('m', json) went out as the raw JSON, with no "m=", and a
    third-party server found no field at all (a RAL server did not notice, the
    value just landed in Body). A form field is always name=value, one or many }
  if (vInt1 = 1) and (vInt2 = 0) then
  begin
    vItem := IndexKind[0, rpkBODY];

    vItem.ContentDispositionInline := FContentDispositionInline;

    if Pos(StringRAL('ral_param'), vItem.ParamName) > 0 then
      vItem.ParamName := 'ral_body';

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

        { real form encoding, name and value: '=', '%', '+', '&' and every
          byte above 127 used to go out raw (only '&' was escaped), and a
          third-party server split the fields wrong. AppendParamLine on the
          RAL side already decodes, so nothing changes between two RALs }
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
          { A BODY part with no real filename goes out named after itself; a
            FIELD part goes out as a plain form field.

            Why the body parts must be named: libmicrohttpd, under the Sagui
            engine, routes any multipart body through its upload machinery and
            materialises only the parts that name a file - the rest vanish in
            silence, no field, no upload, no payload. Body parts are RAL's own
            envelope (typed params, "SQL", "ParamCount", "N0"), so naming them
            costs nothing outside: no foreign server could read them anyway.

            Why the field parts must NOT be named: a server that is not RAL
            files a named part under uploads instead of fields - PHP's $_FILES,
            Spring's MultipartFile, Go's MultipartForm.File. A RAL client
            posting an ordinary form, or a login, to such a server would have
            its fields land in the wrong place.

            Measured, not assumed: a field mixed with a named body part still
            reaches a Sagui server - once the library is processing the
            multipart it hands the unnamed part over as a field. What it drops
            is a multipart made ONLY of unnamed parts, and that never reaches
            it: fields on their own travel urlencoded, a body part is always
            named, and the one fields-only multipart - an encrypted form - is
            declared octet-stream, which libmicrohttpd leaves alone. }
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

  { A multipart body goes out uncompressed, on purpose.

    Compressing it leaves the header saying "multipart/form-data" while the
    bytes are gzip. That is legal HTTP - Content-Encoding describes a transform
    over the declared type - but it only works against a server that
    decompresses before parsing the parts. RAL's own engines do; servers that
    parse multipart natively do not, and libmicrohttpd under the Sagui engine is
    one of those: it read the gzip bytes as parts, found none, and dropped the
    whole body without an error.

    Little is lost by not compressing it. What makes a multipart body here are
    typed params of a few bytes and files that usually arrive compressed
    already, while the response - where the volume actually is - still
    compresses normally.

    A urlencoded form request goes out uncompressed for the same reason: Indy
    and libmicrohttpd parse it before anything decompresses, read gzip bytes
    as the form and lose every field - a lone AddField included, which before
    reached a RAL server's Body because it travelled as a raw body. }
  if (not ACompressMultipart) and (Result <> ctNone) and
     ((Pos(StringRAL(rctMULTIPARTFORMDATA), LowerCase(AContentType)) > 0) or
      (Pos(StringRAL(rctAPPLICATIONXWWWFORMURLENCODED), LowerCase(AContentType)) > 0)) then
    Result := ctNone;

  { already compressed (a JPEG, a zip): compressing again gains nothing but CPU
    and a second buffer - see RALIsCompressedType }
  if (Result <> ctNone) and (FSkipCompressedTypes or (FSkipCompressTypes <> nil)) and
     RALIsCompressedType(AContentType, FSkipCompressedTypes, FSkipCompressTypes) then
    Result := ctNone;

  { a compressor that is not linked used to make the whole body vanish (nil
    out of Compress, and EncodeBody handed nil on): it goes uncompressed, and
    the header says so }
  if (Result <> ctNone) and (GetCompressClass(Result) = nil) then
    Result := ctNone;

  { and the caller hears about it through CompressType: whoever fills
    Content-Encoding reads it back from here, and a header promising gzip over
    bytes that were never compressed makes the other side fail to inflate }
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

    { Ciphertext is not multipart in any sense a parser can use, so it stops
      saying it is. Announcing multipart over ciphertext made libmicrohttpd,
      under the Sagui engine, hand the ciphertext to its multipart machinery,
      find no parts and drop the body - which is how the AES transport lost
      every param. Nothing goes with the header: DecodeBody reads the boundary
      back from the first line of the plaintext, which IS the delimiter.

      Only on the request path, where the far end may parse on its own. }
    if (vCipher <> nil) and (not ACompressMultipart) and
       (Pos(StringRAL(rctMULTIPARTFORMDATA), LowerCase(AContentType)) > 0) then
      AContentType := rctAPPLICATIONOCTETSTREAM;
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
  { a new stream the caller owns, as it always was; the params are left as they
    are. Nothing is copied on the way but the body into the result }
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
    { no body, so nothing was compressed: a Content-Encoding over zero bytes
      is a lie some clients choke on }
    FCompressType := ctNone;
    Exit;
  end;

  vCompress := EffectiveCompress(AContentType, ACompressMultipart);
  if (vCompress = ctNone) and
     ((FCriptoOptions.CriptType = crNone) or (Trim(FCriptoOptions.Key) = '')) then
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

constructor TRALParams.Create;
begin
  inherited;
  FParams := TList.Create;
  FBuffers := TList.Create;
  FDecoded := nil;
  FDecodedIsParam := False;
  FSpoolAbove := 0;
  FSkipCompressedTypes := False;
  FSkipCompressTypes := nil;
  FCriptoOptions := TRALCriptoOptions.Create;

  FCompressType := ctGZip;
  FNextParam := 0;
end;

destructor TRALParams.Destroy;
begin
  ClearParams;
  FreeAndNil(FParams);
  FreeAndNil(FBuffers);
  FreeAndNil(FCriptoOptions);
  inherited;
end;

function TRALParams.GetParam(AName: StringRAL; AKind: TRALParamKind): TRALParam;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  Result := nil;

  for vInt := 0 to FParams.Count - 1 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    { Kind first, which is an enum, and only then the name: this lookup runs
      once per param inserted, and SameText on Delphi converts both sides from
      UTF-8 to UTF-16 every call - two heap allocations per comparison }
    if (vParam.Kind = AKind) and RALSameName(vParam.ParamName, AName) then
    begin
      Result := vParam;
      Break;
    end;
  end;
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

function TRALParams.GetParam(AName: StringRAL): TRALParam;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  Result := nil;

  for vInt := 0 to FParams.Count - 1 do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if RALSameName(vParam.ParamName, AName) then
    begin
      Result := vParam;
      Break;
    end;
  end;
end;

function TRALParams.NewParam: TRALParam;
begin
  Result := TRALParam.Create;
  Result.Kind := rpkNONE;
  FParams.Add(Result);
end;

function TRALParams.NextParamStr: StringRAL;
begin
  FNextParam := FNextParam + 1;
  Result := 'ral_param' + IntToStr(FNextParam);
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
  { Decide from the data, not from the engine name.

    Engines do not agree on the shape of the list they hand over: Indy and
    Synopse pass real header lines ('Name: Value'), while fpHTTP passes its
    TRequest.CustomHeaders, which is a name=value list. Keying off the engine
    got both wrong at different times - '=' chopped Indy's headers at whatever
    equals sign sat inside the value (that is how 'Content-Encription' went
    missing and encrypted bodies reached the multipart decoder undecrypted),
    and ':' matched nothing at all in fpHTTP's list, silently dropping every
    header.

    Whichever of ': ' and '=' comes FIRST in the line is the separator, which
    settles both: 'Content-Type: multipart/form-data; boundary=ral01' splits at
    the colon, and 'Host=127.0.0.1:18921' splits at the equals. The engine table
    below only decides when the line carries neither. }
  vPos := Pos(StringRAL(': '), ASource);
  vMin := Pos(StringRAL('='), ASource);

  if (vPos > 0) and ((vMin = 0) or (vPos < vMin)) then
    Result := ': '
  else if vMin > 0 then
    Result := '='
  else
  begin
    Engine := Self.GetParam('RALEngine').AsString;
    if SameText(Engine, ENGINESYNOPSE) or SameText(Engine, ENGINEINDY) then
      Result := ': '
    else
      Result := '=';
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
  vName, vValue: StringRAL;
  vParam: TRALParam;
begin
  if ALine = '' then
    Exit;

  vPos := Pos(ANameSeparator, ALine);
  if vPos > 0 then
  begin
    vName := Copy(ALine, POSINISTR, vPos - 1);
    vName := TRALHTTPCoder.DecodeURL(vName);

    vValue := Copy(ALine, vPos + Length(ANameSeparator), Length(ALine));
    vValue := TRALHTTPCoder.DecodeURL(vValue);

    vParam := GetKind[vName, AKind];
    if vParam = nil then
      vParam := NewParam;
    vParam.ParamName := vName;
    if vValue <> '' then
      vParam.AsString := vValue;
    vParam.ContentType := rctTEXTPLAIN;
    vParam.Kind := AKind;

    { the Indy and mORMot2 clients feed their response headers through here }
    if (AKind = rpkHEADER) and (vValue <> '') and RALSameName(vName, 'Set-Cookie') then
      AddSetCookie(vValue);
  end;
end;

{ ONE RULE FOR EVERY ENGINE: a Set-Cookie the server sent is also a cookie
  param of the response - name and value only, the attributes after the first
  ';' are the browser's business. netHTTP, fpHTTP and OkHttp each did this in
  their own way while Indy, mORMot2 and MsQuic did not, so whether an
  application could read a cookie the server set depended on the transport.
  It also keeps several cookies alive: AddParam replaces a param by name and
  kind, so as headers alone only the LAST Set-Cookie of an answer survived. }
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

  { the part's content moves into the param: a window over the body when the
    decoder was told to slice, the part's own buffer otherwise. It used to be
    copied here, after the decoder had already copied it once }
  vParam.AdoptStream(AFormData.ReleaseBuffer);
  vParam.FileName := AFormData.FileName;

  if AFormData.ContentType <> '' then
    vParam.ContentType := AFormData.ContentType
  else
    vParam.ContentType := rctTEXTPLAIN;

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
//var
//  vCript: TRALCripto;
begin
  Result := TRALHashes.Encrypt(AStream, FCriptoOptions.Key, FCriptoOptions.CriptType);
//  Result := nil;
//  case FCriptoOptions.CriptType of
//    crAES128:
//    begin
//      vCript := TRALCriptoAES.Create;
//      TRALCriptoAES(vCript).AESType := tAES128;
//    end;
//    crAES192:
//    begin
//      vCript := TRALCriptoAES.Create;
//      TRALCriptoAES(vCript).AESType := tAES192;
//    end;
//    crAES256:
//    begin
//      vCript := TRALCriptoAES.Create;
//      TRALCriptoAES(vCript).AESType := tAES256;
//    end;
//  end;
//
//  try
//    vCript.Key := FCriptoOptions.Key;
//    Result := vCript.EncryptAsStream(AStream);
//  finally
//    FreeAndNil(vCript);
//  end;
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
  // the test used to read Result, which had just been emptied: the string
  // overload never decompressed anything
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
//var
//  vCript: TRALCripto;
begin
  Result := TRALHashes.Decrypt(AStream, FCriptoOptions.Key, FCriptoOptions.CriptType);
//  case FCriptoOptions.CriptType of
//    crAES128:
//    begin
//      vCript := TRALCriptoAES.Create;
//      TRALCriptoAES(vCript).AESType := tAES128;
//    end;
//    crAES192:
//    begin
//      vCript := TRALCriptoAES.Create;
//      TRALCriptoAES(vCript).AESType := tAES192;
//    end;
//    crAES256:
//    begin
//      vCript := TRALCriptoAES.Create;
//      TRALCriptoAES(vCript).AESType := tAES256;
//    end;
//  end;
//
//  try
//    vCript.Key := FCriptoOptions.Key;
//    Result := vCript.DecryptAsStream(AStream);
//  finally
//    FreeAndNil(vCript);
//  end;
end;

function TRALParams.Decrypt(const ASource: StringRAL): StringRAL;
//var
//  vCript: TRALCripto;
begin
  Result := TRALHashes.Decrypt(ASource, FCriptoOptions.Key, FCriptoOptions.CriptType);
//  case FCriptoOptions.CriptType of
//    crAES128:
//    begin
//      vCript := TRALCriptoAES.Create;
//      TRALCriptoAES(vCript).AESType := tAES128;
//    end;
//    crAES192:
//    begin
//      vCript := TRALCriptoAES.Create;
//      TRALCriptoAES(vCript).AESType := tAES192;
//    end;
//    crAES256:
//    begin
//      vCript := TRALCriptoAES.Create;
//      TRALCriptoAES(vCript).AESType := tAES256;
//    end;
//  end;
//
//  try
//    vCript.Key := FCriptoOptions.Key;
//    Result := vCript.Decrypt(ASource);
//  finally
//    FreeAndNil(vCript);
//  end;
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

end.
