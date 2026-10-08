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
  { The cookie's SameSite. cssDefault, first so that a zeroed record has it,
    writes no attribute and leaves the browser's own rule - Lax on Chromium,
    None on Firefox and Safari. cssLax used to be the first, and so the value
    a record started with, which is why it wrote nothing: asking for Lax
    explicitly sent nothing either }
  TRALCookieSiteScope = (cssDefault, cssLax, cssNone, cssStrict);

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

  TRALParams = class;

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
    { the list this param is in, and its place in that list's name index:
      the order it was created in, the hash of its name, the next param of
      its bucket and whether it is in the index at all - see
      TRALParams.IndexAdd }
    FOwner: TRALParams;
    FSeq: Cardinal;
    FHash: Cardinal;
    FNextSame: TRALParam;
    FIndexed: Boolean;
    { made from a header block that is still text (TRALParams.FPending) and
      not in the list yet: it joins the list, in its place, when the block is
      parsed. A list chains its detached params through FNextDetached }
    FDetached: Boolean;
    FNextDetached: TRALParam;
    procedure SetParamName(const AValue: StringRAL);
  protected
    function GetAsBoolean: Boolean;
    function GetAsDouble: DoubleRAL;
    function GetAsInteger: IntegerRAL;
    function GetAsInt64: Int64;
    { deprecated through the getter, which is what FPC warns about where the
      property is read - and only there, not where it is written. Delphi
      refuses the directive on a property and only warns about a getter in
      the unit that declares it, so it gets the doc comment alone }
    function GetAsStream: TStream;
      {$IFDEF FPC}deprecated 'AsStream builds a copy the caller must free: read Content, or call SaveToStream';{$ENDIF}
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
    /// Writing copies the stream into the param. READING builds a new copy of
    /// the whole value on every read, which the caller has to free - not the
    /// param's own stream, despite the name. Deprecated for reading: Content
    /// is the param's stream, and SaveToStream says that it makes a copy
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
    property ParamName: StringRAL read FParamName write SetParamName;
  end;

  /// One line of a header block kept as text (TRALParams.FPending): where its
  /// name and its value are
  TRALPendingLine = record
    NameStart: IntegerRAL;
    NameLen: IntegerRAL;
    ValueStart: IntegerRAL;
    ValueLen: IntegerRAL;
    /// The param a lookup made from this line (and the other lines of its
    /// name), or nil. A TRALParam - declared as TObject, the class comes later
    Owner: TObject;
  end;

  { TRALParams }

  /// Collection of TRALParam objects
  TRALParams = class
  public const
    /// The most lines a header block may have to be kept as text; a longer one
    /// is parsed at once. Browsers send 15 to 21
    PendingLinesMax = 32;
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
    FBodyError: StringRAL;
    FCompressType: TRALCompressType;
    FContentDispositionInline: Boolean;
    FCriptoOptions: TRALCriptoOptions;
    FNextParam: IntegerRAL;
    { the params, in the order they were created. Read through FParams, which
      parses a pending header block first - see GetList }
    FList: TList;
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
    { The name index. A lookup used to walk the whole list, and every param
      parsed off the wire is looked up first - the query string, a form, the
      headers, the cookies - so N params cost N*N/2 comparisons, before any
      authentication: 50 000 fields in a 400 KB form made a billion. Built
      with the first param and doubled when it fills up, so a name is looked
      up one way whatever the size of the list }
    FBuckets: array of TRALParam;
    FSeqNext: Cardinal;
    { A block of header lines kept as text until something needs the list
      (AppendParamsListText). A server engine hands over every header the
      client sent, and most are never read: each one parsed is a param, its
      name and its value, three allocations - a third of all a GET made. A
      lookup by name answers from the lines and makes that one param alone,
      at the place in the order it would have had: FPendingSeq is the creation
      order reserved for the first line. Anything else parses the whole block
      first, through FParams }
    FPending: StringRAL;
    FPendingKind: TRALParamKind;
    FPendingSeq: Cardinal;
    FPendingCount: IntegerRAL;
    FPendingLines: array[0..PendingLinesMax - 1] of TRALPendingLine;
    FDetached: TRALParam;
    FDetachedCount: IntegerRAL;
    { while the block is parsed: NewParam takes FFlushSeq, the order reserved
      for the line being parsed, instead of the next one }
    FFlushing: Boolean;
    FFlushSeq: Cardinal;
    { the order reserved for the block being parsed: a line merges only into a
      param of the block itself - one older than the block got its name by a
      rename made after the block arrived, which the parse at once would have
      come before }
    FFlushBase: Cardinal;
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
    procedure IndexAdd(AParam: TRALParam; AHash: Cardinal);
    procedure IndexBuild;
    function IndexFind(const AName: StringRAL; AHash: Cardinal; AKind: TRALParamKind;
      AAnyKind: Boolean): TRALParam;
    procedure IndexRemove(AParam: TRALParam);
    /// The first param with no name - of that kind, unless AAnyKind.
    function FindNameless(AKind: TRALParamKind; AAnyKind: Boolean): TRALParam;
    /// The param of that name and kind, created when there is none, with its
    /// name hashed once.
    function FindOrNewParam(const AName: StringRAL; AKind: TRALParamKind): TRALParam;
    /// Decodes a name and a value already cut apart and stores them.
    procedure AppendParamPair(AName, AValue: StringRAL; AKind: TRALParamKind);
    /// Keeps a header block as text instead of parsing it, when it can be:
    /// ASCII already checked, at most PendingLinesMax lines, no Set-Cookie
    /// (which also makes cookie params) and no name of a param of that kind
    /// already in the list (parsing would have merged into it)
    function DeferBlock(const ASource, ANameSeparator: StringRAL;
      AKind: TRALParamKind): boolean;
    /// Parses the pending block into the list, each param at its reserved place
    procedure FlushPending;
    /// The list, with the pending block parsed into it
    function GetList: TList;
    /// The param of AName made from the pending block, or nil when the block
    /// has no such line - or the first one comes at or after the creation order
    /// ABefore. Detached: in the index, not in the list yet
    function PendingMake(const AName: StringRAL; ABefore: Cardinal): TRALParam;
    /// Every access to the list goes through here: no method sees a list
    /// without the headers of a pending block
    property FParams: TList read GetList;
  protected
    /// Decodes the ALine URL and adds it to the param list.
    procedure AppendParamLine(const ALine: StringRAL; const ANameSeparator: StringRAL;
      AKind: TRALParamKind);
    procedure AppendParamSpan(const ASource: StringRAL; AStart, ALen: IntegerRAL;
      const ANameSeparator: StringRAL; AKind: TRALParamKind);
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
    { deprecated the way TRALParam.GetAsStream is - see there }
    function GetBody: TList;
      {$IFDEF FPC}deprecated 'Body builds a list the caller must free: read Count(rpkBODY), IndexKind or SingleBody';{$ENDIF}
    function GetParam(AIndex: IntegerRAL; AKind: TRALParamKind): TRALParam; overload;
    function GetParam(AIndex: IntegerRAL): TRALParam; overload;
    function GetParam(const AName: StringRAL): TRALParam; overload;
    function GetParam(const AName: StringRAL; AKind: TRALParamKind): TRALParam; overload;
    /// Moves to the next param and returns its index.
    function NextParamInt: IntegerRAL;
    /// Moves to the next param and returns its internal name.
    function NextParamStr: StringRAL;
    function GetCriptoOptions: TRALCriptoOptions;
    procedure SetCriptoOptions(const AValue: TRALCriptoOptions);
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
    /// Fills the 'ADest' Strings with RALParams matching 'AKind'. Headers and
    /// cookies come out as RALSafeHeaderText leaves them: no CR, LF or NUL.
    procedure AssignParams(ADest: TStrings; AKind: TRALParamKind;
                           ASeparator: StringRAL = '='); overload;
    /// Returns an UTF8 String with RALParams matching 'AKind'.
    function AssignParamsListText(AKind: TRALParamKind;
                                  const ANameSeparator: StringRAL = '='): StringRAL;
    /// Returns an UTF8 String with RALParams matching 'AKind'. Can accept a different Line Separator than CRLF.
    /// Headers and cookies come out as RALSafeHeaderText leaves them, unless URL-encoded.
    function AssignParamsText(AKind: TRALParamKind; AUrlEncoded: boolean = False;
                              const ANameSeparator: StringRAL = '=';
                              const ALineSeparator: StringRAL = '&'): StringRAL;
    /// Returns an UTF8 String with RALParams matching 'AKind' using default URL separators.
    function AssignParamsUrl(AKind: TRALParamKind): StringRAL;
    /// The value of a request's Cookie header: every rpkCOOKIE param as
    /// name=value, joined by '; ', as RALSafeHeaderText leaves it. A param
    /// named Set-Cookie holds a whole cookie line (what AddCookie(TRALCookie)
    /// of a response stores) and gives its name=value pair alone - a Cookie
    /// header carries no attributes. Every client engine builds its header here.
    function CookieHeaderText: StringRAL; overload;
    /// The same, merged with AJarCookies - the Cookie value an engine's own
    /// jar holds for the URL - into the one header a request may carry: the
    /// jar's pairs first, then the params, and for a name both have, the
    /// param's value alone. The application's cookie wins for that request; the
    /// jar is not touched.
    function CookieHeaderText(const AJarCookies: StringRAL): StringRAL; overload;
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
    /// The body param when the body is that one value - one rpkBODY and no
    /// rpkFIELD, which EncodeBody sends as the whole body - or nil
    function SingleBody: TRALParam;
    /// creates and returns an empty param for a more flexible way of coding.
    function NewParam: TRALParam;
    /// converts a HTML encoded URL into a TStringList.
    function URLEncodedToList(ASource: StringRAL): TStringList;
    /// returns all the params in a comma separated UTF8string.
    function AsString: StringRAL;
    /// returns all the params in a JSON UTF8string format.
    function AsJSON: StringRAL;

    /// Why the last DecodeBody could not take the body apart, '' when it
    /// could: a multipart that yields no part and is not an empty form, the
    /// body dropped. TRALServer answers such a request 400 before any route
    property BodyError: StringRAL read FBodyError;
    /// A NEW list of the rpkBODY params, which the caller has to free - every
    /// read builds another, so 'if Params.Body.Count > 0' leaks one. Count(rpkBODY),
    /// IndexKind[i, rpkBODY] and SingleBody read the same without allocating
    property Body: TList read GetBody;
    /// Grabs a param by its index on the TRALParams list.
    property Index[AIndex: IntegerRAL]: TRALParam read GetParam;
    /// Grabs a param by its index on the TRALParams list.
    property IndexKind[AIndex: IntegerRAL; AKind: TRALParamKind]: TRALParam read GetParam;
    /// Grabs a param by its name.
    property Get[const AName: StringRAL]: TRALParam read GetParam;
    /// Grabs a param by its name and kind since you can have multiple kinds with same name.
    property GetKind[const AName: StringRAL; AKind: TRALParamKind]: TRALParam read GetParam;
    { The body received, decoded (DecodeBody): what RequestStream on the
      server and ResponseStream on the client read. Owned by the params }
    property Decoded: TStream read GetDecoded;
    /// Bodies above this size are spooled to a temporary file (0: never)
    property SpoolAbove: Int64RAL read FSpoolAbove write FSpoolAbove;
    /// Encoding skips the built-in list of already compressed types
    property SkipCompressedTypes: Boolean read FSkipCompressedTypes write FSkipCompressedTypes;
    /// More types encoding does not compress; referenced, not owned
    property SkipCompressTypes: TStrings read FSkipCompressTypes write FSkipCompressTypes;
    /// Whether CriptoOptions exists yet - asking for it would create it
    function HasCriptoOptions: boolean;
    /// A cipher with a key is set, without creating CriptoOptions to find out.
    /// ATrimKey: a key of blanks counts as none
    function HasCipher(ATrimKey: boolean): boolean;
  published
    /// Which algorithm to compress the content of params.
    property CompressType: TRALCompressType read FCompressType write FCompressType;
    /// Configuration of the cryptography used on params for a secure P2P
    /// traffic. Created the first time it is asked for: a request or a
    /// response with no cipher never makes one
    property CriptoOptions: TRALCriptoOptions read GetCriptoOptions write SetCriptoOptions;
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

const
  cRALParamNames = 32;

var
  { Strings every param and every request write, kept here with a reference
    count. Delphi copies a literal to the heap each time one is assigned to a
    field or a result - a param cost an allocation for its 'text/plain', a
    body one more for 'ral_body', and naming it 'ral_param1' three, through
    UTF-16 - while handing one of these over only counts a reference. FPC never
    copies a literal, so there they change nothing }
  gTextPlain: StringRAL;
  gOctetStream: StringRAL;
  gRalBody: StringRAL;
  gInline: StringRAL;
  gColonSpace: StringRAL;
  gEquals: StringRAL;
  gParamNames: array[1..cRALParamNames] of StringRAL;

{ FNV-1a of a name, every byte OR $20 first: 'A'..'Z' land on 'a'..'z', so
  two names RALSameName calls equal always hash alike - a few other pairs
  collide too, which costs one comparison and nothing else. No branch per
  byte, and 32-bit arithmetic that wraps on purpose: overflow and range
  checks are off for this function alone, whatever the project chose }
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

function DateTimeToCookieExpireDate(ADateTime: TDateTime): StringRAL;
begin
  { RALHTTPDate and not FormatDateTime: the RTL writes the locale's time
    separator where ':' stands, and an Expires such as 14.00.00 is invalid -
    the browser keeps the cookie for the session only }
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
    begin
      { a date that does not parse is ignored (RFC 6265 5.2.1), not the end of
        the whole cookie - nor of the response the cookie was going out with.
        The line carries UTC and Expires is local time, as GetCookieText
        writes it: read, it came back as the UTC value taken for local, hours
        off by the zone - and a cookie copied from one answer to another
        moved by that much each time }
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
      // Primeiro (e único) name=value que sobra é o cookie propriamente dito
      Result.Name  := Name;
      Result.Value := Value;
    end;
  end;
end;

{ the name=value pair of a cookie line: what comes before the first ';' }
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

{ A cookie is kept in one of two shapes. One that arrived - the Cookie header
  of a request, a Set-Cookie on the client - is a param named after it holding
  its value, attributes gone. One AddCookie(TRALCookie) set on a response is a
  param named Set-Cookie holding the whole line. This read only the second
  shape, by the cookie's name, so it parsed a bare value and answered an empty
  Name and Value for every cookie received - and raised on nil for a cookie
  that was not there. }
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

procedure TRALParam.SetParamName(const AValue: StringRAL);
begin
  { the index of the list holding it keys on the name, and a param enters it
    only once it has one. A param in a list always has the index there:
    NewParam builds it before handing the param out }
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
    { a file goes as it is and the param keeps an unopened twin, so whoever
      reads it afterwards (OnResponse, a log) still finds the file }
    if Result is TRALFileStream then
      FContent := TRALFileStream(Result).Twin
    else
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
  vIni, vFim: IntegerRAL;
begin
  Result := '';
  if Self = nil then
    Exit;

  { the type up to its parameters, without the blanks around it: RFC 9110
    lets OWS stand before the ';' and around the field value, and the fpHTTP
    client handed the value over with the blank after the colon - a typed
    answer lost its marker and its date read as zero. Offsets count from 0 }
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
    { opened now, as it always was - a file that cannot be read raises here,
      in the caller's hands - but shared with writers: fmShareDenyWrite held
      off a new version of the file until the answer had gone out. And a
      TRALFileStream, which EncodeBody hands to the engine as it is, where the
      whole file used to be copied into memory first }
    FContent := TRALFileStream.Create(string(AFileName), 0, -1, True);
  end
  else
  begin
    FContent := TMemoryStream.Create;
  end;

  { Same guard as SetAsString/SetAsStream: file content must not inherit a typed
    marker from whatever the param held before, or a file that happens to be the
    right size would be read as a number. }
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
var
  vName: StringRAL;
  vInt: IntegerRAL;
begin
  { the file name inside a quoted string: a quote or a backslash in it ended
    the string early or escaped what followed. Advice to a browser saving the
    file, so the two simply go }
  vName := FFileName;
  vInt := POSINISTR;
  while vInt <= RALHighStr(vName) do
    if (vName[vInt] = '"') or (vName[vInt] = '\') then
      Delete(vName, vInt - POSINISTR + 1, 1) // Delete counts from 1 everywhere
    else
      Inc(vInt);

  { inline keeps no name= - a lone body param travels without its name, see
    CLAUDE.md - but it does say the file name it is served under: a browser
    saving the page fell back on the URL's }
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
  { the copy made in one go - see TRALStringStream.WriteStream - where it
    used to grow the new stream piece by piece }
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

  { Writing text over a typed param has to drop the marker, otherwise the value
    is text while ContentType still claims a binary type - and a payload that
    happens to match the expected size gets read as that type. '12345678'
    assigned over an rctRALDOUBLE param is eight bytes, so it would come back as
    6.82E-38 instead of 12345678. Only SetTypedValue may set these markers. }
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
    { through the setter: written straight into the field, the name changed
      behind the index of the list, and the param was no longer found by it }
    if RALSameName(AHeader, 'name') then
      ParamName := AValue
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
  // nil, not whatever the stack held, when there is nothing to add
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

{ True when every byte of AText is below 128 }
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
  { the whole block went to UTF-16 and back before it was even read - two
    copies of every header of every request on mORMot2, and of every answer on
    its client. Over ASCII, which is what headers are, the trip changes
    nothing, so it is skipped; a block with any byte above 127 still takes it,
    whatever it does to such a byte }
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

  { the segment [vStart, AEnd) holds name, separator and value, and both are
    cut straight from the text: copying the segment first and cutting the
    copy again cost one more string per param. A segment without the
    separator is skipped, as a line is }
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
  { ONE scan, no copy of a segment, and the bytes read through a pointer:
    offsets are 0-based from the start of the text, whatever the compiler
    makes of string indexes, and Copy takes them plus one.

    It used to Delete each segment off the front of the text - shifting the
    rest every time, quadratic in the number of segments of a query string or
    form body that comes straight from the network - and kept that Delete
    inside "if vLine <> ''": an empty segment ("?&a=1", "a=1&&b=2") consumed
    nothing and the loop spun at 100% CPU forever (SEC-01 of the 10/09/2026
    audit). Empty segments are still skipped, and so is everything when there
    is no name separator to look for. }
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

  { The segments of AFullURI past APartialURI, as ral_uriparam1, 2... - what
    the route matching gives a route with AllowURIParams. The prefix has to
    be whole segments at the start: it was looked for anywhere (Pos > 0), so
    the cut took the wrong characters. And the loop wrote the first param
    empty and dropped the last one, written for a FixRoute that ended in '/' }
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
  { a header or a cookie is one line of the message: a CR or LF in it would
    end the line where the value chose. Other kinds are left as they are }
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

{ Built into a buffer that grows geometrically instead of by concatenation.
  Every "Result := Result + x" reallocates the whole string and copies it, so a
  response with eight headers reallocated two dozen times - and each
  reallocation takes the memory manager's lock, which is what turns into a
  queue once twenty threads are answering at once. Measured on the QUIC engine:
  building the response headers was 0.155 ms of a 0.433 ms request.

  The result is identical, deliberately: the line separator still goes in only
  before a param that is not the first to produce output, and the whole thing
  is still TrimRight'ed at the end.

  Headers and cookies - the text every response's headers are made of - are
  measured first and written into a string of their exact size: one
  allocation, where growing from 256 bytes and cutting to size at the end took
  two (and on Delphi the cut is a ReallocMem). The escaping of CR, LF and NUL
  keeps the length, so it is done in place, in the copy. }
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
    { a value that came back longer the second time (nothing in RAL does
      that) must not write past the string }
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

{ ONE RULE FOR EVERY CLIENT, the way AddSetCookie is one for every answer.
  Each engine joined the params itself, as ParamName=Value, so a cookie added
  as a TRALCookie record went out as "Cookie: Set-Cookie=sessao=abc; Path=/" -
  a cookie called Set-Cookie, which no server read under the name it was given }
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

{ The engines whose library keeps a cookie jar (Indy, netHTTP) used to let the
  library write the header as well: Indy as a second Cookie line, which the
  server read last, the RTL over RAL's own - so once an answer had set a
  cookie, the next request lost the application's. They hand the jar's text
  here instead and send one header. Names are compared case-sensitively, as
  RFC 6265 compares them. }
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
    if FBuffers = nil then
      FBuffers := TList.Create;
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
          { bytes and not one part out of them, with no close delimiter either -
            which an empty form still has: no boundary declared, none in the
            body, or a first part cut short. The body vanished from the params
            without a word, and the route ran as if nothing had been sent }
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
  vInt, vInt1, vInt2: integer;
  vItem, vBody: TRALParam;
  vString, vValor, vFile: StringRAL;
  vFormAsMultipart, vSingle: boolean;
begin
  Result := nil;

  { one walk for the body and field params, and the first body one: it took
    three - Count(rpkBODY), Count(rpkFIELD), IndexKind - over a list a
    response fills with its headers }
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
    HasCipher(True);

  AContentDisposition := '';

  { the shortcut "the body IS this value" is for a lone rpkBODY only. It used to
    take a lone rpkFIELD too, by counting params instead of looking at their
    kind: AddField('m', json) went out as the raw JSON, with no "m=", and a
    third-party server found no field at all (a RAL server did not notice, the
    value just landed in Body). A form field is always name=value, one or many }
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

{ the name by const: by value it cost a reference count up and down, and an
  exception frame, on every lookup }
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
  { room for what a request carries - the RAL header and three or four from
    the client - in one allocation: TList grows by four, so the fifth param
    reallocated the list }
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
  { no name yet, so not in the index: it enters when it gets one
    (SetParamName) - indexing it nameless only to move it a line later cost a
    second pass on every param. The index is built with the first param and
    doubled by the size of the list }
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

  { the name is hashed once: a lookup followed by assigning ParamName hashed
    it twice for every param parsed or added }
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

{ AppendParamLine over ALen characters of ASource from AStart, without the
  copy of the line it would take: the name and the value are the only strings
  made. The same answer - the first ANameSeparator splits, a line without one
  adds nothing }
{ Where the first ANameSeparator of the span starts, 0 when it has none }
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
    it would have done AFTER the parse. So its lines are not parsed again, it
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
  { the param the parse would have made: placed by its FIRST line, with the
    name as the LAST line writes it and the last value that was not empty -
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
  { an HTTP header is not URL-encoded - a query string, a form field and a
    cookie are. Decoding headers too turned every '+' into a space: the
    base64 of an "Authorization: Basic" whenever it holds one (DecodeAuth
    reads it from here, on every engine), and media types such as
    application/ld+json or image/svg+xml; any '%XX' in a header was
    rewritten as well. Nothing on the sending side ever encoded a header }
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
//var
//  vCript: TRALCripto;
begin
  Result := TRALHashes.Encrypt(AStream, CriptoOptions.Key, CriptoOptions.CriptType);
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
  Result := TRALHashes.Decrypt(AStream, CriptoOptions.Key, CriptoOptions.CriptType);
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
  Result := TRALHashes.Decrypt(ASource, CriptoOptions.Key, CriptoOptions.CriptType);
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

procedure FillSharedStrings;
var
  vInt: IntegerRAL;
begin
  { on Delphi this is the one copy of each literal; from here on they are
    handed over by reference }
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
