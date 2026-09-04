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
    FContent: TStream;
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
    function GetContentDisposition: StringRAL;
    function GetContentSize: Int64RAL;
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
    function SaveToStream: TStream; overload;
    procedure SaveToStream(AStream: TStream); overload;
    function Size: Int64;

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDouble: DoubleRAL read GetAsDouble write SetAsDouble;
    property AsInteger: IntegerRAL read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsStream: TStream read GetAsStream write SetAsStream;
    property AsString: StringRAL read GetAsString write SetAsString;
    property Content: TStream read FContent;
    property ContentDisposition: StringRAL read GetContentDisposition write SetContentDisposition;
    property ContentDispositionInline: Boolean read FContentDispositionInline write FContentDispositionInline;
    property ContentSize: Int64RAL read GetContentSize;
    property ContentType: StringRAL read FContentType write FContentType;
    property FileName: StringRAL read FFileName write FFileName;
    property Kind: TRALParamKind read FKind write FKind;
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
  protected
    /// Decodes the ALine URL and adds it to the param list.
    procedure AppendParamLine(const ALine: StringRAL; const ANameSeparator: StringRAL;
      AKind: TRALParamKind);
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
    /// AddParam is used to include a TRALParam Object into the internal list.
    function AddParam(const AName: StringRAL; const AValue: StringRAL;
                      AKind: TRALParamKind = rpkNONE): TRALParam; overload;
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
    /// Returns a TStream with the filtered Stream body contents.
    function DecodeBody(ASource: TStream; const AContentType: StringRAL;
                        const AContentDisposition: StringRAL = ''): TStream; overload;
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
    { AComprimirMultipart False leaves a multipart body uncompressed - only the
      client request path asks for that, and the reason is written where the
      flag is read. Everything else keeps compressing as it always did. }
    function EncodeBody(var AContentType, AContentDisposition: StringRAL;
      AComprimirMultipart: boolean = True): TStream;
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
    if SameText(Name, 'HttpOnly') then
      Result.HttpOnly := True
    else if SameText(Name, 'Secure') then
      Result.Secure := True
    else if SameText(Name, 'Path') then
      Result.Path := Value
    else if SameText(Name, 'Domain') then
      Result.Domain := Value
    else if SameText(Name, 'SameSite') then
    begin
      if SameText(Value, 'None') then
        Result.SameSite := cssNone
      else if SameText(Value, 'Lax') then
        Result.SameSite := cssLax
      else if SameText(Value, 'Strict') then
        Result.SameSite := cssStrict;
    end
    else if SameText(Name, 'Expires') then
      Result.Expires := HTTPDateTimeToDateTime(Value)
    else if SameText(Name, 'Max-Age') then
      Result.MaxAge := StrToInt64Def(Value, 0)
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
  ASource.AsStream := Self.Content;
  ASource.ContentType := Self.ContentType;
end;

constructor TRALParam.Create;
begin
  inherited;
  FContent := nil;
  FContentType := rctTEXTPLAIN;
  FKind := rpkNONE;
end;

destructor TRALParam.Destroy;
begin
  FreeAndNil(FContent);
  inherited;
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
    Result := StrToDateTimeDef(StreamToString(FContent), 0);
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
    Result := StrToDateTimeDef(StreamToString(FContent), 0, ACustomFormat);
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
  vInt, vFim: Integer;
  vTmp: Byte;
begin
  vBytes := @ABuffer;
  vFim := ASize - 1;
  for vInt := 0 to (ASize div 2) - 1 do
  begin
    vTmp := vBytes[vInt];
    vBytes[vInt] := vBytes[vFim - vInt];
    vBytes[vFim - vInt] := vTmp;
  end;
end;
{$ELSE}
begin
  { little-endian target: the wire format already matches memory }
end;
{$IFEND}

function TRALParam.IsTyped: Boolean;
begin
  Result := (Self <> nil) and
            (SameText(MediaType, rctRALINT32) or
             SameText(MediaType, rctRALINT64) or
             SameText(MediaType, rctRALDOUBLE) or
             SameText(MediaType, rctRALCURRENCY) or
             SameText(MediaType, rctRALBOOLEAN) or
             SameText(MediaType, rctRALDATETIME));
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
  Result := (Self <> nil) and SameText(MediaType, AType) and
            (FContent <> nil) and (FContent.Size = ASize);

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
    Result := StrToCurrDef(StreamToString(FContent), 0);
end;
function TRALParam.IsNilOrEmpty: Boolean;
begin
  Result := (Self = nil) or ((Self <> nil) and (Self.Size = 0));
end;

function TRALParam.Size: Int64;
begin
  if FContent <> nil then
    Result := FContent.Size
  else
    Result := 0;
end;

procedure TRALParam.OpenFile(const AFileName: StringRAL);
begin
  if FContent <> nil then
    FreeAndNil(FContent);

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
    Result := StrToInt64Def(StreamToString(FContent), 0);
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
    vStr := StreamToString(FContent);
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
    Result := StrToFloatDef(StreamToString(FContent), 0);
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
    Result := StrToIntDef(StreamToString(FContent), 0);
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
    Result := StreamToString(FContent);
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
  Result := FContent.Size;
end;

procedure TRALParam.SaveToFile(const AFileName: StringRAL);
begin
  SaveStream(FContent, AFileName);
end;

procedure TRALParam.SaveToFile;
begin
  SaveToFile('', '');
end;

procedure TRALParam.SaveToStream(AStream: TStream);
begin
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
  if FContent <> nil then
    FreeAndNil(FContent);

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

procedure TRALParam.SetAsString(const AValue: StringRAL);
begin
  if FContent <> nil then
    FreeAndNil(FContent);

  FContent := StringToStreamUTF8(AValue);

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
    if SameText(AHeader, 'name') then
      FParamName := AValue
    else if SameText(AHeader, 'filename') then
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
  vInt: IntegerRAL;
  vLine: StringRAL;
  vIs13: Boolean;
begin
  {$IFDEF FPC}
    ASource := UTF8Decode(ASource);
  {$ELSE}
    ASource := UTF8ToString(ASource);
  {$ENDIF}

  if (ASource <> '') and (ANameSeparator = '') then
    ANameSeparator := FindHeaderNameSeparator(ASource);

  vLine := '';
  for vInt := POSINISTR to RALHighStr(ASource) do
  begin
    if ASource[vInt] = #13 then
    begin
      AppendParamLine(vLine, ANameSeparator, AKind);
      vIs13 := True;
      vLine := '';
    end
    else if ASource[vInt] = #10 then
    begin
      if not vIs13 then
        AppendParamLine(vLine, ANameSeparator, AKind);
      vIs13 := False;
      vLine := '';
    end
    else
    begin
      vLine := vLine + ASource[vInt];
      vIs13 := False;
    end;
  end;

  if vLine <> '' then
    AppendParamLine(vLine, ANameSeparator, AKind);
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

function TRALParams.AssignParamsText(AKind: TRALParamKind; AUrlEncoded: boolean;
  const ANameSeparator: StringRAL; const ALineSeparator: StringRAL): StringRAL;
var
  vInt: integer;
  vParam: TRALParam;
begin
  Result := '';
  for vInt := 0 to Pred(Count) do
  begin
    vParam := TRALParam(FParams.Items[vInt]);
    if vParam.Kind = AKind then
    begin
      if Result <> '' then
        Result := Result + ALineSeparator;
      Result := Result + vParam.ParamName + ANameSeparator;
      if AUrlEncoded then
        Result := Result + TRALHTTPCoder.EncodeURL(vParam.AsString)
      else
        Result := Result + vParam.AsString;
    end;
  end;

  Result := TrimRight(Result);
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

{ True when the stream opens with the two dashes that start a multipart
  delimiter. Enough to tell a plain multipart body from a compressed one: every
  compressor RAL uses writes a header of its own first, and none of them starts
  with "--" (deflate opens with 0x1F 0x8B). }
function ComecaComDelimitador(AStream: TStream): boolean;
var
  vDois: array [0 .. 1] of Byte;
  vPos: Int64RAL;
begin
  Result := False;
  if (AStream = nil) or (AStream.Size < 2) then
    Exit;
  vPos := AStream.Position;
  try
    AStream.Position := 0;
    AStream.ReadBuffer(vDois[0], 2);
    Result := (vDois[0] = Ord('-')) and (vDois[1] = Ord('-'));
  finally
    AStream.Position := vPos;
  end;
end;

function TRALParams.DecodeBody(ASource: TStream;
  const AContentType, AContentDisposition: StringRAL): TStream;
var
  vParam: TRALParam;
  vDecoder: TRALMultipartDecoder;
  vTemp: TStream;
begin
  Result := nil;
  if ASource = nil then
    Exit;

  ASource.Position := 0;

  Result := TMemoryStream.Create;
  Result.CopyFrom(ASource, ASource.Size);

  if (FCriptoOptions.CriptType <> crNone) and (FCriptoOptions.Key <> '') then
  begin
    vTemp := Decrypt(Result);
    FreeAndNil(Result);
    Result := vTemp;
  end;

  { A body that is ALREADY multipart is not decompressed, whatever the settings
    say. EncodeBody stopped compressing multipart (the reason is written there),
    and this side has no header to learn that from when both ends are two plain
    TRALParams in the same process - they are born with CompressType = gzip, so
    it would try to inflate a body that was never deflated.

    Sniffing the bytes rather than trusting the flag also keeps senders from
    before that change working: their multipart really is compressed, a deflate
    stream starts with 0x1F 0x8B, and only a plain one starts with the two
    dashes of a delimiter. }
  if (FCompressType <> ctNone) and
     not ((Pos(rctMULTIPARTFORMDATA, LowerCase(AContentType)) > 0) and
          ComecaComDelimitador(Result)) then
  begin
    vTemp := Decompress(Result);
    FreeAndNil(Result);
    Result := vTemp;
  end;

  if Pos(rctMULTIPARTFORMDATA, LowerCase(AContentType)) > 0 then
  begin
    vDecoder := TRALMultipartDecoder.Create;
    try
      vDecoder.ContentType := AContentType;
      vDecoder.OnFormDataComplete := {$IFDEF FPC}@{$ENDIF}OnFormBodyData;
      vDecoder.ProcessMultiPart(Result);
    finally
      FreeAndNil(vDecoder);
    end;
  end
  else if Pos(rctAPPLICATIONXWWWFORMURLENCODED, LowerCase(AContentType)) > 0 then
  begin
    DecodeFields(StreamToString(Result));
  end
  else
  begin
    vParam := NewParam;
    vParam.ParamName := 'ral_body';
    vParam.FileName := '';
    vParam.ContentDisposition := AContentDisposition;

    { Content first, ContentType after - the order is load-bearing. A single
      body param travels with its own content type as the HTTP header, so this
      is what restores a typed marker on the way in; assigning the type before
      the stream would clear it again (SetAsStream drops it). Same ordering as
      TRALParam.Clone. }
    vParam.AsStream := Result;
    vParam.ContentType := AContentType;
    vParam.Kind := rpkBODY;
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

  // deve manter TStringStream pois nesse ponto o ASource ainda pode estar
  // compress e criptografado
  vStream := TStringStream.Create(ASource);
  try
    Result := DecodeBody(vStream, AContentType, AContentDisposition);
  finally
    FreeAndNil(vStream);
  end;
end;

function TRALParams.EncodeBody(var AContentType, AContentDisposition: StringRAL;
  AComprimirMultipart: boolean): TStream;
var
  vMultPart: TRALMultipartEncoder;
  vInt1, vInt2: integer;
  vItem: TRALParam;
  vString, vValor: StringRAL;
  vTemp: TStream;
begin
  Result := nil;

  vInt1 := Count(rpkBODY);
  vInt2 := Count(rpkFIELD);

  AContentDisposition := '';

  if vInt1 + vInt2 = 1 then
  begin
    if vInt1 > 0 then
      vItem := IndexKind[0, rpkBODY]
    else
      vItem := IndexKind[0, rpkFIELD];

    vItem.ContentDispositionInline := FContentDispositionInline;

    if Pos(StringRAL('ral_param'), vItem.ParamName) > 0 then
      vItem.ParamName := 'ral_body';

    Result := vItem.SaveToStream;

    AContentType := vItem.ContentType;
    AContentDisposition := vItem.ContentDisposition;
  end
  else if (vInt2 > 0) and (vInt1 = 0) then
  begin
    vString := '';
    for vInt1 := 0 to Pred(Count) do
    begin
      vItem := Index[vInt1];
      if vItem.Kind in [rpkFIELD] then
      begin
        if vString <> '' then
          vString := vString + '&';

        vValor := vItem.ParamName + '=' + vItem.AsString;
        vValor := StringReplace(vValor, '&', '%26', [rfReplaceAll]);
        vValor := StringReplace(vValor, '&amp;', '%26', [rfReplaceAll]);

        vString := vString + vValor;
      end;
    end;
    Result := TStringStream.Create(vString);
    Result.Position := 0;

    AContentType := rctAPPLICATIONXWWWFORMURLENCODED;
  end
  else if vInt1 + vInt2 > 1 then
  begin
    vMultPart := TRALMultipartEncoder.Create;
    try
      for vInt1 := 0 to Pred(Count) do
      begin
        vItem := Index[vInt1];
        if vItem.Kind in [rpkBODY, rpkFIELD] then
        begin
          vMultPart.AddStream(Index[vInt1].ParamName, Index[vInt1].Content,
            Index[vInt1].FileName, Index[vInt1].ContentType);
        end;
      end;
      Result := vMultPart.AsStream;
      AContentType := vMultPart.ContentType;
    finally
      FreeAndNil(vMultPart);
    end;
  end;

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
    compresses normally. }
  if (not AComprimirMultipart) and (FCompressType <> ctNone) and
     (Pos(StringRAL(rctMULTIPARTFORMDATA), LowerCase(AContentType)) > 0) then
    { and the caller hears about it through CompressType: whoever fills
      Content-Encoding reads it back from here, and a header promising gzip over
      bytes that were never compressed makes the other side fail to inflate }
    FCompressType := ctNone;

  if (FCompressType <> ctNone) and (Result <> nil) then
  begin
    vTemp := Compress(Result);
    FreeAndNil(Result);
    Result := vTemp;
  end;

  if (FCriptoOptions.CriptType <> crNone) and (Trim(FCriptoOptions.Key) <> '') and
    (Result <> nil) then
  begin
    vTemp := Encrypt(Result);
    FreeAndNil(Result);
    Result := vTemp;
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
  FCriptoOptions := TRALCriptoOptions.Create;

  FCompressType := ctGZip;
  FNextParam := 0;
end;

destructor TRALParams.Destroy;
begin
  ClearParams;
  FreeAndNil(FParams);
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
    if (SameText(vParam.ParamName, AName)) and (vParam.Kind = AKind) then
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
    if SameText(vParam.ParamName, AName) then
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
  end;
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

  vParam.AsStream := AFormData.AsStream;
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
  if Result <> '' then
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
    if SameText(vParam.ParamName, AName) and (vParam.Kind = AKind) then
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
    if SameText(vParam.ParamName, AName) then
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
