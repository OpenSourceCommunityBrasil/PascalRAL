{ The generic channel for REST gateways (Evolution API, WPPConnect Server, Z-API,
  signal-cli and the like): the address of each kind of message, the authentication
  header and the fields of the JSON, both ways, are properties of the component; a
  profile fills them in for the common gateways. }
unit RALChatGateway;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALJson, RALClient, RALRequest, RALResponse, RALBase64,
  RALHashBase, RALSHA1, RALSHA2_32, RALSHA2_64,
  RALChatMessage, RALChatVerify, RALChatChannel;

type
  /// The gateways the component knows how to fill in.
  TRALChatGatewayProfile = (
    /// Nothing is filled in: the properties are the ones of the form.
    gpCustom,
    /// Evolution API (v2).
    gpEvolution,
    /// WPPConnect Server.
    gpWPPConnect,
    /// Z-API.
    gpZAPI
  );

  /// What proves a webhook comes from the gateway.
  TRALChatGatewayVerifyMode = (
    { Nothing: the gateway signs nothing; the webhook module warns in its log at every
      request. }
    gvNone,
    /// An HMAC of the raw body in a header.
    gvHMAC,
    /// A shared secret in a header.
    gvSecret,
    /// The address of the sender is in a list.
    gvIPList
  );

  /// The hash of the HMAC.
  TRALChatGatewayHash = (
    /// HMAC-SHA1.
    ghSHA1,
    /// HMAC-SHA256.
    ghSHA256,
    /// HMAC-SHA512.
    ghSHA512
  );

  /// The HTTP method of an endpoint.
  TRALChatGatewayMethod = (
    /// POST.
    gmPost,
    /// PUT.
    gmPut,
    /// PATCH.
    gmPatch,
    /// DELETE.
    gmDelete,
    /// GET.
    gmGet
  );

  { Proves a webhook comes from the gateway, by the mode the component asks: an HMAC, a
    secret header, a list of addresses, or nothing. }
  TRALChatGatewayVerifier = class(TRALChatVerifier)
  private
    FAllowedIPs: TStrings;
    FHash: TRALChatGatewayHash;
    FHeaderName: StringRAL;
    FIPHeader: StringRAL;
    FMode: TRALChatGatewayVerifyMode;
    FPrefix: StringRAL;
    FSecret: StringRAL;
    FSignatureEncoding: TRALChatSignatureEncoding;
    FTemplate: StringRAL;
    FTimestampHeader: StringRAL;

    /// Whether the address is in AllowedIPs (exact or an IPv4 range).
    function IPAllowed(const AIp: StringRAL): Boolean;
    procedure SetAllowedIPs(AValue: TStrings);
    /// The HMAC of the signed text, in the encoding of the gateway.
    function Sign(const ASigned: StringRAL): StringRAL;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function IsOpen: Boolean; override;
    function Verify(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
      out ATimestamp: Int64RAL): Boolean; override;
  published
    { The addresses that may call (gvIPList): one per line, a single address or an IPv4
      range such as 10.0.0.0/8. }
    property AllowedIPs: TStrings read FAllowedIPs write SetAllowedIPs;
    /// The hash of the HMAC (gvHMAC).
    property Hash: TRALChatGatewayHash read FHash write FHash default ghSHA256;
    /// The header with the signature (gvHMAC) or the secret (gvSecret).
    property HeaderName: StringRAL read FHeaderName write FHeaderName;
    { The header that holds the address of the caller when the gateway sits behind a
      proxy (X-Forwarded-For: the first address counts); empty = the address of the
      connection. }
    property IPHeader: StringRAL read FIPHeader write FIPHeader;
    /// What the gateway sends to prove itself.
    property Mode: TRALChatGatewayVerifyMode read FMode write FMode default gvSecret;
    /// Text before the signature in the header ('sha256=').
    property Prefix: StringRAL read FPrefix write FPrefix;
    { The key of the HMAC or the secret of the header; empty refuses every webhook. Set
      it from the environment in the program rather than in the form. }
    property Secret: StringRAL read FSecret write FSecret;
    /// How the signature is written (gvHMAC).
    property SignatureEncoding: TRALChatSignatureEncoding read FSignatureEncoding
      write FSignatureEncoding default seHex;
    { The signed text (gvHMAC): the word body in braces is the raw body and ts the
      timestamp header; the body alone by default. }
    property Template: StringRAL read FTemplate write FTemplate;
    /// The header with the Unix timestamp (replay window); empty when there is none.
    property TimestampHeader: StringRAL read FTimestampHeader write FTimestampHeader;
  end;

  { One address of the gateway: the URL, the method and the JSON body, both with
    placeholders in braces (see TRALChatGateway). Empty URL = the gateway cannot do it. }
  TRALChatGatewayEndpoint = class(TPersistent)
  private
    FBody: StringRAL;
    FMethod: TRALChatGatewayMethod;
    FOnChange: TNotifyEvent;
    FUrl: StringRAL;

    procedure SetBody(const AValue: StringRAL);
    procedure SetMethod(AValue: TRALChatGatewayMethod);
    procedure SetUrl(const AValue: StringRAL);
  public
    constructor Create;

    procedure Assign(Source: TPersistent); override;
    /// Sets the three at once.
    procedure Setup(const AUrl, ABody: StringRAL; AMethod: TRALChatGatewayMethod = gmPost);

    /// Called when the URL changes.
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    { The JSON sent: placeholders in braces stand for JSON values (a text comes quoted
      and escaped, so do not quote it); empty = no body. }
    property Body: StringRAL read FBody write SetBody;
    /// The HTTP method.
    property Method: TRALChatGatewayMethod read FMethod write SetMethod default gmPost;
    { The address, with the base address of the gateway in front unless it starts with
      http: placeholders in braces are replaced by the encoded value. }
    property Url: StringRAL read FUrl write SetUrl;
  end;

  { What the gateway is asked for each kind of message, and how it answers. The values
    the placeholders can take: chat, jid, isgroup, text, caption, title, footer,
    buttontext, reply (the id of the message answered), target (the id of the message
    edited or removed), mediatype, mime, filename, extension, url, base64, datauri,
    media (the URL, or else the base64), mediauri (the URL, or else the data URI),
    latitude, longitude, address, contactname, contactphone, token, instance,
    instancekey, buttons and rows (the arrays made with ButtonItem and RowItem, whose
    placeholders are id, caption, url, title and description). }
  TRALChatGatewayOutgoing = class(TPersistent)
  private
    FAudio: TRALChatGatewayEndpoint;
    FButtonItem: StringRAL;
    FButtons: TRALChatGatewayEndpoint;
    FContact: TRALChatGatewayEndpoint;
    FDelete: TRALChatGatewayEndpoint;
    FDocument: TRALChatGatewayEndpoint;
    FEdit: TRALChatGatewayEndpoint;
    FErrorPath: StringRAL;
    FImage: TRALChatGatewayEndpoint;
    FList: TRALChatGatewayEndpoint;
    FLocation: TRALChatGatewayEndpoint;
    FMessageIdPath: StringRAL;
    FOnChange: TNotifyEvent;
    FRowItem: StringRAL;
    FSticker: TRALChatGatewayEndpoint;
    FText: TRALChatGatewayEndpoint;
    FTyping: TRALChatGatewayEndpoint;
    FVideo: TRALChatGatewayEndpoint;

    /// Tells the owner an address changed.
    procedure Changed(Sender: TObject);
    /// Makes an endpoint that reports its changes.
    function NewEndpoint: TRALChatGatewayEndpoint;
    procedure SetAudio(AValue: TRALChatGatewayEndpoint);
    procedure SetButtons(AValue: TRALChatGatewayEndpoint);
    procedure SetContact(AValue: TRALChatGatewayEndpoint);
    procedure SetDelete(AValue: TRALChatGatewayEndpoint);
    procedure SetDocument(AValue: TRALChatGatewayEndpoint);
    procedure SetEdit(AValue: TRALChatGatewayEndpoint);
    procedure SetImage(AValue: TRALChatGatewayEndpoint);
    procedure SetList(AValue: TRALChatGatewayEndpoint);
    procedure SetLocation(AValue: TRALChatGatewayEndpoint);
    procedure SetSticker(AValue: TRALChatGatewayEndpoint);
    procedure SetText(AValue: TRALChatGatewayEndpoint);
    procedure SetTyping(AValue: TRALChatGatewayEndpoint);
    procedure SetVideo(AValue: TRALChatGatewayEndpoint);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    /// Empties every endpoint and template.
    procedure Clear;

    /// Called when an address changes.
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    /// Audio and voice notes.
    property Audio: TRALChatGatewayEndpoint read FAudio write SetAudio;
    /// The template of one button (placeholders id, caption, url).
    property ButtonItem: StringRAL read FButtonItem write FButtonItem;
    /// A text with reply buttons.
    property Buttons: TRALChatGatewayEndpoint read FButtons write SetButtons;
    /// A contact card.
    property Contact: TRALChatGatewayEndpoint read FContact write SetContact;
    /// Removes a sent message.
    property Delete: TRALChatGatewayEndpoint read FDelete write SetDelete;
    /// A file.
    property Document: TRALChatGatewayEndpoint read FDocument write SetDocument;
    /// Changes the text of a sent message.
    property Edit: TRALChatGatewayEndpoint read FEdit write SetEdit;
    { Where the error text of a refused call is in the answer (paths separated by
      semicolons, the first one found counts). }
    property ErrorPath: StringRAL read FErrorPath write FErrorPath;
    /// A picture.
    property Image: TRALChatGatewayEndpoint read FImage write SetImage;
    /// A text with a list of options.
    property List: TRALChatGatewayEndpoint read FList write SetList;
    /// A place.
    property Location: TRALChatGatewayEndpoint read FLocation write SetLocation;
    { Where the id of the sent message is in the answer (paths separated by semicolons,
      the first one found counts). }
    property MessageIdPath: StringRAL read FMessageIdPath write FMessageIdPath;
    /// The template of one option of the list (placeholders id, title, description).
    property RowItem: StringRAL read FRowItem write FRowItem;
    /// A sticker.
    property Sticker: TRALChatGatewayEndpoint read FSticker write SetSticker;
    /// A plain text.
    property Text: TRALChatGatewayEndpoint read FText write SetText;
    /// Shows "typing".
    property Typing: TRALChatGatewayEndpoint read FTyping write SetTyping;
    /// A video.
    property Video: TRALChatGatewayEndpoint read FVideo write SetVideo;
  end;

  { Where each piece of a received message is in the JSON the gateway posts. A path is
    the names of the members joined by dots, a number picking an element of an array
    (data.key.id, response.0.id); several paths separated by semicolons are tried in
    order and the first one found counts. Paths are relative to the item (see
    ItemsPath), except EventPath. }
  TRALChatGatewayIncoming = class(TPersistent)
  private
    FButtonIdPath: StringRAL;
    FButtonTextPath: StringRAL;
    FCaptionPath: StringRAL;
    FChatIdPath: StringRAL;
    FChatIdStrip: StringRAL;
    FContactNamePath: StringRAL;
    FContactPhonePath: StringRAL;
    FEventIdPath: StringRAL;
    FEventPath: StringRAL;
    FEventValues: StringRAL;
    FFileNamePath: StringRAL;
    FFromMePath: StringRAL;
    FGroupMark: StringRAL;
    FGroupPath: StringRAL;
    FItemsPath: StringRAL;
    FKindRules: TStrings;
    FLatitudePath: StringRAL;
    FLongitudePath: StringRAL;
    FMediaBase64Path: StringRAL;
    FMediaHosts: TStrings;
    FMediaUrlPath: StringRAL;
    FMessageIdPath: StringRAL;
    FMimePath: StringRAL;
    FPlaceNamePath: StringRAL;
    FSenderIdPath: StringRAL;
    FSenderNamePath: StringRAL;
    FSenderPhonePath: StringRAL;
    FTextPath: StringRAL;
    FTimestampPath: StringRAL;

    procedure SetKindRules(AValue: TStrings);
    procedure SetMediaHosts(AValue: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    /// Empties every path.
    procedure Clear;
  published
    /// Where the id of the pressed button is.
    property ButtonIdPath: StringRAL read FButtonIdPath write FButtonIdPath;
    /// Where the label of the pressed button is.
    property ButtonTextPath: StringRAL read FButtonTextPath write FButtonTextPath;
    /// Where the caption of a file is.
    property CaptionPath: StringRAL read FCaptionPath write FCaptionPath;
    /// Where the id of the conversation is.
    property ChatIdPath: StringRAL read FChatIdPath write FChatIdPath;
    { Endings taken off the ids of the conversation and of the sender, separated by
      semicolons (the s.whatsapp.net ending): what is left is the number the gateway
      sends to. }
    property ChatIdStrip: StringRAL read FChatIdStrip write FChatIdStrip;
    /// Where the name of a shared contact is.
    property ContactNamePath: StringRAL read FContactNamePath write FContactNamePath;
    /// Where the number (or the vCard) of a shared contact is.
    property ContactPhonePath: StringRAL read FContactPhonePath write FContactPhonePath;
    /// Where the id of the event is, for the duplicate filter.
    property EventIdPath: StringRAL read FEventIdPath write FEventIdPath;
    /// Where the name of the event is (relative to the root of the JSON).
    property EventPath: StringRAL read FEventPath write FEventPath;
    { The events that carry a message, separated by semicolons (messages.upsert); others
      are ignored; empty = every one. Case and the difference between dot and underscore
      do not matter. }
    property EventValues: StringRAL read FEventValues write FEventValues;
    /// Where the name of a file is.
    property FileNamePath: StringRAL read FFileNamePath write FFileNamePath;
    { Where the "sent by me" flag is: a message that is true there (the account's own
      messages) is ignored. }
    property FromMePath: StringRAL read FFromMePath write FFromMePath;
    /// A text that makes the id of the conversation a group (a g.us ending).
    property GroupMark: StringRAL read FGroupMark write FGroupMark;
    /// Where the "is a group" flag is.
    property GroupPath: StringRAL read FGroupPath write FGroupPath;
    { Where the message is in the root of the JSON (data); an array holds several; empty
      = the root itself. }
    property ItemsPath: StringRAL read FItemsPath write FItemsPath;
    { What kind a message is, one rule per line: path|value|kind. A rule matches when
      the path exists (value empty) or holds the value; the first match wins; kinds are
      text, image, audio, video, document, sticker, location, contact and callback. No
      match = text. }
    property KindRules: TStrings read FKindRules write SetKindRules;
    /// Where the latitude of a place is.
    property LatitudePath: StringRAL read FLatitudePath write FLatitudePath;
    /// Where the longitude of a place is.
    property LongitudePath: StringRAL read FLongitudePath write FLongitudePath;
    /// Where the file comes inline, in base64 (a data URI is accepted).
    property MediaBase64Path: StringRAL read FMediaBase64Path write FMediaBase64Path;
    { The hosts a file may be downloaded from besides the one of the gateway, one per
      line (a name also covers its subdomains). }
    property MediaHosts: TStrings read FMediaHosts write SetMediaHosts;
    /// Where the address of the file is.
    property MediaUrlPath: StringRAL read FMediaUrlPath write FMediaUrlPath;
    /// Where the id of the message is.
    property MessageIdPath: StringRAL read FMessageIdPath write FMessageIdPath;
    /// Where the MIME type of the file is.
    property MimePath: StringRAL read FMimePath write FMimePath;
    /// Where the name of a place is.
    property PlaceNamePath: StringRAL read FPlaceNamePath write FPlaceNamePath;
    /// Where the id of the sender is (in a group, the member; else the conversation).
    property SenderIdPath: StringRAL read FSenderIdPath write FSenderIdPath;
    /// Where the name of the sender is.
    property SenderNamePath: StringRAL read FSenderNamePath write FSenderNamePath;
    /// Where the phone of the sender is; empty = the digits of the id.
    property SenderPhonePath: StringRAL read FSenderPhonePath write FSenderPhonePath;
    /// Where the text is.
    property TextPath: StringRAL read FTextPath write FTextPath;
    { Where the time of the message is, in Unix seconds or milliseconds (told apart by
      size). }
    property TimestampPath: StringRAL read FTimestampPath write FTimestampPath;
  end;

  { A channel for the REST gateways that put an HTTP API in front of an account that is
    not the official one. Pick a Profile, fill Instance and the token (never in the
    form: use TokenSource), point BaseUrl at the gateway and give the webhook module the
    same route. Everything the profile fills can be changed.  These gateways are not the
    official API: they can violate the terms of the platform and the number may be
    banned, which is why the limits start low. }
  TRALChatGateway = class(TRALChatChannel)
  private
    FAuthHeader: StringRAL;
    FAuthValue: StringRAL;
    FBaseUrl: StringRAL;
    /// Serializes the use of the client.
    FCallLock: TCriticalSection;
    FClient: TRALClient;
    FEngineType: string;
    FHeaders: TStrings;
    FIncoming: TRALChatGatewayIncoming;
    FInstance: StringRAL;
    FInstanceKey: StringRAL;
    /// Guards the lazy client.
    FLock: TCriticalSection;
    FMaxDownloadSize: Int64RAL;
    FOutgoing: TRALChatGatewayOutgoing;
    /// The channel's own client, made on first use.
    FOwnClient: TRALClient;
    FProfile: TRALChatGatewayProfile;
    FVerifier: TRALChatGatewayVerifier;

    /// Adds the authentication and the extra headers to the request of AClient.
    procedure AddHeaders(AClient: TRALClient; AValues: TObject);
    /// Reads the answer of the gateway into AResult.
    procedure ApplyAnswer(AStatus: Integer; const AText: StringRAL; AResponse: TRALResponse;
      var AResult: TRALChatSendResult);
    /// Makes the values the placeholders of a message take.
    function BuildValues(AMessage: TRALChatMessage; const ATarget: StringRAL): TObject;
    /// Makes the buttons and rows arrays and the file values.
    procedure FillMessageValues(AMessage: TRALChatMessage; AValues: TObject);
    /// The client in use: the linked one or the channel's own.
    function GetClient: TRALClient;
    /// Hides the secrets in AText.
    function Hide(const AText: StringRAL): StringRAL;
    /// Whether the host may be downloaded from.
    function HostAllowed(const AHost: StringRAL): Boolean;
    /// The host of the URL, in lower case.
    function HostOf(const AUrl: StringRAL): StringRAL;
    /// Opens a file received by address (only from the hosts allowed).
    function LoadMedia(AMedia: TRALChatMedia): TStream;
    /// Refreshes the capabilities when an address changes.
    procedure OutgoingChanged(Sender: TObject);
    /// The endpoint that takes AMessage; nil when the gateway has none.
    function PickEndpoint(AMessage: TRALChatMessage): TRALChatGatewayEndpoint;
    /// Reads one message of the webhook into AMessage; False when it is to be ignored.
    function ReadItem(AItem: TRALJSON; AMessage: TRALChatMessage): Boolean;
    /// Calls the endpoint with the values and fills AResult.
    procedure Send(AEndpoint: TRALChatGatewayEndpoint; AValues: TObject;
      var AResult: TRALChatSendResult);
    procedure SetClient(AValue: TRALClient);
    procedure SetHeaders(AValue: TStrings);
    procedure SetIncoming(AValue: TRALChatGatewayIncoming);
    procedure SetOutgoing(AValue: TRALChatGatewayOutgoing);
    procedure SetProfile(AValue: TRALChatGatewayProfile);
    procedure SetVerification(AValue: TRALChatGatewayVerifier);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Fills every property of the gateway with what its documentation says (the address
      of the gateway, the instance and the secrets stay as they are); gpCustom does
      nothing. }
    procedure ApplyProfile(AProfile: TRALChatGatewayProfile);
    procedure Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId: StringRAL; out AResult: TRALChatSendResult); override;
    function Dialect: TRALChatDialect; override;
    procedure ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList); override;
    { Sets the capabilities from the endpoints that have an address; ApplyProfile and a
      change of an address already do it. }
    procedure RefreshCapabilities;
  published
    /// The header that carries the token, empty = none.
    property AuthHeader: StringRAL read FAuthHeader write FAuthHeader;
    { The value of that header: the placeholder token in braces is replaced by the token
      (Bearer followed by it, for instance). }
    property AuthValue: StringRAL read FAuthValue write FAuthValue;
    /// The address of the gateway, without the final slash (http://localhost:8080).
    property BaseUrl: StringRAL read FBaseUrl write FBaseUrl;
    { The client that calls the gateway; leave its BaseURL empty (the channel sends
      whole URLs). Without one the channel makes its own, of EngineType. }
    property Client: TRALClient read FClient write SetClient;
    { The engine of the channel's own client (the engine unit must be in the program);
      empty = the first registered one. }
    property EngineType: string read FEngineType write FEngineType;
    { More headers on every call, one Name: value per line (the placeholders token,
      instance and instancekey in braces are replaced). }
    property Headers: TStrings read FHeaders write SetHeaders;
    /// What is read from the webhook.
    property Incoming: TRALChatGatewayIncoming read FIncoming write SetIncoming;
    /// The instance, session or account of the gateway ({instance} in the addresses).
    property Instance: StringRAL read FInstance write FInstance;
    { A second secret some gateways put in the address (the placeholder instancekey); it
      is hidden from logs like the token. }
    property InstanceKey: StringRAL read FInstanceKey write FInstanceKey;
    /// The largest file taken from the gateway, in bytes.
    property MaxDownloadSize: Int64RAL read FMaxDownloadSize write FMaxDownloadSize
      default 16777216;
    /// What is sent.
    property Outgoing: TRALChatGatewayOutgoing read FOutgoing write SetOutgoing;
    { The gateway the properties follow: setting it fills them in (gpCustom keeps them
      as they are). }
    property Profile: TRALChatGatewayProfile read FProfile write SetProfile default gpCustom;
    { How the webhook is proven; the default asks for a secret in a header, and nothing
      is accepted until the secret is set. }
    property Verification: TRALChatGatewayVerifier read FVerifier write SetVerification;
  end;

implementation

const
  /// The longest error text taken from an answer.
  cMaxErrorText = 200;
  /// Expand mode: values as JSON literals.
  cModeJson = 0;
  /// Expand mode: values percent-encoded for an address.
  cModeUrl = 1;
  /// Expand mode: values as they are.
  cModePlain = 2;

type
  /// An error that a new attempt cannot fix.
  ERALChatGatewayFinal = class(ERALChatError);

  { The values of the placeholders of one call: the plain text (for the URL and headers)
    and the JSON (for the body). }
  TGwValues = class
  private
    /// The JSON literal of each value.
    FJson: TStringList;
    /// The names of the placeholders.
    FNames: TStringList;
    /// The plain text of each value.
    FPlain: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds a true or false.
    procedure AddBool(const AName: StringRAL; AValue: Boolean);
    /// Adds a number written as text.
    procedure AddNumber(const AName, AText: StringRAL);
    /// Adds a value with its JSON literal and its plain text.
    procedure AddRaw(const AName, AJson, APlain: StringRAL);
    /// Adds a text, quoted and escaped in JSON.
    procedure AddStr(const AName, AValue: StringRAL);
    /// Looks a value up by name.
    function Find(const AName: StringRAL; out APlain, AJson: StringRAL): Boolean;
  end;

/// ---- helpers ----

/// A text as a JSON string literal.
function JsonLiteral(const AValue: StringRAL): StringRAL;
var
  vItem: TRALJSONString;
begin
  vItem := TRALJSONString.Create(AValue);
  try
    Result := vItem.ToJSON;
  finally
    vItem.Free;
  end;
end;

/// The text with everything but unreserved characters as %XX.
function UrlPart(const AValue: StringRAL): StringRAL;
const
  cHex: StringRAL = '0123456789ABCDEF';
var
  vIdx: Integer;
  vByte: Byte;
begin
  Result := '';
  for vIdx := 1 to Length(AValue) do
  begin
    vByte := Byte(AValue[vIdx]);
    if vByte in [65..90, 97..122, 48..57, 45, 46, 95, 126] then
      Result := Result + Copy(AValue, vIdx, 1)
    else
      Result := Result + '%' + Copy(cHex, (vByte shr 4) + 1, 1) + Copy(cHex, (vByte and 15) + 1, 1);
  end;
end;

/// A floating point number as text, with a dot.
function FloatText(AValue: Double): StringRAL;
var
  vText: AnsiString;
begin
  Str(AValue:0:8, vText);
  Result := StringRAL(vText);
end;

/// A number written with a dot; 0 when it is not one.
function ParseFloat(const AText: StringRAL): Double;
var
  vCode: Integer;
begin
  Val(Trim(string(AText)), Result, vCode);
  if vCode <> 0 then
    Result := 0;
end;

/// Replaces the placeholders in braces of ATemplate with the values.
function Expand(const ATemplate: StringRAL; AValues: TGwValues; AMode: Integer): StringRAL;
var
  vIdx, vEnd, vRun, vLen: Integer;
  vName, vPlain, vJson: StringRAL;
begin
  Result := '';
  vLen := Length(ATemplate);
  vIdx := 1;
  vRun := 1;
  while vIdx <= vLen do
  begin
    if ATemplate[vIdx] = '{' then
    begin
      vEnd := vIdx + 1;
      while (vEnd <= vLen) and (ATemplate[vEnd] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do
        Inc(vEnd);
      if (vEnd <= vLen) and (ATemplate[vEnd] = '}') and (vEnd > vIdx + 1) and
         not (ATemplate[vIdx + 1] in ['0'..'9']) then
      begin
        vName := StringRAL(LowerCase(string(Copy(ATemplate, vIdx + 1, vEnd - vIdx - 1))));
        if not AValues.Find(vName, vPlain, vJson) then
        begin
          vPlain := '';
          vJson := 'null';
        end;
        Result := Result + Copy(ATemplate, vRun, vIdx - vRun);
        if AMode = cModeUrl then
          Result := Result + UrlPart(vPlain)
        else if AMode = cModePlain then
          Result := Result + vPlain
        else if (vIdx > 1) and (ATemplate[vIdx - 1] = '"') and (vEnd < vLen) and
                (ATemplate[vEnd + 1] = '"') and (vJson <> '') and (vJson[1] = '"') then
          // the author quoted it already: the escaped text goes in without its own quotes
          Result := Result + Copy(vJson, 2, Length(vJson) - 2)
        else
          Result := Result + vJson;
        vIdx := vEnd + 1;
        vRun := vIdx;
        Continue;
      end;
    end;
    Inc(vIdx);
  end;
  Result := Result + Copy(ATemplate, vRun, vLen - vRun + 1);
end;

/// The node of a path (names joined by dots; a number picks an array element), or nil.
function PathNode(ARoot: TRALJSON; const APath: StringRAL): TRALJSON;
var
  vPos, vNext, vNum: Integer;
  vName: StringRAL;
begin
  Result := ARoot;
  vPos := 1;
  while (Result <> nil) and (vPos <= Length(APath)) do
  begin
    vNext := vPos;
    while (vNext <= Length(APath)) and (APath[vNext] <> '.') do
      Inc(vNext);
    vName := Copy(APath, vPos, vNext - vPos);
    vPos := vNext + 1;
    if Result.Kind = rjkObject then
      Result := TRALJSONObject(Result).Find(vName)
    else if Result.Kind = rjkArray then
    begin
      vNum := StrToIntDef(string(vName), -1);
      if (vNum >= 0) and (vNum < TRALJSONArray(Result).Count) then
        Result := TRALJSONArray(Result).Items[vNum]
      else
        Result := nil;
    end
    else
      Result := nil;
  end;
  if (Result <> nil) and (Result.Kind = rjkNull) then
    Result := nil;
end;

/// A scalar as text.
function ScalarText(AValue: TRALJSON): StringRAL;
begin
  Result := '';
  if AValue = nil then
    Exit;
  case AValue.Kind of
    rjkString:
      Result := AValue.AsString;
    rjkNumber:
      Result := TRALJSONNumber(AValue).Value;
    rjkBoolean:
      if AValue.AsBoolean then
        Result := 'true'
      else
        Result := 'false';
  end;
end;

/// The next path of a list separated by semicolons; False at the end.
function NextPath(const AList: StringRAL; var APos: Integer; out APath: StringRAL): Boolean;
var
  vEnd: Integer;
begin
  Result := False;
  APath := '';
  while APos <= Length(AList) do
  begin
    vEnd := APos;
    while (vEnd <= Length(AList)) and (AList[vEnd] <> ';') do
      Inc(vEnd);
    APath := StringRAL(Trim(string(Copy(AList, APos, vEnd - APos))));
    APos := vEnd + 1;
    if APath <> '' then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

/// The first non-empty text among the paths of the list.
function PathText(ARoot: TRALJSON; const APaths: StringRAL): StringRAL;
var
  vPos: Integer;
  vPath: StringRAL;
begin
  Result := '';
  vPos := 1;
  while NextPath(APaths, vPos, vPath) do
  begin
    Result := ScalarText(PathNode(ARoot, vPath));
    if Result <> '' then
      Exit;
  end;
end;

/// Whether one of the paths of the list exists.
function PathExists(ARoot: TRALJSON; const APaths: StringRAL): Boolean;
var
  vPos: Integer;
  vPath: StringRAL;
begin
  Result := False;
  vPos := 1;
  while NextPath(APaths, vPos, vPath) do
    if PathNode(ARoot, vPath) <> nil then
    begin
      Result := True;
      Exit;
    end;
end;

/// Whether the text is in a list separated by semicolons (ignoring case).
function InEventList(const AList, AValue: StringRAL): Boolean;
var
  vPos: Integer;
  vItem: StringRAL;
  vWant: string;
begin
  Result := AList = '';
  if Result then
    Exit;
  vWant := StringReplace(LowerCase(string(AValue)), '_', '.', [rfReplaceAll]);
  vPos := 1;
  while NextPath(AList, vPos, vItem) do
    if StringReplace(LowerCase(string(vItem)), '_', '.', [rfReplaceAll]) = vWant then
    begin
      Result := True;
      Exit;
    end;
end;

/// The digits of a text.
function DigitsOf(const AText: StringRAL): StringRAL;
var
  vIdx: Integer;
begin
  Result := '';
  for vIdx := 1 to Length(AText) do
    if Byte(AText[vIdx]) in [48..57] then
      Result := Result + Copy(AText, vIdx, 1);
end;

/// Whether the text is only digits (and a leading plus).
function IsNumber(const AText: StringRAL): Boolean;
begin
  Result := (AText <> '') and (DigitsOf(AText) = StringRAL(StringReplace(string(AText), '+', '', [])));
end;

/// Takes the line of a vCard that starts with AKey.
function VCardLine(const AText: string; const AKey: string): string;
var
  vPos, vEnd: Integer;
begin
  Result := '';
  vPos := Pos(AKey, AText);
  if vPos = 0 then
    Exit;
  vPos := vPos + Length(AKey);
  vEnd := vPos;
  while (vEnd <= Length(AText)) and (AText[vEnd] <> #10) and (AText[vEnd] <> #13) do
    Inc(vEnd);
  Result := Copy(AText, vPos, vEnd - vPos);
end;

/// The name in a vCard, or the text itself when it is not one.
function VCardName(const AText: StringRAL): StringRAL;
begin
  Result := AText;
  if Pos('BEGIN:VCARD', string(AText)) > 0 then
    Result := StringRAL(Trim(VCardLine(string(AText), 'FN:')));
end;

/// The number in a vCard (waid first, then TEL), or the text itself when it is not one.
function VCardPhone(const AText: StringRAL): StringRAL;
var
  vText, vLine: string;
  vPos: Integer;
begin
  Result := AText;
  vText := string(AText);
  if Pos('BEGIN:VCARD', vText) = 0 then
    Exit;
  vPos := Pos('waid=', vText);
  if vPos > 0 then
  begin
    Result := '';
    Inc(vPos, 5);
    while (vPos <= Length(vText)) and (vText[vPos] >= '0') and (vText[vPos] <= '9') do
    begin
      Result := Result + StringRAL(vText[vPos]);
      Inc(vPos);
    end;
    Exit;
  end;
  vPos := Pos('TEL', vText);
  vLine := '';
  if vPos > 0 then
    vLine := VCardLine(Copy(vText, vPos, MaxInt), 'TEL');
  vPos := Length(vLine);
  while (vPos > 0) and (vLine[vPos] <> ':') do
    Dec(vPos);
  Result := StringRAL(Trim(Copy(vLine, vPos + 1, MaxInt)));
end;

/// Reads a dotted IPv4 address; False when it is not one.
function ParseIPv4(const AText: StringRAL; out AValue: Cardinal): Boolean;
var
  vParts: Integer;
  vPos, vNum, vStart: Integer;
  vText: string;
begin
  Result := False;
  AValue := 0;
  vText := string(AText);
  vParts := 0;
  vPos := 1;
  while vPos <= Length(vText) + 1 do
  begin
    vStart := vPos;
    while (vPos <= Length(vText)) and (vText[vPos] <> '.') do
      Inc(vPos);
    vNum := StrToIntDef(Copy(vText, vStart, vPos - vStart), -1);
    if (vNum < 0) or (vNum > 255) or (vPos = vStart) then
      Exit;
    AValue := (AValue shl 8) or Cardinal(vNum);
    Inc(vParts);
    Inc(vPos);
  end;
  Result := vParts = 4;
end;

/// ---- TGwValues ----

constructor TGwValues.Create;
begin
  inherited Create;
  FNames := TStringList.Create;
  FPlain := TStringList.Create;
  FJson := TStringList.Create;
end;

destructor TGwValues.Destroy;
begin
  FJson.Free;
  FPlain.Free;
  FNames.Free;
  inherited Destroy;
end;

procedure TGwValues.AddRaw(const AName, AJson, APlain: StringRAL);
begin
  FNames.Add(string(AName));
  FPlain.Add(string(APlain));
  FJson.Add(string(AJson));
end;

procedure TGwValues.AddBool(const AName: StringRAL; AValue: Boolean);
begin
  if AValue then
    AddRaw(AName, 'true', 'true')
  else
    AddRaw(AName, 'false', 'false');
end;

procedure TGwValues.AddNumber(const AName, AText: StringRAL);
begin
  AddRaw(AName, AText, AText);
end;

procedure TGwValues.AddStr(const AName, AValue: StringRAL);
begin
  AddRaw(AName, JsonLiteral(AValue), AValue);
end;

function TGwValues.Find(const AName: StringRAL; out APlain, AJson: StringRAL): Boolean;
var
  vIdx: Integer;
begin
  vIdx := FNames.IndexOf(string(AName));
  Result := vIdx >= 0;
  if Result then
  begin
    APlain := StringRAL(FPlain[vIdx]);
    AJson := StringRAL(FJson[vIdx]);
  end;
end;

/// ---- TRALChatGatewayVerifier ----

constructor TRALChatGatewayVerifier.Create;
begin
  inherited Create;
  FAllowedIPs := TStringList.Create;
  FMode := gvSecret;
  FHash := ghSHA256;
  FSignatureEncoding := seHex;
  FTemplate := '{body}';
end;

destructor TRALChatGatewayVerifier.Destroy;
begin
  FAllowedIPs.Free;
  inherited Destroy;
end;

procedure TRALChatGatewayVerifier.Assign(Source: TPersistent);
var
  vSource: TRALChatGatewayVerifier;
begin
  if Source is TRALChatGatewayVerifier then
  begin
    vSource := TRALChatGatewayVerifier(Source);
    FAllowedIPs.Assign(vSource.FAllowedIPs);
    FHash := vSource.FHash;
    FHeaderName := vSource.FHeaderName;
    FIPHeader := vSource.FIPHeader;
    FMode := vSource.FMode;
    FPrefix := vSource.FPrefix;
    FSecret := vSource.FSecret;
    FSignatureEncoding := vSource.FSignatureEncoding;
    FTemplate := vSource.FTemplate;
    FTimestampHeader := vSource.FTimestampHeader;
  end
  else
    inherited Assign(Source);
end;

function TRALChatGatewayVerifier.IPAllowed(const AIp: StringRAL): Boolean;
var
  vIdx, vSlash, vBits: Integer;
  vEntry: string;
  vIp, vNet, vMask: Cardinal;
  vText: string;
begin
  Result := False;
  vText := string(AIp);
  if Copy(vText, 1, 7) = '::ffff:' then
    Delete(vText, 1, 7);
  for vIdx := 0 to FAllowedIPs.Count - 1 do
  begin
    vEntry := Trim(FAllowedIPs[vIdx]);
    if vEntry = '' then
      Continue;
    vSlash := Pos('/', vEntry);
    if vSlash = 0 then
      Result := SameText(vEntry, vText)
    else
    begin
      vBits := StrToIntDef(Copy(vEntry, vSlash + 1, MaxInt), -1);
      if (vBits < 0) or (vBits > 32) or not ParseIPv4(StringRAL(Copy(vEntry, 1, vSlash - 1)), vNet) or
         not ParseIPv4(StringRAL(vText), vIp) then
        Continue;
      if vBits = 0 then
        vMask := 0
      else
        vMask := Cardinal($FFFFFFFF) shl (32 - vBits);
      Result := (vIp and vMask) = (vNet and vMask);
    end;
    if Result then
      Exit;
  end;
end;

function TRALChatGatewayVerifier.IsOpen: Boolean;
begin
  Result := FMode = gvNone;
end;

procedure TRALChatGatewayVerifier.SetAllowedIPs(AValue: TStrings);
begin
  FAllowedIPs.Assign(AValue);
end;

function TRALChatGatewayVerifier.Sign(const ASigned: StringRAL): StringRAL;
var
  vHash: TRALHashBase;
begin
  case FHash of
    ghSHA1:
      vHash := TRALSHA1.Create;
    ghSHA512:
    begin
      vHash := TRALSHA2_64.Create;
      TRALSHA2_64(vHash).Version := rsv512;
    end;
  else
    vHash := TRALSHA2_32.Create;
    TRALSHA2_32(vHash).Version := rsv256;
  end;
  try
    if FSignatureEncoding = seBase64 then
      vHash.OutputType := rhotBase64
    else
      vHash.OutputType := rhotHex;
    Result := vHash.HMACAsString(ASigned, FSecret);
  finally
    vHash.Free;
  end;
end;

function TRALChatGatewayVerifier.Verify(const ABody: StringRAL; AHeaders: TRALChatHeaderReader;
  out ATimestamp: Int64RAL): Boolean;
var
  vGot, vWant, vTs, vSigned, vIp: StringRAL;
  vPos: Integer;
begin
  ATimestamp := 0;
  Result := False;
  case FMode of
    gvNone:
      Result := True;
    gvSecret:
      Result := (FSecret <> '') and (FHeaderName <> '') and Assigned(AHeaders) and
        RALSameSecret(AHeaders(FHeaderName), FSecret);
    gvIPList:
    begin
      if not Assigned(AHeaders) then
        Exit;
      if FIPHeader <> '' then
      begin
        vIp := AHeaders(FIPHeader);
        vPos := Pos(',', string(vIp));
        if vPos > 0 then
          vIp := Copy(vIp, 1, vPos - 1);
        vIp := StringRAL(Trim(string(vIp)));
      end
      else
        vIp := AHeaders(cRALChatVerifyClientIP);
      Result := (vIp <> '') and IPAllowed(vIp);
    end;
    gvHMAC:
    begin
      if (FSecret = '') or (FHeaderName = '') or not Assigned(AHeaders) then
        Exit;
      vGot := AHeaders(FHeaderName);
      if (vGot = '') or (Copy(vGot, 1, Length(FPrefix)) <> FPrefix) then
        Exit;
      vGot := Copy(vGot, Length(FPrefix) + 1, MaxInt);
      vTs := '';
      if FTimestampHeader <> '' then
      begin
        vTs := AHeaders(FTimestampHeader);
        ATimestamp := StrToInt64Def(string(vTs), 0);
        if ATimestamp = 0 then
          Exit;
      end;
      // the template is expanded around the body without touching the body's bytes
      vSigned := FTemplate;
      vPos := Pos('{ts}', string(vSigned));
      if vPos > 0 then
        vSigned := Copy(vSigned, 1, vPos - 1) + vTs + Copy(vSigned, vPos + 4, MaxInt);
      vPos := Pos('{body}', string(vSigned));
      if vPos > 0 then
        vSigned := Copy(vSigned, 1, vPos - 1) + ABody + Copy(vSigned, vPos + 6, MaxInt)
      else
        vSigned := vSigned + ABody;
      vWant := Sign(vSigned);
      if FSignatureEncoding = seHex then
      begin
        vWant := StringRAL(LowerCase(string(vWant)));
        vGot := StringRAL(LowerCase(string(vGot)));
      end;
      Result := RALSameSecret(vGot, vWant);
    end;
  end;
end;

/// ---- TRALChatGatewayEndpoint ----

constructor TRALChatGatewayEndpoint.Create;
begin
  inherited Create;
  FMethod := gmPost;
end;

procedure TRALChatGatewayEndpoint.Assign(Source: TPersistent);
begin
  if Source is TRALChatGatewayEndpoint then
  begin
    FBody := TRALChatGatewayEndpoint(Source).FBody;
    FMethod := TRALChatGatewayEndpoint(Source).FMethod;
    SetUrl(TRALChatGatewayEndpoint(Source).FUrl);
  end
  else
    inherited Assign(Source);
end;

procedure TRALChatGatewayEndpoint.SetBody(const AValue: StringRAL);
begin
  FBody := AValue;
end;

procedure TRALChatGatewayEndpoint.SetMethod(AValue: TRALChatGatewayMethod);
begin
  FMethod := AValue;
end;

procedure TRALChatGatewayEndpoint.SetUrl(const AValue: StringRAL);
begin
  FUrl := AValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TRALChatGatewayEndpoint.Setup(const AUrl, ABody: StringRAL; AMethod: TRALChatGatewayMethod);
begin
  FBody := ABody;
  FMethod := AMethod;
  SetUrl(AUrl);
end;

/// ---- TRALChatGatewayOutgoing ----

constructor TRALChatGatewayOutgoing.Create;
begin
  inherited Create;
  FAudio := NewEndpoint;
  FButtons := NewEndpoint;
  FContact := NewEndpoint;
  FDelete := NewEndpoint;
  FDocument := NewEndpoint;
  FEdit := NewEndpoint;
  FImage := NewEndpoint;
  FList := NewEndpoint;
  FLocation := NewEndpoint;
  FSticker := NewEndpoint;
  FText := NewEndpoint;
  FTyping := NewEndpoint;
  FVideo := NewEndpoint;
end;

destructor TRALChatGatewayOutgoing.Destroy;
begin
  FVideo.Free;
  FTyping.Free;
  FText.Free;
  FSticker.Free;
  FLocation.Free;
  FList.Free;
  FImage.Free;
  FEdit.Free;
  FDocument.Free;
  FDelete.Free;
  FContact.Free;
  FButtons.Free;
  FAudio.Free;
  inherited Destroy;
end;

procedure TRALChatGatewayOutgoing.Assign(Source: TPersistent);
var
  vSource: TRALChatGatewayOutgoing;
begin
  if Source is TRALChatGatewayOutgoing then
  begin
    vSource := TRALChatGatewayOutgoing(Source);
    FAudio.Assign(vSource.FAudio);
    FButtons.Assign(vSource.FButtons);
    FContact.Assign(vSource.FContact);
    FDelete.Assign(vSource.FDelete);
    FDocument.Assign(vSource.FDocument);
    FEdit.Assign(vSource.FEdit);
    FImage.Assign(vSource.FImage);
    FList.Assign(vSource.FList);
    FLocation.Assign(vSource.FLocation);
    FSticker.Assign(vSource.FSticker);
    FText.Assign(vSource.FText);
    FTyping.Assign(vSource.FTyping);
    FVideo.Assign(vSource.FVideo);
    FButtonItem := vSource.FButtonItem;
    FErrorPath := vSource.FErrorPath;
    FMessageIdPath := vSource.FMessageIdPath;
    FRowItem := vSource.FRowItem;
  end
  else
    inherited Assign(Source);
end;

procedure TRALChatGatewayOutgoing.Changed(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TRALChatGatewayOutgoing.Clear;
begin
  FAudio.Setup('', '');
  FButtons.Setup('', '');
  FContact.Setup('', '');
  FDelete.Setup('', '');
  FDocument.Setup('', '');
  FEdit.Setup('', '');
  FImage.Setup('', '');
  FList.Setup('', '');
  FLocation.Setup('', '');
  FSticker.Setup('', '');
  FText.Setup('', '');
  FTyping.Setup('', '');
  FVideo.Setup('', '');
  FButtonItem := '';
  FErrorPath := '';
  FMessageIdPath := '';
  FRowItem := '';
end;

function TRALChatGatewayOutgoing.NewEndpoint: TRALChatGatewayEndpoint;
begin
  Result := TRALChatGatewayEndpoint.Create;
  Result.OnChange := Changed;
end;

procedure TRALChatGatewayOutgoing.SetAudio(AValue: TRALChatGatewayEndpoint);
begin
  FAudio.Assign(AValue);
end;

procedure TRALChatGatewayOutgoing.SetButtons(AValue: TRALChatGatewayEndpoint);
begin
  FButtons.Assign(AValue);
end;

procedure TRALChatGatewayOutgoing.SetContact(AValue: TRALChatGatewayEndpoint);
begin
  FContact.Assign(AValue);
end;

procedure TRALChatGatewayOutgoing.SetDelete(AValue: TRALChatGatewayEndpoint);
begin
  FDelete.Assign(AValue);
end;

procedure TRALChatGatewayOutgoing.SetDocument(AValue: TRALChatGatewayEndpoint);
begin
  FDocument.Assign(AValue);
end;

procedure TRALChatGatewayOutgoing.SetEdit(AValue: TRALChatGatewayEndpoint);
begin
  FEdit.Assign(AValue);
end;

procedure TRALChatGatewayOutgoing.SetImage(AValue: TRALChatGatewayEndpoint);
begin
  FImage.Assign(AValue);
end;

procedure TRALChatGatewayOutgoing.SetList(AValue: TRALChatGatewayEndpoint);
begin
  FList.Assign(AValue);
end;

procedure TRALChatGatewayOutgoing.SetLocation(AValue: TRALChatGatewayEndpoint);
begin
  FLocation.Assign(AValue);
end;

procedure TRALChatGatewayOutgoing.SetSticker(AValue: TRALChatGatewayEndpoint);
begin
  FSticker.Assign(AValue);
end;

procedure TRALChatGatewayOutgoing.SetText(AValue: TRALChatGatewayEndpoint);
begin
  FText.Assign(AValue);
end;

procedure TRALChatGatewayOutgoing.SetTyping(AValue: TRALChatGatewayEndpoint);
begin
  FTyping.Assign(AValue);
end;

procedure TRALChatGatewayOutgoing.SetVideo(AValue: TRALChatGatewayEndpoint);
begin
  FVideo.Assign(AValue);
end;

/// ---- TRALChatGatewayIncoming ----

constructor TRALChatGatewayIncoming.Create;
begin
  inherited Create;
  FKindRules := TStringList.Create;
  FMediaHosts := TStringList.Create;
end;

destructor TRALChatGatewayIncoming.Destroy;
begin
  FMediaHosts.Free;
  FKindRules.Free;
  inherited Destroy;
end;

procedure TRALChatGatewayIncoming.Assign(Source: TPersistent);
var
  vSource: TRALChatGatewayIncoming;
begin
  if Source is TRALChatGatewayIncoming then
  begin
    vSource := TRALChatGatewayIncoming(Source);
    FButtonIdPath := vSource.FButtonIdPath;
    FButtonTextPath := vSource.FButtonTextPath;
    FCaptionPath := vSource.FCaptionPath;
    FChatIdPath := vSource.FChatIdPath;
    FChatIdStrip := vSource.FChatIdStrip;
    FContactNamePath := vSource.FContactNamePath;
    FContactPhonePath := vSource.FContactPhonePath;
    FEventIdPath := vSource.FEventIdPath;
    FEventPath := vSource.FEventPath;
    FEventValues := vSource.FEventValues;
    FFileNamePath := vSource.FFileNamePath;
    FFromMePath := vSource.FFromMePath;
    FGroupMark := vSource.FGroupMark;
    FGroupPath := vSource.FGroupPath;
    FItemsPath := vSource.FItemsPath;
    FKindRules.Assign(vSource.FKindRules);
    FLatitudePath := vSource.FLatitudePath;
    FLongitudePath := vSource.FLongitudePath;
    FMediaBase64Path := vSource.FMediaBase64Path;
    FMediaHosts.Assign(vSource.FMediaHosts);
    FMediaUrlPath := vSource.FMediaUrlPath;
    FMessageIdPath := vSource.FMessageIdPath;
    FMimePath := vSource.FMimePath;
    FPlaceNamePath := vSource.FPlaceNamePath;
    FSenderIdPath := vSource.FSenderIdPath;
    FSenderNamePath := vSource.FSenderNamePath;
    FSenderPhonePath := vSource.FSenderPhonePath;
    FTextPath := vSource.FTextPath;
    FTimestampPath := vSource.FTimestampPath;
  end
  else
    inherited Assign(Source);
end;

procedure TRALChatGatewayIncoming.Clear;
begin
  FButtonIdPath := '';
  FButtonTextPath := '';
  FCaptionPath := '';
  FChatIdPath := '';
  FChatIdStrip := '';
  FContactNamePath := '';
  FContactPhonePath := '';
  FEventIdPath := '';
  FEventPath := '';
  FEventValues := '';
  FFileNamePath := '';
  FFromMePath := '';
  FGroupMark := '';
  FGroupPath := '';
  FItemsPath := '';
  FKindRules.Clear;
  FLatitudePath := '';
  FLongitudePath := '';
  FMediaBase64Path := '';
  FMediaHosts.Clear;
  FMediaUrlPath := '';
  FMessageIdPath := '';
  FMimePath := '';
  FPlaceNamePath := '';
  FSenderIdPath := '';
  FSenderNamePath := '';
  FSenderPhonePath := '';
  FTextPath := '';
  FTimestampPath := '';
end;

procedure TRALChatGatewayIncoming.SetKindRules(AValue: TStrings);
begin
  FKindRules.Assign(AValue);
end;

procedure TRALChatGatewayIncoming.SetMediaHosts(AValue: TStrings);
begin
  FMediaHosts.Assign(AValue);
end;

/// ---- TRALChatGateway ----

constructor TRALChatGateway.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCallLock := TCriticalSection.Create;
  FLock := TCriticalSection.Create;
  FHeaders := TStringList.Create;
  FIncoming := TRALChatGatewayIncoming.Create;
  FOutgoing := TRALChatGatewayOutgoing.Create;
  FOutgoing.OnChange := OutgoingChanged;
  FMaxDownloadSize := 16777216;
  ChannelName := 'gateway';
  Capabilities.MaxText := 4096;
  Capabilities.MaxButtons := 3;
  Capabilities.MaxMediaBytes := 16777216;
  // unofficial WhatsApp numbers get banned: a message a second in all, one every 3 s to
  // a chat
  Rates.Total := 1;
  Rates.TotalBurst := 2;
  Rates.PerChat := 1 / 3;
  Rates.PerChatBurst := 2;
  Rates.PerPair := 1 / 3;
  Rates.PerPairBurst := 2;
  FVerifier := TRALChatGatewayVerifier.Create;
  Verifier := FVerifier;
  RefreshCapabilities;
end;

destructor TRALChatGateway.Destroy;
begin
  FOwnClient.Free;
  FOutgoing.Free;
  FIncoming.Free;
  FHeaders.Free;
  FLock.Free;
  FCallLock.Free;
  inherited Destroy;
end;

procedure TRALChatGateway.AddHeaders(AClient: TRALClient; AValues: TObject);
var
  vIdx, vPos: Integer;
  vLine: string;
begin
  if (FAuthHeader <> '') and (FAuthValue <> '') then
    AClient.Request.AddHeader(FAuthHeader, Expand(FAuthValue, TGwValues(AValues), cModePlain));
  for vIdx := 0 to FHeaders.Count - 1 do
  begin
    vLine := FHeaders[vIdx];
    vPos := Pos(':', vLine);
    if vPos > 1 then
      AClient.Request.AddHeader(StringRAL(Trim(Copy(vLine, 1, vPos - 1))),
        Expand(StringRAL(Trim(Copy(vLine, vPos + 1, MaxInt))), TGwValues(AValues), cModePlain));
  end;
end;

procedure TRALChatGateway.ApplyAnswer(AStatus: Integer; const AText: StringRAL;
  AResponse: TRALResponse; var AResult: TRALChatSendResult);
var
  vRoot: TRALJSON;
  vError, vRetry: StringRAL;
begin
  vRoot := nil;
  try
    vRoot := TRALJSON.ParseJSON(AText);
  except
    FreeAndNil(vRoot);
  end;
  try
    if (AStatus >= 200) and (AStatus < 300) then
    begin
      AResult.Ok := True;
      AResult.MessageId := PathText(vRoot, FOutgoing.MessageIdPath);
      Exit;
    end;
    vError := PathText(vRoot, FOutgoing.ErrorPath);
    if vError = '' then
      vError := Copy(StringRAL(Trim(string(AText))), 1, cMaxErrorText);
    if vError = '' then
      AResult.ErrorText := Hide(StringRAL(Format(emChatGwHttpEmpty, [string(ChannelName), AStatus])))
    else
      AResult.ErrorText := Hide(StringRAL(Format(emChatGwHttp, [string(ChannelName), AStatus,
        string(Copy(vError, 1, cMaxErrorText))])));
    AResult.RetryScope := rsChannel;
    if AStatus = 429 then
    begin
      AResult.Transient := True;
      vRetry := AResponse.GetHeader('Retry-After');
      AResult.RetryAfterMs := StrToInt64Def(string(vRetry), 0) * 1000;
    end
    else if (AStatus = 408) or (AStatus >= 500) then
      AResult.Transient := True;
  finally
    vRoot.Free;
  end;
end;

procedure TRALChatGateway.ApplyProfile(AProfile: TRALChatGatewayProfile);
const
  cSend = '"phone":{chat},"isGroup":{isgroup}';
var
  vIn: TRALChatGatewayIncoming;
  vOut: TRALChatGatewayOutgoing;
  vPre: StringRAL;
begin
  FProfile := AProfile;
  if AProfile = gpCustom then
    Exit;
  vIn := FIncoming;
  vOut := FOutgoing;
  vIn.Clear;
  vOut.Clear;
  FHeaders.Clear;
  FVerifier.AllowedIPs.Clear;
  case AProfile of
    gpEvolution:
    begin
      // Evolution API v2: https://doc.evolution-api.com
      FBaseUrl := 'http://localhost:8080';
      FAuthHeader := 'apikey';
      FAuthValue := '{token}';
      vOut.Text.Setup('/message/sendText/{instance}', '{"number":{chat},"text":{text}}');
      vPre := '{"number":{chat},';
      vOut.Image.Setup('/message/sendMedia/{instance}', vPre +
        '"mediatype":"image","mimetype":{mime},"caption":{caption},"media":{media},"fileName":{filename}}');
      vOut.Video.Setup('/message/sendMedia/{instance}', vPre +
        '"mediatype":"video","mimetype":{mime},"caption":{caption},"media":{media},"fileName":{filename}}');
      vOut.Document.Setup('/message/sendMedia/{instance}', vPre +
        '"mediatype":"document","mimetype":{mime},"caption":{caption},"media":{media},"fileName":{filename}}');
      vOut.Audio.Setup('/message/sendWhatsAppAudio/{instance}', vPre + '"audio":{media}}');
      vOut.Sticker.Setup('/message/sendSticker/{instance}', vPre + '"sticker":{media}}');
      vOut.Location.Setup('/message/sendLocation/{instance}', vPre +
        '"name":{title},"address":{address},"latitude":{latitude},"longitude":{longitude}}');
      vOut.Contact.Setup('/message/sendContact/{instance}', vPre +
        '"contact":[{"fullName":{contactname},"wuid":{contactphone},"phoneNumber":{contactphone}}]}');
      vOut.Buttons.Setup('/message/sendButtons/{instance}', vPre +
        '"title":{text},"description":{text},"footer":{footer},"buttons":{buttons}}');
      vOut.ButtonItem := '{"type":"reply","displayText":{caption},"id":{id}}';
      vOut.List.Setup('/message/sendList/{instance}', vPre +
        '"title":{title},"description":{text},"buttonText":{buttontext},"footerText":{footer},' +
        '"values":[{"title":{title},"rows":{rows}}]}');
      vOut.RowItem := '{"title":{title},"description":{description},"rowId":{id}}';
      vOut.Typing.Setup('/chat/sendPresence/{instance}', vPre + '"delay":3000,"presence":"composing"}');
      vOut.Edit.Setup('/chat/updateMessage/{instance}', vPre +
        '"text":{text},"key":{"remoteJid":{jid},"fromMe":true,"id":{target}}}');
      vOut.Delete.Setup('/chat/deleteMessageForEveryone/{instance}',
        '{"id":{target},"remoteJid":{jid},"fromMe":true}', gmDelete);
      vOut.MessageIdPath := 'key.id';
      vOut.ErrorPath := 'response.message.0;response.message;message;error';
      vIn.EventPath := 'event';
      vIn.EventValues := 'messages.upsert';
      vIn.ItemsPath := 'data';
      vIn.EventIdPath := 'key.id';
      vIn.MessageIdPath := 'key.id';
      vIn.FromMePath := 'key.fromMe';
      vIn.ChatIdPath := 'key.remoteJid';
      vIn.ChatIdStrip := '@s.whatsapp.net';
      vIn.GroupMark := '@g.us';
      vIn.SenderIdPath := 'key.participant;key.remoteJid';
      vIn.SenderNamePath := 'pushName';
      vIn.TimestampPath := 'messageTimestamp';
      vIn.TextPath := 'message.conversation;message.extendedTextMessage.text';
      vIn.CaptionPath := 'message.imageMessage.caption;message.videoMessage.caption;message.documentMessage.caption';
      vIn.MediaBase64Path := 'message.base64';
      vIn.MimePath := 'message.imageMessage.mimetype;message.audioMessage.mimetype;' +
        'message.videoMessage.mimetype;message.documentMessage.mimetype;message.stickerMessage.mimetype';
      vIn.FileNamePath := 'message.documentMessage.fileName';
      vIn.LatitudePath := 'message.locationMessage.degreesLatitude';
      vIn.LongitudePath := 'message.locationMessage.degreesLongitude';
      vIn.PlaceNamePath := 'message.locationMessage.name';
      vIn.ContactNamePath := 'message.contactMessage.displayName';
      vIn.ContactPhonePath := 'message.contactMessage.vcard';
      vIn.ButtonIdPath := 'message.buttonsResponseMessage.selectedButtonId;' +
        'message.listResponseMessage.singleSelectReply.selectedRowId;message.templateButtonReplyMessage.selectedId';
      vIn.ButtonTextPath := 'message.buttonsResponseMessage.selectedDisplayText;' +
        'message.listResponseMessage.title;message.templateButtonReplyMessage.selectedDisplayText';
      vIn.KindRules.Add('message.imageMessage||image');
      vIn.KindRules.Add('message.audioMessage||audio');
      vIn.KindRules.Add('message.videoMessage||video');
      vIn.KindRules.Add('message.documentMessage||document');
      vIn.KindRules.Add('message.documentWithCaptionMessage||document');
      vIn.KindRules.Add('message.stickerMessage||sticker');
      vIn.KindRules.Add('message.locationMessage||location');
      vIn.KindRules.Add('message.contactMessage||contact');
      vIn.KindRules.Add('message.buttonsResponseMessage||callback');
      vIn.KindRules.Add('message.listResponseMessage||callback');
      vIn.KindRules.Add('message.templateButtonReplyMessage||callback');
      // the instance webhook can carry headers of your choice: send this one with the
      // secret
      FVerifier.Mode := gvSecret;
      FVerifier.HeaderName := 'X-Webhook-Secret';
    end;
    gpWPPConnect:
    begin
      // WPPConnect Server: https://wppconnect.io
      FBaseUrl := 'http://localhost:21465';
      FAuthHeader := 'Authorization';
      FAuthValue := 'Bearer {token}';
      vPre := '/api/{instance}/';
      vOut.Text.Setup(vPre + 'send-message', '{' + cSend + ',"isNewsletter":false,"message":{text}}');
      vOut.Image.Setup(vPre + 'send-image', '{' + cSend +
        ',"filename":{filename},"caption":{caption},"base64":{mediauri}}');
      vOut.Video.Setup(vPre + 'send-file', '{' + cSend +
        ',"filename":{filename},"message":{caption},"base64":{mediauri}}');
      vOut.Document.Setup(vPre + 'send-file', '{' + cSend +
        ',"filename":{filename},"message":{caption},"base64":{mediauri}}');
      vOut.Audio.Setup(vPre + 'send-voice-base64', '{' + cSend + ',"base64Ptt":{mediauri}}');
      vOut.Sticker.Setup(vPre + 'send-sticker', '{' + cSend + ',"path":{mediauri}}');
      vOut.Location.Setup(vPre + 'send-location', '{' + cSend +
        ',"lat":{latitude},"lng":{longitude},"title":{title},"address":{address}}');
      vOut.Contact.Setup(vPre + 'contact-vcard', '{' + cSend + ',"contactsId":[{contactphone}]}');
      vOut.Buttons.Setup(vPre + 'send-buttons', '{' + cSend +
        ',"message":{text},"options":{"useTemplateButtons":true,"buttons":{buttons}}}');
      vOut.ButtonItem := '{"id":{id},"text":{caption}}';
      vOut.List.Setup(vPre + 'send-list-message', '{' + cSend +
        ',"description":{text},"buttonText":{buttontext},"sections":[{"title":{title},"rows":{rows}}]}');
      vOut.RowItem := '{"rowId":{id},"title":{title},"description":{description}}';
      vOut.Typing.Setup(vPre + 'typing', '{' + cSend + ',"value":true}');
      vOut.Edit.Setup(vPre + 'edit-message', '{"id":{target},"newText":{text}}');
      vOut.Delete.Setup(vPre + 'delete-message', '{' + cSend + ',"messageId":{target}}');
      vOut.MessageIdPath := 'response.0.id;response.id';
      vOut.ErrorPath := 'message;error;response.message';
      vIn.EventPath := 'event';
      vIn.EventValues := 'onmessage';
      vIn.EventIdPath := 'id';
      vIn.MessageIdPath := 'id';
      vIn.FromMePath := 'fromMe';
      vIn.ChatIdPath := 'chatId;from';
      vIn.ChatIdStrip := '@c.us;@s.whatsapp.net';
      vIn.GroupPath := 'isGroupMsg';
      vIn.GroupMark := '@g.us';
      vIn.SenderIdPath := 'author;sender.id;from';
      vIn.SenderNamePath := 'notifyName;sender.pushname;sender.name';
      vIn.TimestampPath := 't';
      vIn.TextPath := 'body';
      vIn.CaptionPath := 'caption';
      vIn.MediaBase64Path := 'body';
      vIn.MimePath := 'mimetype';
      vIn.FileNamePath := 'filename';
      vIn.LatitudePath := 'lat';
      vIn.LongitudePath := 'lng';
      vIn.PlaceNamePath := 'loc';
      vIn.ContactNamePath := 'body';
      vIn.ContactPhonePath := 'body';
      vIn.ButtonIdPath := 'selectedButtonId;selectedRowId';
      vIn.ButtonTextPath := 'content;body';
      vIn.KindRules.Add('type|image|image');
      vIn.KindRules.Add('type|ptt|audio');
      vIn.KindRules.Add('type|audio|audio');
      vIn.KindRules.Add('type|video|video');
      vIn.KindRules.Add('type|document|document');
      vIn.KindRules.Add('type|sticker|sticker');
      vIn.KindRules.Add('type|location|location');
      vIn.KindRules.Add('type|vcard|contact');
      vIn.KindRules.Add('type|buttons_response|callback');
      vIn.KindRules.Add('type|list_response|callback');
      // the server signs nothing and has no header to configure: the sender's address is
      // the only proof; fill AllowedIPs, or leave gvNone and accept the warning in the
      // log
      FVerifier.Mode := gvIPList;
      FVerifier.IPHeader := '';
    end;
    gpZAPI:
    begin
      // Z-API: https://developer.z-api.io
      FBaseUrl := 'https://api.z-api.io';
      FAuthHeader := 'Client-Token';
      FAuthValue := '{token}';
      vPre := '/instances/{instance}/token/{instancekey}/';
      vOut.Text.Setup(vPre + 'send-text', '{"phone":{chat},"message":{text}}');
      vOut.Image.Setup(vPre + 'send-image', '{"phone":{chat},"image":{mediauri},"caption":{caption}}');
      vOut.Video.Setup(vPre + 'send-video', '{"phone":{chat},"video":{mediauri},"caption":{caption}}');
      vOut.Audio.Setup(vPre + 'send-audio', '{"phone":{chat},"audio":{mediauri}}');
      vOut.Document.Setup(vPre + 'send-document/{extension}',
        '{"phone":{chat},"document":{mediauri},"fileName":{filename},"caption":{caption}}');
      vOut.Sticker.Setup(vPre + 'send-sticker', '{"phone":{chat},"sticker":{mediauri}}');
      vOut.Location.Setup(vPre + 'send-location',
        '{"phone":{chat},"title":{title},"address":{address},"latitude":{latitude},"longitude":{longitude}}');
      vOut.Contact.Setup(vPre + 'send-contact',
        '{"phone":{chat},"contactName":{contactname},"contactPhone":{contactphone}}');
      vOut.Buttons.Setup(vPre + 'send-button-list',
        '{"phone":{chat},"message":{text},"buttonList":{"buttons":{buttons}}}');
      vOut.ButtonItem := '{"id":{id},"label":{caption}}';
      vOut.List.Setup(vPre + 'send-option-list',
        '{"phone":{chat},"message":{text},"optionList":{"title":{title},"buttonLabel":{buttontext},"options":{rows}}}');
      vOut.RowItem := '{"id":{id},"title":{title},"description":{description}}';
      vOut.Delete.Setup(vPre + 'messages?messageId={target}&phone={chat}&owner=true', '', gmDelete);
      vOut.MessageIdPath := 'messageId;id';
      vOut.ErrorPath := 'error;message';
      vIn.EventPath := 'type';
      vIn.EventValues := 'ReceivedCallback';
      vIn.EventIdPath := 'messageId';
      vIn.MessageIdPath := 'messageId';
      vIn.FromMePath := 'fromMe';
      vIn.ChatIdPath := 'phone';
      vIn.GroupPath := 'isGroup';
      vIn.GroupMark := '-group';
      vIn.SenderIdPath := 'participantPhone;phone';
      vIn.SenderNamePath := 'senderName;chatName';
      vIn.TimestampPath := 'momment';
      vIn.TextPath := 'text.message';
      vIn.CaptionPath := 'image.caption;video.caption;document.caption';
      vIn.MediaUrlPath := 'image.imageUrl;audio.audioUrl;video.videoUrl;document.documentUrl;sticker.stickerUrl';
      vIn.MimePath := 'image.mimeType;audio.mimeType;video.mimeType;document.mimeType;sticker.mimeType';
      vIn.FileNamePath := 'document.fileName';
      vIn.LatitudePath := 'location.latitude';
      vIn.LongitudePath := 'location.longitude';
      vIn.PlaceNamePath := 'location.name';
      vIn.ContactNamePath := 'contact.displayName';
      vIn.ContactPhonePath := 'contact.vCard';
      vIn.ButtonIdPath := 'buttonsResponseMessage.buttonId;listResponseMessage.selectedRowId';
      vIn.ButtonTextPath := 'buttonsResponseMessage.message;listResponseMessage.title';
      vIn.MediaHosts.Add('z-api.io');
      vIn.MediaHosts.Add('backblazeb2.com');
      vIn.KindRules.Add('image.imageUrl||image');
      vIn.KindRules.Add('audio.audioUrl||audio');
      vIn.KindRules.Add('video.videoUrl||video');
      vIn.KindRules.Add('document.documentUrl||document');
      vIn.KindRules.Add('sticker.stickerUrl||sticker');
      vIn.KindRules.Add('location.latitude||location');
      vIn.KindRules.Add('contact.displayName||contact');
      vIn.KindRules.Add('buttonsResponseMessage||callback');
      vIn.KindRules.Add('listResponseMessage||callback');
      FVerifier.Mode := gvSecret;
      FVerifier.HeaderName := 'Client-Token';
    end;
  end;
  RefreshCapabilities;
end;

function TRALChatGateway.BuildValues(AMessage: TRALChatMessage; const ATarget: StringRAL): TObject;
var
  vValues: TGwValues;
  vChat, vJid: StringRAL;
  vGroup: Boolean;
begin
  vValues := TGwValues.Create;
  Result := vValues;
  try
    vChat := AMessage.Chat.Id;
    vGroup := AMessage.Chat.ChatType = cctGroup;
    vJid := vChat;
    if Pos('@', string(vChat)) = 0 then
      if vGroup then
        vJid := vChat + '@g.us'
      else
        vJid := vChat + '@s.whatsapp.net';
    vValues.AddStr('chat', vChat);
    vValues.AddStr('jid', vJid);
    vValues.AddBool('isgroup', vGroup);
    vValues.AddStr('text', AMessage.Text);
    vValues.AddStr('caption', AMessage.Text);
    vValues.AddStr('footer', '');
    vValues.AddStr('target', ATarget);
    vValues.AddStr('instance', FInstance);
    vValues.AddStr('instancekey', FInstanceKey);
    if AMessage.HasReplyTo then
      vValues.AddStr('reply', AMessage.ReplyTo.Id)
    else
      vValues.AddStr('reply', '');
    vValues.AddStr('buttontext', AMessage.List.ButtonText);
    if AMessage.Location.Valid then
    begin
      vValues.AddNumber('latitude', FloatText(AMessage.Location.Latitude));
      vValues.AddNumber('longitude', FloatText(AMessage.Location.Longitude));
      vValues.AddStr('title', AMessage.Location.Title);
      vValues.AddStr('address', AMessage.Location.Title);
    end
    else
    begin
      vValues.AddStr('title', AMessage.List.Title);
      vValues.AddStr('address', '');
    end;
    if AMessage.Contact.Valid then
    begin
      vValues.AddStr('contactname', AMessage.Contact.Name);
      vValues.AddStr('contactphone', AMessage.Contact.Phone);
    end;
  except
    vValues.Free;
    raise;
  end;
end;

procedure TRALChatGateway.Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
  const ATargetId: StringRAL; out AResult: TRALChatSendResult);
var
  vEndpoint: TRALChatGatewayEndpoint;
  vValues: TObject;
begin
  AResult.Ok := False;
  AResult.MessageId := '';
  AResult.Transient := False;
  AResult.RetryAfterMs := 0;
  AResult.RetryScope := rsChat;
  AResult.ErrorText := '';
  AResult.Info := '';
  vValues := nil;
  try
    try
      case AKind of
        okMessage:
          vEndpoint := PickEndpoint(AMessage);
        okTyping:
          vEndpoint := FOutgoing.Typing;
        okEdit:
          vEndpoint := FOutgoing.Edit;
      else
        vEndpoint := FOutgoing.Delete;
      end;
      if (vEndpoint = nil) or (vEndpoint.Url = '') then
        raise ERALChatGatewayFinal.CreateFmt(emChatGwNoEndpoint, [string(ChannelName)]);
      vValues := BuildValues(AMessage, ATargetId);
      if AKind = okMessage then
        FillMessageValues(AMessage, vValues);
      Send(vEndpoint, vValues, AResult);
    except
      on E: ERALChatError do
      begin
        AResult.Ok := False;
        AResult.Transient := False;
        AResult.ErrorText := Hide(StringRAL(E.Message));
      end;
      on E: Exception do
      begin
        AResult.Ok := False;
        AResult.Transient := True;
        AResult.ErrorText := Hide(StringRAL(E.ClassName + ': ' + E.Message));
      end;
    end;
  finally
    vValues.Free;
  end;
end;

function TRALChatGateway.Dialect: TRALChatDialect;
begin
  Result := cdWhatsApp;
end;

procedure TRALChatGateway.FillMessageValues(AMessage: TRALChatMessage; AValues: TObject);
var
  vValues, vItem: TGwValues;
  vMedia: TRALChatMedia;
  vStream: TStream;
  vMime, vName, vKind, vUrl, vB64, vJoin, vExt: StringRAL;
  vIdx, vCount: Integer;
begin
  vValues := TGwValues(AValues);
  if AMessage.HasMedia then
  begin
    vMedia := AMessage.Media;
    vMime := vMedia.MimeType;
    vName := vMedia.FileName;
    vUrl := vMedia.Url;
    vB64 := '';
    if vMedia.Data <> nil then
    begin
      if vMedia.Data.Size > Capabilities.MaxMediaBytes then
        raise ERALChatGatewayFinal.CreateFmt(emChatGwTooBig, [Capabilities.MaxMediaBytes]);
      vStream := vMedia.Open;
      try
        vB64 := TRALBase64.Encode(vStream);
      finally
        vStream.Free;
      end;
    end;
    if (vUrl = '') and (vB64 = '') then
      raise ERALChatGatewayFinal.Create(emChatGwNoMedia);
    case AMessage.Kind of
      ckImage: vKind := 'image';
      ckAudio: vKind := 'audio';
      ckVideo: vKind := 'video';
      ckSticker: vKind := 'sticker';
      ckDocument: vKind := 'document';
    else
      vKind := 'document';
      if Copy(vMime, 1, 6) = 'image/' then
        vKind := 'image'
      else if Copy(vMime, 1, 6) = 'audio/' then
        vKind := 'audio'
      else if Copy(vMime, 1, 6) = 'video/' then
        vKind := 'video';
    end;
    if vMime = '' then
      vMime := 'application/octet-stream';
    vExt := StringRAL(StringReplace(ExtractFileExt(string(vName)), '.', '', []));
    if vExt = '' then
      vExt := 'bin';
    vValues.AddStr('mediatype', vKind);
    vValues.AddStr('mime', vMime);
    vValues.AddStr('filename', vName);
    vValues.AddStr('extension', vExt);
    vValues.AddStr('url', vUrl);
    vValues.AddStr('base64', vB64);
    vValues.AddStr('datauri', 'data:' + vMime + ';base64,' + vB64);
    if vUrl <> '' then
    begin
      vValues.AddStr('media', vUrl);
      vValues.AddStr('mediauri', vUrl);
    end
    else
    begin
      vValues.AddStr('media', vB64);
      vValues.AddStr('mediauri', 'data:' + vMime + ';base64,' + vB64);
    end;
  end;
  // the buttons and the rows of a list
  vJoin := '';
  vCount := Length(AMessage.Buttons);
  if (Capabilities.MaxButtons > 0) and (vCount > Capabilities.MaxButtons) then
    vCount := Capabilities.MaxButtons;
  for vIdx := 0 to vCount - 1 do
  begin
    vItem := TGwValues.Create;
    try
      vItem.AddStr('id', AMessage.Buttons[vIdx].Id);
      vItem.AddStr('caption', AMessage.Buttons[vIdx].Caption);
      vItem.AddStr('url', AMessage.Buttons[vIdx].Url);
      vItem.AddNumber('index', StringRAL(IntToStr(vIdx)));
      if vJoin <> '' then
        vJoin := vJoin + ',';
      vJoin := vJoin + Expand(FOutgoing.ButtonItem, vItem, cModeJson);
    finally
      vItem.Free;
    end;
  end;
  vValues.AddRaw('buttons', '[' + vJoin + ']', '');
  vJoin := '';
  for vIdx := 0 to High(AMessage.List.Items) do
  begin
    vItem := TGwValues.Create;
    try
      vItem.AddStr('id', AMessage.List.Items[vIdx].Id);
      vItem.AddStr('title', AMessage.List.Items[vIdx].Title);
      vItem.AddStr('description', AMessage.List.Items[vIdx].Description);
      vItem.AddNumber('index', StringRAL(IntToStr(vIdx)));
      if vJoin <> '' then
        vJoin := vJoin + ',';
      vJoin := vJoin + Expand(FOutgoing.RowItem, vItem, cModeJson);
    finally
      vItem.Free;
    end;
  end;
  vValues.AddRaw('rows', '[' + vJoin + ']', '');
end;

function TRALChatGateway.GetClient: TRALClient;
var
  vList: TStringList;
begin
  Result := FClient;
  if Result <> nil then
    Exit;
  FLock.Enter;
  try
    if FOwnClient = nil then
    begin
      FOwnClient := TRALClient.Create(nil);
      try
        if FEngineType <> '' then
          FOwnClient.EngineType := FEngineType
        else
        begin
          vList := TStringList.Create;
          try
            GetEngineList(vList);
            if vList.Count = 0 then
              raise ERALChatError.CreateFmt(emChatGwNoClient, [string(ChannelName)]);
            FOwnClient.EngineType := vList.Names[0];
            if FOwnClient.EngineType = '' then
              FOwnClient.EngineType := vList[0];
          finally
            vList.Free;
          end;
        end;
        FOwnClient.KeepCookies := False;
      except
        FreeAndNil(FOwnClient);
        raise;
      end;
    end;
    Result := FOwnClient;
  finally
    FLock.Leave;
  end;
end;

function TRALChatGateway.Hide(const AText: StringRAL): StringRAL;
var
  vToken: StringRAL;
begin
  Result := AText;
  vToken := '';
  try
    vToken := GetToken;
  except
    // no token: nothing to hide
  end;
  if Length(vToken) >= 4 then
    Result := StringRAL(StringReplace(string(Result), string(vToken), '***', [rfReplaceAll]));
  if Length(FInstanceKey) >= 4 then
    Result := StringRAL(StringReplace(string(Result), string(FInstanceKey), '***', [rfReplaceAll]));
end;

function TRALChatGateway.HostAllowed(const AHost: StringRAL): Boolean;
var
  vIdx: Integer;
  vEntry, vHost: string;
begin
  vHost := LowerCase(string(AHost));
  Result := (vHost <> '') and (vHost = string(HostOf(FBaseUrl)));
  for vIdx := 0 to FIncoming.MediaHosts.Count - 1 do
  begin
    if Result then
      Exit;
    vEntry := LowerCase(Trim(FIncoming.MediaHosts[vIdx]));
    Result := (vEntry <> '') and ((vHost = vEntry) or
      ((Length(vHost) > Length(vEntry)) and
       (Copy(vHost, Length(vHost) - Length(vEntry), MaxInt) = '.' + vEntry)));
  end;
end;

function TRALChatGateway.HostOf(const AUrl: StringRAL): StringRAL;
var
  vText: string;
  vPos: Integer;
begin
  vText := LowerCase(string(AUrl));
  vPos := Pos('://', vText);
  if vPos > 0 then
    Delete(vText, 1, vPos + 2);
  vPos := 1;
  while (vPos <= Length(vText)) and (vText[vPos] <> '/') and (vText[vPos] <> '?') and
         (vText[vPos] <> '#') and (vText[vPos] <> ':') do
    Inc(vPos);
  Result := StringRAL(Copy(vText, 1, vPos - 1));
end;

procedure TRALChatGateway.Loaded;
begin
  inherited Loaded;
  RefreshCapabilities;
end;

function TRALChatGateway.LoadMedia(AMedia: TRALChatMedia): TStream;
var
  vResponse: TRALResponse;
  vClient: TRALClient;
  vValues: TGwValues;
  vData: TStream;
  vHost: StringRAL;
begin
  if AMedia.Url = '' then
    raise ERALChatError.Create(emChatGwNoMedia);
  vHost := HostOf(AMedia.Url);
  if not HostAllowed(vHost) then
    raise ERALChatError.CreateFmt(emChatGwHost, [string(vHost)]);
  if (AMedia.Size > 0) and (AMedia.Size > FMaxDownloadSize) then
    raise ERALChatError.CreateFmt(emChatGwTooBig, [FMaxDownloadSize]);
  vClient := GetClient;
  vResponse := nil;
  Result := nil;
  vValues := TGwValues.Create;
  FCallLock.Enter;
  try
    try
      vValues.AddStr('token', '');
      vValues.AddStr('instance', FInstance);
      vValues.AddStr('instancekey', FInstanceKey);
      // the credentials go only to the gateway itself
      if vHost = HostOf(FBaseUrl) then
      begin
        vValues.AddStr('token', GetToken);
      end;
      vClient.Request.Clear;
      if vHost = HostOf(FBaseUrl) then
        AddHeaders(vClient, vValues);
      try
        vClient.Get(AMedia.Url, vResponse);
      except
        on E: Exception do
          raise ERALChatError.Create(string(Hide(StringRAL(E.Message))));
      end;
      if vResponse.StatusCode <> 200 then
        raise ERALChatError.CreateFmt(emChatGwDownload, [vResponse.StatusCode]);
      vData := vResponse.ResponseStream;
      if (vData <> nil) and (vData.Size > FMaxDownloadSize) then
        raise ERALChatError.CreateFmt(emChatGwTooBig, [FMaxDownloadSize]);
      Result := TMemoryStream.Create;
      try
        if vData <> nil then
        begin
          vData.Position := 0;
          Result.CopyFrom(vData, 0);
        end;
        Result.Position := 0;
      except
        FreeAndNil(Result);
        raise;
      end;
    finally
      vResponse.Free;
      vValues.Free;
    end;
  finally
    FCallLock.Leave;
  end;
end;

procedure TRALChatGateway.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
end;

procedure TRALChatGateway.OutgoingChanged(Sender: TObject);
begin
  RefreshCapabilities;
end;

procedure TRALChatGateway.ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList);
var
  vRoot, vNode: TRALJSON;
  vIdx: Integer;
  vMessage: TRALChatMessage;

  procedure ReadOne(AItem: TRALJSON);
  begin
    vMessage := TRALChatMessage.Create;
    try
      if ReadItem(AItem, vMessage) then
      begin
        vMessage.ChannelName := ChannelName;
        if KeepRaw then
          vMessage.Raw := ABody;
        AMessages.AddMessage(vMessage);
        vMessage := nil;
      end;
    finally
      vMessage.Free;
    end;
  end;

begin
  vRoot := TRALJSON.ParseJSON(ABody);
  try
    if (vRoot = nil) or not (vRoot.Kind in [rjkObject, rjkArray]) then
      raise ERALChatError.CreateFmt(emChatGwBadWebhook, [string(ChannelName)]);
    if (FIncoming.EventPath <> '') and (FIncoming.EventValues <> '') and
       not InEventList(FIncoming.EventValues, PathText(vRoot, FIncoming.EventPath)) then
      Exit;
    vNode := vRoot;
    if FIncoming.ItemsPath <> '' then
      vNode := PathNode(vRoot, FIncoming.ItemsPath);
    if vNode = nil then
      Exit;
    if vNode.Kind = rjkArray then
      for vIdx := 0 to TRALJSONArray(vNode).Count - 1 do
        ReadOne(TRALJSONArray(vNode).Items[vIdx])
    else
      ReadOne(vNode);
  finally
    vRoot.Free;
  end;
end;

function TRALChatGateway.PickEndpoint(AMessage: TRALChatMessage): TRALChatGatewayEndpoint;
var
  vMime: StringRAL;
begin
  Result := FOutgoing.Text;
  if AMessage.HasMedia then
  begin
    case AMessage.Kind of
      ckImage: Result := FOutgoing.Image;
      ckAudio: Result := FOutgoing.Audio;
      ckVideo: Result := FOutgoing.Video;
      ckSticker: Result := FOutgoing.Sticker;
      ckDocument: Result := FOutgoing.Document;
    else
      vMime := AMessage.Media.MimeType;
      if Copy(vMime, 1, 6) = 'image/' then
        Result := FOutgoing.Image
      else if Copy(vMime, 1, 6) = 'audio/' then
        Result := FOutgoing.Audio
      else if Copy(vMime, 1, 6) = 'video/' then
        Result := FOutgoing.Video
      else
        Result := FOutgoing.Document;
    end;
  end
  else if (AMessage.Kind = ckLocation) and AMessage.Location.Valid then
    Result := FOutgoing.Location
  else if (AMessage.Kind = ckContact) and AMessage.Contact.Valid then
    Result := FOutgoing.Contact
  else if (Length(AMessage.Buttons) > 0) and (FOutgoing.Buttons.Url <> '') then
    Result := FOutgoing.Buttons
  else if (Length(AMessage.List.Items) > 0) and (FOutgoing.List.Url <> '') then
    Result := FOutgoing.List;
end;

function TRALChatGateway.ReadItem(AItem: TRALJSON; AMessage: TRALChatMessage): Boolean;
var
  vIn: TRALChatGatewayIncoming;
  vChat, vSender, vText, vRule, vPath, vValue, vKindName, vB64: StringRAL;
  vInfo: TRALChatInfo;
  vUser: TRALChatUser;
  vLoc: TRALChatLocation;
  vContact: TRALChatContact;
  vIdx, vPos, vBar: Integer;
  vStream: TStream;
  vNumber: Int64;
  vGroup: Boolean;

  function Strip(const AId: StringRAL): StringRAL;
  var
    vP: Integer;
    vSuffix: StringRAL;
  begin
    Result := AId;
    vP := 1;
    while NextPath(vIn.ChatIdStrip, vP, vSuffix) do
      if (Length(Result) > Length(vSuffix)) and
         (Copy(Result, Length(Result) - Length(vSuffix) + 1, Length(vSuffix)) = vSuffix) then
        Result := Copy(Result, 1, Length(Result) - Length(vSuffix));
  end;

begin
  Result := False;
  vIn := FIncoming;
  if AItem.Kind <> rjkObject then
    Exit;
  if (vIn.FromMePath <> '') and SameText(string(PathText(AItem, vIn.FromMePath)), 'true') then
    Exit;
  vChat := PathText(AItem, vIn.ChatIdPath);
  if vChat = '' then
    Exit;
  vGroup := SameText(string(PathText(AItem, vIn.GroupPath)), 'true') or
    ((vIn.GroupMark <> '') and (Pos(string(vIn.GroupMark), string(vChat)) > 0));
  vChat := Strip(vChat);
  vInfo.Id := vChat;
  vInfo.Title := '';
  if vGroup then
    vInfo.ChatType := cctGroup
  else
    vInfo.ChatType := cctPrivate;
  AMessage.Chat := vInfo;
  vSender := Strip(PathText(AItem, vIn.SenderIdPath));
  if vSender = '' then
    vSender := vChat;
  vUser.Id := vSender;
  vUser.Name := PathText(AItem, vIn.SenderNamePath);
  vUser.Phone := PathText(AItem, vIn.SenderPhonePath);
  if (vUser.Phone = '') and IsNumber(vSender) then
    vUser.Phone := vSender;
  AMessage.From := vUser;
  AMessage.Id := PathText(AItem, vIn.MessageIdPath);
  AMessage.EventId := PathText(AItem, vIn.EventIdPath);
  vNumber := StrToInt64Def(string(PathText(AItem, vIn.TimestampPath)), 0);
  if vNumber >= 100000000000 then
    vNumber := vNumber div 1000;
  AMessage.Timestamp := vNumber;
  // the kind: the first rule that matches
  vKindName := 'text';
  for vIdx := 0 to vIn.KindRules.Count - 1 do
  begin
    vRule := StringRAL(vIn.KindRules[vIdx]);
    vBar := Pos('|', string(vRule));
    if vBar = 0 then
      Continue;
    vPath := Copy(vRule, 1, vBar - 1);
    vValue := Copy(vRule, vBar + 1, MaxInt);
    vPos := Pos('|', string(vValue));
    if vPos = 0 then
      Continue;
    vRule := StringRAL(Trim(string(Copy(vValue, vPos + 1, MaxInt))));
    vValue := Copy(vValue, 1, vPos - 1);
    if ((vValue = '') and PathExists(AItem, vPath)) or
       ((vValue <> '') and SameText(string(PathText(AItem, vPath)), string(vValue))) then
    begin
      vKindName := StringRAL(LowerCase(string(vRule)));
      Break;
    end;
  end;
  if vKindName = 'callback' then
  begin
    AMessage.Kind := ckCallback;
    AMessage.CallbackData := PathText(AItem, vIn.ButtonIdPath);
    AMessage.Text := PathText(AItem, vIn.ButtonTextPath);
    if AMessage.CallbackData = '' then
      Exit;
  end
  else if vKindName = 'location' then
  begin
    AMessage.Kind := ckLocation;
    vLoc.Valid := True;
    vLoc.Latitude := ParseFloat(PathText(AItem, vIn.LatitudePath));
    vLoc.Longitude := ParseFloat(PathText(AItem, vIn.LongitudePath));
    vLoc.Title := PathText(AItem, vIn.PlaceNamePath);
    AMessage.Location := vLoc;
  end
  else if vKindName = 'contact' then
  begin
    AMessage.Kind := ckContact;
    vContact.Valid := True;
    vContact.Name := VCardName(PathText(AItem, vIn.ContactNamePath));
    vContact.Phone := VCardPhone(PathText(AItem, vIn.ContactPhonePath));
    AMessage.Contact := vContact;
  end
  else if (vKindName = 'image') or (vKindName = 'audio') or (vKindName = 'video') or
          (vKindName = 'document') or (vKindName = 'sticker') then
  begin
    if vKindName = 'image' then
      AMessage.Kind := ckImage
    else if vKindName = 'audio' then
      AMessage.Kind := ckAudio
    else if vKindName = 'video' then
      AMessage.Kind := ckVideo
    else if vKindName = 'sticker' then
      AMessage.Kind := ckSticker
    else
      AMessage.Kind := ckDocument;
    AMessage.Text := PathText(AItem, vIn.CaptionPath);
    AMessage.Media.MimeType := PathText(AItem, vIn.MimePath);
    AMessage.Media.FileName := PathText(AItem, vIn.FileNamePath);
    AMessage.Media.Url := PathText(AItem, vIn.MediaUrlPath);
    AMessage.Media.Loader := LoadMedia;
    vB64 := PathText(AItem, vIn.MediaBase64Path);
    vPos := Pos(',', string(vB64));
    if (Copy(vB64, 1, 5) = 'data:') and (vPos > 0) then
      vB64 := Copy(vB64, vPos + 1, MaxInt);
    if (vB64 <> '') and (Length(vB64) <= (FMaxDownloadSize div 3) * 4 + 8) then
    begin
      vStream := TRALBase64.DecodeAsStream(vB64);
      if vStream <> nil then
      begin
        AMessage.Media.Data := vStream;
        AMessage.Media.OwnsData := True;
        AMessage.Media.Size := vStream.Size;
      end;
    end;
    if (AMessage.Media.Url = '') and (AMessage.Media.Data = nil) then
      Exit;
  end
  else
  begin
    vText := PathText(AItem, vIn.TextPath);
    if vText = '' then
      Exit;
    AMessage.Kind := ckText;
    AMessage.Text := vText;
  end;
  Result := True;
end;

procedure TRALChatGateway.RefreshCapabilities;
var
  vSet: TRALChatCapabilitySet;
begin
  vSet := [];
  if (FOutgoing.Image.Url <> '') or (FOutgoing.Audio.Url <> '') or (FOutgoing.Video.Url <> '') or
     (FOutgoing.Document.Url <> '') or (FOutgoing.Sticker.Url <> '') then
    Include(vSet, ccMedia);
  if FOutgoing.Buttons.Url <> '' then
    Include(vSet, ccButtons);
  if FOutgoing.List.Url <> '' then
    Include(vSet, ccLists);
  if FOutgoing.Typing.Url <> '' then
    Include(vSet, ccTyping);
  if FOutgoing.Edit.Url <> '' then
    Include(vSet, ccEdit);
  if FOutgoing.Delete.Url <> '' then
    Include(vSet, ccDelete);
  Capabilities.Supported := vSet;
end;

procedure TRALChatGateway.Send(AEndpoint: TRALChatGatewayEndpoint; AValues: TObject;
  var AResult: TRALChatSendResult);
const
  cMethods: array[TRALChatGatewayMethod] of TRALMethod = (amPOST, amPUT, amPATCH, amDELETE, amGET);
var
  vClient: TRALClient;
  vResponse: TRALResponse;
  vValues: TGwValues;
  vUrl, vBody, vCheck: StringRAL;
  vRoot: TRALJSON;
begin
  vValues := TGwValues(AValues);
  vCheck := StringRAL(LowerCase(string(AEndpoint.Url + AEndpoint.Body + FAuthValue + StringRAL(FHeaders.Text))));
  if Pos('{token}', string(vCheck)) > 0 then
    vValues.AddStr('token', GetToken)
  else
    vValues.AddStr('token', '');
  vUrl := Expand(AEndpoint.Url, vValues, cModeUrl);
  if (Copy(vUrl, 1, 7) <> 'http://') and (Copy(vUrl, 1, 8) <> 'https://') then
  begin
    if FBaseUrl = '' then
      raise ERALChatGatewayFinal.CreateFmt(emChatGwNoUrl, [string(ChannelName)]);
    if (vUrl <> '') and (vUrl[1] <> '/') then
      vUrl := '/' + vUrl;
    if FBaseUrl[Length(FBaseUrl)] = '/' then
      vUrl := Copy(FBaseUrl, 1, Length(FBaseUrl) - 1) + vUrl
    else
      vUrl := FBaseUrl + vUrl;
  end;
  vBody := '';
  if AEndpoint.Body <> '' then
  begin
    vBody := Expand(AEndpoint.Body, vValues, cModeJson);
    // a body that is not JSON would only come back as a puzzling error from the gateway
    vRoot := nil;
    try
      try
        vRoot := TRALJSON.ParseJSON(vBody);
      except
        raise ERALChatGatewayFinal.CreateFmt(emChatGwBadTemplate, [string(ChannelName)]);
      end;
    finally
      vRoot.Free;
    end;
  end;
  vClient := GetClient;
  vResponse := nil;
  FCallLock.Enter;
  try
    try
      vClient.Request.Clear;
      AddHeaders(vClient, vValues);
      if vBody <> '' then
        vClient.Request.AddBody(vBody, 'application/json');
      vClient.Execute(vUrl, cMethods[AEndpoint.Method], vResponse);
      ApplyAnswer(vResponse.StatusCode, vResponse.ResponseText, vResponse, AResult);
    finally
      vResponse.Free;
    end;
  finally
    FCallLock.Leave;
  end;
end;

procedure TRALChatGateway.SetClient(AValue: TRALClient);
begin
  if FClient = AValue then
    Exit;
  if FClient <> nil then
    FClient.RemoveFreeNotification(Self);
  FClient := AValue;
  if FClient <> nil then
    FClient.FreeNotification(Self);
end;

procedure TRALChatGateway.SetHeaders(AValue: TStrings);
begin
  FHeaders.Assign(AValue);
end;

procedure TRALChatGateway.SetIncoming(AValue: TRALChatGatewayIncoming);
begin
  FIncoming.Assign(AValue);
end;

procedure TRALChatGateway.SetOutgoing(AValue: TRALChatGatewayOutgoing);
begin
  FOutgoing.Assign(AValue);
end;

procedure TRALChatGateway.SetProfile(AValue: TRALChatGatewayProfile);
begin
  if csLoading in ComponentState then
    FProfile := AValue
  else
    ApplyProfile(AValue);
end;

procedure TRALChatGateway.SetVerification(AValue: TRALChatGatewayVerifier);
begin
  FVerifier.Assign(AValue);
end;

end.
