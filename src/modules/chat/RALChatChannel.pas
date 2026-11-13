{ The base of every chat channel: what a platform package (Telegram, WhatsApp...)
  implements - verify the webhook, read its body, deliver a message - plus what
  they all share: the token (property, environment or file, never in a log), the
  capabilities, the outbound limits and the retry rules. }
unit RALChatChannel;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALCustomObjects, RALChatMessage, RALChatVerify;

type
  /// What an outbound operation does.
  TRALChatOutKind = (
    /// Sends a message.
    okMessage,
    /// Shows "typing" in a conversation.
    okTyping,
    /// Changes a sent message.
    okEdit,
    /// Removes a sent message.
    okDelete
  );

  /// How far a limit the platform announced reaches.
  TRALChatRetryScope = (
    /// The whole channel waits.
    rsChannel,
    /// Only the conversation waits.
    rsChat
  );

  /// What a delivery came to.
  TRALChatSendResult = record
    /// Whether the platform accepted it.
    Ok: Boolean;
    /// The id the platform gave the sent message.
    MessageId: StringRAL;
    { When it failed: whether a new attempt may work (a 429, a 5xx, a timeout);
      False is final (a refused token, a blocked user). }
    Transient: Boolean;
    { What the platform asked to wait before trying again (Retry-After, retry_after,
      the WhatsApp 131056 rule), in milliseconds; 0 = nothing asked. }
    RetryAfterMs: Int64RAL;
    /// Whom RetryAfterMs holds for.
    RetryScope: TRALChatRetryScope;
    /// Why it failed, without any secret.
    ErrorText: StringRAL;
    /// Something about the delivery the application may want (a price category).
    Info: StringRAL;
  end;

  /// How a channel gets what the users write.
  TRALChatReceiveMode = (
    /// The platform calls the webhook module.
    crmWebhook,
    /// The channel asks (getUpdates, the Discord gateway, Slack Socket Mode) itself.
    crmPolling,
    { Webhook when the platform has one registered, polling when none is; decided when
      the channel starts. }
    crmAuto
  );

  /// Where the token comes from.
  TRALChatTokenSource = (
    /// The Token property (it lands in the form file).
    tsProperty,
    /// The environment variable named by TokenName.
    tsEnvironment,
    /// The first line of the file named by TokenName.
    tsFile
  );

  { Outbound limits of a channel, in messages per second, at three levels: all the
    channel, each conversation and each pair (the WhatsApp number-user rule);
    a burst is how many may go at once before the rate holds them. 0 = no limit. }
  TRALChatRates = class(TPersistent)
  private
    FPerChat: Double;
    FPerChatBurst: Integer;
    FPerPair: Double;
    FPerPairBurst: Integer;
    FTotal: Double;
    FTotalBurst: Integer;
  public
    constructor Create;

    /// Copies other limits.
    procedure Assign(Source: TPersistent); override;
  published
    /// Messages per second to one conversation.
    property PerChat: Double read FPerChat write FPerChat;
    /// How many go at once to one conversation.
    property PerChatBurst: Integer read FPerChatBurst write FPerChatBurst default 1;
    /// Messages per second to one pair (channel account and user).
    property PerPair: Double read FPerPair write FPerPair;
    /// How many go at once to one pair.
    property PerPairBurst: Integer read FPerPairBurst write FPerPairBurst default 1;
    /// Messages per second through the whole channel.
    property Total: Double read FTotal write FTotal;
    /// How many go at once through the whole channel.
    property TotalBurst: Integer read FTotalBurst write FTotalBurst default 1;
  end;

  { The base class of a channel. A platform package descends from it, fills
    Capabilities, Rates and Verifier in its constructor and implements
    ParseWebhook and Deliver. }
  TRALChatChannel = class(TRALComponent)
  private
    FCapabilities: TRALChatCapabilities;
    FChannelName: StringRAL;
    FKeepRaw: Boolean;
    FMaxRetries: Integer;
    FRates: TRALChatRates;
    FReplayWindow: Integer;
    FRetryBase: Integer;
    FRetryMax: Integer;
    FToken: StringRAL;
    FTokenName: StringRAL;
    FTokenSource: TRALChatTokenSource;
    FVerifier: TRALChatVerifier;

    procedure SetCapabilities(AValue: TRALChatCapabilities);
    procedure SetRates(AValue: TRALChatRates);
    procedure SetVerifier(AValue: TRALChatVerifier);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Gives the platform the answer its webhook wants in the response itself (Discord's
      PONG and deferred types, Slack's challenge): True with AStatus, AContentType and
      AText set; False answers 200 "ok". Called after ParseWebhook, on the webhook's
      thread, so it must be quick. }
    function Acknowledge(const ABody: StringRAL; AMessages: TRALChatMessageList;
      out AStatus: Integer; out AContentType, AText: StringRAL): Boolean; virtual;
    { Delivers AMessage (for typing, edit and delete AMessage only names the
      conversation, and ATargetId is the message acted on) and says how it went; it
      should not raise for a platform answer (an exception is taken as a transient
      failure). Runs on the outbox's thread. }
    procedure Deliver(AKind: TRALChatOutKind; AMessage: TRALChatMessage;
      const ATargetId: StringRAL; out AResult: TRALChatSendResult); virtual; abstract;
    { The dialect of the channel's text; the bot converts the text of a message
      with it unless FormatText is overridden. }
    function Dialect: TRALChatDialect; virtual;
    { Converts the text of a message to what the channel sends: by default
      RALChatConvertText with Dialect. A platform that needs more overrides it. }
    function FormatText(const AText: StringRAL; AFormat: TRALChatTextFormat): StringRAL; virtual;
    { The token, from where TokenSource says; raises when it is empty. The message
      of the exception never carries it. }
    function GetToken: StringRAL;
    { Answers the handshake some platforms do with a GET (Meta's verify token);
      False when the channel has none (the route then answers 405). }
    function Handshake(AQuery: TRALChatHeaderReader; out AStatus: Integer;
      out ABody: StringRAL): Boolean; virtual;
    { The key the pair limiter counts a message under: the conversation by default;
      a platform with several accounts puts the account in front. }
    function PairOf(AMessage: TRALChatMessage): StringRAL; virtual;
    { Reads the body of a verified webhook and adds the messages (and button
      answers) it carries to AMessages, each with ChannelName and EventId set.
      A platform notification that is not a message adds nothing. }
    procedure ParseWebhook(const ABody: StringRAL; AMessages: TRALChatMessageList); virtual; abstract;

    /// What the channel can do; a platform package fills it in its constructor.
    property Capabilities: TRALChatCapabilities read FCapabilities write SetCapabilities;
    { Proves a webhook comes from the platform; none set refuses every request.
      Not published: a secret has no place in the form file. }
    property Verifier: TRALChatVerifier read FVerifier write SetVerifier;
  published
    { The name of the channel: the route of its webhook (under the module's Domain)
      and the name the application sends through. }
    property ChannelName: StringRAL read FChannelName write FChannelName;
    /// Keeps the platform's original JSON in Message.Raw.
    property KeepRaw: Boolean read FKeepRaw write FKeepRaw default False;
    { New attempts after a transient failure, with waits that grow from RetryBase
      to RetryMax. }
    property MaxRetries: Integer read FMaxRetries write FMaxRetries default 5;
    /// The limits of the outbound queue.
    property Rates: TRALChatRates read FRates write SetRates;
    { Refuses a webhook whose timestamp differs from now by more than this many
      seconds, where the platform sends one; 0 = no check. }
    property ReplayWindow: Integer read FReplayWindow write FReplayWindow default 0;
    /// The first wait before a new attempt, in milliseconds.
    property RetryBase: Integer read FRetryBase write FRetryBase default 1000;
    /// The longest wait before a new attempt, in milliseconds.
    property RetryMax: Integer read FRetryMax write FRetryMax default 60000;
    /// The token when TokenSource is tsProperty.
    property Token: StringRAL read FToken write FToken;
    /// The variable (tsEnvironment) or file (tsFile) that holds the token.
    property TokenName: StringRAL read FTokenName write FTokenName;
    /// Where the token comes from.
    property TokenSource: TRALChatTokenSource read FTokenSource write FTokenSource
      default tsProperty;
  end;

implementation

{ TRALChatRates }

constructor TRALChatRates.Create;
begin
  inherited Create;
  FPerChatBurst := 1;
  FPerPairBurst := 1;
  FTotalBurst := 1;
end;

procedure TRALChatRates.Assign(Source: TPersistent);
begin
  if Source is TRALChatRates then
  begin
    FPerChat := TRALChatRates(Source).FPerChat;
    FPerChatBurst := TRALChatRates(Source).FPerChatBurst;
    FPerPair := TRALChatRates(Source).FPerPair;
    FPerPairBurst := TRALChatRates(Source).FPerPairBurst;
    FTotal := TRALChatRates(Source).FTotal;
    FTotalBurst := TRALChatRates(Source).FTotalBurst;
  end
  else
    inherited Assign(Source);
end;

{ TRALChatChannel }

constructor TRALChatChannel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCapabilities := TRALChatCapabilities.Create;
  FRates := TRALChatRates.Create;
  FMaxRetries := 5;
  FRetryBase := 1000;
  FRetryMax := 60000;
end;

destructor TRALChatChannel.Destroy;
begin
  FVerifier.Free;
  FRates.Free;
  FCapabilities.Free;
  inherited Destroy;
end;

function TRALChatChannel.Acknowledge(const ABody: StringRAL; AMessages: TRALChatMessageList;
  out AStatus: Integer; out AContentType, AText: StringRAL): Boolean;
begin
  AStatus := 0;
  AContentType := '';
  AText := '';
  Result := False;
end;

function TRALChatChannel.Dialect: TRALChatDialect;
begin
  Result := cdPlain;
end;

function TRALChatChannel.FormatText(const AText: StringRAL;
  AFormat: TRALChatTextFormat): StringRAL;
begin
  Result := RALChatConvertText(AText, Dialect, AFormat = tfMarkup);
end;

function TRALChatChannel.GetToken: StringRAL;
var
  vList: TStringList;
begin
  Result := '';
  case FTokenSource of
    tsProperty:
      Result := FToken;
    tsEnvironment:
      Result := StringRAL(GetEnvironmentVariable(string(FTokenName)));
    tsFile:
    begin
      vList := TStringList.Create;
      try
        try
          vList.LoadFromFile(string(FTokenName));
        except
          raise ERALChatError.CreateFmt(emChatTokenFile, [string(FChannelName)]);
        end;
        if vList.Count > 0 then
          Result := StringRAL(Trim(vList[0]));
      finally
        vList.Free;
      end;
    end;
  end;
  if Result = '' then
    if FTokenSource = tsEnvironment then
      raise ERALChatError.CreateFmt(emChatTokenEnvironment, [string(FChannelName)])
    else
      raise ERALChatError.CreateFmt(emChatNoToken, [string(FChannelName)]);
end;

function TRALChatChannel.Handshake(AQuery: TRALChatHeaderReader; out AStatus: Integer;
  out ABody: StringRAL): Boolean;
begin
  AStatus := 0;
  ABody := '';
  Result := False;
end;

function TRALChatChannel.PairOf(AMessage: TRALChatMessage): StringRAL;
begin
  Result := AMessage.Chat.Id;
end;

procedure TRALChatChannel.SetCapabilities(AValue: TRALChatCapabilities);
begin
  FCapabilities.Assign(AValue);
end;

procedure TRALChatChannel.SetRates(AValue: TRALChatRates);
begin
  FRates.Assign(AValue);
end;

procedure TRALChatChannel.SetVerifier(AValue: TRALChatVerifier);
begin
  if FVerifier = AValue then
    Exit;
  FVerifier.Free;
  FVerifier := AValue;
end;

end.
