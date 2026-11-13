{ A conversation in steps, built in the Object Inspector: each step asks something
  (text, buttons), checks the answer, keeps it in the session of the conversation
  and goes to the next step, or to the one the answer picks. The step a conversation
  is at lives in the bot's sessions, so a persistent store resumes it after a
  restart. }
unit RALChatFlow;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALTools, RALCustomObjects, RALRegex,
  RALChatMessage, RALChatChannel, RALChatBot;

const
  /// A NextStep (or a branch target) that ends the flow.
  cRALChatFlowEnd = '@end';

type
  TRALChatFlow = class;
  TRALChatFlowStep = class;

  /// What an answer must be.
  TRALChatFlowValidation = (
    /// Anything, even a picture: the text (or the media id) is kept.
    fvNone,
    /// Some text.
    fvText,
    /// A whole number.
    fvInteger,
    /// A number, with a dot or a comma for the decimals; kept with a dot.
    fvNumber,
    /// An e-mail address.
    fvEmail,
    /// A date, day/month/year or year-month-day; kept as year-month-day.
    fvDate,
    /// One of Options: a button, its number in a channel without buttons, its id or
    /// its caption; the id is kept.
    fvOptions,
    /// Text that matches the regular expression Pattern.
    fvRegex,
    /// A picture, audio, video or file; its media id is kept.
    fvMedia,
    /// A location; kept as "latitude,longitude".
    fvLocation,
    /// A contact; its phone number is kept.
    fvContact
  );

  /// Why a flow ended without reaching its end.
  TRALChatFlowStop = (
    /// The user wrote one of CancelWords, or the application called Cancel.
    fsCancelled,
    /// The answer came after Timeout seconds.
    fsTimeout,
    /// More invalid answers than MaxAttempts.
    fsAttempts
  );

  { What a step's event sees and may change: the conversation, the message that
    arrived, the answer and where the flow goes next. }
  TRALChatFlowContext = class
  private
    FAnswer: StringRAL;
    FChannel: TRALChatChannel;
    FChatId: StringRAL;
    FErrorText: StringRAL;
    FFlow: TRALChatFlow;
    FMessage: TRALChatMessage;
    FNextStep: string;
    FStep: TRALChatFlowStep;
    FValid: Boolean;

    function GetChannelName: StringRAL;
  public
    /// The value kept under AName in the conversation's session ('' when none).
    function GetValue(const AName: StringRAL): StringRAL;
    /// Keeps AValue under AName in the conversation's session; '' removes it.
    procedure SetValue(const AName, AValue: StringRAL);

    /// The answer as it will be kept; the event may change it.
    property Answer: StringRAL read FAnswer write FAnswer;
    /// The channel of the conversation.
    property Channel: TRALChatChannel read FChannel;
    /// The name of the channel.
    property ChannelName: StringRAL read GetChannelName;
    /// The conversation.
    property ChatId: StringRAL read FChatId;
    /// The text sent when Valid is False; empty uses the step's ErrorText.
    property ErrorText: StringRAL read FErrorText write FErrorText;
    /// The flow.
    property Flow: TRALChatFlow read FFlow;
    /// The message that arrived (nil when the flow was started by the application).
    property Message: TRALChatMessage read FMessage;
    { The step that comes next: the name of a step, cRALChatFlowEnd, or empty for
      the one the step's Branches, NextStep or order give. }
    property NextStep: string read FNextStep write FNextStep;
    /// The step being answered or entered.
    property Step: TRALChatFlowStep read FStep;
    /// Whether the answer is accepted; the event may refuse it or accept it.
    property Valid: Boolean read FValid write FValid;
  end;

  /// A step is entered: APrompt is the message about to be sent; the event may change it.
  TRALChatFlowPromptEvent = procedure(Sender: TObject; AContext: TRALChatFlowContext;
    APrompt: TRALChatMessage) of object;

  /// Something happened in the flow; AContext says where.
  TRALChatFlowEvent = procedure(Sender: TObject; AContext: TRALChatFlowContext) of object;

  /// The flow ended before its end.
  TRALChatFlowStopEvent = procedure(Sender: TObject; AContext: TRALChatFlowContext;
    AReason: TRALChatFlowStop) of object;

  /// One step: the question, the check of the answer and where to go next.
  TRALChatFlowStep = class(TCollectionItem)
  private
    FBranches: TStrings;
    FErrorText: StringRAL;
    FName: string;
    FNextStep: string;
    FOnAnswer: TRALChatFlowEvent;
    FOnEnter: TRALChatFlowPromptEvent;
    FOptions: TStrings;
    FPattern: StringRAL;
    FPrompt: StringRAL;
    FValidation: TRALChatFlowValidation;
    FVariable: string;

    procedure SetBranches(AValue: TStrings);
    procedure SetOptions(AValue: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
    { Checks AMessage against Validation: True with the value to keep in AAnswer,
      or False with the text to send in AError. }
    function Check(AMessage: TRALChatMessage; out AAnswer, AError: StringRAL): Boolean;
    /// The name the answer is kept under: Variable, or Name when Variable is empty.
    function VariableName: string;
  published
    { Where an answer goes, one "answer=step" per line (the answer as kept, such as
      an option id); checked before NextStep. }
    property Branches: TStrings read FBranches write SetBranches;
    /// The text sent for an invalid answer; empty uses one that fits Validation.
    property ErrorText: StringRAL read FErrorText write FErrorText;
    /// The name of the step, for NextStep and Branches.
    property Name: string read FName write FName;
    { The step after this one: a step name, cRALChatFlowEnd, or empty for the next in
      the list (the last one ends the flow). }
    property NextStep: string read FNextStep write FNextStep;
    { Runs for an answer after the check: it may refuse it (Valid), change it (Answer)
      or choose the next step (NextStep). }
    property OnAnswer: TRALChatFlowEvent read FOnAnswer write FOnAnswer;
    /// Runs before the question is sent: the event may change it.
    property OnEnter: TRALChatFlowPromptEvent read FOnEnter write FOnEnter;
    /// The buttons of the question, one "id=caption" (or just a caption) per line.
    property Options: TStrings read FOptions write SetOptions;
    /// The regular expression of fvRegex.
    property Pattern: StringRAL read FPattern write FPattern;
    /// The question; a name in braces is replaced by the value kept under that name.
    property Prompt: StringRAL read FPrompt write FPrompt;
    /// What the answer must be.
    property Validation: TRALChatFlowValidation read FValidation write FValidation
      default fvNone;
    /// The name the answer is kept under in the session; empty uses Name.
    property Variable: string read FVariable write FVariable;
  end;

  /// The steps of a flow.
  TRALChatFlowSteps = class(TOwnedCollection)
  private
    function GetItem(AIndex: Integer): TRALChatFlowStep;
  public
    /// Creates the TRALChatFlowSteps.
    constructor Create(AOwner: TPersistent);

    /// Adds a step.
    function Add: TRALChatFlowStep;
    /// The step named AName (case ignored), nil when none.
    function Find(const AName: string): TRALChatFlowStep;

    /// The step at AIndex.
    property Items[AIndex: Integer]: TRALChatFlowStep read GetItem; default;
  end;

  { A conversation in steps on a bot. A command (StartCommand) or the application
    (Start) begins it; while it runs, every message of that conversation goes to it
    and not to the bot's commands and events. The answers stay in the bot's
    sessions under each step's variable, where the application reads them in
    OnFinish or later. }
  TRALChatFlow = class(TRALChatInterceptor)
  private
    FBot: TRALChatBot;
    FCancelText: StringRAL;
    FCancelWords: TStrings;
    FChannels: string;
    FFinishText: StringRAL;
    FMaxAttempts: Integer;
    FOnCancel: TRALChatFlowStopEvent;
    FOnFinish: TRALChatFlowEvent;
    FOnStart: TRALChatFlowEvent;
    FStartCommand: string;
    FSteps: TRALChatFlowSteps;
    FTimeout: Integer;
    FTimeoutText: StringRAL;

    /// Sends AText (with the session's values in it) to the conversation, when not empty.
    procedure Answer(AContext: TRALChatFlowContext; const AText: StringRAL);
    /// Makes a context for the conversation of AChannel/AChatId.
    function Context(AChannel: TRALChatChannel; const AChatId: StringRAL;
      AMessage: TRALChatMessage): TRALChatFlowContext;
    /// Sends the question of AStep and records it as the current step.
    procedure Enter(AContext: TRALChatFlowContext; AStep: TRALChatFlowStep);
    /// Ends the flow at its end.
    procedure Finish(AContext: TRALChatFlowContext);
    /// Forgets the flow's own keys of the conversation.
    procedure Forget(AContext: TRALChatFlowContext);
    /// Whether AText is one of CancelWords (case ignored, a leading slash ignored).
    function IsCancel(const AText: StringRAL): Boolean;
    /// Whether the flow listens to the channel called AChannel.
    function Listens(const AChannel: string): Boolean;
    /// The step after AStep for the answer AAnswer; nil ends the flow.
    function NextOf(AStep: TRALChatFlowStep; const AAnswer: StringRAL;
      const AChosen: string): TRALChatFlowStep;
    /// Handles an answer to the current step.
    procedure Process(AContext: TRALChatFlowContext);
    /// Puts the session's values in place of the names in braces of AText.
    function Render(AContext: TRALChatFlowContext; const AText: StringRAL): StringRAL;
    procedure SetBot(AValue: TRALChatBot);
    procedure SetCancelWords(AValue: TStrings);
    procedure SetSteps(AValue: TRALChatFlowSteps);
    /// Whether AMessage is the StartCommand.
    function StartsFlow(AMessage: TRALChatMessage): Boolean;
    /// Ends the flow before its end.
    procedure StopFlow(AContext: TRALChatFlowContext; AReason: TRALChatFlowStop);
  protected
    function Intercept(AChannel: TRALChatChannel; AMessage: TRALChatMessage): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Ends the flow in a conversation (OnCancel with fsCancelled, CancelText sent).
    procedure Cancel(const AChannelName, AChatId: StringRAL);
    /// The step a conversation is at, nil when the flow is not running in it.
    function CurrentStep(const AChannelName, AChatId: StringRAL): TRALChatFlowStep;
    /// Whether the flow is running in a conversation.
    function IsRunning(const AChannelName, AChatId: StringRAL): Boolean;
    { Begins the flow in a conversation, at its first step or at AStepName; one
      already running there starts over. }
    procedure Start(const AChannelName, AChatId: StringRAL; const AStepName: string = '');
  published
    /// The bot the flow runs on.
    property Bot: TRALChatBot read FBot write SetBot;
    /// The text sent when the user cancels; empty uses a short one.
    property CancelText: StringRAL read FCancelText write FCancelText;
    /// Words that end the flow, one per line, case ignored; a leading slash is ignored.
    property CancelWords: TStrings read FCancelWords write SetCancelWords;
    /// The channels it runs on, names separated by commas; empty runs on all.
    property Channels: string read FChannels write FChannels;
    /// The text sent at the end; a name in braces is replaced by that answer.
    property FinishText: StringRAL read FFinishText write FFinishText;
    /// Invalid answers in a row a step accepts before the flow ends; 0 = no limit.
    property MaxAttempts: Integer read FMaxAttempts write FMaxAttempts default 3;
    /// The flow ended before its end.
    property OnCancel: TRALChatFlowStopEvent read FOnCancel write FOnCancel;
    /// The last step was answered: the answers are in the session (AContext.GetValue).
    property OnFinish: TRALChatFlowEvent read FOnFinish write FOnFinish;
    /// The flow begins in a conversation.
    property OnStart: TRALChatFlowEvent read FOnStart write FOnStart;
    /// The command that begins the flow, without the slash; empty = only Start does.
    property StartCommand: string read FStartCommand write FStartCommand;
    /// The steps.
    property Steps: TRALChatFlowSteps read FSteps write SetSteps;
    { Seconds an answer may take; a later message ends the flow (OnCancel with
      fsTimeout, TimeoutText sent) and is then handled as usual. 0 = no limit, but
      the bot's SessionTimeout still forgets an idle conversation. }
    property Timeout: Integer read FTimeout write FTimeout default 600;
    /// The text sent when the answer came too late; empty uses a short one.
    property TimeoutText: StringRAL read FTimeoutText write FTimeoutText;
  end;

implementation

const
  /// The session name of the running flow.
  cFlowKey = '@flow';
  /// The session name of the current step.
  cStepKey = '@flow.step';
  /// The session name of the time the step was entered (Unix seconds).
  cSinceKey = '@flow.since';
  /// The session name of the invalid answers in a row.
  cTriesKey = '@flow.tries';

/// Whether AText is an e-mail address: one at sign, a dot after it, no space.
function IsEmail(const AText: StringRAL): Boolean;
var
  vText: string;
  vAt, vDot, vIdx: Integer;
begin
  Result := False;
  vText := string(AText);
  vAt := Pos('@', vText);
  if (vAt < 2) or (Pos('@', Copy(vText, vAt + 1, MaxInt)) > 0) then
    Exit;
  for vIdx := 1 to Length(vText) do
    if vText[vIdx] <= ' ' then
      Exit;
  vDot := LastDelimiter('.', vText);
  Result := (vDot > vAt + 1) and (vDot < Length(vText));
end;

{ Reads a date day/month/year (or with - or .) or year-month-day; True with it as
  year-month-day in AValue. }
function ParseDate(const AText: StringRAL; out AValue: StringRAL): Boolean;
var
  vParts: TStringList;
  vText: string;
  vDay, vMonth, vYear: Integer;
  vDate: TDateTime;
begin
  Result := False;
  AValue := '';
  vText := StringReplace(StringReplace(Trim(string(AText)), '.', '/', [rfReplaceAll]),
    '-', '/', [rfReplaceAll]);
  vParts := TStringList.Create;
  try
    vParts.Delimiter := '/';
    vParts.StrictDelimiter := True;
    vParts.DelimitedText := vText;
    if vParts.Count <> 3 then
      Exit;
    if Length(vParts[0]) = 4 then
    begin
      vYear := StrToIntDef(vParts[0], -1);
      vMonth := StrToIntDef(vParts[1], -1);
      vDay := StrToIntDef(vParts[2], -1);
    end
    else
    begin
      vDay := StrToIntDef(vParts[0], -1);
      vMonth := StrToIntDef(vParts[1], -1);
      vYear := StrToIntDef(vParts[2], -1);
      if (vYear >= 0) and (vYear < 100) and (Length(vParts[2]) = 2) then
        vYear := 2000 + vYear;
    end;
    if (vYear < 1) or (vYear > 9999) or not TryEncodeDate(vYear, vMonth, vDay, vDate) then
      Exit;
    AValue := StringRAL(FormatDateTime('yyyy"-"mm"-"dd', vDate));
    Result := True;
  finally
    vParts.Free;
  end;
end;

/// Reads a number with a dot or a comma for decimals; True with it, dotted, in AValue.
function ParseNumber(const AText: StringRAL; out AValue: StringRAL): Boolean;
var
  vText: string;
  vNumber: Double;
  vFormat: TFormatSettings;
begin
  vText := StringReplace(Trim(string(AText)), ',', '.', [rfReplaceAll]);
  vFormat := RALInvariantFormat;
  vFormat.DecimalSeparator := '.';
  vFormat.ThousandSeparator := #0;
  Result := (vText <> '') and (Pos('.', Copy(vText, Pos('.', vText) + 1, MaxInt)) = 0) and
    TryStrToFloat(vText, vNumber, vFormat);
  if Result then
    AValue := StringRAL(vText)
  else
    AValue := '';
end;

{ TRALChatFlowContext }

function TRALChatFlowContext.GetChannelName: StringRAL;
begin
  Result := FChannel.ChannelName;
end;

function TRALChatFlowContext.GetValue(const AName: StringRAL): StringRAL;
begin
  Result := FFlow.Bot.Sessions.GetValue(FChannel.ChannelName, FChatId, AName);
end;

procedure TRALChatFlowContext.SetValue(const AName, AValue: StringRAL);
begin
  FFlow.Bot.Sessions.SetValue(FChannel.ChannelName, FChatId, AName, AValue);
end;

{ TRALChatFlowStep }

procedure TRALChatFlowStep.Assign(ASource: TPersistent);
var
  vSource: TRALChatFlowStep;
begin
  if ASource is TRALChatFlowStep then
  begin
    vSource := TRALChatFlowStep(ASource);
    FBranches.Assign(vSource.FBranches);
    FErrorText := vSource.FErrorText;
    FName := vSource.FName;
    FNextStep := vSource.FNextStep;
    FOnAnswer := vSource.FOnAnswer;
    FOnEnter := vSource.FOnEnter;
    FOptions.Assign(vSource.FOptions);
    FPattern := vSource.FPattern;
    FPrompt := vSource.FPrompt;
    FValidation := vSource.FValidation;
    FVariable := vSource.FVariable;
  end
  else
    inherited Assign(ASource);
end;

function TRALChatFlowStep.Check(AMessage: TRALChatMessage; out AAnswer,
  AError: StringRAL): Boolean;
var
  vText, vId, vCaption: StringRAL;
  vIdx, vChoice: Integer;
  vValue: Int64;
  vRegex: TRALRegex;
begin
  Result := False;
  AAnswer := '';
  AError := FErrorText;
  vText := StringRAL(Trim(string(AMessage.Text)));
  case FValidation of
    fvNone:
    begin
      if (AMessage.Kind in [ckImage, ckAudio, ckVideo, ckDocument, ckSticker]) and
         (AMessage.Media.Id <> '') then
        AAnswer := AMessage.Media.Id
      else if AMessage.Kind = ckCallback then
        AAnswer := AMessage.CallbackData
      else
        AAnswer := vText;
      Result := True;
    end;
    fvText:
    begin
      Result := (AMessage.Kind in [ckText, ckCommand]) and (vText <> '');
      AAnswer := vText;
    end;
    fvInteger:
    begin
      Result := (AMessage.Kind = ckText) and TryStrToInt64(string(vText), vValue);
      if Result then
        AAnswer := StringRAL(IntToStr(vValue));
      if AError = '' then
        AError := cmChatFlowBadNumber;
    end;
    fvNumber:
    begin
      Result := (AMessage.Kind = ckText) and ParseNumber(vText, AAnswer);
      if AError = '' then
        AError := cmChatFlowBadNumber;
    end;
    fvEmail:
    begin
      Result := (AMessage.Kind = ckText) and IsEmail(vText);
      if Result then
        AAnswer := StringRAL(LowerCase(string(vText)));
      if AError = '' then
        AError := cmChatFlowBadEmail;
    end;
    fvDate:
    begin
      Result := (AMessage.Kind = ckText) and ParseDate(vText, AAnswer);
      if AError = '' then
        AError := cmChatFlowBadDate;
    end;
    fvOptions:
    begin
      if AMessage.Kind = ckCallback then
        vText := AMessage.CallbackData;
      for vIdx := 0 to FOptions.Count - 1 do
      begin
        vId := StringRAL(FOptions.Names[vIdx]);
        if vId = '' then
        begin
          vId := StringRAL(FOptions[vIdx]);
          vCaption := vId;
        end
        else
          vCaption := StringRAL(FOptions.ValueFromIndex[vIdx]);
        if (AMessage.Kind in [ckText, ckCallback]) and
           (SameText(string(vText), string(vId)) or SameText(string(vText), string(vCaption))) then
        begin
          AAnswer := vId;
          Result := True;
          Break;
        end;
      end;
      // a number typed where the channel showed real buttons
      if (not Result) and (AMessage.Kind = ckText) and RALChatIsChoice(vText, vChoice) and
         (vChoice <= FOptions.Count) then
      begin
        AAnswer := StringRAL(FOptions.Names[vChoice - 1]);
        if AAnswer = '' then
          AAnswer := StringRAL(FOptions[vChoice - 1]);
        Result := True;
      end;
      if AError = '' then
        AError := cmChatFlowBadOption;
    end;
    fvRegex:
    begin
      if (AMessage.Kind = ckText) and (FPattern <> '') then
      begin
        vRegex := TRALRegex.Create(FPattern);
        try
          Result := vRegex.IsMatch(vText);
        finally
          vRegex.Free;
        end;
      end;
      AAnswer := vText;
    end;
    fvMedia:
    begin
      Result := (AMessage.Kind in [ckImage, ckAudio, ckVideo, ckDocument]) and
        (AMessage.Media.Id <> '');
      if Result then
        AAnswer := AMessage.Media.Id;
      if AError = '' then
        AError := cmChatFlowBadMedia;
    end;
    fvLocation:
    begin
      Result := AMessage.Location.Valid;
      if Result then
        AAnswer := StringRAL(StringReplace(FloatToStr(AMessage.Location.Latitude,
          RALInvariantFormat), ',', '.', []) + ',' + StringReplace(FloatToStr(
          AMessage.Location.Longitude, RALInvariantFormat), ',', '.', []));
      if AError = '' then
        AError := cmChatFlowBadLocation;
    end;
    fvContact:
    begin
      Result := AMessage.Contact.Valid and (AMessage.Contact.Phone <> '');
      if Result then
        AAnswer := AMessage.Contact.Phone;
      if AError = '' then
        AError := cmChatFlowBadContact;
    end;
  end;
  if not Result then
    AAnswer := ''
  else
    AError := '';
  if (not Result) and (AError = '') then
    AError := cmChatFlowBadAnswer;
end;

constructor TRALChatFlowStep.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FBranches := TStringList.Create;
  FOptions := TStringList.Create;
  FValidation := fvNone;
end;

destructor TRALChatFlowStep.Destroy;
begin
  FreeAndNil(FOptions);
  FreeAndNil(FBranches);
  inherited Destroy;
end;

function TRALChatFlowStep.GetDisplayName: string;
begin
  if FName <> '' then
    Result := FName
  else
    Result := inherited GetDisplayName;
end;

procedure TRALChatFlowStep.SetBranches(AValue: TStrings);
begin
  FBranches.Assign(AValue);
end;

procedure TRALChatFlowStep.SetOptions(AValue: TStrings);
begin
  FOptions.Assign(AValue);
end;

function TRALChatFlowStep.VariableName: string;
begin
  if FVariable <> '' then
    Result := FVariable
  else if FName <> '' then
    Result := FName
  else
    Result := 'step' + IntToStr(Index + 1);
end;

{ TRALChatFlowSteps }

function TRALChatFlowSteps.Add: TRALChatFlowStep;
begin
  Result := TRALChatFlowStep(inherited Add);
end;

constructor TRALChatFlowSteps.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TRALChatFlowStep);
end;

function TRALChatFlowSteps.Find(const AName: string): TRALChatFlowStep;
var
  vIdx: Integer;
begin
  Result := nil;
  if AName = '' then
    Exit;
  for vIdx := 0 to Count - 1 do
    if SameText(Items[vIdx].Name, AName) then
    begin
      Result := Items[vIdx];
      Exit;
    end;
end;

function TRALChatFlowSteps.GetItem(AIndex: Integer): TRALChatFlowStep;
begin
  Result := TRALChatFlowStep(inherited Items[AIndex]);
end;

{ TRALChatFlow }

procedure TRALChatFlow.Answer(AContext: TRALChatFlowContext; const AText: StringRAL);
var
  vText: StringRAL;
  vMessage: TRALChatMessage;
begin
  vText := Render(AContext, AText);
  if vText = '' then
    Exit;
  vMessage := TRALChatMessage.Create;
  try
    vMessage.ChatId := AContext.ChatId;
    vMessage.Text := vText;
    FBot.Send(string(AContext.ChannelName), vMessage);
  finally
    vMessage.Free;
  end;
end;

procedure TRALChatFlow.Cancel(const AChannelName, AChatId: StringRAL);
var
  vContext: TRALChatFlowContext;
  vChannel: TRALChatChannel;
begin
  if (FBot = nil) or not IsRunning(AChannelName, AChatId) then
    Exit;
  vChannel := FBot.Channels.Find(string(AChannelName));
  if vChannel = nil then
    raise ERALChatError.CreateFmt(emChatNoChannel, [string(AChannelName)]);
  vContext := Context(vChannel, AChatId, nil);
  try
    StopFlow(vContext, fsCancelled);
  finally
    vContext.Free;
  end;
end;

function TRALChatFlow.Context(AChannel: TRALChatChannel; const AChatId: StringRAL;
  AMessage: TRALChatMessage): TRALChatFlowContext;
begin
  Result := TRALChatFlowContext.Create;
  Result.FFlow := Self;
  Result.FChannel := AChannel;
  Result.FChatId := AChatId;
  Result.FMessage := AMessage;
  Result.FValid := True;
end;

constructor TRALChatFlow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSteps := TRALChatFlowSteps.Create(Self);
  FCancelWords := TStringList.Create;
  FCancelWords.Add('cancel');
  FMaxAttempts := 3;
  FTimeout := 600;
end;

function TRALChatFlow.CurrentStep(const AChannelName, AChatId: StringRAL): TRALChatFlowStep;
begin
  Result := nil;
  if (FBot = nil) or not IsRunning(AChannelName, AChatId) then
    Exit;
  Result := FSteps.Find(string(FBot.Sessions.GetValue(AChannelName, AChatId,
    cStepKey)));
end;

destructor TRALChatFlow.Destroy;
begin
  SetBot(nil);
  FreeAndNil(FCancelWords);
  FreeAndNil(FSteps);
  inherited Destroy;
end;

procedure TRALChatFlow.Enter(AContext: TRALChatFlowContext; AStep: TRALChatFlowStep);
var
  vPrompt: TRALChatMessage;
  vIdx: Integer;
  vId, vCaption: string;
begin
  AContext.FStep := AStep;
  AContext.SetValue(cStepKey, StringRAL(AStep.Name));
  AContext.SetValue(cSinceKey, StringRAL(IntToStr(FBot.UnixNow)));
  AContext.SetValue(cTriesKey, '');
  vPrompt := TRALChatMessage.Create;
  try
    vPrompt.ChatId := AContext.ChatId;
    vPrompt.Text := Render(AContext, AStep.Prompt);
    for vIdx := 0 to AStep.Options.Count - 1 do
    begin
      vId := AStep.Options.Names[vIdx];
      if vId = '' then
      begin
        vId := AStep.Options[vIdx];
        vCaption := vId;
      end
      else
        vCaption := AStep.Options.ValueFromIndex[vIdx];
      vPrompt.AddButton(StringRAL(vId), StringRAL(vCaption));
    end;
    if Assigned(AStep.OnEnter) then
      AStep.OnEnter(AStep, AContext, vPrompt);
    if (vPrompt.Text <> '') or (Length(vPrompt.Buttons) > 0) then
      FBot.Send(string(AContext.ChannelName), vPrompt);
  finally
    vPrompt.Free;
  end;
end;

procedure TRALChatFlow.Finish(AContext: TRALChatFlowContext);
begin
  Forget(AContext);
  if Assigned(FOnFinish) then
    FOnFinish(Self, AContext);
  Answer(AContext, FFinishText);
end;

procedure TRALChatFlow.Forget(AContext: TRALChatFlowContext);
begin
  AContext.SetValue(cFlowKey, '');
  AContext.SetValue(cStepKey, '');
  AContext.SetValue(cSinceKey, '');
  AContext.SetValue(cTriesKey, '');
end;

function TRALChatFlow.Intercept(AChannel: TRALChatChannel; AMessage: TRALChatMessage): Boolean;
var
  vContext: TRALChatFlowContext;
  vSince: Int64RAL;
begin
  Result := False;
  if (FBot = nil) or (AChannel = nil) or (AMessage.Chat.Id = '') or
     (FSteps.Count = 0) or not Listens(string(AChannel.ChannelName)) then
    Exit;
  vContext := Context(AChannel, AMessage.Chat.Id, AMessage);
  try
    if IsRunning(AChannel.ChannelName, AMessage.Chat.Id) then
    begin
      vSince := StrToInt64Def(string(vContext.GetValue(cSinceKey)), 0);
      if (FTimeout > 0) and (vSince > 0) and (FBot.UnixNow - vSince > FTimeout) then
      begin
        // too late: the flow ends and the message is handled as if it had not run
        StopFlow(vContext, fsTimeout);
        if not StartsFlow(AMessage) then
          Exit;
      end
      else
      begin
        Result := True;
        if StartsFlow(AMessage) then
          Start(AChannel.ChannelName, AMessage.Chat.Id)
        else if (AMessage.Kind in [ckText, ckCommand]) and IsCancel(AMessage.Text) then
          StopFlow(vContext, fsCancelled)
        else
          Process(vContext);
        Exit;
      end;
    end;
    if StartsFlow(AMessage) then
    begin
      Result := True;
      Start(AChannel.ChannelName, AMessage.Chat.Id);
    end;
  finally
    vContext.Free;
  end;
end;

function TRALChatFlow.IsCancel(const AText: StringRAL): Boolean;
var
  vText: string;
  vIdx: Integer;
begin
  Result := False;
  vText := Trim(string(AText));
  if (vText <> '') and (vText[1] = '/') then
    Delete(vText, 1, 1);
  if vText = '' then
    Exit;
  for vIdx := 0 to FCancelWords.Count - 1 do
    if SameText(vText, Trim(FCancelWords[vIdx])) then
    begin
      Result := True;
      Exit;
    end;
end;

function TRALChatFlow.IsRunning(const AChannelName, AChatId: StringRAL): Boolean;
begin
  Result := (FBot <> nil) and (FBot.Sessions.GetValue(AChannelName, AChatId, cFlowKey) =
    StringRAL(Name));
end;

function TRALChatFlow.Listens(const AChannel: string): Boolean;
var
  vList: TStringList;
  vIdx: Integer;
begin
  Result := Trim(FChannels) = '';
  if Result then
    Exit;
  vList := TStringList.Create;
  try
    vList.CommaText := FChannels;
    for vIdx := 0 to vList.Count - 1 do
      if SameText(Trim(vList[vIdx]), AChannel) then
      begin
        Result := True;
        Exit;
      end;
  finally
    vList.Free;
  end;
end;

function TRALChatFlow.NextOf(AStep: TRALChatFlowStep; const AAnswer: StringRAL;
  const AChosen: string): TRALChatFlowStep;
var
  vTarget: string;
  vIdx: Integer;
begin
  vTarget := AChosen;
  if vTarget = '' then
    for vIdx := 0 to AStep.Branches.Count - 1 do
      if SameText(Trim(AStep.Branches.Names[vIdx]), string(AAnswer)) then
      begin
        vTarget := Trim(AStep.Branches.ValueFromIndex[vIdx]);
        Break;
      end;
  if vTarget = '' then
    vTarget := AStep.NextStep;
  if SameText(vTarget, cRALChatFlowEnd) then
    Result := nil
  else if vTarget <> '' then
  begin
    Result := FSteps.Find(vTarget);
    if Result = nil then
      raise ERALChatError.CreateFmt(emChatFlowNoStep, [Name, vTarget]);
  end
  else if AStep.Index < FSteps.Count - 1 then
    Result := FSteps[AStep.Index + 1]
  else
    Result := nil;
end;

procedure TRALChatFlow.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FBot) then
    FBot := nil;
end;

procedure TRALChatFlow.Process(AContext: TRALChatFlowContext);
var
  vStep, vNext: TRALChatFlowStep;
  vAnswer, vError: StringRAL;
  vTries: Integer;
begin
  vStep := FSteps.Find(string(AContext.GetValue(cStepKey)));
  if vStep = nil then
  begin
    // the step was removed or renamed since: start over
    Enter(AContext, FSteps[0]);
    Exit;
  end;
  AContext.FStep := vStep;
  AContext.FValid := vStep.Check(AContext.Message, vAnswer, vError);
  AContext.FAnswer := vAnswer;
  AContext.FErrorText := '';
  AContext.FNextStep := '';
  if Assigned(vStep.OnAnswer) then
    vStep.OnAnswer(vStep, AContext);
  if not AContext.Valid then
  begin
    vTries := StrToIntDef(string(AContext.GetValue(cTriesKey)), 0) + 1;
    if (FMaxAttempts > 0) and (vTries >= FMaxAttempts) then
    begin
      StopFlow(AContext, fsAttempts);
      Exit;
    end;
    AContext.SetValue(cTriesKey, StringRAL(IntToStr(vTries)));
    if AContext.ErrorText <> '' then
      Answer(AContext, AContext.ErrorText)
    else if vError <> '' then
      Answer(AContext, vError)
    else
      Answer(AContext, cmChatFlowBadAnswer);
    Exit;
  end;
  AContext.SetValue(StringRAL(vStep.VariableName), AContext.Answer);
  vNext := NextOf(vStep, AContext.Answer, AContext.NextStep);
  if vNext = nil then
    Finish(AContext)
  else
    Enter(AContext, vNext);
end;

function TRALChatFlow.Render(AContext: TRALChatFlowContext; const AText: StringRAL): StringRAL;
var
  vText, vOut, vName: string;
  vIdx, vStop: Integer;
begin
  vText := string(AText);
  if Pos('{', vText) = 0 then
  begin
    Result := AText;
    Exit;
  end;
  vOut := '';
  vIdx := 1;
  while vIdx <= Length(vText) do
  begin
    if vText[vIdx] = '{' then
    begin
      vStop := vIdx + 1;
      while (vStop <= Length(vText)) and (vText[vStop] <> '}') and (vText[vStop] <> '{') do
        Inc(vStop);
      if (vStop <= Length(vText)) and (vText[vStop] = '}') and (vStop > vIdx + 1) and
         (vText[vIdx + 1] <> '@') then
      begin
        vName := Copy(vText, vIdx + 1, vStop - vIdx - 1);
        vOut := vOut + string(AContext.GetValue(StringRAL(vName)));
        vIdx := vStop + 1;
        Continue;
      end;
    end;
    vOut := vOut + vText[vIdx];
    Inc(vIdx);
  end;
  Result := StringRAL(vOut);
end;

procedure TRALChatFlow.SetBot(AValue: TRALChatBot);
begin
  if FBot = AValue then
    Exit;
  if FBot <> nil then
  begin
    FBot.RemoveInterceptor(Self);
    FBot.RemoveFreeNotification(Self);
  end;
  FBot := AValue;
  if FBot <> nil then
  begin
    FBot.FreeNotification(Self);
    FBot.AddInterceptor(Self);
  end;
end;

procedure TRALChatFlow.SetCancelWords(AValue: TStrings);
begin
  FCancelWords.Assign(AValue);
end;

procedure TRALChatFlow.SetSteps(AValue: TRALChatFlowSteps);
begin
  FSteps.Assign(AValue);
end;

procedure TRALChatFlow.Start(const AChannelName, AChatId: StringRAL; const AStepName: string);
var
  vContext: TRALChatFlowContext;
  vChannel: TRALChatChannel;
  vStep: TRALChatFlowStep;
begin
  if FBot = nil then
    raise ERALChatError.CreateFmt(emChatFlowNoBot, [Name]);
  if FSteps.Count = 0 then
    raise ERALChatError.CreateFmt(emChatFlowNoSteps, [Name]);
  vChannel := FBot.Channels.Find(string(AChannelName));
  if vChannel = nil then
    raise ERALChatError.CreateFmt(emChatNoChannel, [string(AChannelName)]);
  if AStepName <> '' then
  begin
    vStep := FSteps.Find(AStepName);
    if vStep = nil then
      raise ERALChatError.CreateFmt(emChatFlowNoStep, [Name, AStepName]);
  end
  else
    vStep := FSteps[0];
  vContext := Context(vChannel, AChatId, nil);
  try
    vContext.SetValue(cFlowKey, StringRAL(Name));
    if Assigned(FOnStart) then
      FOnStart(Self, vContext);
    Enter(vContext, vStep);
  finally
    vContext.Free;
  end;
end;

function TRALChatFlow.StartsFlow(AMessage: TRALChatMessage): Boolean;
begin
  Result := (FStartCommand <> '') and (AMessage.Kind = ckCommand) and
    SameText(string(AMessage.Command), FStartCommand);
end;

procedure TRALChatFlow.StopFlow(AContext: TRALChatFlowContext; AReason: TRALChatFlowStop);
begin
  if AContext.FStep = nil then
    AContext.FStep := FSteps.Find(string(AContext.GetValue(cStepKey)));
  Forget(AContext);
  if Assigned(FOnCancel) then
    FOnCancel(Self, AContext, AReason);
  case AReason of
    fsCancelled:
      if FCancelText <> '' then
        Answer(AContext, FCancelText)
      else
        Answer(AContext, cmChatFlowCancelled);
    fsTimeout:
      if FTimeoutText <> '' then
        Answer(AContext, FTimeoutText)
      else
        Answer(AContext, cmChatFlowTimeout);
    fsAttempts:
      Answer(AContext, cmChatFlowTooManyTries);
  end;
end;

end.
