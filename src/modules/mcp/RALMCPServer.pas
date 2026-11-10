{ The Model Context Protocol server (cRALMCPProtocolVersion): tools, resources,
  prompts, completion, subscriptions, input requests and tasks, apart from the
  transport that carries them (RALMCPModule for HTTP, RALMCPStdio). }
unit RALMCPServer;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs, Contnrs, DateUtils,
  RALTypes, RALTools, RALBase64, RALJson, RALSHA2_32, RALCustomObjects, RALRequest,
  RALKeyValueStore, RALJSONRPC;

const
  /// The protocol revision the server speaks.
  cRALMCPProtocolVersion = '2026-07-28';
  /// The id of the tasks extension.
  cRALMCPTasksExtension = 'io.modelcontextprotocol/tasks';
  /// A header does not match the body.
  cRALMCPHeaderMismatch = -32020;
  /// The request needs a capability the client did not declare.
  cRALMCPMissingCapability = -32021;
  /// The protocol version is not one the server speaks.
  cRALMCPUnsupportedVersion = -32022;
  /// The _meta key of the protocol version.
  cRALMCPMetaVersion = 'io.modelcontextprotocol/protocolVersion';
  /// The _meta key of the client capabilities.
  cRALMCPMetaCapabilities = 'io.modelcontextprotocol/clientCapabilities';
  /// The _meta key of the client information.
  cRALMCPMetaClientInfo = 'io.modelcontextprotocol/clientInfo';
  /// The _meta key of the log level asked for.
  cRALMCPMetaLogLevel = 'io.modelcontextprotocol/logLevel';
  /// The _meta key of the server information in results.
  cRALMCPMetaServerInfo = 'io.modelcontextprotocol/serverInfo';
  /// The _meta key that ties a message to its subscription.
  cRALMCPMetaSubscription = 'io.modelcontextprotocol/subscriptionId';

type
  /// The JSON kind a declared parameter of a tool takes.
  TRALMCPParamType = (mptString, mptNumber, mptInteger, mptBoolean, mptObject, mptArray);

  /// Who may serve a cached list or read from a shared cache.
  TRALMCPCacheScope = (mcsPrivate, mcsPublic);

  /// The hints a tool declares about itself; clients take them as untrusted.
  TRALMCPToolHint = (thReadOnly, thDestructive, thIdempotent, thOpenWorld);
  /// A set of tool hints.
  TRALMCPToolHints = set of TRALMCPToolHint;

  /// The state of a task.
  TRALMCPTaskStatus = (mtsWorking, mtsInputRequired, mtsCompleted, mtsFailed, mtsCancelled);

  TRALMCPServer = class;

  { An MCP protocol error: the JSON-RPC code, message and data, and the HTTP
    status a transport answers it with. }
  ERALMCPError = class(ERALJSONRPCError)
  private
    FHTTPStatus: IntegerRAL;
  public
    /// The error ACode with AMessage, the JSON text AData and the HTTP status AStatus.
    constructor CreateMCP(ACode: IntegerRAL; const AMessage: string;
      const AData: StringRAL = ''; AStatus: IntegerRAL = 200);

    /// The HTTP status of the answer.
    property HTTPStatus: IntegerRAL read FHTTPStatus;
  end;

  { What a transport gives a running call to talk back before its answer:
    notifications on the call's own stream, and whether the caller is gone. }
  TRALMCPChannel = class
  private
    /// Whether Cancel was called.
    FCancelled: Boolean;
    FRequest: TObject;
  public
    /// Marks the call as abandoned by the caller.
    procedure Cancel;
    /// Whether the caller went away or cancelled.
    function Cancelled: Boolean; virtual;
    { Sends the JSON-RPC notification AMessage before the answer; False when
      it could not go out. }
    function Notify(const AMessage: StringRAL): Boolean; virtual; abstract;
    /// Whether Notify reaches the caller before the answer (a live stream).
    function Streaming: Boolean; virtual;

    /// What carried the request: the TRALRequest on HTTP; nil on stdio.
    property Request: TObject read FRequest write FRequest;
  end;

  { One call being answered: its parameters, what the client declared, and
    the ways to report progress, log, and ask the client for input (MRTR). }
  TRALMCPCall = class
  private
    FCapabilities: TRALJSON;
    FChannel: TRALMCPChannel;
    FClientName: StringRAL;
    /// The input requests to send back, owned; nil when none.
    FInputRequests: TRALJSONObject;
    /// The inputResponses of the request; not owned.
    FInputResponses: TRALJSON;
    /// The last progress sent, which the next must exceed.
    FLastProgress: Double;
    /// The index of the log level asked for; -1 = no logs.
    FLogLevel: IntegerRAL;
    FMethod: StringRAL;
    /// The state to send back with the input requests.
    FNewState: StringRAL;
    /// Whether NewState was set.
    FNewStateSet: Boolean;
    FParams: TRALJSONObject;
    /// The progress token of the request; nil = no progress.
    FProgressToken: TRALJSON;
    FServer: TRALMCPServer;
    FState: StringRAL;
    /// The task the call runs in; nil outside tasks.
    FTask: TObject;

    function GetArguments: TRALJSONObject;
  public
    /// A call of AMethod with AParams (not owned) for AServer over AChannel.
    constructor Create(AServer: TRALMCPServer; AChannel: TRALMCPChannel;
      const AMethod: StringRAL; AParams: TRALJSONObject);
    destructor Destroy; override;

    /// The argument AName of a tool or prompt; nil when absent.
    function Argument(const AName: StringRAL): TRALJSON;
    /// Whether the caller went away, cancelled the call or its task.
    function Cancelled: Boolean;
    { Asks the client for form input through elicitation: AKey names the
      answer, ASchema (owned) is the flat object schema of the form. }
    procedure Elicit(const AKey, AMessage: StringRAL; ASchema: TRALJSONObject);
    { Whether the client declared the capability APath, dotted: elicitation,
      elicitation.url, sampling, roots or extensions.<id>. }
    function HasCapability(const APath: StringRAL): Boolean;
    /// Whether the call asks the client for input instead of answering.
    function InputRequired: Boolean;
    /// The answer of the client to the input request AKey; nil when none came.
    function InputResponse(const AKey: StringRAL): TRALJSON;
    { Sends notifications/message at ALevel (debug..emergency) when the client
      asked for logs at that level or below; AData is JSON text. }
    procedure Log(const ALevel: StringRAL; const AData: StringRAL;
      const ALogger: StringRAL = '');
    { Sends notifications/progress when the client gave a progress token and the
      transport streams; AProgress must grow, ATotal below 0 is left out. }
    procedure Progress(AProgress: Double; ATotal: Double = -1; const AMessage: StringRAL = '');
    { Adds an input request AKey for the client: ARequest (owned) is an object
      with method (elicitation/create, sampling/createMessage, roots/list) and
      params. Raises Missing capability when the client did not declare it. }
    procedure RequestInput(const AKey: StringRAL; ARequest: TRALJSONObject);
    { Keeps AState for the next step of an input request: the server signs it
      and the client sends it back unchanged. }
    procedure SetState(const AState: StringRAL);

    /// The arguments of a tool or prompt; nil when none came.
    property Arguments: TRALJSONObject read GetArguments;
    /// The capabilities the client declared, as JSON (an object).
    property Capabilities: TRALJSON read FCapabilities;
    /// The transport of the call.
    property Channel: TRALMCPChannel read FChannel;
    /// The name the client gave in its clientInfo; empty when none.
    property ClientName: StringRAL read FClientName;
    /// The method being answered.
    property Method: StringRAL read FMethod;
    /// The params of the request.
    property Params: TRALJSONObject read FParams;
    /// The server answering.
    property Server: TRALMCPServer read FServer;
    /// The state the client sent back, already checked; empty on a first call.
    property State: StringRAL read FState;
  end;

  /// The answer of a tool: content blocks, structured content and the error flag.
  TRALMCPToolResult = class
  private
    FContent: TRALJSONArray;
    FIsError: Boolean;
    FStructured: TRALJSON;
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds an audio block of base64 AData of AMimeType.
    procedure AddAudio(const AData, AMimeType: StringRAL);
    /// Adds the content block AContent (owned) as it is.
    procedure AddContent(AContent: TRALJSONObject);
    /// Adds an image block of base64 AData of AMimeType.
    procedure AddImage(const AData, AMimeType: StringRAL);
    /// Adds an embedded binary resource: AURI, AMimeType and base64 ABlob.
    procedure AddResourceBlob(const AURI, AMimeType, ABlob: StringRAL);
    /// Adds a link to the resource AURI named AName.
    procedure AddResourceLink(const AURI, AName: StringRAL; const AMimeType: StringRAL = '';
      const ADescription: StringRAL = '');
    /// Adds an embedded text resource: AURI, AMimeType and AText.
    procedure AddResourceText(const AURI, AMimeType, AText: StringRAL);
    /// Adds a text block.
    procedure AddText(const AText: StringRAL);
    /// Marks the call failed in execution with the message AText, for the model to read.
    procedure Fail(const AText: StringRAL);
    /// Sets the structured content (owned) and mirrors it in a text block.
    procedure SetStructured(AValue: TRALJSON);

    /// The content blocks.
    property Content: TRALJSONArray read FContent;
    /// Whether the tool failed in execution.
    property IsError: Boolean read FIsError write FIsError;
    /// The structured content; nil when none.
    property Structured: TRALJSON read FStructured;
  end;

  /// What a resource read returns: one or more text or binary contents.
  TRALMCPResourceContents = class
  private
    FContents: TRALJSONArray;
    FURI: StringRAL;
  public
    /// Contents of the resource AURI.
    constructor Create(const AURI: StringRAL);
    destructor Destroy; override;

    /// Adds binary content (base64 ABlob) of AMimeType; AURI empty = the one read.
    procedure AddBlob(const ABlob, AMimeType: StringRAL; const AURI: StringRAL = '');
    /// Adds text content of AMimeType; AURI empty = the one read.
    procedure AddText(const AText: StringRAL; const AMimeType: StringRAL = 'text/plain';
      const AURI: StringRAL = '');

    /// The contents added.
    property Contents: TRALJSONArray read FContents;
    /// The URI being read.
    property URI: StringRAL read FURI;
  end;

  /// What a prompt returns: an optional description and its messages.
  TRALMCPPromptResult = class
  private
    FDescription: StringRAL;
    FMessages: TRALJSONArray;
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds a message of ARole (user or assistant) with the content AContent (owned).
    procedure AddMessage(const ARole: StringRAL; AContent: TRALJSONObject);
    /// Adds a text message of ARole (user or assistant).
    procedure AddText(const ARole, AText: StringRAL);

    /// The description of the prompt returned.
    property Description: StringRAL read FDescription write FDescription;
    /// The messages.
    property Messages: TRALJSONArray read FMessages;
  end;

  /// Runs a tool: fill AResult, or ask for input with ACall.Elicit/RequestInput.
  TRALMCPToolEvent = procedure(Sender: TObject; ACall: TRALMCPCall;
    AResult: TRALMCPToolResult) of object;
  /// Reads a resource: add its contents to AContents.
  TRALMCPReadEvent = procedure(Sender: TObject; ACall: TRALMCPCall;
    AContents: TRALMCPResourceContents) of object;
  /// Reads a resource of a template; AVariables holds the values the URI gave.
  TRALMCPTemplateReadEvent = procedure(Sender: TObject; ACall: TRALMCPCall;
    AVariables: TStrings; AContents: TRALMCPResourceContents) of object;
  /// Builds a prompt from ACall.Arguments into AResult.
  TRALMCPPromptEvent = procedure(Sender: TObject; ACall: TRALMCPCall;
    AResult: TRALMCPPromptResult) of object;
  { Completes the argument AArgument of the prompt or resource ARef (ARefType
    ref/prompt or ref/resource) that starts with AValue: add up to 100 values. }
  TRALMCPCompleteEvent = procedure(Sender: TObject; ACall: TRALMCPCall;
    const ARefType, ARef, AArgument, AValue: StringRAL; AValues: TStrings;
    var AHasMore: Boolean) of object;

  /// A declared parameter of a tool, from which its input schema is built.
  TRALMCPParam = class(TCollectionItem)
  private
    FDescription: StringRAL;
    FHeader: StringRAL;
    FName: StringRAL;
    FParamType: TRALMCPParamType;
    FRequired: Boolean;
    FValues: TStrings;

    procedure SetValues(AValue: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
  published
    /// What the parameter is, for the model.
    property Description: StringRAL read FDescription write FDescription;
    { Copied to the HTTP header Mcp-Param-<Header> by clients (x-mcp-header);
      only for strings, integers and booleans that are not secret. }
    property Header: StringRAL read FHeader write FHeader;
    /// The parameter name.
    property Name: StringRAL read FName write FName;
    /// The JSON kind it takes.
    property ParamType: TRALMCPParamType read FParamType write FParamType default mptString;
    /// Whether a call without it fails in execution.
    property Required: Boolean read FRequired write FRequired default True;
    /// The values allowed (an enum), one per line; empty = any.
    property Values: TStrings read FValues write SetValues;
  end;

  /// The declared parameters of a tool.
  TRALMCPParams = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALMCPParam;
  public
    /// Adds the parameter AName of AType and returns it.
    function AddParam(const AName: StringRAL; AType: TRALMCPParamType = mptString;
      const ADescription: StringRAL = ''; ARequired: Boolean = True): TRALMCPParam;

    /// The parameters by position.
    property Items[AIndex: IntegerRAL]: TRALMCPParam read GetItem; default;
  end;

  /// A tool the model may call.
  TRALMCPTool = class(TCollectionItem)
  private
    FDescription: StringRAL;
    FHints: TRALMCPToolHints;
    FInputSchema: StringRAL;
    FName: StringRAL;
    FOnCall: TRALMCPToolEvent;
    FOutputSchema: StringRAL;
    FParams: TRALMCPParams;
    FRoles: StringRAL;
    FRunAsTask: Boolean;
    FTitle: StringRAL;

    procedure SetParams(AValue: TRALMCPParams);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
    /// The tool as tools/list shows it; the caller frees it.
    function Definition: TRALJSONObject;
    /// The input schema: InputSchema, or the one built from Params.
    function Schema: TRALJSONObject;
  published
    /// What the tool does, for the model.
    property Description: StringRAL read FDescription write FDescription;
    /// The hints declared in the annotations; empty = none.
    property Hints: TRALMCPToolHints read FHints write FHints default [];
    /// A JSON Schema written by hand, replacing the one built from Params.
    property InputSchema: StringRAL read FInputSchema write FInputSchema;
    { The tool name: 1 to 128 of letters, digits, _ - and .; unique, case
      sensitive. }
    property Name: StringRAL read FName write FName;
    /// Runs the tool.
    property OnCall: TRALMCPToolEvent read FOnCall write FOnCall;
    { The JSON Schema of the structured content; with it the tool must set
      AResult.SetStructured. }
    property OutputSchema: StringRAL read FOutputSchema write FOutputSchema;
    /// The parameters, from which the input schema is built and checked.
    property Params: TRALMCPParams read FParams write SetParams;
    { Roles the authenticated user needs, separated by commas (any of them);
      empty = everyone. Others neither see nor call the tool. }
    property Roles: StringRAL read FRoles write FRoles;
    { Runs the call as a task (extension io.modelcontextprotocol/tasks) when
      the client declares it; others get the answer when the call ends. }
    property RunAsTask: Boolean read FRunAsTask write FRunAsTask default False;
    /// A name for people.
    property Title: StringRAL read FTitle write FTitle;
  end;

  /// The tools of a server.
  TRALMCPTools = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALMCPTool;
  public
    /// Adds the tool AName with AEvent and returns it.
    function AddTool(const AName, ADescription: StringRAL; AEvent: TRALMCPToolEvent): TRALMCPTool;
    /// The tool AName; nil when none.
    function Find(const AName: StringRAL): TRALMCPTool;

    /// The tools by position.
    property Items[AIndex: IntegerRAL]: TRALMCPTool read GetItem; default;
  end;

  /// A resource the client may read by its URI.
  TRALMCPResource = class(TCollectionItem)
  private
    FDescription: StringRAL;
    FMimeType: StringRAL;
    FName: StringRAL;
    FOnRead: TRALMCPReadEvent;
    FTitle: StringRAL;
    FURI: StringRAL;
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(ASource: TPersistent); override;
    /// The resource as resources/list shows it; the caller frees it.
    function Definition: TRALJSONObject;
  published
    /// What the resource holds.
    property Description: StringRAL read FDescription write FDescription;
    /// The media type of its contents.
    property MimeType: StringRAL read FMimeType write FMimeType;
    /// The resource name.
    property Name: StringRAL read FName write FName;
    /// Reads the resource.
    property OnRead: TRALMCPReadEvent read FOnRead write FOnRead;
    /// A name for people.
    property Title: StringRAL read FTitle write FTitle;
    /// The URI that identifies it.
    property URI: StringRAL read FURI write FURI;
  end;

  /// The resources of a server.
  TRALMCPResources = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALMCPResource;
  public
    /// Adds the resource AURI named AName with AEvent and returns it.
    function AddResource(const AURI, AName: StringRAL; AEvent: TRALMCPReadEvent;
      const AMimeType: StringRAL = ''): TRALMCPResource;
    /// The resource AURI; nil when none.
    function Find(const AURI: StringRAL): TRALMCPResource;

    /// The resources by position.
    property Items[AIndex: IntegerRAL]: TRALMCPResource read GetItem; default;
  end;

  { A family of resources whose URIs follow a template (RFC 6570 simple
    expressions: a name in braces, or a name after a plus sign in braces). }
  TRALMCPResourceTemplate = class(TCollectionItem)
  private
    FDescription: StringRAL;
    FMimeType: StringRAL;
    FName: StringRAL;
    FOnRead: TRALMCPTemplateReadEvent;
    FTitle: StringRAL;
    FURITemplate: StringRAL;
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(ASource: TPersistent); override;
    /// The template as resources/templates/list shows it; the caller frees it.
    function Definition: TRALJSONObject;
    /// Whether AURI fits the template; the values go to AVariables.
    function Match(const AURI: StringRAL; AVariables: TStrings): Boolean;
  published
    /// What the resources hold.
    property Description: StringRAL read FDescription write FDescription;
    /// The media type of their contents.
    property MimeType: StringRAL read FMimeType write FMimeType;
    /// The template name.
    property Name: StringRAL read FName write FName;
    /// Reads a resource of the template.
    property OnRead: TRALMCPTemplateReadEvent read FOnRead write FOnRead;
    /// A name for people.
    property Title: StringRAL read FTitle write FTitle;
    /// The URI template, such as file:///docs/ followed by the path variable in braces.
    property URITemplate: StringRAL read FURITemplate write FURITemplate;
  end;

  /// The resource templates of a server.
  TRALMCPResourceTemplates = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALMCPResourceTemplate;
  public
    /// Adds the template AURITemplate named AName with AEvent and returns it.
    function AddTemplate(const AURITemplate, AName: StringRAL;
      AEvent: TRALMCPTemplateReadEvent): TRALMCPResourceTemplate;

    /// The templates by position.
    property Items[AIndex: IntegerRAL]: TRALMCPResourceTemplate read GetItem; default;
  end;

  /// An argument of a prompt.
  TRALMCPPromptArgument = class(TCollectionItem)
  private
    FDescription: StringRAL;
    FName: StringRAL;
    FRequired: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(ASource: TPersistent); override;
  published
    /// What the argument is.
    property Description: StringRAL read FDescription write FDescription;
    /// The argument name.
    property Name: StringRAL read FName write FName;
    /// Whether a get without it is Invalid params.
    property Required: Boolean read FRequired write FRequired default False;
  end;

  /// The arguments of a prompt.
  TRALMCPPromptArguments = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALMCPPromptArgument;
  public
    /// Adds the argument AName and returns it.
    function AddArgument(const AName: StringRAL; ARequired: Boolean = False;
      const ADescription: StringRAL = ''): TRALMCPPromptArgument;

    /// The arguments by position.
    property Items[AIndex: IntegerRAL]: TRALMCPPromptArgument read GetItem; default;
  end;

  /// A prompt template the user may pick.
  TRALMCPPrompt = class(TCollectionItem)
  private
    FArguments: TRALMCPPromptArguments;
    FDescription: StringRAL;
    FName: StringRAL;
    FOnGet: TRALMCPPromptEvent;
    FTitle: StringRAL;

    procedure SetArguments(AValue: TRALMCPPromptArguments);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
    /// The prompt as prompts/list shows it; the caller frees it.
    function Definition: TRALJSONObject;
  published
    /// Its arguments.
    property Arguments: TRALMCPPromptArguments read FArguments write SetArguments;
    /// What the prompt does.
    property Description: StringRAL read FDescription write FDescription;
    /// The prompt name.
    property Name: StringRAL read FName write FName;
    /// Builds the prompt.
    property OnGet: TRALMCPPromptEvent read FOnGet write FOnGet;
    /// A name for people.
    property Title: StringRAL read FTitle write FTitle;
  end;

  /// The prompts of a server.
  TRALMCPPrompts = class(TOwnedCollection)
  private
    function GetItem(AIndex: IntegerRAL): TRALMCPPrompt;
  public
    /// Adds the prompt AName with AEvent and returns it.
    function AddPrompt(const AName, ADescription: StringRAL; AEvent: TRALMCPPromptEvent): TRALMCPPrompt;
    /// The prompt AName; nil when none.
    function Find(const AName: StringRAL): TRALMCPPrompt;

    /// The prompts by position.
    property Items[AIndex: IntegerRAL]: TRALMCPPrompt read GetItem; default;
  end;

  { An MCP server (cRALMCPProtocolVersion, stateless): declare tools, resources
    and prompts in the Object Inspector or in code, and link a transport
    (TRALMCPModule for HTTP, TRALMCPStdio for a console program). }
  TRALMCPServer = class(TRALComponent)
  private
    FCacheScope: TRALMCPCacheScope;
    FCacheTTL: IntegerRAL;
    /// Calls running, as TRALMCPSubscription (channel and id), for cancellation.
    FCalls: TList;
    FInstructions: StringRAL;
    /// Guards FCalls, FSubscriptions and FTasks.
    FLock: TCriticalSection;
    FOnComplete: TRALMCPCompleteEvent;
    FPageSize: IntegerRAL;
    FPrompts: TRALMCPPrompts;
    FResources: TRALMCPResources;
    FResourceTemplates: TRALMCPResourceTemplates;
    FServerDescription: StringRAL;
    FServerName: StringRAL;
    FServerTitle: StringRAL;
    FServerVersion: StringRAL;
    FStateKey: StringRAL;
    FStateLife: IntegerRAL;
    /// Whether the server is going away: listens end.
    FStopping: Boolean;
    /// The listens open, as TRALMCPSubscription.
    FSubscriptions: TList;
    FTaskPollInterval: IntegerRAL;
    /// The tasks, as TRALMCPTaskItem.
    FTasks: TList;
    FTaskStore: TRALKeyValueStore;
    FTaskTTL: IntegerRAL;
    FTools: TRALMCPTools;
    FWebsiteURL: StringRAL;

    /// Answers completion/complete.
    function DoComplete(ACall: TRALMCPCall): TRALJSONObject;
    /// Answers server/discover.
    function DoDiscover(ACall: TRALMCPCall): TRALJSONObject;
    /// Answers prompts/get.
    function DoGetPrompt(ACall: TRALMCPCall): TRALJSONObject;
    /// Answers the list methods, a page from ACursor.
    function DoList(ACall: TRALMCPCall): TRALJSONObject;
    /// Answers subscriptions/listen; returns when the stream ends.
    function DoListen(ACall: TRALMCPCall; AId: TRALJSON): TRALJSONObject;
    /// Answers resources/read.
    function DoReadResource(ACall: TRALMCPCall): TRALJSONObject;
    /// Answers tasks/get, tasks/update and tasks/cancel.
    function DoTask(ACall: TRALMCPCall): TRALJSONObject;
    /// Answers tools/call.
    function DoToolCall(ACall: TRALMCPCall): TRALJSONObject;
    /// Adds resultType, the cache fields when ACacheable, and serverInfo to AResult.
    procedure Finish(AResult: TRALJSONObject; ACacheable: Boolean);
    /// The key that signs request states.
    function GetStateKey: TBytes;
    /// The result asking for input: the requests and the signed state.
    function InputRequiredResult(ACall: TRALMCPCall): TRALJSONObject;
    /// Whether the request of ACall may use ATool (Roles).
    function MayUse(ACall: TRALMCPCall; const ARoles: StringRAL): Boolean;
    /// Sends AMessage to the listens subscribed by AFilter (and, for tasks, ATaskId).
    procedure Publish(const AFilter, AMethod: StringRAL; AParams: TRALJSONObject;
      const AURI: StringRAL = ''; const ATaskId: StringRAL = '');
    { The state signed for the client: AState with an expiry, bound to the
      method and the name or URI of ACall. }
    function SealState(ACall: TRALMCPCall; const AState: StringRAL): StringRAL;
    procedure SetPrompts(AValue: TRALMCPPrompts);
    procedure SetResources(AValue: TRALMCPResources);
    procedure SetResourceTemplates(AValue: TRALMCPResourceTemplates);
    procedure SetTaskStore(AValue: TRALKeyValueStore);
    procedure SetTools(AValue: TRALMCPTools);
    { The state the client sent back to ACall, checked and opened; raises
      Invalid params when it was changed or expired. }
    function UnsealState(ACall: TRALMCPCall; const ASealed: StringRAL): StringRAL;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Marks the running call AId (JSON text of the id) of AChannelGroup as
      cancelled (notifications/cancelled on stdio). }
    procedure CancelCall(AGroup: TObject; const AId: StringRAL);
    { Answers the JSON-RPC message ARequest received over AChannel: the answer
      text, or '' for a notification. AStatus is the HTTP status a transport
      gives it (200, 202, 400, 404). Runs on the caller's thread. }
    function Execute(const ARequest: StringRAL; AChannel: TRALMCPChannel;
      out AStatus: IntegerRAL): StringRAL;
    /// Whether any subscriptions/listen is open.
    function Listening: Boolean;
    /// Tells listens that the list of prompts changed.
    procedure NotifyPromptsChanged;
    /// Tells listens that the list of resources changed.
    procedure NotifyResourcesChanged;
    /// Tells listens subscribed to AURI that the resource changed.
    procedure NotifyResourceUpdated(const AURI: StringRAL);
    /// Tells listens that the list of tools changed.
    procedure NotifyToolsChanged;
    /// Ends the open listens (a server shutting down).
    procedure StopListens;
  published
    /// Who may cache the lists and reads: private (default) or public.
    property CacheScope: TRALMCPCacheScope read FCacheScope write FCacheScope default mcsPrivate;
    /// How long clients may keep lists and reads, in milliseconds; 0 = not at all.
    property CacheTTL: IntegerRAL read FCacheTTL write FCacheTTL default 0;
    /// Text that tells the model how to use the server.
    property Instructions: StringRAL read FInstructions write FInstructions;
    /// Completes prompt and resource arguments (capability completions).
    property OnComplete: TRALMCPCompleteEvent read FOnComplete write FOnComplete;
    /// Items per page of the lists; 0 = all in one page.
    property PageSize: IntegerRAL read FPageSize write FPageSize default 0;
    /// The prompts.
    property Prompts: TRALMCPPrompts read FPrompts write SetPrompts;
    /// The resources.
    property Resources: TRALMCPResources read FResources write SetResources;
    /// The resource templates.
    property ResourceTemplates: TRALMCPResourceTemplates read FResourceTemplates
      write SetResourceTemplates;
    /// What the server is, for people.
    property ServerDescription: StringRAL read FServerDescription write FServerDescription;
    /// The server name sent in serverInfo.
    property ServerName: StringRAL read FServerName write FServerName;
    /// A name for people.
    property ServerTitle: StringRAL read FServerTitle write FServerTitle;
    /// The server version sent in serverInfo.
    property ServerVersion: StringRAL read FServerVersion write FServerVersion;
    { The secret that signs the state of input requests; empty = a random one
      per process (servers behind a balancer share one). }
    property StateKey: StringRAL read FStateKey write FStateKey;
    /// Seconds a signed state stays valid.
    property StateLife: IntegerRAL read FStateLife write FStateLife default 600;
    /// The poll interval suggested to clients for tasks, in milliseconds.
    property TaskPollInterval: IntegerRAL read FTaskPollInterval write FTaskPollInterval
      default 1000;
    { Where task snapshots are kept besides memory, so that tasks/get works on
      any server sharing the store; nil = memory only. }
    property TaskStore: TRALKeyValueStore read FTaskStore write SetTaskStore;
    /// How long a task is kept, in milliseconds from its creation.
    property TaskTTL: IntegerRAL read FTaskTTL write FTaskTTL default 3600000;
    /// The tools.
    property Tools: TRALMCPTools read FTools write SetTools;
    /// A website of the server, sent in serverInfo.
    property WebsiteURL: StringRAL read FWebsiteURL write FWebsiteURL;
  end;

/// A new text content block.
function RALMCPTextContent(const AText: StringRAL): TRALJSONObject;
/// The current UTC time in ISO 8601 with Z.
function RALMCPNow: StringRAL;

implementation

const
  /// The log levels, from the least to the most severe.
  cLogLevels: array[0..7] of StringRAL = ('debug', 'info', 'notice', 'warning', 'error',
    'critical', 'alert', 'emergency');
  /// The names of the task states.
  cTaskStatus: array[TRALMCPTaskStatus] of StringRAL = ('working', 'input_required',
    'completed', 'failed', 'cancelled');
  /// The JSON Schema types of the parameter kinds.
  cParamTypes: array[TRALMCPParamType] of StringRAL = ('string', 'number', 'integer',
    'boolean', 'object', 'array');

type
  /// An open subscriptions/listen.
  TRALMCPSubscription = class
  public
    /// The transport of the stream.
    Channel: TRALMCPChannel;
    /// The JSON of the request id.
    Id: StringRAL;
    /// Whether the prompts list is followed.
    Prompts: Boolean;
    /// Whether the resources list is followed.
    Resources: Boolean;
    /// The resource URIs followed.
    ResourceURIs: TStringList;
    /// The task ids followed.
    TaskIds: TStringList;
    /// Whether the tools list is followed.
    Tools: Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

  /// A task: its state, and the input it waits for.
  TRALMCPTaskItem = class
  public
    /// Whether tasks/cancel asked to stop.
    CancelAsked: Boolean;
    /// When it was created, ISO 8601.
    CreatedAt: StringRAL;
    /// When it was created, in ticks of the server clock.
    CreatedTick: Int64;
    /// The error of a failed task, JSON text.
    Error: StringRAL;
    /// The input requests pending, JSON text.
    InputRequests: StringRAL;
    /// The answers that came with tasks/update, JSON object owned.
    InputResponses: TRALJSONObject;
    /// When it changed last, ISO 8601.
    LastUpdatedAt: StringRAL;
    /// The result of a completed task, JSON text.
    Result: StringRAL;
    /// The state.
    Status: TRALMCPTaskStatus;
    /// A message of the state.
    StatusMessage: StringRAL;
    /// The task id.
    TaskId: StringRAL;
    /// Wakes the task when input arrives or it is cancelled.
    Wake: TEvent;

    constructor Create;
    destructor Destroy; override;

    /// The task as tasks/get shows it; the caller frees it.
    function Snapshot(ATTL, APoll: IntegerRAL): TRALJSONObject;
  end;

  /// A channel with no stream: a task running in its own thread.
  TRALMCPTaskChannel = class(TRALMCPChannel)
  public
    /// The task.
    Task: TRALMCPTaskItem;

    function Cancelled: Boolean; override;
    function Notify(const AMessage: StringRAL): Boolean; override;
  end;

  /// Runs a tool call as a task.
  TRALMCPTaskThread = class(TThread)
  private
    /// The capabilities of the client, owned.
    FCapabilities: TRALJSON;
    /// The params of the call, owned.
    FParams: TRALJSONObject;
    /// The server of the task.
    FServer: TRALMCPServer;
    /// The task run.
    FTask: TRALMCPTaskItem;
    /// The tool run.
    FTool: TRALMCPTool;
  protected
    procedure Execute; override;
  public
    /// Runs ATool with AParams (owned) for ATask on AServer.
    constructor Create(AServer: TRALMCPServer; ATool: TRALMCPTool; ATask: TRALMCPTaskItem;
      AParams: TRALJSONObject; ACapabilities: TRALJSON);
    destructor Destroy; override;
  end;

var
  /// The random state key of the process.
  vProcessKey: TBytes;

function RALMCPTextContent(const AText: StringRAL): TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('type', 'text');
  Result.Add('text', AText);
end;

function RALMCPNow: StringRAL;
begin
  Result := RALDateTimeToISO8601(RALDateTimeToGMT(Now), True);
end;

/// AValue as a JSON string literal.
function Quote(const AValue: StringRAL): StringRAL;
var
  vString: TRALJSONString;
begin
  vString := TRALJSONString.Create(AValue);
  try
    Result := vString.ToJSON;
  finally
    vString.Free;
  end;
end;

/// The member AName of AObject when it is a string; ADefault otherwise.
function StrOf(AObject: TRALJSON; const AName: StringRAL; const ADefault: StringRAL = ''): StringRAL;
var
  vValue: TRALJSON;
begin
  Result := ADefault;
  if (AObject = nil) or (AObject.Kind <> rjkObject) then
    Exit;
  vValue := TRALJSONObject(AObject).Find(AName);
  if (vValue <> nil) and (vValue.Kind = rjkString) then
    Result := vValue.AsString;
end;

/// A copy of AValue; nil for nil.
function Clone(AValue: TRALJSON): TRALJSON;
begin
  Result := RALJSONClone(AValue);
end;

/// Raises the MCP error ACode with AMessage, AData and the HTTP status AStatus.
procedure Refuse(ACode: IntegerRAL; const AMessage: string; const AData: StringRAL = '';
  AStatus: IntegerRAL = 200);
begin
  raise ERALMCPError.CreateMCP(ACode, AMessage, AData, AStatus);
end;

/// The index of ALevel among the log levels; -1 when it is not one.
function LevelIndex(const ALevel: StringRAL): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to High(cLogLevels) do
    if cLogLevels[vInt] = ALevel then
    begin
      Result := vInt;
      Exit;
    end;
  Result := -1;
end;

/// HMAC-SHA256 of AData with AKey.
function HMAC256(const AData: StringRAL; const AKey: TBytes): TBytes;
var
  vHash: TRALSHA2_32;
  vStream: TBytesStream;
begin
  vHash := TRALSHA2_32.Create;
  vStream := TBytesStream.Create;
  try
    vHash.Version := rsv256;
    if AData <> '' then
      vStream.WriteBuffer(Pointer(AData)^, Length(AData));
    vStream.Position := 0;
    Result := vHash.HMACAsDigest(vStream, AKey);
  finally
    vStream.Free;
    vHash.Free;
  end;
end;

/// Base64url of AData, without padding.
function B64Url(const AData: TBytes): StringRAL;
begin
  if Length(AData) = 0 then
    Result := ''
  else
    Result := TRALBase64.EncodeUrl(@AData[0], Length(AData));
end;

/// Base64url of the text AText.
function B64UrlText(const AText: StringRAL): StringRAL;
begin
  if AText = '' then
    Result := ''
  else
    Result := TRALBase64.EncodeUrl(Pointer(AText), Length(AText));
end;

/// The text of base64url AValue; '' when it is not base64url.
function UnB64Url(const AValue: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 1 to Length(AValue) do
    if not (AValue[vInt] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_']) then
      Exit;
  try
    Result := TRALBase64.Decode(TRALBase64.FromBase64Url(AValue));
  except
    Result := '';
  end;
end;

/// A random id of 128 bits, as hexadecimal.
function NewId: StringRAL;
const
  cHex: array[0..15] of AnsiChar = '0123456789abcdef';
var
  vBytes: TBytes;
  vInt: IntegerRAL;
begin
  vBytes := RandomBytes(16);
  SetLength(Result, 32);
  for vInt := 0 to 15 do
  begin
    Result[vInt * 2 + 1] := cHex[vBytes[vInt] shr 4];
    Result[vInt * 2 + 2] := cHex[vBytes[vInt] and 15];
  end;
end;

/// Milliseconds of a monotonic clock.
function Ticks: Int64;
begin
  Result := RALTickMicro div 1000;
end;

{ ERALMCPError }

constructor ERALMCPError.CreateMCP(ACode: IntegerRAL; const AMessage: string;
  const AData: StringRAL; AStatus: IntegerRAL);
begin
  CreateRPC(ACode, AMessage, AData);
  FHTTPStatus := AStatus;
end;

{ TRALMCPChannel }

procedure TRALMCPChannel.Cancel;
begin
  FCancelled := True;
end;

function TRALMCPChannel.Cancelled: Boolean;
begin
  Result := FCancelled;
end;

function TRALMCPChannel.Streaming: Boolean;
begin
  Result := False;
end;

{ TRALMCPCall }

constructor TRALMCPCall.Create(AServer: TRALMCPServer; AChannel: TRALMCPChannel;
  const AMethod: StringRAL; AParams: TRALJSONObject);
begin
  inherited Create;
  FServer := AServer;
  FChannel := AChannel;
  FMethod := AMethod;
  FParams := AParams;
  FLogLevel := -1;
  FLastProgress := -1;
end;

destructor TRALMCPCall.Destroy;
begin
  FInputRequests.Free;
  inherited Destroy;
end;

function TRALMCPCall.Argument(const AName: StringRAL): TRALJSON;
begin
  if Arguments = nil then
    Result := nil
  else
    Result := Arguments.Find(AName);
end;

function TRALMCPCall.Cancelled: Boolean;
begin
  Result := (FChannel <> nil) and FChannel.Cancelled;
end;

procedure TRALMCPCall.Elicit(const AKey, AMessage: StringRAL; ASchema: TRALJSONObject);
var
  vRequest, vParams: TRALJSONObject;
begin
  vRequest := TRALJSONObject.Create;
  vRequest.Add('method', 'elicitation/create');
  vParams := TRALJSONObject.Create;
  vParams.Add('mode', 'form');
  vParams.Add('message', AMessage);
  vParams.Add('requestedSchema', ASchema);
  vRequest.Add('params', vParams);
  RequestInput(AKey, vRequest);
end;

function TRALMCPCall.GetArguments: TRALJSONObject;
var
  vArgs: TRALJSON;
begin
  Result := nil;
  if FParams = nil then
    Exit;
  vArgs := FParams.Find('arguments');
  if (vArgs <> nil) and (vArgs.Kind = rjkObject) then
    Result := TRALJSONObject(vArgs);
end;

function TRALMCPCall.HasCapability(const APath: StringRAL): Boolean;
var
  vNode: TRALJSON;
  vRest, vPart: StringRAL;
  vDot: IntegerRAL;
begin
  vNode := FCapabilities;
  vRest := APath;
  // Extension ids hold dots and slashes: "extensions." takes the rest whole.
  while (vNode <> nil) and (vRest <> '') do
  begin
    if vNode.Kind <> rjkObject then
    begin
      vNode := nil;
      Break;
    end;
    if Copy(vRest, 1, 11) = 'extensions.' then
    begin
      vNode := TRALJSONObject(vNode).Find('extensions');
      if (vNode <> nil) and (vNode.Kind = rjkObject) then
        vNode := TRALJSONObject(vNode).Find(Copy(vRest, 12, Length(vRest)))
      else
        vNode := nil;
      vRest := '';
      Break;
    end;
    vDot := Pos('.', string(vRest));
    if vDot = 0 then
    begin
      vPart := vRest;
      vRest := '';
    end
    else
    begin
      vPart := Copy(vRest, 1, vDot - 1);
      vRest := Copy(vRest, vDot + 1, Length(vRest));
    end;
    vNode := TRALJSONObject(vNode).Find(vPart);
    // An empty elicitation object stands for the form mode.
    if (vNode <> nil) and (vPart = 'elicitation') and (vRest = 'form') and
      (vNode.Kind = rjkObject) and (TRALJSONObject(vNode).Count = 0) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := (vNode <> nil) and not vNode.IsNull;
end;

function TRALMCPCall.InputRequired: Boolean;
begin
  Result := (FInputRequests <> nil) or FNewStateSet;
end;

function TRALMCPCall.InputResponse(const AKey: StringRAL): TRALJSON;
begin
  if (FInputResponses = nil) or (FInputResponses.Kind <> rjkObject) then
    Result := nil
  else
    Result := TRALJSONObject(FInputResponses).Find(AKey);
end;

procedure TRALMCPCall.Log(const ALevel: StringRAL; const AData: StringRAL;
  const ALogger: StringRAL);
var
  vLevel: IntegerRAL;
  vMessage, vParams: TRALJSONObject;
begin
  vLevel := LevelIndex(ALevel);
  if (FLogLevel < 0) or (vLevel < FLogLevel) or (FChannel = nil) or not FChannel.Streaming then
    Exit;
  vMessage := TRALJSONObject.Create;
  try
    vMessage.Add('jsonrpc', '2.0');
    vMessage.Add('method', 'notifications/message');
    vParams := TRALJSONObject.Create;
    vParams.Add('level', ALevel);
    if ALogger <> '' then
      vParams.Add('logger', ALogger);
    vParams.Add('data', TRALJSON.ParseJSON(AData));
    vMessage.Add('params', vParams);
    FChannel.Notify(vMessage.ToJSON);
  finally
    vMessage.Free;
  end;
end;

procedure TRALMCPCall.Progress(AProgress, ATotal: Double; const AMessage: StringRAL);
var
  vMessage, vParams: TRALJSONObject;
begin
  if (FProgressToken = nil) or (FChannel = nil) or not FChannel.Streaming or
    (AProgress <= FLastProgress) then
    Exit;
  FLastProgress := AProgress;
  vMessage := TRALJSONObject.Create;
  try
    vMessage.Add('jsonrpc', '2.0');
    vMessage.Add('method', 'notifications/progress');
    vParams := TRALJSONObject.Create;
    vParams.Add('progressToken', Clone(FProgressToken));
    vParams.Add('progress', AProgress);
    if ATotal >= 0 then
      vParams.Add('total', ATotal);
    if AMessage <> '' then
      vParams.Add('message', AMessage);
    vMessage.Add('params', vParams);
    FChannel.Notify(vMessage.ToJSON);
  finally
    vMessage.Free;
  end;
end;

procedure TRALMCPCall.RequestInput(const AKey: StringRAL; ARequest: TRALJSONObject);
var
  vMethod: StringRAL;
  vNeed: StringRAL;
begin
  vMethod := StrOf(ARequest, 'method');
  if vMethod = 'elicitation/create' then
  begin
    if StrOf(ARequest.Find('params'), 'mode', 'form') = 'url' then
      vNeed := 'elicitation.url'
    else
      vNeed := 'elicitation.form';
  end
  else if vMethod = 'sampling/createMessage' then
    vNeed := 'sampling'
  else
    vNeed := 'roots';
  if not HasCapability(vNeed) then
  begin
    ARequest.Free;
    if vNeed = 'elicitation.url' then
      vNeed := '{"elicitation":{"url":{}}}'
    else if vNeed = 'elicitation.form' then
      vNeed := '{"elicitation":{"form":{}}}'
    else
      vNeed := '{"' + vNeed + '":{}}';
    Refuse(cRALMCPMissingCapability, 'Missing required client capability',
      '{"requiredCapabilities":' + vNeed + '}', 400);
  end;
  if FInputRequests = nil then
    FInputRequests := TRALJSONObject.Create;
  FInputRequests.Add(AKey, ARequest);
end;

procedure TRALMCPCall.SetState(const AState: StringRAL);
begin
  FNewState := AState;
  FNewStateSet := True;
end;

{ TRALMCPToolResult }

constructor TRALMCPToolResult.Create;
begin
  inherited Create;
  FContent := TRALJSONArray.Create;
end;

destructor TRALMCPToolResult.Destroy;
begin
  FContent.Free;
  FStructured.Free;
  inherited Destroy;
end;

procedure TRALMCPToolResult.AddAudio(const AData, AMimeType: StringRAL);
var
  vBlock: TRALJSONObject;
begin
  vBlock := TRALJSONObject.Create;
  vBlock.Add('type', 'audio');
  vBlock.Add('data', AData);
  vBlock.Add('mimeType', AMimeType);
  FContent.Add(vBlock);
end;

procedure TRALMCPToolResult.AddContent(AContent: TRALJSONObject);
begin
  FContent.Add(AContent);
end;

procedure TRALMCPToolResult.AddImage(const AData, AMimeType: StringRAL);
var
  vBlock: TRALJSONObject;
begin
  vBlock := TRALJSONObject.Create;
  vBlock.Add('type', 'image');
  vBlock.Add('data', AData);
  vBlock.Add('mimeType', AMimeType);
  FContent.Add(vBlock);
end;

procedure TRALMCPToolResult.AddResourceBlob(const AURI, AMimeType, ABlob: StringRAL);
var
  vBlock, vResource: TRALJSONObject;
begin
  vBlock := TRALJSONObject.Create;
  vBlock.Add('type', 'resource');
  vResource := TRALJSONObject.Create;
  vResource.Add('uri', AURI);
  if AMimeType <> '' then
    vResource.Add('mimeType', AMimeType);
  vResource.Add('blob', ABlob);
  vBlock.Add('resource', vResource);
  FContent.Add(vBlock);
end;

procedure TRALMCPToolResult.AddResourceLink(const AURI, AName, AMimeType,
  ADescription: StringRAL);
var
  vBlock: TRALJSONObject;
begin
  vBlock := TRALJSONObject.Create;
  vBlock.Add('type', 'resource_link');
  vBlock.Add('uri', AURI);
  vBlock.Add('name', AName);
  if ADescription <> '' then
    vBlock.Add('description', ADescription);
  if AMimeType <> '' then
    vBlock.Add('mimeType', AMimeType);
  FContent.Add(vBlock);
end;

procedure TRALMCPToolResult.AddResourceText(const AURI, AMimeType, AText: StringRAL);
var
  vBlock, vResource: TRALJSONObject;
begin
  vBlock := TRALJSONObject.Create;
  vBlock.Add('type', 'resource');
  vResource := TRALJSONObject.Create;
  vResource.Add('uri', AURI);
  if AMimeType <> '' then
    vResource.Add('mimeType', AMimeType);
  vResource.Add('text', AText);
  vBlock.Add('resource', vResource);
  FContent.Add(vBlock);
end;

procedure TRALMCPToolResult.AddText(const AText: StringRAL);
begin
  FContent.Add(RALMCPTextContent(AText));
end;

procedure TRALMCPToolResult.Fail(const AText: StringRAL);
begin
  FIsError := True;
  AddText(AText);
end;

procedure TRALMCPToolResult.SetStructured(AValue: TRALJSON);
begin
  if AValue = FStructured then
    Exit;
  FStructured.Free;
  FStructured := AValue;
  if AValue <> nil then
    AddText(AValue.ToJSON);
end;

{ TRALMCPResourceContents }

constructor TRALMCPResourceContents.Create(const AURI: StringRAL);
begin
  inherited Create;
  FURI := AURI;
  FContents := TRALJSONArray.Create;
end;

destructor TRALMCPResourceContents.Destroy;
begin
  FContents.Free;
  inherited Destroy;
end;

procedure TRALMCPResourceContents.AddBlob(const ABlob, AMimeType, AURI: StringRAL);
var
  vItem: TRALJSONObject;
begin
  vItem := TRALJSONObject.Create;
  if AURI = '' then
    vItem.Add('uri', FURI)
  else
    vItem.Add('uri', AURI);
  if AMimeType <> '' then
    vItem.Add('mimeType', AMimeType);
  vItem.Add('blob', ABlob);
  FContents.Add(vItem);
end;

procedure TRALMCPResourceContents.AddText(const AText, AMimeType, AURI: StringRAL);
var
  vItem: TRALJSONObject;
begin
  vItem := TRALJSONObject.Create;
  if AURI = '' then
    vItem.Add('uri', FURI)
  else
    vItem.Add('uri', AURI);
  if AMimeType <> '' then
    vItem.Add('mimeType', AMimeType);
  vItem.Add('text', AText);
  FContents.Add(vItem);
end;

{ TRALMCPPromptResult }

constructor TRALMCPPromptResult.Create;
begin
  inherited Create;
  FMessages := TRALJSONArray.Create;
end;

destructor TRALMCPPromptResult.Destroy;
begin
  FMessages.Free;
  inherited Destroy;
end;

procedure TRALMCPPromptResult.AddMessage(const ARole: StringRAL; AContent: TRALJSONObject);
var
  vMessage: TRALJSONObject;
begin
  vMessage := TRALJSONObject.Create;
  vMessage.Add('role', ARole);
  vMessage.Add('content', AContent);
  FMessages.Add(vMessage);
end;

procedure TRALMCPPromptResult.AddText(const ARole, AText: StringRAL);
begin
  AddMessage(ARole, RALMCPTextContent(AText));
end;

{ TRALMCPParam }

constructor TRALMCPParam.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FRequired := True;
  FValues := TStringList.Create;
end;

destructor TRALMCPParam.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

procedure TRALMCPParam.Assign(ASource: TPersistent);
begin
  if ASource is TRALMCPParam then
  begin
    FName := TRALMCPParam(ASource).Name;
    FDescription := TRALMCPParam(ASource).Description;
    FHeader := TRALMCPParam(ASource).Header;
    FParamType := TRALMCPParam(ASource).ParamType;
    FRequired := TRALMCPParam(ASource).Required;
    FValues.Assign(TRALMCPParam(ASource).Values);
  end
  else
    inherited Assign(ASource);
end;

function TRALMCPParam.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := string(FName);
end;

procedure TRALMCPParam.SetValues(AValue: TStrings);
begin
  FValues.Assign(AValue);
end;

{ TRALMCPParams }

function TRALMCPParams.AddParam(const AName: StringRAL; AType: TRALMCPParamType;
  const ADescription: StringRAL; ARequired: Boolean): TRALMCPParam;
begin
  Result := TRALMCPParam(Add);
  Result.Name := AName;
  Result.ParamType := AType;
  Result.Description := ADescription;
  Result.Required := ARequired;
end;

function TRALMCPParams.GetItem(AIndex: IntegerRAL): TRALMCPParam;
begin
  Result := TRALMCPParam(inherited Items[AIndex]);
end;

{ TRALMCPTool }

constructor TRALMCPTool.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FParams := TRALMCPParams.Create(Self, TRALMCPParam);
end;

destructor TRALMCPTool.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

procedure TRALMCPTool.Assign(ASource: TPersistent);
begin
  if ASource is TRALMCPTool then
  begin
    FName := TRALMCPTool(ASource).Name;
    FTitle := TRALMCPTool(ASource).Title;
    FDescription := TRALMCPTool(ASource).Description;
    FHints := TRALMCPTool(ASource).Hints;
    FInputSchema := TRALMCPTool(ASource).InputSchema;
    FOutputSchema := TRALMCPTool(ASource).OutputSchema;
    FParams.Assign(TRALMCPTool(ASource).Params);
    FRoles := TRALMCPTool(ASource).Roles;
    FRunAsTask := TRALMCPTool(ASource).RunAsTask;
    FOnCall := TRALMCPTool(ASource).OnCall;
  end
  else
    inherited Assign(ASource);
end;

function TRALMCPTool.Definition: TRALJSONObject;
var
  vHints: TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('name', FName);
  if FTitle <> '' then
    Result.Add('title', FTitle);
  if FDescription <> '' then
    Result.Add('description', FDescription);
  Result.Add('inputSchema', Schema);
  if FOutputSchema <> '' then
    Result.Add('outputSchema', TRALJSON.ParseJSON(FOutputSchema));
  if FHints <> [] then
  begin
    vHints := TRALJSONObject.Create;
    vHints.Add('readOnlyHint', thReadOnly in FHints);
    vHints.Add('destructiveHint', thDestructive in FHints);
    vHints.Add('idempotentHint', thIdempotent in FHints);
    vHints.Add('openWorldHint', thOpenWorld in FHints);
    Result.Add('annotations', vHints);
  end;
end;

function TRALMCPTool.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := string(FName);
end;

function TRALMCPTool.Schema: TRALJSONObject;
var
  vProps, vProp: TRALJSONObject;
  vRequired, vEnum: TRALJSONArray;
  vInt, vValue: IntegerRAL;
  vParam: TRALMCPParam;
begin
  if FInputSchema <> '' then
  begin
    Result := TRALJSONObject(TRALJSON.ParseJSON(FInputSchema));
    Exit;
  end;
  Result := TRALJSONObject.Create;
  Result.Add('type', 'object');
  vProps := TRALJSONObject.Create;
  vRequired := TRALJSONArray.Create;
  for vInt := 0 to FParams.Count - 1 do
  begin
    vParam := FParams[vInt];
    vProp := TRALJSONObject.Create;
    vProp.Add('type', cParamTypes[vParam.ParamType]);
    if vParam.Description <> '' then
      vProp.Add('description', vParam.Description);
    if vParam.Values.Count > 0 then
    begin
      vEnum := TRALJSONArray.Create;
      for vValue := 0 to vParam.Values.Count - 1 do
        vEnum.Add(StringRAL(vParam.Values[vValue]));
      vProp.Add('enum', vEnum);
    end;
    if vParam.Header <> '' then
      vProp.Add('x-mcp-header', vParam.Header);
    vProps.Add(vParam.Name, vProp);
    if vParam.Required then
      vRequired.Add(vParam.Name);
  end;
  Result.Add('properties', vProps);
  if vRequired.Count > 0 then
    Result.Add('required', vRequired)
  else
    vRequired.Free;
  if FParams.Count = 0 then
    Result.Add('additionalProperties', False);
end;

procedure TRALMCPTool.SetParams(AValue: TRALMCPParams);
begin
  FParams.Assign(AValue);
end;

{ TRALMCPTools }

function TRALMCPTools.AddTool(const AName, ADescription: StringRAL;
  AEvent: TRALMCPToolEvent): TRALMCPTool;
begin
  Result := TRALMCPTool(Add);
  Result.Name := AName;
  Result.Description := ADescription;
  Result.OnCall := AEvent;
end;

function TRALMCPTools.Find(const AName: StringRAL): TRALMCPTool;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
    if Items[vInt].Name = AName then
    begin
      Result := Items[vInt];
      Exit;
    end;
  Result := nil;
end;

function TRALMCPTools.GetItem(AIndex: IntegerRAL): TRALMCPTool;
begin
  Result := TRALMCPTool(inherited Items[AIndex]);
end;

{ TRALMCPResource }

procedure TRALMCPResource.Assign(ASource: TPersistent);
begin
  if ASource is TRALMCPResource then
  begin
    FURI := TRALMCPResource(ASource).URI;
    FName := TRALMCPResource(ASource).Name;
    FTitle := TRALMCPResource(ASource).Title;
    FDescription := TRALMCPResource(ASource).Description;
    FMimeType := TRALMCPResource(ASource).MimeType;
    FOnRead := TRALMCPResource(ASource).OnRead;
  end
  else
    inherited Assign(ASource);
end;

function TRALMCPResource.Definition: TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('uri', FURI);
  Result.Add('name', FName);
  if FTitle <> '' then
    Result.Add('title', FTitle);
  if FDescription <> '' then
    Result.Add('description', FDescription);
  if FMimeType <> '' then
    Result.Add('mimeType', FMimeType);
end;

function TRALMCPResource.GetDisplayName: string;
begin
  if FURI = '' then
    Result := inherited GetDisplayName
  else
    Result := string(FURI);
end;

{ TRALMCPResources }

function TRALMCPResources.AddResource(const AURI, AName: StringRAL; AEvent: TRALMCPReadEvent;
  const AMimeType: StringRAL): TRALMCPResource;
begin
  Result := TRALMCPResource(Add);
  Result.URI := AURI;
  Result.Name := AName;
  Result.MimeType := AMimeType;
  Result.OnRead := AEvent;
end;

function TRALMCPResources.Find(const AURI: StringRAL): TRALMCPResource;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
    if Items[vInt].URI = AURI then
    begin
      Result := Items[vInt];
      Exit;
    end;
  Result := nil;
end;

function TRALMCPResources.GetItem(AIndex: IntegerRAL): TRALMCPResource;
begin
  Result := TRALMCPResource(inherited Items[AIndex]);
end;

{ TRALMCPResourceTemplate }

procedure TRALMCPResourceTemplate.Assign(ASource: TPersistent);
begin
  if ASource is TRALMCPResourceTemplate then
  begin
    FURITemplate := TRALMCPResourceTemplate(ASource).URITemplate;
    FName := TRALMCPResourceTemplate(ASource).Name;
    FTitle := TRALMCPResourceTemplate(ASource).Title;
    FDescription := TRALMCPResourceTemplate(ASource).Description;
    FMimeType := TRALMCPResourceTemplate(ASource).MimeType;
    FOnRead := TRALMCPResourceTemplate(ASource).OnRead;
  end
  else
    inherited Assign(ASource);
end;

function TRALMCPResourceTemplate.Definition: TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('uriTemplate', FURITemplate);
  Result.Add('name', FName);
  if FTitle <> '' then
    Result.Add('title', FTitle);
  if FDescription <> '' then
    Result.Add('description', FDescription);
  if FMimeType <> '' then
    Result.Add('mimeType', FMimeType);
end;

function TRALMCPResourceTemplate.GetDisplayName: string;
begin
  if FURITemplate = '' then
    Result := inherited GetDisplayName
  else
    Result := string(FURITemplate);
end;

function TRALMCPResourceTemplate.Match(const AURI: StringRAL; AVariables: TStrings): Boolean;
var
  vTpl, vURI: IntegerRAL;
  vClose, vStop: IntegerRAL;
  vName, vValue: StringRAL;
  vReserved: Boolean;
  vNext: AnsiChar;
begin
  // RFC 6570 level 1 and 2: a simple expression takes up to the next literal
  // and no '/', a reserved one (with +) may take '/'.
  Result := False;
  AVariables.Clear;
  vTpl := 1;
  vURI := 1;
  while vTpl <= Length(FURITemplate) do
  begin
    if FURITemplate[vTpl] = '{' then
    begin
      vClose := vTpl + 1;
      while (vClose <= Length(FURITemplate)) and (FURITemplate[vClose] <> '}') do
        Inc(vClose);
      vName := Copy(FURITemplate, vTpl + 1, vClose - vTpl - 1);
      vReserved := (vName <> '') and (vName[1] = '+');
      if vReserved then
        Delete(vName, 1, 1);
      vTpl := vClose + 1;
      if vTpl <= Length(FURITemplate) then
        vNext := FURITemplate[vTpl]
      else
        vNext := #0;
      vStop := vURI;
      while (vStop <= Length(AURI)) and (AURI[vStop] <> vNext) and
        (vReserved or (AURI[vStop] <> '/')) do
        Inc(vStop);
      vValue := Copy(AURI, vURI, vStop - vURI);
      if vValue = '' then
        Exit;
      AVariables.Values[string(vName)] := string(vValue);
      vURI := vStop;
    end
    else
    begin
      if (vURI > Length(AURI)) or (AURI[vURI] <> FURITemplate[vTpl]) then
        Exit;
      Inc(vTpl);
      Inc(vURI);
    end;
  end;
  Result := vURI > Length(AURI);
end;

{ TRALMCPResourceTemplates }

function TRALMCPResourceTemplates.AddTemplate(const AURITemplate, AName: StringRAL;
  AEvent: TRALMCPTemplateReadEvent): TRALMCPResourceTemplate;
begin
  Result := TRALMCPResourceTemplate(Add);
  Result.URITemplate := AURITemplate;
  Result.Name := AName;
  Result.OnRead := AEvent;
end;

function TRALMCPResourceTemplates.GetItem(AIndex: IntegerRAL): TRALMCPResourceTemplate;
begin
  Result := TRALMCPResourceTemplate(inherited Items[AIndex]);
end;

{ TRALMCPPromptArgument }

procedure TRALMCPPromptArgument.Assign(ASource: TPersistent);
begin
  if ASource is TRALMCPPromptArgument then
  begin
    FName := TRALMCPPromptArgument(ASource).Name;
    FDescription := TRALMCPPromptArgument(ASource).Description;
    FRequired := TRALMCPPromptArgument(ASource).Required;
  end
  else
    inherited Assign(ASource);
end;

function TRALMCPPromptArgument.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := string(FName);
end;

{ TRALMCPPromptArguments }

function TRALMCPPromptArguments.AddArgument(const AName: StringRAL; ARequired: Boolean;
  const ADescription: StringRAL): TRALMCPPromptArgument;
begin
  Result := TRALMCPPromptArgument(Add);
  Result.Name := AName;
  Result.Required := ARequired;
  Result.Description := ADescription;
end;

function TRALMCPPromptArguments.GetItem(AIndex: IntegerRAL): TRALMCPPromptArgument;
begin
  Result := TRALMCPPromptArgument(inherited Items[AIndex]);
end;

{ TRALMCPPrompt }

constructor TRALMCPPrompt.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FArguments := TRALMCPPromptArguments.Create(Self, TRALMCPPromptArgument);
end;

destructor TRALMCPPrompt.Destroy;
begin
  FArguments.Free;
  inherited Destroy;
end;

procedure TRALMCPPrompt.Assign(ASource: TPersistent);
begin
  if ASource is TRALMCPPrompt then
  begin
    FName := TRALMCPPrompt(ASource).Name;
    FTitle := TRALMCPPrompt(ASource).Title;
    FDescription := TRALMCPPrompt(ASource).Description;
    FArguments.Assign(TRALMCPPrompt(ASource).Arguments);
    FOnGet := TRALMCPPrompt(ASource).OnGet;
  end
  else
    inherited Assign(ASource);
end;

function TRALMCPPrompt.Definition: TRALJSONObject;
var
  vArgs: TRALJSONArray;
  vArg: TRALJSONObject;
  vInt: IntegerRAL;
begin
  Result := TRALJSONObject.Create;
  Result.Add('name', FName);
  if FTitle <> '' then
    Result.Add('title', FTitle);
  if FDescription <> '' then
    Result.Add('description', FDescription);
  if FArguments.Count > 0 then
  begin
    vArgs := TRALJSONArray.Create;
    for vInt := 0 to FArguments.Count - 1 do
    begin
      vArg := TRALJSONObject.Create;
      vArg.Add('name', FArguments[vInt].Name);
      if FArguments[vInt].Description <> '' then
        vArg.Add('description', FArguments[vInt].Description);
      vArg.Add('required', FArguments[vInt].Required);
      vArgs.Add(vArg);
    end;
    Result.Add('arguments', vArgs);
  end;
end;

function TRALMCPPrompt.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := string(FName);
end;

procedure TRALMCPPrompt.SetArguments(AValue: TRALMCPPromptArguments);
begin
  FArguments.Assign(AValue);
end;

{ TRALMCPPrompts }

function TRALMCPPrompts.AddPrompt(const AName, ADescription: StringRAL;
  AEvent: TRALMCPPromptEvent): TRALMCPPrompt;
begin
  Result := TRALMCPPrompt(Add);
  Result.Name := AName;
  Result.Description := ADescription;
  Result.OnGet := AEvent;
end;

function TRALMCPPrompts.Find(const AName: StringRAL): TRALMCPPrompt;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to Count - 1 do
    if Items[vInt].Name = AName then
    begin
      Result := Items[vInt];
      Exit;
    end;
  Result := nil;
end;

function TRALMCPPrompts.GetItem(AIndex: IntegerRAL): TRALMCPPrompt;
begin
  Result := TRALMCPPrompt(inherited Items[AIndex]);
end;

{ TRALMCPSubscription }

constructor TRALMCPSubscription.Create;
begin
  inherited Create;
  ResourceURIs := TStringList.Create;
  TaskIds := TStringList.Create;
end;

destructor TRALMCPSubscription.Destroy;
begin
  ResourceURIs.Free;
  TaskIds.Free;
  inherited Destroy;
end;

{ TRALMCPTaskItem }

constructor TRALMCPTaskItem.Create;
begin
  inherited Create;
  Wake := TEvent.Create(nil, False, False, '');
end;

destructor TRALMCPTaskItem.Destroy;
begin
  InputResponses.Free;
  Wake.Free;
  inherited Destroy;
end;

function TRALMCPTaskItem.Snapshot(ATTL, APoll: IntegerRAL): TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('taskId', TaskId);
  Result.Add('status', cTaskStatus[Status]);
  if StatusMessage <> '' then
    Result.Add('statusMessage', StatusMessage);
  Result.Add('createdAt', CreatedAt);
  Result.Add('lastUpdatedAt', LastUpdatedAt);
  Result.Add('ttlMs', Int64(ATTL));
  if not (Status in [mtsCompleted, mtsFailed, mtsCancelled]) then
    Result.Add('pollIntervalMs', Int64(APoll));
  case Status of
    mtsInputRequired:
      if InputRequests <> '' then
        Result.Add('inputRequests', TRALJSON.ParseJSON(InputRequests));
    mtsCompleted:
      Result.Add('result', TRALJSON.ParseJSON(Self.Result));
    mtsFailed:
      Result.Add('error', TRALJSON.ParseJSON(Error));
  end;
end;

{ TRALMCPTaskChannel }

function TRALMCPTaskChannel.Cancelled: Boolean;
begin
  Result := Task.CancelAsked;
end;

function TRALMCPTaskChannel.Notify(const AMessage: StringRAL): Boolean;
begin
  // Progress and logs do not reach a task (tasks extension).
  Result := False;
end;

{ TRALMCPTaskThread }

constructor TRALMCPTaskThread.Create(AServer: TRALMCPServer; ATool: TRALMCPTool;
  ATask: TRALMCPTaskItem; AParams: TRALJSONObject; ACapabilities: TRALJSON);
begin
  FServer := AServer;
  FTool := ATool;
  FTask := ATask;
  FParams := AParams;
  FCapabilities := ACapabilities;
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TRALMCPTaskThread.Destroy;
begin
  FParams.Free;
  FCapabilities.Free;
  inherited Destroy;
end;

procedure TRALMCPTaskThread.Execute;
var
  vChannel: TRALMCPTaskChannel;
  vCall: TRALMCPCall;
  vResult: TRALMCPToolResult;
  vAnswer: TRALJSONObject;
  vState: StringRAL;
  vResponses: TRALJSONObject;
  vDone: Boolean;
begin
  vChannel := TRALMCPTaskChannel.Create;
  vChannel.Task := FTask;
  vState := '';
  vResponses := nil;
  try
    vDone := False;
    while not vDone do
    begin
      vCall := TRALMCPCall.Create(FServer, vChannel, 'tools/call', FParams);
      vResult := TRALMCPToolResult.Create;
      try
        vCall.FCapabilities := FCapabilities;
        vCall.FState := vState;
        vCall.FInputResponses := vResponses;
        vCall.FTask := FTask;
        try
          if Assigned(FTool.OnCall) then
            FTool.OnCall(FServer, vCall, vResult);
          if FTask.CancelAsked then
          begin
            FServer.FLock.Enter;
            try
              FTask.Status := mtsCancelled;
              FTask.LastUpdatedAt := RALMCPNow;
            finally
              FServer.FLock.Leave;
            end;
            vDone := True;
          end
          else if vCall.InputRequired then
          begin
            // The task waits for tasks/update, then runs the tool again with
            // the answers and its state, as a retried call would.
            FServer.FLock.Enter;
            try
              FTask.Status := mtsInputRequired;
              if vCall.FInputRequests <> nil then
                FTask.InputRequests := vCall.FInputRequests.ToJSON
              else
                FTask.InputRequests := '{}';
              FTask.LastUpdatedAt := RALMCPNow;
              vState := vCall.FNewState;
            finally
              FServer.FLock.Leave;
            end;
            FServer.Publish('task', 'notifications/tasks', FTask.Snapshot(FServer.TaskTTL,
              FServer.TaskPollInterval), '', FTask.TaskId);
            while (FTask.InputResponses = nil) and not FTask.CancelAsked and not Terminated do
              FTask.Wake.WaitFor(1000);
            FServer.FLock.Enter;
            try
              FreeAndNil(vResponses);
              vResponses := FTask.InputResponses;
              FTask.InputResponses := nil;
              FTask.InputRequests := '';
              if not FTask.CancelAsked then
                FTask.Status := mtsWorking;
              FTask.LastUpdatedAt := RALMCPNow;
            finally
              FServer.FLock.Leave;
            end;
          end
          else
          begin
            vAnswer := TRALJSONObject.Create;
            try
              vAnswer.Add('resultType', 'complete');
              vAnswer.Add('content', Clone(vResult.Content));
              if vResult.Structured <> nil then
                vAnswer.Add('structuredContent', Clone(vResult.Structured));
              vAnswer.Add('isError', vResult.IsError);
              FServer.FLock.Enter;
              try
                FTask.Result := vAnswer.ToJSON;
                FTask.Status := mtsCompleted;
                FTask.LastUpdatedAt := RALMCPNow;
              finally
                FServer.FLock.Leave;
              end;
            finally
              vAnswer.Free;
            end;
            vDone := True;
          end;
        except
          on E: Exception do
          begin
            FServer.FLock.Enter;
            try
              FTask.Status := mtsFailed;
              FTask.StatusMessage := StringRAL(UTF8Encode(E.Message));
              FTask.Error := '{"code":-32603,"message":' +
                Quote(StringRAL(UTF8Encode(E.Message))) + '}';
              FTask.LastUpdatedAt := RALMCPNow;
            finally
              FServer.FLock.Leave;
            end;
            vDone := True;
          end;
        end;
      finally
        vCall.FInputResponses := nil;
        vResult.Free;
        vCall.Free;
      end;
      if vDone then
        FServer.Publish('task', 'notifications/tasks', FTask.Snapshot(FServer.TaskTTL,
          FServer.TaskPollInterval), '', FTask.TaskId);
    end;
  finally
    vResponses.Free;
    vChannel.Free;
  end;
end;

{ TRALMCPServer }

constructor TRALMCPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FCalls := TList.Create;
  FSubscriptions := TList.Create;
  FTasks := TList.Create;
  FTools := TRALMCPTools.Create(Self, TRALMCPTool);
  FResources := TRALMCPResources.Create(Self, TRALMCPResource);
  FResourceTemplates := TRALMCPResourceTemplates.Create(Self, TRALMCPResourceTemplate);
  FPrompts := TRALMCPPrompts.Create(Self, TRALMCPPrompt);
  FServerName := 'PascalRAL';
  FServerVersion := '2.0';
  FStateLife := 600;
  FTaskTTL := 3600000;
  FTaskPollInterval := 1000;
end;

destructor TRALMCPServer.Destroy;
var
  vInt: IntegerRAL;
  vWait: IntegerRAL;
begin
  StopListens;
  // Listens and task threads see FStopping and leave; give them a moment.
  vWait := 0;
  while (vWait < 100) and (FSubscriptions.Count > 0) do
  begin
    Sleep(20);
    Inc(vWait);
  end;
  FLock.Enter;
  try
    for vInt := 0 to FTasks.Count - 1 do
    begin
      TRALMCPTaskItem(FTasks[vInt]).CancelAsked := True;
      TRALMCPTaskItem(FTasks[vInt]).Wake.SetEvent;
    end;
  finally
    FLock.Leave;
  end;
  vWait := 0;
  while vWait < 50 do
  begin
    FLock.Enter;
    try
      vInt := 0;
      while (vInt < FTasks.Count) and
        (TRALMCPTaskItem(FTasks[vInt]).Status in [mtsCompleted, mtsFailed, mtsCancelled]) do
        Inc(vInt);
      if vInt >= FTasks.Count then
        Break;
    finally
      FLock.Leave;
    end;
    Sleep(20);
    Inc(vWait);
  end;
  for vInt := 0 to FTasks.Count - 1 do
    TObject(FTasks[vInt]).Free;
  FTasks.Free;
  FSubscriptions.Free;
  FCalls.Free;
  FTools.Free;
  FResources.Free;
  FResourceTemplates.Free;
  FPrompts.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALMCPServer.CancelCall(AGroup: TObject; const AId: StringRAL);
var
  vInt: IntegerRAL;
  vSub: TRALMCPSubscription;
begin
  FLock.Enter;
  try
    for vInt := 0 to FCalls.Count - 1 do
    begin
      vSub := TRALMCPSubscription(FCalls[vInt]);
      if (vSub.Id = AId) and (vSub.Channel.Request = AGroup) then
        vSub.Channel.Cancel;
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALMCPServer.DoComplete(ACall: TRALMCPCall): TRALJSONObject;
var
  vRef, vArgument: TRALJSON;
  vRefType, vRefName: StringRAL;
  vValues: TStringList;
  vHasMore: Boolean;
  vList: TRALJSONArray;
  vCompletion: TRALJSONObject;
  vInt: IntegerRAL;
begin
  if not Assigned(FOnComplete) then
    Refuse(cRALJSONRPCMethodNotFound, 'Method not found', '', 404);
  vRef := ACall.Params.Find('ref');
  vArgument := ACall.Params.Find('argument');
  vRefType := StrOf(vRef, 'type');
  if vRefType = 'ref/prompt' then
    vRefName := StrOf(vRef, 'name')
  else if vRefType = 'ref/resource' then
    vRefName := StrOf(vRef, 'uri')
  else
    Refuse(cRALJSONRPCInvalidParams, 'Invalid params');
  if (vArgument = nil) or (StrOf(vArgument, 'name') = '') then
    Refuse(cRALJSONRPCInvalidParams, 'Invalid params');
  vValues := TStringList.Create;
  try
    vHasMore := False;
    FOnComplete(Self, ACall, vRefType, vRefName, StrOf(vArgument, 'name'),
      StrOf(vArgument, 'value'), vValues, vHasMore);
    vList := TRALJSONArray.Create;
    for vInt := 0 to vValues.Count - 1 do
      if vInt < 100 then
        vList.Add(StringRAL(vValues[vInt]));
    vCompletion := TRALJSONObject.Create;
    vCompletion.Add('values', vList);
    vCompletion.Add('total', Int64(vValues.Count));
    vCompletion.Add('hasMore', vHasMore or (vValues.Count > 100));
    Result := TRALJSONObject.Create;
    Result.Add('completion', vCompletion);
  finally
    vValues.Free;
  end;
end;

function TRALMCPServer.DoDiscover(ACall: TRALMCPCall): TRALJSONObject;
var
  vVersions: TRALJSONArray;
  vCaps, vExt, vItem: TRALJSONObject;
  vInt: IntegerRAL;
  vTasks: Boolean;
begin
  Result := TRALJSONObject.Create;
  vVersions := TRALJSONArray.Create;
  vVersions.Add(StringRAL(cRALMCPProtocolVersion));
  Result.Add('supportedVersions', vVersions);
  vCaps := TRALJSONObject.Create;
  if FTools.Count > 0 then
  begin
    vItem := TRALJSONObject.Create;
    vItem.Add('listChanged', True);
    vCaps.Add('tools', vItem);
  end;
  if (FResources.Count > 0) or (FResourceTemplates.Count > 0) then
  begin
    vItem := TRALJSONObject.Create;
    vItem.Add('subscribe', True);
    vItem.Add('listChanged', True);
    vCaps.Add('resources', vItem);
  end;
  if FPrompts.Count > 0 then
  begin
    vItem := TRALJSONObject.Create;
    vItem.Add('listChanged', True);
    vCaps.Add('prompts', vItem);
  end;
  if Assigned(FOnComplete) then
    vCaps.Add('completions', TRALJSONObject.Create);
  vCaps.Add('logging', TRALJSONObject.Create);
  vTasks := False;
  for vInt := 0 to FTools.Count - 1 do
    vTasks := vTasks or FTools[vInt].RunAsTask;
  if vTasks then
  begin
    vExt := TRALJSONObject.Create;
    vExt.Add(cRALMCPTasksExtension, TRALJSONObject.Create);
    vCaps.Add('extensions', vExt);
  end;
  Result.Add('capabilities', vCaps);
  if FInstructions <> '' then
    Result.Add('instructions', FInstructions);
end;

function TRALMCPServer.DoGetPrompt(ACall: TRALMCPCall): TRALJSONObject;
var
  vPrompt: TRALMCPPrompt;
  vResult: TRALMCPPromptResult;
  vInt: IntegerRAL;
begin
  vPrompt := FPrompts.Find(StrOf(ACall.Params, 'name'));
  if vPrompt = nil then
    Refuse(cRALJSONRPCInvalidParams, 'Unknown prompt: ' + string(StrOf(ACall.Params, 'name')));
  for vInt := 0 to vPrompt.Arguments.Count - 1 do
    if vPrompt.Arguments[vInt].Required and (ACall.Argument(vPrompt.Arguments[vInt].Name) = nil) then
      Refuse(cRALJSONRPCInvalidParams, 'Missing required argument: ' +
        string(vPrompt.Arguments[vInt].Name));
  vResult := TRALMCPPromptResult.Create;
  try
    if Assigned(vPrompt.OnGet) then
      vPrompt.OnGet(Self, ACall, vResult);
    if ACall.InputRequired then
    begin
      Result := InputRequiredResult(ACall);
      Exit;
    end;
    Result := TRALJSONObject.Create;
    if vResult.Description <> '' then
      Result.Add('description', vResult.Description);
    Result.Add('messages', Clone(vResult.Messages));
  finally
    vResult.Free;
  end;
end;

function TRALMCPServer.DoList(ACall: TRALMCPCall): TRALJSONObject;
var
  vCursor: StringRAL;
  vStart, vCount, vInt, vAdded: IntegerRAL;
  vList: TRALJSONArray;
  vName: StringRAL;
  vItem: TRALJSONObject;
begin
  vStart := 0;
  vCursor := StrOf(ACall.Params, 'cursor');
  if vCursor <> '' then
  begin
    vCursor := UnB64Url(vCursor);
    if Copy(vCursor, 1, 2) = 'p:' then
      vStart := StrToIntDef(string(Copy(vCursor, 3, Length(vCursor))), -1)
    else
      vStart := -1;
    if vStart < 0 then
      Refuse(cRALJSONRPCInvalidParams, 'Invalid cursor');
  end;
  if ACall.Method = 'tools/list' then
  begin
    vName := 'tools';
    vCount := FTools.Count;
  end
  else if ACall.Method = 'resources/list' then
  begin
    vName := 'resources';
    vCount := FResources.Count;
  end
  else if ACall.Method = 'resources/templates/list' then
  begin
    vName := 'resourceTemplates';
    vCount := FResourceTemplates.Count;
  end
  else
  begin
    vName := 'prompts';
    vCount := FPrompts.Count;
  end;
  vList := TRALJSONArray.Create;
  Result := TRALJSONObject.Create;
  Result.Add(vName, vList);
  vAdded := 0;
  vInt := vStart;
  while (vInt < vCount) and ((FPageSize <= 0) or (vAdded < FPageSize)) do
  begin
    vItem := nil;
    if vName = 'tools' then
    begin
      if MayUse(ACall, FTools[vInt].Roles) then
        vItem := FTools[vInt].Definition;
    end
    else if vName = 'resources' then
      vItem := FResources[vInt].Definition
    else if vName = 'resourceTemplates' then
      vItem := FResourceTemplates[vInt].Definition
    else
      vItem := FPrompts[vInt].Definition;
    if vItem <> nil then
    begin
      vList.Add(vItem);
      Inc(vAdded);
    end;
    Inc(vInt);
  end;
  if vInt < vCount then
    Result.Add('nextCursor', B64UrlText(StringRAL('p:' + IntToStr(vInt))));
end;

function TRALMCPServer.DoListen(ACall: TRALMCPCall; AId: TRALJSON): TRALJSONObject;
var
  vFilter, vAccepted, vMessage, vParams, vMeta: TRALJSONObject;
  vSub: TRALMCPSubscription;
  vList: TRALJSON;
  vArray: TRALJSONArray;
  vInt: IntegerRAL;
begin
  if not ACall.Channel.Streaming then
    Refuse(cRALJSONRPCInternalError, 'subscriptions/listen needs a live stream');
  if (ACall.Params.Find('notifications') = nil) or
    (ACall.Params.Find('notifications').Kind <> rjkObject) then
    Refuse(cRALJSONRPCInvalidParams, 'Invalid params', '', 400);
  vFilter := TRALJSONObject(ACall.Params.Find('notifications'));
  vSub := TRALMCPSubscription.Create;
  vSub.Channel := ACall.Channel;
  vSub.Id := AId.ToJSON;
  vAccepted := TRALJSONObject.Create;
  try
    if (vFilter.Find('toolsListChanged') <> nil) and vFilter.Find('toolsListChanged').AsBoolean then
    begin
      vSub.Tools := True;
      vAccepted.Add('toolsListChanged', True);
    end;
    if (vFilter.Find('promptsListChanged') <> nil) and vFilter.Find('promptsListChanged').AsBoolean then
    begin
      vSub.Prompts := True;
      vAccepted.Add('promptsListChanged', True);
    end;
    if (vFilter.Find('resourcesListChanged') <> nil) and
      vFilter.Find('resourcesListChanged').AsBoolean then
    begin
      vSub.Resources := True;
      vAccepted.Add('resourcesListChanged', True);
    end;
    vList := vFilter.Find('resourceSubscriptions');
    if (vList <> nil) and (vList.Kind = rjkArray) then
    begin
      vArray := TRALJSONArray.Create;
      for vInt := 0 to TRALJSONArray(vList).Count - 1 do
      begin
        vSub.ResourceURIs.Add(string(TRALJSONArray(vList).Items[vInt].AsString));
        vArray.Add(TRALJSONArray(vList).Items[vInt].AsString);
      end;
      vAccepted.Add('resourceSubscriptions', vArray);
    end;
    vList := vFilter.Find('taskIds');
    if (vList <> nil) and (vList.Kind = rjkArray) then
    begin
      if not ACall.HasCapability('extensions.' + cRALMCPTasksExtension) then
        Refuse(cRALMCPMissingCapability, 'Missing required client capability',
          '{"requiredCapabilities":{"extensions":{"' + cRALMCPTasksExtension + '":{}}}}', 400);
      vArray := TRALJSONArray.Create;
      for vInt := 0 to TRALJSONArray(vList).Count - 1 do
      begin
        vSub.TaskIds.Add(string(TRALJSONArray(vList).Items[vInt].AsString));
        vArray.Add(TRALJSONArray(vList).Items[vInt].AsString);
      end;
      vAccepted.Add('taskIds', vArray);
    end;
    // The acknowledgement goes first, with the subscription id.
    vMessage := TRALJSONObject.Create;
    try
      vMessage.Add('jsonrpc', '2.0');
      vMessage.Add('method', 'notifications/subscriptions/acknowledged');
      vParams := TRALJSONObject.Create;
      vMeta := TRALJSONObject.Create;
      vMeta.Add(cRALMCPMetaSubscription, Clone(AId));
      vParams.Add('_meta', vMeta);
      vParams.Add('notifications', vAccepted);
      vAccepted := nil;
      vMessage.Add('params', vParams);
      ACall.Channel.Notify(vMessage.ToJSON);
    finally
      vMessage.Free;
    end;
  except
    vAccepted.Free;
    vSub.Free;
    raise;
  end;
  FLock.Enter;
  try
    FSubscriptions.Add(vSub);
  finally
    FLock.Leave;
  end;
  try
    while not ACall.Channel.Cancelled and not FStopping do
      Sleep(50);
  finally
    FLock.Enter;
    try
      FSubscriptions.Remove(vSub);
    finally
      FLock.Leave;
    end;
    vSub.Free;
  end;
  Result := TRALJSONObject.Create;
  vMeta := TRALJSONObject.Create;
  vMeta.Add(cRALMCPMetaSubscription, Clone(AId));
  Result.Add('_meta', vMeta);
end;

function TRALMCPServer.DoReadResource(ACall: TRALMCPCall): TRALJSONObject;
var
  vURI: StringRAL;
  vResource: TRALMCPResource;
  vContents: TRALMCPResourceContents;
  vVariables: TStringList;
  vInt: IntegerRAL;
  vFound: Boolean;
begin
  vURI := StrOf(ACall.Params, 'uri');
  if vURI = '' then
    Refuse(cRALJSONRPCInvalidParams, 'Invalid params');
  vContents := TRALMCPResourceContents.Create(vURI);
  vVariables := TStringList.Create;
  try
    vFound := False;
    vResource := FResources.Find(vURI);
    if vResource <> nil then
    begin
      vFound := True;
      if Assigned(vResource.OnRead) then
        vResource.OnRead(Self, ACall, vContents);
    end
    else
      for vInt := 0 to FResourceTemplates.Count - 1 do
        if FResourceTemplates[vInt].Match(vURI, vVariables) then
        begin
          vFound := True;
          if Assigned(FResourceTemplates[vInt].OnRead) then
            FResourceTemplates[vInt].OnRead(Self, ACall, vVariables, vContents);
          Break;
        end;
    if ACall.InputRequired then
    begin
      Result := InputRequiredResult(ACall);
      Exit;
    end;
    // A resource that does not exist is -32602, never empty contents.
    if (not vFound) or (vContents.Contents.Count = 0) then
      Refuse(cRALJSONRPCInvalidParams, 'Resource not found', '{"uri":' + Quote(vURI) + '}');
    Result := TRALJSONObject.Create;
    Result.Add('contents', Clone(vContents.Contents));
  finally
    vVariables.Free;
    vContents.Free;
  end;
end;

function TRALMCPServer.DoTask(ACall: TRALMCPCall): TRALJSONObject;
var
  vId: StringRAL;
  vInt: IntegerRAL;
  vTask: TRALMCPTaskItem;
  vResponses: TRALJSON;
  vStored: RawByteString;
begin
  if not ACall.HasCapability('extensions.' + cRALMCPTasksExtension) then
    Refuse(cRALMCPMissingCapability, 'Missing required client capability',
      '{"requiredCapabilities":{"extensions":{"' + cRALMCPTasksExtension + '":{}}}}', 400);
  vId := StrOf(ACall.Params, 'taskId');
  FLock.Enter;
  try
    // Tasks past their TTL go.
    for vInt := FTasks.Count - 1 downto 0 do
    begin
      vTask := TRALMCPTaskItem(FTasks[vInt]);
      if (vTask.Status in [mtsCompleted, mtsFailed, mtsCancelled]) and
        (Ticks - vTask.CreatedTick > FTaskTTL) then
      begin
        FTasks.Delete(vInt);
        vTask.Free;
      end;
    end;
    vTask := nil;
    for vInt := 0 to FTasks.Count - 1 do
      if TRALMCPTaskItem(FTasks[vInt]).TaskId = vId then
        vTask := TRALMCPTaskItem(FTasks[vInt]);
    if vTask = nil then
    begin
      if (ACall.Method = 'tasks/get') and (FTaskStore <> nil) and
        FTaskStore.Get('mcp.task:' + vId, vStored) then
      begin
        Result := TRALJSONObject(TRALJSON.ParseJSON(StringRAL(vStored)));
        Exit;
      end;
      Refuse(cRALJSONRPCInvalidParams, 'Failed to retrieve task: Task not found');
    end;
    if ACall.Method = 'tasks/get' then
      Result := vTask.Snapshot(FTaskTTL, FTaskPollInterval)
    else if ACall.Method = 'tasks/update' then
    begin
      vResponses := ACall.Params.Find('inputResponses');
      if (vResponses = nil) or (vResponses.Kind <> rjkObject) then
        Refuse(cRALJSONRPCInvalidParams, 'Invalid params');
      if vTask.Status = mtsInputRequired then
      begin
        vTask.InputResponses.Free;
        vTask.InputResponses := TRALJSONObject(Clone(vResponses));
        vTask.Wake.SetEvent;
      end;
      Result := TRALJSONObject.Create;
    end
    else
    begin
      vTask.CancelAsked := True;
      vTask.Wake.SetEvent;
      Result := TRALJSONObject.Create;
    end;
  finally
    FLock.Leave;
  end;
end;

function TRALMCPServer.DoToolCall(ACall: TRALMCPCall): TRALJSONObject;
var
  vTool: TRALMCPTool;
  vResult: TRALMCPToolResult;
  vInt: IntegerRAL;
  vParam: TRALMCPParam;
  vValue: TRALJSON;
  vFits: Boolean;
  vProblem: StringRAL;
  vTask: TRALMCPTaskItem;
begin
  vTool := FTools.Find(StrOf(ACall.Params, 'name'));
  if (vTool = nil) or not MayUse(ACall, vTool.Roles) then
    Refuse(cRALJSONRPCInvalidParams, 'Unknown tool: ' + string(StrOf(ACall.Params, 'name')));
  vResult := TRALMCPToolResult.Create;
  try
    // A wrong argument is an error of execution the model can read and fix.
    vProblem := '';
    if vTool.InputSchema = '' then
      for vInt := 0 to vTool.Params.Count - 1 do
      begin
        vParam := vTool.Params[vInt];
        vValue := ACall.Argument(vParam.Name);
        if (vValue = nil) or vValue.IsNull then
        begin
          if vParam.Required then
            vProblem := 'Missing required argument: ' + vParam.Name;
          Continue;
        end;
        case vParam.ParamType of
          mptString: vFits := vValue.Kind = rjkString;
          mptNumber: vFits := vValue.Kind = rjkNumber;
          mptInteger: vFits := (vValue.Kind = rjkNumber) and
            (Pos('.', string(TRALJSONNumber(vValue).Value)) = 0);
          mptBoolean: vFits := vValue.Kind = rjkBoolean;
          mptObject: vFits := vValue.Kind = rjkObject;
        else
          vFits := vValue.Kind = rjkArray;
        end;
        if vFits and (vParam.Values.Count > 0) then
          vFits := vParam.Values.IndexOf(string(vValue.AsString)) >= 0;
        if not vFits then
          vProblem := 'Invalid argument: ' + vParam.Name;
      end;
    if vProblem <> '' then
      vResult.Fail(vProblem)
    else if vTool.RunAsTask and ACall.HasCapability('extensions.' + cRALMCPTasksExtension) then
    begin
      vTask := TRALMCPTaskItem.Create;
      vTask.TaskId := NewId;
      vTask.Status := mtsWorking;
      vTask.CreatedAt := RALMCPNow;
      vTask.LastUpdatedAt := vTask.CreatedAt;
      vTask.CreatedTick := Ticks;
      FLock.Enter;
      try
        FTasks.Add(vTask);
      finally
        FLock.Leave;
      end;
      TRALMCPTaskThread.Create(Self, vTool, vTask, TRALJSONObject(Clone(ACall.Params)),
        Clone(ACall.Capabilities));
      FLock.Enter;
      try
        Result := vTask.Snapshot(FTaskTTL, FTaskPollInterval);
      finally
        FLock.Leave;
      end;
      Result.Add('resultType', 'task');
      Exit;
    end
    else if Assigned(vTool.OnCall) then
      vTool.OnCall(Self, ACall, vResult);
    if ACall.InputRequired then
    begin
      Result := InputRequiredResult(ACall);
      Exit;
    end;
    Result := TRALJSONObject.Create;
    Result.Add('content', Clone(vResult.Content));
    if vResult.Structured <> nil then
      Result.Add('structuredContent', Clone(vResult.Structured));
    Result.Add('isError', vResult.IsError);
  finally
    vResult.Free;
  end;
end;

function TRALMCPServer.Execute(const ARequest: StringRAL; AChannel: TRALMCPChannel;
  out AStatus: IntegerRAL): StringRAL;
var
  vRoot: TRALJSON;
  vObject, vParams, vMeta, vAnswer, vResult: TRALJSONObject;
  vId, vMethod, vVersion, vCaps, vLevel, vInfo, vState: TRALJSON;
  vCall: TRALMCPCall;
  vData: StringRAL;
  vCacheable: Boolean;
  vOwnedParams: Boolean;
  vRunning: TRALMCPSubscription;
begin
  AStatus := 200;
  vRunning := nil;
  vId := nil;
  try
    vRoot := TRALJSON.ParseJSON(ARequest);
  except
    AStatus := 400;
    vAnswer := RALJSONRPCErrorResponse(nil, cRALJSONRPCParseError, 'Parse error');
    try
      Result := vAnswer.ToJSON;
    finally
      vAnswer.Free;
    end;
    Exit;
  end;
  vOwnedParams := False;
  vParams := nil;
  vCall := nil;
  try
    try
      // One request or notification per message: no batches in MCP.
      if vRoot.Kind <> rjkObject then
        Refuse(cRALJSONRPCInvalidRequest, 'Invalid Request', '', 400);
      vObject := TRALJSONObject(vRoot);
      vId := vObject.Find('id');
      if (vId <> nil) and not (vId.Kind in [rjkString, rjkNumber]) then
      begin
        vId := nil;
        Refuse(cRALJSONRPCInvalidRequest, 'Invalid Request', '', 400);
      end;
      vMethod := vObject.Find('method');
      if (StrOf(vObject, 'jsonrpc') <> '2.0') or (vMethod = nil) or
        (vMethod.Kind <> rjkString) then
        Refuse(cRALJSONRPCInvalidRequest, 'Invalid Request', '', 400);
      if (vObject.Find('params') <> nil) and (vObject.Find('params').Kind = rjkObject) then
        vParams := TRALJSONObject(vObject.Find('params'))
      else if vObject.Find('params') <> nil then
        Refuse(cRALJSONRPCInvalidParams, 'Invalid params', '', 400)
      else
      begin
        vParams := TRALJSONObject.Create;
        vOwnedParams := True;
      end;

      if vId = nil then
      begin
        // Notifications: only cancellation means something to the server.
        AStatus := 202;
        Result := '';
        if (vMethod.AsString = 'notifications/cancelled') and (vParams.Find('requestId') <> nil) then
          CancelCall(AChannel.Request, vParams.Find('requestId').ToJSON);
        Exit;
      end;

      vMeta := nil;
      if (vParams.Find('_meta') <> nil) and (vParams.Find('_meta').Kind = rjkObject) then
        vMeta := TRALJSONObject(vParams.Find('_meta'));
      if vMeta = nil then
        Refuse(cRALJSONRPCInvalidParams, 'Missing _meta', '', 400);
      vVersion := vMeta.Find(cRALMCPMetaVersion);
      vCaps := vMeta.Find(cRALMCPMetaCapabilities);
      if (vVersion = nil) or (vVersion.Kind <> rjkString) or (vCaps = nil) or
        (vCaps.Kind <> rjkObject) then
        Refuse(cRALJSONRPCInvalidParams, 'Missing required _meta fields', '', 400);
      if vVersion.AsString <> cRALMCPProtocolVersion then
        Refuse(cRALMCPUnsupportedVersion, 'Unsupported protocol version',
          '{"supported":["' + cRALMCPProtocolVersion + '"],"requested":' +
          Quote(vVersion.AsString) + '}', 400);

      vCall := TRALMCPCall.Create(Self, AChannel, vMethod.AsString, vParams);
      vRunning := TRALMCPSubscription.Create;
      vRunning.Channel := AChannel;
      vRunning.Id := vId.ToJSON;
      FLock.Enter;
      try
        FCalls.Add(vRunning);
      finally
        FLock.Leave;
      end;
      vCall.FCapabilities := vCaps;
      vInfo := vMeta.Find(cRALMCPMetaClientInfo);
      vCall.FClientName := StrOf(vInfo, 'name');
      vCall.FProgressToken := vMeta.Find('progressToken');
      vLevel := vMeta.Find(cRALMCPMetaLogLevel);
      if vLevel <> nil then
      begin
        vCall.FLogLevel := LevelIndex(vLevel.AsString);
        if vCall.FLogLevel < 0 then
          Refuse(cRALJSONRPCInvalidParams, 'Invalid log level');
      end;
      vCall.FInputResponses := vParams.Find('inputResponses');
      vState := vParams.Find('requestState');
      if (vState <> nil) and (vState.Kind = rjkString) then
        vCall.FState := UnsealState(vCall, vState.AsString);

      vCacheable := False;
      if vCall.Method = 'server/discover' then
      begin
        vResult := DoDiscover(vCall);
        vCacheable := True;
      end
      else if (vCall.Method = 'tools/list') or (vCall.Method = 'resources/list') or
        (vCall.Method = 'resources/templates/list') or (vCall.Method = 'prompts/list') then
      begin
        vResult := DoList(vCall);
        vCacheable := True;
      end
      else if vCall.Method = 'tools/call' then
        vResult := DoToolCall(vCall)
      else if vCall.Method = 'resources/read' then
      begin
        vResult := DoReadResource(vCall);
        vCacheable := (vState = nil) and (vResult.Find('resultType') = nil);
      end
      else if vCall.Method = 'prompts/get' then
        vResult := DoGetPrompt(vCall)
      else if vCall.Method = 'completion/complete' then
        vResult := DoComplete(vCall)
      else if vCall.Method = 'subscriptions/listen' then
        vResult := DoListen(vCall, vId)
      else if (vCall.Method = 'tasks/get') or (vCall.Method = 'tasks/update') or
        (vCall.Method = 'tasks/cancel') then
        vResult := DoTask(vCall)
      else
      begin
        Refuse(cRALJSONRPCMethodNotFound, 'Method not found', '', 404);
        vResult := nil;
      end;
      Finish(vResult, vCacheable);
      vAnswer := TRALJSONObject.Create;
      try
        vAnswer.Add('jsonrpc', '2.0');
        vAnswer.Add('id', Clone(vId));
        vAnswer.Add('result', vResult);
        Result := vAnswer.ToJSON;
      finally
        vAnswer.Free;
      end;
    except
      on E: ERALJSONRPCError do
      begin
        if E is ERALMCPError then
          AStatus := ERALMCPError(E).HTTPStatus;
        vAnswer := RALJSONRPCErrorResponse(vId, E.Code, StringRAL(UTF8Encode(E.Message)), E.Data);
        try
          Result := vAnswer.ToJSON;
        finally
          vAnswer.Free;
        end;
      end;
      on E: Exception do
      begin
        vData := '';
        vAnswer := RALJSONRPCErrorResponse(vId, cRALJSONRPCInternalError, 'Internal error');
        try
          Result := vAnswer.ToJSON;
        finally
          vAnswer.Free;
        end;
      end;
    end;
  finally
    if vRunning <> nil then
    begin
      FLock.Enter;
      try
        FCalls.Remove(vRunning);
      finally
        FLock.Leave;
      end;
      vRunning.Free;
    end;
    vCall.Free;
    if vOwnedParams then
      vParams.Free;
    vRoot.Free;
  end;
end;

procedure TRALMCPServer.Finish(AResult: TRALJSONObject; ACacheable: Boolean);
var
  vMeta, vInfo: TRALJSONObject;
begin
  if AResult.Find('resultType') = nil then
  begin
    AResult.Add('resultType', 'complete');
    if ACacheable then
    begin
      AResult.Add('ttlMs', Int64(FCacheTTL));
      if FCacheScope = mcsPublic then
        AResult.Add('cacheScope', 'public')
      else
        AResult.Add('cacheScope', 'private');
    end;
  end;
  vInfo := TRALJSONObject.Create;
  vInfo.Add('name', FServerName);
  vInfo.Add('version', FServerVersion);
  if FServerTitle <> '' then
    vInfo.Add('title', FServerTitle);
  if FServerDescription <> '' then
    vInfo.Add('description', FServerDescription);
  if FWebsiteURL <> '' then
    vInfo.Add('websiteUrl', FWebsiteURL);
  if (AResult.Find('_meta') <> nil) and (AResult.Find('_meta').Kind = rjkObject) then
    TRALJSONObject(AResult.Find('_meta')).Add(cRALMCPMetaServerInfo, vInfo)
  else
  begin
    vMeta := TRALJSONObject.Create;
    vMeta.Add(cRALMCPMetaServerInfo, vInfo);
    AResult.Add('_meta', vMeta);
  end;
end;

function TRALMCPServer.GetStateKey: TBytes;
begin
  if FStateKey <> '' then
  begin
    SetLength(Result, Length(FStateKey));
    Move(Pointer(FStateKey)^, Result[0], Length(FStateKey));
  end
  else
    Result := vProcessKey;
end;

function TRALMCPServer.InputRequiredResult(ACall: TRALMCPCall): TRALJSONObject;
begin
  Result := TRALJSONObject.Create;
  Result.Add('resultType', 'input_required');
  if ACall.FInputRequests <> nil then
  begin
    Result.Add('inputRequests', ACall.FInputRequests);
    ACall.FInputRequests := nil;
  end;
  Result.Add('requestState', SealState(ACall, ACall.FNewState));
end;

function TRALMCPServer.MayUse(ACall: TRALMCPCall; const ARoles: StringRAL): Boolean;
begin
  Result := (ARoles = '') or ((ACall.Channel <> nil) and
    (ACall.Channel.Request is TRALRequest) and
    TRALRequest(ACall.Channel.Request).Identity.HasRoles(ARoles, False));
end;

function TRALMCPServer.Listening: Boolean;
begin
  FLock.Enter;
  try
    Result := FSubscriptions.Count > 0;
  finally
    FLock.Leave;
  end;
end;

procedure TRALMCPServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FTaskStore) then
    FTaskStore := nil;
end;

procedure TRALMCPServer.NotifyPromptsChanged;
begin
  Publish('prompts', 'notifications/prompts/list_changed', TRALJSONObject.Create);
end;

procedure TRALMCPServer.NotifyResourcesChanged;
begin
  Publish('resources', 'notifications/resources/list_changed', TRALJSONObject.Create);
end;

procedure TRALMCPServer.NotifyResourceUpdated(const AURI: StringRAL);
var
  vParams: TRALJSONObject;
begin
  vParams := TRALJSONObject.Create;
  vParams.Add('uri', AURI);
  Publish('uri', 'notifications/resources/updated', vParams, AURI);
end;

procedure TRALMCPServer.NotifyToolsChanged;
begin
  Publish('tools', 'notifications/tools/list_changed', TRALJSONObject.Create);
end;

procedure TRALMCPServer.Publish(const AFilter, AMethod: StringRAL; AParams: TRALJSONObject;
  const AURI, ATaskId: StringRAL);
var
  vInt: IntegerRAL;
  vSub: TRALMCPSubscription;
  vMessage, vParams, vMeta: TRALJSONObject;
  vWants: Boolean;
begin
  try
    if (AFilter = 'task') and (FTaskStore <> nil) then
      FTaskStore.Put('mcp.task:' + ATaskId, RawByteString(AParams.ToJSON), FTaskTTL);
    FLock.Enter;
    try
      for vInt := 0 to FSubscriptions.Count - 1 do
      begin
        vSub := TRALMCPSubscription(FSubscriptions[vInt]);
        if AFilter = 'tools' then
          vWants := vSub.Tools
        else if AFilter = 'prompts' then
          vWants := vSub.Prompts
        else if AFilter = 'resources' then
          vWants := vSub.Resources
        else if AFilter = 'uri' then
          vWants := vSub.ResourceURIs.IndexOf(string(AURI)) >= 0
        else
          vWants := vSub.TaskIds.IndexOf(string(ATaskId)) >= 0;
        if not vWants then
          Continue;
        vMessage := TRALJSONObject.Create;
        try
          vMessage.Add('jsonrpc', '2.0');
          vMessage.Add('method', AMethod);
          vParams := TRALJSONObject(Clone(AParams));
          vMeta := TRALJSONObject.Create;
          vMeta.Add(cRALMCPMetaSubscription, TRALJSON.ParseJSON(vSub.Id));
          vParams.Add('_meta', vMeta);
          vMessage.Add('params', vParams);
          vSub.Channel.Notify(vMessage.ToJSON);
        finally
          vMessage.Free;
        end;
      end;
    finally
      FLock.Leave;
    end;
  finally
    AParams.Free;
  end;
end;

function TRALMCPServer.SealState(ACall: TRALMCPCall; const AState: StringRAL): StringRAL;
var
  vPayload: StringRAL;
begin
  // payload = expiry|method|name|state; the HMAC covers it and the key.
  vPayload := StringRAL(IntToStr(DateTimeToUnix(RALDateTimeToGMT(Now)) + FStateLife)) + '|' + ACall.Method +
    '|' + StrOf(ACall.Params, 'name', StrOf(ACall.Params, 'uri')) + '|' + AState;
  Result := B64UrlText(vPayload) + '.' + B64Url(HMAC256(vPayload, GetStateKey));
end;

procedure TRALMCPServer.SetPrompts(AValue: TRALMCPPrompts);
begin
  FPrompts.Assign(AValue);
end;

procedure TRALMCPServer.SetResources(AValue: TRALMCPResources);
begin
  FResources.Assign(AValue);
end;

procedure TRALMCPServer.SetResourceTemplates(AValue: TRALMCPResourceTemplates);
begin
  FResourceTemplates.Assign(AValue);
end;

procedure TRALMCPServer.SetTaskStore(AValue: TRALKeyValueStore);
begin
  if FTaskStore = AValue then
    Exit;
  if FTaskStore <> nil then
    FTaskStore.RemoveFreeNotification(Self);
  FTaskStore := AValue;
  if FTaskStore <> nil then
    FTaskStore.FreeNotification(Self);
end;

procedure TRALMCPServer.SetTools(AValue: TRALMCPTools);
begin
  FTools.Assign(AValue);
end;

procedure TRALMCPServer.StopListens;
begin
  FStopping := True;
end;

function TRALMCPServer.UnsealState(ACall: TRALMCPCall; const ASealed: StringRAL): StringRAL;
var
  vDot, vBar: IntegerRAL;
  vPayload, vRest: StringRAL;
  vExpiry: Int64;
  vMac: TBytes;
begin
  Result := '';
  vDot := Pos('.', string(ASealed));
  if vDot = 0 then
    Refuse(cRALJSONRPCInvalidParams, 'Invalid requestState');
  vPayload := UnB64Url(Copy(ASealed, 1, vDot - 1));
  if vPayload = '' then
    Refuse(cRALJSONRPCInvalidParams, 'Invalid requestState');
  vMac := HMAC256(vPayload, GetStateKey);
  if not RALSameSecret(B64Url(vMac), Copy(ASealed, vDot + 1, Length(ASealed))) then
    Refuse(cRALJSONRPCInvalidParams, 'Invalid requestState');
  vBar := Pos('|', string(vPayload));
  vExpiry := StrToInt64Def(string(Copy(vPayload, 1, vBar - 1)), 0);
  if vExpiry < DateTimeToUnix(RALDateTimeToGMT(Now)) then
    Refuse(cRALJSONRPCInvalidParams, 'Expired requestState');
  vRest := Copy(vPayload, vBar + 1, Length(vPayload));
  // The state belongs to the same method and the same name or URI.
  vBar := Pos('|', string(vRest));
  if Copy(vRest, 1, vBar - 1) <> ACall.Method then
    Refuse(cRALJSONRPCInvalidParams, 'Invalid requestState');
  vRest := Copy(vRest, vBar + 1, Length(vRest));
  vBar := Pos('|', string(vRest));
  if Copy(vRest, 1, vBar - 1) <> StrOf(ACall.Params, 'name', StrOf(ACall.Params, 'uri')) then
    Refuse(cRALJSONRPCInvalidParams, 'Invalid requestState');
  Result := Copy(vRest, vBar + 1, Length(vRest));
end;

initialization
  vProcessKey := RandomBytes(32);

end.
