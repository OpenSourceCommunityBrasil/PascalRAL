/// Server-side templates: the engine base, the Mustache engine and the data views.
unit RALTemplate;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, TypInfo, SyncObjs, Contnrs,
  RALTypes, RALConsts, RALTools, RALMIMETypes, RALJson, RALResponse, RALExceptions,
  RALErrorCodes, RALHtml;

const
  /// TRALTemplateEngine.Extension when nobody says otherwise.
  cRALTemplateExtension = '.html';
  /// How deep partials and parents may nest before rendering stops.
  cRALTemplateMaxDepth = 100;

type
  /// What a value of the data is to a template.
  TRALTemplateKind = (rtkNull, rtkBoolean, rtkText, rtkMap, rtkList);

  TRALTemplateData = class;

  /// Walks the items of a list value.
  TRALTemplateIterator = class
  public
    /// The current item, owned by the iterator and valid until the next move.
    function Current: TRALTemplateData; virtual; abstract;
    /// Moves to the next item (the first one on the first call); False at the end.
    function MoveNext: Boolean; virtual; abstract;
  end;

  { A view over one value of the data a template renders. A view never owns
    the data it shows; Member and Iterate return new objects the caller frees. }
  TRALTemplateData = class
  public
    /// Whether a section skips the value: null, false or an empty list.
    function IsFalsey: Boolean; virtual;
    /// A new iterator over the items of a list; nil for any other kind.
    function Iterate: TRALTemplateIterator; virtual;
    /// What the value is.
    function Kind: TRALTemplateKind; virtual; abstract;
    /// A new view over the member AName; nil when the value has none.
    function Member(const AName: StringRAL): TRALTemplateData; virtual;
    /// The value as an interpolation writes it, before escaping.
    function Text: StringRAL; virtual;
  end;

  /// A plain value: null, a boolean or a text.
  TRALTemplateValue = class(TRALTemplateData)
  private
    /// The value of a boolean.
    FBoolean: Boolean;
    /// What the value is.
    FKind: TRALTemplateKind;
    /// The value of a text.
    FText: StringRAL;
  public
    /// A boolean value.
    constructor CreateBoolean(AValue: Boolean);
    /// The null value.
    constructor CreateNull;
    /// A text value.
    constructor CreateText(const AValue: StringRAL);

    /// A new value equal to this one.
    function Clone: TRALTemplateValue;
    function IsFalsey: Boolean; override;
    function Kind: TRALTemplateKind; override;
    function Text: StringRAL; override;
  end;

  { A map built in code: names bound to objects of the application (shown by
    their registered views, not owned) or to plain values. }
  TRALTemplateMap = class(TRALTemplateData)
  private
    /// The names; objects are the bound objects or owned TRALTemplateValue.
    FItems: TStringList;
    /// The plain values, owned.
    FValues: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    /// Binds AName to AObject, which must outlive the render; returns the map.
    function Add(const AName: StringRAL; AObject: TObject): TRALTemplateMap; overload;
    /// Binds AName to the text AValue; returns the map.
    function Add(const AName, AValue: StringRAL): TRALTemplateMap; overload;
    /// Binds AName to the boolean AValue; returns the map.
    function AddBoolean(const AName: StringRAL; AValue: Boolean): TRALTemplateMap;
    function Kind: TRALTemplateKind; override;
    function Member(const AName: StringRAL): TRALTemplateData; override;
  end;

  { A view over an object of the application, created by RALTemplateDataOf
    from the view class registered for the object's class. }
  TRALTemplateObjectData = class(TRALTemplateData)
  protected
    /// The object shown, not owned.
    FObject: TObject;
  public
    /// Shows AObject, which must outlive the view.
    constructor Create(AObject: TObject); virtual;
  end;

  /// Class of a view over objects.
  TRALTemplateObjectDataClass = class of TRALTemplateObjectData;

  /// A JSON value: objects are maps, arrays are lists, null and false are falsey.
  TRALTemplateJSONData = class(TRALTemplateObjectData)
  public
    function IsFalsey: Boolean; override;
    function Iterate: TRALTemplateIterator; override;
    function Kind: TRALTemplateKind; override;
    function Member(const AName: StringRAL): TRALTemplateData; override;
    function Text: StringRAL; override;
  end;

  /// A string list: a list of its lines and a map of its Name=Value pairs.
  TRALTemplateStringsData = class(TRALTemplateObjectData)
  public
    function IsFalsey: Boolean; override;
    function Iterate: TRALTemplateIterator; override;
    function Kind: TRALTemplateKind; override;
    function Member(const AName: StringRAL): TRALTemplateData; override;
    function Text: StringRAL; override;
  end;

  /// A collection: a list of its items.
  TRALTemplateCollectionData = class(TRALTemplateObjectData)
  public
    function IsFalsey: Boolean; override;
    function Iterate: TRALTemplateIterator; override;
    function Kind: TRALTemplateKind; override;
  end;

  { Any other object: a map of its published properties. An object found in a
    property is shown by the view registered for its class. }
  TRALTemplatePropertiesData = class(TRALTemplateObjectData)
  public
    function Kind: TRALTemplateKind; override;
    function Member(const AName: StringRAL): TRALTemplateData; override;
  end;

  { Applies a filter the engine does not know to AValue; set AHandled when it
    did. AArgument is the text after the colon, without its quotes. }
  TRALTemplateFilterEvent = procedure(Sender: TObject; const AFilter, AArgument: StringRAL;
    var AValue: StringRAL; var AHandled: Boolean) of object;

  { Gives the source of the template AName before the engine looks for its
    file; set AFound when ASource holds it. Runs under the engine's lock. }
  TRALTemplateLoadEvent = procedure(Sender: TObject; const AName: StringRAL;
    var ASource: StringRAL; var AFound: Boolean) of object;

  { The base of the template engines: finds templates by name (AddTemplate,
    OnLoadTemplate, then files under TemplateRoot), keeps them compiled and
    renders them into a stream or a response. Descendants give the language. }
  TRALTemplateEngine = class(TComponent)
  private
    FAllowFilters: Boolean;
    /// Compiled templates by name and indentation; objects are cache entries.
    FCache: TStringList;
    FExtension: StringRAL;
    FLayout: StringRAL;
    /// Guards FCache, FRetired and FSources.
    FLock: TCriticalSection;
    FOnFilter: TRALTemplateFilterEvent;
    FOnLoadTemplate: TRALTemplateLoadEvent;
    /// Compiled templates replaced while a render may still use them.
    FRetired: TObjectList;
    /// Sources given by AddTemplate, by name; objects are owned source holders.
    FSources: TStringList;
    FStreamed: Boolean;
    FTemplateRoot: StringRAL;
    FWatchFiles: Boolean;
  protected
    /// Compiles ASource, called AName in error messages; the engine owns the result.
    function Compile(const AName, ASource: StringRAL): TObject; virtual; abstract;
    { Writes ACompiled with AData (the last item is searched first) to AStream;
      ALayout is the compiled layout the page goes into, or nil. }
    procedure Execute(ACompiled, ALayout: TObject; const AData: array of TObject;
      AStream: TStream); virtual; abstract;
    /// The file of the template AName, or '' when the name is refused.
    function FileOf(const AName: StringRAL): StringRAL;
    /// Retires every compiled variant of AName; the caller holds the lock.
    procedure Forget(const AName: StringRAL);
    { The source of AName: AddTemplate, OnLoadTemplate or its file (AFileName
      is then set); False when there is none. The caller holds the lock. }
    function LoadSource(const AName: StringRAL; out ASource, AFileName: StringRAL): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Adds or replaces the template AName from source text; it wins over files.
    procedure AddTemplate(const AName, ASource: StringRAL);
    /// AValue after the filter AFilter (built-in or OnFilter); unknown ones keep it.
    function ApplyFilter(const AFilter, AArgument, AValue: StringRAL): StringRAL; virtual;
    { The compiled template AName with AIndent put before each of its lines, or
      nil when there is no such template. Safe from any thread. }
    function Find(const AName: StringRAL; const AIndent: StringRAL = ''): TObject;
    /// Drops every compiled template, so sources are read again on the next render.
    procedure Reload;
    /// Writes the template AName with AData to AStream, inside Layout when AUseLayout.
    procedure Render(const AName: StringRAL; const AData: array of TObject;
      AStream: TStream; AUseLayout: Boolean = False); overload;
    { Answers AResponse with the template AName as HTML, inside Layout when
      AUseLayout; sent in pieces as it renders when Streamed. }
    procedure Render(AResponse: TRALResponse; const AName: StringRAL;
      const AData: array of TObject; AStatusCode: IntegerRAL = 200;
      AUseLayout: Boolean = True); overload;
    /// The source text ASource rendered with AData; nothing is cached.
    function RenderSource(const ASource: StringRAL; const AData: array of TObject): StringRAL;
    /// The template AName rendered with AData, without the layout.
    function RenderText(const AName: StringRAL; const AData: array of TObject): StringRAL;
  published
    /// Reads filters in variable tags (a value, a bar, a filter, a colon, its argument).
    property AllowFilters: Boolean read FAllowFilters write FAllowFilters default False;
    /// Added to a template name without an extension to find its file.
    property Extension: StringRAL read FExtension write FExtension;
    { The template every page rendered for a response goes into, in its block
      named content; empty = no layout. }
    property Layout: StringRAL read FLayout write FLayout;
    /// Applies the filters the engine does not know.
    property OnFilter: TRALTemplateFilterEvent read FOnFilter write FOnFilter;
    /// Gives template sources from anywhere: resources, a database.
    property OnLoadTemplate: TRALTemplateLoadEvent read FOnLoadTemplate write FOnLoadTemplate;
    /// Sends a page to the client in pieces while it renders, instead of whole.
    property Streamed: Boolean read FStreamed write FStreamed default False;
    { The folder template files are read from; relative to the executable when
      not absolute. Empty = no files, only AddTemplate and OnLoadTemplate. }
    property TemplateRoot: StringRAL read FTemplateRoot write FTemplateRoot;
    /// Reads a template file again when its date changes; off spares a check per render.
    property WatchFiles: Boolean read FWatchFiles write FWatchFiles default True;
  end;

  { Mustache (mustache.github.io): escaped and raw variables, sections,
    inverted sections, comments, partials, dynamic partial names, delimiter
    changes and inheritance with parents and blocks. Lambdas are not run. }
  TRALMustacheEngine = class(TRALTemplateEngine)
  protected
    function Compile(const AName, ASource: StringRAL): TObject; override;
    procedure Execute(ACompiled, ALayout: TObject; const AData: array of TObject;
      AStream: TStream); override;
  end;

/// A new view over AObject (nil gives null), by the view class registered for its class.
function RALTemplateDataOf(AObject: TObject): TRALTemplateData;
/// Makes AView the view of AClass and its descendants without a closer registration.
procedure RALRegisterTemplateData(AClass: TClass; AView: TRALTemplateObjectDataClass);
/// Removes the view registered for AClass.
procedure RALUnregisterTemplateData(AClass: TClass);

implementation

const
  /// Size of the buffer between a render and its stream.
  cWriterBuffer = 16384;

type
  /// A registered view class.
  TRALTemplateRegistration = class
  public
    /// The class shown.
    DataClass: TClass;
    /// The view that shows it.
    ViewClass: TRALTemplateObjectDataClass;
  end;

  /// The source of a template given by TRALTemplateEngine.AddTemplate.
  TRALTemplateSource = class
  public
    /// The source text.
    Source: StringRAL;
  end;

  /// A compiled template kept by the engine.
  TRALTemplateEntry = class
  public
    /// The compiled template, owned.
    Compiled: TObject;
    /// The date of FileName when it was read.
    FileDate: TDateTime;
    /// The file the source came from; empty for other sources.
    FileName: StringRAL;

    destructor Destroy; override;
  end;

  /// Iterator over a list read by position.
  TRALTemplateIndexIterator = class(TRALTemplateIterator)
  private
    /// The current item, owned.
    FCurrent: TRALTemplateData;
    /// The position of the current item; -1 before the first.
    FIndex: IntegerRAL;
    /// The object listed.
    FList: TObject;
  public
    /// An iterator over AList, before its first item.
    constructor Create(AList: TObject);
    destructor Destroy; override;

    /// How many items the list has.
    function Count: IntegerRAL; virtual; abstract;
    function Current: TRALTemplateData; override;
    /// A new view over the item at AIndex.
    function ItemAt(AIndex: IntegerRAL): TRALTemplateData; virtual; abstract;
    function MoveNext: Boolean; override;
  end;

  /// Iterator over a JSON array.
  TRALTemplateJSONIterator = class(TRALTemplateIndexIterator)
  public
    function Count: IntegerRAL; override;
    function ItemAt(AIndex: IntegerRAL): TRALTemplateData; override;
  end;

  /// Iterator over the lines of a string list.
  TRALTemplateStringsIterator = class(TRALTemplateIndexIterator)
  public
    function Count: IntegerRAL; override;
    function ItemAt(AIndex: IntegerRAL): TRALTemplateData; override;
  end;

  /// Iterator over the items of a collection.
  TRALTemplateCollectionIterator = class(TRALTemplateIndexIterator)
  public
    function Count: IntegerRAL; override;
    function ItemAt(AIndex: IntegerRAL): TRALTemplateData; override;
  end;

  /// Buffers what a render writes and passes it to a stream in blocks.
  TRALTemplateWriter = class
  private
    /// The pending bytes.
    FBuffer: StringRAL;
    /// How many bytes of FBuffer are pending.
    FCount: IntegerRAL;
    /// Where the bytes go.
    FStream: TStream;
  public
    /// A writer into AStream.
    constructor Create(AStream: TStream);

    /// Writes the pending bytes to the stream.
    procedure Flush;
    /// Appends AText.
    procedure Write(const AText: StringRAL);
  end;

  /// A stream whose writes are sent to the client as pieces of a response.
  TRALTemplateResponseStream = class(TStream)
  private
    /// The response written to.
    FResponse: TRALResponse;
  public
    /// A stream that sends to AResponse.
    constructor Create(AResponse: TRALResponse);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  /// The kinds of node of a compiled Mustache template.
  TRALMustacheKind = (mkText, mkVariable, mkRaw, mkSection, mkInverted, mkPartial,
    mkParent, mkBlock);

  /// A dotted name split at its dots; a single '.' is the current item.
  TRALMustachePath = array of StringRAL;

  /// One node of a compiled Mustache template.
  TRALMustacheNode = class
  public
    /// The nodes inside a section, inverted section, parent or block; owned.
    Children: TObjectList;
    /// A partial whose name is read from the data.
    Dynamic: Boolean;
    /// Filters of a variable as Name=Argument, in order; nil when none.
    Filters: TStringList;
    /// What a standalone partial or parent puts before each line.
    Indent: StringRAL;
    /// What the node is.
    Kind: TRALMustacheKind;
    /// The name split at its dots.
    Path: TRALMustachePath;
    /// The text of a text node, or the name of a tag.
    Text: StringRAL;

    /// A node of AKind with the text or name AText.
    constructor Create(AKind: TRALMustacheKind; const AText: StringRAL);
    destructor Destroy; override;
  end;

  /// One piece of Mustache source: text up to and with a line break, or a tag.
  TRALMustacheToken = class
  public
    /// Whether the token begins a line of the output.
    AtLineStart: Boolean;
    /// What a block takes off the start of the lines of its content.
    BlockIndent: StringRAL;
    /// What a standalone line had before its first tag.
    Indent: StringRAL;
    /// Where the tag starts in the source, for error messages.
    Pos: IntegerRAL;
    /// Whether a standalone line dropped the text.
    Removed: Boolean;
    /// The tag sigil; #0 for a variable, #1 for text.
    Sigil: AnsiChar;
    /// Whether the tag is on a line holding only tags and blanks.
    Standalone: Boolean;
    /// The text, or the content of the tag.
    Text: StringRAL;
  end;

  /// Turns Mustache source into nodes.
  TRALMustacheParser = class
  private
    /// The current close delimiter.
    FClose: StringRAL;
    /// Reads filters in variable tags.
    FFilters: Boolean;
    /// The template name, for error messages.
    FName: StringRAL;
    /// The current open delimiter.
    FOpen: StringRAL;
    /// The source.
    FSource: StringRAL;
    /// The tokens of the source, owned.
    FTokens: TObjectList;

    /// Adds AText as a text node of AList, when it is not empty.
    procedure AddText(AList: TObjectList; const AText: StringRAL);
    /// Adds the text from AFrom to ATo as tokens, one per line.
    procedure AddTextTokens(AFrom, ATo: IntegerRAL);
    /// Builds the nodes of the kept tokens into ARoot.
    procedure Build(ARoot: TObjectList);
    { Takes the indentation of the first line of each block off the lines of
      its content (Mustache spec, block reindentation). }
    procedure Dedent;
    /// The line of the byte at APos, from 1.
    function LineOf(APos: IntegerRAL): IntegerRAL;
    /// Marks the lines holding only blanks and standalone tags.
    procedure MarkStandalone;
    /// Reads the filters of a variable into ANode.
    procedure ReadFilters(ANode: TRALMustacheNode; const AContent: StringRAL);
    /// Splits the source into tokens, following delimiter changes.
    procedure Tokenize;
  public
    /// A parser of ASource, called AName in errors, reading filters when AFilters.
    constructor Create(const AName, ASource: StringRAL; AFilters: Boolean);
    destructor Destroy; override;

    /// Parses the source into ARoot.
    procedure Parse(ARoot: TObjectList);
  end;

  /// One render of a Mustache template: the context stack and the blocks in force.
  TRALMustacheRenderer = class
  private
    /// How many partials and parents are open.
    FDepth: IntegerRAL;
    /// The engine, which finds partials and applies filters.
    FEngine: TRALTemplateEngine;
    /// Lists of block overrides in force, the outermost first.
    FOverrides: TList;
    /// For each context, whether the renderer frees it.
    FOwned: TList;
    /// The context stack, the innermost last.
    FStack: TList;
    /// Where the output goes.
    FWriter: TRALTemplateWriter;

    /// The override of the block AName, outermost first; nil when none.
    function FindOverride(const AName: StringRAL): TRALMustacheNode;
    /// The value of APath; AOwned says whether the caller frees it. nil when missing.
    function Lookup(const APath: TRALMustachePath; out AOwned: Boolean): TRALTemplateData;
    /// Removes the innermost context.
    procedure Pop;
    /// Adds AData as the innermost context.
    procedure Push(AData: TRALTemplateData; AOwned: Boolean);
    /// Renders ANodes with AIndent before each line of what they write.
    procedure RenderIndented(ANodes: TObjectList; const AIndent: StringRAL);
    /// Renders the parent ANode: its template with its blocks as overrides.
    procedure RenderParent(ANode: TRALMustacheNode);
    /// Renders the partial ANode.
    procedure RenderPartial(ANode: TRALMustacheNode);
    /// Renders the section ANode.
    procedure RenderSection(ANode: TRALMustacheNode);
    /// Renders the variable ANode.
    procedure RenderVariable(ANode: TRALMustacheNode);
  public
    /// A render for AEngine into AStream.
    constructor Create(AEngine: TRALTemplateEngine; AStream: TStream);
    destructor Destroy; override;

    /// Adds the root contexts, the last one searched first.
    procedure AddContexts(const AData: array of TObject);
    /// Writes what is still buffered.
    procedure Finish;
    /// Renders ANodes in the current context.
    procedure Render(ANodes: TObjectList);
    /// Renders ALayout with APage as its block content and APage's blocks as overrides.
    procedure RenderLayout(APage, ALayout: TObjectList);
  end;

var
  /// The registered views, in registration order.
  vRegistry: TObjectList;
  /// Date formatting where '/' and ':' in a format are written as they are.
  vDateFormat: TFormatSettings;

/// AText as StringRAL from the RTL's string type.
function ToRAL(const AText: string): StringRAL;
begin
  {$IFDEF FPC}
  Result := AText;
  {$ELSE}
  Result := UTF8Encode(AText);
  {$ENDIF}
end;

/// AText from StringRAL to the RTL's string type.
function FromRAL(const AText: StringRAL): string;
begin
  {$IFDEF FPC}
  Result := AText;
  {$ELSE}
  Result := UTF8ToString(AText);
  {$ENDIF}
end;

/// The position of ASub in AText from AFrom; 0 when it is not there.
function FindFrom(const ASub, AText: StringRAL; AFrom: IntegerRAL): IntegerRAL;
var
  vLen, vMax, vInt: IntegerRAL;
begin
  Result := 0;
  vLen := Length(ASub);
  vMax := Length(AText) - vLen + 1;
  if vLen = 0 then
    Exit;
  while AFrom <= vMax do
  begin
    if AText[AFrom] = ASub[1] then
    begin
      vInt := 2;
      while (vInt <= vLen) and (AText[AFrom + vInt - 1] = ASub[vInt]) do
        Inc(vInt);
      if vInt > vLen then
      begin
        Result := AFrom;
        Exit;
      end;
    end;
    Inc(AFrom);
  end;
end;

/// AName split at its dots.
function SplitPath(const AName: StringRAL): TRALMustachePath;
var
  vInt, vStart, vCount: IntegerRAL;
begin
  Result := nil;
  if AName = '.' then
  begin
    SetLength(Result, 1);
    Result[0] := '.';
    Exit;
  end;
  vCount := 0;
  SetLength(Result, 0);
  vStart := 1;
  for vInt := 1 to Length(AName) + 1 do
    if (vInt > Length(AName)) or (AName[vInt] = '.') then
    begin
      SetLength(Result, vCount + 1);
      Result[vCount] := Copy(AName, vStart, vInt - vStart);
      Inc(vCount);
      vStart := vInt + 1;
    end;
end;

/// ASource with AIndent before its first line and after every line break but a final one.
function IndentSource(const ASource, AIndent: StringRAL): StringRAL;
var
  vInt, vStart: IntegerRAL;
begin
  if (AIndent = '') or (ASource = '') then
  begin
    Result := ASource;
    Exit;
  end;
  Result := AIndent;
  vStart := 1;
  for vInt := 1 to Length(ASource) do
    if (ASource[vInt] = #10) and (vInt < Length(ASource)) then
    begin
      Result := Result + Copy(ASource, vStart, vInt - vStart + 1) + AIndent;
      vStart := vInt + 1;
    end;
  Result := Result + Copy(ASource, vStart, Length(ASource));
end;

function RALTemplateDataOf(AObject: TObject): TRALTemplateData;
var
  vInt, vBest, vDist, vBestDist: IntegerRAL;
  vClass: TClass;
  vReg: TRALTemplateRegistration;
begin
  if AObject = nil then
  begin
    Result := TRALTemplateValue.CreateNull;
    Exit;
  end;
  vBest := -1;
  vBestDist := MaxInt;
  for vInt := 0 to vRegistry.Count - 1 do
  begin
    vReg := TRALTemplateRegistration(vRegistry[vInt]);
    if not AObject.InheritsFrom(vReg.DataClass) then
      Continue;
    vDist := 0;
    vClass := AObject.ClassType;
    while vClass <> vReg.DataClass do
    begin
      vClass := vClass.ClassParent;
      Inc(vDist);
    end;
    if vDist < vBestDist then
    begin
      vBest := vInt;
      vBestDist := vDist;
    end;
  end;
  if vBest < 0 then
    Result := TRALTemplatePropertiesData.Create(AObject)
  else
    Result := TRALTemplateRegistration(vRegistry[vBest]).ViewClass.Create(AObject);
end;

procedure RALRegisterTemplateData(AClass: TClass; AView: TRALTemplateObjectDataClass);
var
  vReg: TRALTemplateRegistration;
begin
  RALUnregisterTemplateData(AClass);
  vReg := TRALTemplateRegistration.Create;
  vReg.DataClass := AClass;
  vReg.ViewClass := AView;
  vRegistry.Add(vReg);
end;

procedure RALUnregisterTemplateData(AClass: TClass);
var
  vInt: IntegerRAL;
begin
  if vRegistry = nil then
    Exit;
  for vInt := vRegistry.Count - 1 downto 0 do
    if TRALTemplateRegistration(vRegistry[vInt]).DataClass = AClass then
      vRegistry.Delete(vInt);
end;

{ TRALTemplateData }

function TRALTemplateData.IsFalsey: Boolean;
begin
  Result := Kind = rtkNull;
end;

function TRALTemplateData.Iterate: TRALTemplateIterator;
begin
  Result := nil;
end;

function TRALTemplateData.Member(const AName: StringRAL): TRALTemplateData;
begin
  Result := nil;
end;

function TRALTemplateData.Text: StringRAL;
begin
  Result := '';
end;

{ TRALTemplateValue }

constructor TRALTemplateValue.CreateBoolean(AValue: Boolean);
begin
  inherited Create;
  FKind := rtkBoolean;
  FBoolean := AValue;
end;

constructor TRALTemplateValue.CreateNull;
begin
  inherited Create;
  FKind := rtkNull;
end;

constructor TRALTemplateValue.CreateText(const AValue: StringRAL);
begin
  inherited Create;
  FKind := rtkText;
  FText := AValue;
end;

function TRALTemplateValue.Clone: TRALTemplateValue;
begin
  case FKind of
    rtkBoolean: Result := TRALTemplateValue.CreateBoolean(FBoolean);
    rtkText: Result := TRALTemplateValue.CreateText(FText);
  else
    Result := TRALTemplateValue.CreateNull;
  end;
end;

function TRALTemplateValue.IsFalsey: Boolean;
begin
  Result := (FKind = rtkNull) or ((FKind = rtkBoolean) and not FBoolean);
end;

function TRALTemplateValue.Kind: TRALTemplateKind;
begin
  Result := FKind;
end;

function TRALTemplateValue.Text: StringRAL;
begin
  case FKind of
    rtkBoolean:
      if FBoolean then
        Result := 'true'
      else
        Result := 'false';
    rtkText:
      Result := FText;
  else
    Result := '';
  end;
end;

{ TRALTemplateMap }

constructor TRALTemplateMap.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
  FValues := TObjectList.Create(True);
end;

destructor TRALTemplateMap.Destroy;
begin
  FItems.Free;
  FValues.Free;
  inherited Destroy;
end;

function TRALTemplateMap.Add(const AName: StringRAL; AObject: TObject): TRALTemplateMap;
begin
  FItems.AddObject(FromRAL(AName), AObject);
  Result := Self;
end;

function TRALTemplateMap.Add(const AName, AValue: StringRAL): TRALTemplateMap;
var
  vValue: TRALTemplateValue;
begin
  vValue := TRALTemplateValue.CreateText(AValue);
  FValues.Add(vValue);
  FItems.AddObject(FromRAL(AName), vValue);
  Result := Self;
end;

function TRALTemplateMap.AddBoolean(const AName: StringRAL; AValue: Boolean): TRALTemplateMap;
var
  vValue: TRALTemplateValue;
begin
  vValue := TRALTemplateValue.CreateBoolean(AValue);
  FValues.Add(vValue);
  FItems.AddObject(FromRAL(AName), vValue);
  Result := Self;
end;

function TRALTemplateMap.Kind: TRALTemplateKind;
begin
  Result := rtkMap;
end;

function TRALTemplateMap.Member(const AName: StringRAL): TRALTemplateData;
var
  vIndex: IntegerRAL;
  vObject: TObject;
begin
  vIndex := FItems.IndexOf(FromRAL(AName));
  if vIndex < 0 then
  begin
    Result := nil;
    Exit;
  end;
  vObject := FItems.Objects[vIndex];
  if (vObject is TRALTemplateValue) and (FValues.IndexOf(vObject) >= 0) then
    Result := TRALTemplateValue(vObject).Clone
  else
    Result := RALTemplateDataOf(vObject);
end;

{ TRALTemplateObjectData }

constructor TRALTemplateObjectData.Create(AObject: TObject);
begin
  inherited Create;
  FObject := AObject;
end;

{ TRALTemplateJSONData }

function TRALTemplateJSONData.IsFalsey: Boolean;
var
  vJSON: TRALJSON;
begin
  vJSON := TRALJSON(FObject);
  case vJSON.Kind of
    rjkNull: Result := True;
    rjkBoolean: Result := not vJSON.AsBoolean;
    rjkArray: Result := TRALJSONArray(vJSON).Count = 0;
  else
    Result := False;
  end;
end;

function TRALTemplateJSONData.Iterate: TRALTemplateIterator;
begin
  if TRALJSON(FObject).Kind = rjkArray then
    Result := TRALTemplateJSONIterator.Create(FObject)
  else
    Result := nil;
end;

function TRALTemplateJSONData.Kind: TRALTemplateKind;
begin
  case TRALJSON(FObject).Kind of
    rjkObject: Result := rtkMap;
    rjkArray: Result := rtkList;
    rjkBoolean: Result := rtkBoolean;
    rjkNull: Result := rtkNull;
  else
    Result := rtkText;
  end;
end;

function TRALTemplateJSONData.Member(const AName: StringRAL): TRALTemplateData;
var
  vChild: TRALJSON;
begin
  Result := nil;
  if TRALJSON(FObject).Kind <> rjkObject then
    Exit;
  vChild := TRALJSONObject(FObject).Find(AName);
  if vChild <> nil then
    Result := TRALTemplateJSONData.Create(vChild);
end;

function TRALTemplateJSONData.Text: StringRAL;
var
  vJSON: TRALJSON;
begin
  vJSON := TRALJSON(FObject);
  case vJSON.Kind of
    rjkString: Result := TRALJSONString(vJSON).Value;
    rjkNumber: Result := TRALJSONNumber(vJSON).Value;
    rjkBoolean:
      if vJSON.AsBoolean then
        Result := 'true'
      else
        Result := 'false';
    rjkNull: Result := '';
  else
    Result := vJSON.ToJSON;
  end;
end;

{ TRALTemplateStringsData }

function TRALTemplateStringsData.IsFalsey: Boolean;
begin
  Result := TStrings(FObject).Count = 0;
end;

function TRALTemplateStringsData.Iterate: TRALTemplateIterator;
begin
  Result := TRALTemplateStringsIterator.Create(FObject);
end;

function TRALTemplateStringsData.Kind: TRALTemplateKind;
begin
  Result := rtkList;
end;

function TRALTemplateStringsData.Member(const AName: StringRAL): TRALTemplateData;
var
  vIndex: IntegerRAL;
begin
  vIndex := TStrings(FObject).IndexOfName(FromRAL(AName));
  if vIndex < 0 then
    Result := nil
  else
    Result := TRALTemplateValue.CreateText(ToRAL(TStrings(FObject).ValueFromIndex[vIndex]));
end;

function TRALTemplateStringsData.Text: StringRAL;
begin
  Result := ToRAL(TStrings(FObject).Text);
end;

{ TRALTemplateCollectionData }

function TRALTemplateCollectionData.IsFalsey: Boolean;
begin
  Result := TCollection(FObject).Count = 0;
end;

function TRALTemplateCollectionData.Iterate: TRALTemplateIterator;
begin
  Result := TRALTemplateCollectionIterator.Create(FObject);
end;

function TRALTemplateCollectionData.Kind: TRALTemplateKind;
begin
  Result := rtkList;
end;

{ TRALTemplatePropertiesData }

function TRALTemplatePropertiesData.Kind: TRALTemplateKind;
begin
  Result := rtkMap;
end;

function TRALTemplatePropertiesData.Member(const AName: StringRAL): TRALTemplateData;
var
  vProp: PPropInfo;
  vType: PTypeInfo;
  vFloat: Extended;
begin
  Result := nil;
  if FObject.ClassInfo = nil then
    Exit;
  vProp := GetPropInfo(FObject, FromRAL(AName));
  if vProp = nil then
    Exit;
  vType := vProp^.PropType{$IFNDEF FPC}^{$ENDIF};
  case vType^.Kind of
    tkInteger, tkChar, tkWChar:
      Result := TRALTemplateValue.CreateText(StringRAL(IntToStr(GetOrdProp(FObject, vProp))));
    tkInt64{$IFDEF FPC}, tkQWord{$ENDIF}:
      Result := TRALTemplateValue.CreateText(StringRAL(IntToStr(GetInt64Prop(FObject, vProp))));
    {$IFDEF FPC}
    tkBool:
      Result := TRALTemplateValue.CreateBoolean(GetOrdProp(FObject, vProp) <> 0);
    {$ENDIF}
    tkEnumeration:
      if GetTypeData(vType)^.BaseType{$IFNDEF FPC}^{$ENDIF} = TypeInfo(Boolean) then
        Result := TRALTemplateValue.CreateBoolean(GetOrdProp(FObject, vProp) <> 0)
      else
        Result := TRALTemplateValue.CreateText(ToRAL(GetEnumName(vType,
          GetOrdProp(FObject, vProp))));
    tkFloat:
    begin
      vFloat := GetFloatProp(FObject, vProp);
      if (vType = TypeInfo(TDateTime)) or (vType = TypeInfo(TDate)) or
        (vType = TypeInfo(TTime)) then
        Result := TRALTemplateValue.CreateText(ToRAL(FormatDateTime(
          'yyyy-mm-dd"T"hh:nn:ss', vFloat)))
      else
        Result := TRALTemplateValue.CreateText(ToRAL(FloatToStr(vFloat, RALInvariantFormat)));
    end;
    tkString, tkLString, tkWString{$IFDEF FPC}, tkAString{$ENDIF}
    {$IFDEF DELPHI2010UP}, tkUString{$ENDIF}{$IFDEF FPC}, tkUString{$ENDIF}:
      {$IFDEF FPC}
      Result := TRALTemplateValue.CreateText(UTF8Encode(GetUnicodeStrProp(FObject, vProp)));
      {$ELSE}
      Result := TRALTemplateValue.CreateText(UTF8Encode(GetStrProp(FObject, vProp)));
      {$ENDIF}
    tkClass:
      Result := RALTemplateDataOf(GetObjectProp(FObject, vProp));
  else
    Result := TRALTemplateValue.CreateNull;
  end;
end;

{ TRALTemplateEntry }

destructor TRALTemplateEntry.Destroy;
begin
  Compiled.Free;
  inherited Destroy;
end;

{ TRALTemplateIndexIterator }

constructor TRALTemplateIndexIterator.Create(AList: TObject);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

destructor TRALTemplateIndexIterator.Destroy;
begin
  FCurrent.Free;
  inherited Destroy;
end;

function TRALTemplateIndexIterator.Current: TRALTemplateData;
begin
  Result := FCurrent;
end;

function TRALTemplateIndexIterator.MoveNext: Boolean;
begin
  FreeAndNil(FCurrent);
  Inc(FIndex);
  Result := FIndex < Count;
  if Result then
    FCurrent := ItemAt(FIndex);
end;

{ TRALTemplateJSONIterator }

function TRALTemplateJSONIterator.Count: IntegerRAL;
begin
  Result := TRALJSONArray(FList).Count;
end;

function TRALTemplateJSONIterator.ItemAt(AIndex: IntegerRAL): TRALTemplateData;
begin
  Result := TRALTemplateJSONData.Create(TRALJSONArray(FList).Items[AIndex]);
end;

{ TRALTemplateStringsIterator }

function TRALTemplateStringsIterator.Count: IntegerRAL;
begin
  Result := TStrings(FList).Count;
end;

function TRALTemplateStringsIterator.ItemAt(AIndex: IntegerRAL): TRALTemplateData;
begin
  Result := TRALTemplateValue.CreateText(ToRAL(TStrings(FList).Strings[AIndex]));
end;

{ TRALTemplateCollectionIterator }

function TRALTemplateCollectionIterator.Count: IntegerRAL;
begin
  Result := TCollection(FList).Count;
end;

function TRALTemplateCollectionIterator.ItemAt(AIndex: IntegerRAL): TRALTemplateData;
begin
  Result := RALTemplateDataOf(TCollection(FList).Items[AIndex]);
end;

{ TRALTemplateWriter }

constructor TRALTemplateWriter.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  SetLength(FBuffer, cWriterBuffer);
end;

procedure TRALTemplateWriter.Flush;
begin
  if FCount > 0 then
    FStream.WriteBuffer(Pointer(FBuffer)^, FCount);
  FCount := 0;
end;

procedure TRALTemplateWriter.Write(const AText: StringRAL);
var
  vLen: IntegerRAL;
begin
  vLen := Length(AText);
  if vLen = 0 then
    Exit;
  if FCount + vLen > cWriterBuffer then
    Flush;
  if vLen >= cWriterBuffer then
    FStream.WriteBuffer(Pointer(AText)^, vLen)
  else
  begin
    Move(Pointer(AText)^, PAnsiChar(Pointer(FBuffer))[FCount], vLen);
    Inc(FCount, vLen);
  end;
end;

{ TRALTemplateResponseStream }

constructor TRALTemplateResponseStream.Create(AResponse: TRALResponse);
begin
  inherited Create;
  FResponse := AResponse;
end;

function TRALTemplateResponseStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TRALTemplateResponseStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := 0;
end;

function TRALTemplateResponseStream.Write(const Buffer; Count: Longint): Longint;
begin
  if FResponse.Send(Buffer, Count) then
    Result := Count
  else
    Result := 0;
end;

{ TRALMustacheNode }

constructor TRALMustacheNode.Create(AKind: TRALMustacheKind; const AText: StringRAL);
begin
  inherited Create;
  Kind := AKind;
  Text := AText;
  if AKind in [mkSection, mkInverted, mkParent, mkBlock] then
    Children := TObjectList.Create(True);
end;

destructor TRALMustacheNode.Destroy;
begin
  Children.Free;
  Filters.Free;
  inherited Destroy;
end;

{ TRALMustacheParser }

constructor TRALMustacheParser.Create(const AName, ASource: StringRAL; AFilters: Boolean);
begin
  inherited Create;
  FName := AName;
  FSource := ASource;
  FFilters := AFilters;
  FOpen := '{{';
  FClose := '}}';
  FTokens := TObjectList.Create(True);
end;

destructor TRALMustacheParser.Destroy;
begin
  FTokens.Free;
  inherited Destroy;
end;

procedure TRALMustacheParser.AddText(AList: TObjectList; const AText: StringRAL);
begin
  if AText <> '' then
    AList.Add(TRALMustacheNode.Create(mkText, AText));
end;

procedure TRALMustacheParser.AddTextTokens(AFrom, ATo: IntegerRAL);
var
  vInt, vStart: IntegerRAL;
  vToken: TRALMustacheToken;
begin
  vStart := AFrom;
  for vInt := AFrom to ATo do
    if (FSource[vInt] = #10) or (vInt = ATo) then
    begin
      vToken := TRALMustacheToken.Create;
      vToken.Sigil := #1;
      vToken.Pos := vStart;
      vToken.Text := Copy(FSource, vStart, vInt - vStart + 1);
      FTokens.Add(vToken);
      vStart := vInt + 1;
    end;
end;

procedure TRALMustacheParser.Build(ARoot: TObjectList);
var
  vInt: IntegerRAL;
  vToken: TRALMustacheToken;
  vList: TObjectList;
  vOpenNodes: TList;
  vNode: TRALMustacheNode;
  vContent: StringRAL;
begin
  vList := ARoot;
  vOpenNodes := TList.Create;
  try
    for vInt := 0 to FTokens.Count - 1 do
    begin
      vToken := TRALMustacheToken(FTokens[vInt]);
      if vToken.Removed then
        Continue;
      vContent := vToken.Text;
      case vToken.Sigil of
        #1:
          AddText(vList, vContent);
        '!', '=': ;
        '#', '^', '<', '$':
        begin
          case vToken.Sigil of
            '#': vNode := TRALMustacheNode.Create(mkSection, vContent);
            '^': vNode := TRALMustacheNode.Create(mkInverted, vContent);
            '<': vNode := TRALMustacheNode.Create(mkParent, vContent);
          else
            vNode := TRALMustacheNode.Create(mkBlock, vContent);
          end;
          vNode.Path := SplitPath(vContent);
          if vToken.Sigil = '$' then
            vNode.Indent := vToken.Indent + vToken.BlockIndent
          else
            vNode.Indent := vToken.Indent;
          if (vToken.Sigil = '<') and (Copy(vContent, 1, 1) = '*') then
          begin
            vNode.Dynamic := True;
            vNode.Path := SplitPath(Copy(vContent, 2, Length(vContent)));
          end;
          vList.Add(vNode);
          vOpenNodes.Add(vNode);
          vList := vNode.Children;
        end;
        '/':
        begin
          if vOpenNodes.Count = 0 then
            raise ERALException.CreateFmt(ecTemplateUnexpectedClose,
              [string(FName), string(vContent), LineOf(vToken.Pos)]);
          vNode := TRALMustacheNode(vOpenNodes[vOpenNodes.Count - 1]);
          if vNode.Text <> vContent then
            raise ERALException.CreateFmt(ecTemplateUnexpectedClose,
              [string(FName), string(vContent), LineOf(vToken.Pos)]);
          vOpenNodes.Delete(vOpenNodes.Count - 1);
          if vOpenNodes.Count = 0 then
            vList := ARoot
          else
            vList := TRALMustacheNode(vOpenNodes[vOpenNodes.Count - 1]).Children;
        end;
        '>':
        begin
          vNode := TRALMustacheNode.Create(mkPartial, vContent);
          vNode.Indent := vToken.Indent;
          if Copy(vContent, 1, 1) = '*' then
          begin
            vNode.Dynamic := True;
            vNode.Path := SplitPath(RALTrim(Copy(vContent, 2, Length(vContent))));
          end;
          vList.Add(vNode);
        end;
        '&', '{':
        begin
          vNode := TRALMustacheNode.Create(mkRaw, vContent);
          ReadFilters(vNode, vContent);
          vList.Add(vNode);
        end;
      else
        vNode := TRALMustacheNode.Create(mkVariable, vContent);
        ReadFilters(vNode, vContent);
        vList.Add(vNode);
      end;
    end;
    if vOpenNodes.Count > 0 then
      raise ERALException.CreateFmt(ecTemplateUnclosedSection,
        [string(FName), string(TRALMustacheNode(vOpenNodes[vOpenNodes.Count - 1]).Text)]);
  finally
    vOpenNodes.Free;
  end;
end;

procedure TRALMustacheParser.Dedent;
var
  vInt, vNext, vEnd, vDepth, vSpace: IntegerRAL;
  vToken, vFirst: TRALMustacheToken;
  vIndent: StringRAL;
begin
  for vInt := 0 to FTokens.Count - 1 do
  begin
    vToken := TRALMustacheToken(FTokens[vInt]);
    if (vToken.Sigil <> '$') or not vToken.Standalone then
      Continue;
    // The first line of content gives the indentation the block takes off.
    vNext := vInt + 1;
    while (vNext < FTokens.Count) and TRALMustacheToken(FTokens[vNext]).Removed do
      Inc(vNext);
    if vNext >= FTokens.Count then
      Continue;
    vFirst := TRALMustacheToken(FTokens[vNext]);
    vIndent := '';
    if (vFirst.Sigil = #1) and vFirst.AtLineStart then
    begin
      vSpace := 1;
      while (vSpace <= Length(vFirst.Text)) and ((vFirst.Text[vSpace] = ' ') or
        (vFirst.Text[vSpace] = #9)) do
        Inc(vSpace);
      vIndent := Copy(vFirst.Text, 1, vSpace - 1);
    end
    else if (vFirst.Sigil <> '/') and vFirst.Standalone then
      vIndent := vFirst.Indent;
    vToken.BlockIndent := vIndent;
    if vIndent = '' then
      Continue;
    vDepth := 1;
    vEnd := vInt + 1;
    while (vEnd < FTokens.Count) and (vDepth > 0) do
    begin
      vFirst := TRALMustacheToken(FTokens[vEnd]);
      if vFirst.Sigil in ['#', '^', '<', '$'] then
        Inc(vDepth)
      else if vFirst.Sigil = '/' then
        Dec(vDepth);
      if (vDepth > 0) and not vFirst.Removed then
      begin
        if (vFirst.Sigil = #1) and vFirst.AtLineStart and
          (Copy(vFirst.Text, 1, Length(vIndent)) = vIndent) then
          Delete(vFirst.Text, 1, Length(vIndent))
        else if (vFirst.Sigil <> #1) and vFirst.Standalone and
          (Copy(vFirst.Indent, 1, Length(vIndent)) = vIndent) then
          Delete(vFirst.Indent, 1, Length(vIndent));
      end;
      Inc(vEnd);
    end;
  end;
end;

function TRALMustacheParser.LineOf(APos: IntegerRAL): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := 1;
  for vInt := 1 to APos - 1 do
    if FSource[vInt] = #10 then
      Inc(Result);
end;

procedure TRALMustacheParser.MarkStandalone;
var
  vInt, vEnd, vScan, vTags, vFirstTag: IntegerRAL;
  vToken: TRALMustacheToken;
  vOther, vKeepBreak, vLineStart: Boolean;
  vIndent, vBody: StringRAL;

  { Whether AText is only blanks, without its line break. }
  function Blank(const AText: StringRAL): Boolean;
  var
    vChar: IntegerRAL;
  begin
    Result := True;
    for vChar := 1 to Length(AText) do
      if not (AText[vChar] in [' ', #9, #13, #10]) then
      begin
        Result := False;
        Exit;
      end;
  end;

begin
  // Mustache spec: a line holding only blanks and section, comment, partial,
  // delimiter, parent or block tags is dropped whole, its blanks becoming the
  // indentation of a partial, parent or block that starts it.
  vInt := 0;
  while vInt < FTokens.Count do
  begin
    vEnd := vInt;
    while (vEnd < FTokens.Count - 1) and not ((TRALMustacheToken(FTokens[vEnd]).Sigil = #1) and
      (Copy(TRALMustacheToken(FTokens[vEnd]).Text,
      Length(TRALMustacheToken(FTokens[vEnd]).Text), 1) = #10)) do
      Inc(vEnd);
    vTags := 0;
    vOther := False;
    vFirstTag := -1;
    for vScan := vInt to vEnd do
    begin
      vToken := TRALMustacheToken(FTokens[vScan]);
      if vToken.Sigil = #1 then
      begin
        if not Blank(vToken.Text) then
          vOther := True;
      end
      else if vToken.Sigil in [#0, '&', '{'] then
        vOther := True
      else
      begin
        Inc(vTags);
        if vFirstTag < 0 then
          vFirstTag := vScan;
      end;
    end;
    if (vTags > 0) and not vOther then
    begin
      vIndent := '';
      if TRALMustacheToken(FTokens[vInt]).Sigil = #1 then
        vIndent := TRALMustacheToken(FTokens[vInt]).Text;
      TRALMustacheToken(FTokens[vFirstTag]).Indent := vIndent;
      // An empty block alone on its line keeps the line break: its content
      // takes the place of the line.
      vKeepBreak := False;
      for vScan := vInt to vEnd - 1 do
        if (TRALMustacheToken(FTokens[vScan]).Sigil = '$') and
          (TRALMustacheToken(FTokens[vScan + 1]).Sigil = '/') and
          (TRALMustacheToken(FTokens[vScan + 1]).Text = TRALMustacheToken(FTokens[vScan]).Text) then
          vKeepBreak := True;
      for vScan := vInt to vEnd do
      begin
        vToken := TRALMustacheToken(FTokens[vScan]);
        if vToken.Sigil <> #1 then
          vToken.Standalone := True
        else if vKeepBreak and (vScan = vEnd) and
          (Copy(vToken.Text, Length(vToken.Text), 1) = #10) then
        begin
          vBody := vToken.Text;
          if (Length(vBody) >= 2) and (vBody[Length(vBody) - 1] = #13) then
            vToken.Text := Copy(vBody, Length(vBody) - 1, 2)
          else
            vToken.Text := Copy(vBody, Length(vBody), 1);
        end
        else
          vToken.Removed := True;
      end;
    end;
    vInt := vEnd + 1;
  end;
  // Where the kept tokens start lines; a dropped line starts none.
  vLineStart := True;
  for vInt := 0 to FTokens.Count - 1 do
  begin
    vToken := TRALMustacheToken(FTokens[vInt]);
    if vToken.Removed then
      Continue;
    vToken.AtLineStart := vLineStart;
    if vToken.Sigil = #1 then
      vLineStart := Copy(vToken.Text, Length(vToken.Text), 1) = #10
    else if not vToken.Standalone then
      vLineStart := False;
  end;
end;

procedure TRALMustacheParser.Parse(ARoot: TObjectList);
begin
  Tokenize;
  MarkStandalone;
  Dedent;
  Build(ARoot);
end;

procedure TRALMustacheParser.ReadFilters(ANode: TRALMustacheNode; const AContent: StringRAL);
var
  vParts: TStringList;
  vInt, vStart, vColon: IntegerRAL;
  vQuote: AnsiChar;
  vPart, vName, vArg: StringRAL;
begin
  if (not FFilters) or (FindFrom('|', AContent, 1) = 0) then
  begin
    ANode.Path := SplitPath(AContent);
    Exit;
  end;
  vParts := TStringList.Create;
  try
    // Bars inside a quoted argument do not split.
    vQuote := #0;
    vStart := 1;
    for vInt := 1 to Length(AContent) + 1 do
    begin
      if vInt <= Length(AContent) then
      begin
        if vQuote <> #0 then
        begin
          if AContent[vInt] = vQuote then
            vQuote := #0;
          Continue;
        end;
        if (AContent[vInt] = '"') or (AContent[vInt] = '''') then
        begin
          vQuote := AContent[vInt];
          Continue;
        end;
        if AContent[vInt] <> '|' then
          Continue;
      end;
      vParts.Add(FromRAL(RALTrim(Copy(AContent, vStart, vInt - vStart))));
      vStart := vInt + 1;
    end;
    ANode.Text := ToRAL(vParts[0]);
    ANode.Path := SplitPath(ANode.Text);
    ANode.Filters := TStringList.Create;
    for vInt := 1 to vParts.Count - 1 do
    begin
      vPart := ToRAL(vParts[vInt]);
      vColon := FindFrom(':', vPart, 1);
      if vColon = 0 then
      begin
        vName := vPart;
        vArg := '';
      end
      else
      begin
        vName := RALTrim(Copy(vPart, 1, vColon - 1));
        vArg := RALTrim(Copy(vPart, vColon + 1, Length(vPart)));
        if (Length(vArg) >= 2) and ((vArg[1] = '"') or (vArg[1] = '''')) and
          (vArg[Length(vArg)] = vArg[1]) then
          vArg := Copy(vArg, 2, Length(vArg) - 2);
      end;
      ANode.Filters.Add(FromRAL(vName) + '=' + FromRAL(vArg));
    end;
  finally
    vParts.Free;
  end;
end;

procedure TRALMustacheParser.Tokenize;
var
  vLen, vPos, vTag, vInner, vClosePos, vEnd, vSpace: IntegerRAL;
  vSigil: AnsiChar;
  vCloseMark, vContent: StringRAL;
  vToken: TRALMustacheToken;
begin
  vLen := Length(FSource);
  vPos := 1;
  while vPos <= vLen do
  begin
    vTag := FindFrom(FOpen, FSource, vPos);
    if vTag = 0 then
    begin
      AddTextTokens(vPos, vLen);
      Break;
    end;
    if vTag > vPos then
      AddTextTokens(vPos, vTag - 1);
    vInner := vTag + Length(FOpen);
    vSigil := #0;
    if (vInner <= vLen) and (FSource[vInner] in ['#', '^', '/', '!', '>', '=', '&', '{',
      '<', '$']) then
    begin
      vSigil := FSource[vInner];
      Inc(vInner);
    end;
    if vSigil = '=' then
      vCloseMark := '=' + FClose
    else if vSigil = '{' then
      vCloseMark := '}' + FClose
    else
      vCloseMark := FClose;
    vClosePos := FindFrom(vCloseMark, FSource, vInner);
    if vClosePos = 0 then
      raise ERALException.CreateFmt(ecTemplateUnclosedTag, [string(FName), LineOf(vTag)]);
    vContent := RALTrim(Copy(FSource, vInner, vClosePos - vInner));
    vEnd := vClosePos + Length(vCloseMark);
    if vSigil = '=' then
    begin
      vSpace := 1;
      while (vSpace <= Length(vContent)) and (vContent[vSpace] <> ' ') and
        (vContent[vSpace] <> #9) do
        Inc(vSpace);
      FOpen := RALTrim(Copy(vContent, 1, vSpace - 1));
      FClose := RALTrim(Copy(vContent, vSpace + 1, Length(vContent)));
      if (FOpen = '') or (FClose = '') or (FindFrom('=', FOpen, 1) > 0) or
        (FindFrom('=', FClose, 1) > 0) then
        raise ERALException.CreateFmt(ecTemplateBadDelimiter, [string(FName), LineOf(vTag)]);
    end;
    vToken := TRALMustacheToken.Create;
    vToken.Sigil := vSigil;
    vToken.Pos := vTag;
    vToken.Text := vContent;
    FTokens.Add(vToken);
    vPos := vEnd;
  end;
end;

{ TRALMustacheRenderer }

constructor TRALMustacheRenderer.Create(AEngine: TRALTemplateEngine; AStream: TStream);
begin
  inherited Create;
  FEngine := AEngine;
  FWriter := TRALTemplateWriter.Create(AStream);
  FStack := TList.Create;
  FOwned := TList.Create;
  FOverrides := TList.Create;
end;

destructor TRALMustacheRenderer.Destroy;
begin
  while FStack.Count > 0 do
    Pop;
  FStack.Free;
  FOwned.Free;
  FOverrides.Free;
  FWriter.Free;
  inherited Destroy;
end;

procedure TRALMustacheRenderer.AddContexts(const AData: array of TObject);
var
  vInt: IntegerRAL;
begin
  for vInt := Low(AData) to High(AData) do
    if AData[vInt] is TRALTemplateData then
      Push(TRALTemplateData(AData[vInt]), False)
    else
      Push(RALTemplateDataOf(AData[vInt]), True);
end;

procedure TRALMustacheRenderer.Finish;
begin
  FWriter.Flush;
end;

function TRALMustacheRenderer.FindOverride(const AName: StringRAL): TRALMustacheNode;
var
  vInt, vNode: IntegerRAL;
  vList: TList;
begin
  for vInt := 0 to FOverrides.Count - 1 do
  begin
    vList := TList(FOverrides[vInt]);
    for vNode := vList.Count - 1 downto 0 do
      if TRALMustacheNode(vList[vNode]).Text = AName then
      begin
        Result := TRALMustacheNode(vList[vNode]);
        Exit;
      end;
  end;
  Result := nil;
end;

function TRALMustacheRenderer.Lookup(const APath: TRALMustachePath;
  out AOwned: Boolean): TRALTemplateData;
var
  vInt: IntegerRAL;
  vNext: TRALTemplateData;
begin
  Result := nil;
  AOwned := False;
  if Length(APath) = 0 then
    Exit;
  if APath[0] = '.' then
  begin
    if FStack.Count > 0 then
      Result := TRALTemplateData(FStack[FStack.Count - 1]);
    Exit;
  end;
  // The first name is searched from the innermost context out; the rest only
  // inside what it found (Mustache spec, dotted names).
  for vInt := FStack.Count - 1 downto 0 do
  begin
    Result := TRALTemplateData(FStack[vInt]).Member(APath[0]);
    if Result <> nil then
      Break;
  end;
  if Result = nil then
    Exit;
  AOwned := True;
  for vInt := 1 to High(APath) do
  begin
    vNext := Result.Member(APath[vInt]);
    Result.Free;
    Result := vNext;
    if Result = nil then
    begin
      AOwned := False;
      Exit;
    end;
  end;
end;

procedure TRALMustacheRenderer.Pop;
var
  vLast: IntegerRAL;
begin
  vLast := FStack.Count - 1;
  if FOwned[vLast] <> nil then
    TObject(FStack[vLast]).Free;
  FStack.Delete(vLast);
  FOwned.Delete(vLast);
end;

procedure TRALMustacheRenderer.Push(AData: TRALTemplateData; AOwned: Boolean);
begin
  FStack.Add(AData);
  if AOwned then
    FOwned.Add(Pointer(1))
  else
    FOwned.Add(nil);
end;

procedure TRALMustacheRenderer.Render(ANodes: TObjectList);
var
  vInt: IntegerRAL;
  vNode, vOverride: TRALMustacheNode;
  vData: TRALTemplateData;
  vOwned: Boolean;
begin
  for vInt := 0 to ANodes.Count - 1 do
  begin
    vNode := TRALMustacheNode(ANodes[vInt]);
    case vNode.Kind of
      mkText:
        FWriter.Write(vNode.Text);
      mkVariable, mkRaw:
        RenderVariable(vNode);
      mkSection:
        RenderSection(vNode);
      mkInverted:
      begin
        vData := Lookup(vNode.Path, vOwned);
        try
          if (vData = nil) or vData.IsFalsey then
            Render(vNode.Children);
        finally
          if vOwned then
            vData.Free;
        end;
      end;
      mkPartial:
        RenderPartial(vNode);
      mkParent:
        RenderParent(vNode);
      mkBlock:
      begin
        vOverride := FindOverride(vNode.Text);
        if vOverride = nil then
          vOverride := vNode;
        if vNode.Indent = '' then
          Render(vOverride.Children)
        else
          RenderIndented(vOverride.Children, vNode.Indent);
      end;
    end;
  end;
end;

procedure TRALMustacheRenderer.RenderLayout(APage, ALayout: TObjectList);
var
  vBlocks: TList;
  vContent: TRALMustacheNode;
  vInt: IntegerRAL;
begin
  vBlocks := TList.Create;
  vContent := TRALMustacheNode.Create(mkBlock, 'content');
  try
    // The page is the block "content" of the layout, and its own top-level
    // blocks override the layout's instead of being written in place.
    vContent.Children.OwnsObjects := False;
    for vInt := 0 to APage.Count - 1 do
      if TRALMustacheNode(APage[vInt]).Kind = mkBlock then
        vBlocks.Add(APage[vInt])
      else
        vContent.Children.Add(APage[vInt]);
    vBlocks.Add(vContent);
    FOverrides.Add(vBlocks);
    try
      Render(ALayout);
    finally
      FOverrides.Delete(FOverrides.Count - 1);
    end;
  finally
    vContent.Free;
    vBlocks.Free;
  end;
end;

procedure TRALMustacheRenderer.RenderIndented(ANodes: TObjectList; const AIndent: StringRAL);
var
  vOuter: TRALTemplateWriter;
  vBuffer: TMemoryStream;
  vText: StringRAL;
begin
  vOuter := FWriter;
  vBuffer := TMemoryStream.Create;
  try
    FWriter := TRALTemplateWriter.Create(vBuffer);
    try
      Render(ANodes);
      FWriter.Flush;
    finally
      FWriter.Free;
      FWriter := vOuter;
    end;
    SetLength(vText, vBuffer.Size);
    if vBuffer.Size > 0 then
      Move(vBuffer.Memory^, Pointer(vText)^, vBuffer.Size);
  finally
    vBuffer.Free;
  end;
  FWriter.Write(IndentSource(vText, AIndent));
end;

procedure TRALMustacheRenderer.RenderParent(ANode: TRALMustacheNode);
var
  vBlocks: TList;
  vInt: IntegerRAL;
  vName: StringRAL;
  vData: TRALTemplateData;
  vOwned: Boolean;
  vTemplate: TObject;
begin
  vName := ANode.Text;
  if ANode.Dynamic then
  begin
    vData := Lookup(ANode.Path, vOwned);
    if vData = nil then
      Exit;
    vName := vData.Text;
    if vOwned then
      vData.Free;
  end;
  vTemplate := FEngine.Find(vName, ANode.Indent);
  if vTemplate = nil then
    Exit;
  if FDepth >= cRALTemplateMaxDepth then
    raise ERALException.CreateFmt(ecTemplateTooDeep, [string(vName), cRALTemplateMaxDepth]);
  vBlocks := TList.Create;
  try
    for vInt := 0 to ANode.Children.Count - 1 do
      if TRALMustacheNode(ANode.Children[vInt]).Kind = mkBlock then
        vBlocks.Add(ANode.Children[vInt]);
    FOverrides.Add(vBlocks);
    Inc(FDepth);
    try
      Render(TObjectList(vTemplate));
    finally
      Dec(FDepth);
      FOverrides.Delete(FOverrides.Count - 1);
    end;
  finally
    vBlocks.Free;
  end;
end;

procedure TRALMustacheRenderer.RenderPartial(ANode: TRALMustacheNode);
var
  vName: StringRAL;
  vData: TRALTemplateData;
  vOwned: Boolean;
  vTemplate: TObject;
begin
  vName := ANode.Text;
  if ANode.Dynamic then
  begin
    vData := Lookup(ANode.Path, vOwned);
    if vData = nil then
      Exit;
    vName := vData.Text;
    if vOwned then
      vData.Free;
  end;
  vTemplate := FEngine.Find(vName, ANode.Indent);
  if vTemplate = nil then
    Exit;
  if FDepth >= cRALTemplateMaxDepth then
    raise ERALException.CreateFmt(ecTemplateTooDeep, [string(vName), cRALTemplateMaxDepth]);
  Inc(FDepth);
  try
    Render(TObjectList(vTemplate));
  finally
    Dec(FDepth);
  end;
end;

procedure TRALMustacheRenderer.RenderSection(ANode: TRALMustacheNode);
var
  vData: TRALTemplateData;
  vOwned: Boolean;
  vIterator: TRALTemplateIterator;
begin
  vData := Lookup(ANode.Path, vOwned);
  if vData = nil then
    Exit;
  try
    if vData.IsFalsey then
      Exit;
    if vData.Kind = rtkList then
    begin
      vIterator := vData.Iterate;
      if vIterator = nil then
        Exit;
      try
        while vIterator.MoveNext do
        begin
          Push(vIterator.Current, False);
          try
            Render(ANode.Children);
          finally
            Pop;
          end;
        end;
      finally
        vIterator.Free;
      end;
    end
    else
    begin
      Push(vData, False);
      try
        Render(ANode.Children);
      finally
        Pop;
      end;
    end;
  finally
    if vOwned then
      vData.Free;
  end;
end;

procedure TRALMustacheRenderer.RenderVariable(ANode: TRALMustacheNode);
var
  vData: TRALTemplateData;
  vOwned: Boolean;
  vText: StringRAL;
  vInt: IntegerRAL;
begin
  vData := Lookup(ANode.Path, vOwned);
  if vData = nil then
    vText := ''
  else
  begin
    vText := vData.Text;
    if vOwned then
      vData.Free;
  end;
  if ANode.Filters <> nil then
    for vInt := 0 to ANode.Filters.Count - 1 do
      vText := FEngine.ApplyFilter(ToRAL(ANode.Filters.Names[vInt]),
        ToRAL(ANode.Filters.ValueFromIndex[vInt]), vText);
  if ANode.Kind = mkVariable then
    vText := RALHtmlEscape(vText);
  FWriter.Write(vText);
end;

{ TRALTemplateEngine }

constructor TRALTemplateEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FCache := TStringList.Create;
  FCache.Sorted := True;
  FCache.CaseSensitive := True;
  FSources := TStringList.Create;
  FSources.Sorted := True;
  FSources.CaseSensitive := True;
  FRetired := TObjectList.Create(True);
  FExtension := cRALTemplateExtension;
  FWatchFiles := True;
end;

destructor TRALTemplateEngine.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FCache.Count - 1 do
    FCache.Objects[vInt].Free;
  FCache.Free;
  for vInt := 0 to FSources.Count - 1 do
    FSources.Objects[vInt].Free;
  FSources.Free;
  FRetired.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALTemplateEngine.AddTemplate(const AName, ASource: StringRAL);
var
  vIndex: IntegerRAL;
  vSource: TRALTemplateSource;
begin
  FLock.Enter;
  try
    vIndex := FSources.IndexOf(FromRAL(AName));
    if vIndex < 0 then
    begin
      vSource := TRALTemplateSource.Create;
      FSources.AddObject(FromRAL(AName), vSource);
    end
    else
      vSource := TRALTemplateSource(FSources.Objects[vIndex]);
    vSource.Source := ASource;
    Forget(AName);
  finally
    FLock.Leave;
  end;
end;

function TRALTemplateEngine.ApplyFilter(const AFilter, AArgument, AValue: StringRAL): StringRAL;
var
  vHandled: Boolean;
  vDate: TDateTime;
  vFloat: Double;
  vJSON: TRALJSONString;
begin
  Result := AValue;
  vHandled := False;
  if Assigned(FOnFilter) then
  begin
    FOnFilter(Self, AFilter, AArgument, Result, vHandled);
    if vHandled then
      Exit;
  end;
  if RALSameName(AFilter, 'upper') then
    Result := ToRAL(AnsiUpperCase(FromRAL(AValue)))
  else if RALSameName(AFilter, 'lower') then
    Result := ToRAL(AnsiLowerCase(FromRAL(AValue)))
  else if RALSameName(AFilter, 'trim') then
    Result := RALTrim(AValue)
  else if RALSameName(AFilter, 'default') then
  begin
    if AValue = '' then
      Result := AArgument;
  end
  else if RALSameName(AFilter, 'json') then
  begin
    vJSON := TRALJSONString.Create(AValue);
    try
      Result := vJSON.ToJSON;
    finally
      vJSON.Free;
    end;
  end
  else if RALSameName(AFilter, 'url') then
    Result := RALUrlEscape(AValue)
  else if RALSameName(AFilter, 'date') then
  begin
    if RALTryISO8601ToDateTime(AValue, vDate) then
    begin
      if AArgument = '' then
        Result := ToRAL(DateToStr(vDate))
      else
        Result := ToRAL(FormatDateTime(FromRAL(AArgument), vDate, vDateFormat));
    end;
  end
  else if RALSameName(AFilter, 'number') then
  begin
    if RALTryStrToFloat(AValue, vFloat) then
    begin
      if AArgument = '' then
        Result := ToRAL(FloatToStr(vFloat))
      else
        Result := ToRAL(FormatFloat(FromRAL(AArgument), vFloat));
    end;
  end;
end;

function TRALTemplateEngine.FileOf(const AName: StringRAL): StringRAL;
var
  vRoot, vName: string;
  vInt: IntegerRAL;
begin
  Result := '';
  if (FTemplateRoot = '') or (AName = '') then
    Exit;
  vName := FromRAL(AName);
  // A name never leaves the root: no drive, no absolute path, no "..".
  for vInt := 1 to Length(vName) do
    if vName[vInt] = '\' then
      vName[vInt] := '/';
  if (vName[1] = '/') or (Pos(':', vName) > 0) or (Pos('../', vName + '/') > 0) then
    Exit;
  if ExtractFileExt(vName) = '' then
    vName := vName + FromRAL(FExtension);
  vRoot := FromRAL(FTemplateRoot);
  if (ExtractFileDrive(vRoot) = '') and (Copy(vRoot, 1, 1) <> PathDelim) then
    vRoot := ExtractFilePath(ParamStr(0)) + vRoot;
  vRoot := IncludeTrailingPathDelimiter(vRoot);
  for vInt := 1 to Length(vName) do
    if vName[vInt] = '/' then
      vName[vInt] := PathDelim;
  Result := ToRAL(vRoot + vName);
end;

function TRALTemplateEngine.Find(const AName: StringRAL; const AIndent: StringRAL): TObject;
var
  vKey: string;
  vIndex: IntegerRAL;
  vEntry: TRALTemplateEntry;
  vDate: TDateTime;
  vSource, vFile: StringRAL;
begin
  Result := nil;
  vKey := FromRAL(AName) + #1 + FromRAL(AIndent);
  FLock.Enter;
  try
    vIndex := FCache.IndexOf(vKey);
    if vIndex >= 0 then
    begin
      vEntry := TRALTemplateEntry(FCache.Objects[vIndex]);
      if (not FWatchFiles) or (vEntry.FileName = '') or
        (FileAge(FromRAL(vEntry.FileName), vDate) and (vDate = vEntry.FileDate)) then
      begin
        Result := vEntry.Compiled;
        Exit;
      end;
      Forget(AName);
    end;
    if not LoadSource(AName, vSource, vFile) then
      Exit;
    vEntry := TRALTemplateEntry.Create;
    try
      vEntry.FileName := vFile;
      if vFile <> '' then
        FileAge(FromRAL(vFile), vEntry.FileDate);
      vEntry.Compiled := Compile(AName, IndentSource(vSource, AIndent));
    except
      vEntry.Free;
      raise;
    end;
    FCache.AddObject(vKey, vEntry);
    Result := vEntry.Compiled;
  finally
    FLock.Leave;
  end;
end;

procedure TRALTemplateEngine.Forget(const AName: StringRAL);
var
  vInt: IntegerRAL;
  vPrefix: string;
  vEntry: TRALTemplateEntry;
begin
  vPrefix := FromRAL(AName) + #1;
  for vInt := FCache.Count - 1 downto 0 do
    if Copy(FCache[vInt], 1, Length(vPrefix)) = vPrefix then
    begin
      // A render on another thread may still walk the old nodes.
      vEntry := TRALTemplateEntry(FCache.Objects[vInt]);
      FRetired.Add(vEntry);
      FCache.Delete(vInt);
    end;
end;

function TRALTemplateEngine.LoadSource(const AName: StringRAL;
  out ASource, AFileName: StringRAL): Boolean;
var
  vIndex: IntegerRAL;
  vStream: TFileStream;
begin
  AFileName := '';
  ASource := '';
  vIndex := FSources.IndexOf(FromRAL(AName));
  if vIndex >= 0 then
  begin
    ASource := TRALTemplateSource(FSources.Objects[vIndex]).Source;
    Result := True;
    Exit;
  end;
  Result := False;
  if Assigned(FOnLoadTemplate) then
  begin
    FOnLoadTemplate(Self, AName, ASource, Result);
    if Result then
      Exit;
  end;
  AFileName := FileOf(AName);
  if (AFileName = '') or not FileExists(FromRAL(AFileName)) then
  begin
    AFileName := '';
    Exit;
  end;
  vStream := TFileStream.Create(FromRAL(AFileName), fmOpenRead or fmShareDenyWrite);
  try
    SetLength(ASource, vStream.Size);
    if vStream.Size > 0 then
      vStream.ReadBuffer(Pointer(ASource)^, vStream.Size);
  finally
    vStream.Free;
  end;
  if (Length(ASource) >= 3) and (Ord(ASource[1]) = $EF) and (Ord(ASource[2]) = $BB) and
    (Ord(ASource[3]) = $BF) then
    Delete(ASource, 1, 3);
  Result := True;
end;

procedure TRALTemplateEngine.Reload;
var
  vInt: IntegerRAL;
begin
  FLock.Enter;
  try
    for vInt := FCache.Count - 1 downto 0 do
    begin
      FRetired.Add(FCache.Objects[vInt]);
      FCache.Delete(vInt);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TRALTemplateEngine.Render(const AName: StringRAL; const AData: array of TObject;
  AStream: TStream; AUseLayout: Boolean);
var
  vPage, vLayout: TObject;
begin
  vPage := Find(AName);
  if vPage = nil then
    raise ERALException.CreateFmt(ecTemplateNotFound, [string(AName)]);
  vLayout := nil;
  if AUseLayout and (FLayout <> '') then
  begin
    vLayout := Find(FLayout);
    if vLayout = nil then
      raise ERALException.CreateFmt(ecTemplateNotFound, [string(FLayout)]);
  end;
  Execute(vPage, vLayout, AData, AStream);
end;

procedure TRALTemplateEngine.Render(AResponse: TRALResponse; const AName: StringRAL;
  const AData: array of TObject; AStatusCode: IntegerRAL; AUseLayout: Boolean);
var
  vStream: TStream;
begin
  if FStreamed then
  begin
    AResponse.BeginStream(AStatusCode, rctTEXTHTML + '; charset=utf-8');
    vStream := TRALTemplateResponseStream.Create(AResponse);
    try
      Render(AName, AData, vStream, AUseLayout);
    finally
      vStream.Free;
    end;
  end
  else
  begin
    vStream := TMemoryStream.Create;
    try
      Render(AName, AData, vStream, AUseLayout);
    except
      vStream.Free;
      raise;
    end;
    vStream.Position := 0;
    AResponse.Answer(AStatusCode, vStream, rctTEXTHTML + '; charset=utf-8', True);
  end;
end;

function TRALTemplateEngine.RenderSource(const ASource: StringRAL;
  const AData: array of TObject): StringRAL;
var
  vCompiled: TObject;
  vStream: TMemoryStream;
begin
  vCompiled := Compile('', ASource);
  vStream := TMemoryStream.Create;
  try
    Execute(vCompiled, nil, AData, vStream);
    SetLength(Result, vStream.Size);
    if vStream.Size > 0 then
      Move(vStream.Memory^, Pointer(Result)^, vStream.Size);
  finally
    vStream.Free;
    vCompiled.Free;
  end;
end;

function TRALTemplateEngine.RenderText(const AName: StringRAL;
  const AData: array of TObject): StringRAL;
var
  vStream: TMemoryStream;
begin
  vStream := TMemoryStream.Create;
  try
    Render(AName, AData, vStream, False);
    SetLength(Result, vStream.Size);
    if vStream.Size > 0 then
      Move(vStream.Memory^, Pointer(Result)^, vStream.Size);
  finally
    vStream.Free;
  end;
end;

{ TRALMustacheEngine }

function TRALMustacheEngine.Compile(const AName, ASource: StringRAL): TObject;
var
  vParser: TRALMustacheParser;
begin
  Result := TObjectList.Create(True);
  vParser := TRALMustacheParser.Create(AName, ASource, AllowFilters);
  try
    try
      vParser.Parse(TObjectList(Result));
    except
      Result.Free;
      raise;
    end;
  finally
    vParser.Free;
  end;
end;

procedure TRALMustacheEngine.Execute(ACompiled, ALayout: TObject;
  const AData: array of TObject; AStream: TStream);
var
  vRenderer: TRALMustacheRenderer;
begin
  vRenderer := TRALMustacheRenderer.Create(Self, AStream);
  try
    vRenderer.AddContexts(AData);
    if ALayout = nil then
      vRenderer.Render(TObjectList(ACompiled))
    else
      vRenderer.RenderLayout(TObjectList(ACompiled), TObjectList(ALayout));
    vRenderer.Finish;
  finally
    vRenderer.Free;
  end;
end;

initialization
  vRegistry := TObjectList.Create(True);
  vDateFormat := {$IFDEF FPC}DefaultFormatSettings{$ELSE}FormatSettings{$ENDIF};
  vDateFormat.DateSeparator := '/';
  vDateFormat.TimeSeparator := ':';
  RALRegisterTemplateData(TRALJSON, TRALTemplateJSONData);
  RALRegisterTemplateData(TStrings, TRALTemplateStringsData);
  RALRegisterTemplateData(TCollection, TRALTemplateCollectionData);

finalization
  FreeAndNil(vRegistry);

end.
