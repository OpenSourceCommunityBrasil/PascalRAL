/// XML document of RAL: a DOM with a fluent API, paths, typed values and datasets.
unit RALXML;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, FMTBcd, Math,
  RALTypes, RALConsts, RALXMLReader, RALXMLWriter, RALStorageXML;

type
  /// Kind of a node.
  TRALXMLNodeKind = (xnkDocument, xnkElement, xnkText, xnkCData, xnkComment,
    xnkProcessingInstruction, xnkDocType);

  TRALXMLDocument = class;
  TRALXMLNode = class;

  /// An attribute of an element.
  TRALXMLNodeAttribute = record
    /// Qualified name.
    Name: StringRAL;
    /// Value, unescaped.
    Value: StringRAL;
  end;

  /// Attributes of an element.
  TRALXMLNodeAttributes = array of TRALXMLNodeAttribute;

  /// Nodes a path selected; it does not own them.
  TRALXMLNodeList = class
  private
    FList: TList;

    function GetItem(AIndex: IntegerRAL): TRALXMLNode;
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds ANode to the list.
    procedure Add(ANode: TRALXMLNode);
    /// Number of nodes.
    function Count: IntegerRAL;

    /// Node at AIndex; nil out of range.
    property Items[AIndex: IntegerRAL]: TRALXMLNode read GetItem; default;
    /// The list itself, for manual iteration.
    property List: TList read FList;
  end;

  { A node of a document: element, text, CDATA, comment, processing instruction or
    DOCTYPE. A node frees its children; a node freed by itself leaves its parent. }
  TRALXMLNode = class
  private
    FAttrCount: IntegerRAL;
    /// Attributes of an element, in document order.
    FAttributes: TRALXMLNodeAttributes;
    FDocument: TRALXMLDocument;
    FFirstChild: TRALXMLNode;
    FKind: TRALXMLNodeKind;
    FLastChild: TRALXMLNode;
    FName: StringRAL;
    /// Namespace of the node, once resolved.
    FNamespaceURI: StringRAL;
    FNextSibling: TRALXMLNode;
    /// NamespaceURI was resolved.
    FNsResolved: boolean;
    FParent: TRALXMLNode;
    FPreviousSibling: TRALXMLNode;
    FValue: StringRAL;

    /// Index of the attribute AName, or -1.
    function AttrIndex(const AName: StringRAL): IntegerRAL;
    /// A copy of the node that belongs to ADocument, with its subtree when ADeep.
    function CloneInto(ADocument: TRALXMLDocument; ADeep: boolean): TRALXMLNode;
    /// Frees the children, iteratively, whatever the depth.
    procedure FreeChildren;
    function GetAttributeValue(const AName: StringRAL): StringRAL;
    function GetChildCount: IntegerRAL;
    function GetChildren(AIndex: IntegerRAL): TRALXMLNode;
    function GetInnerText: StringRAL;
    function GetLocalName: StringRAL;
    function GetNamespaceURI: StringRAL;
    function GetOwnText: StringRAL;
    function GetPrefix: StringRAL;
    /// AName from the name table of the document.
    function Intern(const AName: StringRAL): StringRAL;
    /// Links AChild before ABefore, or last when ABefore is nil.
    procedure Link(AChild, ABefore: TRALXMLNode);
    /// Forgets the resolved namespaces of the node and its descendants.
    procedure ResetNamespaces;
    procedure SetAttributeValue(const AName, AValue: StringRAL);
    procedure SetNodeName(const AValue: StringRAL);
    procedure SetText(const AValue: StringRAL);
    /// Takes the node out of its parent, without freeing it.
    procedure Unlink;
  public
    /// A node of ADocument, of AKind, named AName; it has no parent yet.
    constructor Create(ADocument: TRALXMLDocument; AKind: TRALXMLNodeKind;
      const AName: StringRAL = '');
    destructor Destroy; override;

    /// Sets the attribute AName (adds or replaces) and returns this node.
    function AddAttribute(const AName, AValue: StringRAL): TRALXMLNode;
    /// Adds a CDATA child and returns this node.
    function AddCData(const AValue: StringRAL): TRALXMLNode;
    /// Adds a new element child named AName and returns the child.
    function AddChild(const AName: StringRAL): TRALXMLNode;
    /// Adds a comment child and returns this node.
    function AddComment(const AValue: StringRAL): TRALXMLNode;
    /// Declares the namespace AURI for APrefix ('' is the default) and returns this node.
    function AddNamespace(const APrefix, AURI: StringRAL): TRALXMLNode;
    /// Adds a processing instruction child and returns this node.
    function AddProcessingInstruction(const ATarget, AData: StringRAL): TRALXMLNode;
    /// Adds a text child and returns this node.
    function AddText(const AValue: StringRAL): TRALXMLNode;
    /// Parses the fragment AXML and adds its nodes as children; returns this node.
    function AddXML(const AXML: StringRAL): TRALXMLNode;
    /// Moves or adds ANode as the last child and returns ANode.
    function AppendChild(ANode: TRALXMLNode): TRALXMLNode;
    /// Adds the records of ADataSet as a storage of AXMLType inside this node.
    function AppendDataSet(ADataSet: TDataSet; AXMLType: TRALXMLType = xtDBWare): TRALXMLNode;
    /// Text decoded from base64.
    function AsBase64: TBytes;
    /// Text as an exact decimal; raises EConvertError for anything else.
    function AsBCD: TBcd;
    /// Text as a boolean: true, false, 1 or 0; raises EConvertError otherwise.
    function AsBoolean: boolean;
    /// Text as an ISO 8601 date and time; raises EConvertError otherwise.
    function AsDateTime: TDateTime;
    /// Text as a number with "." for the decimals; INF, -INF and NaN are read.
    function AsFloat: Double;
    /// Text as a 64-bit integer; raises EConvertError otherwise.
    function AsInt64: Int64RAL;
    /// Text as an integer; raises EConvertError otherwise.
    function AsInteger: IntegerRAL;
    /// Text of the node.
    function AsString: StringRAL;
    /// Value of the attribute AName, or '' when it has none.
    function Attribute(const AName: StringRAL): StringRAL;
    /// Name of the attribute AIndex.
    function AttributeName(AIndex: IntegerRAL): StringRAL;
    /// Value of the attribute ALocalName in the namespace AURI, or ''.
    function AttributeNS(const AURI, ALocalName: StringRAL): StringRAL;
    /// First element child named AName (qualified, or local without a prefix), or nil.
    function Child(const AName: StringRAL): TRALXMLNode;
    /// First element child of the namespace AURI named ALocalName, or nil.
    function ChildNS(const AURI, ALocalName: StringRAL): TRALXMLNode;
    /// Removes the attributes, the children and the text.
    procedure Clear;
    /// A copy of the node, with its whole subtree when ADeep; it has no parent.
    function Clone(ADeep: boolean = True): TRALXMLNode;
    /// Removes the attribute AName.
    procedure DeleteAttribute(const AName: StringRAL);
    /// Takes the node out of its parent without freeing it, and returns it.
    function Detach: TRALXMLNode;
    /// Number of element children.
    function ElementCount: IntegerRAL;
    /// First element child, or nil.
    function FirstElement: TRALXMLNode;
    /// Value of the attribute a path names (".../@name" or "a.b.name"), or ''.
    function GetAttribute(const APath: StringRAL): StringRAL;
    /// First node a path selects from this node, or nil.
    function GetNode(const APath: StringRAL): TRALXMLNode;
    /// Every node a path selects from this node; the caller frees the list.
    function GetNodes(const APath: StringRAL): TRALXMLNodeList;
    /// Text of the node a path selects, or the attribute it ends in; '' when none.
    function GetText(const APath: StringRAL): StringRAL;
    /// True when the element has the attribute AName.
    function HasAttribute(const AName: StringRAL): boolean;
    /// True when an element child named AName exists.
    function HasChild(const AName: StringRAL): boolean;
    /// Inserts ANode before ARef (a child of this node) and returns ANode.
    function InsertBefore(ANode, ARef: TRALXMLNode): TRALXMLNode;
    /// Namespace bound to APrefix where this node stands, or ''.
    function LookupNamespaceURI(const APrefix: StringRAL): StringRAL;
    /// Prefix bound to AURI where this node stands; '' for the default namespace or none.
    function LookupPrefix(const AURI: StringRAL): StringRAL;
    /// Next element sibling, or nil.
    function NextElement: TRALXMLNode;
    /// Loads the dataset this node holds (as AppendDataSet writes it) into ADataSet.
    procedure ReadDataSet(ADataSet: TDataSet);
    /// Removes the attribute AName and returns this node.
    function RemoveAttribute(const AName: StringRAL): TRALXMLNode;
    /// Removes and frees the child AChild.
    procedure RemoveChild(const AChild: TRALXMLNode);
    /// Writes the node and its subtree into AStream, in UTF-8.
    procedure SaveToStream(AStream: TStream; AIndent: boolean = False);
    { Returns the node and its subtree as XML text; AIndent puts each element on a
      line of its own, ALevel indents the first one. }
    function ToXML(AIndent: boolean = True; ALevel: IntegerRAL = 0): StringRAL;
    /// Sets the text to base64 of AValue and returns this node.
    function WithBase64(const AValue: TBytes): TRALXMLNode;
    /// Sets the text to AValue as an exact decimal and returns this node.
    function WithBCD(const AValue: TBcd): TRALXMLNode;
    /// Replaces the children with a CDATA section and returns this node.
    function WithCData(const AValue: StringRAL): TRALXMLNode;
    /// Sets the text to AValue in ISO 8601, local time and its offset; returns this node.
    function WithDateTime(AValue: TDateTime; AInputIsUTC: boolean = False): TRALXMLNode;
    /// Replaces the children with the text AText and returns this node.
    function WithText(const AText: StringRAL): TRALXMLNode;
    /// Sets the text to AValue and returns this node.
    function WithValue(AValue: Int64RAL): TRALXMLNode; overload;
    /// Sets the text to AValue, with "." for the decimals, and returns this node.
    function WithValue(AValue: Double): TRALXMLNode; overload;
    /// Sets the text to true or false and returns this node.
    function WithValue(AValue: boolean): TRALXMLNode; overload;

    /// Number of attributes.
    property AttributeCount: IntegerRAL read FAttrCount;
    /// Value of the attribute AName; assigning sets it.
    property Attributes[const AName: StringRAL]: StringRAL read GetAttributeValue
      write SetAttributeValue;
    /// Number of children of any kind.
    property ChildCount: IntegerRAL read GetChildCount;
    /// Child AIndex of any kind; nil out of range.
    property Children[AIndex: IntegerRAL]: TRALXMLNode read GetChildren;
    /// Document the node belongs to.
    property Document: TRALXMLDocument read FDocument;
    /// First child of any kind.
    property FirstChild: TRALXMLNode read FFirstChild;
    /// Text of every text and CDATA descendant, in document order.
    property InnerText: StringRAL read GetInnerText;
    /// Kind of the node.
    property Kind: TRALXMLNodeKind read FKind;
    /// Last child of any kind.
    property LastChild: TRALXMLNode read FLastChild;
    /// Part of the name after the prefix.
    property LocalName: StringRAL read GetLocalName;
    /// Namespace of the element, from the declarations in scope.
    property NamespaceURI: StringRAL read GetNamespaceURI;
    /// Next sibling of any kind.
    property NextSibling: TRALXMLNode read FNextSibling;
    /// Qualified name of an element, target of a processing instruction.
    property NodeName: StringRAL read FName write SetNodeName;
    /// Parent node; nil for a detached node and for the document node.
    property Parent: TRALXMLNode read FParent;
    /// Prefix of the element's name, or ''.
    property Prefix: StringRAL read GetPrefix;
    /// Previous sibling of any kind.
    property PreviousSibling: TRALXMLNode read FPreviousSibling;
    /// Text of the direct text and CDATA children; assigning replaces the children.
    property Text: StringRAL read GetOwnText write SetText;
    /// Content of a text, CDATA, comment or processing instruction node.
    property Value: StringRAL read FValue write FValue;
  end;

  { A whole XML document: prolog, root element and what follows. LoadFrom* parse with
    TRALXMLReader and its Options; SaveTo* write UTF-8. }
  TRALXMLDocument = class
  private
    FDocumentNode: TRALXMLNode;
    FEncoding: StringRAL;
    FIndent: boolean;
    FNames: TRALXMLNameTable;
    FOptions: TRALXMLReaderOptions;
    FPreserveWhitespace: boolean;
    FStandalone: StringRAL;
    FVersion: StringRAL;

    function GetRoot: TRALXMLNode;
    /// Builds the nodes from what AReader reads.
    procedure Parse(AReader: TRALXMLReader);
  public
    constructor Create;
    destructor Destroy; override;

    /// Removes every node.
    procedure Clear;
    /// Creates a detached element of the document; append it where it belongs.
    function CreateElement(const AName: StringRAL): TRALXMLNode;
    /// Replaces the root element with a new one named ARootName and returns it.
    function CreateRoot(const ARootName: StringRAL): TRALXMLNode;
    /// Value of the attribute a path names (".../@name" or "root.a.name"), or ''.
    function GetAttribute(const APath: StringRAL): StringRAL;
    { First node a path selects, or nil: "/a/b", "a/b[2]", "//c[@@x='1']", "a/*",
      "text()", or the dotted form "root.a.b". }
    function GetNode(const APath: StringRAL): TRALXMLNode;
    /// Every node a path selects; the caller frees the list.
    function GetNodes(const APath: StringRAL): TRALXMLNodeList;
    /// Text of the node a path selects, or the attribute it ends in; '' when none.
    function GetText(const APath: StringRAL): StringRAL;
    /// Parses ASize bytes at AData; ACharset is the charset parameter of the media type.
    procedure LoadFromBuffer(AData: Pointer; ASize: IntegerRAL; const ACharset: StringRAL = '');
    /// Parses the file AFileName.
    procedure LoadFromFile(const AFileName: string);
    /// Parses AStream from its start; ACharset as in LoadFromBuffer.
    procedure LoadFromStream(AStream: TStream; const ACharset: StringRAL = '');
    /// Parses AXML; ACharset as in LoadFromBuffer.
    procedure LoadFromString(const AXML: StringRAL; const ACharset: StringRAL = '');
    /// Writes the document into the file AFileName, in UTF-8.
    procedure SaveToFile(const AFileName: string; AAllowHeader: boolean = True);
    /// Writes the document into AStream in UTF-8; AAllowHeader adds the XML declaration.
    procedure SaveToStream(AStream: TStream; AAllowHeader: boolean = True);
    /// Returns the document as text; AAllowHeader writes the XML declaration.
    function SaveToString(AAllowHeader: boolean = True): StringRAL;

    /// Node that holds the prolog, the root element and what follows it.
    property DocumentNode: TRALXMLNode read FDocumentNode;
    /// Encoding the parsed document declared; documents are always written in UTF-8.
    property Encoding: StringRAL read FEncoding write FEncoding;
    /// SaveTo* put each element on a line of its own.
    property Indent: boolean read FIndent write FIndent;
    /// Interned names of the document.
    property NameTable: TRALXMLNameTable read FNames;
    /// Limits and choices of the parser.
    property Options: TRALXMLReaderOptions read FOptions;
    /// Keeps text nodes that hold only blanks between elements.
    property PreserveWhitespace: boolean read FPreserveWhitespace write FPreserveWhitespace;
    /// Root element, or nil.
    property Root: TRALXMLNode read GetRoot;
    /// Standalone of the XML declaration: yes, no or ''.
    property Standalone: StringRAL read FStandalone write FStandalone;
    /// Version of the XML declaration.
    property Version: StringRAL read FVersion write FVersion;
  end;

implementation

uses
  RALTools, RALBase64, RALDBTypes;

/// Splits a qualified name into prefix and local part
procedure SplitQName(const AName: StringRAL; out APrefix, ALocal: StringRAL);
var
  vPos: IntegerRAL;
begin
  vPos := Pos(StringRAL(':'), AName);
  if vPos = 0 then
  begin
    APrefix := '';
    ALocal := AName;
  end
  else
  begin
    APrefix := Copy(AName, 1, vPos - 1);
    ALocal := Copy(AName, vPos + 1, MaxInt);
  end;
end;

{ TRALXMLNodeList }

constructor TRALXMLNodeList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TRALXMLNodeList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TRALXMLNodeList.Add(ANode: TRALXMLNode);
begin
  FList.Add(ANode);
end;

function TRALXMLNodeList.Count: IntegerRAL;
begin
  Result := FList.Count;
end;

function TRALXMLNodeList.GetItem(AIndex: IntegerRAL): TRALXMLNode;
begin
  if (AIndex >= 0) and (AIndex < FList.Count) then
    Result := TRALXMLNode(FList[AIndex])
  else
    Result := nil;
end;

{ path evaluation }

type
  /// One step of a path: what it matches, and its predicate
  TRALXMLPathStep = record
    /// The step searches every descendant (//), not only the children.
    Descendant: boolean;
    /// Name the step matches, '*' or a node test.
    Test: StringRAL;
    /// Predicate: 0 none, 1 position, 2 attribute present, 3 attribute, 4 child, 5 text.
    PredKind: IntegerRAL;
    /// Name the predicate tests.
    PredName: StringRAL;
    /// Value the predicate compares with.
    PredValue: StringRAL;
    /// Position of a positional predicate, from 1.
    PredIndex: IntegerRAL;
  end;
  /// Steps of a path.
  TRALXMLPathSteps = array of TRALXMLPathStep;

/// APath is in XPath form (has a slash, a bracket or an at sign), not dotted.
function IsXPath(const APath: StringRAL): boolean;
begin
  Result := (Pos(StringRAL('/'), APath) > 0) or (Pos(StringRAL('['), APath) > 0) or
    (Pos(StringRAL('@'), APath) > 0) or (Pos(StringRAL('('), APath) > 0) or
    (Pos(StringRAL('*'), APath) > 0);
end;

/// AValue without its quotes, single or double.
function Unquote(const AValue: StringRAL): StringRAL;
begin
  Result := RALTrim(AValue);
  if (Length(Result) >= 2) and (((Result[1] = '''') and (Result[Length(Result)] = '''')) or
     ((Result[1] = '"') and (Result[Length(Result)] = '"'))) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

/// Parses the predicate text between brackets into AStep
procedure ParsePredicate(var AStep: TRALXMLPathStep; const AText: StringRAL);
var
  vText: StringRAL;
  vPos: IntegerRAL;
begin
  vText := RALTrim(AText);
  if TryStrToInt(string(vText), AStep.PredIndex) then
  begin
    AStep.PredKind := 1;
    Exit;
  end;

  vPos := Pos(StringRAL('='), vText);
  if (vText <> '') and (vText[1] = '@') then
  begin
    if vPos = 0 then
    begin
      AStep.PredKind := 2;
      AStep.PredName := RALTrim(Copy(vText, 2, MaxInt));
    end
    else
    begin
      AStep.PredKind := 3;
      AStep.PredName := RALTrim(Copy(vText, 2, vPos - 2));
      AStep.PredValue := Unquote(Copy(vText, vPos + 1, MaxInt));
    end;
  end
  else if vPos > 0 then
  begin
    AStep.PredName := RALTrim(Copy(vText, 1, vPos - 1));
    AStep.PredValue := Unquote(Copy(vText, vPos + 1, MaxInt));
    if AStep.PredName = 'text()' then
      AStep.PredKind := 5
    else
      AStep.PredKind := 4;
  end
  else
    raise EConvertError.Create(Format(emXMLPathInvalid, [string(AText)]));
end;

/// The steps of APath; AAbsolute when it starts at the root.
function ParsePath(const APath: StringRAL; out AAbsolute: boolean): TRALXMLPathSteps;
var
  vInt, vStart, vCount, vBracket: IntegerRAL;
  vDescendant: boolean;
  vStep: StringRAL;
  vPos: IntegerRAL;
begin
  Result := nil;
  vCount := 0;
  vInt := 1;
  AAbsolute := (APath <> '') and (APath[1] = '/');
  vDescendant := False;
  if AAbsolute then
  begin
    Inc(vInt);
    if (vInt <= Length(APath)) and (APath[vInt] = '/') then
    begin
      vDescendant := True;
      Inc(vInt);
    end;
  end;

  while vInt <= Length(APath) do
  begin
    vStart := vInt;
    vBracket := 0;
    while (vInt <= Length(APath)) and ((APath[vInt] <> '/') or (vBracket > 0)) do
    begin
      if APath[vInt] = '[' then
        Inc(vBracket)
      else if APath[vInt] = ']' then
        Dec(vBracket);
      Inc(vInt);
    end;
    vStep := Copy(APath, vStart, vInt - vStart);

    SetLength(Result, vCount + 1);
    Result[vCount].Descendant := vDescendant;
    Result[vCount].PredKind := 0;
    vPos := Pos(StringRAL('['), vStep);
    if vPos > 0 then
    begin
      if vStep[Length(vStep)] <> ']' then
        raise EConvertError.Create(Format(emXMLPathInvalid, [string(APath)]));
      ParsePredicate(Result[vCount], Copy(vStep, vPos + 1, Length(vStep) - vPos - 1));
      vStep := Copy(vStep, 1, vPos - 1);
    end;
    Result[vCount].Test := RALTrim(vStep);
    Inc(vCount);

    vDescendant := False;
    if vInt <= Length(APath) then
    begin
      Inc(vInt);
      if (vInt <= Length(APath)) and (APath[vInt] = '/') then
      begin
        vDescendant := True;
        Inc(vInt);
      end;
    end;
  end;
end;

/// ANode matches the name test ATest.
function NameMatches(ANode: TRALXMLNode; const ATest: StringRAL): boolean;
var
  vPrefix, vLocal: StringRAL;
begin
  Result := False;
  if ANode.Kind <> xnkElement then
    Exit;
  if (ATest = '*') or (ATest = 'node()') then
  begin
    Result := True;
    Exit;
  end;
  if ANode.NodeName = ATest then
  begin
    Result := True;
    Exit;
  end;

  SplitQName(ATest, vPrefix, vLocal);
  if ANode.LocalName <> vLocal then
    Exit;
  // no prefix in the test matches the local name; a prefix matches by namespace
  Result := (vPrefix = '') or (ANode.Prefix = vPrefix) or
    ((ANode.NamespaceURI <> '') and (ANode.NamespaceURI = ANode.LookupNamespaceURI(vPrefix)));
end;

/// ANode passes the predicate of AStep.
function PredicateMatches(ANode: TRALXMLNode; const AStep: TRALXMLPathStep): boolean;
var
  vChild: TRALXMLNode;
begin
  case AStep.PredKind of
    2:
      Result := ANode.HasAttribute(AStep.PredName);
    3:
      Result := ANode.HasAttribute(AStep.PredName) and
        (ANode.Attribute(AStep.PredName) = AStep.PredValue);
    4:
      begin
        Result := False;
        vChild := ANode.FirstChild;
        while vChild <> nil do
        begin
          if NameMatches(vChild, AStep.PredName) and (vChild.Text = AStep.PredValue) then
          begin
            Result := True;
            Exit;
          end;
          vChild := vChild.NextSibling;
        end;
      end;
    5:
      Result := ANode.Text = AStep.PredValue;
  else
    Result := True;
  end;
end;

/// Adds to AOut the nodes AStep selects from AContext
procedure ApplyStep(AContext: TRALXMLNode; const AStep: TRALXMLPathStep; AOut: TList);
var
  vMatched: TList;
  vNode, vStop: TRALXMLNode;
  vInt: IntegerRAL;

  procedure Consider(ANode: TRALXMLNode);
  begin
    if AStep.Test = 'text()' then
    begin
      if ANode.Kind in [xnkText, xnkCData] then
        vMatched.Add(ANode);
    end
    else if NameMatches(ANode, AStep.Test) and PredicateMatches(ANode, AStep) then
      vMatched.Add(ANode);
  end;

begin
  if AStep.Test = '.' then
  begin
    AOut.Add(AContext);
    Exit;
  end;
  if AStep.Test = '..' then
  begin
    if AContext.Parent <> nil then
      AOut.Add(AContext.Parent);
    Exit;
  end;

  vMatched := TList.Create;
  try
    if AStep.Descendant then
    begin
      // every descendant, in document order, walked without recursion
      vNode := AContext.FirstChild;
      vStop := AContext;
      while vNode <> nil do
      begin
        Consider(vNode);
        if vNode.FirstChild <> nil then
          vNode := vNode.FirstChild
        else
        begin
          while (vNode <> vStop) and (vNode.NextSibling = nil) do
            vNode := vNode.Parent;
          if vNode = vStop then
            vNode := nil
          else
            vNode := vNode.NextSibling;
        end;
      end;
    end
    else
    begin
      vNode := AContext.FirstChild;
      while vNode <> nil do
      begin
        Consider(vNode);
        vNode := vNode.NextSibling;
      end;
    end;

    if AStep.PredKind = 1 then
    begin
      if (AStep.PredIndex >= 1) and (AStep.PredIndex <= vMatched.Count) then
        AOut.Add(vMatched[AStep.PredIndex - 1]);
    end
    else
      for vInt := 0 to vMatched.Count - 1 do
        if AOut.IndexOf(vMatched[vInt]) < 0 then
          AOut.Add(vMatched[vInt]);
  finally
    vMatched.Free;
  end;
end;

{ Evaluates APath from AContext into AOut; ADocumentLevel lets a relative path start
  at the root. }
procedure SelectNodes(AContext: TRALXMLNode; const APath: StringRAL; AOut: TList;
  ADocumentLevel: boolean);
var
  vSteps: TRALXMLPathSteps;
  vAbsolute: boolean;
  vCurrent, vNext: TList;
  vInt, vNode: IntegerRAL;
  vStart, vRoot: TRALXMLNode;
  vParts: TStringList;
  vName: StringRAL;
  vChild: TRALXMLNode;
begin
  if APath = '' then
    Exit;

  if not IsXPath(APath) then
  begin
    // the dotted form of xmlpascal: child names, the root name possibly first
    vParts := TStringList.Create;
    try
      vParts.Delimiter := '.';
      vParts.StrictDelimiter := True;
      vParts.DelimitedText := string(APath);
      vStart := AContext;
      if ADocumentLevel then
      begin
        vStart := AContext.Document.Root;
        if vStart = nil then
          Exit;
        if (vParts.Count > 0) and (StringRAL(vParts[0]) = vStart.NodeName) then
          vParts.Delete(0);
        if vParts.Count = 0 then
        begin
          AOut.Add(vStart);
          Exit;
        end;
      end;
      for vInt := 0 to vParts.Count - 2 do
      begin
        if vParts[vInt] = '' then
          Continue;
        vStart := vStart.Child(StringRAL(vParts[vInt]));
        if vStart = nil then
          Exit;
      end;
      vName := StringRAL(vParts[vParts.Count - 1]);
      vChild := vStart.FirstChild;
      while vChild <> nil do
      begin
        if NameMatches(vChild, vName) then
          AOut.Add(vChild);
        vChild := vChild.NextSibling;
      end;
    finally
      vParts.Free;
    end;
    Exit;
  end;

  vSteps := ParsePath(APath, vAbsolute);
  if Length(vSteps) = 0 then
    Exit;

  if vAbsolute then
    vStart := AContext.Document.DocumentNode
  else
    vStart := AContext;

  vCurrent := TList.Create;
  vNext := TList.Create;
  try
    vCurrent.Add(vStart);
    for vInt := 0 to High(vSteps) do
    begin
      vNext.Clear;
      for vNode := 0 to vCurrent.Count - 1 do
        ApplyStep(TRALXMLNode(vCurrent[vNode]), vSteps[vInt], vNext);
      vCurrent.Assign(vNext);

      // a relative path from the document may name the root or start below it
      if (vInt = 0) and (vCurrent.Count = 0) and ADocumentLevel and not vAbsolute then
      begin
        vRoot := AContext.Document.Root;
        if vRoot <> nil then
          ApplyStep(vRoot, vSteps[0], vCurrent);
      end;
      if vCurrent.Count = 0 then
        Break;
    end;
    for vInt := 0 to vCurrent.Count - 1 do
      AOut.Add(vCurrent[vInt]);
  finally
    vCurrent.Free;
    vNext.Free;
  end;
end;

/// Splits the attribute a path ends in: "a/b/@@c" or, dotted, the last segment
procedure SplitAttributePath(const APath: StringRAL; out ANodePath, AAttribute: StringRAL);
var
  vPos: IntegerRAL;
begin
  vPos := Length(APath);
  if IsXPath(APath) then
  begin
    while (vPos > 0) and (APath[vPos] <> '@') do
      Dec(vPos);
    if (vPos > 0) and ((vPos = 1) or (APath[vPos - 1] = '/')) and
       (Pos(StringRAL(']'), Copy(APath, vPos, MaxInt)) = 0) then
    begin
      AAttribute := Copy(APath, vPos + 1, MaxInt);
      ANodePath := Copy(APath, 1, vPos - 1);
      if (ANodePath <> '') and (ANodePath[Length(ANodePath)] = '/') then
        ANodePath := Copy(ANodePath, 1, Length(ANodePath) - 1);
      if ANodePath = '' then
        ANodePath := '.';
      Exit;
    end;
    ANodePath := APath;
    AAttribute := '';
  end
  else
  begin
    while (vPos > 0) and (APath[vPos] <> '.') do
      Dec(vPos);
    AAttribute := Copy(APath, vPos + 1, MaxInt);
    ANodePath := Copy(APath, 1, vPos - 1);
  end;
end;

{ TRALXMLNode }

constructor TRALXMLNode.Create(ADocument: TRALXMLDocument; AKind: TRALXMLNodeKind;
  const AName: StringRAL);
begin
  inherited Create;
  FDocument := ADocument;
  FKind := AKind;
  FName := Intern(AName);
end;

destructor TRALXMLNode.Destroy;
begin
  if FParent <> nil then
    Unlink;
  FreeChildren;
  inherited Destroy;
end;

function TRALXMLNode.Intern(const AName: StringRAL): StringRAL;
begin
  if (FDocument <> nil) and (AName <> '') then
    Result := FDocument.NameTable.Add(AName)
  else
    Result := AName;
end;

procedure TRALXMLNode.FreeChildren;
var
  vNode, vNext, vParent: TRALXMLNode;
begin
  // post-order, without recursion: a deep tree does not exhaust the stack
  vNode := FFirstChild;
  while vNode <> nil do
  begin
    while vNode.FFirstChild <> nil do
      vNode := vNode.FFirstChild;
    vNext := vNode.FNextSibling;
    vParent := vNode.FParent;
    vParent.FFirstChild := vNext;
    if vNext = nil then
      vParent.FLastChild := nil
    else
      vNext.FPreviousSibling := nil;
    vNode.FParent := nil;
    vNode.FNextSibling := nil;
    vNode.Free;

    if vNext <> nil then
      vNode := vNext
    else if vParent = Self then
      vNode := nil
    else
      vNode := vParent;
  end;
  FFirstChild := nil;
  FLastChild := nil;
end;

procedure TRALXMLNode.Unlink;
begin
  if FParent = nil then
    Exit;
  if FPreviousSibling <> nil then
    FPreviousSibling.FNextSibling := FNextSibling
  else
    FParent.FFirstChild := FNextSibling;
  if FNextSibling <> nil then
    FNextSibling.FPreviousSibling := FPreviousSibling
  else
    FParent.FLastChild := FPreviousSibling;
  FParent := nil;
  FNextSibling := nil;
  FPreviousSibling := nil;
end;

procedure TRALXMLNode.Link(AChild, ABefore: TRALXMLNode);
var
  vNode: TRALXMLNode;
begin
  if AChild = nil then
    Exit;
  // a node cannot become a child of itself or of one of its descendants
  vNode := Self;
  while vNode <> nil do
  begin
    if vNode = AChild then
      raise ERALXMLError.CreateAt(emXMLHierarchy, 0, 0);
    vNode := vNode.FParent;
  end;

  AChild.Unlink;
  AChild.FParent := Self;
  if ABefore = nil then
  begin
    AChild.FPreviousSibling := FLastChild;
    if FLastChild <> nil then
      FLastChild.FNextSibling := AChild
    else
      FFirstChild := AChild;
    FLastChild := AChild;
  end
  else
  begin
    if ABefore.FParent <> Self then
      raise ERALXMLError.CreateAt(emXMLHierarchy, 0, 0);
    AChild.FNextSibling := ABefore;
    AChild.FPreviousSibling := ABefore.FPreviousSibling;
    if ABefore.FPreviousSibling <> nil then
      ABefore.FPreviousSibling.FNextSibling := AChild
    else
      FFirstChild := AChild;
    ABefore.FPreviousSibling := AChild;
  end;
  AChild.ResetNamespaces;
end;

procedure TRALXMLNode.ResetNamespaces;
var
  vNode: TRALXMLNode;
begin
  // a moved subtree resolves its prefixes again where it now stands
  FNsResolved := False;
  vNode := FFirstChild;
  while vNode <> nil do
  begin
    if vNode.FKind = xnkElement then
      vNode.ResetNamespaces;
    vNode := vNode.FNextSibling;
  end;
end;

function TRALXMLNode.AttrIndex(const AName: StringRAL): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FAttrCount - 1 do
    if FAttributes[vInt].Name = AName then
    begin
      Result := vInt;
      Exit;
    end;
  Result := -1;
end;

function TRALXMLNode.AddAttribute(const AName, AValue: StringRAL): TRALXMLNode;
var
  vIdx: IntegerRAL;
begin
  Result := Self;
  vIdx := AttrIndex(AName);
  if vIdx >= 0 then
  begin
    FAttributes[vIdx].Value := AValue;
    Exit;
  end;
  if FAttrCount >= Length(FAttributes) then
    SetLength(FAttributes, FAttrCount * 2 + 2);
  FAttributes[FAttrCount].Name := Intern(AName);
  FAttributes[FAttrCount].Value := AValue;
  Inc(FAttrCount);
  if (Copy(AName, 1, 5) = 'xmlns') then
    ResetNamespaces;
end;

function TRALXMLNode.AddNamespace(const APrefix, AURI: StringRAL): TRALXMLNode;
begin
  if APrefix = '' then
    Result := AddAttribute('xmlns', AURI)
  else
    Result := AddAttribute('xmlns:' + APrefix, AURI);
end;

function TRALXMLNode.RemoveAttribute(const AName: StringRAL): TRALXMLNode;
begin
  DeleteAttribute(AName);
  Result := Self;
end;

procedure TRALXMLNode.DeleteAttribute(const AName: StringRAL);
var
  vIdx, vInt: IntegerRAL;
begin
  vIdx := AttrIndex(AName);
  if vIdx < 0 then
    Exit;
  for vInt := vIdx to FAttrCount - 2 do
    FAttributes[vInt] := FAttributes[vInt + 1];
  Dec(FAttrCount);
  FAttributes[FAttrCount].Name := '';
  FAttributes[FAttrCount].Value := '';
  if (Copy(AName, 1, 5) = 'xmlns') then
    ResetNamespaces;
end;

function TRALXMLNode.Attribute(const AName: StringRAL): StringRAL;
begin
  Result := GetAttributeValue(AName);
end;

function TRALXMLNode.AttributeName(AIndex: IntegerRAL): StringRAL;
begin
  if (AIndex >= 0) and (AIndex < FAttrCount) then
    Result := FAttributes[AIndex].Name
  else
    Result := '';
end;

function TRALXMLNode.AttributeNS(const AURI, ALocalName: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
  vPrefix, vLocal: StringRAL;
begin
  Result := '';
  for vInt := 0 to FAttrCount - 1 do
  begin
    SplitQName(FAttributes[vInt].Name, vPrefix, vLocal);
    if vLocal <> ALocalName then
      Continue;
    if ((vPrefix = '') and (AURI = '')) or
       ((vPrefix <> '') and (LookupNamespaceURI(vPrefix) = AURI)) then
    begin
      Result := FAttributes[vInt].Value;
      Exit;
    end;
  end;
end;

function TRALXMLNode.GetAttributeValue(const AName: StringRAL): StringRAL;
var
  vIdx: IntegerRAL;
begin
  vIdx := AttrIndex(AName);
  if vIdx >= 0 then
    Result := FAttributes[vIdx].Value
  else
    Result := '';
end;

procedure TRALXMLNode.SetAttributeValue(const AName, AValue: StringRAL);
begin
  AddAttribute(AName, AValue);
end;

function TRALXMLNode.HasAttribute(const AName: StringRAL): boolean;
begin
  Result := AttrIndex(AName) >= 0;
end;

function TRALXMLNode.AppendChild(ANode: TRALXMLNode): TRALXMLNode;
begin
  Link(ANode, nil);
  Result := ANode;
end;

function TRALXMLNode.InsertBefore(ANode, ARef: TRALXMLNode): TRALXMLNode;
begin
  Link(ANode, ARef);
  Result := ANode;
end;

function TRALXMLNode.Detach: TRALXMLNode;
begin
  Unlink;
  Result := Self;
end;

function TRALXMLNode.AddChild(const AName: StringRAL): TRALXMLNode;
begin
  if not RALXMLIsName(AName) then
    raise ERALXMLError.CreateAt(Format(emXMLInvalidName, [string(AName)]), 0, 0);
  Result := TRALXMLNode.Create(FDocument, xnkElement, AName);
  Link(Result, nil);
end;

function TRALXMLNode.AddText(const AValue: StringRAL): TRALXMLNode;
var
  vNode: TRALXMLNode;
begin
  Result := Self;
  if AValue = '' then
    Exit;
  // adjacent text is one node
  if (FLastChild <> nil) and (FLastChild.FKind = xnkText) then
  begin
    FLastChild.FValue := FLastChild.FValue + AValue;
    Exit;
  end;
  vNode := TRALXMLNode.Create(FDocument, xnkText);
  vNode.FValue := AValue;
  Link(vNode, nil);
end;

function TRALXMLNode.AddCData(const AValue: StringRAL): TRALXMLNode;
var
  vNode: TRALXMLNode;
begin
  vNode := TRALXMLNode.Create(FDocument, xnkCData);
  vNode.FValue := AValue;
  Link(vNode, nil);
  Result := Self;
end;

function TRALXMLNode.AddComment(const AValue: StringRAL): TRALXMLNode;
var
  vNode: TRALXMLNode;
begin
  vNode := TRALXMLNode.Create(FDocument, xnkComment);
  vNode.FValue := AValue;
  Link(vNode, nil);
  Result := Self;
end;

function TRALXMLNode.AddProcessingInstruction(const ATarget, AData: StringRAL): TRALXMLNode;
var
  vNode: TRALXMLNode;
begin
  vNode := TRALXMLNode.Create(FDocument, xnkProcessingInstruction, ATarget);
  vNode.FValue := AData;
  Link(vNode, nil);
  Result := Self;
end;

function TRALXMLNode.AddXML(const AXML: StringRAL): TRALXMLNode;
var
  vDoc: TRALXMLDocument;
  vNode, vNext: TRALXMLNode;
begin
  Result := Self;
  // parsed inside a wrapper that declares the namespaces in scope here
  vDoc := TRALXMLDocument.Create;
  try
    if FDocument <> nil then
    begin
      vDoc.Options.Assign(FDocument.Options);
      vDoc.PreserveWhitespace := FDocument.PreserveWhitespace;
    end;
    vDoc.LoadFromString('<ralfragment>' + AXML + '</ralfragment>');
    vNode := vDoc.Root.FirstChild;
    while vNode <> nil do
    begin
      vNext := vNode.NextSibling;
      AppendChild(vNode.CloneInto(FDocument, True));
      vNode := vNext;
    end;
  finally
    vDoc.Free;
  end;
end;

function TRALXMLNode.Child(const AName: StringRAL): TRALXMLNode;
begin
  Result := FFirstChild;
  while Result <> nil do
  begin
    if NameMatches(Result, AName) then
      Exit;
    Result := Result.FNextSibling;
  end;
end;

function TRALXMLNode.ChildNS(const AURI, ALocalName: StringRAL): TRALXMLNode;
begin
  Result := FFirstChild;
  while Result <> nil do
  begin
    if (Result.FKind = xnkElement) and (Result.LocalName = ALocalName) and
       (Result.NamespaceURI = AURI) then
      Exit;
    Result := Result.FNextSibling;
  end;
end;

function TRALXMLNode.HasChild(const AName: StringRAL): boolean;
begin
  Result := Child(AName) <> nil;
end;

function TRALXMLNode.FirstElement: TRALXMLNode;
begin
  Result := FFirstChild;
  while (Result <> nil) and (Result.FKind <> xnkElement) do
    Result := Result.FNextSibling;
end;

function TRALXMLNode.NextElement: TRALXMLNode;
begin
  Result := FNextSibling;
  while (Result <> nil) and (Result.FKind <> xnkElement) do
    Result := Result.FNextSibling;
end;

function TRALXMLNode.ElementCount: IntegerRAL;
var
  vNode: TRALXMLNode;
begin
  Result := 0;
  vNode := FFirstChild;
  while vNode <> nil do
  begin
    if vNode.FKind = xnkElement then
      Inc(Result);
    vNode := vNode.FNextSibling;
  end;
end;

function TRALXMLNode.GetChildCount: IntegerRAL;
var
  vNode: TRALXMLNode;
begin
  Result := 0;
  vNode := FFirstChild;
  while vNode <> nil do
  begin
    Inc(Result);
    vNode := vNode.FNextSibling;
  end;
end;

function TRALXMLNode.GetChildren(AIndex: IntegerRAL): TRALXMLNode;
begin
  Result := FFirstChild;
  while (Result <> nil) and (AIndex > 0) do
  begin
    Result := Result.FNextSibling;
    Dec(AIndex);
  end;
  if AIndex < 0 then
    Result := nil;
end;

procedure TRALXMLNode.Clear;
begin
  FreeChildren;
  FAttrCount := 0;
  FAttributes := nil;
  if FKind <> xnkElement then
    FValue := '';
end;

procedure TRALXMLNode.RemoveChild(const AChild: TRALXMLNode);
begin
  if (AChild = nil) or (AChild.FParent <> Self) then
    Exit;
  AChild.Free;
end;

function TRALXMLNode.Clone(ADeep: boolean): TRALXMLNode;
begin
  Result := CloneInto(FDocument, ADeep);
end;

function TRALXMLNode.CloneInto(ADocument: TRALXMLDocument; ADeep: boolean): TRALXMLNode;
var
  vChild: TRALXMLNode;
  vInt: IntegerRAL;
begin
  Result := TRALXMLNode.Create(ADocument, FKind, FName);
  Result.FValue := FValue;
  Result.FAttrCount := FAttrCount;
  SetLength(Result.FAttributes, FAttrCount);
  for vInt := 0 to FAttrCount - 1 do
  begin
    Result.FAttributes[vInt].Name := Result.Intern(FAttributes[vInt].Name);
    Result.FAttributes[vInt].Value := FAttributes[vInt].Value;
  end;
  if not ADeep then
    Exit;
  vChild := FFirstChild;
  while vChild <> nil do
  begin
    Result.Link(vChild.CloneInto(ADocument, True), nil);
    vChild := vChild.FNextSibling;
  end;
end;

function TRALXMLNode.GetOwnText: StringRAL;
var
  vNode: TRALXMLNode;
begin
  if FKind <> xnkElement then
  begin
    if FKind = xnkDocument then
      Result := GetInnerText
    else
      Result := FValue;
    Exit;
  end;
  Result := '';
  vNode := FFirstChild;
  while vNode <> nil do
  begin
    if vNode.FKind in [xnkText, xnkCData] then
      Result := Result + vNode.FValue;
    vNode := vNode.FNextSibling;
  end;
end;

procedure TRALXMLNode.SetText(const AValue: StringRAL);
begin
  if FKind <> xnkElement then
  begin
    FValue := AValue;
    Exit;
  end;
  FreeChildren;
  AddText(AValue);
end;

function TRALXMLNode.GetInnerText: StringRAL;
var
  vNode: TRALXMLNode;
begin
  if FKind in [xnkText, xnkCData] then
  begin
    Result := FValue;
    Exit;
  end;
  Result := '';
  vNode := FFirstChild;
  while vNode <> nil do
  begin
    if vNode.FKind in [xnkText, xnkCData] then
      Result := Result + vNode.FValue;
    if vNode.FFirstChild <> nil then
      vNode := vNode.FFirstChild
    else
    begin
      while (vNode <> Self) and (vNode.FNextSibling = nil) do
        vNode := vNode.FParent;
      if vNode = Self then
        vNode := nil
      else
        vNode := vNode.FNextSibling;
    end;
  end;
end;

procedure TRALXMLNode.SetNodeName(const AValue: StringRAL);
begin
  if (FKind = xnkElement) and not RALXMLIsName(AValue) then
    raise ERALXMLError.CreateAt(Format(emXMLInvalidName, [string(AValue)]), 0, 0);
  FName := Intern(AValue);
  FNsResolved := False;
end;

function TRALXMLNode.GetLocalName: StringRAL;
var
  vPrefix: StringRAL;
begin
  SplitQName(FName, vPrefix, Result);
end;

function TRALXMLNode.GetPrefix: StringRAL;
var
  vLocal: StringRAL;
begin
  SplitQName(FName, Result, vLocal);
end;

function TRALXMLNode.LookupNamespaceURI(const APrefix: StringRAL): StringRAL;
var
  vNode: TRALXMLNode;
  vAttr: StringRAL;
  vIdx: IntegerRAL;
begin
  Result := '';
  if APrefix = 'xml' then
  begin
    Result := RALXMLNamespaceXML;
    Exit;
  end;
  if APrefix = '' then
    vAttr := 'xmlns'
  else
    vAttr := 'xmlns:' + APrefix;
  vNode := Self;
  while vNode <> nil do
  begin
    if vNode.FKind = xnkElement then
    begin
      vIdx := vNode.AttrIndex(vAttr);
      if vIdx >= 0 then
      begin
        Result := vNode.FAttributes[vIdx].Value;
        Exit;
      end;
    end;
    vNode := vNode.FParent;
  end;
end;

function TRALXMLNode.LookupPrefix(const AURI: StringRAL): StringRAL;
var
  vNode: TRALXMLNode;
  vInt: IntegerRAL;
begin
  Result := '';
  vNode := Self;
  while vNode <> nil do
  begin
    if vNode.FKind = xnkElement then
      for vInt := 0 to vNode.FAttrCount - 1 do
        if (vNode.FAttributes[vInt].Value = AURI) and
           (Copy(vNode.FAttributes[vInt].Name, 1, 6) = 'xmlns:') then
        begin
          Result := Copy(vNode.FAttributes[vInt].Name, 7, MaxInt);
          Exit;
        end;
    vNode := vNode.FParent;
  end;
end;

function TRALXMLNode.GetNamespaceURI: StringRAL;
begin
  if FKind <> xnkElement then
  begin
    Result := '';
    Exit;
  end;
  if not FNsResolved then
  begin
    FNamespaceURI := LookupNamespaceURI(GetPrefix);
    // a parsed or resolved node keeps it until it moves or its declarations change
    FNsResolved := (FDocument <> nil) and (FParent <> nil);
    if FNsResolved and (FDocument <> nil) then
      FNamespaceURI := FDocument.NameTable.Add(FNamespaceURI);
  end;
  Result := FNamespaceURI;
end;

function TRALXMLNode.AsString: StringRAL;
begin
  Result := GetOwnText;
end;

function TRALXMLNode.AsInteger: IntegerRAL;
var
  vValue: IntegerRAL;
begin
  if not TryStrToInt(string(RALTrim(GetOwnText)), vValue) then
    raise EConvertError.Create(Format(emXMLValueType, [string(GetOwnText), 'integer']));
  Result := vValue;
end;

function TRALXMLNode.AsInt64: Int64RAL;
var
  vValue: Int64;
begin
  if not TryStrToInt64(string(RALTrim(GetOwnText)), vValue) then
    raise EConvertError.Create(Format(emXMLValueType, [string(GetOwnText), 'long']));
  Result := vValue;
end;

function TRALXMLNode.AsFloat: Double;
var
  vText: StringRAL;
begin
  vText := RALTrim(GetOwnText);
  if vText = 'INF' then
    Result := Infinity
  else if vText = '-INF' then
    Result := NegInfinity
  else if vText = 'NaN' then
    Result := NaN
  else if not TryStrToFloat(string(vText), Result, RALInvariantFormat) then
    raise EConvertError.Create(Format(emXMLValueType, [string(vText), 'double']));
end;

function TRALXMLNode.AsBoolean: boolean;
var
  vText: StringRAL;
begin
  vText := RALTrim(GetOwnText);
  if (vText = 'true') or (vText = '1') then
    Result := True
  else if (vText = 'false') or (vText = '0') then
    Result := False
  else
    raise EConvertError.Create(Format(emXMLValueType, [string(vText), 'boolean']));
end;

function TRALXMLNode.AsDateTime: TDateTime;
begin
  if not RALTryISO8601ToDateTime(RALTrim(GetOwnText), Result) then
    raise EConvertError.Create(Format(emXMLValueType, [string(GetOwnText), 'dateTime']));
end;

function TRALXMLNode.AsBCD: TBcd;
begin
  if not RALTryTextToBCD(RALTrim(GetOwnText), Result) then
    raise EConvertError.Create(Format(emXMLValueType, [string(GetOwnText), 'decimal']));
end;

function TRALXMLNode.AsBase64: TBytes;
var
  vText: StringRAL;
begin
  vText := RALTrim(GetOwnText);
  if vText = '' then
    Result := nil
  else
    Result := TRALBase64.DecodeAsBytes(vText);
end;

function TRALXMLNode.WithText(const AText: StringRAL): TRALXMLNode;
begin
  SetText(AText);
  Result := Self;
end;

function TRALXMLNode.WithCData(const AValue: StringRAL): TRALXMLNode;
begin
  FreeChildren;
  AddCData(AValue);
  Result := Self;
end;

function TRALXMLNode.WithValue(AValue: Int64RAL): TRALXMLNode;
begin
  Result := WithText(StringRAL(IntToStr(AValue)));
end;

function TRALXMLNode.WithValue(AValue: Double): TRALXMLNode;
begin
  // xsd:double: "." for the decimals, INF, -INF and NaN for what is not a number
  if IsNan(AValue) then
    Result := WithText('NaN')
  else if IsInfinite(AValue) then
  begin
    if AValue > 0 then
      Result := WithText('INF')
    else
      Result := WithText('-INF');
  end
  else
    Result := WithText(StringRAL(FloatToStr(AValue, RALInvariantFormat)));
end;

function TRALXMLNode.WithValue(AValue: boolean): TRALXMLNode;
begin
  if AValue then
    Result := WithText('true')
  else
    Result := WithText('false');
end;

function TRALXMLNode.WithDateTime(AValue: TDateTime; AInputIsUTC: boolean): TRALXMLNode;
begin
  Result := WithText(RALDateTimeToISO8601(AValue, AInputIsUTC));
end;

function TRALXMLNode.WithBCD(const AValue: TBcd): TRALXMLNode;
begin
  Result := WithText(RALBCDToText(AValue));
end;

function TRALXMLNode.WithBase64(const AValue: TBytes): TRALXMLNode;
begin
  if Length(AValue) = 0 then
    Result := WithText('')
  else
    Result := WithText(TRALBase64.Encode(AValue));
end;

function TRALXMLNode.GetNode(const APath: StringRAL): TRALXMLNode;
var
  vList: TList;
begin
  Result := nil;
  vList := TList.Create;
  try
    SelectNodes(Self, APath, vList, FKind = xnkDocument);
    if vList.Count > 0 then
      Result := TRALXMLNode(vList[0]);
  finally
    vList.Free;
  end;
end;

function TRALXMLNode.GetNodes(const APath: StringRAL): TRALXMLNodeList;
begin
  Result := TRALXMLNodeList.Create;
  try
    SelectNodes(Self, APath, Result.List, FKind = xnkDocument);
  except
    Result.Free;
    raise;
  end;
end;

function TRALXMLNode.GetText(const APath: StringRAL): StringRAL;
var
  vNodePath, vAttr: StringRAL;
  vNode: TRALXMLNode;
begin
  Result := '';
  if IsXPath(APath) then
  begin
    SplitAttributePath(APath, vNodePath, vAttr);
    if vAttr <> '' then
    begin
      vNode := GetNode(vNodePath);
      if vNode <> nil then
        Result := vNode.Attribute(vAttr);
      Exit;
    end;
  end;
  vNode := GetNode(APath);
  if vNode <> nil then
    Result := vNode.Text;
end;

function TRALXMLNode.GetAttribute(const APath: StringRAL): StringRAL;
var
  vNodePath, vAttr: StringRAL;
  vNode: TRALXMLNode;
begin
  Result := '';
  SplitAttributePath(APath, vNodePath, vAttr);
  if vNodePath = '' then
  begin
    if FKind = xnkDocument then
      vNode := FDocument.Root
    else
      vNode := Self;
  end
  else
    vNode := GetNode(vNodePath);
  if vNode <> nil then
    Result := vNode.Attribute(vAttr);
end;

/// Writes ANode, itself too when ASelfToo, with AWriter, iteratively.
procedure WriteNode(AWriter: TRALXMLWriter; ANode: TRALXMLNode; ASelfToo: boolean);
var
  vNode, vStop: TRALXMLNode;

  procedure Open(N: TRALXMLNode);
  var
    vIdx: IntegerRAL;
  begin
    case N.FKind of
      xnkElement:
        begin
          AWriter.StartElement(N.FName);
          for vIdx := 0 to N.FAttrCount - 1 do
            AWriter.Attribute(N.FAttributes[vIdx].Name, N.FAttributes[vIdx].Value);
        end;
      xnkText:
        AWriter.Text(N.FValue);
      xnkCData:
        AWriter.CData(N.FValue);
      xnkComment:
        AWriter.Comment(N.FValue);
      xnkProcessingInstruction:
        AWriter.ProcessingInstruction(N.FName, N.FValue);
      xnkDocType:
        AWriter.DocType(N.FName, N.Attribute('public'), N.Attribute('system'), N.FValue);
    end;
  end;

  procedure Close(N: TRALXMLNode);
  begin
    if N.FKind = xnkElement then
      AWriter.EndElement;
  end;

begin
  // iterative depth-first walk: open on the way down, close on the way up
  if ASelfToo and (ANode.FKind <> xnkDocument) then
  begin
    Open(ANode);
    vStop := ANode;
  end
  else
    vStop := ANode;

  vNode := ANode.FFirstChild;
  while vNode <> nil do
  begin
    Open(vNode);
    if vNode.FFirstChild <> nil then
    begin
      vNode := vNode.FFirstChild;
      Continue;
    end;
    Close(vNode);
    while (vNode <> vStop) and (vNode.FNextSibling = nil) do
    begin
      vNode := vNode.FParent;
      if vNode <> vStop then
        Close(vNode);
    end;
    if vNode = vStop then
      vNode := nil
    else
      vNode := vNode.FNextSibling;
  end;

  if ASelfToo and (ANode.FKind <> xnkDocument) then
    Close(ANode);
end;

procedure TRALXMLNode.SaveToStream(AStream: TStream; AIndent: boolean);
var
  vWriter: TRALXMLWriter;
begin
  vWriter := TRALXMLWriter.Create(AStream, AIndent);
  try
    WriteNode(vWriter, Self, True);
  finally
    vWriter.Free;
  end;
end;

function TRALXMLNode.ToXML(AIndent: boolean; ALevel: IntegerRAL): StringRAL;
var
  vStream: TBytesStream;
  vText: StringRAL;
  vInt, vPos, vLines, vPad: IntegerRAL;
begin
  vStream := TBytesStream.Create;
  try
    SaveToStream(vStream, AIndent);
    Result := RALXMLMakeString(PByte(vStream.Bytes), vStream.Size);
  finally
    vStream.Free;
  end;

  if AIndent and (ALevel > 0) and (Result <> '') then
  begin
    // every line moves right by two blanks per level
    vPad := ALevel * 2;
    vLines := 1;
    for vInt := 1 to Length(Result) do
      if Result[vInt] = #10 then
        Inc(vLines);
    vText := Result;
    SetLength(Result, Length(vText) + vLines * vPad);
    FillChar(Result[1], vPad, Ord(' '));
    vPos := vPad + 1;
    for vInt := 1 to Length(vText) do
    begin
      Result[vPos] := vText[vInt];
      Inc(vPos);
      if vText[vInt] = #10 then
      begin
        FillChar(Result[vPos], vPad, Ord(' '));
        Inc(vPos, vPad);
      end;
    end;
  end;
end;

function TRALXMLNode.AppendDataSet(ADataSet: TDataSet; AXMLType: TRALXMLType): TRALXMLNode;
var
  vStream: TMemoryStream;
  vDoc: TRALXMLDocument;
begin
  Result := Self;
  vStream := TMemoryStream.Create;
  try
    RALStorageXMLSave(ADataSet, vStream, AXMLType);
    vDoc := TRALXMLDocument.Create;
    try
      vDoc.LoadFromStream(vStream);
      AppendChild(vDoc.Root.CloneInto(FDocument, True));
    finally
      vDoc.Free;
    end;
  finally
    vStream.Free;
  end;
end;

procedure TRALXMLNode.ReadDataSet(ADataSet: TDataSet);
var
  vStream: TMemoryStream;
  vNode: TRALXMLNode;
begin
  // the storage is this node itself when it is one, else its first element child
  vNode := Self;
  if (FKind <> xnkElement) or not RALStorageXMLIsRoot(FName) then
    vNode := FirstElement;
  if vNode = nil then
    raise ERALXMLError.CreateAt(emXMLNoDataSet, 0, 0);
  vStream := TMemoryStream.Create;
  try
    vNode.SaveToStream(vStream, False);
    vStream.Position := 0;
    RALStorageXMLLoad(ADataSet, vStream);
  finally
    vStream.Free;
  end;
end;

{ TRALXMLDocument }

constructor TRALXMLDocument.Create;
begin
  inherited Create;
  FNames := TRALXMLNameTable.Create;
  FOptions := TRALXMLReaderOptions.Create;
  FDocumentNode := TRALXMLNode.Create(Self, xnkDocument);
  FVersion := '1.0';
  FEncoding := 'UTF-8';
end;

destructor TRALXMLDocument.Destroy;
begin
  FreeAndNil(FDocumentNode);
  FreeAndNil(FOptions);
  FreeAndNil(FNames);
  inherited Destroy;
end;

procedure TRALXMLDocument.Clear;
begin
  FDocumentNode.Clear;
  FVersion := '1.0';
  FEncoding := 'UTF-8';
  FStandalone := '';
end;

function TRALXMLDocument.GetRoot: TRALXMLNode;
begin
  Result := FDocumentNode.FirstElement;
end;

function TRALXMLDocument.CreateElement(const AName: StringRAL): TRALXMLNode;
begin
  if not RALXMLIsName(AName) then
    raise ERALXMLError.CreateAt(Format(emXMLInvalidName, [string(AName)]), 0, 0);
  Result := TRALXMLNode.Create(Self, xnkElement, AName);
end;

function TRALXMLDocument.CreateRoot(const ARootName: StringRAL): TRALXMLNode;
var
  vOld: TRALXMLNode;
begin
  vOld := GetRoot;
  Result := CreateElement(ARootName);
  if vOld <> nil then
  begin
    FDocumentNode.InsertBefore(Result, vOld);
    vOld.Free;
  end
  else
    FDocumentNode.AppendChild(Result);
end;

procedure TRALXMLDocument.Parse(AReader: TRALXMLReader);
var
  vCurrent, vNode: TRALXMLNode;
  vInt: IntegerRAL;
  vAttr: TRALXMLAttribute;
begin
  FDocumentNode.Clear;
  vCurrent := FDocumentNode;
  while AReader.Read do
  begin
    case AReader.Token of
      xtkStartElement:
        begin
          vNode := TRALXMLNode.Create(Self, xnkElement);
          vNode.FName := AReader.Name;
          vNode.FAttrCount := AReader.AttributeCount;
          SetLength(vNode.FAttributes, AReader.AttributeCount);
          for vInt := 0 to AReader.AttributeCount - 1 do
          begin
            vAttr := RALXMLReaderAttribute(AReader, vInt);
            vNode.FAttributes[vInt].Name := vAttr.Name;
            vNode.FAttributes[vInt].Value := vAttr.Value;
          end;
          // linked by hand: the reader already resolved the namespace
          vNode.FParent := vCurrent;
          vNode.FPreviousSibling := vCurrent.FLastChild;
          if vCurrent.FLastChild <> nil then
            vCurrent.FLastChild.FNextSibling := vNode
          else
            vCurrent.FFirstChild := vNode;
          vCurrent.FLastChild := vNode;
          vNode.FNamespaceURI := AReader.NamespaceURI;
          vNode.FNsResolved := AReader.Options.Namespaces;
          vCurrent := vNode;
        end;
      xtkEndElement:
        vCurrent := vCurrent.FParent;
      xtkText:
        if FPreserveWhitespace or not AReader.IsWhitespace then
          vCurrent.AddText(AReader.Value);
      xtkCData:
        vCurrent.AddCData(AReader.Value);
      xtkComment:
        vCurrent.AddComment(AReader.Value);
      xtkProcessingInstruction:
        vCurrent.AddProcessingInstruction(AReader.Name, AReader.Value);
      xtkDocType:
        begin
          vNode := TRALXMLNode.Create(Self, xnkDocType, AReader.DocTypeName);
          vNode.FValue := AReader.Value;
          if AReader.DocTypePublicId <> '' then
            vNode.AddAttribute('public', AReader.DocTypePublicId);
          if AReader.DocTypeSystemId <> '' then
            vNode.AddAttribute('system', AReader.DocTypeSystemId);
          vCurrent.Link(vNode, nil);
        end;
    end;
  end;
  FVersion := AReader.XMLVersion;
  FEncoding := AReader.XMLEncoding;
  FStandalone := AReader.Standalone;
end;

procedure TRALXMLDocument.LoadFromBuffer(AData: Pointer; ASize: IntegerRAL;
  const ACharset: StringRAL);
var
  vReader: TRALXMLReader;
begin
  vReader := TRALXMLReader.Create(FNames, FOptions);
  try
    vReader.SetInput(AData, ASize, ACharset);
    Parse(vReader);
  finally
    vReader.Free;
  end;
end;

procedure TRALXMLDocument.LoadFromString(const AXML: StringRAL; const ACharset: StringRAL);
var
  vReader: TRALXMLReader;
begin
  vReader := TRALXMLReader.Create(FNames, FOptions);
  try
    vReader.SetInputString(AXML, ACharset);
    Parse(vReader);
  finally
    vReader.Free;
  end;
end;

procedure TRALXMLDocument.LoadFromStream(AStream: TStream; const ACharset: StringRAL);
var
  vReader: TRALXMLReader;
begin
  vReader := TRALXMLReader.Create(FNames, FOptions);
  try
    vReader.SetInputStream(AStream, ACharset);
    Parse(vReader);
  finally
    vReader.Free;
  end;
end;

procedure TRALXMLDocument.LoadFromFile(const AFileName: string);
var
  vStream: TMemoryStream;
begin
  vStream := TMemoryStream.Create;
  try
    vStream.LoadFromFile(AFileName);
    LoadFromStream(vStream);
  finally
    vStream.Free;
  end;
end;

procedure TRALXMLDocument.SaveToStream(AStream: TStream; AAllowHeader: boolean);
var
  vWriter: TRALXMLWriter;
begin
  vWriter := TRALXMLWriter.Create(AStream, FIndent);
  try
    if AAllowHeader then
      vWriter.WriteDeclaration(FStandalone);
    WriteNode(vWriter, FDocumentNode, False);
  finally
    vWriter.Free;
  end;
end;

function TRALXMLDocument.SaveToString(AAllowHeader: boolean): StringRAL;
var
  vStream: TBytesStream;
begin
  vStream := TBytesStream.Create;
  try
    SaveToStream(vStream, AAllowHeader);
    Result := RALXMLMakeString(PByte(vStream.Bytes), vStream.Size);
  finally
    vStream.Free;
  end;
end;

procedure TRALXMLDocument.SaveToFile(const AFileName: string; AAllowHeader: boolean);
var
  vStream: TFileStream;
begin
  vStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(vStream, AAllowHeader);
  finally
    vStream.Free;
  end;
end;

function TRALXMLDocument.GetNode(const APath: StringRAL): TRALXMLNode;
begin
  Result := FDocumentNode.GetNode(APath);
end;

function TRALXMLDocument.GetNodes(const APath: StringRAL): TRALXMLNodeList;
begin
  Result := FDocumentNode.GetNodes(APath);
end;

function TRALXMLDocument.GetText(const APath: StringRAL): StringRAL;
begin
  Result := FDocumentNode.GetText(APath);
end;

function TRALXMLDocument.GetAttribute(const APath: StringRAL): StringRAL;
begin
  Result := FDocumentNode.GetAttribute(APath);
end;

end.
