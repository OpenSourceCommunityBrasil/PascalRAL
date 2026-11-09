/// Canonical XML of RAL: C14N 1.0 and exclusive C14N of a document or of a subtree.
unit RALXMLC14N;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALXMLReader, RALXML;

const
  /// Algorithm URI of Canonical XML 1.0 (RFC 3076), without comments.
  RALC14NInclusive = 'http://www.w3.org/TR/2001/REC-xml-c14n-20010315';
  /// Algorithm URI of Canonical XML 1.0 with comments.
  RALC14NInclusiveComments = 'http://www.w3.org/TR/2001/REC-xml-c14n-20010315#WithComments';
  /// Algorithm URI of Exclusive XML Canonicalization 1.0, without comments.
  RALC14NExclusive = 'http://www.w3.org/2001/10/xml-exc-c14n#';
  /// Algorithm URI of Exclusive XML Canonicalization 1.0 with comments.
  RALC14NExclusiveComments = 'http://www.w3.org/2001/10/xml-exc-c14n#WithComments';

type
  /// Which canonicalization: Canonical XML 1.0 or its exclusive form.
  TRALXMLC14NMethod = (xcmInclusive, xcmExclusive);

  /// A namespace declaration of the canonicalizer: prefix ('' is the default) and URI.
  TRALXMLC14NNamespace = record
    /// Prefix declared; '' for the default namespace.
    Prefix: StringRAL;
    /// URI bound to it.
    URI: StringRAL;
  end;

  /// An attribute the canonicalizer writes, with its sort key.
  TRALXMLC14NAttribute = record
    /// Part of the name after the prefix.
    LocalName: StringRAL;
    /// Qualified name.
    Name: StringRAL;
    /// Namespace of the attribute; '' when it has no prefix.
    URI: StringRAL;
    /// Value, unescaped.
    Value: StringRAL;
  end;

  /// Stack depths of the canonicalizer saved when an element opens.
  TRALXMLC14NDepth = record
    /// Declarations in scope.
    InScope: IntegerRAL;
    /// Declarations rendered.
    Rendered: IntegerRAL;
  end;

  { Canonical form of a document or of the subtree of an element, in UTF-8: what
    XML signatures and WS-Security hash. The document must be loaded with
    PreserveWhitespace, or the blanks between elements are already gone. }
  TRALXMLCanonicalizer = class
  private
    /// Element being canonicalized as a subtree; nil for a document.
    FApex: TRALXMLNode;
    FInclusivePrefixes: StringRAL;
    /// Namespace declarations in scope, nearest last.
    FInScope: array of TRALXMLC14NNamespace;
    /// Entries used of FInScope.
    FInScopeCount: IntegerRAL;
    /// Bytes used of FOut.
    FLen: IntegerRAL;
    FMethod: TRALXMLC14NMethod;
    /// Output, with spare room.
    FOut: StringRAL;
    /// InclusivePrefixes, split; empty is the default namespace.
    FPrefixList: array of StringRAL;
    /// Namespaces rendered by output ancestors, nearest last.
    FRendered: array of TRALXMLC14NNamespace;
    /// Entries used of FRendered.
    FRenderedCount: IntegerRAL;
    /// Stack depths saved per open element.
    FSaved: array of TRALXMLC14NDepth;
    /// Entries used of FSaved.
    FSavedCount: IntegerRAL;
    FWithComments: boolean;

    /// Appends AText to the output.
    procedure Append(const AText: StringRAL);
    /// Appends AText escaped as an attribute value.
    procedure AppendAttr(const AText: StringRAL);
    /// Appends AText escaped as character content.
    procedure AppendText(const AText: StringRAL);
    /// Opens AElement: its namespaces, attributes and start tag.
    procedure EnterElement(AElement: TRALXMLNode);
    /// URI bound to APrefix by the declarations in scope; '' when none.
    function InScopeURI(const APrefix: StringRAL): StringRAL;
    /// Closes AElement: its end tag, and forgets its declarations.
    procedure LeaveElement(AElement: TRALXMLNode);
    /// Splits InclusivePrefixes into FPrefixList.
    procedure ParsePrefixList;
    /// Pushes onto FInScope the declarations in scope above AElement.
    procedure PushAncestors(AElement: TRALXMLNode);
    /// Pushes the declaration APrefix = AURI onto FInScope.
    procedure PushInScope(const APrefix, AURI: StringRAL);
    /// URI an output ancestor last rendered for APrefix; '' when none.
    function RenderedURI(const APrefix: StringRAL): StringRAL;
    /// Resets the working state for a new run.
    procedure Reset;
    /// Writes ANode as a child of the output (not an element).
    procedure WriteLeaf(ANode: TRALXMLNode);
    /// Writes ARoot and its subtree.
    procedure WriteSubtree(ARoot: TRALXMLNode);
  public
    /// A canonicalizer of AMethod; AWithComments keeps the comments.
    constructor Create(AMethod: TRALXMLC14NMethod = xcmInclusive;
      AWithComments: boolean = False);

    { Canonical form of ANode: a document node (the whole document), an element (its
      subtree, with the namespaces and, for C14N 1.0, the xml:* attributes it
      inherits) or a single text, comment or processing instruction. }
    function Canonicalize(ANode: TRALXMLNode): StringRAL;
    /// Writes the canonical form of ANode into AStream.
    procedure CanonicalizeTo(ANode: TRALXMLNode; AStream: TStream);
    { Sets Method and WithComments from an algorithm URI (RALC14N*); False when
      the URI is not one of them, which leaves both alone. }
    function SetAlgorithm(const AURI: StringRAL): boolean;

    { Exclusive C14N only: prefixes treated as in C14N 1.0, separated by blanks;
      #default is the default namespace (the InclusiveNamespaces PrefixList). }
    property InclusivePrefixes: StringRAL read FInclusivePrefixes write FInclusivePrefixes;
    /// Canonical XML 1.0 or exclusive.
    property Method: TRALXMLC14NMethod read FMethod write FMethod;
    /// Keeps the comments (the #WithComments algorithms).
    property WithComments: boolean read FWithComments write FWithComments;
  end;

{ Canonical form of ANode (see TRALXMLCanonicalizer.Canonicalize) with AMethod;
  AInclusivePrefixes is the PrefixList of the exclusive form. }
function RALXMLCanonicalize(ANode: TRALXMLNode; AMethod: TRALXMLC14NMethod = xcmInclusive;
  AWithComments: boolean = False; const AInclusivePrefixes: StringRAL = ''): StringRAL;
/// Algorithm URI of AMethod, with or without comments.
function RALXMLC14NAlgorithm(AMethod: TRALXMLC14NMethod; AWithComments: boolean): StringRAL;

implementation

{ helpers }

/// Orders two byte strings by their bytes: UTF-8 in byte order is code point order.
function CompareRaw(const A, B: StringRAL): IntegerRAL;
var
  vPA, vPB: PByte;
  vLen, vInt: IntegerRAL;
begin
  vLen := Length(A);
  if Length(B) < vLen then
    vLen := Length(B);
  vPA := PByte(Pointer(A));
  vPB := PByte(Pointer(B));
  for vInt := 0 to vLen - 1 do
  begin
    if vPA^ <> vPB^ then
    begin
      Result := IntegerRAL(vPA^) - IntegerRAL(vPB^);
      Exit;
    end;
    Inc(vPA);
    Inc(vPB);
  end;
  Result := Length(A) - Length(B);
end;

/// Prefix and local part of a qualified name.
procedure SplitName(const AName: StringRAL; out APrefix, ALocal: StringRAL);
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

/// The attribute AName declares a namespace; APrefix is the one it declares.
function IsNsDecl(const AName: StringRAL; out APrefix: StringRAL): boolean;
begin
  Result := False;
  APrefix := '';
  if AName = 'xmlns' then
    Result := True
  else if (Length(AName) > 6) and (Copy(AName, 1, 6) = 'xmlns:') then
  begin
    APrefix := Copy(AName, 7, MaxInt);
    Result := True;
  end;
end;

function RALXMLCanonicalize(ANode: TRALXMLNode; AMethod: TRALXMLC14NMethod;
  AWithComments: boolean; const AInclusivePrefixes: StringRAL): StringRAL;
var
  vC14N: TRALXMLCanonicalizer;
begin
  vC14N := TRALXMLCanonicalizer.Create(AMethod, AWithComments);
  try
    vC14N.InclusivePrefixes := AInclusivePrefixes;
    Result := vC14N.Canonicalize(ANode);
  finally
    vC14N.Free;
  end;
end;

function RALXMLC14NAlgorithm(AMethod: TRALXMLC14NMethod; AWithComments: boolean): StringRAL;
begin
  if AMethod = xcmExclusive then
  begin
    if AWithComments then
      Result := RALC14NExclusiveComments
    else
      Result := RALC14NExclusive;
  end
  else if AWithComments then
    Result := RALC14NInclusiveComments
  else
    Result := RALC14NInclusive;
end;

{ TRALXMLCanonicalizer }

constructor TRALXMLCanonicalizer.Create(AMethod: TRALXMLC14NMethod; AWithComments: boolean);
begin
  inherited Create;
  FMethod := AMethod;
  FWithComments := AWithComments;
end;

function TRALXMLCanonicalizer.SetAlgorithm(const AURI: StringRAL): boolean;
begin
  Result := True;
  if AURI = RALC14NInclusive then
  begin
    FMethod := xcmInclusive;
    FWithComments := False;
  end
  else if AURI = RALC14NInclusiveComments then
  begin
    FMethod := xcmInclusive;
    FWithComments := True;
  end
  else if AURI = RALC14NExclusive then
  begin
    FMethod := xcmExclusive;
    FWithComments := False;
  end
  else if AURI = RALC14NExclusiveComments then
  begin
    FMethod := xcmExclusive;
    FWithComments := True;
  end
  else
    Result := False;
end;

procedure TRALXMLCanonicalizer.Append(const AText: StringRAL);
var
  vLen: IntegerRAL;
begin
  vLen := Length(AText);
  if vLen = 0 then
    Exit;
  if FLen + vLen > Length(FOut) then
  begin
    if Length(FOut) * 2 > FLen + vLen then
      SetLength(FOut, Length(FOut) * 2)
    else
      SetLength(FOut, FLen + vLen + 256);
  end;
  Move(Pointer(AText)^, PByte(Pointer(FOut))[FLen], vLen);
  Inc(FLen, vLen);
end;

procedure TRALXMLCanonicalizer.AppendAttr(const AText: StringRAL);
var
  vPtr, vStart, vEnd: PByte;
  vRep: StringRAL;

  procedure Flush(AUpTo: PByte);
  var
    vPiece: StringRAL;
  begin
    if NativeUInt(AUpTo) > NativeUInt(vStart) then
    begin
      SetLength(vPiece, NativeUInt(AUpTo) - NativeUInt(vStart));
      Move(vStart^, Pointer(vPiece)^, Length(vPiece));
      Append(vPiece);
    end;
  end;

begin
  if AText = '' then
    Exit;
  vPtr := PByte(Pointer(AText));
  vEnd := vPtr + Length(AText);
  vStart := vPtr;
  while NativeUInt(vPtr) < NativeUInt(vEnd) do
  begin
    case vPtr^ of
      Ord('&'): vRep := '&amp;';
      Ord('<'): vRep := '&lt;';
      Ord('"'): vRep := '&quot;';
      9: vRep := '&#x9;';
      10: vRep := '&#xA;';
      13: vRep := '&#xD;';
    else
      begin
        Inc(vPtr);
        Continue;
      end;
    end;
    Flush(vPtr);
    Append(vRep);
    Inc(vPtr);
    vStart := vPtr;
  end;
  Flush(vPtr);
end;

procedure TRALXMLCanonicalizer.AppendText(const AText: StringRAL);
var
  vPtr, vStart, vEnd: PByte;
  vRep: StringRAL;

  procedure Flush(AUpTo: PByte);
  var
    vPiece: StringRAL;
  begin
    if NativeUInt(AUpTo) > NativeUInt(vStart) then
    begin
      SetLength(vPiece, NativeUInt(AUpTo) - NativeUInt(vStart));
      Move(vStart^, Pointer(vPiece)^, Length(vPiece));
      Append(vPiece);
    end;
  end;

begin
  if AText = '' then
    Exit;
  vPtr := PByte(Pointer(AText));
  vEnd := vPtr + Length(AText);
  vStart := vPtr;
  while NativeUInt(vPtr) < NativeUInt(vEnd) do
  begin
    case vPtr^ of
      Ord('&'): vRep := '&amp;';
      Ord('<'): vRep := '&lt;';
      Ord('>'): vRep := '&gt;';
      13: vRep := '&#xD;';
    else
      begin
        Inc(vPtr);
        Continue;
      end;
    end;
    Flush(vPtr);
    Append(vRep);
    Inc(vPtr);
    vStart := vPtr;
  end;
  Flush(vPtr);
end;

procedure TRALXMLCanonicalizer.Reset;
begin
  FOut := '';
  FLen := 0;
  FInScopeCount := 0;
  FRenderedCount := 0;
  FSavedCount := 0;
  ParsePrefixList;
end;

procedure TRALXMLCanonicalizer.ParsePrefixList;
var
  vInt, vStart: IntegerRAL;
  vItem: StringRAL;
begin
  SetLength(FPrefixList, 0);
  vInt := 1;
  while vInt <= Length(FInclusivePrefixes) do
  begin
    while (vInt <= Length(FInclusivePrefixes)) and
          (Ord(FInclusivePrefixes[vInt]) in [9, 10, 13, 32]) do
      Inc(vInt);
    vStart := vInt;
    while (vInt <= Length(FInclusivePrefixes)) and
          not (Ord(FInclusivePrefixes[vInt]) in [9, 10, 13, 32]) do
      Inc(vInt);
    if vInt > vStart then
    begin
      vItem := Copy(FInclusivePrefixes, vStart, vInt - vStart);
      if vItem = '#default' then
        vItem := '';
      SetLength(FPrefixList, Length(FPrefixList) + 1);
      FPrefixList[High(FPrefixList)] := vItem;
    end;
  end;
end;

procedure TRALXMLCanonicalizer.PushInScope(const APrefix, AURI: StringRAL);
begin
  if FInScopeCount >= Length(FInScope) then
    SetLength(FInScope, FInScopeCount * 2 + 8);
  FInScope[FInScopeCount].Prefix := APrefix;
  FInScope[FInScopeCount].URI := AURI;
  Inc(FInScopeCount);
end;

function TRALXMLCanonicalizer.InScopeURI(const APrefix: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  for vInt := FInScopeCount - 1 downto 0 do
    if FInScope[vInt].Prefix = APrefix then
    begin
      Result := FInScope[vInt].URI;
      Exit;
    end;
  Result := '';
end;

function TRALXMLCanonicalizer.RenderedURI(const APrefix: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  for vInt := FRenderedCount - 1 downto 0 do
    if FRendered[vInt].Prefix = APrefix then
    begin
      Result := FRendered[vInt].URI;
      Exit;
    end;
  Result := '';
end;

procedure TRALXMLCanonicalizer.PushAncestors(AElement: TRALXMLNode);
var
  vChain: array of TRALXMLNode;
  vNode: TRALXMLNode;
  vInt, vAttr: IntegerRAL;
  vPrefix: StringRAL;
begin
  // farthest first, so the nearest declaration is the last one pushed
  SetLength(vChain, 0);
  vNode := AElement.Parent;
  while vNode <> nil do
  begin
    if vNode.Kind = xnkElement then
    begin
      SetLength(vChain, Length(vChain) + 1);
      vChain[High(vChain)] := vNode;
    end;
    vNode := vNode.Parent;
  end;
  for vInt := High(vChain) downto 0 do
    for vAttr := 0 to vChain[vInt].AttributeCount - 1 do
      if IsNsDecl(vChain[vInt].AttributeName(vAttr), vPrefix) then
        PushInScope(vPrefix, vChain[vInt].AttributeValue(vAttr));
end;

procedure TRALXMLCanonicalizer.EnterElement(AElement: TRALXMLNode);
var
  vNs: array of TRALXMLC14NNamespace;
  vAttrs: array of TRALXMLC14NAttribute;
  vNsCount, vAttrCount, vInt, vJdx: IntegerRAL;
  vName, vPrefix, vLocal, vURI: StringRAL;
  vNsTmp: TRALXMLC14NNamespace;
  vAttrTmp: TRALXMLC14NAttribute;
  vNode: TRALXMLNode;

  procedure AddCandidate(const APrefix: StringRAL);
  var
    vIdx: IntegerRAL;
    vValue: StringRAL;
  begin
    if APrefix = 'xml' then
      Exit;
    for vIdx := 0 to vNsCount - 1 do
      if vNs[vIdx].Prefix = APrefix then
        Exit;
    vValue := InScopeURI(APrefix);
    // a prefix with nothing bound has nothing to render
    if (APrefix <> '') and (vValue = '') then
      Exit;
    if RenderedURI(APrefix) = vValue then
      Exit;
    if vNsCount >= Length(vNs) then
      SetLength(vNs, vNsCount * 2 + 4);
    vNs[vNsCount].Prefix := APrefix;
    vNs[vNsCount].URI := vValue;
    Inc(vNsCount);
  end;

  procedure AddAttr(const AName, AValue: StringRAL);
  var
    vP, vL: StringRAL;
  begin
    if vAttrCount >= Length(vAttrs) then
      SetLength(vAttrs, vAttrCount * 2 + 4);
    SplitName(AName, vP, vL);
    vAttrs[vAttrCount].Name := AName;
    vAttrs[vAttrCount].Value := AValue;
    vAttrs[vAttrCount].LocalName := vL;
    if vP = '' then
      vAttrs[vAttrCount].URI := ''
    else if vP = 'xml' then
      vAttrs[vAttrCount].URI := RALXMLNamespaceXML
    else
      vAttrs[vAttrCount].URI := InScopeURI(vP);
    Inc(vAttrCount);
  end;

  function HasAttr(const AName: StringRAL): boolean;
  var
    vIdx: IntegerRAL;
  begin
    Result := False;
    for vIdx := 0 to vAttrCount - 1 do
      if vAttrs[vIdx].Name = AName then
      begin
        Result := True;
        Exit;
      end;
  end;

begin
  if FSavedCount >= Length(FSaved) then
    SetLength(FSaved, FSavedCount * 2 + 16);
  FSaved[FSavedCount].InScope := FInScopeCount;
  FSaved[FSavedCount].Rendered := FRenderedCount;
  Inc(FSavedCount);

  // the declarations of the element itself, and its other attributes
  vNsCount := 0;
  vAttrCount := 0;
  for vInt := 0 to AElement.AttributeCount - 1 do
    if IsNsDecl(AElement.AttributeName(vInt), vPrefix) then
      PushInScope(vPrefix, AElement.AttributeValue(vInt));
  for vInt := 0 to AElement.AttributeCount - 1 do
  begin
    vName := AElement.AttributeName(vInt);
    if not IsNsDecl(vName, vPrefix) then
      AddAttr(vName, AElement.AttributeValue(vInt));
  end;

  // C14N 1.0: the apex takes the xml:* attributes its ancestors have and it lacks
  if (FMethod = xcmInclusive) and (AElement = FApex) then
  begin
    vNode := AElement.Parent;
    while vNode <> nil do
    begin
      if vNode.Kind = xnkElement then
        for vInt := 0 to vNode.AttributeCount - 1 do
        begin
          vName := vNode.AttributeName(vInt);
          if (Copy(vName, 1, 4) = 'xml:') and not HasAttr(vName) then
            AddAttr(vName, vNode.AttributeValue(vInt));
        end;
      vNode := vNode.Parent;
    end;
  end;

  // the namespaces to render
  if FMethod = xcmInclusive then
  begin
    for vInt := FInScopeCount - 1 downto 0 do
      AddCandidate(FInScope[vInt].Prefix);
  end
  else
  begin
    // visibly utilized: the element's prefix and its attributes' prefixes
    SplitName(AElement.NodeName, vPrefix, vLocal);
    AddCandidate(vPrefix);
    for vInt := 0 to vAttrCount - 1 do
    begin
      SplitName(vAttrs[vInt].Name, vPrefix, vLocal);
      if vPrefix <> '' then
        AddCandidate(vPrefix);
    end;
    for vInt := 0 to High(FPrefixList) do
      AddCandidate(FPrefixList[vInt]);
  end;

  for vInt := 0 to vNsCount - 1 do
  begin
    if FRenderedCount >= Length(FRendered) then
      SetLength(FRendered, FRenderedCount * 2 + 8);
    FRendered[FRenderedCount] := vNs[vInt];
    Inc(FRenderedCount);
  end;

  { namespaces by prefix (the default first), attributes by namespace URI, then
    local name }
  for vInt := 1 to vNsCount - 1 do
  begin
    vNsTmp := vNs[vInt];
    vJdx := vInt - 1;
    while (vJdx >= 0) and (CompareRaw(vNs[vJdx].Prefix, vNsTmp.Prefix) > 0) do
    begin
      vNs[vJdx + 1] := vNs[vJdx];
      Dec(vJdx);
    end;
    vNs[vJdx + 1] := vNsTmp;
  end;
  for vInt := 1 to vAttrCount - 1 do
  begin
    vAttrTmp := vAttrs[vInt];
    vJdx := vInt - 1;
    while (vJdx >= 0) and ((CompareRaw(vAttrs[vJdx].URI, vAttrTmp.URI) > 0) or
          ((vAttrs[vJdx].URI = vAttrTmp.URI) and
           (CompareRaw(vAttrs[vJdx].LocalName, vAttrTmp.LocalName) > 0))) do
    begin
      vAttrs[vJdx + 1] := vAttrs[vJdx];
      Dec(vJdx);
    end;
    vAttrs[vJdx + 1] := vAttrTmp;
  end;

  Append('<');
  Append(AElement.NodeName);
  for vInt := 0 to vNsCount - 1 do
  begin
    if vNs[vInt].Prefix = '' then
      Append(' xmlns="')
    else
    begin
      Append(' xmlns:');
      Append(vNs[vInt].Prefix);
      Append('="');
    end;
    vURI := vNs[vInt].URI;
    AppendAttr(vURI);
    Append('"');
  end;
  for vInt := 0 to vAttrCount - 1 do
  begin
    Append(' ');
    Append(vAttrs[vInt].Name);
    Append('="');
    AppendAttr(vAttrs[vInt].Value);
    Append('"');
  end;
  Append('>');
end;

procedure TRALXMLCanonicalizer.LeaveElement(AElement: TRALXMLNode);
begin
  Append('</');
  Append(AElement.NodeName);
  Append('>');
  Dec(FSavedCount);
  FInScopeCount := FSaved[FSavedCount].InScope;
  FRenderedCount := FSaved[FSavedCount].Rendered;
end;

procedure TRALXMLCanonicalizer.WriteLeaf(ANode: TRALXMLNode);
begin
  case ANode.Kind of
    xnkText, xnkCData:
      AppendText(ANode.Value);
    xnkComment:
      if FWithComments then
      begin
        Append('<!--');
        Append(ANode.Value);
        Append('-->');
      end;
    xnkProcessingInstruction:
      begin
        Append('<?');
        Append(ANode.NodeName);
        if ANode.Value <> '' then
        begin
          Append(' ');
          Append(ANode.Value);
        end;
        Append('?>');
      end;
  end;
end;

procedure TRALXMLCanonicalizer.WriteSubtree(ARoot: TRALXMLNode);
var
  vNode: TRALXMLNode;
begin
  // iterative: the depth of a document is not the depth of the stack
  vNode := ARoot;
  while True do
  begin
    if vNode.Kind = xnkElement then
    begin
      EnterElement(vNode);
      if vNode.FirstChild <> nil then
      begin
        vNode := vNode.FirstChild;
        Continue;
      end;
      LeaveElement(vNode);
    end
    else
      WriteLeaf(vNode);

    while (vNode <> ARoot) and (vNode.NextSibling = nil) do
    begin
      vNode := vNode.Parent;
      LeaveElement(vNode);
    end;
    if vNode = ARoot then
      Break;
    vNode := vNode.NextSibling;
  end;
end;

function TRALXMLCanonicalizer.Canonicalize(ANode: TRALXMLNode): StringRAL;
var
  vChild: TRALXMLNode;
  vSeenRoot: boolean;
begin
  Reset;
  try
    if ANode = nil then
    begin
      Result := '';
      Exit;
    end;

    if ANode.Kind = xnkDocument then
    begin
      { outside the root element: no DOCTYPE, no blanks; a comment or processing
        instruction before it ends in a line break, one after it starts with one }
      FApex := nil;
      vSeenRoot := False;
      vChild := ANode.FirstChild;
      while vChild <> nil do
      begin
        case vChild.Kind of
          xnkElement:
            begin
              WriteSubtree(vChild);
              vSeenRoot := True;
            end;
          xnkComment, xnkProcessingInstruction:
            if (vChild.Kind = xnkProcessingInstruction) or FWithComments then
            begin
              if vSeenRoot then
                Append(#10);
              WriteLeaf(vChild);
              if not vSeenRoot then
                Append(#10);
            end;
        end;
        vChild := vChild.NextSibling;
      end;
    end
    else if ANode.Kind = xnkElement then
    begin
      FApex := ANode;
      PushAncestors(ANode);
      WriteSubtree(ANode);
    end
    else
      WriteLeaf(ANode);

    SetLength(FOut, FLen);
    Result := FOut;
  finally
    FOut := '';
    FLen := 0;
    FApex := nil;
  end;
end;

procedure TRALXMLCanonicalizer.CanonicalizeTo(ANode: TRALXMLNode; AStream: TStream);
var
  vText: StringRAL;
begin
  vText := Canonicalize(ANode);
  if vText <> '' then
    AStream.WriteBuffer(Pointer(vText)^, Length(vText));
end;

end.
