/// Regular expressions of RAL: RE2 syntax, linear time, the same on every compiler.
/// No backtracking: a pattern compiles to a Thompson NFA, "is there a match" runs
/// as a lazily built DFA, and captures run on a Pike VM; a required literal is
/// searched first. Backreferences and lookaround are refused when compiling.
unit RALRegex;

{$I ..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALExceptions, RALTypes, RALConsts;

const
  /// Longest pattern accepted, in bytes.
  RALRegexMaxPatternLength = 32768;
  /// Most instructions a compiled pattern may have (counted repeats expand).
  RALRegexMaxProgram = 50000;
  /// Largest count of a counted repeat ({n,m}), as in RE2.
  RALRegexMaxRepeat = 1000;
  /// Most states the lazy DFA of one pattern builds before the NFA takes over.
  RALRegexMaxDFAStates = 4096;

type
  /// A pattern that does not compile; Position is the byte (from 1) where it failed.
  ERALRegex = class(ERALException)
  private
    FPosition: IntegerRAL;
  public
    constructor CreatePos(const AMessage: string; APosition: IntegerRAL);

    /// Byte of the pattern, from 1, where the error was found.
    property Position: IntegerRAL read FPosition;
  end;

  { How a pattern reads: roIgnoreCase as (?i), roMultiLine as (?m) (^ and $ at line
    ends), roDotAll as (?s) (. takes a line break), roFullMatch as ^(?:...)\z (the
    whole text must match). }
  TRALRegexOption = (roIgnoreCase, roMultiLine, roDotAll, roFullMatch);
  /// Set of TRALRegexOption.
  TRALRegexOptions = set of TRALRegexOption;

  /// One capture group of a match: Index from 1 and Length in bytes; Index 0 = did not take part.
  TRALRegexGroup = record
    /// Byte of the text, from 1, where the group starts; 0 when it did not take part.
    Index: IntegerRAL;
    /// Length of the group, in bytes.
    Length: IntegerRAL;
  end;

  /// The groups of a match: 0 is the whole match.
  TRALRegexGroups = array of TRALRegexGroup;

  /// One match of a pattern in a text.
  TRALRegexMatch = record
    /// The groups, 0 being the whole match; empty when Success is False.
    Groups: TRALRegexGroups;
    /// Names of the groups, '' for the unnamed ones (shared with the pattern).
    Names: array of StringRAL;
    /// Whether the pattern matched.
    Success: Boolean;
    /// The text that was searched.
    Text: StringRAL;

    /// Text of group AIndex; '' when it did not take part.
    function Group(AIndex: IntegerRAL): StringRAL;
    /// Text of the group named AName; '' when there is none or it did not take part.
    function GroupByName(const AName: StringRAL): StringRAL;
    /// Byte of the text, from 1, where the match starts.
    function Index: IntegerRAL;
    /// Length of the match, in bytes.
    function Length: IntegerRAL;
    /// Text of the whole match.
    function Value: StringRAL;
  end;

  /// Every match of a pattern in a text, left to right.
  TRALRegexMatches = array of TRALRegexMatch;
  /// Pieces of a text split by a pattern.
  TRALRegexPieces = array of StringRAL;

  /// Instruction of the compiled program.
  TRALRegexOp = (ropChar, ropCharFold, ropClass, ropAny, ropAnyNotNL, ropSplit,
    ropJmp, ropSave, ropMatch, ropBeginLine, ropEndLine, ropBeginText, ropEndText,
    ropWordBoundary, ropNotWordBoundary);

  /// One instruction: X and Y are targets (split, jmp), the class or the slot.
  TRALRegexInst = record
    Op: TRALRegexOp;
    /// Code point of ropChar (folded for ropCharFold).
    Rune: Cardinal;
    /// First target, class index or save slot.
    X: IntegerRAL;
    /// Second target of ropSplit (the lower priority one).
    Y: IntegerRAL;
  end;

  /// A range of code points of a class.
  TRALRegexRange = record
    Lo: Cardinal;
    Hi: Cardinal;
  end;

  /// A character class: sorted, merged ranges.
  TRALRegexClass = record
    Negated: Boolean;
    Ranges: array of TRALRegexRange;
  end;

  /// One state of the lazy DFA: the NFA instructions it stands for.
  TRALRegexDFAState = class
  public
    /// Whether the set holds ropMatch.
    Accepting: Boolean;
    /// Whether the set holds ropEndText (it may accept when the text ends).
    HasEndText: Boolean;
    /// Whether the state accepts when the text ends there: -1 not known yet, 0, 1.
    AcceptsAtEnd: IntegerRAL;
    /// Instructions of the set, in order.
    PCs: array of IntegerRAL;
    /// Next state for each ASCII byte: -2 not built yet, -1 dead.
    Next: array[0..127] of IntegerRAL;
    /// Next state in the same bucket of the state table, or -1.
    Chain: IntegerRAL;
    /// Hash of PCs.
    Hash: Cardinal;
    /// Index of the state in the table.
    Id: IntegerRAL;
  end;

  { A compiled pattern. Compile once (the constructor), use from any thread: the
    program never changes after it; the DFA cache is shared under a lock, and a
    thread that finds it busy runs the NFA instead of waiting. }
  TRALRegex = class
  private
    FAnchorStart: Boolean;
    FCapCount: IntegerRAL;
    FClasses: array of TRALRegexClass;
    FDFAEnabled: Boolean;
    FDFAFull: Boolean;
    FDFALock: TCriticalSection;
    FDFAStart: array[Boolean] of IntegerRAL;
    FDFAStates: TList;
    /// The same states as FDFAStates, for the hot loop.
    FDFAArray: array of TRALRegexDFAState;
    /// Buckets of the state table: first state of each hash, or -1.
    FDFABuckets: array of IntegerRAL;
    FLiteral: RawByteString;
    /// Horspool skip table of FLiteral.
    FLiteralSkip: array[0..255] of IntegerRAL;
    FLiteralOnly: Boolean;
    FLiteralEnd: Boolean;
    FNames: array of StringRAL;
    FOptions: TRALRegexOptions;
    FPattern: StringRAL;
    FProg: array of TRALRegexInst;
    FProgCount: IntegerRAL;

    function AddDFAState(const APCs: array of IntegerRAL; ACount: IntegerRAL): IntegerRAL;
    function ClassMatches(AClass: IntegerRAL; ARune: Cardinal): Boolean;
    procedure Compile;
    function DFAMatch(const AText: StringRAL; AStart: IntegerRAL;
      out AUsed: Boolean): Boolean;
    function DFAStep(AState: IntegerRAL; ARune: Cardinal): IntegerRAL;
    procedure FreeDFA;
    function GetGroupName(AIndex: IntegerRAL): StringRAL;
    function LiteralMatch(const AText: StringRAL; AStart: IntegerRAL;
      out AIndex: IntegerRAL): Boolean;
    function PikeSearch(const AText: StringRAL; AStart: IntegerRAL;
      ACaps: TRALRegexGroups; AWantCaps: Boolean): Boolean;
  public
    constructor Create(const APattern: StringRAL; AOptions: TRALRegexOptions = []);
    destructor Destroy; override;

    /// Escapes every metacharacter of AText, so it matches itself.
    class function Escape(const AText: StringRAL): StringRAL;
    /// Index of the group named AName, or -1.
    function GroupIndex(const AName: StringRAL): IntegerRAL;
    /// Whether the pattern matches somewhere in AText (the whole text with roFullMatch).
    function IsMatch(const AText: StringRAL): Boolean;
    /// The first match at or after byte AStart (from 1), leftmost-first like Perl.
    function Match(const AText: StringRAL; AStart: IntegerRAL = 1): TRALRegexMatch;
    /// Every match, left to right; an empty match advances one character.
    function Matches(const AText: StringRAL): TRALRegexMatches;
    /// AText with every match replaced by AReplacement, where $0..$99 and ${name}
    /// are groups and $$ is a dollar.
    function Replace(const AText, AReplacement: StringRAL): StringRAL;
    /// The pieces of AText between the matches.
    function Split(const AText: StringRAL): TRALRegexPieces;

    /// Number of capture groups, group 0 (the whole match) included.
    property GroupCount: IntegerRAL read FCapCount;
    /// Name of group AIndex, '' when unnamed.
    property GroupNames[AIndex: IntegerRAL]: StringRAL read GetGroupName;
    /// The options it was compiled with.
    property Options: TRALRegexOptions read FOptions;
    /// The pattern as given.
    property Pattern: StringRAL read FPattern;
  end;

/// Whether APattern matches somewhere in AText; compiles the pattern every time.
function RALRegexIsMatch(const AText, APattern: StringRAL;
  AOptions: TRALRegexOptions = []): Boolean;

implementation

type
  TNodeKind = (nkEmpty, nkLiteral, nkClass, nkAny, nkConcat, nkAlternate,
    nkRepeat, nkCapture, nkAssert);

  TNode = record
    Kind: TNodeKind;
    Rune: Cardinal;
    Fold: Boolean;
    DotAll: Boolean;
    ClassIdx: IntegerRAL;
    MinRep: IntegerRAL;
    MaxRep: IntegerRAL;
    Greedy: Boolean;
    CapIndex: IntegerRAL;
    Assert: TRALRegexOp;
    Children: array of IntegerRAL;
  end;

  TFlags = record
    IgnoreCase: Boolean;
    MultiLine: Boolean;
    DotAll: Boolean;
    Ungreedy: Boolean;
  end;

  { The parser: the pattern to a tree of TNode, with the classes and group names
    going straight to the regex. }
  TParser = class
  private
    FNodes: array of TNode;
    FNodeCount: IntegerRAL;
    FPos: IntegerRAL;
    FLen: IntegerRAL;
    FText: RawByteString;
    FOwner: TRALRegex;
    FCaps: IntegerRAL;
    procedure Error(const AMessage: string);
    function NewNode(AKind: TNodeKind): IntegerRAL;
    procedure AddChild(ANode, AChild: IntegerRAL);
    function Peek: Cardinal;
    function PeekAt(AOffset: IntegerRAL): Byte;
    function Next: Cardinal;
    function AtEnd: Boolean;
    function ParseAlternate(var AFlags: TFlags; ADepth: IntegerRAL): IntegerRAL;
    function ParseConcat(var AFlags: TFlags; ADepth: IntegerRAL): IntegerRAL;
    function ParseRepeat(var AFlags: TFlags; ADepth: IntegerRAL): IntegerRAL;
    function ParseAtom(var AFlags: TFlags; ADepth: IntegerRAL): IntegerRAL;
    function ParseClass(var AFlags: TFlags): IntegerRAL;
    function ParseEscapeInClass(var ARanges: TList; out ARune: Cardinal): Boolean;
    function ParseNumber(out AValue: IntegerRAL): Boolean;
    function ParseHex: Cardinal;
    function LiteralNode(ARune: Cardinal; const AFlags: TFlags): IntegerRAL;
    function ClassNode(const ARanges: array of TRALRegexRange; ANegated: Boolean;
      const AFlags: TFlags): IntegerRAL;
    function PerlClassNode(ALetter: Cardinal; const AFlags: TFlags): IntegerRAL;
  public
    constructor Create(AOwner: TRALRegex; const AText: RawByteString);
    function Parse(const AFlags: TFlags): IntegerRAL;
  end;

type
  PRALRegexRange = ^TRALRegexRange;
  PRALRegexInst = ^TRALRegexInst;

const
  cRuneError = $FFFD;
  cMaxDepth = 1000;

procedure FillSkip(const ALit: RawByteString; var ASkip: array of IntegerRAL); forward;

{ UTF-8 }

/// Decodes the code point at byte APos (from 0) of AText and moves APos past it.
function DecodeRune(AText: PByte; ALen: IntegerRAL; var APos: IntegerRAL): Cardinal;
var
  vB: Byte;
  vNeed, vInt: IntegerRAL;
  vMin: Cardinal;
begin
  vB := AText[APos];
  if vB < $80 then
  begin
    Result := vB;
    Inc(APos);
    Exit;
  end;
  if (vB and $E0) = $C0 then
  begin
    vNeed := 1;
    Result := vB and $1F;
    vMin := $80;
  end
  else if (vB and $F0) = $E0 then
  begin
    vNeed := 2;
    Result := vB and $0F;
    vMin := $800;
  end
  else if (vB and $F8) = $F0 then
  begin
    vNeed := 3;
    Result := vB and $07;
    vMin := $10000;
  end
  else
  begin
    Result := cRuneError;
    Inc(APos);
    Exit;
  end;
  if APos + vNeed >= ALen then
  begin
    Result := cRuneError;
    Inc(APos);
    Exit;
  end;
  for vInt := 1 to vNeed do
  begin
    vB := AText[APos + vInt];
    if (vB and $C0) <> $80 then
    begin
      Result := cRuneError;
      Inc(APos);
      Exit;
    end;
    Result := (Result shl 6) or (vB and $3F);
  end;
  if (Result < vMin) or (Result > $10FFFF) or ((Result >= $D800) and (Result <= $DFFF)) then
  begin
    Result := cRuneError;
    Inc(APos);
    Exit;
  end;
  Inc(APos, vNeed + 1);
end;

/// The code point that ends just before byte APos (from 0); 0 at the start.
function RuneBefore(AText: PByte; APos: IntegerRAL): Cardinal;
var
  vStart, vPos: IntegerRAL;
begin
  Result := 0;
  if APos <= 0 then
    Exit;
  // back over at most three continuation bytes
  vStart := APos - 1;
  while (vStart > 0) and (APos - vStart < 4) and ((AText[vStart] and $C0) = $80) do
    Dec(vStart);
  vPos := vStart;
  Result := DecodeRune(AText, APos, vPos);
  if vPos <> APos then
    Result := AText[APos - 1];
end;

function EncodeRune(ARune: Cardinal): RawByteString;
begin
  if ARune < $80 then
    Result := AnsiChar(ARune)
  else if ARune < $800 then
    Result := AnsiChar($C0 or (ARune shr 6)) + AnsiChar($80 or (ARune and $3F))
  else if ARune < $10000 then
    Result := AnsiChar($E0 or (ARune shr 12)) + AnsiChar($80 or ((ARune shr 6) and $3F)) +
      AnsiChar($80 or (ARune and $3F))
  else
    Result := AnsiChar($F0 or (ARune shr 18)) + AnsiChar($80 or ((ARune shr 12) and $3F)) +
      AnsiChar($80 or ((ARune shr 6) and $3F)) + AnsiChar($80 or (ARune and $3F));
end;

function IsWordRune(ARune: Cardinal): Boolean;
begin
  Result := ((ARune >= Ord('0')) and (ARune <= Ord('9'))) or
    ((ARune >= Ord('A')) and (ARune <= Ord('Z'))) or
    ((ARune >= Ord('a')) and (ARune <= Ord('z'))) or (ARune = Ord('_'));
end;

{ Simple case folding: ASCII, Latin-1, Latin Extended-A, Greek and Cyrillic. The
  folded form is the lower case one. }
function FoldRune(ARune: Cardinal): Cardinal;
begin
  Result := ARune;
  if ARune < $80 then
  begin
    if (ARune >= Ord('A')) and (ARune <= Ord('Z')) then
      Result := ARune + 32;
  end
  else if ((ARune >= $C0) and (ARune <= $DE) and (ARune <> $D7)) then
    Result := ARune + 32
  else if (ARune >= $100) and (ARune <= $17F) then
  begin
    if (ARune >= $139) and (ARune <= $148) then
    begin
      if Odd(ARune) then
        Result := ARune + 1;
    end
    else if (ARune >= $179) and (ARune <= $17E) then
    begin
      if Odd(ARune) then
        Result := ARune + 1;
    end
    else if (ARune <> $130) and (ARune <> $131) and (ARune <> $138) and (ARune <> $149) and
      (ARune <> $17F) and (not Odd(ARune)) then
      Result := ARune + 1;
  end
  else if (ARune >= $391) and (ARune <= $3AB) and (ARune <> $3A2) then
    Result := ARune + 32
  else if (ARune >= $410) and (ARune <= $42F) then
    Result := ARune + 32
  else if (ARune >= $400) and (ARune <= $40F) then
    Result := ARune + 80;
end;

var
  /// Code points (up to $4FF) that have another case form.
  gHasCasePair: array[0..$4FF] of Boolean;

procedure FillCasePairs;
var
  vRune: Cardinal;
begin
  for vRune := 0 to $4FF do
    if FoldRune(vRune) <> vRune then
    begin
      gHasCasePair[vRune] := True;
      if FoldRune(vRune) <= $4FF then
        gHasCasePair[FoldRune(vRune)] := True;
    end;
end;

{ ERALRegex }

constructor ERALRegex.CreatePos(const AMessage: string; APosition: IntegerRAL);
begin
  inherited Create(AMessage + ' (' + IntToStr(APosition) + ')');
  FPosition := APosition;
end;

{ Ranges }

procedure SortMerge(var ARanges: array of TRALRegexRange; var ACount: IntegerRAL);
var
  vInt, vJ, vOut: IntegerRAL;
  vTmp: TRALRegexRange;
begin
  // insertion sort: classes are small
  for vInt := 1 to ACount - 1 do
  begin
    vTmp := ARanges[vInt];
    vJ := vInt - 1;
    while (vJ >= 0) and (ARanges[vJ].Lo > vTmp.Lo) do
    begin
      ARanges[vJ + 1] := ARanges[vJ];
      Dec(vJ);
    end;
    ARanges[vJ + 1] := vTmp;
  end;
  if ACount = 0 then
    Exit;
  vOut := 0;
  for vInt := 1 to ACount - 1 do
  begin
    if ARanges[vInt].Lo <= ARanges[vOut].Hi + 1 then
    begin
      if ARanges[vInt].Hi > ARanges[vOut].Hi then
        ARanges[vOut].Hi := ARanges[vInt].Hi;
    end
    else
    begin
      Inc(vOut);
      ARanges[vOut] := ARanges[vInt];
    end;
  end;
  ACount := vOut + 1;
end;

function MakeRange(ALo, AHi: Cardinal): TRALRegexRange;
begin
  Result.Lo := ALo;
  Result.Hi := AHi;
end;

{ TParser }

constructor TParser.Create(AOwner: TRALRegex; const AText: RawByteString);
begin
  inherited Create;
  FOwner := AOwner;
  FText := AText;
  FLen := System.Length(AText);
  FPos := 0;
  FCaps := 1;
  SetLength(FNodes, 64);
  FNodeCount := 0;
end;

procedure TParser.Error(const AMessage: string);
begin
  raise ERALRegex.CreatePos(AMessage, FPos + 1);
end;

function TParser.NewNode(AKind: TNodeKind): IntegerRAL;
begin
  if FNodeCount >= System.Length(FNodes) then
    SetLength(FNodes, System.Length(FNodes) * 2);
  Result := FNodeCount;
  Inc(FNodeCount);
  // SetLength zeroed the new slots, and a node is never reused
  FNodes[Result].Kind := AKind;
  FNodes[Result].Greedy := True;
end;

procedure TParser.AddChild(ANode, AChild: IntegerRAL);
var
  vLen: IntegerRAL;
begin
  vLen := System.Length(FNodes[ANode].Children);
  SetLength(FNodes[ANode].Children, vLen + 1);
  FNodes[ANode].Children[vLen] := AChild;
end;

function TParser.AtEnd: Boolean;
begin
  Result := FPos >= FLen;
end;

function TParser.PeekAt(AOffset: IntegerRAL): Byte;
begin
  if FPos + AOffset < FLen then
    Result := PByte(Pointer(FText))[FPos + AOffset]
  else
    Result := 0;
end;

function TParser.Peek: Cardinal;
var
  vPos: IntegerRAL;
begin
  if AtEnd then
  begin
    Result := 0;
    Exit;
  end;
  vPos := FPos;
  Result := DecodeRune(PByte(Pointer(FText)), FLen, vPos);
end;

function TParser.Next: Cardinal;
begin
  if AtEnd then
    Error(emRegexUnexpectedEnd);
  Result := DecodeRune(PByte(Pointer(FText)), FLen, FPos);
end;

function TParser.Parse(const AFlags: TFlags): IntegerRAL;
var
  vFlags: TFlags;
begin
  vFlags := AFlags;
  Result := ParseAlternate(vFlags, 0);
  if not AtEnd then
    Error(emRegexUnmatchedParen);
end;

function TParser.ParseAlternate(var AFlags: TFlags; ADepth: IntegerRAL): IntegerRAL;
var
  vFirst, vNext: IntegerRAL;
begin
  if ADepth > cMaxDepth then
    Error(emRegexTooDeep);
  vFirst := ParseConcat(AFlags, ADepth);
  if AtEnd or (PeekAt(0) <> Ord('|')) then
  begin
    Result := vFirst;
    Exit;
  end;
  Result := NewNode(nkAlternate);
  AddChild(Result, vFirst);
  while (not AtEnd) and (PeekAt(0) = Ord('|')) do
  begin
    Inc(FPos);
    vNext := ParseConcat(AFlags, ADepth);
    AddChild(Result, vNext);
  end;
end;

function TParser.ParseConcat(var AFlags: TFlags; ADepth: IntegerRAL): IntegerRAL;
var
  vItem: IntegerRAL;
begin
  Result := NewNode(nkConcat);
  while (not AtEnd) and (PeekAt(0) <> Ord('|')) and (PeekAt(0) <> Ord(')')) do
  begin
    vItem := ParseRepeat(AFlags, ADepth);
    if vItem >= 0 then
      AddChild(Result, vItem);
  end;
  if System.Length(FNodes[Result].Children) = 0 then
    FNodes[Result].Kind := nkEmpty
  else if System.Length(FNodes[Result].Children) = 1 then
    Result := FNodes[Result].Children[0];
end;

function TParser.ParseNumber(out AValue: IntegerRAL): Boolean;
var
  vB: Byte;
begin
  Result := False;
  AValue := 0;
  while not AtEnd do
  begin
    vB := PeekAt(0);
    if (vB < Ord('0')) or (vB > Ord('9')) then
      Break;
    if AValue > RALRegexMaxRepeat * 10 then
      Error(emRegexRepeatSize);
    AValue := AValue * 10 + (vB - Ord('0'));
    Inc(FPos);
    Result := True;
  end;
end;

function TParser.ParseRepeat(var AFlags: TFlags; ADepth: IntegerRAL): IntegerRAL;
var
  vAtom, vMin, vMax, vSave: IntegerRAL;
  vB: Byte;
  vCounted, vHadRepeat: Boolean;
begin
  vAtom := ParseAtom(AFlags, ADepth);
  Result := vAtom;
  if vAtom < 0 then
    Exit;
  vHadRepeat := False;
  while not AtEnd do
  begin
    vB := PeekAt(0);
    vCounted := False;
    vMin := 0;
    vMax := -1;
    case vB of
      Ord('*'): begin vMin := 0; vMax := -1; end;
      Ord('+'): begin vMin := 1; vMax := -1; end;
      Ord('?'): begin vMin := 0; vMax := 1; end;
      Ord('{'):
        begin
          vSave := FPos;
          Inc(FPos);
          if not ParseNumber(vMin) then
          begin
            // not a repeat: '{' is a literal (as in RE2)
            FPos := vSave;
            Break;
          end;
          vMax := vMin;
          if (not AtEnd) and (PeekAt(0) = Ord(',')) then
          begin
            Inc(FPos);
            if not ParseNumber(vMax) then
              vMax := -1;
          end;
          if AtEnd or (PeekAt(0) <> Ord('}')) then
          begin
            FPos := vSave;
            Break;
          end;
          if (vMin > RALRegexMaxRepeat) or (vMax > RALRegexMaxRepeat) or
             ((vMax >= 0) and (vMax < vMin)) then
            Error(emRegexRepeatSize);
          vCounted := True;
        end;
    else
      Break;
    end;
    if vHadRepeat then
      Error(emRegexBadRepeat);
    vHadRepeat := True;
    if not vCounted then
      Inc(FPos)
    else
      Inc(FPos); // the '}'
    if FNodes[Result].Kind = nkEmpty then
      Error(emRegexMissingRepeatArg);
    if (FNodes[Result].Kind = nkAssert) then
      Error(emRegexMissingRepeatArg);
    vAtom := NewNode(nkRepeat);
    FNodes[vAtom].MinRep := vMin;
    FNodes[vAtom].MaxRep := vMax;
    FNodes[vAtom].Greedy := not AFlags.Ungreedy;
    AddChild(vAtom, Result);
    if (not AtEnd) and (PeekAt(0) = Ord('?')) then
    begin
      Inc(FPos);
      FNodes[vAtom].Greedy := AFlags.Ungreedy;
    end;
    Result := vAtom;
  end;
end;

function TParser.LiteralNode(ARune: Cardinal; const AFlags: TFlags): IntegerRAL;
begin
  Result := NewNode(nkLiteral);
  FNodes[Result].Rune := ARune;
  // folded only when the code point has another case form: '1' stays a plain literal
  if AFlags.IgnoreCase and (ARune <= $4FF) and gHasCasePair[ARune] then
  begin
    FNodes[Result].Rune := FoldRune(ARune);
    FNodes[Result].Fold := True;
  end;
end;

function TParser.ClassNode(const ARanges: array of TRALRegexRange; ANegated: Boolean;
  const AFlags: TFlags): IntegerRAL;
var
  vCount, vInt, vIdx: IntegerRAL;
  vList: array of TRALRegexRange;
  vRune: Cardinal;
  vLo, vHi: Cardinal;
begin
  vCount := System.Length(ARanges);
  SetLength(vList, vCount);
  for vInt := 0 to vCount - 1 do
    vList[vInt] := ARanges[vInt];
  if AFlags.IgnoreCase then
  begin
    // every code point whose fold falls in a range, and the folds of the range
    for vInt := 0 to System.Length(ARanges) - 1 do
    begin
      vLo := ARanges[vInt].Lo;
      vHi := ARanges[vInt].Hi;
      if vHi > $4FF then
        vHi := $4FF;
      vRune := vLo;
      while vRune <= vHi do
      begin
        if FoldRune(vRune) <> vRune then
        begin
          SetLength(vList, vCount + 1);
          vList[vCount] := MakeRange(FoldRune(vRune), FoldRune(vRune));
          Inc(vCount);
        end;
        Inc(vRune);
      end;
    end;
    // upper case forms of lower case members
    for vRune := 0 to $4FF do
      if FoldRune(vRune) <> vRune then
        for vInt := 0 to System.Length(ARanges) - 1 do
          if (FoldRune(vRune) >= ARanges[vInt].Lo) and (FoldRune(vRune) <= ARanges[vInt].Hi) then
          begin
            SetLength(vList, vCount + 1);
            vList[vCount] := MakeRange(vRune, vRune);
            Inc(vCount);
            Break;
          end;
  end;
  SortMerge(vList, vCount);
  vIdx := System.Length(FOwner.FClasses);
  SetLength(FOwner.FClasses, vIdx + 1);
  FOwner.FClasses[vIdx].Negated := ANegated;
  SetLength(FOwner.FClasses[vIdx].Ranges, vCount);
  for vInt := 0 to vCount - 1 do
    FOwner.FClasses[vIdx].Ranges[vInt] := vList[vInt];
  Result := NewNode(nkClass);
  FNodes[Result].ClassIdx := vIdx;
end;

procedure AddPerlRanges(ALetter: Cardinal; AList: TList; out ANegated: Boolean);

  procedure Add(ALo, AHi: Cardinal);
  var
    vRange: ^TRALRegexRange;
  begin
    New(vRange);
    vRange^.Lo := ALo;
    vRange^.Hi := AHi;
    AList.Add(vRange);
  end;

begin
  ANegated := (ALetter = Ord('D')) or (ALetter = Ord('W')) or (ALetter = Ord('S'));
  case ALetter of
    Ord('d'), Ord('D'): Add(Ord('0'), Ord('9'));
    Ord('w'), Ord('W'):
      begin
        Add(Ord('0'), Ord('9'));
        Add(Ord('A'), Ord('Z'));
        Add(Ord('_'), Ord('_'));
        Add(Ord('a'), Ord('z'));
      end;
    Ord('s'), Ord('S'):
      begin
        Add(9, 10);
        Add(12, 13);
        Add(32, 32);
      end;
  end;
end;

procedure FreeRangeList(AList: TList);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to AList.Count - 1 do
    Dispose(PRALRegexRange(AList[vInt]));
  AList.Free;
end;

function TParser.PerlClassNode(ALetter: Cardinal; const AFlags: TFlags): IntegerRAL;
var
  vList: TList;
  vNeg: Boolean;
  vRanges: array of TRALRegexRange;
  vInt: IntegerRAL;
  vFlags: TFlags;
begin
  vList := TList.Create;
  try
    AddPerlRanges(ALetter, vList, vNeg);
    SetLength(vRanges, vList.Count);
    for vInt := 0 to vList.Count - 1 do
      vRanges[vInt] := TRALRegexRange(vList[vInt]^);
  finally
    FreeRangeList(vList);
  end;
  vFlags := AFlags;
  vFlags.IgnoreCase := False; // \d \w \s are ASCII: folding adds nothing they lack
  Result := ClassNode(vRanges, vNeg, vFlags);
end;

function TParser.ParseHex: Cardinal;
var
  vB: Byte;
  vDigits: IntegerRAL;
  vBrace: Boolean;

  function HexValue(AB: Byte): IntegerRAL;
  begin
    case AB of
      Ord('0')..Ord('9'): Result := AB - Ord('0');
      Ord('a')..Ord('f'): Result := AB - Ord('a') + 10;
      Ord('A')..Ord('F'): Result := AB - Ord('A') + 10;
    else
      Result := -1;
    end;
  end;

begin
  Result := 0;
  vBrace := (not AtEnd) and (PeekAt(0) = Ord('{'));
  if vBrace then
    Inc(FPos);
  vDigits := 0;
  while not AtEnd do
  begin
    vB := PeekAt(0);
    if vBrace and (vB = Ord('}')) then
    begin
      Inc(FPos);
      vBrace := False;
      Break;
    end;
    if HexValue(vB) < 0 then
      Break;
    Result := Result * 16 + Cardinal(HexValue(vB));
    if Result > $10FFFF then
      Error(emRegexBadEscape);
    Inc(FPos);
    Inc(vDigits);
    if (not vBrace) and (vDigits = 2) then
      Break;
  end;
  if vBrace or (vDigits = 0) then
    Error(emRegexBadEscape);
end;

{ An escape inside or outside a class that stands for one code point; False when
  it is a Perl class (\d...), whose ranges were added to ARanges. }
function TParser.ParseEscapeInClass(var ARanges: TList; out ARune: Cardinal): Boolean;
var
  vC: Cardinal;
  vNeg: Boolean;
  vTmp: TList;
  vInt: IntegerRAL;
  vRange: ^TRALRegexRange;
  vPrev: Cardinal;
begin
  Result := True;
  ARune := 0;
  vC := Next;
  case vC of
    Ord('d'), Ord('w'), Ord('s'):
      begin
        AddPerlRanges(vC, ARanges, vNeg);
        Result := False;
      end;
    Ord('D'), Ord('W'), Ord('S'):
      begin
        // the complement, as ranges
        vTmp := TList.Create;
        try
          AddPerlRanges(vC, vTmp, vNeg);
          vPrev := 0;
          for vInt := 0 to vTmp.Count - 1 do
          begin
            if TRALRegexRange(vTmp[vInt]^).Lo > vPrev then
            begin
              New(vRange);
              vRange^.Lo := vPrev;
              vRange^.Hi := TRALRegexRange(vTmp[vInt]^).Lo - 1;
              ARanges.Add(vRange);
            end;
            vPrev := TRALRegexRange(vTmp[vInt]^).Hi + 1;
          end;
          New(vRange);
          vRange^.Lo := vPrev;
          vRange^.Hi := $10FFFF;
          ARanges.Add(vRange);
        finally
          FreeRangeList(vTmp);
        end;
        Result := False;
      end;
    Ord('n'): ARune := 10;
    Ord('t'): ARune := 9;
    Ord('r'): ARune := 13;
    Ord('f'): ARune := 12;
    Ord('v'): ARune := 11;
    Ord('a'): ARune := 7;
    Ord('x'): ARune := ParseHex;
    Ord('0')..Ord('7'):
      begin
        if (vC <> Ord('0')) and ((AtEnd) or (PeekAt(0) < Ord('0')) or (PeekAt(0) > Ord('7'))) then
          Error(emRegexBackReference);
        // octal, up to three digits (\0, \012)
        ARune := vC - Ord('0');
        vInt := 1;
        while (vInt < 3) and (not AtEnd) and (PeekAt(0) >= Ord('0')) and (PeekAt(0) <= Ord('7')) do
        begin
          ARune := ARune * 8 + Cardinal(PeekAt(0) - Ord('0'));
          Inc(FPos);
          Inc(vInt);
        end;
      end;
    Ord('8'), Ord('9'): Error(emRegexBackReference);
    Ord('p'), Ord('P'): Error(emRegexUnicodeClass);
  else
    if ((vC >= Ord('a')) and (vC <= Ord('z'))) or ((vC >= Ord('A')) and (vC <= Ord('Z'))) then
      Error(emRegexBadEscape);
    // any other escaped character is itself
    ARune := vC;
  end;
end;

function TParser.ParseClass(var AFlags: TFlags): IntegerRAL;
var
  vList: TList;
  vNeg, vFirst: Boolean;
  vLo, vHi: Cardinal;
  vRanges: array of TRALRegexRange;
  vInt, vStart: IntegerRAL;
  vRange: ^TRALRegexRange;
  vName: RawByteString;
  vNamedNeg: Boolean;

  procedure AddRange(ALo, AHi: Cardinal);
  begin
    New(vRange);
    vRange^.Lo := ALo;
    vRange^.Hi := AHi;
    vList.Add(vRange);
  end;

  procedure AddNamed(const AName: RawByteString);
  begin
    if AName = 'alpha' then begin AddRange(Ord('A'), Ord('Z')); AddRange(Ord('a'), Ord('z')); end
    else if AName = 'digit' then AddRange(Ord('0'), Ord('9'))
    else if AName = 'alnum' then begin AddRange(Ord('0'), Ord('9')); AddRange(Ord('A'), Ord('Z')); AddRange(Ord('a'), Ord('z')); end
    else if AName = 'upper' then AddRange(Ord('A'), Ord('Z'))
    else if AName = 'lower' then AddRange(Ord('a'), Ord('z'))
    else if AName = 'space' then begin AddRange(9, 13); AddRange(32, 32); end
    else if AName = 'xdigit' then begin AddRange(Ord('0'), Ord('9')); AddRange(Ord('A'), Ord('F')); AddRange(Ord('a'), Ord('f')); end
    else if AName = 'word' then begin AddRange(Ord('0'), Ord('9')); AddRange(Ord('A'), Ord('Z')); AddRange(Ord('_'), Ord('_')); AddRange(Ord('a'), Ord('z')); end
    else if AName = 'punct' then begin AddRange($21, $2F); AddRange($3A, $40); AddRange($5B, $60); AddRange($7B, $7E); end
    else if AName = 'blank' then begin AddRange(9, 9); AddRange(32, 32); end
    else if AName = 'cntrl' then begin AddRange(0, $1F); AddRange($7F, $7F); end
    else if AName = 'print' then AddRange($20, $7E)
    else if AName = 'graph' then AddRange($21, $7E)
    else if AName = 'ascii' then AddRange(0, $7F)
    else
      Error(emRegexBadClass);
  end;

begin
  // the '[' was consumed
  vStart := FPos;
  vList := TList.Create;
  try
    vNeg := False;
    if (not AtEnd) and (PeekAt(0) = Ord('^')) then
    begin
      vNeg := True;
      Inc(FPos);
    end;
    vFirst := True;
    while True do
    begin
      if AtEnd then
      begin
        FPos := vStart;
        Error(emRegexMissingBracket);
      end;
      if (PeekAt(0) = Ord(']')) and (not vFirst) then
      begin
        Inc(FPos);
        Break;
      end;
      vFirst := False;
      // [:alpha:] and [:^alpha:]
      if (PeekAt(0) = Ord('[')) and (PeekAt(1) = Ord(':')) then
      begin
        vInt := FPos + 2;
        vNamedNeg := (vInt < FLen) and (PByte(Pointer(FText))[vInt] = Ord('^'));
        if vNamedNeg then
          Inc(vInt);
        vName := '';
        while (vInt < FLen) and (PByte(Pointer(FText))[vInt] <> Ord(':')) do
        begin
          vName := vName + AnsiChar(PByte(Pointer(FText))[vInt]);
          Inc(vInt);
        end;
        if (vInt + 1 < FLen) and (PByte(Pointer(FText))[vInt + 1] = Ord(']')) then
        begin
          if vNamedNeg then
            Error(emRegexBadClass);
          AddNamed(vName);
          FPos := vInt + 2;
          Continue;
        end;
      end;
      if PeekAt(0) = Ord('\') then
      begin
        Inc(FPos);
        if not ParseEscapeInClass(vList, vLo) then
          Continue;
      end
      else
        vLo := Next;
      vHi := vLo;
      if (PeekAt(0) = Ord('-')) and (FPos + 1 < FLen) and (PeekAt(1) <> Ord(']')) then
      begin
        Inc(FPos);
        if PeekAt(0) = Ord('\') then
        begin
          Inc(FPos);
          if not ParseEscapeInClass(vList, vHi) then
            Error(emRegexBadRange);
        end
        else
          vHi := Next;
        if vHi < vLo then
          Error(emRegexBadRange);
      end;
      AddRange(vLo, vHi);
    end;
    SetLength(vRanges, vList.Count);
    for vInt := 0 to vList.Count - 1 do
      vRanges[vInt] := TRALRegexRange(vList[vInt]^);
  finally
    FreeRangeList(vList);
  end;
  Result := ClassNode(vRanges, vNeg, AFlags);
end;

function TParser.ParseAtom(var AFlags: TFlags; ADepth: IntegerRAL): IntegerRAL;
var
  vC: Cardinal;
  vB: Byte;
  vName: RawByteString;
  vCap, vInner, vInt: IntegerRAL;
  vFlags: TFlags;
  vOn, vScoped: Boolean;
  vList: TList;
  vRanges: array of TRALRegexRange;
  vRune: Cardinal;
begin
  Result := -1;
  vB := PeekAt(0);
  case vB of
    Ord('('):
      begin
        Inc(FPos);
        vFlags := AFlags;
        vCap := -1;
        if PeekAt(0) = Ord('?') then
        begin
          Inc(FPos);
          vB := PeekAt(0);
          if (vB = Ord('P')) and (PeekAt(1) = Ord('<')) then
            Inc(FPos);
          vB := PeekAt(0);
          if (vB = Ord('=')) or (vB = Ord('!')) or
             ((vB = Ord('<')) and ((PeekAt(1) = Ord('=')) or (PeekAt(1) = Ord('!')))) then
            Error(emRegexLookaround);
          if vB = Ord('<') then
          begin
            // named group
            Inc(FPos);
            vName := '';
            while (not AtEnd) and (PeekAt(0) <> Ord('>')) do
            begin
              vB := PeekAt(0);
              if not (((vB >= Ord('0')) and (vB <= Ord('9'))) or ((vB >= Ord('A')) and (vB <= Ord('Z'))) or
                      ((vB >= Ord('a')) and (vB <= Ord('z'))) or (vB = Ord('_'))) then
                Error(emRegexBadGroupName);
              vName := vName + AnsiChar(vB);
              Inc(FPos);
            end;
            if AtEnd or (vName = '') then
              Error(emRegexBadGroupName);
            Inc(FPos);
            for vInt := 0 to System.Length(FOwner.FNames) - 1 do
              if FOwner.FNames[vInt] = StringRAL(vName) then
                Error(emRegexDupGroupName);
            vCap := FCaps;
            Inc(FCaps);
            SetLength(FOwner.FNames, FCaps);
            FOwner.FNames[vCap] := StringRAL(vName);
          end
          else
          begin
            // flags: (?i) (?-i) (?i:...) (?:...)
            vOn := True;
            vScoped := False;
            while True do
            begin
              if AtEnd then
                Error(emRegexMissingParen);
              vB := PeekAt(0);
              Inc(FPos);
              case vB of
                Ord('i'): vFlags.IgnoreCase := vOn;
                Ord('m'): vFlags.MultiLine := vOn;
                Ord('s'): vFlags.DotAll := vOn;
                Ord('U'): vFlags.Ungreedy := vOn;
                Ord('-'):
                  begin
                    if not vOn then
                      Error(emRegexBadFlags);
                    vOn := False;
                  end;
                Ord(':'):
                  begin
                    vScoped := True;
                    Break;
                  end;
                Ord(')'): Break;
              else
                Error(emRegexBadFlags);
              end;
            end;
            if not vScoped then
            begin
              // (?i) changes the flags of what follows in this group
              AFlags := vFlags;
              Result := -1;
              Exit;
            end;
          end;
        end
        else
        begin
          vCap := FCaps;
          Inc(FCaps);
          SetLength(FOwner.FNames, FCaps);
          FOwner.FNames[vCap] := '';
        end;
        vInner := ParseAlternate(vFlags, ADepth + 1);
        if AtEnd or (PeekAt(0) <> Ord(')')) then
          Error(emRegexMissingParen);
        Inc(FPos);
        if vCap >= 0 then
        begin
          Result := NewNode(nkCapture);
          FNodes[Result].CapIndex := vCap;
          AddChild(Result, vInner);
        end
        else
          Result := vInner;
      end;
    Ord(')'):
      Error(emRegexUnmatchedParen);
    Ord('['):
      begin
        Inc(FPos);
        Result := ParseClass(AFlags);
      end;
    Ord('.'):
      begin
        Inc(FPos);
        Result := NewNode(nkAny);
        FNodes[Result].DotAll := AFlags.DotAll;
      end;
    Ord('^'):
      begin
        Inc(FPos);
        Result := NewNode(nkAssert);
        if AFlags.MultiLine then
          FNodes[Result].Assert := ropBeginLine
        else
          FNodes[Result].Assert := ropBeginText;
      end;
    Ord('$'):
      begin
        Inc(FPos);
        Result := NewNode(nkAssert);
        if AFlags.MultiLine then
          FNodes[Result].Assert := ropEndLine
        else
          FNodes[Result].Assert := ropEndText;
      end;
    Ord('*'), Ord('+'), Ord('?'):
      Error(emRegexMissingRepeatArg);
    Ord('\'):
      begin
        Inc(FPos);
        if AtEnd then
          Error(emRegexTrailingBackslash);
        vB := PeekAt(0);
        case vB of
          Ord('b'), Ord('B'), Ord('A'), Ord('z'):
            begin
              Inc(FPos);
              Result := NewNode(nkAssert);
              case vB of
                Ord('b'): FNodes[Result].Assert := ropWordBoundary;
                Ord('B'): FNodes[Result].Assert := ropNotWordBoundary;
                Ord('A'): FNodes[Result].Assert := ropBeginText;
                Ord('z'): FNodes[Result].Assert := ropEndText;
              end;
            end;
          Ord('Z'):
            Error(emRegexBadEscape);
          Ord('Q'):
            begin
              // \Q...\E: literal text
              Inc(FPos);
              Result := NewNode(nkConcat);
              while not AtEnd do
              begin
                if (PeekAt(0) = Ord('\')) and (PeekAt(1) = Ord('E')) then
                begin
                  Inc(FPos, 2);
                  Break;
                end;
                vC := Next;
                AddChild(Result, LiteralNode(vC, AFlags));
              end;
              if System.Length(FNodes[Result].Children) = 0 then
                FNodes[Result].Kind := nkEmpty;
            end;
        else
          vList := TList.Create;
          try
            if ParseEscapeInClass(vList, vRune) then
              Result := LiteralNode(vRune, AFlags)
            else
            begin
              SetLength(vRanges, vList.Count);
              for vInt := 0 to vList.Count - 1 do
                vRanges[vInt] := TRALRegexRange(vList[vInt]^);
              vFlags := AFlags;
              vFlags.IgnoreCase := False;
              Result := ClassNode(vRanges, False, vFlags);
            end;
          finally
            FreeRangeList(vList);
          end;
        end;
      end;
  else
    vC := Next;
    Result := LiteralNode(vC, AFlags);
  end;
end;

{ Compiler }

type
  TCompiler = class
  private
    FParser: TParser;
    FOwner: TRALRegex;
    procedure Emit(AOp: TRALRegexOp; ARune: Cardinal; AX, AY: IntegerRAL);
    procedure Patch(APC: IntegerRAL; AX, AY: IntegerRAL);
    procedure Gen(ANode: IntegerRAL);
  public
    constructor Create(AOwner: TRALRegex; AParser: TParser);
  end;

constructor TCompiler.Create(AOwner: TRALRegex; AParser: TParser);
begin
  inherited Create;
  FOwner := AOwner;
  FParser := AParser;
end;

procedure TCompiler.Emit(AOp: TRALRegexOp; ARune: Cardinal; AX, AY: IntegerRAL);
begin
  if FOwner.FProgCount >= RALRegexMaxProgram then
    raise ERALRegex.CreatePos(emRegexTooLarge, 0);
  if FOwner.FProgCount >= System.Length(FOwner.FProg) then
    SetLength(FOwner.FProg, System.Length(FOwner.FProg) * 2 + 16);
  FOwner.FProg[FOwner.FProgCount].Op := AOp;
  FOwner.FProg[FOwner.FProgCount].Rune := ARune;
  FOwner.FProg[FOwner.FProgCount].X := AX;
  FOwner.FProg[FOwner.FProgCount].Y := AY;
  Inc(FOwner.FProgCount);
end;

procedure TCompiler.Patch(APC: IntegerRAL; AX, AY: IntegerRAL);
begin
  FOwner.FProg[APC].X := AX;
  FOwner.FProg[APC].Y := AY;
end;

procedure TCompiler.Gen(ANode: IntegerRAL);
var
  vNode: ^TNode;
  vInt, vSplit, vStart, vCount, vOpt: IntegerRAL;
  vJumps: array of IntegerRAL;
  vSplits: array of IntegerRAL;
begin
  vNode := @FParser.FNodes[ANode];
  case vNode^.Kind of
    nkEmpty: ;
    nkLiteral:
      if vNode^.Fold then
        Emit(ropCharFold, vNode^.Rune, 0, 0)
      else
        Emit(ropChar, vNode^.Rune, 0, 0);
    nkClass:
      Emit(ropClass, 0, vNode^.ClassIdx, 0);
    nkAny:
      if vNode^.DotAll then
        Emit(ropAny, 0, 0, 0)
      else
        Emit(ropAnyNotNL, 0, 0, 0);
    nkAssert:
      Emit(vNode^.Assert, 0, 0, 0);
    nkConcat:
      for vInt := 0 to System.Length(vNode^.Children) - 1 do
        Gen(FParser.FNodes[ANode].Children[vInt]);
    nkCapture:
      begin
        Emit(ropSave, 0, 2 * vNode^.CapIndex, 0);
        Gen(vNode^.Children[0]);
        Emit(ropSave, 0, 2 * FParser.FNodes[ANode].CapIndex + 1, 0);
      end;
    nkAlternate:
      begin
        vCount := System.Length(vNode^.Children);
        SetLength(vJumps, vCount);
        for vInt := 0 to vCount - 1 do
        begin
          if vInt < vCount - 1 then
          begin
            vSplit := FOwner.FProgCount;
            Emit(ropSplit, 0, vSplit + 1, 0);
            Gen(FParser.FNodes[ANode].Children[vInt]);
            vJumps[vInt] := FOwner.FProgCount;
            Emit(ropJmp, 0, 0, 0);
            FOwner.FProg[vSplit].Y := FOwner.FProgCount;
          end
          else
          begin
            Gen(FParser.FNodes[ANode].Children[vInt]);
            vJumps[vInt] := -1;
          end;
        end;
        for vInt := 0 to vCount - 2 do
          FOwner.FProg[vJumps[vInt]].X := FOwner.FProgCount;
      end;
    nkRepeat:
      begin
        // the mandatory copies
        for vInt := 1 to vNode^.MinRep do
          Gen(FParser.FNodes[ANode].Children[0]);
        vNode := @FParser.FNodes[ANode];
        if vNode^.MaxRep < 0 then
        begin
          // x* after the copies (x+ = x x*)
          vStart := FOwner.FProgCount;
          Emit(ropSplit, 0, 0, 0);
          Gen(FParser.FNodes[ANode].Children[0]);
          Emit(ropJmp, 0, vStart, 0);
          if FParser.FNodes[ANode].Greedy then
            Patch(vStart, vStart + 1, FOwner.FProgCount)
          else
            Patch(vStart, FOwner.FProgCount, vStart + 1);
        end
        else
        begin
          // the optional copies, nested: x{2,4} = xx(x(x)?)?
          vOpt := vNode^.MaxRep - vNode^.MinRep;
          SetLength(vSplits, vOpt);
          for vInt := 0 to vOpt - 1 do
          begin
            vSplits[vInt] := FOwner.FProgCount;
            Emit(ropSplit, 0, 0, 0);
            Gen(FParser.FNodes[ANode].Children[0]);
          end;
          for vInt := 0 to vOpt - 1 do
            if FParser.FNodes[ANode].Greedy then
              Patch(vSplits[vInt], vSplits[vInt] + 1, FOwner.FProgCount)
            else
              Patch(vSplits[vInt], FOwner.FProgCount, vSplits[vInt] + 1);
        end;
      end;
  end;
end;

{ The longest run of case-sensitive literals every match must contain. }
function RequiredLiteral(AParser: TParser; ANode: IntegerRAL): RawByteString;
var
  vNode: ^TNode;
  vInt: IntegerRAL;
  vRun, vBest, vSub: RawByteString;
begin
  Result := '';
  vNode := @AParser.FNodes[ANode];
  case vNode^.Kind of
    nkLiteral:
      if not vNode^.Fold then
        Result := EncodeRune(vNode^.Rune);
    nkCapture:
      Result := RequiredLiteral(AParser, vNode^.Children[0]);
    nkRepeat:
      if vNode^.MinRep > 0 then
        Result := RequiredLiteral(AParser, vNode^.Children[0]);
    nkConcat:
      begin
        vRun := '';
        vBest := '';
        for vInt := 0 to System.Length(AParser.FNodes[ANode].Children) - 1 do
        begin
          vNode := @AParser.FNodes[AParser.FNodes[ANode].Children[vInt]];
          if (vNode^.Kind = nkLiteral) and (not vNode^.Fold) then
            vRun := vRun + EncodeRune(vNode^.Rune)
          else if vNode^.Kind = nkAssert then
            // takes no byte: the run goes on
          else
          begin
            if System.Length(vRun) > System.Length(vBest) then
              vBest := vRun;
            vRun := '';
            vSub := RequiredLiteral(AParser, AParser.FNodes[ANode].Children[vInt]);
            if System.Length(vSub) > System.Length(vBest) then
              vBest := vSub;
          end;
        end;
        if System.Length(vRun) > System.Length(vBest) then
          vBest := vRun;
        Result := vBest;
      end;
  end;
end;

{ Whether the tree is only case-sensitive literals, optionally between \A and \z:
  the pattern is then a plain text search. }
function PlainLiteral(AParser: TParser; ANode: IntegerRAL; out AText: RawByteString;
  out AStart, AEnd: Boolean): Boolean;
var
  vNode: ^TNode;
  vInt, vLast: IntegerRAL;
  vChild: ^TNode;
begin
  Result := False;
  AText := '';
  AStart := False;
  AEnd := False;
  vNode := @AParser.FNodes[ANode];
  if (vNode^.Kind = nkLiteral) and (not vNode^.Fold) then
  begin
    AText := EncodeRune(vNode^.Rune);
    Result := True;
    Exit;
  end;
  if vNode^.Kind <> nkConcat then
    Exit;
  vLast := System.Length(vNode^.Children) - 1;
  for vInt := 0 to vLast do
  begin
    vChild := @AParser.FNodes[AParser.FNodes[ANode].Children[vInt]];
    if (vChild^.Kind = nkLiteral) and (not vChild^.Fold) then
      AText := AText + EncodeRune(vChild^.Rune)
    else if (vChild^.Kind = nkAssert) and (vChild^.Assert = ropBeginText) and (vInt = 0) then
      AStart := True
    else if (vChild^.Kind = nkAssert) and (vChild^.Assert = ropEndText) and (vInt = vLast) then
      AEnd := True
    else
      Exit;
  end;
  Result := AText <> '';
end;

{ TRALRegexMatch }

function TRALRegexMatch.Group(AIndex: IntegerRAL): StringRAL;
begin
  Result := '';
  if (not Success) or (AIndex < 0) or (AIndex >= System.Length(Groups)) or
     (Groups[AIndex].Index = 0) then
    Exit;
  Result := Copy(Text, Groups[AIndex].Index, Groups[AIndex].Length);
end;

function TRALRegexMatch.GroupByName(const AName: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 1 to System.Length(Names) - 1 do
    if Names[vInt] = AName then
    begin
      Result := Group(vInt);
      Exit;
    end;
end;

function TRALRegexMatch.Index: IntegerRAL;
begin
  if Success then
    Result := Groups[0].Index
  else
    Result := 0;
end;

function TRALRegexMatch.Length: IntegerRAL;
begin
  if Success then
    Result := Groups[0].Length
  else
    Result := 0;
end;

function TRALRegexMatch.Value: StringRAL;
begin
  Result := Group(0);
end;

{ TRALRegex }

constructor TRALRegex.Create(const APattern: StringRAL; AOptions: TRALRegexOptions);
begin
  inherited Create;
  FPattern := APattern;
  FOptions := AOptions;
  FDFALock := TCriticalSection.Create;
  FDFAStates := TList.Create;
  SetLength(FDFABuckets, 1024);
  FillChar(FDFABuckets[0], 1024 * SizeOf(IntegerRAL), $FF);
  if System.Length(APattern) > RALRegexMaxPatternLength then
    raise ERALRegex.CreatePos(emRegexTooLarge, RALRegexMaxPatternLength);
  Compile;
end;

destructor TRALRegex.Destroy;
begin
  FreeDFA;
  FreeAndNil(FDFAStates);
  FreeAndNil(FDFALock);
  inherited Destroy;
end;

procedure TRALRegex.FreeDFA;
var
  vInt: IntegerRAL;
begin
  if FDFAStates <> nil then
  begin
    for vInt := 0 to FDFAStates.Count - 1 do
      TObject(FDFAStates[vInt]).Free;
    FDFAStates.Clear;
  end;
  if System.Length(FDFABuckets) > 0 then
    FillChar(FDFABuckets[0], System.Length(FDFABuckets) * SizeOf(IntegerRAL), $FF);
end;

procedure TRALRegex.Compile;
var
  vParser: TParser;
  vCompiler: TCompiler;
  vFlags: TFlags;
  vRoot, vInt: IntegerRAL;
  vLit: RawByteString;
  vStart, vEnd, vSimple: Boolean;
begin
  FillChar(vFlags, SizeOf(vFlags), 0);
  vFlags.IgnoreCase := roIgnoreCase in FOptions;
  vFlags.MultiLine := roMultiLine in FOptions;
  vFlags.DotAll := roDotAll in FOptions;
  FProgCount := 0;
  SetLength(FProg, 64);
  SetLength(FNames, 1);
  FNames[0] := '';
  vParser := TParser.Create(Self, RawByteString(FPattern));
  try
    vRoot := vParser.Parse(vFlags);
    FCapCount := vParser.FCaps;
    SetLength(FNames, FCapCount);
    if roFullMatch in FOptions then
    begin
      // ^(?:...)\z
      vInt := vParser.NewNode(nkConcat);
      vParser.AddChild(vInt, vParser.NewNode(nkAssert));
      vParser.FNodes[vParser.FNodes[vInt].Children[0]].Assert := ropBeginText;
      vParser.AddChild(vInt, vRoot);
      vParser.AddChild(vInt, vParser.NewNode(nkAssert));
      vParser.FNodes[vParser.FNodes[vInt].Children[2]].Assert := ropEndText;
      vRoot := vInt;
    end;
    FLiteralOnly := PlainLiteral(vParser, vRoot, vLit, vStart, vEnd);
    if FLiteralOnly then
    begin
      FLiteral := vLit;
      FAnchorStart := vStart;
      FLiteralEnd := vEnd;
    end
    else
      FLiteral := RequiredLiteral(vParser, vRoot);
    FillSkip(FLiteral, FLiteralSkip);
    vCompiler := TCompiler.Create(Self, vParser);
    try
      vCompiler.Emit(ropSave, 0, 0, 0);
      vCompiler.Gen(vRoot);
      vCompiler.Emit(ropSave, 0, 1, 0);
      vCompiler.Emit(ropMatch, 0, 0, 0);
    finally
      vCompiler.Free;
    end;
    // the DFA serves programs whose only assertions are at the text ends
    vSimple := True;
    for vInt := 0 to FProgCount - 1 do
      if FProg[vInt].Op in [ropBeginLine, ropEndLine, ropWordBoundary, ropNotWordBoundary] then
        vSimple := False;
    FDFAEnabled := vSimple;
    FAnchorStart := FAnchorStart or ((FProgCount > 1) and (FProg[1].Op = ropBeginText));
    FDFAStart[False] := -2;
    FDFAStart[True] := -2;
  finally
    vParser.Free;
  end;
end;

function TRALRegex.GetGroupName(AIndex: IntegerRAL): StringRAL;
begin
  if (AIndex >= 0) and (AIndex < System.Length(FNames)) then
    Result := FNames[AIndex]
  else
    Result := '';
end;

function TRALRegex.GroupIndex(const AName: StringRAL): IntegerRAL;
var
  vInt: IntegerRAL;
begin
  Result := -1;
  for vInt := 1 to System.Length(FNames) - 1 do
    if FNames[vInt] = AName then
    begin
      Result := vInt;
      Exit;
    end;
end;

class function TRALRegex.Escape(const AText: StringRAL): StringRAL;
var
  vInt, vStart: IntegerRAL;
  vB: Byte;
  vP: PByte;
begin
  // slices of AText, never single bytes: a byte above 127 appended alone would be
  // taken as an ANSI character and converted on Delphi
  Result := '';
  vP := PByte(Pointer(AText));
  vStart := 0;
  for vInt := 0 to System.Length(AText) - 1 do
  begin
    vB := vP[vInt];
    if AnsiChar(vB) in ['\', '.', '+', '*', '?', '(', ')', '|', '[', ']', '{', '}', '^', '$'] then
    begin
      Result := Result + Copy(AText, vStart + 1, vInt - vStart) + '\';
      vStart := vInt;
    end;
  end;
  Result := Result + Copy(AText, vStart + 1, MaxInt);
end;

function TRALRegex.ClassMatches(AClass: IntegerRAL; ARune: Cardinal): Boolean;
var
  vLo, vHi, vMid: IntegerRAL;
begin
  Result := False;
  vLo := 0;
  vHi := System.Length(FClasses[AClass].Ranges) - 1;
  while vLo <= vHi do
  begin
    vMid := (vLo + vHi) div 2;
    if ARune < FClasses[AClass].Ranges[vMid].Lo then
      vHi := vMid - 1
    else if ARune > FClasses[AClass].Ranges[vMid].Hi then
      vLo := vMid + 1
    else
    begin
      Result := True;
      Break;
    end;
  end;
  if FClasses[AClass].Negated then
    Result := not Result;
end;

{ Plain text search from byte AStart (0-based): a scan for short needles, else
  Boyer-Moore-Horspool with ASkip, the table of ALit. }
function FindBytes(AText: PByte; ALen: IntegerRAL; const ALit: RawByteString;
  AStart: IntegerRAL; const ASkip: array of IntegerRAL): IntegerRAL;
var
  vLitLen, vPos, vJ: IntegerRAL;
  vLit: PByte;
  vFirst: Byte;
begin
  Result := -1;
  vLitLen := System.Length(ALit);
  vLit := PByte(Pointer(ALit));
  if (vLitLen = 0) then
  begin
    Result := AStart;
    Exit;
  end;
  if AStart + vLitLen > ALen then
    Exit;
  if (vLitLen < 4) or (ALen - AStart < 64) then
  begin
    vFirst := vLit[0];
    for vPos := AStart to ALen - vLitLen do
      if (AText[vPos] = vFirst) and CompareMem(AText + vPos, vLit, vLitLen) then
      begin
        Result := vPos;
        Exit;
      end;
    Exit;
  end;
  vPos := AStart;
  while vPos + vLitLen <= ALen do
  begin
    vJ := vLitLen - 1;
    while (vJ >= 0) and (AText[vPos + vJ] = vLit[vJ]) do
      Dec(vJ);
    if vJ < 0 then
    begin
      Result := vPos;
      Exit;
    end;
    Inc(vPos, ASkip[AText[vPos + vLitLen - 1]]);
  end;
end;

/// Fills ASkip, the Horspool table of ALit.
procedure FillSkip(const ALit: RawByteString; var ASkip: array of IntegerRAL);
var
  vInt, vLitLen: IntegerRAL;
begin
  vLitLen := System.Length(ALit);
  for vInt := 0 to 255 do
    ASkip[vInt] := vLitLen;
  for vInt := 0 to vLitLen - 2 do
    ASkip[PByte(Pointer(ALit))[vInt]] := vLitLen - 1 - vInt;
end;

function TRALRegex.LiteralMatch(const AText: StringRAL; AStart: IntegerRAL;
  out AIndex: IntegerRAL): Boolean;
var
  vLen, vLitLen: IntegerRAL;
  vP: PByte;
begin
  Result := False;
  AIndex := -1;
  vLen := System.Length(AText);
  vLitLen := System.Length(FLiteral);
  vP := PByte(Pointer(AText));
  if FAnchorStart then
  begin
    if (AStart <> 0) or (vLen < vLitLen) or
       (not CompareMem(vP, Pointer(FLiteral), vLitLen)) then
      Exit;
    if FLiteralEnd and (vLen <> vLitLen) then
      Exit;
    AIndex := 0;
    Result := True;
  end
  else if FLiteralEnd then
  begin
    if (vLen - vLitLen < AStart) or
       (not CompareMem(vP + (vLen - vLitLen), Pointer(FLiteral), vLitLen)) then
      Exit;
    AIndex := vLen - vLitLen;
    Result := True;
  end
  else
  begin
    AIndex := FindBytes(vP, vLen, FLiteral, AStart, FLiteralSkip);
    Result := AIndex >= 0;
  end;
end;

{ Pike VM }

type
  TThreadList = record
    Dense: array of IntegerRAL;   // pcs in priority order
    Sparse: array of IntegerRAL;  // pc -> index in Dense
    Count: IntegerRAL;
    Caps: array of IntegerRAL;    // Count * NCap, the captures of each thread
  end;

procedure ListInit(var AList: TThreadList; AProgLen, ANCap: IntegerRAL);
begin
  SetLength(AList.Dense, AProgLen);
  SetLength(AList.Sparse, AProgLen);
  SetLength(AList.Caps, AProgLen * ANCap);
  AList.Count := 0;
end;

function ListHas(const AList: TThreadList; APC: IntegerRAL): Boolean;
var
  vIdx: IntegerRAL;
begin
  vIdx := AList.Sparse[APC];
  Result := (vIdx >= 0) and (vIdx < AList.Count) and (AList.Dense[vIdx] = APC);
end;

type
  TStackItem = record
    PC: IntegerRAL;
    Slot: IntegerRAL;   // >= 0: restore Caps[Slot] := Value instead of exploring
    Value: IntegerRAL;
  end;

  TPikeContext = record
    Prog: PRALRegexInst;
    Text: PByte;
    Len: IntegerRAL;
    NCap: IntegerRAL;
    Stack: array of TStackItem;
  end;

{ Adds APC and what it reaches without consuming to AList, at byte APos, with the
  captures ACaps (changed and restored on the way). }
procedure AddThread(var ACtx: TPikeContext; var AList: TThreadList; APC, APos: IntegerRAL;
  var ACaps: array of IntegerRAL);
var
  vTop, vPC, vIdx: IntegerRAL;
  vInst: PRALRegexInst;
  vPrev, vNext: Cardinal;
  vTmp: IntegerRAL;
  vOk: Boolean;
begin
  vTop := 0;
  if System.Length(ACtx.Stack) < 16 then
    SetLength(ACtx.Stack, 16);
  ACtx.Stack[0].PC := APC;
  ACtx.Stack[0].Slot := -1;
  vTop := 1;
  while vTop > 0 do
  begin
    Dec(vTop);
    if ACtx.Stack[vTop].Slot >= 0 then
    begin
      ACaps[ACtx.Stack[vTop].Slot] := ACtx.Stack[vTop].Value;
      Continue;
    end;
    vPC := ACtx.Stack[vTop].PC;
    while True do
    begin
      if ListHas(AList, vPC) then
        Break;
      vIdx := AList.Count;
      AList.Sparse[vPC] := vIdx;
      AList.Dense[vIdx] := vPC;
      Inc(AList.Count);
      vInst := ACtx.Prog;
      Inc(vInst, vPC);
      case vInst^.Op of
        ropJmp:
          begin
            vPC := vInst^.X;
            Continue;
          end;
        ropSplit:
          begin
            if vTop + 2 >= System.Length(ACtx.Stack) then
              SetLength(ACtx.Stack, System.Length(ACtx.Stack) * 2);
            ACtx.Stack[vTop].PC := vInst^.Y;
            ACtx.Stack[vTop].Slot := -1;
            Inc(vTop);
            vPC := vInst^.X;
            Continue;
          end;
        ropSave:
          begin
            if vInst^.X < ACtx.NCap then
            begin
              if vTop + 2 >= System.Length(ACtx.Stack) then
                SetLength(ACtx.Stack, System.Length(ACtx.Stack) * 2);
              ACtx.Stack[vTop].Slot := vInst^.X;
              ACtx.Stack[vTop].Value := ACaps[vInst^.X];
              Inc(vTop);
              ACaps[vInst^.X] := APos;
            end;
            vPC := vPC + 1;
            Continue;
          end;
        ropBeginLine, ropEndLine, ropBeginText, ropEndText,
        ropWordBoundary, ropNotWordBoundary:
          begin
            case vInst^.Op of
              ropBeginText: vOk := APos = 0;
              ropEndText: vOk := APos = ACtx.Len;
              ropBeginLine: vOk := (APos = 0) or (ACtx.Text[APos - 1] = 10);
              ropEndLine: vOk := (APos = ACtx.Len) or (ACtx.Text[APos] = 10);
            else
              begin
                vPrev := RuneBefore(ACtx.Text, APos);
                if APos < ACtx.Len then
                begin
                  vTmp := APos;
                  vNext := DecodeRune(ACtx.Text, ACtx.Len, vTmp);
                end
                else
                  vNext := 0;
                vOk := ((APos > 0) and IsWordRune(vPrev)) <> ((APos < ACtx.Len) and IsWordRune(vNext));
                if vInst^.Op = ropNotWordBoundary then
                  vOk := not vOk;
              end;
            end;
            if not vOk then
              Break;
            vPC := vPC + 1;
            Continue;
          end;
      else
        // a consuming instruction or Match: the thread waits here with its captures
        if ACtx.NCap > 0 then
          Move(ACaps[0], AList.Caps[vIdx * ACtx.NCap], ACtx.NCap * SizeOf(IntegerRAL));
        Break;
      end;
    end;
  end;
end;

function TRALRegex.PikeSearch(const AText: StringRAL; AStart: IntegerRAL;
  ACaps: TRALRegexGroups; AWantCaps: Boolean): Boolean;
var
  vCtx: TPikeContext;
  vCur, vNext, vTmpList: TThreadList;
  vPos, vNextPos, vInt, vPC, vNCap, vLitPos: IntegerRAL;
  vRune: Cardinal;
  vTmpCaps, vBest: array of IntegerRAL;
  vInst: PRALRegexInst;
  vOk, vMatched: Boolean;
begin
  Result := False;
  if AWantCaps then
    vNCap := 2 * FCapCount
  else
    vNCap := 2;
  vCtx.Prog := @FProg[0];
  vCtx.Text := PByte(Pointer(AText));
  vCtx.Len := System.Length(AText);
  vCtx.NCap := vNCap;
  SetLength(vCtx.Stack, 16);
  ListInit(vCur, FProgCount, vNCap);
  ListInit(vNext, FProgCount, vNCap);
  SetLength(vTmpCaps, vNCap);
  SetLength(vBest, vNCap);
  vMatched := False;
  vPos := AStart;
  while True do
  begin
    if (not vMatched) and ((not FAnchorStart) or (vPos = 0)) then
    begin
      if (vCur.Count = 0) and (FLiteral <> '') and (not FAnchorStart) then
      begin
        // nothing running: jump to where the required literal can still be
        vLitPos := FindBytes(vCtx.Text, vCtx.Len, FLiteral, vPos, FLiteralSkip);
        if vLitPos < 0 then
          Break;
      end;
      for vInt := 0 to vNCap - 1 do
        vTmpCaps[vInt] := -1;
      AddThread(vCtx, vCur, 0, vPos, vTmpCaps);
    end;
    if vCur.Count = 0 then
      Break;
    if vPos < vCtx.Len then
    begin
      vNextPos := vPos;
      vRune := DecodeRune(vCtx.Text, vCtx.Len, vNextPos);
    end
    else
    begin
      vNextPos := vPos;
      vRune := $FFFFFFFF;
    end;
    vNext.Count := 0;
    for vInt := 0 to vCur.Count - 1 do
    begin
      vPC := vCur.Dense[vInt];
      vInst := @FProg[vPC];
      if vInst^.Op = ropMatch then
      begin
        Move(vCur.Caps[vInt * vNCap], vBest[0], vNCap * SizeOf(IntegerRAL));
        vMatched := True;
        // leftmost-first: the threads after this one have lower priority
        Break;
      end;
      if vPos >= vCtx.Len then
        Continue;
      case vInst^.Op of
        ropChar: vOk := vRune = vInst^.Rune;
        ropCharFold: vOk := FoldRune(vRune) = vInst^.Rune;
        ropClass: vOk := ClassMatches(vInst^.X, vRune);
        ropAny: vOk := True;
        ropAnyNotNL: vOk := vRune <> 10;
      else
        vOk := False;
      end;
      if vOk then
      begin
        Move(vCur.Caps[vInt * vNCap], vTmpCaps[0], vNCap * SizeOf(IntegerRAL));
        AddThread(vCtx, vNext, vPC + 1, vNextPos, vTmpCaps);
      end;
    end;
    if vPos >= vCtx.Len then
      Break;
    vTmpList := vCur;
    vCur := vNext;
    vNext := vTmpList;
    vPos := vNextPos;
  end;
  Result := vMatched;
  if vMatched and (ACaps <> nil) then
    for vInt := 0 to (vNCap div 2) - 1 do
    begin
      if (vBest[2 * vInt] >= 0) and (vBest[2 * vInt + 1] >= 0) then
      begin
        ACaps[vInt].Index := vBest[2 * vInt] + 1;
        ACaps[vInt].Length := vBest[2 * vInt + 1] - vBest[2 * vInt];
      end
      else
      begin
        ACaps[vInt].Index := 0;
        ACaps[vInt].Length := 0;
      end;
    end;
end;

{ Lazy DFA }

{ The closure of the instructions of APCs (in order, already followed through
  jumps and splits) is the state; this builds it from start points. }
procedure DFAClosure(ARegex: TRALRegex; var ASet: TThreadList; APC: IntegerRAL;
  AAtStart: Boolean; AAtEnd: Boolean = False);
var
  vStack: array of IntegerRAL;
  vTop, vPC, vIdx: IntegerRAL;
begin
  SetLength(vStack, 16);
  vStack[0] := APC;
  vTop := 1;
  while vTop > 0 do
  begin
    Dec(vTop);
    vPC := vStack[vTop];
    while True do
    begin
      if ListHas(ASet, vPC) then
        Break;
      vIdx := ASet.Count;
      ASet.Sparse[vPC] := vIdx;
      ASet.Dense[vIdx] := vPC;
      Inc(ASet.Count);
      case ARegex.FProg[vPC].Op of
        ropJmp:
          begin
            vPC := ARegex.FProg[vPC].X;
            Continue;
          end;
        ropSplit:
          begin
            if vTop + 1 >= System.Length(vStack) then
              SetLength(vStack, System.Length(vStack) * 2);
            vStack[vTop] := ARegex.FProg[vPC].Y;
            Inc(vTop);
            vPC := ARegex.FProg[vPC].X;
            Continue;
          end;
        ropSave:
          begin
            vPC := vPC + 1;
            Continue;
          end;
        ropBeginText:
          begin
            if not AAtStart then
              Break;
            vPC := vPC + 1;
            Continue;
          end;
        ropEndText:
          begin
            // decided when the text ends: kept in the set until then
            if not AAtEnd then
              Break;
            vPC := vPC + 1;
            Continue;
          end;
      else
        // consuming or Match
        Break;
      end;
    end;
  end;
end;

function TRALRegex.AddDFAState(const APCs: array of IntegerRAL; ACount: IntegerRAL): IntegerRAL;
var
  vInt, vOut, vIdx: IntegerRAL;
  vState: TRALRegexDFAState;
  vKept: array of IntegerRAL;
  vHash: Cardinal;
  vSame: Boolean;
begin
  // only the instructions that matter for the future: consuming, Match, EndText
  SetLength(vKept, ACount);
  vOut := 0;
  {$IFOPT Q+}{$DEFINE RALRegexQ}{$Q-}{$ENDIF}
  {$IFOPT R+}{$DEFINE RALRegexR}{$R-}{$ENDIF}
  vHash := 2166136261;
  for vInt := 0 to ACount - 1 do
    if FProg[APCs[vInt]].Op in [ropChar, ropCharFold, ropClass, ropAny, ropAnyNotNL,
      ropMatch, ropEndText] then
    begin
      vKept[vOut] := APCs[vInt];
      vHash := (vHash xor Cardinal(APCs[vInt])) * 16777619;
      Inc(vOut);
    end;
  {$IFDEF RALRegexQ}{$Q+}{$UNDEF RALRegexQ}{$ENDIF}
  {$IFDEF RALRegexR}{$R+}{$UNDEF RALRegexR}{$ENDIF}
  vIdx := FDFABuckets[vHash and 1023];
  while vIdx >= 0 do
  begin
    vState := FDFAArray[vIdx];
    if (vState.Hash = vHash) and (System.Length(vState.PCs) = vOut) then
    begin
      vSame := True;
      for vInt := 0 to vOut - 1 do
        if vState.PCs[vInt] <> vKept[vInt] then
        begin
          vSame := False;
          Break;
        end;
      if vSame then
      begin
        Result := vIdx;
        Exit;
      end;
    end;
    vIdx := vState.Chain;
  end;
  if FDFAStates.Count >= RALRegexMaxDFAStates then
  begin
    FDFAFull := True;
    Result := -3;
    Exit;
  end;
  vState := TRALRegexDFAState.Create;
  vState.Hash := vHash;
  SetLength(vState.PCs, vOut);
  for vInt := 0 to vOut - 1 do
  begin
    vState.PCs[vInt] := vKept[vInt];
    if FProg[vKept[vInt]].Op = ropMatch then
      vState.Accepting := True;
    if FProg[vKept[vInt]].Op = ropEndText then
      vState.HasEndText := True;
  end;
  for vInt := 0 to 127 do
    vState.Next[vInt] := -2;
  vState.AcceptsAtEnd := -1;
  Result := FDFAStates.Add(vState);
  vState.Id := Result;
  if Result >= System.Length(FDFAArray) then
    SetLength(FDFAArray, System.Length(FDFAArray) * 2 + 16);
  FDFAArray[Result] := vState;
  vState.Chain := FDFABuckets[vHash and 1023];
  FDFABuckets[vHash and 1023] := Result;
end;

function TRALRegex.DFAStep(AState: IntegerRAL; ARune: Cardinal): IntegerRAL;
var
  vState: TRALRegexDFAState;
  vSet: TThreadList;
  vInt, vPC: IntegerRAL;
  vOk: Boolean;
begin
  vState := FDFAArray[AState];
  if (ARune < 128) and (vState.Next[ARune] <> -2) then
  begin
    Result := vState.Next[ARune];
    Exit;
  end;
  ListInit(vSet, FProgCount, 0);
  for vInt := 0 to System.Length(vState.PCs) - 1 do
  begin
    vPC := vState.PCs[vInt];
    case FProg[vPC].Op of
      ropChar: vOk := ARune = FProg[vPC].Rune;
      ropCharFold: vOk := FoldRune(ARune) = FProg[vPC].Rune;
      ropClass: vOk := ClassMatches(FProg[vPC].X, ARune);
      ropAny: vOk := True;
      ropAnyNotNL: vOk := ARune <> 10;
    else
      vOk := False;
    end;
    if vOk then
      DFAClosure(Self, vSet, vPC + 1, False);
  end;
  // unanchored: a match may still start here
  if not FAnchorStart then
    DFAClosure(Self, vSet, 0, False);
  if vSet.Count = 0 then
    Result := -1
  else
    Result := AddDFAState(vSet.Dense, vSet.Count);
  // the state object did not move: the list holds references
  if (ARune < 128) and (Result <> -3) then
    vState.Next[ARune] := Result;
end;

{ Whether there is a match at or after byte AStart (0-based), by the DFA; AUsed is
  False when the DFA could not answer (busy, full, or a program it does not run). }
function TRALRegex.DFAMatch(const AText: StringRAL; AStart: IntegerRAL;
  out AUsed: Boolean): Boolean;
var
  vSet: TThreadList;
  vState, vPos, vLen, vInt, vPC: IntegerRAL;
  vP: PByte;
  vRune: Cardinal;
  vDFA: TRALRegexDFAState;
  vAtStart: Boolean;
begin
  Result := False;
  AUsed := False;
  if (not FDFAEnabled) or FDFAFull then
    Exit;
  if FAnchorStart and (AStart > 0) then
  begin
    AUsed := True;
    Exit;
  end;
  if not FDFALock.TryEnter then
    Exit;
  try
    vAtStart := AStart = 0;
    vState := FDFAStart[vAtStart];
    if vState = -2 then
    begin
      ListInit(vSet, FProgCount, 0);
      DFAClosure(Self, vSet, 0, vAtStart);
      vState := AddDFAState(vSet.Dense, vSet.Count);
      if vState = -3 then
        Exit;
      FDFAStart[vAtStart] := vState;
    end;
    vP := PByte(Pointer(AText));
    vLen := System.Length(AText);
    vPos := AStart;
    vDFA := FDFAArray[vState];
    while True do
    begin
      if vDFA.Accepting then
      begin
        Result := True;
        AUsed := True;
        Exit;
      end;
      if vPos >= vLen then
        Break;
      if vP[vPos] < $80 then
      begin
        // the cached transition, without a call
        vRune := vP[vPos];
        Inc(vPos);
        vState := vDFA.Next[vRune];
        if vState = -2 then
          vState := DFAStep(vDFA.Id, vRune);
      end
      else
      begin
        vRune := DecodeRune(vP, vLen, vPos);
        vState := DFAStep(vDFA.Id, vRune);
      end;
      if vState = -3 then
        Exit;
      if vState = -1 then
      begin
        AUsed := True;
        Exit;
      end;
      vDFA := FDFAArray[vState];
    end;
    // the end of the text: the EndText instructions may lead to Match (once per state)
    if vDFA.HasEndText then
    begin
      if (vDFA.AcceptsAtEnd < 0) or (vLen = 0) then
      begin
        ListInit(vSet, FProgCount, 0);
        for vInt := 0 to System.Length(vDFA.PCs) - 1 do
        begin
          vPC := vDFA.PCs[vInt];
          if FProg[vPC].Op = ropEndText then
            DFAClosure(Self, vSet, vPC + 1, vLen = 0, True);
        end;
        for vInt := 0 to vSet.Count - 1 do
          if FProg[vSet.Dense[vInt]].Op = ropMatch then
            Result := True;
        // an empty text may pass \A after \z: not cached, it is not the common answer
        if vLen > 0 then
          vDFA.AcceptsAtEnd := Ord(Result);
      end
      else
        Result := vDFA.AcceptsAtEnd = 1;
    end;
    AUsed := True;
  finally
    FDFALock.Leave;
  end;
end;

function TRALRegex.IsMatch(const AText: StringRAL): Boolean;
var
  vIdx: IntegerRAL;
  vUsed: Boolean;
begin
  if FLiteralOnly then
  begin
    Result := LiteralMatch(AText, 0, vIdx);
    Exit;
  end;
  // the required literal first: no literal, no match
  if (FLiteral <> '') and (FindBytes(PByte(Pointer(AText)), System.Length(AText), FLiteral, 0, FLiteralSkip) < 0) then
  begin
    Result := False;
    Exit;
  end;
  Result := DFAMatch(AText, 0, vUsed);
  if not vUsed then
    Result := PikeSearch(AText, 0, nil, False);
end;

function TRALRegex.Match(const AText: StringRAL; AStart: IntegerRAL): TRALRegexMatch;
var
  vIdx, vInt: IntegerRAL;
  vUsed: Boolean;
begin
  Result.Success := False;
  Result.Text := AText;
  Result.Groups := nil;
  SetLength(Result.Names, System.Length(FNames));
  for vInt := 0 to System.Length(FNames) - 1 do
    Result.Names[vInt] := FNames[vInt];
  if AStart < 1 then
    AStart := 1;
  if AStart > System.Length(AText) + 1 then
    Exit;
  if FLiteralOnly then
  begin
    if LiteralMatch(AText, AStart - 1, vIdx) then
    begin
      Result.Success := True;
      SetLength(Result.Groups, 1);
      Result.Groups[0].Index := vIdx + 1;
      Result.Groups[0].Length := System.Length(FLiteral);
    end;
    Exit;
  end;
  if (FLiteral <> '') and
     (FindBytes(PByte(Pointer(AText)), System.Length(AText), FLiteral, AStart - 1, FLiteralSkip) < 0) then
    Exit;
  // the DFA rejects fast; the captures need the Pike VM
  if not DFAMatch(AText, AStart - 1, vUsed) and vUsed then
    Exit;
  SetLength(Result.Groups, FCapCount);
  Result.Success := PikeSearch(AText, AStart - 1, Result.Groups, True);
  if not Result.Success then
    Result.Groups := nil;
end;

function TRALRegex.Matches(const AText: StringRAL): TRALRegexMatches;
var
  vPos, vCount, vEnd: IntegerRAL;
  vMatch: TRALRegexMatch;
  vP: PByte;
  vLen: IntegerRAL;
begin
  Result := nil;
  vCount := 0;
  vPos := 1;
  vP := PByte(Pointer(AText));
  vLen := System.Length(AText);
  while vPos <= vLen + 1 do
  begin
    vMatch := Match(AText, vPos);
    if not vMatch.Success then
      Break;
    SetLength(Result, vCount + 1);
    Result[vCount] := vMatch;
    Inc(vCount);
    vEnd := vMatch.Index + vMatch.Length;
    if vMatch.Length = 0 then
    begin
      // an empty match: step one character
      vEnd := vMatch.Index - 1;
      if vEnd >= vLen then
        Break;
      DecodeRune(vP, vLen, vEnd);
      vEnd := vEnd + 1;
    end;
    vPos := vEnd;
  end;
end;

function TRALRegex.Replace(const AText, AReplacement: StringRAL): StringRAL;
var
  vList: TRALRegexMatches;
  vInt, vLast, vPos, vNum, vRepLen, vRun: IntegerRAL;
  vRep: PByte;
  vName: RawByteString;
  vB: Byte;
begin
  vList := Matches(AText);
  if System.Length(vList) = 0 then
  begin
    Result := AText;
    Exit;
  end;
  Result := '';
  vLast := 1;
  vRep := PByte(Pointer(AReplacement));
  vRepLen := System.Length(AReplacement);
  for vInt := 0 to System.Length(vList) - 1 do
  begin
    Result := Result + Copy(AText, vLast, vList[vInt].Index - vLast);
    vPos := 0;
    vRun := 0;
    while vPos < vRepLen do
    begin
      vB := vRep[vPos];
      if (vB = Ord('$')) and (vPos + 1 < vRepLen) then
      begin
        // the literal run before the '$', as a slice
        Result := Result + Copy(AReplacement, vRun + 1, vPos - vRun);
        vB := vRep[vPos + 1];
        if vB = Ord('$') then
        begin
          Result := Result + '$';
          Inc(vPos, 2);
          vRun := vPos;
          Continue;
        end;
        if (vB >= Ord('0')) and (vB <= Ord('9')) then
        begin
          vNum := vB - Ord('0');
          Inc(vPos, 2);
          if (vPos < vRepLen) and (vRep[vPos] >= Ord('0')) and (vRep[vPos] <= Ord('9')) and
             (vNum * 10 + (vRep[vPos] - Ord('0')) < FCapCount) then
          begin
            vNum := vNum * 10 + (vRep[vPos] - Ord('0'));
            Inc(vPos);
          end;
          Result := Result + vList[vInt].Group(vNum);
          vRun := vPos;
          Continue;
        end;
        if vB = Ord('{') then
        begin
          vName := '';
          Inc(vPos, 2);
          while (vPos < vRepLen) and (vRep[vPos] <> Ord('}')) do
          begin
            vName := vName + AnsiChar(vRep[vPos]);
            Inc(vPos);
          end;
          Inc(vPos);
          if (vName <> '') and (vName[1] >= '0') and (vName[1] <= '9') then
            Result := Result + vList[vInt].Group(StrToIntDef(string(vName), -1))
          else
            Result := Result + vList[vInt].GroupByName(StringRAL(vName));
          vRun := vPos;
          Continue;
        end;
        // a '$' followed by anything else is itself
        vRun := vPos;
      end;
      Inc(vPos);
    end;
    Result := Result + Copy(AReplacement, vRun + 1, MaxInt);
    vLast := vList[vInt].Index + vList[vInt].Length;
  end;
  Result := Result + Copy(AText, vLast, MaxInt);
end;

function TRALRegex.Split(const AText: StringRAL): TRALRegexPieces;
var
  vList: TRALRegexMatches;
  vInt, vLast, vCount: IntegerRAL;
begin
  vList := Matches(AText);
  SetLength(Result, System.Length(vList) + 1);
  vLast := 1;
  vCount := 0;
  for vInt := 0 to System.Length(vList) - 1 do
  begin
    // an empty match at the very start splits nothing off
    if (vList[vInt].Length = 0) and (vList[vInt].Index = 1) then
      Continue;
    Result[vCount] := Copy(AText, vLast, vList[vInt].Index - vLast);
    Inc(vCount);
    vLast := vList[vInt].Index + vList[vInt].Length;
  end;
  Result[vCount] := Copy(AText, vLast, MaxInt);
  SetLength(Result, vCount + 1);
end;

function RALRegexIsMatch(const AText, APattern: StringRAL; AOptions: TRALRegexOptions): Boolean;
var
  vRegex: TRALRegex;
begin
  vRegex := TRALRegex.Create(APattern, AOptions);
  try
    Result := vRegex.IsMatch(AText);
  finally
    vRegex.Free;
  end;
end;

initialization
  FillCasePairs;

end.
