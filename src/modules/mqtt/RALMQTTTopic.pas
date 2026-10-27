/// Topic Names and Topic Filters: validation, matching (section 4.7 of both
/// standards), shared subscriptions ($share/), and the topic tree the broker
/// uses to find the subscribers of a message without walking every
/// subscription.
unit RALMQTTTopic;

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
  RALTypes, RALMQTTTypes;

type
  /// The levels of a topic, split at '/'.
  TRALMQTTLevels = array of StringRAL;

  { One level of the topic tree: the subscriptions whose filter ends here, and
    the next levels, by name, plus the '+' and '#' wildcards apart }
  TRALMQTTTopicNode = class
  private
    FChildren: TStringList;
    FHash: TRALMQTTTopicNode;
    FItems: TList;
    FParent: TRALMQTTTopicNode;
    FPlus: TRALMQTTTopicNode;
    FName: StringRAL;

    function IsEmpty: boolean;
  public
    constructor Create(AParent: TRALMQTTTopicNode; const AName: StringRAL);
    destructor Destroy; override;
  end;

  { The subscriptions of a broker, filed by the levels of their filters. Add
    and Remove take any pointer as the subscription; Match collects the ones
    whose filter matches a Topic Name, descending only the branches that can
    match. Not thread-safe: the broker holds its own lock around it }
  TRALMQTTTopicTree = class
  private
    FCount: IntegerRAL;
    FRoot: TRALMQTTTopicNode;

    function FindNode(const AFilter: StringRAL; ACreate: boolean): TRALMQTTTopicNode;
    procedure Prune(ANode: TRALMQTTTopicNode);
  public
    constructor Create;
    destructor Destroy; override;

    /// Files AItem under AFilter (already validated).
    procedure Add(const AFilter: StringRAL; AItem: Pointer);
    /// Removes everything.
    procedure Clear;
    { Adds to AList every item whose filter matches ATopic (a Topic Name), the
      '$' rule included; an item filed under several matching filters comes
      once per filter }
    procedure Match(const ATopic: StringRAL; AList: TList);
    /// Takes AItem out of AFilter; False when it was not there.
    function Remove(const AFilter: StringRAL; AItem: Pointer): boolean;

    /// Items filed.
    property Count: IntegerRAL read FCount;
  end;

{ True when AName can be a Topic Name: 1 to 65 535 bytes of valid UTF-8, no
  wildcard [MQTT-4.7.0-1], [MQTT-4.7.3-1] }
function RALMQTTValidTopicName(const AName: StringRAL): boolean;
{ True when AFilter can be a Topic Filter: '#' only as the last level and
  alone in it, '+' alone in its level [MQTT-4.7.1-1], [MQTT-4.7.1-2]; a
  shared one ($share/name/filter) needs a name without '/', '+' or '#' and a
  filter after it [MQTT-4.8.2-1], [MQTT-4.8.2-2] }
function RALMQTTValidTopicFilter(const AFilter: StringRAL): boolean;
{ True when AFilter matches ATopic. A topic starting with '$' is not matched
  by a filter starting with a wildcard [MQTT-4.7.2-1]. Comparison is byte by
  byte, no normalisation [MQTT-4.7.3-4] }
function RALMQTTTopicMatches(const AFilter, ATopic: StringRAL): boolean;
{ Splits a shared subscription filter: True for '$share/AShareName/AFilter',
  False (and AFilter is the whole text) for any other }
function RALMQTTSplitShared(const AText: StringRAL; out AShareName,
  AFilter: StringRAL): boolean;
/// True when AFilter has '+' or '#'.
function RALMQTTHasWildcard(const AFilter: StringRAL): boolean;

implementation

const
  cShare: StringRAL = '$share/';

function RALMQTTHasWildcard(const AFilter: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  Result := False;
  for vInt := 1 to Length(AFilter) do
    if (AFilter[vInt] = '+') or (AFilter[vInt] = '#') then
    begin
      Result := True;
      Exit;
    end;
end;

function RALMQTTValidTopicName(const AName: StringRAL): boolean;
begin
  Result := (AName <> '') and (Length(AName) <= 65535) and
            (not RALMQTTHasWildcard(AName)) and RALMQTTValidString(AName);
end;

function ValidPlainFilter(const AFilter: StringRAL): boolean;
var
  vInt, vLen: IntegerRAL;
  vChar: AnsiChar;
  vLevelStart: boolean;
begin
  Result := False;
  vLen := Length(AFilter);
  if (vLen = 0) or (vLen > 65535) then
    Exit;
  vLevelStart := True;
  for vInt := 1 to vLen do
  begin
    vChar := AnsiChar(AFilter[vInt]);
    if vChar = '#' then
    begin
      // alone in its level, and that level is the last one
      if (not vLevelStart) or (vInt <> vLen) then
        Exit;
    end
    else if vChar = '+' then
    begin
      if (not vLevelStart) or ((vInt < vLen) and (AFilter[vInt + 1] <> '/')) then
        Exit;
    end;
    vLevelStart := vChar = '/';
  end;
  Result := RALMQTTValidString(AFilter);
end;

function RALMQTTSplitShared(const AText: StringRAL; out AShareName,
  AFilter: StringRAL): boolean;
var
  vRest: StringRAL;
  vPos: IntegerRAL;
begin
  Result := False;
  AShareName := '';
  AFilter := AText;
  if Copy(AText, 1, Length(cShare)) <> cShare then
    Exit;
  vRest := Copy(AText, Length(cShare) + 1, MaxInt);
  vPos := Pos(StringRAL('/'), vRest);
  if vPos <= 0 then
  begin
    // '$share/name' with no filter: the name, and an empty filter
    AShareName := vRest;
    AFilter := '';
    Result := True;
    Exit;
  end;
  AShareName := Copy(vRest, 1, vPos - 1);
  AFilter := Copy(vRest, vPos + 1, MaxInt);
  Result := True;
end;

function RALMQTTValidTopicFilter(const AFilter: StringRAL): boolean;
var
  vShare, vFilter: StringRAL;
begin
  if RALMQTTSplitShared(AFilter, vShare, vFilter) then
    Result := (vShare <> '') and (Pos(StringRAL('+'), vShare) = 0) and
              (Pos(StringRAL('#'), vShare) = 0) and ValidPlainFilter(vFilter) and
              RALMQTTValidString(vShare)
  else
    Result := ValidPlainFilter(AFilter);
end;

{ the levels of a topic or filter: 'a//b' is three, the middle one empty }
procedure SplitLevels(const AText: StringRAL; var ALevels: TRALMQTTLevels;
  out ACount: IntegerRAL);
var
  vStart, vEnd, vLen: IntegerRAL;
begin
  ACount := 0;
  vLen := Length(AText);
  vStart := 1;
  repeat
    vEnd := vStart;
    while (vEnd <= vLen) and (AText[vEnd] <> '/') do
      Inc(vEnd);
    if ACount = Length(ALevels) then
      SetLength(ALevels, ACount * 2 + 8);
    ALevels[ACount] := Copy(AText, vStart, vEnd - vStart);
    Inc(ACount);
    vStart := vEnd + 1;
  until vEnd > vLen;
end;

function RALMQTTTopicMatches(const AFilter, ATopic: StringRAL): boolean;
var
  vFilter, vTopic: TRALMQTTLevels;
  vFCount, vTCount, vInt: IntegerRAL;
begin
  Result := False;
  if (AFilter = '') or (ATopic = '') then
    Exit;
  // the '$' rule: $SYS/... is not reached by '#' or '+/...'
  if (ATopic[1] = '$') and ((AFilter[1] = '#') or (AFilter[1] = '+')) then
    Exit;
  SplitLevels(AFilter, vFilter, vFCount);
  SplitLevels(ATopic, vTopic, vTCount);
  for vInt := 0 to vFCount - 1 do
  begin
    // '#' takes this level and everything below it, the parent level too
    if vFilter[vInt] = '#' then
    begin
      Result := True;
      Exit;
    end;
    if vInt >= vTCount then
      Exit;
    if (vFilter[vInt] <> '+') and (vFilter[vInt] <> vTopic[vInt]) then
      Exit;
  end;
  Result := vFCount = vTCount;
end;

{ TRALMQTTTopicNode }

constructor TRALMQTTTopicNode.Create(AParent: TRALMQTTTopicNode; const AName: StringRAL);
begin
  inherited Create;
  FParent := AParent;
  FName := AName;
  FItems := TList.Create;
end;

destructor TRALMQTTTopicNode.Destroy;
var
  vInt: IntegerRAL;
begin
  if FChildren <> nil then
  begin
    for vInt := 0 to FChildren.Count - 1 do
      FChildren.Objects[vInt].Free;
    FreeAndNil(FChildren);
  end;
  FreeAndNil(FPlus);
  FreeAndNil(FHash);
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TRALMQTTTopicNode.IsEmpty: boolean;
begin
  Result := (FItems.Count = 0) and (FPlus = nil) and (FHash = nil) and
            ((FChildren = nil) or (FChildren.Count = 0));
end;

{ TRALMQTTTopicTree }

constructor TRALMQTTTopicTree.Create;
begin
  inherited Create;
  FRoot := TRALMQTTTopicNode.Create(nil, '');
end;

destructor TRALMQTTTopicTree.Destroy;
begin
  FreeAndNil(FRoot);
  inherited Destroy;
end;

procedure TRALMQTTTopicTree.Add(const AFilter: StringRAL; AItem: Pointer);
begin
  FindNode(AFilter, True).FItems.Add(AItem);
  Inc(FCount);
end;

procedure TRALMQTTTopicTree.Clear;
begin
  FreeAndNil(FRoot);
  FRoot := TRALMQTTTopicNode.Create(nil, '');
  FCount := 0;
end;

function TRALMQTTTopicTree.FindNode(const AFilter: StringRAL;
  ACreate: boolean): TRALMQTTTopicNode;
var
  vStart, vEnd, vLen, vIndex: IntegerRAL;
  vLevel: StringRAL;
  vNext: TRALMQTTTopicNode;
begin
  Result := FRoot;
  vLen := Length(AFilter);
  vStart := 1;
  while Result <> nil do
  begin
    vEnd := vStart;
    while (vEnd <= vLen) and (AFilter[vEnd] <> '/') do
      Inc(vEnd);
    vLevel := Copy(AFilter, vStart, vEnd - vStart);

    if vLevel = '+' then
    begin
      vNext := Result.FPlus;
      if (vNext = nil) and ACreate then
      begin
        vNext := TRALMQTTTopicNode.Create(Result, vLevel);
        Result.FPlus := vNext;
      end;
    end
    else if vLevel = '#' then
    begin
      vNext := Result.FHash;
      if (vNext = nil) and ACreate then
      begin
        vNext := TRALMQTTTopicNode.Create(Result, vLevel);
        Result.FHash := vNext;
      end;
    end
    else
    begin
      vNext := nil;
      if Result.FChildren <> nil then
      begin
        vIndex := Result.FChildren.IndexOf(string(vLevel));
        if vIndex >= 0 then
          vNext := TRALMQTTTopicNode(Result.FChildren.Objects[vIndex]);
      end;
      if (vNext = nil) and ACreate then
      begin
        if Result.FChildren = nil then
        begin
          Result.FChildren := TStringList.Create;
          Result.FChildren.CaseSensitive := True;
          Result.FChildren.Sorted := True;
          Result.FChildren.Duplicates := dupError;
        end;
        vNext := TRALMQTTTopicNode.Create(Result, vLevel);
        Result.FChildren.AddObject(string(vLevel), vNext);
      end;
    end;
    Result := vNext;
    if vEnd > vLen then
      Break;
    vStart := vEnd + 1;
  end;
end;

procedure TRALMQTTTopicTree.Match(const ATopic: StringRAL; AList: TList);
var
  vLevels: TRALMQTTLevels;
  vCount: IntegerRAL;

  procedure AddItems(ANode: TRALMQTTTopicNode);
  var
    vInt: IntegerRAL;
  begin
    for vInt := 0 to ANode.FItems.Count - 1 do
      AList.Add(ANode.FItems[vInt]);
  end;

  procedure Walk(ANode: TRALMQTTTopicNode; ALevel: IntegerRAL);
  var
    vIndex: IntegerRAL;
  begin
    // '#' here matches this level and all below; the '$' rule at the root
    if (ANode.FHash <> nil) and not ((ALevel = 0) and (vLevels[0] <> '') and
       (vLevels[0][1] = '$')) then
      AddItems(ANode.FHash);
    if ALevel = vCount then
    begin
      AddItems(ANode);
      Exit;
    end;
    if (ANode.FPlus <> nil) and not ((ALevel = 0) and (vLevels[0] <> '') and
       (vLevels[0][1] = '$')) then
      Walk(ANode.FPlus, ALevel + 1);
    if ANode.FChildren <> nil then
    begin
      vIndex := ANode.FChildren.IndexOf(string(vLevels[ALevel]));
      if vIndex >= 0 then
        Walk(TRALMQTTTopicNode(ANode.FChildren.Objects[vIndex]), ALevel + 1);
    end;
  end;

begin
  if ATopic = '' then
    Exit;
  SplitLevels(ATopic, vLevels, vCount);
  Walk(FRoot, 0);
end;

procedure TRALMQTTTopicTree.Prune(ANode: TRALMQTTTopicNode);
var
  vParent: TRALMQTTTopicNode;
  vIndex: IntegerRAL;
begin
  // empty branches go, so a broker with changing topics does not grow forever
  while (ANode <> nil) and (ANode <> FRoot) and ANode.IsEmpty do
  begin
    vParent := ANode.FParent;
    if vParent.FPlus = ANode then
      vParent.FPlus := nil
    else if vParent.FHash = ANode then
      vParent.FHash := nil
    else if vParent.FChildren <> nil then
    begin
      vIndex := vParent.FChildren.IndexOf(string(ANode.FName));
      if vIndex >= 0 then
        vParent.FChildren.Delete(vIndex);
    end;
    ANode.Free;
    ANode := vParent;
  end;
end;

function TRALMQTTTopicTree.Remove(const AFilter: StringRAL; AItem: Pointer): boolean;
var
  vNode: TRALMQTTTopicNode;
  vIndex: IntegerRAL;
begin
  Result := False;
  vNode := FindNode(AFilter, False);
  if vNode = nil then
    Exit;
  vIndex := vNode.FItems.IndexOf(AItem);
  if vIndex < 0 then
    Exit;
  vNode.FItems.Delete(vIndex);
  Dec(FCount);
  Prune(vNode);
  Result := True;
end;

end.
