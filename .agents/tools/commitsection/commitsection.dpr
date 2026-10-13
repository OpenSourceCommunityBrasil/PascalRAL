{ Tells in which section of CHANGELOG.md a commit message lands, with the same rule as
  .github/workflows/changelog.yml: the first keyword found in subject and body. }
program commitsection;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$APPTYPE CONSOLE}

uses
  Classes, SysUtils;

type
  /// One section of the changelog and the keywords that send a commit to it.
  TSection = record
    /// Keywords, tested as plain substrings of the lowercased message.
    Keywords: array of string;
    /// Section title in CHANGELOG.md, or what happens to the commit.
    Title: string;
  end;

var
  /// The sections in the order the workflow tests them.
  vSections: array of TSection;

/// Appends a section to vSections.
procedure AddSection(const ATitle: string; const AKeywords: array of string);
var
  vIndex: Integer;
begin
  SetLength(vSections, Length(vSections) + 1);
  vSections[High(vSections)].Title := ATitle;
  SetLength(vSections[High(vSections)].Keywords, Length(AKeywords));
  for vIndex := 0 to High(AKeywords) do
    vSections[High(vSections)].Keywords[vIndex] := AKeywords[vIndex];
end;

/// Fills vSections exactly as categorize_commit in changelog.yml.
procedure LoadSections;
begin
  AddSection('Security', ['security', 'vulnerability', 'cve', 'exploit']);
  AddSection('Breaking Changes', ['breaking change', 'breaking:', 'break:']);
  AddSection('Deprecated', ['deprecat', 'obsolete', 'phase out']);
  AddSection('Removed', ['remove', 'delete', 'drop', 'eliminate']);
  AddSection('Added', ['add', 'new', 'create', 'implement', 'feat', 'feature']);
  AddSection('Fixed', ['fix', 'resolve', 'correct', 'patch', 'bug', 'issue']);
  AddSection('internal: left out of CHANGELOG.md', ['chore', 'chr']);
end;

/// Answers the whole word of AText that holds the position APos.
function WordAt(const AText: string; APos: Integer): string;
var
  vFirst, vLast: Integer;
begin
  vFirst := APos;
  while (vFirst > 1) and CharInSet(AText[vFirst - 1], ['a'..'z', 'A'..'Z', '0'..'9', '_']) do
    Dec(vFirst);
  vLast := APos;
  while (vLast < Length(AText)) and
    CharInSet(AText[vLast + 1], ['a'..'z', 'A'..'Z', '0'..'9', '_']) do
    Inc(vLast);
  Result := Copy(AText, vFirst, vLast - vFirst + 1);
end;

{ Explains where AKeyword was found: in the subject or the body, and inside which word
  when it is only part of one ("address" holds "add"). }
procedure Explain(const AKeyword, ASubject, ABody: string);
var
  vPos: Integer;
  vText, vWhere, vWord: string;
begin
  vPos := Pos(AKeyword, LowerCase(ASubject));
  if vPos > 0 then
  begin
    vText := ASubject;
    vWhere := 'subject';
  end
  else
  begin
    vPos := Pos(AKeyword, LowerCase(ABody));
    vText := ABody;
    vWhere := 'body';
  end;
  vWord := WordAt(vText, vPos);
  Write('  keyword "', AKeyword, '" in the ', vWhere);
  if not SameText(Trim(vWord), Trim(AKeyword)) and (Pos(' ', AKeyword) = 0) then
    Write(', inside the word "', vWord, '"');
  WriteLn;
  if vWhere = 'body' then
    WriteLn('  note: the body decides it; a keyword of an earlier section anywhere in',
      ' the message wins');
end;

/// Reads a whole text file, or standard input for '-'.
function ReadMessage(const AFileName: string): string;
var
  vLine: string;
  vLines: TStringList;
begin
  vLines := TStringList.Create;
  try
    if AFileName = '-' then
      while not Eof(Input) do
      begin
        ReadLn(vLine);
        vLines.Add(vLine);
      end
    else
      vLines.LoadFromFile(AFileName);
    Result := vLines.Text;
  finally
    vLines.Free;
  end;
end;

/// Prints how to call the program.
procedure ShowUsage;
begin
  WriteLn('usage: commitsection "<subject>" ["<body>"]');
  WriteLn('       commitsection --file=<message file | ->');
  WriteLn('  Prints the CHANGELOG.md section the commit lands in (changelog.yml rule).');
  WriteLn('  With --file, the first line is the subject and the rest the body;');
  WriteLn('  "git log -1 --format=%B > msg.txt" writes one.');
end;

var
  vBody, vKeyword, vLower, vMessage, vSubject: string;
  vKey, vLineEnd, vSection: Integer;
  vFound: Boolean;

begin
  try
    if ParamCount = 0 then
    begin
      ShowUsage;
      ExitCode := 2;
      Exit;
    end;
    if Pos('--file=', ParamStr(1)) = 1 then
    begin
      vMessage := AdjustLineBreaks(ReadMessage(Copy(ParamStr(1), 8, MaxInt)), tlbsLF);
      vLineEnd := Pos(#10, vMessage);
      if vLineEnd = 0 then
        vLineEnd := Length(vMessage) + 1;
      vSubject := Trim(Copy(vMessage, 1, vLineEnd - 1));
      vBody := Trim(Copy(vMessage, vLineEnd + 1, MaxInt));
    end
    else
    begin
      vSubject := Trim(ParamStr(1));
      if ParamCount > 1 then
        vBody := Trim(ParamStr(2))
      else
        vBody := '';
    end;

    // the workflow leaves these out of git log before categorizing (git --grep)
    vMessage := vSubject + #10 + vBody;
    if (Pos('[skip ci]', vMessage) > 0) or (Pos('docs: update changelog', vMessage) > 0) or
      (Pos('changelog.yml', vMessage) > 0) then
    begin
      WriteLn('left out of CHANGELOG.md: the message holds "[skip ci]", ',
        '"docs: update changelog" or "changelog.yml"');
      Exit;
    end;

    LoadSections;
    vLower := LowerCase(vSubject + ' ' + vBody);
    vFound := False;
    for vSection := 0 to High(vSections) do
    begin
      for vKey := 0 to High(vSections[vSection].Keywords) do
      begin
        vKeyword := vSections[vSection].Keywords[vKey];
        if Pos(vKeyword, vLower) > 0 then
        begin
          WriteLn(vSections[vSection].Title);
          Explain(vKeyword, vSubject, vBody);
          vFound := True;
          Break;
        end;
      end;
      if vFound then
        Break;
    end;
    if not vFound then
      WriteLn('Changed', sLineBreak, '  no keyword found');
  except
    on E: Exception do
    begin
      WriteLn('commitsection: ', E.Message);
      ExitCode := 2;
    end;
  end;
end.
