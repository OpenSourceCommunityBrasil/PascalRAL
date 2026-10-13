{ Checks the agent material of a RAL checkout: the skills' frontmatter and names, the
  skill table of AGENTS.md, the paths the agent files cite, and what they must not
  carry (local material, machine paths, Python tools, the test orchestrator,
  Portuguese text). }
program checkskills;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$APPTYPE CONSOLE}

uses
  Classes, SysUtils;

const
  /// Longest description the Agent Skills format accepts.
  cMaxDescription = 1024;
  /// Longest skill name the Agent Skills format accepts.
  cMaxName = 64;
  /// Prefix every skill of this repository carries.
  cPrefix = 'ral-';

var
  /// Number of problems found so far.
  vProblems: Integer;
  /// Root folder of the RAL checkout.
  vRepo: string;

/// Reports one problem at AFile:ALine.
procedure Problem(const AFile: string; ALine: Integer; const AText: string);
begin
  Inc(vProblems);
  if ALine > 0 then
    WriteLn(AFile, ':', ALine, ': ', AText)
  else
    WriteLn(AFile, ': ', AText);
end;

/// Answers AFile relative to the repository root, with forward slashes.
function RelPath(const AFile: string): string;
begin
  Result := StringReplace(ExtractRelativePath(IncludeTrailingPathDelimiter(vRepo), AFile),
    '\', '/', [rfReplaceAll]);
end;

/// Tells whether every character of AName is a lowercase letter, a digit or a hyphen.
function IsSkillName(const AName: string): Boolean;
var
  vIndex: Integer;
begin
  Result := AName <> '';
  for vIndex := 1 to Length(AName) do
    if not CharInSet(AName[vIndex], ['a'..'z', '0'..'9', '-']) then
      Exit(False);
end;

{ Reads the YAML frontmatter of ALines: the value of AKey, joining the indented lines of
  a folded (>) or literal (|) block. Answers False when the key is absent. }
function FrontValue(ALines: TStrings; const AKey: string; out AValue: string): Boolean;
var
  vIndex, vEnd: Integer;
  vLine: string;
begin
  Result := False;
  AValue := '';
  vEnd := -1;
  for vIndex := 1 to ALines.Count - 1 do
    if Trim(ALines[vIndex]) = '---' then
    begin
      vEnd := vIndex;
      Break;
    end;
  vIndex := 1;
  while vIndex < vEnd do
  begin
    vLine := ALines[vIndex];
    if Pos(AKey + ':', vLine) = 1 then
    begin
      Result := True;
      AValue := Trim(Copy(vLine, Length(AKey) + 2, MaxInt));
      if (AValue = '>') or (AValue = '|') or (AValue = '>-') or (AValue = '|-') then
      begin
        AValue := '';
        while (vIndex + 1 < vEnd) and (Copy(ALines[vIndex + 1], 1, 1) = ' ') do
        begin
          Inc(vIndex);
          if AValue <> '' then
            AValue := AValue + ' ';
          AValue := AValue + Trim(ALines[vIndex]);
        end;
      end;
      Exit;
    end;
    Inc(vIndex);
  end;
end;

/// Checks one skill folder; adds its name to ANames.
procedure CheckSkill(const ADir: string; ANames: TStrings);
var
  vDescription, vFile, vFolder, vName: string;
  vLines: TStringList;
begin
  vFolder := ExtractFileName(ExcludeTrailingPathDelimiter(ADir));
  ANames.Add(vFolder);
  vFile := IncludeTrailingPathDelimiter(ADir) + 'SKILL.md';
  if not FileExists(vFile) then
  begin
    Problem(RelPath(ADir), 0, 'no SKILL.md');
    Exit;
  end;
  vLines := TStringList.Create;
  try
    vLines.LoadFromFile(vFile);
    if (vLines.Count = 0) or (Trim(vLines[0]) <> '---') then
    begin
      Problem(RelPath(vFile), 1, 'does not start with a --- frontmatter');
      Exit;
    end;
    if not FrontValue(vLines, 'name', vName) then
      Problem(RelPath(vFile), 0, 'frontmatter has no name')
    else
    begin
      if vName <> vFolder then
        Problem(RelPath(vFile), 0, Format('name "%s" differs from the folder "%s"',
          [vName, vFolder]));
      if not IsSkillName(vName) or (Length(vName) > cMaxName) then
        Problem(RelPath(vFile), 0, 'name must be lowercase letters, digits and hyphens, ' +
          IntToStr(cMaxName) + ' at most');
      if Pos(cPrefix, vName) <> 1 then
        Problem(RelPath(vFile), 0, 'name must start with ' + cPrefix);
    end;
    if not FrontValue(vLines, 'description', vDescription) or (vDescription = '') then
      Problem(RelPath(vFile), 0, 'frontmatter has no description')
    else if Length(vDescription) > cMaxDescription then
      Problem(RelPath(vFile), 0, Format('description has %d characters (max %d)',
        [Length(vDescription), cMaxDescription]));
  finally
    vLines.Free;
  end;
end;

/// Collects the skill names (`ral-...`) of the Skills section of AGENTS.md.
procedure TableNames(ANames: TStrings);
var
  vLines: TStringList;
  vIndex, vPos, vEnd: Integer;
  vInSection: Boolean;
  vLine, vToken: string;
begin
  vLines := TStringList.Create;
  try
    vLines.LoadFromFile(IncludeTrailingPathDelimiter(vRepo) + 'AGENTS.md');
    vInSection := False;
    for vIndex := 0 to vLines.Count - 1 do
    begin
      vLine := vLines[vIndex];
      if Pos('## ', vLine) = 1 then
        vInSection := Trim(vLine) = '## Skills';
      if not vInSection or (Pos('|', TrimLeft(vLine)) <> 1) then
        Continue;
      vPos := Pos('`' + cPrefix, vLine);
      if vPos = 0 then
        Continue;
      vToken := Copy(vLine, vPos + 1, MaxInt);
      vEnd := Pos('`', vToken);
      if vEnd > 0 then
        ANames.Add(Copy(vToken, 1, vEnd - 1));
    end;
  finally
    vLines.Free;
  end;
end;

/// Tells whether ALine holds AText, ignoring case.
function HasText(const ALine, AText: string): Boolean;
begin
  Result := Pos(LowerCase(AText), LowerCase(ALine)) > 0;
end;

{ Tells whether ALine cites a folder of a developer install by absolute path (a drive
  letter followed by an IDE, Program Files, Users or projects folder). }
function HasMachinePath(const ALine: string): Boolean;
const
  cRoots: array[0..5] of string = ('ide\', 'program files', 'users\', 'projetos\',
    'lazarus\', 'embarcadero\');
var
  vIndex, vRoot: Integer;
  vRest: string;
begin
  Result := False;
  for vIndex := 1 to Length(ALine) - 2 do
    if CharInSet(ALine[vIndex], ['A'..'Z', 'a'..'z']) and (ALine[vIndex + 1] = ':') and
      (ALine[vIndex + 2] = '\') and
      ((vIndex = 1) or not CharInSet(ALine[vIndex - 1], ['A'..'Z', 'a'..'z'])) then
    begin
      vRest := LowerCase(Copy(ALine, vIndex + 3, 20));
      for vRoot := Low(cRoots) to High(cRoots) do
        if Pos(cRoots[vRoot], vRest) = 1 then
          Exit(True);
    end;
end;

{ Tells whether ALine cites local material other than the compiled tools and
  ENVIRONMENT.md. }
function HasLocalMaterial(const ALine: string): Boolean;
var
  vLine, vRest: string;
  vPos: Integer;
begin
  Result := False;
  vLine := StringReplace(LowerCase(ALine), '\', '/', [rfReplaceAll]);
  vPos := Pos('.agents/local/', vLine);
  while vPos > 0 do
  begin
    vRest := Copy(vLine, vPos + Length('.agents/local/'), MaxInt);
    if (Pos('bin', vRest) <> 1) and (Pos('environment.md', vRest) <> 1) then
      Exit(True);
    vLine := Copy(vLine, vPos + 1, MaxInt);
    vPos := Pos('.agents/local/', vLine);
  end;
end;

{ Tells whether ALine carries a letter with an accent (Portuguese text, in UTF-8) outside
  inline code, where a mangled-text example may quote one. }
function HasAccent(const ALine: string): Boolean;
var
  vIndex: Integer;
  vInCode: Boolean;
begin
  Result := False;
  vInCode := False;
  for vIndex := 1 to Length(ALine) do
    if ALine[vIndex] = '`' then
      vInCode := not vInCode
    // UTF-8 of the accented Latin-1 letters starts with $C3
    else if not vInCode and (Ord(ALine[vIndex]) = $C3) then
      Exit(True);
end;

/// Checks every path written between backticks that starts with src, pkg or .agents.
procedure CheckPaths(const AFile, ALine: string; ALineNo: Integer);
var
  vRest, vToken, vPath: string;
  vStart, vEnd: Integer;
begin
  vRest := ALine;
  repeat
    vStart := Pos('`', vRest);
    if vStart = 0 then
      Exit;
    vRest := Copy(vRest, vStart + 1, MaxInt);
    vEnd := Pos('`', vRest);
    if vEnd = 0 then
      Exit;
    vToken := Copy(vRest, 1, vEnd - 1);
    vRest := Copy(vRest, vEnd + 1, MaxInt);
    vPath := StringReplace(vToken, '\', '/', [rfReplaceAll]);
    if not ((Pos('src/', vPath) = 1) or (Pos('pkg/', vPath) = 1) or
      ((Pos('.agents/', vPath) = 1) and (Pos('.agents/local', vPath) <> 1))) then
      Continue;
    if (Pos('<', vPath) > 0) or (Pos('*', vPath) > 0) or (Pos('...', vPath) > 0) or
      (Pos(' ', vPath) > 0) or (Pos(';', vPath) > 0) then
      Continue;
    while (vPath <> '') and CharInSet(vPath[Length(vPath)], ['/', '.', ',', ':', ';']) do
      Delete(vPath, Length(vPath), 1);
    vPath := StringReplace(vPath, '/', PathDelim, [rfReplaceAll]);
    if not FileExists(IncludeTrailingPathDelimiter(vRepo) + vPath) and
      not DirectoryExists(IncludeTrailingPathDelimiter(vRepo) + vPath) then
      Problem(AFile, ALineNo, 'cites a path that does not exist: ' + vToken);
  until False;
end;

/// Scans one committed agent file line by line.
procedure CheckFile(const AFile: string);
var
  vLines: TStringList;
  vIndex: Integer;
  vDefinesLocal, vIsTemplate: Boolean;
  vLine, vRel: string;
begin
  vRel := RelPath(AFile);
  // these two define the local folder and the machine paths file
  vDefinesLocal := (vRel = 'AGENTS.md') or (vRel = '.agents/README.md');
  vIsTemplate := vRel = '.agents/ENVIRONMENT.example.md';
  vLines := TStringList.Create;
  try
    vLines.LoadFromFile(AFile);
    for vIndex := 0 to vLines.Count - 1 do
    begin
      vLine := vLines[vIndex];
      CheckPaths(vRel, vLine, vIndex + 1);
      if not vDefinesLocal and HasLocalMaterial(vLine) then
        Problem(vRel, vIndex + 1, 'cites local material (.agents/local)');
      if not vIsTemplate and HasMachinePath(vLine) then
        Problem(vRel, vIndex + 1, 'cites a path of a developer machine');
      if HasText(vLine, '.py') or HasText(vLine, 'python') then
        Problem(vRel, vIndex + 1, 'cites a Python tool');
      if HasText(vLine, 'RALOrquestrador') or HasText(vLine, 'orqcli') then
        Problem(vRel, vIndex + 1, 'cites the test orchestrator, which is not in this repository');
      if HasAccent(vLine) then
        Problem(vRel, vIndex + 1, 'has accented letters: agent files are English');
    end;
  finally
    vLines.Free;
  end;
end;

/// Adds to AList every file below ADir whose name matches AMask.
procedure ListFiles(const ADir, AMask: string; AList: TStrings; ARecursive: Boolean);
var
  vRec: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(ADir) + AMask, faAnyFile, vRec) = 0 then
  try
    repeat
      if vRec.Attr and faDirectory = 0 then
        AList.Add(IncludeTrailingPathDelimiter(ADir) + vRec.Name);
    until FindNext(vRec) <> 0;
  finally
    FindClose(vRec);
  end;
  if ARecursive and (FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faDirectory,
    vRec) = 0) then
  try
    repeat
      if (vRec.Attr and faDirectory <> 0) and (vRec.Name <> '.') and (vRec.Name <> '..') then
        ListFiles(IncludeTrailingPathDelimiter(ADir) + vRec.Name, AMask, AList, True);
    until FindNext(vRec) <> 0;
  finally
    FindClose(vRec);
  end;
end;

var
  vAgents, vSkillsDir: string;
  vFiles, vFolders, vTable: TStringList;
  vIndex: Integer;
  vRec: TSearchRec;

begin
  vFiles := TStringList.Create;
  vFolders := TStringList.Create;
  vTable := TStringList.Create;
  try
    try
      vRepo := GetCurrentDir;
      for vIndex := 1 to ParamCount do
        if Pos('--repo=', ParamStr(vIndex)) = 1 then
          vRepo := ExpandFileName(Copy(ParamStr(vIndex), 8, MaxInt))
        else
        begin
          WriteLn('usage: checkskills [--repo=<RAL root>]');
          ExitCode := 2;
          Exit;
        end;
      vRepo := ExcludeTrailingPathDelimiter(vRepo);
      vAgents := IncludeTrailingPathDelimiter(vRepo) + '.agents';
      vSkillsDir := IncludeTrailingPathDelimiter(vAgents) + 'skills';
      if not FileExists(IncludeTrailingPathDelimiter(vRepo) + 'AGENTS.md') then
        raise Exception.Create(vRepo + ' has no AGENTS.md; use --repo=');

      // skills
      if FindFirst(IncludeTrailingPathDelimiter(vSkillsDir) + '*', faDirectory, vRec) = 0 then
      try
        repeat
          if (vRec.Attr and faDirectory <> 0) and (vRec.Name <> '.') and
            (vRec.Name <> '..') then
            CheckSkill(IncludeTrailingPathDelimiter(vSkillsDir) + vRec.Name, vFolders);
        until FindNext(vRec) <> 0;
      finally
        FindClose(vRec);
      end;

      // the AGENTS.md table against the folders
      TableNames(vTable);
      vTable.Sorted := True;
      vFolders.Sorted := True;
      for vIndex := 0 to vFolders.Count - 1 do
        if vTable.IndexOf(vFolders[vIndex]) < 0 then
          Problem('AGENTS.md', 0, 'skill ' + vFolders[vIndex] + ' is missing from the Skills table');
      for vIndex := 0 to vTable.Count - 1 do
        if vFolders.IndexOf(vTable[vIndex]) < 0 then
          Problem('AGENTS.md', 0, 'the Skills table lists ' + vTable[vIndex] +
            ', which has no folder in .agents/skills');

      // every committed agent file
      vFiles.Add(IncludeTrailingPathDelimiter(vRepo) + 'AGENTS.md');
      ListFiles(vAgents, '*.md', vFiles, False);
      ListFiles(vSkillsDir, '*.md', vFiles, True);
      for vIndex := 0 to vFiles.Count - 1 do
        CheckFile(vFiles[vIndex]);

      if vProblems = 0 then
        WriteLn(Format('OK: %d skills, %d files checked', [vFolders.Count, vFiles.Count]))
      else
      begin
        WriteLn(Format('%d problem(s)', [vProblems]));
        ExitCode := 1;
      end;
    except
      on E: Exception do
      begin
        WriteLn('checkskills: ', E.Message);
        ExitCode := 2;
      end;
    end;
  finally
    vTable.Free;
    vFolders.Free;
    vFiles.Free;
  end;
end.
