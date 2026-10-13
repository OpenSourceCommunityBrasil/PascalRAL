{ Lists the units a Delphi package (.bpl) contains and flags those that do not belong
  to it: a unit of another RAL package, or a third-party unit linked in statically. }
program bplunits;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$APPTYPE CONSOLE}

uses
  Classes, SysUtils;

const
  /// Resource type of RCDATA in a PE resource directory.
  cRTRCData = 10;
  /// Name of the resource the Delphi compiler writes into every package.
  cPackageInfo = 'PACKAGEINFO';

type
  /// What a package's PACKAGEINFO resource lists.
  TPackageInfo = record
    /// Units compiled into the package.
    Contains: TStringList;
    /// Packages the package requires.
    Requires: TStringList;
  end;

var
  /// Unit name prefixes accepted as linked in on purpose (--allow).
  vAllow: TStringList;
  /// Every unit under src, name=relative path, sorted for lookup.
  vSourceUnits: TStringList;
  /// Root folder of the RAL checkout (--repo, default the current folder).
  vRepo: string;

/// Reads a whole file into memory.
function ReadBytes(const AFileName: string): TBytes;
var
  vStream: TFileStream;
begin
  Result := nil;
  vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, vStream.Size);
    if Length(Result) > 0 then
      vStream.ReadBuffer(Result[0], Length(Result));
  finally
    vStream.Free;
  end;
end;

/// Reads a little-endian 16-bit value at AOffset.
function ReadWord(const AData: TBytes; AOffset: Integer): Word;
begin
  if (AOffset < 0) or (AOffset + 2 > Length(AData)) then
    raise Exception.Create('truncated file');
  Result := AData[AOffset] or (AData[AOffset + 1] shl 8);
end;

/// Reads a little-endian 32-bit value at AOffset.
function ReadDWord(const AData: TBytes; AOffset: Integer): Cardinal;
begin
  if (AOffset < 0) or (AOffset + 4 > Length(AData)) then
    raise Exception.Create('truncated file');
  Result := Cardinal(AData[AOffset]) or (Cardinal(AData[AOffset + 1]) shl 8) or
    (Cardinal(AData[AOffset + 2]) shl 16) or (Cardinal(AData[AOffset + 3]) shl 24);
end;

{ Returns the bytes of the RCDATA resource named PACKAGEINFO of a PE file, or an empty
  array when the file has none. }
function FindPackageInfo(const AData: TBytes): TBytes;
var
  vBase, vChar, vCount, vEntry, vIndex, vNameLen, vOffset, vOptional, vPE,
    vSections: Integer;
  vDataRVA, vDataSize, vName, vRVA: Cardinal;
  vResName: string;
  vResFound, vTypeFound: Boolean;

  function RVAToOffset(ARVA: Cardinal): Integer;
  var
    vRawAddr, vRawSize, vVirtAddr, vVirtSize: Cardinal;
    vSec, vSecHeader: Integer;
  begin
    Result := -1;
    for vSec := 0 to vSections - 1 do
    begin
      vSecHeader := vOptional + ReadWord(AData, vPE + 20) + vSec * 40;
      vVirtSize := ReadDWord(AData, vSecHeader + 8);
      vVirtAddr := ReadDWord(AData, vSecHeader + 12);
      vRawSize := ReadDWord(AData, vSecHeader + 16);
      vRawAddr := ReadDWord(AData, vSecHeader + 20);
      if vRawSize > vVirtSize then
        vVirtSize := vRawSize;
      if (ARVA >= vVirtAddr) and (ARVA < vVirtAddr + vVirtSize) then
        Exit(Integer(ARVA - vVirtAddr + vRawAddr));
    end;
  end;

begin
  Result := nil;
  if (Length(AData) < $40) or (AData[0] <> Ord('M')) or (AData[1] <> Ord('Z')) then
    raise Exception.Create('not a PE file');
  vPE := ReadDWord(AData, $3C);
  vSections := ReadWord(AData, vPE + 6);
  vOptional := vPE + 24;
  // the data directories start at 96 (PE32) or 112 (PE32+); resources are the third
  if ReadWord(AData, vOptional) = $10B then
    vOffset := vOptional + 96 + 2 * 8
  else
    vOffset := vOptional + 112 + 2 * 8;
  vRVA := ReadDWord(AData, vOffset);
  vBase := RVAToOffset(vRVA);
  if vBase < 0 then
    Exit;

  // level 1: type; level 2: name; level 3: language (the first one is taken)
  vEntry := vBase;
  vTypeFound := False;
  vCount := ReadWord(AData, vEntry + 12) + ReadWord(AData, vEntry + 14);
  for vIndex := 0 to vCount - 1 do
    if ReadDWord(AData, vEntry + 16 + vIndex * 8) = cRTRCData then
    begin
      vEntry := vBase + Integer(ReadDWord(AData, vEntry + 20 + vIndex * 8) and $7FFFFFFF);
      vTypeFound := True;
      Break;
    end;
  if not vTypeFound then
    Exit;

  vResFound := False;
  vCount := ReadWord(AData, vEntry + 12) + ReadWord(AData, vEntry + 14);
  for vIndex := 0 to vCount - 1 do
  begin
    vName := ReadDWord(AData, vEntry + 16 + vIndex * 8);
    if vName and $80000000 = 0 then
      Continue;
    vOffset := vBase + Integer(vName and $7FFFFFFF);
    vNameLen := ReadWord(AData, vOffset);
    SetLength(vResName, vNameLen);
    for vChar := 1 to vNameLen do
      vResName[vChar] := Char(ReadWord(AData, vOffset + vChar * 2));
    if SameText(vResName, cPackageInfo) then
    begin
      vEntry := vBase + Integer(ReadDWord(AData, vEntry + 20 + vIndex * 8) and $7FFFFFFF);
      vResFound := True;
      Break;
    end;
  end;
  if not vResFound then
    Exit;

  // language directory: its first entry points at the data entry
  vOffset := vBase + Integer(ReadDWord(AData, vEntry + 20) and $7FFFFFFF);
  vDataRVA := ReadDWord(AData, vOffset);
  vDataSize := ReadDWord(AData, vOffset + 4);
  vOffset := RVAToOffset(vDataRVA);
  if (vOffset < 0) or (vOffset + Integer(vDataSize) > Length(AData)) then
    raise Exception.Create('PACKAGEINFO outside the file');
  SetLength(Result, vDataSize);
  Move(AData[vOffset], Result[0], vDataSize);
end;

/// Reads a zero-terminated ANSI name at AOffset and moves AOffset past its terminator.
function ReadName(const AData: TBytes; var AOffset: Integer): string;
var
  vStart: Integer;
  vAnsi: AnsiString;
begin
  vStart := AOffset;
  while (AOffset < Length(AData)) and (AData[AOffset] <> 0) do
    Inc(AOffset);
  if AOffset >= Length(AData) then
    raise Exception.Create('PACKAGEINFO truncated');
  SetLength(vAnsi, AOffset - vStart);
  if AOffset > vStart then
    Move(AData[vStart], vAnsi[1], AOffset - vStart);
  Result := string(vAnsi);
  Inc(AOffset);
end;

{ Parses a PACKAGEINFO resource: flags (4 bytes), the required packages (a count, then
  a hash byte and a name each) and the contained units (a count, then flags, hash and
  name each). }
function ParsePackageInfo(const AData: TBytes): TPackageInfo;
var
  vCount, vIndex, vOffset: Integer;
begin
  Result.Contains := TStringList.Create;
  Result.Requires := TStringList.Create;
  vOffset := 4;
  vCount := Integer(ReadDWord(AData, vOffset));
  Inc(vOffset, 4);
  for vIndex := 1 to vCount do
  begin
    Inc(vOffset);
    Result.Requires.Add(ReadName(AData, vOffset));
  end;
  vCount := Integer(ReadDWord(AData, vOffset));
  Inc(vOffset, 4);
  for vIndex := 1 to vCount do
  begin
    Inc(vOffset, 2);
    Result.Contains.Add(ReadName(AData, vOffset));
  end;
end;

/// Adds every .pas below ADir to vSourceUnits as name=path relative to vRepo.
procedure IndexSources(const ADir: string);
var
  vRec: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faAnyFile, vRec) = 0 then
  try
    repeat
      if (vRec.Name = '.') or (vRec.Name = '..') then
        Continue;
      if vRec.Attr and faDirectory <> 0 then
        IndexSources(IncludeTrailingPathDelimiter(ADir) + vRec.Name)
      else if SameText(ExtractFileExt(vRec.Name), '.pas') then
        vSourceUnits.Add(ChangeFileExt(vRec.Name, '') + '=' +
          ExtractRelativePath(IncludeTrailingPathDelimiter(vRepo),
          IncludeTrailingPathDelimiter(ADir) + vRec.Name));
    until FindNext(vRec) <> 0;
  finally
    FindClose(vRec);
  end;
end;

/// Finds the file AName below ADir, or answers ''.
function FindFileBelow(const ADir, AName: string): string;
var
  vRec: TSearchRec;
begin
  Result := '';
  if FileExists(IncludeTrailingPathDelimiter(ADir) + AName) then
    Exit(IncludeTrailingPathDelimiter(ADir) + AName);
  if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*', faDirectory, vRec) = 0 then
  try
    repeat
      if (vRec.Attr and faDirectory <> 0) and (vRec.Name <> '.') and (vRec.Name <> '..') then
      begin
        Result := FindFileBelow(IncludeTrailingPathDelimiter(ADir) + vRec.Name, AName);
        if Result <> '' then
          Exit;
      end;
    until FindNext(vRec) <> 0;
  finally
    FindClose(vRec);
  end;
end;

/// Reads the unit names of the contains clause of a .dpk, lowercased.
function DpkContains(const ADpk: string): TStringList;
var
  vClause, vItem: string;
  vLines: TStringList;
  vIndex, vPos: Integer;
begin
  Result := TStringList.Create;
  vLines := TStringList.Create;
  try
    vLines.LoadFromFile(ADpk);
    vClause := '';
    vPos := Pos('contains', LowerCase(vLines.Text));
    if vPos = 0 then
      Exit;
    vClause := Copy(vLines.Text, vPos + Length('contains'), MaxInt);
    vPos := Pos(';', vClause);
    if vPos > 0 then
      vClause := Copy(vClause, 1, vPos - 1);
    vLines.Text := StringReplace(vClause, ',', sLineBreak, [rfReplaceAll]);
    for vIndex := 0 to vLines.Count - 1 do
    begin
      vItem := Trim(vLines[vIndex]);
      vPos := Pos(' ', vItem);
      if vPos > 0 then
        vItem := Copy(vItem, 1, vPos - 1);
      if vItem <> '' then
        Result.Add(LowerCase(vItem));
    end;
  finally
    vLines.Free;
  end;
end;

/// Tells whether AUnit starts with one of the --allow prefixes.
function IsAllowed(const AUnit: string): Boolean;
var
  vIndex: Integer;
begin
  Result := False;
  for vIndex := 0 to vAllow.Count - 1 do
    if Pos(LowerCase(vAllow[vIndex]), LowerCase(AUnit)) = 1 then
      Exit(True);
end;

/// Checks one package; answers the number of problems found.
function CheckPackage(const ABpl: string): Integer;
var
  vDpk, vName, vPath, vUnit: string;
  vInfo: TPackageInfo;
  vData, vFile: TBytes;
  vOwn: TStringList;
  vIndex, vOwnCount: Integer;
begin
  Result := 0;
  vName := ChangeFileExt(ExtractFileName(ABpl), '');
  vFile := ReadBytes(ABpl);
  vData := FindPackageInfo(vFile);
  if Length(vData) = 0 then
  begin
    WriteLn(vName, ': no PACKAGEINFO resource (not a Delphi package?)');
    Exit(1);
  end;
  vInfo := ParsePackageInfo(vData);
  vDpk := FindFileBelow(IncludeTrailingPathDelimiter(vRepo) + 'pkg' + PathDelim + 'Delphi',
    vName + '.dpk');
  if vDpk <> '' then
    vOwn := DpkContains(vDpk)
  else
    vOwn := TStringList.Create;
  try
    WriteLn(Format('%s  %d KB  requires: %s', [vName, (Length(vFile) + 1023) div 1024,
      StringReplace(vInfo.Requires.CommaText, ',', ', ', [rfReplaceAll])]));
    if vDpk = '' then
      WriteLn('  no ', vName, '.dpk under pkg\Delphi: every RAL unit is reported');
    vOwnCount := 0;
    for vIndex := 0 to vInfo.Contains.Count - 1 do
    begin
      vUnit := vInfo.Contains[vIndex];
      if SameText(vUnit, vName) or SameText(vUnit, 'SysInit') or
        (vOwn.IndexOf(LowerCase(vUnit)) >= 0) then
      begin
        Inc(vOwnCount);
        Continue;
      end;
      vPath := vSourceUnits.Values[vUnit];
      if (vPath <> '') and (Pos('src' + PathDelim + 'others' + PathDelim, vPath) = 1) then
        Inc(vOwnCount) // a submodule unit compiled in with the package's own units
      else if vPath <> '' then
      begin
        WriteLn('  RAL unit of another package: ', vUnit, ' (', vPath, ')');
        Inc(Result);
      end
      else if IsAllowed(vUnit) then
        Inc(vOwnCount)
      else
      begin
        WriteLn('  third-party unit linked in: ', vUnit);
        Inc(Result);
      end;
    end;
    if Result = 0 then
      WriteLn(Format('  OK: %d units, all its own', [vOwnCount]))
    else
      WriteLn(Format('  FAIL: %d unit(s) that are not its own', [Result]));
  finally
    vOwn.Free;
    vInfo.Contains.Free;
    vInfo.Requires.Free;
  end;
end;

/// Prints how to call the program.
procedure ShowUsage;
begin
  WriteLn('usage: bplunits [--repo=<RAL root>] [--allow=prefix,...] <file.bpl | folder> ...');
  WriteLn('  Lists the units each package contains and flags any unit of another RAL');
  WriteLn('  package or third-party unit linked in. Exit code 1 when one is found.');
  WriteLn('  --allow  unit name prefixes linked in on purpose (SynopseRAL: --allow=mormot.)');
end;

var
  vArg, vFolder: string;
  vFiles: TStringList;
  vIndex, vProblems: Integer;
  vRec: TSearchRec;

begin
  vAllow := TStringList.Create;
  vSourceUnits := TStringList.Create;
  vFiles := TStringList.Create;
  try
    try
      vRepo := GetCurrentDir;
      for vIndex := 1 to ParamCount do
      begin
        vArg := ParamStr(vIndex);
        if Pos('--repo=', vArg) = 1 then
          vRepo := ExpandFileName(Copy(vArg, 8, MaxInt))
        else if Pos('--allow=', vArg) = 1 then
          vAllow.CommaText := Copy(vArg, 9, MaxInt)
        else if DirectoryExists(vArg) then
        begin
          vFolder := IncludeTrailingPathDelimiter(vArg);
          if FindFirst(vFolder + '*.bpl', faAnyFile, vRec) = 0 then
          try
            repeat
              vFiles.Add(vFolder + vRec.Name);
            until FindNext(vRec) <> 0;
          finally
            FindClose(vRec);
          end;
        end
        else
          vFiles.Add(vArg);
      end;
      if vFiles.Count = 0 then
      begin
        ShowUsage;
        ExitCode := 2;
        Exit;
      end;
      if not DirectoryExists(IncludeTrailingPathDelimiter(vRepo) + 'src') then
        raise Exception.Create(vRepo + ' is not a RAL checkout (no src folder); use --repo=');
      IndexSources(IncludeTrailingPathDelimiter(vRepo) + 'src');
      vSourceUnits.Sorted := True;
      vProblems := 0;
      for vIndex := 0 to vFiles.Count - 1 do
        Inc(vProblems, CheckPackage(vFiles[vIndex]));
      if vProblems > 0 then
        ExitCode := 1;
    except
      on E: Exception do
      begin
        WriteLn('bplunits: ', E.Message);
        ExitCode := 2;
      end;
    end;
  finally
    vFiles.Free;
    vSourceUnits.Free;
    vAllow.Free;
  end;
end.
