unit RALTranslate;

interface

uses
  {$IFDEF FPC}
    Translations, LCLType,
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Windows,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, IniFiles,
  RALTypes, RALConsts;

implementation

{$IFDEF FPC}
  function ResourcesTranslate(AName, AValue : AnsiString; AHash : Longint; AArg: Pointer) : AnsiString;
  var
    vTrans: TCustomIniFile;
    vPos: IntegerRAL;
    vSession, vKey: StringRAL;
  begin
    vTrans := TCustomIniFile(AArg);

    Result := '';
    if vTrans = nil then
      Exit;

    vPos := Pos('.', AName);
    vSession := Copy(AName, 1, vPos - 1);
    vKey := Copy(AName, vPos + 1, Length(AName));

    Result := vTrans.ReadString(vSession, vKey, '');
  end;
{$ELSE}
  function GetIniValue(AIniFile : TCustomIniFile; ASession, AKey: StringRAL): PWideChar;
  var
    vStr : WideString;
    vLen : integer;
  begin
    vStr := AIniFile.ReadString(ASession, AKey, '');
    {$IFNDEF FPC}
      vStr := UTF8Decode(vStr);
    {$ENDIF}
    vLen := (Length(vStr) + 1) * SizeOf(WideChar);
    Result := GetMemory(vLen);
    Move(vStr[POSINISTR], Result^, vLen);
  end;

  procedure SetResourceString(ARes: PResStringRec; AValue: PChar);
  var
    PProtect: DWORD;
  begin
    {$IFDEF MSWINDOWS}
      VirtualProtect(ARes, SizeOf(ARes^), PAGE_EXECUTE_READWRITE, @PProtect);
      ARes^.Identifier := Integer(AValue);
      VirtualProtect(ARes, SizeOf(ARes^), PProtect, @PProtect);
    {$ENDIF}
  end;

  procedure TranslateRALConsts(vTrans : TCustomIniFile);
  const
    cPRes : array[0..13] of PResStringRec = (@emContentCheckError, @emCompressLibFilesError,
                    @emCompressInvalidFormat, @emCryptEmptyKey, @emDBLinkMissing,
                    @emInvalidJSONFormat, @emQueryVersionError, @emRouteAlreadyExists,
                    @emStorageNotFound, @emStorageInvalidBinary, @emSaguiServerCreateError,
                    @emSaguiServerUnsupportedTLS, @emSaguiLibraryLoadError,
                    @wmIPv6notImplemented);
    cRes : array[0..13] of StringRAL = ('emContentCheckError', 'emCompressLibFilesError',
                    'emCompressInvalidFormat', 'emCryptEmptyKey', 'emDBLinkMissing',
                    'emInvalidJSONFormat', 'emQueryVersionError', 'emRouteAlreadyExists',
                    'emStorageNotFound', 'emStorageInvalidBinary', 'emSaguiServerCreateError',
                    'emSaguiServerUnsupportedTLS', 'emSaguiLibraryLoadError',
                    'wmIPv6notImplemented');
    var
      vInt: IntegerRAL;
  begin
    for vInt := 0 to High(cRes) do
      SetResourceString(cPRes[vInt], GetIniValue(vTrans, 'RALConsts', cRes[vInt]));
  end;

  function RetrieveLocaleInfo(ALangID : Cardinal; AInfoType: LCTYPE; var AInfo: StringRAL): LongInt;
  var
    Buffer: string; // deve ser string
  begin
    AInfo := '';
    Result := GetLocaleInfo(ALangID, AInfoType, nil, 0);
    if Result <> 0 then
    begin
      Buffer := '';
      SetLength(Buffer, Result);
      Result := GetLocaleInfo(ALangID, AInfoType, @Buffer[POSINISTR], Result);
      if Result <> 0 then
        AInfo := Copy(Buffer, 1, Length(Buffer) - 1);
    end;
  end;
{$ENDIF}

function GetLanguage : StringRAL;
{$IFNDEF FPC}
  var
    vLangID : Cardinal;
    vLang, vCountry: StringRAL;
{$ENDIF}
begin
  {$IFDEF FPC}
    Result := GetLanguageID.LanguageID;
  {$ELSE}
    vLangID := GetSystemDefaultLCID;
    RetrieveLocaleInfo(vLangID, LOCALE_SISO639LANGNAME, vLang);
    RetrieveLocaleInfo(vLangID, LOCALE_SISO3166CTRYNAME, vCountry);
    Result := Format('%s_%s', [vLang, vCountry]);
  {$ENDIF}
end;

procedure RALResourcesTranslate(ALangID: StringRAL = '');
var
  vTrans: TCustomIniFile;
  vIniFile: StringRAL;
  vResource : TResourceStream;
  vHandle : NativeInt;
  vInt: IntegerRAL;
  vSession: TStringList;
begin
  if (ALangID = '') then
    ALangID := GetLanguage;

  vResource := nil;
  vTrans := nil;

  vIniFile := ExtractFilePath(ParamStr(0)) + 'rallang.' + ALangID;
  if FileExists(vIniFile) then
  begin
    vTrans := TIniFile.Create(vIniFile);
  end
  else
  begin
    vHandle := FindResource(HInstance, 'rallang', RT_RCDATA);
    if vHandle <> 0 then
    begin
      vResource := TResourceStream.Create(HInstance, 'rallang', RT_RCDATA);
      vResource.Position := 0;
      vTrans := TMemIniFile.Create(vResource);
    end;
  end;

  if vTrans = nil then
    Exit;

  try
    {$IFDEF FPC}
      vSession := TStringList.Create;
      try
        vTrans.ReadSections(vSession);
        for vInt := 0 to Pred(vSession.Count) do
          SetUnitResourceStrings(vSession.Strings[vInt], @ResourcesTranslate, vTrans);
      finally
        FreeAndNil(vSession);
      end;
    {$ELSE}
      TranslateRALConsts(vTrans);
    {$ENDIF}
  finally
    FreeAndNil(vTrans);
    if vResource <> nil then
      FreeAndNil(vResource);
  end;
end;

initialization
  RALResourcesTranslate;

end.

