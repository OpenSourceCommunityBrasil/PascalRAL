/// Class for mapping default MIMETypes according to IANA
// https://www.iana.org/assignments/media-types

unit RALMIMETypes;

{$I ..\base\PascalRAL.inc}
{$IFDEF FPC}
  {$mode Delphi}
{$ENDIF}

interface

uses
  {$IFDEF RALApple}
    Macapi.CoreFoundation, Macapi.Helpers, Macapi.ObjectiveC,
    Macapi.CoreServices,
  {$ENDIF}
  {$IFDEF RALAppleFPC}
    MacOSAll, CFBase, CFString,
  {$ENDIF}
  {$IFDEF RALWindows}
    Windows, Registry,
  {$ENDIF}
  {$IFDEF RALLinux}
   System.IOUtils,
  {$ENDIF}
  Classes, SysUtils,
  RALTypes;

const
  {$REGION 'Const definitions'}
  rctNONE = '';
  rctAPPLICATIONATOMXML = 'application/atom+xml';
  rctAPPLICATIONECMASCRIPT = 'application/ecmascript';
  rctAPPLICATIONEDIX12 = 'application/EDI-X12';
  rctAPPLICATIONEDIFACT = 'application/EDIFACT';
  rctAPPLICATIONFONTWOFF = 'application/font-woff';
  rctAPPLICATIONGZIP = 'application/gzip';
  rctAPPLICATIONJAVASCRIPT = 'application/javascript';
  rctAPPLICATIONJSON = 'application/json';
  rctAPPLICATIONBSON = 'application/bson';
  rctAPPLICATIONOCTETSTREAM = 'application/octet-stream';
  rctAPPLICATIONOGG = 'application/ogg';
  rctAPPLICATIONPDF = 'application/pdf';
  rctAPPLICATIONPOSTSCRIPT = 'application/postscript';
  rctAPPLICATIONRDFXML = 'application/rdf+xml';
  rctAPPLICATIONRSSXML = 'application/rss+xml';
  rctAPPLICATIONSOAPXML = 'application/soap+xml';
  rctAPPLICATIONVNDANDROIDPACKAGEARCHIVE = 'application/vnd.android.package-archive';
  rctAPPLICATIONVNDDART = 'application/vnd.dart';
  rctAPPLICATIONVNDEMBARCADEROFIREDACJSON = 'application/vnd.embarcadero.firedac+json';
  rctAPPLICATIONVNDGOOGLEEARTHKMLXML = 'application/vnd.google-earth.kml+xml';
  rctAPPLICATIONVNDGOOGLEEARTHKMZ = 'application/vnd.google-earth.kmz';
  rctAPPLICATIONVNDMOZILLAXULXML = 'application/vnd.mozilla.xul+xml';
  rctAPPLICATIONVNDMSEXCEL = 'application/vnd.ms-excel';
  rctAPPLICATIONVNDMSPOWERPOINT = 'application/vnd.ms-powerpoint';
  rctAPPLICATIONVNDOASISOPENDOCUMENTGRAPHICS =
    'application/vnd.oasis.opendocument.graphics';
  rctAPPLICATIONVNDOASISOPENDOCUMENTPRESENTATION =
    'application/vnd.oasis.opendocument.presentation';
  rctAPPLICATIONVNDOASISOPENDOCUMENTSPREADSHEET =
    'application/vnd.oasis.opendocument.spreadsheet';
  rctAPPLICATIONVNDOASISOPENDOCUMENTTEXT = 'application/vnd.oasis.opendocument.text';
  rctAPPLICATIONVNDOPENXMLFORMATSOFFICEDOCUMENTPRESENTATIONMLPRESENTATION =
    'application/vnd.openxmlformats-officedocument.presentationml.presentation';
  rctAPPLICATIONVNDOPENXMLFORMATSOFFICEDOCUMENTSPREADSHEETMLSHEET =
    'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet';
  rctAPPLICATIONVNDOPENXMLFORMATSOFFICEDOCUMENTWORDPROCESSINGMLDOCUMENT =
    'application/vnd.openxmlformats-officedocument.wordprocessingml.document';
  rctAPPLICATIONXDEB = 'application/x-deb';
  rctAPPLICATIONXDVI = 'application/x-dvi';
  rctAPPLICATIONXFONTTTF = 'application/x-font-ttf';
  rctAPPLICATIONXJAVASCRIPT = 'application/x-javascript';
  rctAPPLICATIONXLATEX = 'application/x-latex';
  rctAPPLICATIONXMPEGURL = 'application/x-mpegURL';
  rctAPPLICATIONXPKCS12 = 'application/x-pkcs12';
  rctAPPLICATIONXPKCS7CERTIFICATES = 'application/x-pkcs7-certificates';
  rctAPPLICATIONXPKCS7CERTREQRESP = 'application/x-pkcs7-certreqresp';
  rctAPPLICATIONXPKCS7MIME = 'application/x-pkcs7-mime';
  rctAPPLICATIONXPKCS7SIGNATURE = 'application/x-pkcs7-signature';
  rctAPPLICATIONXRARCOMPRESSED = 'application/x-rar-compressed';
  rctAPPLICATIONXSHOCKWAVEFLASH = 'application/x-shockwave-flash';
  rctAPPLICATIONXSTUFFIT = 'application/x-stuffit';
  rctAPPLICATIONXTAR = 'application/x-tar';
  rctAPPLICATIONXWWWFORMURLENCODED = 'application/x-www-form-urlencoded';
  rctAPPLICATIONXXPINSTALL = 'application/x-xpinstall';
  rctAPPLICATIONXHTMLXML = 'application/xhtml+xml';
  rctAPPLICATIONXML = 'application/xml';
  rctAPPLICATIONXMLDTD = 'application/xml-dtd';
  rctAPPLICATIONXOPXML = 'application/xop+xml';
  rctAPPLICATIONZIP = 'application/zip';
  rctAUDIOBASIC = 'audio/basic';
  rctAUDIOL24 = 'audio/L24';
  rctAUDIOMP4 = 'audio/mp4';
  rctAUDIOMPEG = 'audio/mpeg';
  rctAUDIOOGG = 'audio/ogg';
  rctAUDIOVNDRNREALAUDIO = 'audio/vnd.rn-realaudio';
  rctAUDIOVNDWAVE = 'audio/vnd.wave';
  rctAUDIOVORBIS = 'audio/vorbis';
  rctAUDIOWEBM = 'audio/webm';
  rctAUDIOXAAC = 'audio/x-aac';
  rctAUDIOXCAF = 'audio/x-caf';
  rctIMAGEGIF = 'image/gif';
  rctIMAGEJPEG = 'image/jpeg';
  rctIMAGEICON = 'image/icon';
  rctIMAGEPJPEG = 'image/pjpeg';
  rctIMAGEPNG = 'image/png';
  rctIMAGESVGXML = 'image/svg+xml';
  rctIMAGETIFF = 'image/tiff';
  rctIMAGEXXCF = 'image/x-xcf';
  rctMESSAGEHTTP = 'message/http';
  rctMESSAGEIMDNXML = 'message/imdn+xml';
  rctMESSAGEPARTIAL = 'message/partial';
  rctMESSAGERFC822 = 'message/rfc822';
  rctMODELEXAMPLE = 'model/example';
  rctMODELIGES = 'model/iges';
  rctMODELMESH = 'model/mesh';
  rctMODELVRML = 'model/vrml';
  rctMODELX3DBINARY = 'model/x3d+binary';
  rctMODELX3DVRML = 'model/x3d+vrml';
  rctMODELX3DXML = 'model/x3d+xml';
  rctMULTIPARTALTERNATIVE = 'multipart/alternative';
  rctMULTIPARTENCRYPTED = 'multipart/encrypted';
  rctMULTIPARTFORMDATA = 'multipart/form-data';
  rctMULTIPARTMIXED = 'multipart/mixed';
  rctMULTIPARTRELATED = 'multipart/related';
  rctMULTIPARTSIGNED = 'multipart/signed';
  rctTEXTCMD = 'text/cmd';
  rctTEXTCSS = 'text/css';
  rctTEXTCSV = 'text/csv';
  rctTEXTHTML = 'text/html';
  rctTEXTJAVASCRIPT = 'text/javascript';
  rctTEXTPLAIN = 'text/plain';

  { Typed binary params.

    A param value normally travels as UTF-8 text, so every numeric read is a
    locale-dependent parse: a client writing 2,5 and a server reading with '.'
    as the decimal separator silently gets 0. Dates are worse - 03/04 is March
    4th or April 3rd depending on the machine.

    These content types mark a param whose payload is the raw value instead:
    little-endian, fixed size, no parse and no locale involved. They apply to
    rpkBODY params carried by multipart (two or more params), where the encoder
    copies the stream verbatim and the decoder restores both the name and the
    content type. Reading stays tolerant: without one of these markers the
    accessors fall back to parsing text, so a client that was not updated keeps
    working exactly as before. }
  rctRALINT32 = 'application/x-ral-int32';
  rctRALINT64 = 'application/x-ral-int64';
  rctRALDOUBLE = 'application/x-ral-double';
  rctRALCURRENCY = 'application/x-ral-currency';
  rctRALBOOLEAN = 'application/x-ral-boolean';
  rctRALDATETIME = 'application/x-ral-datetime';

  rctTEXTVCARD = 'text/vcard';
  rctTEXTXGWTRPC = 'text/x-gwt-rpc';
  rctTEXTXJQUERYTMPL = 'text/x-jquery-tmpl';
  rctTEXTXMARKDOWN = 'text/x-markdown';
  rctTEXTXML = 'text/xml';
  rctVIDEOMP4 = 'video/mp4';
  rctVIDEOMPEG = 'video/mpeg';
  rctVIDEOOGG = 'video/ogg';
  rctVIDEOQUICKTIME = 'video/quicktime';
  rctVIDEOWEBM = 'video/webm';
  rctVIDEOXFLV = 'video/x-flv';
  rctVIDEOXMATROSKA = 'video/x-matroska';
  rctVIDEOXMSWMV = 'video/x-ms-wmv';
  {$ENDREGION}

type
  TRALMIMEStrings = array of StringRAL;

  { TRALMIMEType }

  TRALMIMEType = class
  private
    FInternalMIMEList: TStringList;
    { extension -> type by hash, for GetMIMEType - which runs for every file
      served. The binary search over the list cut the name out of an entry and
      lowercased it at every probe, two strings each, twenty per lookup; and
      the list is sorted by AnsiCompareText, which on Windows ignores hyphens,
      while the search compared byte by byte - an extension with a hyphen or an
      underscore could be in the list and not be found. Open addressing, the
      slots a power of two, kept at most half full }
    FExtKeys: TRALMIMEStrings;
    FExtTypes: TRALMIMEStrings;
    FExtCount: IntegerRAL;
    function ExtSlot(const AExt: StringRAL; out AFound: boolean): IntegerRAL;
    procedure ExtAdd(const AExt, AType: StringRAL);

    class var FInstance: TRALMIMEType;
  protected
    procedure SetDefaultTypes;
    function GetSystemTypes: boolean;

    // busca binaria
    function IndexOfExt(AExt: StringRAL): IntegerRAL;

    {$IF DEFINED(RALApple) or DEFINED(RALAppleFPC)}
      function GetMimeTypeMACOs(AExtension: string): string;
    {$IFEND}
    class procedure ReleaseInstance; static;
  public
    constructor Create;
    destructor Destroy; override;

    class function GetInstance: TRALMIMEType; static;

    function GetMIMEContentExt(const AContentType: StringRAL): StringRAL;
    function GetMIMEType(const AFileName: StringRAL): StringRAL;

    function AddMIMEType(AExt, AType : StringRAL) : boolean;
    {$IFDEF RALDEBUG}
    function GetInternalList: StringRAL;
    {$ENDIF}
  end;

const
  DEFAULTCONTENTTYPE = rctNONE;

/// True for media types whose bytes are compressed already - images, audio,
/// video, archives, web fonts: a coding over them spends a whole pass to come
/// out the same size or larger. Parameters ('; charset=') and case are ignored
function RALIsCompressedMediaType(const AContentType: StringRAL): boolean;

implementation

uses
  RALTools;

{$I RALMIMETypes.inc}

function RALIsCompressedMediaType(const AContentType: StringRAL): boolean;
const
  { whole types. What compresses is left out on purpose - SVG, BMP, TIFF,
    WAV, TTF/OTF, PDF - and so is application/octet-stream, which says nothing
    about the bytes. Windows' registry spells a few its own way, and those are
    here too }
  cTypes: array[0..36] of StringRAL = (
    'image/png', 'image/jpeg', 'image/pjpeg', 'image/gif', 'image/webp',
    'image/avif', 'image/heic', 'image/heif', 'image/jxl', 'image/jp2',
    'audio/mpeg', 'audio/mp4', 'audio/aac', 'audio/x-aac', 'audio/ogg',
    'audio/opus', 'audio/webm', 'audio/flac', 'audio/x-flac', 'audio/vorbis',
    'audio/x-m4a', 'audio/x-ms-wma',
    'application/zip', 'application/x-zip-compressed', 'application/gzip',
    'application/x-gzip',
    'application/x-bzip2', 'application/x-xz', 'application/x-7z-compressed',
    'application/x-rar-compressed', 'application/vnd.rar', 'application/zstd',
    'application/java-archive', 'application/vnd.android.package-archive',
    'application/font-woff', 'font/woff', 'font/woff2');
var
  vType: StringRAL;
  vPos, vInt: IntegerRAL;
begin
  vType := RALTrim(AContentType);
  vPos := Pos(StringRAL(';'), vType);
  if vPos > 0 then
    vType := RALTrim(Copy(vType, 1, vPos - 1));

  { video is compressed whatever its container says }
  Result := RALSameName(Copy(vType, 1, 6), 'video/');
  if Result then
    Exit;

  for vInt := Low(cTypes) to High(cTypes) do
    if RALSameName(vType, cTypes[vInt]) then
    begin
      Result := True;
      Break;
    end;
end;

{ TRALMIMEType }

constructor TRALMIMEType.Create;
begin
  FInternalMIMEList := TStringList.Create;
  FInternalMIMEList.Sorted := True;

  FInternalMIMEList.Clear;

  SetDefaultTypes;
  GetSystemTypes;
end;

destructor TRALMIMEType.Destroy;
begin
  if Assigned(FInternalMIMEList) then
    FreeAndNil(FInternalMIMEList);
  inherited;
end;

{$IFDEF RALDEBUG}
function TRALMIMEType.GetInternalList: StringRAL;
begin
  Result := FInternalMIMEList.Text;
end;
{$ENDIF}

function TRALMIMEType.GetMIMEContentExt(const AContentType: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  try
    for vInt := 0 to Pred(FInternalMIMEList.Count) do
    begin
      if SameText(FInternalMIMEList.ValueFromIndex[vInt], AContentType) then
      begin
        Result := FInternalMIMEList.Names[vInt];
        Break;
      end;
    end;
  except
    Result := '';
  end;
end;

{ FNV-1a of the extension with 'A'..'Z' folded, so that two spellings
  RALSameName calls equal always land on the same slot. 32-bit arithmetic that
  wraps on purpose: overflow and range checks are off for this one function }
{$IFOPT Q+}{$DEFINE RALMIME_Q}{$Q-}{$ENDIF}
{$IFOPT R+}{$DEFINE RALMIME_R}{$R-}{$ENDIF}
function ExtHash(const AExt: StringRAL): Cardinal;
var
  vByte: PByte;
  vInt: IntegerRAL;
  vChr: Byte;
begin
  Result := 2166136261;
  vByte := PByte(Pointer(AExt));
  for vInt := 1 to Length(AExt) do
  begin
    vChr := vByte^;
    if (vChr >= Ord('A')) and (vChr <= Ord('Z')) then
      Inc(vChr, 32);
    Result := (Result xor vChr) * Cardinal(16777619);
    Inc(vByte);
  end;
end;
{$IFDEF RALMIME_Q}{$Q+}{$UNDEF RALMIME_Q}{$ENDIF}
{$IFDEF RALMIME_R}{$R+}{$UNDEF RALMIME_R}{$ENDIF}

function TRALMIMEType.ExtSlot(const AExt: StringRAL; out AFound: boolean): IntegerRAL;
var
  vMask: IntegerRAL;
begin
  AFound := False;
  Result := -1;
  if Length(FExtKeys) = 0 then
    Exit;
  vMask := Length(FExtKeys) - 1;
  Result := IntegerRAL(ExtHash(AExt) and Cardinal(vMask));
  { linear probing: the run ends at an empty slot. Keys are never removed,
    so a run is never broken }
  while FExtKeys[Result] <> '' do
  begin
    if RALSameName(FExtKeys[Result], AExt) then
    begin
      AFound := True;
      Exit;
    end;
    Result := (Result + 1) and vMask;
  end;
end;

procedure TRALMIMEType.ExtAdd(const AExt, AType: StringRAL);
var
  vOldKeys, vOldTypes: TRALMIMEStrings;
  vInt, vSlot: IntegerRAL;
  vFound: boolean;
begin
  if AExt = '' then
    Exit;
  if (FExtCount + 1) * 2 > Length(FExtKeys) then
  begin
    vOldKeys := FExtKeys;
    vOldTypes := FExtTypes;
    FExtKeys := nil;
    FExtTypes := nil;
    if Length(vOldKeys) = 0 then
      SetLength(FExtKeys, 1024)
    else
      SetLength(FExtKeys, Length(vOldKeys) * 2);
    SetLength(FExtTypes, Length(FExtKeys));
    for vInt := 0 to High(vOldKeys) do
      if vOldKeys[vInt] <> '' then
      begin
        vSlot := ExtSlot(vOldKeys[vInt], vFound);
        FExtKeys[vSlot] := vOldKeys[vInt];
        FExtTypes[vSlot] := vOldTypes[vInt];
      end;
  end;
  vSlot := ExtSlot(AExt, vFound);
  if vFound then
    Exit; // the first type given for an extension is the one that stays
  FExtKeys[vSlot] := AExt;
  FExtTypes[vSlot] := AType;
  Inc(FExtCount);
end;

function TRALMIMEType.GetMIMEType(const AFileName: StringRAL): StringRAL;
var
  vSlot : IntegerRAL;
  vExt : StringRAL;
  vFound : boolean;
begin
  Result := '';
  vExt := ExtractFileExt(AFileName);
  vSlot := ExtSlot(vExt, vFound);
  if vFound then
  begin
    Result := FExtTypes[vSlot];
  end
  {$IF DEFINED(RALApple) or DEFINED(RALAppleFPC)}
    else
    begin
      { asked every time, not cached: the list is a singleton that request
        threads search without a lock, and adding to it here - a sorted
        insert - moved entries under a binary search running on another
        thread. The system lookup only happens for extensions the list does
        not have }
      Result := GetMimeTypeMACOs(vExt);
    end
  {$IFEND};
end;

{$IFDEF RALApple}
{ Delphi's RTL does not import the UTType functions (Macapi.CoreServices leaves
  LaunchServices out), so they are declared here. They live in CoreServices on
  macOS and in MobileCoreServices on iOS }
const
  {$IFDEF IOS}
  cUTTypeLib = '/System/Library/Frameworks/MobileCoreServices.framework/MobileCoreServices';
  {$ELSE}
  cUTTypeLib = '/System/Library/Frameworks/CoreServices.framework/CoreServices';
  {$ENDIF}
  {$IF NOT DECLARED(_PU)}
    {$IFDEF UNDERSCOREIMPORTNAME}
    _PU = '_';
    {$ELSE}
    _PU = '';
    {$ENDIF}
  {$IFEND}

function UTTypeCreatePreferredIdentifierForTag(AInTagClass, AInTag,
  AInConformingToUTI: CFStringRef): CFStringRef; cdecl;
  external cUTTypeLib name _PU + 'UTTypeCreatePreferredIdentifierForTag';
function UTTypeCopyPreferredTagWithClass(AInUTI, AInTagClass: CFStringRef): CFStringRef; cdecl;
  external cUTTypeLib name _PU + 'UTTypeCopyPreferredTagWithClass';

{ kUTTagClassFilenameExtension and kUTTagClassMIMEType are exported data the RTL
  does not import either; their values are fixed UTI tag class names }
function kUTTagClassFilenameExtension: CFStringRef;
begin
  Result := CFSTR('public.filename-extension');
end;

function kUTTagClassMIMEType: CFStringRef;
begin
  Result := CFSTR('public.mime-type');
end;
{$ENDIF}

{$IF DEFINED(RALApple) or DEFINED(RALAppleFPC)}
function TRALMIMEType.GetMimeTypeMACOs(AExtension: string): string;
var
  ExtCF, UTI, MimeCF: CFStringRef;
  {$IFDEF RALAppleFPC}
    Buffer: array[0..255] of Char;
  {$ENDIF}
begin
  Result := '';

  if (AExtension <> '') and (AExtension[POSINISTR] = '.') then
    Delete(AExtension, POSINISTR, 1);

  {$IFDEF RALApple}
    ExtCF := CFStringCreateWithCString(nil,
                                       MarshaledAString(UTF8String(AExtension)),
                                       kCFStringEncodingUTF8);
  {$ELSE}
    ExtCF := CFStringCreateWithCString(nil, PChar(AExtension), kCFStringEncodingUTF8);
  {$ENDIF}

  if ExtCF = nil then
    Exit;

  try
    UTI := UTTypeCreatePreferredIdentifierForTag(kUTTagClassFilenameExtension,
                                                 ExtCF, nil);

    if UTI <> nil then
    begin
      try
        MimeCF := UTTypeCopyPreferredTagWithClass(UTI, kUTTagClassMIMEType);

        if MimeCF <> nil then
        begin
          try
            {$IFDEF RALApple}
              Result := CFStringRefToStr(MimeCF);
            {$ELSE}
              if CFStringGetCString(MimeCF, Buffer, SizeOf(Buffer), kCFStringEncodingUTF8) then
                Result := Buffer;
            {$ENDIF}
          finally
            CFRelease(MimeCF);
          end;
        end
      finally
        CFRelease(UTI);
      end;
    end;
  finally
    CFRelease(ExtCF);
  end;
end;
{$IFEND}

function TRALMIMEType.AddMIMEType(AExt, AType: StringRAL): boolean;
var
  vFound: boolean;
begin
  ExtSlot(AExt, vFound);
  Result := not vFound;
  if Result then
  begin
    FInternalMIMEList.Add(AExt + '=' + AType);
    ExtAdd(AExt, AType);
  end;
end;

function TRALMIMEType.GetSystemTypes: boolean;
  {$IFDEF RALWindows}
  procedure LoadRegistry;
  const
    CExtsKey = '\';
    CTypesKey = '\MIME\Database\Content Type\';
  var
    LReg: TRegistry;
    LKeys: TStringList;
    LExt, LType: string;
  begin
    LReg := TRegistry.Create;
    try
      LKeys := TStringList.Create;
      try
        LReg.RootKey := HKEY_CLASSES_ROOT;
        if LReg.OpenKeyReadOnly(CExtsKey) then
        begin
          LReg.GetKeyNames(LKeys);
          for LExt in LKeys do
          begin
            if (LExt <> '') and (LExt[POSINISTR] = '.') and (LReg.OpenKeyReadOnly(CExtsKey + LExt)) then
            begin
              LType := Trim(LReg.ReadString('Content Type'));
              if LType <> '' then
                AddMIMEType(LExt, LType);
            end;
          end;
        end;

        if LReg.OpenKeyReadOnly(CTypesKey) then
        begin
          LReg.GetKeyNames(LKeys);
          for LType in LKeys do
          begin
            if (Trim(LType) <> '') and (LReg.OpenKeyReadOnly(CTypesKey + LType)) then
            begin
              LExt := Trim(LReg.ReadString('Extension')); // do not localize
              if (LExt <> '') and (LExt[POSINISTR] = '.') then
                AddMIMEType(LExt, LType);
            end;
          end;
        end;
      finally
        FreeAndNil(LKeys);
      end;
    finally
      FreeAndNil(LReg);
    end;
  end;
  {$ENDIF}

  {$IF DEFINED(RALLinux) OR DEFINED(RALApple) or DEFINED(RALAppleFPC)}
  procedure LoadMimeTypes(const AFileName: string);
  var
    LTypes: TStringList;
    LItem: string;
    LInt, LPos : Integer;
    LExtTmp, LExt, LType: string;
  begin
    // Content Sample
    // LTYpe                  TABs     LExt LExt LExt LExt
    // application/onenote #9 #9 #9 #9 one onetoc2 onetmp onepkg

    LTypes := TStringList.Create;
    try
      LTypes.LoadFromFile(AFileName);

      for LInt := 0 to Pred(LTypes.Count) do
      begin
        LItem := Trim(LTypes.Strings[LInt]);
        if (LItem <> '') and (LItem[POSINISTR] <> '#') then
        begin
          LPos := LastDelimiter(#9, LItem);
          if LPos > 0 then
          begin
            LType := Trim(Copy(LItem, POSINISTR, LPos));
            LExtTmp := Trim(Copy(LItem, LPos, Length(LItem)));

            while LExtTmp <> '' do
            begin
              LPos := Pos(' ', LExtTmp);
              if LPos <= POSINISTR then
                LPos := Length(LExtTmp) + 1;

              LExt := Trim(Copy(LExtTmp, 1, LPos));
              if (LExt <> '') and (LExt[POSINISTR] <> '.') then
                LExt := '.' + LExt;

              AddMIMEType(LExt, LType);
              Delete(LExtTmp, 1, LPos);
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LTypes);
    end;
  end;
  {$IFEND}

  {$IFDEF RALLinux}
  procedure LoadGlobs(const AFileName: string);
  var
    LTypes: TStringList;
    LInt: Integer;
    LItem: string;
    LPos1, LPos2: Integer;
    LExt, LType: string;
  begin
    LTypes := TStringList.Create;
    try
      LTypes.LoadFromFile(AFileName);

      for LInt := 0 to Pred(LTypes.Count) do
      begin
        LItem := Trim(LTypes.Strings[LInt]);

        if (LItem <> '') and (LItem[POSINISTR] <> '#') then
        begin
          LPos1 := Pos(':', LItem);
          if LPos1 >= POSINISTR then
          begin
            LPos2 := Pos(':', LItem, LPos1 + 1);
            if LPos2 > 0 then
            begin
              // globs2 -> prioridade:mime:padrao
              LType := Copy(LItem, LPos1 + 1, LPos2 - LPos1 - 1);
              LExt := Copy(LItem, LPos2 + 1, Length(LItem));
            end
            else
            begin
              // globs -> mime:padrao
              LType := Copy(LItem, 1, LPos1 - 1);
              LExt := Copy(LItem, LPos1 + 1, Length(LItem));
            end;

            if (LExt <> '') and ((LExt[POSINISTR] = '*') or (LExt[POSINISTR] = '.')) then
            begin
              if (LExt[POSINISTR] = '*') then
                Delete(LExt, POSINISTR, 1);

              if (LExt <> '') and (LExt[POSINISTR] <> '.') then
                LExt := '.' + LExt;

              AddMIMEType(LExt, LType);
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LTypes);
    end;
  end;
  {$ENDIF}
begin
  Result := False;
  try
    {$IFDEF RALWindows}
    LoadRegistry;
    {$ENDIF}
    {$IFDEF RALLinux}
    if FileExists('/etc/mime.types') then
      LoadMimeTypes('/etc/mime.types');
    if FileExists('/usr/share/mime/globs2') then
      LoadGlobs('/usr/share/mime/globs2');
    if FileExists('/usr/share/mime/globs') then
      LoadGlobs('/usr/share/mime/globs');
    {$ENDIF}
    {$IF DEFINED(RALApple) or DEFINED(RALAppleFPC)}
    if FileExists('/etc/apache2/mime.types') then
      LoadMimeTypes('/etc/apache2/mime.types');
    {$IFEND}
    Result := True;
  except
    Result := False;
  end;
end;

function TRALMIMEType.IndexOfExt(AExt: StringRAL): IntegerRAL;
var
  vPinIni, vPinFim, vPinMeio : IntegerRAL;
  vName : StringRAL;
begin
  Result := -1;
  if FInternalMIMEList.Count = 0 then
    Exit;

  AExt := LowerCase(AExt);

  vPinIni := 0;
  vPinFim := FInternalMIMEList.Count - 1;
  while (vPinIni <= vPinFim) do
  begin
    vPinMeio := vPinIni + ((vPinFim - vPinIni) shr 1);
    vName := LowerCase(FInternalMIMEList.Names[vPinMeio]);
    if vName > AExt then
      vPinFim := vPinMeio - 1
    else if vName < AExt then
      vPinIni := vPinMeio + 1
    else if vName = AExt then
      Exit(vPinMeio);
  end;
end;

procedure TRALMIMEType.SetDefaultTypes;
var
  vInt : IntegerRAL;
begin
  for vInt := Low(RAL_MIME_TYPES) to High(RAL_MIME_TYPES) do
    AddMIMEType(RAL_MIME_TYPES[vInt].Ext, RAL_MIME_TYPES[vInt].MIME);
end;

class function TRALMIMEType.GetInstance: TRALMIMEType;
begin
  if FInstance = nil then
    FInstance := TRALMIMEType.Create;
  Result := FInstance;
end;

class procedure TRALMIMEType.ReleaseInstance;
begin
  FreeAndNil(FInstance);
end;

initialization
  TRALMIMEType.GetInstance;

finalization
  TRALMIMEType.ReleaseInstance;

end.
