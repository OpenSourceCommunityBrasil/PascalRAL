{ The media types of file extensions (IANA,
  https://www.iana.org/assignments/media-types) }
unit RALMIMETypes;

{$I ..\core\PascalRAL.inc}
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
  /// No media type.
  rctNONE = '';
  /// The media type application/atom+xml.
  rctAPPLICATIONATOMXML = 'application/atom+xml';
  /// The media type application/ecmascript.
  rctAPPLICATIONECMASCRIPT = 'application/ecmascript';
  /// The media type application/EDI-X12.
  rctAPPLICATIONEDIX12 = 'application/EDI-X12';
  /// The media type application/EDIFACT.
  rctAPPLICATIONEDIFACT = 'application/EDIFACT';
  /// The media type application/font-woff.
  rctAPPLICATIONFONTWOFF = 'application/font-woff';
  /// The media type application/gzip.
  rctAPPLICATIONGZIP = 'application/gzip';
  /// The media type application/javascript.
  rctAPPLICATIONJAVASCRIPT = 'application/javascript';
  /// The media type application/json.
  rctAPPLICATIONJSON = 'application/json';
  /// RFC 9457 problem details, the body of TRALServer.ErrorFormat = efProblemJSON.
  rctAPPLICATIONPROBLEMJSON = 'application/problem+json';
  /// The media type application/x-ndjson.
  rctAPPLICATIONNDJSON = 'application/x-ndjson';
  /// The media type application/bson.
  rctAPPLICATIONBSON = 'application/bson';
  /// The media type application/octet-stream.
  rctAPPLICATIONOCTETSTREAM = 'application/octet-stream';
  /// The media type application/ogg.
  rctAPPLICATIONOGG = 'application/ogg';
  /// The media type application/pdf.
  rctAPPLICATIONPDF = 'application/pdf';
  /// The media type application/postscript.
  rctAPPLICATIONPOSTSCRIPT = 'application/postscript';
  /// The media type application/rdf+xml.
  rctAPPLICATIONRDFXML = 'application/rdf+xml';
  /// The media type application/rss+xml.
  rctAPPLICATIONRSSXML = 'application/rss+xml';
  /// The media type application/soap+xml.
  rctAPPLICATIONSOAPXML = 'application/soap+xml';
  /// The media type application/vnd.android.package-archive.
  rctAPPLICATIONVNDANDROIDPACKAGEARCHIVE = 'application/vnd.android.package-archive';
  /// The media type application/vnd.dart.
  rctAPPLICATIONVNDDART = 'application/vnd.dart';
  /// The media type application/vnd.embarcadero.firedac+json.
  rctAPPLICATIONVNDEMBARCADEROFIREDACJSON = 'application/vnd.embarcadero.firedac+json';
  /// The media type application/vnd.google-earth.kml+xml.
  rctAPPLICATIONVNDGOOGLEEARTHKMLXML = 'application/vnd.google-earth.kml+xml';
  /// The media type application/vnd.google-earth.kmz.
  rctAPPLICATIONVNDGOOGLEEARTHKMZ = 'application/vnd.google-earth.kmz';
  /// The media type application/vnd.mozilla.xul+xml.
  rctAPPLICATIONVNDMOZILLAXULXML = 'application/vnd.mozilla.xul+xml';
  /// The media type application/vnd.ms-excel.
  rctAPPLICATIONVNDMSEXCEL = 'application/vnd.ms-excel';
  /// The media type application/vnd.ms-powerpoint.
  rctAPPLICATIONVNDMSPOWERPOINT = 'application/vnd.ms-powerpoint';
  /// The media type application/vnd.oasis.opendocument.graphics.
  rctAPPLICATIONVNDOASISOPENDOCUMENTGRAPHICS =
    'application/vnd.oasis.opendocument.graphics';
  /// The media type application/vnd.oasis.opendocument.presentation.
  rctAPPLICATIONVNDOASISOPENDOCUMENTPRESENTATION =
    'application/vnd.oasis.opendocument.presentation';
  /// The media type application/vnd.oasis.opendocument.spreadsheet.
  rctAPPLICATIONVNDOASISOPENDOCUMENTSPREADSHEET =
    'application/vnd.oasis.opendocument.spreadsheet';
  /// The media type application/vnd.oasis.opendocument.text.
  rctAPPLICATIONVNDOASISOPENDOCUMENTTEXT = 'application/vnd.oasis.opendocument.text';
  { The media type application/vnd.openxmlformats-
    officedocument.presentationml.presentation. }
  rctAPPLICATIONVNDOPENXMLFORMATSOFFICEDOCUMENTPRESENTATIONMLPRESENTATION =
    'application/vnd.openxmlformats-officedocument.presentationml.presentation';
  /// The media type application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.
  rctAPPLICATIONVNDOPENXMLFORMATSOFFICEDOCUMENTSPREADSHEETMLSHEET =
    'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet';
  { The media type application/vnd.openxmlformats-
    officedocument.wordprocessingml.document. }
  rctAPPLICATIONVNDOPENXMLFORMATSOFFICEDOCUMENTWORDPROCESSINGMLDOCUMENT =
    'application/vnd.openxmlformats-officedocument.wordprocessingml.document';
  /// The media type application/x-deb.
  rctAPPLICATIONXDEB = 'application/x-deb';
  /// The media type application/x-dvi.
  rctAPPLICATIONXDVI = 'application/x-dvi';
  /// The media type application/x-font-ttf.
  rctAPPLICATIONXFONTTTF = 'application/x-font-ttf';
  /// The media type application/x-javascript.
  rctAPPLICATIONXJAVASCRIPT = 'application/x-javascript';
  /// The media type application/x-latex.
  rctAPPLICATIONXLATEX = 'application/x-latex';
  /// The media type application/x-mpegURL.
  rctAPPLICATIONXMPEGURL = 'application/x-mpegURL';
  /// The media type application/x-pkcs12.
  rctAPPLICATIONXPKCS12 = 'application/x-pkcs12';
  /// The media type application/x-pkcs7-certificates.
  rctAPPLICATIONXPKCS7CERTIFICATES = 'application/x-pkcs7-certificates';
  /// The media type application/x-pkcs7-certreqresp.
  rctAPPLICATIONXPKCS7CERTREQRESP = 'application/x-pkcs7-certreqresp';
  /// The media type application/x-pkcs7-mime.
  rctAPPLICATIONXPKCS7MIME = 'application/x-pkcs7-mime';
  /// The media type application/x-pkcs7-signature.
  rctAPPLICATIONXPKCS7SIGNATURE = 'application/x-pkcs7-signature';
  /// The media type application/x-rar-compressed.
  rctAPPLICATIONXRARCOMPRESSED = 'application/x-rar-compressed';
  /// The media type application/x-shockwave-flash.
  rctAPPLICATIONXSHOCKWAVEFLASH = 'application/x-shockwave-flash';
  /// The media type application/x-stuffit.
  rctAPPLICATIONXSTUFFIT = 'application/x-stuffit';
  /// The media type application/x-tar.
  rctAPPLICATIONXTAR = 'application/x-tar';
  /// The media type application/x-www-form-urlencoded.
  rctAPPLICATIONXWWWFORMURLENCODED = 'application/x-www-form-urlencoded';
  /// The media type application/x-xpinstall.
  rctAPPLICATIONXXPINSTALL = 'application/x-xpinstall';
  /// The media type application/xhtml+xml.
  rctAPPLICATIONXHTMLXML = 'application/xhtml+xml';
  /// The media type application/xml.
  rctAPPLICATIONXML = 'application/xml';
  /// The media type application/xml-dtd.
  rctAPPLICATIONXMLDTD = 'application/xml-dtd';
  /// The media type application/xop+xml.
  rctAPPLICATIONXOPXML = 'application/xop+xml';
  /// The media type application/zip.
  rctAPPLICATIONZIP = 'application/zip';
  /// The media type audio/basic.
  rctAUDIOBASIC = 'audio/basic';
  /// The media type audio/L24.
  rctAUDIOL24 = 'audio/L24';
  /// The media type audio/mp4.
  rctAUDIOMP4 = 'audio/mp4';
  /// The media type audio/mpeg.
  rctAUDIOMPEG = 'audio/mpeg';
  /// The media type audio/ogg.
  rctAUDIOOGG = 'audio/ogg';
  /// The media type audio/vnd.rn-realaudio.
  rctAUDIOVNDRNREALAUDIO = 'audio/vnd.rn-realaudio';
  /// The media type audio/vnd.wave.
  rctAUDIOVNDWAVE = 'audio/vnd.wave';
  /// The media type audio/vorbis.
  rctAUDIOVORBIS = 'audio/vorbis';
  /// The media type audio/webm.
  rctAUDIOWEBM = 'audio/webm';
  /// The media type audio/x-aac.
  rctAUDIOXAAC = 'audio/x-aac';
  /// The media type audio/x-caf.
  rctAUDIOXCAF = 'audio/x-caf';
  /// The media type image/gif.
  rctIMAGEGIF = 'image/gif';
  /// The media type image/jpeg.
  rctIMAGEJPEG = 'image/jpeg';
  /// The media type image/icon.
  rctIMAGEICON = 'image/icon';
  /// The media type image/pjpeg.
  rctIMAGEPJPEG = 'image/pjpeg';
  /// The media type image/png.
  rctIMAGEPNG = 'image/png';
  /// The media type image/svg+xml.
  rctIMAGESVGXML = 'image/svg+xml';
  /// The media type image/tiff.
  rctIMAGETIFF = 'image/tiff';
  /// The media type image/x-xcf.
  rctIMAGEXXCF = 'image/x-xcf';
  /// The media type message/http.
  rctMESSAGEHTTP = 'message/http';
  /// The media type message/imdn+xml.
  rctMESSAGEIMDNXML = 'message/imdn+xml';
  /// The media type message/partial.
  rctMESSAGEPARTIAL = 'message/partial';
  /// The media type message/rfc822.
  rctMESSAGERFC822 = 'message/rfc822';
  /// The media type model/example.
  rctMODELEXAMPLE = 'model/example';
  /// The media type model/iges.
  rctMODELIGES = 'model/iges';
  /// The media type model/mesh.
  rctMODELMESH = 'model/mesh';
  /// The media type model/vrml.
  rctMODELVRML = 'model/vrml';
  /// The media type model/x3d+binary.
  rctMODELX3DBINARY = 'model/x3d+binary';
  /// The media type model/x3d+vrml.
  rctMODELX3DVRML = 'model/x3d+vrml';
  /// The media type model/x3d+xml.
  rctMODELX3DXML = 'model/x3d+xml';
  /// The media type multipart/alternative.
  rctMULTIPARTALTERNATIVE = 'multipart/alternative';
  /// The media type multipart/encrypted.
  rctMULTIPARTENCRYPTED = 'multipart/encrypted';
  /// The media type multipart/form-data.
  rctMULTIPARTFORMDATA = 'multipart/form-data';
  /// The media type multipart/mixed.
  rctMULTIPARTMIXED = 'multipart/mixed';
  /// The media type multipart/related.
  rctMULTIPARTRELATED = 'multipart/related';
  /// The media type multipart/signed.
  rctMULTIPARTSIGNED = 'multipart/signed';
  /// The media type text/cmd.
  rctTEXTCMD = 'text/cmd';
  /// The media type text/css.
  rctTEXTCSS = 'text/css';
  /// The media type text/event-stream.
  rctTEXTEVENTSTREAM = 'text/event-stream';
  /// The media type text/csv.
  rctTEXTCSV = 'text/csv';
  /// The media type text/html.
  rctTEXTHTML = 'text/html';
  /// The media type text/javascript.
  rctTEXTJAVASCRIPT = 'text/javascript';
  /// The media type text/plain.
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
    accessors fall back to parsing text, which is what a client without them
    sends. }
  rctRALINT32 = 'application/x-ral-int32';
  /// The media type application/x-ral-int64.
  rctRALINT64 = 'application/x-ral-int64';
  /// The media type application/x-ral-double.
  rctRALDOUBLE = 'application/x-ral-double';
  /// The media type application/x-ral-currency.
  rctRALCURRENCY = 'application/x-ral-currency';
  /// The media type application/x-ral-boolean.
  rctRALBOOLEAN = 'application/x-ral-boolean';
  /// The media type application/x-ral-datetime.
  rctRALDATETIME = 'application/x-ral-datetime';

  /// The media type text/vcard.
  rctTEXTVCARD = 'text/vcard';
  /// The media type text/x-gwt-rpc.
  rctTEXTXGWTRPC = 'text/x-gwt-rpc';
  /// The media type text/x-jquery-tmpl.
  rctTEXTXJQUERYTMPL = 'text/x-jquery-tmpl';
  /// The media type text/x-markdown.
  rctTEXTXMARKDOWN = 'text/x-markdown';
  /// The media type text/xml.
  rctTEXTXML = 'text/xml';
  /// The media type video/mp4.
  rctVIDEOMP4 = 'video/mp4';
  /// The media type video/mpeg.
  rctVIDEOMPEG = 'video/mpeg';
  /// The media type video/ogg.
  rctVIDEOOGG = 'video/ogg';
  /// The media type video/quicktime.
  rctVIDEOQUICKTIME = 'video/quicktime';
  /// The media type video/webm.
  rctVIDEOWEBM = 'video/webm';
  /// The media type video/x-flv.
  rctVIDEOXFLV = 'video/x-flv';
  /// The media type video/x-matroska.
  rctVIDEOXMATROSKA = 'video/x-matroska';
  /// The media type video/x-ms-wmv.
  rctVIDEOXMSWMV = 'video/x-ms-wmv';
  {$ENDREGION}

type
  /// A list of strings.
  TRALMIMEStrings = array of StringRAL;

  /// The media type of a file extension: the built-in table plus what the system knows.
  TRALMIMEType = class
  private
    /// Slots used in the hash.
    FExtCount: IntegerRAL;
    { The extensions of the hash GetMIMEType looks up (it runs for every file
      served): open addressing, the slots a power of two, kept at most half
      full }
    FExtKeys: TRALMIMEStrings;
    /// The media types of the hash, slot by slot.
    FExtTypes: TRALMIMEStrings;
    /// Every extension=type known.
    FInternalMIMEList: TStringList;

    /// Adds AExt to the hash.
    procedure ExtAdd(const AExt, AType: StringRAL);
    /// The slot of AExt in the hash; AFound says whether it is there.
    function ExtSlot(const AExt: StringRAL; out AFound: boolean): IntegerRAL;

    class var
      /// The shared instance.
      FInstance: TRALMIMEType;
  protected
    { Adds the types the system knows (the Windows registry, /etc/mime.types); False
      when there are none. }
    function GetSystemTypes: boolean;
    /// The index of AExt in the list, by binary search; -1 when absent.
    function IndexOfExt(AExt: StringRAL): IntegerRAL;
    /// Adds the built-in table.
    procedure SetDefaultTypes;

    {$IF DEFINED(RALApple) or DEFINED(RALAppleFPC)}
      /// The media type the system gives AExtension (Apple).
      function GetMimeTypeMACOs(AExtension: string): string;
    {$IFEND}
    /// Frees the shared instance.
    class procedure ReleaseInstance; static;
  public
    constructor Create;
    destructor Destroy; override;

    /// Adds AExt as AType; False when AExt is known already.
    function AddMIMEType(AExt, AType : StringRAL) : boolean;
    /// The shared instance, created on first use; do not free it.
    class function GetInstance: TRALMIMEType; static;
    /// The extension of a media type, or empty.
    function GetMIMEContentExt(const AContentType: StringRAL): StringRAL;
    /// The media type of a file name, by its extension.
    function GetMIMEType(const AFileName: StringRAL): StringRAL;
    {$IFDEF RALDEBUG}
    /// Every extension=type, one per line (RALDEBUG).
    function GetInternalList: StringRAL;
    {$ENDIF}
  end;

const
  /// The media type of nothing.
  DEFAULTCONTENTTYPE = rctNONE;

{ True for media types whose bytes are compressed already - images, audio,
  video, archives, web fonts: a coding over them spends a whole pass to come
  out the same size or larger. Parameters ('; charset=') and case are ignored. }
function RALIsCompressedMediaType(const AContentType: StringRAL): boolean;

implementation

uses
  RALTools;

{$I RALMIMETypes.inc}

function RALIsCompressedMediaType(const AContentType: StringRAL): boolean;
const
  { whole types. What compresses (SVG, BMP, TIFF, WAV, TTF/OTF, PDF) is not
    here, nor application/octet-stream, which says nothing about the bytes;
    the spellings of the Windows registry are }
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
  RALSameName calls equal always land on the same slot. The 32-bit arithmetic
  wraps: overflow and range checks are off for this one function }
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
  /// The library of the Uniform Type Identifiers (iOS).
  cUTTypeLib = '/System/Library/Frameworks/MobileCoreServices.framework/MobileCoreServices';
  {$ELSE}
  /// The library of the Uniform Type Identifiers (macOS).
  cUTTypeLib = '/System/Library/Frameworks/CoreServices.framework/CoreServices';
  {$ENDIF}
  {$IF NOT DECLARED(_PU)}
    {$IFDEF UNDERSCOREIMPORTNAME}
    /// The symbol prefix of the platform.
    _PU = '_';
    {$ELSE}
    /// The symbol prefix of the platform.
    _PU = '';
    {$ENDIF}
  {$IFEND}

/// The Apple UTTypeCreatePreferredIdentifierForTag.
function UTTypeCreatePreferredIdentifierForTag(AInTagClass, AInTag,
  AInConformingToUTI: CFStringRef): CFStringRef; cdecl;
  external cUTTypeLib name _PU + 'UTTypeCreatePreferredIdentifierForTag';
/// The Apple UTTypeCopyPreferredTagWithClass.
function UTTypeCopyPreferredTagWithClass(AInUTI, AInTagClass: CFStringRef): CFStringRef; cdecl;
  external cUTTypeLib name _PU + 'UTTypeCopyPreferredTagWithClass';

{ kUTTagClassFilenameExtension and kUTTagClassMIMEType are exported data the RTL
  does not import either; their values are fixed UTI tag class names }
function kUTTagClassFilenameExtension: CFStringRef;
begin
  Result := CFSTR('public.filename-extension');
end;

/// The Apple kUTTagClassMIMEType.
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
