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

  { TRALMIMEType }

  TRALMIMEType = class
  private
    FInternalMIMEList: TStringList;

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

implementation

{$I RALMimeTypes.inc}

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

function TRALMIMEType.GetMIMEType(const AFileName: StringRAL): StringRAL;
var
  vIdx : IntegerRAL;
  vExt : StringRAL;
begin
  Result := '';
  vExt := ExtractFileExt(AFileName);
  vIdx := IndexOfExt(vExt);
  if vIdx >= 0 then
  begin
    Result := FInternalMIMEList.ValueFromIndex[vIdx];
  end
  {$IF DEFINED(RALApple) or DEFINED(RALAppleFPC)}
    else
    begin
      Result := GetMimeTypeMACOs(vExt);
      if Result <> '' then
        AddMIMEType(vExt, Result);
    end
  {$IFEND};
end;

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
                                       MarshaledAString(AnsiString(AExtension))),
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
begin
  Result := IndexOfExt(AExt) < 0;
  if Result then
    FInternalMIMEList.Add(AExt + '=' + AType);
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
    LInt : Integer;
    LItem : string;
    LPos1, LPos2: Integer;
    LExt, LType: string;
  begin
    LTypes := TStringList.Create;
    try
      LTypes.LoadFromFile(AFileName);

      for LInt := 0 to Pred(SL.Count) do
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
