/// Class for mapping default MIMETypes according to IANA
unit RALMIMETypes;

{$I ..\base\PascalRAL.inc}
{$IFDEF FPC}
  {$mode Delphi}
{$ENDIF}

interface

uses
  {$IFDEF RALApple}
    Macapi.CoreFoundation,
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
    FLoading : boolean;

    class var FInstance: TRALMIMEType;

    procedure SetDefaultTypes;
    function GetSystemTypes: boolean;

    // busca binaria
    function IndexOfExt(AExt: StringRAL): IntegerRAL;

    constructor Create;
    destructor Destroy; override;
  public
    class function GetInstance: TRALMIMEType; static;
    class procedure ReleaseInstance; static;

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

  FLoading := True;

  SetDefaultTypes;
  GetSystemTypes;

  FLoading := False;
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
      if SameText(FInternalMIMEList.ValueFromIndex[vInt], aContentType) then
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
begin
  Result := '';
  vIdx := IndexOfExt(ExtractFileExt(AFileName));
  if vIdx >= 0 then
    Result := FInternalMIMEList.ValueFromIndex[vIdx];
end;

function TRALMIMEType.AddMIMEType(AExt, AType: StringRAL): boolean;
var
  vIdx : integer;
begin
  vIdx := IndexOfExt(AExt);
  Result := vIdx < 0;
  if Result then
  begin
    FInternalMIMEList.Add(AExt + '=' + AType);
  end
  else if (not Result) and (not FLoading) then
  begin
    FInternalMIMEList.Delete(vIdx);
    FInternalMIMEList.Add(AExt + '=' + AType);
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

  {$IFDEF RALLinux}
  procedure LoadFile(const aFileName: string);
  var
    LTypes: TStringList;
    LItem: string;
    {$IFDEF DELPHIXE7UP}
    LArr: TArray<string>;
    {$ELSE}
    LArr: array of string;
    {$ENDIF}
    i, j: Integer;
  begin
    if not FileExists(aFileName) then
      Exit;
    LTypes := TStringList.Create;
    try
      try
        LTypes.LoadFromFile(aFileName);
      except
        // if file is not accessible (eg, no rights), then just exit
        Exit;
      end;
      for j := 0 to LTypes.Count - 1 do
      begin
        LItem := LTypes[j].Trim;
        if (LItem <> '') and not LItem.StartsWith('#') then
        begin
          LArr := LItem.Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
          if (LArr[0].Trim <> '') and (Length(LArr) > 1) then
            FInternalMIMEList.Add(LArr[1].Trim + '=' + LArr[0].Trim);
        end;
      end;
    finally
      LTypes.Free;
    end;
  end;
  {$ENDIF}

  {$IFDEF RALApple}
  procedure LoadFile(const aFileName: string);
  const
    CBinary: RawByteString = 'bplist';
  var
    LItems, LExts: TStringList;
    i: Integer;
    LArr: TArray<string>;
    LType: string;
    LMode: Integer;
    j: Integer;
    LFile: TFileStream;
    LHeader: RawByteString;
  begin
    if not FileExists(aFileName) then
      Exit;

    LItems := TStringList.Create;
    try
      LExts := TStringList.Create;
      try
        try
          LFile := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
          try
            SetLength(LHeader, Length(CBinary));
            // ignore binary plist
            if (LFile.Read(LHeader[1], Length(CBinary)) = Length(CBinary)) and
              (LHeader = CBinary) then
              Exit;
            LFile.Position := 0;
            LItems.LoadFromStream(LFile);
          finally
            LFile.Free;
          end;
        except
          // if file is not accessible (eg, no rights), then just exit
          Exit;
        end;

        LMode := -1;
        for i := 0 to LItems.Count - 1 do
        begin
          LArr := LItems[i].Split(['<', '>', #9, ' '], TStringSplitOptions.ExcludeEmpty);
          if Length(LArr) = 3 then
          begin
            if SameText(LArr[0], 'key') and SameText(LArr[1], 'CFBundleTypeExtensions')
            then
              LMode := 0
            else if SameText(LArr[0], 'key') and SameText(LArr[1], 'CFBundleTypeMIMETypes')
            then
              LMode := 1
            else if SameText(LArr[0], 'key') then
              LMode := 2
            else if SameText(LArr[0], 'string') then
            begin
              if LMode = 0 then
                LExts.Add(LArr[1])
              else if LMode = 1 then
                LType := LArr[1];
            end
          end
          else if (Length(LArr) = 1) and SameText(LArr[0], '/dict') and (LMode >= 0) then
          begin
            if LType.Trim <> '' then
              for j := 0 to LExts.Count - 1 do
                FInternalMIMEList.Add(LExt + '=' + LType);
            LMode := -1;
            LExts.Clear;
            LType := '';
          end
        end;
      finally
        LExts.Free;
      end;
    finally
      LItems.Free;
    end;
  end;
  {$ENDIF}

  {$IFDEF RALLinux}
const
  CTypeFile = 'mime.types';
  {$ENDIF}
  {$IFDEF RALApple}
const
  CTypeFile = '/Applications/Safari.app/Contents/Info.plist';
  {$ENDIF}
begin
  Result := False;
  try
    {$IFDEF RALWindows}
    LoadRegistry;
    {$ENDIF}
    {$IFDEF RALLinux}
    LoadFile('/etc/' + CTypeFile);
    {$ENDIF}
    {$IFDEF RALApple}
    LoadFile(CTypeFile);
    {$ENDIF}
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
    vPinMeio := vPinIni + ((vPinFim - vPinIni) div 2);
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
