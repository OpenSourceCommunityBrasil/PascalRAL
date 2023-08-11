unit RALMIMETypes;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
  {$IFDEF MACOS}
  Macapi.CoreFoundation,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows, Registry,
  {$ENDIF}
  {$IFDEF POSIX}
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
  TRALMIMEType = class
  private
    FInternalMIMEList: TStringList;
    procedure SetDefaultTypes;
    function GetSystemTypes: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GetMIMEType(aFileName: StringRAL): StringRAL;
    function GetMIMEContentExt(aContentType: StringRAL): StringRAL;
  end;

const
  DEFAULTCONTENTTYPE = rctNONE;

implementation

{ TRALMIMEType }

constructor TRALMIMEType.Create;
begin
  FInternalMIMEList := TStringList.Create;
  FInternalMIMEList.Delimiter := '=';
  FInternalMIMEList.Duplicates := dupIgnore;
  FInternalMIMEList.Sorted := True;
  SetDefaultTypes;
  GetSystemTypes;  
end;

destructor TRALMIMEType.Destroy;
begin
  if Assigned(FInternalMIMEList) then
    FreeAndNil(FInternalMIMEList);
  inherited;
end;

function TRALMIMEType.GetMIMEContentExt(aContentType: StringRAL): StringRAL;
var
  vInt : IntegerRAL;
begin
  Result := '';
  try
    for vInt := 0 to Pred(FInternalMIMEList.Count) do
    begin
      if SameText(FInternalMIMEList.ValueFromIndex[vInt],aContentType) then
      begin
        Result := FInternalMIMEList.Names[vInt];
        Break;
      end;
    end;
  except
    Result := '';
  end;
end;

function TRALMIMEType.GetMIMEType(aFileName: StringRAL): StringRAL;
begin
  Result := FInternalMIMEList.Values[ExtractFileExt(aFileName)];
end;

function TRALMIMEType.GetSystemTypes: boolean;
{$IF DEFINED(MSWINDOWS)}
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
          LReg.CloseKey;
          for LExt in LKeys do
            if LExt.StartsWith('.') then
              if LReg.OpenKeyReadOnly(CExtsKey + LExt) then
              begin
                LType := LReg.ReadString('Content Type').Trim;
                if LType <> '' then
                  FInternalMIMEList.Add(LExt + '=' + LType);
                LReg.CloseKey;
              end;
        end;

        if LReg.OpenKeyReadOnly(CTypesKey) then
        begin
          LReg.GetKeyNames(LKeys);
          LReg.CloseKey;
          for LType in LKeys do
            if (LType.Trim <> '') and LReg.OpenKeyReadOnly(CTypesKey + LType) then
            begin
              LExt := LReg.ReadString('Extension').Trim; // do not localize
              if LExt <> '' then
                FInternalMIMEList.Add(LExt + '=' + LType);
              LReg.CloseKey;
            end;
        end;
      finally
        LKeys.Free;
      end;
    finally
      LReg.Free;
    end;
  end;

{$ENDIF}
{$IF DEFINED(LINUX)}
  procedure LoadFile(const aFileName: string);
  var
    LTypes: TStringList;
    LItem: string;
    LArr: array of string;
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
          if LArr[0].Trim <> '' then
            for i := 1 to Length(LArr) - 1 do
              FInternalMIMEList.Add(LExt + '=' + LType);
        end;
      end;
    finally
      LTypes.Free;
    end;
  end;
{$ENDIF}
{$IF DEFINED(MACOS) and not DEFINED(IOS)}
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
{$IF DEFINED(LINUX)}

const
  CTypeFile = 'mime.types';
  {$ENDIF}
  {$IF DEFINED(MACOS) and not DEFINED(IOS)}
const
  CTypeFile = '/Applications/Safari.app/Contents/Info.plist';
  {$ENDIF}
begin
  FInternalMIMEList.Clear;
  Result := False;
  try
    {$IF DEFINED(MSWINDOWS)}
    LoadRegistry;
    {$ENDIF}
    {$IF DEFINED(LINUX)}
    LoadFile(TPath.Combine(TPath.GetHomePath, '.' + CTypeFile));
    LoadFile('/etc/' + CTypeFile);
    {$ENDIF}
    {$IF DEFINED(MACOS) and not DEFINED(IOS)}
    LoadFile(CTypeFile);
    {$ENDIF}
    Result := True;
  except
    Result := False;
  end;
end;

procedure TRALMIMEType.SetDefaultTypes;
begin
  FInternalMIMEList.Clear;
  with FInternalMIMEList do
  begin
    {$REGION 'Add Default values'}
    Add('.ez=application/andrew-inset');
    Add('.aw=application/applixware');
    Add('.atom=application/atom+xml');
    Add('.atomcat=application/atomcat+xml');
    Add('.atomsvc=application/atomsvc+xml');
    Add('.bson=application/bson');
    Add('.ccxml=application/ccxml+xml');
    Add('.cdmia=application/cdmi-capability');
    Add('.cdmic=application/cdmi-container');
    Add('.cdmid=application/cdmi-domain');
    Add('.cdmio=application/cdmi-object');
    Add('.cdmiq=application/cdmi-queue');
    Add('.cu=application/cu-seeme');
    Add('.davmount=application/davmount+xml');
    Add('.dbk=application/docbook+xml');
    Add('.dssc=application/dssc+der');
    Add('.xdssc=application/dssc+xml');
    Add('.ecma=application/ecmascript');
    Add('.emma=application/emma+xml');
    Add('.epub=application/epub+zip');
    Add('.exi=application/exi');
    Add('.pfr=application/font-tdpfr');
    Add('.gml=application/gml+xml');
    Add('.gpx=application/gpx+xml');
    Add('.gxf=application/gxf');
    Add('.stk=application/hyperstudio');
    Add('.ink=application/inkml+xml');
    Add('.inkml=application/inkml+xml');
    Add('.ipfix=application/ipfix');
    Add('.jar=application/java-archive');
    Add('.ser=application/java-serialized-object');
    Add('.class=application/java-vm');
    Add('.js=application/javascript');
    Add('.json=application/json');
    Add('.map=application/json');
    Add('.jsonml=application/jsonml+json');
    Add('.lostxml=application/lost+xml');
    Add('.hqx=application/mac-binhex40');
    Add('.cpt=application/mac-compactpro');
    Add('.mads=application/mads+xml');
    Add('.mrc=application/marc');
    Add('.mrcx=application/marcxml+xml');
    Add('.ma=application/mathematica');
    Add('.nb=application/mathematica');
    Add('.mb=application/mathematica');
    Add('.mathml=application/mathml+xml');
    Add('.mbox=application/mbox');
    Add('.mscml=application/mediaservercontrol+xml');
    Add('.metalink=application/metalink+xml');
    Add('.meta4=application/metalink4+xml');
    Add('.mets=application/mets+xml');
    Add('.mods=application/mods+xml');
    Add('.m21=application/mp21');
    Add('.mp21=application/mp21');
    Add('.mp4s=application/mp4');
    Add('.doc=application/msword');
    Add('.dot=application/msword');
    Add('.mxf=application/mxf');
    Add('.bin=application/octet-stream');
    Add('.bpk=application/octet-stream');
    Add('.class=application/octet-stream');
    Add('.deploy=application/octet-stream');
    Add('.dist=application/octet-stream');
    Add('.distz=application/octet-stream');
    Add('.dmg=application/octet-stream');
    Add('.dms=application/octet-stream');
    Add('.dump=application/octet-stream');
    Add('.elc=application/octet-stream');
    Add('.iso=application/octet-stream');
    Add('.lha=application/octet-stream');
    Add('.lrf=application/octet-stream');
    Add('.lzh=application/octet-stream');
    Add('.mar=application/octet-stream');
    Add('.pkg=application/octet-stream');
    Add('.so=application/octet-stream');
    Add('.oda=application/oda');
    Add('.opf=application/oebps-package+xml');
    Add('.ogx=application/ogg');
    Add('.omdoc=application/omdoc+xml');
    Add('.onetoc=application/onenote');
    Add('.onetoc2=application/onenote');
    Add('.onetmp=application/onenote');
    Add('.onepkg=application/onenote');
    Add('.oxps=application/oxps');
    Add('.xer=application/patch-ops-error+xml');
    Add('.pdf=application/pdf');
    Add('.pgp=application/pgp-encrypted');
    Add('.asc=application/pgp-signature');
    Add('.sig=application/pgp-signature');
    Add('.prf=application/pics-rules');
    Add('.p10=application/pkcs10');
    Add('.p7m=application/pkcs7-mime');
    Add('.p7c=application/pkcs7-mime');
    Add('.p7s=application/pkcs7-signature');
    Add('.p8=application/pkcs8');
    Add('.ac=application/pkix-attr-cert');
    Add('.cer=application/pkix-cert');
    Add('.crl=application/pkix-crl');
    Add('.pkipath=application/pkix-pkipath');
    Add('.pki=application/pkixcmp');
    Add('.pls=application/pls+xml');
    Add('.ai=application/postscript');
    Add('.eps=application/postscript');
    Add('.ps=application/postscript');
    Add('.cww=application/prs.cww');
    Add('.pskcxml=application/pskc+xml');
    Add('.rdf=application/rdf+xml');
    Add('.rif=application/reginfo+xml');
    Add('.rnc=application/relax-ng-compact-syntax');
    Add('.rl=application/resource-lists+xml');
    Add('.rld=application/resource-lists-diff+xml');
    Add('.rs=application/rls-services+xml');
    Add('.gbr=application/rpki-ghostbusters');
    Add('.mft=application/rpki-manifest');
    Add('.roa=application/rpki-roa');
    Add('.rsd=application/rsd+xml');
    Add('.rss=application/rss+xml');
    Add('.rtf=application/rtf');
    Add('.sbml=application/sbml+xml');
    Add('.scq=application/scvp-cv-request');
    Add('.scs=application/scvp-cv-response');
    Add('.spq=application/scvp-vp-request');
    Add('.spp=application/scvp-vp-response');
    Add('.sdp=application/sdp');
    Add('.setpay=application/set-payment-initiation');
    Add('.setreg=application/set-registration-initiation');
    Add('.shf=application/shf+xml');
    Add('.smi=application/smil+xml');
    Add('.smil=application/smil+xml');
    Add('.soap=application/soap+xml');
    Add('.rq=application/sparql-query');
    Add('.srx=application/sparql-results+xml');
    Add('.gram=application/srgs');
    Add('.grxml=application/srgs+xml');
    Add('.sru=application/sru+xml');
    Add('.ssdl=application/ssdl+xml');
    Add('.ssml=application/ssml+xml');
    Add('.tei=application/tei+xml');
    Add('.teicorpus=application/tei+xml');
    Add('.tfi=application/thraud+xml');
    Add('.tsd=application/timestamped-data');
    Add('.plb=application/vnd.3gpp.pic-bw-large');
    Add('.psb=application/vnd.3gpp.pic-bw-small');
    Add('.pvb=application/vnd.3gpp.pic-bw-var');
    Add('.tcap=application/vnd.3gpp2.tcap');
    Add('.pwn=application/vnd.3m.post-it-notes');
    Add('.aso=application/vnd.accpac.simply.aso');
    Add('.imp=application/vnd.accpac.simply.imp');
    Add('.acu=application/vnd.acucobol');
    Add('.atc=application/vnd.acucorp');
    Add('.acutc=application/vnd.acucorp');
    Add('.air=application/vnd.adobe.air-application-installer-package+zip');
    Add('.fcdt=application/vnd.adobe.formscentral.fcdt');
    Add('.fxp=application/vnd.adobe.fxp');
    Add('.fxpl=application/vnd.adobe.fxp');
    Add('.xdp=application/vnd.adobe.xdp+xml');
    Add('.xfdf=application/vnd.adobe.xfdf');
    Add('.ahead=application/vnd.ahead.space');
    Add('.azf=application/vnd.airzip.filesecure.azf');
    Add('.azs=application/vnd.airzip.filesecure.azs');
    Add('.azw=application/vnd.amazon.ebook');
    Add('.acc=application/vnd.americandynamics.acc');
    Add('.ami=application/vnd.amiga.ami');
    Add('.apk=application/vnd.android.package-archive');
    Add('.cii=application/vnd.anser-web-certificate-issue-initiation');
    Add('.fti=application/vnd.anser-web-funds-transfer-initiation');
    Add('.atx=application/vnd.antix.game-component');
    Add('.mpkg=application/vnd.apple.installer+xml');
    Add('.m3u8=application/vnd.apple.mpegurl');
    Add('.swi=application/vnd.aristanetworks.swi');
    Add('.iota=application/vnd.astraea-software.iota');
    Add('.aep=application/vnd.audiograph');
    Add('.mpm=application/vnd.blueice.multipass');
    Add('.bmi=application/vnd.bmi');
    Add('.rep=application/vnd.businessobjects');
    Add('.cdxml=application/vnd.chemdraw+xml');
    Add('.mmd=application/vnd.chipnuts.karaoke-mmd');
    Add('.cdy=application/vnd.cinderella');
    Add('.cla=application/vnd.claymore');
    Add('.rp9=application/vnd.cloanto.rp9');
    Add('.c4g=application/vnd.clonk.c4group');
    Add('.c4d=application/vnd.clonk.c4group');
    Add('.c4f=application/vnd.clonk.c4group');
    Add('.c4p=application/vnd.clonk.c4group');
    Add('.c4u=application/vnd.clonk.c4group');
    Add('.c11amc=application/vnd.cluetrust.cartomobile-config');
    Add('.c11amz=application/vnd.cluetrust.cartomobile-config-pkg');
    Add('.csp=application/vnd.commonspace');
    Add('.cdbcmsg=application/vnd.contact.cmsg');
    Add('.cmc=application/vnd.cosmocaller');
    Add('.clkx=application/vnd.crick.clicker');
    Add('.clkk=application/vnd.crick.clicker.keyboard');
    Add('.clkp=application/vnd.crick.clicker.palette');
    Add('.clkt=application/vnd.crick.clicker.template');
    Add('.clkw=application/vnd.crick.clicker.wordbank');
    Add('.wbs=application/vnd.criticaltools.wbs+xml');
    Add('.pml=application/vnd.ctc-posml');
    Add('.ppd=application/vnd.cups-ppd');
    Add('.car=application/vnd.curl.car');
    Add('.pcurl=application/vnd.curl.pcurl');
    Add('.dart=application/vnd.dart');
    Add('.rdz=application/vnd.data-vision.rdz');
    Add('.uvf=application/vnd.dece.data');
    Add('.uvvf=application/vnd.dece.data');
    Add('.uvd=application/vnd.dece.data');
    Add('.uvvd=application/vnd.dece.data');
    Add('.uvt=application/vnd.dece.ttml+xml');
    Add('.uvvt=application/vnd.dece.ttml+xml');
    Add('.uvx=application/vnd.dece.unspecified');
    Add('.uvvx=application/vnd.dece.unspecified');
    Add('.uvz=application/vnd.dece.zip');
    Add('.uvvz=application/vnd.dece.zip');
    Add('.fe_launch=application/vnd.denovo.fcselayout-link');
    Add('.dna=application/vnd.dna');
    Add('.mlp=application/vnd.dolby.mlp');
    Add('.dpg=application/vnd.dpgraph');
    Add('.dfac=application/vnd.dreamfactory');
    Add('.kpxx=application/vnd.ds-keypoint');
    Add('.ait=application/vnd.dvb.ait');
    Add('.svc=application/vnd.dvb.service');
    Add('.geo=application/vnd.dynageo');
    Add('.mag=application/vnd.ecowin.chart');
    Add('.nml=application/vnd.enliven');
    Add('.esf=application/vnd.epson.esf');
    Add('.msf=application/vnd.epson.msf');
    Add('.qam=application/vnd.epson.quickanime');
    Add('.slt=application/vnd.epson.salt');
    Add('.ssf=application/vnd.epson.ssf');
    Add('.es3=application/vnd.eszigno3+xml');
    Add('.et3=application/vnd.eszigno3+xml');
    Add('.ez2=application/vnd.ezpix-album');
    Add('.ez3=application/vnd.ezpix-package');
    Add('.fdf=application/vnd.fdf');
    Add('.mseed=application/vnd.fdsn.mseed');
    Add('.seed=application/vnd.fdsn.seed');
    Add('.dataless=application/vnd.fdsn.seed');
    Add('.json=application/vnd.embarcadero.firedac+json');
    Add('.xml=application/vnd.embarcadero.firedac+xml');
    Add('.bin=application/vnd.embarcadero.firedac+bin');
    Add('.gph=application/vnd.flographit');
    Add('.ftc=application/vnd.fluxtime.clip');
    Add('.fm=application/vnd.framemaker');
    Add('.frame=application/vnd.framemaker');
    Add('.maker=application/vnd.framemaker');
    Add('.book=application/vnd.framemaker');
    Add('.fnc=application/vnd.frogans.fnc');
    Add('.ltf=application/vnd.frogans.ltf');
    Add('.fsc=application/vnd.fsc.weblaunch');
    Add('.oas=application/vnd.fujitsu.oasys');
    Add('.oa2=application/vnd.fujitsu.oasys2');
    Add('.oa3=application/vnd.fujitsu.oasys3');
    Add('.fg5=application/vnd.fujitsu.oasysgp');
    Add('.bh2=application/vnd.fujitsu.oasysprs');
    Add('.ddd=application/vnd.fujixerox.ddd');
    Add('.xdw=application/vnd.fujixerox.docuworks');
    Add('.xbd=application/vnd.fujixerox.docuworks.binder');
    Add('.fzs=application/vnd.fuzzysheet');
    Add('.txd=application/vnd.genomatix.tuxedo');
    Add('.ggb=application/vnd.geogebra.file');
    Add('.ggt=application/vnd.geogebra.tool');
    Add('.gex=application/vnd.geometry-explorer');
    Add('.gre=application/vnd.geometry-explorer');
    Add('.gxt=application/vnd.geonext');
    Add('.g2w=application/vnd.geoplan');
    Add('.g3w=application/vnd.geospace');
    Add('.gmx=application/vnd.gmx');
    Add('.kml=application/vnd.google-earth.kml+xml');
    Add('.kmz=application/vnd.google-earth.kmz');
    Add('.gqf=application/vnd.grafeq');
    Add('.gqs=application/vnd.grafeq');
    Add('.gac=application/vnd.groove-account');
    Add('.ghf=application/vnd.groove-help');
    Add('.gim=application/vnd.groove-identity-message');
    Add('.grv=application/vnd.groove-injector');
    Add('.gtm=application/vnd.groove-tool-message');
    Add('.tpl=application/vnd.groove-tool-template');
    Add('.vcg=application/vnd.groove-vcard');
    Add('.hal=application/vnd.hal+xml');
    Add('.zmm=application/vnd.handheld-entertainment+xml');
    Add('.hbci=application/vnd.hbci');
    Add('.les=application/vnd.hhe.lesson-player');
    Add('.hpgl=application/vnd.hp-hpgl');
    Add('.hpid=application/vnd.hp-hpid');
    Add('.hps=application/vnd.hp-hps');
    Add('.jlt=application/vnd.hp-jlyt');
    Add('.pcl=application/vnd.hp-pcl');
    Add('.pclxl=application/vnd.hp-pclxl');
    Add('.sfd-hdstx=application/vnd.hydrostatix.sof-data');
    Add('.mpy=application/vnd.ibm.minipay');
    Add('.afp=application/vnd.ibm.modcap');
    Add('.listafp=application/vnd.ibm.modcap');
    Add('.list3820=application/vnd.ibm.modcap');
    Add('.irm=application/vnd.ibm.rights-management');
    Add('.sc=application/vnd.ibm.secure-container');
    Add('.icc=application/vnd.iccprofile');
    Add('.icm=application/vnd.iccprofile');
    Add('.igl=application/vnd.igloader');
    Add('.ivp=application/vnd.immervision-ivp');
    Add('.ivu=application/vnd.immervision-ivu');
    Add('.igm=application/vnd.insors.igm');
    Add('.xpw=application/vnd.intercon.formnet');
    Add('.xpx=application/vnd.intercon.formnet');
    Add('.i2g=application/vnd.intergeo');
    Add('.qbo=application/vnd.intu.qbo');
    Add('.qfx=application/vnd.intu.qfx');
    Add('.rcprofile=application/vnd.ipunplugged.rcprofile');
    Add('.irp=application/vnd.irepository.package+xml');
    Add('.xpr=application/vnd.is-xpr');
    Add('.fcs=application/vnd.isac.fcs');
    Add('.jam=application/vnd.jam');
    Add('.rms=application/vnd.jcp.javame.midlet-rms');
    Add('.jisp=application/vnd.jisp');
    Add('.joda=application/vnd.joost.joda-archive');
    Add('.ktz=application/vnd.kahootz');
    Add('.ktr=application/vnd.kahootz');
    Add('.karbon=application/vnd.kde.karbon');
    Add('.chrt=application/vnd.kde.kchart');
    Add('.kfo=application/vnd.kde.kformula');
    Add('.flw=application/vnd.kde.kivio');
    Add('.kon=application/vnd.kde.kontour');
    Add('.kpr=application/vnd.kde.kpresenter');
    Add('.kpt=application/vnd.kde.kpresenter');
    Add('.ksp=application/vnd.kde.kspread');
    Add('.kwd=application/vnd.kde.kword');
    Add('.kwt=application/vnd.kde.kword');
    Add('.htke=application/vnd.kenameaapp');
    Add('.kia=application/vnd.kidspiration');
    Add('.kne=application/vnd.kinar');
    Add('.knp=application/vnd.kinar');
    Add('.skp=application/vnd.koan');
    Add('.skd=application/vnd.koan');
    Add('.skt=application/vnd.koan');
    Add('.skm=application/vnd.koan');
    Add('.sse=application/vnd.kodak-descriptor');
    Add('.lasxml=application/vnd.las.las+xml');
    Add('.lbd=application/vnd.llamagraphics.life-balance.desktop');
    Add('.lbe=application/vnd.llamagraphics.life-balance.exchange+xml');
    Add('.123=application/vnd.lotus-1-2-3');
    Add('.apr=application/vnd.lotus-approach');
    Add('.pre=application/vnd.lotus-freelance');
    Add('.nsf=application/vnd.lotus-notes');
    Add('.org=application/vnd.lotus-organizer');
    Add('.scm=application/vnd.lotus-screencam');
    Add('.lwp=application/vnd.lotus-wordpro');
    Add('.portpkg=application/vnd.macports.portpkg');
    Add('.mcd=application/vnd.mcd');
    Add('.mc1=application/vnd.medcalcdata');
    Add('.cdkey=application/vnd.mediastation.cdkey');
    Add('.mwf=application/vnd.mfer');
    Add('.mfm=application/vnd.mfmp');
    Add('.flo=application/vnd.micrografx.flo');
    Add('.igx=application/vnd.micrografx.igx');
    Add('.mif=application/vnd.mif');
    Add('.daf=application/vnd.mobius.daf');
    Add('.dis=application/vnd.mobius.dis');
    Add('.mbk=application/vnd.mobius.mbk');
    Add('.mqy=application/vnd.mobius.mqy');
    Add('.msl=application/vnd.mobius.msl');
    Add('.plc=application/vnd.mobius.plc');
    Add('.txf=application/vnd.mobius.txf');
    Add('.mpn=application/vnd.mophun.application');
    Add('.mpc=application/vnd.mophun.certificate');
    Add('.xul=application/vnd.mozilla.xul+xml');
    Add('.cil=application/vnd.ms-artgalry');
    Add('.cab=application/vnd.ms-cab-compressed');
    Add('.xls=application/vnd.ms-excel');
    Add('.xlm=application/vnd.ms-excel');
    Add('.xla=application/vnd.ms-excel');
    Add('.xlc=application/vnd.ms-excel');
    Add('.xlt=application/vnd.ms-excel');
    Add('.xlw=application/vnd.ms-excel');
    Add('.xlam=application/vnd.ms-excel.addin.macroenabled.12');
    Add('.xlsb=application/vnd.ms-excel.sheet.binary.macroenabled.12');
    Add('.xlsm=application/vnd.ms-excel.sheet.macroenabled.12');
    Add('.xltm=application/vnd.ms-excel.template.macroenabled.12');
    Add('.eot=application/vnd.ms-fontobject');
    Add('.chm=application/vnd.ms-htmlhelp');
    Add('.ims=application/vnd.ms-ims');
    Add('.lrm=application/vnd.ms-lrm');
    Add('.thmx=application/vnd.ms-officetheme');
    Add('.cat=application/vnd.ms-pki.seccat');
    Add('.stl=application/vnd.ms-pki.stl');
    Add('.ppt=application/vnd.ms-powerpoint');
    Add('.pps=application/vnd.ms-powerpoint');
    Add('.pot=application/vnd.ms-powerpoint');
    Add('.ppam=application/vnd.ms-powerpoint.addin.macroenabled.12');
    Add('.pptm=application/vnd.ms-powerpoint.presentation.macroenabled.12');
    Add('.sldm=application/vnd.ms-powerpoint.slide.macroenabled.12');
    Add('.ppsm=application/vnd.ms-powerpoint.slideshow.macroenabled.12');
    Add('.potm=application/vnd.ms-powerpoint.template.macroenabled.12');
    Add('.mpp=application/vnd.ms-project');
    Add('.mpt=application/vnd.ms-project');
    Add('.docm=application/vnd.ms-word.document.macroenabled.12');
    Add('.dotm=application/vnd.ms-word.template.macroenabled.12');
    Add('.wps=application/vnd.ms-works');
    Add('.wks=application/vnd.ms-works');
    Add('.wcm=application/vnd.ms-works');
    Add('.wdb=application/vnd.ms-works');
    Add('.wpl=application/vnd.ms-wpl');
    Add('.xps=application/vnd.ms-xpsdocument');
    Add('.mseq=application/vnd.mseq');
    Add('.mus=application/vnd.musician');
    Add('.msty=application/vnd.muvee.style');
    Add('.taglet=application/vnd.mynfc');
    Add('.nlu=application/vnd.neurolanguage.nlu');
    Add('.ntf=application/vnd.nitf');
    Add('.nitf=application/vnd.nitf');
    Add('.nnd=application/vnd.noblenet-directory');
    Add('.nns=application/vnd.noblenet-sealer');
    Add('.nnw=application/vnd.noblenet-web');
    Add('.ngdat=application/vnd.nokia.n-gage.data');
    Add('.n-gage=application/vnd.nokia.n-gage.symbian.install');
    Add('.rpst=application/vnd.nokia.radio-preset');
    Add('.rpss=application/vnd.nokia.radio-presets');
    Add('.edm=application/vnd.novadigm.edm');
    Add('.edx=application/vnd.novadigm.edx');
    Add('.FExt=application/vnd.novadigm.FExt');
    Add('.odc=application/vnd.oasis.opendocument.chart');
    Add('.otc=application/vnd.oasis.opendocument.chart-template');
    Add('.odb=application/vnd.oasis.opendocument.database');
    Add('.odf=application/vnd.oasis.opendocument.formula');
    Add('.odft=application/vnd.oasis.opendocument.formula-template');
    Add('.odg=application/vnd.oasis.opendocument.graphics');
    Add('.otg=application/vnd.oasis.opendocument.graphics-template');
    Add('.odi=application/vnd.oasis.opendocument.image');
    Add('.oti=application/vnd.oasis.opendocument.image-template');
    Add('.odp=application/vnd.oasis.opendocument.presentation');
    Add('.otp=application/vnd.oasis.opendocument.presentation-template');
    Add('.ods=application/vnd.oasis.opendocument.spreadsheet');
    Add('.ots=application/vnd.oasis.opendocument.spreadsheet-template');
    Add('.odt=application/vnd.oasis.opendocument.text');
    Add('.odm=application/vnd.oasis.opendocument.text-master');
    Add('.ott=application/vnd.oasis.opendocument.text-template');
    Add('.oth=application/vnd.oasis.opendocument.text-web');
    Add('.xo=application/vnd.olpc-sugar');
    Add('.dd2=application/vnd.oma.dd2+xml');
    Add('.oxt=application/vnd.openofficeorg.extension');
    Add('.pptx=application/vnd.openxmlformats-officedocument.presentationml.presentation');
    Add('.sldx=application/vnd.openxmlformats-officedocument.presentationml.slide');
    Add('.ppsx=application/vnd.openxmlformats-officedocument.presentationml.slideshow');
    Add('.potx=application/vnd.openxmlformats-officedocument.presentationml.template');
    Add('.xlsx=application/vnd.openxmlformats-officedocument.spreadsheetml.sheet');
    Add('.xltx=application/vnd.openxmlformats-officedocument.spreadsheetml.template');
    Add('.docx=application/vnd.openxmlformats-officedocument.wordprocessingml.document');
    Add('.dotx=application/vnd.openxmlformats-officedocument.wordprocessingml.template');
    Add('.mgp=application/vnd.osgeo.mapguide.package');
    Add('.dp=application/vnd.osgi.dp');
    Add('.esa=application/vnd.osgi.subsystem');
    Add('.pdb=application/vnd.palm');
    Add('.pqa=application/vnd.palm');
    Add('.oprc=application/vnd.palm');
    Add('.paw=application/vnd.pawaafile');
    Add('.str=application/vnd.pg.format');
    Add('.ei6=application/vnd.pg.osasli');
    Add('.efif=application/vnd.picsel');
    Add('.wg=application/vnd.pmi.widget');
    Add('.plf=application/vnd.pocketlearn');
    Add('.pbd=application/vnd.powerbuilder6');
    Add('.box=application/vnd.previewsystems.box');
    Add('.mgz=application/vnd.proteus.magazine');
    Add('.qps=application/vnd.publishare-delta-tree');
    Add('.ptid=application/vnd.pvi.ptid1');
    Add('.qxd=application/vnd.quark.quarkxpress');
    Add('.qxt=application/vnd.quark.quarkxpress');
    Add('.qwd=application/vnd.quark.quarkxpress');
    Add('.qwt=application/vnd.quark.quarkxpress');
    Add('.qxl=application/vnd.quark.quarkxpress');
    Add('.qxb=application/vnd.quark.quarkxpress');
    Add('.bed=application/vnd.realvnc.bed');
    Add('.mxl=application/vnd.recordare.musicxml');
    Add('.musicxml=application/vnd.recordare.musicxml+xml');
    Add('.cryptonote=application/vnd.rig.cryptonote');
    Add('.cod=application/vnd.rim.cod');
    Add('.rm=application/vnd.rn-realmedia');
    Add('.rmvb=application/vnd.rn-realmedia-vbr');
    Add('.link66=application/vnd.route66.link66+xml');
    Add('.st=application/vnd.sailingtracker.track');
    Add('.see=application/vnd.seemail');
    Add('.sema=application/vnd.sema');
    Add('.semd=application/vnd.semd');
    Add('.semf=application/vnd.semf');
    Add('.ifm=application/vnd.shana.informed.formdata');
    Add('.itp=application/vnd.shana.informed.formtemplate');
    Add('.iif=application/vnd.shana.informed.interchange');
    Add('.ipk=application/vnd.shana.informed.package');
    Add('.twd=application/vnd.simtech-mindmapper');
    Add('.twds=application/vnd.simtech-mindmapper');
    Add('.mmf=application/vnd.smaf');
    Add('.teacher=application/vnd.smart.teacher');
    Add('.sdkm=application/vnd.solent.sdkm+xml');
    Add('.sdkd=application/vnd.solent.sdkm+xml');
    Add('.dxp=application/vnd.spotfire.dxp');
    Add('.sfs=application/vnd.spotfire.sfs');
    Add('.sdc=application/vnd.stardivision.calc');
    Add('.sda=application/vnd.stardivision.draw');
    Add('.sdd=application/vnd.stardivision.impress');
    Add('.smf=application/vnd.stardivision.math');
    Add('.sdw=application/vnd.stardivision.writer');
    Add('.vor=application/vnd.stardivision.writer');
    Add('.sgl=application/vnd.stardivision.writer-global');
    Add('.smzip=application/vnd.stepmania.package');
    Add('.sm=application/vnd.stepmania.stepchart');
    Add('.sxc=application/vnd.sun.xml.calc');
    Add('.stc=application/vnd.sun.xml.calc.template');
    Add('.sxd=application/vnd.sun.xml.draw');
    Add('.std=application/vnd.sun.xml.draw.template');
    Add('.sxi=application/vnd.sun.xml.impress');
    Add('.sti=application/vnd.sun.xml.impress.template');
    Add('.sxm=application/vnd.sun.xml.math');
    Add('.sxw=application/vnd.sun.xml.writer');
    Add('.sxg=application/vnd.sun.xml.writer.global');
    Add('.stw=application/vnd.sun.xml.writer.template');
    Add('.sus=application/vnd.sus-calendar');
    Add('.susp=application/vnd.sus-calendar');
    Add('.svd=application/vnd.svd');
    Add('.sis=application/vnd.symbian.install');
    Add('.sisx=application/vnd.symbian.install');
    Add('.xsm=application/vnd.syncml+xml');
    Add('.bdm=application/vnd.syncml.dm+wbxml');
    Add('.xdm=application/vnd.syncml.dm+xml');
    Add('.tao=application/vnd.tao.intent-module-archive');
    Add('.pcap=application/vnd.tcpdump.pcap');
    Add('.cap=application/vnd.tcpdump.pcap');
    Add('.dmp=application/vnd.tcpdump.pcap');
    Add('.tmo=application/vnd.tmobile-livetv');
    Add('.tpt=application/vnd.trid.tpt');
    Add('.mxs=application/vnd.triscape.mxs');
    Add('.tra=application/vnd.trueapp');
    Add('.ufd=application/vnd.ufdl');
    Add('.ufdl=application/vnd.ufdl');
    Add('.utz=application/vnd.uiq.theme');
    Add('.umj=application/vnd.umajin');
    Add('.unityweb=application/vnd.unity');
    Add('.uoml=application/vnd.uoml+xml');
    Add('.vcx=application/vnd.vcx');
    Add('.vsd=application/vnd.visio');
    Add('.vst=application/vnd.visio');
    Add('.vss=application/vnd.visio');
    Add('.vsw=application/vnd.visio');
    Add('.vis=application/vnd.visionary');
    Add('.vsf=application/vnd.vsf');
    Add('.wbxml=application/vnd.wap.wbxml');
    Add('.wmlc=application/vnd.wap.wmlc');
    Add('.wmlsc=application/vnd.wap.wmlscriptc');
    Add('.wtb=application/vnd.webturbo');
    Add('.nbp=application/vnd.wolfram.player');
    Add('.wpd=application/vnd.wordperfect');
    Add('.wqd=application/vnd.wqd');
    Add('.stf=application/vnd.wt.stf');
    Add('.xar=application/vnd.xara');
    Add('.xfdl=application/vnd.xfdl');
    Add('.hvd=application/vnd.yamaha.hv-dic');
    Add('.hvs=application/vnd.yamaha.hv-script');
    Add('.hvp=application/vnd.yamaha.hv-voice');
    Add('.osf=application/vnd.yamaha.openscoreformat');
    Add('.osfpvg=application/vnd.yamaha.openscoreformat.osfpvg+xml');
    Add('.saf=application/vnd.yamaha.smaf-audio');
    Add('.spf=application/vnd.yamaha.smaf-phrase');
    Add('.cmp=application/vnd.yellowriver-custom-menu');
    Add('.zir=application/vnd.zul');
    Add('.zirz=application/vnd.zul');
    Add('.zaz=application/vnd.zzazz.deck+xml');
    Add('.vxml=application/voicexml+xml');
    Add('.wgt=application/widget');
    Add('.hlp=application/winhlp');
    Add('.wsdl=application/wsdl+xml');
    Add('.wspolicy=application/wspolicy+xml');
    Add('.7z=application/x-7z-compressed');
    Add('.abw=application/x-abiword');
    Add('.ace=application/x-ace-compressed');
    Add('.dmg=application/x-apple-diskimage');
    Add('.aab=application/x-authorware-bin');
    Add('.x32=application/x-authorware-bin');
    Add('.u32=application/x-authorware-bin');
    Add('.vox=application/x-authorware-bin');
    Add('.aam=application/x-authorware-map');
    Add('.aas=application/x-authorware-seg');
    Add('.bcpio=application/x-bcpio');
    Add('.torrent=application/x-bittorrent');
    Add('.blb=application/x-blorb');
    Add('.blorb=application/x-blorb');
    Add('.bz=application/x-bzip');
    Add('.bz2=application/x-bzip2');
    Add('.boz=application/x-bzip2');
    Add('.cbr=application/x-cbr');
    Add('.cba=application/x-cbr');
    Add('.cbt=application/x-cbr');
    Add('.cbz=application/x-cbr');
    Add('.cb7=application/x-cbr');
    Add('.vcd=application/x-cdlink');
    Add('.cfs=application/x-cfs-compressed');
    Add('.chat=application/x-chat');
    Add('.pgn=application/x-chess-pgn');
    Add('.nsc=application/x-conference');
    Add('.cpio=application/x-cpio');
    Add('.csh=application/x-csh');
    Add('.deb=application/x-debian-package');
    Add('.udeb=application/x-debian-package');
    Add('.dgc=application/x-dgc-compressed');
    Add('.dir=application/x-director');
    Add('.dcr=application/x-director');
    Add('.dxr=application/x-director');
    Add('.cst=application/x-director');
    Add('.cct=application/x-director');
    Add('.cxt=application/x-director');
    Add('.w3d=application/x-director');
    Add('.fgd=application/x-director');
    Add('.swa=application/x-director');
    Add('.wad=application/x-doom');
    Add('.ncx=application/x-dtbncx+xml');
    Add('.dtb=application/x-dtbook+xml');
    Add('.res=application/x-dtbresource+xml');
    Add('.dvi=application/x-dvi');
    Add('.evy=application/x-envoy');
    Add('.eva=application/x-eva');
    Add('.bdf=application/x-font-bdf');
    Add('.gsf=application/x-font-ghostscript');
    Add('.psf=application/x-font-linux-psf');
    Add('.otf=application/x-font-otf');
    Add('.pcf=application/x-font-pcf');
    Add('.snf=application/x-font-snf');
    Add('.ttf=application/x-font-ttf');
    Add('.ttc=application/x-font-ttf');
    Add('.pfa=application/x-font-type1');
    Add('.pfb=application/x-font-type1');
    Add('.pfm=application/x-font-type1');
    Add('.afm=application/x-font-type1');
    Add('.woff=application/x-font-woff');
    Add('.arc=application/x-freearc');
    Add('.spl=application/x-futuresplash');
    Add('.gca=application/x-gca-compressed');
    Add('.ulx=application/x-glulx');
    Add('.gnumeric=application/x-gnumeric');
    Add('.gramps=application/x-gramps-xml');
    Add('.gtar=application/x-gtar');
    Add('.hdf=application/x-hdf');
    Add('.install=application/x-install-instructions');
    Add('.iso=application/x-iso9660-image');
    Add('.jnlp=application/x-java-jnlp-file');
    Add('.latex=application/x-latex');
    Add('.lzh=application/x-lzh-compressed');
    Add('.lha=application/x-lzh-compressed');
    Add('.mie=application/x-mie');
    Add('.prc=application/x-mobipocket-ebook');
    Add('.mobi=application/x-mobipocket-ebook');
    Add('.application=application/x-ms-application');
    Add('.lnk=application/x-ms-shortcut');
    Add('.wmd=application/x-ms-wmd');
    Add('.wmz=application/x-ms-wmz');
    Add('.xbap=application/x-ms-xbap');
    Add('.mdb=application/x-msaccess');
    Add('.obd=application/x-msbinder');
    Add('.crd=application/x-mscardfile');
    Add('.clp=application/x-msclip');
    Add('.exe=application/x-msdownload');
    Add('.dll=application/x-msdownload');
    Add('.com=application/x-msdownload');
    Add('.bat=application/x-msdownload');
    Add('.msi=application/x-msdownload');
    Add('.mvb=application/x-msmediaview');
    Add('.m13=application/x-msmediaview');
    Add('.m14=application/x-msmediaview');
    Add('.wmf=application/x-msmetafile');
    Add('.wmz=application/x-msmetafile');
    Add('.emf=application/x-msmetafile');
    Add('.emz=application/x-msmetafile');
    Add('.mny=application/x-msmoney');
    Add('.pub=application/x-mspublisher');
    Add('.scd=application/x-msschedule');
    Add('.trm=application/x-msterminal');
    Add('.wri=application/x-mswrite');
    Add('.nc=application/x-netcdf');
    Add('.cdf=application/x-netcdf');
    Add('.nzb=application/x-nzb');
    Add('.p12=application/x-pkcs12');
    Add('.pfx=application/x-pkcs12');
    Add('.p7b=application/x-pkcs7-certificates');
    Add('.spc=application/x-pkcs7-certificates');
    Add('.p7r=application/x-pkcs7-certreqresp');
    Add('.rar=application/x-rar-compressed');
    Add('.ris=application/x-research-info-systems');
    Add('.sh=application/x-sh');
    Add('.shar=application/x-shar');
    Add('.swf=application/x-shockwave-flash');
    Add('.xap=application/x-silverlight-app');
    Add('.sql=application/x-sql');
    Add('.sit=application/x-stuffit');
    Add('.sitx=application/x-stuffitx');
    Add('.srt=application/x-subrip');
    Add('.sv4cpio=application/x-sv4cpio');
    Add('.sv4crc=application/x-sv4crc');
    Add('.t3=application/x-t3vm-image');
    Add('.gam=application/x-tads');
    Add('.tar=application/x-tar');
    Add('.tcl=application/x-tcl');
    Add('.tex=application/x-tex');
    Add('.tfm=application/x-tex-tfm');
    Add('.texinfo=application/x-texinfo');
    Add('.texi=application/x-texinfo');
    Add('.obj=application/x-tgif');
    Add('.ustar=application/x-ustar');
    Add('.src=application/x-wais-source');
    Add('.der=application/x-x509-ca-cert');
    Add('.crt=application/x-x509-ca-cert');
    Add('.fig=application/x-xfig');
    Add('.xlf=application/x-xliff+xml');
    Add('.xpi=application/x-xpinstall');
    Add('.xz=application/x-xz');
    Add('.yaml=application/x-yaml');
    Add('.z1=application/x-zmachine');
    Add('.z2=application/x-zmachine');
    Add('.z3=application/x-zmachine');
    Add('.z4=application/x-zmachine');
    Add('.z5=application/x-zmachine');
    Add('.z6=application/x-zmachine');
    Add('.z7=application/x-zmachine');
    Add('.z8=application/x-zmachine');
    Add('.xaml=application/xaml+xml');
    Add('.xdf=application/xcap-diff+xml');
    Add('.xenc=application/xenc+xml');
    Add('.xhtml=application/xhtml+xml');
    Add('.xht=application/xhtml+xml');
    Add('.xml=application/xml');
    Add('.xsl=application/xml');
    Add('.dtd=application/xml-dtd');
    Add('.xop=application/xop+xml');
    Add('.xpl=application/xproc+xml');
    Add('.xslt=application/xslt+xml');
    Add('.xspf=application/xspf+xml');
    Add('.mxml=application/xv+xml');
    Add('.xhvml=application/xv+xml');
    Add('.xvml=application/xv+xml');
    Add('.xvm=application/xv+xml');
    Add('.yang=application/yang');
    Add('.yin=application/yin+xml');
    Add('.zip=application/zip');
    Add('.adp=audio/adpcm');
    Add('.au=audio/basic');
    Add('.snd=audio/basic');
    Add('.mid=audio/midi');
    Add('.midi=audio/midi');
    Add('.kar=audio/midi');
    Add('.rmi=audio/midi');
    Add('.mp4a=audio/mp4');
    Add('.mpga=audio/mpeg');
    Add('.mp2=audio/mpeg');
    Add('.mp2a=audio/mpeg');
    Add('.mp3=audio/mpeg');
    Add('.m2a=audio/mpeg');
    Add('.m3a=audio/mpeg');
    Add('.oga=audio/ogg');
    Add('.ogg=audio/ogg');
    Add('.spx=audio/ogg');
    Add('.s3m=audio/s3m');
    Add('.sil=audio/silk');
    Add('.uva=audio/vnd.dece.audio');
    Add('.uvva=audio/vnd.dece.audio');
    Add('.eol=audio/vnd.digital-winds');
    Add('.dra=audio/vnd.dra');
    Add('.dts=audio/vnd.dts');
    Add('.dtshd=audio/vnd.dts.hd');
    Add('.lvp=audio/vnd.lucent.voice');
    Add('.pya=audio/vnd.ms-playready.media.pya');
    Add('.ecelp4800=audio/vnd.nuera.ecelp4800');
    Add('.ecelp7470=audio/vnd.nuera.ecelp7470');
    Add('.ecelp9600=audio/vnd.nuera.ecelp9600');
    Add('.rip=audio/vnd.rip');
    Add('.weba=audio/webm');
    Add('.aac=audio/x-aac');
    Add('.aif=audio/x-aiff');
    Add('.aiff=audio/x-aiff');
    Add('.aifc=audio/x-aiff');
    Add('.caf=audio/x-caf');
    Add('.flac=audio/x-flac');
    Add('.mka=audio/x-matroska');
    Add('.m3u=audio/x-mpegurl');
    Add('.wax=audio/x-ms-wax');
    Add('.wma=audio/x-ms-wma');
    Add('.ram=audio/x-pn-realaudio');
    Add('.ra=audio/x-pn-realaudio');
    Add('.rmp=audio/x-pn-realaudio-plugin');
    Add('.wav=audio/x-wav');
    Add('.xm=audio/xm');
    Add('.cdx=chemical/x-cdx');
    Add('.cif=chemical/x-cif');
    Add('.cmdf=chemical/x-cmdf');
    Add('.cml=chemical/x-cml');
    Add('.csml=chemical/x-csml');
    Add('.xyz=chemical/x-xyz');
    Add('.bmp=image/bmp');
    Add('.cgm=image/cgm');
    Add('.g3=image/g3fax');
    Add('.gif=image/gif');
    Add('.ief=image/ief');
    Add('.jpeg=image/jpeg');
    Add('.jpg=image/jpeg');
    Add('.jpe=image/jpeg');
    Add('.ktx=image/ktx');
    Add('.png=image/png');
    Add('.btif=image/prs.btif');
    Add('.sgi=image/sgi');
    Add('.svg=image/svg+xml');
    Add('.svgz=image/svg+xml');
    Add('.tiff=image/tiff');
    Add('.tif=image/tiff');
    Add('.psd=image/vnd.adobe.photoshop');
    Add('.uvi=image/vnd.dece.graphic');
    Add('.uvvi=image/vnd.dece.graphic');
    Add('.uvg=image/vnd.dece.graphic');
    Add('.uvvg=image/vnd.dece.graphic');
    Add('.sub=image/vnd.dvb.subtitle');
    Add('.djvu=image/vnd.djvu');
    Add('.djv=image/vnd.djvu');
    Add('.dwg=image/vnd.dwg');
    Add('.dxf=image/vnd.dxf');
    Add('.fbs=image/vnd.fastbidsheet');
    Add('.fpx=image/vnd.fpx');
    Add('.fst=image/vnd.fst');
    Add('.mmr=image/vnd.fujixerox.edmics-mmr');
    Add('.rlc=image/vnd.fujixerox.edmics-rlc');
    Add('.mdi=image/vnd.ms-modi');
    Add('.wdp=image/vnd.ms-photo');
    Add('.npx=image/vnd.net-fpx');
    Add('.wbmp=image/vnd.wap.wbmp');
    Add('.xif=image/vnd.xiff');
    Add('.webp=image/webp');
    Add('.3ds=image/x-3ds');
    Add('.ras=image/x-cmu-raster');
    Add('.cmx=image/x-cmx');
    Add('.fh=image/x-freehand');
    Add('.fhc=image/x-freehand');
    Add('.fh4=image/x-freehand');
    Add('.fh5=image/x-freehand');
    Add('.fh7=image/x-freehand');
    Add('.ico=image/x-icon');
    Add('.sid=image/x-mrsid-image');
    Add('.pcx=image/x-pcx');
    Add('.pic=image/x-pict');
    Add('.pct=image/x-pict');
    Add('.pnm=image/x-portable-anymap');
    Add('.pbm=image/x-portable-bitmap');
    Add('.pgm=image/x-portable-graymap');
    Add('.ppm=image/x-portable-pixmap');
    Add('.rgb=image/x-rgb');
    Add('.tga=image/x-tga');
    Add('.xbm=image/x-xbitmap');
    Add('.xpm=image/x-xpixmap');
    Add('.xwd=image/x-xwindowdump');
    Add('.eml=message/rfc822');
    Add('.mime=message/rfc822');
    Add('.igs=model/iges');
    Add('.iges=model/iges');
    Add('.msh=model/mesh');
    Add('.mesh=model/mesh');
    Add('.silo=model/mesh');
    Add('.dae=model/vnd.collada+xml');
    Add('.dwf=model/vnd.dwf');
    Add('.gdl=model/vnd.gdl');
    Add('.gtw=model/vnd.gtw');
    Add('.mts=model/vnd.mts');
    Add('.vtu=model/vnd.vtu');
    Add('.wrl=model/vrml');
    Add('.vrml=model/vrml');
    Add('.x3db=model/x3d+binary');
    Add('.x3dbz=model/x3d+binary');
    Add('.x3dv=model/x3d+vrml');
    Add('.x3dvz=model/x3d+vrml');
    Add('.x3d=model/x3d+xml');
    Add('.x3dz=model/x3d+xml');
    Add('.appcache=text/cache-manifest');
    Add('.manifest=text/cache-manifest');
    Add('.ics=text/calendar');
    Add('.ifb=text/calendar');
    Add('.cmd=text/cmd');
    Add('.css=text/css');
    Add('.csv=text/csv');
    Add('.html=text/html');
    Add('.htm=text/html');
    Add('.n3=text/n3');
    Add('.txt=text/plain');
    Add('.text=text/plain');
    Add('.conf=text/plain');
    Add('.def=text/plain');
    Add('.list=text/plain');
    Add('.log=text/plain');
    Add('.in=text/plain');
    Add('.dsc=text/prs.lines.tag');
    Add('.rtx=text/richtext');
    Add('.sgml=text/sgml');
    Add('.sgm=text/sgml');
    Add('.tsv=text/tab-separated-values');
    Add('.t=text/troff');
    Add('.tr=text/troff');
    Add('.roff=text/troff');
    Add('.man=text/troff');
    Add('.me=text/troff');
    Add('.ms=text/troff');
    Add('.ttl=text/turtle');
    Add('.uri=text/uri-list');
    Add('.uris=text/uri-list');
    Add('.urls=text/uri-list');
    Add('.vcard=text/vcard');
    Add('.curl=text/vnd.curl');
    Add('.dcurl=text/vnd.curl.dcurl');
    Add('.scurl=text/vnd.curl.scurl');
    Add('.mcurl=text/vnd.curl.mcurl');
    Add('.sub=text/vnd.dvb.subtitle');
    Add('.fly=text/vnd.fly');
    Add('.flx=text/vnd.fmi.flexstor');
    Add('.gv=text/vnd.graphviz');
    Add('.3dml=text/vnd.in3d.3dml');
    Add('.spot=text/vnd.in3d.spot');
    Add('.jad=text/vnd.sun.j2me.app-descriptor');
    Add('.wml=text/vnd.wap.wml');
    Add('.wmls=text/vnd.wap.wmlscript');
    Add('.s=text/x-asm');
    Add('.asm=text/x-asm');
    Add('.c=text/x-c');
    Add('.cc=text/x-c');
    Add('.cxx=text/x-c');
    Add('.cpp=text/x-c');
    Add('.h=text/x-c');
    Add('.hh=text/x-c');
    Add('.dic=text/x-c');
    Add('.f=text/x-fortran');
    Add('.for=text/x-fortran');
    Add('.f77=text/x-fortran');
    Add('.f90=text/x-fortran');
    Add('.java=text/x-java-source');
    Add('.opml=text/x-opml');
    Add('.p=text/x-pascal');
    Add('.pas=text/x-pascal');
    Add('.nfo=text/x-nfo');
    Add('.etx=text/x-setext');
    Add('.sfv=text/x-sfv');
    Add('.uu=text/x-uuencode');
    Add('.vcs=text/x-vcalendar');
    Add('.vcf=text/x-vcard');
    Add('.vcf=text/x-yaml');
    Add('.xml=text/xml');
    Add('.xsl=text/xml');
    Add('.dtd=text/xml-dtd');
    Add('.yaml=text/yaml');
    Add('.3gp=video/3gpp');
    Add('.3g2=video/3gpp2');
    Add('.h261=video/h261');
    Add('.h263=video/h263');
    Add('.h264=video/h264');
    Add('.jpgv=video/jpeg');
    Add('.jpm=video/jpm');
    Add('.jpgm=video/jpm');
    Add('.mj2=video/mj2');
    Add('.mjp2=video/mj2');
    Add('.mp4=video/mp4');
    Add('.mp4v=video/mp4');
    Add('.mpg4=video/mp4');
    Add('.mpeg=video/mpeg');
    Add('.mpg=video/mpeg');
    Add('.mpe=video/mpeg');
    Add('.m1v=video/mpeg');
    Add('.m2v=video/mpeg');
    Add('.ogv=video/ogg');
    Add('.qt=video/quicktime');
    Add('.mov=video/quicktime');
    Add('.uvh=video/vnd.dece.hd');
    Add('.uvvh=video/vnd.dece.hd');
    Add('.uvm=video/vnd.dece.mobile');
    Add('.uvvm=video/vnd.dece.mobile');
    Add('.uvp=video/vnd.dece.pd');
    Add('.uvvp=video/vnd.dece.pd');
    Add('.uvs=video/vnd.dece.sd');
    Add('.uvvs=video/vnd.dece.sd');
    Add('.uvv=video/vnd.dece.video');
    Add('.uvvv=video/vnd.dece.video');
    Add('.dvb=video/vnd.dvb.file');
    Add('.fvt=video/vnd.fvt');
    Add('.mxu=video/vnd.mpegurl');
    Add('.m4u=video/vnd.mpegurl');
    Add('.pyv=video/vnd.ms-playready.media.pyv');
    Add('.uvu=video/vnd.uvvu.mp4');
    Add('.uvvu=video/vnd.uvvu.mp4');
    Add('.viv=video/vnd.vivo');
    Add('.webm=video/webm');
    Add('.f4v=video/x-f4v');
    Add('.fli=video/x-fli');
    Add('.flv=video/x-flv');
    Add('.m4v=video/x-m4v');
    Add('.mkv=video/x-matroska');
    Add('.mk3d=video/x-matroska');
    Add('.mks=video/x-matroska');
    Add('.mng=video/x-mng');
    Add('.asf=video/x-ms-asf');
    Add('.asx=video/x-ms-asf');
    Add('.vob=video/x-ms-vob');
    Add('.wm=video/x-ms-wm');
    Add('.wmv=video/x-ms-wmv');
    Add('.wmx=video/x-ms-wmx');
    Add('.wvx=video/x-ms-wvx');
    Add('.avi=video/x-msvideo');
    Add('.movie=video/x-sgi-movie');
    Add('.smv=video/x-smv');
    Add('.ice=x-conference/x-cooltalk');
    {$ENDREGION}
  end;
end;

end.
