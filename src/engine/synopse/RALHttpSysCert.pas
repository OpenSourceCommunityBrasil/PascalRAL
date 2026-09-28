/// The certificate of an http.sys port: which one is bound, and binding
/// another one (Windows only)
unit RALHttpSysCert;

{$I ..\..\base\PascalRAL.inc}

{ http.sys does not read a certificate file: it serves the certificate that
  "netsh http add sslcert" bound to the IP and port, from a store of the
  machine. This unit does what that command does, so that TRALSynopseServer
  in smHttpSys can take a certificate like the other modes:
  - HttpSysBindCertificate imports a PKCS#12 into LocalMachine\My (the key in
    the machine key set, where LSASS - which runs the TLS of http.sys - can use
    it) and binds it to 0.0.0.0:port and [::]:port, replacing what was there;
  - HttpSysBoundCertificate reads which certificate is bound, so a provider
    can tell a certificate from a CA apart from a self-signed one.
  Writing to the machine store and to the http.sys configuration needs
  administrator rights, exactly like netsh; reading does not. A binding takes
  effect on the next TLS handshake, with the server running.

  Declared here, and not taken from mORMot2, because mormot.lib.winhttp has no
  query of the configuration and no machine store. }

interface

{$IFDEF RALWindows}

uses
  Windows, SysUtils,
  RALTypes, RALConsts;

/// The DER of the certificate bound to 0.0.0.0:APort, or to [::]:APort when
/// the IPv4 one is not bound; empty when neither is, or the certificate is
/// not in the store the binding names
function HttpSysBoundCertificate(APort: Word): TBytes;
/// The SHA-1 (thumbprint) bound to 0.0.0.0:APort or [::]:APort; empty when none
function HttpSysBoundThumbprint(APort: Word): TBytes;
/// Imports APfx into LocalMachine\My and binds it to 0.0.0.0:APort and
/// [::]:APort. Raises, saying what failed, when Windows refuses (not
/// administrator, wrong password)
procedure HttpSysBindCertificate(APort: Word; const APfx: TBytes;
  const APassword: StringRAL; const AThumbprint: TBytes);
/// Removes the bindings of 0.0.0.0:APort and [::]:APort - what "netsh http
/// delete sslcert" does. Nothing bound is not an error. Needs administrator
/// rights
procedure HttpSysUnbindCertificate(APort: Word);
/// The DER of the certificate that carries the key in a PKCS#12, read by
/// Windows (so every cipher Windows reads is read); nothing is kept in any
/// store or key container. Empty when the file does not open with APassword
function WinPfxCertificate(const APfx: TBytes; const APassword: StringRAL): TBytes;
/// Deletes from LocalMachine\My the certificate with this thumbprint, and the
/// key its import left in the machine key set, when its friendly name starts
/// with AFriendlyNamePrefix - only certificates this process put there, never
/// somebody else's. False when it did nothing
function HttpSysRemoveCertificate(const AThumbprint: TBytes;
  const AFriendlyNamePrefix: string): boolean;

{$ENDIF}

implementation

{$IFDEF RALWindows}

const
  cHttpApi = 'httpapi.dll';
  cCrypt32 = 'crypt32.dll';

  HTTP_INITIALIZE_CONFIG = $00000002;
  HttpServiceConfigSSLCertInfo = 1;
  HttpServiceConfigQueryExact = 0;

  CERT_STORE_PROV_SYSTEM_W = PAnsiChar(10);
  CERT_SYSTEM_STORE_LOCAL_MACHINE = $00020000;
  CERT_STORE_OPEN_EXISTING_FLAG = $00004000;
  CERT_STORE_READONLY_FLAG = $00008000;
  CRYPT_MACHINE_KEYSET = $00000020;
  PKCS12_INCLUDE_EXTENDED_PROPERTIES = $00000010;
  X509_ASN_ENCODING = $00000001;
  PKCS_7_ASN_ENCODING = $00010000;
  CERT_FIND_SHA1_HASH = $00010000;
  CERT_STORE_ADD_REPLACE_EXISTING = 3;
  CERT_KEY_PROV_INFO_PROP_ID = 2;
  PKCS12_NO_PERSIST_KEY = $00008000;
  CERT_FRIENDLY_NAME_PROP_ID = 11;
  CRYPT_DELETEKEYSET = $00000010;
  CRYPT_SILENT = $00000040;
  NCRYPT_MACHINE_KEY_FLAG = $00000020;
  NCRYPT_SILENT_FLAG = $00000040;

  { the application of the bindings RAL makes; netsh shows it as appid }
  cRALAppId: TGUID = '{68DF0CEA-997C-49FF-AA33-56E8BB609C8F}';

type
  THttpApiVersion = record
    Major: Word;
    Minor: Word;
  end;

  THttpServiceConfigSslKey = record
    pIpPort: Pointer;
  end;

  THttpServiceConfigSslParam = record
    SslHashLength: Cardinal;
    pSslHash: Pointer;
    AppId: TGUID;
    pSslCertStoreName: PWideChar;
    DefaultCertCheckMode: Cardinal;
    DefaultRevocationFreshnessTime: Cardinal;
    DefaultRevocationUrlRetrievalTimeout: Cardinal;
    pDefaultSslCtlIdentifier: PWideChar;
    pDefaultSslCtlStoreName: PWideChar;
    DefaultFlags: Cardinal;
  end;

  THttpServiceConfigSslSet = record
    KeyDesc: THttpServiceConfigSslKey;
    ParamDesc: THttpServiceConfigSslParam;
  end;
  PHttpServiceConfigSslSet = ^THttpServiceConfigSslSet;

  THttpServiceConfigSslQuery = record
    QueryDesc: Integer;
    KeyDesc: THttpServiceConfigSslKey;
    dwToken: Cardinal;
  end;

  HCERTSTORE = Pointer;

  TCertContext = record
    dwCertEncodingType: Cardinal;
    pbCertEncoded: PByte;
    cbCertEncoded: Cardinal;
    pCertInfo: Pointer;
    hCertStore: HCERTSTORE;
  end;
  PCCERT_CONTEXT = ^TCertContext;

  TCryptDataBlob = record
    cbData: Cardinal;
    pbData: PByte;
  end;
  PCryptDataBlob = ^TCryptDataBlob;

  { sockaddr_in6 is the larger of the two: 28 bytes }
  TSockAddrBuf = array[0..27] of Byte;

  { CERT_KEY_PROV_INFO_PROP_ID: where the key of a certificate lives }
  TCryptKeyProvInfo = record
    pwszContainerName: PWideChar;
    pwszProvName: PWideChar;
    dwProvType: Cardinal;
    dwFlags: Cardinal;
    cProvParam: Cardinal;
    rgProvParam: Pointer;
    dwKeySpec: Cardinal;
  end;
  PCryptKeyProvInfo = ^TCryptKeyProvInfo;

function HttpInitialize(Version: THttpApiVersion; Flags: Cardinal;
  pReserved: Pointer): Cardinal; stdcall; external cHttpApi;
function HttpTerminate(Flags: Cardinal; pReserved: Pointer): Cardinal; stdcall;
  external cHttpApi;
function HttpSetServiceConfiguration(ServiceHandle: THandle; ConfigId: Integer;
  pConfigInformation: Pointer; ConfigInformationLength: Cardinal;
  pOverlapped: Pointer): Cardinal; stdcall; external cHttpApi;
function HttpDeleteServiceConfiguration(ServiceHandle: THandle; ConfigId: Integer;
  pConfigInformation: Pointer; ConfigInformationLength: Cardinal;
  pOverlapped: Pointer): Cardinal; stdcall; external cHttpApi;
function HttpQueryServiceConfiguration(ServiceHandle: THandle; ConfigId: Integer;
  pInput: Pointer; InputLength: Cardinal; pOutput: Pointer; OutputLength: Cardinal;
  pReturnLength: PCardinal; pOverlapped: Pointer): Cardinal; stdcall;
  external cHttpApi;

function PFXImportCertStore(pPFX: PCryptDataBlob; szPassword: PWideChar;
  dwFlags: Cardinal): HCERTSTORE; stdcall; external cCrypt32;
function CertOpenStore(lpszStoreProvider: PAnsiChar; dwEncodingType: Cardinal;
  hCryptProv: NativeUInt; dwFlags: Cardinal; pvPara: Pointer): HCERTSTORE;
  stdcall; external cCrypt32;
function CertCloseStore(hCertStore: HCERTSTORE; dwFlags: Cardinal): BOOL;
  stdcall; external cCrypt32;
function CertEnumCertificatesInStore(hCertStore: HCERTSTORE;
  pPrevCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT; stdcall; external cCrypt32;
function CertFindCertificateInStore(hCertStore: HCERTSTORE;
  dwCertEncodingType, dwFindFlags, dwFindType: Cardinal; pvFindPara: Pointer;
  pPrevCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT; stdcall; external cCrypt32;
function CertAddCertificateContextToStore(hCertStore: HCERTSTORE;
  pCertContext: PCCERT_CONTEXT; dwAddDisposition: Cardinal;
  ppStoreContext: Pointer): BOOL; stdcall; external cCrypt32;
function CertDeleteCertificateFromStore(pCertContext: PCCERT_CONTEXT): BOOL;
  stdcall; external cCrypt32;
function CertFreeCertificateContext(pCertContext: PCCERT_CONTEXT): BOOL;
  stdcall; external cCrypt32;
function CertGetCertificateContextProperty(pCertContext: PCCERT_CONTEXT;
  dwPropId: Cardinal; pvData: Pointer; pcbData: PCardinal): BOOL; stdcall;
  external cCrypt32;

function RALCryptAcquireContextW(var phProv: NativeUInt; pszContainer,
  pszProvider: PWideChar; dwProvType, dwFlags: Cardinal): BOOL; stdcall;
  external 'advapi32.dll' name 'CryptAcquireContextW';
function NCryptOpenStorageProvider(var phProvider: NativeUInt;
  pszProviderName: PWideChar; dwFlags: Cardinal): Integer; stdcall;
  external 'ncrypt.dll';
function NCryptOpenKey(hProvider: NativeUInt; var phKey: NativeUInt;
  pszKeyName: PWideChar; dwLegacyKeySpec, dwFlags: Cardinal): Integer; stdcall;
  external 'ncrypt.dll';
function NCryptDeleteKey(hKey: NativeUInt; dwFlags: Cardinal): Integer; stdcall;
  external 'ncrypt.dll';
function NCryptFreeObject(hObject: NativeUInt): Integer; stdcall;
  external 'ncrypt.dll';

procedure CheckHttpApi(AResult: Cardinal; const AWhat: string);
begin
  if AResult <> NO_ERROR then
    raise Exception.CreateFmt(emHttpSysCallFailed, [AWhat, AResult,
      SysErrorMessage(AResult)]);
end;

{ 0.0.0.0:APort (AIPv6 False) or [::]:APort }
procedure FillAddress(out AAddr: TSockAddrBuf; APort: Word; AIPv6: boolean);
begin
  FillChar(AAddr, SizeOf(AAddr), 0);
  if AIPv6 then
    AAddr[0] := 23 // AF_INET6
  else
    AAddr[0] := 2; // AF_INET
  AAddr[2] := Hi(APort);
  AAddr[3] := Lo(APort);
end;

function HttpVersion1: THttpApiVersion;
begin
  Result.Major := 1;
  Result.Minor := 0;
end;

{ the hash and the store bound to the address; False when nothing is }
function QueryBinding(APort: Word; AIPv6: boolean; out AHash: TBytes;
  out AStore: string): boolean;
var
  vAddr: TSockAddrBuf;
  vQuery: THttpServiceConfigSslQuery;
  vBuffer: array of Byte;
  vSize: Cardinal;
  vRes: Cardinal;
  vSet: PHttpServiceConfigSslSet;
begin
  Result := False;
  AHash := nil;
  AStore := '';
  FillAddress(vAddr, APort, AIPv6);
  FillChar(vQuery, SizeOf(vQuery), 0);
  vQuery.QueryDesc := HttpServiceConfigQueryExact;
  vQuery.KeyDesc.pIpPort := @vAddr;

  vSize := 4096;
  SetLength(vBuffer, vSize);
  vRes := HttpQueryServiceConfiguration(0, HttpServiceConfigSSLCertInfo, @vQuery,
    SizeOf(vQuery), @vBuffer[0], vSize, @vSize, nil);
  if vRes = ERROR_INSUFFICIENT_BUFFER then
  begin
    SetLength(vBuffer, vSize);
    vRes := HttpQueryServiceConfiguration(0, HttpServiceConfigSSLCertInfo, @vQuery,
      SizeOf(vQuery), @vBuffer[0], vSize, @vSize, nil);
  end;
  if vRes <> NO_ERROR then
    Exit;

  vSet := PHttpServiceConfigSslSet(@vBuffer[0]);
  SetLength(AHash, vSet^.ParamDesc.SslHashLength);
  if Length(AHash) > 0 then
    Move(vSet^.ParamDesc.pSslHash^, AHash[0], Length(AHash));
  if vSet^.ParamDesc.pSslCertStoreName <> nil then
    AStore := string(WideString(vSet^.ParamDesc.pSslCertStoreName));
  if AStore = '' then
    AStore := 'MY';
  Result := Length(AHash) > 0;
end;

function OpenMachineStore(const AName: string; AReadOnly: boolean): HCERTSTORE;
var
  vName: WideString;
  vFlags: Cardinal;
begin
  vName := WideString(AName);
  vFlags := CERT_SYSTEM_STORE_LOCAL_MACHINE or CERT_STORE_OPEN_EXISTING_FLAG;
  if AReadOnly then
    vFlags := vFlags or CERT_STORE_READONLY_FLAG;
  Result := CertOpenStore(CERT_STORE_PROV_SYSTEM_W, 0, 0, vFlags, PWideChar(vName));
end;

function FindByHash(AStore: HCERTSTORE; const AHash: TBytes): PCCERT_CONTEXT;
var
  vBlob: TCryptDataBlob;
begin
  Result := nil;
  if Length(AHash) = 0 then
    Exit;
  vBlob.cbData := Length(AHash);
  vBlob.pbData := @AHash[0];
  Result := CertFindCertificateInStore(AStore, X509_ASN_ENCODING or
    PKCS_7_ASN_ENCODING, 0, CERT_FIND_SHA1_HASH, @vBlob, nil);
end;

function HasKey(ACert: PCCERT_CONTEXT): boolean;
var
  vSize: Cardinal;
begin
  vSize := 0;
  Result := CertGetCertificateContextProperty(ACert, CERT_KEY_PROV_INFO_PROP_ID, nil,
    @vSize) and (vSize > 0);
end;

{ Taking a certificate out of a store leaves its key where the import put it -
  one file in MachineKeys for every certificate ever bound. This deletes the
  key: CryptAcquireContext(CRYPT_DELETEKEYSET) for a CSP key, NCryptDeleteKey
  for a CNG one (dwProvType 0) }
procedure DeleteCertificateKey(ACert: PCCERT_CONTEXT);
var
  vSize, vSpec, vFlags: Cardinal;
  vBuf: TBytes;
  vInfo: PCryptKeyProvInfo;
  vProv, vKey: NativeUInt;
begin
  vSize := 0;
  if not CertGetCertificateContextProperty(ACert, CERT_KEY_PROV_INFO_PROP_ID, nil,
    @vSize) or (vSize = 0) then
    Exit;
  SetLength(vBuf, vSize);
  if not CertGetCertificateContextProperty(ACert, CERT_KEY_PROV_INFO_PROP_ID,
    @vBuf[0], @vSize) then
    Exit;
  { the strings live inside the same buffer }
  vInfo := PCryptKeyProvInfo(@vBuf[0]);
  if vInfo^.pwszContainerName = nil then
    Exit;

  vProv := 0;
  if vInfo^.dwProvType <> 0 then
  begin
    { a deleted key set hands no handle back }
    RALCryptAcquireContextW(vProv, vInfo^.pwszContainerName, vInfo^.pwszProvName,
      vInfo^.dwProvType, CRYPT_DELETEKEYSET or CRYPT_SILENT or
      (vInfo^.dwFlags and CRYPT_MACHINE_KEYSET));
    Exit;
  end;

  if NCryptOpenStorageProvider(vProv, vInfo^.pwszProvName, 0) <> 0 then
    Exit;
  try
    { AT_KEYEXCHANGE or AT_SIGNATURE for a key that came through a CSP name;
      anything else (CERT_NCRYPT_KEY_SPEC) means none }
    vSpec := vInfo^.dwKeySpec;
    if vSpec > 2 then
      vSpec := 0;
    vFlags := NCRYPT_SILENT_FLAG or (vInfo^.dwFlags and NCRYPT_MACHINE_KEY_FLAG);
    vKey := 0;
    if (NCryptOpenKey(vProv, vKey, vInfo^.pwszContainerName, vSpec, vFlags) = 0) or
      ((vSpec <> 0) and
       (NCryptOpenKey(vProv, vKey, vInfo^.pwszContainerName, 0, vFlags) = 0)) then
      { frees the key handle as well }
      NCryptDeleteKey(vKey, 0);
  finally
    NCryptFreeObject(vProv);
  end;
end;

{ Deletes the certificate with this thumbprint from LocalMachine\My, and its
  key; an empty AFriendlyNamePrefix takes it whatever its name }
function RemoveFromMachineStore(const AThumbprint: TBytes;
  const AFriendlyNamePrefix: string): boolean;
var
  vMy: HCERTSTORE;
  vCert: PCCERT_CONTEXT;
  vSize: Cardinal;
  vName: WideString;
begin
  Result := False;
  vMy := OpenMachineStore('MY', False);
  if vMy = nil then
    Exit;
  try
    vCert := FindByHash(vMy, AThumbprint);
    if vCert = nil then
      Exit;

    vSize := 0;
    vName := '';
    if CertGetCertificateContextProperty(vCert, CERT_FRIENDLY_NAME_PROP_ID, nil,
      @vSize) and (vSize > 2) then
    begin
      SetLength(vName, vSize div 2 - 1);
      CertGetCertificateContextProperty(vCert, CERT_FRIENDLY_NAME_PROP_ID,
        PWideChar(vName), @vSize);
    end;

    if (AFriendlyNamePrefix = '') or
      (Copy(string(vName), 1, Length(AFriendlyNamePrefix)) = AFriendlyNamePrefix) then
    begin
      DeleteCertificateKey(vCert);
      { deletes and frees the context }
      Result := CertDeleteCertificateFromStore(vCert);
    end
    else
      CertFreeCertificateContext(vCert);
  finally
    CertCloseStore(vMy, 0);
  end;
end;

{ Imports APfx into AStore: the certificate that carries the key, the key in
  the machine key set }
procedure ImportIntoStore(AStore: HCERTSTORE; const APfx: TBytes;
  const APassword: StringRAL);
var
  vBlob: TCryptDataBlob;
  vPassword: WideString;
  vTemp: HCERTSTORE;
  vCert, vFound: PCCERT_CONTEXT;
begin
  { the key goes to the machine key set: http.sys runs TLS in LSASS, which
    does not see the keys of a user }
  vBlob.cbData := Length(APfx);
  vBlob.pbData := @APfx[0];
  vPassword := WideString(UnicodeString(APassword));
  vTemp := PFXImportCertStore(@vBlob, PWideChar(vPassword),
    CRYPT_MACHINE_KEYSET or PKCS12_INCLUDE_EXTENDED_PROPERTIES);
  if vTemp = nil then
    raise Exception.CreateFmt(emHttpSysImportFailed, [SysErrorMessage(GetLastError)]);
  try
    { the certificate that came with its key }
    vFound := nil;
    vCert := CertEnumCertificatesInStore(vTemp, nil);
    while vCert <> nil do
    begin
      if HasKey(vCert) then
      begin
        vFound := vCert;
        Break;
      end;
      vCert := CertEnumCertificatesInStore(vTemp, vCert);
    end;
    if vFound = nil then
      raise Exception.Create(emHttpSysNoKey);
    try
      if not CertAddCertificateContextToStore(AStore, vFound,
        CERT_STORE_ADD_REPLACE_EXISTING, nil) then
      begin
        { the import already persisted the key: it goes with the error }
        DeleteCertificateKey(vFound);
        raise Exception.CreateFmt(emHttpSysStoreFailed, [SysErrorMessage(GetLastError)]);
      end;
    finally
      CertFreeCertificateContext(vFound);
    end;
  finally
    CertCloseStore(vTemp, 0);
  end;
end;

function HttpSysBoundThumbprint(APort: Word): TBytes;
var
  vStore: string;
begin
  if HttpInitialize(HttpVersion1, HTTP_INITIALIZE_CONFIG, nil) <> NO_ERROR then
  begin
    Result := nil;
    Exit;
  end;
  try
    if not QueryBinding(APort, False, Result, vStore) then
      QueryBinding(APort, True, Result, vStore);
  finally
    HttpTerminate(HTTP_INITIALIZE_CONFIG, nil);
  end;
end;

function HttpSysBoundCertificate(APort: Word): TBytes;
var
  vHash: TBytes;
  vStoreName: string;
  vStore: HCERTSTORE;
  vCert: PCCERT_CONTEXT;
begin
  Result := nil;
  if HttpInitialize(HttpVersion1, HTTP_INITIALIZE_CONFIG, nil) <> NO_ERROR then
    Exit;
  try
    if not QueryBinding(APort, False, vHash, vStoreName) then
      if not QueryBinding(APort, True, vHash, vStoreName) then
        Exit;
  finally
    HttpTerminate(HTTP_INITIALIZE_CONFIG, nil);
  end;

  vStore := OpenMachineStore(vStoreName, True);
  if vStore = nil then
    Exit;
  try
    vCert := FindByHash(vStore, vHash);
    if vCert <> nil then
    begin
      SetLength(Result, vCert^.cbCertEncoded);
      Move(vCert^.pbCertEncoded^, Result[0], vCert^.cbCertEncoded);
      CertFreeCertificateContext(vCert);
    end;
  finally
    CertCloseStore(vStore, 0);
  end;
end;

procedure BindAddress(APort: Word; AIPv6: boolean; const AThumbprint: TBytes);
var
  vAddr: TSockAddrBuf;
  vSet: THttpServiceConfigSslSet;
  vStoreName: WideString;
  vRes: Cardinal;
begin
  FillAddress(vAddr, APort, AIPv6);
  FillChar(vSet, SizeOf(vSet), 0);
  vSet.KeyDesc.pIpPort := @vAddr;

  { what "netsh http delete sslcert" does; nothing there is not an error }
  vRes := HttpDeleteServiceConfiguration(0, HttpServiceConfigSSLCertInfo, @vSet,
    SizeOf(vSet), nil);
  if (vRes <> NO_ERROR) and (vRes <> ERROR_FILE_NOT_FOUND) then
    CheckHttpApi(vRes, 'HttpDeleteServiceConfiguration');

  vStoreName := 'MY';
  vSet.ParamDesc.SslHashLength := Length(AThumbprint);
  vSet.ParamDesc.pSslHash := @AThumbprint[0];
  vSet.ParamDesc.AppId := cRALAppId;
  vSet.ParamDesc.pSslCertStoreName := PWideChar(vStoreName);
  CheckHttpApi(HttpSetServiceConfiguration(0, HttpServiceConfigSSLCertInfo, @vSet,
    SizeOf(vSet), nil), 'HttpSetServiceConfiguration');
end;

procedure HttpSysBindCertificate(APort: Word; const APfx: TBytes;
  const APassword: StringRAL; const AThumbprint: TBytes);
var
  vMy: HCERTSTORE;
  vFound: PCCERT_CONTEXT;
  vImported: boolean;
begin
  if (Length(APfx) = 0) or (Length(AThumbprint) <> 20) then
    raise Exception.Create(emHttpSysNoCertificate);

  { the store first: without administrator rights it does not open for
    writing, and nothing may be imported before that is known - the import
    persists the key in the machine key set, and a key with no certificate in
    the store would be left there for nobody }
  vMy := OpenMachineStore('MY', False);
  if vMy = nil then
    raise Exception.CreateFmt(emHttpSysStoreFailed, [SysErrorMessage(GetLastError)]);
  try
    { already there with its key - the same certificate bound again after a
      restart, or by the next run of the application: importing it once more
      would put a new key in the machine key set and orphan the one before }
    vImported := True;
    vFound := FindByHash(vMy, AThumbprint);
    if vFound <> nil then
    begin
      vImported := not HasKey(vFound);
      CertFreeCertificateContext(vFound);
    end;
    if vImported then
      ImportIntoStore(vMy, APfx, APassword);
  finally
    CertCloseStore(vMy, 0);
  end;

  try
    CheckHttpApi(HttpInitialize(HttpVersion1, HTTP_INITIALIZE_CONFIG, nil),
      'HttpInitialize');
    try
      BindAddress(APort, False, AThumbprint);
      { IPv6 is best effort: a machine without it still serves on IPv4 }
      try
        BindAddress(APort, True, AThumbprint);
      except
      end;
    finally
      HttpTerminate(HTTP_INITIALIZE_CONFIG, nil);
    end;
  except
    { bound to nothing: what was imported for it leaves the store again }
    if vImported then
      RemoveFromMachineStore(AThumbprint, '');
    raise;
  end;
end;

procedure DeleteBinding(APort: Word; AIPv6: boolean);
var
  vAddr: TSockAddrBuf;
  vSet: THttpServiceConfigSslSet;
  vRes: Cardinal;
begin
  FillAddress(vAddr, APort, AIPv6);
  FillChar(vSet, SizeOf(vSet), 0);
  vSet.KeyDesc.pIpPort := @vAddr;
  vRes := HttpDeleteServiceConfiguration(0, HttpServiceConfigSSLCertInfo, @vSet,
    SizeOf(vSet), nil);
  if (vRes <> NO_ERROR) and (vRes <> ERROR_FILE_NOT_FOUND) then
    CheckHttpApi(vRes, 'HttpDeleteServiceConfiguration');
end;

procedure HttpSysUnbindCertificate(APort: Word);
begin
  CheckHttpApi(HttpInitialize(HttpVersion1, HTTP_INITIALIZE_CONFIG, nil),
    'HttpInitialize');
  try
    DeleteBinding(APort, False);
    try
      DeleteBinding(APort, True);
    except
    end;
  finally
    HttpTerminate(HTTP_INITIALIZE_CONFIG, nil);
  end;
end;

function WinPfxCertificate(const APfx: TBytes; const APassword: StringRAL): TBytes;
var
  vBlob: TCryptDataBlob;
  vPassword: WideString;
  vTemp: HCERTSTORE;
  vCert, vFirst: PCCERT_CONTEXT;
  vSize: Cardinal;
begin
  Result := nil;
  if Length(APfx) = 0 then
    Exit;
  vBlob.cbData := Length(APfx);
  vBlob.pbData := @APfx[0];
  vPassword := WideString(UnicodeString(APassword));
  vTemp := PFXImportCertStore(@vBlob, PWideChar(vPassword), PKCS12_NO_PERSIST_KEY);
  if vTemp = nil then
    Exit;
  try
    vFirst := nil;
    vCert := CertEnumCertificatesInStore(vTemp, nil);
    while vCert <> nil do
    begin
      vSize := 0;
      if CertGetCertificateContextProperty(vCert, CERT_KEY_PROV_INFO_PROP_ID, nil,
        @vSize) or (vFirst = nil) then
      begin
        SetLength(Result, vCert^.cbCertEncoded);
        Move(vCert^.pbCertEncoded^, Result[0], vCert^.cbCertEncoded);
        if vSize > 0 then
        begin
          CertFreeCertificateContext(vCert);
          Break;
        end;
        vFirst := vCert;
      end;
      vCert := CertEnumCertificatesInStore(vTemp, vCert);
    end;
  finally
    CertCloseStore(vTemp, 0);
  end;
end;

function HttpSysRemoveCertificate(const AThumbprint: TBytes;
  const AFriendlyNamePrefix: string): boolean;
begin
  { an empty prefix would take anybody's certificate }
  Result := (AFriendlyNamePrefix <> '') and
    RemoveFromMachineStore(AThumbprint, AFriendlyNamePrefix);
end;

{$ENDIF}

end.
