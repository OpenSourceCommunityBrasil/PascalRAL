/// A self-signed certificate made, attached and renewed by RAL: a plugin for a
/// server with no certificate from a CA, and a generator on its own
unit RALSelfSigned;

{$I ..\PascalRAL.inc}

{ Linked to a server (Server, or Server.AddPlugin), it looks at the certificate
  the engine was given when the server starts:
  - a certificate from a CA: left alone, and nothing is watched;
  - a self-signed certificate that is valid: used as it is, and watched;
  - a self-signed one that expired, or expires within RenewBeforeDays:
    renewed - a new key and certificate with the same names, written where the
    old one was (the same files when it came from files);
  - none: the plugin's own certificate in Folder, made when missing or due,
    and the engine is pointed at it. SSL is turned on.
  While the server runs, a thread checks every CheckInterval seconds and
  renews ahead of the expiry. The new certificate reaches the running engine
  through TRALServer.SetTLSCertificate, which swaps it under the listener:
  connections already open keep the old one, the next handshakes get the new
  one, and nothing is stopped (Sagui is the exception - libsagui only takes a
  certificate when it starts listening, so it reopens its listener).

  Without a server it is a generator: Generate or LoadOrGenerate, then
  CertificateFile, PrivateKeyFile, PfxFile, Fingerprint and ExportFingerprint.

  A renewed certificate is a new certificate, with a new fingerprint: a client
  that pins the old one (TRALClient.SSL.Pins) stops accepting the server.
  OnCertificateRenewed hands over both fingerprints for whoever distributes the
  pins; several lines for the same host in SSL.Pins is how a client accepts the
  old and the new during the change.

  The key is kept unencrypted in Folder, as "openssl req -nodes" does: the
  folder is what protects it. The default is under the user's profile, not
  next to the executable, where a web module serving files would expose it.
  Everything here is Pascal (RALRSA, RALX509): no library is needed to make
  the certificate, only the one each engine already loads to serve it. }

interface

uses
  {$IFDEF RALWindows}
  Windows,
  {$ENDIF}
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALConsts, RALTools, RALPlugin, RALServer, RALRSA, RALX509, RALASN1;

type
  /// Where a TRALSelfSignedPlugin stands
  TRALSelfSignedState = (
    /// nothing done yet
    sssNone,
    /// a self-signed certificate is in use (loaded or made) and watched
    sssActive,
    /// the server has a certificate from a CA, or one this plugin cannot read:
    /// left alone
    sssExternal,
    /// the engine has no TLS of its own (CGI, UniGUI)
    sssUnsupported,
    /// the last attempt failed - see OnError
    sssFailed);

  TRALSelfSignedCertEvent = procedure(Sender: TObject;
    const AFingerprint: StringRAL) of object;
  TRALSelfSignedRenewEvent = procedure(Sender: TObject;
    const AOldFingerprint, ANewFingerprint: StringRAL) of object;
  TRALSelfSignedErrorEvent = procedure(Sender: TObject; AError: Exception) of object;

  TRALSelfSignedPlugin = class;

  { TRALSelfSignedWatcher }

  /// Wakes every CheckInterval and renews when the certificate is due
  TRALSelfSignedWatcher = class(TThread)
  private
    FOwner: TRALSelfSignedPlugin;
    FWake: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TRALSelfSignedPlugin);
    destructor Destroy; override;
    /// asks the thread to end, without waiting
    procedure Stop;
    /// wakes it now, to check at once
    procedure Wake;
  end;

  { TRALSelfSignedPlugin }

  TRALSelfSignedPlugin = class(TRALPlugin)
  private
    FAutoRenew: boolean;
    FCheckInterval: IntegerRAL;
    FCommonName: StringRAL;
    FFileName: StringRAL;
    FFolder: StringRAL;
    FHostNames: TStrings;
    FKeyBits: IntegerRAL;
    FPfxPassword: StringRAL;
    FRenewBeforeDays: IntegerRAL;
    FValidDays: IntegerRAL;

    FLock: TCriticalSection;
    FCertificate: TRALX509Certificate;
    FKey: TRALRSAKey;
    FState: TRALSelfSignedState;
    FWatcher: TRALSelfSignedWatcher;
    { set while this plugin restarts its own server: the stop and start it
      causes are its own, and must not stop the watcher or provision again }
    FRestarting: boolean;
    { where the certificate in use is written when it is renewed }
    FTargetCert: string;
    FTargetKey: string;
    FTargetPfx: string;
    FTargetPfxPassword: StringRAL;
    FOwnFiles: boolean;
    { the password of the PFX of this run, when PfxPassword is empty: the PEM
      files are the lasting copy, the PFX is made again at every start }
    FRunPassword: StringRAL;
    FLastError: string;

    FOnCertificateCreated: TRALSelfSignedCertEvent;
    FOnCertificateRenewed: TRALSelfSignedRenewEvent;
    FOnError: TRALSelfSignedErrorEvent;

    function GetCertificateFile: string;
    function GetFingerprint: StringRAL;
    function GetFingerprintSHA1: StringRAL;
    function GetNotAfter: TDateTime;
    function GetNotBefore: TDateTime;
    function GetPfxFile: string;
    function GetPrivateKeyFile: string;
    procedure SetCheckInterval(AValue: IntegerRAL);
    procedure SetHostNames(AValue: TStrings);
    procedure SetKeyBits(AValue: IntegerRAL);
    procedure SetValidDays(AValue: IntegerRAL);

    function ServerOf: TRALServer;
    function BaseName: string;
    /// whether ACert is the one in this plugin's own files
    function IsOwnCertificate(ACert: TRALX509Certificate): boolean;
    function EffectivePfxPassword: StringRAL;
    /// the host names of a new certificate: HostNames, or the defaults
    procedure FillHostNames(AList: TStrings);
    function EffectiveCommonName(AHosts: TStrings): StringRAL;
    /// a new key and certificate; ACN and AHosts empty = the properties
    procedure MakeCertificate(const ACN: StringRAL; AHosts: TStrings;
      out AKey: TRALRSAKey; out ACert: TRALX509Certificate);
    /// takes AKey and ACert as the ones in use (frees the previous)
    procedure Adopt(AKey: TRALRSAKey; ACert: TRALX509Certificate);
    /// writes the certificate in use to the target files and the PFX
    procedure SaveTarget;
    /// the certificate in use in every form the engines take
    procedure FillTLS(out ATLS: TRALTLSCertificate);
    /// the plugin's own files in Folder: loaded when good, made otherwise
    procedure UseOwnFiles(AForceNew: boolean);
    /// reads what the engine was given; nil when it cannot be read. AMissing
    /// says the engine names files that do not exist
    function ReadEngineCertificate(const ATLS: TRALTLSCertificate;
      out AMissing: boolean): TRALX509Certificate;
    /// decides, makes and hands the certificate to the engine
    procedure Provision;
    /// hands the certificate in use to the engine; restarts the server when
    /// the engine says it has to
    procedure Apply;
    /// renews when due (the watcher calls it)
    procedure CheckRenewal;
    procedure DoRenew;
    procedure StartWatcher;
    procedure StopWatcher;
    procedure ReportError(AError: Exception);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ServerActivating; override;
    procedure ServerDeactivating; override;

    /// Makes a new key and certificate from the properties and saves them in
    /// Folder, whatever is there. On a running server it takes over at once
    procedure Generate;
    /// The certificate in Folder when it is good (key matches, names match,
    /// not due); a new one otherwise
    procedure LoadOrGenerate;
    /// A new certificate now, with the names of the one in use. On a running
    /// server the engine takes it without stopping
    procedure Renew;
    /// Whether the certificate in use expires within RenewBeforeDays (or is
    /// expired); True when there is none
    function NeedsRenewal: boolean;
    /// Writes the fingerprints to AFileName, one Name=Value per line: SHA256
    /// (colon separated), SHA1, Pin (the value for TRALClient.SSL.Pins),
    /// Subject, NotBefore and NotAfter (UTC, ISO 8601). Empty = Folder, with
    /// the certificate's name and '.fingerprint'
    procedure ExportFingerprint(const AFileName: string = '');
    /// The fingerprint as text with ASeparator between the bytes ('AB:CD:...')
    function FingerprintText(const ASeparator: StringRAL = ':'): StringRAL;
    /// The certificate in use, as PEM
    function CertificatePEM: StringRAL;

    /// The certificate in use; nil before any. Belongs to the plugin
    property Certificate: TRALX509Certificate read FCertificate;
    /// Files of the certificate in use: the plugin's own, or the ones the
    /// engine had when the plugin renewed a certificate of the application
    property CertificateFile: string read GetCertificateFile;
    property PrivateKeyFile: string read GetPrivateKeyFile;
    property PfxFile: string read GetPfxFile;
    /// SHA-256 of the certificate in use, uppercase hex with no separator -
    /// the format of TRALClient.SSL.Pins and of TRALCertInfo.Fingerprint
    property Fingerprint: StringRAL read GetFingerprint;
    /// SHA-1, the thumbprint of Windows (store, netsh)
    property FingerprintSHA1: StringRAL read GetFingerprintSHA1;
    /// validity of the certificate in use, UTC
    property NotBefore: TDateTime read GetNotBefore;
    property NotAfter: TDateTime read GetNotAfter;
    property State: TRALSelfSignedState read FState;
    /// the message of the last failure, empty when there was none
    property LastError: string read FLastError;
  published
    /// Renew while the server runs, ahead of the expiry
    property AutoRenew: boolean read FAutoRenew write FAutoRenew default True;
    /// Seconds between two checks of the expiry, while the server runs
    property CheckInterval: IntegerRAL read FCheckInterval write SetCheckInterval default 3600;
    /// Subject CN; empty = the first host name, or 'localhost'
    property CommonName: StringRAL read FCommonName write FCommonName;
    /// Name of the files, without extension (.crt, .key, .pfx); empty =
    /// 'ralselfsigned', and 'ralselfsigned-<port>' when linked to a server
    property FileName: StringRAL read FFileName write FFileName;
    /// Where the files are kept; empty = RALSelfSignedDefaultFolder
    property Folder: StringRAL read FFolder write FFolder;
    /// Names the certificate is valid for: DNS names ('*.example.com' too)
    /// and IP addresses, one per line ('DNS:' and 'IP:' force the kind).
    /// Empty = localhost, 127.0.0.1, ::1 and the name of this computer
    property HostNames: TStrings read FHostNames write SetHostNames;
    /// Size of the RSA key; 2048 is what every client accepts
    property KeyBits: IntegerRAL read FKeyBits write SetKeyBits default RALRSADefaultBits;
    /// Password of the PFX; empty = a random one for each run (the PFX is
    /// made again from the PEM files at every start)
    property PfxPassword: StringRAL read FPfxPassword write FPfxPassword;
    /// Renew this many days before the expiry
    property RenewBeforeDays: IntegerRAL read FRenewBeforeDays write FRenewBeforeDays default 30;
    /// Validity of a new certificate, in days
    property ValidDays: IntegerRAL read FValidDays write SetValidDays default 365;

    /// A certificate was made where there was none. Fires on the thread that
    /// made it: the server's starting thread, or the watcher
    property OnCertificateCreated: TRALSelfSignedCertEvent read FOnCertificateCreated
      write FOnCertificateCreated;
    /// A certificate was renewed: the old and the new fingerprint, for the
    /// clients that pin it. Fires on the thread that renewed it
    property OnCertificateRenewed: TRALSelfSignedRenewEvent read FOnCertificateRenewed
      write FOnCertificateRenewed;
    /// Something failed. While starting the exception also goes up and keeps
    /// the server stopped; in the watcher only this event hears it
    property OnError: TRALSelfSignedErrorEvent read FOnError write FOnError;
  end;

/// Where TRALSelfSignedPlugin keeps its files by default: %LOCALAPPDATA%\PascalRAL\
/// certs on Windows, ~/.config/pascalral/certs elsewhere
function RALSelfSignedDefaultFolder: string;

implementation

{$IFDEF FPC}
  {$IFDEF UNIX}
uses
  BaseUnix;
  {$ENDIF}
{$ELSE}
  {$IFDEF POSIX}
uses
  Posix.SysStat;
  {$ENDIF}
{$ENDIF}

function RALSelfSignedDefaultFolder: string;
var
  vBase: string;
begin
  {$IFDEF RALWindows}
  vBase := GetEnvironmentVariable('LOCALAPPDATA');
  if vBase = '' then
    vBase := GetEnvironmentVariable('APPDATA');
  if vBase <> '' then
    Result := IncludeTrailingPathDelimiter(vBase) + 'PascalRAL' + PathDelim + 'certs'
  else
    Result := ExtractFilePath(ParamStr(0)) + 'certs';
  {$ELSE}
  vBase := GetEnvironmentVariable('XDG_CONFIG_HOME');
  if vBase = '' then
  begin
    vBase := GetEnvironmentVariable('HOME');
    if vBase <> '' then
      vBase := IncludeTrailingPathDelimiter(vBase) + '.config';
  end;
  if vBase <> '' then
    Result := IncludeTrailingPathDelimiter(vBase) + 'pascalral' + PathDelim + 'certs'
  else
    Result := ExtractFilePath(ParamStr(0)) + 'certs';
  {$ENDIF}
  Result := IncludeTrailingPathDelimiter(Result);
end;

{ files }

procedure RestrictToOwner(const AFileName: string);
begin
  { the key is readable by its owner only; Windows already keeps a profile
    folder to its user }
  {$IFDEF FPC}
    {$IFDEF UNIX}
  FpChmod(AFileName, &600);
    {$ENDIF}
  {$ELSE}
    {$IFDEF POSIX}
  chmod(PAnsiChar(UTF8Encode(AFileName)), $180);
    {$ENDIF}
  {$ENDIF}
end;

{ writes next to the file and then replaces it, so a reader never finds half
  a certificate }
procedure WriteFileReplacing(const AFileName: string; const AData: TBytes;
  APrivate: boolean);
var
  vTemp: string;
begin
  ForceDirectories(ExtractFilePath(ExpandFileName(AFileName)));
  vTemp := AFileName + '.tmp';
  RALWriteFileBytes(vTemp, AData);
  if APrivate then
    RestrictToOwner(vTemp);
  if FileExists(AFileName) and not DeleteFile(AFileName) then
  begin
    DeleteFile(vTemp);
    raise Exception.CreateFmt(emSelfSignedWriteFailed, [AFileName,
      SysErrorMessage(GetLastError)]);
  end;
  if not RenameFile(vTemp, AFileName) then
    raise Exception.CreateFmt(emSelfSignedWriteFailed, [AFileName,
      SysErrorMessage(GetLastError)]);
end;

function RandomPassword: StringRAL;
const
  { no 0/O, 1/l/I: it may be read by a person }
  cChars: array[0..55] of AnsiChar =
    'ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnpqrstuvwxyz23456789';
var
  vBytes: TBytes;
  vInt: IntegerRAL;
begin
  vBytes := RandomBytes(24);
  SetLength(Result, Length(vBytes));
  for vInt := 0 to High(vBytes) do
    Result[vInt + POSINISTR] := cChars[vBytes[vInt] mod Length(cChars)];
end;

{ TRALSelfSignedWatcher }

constructor TRALSelfSignedWatcher.Create(AOwner: TRALSelfSignedPlugin);
begin
  FOwner := AOwner;
  FWake := TEvent.Create(nil, False, False, '');
  inherited Create(False);
end;

destructor TRALSelfSignedWatcher.Destroy;
begin
  inherited;
  FreeAndNil(FWake);
end;

procedure TRALSelfSignedWatcher.Execute;
var
  vWait: Cardinal;
begin
  while not Terminated do
  begin
    vWait := Cardinal(FOwner.FCheckInterval) * 1000;
    FWake.WaitFor(vWait);
    if Terminated then
      Break;
    try
      FOwner.CheckRenewal;
    except
      // reported by CheckRenewal; the watcher keeps watching
    end;
  end;
end;

procedure TRALSelfSignedWatcher.Stop;
begin
  Terminate;
  FWake.SetEvent;
end;

procedure TRALSelfSignedWatcher.Wake;
begin
  FWake.SetEvent;
end;

{ TRALSelfSignedPlugin }

constructor TRALSelfSignedPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FHostNames := TStringList.Create;
  FAutoRenew := True;
  FCheckInterval := 3600;
  FKeyBits := RALRSADefaultBits;
  FRenewBeforeDays := 30;
  FValidDays := 365;
  FState := sssNone;
end;

destructor TRALSelfSignedPlugin.Destroy;
begin
  { the watcher before anything it reads; the host is told by the base
    destructor, which does not call ServerDeactivating on a plugin that is
    being destroyed }
  StopWatcher;
  inherited Destroy;
  FreeAndNil(FCertificate);
  FreeAndNil(FKey);
  FreeAndNil(FHostNames);
  FreeAndNil(FLock);
end;

procedure TRALSelfSignedPlugin.Loaded;
begin
  inherited;
  { linked while loading, the plugin could not hear about a server that was
    already running: its properties were not read yet }
  if (Host <> nil) and (ServerOf <> nil) and ServerOf.Active and CanNotify then
    ServerActivating;
end;

function TRALSelfSignedPlugin.ServerOf: TRALServer;
begin
  if Host is TRALServer then
    Result := TRALServer(Host)
  else
    Result := nil;
end;

function TRALSelfSignedPlugin.BaseName: string;
begin
  if FFileName <> '' then
    Result := string(FFileName)
  else if ServerOf <> nil then
    Result := 'ralselfsigned-' + IntToStr(ServerOf.Port)
  else
    Result := 'ralselfsigned';
  if FFolder <> '' then
    Result := IncludeTrailingPathDelimiter(string(FFolder)) + Result
  else
    Result := RALSelfSignedDefaultFolder + Result;
end;

function TRALSelfSignedPlugin.IsOwnCertificate(ACert: TRALX509Certificate): boolean;
var
  vOwn: TRALX509Certificate;
begin
  Result := False;
  if not FileExists(BaseName + '.crt') then
    Exit;
  try
    vOwn := TRALX509Certificate.FromFile(BaseName + '.crt');
    try
      Result := RALSameBytes(vOwn.DER, ACert.DER);
    finally
      vOwn.Free;
    end;
  except
    Result := False;
  end;
end;

function TRALSelfSignedPlugin.EffectivePfxPassword: StringRAL;
begin
  if FPfxPassword <> '' then
    Result := FPfxPassword
  else
  begin
    if FRunPassword = '' then
      FRunPassword := RandomPassword;
    Result := FRunPassword;
  end;
end;

procedure TRALSelfSignedPlugin.FillHostNames(AList: TStrings);
var
  vInt: IntegerRAL;
  vName: string;
begin
  AList.Clear;
  for vInt := 0 to FHostNames.Count - 1 do
    if Trim(FHostNames[vInt]) <> '' then
      AList.Add(Trim(FHostNames[vInt]));
  if AList.Count > 0 then
    Exit;

  AList.Add('localhost');
  AList.Add('127.0.0.1');
  AList.Add('::1');
  {$IFDEF RALWindows}
  vName := GetEnvironmentVariable('COMPUTERNAME');
  {$ELSE}
  vName := GetEnvironmentVariable('HOSTNAME');
  {$ENDIF}
  vName := LowerCase(Trim(vName));
  if (vName <> '') and (AList.IndexOf(vName) < 0) then
    AList.Add(vName);
end;

function TRALSelfSignedPlugin.EffectiveCommonName(AHosts: TStrings): StringRAL;
var
  vInt: IntegerRAL;
  vName: string;
  vAddr: TBytes;
begin
  Result := FCommonName;
  if Result <> '' then
    Exit;
  { the first DNS name: an address as a CN is what browsers stopped reading }
  for vInt := 0 to AHosts.Count - 1 do
  begin
    vName := AHosts[vInt];
    if SameText(Copy(vName, 1, 4), 'DNS:') then
      vName := Trim(Copy(vName, 5, MaxInt))
    else if SameText(Copy(vName, 1, 3), 'IP:') then
      Continue;
    if not RALParseIPAddress(StringRAL(vName), vAddr) then
    begin
      Result := StringRAL(vName);
      Exit;
    end;
  end;
  Result := 'localhost';
end;

procedure TRALSelfSignedPlugin.MakeCertificate(const ACN: StringRAL; AHosts: TStrings;
  out AKey: TRALRSAKey; out ACert: TRALX509Certificate);
var
  vHosts: TStringList;
  vCN: StringRAL;
  vNow: TDateTime;
begin
  vHosts := TStringList.Create;
  try
    if (AHosts <> nil) and (AHosts.Count > 0) then
      vHosts.Assign(AHosts)
    else
      FillHostNames(vHosts);
    vCN := ACN;
    if vCN = '' then
      vCN := EffectiveCommonName(vHosts);

    AKey := TRALRSAKey.Generate(FKeyBits);
    try
      vNow := RALNowUTC;
      { an hour back: a client whose clock runs a little behind still takes it }
      ACert := TRALX509Certificate.Create(RALCreateCertificate(AKey, vCN, vHosts,
        IncHour(vNow, -1), IncDay(vNow, FValidDays)));
    except
      FreeAndNil(AKey);
      raise;
    end;
  finally
    vHosts.Free;
  end;
end;

procedure TRALSelfSignedPlugin.Adopt(AKey: TRALRSAKey; ACert: TRALX509Certificate);
begin
  if FKey <> AKey then
    FreeAndNil(FKey);
  if FCertificate <> ACert then
    FreeAndNil(FCertificate);
  FKey := AKey;
  FCertificate := ACert;
end;

procedure TRALSelfSignedPlugin.SaveTarget;
begin
  if FTargetKey <> '' then
    WriteFileReplacing(FTargetKey, RALStringToBytes(FKey.PrivateKeyPEM), True);
  if FTargetCert <> '' then
    WriteFileReplacing(FTargetCert, RALStringToBytes(FCertificate.ToPEM), False);
  if FTargetPfx <> '' then
    WriteFileReplacing(FTargetPfx, RALCreatePKCS12(FCertificate.DER, FKey,
      FTargetPfxPassword, RALSelfSignedFriendlyName + ' ' + FCertificate.CommonName),
      True);
end;

procedure TRALSelfSignedPlugin.FillTLS(out ATLS: TRALTLSCertificate);
begin
  RALClearTLSCertificate(ATLS);
  ATLS.CertificateFile := StringRAL(FTargetCert);
  ATLS.PrivateKeyFile := StringRAL(FTargetKey);
  ATLS.PfxFile := StringRAL(FTargetPfx);
  ATLS.PfxPassword := FTargetPfxPassword;
  ATLS.CertificatePEM := FCertificate.ToPEM;
  ATLS.PrivateKeyPEM := FKey.PrivateKeyPEM;
  ATLS.CertificateDER := Copy(FCertificate.DER, 0, Length(FCertificate.DER));
  ATLS.PrivateKeyDER := FKey.PrivateKeyPKCS1;
end;

procedure TRALSelfSignedPlugin.UseOwnFiles(AForceNew: boolean);
var
  vBase: string;
  vKey: TRALRSAKey;
  vCert: TRALX509Certificate;
  vHosts: TStringList;
  vInt: IntegerRAL;
  vGood, vCreated: boolean;
begin
  vBase := BaseName;
  FTargetCert := vBase + '.crt';
  FTargetKey := vBase + '.key';
  FTargetPfx := vBase + '.pfx';
  FTargetPfxPassword := EffectivePfxPassword;
  FOwnFiles := True;

  vKey := nil;
  vCert := nil;
  vGood := False;
  if (not AForceNew) and FileExists(FTargetCert) and FileExists(FTargetKey) then
  begin
    try
      vCert := TRALX509Certificate.FromFile(FTargetCert);
      vKey := TRALRSAKey.FromFile(FTargetKey);
      vGood := vCert.MatchesKey(vKey) and vCert.IsSelfSigned and
        (RALNowUTC < IncDay(vCert.NotAfter, -FRenewBeforeDays));
      { the names asked for are still the names in it }
      if vGood then
      begin
        vHosts := TStringList.Create;
        try
          FillHostNames(vHosts);
          for vInt := 0 to vHosts.Count - 1 do
            if vCert.HostNames.IndexOf(vHosts[vInt]) < 0 then
            begin
              { 'DNS:x' and 'IP:x' are stored as x }
              if (Pos(':', vHosts[vInt]) > 0) and
                (vCert.HostNames.IndexOf(Trim(Copy(vHosts[vInt],
                  Pos(':', vHosts[vInt]) + 1, MaxInt))) >= 0) and
                (SameText(Copy(vHosts[vInt], 1, 4), 'DNS:') or
                 SameText(Copy(vHosts[vInt], 1, 3), 'IP:')) then
                Continue;
              vGood := False;
              Break;
            end;
          if vGood and (FCommonName <> '') and (vCert.CommonName <> FCommonName) then
            vGood := False;
        finally
          vHosts.Free;
        end;
      end;
    except
      vGood := False;
    end;
  end;

  vCreated := not vGood;
  if not vGood then
  begin
    FreeAndNil(vKey);
    FreeAndNil(vCert);
    MakeCertificate('', nil, vKey, vCert);
  end;
  Adopt(vKey, vCert);

  if vCreated then
    SaveTarget
  else
    { the PEM files are good; the PFX carries this run's password }
    WriteFileReplacing(FTargetPfx, RALCreatePKCS12(FCertificate.DER, FKey,
      FTargetPfxPassword, RALSelfSignedFriendlyName + ' ' + FCertificate.CommonName),
      True);

  if vCreated and Assigned(FOnCertificateCreated) then
    FOnCertificateCreated(Self, FCertificate.FingerprintSHA256);
end;

function TRALSelfSignedPlugin.ReadEngineCertificate(const ATLS: TRALTLSCertificate;
  out AMissing: boolean): TRALX509Certificate;
var
  vData: TBytes;
begin
  Result := nil;
  AMissing := False;
  try
    if Length(ATLS.CertificateDER) > 0 then
      Result := TRALX509Certificate.Create(ATLS.CertificateDER)
    else if ATLS.CertificatePEM <> '' then
      Result := TRALX509Certificate.FromPEM(ATLS.CertificatePEM)
    else if ATLS.CertificateFile <> '' then
    begin
      if FileExists(string(ATLS.CertificateFile)) then
        Result := TRALX509Certificate.FromFile(string(ATLS.CertificateFile))
      else
        AMissing := True;
    end
    else if ATLS.PfxFile <> '' then
    begin
      if not FileExists(string(ATLS.PfxFile)) then
        AMissing := True
      else
      begin
        { a "PFX" that is a PEM holding the certificate and the key is read
          here; a real PKCS#12 only when the engine could (it filled the DER) }
        vData := RALReadFileBytes(string(ATLS.PfxFile));
        if IsPem(RALBytesToString(vData)) then
          Result := TRALX509Certificate.FromPEM(RALBytesToString(vData));
      end;
    end;
  except
    FreeAndNil(Result);
  end;
end;

procedure TRALSelfSignedPlugin.Provision;
var
  vServer: TRALServer;
  vTLS: TRALTLSCertificate;
  vCurrent: TRALX509Certificate;
  vMissing, vOwn: boolean;
  vProv: TRALTLSProvisioning;
  vKey: TRALRSAKey;
  vCert: TRALX509Certificate;
  vOld: StringRAL;
begin
  vServer := ServerOf;
  if vServer = nil then
    Exit;

  vProv := vServer.TLSProvisioning;
  if vProv = tpNone then
  begin
    FState := sssUnsupported;
    Exit;
  end;

  vCurrent := nil;
  vMissing := False;
  if vServer.GetTLSCertificate(vTLS) then
  begin
    { our own, from an earlier start of this server in this process }
    vOwn := FOwnFiles and (FCertificate <> nil) and
      (((vTLS.CertificateFile <> '') and SameFileName(string(vTLS.CertificateFile), FTargetCert)) or
       ((vTLS.PfxFile <> '') and SameFileName(string(vTLS.PfxFile), FTargetPfx)));
    if not vOwn then
    begin
      vCurrent := ReadEngineCertificate(vTLS, vMissing);
      if (vCurrent = nil) and not vMissing then
      begin
        { a certificate the plugin cannot read - a PKCS#12 off Windows, a
          protected key: somebody chose it, it is not this plugin's }
        FState := sssExternal;
        Exit;
      end;
      { this plugin's own, from an earlier run: http.sys keeps a binding after
        the process ends. Its files decide, as when nothing is configured }
      if (vCurrent <> nil) and IsOwnCertificate(vCurrent) then
        FreeAndNil(vCurrent);
    end;
  end;

  if vCurrent <> nil then
  begin
    try
      if not vCurrent.IsSelfSigned then
      begin
        FState := sssExternal;
        Exit;
      end;

      { a self-signed certificate of the application: renewed where it is }
      FTargetCert := string(vTLS.CertificateFile);
      FTargetKey := string(vTLS.PrivateKeyFile);
      FTargetPfx := string(vTLS.PfxFile);
      FTargetPfxPassword := vTLS.PfxPassword;
      FOwnFiles := False;
      { without a key file there is nothing to rewrite in place (PEM text, the
        Windows store, a PEM holding both): the plugin's own files }
      if (vProv = tpPEMText) or (vProv = tpWindowsStore) or
        ((FTargetCert <> '') and (FTargetKey = '')) or
        ((FTargetCert = '') and (FTargetPfx = '')) then
      begin
        FTargetCert := BaseName + '.crt';
        FTargetKey := BaseName + '.key';
        FTargetPfx := BaseName + '.pfx';
        FTargetPfxPassword := EffectivePfxPassword;
        FOwnFiles := True;
      end;

      { configured with SSL off it is not what the server serves - for
        http.sys, a binding somebody left on the port: the plugin's own }
      if not vServer.SSLEnabled then
      begin
        FreeAndNil(vCurrent);
        UseOwnFiles(False);
        Apply;
        FState := sssActive;
        Exit;
      end;

      if RALNowUTC < IncDay(vCurrent.NotAfter, -FRenewBeforeDays) then
      begin
        { good as it is: used, and watched. Its key is only needed to renew,
          and renewing makes a new one }
        FreeAndNil(FKey);
        FreeAndNil(FCertificate);
        FCertificate := vCurrent;
        vCurrent := nil;
        FState := sssActive;
        Exit;
      end;

      vOld := vCurrent.FingerprintSHA256;
      MakeCertificate(vCurrent.CommonName, vCurrent.HostNames, vKey, vCert);
      Adopt(vKey, vCert);
      SaveTarget;
      Apply;
      FState := sssActive;
      if Assigned(FOnCertificateRenewed) then
        FOnCertificateRenewed(Self, vOld, FCertificate.FingerprintSHA256);
      Exit;
    finally
      vCurrent.Free;
    end;
  end;

  { none (or files named that do not exist): the plugin's own }
  UseOwnFiles(False);
  Apply;
  FState := sssActive;
end;

procedure TRALSelfSignedPlugin.Apply;
var
  vServer: TRALServer;
  vTLS: TRALTLSCertificate;
begin
  vServer := ServerOf;
  if (vServer = nil) or (FCertificate = nil) or (FKey = nil) then
    Exit;

  FillTLS(vTLS);
  if vServer.SetTLSCertificate(vTLS) = tarRestartNeeded then
  begin
    { the listener opened on plain http (the plugin joined a running server,
      or the form started it before the plugin was loaded): TLS needs a new
      listener. The stop and the start are this plugin's own doing }
    FRestarting := True;
    try
      vServer.Active := False;
      vServer.Active := True;
    finally
      FRestarting := False;
    end;
  end;
end;

procedure TRALSelfSignedPlugin.ServerActivating;
begin
  if FRestarting then
    Exit;

  FLock.Acquire;
  try
    try
      FLastError := '';
      Provision;
    except
      on E: Exception do
      begin
        FState := sssFailed;
        ReportError(E);
        raise;
      end;
    end;
  finally
    FLock.Release;
  end;

  if (FState = sssActive) and FAutoRenew then
    StartWatcher;
end;

procedure TRALSelfSignedPlugin.ServerDeactivating;
begin
  if FRestarting then
    Exit;
  StopWatcher;
end;

procedure TRALSelfSignedPlugin.StartWatcher;
begin
  if FWatcher = nil then
    FWatcher := TRALSelfSignedWatcher.Create(Self);
end;

procedure TRALSelfSignedPlugin.StopWatcher;
var
  vWatcher: TRALSelfSignedWatcher;
begin
  vWatcher := FWatcher;
  if vWatcher = nil then
    Exit;
  FWatcher := nil;
  vWatcher.Stop;
  { from the watcher itself - the engine stopped the server while the watcher
    was renewing: it cannot wait for itself, so it frees itself }
  if GetCurrentThreadId = vWatcher.ThreadID then
    vWatcher.FreeOnTerminate := True
  else
  begin
    vWatcher.WaitFor;
    vWatcher.Free;
  end;
end;

procedure TRALSelfSignedPlugin.ReportError(AError: Exception);
begin
  FLastError := AError.Message;
  if Assigned(FOnError) then
    try
      FOnError(Self, AError);
    except
      // the handler's own failure must not hide the one it was told about
    end;
end;

function TRALSelfSignedPlugin.NeedsRenewal: boolean;
begin
  Result := (FCertificate = nil) or
    (RALNowUTC >= IncDay(FCertificate.NotAfter, -FRenewBeforeDays));
end;

procedure TRALSelfSignedPlugin.CheckRenewal;
begin
  FLock.Acquire;
  try
    if (FState <> sssActive) or not NeedsRenewal then
      Exit;
    try
      DoRenew;
    except
      on E: Exception do
      begin
        ReportError(E);
        raise;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TRALSelfSignedPlugin.DoRenew;
var
  vKey: TRALRSAKey;
  vCert: TRALX509Certificate;
  vOld: StringRAL;
begin
  // runs under FLock
  if FCertificate = nil then
  begin
    UseOwnFiles(True);
    Apply;
    FState := sssActive;
    Exit;
  end;

  vOld := FCertificate.FingerprintSHA256;
  MakeCertificate(FCertificate.CommonName, FCertificate.HostNames, vKey, vCert);
  if FTargetCert = '' then
  begin
    FTargetCert := BaseName + '.crt';
    FTargetKey := BaseName + '.key';
    FTargetPfx := BaseName + '.pfx';
    FTargetPfxPassword := EffectivePfxPassword;
    FOwnFiles := True;
  end;
  Adopt(vKey, vCert);
  SaveTarget;
  Apply;
  FState := sssActive;
  if Assigned(FOnCertificateRenewed) then
    FOnCertificateRenewed(Self, vOld, FCertificate.FingerprintSHA256);
end;

procedure TRALSelfSignedPlugin.Generate;
begin
  FLock.Acquire;
  try
    try
      UseOwnFiles(True);
      if (ServerOf <> nil) and ServerOf.Active then
        Apply;
      FState := sssActive;
    except
      on E: Exception do
      begin
        ReportError(E);
        raise;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TRALSelfSignedPlugin.LoadOrGenerate;
begin
  FLock.Acquire;
  try
    try
      UseOwnFiles(False);
      if (ServerOf <> nil) and ServerOf.Active then
        Apply;
      FState := sssActive;
    except
      on E: Exception do
      begin
        ReportError(E);
        raise;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TRALSelfSignedPlugin.Renew;
begin
  FLock.Acquire;
  try
    try
      DoRenew;
    except
      on E: Exception do
      begin
        ReportError(E);
        raise;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TRALSelfSignedPlugin.ExportFingerprint(const AFileName: string);
var
  vText: StringRAL;
  vFile: string;
begin
  FLock.Acquire;
  try
    if FCertificate = nil then
      raise Exception.Create(emSelfSignedNoCertificate);

    vFile := AFileName;
    if vFile = '' then
      vFile := BaseName + '.fingerprint';

    vText := 'SHA256=' + RALFormatFingerprint(FCertificate.FingerprintSHA256) + #10 +
      'SHA1=' + RALFormatFingerprint(FCertificate.FingerprintSHA1) + #10 +
      'Pin=' + FCertificate.FingerprintSHA256 + #10 +
      'Subject=' + FCertificate.Subject + #10 +
      'NotBefore=' + StringRAL(FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"',
        FCertificate.NotBefore)) + #10 +
      'NotAfter=' + StringRAL(FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"',
        FCertificate.NotAfter)) + #10;
    ForceDirectories(ExtractFilePath(ExpandFileName(vFile)));
    RALWriteFileBytes(vFile, RALStringToBytes(vText));
  finally
    FLock.Release;
  end;
end;

function TRALSelfSignedPlugin.FingerprintText(const ASeparator: StringRAL): StringRAL;
begin
  Result := RALFormatFingerprint(Fingerprint, ASeparator);
end;

function TRALSelfSignedPlugin.CertificatePEM: StringRAL;
begin
  FLock.Acquire;
  try
    if FCertificate <> nil then
      Result := FCertificate.ToPEM
    else
      Result := '';
  finally
    FLock.Release;
  end;
end;

function TRALSelfSignedPlugin.GetCertificateFile: string;
begin
  Result := FTargetCert;
end;

function TRALSelfSignedPlugin.GetPrivateKeyFile: string;
begin
  Result := FTargetKey;
end;

function TRALSelfSignedPlugin.GetPfxFile: string;
begin
  Result := FTargetPfx;
end;

function TRALSelfSignedPlugin.GetFingerprint: StringRAL;
begin
  FLock.Acquire;
  try
    if FCertificate <> nil then
      Result := FCertificate.FingerprintSHA256
    else
      Result := '';
  finally
    FLock.Release;
  end;
end;

function TRALSelfSignedPlugin.GetFingerprintSHA1: StringRAL;
begin
  FLock.Acquire;
  try
    if FCertificate <> nil then
      Result := FCertificate.FingerprintSHA1
    else
      Result := '';
  finally
    FLock.Release;
  end;
end;

function TRALSelfSignedPlugin.GetNotAfter: TDateTime;
begin
  if FCertificate <> nil then
    Result := FCertificate.NotAfter
  else
    Result := 0;
end;

function TRALSelfSignedPlugin.GetNotBefore: TDateTime;
begin
  if FCertificate <> nil then
    Result := FCertificate.NotBefore
  else
    Result := 0;
end;

procedure TRALSelfSignedPlugin.SetCheckInterval(AValue: IntegerRAL);
begin
  if AValue < 1 then
    AValue := 1;
  FCheckInterval := AValue;
  { a watcher sleeping on the old interval checks now and sleeps the new one }
  if FWatcher <> nil then
    FWatcher.Wake;
end;

procedure TRALSelfSignedPlugin.SetHostNames(AValue: TStrings);
begin
  FHostNames.Assign(AValue);
end;

procedure TRALSelfSignedPlugin.SetKeyBits(AValue: IntegerRAL);
begin
  if AValue < 1024 then
    AValue := 1024;
  FKeyBits := AValue;
end;

procedure TRALSelfSignedPlugin.SetValidDays(AValue: IntegerRAL);
begin
  if AValue < 1 then
    AValue := 1;
  FValidDays := AValue;
end;

end.
