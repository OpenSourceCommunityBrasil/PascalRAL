unit AcmeHttp;

{$IFDEF FPC}
{$mode delphi}
{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  AcmeCommon,
  RALDynLibs, RALTypes;

type
  {$IFDEF MSWINDOWS}
  TCurlLong = LongInt;
  {$ELSE}
  TCurlLong = NativeInt;
  {$ENDIF}
  TAcmeHttp = class
  private
    Lib: TRALDynLibs;
    Initialized: Boolean;
    curl_global_init: function(Flags: TCurlLong): Integer; cdecl;
    curl_global_cleanup: procedure; cdecl;
    curl_easy_init: function: Pointer; cdecl;
    curl_easy_cleanup: procedure(P: Pointer); cdecl;
    curl_easy_setopt_ptr: function(
      P: Pointer;
      Option: Integer;
      Value: Pointer
    ): Integer; cdecl;
    curl_easy_setopt_long: function(
      P: Pointer;
      Option: Integer;
      Value: TCurlLong
    ): Integer; cdecl;
    curl_easy_perform: function(P: Pointer): Integer; cdecl;
    curl_easy_getinfo: function(
      P: Pointer;
      Info: Integer;
      var Value: TCurlLong
    ): Integer; cdecl;
    curl_slist_append: function(
      List: Pointer;
      Text: PAnsiChar
    ): Pointer; cdecl;
    curl_slist_free_all: procedure(P: Pointer); cdecl;
    curl_easy_strerror: function(Code: Integer): PAnsiChar; cdecl;
  public
    constructor Create(const LibraryPath: string);
    destructor Destroy; override;
    function Request(
      const Method: string;
      const URL: string;
      const Body: string;
      const CABundle: string;
      TimeoutMs: Integer
    ): TAcmeResponse;
  end;

implementation

type
  TCapture = class
    Body: RawByteString;
    Headers: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TCapture.Create;
begin
  inherited;
  Headers := TStringList.Create;
  Headers.NameValueSeparator := ':';
end;

destructor TCapture.Destroy;
begin
  Headers.Free;
  inherited;
end;

function WriteBody(
  Data: Pointer;
  Size: NativeUInt;
  Count: NativeUInt;
  User: Pointer
): NativeUInt; cdecl;
var
  Capture: TCapture;
  Chunk: RawByteString;
begin
  Result := 0;
  try
    Capture := TCapture(User);
    if (Size > 4194304) or (Count > 4194304) or (Size * Count > 4194304) then
      Exit;
    if NativeUInt(Length(Capture.Body)) + Size * Count > 4194304 then
      Exit;
    SetString(Chunk, PAnsiChar(Data), Size * Count);
    Capture.Body := Capture.Body + Chunk;
    Result := Size * Count;
  except
    Result := 0;
  end;
end;

function WriteHeader(
  Data: Pointer;
  Size: NativeUInt;
  Count: NativeUInt;
  User: Pointer
): NativeUInt; cdecl;
var
  Line: RawByteString;
  S: string;
  P: Integer;
  Capture: TCapture;
begin
  Result := 0;
  try
    if (Size > 65536) or (Count > 65536) or (Size * Count > 65536) then
      Exit;
    SetString(Line, PAnsiChar(Data), Size * Count);
    S := Trim(string(Line));
    Capture := TCapture(User);
    if Pos('HTTP/', S) = 1 then
      Capture.Headers.Clear
    else
    begin
      P := Pos(':', S);
      if P > 0 then
        Capture.Headers.Values[Copy(S, 1, P - 1)] := Trim(Copy(S, P + 1, MaxInt));
    end;
    Result := Size * Count;
  except
    Result := 0;
  end;
end;

constructor TAcmeHttp.Create(const LibraryPath: string);
begin
  inherited Create;
  Lib := TRALDynLibs.Create(LibraryPath);
  Lib.Bind(curl_global_init, 'curl_global_init');
  Lib.Bind(curl_global_cleanup, 'curl_global_cleanup');
  Lib.Bind(curl_easy_init, 'curl_easy_init');
  Lib.Bind(curl_easy_cleanup, 'curl_easy_cleanup');
  Lib.Bind(curl_easy_setopt_ptr, 'curl_easy_setopt');
  Lib.Bind(curl_easy_setopt_long, 'curl_easy_setopt');
  Lib.Bind(curl_easy_perform, 'curl_easy_perform');
  Lib.Bind(curl_easy_getinfo, 'curl_easy_getinfo');
  Lib.Bind(curl_slist_append, 'curl_slist_append');
  Lib.Bind(curl_slist_free_all, 'curl_slist_free_all');
  Lib.Bind(curl_easy_strerror, 'curl_easy_strerror');
  if curl_global_init(3) <> 0 then
    raise ERALException.Create('libcurl initialization failed.');
  Initialized := True;
end;

destructor TAcmeHttp.Destroy;
begin
  if Initialized then
    curl_global_cleanup();
  Lib.Free;
  inherited;
end;

function TAcmeHttp.Request(
  const Method: string;
  const URL: string;
  const Body: string;
  const CABundle: string;
  TimeoutMs: Integer
): TAcmeResponse;
var
  Handle: Pointer;
  Headers: Pointer;
  Capture: TCapture;
  Code: Integer;
  Status: TCurlLong;
  URLBytes: UTF8String;
  BodyBytes: UTF8String;
  CABundleBytes: UTF8String;
  Agent: UTF8String;
  ErrorBuffer: array[0..255] of AnsiChar;

  procedure SetPtr(
    Option: Integer;
    Value: Pointer
  );
  begin
    if curl_easy_setopt_ptr(Handle, Option, Value) <> 0 then
      raise ERALException.Create('Cannot configure libcurl option.');
  end;

  procedure SetLong(
    Option: Integer;
    Value: TCurlLong
  );
  begin
    if curl_easy_setopt_long(Handle, Option, Value) <> 0 then
      raise ERALException.Create('Cannot configure libcurl option.');
  end;

begin
  if not SameText(Copy(URL, 1, 8), 'https://') then
    raise ERALException.Create('ACME endpoints must use HTTPS.');
  if TimeoutMs < 1 then
    raise ERALException.Create('HTTP deadline expired.');
  Result.Status := 0;
  Result.Body := '';
  Result.Nonce := '';
  Result.Location := '';
  Result.RetryAfter := '';
  Capture := TCapture.Create;
  try
    Handle := curl_easy_init();
    if Handle = nil then
      raise ERALException.Create('Cannot create HTTP handle.');
    Headers := nil;
    try
      URLBytes := UTF8Encode(URL);
      BodyBytes := UTF8Encode(Body);
      CABundleBytes := UTF8Encode(CABundle);
      FillChar(ErrorBuffer, SizeOf(ErrorBuffer), 0);
      SetPtr(10010, @ErrorBuffer); // CURLOPT_ERRORBUFFER
      Agent := 'Pascal-LetsEncrypt/1.0';
      SetPtr(10002, PAnsiChar(URLBytes)); // CURLOPT_URL
      SetPtr(10018, PAnsiChar(Agent));
      SetLong(181, 2); // CURLOPT_PROTOCOLS: HTTPS only
      SetLong(52, 0); // no redirects: JWS URL must match exactly
      SetLong(64, 1);
      SetLong(81, 2); // verify CA and hostname
      SetLong(99, 1); // NOSIGNAL, for worker threads
      SetLong(155, TimeoutMs);
      SetLong(156, TimeoutMs);
      if CABundle <> '' then
        SetPtr(10065, PAnsiChar(CABundleBytes));
      SetPtr(20011, @WriteBody);
      SetPtr(10001, Pointer(Capture));
      SetPtr(20079, @WriteHeader);
      SetPtr(10029, Pointer(Capture));
      if Method = 'HEAD' then
        SetLong(44, 1)
      else if Method = 'POST' then
      begin
        Headers := curl_slist_append(nil, 'Content-Type: application/jose+json');
        if Headers = nil then
          raise ERALException.Create('Cannot allocate HTTP headers.');
        SetPtr(10023, Headers);
        SetLong(47, 1);
        SetLong(60, Length(BodyBytes));
        SetPtr(10015, PAnsiChar(BodyBytes));
      end
      else if Method <> 'GET' then
        raise ERALException.Create('Unsupported HTTP method.');
      Code := curl_easy_perform(Handle);
      if Code <> 0 then
        raise ERALException.Create('HTTPS request failed: ' +
          string(curl_easy_strerror(Code)) + '. ' + string(PAnsiChar(@ErrorBuffer)));
      if curl_easy_getinfo(Handle, $200002, Status) <> 0 then
        raise ERALException.Create('Cannot read HTTP status.');
      Result.Status := Status;
      {$IFDEF FPC}
      Result.Body := string(Capture.Body);
      {$ELSE}
      Result.Body := UTF8ToString(UTF8String(Capture.Body));
      {$ENDIF}
      Result.Nonce := Capture.Headers.Values['Replay-Nonce'];
      Result.Location := Capture.Headers.Values['Location'];
      Result.RetryAfter := Capture.Headers.Values['Retry-After'];
    finally
      if Headers <> nil then
        curl_slist_free_all(Headers);
      curl_easy_cleanup(Handle);
    end;
  finally
    Capture.Free;
  end;
end;

end.
