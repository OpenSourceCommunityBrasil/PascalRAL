/// HTTPS GET, LCL-free. On Windows it uses WinINet: TLS and proxy are the
/// system's, and the executable needs no OpenSSL DLL beside it. Elsewhere it
/// uses fphttpclient with the system OpenSSL.
unit RALInst.HTTP;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  /// Download progress; ATotal is -1 when the server does not say.
  TProgressoHTTP = procedure(const ALidos, ATotal: int64) of object;

  /// Minimal HTTP client: GET with custom headers and progress.
  TClienteHTTP = class
  private
    FCabecalhos: TStringList;
    FErro: string;
    FOnProgresso: TProgressoHTTP;
    FResposta: TStringList;
    FStatus: integer;
    {$IFNDEF MSWINDOWS}
    /// fphttpclient progress, passed on to OnProgresso
    procedure DadosRecebidos(Sender: TObject; const ContentLength, CurrentPos: int64);
    {$ENDIF}
    /// Keeps the response headers, 'Name:value'
    procedure GuardarCabecalhos(const ABrutos: string);
    /// Calls OnProgresso when assigned
    procedure Progresso(const ALidos, ATotal: int64);
  public
    constructor Create;
    destructor Destroy; override;
    /// Header of the last response ('' when absent)
    function Cabecalho(const ANome: string): string;
    /// True with status 2xx or 304, and ADestino gets the body. False with Erro
    /// set (no network, DNS, TLS) or with the response Status
    function Obter(const AURL: string; ADestino: TStream): boolean;
    /// Obter into a string
    function ObterTexto(const AURL: string; out ATexto: string): boolean;

    /// Sent with every request, 'Name: value'
    property Cabecalhos: TStringList read FCabecalhos;
    property Erro: string read FErro;
    property Status: integer read FStatus;
    property OnProgresso: TProgressoHTTP read FOnProgresso write FOnProgresso;
  end;

const
  /// User-Agent of every request
  AgenteHTTP = 'RALInstaller/1.0 ' +
               '(+https://github.com/OpenSourceCommunityBrasil/PascalRAL)';

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows, WinINet,
  {$ELSE}
  fphttpclient, opensslsockets,
  {$ENDIF}
  RALInst.Mensagens;

{ TClienteHTTP }

constructor TClienteHTTP.Create;
begin
  inherited Create;
  FCabecalhos := TStringList.Create;
  FResposta := TStringList.Create;
  FResposta.NameValueSeparator := ':';
end;

destructor TClienteHTTP.Destroy;
begin
  FResposta.Free;
  FCabecalhos.Free;
  inherited Destroy;
end;

procedure TClienteHTTP.Progresso(const ALidos, ATotal: int64);
begin
  if Assigned(FOnProgresso) then
    FOnProgresso(ALidos, ATotal);
end;

procedure TClienteHTTP.GuardarCabecalhos(const ABrutos: string);
var
  vLinhas: TStringList;
  vInt, vPos: integer;
begin
  FResposta.Clear;
  vLinhas := TStringList.Create;
  try
    vLinhas.Text := ABrutos;
    for vInt := 0 to Pred(vLinhas.Count) do
    begin
      vPos := Pos(':', vLinhas[vInt]);
      if vPos > 1 then
        FResposta.Add(Trim(Copy(vLinhas[vInt], 1, vPos - 1)) + ':' +
                      Trim(Copy(vLinhas[vInt], vPos + 1, MaxInt)));
    end;
  finally
    vLinhas.Free;
  end;
end;

function TClienteHTTP.Cabecalho(const ANome: string): string;
var
  vInt: integer;
begin
  Result := '';
  for vInt := 0 to Pred(FResposta.Count) do
    if SameText(FResposta.Names[vInt], ANome) then
      Exit(FResposta.ValueFromIndex[vInt]);
end;

function TClienteHTTP.ObterTexto(const AURL: string; out ATexto: string): boolean;
var
  vStream: TStringStream;
begin
  vStream := TStringStream.Create('');
  try
    Result := Obter(AURL, vStream);
    ATexto := vStream.DataString;
  finally
    vStream.Free;
  end;
end;

{$IFDEF MSWINDOWS}

function MensagemWinInet(ACodigo: DWORD): string;
var
  vBuf: array[0..511] of char;
  vTam: DWORD;
begin
  vTam := FormatMessageA(FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_IGNORE_INSERTS,
            Pointer(GetModuleHandle('wininet.dll')), ACodigo, 0, @vBuf[0],
            SizeOf(vBuf), nil);
  if vTam > 0 then
    Result := Trim(string(PChar(@vBuf[0])))
  else
    Result := SysErrorMessage(ACodigo);
  Result := Format(cmErroCodigo, [Result, ACodigo]);
end;

function TClienteHTTP.Obter(const AURL: string; ADestino: TStream): boolean;
var
  hNet, hUrl: HINTERNET;
  vPedido, vTexto: string;
  vBuf: array[0..65535] of byte;
  vLidos, vTam, vIdx, vCodigo: DWORD;
  vTotal, vAcumulado: int64;
  vCabBuf: array of char;
  vInt: integer;
begin
  Result := False;
  FStatus := 0;
  FErro := '';
  FResposta.Clear;

  vPedido := '';
  for vInt := 0 to Pred(FCabecalhos.Count) do
    vPedido := vPedido + FCabecalhos[vInt] + #13#10;

  hNet := InternetOpenA(PChar(AgenteHTTP), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if hNet = nil then
  begin
    FErro := MensagemWinInet(GetLastError);
    Exit;
  end;
  try
    if vPedido = '' then
      hUrl := InternetOpenUrlA(hNet, PChar(AURL), nil, 0,
                INTERNET_FLAG_RELOAD or INTERNET_FLAG_NO_CACHE_WRITE or
                INTERNET_FLAG_NO_UI or INTERNET_FLAG_NO_COOKIES, 0)
    else
      hUrl := InternetOpenUrlA(hNet, PChar(AURL), PChar(vPedido), Length(vPedido),
                INTERNET_FLAG_RELOAD or INTERNET_FLAG_NO_CACHE_WRITE or
                INTERNET_FLAG_NO_UI or INTERNET_FLAG_NO_COOKIES, 0);
    if hUrl = nil then
    begin
      FErro := MensagemWinInet(GetLastError);
      Exit;
    end;
    try
      vCodigo := 0;
      vTam := SizeOf(vCodigo);
      vIdx := 0;
      if HttpQueryInfoA(hUrl, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER,
                        @vCodigo, @vTam, @vIdx) then
        FStatus := vCodigo;

      SetLength(vCabBuf, 16384);
      vTam := Length(vCabBuf);
      vIdx := 0;
      if HttpQueryInfoA(hUrl, HTTP_QUERY_RAW_HEADERS_CRLF, @vCabBuf[0], @vTam,
                        @vIdx) then
      begin
        SetString(vTexto, PChar(@vCabBuf[0]), vTam);
        GuardarCabecalhos(vTexto);
      end;
      vTotal := StrToInt64Def(Cabecalho('Content-Length'), -1);

      vAcumulado := 0;
      repeat
        vLidos := 0;
        if not InternetReadFile(hUrl, @vBuf[0], SizeOf(vBuf), @vLidos) then
        begin
          FErro := MensagemWinInet(GetLastError);
          Exit;
        end;
        if vLidos > 0 then
        begin
          ADestino.WriteBuffer(vBuf[0], vLidos);
          Inc(vAcumulado, vLidos);
          Progresso(vAcumulado, vTotal);
        end;
      until vLidos = 0;

      Result := ((FStatus >= 200) and (FStatus < 300)) or (FStatus = 304);
      if not Result then
        FErro := 'HTTP ' + IntToStr(FStatus);
    finally
      InternetCloseHandle(hUrl);
    end;
  finally
    InternetCloseHandle(hNet);
  end;
end;

{$ELSE}

procedure TClienteHTTP.DadosRecebidos(Sender: TObject; const ContentLength,
  CurrentPos: int64);
begin
  Progresso(CurrentPos, ContentLength);
end;

function TClienteHTTP.Obter(const AURL: string; ADestino: TStream): boolean;
var
  vCli: TFPHTTPClient;
  vInt, vPos: integer;
begin
  Result := False;
  FStatus := 0;
  FErro := '';
  FResposta.Clear;
  vCli := TFPHTTPClient.Create(nil);
  try
    vCli.AllowRedirect := True;
    vCli.AddHeader('User-Agent', AgenteHTTP);
    for vInt := 0 to Pred(FCabecalhos.Count) do
    begin
      vPos := Pos(':', FCabecalhos[vInt]);
      if vPos > 0 then
        vCli.AddHeader(Trim(Copy(FCabecalhos[vInt], 1, vPos - 1)),
                       Trim(Copy(FCabecalhos[vInt], vPos + 1, MaxInt)));
    end;
    vCli.OnDataReceived := @DadosRecebidos;
    try
      vCli.HTTPMethod('GET', AURL, ADestino, []);
      FStatus := vCli.ResponseStatusCode;
      GuardarCabecalhos(vCli.ResponseHeaders.Text);
      Result := ((FStatus >= 200) and (FStatus < 300)) or (FStatus = 304);
      if not Result then
        FErro := 'HTTP ' + IntToStr(FStatus);
    except
      on E: Exception do
      begin
        FStatus := vCli.ResponseStatusCode;
        FErro := E.Message;
      end;
    end;
  finally
    vCli.Free;
  end;
end;

{$ENDIF}

end.
