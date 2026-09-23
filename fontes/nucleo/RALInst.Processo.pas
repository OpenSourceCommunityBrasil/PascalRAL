unit RALInst.Processo;

{$mode ObjFPC}{$H+}

// Executar um processo e ver o que ele disse, sem LCL: o mesmo codigo serve a
// GUI, a CLI e aos testes.
//
// A saida e lida enquanto o processo roda. Esperar o fim com o pipe cheio
// trava o compilador — e um `dcc32 -B` de pacote grande enche o pipe.

interface

uses
  Classes, SysUtils, Process;

type
  // cada linha de saida, e as linhas que o proprio instalador escreve
  TLogLinha = procedure(const ALinha: string) of object;

  { TExecucao }

  TExecucao = class
  private
    FExecutavel: string;
    FParametros: TStringList;
    FPastaTrabalho: string;
    FLog: TLogLinha;
    FSaida: TStringList;
    FCodigoSaida: integer;
    FErro: string;
    procedure Logar(const ALinha: string);
  public
    constructor Create;
    destructor Destroy; override;

    // True quando o processo rodou e devolveu codigo 0
    function Executar: boolean;
    // a linha de comando como seria digitada, para o log e para o dossiê
    function LinhaComando: string;

    property Executavel: string read FExecutavel write FExecutavel;
    property Parametros: TStringList read FParametros;
    property PastaTrabalho: string read FPastaTrabalho write FPastaTrabalho;
    property Log: TLogLinha read FLog write FLog;
    property Saida: TStringList read FSaida;
    property CodigoSaida: integer read FCodigoSaida;
    // preenchido quando nem deu para executar (executavel inexistente)
    property Erro: string read FErro;
  end;

var
  // chamado enquanto o processo roda sem saida nova: a GUI processa as
  // mensagens aqui, e a janela nao congela durante um dcc32 de 30 segundos
  AoEsperarProcesso: procedure = nil;

// atalho: executa e devolve True se o codigo de saida foi 0
function Executar(const AExecutavel: string; AParametros: TStrings;
  ALog: TLogLinha; const APastaTrabalho: string = ''): boolean;

// aspas so quando precisa, do jeito que o cmd do Windows entende
function CitarArgumento(const AArgumento: string): string;

// onde o instalador guarda cache, recibos e logs; a mesma para o instalador e
// para as ferramentas de teste, qualquer que seja o nome do executavel
function PastaDadosInstalador: string;

// no Windows, o caminho com o prefixo \\?\ quando passa do limite de 260
// caracteres (os exemplos do mORMot2 e a documentacao do Zeos passam, numa
// pasta de instalacao um pouco funda); nos outros sistemas, o proprio caminho
function CaminhoLongo(const ACaminho: string): string;
// ForceDirectories que aceita caminho longo
function CriarPastas(const APasta: string): boolean;

// algum processo com um destes nomes de executavel ('bds.exe', 'lazarus.exe')
// roda a partir de APasta? Escrever na configuracao de uma IDE aberta e
// perdido: ela regrava tudo ao fechar. Sem acesso ao caminho do processo,
// conta como aberta — melhor recusar a toa do que perder a instalacao. Fora
// do Windows e do Linux, nao ha como saber: False
function ProgramaEmExecucao(const ANomes: array of string; const APasta: string): boolean;

implementation

{$IFDEF MSWINDOWS}
uses
  Windows, JwaTlHelp32;

const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;

function QueryFullProcessImageNameW(hProcess: THandle; dwFlags: DWORD;
  lpExeName: PWideChar; var lpdwSize: DWORD): BOOL; stdcall;
  external 'kernel32' name 'QueryFullProcessImageNameW';
{$ENDIF}
{$IFDEF LINUX}
uses
  BaseUnix;
{$ENDIF}

function MesmaPastaProc(const A, B: string): boolean;
begin
  Result := SameFileName(IncludeTrailingPathDelimiter(ExpandFileName(A)),
                         IncludeTrailingPathDelimiter(ExpandFileName(B)));
end;

function NomeNaLista(const ANome: string; const ANomes: array of string): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := Low(ANomes) to High(ANomes) do
    if SameFileName(ANome, ANomes[vInt]) then
      Exit(True);
end;

function ProgramaEmExecucao(const ANomes: array of string; const APasta: string): boolean;
{$IFDEF MSWINDOWS}
var
  vSnap, vProc: THandle;
  vEntrada: TProcessEntry32W;
  vCaminho: string;
  vBuf: array[0..MAX_PATH] of WideChar;
  vTam: DWORD;
begin
  Result := False;
  vSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if vSnap = INVALID_HANDLE_VALUE then
    Exit;
  try
    vEntrada.dwSize := SizeOf(vEntrada);
    if not Process32FirstW(vSnap, vEntrada) then
      Exit;
    repeat
      if NomeNaLista(UTF8Encode(WideString(vEntrada.szExeFile)), ANomes) then
      begin
        vCaminho := '';
        vProc := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, vEntrada.th32ProcessID);
        if vProc <> 0 then
        try
          vTam := Length(vBuf);
          if QueryFullProcessImageNameW(vProc, 0, @vBuf[0], vTam) then
            vCaminho := UTF8Encode(WideString(Copy(vBuf, 0, vTam)));
        finally
          CloseHandle(vProc);
        end;
        if (vCaminho = '') or MesmaPastaProc(ExtractFilePath(vCaminho), APasta) then
          Exit(True);
      end;
    until not Process32NextW(vSnap, vEntrada);
  finally
    CloseHandle(vSnap);
  end;
end;
{$ELSE}
{$IFDEF LINUX}
var
  vBusca: TSearchRec;
  vExe: string;
begin
  Result := False;
  // /proc/<pid>/exe aponta para o executavel de cada processo
  if FindFirst('/proc/*', faDirectory, vBusca) = 0 then
  try
    repeat
      if StrToIntDef(vBusca.Name, -1) < 0 then
        Continue;
      vExe := fpReadLink('/proc/' + vBusca.Name + '/exe');
      if (vExe <> '') and NomeNaLista(ExtractFileName(vExe), ANomes) and
         MesmaPastaProc(ExtractFilePath(vExe), APasta) then
        Exit(True);
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}
{$ENDIF}

function PastaDadosInstalador: string;
begin
  Result := ExtractFilePath(ExcludeTrailingPathDelimiter(GetAppConfigDir(False))) +
            'RALInstaller' + PathDelim;
end;

function CaminhoLongo(const ACaminho: string): string;
begin
  Result := ACaminho;
  {$IFDEF MSWINDOWS}
  if (Length(Result) < 240) or (Copy(Result, 1, 4) = '\\?\') then
    Exit;
  Result := ExpandFileName(Result);
  if Copy(Result, 1, 2) = '\\' then
    Result := '\\?\UNC\' + Copy(Result, 3, MaxInt)
  else
    Result := '\\?\' + Result;
  {$ENDIF}
end;

function CriarPastas(const APasta: string): boolean;
{$IFDEF MSWINDOWS}
var
  vPasta, vParte, vAtual: string;
  vPos: integer;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  vPasta := ExcludeTrailingPathDelimiter(ExpandFileName(APasta));
  if Length(vPasta) < 240 then
    Exit(ForceDirectories(vPasta));
  // a parte curta pelo caminho normal; o resto, um nivel por vez, com \\?\
  vAtual := '';
  vParte := vPasta;
  Result := True;
  repeat
    vPos := Pos(PathDelim, vParte);
    if vPos = 0 then
      vPos := Length(vParte) + 1;
    vAtual := vAtual + Copy(vParte, 1, vPos - 1);
    Delete(vParte, 1, vPos);
    if (Length(vAtual) > 3) and not DirectoryExists(CaminhoLongo(vAtual)) then
      if not CreateDir(CaminhoLongo(vAtual)) then
        Exit(False);
    vAtual := vAtual + PathDelim;
  until vParte = '';
  {$ELSE}
  Result := ForceDirectories(APasta);
  {$ENDIF}
end;

function CitarArgumento(const AArgumento: string): string;
begin
  if (AArgumento <> '') and (Pos(' ', AArgumento) = 0) and (Pos('"', AArgumento) = 0) then
    Result := AArgumento
  else
    Result := '"' + StringReplace(AArgumento, '"', '""', [rfReplaceAll]) + '"';
end;

{ TExecucao }

constructor TExecucao.Create;
begin
  inherited Create;
  FParametros := TStringList.Create;
  FSaida := TStringList.Create;
  FCodigoSaida := -1;
end;

destructor TExecucao.Destroy;
begin
  FParametros.Free;
  FSaida.Free;
  inherited Destroy;
end;

procedure TExecucao.Logar(const ALinha: string);
begin
  FSaida.Add(ALinha);
  if Assigned(FLog) then
    FLog(ALinha);
end;

function TExecucao.LinhaComando: string;
var
  vInt: integer;
begin
  Result := CitarArgumento(FExecutavel);
  for vInt := 0 to Pred(FParametros.Count) do
    Result := Result + ' ' + CitarArgumento(FParametros[vInt]);
end;

function TExecucao.Executar: boolean;
var
  vProcesso: TProcess;
  vBuf: array[0..8191] of char;
  vLidos, vPos: integer;
  vPendente, vTexto: string;

  procedure DespejarLinhas(AFinal: boolean);
  begin
    vPos := Pos(#10, vPendente);
    while vPos > 0 do
    begin
      Logar(TrimRight(Copy(vPendente, 1, vPos - 1)));
      Delete(vPendente, 1, vPos);
      vPos := Pos(#10, vPendente);
    end;
    if AFinal and (Trim(vPendente) <> '') then
    begin
      Logar(TrimRight(vPendente));
      vPendente := '';
    end;
  end;

begin
  Result := False;
  FSaida.Clear;
  FErro := '';
  FCodigoSaida := -1;
  vPendente := '';

  if Assigned(FLog) then
    FLog('> ' + LinhaComando);

  vProcesso := TProcess.Create(nil);
  try
    try
      vProcesso.Executable := FExecutavel;
      vProcesso.Parameters.Assign(FParametros);
      if FPastaTrabalho <> '' then
        vProcesso.CurrentDirectory := ExcludeTrailingPathDelimiter(FPastaTrabalho);
      vProcesso.Options := [poUsePipes, poStderrToOutPut];
      vProcesso.ShowWindow := swoHIDE;
      vProcesso.Execute;

      while vProcesso.Running or (vProcesso.Output.NumBytesAvailable > 0) do
      begin
        if vProcesso.Output.NumBytesAvailable > 0 then
        begin
          vLidos := vProcesso.Output.Read(vBuf, SizeOf(vBuf));
          SetString(vTexto, vBuf, vLidos);
          vPendente := vPendente + vTexto;
          DespejarLinhas(False);
        end
        else
        begin
          if Assigned(AoEsperarProcesso) then
            AoEsperarProcesso;
          Sleep(15);
        end;
      end;
      DespejarLinhas(True);

      FCodigoSaida := vProcesso.ExitStatus;
      Result := FCodigoSaida = 0;
    except
      on E: Exception do
      begin
        FErro := E.Message;
        Logar('ERRO: não foi possível executar ' + FExecutavel + ': ' + E.Message);
      end;
    end;
  finally
    vProcesso.Free;
  end;
end;

function Executar(const AExecutavel: string; AParametros: TStrings;
  ALog: TLogLinha; const APastaTrabalho: string): boolean;
var
  vExec: TExecucao;
begin
  vExec := TExecucao.Create;
  try
    vExec.Executavel := AExecutavel;
    vExec.Parametros.Assign(AParametros);
    vExec.PastaTrabalho := APastaTrabalho;
    vExec.Log := ALog;
    Result := vExec.Executar;
  finally
    vExec.Free;
  end;
end;

end.
