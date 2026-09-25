/// Runs a process and captures what it says, LCL-free: the same code serves the
/// GUI, the CLI and the tests. The output is read while the process runs:
/// waiting for the end with the pipe full hangs the compiler, and a `dcc32 -B`
/// of a large package fills the pipe.
unit RALInst.Processo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process;

type
  /// Receives each output line, and the lines the installer itself writes.
  TLogLinha = procedure(const ALinha: string) of object;

  /// One external process run.
  TExecucao = class
  private
    FCodigoSaida: integer;
    FErro: string;
    FExecutavel: string;
    FLog: TLogLinha;
    FParametros: TStringList;
    FPastaTrabalho: string;
    FSaida: TStringList;
    /// Keeps the line in Saida and passes it to Log
    procedure Logar(const ALinha: string);
  public
    constructor Create;
    destructor Destroy; override;
    /// True when the process ran and returned exit code 0
    function Executar: boolean;
    /// The command line as it would be typed, for the log and the report
    function LinhaComando: string;

    property CodigoSaida: integer read FCodigoSaida;
    /// Set when the process could not even start (missing executable)
    property Erro: string read FErro;
    property Executavel: string read FExecutavel write FExecutavel;
    property Log: TLogLinha read FLog write FLog;
    property Parametros: TStringList read FParametros;
    property PastaTrabalho: string read FPastaTrabalho write FPastaTrabalho;
    property Saida: TStringList read FSaida;
  end;

var
  /// Called while a process runs with no new output: the GUI processes its
  /// messages here, so the window does not freeze during a 30 s dcc32
  AoEsperarProcesso: procedure = nil;

/// On Windows, the path with the \\?\ prefix when it passes the 260-character
/// limit (mORMot2 examples and Zeos documentation do, in a slightly deep
/// installation folder); elsewhere, the path itself
function CaminhoLongo(const ACaminho: string): string;
/// Quotes only when needed, the way the Windows cmd understands
function CitarArgumento(const AArgumento: string): string;
/// ForceDirectories that accepts long paths
function CriarPastas(const APasta: string): boolean;
/// Shortcut: runs and returns True if the exit code was 0
function Executar(const AExecutavel: string; AParametros: TStrings;
  ALog: TLogLinha; const APastaTrabalho: string = ''): boolean;
/// Where the installer keeps cache, receipts and logs; the same for the
/// installer and the test tools, whatever the executable name
function PastaDadosInstalador: string;
/// Is a process with one of these executable names ('bds.exe', 'lazarus.exe')
/// running from APasta? Writing to the configuration of an open IDE is lost: it
/// rewrites everything when it closes. Without access to the process path it
/// counts as open (better to refuse for nothing than to lose the
/// installation). Outside Windows and Linux there is no way to know: False
function ProgramaEmExecucao(const ANomes: array of string;
  const APasta: string): boolean;

implementation

uses
  {$IFDEF MSWINDOWS} Windows, JwaTlHelp32, {$ENDIF}
  {$IFDEF LINUX} BaseUnix, {$ENDIF}
  RALInst.Mensagens;

{$IFDEF MSWINDOWS}
const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;

function QueryFullProcessImageNameW(hProcess: THandle; dwFlags: DWORD;
  lpExeName: PWideChar; var lpdwSize: DWORD): BOOL; stdcall;
  external 'kernel32' name 'QueryFullProcessImageNameW';
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

function ProgramaEmExecucao(const ANomes: array of string;
  const APasta: string): boolean;
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
        vProc := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False,
                             vEntrada.th32ProcessID);
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
  if (AArgumento <> '') and (Pos(' ', AArgumento) = 0) and
     (Pos('"', AArgumento) = 0) then
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
        Logar(Format(emExecutar, [FExecutavel, E.Message]));
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
