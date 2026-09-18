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

implementation

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
