program conferir;

{$mode ObjFPC}{$H+}

// "Compilar para conferir" (etapa M6).
//   conferir --testes
//   conferir --compilar [--samples-repo=<repo>] [--ral=<raiz do RAL 2.0>]
//            [--delphi=<raiz da IDE>] [--lazarus=<raiz do Lazarus>]
//            [--delphi-projeto=<rel>] [--lazarus-projeto=<rel>]
// --testes: leitura do primeiro erro de cada arquivo da saida do compilador e
// lista de IDEs a partir de recibos sinteticos (nao toca nos recibos reais).
// --compilar: extrai o pai de 677864d dos samples (git archive) para uma pasta
// temporaria, migra de 1.1 para 2.0, e compila um sample pelo Delphi e outro
// pelo Lazarus contra os fontes do RAL 2.0; o segundo Delphi parte do
// resultado ainda em memoria. Um erro de compilacao e relatado com o arquivo
// e a linha; nada e gravado fora de pastas temporarias.
//
// Compilar (da raiz do instalador):
//   fpc -B -MObjFPC -Sh -Fumigrador/src -Fusrc/classes -FUmigrador/lib/testes
//       -FEmigrador/lib/testes migrador/tests/conferir/conferir.lpr

uses
  Classes, SysUtils, md5,
  RALInst.IDE, RALMig.Tipos, RALMig.Motor, RALMig.Conferir,
  RALMig.Acoes.Projeto, RALMig.Acoes.Formulario, RALMig.Acoes.Codigo;

const
  REPO_PADRAO = 'D:\Projetos\Delphi\OpenSourceCommunityBrasil\PascalRAL\PascalRAL-Samples-2.0';
  RAL_PADRAO = 'D:\Projetos\Delphi\OpenSourceCommunityBrasil\PascalRAL\PascalRAL-2.0';
  DELPHI_PADRAO = 'D:\IDE\Embarcadero\Studio\37.0';
  LAZARUS_PADRAO = 'D:\IDE\Lazarus\4.8';
  DELPHI_PROJETO = 'Delphi/VCL/Basic Server/Indy/BasicServerIndy.dpr';
  LAZARUS_PROJETO = 'Lazarus/Console/ConsoleApplication/Console.lpi';
  COMMIT = '677864d';

var
  GFalhas: integer = 0;
  GRaiz, GRepo, GRal: string;

procedure Conferir(ACondicao: boolean; const ADescricao: string);
begin
  if ACondicao then
    WriteLn('  ok    ', ADescricao)
  else
  begin
    WriteLn('  FALHA ', ADescricao);
    Inc(GFalhas);
  end;
end;

function TemFlag(const ANome: string): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := 1 to ParamCount do
    if ParamStr(vInt) = '--' + ANome then
      Exit(True);
end;

function Opcao(const ANome, APadrao: string): string;
var
  vInt: integer;
begin
  Result := APadrao;
  for vInt := 1 to ParamCount do
    if Copy(ParamStr(vInt), 1, Length(ANome) + 3) = '--' + ANome + '=' then
      Result := Copy(ParamStr(vInt), Length(ANome) + 4, MaxInt);
end;

procedure Gravar(const ACaminho, ATexto: string);
var
  vFluxo: TFileStream;
begin
  ForceDirectories(ExtractFilePath(ACaminho));
  vFluxo := TFileStream.Create(ACaminho, fmCreate);
  try
    if ATexto <> '' then
      vFluxo.WriteBuffer(ATexto[1], Length(ATexto));
  finally
    vFluxo.Free;
  end;
end;

procedure ApagarPasta(const APasta: string);
var
  vBusca: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(APasta) + '*', faAnyFile, vBusca) = 0 then
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
        ApagarPasta(IncludeTrailingPathDelimiter(APasta) + vBusca.Name)
      else
      begin
        FileSetAttr(IncludeTrailingPathDelimiter(APasta) + vBusca.Name, 0);
        DeleteFile(IncludeTrailingPathDelimiter(APasta) + vBusca.Name);
      end;
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
  RemoveDir(APasta);
end;

procedure Extrair(const ACommit, ADestino: string);
var
  vComando: string;
begin
  ForceDirectories(ADestino);
  vComando := 'git -C "' + GRepo + '" archive ' + ACommit + ' | tar -x -C "' +
    ExcludeTrailingPathDelimiter(ADestino) + '"';
  {$IFDEF MSWINDOWS}
  ExecuteProcess(GetEnvironmentVariable('ComSpec'), '/c ' + vComando);
  {$ELSE}
  ExecuteProcess('/bin/sh', ['-c', vComando]);
  {$ENDIF}
end;

function NovoMotor(const APasta: string): TMotorMigracao;
begin
  Result := TMotorMigracao.Create;
  Result.Pasta := APasta;
  Result.De := '1.1';
  Result.Para := '2.0';
  Result.Idioma := 'pt-BR';
end;

{ --testes }

procedure TesteErros;
var
  vSaida: TStringList;
  vErros: string;
begin
  WriteLn('primeiro erro de cada arquivo');
  vSaida := TStringList.Create;
  try
    vSaida.Add('Embarcadero Delphi for Win32 compiler version 37.0');
    vSaida.Add('C:\tmp\Principal.pas(41) Error: E2003 Undeclared identifier: ''CompressType''');
    vSaida.Add('C:\tmp\Principal.pas(77) Error: E2003 Undeclared identifier: ''Outro''');
    vSaida.Add('C:\tmp\Console.dpr(12) Fatal: F2613 Unit ''RALIndyRegister'' not found.');
    vErros := PrimeiroErroPorArquivo(vSaida);
    Conferir(vErros = 'Principal.pas(41): E2003 Undeclared identifier: ''CompressType''' +
      LineEnding + 'Console.dpr(12): F2613 Unit ''RALIndyRegister'' not found.',
      'Delphi: um erro por arquivo, o primeiro');
    vSaida.Clear;
    vSaida.Add('unit1.pas(34,5) Error: Identifier not found "Foo"');
    vSaida.Add('unit1.pas(35,5) Error: Identifier not found "Bar"');
    vSaida.Add('Fatal: Compilation aborted');
    vErros := PrimeiroErroPorArquivo(vSaida);
    Conferir(vErros = 'unit1.pas(34): Identifier not found "Foo"' + LineEnding +
      'Compilation aborted', 'FPC: linha e coluna viram so a linha; erro sem arquivo vem puro');
    vSaida.Clear;
    vSaida.Add('Hint: nada');
    Conferir(PrimeiroErroPorArquivo(vSaida) = '', 'sem erro: vazio');
  finally
    vSaida.Free;
  end;
end;

procedure TesteRecibos;
var
  vPasta, vRecibos: string;
  vLista: TIDEsConferencia;
begin
  WriteLn('IDEs com o RAL a partir de recibos');
  vPasta := GRaiz + 'recibos' + PathDelim;
  vRecibos := vPasta;
  Gravar(vRecibos + 'delphi-37.0-a.json',
    '{"instalador":"RALInstaller","data":"2026-10-01T10:00:00","fontes":"' +
    StringReplace(GRal, '\', '\\', [rfReplaceAll]) + '","ide":{"tipo":"delphi",' +
    '"nome":"Delphi 13","raiz":"' + StringReplace(Opcao('delphi', DELPHI_PADRAO), '\', '\\',
    [rfReplaceAll]) + '"},"ral":{"versao":"2.0"},"pacotes":["win32 IndyRAL: ok"]}');
  vLista := TIDEsConferencia.Create;
  try
    if not DirectoryExists(Opcao('delphi', DELPHI_PADRAO)) then
    begin
      WriteLn('  pulado: nao ha IDE em ', Opcao('delphi', DELPHI_PADRAO));
      Exit;
    end;
    IDEsComRAL('2.0', vLista, vRecibos);
    Conferir(vLista.Count = 1, 'um recibo 2.0: uma IDE');
    if vLista.Count = 1 then
    begin
      Conferir(vLista[0].IDE.Tipo = tiDelphi, 'a IDE e Delphi: ' + vLista[0].IDE.Nome);
      Conferir(SameFileName(vLista[0].RaizRAL, IncludeTrailingPathDelimiter(GRal)),
        'os fontes sao os do recibo');
    end;
    vLista.Clear;
    IDEsComRAL('1.2', vLista, vRecibos);
    Conferir(vLista.Count = 0, 'outra versao: nenhuma IDE');
    vLista.Clear;
    IDEsComRAL('2.0', vLista, GRaiz + 'nao-existe');
    Conferir(vLista.Count = 0, 'sem recibos: nenhuma IDE');
    Conferir(AdicionarIDEManual(vLista, Opcao('delphi', DELPHI_PADRAO), GRal) <> nil,
      'IDE dada a mao (--ral): aceita');
    Conferir(AdicionarIDEManual(vLista, GRaiz, GRal) = nil, 'pasta que nao e IDE: recusada');
  finally
    vLista.Free;
  end;
end;

{ --compilar }

procedure Compilar(AIDE: TIDEInstance; const AProjeto, ARotulo: string);
var
  vErros: string;
  vOk: boolean;
  vInicio: QWord;
begin
  vInicio := GetTickCount64;
  vOk := CompilarProjeto(AIDE, AProjeto, GRal, vErros);
  WriteLn('  ', ARotulo, ': ', (GetTickCount64 - vInicio) div 1000, ' s');
  if vErros <> '' then
    WriteLn('        ', StringReplace(vErros, LineEnding, LineEnding + '        ', [rfReplaceAll]));
  Conferir(vOk, ARotulo + ' compila');
end;

/// md5 of the files of a configuration folder (not of its subfolders)
function FotoConfig(const APasta: string): string;
var
  vBusca: TSearchRec;
begin
  Result := '';
  if FindFirst(IncludeTrailingPathDelimiter(APasta) + '*', faAnyFile, vBusca) = 0 then
  try
    repeat
      if (vBusca.Attr and faDirectory) = 0 then
        Result := Result + vBusca.Name + '=' +
          MD5Print(MD5File(IncludeTrailingPathDelimiter(APasta) + vBusca.Name)) + LineEnding;
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
end;

/// How many temporary folders of this process are left
function PastasSobrando: integer;
var
  vBusca: TSearchRec;
begin
  Result := 0;
  if FindFirst(IncludeTrailingPathDelimiter(GetTempDir) + 'ralmig-conferir-' +
       IntToStr(GetProcessID) + '-*', faDirectory, vBusca) = 0 then
  try
    repeat
      Inc(Result);
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
end;

function LerTudo(const ACaminho: string): string;
var
  vLista: TStringList;
begin
  vLista := TStringList.Create;
  try
    vLista.LoadFromFile(ACaminho);
    Result := vLista.Text;
  finally
    vLista.Free;
  end;
end;

/// Puts a unit that does not exist in the uses of a migrated file: the
/// compilation must say so
procedure Estragar(const ACaminho, AAntes, ADepois: string);
var
  vTexto: string;
begin
  vTexto := LerTudo(ACaminho);
  Conferir(Pos(AAntes, vTexto) > 0, 'ponto de estrago achado em ' + ExtractFileName(ACaminho));
  Gravar(ACaminho, StringReplace(vTexto, AAntes, ADepois, []));
end;

procedure TesteComErro;
var
  vPasta, vErros, vDprojPas, vLpr: string;
  vLista: TIDEsConferencia;
  vCand: TIDEConferencia;
  vAntes: string;
begin
  WriteLn('uma unit que nao existe: a conferencia enxerga o erro');
  vErros := '';
  vPasta := GRaiz + 'samples' + PathDelim;
  vLista := TIDEsConferencia.Create;
  try
    vCand := AdicionarIDEManual(vLista, Opcao('delphi', DELPHI_PADRAO), GRal);
    if vCand <> nil then
    begin
      vDprojPas := vPasta + 'Delphi' + PathDelim + 'VCL' + PathDelim + 'Basic Server' +
        PathDelim + 'Indy' + PathDelim + 'Principal.pas';
      Estragar(vDprojPas, 'uses', 'uses RALNaoExiste,');
      Conferir(not CompilarProjeto(vCand.IDE, vPasta + StringReplace(
        Opcao('delphi-projeto', DELPHI_PROJETO), '/', PathDelim, [rfReplaceAll]), GRal, vErros),
        'Delphi: nao compila');
      WriteLn('        ', StringReplace(vErros, LineEnding, LineEnding + '        ', [rfReplaceAll]));
      Conferir(Pos('Principal.pas(', vErros) > 0, 'Delphi: o erro traz arquivo e linha');
      Conferir(Pos('RALNaoExiste', vErros) > 0, 'Delphi: o erro cita a unit');
    end;
    vCand := AdicionarIDEManual(vLista, Opcao('lazarus', LAZARUS_PADRAO), GRal);
    if vCand <> nil then
    begin
      vLpr := vPasta + StringReplace(ChangeFileExt(Opcao('lazarus-projeto', LAZARUS_PROJETO),
        '.lpr'), '/', PathDelim, [rfReplaceAll]);
      Estragar(vLpr, 'RALfpHTTPServer,', 'RALNaoExiste, RALfpHTTPServer,');
      vAntes := FotoConfig(vCand.IDE.ConfigDir);
      Conferir(not CompilarProjeto(vCand.IDE, vPasta + StringReplace(
        Opcao('lazarus-projeto', LAZARUS_PROJETO), '/', PathDelim, [rfReplaceAll]), GRal, vErros),
        'Lazarus: nao compila');
      WriteLn('        ', StringReplace(vErros, LineEnding, LineEnding + '        ', [rfReplaceAll]));
      Conferir(Pos('RALNaoExiste', vErros) > 0, 'Lazarus: o erro cita a unit');
      Conferir(FotoConfig(vCand.IDE.ConfigDir) = vAntes, 'a configuracao real do Lazarus nao mudou');
    end;
  finally
    vLista.Free;
  end;
  Conferir(PastasSobrando = 0, 'nenhuma pasta temporaria sobrou');
end;
procedure TesteCompilar;
var
  vPasta, vDelphi, vLazarus: string;
  vMotor: TMotorMigracao;
  vLista: TIDEsConferencia;
  vCand: TIDEConferencia;
  vErros: string;
begin
  vErros := '';
  WriteLn('compilar um sample migrado de 1.1 para 2.0');
  if not DirectoryExists(GRepo) then
  begin
    WriteLn('  pulado: nao ha repositorio em ', GRepo);
    Exit;
  end;
  vPasta := GRaiz + 'samples' + PathDelim;
  Extrair(COMMIT + '~1', vPasta);
  Conferir(DirectoryExists(vPasta + 'Delphi'), 'git archive extraiu o sample');
  vMotor := NovoMotor(vPasta);
  try
    vMotor.Simular;
    Conferir(vMotor.Resultado.Erros.Count = 0, 'migracao sem erro');
    Conferir(vMotor.Gravar(False), 'migracao gravada na pasta temporaria');
  finally
    vMotor.Free;
  end;
  vLista := TIDEsConferencia.Create;
  try
    vDelphi := Opcao('delphi', DELPHI_PADRAO);
    vLazarus := Opcao('lazarus', LAZARUS_PADRAO);
    vCand := AdicionarIDEManual(vLista, vDelphi, GRal);
    if vCand <> nil then
      Compilar(vCand.IDE, vPasta + StringReplace(Opcao('delphi-projeto', DELPHI_PROJETO), '/',
        PathDelim, [rfReplaceAll]), vCand.IDE.Nome + ' (dcc32)')
    else
      Conferir(False, 'IDE Delphi nao achada em ' + vDelphi);
    vCand := AdicionarIDEManual(vLista, vLazarus, GRal);
    if vCand <> nil then
      Compilar(vCand.IDE, vPasta + StringReplace(Opcao('lazarus-projeto', LAZARUS_PROJETO), '/',
        PathDelim, [rfReplaceAll]), vCand.IDE.Nome + ' (lazbuild)')
    else
      Conferir(False, 'IDE Lazarus nao achada em ' + vLazarus);
  finally
    vLista.Free;
  end;

  WriteLn('compilar o resultado ainda em memoria');
  vPasta := GRaiz + 'samples-mem' + PathDelim;
  Extrair(COMMIT + '~1', vPasta);
  vMotor := NovoMotor(vPasta);
  vLista := TIDEsConferencia.Create;
  try
    vMotor.Simular;
    vCand := AdicionarIDEManual(vLista, Opcao('delphi', DELPHI_PADRAO), GRal);
    if vCand <> nil then
    begin
      Conferir(CompilarConjunto(vCand.IDE, vMotor.Conjunto,
        vPasta + StringReplace(Opcao('delphi-projeto', DELPHI_PROJETO), '/', PathDelim,
        [rfReplaceAll]), GRal, vErros), 'Delphi: o conjunto em memoria compila');
      if vErros <> '' then
        WriteLn('        ', vErros);
    end;
  finally
    vLista.Free;
    vMotor.Free;
  end;
end;

begin
  DefaultSystemCodePage := CP_UTF8;
  if not (TemFlag('testes') or TemFlag('compilar')) then
  begin
    WriteLn('uso: conferir --testes | --compilar [opcoes]');
    Halt(2);
  end;
  GRepo := Opcao('samples-repo', REPO_PADRAO);
  GRal := ExcludeTrailingPathDelimiter(Opcao('ral', RAL_PADRAO));
  GRaiz := IncludeTrailingPathDelimiter(GetTempDir) + 'ralmig-conferir-teste-' +
           IntToStr(GetProcessID) + PathDelim;
  ApagarPasta(GRaiz);
  try
    if TemFlag('testes') then
    begin
      TesteErros;
      TesteRecibos;
    end;
    if TemFlag('compilar') then
    begin
      TesteCompilar;
      TesteComErro;
    end;
  finally
    ApagarPasta(GRaiz);
  end;
  if GFalhas > 0 then
  begin
    WriteLn(GFalhas, ' falha(s)');
    Halt(1);
  end;
  WriteLn('todos os testes passaram');
end.
