program motor;

{$mode ObjFPC}{$H+}

// Motor de migracao (etapa M2).
//   motor --testes
// Trabalha numa pasta temporaria sintetica; nunca toca em projeto de verdade.
// As regras e as APIs vem de migrador/regras e migrador/api (achadas subindo
// do executavel). Erros de "acao sem implementacao" (formulario e codigo, de
// outras etapas) sao esperados enquanto aquelas units nao existem e sao
// filtrados.
//
// Compilar (da raiz do instalador):
//   fpc -B -MObjFPC -Sh -Fumigrador/src -Fusrc/classes -FUmigrador/lib/testes
//       -FEmigrador/lib/testes migrador/tests/motor/motor.lpr

uses
  Classes, SysUtils, md5,
  RALMig.Regras, RALMig.Tipos, RALMig.Motor, RALMig.Deteccao, RALMig.Copia,
  RALMig.Acoes.Projeto;

var
  GFalhas: integer = 0;
  GRaiz: string;

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

{ Helpers }

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

function Ler(const ACaminho: string): string;
var
  vFluxo: TFileStream;
begin
  Result := '';
  vFluxo := TFileStream.Create(ACaminho, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, vFluxo.Size);
    if vFluxo.Size > 0 then
      vFluxo.ReadBuffer(Result[1], vFluxo.Size);
  finally
    vFluxo.Free;
  end;
end;

procedure Varrer(const APasta, ARel: string; ADest: TStringList; ASemCopias: boolean);
var
  vBusca: TSearchRec;
begin
  if FindFirst(APasta + ARel + '*', faAnyFile, vBusca) <> 0 then
    Exit;
  try
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
      begin
        if not (ASemCopias and (vBusca.Name = '_ral-migracao')) then
          Varrer(APasta, ARel + vBusca.Name + '/', ADest, ASemCopias);
      end
      else
        ADest.Add(ARel + vBusca.Name + '=' + MD5Print(MD5File(APasta + ARel + vBusca.Name)));
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
end;

/// path=md5 of every file under the folder, sorted
function Foto(const APasta: string; ASemCopias: boolean): string;
var
  vLista: TStringList;
begin
  vLista := TStringList.Create;
  try
    vLista.Sorted := True;
    Varrer(IncludeTrailingPathDelimiter(APasta), '', vLista, ASemCopias);
    Result := vLista.Text;
  finally
    vLista.Free;
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
        DeleteFile(IncludeTrailingPathDelimiter(APasta) + vBusca.Name);
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
  RemoveDir(APasta);
end;

const
  DPROJ =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#10 +
    '    <PropertyGroup Condition="''$(Base)''!=''''">'#10 +
    '        <DCC_UsePackage>rtl;RALDBFireDACLink;PascalRALDsgn;PascalRAL;$(DCC_UsePackage)</DCC_UsePackage>'#10 +
    '        <DCC_UnitSearchPath>$(PascalRAL)\src\base;$(PascalRAL)\src\utils;$(DCC_UnitSearchPath)</DCC_UnitSearchPath>'#10 +
    '    </PropertyGroup>'#10 +
    '</Project>'#10;
  DPROJ_ESPERADO_PACOTES =
    '<DCC_UsePackage>rtl;RALDBFireDACPackage;PascalRAL;RALStoragePackage;$(DCC_UsePackage)</DCC_UsePackage>';
  DPROJ_ESPERADO_CAMINHOS =
    '<DCC_UnitSearchPath>$(PascalRAL)\src\core;$(PascalRAL)\src\common;' +
    '$(PascalRAL)\src\storage;$(PascalRAL)\src\compress;$(PascalRAL)\src\crypto;' +
    '$(DCC_UnitSearchPath)</DCC_UnitSearchPath>';
  DPR =
    'program p;'#10#10'uses'#10'  RALIndyRegister,'#10'  Unit1 in ''Unit1.pas'';'#10#10 +
    'begin'#10'end.'#10;
  UNIT1 =
    'unit Unit1;'#10#10'interface'#10#10'uses'#10'  SysUtils, RALDBTypes;'#10#10 +
    'implementation'#10#10'end.'#10;
  LPI =
    '<?xml version="1.0" encoding="UTF-8"?>'#10'<CONFIG>'#10'  <ProjectOptions>'#10 +
    '    <RequiredPackages Count="2">'#10'      <Item1>'#10'        <PackageName Value="raldbzeoslink"/>'#10 +
    '      </Item1>'#10'      <Item2>'#10'        <PackageName Value="pascalral"/>'#10 +
    '      </Item2>'#10'    </RequiredPackages>'#10'  </ProjectOptions>'#10'</CONFIG>'#10;
  LPR =
    'program q;'#10#10'uses'#10'  raldbzeoslink, pascalral, Unit1;'#10#10'begin'#10'end.'#10;

procedure CriarProjeto(const APasta: string);
begin
  Gravar(APasta + 'p.dproj', DPROJ);
  Gravar(APasta + 'p.dpr', DPR);
  Gravar(APasta + 'Unit1.pas', UNIT1);
  Gravar(APasta + 'lazarus' + PathDelim + 'Unit1.pas', UNIT1);
  Gravar(APasta + 'lazarus' + PathDelim + 'q.lpi', LPI);
  Gravar(APasta + 'lazarus' + PathDelim + 'q.lpr', LPR);
end;

function NovoMotor(const APasta, ADe, APara: string): TMotorMigracao;
begin
  Result := TMotorMigracao.Create;
  Result.Pasta := APasta;
  Result.De := ADe;
  Result.Para := APara;
  Result.Idioma := 'pt-BR';
end;

/// Errors that are not "action without implementation"
function ErrosReais(AMotor: TMotorMigracao): integer;
var
  vInt: integer;
begin
  Result := 0;
  for vInt := 0 to AMotor.Resultado.Erros.Count - 1 do
    if Pos('implementa', AMotor.Resultado.Erros[vInt]) = 0 then
    begin
      WriteLn('        erro: ', AMotor.Resultado.Erros[vInt]);
      Inc(Result);
    end;
end;

function Migrar(const APasta, ADe, APara: string; ACopia: boolean): boolean;
var
  vMotor: TMotorMigracao;
begin
  vMotor := NovoMotor(APasta, ADe, APara);
  try
    vMotor.Simular;
    Result := (ErrosReais(vMotor) = 0) and vMotor.Gravar(ACopia);
  finally
    vMotor.Free;
  end;
end;

{ Testes }

procedure TesteCadeia;
var
  vA, vB: string;
begin
  WriteLn('cadeia 1.1 -> 2.0');
  vA := GRaiz + 'cadeia-a' + PathDelim;
  vB := GRaiz + 'cadeia-b' + PathDelim;
  CriarProjeto(vA);
  CriarProjeto(vB);
  Conferir(Migrar(vA, '1.1', '2.0', False), 'cadeia direta roda sem erro');
  Conferir(Migrar(vB, '1.1', '1.2', False), 'passo 1.1 -> 1.2 roda');
  Conferir(Migrar(vB, '1.2', '2.0', False), 'passo 1.2 -> 2.0 roda');
  Conferir(Foto(vA, False) = Foto(vB, False), 'a cadeia direta = os passos um a um');
  Conferir(Pos(DPROJ_ESPERADO_PACOTES, Ler(vA + 'p.dproj')) > 0, 'DCC_UsePackage esperado');
  Conferir(Pos(DPROJ_ESPERADO_CAMINHOS, Ler(vA + 'p.dproj')) > 0, 'DCC_UnitSearchPath esperado');
  if Pos(DPROJ_ESPERADO_CAMINHOS, Ler(vA + 'p.dproj')) = 0 then
    WriteLn(Ler(vA + 'p.dproj'));
  Conferir(Pos('RALIndyReg,', Ler(vA + 'p.dpr')) > 0, 'unit do .dpr renomeada');
  Conferir(Pos('RALStorageTypes', Ler(vA + 'Unit1.pas')) > 0, 'unit do .pas renomeada');
  Conferir(Pos('raldbzeospackage', Ler(vA + 'lazarus' + PathDelim + 'q.lpi')) > 0, '.lpi: pacote renomeado');
  Conferir(Pos('ralstoragepackage', Ler(vA + 'lazarus' + PathDelim + 'q.lpi')) > 0, '.lpi: pacote exigido');
  Conferir(Pos('raldbzeospackage, pascalral', Ler(vA + 'lazarus' + PathDelim + 'q.lpr')) > 0, '.lpr: uses renomeado');
end;

procedure TesteIdempotente;
var
  vPasta: string;
  vMotor: TMotorMigracao;
  vLista: TList;
begin
  WriteLn('idempotencia');
  vPasta := GRaiz + 'cadeia-a' + PathDelim;
  vMotor := NovoMotor(vPasta, '1.1', '2.0');
  vLista := TList.Create;
  try
    vMotor.Simular;
    vMotor.Conjunto.ListarAlterados(vLista);
    Conferir(vLista.Count = 0, 'a segunda rodada nao muda arquivo nenhum');
    Conferir(vMotor.Resultado.FeitoCount = 0, 'a segunda rodada nao tem item feito');
  finally
    vLista.Free;
    vMotor.Free;
  end;
end;

procedure TesteAcaoDesconhecida;
var
  vRaiz: string;
  vMotor: TMotorMigracao;
begin
  WriteLn('acao desconhecida');
  vRaiz := GRaiz + 'raiz-ral' + PathDelim;
  Gravar(vRaiz + 'ralmigracao.json',
    '{"formato":1,"origem":"RAL","de":"1.1","para":"1.2","regras":[{"id":"1.2/x",' +
    '"acao":"apagar-tudo","onde":["pas"],"descricao":"x","manual":"x"}]}');
  vMotor := NovoMotor(GRaiz + 'cadeia-a', '1.1', '2.0');
  try
    vMotor.RaizRAL := vRaiz;
    Conferir(not vMotor.CarregarRegras, 'ralmigracao.json com acao desconhecida e recusado');
    Conferir(Pos('apagar-tudo', vMotor.Regras.Erros.Text) > 0, 'a mensagem cita a acao');
    vMotor.Simular;
    Conferir(vMotor.Resultado.Erros.Count > 0, 'Simular informa o erro');
    Conferir(vMotor.CodigoSaida = SAIDA_ERRO, 'codigo de saida 2');
  finally
    vMotor.Free;
  end;
end;

procedure TesteRetrocesso;
var
  vPasta, vAntes: string;
  vMotor: TMotorMigracao;
begin
  WriteLn('2.0 -> 1.1');
  vPasta := GRaiz + 'cadeia-a' + PathDelim;
  vAntes := Foto(vPasta, False);
  vMotor := NovoMotor(vPasta, '2.0', '1.1');
  try
    vMotor.Simular;
    Conferir(vMotor.Recusado, 'recusado (D-M8)');
    Conferir(vMotor.CodigoSaida = SAIDA_RECUSADO, 'codigo de saida 3');
    Conferir(not vMotor.Gravar(False) or (Foto(vPasta, False) = vAntes), 'nada gravado');
    Conferir(Foto(vPasta, False) = vAntes, 'a pasta ficou igual');
  finally
    vMotor.Free;
  end;
end;

procedure TesteCopiaDesfazer;
var
  vPasta, vAntes, vZip, vErro: string;
  vMotor: TMotorMigracao;
begin
  WriteLn('copia de seguranca e desfazer');
  vPasta := GRaiz + 'copia' + PathDelim;
  CriarProjeto(vPasta);
  vAntes := Foto(vPasta, True);
  vMotor := NovoMotor(vPasta, '1.1', '2.0');
  try
    vMotor.Simular;
    Conferir(ErrosReais(vMotor) = 0, 'simula sem erro real');
    Conferir(Foto(vPasta, True) = vAntes, 'Simular nao grava');
    Conferir(vMotor.Gravar(True), 'Gravar(com copia)');
    vZip := vMotor.ZipCopia;
  finally
    vMotor.Free;
  end;
  Conferir(FileExists(vZip), 'o zip da copia existe');
  Conferir(Foto(vPasta, True) <> vAntes, 'os arquivos mudaram');
  Conferir(Desfazer(vZip, vPasta, nil, vErro), 'Desfazer ' + vErro);
  Conferir(Foto(vPasta, True) = vAntes, 'a pasta voltou identica');
end;

procedure TesteDeteccao;
var
  vEvid: TStrings;
  vVersao: string;
  vMotor: TMotorMigracao;
begin
  WriteLn('deteccao');
  vMotor := NovoMotor(GRaiz + 'cadeia-b', '', '');
  try
    vVersao := vMotor.Detectar(vEvid);
    try
      Conferir(vVersao = '2.0', 'projeto ja migrado: ' + vVersao);
      Conferir(vEvid.Count > 0, 'ha evidencias');
    finally
      vEvid.Free;
    end;
  finally
    vMotor.Free;
  end;
  vMotor := NovoMotor(GRaiz + 'copia', '', '');
  try
    Gravar(GRaiz + 'vazia' + PathDelim + 'x.pas', 'unit x; interface implementation end.');
    vMotor.Pasta := GRaiz + 'vazia';
    vVersao := vMotor.Detectar(vEvid);
    try
      Conferir(vVersao = '', 'sem pista: versao vazia');
    finally
      vEvid.Free;
    end;
  finally
    vMotor.Free;
  end;
end;

begin
  if Pos('--testes', CmdLine) = 0 then
  begin
    WriteLn('uso: motor --testes');
    Halt(2);
  end;
  GRaiz := IncludeTrailingPathDelimiter(GetTempDir) + 'ralmig-motor-' +
           IntToStr(GetProcessID) + PathDelim;
  ApagarPasta(GRaiz);
  try
    TesteCadeia;
    TesteIdempotente;
    TesteAcaoDesconhecida;
    TesteRetrocesso;
    TesteCopiaDesfazer;
    TesteDeteccao;
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
