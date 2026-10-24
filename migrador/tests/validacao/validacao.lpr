program validacao;

{$mode ObjFPC}{$H+}

// Validacao do resultado da migracao (etapa M6).
//   validacao --testes
//   validacao --samples [--samples-repo=<repo PascalRAL-Samples-2.0>]
// --testes: projeto sintetico com os erros que a IDE acusaria (propriedade da
// 1.2 que nenhuma regra tirou, classe, unit e pacote inexistentes) e com
// coisas que nao sao do RAL (TButton), contra a API 2.0 embutida/em
// migrador/api.
// --samples: extrai o pai de 677864d do repositorio dos samples (git archive),
// migra de 1.1 para 2.0 em memoria (nada e gravado) e exige ZERO itens de
// validacao; valida tambem o 677864d direto (ja migrado a mao) e relata.
// Sempre em pastas temporarias; nunca grava nos samples.
//
// Compilar (da raiz do instalador):
//   fpc -B -MObjFPC -Sh -Fumigrador/src -Fusrc/classes -FUmigrador/lib/testes
//       -FEmigrador/lib/testes migrador/tests/validacao/validacao.lpr

uses
  Classes, SysUtils,
  RALMig.API, RALMig.Tipos, RALMig.Projeto, RALMig.Motor, RALMig.Deteccao,
  RALMig.Validacao, RALMig.Acoes.Projeto, RALMig.Acoes.Formulario,
  RALMig.Acoes.Codigo;

const
  REPO_PADRAO = 'D:\Projetos\Delphi\OpenSourceCommunityBrasil\PascalRAL\PascalRAL-Samples-2.0';
  COMMIT = '677864d';

var
  GFalhas: integer = 0;
  GRaiz: string;
  GRepo: string;

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

/// The manual items of the rule validacao whose problem has the text
function Achou(AResultado: TResultadoMigracao; const AArquivo: string; ALinha: integer;
  const ATexto: string): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := 0 to AResultado.ManualCount - 1 do
    with AResultado.Manuais[vInt] do
      if (Regra = REGRA_VALIDACAO) and SameText(Arquivo, AArquivo) and
         ((ALinha = 0) or (Linha = ALinha)) and (Pos(ATexto, Problema) > 0) then
        Exit(True);
end;

function ContarValidacao(AResultado: TResultadoMigracao): integer;
var
  vInt: integer;
begin
  Result := 0;
  for vInt := 0 to AResultado.ManualCount - 1 do
    if AResultado.Manuais[vInt].Regra = REGRA_VALIDACAO then
      Inc(Result);
end;

procedure ListarValidacao(AResultado: TResultadoMigracao);
var
  vInt: integer;
begin
  for vInt := 0 to AResultado.ManualCount - 1 do
    with AResultado.Manuais[vInt] do
      if Regra = REGRA_VALIDACAO then
        WriteLn('        ', Arquivo, ':', Linha, ': ', Problema);
end;

{ Testes sinteticos }

const
  DFM =
    'object Form1: TForm1'#10 +                                   // 1
    '  Left = 0'#10 +                                             // 2
    '  object Srv: TRALIndyServer'#10 +                           // 3
    '    CompressType = ctNone'#10 +                              // 4
    '    Port = 8000'#10 +                                        // 5
    '    Name2 = 1'#10 +                                          // 6
    '    IPConfig.IPv6Enabled = False'#10 +                       // 7
    '    IPConfig.Nada = 1'#10 +                                  // 8
    '    Left = 10'#10 +                                          // 9
    '  end'#10 +                                                  // 10
    '  object Btn: TButton'#10 +                                  // 11
    '    Caption = ''x'''#10 +                                    // 12
    '    ProprioDoBotao = 1'#10 +                                 // 13
    '  end'#10 +                                                  // 14
    '  object Fantasma: TRALNaoExiste'#10 +                       // 15
    '    Qualquer = 1'#10 +                                       // 16
    '  end'#10 +                                                  // 17
    '  object Ok: TRALClient'#10 +                                // 18
    '    Name = ''x'''#10 +                                       // 19
    '  end'#10 +                                                  // 20
    '  object Meu: TRALMeu'#10 +                                  // 21
    '    Qualquer = 1'#10 +                                       // 22
    '  end'#10 +                                                  // 23
    'end'#10;
  PAS =
    'unit Unit1;'#10#10'interface'#10#10'uses'#10 +                // 1-5
    '  SysUtils, RALIndyRegister, RALIndyServer, Unit9;'#10#10 +  // 6-7
    'type'#10'  TRALMeu = class(TObject)'#10'  end;'#10#10 +
    'implementation'#10#10'uses RALDBFireDACLinkReg;'#10#10'end.'#10;
  DPROJ =
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#10 +
    '    <PropertyGroup>'#10 +
    '        <DCC_UsePackage>rtl;vcl;RALDBFireDACLink;PascalRAL;IndyRAL;Outro;$(DCC_UsePackage)</DCC_UsePackage>'#10 +
    '    </PropertyGroup>'#10 +
    '</Project>'#10;
  LPI =
    '<?xml version="1.0" encoding="UTF-8"?>'#10'<CONFIG>'#10'  <ProjectOptions>'#10 +
    '    <RequiredPackages Count="3">'#10'      <Item1>'#10'        <PackageName Value="raldbzeoslink"/>'#10 +
    '      </Item1>'#10'      <Item2>'#10'        <PackageName Value="LCL"/>'#10 +
    '      </Item2>'#10'      <Item3>'#10'        <PackageName Value="PascalRAL"/>'#10 +
    '      </Item3>'#10'    </RequiredPackages>'#10'  </ProjectOptions>'#10'</CONFIG>'#10;
  DPK =
    'package P;'#10#10'requires'#10'  rtl,'#10'  RALDBFireDACLink,'#10'  PascalRAL;'#10#10 +
    'contains'#10'  Unit1 in ''Unit1.pas'';'#10#10'end.'#10;

procedure TesteSintetico;
var
  vPasta: string;
  vLoc: TLocalizadorArquivos;
  vConj: TConjuntoArquivos;
  vRes: TResultadoMigracao;
  vApi: TApi;
  vTotal: integer;
begin
  WriteLn('projeto sintetico contra a API 2.0');
  vPasta := GRaiz + 'sint' + PathDelim;
  Gravar(vPasta + 'Unit1.dfm', DFM);
  Gravar(vPasta + 'Unit1.pas', PAS);
  Gravar(vPasta + 'p.dproj', DPROJ);
  Gravar(vPasta + 'q.lpi', LPI);
  Gravar(vPasta + 'p.dpk', DPK);
  vApi := CarregarApiDaVersao('2.0', '');
  Conferir(vApi <> nil, 'API 2.0 carregada');
  if vApi = nil then
    Exit;
  vLoc := TLocalizadorArquivos.Create(vPasta, False);
  vRes := TResultadoMigracao.Create;
  try
    vLoc.Executar;
    vConj := vLoc.Conjunto;
    vRes.Para := '2.0';
    vTotal := ValidarResultado(vConj, vApi, vRes);
    ListarValidacao(vRes);
    Conferir(Achou(vRes, 'Unit1.dfm', 4, 'Property CompressType does not exist'),
      'CompressType (1.2) acusado na linha 4');
    Conferir(not Achou(vRes, 'Unit1.dfm', 5, ''), 'Port existe: nada na linha 5');
    Conferir(Achou(vRes, 'Unit1.dfm', 6, 'Property Name2'), 'Name2 acusado');
    Conferir(not Achou(vRes, 'Unit1.dfm', 7, ''), 'IPConfig.IPv6Enabled existe (segundo nivel)');
    Conferir(Achou(vRes, 'Unit1.dfm', 8, 'Property IPConfig.Nada'), 'IPConfig.Nada acusado (segundo nivel)');
    Conferir(not Achou(vRes, 'Unit1.dfm', 9, ''), 'Left de componente aceito');
    Conferir(not Achou(vRes, 'Unit1.dfm', 13, '') and not Achou(vRes, 'Unit1.dfm', 12, '') and
      not Achou(vRes, 'Unit1.dfm', 11, ''), 'TButton e as propriedades dele: nada');
    Conferir(Achou(vRes, 'Unit1.dfm', 15, 'Class TRALNaoExiste not found'), 'classe inexistente');
    Conferir(not Achou(vRes, 'Unit1.dfm', 16, ''), 'propriedade de classe inexistente nao e acusada a parte');
    Conferir(not Achou(vRes, 'Unit1.dfm', 19, ''), 'Name aceito');
    Conferir(not Achou(vRes, 'Unit1.dfm', 21, '') and not Achou(vRes, 'Unit1.dfm', 22, ''),
      'TRALMeu declarada no projeto nao e do RAL');
    Conferir(Achou(vRes, 'Unit1.pas', 6, 'RALIndyRegister'), 'unit inexistente no uses (interface)');
    Conferir(not Achou(vRes, 'Unit1.pas', 6, 'Unit9') and not Achou(vRes, 'Unit1.pas', 6, 'SysUtils') and
      not Achou(vRes, 'Unit1.pas', 6, 'RALIndyServer'), 'units que nao sao do RAL ou que existem: nada');
    Conferir(Achou(vRes, 'Unit1.pas', 0, 'RALDBFireDACLinkReg'), 'unit inexistente na implementation');
    Conferir(Achou(vRes, 'p.dproj', 3, 'pacote RALDBFireDACLink'), 'pacote inexistente no dproj');
    Conferir(not Achou(vRes, 'p.dproj', 0, 'Outro') and not Achou(vRes, 'p.dproj', 0, 'PascalRAL é'),
      'pacote de terceiro e pacote existente: nada');
    Conferir(Achou(vRes, 'q.lpi', 0, 'raldbzeoslink'), 'pacote inexistente no lpi');
    Conferir(not Achou(vRes, 'q.lpi', 0, 'LCL'), 'LCL nao e do RAL');
    Conferir(Achou(vRes, 'p.dpk', 5, 'RALDBFireDACLink'), 'pacote inexistente no requires do dpk');
    Conferir(vTotal = ContarValidacao(vRes), 'o retorno e o numero de itens');
    Conferir(vTotal = 9, 'nove itens no total (' + IntToStr(vTotal) + ')');
  finally
    vRes.Free;
    vLoc.Free;
    vApi.Free;
  end;
end;

{ Samples }

/// Validates the folder as it is (no migration)
procedure ValidarPasta(const APasta, ARotulo: string; var ATotal: integer);
var
  vLoc: TLocalizadorArquivos;
  vRes: TResultadoMigracao;
  vApi: TApi;
begin
  vApi := CarregarApiDaVersao('2.0', '');
  vLoc := TLocalizadorArquivos.Create(APasta, False);
  vRes := TResultadoMigracao.Create;
  try
    vLoc.Executar;
    vRes.Para := '2.0';
    ATotal := ValidarResultado(vLoc.Conjunto, vApi, vRes);
    WriteLn('  ', ARotulo, ': ', vLoc.Conjunto.Count, ' arquivos, ', ATotal,
      ' item(ns) de validacao');
    ListarValidacao(vRes);
  finally
    vRes.Free;
    vLoc.Free;
    vApi.Free;
  end;
end;

procedure TesteSamples;
var
  vAntigo, vNovo: string;
  vMotor: TMotorMigracao;
  vApi: TApi;
  vTotal, vErros, vInt: integer;
begin
  WriteLn('samples ', COMMIT);
  if not DirectoryExists(GRepo) then
  begin
    WriteLn('  pulado: nao ha repositorio em ', GRepo);
    Exit;
  end;
  vAntigo := GRaiz + 'samples-antigo' + PathDelim;
  vNovo := GRaiz + 'samples-novo' + PathDelim;
  Extrair(COMMIT + '~1', vAntigo);
  Extrair(COMMIT, vNovo);
  Conferir(DirectoryExists(vAntigo + 'Delphi') and DirectoryExists(vNovo + 'Delphi'),
    'git archive extraiu os dois commits');
  if not DirectoryExists(vAntigo + 'Delphi') then
    Exit;

  vMotor := TMotorMigracao.Create;
  vApi := CarregarApiDaVersao('2.0', '');
  try
    vMotor.Pasta := vAntigo;
    vMotor.De := '1.1';
    vMotor.Para := '2.0';
    vMotor.Idioma := 'pt-BR';
    vMotor.Simular;
    vErros := 0;
    for vInt := 0 to vMotor.Resultado.Erros.Count - 1 do
    begin
      WriteLn('        erro do motor: ', vMotor.Resultado.Erros[vInt]);
      Inc(vErros);
    end;
    Conferir(vErros = 0, 'a migracao 1.1 -> 2.0 roda sem erro');
    vTotal := ValidarResultado(vMotor.Conjunto, vApi, vMotor.Resultado);
    WriteLn('  migrado em memoria: ', vMotor.Conjunto.Count, ' arquivos, ', vTotal,
      ' item(ns) de validacao');
    ListarValidacao(vMotor.Resultado);
    Conferir(vTotal = 0, 'samples migrados de 1.1 para 2.0: zero itens de validacao');
  finally
    vApi.Free;
    vMotor.Free;
  end;

  ValidarPasta(vNovo, COMMIT + ' (migrado a mao)', vTotal);
  WriteLn('  (informativo: o ', COMMIT, ' direto acusou ', vTotal, ')');
end;

begin
  DefaultSystemCodePage := CP_UTF8;
  if not (TemFlag('testes') or TemFlag('samples')) then
  begin
    WriteLn('uso: validacao --testes | --samples [--samples-repo=<repo>]');
    Halt(2);
  end;
  GRepo := Opcao('samples-repo', REPO_PADRAO);
  GRaiz := IncludeTrailingPathDelimiter(GetTempDir) + 'ralmig-validacao-' +
           IntToStr(GetProcessID) + PathDelim;
  ApagarPasta(GRaiz);
  try
    if TemFlag('testes') then
      TesteSintetico;
    if TemFlag('samples') then
      TesteSamples;
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
