program projeto;

{$mode ObjFPC}{$H+}

// Localizador de arquivos e acoes de projeto/pacote (etapas M2 e M3).
//   projeto --testes     casos sinteticos (pasta temporaria)
//   projeto --gabarito [--samples=<repo PascalRAL-Samples-2.0>]
//                        os testes sinteticos + a migracao 1.1 -> 2.0 do pai de
//                        677864d comparada com 677864d (so projeto e pacote)
// Nada e gravado no repositorio dos samples: os dois commits saem por
// "git archive" para uma pasta temporaria. Sem o repositorio, o gabarito e
// pulado.
//
// Compilar (da raiz do instalador):
//   fpc -B -MObjFPC -Sh -Fumigrador/src -Fusrc/classes -FUmigrador/lib/testes
//       -FEmigrador/lib/testes migrador/tests/projeto/projeto.lpr

uses
  Classes, SysUtils, md5,
  RALMig.Texto, RALMig.Tipos, RALMig.Motor, RALMig.Projeto, RALMig.ProjetoXML,
  RALMig.&Uses, RALMig.Inventario, RALMig.Acoes.Projeto;

const
  COMMIT_GABARITO = '677864d';
  REPO_PADRAO = 'D:\Projetos\Delphi\OpenSourceCommunityBrasil\PascalRAL\PascalRAL-Samples-2.0';

var
  GFalhas: integer = 0;
  GRaiz: string;
  GSamples: string;

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

procedure ListarArquivos(const APasta, ARel: string; ADest: TStringList);
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
        ListarArquivos(APasta, ARel + vBusca.Name + '/', ADest)
      else
        ADest.Add(ARel + vBusca.Name);
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
end;

function Foto(const APasta: string): string;
var
  vLista: TStringList;
  vInt: integer;
begin
  vLista := TStringList.Create;
  try
    ListarArquivos(IncludeTrailingPathDelimiter(APasta), '', vLista);
    vLista.Sort;
    for vInt := 0 to vLista.Count - 1 do
      vLista[vInt] := vLista[vInt] + '=' +
        MD5Print(MD5File(IncludeTrailingPathDelimiter(APasta) + vLista[vInt]));
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

function Opcao(const ANome, APadrao: string): string;
var
  vInt: integer;
begin
  Result := APadrao;
  for vInt := 1 to ParamCount do
    if Copy(ParamStr(vInt), 1, Length(ANome) + 3) = '--' + ANome + '=' then
      Result := Copy(ParamStr(vInt), Length(ANome) + 4, MaxInt);
end;

function TemFlag(const ANome: string): boolean;
var
  vInt: integer;
begin
  Result := False;
  for vInt := 1 to ParamCount do
    if ParamStr(vInt) = '--' + ANome then
      Result := True;
end;

function NovoMotor(const APasta, ADe, APara: string): TMotorMigracao;
begin
  Result := TMotorMigracao.Create;
  Result.Pasta := APasta;
  Result.De := ADe;
  Result.Para := APara;
  Result.Idioma := 'pt-BR';
end;

/// Runs the whole chain; only "action without implementation" errors are ignored
function Migrar(const APasta, ADe, APara: string; out AManuais: integer): boolean;
var
  vMotor: TMotorMigracao;
  vInt: integer;
begin
  vMotor := NovoMotor(APasta, ADe, APara);
  try
    vMotor.Simular;
    Result := True;
    for vInt := 0 to vMotor.Resultado.Erros.Count - 1 do
      if Pos('implementa', vMotor.Resultado.Erros[vInt]) = 0 then
      begin
        WriteLn('        erro: ', vMotor.Resultado.Erros[vInt]);
        Result := False;
      end;
    AManuais := vMotor.Resultado.ManualCount;
    Result := Result and vMotor.Gravar(False);
  finally
    vMotor.Free;
  end;
end;

{ Sinteticos }

procedure TesteDpk;
var
  vPasta, vTexto: string;
  vManuais: integer;
begin
  WriteLn('.dpk do usuario');
  vPasta := GRaiz + 'dpk' + PathDelim;
  Gravar(vPasta + 'runtime.dpk',
    'package runtime;'#10#10'{$R *.res}'#10'{$RUNONLY}'#10#10'requires'#10 +
    '  rtl,'#10'  PascalRALDsgn,'#10'  RALDBFireDACLink;'#10#10'end.'#10);
  Gravar(vPasta + 'designonly' + PathDelim + 'so.dpk',
    'package so;'#10#10'{$DESIGNONLY}'#10#10'requires'#10'  designide,'#10'  PascalRALDsgn;'#10#10'end.'#10);
  Gravar(vPasta + 'comregister' + PathDelim + 'reg.dpk',
    'package reg;'#10#10'requires'#10'  PascalRALDsgn;'#10#10'contains'#10 +
    '  RegUnit in ''RegUnit.pas'';'#10#10'end.'#10);
  Gravar(vPasta + 'comregister' + PathDelim + 'RegUnit.pas',
    'unit RegUnit;'#10#10'interface'#10#10'procedure Register;'#10#10'implementation'#10#10 +
    'procedure Register;'#10'begin'#10'end;'#10#10'end.'#10);
  Conferir(Migrar(vPasta, '1.1', '2.0', vManuais), 'migra');
  vTexto := Ler(vPasta + 'runtime.dpk');
  Conferir((Pos('PascalRAL,', vTexto) > 0) and (Pos('PascalRALDsgn', vTexto) = 0),
    'PascalRALDsgn -> PascalRAL no pacote de runtime');
  Conferir((Pos('RALDBFireDACPackage;', vTexto) > 0) and (Pos('RALDBFireDACLink', vTexto) = 0),
    'RALDBFireDACLink -> RALDBFireDACPackage');
  Conferir(Pos('PascalRALDsgn', Ler(vPasta + 'designonly' + PathDelim + 'so.dpk')) > 0,
    '{$DESIGNONLY}: continua com PascalRALDsgn');
  Conferir(Pos('PascalRALDsgn', Ler(vPasta + 'comregister' + PathDelim + 'reg.dpk')) > 0,
    'com procedure Register: continua com PascalRALDsgn');
  Conferir(vManuais = 2, 'dois itens manuais (designide): ' + IntToStr(vManuais));
end;

procedure TesteLpiItens;
var
  vPasta, vNovo, vVelho: string;
  vManuais: integer;
begin
  WriteLn('.lpi: pacote acrescentado no estilo da lista');
  vPasta := GRaiz + 'lpi' + PathDelim;
  Gravar(vPasta + 'novo' + PathDelim + 'p.lpi',
    '<?xml version="1.0" encoding="UTF-8"?>'#10'<CONFIG>'#10'  <ProjectOptions>'#10 +
    '    <RequiredPackages>'#10'      <Item>'#10'        <PackageName Value="LCL"/>'#10 +
    '      </Item>'#10'      <Item>'#10'        <PackageName Value="raldbzeoslink"/>'#10 +
    '      </Item>'#10'    </RequiredPackages>'#10'  </ProjectOptions>'#10'</CONFIG>'#10);
  Gravar(vPasta + 'velho' + PathDelim + 'p.lpi',
    '<?xml version="1.0" encoding="UTF-8"?>'#10'<CONFIG>'#10'  <ProjectOptions>'#10 +
    '    <RequiredPackages Count="2">'#10'      <Item1>'#10'        <PackageName Value="LCL"/>'#10 +
    '      </Item1>'#10'      <Item2>'#10'        <PackageName Value="raldbzeoslink"/>'#10 +
    '      </Item2>'#10'    </RequiredPackages>'#10'  </ProjectOptions>'#10'</CONFIG>'#10);
  Gravar(vPasta + 'novo' + PathDelim + 'u.pas', 'unit u;'#10'interface'#10'uses RALStorageBIN;'#10 +
    'implementation'#10'end.'#10);
  Gravar(vPasta + 'velho' + PathDelim + 'u.pas', 'unit u;'#10'interface'#10'uses RALStorageBIN;'#10 +
    'implementation'#10'end.'#10);
  Conferir(Migrar(vPasta, '1.1', '2.0', vManuais), 'migra');
  vNovo := Ler(vPasta + 'novo' + PathDelim + 'p.lpi');
  vVelho := Ler(vPasta + 'velho' + PathDelim + 'p.lpi');
  Conferir((Pos('<Item>'#10'        <PackageName Value="ralstoragepackage"/>', vNovo) > 0) and
    (Pos('Item1', vNovo) = 0) and (Pos('Count', vNovo) = 0),
    'lista sem Count: o item novo e <Item>, sem Count');
  Conferir((Pos('<Item3>', vVelho) > 0) and (Pos('Count="3"', vVelho) > 0),
    'lista com Count: o item novo e <Item3> e o Count sobe');
end;

procedure TesteLocalizador;
var
  vPasta, vFora: string;
  vLoc: TLocalizadorArquivos;
  vTexto, vBinario: TFileStream;
  vTxt: TStringStream;
begin
  WriteLn('localizador');
  vPasta := GRaiz + 'loc' + PathDelim + 'proj' + PathDelim;
  vFora := GRaiz + 'loc' + PathDelim + 'fora' + PathDelim;
  Gravar(vPasta + 'a.dpr', 'program a; uses U1 in ''U1.pas'', Ext in ''..\fora\Ext.pas''; begin end.');
  Gravar(vPasta + 'U1.pas', 'unit U1; interface implementation end.');
  Gravar(vPasta + '__history' + PathDelim + 'U1.pas.~1~', 'x');
  Gravar(vPasta + 'lib' + PathDelim + 'Velha.pas', 'unit Velha; interface implementation end.');
  Gravar(vPasta + 'Win32' + PathDelim + 'Debug' + PathDelim + 'Saida.pas', 'unit Saida; interface implementation end.');
  Gravar(vPasta + 'sub' + PathDelim + 'U2.pas', 'unit U2; interface implementation end.');
  Gravar(vPasta + 'g.groupproj',
    '<Project><ItemGroup><Projects Include="a.dpr"/><Projects Include="..\fora\b.dproj"/></ItemGroup></Project>');
  Gravar(vFora + 'Ext.pas', 'unit Ext; interface implementation end.');
  Gravar(vFora + 'b.dproj', '<Project/>');
  // forms: one text, one binary
  Gravar(vPasta + 'F1.dfm', 'object F1: TForm1'#10'end'#10);
  vTxt := TStringStream.Create('object F2: TForm1'#10'end'#10);
  vBinario := TFileStream.Create(vPasta + 'F2.dfm', fmCreate);
  try
    ObjectTextToBinary(vTxt, vBinario);
  finally
    vBinario.Free;
    vTxt.Free;
  end;
  vTexto := nil;
  vLoc := TLocalizadorArquivos.Create(vPasta, False);
  try
    vLoc.Executar;
    Conferir(vLoc.Conjunto.Buscar('U1.pas') <> nil, 'unit da pasta entra');
    Conferir(vLoc.Conjunto.Buscar('sub/U2.pas') <> nil, 'unit de subpasta entra');
    Conferir(vLoc.Conjunto.Buscar('__history/U1.pas.~1~') = nil, '__history fica de fora');
    Conferir(vLoc.Conjunto.Buscar('lib/Velha.pas') = nil, 'lib fica de fora');
    Conferir(vLoc.Conjunto.Buscar('Win32/Debug/Saida.pas') = nil, 'Win32 fica de fora');
    Conferir(vLoc.Conjunto.Buscar('g.groupproj') <> nil, '.groupproj entra');
    Conferir(vLoc.ProjetosFora.Count = 1, 'projeto do grupo fora da pasta e listado');
    Conferir(vLoc.UnitsFora.Count = 1, 'unit citada por "in" fora da pasta e listada');
    Conferir(vLoc.Conjunto.Buscar('F1.dfm') <> nil, 'formulario em texto entra');
    Conferir(vLoc.Conjunto.Buscar('F2.dfm').Ignorado, 'formulario binario fica ignorado');
    Conferir(vLoc.Conjunto.Buscar('F2.dfm').Motivo <> '', '... com motivo');
    vLoc.IncluirFora(vFora + 'Ext.pas');
    Conferir(vLoc.Conjunto.Buscar(vFora + 'Ext.pas') <> nil, 'IncluirFora acrescenta o arquivo');
  finally
    vLoc.Free;
  end;
  vLoc := TLocalizadorArquivos.Create(vPasta, True);
  try
    vLoc.Executar;
    Conferir(not vLoc.Conjunto.Buscar('F2.dfm').Ignorado, 'ConverterBinarios: nao ignora');
    Conferir(Pos('object F2', vLoc.Conjunto.Buscar('F2.dfm').Atual) > 0, '... e converte para texto');
    Conferir(vLoc.Conjunto.Buscar('F2.dfm').Alterado, '... marcado como alterado');
  finally
    vLoc.Free;
  end;
  vLoc := TLocalizadorArquivos.Create(vPasta + 'a.dpr', False);
  try
    vLoc.Executar;
    Conferir(vLoc.Conjunto.Buscar('a.dpr') <> nil, 'a partir de um arquivo de projeto');
  finally
    vLoc.Free;
  end;
end;

{ Gabarito }

procedure Extrair(const ACommit, ADestino: string);
var
  vComando: string;
begin
  ForceDirectories(ADestino);
  vComando := 'git -C "' + GSamples + '" archive ' + ACommit + ' | tar -x -C "' +
    ExcludeTrailingPathDelimiter(ADestino) + '"';
  {$IFDEF MSWINDOWS}
  ExecuteProcess(GetEnvironmentVariable('ComSpec'), '/c ' + vComando);
  {$ELSE}
  ExecuteProcess('/bin/sh', ['-c', vComando]);
  {$ENDIF}
end;

procedure Separar(const ATexto: string; ADest: TStringList);
var
  vItens: TStringList;
  vInt: integer;
begin
  ADest.Sorted := True;
  ADest.CaseSensitive := False;
  ADest.Duplicates := dupIgnore;
  vItens := TStringList.Create;
  try
    vItens.StrictDelimiter := True;
    vItens.Delimiter := ';';
    vItens.DelimitedText := ATexto;
    for vInt := 0 to vItens.Count - 1 do
      if Trim(vItens[vInt]) <> '' then
        ADest.Add(Trim(vItens[vInt]));
  finally
    vItens.Free;
  end;
end;

var
  GIguais, GPrevistas, GImprevistas: array[0..2] of integer;
  GDetalhes: TStringList;
  GGabUsaPlugins: boolean;

/// Does a .pas beside the gabarito project use RALContent or RALCORS?
function GabaritoUsaPlugins(const AProjeto: string): boolean;
var
  vBusca: TSearchRec;
  vDir: string;
  vTexto: string;
begin
  Result := False;
  vDir := ExtractFilePath(AProjeto);
  if FindFirst(vDir + '*.pas', faAnyFile, vBusca) = 0 then
  try
    repeat
      vTexto := Ler(vDir + vBusca.Name);
      if (Pos('RALContent', vTexto) > 0) or (Pos('RALCORS', vTexto) > 0) then
        Result := True;
    until FindNext(vBusca) <> 0;
  finally
    FindClose(vBusca);
  end;
end;

const
  NOMES: array[0..2] of string = ('.dproj', '.lpi', '.lpr');

/// Compares two sets; extras of ours must be required packages, extras of the
/// gabarito only PascalRALDsgn
procedure CompararConjuntos(AKind: integer; const ARel: string; AMeu, AGab: TStringList);
const
  EXIGIDOS = ',ralpluginauthpackage,ralplugintlspackage,ralstoragepackage,ralmodulewebpackage,' +
    'ralmoduleswaggerpackage,pascalral,';
var
  vInt: integer;
  vPrevista, vImprevista: boolean;
  vNome: string;
begin
  vPrevista := False;
  vImprevista := False;
  for vInt := 0 to AMeu.Count - 1 do
    if AGab.IndexOf(AMeu[vInt]) < 0 then
    begin
      vNome := AMeu[vInt];
      if (AKind <> 2) and (Pos(',' + LowerCase(vNome) + ',', EXIGIDOS) > 0) then
      begin
        vPrevista := True;
        GDetalhes.Add('pacote-exigir (' + vNome + ') ' + NOMES[AKind]);
      end
      else
      begin
        vImprevista := True;
        WriteLn('        ', ARel, ': so no migrado: ', vNome);
      end;
    end;
  for vInt := 0 to AGab.Count - 1 do
    if AMeu.IndexOf(AGab[vInt]) < 0 then
    begin
      vNome := AGab[vInt];
      if (AKind = 0) and SameText(vNome, 'PascalRALDsgn') then
      begin
        vPrevista := True;
        GDetalhes.Add('pacote-design-para-runtime (PascalRALDsgn) ' + NOMES[AKind]);
      end
      else if (AKind = 1) and SameText(vNome, 'pascalral') and GGabUsaPlugins then
      begin
        // the gabarito needs pascalral because the form actions (M4) added
        // RALContent/RALCORS to the uses; no rule lists those units yet
        vPrevista := True;
        GDetalhes.Add('pacote-exigir (pascalral) por unit de plugin (depende da M4) ' + NOMES[AKind]);
      end
      else
      begin
        vImprevista := True;
        WriteLn('        ', ARel, ': so no gabarito: ', vNome);
      end;
    end;
  if vImprevista then
    Inc(GImprevistas[AKind])
  else if vPrevista then
    Inc(GPrevistas[AKind])
  else
    Inc(GIguais[AKind]);
end;

procedure CompararDproj(const ARel, AMeu, AGab: string);
var
  vA, vB: TProjetoXML;
  vTA, vTB: TTextoOriginal;
  vLA, vLB: TList;
  vInt: integer;
  vSA, vSB: TStringList;
begin
  vTA := TTextoOriginal.Create;
  vTB := TTextoOriginal.Create;
  vTA.CarregarArquivo(AMeu);
  vTB.CarregarArquivo(AGab);
  vA := TProjetoXML.Create(vTA);
  vB := TProjetoXML.Create(vTB);
  vLA := vA.Buscar('//DCC_UsePackage');
  vLB := vB.Buscar('//DCC_UsePackage');
  try
    Conferir(vLA.Count = vLB.Count, ARel + ': mesmos PropertyGroup com DCC_UsePackage');
    for vInt := 0 to vLA.Count - 1 do
    begin
      if vInt >= vLB.Count then
        Break;
      vSA := TStringList.Create;
      vSB := TStringList.Create;
      try
        Separar(vA.ValorTexto(TElementoXML(vLA[vInt])), vSA);
        Separar(vB.ValorTexto(TElementoXML(vLB[vInt])), vSB);
        CompararConjuntos(0, ARel, vSA, vSB);
      finally
        vSB.Free;
        vSA.Free;
      end;
    end;
  finally
    vLB.Free;
    vLA.Free;
    vB.Free;
    vA.Free;
    vTB.Free;
    vTA.Free;
  end;
end;

procedure CompararLpi(const ARel, AMeu, AGab: string);
var
  vA, vB: TProjetoXML;
  vTA, vTB: TTextoOriginal;
  vSA, vSB: TStringList;
begin
  vTA := TTextoOriginal.Create;
  vTB := TTextoOriginal.Create;
  vTA.CarregarArquivo(AMeu);
  vTB.CarregarArquivo(AGab);
  vA := TProjetoXML.Create(vTA);
  vB := TProjetoXML.Create(vTB);
  vSA := TStringList.Create;
  vSB := TStringList.Create;
  GGabUsaPlugins := GabaritoUsaPlugins(AGab);
  try
    vSA.Sorted := True;
    vSB.Sorted := True;
    vSA.CaseSensitive := False;
    vSB.CaseSensitive := False;
    vSA.Duplicates := dupIgnore;
    vSB.Duplicates := dupIgnore;
    if vA.ContainerDePacotes <> nil then
      vA.ListarPacotes(vA.ContainerDePacotes, vSA);
    if vB.ContainerDePacotes <> nil then
      vB.ListarPacotes(vB.ContainerDePacotes, vSB);
    CompararConjuntos(1, ARel, vSA, vSB);
  finally
    vSB.Free;
    vSA.Free;
    vB.Free;
    vA.Free;
    vTB.Free;
    vTA.Free;
  end;
end;

procedure UsesDe(const ACaminho: string; ADest: TStringList);
var
  vTexto: TTextoOriginal;
  vEd: TEditorUses;
  vCl, vIt: integer;
begin
  ADest.Sorted := True;
  ADest.CaseSensitive := False;
  ADest.Duplicates := dupIgnore;
  vTexto := TTextoOriginal.Create;
  try
    vTexto.CarregarArquivo(ACaminho);
    vEd := TEditorUses.Create(vTexto);
    try
      for vCl := 0 to vEd.Count - 1 do
        for vIt := 0 to vEd.Clausulas[vCl].Count - 1 do
          ADest.Add(vEd.Clausulas[vCl].Item(vIt).Nome);
    finally
      vEd.Free;
    end;
  finally
    vTexto.Free;
  end;
end;

procedure CompararLpr(const ARel, AMeu, AGab: string);
var
  vSA, vSB: TStringList;
begin
  vSA := TStringList.Create;
  vSB := TStringList.Create;
  try
    UsesDe(AMeu, vSA);
    UsesDe(AGab, vSB);
    CompararConjuntos(2, ARel, vSA, vSB);
  finally
    vSB.Free;
    vSA.Free;
  end;
end;

procedure TesteGabarito;
var
  vAntigo, vNovo, vCopiaCli: string;
  vArquivos, vVistos: TStringList;
  vInt, vManuais: integer;
  vExt, vExe, vAntes: string;
begin
  WriteLn('gabarito ', COMMIT_GABARITO);
  if not DirectoryExists(GSamples) then
  begin
    WriteLn('  pulado: nao ha repositorio em ', GSamples);
    Exit;
  end;
  vAntigo := GRaiz + 'gab-antigo' + PathDelim;
  vNovo := GRaiz + 'gab-novo' + PathDelim;
  vCopiaCli := GRaiz + 'gab-cli' + PathDelim;
  Extrair(COMMIT_GABARITO + '~1', vAntigo);
  Extrair(COMMIT_GABARITO, vNovo);
  Extrair(COMMIT_GABARITO + '~1', vCopiaCli);
  Conferir(DirectoryExists(vAntigo + 'Delphi') and DirectoryExists(vNovo + 'Delphi'),
    'git archive extraiu os dois commits');
  if not DirectoryExists(vAntigo + 'Delphi') then
    Exit;
  vExe := '';
  // the CLI runs on a third copy, and must not write anything
  vExe := AcharPastaMigrador + PathDelim + 'ralmigra' + ExtractFileExt(ParamStr(0));
  if FileExists(vExe) then
  begin
    vAntes := Foto(vCopiaCli);
    {$IFDEF MSWINDOWS}
    ExecuteProcess(GetEnvironmentVariable('ComSpec'), '/c ""' + vExe + '" previa "' +
      ExcludeTrailingPathDelimiter(vCopiaCli) + '" --de=1.1 --para=2.0 > "' + GRaiz + 'previa.txt""');
    {$ELSE}
    ExecuteProcess('/bin/sh', ['-c', '"' + vExe + '" previa "' + vCopiaCli +
      '" --de=1.1 --para=2.0 > "' + GRaiz + 'previa.txt"']);
    {$ENDIF}
    Conferir(FileExists(GRaiz + 'previa.txt') and (Pos('+++ b/', Ler(GRaiz + 'previa.txt')) > 0),
      'ralmigra previa mostrou o diff');
    Conferir(Foto(vCopiaCli) = vAntes, 'ralmigra previa nao gravou nada (hash da arvore igual)');
  end
  else
    WriteLn('  pulado: ralmigra nao compilado em ', vExe);

  Conferir(Migrar(vAntigo, '1.1', '2.0', vManuais), 'migra os samples 1.x de 1.1 para 2.0');
  vArquivos := TStringList.Create;
  vVistos := TStringList.Create;
  GDetalhes := TStringList.Create;
  try
    ListarArquivos(vNovo, '', vArquivos);
    for vInt := 0 to vArquivos.Count - 1 do
    begin
      if not FileExists(vAntigo + vArquivos[vInt]) then
        Continue;
      vExt := LowerCase(ExtractFileExt(vArquivos[vInt]));
      if vExt = '.dproj' then
        CompararDproj(vArquivos[vInt], vAntigo + vArquivos[vInt], vNovo + vArquivos[vInt])
      else if vExt = '.lpi' then
        CompararLpi(vArquivos[vInt], vAntigo + vArquivos[vInt], vNovo + vArquivos[vInt])
      else if vExt = '.lpr' then
        CompararLpr(vArquivos[vInt], vAntigo + vArquivos[vInt], vNovo + vArquivos[vInt]);
    end;
    for vInt := 0 to 2 do
    begin
      WriteLn(Format('  %-7s iguais: %d, diferencas previstas: %d, imprevistas: %d',
        [NOMES[vInt], GIguais[vInt], GPrevistas[vInt], GImprevistas[vInt]]));
      Conferir(GImprevistas[vInt] = 0, NOMES[vInt] + ': nenhuma diferenca imprevista');
    end;
    GDetalhes.Sorted := True;
    vVistos.Sorted := True;
    for vInt := 0 to GDetalhes.Count - 1 do
      if vVistos.IndexOf(GDetalhes[vInt]) < 0 then
        vVistos.Add(GDetalhes[vInt]);
    for vInt := 0 to vVistos.Count - 1 do
      WriteLn('        prevista: ', vVistos[vInt]);
  finally
    GDetalhes.Free;
    vVistos.Free;
    vArquivos.Free;
  end;
end;

begin
  if not (TemFlag('testes') or TemFlag('gabarito')) then
  begin
    WriteLn('uso: projeto --testes | --gabarito [--samples=<repo>]');
    Halt(2);
  end;
  GSamples := Opcao('samples', REPO_PADRAO);
  GRaiz := IncludeTrailingPathDelimiter(GetTempDir) + 'ralmig-projeto-' +
           IntToStr(GetProcessID) + PathDelim;
  ApagarPasta(GRaiz);
  try
    TesteDpk;
    TesteLpiItens;
    TesteLocalizador;
    if TemFlag('gabarito') then
      TesteGabarito;
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
