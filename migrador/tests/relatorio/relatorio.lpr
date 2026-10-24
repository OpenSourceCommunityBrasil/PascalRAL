program relatorio;

{$mode ObjFPC}{$H+}

// Relatorio da migracao e ralmigra completa (etapa M7).
//   relatorio --testes     relatorio de um resultado sintetico
//   relatorio --samples [--exe=<ralmigra>] [--repo=<PascalRAL-Samples-2.0>]
//                          migra uma copia do pai de 677864d, desfaz, confere o git
// Trabalha em pastas temporarias; nunca grava no repositorio dos samples.
//
// Compilar (da raiz do instalador):
//   fpc -B -MObjFPC -Sh -Fumigrador/src -Fusrc/classes -FUmigrador/lib/testes
//       -FEmigrador/lib/testes migrador/tests/relatorio/relatorio.lpr
// A ralmigra: `lazbuild -B ralmigra.lpi` de dentro de migrador/.

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Classes, SysUtils,
  RALInst.Processo, RALInst.SHA256, RALMig.Texto, RALMig.Tipos, RALMig.Relatorio;

var
  GFalhas: integer = 0;

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

procedure ApagarArvore(const APasta: string);
var
  vBusca: TSearchRec;
  vPasta: string;
begin
  vPasta := IncludeTrailingPathDelimiter(APasta);
  if FindFirst(vPasta + '*', faAnyFile, vBusca) = 0 then
  begin
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
        ApagarArvore(vPasta + vBusca.Name)
      else
      begin
        FileSetAttr(vPasta + vBusca.Name, 0);
        DeleteFile(vPasta + vBusca.Name);
      end;
    until FindNext(vBusca) <> 0;
    FindClose(vBusca);
  end;
  RemoveDir(APasta);
end;

function NovaPasta(const ANome: string): string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir) + 'ralmig-rel-' + ANome + PathDelim;
  ApagarArvore(ExcludeTrailingPathDelimiter(Result));
  ForceDirectories(Result);
end;

function LerTudo(const ACaminho: string): string;
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

procedure GravarTudo(const ACaminho, ATexto: string);
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

function Tem(const ATexto, AParte: string): boolean;
begin
  Result := Pos(AParte, ATexto) > 0;
end;

function Contar(const ATexto, AParte: string): integer;
var
  vPos: integer;
begin
  Result := 0;
  vPos := Pos(AParte, ATexto);
  while vPos > 0 do
  begin
    Inc(Result);
    vPos := Pos(AParte, ATexto, vPos + Length(AParte));
  end;
end;

function Antes(const ATexto, A, B: string): boolean;
begin
  Result := (Pos(A, ATexto) > 0) and (Pos(A, ATexto) < Pos(B, ATexto));
end;

{ Processes }

/// Runs a program; returns its exit code (-1 when it could not start)
function Rodar(const AExe: string; const AArgs: array of string; const APasta: string;
  out ASaida: string): integer;
var
  vExec: TExecucao;
  vInt: integer;
begin
  vExec := TExecucao.Create;
  try
    vExec.Executavel := AExe;
    for vInt := 0 to High(AArgs) do
      vExec.Parametros.Add(AArgs[vInt]);
    vExec.PastaTrabalho := APasta;
    vExec.Executar;
    ASaida := vExec.Saida.Text;
    if vExec.Erro <> '' then
      Result := -1
    else
      Result := vExec.CodigoSaida;
  finally
    vExec.Free;
  end;
end;

function Git(const APasta: string; const AArgs: array of string; out ASaida: string): integer;
var
  vTodos: array of string;
  vInt: integer;
begin
  vTodos := nil;
  SetLength(vTodos, Length(AArgs) + 6);
  vTodos[0] := '-c';
  vTodos[1] := 'user.name=teste';
  vTodos[2] := '-c';
  vTodos[3] := 'user.email=teste@teste.invalid';
  vTodos[4] := '-c';
  vTodos[5] := 'core.autocrlf=false';
  for vInt := 0 to High(AArgs) do
    vTodos[6 + vInt] := AArgs[vInt];
  Result := Rodar('git', vTodos, APasta, ASaida);
end;

/// Name + SHA-256 of every file of the tree (outside .git and the reports), as a text
function HashDaArvore(const APasta: string): string;
var
  vBusca: TSearchRec;
  vPasta: string;
begin
  Result := '';
  vPasta := IncludeTrailingPathDelimiter(APasta);
  if FindFirst(vPasta + '*', faAnyFile, vBusca) = 0 then
  begin
    repeat
      if (vBusca.Name = '.') or (vBusca.Name = '..') or (vBusca.Name = '.git') then
        Continue;
      if (vBusca.Attr and faDirectory) <> 0 then
        Result := Result + HashDaArvore(vPasta + vBusca.Name)
      else if Copy(vBusca.Name, 1, 13) <> 'MIGRACAO-RAL-' then
        Result := Result + vPasta + vBusca.Name + ' ' + SHA256Arquivo(vPasta + vBusca.Name) + #10;
    until FindNext(vBusca) <> 0;
    FindClose(vBusca);
  end;
end;

{ Synthetic result }

function ResultadoSintetico: TResultadoMigracao;
begin
  Result := TResultadoMigracao.Create;
  Result.De := '1.1';
  Result.Para := '2.0';
  Result.Idioma := 'pt-BR';
  Result.AdicionarFeito('pacote-trocar', 'Unit1.pas', 12, 'uses: RALServer -> RALServer.Core');
  Result.AdicionarFeito('pacote-trocar', 'Unit1.pas', 14, 'uses: RALClient -> RALClient.Core');
  Result.AdicionarFeito('prop-renomear', 'Unit1.pas', 40, 'Porta virou ServerPort');
  Result.AdicionarFeito('prop-renomear', 'Form1.dfm', 7, 'Porta virou ServerPort');
  Result.AdicionarManual('corpo-stream', 'Unit1.pas', 52,
    'O corpo agora e TStream: "a < b & c" nao compila.',
    'Troque a leitura do corpo.' + #13#10 + 'Use o trecho abaixo.',
    'if (a < b) and (c & d) then'#13#10'  Stream := Req.Body;',
    'plugins.md#o-que-mudou-da-12-para-a-20');
  Result.AdicionarManual('script-injetado', 'Form1.dfm', 0,
    'Nome <script>alert(1)</script> no formulario', 'Renomeie.', '', '');
  Result.AdicionarComportamento('timeout-padrao', '1.2->2.0',
    'O tempo limite padrao passou de 30 para 60 s.', 'Defina Timeout se precisa de 30 s.',
    'migrar-para-2.0.md#timeout');
  Result.Erros.Add('Falha ao ler Antigo.pas: acesso negado');
end;

{ Tests }

procedure TesteWiki;
begin
  WriteLn('link da wiki');
  Conferir(LinkDaWiki('plugins.md#o-que-mudou-da-12-para-a-20') =
    'https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/plugins#o-que-mudou-da-12-para-a-20',
    'pagina.md#ancora');
  Conferir(LinkDaWiki('plugins.md') = URL_WIKI + 'plugins', 'sem ancora');
  Conferir(LinkDaWiki('') = '', 'vazio');
  Conferir(LinkDaWiki('https://exemplo.invalid/x') = 'https://exemplo.invalid/x', 'url completa');
  Conferir(NomeRelatorio('1.1', '2.0', 'md') = 'MIGRACAO-RAL-1.1-2.0.md', 'nome do arquivo');
end;

procedure TesteMarkdown(const AMd: string);
begin
  WriteLn('markdown');
  Conferir(Tem(AMd, '# Migração do RAL 1.1 para 2.0'), 'titulo');
  Conferir(Tem(AMd, '## Erros') and Tem(AMd, 'acesso negado'), 'bloco de erros');
  Conferir(Antes(AMd, '## Erros', '## Feito'), 'erros no topo, antes do feito');
  Conferir(Antes(AMd, '## Feito', '## Precisa de você') and
           Antes(AMd, '## Precisa de você', '## Comportamento que mudou'), 'ordem dos tres blocos');
  Conferir(Tem(AMd, '### Unit1.pas') and Tem(AMd, '### Form1.dfm'), 'feito por arquivo');
  Conferir(Tem(AMd, '`pacote-trocar` 2 alteração(ões)'), 'feito por regra, com contagem');
  Conferir(Tem(AMd, 'linha 12: uses: RALServer'), 'feito com a linha');
  Conferir(Tem(AMd, '### Unit1.pas:52 - corpo-stream'), 'manual com arquivo:linha');
  Conferir(Tem(AMd, '**Problema:**') and Tem(AMd, '**O que fazer:**'), 'problema e o que fazer');
  Conferir(Tem(AMd, '```pascal' + LineEnding + 'if (a < b) and (c & d) then'), 'trecho em bloco, sem escape');
  Conferir(Tem(AMd, '(wiki/plugins#o-que-mudou-da-12-para-a-20)') or
           Tem(AMd, 'wiki/plugins#o-que-mudou-da-12-para-a-20)'), 'link da wiki do manual');
  Conferir(Tem(AMd, '### timeout-padrao') and Tem(AMd, 'wiki/migrar-para-2.0#timeout'),
           'comportamento com link');
  Conferir(Tem(AMd, 'Defina Timeout'), 'o que fazer do comportamento');
  Conferir(Tem(AMd, 'a \< b \& c'), 'texto do usuario escapado');
  Conferir(Tem(AMd, '2 arquivo(s) alterado(s), 4 alteração(ões), 2 item(ns) para fazer à mão, ' +
    '1 comportamento(s) que mudou(aram), 1 erro(s).'), 'contagens');
  Conferir(Tem(AMd, 'Arquivos não migrados') and Tem(AMd, 'formulario binario'), 'arquivos ignorados');
  Conferir(Tem(AMd, 'não faz parte do desfazer'), 'rodape: relatorio fora do desfazer');
end;

procedure TesteHtml(const AHtml: string);
begin
  WriteLn('html');
  Conferir(Tem(AHtml, '<!DOCTYPE html>') and Tem(AHtml, '<meta charset="utf-8">'), 'autocontido');
  Conferir(Tem(AHtml, 'prefers-color-scheme:dark'), 'tema escuro');
  Conferir(not Tem(AHtml, '<script') and not Tem(AHtml, 'src=') and not Tem(AHtml, 'http-equiv'),
           'sem script nem recurso externo');
  Conferir(Tem(AHtml, 'if (a &lt; b) and (c &amp; d) then'), 'trecho escapado (< e &)');
  Conferir(Tem(AHtml, '&lt;script&gt;alert(1)&lt;/script&gt;'), 'tag do usuario escapada');
  Conferir(Tem(AHtml, 'a &lt; b &amp; c'), 'problema escapado');
  Conferir(Tem(AHtml, 'href="https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/plugins#o-que-mudou-da-12-para-a-20"'),
           'link da wiki');
  Conferir(Contar(AHtml, '<ul>') = Contar(AHtml, '</ul>'), 'ul balanceado');
  Conferir(Contar(AHtml, '<li>') = Contar(AHtml, '</li>'), 'li balanceado');
  Conferir(Contar(AHtml, '<pre>') = Contar(AHtml, '</pre>') + 0, 'pre balanceado');
  Conferir(Tem(AHtml, '<div class="erro">') and Tem(AHtml, 'acesso negado'), 'erros destacados');
  Conferir(Tem(AHtml, '<h2>Feito</h2>') and Tem(AHtml, '<h2>Precisa de você</h2>') and
           Tem(AHtml, '<h2>Comportamento que mudou</h2>'), 'tres blocos');
  Conferir(Tem(AHtml, '<html lang="pt-BR">'), 'idioma');
end;

procedure TesteSintetico;
var
  vRes: TResultadoMigracao;
  vConj: TConjuntoArquivos;
  vPasta, vMd, vHtml, vArqMd, vArqHtml, vErro, vSemCopia, vComCopia: string;
  vDados: TDadosRelatorio;
  vArq: TArquivoMigrado;
begin
  WriteLn('resultado sintetico');
  vPasta := NovaPasta('sintetico');
  GravarTudo(vPasta + 'Unit1.pas', 'unit Unit1;'#13#10'end.'#13#10);
  GravarTudo(vPasta + 'Binario.dfm', 'binario');
  vRes := ResultadoSintetico;
  vConj := TConjuntoArquivos.Create(vPasta);
  try
    vArq := vConj.Adicionar('Unit1.pas');
    vArq.Texto.Inserir(1, '// migrado'#13#10);
    vConj.CriarArquivo('Novo.pas', 'unit Novo;');
    vArq := vConj.Adicionar('Binario.dfm');
    vArq.Ignorado := True;
    vArq.Motivo := 'formulario binario que o usuario nao deixou converter';
    vDados := DadosRelatorio(vRes, vConj, '', vPasta + '_ral-migracao' + PathDelim +
      '1.1-2.0-20261019-101500.zip', True);
    vDados.Data := EncodeDate(2026, 10, 19) + EncodeTime(10, 15, 0, 0);
    vMd := GerarMarkdown(vDados);
    vHtml := GerarHTML(vDados);
    TesteWiki;
    TesteMarkdown(vMd);
    TesteHtml(vHtml);

    WriteLn('cabecalho');
    Conferir(Tem(vMd, '2026-10-19 10:15'), 'data');
    Conferir(Tem(vMd, 'RAL 1.1 → RAL 2.0'), 'versoes');
    Conferir(Tem(vMd, '`_ral-migracao/1.1-2.0-20261019-101500.zip`'), 'zip relativo a pasta');
    Conferir(Tem(vMd, 'ralmigra desfazer "' + vPasta), 'como desfazer');
    Conferir(GerarMarkdown(vDados) = vMd, 'mesmo resultado = mesmo relatorio');
    Conferir(Tem(LinhaResumo(vDados), '2 arquivo(s) alterado(s)'), 'LinhaResumo');

    vDados.ZipCopia := '';
    vSemCopia := GerarMarkdown(vDados);
    Conferir(Tem(vSemCopia, 'Sem cópia: não há como desfazer.') and
             not Tem(vSemCopia, 'ralmigra desfazer'), 'sem copia: nao ha como desfazer');
    vDados.Gravado := False;
    vComCopia := GerarMarkdown(vDados);
    Conferir(Tem(vComCopia, 'Prévia: nada foi gravado.') and
             not Tem(vComCopia, 'não faz parte do desfazer'), 'previa: nada gravado');

    WriteLn('gravar');
    vDados.Gravado := True;
    Conferir(GravarRelatorio(vPasta, vDados, vArqMd, vArqHtml, vErro), 'GravarRelatorio ' + vErro);
    Conferir(ExtractFileName(vArqMd) = 'MIGRACAO-RAL-1.1-2.0.md', 'nome .md');
    Conferir(ExtractFileName(vArqHtml) = 'MIGRACAO-RAL-1.1-2.0.html', 'nome .html');
    Conferir(LerTudo(vArqMd) = GerarMarkdown(vDados), 'o .md gravado e o gerado');
    Conferir(LerTudo(vArqHtml) = GerarHTML(vDados), 'o .html gravado e o gerado');
    Conferir(Copy(LerTudo(vArqMd), 1, 3) <> #$EF#$BB#$BF, 'sem BOM');
  finally
    vConj.Free;
    vRes.Free;
  end;
  ApagarArvore(ExcludeTrailingPathDelimiter(vPasta));

  WriteLn('resultado vazio');
  vRes := TResultadoMigracao.Create;
  try
    vRes.De := '1.2';
    vRes.Para := '2.0';
    vDados := DadosRelatorio(vRes, nil, 'C:\x', '', True);
    vMd := GerarMarkdown(vDados);
    Conferir(Tem(vMd, 'Nenhuma alteração foi feita.') and Tem(vMd, 'Nada para fazer à mão.') and
             Tem(vMd, 'Nenhum comportamento mudou'), 'frases de bloco vazio');
    Conferir(not Tem(vMd, '## Erros'), 'sem bloco de erros');
    Conferir(Tem(GerarHTML(vDados), '<h2>Feito</h2>'), 'html vazio');
  finally
    vRes.Free;
  end;
end;

{ Samples }

var
  GExe: string;
  GRepo: string;

function Opcao(const ANome, APadrao: string): string;
var
  vInt: integer;
begin
  Result := APadrao;
  for vInt := 1 to ParamCount do
    if Copy(ParamStr(vInt), 1, Length(ANome) + 3) = '--' + ANome + '=' then
      Result := Copy(ParamStr(vInt), Length(ANome) + 4, MaxInt);
end;

/// The files the migration left that the test allows (reports and copies)
function EhRelatorioOuCopia(const ALinha: string): boolean;
begin
  Result := Tem(ALinha, 'MIGRACAO-RAL-') or Tem(ALinha, '_ral-migracao');
end;

function ExtrairSamples(const APasta: string): boolean;
var
  vSaida, vTar: string;
begin
  vTar := APasta + 'samples.tar';
  Result := (Rodar('git', ['-C', GRepo, 'archive', '677864d~1', '-o', vTar], '', vSaida) = 0) and
            (Rodar('tar', ['-x', '-f', vTar, '-C', APasta + 'src'], '', vSaida) = 0);
  DeleteFile(vTar);
end;

function FazerRepositorio(const APasta: string): boolean;
var
  vSaida: string;
begin
  Result := (Git(APasta, ['init', '-q'], vSaida) = 0) and
            (Git(APasta, ['add', '-A'], vSaida) = 0) and
            (Git(APasta, ['commit', '-q', '-m', 'sample 1.x'], vSaida) = 0);
end;

function StatusLimpo(const APasta: string; AAceitaRelatorios: boolean; out ASobras: string): boolean;
var
  vSaida: string;
  vLinhas: TStringList;
  vInt: integer;
begin
  ASobras := '';
  vLinhas := TStringList.Create;
  try
    Git(APasta, ['status', '--porcelain', '-uall'], vSaida);
    vLinhas.Text := vSaida;
    for vInt := 0 to vLinhas.Count - 1 do
      if (Trim(vLinhas[vInt]) <> '') and not (AAceitaRelatorios and EhRelatorioOuCopia(vLinhas[vInt])) then
        ASobras := ASobras + vLinhas[vInt] + #10;
  finally
    vLinhas.Free;
  end;
  Result := ASobras = '';
end;

procedure ApagarRelatoriosECopias(const APasta: string);
var
  vBusca: TSearchRec;
begin
  ApagarArvore(APasta + '_ral-migracao');
  if FindFirst(APasta + 'MIGRACAO-RAL-*', faAnyFile, vBusca) = 0 then
  begin
    repeat
      DeleteFile(APasta + vBusca.Name);
    until FindNext(vBusca) <> 0;
    FindClose(vBusca);
  end;
end;

/// "### file:line - rule" headings of the manual block of a report
function ManuaisDoRelatorio(const AMd: string): TStringList;
var
  vLinhas: TStringList;
  vInt: integer;
  vNoBloco: boolean;
begin
  Result := TStringList.Create;
  vLinhas := TStringList.Create;
  try
    vLinhas.Text := AMd;
    vNoBloco := False;
    for vInt := 0 to vLinhas.Count - 1 do
    begin
      if Copy(vLinhas[vInt], 1, 3) = '## ' then
        vNoBloco := vLinhas[vInt] = '## Precisa de você'
      else if vNoBloco and (Copy(vLinhas[vInt], 1, 4) = '### ') then
        Result.Add(Copy(vLinhas[vInt], 5, MaxInt));
    end;
  finally
    vLinhas.Free;
  end;
end;

procedure ListarManuaisPorRegra(AManuais: TStringList);
var
  vPorRegra: TStringList;
  vInt, vInd: integer;
  vRegra: string;
begin
  vPorRegra := TStringList.Create;
  try
    vPorRegra.Sorted := True;
    for vInt := 0 to AManuais.Count - 1 do
    begin
      vRegra := Trim(Copy(AManuais[vInt], Pos(' - ', AManuais[vInt]) + 3, MaxInt));
      vInd := vPorRegra.IndexOf(vRegra);
      if vInd < 0 then
        vInd := vPorRegra.AddObject(vRegra, TObject(PtrInt(0)));
      vPorRegra.Objects[vInd] := TObject(PtrInt(vPorRegra.Objects[vInd]) + 1);
    end;
    WriteLn('  itens manuais dos samples: ', AManuais.Count);
    for vInt := 0 to vPorRegra.Count - 1 do
      WriteLn('    ', vPorRegra[vInt], ': ', PtrInt(vPorRegra.Objects[vInt]));
  finally
    vPorRegra.Free;
  end;
end;

procedure TesteSamples;
var
  vRaiz, vPasta, vSaida, vSobras, vMdTexto, vHash1, vHash2: string;
  vManuais: TStringList;
  vCodigo: integer;
  vLimpo: boolean;
begin
  WriteLn('samples: aplicar --backup e desfazer');
  Conferir(FileExists(GExe), 'ralmigra existe: ' + GExe);
  if not FileExists(GExe) then
    Exit;
  vRaiz := NovaPasta('samples');
  vPasta := vRaiz + 'src' + PathDelim;
  ForceDirectories(vPasta);
  Conferir(ExtrairSamples(vRaiz), 'git archive 677864d~1 | tar -x');
  Conferir(FazerRepositorio(vPasta), 'git init/add/commit na copia');
  vLimpo := StatusLimpo(vPasta, False, vSobras);
  Conferir(vLimpo, 'copia limpa antes de migrar');

  vCodigo := Rodar(GExe, ['aplicar', vPasta, '--de=1.1', '--para=2.0', '--backup'], '', vSaida);
  Conferir(vCodigo = 1, 'aplicar --backup sai com 1 (sobrou manual): ' + IntToStr(vCodigo));
  WriteLn(Copy(vSaida, 1, 1500));
  Conferir(FileExists(vPasta + 'MIGRACAO-RAL-1.1-2.0.md'), 'relatorio .md existe');
  Conferir(FileExists(vPasta + 'MIGRACAO-RAL-1.1-2.0.html'), 'relatorio .html existe');
  Conferir(DirectoryExists(vPasta + '_ral-migracao'), 'cobertura: _ral-migracao existe');
  vLimpo := StatusLimpo(vPasta, False, vSobras);
  Conferir(not vLimpo, 'a migracao mudou arquivos');
  if FileExists(vPasta + 'MIGRACAO-RAL-1.1-2.0.md') then
  begin
    vMdTexto := LerTudo(vPasta + 'MIGRACAO-RAL-1.1-2.0.md');
    vManuais := ManuaisDoRelatorio(vMdTexto);
    try
      ListarManuaisPorRegra(vManuais);
      WriteLn('  itens:');
      WriteLn(vManuais.Text);
      Conferir(vManuais.Count > 0, 'o relatorio lista os itens manuais');
    finally
      vManuais.Free;
    end;
  end;

  vCodigo := Rodar(GExe, ['desfazer', vPasta], '', vSaida);
  Conferir(vCodigo = 0, 'desfazer sai com 0: ' + IntToStr(vCodigo));
  WriteLn(Copy(vSaida, 1, 600));
  vLimpo := StatusLimpo(vPasta, True, vSobras);
  Conferir(vLimpo, 'git status limpo fora dos relatorios e de _ral-migracao: ' + vSobras);
  Conferir(FileExists(vPasta + 'MIGRACAO-RAL-1.1-2.0.md'), 'o relatorio fica depois do desfazer');
  ApagarRelatoriosECopias(vPasta);
  vLimpo := StatusLimpo(vPasta, False, vSobras);
  Conferir(vLimpo, 'apagados relatorios e copias: git status vazio ' + vSobras);

  WriteLn('samples: idempotencia (a segunda rodada nao muda nada)');
  vCodigo := Rodar(GExe, ['aplicar', vPasta, '--de=1.1', '--para=2.0'], '', vSaida);
  Conferir(vCodigo = 1, 'aplicar sem --backup sai com 1: ' + IntToStr(vCodigo));
  Conferir(Tem(vSaida, 'não há como desfazer'), 'sem --backup avisa que nao ha como desfazer');
  vHash1 := HashDaArvore(vPasta);
  vCodigo := Rodar(GExe, ['aplicar', vPasta, '--de=1.1', '--para=2.0'], '', vSaida);
  vHash2 := HashDaArvore(vPasta);
  Conferir(vCodigo = 1, 'segunda rodada sai com o mesmo codigo: ' + IntToStr(vCodigo));
  Conferir(vHash1 = vHash2, 'a segunda rodada nao muda nenhum arquivo');
  Conferir(Tem(vSaida, '0 arquivo(s) gravado(s)'), 'a segunda rodada grava 0 arquivos');

  WriteLn('samples: sem copia, desfazer nao muda nada');
  vHash1 := HashDaArvore(vPasta);
  vCodigo := Rodar(GExe, ['desfazer', vPasta], '', vSaida);
  Conferir(vCodigo = 2, 'desfazer sem copia sai com 2: ' + IntToStr(vCodigo));
  Conferir(Tem(vSaida, 'não há como desfazer'), 'diz que nao ha como desfazer');
  Conferir(HashDaArvore(vPasta) = vHash1, 'nada mudou');
end;

procedure TesteRecusa;
var
  vRaiz, vPasta, vSaida, vHash: string;
  vCodigo: integer;
begin
  WriteLn('samples: destino mais velho recusado');
  if not FileExists(GExe) then
    Exit;
  vRaiz := NovaPasta('recusa');
  vPasta := vRaiz + 'src' + PathDelim;
  ForceDirectories(vPasta);
  if not ExtrairSamples(vRaiz) then
  begin
    Conferir(False, 'extrair os samples');
    Exit;
  end;
  vHash := HashDaArvore(vPasta);
  vCodigo := Rodar(GExe, ['aplicar', '--de=2.0', '--para=1.1', vPasta], '', vSaida);
  Conferir(vCodigo = 3, 'aplicar --de=2.0 --para=1.1 sai com 3: ' + IntToStr(vCodigo));
  Conferir(HashDaArvore(vPasta) = vHash, 'a arvore nao foi tocada (hash)');
  Conferir(not DirectoryExists(vPasta + '_ral-migracao'), 'nenhuma cópia criada');
  vCodigo := Rodar(GExe, ['previa', '--de=2.0', '--para=1.1', vPasta], '', vSaida);
  Conferir(vCodigo = 3, 'previa tambem sai com 3');
  ApagarArvore(ExcludeTrailingPathDelimiter(vRaiz));
end;

procedure TesteGit;
var
  vRaiz, vPasta, vSaida: string;
begin
  WriteLn('samples: aviso do git com mudancas nao commitadas');
  if not FileExists(GExe) then
    Exit;
  vRaiz := NovaPasta('git');
  vPasta := vRaiz + 'src' + PathDelim;
  ForceDirectories(vPasta);
  if not (ExtrairSamples(vRaiz) and FazerRepositorio(vPasta)) then
  begin
    Conferir(False, 'preparar o repositorio');
    Exit;
  end;
  GravarTudo(vPasta + 'README.md', LerTudo(vPasta + 'README.md') + #10'mudanca nao commitada'#10);
  Rodar(GExe, ['aplicar', vPasta, '--de=1.1', '--para=2.0', '--backup'], '', vSaida);
  Conferir(Tem(vSaida, 'repositório git com mudanças'), 'com --backup avisa do git');
  Rodar(GExe, ['desfazer', vPasta], '', vSaida);
  Rodar(GExe, ['aplicar', vPasta, '--de=1.1', '--para=2.0'], '', vSaida);
  Conferir(Tem(vSaida, 'repositório git com mudanças'), 'sem --backup avisa do git');
  ApagarArvore(ExcludeTrailingPathDelimiter(vRaiz));
end;

var
  GFazSamples: boolean;
  GInt: integer;

begin
  DefaultSystemCodePage := CP_UTF8;
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  {$ENDIF}
  GFazSamples := False;
  for GInt := 1 to ParamCount do
    if ParamStr(GInt) = '--samples' then
      GFazSamples := True;
  GExe := ExpandFileName(Opcao('exe', 'migrador' + PathDelim + 'ralmigra.exe'));
  GRepo := Opcao('repo', 'D:\Projetos\Delphi\OpenSourceCommunityBrasil\PascalRAL\PascalRAL-Samples-2.0');
  if (ParamCount = 0) or (ParamStr(1) = '--testes') then
    TesteSintetico;
  if GFazSamples then
  begin
    TesteSamples;
    TesteRecusa;
    TesteGit;
  end;
  WriteLn;
  if GFalhas = 0 then
    WriteLn('relatorio: tudo certo')
  else
    WriteLn('relatorio: ', GFalhas, ' falha(s)');
  if GFalhas > 0 then
    Halt(1);
end.
